{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- |Module containing definition of and functions for maintaining the in-database state storage for Refurb.
module Refurb.Store where

import ClassyPrelude
import Composite.Opaleye (defaultRecTable)
import Composite.Opaleye.TH (deriveOpaleyeEnum)
import Composite.Record ((:->), Record)
import Composite.TH (withLensesAndProxies)
import Control.Arrow (returnA)
import Control.Lens (view)
import Control.Monad.Base (liftBase)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (MonadLogger, logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.These (These(This, These, That))
import qualified Database.PostgreSQL.Simple as PG
import Opaleye (Column, PGBool, PGInt4, PGFloat8, PGText, PGTimestamptz, QueryArr, Table(TableWithSchema), asc, orderBy, queryTable, runQuery)
import Refurb.MigrationUtils (doesTableExist, qqSqls)
import Refurb.Types (Migration, migrationQualifiedKey)

-- |Result of running a migration, either success or failure.
data MigrationResult
  = MigrationSuccess
  | MigrationFailure
  deriving (Eq, Show)

deriveOpaleyeEnum ''MigrationResult "migration_result_enum" (stripPrefix "migration" . toLower)

withLensesAndProxies [d|
  type FId           = "id"            :-> Int32
  type FIdMay        = "id"            :-> Maybe Int32
  type CId           = "id"            :-> Column PGInt4
  type CIdMay        = "id"            :-> Maybe (Column PGInt4)
  type FQualifiedKey = "qualified_key" :-> Text
  type CQualifiedKey = "qualified_key" :-> Column PGText
  type FApplied      = "applied"       :-> UTCTime
  type CApplied      = "applied"       :-> Column PGTimestamptz
  type FOutput       = "output"        :-> Text
  type COutput       = "output"        :-> Column PGText
  type FResult       = "result"        :-> MigrationResult
  type CResult       = "result"        :-> Column PGMigrationResult
  type FDuration     = "duration"      :-> Double
  type CDuration     = "duration"      :-> Column PGFloat8

  type FProdSystem = "prod_system" :-> Bool
  type CProdSystem = "prod_system" :-> Column PGBool
  |]

-- |Fields of a migration log entry in memory fetched from the database (with ID)
type MigrationLog      = '[FId   , FQualifiedKey, FApplied, FOutput, FResult, FDuration]
-- |Fields of a migration log entry to insert in the database (with the ID column optional)
type MigrationLogW     = '[FIdMay, FQualifiedKey, FApplied, FOutput, FResult, FDuration]
-- |Columns of a migration log when reading from the database (with ID)
type MigrationLogColsR = '[CId   , CQualifiedKey, CApplied, COutput, CResult, CDuration]
-- |Columns of a migration log when inserting into the database (with ID column optional)
type MigrationLogColsW = '[CIdMay, CQualifiedKey, CApplied, COutput, CResult, CDuration]

-- |Fields of the Refurb config in memory
type RefurbConfig     = '[FProdSystem]
-- |Columns of the Refurb config in the database
type RefurbConfigCols = '[CProdSystem]

-- |The migration log table which records all executed migrations and their results
migrationLog :: Table (Record MigrationLogColsW) (Record MigrationLogColsR)
migrationLog = TableWithSchema "refurb" "migration_log" defaultRecTable

-- |The refurb config table which controls whether this database is considered a production one or not
refurbConfig :: Table (Record RefurbConfigCols) (Record RefurbConfigCols)
refurbConfig = TableWithSchema "refurb" "config" defaultRecTable

-- |Test to see if the schema seems to be installed by looking for an existing refurb_config table
isSchemaPresent :: (MonadBaseControl IO m, MonadMask m, MonadLogger m) => PG.Connection -> m Bool
isSchemaPresent conn = do
  $logDebug "Checking if schema present"
  runReaderT (doesTableExist "refurb" "config") conn

-- |Check if this database is configured as a production database by reading the refurb config table
isProdSystem :: (MonadBaseControl IO m, MonadLogger m) => PG.Connection -> m Bool
isProdSystem conn = do
  $logDebug "Checking if this is a prod system"
  map (fromMaybe False . headMay) . liftBase . runQuery conn $ proc () -> do
    config <- queryTable refurbConfig -< ()
    returnA -< view cProdSystem config

-- |Create the refurb schema elements. Will fail if they already exist.
initializeSchema :: (MonadBaseControl IO m, MonadLogger m) => PG.Connection -> m ()
initializeSchema conn = do
  $logDebug "Initializing refurb schema"

  liftBase $ traverse_ (void . PG.execute_ conn) [qqSqls|
    create schema refurb;
    set search_path = 'refurb';
    create type migration_result_enum as enum('success', 'failure');
    create table config (prod_system boolean not null);
    insert into config (prod_system) values (false);
    create sequence migration_log_serial;
    create table migration_log
      ( id            int                   not null
                                            primary key
                                            default nextval('migration_log_serial')
      , qualified_key text                  not null
                                            unique
      , applied       timestamptz           not null
      , output        text                  not null
      , result        migration_result_enum not null
      , duration      double precision      not null
      );
    |]

-- |Read the migration log and stitch it together with the expected migration list, forming a list in the same order as the known migrations but with
-- 'These' representing whether the migration log for the known migration is present or not.
--
-- * @'This' migration@ represents a known migration that has no log entry.
-- * @'That' migrationLog@ represents an unknown migration that was applied in the past.
-- * @'These' migration migrationLog@ represents a migration that has an attempted application in the log.
readMigrationStatus
  :: (MonadBaseControl IO m, MonadLogger m)
  => PG.Connection
  -> [Migration]
  -> QueryArr (Record MigrationLogColsR) ()
  -> m [These Migration (Record MigrationLog)]
readMigrationStatus conn migrations restriction = do
  $logDebug "Reading migration status"
  migrationStatus <- liftBase $ runQuery conn . orderBy (asc $ view cQualifiedKey) $ proc () -> do
    mlog <- queryTable migrationLog -< ()
    restriction -< mlog
    returnA -< mlog

  let migrationLogByKey = mapFromList . map (view fQualifiedKey &&& id) $ migrationStatus

      alignMigration
        :: Migration
        -> ([These Migration (Record MigrationLog)], Map Text (Record MigrationLog))
        -> ([These Migration (Record MigrationLog)], Map Text (Record MigrationLog))
      alignMigration m@(migrationQualifiedKey -> k) (t, l) =
        first ((:t) . maybe (This m) (These m)) (updateLookupWithKey (\ _ _ -> Nothing) k l)

      (aligned, extra) = foldr alignMigration ([], migrationLogByKey) migrations

  pure $ map That (toList extra) ++ aligned

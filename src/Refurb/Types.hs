{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |Module containing externally useful types for Refurb, most notably the 'Migration' type.
module Refurb.Types
  ( connectInfoAsLogString
  , MigrationType(..)
  , MonadMigration
  , Migration(..), migrationSchema, migrationKey, migrationType, migrationCheck, migrationExecute, migrationQualifiedKey
  , schemaMigration, seedDataMigration, withCheck
  ) where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Database.PostgreSQL.Simple as PG

-- |Omit password from 'PG.ConnectInfo'
omitPassword :: PG.ConnectInfo -> PG.ConnectInfo
omitPassword info = info { PG.connectPassword = "<redacted>" }

-- |Given a 'PG.ConnectInfo' make up the log-safe connection string to show to humans, which omits the password.
connectInfoAsLogString :: PG.ConnectInfo -> Text
connectInfoAsLogString =
  decodeUtf8 . PG.postgreSQLConnectionString . omitPassword

-- |Enumeration of the types of migration that are known about.
data MigrationType
  = MigrationSchema
  -- ^Migration that updates the schema of the database and should be run everywhere.
  | MigrationSeedData
  -- ^Migration that installs or replaces data for testing purposes and should never be run in production.
  deriving (Eq, Show)

-- |Constraint for actions run in the context of a migration, with access to underlying IO, PostgreSQL connection, and logging.
type MonadMigration m = (MonadBaseControl IO m, MonadMask m, MonadReader PG.Connection m, MonadLogger m)

-- |Data type of a migration, with its key, type, and actions.
data Migration = Migration
  { _migrationSchema  :: Text
  -- ^Schema for the migration to run in, which also qualifies the migration key."
  , _migrationKey     :: Text
  -- ^Unique key to identify this migration among all known migrations. Never reuse keys, as they're the only link between the stored migration log and known
  -- migrations.
  , _migrationType    :: MigrationType
  -- ^What type of migration this is.
  , _migrationCheck   :: forall m. MonadMigration m => Maybe (m ())
  -- ^Optional action to execute before the primary execution to verify preconditions.
  , _migrationExecute :: forall m. MonadMigration m =>        m ()
  -- ^Main migration action, such as creating tables or updating data.
  }

-- |The fully qualified key of the migration, schema.key
migrationQualifiedKey :: Migration -> Text
migrationQualifiedKey (Migration { _migrationSchema, _migrationKey }) =
  _migrationSchema <> "." <> _migrationKey

makeLenses ''Migration

-- |Helper to construct a 'MigrationSchema' type 'Migration' with the given execution action and no check action.
schemaMigration :: Text -> Text -> (forall m. MonadMigration m => m ()) -> Migration
schemaMigration schema key execute = Migration
  { _migrationSchema  = schema
  , _migrationKey     = key
  , _migrationType    = MigrationSchema
  , _migrationCheck   = Nothing
  , _migrationExecute = execute
  }

-- |Helper to construct a 'MigrationSeedData' type 'Migration' with the given execution action and no check action.
seedDataMigration :: Text -> Text -> (forall m. MonadMigration m => m ()) -> Migration
seedDataMigration schema key execute = Migration
  { _migrationSchema  = schema
  , _migrationKey     = key
  , _migrationType    = MigrationSeedData
  , _migrationCheck   = Nothing
  , _migrationExecute = execute
  }

-- |Attach a check function to a 'Migration'.
withCheck :: Migration -> (forall m. MonadMigration m => m ()) -> Migration
withCheck m c = m { _migrationCheck = Just c }

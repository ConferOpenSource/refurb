{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Refurb.Run.Migrate where

import ClassyPrelude hiding ((</>), defaultTimeLocale, getCurrentTime, formatTime)
import Composite.Record (Record, pattern (:*:), pattern RNil)
import Control.Arrow (returnA)
import Control.Monad.Base (liftBase)
import Control.Monad.Logger (askLoggerIO, runLoggingT)
import Control.Lens (each, toListOf, view)
import Data.AffineSpace ((.-.))
import qualified Data.DList as DL
#if MIN_VERSION_these(1,0,0)
import Data.These.Lens (_This)
#else
import Data.These (_This)
#endif
import Data.Thyme.Clock (NominalDiffTime, getCurrentTime, toSeconds)
import Data.Thyme.Format (defaultTimeLocale, formatTime)
import Data.Thyme.Format.Human (humanTimeDiff)
import Data.Thyme.Time.Core (fromThyme)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import Language.Haskell.TH (Loc, loc_package, loc_module, loc_filename, loc_start)
import Opaleye (Insert (Insert), rCount, runInsert, toFields)
import Refurb.Cli (GoNoGo(GoNoGo), PreMigrationBackup(PreMigrationBackup), InstallSeedData(InstallSeedData))
import Refurb.MigrationUtils (doesSchemaExist)
import Refurb.Run.Backup (backup)
import Refurb.Run.Internal (MonadRefurb, contextDbConn, contextMigrations, optionallyColoredM)
import Refurb.Store (MigrationLogW, MigrationLogColsW, MigrationResult(MigrationSuccess, MigrationFailure), migrationLog, isProdSystem, readMigrationStatus)
import Refurb.Types (Migration, migrationQualifiedKey, migrationSchema, migrationType, migrationCheck, migrationExecute, MigrationType(MigrationSchema))
import System.Exit (exitFailure)
import System.Log.FastLogger (LogStr, fromLogStr, toLogStr)
import Text.PrettyPrint.ANSI.Leijen (Doc, (</>), (<+>), hang, fillSep, red, green, white, text)

-- |Helper which produces the standard prefix 'Doc' for a given migration: @migration key: @ with color.
migrationPrefixDoc :: Migration -> Doc
migrationPrefixDoc migration = white (text . unpack . migrationQualifiedKey $ migration) ++ text ":"

-- |Implement the @migrate@ command by verifying that seed data is only applied to non-production databases, reading the migration status, and determining
-- from that status which migrations to apply. If the user requested execution of migrations, delegate to 'applyMigrations' to actually do the work.
migrate :: (MonadUnliftIO m, MonadRefurb m) => GoNoGo -> Maybe PreMigrationBackup -> InstallSeedData -> m ()
migrate (GoNoGo isGo) backupMay (InstallSeedData shouldInstallSeedData) = do
  disp <- optionallyColoredM
  dbConn <- asks contextDbConn
  migrations <- asks contextMigrations

  when shouldInstallSeedData $
    whenM (isProdSystem dbConn) $ do
      disp . red . text $ "Refusing to install seed data on production system."
      liftBase exitFailure

  migrationStatus <- readMigrationStatus dbConn (filter useMigration migrations) (proc _ -> returnA -< ())

  let migrationsToApply = toListOf (each . _This) migrationStatus
  disp . hang 2 $ "Migrations to apply: " </> fillSep (map ((++ text ",") . white . text . unpack . migrationQualifiedKey) migrationsToApply)

  if isGo
    then traverse_ (\ (PreMigrationBackup path) -> backup path) backupMay >> applyMigrations migrationsToApply
    else disp $ text "Not applying migrations without --execute"

  where
    useMigration m = view migrationType m == MigrationSchema || shouldInstallSeedData

-- |Given a pre-vetted list of 'Migration' structures to apply to the database, iterate through them and run their check actions (if any) followed by
-- execution actions with log output captured.
applyMigrations :: (MonadUnliftIO m, MonadRefurb m) => [Migration] -> m ()
applyMigrations migrations = do
  disp <- optionallyColoredM
  dbConn <- asks contextDbConn

  for_ migrations $ \ migration -> do
    let schema = view migrationSchema migration
    unlessM (runReaderT (doesSchemaExist schema) dbConn) $
      void . liftIO $ PG.execute_ dbConn (PG.Query $ "create schema " <> encodeUtf8 schema)

    void . liftIO $ PG.execute dbConn "set search_path = ?" (PG.Only $ view migrationSchema migration)

    for_ (view migrationCheck migration) $ \ check ->
      onException
        ( do runReaderT check dbConn
             disp $ migrationPrefixDoc migration <+> green (text "check passed") )
        (    disp $ migrationPrefixDoc migration <+> red   (text "check failed") )

    outputRef <- liftBase $ newIORef (mempty :: DList ByteString)
    start <- liftBase getCurrentTime

    let insertLog result = do
          end <- liftBase getCurrentTime
          output <- decodeUtf8 . concat . intersperse "\n" <$> liftBase (readIORef outputRef)
          let duration = end .-. start
              suffix = text "after" <+> text (humanTimeDiff duration)

          case result of
            MigrationSuccess ->    disp $ migrationPrefixDoc migration <+> green (text "success") <+> suffix
            MigrationFailure -> do disp $ migrationPrefixDoc migration <+> red   (text "failure") <+> suffix
                                   putStrLn output

          void . liftIO $ PG.execute_ dbConn "set search_path = 'public'"
          liftIO . runInsert dbConn . (\rows -> Insert migrationLog rows rCount Nothing) . singleton . (toFields :: Record MigrationLogW -> Record MigrationLogColsW) $
            Nothing :*: migrationQualifiedKey migration :*: fromThyme start :*: output :*: result :*: (toSeconds :: NominalDiffTime -> Double) duration :*: RNil

    onException
      ( do
        logFunc <- askLoggerIO
        runLoggingT (runReaderT (view migrationExecute migration) dbConn) $ \ loc src lvl str -> do
          logFunc loc src lvl str
          dateLogStr <- nowLogString
          let message = fromLogStr $ dateLogStr <> " [" <> (toLogStr . show) lvl <> "] " <> str <> " @(" <> locLogString loc <> ")"
          modifyIORef' outputRef (`DL.snoc` message)
        insertLog MigrationSuccess )
      ( insertLog MigrationFailure )

-- |Format a 'Loc' in the way we want for logging output - @package:module filename:line:column@
locLogString :: Loc -> LogStr
locLogString loc = p <> ":" <> m <> " " <> f <> ":" <> l <> ":" <> c
  where p = toLogStr . loc_package $ loc
        m = toLogStr . loc_module $ loc
        f = toLogStr . loc_filename $ loc
        l = toLogStr . show . fst . loc_start $ loc
        c = toLogStr . show . snd . loc_start $ loc

-- |Format the current timestamp in the way we want for logging output - @yyyy-mm-dd hh:mm:ss.SSS@
nowLogString :: IO LogStr
nowLogString = do
  now <- getCurrentTime
  pure . toLogStr $ formatTime defaultTimeLocale "%Y-%m-%d %T%Q" now

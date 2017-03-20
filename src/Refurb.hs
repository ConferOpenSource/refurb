-- |Top level module of Refurb along which re-exports the library portion of Refurb ('Refurb.Types' and 'Refurb.MigrationUtils')
module Refurb
  ( refurbMain
  , module Refurb.MigrationUtils
  , module Refurb.Types
  ) where

import ClassyPrelude
import Control.Monad.Logger (LogLevel(LevelDebug), filterLogger, logDebug, runStdoutLoggingT)
import qualified Database.PostgreSQL.Simple as PG
import qualified Options.Applicative as OA
import Refurb.Cli (Command(CommandMigrate, CommandShowLog, CommandShowMigration, CommandBackup), Opts(Opts, debug, command, configFile), optsParser)
import Refurb.MigrationUtils
import Refurb.Run.Backup (backup)
import Refurb.Run.Internal (Context(Context))
import Refurb.Run.Info (showMigration, showLog)
import Refurb.Run.Migrate (migrate)
import Refurb.Store (isSchemaPresent, initializeSchema)
import Refurb.Types

-- |Main entry point for refurbishing.
--
-- In @refurb readDatabaseConnectionString migrations@, @readDatabaseConnectionString@ is a function taking the configuration file path from the command line
-- and yielding a pair of actual and loggable connection strings, and @migrations@ is a list of 'Migration' records to consider.
--
-- For example:
--
-- @
--   module Main where
--
--   import Refurb ('Migration', 'MonadMigration', 'execute_', 'schemaMigration', refurbMain)
--
--   migrations :: ['Migration']
--   migrations =
--     [ schemaMigration "create-my-table" createMyTable
--     ]
--
--   createMyTable :: MonadMigration m => m ()
--   createMyTable =
--     void $ execute_ "create table my_table (...)"
--
--   main :: IO ()
--   main = refurbMain readDatabaseConnInfo migrations
-- @
refurbMain :: (FilePath -> IO ConnInfo) -> [Migration] -> IO ()
refurbMain readConnInfo migrations = do
  opts@(Opts {..}) <- OA.execParser optsParser

  connInfo <- readConnInfo configFile

  let logFilter = if debug
                  then \ _ _   -> True
                  else \ _ lvl -> lvl > LevelDebug

  runStdoutLoggingT . filterLogger logFilter $ do
    $logDebug $ "Connecting to " <> tshow (connInfoAsLogString connInfo)
    bracket (liftBase . PG.connectPostgreSQL $ connInfoAsConnString connInfo) (liftBase . PG.close) $ \ conn -> do
      let context = Context opts conn connInfo migrations

      unlessM (isSchemaPresent conn) $ initializeSchema conn

      void . liftIO $ PG.execute_ conn "set search_path = 'public'"
      flip runReaderT context $
        case command of
          CommandMigrate goNoGo backupMay installSeedData ->
            migrate goNoGo backupMay installSeedData
          CommandShowLog ->
            showLog
          CommandShowMigration key ->
            showMigration key
          CommandBackup path ->
            backup path

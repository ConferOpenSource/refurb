module Refurb.Run where

import ClassyPrelude hiding (getCurrentTime)
import Refurb.Cli (Command(CommandMigrate, CommandShowLog, CommandShowMigration, CommandBackup))
import Refurb.Run.Backup (backup)
import Refurb.Run.Internal (MonadRefurb, contextDbConn)
import Refurb.Run.Info (showMigration, showLog)
import Refurb.Run.Migrate (migrate)
import Refurb.Store (isSchemaPresent, initializeSchema)

connectedMain :: MonadRefurb m => Command -> m ()
connectedMain command = do
  dbConn <- asks contextDbConn

  unlessM (isSchemaPresent dbConn) $ initializeSchema dbConn

  case command of
    CommandMigrate goNoGo backupMay installSeedData ->
      migrate goNoGo backupMay installSeedData
    CommandShowLog ->
      showLog
    CommandShowMigration migrationKey ->
      showMigration migrationKey
    CommandBackup path ->
      backup path


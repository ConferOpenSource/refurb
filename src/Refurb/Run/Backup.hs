module Refurb.Run.Backup where

import ClassyPrelude
import Control.Monad.Logger (logInfo, logError)
import Refurb.Run.Internal (MonadRefurb, contextDbConnInfo)
import Refurb.Types (ConnInfo(ConnInfo), connDbName, connUser, connHost, connPort, connPassword)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import qualified System.Process as Proc

-- |Handle the @backup@ command by calling @pg_dump@ to save a database backup.
backup :: MonadRefurb m => FilePath -> m ()
backup path = do
  ConnInfo {..} <- asks contextDbConnInfo
  $logInfo $ "Backing up database to " <> tshow path
  env <- liftBase getEnvironment
  let createProcess =
        ( Proc.proc "pg_dump"
          [ "-Z", "9"  -- max compression
          , "-F", "c"  -- "custom" format - custom to pg_dump / pg_restore
          , "-f", path
          , "-d", unpack connDbName
          , "-U", unpack connUser
          , "-h", unpack connHost
          , "-p", show connPort
          ]
        ) { Proc.env = Just $ ("PGPASS", unpack connPassword) : env }

  (exitCode, out, err) <- liftBase $ Proc.readCreateProcessWithExitCode createProcess ""

  case exitCode of
    ExitSuccess ->
      $logInfo "Backup complete."
    ExitFailure code -> do
      $logError $ "Backup failed with code " <> tshow code
      $logError $ "pg_dump stdout:\n" <> pack out
      $logError $ "pg_dump stderr:\n" <> pack err
      fail "pg_dump failed"

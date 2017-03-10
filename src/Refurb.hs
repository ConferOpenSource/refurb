module Refurb
  ( refurbMain
  , module Refurb.MigrationUtils
  , module Refurb.Types
  ) where

import ClassyPrelude
import Control.Monad.Logger (LogLevel(LevelDebug), filterLogger, logDebug, runStdoutLoggingT)
import qualified Database.PostgreSQL.Simple as PG
import qualified Options.Applicative as OA
import Refurb.Cli (Opts(Opts, debug, command, configFile), optsParserInfo)
import Refurb.MigrationUtils
import Refurb.Run (connectedMain)
import Refurb.Run.Internal (Context(Context))
import Refurb.Types

-- |Main entry point for refurbishing.
--
-- In @refurb readDatabaseConnectionString migrations@, @readDatabaseConnectionString@ is a function taking the configuration file path from the command line
-- and yielding a pair of actual and loggable connection strings, and @migrations@ is a list of 'Migration' records to consider.
refurbMain :: (FilePath -> IO ConnInfo) -> [Migration] -> IO ()
refurbMain readConnInfo migrations = do
  opts@(Opts {..}) <- OA.execParser optsParserInfo

  connInfo <- readConnInfo configFile

  let logFilter = if debug
                  then \ _ _   -> True
                  else \ _ lvl -> lvl > LevelDebug

  runStdoutLoggingT . filterLogger logFilter $ do
    $logDebug $ "Connecting to " <> tshow (connInfoAsLogString connInfo)
    bracket (liftBase . PG.connectPostgreSQL $ connInfoAsConnString connInfo) (liftBase . PG.close) $ \ conn -> do
      let context = Context opts conn connInfo migrations
      runReaderT (connectedMain command) context

module Refurb.Run.Internal where

import ClassyPrelude
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import qualified Database.PostgreSQL.Simple as PG
import Refurb.Cli (Opts, colorize)
import Refurb.Store (MigrationResult(MigrationSuccess, MigrationFailure))
import Refurb.Types (ConnInfo, Migration)
import Text.PrettyPrint.ANSI.Leijen (Doc, green, red, plain, text, putDoc)

data Context = Context
  { contextOptions    :: Opts
  , contextDbConn     :: PG.Connection
  , contextDbConnInfo :: ConnInfo
  , contextMigrations :: [Migration]
  }

type MonadRefurb m = (MonadBaseControl IO m, MonadMask m, MonadReader Context m, MonadLogger m, MonadLoggerIO m)

optionallyColorM :: MonadRefurb m => m (Doc -> Doc)
optionallyColorM =
  bool plain id <$> asks (colorize . contextOptions)

optionallyColoredM :: MonadRefurb m => m (Doc -> m ())
optionallyColoredM = do
  maybePlain <- optionallyColorM
  pure $ \ doc -> do
    liftBase $ putDoc (maybePlain doc)
    putStrLn ""

migrationResultDoc :: MigrationResult -> Doc
migrationResultDoc = \ case
  MigrationSuccess -> green (text "success")
  MigrationFailure -> red   (text "failure")


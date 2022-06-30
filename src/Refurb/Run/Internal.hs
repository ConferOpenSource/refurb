{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Module containing shared types and functions used for implementing the various commands.
module Refurb.Run.Internal where

import ClassyPrelude
import Control.Monad (MonadFail)
import Control.Monad.Base (liftBase)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Database.PostgreSQL.Simple as PG
import Refurb.Cli (Opts, colorize)
import Refurb.Store (MigrationResult(MigrationSuccess, MigrationFailure))
import Refurb.Types (ConnInfo, Migration)
import Text.PrettyPrint.ANSI.Leijen (Doc, green, red, plain, text, putDoc)

-- |Reader context for all command execution which contains the command line options, database connection and connection information, and known migrations.
data Context = Context
  { contextOptions    :: Opts
  -- ^The 'Opts' structure parsed from the command line by @Refurb.Cli@.
  , contextDbConn     :: PG.Connection
  -- ^The open database 'PG.Connection'.
  , contextDbConnInfo :: ConnInfo
  -- ^The information used to connect to the database, required for running command line tools like @pg_dump@ against the same database.
  , contextMigrations :: [Migration]
  -- ^The known migrations passed in to 'Refurb.refurbMain'.
  }

-- |Constraint of actions for command execution, including access to the 'Context', logging, and underlying IO.
type MonadRefurb m = (MonadBaseControl IO m, MonadFail m, MonadMask m, MonadReader Context m, MonadLogger m, MonadLoggerIO m)

-- |Given the configuration implicitly available to 'MonadRefurb', produce a function which possibly strips ANSI colorization from a 'Doc' if the user
-- requested colorless output.
optionallyColorM :: MonadRefurb m => m (Doc -> Doc)
optionallyColorM =
  bool plain id <$> asks (colorize . contextOptions)

-- |Given the configuration implicitly available to 'MonadRefurb', produce a function which emits a 'Doc' on stdout that is colored unless the user requested
-- colorless output.
optionallyColoredM :: MonadRefurb m => m (Doc -> m ())
optionallyColoredM = do
  maybePlain <- optionallyColorM
  pure $ \ doc -> do
    liftBase $ putDoc (maybePlain doc)
    putStrLn ""

-- |Produce a colorized 'Doc' with @success@ or @failure@, based on which 'MigrationResult' value was passed.
migrationResultDoc :: MigrationResult -> Doc
migrationResultDoc = \ case
  MigrationSuccess -> green (text "success")
  MigrationFailure -> red   (text "failure")

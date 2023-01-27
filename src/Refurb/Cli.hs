{-# LANGUAGE NoImplicitPrelude #-}
-- |Module with @optparse-applicative@ parsers for and datatypes to represent the command line arguments.
module Refurb.Cli where

import ClassyPrelude
import Composite.Record ((:->)(Val))
import Data.Word (Word16)
import qualified Options.Applicative as OA
import Refurb.Store (FQualifiedKey)

-- |Newtype wrapper for the @--execute@ boolean (@True@ if given, @False@ if omitted)
newtype GoNoGo = GoNoGo Bool deriving (Eq, Show)

-- |Newtype wrapper for the @--backup-first@ option to the @migrate@ command.
newtype PreMigrationBackup = PreMigrationBackup FilePath deriving (Eq, Show)

-- |Newtype wrapper for the @--seed@ boolean (@True@ if given, @False@ if omitted)
newtype InstallSeedData = InstallSeedData Bool deriving (Eq, Show)

-- |The various top level commands that can be requested by the user
data Command
  = CommandMigrate GoNoGo (Maybe PreMigrationBackup) InstallSeedData
  -- ^Migrate the database or show what migrations would be applied, possibly backing up beforehand.
  | CommandShowLog
  -- ^Show the migration status.
  | CommandShowMigration FQualifiedKey
  -- ^Show status of a particular migration with its log output.
  | CommandBackup FilePath
  -- ^Back up the database.
  deriving (Eq, Show)

-- |Option parser for the @migrate@ command
commandMigrateParser :: OA.ParserInfo Command
commandMigrateParser =
  OA.info
    (
      CommandMigrate
        <$> ( GoNoGo <$> OA.switch
              (  OA.long "execute"
              <> OA.short 'e'
              <> OA.help "Actually run migrations. Without this switch the migrations to run will be logged but none of them executed."
              )
            )
        <*> ( OA.option (Just . PreMigrationBackup <$> OA.auto)
              (  OA.value Nothing
              <> OA.long "backup-first"
              <> OA.short 'b'
              <> OA.metavar "BACKUP-FILE"
              <> OA.help "Back up the database before applying migrations. Has no effect without --execute."
              )
            )
        <*> ( InstallSeedData <$> OA.switch
              (  OA.long "seed"
              <> OA.short 's'
              <> OA.help "Apply seed scripts in addition to schema migrations. Not available on prod databases."
              )
            )
    )
    ( OA.progDesc "Apply migrations to the database, or see which ones would be applied" )

-- |Option parser for the @show-log@ command
commandShowLogParser :: OA.ParserInfo Command
commandShowLogParser =
  OA.info
    (
      pure CommandShowLog
    )
    ( OA.progDesc "Show migrations along with their status in the database" )

-- |Option parser for the @show-migration@ command
commandShowMigrationParser :: OA.ParserInfo Command
commandShowMigrationParser =
  OA.info
    (
      CommandShowMigration
        <$> (Val . pack <$> OA.strArgument (OA.metavar "MIGRATION-KEY"))
    )
    ( OA.progDesc "Show status of and log details for a particular migration" )

-- |Option parser for the @backup@ command
commandBackupParser :: OA.ParserInfo Command
commandBackupParser =
  OA.info
    (
      CommandBackup
        <$> OA.strArgument (OA.metavar "BACKUP-FILE")
    )
    ( OA.progDesc "Back up the database" )

-- |Options for connecting to database.
data ConnectOps
  = ConnectOpsFile FilePath
  -- ^Connect via a file
  | ConnectOpsParams String Word16 String String
  -- ^Connect via parameters - reads password from PGPASS

-- |Structure holding the parsed command line arguments and options.
data Opts = Opts
  { debug    :: Bool
  -- ^Whether to turn on debug logging to the console
  , colorize :: Bool
  -- ^Whether to colorize console output
  , config   :: ConnectOps
  -- ^See 'ConnectOps'
  , command  :: Command
  -- ^Which command the user chose and the options for that command
  }

connectOpsParser :: OA.Parser ConnectOps
connectOpsParser = fileParser <|> paramsParser
  where
    fileParser = ConnectOpsFile
      <$> OA.strOption
            (  OA.long "config"
            <> OA.short 'c'
            <> OA.metavar "SERVER-CONFIG"
            <> OA.help "Path to server config file to read database connection information from"
            )
    paramsParser = ConnectOpsParams
      <$> OA.strOption
            (  OA.long "host"
            <> OA.short 'h'
            <> OA.metavar "DATABASE-HOST"
            <> OA.value "localhost"
            <> OA.help "Database host"
            <> OA.showDefault
            )
      <*> OA.option OA.auto
            (  OA.long "port"
            <> OA.short 'p'
            <> OA.metavar "DATABASE-PORT"
            <> OA.value 5432
            <> OA.help "Database port"
            <> OA.showDefault
            )
      <*> OA.strOption
            (  OA.long "dbname"
            <> OA.short 'd'
            <> OA.metavar "DATABASE-NAME"
            <> OA.value "postgres"
            <> OA.help "Database name"
            <> OA.showDefault
            )
      <*> OA.strOption
            (  OA.long "user"
            <> OA.short 'u'
            <> OA.metavar "DATABASE-USER"
            <> OA.value "postgres"
            <> OA.help "Database user"
            <> OA.showDefault
            )


-- |Parser for the command line arguments
optsParser :: OA.ParserInfo Opts
optsParser =
  OA.info
    (
      OA.helper <*> (
        Opts
          <$> OA.switch
                (  OA.long "debug"
                <> OA.short 'd'
                <> OA.help "Turn on debug diagnostic logging"
                )
          <*> (not <$> OA.switch (OA.long "no-color" <> OA.help "disable ANSI colorization"))
          <*> connectOpsParser
          <*> OA.hsubparser
                (  OA.command "migrate"        commandMigrateParser
                <> OA.command "show-log"       commandShowLogParser
                <> OA.command "show-migration" commandShowMigrationParser
                <> OA.command "backup"         commandBackupParser
                )
      )
    )
    (  OA.fullDesc
    <> OA.header "Maintain server database"
    )

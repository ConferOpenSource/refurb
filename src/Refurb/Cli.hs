module Refurb.Cli where

import ClassyPrelude
import Frames ((:->)(Col))
import qualified Options.Applicative as OA
import Refurb.Store (FKey)

newtype GoNoGo = GoNoGo Bool deriving (Eq, Show)
newtype PreMigrationBackup = PreMigrationBackup FilePath deriving (Eq, Show)
newtype InstallSeedData = InstallSeedData Bool deriving (Eq, Show)

data Command
  = CommandMigrate GoNoGo (Maybe PreMigrationBackup) InstallSeedData
  | CommandShowLog
  | CommandShowMigration FKey
  | CommandBackup FilePath
  deriving (Eq, Show)

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

commandShowLogParser :: OA.ParserInfo Command
commandShowLogParser =
  OA.info
    (
      pure CommandShowLog
    )
    ( OA.progDesc "Show migrations along with their status in the database" )

commandShowMigrationParser :: OA.ParserInfo Command
commandShowMigrationParser =
  OA.info
    (
      CommandShowMigration
        <$> (Col . pack <$> OA.strArgument (OA.metavar "MIGRATION-KEY"))
    )
    ( OA.progDesc "Show status of and log details for a particular migration" )

commandBackupParser :: OA.ParserInfo Command
commandBackupParser =
  OA.info
    (
      CommandBackup
        <$> OA.strArgument (OA.metavar "BACKUP-FILE")
    )
    ( OA.progDesc "Back up the database" )

data Opts = Opts
  { debug      :: Bool
  , colorize   :: Bool
  , configFile :: FilePath
  , command    :: Command
  }

optsParser :: OA.Parser Opts
optsParser =
  Opts
    <$> OA.switch
          (  OA.long "debug"
          <> OA.short 'd'
          <> OA.help "Turn on debug diagnostic logging"
          )
    <*> (not <$> OA.switch (OA.long "no-color" <> OA.help "disable ANSI colorization"))
    <*> OA.strOption
          (  OA.long "config"
          <> OA.short 'c'
          <> OA.metavar "SERVER-CONFIG"
          <> OA.help "Path to server config file to read database connection information from"
          )
    <*> OA.hsubparser
          (  OA.command "migrate"        commandMigrateParser
          <> OA.command "show-log"       commandShowLogParser
          <> OA.command "show-migration" commandShowMigrationParser
          <> OA.command "backup"         commandBackupParser
          )

optsParserInfo :: OA.ParserInfo Opts
optsParserInfo =
  OA.info
    (OA.helper <*> optsParser)
    (  OA.fullDesc
    <> OA.header "Maintain server database"
    )


module Refurb.Run.Migrate where

import ClassyPrelude hiding ((</>), defaultTimeLocale, getCurrentTime, formatTime)
import Control.Arrow (returnA)
import Control.Monad.Logger (askLoggerIO, runLoggingT)
import Control.Lens (each, toListOf, view)
import Data.AffineSpace ((.-.))
import qualified Data.DList as DL
import Data.These (_This)
import Data.Thyme.Clock (NominalDiffTime, getCurrentTime, toSeconds)
import Data.Thyme.Format (formatTime)
import Data.Thyme.Format.Human (humanTimeDiff)
import Data.Thyme.Time.Core (fromThyme)
import Frames (Record, (&:), pattern Nil)
import Language.Haskell.TH (Loc, loc_package, loc_module, loc_filename, loc_start)
import Opaleye (constant, runInsertMany)
import Refurb.Cli (GoNoGo(GoNoGo), PreMigrationBackup(PreMigrationBackup), InstallSeedData(InstallSeedData))
import Refurb.Run.Backup (backup)
import Refurb.Run.Internal (MonadRefurb, contextDbConn, contextMigrations, optionallyColoredM)
import Refurb.Store (MigrationLogW, MigrationLogColsW, MigrationResult(MigrationSuccess, MigrationFailure), migrationLog, isProdSystem, readMigrationStatus)
import Refurb.Types (Migration, migrationKey, migrationType, migrationCheck, migrationExecute, MigrationType(MigrationSchema))
import System.Exit (exitFailure)
import System.Locale (defaultTimeLocale)
import System.Log.FastLogger (LogStr, fromLogStr, toLogStr)
import Text.PrettyPrint.ANSI.Leijen (Doc, (</>), (<+>), hang, fillSep, red, green, white, text)

migrationPrefixDoc :: Migration -> Doc
migrationPrefixDoc migration = white (text . unpack . view migrationKey $ migration) ++ text ":"

migrate :: MonadRefurb m => GoNoGo -> Maybe PreMigrationBackup -> InstallSeedData -> m ()
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
  disp . hang 2 $ "Migrations to apply: " </> fillSep (map ((++ text ",") . white . text . unpack . view migrationKey) migrationsToApply)

  if isGo
    then traverse_ (\ (PreMigrationBackup path) -> backup path) backupMay >> applyMigrations migrationsToApply
    else disp $ text "Not applying migrations without --execute"

  where
    useMigration m = view migrationType m == MigrationSchema || shouldInstallSeedData

applyMigrations :: MonadRefurb m => [Migration] -> m ()
applyMigrations migrations = do
  disp <- optionallyColoredM
  dbConn <- asks contextDbConn

  for_ migrations $ \ migration -> do
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

          liftIO . runInsertMany dbConn migrationLog . singleton . (constant :: Record MigrationLogW -> Record MigrationLogColsW) $
            Nothing &: view migrationKey migration &: fromThyme start &: output &: result &: (toSeconds :: NominalDiffTime -> Double) duration &: Nil

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

locLogString :: Loc -> LogStr
locLogString loc = p <> ":" <> m <> " " <> f <> ":" <> l <> ":" <> c
  where p = toLogStr . loc_package $ loc
        m = toLogStr . loc_module $ loc
        f = toLogStr . loc_filename $ loc
        l = toLogStr . show . fst . loc_start $ loc
        c = toLogStr . show . snd . loc_start $ loc

nowLogString :: IO LogStr
nowLogString = do
  now <- getCurrentTime
  pure . toLogStr $ formatTime defaultTimeLocale "%Y-%m-%d %T%Q" now


module Refurb.Run.Info where

import ClassyPrelude
import Composite.Record (Record)
import Control.Arrow (returnA)
import Control.Lens (Getting, _Wrapped, each, preview, to, view)
import Data.Monoid (First)
import Data.These (These(This, That, These), there)
import Data.Thyme.Clock (NominalDiffTime, fromSeconds)
import Data.Thyme.Format.Human (humanTimeDiff)
import Opaleye ((.==), constant, restrict)
import Refurb.Run.Internal (MonadRefurb, contextDbConn, contextMigrations, optionallyColoredM, migrationResultDoc)
import Refurb.Store (FQualifiedKey, MigrationLog, cQualifiedKey, fId, fApplied, fDuration, fOutput, fResult, fQualifiedKey, readMigrationStatus)
import Refurb.Types (Migration, MigrationType(MigrationSeedData), migrationQualifiedKey, migrationType)
import Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), fill, bold, underline, black, red, white, parens, text)

-- |Given a migration status as read by 'readMigrationStatus', pretty print that information as a table on stdout.
showMigrationStatus :: (MonadRefurb m, MonoTraversable t, Element t ~ These Migration (Record MigrationLog)) => t -> m ()
showMigrationStatus migrationStatus = do
  disp <- optionallyColoredM
  disp . bold . underline $ row (text "ID") (text "Timestamp") (text "Duration") (text "Result") (text "Key")
  for_ migrationStatus $ \ these ->
    disp $ case these of
      These m mlog -> mlogRow mlog <+> seedDoc m
      This m       -> row (text "") (text "not applied") (text "") (text "") (white . text . unpack $ migrationQualifiedKey m) <+> seedDoc m
      That mlog    -> mlogRow mlog <+> parens (red "not in known migrations")

  where
    row :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc
    row i t d r k = fill 6 i <+> fill 19 t <+> fill 15 d <+> fill 7 r <+> k

    field :: Getting (First String) s String -> s -> Doc
    field f = text . fromMaybe "" . preview f

    seedDoc :: Migration -> Doc
    seedDoc (view migrationType -> mtype)
      | mtype == MigrationSeedData = text "(seed data)"
      | otherwise                  = mempty

    mlogRow :: Record MigrationLog -> Doc
    mlogRow =
      row
        <$> field (fId . to show)
        <*> view (fApplied . to (white . text . formatTime defaultTimeLocale "%F %T"))
        <*> field (fDuration . to (humanTimeDiff . (fromSeconds :: Double -> NominalDiffTime)))
        <*> view (fResult . to migrationResultDoc)
        <*> view (fQualifiedKey . to (white . text . unpack))

-- |Implement the @show-log@ command by reading the entire migration log and displaying it with 'showMigrationStatus'.
showLog :: MonadRefurb m => m ()
showLog = do
  dbConn <- asks contextDbConn
  migrations <- asks contextMigrations
  migrationStatus <- readMigrationStatus dbConn migrations (proc _ -> returnA -< ())
  showMigrationStatus migrationStatus

-- |Implement the @show-migration@ command by reading migration log pertaining to the given migration key and displaying it with 'showMigrationStatus' plus
-- its log output.
showMigration :: MonadRefurb m => FQualifiedKey -> m ()
showMigration (view _Wrapped -> key) = do
  disp <- optionallyColoredM
  dbConn <- asks contextDbConn
  migrations <- asks $ filter ((== key) . migrationQualifiedKey) . contextMigrations
  migrationStatus <- readMigrationStatus dbConn migrations $ proc mlog ->
    restrict -< view cQualifiedKey mlog .== constant key

  showMigrationStatus migrationStatus
  putStrLn ""
  case preview (each . there) migrationStatus of
    Nothing   -> disp . black $ "Never been run." -- n.b.: black is not black
    Just mlog -> putStrLn . view fOutput $ mlog


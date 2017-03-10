module Refurb.MigrationUtils where

import ClassyPrelude
import Control.Monad.Logger (logDebug)
import Data.Profunctor.Product.Default (Default)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.Types (fromQuery)
import qualified Opaleye
import Refurb.Types (MonadMigration)

execute :: (MonadMigration m, PG.ToRow q) => PG.Query -> q -> m Int64
execute q p = do
  conn <- ask
  $logDebug $ decodeUtf8 (fromQuery q) <> " with " <> tshow (toRow p)
  liftBase $ PG.execute conn q p

executeMany :: (MonadMigration m, PG.ToRow q) => PG.Query -> [q] -> m Int64
executeMany q ps = do
  conn <- ask
  $logDebug $ decodeUtf8 (fromQuery q) <> " with ["
    <> maybe "" ((if length ps > 1 then (<> ", ...") else id) . tshow . toRow) (headMay ps) <> "]"
  liftBase $ PG.executeMany conn q ps

execute_ :: MonadMigration m => PG.Query -> m Int64
execute_ q = do
  conn <- ask
  $logDebug . decodeUtf8 $ fromQuery q
  liftBase $ PG.execute_ conn q

query :: (MonadMigration m, PG.ToRow q, PG.FromRow r) => PG.Query -> q -> m [r]
query q p = do
  conn <- ask
  $logDebug $ decodeUtf8 (fromQuery q) <> " with " <> tshow (toRow p)
  liftBase $ PG.query conn q p

query_ :: (MonadMigration m, PG.FromRow r) => PG.Query -> m [r]
query_ q = do
  conn <- ask
  $logDebug . decodeUtf8 $ fromQuery q
  liftBase $ PG.query_ conn q

runQuery
  :: ( MonadMigration m
     , Default Opaleye.Unpackspec columns columns
     , Default Opaleye.QueryRunner columns haskells
     )
  => Opaleye.Query columns -> m [haskells]
runQuery q = do
  conn <- ask
  for_ (Opaleye.showSql q) ($logDebug . pack)
  liftBase $ Opaleye.runQuery conn q

doesTableExist :: MonadMigration m => Text -> m Bool
doesTableExist t =
  not . (null :: [PG.Only Int] -> Bool) <$> query "select 1 from information_schema.tables where table_name = ?" (PG.Only t)

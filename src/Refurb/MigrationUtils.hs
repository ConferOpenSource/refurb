{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- |Utilities for writing migrations.
module Refurb.MigrationUtils where

import ClassyPrelude
import Control.Monad.Base (liftBase)
import Control.Monad.Logger (logDebug)
import Data.Profunctor.Product.Default (Default)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.Types (fromQuery)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Opaleye
import Opaleye.Internal.Table (tableIdentifier)
import Refurb.Types (MonadMigration)

-- |Simple quasiquoter which just makes it easier to embed literal chunks of SQL in migrations.
--
-- For example:
--
-- @
--   createStuffIndex :: MonadMigration m => m ()
--   createStuffIndex =
--     execute_
--       [qqSql|
--         create index stuff_index
--           on stuff (things)
--           where is_what_we_want_to_index = 't'
--       |]
-- @
qqSql :: TH.QuasiQuoter
qqSql = TH.QuasiQuoter
  { TH.quoteExp  = \ s -> [| $(TH.lift s) :: PG.Query |]
  , TH.quotePat  = error "qqSql should only be used in an expression context"
  , TH.quoteType = error "qqSql should only be used in an expression context"
  , TH.quoteDec  = error "qqSql should only be used in an expression context"
  }

-- |Quasiquoter which takes a block of literal SQL and converts it into a list of 'PG.Query' values, e.g. to pass to 'executeSeries_'. A semicolon at the
-- beginning or end of a line (sans whitespace) separates SQL statements.
--
-- For example:
--
-- @
--   createStuff :: MonadMigration m => m ()
--   createStuff =
--     executeSeries_ [qqSqls|
--       create sequence stuff_seq;
--       create table stuff
--         ( id bigint not null primary key default nextval('stuff_seq')
--         );
--       |]
-- @
qqSqls :: TH.QuasiQuoter
qqSqls = TH.QuasiQuoter
  { TH.quoteExp  = \ s -> [| $(bodyToStatements s) :: [PG.Query] |]
  , TH.quotePat  = error "qqSql should only be used in an expression context"
  , TH.quoteType = error "qqSql should only be used in an expression context"
  , TH.quoteDec  = error "qqSql should only be used in an expression context"
  }
  where
    bodyToStatements :: String -> TH.Q TH.Exp
    bodyToStatements = TH.lift . map (unpack . unlines) . filter (not . null) . map (filter (not . null)) . go [] . lines . pack
      where
        go acc [] = [acc]
        go acc ((T.strip -> l):ls)
          | Just l' <- T.stripSuffix ";" =<< T.stripPrefix ";" l =
            reverse acc : [l'] : go [] ls
          | Just l' <- T.stripPrefix ";" l =
            reverse acc : go [l'] ls
          | Just l' <- T.stripSuffix ";" l =
            reverse (l' : acc) : go [] ls
          | otherwise =
            go (l : acc) ls

-- |Execute some parameterized SQL against the database connection.
-- Wraps 'PG.execute' using the 'MonadMigration' reader to get the connection.
execute :: (MonadMigration m, PG.ToRow q) => PG.Query -> q -> m Int64
execute q p = do
  conn <- ask
  $logDebug $ decodeUtf8 (fromQuery q) <> " with " <> tshow (toRow p)
  liftBase $ PG.execute conn q p

-- |Execute some parameterized SQL against the database connection.
-- Wraps 'PG.executeMany' using the 'MonadMigration' reader to get the connection.
executeMany :: (MonadMigration m, PG.ToRow q) => PG.Query -> [q] -> m Int64
executeMany q ps = do
  conn <- ask
  $logDebug $ decodeUtf8 (fromQuery q) <> " with ["
    <> maybe "" ((if length ps > 1 then (<> ", ...") else id) . tshow . toRow) (headMay ps) <> "]"
  liftBase $ PG.executeMany conn q ps

-- |Execute some fixed SQL against the database connection.
-- Wraps 'PG.execute_' using the 'MonadMigration' reader to get the connection.
execute_ :: MonadMigration m => PG.Query -> m Int64
execute_ q = do
  conn <- ask
  $logDebug . decodeUtf8 $ fromQuery q
  liftBase $ PG.execute_ conn q

-- |Execute a series of fixed SQL statements against the database connection.
-- Equivalent to `traverse_ (void . execute_)`
executeSeries_ :: MonadMigration m => [PG.Query] -> m ()
executeSeries_ = traverse_ (void . execute_)

-- |Run a parameterized query against the database connection.
-- Wraps 'PG.query' using the 'MonadMigration' reader to get the connection.
query :: (MonadMigration m, PG.ToRow q, PG.FromRow r) => PG.Query -> q -> m [r]
query q p = do
  conn <- ask
  $logDebug $ decodeUtf8 (fromQuery q) <> " with " <> tshow (toRow p)
  liftBase $ PG.query conn q p

-- |Run a fixed query against the database connection.
-- Wraps 'PG.query_' using the 'MonadMigration' reader to get the connection.
query_ :: (MonadMigration m, PG.FromRow r) => PG.Query -> m [r]
query_ q = do
  conn <- ask
  $logDebug . decodeUtf8 $ fromQuery q
  liftBase $ PG.query_ conn q

-- |Run an Opaleye query against the database connection.
-- Wraps 'Opaleye.runSelect' using the 'MonadMigration' reader to get the connection.
runQuery
  :: ( MonadMigration m
     , Default Opaleye.Unpackspec columns columns
     , Default Opaleye.FromFields columns haskells
     )
  => Opaleye.Select columns -> m [haskells]
runQuery q = do
  conn <- ask
  for_ (Opaleye.showSql q) ($logDebug . pack)
  liftBase $ Opaleye.runSelect conn q

-- |Run an Opaleye 'Opaleye.runInsert' against the database connection.
runInsertMany :: MonadMigration m => Opaleye.Table columns columns' -> [columns] -> m Int64
runInsertMany table rows = do
  conn <- ask
  $logDebug $ "inserting " <> tshow (length rows) <> " rows into " <> tshow (tableIdentifier table)
  liftBase $ Opaleye.runInsert conn (Opaleye.Insert table rows Opaleye.rCount Nothing)

-- |Run an Opaleye 'Opaleye.runUpdate' against the database connection.
runUpdate :: MonadMigration m => Opaleye.Table columnsW columnsR -> (columnsR -> columnsW) -> (columnsR -> Opaleye.Field Opaleye.SqlBool) -> m Int64
runUpdate table permute filt = do
  conn <- ask
  $logDebug $ "updating " <> tshow (tableIdentifier table)
  liftBase $ Opaleye.runUpdate conn (Opaleye.Update table permute filt Opaleye.rCount)

-- |Run an Opaleye 'Opaleye.runDelete' against the database connection.
runDelete :: MonadMigration m => Opaleye.Table columnsW columnsR -> (columnsR -> Opaleye.Field Opaleye.SqlBool) -> m Int64
runDelete table filt = do
  conn <- ask
  $logDebug $ "deleting from " <> tshow (tableIdentifier table)
  liftBase $ Opaleye.runDelete conn (Opaleye.Delete table filt Opaleye.rCount)

-- |Check if a schema exists using the @information_schema@ views.
doesSchemaExist :: MonadMigration m => Text -> m Bool
doesSchemaExist schema =
  not . (null :: [PG.Only Int] -> Bool) <$> query "select 1 from information_schema.schemata where schema_name = ?" (PG.Only schema)

-- |Check if a table exists in a schema using the @information_schema@ views.
doesTableExist :: MonadMigration m => Text -> Text -> m Bool
doesTableExist schema table =
  not . (null :: [PG.Only Int] -> Bool) <$> query "select 1 from information_schema.tables where table_schema = ? and table_name = ?" (schema, table)

-- |Check if a column exists in a schema on a table using the @information_schema@ views.
doesColumnExist :: MonadMigration m => Text -> Text -> Text -> m Bool
doesColumnExist schema table column =
  not . (null :: [PG.Only Int] -> Bool) <$> query "select 1 from information_schema.columns where table_schema = ? and table_name = ? and column_name = ?" (schema, table, column)

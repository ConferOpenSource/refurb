module Refurb.Types
  ( ConnInfo(..)
  , connInfoAsConnString, connInfoAsLogString
  , MigrationType(..)
  , MonadMigration
  , Migration(..), migrationKey, migrationType, migrationCheck, migrationExecute
  , schemaMigration, seedDataMigration, withCheck
  ) where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (MonadLogger)
import qualified Data.ByteString.Char8 as BSC8
import Data.Word (Word16)
import qualified Database.PostgreSQL.Simple as PG

data ConnInfo = ConnInfo
  { connHost     :: Text
  , connPort     :: Word16
  , connUser     :: Text
  , connPassword :: Text
  , connDbName   :: Text
  }

commonParams :: ConnInfo -> [(ByteString, ByteString)]
commonParams (ConnInfo {..}) =
  [ ("host",   encodeUtf8 connHost)
  , ("port",   encodeUtf8 . tshow $ connPort)
  , ("user",   encodeUtf8 connUser)
  , ("dbname", encodeUtf8 connDbName)
  ]

asConnString :: [(ByteString, ByteString)] -> ByteString
asConnString = BSC8.intercalate " " . map (\ (key, val) -> key <> "=" <> val)

connInfoAsConnString :: ConnInfo -> ByteString
connInfoAsConnString connInfo@(ConnInfo { connPassword }) =
  asConnString (("password", encodeUtf8 connPassword) : commonParams connInfo)

connInfoAsLogString :: ConnInfo -> Text
connInfoAsLogString =
  decodeUtf8 . asConnString . commonParams

data MigrationType
  = MigrationSchema
  | MigrationSeedData
  deriving (Eq, Show)

type MonadMigration m = (MonadBaseControl IO m, MonadMask m, MonadReader PG.Connection m, MonadLogger m)

data Migration = Migration
  { _migrationKey     :: Text
  , _migrationType    :: MigrationType
  , _migrationCheck   :: forall m. MonadMigration m => Maybe (m ())
  , _migrationExecute :: forall m. MonadMigration m =>        m ()
  }

makeLenses ''Migration

schemaMigration :: Text -> (forall m. MonadMigration m => m ()) -> Migration
schemaMigration key execute = Migration
  { _migrationKey     = key
  , _migrationType    = MigrationSchema
  , _migrationCheck   = Nothing
  , _migrationExecute = execute
  }

seedDataMigration :: Text -> (forall m. MonadMigration m => m ()) -> Migration
seedDataMigration key execute = Migration
  { _migrationKey     = key
  , _migrationType    = MigrationSeedData
  , _migrationCheck   = Nothing
  , _migrationExecute = execute
  }

withCheck :: Migration -> (forall m. MonadMigration m => m ()) -> Migration
withCheck m c = m { _migrationCheck = Just c }

-- |Module containing externally useful types for Refurb, most notably the 'Migration' type.
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

-- |Structure with connection information for connecting to the database.
data ConnInfo = ConnInfo
  { connHost     :: Text
  -- ^Hostname or IP address of the PostgreSQL server.
  , connPort     :: Word16
  -- ^Port number the PostgreSQL server is running on (usually @5432@).
  , connUser     :: Text
  -- ^What user to connect to the database as.
  , connPassword :: Text
  -- ^What password to connect to the database with.
  , connDbName   :: Text
  -- ^What database in the PostgreSQL server to attach to.
  }

-- |Given a 'ConnInfo' generate the connection string pairs that are shared between the loggable and real version, that is all of them except password.
commonParams :: ConnInfo -> [(ByteString, ByteString)]
commonParams (ConnInfo {..}) =
  [ ("host",   encodeUtf8 connHost)
  , ("port",   encodeUtf8 . tshow $ connPort)
  , ("user",   encodeUtf8 connUser)
  , ("dbname", encodeUtf8 connDbName)
  ]

-- |Given a list of key/value pairs, make up a @key1=value1 key2=value2@ string that PostgreSQL expects.
asConnString :: [(ByteString, ByteString)] -> ByteString
asConnString = BSC8.intercalate " " . map (\ (key, val) -> key <> "=" <> val)

-- |Given a 'ConnInfo' make up the real connection string to pass when connecting to the database. Includes password, so never log this.
connInfoAsConnString :: ConnInfo -> ByteString
connInfoAsConnString connInfo@(ConnInfo { connPassword }) =
  asConnString (("password", encodeUtf8 connPassword) : commonParams connInfo)

-- |Given a 'ConnInfo' make up the log-safe connection string to show to humans, which omits the password.
connInfoAsLogString :: ConnInfo -> Text
connInfoAsLogString =
  decodeUtf8 . asConnString . commonParams

-- |Enumeration of the types of migration that are known about.
data MigrationType
  = MigrationSchema
  -- ^Migration that updates the schema of the database and should be run everywhere.
  | MigrationSeedData
  -- ^Migration that installs or replaces data for testing purposes and should never be run in production.
  deriving (Eq, Show)

-- |Constraint for actions run in the context of a migration, with access to underlying IO, PostgreSQL connection, and logging.
type MonadMigration m = (MonadBaseControl IO m, MonadMask m, MonadReader PG.Connection m, MonadLogger m)

-- |Data type of a migration, with its key, type, and actions.
data Migration = Migration
  { _migrationKey     :: Text
  -- ^Unique key to identify this migration among all known migrations. Never reuse keys, as they're the only link between the stored migration log and known
  -- migrations.
  , _migrationType    :: MigrationType
  -- ^What type of migration this is.
  , _migrationCheck   :: forall m. MonadMigration m => Maybe (m ())
  -- ^Optional action to execute before the primary execution to verify preconditions.
  , _migrationExecute :: forall m. MonadMigration m =>        m ()
  -- ^Main migration action, such as creating tables or updating data.
  }

makeLenses ''Migration

-- |Helper to construct a 'MigrationSchema' type 'Migration' with the given execution action and no check action.
schemaMigration :: Text -> (forall m. MonadMigration m => m ()) -> Migration
schemaMigration key execute = Migration
  { _migrationKey     = key
  , _migrationType    = MigrationSchema
  , _migrationCheck   = Nothing
  , _migrationExecute = execute
  }

-- |Helper to construct a 'MigrationSeedData' type 'Migration' with the given execution action and no check action.
seedDataMigration :: Text -> (forall m. MonadMigration m => m ()) -> Migration
seedDataMigration key execute = Migration
  { _migrationKey     = key
  , _migrationType    = MigrationSeedData
  , _migrationCheck   = Nothing
  , _migrationExecute = execute
  }

-- |Attach a check function to a 'Migration'.
withCheck :: Migration -> (forall m. MonadMigration m => m ()) -> Migration
withCheck m c = m { _migrationCheck = Just c }

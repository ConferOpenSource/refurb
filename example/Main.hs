module Main where

import ClassyPrelude
import Database.PostgreSQL.Simple (Only(Only))
import Refurb
  ( ConnInfo(ConnInfo)
  , Migration, schemaMigration, seedDataMigration, withCheck
  , MonadMigration, doesTableExist, execute_, executeMany
  , refurbMain
  )

migrations :: [Migration]
migrations =
  [ schemaMigration   "example" "create-first-table" createFirstTable
  , seedDataMigration "example" "populate-first-table" populateFirstTable `withCheck` firstTableMustExist
  ]

createFirstTable :: MonadMigration m => m ()
createFirstTable = do
  void $ execute_ "create sequence first_table_seq"
  void $ execute_ "create table first_table (id int not null primary key default nextval('first_table_seq'), t text not null)"

firstTableMustExist :: MonadMigration m => m ()
firstTableMustExist =
  doesTableExist "example" "first_table" >>= bool (fail "first_table doesn't exist!!") (pure ())

populateFirstTable :: MonadMigration m => m ()
populateFirstTable =
  void $ executeMany "insert into first_table (t) values (?)"
    (map (Only . asText) $ words "foo bar baz")

main :: IO ()
main = refurbMain (const . pure $ ConnInfo "localhost" 5432 "example" "example" "example") migrations


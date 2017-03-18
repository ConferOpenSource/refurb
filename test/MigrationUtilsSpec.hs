module MigrationUtilsSpec where

import ClassyPrelude
import Refurb.MigrationUtils
import Test.Hspec (Spec, describe, it, shouldBe)

migrationUtilsSuite :: Spec
migrationUtilsSuite =
  describe "MigrationUtils" $ do
    it "can quote literal SQL" $ do
      [qqSql|foo|] `shouldBe` "foo"

    it "can quote a series of literal SQL statements" $ do
      [qqSqls|foo|] `shouldBe` ["foo\n"]

      [qqSqls|
        do
        a
        thing;
        and another thing
        ;this thing too
        ;but also this;
      |] `shouldBe` ["do\na\nthing\n", "and another thing\n", "this thing too\n", "but also this\n"]

module Data.Depsolver.ParseSpec (spec) where

import TestHelper

import Data.Depsolver.Repository
    ( emptyRepository
    , mkVersion
    )

import Data.Depsolver.Parse (parseRepo, parseVersion)

spec :: Spec
spec = do
  describe "parseRepo" $ do
         it "parses empty repository" $
            parseRepo "[]" `shouldBe` Just emptyRepository
         it "does not parse empty string" $
            parseRepo "" `shouldBe` Nothing
  describe "parseVersion" $ do
         it "does not parse an empty version string" $
            parseVersion "" `shouldBe` Nothing
         it "can parse a single digit" $
            parseVersion "1" `shouldBe` Just (mkVersion ["1"])
         it "can parse two dotted integers" $
            parseVersion "1.1" `shouldBe` Just (mkVersion ["1", "1"])

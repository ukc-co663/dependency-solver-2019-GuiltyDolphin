module Data.Depsolver.ParseSpec (spec) where

import TestHelper

import Data.Depsolver.Repository (emptyRepository)

import Data.Depsolver.Parse (parseRepo)

spec :: Spec
spec = do
  describe "parseRepo" $ do
         it "parses empty repository" $
            parseRepo "[]" `shouldBe` Just emptyRepository
         it "does not parse empty string" $
            parseRepo "" `shouldBe` Nothing

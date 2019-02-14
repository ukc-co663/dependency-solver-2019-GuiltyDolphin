module Data.Depsolver.ParseSpec (spec) where

import Data.List (intersperse)

import TestHelper

import Data.Depsolver.Repository
    ( emptyRepository
    , repoPackages
    , PackageDesc
    , mkPackage
    , packageName
    , packageVersion
    , mkVersion
    )

import Data.Depsolver.Parse (parseRepo, parseVersion)


packageUnspecifiedDepsAndConflicts :: PackageDesc
packageUnspecifiedDepsAndConflicts =
    mkPackage "A" (mkVersion ["1"])


mkPackageString :: String -> String -> String
mkPackageString name version =
    concat ["{\"name\": \"", name, "\", \"version\": \"", version, "\"}"]


mkRepoString :: [String] -> String
mkRepoString packageStrings = '[' : concat (intersperse "," packageStrings) ++ "]"


spec :: Spec
spec = do
  describe "parseRepo" $ do
         it "parses empty repository" $
            parseRepo "[]" `shouldBe` Just emptyRepository
         it "does not parse empty string" $
            parseRepo "" `shouldBe` Nothing
         it "parses repository with no-dependency, no-conflict package" $ do
            let parseRes = fmap repoPackages (parseRepo $ mkRepoString [mkPackageString "A" "1"])
            parseRes `shouldBe` Just [packageUnspecifiedDepsAndConflicts]
            fmap packageName    <$> parseRes `shouldBe` Just ["A"]
            fmap packageVersion <$> parseRes `shouldBe` Just [mkVersion ["1"]]
            parseRes `shouldNotBe` Just [mkPackage "B" (mkVersion ["1"])]
            parseRes `shouldNotBe` Just [mkPackage "A" (mkVersion ["2"])]
  describe "parseVersion" $ do
         it "does not parse an empty version string" $
            parseVersion "" `shouldBe` Nothing
         it "can parse a single digit" $
            parseVersion "1" `shouldBe` Just (mkVersion ["1"])
         it "can parse two dotted integers" $
            parseVersion "1.1" `shouldBe` Just (mkVersion ["1", "1"])

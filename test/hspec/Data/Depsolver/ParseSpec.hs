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
    , packageDependencies
    , packageConflicts
    , mkDependency
    , mkVersion
    )
import qualified Data.Depsolver.Repository as R

import Data.Depsolver.Parse (parseRepo, parseVersion, parseDependency)


verMatchB2 :: R.VersionMatch
verMatchB2 = mkDependency "B" R.VEQ (mkVersion ["2"])


depsB :: [[R.VersionMatch]]
depsB = [[verMatchB2]]


verMatchC71 :: R.VersionMatch
verMatchC71 = mkDependency "C" R.VEQ (mkVersion ["7", "1"])


conflictsC :: [R.VersionMatch]
conflictsC = [verMatchC71]


packageUnspecifiedDepsAndConflicts :: PackageDesc
packageUnspecifiedDepsAndConflicts =
    mkPackage "A" (mkVersion ["1"]) [] []


packageBasic :: PackageDesc
packageBasic =
    mkPackage "A" (mkVersion ["1"]) depsB conflictsC


mkPackageString :: String -> String -> String
mkPackageString name version =
    concat ["{\"name\": \"", name, "\", \"version\": \"", version, "\"}"]


mkPackageStringFull :: String -> String -> [[String]] -> [String] -> String
mkPackageStringFull name version deps conflicts =
    concat [ "{"
           ,       kvs "name" name
           , ", ", kvs "version" version
           , ", ", kv  "depends" (show deps)
           , ", ", kv  "conflicts" (show conflicts)
           , "}"]
    where kv k v  = concat ["\"", k, "\": ", v]
          kvs k v = kv k ('"':v++"\"")


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
            fmap repoPackages (parseRepo $ mkRepoString [mkPackageString "A" "1"])
                     `shouldBe` Just [packageUnspecifiedDepsAndConflicts]
         it "parses repository with basic package" $ do
            let parseRes = fmap repoPackages
                           (parseRepo $ mkRepoString
                                          [mkPackageStringFull "A" "1" [["B=2"]] ["C=7.1"]])
            parseRes `shouldBe` Just [packageBasic]
            fmap packageName    <$> parseRes `shouldBe` Just ["A"]
            fmap packageVersion <$> parseRes `shouldBe` Just [version1]
            fmap packageDependencies <$> parseRes `shouldBe` Just [depsB]
            fmap packageConflicts <$> parseRes `shouldBe` Just [conflictsC]
            parseRes `shouldNotBe` Just [mkPackage "B" version1 depsB conflictsC]
            parseRes `shouldNotBe` Just [mkPackage "A" version2 depsB conflictsC]
            parseRes `shouldNotBe` Just [mkPackage "A" version1 [] conflictsC]
            parseRes `shouldNotBe` Just [mkPackage "A" version1 depsB []]
  describe "parseVersion" $ do
         it "does not parse an empty version string" $
            parseVersion "" `shouldBe` Nothing
         it "can parse a single digit" $
            parseVersion "1" `shouldBe` Just version1
         it "can parse two dotted integers" $
            parseVersion "1.1" `shouldBe` Just (mkVersion ["1", "1"])
  describe "parseDependency" $ do
         it "(=)" $
            parseDependency "A=1" `shouldBe` Just (mkDependency "A" R.VEQ version1)
         it "(>=)" $
            parseDependency "A>=1" `shouldBe` Just (mkDependency "A" R.VGTE version1)
         it "(<=)" $
            parseDependency "A<=1" `shouldBe` Just (mkDependency "A" R.VLTE version1)
         it "(<)" $
            parseDependency "A<1" `shouldBe` Just (mkDependency "A" R.VLT version1)
         it "(>)" $
            parseDependency "A>1" `shouldBe` Just (mkDependency "A" R.VGT version1)
      where version1 = mkVersion ["1"]
            version2 = mkVersion ["2"]

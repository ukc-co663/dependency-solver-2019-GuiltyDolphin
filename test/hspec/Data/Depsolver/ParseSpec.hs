module Data.Depsolver.ParseSpec (spec) where

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
    , emptyRepoState
    , mkDependency
    , mkWildcardDependency
    , mkVersion
    )
import qualified Data.Depsolver.Repository as R
import qualified Data.Depsolver.Repository.Internal as RI

import Data.Depsolver.Parse
    ( parseRepo
    , parseVersion
    , parseDependency
    , parseRepoState
    )


nameA, nameB, nameC :: R.PackageName
nameA = R.mkPackageName "A"
nameB = R.mkPackageName "B"
nameC = R.mkPackageName "C"


verMatchB2 :: R.VersionMatch
verMatchB2 = mkDependency nameB R.VEQ (mkVersion ["2"])


depsB :: [[R.VersionMatch]]
depsB = [[verMatchB2]]


verMatchC71 :: R.VersionMatch
verMatchC71 = mkDependency nameC R.VEQ (mkVersion ["7", "1"])


conflictsC :: [R.VersionMatch]
conflictsC = [verMatchC71]


exampleRepoState1 :: R.RepoState
exampleRepoState1 = R.mkRepoState [R.mkPackageId nameA (mkVersion ["1"])]


exampleRepoState2 :: R.RepoState
exampleRepoState2 =
    R.mkRepoState [ R.mkPackageId nameA (mkVersion ["1"])
                  , R.mkPackageId nameB (mkVersion ["2", "7"])]


packageUnspecifiedDepsAndConflicts :: PackageDesc
packageUnspecifiedDepsAndConflicts =
    mkPackage nameA (mkVersion ["1"]) (RI.mkSize 1) [] []


packageBasic :: PackageDesc
packageBasic =
    mkPackage nameA (mkVersion ["1"]) (RI.mkSize 1) depsB conflictsC


spec :: Spec
spec = do
  describe "(parse . show) is identity" $ do
         -- printing seems to be very slow, so just use small
         -- values for now (we don't need very large cases to
         -- verify)
         let scaleTiny = scale (`min` 5)
             parseTest parser r = parser (show r) == Just r
         it "parseRepo" $ do
           forAll (scaleTiny arbitrary) (parseTest parseRepo)
         it "parseRepoState" $ do
           forAll (scaleTiny arbitrary) (parseTest parseRepoState)
  describe "parseRepo" $ do
         it "parses empty repository" $
            parseRepo "[]" `shouldBe` Just emptyRepository
         it "does not parse empty string" $
            parseRepo "" `shouldBe` Nothing
         it "parses repository with no-dependency, no-conflict package" $ do
            fmap repoPackages (parseRepo $ mkRepoString [mkPackageString "A" "1" "1"])
                     `shouldBe` Just [packageUnspecifiedDepsAndConflicts]
         it "parses repository with basic package" $ do
            let parseRes = fmap repoPackages
                           (parseRepo $ mkRepoStringFromSpecs [("A", "1", "1", [["B=2"]], ["C=7.1"])])
            parseRes `shouldBe` Just [packageBasic]
            fmap packageName    <$> parseRes `shouldBe` Just [nameA]
            fmap packageVersion <$> parseRes `shouldBe` Just [version1]
            fmap packageDependencies <$> parseRes `shouldBe` Just [depsB]
            fmap packageConflicts <$> parseRes `shouldBe` Just [conflictsC]
            parseRes `shouldNotBe` Just [mkPackage nameB version1 size1 depsB conflictsC]
            parseRes `shouldNotBe` Just [mkPackage nameA version2 size1 depsB conflictsC]
            parseRes `shouldNotBe` Just [mkPackage nameA version1 size1 [] conflictsC]
            parseRes `shouldNotBe` Just [mkPackage nameA version1 size1 depsB []]
            parseRes `shouldNotBe` Just [mkPackage nameA version1 size2 depsB conflictsC]
  describe "parseVersion" $ do
         it "does not parse an empty version string" $
            parseVersion "" `shouldBe` Nothing
         it "can parse a single digit" $
            parseVersion "1" `shouldBe` Just version1
         it "can parse two dotted integers" $
            parseVersion "1.1" `shouldBe` Just (mkVersion ["1", "1"])
  describe "parseDependency" $ do
         let mkDep cmp = mkDependency nameA cmp version1
             cmpTest cmpStr cmp =
                 it ("(" ++ cmpStr ++ ")") $
                    parseDependency ("\"A" <> cmpStr <> "1\"")  `shouldBe` Just (mkDep cmp)
         cmpTest "=" R.VEQ
         cmpTest ">=" R.VGTE
         cmpTest "<=" R.VLTE
         cmpTest "<" R.VLT
         cmpTest ">" R.VGT
         it "wildcard" $
            parseDependency "\"A\"" `shouldBe` Just (mkWildcardDependency nameA)
  describe "parseRepoState" $ do
         it "empty repository" $
            parseRepoState "[]" `shouldBe` Just emptyRepoState
         it "repository with a single package" $
            parseRepoState "[\"A=1\"]" `shouldBe` Just exampleRepoState1
         it "repository with two packages" $
            parseRepoState "[\"A=1\", \"B=2.7\"]" `shouldBe` Just exampleRepoState2
      where version1 = mkVersion ["1"]
            version2 = mkVersion ["2"]
            size1 = RI.mkSize 1
            size2 = RI.mkSize 2

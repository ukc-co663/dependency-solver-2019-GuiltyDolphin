module Data.Depsolver.ParseSpec (spec) where

import TestHelper

import qualified Data.Depsolver.Repository as R
import qualified Data.Depsolver.Constraint.Internal as RI
import qualified Data.Depsolver.Repository.Internal as RI
import qualified Data.Depsolver.RepoState.Internal as RI

import Data.Depsolver.Parse
    ( parseRepo
    , parseVersion
    , parseDependency
    , parseRepoState
    , parseConstraints
    )


nameA, nameB, nameC :: R.PackageName
nameA = R.mkPackageName "A"
nameB = R.mkPackageName "B"
nameC = R.mkPackageName "C"


verMatchB2 :: R.VersionMatch
verMatchB2 = RI.mkDependency nameB R.VEQ (RI.mkVersion ["2"])


depsB :: [[R.VersionMatch]]
depsB = [[verMatchB2]]


verMatchC71 :: R.VersionMatch
verMatchC71 = RI.mkDependency nameC R.VEQ (RI.mkVersion ["7", "1"])


conflictsC :: [R.VersionMatch]
conflictsC = [verMatchC71]


exampleRepoState1 :: RI.RepoState
exampleRepoState1 = RI.mkRepoState [R.mkPackageId nameA (RI.mkVersion ["1"])]


exampleRepoState2 :: RI.RepoState
exampleRepoState2 =
    RI.mkRepoState [ R.mkPackageId nameA (RI.mkVersion ["1"])
                   , R.mkPackageId nameB (RI.mkVersion ["2", "7"])]


packageUnspecifiedDepsAndConflicts :: RI.PackageDesc
packageUnspecifiedDepsAndConflicts =
    RI.mkPackage nameA (RI.mkVersion ["1"]) (RI.mkSize 1) [] []


packageBasic :: RI.PackageDesc
packageBasic =
    RI.mkPackage nameA (RI.mkVersion ["1"]) (RI.mkSize 1) depsB conflictsC


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
         it "parseConstraints" $ do
           forAll (scaleTiny arbitrary) (parseTest parseConstraints)
  describe "parseRepo" $ do
         it "parses empty repository" $
            parseRepo "[]" `shouldBe` Just RI.emptyRepository
         it "does not parse empty string" $
            parseRepo "" `shouldBe` Nothing
         it "parses repository with no-dependency, no-conflict package" $ do
            fmap RI.repoPackages (parseRepo $ mkRepoString [mkPackageString "A" "1" "1"])
                     `shouldBe` Just [packageUnspecifiedDepsAndConflicts]
         it "parses repository with basic package" $ do
            let parseRes = fmap RI.repoPackages
                           (parseRepo $ mkRepoStringFromSpecs [("A", "1", "1", [["B=2"]], ["C=7.1"])])
            parseRes `shouldBe` Just [packageBasic]
            fmap RI.packageName    <$> parseRes `shouldBe` Just [nameA]
            fmap RI.packageVersion <$> parseRes `shouldBe` Just [version1]
            fmap RI.packageDependencies <$> parseRes `shouldBe` Just [RI.mkDependencies depsB]
            fmap RI.packageConflicts <$> parseRes `shouldBe` Just [RI.mkConflicts conflictsC]
            parseRes `shouldNotBe` Just [RI.mkPackage nameB version1 size1 depsB conflictsC]
            parseRes `shouldNotBe` Just [RI.mkPackage nameA version2 size1 depsB conflictsC]
            parseRes `shouldNotBe` Just [RI.mkPackage nameA version1 size1 [] conflictsC]
            parseRes `shouldNotBe` Just [RI.mkPackage nameA version1 size1 depsB []]
            parseRes `shouldNotBe` Just [RI.mkPackage nameA version1 size2 depsB conflictsC]
  describe "parseVersion" $ do
         it "does not parse an empty version string" $
            parseVersion "" `shouldBe` Nothing
         it "can parse a single digit" $
            parseVersion "1" `shouldBe` Just version1
         it "can parse two dotted integers" $
            parseVersion "1.1" `shouldBe` Just (RI.mkVersion ["1", "1"])
  describe "parseDependency" $ do
         let mkDep cmp = RI.mkDependency nameA cmp version1
             cmpTest cmpStr cmp =
                 it ("(" ++ cmpStr ++ ")") $
                    parseDependency ("\"A" <> cmpStr <> "1\"")  `shouldBe` Just (mkDep cmp)
         cmpTest "=" R.VEQ
         cmpTest ">=" R.VGTE
         cmpTest "<=" R.VLTE
         cmpTest "<" R.VLT
         cmpTest ">" R.VGT
         it "wildcard" $
            parseDependency "\"A\"" `shouldBe` Just (RI.mkWildcardDependency nameA)
  describe "parseRepoState" $ do
         it "empty repository" $
            parseRepoState "[]" `shouldBe` Just RI.emptyRepoState
         it "repository with a single package" $
            parseRepoState "[\"A=1\"]" `shouldBe` Just exampleRepoState1
         it "repository with two packages" $
            parseRepoState "[\"A=1\", \"B=2.7\"]" `shouldBe` Just exampleRepoState2
  describe "parseConstraints" $ do
         it "empty constraints" $
            parseConstraints "[]" `shouldBe` Just RI.emptyConstraints
         it "single positive wildcard constraint" $
            parseConstraints "[\"+A\"]" `shouldBe` Just exampleConstraints1
         it "single negative wildcard constraint" $
            parseConstraints "[\"-B\"]" `shouldBe` Just exampleConstraints2
         it "a positive and a negative wildcard constraint" $
            parseConstraints "[\"+A\", \"-B\"]" `shouldBe` Just exampleConstraints3
      where exampleConstraints1 = RI.mkConstraints [exampleConstraint1]
            exampleConstraints2 = RI.mkConstraints [exampleConstraint2]
            exampleConstraints3 = RI.mkConstraints [exampleConstraint1, exampleConstraint2]
            exampleConstraint1 = RI.mkPositiveConstraint (RI.mkWildcardDependency nameA)
            exampleConstraint2 = RI.mkNegativeConstraint (RI.mkWildcardDependency nameB)
            version1 = RI.mkVersion ["1"]
            version2 = RI.mkVersion ["2"]
            size1 = RI.mkSize 1
            size2 = RI.mkSize 2

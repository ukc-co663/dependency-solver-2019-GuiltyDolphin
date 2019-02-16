{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestHelper
    ( module Test.Hspec
    , module Test.QuickCheck
    , gen2
    , repoWithoutPackage
    , repoWithoutPackageVersion
    , repoWithDependency
    , repoStateWithPackage
    , repoStateWithPackageVersion
    , mkPackageString
    , mkPackageStringFull
    , mkRepoString
    , mkRepoStringFromSpecs
    , repoExamples
    , getExampleRepo
    , getExampleValidStates
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.List (foldl', intersperse)
import Data.Maybe (fromJust)

import qualified Data.Depsolver.Parse as P
import qualified Data.Depsolver.Repository.Internal as RI


data RepoExample = RepoExample
    { getExampleRepo :: RI.Repository
    , getExampleValidStates :: [RI.RepoState]
    }


mkExampleRepo :: [(String, String, [[String]], [String])] -> RI.Repository
mkExampleRepo = fromJust . P.parseRepo . mkRepoStringFromSpecs


mkExampleRepoState :: [(String, String)] -> RI.RepoState
mkExampleRepoState = fromJust . P.parseRepoState . mkRepoStateString


repoExamples :: [RepoExample]
repoExamples = fmap (uncurry RepoExample)
               [ repoExample1
               , repoExample2
               , repoExample3 ]
    where repoExample1 = ( mkExampleRepo [("A", "1.7", [], [])]
                         , [mkExampleRepoState [("A", "1.7")]] )
          repoExample2 = ( mkExampleRepo [ ("A", "1.7", [["B=2"]], [])
                                         , ("B", "2", [], []) ]
                         , fmap mkExampleRepoState [ [("A", "1.7"), ("B", "2")]
                                                   , [("B", "2")] ] )
          repoExample3 = ( mkExampleRepo [ ("A", "1", [["B=2"], ["C=3"]], [])
                                         , ("B", "2", [], [])
                                         , ("C", "3", [], []) ]
                         , fmap mkExampleRepoState [ [("A", "1"), ("B", "2"), ("C", "3")]
                                                   , [("A", "1"), ("B", "2")]
                                                   , [("A", "1"), ("C", "3")]
                                                   , [("B", "2"), ("C", "3")]
                                                   , [("B", "2")]
                                                   , [("C", "3")] ] )


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


jarryStr :: [String] -> String
jarryStr = brackets . concat . intersperse ","
    where brackets s = '[' : s ++ "]"


mkRepoString :: [String] -> String
mkRepoString = jarryStr


mkRepoStringFromSpecs :: [(String, String, [[String]], [String])] -> String
mkRepoStringFromSpecs =
    mkRepoString . map (\(name, version, depends, conflicts) ->
                            mkPackageStringFull name version depends conflicts)


mkRepoStateString :: [(String, String)] -> String
mkRepoStateString = jarryStr . fmap (\(p, v) -> concat ["\"", p, "=", v, "\""])


gen2 :: (Gen a, Gen b) -> Gen (a, b)
gen2 (g1, g2) = do
  x <- g1
  y <- g2
  pure (x, y)


deriving instance Arbitrary RI.Repository


deleteRepoPackagesBy :: (RI.PackageDesc -> Bool) -> RI.Repository -> RI.Repository
deleteRepoPackagesBy p = RI.mkRepository . filter p . RI.repoPackages


-- | Generate a repository that is guaranteed not to contain
-- | the given package name.
repoWithoutPackage :: RI.PackageName -> Gen RI.Repository
repoWithoutPackage pname = deleteRepoPackageByName pname <$> arbitrary
    where deleteRepoPackageByName n = deleteRepoPackagesBy ((/=n) . RI.packageName)


-- | Generate a repository that is guaranteed not to contain
-- | the given package with the specified version.
repoWithoutPackageVersion :: RI.PackageVersion -> Gen RI.Repository
repoWithoutPackageVersion (RI.PackageVersion (pname, version)) =
    deleteRepoPackageByNameVersion <$> arbitrary
    where deleteRepoPackageByNameVersion =
              RI.mkRepository . filter notPV . RI.repoPackages
          notPV p = RI.packageName p /= pname && RI.packageVersion p /= version


-- | Generate a repository that is guaranteed to contain
-- | a dependency between the first and second packages.
repoWithDependency :: RI.PackageVersion -> RI.PackageDesc -> Gen RI.Repository
repoWithDependency (RI.PackageVersion (p1name, p1version)) p2 =
    let dep = RI.mkDependency (RI.packageName p2) RI.VEQ (RI.packageVersion p2)
        p1 = RI.mkPackage p1name p1version [[dep]] []
    in repoWithPackages [p1, p2]
    where repoWithPackages ps = do
            repo <- arbitrary
            pure $ foldl' addPackage repo ps
          addPackage repo p = RI.mkRepository . (p:) $ RI.repoPackages repo


instance Arbitrary RI.PackageDesc where
    arbitrary = do
      name <- arbitrary
      version <- arbitrary
      deps <- arbitrary
      conflicts <- arbitrary
      pure $ RI.PackageDesc {
                  RI.packageName = name
                , RI.packageVersion = version
                , RI.packageDependencies = deps
                , RI.packageConflicts = conflicts
                }


instance Arbitrary RI.PackageName where
    arbitrary = fmap RI.mkPackageName $ listOf1 (elements pnameChar)
        where pnameChar = '.' : '-' : ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']


deriving instance Arbitrary RI.RepoState


-- | Generate a repository state that is guaranteed to have
-- | an entry for the given package.
repoStateWithPackage :: RI.PackageName -> Gen RI.RepoState
repoStateWithPackage pname = do
  version <- arbitrary
  repoStateWithPackageVersion (RI.mkPackageVersion pname version)


-- | Generate a repository state that is guaranteed to have
-- | an entry for the given package.
repoStateWithPackageVersion :: RI.PackageVersion -> Gen RI.RepoState
repoStateWithPackageVersion p = do
  fmap (addRepoStatePackage p) arbitrary
    where addRepoStatePackage pv =
              RI.mkRepoState . (pv:) . RI.repoStatePackageVersions


deriving instance Arbitrary RI.PackageVersion


instance Arbitrary RI.Version where
    arbitrary = fmap RI.mkVersion randNums
        where randNums = listOf1 randVerNum
              randVerNum = listOf1 verDigit
              verDigit = elements ['0'..'9']


instance Arbitrary RI.VersionMatch where
    arbitrary = do
      name <- arbitrary
      compType <- arbitrary
      version <- arbitrary
      pure $ RI.VersionMatch name compType version


instance Arbitrary RI.VersionCmp where
    arbitrary = elements [RI.VLTE, RI.VLT, RI.VEQ, RI.VGT, RI.VGTE]

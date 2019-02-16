{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestHelper
    ( module Test.Hspec
    , module Test.QuickCheck
    , gen2
    , repoWithoutPackage
    , repoWithoutPackageVersion
    , repoWithDependency
    , repoWithConflict
    , repoStateWithPackage
    , repoStateWithPackageVersion
    , repoStateWithPackageVersions
    , mkPackageString
    , mkPackageStringFull
    , mkRepoString
    , mkRepoStringFromSpecs
    , repoExamples
    , getExampleRepo
    , getExampleValidStates
    , fileCaseExamples
    ) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl', intersperse)
import Data.Maybe (fromJust)
import System.FilePath ((</>))

import qualified System.FilePath.Find as Find
import System.FilePath.Find ((==?), (&&?), (~~?), always)

import qualified Data.Depsolver.Parse as P
import qualified Data.Depsolver.Repository.Internal as RI


fileCaseExamples :: (MonadIO m) => m [(FilePath, (RI.Repository, RI.RepoState))]
fileCaseExamples = do
  directories <- liftIO $ Find.find always (Find.fileType ==? Find.Directory
                                           &&? Find.fileName ~~? "(seen|example)-*") testCasesDir
  let exampleFiles = map (\d -> (d, (d </> "repository.json", d </> "initial.json"))) directories
  mapM (\(d, (repoFile, repoStateFile)) -> do
          repoS <- liftIO $ readFile repoFile
          repoStateS <- liftIO $ readFile repoStateFile
          let Just repo = P.parseRepo repoS
              Just repoState = P.parseRepoState repoStateS
          pure (d, (repo, repoState))) exampleFiles
  where testCasesDir = "test/cases"


data RepoExample = RepoExample
    { getExampleRepo :: RI.Repository
    , getExampleValidStates :: [RI.RepoState]
    }


mkExampleRepo :: [(String, String, [[String]], [String])] -> RI.Repository
mkExampleRepo = fromJust . P.parseRepo . mkRepoStringFromSpecs


mkExampleRepoState :: [(String, String)] -> RI.RepoState
mkExampleRepoState = fromJust . P.parseRepoState . mkRepoStateString


repoExamples :: [RepoExample]
repoExamples = fmap ((\(r, rs) -> RepoExample (mkExampleRepo r) (fmap mkExampleRepoState rs)))
               [ repoExample1
               , repoExample2
               , repoExample3 ]
    where repoExample1 =
              ( [("A", "1.7", [], [])]
              , [[("A", "1.7")]] )
          repoExample2 =
              ( [ ("A", "1.7", [["B=2"]], [])
                , ("B", "2", [], [])
                ]
              , [ [("A", "1.7"), ("B", "2")]
                , [("B", "2")] ] )
          repoExample3 =
              ( [ ("A", "1", [["B=2"], ["C=3"]], [])
                , ("B", "2", [], [])
                , ("C", "3", [], [])
                ]
              , [ [("A", "1"), ("B", "2"), ("C", "3")]
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


-- | Generate a repository that is guaranteed to contain the
-- | given packages.
repoWithPackages :: [RI.PackageDesc] -> Gen RI.Repository
repoWithPackages ps = do
  repo <- arbitrary
  pure $ foldl' addPackage repo ps
  where addPackage repo p = RI.mkRepository . (p:) $ RI.repoPackages repo


-- | Generate a repository that is guaranteed to contain
-- | a dependency between the first and second packages.
repoWithDependency :: RI.PackageVersion -> RI.PackageDesc -> Gen RI.Repository
repoWithDependency (RI.PackageVersion (p1name, p1version)) p2 =
    let dep = RI.mkDependency (RI.packageName p2) RI.VEQ (RI.packageVersion p2)
        p1 = RI.mkPackage p1name p1version [[dep]] []
    in repoWithPackages [p1, p2]


-- | Generate a repository that is guaranteed to contain
-- | a conflict between the first and second packages.
repoWithConflict :: RI.PackageVersion -> RI.PackageVersion -> Gen RI.Repository
repoWithConflict (RI.PackageVersion (p1name, p1version)) (RI.PackageVersion (p2name, p2version)) =
    let dep = RI.mkDependency p2name RI.VEQ p2version
        p1 = RI.mkPackage p1name p1version [] [dep]
        p2 = RI.mkPackage p2name p2version [] []
    in repoWithPackages [p1, p2]


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
repoStateWithPackageVersion p = repoStateWithPackageVersions [p]


-- | Generate a repository state that is guaranteed to have
-- | an entry for each of the given package versions.
repoStateWithPackageVersions :: [RI.PackageVersion] -> Gen RI.RepoState
repoStateWithPackageVersions pvs = do
  repoState <- arbitrary
  pure $ foldl' addRepoStatePackage repoState pvs
    where addRepoStatePackage rs pv =
              RI.mkRepoState . (pv:) . RI.repoStatePackageVersions $ rs


deriving instance Arbitrary RI.PackageVersion


instance Arbitrary RI.Version where
    arbitrary = fmap RI.mkVersion randNums
        where randNums = listOf1 randVerNum
              randVerNum = listOf1 verDigit
              verDigit = elements ['0'..'9']


instance Arbitrary RI.VersionMatch where
    arbitrary = do
      name <- arbitrary
      frequency  $ [ (7, do
                        compType <- arbitrary
                        version <- arbitrary
                        pure $ RI.VersionMatch name compType version)
                   , (3, pure $ RI.VersionMatchWild name) ]


instance Arbitrary RI.VersionCmp where
    arbitrary = elements [RI.VLTE, RI.VLT, RI.VEQ, RI.VGT, RI.VGTE]

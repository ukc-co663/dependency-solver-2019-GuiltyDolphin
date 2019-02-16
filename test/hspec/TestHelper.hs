{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module TestHelper
    ( module Test.Hspec
    , module Test.QuickCheck
    , gen2
    , RepoGen
    , execRepoGen
    , runRepoGen
    , makeDependency
    , makeConflict
    , makeWildConflict
    , withoutPackage
    , withoutPackageName
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
import Control.Monad.State (State, execState, get, modify, runState)
import Data.List (find, foldl', intersperse)
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
               , repoExample3
               , repoExample4
               , repoExample5
               ]
    where a1 = ("A", "1")
          b2 = ("B", "2")
          b3 = ("B", "3")
          c3 = ("C", "3")
          b2p = ("B", "2", [], [])
          b3p = ("B", "3", [], [])
          c3p = ("C", "3", [], [])
          repoExample1 =
              ( [("A", "1", [], [])]
              , [[a1]] )
          repoExample2 =
              ( [ ("A", "1", [["B=2"]], []), b2p ]
              , [ [a1, b2]
                , [b2] ] )
          repoExample3 =
              ( [ ("A", "1", [["B=2"], ["C=3"]], []), b2p, c3p ]
              , [ [a1, b2, c3]
                , [a1, b2]
                , [a1, c3]
                , [b2, c3]
                , [b2]
                , [c3] ] )
          repoExample4 =
              ( [ ("A", "1", [["B=2"]], []), b2p, b3p ]
              , [ [a1, b2, b3]
                , [a1, b2]
                , [b2, b3]
                , [b2]
                , [b3] ] )
          repoExample5 =
              ( [ ("A", "1", [["B"]], []), b2p, b3p ]
              , [ [a1, b2, b3]
                , [a1, b2]
                , [a1, b3]
                , [b2, b3]
                , [b2]
                , [b3] ] )


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


type RepoGen = State RI.Repository


runRepoGen :: (Arbitrary a) => RepoGen a -> Gen (RI.Repository, a)
runRepoGen rg = fmap ((\(x,y) -> (y,x)) . runState rg) arbitrary


execRepoGen :: RepoGen a -> Gen RI.Repository
execRepoGen rg = fmap (execState rg) arbitrary


deriving instance Arbitrary RI.Repository


removePackagesBy :: (RI.PackageDesc -> Bool) -> RepoGen ()
removePackagesBy p = modifyPackages (filter (not . p))


withoutPackageName :: RI.PackageName -> RepoGen ()
withoutPackageName name = removePackagesBy ((==name) . RI.packageName)


withoutPackage :: RI.PackageVersion -> RepoGen ()
withoutPackage pv = removePackagesBy ((==pv) . RI.toPackageVersion)


lookupOrNew :: RI.PackageVersion -> RepoGen RI.PackageDesc
lookupOrNew pv = do
  repo <- get
  case find ((==pv) . RI.toPackageVersion) (RI.repoPackages repo) of
    Just p -> pure p
    Nothing -> let (RI.PackageVersion (pname, pversion)) = pv
                   p = RI.mkPackage pname pversion [] []
               in putPackage p >> pure p


modifyPackages :: ([RI.PackageDesc] -> [RI.PackageDesc]) -> RepoGen ()
modifyPackages f = modify (RI.mkRepository . f . RI.repoPackages)


putPackage :: RI.PackageDesc -> RepoGen ()
putPackage p =
    modifyPackages insertPackageUniquely
    where insertPackageUniquely = (p:) . filter ((not . equalPV p))
          equalPV p1 p2 = RI.toPackageVersion p1 == RI.toPackageVersion p2


makeConflict :: RI.PackageVersion -> RI.PackageVersion -> RepoGen ()
makeConflict p1pv (RI.PackageVersion (p2name, p2version)) = do
    p1 <- lookupOrNew p1pv
    let dep = RI.mkDependency p2name RI.VEQ p2version
        p1' = RI.mkPackage (RI.packageName p1) (RI.packageVersion p1)
              (RI.packageDependencies p1) (dep : RI.packageConflicts p1)
    putPackage p1'


makeWildConflict :: RI.PackageVersion -> RI.PackageVersion -> RepoGen ()
makeWildConflict p1pv (RI.PackageVersion (p2name, _)) = do
    p1 <- lookupOrNew p1pv
    let dep = RI.mkWildcardDependency p2name
        p1' = RI.mkPackage (RI.packageName p1) (RI.packageVersion p1)
              (RI.packageDependencies p1) (dep : RI.packageConflicts p1)
    putPackage p1'


makeDependency :: RI.PackageVersion -> RI.PackageVersion -> RepoGen ()
makeDependency p1pv (RI.PackageVersion (p2name, p2version)) = do
    p1 <- lookupOrNew p1pv
    let dep = RI.mkDependency p2name RI.VEQ p2version
        p1' = RI.mkPackage (RI.packageName p1) (RI.packageVersion p1)
              ([dep] : RI.packageDependencies p1) (RI.packageConflicts p1)
    putPackage p1'


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

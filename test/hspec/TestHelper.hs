{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , newPackage
    , withoutPackage
    , withoutPackageName
    , repoStateWithPackage
    , repoStateWithPackageVersions
    , repoStateContaining
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
import qualified QuickCheck.GenT as GenT
import QuickCheck.GenT (liftGen)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, State, get, modify, runState, state, lift)
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


class ArbyRepo a where
    arby :: [(RI.PackageVersion)] -> Gen a


instance ArbyRepo a => ArbyRepo [a] where
    arby pvs = listOf (arby pvs)


gen2 :: (Gen a, Gen b) -> Gen (a, b)
gen2 (g1, g2) = do
  x <- g1
  y <- g2
  pure (x, y)


newtype RepoGen a = RepoGen { unRepoGen :: GenT.GenT (State RI.Repository) a }
    deriving (Functor, Applicative, Monad, GenT.MonadGen)


instance MonadState s (GenT.GenT (State s)) where
    state = lift . state


deriving instance (MonadState RI.Repository RepoGen)


runRepoGen :: RepoGen a -> Gen (RI.Repository, a)
runRepoGen rg = do
    gens <- GenT.runGenT $ unRepoGen rg
    let (a, r) = runState gens RI.emptyRepository
    pure (r, a)


execRepoGen :: RepoGen a -> Gen RI.Repository
execRepoGen = fmap fst . runRepoGen


instance Arbitrary RI.Repository where
    arbitrary = do
      pvs <- arbitrary
      fmap RI.mkRepository $ mapM (arbyPackage pvs) pvs
      where arbyPackage pvs (RI.PackageVersion (pname, pversion)) = do
                                   deps <- arby pvs
                                   conflicts <- arby pvs
                                   pure $ RI.mkPackage pname pversion deps conflicts


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


-- | Create a new package with no dependencies or conflicts.
-- | If a package with the given identifier already exists,
-- | it is overwritten.
newPackage :: RI.PackageVersion -> RepoGen ()
newPackage (RI.PackageVersion (pname, pversion)) =
    let p = RI.mkPackage pname pversion [] []
    in putPackage p


putPackage :: RI.PackageDesc -> RepoGen ()
putPackage p =
    modifyPackages insertPackageUniquely
    where insertPackageUniquely = (p:) . filter ((not . equalPV p))
          equalPV p1 p2 = RI.toPackageVersion p1 == RI.toPackageVersion p2


makeConflict :: RI.PackageVersion -> RI.PackageVersion -> RepoGen ()
makeConflict p1pv p2pv = do
    p1 <- lookupOrNew p1pv
    p2 <- lookupOrNew p2pv
    let dep = RI.mkDependency (RI.packageName p2) RI.VEQ (RI.packageVersion p2)
        p1' = RI.mkPackage (RI.packageName p1) (RI.packageVersion p1)
              (RI.packageDependencies p1) (dep : RI.packageConflicts p1)
    putPackage p1'


makeWildConflict :: RI.PackageVersion -> RI.PackageVersion -> RepoGen ()
makeWildConflict p1pv p2pv = do
    p1 <- lookupOrNew p1pv
    p2 <- lookupOrNew p2pv
    let dep = RI.mkWildcardDependency (RI.packageName p2)
        p1' = RI.mkPackage (RI.packageName p1) (RI.packageVersion p1)
              (RI.packageDependencies p1) (dep : RI.packageConflicts p1)
    putPackage p1'


makeDependency :: RI.PackageVersion -> [RI.PackageVersion] -> RepoGen ()
makeDependency p1pv p2pvs = do
    p1 <- lookupOrNew p1pv
    targets <- mapM lookupOrNew p2pvs
    let deps = fmap (\target -> RI.mkDependency (RI.packageName target) RI.VEQ (RI.packageVersion target)) targets
        p1' = RI.mkPackage (RI.packageName p1) (RI.packageVersion p1)
              (deps : RI.packageDependencies p1) (RI.packageConflicts p1)
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


instance ArbyRepo RI.PackageName where
    arby = elements . fmap pvName
        where pvName = fst . RI.getPackageVersion


deriving instance Arbitrary RI.RepoState


-- | Generate a repository state that is guaranteed to have
-- | an entry for the given package.
repoStateWithPackage :: RI.PackageName -> Gen RI.RepoState
repoStateWithPackage pname = do
  version <- arbitrary
  pure $ repoStateWithPackageVersions [RI.mkPackageVersion pname version]


-- | Create a new repository state with the given package versions.
repoStateWithPackageVersions :: [RI.PackageVersion] -> RI.RepoState
repoStateWithPackageVersions pvs =
  let repoState = RI.emptyRepoState
  in foldl' addRepoStatePackage repoState pvs
    where addRepoStatePackage rs pv =
              RI.mkRepoState . (pv:) . RI.repoStatePackageVersions $ rs


repoStateContaining :: RI.Repository -> [RI.PackageVersion] -> Gen RI.RepoState
repoStateContaining repo pvs = do
  let allowedPackages = fmap RI.toPackageVersion $ RI.repoPackages repo
  arbyPvs <- sublistOf allowedPackages
  pure $ RI.mkRepoState (pvs <> arbyPvs)


deriving instance Arbitrary RI.PackageVersion


instance Arbitrary RI.Version where
    arbitrary = fmap RI.mkVersion randNums
        where randNums = listOf1 randVerNum
              randVerNum = listOf1 verDigit
              verDigit = elements ['0'..'9']


instance ArbyRepo RI.Version where
    arby = elements . fmap pvVersion
        where pvVersion = snd . RI.getPackageVersion


instance Arbitrary RI.VersionMatch where
    arbitrary = do
      name <- arbitrary
      frequency  $ [ (7, do
                        compType <- arbitrary
                        version <- arbitrary
                        pure $ RI.VersionMatch name compType version)
                   , (3, pure $ RI.VersionMatchWild name) ]


instance ArbyRepo RI.VersionMatch where
    arby pvs = do
      name <- arby pvs
      frequency [ (7, do
                     compType <- arbitrary
                     version <- frequency [(7, arby pvs), (3, arbitrary)]
                     pure $ RI.VersionMatch name compType version)
                , (3, pure $ RI.VersionMatchWild name) ]


instance Arbitrary RI.VersionCmp where
    arbitrary = elements [RI.VLTE, RI.VLT, RI.VEQ, RI.VGT, RI.VGTE]

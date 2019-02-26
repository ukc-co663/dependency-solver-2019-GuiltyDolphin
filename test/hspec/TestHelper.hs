{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TestHelper
    ( module Test.Hspec
    , module Test.QuickCheck
    , module Control.Monad.State
    , gen2
    , RepoGen
    , execRepoGen
    , runRepoGen
    , genNewPackage
    , genNewPackageWithVersion
    , makeDependency
    , makeDependencies
    , makeConflictOp
    , makeConflict
    , makeWildConflict
    , versionSat
    , deletePackage
    , repoStateWithPackages
    , stateElem
    , arbyState
    , mkPackageString
    , mkRepoString
    , mkRepoStringFromSpecs
    , repoExamples
    , getExampleRepo
    , getExampleValidStates
    , fileCaseExamples

    -- ** RepoGen helpers
    , with2NewPackages
    , gen2packages
    , gen3packages
    , genNewDependency
    , makeDependency1
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
import qualified Data.Depsolver.Constraint.Internal as RI
import qualified Data.Depsolver.Repository.Internal as RI
import qualified Data.Depsolver.RepoState.Internal as RI


fileCaseExamples :: (MonadIO m) => m [(FilePath, (RI.Repository, RI.RepoState, RI.Constraints))]
fileCaseExamples = do
  directories <- liftIO $ Find.find always (Find.fileType ==? Find.Directory
                                           &&? Find.fileName ~~? "(seen|example)-*") testCasesDir
  let exampleFiles = map (\d -> (d, (d </> "repository.json", d </> "initial.json", d </> "constraints.json"))) directories
  mapM (\(d, (repoFile, repoStateFile, constraintFile)) -> do
          repoS <- liftIO $ readFile repoFile
          repoStateS <- liftIO $ readFile repoStateFile
          constraintsS <- liftIO $ readFile constraintFile
          let Just repo = P.parseRepo repoS
              Just repoState = P.parseRepoState repoStateS
              Just constraints = P.parseConstraints constraintsS
          pure (d, (repo, repoState, constraints))) exampleFiles
  where testCasesDir = "test/cases"


data RepoExample = RepoExample
    { getExampleRepo :: RI.Repository
    , getExampleValidStates :: [RI.RepoState]
    }


mkExampleRepo :: [(String, String, String, [[String]], [String])] -> RI.Repository
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
          b2p = ("B", "2", "100", [], [])
          b3p = ("B", "3", "200", [], [])
          c3p = ("C", "3", "300", [], [])
          repoExample1 =
              ( [("A", "1", "1", [], [])]
              , [[a1]] )
          repoExample2 =
              ( [ ("A", "1", "1", [["B=2"]], []), b2p ]
              , [ [a1, b2]
                , [b2] ] )
          repoExample3 =
              ( [ ("A", "1", "1", [["B=2"], ["C=3"]], []), b2p, c3p ]
              , [ [a1, b2, c3]
                , [a1, b2]
                , [a1, c3]
                , [b2, c3]
                , [b2]
                , [c3] ] )
          repoExample4 =
              ( [ ("A", "1", "1", [["B=2"]], []), b2p, b3p ]
              , [ [a1, b2, b3]
                , [a1, b2]
                , [b2, b3]
                , [b2]
                , [b3] ] )
          repoExample5 =
              ( [ ("A", "1", "1", [["B"]], []), b2p, b3p ]
              , [ [a1, b2, b3]
                , [a1, b2]
                , [a1, b3]
                , [b2, b3]
                , [b2]
                , [b3] ] )


-- | Helper for building a JSON key-value pair.
kv :: String -> String -> String
kv k v = concat ["\"", k, "\": ", v]


-- | Helper for building a JSON key-value pair
-- | where the value is a string.
kvs :: String -> String -> String
kvs k v = kv k ('"':v++"\"")


mkPackageString :: String -> String -> String -> String
mkPackageString name version size =
    concat [ "{"
           ,       kvs "name" name
           , ", ", kvs "version" version
           , ", ", kv  "size" size
           , "}"]


mkPackageStringFull :: String -> String -> String -> [[String]] -> [String] -> String
mkPackageStringFull name version size deps conflicts =
    concat [ "{"
           ,       kvs "name" name
           , ", ", kvs "version" version
           , ", ", kv  "size" size
           , ", ", kv  "depends" (show deps)
           , ", ", kv  "conflicts" (show conflicts)
           , "}"]


jarryStr :: [String] -> String
jarryStr = brackets . concat . intersperse ","
    where brackets s = '[' : s ++ "]"


mkRepoString :: [String] -> String
mkRepoString = jarryStr


mkRepoStringFromSpecs :: [(String, String, String, [[String]], [String])] -> String
mkRepoStringFromSpecs =
    mkRepoString . map (\(name, version, size, depends, conflicts) ->
                            mkPackageStringFull name version size depends conflicts)


mkRepoStateString :: [(String, String)] -> String
mkRepoStateString = jarryStr . fmap (\(p, v) -> concat ["\"", p, "=", v, "\""])


class ArbyRepo a where
    arby :: [RI.PackageId] -> Gen a


arbyRepo :: (ArbyRepo a) => RI.Repository -> Gen a
arbyRepo = arby . fmap RI.packageId . RI.repoPackages


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
      where arbyPackage pvs (RI.PackageId (pname, pversion)) = do
              size <- arbitrary
              deps <- arby pvs
              conflicts <- arby pvs
              pure $ RI.mkPackage pname pversion size deps conflicts


getPackageIds :: RepoGen [RI.PackageId]
getPackageIds = fmap (fmap RI.packageId . RI.repoPackages) get


removePackagesBy :: (RI.PackageDesc -> Bool) -> RepoGen ()
removePackagesBy p = modifyPackages (filter (not . p))


deletePackage :: RI.PackageId -> RepoGen ()
deletePackage pv = removePackagesBy ((==pv) . RI.packageId)


lookupOrNew :: RI.PackageId -> RepoGen RI.PackageDesc
lookupOrNew pv = do
  repo <- get
  case find ((==pv) . RI.packageId) (RI.repoPackages repo) of
    Just p -> pure p
    Nothing -> newPackage pv


modifyPackages :: ([RI.PackageDesc] -> [RI.PackageDesc]) -> RepoGen ()
modifyPackages f = modify (RI.mkRepository . f . RI.repoPackages)


-- | Create a new package with no dependencies or conflicts.
-- | If a package with the given identifier already exists,
-- | it is overwritten.
newPackage :: RI.PackageId -> RepoGen RI.PackageDesc
newPackage (RI.PackageId (pname, pversion)) = do
  size <- liftGen arbitrary
  let p = RI.mkPackage pname pversion size [] []
  putPackage p >> pure p


-- | Generate a new package with a unique combination
-- | of name and version. The package will have no
-- | conflicts or dependencies.
genNewPackage :: RepoGen RI.PackageId
genNewPackage = do
  pvs <- getPackageIds
  pv <- liftGen (arbitrary `suchThat` (`notElem` pvs))
  newPackage pv >> pure pv


-- | Generate a new package with a unique combination
-- | of name and (the given) version. The package will
-- | have no conflicts or dependencies.
genNewPackageWithVersion :: RI.Version -> RepoGen RI.PackageId
genNewPackageWithVersion v = do
  pvs <- getPackageIds
  name <- liftGen (arbitrary `suchThat` (\n -> (RI.mkPackageId n v) `notElem` pvs))
  let pv = RI.mkPackageId name v
  newPackage pv >> pure pv


putPackage :: RI.PackageDesc -> RepoGen ()
putPackage p =
    modifyPackages insertPackageUniquely
    where insertPackageUniquely = (p:) . filter ((not . equalPV p))
          equalPV p1 p2 = RI.packageId p1 == RI.packageId p2


addConflict :: RI.PackageDesc -> RI.VersionMatch -> RI.PackageDesc
addConflict p c = p { RI.packageConflicts = RI.mkConflicts [c] <> RI.packageConflicts p }


makeConflictOp :: RI.VersionCmp -> RI.PackageId -> RI.PackageId -> RI.Version -> RepoGen ()
makeConflictOp op p1pv p2pv v = do
    p1 <- lookupOrNew p1pv
    p2 <- lookupOrNew p2pv
    let dep = RI.mkDependency (RI.packageName p2) op v
        p1' = addConflict p1 dep
    putPackage p1'


makeConflict :: RI.PackageId -> RI.PackageId -> RepoGen ()
makeConflict p1pv p2pv = do
  p2v <- fmap RI.packageVersion $ lookupOrNew p2pv
  makeConflictOp RI.VEQ p1pv p2pv p2v


makeWildConflict :: RI.PackageId -> RI.PackageId -> RepoGen ()
makeWildConflict p1pv p2pv = do
    p1 <- lookupOrNew p1pv
    p2 <- lookupOrNew p2pv
    let dep = RI.mkWildcardDependency (RI.packageName p2)
        p1' = addConflict p1 dep
    putPackage p1'


makeDependencies :: RI.PackageId -> [[RI.PackageId]] -> RepoGen ()
makeDependencies p1pv p2pvs = do
    p1 <- lookupOrNew p1pv
    targets <- mapM (mapM lookupOrNew) p2pvs
    let deps = fmap (fmap (\target -> RI.mkDependency (RI.packageName target) RI.VEQ (RI.packageVersion target))) targets
        p1' = p1 { RI.packageDependencies = RI.mkDependencies deps <> RI.packageDependencies p1 }
    putPackage p1'


makeDependency :: RI.PackageId -> [RI.PackageId] -> RepoGen ()
makeDependency p1pv p2pvs = makeDependencies p1pv [p2pvs]


-- | Generate a new version satisfying the given predicate.
versionSat :: (RI.Version -> Bool) -> RepoGen RI.Version
versionSat p = liftGen (arbitrary `suchThat` p)


instance Arbitrary RI.PackageDesc where
    arbitrary = do
      name <- arbitrary
      version <- arbitrary
      size <- arbitrary
      deps <- arbitrary
      conflicts <- arbitrary
      pure $ RI.PackageDesc {
                  RI.packageName = name
                , RI.packageVersion = version
                , RI.packageSize = size
                , RI.packageDependencies = deps
                , RI.packageConflicts = conflicts
                }


instance Arbitrary RI.PackageName where
    arbitrary = fmap RI.mkPackageName $ listOf1 (elements pnameChar)
        where pnameChar = '.' : '-' : ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']


instance ArbyRepo RI.PackageName where
    arby = elements . fmap RI.packageIdName


instance Arbitrary RI.Size where
    -- sizes must be positive
    arbitrary = fmap (RI.mkSize . getPositive) arbitrary
    shrink = fmap (RI.mkSize . getPositive) . shrink . Positive . RI.fromSize


instance Arbitrary RI.Dependencies where
    arbitrary = fmap RI.mkDependencies' arbitrary
    shrink = fmap RI.mkDependencies' . shrink . RI.fromDependencies


instance Arbitrary RI.Conflicts where
    arbitrary = fmap RI.mkConflicts' arbitrary
    shrink = fmap RI.mkConflicts' . shrink . RI.fromConflicts


instance Arbitrary RI.RepoState where
    arbitrary = fmap RI.mkRepoState' arbitrary
    shrink = fmap RI.mkRepoState' . shrink . RI.fromRepoState


-- | Create a new repository state with the given package versions.
repoStateWithPackages :: [RI.PackageId] -> RI.RepoState
repoStateWithPackages pvs =
  let repoState = RI.emptyRepoState
  in foldl' (flip RI.installPackage) repoState pvs


stateElem :: RI.PackageId -> RI.RepoState -> Bool
stateElem pv = (pv `elem`) . RI.repoStatePackageIds


arbyState :: RepoGen RI.RepoState
arbyState = do
  repo <- get
  let allowedPackages = fmap RI.packageId $ RI.repoPackages repo
  liftGen (fmap RI.mkRepoState $ sublistOf allowedPackages)


deriving instance Arbitrary RI.PackageId


instance Arbitrary RI.Version where
    arbitrary = fmap RI.mkVersion randNums
        where randNums = listOf1 randVerNum
              randVerNum = listOf1 verDigit
              verDigit = elements ['0'..'9']


instance ArbyRepo RI.Version where
    arby = elements . fmap RI.packageIdVersion


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


instance Arbitrary RI.Constraints where
    arbitrary = fmap RI.mkConstraints arbitrary
    shrink = fmap RI.mkConstraints . shrink . RI.fromConstraints


instance ArbyRepo RI.Constraints where
    arby [] = pure $ RI.mkConstraints []
    arby ps = fmap RI.mkConstraints . arby $ ps


instance Arbitrary RI.Constraint where
    arbitrary = do
      dep <- arbitrary
      wanted <- arbitrary
      pure $ (if wanted
              then RI.mkPositiveConstraint
              else RI.mkNegativeConstraint) dep


instance ArbyRepo RI.Constraint where
    arby rs = do
      dep <- arby rs
      wanted <- arbitrary
      pure $ (if wanted
              then RI.mkPositiveConstraint
              else RI.mkNegativeConstraint) dep


---------------------------
----- RepoGen Helpers -----
---------------------------


type PackagePair = (RI.PackageId, RI.PackageId)


makeDependency1 :: PackagePair -> RepoGen ()
makeDependency1 (p1, p2) = makeDependency p1 [p2]


genNewDependency :: RepoGen PackagePair
genNewDependency = with2NewPackages makeDependency1


gen2packages :: RepoGen PackagePair
gen2packages = do
  p1 <- genNewPackage
  p2 <- genNewPackage
  pure (p1, p2)


gen3packages :: RepoGen (RI.PackageId, RI.PackageId, RI.PackageId)
gen3packages = do
  p1 <- genNewPackage
  (p2, p3) <- gen2packages
  pure (p1, p2, p3)


with2NewPackages :: (PackagePair -> RepoGen b) -> RepoGen PackagePair
with2NewPackages f = gen2packages >>= (\packs -> f packs >> pure packs)

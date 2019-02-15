{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestHelper
    ( module Test.Hspec
    , module Test.QuickCheck
    , gen2
    , repoWithoutPackage
    , repoWithoutPackageVersion
    , repoStateWithPackage
    , repoStateWithPackageVersion
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.List (intersperse)

import qualified Data.Depsolver.Repository.Internal as RI


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
repoWithoutPackage :: String -> Gen RI.Repository
repoWithoutPackage pname = deleteRepoPackageByName pname <$> arbitrary
    where deleteRepoPackageByName n = deleteRepoPackagesBy ((/=n) . RI.packageName)


-- | Generate a repository that is guaranteed not to contain
-- | the given package with the specified version.
repoWithoutPackageVersion :: String -> RI.Version -> Gen RI.Repository
repoWithoutPackageVersion pname version = deleteRepoPackageByNameVersion <$> arbitrary
    where deleteRepoPackageByNameVersion =
              RI.mkRepository . filter notPV . RI.repoPackages
          notPV p = RI.packageName p /= pname && RI.packageVersion p /= version


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


deriving instance Arbitrary RI.RepoState


-- | Generate a repository state that is guaranteed to have
-- | an entry for the given package.
repoStateWithPackage :: String -> Gen RI.RepoState
repoStateWithPackage pname = do
  version <- arbitrary
  repoStateWithPackageVersion pname version


-- | Generate a repository state that is guaranteed to have
-- | an entry for the given package.
repoStateWithPackageVersion :: String -> RI.Version -> Gen RI.RepoState
repoStateWithPackageVersion pname version = do
  fmap (addRepoStatePackage (pname, version)) arbitrary
    where addRepoStatePackage pv =
              RI.mkRepoState . (pv:) . RI.repoStatePackageVersions


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

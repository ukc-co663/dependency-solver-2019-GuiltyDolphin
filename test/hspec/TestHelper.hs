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

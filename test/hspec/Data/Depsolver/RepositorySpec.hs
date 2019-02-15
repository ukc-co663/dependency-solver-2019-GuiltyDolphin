module Data.Depsolver.RepositorySpec (spec) where

import TestHelper

import Data.Depsolver.Repository
    ( repoPackages
    , emptyRepository
    , emptyRepoState
    , validState
    , mkRepository
    , mkRepoState
    , mkVersion
    , mkPackage
    , toVersionList
    )

spec :: Spec
spec = do
  describe "toVersionList" $ do
         it "is inverse of mkVersion" $
            property $ \vs -> toVersionList (mkVersion vs) == vs
  describe "repoPackages" $ do
         it "empty repository has no packages" $
            repoPackages emptyRepository `shouldBe` []
  describe "validState" $ do
         it "the empty state is valid for any repository" $
            property (\repo -> validState repo emptyRepoState)
         it "the empty repository is not valid with any non-empty state" $
            property (\state -> state /= emptyRepoState ==> not (validState emptyRepository state))
         it "a state is not valid if it contains a package name that is not in the repository" $
            property (\p -> forAll (gen2 (repoWithoutPackage p, repoStateWithPackage p))
                      (\(repo, repoState) -> not (validState repo repoState)))
         it "simple case: package with same name but different version in state" $
            let repo = mkRepository [mkPackage "A" (mkVersion ["1"]) [] []]
                repoState = mkRepoState [("A", mkVersion ["2"])]
            in validState repo repoState `shouldBe` False
         it "a state is not valid if it contains a package name and version that is not in the repository" $
            property (\(p, v) -> forAll (gen2 (repoWithoutPackageVersion p v, repoStateWithPackageVersion p v))
                      (\(repo, repoState) -> not (validState repo repoState)))

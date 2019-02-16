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
    , mkPackageName
    , mkPackageVersion
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
  describe "repoExamples" $ do
         it "example valid states are valid" $
            mapM_ (\er -> do
                   let er' = getExampleRepo er
                   mapM_ (\es -> (er', es) `shouldSatisfy` uncurry validState) (getExampleValidStates er))
                          repoExamples
  describe "validState" $ do
         it "the empty state is valid for any repository" $
            property (\repo -> validState repo emptyRepoState)
         it "the empty repository is not valid with any non-empty state" $
            property (\state -> state /= emptyRepoState ==> not (validState emptyRepository state))
         it "a state is not valid if it contains a package name that is not in the repository" $
            property (\p -> forAll (gen2 (repoWithoutPackage p, repoStateWithPackage p))
                      (\(repo, repoState) -> not (validState repo repoState)))
         it "simple case: package with same name but different version in state" $
            let repo = mkRepository [mkPackage (mkPackageName "A") (mkVersion ["1"]) [] []]
                repoState = mkRepoState [mkPackageVersion (mkPackageName "A") (mkVersion ["2"])]
            in (repo, repoState) `shouldNotSatisfy` uncurry validState
         it "a state is not valid if it contains a package name and version that is not in the repository" $
            property (\p -> forAll (gen2 (repoWithoutPackageVersion p, repoStateWithPackageVersion p))
                      (\(repo, repoState) -> not (validState repo repoState)))
         it "a state is not valid if it contains packages with unmet dependencies" $
            property (\(p1, p2) -> forAll (gen2 (repoWithDependency p1 p2, repoStateWithPackageVersion p1))
                      (\(repo, repoState) -> not (validState repo repoState)))

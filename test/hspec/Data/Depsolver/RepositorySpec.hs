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
  describe "fileCases" $ do
         describe "example valid states are valid" $
                  runIO fileCaseExamples >>=
                        mapM_ (\(dir, (repo, repoState))
                                   -> it dir (validState repo repoState `shouldBe` True))
  describe "validState" $ do
         it "the empty state is valid for any repository" $
            property (\repo -> validState repo emptyRepoState)
         it "the empty repository is not valid with any non-empty state" $
            property (\state -> state /= emptyRepoState ==> not (validState emptyRepository state))
         it "a state is not valid if it contains a package name that is not in the repository" $
            propStateInvalid1 withoutPackageName repoStateWithPackage
         it "simple case: package with same name but different version in state" $
            let repo = mkRepository [mkPackage (mkPackageName "A") (mkVersion ["1"]) [] []]
                repoState = mkRepoState [mkPackageVersion (mkPackageName "A") (mkVersion ["2"])]
            in (repo, repoState) `shouldNotSatisfy` uncurry validState
         it "a state is not valid if it contains a package name and version that is not in the repository" $
            propStateInvalid1 withoutPackage repoStateWithPackageVersions1
         context "all dependencies met" $ do
                 it "A>>[], [A]" $
                    propStateValid1 newPackage repoStateWithPackageVersions1
                 it "A>>B, [A, B]" $
                    propStateValid2 makeDependency1 repoStateWithPackageVersions2
                 it "A>>B, B>>C, [A, B, C]" $
                    propStateValid3 makeTransDep repoStateWithPackageVersions3
                 it "A>>B, B>>A, [A, B]" $
                    propStateValid2 (\p1 p2 -> do
                                       makeDependency1 p1 p2
                                       makeDependency1 p2 p1) repoStateWithPackageVersions2
         context "unmet dependencies" $ do
                 it "A>>B, [A]" $
                    propStateInvalid2 makeDependency1 (\p1 _ -> repoStateWithPackageVersions1 p1)
                 it "A>>B, B>>C, [A, B]" $
                    propStateInvalid3 makeTransDep (\p1 p2 _ -> repoStateWithPackageVersions2 p1 p2)
                 it "A>>B, B>>C, [A, C]" $
                    propStateInvalid3 makeTransDep (\p1 _ p3 -> repoStateWithPackageVersions2 p1 p3)
         context "with conflicts" $ do
                 it "A~B=x, [A, B=x]" $
                    propStateInvalid2 makeConflict repoStateWithPackageVersions2
                 it "A~B, [A, B]" $
                    propStateInvalid2 makeWildConflict repoStateWithPackageVersions2
                 it "A>>[B,C] B~C [anything with A]" $
                    propStateInvalid3' (\p1 p2 p3 -> do
                                         makeDependency p1 [p2, p3]
                                         makeConflict p2 p3)
                                      (\r p1 _ _ -> repoStateContaining r [p1])
                 it "A~A, [any state with A]" $
                    propStateInvalid1' (\p -> makeConflict p p)
                                      (\r p -> repoStateContaining r [p])
      where checkRepoWithState p rg stateGen =
                forAll (gen2 (execRepoGen rg, stateGen)) p
            checkRepoWithState' p rg stateGen =
                forAll (do
                         repo <- execRepoGen rg
                         state <- stateGen repo
                         pure (repo, state)) p

            checkStateValid   = checkRepoWithState (uncurry validState)
            propStateValid1 rg stateGen = property $ \p1 -> checkStateValid (rg p1) (stateGen p1)
            propStateValid2 rg stateGen = property $ \(p1, p2) -> checkStateValid (rg p1 p2) (stateGen p1 p2)
            propStateValid3 rg stateGen = property $ \(p1, p2, p3) -> checkStateValid (rg p1 p2 p3) (stateGen p1 p2 p3)

            checkStateInvalid = checkRepoWithState (not . uncurry validState)
            propStateInvalid1 rg stateGen = property $ \p1 -> checkStateInvalid (rg p1) (stateGen p1)
            propStateInvalid2 rg stateGen = property $ \(p1, p2) -> checkStateInvalid (rg p1 p2) (stateGen p1 p2)
            propStateInvalid3 rg stateGen = property $ \(p1, p2, p3) -> checkStateInvalid (rg p1 p2 p3) (stateGen p1 p2 p3)

            checkStateInvalid' = checkRepoWithState' (not . uncurry validState)
            propStateInvalid1' rg stateGen =
                property $ \p1 -> checkStateInvalid' (rg p1) (\r -> stateGen r p1)
            propStateInvalid3' rg stateGen =
                property $ \(p1, p2, p3) -> checkStateInvalid' (rg p1 p2 p3) (\r -> stateGen r p1 p2 p3)

            repoStateWithPackageVersions1 p1 = pure $ repoStateWithPackageVersions [p1]
            repoStateWithPackageVersions2 p1 p2 = pure $ repoStateWithPackageVersions [p1, p2]
            repoStateWithPackageVersions3 p1 p2 p3 = pure $ repoStateWithPackageVersions [p1, p2, p3]

            makeDependency1 p1 p2 = makeDependency p1 [p2]
            makeTransDep p1 p2 p3 = makeDependency1 p1 p2 >> makeDependency1 p2 p3

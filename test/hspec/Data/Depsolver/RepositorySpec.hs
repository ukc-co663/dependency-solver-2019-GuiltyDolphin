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
            propStateInvalid withoutPackageName repoStateWithPackage
         it "simple case: package with same name but different version in state" $
            let repo = mkRepository [mkPackage (mkPackageName "A") (mkVersion ["1"]) [] []]
                repoState = mkRepoState [mkPackageVersion (mkPackageName "A") (mkVersion ["2"])]
            in (repo, repoState) `shouldNotSatisfy` uncurry validState
         it "a state is not valid if it contains a package name and version that is not in the repository" $
            propStateInvalid withoutPackage repoStateWithPackageVersions1
         context "all dependencies met" $ do
                 it "A>>[], [A]" $
                    propStateValid newPackage repoStateWithPackageVersions1
                 it "A>>B, [A, B]" $
                    propStateValid makeDependency1 repoStateWithPackageVersions2
                 it "A>>B, B>>C, [A, B, C]" $
                    propStateValid makeTransDep repoStateWithPackageVersions3
                 it "A>>B, B>>A, [A, B]" $
                    propStateValid (\(p1, p2) -> do
                                       makeDependency1 (p1, p2)
                                       makeDependency1 (p2, p1)) repoStateWithPackageVersions2
         context "unmet dependencies" $ do
                 it "A>>B, [A]" $
                    propStateInvalid makeDependency1 (\(p1,_) -> repoStateWithPackageVersions1 p1)
                 it "A>>B, B>>C, [A, B]" $
                    propStateInvalid makeTransDep (\(p1,p2,_) -> repoStateWithPackageVersions2 (p1, p2))
                 it "A>>B, B>>C, [A, C]" $
                    propStateInvalid makeTransDep (\(p1,_,p3) -> repoStateWithPackageVersions2 (p1, p3))
         context "with conflicts" $ do
                 it "A~B=x, [A, B=x]" $
                    propStateInvalid makeConflict1 repoStateWithPackageVersions2
                 it "A~B, [A, B]" $
                    propStateInvalid makeWildConflict1 repoStateWithPackageVersions2
                 it "A>>[B,C] B~C [anything with A]" $
                    propStateInvalid' (\(p1,p2,p3) -> do
                                         makeDependency p1 [p2, p3]
                                         makeConflict p2 p3)
                                      (\r (p1,_,_) -> repoStateContaining r [p1])
                 it "A~A, [any state with A]" $
                    propStateInvalid' (\p -> makeConflict p p)
                                         (\r p -> repoStateContaining r [p])
      where checkRepoWithState p rg stateGen =
                forAll (gen2 (execRepoGen rg, stateGen)) p
            checkRepoWithState' p rg stateGen =
                forAll (do
                         repo <- execRepoGen rg
                         state <- stateGen repo
                         pure (repo, state)) p

            checkStateValid   = checkRepoWithState (uncurry validState)
            propStateValid rg stateGen = property $ \p -> checkStateValid (rg p) (stateGen p)

            checkStateInvalid = checkRepoWithState (not . uncurry validState)
            propStateInvalid rg stateGen = property $ \p -> checkStateInvalid (rg p) (stateGen p)

            checkStateInvalid' = checkRepoWithState' (not . uncurry validState)
            propStateInvalid' rg stateGen =
                property $ \p -> checkStateInvalid' (rg p) (\r -> stateGen r p)

            repoStateWithPackageVersions1 p1 = pure $ repoStateWithPackageVersions [p1]
            repoStateWithPackageVersions2 (p1, p2) = pure $ repoStateWithPackageVersions [p1, p2]
            repoStateWithPackageVersions3 (p1, p2, p3) = pure $ repoStateWithPackageVersions [p1, p2, p3]

            makeConflict1 (p1, p2) = makeConflict p1 p2
            makeWildConflict1 (p1, p2) = makeWildConflict p1 p2

            makeDependency1 (p1, p2) = makeDependency p1 [p2]
            makeTransDep (p1, p2, p3) = makeDependency1 (p1, p2) >> makeDependency1 (p2, p3)

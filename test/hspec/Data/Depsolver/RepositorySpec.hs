module Data.Depsolver.RepositorySpec (spec) where

import TestHelper

import Data.List (tails)
import Data.Maybe (fromJust)

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

import qualified Data.Depsolver.Parse as P
import qualified Data.Depsolver.Repository as R


-- | Given a list of version strings, ensure that every
-- | version earlier in the list version satisfies the
-- | comparator when compared with any version later in the list.
versionTest :: String -> (R.Version -> R.Version -> Bool) -> [String] -> SpecWith ()
versionTest s cmp vstrs =
    describe (unwords ["V1", s, "V2"]) $
      let orderedVersions = fmap (fromJust . P.parseVersion) vstrs
          versionTails = fmap (\(vg : vs) -> (vg, vs)) (init . init . tails $ orderedVersions)
      in mapM_ (\(v1, vs) -> mapM_ (vtest v1) vs) versionTails
    where vtest v1 v2 = it (unwords [show v1, s, show v2]) $ (v1, v2) `shouldSatisfy` uncurry cmp


spec :: Spec
spec = do
  describe "version comparison" $ do
         versionTest ">" (>) ["10", "2", "1.7.3", "1.2", "1", "0.1", "0"]
         versionTest "=" (==) ["10.2", "10.02", "10.2.0", "10.02.0", "10.02.00", "010.02.00"]
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
                forAll (gen2 (pure $ execRepoGen rg, stateGen)) p

            checkRepoWithState' p rg stateGen =
                forAll (do
                         let repo = execRepoGen rg
                         state <- stateGen repo
                         pure (repo, state)) p

            propChecker pf rg stateGen =
                property $ \p -> (checkRepoWithState pf) (rg p) (stateGen p)

            propChecker' pf rg stateGen =
                property $ \p -> (checkRepoWithState' pf) (rg p) (\r -> stateGen r p)

            validState'   = uncurry validState
            notValidState = not . validState'

            propStateValid, propStateInvalid :: (Arbitrary a, Show a) => (a -> RepoGen b) -> (a -> Gen R.RepoState) -> Property
            propStateValid    = propChecker  validState'
            propStateInvalid  = propChecker  notValidState
            propStateValid', propStateInvalid' :: (Arbitrary a, Show a) => (a -> RepoGen b) -> (R.Repository -> a -> Gen R.RepoState) -> Property
            propStateValid'   = propChecker' validState'
            propStateInvalid' = propChecker' notValidState

            repoStateWithPackageVersions1  p1          = pure $ repoStateWithPackageVersions [p1]
            repoStateWithPackageVersions2 (p1, p2)     = pure $ repoStateWithPackageVersions [p1, p2]
            repoStateWithPackageVersions3 (p1, p2, p3) = pure $ repoStateWithPackageVersions [p1, p2, p3]

            makeConflict1 (p1, p2) = makeConflict p1 p2
            makeWildConflict1 (p1, p2) = makeWildConflict p1 p2

            makeDependency1 (p1, p2) = makeDependency p1 [p2]
            makeTransDep (p1, p2, p3) = makeDependency1 (p1, p2) >> makeDependency1 (p2, p3)

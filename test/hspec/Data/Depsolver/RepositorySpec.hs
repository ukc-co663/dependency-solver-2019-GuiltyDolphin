module Data.Depsolver.RepositorySpec (spec) where

import qualified QuickCheck.GenT as G

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


{-
  Some notes on test notation:

  * A~B means that A conflicts with B
  * A>>B means that A depends upon B
  * A>>[B,C], A>>[D] means that A depends on having either B and C, or on D
  * A~[B, C] means that A conflicts with both B and C
  * A~B=y means that A conflicts with B of a specific version (y)

  Thus the following tests read:

  * "A~B=x, y!x, [A, B=y]"
    * A conflicts with B of version x
    * y is not equal to x
    * our final state has A, and B of version y
  * "A~B, [!(A^B)]"
    * A depends on any version of B
    * our final state does not have both A and B
-}


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
            property (\repoState -> repoState /= emptyRepoState ==> not (validState emptyRepository repoState))
         it "simple case: package with same name but different version in state" $
            let repo = mkRepository [mkPackage (mkPackageName "A") (mkVersion ["1"]) [] []]
                repoState = mkRepoState [mkPackageVersion (mkPackageName "A") (mkVersion ["2"])]
            in (repo, repoState) `shouldNotSatisfy` uncurry validState
         it "a state is not valid if it contains a package that is not in the repository" $
            propStateInvalid withoutPackage repoStateWithPackages1
         context "all dependencies met" $ do
                 it "A>>[], [A]" $
                    propStateValid genNewPackage repoStateWithPackages1
                 it "A>>B, [A, B]" $
                    propStateValid genNewDependency repoStateWithPackages2
                 it "A>>B, B>>C, [A, B, C]" $
                    propStateValid makeTransDep repoStateWithPackages3
                 it "A>>B, B>>A, [A, B]" $
                    propStateValid (with2NewPackages $ \(p1, p2) -> makeDependency1 (p2, p1))
                                   repoStateWithPackages2
                 it "A>>[[B], [C]], [A, B]" $
                    propStateValid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2], [p3]]
                                     pure (p1, p2)) repoStateWithPackages2
                 it "A>>[[B], [C]], [A, C]" $
                    propStateValid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2], [p3]]
                                     pure (p1, p3)) repoStateWithPackages2
         context "unmet dependencies" $ do
                 it "A>>B, [A]" $
                    propStateInvalid genNewDependency (repoStateWithPackages1 . fst)
                 it "A>>B, B>>C, [A, B]" $
                    propStateInvalid makeTransDep (\(p1,p2,_) -> repoStateWithPackages2 (p1, p2))
                 it "A>>B, B>>C, [A, C]" $
                    propStateInvalid makeTransDep (\(p1,_,p3) -> repoStateWithPackages2 (p1, p3))
                 it "A>>[[B], [C]], [A]" $
                    propStateInvalid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2], [p3]]
                                     pure p1) repoStateWithPackages1
         context "without conflicts" $ do
                 it "A~B=x, y!=x, [A, B=y]" $
                    conflictTest propStateValid noReq R.VEQ (/=)
                 it "A~B>x, y<x, [A, B=y]" $
                    conflictTest propStateValid noReq R.VGT (<)
                 it "A~B, [!(A^B)]" $
                    propStateValid genNewConflict
                            (\(p1, p2) -> arbyState `G.suchThat` (\s -> not (all (`stateElem`s) [p1,p2])))
         context "with conflicts" $ do
                 it "A~B=x, [A, B=x]" $
                    propStateInvalid genNewConflict repoStateWithPackages2
                 it "A~B, [A, B]" $
                    propStateInvalid genNewWildConflict repoStateWithPackages2
                 it "A>>B, B~A [anything with A]" $
                    propStateInvalid (do
                                     (p1, p2) <- gen2packages
                                     makeDependency p1 [p2]
                                     makeConflict p2 p1
                                     pure p1)
                                     repoStateWithPackages1
                 it "A>>[B,C] B~C [anything with A]" $
                    propStateInvalid (do
                                      (p1, p2, p3) <- gen3packages
                                      makeDependency p1 [p2, p3]
                                      makeConflict p2 p3
                                      pure p1)
                                      repoStateWithPackages1
                 it "A~A, [any state with A]" $
                    propStateInvalid (do
                                      p <- genNewPackage
                                      makeConflict p p
                                      pure p)
                                      repoStateWithPackages1
                 it "A~B>x, y!=0, y>x, [A, B=y]" $
                    conflictTest propStateInvalid (/=zeroVersion) R.VGT (>)
      where -- | Test a generated (repo, state) pair.
            checkRepoWithState :: (Testable b) => ((R.Repository, R.RepoState) -> b)
                               -> RepoGen a -> (a -> RepoGen R.RepoState) -> Property
            checkRepoWithState p rg rsg = forAll (runRepoGen (rg >>= rsg)) p

            validState'   = uncurry validState
            notValidState = not . validState'

            propStateValid, propStateInvalid :: RepoGen a -> (a -> RepoGen R.RepoState) -> Property
            propStateValid    = checkRepoWithState validState'
            propStateInvalid  = checkRepoWithState notValidState

            repoStateWithPackages1  p1          = pure $ repoStateWithPackages [p1]
            repoStateWithPackages2 (p1, p2)     = pure $ repoStateWithPackages [p1, p2]
            repoStateWithPackages3 (p1, p2, p3) = pure $ repoStateWithPackages [p1, p2, p3]

            makeDependency1 (p1, p2) = makeDependency p1 [p2]
            with2NewPackages f = gen2packages >>= (\packs -> f packs >> pure packs)
            genNewWildConflict = with2NewPackages (uncurry makeWildConflict)
            genNewDependency   = with2NewPackages makeDependency1
            genNewConflict     = with2NewPackages (uncurry makeConflict)

            makeTransDep = do
                (p1, p2, p3) <- gen3packages
                makeDependency1 (p1, p2)
                makeDependency1 (p2, p3)
                pure (p1, p2, p3)

            -- | @conflictTest f yreq opCmp op@ generates a test
            -- | of the form "A~B#x, y@x, [A, B=y]"
            -- | where '#' is 'opCmp', and '@' is 'op'.
            -- |
            -- | @yreq@ is used to constrain the values @y@ can take.
            -- |
            -- | For example, @conflictTest _ (/=zeroVersion) VEQ (<) generates
            -- | "A~B=x, y!=0, y<x, [A, B=y]".
            conflictTest f yreq opCmp op = f conflictGen repoStateWithPackages2
                where conflictGen = do
                            p1 <- genNewPackage
                            y <- versionSat yreq
                            let p1name = packageIdName p1
                            p2y <- genNewPackageWithVersion y `G.suchThat` ((/=p1name) . packageIdName)
                            x <- versionSat (op . packageIdVersion $ p2y)
                            makeConflictOp opCmp p1 p2y x
                            pure (p1, p2y)
                      packageIdVersion = snd . R.getPackageVersion
                      packageIdName = fst . R.getPackageVersion
            zeroVersion = mkVersion ["0"]
            noReq = const True
            -- | Generate a package that is guaranteed not to be in the repository.
            withoutPackage = genNewPackage >>= (\s -> deletePackage s >> pure s)
            gen2packages = do
                            p1 <- genNewPackage
                            p2 <- genNewPackage
                            pure (p1, p2)
            gen3packages = do
                            p1 <- genNewPackage
                            (p2, p3) <- gen2packages
                            pure (p1, p2, p3)

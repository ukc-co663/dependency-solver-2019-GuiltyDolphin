module Data.Depsolver.RepoStateSpec (spec) where

import qualified QuickCheck.GenT as G

import TestHelper

import qualified Data.Depsolver.Repository.Internal as RI
import qualified Data.Depsolver.RepoState.Internal as RI

import Data.Depsolver.RepoState (RepoState, validState)


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
  describe "repoExamples" $ do
         it "example valid states are valid" $
            mapM_ (\er -> do
                   let er' = getExampleRepo er
                   mapM_ (\es -> (er', es) `shouldSatisfy` validState') (getExampleValidStates er))
                          repoExamples
  describe "fileCases" $ do
         describe "example valid states are valid" $
                  runIO fileCaseExamples >>=
                        mapM_ (\(dir, (repo, repoState, _))
                                   -> it dir (validState' (repo, repoState) `shouldBe` True))
  describe "validState" $ do
         it "the empty state is valid for any repository" $
            property (\repo -> validState' (repo, RI.emptyRepoState))
         it "the empty repository is not valid with any non-empty state" $
            property (\repoState -> repoState /= RI.emptyRepoState ==> notValidState (RI.emptyRepository, repoState))
         it "simple case: package with same name but different version in state" $
            let repo = RI.mkRepository [RI.mkPackage (RI.mkPackageName "A") (RI.mkVersion ["1"]) (RI.mkSize 1) [] []]
                repoState = RI.mkRepoState [RI.mkPackageId (RI.mkPackageName "A") (RI.mkVersion ["2"])]
            in (repo, repoState) `shouldNotSatisfy` validState'
         it "a state is not valid if it contains a package that is not in the repository" $
            propStateInvalid withoutPackage repoStateWithPackages1
         context "all dependencies met" $ do
                 it "A>>[], [A]" $
                    propStateValid genNewPackage repoStateWithPackages1
                 it "A>>B, [A, B]" $
                    propStateValid genNewDependency repoStateWithPackages2
                 it "A>>B, B>>C, [A, B, C]" $
                    propStateValid makeTransDep repoStateWithPackages3
                 it "A>>[[B], [C]], [A, B, C]" $
                    propStateValid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2], [p3]]
                                     pure (p1, p2, p3)) repoStateWithPackages3
                 it "A>>[[B], [B, C]], [A, B]" $
                    propStateValid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2], [p2, p3]]
                                     pure (p1, p2)) repoStateWithPackages2
                 it "A>>[[B, C]], [A, B]" $
                    propStateValid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2, p3]]
                                     pure (p1, p2)) repoStateWithPackages2
                 it "B~A, C>>[[D,A]], D>>[[A,C],E], E>>C, [B,C,D,E]" $
                    propStateValid (do
                                     (a, b, c) <- gen3packages
                                     (d, e) <- gen2packages
                                     makeConflict b a
                                     makeDependencies c [[d, a]]
                                     makeDependencies d [[a, c], [e]]
                                     makeDependencies e [[c]]
                                     pure (b, c, d, e)) repoStateWithPackages4
                 context "with cyclic dependencies" $ do
                   -- we don't care about how the dependencies came to be satisfied, only that they presently are
                   it "A>>B, B>>A, [A, B]" $
                     propStateValid (with2NewPackages $ \(p1, p2) -> makeDependency1 (p2, p1))
                                    repoStateWithPackages2
                   -- for example, we would never be able to construct this through a series of commands,
                   -- but it still represents a valid state
                   it "A>>A, [A]" $
                     propStateValid (do
                       p1 <- genNewPackage
                       makeDependency p1 [p1]
                       pure p1)
                       repoStateWithPackages1
         context "unmet dependencies" $ do
                 it "A>>B, [A]" $
                    propStateInvalid genNewDependency (repoStateWithPackages1 . fst)
                 it "A>>B, B>>C, [A, B]" $
                    propStateInvalid makeTransDep (\(p1,p2,_) -> repoStateWithPackages2 (p1, p2))
                 it "A>>B, B>>C, [A, C]" $
                    propStateInvalid makeTransDep (\(p1,_,p3) -> repoStateWithPackages2 (p1, p3))
                 it "A>>[[B, C]], [A]" $
                    propStateInvalid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2, p3]]
                                     pure p1) repoStateWithPackages1
                 it "A>>[[B], [C]], [A, B]" $
                    propStateInvalid (do
                                     (p1, p2, p3) <- gen3packages
                                     makeDependencies p1 [[p2], [p3]]
                                     pure (p1, p2)) repoStateWithPackages2
         context "without conflicts" $ do
                 it "A~B=x, y!=x, [A, B=y]" $
                    conflictTest propStateValid noReq RI.VEQ (/=)
                 it "A~B>x, y<x, [A, B=y]" $
                    conflictTest propStateValid noReq RI.VGT (<)
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
                 it "A~A, [anything with A]" $
                    propStateInvalid (do
                                      p <- genNewPackage
                                      makeConflict p p
                                      pure p)
                                      repoStateWithPackages1
                 it "A~B>x, y!=0, y>x, [A, B=y]" $
                    conflictTest propStateInvalid (/=zeroVersion) RI.VGT (>)
      where -- | Test a generated (repo, state) pair.
            checkRepoWithState :: (Testable b) => ((RI.Repository, RepoState) -> b)
                               -> RepoGen a -> (a -> RepoGen RepoState) -> Property
            checkRepoWithState p rg rsg = forAll (runRepoGen (rg >>= rsg)) p

            validState'   = uncurry (validState . RI.compileRepository)
            notValidState = not . validState'

            propStateValid, propStateInvalid :: RepoGen a -> (a -> RepoGen RepoState) -> Property
            propStateValid    = checkRepoWithState validState'
            propStateInvalid  = checkRepoWithState notValidState

            repoStateWithPackages1  p1          = pure $ repoStateWithPackages [p1]
            repoStateWithPackages2 (p1, p2)     = pure $ repoStateWithPackages [p1, p2]
            repoStateWithPackages3 (p1, p2, p3) = pure $ repoStateWithPackages [p1, p2, p3]
            repoStateWithPackages4 (p1, p2, p3, p4) = pure $ repoStateWithPackages [p1, p2, p3, p4]

            genNewWildConflict = with2NewPackages (uncurry makeWildConflict)
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
                            let p1name = RI.packageIdName p1
                            p2y <- genNewPackageWithVersion y `G.suchThat` ((/=p1name) . RI.packageIdName)
                            x <- versionSat (op . RI.packageIdVersion $ p2y)
                            makeConflictOp opCmp p1 p2y x
                            pure (p1, p2y)
            zeroVersion = RI.mkVersion ["0"]
            noReq = const True
            -- | Generate a package that is guaranteed not to be in the repository.
            withoutPackage = genNewPackage >>= (\s -> deletePackage s >> pure s)

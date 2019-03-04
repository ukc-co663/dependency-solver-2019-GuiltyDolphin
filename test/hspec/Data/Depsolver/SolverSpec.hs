module Data.Depsolver.SolverSpec (spec) where


import TestHelper

import Data.Depsolver.Constraint
import Data.Depsolver.RepoState
import Data.Depsolver.Repository
import qualified Data.Depsolver.Constraint.Internal as RI
import qualified Data.Depsolver.Repository.Internal as RI
import qualified Data.Depsolver.Solver as RI

import Data.Depsolver.Solver (solve, satisfiesConstraints)


type SolverResult = (RepoState, [RI.Command])


{- TEST NOTATION

   A test:

   @
     (Repo, InitState, Constraints), Preconditions ==> (FinalState, Commands)
   @

   Means that for a repository, initial state, and set of constraints
   matching @Repo@, @InitState@, and @Constraints@ respectively,
   when the given @Preconditions@ are met, the solver should produce a final
   state and set of commands matching @FinalState@ and @Commands@ respectively.

   The syntax for @Repo@, @InitState@, and @FinalState@ follow the syntax
   specified in 'RepoStateSpec', but additional notation is used to indicate the
   size of a package (which is irrelevant for 'RepoStateSpec'):

   @
     A=x@n
   @

   Describes a package @A@ with version @x@ and size @n@.

   Thus the test:

   @
     ([A=x@n, A=y@m], [], [+A]), n<m ==> ([A=x], [+A=x])
   @

   Reads:

   - for a repository with two packages, both called @A@,
     with different versions, and one being smaller than the other
   - given an empty initial state
   - given a constraint that any version of @A@ is installed
   - the final state should exclusively contain the smaller
     package
   - the set of commands should be the command to install the
     smaller package
-}


spec :: Spec
spec = do
  describe "satisfiesConstraints" $ do
    it "every state satisfies the empty constraints" $
      property (\(repo, rstate) -> satisfiesConstraints rstate
                                   (RI.compileConstraintsToPackageConstraints repo emptyConstraints))
  describe "solve" $ do
    context "with satisfiable constraints" $ do
      it "(_, S, []) ==> (S, [])" $
        property (\repo -> forAll (arbitrary `suchThat` validState (RI.compileRepository repo)) $
                           \rstate -> solve repo emptyConstraints rstate === Just (rstate, []))
      it "(_, S, C) when C already satisfied ==> (S, [])" $
        property (\(repo, rstate) -> forAll (arbitrary `suchThat` (satisfiesConstraints rstate . RI.compileConstraintsToPackageConstraints repo))
                  $ \constrs -> solve repo constrs rstate === Just (rstate, []))
      it "([A], [], [+A]) ==> ([A], [+A])" $
        checkSolver (do
          p1 <- genNewPackage
          let rs = emptyRepoState
              rsFinal = repoStateWithPackages [p1]
              cstr = mkWildPositiveConstraints [p1]
              cmds = [RI.mkInstall p1]
          pure (rs, cstr, (rsFinal, cmds)))
      it "([A], [A], [-A]) ==> ([], [-A])" $
        checkSolver (do
          p1 <- genNewPackage
          let rs = repoStateWithPackages [p1]
              rsFinal = emptyRepoState
              cstr = mkWildNegativeConstraints [p1]
              cmds = [RI.mkUninstall p1]
          pure (rs, cstr, (rsFinal, cmds)))
      it "([A>>B], [], [+A]) ==> ([A, B], [+B, +A])" $
        checkSolver (do
          (p1, p2) <- genNewDependency
          let rs = emptyRepoState
              rsFinal = repoStateWithPackages [p1, p2]
              cstr = mkEqPositiveConstraints [p1]
              cmds = [RI.mkInstall p2, RI.mkInstall p1]
          pure (rs, cstr, (rsFinal, cmds)))
      it "([A=x@n, A=y@m], [], [+A]), n<m ==> ([A=x], [+A=x])" $
        checkSolver (do
          p1 <- genNewPackage
          p2 <- genLargerPackageSameName p1
          let rs = emptyRepoState
              rsFinal = repoStateWithPackages [p1]
              -- this should essentially be just 'A', as these
              -- packages should have the same name
              cstr = mkWildPositiveConstraints [p1, p2]
              cmds = [RI.mkInstall p1]
          pure (rs, cstr, (rsFinal, cmds)))
      context "with hotswapping" $ do
        it "([A, B>>[[C,A]], C>>[[A,B]]], [A, B], [+B, +C, -A]) ==> ([B,C], [+C,-A])" $
          checkSolver (do
            (a, b, c) <- gen3packages
            makeDependencies b [[c, a]]
            makeDependencies c [[a, b]]

            -- essentially, this tests that we don't care about
            -- how dependencies are satisfied, as long as packages
            -- in the state have all of their dependencies satisfied
            -- after each command. This allows for hotswapping: installing
            -- a package by first installing a particular dependency, but then
            -- later uninstalling that package, as a series of commands have led
            -- to the dependency being satisfied regardless
            let rsStart = repoStateWithPackages [a, b]
                rsFinal = repoStateWithPackages [b, c]
                cstr = mkEqPositiveConstraints  [b, c] <> mkEqNegativeConstraints [a]
                cmds = [ RI.mkInstall c
                       , RI.mkUninstall a ]
            pure (rsStart, cstr, (rsFinal, cmds)))
        it "([A, B>>[[C,A]], C>>[[A,B],D], D>>B], [], [-A,+B,+C]) ==> ([B,C,D], [+A,+B,+D,+C,-A])" $
          checkSolver (do
            -- the repository is constructed in such a way that
            -- a package would have to first be installed, then later
            -- uninstalled to satisfy all the constraints
            (a, b) <- gen2packages
            (c, d) <- gen2packages
            makeDependencies b [[c, a]]
            makeDependencies c [[a, b], [d]]
            makeDependencies d [[b]]

            let rsStart = emptyRepoState
                rsFinal = repoStateWithPackages [b, c, d]
                cstr = mkEqNegativeConstraints [a] <> mkEqPositiveConstraints [b, c]
                cmds = [ RI.mkInstall a
                       , RI.mkInstall b
                       , RI.mkInstall d
                       , RI.mkInstall c
                       , RI.mkUninstall a ]
            pure (rsStart, cstr, (rsFinal, cmds)))
    context "with unsatisfiable constraints" $ do
      it "([A~A], [], [+A]) ==> Nothing" $
        checkSolverInvalid (do
          p1 <- genNewPackage
          makeConflict p1 p1
          pure (emptyRepoState, mkEqPositiveConstraints [p1]))
      it "([A>>[[B,C]], B>>A, C>>[[A,B]]], [], [+A]) ==> Nothing" $
        checkSolverInvalid (do
          (p1, p2, p3) <- gen3packages
          makeDependencies p1 [[p2, p3]]
          makeDependencies p2 [[p1]]
          makeDependencies p3 [[p1, p2]]
          pure (emptyRepoState, mkEqPositiveConstraints [p1]))
      where -- | Check that the solver produces the expected result for the generated
            -- | inputs.
            checkSolver :: RepoGen (RepoState, Constraints, SolverResult) -> Property
            checkSolver rg = forAll (runRepoGen rg)
                             (\(repo, (initState, cstrs, (expectedState, expectedCmds))) ->
                              let res = solve repo cstrs initState
                              in res === Just (expectedState, expectedCmds))

            -- | Check that the solver determines the given inputs to be unsatisfiable.
            checkSolverInvalid :: RepoGen (RepoState, Constraints) -> Property
            checkSolverInvalid rg = forAll (runRepoGen rg)
                                    (\(repo, (initState, cstrs)) ->
                                     solve repo cstrs initState === Nothing)

            mkWildPositiveConstraints, mkWildNegativeConstraints :: [PackageId] -> Constraints
            mkWildPositiveConstraints = RI.mkConstraints . fmap mkPositiveWildConstraint
            mkPositiveWildConstraint =
                RI.mkPositiveConstraint . RI.mkWildcardDependency . RI.packageIdName
            mkWildNegativeConstraints = RI.mkConstraints . fmap mkNegativeWildConstraint
            mkNegativeWildConstraint =
                RI.mkNegativeConstraint . RI.mkWildcardDependency . RI.packageIdName
            mkEqPositiveConstraints = RI.mkConstraints . fmap mkPositiveEqConstraint
            mkPositiveEqConstraint pid =
                let (pn, pv) = RI.getPackageId pid
                in RI.mkPositiveConstraint $ RI.mkDependency pn RI.VEQ pv
            mkEqNegativeConstraints = RI.mkConstraints . fmap mkNegativeEqConstraint
            mkNegativeEqConstraint pid =
                let (pn, pv) = RI.getPackageId pid
                in RI.mkNegativeConstraint $ RI.mkDependency pn RI.VEQ pv

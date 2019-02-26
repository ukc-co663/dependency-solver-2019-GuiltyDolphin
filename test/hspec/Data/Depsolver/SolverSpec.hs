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


spec :: Spec
spec = do
  describe "satisfiesConstraints" $ do
    it "every state satisfies the empty constraints" $
      property (\(repo, rstate) -> satisfiesConstraints repo rstate emptyConstraints)
  describe "solve" $ do
    it "(_, S, []) ==> (S, [])" $
      property (\repo -> forAll (arbitrary `suchThat` validState repo) $
                         \rstate -> solve repo emptyConstraints rstate === Just (rstate, []))
    it "(_, S, C) when C already satisfied ==> (S, [])" $
      property (\(repo, rstate) -> forAll (arbitrary `suchThat` satisfiesConstraints repo rstate)
                $ \constrs -> solve repo constrs rstate === Just (rstate, []))
    it "a state resulting from solve is always a valid state (if the initial state is valid)" $
      property (\repo -> forAll (gen2 (arbitrary `suchThat` validState repo, arbyRepo repo)) $
                \(rstate, constrs) -> let res = solve repo constrs rstate
                                      in counterexample (show res) (maybe False (validState repo . fst) res))
    it "([A], [], [+A]) ==> ([A], [+A])" $
      checkSolver (do
        p1 <- genNewPackage
        let rs = emptyRepoState
            rsFinal = repoStateWithPackages [p1]
            cstr = mkWildConstraints [p1]
            cmds = [RI.mkInstall p1]
        pure (rs, cstr, (rsFinal, cmds)))
      where -- | Check that the solver produces the expected result for the generated
            -- | inputs.
            checkSolver :: RepoGen (RepoState, Constraints, SolverResult) -> Property
            checkSolver rg = forAll (runRepoGen rg)
                             (\(repo, (initState, cstrs, (expectedState, expectedCmds))) ->
                              let res = solve repo cstrs initState
                              in res === Just (expectedState, expectedCmds))

            mkWildConstraints :: [PackageId] -> Constraints
            mkWildConstraints = RI.mkConstraints . fmap mkPositiveWildConstraint
            mkPositiveWildConstraint =
                RI.mkPositiveConstraint . RI.mkWildcardDependency . RI.packageIdName

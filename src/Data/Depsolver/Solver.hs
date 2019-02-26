module Data.Depsolver.Solver
    ( solve
    , satisfiesConstraints

    -- ** Commands
    , Command
    , mkInstall
    , mkUninstall
    ) where


import Data.List ((\\), delete)
import Data.Maybe (fromMaybe, listToMaybe)

import Data.Depsolver.Constraint
import Data.Depsolver.RepoState
import Data.Depsolver.Repository


-- | A command to manipulate the repository state.
data Command =
    -- | Install the given package.
    Install PackageId |
    -- | Uninstall the given package.
    Uninstall PackageId
    deriving (Eq)


instance Show Command where
    show (Install p)   = '+' : show p
    show (Uninstall p) = '-' : show p


-- | Create a command to install the given package.
mkInstall :: PackageId -> Command
mkInstall = Install


-- | Create a command to uninstall the given package.
mkUninstall :: PackageId -> Command
mkUninstall = Uninstall


solveRec :: Repository -> Constraints -> [Command] -> [Command] -> RepoState -> Maybe (RepoState, [Command])
solveRec r cstrs [] cmdsAcc rs =
    if satisfiesConstraints r rs cstrs then pure (rs, cmdsAcc) else Nothing
solveRec r cstrs unconsumed cmdsAcc rs =
    if satisfiesConstraints r rs cstrs then pure (rs, cmdsAcc) else
        let nextSolns = maybe [] (fmap $ \(s,c) -> solveRec r cstrs (delete c unconsumed) (c:cmdsAcc) s) nextValidStates
        in fromMaybe Nothing (listToMaybe nextSolns)
    where nextValidStates = case validNextCommands of
                              [] -> Nothing
                              cmds -> pure $ fmap (\c -> (runCommand c rs, c)) cmds
          validNextCommands = filter (\c -> validState r (runCommand c rs)) sensibleNextCommands
          sensibleNextCommands = (unconsumed \\ installedCommands) \\ uninstalledCommands
          -- commands that would install already-installed packages
          installedCommands = fmap mkInstall $ spids
          -- commands that would uninstall already-uninstalled packages
          uninstalledCommands = fmap mkUninstall $ pids \\ spids
          pids = fmap packageId $ repoPackages r
          spids = repoStatePackageIds rs
          runCommand (Install v) rstate = installPackage v rstate
          runCommand (Uninstall v) rstate = uninstallPackage v rstate


-- | Given a repository, a set of constraints, and an initial state,
-- | find a state that satisfies the constraints, and the list of commands
-- | that takes the initial state to the new state.
-- |
-- | If the constraints cannot be satisfied with a valid state, then this
-- | returns Nothing.
solve :: Repository -> Constraints -> RepoState -> Maybe (RepoState, [Command])
solve r cstrs = fmap (\(s, cs) -> (s, reverse cs)) . solveRec r cstrs allCommands []
    where allCommands = fmap mkInstall pids <> fmap mkUninstall pids
          pids = fmap packageId $ repoPackages r


satisfiesConstraints :: Repository -> RepoState -> Constraints -> Bool
satisfiesConstraints r rs cs =
    let (deps, conflicts) = compileConstraintsToPackageConstraints cs
    in stateMeetsConstraints r rs deps conflicts

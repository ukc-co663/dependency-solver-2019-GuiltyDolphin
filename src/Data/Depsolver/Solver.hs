module Data.Depsolver.Solver
    ( solve
    , satisfiesConstraints

    -- ** Commands
    , Command
    , mkInstall
    , mkUninstall
    ) where


import qualified Data.Foldable as F
import Data.Function (on)
import Data.List ((\\), delete)
import Data.Maybe (catMaybes)

import qualified Text.JSON as TJ

import Data.Depsolver.Constraint
import Data.Depsolver.RepoState
import Data.Depsolver.Repository
import Data.Depsolver.Repository.Internal (wantString)


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


instance TJ.JSON Command where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON = maybe (TJ.Error "command string") parseCommand . wantString
        where parseCommand :: String -> TJ.Result Command
              parseCommand (x:s) = do
                command <- case x of
                             '+' -> pure Install
                             '-' -> pure Uninstall
                             _ -> TJ.Error "command"
                fmap command $ TJ.readJSON (TJ.JSString . TJ.toJSString $ s)
              parseCommand _ = TJ.Error "command"


-- | Create a command to install the given package.
mkInstall :: PackageId -> Command
mkInstall = Install


-- | Create a command to uninstall the given package.
mkUninstall :: PackageId -> Command
mkUninstall = Uninstall


-- | The cost of performing an operation
-- | (or a series of operations).
type Cost = Size


-- | The cost of uninstalling a package is 10^6.
uninstallCost :: Cost
uninstallCost = 1000000


solveRec :: Repository -> Constraints -> [Command] -> (Cost, [Command]) -> RepoState -> Maybe (Cost, RepoState, [Command])
solveRec r cstrs [] (currCost, cmdsAcc) rs =
    if satisfiesConstraints r rs cstrs then pure (currCost, rs, cmdsAcc) else Nothing
solveRec r cstrs unconsumed (currCost, cmdsAcc) rs =
    if satisfiesConstraints r rs cstrs then pure (currCost, rs, cmdsAcc) else
        let nextSolns = maybe []
                        (catMaybes . fmap (\(c, s, cmd) -> solveRec r cstrs (delete cmd unconsumed)
                                           (currCost + c, cmd:cmdsAcc) s)) nextValidStates
        in case nextSolns of
             [] -> Nothing
             solns -> Just $ cheapest solns
    where nextValidStates = case validNextCommands of
                              [] -> Nothing
                              cmds -> pure $ fmap (\c -> (commandCost c, runCommand c rs, c)) cmds
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
          cheapest = F.minimumBy (compare `on` fst3)
          fst3 (x,_,_) = x
          -- use 'maxBound' as a guard against invalid operations
          -- (so we would likely never choose them)
          commandCost (Install v) = maybe maxBound packageSize $ lookupPackage v r
          commandCost Uninstall{} = uninstallCost


-- | Given a repository, a set of constraints, and an initial state,
-- | find a state that satisfies the constraints, and the list of commands
-- | that takes the initial state to the new state.
-- |
-- | If the constraints cannot be satisfied with a valid state, then this
-- | returns Nothing.
solve :: Repository -> Constraints -> RepoState -> Maybe (RepoState, [Command])
solve r cstrs = fmap (\(_, s, cs) -> (s, reverse cs)) . solveRec r cstrs allCommands (0, [])
    where allCommands = fmap mkInstall pids <> fmap mkUninstall pids
          pids = fmap packageId $ repoPackages r


satisfiesConstraints :: Repository -> RepoState -> Constraints -> Bool
satisfiesConstraints r rs cs =
    let (deps, conflicts) = compileConstraintsToPackageConstraints cs
    in stateMeetsConstraints r rs deps conflicts

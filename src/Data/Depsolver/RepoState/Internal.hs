{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Depsolver.RepoState.Internal
    ( RepoState
    , fromRepoState
    , repoStatePackageIds
    , emptyRepoState
    , mkRepoState
    , mkRepoState'

    -- ** State validity
    , validState
    , stateMeetsConstraints

    -- ** Manipulating the state
    , installPackage
    , uninstallPackage
    ) where


import Data.Hashable (Hashable)
import qualified Data.HashSet as Set

import qualified Text.JSON as TJ

import Data.Depsolver.Repository


type Set = Set.HashSet


-- | The list of installed packages (and their versions).
newtype RepoState = RepoState {
      -- ^ Packages and their installed version.
      fromRepoState :: Set PackageId
    } deriving (Eq, Hashable)


instance TJ.JSON RepoState where
    readJSON = fmap mkRepoState . TJ.readJSON
    showJSON = TJ.showJSON . Set.toList . repoStatePackageIds


instance Show RepoState where
    show = TJ.encodeStrict


mkRepoState' :: Set PackageId -> RepoState
mkRepoState' = RepoState


-- | Create a new repository state with the given installed
-- | packages.
mkRepoState :: [PackageId] -> RepoState
mkRepoState = mkRepoState' . Set.fromList


-- | The state of a repository with no installed packages.
emptyRepoState :: RepoState
emptyRepoState = RepoState Set.empty


repoStatePackageIds :: RepoState -> Set PackageId
repoStatePackageIds = fromRepoState


-- | True if the state is valid given the constraints of
-- | the repository.
validState :: CompiledRepository -> RepoState -> Bool
validState r rs = all meetsPackageDependencies . repoStatePackageIds $ rs
    where meetsPackageDependencies pv =
              maybe False (\(_,deps,cflcts) -> stateMeetsConstraints rs deps cflcts) (lookupPackage' pv r)


-- | True if the state is valid for the given dependencies and conflicts.
stateMeetsConstraints :: RepoState -> CompiledDependencies -> CompiledConflicts -> Bool
stateMeetsConstraints rs dependencies conflicts =
    let pids = repoStatePackageIds rs
    in dependencyIsMet dependencies pids
       && not (conflictIsMet conflicts pids)


modifyAsSet :: (Set PackageId -> Set PackageId) -> RepoState -> RepoState
modifyAsSet f = mkRepoState' . f . fromRepoState


-- | Add the package to the state.
installPackage :: PackageId -> RepoState -> RepoState
installPackage = modifyAsSet . Set.insert


-- | Remove the package from the state.
uninstallPackage :: PackageId -> RepoState -> RepoState
uninstallPackage = modifyAsSet . Set.delete

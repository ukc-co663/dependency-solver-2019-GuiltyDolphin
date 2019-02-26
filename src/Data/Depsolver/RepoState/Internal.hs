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


import qualified Data.Set as Set

import qualified Text.JSON as TJ

import Data.Depsolver.Repository


-- | The list of installed packages (and their versions).
newtype RepoState = RepoState {
      -- ^ Packages and their installed version.
      fromRepoState :: Set.Set PackageId
    } deriving (Eq)


deriving instance TJ.JSON RepoState


instance Show RepoState where
    show = TJ.encodeStrict


mkRepoState' :: Set.Set PackageId -> RepoState
mkRepoState' = RepoState


-- | Create a new repository state with the given installed
-- | packages.
mkRepoState :: [PackageId] -> RepoState
mkRepoState = mkRepoState' . Set.fromList


-- | The state of a repository with no installed packages.
emptyRepoState :: RepoState
emptyRepoState = RepoState Set.empty


repoStatePackageIds :: RepoState -> [PackageId]
repoStatePackageIds = Set.toList . fromRepoState


-- | True if the state is valid given the constraints of
-- | the repository.
validState :: Repository -> RepoState -> Bool
validState r rs = all meetsPackageDependencies . repoStatePackageIds $ rs
    where meetsPackageDependencies pv =
              maybe False (\p -> stateMeetsConstraints r rs (packageDependencies p) (packageConflicts p)) (lookupPackage pv r)


-- | True if the state is valid for the given dependencies and conflicts.
stateMeetsConstraints :: Repository -> RepoState -> Dependencies -> Conflicts -> Bool
stateMeetsConstraints r rs dependencies conflicts =
    let pids = repoStatePackageIds rs
    in dependencyIsMet r dependencies pids
       && not (conflictIsMet r conflicts pids)


modifyAsSet :: (Set.Set PackageId -> Set.Set PackageId) -> RepoState -> RepoState
modifyAsSet f = mkRepoState' . f . fromRepoState


-- | Add the package to the state.
installPackage :: PackageId -> RepoState -> RepoState
installPackage = modifyAsSet . Set.insert


-- | Remove the package from the state.
uninstallPackage :: PackageId -> RepoState -> RepoState
uninstallPackage = modifyAsSet . Set.delete

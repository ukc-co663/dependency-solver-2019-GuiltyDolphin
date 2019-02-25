{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Depsolver.RepoState.Internal
    ( RepoState
    , repoStatePackageIds
    , emptyRepoState
    , mkRepoState
    , validState
    ) where


import qualified Text.JSON as TJ

import Data.Depsolver.Repository


-- | The list of installed packages (and their versions).
newtype RepoState = RepoState {
      -- ^ Packages and their installed version.
      repoStatePackageIds :: [PackageId]
    } deriving (Eq)


deriving instance TJ.JSON RepoState


instance Show RepoState where
    show = TJ.encodeStrict


-- | Create a new repository state with the given installed
-- | packages.
mkRepoState :: [PackageId] -> RepoState
mkRepoState = RepoState


-- | The state of a repository with no installed packages.
emptyRepoState :: RepoState
emptyRepoState = RepoState []


-- | True if the state is valid given the constraints of
-- | the repository.
validState :: Repository -> RepoState -> Bool
validState r rs = all meetsPackageDependencies . repoStatePackageIds $ rs
    where meetsPackageDependencies pv =
              maybe False (\p -> meetsADependency (packageDependencies p)
                                 && meetsNoConflicts (packageConflicts p)) (getPackage r pv)
          stateMeetsDependency [vm] =
            maybe False stateHasAnyOf $ getPackagesThatSatisfy r vm
          stateMeetsDependency _ = False
          stateMeetsConflict vm =
              maybe False stateHasAnyOf $ getPackagesThatSatisfy r vm
          stateHasAnyOf = any stateHasPackage
          stateHasPackage pv = (packageId pv) `elem` (repoStatePackageIds rs)
          meetsADependency [] = True
          meetsADependency deps = any stateMeetsDependency deps
          meetsNoConflicts = not . any stateMeetsConflict

module Data.Depsolver.RepoState
    ( RepoState
    , emptyRepoState
    , repoStatePackageIds

    -- ** State validity
    , validState
    , stateMeetsConstraints

    -- ** Manipulating the state
    , installPackage
    , uninstallPackage
    ) where


import Data.Depsolver.RepoState.Internal

module Data.Depsolver.Repository
    (
    -- ** Repositories
      Repository
    , mkRepository
    , emptyRepository
    , repoPackages

    -- ** Repository States
    , RepoState
    , mkRepoState
    , emptyRepoState
    , validState

    -- ** Packages
    , PackageDesc
    , mkPackage
    , packageName
    , packageVersion
    , packageDependencies
    , packageConflicts

    -- ** Dependencies
    , VersionMatch
    , VersionCmp(..)
    , mkDependency

    -- ** Versions
    , Version
    , mkVersion
    , toVersionList
    ) where


import Data.Depsolver.Repository.Internal

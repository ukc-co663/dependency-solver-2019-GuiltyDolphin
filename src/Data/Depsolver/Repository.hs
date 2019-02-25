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
    , PackageName
    , mkPackageName

    -- ** Dependencies
    , VersionMatch
    , VersionCmp(..)
    , mkDependency
    , mkWildcardDependency

    -- ** Installed Packages
    , PackageId
    , getPackageId
    , mkPackageId

    -- ** Versions
    , Version
    , mkVersion
    , toVersionList
    ) where


import Data.Depsolver.Repository.Internal

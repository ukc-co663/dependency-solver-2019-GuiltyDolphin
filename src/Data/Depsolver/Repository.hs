module Data.Depsolver.Repository
    (
    -- ** Repositories
      Repository
    , mkRepository
    , emptyRepository
    , repoPackages
    , getPackage
    , getPackagesThatSatisfy

    -- ** Packages
    , PackageDesc
    , mkPackage
    , packageName
    , packageVersion
    , packageDependencies
    , packageConflicts
    , packageId
    , PackageName
    , mkPackageName

    -- ** Dependencies
    , Dependencies
    , mkDependencies
    , dependencyIsMet
    , Conflicts
    , mkConflicts
    , conflictIsMet
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

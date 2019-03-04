module Data.Depsolver.Repository
    (
    -- ** Repositories
      Repository
    , mkRepository
    , emptyRepository
    , repoPackages
    , repoPackageIds
    , lookupPackage
    , getPackagesThatSatisfy

    -- ** Packages
    , PackageDesc
    , mkPackage
    , packageName
    , packageVersion
    , packageSize
    , packageDependencies
    , packageConflicts
    , packageId
    , PackageName
    , mkPackageName

    -- *** Package size
    , Size

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

    -- ** Compiling
    , compileConflicts
    , compileDependencies
    , compileRepository
    ) where


import Data.Depsolver.Repository.Internal

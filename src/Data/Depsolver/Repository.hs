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


-- | Repository containing information about available
-- | packages.
newtype Repository = Repository {
      -- ^ Available packages in the repository.
      repoPackages :: [PackageDesc]
    } deriving (Eq, Show)


-- | Create a new repository with the given package descriptions.
mkRepository :: [PackageDesc] -> Repository
mkRepository = Repository


-- | Repository with no packages.
emptyRepository :: Repository
emptyRepository = Repository { repoPackages = [] }


-- | The list of installed packages (and their versions).
newtype RepoState = RepoState [(String, Version)]
    deriving (Eq, Show)


-- | Create a new repository state with the given installed
-- | packages.
mkRepoState :: [(String, Version)] -> RepoState
mkRepoState = RepoState


-- | The state of a repository with no installed packages.
emptyRepoState :: RepoState
emptyRepoState = RepoState []


newtype Version = Version [String]
    deriving (Eq, Show)


-- | Make a new version with the given version specifiers.
-- |
-- | The left-most specifier should be the most major version
-- | identifier, e.g., 1.0.0 could be represented as
-- | @mkVersion ["1", "0", "0"]@.
mkVersion :: [String] -> Version
mkVersion = Version


-- | Return each component of a version as a list.
-- |
-- | @toVersionList (mkVersion ["1", "0"]) == ["1", "0"]@
toVersionList :: Version -> [String]
toVersionList (Version vs) = vs


data PackageDesc = PackageDesc {
      -- ^ Name of the package.
      packageName :: String
      -- ^ Version of the package.
    , packageVersion :: Version
      -- ^ Dependencies of the package
      -- ^ (as a conjunction of disjunctions).
    , packageDependencies :: [[VersionMatch]]
      -- ^ Packages that conflict with the package.
    , packageConflicts :: [VersionMatch]
    } deriving (Eq, Show)


mkPackage :: String -> Version -> [[VersionMatch]] -> [VersionMatch] -> PackageDesc
mkPackage name version deps conflicts =
    PackageDesc { packageName = name
                , packageVersion = version
                , packageDependencies = deps
                , packageConflicts = conflicts
                }


data VersionCmp = VLTE | VLT | VEQ | VGT | VGTE
                  deriving (Eq, Show)

data VersionMatch = VersionMatch String VersionCmp Version
                    deriving (Eq, Show)

mkDependency :: String -> VersionCmp -> Version -> VersionMatch
mkDependency = VersionMatch

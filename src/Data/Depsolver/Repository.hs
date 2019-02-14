module Data.Depsolver.Repository
    (
    -- ** Repositories
      Repository
    , mkRepository
    , emptyRepository
    , repoPackages

    -- ** Packages
    , PackageDesc
    , mkPackage
    , packageName
    , packageVersion

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
    } deriving (Eq, Show)


mkPackage :: String -> Version -> PackageDesc
mkPackage name version =
    PackageDesc { packageName = name
                , packageVersion = version
                }

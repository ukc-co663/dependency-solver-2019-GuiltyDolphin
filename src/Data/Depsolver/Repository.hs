module Data.Depsolver.Repository
    (
    -- ** Repositories
      Repository
    , emptyRepository

    -- ** Versions
    , Version
    , mkVersion
    , toVersionList
    ) where

-- | Repository containing information about available
-- | packages.
newtype Repository = Repository ()
    deriving (Eq, Show)

-- | Repository with no packages.
emptyRepository :: Repository
emptyRepository = Repository ()


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

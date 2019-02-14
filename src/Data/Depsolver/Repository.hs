module Data.Depsolver.Repository
    ( Repository
    , emptyRepository
    ) where

-- | Repository containing information about available
-- | packages.
newtype Repository = Repository ()
    deriving (Eq, Show)

-- | Repository with no packages.
emptyRepository :: Repository
emptyRepository = Repository ()

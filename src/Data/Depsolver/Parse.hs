module Data.Depsolver.Parse
    ( parseRepo
    ) where

import Data.Depsolver.Repository (Repository, emptyRepository)

-- | Attempt to parse a string into a repository.
parseRepo :: String -> Maybe Repository
parseRepo "[]" = Just emptyRepository
parseRepo _    = Nothing

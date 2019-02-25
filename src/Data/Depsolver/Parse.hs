module Data.Depsolver.Parse
    ( parseRepo
    , parseVersion
    , parseDependency
    , parseRepoState
    ) where

import qualified Text.JSON as TJ

import qualified Data.Depsolver.Repository as R
import qualified Data.Depsolver.RepoState as RS
import Data.Depsolver.Repository.Internal (parseVersion)


resToMaybe :: TJ.Result a -> Maybe a
resToMaybe = either (const Nothing) Just . TJ.resultToEither


-- | Attempt to parse a string into a repository.
parseRepo :: String -> Maybe R.Repository
parseRepo = resToMaybe . TJ.decodeStrict


-- | Parse a package dependency.
parseDependency :: String -> Maybe R.VersionMatch
parseDependency = resToMaybe . TJ.decode


-- | Parse a JSON list of installed packages as a 'RepoState'.
parseRepoState :: String -> Maybe RS.RepoState
parseRepoState = resToMaybe . TJ.decodeStrict

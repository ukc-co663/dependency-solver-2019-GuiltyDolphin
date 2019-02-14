module Data.Depsolver.Parse
    ( parseRepo
    , parseVersion
    ) where

import Data.Depsolver.Repository
    (Repository, emptyRepository, Version, mkVersion)


-- | Attempt to parse a string into a repository.
parseRepo :: String -> Maybe Repository
parseRepo "[]" = Just emptyRepository
parseRepo _    = Nothing


-- | Parse a package version.
parseVersion :: String -> Maybe Version
parseVersion "" = Nothing
parseVersion vs = Just . mkVersion . splitOnPeriod $ vs
    -- as in https://www.haskell.org/onlinereport/standard-prelude.html
    where splitOnPeriod :: String -> [String]
          splitOnPeriod s = case dropWhile isPeriod s of
                              "" -> []
                              s' -> let (s1, s'') = break isPeriod s'
                                    in s1 : splitOnPeriod s''
              where isPeriod = (=='.')

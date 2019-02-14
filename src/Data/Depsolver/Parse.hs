module Data.Depsolver.Parse
    ( parseRepo
    , parseVersion
    ) where

import qualified Text.JSON as TJ

import Data.Depsolver.Repository
    ( Repository
    , mkRepository
    , mkPackage
    , Version
    , mkVersion )


-- | Attempt to parse a string into a repository.
parseRepo :: String -> Maybe Repository
parseRepo rs = do
  TJ.JSArray repoJson <- resToMaybe $ TJ.decodeStrict rs
  packages <- mapM parsePackage repoJson
  pure $ mkRepository packages
    where parsePackage (TJ.JSObject p) = do
              let packageMap = TJ.fromJSObject p
              name <- lookup "name" packageMap >>= parseName
              version <- lookup "version" packageMap >>= wantString >>= parseVersion
              pure $ mkPackage name version
          parsePackage _ = Nothing
          resToMaybe =
              either (const Nothing) Just . TJ.resultToEither
          wantString (TJ.JSString s) = Just $ TJ.fromJSString s
          wantString _ = Nothing
          parseName = wantString


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

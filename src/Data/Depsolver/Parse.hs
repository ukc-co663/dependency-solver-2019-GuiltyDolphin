module Data.Depsolver.Parse
    ( parseRepo
    , parseVersion
    , parseDependency
    ) where

import qualified Text.JSON as TJ

import Data.Depsolver.Repository
    ( Repository
    , mkRepository
    , mkPackage
    , Version
    , mkVersion )
import qualified Data.Depsolver.Repository as R


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
              let optDeps = lookup "depends" packageMap
              deps <- maybe (pure []) parseDeps optDeps
              let optConflicts = lookup "conflicts" packageMap
              conflicts <- maybe (pure []) parseConflicts optConflicts
              pure $ mkPackage name version deps conflicts
          parsePackage _ = Nothing
          resToMaybe =
              either (const Nothing) Just . TJ.resultToEither
          wantString (TJ.JSString s) = Just $ TJ.fromJSString s
          wantString _ = Nothing
          wantStrings (TJ.JSArray a) = mapM wantString a
          wantStrings _ = Nothing
          parseName = wantString
          parseDeps (TJ.JSArray deps) =
              mapM ((>>= mapM parseDependency) . wantStrings) deps
          parseDeps _ = Nothing
          parseConflicts = (>>= mapM parseDependency) . wantStrings


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


-- | Parse a package dependency.
parseDependency :: String -> Maybe R.VersionMatch
parseDependency ds =
    case break (`elem` "<=>") ds of
      (_, "") -> Nothing
      (pname, op:vs) ->
          let (vop, verStr) = case op of
                                '=' -> (R.VEQ, vs)
                                _   -> case vs of
                                         '=':vs' -> (case op of
                                                       '<' -> R.VLTE
                                                       '>' -> R.VGTE
                                                       _   -> error "pattern already satisfied"
                                                    , vs')
                                         _ -> (case op of
                                                 '<' -> R.VLT
                                                 '>' -> R.VGT
                                                 _   -> error "pattern already satisfied"
                                              , vs)
          in parseVersion verStr >>= pure . R.mkDependency pname vop

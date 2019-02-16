{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Depsolver.Repository.Internal
    (
    -- ** Repositories
      Repository(..)
    , mkRepository
    , emptyRepository

    -- ** Repository States
    , RepoState(..)
    , mkRepoState
    , emptyRepoState
    , validState

    -- ** Packages
    , PackageDesc(..)
    , mkPackage
    , PackageName(..)
    , mkPackageName

    -- ** Dependencies
    , VersionMatch(..)
    , VersionCmp(..)
    , mkDependency

    -- ** Installed Packages
    , PackageVersion(..)
    , mkPackageVersion

    -- ** Versions
    , Version
    , mkVersion
    , toVersionList
    , parseVersion
    ) where


import Control.Arrow ((&&&))
import Data.List (find, intersperse)
import Data.Maybe (fromMaybe)

import qualified Text.JSON as TJ


wantString :: TJ.JSValue -> Maybe String
wantString (TJ.JSString s) = Just $ TJ.fromJSString s
wantString _ = Nothing


-- | Repository containing information about available
-- | packages.
newtype Repository = Repository {
      -- ^ Available packages in the repository.
      repoPackages :: [PackageDesc]
    } deriving (Eq)


deriving instance TJ.JSON Repository


instance Show Repository where
    show = TJ.encodeStrict


-- | Create a new repository with the given package descriptions.
mkRepository :: [PackageDesc] -> Repository
mkRepository = Repository


-- | Repository with no packages.
emptyRepository :: Repository
emptyRepository = Repository { repoPackages = [] }


getPackage :: Repository -> PackageVersion -> Maybe PackageDesc
getPackage r pv = find (\p -> pv == toPackageVersion p) (repoPackages r)


-- | The list of installed packages (and their versions).
newtype RepoState = RepoState {
      -- ^ Packages and their installed version.
      repoStatePackageVersions :: [PackageVersion]
    } deriving (Eq)


deriving instance TJ.JSON RepoState


instance Show RepoState where
    show = TJ.encodeStrict


-- | Create a new repository state with the given installed
-- | packages.
mkRepoState :: [PackageVersion] -> RepoState
mkRepoState = RepoState


-- | The state of a repository with no installed packages.
emptyRepoState :: RepoState
emptyRepoState = RepoState []


-- | True if the state is valid given the constraints of
-- | the repository.
validState :: Repository -> RepoState -> Bool
validState r rs = all meetsPackageDependencies . repoStatePackageVersions $ rs
    where meetsPackageDependencies pv =
              maybe False (meetsADependency . packageDependencies) (getPackage r pv)
          stateMeetsDependency [vm] =
            maybe False stateHasPackage $ repoProvidesMatchingPackage vm
          stateMeetsDependency _ = False
          repoProvidesMatchingPackage (VersionMatch pname _ version) =
              getPackage r (mkPackageVersion pname version)
          stateHasPackage pv = (toPackageVersion pv) `elem` (repoStatePackageVersions rs)
          meetsADependency [] = True
          meetsADependency deps = any stateMeetsDependency deps


newtype PackageName = PackageName { getPackageName :: String }
    deriving (Eq)


instance Show PackageName where
    show = getPackageName


instance TJ.JSON PackageName where
    showJSON = TJ.JSString . TJ.toJSString . getPackageName
    readJSON = maybe (TJ.Error "package name") (TJ.Ok . mkPackageName) . wantString


mkPackageName :: String -> PackageName
mkPackageName = PackageName


-- | A package and its installed version.
newtype PackageVersion = PackageVersion {
      getPackageVersion :: (PackageName, Version)
    } deriving (Eq)


instance Show PackageVersion where
    show (PackageVersion (p, v)) = show p <> "=" <> show v


instance TJ.JSON PackageVersion where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON =
        maybe (TJ.Error "package=version") TJ.Ok . ((>>= parsePackState) . wantString)
        where
          parsePackState pv =
              case break (=='=') pv of
                (name, '=':versionStr) ->
                    fmap (mkPackageVersion (mkPackageName name)) (parseVersion versionStr)
                _ -> Nothing


-- | Make a new PackageVersion from a package name and
-- | its installed version.
mkPackageVersion :: PackageName -> Version -> PackageVersion
mkPackageVersion = curry PackageVersion


newtype Version = Version {
      -- ^ The components of the version as a list.
      -- ^
      -- ^ @toVersionList (mkVersion ["1", "0"]) == ["1", "0"]@
      toVersionList :: [String]
    } deriving (Eq)


instance Show Version where
    show = concat . intersperse "." . toVersionList


instance TJ.JSON Version where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON = maybe (TJ.Error "version string") TJ.Ok . ((>>=parseVersion) . wantString)


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


-- | Make a new version with the given version specifiers.
-- |
-- | The left-most specifier should be the most major version
-- | identifier, e.g., 1.0.0 could be represented as
-- | @mkVersion ["1", "0", "0"]@.
mkVersion :: [String] -> Version
mkVersion = Version


data PackageDesc = PackageDesc {
      -- ^ Name of the package.
      packageName :: PackageName
      -- ^ Version of the package.
    , packageVersion :: Version
      -- ^ Dependencies of the package
      -- ^ (as a conjunction of disjunctions).
    , packageDependencies :: [[VersionMatch]]
      -- ^ Packages that conflict with the package.
    , packageConflicts :: [VersionMatch]
    } deriving (Eq)


instance TJ.JSON PackageDesc where
    showJSON (PackageDesc { packageName = name
                          , packageVersion = version
                          , packageDependencies = deps
                          , packageConflicts = conflicts
                          }) = TJ.JSObject $
                             TJ.toJSObject [ ("name", TJ.showJSON . getPackageName $ name)
                                           , ("version", TJ.showJSON version)
                                           , ("depends", TJ.showJSON deps)
                                           , ("conflicts", TJ.showJSON conflicts)]
    readJSON (TJ.JSObject p) = do
              let packageMap = TJ.fromJSObject p
              name <- reqField packageMap "name"
              version <- reqField packageMap "version"
              deps <- optFieldWithDefault [] packageMap "depends"
              conflicts <- optFieldWithDefault [] packageMap "conflicts"
              pure $ mkPackage name version deps conflicts
        where
          reqField :: (TJ.JSON a) => [(String, TJ.JSValue)] -> String -> TJ.Result a
          reqField pmap fieldName =
              fromMaybe (TJ.Error ("'" ++ fieldName ++ "' field")) $ optField pmap fieldName
          optField :: (TJ.JSON a) => [(String, TJ.JSValue)] -> String -> Maybe (TJ.Result a)
          optField pmap fieldName = fmap TJ.readJSON $ lookup fieldName pmap
          optFieldWithDefault :: (TJ.JSON a) => a -> [(String, TJ.JSValue)] -> String -> TJ.Result a
          optFieldWithDefault default' pmap = fromMaybe (TJ.Ok default') . optField pmap
    readJSON _ = TJ.Error "package description must be an object"


instance Show PackageDesc where
    show = TJ.encodeStrict


mkPackage :: PackageName -> Version -> [[VersionMatch]] -> [VersionMatch] -> PackageDesc
mkPackage name version deps conflicts =
    PackageDesc { packageName = name
                , packageVersion = version
                , packageDependencies = deps
                , packageConflicts = conflicts
                }


toPackageVersion :: PackageDesc -> PackageVersion
toPackageVersion = uncurry mkPackageVersion . (packageName&&&packageVersion)


data VersionCmp = VLTE | VLT | VEQ | VGT | VGTE
                  deriving (Eq)


instance Show VersionCmp where
    show cmp = case cmp of
                 VLTE -> "<="
                 VLT  -> "<"
                 VEQ  -> "="
                 VGT  -> ">"
                 VGTE -> ">="


data VersionMatch = VersionMatch PackageName VersionCmp Version
                    deriving (Eq)


instance Show VersionMatch where
    show (VersionMatch p cmp v) = concat [show p, show cmp, show v]


instance TJ.JSON VersionMatch where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON = maybe (TJ.Error "version specifier string") parseDependency . wantString
        where parseDependency :: String -> TJ.Result VersionMatch
              parseDependency ds =
                  case break (`elem` "<=>") ds of
                    (_, "") -> TJ.Error "version specifier operator"
                    (pname, op:vs) ->
                        let (vop, verStr) = case op of
                                              '=' -> (VEQ, vs)
                                              _   -> case vs of
                                                       '=':vs' -> (case op of
                                                                     '<' -> VLTE
                                                                     '>' -> VGTE
                                                                     _   -> error "pattern already satisfied"
                                                                  , vs')
                                                       _ -> (case op of
                                                               '<' -> VLT
                                                               '>' -> VGT
                                                               _   -> error "pattern already satisfied"
                                                            , vs)
                        in maybe (TJ.Error "version") TJ.Ok (parseVersion verStr)
                               >>= pure . mkDependency (mkPackageName pname) vop


mkDependency :: PackageName -> VersionCmp -> Version -> VersionMatch
mkDependency = VersionMatch

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Depsolver.Repository.Internal
    (
    -- ** Repositories
      Repository(..)
    , mkRepository
    , emptyRepository
    , repoPackages
    , lookupPackage
    , getPackagesThatSatisfy

    -- ** Packages
    , PackageDesc(..)
    , mkPackage
    , PackageName(..)
    , mkPackageName
    , packageId

    -- *** Package Size
    , Size(..)
    , mkSize

    -- ** Dependencies
    , Dependencies
    , mkDependencies
    , mkDependencies'
    , fromDependencies
    , emptyDependencies
    , dependencyIsMet
    , Conflicts
    , mkConflicts
    , mkConflicts'
    , fromConflicts
    , emptyConflicts
    , conflictIsMet
    , VersionMatch(..)
    , VersionCmp(..)
    , mkDependency
    , mkWildcardDependency

    -- ** Installed Packages
    , PackageId(..)
    , mkPackageId
    , packageIdName
    , packageIdVersion

    -- ** Versions
    , Version
    , mkVersion
    , toVersionList
    , parseVersion

    -- ** Parser helpers
    , parseDependency
    , wantString
    ) where


import Control.Arrow ((&&&))
import qualified Data.Foldable as F
import Data.Function (on)
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as M
import Data.List (dropWhileEnd, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Text.JSON as TJ


wantString :: TJ.JSValue -> Maybe String
wantString (TJ.JSString s) = Just $ TJ.fromJSString s
wantString _ = Nothing


type PackageMap = M.HashMap PackageName (M.HashMap Version PackageDesc)


-- | Repository containing information about available
-- | packages.
newtype Repository = Repository {
      -- ^ Available packages in the repository.
      fromRepository :: PackageMap
    } deriving (Eq)


repoPackages :: Repository -> [PackageDesc]
repoPackages = mapOp (M.foldr (\vmap packs -> packs <> M.elems vmap) [])


instance TJ.JSON Repository where
    readJSON = fmap mkRepository . TJ.readJSON
    showJSON = TJ.showJSON . repoPackages


instance Show Repository where
    show = TJ.encodeStrict


-- | Create a new repository with the given package descriptions.
mkRepository :: [PackageDesc] -> Repository
mkRepository pvs =
    Repository $ foldr insertPackage' M.empty pvs
    where insertPackage' p m =
              let n = packageName p
                  v = packageVersion p
                  vmap = M.lookupDefault M.empty n m
                  vmap' = M.insert v p vmap
              in M.insert n vmap' m


-- | Repository with no packages.
emptyRepository :: Repository
emptyRepository = Repository M.empty


mapOp :: (PackageMap -> a) -> Repository -> a
mapOp f = f . fromRepository


lookupPackage :: PackageId -> Repository -> Maybe PackageDesc
lookupPackage pid r = let (n, v) = getPackageId pid
                      in mapOp (\m -> M.lookup n m >>= M.lookup v) r


getPackagesThatSatisfy :: Repository -> VersionMatch -> Maybe [PackageDesc]
getPackagesThatSatisfy r (VersionMatchWild pname) = getPackageAnyVersion r pname
getPackagesThatSatisfy r (VersionMatch pname cmp version) = do
  withSameName <- getPackageAnyVersion r pname
  pure $ filter satisfiesVersionReq withSameName
    where satisfiesVersionReq pkg =
              let cmpop = case cmp of
                            VLT -> (<)
                            VGT -> (>)
                            VEQ -> (==)
                            VLTE -> (<=)
                            VGTE -> (>=)
              in cmpop (packageVersion pkg) version


getPackageAnyVersion :: Repository -> PackageName -> Maybe [PackageDesc]
getPackageAnyVersion r pn = mapOp (fmap M.elems . M.lookup pn) r


newtype PackageName = PackageName { getPackageName :: String }
    deriving (Eq, Ord, Hashable)


instance Show PackageName where
    show = getPackageName


instance TJ.JSON PackageName where
    showJSON = TJ.JSString . TJ.toJSString . getPackageName
    readJSON = maybe (TJ.Error "package name") (TJ.Ok . mkPackageName) . wantString


mkPackageName :: String -> PackageName
mkPackageName = PackageName


-- | A package and its installed version.
newtype PackageId = PackageId {
      getPackageId :: (PackageName, Version)
    } deriving (Eq, Ord, Hashable)


instance Show PackageId where
    show (PackageId (p, v)) = show p <> "=" <> show v


instance TJ.JSON PackageId where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON =
        maybe (TJ.Error "package=version") TJ.Ok . ((>>= parsePackState) . wantString)
        where
          parsePackState pv =
              case break (=='=') pv of
                (name, '=':versionStr) ->
                    fmap (mkPackageId (mkPackageName name)) (parseVersion versionStr)
                _ -> Nothing


-- | Make a new PackageId from a package name and
-- | its installed version.
mkPackageId :: PackageName -> Version -> PackageId
mkPackageId = curry PackageId


-- | Get the package name from a package identifier.
packageIdName :: PackageId -> PackageName
packageIdName = fst . getPackageId


-- | Get the package version from a package identifier.
packageIdVersion :: PackageId -> Version
packageIdVersion = snd . getPackageId


data Version = Version {
      -- ^ The components of the version as a list.
      -- ^
      -- ^ @toVersionList (mkVersion ["1", "0"]) == ["1", "0"]@
      toVersionList :: [String]
    , getCanonicalVersionString :: [Integer]
    }


instance Hashable Version where
    hashWithSalt n (Version _ cv) = hashWithSalt n cv


-- | Produce the canonical representation of the given version.
-- |
-- | - leading zeros are removed (e.g., 10.02 becomes 10.2)
-- | - trailing zero components are removed (e.g., 10.0 becomes 10)
canonicalVersionString :: [String] -> [Integer]
canonicalVersionString = (dropWhileEnd (==0)) . fmap read


instance Eq Version where
    (==) = (==) `on` getCanonicalVersionString


instance Ord Version where
    (<=) = (<=) `on` getCanonicalVersionString


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
mkVersion vs = Version vs (canonicalVersionString vs)


-- | A package size (a postive integer).
newtype Size = Size { fromSize :: Int } deriving (Bounded, Eq, Num, Ord)


deriving instance TJ.JSON Size


-- | Create a new package size.
mkSize :: Int -> Size
mkSize = Size


-- | Package dependencies as a conjunction of disjunctions.
newtype Dependencies = Dependencies { fromDependencies :: Set.Set (Set.Set VersionMatch) }
    deriving (Eq, Semigroup, Show)


deriving instance TJ.JSON Dependencies


mkDependencies' :: Set.Set (Set.Set VersionMatch) -> Dependencies
mkDependencies' = Dependencies


-- | Construct a set of dependencies from a list of packages to match.
-- |
-- | The list should be given as a conjunction of disjunctions; at least
-- | one requirement from each inner list must be satisfied.
mkDependencies :: [[VersionMatch]] -> Dependencies
mkDependencies = mkDependencies' . Set.fromList . fmap Set.fromList


-- | The empty set of dependencies.
emptyDependencies :: Dependencies
emptyDependencies = mkDependencies []


-- | True if a list of packages satisfies the dependencies for some repository.
dependencyIsMet :: F.Foldable f => Repository -> Dependencies -> f PackageId -> Bool
dependencyIsMet r deps pvs =
    let depSet = fromDependencies deps
    in F.null depSet || F.any meetsDependency depSet
    where meetsDependency =
              all (maybe False pvsHasAnyOf . fmap (fmap packageId) . getPackagesThatSatisfy r)
          pvsHasAnyOf = F.any (`elem`pvs)


-- | Package conflicts.
newtype Conflicts = Conflicts { fromConflicts :: Set.Set VersionMatch }
    deriving (Eq, Semigroup, Show)


deriving instance TJ.JSON Conflicts


mkConflicts' :: Set.Set VersionMatch -> Conflicts
mkConflicts' = Conflicts


-- | Construct a set of conflicts from a list of packages to match against.
-- |
-- | None of the constraints can be met for the conflicts to be satisfied.
mkConflicts :: [VersionMatch] -> Conflicts
mkConflicts = mkConflicts' . Set.fromList


-- | The empty set of conflicts.
emptyConflicts :: Conflicts
emptyConflicts = mkConflicts []


conflictsToDependencies :: Conflicts -> Dependencies
conflictsToDependencies = Dependencies . Set.singleton . fromConflicts


-- | True if a list of packages conflicts with the given conflicts requirement.
conflictIsMet :: F.Foldable f => Repository -> Conflicts -> f PackageId -> Bool
conflictIsMet r cs ps = not (F.null (fromConflicts cs))
                     && dependencyIsMet r (conflictsToDependencies cs) ps


data PackageDesc = PackageDesc {
      -- ^ Name of the package.
      packageName :: PackageName
      -- ^ Version of the package.
    , packageVersion :: Version
      -- ^ Size of the package.
    , packageSize :: Size
      -- ^ Dependencies of the package
      -- ^ (as a conjunction of disjunctions).
    , packageDependencies :: Dependencies
      -- ^ Packages that conflict with the package.
    , packageConflicts :: Conflicts
    } deriving (Eq)


instance TJ.JSON PackageDesc where
    showJSON (PackageDesc { packageName = name
                          , packageVersion = version
                          , packageSize = size
                          , packageDependencies = deps
                          , packageConflicts = conflicts
                          }) = TJ.JSObject $
                             TJ.toJSObject [ ("name", TJ.showJSON . getPackageName $ name)
                                           , ("version", TJ.showJSON version)
                                           , ("size", TJ.showJSON size)
                                           , ("depends", TJ.showJSON deps)
                                           , ("conflicts", TJ.showJSON conflicts)]
    readJSON (TJ.JSObject p) = do
              let packageMap = TJ.fromJSObject p
              name <- reqField packageMap "name"
              version <- reqField packageMap "version"
              size <- reqField packageMap "size"
              deps <- optFieldWithDefault [] packageMap "depends"
              conflicts <- optFieldWithDefault [] packageMap "conflicts"
              pure $ mkPackage name version size deps conflicts
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


mkPackage :: PackageName -> Version -> Size -> [[VersionMatch]] -> [VersionMatch] -> PackageDesc
mkPackage name version size deps conflicts =
    PackageDesc { packageName = name
                , packageVersion = version
                , packageSize = size
                , packageDependencies = mkDependencies deps
                , packageConflicts = mkConflicts conflicts
                }


packageId :: PackageDesc -> PackageId
packageId = uncurry mkPackageId . (packageName&&&packageVersion)


data VersionCmp = VLTE | VLT | VEQ | VGT | VGTE
                  deriving (Eq, Ord)


instance Show VersionCmp where
    show cmp = case cmp of
                 VLTE -> "<="
                 VLT  -> "<"
                 VEQ  -> "="
                 VGT  -> ">"
                 VGTE -> ">="


data VersionMatch = VersionMatch PackageName VersionCmp Version
                  | VersionMatchWild PackageName
                    deriving (Eq, Ord)


instance Show VersionMatch where
    show (VersionMatch p cmp v) = concat [show p, show cmp, show v]
    show (VersionMatchWild p) = show p


instance TJ.JSON VersionMatch where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON = maybe (TJ.Error "version specifier string") parseDependency . wantString


-- | Parse a dependency string (without quotes).
parseDependency :: String -> TJ.Result VersionMatch
parseDependency ds =
    case break (`elem` "<=>") ds of
      (pname, "") -> pure . mkWildcardDependency . mkPackageName $ pname
      (pname, op:vs) ->
          let (vop, verStr) =
                  case op of
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


mkWildcardDependency :: PackageName -> VersionMatch
mkWildcardDependency = VersionMatchWild

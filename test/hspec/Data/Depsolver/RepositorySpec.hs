module Data.Depsolver.RepositorySpec (spec) where

import TestHelper

import Data.List (tails)
import Data.Maybe (fromJust)

import Data.Depsolver.Repository
    ( repoPackages
    , emptyRepository
    , mkVersion
    , toVersionList
    )


import qualified Data.Depsolver.Parse as P
import qualified Data.Depsolver.Repository as R


-- | Given a list of version strings, ensure that every
-- | version earlier in the list version satisfies the
-- | comparator when compared with any version later in the list.
versionTest :: String -> (R.Version -> R.Version -> Bool) -> [String] -> SpecWith ()
versionTest s cmp vstrs =
    describe (unwords ["V1", s, "V2"]) $
      let orderedVersions = fmap (fromJust . P.parseVersion) vstrs
          versionTails = fmap (\(vg : vs) -> (vg, vs)) (init . init . tails $ orderedVersions)
      in mapM_ (\(v1, vs) -> mapM_ (vtest v1) vs) versionTails
    where vtest v1 v2 = it (unwords [show v1, s, show v2]) $ (v1, v2) `shouldSatisfy` uncurry cmp


spec :: Spec
spec = do
  describe "version comparison" $ do
         versionTest ">" (>) ["10", "2", "1.7.3", "1.2", "1", "0.1", "0"]
         versionTest "=" (==) ["10.2", "10.02", "10.2.0", "10.02.0", "10.02.00", "010.02.00"]
  describe "toVersionList" $ do
         it "is inverse of mkVersion" $
            property $ \vs -> toVersionList (mkVersion vs) == vs
  describe "repoPackages" $ do
         it "empty repository has no packages" $
            repoPackages emptyRepository `shouldBe` []

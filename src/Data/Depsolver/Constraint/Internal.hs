{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Depsolver.Constraint.Internal
    (
    -- ** Constraint
      Constraint(..)
    , mkNegativeConstraint
    , mkPositiveConstraint

    -- ** Constraints
    , Constraints
    , emptyConstraints
    , fromConstraints
    , mkConstraints
    ) where


import qualified Text.JSON as TJ

import Data.Depsolver.Repository
import Data.Depsolver.Repository.Internal (parseDependency, wantString)


newtype Constraints = Constraints { fromConstraints :: [Constraint] }
    deriving (Eq)


instance Show Constraints where
    show = TJ.encodeStrict


deriving instance TJ.JSON Constraints


-- | A constraint on the final package state..
data Constraint =
    -- | The final state must have a matching package.
    Wanted VersionMatch |
    -- | The final state must not have a matching package.
    Unwanted VersionMatch
    deriving (Eq)


instance Show Constraint where
    show (Wanted v)   = '+' : show v
    show (Unwanted v) = '-' : show v


instance TJ.JSON Constraint where
    showJSON = TJ.JSString . TJ.toJSString . show
    readJSON = maybe (TJ.Error "constraint string") parseConstraint . wantString
        where parseConstraint :: String -> TJ.Result Constraint
              parseConstraint s =
                  case s of
                    ('+':ds) -> parseDependency ds >>= pure . Wanted
                    ('-':ds) -> parseDependency ds >>= pure . Unwanted
                    _ -> TJ.Error "constraint"


-- | Build a set of constraints from a list of constraints.
mkConstraints :: [Constraint] -> Constraints
mkConstraints = Constraints


-- | An empty set of constraints.
emptyConstraints :: Constraints
emptyConstraints = mkConstraints []


-- | Indicate the final state should satisfy the given constraint.
mkPositiveConstraint :: VersionMatch -> Constraint
mkPositiveConstraint = Wanted


-- | Indicate the final state should not satisfy the given constraint.
mkNegativeConstraint :: VersionMatch -> Constraint
mkNegativeConstraint = Unwanted

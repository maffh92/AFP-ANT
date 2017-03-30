{-# LANGUAGE DeriveGeneric #-}
module Ant.Arbitrary.Optimization where

import           Generic.Random.Generic
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Control.Category
import           Prelude                hiding (id, (.))

import           Ant.Optimization

-- | Reified optimization.
data Op = UnRC         -- ^ unreachableOpt
        | DC           -- ^ duplicateCodeOpt
        | Op :.: Op    -- ^ composition
        | Id           -- ^ identity
        deriving (Eq, Ord, Generic)

instance Arbitrary Op where
  arbitrary = genericArbitrary uniform

instance Show Op where
  show op =
    case op of
      UnRC          -> "unreachableOpt"
      DC            -> "duplicateCodeOpt"
      (op1 :.: op2) -> "(" ++ show op1 ++ "." ++ show op2 ++ ")"
      Id            -> "id"

-- | Interpret a reified Optimization as a real optimization.
toOptimization :: Ord l => Op -> Optimization l
toOptimization op =
  case op of
    UnRC          -> unreachableOpt
    DC            -> duplicateCodeOpt
    (op1 :.: op2) -> toOptimization op1 . toOptimization op2
    Id            -> id

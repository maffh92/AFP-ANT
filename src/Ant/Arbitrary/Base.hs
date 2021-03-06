{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module: Ant.Arbitrary.Base
Description: Generate arbitrary ant programs using QuickCheck
-}

module Ant.Arbitrary.Base where

import           Control.Category
import           Control.Monad          (replicateM)
import           Generic.Random.Generic
import           Prelude                hiding (id, (.))
import           Test.QuickCheck
import           Data.List              (foldl')

import           Ant
import           Ant.Base

-- | An AntMTest
newtype AntMTest l = AntMTest { unAntMTest :: [Command l] }
                   deriving Show

-- | An arbitrary label
instance Label l => Arbitrary (AntMTest l) where
  arbitrary = do
    n <- (+1) <$> arbitrarySizedNatural
    AntMTest <$> genProgram n

-- | Generate a program
genProgram :: Label l => Int -> Gen [Command l]
genProgram n =
  let lbs = take n (iterate su z)
  in  replicateM n (genCommand lbs)

-- | Generate a command
genCommand :: Label l => [l] -> Gen (Command l)
genCommand lbs =
  oneof [ Mark   <$> arbitrary <*> genGoto
        , Unmark <$> arbitrary <*> genGoto
        , Drop   <$> genGoto
        , Turn   <$> arbitrary <*> genGoto

        , Move   <$> genGoto <*> genGoto
        , Flip   <$> (getPositive <$> arbitrary) <*> genGoto <*> genGoto
        , Sense  <$> arbitrary <*> genGoto <*> genGoto <*> arbitrary
        , PickUp <$> genGoto <*> genGoto]
  where
    genGoto = elements lbs

-- | Make an AntMT from an AntMTest
toAntM :: Label l => AntMTest l -> AntM l ()
toAntM = foldl' (\m -> (m >>) . interpret) (return ())
       . unAntMTest

-- | Make an AntM out of a command
interpret :: Label l => Command l -> AntM l ()
interpret cmd =
  case cmd of
    Sense d l1 l2 c ->  sense d c (goto l1) (goto l2)
    Mark m l        ->  mark m >> goto l
    Unmark m l      ->  unmark m >> goto l
    PickUp l1 l2    ->  pickup (goto l1) (goto l2)
    Drop l          ->  drop' >> goto l
    Turn dir l      ->  turn dir >> goto l
    Move l1 l2      ->  move (goto l1) (goto l2)
    Flip toss l1 l2 ->  flip' toss (goto l1) (goto l2)

-- Arbitrary instances for everything

-- | Arbitrary instance for SenseDir
instance Arbitrary SenseDir where
  arbitrary = genericArbitrary uniform

-- | Arbitrary instance for Condition
instance Arbitrary Condition where
  arbitrary = genericArbitrary uniform

-- | Arbitrary instance for Marker
instance Arbitrary Marker where
  arbitrary = genericArbitrary uniform

-- | Arbitrary instance for LeftOrRight
instance Arbitrary LeftOrRight where
  arbitrary = genericArbitrary uniform


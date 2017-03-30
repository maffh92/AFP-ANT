{-# OPTIONS_GHC -Wno-orphans #-}
module Spec.Base where

import           Control.Category
import           Control.Monad          (replicateM)
import           Generic.Random.Generic
import           Prelude                hiding (id, (.))
import           Test.QuickCheck

import           Ant
import           Ant.Base

newtype AntMTest = AntMTest { unAntMTest :: [Command L] }
                            deriving (Show)

instance Arbitrary AntMTest where
  arbitrary = do
    n <- (+1) <$> arbitrarySizedNatural
    let lbs = take n (iterate s z)
    AntMTest <$> replicateM n (genCommand lbs)

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

toAntM :: AntMTest -> AntM L ()
toAntM = interpretAll . unAntMTest

interpretAll :: Label l => [Command l] -> AntM l ()
interpretAll = foldl (\m -> (m >>) . interpret) (return ())

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
instance Arbitrary SenseDir where
  arbitrary = genericArbitrary uniform

instance Arbitrary Condition where
  arbitrary = genericArbitrary uniform

instance Arbitrary Marker where
  arbitrary = genericArbitrary uniform

instance Arbitrary LeftOrRight where
  arbitrary = genericArbitrary uniform


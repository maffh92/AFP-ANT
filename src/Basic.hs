{- Basic combinators that can be combined to get more complex constructs.
 -
 -}

{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Basic
  ( forever
  , ifThenElse
  , doWhile
  , chance
  ) where

import Control.Monad.Fix
import Data.Ratio
import Ant.Monad
import Ant.Base

-- | Runs a program forever
forever :: (MonadFix m, Label l) => AntT m l () -> AntT m l ()
forever program =
  mdo
    x <- label
    program
    goto x

-- | Alias for sense
ifThenElse :: (MonadFix m, Label l) => SenseDir -> Condition -> AntT m l () -> AntT m l () -> AntT m l ()
ifThenElse = sense

-- | Repeats the program @branch1@ until the @senser@ fails, then it will branch
-- to @branch2@.
doWhile :: (MonadFix m, Label l) => SenseDir -> Condition -> AntT m l () -> AntT m l () -> AntT m l ()
doWhile dir cond b1 b2 =
  mdo
    x <- label
    b1
    sense dir cond (goto x) b2

-- | Given a percentage @k@, attempts to approach the percentage with the
-- formula round (100 / k) - 1 to obtain the /n/ argument for flip.
--  {- example
--   - chance 50 p1 p2 = flip  1 p1 p2 -- for a ~ 50% chance for p1
--   - chance 2  p1 p2 = flip 49 p1 p2 -- for a ~  2% chance for p1
--   -}
--
-- NOTE: k <=   0 will always choose b2
--       k >= 100 will never choose b2
chance :: (MonadFix m, Label l) => Int -> AntT m l () -> AntT m l () -> AntT m l ()
chance k b1 b2
  | k <=   0 = b2 -- Choose branch1 with   0% chance
  | k >= 100 = b1 -- Choose branch1 with 100% chance
  | otherwise =
    let k' = k % 100
        d' = round $ (fromIntegral . denominator $ k') /
          (fromIntegral . numerator $ k')
    in flip' (d' - 1) b1 b2



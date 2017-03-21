module Ant
  ( module Ant.Base
  , module Ant.Monad
  , compile
  ) where

import           Ant.Base
import           Ant.Monad
import           Control.Lens
import           Data.Map     (toList)

compile :: AntM L () -> String
compile = showCmds . toList . view commands . fst . snd . runAntM zero

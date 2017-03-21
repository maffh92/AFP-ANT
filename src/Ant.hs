module Ant
  ( module Ant.Base
  , module Ant.Monad
  , compile
  ) where

import           Ant.Base
import           Ant.Monad
import           Control.Lens
import           Data.Map     (toList)

compile :: AntM Int () -> String
compile = showCmds . toList . view commands . fst . snd . runAntM 0

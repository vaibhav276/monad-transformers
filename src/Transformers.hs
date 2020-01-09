-- -*- dante-target: "monad-transformers"; -*-

module Transformers where

import Control.Monad.Identity

type M a = Identity a

runEval :: M a -> a
runEval ma = runIdentity ma

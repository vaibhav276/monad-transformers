-- -*- dante-target: "monad-transformers"; -*-

module Transformers where

import Control.Monad.Identity
import Control.Monad.Except

type M a = ExceptT String Identity a

runEval :: M a -> Either String a
runEval ma = runIdentity (runExceptT ma)

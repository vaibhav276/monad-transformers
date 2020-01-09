-- -*- dante-target: "monad-transformers"; -*-

module Eval where

import qualified Data.Map     as Map
import           Data.Maybe

type Name = String

data Exp =
  Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value =
  IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value

eval :: Monad m => Env -> Exp -> m Value
eval _ (Lit i) = pure $ IntVal i
eval e (Var x) = pure $ fromJust $ Map.lookup x e -- TODO: no failure case handling
eval e (Plus e1 e2) = do v1 <- eval e e1
                         v2 <- eval e e2
                         case v1 of -- TODO: non-exhaustive pattern matches
                           IntVal i1 -> case v2 of -- TODO: non-exhaustive pattern matches
                             IntVal i2 -> pure $ IntVal (i1 + i2)
eval e (Abs n e1) = pure $ FunVal e n e1
eval e (App e1 e2) = do v1 <- eval e e1
                        v2 <- eval e e2
                        case v1 of -- TODO: non-exhaustive pattern matches
                          FunVal e' n' b' -> case v2 of -- TODO: non-exhaustive pattern matches
                            IntVal _ -> eval (Map.insert n' v2 e') b'


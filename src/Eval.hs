-- -*- dante-target: "monad-transformers"; -*-

module Eval where

import           Control.Monad.Except
import qualified Data.Map             as Map

import           Transformers

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

eval :: Env -> Exp -> M Value
eval _ (Lit i) = pure $ IntVal i
eval e (Var x) = case Map.lookup x e of
                   Just v -> pure v
                   _      -> throwError ("Undefined var: " ++ x)
eval e (Plus e1 e2) = do v1 <- eval e e1
                         v2 <- eval e e2
                         case (v1, v2) of
                           (IntVal i1, IntVal i2) -> pure $ IntVal (i1 + i2)
                           _                      -> throwError "Type error"
eval e (Abs n e1) = pure $ FunVal e n e1
eval e (App e1 e2) = do v1 <- eval e e1
                        v2 <- eval e e2
                        case (v1, v2) of
                          (FunVal e' n' b', IntVal _) -> eval (Map.insert n' v2 e') b'
                          _                           -> throwError "Type error"

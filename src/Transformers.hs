-- | Monad Transformers

module Transformers where

import qualified Data.Map   as Map
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

eval :: Env -> Exp -> Value
eval e (Lit i) = IntVal i
eval e (Var x) = fromJust (Map.lookup x e)
eval e (Plus e1 e2) = let IntVal v1 = eval e e1
                          IntVal v2 = eval e e2
                      in IntVal (v1 + v2)
eval e (Abs n e1) = FunVal e n e1
eval e (App e1 e2) = let (FunVal e' n' b') = eval e e1
                         v@(IntVal _) = eval e e2
                     in eval (Map.insert n' v e') b'

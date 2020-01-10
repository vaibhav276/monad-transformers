-- -*- dante-target: "monad-transformers"; -*-

module Eval where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.Map               as Map


type M = ReaderT Env (ExceptT String Identity)

runEval :: Env -> M a -> Either String a
runEval e ma = runIdentity (runExceptT (runReaderT ma e))


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

eval :: Exp -> M Value
eval (Lit i) = pure $ IntVal i
eval (Var x) = do e <- ask
                  case Map.lookup x e of
                    Just v -> pure v
                    _      -> throwError ("Undefined var: " ++ x)
eval (Plus e1 e2) = do v1 <- eval e1
                       v2 <- eval e2
                       case (v1, v2) of
                         (IntVal i1, IntVal i2) -> pure $ IntVal (i1 + i2)
                         _                      -> throwError "Type error"
eval (Abs n e1) = do e <- ask
                     pure $ FunVal e n e1
eval (App e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      case (v1, v2) of
                        (FunVal e' n' b', IntVal _) -> local (const (Map.insert n' v2 e')) (eval b')
                        _                           -> throwError "Type error"

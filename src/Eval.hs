-- -*- dante-target: "monad-transformers"; -*-

module Eval where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map               as Map


type M = ReaderT Env (ExceptT String (StateT Integer (WriterT [String] Identity)))

runEval :: Env -> St -> M a -> ((Either String a, St), [String])
runEval e s ma = runIdentity (runWriterT (runStateT (runExceptT (runReaderT ma e)) s))

tick :: (Num s, MonadState s m) => m ()
-- tick = do s <- get
--           put (s+1)
tick = modify (+1)

type Env = Map.Map Name Value
type St = Integer
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

eval :: Exp -> M Value
eval (Lit i) = do tick
                  pure $ IntVal i
eval (Var x) = do tick
                  tell ["Var ref: " ++ x]
                  e <- ask
                  case Map.lookup x e of
                    Just v -> pure v
                    _      -> throwError ("Undefined var: " ++ x)
eval (Plus e1 e2) = do tick
                       v1 <- eval e1
                       v2 <- eval e2
                       case (v1, v2) of
                         (IntVal i1, IntVal i2) -> pure $ IntVal (i1 + i2)
                         _                      -> throwError "Type error"
eval (Abs n e1) = do tick
                     e <- ask
                     pure $ FunVal e n e1
eval (App e1 e2) = do tick
                      v1 <- eval e1
                      v2 <- eval e2
                      case (v1, v2) of
                        (FunVal e' n' b', IntVal _) -> local (const (Map.insert n' v2 e')) (eval b')
                        _                           -> throwError "Type error"

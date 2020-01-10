-- -*- dante-target: "monad-transformers"; -*-
module Main where

import qualified Data.Map as Map

import Eval

main :: IO ()
main = putStrLn $ unlines $ runTests tests

-- Tests
tests :: [Exp]
tests = [
  Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
  , Plus (Lit 1) (Abs "x" (Var "x"))
  ]

runTests :: [Exp] -> [String]
runTests = map (show . runEval Map.empty . eval)

-- >>> (runEval Map.empty . eval) (tests!!0)
-- Right (IntVal 18)

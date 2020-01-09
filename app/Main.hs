-- -*- dante-target: "monad-transformers"; -*-
module Main where

import qualified Data.Map as Map

import Transformers
import Eval

main :: IO ()
main = putStrLn $ unlines $ runTests tests

-- Tests
tests :: [Exp]
tests = [
  Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
  ]

runTests :: [Exp] -> [String]
runTests ts = map (show . runEval . (eval Map.empty)) ts

-- >>> runEval (eval Map.empty exampleExp)
-- IntVal 18

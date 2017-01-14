{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalSpec where

import Data.Text
import Test.QuickCheck
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Parser
import EvalT

runCode :: Text -> Env -> Env
runCode code env = env' where
    (_, env') = runState (runExceptT $ eval $ unpack code) env

assertEnv :: Env -> [(String, Value)] -> Bool
assertEnv env [] = True
assertEnv env ((var, value):xs) = case M.lookup (pack var) env of
    Just v -> (v == value) && assertEnv env xs
    Nothing -> False

test :: String -> [(String, Value)] -> Bool
test code = assertEnv env' where
    env' = runCode (pack code) M.empty

-- triple: (desciption, code, assert table)
evalSpecs = packSpecs [
        ("Expr-test1", "(set! a (+ 1 2))", [("a", DoubleValue 3)])
    ] where
        packSpecs [] = []
        packSpecs ((desc, code, table):xs) = (desc, quickCheck $ test code table) : packSpecs xs

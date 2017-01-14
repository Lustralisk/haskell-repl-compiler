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
        -- set
        ("Set-test1", "(begin (set! abc 1.2) (set! def 3.2) (set! aaa (+ abc def)))",
            [("abc", DoubleValue 1.2), ("def", DoubleValue 3.2), ("aaa", DoubleValue 4.4)]),
        -- expr
        ("Expr-test1", "(set! a (+ 1 2))", [("a", DoubleValue 3)])
        ("Expr-stat-list-test", "(begin (set! a 10.0) (set! a 100.0))",
            [("a", DoubleValue 100.0)]),
        ("Expr-let-test", "(begin (set! a 100.0) (set! p (let a (+ 1 2) (+ a 3))))",
            [("p", DoubleValue 6), ("a", DoubleValue 100.0)]),
        ("Expr-lambda-test", "(begin (set! a (lambda x (+ x 1))) (set! p (a 3.0)))",
            [("p", DoubleValue 4.0)]),
        ("Expr-while-test", "(set! p (let a (+ 1 2) (+ a 3)))", [("p", DoubleValue 6)]),
        ("Expr-if-test1", "(set! p (if (= 1 1) (+ 1 2) (+ 1 3)))", [("p", DoubleValue 3)]),
        ("Expr-if-test2", "(set! p (if (< 1 1) (+ 1 2) (+ 1 3)))", [("p", DoubleValue 4)]),
    ] where
        packSpecs [] = []
        packSpecs ((desc, code, table):xs) = (desc, quickCheck $ test code table) : packSpecs xs

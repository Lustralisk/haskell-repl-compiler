{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalSpec where

import Data.Text hiding (all)
import Test.QuickCheck
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Parser
import Printer
import Eval

runCode :: Text -> Env -> Env
runCode code env = env' where
    (_, env') = runState (runExceptT $ eval $ unpack code) env

assertEnv :: Env -> [(String, Value)] -> Bool
assertEnv env [] = True
assertEnv env ((var, value):xs) = case M.lookup (pack var) env of
    Just v -> (v == value) && assertEnv env xs
    Nothing -> (value == Undefined) && assertEnv env xs

test :: String -> [(String, Value)] -> Bool
test code c = assertEnv env' c &&  assertEnv env'' c where
    env' = runCode (pack code) M.empty
    prettyCode = replace "\r\n" " " $ prettyPrint $ pack code
    env'' = runCode prettyCode M.empty


-- triple: (desciption, code, assert table)
evalSpecs = packSpecs [
        ("Expr-test-bool1", "(set! rst (and True False))",
            [("rst", BoolValue False)]),
        ("Expr-test-bool2", "(set! rst (or True False))",
            [("rst", BoolValue True)]),
        ("Expr-test-bool3", "(set! rst (not True))",
            [("rst", BoolValue False)]),
        ("Expr-test-bool4", "(set! rst (and (not True) (not False)))",
            [("rst", BoolValue False)]),
        ("Expr-test-bool5", "(set! rst (and True 1))",
            [("rst", Undefined)]),
        ("Expr-test-bool6", "(begin (set! tmp True) (set! rst (not tmp)))",
            [("rst", BoolValue False)]),
        ("Expr-test-bool7", "(begin (set! tmp 1) (set! rst (not tmp)))",
            [("rst", Undefined)]),
        ("Expr-test-double1", "(set! rst (+ 1 2))",
            [("rst", DoubleValue 3)]),
        ("Expr-test-double2", "(set! rst (- 1 2))",
            [("rst", DoubleValue (-1))]),
        ("Expr-test-double3", "(set! rst (* 1.4 2))",
            [("rst", DoubleValue 2.8)]),
        ("Expr-test-double4", "(set! rst (/ 1 2))",
            [("rst", DoubleValue 0.5)]),
        ("Expr-test-double5", "(set! rst (/ 1 0))",
            [("rst", Undefined)]),
        ("Expr-test-double6", "(set! rst (+ 1 (/ 2 0)))",
            [("rst", Undefined)]),
        ("Expr-test-double7", "(set! rst (* 1 'A')",
            [("rst", Undefined)]),
        ("Expr-test-cmp1", "(set! rst (> 1 0))",
            [("rst", BoolValue True)]),
        ("Expr-test-cmp2", "(set! rst (< 1 0))",
            [("rst", BoolValue False)]),
        ("Expr-test-cmp3", "(set! rst (>= 1 0))",
            [("rst", BoolValue True)]),
        ("Expr-test-cmp4", "(set! rst (<= 1 0))",
            [("rst", BoolValue False)]),
        ("Expr-test-cmp5", "(set! rst (>= 1 1))",
            [("rst", BoolValue True)]),
        ("Expr-test-cmp6", "(set! rst (<= 1 1))",
            [("rst", BoolValue True)]),
        ("Expr-test-cmp7", "(set! rst (= 1 1))",
            [("rst", BoolValue True)]),
        ("Expr-test-cmp8", "(set! rst (= (/ 1 0) (/ 1 0)))",
            [("rst", Undefined)]),
        ("Expr-test-cmp9", "(set! rst (= 0 (- 0 0)))",
            [("rst", BoolValue True)]),
        ("Expr-test-cmp10", "(set! rst (> 1 True))",
            [("rst", Undefined)]),
        ("Stmt-test-while1", "(begin (set! i 0) (while (< i 5) (set! i (+ i 1))))",
            [("i", DoubleValue 5)]),
        ("Stmt-test-while2", "(begin (set! i 5) (while (> i 0) (set! i (- i 1))))",
            [("i", DoubleValue 0)]),
        ("Stmt-test-while3", "(begin (set! i 1) (while (< i 10) (set! i (* i 2))))",
            [("i", DoubleValue 16)]),
        ("Stmt-test-while4", "(begin (set! i 1) (while (< i True) (set! i (* i 2))))",
            [("i", DoubleValue 1)]),
        ("Stmt-test-if1", "(if (< 1 2) (set! x 1) (set! y 2))",
            [("x", DoubleValue 1), ("y", Undefined)]),
        ("Stmt-test-if2", "(if (> 1 2) (set! x 1) (set! y 2))",
            [("x", Undefined), ("y", DoubleValue 2)]),
        ("Stmt-test-if3", "(if (+ 1 2) (set! x 1) (set! y 2))",
            [("x", Undefined), ("y", Undefined)]),
        ("Stmt-test-if4", "(if (< 1 2) skip (set! y 2))",
            [("x", Undefined), ("y", Undefined)]),
        ("Stmt-test-if5", "(if (> 1 2) (set! x 1) skip))",
            [("x", Undefined), ("y", Undefined)]),
        ("Stmt-test-begin", "(begin (set! x 1) (set! y 2) (set! z 3) skip)",
            [("x", DoubleValue 1), ("y", DoubleValue 2), ("z", DoubleValue 3)]),
        ("Stmt-test-vec1", "(make-vector rst 5)",
            [("rst", ListValue [Undefined, Undefined, Undefined, Undefined, Undefined])]),
        ("Stmt-test-vec2", "(make-vector rst 1.2)",
            [("rst", Undefined)]),
        ("Stmt-test-vec3", "(make-vector rst True)",
            [("rst", Undefined)]),
        ("Stmt-test-vec4", "(make-vector rst (- 1 4))",
            [("rst", Undefined)]),
        ("Stmt-test-vec5", "(make-vector rst 0)",
            [("rst", Undefined)]),
        ("Stmt-test-vec6", "(make-vector rst -1)",
            [("rst", Undefined)]),
        ("Stmt-test-vec7", "(begin (make-vector rst 4) (vector-set! rst 0 1))",
            [("rst", ListValue [DoubleValue 1, Undefined, Undefined, Undefined])]),
        ("Stmt-test-vec8", "(begin (make-vector rst 4) (vector-set! rst 3 1))",
            [("rst", ListValue [Undefined, Undefined, Undefined, DoubleValue 1])]),
        ("Stmt-test-vec9", "(begin (make-vector rst 4) (vector-set! rst 1 True))",
            [("rst", ListValue [Undefined, BoolValue True, Undefined, Undefined])]),
        ("Stmt-test-vec10", "(begin (make-vector rst 4) (set! i 0) (while (< i 4) (begin (vector-set! rst i i) (set! i (+ i 1)))))",
            [("rst", ListValue [DoubleValue 0, DoubleValue 1, DoubleValue 2, DoubleValue 3])]),
        ("Stmt-test-vec11", "(begin (make-vector rst 4) (vector-set! rst 5 1))",
            [("rst", ListValue [Undefined, Undefined, Undefined, Undefined])]),
        ("Stmt-test-vec12", "(begin (make-vector tmp 4) (vector-set! tmp 0 1) (set! rst (vector-ref tmp 0)))",
            [("rst", DoubleValue 1)]),
        ("Stmt-test-vec13", "(make-vector rst 1.2)",
            [("rst", Undefined)]),
        ("Func-test-fst1", "(define (foo x) (return x))",
            [("x", Undefined)]),
        ("Expr-test-let1", "(set! rst (let x 1 2))",
            [("x", Undefined), ("rst", DoubleValue 2)]),
        ("Expr-test-let2", "(set! rst (let x 1 (+ 1 x)))",
            [("x", Undefined), ("rst", DoubleValue 2)]),
        ("Expr-test-let3", "(set! rst (let x 1 (and True x)))",
            [("x", DoubleValue 1), ("rst", Undefined)]),
        ("Expr-test-let4", "(set! rst (let y (let x 1 (+ 1 x)) (+ 1 y)))",
            [("x", Undefined), ("y", Undefined), ("rst", DoubleValue 3)]),
        ("Expr-test-let5", "(set! rst (let y 1 (let x 1 (+ x y))))",
            [("x", Undefined), ("y", Undefined), ("rst", DoubleValue 2)]),
        ("Expr-test-high1", "(begin (set! tmp (lambda x (+ 1 x))) (set! rst (tmp 1)))",
            [("rst", DoubleValue 2)]),
        ("Expr-test-high2", "(begin (set! tmp (lambda x (and True x))) (set! rst (tmp False)))",
            [("rst", BoolValue False)]),
        ("Expr-test-high3", "(begin (set! tmp (lambda x (lambda y (+ x y)))) (set! rst ((tmp 1) 2)))",
            [("rst", DoubleValue 3)]),
        ("Expr-test-high4", "(begin (set! tmp (lambda x (lambda y (+ x y)))) (set! rst ((tmp 1) True)))",
            [("rst", Undefined)]),
        ("Expr-test-high5", "(set! rst ((lambda f (f 2)) (lambda x (* 2 x))))",
            [("rst", DoubleValue 4)]),
        ("Expr-test-high6", "(set! rst ((lambda x (* x 2)) ((lambda x (* 2 x)) 2)))",
            [("rst", DoubleValue 8)]),
        ("Expr-test-high7", "(begin (set! x 2) (set! rst ((lambda x (* 2 x)) 2)))",
            [("rst", DoubleValue 4), ("x", DoubleValue 2)])
    ] where
        packSpecs [] = []
        packSpecs ((desc, code, table):xs) = (desc, quickCheck $ test code table) : packSpecs xs

{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalSpec (
        evalTests,
        evalSpecs
    ) where

import Test.QuickCheck
import qualified Data.Map as M
import Control.Applicative
import Parser
import EvalT

uniOpers = [Not]
binOpers = [And, Or]
opers e1 e2 = [Not e1, And e1 e2, Or e1 e2]

genBoolLit :: Gen Expr
genBoolLit =  elements [BoolLit True, BoolLit False]

genInt :: Gen Expr
genInt = Number . toEnum <$> (choose (0, 100) :: Gen Int)

genDouble :: Gen Expr
genDouble = Number <$> choose (0, 100.0)

genBooleanExpr :: Gen Expr
genBooleanExpr = sized genN where
    genN 0 = genBoolLit
    genN n = do
        e1 <- genN (min 10 n - 1)
        e2 <- genN (min 10 n - 1)
        elements [Not e1, And e1 e2, Or e1 e2]

genNumberExpr :: Gen Expr
genNumberExpr = sized genN where
    genN 0 = oneof [genInt, genDouble]
    genN n = do
        e1 <- genN (min 6 n - 1)
        e2 <- genN (min 6 n - 1)
        elements [Add e1 e2, Sub e1 e2, Mul e1 e2, Div e1 e2]

prop_resultIsBoolValue :: Property
prop_resultIsBoolValue = forAll genBooleanExpr $ \x -> case runEvalExpr $ show x of
    (Right (BoolValue _)) -> True
    _ -> False

prop_resultIsFalse :: Property
prop_resultIsFalse = forAll genBooleanExpr $ \x -> case runEvalExpr $ "(and False " ++ show x ++ ")" of
    (Right (BoolValue False)) -> True
    _ -> False

prop_resultIsTrue :: Property
prop_resultIsTrue = forAll genBooleanExpr $ \x -> case runEvalExpr $ "(or True " ++ show x ++ ")" of
    (Right (BoolValue True)) -> True
    _ -> False

prop_resultIsDoubleValue :: Property
prop_resultIsDoubleValue = forAll genNumberExpr $ \x -> case runEvalExpr $ show x of
    (Right (DoubleValue _)) -> True
    (Left (DividedByZeroError _)) -> True
    _ -> False

evalTests = [("Boolean expression return Bool", quickCheck prop_resultIsBoolValue),
            ("Number expression return Number", quickCheck prop_resultIsDoubleValue),
            ("And False Expr is False", quickCheck prop_resultIsFalse),
            ("Or True Expr is True", quickCheck prop_resultIsTrue)]

{- specific example -}

prop_ifStmt1 = undefined
prop_recursiveFunc = evalStatement "(begin (define (foo x) (if (> x 0) (begin (return (* x (foo (- x 1))))) (return 1))) (return (foo 5)))"

-- evalSpecs = [("1", quickCheck prop_recursiveFunc)]

{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalTest where

import Test.QuickCheck
import qualified Data.Map as M
import Control.Applicative
import Parser
import Eval
import Specs.ParserTest

evalTests = [("Boolean expression return Bool", quickCheck prop_resultIsBoolValue),
            ("Number expression return Number", quickCheck prop_resultIsDoubleValue),
            ("And False Expr is False", quickCheck prop_resultIsFalse),
            ("Or True Expr is True", quickCheck prop_resultIsTrue),
            ("Double Check", quickCheck prop_doubleCheck)]

prop_resultIsBoolValue :: Property
prop_resultIsBoolValue = forAll (genBooleanExpr 10) $ \x -> case runEvalExpr $ show x of
    (Right (BoolValue _)) -> True
    _ -> False

prop_resultIsFalse :: Property
prop_resultIsFalse = forAll (genBooleanExpr 10) $ \x -> case runEvalExpr $ "(and False " ++ show x ++ ")" of
    (Right (BoolValue False)) -> True
    _ -> False

prop_resultIsTrue :: Property
prop_resultIsTrue = forAll (genBooleanExpr 10) $ \x -> case runEvalExpr $ "(or True " ++ show x ++ ")" of
    (Right (BoolValue True)) -> True
    _ -> False

prop_resultIsDoubleValue :: Property
prop_resultIsDoubleValue = forAll (genNumberExpr 8) $ \x -> case runEvalExpr $ show x of
    (Right (DoubleValue _)) -> True
    (Left (DividedByZeroError _)) -> True
    _ -> False

prop_doubleCheck :: Property
prop_doubleCheck = forAll (genStatement 200) $ \x -> case runEvalExpr $ show x of
    (Right _) -> True
    (Left _) -> True
    _ -> False

{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import Parser

data Value = BoolValue Bool
           | CharValue Char
           | DoubleValue Double
           | ListValue [Value]

{- Divide eval -}
evalParser :: Expr -> Value
evalParser (Number e) = DoubleValue e
evalParser FalseLit = BoolValue False
evalParser TrueLit = BoolValue True
evalParser (Add e1 e2) = DoubleValue (n1 + n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Sub e1 e2) = DoubleValue (n1 - n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Mul e1 e2) = DoubleValue (n1 * n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Div e1 e2) = DoubleValue (n1 / n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2

evalParser (And e1 e2) = BoolValue (n1 && n2)
    where (BoolValue n1) = evalParser e1
          (BoolValue n2) = evalParser e2
evalParser (Or e1 e2) = BoolValue (n1 || n2)
    where (BoolValue n1) = evalParser e1
          (BoolValue n2) = evalParser e2
evalParser (Not e) = BoolValue (not n)
    where (BoolValue n) = evalParser e
evalParser (Eq e1 e2) = BoolValue (n1 == n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Lw e1 e2) = BoolValue (n1 < n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Le e1 e2) = BoolValue (n1 <= n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Gr e1 e2) = BoolValue (n1 > n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2
evalParser (Ge e1 e2) = BoolValue (n1 >= n2)
    where (DoubleValue n1) = evalParser e1
          (DoubleValue n2) = evalParser e2



eval :: Text -> Value
eval t = let (Right expr) = (parseOnly exprParser t) in evalParser expr

printEval :: Value -> String
printEval (BoolValue a) = show a
printEval (DoubleValue a) = show a
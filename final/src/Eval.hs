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
evalParser (CharLit e) = CharValue e
evalParser FalseLit = BoolValue False
evalParser TrueLit = BoolValue True
evalParser (Add e1 e2) = DoubleValue (v1 + v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Sub e1 e2) = DoubleValue (v1 - v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Mul e1 e2) = DoubleValue (v1 * v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Div e1 e2) = DoubleValue (v1 / v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2

evalParser (And e1 e2) = BoolValue (v1 && v2)
    where (BoolValue v1) = evalParser e1
          (BoolValue v2) = evalParser e2
evalParser (Or e1 e2) = BoolValue (v1 || v2)
    where (BoolValue v1) = evalParser e1
          (BoolValue v2) = evalParser e2
evalParser (Not e) = BoolValue (not n)
    where (BoolValue n) = evalParser e
evalParser (Eq e1 e2) = BoolValue (v1 == v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Lw e1 e2) = BoolValue (v1 < v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Le e1 e2) = BoolValue (v1 <= v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Gr e1 e2) = BoolValue (v1 > v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2
evalParser (Ge e1 e2) = BoolValue (v1 >= v2)
    where (DoubleValue v1) = evalParser e1
          (DoubleValue v2) = evalParser e2

evalParser Nil = ListValue []
evalParser (Cons e1 e2) = ListValue (v1:v2)
    where v1 = evalParser e1
          (ListValue v2) = evalParser e2
{- Needs error detection -}
evalParser (Car (Cons e _)) = v
    where v = evalParser e
evalParser (Cdr (Cons _ e)) = v
    where v = evalParser e



eval :: Text -> Value
eval t = let (Right expr) = (parseOnly exprParser t) in evalParser expr

printEval :: Value -> String
printEval (BoolValue b) = show b
printEval (DoubleValue d) = show d
printEval (CharValue c) = show c
printEval (ListValue l) = Prelude.concat [printEval li ++ ", " | li <- l]
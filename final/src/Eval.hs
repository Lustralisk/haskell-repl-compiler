{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import Parser

data Value = B Bool
           | C Char
           | D Double
           | L [Value]

{- Divide eval -}
evalParser :: Expr -> Either Bool Double
evalParser (Number e) = Right e
evalParser FalseLit = Left False
evalParser TrueLit = Left True
evalParser (Add e1 e2) = Right (n1 + n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Sub e1 e2) = Right (n1 - n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Mul e1 e2) = Right (n1 * n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Div e1 e2) = Right (n1 / n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2

evalParser (And e1 e2) = Left (n1 && n2)
    where (Left n1) = evalParser e1
          (Left n2) = evalParser e2
evalParser (Or e1 e2) = Left (n1 || n2)
    where (Left n1) = evalParser e1
          (Left n2) = evalParser e2
evalParser (Not e) = Left (not n)
    where (Left n) = evalParser e
evalParser (Eq e1 e2) = Left (n1 == n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Lw e1 e2) = Left (n1 < n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Le e1 e2) = Left (n1 <= n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Gr e1 e2) = Left (n1 > n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2
evalParser (Ge e1 e2) = Left (n1 >= n2)
    where (Right n1) = evalParser e1
          (Right n2) = evalParser e2



eval :: Text -> Either Bool Double
eval t = let (Right expr) = (parseOnly exprParser t) in evalParser expr

printEval :: Either Bool Double -> String
printEval (Left a) = show a
printEval (Right a) = show a
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import Parser

data Value = BoolValue Bool
           | CharValue Char
           | DoubleValue Double
           | ListValue [Value]

type Env = M.Map Text Value

evalExprParser :: Env -> Expr -> Value
evalExprParser env (Variable e) = case M.lookup e env of
    Just v -> v {- Nothing -> throw error -}
evalExprParser _ (Number e) = DoubleValue e
evalExprParser _ (CharLit e) = CharValue e
evalExprParser _ FalseLit = BoolValue False
evalExprParser _ TrueLit = BoolValue True
evalExprParser env (Add e1 e2) = DoubleValue (v1 + v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Sub e1 e2) = DoubleValue (v1 - v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Mul e1 e2) = DoubleValue (v1 * v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Div e1 e2) = DoubleValue (v1 / v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2

evalExprParser env (And e1 e2) = BoolValue (v1 && v2)
    where (BoolValue v1) = evalExprParser env e1
          (BoolValue v2) = evalExprParser env e2
evalExprParser env (Or e1 e2) = BoolValue (v1 || v2)
    where (BoolValue v1) = evalExprParser env e1
          (BoolValue v2) = evalExprParser env e2
evalExprParser env (Not e) = BoolValue (not n)
    where (BoolValue n) = evalExprParser env e
evalExprParser env (Eq e1 e2) = BoolValue (v1 == v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Lw e1 e2) = BoolValue (v1 < v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Le e1 e2) = BoolValue (v1 <= v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Gr e1 e2) = BoolValue (v1 > v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
evalExprParser env (Ge e1 e2) = BoolValue (v1 >= v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2

evalExprParser _ Nil = ListValue []
evalExprParser env (Cons e1 e2) = ListValue (v1:v2)
    where v1 = evalExprParser env e1
          (ListValue v2) = evalExprParser env e2
{- Needs error detection -}
evalExprParser env (Car (Cons e _)) = v
    where v = evalExprParser env e
evalExprParser env (Cdr (Cons _ e)) = v
    where v = evalExprParser env e

evalExpr :: Env -> Text -> Value
evalExpr env t = let (Right expr) = (parseOnly exprParser t) in evalExprParser env expr

printEvalExpr :: Value -> String
printEvalExpr (BoolValue b) = show b
printEvalExpr (DoubleValue d) = show d
printEvalExpr (CharValue c) = show c
printEvalExpr (ListValue l) = Prelude.concat [printEvalExpr li ++ ", " | li <- l]

evalStatementParser :: Env -> Statement -> Env
evalStatementParser env (StatementList s) = case s of
    [] -> env
    (x:xs) -> evalStatementParser env' (StatementList xs) where
        env' = evalStatementParser env x
evalStatementParser env (Set t e) = (M.insert t (evalExprParser env e) env)
evalStatementParser env Skip = env
evalStatementParser env (If e s1 s2) = case evalExprParser env e of
    BoolValue True -> evalStatementParser env s1
    BoolValue False -> evalStatementParser env s2
evalStatementParser env (While e s) = case evalExprParser env e of
    BoolValue True -> do
        evalStatementParser env' (While e s) where
            env' = evalStatementParser env s
    BoolValue False -> env

evalStatement :: Env -> [Char] -> Env
evalStatement env line = evalStatementParser env statement where
    (Right statement) = parseOnly statementParser $ pack line

toText text = pack text
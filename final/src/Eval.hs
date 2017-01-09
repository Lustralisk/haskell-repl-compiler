{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import Parser
import Debug.Trace

data Value = BoolValue Bool
           | CharValue Char
           | DoubleValue Double
           | FunctionValue [Text] Statement Env
           | ListValue [Value]
           deriving Show

type Env = M.Map Text Value

{- Useful util -}
isInt :: Value -> Bool
isInt (DoubleValue d) = ((toEnum (fromEnum d)) :: Double) == d
isInt _ = False

updateM :: Text -> Value -> Env -> Env
updateM t v env = M.insert t v (M.delete t env)

inject :: Env -> Env -> [Text] -> [Expr] -> Env
inject env' env ts es = case ts of
    [] -> env'
    (x:xs) -> inject (updateM x (evalExprParser env y) env') env xs ys where
        (y:ys) = es

-------------------------------------------------------------------------------
--- evalExprParser
--- Calculate Value of Expr based on given Env
-------------------------------------------------------------------------------
evalExprParser :: Env -> Expr -> Value
{- Need to due with Nothing -}
evalExprParser env (Variable t) = case M.lookup t env of
    Just v -> v
evalExprParser env (Vec t e) = case M.lookup t env of
    Just v -> v
evalExprParser _ (Number e) = DoubleValue e
evalExprParser _ (CharLit e) = CharValue e
evalExprParser _ FalseLit = BoolValue False
evalExprParser _ TrueLit = BoolValue True
evalExprParser env (Function t es) = case M.lookup t env of
    Just (FunctionValue ts stat env') -> case M.lookup "$$result$$" env'' of
        Just v -> v
        where env'' = evalStatementParser (inject env' env ts es) stat
evalExprParser env (Let t e1 e2) = evalExprParser env' e2 where
    env' = updateM t (evalExprParser env e1) env
evalExprParser env (LambdaCall (Lambda t e1) e2) = evalExprParser env (Let t e2 e1)
{- Need to check whether e1, e2 are Double -}
{- Need to due with Inf -}
evalExprParser env (Add e1 e2) = DoubleValue (v1 + v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
evalExprParser env (Sub e1 e2) = DoubleValue (v1 - v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
{- Need to due with Inf -}
evalExprParser env (Mul e1 e2) = DoubleValue (v1 * v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
{- Need to due with divid by zero -}
evalExprParser env (Div e1 e2) = DoubleValue (v1 / v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2

{- Need to check whether e1, e2 are Bool -}
evalExprParser env (And e1 e2) = BoolValue (v1 && v2)
    where (BoolValue v1) = evalExprParser env e1
          (BoolValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Bool -}
evalExprParser env (Or e1 e2) = BoolValue (v1 || v2)
    where (BoolValue v1) = evalExprParser env e1
          (BoolValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Bool -}
evalExprParser env (Not e) = BoolValue (not n)
    where (BoolValue n) = evalExprParser env e
{- Need to check whether e1, e2 are Double -}
evalExprParser env (Eq e1 e2) = BoolValue (v1 == v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
evalExprParser env (Lw e1 e2) = BoolValue (v1 < v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
evalExprParser env (Le e1 e2) = BoolValue (v1 <= v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
evalExprParser env (Gr e1 e2) = BoolValue (v1 > v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2
{- Need to check whether e1, e2 are Double -}
evalExprParser env (Ge e1 e2) = BoolValue (v1 >= v2)
    where (DoubleValue v1) = evalExprParser env e1
          (DoubleValue v2) = evalExprParser env e2

evalExprParser _ Nil = ListValue []
evalExprParser env (Cons e1 e2) = ListValue (v1:v2)
    where v1 = evalExprParser env e1
          (ListValue v2) = evalExprParser env e2
{- Need error detection -}
evalExprParser env (Car (Cons e _)) = v
    where v = evalExprParser env e
evalExprParser env (Cdr (Cons _ e)) = v
    where v = evalExprParser env e

-------------------------------------------------------------------------------
--- evalStatementParser
--- Execute Statement based on given Env and may effect on it,
---     resulting in return Env
-------------------------------------------------------------------------------
evalStatementParser :: Env -> Statement -> Env
evalStatementParser env (StatementList s) = case s of
    [] -> env
    (x:xs) -> case x of
        (Return e) -> updateM "$$result$$" (evalExprParser env e) env
        _ -> evalStatementParser env' (StatementList xs) where
            env' = evalStatementParser env x
evalStatementParser env (Set t e) = (updateM t (evalExprParser env e) env)
evalStatementParser env Skip = env
{- Need to check whether e is Bool -}
evalStatementParser env (If e s1 s2) = case evalExprParser env e of
    BoolValue True -> evalStatementParser env s1
    BoolValue False -> evalStatementParser env s2
{- Need to check whether e is Bool -}
evalStatementParser env (While e s) = case evalExprParser env e of
    BoolValue True -> case M.lookup "$$result$$" env of
        Just v -> env
        Nothing -> evalStatementParser env' (While e s) where
            env' = evalStatementParser env s
    BoolValue False -> env
{- Need to check whether e is Int using isInt at L19 -}
evalStatementParser env (MakeVector t e) = (updateM t (ListValue [(DoubleValue 0) | i <- [1..length]]) env) where
    (DoubleValue length') = evalExprParser env e
    length = fromEnum length'
{- Need to check whether e1, e2 are Int using isInt at L19 -}
evalStatementParser env (SetVector t e1 e2) = case M.lookup t env of
    Just (ListValue v) -> updateM t (ListValue ((Prelude.take i v) ++ [evalExprParser env e2] ++ (Prelude.drop (i + 1) v))) env where
        (DoubleValue i') = evalExprParser env e1
        i = fromEnum i'
evalStatementParser env (Return e) = updateM "$$result$$" (evalExprParser env e) env

-------------------------------------------------------------------------------
--- evalFunctionParser
-------------------------------------------------------------------------------
evalFunctionParser :: Env -> Function -> Env
evalFunctionParser env (Def t ts stat) = do
    evalStatementParser env' stat where
        env' = updateM t (FunctionValue ts stat env') env

evalExpr :: Env -> Text -> Value
evalExpr env t = let (Right expr) = (parseOnly exprParser t) in evalExprParser env expr

{- Need to somewhat refine -}
printEvalExpr :: Value -> String
printEvalExpr (BoolValue b) = show b
printEvalExpr (DoubleValue d) = show d
printEvalExpr (CharValue c) = show c
{- Need to refine -}
printEvalExpr (ListValue l) = Prelude.concat [printEvalExpr li ++ ", " | li <- l]

evalStatement :: Env -> [Char] -> Env
evalStatement env line = evalStatementParser env statement where
    (Right statement) = parseOnly statementParser $ pack line

evalFunction :: Env -> [Char] -> Env
evalFunction env line = evalFunctionParser env statement where
    (Right statement) = parseOnly functionParser $ pack line

toText text = pack text

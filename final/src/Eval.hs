{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
-- Note: followings not in dependencies
import Control.Monad.Except
import Control.Applicative
import Parser

data Value = BoolValue Bool
           | CharValue Char
           | DoubleValue Double
           | FunctionValue [Text] Statement Env
           | ListValue [Value]
           deriving Show

type Env = M.Map Text Value

{- Error Check -}

-- Error Enum
data EvalError = TypeError String
               | NotFoundError String
               | OtherError String

instance Show EvalError where
    show (TypeError msg) = "TypeError: " ++ msg
    show (NotFoundError msg) = "NotFoundError: " ++ msg
    show (OtherError msg) = msg

type EvalMonad = Either EvalError

-- Type Enum
data ValueType = BoolType | CharType | DoubleType | FunctionType | ListType

-- Error Util
---- Note: could simplify by (<|>)?
checkDoubleValueType :: EvalMonad Value -> EvalMonad Value
checkDoubleValueType (Right (DoubleValue v)) = return (DoubleValue v)
checkDoubleValueType (Right value) = throwError (TypeError (show value))
checkDoubleValueType (Left err) = throwError err

checkBoolValueType :: EvalMonad Value -> EvalMonad Value
checkBoolValueType (Right (BoolValue v)) = return (BoolValue v)
checkBoolValueType (Right value) = throwError (TypeError (show value))
checkBoolValueType (Left err) = throwError err

checkCharValueType :: EvalMonad Value -> EvalMonad Value
checkCharValueType (Right (CharValue v)) = return (CharValue v)
checkCharValueType (Right value) = throwError (TypeError (show value))
checkCharValueType (Left err) = throwError err

checkListValueType :: EvalMonad Value -> EvalMonad Value
checkListValueType (Right (ListValue v)) = case v of
    [] -> return (ListValue v)
    [x] -> return (ListValue v)
    -- todo: type conflict
    (x:xs) -> return (ListValue v)
checkListValueType (Right value) = throwError (TypeError (show value))
checkListValueType (Left err) = throwError err

checkValueType :: ValueType -> EvalMonad Value -> EvalMonad Value
checkValueType DoubleType = checkDoubleValueType
checkValueType BoolType = checkBoolValueType
checkValueType CharType = checkCharValueType
checkValueType ListType = checkListValueType

handleExprError :: Expr -> EvalError -> EvalMonad Value
handleExprError expr (TypeError msg) = Left (TypeError (msg ++ "\nIn the expression:\n    " ++ show expr))
handleExprError _ err = Left err

{- Useful util -}
toText text = pack text
splitLn :: Text -> [[Char]]
splitLn = (Prelude.map unpack) . (splitOn "\r\n")

charCount :: Char -> [Char] -> Int
charCount c s = Data.Text.count (pack [c]) (pack s)

newCount :: Int -> [Char] -> Int
newCount i s = i + (charCount '(' s) - (charCount ')' s)

isInt :: Value -> Bool
isInt (DoubleValue d) = ((toEnum (fromEnum d)) :: Double) == d
isInt _ = False

updateM :: Text -> EvalMonad Value -> Env -> Env
updateM t (Right v) env = M.insert t v (M.delete t env)
updateM t (Left _) env = env

inject :: Env -> Env -> [Text] -> [Expr] -> Env
inject env' env ts es = case ts of
    [] -> env'
    (x:xs) -> inject (updateM x (evalExprParser env y) env') env xs ys where
        (y:ys) = es

-------------------------------------------------------------------------------
--- evalExprParser
--- Calculate Value of Expr based on given Env
-------------------------------------------------------------------------------
evalExprParser :: Env -> Expr -> EvalMonad Value
{- Need to due with Nothing -}
evalExprParser env (Variable t) = case M.lookup t env of
    Nothing -> Left (NotFoundError "No such variable")
    Just v -> Right v
evalExprParser env (Vec t e) = case M.lookup t env of
    Nothing -> Left (NotFoundError "No such vector")
    Just v -> Right v
evalExprParser _ (Number e) = Right (DoubleValue e)
evalExprParser _ (CharLit e) = Right (CharValue e)
evalExprParser _ FalseLit = Right (BoolValue False)
evalExprParser _ TrueLit = Right (BoolValue True)
evalExprParser env (Function t es) = case M.lookup t env of
    Just (FunctionValue ts stat env') -> case M.lookup "$$result$$" env'' of
        Nothing -> Left (NotFoundError "No such function")
        Just v -> Right v
        where env'' = evalStatementParser (inject env' env ts es) stat
evalExprParser env (Let t e1 e2) = evalExprParser env' e2 where
    env' = updateM t (evalExprParser env e1) env
{- Not finished yet -}
{-evalExprParser env (Lambda t e) = (FunctionValue "$$x$$" (Return e) env)
evalExprParser env (LambdaCall e1 e2) = evalExprParser env (Let t e2 e3)
    where (Lambda t e3) = evalExprParser env e1-}
{- Need to check whether e1, e2 are Double -}
{- Need to due with Inf -}

-- !!! Abandon:
-- evalExprParser env (Add e1 e2) = case (evalExprParser env e1, evalExprParser env e2) of
--     (Right (DoubleValue v1), Right (DoubleValue v2)) -> Right (DoubleValue (v1 + v2))
--     (Right (DoubleValue v1), Right _) -> Left "Add: right operand type error"
--     (Right _, Right (DoubleValue v2)) -> Left "Add: left operand type error"
--     (Right _, Right _) -> Left "Add: both operands type error"
--     (Left err, _) -> Left err
--     (_, Left err) -> Left err

evalExprParser env expr@(Add e1 e2) = (do
        (DoubleValue v1) <- checkDoubleValueType (evalExprParser env e1)
        (DoubleValue v2) <- checkDoubleValueType (evalExprParser env e2)
        return (DoubleValue (v1 + v2))
    ) `catchError` handleExprError expr

evalExprParser env expr@(Sub e1 e2) = (do
        (DoubleValue v1) <- checkDoubleValueType (evalExprParser env e1)
        (DoubleValue v2) <- checkDoubleValueType (evalExprParser env e2)
        return (DoubleValue (v1 - v2))
    ) `catchError` handleExprError expr

{- Need to due with Inf -}
evalExprParser env expr@(Mul e1 e2) = (do
        (DoubleValue v1) <- checkDoubleValueType (evalExprParser env e1)
        (DoubleValue v2) <- checkDoubleValueType (evalExprParser env e2)
        return (DoubleValue (v1 * v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Double -}
{- Need to due with divid by zero -}
evalExprParser env expr@(Div e1 e2) = (do
        (DoubleValue v1) <- checkDoubleValueType (evalExprParser env e1)
        (DoubleValue v2) <- checkDoubleValueType (evalExprParser env e2)
        return (DoubleValue (v1 / v2))
    ) `catchError` handleExprError expr

{- Need to check whether e1, e2 are Bool -}
evalExprParser env expr@(And e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 && v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Bool -}
evalExprParser env expr@(Or e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 || v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Bool -}
evalExprParser env expr@(Not e) = (do
        (BoolValue v) <- checkBoolValueType (evalExprParser env e)
        return (BoolValue (not v))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Double -}
evalExprParser env expr@(Eq e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 == v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Double -}
evalExprParser env expr@(Lw e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 < v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Double -}
evalExprParser env expr@(Le e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 <= v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Double -}
evalExprParser env expr@(Gr e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 > v2))
    ) `catchError` handleExprError expr
{- Need to check whether e1, e2 are Double -}
evalExprParser env expr@(Ge e1 e2) = (do
        (BoolValue v1) <- checkBoolValueType (evalExprParser env e1)
        (BoolValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (BoolValue (v1 >= v2))
    ) `catchError` handleExprError expr

evalExprParser _ Nil = Right (ListValue [])
evalExprParser env expr@(Cons e1 e2) = (do
        v1 <- checkBoolValueType (evalExprParser env e1)
        (ListValue v2) <- checkBoolValueType (evalExprParser env e2)
        return (ListValue (v1:v2))
    ) `catchError` handleExprError expr
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
evalStatementParser env (Set t e) = updateM t (evalExprParser env e) env
evalStatementParser env Skip = env
{- Need to check whether e is Bool -}
evalStatementParser env (If e s1 s2) = case evalExprParser env e of
    Right (BoolValue True) -> evalStatementParser env s1
    Right (BoolValue False) -> evalStatementParser env s2
{- Need to check whether e is Bool -}
evalStatementParser env (While e s) = case evalExprParser env e of
    Right (BoolValue True) -> case M.lookup "$$result$$" env of
        Just v -> env
        Nothing -> evalStatementParser env' (While e s) where
            env' = evalStatementParser env s
    Right (BoolValue False) -> env
{- Need to check whether e is Int using isInt at L19 -}
evalStatementParser env (MakeVector t e) = updateM t (Right (ListValue [DoubleValue 0 | i <- [1..length]])) env where
    Right (DoubleValue length') = evalExprParser env e
    length = fromEnum length'
{- Need to check whether e1, e2 are Int using isInt at L19 -}
evalStatementParser env (SetVector t e1 e2) = case M.lookup t env of
    Just (ListValue v) -> updateM t (Right (ListValue (Prelude.take i v ++ [v'] ++ Prelude.drop (i + 1) v))) env where
        Right (DoubleValue i') = evalExprParser env e1
        Right v' = evalExprParser env e2
        i = fromEnum i'
evalStatementParser env (Return e) = updateM "$$result$$" (evalExprParser env e) env

-------------------------------------------------------------------------------
--- evalFunctionParser
-------------------------------------------------------------------------------
evalFunctionParser :: Env -> Function -> Env
evalFunctionParser env (Def t ts stat) = env' where
        env' = updateM t (Right (FunctionValue ts stat env')) env


-- All needs error detection
evalExpr :: Env -> [Char] -> EvalMonad Value
evalExpr env t = let (Right expr) = (parseOnly exprParser (pack t)) in evalExprParser env expr

evalStatement :: Env -> [Char] -> Env
evalStatement env line = evalStatementParser env statement where
    (Right statement) = parseOnly statementParser $ pack line

evalFunction :: Env -> [Char] -> Env
evalFunction env line = evalFunctionParser env function where
    (Right function) = parseOnly functionParser $ pack line

eval :: Env -> [Char] -> (Env, [Char])
eval env line = case parseOnly functionParser $ pack line of
    (Right function) -> (evalFunctionParser env function, "")
    _ -> case parseOnly statementParser $ pack line of
        (Right statement) -> (evalStatementParser env statement, "")
        _ -> (env, printEvalExpr $ evalExpr env line)

{- Need to somewhat refine -}
printEvalExpr :: EvalMonad Value -> String
printEvalExpr (Right (BoolValue b)) = show b
printEvalExpr (Right (DoubleValue d)) = show d
printEvalExpr (Right (CharValue c)) = show c
{- Need to refine -}
printEvalExpr (Right (ListValue l)) = Prelude.concat [printEvalExpr (Right li) ++ ", " | li <- l]
printEvalExpr (Left err) = show err

{-# LANGUAGE OverloadedStrings #-}

module EvalT where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import Debug.Trace
-- Note: followings not in dependencies
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Parser

data Value = BoolValue Bool
           | CharValue Char
           | DoubleValue Double
           | FunctionValue [Text] Statement Env
           | ListValue [Value]
           | Undefined
           deriving Show

type Env = M.Map Text Value

data Errors = ParseError Text
           | TypeError Value Expr
           | ConditionError Value Expr
           | NotFoundError Text Expr
           | ValueError Value Expr
           | FuncTypeError Text Expr
           | DividedByZeroError Expr
           | OutOfIndexError Value Expr
           | OtherError Text

instance Show Errors where
    show (ParseError msg) = "ParseError: " ++ unpack msg
    show (TypeError value expr) = "TypeError: " ++ show value ++ "\nIn the expression: " ++ show expr
    show (NotFoundError var expr) = "NotFoundError: " ++ unpack var ++ "\nIn the expression: " ++ show expr
    show (ValueError value expr) = "ValueError: " ++ show value ++ "\nIn the expression: " ++ show expr
    show (FuncTypeError t expr) = "FunctionTypeError: " ++ unpack t ++ "\nIn the expression: " ++ show expr
    show (DividedByZeroError expr) = "DividedByZeroError: " ++ show expr
    show (OutOfIndexError value expr) = "OutOfIndexError: " ++ show value ++ "\nIn the expression: " ++ show expr
    show (OtherError msg) = unpack msg

-- Suppose State Env Value :: Env -> (Value, Env) is a state monad
--   and Either Error Value is an either monad
--   the transformer should convert state monad to an either monad
--   *seems that EitherT is called ErrorT in Haskell*
type Eval a = ExceptT Errors (State Env) a

insert :: Text -> Value -> Eval ()
insert t v = state $ \env -> ((), M.insert t v env)

delete :: Text -> Eval ()
delete t = state $ \env -> ((), M.delete t env)

search :: Text -> Eval Value
search t = state $ \env -> case M.lookup t env of
    Nothing -> (Undefined, env)
    Just v -> (v, env)

inject :: Env -> Text -> Expr -> [Text] -> [Expr] -> Eval ()
inject env t e ts es = case (ts, es) of
    ([], []) -> return ()
    (x:xs, []) -> throwError $ FuncTypeError t e
    ([], y:ys) -> throwError $ FuncTypeError t e
    (x:xs, y:ys) -> do
        fenv <- get
        put env
        v <- evalExprParser y
        put fenv
        insert x v
        inject env t e xs ys

updateM :: Text -> [Text] -> Statement -> Env -> Env
updateM t ts stmt env = env' where
    env' = M.insert t (FunctionValue ts stmt env') env

isInt :: Double -> Bool
isInt d = ((toEnum (fromEnum d)) :: Double) == d

toText :: String -> Text
toText = pack

splitLn :: Text -> [String]
splitLn = Prelude.map unpack . splitOn "\r\n"

charCount :: Char -> String -> Int
charCount c s = Data.Text.count (pack [c]) (pack s)

newCount :: Int -> String -> Int
newCount i s = i + charCount '(' s - charCount ')' s

-- runEval env ev = runState (runExceptT ev) env

evalExprParser :: Expr -> Eval Value
evalExprParser expr@(Variable t) = do
    v <- search t
    case v of
        Undefined -> throwError $ NotFoundError t expr
        _ -> return v
evalExprParser expr@(Vec t e) = do
    v <- search t
    case v of
        Undefined -> throwError $ NotFoundError t expr
        _ -> return v
evalExprParser expr@(Function t es) = do
    v <- search t
    case v of
        (FunctionValue ts stmt fenv) -> do
            env <- get
            xxx <- search "x"
            put fenv
            inject env t expr ts es
            evalStatementParser stmt
            v' <- search "$$result$$"
            put env
            case v' of
                Undefined -> throwError $ NotFoundError ("Variable " `append` t) expr
                _ -> return v'
        _ -> throwError $ NotFoundError ("Function " `append` t) expr
evalExprParser (Let t e1 e2) = do
    env <- get
    v1 <- evalExprParser e1
    insert t v1
    v2 <- evalExprParser e2
    put env
    return v2
evalExprParser (Lambda t e) = do
    env <- get
    return $ FunctionValue [t] (Return e) env
evalExprParser (Number n) = return $ DoubleValue n
evalExprParser TrueLit = return $ BoolValue True
evalExprParser expr@(Add e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 + v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Sub e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 - v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Lw e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 < v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Gr e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 > v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Div e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue _, DoubleValue 0) -> throwError $ DividedByZeroError expr
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 / v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr

evalStatementParser :: Statement -> Eval ()
evalStatementParser (StatementList []) = return ()
evalStatementParser (StatementList (x:xs)) = case x of
    (Return e) -> evalStatementParser x
    _ -> do
        evalStatementParser x
        evalStatementParser (StatementList xs)
evalStatementParser (Set t e) = do
    v <- evalExprParser e
    insert t v
evalStatementParser Skip = return ()
evalStatementParser (If e s1 s2) = do
    v <- evalExprParser e
    case v of
        (BoolValue True) -> evalStatementParser s1
        (BoolValue False) -> evalStatementParser s2
        _ -> throwError $ ConditionError v e
evalStatementParser stmt@(While e s) = do
    v <- evalExprParser e
    case v of
        (BoolValue True) -> do
            v <- search "$$result$$"
            case v of
                Undefined -> do
                    evalStatementParser s
                    evalStatementParser stmt
                _ -> return ()
        (BoolValue False) -> return ()
evalStatementParser (MakeVector t e) = do
    (DoubleValue v) <- evalExprParser e
    let len = fromEnum v in insert t (ListValue [DoubleValue 0 | i <- [1..len]])
evalStatementParser stmt@(SetVector t e1 e2) = do
    v <- search t
    case v of
        (ListValue v) -> do
            value <- evalExprParser e1
            case value of
                (DoubleValue i') -> do
                    v' <- evalExprParser e2
                    if isInt i'
                    then let i = fromEnum i' in insert t (ListValue (Prelude.take i v ++ [v'] ++ Prelude.drop (i + 1) v))
                    else throwError $ ValueError v' e2
                _ -> throwError $ TypeError value e1
        _ -> throwError $ NotFoundError t (Variable t)
evalStatementParser (Return e) = do
    v <- evalExprParser e
    insert "$$result$$" v


evalFunctionParser :: Function -> Eval ()
evalFunctionParser (Def t ts stmt) = do
    env <- get
    put $ updateM t ts stmt env

evalExpr :: String -> Eval Value
evalExpr t = case parseOnly exprParser $ pack t of
    (Right expr) -> evalExprParser expr
    (Left msg) -> throwError $ ParseError $ pack msg

evalStatement :: String -> Eval ()
evalStatement line = case parseOnly statementParser $ pack line of
    (Right stmt) -> evalStatementParser stmt
    (Left msg) -> throwError $ ParseError $ pack msg

evalFunction :: String -> Eval ()
evalFunction line = case parseOnly functionParser $ pack line of
    (Right function) -> evalFunctionParser function
    (Left msg) -> throwError $ ParseError $ pack msg


eval :: String -> Eval String
eval line = case parseOnly functionParser $ pack line of
    (Right function) -> (\() -> "") `liftM` evalFunctionParser function
    _ -> case parseOnly statementParser $ pack line of
        (Right statement) -> (\() -> "") `liftM` evalStatementParser statement
        _ -> printEvalExpr $ evalExpr line

runEval line env = runState (runExceptT $ eval line) env

printEvalExpr :: Eval Value -> Eval String
printEvalExpr = liftM showEvalExpr where
    showEvalExpr (BoolValue b) = show b
    showEvalExpr (DoubleValue d) = show d
    showEvalExpr (CharValue c) = show c
    showEvalExpr (FunctionValue [t] s e) = (show [t]) ++ " " ++ (show s) ++ " " ++ (show e)
    showEvalExpr (ListValue l) = Prelude.concat [showEvalExpr li ++ ", " | li <- l]

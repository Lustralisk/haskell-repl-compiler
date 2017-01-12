{-# LANGUAGE OverloadedStrings #-}

module EvalT where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
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

data Errors = TypeError Expr
           | ConditionError Expr
           | NotFoundError String
           | DividedByZeroError Expr
           | OutOfIndexError String
           | OtherError String

instance Show Errors where
    show (TypeError expr) = "TypeError: " ++ show expr
    show (NotFoundError msg) = "NotFoundError: " ++ msg
    show (DividedByZeroError expr) = "DividedByZeroError: " ++ show expr
    show (OtherError msg) = msg

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

-- runEval env ev = runState (runExceptT ev) env

evalExprParser :: Expr -> Eval Value
evalExprParser (Number n) = return $ DoubleValue n
evalExprParser TrueLit = return $ BoolValue True
evalExprParser expr@(Add e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 + v2)
        _ -> throwError $ TypeError expr
evalExprParser expr@(Div e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue _, DoubleValue 0) -> throwError $ DividedByZeroError expr
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 / v2)
        _ -> throwError $ TypeError expr


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
        _ -> throwError $ ConditionError e
evalStatementParser stat@(While e s) = do
    v <- evalExprParser e
    case v of
        (BoolValue True) -> do
            v <- search "$$result$$"
            case v of
                Undefined -> do
                    evalStatementParser s
                    evalStatementParser stat
                _ -> return ()
        (BoolValue False) -> return ()
evalStatementParser (Return e) = do
    v <- evalExprParser e
    insert "$$result$$" v

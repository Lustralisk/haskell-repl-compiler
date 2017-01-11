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
import Control.Monad.Except
import Control.Monad.Identity
import Parser

data Value = BoolValue Bool
           | CharValue Char
           | DoubleValue Double
           | FunctionValue [Text] Statement Env
           | ListValue [Value]
           deriving Show

type Env = M.Map Text Value

data Errors = TypeError String
           | NotFoundError String
           | OtherError String

instance Show Errors where
    show (TypeError msg) = "TypeError: " ++ msg
    show (NotFoundError msg) = "NotFoundError: " ++ msg
    show (OtherError msg) = msg

-- Suppose State Env Value :: Env -> (Value, Env) is a state monad
--   and Either Error Value is an either monad
--   the transformer should convert state monad to an either monad
--   *seems that EitherT is called ErrorT in Haskell*
type Eval a = ExceptT Errors Identity a



evalExprParser :: Env -> Expr -> Eval Value
evalExprParser env (Number n) = return $ DoubleValue n
evalExprParser env TrueLit = return $ BoolValue True
evalExprParser env (Add e1 e2) = do
    r1 <- evalExprParser env e1
    r2 <- evalExprParser env e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 + v2)
        _ -> throwError $ TypeError "shit"
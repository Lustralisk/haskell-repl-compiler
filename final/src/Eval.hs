{-# LANGUAGE OverloadedStrings #-}

module Eval where

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
           deriving (Show, Eq)

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
        (ListValue l) -> do
            (DoubleValue i) <- evalExprParser e
            if isInt i
                then let i' = fromEnum i in if (i' >= 0) && (i' < Prelude.length l)
                    then return (l !! i')
                    else throwError $ OutOfIndexError v e
                else throwError $ TypeError (DoubleValue i) e
        _ -> throwError $ NotFoundError t expr
evalExprParser expr@(Function t es) = do
    v <- search t
    case v of
        (FunctionValue ts stmt fenv) -> do
            env <- get
            put fenv
            inject env t expr ts es
            evalStatementParser stmt
            v' <- search "$$result$$"
            put env
            case v' of
                Undefined -> throwError $ NotFoundError ("Variable " `append` t) expr
                _ -> return v'
        _ -> throwError $ NotFoundError ("Function " `append` t) expr
evalExprParser expr@(LambdaCall e1 e2) = do
    r1 <- evalExprParser e1
    case r1 of
        (FunctionValue ts stmt fenv) -> do
            env <- get
            put fenv
            inject env "anonymous" expr ts [e2]
            evalStatementParser stmt
            v <- search "$$result$$"
            put env
            case v of
                Undefined -> throwError $ NotFoundError "Variable " expr
                _ -> return v
        _ -> throwError $ NotFoundError "Lambda " expr
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
evalExprParser (BoolLit b) = return $ BoolValue b
evalExprParser (CharLit c) = return $ CharValue c
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
evalExprParser expr@(Mul e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ DoubleValue (v1 * v2)
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
evalExprParser expr@(And e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (BoolValue v1, BoolValue v2) -> return $ BoolValue (v1 && v2)
        (BoolValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Or e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (BoolValue v1, BoolValue v2) -> return $ BoolValue (v1 || v2)
        (BoolValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Not e) = do
    r <- evalExprParser e
    case r of
        (BoolValue v) -> return $ BoolValue (not v)
        _ -> throwError $ TypeError r expr
evalExprParser expr@(Eq e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 == v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Lw e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 < v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Le e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 <= v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Gr e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 > v2)
        (DoubleValue _, _) -> throwError $ TypeError r2 expr
        _ -> throwError $ TypeError r1 expr
evalExprParser expr@(Ge e1 e2) = do
    r1 <- evalExprParser e1
    r2 <- evalExprParser e2
    case (r1, r2) of
        (DoubleValue v1, DoubleValue v2) -> return $ BoolValue (v1 >= v2)
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
    v <- evalExprParser e
    case v of
        (DoubleValue n) -> if isInt n
            then let len = fromEnum n in if n > 0
                then insert t (ListValue [Undefined | i <- [1..len]])
                else throwError $ OutOfIndexError v e
            else throwError $ TypeError (DoubleValue n) e
        vi -> throwError $ TypeError vi e
evalStatementParser stmt@(SetVector t e1 e2) = do
    lv <- search t
    case lv of
        (ListValue v) -> do
            value <- evalExprParser e1
            case value of
                (DoubleValue i') -> do
                    v' <- evalExprParser e2
                    if isInt i'
                    then let i = fromEnum i' in if (i < Prelude.length v) && (i >= 0)
                        then insert t (ListValue (Prelude.take i v ++ [v'] ++ Prelude.drop (i + 1) v))
                        else throwError $ OutOfIndexError value e1
                    else throwError $ ValueError v' e2
                _ -> throwError $ TypeError value e1
        _ -> throwError $ NotFoundError t (Variable t)
evalStatementParser (Return e) = do
    v <- evalExprParser e
    insert "$$result$$" v


evalFunctionParser :: Function -> Eval ()
evalFunctionParser (Def t ts stmt) = modify $ updateM t ts stmt

evalProgramParser :: Program -> Eval ()
evalProgramParser (Prog funcs) = case funcs of
    [] -> return ()
    (f:fs) -> do
        evalFunctionParser f
        evalProgramParser (Prog fs)

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

evalProgram :: String -> Eval ()
evalProgram line = case parseOnly programParser $ pack line of
    (Right program) -> evalProgramParser program
    (Left msg) -> throwError $ ParseError $ pack msg

eval :: String -> Eval String
eval line = case parseOnly programParser $ pack line of
    (Right program) -> do
        (\() -> "") `liftM` evalProgramParser program
        main <- search "main"
        case main of
            (FunctionValue ts stmt env') -> do
                v <- printEvalExpr $ evalExpr "(main)"
                (\() -> "") `liftM` delete "main"
                return v
            _ -> return ""
    _ -> case parseOnly statementParser $ pack line of
        (Right statement) -> (\() -> "") `liftM` evalStatementParser statement
        _ -> printEvalExpr $ evalExpr line

runEvalExpr :: String -> Either Errors Value
runEvalExpr line = case runState (runExceptT $ evalExpr line) M.empty of
    (Right a, _) -> Right a
    (Left err, _) -> Left err

runResult line env = runState (runExceptT $ eval $ unpack line) env

runEvalStmt :: String -> Either Errors ()
runEvalStmt line = case runState (runExceptT $ evalStatement line) M.empty of
    (Right a, _) -> Right ()
    (Left err, _) -> Left err

runEval :: String -> String
runEval line = case runState (runExceptT $ eval line) M.empty of
    (Right a, _) -> a
    (Left err, _) -> show err

showEvalExpr :: Value -> String
showEvalExpr (BoolValue b) = show b
showEvalExpr (DoubleValue d) = show d
showEvalExpr (CharValue c) = show c
showEvalExpr (FunctionValue [t] s e) = show [t] ++ " " ++ show s ++ " " ++ show e
showEvalExpr Undefined = "Undefined"
showEvalExpr (ListValue l) = Prelude.concat [showEvalExpr li ++ ", " | li <- l]

printEvalExpr :: Eval Value -> Eval String
printEvalExpr = fmap showEvalExpr

{-# LANGUAGE OverloadedStrings #-}

module Exec where

import Control.Applicative
import Data.Functor
import Data.Either
import Control.Monad
import qualified Data.Map as M
import System.IO
import Parser
import Eval

{-
mainFunc :: IO ()
mainFunc = do
    line <- getLine
    if Data.Text.null $ pack line
    then return ()
    else
        putStrLn $ printEval $ eval $ pack line
    mainFunc
-}

-------------------------------------------------------------------------------
--- REPL loop
--- REPL commands:
---   [Expr/Statement/Function]
---   Show [Variable]
---   Exec [Expr]
---   Define [Function]
-------------------------------------------------------------------------------
replLoop :: Env -> [Char] -> Int -> IO ()
replLoop env hist cnt = do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    if newCount cnt line /= 0
        then replLoop env (hist ++ " " ++ line) $ newCount cnt line
    else
        if line == ""
            then return ()
        else
            case words (hist ++ " " ++ line) of
                ["Show", s] -> case M.lookup (toText s) env of {- show cmd -}
                    Just v -> do
                        putStrLn (s ++ " = " ++ (printEvalExpr $ Right v))
                        replLoop env "" 0
                    Nothing -> do
                        putStrLn ("No such variable, " ++ s)
                        replLoop env "" 0
                _ -> case out of
                    "" -> replLoop env' "" 0
                    _ -> do
                        putStrLn out
                        replLoop env' "" 0
                    where
                        line' = hist ++ " " ++ line
                        (env', out) = eval env line'


repl :: IO ()
repl = replLoop M.empty "" 0

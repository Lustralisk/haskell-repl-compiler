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

replLoop :: Env -> IO ()
replLoop env = do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    if line == ""
    then return ()
    else
        case words line of
            ["show", s] -> case M.lookup (toText s) env of {- show cmd -}
                Just v -> do
                    putStrLn (s ++ " = " ++ (printEvalExpr v))
                    replLoop env
                Nothing -> do
                    putStrLn ("No such variable, " ++ s)
                    replLoop env
            _ -> replLoop env' where
                env' = evalStatement env line

repl :: IO ()
repl = replLoop M.empty
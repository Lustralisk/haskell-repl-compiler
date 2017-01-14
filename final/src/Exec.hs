{-# LANGUAGE OverloadedStrings #-}

module Exec where

import Control.Applicative
import Data.Functor
import Data.Either
import Control.Monad
import qualified Data.Map as M
import System.IO
-- import System.Console.Readline
import Control.Monad.State
import Parser
import EvalT
import Printer
import Debug.Trace

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

prettyPrinterLoop :: [String] -> IO ()
prettyPrinterLoop ts = case ts of
    [] -> return ()
    (x:xs) -> do
        putStrLn x
        prettyPrinterLoop xs

prettyPrinter :: String -> IO ()
prettyPrinter s = prettyPrinterLoop $ splitLn (prettyPrint s)

-------------------------------------------------------------------------------
--- REPL loop
--- REPL commands:
---   [Expr/Statement/Function]
---   Show [Variable]
---   Pretty [Expr/Statement/Function]
---   Exec [Expr]
---   Define [Function]
-------------------------------------------------------------------------------
type Repl a = StateT Env IO a

replLoop :: String -> Int -> Repl ()
replLoop hist cnt = lift $ do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    if newCount cnt line /= 0
        then replLoop env (hist ++ " " ++ line) $ newCount cnt line
    else
        unless (line == "") $
            case trace (hist ++ " " ++ line) (hist ++ " " ++ line) of
                (' ':'s':'h':'o':'w':' ':s) -> case M.lookup (toText s) env of {- show cmd -}
                    Just v -> do
                        putStrLn (s ++ " = " ++ showEvalExpr v)
                        replLoop env "" 0
                    Nothing -> do
                        putStrLn ("No such variable, " ++ s)
                        replLoop env "" 0
                (' ':'p':'r':'e':'t':'t':'y':' ':s) -> do
                    prettyPrinter s
                    replLoop env "" 0
                _ -> case out of
                    "" -> replLoop env' "" 0
                    _ -> do
                        putStrLn out
                        replLoop env' "" 0
                    where
                        line' = hist ++ " " ++ line
                        (out, env') = eval env line'

evalLine :: String -> Repl String
evalLine env line =

repl :: IO ()
repl = replLoop M.empty "" 0

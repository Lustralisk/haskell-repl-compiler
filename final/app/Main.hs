{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.Comp
import System.Environment

data Mod = MRepl | Output | Input | Prog

instance Read Mod where
    read s = 

main :: IO ()
main = do
    args <- getArgs
    runReplT
-- main = translateLang "/Users/ocNflag/Desktop/in.txt" "/Users/ocNflag/Desktop/out.txt"

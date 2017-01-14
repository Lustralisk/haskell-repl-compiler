{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.Comp

main :: IO ()
main = do
    args <- getArgs
    runStateT replT (M.emtpy, "", "", 0)
-- main = translateLang "/Users/ocNflag/Desktop/in.txt" "/Users/ocNflag/Desktop/out.txt"

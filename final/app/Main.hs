{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.Comp

main :: IO ()
main = runReplT
-- main = translateLang "/Users/ocNflag/Desktop/in.txt" "/Users/ocNflag/Desktop/out.txt"

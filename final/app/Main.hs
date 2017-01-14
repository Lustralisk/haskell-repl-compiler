{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.Comp

main :: IO ()
main = repl
-- main = translateLang "/Users/ocNflag/Desktop/in.txt" "/Users/ocNflag/Desktop/out.txt"

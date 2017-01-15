{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.Comp
import Compiler.IRExec


main :: IO ()
-- main = runReplT
main = execWrapper "/Users/ocNflag/Desktop/out.txt"

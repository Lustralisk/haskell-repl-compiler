{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.CompExec
import Compiler.IRExec


main :: IO ()
--main = runReplT
main = execWrapper "/Users/ocNflag/Desktop/out.txt"

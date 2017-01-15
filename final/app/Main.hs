{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import System.IO
import Compiler.CompExec
import Compiler.IRExec


main :: IO ()
--main = do
--	cmd <- getLine
--	case words cmd of
--		["-i", file1, "-o", file2] -> 
--		["-t", file1, "-o", file2] -> 
--		["-repl"] -> 
main = runReplT
--main = execWrapper "/Users/lustralisk/Desktop/out.txt"

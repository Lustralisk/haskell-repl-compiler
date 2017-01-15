{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.CompExec
import Compiler.IRExec
import System.Environment

main :: IO ()
-- main = do
--     cmd <- getArgs
--     case cmd of
--         ["-i", file1, "-o", file2] -> return ()
--         ["-t", file1, "-o", file2] -> return ()
--         [file1, "-o", file2, "-is", arch] -> return ()
--         [file1, "-o", file2] -> return ()
--         ["-repl"] -> runReplT
main = execWrapper "/Users/dycraft/Desktop/out.txt"

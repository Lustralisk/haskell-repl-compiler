{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.CompExec
import Compiler.IRExec
import System.Environment

main :: IO ()
main = do
    cmd <- getArgs
    case cmd of
        ["-i", file1, "-o", file2] -> return () -- 解释执行
        ["-t", file1, "-o", file2] -> return () -- 输出语法树
        [file1, "-o", file2] -> translateLang file1 file2 -- 编译
        ["-repl"] -> runReplT
--main = execWrapper "/Users/ocNflag/Desktop/out.txt"

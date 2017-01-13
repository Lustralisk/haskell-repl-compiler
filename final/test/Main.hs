{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import Text.Printf
import Specs.ParserSpec
import Specs.EvalSpec

tests  = evalTests
        ++ parserTests

main :: IO ()
main = do
    putStrLn ""
    mapM_ (\(s,a) -> printf "%-30s:  " s >> a) tests

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import Text.Printf
import Specs.ParserTest
import Specs.EvalTest
import Specs.EvalSpec

tests  = evalSpecs
        ++ parserTests
        ++ evalTests

main :: IO ()
main = do
    putStrLn ""
    mapM_ (\(s,a) -> printf "%-30s:  " s >> a) tests

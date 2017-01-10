{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Test.QuickCheck
import Text.Printf
import Specs.ParserSpec
import Specs.EvalSpec

tests  = evalTests
        ++ parserTests

main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

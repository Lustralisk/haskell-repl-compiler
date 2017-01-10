{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalSpec (evalTests) where

import Test.QuickCheck

evalTests = [("reverse.reverse/id", ())]

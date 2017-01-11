{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalSpec where

import Test.QuickCheck
import Parser
import Eval

evalTests = []

instance Arbitrary Expr where
    arbitrary = choose ()

-- instance Arbitrary Expr where
--     arbitrary = sized arbExpr
--
-- arbExpr :: Int -> Gen Expr
-- arbExpr 0 = do
--     a <- arbitrary
--     return $ Node a []
-- arbExpr n = do
--     (Positive m) <- arbitrary
--     let n' = n `div` (m + 1)
--     f <- replicateM m (arbTree n')
--     a <- arbitrary
--     return $ Node a f

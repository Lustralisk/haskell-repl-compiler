{-# LANGUAGE OverloadedStrings #-}

module Specs.EvalSpec (evalTests) where

import Test.QuickCheck
import qualified Data.Map as M
import Parser
import EvalT

instance Arbitrary Expr where
    arbitrary = sized genExpr
    -- shrink = shrinkExpr

lits = [TrueLit, FalseLit]
uniOpers = [Not]
binOpers = [And, Or]
opers e1 e2 = [Not e1, And e1 e2, Or e1 e2]

genExpr :: Int -> Gen Expr
genExpr 0 = elements lits
genExpr n = do
    e1 <- genExpr (n - 1)
    e2 <- genExpr (n - 1)
    elements $ opers e1 e2

-- shrinkExpr :: Expr -> [Expr]
-- shrinkExpr TrueLit = []
-- shrinkExpr FalseLit = []
-- shrinkExpr (Not e) = lits ++ shrinkTree e

prop_ResultIsBoolValue :: String -> Bool
prop_ResultIsBoolValue expr = (evalExpr expr) >>= isBoolValue where
    isBoolValue (BoolValue _) = True
    isBoolValue _ = False

evalTests = [("ResultIsBoolValue", quickCheck prop_ResultIsBoolValue)]

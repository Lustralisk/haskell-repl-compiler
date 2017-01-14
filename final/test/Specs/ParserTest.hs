{-# LANGUAGE OverloadedStrings #-}

module Specs.ParserTest where

import Test.QuickCheck
import Data.Text
import Data.Attoparsec.Text
import Parser

parserTests = [("show . parse == id", quickCheck prop_ShowParse)]

genBoolLit :: Gen Expr
genBoolLit =  elements [BoolLit True, BoolLit False]

genInt :: Gen Expr
genInt = Number . toEnum <$> (choose (0, 100) :: Gen Int)

genChar :: Gen Char
genChar = arbitrary

genDouble :: Gen Expr
genDouble = Number <$> choose (0, 100.0)

genNil :: Gen Expr
genNil = return Nil

genVarName :: Gen Text
genVarName = pack <$> listOf1 elements ['a'..'z']

genVar :: Gen Expr
genVar = Variable <$> genVarName

genBooleanExpr :: Int -> Gen Expr
genBooleanExpr limit = sized genN where
    genN 0 = genBoolLit
    genN 1 = genBoolLit
    genN n = do
        e1 <- genN (min limit n - 1)
        e2 <- genN (min limit n - 2)
        elements [Not e1, And e1 e2, Or e1 e2]

genNumberExpr :: Int -> Gen Expr
genNumberExpr limit = sized genN where
    genN 0 = oneof [genInt, genDouble]
    genN 1 = oneof [genInt, genDouble]
    genN n = do
        e1 <- genN (min limit n - 1)
        e2 <- genN (min limit n - 2)
        elements [Add e1 e2, Sub e1 e2, Mul e1 e2, Div e1 e2]

genExpr :: Int -> Gen Expr
genExpr limit = sized genN where
    genN 0 = oneof [genInt, genDouble, genBoolLit, genChar, genNil, genVar]
    genN 1 = oneof [genInt, genDouble, genBoolLit, genChar, genNil, genVar]
    genN n = frequency [(1, genUniExpr), (3, genBinExpr)] where
        genBinExpr = do
            e1 <- genN (min limit n - 1)
            e2 <- genN (min limit n - 2)
            t <- genVarName
            elements [Add e1 e2, Sub e1 e2, Mul e1 e2, Div e1 e2, And e1 e2, Or e1 e2,
                Eq e1 e2, Lw e1 e2, Le e1 e2, Gr e1 e2, Ge e1 e2, Cons e1 e2,
                Let t e1 e2, Function t [e1, e2], LambdaCall e1 e2]
        genUniExpr = do
            e <- genN (min limit n - 1)
            t <- genVarName
            elements [Not e, Car e, Cdr e, Vec t e, Function t [e], Lambda t e]

prop_ShowParse :: Property
prop_ShowParse = forAll (genExpr 100) $ \x -> parseOnly exprParser $ pack $ show x

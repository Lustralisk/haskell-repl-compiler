{-# LANGUAGE OverloadedStrings #-}

module Specs.ParserTest where

import Test.QuickCheck
import Data.Text
import Data.Attoparsec.Text
import Control.Monad.State
import Control.Applicative
import Control.Monad
import Debug.Trace
import Parser

parserTests = [("Expr: show . parse == id", quickCheck prop_ShowParseExpr),
                ("Stmt: show . parse == id", quickCheck prop_ShowParseStmt),
                ("Func: show . parse == id", quickCheck prop_ShowParseFunc)]

genVarName :: Gen Text
genVarName = pack <$> listOf1 (elements ['a'..'z'])

{- Gen Expression -}

genBoolLit :: Gen Expr
genBoolLit =  elements [BoolLit True, BoolLit False]

genInt :: Gen Expr
genInt = Number . toEnum <$> (choose (0, 100) :: Gen Int)

genChar :: Gen Expr
-- genChar = CharLit <$> arbitrary
genChar = CharLit <$> elements (['A'..'Z'] ++ ['a'..'z'])

genDouble :: Gen Expr
genDouble = Number <$> choose (1, 100.0)

genNil :: Gen Expr
genNil = return Nil

genVar :: Gen Expr
genVar = Variable <$> genVarName

genBooleanExpr :: Int -> Gen Expr
genBooleanExpr limit = sized genN where
    genN 0 = genBoolLit
    genN n = do
        e1 <- genN (min limit n - 1)
        e2 <- genN (min limit n - 1)
        elements [Not e1, And e1 e2, Or e1 e2]

genNumberExpr :: Int -> Gen Expr
genNumberExpr limit = sized genN where
    genN 0 = oneof [genInt, genDouble]
    genN n = do
        e1 <- genN (min limit n - 1)
        e2 <- genN (min limit n - 1)
        elements [Add e1 e2, Sub e1 e2, Mul e1 e2, Div e1 e2]

genExpr :: Int -> Gen Expr
genExpr limit = sized genN where
    genN 0 = oneof [genInt, genDouble, genBoolLit, genChar, genNil, genVar]
    genN n = frequency [(1, genUniExpr), (3, genBinExpr)] where
        genBinExpr = do
            e1 <- genN (min limit n - 1)
            e2 <- genN (min limit n - 1)
            t <- genVarName
            elements [Add e1 e2, Sub e1 e2, Mul e1 e2, Div e1 e2, And e1 e2, Or e1 e2,
                Eq e1 e2, Lw e1 e2, Le e1 e2, Gr e1 e2, Ge e1 e2, Cons e1 e2,
                Let t e1 e2, Function t [e1, e2]]
        genUniExpr = do
            e <- genN (min limit n - 1)
            t <- genVarName
            elements [Not e, Car e, Cdr e, Vec t e, Function t [e], Lambda t e]

{- Gen Statement -}

genSet :: Gen Statement
genSet = liftM2 Set genVarName (genExpr 2)

genSkip :: Gen Statement
genSkip = return Skip

genIf :: Gen Statement
genIf = liftM3 If (genExpr 2) (genStmtList 3) (genStmtList 3)

genWhile :: Gen Statement
genWhile = liftM2 While (genExpr 2) (genStmtList 3)

genMakeVector :: Gen Statement
genMakeVector = liftM2 MakeVector genVarName (genExpr 2)

genSetVector :: Gen Statement
genSetVector = liftM3 SetVector genVarName (genExpr 1) (genExpr 2)

genReturn :: Gen Statement
genReturn = liftM Return (genExpr 2)

genStmtList :: Int -> Gen Statement
genStmtList limit = liftM StatementList $ vectorOf limit $ oneof [genSet, genSkip, genMakeVector, genSetVector, genReturn]

genStatement :: Int -> Gen Statement
genStatement limit = liftM StatementList $ vectorOf limit $ oneof [genStmtList limit, genSet, genSkip, genIf, genWhile, genMakeVector, genSetVector, genReturn]

{- Gen Function -}

genFunction :: Int -> Gen Function
genFunction limit = do
    v <- genVarName
    vs <- listOf1 genVarName
    s <- genStatement limit
    return (Def v vs s)

{- Gen Property -}

prop_ShowParseExpr :: Property
prop_ShowParseExpr = forAll (genExpr 5) $ \x -> parseOnly exprParser (pack $ show x) == Right x

prop_ShowParseStmt :: Property
prop_ShowParseStmt = forAll (genStatement 6) $ \x -> parseOnly statementParser (pack $ show x) == Right x

prop_ShowParseFunc :: Property
prop_ShowParseFunc = forAll (genFunction 6) $ \x -> parseOnly functionParser (pack $ show x) == Right x

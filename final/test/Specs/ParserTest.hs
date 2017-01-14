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
                ("Stmt: show . parse == id", quickCheck prop_ShowParseStmt)]

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

type Count a = State Int a

genVarNameT :: Count (Gen Text)
genVarNameT = return (pack <$> listOf1 (elements ['a'..'z']))

consume :: Int -> Count ()
consume n = state $ \count -> ((), count - n)

listOf1' :: Int -> Gen a -> Gen [a]
listOf1' n gen = do
    k <- choose (1, n)
    vectorOf k gen

genSet :: Count (Gen Statement)
genSet = do
    consume 1
    return $ liftM2 Set genVarName (genExpr 2)

genSkip :: Count (Gen Statement)
genSkip = do
    consume 1
    return $ return Skip

genIf :: Count (Gen Statement)
genIf = do
    consume 1
    t1 <- genStmtList
    t2 <- genStmtList
    return $ liftM3 If (genExpr 2) t1 t2

genWhile :: Count (Gen Statement)
genWhile = do
    consume 1
    t <- genStmtList
    return $ liftM2 While (genExpr 2) t

genMakeVector :: Count (Gen Statement)
genMakeVector = do
    consume 1
    return $ liftM2 MakeVector genVarName (genExpr 2)

genSetVector :: Count (Gen Statement)
genSetVector = do
    consume 2
    return $ liftM3 SetVector genVarName (genExpr 1) (genExpr 2)

genReturn :: Count (Gen Statement)
genReturn = do
    consume 1
    return $ liftM Return (genExpr 2)

genStmtList :: Count (Gen Statement)
genStmtList = do
    a0 <- trace "0" genStmtList
    a1 <- trace "1" genSet
    a2 <- trace "2" genSkip
    a3 <- trace "3" genWhile
    a4 <- trace "4" genIf
    a5 <- trace "5" genMakeVector
    a6 <- trace "6" genSetVector
    a7 <- trace "7" genReturn
    n <- get
    if n <= 0
        then let l1 = listOf1 a2 in return $ liftM StatementList l1
        else let l2 = listOf1 a2 in return $ liftM StatementList l2

genStatement :: Int -> Gen Statement
genStatement limit = a where
    (a, _) = runState genStmtList limit

{- Gen Function -}

prop_ShowParseExpr :: Property
prop_ShowParseExpr = forAll (genExpr 5) $ \x -> parseOnly exprParser (pack $ show x) == Right x

prop_ShowParseStmt :: Property
prop_ShowParseStmt = forAll (genStatement 10) $ \x -> parseOnly statementParser (pack $ show x) == Right x

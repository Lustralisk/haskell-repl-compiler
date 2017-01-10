{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import Parser

concate = Data.Text.concat
inden n = Data.Text.replicate n " "
(>>>) :: Int -> [Text] -> Text
(>>>) n ss = concate $ (inden n):ss

printExpr :: Int -> Bool -> Expr -> Text
printExpr offset _ (Variable t) = offset >>> [t]
printExpr offset _ (Vec t e) = offset >>> [t, (printExpr offset True e)]
printExpr offset _ (Number e) = offset >>> [pack (show e)]
printExpr offset _ (CharLit e) = offset >>> ["\'", pack [e], "\'"]
printExpr offset _ TrueLit = offset >>> ["True"]
printExpr offset _ FalseLit = offset >>> ["False"]
printExpr offset _ (Function t es') = case es' of
    [] -> offset >>> ["(", t, ")"]
    (e:es) -> offset >>> ["(", t, (printExpr 1 True e), ss, ")"] where
        offset' = 2 + offset + (Data.Text.length t)
        trans = ("\r\n" `append`).(printExpr offset' True)
        ss = concate $ Prelude.map trans es

printExpr offset _ (Let t e1 e2) = offset >>> ["(let ", t, "\r\n", s1, "\r\n", s2, ")"] where
    s1 = printExpr (offset + 4) True e1
    s2 = printExpr (offset + 4) True e2

printExpr offset _ (Add e1 e2) = offset >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Sub e1 e2) = offset >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Mul e1 e2) = offset >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Div e1 e2) = offset >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (And e1 e2) = offset >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Or e1 e2) = offset >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Not e) = offset >>> ["(not ", s, ")"] where
        s = printExpr 0 True e
printExpr offset _ (Eq e1 e2) = offset >>> ["(eq ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Lw e1 e2) = offset >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Le e1 e2) = offset >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Gr e1 e2) = offset >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset _ (Ge e1 e2) = offset >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2



prettyPrint :: [Char] -> Text
prettyPrint line = case parseOnly exprParser $ pack line of
    (Right expr) -> printExpr 0 True expr
    _ -> "What?"
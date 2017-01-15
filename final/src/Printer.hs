{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import Parser

concate = Data.Text.concat
inden n = Data.Text.replicate n " "
(>>>) :: (Int, Bool) -> [Text] -> Text
(>>>) (n, s) ss = case s of
    True -> concate $ (inden n):ss
    False -> concate ss

-- True -> another block
-- False -> continue

printExpr :: Int -> Bool -> Expr -> Text
printExpr offset style (Variable t) = (offset, style) >>> [t]
printExpr offset style (Vec t e) = (offset, style) >>> ["(vector-ref ", t, (printExpr offset True e), ")"]
printExpr offset style (Number e) = (offset, style) >>> [pack (show e)]
printExpr offset style (CharLit e) = (offset, style) >>> ["\'", pack [e], "\'"]
printExpr offset style (BoolLit True) = (offset, style) >>> ["True"]
printExpr offset style (BoolLit False) = (offset, style) >>> ["False"]
printExpr offset style (Function t es') = case es' of
    [] -> (offset, style) >>> ["(", t, ")"]
    (e:es) -> (offset, style) >>> ["(", t, (printExpr 1 True e), ss, ")"] where
        offset' = 2 + offset + (Data.Text.length t)
        trans = ("\r\n" `append`).(printExpr offset' True)
        ss = concate $ Prelude.map trans es

printExpr offset style (Lambda t e) = (offset, style) >>> ["(lambda ", t, "\r\n", s, ")"] where
    s = printExpr (offset + 8 + Data.Text.length t) True e

printExpr offset style (LambdaCall e1 e2) = (offset, style) >>> ["(", s1, " ", s2, ")"] where
    s1 = printExpr (offset + 1) True e1
    s2 = printExpr (offset + 1) True e2

printExpr offset style (Let t e1 e2) = (offset, style) >>> ["(let ", t, "\r\n", s1, "\r\n", s2, ")"] where
    s1 = printExpr (offset + 2) True e1
    s2 = printExpr (offset + 2) True e2

printExpr offset style (Add e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Add e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Add e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Add e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Add e1 e2) = (offset, style) >>> ["(+ ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2

printExpr offset style (Sub e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Sub e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Sub e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Sub e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Sub e1 e2) = (offset, style) >>> ["(- ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2

printExpr offset style (Mul e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Mul e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Mul e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Mul e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Mul e1 e2) = (offset, style) >>> ["(* ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2

printExpr offset style (Div e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Div e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Div e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Div e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Div e1 e2) = (offset, style) >>> ["(/ ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2

printExpr offset style (And e1@(BoolLit _) e2@(BoolLit _)) = (offset, style) >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (And e1@(BoolLit _) e2@(Variable _)) = (offset, style) >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (And e1@(Variable _) e2@(BoolLit _)) = (offset, style) >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (And e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (And e1 e2) = (offset, style) >>> ["(and ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 5) False e1
        s2 = printExpr (offset + 5) True e2
printExpr offset style (Or e1@(BoolLit _) e2@(BoolLit _)) = (offset, style) >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Or e1@(BoolLit _) e2@(Variable _)) = (offset, style) >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Or e1@(Variable _) e2@(BoolLit _)) = (offset, style) >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Or e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Or e1 e2) = (offset, style) >>> ["(or ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 4) False e1
        s2 = printExpr (offset + 4) True e2
printExpr offset style (Not e) = (offset, style) >>> ["(not ", s, ")"] where
    s = printExpr (offset + 5) False e
printExpr offset style (Eq e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Eq e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Eq e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Eq e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Eq e1 e2) = (offset, style) >>> ["(= ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2
printExpr offset style (Lw e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Lw e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Lw e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Lw e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Lw e1 e2) = (offset, style) >>> ["(< ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2
printExpr offset style (Le e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Le e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Le e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Le e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Le e1 e2) = (offset, style) >>> ["(<= ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 4) False e1
        s2 = printExpr (offset + 4) True e2
printExpr offset style (Gr e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Gr e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Gr e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Gr e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Gr e1 e2) = (offset, style) >>> ["(> ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) False e1
        s2 = printExpr (offset + 3) True e2
printExpr offset style (Ge e1@(Number _) e2@(Number _)) = (offset, style) >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Ge e1@(Number _) e2@(Variable _)) = (offset, style) >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Ge e1@(Variable _) e2@(Number _)) = (offset, style) >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Ge e1@(Variable _) e2@(Variable _)) = (offset, style) >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 True e1
        s2 = printExpr 0 True e2
printExpr offset style (Ge e1 e2) = (offset, style) >>> ["(>= ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 4) False e1
        s2 = printExpr (offset + 4) True e2

printExpr offset style (Cons e1 e2) = (offset, style) >>> ["(cons ", s1, "\r\n", s2, ")"] where
    s1 = printExpr 0 True e1
    s2 = printExpr (offset + 6) True e2
printExpr offset style (Car e) = (offset, style) >>> ["(car ", s, ")"] where
    s = printExpr 0 True e
printExpr offset style (Cdr e) = (offset, style) >>> ["(cdr ", s, ")"] where
    s = printExpr 0 True e

printStatement :: Int -> Bool -> Statement -> Text
printStatement offset style (StatementList s) = case s of
    [] -> (offset, style) >>> ["skip"]
    (x:xs) -> (offset, style) >>> ["(begin", sss, ")"] where
        sss = printStatementList (offset + 2) style s where
            printStatementList offset' style' s' = case s' of
                [] -> ""
                (x':xs') -> (offset', style') >>> [b, bs] where
                    b = "\r\n" `append` (printStatement offset' True x')
                    bs = printStatementList offset' style' xs'
printStatement offset style Skip = (offset, style) >>> ["skip"]
printStatement offset style (Set t e) = (offset, style) >>> ["(set! ", t, "\r\n", c, ")"] where
    c = printExpr (offset + 6) True e
printStatement offset style (If e s1 s2) = (offset, style) >>> ["(if ", c, "\r\n", b1, "\r\n", b2, ")"] where
    c = printExpr (offset + 4) False e
    b1 = printStatement (offset + 4) True s1
    b2 = printStatement (offset + 2) True s2
printStatement offset style (While e s) = (offset, style) >>> ["(while ", c, "\r\n", b, ")"] where
    c = printExpr offset False e
    b = printStatement (offset + 2) True s
printStatement offset style (MakeVector t e) = (offset, style) >>> ["(make-vector ", t, "\r\n", c, ")"] where
    c = printExpr (offset + 13) True e
printStatement offset style (SetVector t e1 e2) = (offset, style) >>> ["(vector-set! ", t, "\r\n", c1, "\r\n", c2, ")"] where
    c1 = printExpr (offset + 13) False e1
    c2 = printExpr (offset + 13) True e2
printStatement offset style (Return e) = (offset, style) >>> ["(return ", c, ")"] where
    c = printExpr (offset + 8) False e

printFunction :: Int -> Bool -> Function -> Text
printFunction offset style (Def t ts ss) = (offset, style) >>> ["(define ", t, " (", " " `intercalate` ts, ")\r\n", bs] where
    bs = printStatement (offset + 2) True ss

prettyPrint :: Text -> Text
prettyPrint line = case parseOnly functionParser line of
    (Right function) -> printFunction 0 True function
    _ -> case parseOnly statementParser line of
        (Right statement) -> printStatement 0 True statement
        _ -> case parseOnly exprParser line of
            (Right expr) -> printExpr 0 True expr
            _ -> "What?"
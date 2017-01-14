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

printExpr :: Int -> Expr -> Text
printExpr offset (Variable t) = offset >>> [t]
printExpr offset (Vec t e) = offset >>> ["(vector-ref ", t, (printExpr offset e), ")"]
printExpr offset (Number e) = offset >>> [pack (show e)]
printExpr offset (CharLit e) = offset >>> ["\'", pack [e], "\'"]
printExpr offset (BoolLit True) = offset >>> ["True"]
printExpr offset (BoolLit False) = offset >>> ["False"]
printExpr offset (Function t es') = case es' of
    [] -> offset >>> ["(", t, ")"]
    (e:es) -> offset >>> ["(", t, (printExpr 1 e), ss, ")"] where
        offset' = 2 + offset + (Data.Text.length t)
        trans = ("\r\n" `append`).(printExpr offset')
        ss = concate $ Prelude.map trans es

printExpr offset (Let t e1 e2) = offset >>> ["(let ", t, " ", s1, "\r\n", s2, ")"] where
    s1 = printExpr (offset + 6 + (Data.Text.length t)) e1
    s2 = printExpr (offset + 2) e2

printExpr offset (Add e1@(Number _) e2@(Number _)) = offset >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Add e1@(Number _) e2@(Variable _)) = offset >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Add e1@(Variable _) e2@(Number _)) = offset >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Add e1@(Variable _) e2@(Variable _)) = offset >>> ["(+ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Add e1 e2) = offset >>> ["(+ ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2

printExpr offset (Sub e1@(Number _) e2@(Number _)) = offset >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Sub e1@(Number _) e2@(Variable _)) = offset >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Sub e1@(Variable _) e2@(Number _)) = offset >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Sub e1@(Variable _) e2@(Variable _)) = offset >>> ["(- ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Sub e1 e2) = offset >>> ["(- ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2

printExpr offset (Mul e1@(Number _) e2@(Number _)) = offset >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Mul e1@(Number _) e2@(Variable _)) = offset >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Mul e1@(Variable _) e2@(Number _)) = offset >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Mul e1@(Variable _) e2@(Variable _)) = offset >>> ["(* ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Mul e1 e2) = offset >>> ["(* ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2

printExpr offset (Div e1@(Number _) e2@(Number _)) = offset >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Div e1@(Number _) e2@(Variable _)) = offset >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Div e1@(Variable _) e2@(Number _)) = offset >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Div e1@(Variable _) e2@(Variable _)) = offset >>> ["(/ ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Div e1 e2) = offset >>> ["(/ ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2

printExpr offset (And e1@(BoolLit _) e2@(BoolLit _)) = offset >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (And e1@(BoolLit _) e2@(Variable _)) = offset >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (And e1@(Variable _) e2@(BoolLit _)) = offset >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (And e1@(Variable _) e2@(Variable _)) = offset >>> ["(and ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (And e1 e2) = offset >>> ["(and ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 5) e1
        s2 = printExpr (offset + 5) e2
printExpr offset (Or e1@(BoolLit _) e2@(BoolLit _)) = offset >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Or e1@(BoolLit _) e2@(Variable _)) = offset >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Or e1@(Variable _) e2@(BoolLit _)) = offset >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Or e1@(Variable _) e2@(Variable _)) = offset >>> ["(or ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Or e1 e2) = offset >>> ["(or ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 4) e1
        s2 = printExpr (offset + 4) e2
printExpr offset (Not e) = offset >>> ["(not ", s, ")"] where
    s = printExpr (offset + 5) e
printExpr offset (Eq e1@(Number _) e2@(Number _)) = offset >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Eq e1@(Number _) e2@(Variable _)) = offset >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Eq e1@(Variable _) e2@(Number _)) = offset >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Eq e1@(Variable _) e2@(Variable _)) = offset >>> ["(= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Eq e1 e2) = offset >>> ["(= ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2
printExpr offset (Lw e1@(Number _) e2@(Number _)) = offset >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Lw e1@(Number _) e2@(Variable _)) = offset >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Lw e1@(Variable _) e2@(Number _)) = offset >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Lw e1@(Variable _) e2@(Variable _)) = offset >>> ["(< ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Lw e1 e2) = offset >>> ["(< ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2
printExpr offset (Le e1@(Number _) e2@(Number _)) = offset >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Le e1@(Number _) e2@(Variable _)) = offset >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Le e1@(Variable _) e2@(Number _)) = offset >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Le e1@(Variable _) e2@(Variable _)) = offset >>> ["(<= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Le e1 e2) = offset >>> ["(<= ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 4) e1
        s2 = printExpr (offset + 4) e2
printExpr offset (Gr e1@(Number _) e2@(Number _)) = offset >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Gr e1@(Number _) e2@(Variable _)) = offset >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Gr e1@(Variable _) e2@(Number _)) = offset >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Gr e1@(Variable _) e2@(Variable _)) = offset >>> ["(> ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Gr e1 e2) = offset >>> ["(> ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 3) e1
        s2 = printExpr (offset + 3) e2
printExpr offset (Ge e1@(Number _) e2@(Number _)) = offset >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Ge e1@(Number _) e2@(Variable _)) = offset >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Ge e1@(Variable _) e2@(Number _)) = offset >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Ge e1@(Variable _) e2@(Variable _)) = offset >>> ["(>= ", s1, " ", s2, ")"] where
        s1 = printExpr 0 e1
        s2 = printExpr 0 e2
printExpr offset (Ge e1 e2) = offset >>> ["(>= ", s1, "\r\n", s2, ")"] where
        s1 = printExpr (offset + 4) e1
        s2 = printExpr (offset + 4) e2

printExpr offset (Cons e1 e2) = offset >>> ["(cons ", s1, "\r\n", s2, ")"] where
    s1 = printExpr 0 e1
    s2 = printExpr (offset + 6) e2
printExpr offset (Car e) = offset >>> ["(car ", s, ")"] where
    s = printExpr 0 e
printExpr offset (Cdr e) = offset >>> ["(cdr ", s, ")"] where
    s = printExpr 0 e

printStatement :: Int -> Statement -> Text
printStatement offset (StatementList s) = case s of
    [] -> offset >>> ["skip"]
    (x:xs) -> offset >>> ["(begin", sss, ")"] where
        sss = printStatementList (offset + 2) s where
            printStatementList offset' s' = case s' of
                [] -> ""
                (x':xs') -> offset >>> [b, bs] where
                    b = "\r\n" `append` (printStatement offset' x')
                    bs = printStatementList offset' xs'
printStatement offset Skip = offset >>> ["skip"]
printStatement offset (Set t e) = offset >>> ["(set! ", t, "\r\n", c, ")"] where
    c = printExpr (offset + 6) e
printStatement offset (If e s1 s2) = offset >>> ["(if ", c, "\r\n", b1, "\r\n", b2, ")"] where
    c = printExpr (offset + 4) e
    b1 = printStatement (offset + 2) s1
    b2 = printStatement (offset + 2) s2
printStatement offset (While e s) = offset >>> ["(while ", c, "\r\n", b, ")"] where
    c = printExpr offset e
    b = printStatement (offset + 2) s
printStatement offset (MakeVector t e) = offset >>> ["(make-vector ", t, "\r\n", c, ")"] where
    c = printExpr (offset + 13) e
printStatement offset (SetVector t e1 e2) = offset >>> ["(vector-set! ", t, "\r\n", c1, "\r\n", c2, ")"] where
    c1 = printExpr (offset + 13) e1
    c2 = printExpr (offset + 13) e2
printStatement offset (Return e) = offset >>> ["(return ", c, ")"] where
    c = printExpr (offset + 8) e

printFunction :: Int -> Function -> Text
printFunction offset (Def t ts ss) = offset >>> ["(define ", t, " (", " " `intercalate` ts, ")\r\n", bs] where
    bs = printStatement (offset + 2) ss

prettyPrint :: Text -> Text
prettyPrint line = case parseOnly functionParser line of
    (Right function) -> printFunction 0 function
    _ -> case parseOnly statementParser line of
        (Right statement) -> printStatement 0 statement
        _ -> case parseOnly exprParser line of
            (Right expr) -> printExpr 0 expr
            _ -> "What?"
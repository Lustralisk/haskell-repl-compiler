{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import Parser

concate = Data.Text.concat

printExpr :: Int -> Bool -> Expr -> Text
printExpr offset _ (Variable t) = t
printExpr offset _ (Vec t e) = concate [t, (printExpr offset True e)]
printExpr offset _ (Number e) = pack (show e)
printExpr offset _ (CharLit e) = concate ["\'", pack [e], "\'"]
printExpr offset _ TrueLit = "True"
printExpr offset _ FalseLit = "False"
printExpr offset _ (Function t es) = concate ["(", t, " ", (intercalate " " (Prelude.map (printExpr offset True) es)), ")"]

printExpr offset _ (Let t e1 e2) = concate ["(let ", t, "\r\n", s1, ", \r\n", s2, "\r\n)"] where
	s1 = printExpr (offset + 2) True e1
	s2 = printExpr (offset + 2) True e2

printExpr offset _ (Add e1 e2) = concate ["(+ ", s1, " ", s2, ")"] where
	s1 = printExpr (offset + 2) True e1
	s2 = printExpr (offset + 2) True e2

prettyPrint :: [Char] -> Text
prettyPrint line = case parseOnly exprParser $ pack line of
    (Right expr) -> printExpr 0 True expr
    _ -> "What?"
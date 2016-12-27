{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Text
import Data.Attoparsec.Text

{- Divide utility -}
(<+>) :: Monad m => m t -> m t -> m [t]
(<+>) = liftM2 (\a b -> [a, b])
(<:>) :: Monad m => m Char -> m [Char] -> m [Char]
(<:>) = liftM2 (\a b -> a:b)
(<++>) :: Monad m => m [Char] -> m [Char] -> m [Char]
(<++>) = liftM2 (\a b -> a ++ b)
    
digits = many1 digit

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

testParser :: Parser Expr -> [Char] -> Either String Expr
testParser p s = parseOnly p $ pack s

{- Divide declare -}
    
data Expr
    = TrueLit
    | FalseLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Number Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Eq Expr Expr
    | Lw Expr Expr
    | Le Expr Expr
    | Gr Expr Expr
    | Ge Expr Expr
    | Nil
    | Car Expr
    | Cdr Expr
    | Cons Expr Expr
    | CharLit Char
    | StringLit String
    deriving Show

exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser <|> 
             floatParser <|> addParser <|> subParser <|> mulParser <|> divParser <|> 
             eqParser <|> lwParser <|> leParser <|> grParser <|> geParser <|> 
             charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

{- WARN need to strengthen -}
floatParser :: Parser Expr
floatParser = do
    d <- lexeme $ digits <++> (char '.' <:> digits) <|> digits
    return (Number $ read $ d)

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)

orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)
    
addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ char '('
    lexeme $ char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ char '('
    lexeme $ char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Div expr1 expr2)

eqParser :: Parser Expr
eqParser = do
    lexeme $ char '('
    lexeme $ char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Eq expr1 expr2)
    
lwParser :: Parser Expr
lwParser = do
    lexeme $ char '('
    lexeme $ char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Lw expr1 expr2)
    
leParser :: Parser Expr
leParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Le expr1 expr2)
   
grParser :: Parser Expr
grParser = do
    lexeme $ char '('
    lexeme $ char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Gr expr1 expr2)
    
geParser :: Parser Expr
geParser = do
    lexeme $ char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Ge expr1 expr2)

consParser :: Parser Expr
consParser = do
    lexeme $ char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ char ')'
    return (Car expr)

cdrParser :: Parser Expr
cdrParser = do
    lexeme $ char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ char ')'
    return (Cdr expr)

charParser :: Parser Expr
charParser = do
    c <- lexeme $ char '\'' *> anyChar <* char '\''
    return (CharLit c)

{- WARN stringlit incorrect -}
stringParser :: Parser Expr
stringParser = do
    s <- lexeme $ string "''" *> manyTill anyChar (string "''")
    return (StringLit s)

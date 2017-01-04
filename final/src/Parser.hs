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

binParser :: Text -> (Expr -> Expr -> Expr) -> Parser Expr
binParser token op = do
    lexeme $ char '('
    lexeme $ string token
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (op expr1 expr2)

uniParser :: Text -> (Expr -> Expr) -> Parser Expr
uniParser token op = do
    lexeme $ char '('
    lexeme $ string token
    expr <- exprParser
    lexeme $ char ')'
    return (op expr)

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
    | Variable Text
    deriving Show

exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser <|> 
             floatParser <|> addParser <|> subParser <|> mulParser <|> divParser <|> 
             eqParser <|> lwParser <|> leParser <|> grParser <|> geParser <|> 
             charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser <|> nilParser <|>
             variableParser

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
notParser = uniParser "not" Not

andParser :: Parser Expr
andParser = binParser "and" And

orParser :: Parser Expr
orParser = binParser "or" Or
    
addParser :: Parser Expr
addParser = binParser "+" Add

subParser :: Parser Expr
subParser = binParser "-" Sub

mulParser :: Parser Expr
mulParser = binParser "*" Mul

divParser :: Parser Expr
divParser = binParser "/" Div

eqParser :: Parser Expr
eqParser = binParser "=" Eq
    
lwParser :: Parser Expr
lwParser = binParser "<" Lw
    
leParser :: Parser Expr
leParser = binParser "<=" Le
   
grParser :: Parser Expr
grParser = binParser ">" Gr
    
geParser :: Parser Expr
geParser = binParser ">=" Ge

nilParser :: Parser Expr
nilParser = lexeme $ string "Nil" $> Nil

consParser :: Parser Expr
consParser = binParser "cons" Cons

carParser :: Parser Expr
carParser = uniParser "car" Car

cdrParser :: Parser Expr
cdrParser = uniParser "cdr" Cdr

charParser :: Parser Expr
charParser = do
    c <- lexeme $ char '\'' *> anyChar <* char '\''
    return (CharLit c)

stringParser :: Parser Expr
stringParser = do
    s <- lexeme $ string "''" *> manyTill anyChar (string "''")
    return $ construct s
        where construct [] = Nil
              construct (x:xs) = Cons (CharLit x) (construct xs)

variableParser :: Parser Expr
variableParser = do
    vari <- lexeme $ manyTill anyChar (char ' ')
    return (Variable $ pack vari)

data Statement
    = StatementList [Statement]
    | Set Text Expr
    | Skip
    | If Expr Statement Statement
    | While Expr Statement

statementParser :: Parser Statement
statementParser = statementListParser <|> setParser <|> skipParser <|>
                  ifParser <|> whileParser

statementListParser :: Parser Statement
statementListParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    statements <- many1 statementParser
    lexeme $ char ')'
    return (StatementList statements)

setParser :: Parser Statement
setParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    vari <- lexeme $ manyTill anyChar (char ' ')
    expr <- exprParser
    lexeme $ char ')'
    return (Set (pack vari) expr)

skipParser :: Parser Statement
skipParser = lexeme $ string "skip" $> Skip

ifParser :: Parser Statement
ifParser = do
    lexeme $ char '('
    lexeme $ string "if"
    expr <- exprParser
    statement1 <- statementParser
    statement2 <- statementParser
    lexeme $ char ')'
    return (If expr statement1 statement2)

whileParser :: Parser Statement
whileParser = do
    lexeme $ char '('
    lexeme $ string "if"
    expr <- exprParser
    statement <- statementParser
    lexeme $ char ')'
    return (While expr statement)
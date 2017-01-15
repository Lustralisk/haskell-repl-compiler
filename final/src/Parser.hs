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
(<:>) :: Monad m => m Char -> m String -> m String
(<:>) = liftM2 (:)
(<++>) :: Monad m => m String -> m String -> m String
(<++>) = liftM2 (++)

digits = many1 digit

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

testParser :: Parser Expr -> String -> Either String Expr
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

variNameParser :: Parser String
variNameParser = lexeme $ many1 $ choice [char c | c <- ['a'..'z']]

{- show util -}

showBracket :: String -> String
showBracket a = "(" ++ a ++ ")"

{- Divide declare -}

data Expr
    = BoolLit Bool
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
    | Vec Text Expr
    | Variable Text
    | Function Text [Expr]
    | Let Text Expr Expr
    | Lambda Text Expr
    | LambdaCall Expr Expr
    deriving (Eq)

instance Show Expr where
    show (BoolLit b) = show b
    show (Not e) = showBracket $ "not " ++ show e
    show (And e1 e2) = showBracket $ "and " ++ show e1 ++ " " ++ show e2
    show (Or e1 e2) = showBracket $ "or " ++ show e1 ++ " " ++ show e2
    show (Number n) = show n
    show (Add e1 e2) = showBracket $ "+ " ++ show e1 ++ " " ++ show e2
    show (Sub e1 e2) = showBracket $ "- " ++ show e1 ++ " " ++ show e2
    show (Mul e1 e2) = showBracket $ "* " ++ show e1 ++ " " ++ show e2
    show (Div e1 e2) = showBracket $ "/ " ++ show e1 ++ " " ++ show e2
    show (Eq e1 e2) = showBracket $ "= " ++ show e1 ++ " " ++ show e2
    show (Lw e1 e2) = showBracket $ "< " ++ show e1 ++ " " ++ show e2
    show (Le e1 e2) = showBracket $ "<= " ++ show e1 ++ " " ++ show e2
    show (Gr e1 e2) = showBracket $ "> " ++ show e1 ++ " " ++ show e2
    show (Ge e1 e2) = showBracket $ ">= " ++ show e1 ++ " " ++ show e2
    show Nil = "Nil"
    show (Car e) = showBracket $ "car " ++ show e
    show (Cdr e) = showBracket $ "cdr " ++ show e
    show (Cons e1 e2) = showBracket $ "cons " ++ show e1 ++ " " ++ show e2
    show (CharLit c) = show c
    show (Vec t e) = showBracket $ "vector-ref " ++ unpack t ++ " " ++ show e
    show (Variable t) = unpack t
    show (Function t es) = showBracket $ unpack t ++ f es where
        f [] = ""
        f (x:xs) = " " ++ show x ++ f xs
    show (Let t e1 e2) = showBracket $ "let " ++ unpack t ++ " " ++ show e1 ++ " " ++ show e2
    show (Lambda t e) = showBracket $ "lambda " ++ unpack t ++ " " ++ show e
    -- show (LambdaCall e e) = showBracket $ ""

exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser <|>
             floatParser <|> addParser <|> subParser <|> mulParser <|> divParser <|>
             eqParser <|> lwParser <|> leParser <|> grParser <|> geParser <|>
             charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser <|> nilParser <|>
             letParser <|> lambdaParser <|>
             vectorParser <|> functionCallParser <|> variableParser <|> lambdaCallParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> BoolLit False

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> BoolLit True

{- WARN need to strengthen -}
floatParser :: Parser Expr
floatParser = do
    d <- lexeme $ digits <++> (char '.' <:> digits) <|> digits
    return (Number $ read d)

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
        where construct = Prelude.foldr (Cons . CharLit) Nil

vectorParser :: Parser Expr
vectorParser = do
    lexeme $ char '('
    lexeme $ string "vector-ref"
    vari <- variNameParser
    expr <- exprParser
    lexeme $ char ')'
    return (Vec (pack vari) expr)

variableParser :: Parser Expr
variableParser = do
    vari <- variNameParser
    return (Variable $ pack vari)

functionCallParser :: Parser Expr
functionCallParser = do
    lexeme $ char '('
    vari <- variNameParser
    exprs <- many1 exprParser
    lexeme $ char ')'
    return (Function (pack vari) exprs)

letParser :: Parser Expr
letParser = do
    lexeme $ char '('
    lexeme $ string "let"
    vari <- variNameParser
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Let (pack vari) expr1 expr2)

lambdaParser :: Parser Expr
lambdaParser = do
    lexeme $ char '('
    lexeme $ string "lambda"
    vari <- variNameParser
    expr <- exprParser
    lexeme $ char ')'
    return (Lambda (pack vari) expr)

lambdaCallParser :: Parser Expr
lambdaCallParser = do
    lexeme $ char '('
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (LambdaCall expr1 expr2)

data Statement
    = StatementList [Statement]
    | Set Text Expr
    | Skip
    | If Expr Statement Statement
    | While Expr Statement
    | MakeVector Text Expr
    | SetVector Text Expr Expr
    | Return Expr
    deriving (Eq)

instance Show Statement where
    show (StatementList xs) = showBracket $ "begin" ++ show' xs where
        show' [] = ""
        show' (x:xs) = " " ++ show x ++ show' xs
    show (Set v e) = showBracket $ "set! " ++ unpack v ++ " " ++ show e
    show Skip = "skip"
    show (If e s1 s2) = showBracket $ "if " ++ show e ++ " " ++ show s1 ++ " " ++ show s2
    show (While e s) = showBracket $ "while " ++ show e ++ " " ++ show s
    show (MakeVector t e) = showBracket $ "make-vector " ++ unpack t ++ " " ++ show e
    show (SetVector t e1 e2) = showBracket $ "vector-set! " ++ unpack t ++ " " ++ show e1 ++ " " ++ show e2
    show (Return e) = showBracket $ "return " ++ show e

statementParser :: Parser Statement
statementParser = statementListParser <|> setParser <|> skipParser <|>
                  ifParser <|> whileParser <|> returnParser <|>
                  vectorMakeParser <|> vectorSetParser

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
    vari <- variNameParser
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
    lexeme $ string "while"
    expr <- exprParser
    statement <- statementParser
    lexeme $ char ')'
    return (While expr statement)

vectorMakeParser :: Parser Statement
vectorMakeParser = do
    lexeme $ char '('
    lexeme $ string "make-vector"
    vari <- variNameParser
    expr <- exprParser
    lexeme $ char ')'
    return (MakeVector (pack vari) expr)

vectorSetParser :: Parser Statement
vectorSetParser = do
    lexeme $ char '('
    lexeme $ string "vector-set!"
    vari <- variNameParser
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (SetVector (pack vari) expr1 expr2)

returnParser :: Parser Statement
returnParser = do
    lexeme $ char '('
    lexeme $ string "return"
    expr <- exprParser
    lexeme $ char ')'
    return (Return expr)

data Function
    = Def Text [Text] Statement
    deriving (Eq)

instance Show Function where
    show (Def t ts s) = showBracket $ "define (" ++ unpack t ++ show' ts ++ ") " ++ show s where
        show' [] = ""
        show' (x:xs) = " " ++ unpack x ++ show' xs

functionParser :: Parser Function
functionParser = defFuncParser

defFuncParser :: Parser Function
defFuncParser = do
    lexeme $ char '('
    lexeme $ string "define"
    lexeme $ char '('
    (func:varis) <- many1 variNameParser
    lexeme $ char ')'
    stat <- statementParser
    lexeme $ char ')'
    return (Def (pack func) (Prelude.map pack varis) stat)

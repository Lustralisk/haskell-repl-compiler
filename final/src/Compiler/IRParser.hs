{-# LANGUAGE OverloadedStrings #-}

module Compiler.IRParser where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Text
import Data.Attoparsec.Text

data REG = A | B | RLT
data MEM = R Integer
data IMM = IMM Double

data CMD = MOV REG REG
            | PUSH REG
            | POP REG

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p


-- cmdParser :: Parser CMD
cmdParser = undefined

regParser :: Parser REG
regParser = regAParser <|> regBParser <|> regRLTParser

memParser :: Parser MEM
memParser = do
    lexeme $ char 'R'
    addr <- decimal
    return (R addr)

immParser :: Parser IMM
immParser = do
    v <- double
    return (IMM v)

regAParser :: Parser REG
regAParser = do
    lexeme $ char 'A'
    return A

regBParser :: Parser REG
regBParser = do
    lexeme $ char 'B'
    return B

regRLTParser :: Parser REG
regRLTParser = do
    lexeme $ string "RLT"
    return RLT

movParser :: Parser CMD
movParser = do
    lexeme $ string "mov"
    space
    src <- regParser
    lexeme $ char ','
    tgt <- regParser
    return (MOV src tgt)

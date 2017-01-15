{-# LANGUAGE OverloadedStrings #-}

module Compiler.IRParser where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Text
import Data.Attoparsec.Text
import qualified Data.Vector as V


data REG = A | B | RLT
        deriving (Show)
data MEM = R Int
data IMM = IMMbool Bool
        | IMMnum Double
        | IMMchar Char
        | IMMstr Text
        | IMMVector (V.Vector IMM)
        | IMMlabel LBL
data VCC = VCC MEM Int
data FLG = T | F | Gr | Eq | Ls | Cs
    deriving (Show)
data LBL = LBLA Int
            | LBLF Text
data DST = DSTreg REG
        | DSTmem MEM
        | DSTvcc VCC
        | DSTflg FLG
data SRC = SRCreg REG
        | SRCmem MEM
        | SRCvcc VCC
        | SRCflg FLG
        | SRCimm IMM
        | SRClbl LBL
data CMD = AND DST SRC
        | OR DST SRC
        | NOT DST
        | ADD DST SRC
        | SUB DST SRC
        | MUL DST SRC
        | DIV DST SRC
        | CMP DST SRC
        | JMP SRC SRC
        | MOV DST SRC
        | HEAD DST SRC
        | TAIL DST SRC
        | CALL LBL
        | RET
        | PUSH DST
        | POP DST
        | HALT

data SEGITEM = LBLITEM LBL
        | CMDITEM CMD
        | ENDITEM
        deriving (Show)

instance Show MEM where
    show (R d) = "r" ++ show d

instance Show IMM where
    show (IMMbool b) = show b
    show (IMMnum d) = show d
    show (IMMchar c) = show c
    show (IMMstr s) = unpack s
    show (IMMlabel l) = show l

instance Show VCC where
    show (VCC m d) = show m ++ "[" ++ show d ++ "]"

instance Show LBL where
    show (LBLA d) = "$$" ++ show d
    show (LBLF t) = "$$" ++ unpack t

instance Show DST where
    show (DSTreg r) = show r
    show (DSTmem m) = show m
    show (DSTvcc v) = show v
    show (DSTflg f) = show f

instance Show SRC where
    show (SRCreg r) = show r
    show (SRCmem m) = show m
    show (SRCvcc v) = show v
    show (SRCflg f) = show f
    show (SRCimm i) = show i
    show (SRClbl l) = show l

showCmd str d s = str ++ " " ++ show d ++ " " ++ show s
showCmd' str d = str ++ " " ++ show d
instance Show CMD where
    show (AND d s) = showCmd "add" d s
    show (OR d s) = showCmd "or" d s
    show (NOT d) = showCmd' "not" d
    show (ADD d s) = showCmd "add" d s
    show (SUB d s) = showCmd "sub" d s
    show (MUL d s) = showCmd "mul" d s
    show (DIV d s) = showCmd "div" d s
    show (CMP d s) = showCmd "cmp" d s
    show (JMP d s) = showCmd "jmp" d s
    show (MOV d s) = showCmd "mov" d s
    show (HEAD d s) = showCmd "head" d s
    show (TAIL d s) = showCmd "tail" d s
    show (CALL l) = "call " ++ show l
    show RET = "ret"
    show (PUSH d) = "push " ++ show d
    show (POP d) = "pop " ++ show d

regParser :: Parser REG
regParser = rParser "A" A <|> rParser "B" B <|> rParser "rlt" RLT where
    rParser token op = do
        string token
        return op

memParser :: Parser MEM
memParser = do
    char 'r'
    addr <- decimal
    return (R addr)

immParser :: Parser IMM
immParser = iParser lblParser IMMlabel <|> bParser "True" True <|> bParser "False" False <|>
            iParser double IMMnum <|> iParser anyChar IMMchar <|>
            iParser takeText IMMstr where
                iParser parser op = do
                    v <- parser
                    return (op v)
                bParser token b = do
                    string token
                    return (IMMbool b)

vccParser :: Parser VCC
vccParser = do
    m <- memParser
    char '['
    d <- decimal
    char ']'
    return (VCC m d)

flgParser :: Parser FLG
flgParser = fParser "T" T <|> fParser "F" F <|> fParser "Gr" Gr <|>
            fParser "Eq" Eq <|> fParser "Ls" Ls <|> fParser "Cs" Cs where
                fParser token op = do
                    string token
                    return op

dstParser :: Parser DST
dstParser = dParser regParser DSTreg <|> dParser vccParser DSTvcc <|>
            dParser memParser DSTmem <|> dParser flgParser DSTflg where
                dParser parser op = do
                    v <- parser
                    return (op v)

srcParser :: Parser SRC
srcParser = sParser lblParser SRClbl <|> sParser regParser SRCreg <|> sParser vccParser SRCvcc <|>
            sParser memParser SRCmem <|> sParser immParser SRCimm <|>
            sParser flgParser SRCflg where
                sParser parser op = do
                    v <- parser
                    return (op v)

lblParser :: Parser LBL
lblParser = lParser decimal LBLA <|> lParser takeText LBLF where
    lParser parser op = do
        string "$$"
        d <- parser
        return (op d)

cmdParser :: Parser CMD
cmdParser = cParser "and" AND <|> cParser "or" OR <|> cParser "add" ADD <|> cParser "sub" SUB <|>
            cParser "mul" MUL <|> cParser "div" DIV <|> cParser "cmp" CMP <|> jmpParser <|>
            cParser "mov" MOV <|> cParser "head" HEAD <|> cParser "tail" TAIL <|> callParser <|>
            retParser <|> dParser "push" PUSH <|> dParser "pop" POP where
                cParser token op = do
                    string token
                    space
                    dst <- dstParser
                    space
                    src <- srcParser
                    return (op dst src)
                dParser token op = do
                    string token
                    space
                    dst <- dstParser
                    return (op dst)
                callParser = do
                    string "call"
                    space
                    label <- lblParser
                    return (CALL label)
                retParser = do
                    string "ret"
                    return RET
                jmpParser = do
                    string "jmp"
                    space
                    dst <- srcParser
                    space
                    src <- srcParser
                    return (JMP dst src)

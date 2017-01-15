{-# LANGUAGE OverloadedStrings #-}

module Compiler.Comp where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import qualified Data.Set as S
import Parser
import EvalT

data CmpValue = DefaultValue
           | FuncValue [Text] Statement CompTable
           | ListValue [Value]
           deriving Show

type Var2Memo = M.Map Text (Int, CmpValue)
type AvailMemo = S.Set Int
type CompTable = (Var2Memo, AvailMemo, Int)

findMemo :: Text -> CompTable -> String
findMemo t (v2m, avm, lbl) = case M.lookup t v2m of
    Just (i, v) -> "r" ++ (show i)
    _ -> "err"

updateCPT :: Text -> Int -> CmpValue -> CompTable -> CompTable
updateCPT t i v (v2m, avm, lbl) = (v2m', avm', lbl) where
    v2m' = M.insert t (i, v) (M.delete t v2m)
    avm' = S.delete i avm

injectCPT :: CompTable -> [Text] -> CompTable
injectCPT cpt ts = case ts of
    [] -> cpt
    (x:xs) -> injectCPT cpttmp xs where
        (cpttmp, _) = getMemo x DefaultValue cpt

getMemo :: Text -> CmpValue -> CompTable -> (CompTable, String)
getMemo t val (v2m, avm, lbl) = (cpt'', "r" ++ (show tmpreg))
  where
      tmpreg = S.findMin avm
      avm' = S.deleteMin avm
      cpt'' = updateCPT t tmpreg val (v2m, avm', lbl)

getVarCPT :: CompTable -> CmpValue -> [Expr] -> String
getVarCPT cpt (FuncValue t _ cptfunc) e = getVarCPT' cpt cptfunc t e

getVarCPT' :: CompTable -> CompTable -> [Text] -> [Expr] -> String
getVarCPT' cpt fcpt t e = case t of
    [] -> ""
    (tl: ts) -> lineall ++ getVarCPT' cpt fcpt ts es where
        (cpt', linee) = compExprParser cpt e'
        regtl = findMemo tl fcpt
        lineval = "push " ++ regtl ++ "\nmov " ++ regtl ++ " A\n"
        lineall = linee ++ lineval
        (e': es) = e

getPopLine :: CompTable -> [Text] -> String
getPopLine fcpt [] = ""
getPopLine fcpt (x: xs) = lineval ++ getPopLine fcpt xs where
    regtl = findMemo x fcpt
    lineval = "pop " ++ regtl ++ "\n"

binOperatorBlock :: CompTable -> Expr -> Expr -> String -> (CompTable, String)
binOperatorBlock cpt e1 e2 mem = (cpt'', code1 ++ code2 ++ code3 ++ code4 ++ code5 ++ code6)
    where (cpt', code1) = compExprParser cpt e1
          code2 = "push A\n"
          (cpt'', code3) = compExprParser cpt' e2
          code4 = "mov B A\n"
          code5 = "pop A\n"
          code6 = mem ++ " A B\n"

cmpOperatorBlock :: CompTable -> Expr -> Expr -> String -> (CompTable, String)
cmpOperatorBlock cpt e1 e2 code7 = (cpt'', code1 ++ code2 ++ code3 ++ code4 ++ code5 ++ code6 ++ code7 ++ code8)
    where (cpt', code1) = compExprParser cpt e1
          code2 = "push A\n"
          (cpt'', code3) = compExprParser cpt' e2
          code4 = "mov B A\n"
          code5 = "pop A\n"
          code6 = "cmp A B\n"
          code8 = "mov A Cs\n"

compExprParser :: CompTable -> Expr -> (CompTable, String)
compExprParser (v2m, avm, lbl) (Variable t) = case M.lookup t v2m of
    Just (i, v) -> ((v2m, avm, lbl), "mov A " ++ "r" ++ (show i) ++ "\n")
    _ -> ((v2m, avm, lbl), "halt\n")
compExprParser cpt (Number n) = (cpt, "mov A " ++ (show n) ++ "\n")
compExprParser cpt (CharLit c) = (cpt, "mov A \'" ++ "c" ++ "\'\n")
compExprParser cpt (BoolLit False) = (cpt, "mov A False" ++ "\n")
compExprParser cpt (BoolLit True) = (cpt, "mov A True" ++ "\n")
compExprParser cpt (Add (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 + n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Add e1 e2) = binOperatorBlock cpt e1 e2 "add"
compExprParser cpt (Sub (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 - n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Sub e1 e2) = binOperatorBlock cpt e1 e2 "sub"
compExprParser cpt (Mul (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 - n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Mul e1 e2) = binOperatorBlock cpt e1 e2 "mul"
compExprParser cpt (Div (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 / n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Div e1 e2) = binOperatorBlock cpt e1 e2 "div"
compExprParser cpt (And (BoolLit n1) (BoolLit n2)) = (cpt, line3) where
    n3 = n1 && n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (And e1 e2) = binOperatorBlock cpt e1 e2 "and"
compExprParser cpt (Or (BoolLit n1) (BoolLit n2)) = (cpt, line3) where
    n3 = n1 || n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Or e1 e2) = binOperatorBlock cpt e1 e2 "or"
compExprParser cpt (Not (BoolLit n1)) = (cpt, line3) where
    n3 = not n1
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Not e) = (cpt', code1 ++ code2)
    where (cpt', code1) = compExprParser cpt e
          code2 = "not A\n"
compExprParser cpt (Eq (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 == n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Eq e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Eq\n"
compExprParser cpt (Lw (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 < n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Lw e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Lw\n"
compExprParser cpt (Gr (Number n1) (Number n2)) = (cpt, line3) where
    n3 = n1 > n2
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Gr e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Gr\n"
compExprParser cpt (Le (Number n1) (Number n2)) = (cpt, line3) where
    n3 = (n1 <= n2)
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Le e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Lw\nor Cs Eq\n"
compExprParser cpt (Ge (Number n1) (Number n2)) = (cpt, line3) where
    n3 = (n1 >= n2)
    line3 = "mov A " ++ (show n3) ++ "\n"
compExprParser cpt (Ge e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Gr\nor Cs Eq\n"
compExprParser (v2m, avm, lbl) (Function fname es) = case M.lookup fname v2m of
    Just (_, funcv) -> ((v2m, avm, lbl), lineall) where
        lineassig = getVarCPT (v2m, avm, lbl) funcv es
        linecall = "call $$" ++ (unpack fname) ++ "\nmov A rlt\n"
        lineres = getPopLine fcpt (Prelude.reverse (t))
        lineall = lineassig ++ linecall ++ lineres
        (FuncValue t s fcpt) = funcv
    _ -> ((v2m, avm, lbl), "halt\n")
compExprParser cpt (Let t e1 e2) = ((v2m, avm'', lbl''), linefinal) where
    (cpt', linee1) = compExprParser cpt e1
    (cpt'', reg) = getMemo t DefaultValue cpt'
    lineval = "mov " ++ reg ++ " A\n"
    (cpt''', linee2) = compExprParser cpt'' e1
    linefinal = linee1 ++ lineval ++ linee2
    (v2m, avm, lbl) = cpt
    (v2m'', avm'', lbl'') = cpt'''
compExprParser cpt (Lambda t e) = (cptfinal, linev ++ linefunc ++ lineret ++ lineend) where
    (cpt', reg) = getMemo t DefaultValue cpt
    linev = "$$Lambda" ++ (show lbl') ++ "\n"
    (cpt'' ,linefunc) = compExprParser (v2m', avm', lbl' + 1) e
    lineret = "move rlt A\nret\n"
    lineend = "mov A " ++ linev ++ "\n"
    (v2m, avm, lbl) = cpt
    (v2m', avm', lbl') = cpt'
    (v2m'', avm'', lbl'') = cpt''
    cpttmp = (v2m, avm'', lbl'')
    (cptfinal, _) = getMemo (pack "A") (FuncValue [t] (Return e) cpt'') cpttmp


compStatParser :: CompTable -> Statement -> (CompTable, String)
compStatParser cpt (StatementList s) = case s of
    [] -> (cpt, "")
    (x:xs) -> case x of
        (Return e) -> (cpt, line' ++ "mov rlt A\n") where
            (cpt', line') = (compExprParser cpt e)
        _ -> (cpt'', line' ++ line'') where
            (cpt', line') = compStatParser cpt x
            (cpt'', line'') = compStatParser cpt' (StatementList xs)
compStatParser cpt (Set t e) = ((v2m'', avm', lbl'), line1 ++ "mov " ++ reg ++ " A\n")
    where
        (cpt', line1) = compExprParser cpt e
        regtmp = findMemo t cpt'
        (v2m, avm, lbl) = cpt'
        Just (num, val) = case M.lookup (pack "A") v2m of
            Nothing -> Just (-1, DefaultValue)
            _ -> M.lookup (pack "A") v2m
        (cpt'', reg) = if regtmp == "err" then getMemo t val cpt' else (cpt', regtmp)
        (v2m', avm', lbl') = cpt''
        Just (num2 , val2) = M.lookup (pack reg) v2m'
        v2m'' = M.insert (pack reg) (num2, val) v2m'
compStatParser cpt Skip = (cpt, "")
compStatParser (v2m, avm, lbl) (If e s1 s2) = (cpt''', linecond ++ jmpthen ++ lineelse ++ jmpend ++ labelthen ++ "\n" ++ linethen ++ labelend ++ "\n")
    where
        lbl' = lbl + 1
        lbl'' = lbl + 2
        (cpt', linecond) = compExprParser (v2m, avm, lbl'') e
        jmpthen = "jmp " ++ labelthen ++ " A\n"
        (cpt'', lineelse) = compStatParser (v2m, avm, lbl'') s2
        jmpend = "jmp " ++ labelend ++ " True\n"
        labelthen = "$$" ++ (show lbl)
        (cpt''', linethen) = compStatParser cpt'' s1
        labelend = "$$" ++ (show lbl')
compStatParser (v2m, avm, lbl) (While e s) = (cpt', labelbegin ++ "\n" ++ linecond ++ jmpend ++ linethen ++ jmpbegin ++ labelend ++ "\n")
    where
        lbl' = lbl + 1
        lbl'' = lbl + 2
        labelbegin = "$$" ++ (show lbl)
        (cpt, linecond) = compExprParser (v2m, avm, lbl'') e
        jmpend = "not A\njmp " ++ labelend  ++ " A\n"
        (cpt', linethen) = compStatParser (v2m, avm, lbl'') s
        jmpbegin = "jmp " ++ labelbegin ++ " True\n"
        labelend = "$$" ++ (show lbl')
compStatParser cpt (MakeVector t e) = (cpt', linelength ++ linefinal) where
    tmpmemo = findMemo t cpt
    (cpt', memo) = if tmpmemo == "err"
        then getMemo t (DefaultValue) cpt
        else (cpt, tmpmemo)
    (cpt'', linelength) = compExprParser cpt e
    linefinal = "mov " ++ memo ++ " vec[A]\n"
compStatParser cpt (SetVector t e1 e2) = case findMemo t cpt of
    "err" -> (cpt, "halt\n")
    memo -> (cpt, linepos ++ linesave ++ lineval ++ linefinal) where
        (cpt', linepos) = compExprParser cpt e1
        linesave = "push A\n"
        (cpt'', lineval) = compExprParser cpt' e2
        linefinal = "pop B\nmov " ++ memo ++"[B] A\n"
compStatParser cpt (Return e) = (cpt', lineexpr ++ linefinal) where
    (cpt', lineexpr) = compExprParser cpt e
    linefinal = "mov rlt A\n"

compFunctionParser :: CompTable -> Function -> (CompTable, String)
compFunctionParser cpt (Def t ts stat) = (cpt', linefunc) where
    (v2m, avm, lbl) = cpt
    (v2m', avm', lbl') = injectCPT cpt ts
    tmpcpt = (v2m', avm', lbl')
    (reccpt, _) = getMemo t (FuncValue ts stat reccpt) tmpcpt
    (v2m'', u1, u2) = reccpt
    ((v2m''', avm''', lbl'''), linebody) = compStatParser reccpt stat
    cpt' = (v2m'', avm''', lbl''')
    lineTag = "$$" ++ (unpack t) ++ "\n"
    linefinal = "ret\n"
    linefunc = lineTag ++ linebody ++ linefinal

compExpr :: CompTable -> String -> (CompTable, String)
compExpr cpt t = let (Right expr) = (parseOnly exprParser (pack t)) in compExprParser cpt expr

compStat :: CompTable -> String -> (CompTable, String)
compStat cpt t = compStatParser cpt statement where
    (Right statement) = parseOnly statementParser $ pack t

comp :: CompTable -> String -> (CompTable, String)
comp cpt line = case parseOnly functionParser $ pack line of
    (Right function) -> compFunctionParser cpt function
    _ -> case parseOnly statementParser $ pack line of
        (Right statement) -> (compStatParser cpt statement)
        _ -> compExpr cpt line

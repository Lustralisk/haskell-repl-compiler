{-# LANGUAGE OverloadedStrings #-}

module CompExec where

import Data.Text
import Eval
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import qualified Data.Set as S
import Parser

data CmpValue = DefaultValue
           | FunctionValue [Text] Statement CompTable
           | ListValue [Value]
           deriving Show

type Var2Memo = M.Map Text (Int, CmpValue)
type AvailMemo = S.Set Int
type CompTable = (Var2Memo, AvailMemo, Int)

findMemo :: Text -> CompTable -> [Char]
findMemo t (v2m, avm, lbl) = case M.lookup t v2m of
    Just (i, v) -> "r" ++ (show i)
    _ -> "err"

updateCPT :: Text -> Int -> CmpValue -> CompTable -> CompTable
updateCPT t i v (v2m, avm, lbl) = (v2m', avm', lbl) where
  v2m' = M.insert t (i, v) (M.delete t v2m)
  avm' = S.delete i avm

injectCPT :: CompTable -> CompTable -> [Text] -> [Int] -> CompTable
injectCPT cpt' cpt ts is = case ts of
    [] -> cpt'
    (x:xs) -> injectCPT (updateCPT x y (DefaultValue) cpt') cpt xs ys where
        (y:ys) = is

getMemo :: Text -> CmpValue -> CompTable -> ([Char], CompTable)
getMemo t val (v2m, avm, lbl) = ("r" ++ (show tmpreg), cpt'')
  where
      tmpreg = S.findMin avm
      avm' = S.deleteMin avm
      cpt'' = updateCPT t tmpreg val (v2m, avm', lbl)

binOperatorBlock :: CompTable -> Expr -> Expr -> [Char] -> [Char]
binOperatorBlock cpt e1 e2 mem = code1 ++ code2 ++ code3 ++ code4 ++ code5 ++ code6
    where code1 = compExprParser cpt e1
          code2 = "push A\n"
          code3 = compExprParser cpt e2
          code4 = "mov B A\n"
          code5 = "pop A\n"
          code6 = mem ++ " A B\n"

cmpOperatorBlock :: CompTable -> Expr -> Expr -> [Char] -> [Char]
cmpOperatorBlock cpt e1 e2 code7 = code1 ++ code2 ++ code3 ++ code4 ++ code5 ++ code6 ++ code7 ++ code8
    where code1 = compExprParser cpt e1
          code2 = "push A\n"
          code3 = compExprParser cpt e2
          code4 = "mov B A\n"
          code5 = "pop A\n"
          code6 = "cmp A B\n"
          code8 = "mov A Cs\n"

compExprParser :: CompTable -> Expr -> [Char]
compExprParser (v2m, avm, lbl) (Variable t) = case M.lookup t v2m of
    Just (i, v) -> "mov A " ++ "r" ++ (show i) ++ "\n"
    _ -> "halt\n"
compExprParser _ (Number n) = "mov A " ++ (show n) ++ "\n"
compExprParser _ (CharLit c) = "mov A \'" ++ "c" ++ "\'\n"
compExprParser _ FalseLit = "mov A False" ++ "\n"
compExprParser _ TrueLit = "mov A True" ++ "\n"
compExprParser cpt (Add e1 e2) = binOperatorBlock cpt e1 e2 "add"
compExprParser cpt (Sub e1 e2) = binOperatorBlock cpt e1 e2 "sub"
compExprParser cpt (Mul e1 e2) = binOperatorBlock cpt e1 e2 "mul"
compExprParser cpt (Div e1 e2) = binOperatorBlock cpt e1 e2 "div"
compExprParser cpt (And e1 e2) = binOperatorBlock cpt e1 e2 "and"
compExprParser cpt (Or e1 e2) = binOperatorBlock cpt e1 e2 "or"
compExprParser cpt (Not e) = code1 ++ code2
    where code1 = compExprParser cpt e
          code2 = "not A\n"
compExprParser cpt (Eq e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Eq\n"
compExprParser cpt (Lw e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Lw\n"
compExprParser cpt (Gr e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Gr\n"
compExprParser cpt (Le e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Lw\nor Cs Eq\n"
compExprParser cpt (Ge e1 e2) = cmpOperatorBlock cpt e1 e2 "mov Cs Gr\nor Cs Eq\n"

compStatParser :: CompTable -> Statement -> (CompTable, [Char])
compStatParser cpt (StatementList s) = case s of
    [] -> (cpt, "")
    (x:xs) -> case x of
        (Return e) -> (cpt, line' ++ "mov rlt A") where
          line' = (compExprParser cpt e)
        _ -> (cpt'', line' ++ line'') where
            (cpt', line') = compStatParser cpt x
            (cpt'', line'') = compStatParser cpt' (StatementList xs)
compStatParser cpt (Set t e) = (cpt', line1 ++ "mov " ++ reg ++ " A\n")
    where
        line1 = compExprParser cpt e
        regtmp = findMemo t cpt
        (reg, cpt') = if regtmp == "err" then getMemo t (DefaultValue) cpt else (regtmp, cpt)
compStatParser cpt Skip = (cpt, "")
compStatParser (v2m, avm, lbl) (If e s1 s2) = (cpt'', linecond ++ jmpthen ++ lineelse ++ jmpend ++ labelthen ++ "\n" ++ linethen ++ labelend ++ "\n")
    where
        lbl' = lbl + 1
        lbl'' = lbl + 2
        linecond = compExprParser (v2m, avm, lbl'') e
        jmpthen = "jmp " ++ labelthen ++ " A\n"
        (cpt', lineelse) = compStatParser (v2m, avm, lbl'') s2
        jmpend = "jmp " ++ labelend ++ " 1\n"
        labelthen = "$$" ++ (show lbl)
        (cpt'', linethen) = compStatParser cpt' s1
        labelend = "$$" ++ (show lbl')
compStatParser (v2m, avm, lbl) (While e s) = (cpt', labelbegin ++ "\n" ++ linecond ++ jmpend ++ linethen ++ jmpbegin ++ labelend ++ "\n")
    where
        lbl' = lbl + 1
        lbl'' = lbl + 2
        labelbegin = "$$" ++ (show lbl)
        linecond = compExprParser (v2m, avm, lbl'') e
        jmpend = "not A\njmp " ++ labelend  ++ " A\n"
        (cpt', linethen) = compStatParser (v2m, avm, lbl'') s
        jmpbegin = "jmp " ++ labelbegin ++ " 1\n"
        labelend = "$$" ++ (show lbl')
compStatParser cpt (MakeVector t e) = (cpt, linelength ++ linefinal) where
    tmpmemo = findMemo t cpt
    (memo, cpt') = if tmpmemo == "err"
      then getMemo t DefaultValue cpt
      else (tmpmemo, cpt)
    linelength = compExprParser cpt e
    linefinal = "mov " ++ memo ++ "vec[A]\n"
compStatParser cpt (SetVector t e1 e2) = case findMemo t cpt of
    "err" -> (cpt, "halt\n")
    memo -> (cpt, linepos ++ linesave ++ lineval ++ linefinal) where
        linepos = compExprParser cpt e1
        linesave = "push A\n"
        lineval = compExprParser cpt e2
        linefinal = "pop B\nmov memo[B] A\n"
compStatParser cpt (Return e) = (cpt, lineexpr ++ linefinal) where
    lineexpr = compExprParser cpt e
    linefinal = "mov rlt A\n"


compExpr :: CompTable -> [Char] -> [Char]
compExpr cpt t = let (Right expr) = (parseOnly exprParser (pack t)) in compExprParser cpt expr

compStat :: CompTable -> [Char] -> (CompTable, [Char])
compStat cpt t = compStatParser cpt statement where
    (Right statement) = parseOnly statementParser $ pack t


comp :: CompTable -> [Char] -> (CompTable, [Char])
comp cpt line = case parseOnly functionParser $ pack line of
    _ -> (cpt, compExpr cpt line)

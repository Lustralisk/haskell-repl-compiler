{-# LANGUAGE OverloadedStrings #-}

module CompExec where

import Data.Text
import Eval
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import qualified Data.Set as S
import Parser

type Var2Reg = M.Map Text Int
type GlobalVar2Reg = M.Map Text Int
type AvailableReg = S.Set Int
type CompTable = (Var2Reg, GlobalVar2Reg, AvailableReg)

findCTReg :: Text -> CompTable -> [Char]
findCTReg t (v2r, gv2r, avr) = case M.lookup t v2r of
    Just v -> "r" ++ (show v)
    _ -> case M.lookup t gv2r of
        Just w -> "r" ++ (show w)
        _ -> "err"

getReg :: Text -> CompTable -> ([Char], CompTable)
getReg t (v2r, gv2r, avr) = ("r" ++ (show tmpreg), (v2r, gv2r, avr))
  where
      isGlobal = M.null v2r
      tmpreg = S.findMin avr
      avr' = S.deleteMin avr
      (v2r', gv2r') = if isGlobal
        then (v2r, (M.insert t tmpreg gv2r))
        else ((M.insert t tmpreg v2r), gv2r)



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
compExprParser (v2r, gv2r, avr) (Variable t) = case M.lookup t v2r of
    Just v -> "mov A " ++ "r" ++ (show v) ++ "\n"
    _ -> case M.lookup t gv2r of
        Just w -> "mov A " ++ "r" ++ (show w) ++ "\n"
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

compStatParser :: CompTable -> Statement -> [Char] -> (CompTable, [Char])
-- compStatParser cpt (StatementList s) line = case s of
--     [] -> (cpt, "")
--     (x:xs) -> case x of
--         (Return e) -> updateM "$$result$$" (evalExprParser env e) env
--         _ -> compStatParser env' (StatementList xs) where
--             env' = evalStatementParser env x
compStatParser cpt (Set t e) line = (cpt', line ++ code1 ++ "mov " ++ reg ++ " A\n")
    where code1 = (compExprParser cpt e)
          regtmp = findCTReg t cpt
          (reg, cpt') = if regtmp == "err"
            then getReg t cpt
            else (regtmp, cpt)
compStatParser cpt Skip line = (cpt, line)

compExpr :: CompTable -> [Char] -> [Char]
compExpr cpt t = let (Right expr) = (parseOnly exprParser (pack t)) in compExprParser cpt expr


comp :: CompTable -> [Char] -> (CompTable, [Char])
comp cpt line = case parseOnly functionParser $ pack line of
    _ -> (cpt, compExpr cpt line)

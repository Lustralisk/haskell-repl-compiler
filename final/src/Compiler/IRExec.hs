{-# LANGUAGE OverloadedStrings #-}

module Compiler.IRExec where

import Data.Text
import System.IO
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Compiler.IRParser
import Compiler.IR

execWrapper :: String -> IO()
execWrapper inp = do
    inh <- openFile inp ReadMode
    (initEnv, codeSeg) <- initialEnvLoop inh (M.empty, M.empty, [], 0, 0, []) [ENDITEM]
    -- let x = execIRLoop initEnv codeSeg
    hClose inh

initialEnvLoop :: Handle -> VMEnv -> CodeSeg -> IO ((VMEnv, CodeSeg))
initialEnvLoop inh env seg = do
    isEof <- hIsEOF inh
    if isEof
        then return (env, seg)
    else do
        curline <- hGetLine inh
        let (env', seg') = envParse env seg curline in initialEnvLoop inh env' seg'

envParse :: VMEnv -> CodeSeg -> String -> (VMEnv, CodeSeg)
envParse env seg line = case parseOnly lblParser $ pack line of
    (Right lbl) -> (env', seg ++ [(LBLITEM lbl)]) where
        (mt, lblt, vs, layer, eip, cstk) = env
        curindex = (Prelude.length seg)
        lblt' = M.insert (show lbl) curindex lblt
        eip' = if (show lbl) == "$$main" then curindex else eip
        env' = (mt, lblt', vs, layer, eip', cstk)
    _  -> case parseOnly cmdParser $ pack line of
        (Right cmd) -> (env, seg ++ [(CMDITEM cmd)])
        _ -> (env, seg)

execIRLoop :: VMEnv -> CodeSeg -> VMEnv
execIRLoop (mt, lblt, vs, layer, eip, cstk) seg = case seg !! eip of
    (LBLITEM label) -> execIRLoop (mt, lblt, vs, layer, eip + 1, cstk) seg
    (CMDITEM cmd) -> execIRLoop env' seg where
        env' = execCMD cmd (mt, lblt, vs, layer, eip, cstk)
    _ -> (mt, lblt, vs, layer, eip, cstk)

execCMD :: CMD -> VMEnv -> VMEnv
execCMD (ADD dst src) env = (updateEIP env') where
     IMMnum v1 = execDST dst env
     IMMnum v2 = execSRC src env
     IMMnum v3 = IMMnum (v1 + v2)
     env' = updateDST dst (IMMnum v3) env
execCMD (SUB dst src) env = (updateEIP env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    IMMnum v3 = IMMnum (v1 - v2)
    env' = updateDST dst (IMMnum v3) env
execCMD (DIV dst src) env = (updateEIP env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    IMMnum v3 = IMMnum (v1 / v2)
    env' = updateDST dst (IMMnum v3) env
execCMD (MUL dst src) env = (updateEIP env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    IMMnum v3 = IMMnum (v1 * v2)
    env' = updateDST dst (IMMnum v3) env
execCMD (MOV dst src) env = (updateEIP env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    env' = updateDST dst (IMMnum v2) env
execCMD (AND dst src) env = (updateEIP env') where
    IMMbool v1 = execDST dst env
    IMMbool v2 = execSRC src env
    IMMbool v3 = IMMbool (v1 && v2)
    env' = updateDST dst (IMMbool v3) env
execCMD (OR dst src) env = (updateEIP env') where
    IMMbool v1 = execDST dst env
    IMMbool v2 = execSRC src env
    IMMbool v3 = IMMbool (v1 || v2)
    env' = updateDST dst (IMMbool v3) env
execCMD (NOT dst) env = (updateEIP env') where
    IMMbool v1 = execDST dst env
    IMMbool v3 = IMMbool (not v1)
    env' = updateDST dst (IMMbool v3) env
execCMD (CMP dst src) env = (updateEIP env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    eq = v1 == v2
    lw = v1 < v2
    gr = v1 > v2
    (mt, lblt, vs, layer, eip, cstk) = env
    mt' = M.insert "Gr" (IMMbool gr) (M.insert "Lw" (IMMbool lw) (M.insert "Eq" (IMMbool eq) mt))
    env' = (mt', lblt, vs, layer, eip, cstk)
execCMD (JMP dst src) env = env' where
    IMMnum v1 = execDST dst env
    lbl = show dst
    IMMbool v2 = execSRC src env
    (mt, lblt, vs, layer, eip, cstk) = env
    Just eip' = if v2 then (M.lookup lbl lblt) else (Just (eip + 1))
    env' = (mt, lblt, vs, layer, eip', cstk)
-- execCMD (PUSH src) env = (updateEIP env') where
--     v2 = execSRC src env
--     (mt, lblt, vs, layer, eip, cstk) = env
--     vs' = v2 : vs
--     env' = (mt, lblt, vs', layer, eip, cstk)
-- execCMD (POP dst) env = (updateEIP env') where
--     (mt, lblt, vs, layer, eip, cstk) = env
--     (x: xs) = vs
--     (mt', lblt', vs', layer', eip', cstk') = updateDST dst x
--     (x', xs') = vs'
--     env' = (mt', lblt', xs', layer', eip', cstk')


execSRC :: SRC -> VMEnv -> IMM
execSRC (SRCimm e) env =  e
execSRC (SRCvcc (VCC memo d)) env = list V.! d where
    (mt, lblt, vs, layer, eip, cstk) = env
    Just (IMMVector list) = M.lookup (show memo) mt
execSRC a env = imm where
    (mt, lblt, vs, layer, eip, cstk) = env
    Just imm = M.lookup (show a) mt

execDST :: DST -> VMEnv -> IMM
execDST (DSTvcc (VCC memo d)) env = list V.! d where
    (mt, lblt, vs, layer, eip, cstk) = env
    Just (IMMVector list) = M.lookup (show memo) mt
execDST a env = imm where
    (mt, lblt, vs, layer, eip, cstk) = env
    Just imm = M.lookup (show a) mt

updateDST :: DST -> IMM -> VMEnv -> VMEnv
updateDST (DSTvcc (VCC memo d)) imm env = env' where
    (mt, lblt, vs, layer, eip, cstk) = env
    env' = (mt', lblt, vs, layer, eip, cstk)
    Just (IMMVector list) = M.lookup (show memo) mt
    Just (IMMnum index) = M.lookup "B" mt
    list' = list V.// [(fromEnum index, imm)]
    mt' = M.insert (show memo) (IMMVector list') mt
updateDST a imm env = env' where
    (mt, lblt, vs, layer, eip, cstk) = env
    env' = (mt', lblt, vs, layer, eip, cstk)
    mt' =  M.insert (show a) imm mt

updateEIP :: VMEnv -> VMEnv
updateEIP (mt, lblt, vs, layer, eip, cstk) = (mt, lblt, vs, layer, eip + 1, cstk)

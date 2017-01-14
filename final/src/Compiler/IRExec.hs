{-# LANGUAGE OverloadedStrings #-}

module Compiler.CompExec where

import Data.Text
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Map as M
import qualified Data.Set as S
import Compiler.IRParser

execWrapper :: [Char] -> VMEnv
buildInitialEnv inp oup = do
    inh <- openFile inp ReadMode
    (initEnv, codeSeg) <- initialEnvLoop inh  (M.empty, M.empty, [], 0, 0, []) [ENDITEM]
    let x = execIRLoop initEnv codeSeg
    print x
    hClose inh

initialEnvLoop :: Handle -> VMEnv -> CodeSeg -> IO ()
initialEnvLoop inh env seg = do
    isEof <- hIsEOF inh
    if isEof
        then return (env, seg)
    else do
        line <- hGetLine inh
        initialEnvLoop inh env' seg' where
            (env', seg') = envParse env seg line

execIRLoop :: VMEnv -> CodeSeg -> VMEnv
execIRLoop (mt, lblt, vs, layer, eip, cstk) seg = case seg !! eip of
    (LBLITEM label) -> execIRLoop (mt, lblt, vs, layer, eip + 1, cstk) seg
    (CMDITEM cmd) -> execIRLoop env' seg where
        env' = execCMD cmd (mt, lblt, vs, layer, eip, cstk)
    otherwise -> (mt, lblt, vs, layer, eip, cstk)

execCMD :: CMD -> VMEnv -> VMEnv
execCMD (ADD dst src) env = updateEIP(env') where
     IMMnum v1 = execDST dst env
     IMMnum v2 = execSRC src env
     IMMnum v3 = IMMnum (v1 + v2)
     env' = updateDST dst v3
execCMD (SUB dst src) env = updateEIP(env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    IMMnum v3 = IMMnum (v1 - v2)
    env' = updateDST dst v3
execCMD (DIV dst src) env = updateEIP(env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    IMMnum v3 = IMMnum (v1 / v2)
    env' = updateDST dst v3
execCMD (MUL dst src) env = updateEIP(env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    IMMnum v3 = IMMnum (v1 * v2)
    env' = updateDST dst v3
execCMD (MOV dst src) env = updateEIP(env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    env' = updateDST dst v2
execCMD (AND dst src) env = updateEIP(env') where
    IMMbool v1 = execDST dst env
    IMMbool v2 = execSRC src env
    IMMbool v3 = IMMbool (v1 && v2)
    env' = updateDST dst v3
execCMD (OR dst src) env = updateEIP(env') where
    IMMbool v1 = execDST dst env
    IMMbool v2 = execSRC src env
    IMMbool v3 = IMMbool (v1 || v2)
    env' = updateDST dst v3
execCMD (NOT dst) env = updateEIP(env') where
    IMMbool v1 = execDST dst env
    IMMbool v3 = IMMbool (not v1)
    env' = updateDST dst v3
execCMD (CMP dst src) env = updateEIP(env') where
    IMMnum v1 = execDST dst env
    IMMnum v2 = execSRC src env
    eq = (v1 == v2)
    lw = (v1 < v2)
    gr = (v1 > v2)
    (mt, lblt, vs, layer, eip, cstk) = env
    mt' = M.insert "Gr" gr (M.insert "Lw" lw (M.insert "Eq" (IMMbool eq)))
    env' = (mt', lblt, vs, layer, eip, cstk)
execCMD (JMP dst src) env = env' where
    IMMnum v1 = execDST dst env
    lbl = (show dst)
    IMMbool v2 = execSRC src env
    (mt, lblt, vs, layer, eip, cstk) = env
    eip' = if v2 then fromJust (M.lookup lbl lblt) else eip + 1
    env' = (mt, lblt, vs, layer, eip', cstk)
execCMD (PUSH src) env = updateEIP(env') where
    v2 = execSRC src env
    (mt, lblt, vs, layer, eip, cstk) = env
    vs' = v2 : vs
    env' = (mt, lblt, vs', layer, eip, cstk)
execCMD (POP dst) env = updateEIP(env')
    (mt, lblt, vs, layer, eip, cstk) = env
    (x: xs) = vs
    (mt', lblt', vs', layer', eip', cstk') = updateDST dst x
    (x', xs') = vs'
    env' = (mt', lblt', xs', layer', eip', cstk')


execSRC :: SRC -> VMEnv -> IMM
execSRC (SRCimm e) env =  e
execSRC (SRCvcc (VCC memo d)) env = list ! d where
    (mt, lblt, vs, layer, eip, cstk) = env
    IMMVector list = M.lookup memo mt
execSRC a env = imm where
    (mt, lblt, vs, layer, eip, cstk) = env
    imm = M.lookup (show a) mt

execDST :: DST -> VMEnv -> IMM
execDST (DSTvcc (VCC memo d)) env = list ! d where
    (mt, rt, ft, lblt, vt, layer, eip, cstk) = env
    IMMVector list = M.lookup （show memo） mt
execDST a env = imm where
    (mt, rt, ft, lblt, vt, layer, eip, cstk) = env
    imm = M.lookup (show a) mt

updateDST :: DST -> IMM -> VMEnv -> VMEnv
updateDST (DSTvcc (VCC memo d)) imm env = env' where
    (mt, rt, ft, lblt, vt, layer, eip, cstk) = env
    env' = (mt', rt, ft, lblt, vt, layer, eip, cstk)
    IMMVector list = M.lookup （show memo） mt
    IMMnum index = M.lookup "B" mt
    list' = list // [(index, imm)]
    mt' = M.insert (show memo）list' mt
updateDST a imm env = env' where
    (mt, rt, ft, lblt, vt, layer, eip, cstk) = env
    env' = (mt', rt, ft, lblt, vt, layer, eip, cstk)
    mt' =  M.insert (show memo) imm mt

updateEIP :: VMEnv -> VMEnv
updateEIP (mt, rt, ft, lblt, vt, layer, eip, cstk) = (mt, rt, ft, lblt, vt, layer, eip + 1, cstk)

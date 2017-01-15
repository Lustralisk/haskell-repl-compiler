{-# LANGUAGE OverloadedStrings #-}

module Compiler.IRExec where

import Data.Text
import System.IO
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad
import Compiler.IRParser
import Compiler.IR

type ExecVM a = StateT (VMEnv, CodeSeg) IO a

io :: IO a -> StateT (VMEnv, CodeSeg) IO a
io = liftIO

execWrapper :: String -> IO ()
execWrapper inp = do
    inh <- openFile inp ReadMode
    --(initEnv, codeSeg) <- runStateT initialEnvLoop inh ((M.empty, M.empty, [], 0, 0, []) [ENDITEM])
    (_, (initEnv, codeSeg)) <- runStateT (initialEnvLoop inh) ((M.empty, M.empty, [], 1, 0, []), [])
    print (initEnv, codeSeg)
    let initEnv' = initMTable initEnv in print (execIRLoop initEnv' codeSeg)
    -- print (initEnv', codeSeg)
    hClose inh

initialEnvLoop :: Handle -> ExecVM ()
initialEnvLoop inh = do
    (env, seg) <- get
    isEof <- io $ hIsEOF inh
    if isEof
    then do
        put (env, seg)
        return ()
    else do
        curline <- io $ hGetLine inh
        put $ envParse env seg curline
        initialEnvLoop inh

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
execIRLoop env@(mt, lblt, vs, layer, eip, cstk) seg
    | layer == 0 = (mt, lblt, vs, layer, eip, cstk)
    | eip >= Prelude.length seg = env
    | otherwise = case seg !! eip of
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
    v1 = execDST dst env
    v2 = execSRC src env
    env' = updateDST dst v2 env
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
    (IMMlabel v1) = execSRC dst env
    lbl = show v1
    IMMbool v2 = execSRC src env
    (mt, lblt, vs, layer, eip, cstk) = env
    Just eip' = if v2 then (M.lookup lbl lblt) else (Just (eip + 1))
    env' = (mt, lblt, vs, layer, eip', cstk)
execCMD (CALL lbl) env = env' where
    (mt, lblt, vs, layer, eip, cstk) = env
    layer' = layer + 1
    cstk' = (eip + 1):cstk
    Just eip' = M.lookup (show lbl) lblt
    env' = (mt, lblt, vs, layer', eip', cstk')
execCMD RET env = env' where
    (mt, lblt, vs, layer, eip, cstk) = env
    (x, xs) = case cstk of
        [] -> (eip, [])
        (y:ys) -> (y, ys)
    layer' = layer - 1
    env' = (mt, lblt, vs, layer', x, xs)
execCMD (PUSH dst) env = (updateEIP env') where
    v2 = execDST dst env
    (mt, lblt, vs, layer, eip, cstk) = env
    vs' = v2 : vs
    env' = (mt, lblt, vs', layer, eip, cstk)
execCMD (POP dst) env = (updateEIP env') where
    (mt, lblt, vs, layer, eip, cstk) = env
    (x: xs) = vs
    (mt', lblt', vs', layer', eip', cstk') = updateDST dst x env
    (x': xs') = vs'
    env' = (mt', lblt', xs', layer', eip', cstk')


execSRC :: SRC -> VMEnv -> IMM
execSRC (SRCimm e) env = e
execSRC (SRCvcc (VCC memo d)) env = list V.! d where
    (mt, lblt, vs, layer, eip, cstk) = env
    Just (IMMVector list) = M.lookup (show memo) mt
execSRC (SRClbl l) env = IMMlabel l
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

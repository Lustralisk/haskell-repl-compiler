{-# LANGUAGE OverloadedStrings #-}

module Compiler.IR where

import Control.Applicative
import Data.Functor
import Data.Either
import System.IO
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import System.IO
import Compiler.IRParser

type MTable = M.Map String IMM
type LblTable = M.Map String Int
type ValueStack = [IMM]
type EIP = Int
type Layer = Int
type CodeSeg = [SEGITEM]
type CallStack = [Int]

type VMEnv = (MTable, LblTable, ValueStack, Layer, EIP, CallStack)

initMTable :: VMEnv -> VMEnv
initMTable (mt, lblt, vs, layer, eip, cstk) = (mt', lblt, vs, layer, eip, cstk) where
    listMem1 = ["Eq", "Lw", "Gr", "Cs", "T", "F", "A", "B", "rlt"]
    listMem2 = ["r" ++ (show c) | c <- [0, 1 .. 255]]
    listMem3 = listMem1 ++ listMem2
    mt' = foldl insertMT M.empty listMem3

insertMT :: MTable -> String -> MTable
insertMT mt str = M.insert str (IMMnum 0) mt

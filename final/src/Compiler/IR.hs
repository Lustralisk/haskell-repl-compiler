{-# LANGUAGE OverloadedStrings #-}

module Compiler.IR where

import Control.Applicative
import Data.Functor
import Data.Either
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import System.IO
import Parser
import Compiler.CompExec

type MTable = M.Map String IMM
type LblTable = M.Map String Int
type ValueStack = [IMM]
type EIP = Int
type Layer = Int
type CodeSeg = [SEGITEM]
type CallStack = [Int]

type VMEnv = (MTable, LblTable, ValueStack, Layer, EIP, CallStack)

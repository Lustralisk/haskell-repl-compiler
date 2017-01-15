{-# LANGUAGE OverloadedStrings #-}

module Compiler.CompExec where

import Control.Applicative
import Data.Functor
import Data.Text
import Data.Either
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import System.IO
import Parser
import Eval
import Compiler.Comp

charCount :: Char -> String -> Int
charCount c s = Data.Text.count (pack [c]) (pack s)

newCount :: Int -> String -> Int
newCount i s = i + charCount '(' s - charCount ')' s

getMemoSet :: AvailMemo
getMemoSet = getMemoSet' 255 S.empty

getMemoSet' :: Int -> AvailMemo -> AvailMemo
getMemoSet' 0 avm = S.insert 0 avm
getMemoSet' x avm = getMemoSet' (x - 1) (S.insert x avm)

translateLang :: String -> String -> IO ()
translateLang input_path output_path = do
  inh <- openFile input_path ReadMode
  ouh <- openFile output_path WriteMode
  executeLoop (M.empty, getMemoSet, 0) "" 0 inh ouh
  hClose inh
  hClose ouh

executeLoop :: CompTable -> String -> Int -> Handle -> Handle -> IO ()
executeLoop cpt hist cnt inh ouh = do
    isEof <- hIsEOF inh
    if not isEof
        then do
            line <- hGetLine inh
            executeLoop cpt (hist ++ " " ++ line) (newCount cnt line) inh ouh
    else do
            putStrLn hist
            --print cpt'
            hPutStr ouh out
            where
                (cpt', out) = comp cpt hist

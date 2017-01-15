{-# LANGUAGE OverloadedStrings #-}

module Compiler.Comp where

import Control.Applicative
import Data.Functor
import Data.Either
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import System.IO
import Parser
import Compiler.CompExec

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
    if isEof
        then return ()
    else do
        line <- hGetLine inh
        case (hist ++ " " ++ line) of
            _ -> case out of
                "" -> executeLoop cpt' "" 0 inh ouh
                _ -> do
                    putStrLn line'
                    --print cpt'
                    hPutStr ouh out
                    executeLoop cpt' "" 0 inh ouh
                where
                    line' = hist ++ " " ++ line
                    (cpt', out) = comp cpt line'

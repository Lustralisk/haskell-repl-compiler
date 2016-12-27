{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Functor
import Data.Text
import Data.Either
import Data.Attoparsec.Text
import Control.Monad
import Parser
import Eval


{- Divide mainFunc -}
mainFunc :: IO ()
mainFunc = do
    line <- getLine
    if Data.Text.null $ pack line
    then return ()
    else
        putStrLn $ printEval $ eval $ pack line
    mainFunc

main :: IO ()
main = mainFunc

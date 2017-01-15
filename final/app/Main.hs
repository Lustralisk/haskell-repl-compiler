{-# LANGUAGE OverloadedStrings #-}

module Main where

import Exec
import Compiler.Comp
import Compiler.IRExec
import System.Environment

data Mod = MRepl | Output String | Input String | Prog String

readMod :: String -> Maybe Mod
readMod "-repl" = Just MRepl
readMod ('-':'o':' ':o) = Just (Output o)
readMod ('-':'i':' ':i) = Just (Input i)
readMod ('-':'t':' ':t) = Just (Prog t)
readMod _ = Nothing

readcl :: [String] -> [Mod]
readcl [] = []
readcl (x:y:xs) = case readMod (x ++ " " ++ y) of
    Just (Output o) -> a
    Just (Input i) ->
    Just (Prog t) -> 
readcl (x:xs) = case readMod x of
    Just m ->

main :: IO ()
main = do
    args <- getArgs
    runReplT

-- main = translateLang "/Users/ocNflag/Desktop/in.txt" "/Users/ocNflag/Desktop/out.txt"
-- =======
-- import Compiler.CompExec
-- import Compiler.IRExec
--
--
-- main :: IO ()
-- --main = runReplT
-- main = execWrapper "/Users/lustralisk/Desktop/out.txt"
-- >>>>>>> ffda66adad8d9675f4eb2e94769864cc3cbf6909

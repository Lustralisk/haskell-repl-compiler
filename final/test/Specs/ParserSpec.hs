{-# LANGUAGE OverloadedStrings #-}

module Specs.ParserSpec where

import Test.QuickCheck
import Data.Text
import Data.Attoparsec.Text
import Parser

prop_falseParser_skipSpace :: Text -> Property
prop_falseParser_skipSpace text = (isInfixOf "False" text) ==> parseOnly falseParser text == Right FalseLit

falseParserTests = [("prop_falseParser_skipSpace", prop_falseParser_skipSpace)]

parserTests = falseParserTests

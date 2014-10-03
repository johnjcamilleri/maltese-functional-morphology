module Main where

import CommonMain
import DictMt
import CompositeMt
import CommandsMt
import Frontend

main :: IO ()
main = commonMain Maltese

data Maltese = Maltese
 deriving Show

instance Language Maltese where
 internDict   _ = malteseDict
 -- composition  _ = decomposeMt
 composition  _ = Just $ compDesc
 paradigms    _ = foldr insertCommand emptyC commands

-- compDesc :: CompDesc
compDesc = [[]]

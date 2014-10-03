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
 composition  _ = decomposeMt
 paradigms    _ = foldr insertCommand emptyC commands

{-# LANGUAGE ScopedTypeVariables #-}

module CommonMain where

import Print
import Trie
import Data.List(intersperse)
import System.Environment(getArgs, getEnv)
import GeneralIO
import System.IO
import Dictionary
import Frontend
import Data.Char
import ErrM
-- import Monad
import Control.Exception (catch, IOException)

gfTypes :: Language a => a -> String
gfTypes l = "types." ++ name l ++ ".gf"

readDict :: Language a => a -> FilePath -> IO Dictionary
readDict l f =
  do database <- parseDict l f
     return $ unionDictionary (internDict l) database

readTrie :: Language a => a -> FilePath -> IO SATrie
readTrie l f = do d <- readDict l f
		  prInfo d
		  return $ trieDict d

uName :: Language a => a -> String
uName l = case name l of
	   [] -> []
	   (x:xs) -> toUpper x : xs

commonMain :: Language a => a -> IO ()
commonMain l = do
  xx <- getArgs
  lex <- catch (getEnv (env l)) (\(_::IOException) ->
   do prErr $ "\n[" ++ (env l) ++ " is undefined, using \"./" ++ (dbaseName l) ++ "\".]\n"
      return $ "./" ++ (dbaseName l))
  case xx of
    []             -> do prErr $ welcome l
		         t <- readTrie l lex
			 run (analysis t (composition l))
    ["-h"]         -> help
    ["-s"]         -> do prErr $ welcome l
	                 putStrLn $ "\n[Synthesiser mode]\n"
			 putStrLn $ "Enter a " ++ (uName l) ++ " word in any form.\n"
			 putStrLn $ "If the word is not found, a [command] with [arguments].\n"
			 putStrLn $ "Type 'c' to list commands.\n"
			 putStrLn $ "Type 'q' to quit.\n"
			 theDictionary <- readDict l lex
			 trieDictL     <- readTrie l lex
			 synthesiser l theDictionary trieDictL
    ["-i"]         -> do prErr $ welcome l
	                 putStrLn $ "\n[Inflection mode]\n"
			 putStrLn $ "Enter [command] [dictionary form].\n"
			 putStrLn $ "Type 'c' to list commands.\n"
			 putStrLn $ "Type 'q' to quit.\n"
			 infMode l
    ["-ib"]         -> do prErr $ welcome l
		          imode l
    _  ->
      do theDictionary <- readDict l lex
         case xx of
	  ["-lex"]            -> outputLex theDictionary
	  ["-lex",file]  -> do writeLex file theDictionary
	                       prErr $ "Wrote full form lexicon: " ++ file
	  ["-tables"]         -> outputTables theDictionary
	  ["-tables",file]    -> do writeTables file theDictionary
	                            prErr $ "Wrote tables: " ++ file
	  ["-gf"]             -> outputGF (gfTypes l) theDictionary
	  ["-gf",file]        -> do writeGF file (gfTypes l) theDictionary
	                            prErr $ "Wrote GF source code: " ++ file
	  ["-gfr"]            -> outputGFRes (gfTypes l) theDictionary
	  ["-gfr",file]       -> do writeGFRes file (gfTypes l) theDictionary
	                            prErr $ "Wrote GF resource: " ++ file
	  ["-latex"]          -> outputLatex theDictionary
	  ["-latex",file]     -> do writeLatex file theDictionary
	                            prErr $ "Wrote LaTeX document: " ++ file
	  ["-xml"]            -> outputXML theDictionary
	  ["-xml",file]       -> do writeXML file theDictionary
	                            prErr $ "Wrote XML source code: " ++ file
	  ["-lexc"]           -> outputLEXC theDictionary
	  ["-lexc",file]      -> do writeLEXC file theDictionary
	                            prErr $ "Wrote LEXC source code: " ++ file
	  ["-xfst"]           -> outputXFST theDictionary
	  ["-xfst",file]      -> do writeXFST file theDictionary
			            prErr $ "Wrote XFST source code: " ++ file
	  ["-sql"]            -> outputSQL theDictionary
	  ["-sql",file]   -> do writeSQL file theDictionary
			        prErr $ "Wrote SQL source code: " ++ file
	  xs             -> do prErr $ "Invalid parameter" ++ unwords xs
	                       help

run :: (String -> [[String]]) -> IO ()
run f =  interact $ unlines . analyze (f) . nWords

analyze :: (String -> [[String]]) -> [String] -> [String]
analyze _  []  = []
analyze f (s:ss)
 = case (f s) of
    [] -> ("[ <" ++ s ++ "> NONE]") : analyze f ss
    xs -> ("[ <" ++ s ++ ">") : (prA xs ++  "]") : analyze f ss
 where
       prA xs = unlines [show n ++ ". " ++ s | (n,s) <- zip [1..] (map pr xs)]
       pr []  = []
       pr [x] = x
       pr xs  = "Composite: " ++ concat (intersperse " | " xs)

welcome :: Language a => a -> String
welcome l = unlines
	    [
	     "********************************************",
	     "* " ++ uName l ++ " Morphology" ++ (padding (uName l) 30) ++ "*",
	     "********************************************",
	     "* Functional Morphology v1.10              *",
	     "* (c) Markus Forsberg & Aarne Ranta 2004   *",
	     "* under GNU General Public License.        *",
	     "********************************************",
	     ""
	    ]
  where padding s n = replicate (max (n - length s) 0) ' '


prInfo :: Dictionary -> IO()
prInfo dict = prErr $ "Dictionary loaded: DF = " ++ show (size dict) ++ " and WF = " ++ show (sizeW dict) ++ ".\n"


help :: IO()
help = prErr help_text

help_text :: String
help_text = unlines ["",
		     " |---------------------------------------|",
		     " |        Program parameters             |",
		     " |---------------------------------------|",
		     " | -h             | Display this message |",
		     " |---------------------------------------|",
		     " | <None>         | Enter tagger mode    |",
		     " |---------------------------------------|",
		     " | -s             | Enter interactive    |",
		     " |                | synthesiser mode     |",
		     " |---------------------------------------|",
		     " | -i             | Enter inflection     |",
		     " |                | mode                 |",
		     " |---------------------------------------|",
		     " | -ib            | Inflection batch     |",
		     " |                | mode                 |",
		     " |---------------------------------------|",
		     " | -lex    [file] | Full form lexicon    |",
		     " | -tables [file] | Tables               |",
		     " | -gf     [file] | GF top-level code    |",
		     " | -gfr    [file] | GF resource code     |",
		     " | -latex  [file] | LaTeX source code    |",
		     " | -xml    [file] | XML source code      |",
		     " | -lexc   [file] | LexC source code     |",
		     " | -xfst   [file] | XFST source code     |",
		     " | -sql    [file] | SQL source code      |",
	       	     " |---------------------------------------|",
		     ""
		    ]

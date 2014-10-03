module GenFM where

import Char
import List(intersperse)

type Name      = String -- generic name
type Cons      = String -- constructor name
type Arg       = String -- constructur arguments
type Lang      = String -- name of language

type POS       = Data -- Parameter type for POS
type Param     = Data -- Parameter type 
type Data      = (Name,[(Cons,[Arg])])
type Paradigm  = String -- A name of a paradigm
type FunDef    = String -- The paradigm functions of 

-- The main object type

type FM = (Lang,[Param], [POS],[Paradigm])

-- Note that this module assumes that all names have a capitalized beginning.
-- No form of type checking is performed in this module.

---------------------------------------
-- abstraction functions

lang :: FM -> Lang
lang (l,_,_,_) = l

params :: FM -> [Param]
params (_,ps,_,_) = ps

isFlat :: Data -> Bool
isFlat = all null . map snd . snd

pos :: FM -> [POS]
pos (_,_,xs,_) = xs

paradigms :: FM -> [Paradigm]
paradigms (_,_,_,ps) = ps

---------------------------------------
-- variable generator

vars :: [String]
vars = ["x" ++ show n | n <- [0..]]

--------------------------------------------
-- An example of a FM object
---------------------------------------------

runTest :: IO()
runTest = putAll test fdef

fdef :: FunDef
fdef = unlines [
		"d1puella :: String -> Entry",
		"d1puella s = feminine . decl1"
	       ]

test :: FM
test = (testLang,testParam,testPos,testPara)
	
testLang  = "Latin"

testParam = 	
    [
     ("Gender",zip ["Masc","Fem","Neu"] (repeat [])),
     ("Case",zip ["Nom", "Voc", "Acc"] (repeat [])),
     ("Number", zip ["Sing","Pl"] (repeat []))
    ]
	     
testPos =
    [("Noun",[("NF",["Case","Number"])])]
    
testPara = ["d1puella","d1poeta"]

---------------------------------------

putAll :: FM -> FunDef -> IO()
putAll fm fd = putStrLn $
	        unlines ["[" ++ file ++ "]\n" ++ s | (file,s) <- prAll fm fd]

prAll :: FM -> FunDef -> [(FilePath,String)]
prAll fm fd =
    [
     prTypes fm,
     prRules fm fd,
     prDict fm,
     prMain fm
    ]

prTypes :: FM -> (FilePath,String)
prTypes fm =
    let l = lang fm
	str =
	    unlines $
			[
			 "module Types" ++ l ++ " where",
			 "",
			 "import General",
			 "import Dictionary",
			 "import Print",
			 "",
			 prParams (params fm),
			 prPos (pos fm)
			]
	in ("Types" ++ l ++ ".hs",str)


prParams :: [Param] -> String
prParams pms = unlines $
	       ["data " ++ name ++ " =\n" ++ prCons cs ++ 
		"\n deriving (Show, Eq, Enum, Ord, Bounded)\n\n" ++
		"instance Param " ++ name ++ " where values = enum\n"
                | (name,cs) <- pms]
 where prCons cs = concat $ intersperse " |\n" ["      " ++ c  ++ " " ++ unwords args | (c,args) <- cs]

-- FIXME: Forgot to add Param definitions to POS.

prPos :: [POS] -> String
prPos = concat . map prP 
  where prP (name,xs) = unlines $
			[
			 "type " ++ name ++ " = " ++ name ++ "Form -> Str",
			 "",
			 "data " ++ name ++ "Form = ",
			 concat $ intersperse "|\n" ["      " ++ c ++ " " ++ unwords args ++ "\n deriving(Show,Eq)" | (c,args) <- xs],
			 "",
			 "instance Param " ++ name ++ "Form where",
			 " values =",
			 concat $ intersperse " ++\n" ["   [" ++ c ++ " " ++ unwords (fst (prV as)) ++ " | " ++ snd (prV as) ++ "]"   | (c,as) <- xs],
			 "",
			 "instance Dict " ++ name ++ "Form where category _ = \"" ++ name ++ "\""
			 ]
	prV args = let vs =  map snd $ zip args vars
		    in (vs,concat $ intersperse ", " [ x ++ " <- values"  | (_,x) <- zip args vars])

prRules :: FM -> FunDef -> (FilePath,String)
prRules fm fd = let l = lang fm
		    str =
			unlines
			[ 
			 "module Rules" ++ l ++ " where",
			 "",
			 "import Types" ++ l,
			 "import General",
			 "import Print",
			 "import Dictionary",
			 "",
			 fd
			 ]
		    in ("Rules" ++ l ++ ".hs", str)
		     
prDict :: FM -> (FilePath,String)
prDict fm = let l = lang fm
		ll = map toLower l
	        str = 
		    unlines
		    [
		    "module Dict" ++ l ++ " where",
		    "",
		    "import Rules" ++ l,
		    "import Dictionary",
		    "import Types" ++ l,
		    "",
		    ll ++ "Dict :: Dictionary",
		    ll ++ "Dict = dictionary $ lexicon",
		    "",
		    "lexicon :: [Entry]",
		    "lexicon = []"
		    ]
		in ("Dict" ++ l ++ ".hs", str)

prMain :: FM -> (FilePath,String)
prMain fm =
    let l   = lang fm
	ll  = map toLower l
	ps  = paradigms fm
        str = unlines $
	      [
	       "module Main where",
	       "",
	       "import CommonMain",
	       "import Rules" ++ l,
	       "import Dict" ++ l,
	       "import Map",
	       "import Dictionary",
	       "import General",
	       "",
	       "main :: IO()",
	       "main = commonMain " ++ l,
	       "",
	       "data " ++ l ++ " = " ++ l,
	       "  deriving Show",
	       "",
	       "instance Language " ++ l ++ " where",
	       " internDict  _ = " ++ ll ++ "Dict",
	       " composition _ = " ++ ll ++ "Decompose",
	       " paradigms   _ = commands",
	       "",
	       "commands :: Map String (String -> Entry)",
	       "commands =",
	       " [ ",
	       concat $ intersperse ",\n" ["  (\"" ++ p ++ "\", " ++ p ++ ")" | p <- ps],
	       " ] |->+ empty",
	       "",
	       ll ++ "Decompose :: [[Attr]] -> Bool",
	       ll ++ "Decompose = noComp"
	      ]
     in ("Main.hs",str)

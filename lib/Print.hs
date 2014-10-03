module Print where

import General
import Dictionary
import Data.List (intersperse)

-- printing morphological objects as strings

prStr :: Str -> String
prStr = concat . intersperse "/" . unStr

prAlts :: Str -> String
prAlts ss =
    case unStr ss of
     [] -> "*"
     ys -> unwords $ intersperse "/" ys

consTable :: Str -> Table String
consTable s = [("INVAR", s)]

consTableW :: Str -> [(String,(Attr,Str))]
consTableW s = [("INVAR", (noComp,s))]

putFun0 :: Param a => (a -> Str) -> IO ()
putFun0 = putStr . unlines . map show . prTable . table

putFun :: Param a => (a -> Str) -> IO ()
putFun = putStr . unlines . map pr . prTable . table where
  pr (a,ss) = a ++ " : " ++ prAlts ss

-- print a parameter value without hierarchy (= parentheses)
prFlat :: String -> String
prFlat = filter (flip notElem "()")

-- show all values for the first parameter
prFirstForm :: Param a => Table a -> String
prFirstForm = prStr . firstForm

-- show one value for the first parameter (used in dictionary)
prDictForm :: Param a => Table a -> String
prDictForm = prDictStr . firstForm

prDictStr :: Str -> String
prDictStr t = case unStr t of
  s:_ -> s
  []  -> "NONE"

prDictionary :: Dictionary -> String
prDictionary = unlines . map (unlines . prOne) . removeAttr where
  prOne (stem, typ, inhs, infl) = stem : typ : unwords inhs :
                             [a ++ ": " ++ prStr s | (a,s) <- infl]

prFullFormLex :: FullFormLex -> String
prFullFormLex = concat . map prOne where
  prOne (s,ps) = unlines [s ++ ":" ++ a | a <- map prAttr ps]
  -- prOne (s,ps) = s ++ " : " ++ unwords (intersperse "/" (map prAttr ps))
  prAttr (a,ss) = ss ++ prCompAttr a

prCompAttr :: Attr -> String
prCompAttr a = " (" ++ show a ++ ") " -- should not happen...

-- a parser

{-
-- parse full-form lexicon from the format we print; ignore unparsable lines
pFullFormLex :: String -> FullFormLex
pFullFormLex s = [r | l <- s', Just r <- [pOne (words l)]] where
  s' = filter nocomment (lines s) where
    nocomment l = case l of
      '-':'-':_ -> False   -- use -- for comment lines
      _ -> True
  pOne (s:":":ps) = Just (s, (pPs ps))
  pPs ws = pCompAttr p : ps where
    (p,ps0) = span (/="/") ws
    ps = if null ps0 then [] else pPs (tail ps0)
  pCompAttr p = case p of
    "(P)"  : p' -> (atP,  unwords p')
    "(WP)" : p' -> (atWP, unwords p')
    _ -> (atW, unwords p) -- ignoring values >2
-}

-- generate GF source code
-- N.B. syntax errors result from GF-reserved-word identifiers!

prGFRes :: Dictionary -> String
prGFRes dict = (unlines (map prGFOper (zip [0..] (removeAttr dict))))

prGFOper :: (Int,(String, Ident, [Ident], Table Ident)) -> String
prGFOper (i,(oper, ty, inhs, tab0)) = begin ++ " : Str -> " ++ ty ++ " = " ++ bind ++ rec ++ end where
  begin = "oper " ++ (oper ++ "_" ++ show i) -- Reduce the number of name clashes!
  bind  = "\\" ++ oper ++ " -> " ++
            "\n  let " ++
            stemv ++ " = Predef.tk " ++ show lg1 ++ " " ++ oper ++ " in"
  stem  = longestPrefix (unStr (formsInTable tab0))
  stemv = if lg == 0 then "x_" else stem ++ "_" -- to avoid clash with res words
  lg1   = length oper - lg
  lg    = length stem
  tab   = mapInTable
             (\w -> stemv ++ " + \"" ++ drop lg w ++ "\"") tab0
  rec   = "\n {s = " ++ tbl ++
          (if null inhs then "" else " ;\n  ") ++
          concat (intersperse " ;\n  "
                    ["h" ++ show i ++ " = " ++ p | (i,p) <- zip [1..] inhs]
                 ) ++
          "\n  }"
  tbl = case tab of
    [("INVAR",ss)] -> altsGF ss --- a hack to avoid one-branch tables; name-sensit.
    _ -> "table {\n" ++
          concat (intersperse " ;\n"
                    ["    "++ a ++ " => "++ altsGFRes b | (a,b) <- tab]
                 ) ++
          "\n    }"
  end   = " ;\n"

prGF :: Dictionary -> String
prGF dict = cats ++ (unlines (map prGFRule (zip [0..] (removeAttr dict))))
 where cs = unlines ["cat " ++ c ++ ";" | c <- map fst $ classifyDict dict]
       cats = "\n" ++ cs ++ "\n\n"
prGFRule ::  (Int,(String, Ident, [Ident], Table Ident)) -> String
prGFRule (i,(id,cat,inhs,tab)) = let name = id ++ "_" ++ show i in
  "fun " ++ name ++ " : " ++ cat ++ " ;\n\n" ++
  "lin " ++ name ++ " = {s = table {\n" ++
    concat (intersperse " ;\n"
               ["  "++ a ++ " => "++ altsGF b | (a,b) <- tab]) ++
          (if null inhs then "}" else " };\n  ") ++
          concat (intersperse " ;\n  "
                    ["h" ++ show i ++ " = " ++ p | (i,p) <- zip [1..] inhs]
                 ) ++
     "\n} ;\n"

-- two GF modes for free variation; old for GF<0.98
altsGF xs = case (unStr xs) of
  [x] -> prQ x
  ys -> "variants"++" {" ++ unwords (intersperse ";" (map prQ ys)) ++ "}" where
 where
   prQ s = '"' : s ++ "\""
altsGFOld = show . prAlts
altsGFRes xs = case (unStr xs) of
  [x] ->  x
  ys -> "variants"++" {" ++ unwords (intersperse ";" ys) ++ "}"


-- code for XML
type TagId = String
type XML   = String
type Struct = Bool

string :: String -> [XML]
string = (:[])

render :: [XML] -> String
render xs = unlines xs

tag :: TagId -> [XML] -> [XML]
tag t xs = (("<" ++ t ++ ">"): (map (' ':) xs)) ++ ["</" ++ t ++ ">"]

tagA :: TagId -> (String,String) -> [XML] -> [XML]
tagA t (a,b) xs = (("<" ++ t ++ " " ++ a ++ "=\"" ++ b ++ "\"" ++ ">"): (map (' ':) xs)) ++ ["</" ++ t ++ ">"]

tagA1 :: TagId -> (String,String) -> XML
tagA1 t (a,b) = "<" ++ t ++ " " ++ a ++ "=\"" ++ b ++ "\"" ++ " />"

prXML :: Dictionary -> String
prXML d =  "<?xml version=\"1.0\"?>\n" ++ (render (tag "lexicon" (concat (map (uncurry pr) (classifyDict d)))))
 where
 pr cat entries = tagA "class" ("category",cat) (concat (map (prEntry . noAttr) entries))
 prEntry (stem,_,inhs,tbl) = tag "lexicon_entry" $ tagA1 "dictionary_form" ("value",stem) :(prInh inhs ++ prTabl tbl)
 prInh inhs = map (\s -> tagA1 "inherent" ("value",s)) inhs
 prTabl tbl = tag "inflection_table" $
	      concat [tagA "inflection_form" ("pos",a) (map (\s -> tagA1 "variant" ("word",s)) (unStr b)) | (a,b) <- existingForms tbl]

-- code for Xerox LexC

prLEXC :: Dictionary -> String
prLEXC = ("LEXICON Root\n" ++) . (++ "END") . unlines . map (uncurry prLEXCRules) . classifyDict

prLEXCRules :: Ident -> [Entry] -> String
prLEXCRules cat entries = unlines $
    ("\n! category " ++ cat ++ "\n") : (map (prEntry . noAttr) entries)
  where
    prEntry (stem,_,inhs,tbl) =
      concat (map (prForm stem inhs) ([(a,unStr b) | (a,b) <- existingForms tbl]))
    prForm stem inhs (a,b) =
      concat [x ++ ":" ++ stem ++ prTags (a:inhs) ++ " # ;\n" | x <- b]
    prTags ts =
      concat ["+" ++ w | t <- ts, w <- words (prFlat t)]
    altsLEXC cs =
      unwords $ intersperse " # ;" [ s  | s <- cs]

-- code for Xerox Finite State Tool

prXFST :: Dictionary -> String
prXFST = unlines . map (uncurry prXFSTRules) . classifyDict

prXFSTRules :: Ident -> [Entry] -> String
prXFSTRules cat entries = unlines $
    ("define " ++ cat ++ " [") :
    intersperse "  |" (map (prEntry . noAttr) entries) ++
    ["  ] ;"]
  where
    prEntry (stem,_,inhs,tbl) =
      concat (intersperse "  |\n" (map (prForm stem inhs)
				   ([(a,unStr b) | (a,b) <- existingForms tbl])))
    prForm stem inhs (a,b) =
      "  [ {" ++ stem ++ "}" ++ prTags (a:inhs) ++ " .x. " ++ altsXFST b ++"]"
    prTags ts =
      unwords [" %+" ++ w | t <- ts, w <- words (prFlat t)]
    altsXFST cs =
      unwords $ intersperse "|" ["{" ++ s ++ "}" | s <- cs]

-- a "book" with LaTeX tables

prLatex :: Dictionary -> String
prLatex d = unlines (beginLatex ++ map prLatexTable (removeAttr d) ++ endLatex) where
  beginLatex = ["\\documentclass{report}",
                "\\usepackage{isolatin1}",
                "\\begin{document}"]
  endLatex   = ["\\end{document}"]

prLatexTable :: EntryN -> String
prLatexTable (ident,cat,inhs,tab) =
  unwords ((ident ++ ",") : cat : inhs) ++ "\n" ++
  "\\begin{center}\n\\begin{tabular}{|l|l|}\\hline\n" ++
  unlines [a ++ " & {\\em " ++ prAlts b ++ "} \\\\" | (a,b) <- tab] ++
  "\\hline\n\\end{tabular}\n\\end{center}\n\\newpage\n\n"
-- use prValue instead of this!
{-
 where
   prTag    = unpar . unwords . twords  -- remove the outermost constructor
   twords s = case words s of
     (_:w:ws) -> w:ws                   -- but only if something is left
     ws -> ws
   unpar s = case s of                  -- remove the outer parentheses
     '(':cs | last cs == ')' -> init cs
     _ -> s
-}
-- SQL

---------------------------------------------------------

wordLength = 50 :: Int
attrLength = 30 :: Int

type Schema       = String -- The database structure
type Element      = String -- the database content
type TableS       = String -- a table
type Column       = String -- a column (attribute)
type Value        = String -- a value of a column (attribute)
type DatabaseName = String

prSqlSchema :: Dictionary-> DatabaseName -> String
prSqlSchema dict dname =
                    "\n-- The Morphology Schema.\n\n" ++
		    "DROP DATABASE IF EXISTS " ++ dname ++ ";\n" ++
		    "CREATE DATABASE " ++  dname ++ ";\n" ++
		    "USE " ++ dname ++ ";\n\n" ++
		    lexicon ++
		    "GRANT ALL PRIVILEGES ON " ++ dname ++".* TO PUBLIC ; \n\n"

-- A instance needs to:
-- * Be put in the lexicon with a unique identifier
-- * Be put in the class schema
-- * Be put in the inherent schema

prSQL :: Dictionary -> String
prSQL = (lexicon ++) . unlines . map prSql . zip [1..] . removeAttr
 where
  prSql (i,(stem, cat, inh, table)) = lexic i stem  cat (expand table inh)
  lexic i stem cat t =
                 unlines [insert "LEXICON" [show i,stem,cat,b,a] | (a,b) <- t]
  expand table inh = [(a ++ " - " ++ (unwords inh) ,s) | (a,b) <- table,
			                                         s <- unStr b]

{-
prWordsCl ::  [(String,[((Int,String),[String])])] -> [String]
prWordsCl                  [] = []
prWordsCl ((c,((n1,w1),as1):xs):xss)
    = (insert c ([show n1,w1,show n1] ++ as1) :
       [insert c ([show n,w,show n1] ++as) | ((n,w),as) <- xs]) ++
       prWordsCl xss

innerNumber :: [(a,[(b,[c])])] -> Int -> [(a,[((Int,b),[c])])]
innerNumber [] _ = []
innerNumber ((a,xs):xss) n = (a,number xs n) :
			     innerNumber xss (n+(length xs))
 where number xs n = zipWith f [n..] xs
       f n (s,zs) = ((n,s),zs)
-}

-----------------------------------------------------

emptyE :: Value
emptyE = "NULL"

insert :: TableS -> [Value] -> Element
insert t vs = "INSERT INTO " ++ t ++ " VALUES ('"
	      ++ (concat (intersperse "','" vs)) ++"');"

type Name           = String
type Type           = String
type TypeConstraint = String
type Constraint     = String

primaryKey :: Name -> Constraint
primaryKey n = "PRIMARY KEY (" ++ n ++ ")"

foreignKey :: Name -> (Name,Name) -> Constraint
foreignKey n (n1,n2) = "FOREIGN (" ++ n ++ ") REFERENCES " ++
		       n1 ++ "(" ++ n2 ++ ")"

varchar :: Int -> Type
varchar n = "VARCHAR(" ++ show n ++ ")"

intType :: Type
intType = "INTEGER"

notNull :: TypeConstraint
notNull = "NOT NULL"

createTable :: Name -> [(Name,Type,TypeConstraint)] -> [Constraint] -> TableS
createTable n xs cs =
    "CREATE TABLE " ++ n ++ "\n(\n" ++
    (concat ((intersperse ",\n" [n ++ " " ++ t ++ " " ++ tc | (n,t,tc) <- xs])))
    ++ concat (intersperse ",\n" cs) ++ ");\n\n"

lexicon :: TableS
lexicon = createTable "LEXICON"
	  [
	   ("ID", intType, notNull),
	   ("DICTIONARY",varchar wordLength,notNull),
	   ("CLASS",varchar wordLength,notNull),
	   ("WORD",varchar wordLength,notNull),
	   ("POS",varchar wordLength,notNull)
          ] []

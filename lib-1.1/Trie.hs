module Trie (tcompile, SATrie, decompose,tlookup,isInTrie, Trie, collapse) where

import Map
import General
import Data.Maybe(fromJust)
import Data.List(nub, inits, tails)


data Trie a = Trie ! (Map Char (Trie a), [a])
 deriving (Show)

type AttrTrie a = Trie (Attr,a)

type SATrie = AttrTrie String

-------------------------------------------------

mTable :: Trie a -> Map Char (Trie a)
mTable (Trie (m,_)) = m

val :: Trie a -> [a]
val (Trie (_,v)) = v

addVal :: Trie a -> [a] -> Trie a
addVal (Trie (m,v)) v1 = (Trie (m,v++v1))

emptyTrie :: Trie a
emptyTrie = Trie (empty,[])

trie :: Map Char (Trie a) -> [a] -> Trie a
trie m val = Trie (m,val)

---------------------------------------------------

tcompile :: [(String, [a])] -> Trie a
tcompile = foldl (flip insert) emptyTrie

insert :: (String,[a]) -> Trie a -> Trie a
insert ([],ys)     t = addVal t ys
insert ((c:cs),ys) t =
  case mTable t ! c of
   Just t' -> case insert (cs,ys) t' of
               tr -> trie ((c,tr) |-> mTable t) (val t)
   Nothing -> case insert (cs,ys) emptyTrie of
               tr -> trie ((c,tr) |-> mTable t) (val t)

trieLookup :: Trie a -> String -> [a] -- [(Attr,String)]
trieLookup t [] = val t
trieLookup trie (c:cs) = case mTable trie ! c of
  Just trie -> trieLookup trie cs
  Nothing   -> []

isInTrie ::  Trie a -> String -> Bool
isInTrie tr = not . null . trieLookup tr

tlookup :: SATrie ->String -> (String,[(Attr,String)])
tlookup t s = (s,[(a,p) | (a,p) <- trieLookup t s])

collapse :: Trie a -> [(String,[a])] -- [(String, Data)]
collapse t = collapse' t []
 where
  collapse' t xs
    | null (val t) = rest
    | otherwise    = (reverse xs, val t) : rest
   where rest = concat $ [collapse' tr (c:xs) | (c,tr) <- flatten (mTable t)]

decompose :: AttrTrie a -> ([Attr] -> Bool) -> String -> [[(Attr,a)]]
decompose trie _  [] = []
decompose trie f sentence =
    concat $ map (legal trie f) $ deconstruct trie sentence

-- backtrack [(sentence,[])] trie

legal :: AttrTrie a ->  ([Attr] -> Bool) -> [String] -> [[(Attr,a)]]
legal trie f input = removeInvalids attrValues
 where
  removeInvalids [] = [] -- Remove all invalid analysis
  removeInvalids (xs:xss)
   | f (map fst xs)           = xs : removeInvalids xss -- Sequence valid
   | otherwise                = removeInvalids xss
  flatten       [] = [[]] -- combine all analyses with all other analyses
  flatten (xs:xss) = [x:ys | x <- xs, ys <- res]
      where res = flatten xss
  attrValues = flatten $ map (trieLookup trie) input

-- Improved decomposition

deconstruct :: AttrTrie a -> String -> [[String]]
deconstruct t [] = [[]]
deconstruct t s  = concat [map (p:) (deconstruct t r) | (p,r) <- split s, isInTrie t p]

split :: String -> [(String,String)]
split s = zip (inits s) (tails s)

-- Old code (longest match)
{-
react :: String -> [String] -> [(String,[String])] -> String -> AttrTrie a -> AttrTrie a -> [String]
react input output back occ (Trie (arcs,res)) init =
    case res of -- Accept = non-empty res.
     [] -> continue back
     _ -> let pushout = (occ:output)
	    in case input of
	        [] -> reverse $ map reverse pushout
		_ -> let pushback = ((input,pushout):back)
		      in continue pushback
 where continue cont = case input of
		        []       -> backtrack cont init
			(l:rest) -> case arcs ! l of
				     Just trie ->
					 react rest output cont (l:occ) trie init
				     Nothing -> backtrack cont init

backtrack :: [(String,[String])] -> AttrTrie a -> [String]
backtrack [] _  = []
backtrack ((input,output):back) t
    = react input output back [] t t
-}

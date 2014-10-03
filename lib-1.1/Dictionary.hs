module Dictionary (Dict(..),
		   Dictionary,
		   Entry,
		   prTable,
		   removeAttr,
		   FullFormLex,
		   Ident,
		   classifyDict,
		   noAttr,
		   entry,
		   entryI,
		   EntryN,
		   dictionary,
		   unDict,
		   size,
		   sizeW,
		   unionDictionary,
		   emptyDict,
		   dict2fullform,
		   nWords
		  ) where

import General
import Data.List (sortBy, group)
import Data.Char

-- untyped dictionary: dictionary word, category, inherent features, inflection

class Param a => Dict a where
  dictword :: (a -> Str) -> String
  dictword f = concat $ take 1 $ unStr (f value0)
  category :: (a -> Str) -> String
  category = const "Undefined"
  defaultAttr :: (a -> Str) -> Attr
  defaultAttr = const noComp
  attrException :: (a -> Str) -> [(a,Attr)]
  attrException = const []

data Dictionary       = Dict [Entry]
type Dictionary_Word  = String
type Category         = String
type Inherent         = String
type Untyped          = String
type Inflection_Table = [(Untyped,(Attr,Str))]

type Entry = (Dictionary_Word, Category, [Inherent], Inflection_Table)

type Ident = String

type EntryN =  (Dictionary_Word, Category, [Inherent], [(Ident,Str)])

emptyDict :: Dictionary
emptyDict = Dict []

infTable :: Dict a => (a -> Str) -> Inflection_Table
infTable f = prTableAttr f (defaultAttr f) (attrException f)

entry  :: Dict a => (a -> Str) -> Entry
entry f = entryI f []

entryI :: Dict a => (a -> Str) -> [Inherent] -> Entry
entryI f ihs = (dictword f, category f, ihs, infTable f)

prTableAttr :: Param a => (a -> Str) -> Attr -> [(a,Attr)] -> [(String,(Attr,Str))]
prTableAttr t da ts =
    [(prValue a,(maybe da id (lookup a ts),s)) | (a,s) <- table t]

prTableW :: Param a => Table a -> [(String,(Attr,Str))]
prTableW t = [ (a,(noComp,s)) | (a,s) <- prTable t]

prTable :: Param a => Table a -> Table String
prTable = map (\ (a,b) -> (prValue a, b))

unDict :: Dictionary -> [Entry]
unDict (Dict xs) = xs

size :: Dictionary -> Int
size = length . unDict

sizeW :: Dictionary -> Int
sizeW dict = sum [length t | (_,_,_,t) <- unDict dict]

sizeEntry :: Entry -> Int
sizeEntry (_,_,_,t) = length t

dictionary :: [Entry] -> Dictionary
dictionary = Dict

unionDictionary :: Dictionary -> Dictionary -> Dictionary
unionDictionary (Dict xs) (Dict ys) = Dict $ xs ++ ys

removeAttr :: Dictionary -> [EntryN]
removeAttr = map noAttr . unDict

noAttr :: Entry ->  EntryN
noAttr (d,c,inh,tab) = (d,c,inh,[(i,s) | (i,(_,s)) <- tab])

-- group a dictionary into categories; reverses the entries...
classifyDict :: Dictionary -> [(Ident,[Entry])]
classifyDict = foldr addNext [] . unDict
 where
  addNext entry@(_,cat,_,_) dict = case dict of
    (c,es) : dict' | cat == c -> (c, entry:es) : dict'
    ces    : dict'            -> ces           : addNext entry dict'
    []                        -> [(cat,[entry])]

-- full-form lexicon: show all different analyses of a word as strings

type FullFormLex = [(String,[(Attr,String)])]

dict2fullform :: Dictionary -> FullFormLex
dict2fullform dict = sortAssocs $
                      concatMap mkOne $ zip (unDict dict) [0..] where
  mkOne ((stem, typ, inhs, infl),n) = concatMap mkForm infl where
    mkForm (par,(a,str)) = [(s, (a,
			    unwords (stem : ("(" ++ show n ++ ")") : typ : sp : par : sp : inhs))) | s <- (unStr str)]
    sp = "-"

-- word analyzator that handles interpunctation and initial upper case letter.

--aAáeEéiIíoOóuUúüyýY

nWords :: String -> [String]
nWords [] = []
nWords (c:cs)
 | alphanumeric c = case span alphanumeric cs of
		    (xs,ys) -> ((case c of
				 'Á' -> 'á'
				 'É' -> 'é'
				 'Í' -> 'í'
				 'Ó' -> 'ó'
				 'U' -> 'ú'--obs!
				 'Ü' -> 'ü'
				 'Ý' -> 'ý'
				 'Ñ' -> 'ñ'
				 c   -> toLower c):xs):nWords ys
 | isSpace c    = nWords cs
 | otherwise    = nWords cs -- throw away special characters
 where
  alphanumeric c = isAlpha c || elem c "ÁáÉéÍíÓóúÜüÝýÑñ"

-- binary search tree applicable to analysis

-- auxiliaries

-- binary search tree with logarithmic lookup
{-
data BinTree a = NT | BT a (BinTree a) (BinTree a) deriving (Show,Read)

sorted2tree :: [(a,b)] -> BinTree (a,b)
sorted2tree [] = NT
sorted2tree xs = BT x (sorted2tree t1) (sorted2tree t2) where
  (t1,(x:t2)) = splitAt (length xs `div` 2) xs

lookupTree :: (Ord a) => a -> BinTree (a,b) -> Maybe b
lookupTree x tree = case tree of
 NT -> Nothing
 BT (a,b) left right
   | x < a  -> lookupTree x left
   | x > a  -> lookupTree x right
   | x == a -> return b

tree2list :: BinTree a -> [a] -- inorder
tree2list NT = []
tree2list (BT z left right) = tree2list left ++ [z] ++ tree2list right
-}

--sortAssocs :: Ord a => [(a,b)] -> [(a,[b])]
--sortAssocs xs = flatten $ xs |->++ empty

-- Merge sort from List.hs adapted to the problem at hand
-- This function is a key function of FM, so optimizations
-- are essential.
sortAssocs :: [(String,(Attr,String))] -> [(String,[(Attr,String)])]
sortAssocs = mergesort (\(a,_) -> \(b,_) -> compare a b)
  where
    mergesort cmp = mergesort' cmp . map wrap

    mergesort' cmp [] = []
    mergesort' cmp [xs] = xs
    mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

    merge_pairs cmp [] = []
    merge_pairs cmp [xs] = [xs]
    merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

    wrap (a,b) = [(a,[b])]

    merge cmp xs [] = xs
    merge cmp [] ys = ys
    merge cmp (x@(a,xs):xss) (y@(_,ys):yss)
     = case x `cmp` y of
        GT -> y : merge cmp (x:xss)   yss
	EQ -> case xs of
               [z] -> merge cmp ((a,z:ys):xss) yss
	       zs  -> merge cmp ((a,zs++ys):xss) yss
        _  -> x : merge cmp    xss (y:yss)

{-
sortAssocs :: [(String,(Attr,String))] -> [(String,[(Attr,String)])]
sortAssocs ys = case sortBy (\(a,_) -> \(b,_) -> compare a b) ys of
                 ((x,v):zs) -> arrange x [v] zs
		 []         -> []
 where
  arrange y vs ((x,v):xs)
    | x == y    = arrange y (v:vs) xs
    | otherwise = (y,vs):arrange x [v] xs
  arrange y vs [] = [(y,vs)]
-}

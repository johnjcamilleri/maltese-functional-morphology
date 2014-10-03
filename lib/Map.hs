module Map (
            Map,
            empty,
	    (!),     -- lookup operator.
	    (!+),    -- lookupMany operator.
	    (|->),   -- insert operator.
	    (|->+),  -- insertMany operator.
	    (|->++),  -- insertMany operator.
	    (<+>),   -- union operator.
	    maybeUpdate,
	    flatten  -- 
	   ) where
    
-- A finite map implementation using red-black trees.

import RedBlack

type Map key el = Tree key el

infixl 6 |->
infixl 6 |->+
infixl 6 |->++
infixl 5 !
infixl 5 !+
infixl 4 <+>

empty :: Map key el
empty = emptyTree

isEmpty :: (Eq key, Eq el) => Map key el -> Bool
isEmpty = isEmptyTree

(!) :: Ord key => Map key el -> key -> Maybe el
fm ! e = lookupTree e fm

(!+) :: Ord key => Map key el -> [key] -> [Maybe el]
_  !+    []  = []
fm !+ (e:es) = (lookupTree e fm): (fm !+ es)

(|->) :: Ord key => (key,el) -> Map key el -> Map key el
(x,y) |-> fm = insertTree (x,y) fm

(|->+) :: Ord key => [(key,el)] -> Map key el -> Map key el
[]         |->+ fm = fm
((x,y):xs) |->+ fm = xs |->+ (insertTree (x,y) fm)

--(|->+++) :: Ord key => [(key,[el])] -> Map key [el] -> Map key [el]
--[]         |->+++ fm = fm
--((x,y):xs) |->+++ fm = xs |->+++ (maybeUpdate x y (y++) fm)

(|->++) :: Ord key => [(key,el)] -> Map key [el] -> Map key [el]
[]         |->++ fm = fm
((x,y):xs) |->++ fm = xs |->++ (maybeUpdate x y (y:) fm)

(<+>) :: Ord key => Map key el -> Map key el -> Map key el
(<+>) fm1 fm2 =  xs |->+ fm2
 where xs = flatten fm1

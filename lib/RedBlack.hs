module RedBlack (
                emptyTree,
		isEmptyTree,
		Tree,
		lookupTree,
		insertTree,
		maybeUpdate,
		flatten
		) where

-- Modified version of Osanaki's implementation.

data Color = R | B
 deriving (Show,Read,Eq)

data Tree key el = E | T !Color !(Tree key el) (key,el) !(Tree key el)
 deriving (Show,Read,Eq)

balance :: Color -> Tree a b -> (a,b) -> Tree a b -> Tree a b  
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

emptyTree :: Tree key el
emptyTree = E

isEmptyTree :: (Eq key, Eq el) => Tree key el -> Bool
isEmptyTree = (E ==)

lookupTree :: Ord a => a -> Tree a b -> Maybe b
lookupTree _ E = Nothing
lookupTree x (T _ a (y,z) b)
   | x < y      = lookupTree x a
   | x > y      = lookupTree x b
   | otherwise  = return z
   
insertTree :: Ord a => (a,b) -> Tree a b -> Tree a b
insertTree (key,el) tree = T B a y b
  where 
    T _ a y b = ins tree
    ins E = T R E (key,el) E
    ins (T color left as@(key',_) right)
      | key < key'    = balance color (ins left) as right
      | key > key'    = balance color left as (ins right)
      | otherwise     = T color left (key',el) right
      
maybeUpdate :: Ord a => a -> b -> ([b] -> [b]) -> Tree a [b] -> Tree a [b]
maybeUpdate  key el f tree = T B a y b
  where 
    T _ a y b = ins tree
    ins E = T R E (key,[el]) E
    ins (T color left as@(key',elOld) right)
      | key < key'    = balance color (ins left) as right
      | key > key'    = balance color left as (ins right)
      | otherwise     = T color left (key',f elOld) right

flatten :: Tree a b -> [(a,b)]
flatten E = []
flatten (T _ left (key,e) right) 
  = (flatten left) ++ ((key,e):(flatten right))

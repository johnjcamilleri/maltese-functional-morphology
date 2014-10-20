
module SharedString (shareString) where

import Data.HashTable.IO as H
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE stringPool #-}
stringPool :: BasicHashTable String String
stringPool = unsafePerformIO $ new

{-# NOINLINE shareString #-}
shareString :: String -> String
shareString s = unsafePerformIO $ do
    mv <- H.lookup stringPool s
    case mv of
	    Just s' -> return s'
	    Nothing -> do
		       H.insert stringPool s s
		       return s

{-
import Data.HashTable as H
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE stringPool #-}
stringPool :: HashTable String String
stringPool = unsafePerformIO $ new (==) hashString

{-# NOINLINE shareString #-}
shareString :: String -> String
shareString s = unsafePerformIO $ do
    mv <- H.lookup stringPool s
    case mv of
	    Just s' -> return s'
	    Nothing -> do
		       H.insert stringPool s s
		       return s
-}

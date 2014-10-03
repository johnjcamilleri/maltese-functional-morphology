-- Top-level functions:
-- * reading/writing morphology databases
-- * writing Lexicon, Tables, GF, XFST, Latex
-- * analysis/synthesis (Trie)

module GeneralIO where

import Print
import General
import Dictionary
import Trie
import System.IO
import Map
import Frontend
import Data.List (nub)
import Data.Maybe(fromJust)
import ErrM

type Stem      = String
type Id        = String

writeLex :: FilePath -> Dictionary -> IO ()
writeLex f m = writeFile f $ prFullFormLex $ dict2fullform m

outputLex m = putStrLn $ prFullFormLex $ dict2fullform m


writeTables :: FilePath -> Dictionary -> IO ()
writeTables f m = writeFile f $ prDictionary m

outputTables m =  putStrLn $ prDictionary m

writeGF :: FilePath -> FilePath -> Dictionary -> IO ()
writeGF f1 f2 m = writeFile f1 $
		  "-- machine-generated GF file\n\n" ++
		  "include " ++ f2 ++ " ;\n\n" ++
		  prGF m


outputGF f2 m = putStrLn $
		"-- machine-generated GF file\n\n" ++
		"include " ++ f2 ++ " ;\n\n" ++
		prGF m

writeGFRes :: FilePath -> FilePath -> Dictionary -> IO ()
writeGFRes f1 f2 m = writeFile f1 $
		  "-- machine-generated GF file\n\n" ++
		  "include " ++ f2 ++ " ;\n\n" ++
		  prGFRes m


outputGFRes f2 m = putStrLn $
		  "-- machine-generated GF file\n\n" ++
		  "include " ++ f2 ++ " ;\n\n" ++
		  prGFRes m

-- writeGF1 :: FilePath -> FilePath -> Dictionary -> IO ()
-- writeGF1 f1 f2 m = writeFile f1 $
-- 		   "-- machine-generated GF file\n\n" ++
--		   "include " ++ f2 ++ " ;\n\n" ++
--		   prGF1 m


writeXML :: FilePath -> Dictionary -> IO ()
writeXML f m = writeFile f $ prXML m

outputXML m = putStrLn $ prXML m

writeXFST :: FilePath -> Dictionary -> IO ()
writeXFST f m = writeFile f $
		"# machine-generated XFST file\n\n" ++
		prXFST m

outputXFST m = putStrLn $
	       "# machine-generated XFST file\n\n" ++
	       prXFST m

writeLEXC :: FilePath -> Dictionary -> IO ()
writeLEXC f m = writeFile f $
		"! machine-generated LEXC file\n\n" ++
		prLEXC m

outputLEXC m = putStrLn $
	       "! machine-generated LEXC file\n\n" ++
	       prLEXC m


writeLatex :: FilePath -> Dictionary -> IO ()
writeLatex f m = writeFile f $ "% machine-generated LaTeX file\n" ++
		 prLatex m

outputLatex m = putStrLn $ "% machine-generated LaTeX file\n" ++
		prLatex m


writeSQL :: FilePath -> Dictionary -> IO ()
writeSQL f m =  writeFile f $ prSQL m

outputSQL :: Dictionary -> IO ()
outputSQL m =  putStrLn $ prSQL m

{- this version not needed? AR

update :: Dictionary -> Dictionary -> Dictionary
update dict d = unionDictionary dict d


createDictionary :: FilePath -> IO ()
createDictionary f = writeDictionary emptyDict f

writeDictionary :: Dictionary -> FilePath -> IO ()
writeDictionary sm file
 = do h <- openFile file WriteMode
      hPutStr h $ unlines (map show (unDict sm))
      hClose h

readDictionary :: FilePath -> IO Dictionary
readDictionary file = do h <- openFile file ReadMode
 		         s <- hGetContents h
                         let ss = lines s
                         return $ dictionary (map (strings.read) ss)

-- readM :: String -> IO Entry
-- readM line = putChar '.' >> return (read line)

updateDictionary :: FilePath -> FilePath -> Dictionary -> IO ()
updateDictionary f1 f2 m0 = do
  m <- readDictionary f1
  writeDictionary (update m0 m) f2

-- Use the update_db function to extend your external resource with
-- more words. The update is non-destructive.
-- You should run 'createDictionary db' the first time you use it.

update_db :: Lang -> Dictionary -> IO ()
update_db l lex = do
  let dbl = db l
  m <- readDictionary dbl
  writeDictionary (update (d2d lex) m) dbl
-}

-- tries...

trieDict :: Dictionary -> SATrie
trieDict d = tcompile [(s, xs) | (s,xs) <- dict2fullform d]

-- buildTrie :: Dictionary -> SATrie
-- buildTrie = tcompile . dict2fullform

analysis :: SATrie -> ([Attr] -> Bool) -> String -> [[String]]
analysis trie f s =  [map snd ys | ys <- decompose trie f s]

-- trieLookup

synthesis :: Stem -> Dictionary -> [Entry]
synthesis s dict = [d | d@(s1,c,xs,t) <- unDict dict, s == s1]

lookupStem :: SATrie -> String -> [(Stem,Int)]
lookupStem trie s = [(s,read n) | s:n:_ <-  map (words . snd) (snd (tlookup trie s))]

synthesiser :: Language a => a -> Dictionary -> SATrie -> IO()
synthesiser l dict trie =
  do
    synt trie $ [((s1,n),(c,xs,t)) | ((s1,c,xs,t),n) <- zip (unDict dict) [0..]] |->++ Map.empty
 where synt trie table =
        do hPutStr stdout "> "
	   hFlush stdout
	   s <- hGetLine stdin
           case words s of
	     ["q"] -> return()
	     ["c"] -> do putStrLn $ unlines (paradigmNames l)
			 synt trie table
	     []  -> synt trie table
             [w] -> case(lookupStem trie w) of
	       [] -> do putStrLn $ "Word '" ++ w ++ "' not in the lexicon."
			synt trie table
	       xs   -> do putStrLn $ "\n" ++ (prDictionary (dictionary (nub (concat (map (lsynt table) xs)))))
	   	          synt trie table
             x:xs -> case (parseCommand l (unwords (x:xs))) of
			    Bad s -> do putStrLn s
			                synt trie table
			    Ok e  -> do putStrLn $ prDictionary $ dictionary [e]
			                synt trie table

       lsynt table (s,n) = nub [(s,b,c,d) | (b,c,d) <-  fromJust (table ! (s,n))]
infMode :: Language a => a  -> IO()
infMode l
        = do putStr "> "
	     hFlush stdout
	     s <- getLine
	     case (words s) of
	      ["q"] -> putStrLn "Session ended."
	      ["c"] -> do putStrLn $ unlines (paradigmNames l)
			  infMode l
	      (x:xs) -> do case (parseCommand l (unwords (x:xs))) of
			    Bad s -> do putStrLn s
			                infMode l
			    Ok e  -> do putStrLn $ prDictionary $ dictionary [e]
			                infMode l
	      _     -> do putStrLn "Give [command] [dictionary form]"
			  infMode l


imode :: Language a => a  -> IO()
imode l = interact (concat . map f . lines)
  where f s =
	 case (words s) of
	  (x:xs) -> do case (parseCommand l (unwords (x:xs))) of
		        Bad s -> s
		        Ok e  -> unlines
				 ["[" ++ unwords (x:xs) ++ "]",
				  prDictionary $ dictionary [e]]
	  _     -> do "Invalid format. Write: [command] [dictionary form]"

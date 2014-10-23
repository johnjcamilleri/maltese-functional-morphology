{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Helpers where

import General
import TypesMt

import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (isJust, fromJust)
import qualified Data.String.Utils as DSU

------------------------------------------------------------------------------
-- Basic definitions

-- _Letter        :: [String] = [ "a" , "b" , "ċ" , "d" , "e" , "f" , "ġ" , "g" , "għ" , "h" , "ħ" , "i" , "ie" , "j" , "k" , "l" , "m" , "n" , "o" , "p" , "q" , "r" , "s" , "t" , "u" , "v" , "w" , "x" , "ż" , "z" ]
_Consonant     :: [String] = [ "b" , "ċ" , "d" , "f" , "ġ" , "g" , "għ" , "h" , "ħ" , "j" , "k" , "l" , "m" , "n" , "p" , "q" , "r" , "s" , "t" , "v" , "w" , "x" , "ż" , "z" ]
_CoronalCons   :: [String] = [ "ċ" , "d" , "n" , "r" , "s" , "t" , "x" , "ż" , "z" ] -- "konsonanti xemxin"
_LiquidCons    :: [String] = [ "l" , "m" , "n" , "r" , "għ" ]
-- _SonorantCons  :: [String] = [ "l" , "m" , "n" , "r" ] -- See {SA pg13}. Currently unused, but see DoublingConsN below
_DoublingConsT :: [String] = [ "ċ" , "d" , "ġ" , "s" , "x" , "ż" , "z" ] -- require doubling when prefixed with 't', eg DDUM, ĠĠORR, SSIB, TTIR, ŻŻID {GM pg68,2b} {OM pg90}
_DoublingConsN :: [String] = [ "l" , "m" , "r" ] -- require doubling when prefixed with 'n', eg LLAĦĦAQ, MMUR, RRID {OM pg90}
_StrongCons    :: [String] = [ "b" , "ċ" , "d" , "f" , "ġ" , "g" , "għ" , "h" , "ħ" , "k" , "l" , "m" , "n" , "p" , "q" , "r" , "s" , "t" , "v" , "x" , "ż" , "z" ]
_WeakCons      :: [String] = [ "j" , "w" ]
_Vowel         :: [String] = [ "a" , "e" , "i" , "o" , "u" ]
_VowelIE       :: [String] = [ "a" , "e" , "i" , "ie" , "o" , "u" ]
_Empty         :: [String] = [ "" ]

------------------------------------------------------------------------------
-- Helpers

classifyRoot :: Root -> VClass
classifyRoot r
  | (c4 r == "") = classifyRoot3 r
  | otherwise    = classifyRoot4 r

classifyRoot3 :: Root -> VClass
classifyRoot3 r
  | check3 (_WeakCons, _StrongCons, _StrongCons) = Weak Assimilative
  | check3 (_StrongCons, _WeakCons, _StrongCons) = Weak Hollow
  | check3 (_StrongCons, _StrongCons, _WeakCons) = Weak Lacking
  | check3 (_StrongCons, _WeakCons, _WeakCons) = Weak Lacking
  | check3 (_Consonant, _Consonant, ["għ"]) = Weak Defective
  | check3 (_Consonant, _Consonant, _Consonant) =
      if c2 r == c3 r
      then Strong Geminated
      else if c2 r `elem` _LiquidCons
           then Strong LiquidMedial
           else Strong Regular
  | c1 r == "'" || c2 r == "'" || c3 r == "'" = Irregular
  | otherwise = error $ "Cannot classify root: "++c1 r++"-"++c2 r++"-"++c3 r
  where
    check3 (t1,t2,t3) = c1 r `elem` t1 && c2 r `elem` t2 && c3 r `elem` t3

classifyRoot4 :: Root -> VClass
classifyRoot4 r
  | check4 (_Consonant, _Consonant, _Consonant, _WeakCons) = Quad QWeak
  | check4 (_Consonant, _Consonant, _Consonant, _Consonant) = Quad QStrong
  | c1 r == "'" || c2 r == "'" || c3 r == "'" || c4 r == "'" = Irregular
  | otherwise = error $ "Cannot classify root: "++c1 r++"-"++c2 r++"-"++c3 r++"-"++c4 r
  where
    check4 (t1,t2,t3,t4) = c1 r `elem` t1 && c2 r `elem` t2 && c3 r `elem` t3 && c4 r `elem` t4

extractVowels :: String -> Vowels = \s ->
  let
    isVow x = [x] `elem` _Vowel
    a = dropWhile (not.isVow) s
    v1 = takeWhile (isVow) a
    b = dropWhile (not.isVow) $ drop (length v1) a
    v2 = takeWhile (isVow) b
  in
  Vowels v1 v2

takePfx = take  -- takePfx 3 "hello" = "hel"
dropPfx = drop  -- dropPfx 3 "hello" = "lo"
takeSfx = dp    -- takeSfx 3 "hello" = "llo"
dropSfx = tk    -- dropSfx 3 "hello" = "he"

takePfxS :: Int -> Str -> Str = mapStr . take
dropPfxS :: Int -> Str -> Str = mapStr . drop
takeSfxS :: Int -> Str -> Str = mapStr . dp
dropSfxS :: Int -> Str -> Str = mapStr . tk

-- Prefix with a 'n'/'t' or double initial consonant, as necessary. See {OM pg 90}
pfx_N :: Str -> Str = mapStr pfx_N'
pfx_N' :: String -> String
pfx_N' s
  | s == "" = ""
  | [head s] `elem` _DoublingConsN = (head s) : s
  | otherwise = 'n' : s

pfx_T :: Str -> Str = mapStr pfx_T'
pfx_T' :: String -> String
pfx_T' s
  | s == "" = ""
  | [head s] `elem` _DoublingConsT = (head s) : s
  | otherwise = 't' : s

pfx_J  :: Str -> Str = mapStr pfx_J'
pfx_J' :: String -> String = \s -> pfx' "j" s

-- Generically prefix a string (avoiding empty strings)
pfx :: Str -> String -> Str
pfx ss s = mapStr (pfx' s) ss
pfx' :: String -> String -> String = \p s -> case (p,s) of {
  (_ , "" ) -> [] ;
  ("", str) -> str ;
  (px, str) -> px ++ str
  } ;

-- Add suffix, avoiding blanks and triple letters {GO pg96-7}
sfx :: Str -> String -> Str
sfx ss s = mapStr (sfx' s) ss
sfx' :: String -> String -> String
sfx' kenn ni
  | kenn == ""                 = ""
  | (x' == x'') && (x'' == y1) = dropSfx 1 kenn ++ ni
  | otherwise                  = kenn ++ ni
  where
    x'  = init (takeSfx 2 kenn) -- s[n-1]
    x'' = takeSfx 1 kenn        -- s[n]
    y1  = takePfx 1 ni

-- Replace any IE in the word with an I or E
ie2i  :: Str -> Str = mapStr ie2i'
ie2i' :: String -> String = ie2_' "i" ;

ie2e  :: Str -> Str = mapStr ie2e'
ie2e' :: String -> String = ie2_' "e" ;

ie2_ :: String -> Str -> Str = \x -> mapStr (ie2_' x)
ie2_' :: String -> String -> String = \iore serviet ->
  DSU.replace "ie" iore serviet

-- | Synonym for isPrefixOf, to be used infix
startsWith :: Str -> String -> Bool
startsWith (Str ss) s = and $ map (isPrefixOf s) ss
-- startsWith :: Eq a => [a] -> [a] -> Bool
-- startsWith a b = isPrefixOf b a

-- | Synonym for isSuffixOf, to be used infix
endsWith :: Str -> String -> Bool
endsWith (Str ss) s =  and $ map (isSuffixOf s) ss
-- endsWith :: Eq a => [a] -> [a] -> Bool
-- endsWith a b = isSuffixOf b a

-- | Check a list of substrings occur in order in a string
--   e.g. matches ["h","ll"] "hello"  = True
--        matches ["ll","he"] "hello" = False
matches :: [String] -> String -> Bool
matches [] s = True
matches (p:ps) s =
  case find p s of
    Just rest -> matches ps rest
    Nothing -> False
  where
    find _ [] = Nothing
    find x y
      | x `isPrefixOf` y = Just (drop (length x) y)
      | otherwise = find x (tail y)

-- | Check a list of substrings (character classes) occur in order in a string
--   e.g. matches [vowels, vowels]         "hello" = Just ["e","o"]
--        matches [vowels, vowels, vowels] "hello" = Nothing
--          where vowels = ["a","e","i","o","u"]
matchesClasses :: [[String]] -> String -> Maybe [String]
matchesClasses [] s = Just []
matchesClasses (p:ps) s =
  case findAny p s of
    Just (c,rest) -> maybe Nothing (\tail -> Just $ c : tail) (matchesClasses ps rest)
    Nothing -> Nothing

  where
    fap = findAny p s

    findAny :: [String] -> String -> Maybe (String,String)
    findAny cs s =
      let search = [ (c,fromJust (find c s)) | c <- cs, isJust (find c s) ]
      in if null search then Nothing else Just (fst (head search), snd (head search))

    find _ [] = Nothing
    find x y
      | x `isPrefixOf` y = Just (drop (length x) y)
      | otherwise = find x (tail y)

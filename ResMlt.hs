{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module ResMlt where

import qualified Data.String.Utils as DSU

------------------------------------------------------------------------------
-- Prelude

takePfx = take
dropSfx = init

nonExist = ""

------------------------------------------------------------------------------
-- Basics

type Str = String

data Number = Sg | Pl
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param Number where values = enum

data Gender = Masc | Fem
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param Gender where values = enum

data GenNum =
    GSg Gender -- DAK, DIK
  | GPl        -- DAWK
  deriving (Show,Eq,Ord)
instance Param GenNum where
  values = [GSg Masc, GSg Fem, GPl]

data Person = P1 | P2 | P3
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param Person where values = enum

data Polarity = Pos | Neg
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param Polarity where values = enum

-- data VAgr =
--     P1Sg
--   | P2Sg
--   | P3SgMasc
--   | P3SgFem
--   | P1Pl
--   | P2Pl
--   | P3Pl
--   deriving (Show,Eq,Enum,Ord,Bounded)
-- instance Param VAgr where values = enum

data VAgr =
    AgP1 Number
  | AgP2 Number
  | AgP3Sg Gender
  | AgP3Pl
  deriving (Show,Eq,Ord)

data VForm =
    VPerf VAgr    -- Perfect tense in all pronoun cases
  | VImpf VAgr    -- Imperfect tense in all pronoun cases
  | VImp Number   -- Imperative is always P2, Sg & Pl
  deriving (Show,Eq,Ord)

data VSuffixForm =
    VSuffixNone  -- ftaħt
  | VSuffixDir VAgr  -- ftaħtu
  | VSuffixInd VAgr  -- ftaħtlu
  | VSuffixDirInd GenNum VAgr  -- ftaħthulu
  deriving (Show,Eq,Ord)

data VerbInfo = VerbInfo {
    v_class :: VClass
  , v_form  :: VDerivedForm
  , v_root  :: Root  -- radicals
  , v_vseq  :: Vowels  -- vowels extracted from mamma
  , v_imp   :: Str  -- Imperative Sg. gives so much information jaħasra!
  }
mkVerbInfo = VerbInfo

data VDerivedForm =
    FormI
  | FormII
  | FormIII
  | FormIV
  | FormV
  | FormVI
  | FormVII
  | FormVIII
  | FormIX
  | FormX
  | FormUnknown
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param VDerivedForm where values = enum

-- Verb classification
data VClass =
    Strong VStrongClass
  | Weak VWeakClass
  | Quad VQuadClass
  | Loan
  | Irregular
  deriving (Show,Eq,Ord)

data VStrongClass =
    Regular
  | LiquidMedial
  | Geminated
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param VStrongClass where values = enum

data VWeakClass =
    Assimilative
  | Hollow
  | Lacking
  | Defective
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param VWeakClass where values = enum

data VQuadClass =
    QStrong
  | QWeak
  deriving (Show,Eq,Enum,Ord,Bounded)
instance Param VQuadClass where values = enum

data Vowels = Vowels { v1, v2 :: Str }
  deriving (Eq, Show)
mkVowels1 x = Vowels x ""
mkVowels2 = Vowels

data Root = Root {c1, c2, c3, c4 :: Str }
  deriving (Eq, Show)

data Verb = Verb {
    s :: VForm ==> VSuffixForm ==> Polarity ==> Str
  , i :: VerbInfo
  }

vowel  = [ 'a' , 'e' , 'i' , 'o' , 'u' ]
-- vowels = [ "a" , "e" , "i" , "ie" , "o" , "u" ]

doublingConsT = [ 'ċ' , 'd' , 'ġ' , 's' , 'x' , 'ż' , 'z' ] -- require doubling when prefixed with 't', eg DDUM, ĠĠORR, SSIB, TTIR, ŻŻID {GM pg68,2b} {OM pg90}
doublingConsN = [ 'l' , 'm' , 'r' ] -- require doubling when prefixed with 'n', eg LLAĦĦAQ, MMUR, RRID {OM pg90}

------------------------------------------------------------------------------
-- Helpers

classifyRoot :: Root -> VClass = \r ->
  Strong Regular -- TODO

extractVowels :: Str -> Vowels = \s ->
  let
    isVow x = x `elem` vowel
    a = dropWhile (not.isVow) s
    v1 = takeWhile (isVow) a
    b = dropWhile (not.isVow) $ drop (length v1) a
    v2 = takeWhile (isVow) b
  in
  Vowels v1 v2

-- Prefix with a 'n'/'t' or double initial consonant, as necessary. See {OM pg 90}
pfx_N :: Str -> Str
pfx_N s
  | s == "" = ""
  | head s `elem` doublingConsN = (head s) : s
  | otherwise = 'n' : s

pfx_T :: Str -> Str
pfx_T s
  | s == "" = ""
  | head s `elem` doublingConsT = (head s) : s
  | otherwise = 't' : s

pfx_J :: Str -> Str = \s -> pfx "j" s

-- Generically prefix a string (avoiding empty strings)
pfx :: Str -> Str -> Str = \p s -> case (p,s) of {
  (_ , "" ) -> [] ;
  ("", str) -> str ;
  (px, str) -> px ++ str
  } ;

-- Add suffix, avoiding triple letters {GO pg96-7}
--- add more cases?
--- potentially slow
-- sfx : Str -> Str -> Str = \a,b ->
--   case <a,takePfx 1 b> of {
--     <"",_> -> [] ;
--     <ke+"nn","n"> -> ke+"n"+b ;
--     <ha+"kk","k"> -> ha+"k"+b ;
--     <ho+"ll","l"> -> ho+"l"+b ;
--     <si+"tt","t"> -> si+"t"+b ;
--     <be+"xx","x"> -> be+"x"+b ;
--     _ -> a + b
--   } ;

-- Replace any IE in the word with an I or E    --- potentially slow
ie2i :: Str -> Str = ie2_ "i" ;
ie2e :: Str -> Str = ie2_ "e" ;
ie2_ :: Str -> Str -> Str = \iore serviet ->
  DSU.replace "ie" iore serviet
  -- case serviet of {
  --   x + "ie" => x + iore ;
  --   x + "ie" + y => x + iore + y ;
  --   x => x
  -- } ;

------------------------------------------------------------------------------
-- Functional morphology stuff

(!) = ($)

type a ==> b = a -> b
infixr ==>

table :: Param a => (a -> Str) -> [(a,Str)]
table f = [(v, f v) | v <- values]

class (Eq a, Show a) => Param a where
  values :: [a]
  value :: Int -> a
  value0 :: a
  prValue :: a -> Str
  value n = values !! n
  value0 = value 0
  prValue = show

enum :: (Enum a, Bounded a) => [a]
enum = [minBound..maxBound]

-- type Str = [String]

-- strings :: [String] -> Str
-- string = id

-- mkStr :: String -> Str
-- mkStr = (:[])

-- nonExist :: Str
-- nonExist = []

tk :: Int -> String -> String
tk i s = take (max 0 (length s - i)) s

dp :: Int -> String -> String
dp i s = drop (max 0 (length s - i)) s

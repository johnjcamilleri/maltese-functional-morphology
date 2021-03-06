{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module TypesMt where

import General
import Invariant

------------------------------------------------------------------------------
-- Trying to mimic GF syntax

(!) = ($)

type a ==> b = a -> b
infixr ==>

------------------------------------------------------------------------------
-- Parameter types for Maltese morphology

data Number = Sg | Pl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param Number where values = enum

data Gender = Masc | Fem
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param Gender where values = enum

data GenNum =
    ASg Gender -- DAK, DIK
  | APl        -- DAWK
  deriving (Eq, Ord, Show, Read)
instance Param GenNum where values = map ASg values ++ [APl]

data Person = P1 | P2 | P3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param Person where values = enum

data Polarity = Pos | Neg
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param Polarity where values = enum

data Vowels = Vowels { v1, v2 :: String }
  deriving (Eq, Ord, Show, Read)
mkVowels1 x = Vowels x ""
mkVowels2 = Vowels

data Root = Root {c1, c2, c3, c4 :: String }
  deriving (Eq, Ord, Show, Read)

-- data Verb = Verb {
--     s :: VForm ==> VSuffixForm ==> Polarity ==> Str
--   , presPart :: GenNum ==> Str  -- Present/active particible, e.g. RIEQED
--   , pastPart :: GenNum ==> Str  -- Passive/past particible, e.g. MAĦBUB
--   , i :: VerbInfo
--   }
type Verb = VForm -> Str

data VAgr =
    AgP1 Number
  | AgP2 Number
  | AgP3Sg Gender
  | AgP3Pl
  deriving (Eq, Ord, Show, Read)
instance Param VAgr where
  values = map AgP1 values ++ map AgP2 values ++ map AgP3Sg values ++ [AgP3Pl]

data VForm =
    VPerf VAgr  VSuffixForm Polarity   -- Perfect tense in all pronoun cases
  | VImpf VAgr  VSuffixForm Polarity   -- Imperfect tense in all pronoun cases
  | VImp Number VSuffixForm Polarity   -- Imperative is always P2, Sg & Pl
  | VPresPart GenNum   -- Present/active particible, e.g. RIEQED
  | VPastPart GenNum   -- Passive/past particible, e.g. MAĦBUB
  deriving (Eq, Ord, Show, Read)
instance Param VForm where
  values =
    [VPerf agr sfx pol | agr<-values, sfx<-values, pol<-values] ++
    [VImpf agr sfx pol | agr<-values, sfx<-values, pol<-values] ++
    [VImp  agr sfx pol | agr<-values, sfx<-values, pol<-values] ++
    [VPresPart gnum | gnum<-values] ++
    [VPastPart gnum | gnum<-values]

data VSuffixForm =
    VSuffixNone  -- ftaħt
  | VSuffixDir VAgr  -- ftaħtu
  | VSuffixInd VAgr  -- ftaħtlu
  | VSuffixDirInd GenNum VAgr  -- ftaħthulu
  deriving (Eq, Ord, Show, Read)
instance Param VSuffixForm where
  values = VSuffixNone : map VSuffixDir values ++ map VSuffixInd values ++ [VSuffixDirInd a b | a<-values, b<-values]

data VerbInfo = VerbInfo {
    v_class :: VClass
  , v_form  :: VDerivedForm
  , v_root  :: Root  -- radicals
  , v_vseq  :: Vowels  -- vowels extracted from mamma
  , v_imp   :: Str  -- Imperative Sg.
  }
  deriving (Eq, Ord, Show, Read)
mkVerbInfo = VerbInfo
-- mkVerbInfo :: VClass -> VDerivedForm -> Root -> Vowels -> Str -> VerbInfo
-- mkVerbInfo c f r v (Str (s:_)) = VerbInfo c f r v s

data VClass =
    Strong VStrongClass
  | Weak VWeakClass
  | Quad VQuadClass
  | Loan
  | Irregular
  deriving (Eq, Ord, Show, Read)
instance Param VClass where
  values = map Strong values ++ map Weak values ++ map Quad values ++ [Loan, Irregular]

data VStrongClass =
    Regular
  | LiquidMedial
  | Geminated
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param VStrongClass where values = enum

data VWeakClass =
    Assimilative
  | Hollow
  | Lacking
  | Defective
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param VWeakClass where values = enum

data VQuadClass =
    QStrong
  | QWeak
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param VQuadClass where values = enum

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Param VDerivedForm where values = enum

-- ===========================================================================

-- -- parameter types for Swedish morphology

-- -- enumerated parameter types

-- data Genus = Utr | Neutr
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Numerus = Sg | Pl
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Species = Indef | Def
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Casus = Nom | Gen
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Sex = NoMasc | Masc
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Modus = Ind | Conj
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Vox = Act | Pass
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Grade = Posit | Compar | Superl
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data Person = P1 | P2 | P3
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param Genus   where values = enum
-- instance Param Numerus where values = enum
-- instance Param Species where values = enum
-- instance Param Casus   where values = enum
-- instance Param Sex     where values = enum
-- instance Param Modus   where values = enum
-- instance Param Vox     where values = enum
-- instance Param Grade   where values = enum
-- instance Param Person  where values = enum
-- --- would we need something more in Haskell?

-- -- real parameter types: mostly hierarchical

-- -- substantives (= common nouns)

-- type Substantive = SubstForm -> Str

-- data SubstForm = SF Numerus Species Casus | SubstComp
--   deriving (Eq, Ord, Show, Read)

-- instance Param SubstForm
--   where values = [SF a b c | a <- values, b <- values, c <- values] ++ [SubstComp]
-- 	prValue (SF a b c) = prValue a ++ " " ++ prValue b ++ " " ++ prValue c
-- 	prValue (SubstComp) = "Komposit"

-- -- adjectives

-- type Adjective = AdjForm -> Str

-- data GenNum = ASg Genus | APl
--   deriving (Eq, Ord, Show, Read)

-- data SexNum = AxSg Sex | AxPl
--   deriving (Eq, Ord, Show, Read)

-- data AdjFormPos =
--     Strong GenNum
--   | Weak   SexNum
--   deriving (Eq, Ord, Show, Read)

-- data AdjFormSuper = SupStrong | SupWeak
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- data AdjFormGrad =
--     Pos    AdjFormPos
--   | Comp
--   | Super  AdjFormSuper
--   deriving (Eq, Ord, Show, Read)

-- data AdjForm = AF AdjFormGrad Casus
--   deriving (Eq, Ord, Show, Read)


-- instance Param GenNum where
--   values = map ASg  values ++ [APl]  -- needed for articles

-- instance Param SexNum where
--   values = map AxSg values ++ [AxPl] -- needed for pronouns

-- instance Param AdjFormPos where
--   values = map Strong values ++ map Weak values

-- instance Param AdjFormSuper where values = enum

-- instance Param AdjFormGrad where
--   values = map Pos values ++ [Comp] ++ map Super values

-- instance Param AdjForm where
--   values = [AF a c | a <- values, c <- values]
--   prValue (AF a c) = prValue a ++ " " ++ prValue c

-- -- verbs

-- type Verb = VerbForm -> Str

-- data VFin =
--    Pres Modus Vox
--  | Pret Modus Vox
--  | Imper          --- no passive
--   deriving (Eq, Ord, Show, Read)

-- data VInf =
--    Inf Vox
--  | Sup Vox
--  | PtPres Casus
--  | PtPret AdjFormPos Casus
--   deriving (Eq, Ord, Show, Read)

-- data VerbForm =
--    VF VFin
--  | VI VInf
--   deriving (Eq, Ord, Show, Read)

-- instance Param VFin where
--   values = map (uncurry Pres) mvs ++ map (uncurry Pret) mvs ++ [Imper] where
--     mvs = [(m,v) | m <- values, v<- values]

-- instance Param VInf where
--   values = map Inf values ++ map Sup values ++ map PtPres values ++
--            [PtPret a c | a <- values, c <- values]

-- instance Param VerbForm where
--   values = map VF values ++ map VI values
--   value0 = VI (Inf Act) -- to show the infinitive as dictionary form
--   prValue (VF f) = prValue f
--   prValue (VI f) = prValue f

-- -- price to pay for hierarchical types: if we want to define non-passive verbs
-- passiveForms :: [VerbForm]
-- passiveForms =
--   map VF (concat [[Pres m Pass, Pret m Pass] | m <- values]) ++
--   map VI [Inf Pass, Sup Pass] ++
--   partPretForms

-- partPretForms :: [VerbForm]
-- partPretForms = [VI (PtPret a c) | a <- values, c <- values]

-- positiveForms :: [AdjForm]
-- positiveForms = [AF (Pos a) c | a <- values, c <- values]

-- -- adverbs

-- type Adverb = AdverbForm -> Str

-- data AdverbForm = AdverbForm Grade
--   deriving (Eq, Ord, Show, Read)

-- instance Param AdverbForm where
-- 			  values = [AdverbForm g | g <- values]
-- 			  prValue (AdverbForm g) = prValue g

-- -- invariant adverbs

-- type AdverbInv = AdverbInvForm -> Str

-- data AdverbInvForm = AdverbInvForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param AdverbInvForm where
-- 			     values = enum
-- 			     prValue _ = invar

-- -- invariant interrogative adverbs

-- type InterrogInv = InterrogInvForm -> Str

-- data InterrogInvForm = InterrogInvForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param InterrogInvForm where
-- 			     values = enum
-- 			     prValue _ = invar


-- -------------------------------
-- -- closed classes -------------
-- -------------------------------

-- -- pronouns

-- type PronPN  = PronCasus -> Str
-- type PronAdj = AdjPronForm -> Str

-- data PronCasus = PNom | PAcc | PGen GenNum
--   deriving (Eq, Ord, Show, Read)

-- instance Param PronCasus where
--   values = PNom : PAcc : map PGen values

-- data AdjPronForm = AP GenNum Casus
--   deriving (Eq, Ord, Show, Read)


-- instance Param AdjPronForm where
--   values = [AP g c | g <- values, c <- values]

-- type PronInv = PronInvForm -> Str

-- data PronInvForm = PronInvForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param PronInvForm where
-- 			     values = enum
-- 			     prValue _ = invar

-- type NumeralInv = NumeralInvForm -> Str

-- data NumeralInvForm = NumeralInvForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param NumeralInvForm where
-- 			     values = enum
-- 			     prValue _ = invar



-- -- invariant
-- data InterjForm = InterjForm
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- type Interjection = InterjForm -> Str

-- instance Param InterjForm where
--   values = enum
--   prValue _ = invar

-- -- articles

-- type Article = ArticleForm -> Str

-- data ArticleForm = ArticleForm GenNum
--  deriving(Eq,Ord,Show,Read)

-- instance Param ArticleForm where
--    values = [ArticleForm g | g <- values]
--    prValue (ArticleForm g) = prValue g

-- -- auxiliary verbs

-- data AuxVerbForm = AuxInf | AuxPres | AuxPret | AuxSup
--   deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param AuxVerbForm where values = enum

-- type AuxVerb = AuxVerbForm -> Str

-- -- Prepositions

-- type Preposition = PrepForm -> Str

-- data PrepForm = PrepForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param PrepForm where
-- 			values = enum
-- 			prValue _ = invar

-- -- Conjunction

-- type Conjunction = ConjForm -> Str

-- data ConjForm = ConjForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param ConjForm where
-- 			values = enum
-- 			prValue _ = invar

-- -- Subjunction

-- type Subjunction = SubForm -> Str

-- data SubForm = SubForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param SubForm where
-- 		       values = enum
-- 		       prValue _ = invar

-- -- Particles

-- type Particle = PartForm -> Str

-- data PartForm = PartForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param PartForm where
-- 			values = enum
-- 			prValue _ = invar

-- -- Infinitive mark
-- type InfMark = InfMarkForm -> Str

-- data InfMarkForm = InfMarkForm
--  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- instance Param InfMarkForm where
-- 			   values = enum
-- 			   prValue _ = invar


-- -- Proper Noun
-- type PN = PNForm -> Str

-- data PNForm = PNForm Casus
--  deriving (Eq, Ord, Show, Read)

-- instance Param PNForm where
-- 		      values = [PNForm c | c <- values]
-- 		      prValue (PNForm c) = prValue c

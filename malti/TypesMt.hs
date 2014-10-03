module TypesMt where

import General
import Invariant

-- Parameter types for Maltese morphology

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

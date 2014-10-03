module BuildMt where

import General
import Dictionary
import TypesMt
import RulesMt
import AttrMt

-- -- combinators for lexicographers

-- substantive :: Substantive -> Genus -> Entry
-- substantive n g = entryI n [prValue g]

-- adjective :: Adjective -> Entry
-- adjective = entry

-- verb :: Verb -> Entry
-- verb = entry

-- verbs :: [String] -> Verb -> [Entry]
-- verbs prefs v = map verb [\f -> strings (map (p ++) (unStr (v f))) | p <- prefs]

-- mkPN :: String -> PN
-- mkPN s (PNForm c) = mkStr $ mkCasus c s

-- pnMasc :: String -> Entry
-- pnMasc s = entryI (mkPN s) [prValue Utr]

-- pnUtr :: String -> Entry
-- pnUtr s = entryI (mkPN s) [prValue Utr]

-- pnNeutr :: String -> Entry
-- pnNeutr s = entryI (mkPN s) [prValue Neutr]

-- adverbInv :: String -> Entry
-- adverbInv = entry . mkAdverbInv

-- mkAdverbInv :: String -> AdverbInv
-- mkAdverbInv s _ = mkStr s


-- --- NB the unused function mkAdverbInv is necessary to resolve
-- --- overloading. AR 24/5/2004
-- interrogInv :: String -> Entry
-- interrogInv = entry . mkAdverbInv
-- mkInterrogInv :: String -> InterrogInv
-- mkInterrogInv s _ = mkStr s

-- mkPronInv :: String -> PronInv
-- mkPronInv s _ = mkStr s
-- pronInv :: String -> Entry
-- pronInv = entry . mkPronInv

-- mkNumeralInv :: String -> NumeralInv
-- mkNumeralInv s _ = mkStr s
-- numeralInv :: String -> Entry
-- numeralInv = entry . mkNumeralInv



-- adverb :: Adverb -> Entry
-- adverb = entry

-- adverbBra :: String -> String -> String -> Entry
-- adverbBra x y z = entry $ mkAdverb x y z

-- advReg :: String -> Entry
-- advReg = adverb . adverbReg

-- interj :: String -> Entry
-- interj = entry . mkInterj

-- mkInterj :: String -> Interjection
-- mkInterj s _ = mkStr s

-- pronJag :: String -> String -> String -> String -> Entry
-- pronJag a b c d = entry $ mkPronJag a b c d

-- pronNagon :: String -> String -> String -> Entry
-- pronNagon a b c = entry $ mkPronNagon a b c

-- pronHan :: String -> String -> String -> Entry
-- pronHan a b c = entry $ mkPronHan a b c

-- pronDylik :: String -> Entry
-- pronDylik = entry . mkPronDylik

-- -- the closed class of personal pronouns: so we manage without Types in Lexicon
-- -- it's not clear if person and sex are really necessary to give in morphology
-- pronounsPN :: [Entry]
-- pronounsPN =
--     [
--      entry pronRefl,
--      pronJag "jag" "mig" "min" "mitt",
--      pronJag "du"  "dig" "din" "ditt",
--      pronHan "han" "honom" "hans",
--      pronHan "hon" "henne" "hennes",
--      pronJag "vi"  "oss" "vår" "vårt",
--      pronJag "ni"  "er"  "er" "ert",
--      pronHan "de"  "dem"  "deras",
--      pronHan "den" "den" "dess",
--      pronHan "det" "det" "dess"
--     ]

-- -- worst-case

-- mkSubst :: Genus -> String -> String -> String -> String -> String -> Entry
-- mkSubst g a b c d e = substantive (mkSubstantive a b c d (mkStr e)) g

-- mkSubstU = mkSubst Utr
-- mkSubstN = mkSubst Neutr
-- mkSubstSgU x y =
--   substantive (noPlural (mkSubstantive x y x x (mkStr x))) Utr
-- mkSubstSgN x y =
--   substantive (noPlural (mkSubstantive x y x x (mkStr x))) Neutr

-- mkAdj :: String -> String -> String -> String -> Entry
-- mkAdj a b c d = adjective $ adjIrreg a b c d

-- mkVerbIrreg :: String -> String -> String -> String  -> String -> Entry
-- mkVerbIrreg a b c d f = verb $ verbIrreg a b c d f

-- -- Conjunctions, subjunctions, prepositions and particles.

-- conj :: String -> Entry
-- conj = entry . mkConj

-- mkConj :: String -> Conjunction
-- mkConj s _ = mkStr s

-- subj :: String -> Entry
-- subj = entry . mkSubj

-- mkSubj :: String -> Subjunction
-- mkSubj s _ = mkStr s

-- prep :: String -> Entry
-- prep = entry . mkPrep

-- mkPrep :: String -> Preposition
-- mkPrep s _ = mkStr s

-- part :: String -> Entry
-- part = entry . mkPart

-- mkPart :: String -> Particle
-- mkPart s _ = mkStr s

-- -- the commonest cases: all these take one string, the "dictionary form"

-- substUtrum s   = substantive s Utr
-- substNeutrum s = substantive s Neutr
-- substMasc s    = substantive s Utr

-- s1 = substUtrum . decl1
-- s2 = substUtrum . decl2
-- s3 = substUtrum . decl3
-- s4 = substNeutrum . decl4
-- s5 = substNeutrum . decl5

-- sRos    = substUtrum . decl1ros
-- sGata   = substUtrum . decl1gata
-- sFlicka = substUtrum . decl1flicka
-- sNyckel = substUtrum . decl2nyckel
-- sKam    = substUtrum . decl2kam
-- sParti  = substNeutrum . decl3parti
-- sMuseum = substNeutrum . decl3museum
-- sPapper = substNeutrum . decl5papper
-- sKikare = substUtrum . decl5kikare
-- sProgram = substNeutrum . decl5program

-- v1 = verb . verbWeak1
-- v2 = verb . verbWeak2
-- v3 = verb . verbWeak3

-- aReg    = adjective . adjReg
-- aFager  = adjective . adjFager
-- aGrund  = adjective . adjGrund
-- aVid    = adjective . adjVid
-- aVaken  = adjective . adjVaken
-- aKorkad = adjective . adjKorkad
-- aAbstrakt = adjective . adjAbstrakt
-- aUdda = adjective . noCompare . const . mkStr  -- not inflected

-- vAI = verb . verbStrongGiva
-- vAU = verb . verbStrongFinna
-- vEI = verb . verbStrongSmita
-- vOA = verb . verbStrongFara
-- vOO = verb . verbStrongKomma
-- vÅÄ = verb . verbStrongAeta
-- vÄÅ = verb . verbStrongLaata
-- vÖA = verb . verbStrongFalla
-- vÖU = verb . verbStrongSupa

-- --- vIrr sta stod statt standen = verb $ verbIrreg sta stod statt standen
-- vStrong sitta satt suttit   = verb $ verbStrong sitta satt suttit

-- -- the following heuristics take care of many common cases: extend them if you like

-- hs :: String -> Entry
-- hs s = case reverse s of
--   'n':'o':'i':_ -> s3 s                       -- stat-ion
--   'a':_         -> s1 s                       -- summ-a
--   'e':'r':'a':_ -> substUtrum (decl5kikare s) -- lär-are
--   'g':'n':_     -> s2 s                       -- stuvni-ng, ku-ng
--   _             -> s2 s  -- NOT RECOMMENDED


-- -- common exceptions

-- noPlural :: Substantive -> Substantive
-- noPlural v = v `only` [SF Sg s c | s <- values, c <- values]

-- noCompare :: Adjective -> Adjective
-- noCompare a = a `only` positiveForms

-- noPassive :: Verb -> Verb
-- noPassive v = v `missing` passiveForms

-- noPartPret :: Verb -> Verb
-- noPartPret v = v `missing` partPretForms

-- shortLong :: String -> Verb -> Verb
-- shortLong = verbShortLong

-- sIrreg = mkSubstantive
-- aIrreg = mkAdjective
-- vIrreg = mkVerb

-- infMark :: String -> Entry
-- infMark = entry . mkInfMark

-- mkInfMark :: String -> InfMark
-- mkInfMark s _ = mkStr s

-- -- Dict instances
-- instance Dict SubstForm where
--     category _ = "Substantiv"
--     defaultAttr _ = atW
--     attrException _= (SubstComp,atP):[((SF Sg Indef n), atWP) | n <- values]

-- instance Dict AdjForm where
-- 		      category _ = "Adjektiv"
-- 		      defaultAttr _ = atW

-- instance Dict VerbForm where
-- 		       category _ = "Verb"
-- 		       defaultAttr _ = atW

-- instance Dict AdverbForm where category _ = "Adverb"

-- instance Dict AdverbInvForm where category _ = "Adverb"

-- instance Dict PronCasus where category _ = "Pronomen"

-- instance Dict InterjForm where category _ = "Interjektion"

-- instance Dict ArticleForm where category _ = "Artikel"

-- instance Dict AuxVerbForm where category _ = "AuxVerb"

-- instance Dict PrepForm where category _ = "Preposition"
-- instance Dict ConjForm where category _ = "Konjunktion"
-- instance Dict SubForm where category _ = "Subjunktion"

-- instance Dict PartForm where  category _ = "Partikel"

-- instance Dict InfMarkForm where category _ = "Infinitivmärke"

-- instance Dict PNForm where category _ = "Egennamn"

-- instance Dict AdjPronForm where category _ = "Pronomen"

-- instance Dict PronInvForm where category _ = "Pronomen"
-- instance Dict NumeralInvForm where category _ = "Numeral"
-- instance Dict InterrogInvForm where category _ = "Interrogativ"

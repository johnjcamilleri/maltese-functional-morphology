{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module RulesMt where

import General
import TypesMt

-- Ensure file encoding: ċġħżĊĠĦŻ

main = putStrLn "RulesMt" -- just for testing

------------------------------------------------------------------------------
-- Verbs

strongVerb :: Root -> Vowels -> (Number ==> String) -> Verb
strongVerb root vseq imp =
  \v -> case v of
    VPerf agr sfx pol -> (if pol==Pos then id else verbNeg info) $ verbPerfPronSuffixTable info ( conjStrongPerf root vseq ) ! agr ! sfx
    -- VPerf agr sfx pol -> undefined -- ( conjStrongPerf root vseq ) ! agr
    VImpf agr sfx pol -> undefined -- ( conjStrongImpf (imp ! Sg) (imp ! Pl) ) ! agr
    VImp  n   sfx pol -> undefined -- imp ! n
    VPresPart gnum -> nonExist -- TODO
    VPastPart gnum -> nonExist -- TODO
  where
    info :: VerbInfo = mkVerbInfo (Strong Regular) (FormI) root vseq (imp ! Sg) ;

conjStrongPerf :: Root -> Vowels -> (VAgr ==> Str) = \root p -> undefined
conjStrongImpf :: Str -> Str -> (VAgr ==> Str) = \imp_sg imp_pl -> undefined

verbPronSuffixTable :: VerbInfo -> (VForm ==> Str) -> (VForm ==> VSuffixForm ==> Str) = \info tbl -> undefined

verbPerfPronSuffixTable :: VerbInfo -> (VAgr ==> Str) -> (VAgr ==> VSuffixForm ==> Str) = \info tbl -> undefined
verbImpfPronSuffixTable :: VerbInfo -> (VAgr ==> Str) -> (VAgr ==> VSuffixForm ==> Str) = \info tbl -> undefined
verbImpPronSuffixTable :: VerbInfo -> (Number ==> Str) -> (Number ==> VSuffixForm ==> Str) = \info tbl -> undefined
verbDirIndSuffixTable :: VAgr -> GenNum -> Str -> (VAgr ==> Str) = \subj dobj ftaht -> undefined

-- verbPolarityTable :: VerbInfo -> (VForm ==> VSuffixForm ==> Str) -> (VForm ==> VSuffixForm ==> Polarity ==> Str) = \info tbl -> undefined
verbNeg :: VerbInfo -> Str -> Str = \info s -> undefined


-- ===========================================================================

-- --2 Substantives
-- --
-- -- We start with substantives, i.e. common nouns. We follow the same pattern
-- -- in all word classes: we start with a *worst-case macro* and branch into
-- -- more and more specific classes (*declensions* and *conjugations*), eventually
-- -- ending up with functions that only need the dictionary form to apply.
-- --
-- -- The worst-case macro for substantives needs four characteristic forms
-- -- (the same as are sometimes cited as the *theme* of a substantive).
-- --
-- -- To get an example of each inflectional pattern, look at the name of
-- -- the pattern variable!

-- mkSubstantive :: String -> String -> String -> String -> Str -> Substantive
-- mkSubstantive apa apan apor aporna apu f = case f of
--   SF n s c -> mkStr $ mkCasus c $ case (n,s) of
--     (Sg,Indef) -> apa
--     (Sg,Def)   -> apan
--     (Pl,Indef) -> apor
--     (Pl,Def)   -> aporna
--   SubstComp    -> apu  -- used in compounds only

-- -- To capture generalizations, we often use help functions orthogonal to
-- -- the declension system: one such help function is the formation of genitive,
-- -- which adds an "s" unless the word already end in "s".

-- mkCasus :: Casus -> String -> String
-- mkCasus c s = case c of
--   Nom -> s
--   Gen -> s +? "s"

-- -- The compound form ("gatu" etc) was added to morphology after it was
-- -- completed for non-compound words. The following function is
-- -- used in those cases where a special compound exists; otherwise the
-- -- compound is defined as non-existing.

-- substCompForm :: Substantive -> String -> Substantive
-- substCompForm s v = s `variants` [(SubstComp,v)]

-- -- The first declension has the general case and some special cases.
-- -- The general case is by far the most common.

-- decl1 :: String -> Substantive
-- decl1 apa = mkSubstantive apa (apa ++ "n") apor (apor ++ "na") ap
--   where
--     apor = tk 1 apa ++ "or"
--     ap = mkStr $ ifEndThen (== 'a') apa (tk 1 apa) apa

-- decl1ros :: String -> Substantive
-- decl1ros ros = mkSubstantive ros (ros ++ "en") rosor (rosor ++ "na") (mkStr ros)
--   where
--     rosor = ros ++ "or"

-- decl1gata :: String -> Substantive
-- decl1gata gata = decl1 gata `substCompForm` (tk 1 gata ++ "u")

-- decl1flicka :: String -> Substantive
-- decl1flicka flicka = decl1 flicka `substCompForm` (tk 1 flicka ++ "e")

-- decl1lära :: String -> Substantive
-- decl1lära lära = decl1 lära `substCompForm` (tk 1 lära ++ "o")


-- -- The second declension has more variation than the first, but much of it is
-- -- determined by the end of the word.
-- --
-- -- So words like "pojke" ending with "e" are captured by the main rule.

-- decl2 :: String -> Substantive
-- decl2 pojke = mkSubstantive pojke pojken (pojk ++ "ar") (pojk ++ "arna") ifPojk
--   where
--     pojk = dropEndIfE pojke
--     pojken = pojke ++ ifVowel lastpojke "n" "en"  -- bro-n, bil-en
--     ifPojk = if (lastpojke == 'e') then (mkStr pojk) else nonExist
--     lastpojke = last pojke

-- -- A special rule is needed for words ending with unstressed "el", "en", "er",
-- -- since we cannot tell from a written word if the "e" is unstressed: e.g.
-- -- in "hushållsel" it isn't.

-- decl2nyckel :: String -> Substantive
-- decl2nyckel nyckel =
--   mkSubstantive nyckel (nyckel ++"n") (nyckl ++"ar") (nyckl ++"arna") nonExist
--     where
--       nyckl = dropSecondLast nyckel

-- -- The duplication of final "m" is a common phenomenon.

-- decl2kam :: String -> Substantive
-- decl2kam kam =
--   mkSubstantive kam (kamm ++"en") (kamm ++"ar") (kamm ++"arna") nonExist
--     where
--       kamm = doubleLast kam

-- -- The words "mor" and "far" are both special.

-- decl2mor :: String -> Substantive
-- decl2mor mor =
--   mkSubstantive mor (moder ++"n") (moedr ++"ar") (moedr ++"arna") nonExist
--   `variants` [(SF Sg Indef v, mkCasus v moder) | v <- values]
--     where
--       moder = tk 1 mor ++ "der"
--       moedr = umlaut $ dropSecondLast moder

-- decl2far :: String -> Substantive
-- decl2far far = decl2mor far `except`
--                             [(SF Pl d c, mkPl d c) | d <- values, c <- values]
--   where
--     mkPl d c = mkCasus c $ (umlaut $ tk 1 far) ++ "der" ++ case d of
--       Indef -> ""
--       _     -> "na"

-- -- The third declension has both utrum and neutrum words, but the gender of nouns is
-- -- not a matter of the inflection engine, since it is inherent.
-- -- The singular definite form becomes of course different.

-- decl3 :: String -> Substantive
-- decl3 sak = mkSubstantive sak (sak ++ "en") (sak ++ "er") (sak ++ "erna") nonExist

-- decl3parti :: String -> Substantive
-- decl3parti parti =
--   mkSubstantive parti (parti ++ "et") (parti ++ "er") (parti ++ "erna") nonExist

-- decl3energi :: String -> Substantive
-- decl3energi energi =
--   mkSubstantive energi (energi ++ "n") (energi ++ "er") (energi ++ "erna") nonExist

-- decl3museum :: String -> Substantive
-- decl3museum museum =
--   mkSubstantive museum (muse ++ "et") (muse ++ "er") (muse ++ "erna")
--                 (mkStr (muse ++ "i"))
--     where
--       muse = tk 2 museum

-- -- The fourth declension has mostly words ending with "e", but we can handle
-- -- other vowels with the same rule ("bi", "biet", "bin", "bina").

-- decl4 :: String -> Substantive
-- decl4 rike = mkSubstantive rike (rike +? "et") (rike ++ "n") (rike ++ "na") nonExist

-- -- The fifth declension, like the third, has both utrum and neutrum words.

-- decl5 :: String -> Substantive
-- decl5 lik = mkSubstantive lik (lik ++ "et") lik (lik ++ "en") nonExist

-- decl5papper :: String -> Substantive
-- decl5papper papper =
--   mkSubstantive papper (pappr ++ "et") papper (pappr ++ "en") nonExist
--     where
--       pappr = dropSecondLast papper -- OBS: decl5 "paket"

-- decl5kikare :: String -> Substantive
-- decl5kikare kikare =
--   mkSubstantive kikare (kikare ++ "n") kikare (kikar ++ "na") (mkStr kikar)
--     where
--       kikar = tk 1 kikare

-- decl5program :: String -> Substantive
-- decl5program program =
--   mkSubstantive program (programm ++"et") program (programm ++"en") nonExist
--     where
--       programm = doubleLast program


-- --2 Adjectives
-- --
-- -- We start with the positive forms, and again with the
-- -- worst-case macro.

-- mkAdjPos :: String -> String -> String -> String -> AdjFormPos -> String
-- mkAdjPos liten litet lilla sma a = case a of
--   Strong gn -> case gn of
--     ASg g -> giveValues [liten, litet] g --- perhaps too sensitive to order
--     APl -> sma
--   Weak sn -> case sn of
--     AxSg g -> giveValues [lilla, lill ++ "e"] g
--     AxPl -> sma
--  where
--    lill = tk 1 lilla

-- -- The worst-case macro for the full declension (including comparison forms)
-- -- is not so much worse.

-- mkAdjective :: String -> String -> String -> String ->
--                          String -> String -> String -> Adjective
-- mkAdjective liten litet lilla sma mindre minst minsta (AF a c) =
--   mkStr $ mkCasus c $ case a of
--     Pos p   -> mkAdjPos liten litet lilla sma p
--     Comp    -> mindre
--     Super s -> giveValues [minst, minsta] s

-- -- It is handy to extract the positive part of a declension only, if
-- -- the other comparicon forms aren't needed or don't make sense.

-- extractPositive ::  Adjective -> AdjFormPos -> Str
-- extractPositive adj a = adj (AF (Pos a) Nom)

-- -- The notion of 'moderately irregular adjective' covers almost all adjectives.

-- adjIrreg god gott battre bast =
--   mkAdjective god gott (god ++ "a") (god ++ "a") battre bast (bast ++ "a")

-- -- Often it is possible to derive the $Pos Sg Neutr$ form even if the
-- -- comparison forms are irregular.

-- adjIrreg3 ung yngre yngst = adjIrreg ung (ung ++ "t") yngre yngst

-- -- Some adjectives must be given $Pos Sg Utr$ $Pos Sg Neutr$, and $Pos Pl$,
-- -- e.g. those ending with unstressed "en".

-- adjAlmostReg ljummen ljummet ljumma =
--   mkAdjective ljummen ljummet ljumma ljumma
--                          (ljumma ++ "re") (ljumma ++ "st") (ljumma ++ "ste")

-- -- Some adjectives that end with "t" don't get it doubled in the $Neutr$ form.
-- -- But since e.g. "våt" does, we make this a separate rule (perhaps it depends
-- -- on whether the second-last letter is a vowel, though).

-- adjAbstrakt abstrakt =
--   adjAlmostReg abstrakt (abstrak ++ "t") (abstrakt ++ "a")
--     where
--       abstrak = tk 1 abstrakt

-- -- Those ending with "en" are much like past participles of strong verbs.
-- -- It is not clear whether the comparison forms always make sense, but
-- -- we generate them anyway.

-- adjVaken vaken = adjAlmostReg vaken (vak ++ "et") (vak ++ "na")
--   where
--     vak = tk 2 vaken

-- -- Some adjectives are like regular past participles. The question is, again,
-- -- if the comparison forms make sense.

-- adjKorkad korkad =
--   adjAlmostReg korkad (korka ++ "t") (korkad ++ "e") `only` positiveForms
--     where
--       korka =tk 1 korkad

-- -- Past participle forms can in fact be inferred from the ending of the
-- -- $Sg Utr$ form:

-- inferPretPart :: String -> AdjFormPos -> String
-- inferPretPart bunden = mkAdjPos bunden bundet bundna bundna where
--   (bundet,bundna) = case reverse bunden of
--     'd':'a':_ -> (bunde ++ "t",  bunden ++ "e")
--     'n':'e':_ -> (bund  ++ "et", redMMNN bund ++ "na") -- fun-na, kom-na
--     'd':'d':_ -> (bund  ++ "tt", bunden ++ "a")
--     't':'t':_ -> (bunden,        bunden ++ "a")
--     'd':_     -> (bunde ++ "t",  bunden ++ "a")
--     _         -> (bunden,        bunden ++ "a")
--   bunde = tk 1 bunden
--   bund  = tk 2 bunden

-- -- Here are some more almost regular patterns.
-- -- Typically, either the "a"-form or the "t"-form is special.
-- -- N.B. with $adjFager$, that we cannot tell from the written word if
-- -- the last "e" is stressed ("gálen" vs. "orèn").

-- adjFager fager = adjAlmostReg fager (fager ++ "t") (dropSecondLast fager ++ "a")
-- adjGrund grund = adjAlmostReg grund (tk 1 grund ++ "t")  (grund ++ "a")
-- adjVid vid     = adjAlmostReg vid   (tk 1 vid  ++  "tt") (vid   ++ "a")
-- adjBodd bodd   = adjAlmostReg bodd  (tk 2 bodd ++  "tt") (bodd  ++ "a")
-- adjBytt bytt   = adjAlmostReg bytt  bytt                 (bytt  ++ "a")

-- -- Completely regular adjectives are a large class.

-- adjReg rik = adjAlmostReg rik (rik ++ "t") (rik ++ "a")

-- --2 Verbs
-- --
-- -- The worst-case macro is bad indeed.

-- mkVerb finna finner finne finn fann funne funnit funnen = mkStr . mkV where
--   mkV v = case v of
--     VF (Pres Ind  Act)  -> finner
--     VF (Pres Conj Act)  -> finne
--     VF (Pres Ind Pass)  -> finn +? "s"  -- finns/finnes by exception
--     VF (Pres Conj Pass) -> finne ++ "s"
--     VF (Pret Ind  Act)  -> fann
--     VF (Pret Conj Act)  -> funne
--     VF (Pret m    Pass) -> mkV (VF (Pret m Act)) +? "s" --- frös - frös ?
--     VF Imper            -> redMM finn
--     VI (Inf Act)        -> finna
--     VI (Inf Pass)       -> finna ++ "s"
--     VI (Sup Act)        -> funnit
--     VI (Sup Pass)       -> funnit ++ "s"
--     VI (PtPres c)       -> mkCasus c $ finna ++ ifEndThen (=='a') finna "nde" "ende"
--     VI (PtPret a c)     -> mkCasus c $ inferPretPart funnen a

-- -- A common exception for second and fourth conjugation is to have
-- -- two present indicative passive forms.

-- presIndPassE leka lek = [(VF (Pres Ind Pass), strings (leks ++ lekes))] where
--   lekes = [tk 1 leka ++ "es"] -- for glömmes (*glömes)
--   leks  = if (dp 1 lek == "s") then [] else [lek ++ "s"]

-- -- Variation in the regular conjugations can mostly be controlled by
-- -- analysing the endings. This is quite involved in the second
-- -- conjugation: think about the differences between
-- -- "leka, vända, byta, tyda, läsa, gömma, känna, hyra, tåla".

-- verbWeak1 tala =
--   mkVerb tala (tala ++ "r") (tk 1 tala ++ "e") --- conj. pres. tale?
--          tala (tala ++ "de") (tala ++ "de") (tala ++ "t")
--          (tala ++ "d")

-- verbWeak2 leka =
--   mkVerb leka  leker leke
--          lek   lekde lekde lekt
--          lekd
--          `excepts` presIndPassE leka lek
--  where
--    stam  = tk 1 leka
--    leke  = stam ++ "e"
--    lek   = redMM stam
--    lekd  = let lek = redMMNN stam in case reverse lek of
--              t:v:b | elem t "dt" && not (isVowel v) -> lek   -- sände
--              t:_   | isVoiced t -> lek ++ "d"
--              _                  -> lek ++ "t"
--    lekt  = let (dd,el) = span (=='d') (reverse lekd) in
--              reverse el ++ replicate (length dd) 't'
--    lekde = lekd ++ "e"
--    leker = presVerbEr stam

-- verbWeak3 bo =
--   mkVerb bo (bo ++ "r") bo
--          bo (bo ++ "dde") (bo ++ "tt")
--          bo (bo ++ "dd")

-- -- Strong and irregular verbs mostly have "er" in the present
-- -- indicative, but other forms exist ("stjäl")
-- -- The past participle may differ from the supine (e.g. "gått" - "gången").

-- verbIrreg finna finner fann funnit funnen =
--   mkVerb finna finner finne
--          finn fann (funn ++ "e")
--          funnit funnen
--          `excepts` ifA (presIndPassE finna finn) []
--  where
--    ifA a b = if dp 1 finna == "a" then a else b
--    finn   = ifA (tk 1 finna) finna
--    finne  = ifA (finn ++ "e") finn
--    funn   = tk 2 funnit
--    funna  = redMM funn +? "na"

-- -- *Strong verbs*: often have stem vowel alternation, but regular supine/participle.

-- verbStrong finna fann funnit =
--   verbIrreg finna (presVerbEr (tk 1 finna)) fann funnit (tk 2 funnit ++ "en")

-- -- So here is a macro for all vowel alternation verbs, followed by
-- -- the possible vowel alternation patterns.

-- verbStrongVowel a u finna = verbStrong finna fann funnit where
--   (f,_,nn) = findStemVowel (tk 1 finna)
--   fann     = redMM $ f ++ a ++ nn
--   funnit   = f ++ u ++ nn ++ "it"

-- verbStrongGiva  = verbStrongVowel "a" "i"
-- verbStrongFinna = verbStrongVowel "a" "u"  -- bära
-- verbStrongFalla = verbStrongVowel "ö" "a"
-- verbStrongSmita = verbStrongVowel "e" "i"
-- verbStrongSupa  = verbStrongVowel "ö" "u"  -- flyga
-- verbStrongKomma = verbStrongVowel "o" "o"  -- sova
-- verbStrongFara  = verbStrongVowel "o" "a"
-- verbStrongLaata = verbStrongVowel "ä" "å"
-- verbStrongAeta  = verbStrongVowel "å" "ä"

-- -- Some verbs have free variation between long and short forms:
-- -- "ge" - "giva", "bli" - "bliva".

-- verbShortLong :: String -> Verb -> Verb
-- verbShortLong ge giva = giva `variants` [
--   (VF (Pres Ind Act),  ge ++ "r"),
--   (VF (Pres Ind Pass), ge ++ "s"),
--   (VF Imper,           ge),
--   (VI (Inf Act),       ge),         --- generalization?
--   (VI (Inf Pass),      ge ++ "s")
--   ] -- N.B. short Sup form does not apply to bli

-- -- Here are some irregular semi-productive verb patterns
-- -- (the productivity is by prefixes such as "be", "för",...).

-- vGiva s v =
--   s +* verbShortLong "ge" (verbStrongGiva "giva") v

-- vBliva s v =
--   s +* verbShortLong "bli" (verbStrongSmita "bliva") v

-- vDraga s v =
--   s +* verbShortLong "dra" (verbStrongFara "draga") v

-- vSe s v =
--   s +* mkVerb "se" "ser" "se" "se" "såg" "såg" "sett" "sedd" v

-- vGaa s v =
--   s +* mkVerb "gå" "går" "gå" "gå" "gick" "ginge" "gått" "gången" v

-- vGoera s v =
--   s +* mkVerb "göra" "gör" "göre" "gör" "gjorde" "gjorde" "gjort" "gjord" v

-- vStaa s v =
--   s +* mkVerb "stå" "står" "stå" "stå" "stod" "stånde" "stått" "stånden" v

-- vFaa s v =
--   s +* (mkVerb "få" "får" "få" "få" "fick" "finge" "fått" []
--   `missing` partPretForms) v

-- vHava s v = s +*
--    (verbShortLong "ha" (mkVerb "hava" "haver" "have" "hav" "hade" "hade" "haft" [])
--    `missing` partPretForms) v

-- vVara s v =
--   s +* (mkVerb "vara" "är" "vare" "var" "var" "vore" "varit" []
--   `missing` passiveForms) v


-- --2 Adverbs

-- -- Adverbs may have comparison forms but are not otherwise inflected.
-- -- There is a productive way to form adverbs from adjectives (e.g.
-- -- "fin" > "fint").

-- mkAdverb :: String -> String -> String -> Adverb
-- mkAdverb bra battre bast = mkStr1 $ giveValues [bra, battre, bast]

-- advAdj :: Adjective -> Adverb
-- advAdj adj =
--   giveValues
--     [adj (AF p Nom) | p <- [Pos (Strong (ASg Neutr)), Comp, Super SupStrong]]

-- adverbReg :: String -> Adverb
-- adverbReg = advAdj . adjReg . tk 1


-- --2 Closed classes
-- --
-- -- For closed classes it is really not practically necessary to have an
-- -- inflection engine: we could just list all the forms. But, of course, it is
-- -- interesting to capture some generalizations


-- --3 Pronouns
-- --
-- -- The following for classes of pronouns are enough to define
-- -- almost all pronouns by giving a few forms.

-- mkPronJag :: String -> String -> String -> String -> PronPN
-- mkPronJag jag mig min mitt c = mkStr $ case c of
--   PNom -> jag
--   PAcc -> mig
--   PGen (ASg Utr) -> min
--   PGen (ASg Neutr) -> mitt
--   PGen APl -> min ++ "a"

-- pronRefl = mkPronJag "" "sig" "sin" "sitt" `missing` [PNom]

-- mkPronHan :: String -> String -> String -> PronPN
-- mkPronHan han honom hans c = mkStr $ case c of
--   PNom -> han
--   PAcc -> honom
--   PGen _ -> hans

-- mkPronNagon :: String -> String -> String -> PronAdj
-- mkPronNagon nagon nagot nagra (AP gn c) = mkStr $ mkCasus c $
--   giveValues [nagon,nagot,nagra] gn

-- mkPronDylik :: String -> PronAdj
-- mkPronDylik dylik = mkPronNagon dylik (dylik ++ "t") (dylik ++ "a")


-- --3 Articles
-- --
-- -- It's almost ridiculous to have a type of articles.
-- -- Since the indefinite article has no plural, perhaps we should
-- -- have had two types!

-- artIndef :: Article
-- artIndef (ArticleForm gn) = case gn of
--   ASg Utr   -> mkStr "en"
--   ASg Neutr -> mkStr "ett"
--   APl       -> nonExist

-- artDef :: Article
-- artDef (ArticleForm gn) = case gn of
--   ASg Utr   -> mkStr "den"
--   ASg Neutr -> mkStr "det"
--   APl       -> strings ["de","dom"]


-- --3 Auxiliary verbs
-- --
-- -- Auxiliary verbs have reduced conjugations. Variant forms are typical, e.g.
-- -- "ska" - "skall".

-- auxVerbGen :: Str -> Str -> Str -> Str -> AuxVerb
-- auxVerbGen vilja vill ville velat = giveValues [vilja, vill, ville, velat]

-- auxVerb :: String -> String -> String -> String -> AuxVerb
-- auxVerb vilja vill ville velat = giveValues $ map mkStr [vilja, vill, ville, velat]


-- --2 Generics for Swedish
-- --
-- -- Here are some *sound laws* and other generic things that we have found useful
-- -- for Swedish.
-- --
-- -- This is a typical operation for unstressed ultimate syllables.

-- dropSecondLast :: String -> String
-- dropSecondLast s = tk 2 s ++ dp 1 s

-- -- What is a vowel or a voiced consonant is language-dependent...

-- isVowel c = elem c "aeiouyåäö"

-- isVoiced c = elem c "bdglmnrv"

-- ifVowel c d e = if isVowel c then d else e

-- -- Final "e" is often dropped.
-- dropEndIfE = dropEndIf (=='e')

-- -- The last letter is ioften doubled.
-- doubleLast s = s ++ dp 1 s

-- -- It is typical to reduce stem-final double-"m" to single, if no vowel follows:
-- -- "glömma" - "glöm" - "glömde". (Alternatively, we could say that a single-"m" is
-- -- doubled in front of a vowel.)

-- redMM s = case reverse s of
--   'm':'m':_ -> init s
--   _ -> s

-- -- Sometimes a double-"n" is reduced as well ("känna" - "kände" ; but "känn").

-- redMMNN s = case reverse s of
--   n:m:_ | n == m && elem n "nm" -> init s
--   _ -> s

-- -- The verb ending "er" is dropped if the verb has a final "l" or "r".

-- presVerbEr far = if (elem (dp 1 far) ["l","r"])
--                     then far
--                     else far ++ "er"

-- --3 Umlaut
-- --
-- -- Let's conclude with something that is not easy to do on this level of generality
-- -- with regular expressions:
-- -- define first the *stem vowel* as the last vowel (or diphtong) in the stem:

-- findStemVowel :: String -> (String, String, String)
-- findStemVowel sprick = (reverse rps, reverse i, reverse kc) where
--   (kc, irps) = break isVowel $ reverse sprick
--   (i,   rps) = span  isVowel $ irps

-- -- Although *umlaut* is not very very useful in Swedish, we are glad to
-- -- present a general rule for it:

-- umlaut :: String -> String
-- umlaut man = m ++ mkUm a ++ n where
--   (m,a,n) = findStemVowel man
--   mkUm v = case v of
--     "a" -> "ä"
--     "o" -> "ö"
--     "å" -> "ä"
--     _   -> v

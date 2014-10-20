module DictMt where

import BuildMt
import RulesMt
import General
import Dictionary

malteseDict :: Dictionary
malteseDict = dictionary [
  vStrong "kiteb" ()
  ]

-- ===========================================================================

-- swedishDict :: Dictionary
-- swedishDict = dictionary $ closedLexicon ++ hardVerbs ++
--  [
--   substUtrum (decl2mor "mor"),
--   --substUtrum (decl2mor "mormor"),
--   substUtrum (decl2far "far"),
--   --substUtrum (decl2far "farfar"),
--   --substUtrum (decl2far "morfar"),
--   substUtrum (decl2far "bror"),
--   --substUtrum (decl2far "farbror"),
--   --substUtrum (decl2far "morbror"),
--   sNyckel "syster",
--   sNyckel "faster",
--   sNyckel "moster",
--   aReg "amerikansk",
--   aReg "apatisk",
--   aAbstrakt "arrogant",
--   adjective (mkAdjective
--      "gammal" "gammalt" "gamla" "gamla" "äldre" "äldst" "äldsta"),
--   v1 "hävda",
--   s3 "imperialism",
--   s3 "imperialist",
--   aReg "jämn",
--   s3 "kapitalist",
--   aReg "klok",
--   aReg "möjlig",
--   substUtrum (mkSubstantive "son" "sonen" "söner" "sönerna" nonExist),
--   adjective (adjIrreg3 "stor" "större" "störst"),
--   v2 "tycka",
--   aReg "tänkbar",
--   aUdda "udda",
--   adjective (adjIrreg3 "ung" "större" "störst"),
--   aReg "vertikal",
--   verb (mkVerb "veta" "vet" "vete" "vet" "visste" "visste" "vetat" "visst") ,
-- -- models for the most common cases

--   vStrong "stjäla" "stal" "stulit",

--   pnMasc "Oden",
--   pnUtr "Sigyn",
--   pnNeutr "Sverige",

--   advReg  "fint",
--   adverbBra "bra" "bättre" "bäst",
--   adverbBra "gärna" "hellre" "helst",
--   adverb (advAdj (adjFager "vacker")),
--   adverbInv "möjligen",
--   adverbInv "kanske",
--   adverbInv "aldrig",
--   adverbInv "alltid",

-- -- interrogatives
--   interrogInv "när",
--   interrogInv "hur",
--   interrogInv "vad",
--   interrogInv "varför",
--   interrogInv "vart",
--   interrogInv "var",
--   interrogInv "varifrån",

--   pronInv "man",

-- -- ambiguities
--   s2 "val",
--   s5 "val",
--   s2 "vals",
--   s3 "vals",

-- -- adjectives

-- -- the most irregular words

--   substMasc (sIrreg "man" "mannen" "män" "männen" nonExist),
--   substUtrum (sIrreg "gås" "gåsen" "gäss" "gässen" nonExist),
--   adjective (aIrreg "liten" "litet" "lilla" "små" "mindre" "minst" "minsta"),
--   verb (noPartPret (vIrreg "få" "får" "få" "få" "fick" "finge" "fått" [])),
--   verb (noPassive (vIrreg "vara" "är" "vare" "var" "var" "vore" "varit" []))
--   ]

-- hardVerbs :: [Entry]
-- hardVerbs =  concat [
--   verbs ["","an","av","be","in","om","upp","ut","åter","över"]
--     (shortLong "ge" (verbStrongGiva "giva")),
--   verbs ["","för","ute","över"]
--     (noPassive (shortLong "bli" (verbStrongSmita "bliva"))),
--   verbs ["","an","av","be","för","in","om","upp","ut","åter","över"]
--      (vIrreg "se" "ser" "se" "se" "såg" "såg" "sett" "sedd"),
--   verbs ["","an","av","be","från","för","genom","in","kring","upp","ut","över"]
--       (vIrreg "gå" "går" "gå" "gå" "gick" "ginge" "gått" "gången"),
--   verbs ["","an","av","be","för","genom","in","upp","ut","åter"]
--       (vIrreg "stå" "står" "stå" "stå" "stod" "stånde" "stått" "stånden"),
--   verbs ["","inne","åter"]
--       (noPartPret (shortLong "ha"
--           (vIrreg "hava" "haver" "have" "hav" "hade" "hade" "haft" [])))
--   ]


-- closedLexicon :: [Entry]
-- closedLexicon = concat
-- 		[
--   		 prepositions,
--                  adverbials,
--    		 conjunctions,
--    		 subjunctions,
--    		 particles,
-- 		 pronounsPN,
-- 		 pronounsAdj,
-- 		 [
-- 		  entry artIndef,
-- 		  entry artDef,
-- 		  infMark "att"
-- 		 ],
--                  auxiliaryVerbs,
-- 		 numerals
-- 		]

-- pronounsAdj =
--     [
--      pronDylik "all",
--      pronDylik "dylik",
--      pronDylik "hurdan",
--      pronDylik "likadan",
--      pronDylik "sådan",
--      pronNagon "annan" "annat" "andra",
--      pronNagon "ingen" "inget" "inga",
--      pronNagon "vilken" "vilket" "vilka",
--      pronNagon "mången" "månget" "många",
--      pronNagon "någon" "något" "några"
--     ]


-- prepositions
--      = map prep
-- 	[
--        "alltsedan",
--        "apropå",
--        "av",
--        "bakom",
--        "bland",
--        "bortom",
--        "bortåt",
--        "brevid",
--        "efter",
--        "emellan",
--        "emot",
--        "enligt",
--        "framför",
--        "från",
--        "för",
--        "förbi",
--        "före",
--        "förutom",
--        "genom",
--        "gentemot",
--        "hos",
--        "härom",
--        "i",
--        "ifrån",
--        "igenom",
--        "inemot",
--        "inför",
--        "inklusive",
--        "innan",
--        "innanför",
--        "inom",
--        "intill",
--        "invid",
--        "jämte",
--        "kontra",
--        "kring",
--        "längs",
--        "med",
--        "medelst",
--        "mellan",
--        "mot",
--        "nedanför",
--        "om",
--        "omkring",
--        "ovan",
--        "ovanför",
--        "ovanpå",
--        "per",
--        "på",
--        "runt",
--        "sedan",
--        "sen",
--        "till",
--        "trots",
--        "undan",
--        "under",
--        "ur",
--        "utan",
--        "utanför",
--        "utanpå",
--        "utefter",
--        "utför",
--        "utifrån",
--        "utmed",
--        "utom",
--        "utöver",
--        "via",
--        "vid",
--        "visavi",
--        "å",
--        "åt",
--        "över"
--        ]

-- conjunctions = map conj $ words
--   "och samt respektive liksom fast men utan eller ty för så"

-- subjunctions = map subj $ words
-- 	       "att förrän innan medan sedan tills eftersom allteftersom emedan för så bara blott ifall om såvida liksom som såsom än ehuru fast fastän oaktat"

-- particles = map part $ words
--    "emot fast in ut upp ner fram åter" -- plus prepositions

-- auxiliaryVerbs = map (\ (x, y, z, u) -> entry (auxVerbGen x y z u)) [
--   (mkStr "vilja", mkStr "vill", mkStr "ville",mkStr "velat"),
--   (mkStr "kunna", mkStr "kan",  mkStr "kunde",mkStr "kunnat"),
--   (nonExist,      mkStr "måste",mkStr "måste",mkStr "måst"),
--   (mkStr "skola", strings ["ska","skall"], mkStr "skulle",nonExist),
--   (mkStr "böra",  mkStr "bör", mkStr "borde",mkStr "bort")
--   ]

-- -- -- not really closed
-- adverbials = map adverbInv $ words
--   "dessutom heller också även dock ändå alltså väl nere ute inne"

-- -- from numerals.Swe.gf
-- numerals = map numeralInv $ words "arton aderton elva ett fem femtio femton fjorton fyra fyrtio hundra nio nittio nitton sex sextio sexton sju sjuttio sjutton tio tjugo tolv tre trettio tretton tusen två åtta åttio"

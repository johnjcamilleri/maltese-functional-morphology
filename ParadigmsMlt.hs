{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module ParadigmsMlt where

import ResMlt
import MorphoMlt

------------------------------------------------------------------------------
-- Paradigms

-- mkV1 :: Str -> Verb
-- mkV1 = loanV

mkV2 :: Str -> Root -> Verb = \mamma root ->
  let
    class_ :: VClass = classifyRoot root
    vseq_ :: Vowels = extractVowels mamma
  in
  case class_ of
    Strong Regular      -> strongV2 root vseq_
    -- Strong LiquidMedial -> liquidMedialV root vseq
    -- Strong Geminated    -> geminatedV root vseq
    -- Weak Assimilative   -> assimilativeV root vseq
    -- Weak Hollow         -> hollowV root vseq
    -- Weak Lacking        -> lackingV root vseq
    -- Weak Defective      -> defectiveV root vseq
    -- Quad QStrong        -> quadV root vseq
    -- Quad QWeak          -> quadWeakV root vseq
    -- Irregular           -> error ("Cannot use smart paradigm for irregular verb:"++mamma)
    -- Loan                -> loanV mamma --- this should probably be an error


mkV3 :: Str -> Str -> Root -> Verb
mkV3 = undefined

------------------------------------------------------------------------------
-- Deeper

strongV2 :: Root -> Vowels -> Verb = \root vseq ->
  let imp = conjStrongImp root vseq
  in strongVWorst root vseq imp

strongV3 :: Root -> Vowels -> Str -> Verb = \root vseq imp_sg ->
  let
    imp = \n -> case n of
      Sg -> imp_sg
      Pl -> (takePfx 3 imp_sg) ++ c3 root ++ "u" -- IFTAĦ > IFTĦU
  in strongVWorst root vseq imp

strongVWorst :: Root -> Vowels -> (Number ==> Str) -> Verb = \root vseq imp ->
  let
    tbl :: (VForm ==> Str) = \vf -> case vf of {
      VPerf agr -> ( conjStrongPerf root vseq ) ! agr ;
      VImpf agr -> ( conjStrongImpf (imp ! Sg) (imp ! Pl) ) ! agr ;
      VImp n    -> imp ! n
    } ;
    info :: VerbInfo = mkVerbInfo (Strong Regular) (FormI) root vseq (imp ! Sg) ;
  in Verb {
    s = verbPolarityTable info (verbPronSuffixTable info tbl) ,
    i = info
    -- hasPresPart = False ;
    -- hasPastPart = False ;
    -- presPart = \\_ => nonExist ;
    -- pastPart = \\_ => nonExist ;
  } ;

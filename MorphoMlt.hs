{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module MorphoMlt where

import ResMlt

verbPolarityTable = undefined

verbPronSuffixTable :: VerbInfo -> (VForm ==> Str) -> (VForm ==> VSuffixForm ==> Str) = \info tbl ->
  \vform -> case vform of {
      VPerf agr -> verbPerfPronSuffixTable info ( \a -> tbl ! VPerf a ) ! agr ;
      VImpf agr -> verbImpfPronSuffixTable info ( \a -> tbl ! VImpf a ) ! agr ;
      VImp  num -> verbImpPronSuffixTable  info ( \n -> tbl ! VImp  n ) ! num
  } ;

verbPerfPronSuffixTable :: VerbInfo -> (VAgr ==> Str) -> (VAgr ==> VSuffixForm ==> Str) = \info tbl ->
  let
    -- TODO: rename?
    patt2 = vowelChangesIE (v_root info) (v_vseq info) ;
  in
  \agr -> case agr of {
    AgP1 Sg -> -- Jiena FTAĦT
      let
        ftaht = tbl ! AgP1 Sg ;
      in
      \vform -> case vform of {
        VSuffixNone -> ftaht ;
        VSuffixDir agr ->
          case agr of {
            AgP1 Sg    -> nonExist ;
            AgP2 Sg    -> ftaht ++ "ek" ;  -- Jiena FTAĦTEK
            AgP3Sg Masc-> ftaht ++ "u" ;  -- Jiena FTAĦTU
            AgP3Sg Fem -> ftaht ++ "ha" ;  -- Jiena FTAĦTHA
            AgP1 Pl    -> nonExist ;
            AgP2 Pl    -> ftaht ++ "kom" ;  -- Jiena FTAĦTKOM
            AgP3Pl     -> ftaht ++ "hom"  -- Jiena FTAĦTHOM
          } ;
        VSuffixInd agr ->
          case agr of {
            AgP1 Sg    -> nonExist ;
            AgP2 Sg    -> ftaht ++ "lek" ;  -- Jiena FTAĦTLEK
            AgP3Sg Masc-> ftaht ++ "lu" ;  -- Jiena FTAĦTLU
            AgP3Sg Fem -> ftaht ++ "ilha" ;  -- Jiena FTAĦTILHA
            AgP1 Pl    -> nonExist ;
            AgP2 Pl    -> ftaht ++ "ilkom" ;  -- Jiena FTAĦTILKOM
            AgP3Pl     -> ftaht ++ "ilhom"  -- Jiena FTAĦTILHOM
          } ;
        VSuffixDirInd dobj agr -> (verbDirIndSuffixTable (AgP1 Sg) dobj ftaht) ! agr
      } ;
    AgP2 Sg -> -- Inti FTAĦT
      let
        ftaht = tbl ! AgP2 Sg ;
      in
      \vform -> case vform of {
        VSuffixNone -> ftaht ;
        VSuffixDir agr ->
          case agr of {
            AgP1 Sg    -> ftaht ++ "ni" ; -- Inti FTAĦTNI
            AgP2 Sg    -> nonExist ;
            AgP3Sg Masc-> ftaht ++ "u" ;  -- Inti FTAĦTU
            AgP3Sg Fem -> ftaht ++ "ha" ;  -- Inti FTAĦTHA
            AgP1 Pl    -> ftaht ++ "na" ; -- Inti FTAĦTNA
            AgP2 Pl    -> nonExist ;
            AgP3Pl     -> ftaht ++ "hom"  -- Inti FTAĦTHOM
          } ;
        VSuffixInd agr ->
          case agr of {
            AgP1 Sg    -> ftaht ++ "li" ; -- Inti FTAĦTLI
            AgP2 Sg    -> nonExist ;
            AgP3Sg Masc-> ftaht ++ "lu" ;  -- Inti FTAĦTLU
            AgP3Sg Fem -> ftaht ++ "ilha" ;  -- Inti FTAĦTILHA
            AgP1 Pl    -> ftaht ++ "ilna" ; -- Inti FTAĦTILNA
            AgP2 Pl    -> nonExist ;
            AgP3Pl     -> ftaht ++ "ilhom"  -- Inti FTAĦTILHOM
          } ;
        VSuffixDirInd dobj agr -> (verbDirIndSuffixTable (AgP2 Sg) dobj ftaht) ! agr
      } ;
    -- AgP3Sg Masc -> -- Huwa FETAĦ
    --   let
    --     mamma = tbl ! AgP3Sg Masc ;
    --     fetah : Str = case <info, mamma> of {
    --       <_, x + "'"> -> x ++ "għ" ; -- QATA' > QATAGĦ
    --       <{imp = _ + "a"}, _> ->  mamma ; -- KANTA > KANTA (i.e. Italian -are)
    --       <_, serv + "a"> -> serv ++ "ie" ; -- SERVA > SERVIE (i.e. Italian -ere/-ire)
    --       <{form = FormIII}, w@#Consonant + "ie" + geb> -> w + (patt2.V1) + info.root.C2 + "i" + info.root.C3 ; -- WIEĠEB > WIĠIB
    --       <_, x + y@#Consonant + "e" + z@#Consonant> -> x + y + "i" + z ; -- KITEB > KITIB
    --       _ -> mamma -- FETAĦ
    --       } ;
    --     -- fetah : Str = case (tbl ! AgP3Sg Masc) of {
    --     --   x + "'" -> x + "għ" ; -- QATA' > QATAGĦ
    --     --   x + "a" -> x + "ie" ; -- SERVA > SERVIE
    --     --   x + "e" + y@#Consonant -> x + "i" + y ; -- KITEB > KITIB
    --     --   x -> x -- FETAĦ
    --     --   } ;
    --     feth : Str = case <info.form, info.class> of {
    --       <FormII, Strong Geminated> -> info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C2 ; -- BEXX
    --       <FormII, Weak Hollow> -> info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C3 ; -- QAJM
    --       <FormII, Weak Lacking> -> info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C2 ; -- NEĦĦ
    --       <FormII, Quad QStrong> -> pfx_T info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C3 + info.root.C4 ; -- TĦARBT
    --       <FormII, Quad QWeak> -> pfx_T info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C3 ; -- SSERV
    --       <FormII, _> -> info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C2 + info.root.C3 ; -- ĦABB
    --       <FormIX, _> -> mamma ; -- info.root.C1 + info.root.C2 + info.vseq.V1 + info.root.C3 ; -- ĦDAR
    --       <FormX, _> -> case info.imp of {
    --         staghg + e@#Vwl + b@#Cns -> staghg + b ; -- STAGĦĠB, STĦARRĠ
    --         _ -> info.imp -- STQARR
    --         } ;
    --       -- <FormX, _> -> "st" + info.vseq.V1 + info.root.C1 + info.root.C2 + info.root.C3 ; -- STAGĦĠB
    --       <_, Weak Hollow> -> info.root.C1 + info.vseq.V1 + info.root.C3 ; -- SAB
    --       <_, Weak Lacking> -> info.root.C1 + info.vseq.V1 + info.root.C2 ; -- MEX
    --       <_, Quad QStrong> -> info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C3 + info.root.C4 ;
    --       <_, Loan> -> dropSfx 1 mamma ; -- ŻVILUPP
    --       _ -> info.root.C1 + info.vseq.V1 + info.root.C2 + info.root.C3
    --       } ;
    --     p2sg_dir_ek : Str = case <info.imp, mamma> of {
    --       <_ + "a", _> -> "ak" ; -- Huwa KANTAK
    --       <_, _ + "a"> -> "iek" ; -- Huwa SERVIEK
    --       _ -> "ek" -- Huwa FETĦEK
    --       } ;
    --     p3sg_dir_u : Str = case <info.imp, mamma> of {
    --       <_ + "a", _> -> "ah" ; -- Huwa KANTAH
    --       <_, _ + "a"> -> "ieh" ; -- Huwa SERVIEH
    --       _ -> "u" -- Huwa FETĦU
    --       } ;
    --   in
    --   \vform -> case vform of {
    --     VSuffixNone -> tbl ! AgP3Sg Masc ;
    --     VSuffixDir agr ->
    --       case agr of {
    --         AgP1 Sg    -> sfx fetah "ni" ; -- Huwa FETAĦNI (n.b. KENN+NI)
    --         AgP2 Sg    -> feth + p2sg_dir_ek ;
    --         AgP3Sg Masc-> feth + p3sg_dir_u ;
    --         AgP3Sg Fem -> fetah + "ha" ;  -- Huwa FETAĦHA
    --         AgP1 Pl    -> sfx fetah "na" ; -- Huwa FETAĦNA (n.b. KENN+NA)
    --         AgP2 Pl    -> sfx fetah "kom" ; -- Huwa FETAĦKOM (n.b. ĦAKK+KOM)
    --         AgP3Pl     -> fetah + "hom"  -- Huwa FETAĦHOM
    --       } ;
    --     VSuffixInd agr ->
    --       let
    --         fethi : Str = case info.imp of {
    --           _ + "a'" -> feth + "a" ; -- QATTA' > QATTGĦALNA --- very specific
    --           _ + "a" -> feth + "a" ; -- KANTA-
    --           _ + "i" -> feth + "ie" ; -- SERVIE-
    --           _ -> (ie2_ patt2.V1 feth) + "i"
    --           } ;
    --       in
    --       case agr of {
    --         AgP1 Sg    -> sfx fetah "li" ; -- Huwa FETAĦLI (n.b. ĦALL+LI)
    --         AgP2 Sg    -> sfx fetah "lek" ; -- Huwa FETAĦLEK (n.b. ĦALL+LEK)
    --         AgP3Sg Masc-> sfx fetah "lu" ;  -- Huwa FETAĦLU (n.b. ĦALL+LU)
    --         AgP3Sg Fem -> fethi + "lha" ;  -- Huwa FETĦILHA
    --         AgP1 Pl    -> fethi + "lna" ; -- Huwa FETĦILNA
    --         AgP2 Pl    -> fethi + "lkom" ; -- Huwa FETĦILKOM
    --         AgP3Pl     -> fethi + "lhom"  -- Huwa FETĦILHOM
    --       } ;
    --     VSuffixDirInd dobj agr -> case info.imp of {
    --       _ + "i" -> (verbDirIndSuffixTable (AgP3Sg Masc) dobj (feth+"i")) ! agr ; -- SERVI-
    --       _ -> (verbDirIndSuffixTable (AgP3Sg Masc) dobj (ie2i fetah)) ! agr
    --       }
    --   } ;
    -- AgP3Sg Fem -> -- Hija FETĦET
    --   let
    --     fethet = tbl ! AgP3Sg Fem ;
    --     fethit : Str = case fethet of {
    --       q@#Cns+ "ie" + #Cns + #Cns + _ -> q+(patt2.V1)+info.root.C2+info.root.C3+"it" ; -- WIEĠBET > WIĠBIT
    --       _ + "għet" -> (dropSfx 2 fethet) + "at" ; -- QATTGĦET > QATTGĦATNI...  --- very specific
    --       _ + "iet" -> fethet ; -- SERVIET
    --       feth + "et" -> (ie2i feth) + "it" ;
    --       _ -> fethet -- QRAT, ŻVILUPPAT...
    --       } ;
    --   in
    --   \vform -> case vform of {
    --     VSuffixNone -> tbl ! AgP3Sg Fem ;
    --     VSuffixDir agr ->
    --       case agr of {
    --         AgP1 Sg    -> fethit + "ni" ; -- Hija FETĦITNI
    --         AgP2 Sg    -> fethit + "ek" ; -- Hija FETĦITEK
    --         AgP3Sg Masc-> fethit + "u" ;  -- Hija FETĦITU
    --         AgP3Sg Fem -> fethit + "ha" ;  -- Hija FETĦITHA
    --         AgP1 Pl    -> fethit + "na" ; -- Hija FETĦITNA
    --         AgP2 Pl    -> fethit + "kom" ; -- Hija FETĦITKOM
    --         AgP3Pl     -> fethit + "hom"  -- Hija FETĦITHOM
    --       } ;
    --     VSuffixInd agr ->
    --       case agr of {
    --         AgP1 Sg    -> fethit + "li" ; -- Hija FETĦITLI
    --         AgP2 Sg    -> fethit + "lek" ; -- Hija FETĦITLEK
    --         AgP3Sg Masc-> fethit + "lu" ;  -- Hija FETĦITLU
    --         AgP3Sg Fem -> (ie2i fethit) + "ilha" ;  -- Hija FETĦITILHA
    --         AgP1 Pl    -> (ie2i fethit) + "ilna" ; -- Hija FETĦITILNA
    --         AgP2 Pl    -> (ie2i fethit) + "ilkom" ; -- Hija FETĦITILKOM
    --         AgP3Pl     -> (ie2i fethit) + "ilhom"  -- Hija FETĦITILHOM
    --       } ;
    --     VSuffixDirInd dobj agr -> (verbDirIndSuffixTable (AgP3Sg Fem) dobj (ie2i fethit)) ! agr
    --   } ;
    AgP1 Pl -> -- Aħna FTAĦNA
      let
        ftahna = tbl ! AgP1 Pl ;
        ftahn = dropSfx 1 ftahna ;
      in
      \vform -> case vform of {
        VSuffixNone -> ftahna ;
        VSuffixDir agr ->
          case agr of {
            AgP1 Sg    -> nonExist ;
            AgP2 Sg    -> ftahn ++ "iek" ;  -- Aħna FTAĦNIEK
            AgP3Sg Masc-> ftahn ++ "ieh" ;  -- Aħna FTAĦNIEH
            AgP3Sg Fem -> ftahn ++ "ieha" ;  -- Aħna FTAĦNIEHA
            AgP1 Pl    -> nonExist ;
            AgP2 Pl    -> ftahn ++ "iekom" ;  -- Aħna FTAĦNIEKOM
            AgP3Pl     -> ftahn ++ "iehom"  -- Aħna FTAĦNIEHOM
          } ;
        VSuffixInd agr ->
          case agr of {
            AgP1 Sg    -> nonExist ;
            AgP2 Sg    -> ftahn ++ "ielek" ;  -- Aħna FTAĦNIELEK
            AgP3Sg Masc-> ftahn ++ "ielu" ;  -- Aħna FTAĦNIELU
            AgP3Sg Fem -> ftahn ++ "ielha" ;  -- Aħna FTAĦNIELHA
            AgP1 Pl    -> nonExist ;
            AgP2 Pl    -> ftahn ++ "ielkom" ;  -- Aħna FTAĦNIELKOM
            AgP3Pl     -> ftahn ++ "ielhom"  -- Aħna FTAĦNIELHOM
          } ;
        VSuffixDirInd (GSg Masc) agr -> (verbDirIndSuffixTable (AgP1 Pl) (GSg Masc) (ftahn+"i")) ! agr ;
        VSuffixDirInd (GSg Fem) agr -> (verbDirIndSuffixTable (AgP1 Pl) (GSg Fem) (ftahn+"i")) ! agr ;
        VSuffixDirInd (GPl) agr -> (verbDirIndSuffixTable (AgP1 Pl) (GPl) (ftahn+"i")) ! agr
      } ;
    AgP2 Pl -> -- Intom FTAĦTU
      let
        ftahtu = tbl ! AgP2 Pl ;
      in
      \vform -> case vform of {
        VSuffixNone -> ftahtu ;
        VSuffixDir agr ->
          case agr of {
            AgP1 Sg    -> ftahtu ++ "ni" ; -- Intom FTAĦTUNI
            AgP2 Sg    -> nonExist ;
            AgP3Sg Masc-> ftahtu ++ "h" ;  -- Intom FTAĦTUH
            AgP3Sg Fem -> ftahtu ++ "ha" ;  -- Intom FTAĦTUHA
            AgP1 Pl    -> ftahtu ++ "na" ; -- Intom FTAĦTUNA
            AgP2 Pl    -> nonExist ;
            AgP3Pl     -> ftahtu ++ "hom"  -- Intom FTAĦTUHOM
          } ;
        VSuffixInd agr ->
          case agr of {
            AgP1 Sg    -> ftahtu ++ "li" ; -- Intom FTAĦTULI
            AgP2 Sg    -> nonExist ;
            AgP3Sg Masc-> ftahtu ++ "lu" ;  -- Intom FTAĦTULU
            AgP3Sg Fem -> ftahtu ++ "lha" ;  -- Intom FTAĦTULHA
            AgP1 Pl    -> ftahtu ++ "lna" ; -- Intom FTAĦTULNA
            AgP2 Pl    -> nonExist ;
            AgP3Pl     -> ftahtu ++ "lhom"  -- Intom FTAĦTULHOM
          } ;
        VSuffixDirInd dobj agr -> (verbDirIndSuffixTable (AgP2 Pl) dobj ftahtu) ! agr
      } ;
    AgP3Pl -> -- Huma FETĦU
      let
        fethu = ie2_ (v1 patt2) (tbl ! AgP3Pl) ;
      in
      \vform -> case vform of {
        VSuffixNone -> tbl ! AgP3Pl ;
        VSuffixDir agr ->
          case agr of {
            AgP1 Sg    -> fethu ++ "ni" ; -- Huma FETĦUNI
            AgP2 Sg    -> fethu ++ "k" ; -- Huma FETĦUK
            AgP3Sg Masc-> fethu ++ "h" ;  -- Huma FETĦUH
            AgP3Sg Fem -> fethu ++ "ha" ;  -- Huma FETĦUHA
            AgP1 Pl    -> fethu ++ "na" ; -- Huma FETĦUNA
            AgP2 Pl    -> fethu ++ "kom" ; -- Huma FETĦUKOM
            AgP3Pl     -> fethu ++ "hom"  -- Huma FETĦUHOM
          } ;
        VSuffixInd agr ->
          case agr of {
            AgP1 Sg    -> fethu ++ "li" ; -- Huma FETĦULI
            AgP2 Sg    -> fethu ++ "lek" ; -- Huma FETĦULEK
            AgP3Sg Masc-> fethu ++ "lu" ;  -- Huma FETĦULU
            AgP3Sg Fem -> fethu ++ "lha" ;  -- Huma FETĦULHA
            AgP1 Pl    -> fethu ++ "lna" ; -- Huma FETĦULNA
            AgP2 Pl    -> fethu ++ "lkom" ; -- Huma FETĦULKOM
            AgP3Pl     -> fethu ++ "lhom"  -- Huma FETĦULHOM
          } ;
        VSuffixDirInd dobj agr -> (verbDirIndSuffixTable (AgP3Pl) dobj fethu) ! agr
      }

    } ; -- end of verbPerfPronSuffixTable

verbImpfPronSuffixTable :: VerbInfo -> (VAgr ==> Str) -> (VAgr ==> VSuffixForm ==> Str) = \info tbl -> undefined

verbImpPronSuffixTable :: VerbInfo -> (Number ==> Str) -> (Number ==> VSuffixForm ==> Str) = \info tbl -> undefined

verbDirIndSuffixTable :: VAgr -> GenNum -> Str -> (VAgr ==> Str) = \subj dobj ftaht ->
  case dobj of {
    GSg Masc -> \agr -> case agr of {
            AgP1 Sg    -> case subj of {
              AgP1 _     -> nonExist ;
              _          -> ftaht ++ "huli"
              } ;
            AgP2 Sg    -> case subj of {
              AgP2 _     -> nonExist ;
              _          -> ftaht ++ "hulek"
              } ;
            AgP3Sg Masc-> ftaht ++ "hulu" ;
            AgP3Sg Fem -> ftaht ++ "hulha" ;
            AgP1 Pl    -> case subj of {
              AgP1 _     -> nonExist ;
              _          -> ftaht ++ "hulna"
              } ;
            AgP2 Pl    -> case subj of {
              AgP2 _     -> nonExist ;
              _          -> ftaht ++ "hulkom"
              } ;
            AgP3Pl     -> ftaht ++ "hulhom"  -- Jiena FTAĦTHULHOM
          } ;
    GSg Fem -> \agr -> case agr of {
            AgP1 Sg    -> case subj of {
              AgP1 _     -> nonExist ;
              _          -> ftaht ++ "hieli"
              } ;
            AgP2 Sg    -> case subj of {
              AgP2 _     -> nonExist ;
              _          -> ftaht ++ "hielek"
              } ;
            AgP3Sg Masc-> ftaht ++ "hielu" ;  -- Jiena FTAĦTHIELU
            AgP3Sg Fem -> ftaht ++ "hielha" ;  -- Jiena FTAĦTHIELHA
            AgP1 Pl    -> case subj of {
              AgP1 _     -> nonExist ;
              _          -> ftaht ++ "hielna"
              } ;
            AgP2 Pl    -> case subj of {
              AgP2 _     -> nonExist ;
              _          -> ftaht ++ "hielkom"
              } ;
            AgP3Pl     -> ftaht ++ "hielhom"  -- Jiena FTAĦTHIELHOM
          } ;
    GPl -> \agr -> case agr of {
            AgP1 Sg    -> case subj of {
              AgP1 _     -> nonExist ;
              _          -> ftaht ++ "homli"
              } ;
            AgP2 Sg    -> case subj of {
              AgP2 _     -> nonExist ;
              _          -> ftaht ++ "homlok"
              } ;
            AgP3Sg Masc-> ftaht ++ "homlu" ;  -- Jiena FTAĦTHOMLU
            AgP3Sg Fem -> ftaht ++ "homlha" ;  -- Jiena FTAĦTOMHLA
            AgP1 Pl    -> case subj of {
              AgP1 _     -> nonExist ;
              _          -> ftaht ++ "homlna"
              } ;
            AgP2 Pl    -> case subj of {
              AgP2 _     -> nonExist ;
              _          -> ftaht ++ "homlkom"
              } ;
            AgP3Pl     -> ftaht ++ "homlhom"  -- Jiena FTAĦTHOMLHOM
          }
  } ;

conjGenericImpf :: Str -> Str -> (VAgr ==> Str) = \imp_sg imp_pl ->
  \agr -> case agr of {
    AgP1 Sg    -> pfx_N imp_sg ;  -- Jiena NIŻLOQ
    AgP2 Sg    -> pfx_T imp_sg ;  -- Inti TIŻLOQ
    AgP3Sg Masc-> pfx_J imp_sg ;  -- Huwa JIŻLOQ
    AgP3Sg Fem -> pfx_T imp_sg ;  -- Hija TIŻLOQ
    AgP1 Pl    -> pfx_N imp_pl ;  -- Aħna NIŻOLQU
    AgP2 Pl    -> pfx_T imp_pl ;  -- Intom TIŻOLQU
    AgP3Pl    -> pfx_J imp_pl    -- Huma JIŻOLQU
  } ;

-- IE/I vowel changes
-- so far only used in derived verbs
-- see {GO pg93}
vowelChangesIE :: Root -> Vowels -> Vowels
vowelChangesIE root vowels
  | (c2 root=="għ") && (v1 vowels=="ie") && (v2 vowels=="e") = mkVowels2 "e" "i" -- QIEGĦED > QEGĦIDKOM
  | (v1 vowels=="ie") && (v2 vowels=="e") = mkVowels2 "i" "i" -- WIEĠEB > WIĠIBKOM
  | (v1 vowels=="ie") && (v2 vowels=="") = mkVowels1 "i" -- STRIEĦ > STRIĦAJT
  | otherwise = vowels

conjStrongPerf :: Root -> Vowels -> (VAgr ==> Str) = \root p ->
  let
    ktib = c1 root ++ c2 root ++ (case v2 p of {"e" -> "i" ; _ -> v2 p }) ++ c3 root ;
    kitb = c1 root ++ v1 p ++ c2 root ++ c3 root ;
  in
    \agr -> case agr of {
      AgP1 Sg    -> ktib ++ "t" ;  -- Jiena KTIBT
      AgP2 Sg    -> ktib ++ "t" ;  -- Inti KTIBT
      AgP3Sg Masc-> c1 root ++ v1 p ++ c2 root ++ v2 p ++ c3 root ;  -- Huwa KITEB
      AgP3Sg Fem -> kitb ++ (case v2 p of {"o" -> "o" ; _ -> "e"}) ++ "t" ;  -- Hija KITBET
      AgP1 Pl    -> ktib ++ "na" ;  -- Aħna KTIBNA
      AgP2 Pl    -> ktib ++ "tu" ;  -- Intom KTIBTU
      AgP3Pl    -> kitb ++ "u"  -- Huma KITBU
    } ;

conjStrongImpf = conjGenericImpf

conjStrongImp :: Root -> Vowels -> (Number ==> Str) = \root vseq ->
  let
    vwls = vowelChangesStrong vseq ;
  in
    \n -> case n of {
      Sg -> v1 (vwls!Sg) ++ c1 root ++ c2 root ++ v2 (vwls!Sg) ++ c3 root ;  -- Inti:  IKTEB
      Pl -> v1 (vwls!Pl) ++ c1 root ++ c2 root ++ c3 root ++ "u"  -- Intom: IKTBU
    } ;

-- Vowel changes for imperative
vowelChangesStrong :: Vowels -> (Number ==> Vowels) = \vseq ->
  \n -> case n of {
    Sg -> case (v1 vseq,v2 vseq) of {
      ("a","a") -> mkVowels2 "o" "o" ; -- RABAT > ORBOT (but: ILGĦAB, AĦBAT)
      ("a","e") -> mkVowels2 "a" "e" ; -- GĦAMEL > AGĦMEL
      ("e","e") -> mkVowels2 "i" "e" ; -- FEHEM > IFHEM
      ("e","a") -> mkVowels2 "i" "a" ; -- FETAĦ > IFTAĦ (but: ONFOĦ)
      ("i","e") -> mkVowels2 "i" "e" ; -- KITEB > IKTEB
      ("o","o") -> mkVowels2 "o" "o"   -- GĦOĠOB > OGĦĠOB
    };
    Pl -> case (v1 vseq,v2 vseq) of {
      ("a","a") -> mkVowels1 "o" ; -- RABAT > ORBTU
      ("a","e") -> mkVowels1 "a" ; -- GĦAMEL > AGĦMLU
      ("e","e") -> mkVowels1 "i" ; -- FEHEM > IFHMU
      ("e","a") -> mkVowels1 "i" ; -- FETAĦ > IFTĦU
      ("i","e") -> mkVowels1 "i" ; -- KITEB > IKTBU
      ("o","o") -> mkVowels1 "o"   -- GĦOĠOB > OGĦĠBU
    }
  } ;

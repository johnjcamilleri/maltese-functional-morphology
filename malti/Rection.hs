module Main where

-- run this on a result of morphological analysis to find rection
-- patterns for verbs and adjectives. AR 26/10/2004

-- main :: IO ()
-- main = do
--   s <- getContents
--   mapM_ (putStrLn . prRection) $ collect $ input $ lines s


-- collect :: [Lemma] -> [Rection]
-- collect ls = case ls of
--   Verb v : Pron _ : Att : Verb _ : ws -> VTV v : collect ws  -- befaller dig att gå
--   Verb v : Pron _ : Att          : ws -> VTS v : collect ws  -- berättar dig att
--   Verb v : Pron _ : Prep p : ws -> V3 v p : collect ws    -- ställer dig på
--   Verb v : Pron p       : ws -> VT v   : collect ws       -- älskar dig
--   Verb v : Prep p       : ws -> V2 v p : collect ws       -- tittar på dig
--   Verb v : Att : Verb w : ws -> VV v   : collect ws       -- tänker att gå  --- missing w's rection
--   Verb v : Att          : ws -> VS v   : collect ws       -- säger att
--   Adj  a : Prep p       : ws -> A2 a p : collect ws       -- skyldig till
--   w                     : ws ->          collect ws
--   _                          -> []

-- data Lemma =
--    Verb String
--  | Att           -- conjunction or infinitive mark
--  | Adj String
--  | Prep String
--  | Pron String   -- accusative form of pronoun
--  | Other
--   deriving Show

-- data Rection =
--    VT String         -- transitive verb
--  | V2 String String  -- 2-place verb with preposition
--  | V3 String String  -- 3-place verb with direct object + preposition
--  | VS String         -- sentence-complement verb ("att" + S)
--  | VV String         -- verb-complement verb ("att" + infinitive)
--  | VTS String        -- acc + sentence-complement verb ("att" + S)
--  | VTV String        -- acc + verb-complement verb ("att" + infinitive)
--  | A2 String String  -- 2-place adjective with preposition
--   deriving Show

-- input :: [String] -> [Lemma]
-- input [] = []
-- input (l:ls) = case words l of
--   _:v:_:"Verb":_        -> Verb v : continue ls
--   _:p:_:"Preposition":_ -> Prep p : continue ls
--   _:a:_:"Adjektiv":_    -> Adj  a : continue ls
--   _:p:_:"Pronomen":_:"PAcc":_ -> Pron p : continue ls
--   _:"att":_             -> Att    : continue ls
--   _                     -> input ls
--  where
--    continue (k:ks) = case words k of
--      _:_:_:_             -> continue ks  -- ignore other analyses
--      "]":_               -> continue ks
--      []                  -> input ks     -- new stanza begins
--      _                   -> input ks
--    continue _ = []


-- -- print lemma first for nicer sorting
-- prRection r = case r of
--    VT s   -> s ++ ": VT " ++ s
--    V2 s p -> s ++ ": V2 " ++ s ++ " " ++ p
--    V3 s p -> s ++ ": V3 " ++ s ++ " " ++ p
--    A2 s p -> s ++ ": A2 " ++ s ++ " " ++ p
--    VS s   -> s ++ ": VS " ++ s
--    VV s   -> s ++ ": VV " ++ s
--    VTS s  -> s ++ ": VTS " ++ s
--    VTV s  -> s ++ ": VTV " ++ s

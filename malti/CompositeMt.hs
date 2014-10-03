module CompositeMt where

import General
import BuildMt
import AttrMt

decomposeMt :: [Attr] -> Bool
decomposeMt [x] = (x /= atP)
decomposeMt xs = decomp xs
  where
   decomp []    = False
   decomp [x]   = elem x [atW, atWP]
   decomp (x:xs) = (atP == x || atWP == x) && decomp xs

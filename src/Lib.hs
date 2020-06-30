module Lib
    ( extendedEuclid'
    , extendedEuclid
    , EuclidRes (..)
    ) where

data EuclidRes = EuclidRes { gcd::Integer
                           , bezoutS::Integer
                           , bezoutT::Integer}
              deriving (Show, Eq)


-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
extendedEuclid :: Integer -> Integer -> EuclidRes
extendedEuclid a b = EuclidRes r'  s'  t
  where (r', s') = loop b a 0 1
        t =  (r' - s' * a) `div` b
        loop 0 r' _ s' = ( r', s')
        loop r r' s s' = loop rm r sm s
          where (rq, rm) = divMod r' r
                sm = s' - rq*s


-- recursive code from german wikipedia
-- see https://de.wikipedia.org/wiki/Erweiterter_euklidischer_Algorithmus
extendedEuclid' :: Integer -> Integer -> EuclidRes
extendedEuclid' a 0 = EuclidRes a 1 0
extendedEuclid' a b = rr
  where
    (di,mo) = divMod a b
    EuclidRes d' s' t' = extendedEuclid' b mo
    rr = EuclidRes d' t' (s' - di*t')


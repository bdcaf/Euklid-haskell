module Lib
    ( extendedEuclid'
    , extendedEuclid
    , extendedEuclidMany
    , toMany
    , EuclidRes (..)
    , EuclidResMany (..)
    ) where

data EuclidRes = EuclidRes { gcd::Integer
                           , bezoutS::Integer
                           , bezoutT::Integer}
              deriving (Show, Eq)

data EuclidResMany = EuclidResMany { gcds::Integer
                                   , bezout::[Integer]
                                   }
              deriving (Show, Eq)
            
toMany :: EuclidRes -> EuclidResMany
toMany (EuclidRes d s t) = EuclidResMany d [s,t]

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
extendedEuclid :: Integer -> Integer -> EuclidRes
extendedEuclid a b = if a<= 0 || b<=0 
                        then error "Both numbers need to be larger than 0."
                        else EuclidRes r'  s'  t
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
extendedEuclid' a b = if a<= 0 || b<=0 
                        then error "Both numbers need to be larger than 0."
                        else EuclidRes d' t' (s' - di*t')
  where
    (di,mo) = divMod a b
    EuclidRes d' s' t' = extendedEuclid' b mo


extendedEuclidMany :: [Integer] -> Maybe EuclidResMany
extendedEuclidMany [] = Nothing
extendedEuclidMany [i] = Just $ EuclidResMany i [1]
extendedEuclidMany (a:as) = fmap (growEuclid a) esub
  where esub = extendedEuclidMany as
        growEuclid d2 (EuclidResMany d ss) = EuclidResMany r (s:s2)
          where
            EuclidRes r s t = extendedEuclid d2 d
            s2 = map (* t) ss


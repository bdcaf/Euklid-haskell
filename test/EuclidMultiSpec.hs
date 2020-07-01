--
-- EuclidSpec.hs
-- Copyright (C) 2020 Clemens Ager <clemens.ager@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module EuclidMultiSpec  ( spec )
where

import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Trivial" $ do
    it "Nothing on empty." $
       extendedEuclidMany [] `shouldBe` Nothing
  describe "Examples" $ do
    it "explanation example 99 78" $
       extendedEuclidMany [99,78] `shouldBe` Just ( toMany (EuclidRes 3 (-11) 14))
    it "explanation example 240 46" $
       extendedEuclidMany [240,46] `shouldBe` Just ( toMany (EuclidRes 2 (-9) 47))
  describe "properties" $ do
      prop "multi algorithm needs to produce same result as the one on two." $ prop_same_as_recursive
      prop "Example on lists" $ prop_same_as_recursive


prop_examples :: [Positive Integer] -> Bool
prop_examples   as = case em of
                       Nothing -> length as == 0 
                       Just (EuclidResMany d bs) -> lengthtest as bs 
                         && beztest a1 bs d 
                         && divtest a1 d
  where em = extendedEuclidMany a1
        a1 = map getPositive as
        lengthtest as bs =  length bs == length as
        beztest a0 b0 d0 = (sum $ zipWith (*) a0 b0) == d0
        divtest a0 d0 = all (== 0) . map (`mod` d0) $ a0

prop_same_as_recursive (Positive a) (Positive b) =  extendedEuclidMany [a,b] == Just ( toMany (extendedEuclid a b))
                             


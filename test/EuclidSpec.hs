--
-- EuclidSpec.hs
-- Copyright (C) 2020 Clemens Ager <clemens.ager@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module EuclidSpec  ( spec )
where

import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Trivial" $ do
    it "trivial example 99 1" $
       extendedEuclid 99 1 `shouldBe` (EuclidRes 1 0 1)
    it "trivial example 99 99" $
       extendedEuclid 99 99 `shouldBe` (EuclidRes 99 0 1)
  describe "Examples" $ do
    it "explanation example 99 78" $
       extendedEuclid 99 78 `shouldBe` (EuclidRes 3 (-11) 14)
    it "explanation example flipped 78 99" $
       extendedEuclid 78 99  `shouldBe` (EuclidRes 3 14 (-11) )
    it "explanation example 99 78" $
       extendedEuclid 240 46 `shouldBe` (EuclidRes 2 (-9) 47)
  describe "properties" $ do
      prop "both numbers divisible a%gcd == 0, b%gcd == 0" $ 
            prop_divisible
      prop "bezout a*s+b*t = gcd" $ 
            prop_bezout
      prop "recursive and iterative algorithm have same result" $ prop_same_as_recursive

prop_divisible (Positive a) (Positive b) =  a `mod` d ==0 && b `mod`d == 0
  where EuclidRes d s t = extendedEuclid a b
                             
prop_bezout (Positive a) (Positive b) =  a*s + b*t == d
  where EuclidRes d s t = extendedEuclid a b

prop_same_as_recursive (Positive a) (Positive b) =  extendedEuclid a b == extendedEuclid' a b
                             

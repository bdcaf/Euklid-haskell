--
-- EuclidSpec.hs
-- Copyright (C) 2020 Clemens Ager <clemens.ager@gmail.com>
--
-- Distributed under terms of the MIT license.
--

module EuclidSpec  ( spec )
where

import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Lib

spec :: Spec
spec = do
  describe "Trivial" $ do
    it "trivial example 99 1" $
       let trivial = extendedEuclid 99 1
       in  trivial `shouldBe` (EuclidRes 1 (0) 1)
    it "trivial example 99 99" $
       let trivial = extendedEuclid 99 99
       in  trivial `shouldBe` (EuclidRes 99 (0) 1)
  describe "Examples" $ do
    it "explanation example 99 78" $
       let wikiExample = extendedEuclid 99 78
       in  wikiExample `shouldBe` (EuclidRes 3 (-11) 14)
    it "explanation example flipped 78 99" $
       let wikiExample = extendedEuclid 78 99
       in  wikiExample `shouldBe` (EuclidRes 3 14 (-11) )
    it "explanation example 99 78" $
       let wikiExample = extendedEuclid 240 46
       in  wikiExample `shouldBe` (EuclidRes 2 (-9) 47)
  describe "properties" $ do
      it "both numbers divisible a%gcd == 0, b%gcd ==0" $ property $
            prop_divisible
      it "bezout a*s+b*t = gcd" $ property $
            prop_bezout
      it "recursive and iterative algorithm have same result" $ property $
            prop_same_as_recursive

prop_divisible a b = a>0 && b>0 ==> a `mod` d ==0 && b `mod`d == 0
  where EuclidRes d s t = extendedEuclid a b
                             
prop_bezout a b = a>0 && b>0 ==> a*s + b*t == d
  where EuclidRes d s t = extendedEuclid a b

prop_same_as_recursive a b = a>0 && b>0 ==> extendedEuclid a b == extendedEuclid' a b
                             

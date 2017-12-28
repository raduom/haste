module Main where

import Test.Hspec

import K.Match

listSignature :: Signature
listSignature = Signature [("[]", 0), ("_:_", 2)]

pNil :: Pattern
pNil = Constructor listSignature "[]" []

pCons :: Pattern
pCons = Constructor listSignature "_:_" [WildCard, WildCard]

oNil :: Occurence
oNil = Occurence listSignature "[]" []

oCons :: Occurence
oCons = Occurence listSignature "_:_" [oNil, oNil]

example_01 :: ([Occurence] -> Int)
example_01 = compilePatternMatch [ ([pNil, WildCard], 1)
                                 , ([WildCard, pNil], 2)
                                 , ([pCons, pCons],   3) ]

main :: IO ()
main = hspec $ do
  describe "Initial paper example" $ do
    it "returns the 1st pattern when ran against [], []" $ do
      example_01 [oNil, oNil] `shouldBe` 1

    it "returns the 2nd pattern when ran against []:[] []" $ do
      example_01 [oCons, oNil] `shouldBe` 2

    it "returns the 3rd pattern when ran against []:[] []:[]" $ do
      example_01 [oCons, oCons] `shouldBe` 3

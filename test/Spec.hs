{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import Test.Hspec

import Data.HVect
import Data.Constraint
import Data.List

import K.Match

instance TermUnfold Int where
  type TUnfold Int = '[]
  tUnfold _ _ = HNil
  witness _ _ = Dict

instance (AllHave Pattern ps) => TermUnfold (HVect ps) where
  type TUnfold (HVect ps) = ps
  tUnfold _ ps = ps
  witness _ _  = Dict
  

{-
listSignature :: Signature
listSignature = Signature [("[]", 0), ("_:_", 2)]

pNil :: Pattern
pNil = Constructor listSignature "[]" []

pCons :: Pattern
pCons = Constructor listSignature "_:_" [WildCard, WildCard]

oNil :: Occurence
oNil = Occurence listSignature "[]" []

oCons :: Occurence
oCons = Occurence listSignature "_:_" [oNil, oNil] -- []:[]

example_01 :: ([Occurence] -> Int)
example_01 = compilePatternMatch [ ([pNil, WildCard], 1)
                                 , ([WildCard, pNil], 2)
                                 , ([pCons, pCons],   3) ]

main :: IO ()
main = hspec $ do
  describe "Initial paper example" $ do
    it "returns the 1st pattern when ran against [], []" $ do
      example_01 [oNil, oCons] `shouldBe` 1

    it "returns the 2nd pattern when ran against []:[] []" $ do
      example_01 [oCons, oNil] `shouldBe` 2

    it "returns the 3rd pattern when ran against []:[] []:[]" $ do
      example_01 [oCons, oCons] `shouldBe` 3

-}

main :: IO ()
main = print "Hello world"

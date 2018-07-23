{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Data.Functor.Foldable (Fix(..))
import Data.Proxy (Proxy(..))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


import Pattern
import Pattern.Class

data Lst  = Cns Lst -- index 2
          | Nil     -- index 1
          | CnsW    -- index 0
          | Wld     -- wildcard
          deriving (Show, Eq)

instance IsPattern Lst where
  toPattern :: Lst -> Fix Pattern
  toPattern Wld     = Fix Wildcard
  toPattern CnsW    = Fix (Pattern 0 [Fix Wildcard])
  toPattern Nil     = Fix (Pattern 1 [])
  toPattern (Cns l) = Fix (Pattern 2 [toPattern l])

instance HasMetadata Lst where
  getMetadata :: Proxy Lst -> Metadata
  getMetadata _ = Metadata
                  [ Metadata []
                  , Metadata []
                  , Metadata [getMetadata (Proxy :: Proxy Lst)]
                  ]

tests :: TestTree
tests = testGroup "Tests" [appendTests]

appendTests :: TestTree
appendTests = testGroup "Append tests"
  [ testCase "append with one leg up" $
      True @?= True ]

main :: IO ()
main = defaultMain tests

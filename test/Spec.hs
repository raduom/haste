{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Main where

import           Data.Functor.Foldable (Fix (..))
import           Data.List             (transpose)
import           Data.Proxy            (Proxy (..))

import           Data.Either           (fromRight)
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Pattern               hiding (getMetadata)
import           Pattern.Class

data Lst  = Cns Lst -- index 1
          | Nil     -- index 0
          | Wld     -- wildcard
          deriving (Show, Eq)

instance IsPattern Lst where
  toPattern :: Lst -> Fix Pattern
  toPattern Wld     = Fix Wildcard
  toPattern Nil     = Fix (Pattern 0 [])
  toPattern (Cns l) = Fix (Pattern 1 [toPattern l])

instance HasMetadata Lst where
  getMetadata :: Proxy Lst -> Metadata
  getMetadata _ = Metadata
                    [ Metadata [] -- Nil
                    , Metadata [getMetadata (Proxy :: Proxy Lst)] -- Cns Lst (1)
                    ]

mkLstPattern :: [[Lst]] -> ClauseMatrix
mkLstPattern ls =
  let as = take (length ls) [1..]
      md = getMetadata (Proxy :: Proxy Lst)
      cs = fmap (Column md . (toPattern <$>)) (transpose ls)
  in case mkClauseMatrix cs as of
       Right matrix -> matrix
       Left  msg    -> error $ "Invalid definition: " ++ show msg

defaultPattern :: ClauseMatrix
defaultPattern =
  mkLstPattern [ [Nil, Wld]
               , [Wld, Nil]
               , [Wld, Wld] ]


appendPattern :: ClauseMatrix
appendPattern =
  mkLstPattern [ [Nil, Wld]
               , [Wld, Nil]
               , [Cns Wld, Cns Wld] ]

tests :: TestTree
tests = testGroup "Tests" [appendTests]

appendTests :: TestTree
appendTests = testGroup "Basic pattern compilation"
  [ testCase "Naive compilation of the append pattern" $
      compilePattern appendPattern @?= Fix Fail
  ]

main :: IO ()
main = defaultMain tests

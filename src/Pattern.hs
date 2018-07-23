{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Pattern ( LHS
               , PatternMatrix
               , Metadata(..)
               , Index
               , Pattern(..)
               , mkClauseMatrix
               , DecisionTree(..)
               , compilePattern
               ) where

import           Data.Bifunctor        (second)
import           Data.Functor.Foldable (Fix (..), unfix)
import           Data.Maybe            (mapMaybe)
import           Data.Semigroup        ((<>))
import           Data.Text             (Text (..), pack)
import           TextShow              (showt)

data Column a = Column
                { getMetadata :: Metadata
                , getTerms    :: [Pattern a]
                } deriving (Show, Eq, Functor)

newtype Metadata = Metadata [Metadata]
                 deriving (Show, Eq)

type Index       = Int
data Pattern a   = Pattern Index [a]
                 | Wildcard
                 deriving (Show, Eq, Functor)

newtype LHS a = LHS [Column a] deriving Functor

type PatternMatrix = LHS (Fix Pattern)

type Action       = Int
data ClauseMatrix = ClauseMatrix PatternMatrix [Action]

-- [ Builders ]

mkClauseMatrix :: [Column (Fix Pattern)]
               -> [Action]
               -> Either Text ClauseMatrix
mkClauseMatrix cs as = do
  validateColumnLength (length as) cs
  pure (ClauseMatrix (LHS cs) as)
  where
    validateColumnLength :: Int -> [Column (Fix Pattern)] -> Either Text ()
    validateColumnLength as =
      mapM_ (\c ->
                if length (getTerms c) == as
                then Right ()
                else Left $ "Wrong column length. Expected " <> showt as <>
                            " and got " <> showt (length (getTerms c)))

-- [ Matrix ]

sigma :: Column a -> [Index]
sigma = mapMaybe ix . getTerms
  where
    ix :: Pattern a -> Maybe Index
    ix (Pattern ix _) = Just ix
    ix Wildcard       = Nothing

sigma₁ :: PatternMatrix -> [Index]
sigma₁ (LHS (c : _)) = sigma c

mSpecialize :: Index -> ClauseMatrix -> (Index, ClauseMatrix)
mSpecialize ix = (ix, ) . expandMatrix ix . filterByIndex ix

mDefault :: ClauseMatrix -> Maybe ClauseMatrix
mDefault (ClauseMatrix (LHS (c : cs)) as) =
  let (Metadata mtd) = getMetadata c
      s₁ = sigma c
  in  if length s₁ /= length mtd
      then Just (ClauseMatrix (LHS cs) as)
      else Nothing

filterByList :: [Bool] -> [a] -> [a]
filterByList (True  : bs) (x : xs) = x : filterByList bs xs
filterByList (False : bs) (_ : xs) = filterByList bs xs
filterByList _ _                   = []

filterByIndex :: Index -> ClauseMatrix -> ClauseMatrix
filterByIndex ix (ClauseMatrix (LHS (c : cs)) as) =
  let filteredRows = map checkPatternIndex (getTerms c)
      newCs = filterByList filteredRows cs
      newAs = filterByList filteredRows as
  in ClauseMatrix (LHS newCs) newAs
  where
    checkPatternIndex :: Pattern a -> Bool
    checkPatternIndex Wildcard        = True
    checkPatternIndex (Pattern ix' _) = ix == ix'

expandMatrix :: Index -> ClauseMatrix -> ClauseMatrix
expandMatrix ix (ClauseMatrix (LHS (c : cs)) as) =
  ClauseMatrix (LHS (expandColumn ix c <> cs)) as

expandColumn :: Index -> Column (Fix Pattern) -> [Column (Fix Pattern)]
expandColumn ix (Column m ps) =
  let metas    = expandMetadata ix m
      patterns = map (expandPattern metas) ps
  in  zipWith Column metas patterns

expandMetadata :: Index -> Metadata -> [Metadata]
expandMetadata ix (Metadata ms) =
  let (Metadata ms') = ms !! ix
  in  ms'

expandPattern :: [Metadata]
              -> Pattern (Fix Pattern)
              -> [Pattern (Fix Pattern)]
expandPattern _ (Pattern ix fixedPs) = map unfix fixedPs
expandPattern ms Wildcard            = replicate (length ms) Wildcard

--[ Target language ]

data L = L
         { getSpecializations :: [(Index, Fix DecisionTree)]
         , getDefault         :: Maybe (Fix DecisionTree)
         }

data DecisionTree a = Leaf Action
                    | Fail
                    | Switch L
                    | Swap Index (Fix DecisionTree)

compilePattern :: ClauseMatrix -> Fix DecisionTree
compilePattern cm@(ClauseMatrix pm ac) =
  let s₁ = sigma₁ pm
      ls = map (`mSpecialize` cm) s₁
      d  = mDefault cm
  in  Fix $ Switch L
      { getSpecializations = map (second compilePattern) ls
      , getDefault = compilePattern <$> d
      }

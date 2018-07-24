{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Pattern ( PatternMatrix
               , ClauseMatrix
               , Column(..)
               , Metadata(..)
               , Pattern(..)
               , Index
               , mkClauseMatrix
               , DecisionTree(..)
               , compilePattern
               ) where

import           Data.Bifunctor        (second)
import           Data.Functor.Classes  (Eq1 (..), Show1 (..))
import           Data.Functor.Foldable (Fix (..), unfix)
import           Data.Maybe            (mapMaybe)
import           Data.Semigroup        ((<>))
import           Data.Text             (Text (..), pack)
import           TextShow              (showt)

data Column = Column
              { getMetadata :: Metadata
              , getTerms    :: [Fix Pattern]
              } deriving (Show, Eq)

newtype Metadata = Metadata [Metadata]
                 deriving (Show, Eq)

type Index       = Int
data Pattern a   = Pattern Index [a]
                 | Wildcard
                 deriving (Show, Eq, Functor)

newtype PatternMatrix = PatternMatrix [Column]
                        deriving (Show, Eq)

type Action       = Int
data ClauseMatrix = ClauseMatrix PatternMatrix [Action]
                    deriving (Show, Eq)

instance Show1 Pattern where
  liftShowsPrec showT _      d Wildcard = showString "_"
  liftShowsPrec showT showTs d (Pattern ix t) =
    showString "P " . showString (show ix) .
    showString " "  . showTs t

instance Eq1 Pattern where
  liftEq _ Wildcard Wildcard = True
  liftEq eqT (Pattern ix ts) (Pattern ix' ts') =
    ix == ix' && and (zipWith eqT ts ts')
  liftEq _ _ _ = False

-- [ Builders ]

mkClauseMatrix :: [Column]
               -> [Action]
               -> Either Text ClauseMatrix
mkClauseMatrix cs as = do
  validateColumnLength (length as) cs
  pure (ClauseMatrix (PatternMatrix cs) as)
  where
    validateColumnLength :: Int -> [Column] -> Either Text ()
    validateColumnLength as =
      mapM_ (\c ->
                if length (getTerms c) == as
                then Right ()
                else Left $ "Wrong column length. Expected " <> showt as <>
                            " and got " <> showt (length (getTerms c)))

-- [ Matrix ]

sigma :: Column -> [Index]
sigma = mapMaybe ix . getTerms
  where
    ix :: Fix Pattern -> Maybe Index
    ix (Fix (Pattern ix _)) = Just ix
    ix (Fix Wildcard)       = Nothing

sigma₁ :: PatternMatrix -> [Index]
sigma₁ (PatternMatrix (c : _)) = sigma c
sigma₁ _             = []

mSpecialize :: Index -> ClauseMatrix -> (Index, ClauseMatrix)
mSpecialize ix = (ix, ) . expandMatrix ix . filterByIndex ix

mDefault :: ClauseMatrix -> Maybe ClauseMatrix
mDefault (ClauseMatrix (PatternMatrix (c : cs)) as) =
  let (Metadata mtd) = getMetadata c
      s₁ = sigma c
  in  if length s₁ /= length mtd
      then Just (ClauseMatrix (PatternMatrix cs) as)
      else Nothing
mDefault _ = Nothing

filterByList :: [Bool] -> [a] -> [a]
filterByList (True  : bs) (x : xs) = x : filterByList bs xs
filterByList (False : bs) (_ : xs) = filterByList bs xs
filterByList _ _                   = []

filterByIndex :: Index -> ClauseMatrix -> ClauseMatrix
filterByIndex ix (ClauseMatrix (PatternMatrix (c : cs)) as) =
  let filteredRows = map checkPatternIndex (getTerms c)
      newCs = filterByList filteredRows cs
      newAs = filterByList filteredRows as
  in ClauseMatrix (PatternMatrix newCs) newAs
  where
    checkPatternIndex :: Fix Pattern -> Bool
    checkPatternIndex (Fix Wildcard)        = True
    checkPatternIndex (Fix (Pattern ix' _)) = ix == ix'

expandMatrix :: Index -> ClauseMatrix -> ClauseMatrix
expandMatrix ix (ClauseMatrix (PatternMatrix (c : cs)) as) =
  ClauseMatrix (PatternMatrix (expandColumn ix c <> cs)) as
expandMatrix _ _ = error "Cannot expand empty matrix."

expandColumn :: Index -> Column -> [Column]
expandColumn ix (Column m ps) =
  let metas    = expandMetadata ix m
      patterns = map (expandPattern metas) ps
  in  zipWith Column metas patterns

expandMetadata :: Index -> Metadata -> [Metadata]
expandMetadata ix (Metadata ms) =
  let (Metadata ms') = ms !! ix
  in  ms'

expandPattern :: [Metadata]
              -> Fix Pattern
              -> [Fix Pattern]
expandPattern _  (Fix (Pattern ix fixedPs)) = fixedPs
expandPattern ms (Fix Wildcard)             = replicate (length ms) (Fix Wildcard)

--[ Target language ]

data L a = L
           { getSpecializations :: [(Index, a)]
           , getDefault         :: Maybe a
           } deriving (Show, Eq, Functor)

data DecisionTree a = Leaf Action
                    | Fail
                    | Switch (L a)
                    | Swap Index a
                    deriving (Show, Eq, Functor)

instance Eq1 DecisionTree where
  liftEq _ Fail Fail = True
  liftEq _ (Leaf a) (Leaf a') = a == a'
  liftEq eqT (Switch l) (Switch l') =
    liftEq eqT l l'
  liftEq eqT (Swap ix t) (Swap ix' t') =
    ix == ix' && t `eqT` t'
  liftEq _ _ _ = False

smEq :: (a -> b -> Bool)
     -> [(Index, a)]
     -> [(Index, b)]
     -> Bool
smEq eq s₁ s₂ =
  and (zipWith combine s₁ s₂)
  where
    combine (ix, a) (ix', a') = ix == ix' && a `eq` a'

instance Eq1 L where
  liftEq eqT (L s (Just d)) (L s' (Just d')) =
    smEq eqT s s' && d `eqT` d'
  liftEq eqT (L s Nothing) (L s' Nothing) =
    smEq eqT s s'
  liftEq _ _ _ = False

instance Show1 DecisionTree where
  liftShowsPrec _ _ _ (Leaf a) = showString $ "Leaf " ++ show a
  liftShowsPrec _ _ _ Fail     = showString "Fail"
  liftShowsPrec showT showL d (Switch l) =
    showString "Switch " . liftShowsPrec showT showL (d + 1) l
  liftShowsPrec showT _ d (Swap ix tm) =
    showString ("Swap " ++ show ix ++ " ") . showT (d + 1) tm

instance Show1 L where
  liftShowsPrec showT _ d (L sm dm) =
    let showSpec (index, tm) = showString (show index ++ ";")
                               . showT (d + 1) tm
        smString = foldl (.) id $ map showSpec sm
        dmString = maybe id (showT (d + 1)) dm
    in  smString . dmString

compilePattern :: ClauseMatrix -> Fix DecisionTree
compilePattern cm@(ClauseMatrix pm ac)
  | length ac == 0 = Fix Fail
  | isWildcardRow pm = Fix $ Leaf $ head ac
  | otherwise =
    let s₁ = sigma₁ pm
        ls = map (`mSpecialize` cm) s₁
        d  = mDefault cm
    in  Fix $ Switch L
        { getSpecializations = map (second compilePattern) ls
        , getDefault = compilePattern <$> d
        }
  where
    isWildcardRow :: PatternMatrix -> Bool
    isWildcardRow (PatternMatrix (Column _ tms : _)) = and (map (Fix Wildcard ==) tms)
    isWildcardRow _ = False

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-|
Module : Match

Implementation of pattern matching according to Luc Maranget's paper Compiling Pattern Matching to good Decision Trees.
-}
module K.Match where

import           Data.List     ((\\), replicate)
import           Data.Maybe    (fromJust, catMaybes)

type Identifier = String

-- | Returns true if I need to build a default matrix if the provided identifier/arity(s)
--   have been used in the other rows on this column.
newtype Signature = Signature [(Identifier, Int)]
                    deriving (Eq, Show)

-- | A untyped representation of patterns. This is hardly enough, though it
--   should suffice to represent all programs in the paper.
data Pattern = WildCard
             | Constructor Signature Identifier [Pattern]
             deriving (Eq, Show)

-- | This data type represents an input to the pattern matching algorithm
--   TODO: Why does this not matter for evaluation?
data Occurence = Occurence Signature Identifier [Occurence]

type ClauseMatrix a = [([Pattern], a)]

firstRow :: ClauseMatrix a -> ([Pattern], a)
firstRow = head

firstColumn :: ClauseMatrix a -> [Pattern]
firstColumn = map (head . fst)

firstPattern :: ClauseMatrix a -> Pattern
firstPattern = head . firstColumn

mReduce :: (Identifier -> Bool) -> ([Pattern], a) -> Bool
mReduce f (Constructor _ identifier _ : _, _) = f identifier
mReduce _ (WildCard : _, _)                   = True
mReduce _ ([], _)                             = True

mSpecialization :: ClauseMatrix a -> Identifier -> Int -> ClauseMatrix a
mSpecialization cm p ar = map unfold $ filter (mReduce (== p)) cm
  where
    unfold :: ([Pattern], a) -> ([Pattern], a)
    unfold (Constructor _ _ ps : rest, a) = (ps ++ rest, a)
    -- | TODO: This is not correct, actually it is VERY WRONG -- should work but sig might be lost
    unfold (WildCard : rest, a)           = ((replicate ar WildCard) ++ rest, a)
    -- TODO: Should never happen
    unfold ps@([], _)                     = ps

removeFirstColumn :: ([Pattern], a) -> ([Pattern], a)
removeFirstColumn (_ : rest, a) = (rest, a)
-- TODO: Should never happen
removeFirstColumn ([], a)       = ([], a)

mDefault0 :: ClauseMatrix a -> ClauseMatrix a

mDefault0 cm = map removeFirstColumn cm

mDefault :: [Identifier] -> ClauseMatrix a -> ClauseMatrix a
mDefault ids = map removeFirstColumn . filter (mReduce (\i -> elem i ids))



-- | This is just a reminder that we should make better use of the type
--   system at some point to enforce some typing rules.
typeCheckPattern :: [([Pattern], a)] -> Bool
typeCheckPattern = undefined

{- |
This is the A language defined in the paper. The type parameter a represents
the semantic domain we map to.
-}
class DecisionTree a where
  dtLeaf :: Int -> a
  dtFail :: a
  dtSwitch :: SwitchClauseList a -> a
  dtSwap :: Int -> a -> a

-- | This represents the L association list from the paper
data SwitchClauseList a = SwitchClauseList
  { -- | The S matrices built for each constructor in the pattern match
    sclCons    :: ![(Identifier, a)]
    -- | The optional default matrix (if there are constructors that have not been used)
  , sclDefault :: !(Maybe a)
  }

headCtors :: ClauseMatrix a -> [(Signature, Identifier, [Pattern])]
headCtors = (=<<) getCtor . firstColumn
  where
    getCtor :: Pattern -> [(Signature, Identifier, [Pattern])]
    getCtor WildCard                       = []
    getCtor (Constructor sig identifier ps) = [(sig, identifier, ps)]

headIds :: ClauseMatrix a -> [(Identifier, Int)]
headIds = map (\(_, it, ps) -> (it, length ps)) . headCtors

signature :: Pattern -> Maybe Signature
signature WildCard = Nothing
signature (Constructor sig _ _) = Just sig

-- | TODO: Write this using types
headSigs :: ClauseMatrix a -> [Signature]
headSigs = catMaybes . map signature . firstColumn

isWildcard :: Pattern -> Bool
isWildcard WildCard = True
isWildcard _ = False

compilePatternMatch :: (DecisionTree a) => ClauseMatrix Int -> a
-- | The first compilation rule.
compilePatternMatch [] = dtFail
-- | The second compilation rule
compilePatternMatch ( (ps, a) : _ )
  | ps == [] = dtLeaf a
  | all isWildcard ps = dtLeaf a
-- | The third compilation rule
compilePatternMatch cm = dtSwitch $
  SwitchClauseList { sclCons = compileSpecialized cm
                   , sclDefault = compileDefault cm
                   }

compileDefault :: (DecisionTree a) => ClauseMatrix Int -> Maybe a
compileDefault cm =
  let sigs = headSigs cm
      hids = map fst $ headIds cm
  in case (sigs, length hids) of
    ([], 0) -> return $ compilePatternMatch $ mDefault0 cm
    (_, 0) -> Nothing
    (Signature cs : _, _) -> return $ compilePatternMatch $ mDefault (map fst cs \\ hids) cm
    ([], _) -> Nothing -- TODO: Will never happen

compileSpecialized :: (DecisionTree a) => ClauseMatrix Int -> [(Identifier, a)]
compileSpecialized cm = map splitByCtor $ headIds cm
  where
    splitByCtor :: (DecisionTree a) => (Identifier, Int) -> (Identifier, a)
    splitByCtor (cid, ar) =
      (cid, compilePatternMatch $ mSpecialization cm cid ar)

instance DecisionTree ([Occurence] -> Int) where
  dtLeaf v = \_ -> v
  dtFail = \_ -> -1     -- encode failure as a return of -1 for now
  dtSwitch (SwitchClauseList cs df) =
    \c@(Occurence _ cid _ : _) ->
      case lookup cid cs of
        Just fa -> fa (oSpecialize c)
        Nothing -> (fromJust df) (oDefault c)
    where
      -- | TODO: Duplication. O vs M specialization
      oSpecialize :: [Occurence] -> [Occurence]
      oSpecialize (Occurence _ _ ps : rest) = ps ++ rest
      oSpecialize []                        = []
      oDefault :: [Occurence] -> [Occurence]
      oDefault = tail
  dtSwap = undefined

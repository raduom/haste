{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module K.Match where

import           Data.Bifunctor  as B (first)
import           Data.Constraint (Dict (..), withDict)
import           Data.HVect
import qualified Data.List       as L (head, nubBy)
import           Data.Proxy
import           Prelude         hiding (head, tail)

-- Pattern

class TermUnfold (a :: *) where
  type TUnfold a :: [*]
  tUnfold :: [a] -> a -> HVect (TUnfold a)
  witness :: (AllHave Pattern tl)
          => p a
          -> Proxy tl
          -> Dict (AllHave Pattern (Append (TUnfold a) tl))

class (TermUnfold a) => Pattern a where
  tSpecialiseEliminate :: a -> a -> Bool
  tDefaultEliminate :: [a]     -- ^ The ctors used in the column
                    -> a       -- ^ The current ctor
                    -> Bool
  tIsWildcard :: a -> Bool

tSameConstructor :: (Pattern a) => a -> a -> Bool
tSameConstructor = tSpecialiseEliminate

type ClauseMatrix p = [(p, Int)]

mSpecialisation :: forall hd tl. (Pattern hd, AllHave Pattern tl)
                => ClauseMatrix (HVect (hd ': tl))
                -> hd
                -> ClauseMatrix (HVect (Append (TUnfold hd) tl))
mSpecialisation cm shd = map (B.first u) $ filter matchH cm
  where
    u :: (Pattern hd, AllHave Pattern tl) => HVect (hd ': tl) -> (HVect (Append (TUnfold hd) tl))
    u (hd :&: tl) = hAppend (tUnfold fc hd) tl
    fc = cmFirstColumn cm
    matchH :: (HVect (hd ': tl), Int) -> Bool
    matchH ((hd :&: _), _) = tSpecialiseEliminate hd shd

mDefault :: forall hd tl. (Pattern hd, AllHave Pattern tl)
         => ClauseMatrix (HVect (hd ': tl))
         -> Maybe (ClauseMatrix (HVect tl))
mDefault cm =
  maybeDefault $
  map (B.first tail) $
  filter matchD cm
  where
    maybeDefault :: [a] -> Maybe [a]
    maybeDefault [] = Nothing
    maybeDefault a  = Just a
    matchD :: (HVect (hd ': tl), Int) -> Bool
    matchD ((hd :&: _), _) = tDefaultEliminate fc hd
    fc = L.nubBy (tSameConstructor) $ cmFirstColumn cm

cmFirstColumn :: forall hd tl. ClauseMatrix (HVect (hd ': tl)) -> [hd]
cmFirstColumn = map (head . fst)

cmFirstPattern :: forall p. ClauseMatrix p -> p
cmFirstPattern = fst . L.head -- TODO: L.Head is unsafe

cmIsWildcardRow :: forall ps. (AllHave Pattern ps) => HVect ps -> Bool
cmIsWildcardRow HNil        = True
cmIsWildcardRow (hd :&: tl) = tIsWildcard hd && cmIsWildcardRow tl

-- Decision trees

class DecisionTree a where
  dtLeaf :: Int -> a
  dtFail :: a
  dtSwitch :: SwitchClauseList a -> a
  dtSwap :: Int -> a -> a

data SwitchClauseList a =
  SwitchClauseList { sclSpecialised :: ![a] -- Can I embed the identifier in a?
                   , sclDefault     :: !(Maybe a)
                   }

compilePatternMatch :: (DecisionTree a, AllHave Pattern ps)
                    => ClauseMatrix (HVect ps)
                    -> a
compilePatternMatch [] = dtFail
compilePatternMatch ((HNil, a) : _) = dtLeaf a
compilePatternMatch cm@(((_ :&: _), a) : _)
  | cmIsWildcardRow (cmFirstPattern cm) = dtLeaf a
  | otherwise = dtSwitch $
                SwitchClauseList { sclSpecialised = compileSpecialised cm
                                 , sclDefault = compileDefault cm
                                 }

compileSpecialised :: forall a hd tl. (DecisionTree a, Pattern hd, AllHave Pattern tl)
                   => ClauseMatrix (HVect (hd ': tl))
                   -> [a]
compileSpecialised cm =
  map (compile . mSpecialisation cm) $
  L.nubBy tSameConstructor $
  cmFirstColumn cm
  where
    compile :: ClauseMatrix (HVect (Append (TUnfold hd) tl)) -> a
    compile = withDict w compilePatternMatch
    w :: Dict (AllHave Pattern (Append (TUnfold hd) tl))
    w = witness (Proxy :: Proxy hd) (Proxy :: Proxy tl)

compileDefault :: forall a hd tl. (DecisionTree a, Pattern hd, AllHave Pattern tl)
               => ClauseMatrix (HVect (hd ': tl))
               -> Maybe a
compileDefault cm = compilePatternMatch <$> mDefault cm

-- HVect++

hAppend :: HVect as -> HVect bs -> HVect (Append as bs)
hAppend HNil bs         = bs
hAppend (ah :&: atl) bs = ah :&: hAppend atl bs

instance (Pattern a, Pattern b) => TermUnfold (a, b) where
  type TUnfold (a, b) = '[a, b]
  tUnfold _ (a, b) = a :&: b :&: HNil
  witness :: (AllHave Pattern tl) => p (x, y) -> Proxy tl -> Dict (AllHave Pattern (Append '[a, b] tl))
  witness _ _ = Dict

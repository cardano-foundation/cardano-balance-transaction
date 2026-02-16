{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Inlined from @cardano-wallet-test-utils@ 'Test.QuickCheck.Extra'.
-}
module Test.QuickCheck.Extra
    ( -- * Reporting
      report

      -- * NonEmpty
    , genNonEmpty
    , shrinkNonEmpty

      -- * DisjointPair
    , DisjointPair
    , genDisjointPair
    , getDisjointPair
    , shrinkDisjointPair

      -- * Map shrinking
    , shrinkMapToSubmaps

      -- * Comparison
    , (.>=.)

      -- * Natural
    , shrinkNatural

      -- * Generic shrinking (SOP)
    , genericRoundRobinShrink
    , (<:>)
    , (<@>)
    )
where

import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Generics.SOP
    ( I (..)
    , NP (..)
    , SOP (..)
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Gen
    , Property
    , Testable
    , counterexample
    , listOf
    , property
    , shrinkList
    , shrinkMapBy
    )
import Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Generics.SOP as SOP

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

-- | Add a named variable to a counterexample.
report :: (Show a, Testable prop) => a -> String -> prop -> Property
report a name = counterexample (name <> ":\n    " <> show a)

--------------------------------------------------------------------------------
-- NonEmpty
--------------------------------------------------------------------------------

-- | Generate a non-empty list.
genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = (:|) <$> g <*> listOf g

-- | Shrink a non-empty list.
shrinkNonEmpty :: (a -> [a]) -> NonEmpty a -> [NonEmpty a]
shrinkNonEmpty f = mapMaybe NE.nonEmpty . shrinkList f . NE.toList

--------------------------------------------------------------------------------
-- DisjointPair
--------------------------------------------------------------------------------

-- | A pair of values made disjoint via 'Monus'.
data DisjointPair a = DisjointPair !a !a
    deriving (Eq, Show)

-- | Get the two disjoint values.
getDisjointPair :: DisjointPair a -> (a, a)
getDisjointPair (DisjointPair a b) = (a, b)

-- | Generate a disjoint pair from two generators.
genDisjointPair :: (Monus a) => Gen a -> Gen (DisjointPair a)
genDisjointPair g = makeDisjointPair <$> g <*> g

makeDisjointPair :: (Monus a) => a -> a -> DisjointPair a
makeDisjointPair a1 a2 = DisjointPair (a1 <\> a2) (a2 <\> a1)

-- | Shrink a disjoint pair.
shrinkDisjointPair
    :: (Monus a) => (a -> [a]) -> DisjointPair a -> [DisjointPair a]
shrinkDisjointPair f (DisjointPair a b) =
    [makeDisjointPair a' b | a' <- f a]
        <> [makeDisjointPair a b' | b' <- f b]

--------------------------------------------------------------------------------
-- Map shrinking
--------------------------------------------------------------------------------

-- | Shrink a map to proper submaps.
shrinkMapToSubmaps :: (Ord k) => Map.Map k v -> [Map.Map k v]
shrinkMapToSubmaps =
    shrinkMapBy Map.fromList Map.toList shrinkToSublist
  where
    shrinkToSublist = shrinkList (const [])

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

-- | Property: a >= b.
(.>=.) :: (Show a, Ord a) => a -> a -> Property
a .>=. b = counterexample (show a <> " < " <> show b) $ property $ a >= b

infix 4 .>=.

--------------------------------------------------------------------------------
-- Natural
--------------------------------------------------------------------------------

-- | Shrink a 'Natural' value.
shrinkNatural :: Natural -> [Natural]
shrinkNatural 0 = []
shrinkNatural n =
    0 : [n - i | i <- takeWhile (< n) (iterate (* 2) 1)]

--------------------------------------------------------------------------------
-- Generic shrinking (SOP)
--------------------------------------------------------------------------------

-- | Round-robin shrinking using generics-sop.
genericRoundRobinShrink
    :: ( SOP.Generic a
       , SOP.Code a ~ '[xs]
       )
    => NP Shrinker xs
    -> a
    -> [a]
genericRoundRobinShrink fs x =
    SOP.to <$> groundRobinShrinkSOP fs (SOP.from x)

-- | A shrinker for a single field.
newtype Shrinker a = Shrinker (a -> [a])

-- | Function application (like '$') with lower fixity than '<:>'.
(<@>) :: (a -> b) -> a -> b
(<@>) = ($)

infixl 4 <@>

-- | Cons a field shrinker onto the list.
(<:>) :: (x -> [x]) -> NP Shrinker xs -> NP Shrinker (x : xs)
f <:> fs = Shrinker f :* fs

infixr 5 <:>

groundRobinShrinkSOP
    :: NP Shrinker xs
    -> SOP I '[xs]
    -> [SOP I '[xs]]
groundRobinShrinkSOP fs (SOP (SOP.Z np)) =
    SOP . SOP.Z <$> groundRobinShrinkNP fs np
groundRobinShrinkSOP _ (SOP (SOP.S ns)) = case ns of {}

groundRobinShrinkNP
    :: NP Shrinker xs
    -> NP I xs
    -> [NP I xs]
groundRobinShrinkNP Nil Nil = []
groundRobinShrinkNP (Shrinker f :* fs) (I x :* xs) =
    interleave
        [I x' :* xs | x' <- f x]
        [I x :* xs' | xs' <- groundRobinShrinkNP fs xs]

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x : xs) (y : ys) = x : y : interleave xs ys

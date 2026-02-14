{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Provides generators and shrinkers for the 'TxWithUTxO' data type.
module Cardano.Balance.Tx.TxWithUTxO.Gen
    ( generate
    , generateWithMinimalUTxO
    , generateWithSurplusUTxO
    , shrinkWith
    , shrinkTxWith
    , shrinkUTxOWith
    )
where

import Cardano.Balance.Tx.Eras
    ( IsRecentEra
    )
import Cardano.Balance.Tx.Tx
    ( Tx
    , TxIn
    , TxOut
    , UTxO (UTxO)
    )
import Cardano.Balance.Tx.TxWithUTxO
    ( pattern TxWithUTxO
    , type TxWithUTxO
    )
import Cardano.Ledger.Api
    ( EraTx (bodyTxL)
    )
import Cardano.Ledger.Api.Tx.Body
    ( allInputsTxBodyF
    )
import Control.Lens
    ( view
    )
import Control.Monad
    ( replicateM
    )
import Data.List
    ( transpose
    )
import Prelude
import Test.QuickCheck
    ( Gen
    , Positive (..)
    , arbitrary
    , frequency
    , suchThat
    )

import qualified Cardano.Balance.Tx.TxWithUTxO as TxWithUTxO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Generates a 'TxWithUTxO' object.
--
-- The domain of the UTxO map is a superset of the transaction input set, but
-- it may or may not be a strict superset.
generate
    :: IsRecentEra era
    => Gen (Tx era)
    -> Gen (TxIn)
    -> Gen (TxOut era)
    -> Gen (TxWithUTxO era)
generate genTx genTxIn genTxOut =
    frequency
        [ (9, generateWithMinimalUTxO genTx genTxOut)
        , (1, generateWithSurplusUTxO genTx genTxIn genTxOut)
        ]

-- | Generates a 'TxWithUTxO' object that has a minimal UTxO set.
--
-- The domain of the UTxO map is exactly equal to the transaction input set.
generateWithMinimalUTxO
    :: IsRecentEra era
    => Gen (Tx era)
    -> Gen (TxOut era)
    -> Gen (TxWithUTxO era)
generateWithMinimalUTxO genTx genTxOut = do
    tx <- genTx
    utxo <- UTxO <$> genMapFromKeysWith genTxOut (txAllInputs tx)
    pure $ TxWithUTxO.constructFiltered tx utxo
  where
    txAllInputs = view (bodyTxL . allInputsTxBodyF)

-- | Generates a 'TxWithUTxO' object that has a surplus UTxO set.
--
-- The domain of the UTxO map is a strict superset of the transaction input set.
generateWithSurplusUTxO
    :: forall era
     . ()
    => IsRecentEra era
    => Gen (Tx era)
    -> Gen (TxIn)
    -> Gen (TxOut era)
    -> Gen (TxWithUTxO era)
generateWithSurplusUTxO genTx genTxIn genTxOut =
    generateWithMinimalUTxO genTx genTxOut >>= \case
        TxWithUTxO tx (UTxO utxo) -> do
            utxoSurplus <- genNonEmptyDisjointMap genTxIn genTxOut utxo
            pure $ TxWithUTxO.constructFiltered tx $ UTxO (utxo <> utxoSurplus)

shrinkWith
    :: IsRecentEra era
    => (Tx era -> [Tx era])
    -> (UTxO era -> [UTxO era])
    -> (TxWithUTxO era -> [TxWithUTxO era])
shrinkWith shrinkTx shrinkUTxO txWithUTxO =
    interleaveRoundRobin
        [ shrinkTxWith shrinkTx txWithUTxO
        , shrinkUTxOWith shrinkUTxO txWithUTxO
        ]

shrinkTxWith
    :: IsRecentEra era
    => (Tx era -> [Tx era])
    -> (TxWithUTxO era -> [TxWithUTxO era])
shrinkTxWith shrinkTx (TxWithUTxO tx utxo) =
    [TxWithUTxO.constructFiltered tx' utxo | tx' <- shrinkTx tx]

shrinkUTxOWith
    :: IsRecentEra era
    => (UTxO era -> [UTxO era])
    -> (TxWithUTxO era -> [TxWithUTxO era])
shrinkUTxOWith shrinkUTxO (TxWithUTxO tx utxo) =
    [TxWithUTxO.constructFiltered tx utxo' | utxo' <- shrinkUTxO utxo]

--------------------------------------------------------------------------------
-- Inlined from Test.QuickCheck.Extra (cardano-wallet-test-utils)
--------------------------------------------------------------------------------

interleaveRoundRobin :: [[a]] -> [a]
interleaveRoundRobin = concat . transpose

genMapFromKeysWith :: Ord k => Gen v -> Set.Set k -> Gen (Map.Map k v)
genMapFromKeysWith genValue =
    fmap Map.fromList . mapM (\k -> (k,) <$> genValue) . Set.toList

genNonEmptyDisjointMap
    :: Ord k => Gen k -> Gen v -> Map.Map k v -> Gen (Map.Map k v)
genNonEmptyDisjointMap genKey genValue existingMap =
    genMapFromKeysWith genValue
        =<< genNonEmptyDisjointSet genKey (Map.keysSet existingMap)

genNonEmptyDisjointSet :: Ord a => Gen a -> Set.Set a -> Gen (Set.Set a)
genNonEmptyDisjointSet genElement0 existingElements = do
    size <- getPositive <$> arbitrary @(Positive Int)
    Set.fromList <$> replicateM size genElement
  where
    genElement = genElement0 `suchThat` (`Set.notMember` existingElements)

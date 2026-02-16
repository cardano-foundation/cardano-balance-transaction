{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Conversions between primitive types and ledger types.
Replaces @Cardano.Wallet.Primitive.Ledger.Convert@.
-}
module Cardano.Balance.Tx.Primitive.Convert
    ( -- * Coin
      toLedgerCoin
    , fromLedgerCoin

      -- * TokenBundle / Value
    , toLedgerTokenBundle
    , fromLedgerTokenBundle

      -- * TxIn
    , toLedgerTxIn
    , fromLedgerTxIn

      -- * Address
    , toLedgerAddress
    , fromLedgerAddress

      -- * TxOut (era-specific)
    , toBabbageTxOut
    , toConwayTxOut
    , fromBabbageTxOut
    , fromConwayTxOut

      -- * UTxO (era-specific)
    , toLedgerUTxOBabbage
    , toLedgerUTxOConway
    , fromLedgerUTxOBabbage
    , fromLedgerUTxOConway

      -- * PolicyId / AssetName
    , toLedgerPolicyId
    , toLedgerAssetName
    , fromLedgerPolicyId
    , fromLedgerAssetName

      -- * Scripts
    , toLedgerTimelockScript
    , toWalletScript
    )
where

import Cardano.CoinSelection.Types.AssetId
    ( AssetId (..)
    )
import Cardano.CoinSelection.Types.AssetName
    ( AssetName (..)
    )
import Cardano.CoinSelection.Types.Coin
    ( Coin (..)
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.CoinSelection.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.CoinSelection.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Crypto.Hash
    ( hashFromBytes
    )
import Cardano.Ledger.Allegra.Scripts
    ( AllegraEraScript
    , Timelock
    )
import Cardano.Ledger.Api
    ( BabbageEra
    , ConwayEra
    )
import Cardano.Ledger.Api.UTxO
    ( UTxO (..)
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxOut (..)
    )
import Cardano.Ledger.BaseTypes
    ( StrictMaybe (SNothing)
    )
import Cardano.Ledger.Hashes
    ( ScriptHash (..)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset (..)
    )
import Cardano.Ledger.Plutus.Data
    ( Datum (NoDatum)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Data.ByteString
    ( ByteString
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Maybe
    ( fromMaybe
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

import qualified Cardano.Address.KeyHash as CA
import qualified Cardano.Address.Script as CA
import qualified Cardano.Balance.Tx.Primitive as W
import qualified Cardano.CoinSelection.Types.Hash as CS
import qualified Cardano.CoinSelection.Types.TokenMap as CS.TokenMap
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Scripts
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.ByteString.Short as SBS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

-- | Convert to ledger 'Coin'.
toLedgerCoin :: W.Coin -> Ledger.Coin
toLedgerCoin (Coin n) = Ledger.Coin (intCast n)

-- | Convert from ledger 'Coin'.
fromLedgerCoin :: Ledger.Coin -> W.Coin
fromLedgerCoin (Ledger.Coin n) = case intCastMaybe n of
    Just nat -> Coin nat
    Nothing -> error $ "fromLedgerCoin: negative: " <> show n

--------------------------------------------------------------------------------
-- TokenBundle / Value
--------------------------------------------------------------------------------

-- | Convert a 'TokenBundle' to a ledger 'MaryValue'.
toLedgerTokenBundle :: W.TokenBundle -> MaryValue
toLedgerTokenBundle (TokenBundle c tm) =
    MaryValue (toLedgerCoin c) multiAsset
  where
    multiAsset :: MultiAsset
    multiAsset =
        MultiAsset
            . Map.fromListWith (Map.unionWith (+))
            . fmap toEntry
            $ CS.TokenMap.toFlatList tm

    toEntry
        :: (AssetId, TokenQuantity)
        -> (Ledger.PolicyID, Map.Map Ledger.AssetName Integer)
    toEntry (AssetId pId aName, TokenQuantity qty) =
        ( toLedgerPolicyId pId
        , Map.singleton (toLedgerAssetName aName) (intCast qty)
        )

-- | Convert a ledger 'MaryValue' to a 'TokenBundle'.
fromLedgerTokenBundle :: MaryValue -> W.TokenBundle
fromLedgerTokenBundle (MaryValue ledgerAda (MultiAsset ledgerTokens)) =
    TokenBundle (fromLedgerCoin ledgerAda) walletTokenMap
  where
    walletTokenMap =
        CS.TokenMap.fromFlatList $
            concatMap expandPolicy $
                Map.toList ledgerTokens

    expandPolicy
        :: (Ledger.PolicyID, Map.Map Ledger.AssetName Integer)
        -> [(AssetId, TokenQuantity)]
    expandPolicy (pId, assets) =
        [ ( AssetId (fromLedgerPolicyId pId) (fromLedgerAssetName aName)
          , toTokenQuantity qty
          )
        | (aName, qty) <- Map.toList assets
        ]

    toTokenQuantity :: Integer -> TokenQuantity
    toTokenQuantity q
        | q >= 0 = TokenQuantity $ fromIntegral q
        | otherwise =
            error $ "fromLedgerTokenBundle: negative quantity: " <> show q

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Convert to a ledger 'TxIn'.
toLedgerTxIn :: W.TxIn -> Ledger.TxIn
toLedgerTxIn (W.TxIn tid ix) =
    Ledger.TxIn (toLedgerTxId tid) (toEnum $ intCast ix)
  where
    toLedgerTxId h =
        Ledger.TxId $
            Hashes.unsafeMakeSafeHash $
                Crypto.UnsafeHash $
                    SBS.toShort h

-- | Convert from a ledger 'TxIn'.
fromLedgerTxIn :: Ledger.TxIn -> W.TxIn
fromLedgerTxIn (Ledger.TxIn (Ledger.TxId tid) ix) =
    W.TxIn (convertId tid) (convertIx ix)
  where
    convertId = Crypto.hashToBytes . Hashes.extractHash
    convertIx = fromMaybe err . intCastMaybe . fromEnum
      where
        err =
            error $
                unwords
                    [ "fromLedgerTxIn:"
                    , "Unexpected out of bounds TxIx"
                    , show ix
                    ]

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

-- | Convert to a ledger 'Addr'.
toLedgerAddress :: W.Address -> Ledger.Addr
toLedgerAddress (W.Address bytes) = case Ledger.decodeAddr bytes of
    Just addr -> addr
    Nothing ->
        error $
            unwords
                [ "toLedgerAddress: invalid address"
                , show bytes
                ]

-- | Convert from a ledger 'Addr'.
fromLedgerAddress :: Ledger.Addr -> W.Address
fromLedgerAddress = W.Address . Ledger.serialiseAddr

--------------------------------------------------------------------------------
-- TxOut (era-specific)
--------------------------------------------------------------------------------

-- | Convert to a Babbage-era ledger 'TxOut'.
toBabbageTxOut :: W.TxOut -> BabbageTxOut BabbageEra
toBabbageTxOut (W.TxOut addr bundle) =
    BabbageTxOut
        (toLedgerAddress addr)
        (toLedgerTokenBundle bundle)
        NoDatum
        SNothing

-- | Convert to a Conway-era ledger 'TxOut'.
toConwayTxOut :: W.TxOut -> BabbageTxOut ConwayEra
toConwayTxOut (W.TxOut addr bundle) =
    BabbageTxOut
        (toLedgerAddress addr)
        (toLedgerTokenBundle bundle)
        NoDatum
        SNothing

{- | Convert from a Babbage-era ledger 'TxOut'.
Inline scripts and datums are discarded.
-}
fromBabbageTxOut :: BabbageTxOut BabbageEra -> W.TxOut
fromBabbageTxOut (BabbageTxOut addr val _ _) =
    W.TxOut (fromLedgerAddress addr) (fromLedgerTokenBundle val)

{- | Convert from a Conway-era ledger 'TxOut'.
Inline scripts and datums are discarded.
-}
fromConwayTxOut :: BabbageTxOut ConwayEra -> W.TxOut
fromConwayTxOut (BabbageTxOut addr val _ _) =
    W.TxOut (fromLedgerAddress addr) (fromLedgerTokenBundle val)

--------------------------------------------------------------------------------
-- UTxO (era-specific)
--------------------------------------------------------------------------------

-- | Convert to a Babbage-era ledger 'UTxO'.
toLedgerUTxOBabbage :: W.UTxO -> UTxO BabbageEra
toLedgerUTxOBabbage (W.UTxO m) =
    UTxO $
        Map.mapKeys toLedgerTxIn $
            Map.map toBabbageTxOut m

-- | Convert to a Conway-era ledger 'UTxO'.
toLedgerUTxOConway :: W.UTxO -> UTxO ConwayEra
toLedgerUTxOConway (W.UTxO m) =
    UTxO $
        Map.mapKeys toLedgerTxIn $
            Map.map toConwayTxOut m

-- | Convert from a Babbage-era ledger 'UTxO'.
fromLedgerUTxOBabbage :: UTxO BabbageEra -> W.UTxO
fromLedgerUTxOBabbage (UTxO m) =
    W.UTxO $
        Map.mapKeys fromLedgerTxIn $
            Map.map fromBabbageTxOut m

-- | Convert from a Conway-era ledger 'UTxO'.
fromLedgerUTxOConway :: UTxO ConwayEra -> W.UTxO
fromLedgerUTxOConway (UTxO m) =
    W.UTxO $
        Map.mapKeys fromLedgerTxIn $
            Map.map fromConwayTxOut m

--------------------------------------------------------------------------------
-- PolicyId / AssetName
--------------------------------------------------------------------------------

-- | Convert to a ledger 'PolicyID'.
toLedgerPolicyId :: TokenPolicyId -> Ledger.PolicyID
toLedgerPolicyId (UnsafeTokenPolicyId (CS.Hash h)) =
    Ledger.PolicyID $ ScriptHash $ case hashFromBytes h of
        Just hsh -> hsh
        Nothing -> error "toLedgerPolicyId: invalid hash"

-- | Convert to a ledger 'AssetName'.
toLedgerAssetName :: AssetName -> Ledger.AssetName
toLedgerAssetName (UnsafeAssetName n) =
    Ledger.AssetName $ SBS.toShort n

-- | Convert from a ledger 'PolicyID'.
fromLedgerPolicyId :: Ledger.PolicyID -> TokenPolicyId
fromLedgerPolicyId (Ledger.PolicyID (ScriptHash hash)) =
    UnsafeTokenPolicyId (CS.Hash (Crypto.hashToBytes hash))

-- | Convert from a ledger 'AssetName'.
fromLedgerAssetName :: Ledger.AssetName -> AssetName
fromLedgerAssetName (Ledger.AssetName bytes) =
    UnsafeAssetName $ SBS.fromShort bytes

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

-- | Convert a @cardano-addresses@ 'CA.Script' to a ledger 'Timelock' script.
toLedgerTimelockScript
    :: forall era
     . ( AllegraEraScript era
       , Core.NativeScript era ~ Timelock era
       )
    => CA.Script CA.KeyHash
    -> Timelock era
toLedgerTimelockScript = \case
    CA.RequireSignatureOf (CA.KeyHash _ keyhash) ->
        case hashFromBytes keyhash of
            Just h ->
                Scripts.mkRequireSignatureTimelock (Ledger.KeyHash h)
            Nothing ->
                error "toLedgerTimelockScript: invalid key hash"
    CA.RequireAllOf contents ->
        Scripts.mkRequireAllOfTimelock $
            StrictSeq.fromList $
                map toLedgerTimelockScript contents
    CA.RequireAnyOf contents ->
        Scripts.mkRequireAnyOfTimelock $
            StrictSeq.fromList $
                map toLedgerTimelockScript contents
    CA.RequireSomeOf num contents ->
        Scripts.mkRequireMOfTimelock
            (intCast num)
            $ StrictSeq.fromList
            $ map toLedgerTimelockScript contents
    CA.ActiveUntilSlot slot ->
        Scripts.RequireTimeExpire (convertSlotNo slot)
    CA.ActiveFromSlot slot ->
        Scripts.RequireTimeStart (convertSlotNo slot)
  where
    convertSlotNo :: Natural -> SlotNo
    convertSlotNo x = SlotNo $ fromMaybe err $ intCastMaybe x
      where
        err =
            error $
                unwords
                    [ "toLedgerTimelockScript:"
                    , "Unexpected out of bounds SlotNo"
                    , show x
                    ]

-- | Convert a ledger 'Timelock' script to a @cardano-addresses@ 'CA.Script'.
toWalletScript
    :: forall era
     . ( AllegraEraScript era
       , Core.NativeScript era ~ Timelock era
       )
    => (ByteString -> CA.KeyRole)
    -> Timelock era
    -> CA.Script CA.KeyHash
toWalletScript tokeyrole tl
    | Just (Ledger.KeyHash h) <-
        Scripts.getRequireSignatureTimelock tl =
        let payload = Crypto.hashToBytes h
        in  CA.RequireSignatureOf (CA.KeyHash (tokeyrole payload) payload)
    | Just contents <- Scripts.getRequireAllOfTimelock tl =
        CA.RequireAllOf $
            map (toWalletScript tokeyrole) $
                F.toList contents
    | Just contents <- Scripts.getRequireAnyOfTimelock tl =
        CA.RequireAnyOf $
            map (toWalletScript tokeyrole) $
                F.toList contents
    | Just (num, contents) <- Scripts.getRequireMOfTimelock tl =
        CA.RequireSomeOf (fromIntegral num) $
            map (toWalletScript tokeyrole) $
                F.toList contents
    | Scripts.RequireTimeExpire (SlotNo slot) <- tl =
        CA.ActiveUntilSlot $ fromIntegral slot
    | Scripts.RequireTimeStart (SlotNo slot) <- tl =
        CA.ActiveFromSlot $ fromIntegral slot
    | otherwise =
        error "toWalletScript: unrecognized Timelock script"

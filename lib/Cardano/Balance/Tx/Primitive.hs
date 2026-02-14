{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Lightweight primitive types for transaction balancing, inlined
-- from @cardano-wallet-primitive@.
--
-- Intended to be imported qualified as @W@:
--
-- @
-- import qualified Cardano.Balance.Tx.Primitive as W
-- @
module Cardano.Balance.Tx.Primitive
    ( -- * Value types (re-exported from cardano-coin-selection)
      Coin (..)
    , TokenBundle (TokenBundle)
    , TokenMap
    , AssetId (..)
    , AssetName (..)
    , TokenQuantity (..)
    , TokenPolicyId (..)
    , Hash (..)

      -- * TxSize
    , TxSize (..)

      -- * Address
    , Address (..)

      -- * TxIn
    , TxIn (..)

      -- * TxOut
    , TxOut (..)

      -- * UTxO
    , UTxO (..)

      -- * Constants
    , txOutMaxCoin
    , txOutMaxTokenQuantity

      -- * Collateral
    , asCollateral

      -- * Coin utilities
    , unsafeToWord64
    , unsafeFromIntegral

      -- * TokenBundle accessors
    , getCoin
    , getAssets
    , fromCoin
    , Flat (..)

      -- * TxOut accessors
    , coin
    , addCoin

      -- * TokenMap re-exports
    , toFlatList
    , fromFlatList
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
import Cardano.CoinSelection.Types.Hash
    ( Hash (..)
    )
import Cardano.CoinSelection.Types.TokenBundle
    ( TokenBundle (TokenBundle)
    )
import Cardano.CoinSelection.Types.TokenMap
    ( TokenMap
    )
import Cardano.CoinSelection.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.CoinSelection.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.ByteString
    ( ByteString
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Data.Word
    ( Word32
    , Word64
    )
import Fmt
    ( Buildable (..)
    , hexF
    , ordinalF
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

import qualified Cardano.CoinSelection.Types.TokenMap as CS.TokenMap
import qualified Data.ByteString as BS
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- TxSize
--------------------------------------------------------------------------------

-- | Transaction size in bytes.
newtype TxSize = TxSize { unTxSize :: Natural }
    deriving stock (Eq, Ord, Show)
    deriving newtype (Num)

instance Semigroup TxSize where
    TxSize a <> TxSize b = TxSize (a + b)

instance Monoid TxSize where
    mempty = TxSize 0

--------------------------------------------------------------------------------
-- Address (serialized bytes)
--------------------------------------------------------------------------------

-- | A serialized Cardano address.
newtype Address = Address { unAddress :: ByteString }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData)

instance Buildable Address where
    build (Address bytes) = hexF (BS.take 8 bytes) <> "..."

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | A transaction input reference (hash + index).
data TxIn = TxIn
    { txId :: !ByteString
    , txIx :: !Word32
    }
    deriving (Eq, Generic, Ord, Show)

instance NFData TxIn

instance Buildable TxIn where
    build (TxIn tid ix) =
        ordinalF (ix + 1) <> " " <> hexF (BS.take 8 tid) <> "..."

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | A transaction output (address + value).
data TxOut = TxOut
    { address :: !Address
    , tokens :: !TokenBundle
    }
    deriving (Eq, Generic, Show)

instance NFData TxOut

-- | Get the ada coin from a TxOut. Matches @W.TxOut.coin@.
coin :: TxOut -> Coin
coin TxOut{tokens = TokenBundle c _} = c

-- | Add ada to a TxOut. Matches @W.TxOut.addCoin@.
addCoin :: Coin -> TxOut -> TxOut
addCoin c TxOut{address, tokens = TokenBundle c0 tm} =
    TxOut address (TokenBundle (c0 <> c) tm)

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | A set of unspent transaction outputs.
newtype UTxO = UTxO { unUTxO :: Map TxIn TxOut }
    deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Maximum coin for a transaction output (~45B ADA).
txOutMaxCoin :: Coin
txOutMaxCoin = Coin 45_000_000_000_000_000

-- | Maximum token quantity for a transaction output.
txOutMaxTokenQuantity :: TokenQuantity
txOutMaxTokenQuantity = TokenQuantity $ fromIntegral (maxBound :: Word64)

--------------------------------------------------------------------------------
-- Collateral
--------------------------------------------------------------------------------

-- | Returns 'Just' the ada value if the output is ada-only.
asCollateral :: TxOut -> Maybe Coin
asCollateral TxOut{tokens = TokenBundle c tm}
    | tm == mempty = Just c
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Coin utilities
--------------------------------------------------------------------------------

-- | Unsafe conversion to 'Word64'. Errors on overflow.
unsafeToWord64 :: Coin -> Word64
unsafeToWord64 (Coin n)
    | n > fromIntegral (maxBound :: Word64) =
        error "unsafeToWord64: overflow"
    | otherwise =
        fromIntegral n

-- | Unsafe conversion from 'Integer'. Errors on negative values.
unsafeFromIntegral :: Integer -> Coin
unsafeFromIntegral n
    | n < 0 = error $ "unsafeFromIntegral: negative: " <> show n
    | otherwise = Coin (fromIntegral n)

--------------------------------------------------------------------------------
-- TokenBundle accessors
--------------------------------------------------------------------------------

-- | Get the coin component of a 'TokenBundle'.
getCoin :: TokenBundle -> Coin
getCoin (TokenBundle c _) = c

-- | Get the set of asset IDs in a 'TokenBundle'.
getAssets :: TokenBundle -> Set AssetId
getAssets (TokenBundle _ tm) =
    Set.fromList $ map fst $ CS.TokenMap.toFlatList tm

-- | Create a 'TokenBundle' from a 'Coin' (no native assets).
fromCoin :: Coin -> TokenBundle
fromCoin c = TokenBundle c mempty

-- | For display — wraps a 'TokenBundle' for flat rendering.
newtype Flat = Flat TokenBundle
    deriving (Show)

instance Buildable Flat where
    build (Flat (TokenBundle (Coin c) tm)) =
        build (show c) <> " lovelace + "
            <> build (show (length (CS.TokenMap.toFlatList tm)))
            <> " assets"

--------------------------------------------------------------------------------
-- TokenMap re-exports
--------------------------------------------------------------------------------

-- | Convert a 'TokenMap' to a flat list of @(AssetId, TokenQuantity)@.
toFlatList :: TokenMap -> [(AssetId, TokenQuantity)]
toFlatList = CS.TokenMap.toFlatList

-- | Construct a 'TokenMap' from a flat list.
fromFlatList :: [(AssetId, TokenQuantity)] -> TokenMap
fromFlatList = CS.TokenMap.fromFlatList

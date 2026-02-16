{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

QuickCheck generators and shrinkers for primitive types.

Generators match the distributions from @cardano-wallet-primitive@ so that
existing property-test coverage thresholds and golden expectations remain
valid.
-}
module Cardano.Balance.Tx.Primitive.Gen
    ( -- * Coin
      genCoin
    , genCoinPositive
    , shrinkCoin
    , shrinkCoinPositive

      -- * Address
    , genAddress

      -- * TxIn
    , genTxIn
    , shrinkTxIn

      -- * TxOut
    , genTxOut
    , shrinkTxOut

      -- * UTxO
    , genUTxO
    , genUTxOLarge
    , shrinkUTxO

      -- * TokenBundle
    , genTokenBundle
    , genTokenBundleSmallRange
    , shrinkTokenBundle
    , shrinkTokenBundleSmallRange
    , genTxOutTokenBundle

      -- * TokenMap
    , genTokenMap
    , shrinkTokenMap

      -- * Encoding boundary
    , genEncodingBoundaryCoin

      -- * Helpers
    , shrinkNatural
    , power
    )
where

import Cardano.Balance.Tx.Primitive
    ( Address (..)
    , AssetId (..)
    , AssetName (..)
    , Coin (..)
    , Hash (..)
    , TokenBundle (TokenBundle)
    , TokenMap
    , TokenPolicyId (..)
    , TokenQuantity (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    )
import Control.Monad
    ( replicateM
    )
import Data.Word
    ( Word32
    , Word8
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , frequency
    , oneof
    , shrink
    , shrinkList
    , shrinkMapBy
    , sized
    , vectorOf
    )
import Prelude

import qualified Cardano.Balance.Tx.Primitive as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

-- | Generate a random 'Coin', scaled by the QuickCheck size parameter.
genCoin :: Gen Coin
genCoin = sized $ \n -> Coin . fromIntegral <$> choose @Int (0, n)

-- | Generate a positive 'Coin'.
genCoinPositive :: Gen Coin
genCoinPositive = sized $ \n -> Coin . fromIntegral <$> choose @Int (1, max 1 n)

-- | Shrink a 'Coin'.
shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin n) = [Coin n' | n' <- shrinkNatural n]

-- | Shrink a positive 'Coin', keeping it positive.
shrinkCoinPositive :: Coin -> [Coin]
shrinkCoinPositive (Coin n) = [Coin n' | n' <- shrinkNatural n, n' > 0]

{- | Generate a 'Coin' near CBOR encoding boundaries (for testing
serialization size). Boundaries occur at 24, 256, 65536, 2^32.
-}
genEncodingBoundaryCoin :: Gen Coin
genEncodingBoundaryCoin = do
    -- Pick a boundary and generate a value near it
    boundary <- elements boundaries
    delta <- choose (-2, 2)
    let n = max 0 (boundary + delta)
    pure $ Coin (fromIntegral @Integer n)
  where
    boundaries =
        [ 0
        , 23
        , 24
        , 255
        , 256
        , 65_535
        , 65_536
        , 4_294_967_295
        , 4_294_967_296
        ]

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

-- | Generate a random 'Address'.
genAddress :: Gen Address
genAddress = do
    len <- elements [29, 57]
    Address . BS.pack <$> vectorOf len arbitrary

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Generate a random 'TxIn'.
genTxIn :: Gen TxIn
genTxIn =
    TxIn . BS.pack
        <$> vectorOf 32 arbitrary
        <*> genTxIx
  where
    genTxIx :: Gen Word32
    genTxIx =
        frequency
            [ (4, pure 0)
            , (3, choose (1, 5))
            , (2, choose (6, 20))
            , (1, choose (21, 255))
            ]

-- | Shrink a 'TxIn'.
shrinkTxIn :: TxIn -> [TxIn]
shrinkTxIn (TxIn tid tix) =
    [TxIn tid tix' | tix' <- shrink tix, tix' < tix]

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Generate a random 'TxOut'.
genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genTokenBundleSmallRange

-- | Shrink a 'TxOut'.
shrinkTxOut :: TxOut -> [TxOut]
shrinkTxOut (TxOut addr bundle) =
    [TxOut addr bundle' | bundle' <- shrinkTokenBundleSmallRange bundle]

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | Generate a random 'UTxO'.
genUTxO :: Gen UTxO
genUTxO = sized $ \n -> do
    k <- choose (0, n)
    entries <- vectorOf k ((,) <$> genTxIn <*> genTxOut)
    pure $ UTxO $ Map.fromList entries

-- | Generate a large 'UTxO' (for stress testing).
genUTxOLarge :: Gen UTxO
genUTxOLarge = do
    k <- choose (50, 200)
    entries <- vectorOf k ((,) <$> genTxIn <*> genTxOut)
    pure $ UTxO $ Map.fromList entries

-- | Shrink a 'UTxO'.
shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO (UTxO m)
    | Map.null m = []
    | otherwise =
        -- Try removing one entry at a time
        [UTxO (Map.deleteAt i m) | i <- [0 .. Map.size m - 1]]

--------------------------------------------------------------------------------
-- TokenBundle
--------------------------------------------------------------------------------

-- | Generate a random 'TokenBundle' (possibly with many assets).
genTokenBundle :: Gen TokenBundle
genTokenBundle =
    TokenBundle
        <$> genCoin
        <*> genTokenMap

-- | Generate a random 'TokenBundle' with few assets (0, 1, or 2-16).
genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange =
    TokenBundle
        <$> genCoin
        <*> genTokenMapSmallRange

-- | Shrink a 'TokenBundle'.
shrinkTokenBundle :: TokenBundle -> [TokenBundle]
shrinkTokenBundle (TokenBundle c tm) =
    [TokenBundle c' tm | c' <- shrinkCoin c]
        <> [TokenBundle c tm' | tm' <- shrinkTokenMap tm]

-- | Shrink a small-range 'TokenBundle'.
shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange = shrinkTokenBundle

{- | Generate a 'TokenBundle' with a specific number of distinct assets.
Uses large-range generators for policy IDs and asset names to avoid
collisions when generating many assets.
-}
genTxOutTokenBundle :: Int -> Gen TokenBundle
genTxOutTokenBundle n = do
    c <- genCoin
    assets <- vectorOf n genAssetIdAndQuantity
    pure $ TokenBundle c (W.fromFlatList assets)
  where
    genAssetIdAndQuantity =
        (,)
            <$> genAssetIdLargeRange
            <*> genTokenQuantityPositive

    genTokenQuantityPositive =
        TokenQuantity . fromIntegral <$> choose @Int (1, 1_000_000)

--------------------------------------------------------------------------------
-- TokenMap
--------------------------------------------------------------------------------

-- | Generate a random 'TokenMap', scaled by the size parameter.
genTokenMap :: Gen TokenMap
genTokenMap = sized $ \size -> do
    assetCount <- choose (0, size)
    W.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,) <$> genAssetId <*> genTokenQuantity

-- | Generate a 'TokenMap' with 0, 1, or 2-16 assets.
genTokenMapSmallRange :: Gen TokenMap
genTokenMapSmallRange = do
    assetCount <-
        oneof
            [ pure 0
            , pure 1
            , choose (2, 16)
            ]
    W.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,) <$> genAssetId <*> genTokenQuantity

-- | Shrink a 'TokenMap'.
shrinkTokenMap :: TokenMap -> [TokenMap]
shrinkTokenMap =
    shrinkMapBy W.fromFlatList W.toFlatList (shrinkList shrinkEntry)
  where
    shrinkEntry (aid, TokenQuantity q) =
        [(aid, TokenQuantity q') | q' <- shrinkNatural q, q' > 0]

--------------------------------------------------------------------------------
-- Internal generators: asset components
--------------------------------------------------------------------------------

-- | Generate an 'AssetId' from the small predefined range (size-dependent).
genAssetId :: Gen AssetId
genAssetId =
    AssetId
        <$> genTokenPolicyId
        <*> genAssetName

-- | Generate an 'AssetId' from random bytes (minimises collisions).
genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange =
    AssetId
        <$> genTokenPolicyIdLargeRange
        <*> genAssetNameLargeRange

{- | Generate a 'TokenPolicyId' from a predefined set of 16 test values
(\"000...0\" through \"FFF...F\"), sized by the QuickCheck size parameter.
-}
genTokenPolicyId :: Gen TokenPolicyId
genTokenPolicyId =
    sized $ \n -> elements $ take (max 1 n) testTokenPolicyIds

-- | Generate a random 28-byte 'TokenPolicyId' (large range).
genTokenPolicyIdLargeRange :: Gen TokenPolicyId
genTokenPolicyIdLargeRange =
    UnsafeTokenPolicyId . Hash . BS.pack <$> vectorOf 28 arbitrary

{- | Generate an 'AssetName' from a predefined set of 26 test values
(\"AssetA\" through \"AssetZ\"), sized by the QuickCheck size parameter.
-}
genAssetName :: Gen AssetName
genAssetName =
    sized $ \n -> elements $ take (max 1 n) testAssetNames

-- | Generate a random 32-byte 'AssetName' (large range).
genAssetNameLargeRange :: Gen AssetName
genAssetNameLargeRange =
    UnsafeAssetName . BS.pack <$> vectorOf 32 arbitrary

-- | Generate a 'TokenQuantity', scaled by the QuickCheck size parameter.
genTokenQuantity :: Gen TokenQuantity
genTokenQuantity =
    sized $ \n -> TokenQuantity . fromIntegral <$> choose @Int (0, n)

--------------------------------------------------------------------------------
-- Test values (matching cardano-wallet-primitive)
--------------------------------------------------------------------------------

{- | 16 predefined token policy IDs, each a 28-byte hash where every byte
is the same hex-digit value. Matches @testTokenPolicyIds@ from
@cardano-wallet-primitive@.
-}
testTokenPolicyIds :: [TokenPolicyId]
testTokenPolicyIds = mkTokenPolicyId <$> [0x00 .. 0x0F]

-- | Create a test 'TokenPolicyId' by repeating a single byte 28 times.
mkTokenPolicyId :: Word8 -> TokenPolicyId
mkTokenPolicyId b = UnsafeTokenPolicyId $ Hash $ BS.replicate 28 b

{- | 26 predefined asset names: \"AssetA\" through \"AssetZ\".
Matches @testAssetNames@ from @cardano-wallet-primitive@.
-}
testAssetNames :: [AssetName]
testAssetNames = mkAssetName <$> ['A' .. 'Z']

-- | Create a test 'AssetName' by appending a character to \"Asset\".
mkAssetName :: Char -> AssetName
mkAssetName = UnsafeAssetName . B8.snoc "Asset"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Shrink a 'Natural' value.
shrinkNatural :: Natural -> [Natural]
shrinkNatural 0 = []
shrinkNatural n =
    0 : [n - i | i <- takeWhile (< n) (iterate (* 2) 1)]

-- | 'Natural'-typed power function. Avoids type ambiguity with Prelude '^'.
power :: Natural -> Natural -> Natural
power = (^)

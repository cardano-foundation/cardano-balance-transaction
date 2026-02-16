{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Balance.Tx.Balance.TokenBundleSizeSpec where

import Cardano.Balance.Tx.Balance.CoinSelection
    ( toCSTokenBundle
    )
import Cardano.Balance.Tx.Balance.TokenBundleSize
    ( computeTokenBundleSerializedLengthBytes
    , mkTokenBundleSizeAssessor
    )
import Cardano.Balance.Tx.Eras
    ( Babbage
    , InAnyRecentEra (..)
    , IsRecentEra (..)
    , RecentEra (..)
    )
import Cardano.Balance.Tx.Tx
    ( ProtVer (..)
    , Version
    )
import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    )
import Cardano.Ledger.Api.Era
    ( eraProtVerLow
    )
import Cardano.Ledger.Api.PParams
    ( PParams
    , ppMaxValSizeL
    , ppProtocolVersionL
    )
import Control.Lens
    ( (&)
    , (.~)
    )
import Data.Default
    ( def
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Word
    ( Word32
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , conjoin
    , counterexample
    , oneof
    , property
    , resize
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Prelude

import qualified Cardano.Balance.Tx.Primitive as W
import qualified Cardano.Balance.Tx.Primitive.Gen as W

spec :: Spec
spec = describe "Assessing the sizes of token bundles" $ do
    it "prop_assessTokenBundleSize_enlarge" $
        property prop_assessTokenBundleSize_enlarge
    it "prop_assessTokenBundleSize_shrink" $
        property prop_assessTokenBundleSize_shrink
    it "unit_assessTokenBundleSize_fixedSizeBundle_32" $
        property unit_assessTokenBundleSize_fixedSizeBundle_32
    it "unit_assessTokenBundleSize_fixedSizeBundle_48" $
        property unit_assessTokenBundleSize_fixedSizeBundle_48
    it "unit_assessTokenBundleSize_fixedSizeBundle_64" $
        property unit_assessTokenBundleSize_fixedSizeBundle_64
    it "unit_assessTokenBundleSize_fixedSizeBundle_128" $
        property unit_assessTokenBundleSize_fixedSizeBundle_128

--------------------------------------------------------------------------------
-- Assessing the sizes of token bundles
--------------------------------------------------------------------------------

prop_assessTokenBundleSize_enlarge
    :: Blind (VariableSize1024 W.TokenBundle)
    -> Blind (VariableSize16 W.TokenBundle)
    -> PParamsInRecentEra
    -> Property
prop_assessTokenBundleSize_enlarge b1' b2' pp =
    assess b1 == TokenBundleSizeExceedsLimit ==>
        conjoin
            [ assess (b1 <> b2)
                === TokenBundleSizeExceedsLimit
            , assess (W.setCoin b1 W.txOutMaxCoin)
                === TokenBundleSizeExceedsLimit
            ]
  where
    assess = assessWalletTokenBundleSize $ mkAssessorFromPParamsInRecentEra pp
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

prop_assessTokenBundleSize_shrink
    :: Blind (VariableSize1024 W.TokenBundle)
    -> Blind (VariableSize16 W.TokenBundle)
    -> PParamsInRecentEra
    -> Property
prop_assessTokenBundleSize_shrink b1' b2' pp =
    assess b1 == TokenBundleSizeWithinLimit ==>
        conjoin
            [ assess (b1 <\> b2)
                === TokenBundleSizeWithinLimit
            , assess (W.setCoin b1 W.txOutMinCoin)
                === TokenBundleSizeWithinLimit
            ]
  where
    assess = assessWalletTokenBundleSize $ mkAssessorFromPParamsInRecentEra pp
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

unit_assessTokenBundleSize_fixedSizeBundle
    :: W.TokenBundle
    -> TokenBundleSizeAssessment
    -> TokenBundleSizeAssessor
    -> W.TxSize
    -> W.TxSize
    -> Property
unit_assessTokenBundleSize_fixedSizeBundle
    bundle
    expectedAssessment
    assessor
    expectedMinLengthBytes
    expectedMaxLengthBytes =
        withMaxSuccess 100 $
            counterexample counterexampleText $
                conjoin . fmap property $
                    [ actualAssessment == expectedAssessment
                    , actualLengthBytes >= expectedMinLengthBytes
                    , actualLengthBytes <= expectedMaxLengthBytes
                    ]
      where
        actualAssessment = assessWalletTokenBundleSize assessor bundle
        v = eraProtVerLow @Babbage
        actualLengthBytes = computeTokenBundleSerializedLengthBytes bundle v
        counterexampleText =
            unlines
                [ "Expected min length bytes:"
                , show expectedMinLengthBytes
                , "Expected max length bytes:"
                , show expectedMaxLengthBytes
                , "Actual length bytes:"
                , show actualLengthBytes
                , "Expected assessment:"
                , show expectedAssessment
                , "Actual assessment:"
                , show actualAssessment
                ]

unit_assessTokenBundleSize_fixedSizeBundle_32
    :: Blind (FixedSize32 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_32 (Blind (FixedSize32 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle
        b
        TokenBundleSizeWithinLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 2116)
        (W.TxSize 2380)

unit_assessTokenBundleSize_fixedSizeBundle_48
    :: Blind (FixedSize48 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_48 (Blind (FixedSize48 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle
        b
        TokenBundleSizeWithinLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 3172)
        (W.TxSize 3564)

unit_assessTokenBundleSize_fixedSizeBundle_64
    :: Blind (FixedSize64 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_64 (Blind (FixedSize64 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle
        b
        TokenBundleSizeExceedsLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 4228)
        (W.TxSize 4748)

unit_assessTokenBundleSize_fixedSizeBundle_128
    :: Blind (FixedSize128 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_128 (Blind (FixedSize128 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle
        b
        TokenBundleSizeExceedsLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 8452)
        (W.TxSize 9484)

instance Arbitrary W.TokenBundle where
    arbitrary = W.genTokenBundleSmallRange
    shrink = W.shrinkTokenBundleSmallRange

newtype FixedSize32 a = FixedSize32 {unFixedSize32 :: a}
    deriving (Eq, Show)

newtype FixedSize48 a = FixedSize48 {unFixedSize48 :: a}
    deriving (Eq, Show)

newtype FixedSize64 a = FixedSize64 {unFixedSize64 :: a}
    deriving (Eq, Show)

newtype FixedSize128 a = FixedSize128 {unFixedSize128 :: a}
    deriving (Eq, Show)

newtype VariableSize16 a = VariableSize16 {unVariableSize16 :: a}
    deriving (Eq, Show)

newtype VariableSize1024 a = VariableSize1024 {unVariableSize1024 :: a}
    deriving (Eq, Show)

instance Arbitrary (FixedSize32 W.TokenBundle) where
    arbitrary = FixedSize32 <$> W.genTxOutTokenBundle 32

instance Arbitrary (FixedSize48 W.TokenBundle) where
    arbitrary = FixedSize48 <$> W.genTxOutTokenBundle 48

instance Arbitrary (FixedSize64 W.TokenBundle) where
    arbitrary = FixedSize64 <$> W.genTxOutTokenBundle 64

instance Arbitrary (FixedSize128 W.TokenBundle) where
    arbitrary = FixedSize128 <$> W.genTxOutTokenBundle 128

instance Arbitrary (VariableSize16 W.TokenBundle) where
    arbitrary = VariableSize16 <$> resize 16 W.genTokenBundle

instance Arbitrary (VariableSize1024 W.TokenBundle) where
    arbitrary = VariableSize1024 <$> resize 1024 W.genTokenBundle

instance Arbitrary Version where
    arbitrary = arbitraryBoundedEnum

type PParamsInRecentEra = InAnyRecentEra PParams

instance Arbitrary PParamsInRecentEra where
    arbitrary =
        oneof
            [ InBabbage <$> genPParams RecentEraBabbage
            , InConway <$> genPParams RecentEraConway
            ]
      where
        genPParams
            :: (IsRecentEra era)
            => RecentEra era
            -> Gen (PParams era)
        genPParams _era = do
            ver <- arbitrary
            maxSize <- genMaxSizeBytes
            return $
                def
                    & ppProtocolVersionL .~ (ProtVer ver 0)
                    & ppMaxValSizeL .~ maxSize
          where
            genMaxSizeBytes :: Gen Natural
            genMaxSizeBytes =
                oneof
                    [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int
                    , fromIntegral <$> arbitrary @Word32
                    ]

babbageTokenBundleSizeAssessor :: TokenBundleSizeAssessor
babbageTokenBundleSizeAssessor =
    mkTokenBundleSizeAssessor $
        (def :: PParams Babbage)
            & ppProtocolVersionL .~ (ProtVer (eraProtVerLow @Babbage) 0)
            & ppMaxValSizeL .~ maryTokenBundleMaxSizeBytes
  where
    maryTokenBundleMaxSizeBytes = 4000

mkAssessorFromPParamsInRecentEra
    :: PParamsInRecentEra
    -> TokenBundleSizeAssessor
mkAssessorFromPParamsInRecentEra (InBabbage pp) =
    mkTokenBundleSizeAssessor pp
mkAssessorFromPParamsInRecentEra (InConway pp) =
    mkTokenBundleSizeAssessor pp

assessWalletTokenBundleSize
    :: TokenBundleSizeAssessor
    -> W.TokenBundle
    -> TokenBundleSizeAssessment
assessWalletTokenBundleSize a =
    assessTokenBundleSize a . toCSTokenBundle

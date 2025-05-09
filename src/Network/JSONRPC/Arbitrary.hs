{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.JSONRPC.Arbitrary where

import           Data.Aeson
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Network.JSONRPC.Data
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Ver where
    arbitrary = elements [V1, V2]

instance Arbitrary Request where
    arbitrary = oneof
        [ Request <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , Notif <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary Response where
    arbitrary = oneof
        [ Response <$> arbitrary <*> res <*> arbitrary
        , ResponseError <$> arbitrary <*> arbitrary <*> arbitrary
        , OrphanError <$> arbitrary <*> arbitrary
        ]
      where
        res = arbitrary `suchThat` (/= Null)


instance Arbitrary ErrorObj where
    arbitrary = oneof
        [ ErrorObj <$> arbitrary <*> arbitrary <*> arbitrary
        , ErrorVal <$> (arbitrary `suchThat` (/= Null))
        ]

instance Arbitrary BatchRequest where
    arbitrary = oneof
        [ BatchRequest <$> arbitrary
        , SingleRequest <$> arbitrary
        ]

instance Arbitrary BatchResponse where
    arbitrary = oneof
        [ BatchResponse <$> arbitrary
        , SingleResponse <$> arbitrary
        ]

instance Arbitrary Message where
    arbitrary = oneof
        [ MsgRequest  <$> arbitrary
        , MsgResponse <$> arbitrary
        , MsgBatch    <$> batch
        ]
      where
        batch = listOf $ oneof [ MsgRequest  <$> arbitrary
                               , MsgResponse <$> arbitrary
                               ]

instance Arbitrary Id where
    arbitrary = IdInt <$> arbitraryBoundedRandom


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Arbitrary instances and data types for use in test suites.
module Network.JsonRpc.Arbitrary
( -- * Arbitrary Data
  ReqRes(..)
) where

import Control.Applicative
import Data.Aeson.Types hiding (Error)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.JsonRpc.Data
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | A pair of a request and its corresponding response.
data ReqRes q r = ReqRes !(Request q) !(Response r)
    deriving (Show, Eq)

instance Arbitrary (ReqRes Value Value) where
    arbitrary = do
        rq <- arbitrary
        rs <- arbitrary
        let rs' = rs { getResId = getReqId rq }
        return $ ReqRes rq rs'

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Arbitrary q, ToRequest q) => Arbitrary (Request q) where
    arbitrary = do
        q <- arbitrary
        let m = requestMethod q
        oneof [ Request  m q <$> arbitrary
              , Request1 m q <$> arbitrary ]

instance (Arbitrary n, ToNotif n) => Arbitrary (Notif n) where
    arbitrary = do
        n <- arbitrary
        let m = notifMethod n
        oneof $ map return [Notif m n, Notif1 m n]

instance Arbitrary r => Arbitrary (Response r) where
    arbitrary = oneof [ Response1 <$> arbitrary <*> arbitrary
                      , Response  <$> arbitrary <*> arbitrary ]

instance Arbitrary Error where
    arbitrary = oneof [ Error1 <$> arbitrary <*> arbitrary
                      , Error  <$> arbitrary <*> arbitrary ]

instance ( Arbitrary q, Arbitrary n, Arbitrary r
         , ToRequest q, ToNotif n, ToJSON r )
    => Arbitrary (Message q n r)
  where
    arbitrary = oneof [ MsgRequest  <$> arbitrary
                      , MsgNotif    <$> arbitrary
                      , MsgResponse <$> arbitrary
                      , MsgError    <$> arbitrary ]

instance Arbitrary Id where
    arbitrary = oneof [IdInt <$> arbitrary, IdTxt <$> arbitrary]

instance Arbitrary ErrorObj where
    arbitrary = ErrorObj <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Value where
    arbitrary = resize 10 $ oneof [val, lsn, objn] where
        val = oneof [ toJSON <$> (arbitrary :: Gen String)
                    , toJSON <$> (arbitrary :: Gen Int)
                    , toJSON <$> (arbitrary :: Gen Double)
                    , toJSON <$> (arbitrary :: Gen Bool) ]
        ls   = toJSON <$> listOf val
        obj  = toJSON . M.fromList <$> listOf ps
        ps   = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls]
        lsn  = toJSON <$> listOf (oneof [ls, obj, val])
        objn = toJSON . M.fromList <$> listOf psn
        psn  = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls, obj]


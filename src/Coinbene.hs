{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbene where

import Data.Word                    (Word64)
import Data.Char                    (toLower)
import GHC.Generics
import Data.Aeson

-----------------------------------------
class CoinSymbol coin where
  coinSymbol :: coin -> String

-----------------------------------------
-- Units
newtype Vol   a = Vol   a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype Price a = Price a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype Cost  a = Cost  a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

instance (Generic a, FromJSON a) => FromJSON (Price a)
instance (Generic a, FromJSON a) => FromJSON (Vol   a)
instance (Generic a, FromJSON a) => FromJSON (Cost  a)

instance (Generic a, ToJSON   a) => ToJSON   (Price a)
instance (Generic a, ToJSON   a) => ToJSON   (Vol   a)
instance (Generic a, ToJSON   a) => ToJSON   (Cost  a)
-----------------------------------------

newtype MilliEpoch = MilliEpoch Word64 deriving (Show, Eq, Ord, Generic, Num, Real, Enum, Integral)
instance FromJSON MilliEpoch
instance ToJSON   MilliEpoch

data AskQuote p v
  = AskQ
    { aqPrice    :: Price p
    , aqQuantity :: Vol   v
    } deriving (Show, Eq, Generic)

data BidQuote p v
  = BidQ
    { bqPrice    :: Price p
    , bqQuantity :: Vol   v
    } deriving (Show, Eq, Generic)

data QuoteBook p v
  = QuoteBook
    { qbAsks::[AskQuote p v]
    , qbBids::[BidQuote p v]
    } deriving (Show, Eq, Generic)

-- `writeQuoteOpts` removes field label name mangling
writeQuoteOpts = defaultOptions {fieldLabelModifier = map toLower . drop 2}

instance (ToJSON p, Generic p, ToJSON v, Generic v) => ToJSON (AskQuote p v) where
    toJSON = genericToJSON writeQuoteOpts
instance (ToJSON p, Generic p, ToJSON v, Generic v) => ToJSON (BidQuote p v) where
    toJSON = genericToJSON writeQuoteOpts
instance (ToJSON p, Generic p, ToJSON v, Generic v) => ToJSON (QuoteBook p v) where
    toJSON = genericToJSON writeQuoteOpts

instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (AskQuote p v) where
    parseJSON = withObject "AskQuote" $ \v -> AskQ <$> v .: "price" <*> v .: "quantity"
instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (BidQuote p v) where
    parseJSON = withObject "BidQuote" $ \v -> BidQ <$> v .: "price" <*> v .: "quantity"
instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (QuoteBook p v) where
    parseJSON = withObject "QuoteBook" $ \v -> QuoteBook <$> v .: "asks" <*> v .: "bids"



{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbene.Core where

import Data.Word                    (Word64)
import Data.Char                    (toLower)
import GHC.Generics
import Data.Aeson

-----------------------------------------
class (Generic coin, FromJSON coin) => Coin coin where
  coinSymbol :: coin -> String
  showBare   :: coin -> String

-----------------------------------------
-- Units
newtype Vol   a = Vol   a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype Price a = Price a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype Cost  a = Cost  a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

-- For showing the values without the constructor
showBarePrice :: Coin p => Price p -> String
showBareVol   :: Coin p => Vol   p -> String
showBareCost  :: Coin p => Cost  p -> String
showBarePrice (Price p) = showBare p
showBareVol   (Vol   v) = showBare v
showBareCost  (Cost  c) = showBare c

instance (Generic a, FromJSON a) => FromJSON (Price a)
instance (Generic a, FromJSON a) => FromJSON (Vol   a)
instance (Generic a, FromJSON a) => FromJSON (Cost  a)

instance (Generic a, ToJSON   a) => ToJSON   (Price a)
instance (Generic a, ToJSON   a) => ToJSON   (Vol   a)
instance (Generic a, ToJSON   a) => ToJSON   (Cost  a)
-----------------------------------------

newtype MilliEpoch = MilliEpoch Word64 deriving (Show, Eq, Ord, Generic, Num, Real, Enum, Integral)

showBareMilliEpoch :: MilliEpoch -> String
showBareMilliEpoch (MilliEpoch w) = show w

instance FromJSON MilliEpoch
instance ToJSON   MilliEpoch

newtype OrderID = OrderID String deriving (Show, Eq, Ord, Generic)
instance FromJSON OrderID
instance ToJSON   OrderID

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


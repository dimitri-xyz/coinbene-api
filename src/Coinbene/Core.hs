{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbene.Core where

import Data.Word                    (Word64)
import Data.Char                    (toLower)
import GHC.Generics
import Data.Aeson
import Data.Proxy

import Data.Scientific

-----------------------------------------
class (Generic coin, FromJSON coin) => Coin coin where
  coinSymbol :: Proxy coin -> String
  showBare   :: coin -> String
  readBare   :: String -> coin

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

instance FromJSON a => FromJSON (Price a)
instance FromJSON a => FromJSON (Vol   a)
instance FromJSON a => FromJSON (Cost  a)

-----------------------------------------
newtype MilliEpoch = MilliEpoch Word64 deriving (Show, Eq, Ord, Generic, Num, Real, Enum, Integral, FromJSON)

showBareMilliEpoch :: MilliEpoch -> String
showBareMilliEpoch (MilliEpoch w) = show w

newtype OrderID  = OrderID String deriving (Show, Eq, Ord, Generic, FromJSON)
data OrderSide   = Bid | Ask deriving (Show, Eq)
data OrderStatus = Filled | Unfilled | PartiallyFilled | Canceled | PartiallyCanceled deriving (Show, Eq)

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

instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (AskQuote p v) where
    parseJSON = withObject "AskQuote" $ \v -> AskQ <$> v .: "price" <*> v .: "quantity"
instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (BidQuote p v) where
    parseJSON = withObject "BidQuote" $ \v -> BidQ <$> v .: "price" <*> v .: "quantity"
instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (QuoteBook p v) where
    parseJSON = withObject "QuoteBook" $ \v -> QuoteBook <$> v .: "asks" <*> v .: "bids"

-----------------------------------------
data OrderInfo =
    LimitOrder
    { market     :: String
    , oSide      :: OrderSide
    , limitPrice :: Price Scientific
    , limitVol   :: Vol   Scientific
    , orderID    :: OrderID
    , created      :: MilliEpoch
    , mModified    :: Maybe MilliEpoch
    , status       :: OrderStatus
    , filledVol    :: Vol  Scientific
    , filledAmount :: Cost Scientific
    , mAvePriceAndFees :: Maybe (Price Scientific, Cost Scientific) -- (average price, fees), nothing means "don't know"
    }
    deriving (Show, Eq)

instance FromJSON OrderInfo where
    parseJSON = withObject "OrderInfo" $ \v -> 
        LimitOrder
            <$> v .: "symbol" 
            <*> do  oSide <- v .: "type"
                    return $ case (oSide :: String) of
                        "buy-limit"  -> Bid 
                        "sell-limit" -> Ask
            <*> fmap (Price . read) (v .: "price")
            <*> fmap (Vol   . read) (v .: "orderquantity")
            <*> v .: "orderid"
            <*> v .: "createtime"
            <*> v .:? "lastmodified"
            <*> do  oStatus <- v .: "orderstatus"
                    return $ case (oStatus :: String) of
                        "filled"          -> Filled
                        "unfilled"        -> Unfilled
                        "canceled"        -> Canceled
                        "partialFilled"   -> PartiallyFilled
                        "partialCanceled" -> PartiallyCanceled
            <*> fmap (Vol  . read) (v .: "filledquantity")
            <*> fmap (Cost . read) (v .: "filledamount")
            <*> do
                mAvePrice <- v .:? "averageprice"
                mFees     <- v .:? "fees"
                return $ case (mAvePrice, mFees) of 
                    (Just avePrice, Just fees) -> Just (avePrice, fees)
                    _ -> Nothing


-----------------------------------------
data BalanceInfo =
    BalanceInfo
        { biAsset     :: String
        , biAvailable :: Cost Scientific
        , biReserved  :: Cost Scientific
        , biTotal     :: Cost Scientific
        }
        deriving (Show)

instance FromJSON BalanceInfo where
    parseJSON = withObject "BalanceInfo" $ \v -> 
        BalanceInfo
            <$> v .: "asset"
            <*> fmap (Cost . read) (v .: "available")
            <*> fmap (Cost . read) (v .: "reserved")
            <*> fmap (Cost . read) (v .: "total")

-----------------------------------------
data Trade p v =
    Trade
        { tTradeID :: Scientific
        , tPrice   :: Price p
        , tVol     :: Vol   v
        , tTakerSide :: OrderSide
        , tTime    :: MilliEpoch 
        }
        deriving Show

instance (Coin p, Coin v) => FromJSON (Trade p v) where
    parseJSON = withObject "Trade" $ \v -> 
        Trade 
            <$> fmap read (v .: "tradeId")
            <*> fmap (Price . readBare) (v .: "price")
            <*> fmap (Vol   . readBare) (v .: "quantity")
            <*> do
                    taker <- v .: "take"
                    case taker of
                        "buy"  -> pure Bid
                        "sell" -> pure Ask
                        _      -> fail ("parseJSON - Unknown taker side when parsing trade: " ++ taker)
            <*> v .: "time"
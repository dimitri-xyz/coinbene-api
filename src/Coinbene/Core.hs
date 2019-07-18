{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbene.Core where

import Data.Word                    (Word64)
import Data.Char                    (toLower)
import GHC.Generics
import Data.Aeson
import Data.Proxy

import Data.Scientific              hiding (Fixed)
import Data.Time                    (UTCTime)
import Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)


import Control.Exception

data ExchangeError
    = ExchangeError Int String
    | JSONDecodingError String
    deriving Show

genericError, didNotCompleteError :: Int
genericError        = 1
didNotCompleteError = 10310

instance Exception ExchangeError

-----------------------------------------
class (Generic coin, FromJSON coin, Show coin, Num coin) => Coin coin where
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

utcTimeToMilliEpoch :: UTCTime -> MilliEpoch
utcTimeToMilliEpoch = MilliEpoch . truncate . (1000*) . realToFrac . utcTimeToPOSIXSeconds
-- NO sub-second precision (because of `utcTimeToPOSIXSeconds`)

showBareMilliEpoch :: MilliEpoch -> String
showBareMilliEpoch (MilliEpoch w) = show w

newtype OrderID  = OrderID String deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)
data OrderSide   = Bid | Ask deriving (Show, Eq)
data OrderStatus = Filled | Unfilled | PartiallyFilled | Canceled | PartiallyCanceled deriving (Show, Eq)

----------------------------------------
-- Futures Market
newtype Leverage = Leverage Int deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Direction   = OpenLong | CloseLong | OpenShort | CloseShort deriving (Show, Eq)

instance FromJSON Direction where
    parseJSON v =
        case v of
            (String "openLong")   -> pure OpenLong
            (String "closeLong")  -> pure CloseLong
            (String "openShort")  -> pure OpenShort
            (String "closeShort") -> pure CloseShort
            _ -> fail ("Unknown order direction: " <> show v)

instance ToJSON Direction where
    toJSON direction =
        case direction of
            OpenLong   -> String "openLong"
            CloseLong  -> String "closeLong"
            OpenShort  -> String "openShort"
            CloseShort -> String "closeShort"

data MarginMode  = Fixed | Crossed deriving (Show, Eq, Generic)

instance FromJSON MarginMode where
    parseJSON v =
        case v of
            (String "fixed")   -> pure Fixed
            (String "crossed") -> pure Crossed
            _ -> fail ("Unknown margin mode: " <> show v)

instance ToJSON MarginMode where
    toJSON marginMode =
        case marginMode of
            Fixed   -> String "fixed"
            Crossed -> String "crossed"

data OrderType = Market | Limit deriving (Show, Eq, Generic)

instance FromJSON OrderType where
    parseJSON v =
        case v of
            (String "limit")  -> pure Limit
            (String "market") -> pure Market
            _ -> fail ("Unknown futures order type: " <> show v)

instance ToJSON OrderType where
    toJSON orderType =
        case orderType of
            Limit  -> String "limit"
            Market -> String "market"

data FuturesStatus = StatusNew | StatusFilled | StatusCanceled | StatusPartiallyFilled deriving (Show, Eq, Generic)

instance FromJSON FuturesStatus where
    parseJSON v =
        case v of
            (String "new")             -> pure StatusNew
            (String "filled")          -> pure StatusFilled
            (String "canceled")        -> pure StatusCanceled
            (String "partiallyFilled") -> pure StatusPartiallyFilled
            _ -> fail ("Unknown futures order status: " <> show v)

instance ToJSON FuturesStatus where
    toJSON direction =
        case direction of
            StatusNew             -> String "new"
            StatusFilled          -> String "filled"
            StatusCanceled        -> String "canceled"
            StatusPartiallyFilled -> String "partiallyFilled"

----------------------------------------
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

-----------------------------------------
data FuturesAccInfo =
    FuturesAccInfo
    { availBal   :: Cost Scientific
    , frozenBal  :: Cost Scientific
    , marginBal  :: Cost Scientific
    , marginRate ::      Scientific
    , totalBal   :: Cost Scientific
    , unrealPNL  :: Cost Scientific
    } deriving (Show, Eq)

instance FromJSON FuturesAccInfo where
    parseJSON = withObject "FuturesAccInfo" $ \v ->
        FuturesAccInfo
            <$> fmap (Cost . read) (v .: "availableBalance")
            <*> fmap (Cost . read) (v .: "frozenBalance"   )
            <*> fmap (Cost . read) (v .: "marginBalance"   )
            <*> fmap (       read) (v .: "marginRate"      )
            <*> fmap (Cost . read) (v .: "balance"         )
            <*> fmap (Cost . read) (v .: "unrealisedPnl"   )
-----------------------------------------

-----------------------------------------
data FuturesOrderInfo =
    FuturesLimitOrder
    { foiOrderID    :: OrderID
    , foiDirection :: Direction
    , foiLeverage :: Leverage
    , foiMarket     :: String
    , foiLimitPrice :: Price Scientific
    , foiLimitVol   :: Vol   Scientific
    , foiLimitValue :: Cost  Scientific
    , foiFees         :: Cost Scientific
    , foiFilledVol    :: Vol  Scientific
    , foiAvePrice     :: Price Scientific
    , foiCreated      :: UTCTime
    , foiStatus       :: FuturesStatus
    } deriving (Show, Eq)

-- Can only Limit order for now
instance FromJSON FuturesOrderInfo where
    parseJSON = withObject "FuturesOrderInfo" $ \v -> do
        orderType <- v .: "orderType"
        case orderType of
            Market -> fail "NOT IMPLEMENTED -  Can only parse Limit orders in a `FuturesOrderInfo`"
            Limit  -> FuturesLimitOrder
                <$> v .: "orderId"
                <*> v .: "direction"
                <*> fmap (Leverage . read) (v .: "leverage")
                <*> v .: "symbol"
                <*> fmap (Price . read) (v .: "orderPrice")
                <*> fmap (Vol   . read) (v .: "quantity")
                <*> fmap (Cost  . read) (v .: "orderValue")
                <*> fmap (Cost  . read) (v .: "fee")
                <*> fmap (Vol   . read) (v .: "filledQuantity")
                <*> fmap (Price . read) (v .: "averagePrice")
                <*> v .: "orderTime"
                <*> v .: "status"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene.Parse where

import GHC.Generics
import Data.Proxy
import Data.Char                    (toLower)
import Data.Aeson
import Data.Aeson.Types             (Object, Parser)
import Data.Vector                  (toList)

import Coinbene.Core

{---------------------------------------

NOTE: Parsing Unwrapped top-level JSON objects

There is a significant difference between CoinBene's responses given to calls
in the spot market API vs. the contract (futures) API. The difference is in
the structure of the JSON objects returned.

For example, a spot market call to
"POST v1/trade/balance"
could return:

{
    "account":"exchange",
    "balance":[
        {
            "asset":"ACT",
            "available":"999999.0000000000000000",
            "reserved":"0.0000000000000000",
            "total":"999999.0000000000000000"
        },
        {
            "asset":"AE",
            "available":"999999.0000000000000000",
            "reserved":"0.0000000000000000",
            "total":"999999.0000000000000000"
        }
        ...
    ],
    "status":"ok",
    "timestamp":1517536673213
}

Notice that there are 2 "fields" on the top level object: "account" and "balance" that
only exist on this response. These may or many not be present depending on which API call was made.
Contrast this with a response from the new futures API:
POST/api/swap/v2/order/place

{
    "code":200,
    "message":null,
    "data":{
        "availableBalance":"0.0789",
        "frozenBalance":"0.0000",
        "marginBalance":"0.0790",
        "marginRate":"0.0005",
        "balance":"0.0789",
        "unrealisedPnl":"0.0000"
    }
}

In this case, all the fields that are not present on *every* succesful response are always "wrapped" inside the
"data" object. This makes parsing much easier. To parse the response to "POST v1/trade/balance" above,
the parser has to do some magic when parsing the top-level JSON object to know that the
"status" and "timestamp" fields are for parsing a `Resp a` data type, but that the fields
"account" and "balance" are for a `BalancesPayload` type in `Resp BalancesPayload`. In the JSON format,
the `Resp` doesn't completely wrap the `BalancesPayload` type, but in the haskell representation, it does.

Because of this quirk, we use the FromUnwrappedToplevelJSON type class. When parsing the JSON, the FromJSON type
class assumes the current object will be completely parsed and expects to see another pair of {} in a field
of the current object to parse a different type.

# The Hack

This doesn't work for CoinBene's spot API, there the parser would have to pass the "current" top-level object it
received (or we would be forced to see each top-level response as a different object) down the "haskell
typeclass parsing chain". The parsers for each type must ignore fields they don't care about and fields with
similar names may accept malformed requests that should be rejected.

So, **this solution introduces bugs**, but allows us to use the standard FromJSON parsing machinery.

To do that, we just wrap all spot market response objects in a "payload object" that describes what extra
fields will be available at the top-level JSON object and then fall back to "standard FromJSON parsing".

-}

-----------------------------------------
marketSymbol :: forall p v. (Coin p, Coin v) => Proxy (Price p) -> Proxy (Vol v) -> String
marketSymbol p v = coinSymbol (Proxy :: Proxy v) ++ coinSymbol (Proxy :: Proxy p)

data Resp payload
  = RespOK
    { rPayload :: payload
    , rTimestamp :: MilliEpoch
    }
  | RespError
    { rDescription :: String
    , rTimestamp :: MilliEpoch
    }
    deriving (Show, Eq, Generic)

data QuoteBookPayload p v
  = BookPayload
    { bpOrderbook :: QuoteBook p v
    , bpSymbol :: String
    } deriving (Show, Eq, Generic)

data OpenOrdersPayload
  = OpenOrdersPayload
    { ooOrders   :: [OrderInfo]
    , ooCount    :: Int
    , ooPageSize :: Int
    , ooPage     :: Int
    } deriving Show

newtype OIDPayload       = OIDPayload         OrderID deriving (Show, Eq, Generic)
newtype OrderInfoPayload = OrderInfoPayload OrderInfo deriving (Show, Generic)

-----------------------------------------
-- This is like FromJSON but `parseUnwrappedJSON` receives an Object not a Value
-- allows parsing of "unwrapped fields" at the top-level JSON object
-- See NOTE: Parsing Unwrapped top-level JSON objects
class FromUnwrappedToplevelJSON a where
    parseUnwrappedJSON :: Object -> Parser a

instance FromUnwrappedToplevelJSON payload => FromJSON (Resp payload) where
    parseJSON (Object h) = do
        status <- h .: "status"
        case (status :: String) of
            "ok" -> RespOK
                <$> parseUnwrappedJSON h
                <*> h .: "timestamp"
            "error" -> RespError
                <$> h .: "description"
                <*> h .: "timestamp"
            _ -> fail ("parseJSON - Unknown response status code:" ++ status)


-----------------------------------------
instance (Coin p, Coin v) => FromUnwrappedToplevelJSON (QuoteBookPayload p v) where
    parseUnwrappedJSON = parseQuoteBookPayload

parseQuoteBookPayload :: forall p v. (Coin p, Coin v) => Object -> Parser (QuoteBookPayload p v)
parseQuoteBookPayload h = do
    mBook   <- h .:? "orderbook"
    mSymbol <- h .:? "symbol"
    case (mBook, mSymbol) of
        (Just b, Just s) -> let expectedMarket = marketSymbol  (Proxy :: Proxy (Price p)) (Proxy :: Proxy (Vol v))
                             in if s == expectedMarket
                                    then return (BookPayload b s)
                                    else fail $ "parseQuoteBookPayload obtained data for: `" ++ s ++
                                                "` market, but asked for " ++ expectedMarket
        _ -> fail "parseQuoteBookPayload was unable to parse orderbook and symbol in response"

instance FromUnwrappedToplevelJSON OIDPayload where
    parseUnwrappedJSON h = fmap OIDPayload (h .: "orderid")

instance FromUnwrappedToplevelJSON OrderInfoPayload where
    parseUnwrappedJSON h = fmap OrderInfoPayload (h .: "order")

-----------------------------------------
instance FromUnwrappedToplevelJSON OpenOrdersPayload where
    parseUnwrappedJSON = parseOpenOrdersPayload

parseOpenOrdersPayload :: Object -> Parser OpenOrdersPayload
parseOpenOrdersPayload h' = do
    orders' <- h' .: "orders"
    case orders' of
        Null       -> return (OpenOrdersPayload [] 0 1 1)
        (Object h) ->
            OpenOrdersPayload
                <$> do
                    orders <- (h .: "result")
                    toList <$> withArray "OrderInfo" (mapM parseJSON) orders
                <*> h .: "totalcount"
                <*> h .: "pagesize"
                <*> h .: "page"

        _ -> fail "parseOpenOrdersPayload was unable to find a matching \"orders\" field in response"

-----------------------------------------
data BalancesPayload
  = BalancesPayload
    { bAccount   :: String
    , bBalances  :: [BalanceInfo]
    }

instance FromUnwrappedToplevelJSON BalancesPayload where
    parseUnwrappedJSON h =
        BalancesPayload
            <$> h .: "account"
            <*> h .: "balance"

-----------------------------------------
data TradesPayload p v
  = TradesPayload
  { tTrades :: [Trade p v]
  , tMarket :: String
  }

instance (Coin p, Coin v) => FromUnwrappedToplevelJSON (TradesPayload p v) where
    parseUnwrappedJSON = parseTradesPayload

parseTradesPayload :: forall p v. (Coin p, Coin v) => Object -> Parser (TradesPayload p v)
parseTradesPayload h = do
    mTrades <- h .:? "trades"
    mSymbol <- h .:? "symbol"
    case (mTrades, mSymbol) of
        (Just ts, Just s) -> let expectedMarket = marketSymbol  (Proxy :: Proxy (Price p)) (Proxy :: Proxy (Vol v))
                              in if s /= expectedMarket
                                    then fail $ "parseTradesPayload obtained data for: `" ++ s ++
                                                "` market, but asked for " ++ expectedMarket
                                    else return (TradesPayload ts s)
        _ -> fail "parseTradesPayload was unable to parse trades and symbol in response"


-----------------------------------------
data FuturesResp payload
  = FuturesResp
    { rCode    :: Int
    , rMessage :: Maybe String
    , rData    :: Maybe payload
    }
    deriving (Show, Eq, Generic)

instance FromJSON payload => FromJSON (FuturesResp payload) where
    parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = map toLower . drop 1 }

data FuturesPlaceOrderPayload =
    FuturesPlaceOrderPayload
    { orderId  :: OrderID
    , clientId :: Maybe String
    } deriving (Show, Eq, Generic)

instance FromJSON FuturesPlaceOrderPayload

data FuturesCancelOrderPayload =
    FuturesCancelOrderPayload
    { cancelOrderId  :: OrderID
    } deriving (Show, Eq, Generic)

instance FromJSON FuturesCancelOrderPayload where
    parseJSON v = FuturesCancelOrderPayload <$> parseJSON v

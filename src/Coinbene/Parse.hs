{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene.Parse where

import GHC.Generics
import Data.Proxy
import Data.Aeson
import Data.Aeson.Types             (Object, Parser)
import Data.Vector                  (toList) 

import Coinbene.Core

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
-- this is like FromJSON but `parsePayload` receives an Object not a Value
-- allows parsing of unwrapped fields at the top-level JSON object
class ParsePayload a where
    parsePayload :: Object -> Parser a

instance ParsePayload payload => FromJSON (Resp payload) where
    parseJSON (Object h) = do
        status <- h .: "status"
        case (status :: String) of
            "ok" -> RespOK
                <$> parsePayload h
                <*> h .: "timestamp"
            "error" -> RespError
                <$> h .: "description"
                <*> h .: "timestamp"
            _ -> fail ("parseJSON - Unknown response status code:" ++ status)


-----------------------------------------
instance (Coin p, Coin v) => ParsePayload (QuoteBookPayload p v) where
    parsePayload = parseQuoteBookPayload

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

instance ParsePayload OIDPayload where
    parsePayload h = fmap OIDPayload (h .: "orderid")

instance ParsePayload OrderInfoPayload where
    parsePayload h = fmap OrderInfoPayload (h .: "order")

-----------------------------------------
instance ParsePayload OpenOrdersPayload where
    parsePayload = parseOpenOrdersPayload

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

instance ParsePayload BalancesPayload where
    parsePayload h = 
        BalancesPayload
            <$> h .: "account"
            <*> h .: "balance"

-----------------------------------------
data TradesPayload p v
  = TradesPayload
  { tTrades :: [Trade p v]
  , tMarket :: String
  }

instance (Coin p, Coin v) => ParsePayload (TradesPayload p v) where
    parsePayload = parseTradesPayload

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


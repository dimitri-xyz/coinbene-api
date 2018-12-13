{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene.Parse where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types             (Object, Parser)

import Coinbene.Core

-----------------------------------------
data Resp tail p v
  = RespOK
    { rPayload :: tail p v
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

data NoPayload a b = NoPayload deriving (Show, Eq, Generic)

data OIDPayload = OIDPayload OrderID deriving (Show, Eq, Generic)

class ParsePayload a where
    parsePayload :: Object -> Parser a
-----------------------------------------
instance ParsePayload (NoPayload a b) where
    parsePayload _ = pure NoPayload 

instance ( FromJSON p, FromJSON v, Generic p, Generic v, Coin p, Coin v) => ParsePayload (QuoteBookPayload p v) where
    parsePayload = parseQuoteBookPayload

instance (FromJSON p, FromJSON v, Generic p, Generic v, Coin p, Coin v, ParsePayload (payload p v)) 
    => FromJSON (Resp payload p v) where
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

-- parseResponse :: (FromJSON p, FromJSON v, Generic p, Generic v, Coin p, Coin v, ParsePayload (QuoteBookPayload p v)) 
--     => FromJSON (Resp QuoteBookPayload p v) where
--     parseJSON (Object h) = do
--         status <- h .: "status"
--         case (status :: String) of
--             "ok" -> RespOK
--                 <$> parsePayload h
--                 <*> h .: "timestamp"
--             "error" -> RespError
--                 <$> h .: "description"
--                 <*> h .: "timestamp"
--             _ -> fail ("parseJSON - Unknown response status code:" ++ status)





parseQuoteBookPayload :: forall p v.
        ( FromJSON   p
        , Generic    p
        , Coin       p
        , FromJSON   v
        , Generic    v
        , Coin       v
        ) => Object -> Parser (QuoteBookPayload p v)
parseQuoteBookPayload h = do
    mBook   <- h .:? "orderbook"
    mSymbol <- h .:? "symbol"
    case (mBook, mSymbol) of
        (Just b, Just s) -> let expectedMarket = coinSymbol (undefined :: v) ++ coinSymbol (undefined :: p) 
                             in if s == expectedMarket
                                    then return (BookPayload b s)
                                    else fail $ "parseQuoteBookPayload obtained data for: `" ++ s ++ 
                                                "` market, but asked for " ++ expectedMarket
        _ -> fail "parseQuoteBookPayload was unable to parse orderbook and symbol in response" 


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

newtype OIDPayload = OIDPayload OrderID deriving (Show, Eq, Generic)

-----------------------------------------
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
instance ( FromJSON p, FromJSON v, Generic p, Generic v, Coin p, Coin v) => ParsePayload (QuoteBookPayload p v) where
    parsePayload = parseQuoteBookPayload

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

instance ParsePayload OIDPayload where
    parsePayload h = fmap OIDPayload (h .: "orderid")

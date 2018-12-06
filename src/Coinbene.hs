{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene where

import Network.HTTP.Client
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Network.HTTP.Types.Status    (statusCode)
import Data.ByteString.Lazy.Char8   (ByteString)

import Data.Word                    (Word64)
import Data.Char                    (toLower)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types             (Object, Parser)

import Coins

-----------------------------------------
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

-----------------------------------------
class ParsePayload a where
    parsePayload :: Object -> Parser a


instance ParsePayload (NoPayload a b) where
    parsePayload _ = pure NoPayload 

instance ( FromJSON   p
         , Generic    p
         , CoinSymbol p
         , FromJSON   v
         , Generic    v
         , CoinSymbol v
         ) => ParsePayload (QuoteBookPayload p v) where
    parsePayload = parseQuoteBookPayload

instance (FromJSON    p
         , Generic    p
         , CoinSymbol p
         , FromJSON   v
         , Generic    v
         , CoinSymbol v
         , ParsePayload (QuoteBookPayload p v)) 
         => FromJSON (Resp QuoteBookPayload p v) where
    parseJSON (Object h) = do
        status <- h .: "status"
        case (status :: String) of
            "ok" -> RespOK
                <$> parsePayload h
                <*> h .: "timestamp"
            "error" -> RespError
                <$> h .: "description"
                <*> h .: "timestamp"
            _ -> fail ("Unknown response status code: " ++ status)


parseQuoteBookPayload :: forall p v.
        ( FromJSON   p
        , Generic    p
        , CoinSymbol p
        , FromJSON   v
        , Generic    v
        , CoinSymbol v
        ) => Object -> Parser (QuoteBookPayload p v)
parseQuoteBookPayload h = do
    mBook   <- h .:? "orderbook"
    mSymbol <- h .:? "symbol"
    case (mBook, mSymbol) of
        (Just b, Just s) -> if s == (coinSymbol (undefined :: v) ++ coinSymbol (undefined :: p) )
                                then return (BookPayload b s)
                                else fail $ "Obtained data for:" ++ s ++ " market"
        _ -> fail "Unable to parse orderbook and symbol in successful response" 


-----------------------------------------
newtype URL = URL {urlToString :: String} deriving (Show,Eq)
-----------------------------------------
-- Just Gets an httpS URL
-- returns (Response Code, Response Body)
--
getSecureURL :: URL -> IO (Int , ByteString)
getSecureURL url = do
    req      <- parseUrlThrow (urlToString url)
    manager  <- newManager tlsManagerSettings
    response <- httpLbs (req {secure = True}) manager
    return ( statusCode $ responseStatus response, responseBody response)

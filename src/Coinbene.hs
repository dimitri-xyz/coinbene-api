{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Market.Types                                   hiding (QuoteBook)


newtype URL = URL {urlToString :: String} deriving (Show,Eq)
-----------------------------------------
-- Just Gets an httpS URL
-- returns ( Response Code, Response Body)
--
getSecureURL :: URL -> IO (Int , ByteString)
getSecureURL url = do
    req      <- parseUrlThrow (urlToString url)
    manager  <- newManager tlsManagerSettings
    response <- httpLbs (req {secure = True}) manager
    return ( statusCode $ responseStatus response, responseBody response)
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
    -- , qbTimestamp :: MilliEpoch
    } deriving (Show, Eq, Generic)

-----------------------------------------
deriving instance Generic BRL
instance FromJSON BRL
instance ToJSON   BRL

deriving instance Generic BTC
instance FromJSON BTC
instance ToJSON   BTC

deriving instance Generic a => Generic (Price a)
deriving instance Generic a => Generic (Vol   a)
instance (Generic a, FromJSON a) => FromJSON (Price a)
instance (Generic a, FromJSON a) => FromJSON (Vol   a)
instance (Generic a, ToJSON   a) => ToJSON   (Price a)
instance (Generic a, ToJSON   a) => ToJSON   (Vol   a)


-- removes field label name mangling
writeQuoteOpts = defaultOptions {fieldLabelModifier = map toLower . drop 2}
------------
-- instance (ToJSON (Price p), ToJSON (Vol v)) => ToJSON (AskQuote p v) where
instance (ToJSON p, Generic p, ToJSON v, Generic v) => ToJSON (AskQuote p v) where
    toJSON = genericToJSON writeQuoteOpts
instance (ToJSON p, Generic p, ToJSON v, Generic v) => ToJSON (BidQuote p v) where
    toJSON = genericToJSON writeQuoteOpts

instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (AskQuote p v) where
    parseJSON = withObject "AskQuote" $ \v -> AskQ <$> v .: "price" <*> v .: "quantity"
instance (FromJSON p, Generic p, FromJSON v, Generic v) => FromJSON (BidQuote p v) where
    parseJSON = withObject "BidQuote" $ \v -> BidQ <$> v .: "price" <*> v .: "quantity"
------------

instance (ToJSON p, Generic p, ToJSON v, Generic v) => ToJSON (QuoteBook p v) where
    toJSON = genericToJSON writeQuoteOpts
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

class ParsePayload a where
    parsePayload :: Object -> Parser a

instance ParsePayload (NoPayload a b) where
    parsePayload _ = pure NoPayload 

instance (FromJSON p, Generic p, FromJSON v, Generic v) => ParsePayload (QuoteBookPayload p v) where
    parsePayload = parseQuoteBookPayload

instance (FromJSON p
         , Generic p
         , FromJSON v
         , Generic v
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


parseQuoteBookPayload :: (FromJSON p, Generic p, FromJSON v, Generic v) => Object -> Parser (QuoteBookPayload p v)
parseQuoteBookPayload h = do
    mBook   <- h .:? "orderbook"
    mSymbol <- h .:? "symbol"
    case (mBook, mSymbol) of
        (Just b, Just s) -> return (BookPayload b s)
        _ -> fail "Unable to parse orderbook and symbol in successful response" 

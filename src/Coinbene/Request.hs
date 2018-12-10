{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Coinbene.Request where

import GHC.Generics

import Network.HTTP.Simple                                  hiding (httpLbs)
import Network.HTTP.Client
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Network.HTTP.Types.Status    (statusCode)
import Data.ByteString.Lazy.Char8   (ByteString, unpack)

import Data.ByteString.Char8        (pack)
import Data.Aeson

import Coinbene
import Coinbene.Parse

-----------------------------------------
class Monad m => HTTP m where
    http :: Request -> Manager -> m (Response ByteString)

instance HTTP IO where
    http = httpLbs


data Confirmation
data OrderSide = Bid | Ask

class Exchange config m where
    getManager :: config -> m Manager
    placeLimit :: ( HTTP m, CoinSymbol p, CoinSymbol v) => config -> OrderSide -> Price p -> Vol v -> m Confirmation
    getBook    :: ( HTTP m, CoinSymbol p, CoinSymbol v) => config              -> Price p -> Vol v -> m (QuoteBook p v)


data API_ID
data API_KEY 

data Coinbene = Coinbene
    { cGetManager :: Manager
    , cGetAPIID   :: API_ID
    , cGetAPIKEY  :: API_KEY
    }

instance Exchange Coinbene IO where
    getManager = pure . cGetManager
    placeLimit = undefined
    getBook    = getCoinbeneBook


getCoinbeneBook
    :: forall m p v conf. 
    ( HTTP m
    , Exchange conf m
    , CoinSymbol p
    , CoinSymbol v
    )
    => conf -> Price p -> Vol v -> m (QuoteBook p v)
getCoinbeneBook conf p v = do
    let marketName = coinSymbol (undefined :: v) ++ coinSymbol (undefined :: p) 
        request
            = setRequestMethod "GET"
            $ setRequestHost "api.coinbene.com"
            $ setRequestPath "/v1/market/orderbook"
            $ setRequestQueryString [("symbol", Just $ pack $ marketName),("depth", Just "200")]
            $ setRequestSecure True
            $ setRequestPort 443
            $ defaultRequest

    manager  <- getManager conf
    response <- http request manager

    case eitherDecode (responseBody response) of
        Left errMsg -> error errMsg
        Right resp  -> return $ bpOrderbook $ rPayload resp

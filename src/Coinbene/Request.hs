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

import Control.Monad.Time           (MonadTime, currentTime)
import Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)

import Coinbene
import Coinbene.Parse

-----------------------------------------
class Monad m => HTTP m where
    http :: Request -> Manager -> m (Response ByteString)

instance HTTP IO where
    http = httpLbs

newtype OrderID = OID String
data OrderSide = Bid | Ask

type Confirmation = OrderID

class Exchange config m where
    placeLimit :: ( HTTP m, MonadTime m, CoinSymbol p, CoinSymbol v) => config -> OrderSide -> Price p -> Vol v -> m Confirmation
    getBook    :: ( HTTP m, MonadTime m, CoinSymbol p, CoinSymbol v) => config              -> Price p -> Vol v -> m (QuoteBook p v)


data API_ID = API_ID
data API_KEY 
data Coinbene = Coinbene
    { getManager :: Manager
    , getAPI_ID  :: API_ID
    , getAPI_KEY :: API_KEY
    }

instance Exchange Coinbene IO where
    placeLimit = placeCoinbeneLimit
    getBook    = getCoinbeneBook


-----------------------------------------
marketSymbol :: forall p v. (CoinSymbol p, CoinSymbol v) => Price p -> Vol v -> String
marketSymbol p v = coinSymbol (undefined :: v) ++ coinSymbol (undefined :: p)

coinbeneRequest
    = setRequestHost "api.coinbene.com"
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest

getCoinbeneBook :: forall m p v.
    ( HTTP m, CoinSymbol p, CoinSymbol v)
    => Coinbene -> Price p -> Vol v -> m (QuoteBook p v)
getCoinbeneBook config p v = do
    response <- http request (getManager config)
    case eitherDecode (responseBody response) of
        Left errMsg -> error ("getCoinbeneBook: " ++ errMsg ++ " response: " ++ show response) -- FIX ME! should not use `error` here
        Right resp  -> return $ bpOrderbook $ rPayload resp
  where
    marketName = marketSymbol (undefined :: Price p) (undefined :: Vol v)
    request
        = setRequestMethod "GET"
        $ setRequestPath "/v1/market/orderbook"
        $ setRequestQueryString query
        $ coinbeneRequest

    query = map toQueryItem $
        [ ("symbol", marketName)
        , ("depth", "200")]

placeCoinbeneLimit :: forall m p v.
    ( HTTP m, MonadTime m, CoinSymbol p, CoinSymbol v)
    => Coinbene -> OrderSide -> Price p -> Vol v -> m (Confirmation)
placeCoinbeneLimit config side p v = do
    signedReq <- signRequest (getAPI_ID config) (getAPI_KEY config) request
    response <- http signedReq (getManager config)
    return $ error ("placeCoinbeneLimit: " ++ " response: " ++ show response)
  where
    marketName = marketSymbol (undefined :: Price p) (undefined :: Vol v)
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/place"
        $ setRequestQueryString query
        $ coinbeneRequest

    query = map toQueryItem $
        [ ("symbol", marketName)
        , ("price", "22")
        , ("quantity", "33")
        , ("type", "buy-limit")
        ]

signRequest :: MonadTime m => API_ID -> API_KEY -> Request -> m Request
signRequest id key request = do
    now <- fmap (MilliEpoch . truncate . (1000*) . realToFrac . utcTimeToPOSIXSeconds) currentTime
    let signedQuery = signQuery id key now (getRequestQueryString request)
    return (setRequestQueryString signedQuery request)
  where
    signQuery :: API_ID -> API_KEY -> MilliEpoch -> Query -> Query
    signQuery id key now query = undefined


-- toQueryItem:: (ByteString, String) -> (ByteString, Maybe ByteString)
toQueryItem (a, b) = (a, Just $ pack b)




-- The items in the sign_list include the `apiid` and the `secret` key
-- def sign(**kwargs):
--     """
--     将传入的参数生成列表形式，排序后用＆拼接成字符串，用hashbli加密成生sign
--     """
--     sign_list = []
--     for key, value in kwargs.items():
--         sign_list.append("{}={}".format(key, value))
--     sign_list.sort()
--     sign_str = "&".join(sign_list)
--     mysecret = sign_str.upper().encode()
--     m = hashlib.md5()
--     m.update(mysecret)
--     return m.hexdigest()

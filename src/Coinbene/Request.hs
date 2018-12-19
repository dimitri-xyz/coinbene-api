{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Coinbene.Request where

import           GHC.Generics

import           Network.HTTP.Simple                        hiding (httpLbs, Proxy)
import           Network.HTTP.Client                        hiding (Proxy)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Status          (statusCode)

import qualified Data.ByteString.Lazy.Char8 as LBS  (ByteString, pack, unpack)
import           Data.ByteString.Char8              (ByteString, pack, unpack)
import           Data.Aeson
import           Data.HashMap.Strict                (fromList)

import           Data.Maybe                         (fromMaybe)
import           Data.Char                          (toUpper, isDigit)
import           Data.List                          (intercalate, sort, dropWhile)
import           Data.Proxy                         (Proxy(..))

import           Control.Monad.Time                 (MonadTime, currentTime)
import           Data.Time.Clock.POSIX              (utcTimeToPOSIXSeconds)

import           Crypto.Hash

import           Coinbene.Core
import           Coinbene.Parse

-----------------------------------------
class Monad m => HTTP m where
    http :: Request -> Manager -> m (Response LBS.ByteString)

instance HTTP IO where
    http = httpLbs

class Exchange config m where
    placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
    getBook       :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m (QuoteBook p v)
    getTrades     :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [Trade p v]
    getOpenOrders :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
    getOrderInfo  :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderInfo
    cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID
    getBalances   :: (HTTP m, MonadTime m) => config            -> m [BalanceInfo]

newtype API_ID  = API_ID  {getID  :: String} deriving Show
newtype API_KEY = API_KEY {getKey :: String} deriving Show

data Coinbene = Coinbene
    { getManager :: Manager
    , getAPI_ID  :: API_ID
    , getAPI_KEY :: API_KEY
    }

instance Exchange Coinbene IO where
    placeLimit     = placeCoinbeneLimit
    getBook config = getCoinbeneBook config 200
    getOrderInfo   = getCoinbeneOrderInfo
    cancel         = cancelCoinbeneOrder
    getOpenOrders  = getOpenCoinbeneOrders
    getBalances    = getCoinbeneBalances
    getTrades config = getCoinbeneTrades config 300


-----------------------------------------
strToQuery :: (String, String) -> (ByteString, Maybe ByteString)
strToQuery (x, y) = (pack x, Just $ pack y)

coinbeneRequest
    = setRequestHost "api.coinbene.com"
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest


-- FIX ME! should not use `error` here. This is not programmer error, but a possible run-time exception
decodeResponse :: ParsePayload payload => String -> Response LBS.ByteString -> payload
decodeResponse errFunctionName response = case eitherDecode' (responseBody response) of
    Left errMsg -> error (errFunctionName ++ ": " ++ errMsg ++ " - response: " ++ show response) 
    Right resp  -> case resp of
                        RespOK payload time -> payload
                        RespError desc time -> error (errFunctionName ++ ": " ++ desc ++ " - response: " ++ show response)


signRequest :: MonadTime m => API_ID -> API_KEY -> [(String, String)] -> Request -> m Request
signRequest id key params request = do
    now <- fmap (MilliEpoch . truncate . (1000*) . realToFrac . utcTimeToPOSIXSeconds) currentTime
    let signedParams = signParams id key now params
    return $ setRequestBodyJSON (fromList signedParams) request
  where
    signParams :: API_ID -> API_KEY -> MilliEpoch -> [(String, String)] -> [(String, String)]
    signParams id key now params =
        let msg = ("apiid", getID id) : ("timestamp", showBareMilliEpoch now) : params
            sig = md5 $ intercalate "&" $ sort $ fmap (fmap toUpper . pairUp) (("secret", getKey key) : msg)
            pairUp (x,y) = x ++ "=" ++ y
            md5 x = show (hashWith MD5 (pack x))
         in ("sign", sig) : msg


-----------------------------------------
getCoinbeneBook :: forall m p v.
    ( HTTP m, Coin p, Coin v)
    => Coinbene -> Int -> Proxy (Price p) -> Proxy (Vol v) -> m (QuoteBook p v)
getCoinbeneBook config depth pp vv = do
    response <- http request (getManager config)
    return $ bpOrderbook $ decodeResponse "getCoinbeneBook" response
  where
    marketName = marketSymbol pp vv
    request
        = setRequestMethod "GET"
        $ setRequestPath "/v1/market/orderbook"
        $ setRequestHeaders [("Content-Type","application/x-www-form-urlencoded;charset=utf-8"),("Connection","keep-alive")]
        $ setRequestQueryString (fmap strToQuery query)
        $ coinbeneRequest

    query =
        [ ("symbol", marketName)
        , ("depth", show depth)
        ]

placeCoinbeneLimit :: forall m p v.
    ( HTTP m, MonadTime m, Coin p, Coin v)
    => Coinbene -> OrderSide -> Price p -> Vol v -> m (OrderID)
placeCoinbeneLimit config side p v = do
    signedReq <- signRequest (getAPI_ID config) (getAPI_KEY config) params request
    response <- http signedReq (getManager config)
    return $ (\(OIDPayload x) -> x) $ decodeResponse "placeCoinbeneLimit" response

  where
    marketName = marketSymbol (Proxy :: Proxy (Price p)) (Proxy :: Proxy (Vol v))
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/place"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params =
        [ ("symbol"  , marketName)
        , ("price"   , showBarePrice p)
        , ("quantity", showBareVol   v)
        , ("type"    , case side of {Bid -> "buy-limit"; Ask -> "sell-limit"})
        ]

getCoinbeneOrderInfo :: (HTTP m, MonadTime m) => Coinbene -> OrderID -> m OrderInfo
getCoinbeneOrderInfo config (OrderID oid) = do
    signedReq <- signRequest (getAPI_ID config) (getAPI_KEY config) params request
    response <- http signedReq (getManager config)
    return $ (\(OrderInfoPayload x) -> x) $ decodeResponse "getCoinbeneOrderInfo" response

  where
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/info"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("orderid"  , oid)]


cancelCoinbeneOrder :: (HTTP m, MonadTime m) => Coinbene -> OrderID -> m OrderID
cancelCoinbeneOrder config (OrderID oid) = do
    signedReq <- signRequest (getAPI_ID config) (getAPI_KEY config) params request
    response <- http signedReq (getManager config)
    return $ (\(OIDPayload x) -> x) $ decodeResponse "cancelCoinbeneOrder" response

  where
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/cancel"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("orderid"  , oid)]


getOpenCoinbeneOrders :: (HTTP m, MonadTime m, Coin p, Coin v)
    => Coinbene -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
getOpenCoinbeneOrders config pp vv = do
    signedReq <- signRequest (getAPI_ID config) (getAPI_KEY config) params request
    response <- http signedReq (getManager config)
    return $ ooOrders $ decodeResponse "getOpenCoinbeneOrders" response

  where
    marketName = marketSymbol pp vv
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/open-orders"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("symbol", marketName)]


getCoinbeneBalances :: (HTTP m, MonadTime m) => Coinbene -> m [BalanceInfo]
getCoinbeneBalances config = do
    signedReq <- signRequest (getAPI_ID config) (getAPI_KEY config) params request
    response <- http signedReq (getManager config)
    return $ bBalances $ decodeResponse "getCoinbeneBalances" response

  where
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/balance"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("account"  , "exchange")]


getCoinbeneTrades :: forall m p v.
    ( HTTP m, Coin p, Coin v)
    => Coinbene -> Int -> Proxy (Price p) -> Proxy (Vol v) -> m [Trade p v]
getCoinbeneTrades config num pp vv = do
    response <- http request (getManager config)
    return $ tTrades $ decodeResponse "getCoinbeneTrades" response
  where
    marketName = marketSymbol pp vv
    request
        = setRequestMethod "GET"
        $ setRequestPath "/v1/market/trades"
        $ setRequestHeaders [("Content-Type","application/x-www-form-urlencoded;charset=utf-8"),("Connection","keep-alive")]
        $ setRequestQueryString (fmap strToQuery query)
        $ coinbeneRequest

    query =
        [ ("symbol", marketName)
        , ("size", show num)
        ]


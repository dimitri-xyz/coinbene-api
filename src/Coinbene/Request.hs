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
import           Network.HTTP.Types.Status          (statusCode, ok200, badGateway502)

import qualified Data.ByteString.Lazy.Char8 as LBS  (ByteString, pack, unpack)
import           Data.ByteString.Char8              (ByteString, pack, unpack)
import           Data.Aeson
import           Data.HashMap.Strict                (fromList)

import           Data.Maybe                         (fromMaybe)
import           Data.Char                          (toUpper, isDigit)
import           Data.List                          (intercalate, sort, dropWhile)
import           Data.Proxy                         (Proxy(..))

-- import           Control.Exception
import           Control.Monad.Catch
import           Control.Concurrent                 (threadDelay)
import           Control.Monad.Time                 (MonadTime, currentTime)
import           Data.Time.Clock.POSIX              (utcTimeToPOSIXSeconds)

import           Crypto.Hash

import           Coinbene.Core
import           Coinbene.Parse

import           Debug.Trace

-----------------------------------------
data Verbosity = Silent | Normal | Verbose | Deafening deriving (Show, Eq, Ord)

retryDelay = 10 * 1000 * 1000 -- 10 seconds (in microseconds)
-----------------------------------------
class MonadCatch m => HTTP m where
    http  :: Request -> Manager -> m (Response LBS.ByteString)
    sleep :: Int -> m ()  -- threadDelay, generalized

instance HTTP IO where
    http = httpLbs
    sleep = threadDelay

-----------------------------------------
retry :: HTTP m => Bool -> Verbosity -> Int -> m a -> m a
retry idempotent verbosity delay action = action `catches`
    [ Handler (\(ex :: ExchangeError) -> waitAndRetry                   verbosity delay action ex)
    , Handler (\(ex :: HttpException) -> handleHttpException idempotent verbosity delay action ex)
    ]

waitAndRetry :: (HTTP m, Exception e) => Verbosity -> Int -> m a -> e -> m a
waitAndRetry verbosity delay action ex = do
    if verbosity >= Normal
        then trace ("Caught Exception: " <> displayException ex <> "\n Retrying after " <> show delay <> " microseconds \n") $ return ()
        else return ()
    sleep delay
    a <- action -- the retry
    if verbosity >= Normal
        then trace ("Retry worked for: " <> displayException ex) $ return a
        else return a

handleHttpException :: HTTP m => Bool -> Verbosity -> Int -> m a -> HttpException -> m a
handleHttpException idem verbosity delay action ex@(HttpExceptionRequest req (StatusCodeException resp _))
    | responseStatus resp == badGateway502 = waitAndRetry verbosity delay action ex
    | otherwise                            = throwM ex
-- for other (network) errors, only retry on idempotent API calls
handleHttpException idem verbosity delay action ex
    | idem      = waitAndRetry verbosity delay action ex
    | otherwise = throwM ex
-----------------------------------------

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
    , verbosity  :: Verbosity
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
    $ setRequestCheckStatus -- force throwing `StatusCodeException` on non-2XX status codes
    $ defaultRequest

decodeResponse :: ParsePayload payload => String -> Response LBS.ByteString -> Either ExchangeError payload
decodeResponse errFunctionName response = case eitherDecode' (responseBody response) of
    Left errMsg -> Left $ JSONDecodingError (errFunctionName ++ ": " ++ errMsg ++ " - response: " ++ show response)
    Right resp  -> case resp of
                        RespOK payload time -> Right payload
                        RespError desc time -> Left $ ExchangeError (errFunctionName ++ ": " ++ desc ++ " - response: " ++ show response)


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
-- FIX ME! "DRY" violations on the following API calls

getCoinbeneBook :: forall m p v.
    ( HTTP m, Coin p, Coin v)
    => Coinbene -> Int -> Proxy (Price p) -> Proxy (Vol v) -> m (QuoteBook p v)
getCoinbeneBook config depth pp vv = retry True (verbosity config) retryDelay $ do
    response <- http
                    (if verbosity config == Deafening then traceShowId request else request)
                    (getManager config)

    let result = decodeResponse "getCoinbeneBook" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return (bpOrderbook payload)

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
placeCoinbeneLimit config side p v = retry False (verbosity config) retryDelay $ do
    signedReq <- signRequest
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (if verbosity config == Verbose then trace ("/v1/trade/order/place " <> show params) params else params)
                    request
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeResponse "placeCoinbeneLimit" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ (\(OIDPayload x) -> x) payload

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
getCoinbeneOrderInfo config (OrderID oid) = retry True (verbosity config) retryDelay $ do
    signedReq <- signRequest
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (if verbosity config == Verbose then trace ("/v1/trade/order/info " <> show params) params else params)
                    request
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeResponse "getCoinbeneOrderInfo" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ (\(OrderInfoPayload x) -> x) payload

  where
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/info"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("orderid"  , oid)]


cancelCoinbeneOrder :: (HTTP m, MonadTime m) => Coinbene -> OrderID -> m OrderID
cancelCoinbeneOrder config (OrderID oid) = retry True (verbosity config) retryDelay $ do
    signedReq <- signRequest
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (if verbosity config == Verbose then trace ("/v1/trade/order/cancel " <> show params) params else params)
                    request
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeResponse "cancelCoinbeneOrder" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ (\(OIDPayload x) -> x) payload

  where
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/cancel"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("orderid"  , oid)]


getOpenCoinbeneOrders :: (HTTP m, MonadTime m, Coin p, Coin v)
    => Coinbene -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
getOpenCoinbeneOrders config pp vv = retry True (verbosity config) retryDelay $ do
    signedReq <- signRequest
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (if verbosity config == Verbose then trace ("/v1/trade/order/open-orders " <> show params) params else params)
                    request
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeResponse "getOpenCoinbeneOrders" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ ooOrders payload

  where
    marketName = marketSymbol pp vv
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/order/open-orders"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("symbol", marketName)]


getCoinbeneBalances :: (HTTP m, MonadTime m) => Coinbene -> m [BalanceInfo]
getCoinbeneBalances config = retry True (verbosity config) retryDelay $ do
    signedReq <- signRequest
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (if verbosity config == Verbose then trace ("/v1/trade/balance " <> show params) params else params)
                    request
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeResponse "getCoinbeneBalances" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ bBalances payload

  where
    request
        = setRequestMethod "POST"
        $ setRequestPath "/v1/trade/balance"
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneRequest

    params = [("account"  , "exchange")]


getCoinbeneTrades :: forall m p v.
    (HTTP m, Coin p, Coin v)
    => Coinbene -> Int -> Proxy (Price p) -> Proxy (Vol v) -> m [Trade p v]
getCoinbeneTrades config num pp vv = retry True (verbosity config) retryDelay $ do
    response <- http
                    (if verbosity config == Deafening then traceShowId request else request)
                    (getManager config)

    let result = decodeResponse "getCoinbeneTrades" (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Deafening then traceShowId x else x)
                $ tTrades payload

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


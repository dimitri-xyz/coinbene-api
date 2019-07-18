{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Coinbene.Request where

import           GHC.Generics

import           Network.HTTP.Simple                        hiding (httpLbs, Proxy)
import           Network.HTTP.Client                        hiding (Proxy)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types                 (statusCode, ok200, badGateway502, internalServerError500, methodGet, methodPost)

import qualified Data.ByteString.Lazy.Char8 as LBS  (ByteString, pack, unpack)
import           Data.ByteString.Char8              (ByteString, pack, unpack)
import           Data.ByteString.Lazy               (toStrict, fromStrict)
import           Data.Aeson
import           Data.HashMap.Strict                (fromList)

import           Data.Maybe                         (fromMaybe)
import           Data.Char                          (toUpper, isDigit)
import           Data.List                          (intercalate, sort, dropWhile, isPrefixOf)
import           Data.Proxy                         (Proxy(..))

import           Control.Monad.Catch
import           Control.Concurrent                 (threadDelay)
import           Control.Monad.Time                 (MonadTime, currentTime)
import           Data.Time.Clock                    (UTCTime(..))
import           Data.Time.ISO8601                  (formatISO8601Millis)

import           Crypto.Hash                        (SHA256, hashWith, MD5(..))
import           Crypto.MAC.HMAC                    (HMAC, hmac, hmacGetDigest)

import           Coinbene.Core
import           Coinbene.Parse

import           Debug.Trace


import Coins (BTC)

{---------------------------------------

    NOTE: Failed Cancellation Requests

CoinBene's order cancellation API currently does not return any information about how much was
still "not executed" when the cancellation request was made. It only fails the request, if the
order was already *fully* executed or cancelled. This 1 extra bit of information is not very
useful, so we ignore it.

For example: consider a request to sell 100 Bitcoins. The request executes 1 Bitcoins and then
we request a cancellation, thinking we will cancel it soon. But that does not happen and another
98.99 Bitcoins are sold. And only when a total of 99.99 Bitcoins have been sold and there is
only 0.01 Bitcoin left to sell, the order is canceled. The order cancellation API requests
returns a success, but honestly, this is deceptive. In this case, it might as well have
returned the failure.

The fact is we don't know how much was still open at the time of cancellation. This information
is not returned and this is a problem. We consider this type of cancellation failure a
success meaning: "ok, request received, but it may be too late."

-}

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
    | responseStatus resp == badGateway502          = waitAndRetry verbosity delay action ex
    | responseStatus resp == internalServerError500 = waitAndRetry verbosity delay action ex
    | otherwise                                     = throwM ex
-- for other (network) errors, only retry on idempotent API calls
handleHttpException idem verbosity delay action ex
    | idem      = waitAndRetry verbosity delay action ex
    | otherwise = throwM ex

-----------------------------------------
newtype API_ID  = API_ID  {getID  :: String} deriving Show
newtype API_KEY = API_KEY {getKey :: String} deriving Show

data Coinbene = Coinbene
    { getManager :: Manager
    , getAPI_ID  :: API_ID
    , getAPI_KEY :: API_KEY
    , verbosity  :: Verbosity
    }

class Exchange config m where
    placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
    getBook       :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m (QuoteBook p v)
    getTrades     :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [Trade p v]
    getOpenOrders :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
    getOrderInfo  :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderInfo
    cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID
    getBalances   :: (HTTP m, MonadTime m) => config            -> m [BalanceInfo]


instance Exchange Coinbene IO where
    placeLimit     = placeCoinbeneLimit
    getBook config = getCoinbeneBook config 200
    getOrderInfo   = getCoinbeneOrderInfo
    cancel         = cancelCoinbeneOrder
    getOpenOrders  = getOpenCoinbeneOrders
    getBalances    = getCoinbeneBalances
    getTrades config = getCoinbeneTrades config 300


newtype ReqID = ReqID String deriving (Eq, Show)

class FuturesExchange config m where
    placeOrder    :: (HTTP m, MonadTime m, Coin p, Coin market) => config -> Proxy market -> ReqID -> OrderSide -> Price p -> Vol p -> m OrderID
    getFuturesOpenOrders :: (HTTP m, MonadTime m, Coin p, Coin market) => config -> Proxy (Price p) -> Proxy market -> m [FuturesOrderInfo]
    getFuturesOrderInfo  :: (HTTP m, MonadTime m) => config -> OrderID -> m FuturesOrderInfo
    cancelOrder   :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID
    getAccInfo    :: (HTTP m, MonadTime m) => config            -> m FuturesAccInfo

instance FuturesExchange Coinbene IO where
    placeOrder config market reqId side = placeCoinbeneFuturesLimit config market Fixed (Leverage 10) reqId (case side of {Bid -> OpenLong; Ask -> OpenShort})
    cancelOrder                         = cancelCoinbeneFuturesLimit
    getAccInfo                          = getCoinbeneFuturesAccInfo
    getFuturesOpenOrders                = getOpenCoinbeneFutures
    getFuturesOrderInfo                 = getCoinbeneFuturesOrderInfo

-----------------------------------------
strToQuery :: (String, String) -> (ByteString, Maybe ByteString)
strToQuery (x, y) = (pack x, Just $ pack y)

coinbeneRequest
    = setRequestHost "api.coinbene.com"
    $ setRequestSecure True
    $ setRequestPort 443
    $ setRequestCheckStatus -- force throwing `StatusCodeException` on non-2XX status codes
    $ defaultRequest

coinbeneFuturesReq
    = setRequestHost "openapi-contract.coinbene.com"
    $ setRequestSecure True
    $ setRequestPort 443
    $ setRequestCheckStatus -- force throwing `StatusCodeException` on non-2XX status codes
    $ defaultRequest

decodeResponse :: FromUnwrappedToplevelJSON payload => String -> Response LBS.ByteString -> Either ExchangeError payload
decodeResponse errFunctionName response = case eitherDecode' (responseBody response) of
    Left errMsg -> Left $ JSONDecodingError (errFunctionName ++ ": " ++ errMsg ++ " - response: " ++ show response)
    Right resp  -> case resp of
                        RespOK payload time -> Right payload
                        RespError desc time -> Left $ ExchangeError genericError (errFunctionName ++ ": " ++ desc ++ " - response: " ++ show response)

decodeFuturesResponse :: FromJSON payload => String -> Response LBS.ByteString -> Either ExchangeError payload
decodeFuturesResponse errFunctionName response = case eitherDecode' (responseBody response) of
    Left errMsg -> Left $ JSONDecodingError (errFunctionName ++ ": " ++ errMsg ++ " - response: " ++ show response)
    Right resp  -> case resp of
                        FuturesResp code message Nothing        -> Left $ ExchangeError code (errFunctionName ++ " - missing data - response: " ++ show response)
                        FuturesResp code message (Just payload) -> Right payload

signSpotRequest :: MonadTime m => API_ID -> API_KEY -> [(String, String)] -> Request -> m Request
signSpotRequest id key params request = do
    now <- utcTimeToMilliEpoch <$> currentTime
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

-- FIX ME! CoinBene has changed the signature method for the Futures API. I am still figuring out how
-- best to code the two options.
signFuturesRequest :: MonadTime m => API_ID -> API_KEY -> [(String, String)] -> Request -> m Request
signFuturesRequest id key params request = do
    now <- utcTimeToMilliEpoch <$> currentTime
    let signedParams = signParams id key now (unpack $ path request) params
    return $ setRequestBodyJSON (fromList signedParams) request
  where
    signParams :: API_ID -> API_KEY -> MilliEpoch -> String -> [(String, String)] -> [(String, String)]
    signParams id key now urlPath params =
        let msg' = ("requestURI", urlPath) : ("apiid", getID id) : ("timestamp", showBareMilliEpoch now) : params
            sig  = hmac (pack $ getKey key) (pack $ intercalate "&" $ sort $ fmap (fmap toUpper . pairUp) msg') :: HMAC SHA256
            pairUp (x,y) = x ++ "=" ++ y
            mac  = show (hmacGetDigest sig)

         in ("sign", mac) : tail msg'

signFuturesRequestV2 :: MonadTime m => API_ID -> API_KEY -> Verbosity -> Request -> m Request
signFuturesRequestV2 apiId key verbosity request = do
    now <- currentTime
    let timeStr = pack (formatISO8601Millis now)
        mac = calculateMAC apiId key timeStr request
    return
        $ addRequestHeader "ACCESS-KEY"       (pack $ getID apiId)
        $ addRequestHeader "ACCESS-TIMESTAMP" timeStr
        $ addRequestHeader "ACCESS-SIGN"      mac
        $ request
  where
    calculateMAC :: API_ID -> API_KEY -> ByteString -> Request -> ByteString
    calculateMAC apiId key timeStr req
        | methodGet  == method req, mempty == getBody req               = pack $ show $ hmacGetDigest $ (hmac (pack $ getKey key) ( (if verbosity >= Verbose then traceShowId else id) $ timeStr <>  "GET" <> path req <>        queryString req) :: HMAC SHA256)
        | methodPost == method req, mempty == getRequestQueryString req = pack $ show $ hmacGetDigest $ (hmac (pack $ getKey key) ( (if verbosity >= Verbose then traceShowId else id) $ timeStr <> "POST" <> path req <> toStrict (getBody req)) :: HMAC SHA256)
        | otherwise = error $ "calculateMAC - An HTTP Request to be authenticated can either have query string (if GET) or a request body (if POST), no other cases are allowed: " <> show req

    getBody :: Request -> LBS.ByteString
    getBody req
        | RequestBodyLBS lbs <- requestBody req = lbs
        | RequestBodyBS   bs <- requestBody req = fromStrict bs
        | otherwise                             = error $ "Cannot read RequestBody in HTTP request (chunked?): " <> show req

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
    signedReq <- signSpotRequest
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
    signedReq <- signSpotRequest
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

-- See NOTE: Failed Cancellation Requests
cancelCoinbeneOrder :: forall m. (HTTP m, MonadTime m) => Coinbene -> OrderID -> m OrderID
cancelCoinbeneOrder config (OrderID oid) = retry True (verbosity config) retryDelay $ do
    signedReq <- signSpotRequest
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (if verbosity config == Verbose then trace ("/v1/trade/order/cancel " <> show params) params else params)
                    request
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeResponse errFunctionName (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left  err      -> acceptAttemptedRequest (verbosity config) errFunctionName (OrderID oid) err
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

    errFunctionName = "cancelCoinbeneOrder"

    -- a failed cancellation attempt because the order was already fully executed or canceled
    -- is considered a successful request
    acceptAttemptedRequest :: Verbosity -> String -> OrderID -> ExchangeError -> m OrderID
    acceptAttemptedRequest verbosity fname oid err = case err of
            ExchangeError _ msg ->
                if (fname <> ": " <> "The transaction has been traded, failure") `isPrefixOf` msg ||
                   (fname <> ": " <> "Invalid Order Status")                     `isPrefixOf` msg
                    then return $ (\x -> if verbosity >= Normal then trace ("Request to cancel already executed order: " <> show x) x else x) oid
                    else throwM err
            _ ->         throwM err

getOpenCoinbeneOrders :: (HTTP m, MonadTime m, Coin p, Coin v)
    => Coinbene -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
getOpenCoinbeneOrders config pp vv = retry True (verbosity config) retryDelay $ do
    signedReq <- signSpotRequest
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
    signedReq <- signSpotRequest
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

----------------------------------------
getCoinbeneFuturesAccInfo :: (HTTP m, MonadTime m) => Coinbene -> m FuturesAccInfo
getCoinbeneFuturesAccInfo config = retry True (verbosity config) retryDelay $ do
    signedReq <- signFuturesRequestV2
                    (getAPI_ID  config)
                    (getAPI_KEY config)
                    (verbosity config)
                    (if verbosity config == Verbose then trace path request else request)
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeFuturesResponse "getCoinbeneFuturesAccInfo"
                    (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ payload

  where
    path = "/api/swap/v2/account/info"
    request
        = setRequestMethod "GET"
        $ setRequestPath (pack path)
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneFuturesReq

----------------------------------------
placeCoinbeneFuturesLimit ::
    forall m p v market. (HTTP m, MonadTime m, Coin p, Coin market)
    => Coinbene -> Proxy market -> MarginMode -> Leverage -> ReqID -> Direction -> Price p -> Vol p -> m OrderID
placeCoinbeneFuturesLimit config market marginMode leverage (ReqID reqId) direction p v = retry False (verbosity config) retryDelay $ do
    signedReq <- signFuturesRequestV2
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (verbosity config)
                    (if verbosity config == Verbose then trace (path <> " " <> show body) request else request)
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeFuturesResponse "placeCoinbeneFuturesLimit"
                    (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ orderId payload

  where
    path = "/api/swap/v2/order/place"
    request
        = setRequestMethod "POST"
        $ setRequestPath (pack path)
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ setRequestQueryString []
        $ setRequestBodyJSON body
        $ coinbeneFuturesReq

    body =
        PlaceBody
        { pbSymbol     = marketSymbol (Proxy :: Proxy (Price p)) (Proxy :: Proxy (Vol market))
        , pbDirection  = direction
        , pbLeverage   = leverage
        , pbOrderPrice = p
        , pbQuantity   = v
        , pbMarginMode = marginMode
        , pbClientId   = if reqId == mempty then Nothing else Just reqId
        }

data PlaceLimitRequestBody p =
    PlaceBody
    { pbSymbol     :: String
    , pbDirection  :: Direction
    , pbLeverage   :: Leverage
    , pbOrderPrice :: Price p
    , pbQuantity   :: Vol   p
    , pbMarginMode :: MarginMode
    , pbClientId   :: Maybe String
    } deriving (Eq, Show, Generic)

instance Coin p => ToJSON (PlaceLimitRequestBody p) where
    toJSON (PlaceBody sy d l (Price p) (Vol q) mode mCoid) = object
        [ "symbol"     .= sy
        , "direction"  .= d
        , "leverage"   .= l -- Must be integer (no decimal point)
        , "orderPrice" .= showBare p
        , "quantity"   .= (show $ truncate $ ((read $ showBare q) :: Double)) -- Must be integer
        , "marginMode" .= mode
        , "clientId"   .= mCoid
        ]


----------------------------------------
cancelCoinbeneFuturesLimit :: forall m . (HTTP m, MonadTime m) => Coinbene -> OrderID -> m OrderID
cancelCoinbeneFuturesLimit config oid = retry False (verbosity config) retryDelay $ do
    signedReq <- signFuturesRequestV2
                    (getAPI_ID config)
                    (getAPI_KEY config)
                    (verbosity config)
                    (if verbosity config == Verbose then trace (path <> " " <> show body) request else request)
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeFuturesResponse errFunctionName (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> acceptAttemptedRequest (verbosity config) errFunctionName oid exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ cancelOrderId payload
  where
    errFunctionName = "cancelCoinbeneFuturesLimit"
    path = "/api/swap/v2/order/cancel"
    request
        = setRequestMethod "POST"
        $ setRequestPath (pack path)
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ setRequestQueryString []
        $ setRequestBodyJSON body
        $ coinbeneFuturesReq

    body = CancelLimitRequestBody oid

    -- a failed cancellation attempt because the order was already fully executed or canceled
    -- is considered a successful request
    acceptAttemptedRequest :: Verbosity -> String -> OrderID -> ExchangeError -> m OrderID
    acceptAttemptedRequest verbosity fname oid err = case err of
            ExchangeError didNotCompleteError msg ->
                    return $ (\x -> if verbosity >= Normal then trace ("Request to cancel already executed order: " <> show x) x else x) oid
            _ -> throwM err

data CancelLimitRequestBody = CancelLimitRequestBody { orderIdToCancel :: OrderID } deriving (Eq, Show, Generic)
instance ToJSON CancelLimitRequestBody where
    toJSON (CancelLimitRequestBody oid) = object [ "orderId" .= oid ]

----------------------------------------
getOpenCoinbeneFutures :: forall m p market. (HTTP m, MonadTime m, Coin p, Coin market) => Coinbene -> Proxy (Price p) -> Proxy market -> m [FuturesOrderInfo]
getOpenCoinbeneFutures config _ _ = retry True (verbosity config) retryDelay $ do
    signedReq <- signFuturesRequestV2
                    (getAPI_ID  config)
                    (getAPI_KEY config)
                    (verbosity config)
                    (if verbosity config == Verbose then trace path request else request)
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeFuturesResponse "getOpenCoinbeneFutures"
                    (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ payload

  where
    marketName = marketSymbol (Proxy :: Proxy (Price p)) (Proxy :: Proxy (Vol market))
    path = "/api/swap/v2/order/openOrders"
    query = [("symbol", marketName)]
    request
        = setRequestMethod "GET"
        $ setRequestPath (pack path)
        $ setRequestQueryString (fmap strToQuery query)
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneFuturesReq

-- FIX ME! DRY violations on all these
----------------------------------------
getCoinbeneFuturesOrderInfo :: (HTTP m, MonadTime m) => Coinbene -> OrderID -> m FuturesOrderInfo
getCoinbeneFuturesOrderInfo config (OrderID oid) = retry True (verbosity config) retryDelay $ do
    signedReq <- signFuturesRequestV2
                    (getAPI_ID  config)
                    (getAPI_KEY config)
                    (verbosity config)
                    (if verbosity config == Verbose then trace path request else request)
    response <- http
                    (if verbosity config == Deafening then traceShowId signedReq else signedReq)
                    (getManager config)

    let result = decodeFuturesResponse "getCoinbeneFuturesOrderInfo"
                    (if verbosity config == Deafening then traceShowId response else response)
    case result of
        Left exception -> throwM exception
        Right payload  -> return
                $ (\x -> if verbosity config == Verbose then traceShowId x else x)
                $ payload

  where
    path = "/api/swap/v2/order/info"
    query = [("orderId", oid)]
    request
        = setRequestMethod "GET"
        $ setRequestPath (pack path)
        $ setRequestQueryString (fmap strToQuery query)
        $ setRequestHeaders [("Content-Type","application/json;charset=utf-8"),("Connection","keep-alive")]
        $ coinbeneFuturesReq


----------------------------------------
newtype CoinbeneFutures = CoinbeneFutures Coinbene

instance Exchange CoinbeneFutures IO where
    placeLimit                                 = translatePlaceLimit
    getOpenOrders                              = translateGetOpenOrders
    getOrderInfo  (CoinbeneFutures config) oid = fromFuturesOrderInfo <$> getFuturesOrderInfo config oid
    cancel        (CoinbeneFutures config)     = cancelOrder config
    getBook       (CoinbeneFutures config) _ _ = return $ QuoteBook{ qbAsks = [], qbBids = [] }
    getBalances                                = undefined
    getTrades                                  = undefined


-- FIX ME! (readBare . showBareVol) is a very expensive no-op to just do
-- type conversion. It is only done on 1 value per call, but still!
translatePlaceLimit
    :: forall m p v.
    (FuturesExchange Coinbene m, HTTP m, MonadTime m, Coin p, Coin v)
    => CoinbeneFutures -> OrderSide -> Price p -> Vol v -> m OrderID
translatePlaceLimit (CoinbeneFutures config) side price vol =
    placeOrder config (Proxy :: Proxy BTC) (ReqID "") side price (Vol . readBare . showBareVol $ vol)

translateGetOpenOrders
    :: forall m p v.
    (FuturesExchange Coinbene m, HTTP m, MonadTime m, Coin p, Coin v)
    => CoinbeneFutures -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
translateGetOpenOrders (CoinbeneFutures config) pp _ =
    fmap fromFuturesOrderInfo <$> getFuturesOpenOrders config pp (Proxy :: Proxy v)

fromFuturesOrderInfo  :: FuturesOrderInfo -> OrderInfo
fromFuturesOrderInfo info = LimitOrder
    { market     = foiMarket info
    , oSide      = case foiDirection info of
                        OpenLong  -> Bid
                        OpenShort -> Ask
                        _         -> error $ "NOT IMPLEMENTED - fromFuturesOrderInfo: " <> show (foiDirection info)
    , limitPrice = foiLimitPrice info
    , limitVol   = foiLimitVol info
    , orderID    = foiOrderID  info
    , created    = utcTimeToMilliEpoch (foiCreated info)
    , mModified  = Nothing
    , status     = case foiStatus info of
                    StatusNew             -> Unfilled
                    StatusFilled          -> Filled
                    StatusCanceled        -> Canceled
                    StatusPartiallyFilled -> PartiallyFilled
                    -- TO DO: We no longer generate `PartiallyCanceled` statuses. We ASSUME those are now included in `StatusCanceled`
                    -- But this MAY NOT be the correct meaning. Is this correct!?
    , filledVol  = foiFilledVol info
    , filledAmount     = undefined -- TO DO: You better not touch this! It's NOT clear if this is exactly = foiFilledVol info * foiAvePrice info
    , mAvePriceAndFees = Just (foiAvePrice info, foiFees info)
    }

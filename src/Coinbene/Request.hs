{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene.Request where

import Network.HTTP.Client
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Network.HTTP.Types.Status    (statusCode)
import Data.ByteString.Lazy.Char8   (ByteString, unpack)

import Network.HTTP.Simple hiding (httpLbs)
import Data.ByteString.Char8        (pack)

import Coinbene

-----------------------------------------
class Monad m => HTTP m where
    http :: Request -> Manager -> m (Response ByteString)

instance HTTP IO where
    http = httpLbs


data Confirmation
data OrderSide = Bid | Ask

class Exchange config where
    getManager ::           config -> Manager
    placeLimit :: (HTTP m, CoinSymbol p, CoinSymbol v) => config -> OrderSide -> Price p -> Vol v -> m Confirmation
    getBook    :: (HTTP m, CoinSymbol p, CoinSymbol v) => config -> OrderSide -> Price p -> Vol v -> m (QuoteBook p v)


data API_ID
data API_KEY 

data Coinbene = Coinbene
    { cGetManager :: Manager
    , cGetAPIID   :: API_ID
    , cGetAPIKEY  :: API_KEY
    }

instance Exchange Coinbene where
    getManager = cGetManager
    placeLimit = undefined
    getBook    = getCoinbeneBook


getCoinbeneBook
    :: forall m p v. (HTTP m, CoinSymbol p, CoinSymbol v) 
    => Coinbene -> OrderSide -> Price p -> Vol v -> m (QuoteBook p v)
getCoinbeneBook config side p v = do
    let marketName = coinSymbol (undefined :: v) ++ coinSymbol (undefined :: p) 
        request
            = setRequestMethod "GET"
            $ setRequestHost "api.coinbene.com"
            $ setRequestPath "/v1/market/orderbook"
            $ setRequestQueryString [("symbol", Just $ pack $ marketName),("depth", Just "200")]
            $ setRequestSecure True
            $ setRequestPort 443
            $ defaultRequest

    -- putStrLn $ show request
    -- putStrLn $ unpack $ (queryString request)

    response <- http request (getManager config)
    -- putStrLn $ show response

    -- putStrLn $ "The status code was: " ++
    --            show (getResponseStatusCode response)
    -- print $ getResponseHeader "Content-Type" response
    return (QuoteBook [] [])


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




    -- -- to place market orders
    -- -- This should fail with a run-time exception if a requested limiting
    -- -- factor cannot be enforced at the exchange. For example, if the
    -- -- client asks to limit the market order by overall cost, but the exchange
    -- -- can only limit by volume.
    -- placeMarket :: HTTP m => config -> OrderSide -> CostAndOrVolume -> m Confirmation

    -- -- | returns order info as it stood immediately AFTER cancellation; or an exception if it wasn't able to complete
    -- --   the request for any other reason (i.e. order already canceled, already executed or network failure)
    -- cancelOrder :: HTTP m => config -> OrderID ->  m Confirmation

    -- -- | returns pending [Order] or an Exception if can't get that information
    -- getPendingOrders :: HTTP m => config -> m [Order Confirmation]

    -- -- | returns [Order] of the right type or an Exception if can't get that information. Maybe parameters are optional.
    -- -- timestamps are start and end time.
    -- getOrders :: HTTP m => config -> Maybe OrderSide -> Maybe Timestamp -> Maybe Timestamp -> m [Order Confirmation]

    -- getFunds :: HTTP m => config -> m (CurrencyVolume , BTCVolume , Timestamp)

    -- transferBTC :: HTTP m => config -> Volume -> BTCWallet -> m BTCTransactionID


{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef ETHEREUM
#define MACRO_CURRENCY     ETH
#define MACRO_MARKET_NAME  "ETHBRL"
#define MACRO_TEST_VOL     0.01
#else
#define MACRO_CURRENCY     BTC
#define MACRO_MARKET_NAME  "BTCBRL"
#define MACRO_TEST_VOL     0.001
#endif

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options

import Data.Proxy
import Data.Aeson
import Coinbene
import Coinbene.Parse
import Coinbene.Request
import Coins

import Network.HTTP.Client          (newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)

import Debug.Trace

instance IsOption API_ID where
    defaultValue = error "User must supply API ID (on command line or environment) for authenticated tests."
    parseValue = Just . API_ID
    optionName = return "API_ID"
    optionHelp = return "Customer's API ID for Coinbene account (hex encoded)."

instance IsOption API_KEY where
    defaultValue = error "User must supply API secret key (on command line or environment) for authenticated tests."
    parseValue = Just . API_KEY
    optionName = return "API_KEY"
    optionHelp = return "Customer's API secret key for Coinbene account (hex encoded)."


main = defaultMainWithIngredients ings $
    askOption $ \apikey ->
    askOption $ \apiid ->
    withResource (mkConfig apiid apikey) (\_ -> return ()) tests
  where
    ings = includingOptions
        [ (Option (Proxy :: Proxy API_ID))
        , (Option (Proxy :: Proxy API_KEY))
        ] : defaultIngredients

    mkConfig apiid apikey = do
        manager <- newManager tlsManagerSettings
        return $ Coinbene manager apiid apikey Silent


---------------------------------------
tests :: IO Coinbene -> TestTree
tests config = testGroup ("\nAPI test cases for " <> MACRO_MARKET_NAME <> ". For other currencies, change compiler flag.")
  [ testCase "benchmark orderbook parsing" $ do
        let mSampleResp = decode encodedSampleResponse
        mSampleResp @?= Just sampleResponse

  , testCase "benchmark \"failed response\" parsing" $ do
        let mSampleError = decode encodedsampleRespError
        mSampleError @?= Just sampleRespError

  , testCase "live - orderbook parsing" $ do
        coinbene <- config
        book <- getBook coinbene (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol MACRO_CURRENCY))
        assertBool (show book) $ qbAsks book /= [] && qbBids book /= []

  , testCase "live - place limit" $ do
        coinbene <- config
        oid <- placeLimit coinbene Ask (Price 9997999 :: Price BRL) (Vol MACRO_TEST_VOL :: Vol MACRO_CURRENCY)
        assertBool (show oid) $ oid /= OrderID "2018" -- forces evaluation

  , testCase "live - place and get order info" $ do
        coinbene <- config
        oid <- placeLimit coinbene Ask (Price 9997999 :: Price BRL) (Vol MACRO_TEST_VOL :: Vol MACRO_CURRENCY)
        info <- getOrderInfo coinbene oid
        assertBool ("placed: " ++ show oid ++ "\n got: " ++ show info) $ oid == (orderID info)

  , testCase "live - place and cancel order" $ do
        coinbene <- config
        oid  <- placeLimit coinbene Ask (Price 9999999 :: Price BRL) (Vol MACRO_TEST_VOL :: Vol MACRO_CURRENCY)
        oid' <- cancel coinbene oid
        assertBool ("placed: " ++ show oid ++ " canceled: " ++ show oid') $ oid == oid'

  , testCase "DISABLED - live - try to cancel executed order" $ do
        -- Careful! Adjust price before running
        -- coinbene <- config
        -- oida <- placeLimit coinbene Ask (Price 700 :: Price BRL) (Vol MACRO_TEST_VOL :: Vol MACRO_CURRENCY)
        -- oidb <- placeLimit coinbene Bid (Price 700 :: Price BRL) (Vol MACRO_TEST_VOL :: Vol MACRO_CURRENCY)
        -- oid' <- cancel coinbene oida
        -- assertBool ("placed: " ++ show oida ++ " canceled: " ++ show oid') $ (oid' == oida)
        return ()

  , testCase "live - place order then get open orders" $ do
        coinbene <- config
        oid <- placeLimit coinbene Ask (Price 9999999 :: Price BRL) (Vol MACRO_TEST_VOL :: Vol MACRO_CURRENCY)
        infos <- getOpenOrders coinbene (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol MACRO_CURRENCY))
        -- must force oid
        assertBool ("Placed order " ++ show oid ++ " but received empty open order list: " ++ show infos) $ oid /= (OrderID "") && infos /= []

  , testCase "live - get balances" $ do
        coinbene <- config
        bals <- getBalances coinbene
        assertBool ("No non-zero balances found: " ++ show bals) $ any (>0) (map biTotal bals)

  , testCase "live - get trades" $ do
        coinbene <- config
        trades <- getTrades coinbene (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol MACRO_CURRENCY))
        assertBool ("Less than 3 trades found: " ++ show trades) $ length trades > 2 && tPrice (head trades) > 0

  , testCase "live - get futures account info" $ do
        coinbene <- config
        accInfo <- getAccInfo coinbene
        assertBool ("Zero total balance found: " ++ show accInfo) $ totalBal accInfo > 0

  ]

---------------------------------------
-- Benchmark orderbook parsing test
sampleResponse :: Resp (QuoteBookPayload BRL MACRO_CURRENCY)
sampleResponse = RespOK
    { rPayload = BookPayload
      { bpOrderbook = QuoteBook
          { qbBids =
              [ BidQ {bqPrice = Price 14770.00, bqQuantity = Vol 0.49930000}
              , BidQ {bqPrice = Price 14750.00, bqQuantity = Vol 0.03130000}
              , BidQ {bqPrice = Price 14553.16, bqQuantity = Vol 0.00020000}
              ]
          , qbAsks =
              [ AskQ {aqPrice = Price 15556.57, aqQuantity = Vol 0.00280000}
              , AskQ {aqPrice = Price 16550.00, aqQuantity = Vol 0.19210000}
              , AskQ {aqPrice = Price 24299.99, aqQuantity = Vol 0.02470000}
              , AskQ {aqPrice = Price 28100.00, aqQuantity = Vol 0.00070000}
              ]
          }
      , bpSymbol = MACRO_MARKET_NAME
      }
    , rTimestamp = MilliEpoch 1544017051349
    }

encodedSampleResponse = "{\"symbol\":\"" <> MACRO_MARKET_NAME <> "\",\"orderbook\":{\"bids\":[{\"quantity\":0.4993,\"price\":14770},{\"quantity\":0.0313,\"price\":14750},{\"quantity\":0.0002,\"price\":14553.16}],\"asks\":[{\"quantity\":0.0028,\"price\":15556.57},{\"quantity\":0.1921,\"price\":16550},{\"quantity\":0.024700000000000000,\"price\":24299.99},{\"quantity\":0.000700000000000000,\"price\":28100}]},\"status\":\"ok\",\"timestamp\":1544017051349}"

---------------------------------------
-- Error response parsing test
sampleRespError :: Resp (QuoteBookPayload BRL MACRO_CURRENCY)
sampleRespError = RespError
    { rDescription = "some error"
    , rTimestamp   = 1234
    }

encodedsampleRespError = "{\"description\":\"some error\",\"status\":\"error\",\"timestamp\":1234}"

---------------------------------------
sampleAsk :: AskQuote BRL MACRO_CURRENCY
sampleAsk = AskQ { aqPrice = 20000, aqQuantity = 3}

sampleBid :: BidQuote BRL MACRO_CURRENCY
sampleBid = BidQ { bqPrice =  5000, bqQuantity = 2}

sampleBook :: QuoteBook BRL MACRO_CURRENCY
sampleBook = QuoteBook
    { qbAsks = [sampleAsk]
    , qbBids = [sampleBid,sampleBid]
    }


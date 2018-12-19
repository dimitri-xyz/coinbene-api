{-# LANGUAGE OverloadedStrings #-}

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
        return $ Coinbene manager apiid apikey


---------------------------------------
tests :: IO Coinbene -> TestTree
tests config = testGroup "\nAPI test cases"
  [ testCase "benchmark orderbook parsing" $ do
        let mSampleResp = decode encodedSampleResponse
        mSampleResp @?= Just sampleResponse

  , testCase "benchmark \"failed response\" parsing" $ do
        let mSampleError = decode encodedsampleRespError
        mSampleError @?= Just sampleRespError

  , testCase "live - orderbook parsing" $ do
        coinbene <- config
        book <- getBook coinbene (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol BTC))
        assertBool (show book) $ qbAsks book /= [] && qbBids book /= []

  , testCase "live - place limit" $ do
        coinbene <- config
        oid <- placeLimit coinbene Ask (Price 9997999 :: Price BRL) (Vol 0.001 :: Vol BTC)
        assertBool (show oid) $ oid /= OrderID "2018" -- forces evaluation

  , testCase "live - place and get order info" $ do
        coinbene <- config
        oid <- placeLimit coinbene Ask (Price 9997999 :: Price BRL) (Vol 0.001 :: Vol BTC)
        info <- getOrderInfo coinbene oid
        assertBool ("placed: " ++ show oid ++ "\n got: " ++ show info) $ oid == (orderID info) 

  , testCase "live - place and cancel order" $ do
        coinbene <- config
        oid  <- placeLimit coinbene Ask (Price 9999999 :: Price BRL) (Vol 0.001 :: Vol BTC)
        oid' <- cancel coinbene oid
        assertBool ("placed: " ++ show oid ++ " canceled: " ++ show oid') $ oid == oid'

  , testCase "live - place order then get open orders" $ do
        coinbene <- config
        oid <- placeLimit coinbene Ask (Price 9999999 :: Price BRL) (Vol 0.001 :: Vol BTC)
        infos <- getOpenOrders coinbene (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol BTC))
        -- must force oid
        assertBool ("Placed order " ++ show oid ++ " but received empty open order list: " ++ show infos) $ oid /= (OrderID "") && infos /= []

  , testCase "live - get balances" $ do
        coinbene <- config
        bals <- getBalances coinbene
        assertBool ("No non-zero balances found: " ++ show bals) $ any (>0) (map biTotal bals)

  , testCase "live - get trades" $ do
        coinbene <- config
        trades <- getTrades coinbene (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol BTC))
        assertBool ("Less than 3 trades found: " ++ show trades) $ length trades > 2 && tPrice (head trades) > 0
  ]

---------------------------------------
-- Benchmark orderbook parsing test
sampleResponse :: Resp (QuoteBookPayload BRL BTC)
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
      , bpSymbol = "BTCBRL"
      }
    , rTimestamp = MilliEpoch 1544017051349
    }

encodedSampleResponse = "{\"symbol\":\"BTCBRL\",\"orderbook\":{\"bids\":[{\"quantity\":0.4993,\"price\":14770},{\"quantity\":0.0313,\"price\":14750},{\"quantity\":0.0002,\"price\":14553.16}],\"asks\":[{\"quantity\":0.0028,\"price\":15556.57},{\"quantity\":0.1921,\"price\":16550},{\"quantity\":0.024700000000000000,\"price\":24299.99},{\"quantity\":0.000700000000000000,\"price\":28100}]},\"status\":\"ok\",\"timestamp\":1544017051349}"

---------------------------------------
-- Error response parsing test
sampleRespError :: Resp (QuoteBookPayload BRL BTC)
sampleRespError = RespError
    { rDescription = "some error"
    , rTimestamp   = 1234
    } 

encodedsampleRespError = "{\"description\":\"some error\",\"status\":\"error\",\"timestamp\":1234}"

---------------------------------------
sampleAsk :: AskQuote BRL BTC
sampleAsk = AskQ { aqPrice = 20000, aqQuantity = 3}

sampleBid :: BidQuote BRL BTC
sampleBid = BidQ { bqPrice =  5000, bqQuantity = 2}

sampleBook :: QuoteBook BRL BTC
sampleBook = QuoteBook 
    { qbAsks = [sampleAsk]
    , qbBids = [sampleBid,sampleBid]
    } 


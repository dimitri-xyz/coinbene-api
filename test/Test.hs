{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Coinbene
import Coinbene.Parse
import Coinbene.Request
import Coins

import Network.HTTP.Client          (newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)

main = defaultMain tests

tests :: TestTree
tests = testGroup "\nAPI test cases"
  [ testCase "benchmark orderbook parsing" $ do
        let mSampleResp = decode encodedSampleResponse
        mSampleResp @?= Just sampleResponse

  , testCase "benchmark \"failed response\" parsing" $ do
        let mSampleError = decode encodedsampleRespError
        mSampleError @?= Just sampleRespError

  , testCase "live BTCBRL orderbook parsing" $ do
        manager  <- newManager tlsManagerSettings
        let coinbene = Coinbene manager undefined undefined
        book <- getBook coinbene (undefined :: Price BRL) (undefined :: Vol BTC)
        assertBool (show book) $ False

  , testCase "live BTCBRL place limit" $ do
        manager  <- newManager tlsManagerSettings
        let coinbene = Coinbene manager (API_ID "deadbeef1234") (API_KEY "adda54321")
        oid <- placeLimit coinbene Ask (Price 9999999 :: Price BRL) (Vol 0.001 :: Vol BTC)
        assertBool (show oid) $ False

  , testCase "live get order info" $ do
        manager  <- newManager tlsManagerSettings
        let coinbene = Coinbene manager (API_ID "deadbeef1234") (API_KEY "adda54321")
        info <- getOrderInfo coinbene (OrderID "201812150426145280027052") -- "201812140948258120019452"
        assertBool (show info) $ False
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


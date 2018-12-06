{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Market.Types                         hiding (QuoteBook)
import Data.ByteString.Lazy.Char8 (unpack)
import Coinbene

main = defaultMain $
    testCase "\nExample test case" $ do

        (code, resp) <- getSecureURL (URL "https://api.coinbene.com/v1/market/orderbook?symbol=btcbrl&depth=200")
        -- assertBool (show (code, response)) $ False

        -- let mBRL = decode ("12345")
        -- mBRL @?= Just (Price 12345 :: Price BRL)

        let mResp = decode encodedResponse
        mResp @?= Just decodedResponse


---------------------------------------
aq :: AskQuote BRL BTC
aq = AskQ { aqPrice = 20000, aqQuantity = 3}

bq :: BidQuote BRL BTC
bq = BidQ { bqPrice =  5000, bqQuantity = 2}


book :: QuoteBook BRL BTC
book = QuoteBook 
    { qbAsks = [aq]
    , qbBids = [bq,bq]
    } 

response :: Resp QuoteBookPayload BRL BTC 
response = RespOK
    { rPayload = BookPayload book "ETHUSDT"
    , rTimestamp = 1234
    }

encodedResponse = "{\"symbol\":\"BTCBRL\",\"orderbook\":{\"bids\":[{\"quantity\":0.4993,\"price\":14770},{\"quantity\":0.0313,\"price\":14750},{\"quantity\":0.0002,\"price\":14553.16},{\"quantity\":0.0114,\"price\":14500.01},{\"quantity\":0.0105,\"price\":14253.15},{\"quantity\":10.000000000000000000,\"price\":1000},{\"quantity\":0.065600000000000000,\"price\":2.36}],\"asks\":[{\"quantity\":0.0028,\"price\":15556.57},{\"quantity\":0.0004,\"price\":15570},{\"quantity\":0.1921,\"price\":16550},{\"quantity\":0.5060,\"price\":17000},{\"quantity\":1,\"price\":17350},{\"quantity\":1,\"price\":17700},{\"quantity\":1.866900000000000000,\"price\":21350},{\"quantity\":1.000000000000000000,\"price\":22776.29},{\"quantity\":0.024700000000000000,\"price\":24299.99},{\"quantity\":0.400000000000000000,\"price\":24350},{\"quantity\":0.014500000000000000,\"price\":24750},{\"quantity\":0.002400000000000000,\"price\":25000},{\"quantity\":0.38,\"price\":25200},{\"quantity\":0.000700000000000000,\"price\":28100}]},\"status\":\"ok\",\"timestamp\":1544017051349}"

  --  "{\"symbol\":\"BTCBRL\"
  --   ,\"orderbook\":
  --       {\"bids\":
  --           [{\"quantity\":0.4993,\"price\":14770}
  --           ,{\"quantity\":0.0313,\"price\":14750}
  --           ,{\"quantity\":0.0002,\"price\":14553.16}
  --           ,{\"quantity\":0.0114,\"price\":14500.01}
  --           ,{\"quantity\":0.0105,\"price\":14253.15}
  --           ,{\"quantity\":10.000000000000000000,\"price\":1000}
  --           ,{\"quantity\":0.065600000000000000,\"price\":2.36}
  --           ]
  --       ,\"asks\":
  --           [{\"quantity\":0.0028,\"price\":15556.57}
  --           ,{\"quantity\":0.0004,\"price\":15570}
  --           ,{\"quantity\":0.1921,\"price\":16550}
  --           ,{\"quantity\":0.5060,\"price\":17000}
  --           ,{\"quantity\":1,\"price\":17350}
  --           ,{\"quantity\":1,\"price\":17700}
  --           ,{\"quantity\":1.866900000000000000,\"price\":21350}
  --           ,{\"quantity\":1.000000000000000000,\"price\":22776.29}
  --           ,{\"quantity\":0.024700000000000000,\"price\":24299.99}
  --           ,{\"quantity\":0.400000000000000000,\"price\":24350}
  --           ,{\"quantity\":0.014500000000000000,\"price\":24750}
  --           ,{\"quantity\":0.002400000000000000,\"price\":25000}
  --           ,{\"quantity\":0.38,\"price\":25200}
  --           ,{\"quantity\":0.000700000000000000,\"price\":28100}
  --           ]
  --       }
  --   ,\"status\":\"ok\"
  --   ,\"timestamp\":1544017051349
  --   }"

decodedResponse :: Resp QuoteBookPayload BRL BTC
decodedResponse = RespOK 
    { rPayload = BookPayload 
      { bpOrderbook = QuoteBook
          { qbBids = 
              [ BidQ {bqPrice = Price 14770.00000, bqQuantity = Vol 0.49930000}
              , BidQ {bqPrice = Price 14750.00000, bqQuantity = Vol 0.03130000}
              , BidQ {bqPrice = Price 14553.15999, bqQuantity = Vol 0.00020000} -- 
              , BidQ {bqPrice = Price 14500.01000, bqQuantity = Vol 0.01140000}
              , BidQ {bqPrice = Price 14253.14999, bqQuantity = Vol 0.01050000} -- 
              , BidQ {bqPrice = Price 1000.00000, bqQuantity = Vol 10.00000000}
              , BidQ {bqPrice = Price 2.35999,     bqQuantity = Vol 0.06560000} --
              ]
          , qbAsks = 
              [ AskQ {aqPrice = Price 15556.56999, aqQuantity = Vol 0.00279999} -- --
              , AskQ {aqPrice = Price 15570.00000, aqQuantity = Vol 0.00040000}
              , AskQ {aqPrice = Price 16550.00000, aqQuantity = Vol 0.19209999}    --
              , AskQ {aqPrice = Price 17000.00000, aqQuantity = Vol 0.50600000}
              , AskQ {aqPrice = Price 17350.00000, aqQuantity = Vol 1.00000000}
              , AskQ {aqPrice = Price 17700.00000, aqQuantity = Vol 1.00000000}
              , AskQ {aqPrice = Price 21350.00000, aqQuantity = Vol 1.86690000}
              , AskQ {aqPrice = Price 22776.29000, aqQuantity = Vol 1.00000000}
              , AskQ {aqPrice = Price 24299.99000, aqQuantity = Vol 0.02469999}     --
              , AskQ {aqPrice = Price 24350.00000, aqQuantity = Vol 0.40000000}
              , AskQ {aqPrice = Price 24750.00000, aqQuantity = Vol 0.01450000}
              , AskQ {aqPrice = Price 25000.00000, aqQuantity = Vol 0.00239999}     --
              , AskQ {aqPrice = Price 25200.00000, aqQuantity = Vol 0.38000000}
              , AskQ {aqPrice = Price 28100.00000, aqQuantity = Vol 0.00069999}     --
              ]
          }
      , bpSymbol = "BTCBRL"
      }
  , rTimestamp = MilliEpoch 1544017051349
  }
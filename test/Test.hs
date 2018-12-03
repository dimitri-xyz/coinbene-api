import Test.Tasty
import Test.Tasty.HUnit

import Coinbene

main = defaultMain $
  testCase "\nExample test case" $ do
    -- assertion no. 1 (passes)
    2 + 2 @?= 4

    -- assertion no. 2 (fails)
    (code, response) <- getSecureURL (URL "https://api.coinbene.com/v1/market/orderbook?symbol=btcbrl&depth=200")
    assertBool (show (code, response)) $ False

    -- assertion no. 3 (would have failed, but won't be executed because
    -- the previous assertion has already failed)
    "foo" @?= "bar"

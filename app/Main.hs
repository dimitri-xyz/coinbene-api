{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent           (threadDelay)
import Control.Concurrent.Async     (async, link)
import Control.Monad                (forever)
import Data.Foldable                (foldl')
import Data.Proxy                   (Proxy(..))
import Text.Printf                  (printf)

import Network.HTTP.Client          (newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)

import Coinbene

main :: IO ()
main = do

    putStrLn "--------------------------- Starting --------------------------------"
    putStrLn "Type <ENTER> to quit"
    putStrLn "---------------------------------------------------------------------"
    putStrLn "\n"

    manager  <- newManager tlsManagerSettings
    let coinbene = Coinbene manager undefined undefined
        lines    = 5

    -- start execution thread
    thread <- async $ forever $ do
        book <- getBook coinbene (Proxy :: Proxy (Price USDT)) (Proxy :: Proxy (Vol BTC))
        putStr $ backtrackCursor $ showTopN lines book
        threadDelay 1500000
  
    -- propagate exceptions to this thread
    link thread
    
    -- run until users presses <ENTER> key
    keyboardWait 

    -- move screen cursor past last update
    putStrLn $ take (2 * lines + 3) $ repeat '\n' 



--------------------------------------------------------------------------------
keyboardWait :: IO ()
keyboardWait = getLine >> return () 

-- | Show top N bids and asks
showTopN :: forall p v. (Coin p, Coin v, Show p, Show v) => Int -> QuoteBook p v -> String
showTopN n book 
    =  coinSymbol (Proxy :: Proxy v) ++ coinSymbol (Proxy :: Proxy p) ++ " market:\n"
    ++ "--------------\n\n"
    ++ concatMap formatRow asks'
    ++ "   ---      --------------------      --------------------\n"
    ++ concatMap formatRow bids'
  where
    bids' = fmap (\b -> ("  BID", bqPrice b, bqQuantity b))           $ take n $ qbBids book
    asks' = fmap (\a -> ("  ASK", aqPrice a, aqQuantity a)) $ reverse $ take n $ qbAsks book
    formatRow :: (Show p, Show v) => (String, Price p, Vol v) -> String
    formatRow (s, p, v) = printf " %s    %22s    %22s                 \n" s (show v) (show p)


-- | (on ANSI terminals) Backtracks cursor as many lines as the string has
backtrackCursor :: String -> String
backtrackCursor ss =
    let newLines = foldl' (\count char -> if char == '\n' then (count + 1) else count) (0::Int) ss
     in ss ++ "\ESC[" ++ show newLines ++ "A"

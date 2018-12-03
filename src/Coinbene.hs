{-# LANGUAGE OverloadedStrings #-}

module Coinbene where

import Network.HTTP.Client
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Network.HTTP.Types.Status    (statusCode)
import Data.ByteString.Lazy.Char8   (unpack)

newtype URL = URL {urlToString :: String} deriving (Show,Eq)

-----------------------------------------
-- Just Gets an httpS URL
-- returns ( Response Code, Response Body)
--
getSecureURL :: URL -> IO (Int , String)
getSecureURL url = do
    req      <- parseUrlThrow (urlToString url)
    manager  <- newManager tlsManagerSettings
    response <- httpLbs (req {secure = True}) manager
    return ( statusCode $ responseStatus response, unpack (responseBody response) )
-----------------------------------------


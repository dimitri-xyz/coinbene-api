-- {-# LANGUAGE OverloadedStrings #-}
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import           Network.HTTP.Client        (defaultManagerSettings, newManager)
-- import           Network.HTTP.Client.TLS    (tlsManagerSettings)
-- import           Network.HTTP.Simple

-- main :: IO ()
-- main = do
--     manager <- newManager tlsManagerSettings

--     let request = setRequestManager manager "https://httpbin.org/get"
--     response <- httpLBS request

--     putStrLn $ "The status code was: " ++
--                show (getResponseStatusCode response)
--     print $ getResponseHeader "Content-Type" response
--     L8.putStrLn $ getResponseBody response





{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Network.HTTP.Client

main :: IO ()
main = do
    request' <- parseRequest "POST http://httpbin.org/post"
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/post"
            $ setRequestQueryString [("hello", Just "world"),("this is", Just "Nice!")]
            $ setRequestBodyLBS "This is my request body"
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    putStrLn $ show request
    putStrLn $ S8.unpack $ (queryString request)
    response <- httpJSON request
    putStrLn $ show response

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)


-- module Main where

-- import Coinbene

-- main :: IO ()
-- main = putStrLn "Hi!"


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coins where

import GHC.Generics
import Data.Scientific
import Data.Aeson

import Coinbene (CoinSymbol, coinSymbol)

-------------------
newtype BTC  = BTC  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype BRL  = BRL  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype LTC  = LTC  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype ETH  = ETH  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
newtype USDT = USDT Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

-------------------
instance FromJSON BTC
instance ToJSON   BTC

instance FromJSON BRL
instance ToJSON   BRL

instance FromJSON LTC
instance ToJSON   LTC

instance FromJSON ETH
instance ToJSON   ETH

instance FromJSON USDT
instance ToJSON   USDT

-------------------
instance CoinSymbol BTC where
  coinSymbol _ = "BTC"
instance CoinSymbol BRL where
  coinSymbol _ = "BRL"
instance CoinSymbol LTC where
  coinSymbol _ = "LTC"
instance CoinSymbol ETH where
  coinSymbol _ = "ETH"
instance CoinSymbol USDT where
  coinSymbol _ = "USDT"

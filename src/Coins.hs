{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coins where

import GHC.Generics
import Data.Scientific
import Data.Aeson

import Coinbene (Coin, coinSymbol, showBare)

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
instance Coin BTC where
  coinSymbol _     = "BTC"
  showBare (BTC x) = formatScientific Fixed (Just 8) x

instance Coin BRL where
  coinSymbol _     = "BRL"
  showBare (BRL x) = formatScientific Fixed (Just 8) x -- arbitrary, could be just 2

instance Coin LTC where
  coinSymbol _     = "LTC"
  showBare (LTC x) = formatScientific Fixed (Just 8) x

instance Coin ETH where
  coinSymbol _     = "ETH"
  showBare (ETH x) = formatScientific Fixed (Just 18) x

instance Coin USDT where
  coinSymbol _      = "USDT"
  showBare (USDT x) = formatScientific Fixed (Just 8) x

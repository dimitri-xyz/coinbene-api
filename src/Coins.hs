{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coins where

import GHC.Generics
import Data.Scientific
import Data.Aeson

import Coinbene.Core (Coin, coinSymbol, showBare, readBare)

-------------------
newtype BTC  = BTC  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic, FromJSON)
newtype BRL  = BRL  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic, FromJSON)
newtype LTC  = LTC  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic, FromJSON)
newtype ETH  = ETH  Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic, FromJSON)
newtype USDT = USDT Scientific deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic, FromJSON)

-------------------
instance Coin BTC where
  coinSymbol _     = "BTC"
  showBare (BTC x) = formatScientific Fixed (Just 8) x
  readBare         = BTC . read

instance Coin BRL where
  coinSymbol _     = "BRL"
  showBare (BRL x) = formatScientific Fixed (Just 8) x -- arbitrary, could be just 2
  readBare         = BRL . read

instance Coin LTC where
  coinSymbol _     = "LTC"
  showBare (LTC x) = formatScientific Fixed (Just 8) x
  readBare         = LTC . read

instance Coin ETH where
  coinSymbol _     = "ETH"
  showBare (ETH x) = formatScientific Fixed (Just 18) x
  readBare         = ETH . read

instance Coin USDT where
  coinSymbol _      = "USDT"
  showBare (USDT x) = formatScientific Fixed (Just 8) x
  readBare          = USDT . read

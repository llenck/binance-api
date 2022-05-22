{-# LANGUAGE OverloadedStrings #-}

{-|
License    : AGPL
Maintainer : lialenck@protonmail.com, github.com/llenck
Stability  : Idk lmao

Contains a few useful functions for fetching data from the Binance API.

The main things to look at are 'getRates' and 'getRatesMap' for Exchange rates,
and 'getAssets' and 'getOrders' for information about your account (these
require a secret key).

All the functions that require both keys take the secret key first, and the
regular key second.

This Module makes heavy use of 'Rational's for representing numbers given by
the binance API. If you're gonna do a lot of computations on these numbers,
converting them to a floating-point type using `fromRational` is recommended.
-}

module Binance.Api (
    CoinInfo(..),
    getAssets,
    coinSum,

    ExchangeRate(..),
    getRates,
    getRatesMap,

    OrderInfo(..),
    getOrders
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Exception (catch)
import Data.List (foldl')
import Numeric (readSigned, readFloat)
import Data.Bifunctor (first, second)
import Data.Function ((&))
import qualified Data.HashMap.Strict as M

import Data.ByteString.Lazy (fromStrict)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Data.Aeson
import Network.HTTP.Simple (getResponseBody, HttpException)

import Binance.Network
import Data.ByteString (ByteString)

readRational :: MonadFail m => String -> m Rational
readRational s = case readSigned readFloat $ s of
        (f, _):_ -> return f
        _ -> fail $ "Can't parse rational"

-- |Information about the amount of a coin that you posess.
--
-- 'coinFree' is what you can directly use, and 'coinLocked' is the amount reserved
-- for orders. I don't know what 'coinFrozen' does.
--
-- To get the total amount, you can use 'coinSum'.
data CoinInfo = CoinInfo {
    coinTicker :: String,
    coinFree :: Rational,
    coinFrozen :: Rational,
    coinLocked :: Rational,
    coinName :: String
} deriving (Show)
instance FromJSON CoinInfo where
    parseJSON = withObject "CoinInfo" $ \v ->
        CoinInfo <$>
        v .: "coin" <*>
        (v .: "free" >>= readRational) <*>
        (v .: "freeze" >>= readRational) <*>
        (v .: "locked" >>= readRational) <*>
        v .: "name"

-- |Sums up 'coinFree', 'coinFrozen' and 'coinLocked' for a 'CoinInfo'
coinSum coin = sum $ map ($ coin) [coinFree, coinFrozen, coinLocked]

getMsTimestamp = round . (*1000) <$> getPOSIXTime

-- |Uses your secret key and regular API key to fetch your assets:
--
-- >>> import Data.ByteString.Char8 (pack)
-- >>> getAssets (pack "private API key") (pack "regular API key")
-- Right [CoinInfo {coinTicker = "ETH", coinFree = 1 % 2, coinFree = 0 % 1, coinLocked = 0 % 1, coinName = "Ethereum"}]
--
-- This only fetches assets from your spot account, as the API for staked assets got added
-- very recently, and I haven't implemented that API yet.
getAssets :: ByteString -> ByteString -> IO (Either String [CoinInfo])
getAssets seckey apikey = catch go h
    where h :: HttpException -> IO (Either String a)
          h e = return $ Left $ "IO error fetching exchange rates: " <> show e
          go = do
              ms_time <- getMsTimestamp
              let q = "timestamp=" <> show ms_time
              bs <- getResponseBody <$> signedQuery seckey apikey "/sapi/v1/capital/config/getall" q

              return $ eitherDecode (fromStrict bs)
                  & first (\s -> s <> ": " <> show bs) -- append json to error
                  & second (filter ((> 0) . coinSum)) -- filter 0-balances

data ExchangeRate = ExchangeRate {
    exSymbol :: String,
    exRate :: Rational
} deriving (Show)
instance FromJSON ExchangeRate where
    parseJSON = withObject "ExchangeRate" $ \v ->
        ExchangeRate <$>
        v .: "symbol" <*>
        (v .: "price" >>= readRational)

-- |This fetches all the exchange rates, and returns them in a List. If you just want
-- to look up specific exchange rates, 'getRatesMap' is probably a better match for you.
getRates :: ByteString -> IO (Either String [ExchangeRate])
getRates apikey = catch go h
    where h :: HttpException -> IO (Either String a)
          h e = return $ Left $ "IO error fetching exchange rates: " <> show e
          go = do
              bs <- getResponseBody <$> query apikey "/api/v3/ticker/price" Nothing
              return $ eitherDecode (fromStrict bs)
                  & first (\s -> s <> ": " <> show bs) -- append json to error

lookupEither k m = case M.lookup k m of
    Nothing -> Left "No such key"
    Just v -> Right v

lookupRate m from to =
    (guard (from == to) >> Right 1)
    <|> lookupSingle from to
    <|> (*) <$> lookupSingle from "USDT" <*> lookupSingle "USDT" to
    <|> Left ("Couldn't convert " <> from <> " -> " <> to)
    where lookupSingle from to =
              lookupEither (from <> to) m <|> (1/) <$> lookupEither (to <> from) m

ratesToMap = foldl' (\m r -> M.insert (exSymbol r) (exRate r) m) M.empty

-- |This fetches all the exchange rates on binance, and returns a function which
-- you can use to look up pretty much all pairs of coins (if there's not an
-- entry for a pair, the function will try to go though USDT before failing).
--
-- This builds a strict 'HashMap' from 'getRates', and partially applies a lookup
-- function that tries to indirectly look exchange rates up.
--
-- >>> import Data.ByteString.Char8 (pack)
-- >>> (Right getRate) <- getRatesMap $ pack "regular API key"
-- >>> fromRational <$> getRate "ETH" "USDT"
-- Right 2925.42
-- >>> fromRational <$> lookupRate "Blub" "Wubbel"
-- Left "Couldn't convert Blub -> Wubbel"
getRatesMap :: ByteString -> IO (Either String (String -> String -> Either String Rational))
getRatesMap apikey = fmap (lookupRate . ratesToMap) <$> getRates apikey

-- a lot of fields are left out, as I don't need them myself, but message me if you
-- want to do more with this library

-- |Represents an open order. For the meaning of non-self-explanatory fields see
-- the binance API documentation.
data OrderInfo = OrderInfo {
    ordSymbol :: String,
    ordPrice :: Rational,
    ordOrigQty :: Rational,
    ordExecutedQty :: Rational,
    ordStatus :: String, -- this should be an enum, but idk about possible Values
    ordType :: String, -- same for this field.
    ordSide :: String,
    ordTime :: UTCTime
} deriving (Show)
instance FromJSON OrderInfo where
    parseJSON = withObject "OrderInfo" $ \v ->
        OrderInfo <$>
        v .: "symbol" <*>
        (v .: "price" >>= readRational) <*>
        (v .: "origQty" >>= readRational) <*>
        (v .: "executedQty" >>= readRational) <*>
        v .: "status" <*>
        v .: "type" <*>
        v .: "side" <*>
        (posixSecondsToUTCTime . fromInteger . (`div` 1000) <$> v .: "time")

-- |Uses your secret key and regular API key to fetch your open orders:
--
-- >>> import Data.ByteString.Char8 (pack)
-- >>> getOrders (pack "private API key") (pack "regular API key")
-- Right [OrderInfo {ordSymbol = "ALGOUSDT", ordPrice = 1 % 2, ordOrigQty = 22 % 1, ordExecutedQty = 0 % 1, ordStatus = "NEW", ordType = "LIMIT", ordSide = "SELL", ordTime = 2022-05-18 08:32:14 UTC}]
getOrders :: ByteString -> ByteString -> IO (Either String [OrderInfo])
getOrders seckey apikey = catch go h
    where h :: HttpException -> IO (Either String a)
          h e = return $ Left $ "IO error fetching orders: " <> show e
          go = do
              ms_time <- getMsTimestamp
              let q = "timestamp=" <> show ms_time
              bs <- getResponseBody <$> signedQuery seckey apikey "/api/v3/openOrders" q
              return $ eitherDecode (fromStrict bs)
                  & first (\s -> s <> ": " <> show bs) -- append json to error

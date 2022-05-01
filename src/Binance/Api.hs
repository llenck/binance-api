{-# LANGUAGE OverloadedStrings #-}

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

coinSum coin = sum $ map ($ coin) [coinFree, coinFrozen, coinLocked]

getMsTimestamp = round . (*1000) <$> getPOSIXTime

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

getRatesMap :: ByteString -> IO (Either String (String -> String -> Either String Rational))
getRatesMap apikey = fmap (lookupRate . ratesToMap) <$> getRates apikey

-- a lot of fields are left out, as I don't need them myself, but message me if you
-- want to do more with this library
data OrderInfo = OrderInfo {
    ordSymbol :: String,
    ordPrice :: Rational,
    ordOrigQty :: Rational,
    ordExecutedQty :: Rational,
    ordStatus :: String,
    ordType :: String,
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

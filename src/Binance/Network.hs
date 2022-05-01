{-# LANGUAGE OverloadedStrings #-}

module Binance.Network (
    query,
    signedQuery
) where

import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Network.HTTP.Simple (parseRequest_, addRequestHeader, httpBS, Response)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)

signQuery seckey query =
    let digest = (hmac seckey (pack query) :: HMAC SHA256) in
    query <> "&signature=" <> (show $ hmacGetDigest digest)

query :: ByteString -> String -> Maybe String -> IO (Response ByteString)
query apikey api q =
    "https://api.binance.com" <> api <> case q of
        Nothing -> ""
        Just s -> '?' : s
    & parseRequest_
    & addRequestHeader "X-MBX-APIKEY" apikey
    & httpBS

signedQuery :: ByteString -> ByteString -> String -> String -> IO (Response ByteString)
signedQuery seckey apikey api q = query apikey api $ Just (signQuery seckey q)

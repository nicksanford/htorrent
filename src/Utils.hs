{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Crypto.Random (getRandomBytes)
import Data.List (unfoldr)
import Data.Maybe (isJust)
import Numeric (readHex)
import qualified System.Random as R
import qualified Crypto.Hash as C
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as UTF8

allowed :: BS.ByteString
allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZYZ0123456789.-_~"

alphaNums g = unfoldr f (randomIndexFromSeed g)
  where f (i, newG) = Just (BS.index alphaNumsList i, randomIndexFromSeed newG)
        randomIndexFromSeed = R.randomR (0, BS.length alphaNumsList - 1)

alphaNumsList :: BS.ByteString
alphaNumsList = "abcdefghijklmnopqrstuvwzyz0123456789"

escape :: BS.ByteString -> BS.ByteString
escape x = case fmap (\(a,b) -> (BS.singleton a, b)) (BS.uncons x) of
  Nothing -> x
  _ -> do
    let nextByte = BS.take 2 x
    let charOfByte = fst . B16.decode $ nextByte
    if isJust $ BS.findSubstring charOfByte allowed
      then BS.concat [charOfByte, escape $ BS.drop 2 x]
      else BS.concat ["%", nextByte, escape $ BS.drop 2 x]

getPeerID :: IO BS.ByteString
getPeerID = do
  g <- R.getStdGen
  return $ BS.concat ["-HT2940-",  BS.pack $ take 12 $ alphaNums g]

randomBytes :: Int -> IO BS.ByteString
randomBytes  = getRandomBytes

shaHash :: BS.ByteString -> BS.ByteString
shaHash = BS.pack . BS.unpack . (BA.convert . (BAE.convertToBase BAE.Base16 :: C.Digest C.SHA1 -> BS.ByteString) .  (C.hashWith C.SHA1 :: BS.ByteString -> C.Digest C.SHA1))

shaHashRaw :: BS.ByteString -> BS.ByteString
shaHashRaw = BS.pack . BS.unpack . (BA.convert .  (C.hashWith C.SHA1 :: BS.ByteString -> C.Digest C.SHA1))

unescape :: BS.ByteString -> BS.ByteString
unescape x = case fmap (\(a,b) -> (BS.singleton a, b)) (BS.uncons x) of
  Nothing -> x
  (Just ("%", rest)) -> BS.concat [BS.take 2 rest, unescape $ BS.drop 2 rest]
  -- figure out how to do the ord stuff
  (Just (_, rest)) -> BS.concat [BS.take 2 rest, unescape $ BS.drop 2 rest]

unhex :: BS.ByteString -> BS.ByteString
unhex x =
  BS.pack $ (fmap fromIntegral nums)
  where z = zip [0..] (UTF8.toString x)
        evens :: String
        evens = [snd tuple | tuple <- z, even $ fst tuple]
        odds :: String
        odds = [snd tuple | tuple <- z, odd $ fst tuple]
        func a b = fst . head . readHex $ [a,b]
        nums = zipWith func evens odds

-- def decode(x):
--     if not x:
--         return ''

--     if x[0] == '%':
--         return x[1:3] + decode(x[3:])
--     else: --         return format(ord(x[0]), '02x') + decode(x[1:])


--g <- R.getStdGen 
--


  

  
--alphaNums :: R.StdGen -> BS.ByteString


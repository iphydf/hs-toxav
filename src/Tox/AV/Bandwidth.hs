{-# LANGUAGE StrictData #-}
-- | How much of a friend's media got through, reported back to them.
module Tox.AV.Bandwidth
  ( Report (..)
  , packetId
  , encode
  , decode
  , loss
  , Meter
  , meter
  , received
  , lost
  , due
  , interval
  ) where

import           Data.Binary.Get         (getWord32be, getWord8, runGetOrFail)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import           Data.Word               (Word32, Word64, Word8)

-- | Bytes lost and received since the last report.
data Report = Report
  { reportLost     :: Word32
  , reportReceived :: Word32
  } deriving (Eq, Show)

packetId :: Word8
packetId = 196

encode :: Report -> ByteString
encode (Report l r) = LBS.toStrict . B.toLazyByteString $
  B.word8 packetId <> B.word32BE l <> B.word32BE r

-- | A packet, its id included.
decode :: ByteString -> Maybe Report
decode bytes
  | BS.length bytes /= 9 = Nothing
  | otherwise = case runGetOrFail get (LBS.fromStrict bytes) of
      Right (_, _, (ident, report)) | ident == packetId -> Just report
      _                                                 -> Nothing
  where
    get = (,) <$> getWord8 <*> (Report <$> getWord32be <*> getWord32be)

-- | Fraction of the bytes lost.
loss :: Report -> Double
loss (Report l r)
  | l == 0 = 0
  | otherwise = fromIntegral l / (fromIntegral l + fromIntegral r)

-- | What arrived since the last report.
data Meter = Meter
  { meterSince    :: Word64
  , meterLost     :: Word64
  , meterReceived :: Word64
  }

-- | Milliseconds between reports.
interval :: Word64
interval = 1000

meter :: Word64 -> Meter
meter now = Meter now 0 0

received, lost :: Int -> Meter -> Meter
received n m = m { meterReceived = meterReceived m + fromIntegral n }
lost n m = m { meterLost = meterLost m + fromIntegral n }

-- | Due once an interval has passed and media came, even without loss: that
-- tells the friend it may send more.
due :: Word64 -> Meter -> Maybe (Report, Meter)
due now m
  | now - meterSince m < interval || meterReceived m + meterLost m == 0 = Nothing
  | otherwise = Just (Report (clamp (meterLost m)) (clamp (meterReceived m)), meter now)
  where
    clamp = fromIntegral . min (fromIntegral (maxBound :: Word32))

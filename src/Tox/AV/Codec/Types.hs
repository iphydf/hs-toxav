{-# LANGUAGE StrictData #-}
-- | What the audio and video codecs share.
module Tox.AV.Codec.Types
  ( Pcm
  , AudioFrame (..)
  , Picture (..)
  , chromaSize
  , complete
  , fromI420
  , keyFrameInterval
  , keyFrame
  , Close (..)
  , managed
  ) where

import           Data.Bits            (shiftR, testBit, (.&.))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           Data.Int             (Int16)
import           Data.Ix              (inRange)
import qualified Data.Vector.Storable as V
import           Data.Word            (Word8)
import           Foreign.ForeignPtr   (ForeignPtr, newForeignPtr)
import           Foreign.Ptr          (FunPtr, Ptr, nullPtr)

import           Tox.AV.RTP           (Codec (..))

-- | Interleaved samples.
type Pcm = V.Vector Int16

data AudioFrame = AudioFrame
  { audioRate     :: Int
  , audioChannels :: Int
  , audioSamples  :: Pcm
  } deriving (Eq, Show)

-- | A picture in I420: full-size luma, then the two chroma planes at half
-- the width and height, rounded up, each without padding.
data Picture = Picture
  { pictureWidth  :: Int
  , pictureHeight :: Int
  , pictureY      :: ByteString
  , pictureU      :: ByteString
  , pictureV      :: ByteString
  } deriving (Eq, Show)

chromaSize :: Int -> Int
chromaSize n = (n + 1) `div` 2

-- | Whether its planes hold as much as its size says.
complete :: Picture -> Bool
complete (Picture w h y u v) =
  w > 0 && h > 0 && BS.length y >= w * h && all ((>= chromaSize w * chromaSize h) . BS.length) [u, v]

fromI420 :: Int -> Int -> ByteString -> Picture
fromI420 w h bytes = Picture w h y u v
  where
    (y, rest) = BS.splitAt (w * h) bytes
    (u, v) = BS.splitAt (chromaSize w * chromaSize h) rest

-- | Frames between key frames, for friends that don't ask for one when they
-- lose a frame, as C toxcore's clients can't.
keyFrameInterval :: Codec -> Int
keyFrameInterval codec = case codec of
  VP8 -> 40
  _   -> 180

-- | Whether a frame decodes without the ones before it.
keyFrame :: Codec -> ByteString -> Bool
keyFrame codec frame = case codec of
  VP8  -> maybe False (not . (`testBit` 0) . fst) (BS.uncons frame)
  H264 -> any ((== 5) . (.&. 0x1f)) (nalHeaders frame)
  H265 -> any (inRange (16, 23) . (.&. 0x3f) . (`shiftR` 1)) (nalHeaders frame)

-- | The first byte of each NAL unit in an Annex B stream.
nalHeaders :: ByteString -> [Word8]
nalHeaders frame = maybe [] (\(header, _) -> header : nalHeaders unit) (BS.uncons unit)
  where unit = BS.drop 3 . snd $ BS.breakSubstring (BS.pack [0, 0, 1]) frame

-- | Codecs are freed once nothing uses them, or straight away with 'close',
-- after which they must not be used.
class Close a where
  close :: a -> IO ()

managed :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO (Maybe (ForeignPtr a))
managed free ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> newForeignPtr free ptr

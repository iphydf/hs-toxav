{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE StrictData               #-}
-- | Opus through libopus.
module Tox.AV.Codec.Audio
  ( Pcm
  , AudioEncoder
  , audioEncoder
  , setAudioBitRate
  , encodeAudio
  , AudioDecoder
  , audioDecoder
  , decodeAudio
  , concealAudio
  , recoverAudio
  , packetChannels
  ) where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BS
import           Data.Int                     (Int16, Int32)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word                    (Word8)
import           Foreign.C.Types              (CInt (..))
import           Foreign.ForeignPtr           (ForeignPtr, finalizeForeignPtr,
                                               withForeignPtr)
import           Foreign.Marshal.Alloc        (alloca, allocaBytes)
import           Foreign.Ptr                  (FunPtr, Ptr, castPtr, nullPtr)

import           Tox.AV.Codec.Types           (Close (..), Pcm, managed)

data OpusEncoderStruct
data OpusDecoderStruct

-- opus_encode and opus_decode take long, so they are safe calls: an unsafe
-- one holds up every other Haskell thread until it returns.
foreign import ccall unsafe "hstoxav_opus_encoder_new"
  c_opus_encoder_new :: Int32 -> CInt -> Int32 -> IO (Ptr OpusEncoderStruct)
foreign import ccall unsafe "hstoxav_opus_encoder_set_bit_rate"
  c_opus_encoder_set_bit_rate :: Ptr OpusEncoderStruct -> Int32 -> IO CInt
foreign import ccall unsafe "&opus_encoder_destroy"
  c_opus_encoder_destroy :: FunPtr (Ptr OpusEncoderStruct -> IO ())
foreign import ccall safe "opus_encode"
  c_opus_encode :: Ptr OpusEncoderStruct -> Ptr Int16 -> CInt -> Ptr Word8 -> Int32 -> IO Int32
foreign import ccall unsafe "opus_decoder_create"
  c_opus_decoder_create :: Int32 -> CInt -> Ptr CInt -> IO (Ptr OpusDecoderStruct)
foreign import ccall unsafe "&opus_decoder_destroy"
  c_opus_decoder_destroy :: FunPtr (Ptr OpusDecoderStruct -> IO ())
foreign import ccall safe "opus_decode"
  c_opus_decode :: Ptr OpusDecoderStruct -> Ptr Word8 -> Int32 -> Ptr Int16 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "opus_packet_get_nb_channels"
  c_opus_packet_get_nb_channels :: Ptr Word8 -> IO CInt

instance Close AudioEncoder where
  close = finalizeForeignPtr . audioEncoderPtr

instance Close AudioDecoder where
  close = finalizeForeignPtr . audioDecoderPtr

data AudioEncoder = AudioEncoder
  { audioEncoderChannels :: Int
  , audioEncoderPtr      :: ForeignPtr OpusEncoderStruct
  }

-- | Bit rate in bits per second. Opus takes only some sampling rates, and 1
-- or 2 channels.
audioEncoder :: Int -> Int -> Int -> IO (Maybe AudioEncoder)
audioEncoder rate channels bitRate =
  fmap (AudioEncoder channels) <$>
    (managed c_opus_encoder_destroy =<< c_opus_encoder_new (fromIntegral rate) (fromIntegral channels) (fromIntegral bitRate))

setAudioBitRate :: AudioEncoder -> Int -> IO Bool
setAudioBitRate enc bitRate = withForeignPtr (audioEncoderPtr enc) $ \ptr ->
  (== 0) <$> c_opus_encoder_set_bit_rate ptr (fromIntegral bitRate)

maxOpusPacket :: Int
maxOpusPacket = 4000

-- | One frame of a duration Opus takes: 2.5 to 60 ms.
encodeAudio :: AudioEncoder -> Pcm -> IO (Maybe ByteString)
encodeAudio enc pcm = withForeignPtr (audioEncoderPtr enc) $ \ptr ->
  V.unsafeWith pcm $ \samples -> allocaBytes maxOpusPacket $ \out -> do
    let frame = V.length pcm `div` audioEncoderChannels enc
    n <- c_opus_encode ptr samples (fromIntegral frame) out (fromIntegral maxOpusPacket)
    if n < 0 then pure Nothing else Just <$> BS.packCStringLen (castPtr out, fromIntegral n)

data AudioDecoder = AudioDecoder
  { audioDecoderRate     :: Int
  , audioDecoderChannels :: Int
  , audioDecoderPtr      :: ForeignPtr OpusDecoderStruct
  }

audioDecoder :: Int -> Int -> IO (Maybe AudioDecoder)
audioDecoder rate channels = alloca $ \err ->
  fmap (AudioDecoder rate channels) <$>
    (managed c_opus_decoder_destroy =<< c_opus_decoder_create (fromIntegral rate) (fromIntegral channels) err)

packetChannels :: ByteString -> IO (Maybe Int)
packetChannels packet
  | BS.null packet = pure Nothing
  | otherwise = BS.unsafeUseAsCString packet $ \ptr -> do
      n <- c_opus_packet_get_nb_channels (castPtr ptr)
      pure $ if n > 0 then Just (fromIntegral n) else Nothing

-- | Samples per channel in the longest Opus frame, 120 ms.
maxFrame :: AudioDecoder -> Int
maxFrame dec = audioDecoderRate dec * 120 `div` 1000

decodeAudio :: AudioDecoder -> ByteString -> IO (Maybe Pcm)
decodeAudio dec packet = BS.unsafeUseAsCStringLen packet $ \(bytes, len) ->
  decodeInto dec (castPtr bytes) (fromIntegral len) (maxFrame dec) False

-- | A frame of this many samples per channel in place of a lost one.
concealAudio :: AudioDecoder -> Int -> IO (Maybe Pcm)
concealAudio dec samples = decodeInto dec nullPtr 0 (min samples (maxFrame dec)) False

-- | A lost frame of this many samples per channel, rebuilt from the next
-- packet (Opus in-band FEC).
recoverAudio :: AudioDecoder -> ByteString -> Int -> IO (Maybe Pcm)
recoverAudio dec packet samples = BS.unsafeUseAsCStringLen packet $ \(bytes, len) ->
  decodeInto dec (castPtr bytes) (fromIntegral len) (min samples (maxFrame dec)) True

decodeInto :: AudioDecoder -> Ptr Word8 -> Int32 -> Int -> Bool -> IO (Maybe Pcm)
decodeInto dec bytes len frame fec = withForeignPtr (audioDecoderPtr dec) $ \ptr -> do
  let channels = audioDecoderChannels dec
  out <- MV.new (frame * channels)
  n <- MV.unsafeWith out $ \samples ->
    c_opus_decode ptr bytes len samples (fromIntegral frame) (if fec then 1 else 0)
  if n < 0 then pure Nothing
  else Just . V.take (fromIntegral n * channels) <$> V.unsafeFreeze out

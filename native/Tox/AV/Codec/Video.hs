{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE StrictData               #-}
-- | Video through libavcodec, and on Android H.264 through the device's own
-- encoder.
module Tox.AV.Codec.Video
  ( VideoEncoder
  , videoEncoder
  , encodeVideo
  , setVideoBitRate
  , videoEncoderError
  , encoderName
  , VideoDecoder
  , videoDecoder
  , decodeVideo
  , encoders
  , decoders
  ) where

import           Control.Monad          (filterM, (<=<))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Int               (Int64)
import           Data.List              (intercalate)
import           Data.Word              (Word64, Word8)
import           Foreign.C.String       (CString, withCString)
import           Foreign.C.Types        (CInt (..))
import           Foreign.ForeignPtr     (ForeignPtr, finalizeForeignPtr,
                                         withForeignPtr)
import           Foreign.Marshal.Alloc  (alloca)
import           Foreign.Ptr            (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import           Foreign.Storable       (peek)

import           Tox.AV.Codec.Types     (Close (..), Picture (..), chromaSize,
                                         complete, keyFrameInterval, managed)
import           Tox.AV.RTP             (Codec (..), MediaTime (..))
#ifdef HSTOXAV_MEDIACODEC
import           Data.Maybe             (isJust)
import           Foreign.C.String       (peekCString)
import           Foreign.Marshal.Alloc  (allocaBytes)
#endif

data VideoStruct

-- Encoding, decoding and opening a codec take long, so those calls are safe:
-- an unsafe one holds up every other Haskell thread until it returns.
foreign import ccall safe "hstoxav_video_encoder_new"
  c_video_encoder_new :: CString -> CInt -> CInt -> Int64 -> CInt -> IO (Ptr VideoStruct)
foreign import ccall safe "hstoxav_video_decoder_new"
  c_video_decoder_new :: CString -> Int64 -> IO (Ptr VideoStruct)
foreign import ccall unsafe "&hstoxav_video_free"
  c_video_free :: FunPtr (Ptr VideoStruct -> IO ())
foreign import ccall safe "hstoxav_video_send_frame"
  c_video_send_frame :: Ptr VideoStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int64 -> CInt -> IO CInt
foreign import ccall safe "hstoxav_video_receive_packet"
  c_video_receive_packet :: Ptr VideoStruct -> Ptr (Ptr Word8) -> Ptr CInt -> Ptr CInt -> Ptr Int64 -> IO CInt
foreign import ccall safe "hstoxav_video_send_packet"
  c_video_send_packet :: Ptr VideoStruct -> Ptr Word8 -> CInt -> Int64 -> IO CInt
foreign import ccall safe "hstoxav_video_receive_frame"
  c_video_receive_frame
    :: Ptr VideoStruct -> Ptr CInt -> Ptr CInt -> Ptr (Ptr Word8) -> Ptr (Ptr Word8) -> Ptr (Ptr Word8)
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr Int64 -> IO CInt
foreign import ccall unsafe "avcodec_find_encoder_by_name"
  c_avcodec_find_encoder_by_name :: CString -> IO (Ptr ())
foreign import ccall unsafe "avcodec_find_decoder_by_name"
  c_avcodec_find_decoder_by_name :: CString -> IO (Ptr ())

instance Close VideoEncoder where
  close = encoderClose

instance Close VideoDecoder where
  close (VideoDecoder dec _) = finalizeForeignPtr dec

-- | Not libavcodec's wrappers of Android's encoders: they hand them pictures
-- with rows packed tight, where some want them padded, and what they make is
-- striped.
encoderNames, decoderNames :: Codec -> [String]
encoderNames codec = case codec of
  VP8  -> ["libvpx"]
  H264 -> ["libopenh264", "libx264"]
  H265 -> ["libx265"]
decoderNames codec = case codec of
  VP8  -> ["vp8"]
  H264 -> ["h264"]
  H265 -> ["hevc"]

available :: (CString -> IO (Ptr ())) -> (Codec -> [String]) -> IO [Codec]
available find names = filterM (fmap (not . null) . filterM found . names) [minBound .. maxBound]
  where
    found name = (/= nullPtr) <$> withCString name find

encoders, decoders :: IO [Codec]
encoders = (++) <$> hardwareEncoders <*> available c_avcodec_find_encoder_by_name encoderNames
decoders = available c_avcodec_find_decoder_by_name decoderNames

-- | Stops at the first that works.
firstJust :: [IO (Maybe a)] -> IO (Maybe a)
firstJust []       = pure Nothing
firstJust (a : as) = a >>= maybe (firstJust as) (pure . Just)

-- | libavcodec's encoders and Android's take pictures and give frames back
-- the same way.
data VideoEncoder = VideoEncoder
  { encoderName    :: String
  , encoderSend    :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int64 -> CInt -> IO CInt
    -- ^ 1 when the encoder is busy and skips the picture.
  , encoderReceive :: Ptr (Ptr Word8) -> Ptr CInt -> Ptr CInt -> Ptr Int64 -> IO CInt
  , encoderBitRate :: Int -> IO Bool
  , encoderError   :: IO String
  , encoderClose   :: IO ()
  }

-- | Takes the width, height and bits per second. 'Left' says why each
-- encoder we tried failed.
videoEncoder :: Codec -> Int -> Int -> Int -> IO (Either String VideoEncoder)
videoEncoder codec width height bitRate = firstRight $
  hardwareEncoder codec width height bitRate
  : [ withCString name $ \cname -> maybe (Left (name <> " refused it")) (Right . softwareEncoder name) <$>
        (managed c_video_free =<< c_video_encoder_new cname (fromIntegral width) (fromIntegral height)
          (fromIntegral bitRate) (fromIntegral (keyFrameInterval codec)))
    | name <- encoderNames codec ]

-- | Stops at the first that works.
firstRight :: [IO (Either String a)] -> IO (Either String a)
firstRight = go []
  where
    go whys []       = pure (Left (intercalate "; " (reverse whys)))
    go whys (a : as) = a >>= either (\why -> go (why : whys) as) (pure . Right)

softwareEncoder :: String -> ForeignPtr VideoStruct -> VideoEncoder
softwareEncoder name enc = VideoEncoder
  { encoderName = name
  , encoderSend = \y u v t key -> withForeignPtr enc $ \ptr -> c_video_send_frame ptr y u v t key
  , encoderReceive = \pdata psize pkey ptime -> withForeignPtr enc $ \ptr -> c_video_receive_packet ptr pdata psize pkey ptime
  , encoderBitRate = const (pure False)
  , encoderError = pure ""
  , encoderClose = finalizeForeignPtr enc
  }

-- | Each frame comes with the time of the picture it's from, in
-- milliseconds, and whether it's a key frame. Encoders may hold a picture's
-- frames back until a later one.
encodeVideo :: VideoEncoder -> Word64 -> Bool -> Picture -> IO (Maybe [(Word64, Bool, ByteString)])
encodeVideo enc time key picture@(Picture _ _ y u v)
  | not (complete picture) = pure Nothing
  | otherwise =
      BS.unsafeUseAsCString y $ \py -> BS.unsafeUseAsCString u $ \pu -> BS.unsafeUseAsCString v $ \pv -> do
        sent <- encoderSend enc (castPtr py) (castPtr pu) (castPtr pv) (fromIntegral time) (if key then 1 else 0)
        if sent < 0 then pure Nothing else receive []
  where
    receive frames = alloca $ \pdata -> alloca $ \psize -> alloca $ \pkey -> alloca $ \ptime -> do
      ret <- encoderReceive enc pdata psize pkey ptime
      case ret of
        0 -> do
          bytes <- peek pdata
          size <- peek psize
          isKey <- peek pkey
          t <- peek ptime
          frame <- BS.packCStringLen (castPtr bytes, fromIntegral size)
          receive ((fromIntegral t, isKey /= 0, frame) : frames)
        1 -> pure (Just (reverse frames))
        _ -> pure Nothing

-- | In bits per second. False when the encoder needs replacing for it.
setVideoBitRate :: VideoEncoder -> Int -> IO Bool
setVideoBitRate = encoderBitRate

-- | What went wrong when it last failed, if it knows.
videoEncoderError :: VideoEncoder -> IO String
videoEncoderError = encoderError

#ifdef HSTOXAV_MEDIACODEC
data MediaCodecStruct

foreign import ccall safe "hstoxav_mediacodec_encoder_new"
  c_mediacodec_encoder_new :: CString -> CInt -> CInt -> CInt -> CInt -> CString -> CInt -> IO (Ptr MediaCodecStruct)
foreign import ccall safe "&hstoxav_mediacodec_free"
  c_mediacodec_free :: FunPtr (Ptr MediaCodecStruct -> IO ())
foreign import ccall unsafe "hstoxav_mediacodec_describe"
  c_mediacodec_describe :: Ptr MediaCodecStruct -> CString -> CInt -> IO ()
foreign import ccall unsafe "hstoxav_mediacodec_error"
  c_mediacodec_error :: Ptr MediaCodecStruct -> CString -> CInt -> IO ()
foreign import ccall safe "hstoxav_mediacodec_set_bit_rate"
  c_mediacodec_set_bit_rate :: Ptr MediaCodecStruct -> CInt -> IO CInt
foreign import ccall safe "hstoxav_mediacodec_send_frame"
  c_mediacodec_send_frame :: Ptr MediaCodecStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int64 -> CInt -> IO CInt
foreign import ccall safe "hstoxav_mediacodec_receive_packet"
  c_mediacodec_receive_packet :: Ptr MediaCodecStruct -> Ptr (Ptr Word8) -> Ptr CInt -> Ptr CInt -> Ptr Int64 -> IO CInt

-- | Android's own encoder: faster than libavcodec's, and lighter on the
-- battery. Only for H.264, the one we send besides VP8. We try it on each
-- call, and it says what went wrong where it doesn't work.
mime :: Codec -> Maybe String
mime H264 = Just "video/avc"
mime _    = Nothing

hardwareEncoders :: IO [Codec]
hardwareEncoders = pure (filter (isJust . mime) [minBound .. maxBound])

hardwareEncoder :: Codec -> Int -> Int -> Int -> IO (Either String VideoEncoder)
hardwareEncoder codec width height bitRate = case mime codec of
  Nothing -> pure (Left "no MediaCodec encoder for it")
  Just m -> withCString m $ \cmime -> allocaBytes whySize $ \why -> do
    made <- managed c_mediacodec_free =<< c_mediacodec_encoder_new cmime (fromIntegral width) (fromIntegral height)
      (fromIntegral bitRate) (fromIntegral (keyFrameInterval codec)) why (fromIntegral whySize)
    case made of
      Nothing  -> Left . ("MediaCodec: " <>) <$> peekCString why
      Just enc -> Right <$> encoder enc
  where
    whySize = 1024
    encoder enc = do
      name <- withForeignPtr enc $ \ptr -> allocaBytes 256 $ \text -> c_mediacodec_describe ptr text 256 >> peekCString text
      pure VideoEncoder
        { encoderName = name
        , encoderSend = \y u v t key -> withForeignPtr enc $ \ptr -> c_mediacodec_send_frame ptr y u v t key
        , encoderReceive = \pdata psize pkey ptime -> withForeignPtr enc $ \ptr -> c_mediacodec_receive_packet ptr pdata psize pkey ptime
        , encoderBitRate = \rate -> withForeignPtr enc $ \ptr -> (== 0) <$> c_mediacodec_set_bit_rate ptr (fromIntegral rate)
        , encoderError = withForeignPtr enc $ \ptr -> allocaBytes 256 $ \text -> c_mediacodec_error ptr text 256 >> peekCString text
        , encoderClose = finalizeForeignPtr enc
        }
#else
hardwareEncoders :: IO [Codec]
hardwareEncoders = pure []

hardwareEncoder :: Codec -> Int -> Int -> Int -> IO (Either String VideoEncoder)
hardwareEncoder _ _ _ _ = pure (Left "no device encoder")
#endif

data VideoDecoder = VideoDecoder (ForeignPtr VideoStruct) ((MediaTime, Picture) -> IO ())

-- | Takes a rough pixel limit (decoders count the padding they add to the
-- width), and where to send each decoded picture with its frame's time.
videoDecoder :: Codec -> Int -> ((MediaTime, Picture) -> IO ()) -> IO (Maybe VideoDecoder)
videoDecoder codec maxPixels deliver = fmap (`VideoDecoder` deliver) <$> firstJust
  [ withCString name (managed c_video_free <=< (`c_video_decoder_new` fromIntegral maxPixels))
  | name <- decoderNames codec ]

-- | Pictures go out before this returns. False when the frame didn't
-- decode.
decodeVideo :: VideoDecoder -> MediaTime -> ByteString -> IO Bool
decodeVideo (VideoDecoder dec deliver) (MediaTime time) frame = withForeignPtr dec $ \ptr ->
  BS.unsafeUseAsCStringLen frame $ \(bytes, len) -> do
    sent <- c_video_send_packet ptr (castPtr bytes) (fromIntegral len) (fromIntegral time)
    if sent < 0 then pure False else receive ptr
  where
    receive ptr =
      alloca $ \pw -> alloca $ \ph -> alloca $ \py -> alloca $ \pu -> alloca $ \pv ->
      alloca $ \sy -> alloca $ \su -> alloca $ \sv -> alloca $ \pt -> do
        ret <- c_video_receive_frame ptr pw ph py pu pv sy su sv pt
        case ret of
          0 -> do
            w <- fromIntegral <$> peek pw
            h <- fromIntegral <$> peek ph
            y <- plane py sy w h
            u <- plane pu su (chromaSize w) (chromaSize h)
            v <- plane pv sv (chromaSize w) (chromaSize h)
            t <- MediaTime . fromIntegral <$> peek pt
            deliver (t, Picture w h y u v)
            receive ptr
          1 -> pure True
          2 -> receive ptr
          _ -> pure False
    plane pptr pstride w h = do
      base <- peek pptr
      stride <- fromIntegral <$> peek pstride
      BS.concat <$> sequence
        [ BS.packCStringLen (castPtr base `plusPtr` (row * stride), w) | row <- [0 .. h - 1] ]

{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}
-- | Video through the browser's codecs (WebCodecs).
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

import           Control.Monad            (filterM, (>=>))
import           Data.Bool                (bool)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import           Data.IORef               (IORef, atomicModifyIORef', newIORef)
import           Data.Word                (Word64, Word8)
import           Foreign.Ptr              (Ptr, castPtr)
import           GHC.Wasm.Prim            (JSString (..), JSVal, freeJSVal,
                                           toJSString)

import           Tox.AV.Codec.Types       (Close (..), Picture (..), complete,
                                           fromI420, keyFrame, keyFrameInterval)
import           Tox.AV.RTP               (Codec (..), MediaTime (..))

-- | WebCodecs' name for each, at a level that takes 1080p. H.264 is
-- constrained baseline, as the native encoders make it.
codecString :: Codec -> JSString
codecString codec = toJSString $ case codec of
  VP8  -> "vp8"
  H264 -> "avc1.42E028"
  H265 -> "hvc1.1.6.L120.B0"

-- Encoders and decoders are JavaScript objects holding the codec, and for
-- encoders, the frames we haven't taken yet. The calls copy our bytes before
-- their first await, as other Haskell threads may grow the memory meanwhile,
-- which detaches memory.buffer.

foreign import javascript unsafe
  "({codec: $1, width: $2, height: $3, bitrate: $4, framerate: 30, latencyMode: 'realtime', avc: {format: 'annexb'}, hevc: {format: 'annexb'}})"
  js_encoderConfig :: JSString -> Int -> Int -> Int -> IO JSVal

foreign import javascript safe
  "return typeof VideoEncoder !== 'undefined' && (await VideoEncoder.isConfigSupported($1)).supported === true;"
  js_encodes :: JSVal -> IO Bool

foreign import javascript safe
  "return typeof VideoDecoder !== 'undefined' && (await VideoDecoder.isConfigSupported({codec: $1})).supported === true;"
  js_decodes :: JSString -> IO Bool

foreign import javascript safe
  "if (!(await VideoEncoder.isConfigSupported($1)).supported) return null;\
  \const s = {chunks: [], failed: false};\
  \s.codec = new VideoEncoder({\
  \  output: chunk => { const bytes = new Uint8Array(chunk.byteLength); chunk.copyTo(bytes); s.chunks.push({key: chunk.type === 'key', bytes}); },\
  \  error: e => { s.failed = true; console.warn(`video encoder: ${e.message}`); }});\
  \s.codec.configure($1);\
  \return s;"
  js_encoderNew :: JSVal -> IO JSVal

-- Flush after each frame, so that we stop waiting even when the encoder drops
-- it. Chromium makes no key frame for a flush.
foreign import javascript safe
  "const s = $1, w = $5, h = $6, c = ((w + 1) >> 1) * ((h + 1) >> 1);\
  \if (s.failed) return null;\
  \const memory = new Uint8Array(__exports.memory.buffer);\
  \const data = new Uint8Array(w * h + 2 * c);\
  \data.set(memory.subarray($2, $2 + w * h));\
  \data.set(memory.subarray($3, $3 + c), w * h);\
  \data.set(memory.subarray($4, $4 + c), w * h + c);\
  \const frame = new VideoFrame(data, {format: 'I420', codedWidth: w, codedHeight: h, timestamp: $7 * 1000});\
  \try { s.codec.encode(frame, {keyFrame: $8 !== 0}); await s.codec.flush(); } catch (e) { s.failed = true; console.warn(`video encoder: ${e.message}`); } finally { frame.close(); }\
  \if (s.failed) return null;\
  \const chunks = s.chunks; s.chunks = []; return chunks;"
  js_encode :: JSVal -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> Int -> Double -> Bool -> IO JSVal

-- A decoder that failed opens again at the next key frame. One can't be
-- flushed after each frame: the next would have to be a key frame. We prefer
-- software decoders, as hardware ones may give RGB, which loses the exact
-- I420. Each format, error and dropped picture goes to the console once.
--
-- Each decoded picture goes to Haskell as packed I420, one at a time so that
-- they stay in order. Other formats and pictures over the limit are dropped.
-- RGB goes back to YUV as BT.709 limited range, as Chrome labels our
-- pictures BT.709 when it converts them.
foreign import javascript safe
  "const config = {codec: $1, optimizeForLatency: true, hardwareAcceleration: 'prefer-software'};\
  \if (!(await VideoDecoder.isConfigSupported(config)).supported) delete config.hardwareAcceleration;\
  \if (!(await VideoDecoder.isConfigSupported(config)).supported) return null;\
  \const s = {codec: null, max: $2, give: $3, closed: false, said: new Set(), done: Promise.resolve()};\
  \s.say = (level, message) => { if (!s.said.has(message)) { s.said.add(message); console[level](`video decoder: ${message}`); } };\
  \s.copy = async frame => {\
  \  const {x, y, width: w, height: h} = frame.visibleRect, cw = (w + 1) >> 1, ch = (h + 1) >> 1;\
  \  const rgb = /^(RGB|BGR)[AX]$/.test(frame.format);\
  \  if (w * h <= s.max && (rgb || frame.format === 'I420' || frame.format === 'I420A' || frame.format === 'NV12')) {\
  \    const bytes = new Uint8Array(w * h + 2 * cw * ch);\
  \    if (rgb) {\
  \      const px = new Uint8Array(4 * w * h), r = frame.format[0] === 'R' ? 0 : 2, b = 2 - r;\
  \      await frame.copyTo(px, {rect: {x, y, width: w, height: h}, layout: [{offset: 0, stride: 4 * w}]});\
  \      for (let i = 0; i < w * h; i++) bytes[i] = (47 * px[4 * i + r] + 157 * px[4 * i + 1] + 16 * px[4 * i + b] + 128 >> 8) + 16;\
  \      for (let row = 0; row < ch; row++) for (let col = 0; col < cw; col++) {\
  \        let sr = 0, sg = 0, sb = 0, n = 0;\
  \        for (let dy = 0; dy < 2 && 2 * row + dy < h; dy++) for (let dx = 0; dx < 2 && 2 * col + dx < w; dx++) {\
  \          const p = 4 * ((2 * row + dy) * w + 2 * col + dx);\
  \          sr += px[p + r]; sg += px[p + 1]; sb += px[p + b]; n++;\
  \        }\
  \        sr /= n; sg /= n; sb /= n;\
  \        bytes[w * h + row * cw + col] = (-26 * sr - 87 * sg + 112 * sb + 128 >> 8) + 128;\
  \        bytes[w * h + cw * ch + row * cw + col] = (112 * sr - 102 * sg - 10 * sb + 128 >> 8) + 128;\
  \      }\
  \    } else if (frame.format === 'NV12') {\
  \      const nv12 = new Uint8Array(w * h + 2 * cw * ch);\
  \      await frame.copyTo(nv12, {rect: {x, y, width: w, height: h}, layout: [{offset: 0, stride: w}, {offset: w * h, stride: 2 * cw}]});\
  \      bytes.set(nv12.subarray(0, w * h));\
  \      for (let i = 0; i < cw * ch; i++) { bytes[w * h + i] = nv12[w * h + 2 * i]; bytes[w * h + cw * ch + i] = nv12[w * h + 2 * i + 1]; }\
  \    } else {\
  \      const planes = frame.format === 'I420' ? bytes : new Uint8Array(2 * w * h + 2 * cw * ch);\
  \      const layout = [{offset: 0, stride: w}, {offset: w * h, stride: cw}, {offset: w * h + cw * ch, stride: cw}];\
  \      if (frame.format === 'I420A') layout.push({offset: w * h + 2 * cw * ch, stride: w});\
  \      await frame.copyTo(planes, {rect: {x, y, width: w, height: h}, layout});\
  \      if (planes !== bytes) bytes.set(planes.subarray(0, bytes.length));\
  \    }\
  \    s.say('info', `${frame.format} ${w}x${h}`);\
  \    if (!s.closed) await s.give(w, h, frame.timestamp / 1000, bytes);\
  \  } else s.say('warn', `dropped ${frame.format} ${w}x${h}`);\
  \};\
  \s.open = () => {\
  \  const codec = new VideoDecoder({\
  \    output: frame => { s.done = s.done.then(() => s.copy(frame)).catch(e => s.say('warn', e.message)).finally(() => frame.close()); },\
  \    error: e => { s.say('warn', e.message); if (s.codec === codec) s.codec = null; }});\
  \  codec.configure(config);\
  \  s.codec = codec; };\
  \return s;"
  js_decoderNew :: JSString -> Int -> JSVal -> IO JSVal

foreign import javascript unsafe
  "const s = $1, key = $4 !== 0;\
  \if (s.codec === null && key) s.open();\
  \if (s.codec === null) return;\
  \const data = new Uint8Array(__exports.memory.buffer, $2, $3).slice();\
  \try { s.codec.decode(new EncodedVideoChunk({type: key ? 'key' : 'delta', timestamp: $5 * 1000, data})); }\
  \catch (e) { s.say('warn', e.message); if (s.codec.state !== 'closed') s.codec.close(); s.codec = null; }"
  js_decode :: JSVal -> Ptr Word8 -> Int -> Bool -> Double -> IO ()

foreign import javascript "wrapper"
  js_giver :: (Int -> Int -> Double -> JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1 === null"
  js_isNull :: JSVal -> IO Bool

foreign import javascript unsafe "$1.length"
  js_count :: JSVal -> IO Int

foreign import javascript unsafe "$1[$2].key"
  js_key :: JSVal -> Int -> IO Bool

foreign import javascript unsafe "$1[$2].bytes"
  js_bytesAt :: JSVal -> Int -> IO JSVal

foreign import javascript unsafe "$1.byteLength"
  js_byteLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.byteLength).set($1)"
  js_copy :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe
  "$1.closed = true;\
  \if ($1.codec !== null && $1.codec.state !== 'closed') $1.codec.close();"
  js_close :: JSVal -> IO ()

nonNull :: JSVal -> IO (Maybe JSVal)
nonNull js = bool (Just js) Nothing <$> js_isNull js

indices :: JSVal -> IO [Int]
indices = fmap (enumFromTo 0 . pred) . js_count

bytesOf :: JSVal -> IO ByteString
bytesOf array = js_byteLength array >>= \n -> BS.create n (js_copy array)

encoderConfig :: Codec -> Int -> Int -> Int -> IO JSVal
encoderConfig = js_encoderConfig . codecString

encoders, decoders :: IO [Codec]
encoders = filterM (\codec -> js_encodes =<< encoderConfig codec 640 480 500000) [minBound .. maxBound]
decoders = filterM (js_decodes . codecString) [minBound .. maxBound]

data VideoEncoder = VideoEncoder
  { encoderCodec  :: Codec
  , sinceKeyFrame :: IORef Int
  , encoderJs     :: JSVal
  }

instance Close VideoEncoder where
  close = js_close . encoderJs

-- | Takes the width, height and bits per second.
videoEncoder :: Codec -> Int -> Int -> Int -> IO (Either String VideoEncoder)
videoEncoder codec width height bitRate =
  encoderConfig codec width height bitRate >>= js_encoderNew >>= nonNull
    >>= maybe (pure (Left "the browser can't encode it"))
          (\js -> Right . flip (VideoEncoder codec) js <$> newIORef (keyFrameInterval codec))

-- | Each frame comes with the time of the picture it's from, in
-- milliseconds, and whether it's a key frame. As we flush after each
-- picture, that's always this one.
encodeVideo :: VideoEncoder -> Word64 -> Bool -> Picture -> IO (Maybe [(Word64, Bool, ByteString)])
encodeVideo enc time asked picture@(Picture w h y u v)
  | not (complete picture) = pure Nothing
  | otherwise = do
      key <- atomicModifyIORef' (sinceKeyFrame enc) $ \n ->
        let key = asked || n >= keyFrameInterval (encoderCodec enc) in (bool (n + 1) 1 key, key)
      chunks <- BS.unsafeUseAsCString y $ \py -> BS.unsafeUseAsCString u $ \pu -> BS.unsafeUseAsCString v $ \pv ->
        js_encode (encoderJs enc) (castPtr py) (castPtr pu) (castPtr pv) w h (fromIntegral time) key
      nonNull chunks >>= traverse (indices >=> mapM (\i -> (time,,) <$> js_key chunks i <*> (bytesOf =<< js_bytesAt chunks i)))

-- | In bits per second. False, as the encoder is made again for it.
setVideoBitRate :: VideoEncoder -> Int -> IO Bool
setVideoBitRate _ _ = pure False

-- | Empty, as the browser's console says what went wrong.
videoEncoderError :: VideoEncoder -> IO String
videoEncoderError _ = pure ""

encoderName :: VideoEncoder -> String
encoderName = ("WebCodecs " <>) . show . encoderCodec

data VideoDecoder = VideoDecoder Codec JSVal JSVal

instance Close VideoDecoder where
  close (VideoDecoder _ js give) = js_close js >> freeJSVal give

-- | Takes a pixel limit, and where to send each decoded picture with its
-- frame's time.
videoDecoder :: Codec -> Int -> ((MediaTime, Picture) -> IO ()) -> IO (Maybe VideoDecoder)
videoDecoder codec maxPixels deliver = do
  give <- js_giver $ \w h time bytes -> bytesOf bytes >>= deliver . (,) (MediaTime (round time)) . fromI420 w h
  js_decoderNew (codecString codec) maxPixels give >>= nonNull
    >>= maybe (Nothing <$ freeJSVal give) (pure . Just . flip (VideoDecoder codec) give)

-- | The browser decodes the frame later, so this is always 'True'.
decodeVideo :: VideoDecoder -> MediaTime -> ByteString -> IO Bool
decodeVideo (VideoDecoder codec js _) (MediaTime time) frame = BS.unsafeUseAsCStringLen frame $ \(bytes, len) ->
  True <$ js_decode js (castPtr bytes) len (keyFrame codec frame) (fromIntegral time)

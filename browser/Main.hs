{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | A call between two ToxAVs in one page, for trying the codecs in a
-- browser: the camera and microphone go into one, and what the other receives
-- comes back out.
module Main (main) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan      (newChan, readChan, writeChan)
import           Control.Concurrent.MVar      (MVar, modifyMVar, modifyMVar_,
                                               newMVar, readMVar)
import           Control.Monad                (forever, void, when)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Unsafe       as BS
import           Data.Int                     (Int16)
import           Data.IORef                   (atomicModifyIORef', newIORef)
import           Data.Tuple                   (swap)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word                    (Word8)
import           Foreign.Ptr                  (Ptr, castPtr)
import           GHC.Wasm.Prim                (JSString (..), JSVal, toJSString)

import qualified Tox.AV                       as AV
import           Tox.AV                       (AudioFrame (..), BitRates (..),
                                               Picture (..), ToxAV,
                                               Transport (..), since)
import           Tox.AV.Codec                 (decoders, encoders, fromI420)
import qualified Tox.AV.Playout               as Playout
import           Tox.AV.Playout               (Playout)

-- | Each side's friend is the other side.
data Side = Caller | Callee
  deriving (Eq, Ord, Show)

rates :: BitRates
rates = BitRates 64 5000

-- Delays are ms since capture: both sides share our clock.

foreign import javascript unsafe "$1.pictureCame($2)"
  js_pictureCame :: JSVal -> Int -> IO ()

foreign import javascript unsafe "$1.audioCame($2, $3)"
  js_audioCame :: JSVal -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.log($2)"
  js_log :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.length"
  js_length :: JSVal -> IO Int

foreign import javascript unsafe "$1.byteLength"
  js_byteLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.byteLength).set(new Uint8Array($1.buffer, $1.byteOffset, $1.byteLength))"
  js_copy :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe "({rate: $1, channels: $2, samples: new Int16Array(__exports.memory.buffer, $3, $4).slice(), delay: $5})"
  js_block :: Int -> Int -> Ptr Int16 -> Int -> Int -> IO JSVal

foreign import javascript unsafe "({width: $1, height: $2, bytes: new Uint8Array(__exports.memory.buffer, $3, $4).slice(), delay: $5})"
  js_shown :: Int -> Int -> Ptr Word8 -> Int -> Int -> IO JSVal

foreign import javascript unsafe "({spedUp: $1, slowedDown: $2, madeUp: $3, skipped: $4})"
  js_stats :: Int -> Int -> Int -> Int -> IO JSVal

foreign import javascript unsafe "null"
  js_null :: IO JSVal

foreign import javascript unsafe "({sendVideo: $1, sendAudio: $2, pull: $3, picture: $4, stats: $5})"
  js_functions :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "wrapper"
  js_sender :: (Int -> Int -> JSVal -> Double -> IO ()) -> IO JSVal

foreign import javascript "wrapper"
  js_puller :: (Double -> IO JSVal) -> IO JSVal

foreign import javascript "wrapper"
  js_shower :: (Double -> Double -> Double -> IO JSVal) -> IO JSVal

foreign import javascript "wrapper"
  js_getter :: IO JSVal -> IO JSVal

foreign export javascript "start"
  start :: JSVal -> IO JSVal

bytesOf :: JSVal -> IO BS.ByteString
bytesOf array = js_byteLength array >>= \n -> BS.create n (js_copy array)

pcmOf :: JSVal -> IO AV.Pcm
pcmOf array = do
  samples <- js_length array >>= MV.new
  MV.unsafeWith samples (js_copy array . castPtr)
  V.unsafeFreeze samples

-- | Connects the two sides, calls the callee, and returns the functions that
-- send the caller's camera pictures and microphone samples, and that pull
-- the callee's audio and pictures when due.
start :: JSVal -> IO JSVal
start page = do
  let say = js_log page . toJSString
  packets <- newChan
  let transport from = Transport (send from) (send from)
      send from to packet = writeChan packets (from, to, packet)
  caller <- AV.new (transport Caller)
  callee <- AV.new (transport Callee)
  let side Caller = caller
      side Callee = callee
  playout <- newMVar Playout.empty
  void . forkIO . forever $ readChan packets >>= \(from, to, packet) ->
    AV.receivePacket (side to) from packet
  mapM_ (\(av, friend) -> AV.friendConnection av friend True) [(caller, Callee), (callee, Caller)]
  void . forkIO . forever $ AV.nextEvent caller >>= say . ("caller: " <>) . show
  void . forkIO . forever $ AV.nextEvent callee >>= received say page callee playout
  encoders >>= say . ("encoders: " <>) . show
  decoders >>= say . ("decoders: " <>) . show
  AV.call caller Callee rates >>= say . ("call: " <>) . show
  video <- sender (say . ("video: " <>)) $ \w h array age -> do
    captured <- capturedAgo age
    bytesOf array >>= AV.sendVideo caller Callee captured . fromI420 w h
  audio <- sender (say . ("audio: " <>)) $ \rate channels array age -> do
    captured <- capturedAgo age
    pcmOf array >>= AV.sendAudio caller Callee captured . AudioFrame rate channels
  -- The next 10 ms of audio, heard some ms from now.
  pull <- js_puller $ \ahead -> do
    now <- AV.clock
    let heardAt = AV.later (round ahead) now
    modifyMVar playout (pure . swap . Playout.pull now heardAt) >>= \case
      Nothing -> js_null
      Just (captured, AudioFrame rate channels pcm) ->
        V.unsafeWith pcm $ \ptr -> js_block rate channels ptr (V.length pcm) (since heardAt captured)
  -- The latest picture due by the end of the next showing (some ms from now),
  -- and how long after its capture it's seen (at the middle of the showing).
  -- With lip sync off, every picture that came is due.
  picture <- js_shower $ \middle end sync -> do
    now <- AV.clock
    let seenUntil = AV.later (round end) now
        dueAt = if sync /= 0 then seenUntil else AV.later 10000 seenUntil
    modifyMVar playout (pure . swap . Playout.pictureAt now dueAt) >>= \case
      Nothing -> js_null
      Just (captured, Picture w h y u v) ->
        BS.unsafeUseAsCStringLen (BS.concat [y, u, v]) $ \(ptr, n) ->
          js_shown w h (castPtr ptr) n (since (AV.later (round middle) now) captured)
  counts <- js_getter $ do
    Playout.Stats up down made skips <- Playout.stats <$> readMVar playout
    js_stats up down made skips
  js_functions video audio pull picture counts

capturedAgo :: Double -> IO AV.MediaTime
capturedAgo age = AV.later (negate (round age)) <$> AV.clock

-- | Logs what sending returns when it changes, not for every frame.
sender :: (Eq a, Show a) => (String -> IO ()) -> (Int -> Int -> JSVal -> Double -> IO a) -> IO JSVal
sender say send = do
  previous <- newIORef Nothing
  js_sender $ \a b array age -> do
    result <- send a b array age
    changed <- atomicModifyIORef' previous $ \old -> (Just result, old /= Just result)
    when changed . say $ show result

received :: (String -> IO ()) -> JSVal -> ToxAV Side -> MVar Playout -> AV.Event Side -> IO ()
received say page callee playout event = case event of
  AV.Invited friend _ -> AV.answer callee friend (BitRates 0 0) >>= say . ("answer: " <>) . show
  AV.VideoReceived _ captured pic -> do
    came <- AV.clock
    modifyMVar_ playout (pure . Playout.picture came captured pic)
    js_pictureCame page (since came captured)
  AV.AudioReceived _ number captured frame -> do
    came <- AV.clock
    modifyMVar_ playout (pure . Playout.audio came number captured frame)
    js_audioCame page (since came captured) (V.foldl' (\m x -> max m (abs (fromIntegral x))) 0 (audioSamples frame))
  _ -> say ("callee: " <> show event)

main :: IO ()
main = pure ()

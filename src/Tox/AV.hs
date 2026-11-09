{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}
-- | Audio and video calls between Tox friends, over any Tox implementation.
-- The client says how to send a friend packets, and hands over the ones that
-- arrive with 'losslessIds' and 'lossyIds'.
module Tox.AV
  ( ToxAV
  , Transport (..)
  , new
  , kill
  , losslessIds
  , lossyIds
  , receivePacket
  , friendConnection
  , Event (..)
  , nextEvent
  , waitingEvents
  , maxWaitingMedia
  , call
  , answer
  , control
  , setBitRate
  , SendError (..)
  , sendAudio
  , sendVideo
  , videoStats
  , videoEncoderName
    -- * Re-exports
  , VideoStats (..)
  , BitRates (..)
  , Control (..)
  , Refusal (..)
  , End (..)
  , Media (..)
  , Codec (..)
  , AudioFrame (..)
  , Picture (..)
  , Pcm
  , MediaTime
  , since
  , later
  , clock
  , Capabilities
  , sendsAudio
  , sendsVideo
  , acceptsAudio
  , acceptsVideo
  ) where

import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newEmptyMVar, newMVar, readMVar,
                                          takeMVar, tryPutMVar)
import           Control.Monad           (forM_, forever, unless, void, when)
import           Data.Bits               ((.&.))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.IORef              (IORef, atomicModifyIORef', newIORef,
                                          readIORef)
import qualified Data.Map.Strict         as Map
import           Data.Sequence           (Seq, (|>))
import qualified Data.Sequence           as Seq
import qualified Data.Vector.Storable    as V
import           Data.Word               (Word16, Word64)
import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (labelThread, myThreadId)
import           System.IO               (fixIO)
import           System.Random           (randomIO)

import qualified Tox.AV.Codec            as Codec
import           Tox.AV.Codec            (AudioDecoder, AudioEncoder,
                                          AudioFrame (..), Pcm, Picture (..),
                                          VideoDecoder, VideoEncoder)
import qualified Tox.AV.MSI              as MSI
import           Tox.AV.MSI              (Capabilities)
import           Tox.AV.RTP              (Codec (..), Media (..),
                                          MediaTime (..), later, since)
import qualified Tox.AV.Session          as Session
import           Tox.AV.Session          (BitRates (..), Control (..), End (..),
                                          Output (..), Refusal (..), Session,
                                          VideoStats (..), losslessIds,
                                          lossyIds)

-- | How to send a friend a packet, its id included. We call these with the
-- calls locked, so that packets go out in order: they must queue the packet
-- without waiting for it to go, and not call back into 'ToxAV'.
data Transport f = Transport
  { sendLossless :: f -> ByteString -> IO ()
  , sendLossy    :: f -> ByteString -> IO ()
  }

data Event f
  = Invited f Capabilities
  | PeerChanged f Capabilities
    -- ^ The friend answered, or changed what they send and accept.
  | Ended f End
  | AudioReceived f Word16 MediaTime AudioFrame
    -- ^ Numbers count up by one, so that the client can tell which frames
    -- never came. The time is when it was captured.
  | VideoReceived f MediaTime Picture
  deriving (Eq, Show)

sendsAudio, sendsVideo, acceptsAudio, acceptsVideo :: Capabilities -> Bool
sendsAudio = flag MSI.sendAudio
sendsVideo = flag MSI.sendVideo
acceptsAudio = flag MSI.receiveAudio
acceptsVideo = flag MSI.receiveVideo

flag :: Capabilities -> Capabilities -> Bool
flag f caps = caps .&. f == f

-- | A call's codecs, each locked on its own so that encoding and decoding,
-- and calls with other friends, don't wait for each other.
data CallCodecs = CallCodecs
  { audioOut      :: MVar (Maybe ((Int, Int), AudioEncoder)) -- ^ By sampling rate and channels.
  , audioIn       :: MVar (Maybe (AudioKey, AudioDecoder))
  , videoOut      :: MVar (Maybe VideoOut)
  , videoIn       :: MVar (Map.Map Codec VideoDecoder)
  , videoWaiting  :: MVar Waiting
  , videoWake     :: MVar () -- ^ Full once frames wait.
  , videoDecoding :: ThreadId -- ^ Decodes the frames that wait, in order.
  }

data VideoOut = VideoOut
  { outSize     :: (Codec, Int, Int) -- ^ The codec, width and height it's for.
  , outBitRate  :: Int -- ^ In bits per second.
  , outEncoder  :: VideoEncoder
  , outCaptured :: Map.Map Word64 MediaTime
    -- ^ When the pictures still in the encoder were captured, keyed by when we
    -- gave them to it.
  }

-- | Frames to decode, and whether we skip frames until a key frame.
data Waiting = Waiting Bool (Seq (Codec, MediaTime, ByteString))

-- | Once as many frames wait as come between our key frames, the decoder
-- can't keep up: we drop them and skip to the next key frame, which decodes
-- without them.
wait :: (Codec, MediaTime, ByteString) -> Waiting -> Waiting
wait x@(codec, _, frame) (Waiting skipping frames)
  | Codec.keyFrame codec frame = Waiting False (frames |> x)
  | skipping || Seq.length frames >= Codec.keyFrameInterval codec = Waiting True Seq.empty
  | otherwise = Waiting False (frames |> x)

-- | A decoder's sampling rate and channels, then the length of the last frame
-- it decoded (samples per channel) and when that frame was sent.
data AudioKey = AudioKey Int Int Int MediaTime

data ToxAV f = ToxAV
  { avTransport :: Transport f
  , avSession   :: MVar (Session f)
  , avCodecs    :: MVar (Map.Map f CallCodecs)
  , avEvents    :: Chan (Event f)
  , avWaiting   :: IORef Int -- ^ Events the client hasn't taken yet.
  , avTicker    :: ThreadId
  }

now :: IO Word64
now = (`div` 1000000) <$> getMonotonicTimeNSec

-- | Our time, for when the frames we send were captured.
clock :: IO MediaTime
clock = MediaTime . fromIntegral <$> now

-- | Milliseconds between checks for bandwidth reports.
tickInterval :: Int
tickInterval = 100

new :: Ord f => Transport f -> IO (ToxAV f)
new transport = do
  ssrc <- randomIO
  session <- Session.new ssrc <$> Codec.decoders <*> Codec.encoders
  sessionVar <- newMVar session
  codecs <- newMVar Map.empty
  events <- newChan
  waiting <- newIORef 0
  fixIO $ \av -> ToxAV transport sessionVar codecs events waiting <$> forkIO (myThreadId >>= (`labelThread` "toxav ticker") >> forever (do
    threadDelay (tickInterval * 1000)
    t <- now
    void $ run av (Right . Session.tick t)))

-- | Hangs up every call.
kill :: Ord f => ToxAV f -> IO ()
kill av = do
  killThread (avTicker av)
  void $ run av (Right . Session.kill)
  modifyMVar (avCodecs av) (\m -> pure (Map.empty, Map.elems m)) >>= mapM_ closeCodecs

nextEvent :: ToxAV f -> IO (Event f)
nextEvent av = readChan (avEvents av) <* atomicModifyIORef' (avWaiting av) (\n -> (n - 1, ()))

waitingEvents :: ToxAV f -> IO Int
waitingEvents = readIORef . avWaiting

-- | Nothing when not in a call with the friend.
videoStats :: Ord f => ToxAV f -> f -> IO (Maybe VideoStats)
videoStats av f = Session.videoStats f <$> readMVar (avSession av)

emit :: ToxAV f -> Event f -> IO ()
emit av event = atomicModifyIORef' (avWaiting av) (\n -> (n + 1, ())) >> writeChan (avEvents av) event

-- | Once the client is this many events behind, new audio and video aren't
-- passed on: they'd be late, and pictures are big.
maxWaitingMedia :: Int
maxWaitingMedia = 100

emitMedia :: ToxAV f -> Event f -> IO ()
emitMedia av event = readIORef (avWaiting av) >>= \waiting -> when (waiting < maxWaitingMedia) $ emit av event

-- | Sends the step's packets with the calls locked, then decodes the media
-- and tells the client.
run :: Ord f => ToxAV f -> (Session f -> Either Refusal (Session f, [Output f])) -> IO (Either Refusal ())
run av f = do
  result <- modifyMVar (avSession av) $ \s -> case f s of
    Left refusal        -> pure (s, Left refusal)
    Right (s', outputs) -> (,) s' . Right . concat <$> mapM send outputs
  traverse deliverAll result
  where
    -- A lost audio frame may be recovered from the one after it.
    deliverAll = \case
      [] -> pure ()
      AudioIn f' number payload : rest -> do
        let (more, rest') = span (isAudioFrom f') rest
            frames = (number, payload) : [(n, p) | AudioIn _ n p <- more]
        audio <- using audioIn av f' (decodeAudio frames)
        forM_ audio $ \(n, time, frame) -> emitMedia av (AudioReceived f' n time frame)
        deliverAll rest'
      output : rest -> deliver output >> deliverAll rest
    isAudioFrom f' = \case
      AudioIn g _ _ -> g == f'
      _           -> False
    send = \case
      SendLossless f' packet -> [] <$ sendLossless (avTransport av) f' packet
      SendLossy f' packet -> [] <$ sendLossy (avTransport av) f' packet
      output -> pure [output]
    deliver = \case
      Session.Event event -> do
        case event of
          Session.Ended f' _ -> dropCodecs av f'
          _                  -> pure ()
        emit av $ case event of
          Session.Invited f' caps     -> Invited f' caps
          Session.PeerChanged f' caps -> PeerChanged f' caps
          Session.Ended f' end        -> Ended f' end
      VideoIn f' codec time frame -> do
        c <- codecsOf av f'
        modifyMVar_ (videoWaiting c) (pure . wait (codec, time, frame))
        void $ tryPutMVar (videoWake c) ()
      _ -> pure ()

codecsOf :: Ord f => ToxAV f -> f -> IO CallCodecs
codecsOf av f = modifyMVar (avCodecs av) $ \m -> case Map.lookup f m of
  Just c -> pure (m, c)
  Nothing -> do
    c <- fixIO $ \c -> CallCodecs <$> newMVar Nothing <*> newMVar Nothing <*> newMVar Nothing <*> newMVar Map.empty
      <*> newMVar (Waiting False Seq.empty) <*> newEmptyMVar <*> forkIO (decoding c)
    pure (Map.insert f c m, c)
  where
    -- Away from the thread that takes packets, so that audio doesn't wait.
    decoding c = myThreadId >>= (`labelThread` "toxav video decoder") >> forever (do
      takeMVar (videoWake c)
      frames <- modifyMVar (videoWaiting c) $ \(Waiting skipping frames) -> pure (Waiting skipping Seq.empty, frames)
      forM_ frames $ \(codec, time, frame) -> modifyMVar_ (videoIn c) (decodeVideo av f codec time frame))

-- | Frees the codecs of a call that ended, each once it is done with what it
-- is encoding or decoding.
dropCodecs :: Ord f => ToxAV f -> f -> IO ()
dropCodecs av f =
  modifyMVar (avCodecs av) (\m -> pure (Map.delete f m, Map.lookup f m)) >>= mapM_ closeCodecs

closeCodecs :: CallCodecs -> IO ()
closeCodecs c = do
  killThread (videoDecoding c)
  modifyMVar_ (audioOut c) $ \x -> Nothing <$ mapM_ (Codec.close . snd) x
  modifyMVar_ (audioIn c) $ \x -> Nothing <$ mapM_ (Codec.close . snd) x
  modifyMVar_ (videoOut c) $ \x -> Nothing <$ mapM_ (Codec.close . outEncoder) x
  modifyMVar_ (videoIn c) $ \x -> Map.empty <$ mapM_ Codec.close x

using :: Ord f => (CallCodecs -> MVar a) -> ToxAV f -> f -> (a -> IO (a, b)) -> IO b
using part av f action = codecsOf av f >>= \c -> modifyMVar (part c) action

samplingRates :: [Int]
samplingRates = [8000, 12000, 16000, 24000, 48000]

-- | A payload is the sampling rate (4 bytes, big-endian), then the Opus packet.
audioPayload :: ByteString -> Maybe (Int, ByteString)
audioPayload bytes
  | BS.length rateBytes == 4, rate `elem` samplingRates = Just (rate, opus)
  | otherwise = Nothing
  where
    (rateBytes, opus) = BS.splitAt 4 bytes
    rate = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 rateBytes

-- | Audio frames in order with their numbers, 'Nothing' for a lost one. We
-- recover it from the frame after it, or make it up, timed to follow the
-- frame before it.
decodeAudio :: [(Word16, Maybe (MediaTime, ByteString))] -> Maybe (AudioKey, AudioDecoder) -> IO (Maybe (AudioKey, AudioDecoder), [(Word16, MediaTime, AudioFrame)])
decodeAudio frames dec = case frames of
  [] -> pure (dec, [])
  (number, Nothing) : rest -> case dec of
    Just (AudioKey rate channels samples previous, d) | samples > 0 -> do
      recovered <- case rest of
        (_, Just (_, next)) : _ | Just (rate', opus) <- audioPayload next, rate' == rate ->
          Codec.recoverAudio d opus samples
        _ -> pure Nothing
      pcm <- maybe (Codec.concealAudio d samples) (pure . Just) recovered
      let time = later (samples * 1000 `div` rate) previous
      (dec', out) <- decodeAudio rest (Just (AudioKey rate channels samples time, d))
      pure (dec', maybe out ((: out) . (number, time,) . AudioFrame rate channels) pcm)
    _ -> decodeAudio rest dec
  (number, Just (time, bytes)) : rest -> do
    decoded <- case audioPayload bytes of
      Nothing -> pure Nothing
      Just (rate, opus) -> Codec.packetChannels opus >>= \case
        Nothing -> pure Nothing
        Just c -> do
          d <- case dec of
            Just (AudioKey rate' c' _ _, d) | (rate', c') == (rate, c) -> pure (Just d)
            _ -> Codec.audioDecoder rate c
          case d of
            Nothing -> pure Nothing
            Just d' -> fmap (rate, c, d',) <$> Codec.decodeAudio d' opus
    case decoded of
      Nothing -> decodeAudio rest dec
      Just (rate, c, d, pcm) -> do
        (dec', out) <- decodeAudio rest (Just (AudioKey rate c (V.length pcm `div` c) time, d))
        pure (dec', (number, time, AudioFrame rate c pcm) : out)

-- | The most pixels a friend's picture may have: 4K. A frame may say up to
-- 16383 by 16383, which would take gigabytes to decode and show.
maxPicture :: Int
maxPicture = 3840 * 2160

decodeVideo :: Ord f => ToxAV f -> f -> Codec -> MediaTime -> ByteString -> Map.Map Codec VideoDecoder -> IO (Map.Map Codec VideoDecoder)
decodeVideo av f codec time frame decoders = do
  dec <- maybe (Codec.videoDecoder codec maxPicture decoded) (pure . Just) (Map.lookup codec decoders)
  case dec of
    Nothing -> decoders <$ counted Session.decodeFailed
    Just d -> do
      ok <- Codec.decodeVideo d time frame
      unless ok $ counted Session.decodeFailed
      pure (Map.insert codec d decoders)
  where
    decoded picture = counted Session.pictureDecoded >> emitMedia av (uncurry (VideoReceived f) picture)
    counted g = modifyMVar_ (avSession av) (pure . g f)

-- | A packet from the friend, its id included.
receivePacket :: Ord f => ToxAV f -> f -> ByteString -> IO ()
receivePacket av f packet = do
  t <- now
  void $ run av (Right . Session.receive t f packet)

friendConnection :: Ord f => ToxAV f -> f -> Bool -> IO ()
friendConnection av f online = void $ run av (Right . Session.connection f online)

timed :: Ord f => ToxAV f -> (Word64 -> Session f -> Either Refusal (Session f, [Output f])) -> IO (Either Refusal ())
timed av f = now >>= \t -> run av (f t)

call :: Ord f => ToxAV f -> f -> BitRates -> IO (Either Refusal ())
call av f rates = timed av $ \t -> Session.call t f rates

answer :: Ord f => ToxAV f -> f -> BitRates -> IO (Either Refusal ())
answer av f rates = timed av $ \t -> Session.answer t f rates

control :: Ord f => ToxAV f -> f -> Control -> IO (Either Refusal ())
control av f ctl = do
  result <- run av (Session.control f ctl)
  case (ctl, result) of
    (Cancel, Right ()) -> dropCodecs av f
    _                  -> pure ()
  pure result

-- | In kilobits per second, 0 to stop sending.
setBitRate :: Ord f => ToxAV f -> f -> Media -> Int -> IO (Either Refusal ())
setBitRate av f m rate = run av (Session.setBitRate f m (fromIntegral rate))

data SendError
  = NotSending Refusal
  | InvalidFrame
  deriving (Eq, Show)

-- | One frame of 2.5 to 60 ms at a sampling rate Opus takes, with 1 or 2
-- channels, and when its first sample was captured by 'clock'.
sendAudio :: Ord f => ToxAV f -> f -> MediaTime -> AudioFrame -> IO (Either SendError ())
sendAudio av f captured (AudioFrame rate channels pcm)
  | rate `notElem` samplingRates || channels `notElem` [1, 2] || V.length pcm `mod` channels /= 0 =
      pure (Left InvalidFrame)
  | otherwise = readMVar (avSession av) >>= \s -> case Session.canSend Audio f s of
    Left refusal -> pure (Left (NotSending refusal))
    Right () -> do
      let kbps = maybe 0 audioBitRate (Session.bitRates f s)
      opus <- using audioOut av f $ \current -> do
        enc <- case current of
          Just (key, enc) | key == (rate, channels) -> pure (Just enc)
          _ -> do
            mapM_ (Codec.close . snd) current
            Codec.audioEncoder rate channels (fromIntegral kbps * 1000)
        case enc of
          Nothing -> pure (Nothing, Nothing)
          Just e -> do
            _ <- Codec.setAudioBitRate e (fromIntegral kbps * 1000)
            (,) (Just ((rate, channels), e)) <$> Codec.encodeAudio e pcm
      case opus of
        Nothing -> pure (Left InvalidFrame)
        Just packet -> either (Left . NotSending) Right <$>
          run av (Session.sendAudio captured f (fromIntegral rate) packet)

-- | A picture, and when it was captured by 'clock'. A codec other than VP8
-- that doesn't work is given up for the call, and the picture goes as VP8.
sendVideo :: Ord f => ToxAV f -> f -> MediaTime -> Picture -> IO (Either SendError ())
sendVideo av f captured picture
  | not (Codec.complete picture) = pure (Left InvalidFrame)
  | otherwise = readMVar (avSession av) >>= \s -> case Session.canSend Video f s of
      Left refusal -> pure (Left (NotSending refusal))
      Right () -> do
        t <- now
        let codec = Session.videoCodec f s
            Picture w h _ _ _ = picture
            bitRate = maybe 0 (fromIntegral . videoBitRate) (Session.bitRates f s) * 1000
            size = (codec, w, h)
            fresh current = do
              mapM_ (Codec.close . outEncoder) current
              fmap (\e -> (VideoOut size bitRate e Map.empty, True)) <$> Codec.videoEncoder codec w h bitRate
        frames <- using videoOut av f $ \current -> do
          out <- case current of
            Just o | outSize o == size -> do
              live <- if outBitRate o == bitRate then pure True else Codec.setVideoBitRate (outEncoder o) bitRate
              if live then pure (Right (o { outBitRate = bitRate }, False)) else fresh current
            _ -> fresh current
          case out of
            Left why -> pure (Nothing, Left why)
            -- An encoder that failed may stay broken (browsers' do), so the
            -- next picture gets a new one.
            Right (o, fresh') -> Codec.encodeVideo (outEncoder o) t (fresh' || Session.wantsKeyFrame f s) picture >>= \case
              Nothing -> do
                err <- Codec.videoEncoderError (outEncoder o)
                Codec.close (outEncoder o)
                pure (Nothing, Left (Codec.encoderName (outEncoder o) <> " couldn't encode a picture" <> (if null err then "" else ": " <> err)))
              Just encoded ->
                let waiting = Map.insert t captured (outCaptured o)
                    stamped = [ (Map.findWithDefault captured time waiting, isKey, bytes) | (time, isKey, bytes) <- encoded ]
                    -- Pictures before the last one out won't come out later.
                    left = case reverse encoded of
                      (time, _, _) : _ -> snd (Map.split time waiting)
                      []               -> waiting
                in pure (Just o { outCaptured = left }, Right stamped)
        case frames of
          Left why
            | codec /= VP8 -> do
                modifyMVar_ (avSession av) (pure . Session.encoderFailed f codec why)
                using videoOut av f $ \current -> (Nothing, ()) <$ mapM_ (Codec.close . outEncoder) current
                sendVideo av f captured picture
            | otherwise -> pure (Left InvalidFrame)
          Right fs -> do
            sent <- mapM (\(time, isKey, bytes) -> run av (Session.sendVideo time f codec isKey bytes)) fs
            pure (either (Left . NotSending) (const (Right ())) (sequence sent))

-- | What we send the friend's video with, Nothing until we send some, and
-- why encoders we'd rather have used didn't work.
videoEncoderName :: Ord f => ToxAV f -> f -> IO (Maybe String)
videoEncoderName av f = do
  failed <- Session.failedEncoders f <$> readMVar (avSession av)
  using' <- Map.lookup f <$> readMVar (avCodecs av) >>= maybe (pure Nothing) (fmap (fmap (Codec.encoderName . outEncoder)) . readMVar . videoOut)
  pure $ (<> concat [ ", as " <> show codec <> " didn't work: " <> why | (codec, why) <- failed ]) <$> using'

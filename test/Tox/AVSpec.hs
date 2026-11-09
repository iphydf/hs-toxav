{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Tox.AVSpec (spec) where

import           Control.Concurrent      (forkIO, killThread, threadDelay)
import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar,
                                          readMVar, takeMVar)
import           Control.Exception       (bracket)
import           Control.Monad           (forM_, forever, replicateM,
                                          replicateM_, when)
import qualified Data.ByteString         as BS
import           Data.IORef              (atomicModifyIORef', newIORef)
import qualified Data.Map.Strict         as Map
import qualified Data.Vector.Storable    as V
import           Data.Word               (Word8)
import           System.Timeout          (timeout)
import           Test.Hspec

import           Tox.AV

data Side = A | B
  deriving (Eq, Ord, Show)

-- | Two instances that are each other's online friend, with packets
-- delivered in order on another thread.
withPair :: ((ToxAV Side, ToxAV Side) -> IO a) -> IO a
withPair = withLossyPair (\_ _ -> pure True)

-- | Like 'withPair', but delivers only the packets that pass.
withLossyPair :: (Side -> BS.ByteString -> IO Bool) -> ((ToxAV Side, ToxAV Side) -> IO a) -> IO a
withLossyPair passes action = do
  wire <- newChan
  peers <- newEmptyMVar
  let transport from = Transport (send from) (send from)
      send from _ packet = writeChan wire (from, packet)
      forward = forever $ do
        (from, packet) <- readChan wire
        (a, b) <- readMVar peers
        delivered <- passes from packet
        when delivered $ receivePacket (if from == A then b else a) from packet
  bracket (forkIO forward) killThread $ \_ -> do
    a <- new (transport A)
    b <- new (transport B)
    putMVar peers (a, b)
    friendConnection a B True
    friendConnection b A True
    result <- action (a, b)
    kill a
    kill b
    pure result

-- | The next event that matches, within a few seconds.
expect :: ToxAV Side -> (Event Side -> Bool) -> IO (Event Side)
expect av wanted = timeout 5000000 go >>= maybe (fail "no such event") pure
  where
    go = do
      event <- nextEvent av
      if wanted event then pure event else go

ok :: Show e => IO (Either e ()) -> IO ()
ok action = action >>= either (fail . show) pure

talking :: (ToxAV Side, ToxAV Side) -> IO ()
talking (a, b) = do
  ok $ call a B (BitRates 48 1000)
  _ <- expect b isInvited
  ok $ answer b A (BitRates 48 1000)
  _ <- expect a isPeerChanged
  pure ()
  where
    isInvited Invited {} = True
    isInvited _          = False
    isPeerChanged PeerChanged {} = True
    isPeerChanged _              = False

picture :: Picture
picture = Picture 64 48 (BS.replicate (64 * 48) 128) (BS.replicate (32 * 24) 64) (BS.replicate (32 * 24) 192)

-- | A picture of noise, which takes long to decode.
noise :: Int -> Picture
noise seed = Picture 640 480 (plane (640 * 480)) (plane (320 * 240)) (plane (320 * 240))
  where
    plane n = fst (BS.unfoldrN n (\x -> Just (fromIntegral (x `div` 65536), x * 1103515245 + 12345)) (seed :: Int))

-- | A smooth pattern that moves from picture to picture, as video does.
moving :: Int -> Picture
moving n = Picture 320 240 luma (BS.replicate (160 * 120) 128) (BS.replicate (160 * 120) 128)
  where
    luma = BS.concat [BS.map (+ row y) columns | y <- [0 .. 239]]
    columns = BS.pack [round (128 + 60 * sin (fromIntegral (x + 4 * n) / 20 :: Double)) | x <- [0 .. 319 :: Int]]
    row y = round (40 * cos (fromIntegral (y - 3 * n) / 15 :: Double)) :: Word8

-- | How close a picture is to another, in dB: 40 and more look the same.
psnr :: Picture -> Picture -> Double
psnr (Picture _ _ a _ _) (Picture _ _ b _ _) = 10 * logBase 10 (255 * 255 / max 1e-9 mse)
  where mse = sum [(fromIntegral x - fromIntegral y) ^ (2 :: Int) | (x, y) <- BS.zip a b] / fromIntegral (BS.length a) :: Double

spec :: Spec
spec = do
  it "carries a call's audio and video, with when they were captured" $ withPair $ \pair@(a, b) -> do
    talking pair
    captured <- clock
    ok $ sendAudio a B captured (AudioFrame 48000 2 (V.replicate (2 * 960) 1000))
    ok $ sendVideo a B (later 5 captured) picture
    AudioReceived _ _ audioTime (AudioFrame rate channels pcm) <- expect b isAudio
    (audioTime, rate, channels, V.length pcm) `shouldBe` (captured, 48000, 2, 2 * 960)
    VideoReceived _ videoTime got <- expect b isVideo
    (videoTime, pictureWidth got, pictureHeight got) `shouldBe` (later 5 captured, 64, 48)

  it "fills in a lost audio frame" $ do
    sent <- newIORef (0 :: Int)
    -- The fifth audio packet from A goes missing.
    let passes from packet = case BS.uncons packet of
          Just (192, _) | from == A -> (/= 5) <$> atomicModifyIORef' sent (\n -> (n + 1, n + 1))
          _ -> pure True
    withLossyPair passes $ \pair@(a, b) -> do
      talking pair
      start <- clock
      let times = [later (20 * i) start | i <- [0 .. 9]]
      forM_ times $ \t -> ok $ sendAudio a B t (AudioFrame 48000 1 (V.replicate 960 1000))
      received <- replicateM 10 (expect b isAudio)
      [(n, t, V.length pcm) | AudioReceived _ n t (AudioFrame _ _ pcm) <- received] `shouldBe` zip3 [0 ..] times (repeat 960)

  it "lets late media go while the client is far behind, but not the call" $ withPair $ \pair@(a, b) -> do
    talking pair
    forM_ [1 .. 300 :: Int] $ \_ -> clock >>= \t -> ok $ sendAudio a B t (AudioFrame 48000 1 (V.replicate 960 1000))
    -- Until B has had them all.
    let settled n = threadDelay 200000 >> waitingEvents b >>= \m -> if m == n then pure m else settled m
    waiting <- timeout 10000000 (settled (-1))
    waiting `shouldSatisfy` maybe False (<= maxWaitingMedia)
    ok $ control a B Cancel
    expect b isEnded `shouldReturn` Ended A HungUp

  it "numbers the audio it passes on, so that the client knows what it let go" $ withPair $ \pair@(a, b) -> do
    talking pair
    let send n = replicateM_ n $ clock >>= \t -> ok $ sendAudio a B t (AudioFrame 48000 1 (V.replicate 960 1000))
        settled n = threadDelay 200000 >> waitingEvents b >>= \m -> if m == n then pure m else settled m
    -- B takes none until 150 have come, then 10 more.
    send 150
    _ <- timeout 10000000 (settled (-1))
    behind <- replicateM maxWaitingMedia (expect b isAudio)
    send 10
    caughtUp <- replicateM 10 (expect b isAudio)
    [n | AudioReceived _ n _ _ <- behind ++ caughtUp] `shouldBe` [0 .. 99] ++ [150 .. 159]

  it "passes on audio that comes while it decodes video" $ do
    -- Holds back delivery while A sends, so the video and audio arrive together.
    gate <- newMVar ()
    withLossyPair (\_ _ -> True <$ readMVar gate) $ \pair@(a, b) -> do
      talking pair
      takeMVar gate
      t <- clock
      forM_ [1 .. 3] $ \i -> ok $ sendVideo a B t (noise i)
      ok $ sendAudio a B t (AudioFrame 48000 1 (V.replicate 960 1000))
      putMVar gate ()
      let picturesBefore n = timeout 10000000 (nextEvent b) >>= \case
            Just AudioReceived {} -> pure n
            Just VideoReceived {} -> picturesBefore (n + 1)
            Just _ -> picturesBefore n
            Nothing -> fail "no audio"
      -- The audio doesn't wait for all three pictures to decode.
      picturesBefore (0 :: Int) >>= (`shouldSatisfy` (< 3))

  it "asks for a key frame when video is lost, so that pictures soon look right again" $ do
    sent <- newIORef (0 :: Int)
    -- Every 10th video packet from A goes missing.
    let passes from packet = case BS.uncons packet of
          Just (193, _) | from == A -> (\n -> n `mod` 10 /= 0) <$> atomicModifyIORef' sent (\n -> (n + 1, n + 1))
          _ -> pure True
    withLossyPair passes $ \pair@(a, b) -> do
      talking pair
      start <- clock
      let times = [later (33 * i) start | i <- [0 .. 59]]
      forM_ (zip [0 ..] times) $ \(i, t) -> do
        ok $ sendVideo a B t (moving i)
        threadDelay 33000
      let collect got = timeout 1000000 (expect b isVideo) >>= \case
            Just (VideoReceived _ t pic) -> collect ((t, pic) : got)
            _ -> pure (reverse got)
      got <- collect []
      let byTime = Map.fromList (zip times [0 :: Int ..])
          scores = [psnr (moving i) pic | (t, pic) <- got, Just i <- [Map.lookup t byTime]]
      -- Without asking, most look wrong, as far down as 15 dB.
      scores `shouldSatisfy` all (>= 25)
      Just stats <- videoStats b A
      (smears stats > 0, smearedFrames stats <= smears stats) `shouldBe` (True, True)

  it "hangs up" $ withPair $ \pair@(a, b) -> do

    talking pair
    ok $ control a B Cancel
    expect b isEnded `shouldReturn` Ended A HungUp
    clock >>= \t -> sendAudio b A t (AudioFrame 48000 1 (V.replicate 960 0)) `shouldReturn` Left (NotSending NotInCall)
  where
    isAudio AudioReceived {} = True
    isAudio _                = False
    isVideo VideoReceived {} = True
    isVideo _                = False
    isEnded Ended {} = True
    isEnded _        = False

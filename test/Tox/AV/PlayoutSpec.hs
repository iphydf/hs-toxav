{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Tox.AV.PlayoutSpec (spec) where

import qualified Data.ByteString      as BS
import           Data.List            (mapAccumL, sortOn)
import qualified Data.Map.Strict      as Map
import qualified Data.Vector          as B
import qualified Data.Vector.Storable as V
import           Test.Hspec

import           Tox.AV.Codec.Types   (AudioFrame (..), Picture (..))
import           Tox.AV.Playout
import           Tox.AV.RTP           (MediaTime (..), since)

-- | 20 ms of a 230 Hz tone at 48 kHz, the nth since the call started. Its
-- period divides neither frames nor blocks, so splices that don't follow it
-- show.
tone :: Int -> AudioFrame
tone = (tones B.!)

-- | Made once for all the tests: 12 s.
tones :: B.Vector AudioFrame
tones = B.generate 600 $ \n -> AudioFrame 48000 1 $ V.generate 960 $ \k ->
  round (10000 * sin (2 * pi * 230 * fromIntegral (960 * n + k) / 48000 :: Double))

dot :: Picture
dot = Picture 2 2 (BS.replicate 4 0) (BS.replicate 1 0) (BS.replicate 1 0)

at :: Int -> MediaTime
at = MediaTime . fromIntegral

data Run = Run
  { heard :: [(Int, Int, AudioFrame)] -- ^ When each block played, how long after its capture, and the block.
  , shown :: [(Int, Int)]             -- ^ When each picture showed, and how long after its capture.
  , final :: Playout
  }

-- | Plays a block heard every 10 ms and asks for the picture seen then,
-- until a time, with frames and pictures coming in at their times (when they
-- came, when they were captured). Frames are numbered in the order given, and
-- each waits for those before it that come, as Tox.AV passes them on.
simulate :: Int -> [(Int, Int, AudioFrame)] -> [(Int, Int)] -> Run
simulate = simulateFrom 0 (const 0)

-- | Like 'simulate', but from a start time, and pulling each block early (how
-- many ms is a function of when it's heard). Audio comes in up to when each
-- block is pulled.
simulateFrom :: Int -> (Int -> Int) -> Int -> [(Int, Int, AudioFrame)] -> [(Int, Int)] -> Run
simulateFrom start ahead end audios pics = go start (sortOn came inOrder) (sortOn fst pics) empty [] []
  where
    inOrder = snd (mapAccumL waits minBound (zip [0 ..] audios))
    waits latest (n, (c, captured, frame))
      | c > end = (latest, (c, n, captured, frame))
      | otherwise = (max latest c, (max latest c, n, captured, frame))
    came (c, _, _, _) = c
    go now as ps p hs ss
      | now > end = Run (reverse hs) (reverse ss) p
      | otherwise =
          let pulledAt = now - ahead now
              (dueAudio, as') = span ((<= pulledAt) . came) as
              (duePics, ps') = span ((<= now) . fst) ps
              p1 = foldl (\q (c, n, captured, frame) -> audio (at c) n (at captured) frame q) p dueAudio
              p2 = foldl (\q (c, captured) -> picture (at c) (at captured) dot q) p1 duePics
              (block, p3) = pull (at pulledAt) (at now) p2
              (pic, p4) = pictureAt (at now) (at now) p3
              ago = since (at now)
          in go (now + 10) as' ps' p4
               (maybe hs (\(captured, b) -> (now, ago captured, b) : hs) block)
               (maybe ss (\(captured, _) -> (now, ago captured) : ss) pic)

-- | Frames every 20 ms for a while, each coming after a delay.
frames :: Int -> (Int -> Int) -> [(Int, Int, AudioFrame)]
frames count delay = [(20 * n + delay n, 20 * n, tone n) | n <- [0 .. count - 1]]

samples :: [(Int, Int, AudioFrame)] -> V.Vector Int
samples hs = V.map fromIntegral (V.concat [audioSamples b | (_, _, b) <- hs])

-- | Whether the samples are a stretch of the tone as it was sent.
asSent :: V.Vector Int -> Bool
asSent x = any (\i -> V.slice i (V.length x) sent == x)
  [i | i <- [0 .. V.length sent - V.length x], V.slice i 8 sent == V.take 8 x]
  where sent = samples [(0, 0, tone n) | n <- [0 .. 599]]

-- | All the same, and at most this.
steadyWithin :: Int -> [Int] -> Bool
steadyWithin most = \case
  d : rest -> d <= most && all (== d) rest
  []       -> False

delaysFrom :: Int -> Run -> [Int]
delaysFrom from r = [d | (t, d, _) <- heard r, t >= from]

-- | The biggest step from one sample to the next. The tone's own is 301.
biggestStep :: V.Vector Int -> Int
biggestStep x = V.maximum (V.map abs (V.zipWith (-) (V.tail x) x))

-- | The quietest peak over each period of the tone, 10000 when nothing
-- cancels out.
quietestPeak :: V.Vector Int -> Int
quietestPeak x = minimum [V.maximum (V.map abs (V.slice i 209 x)) | i <- [0, 209 .. V.length x - 209]]

-- | Crossings of zero a second, twice the pitch.
crossings :: V.Vector Int -> Double
crossings x = fromIntegral (V.length (V.filter id (V.zipWith (\a b -> (a < 0) /= (b < 0)) x (V.tail x))))
  * 48000 / fromIntegral (V.length x)

spec :: Spec
spec = do
  it "plays steady audio after its delay, as it came" $ do
    -- Sped up from the start delay to within 20 ms of what the frames need.
    let r = simulate 2900 (frames 150 (const 30)) []
    delaysFrom 1000 r `shouldSatisfy` steadyWithin 50
    asSent (samples [h | h@(t, _, _) <- heard r, t >= 1000]) `shouldBe` True
    stats (final r) `shouldSatisfy` \(Stats _ down made skips) -> (down, made, skips) == (0, 0, 0)

  it "pulls audio ahead of hearing it without running out" $ do
    let r = simulateFrom 0 (const 40) 2900 (frames 150 (const 30)) []
    delaysFrom 1000 r `shouldSatisfy` steadyWithin 90
    asSent (samples [h | h@(t, _, _) <- heard r, t >= 1000]) `shouldBe` True
    madeUp (stats (final r)) `shouldBe` 0

  it "keeps steady when blocks are pulled in bursts" $ do
    -- Every 40 ms, the blocks heard 40 to 70 ms later.
    let statsBy end = stats (final (simulateFrom 0 (\t -> 40 + t `mod` 40) end (frames 150 (const 30)) []))
    statsBy 2900 `shouldBe` statsBy 1000
    madeUp (statsBy 2900) `shouldBe` 0

  it "waits as long as almost all frames need" $ do
    -- Delays of 20 to 49 ms.
    let r = simulate 5900 (frames 300 (\n -> 20 + n * 7 `mod` 30)) []
    delaysFrom 3000 r `shouldSatisfy` all (\d -> d >= 40 && d <= 70)

  it "fills in a stall, then catches up by speeding up without changing the pitch" $ do
    -- Frames captured from 2.02 s to 2.16 s come together at 2.18 s.
    let r = simulate 5900 (frames 300 (\n -> if n >= 101 && n <= 108 then 2180 - 20 * n else 20)) []
        Stats up _ made skips = stats (final r)
    (up > 0, made > 0, skips) `shouldBe` (True, True, 0)
    last (delaysFrom 5000 r) `shouldSatisfy` (<= 45)
    let caughtUp = samples [h | h@(t, _, _) <- heard r, t >= 2200, t < 3200]
    crossings caughtUp `shouldSatisfy` \c -> c > 450 && c < 470
    quietestPeak caughtUp `shouldSatisfy` (> 9000)
    biggestStep (samples (heard r)) `shouldSatisfy` (< 450)

  it "comes back down after a long stall" $ do
    -- Frames captured from 1 s to 1.5 s come together at 1.52 s.
    let r = simulate 5900 (frames 300 (\n -> if n >= 50 && n <= 75 then 1520 - 20 * n else 20)) []
    last (delaysFrom 5000 r) `shouldSatisfy` (<= 45)

  it "jumps ahead when it starts too far behind to catch up" $ do
    -- The client only starts pulling after a second.
    let r = simulateFrom 1000 (const 0) 2900 (frames 150 (const 20)) []
    skipped (stats (final r)) `shouldBe` 1
    delaysFrom 1100 r `shouldSatisfy` all (<= 45)

  it "plays frames stamped when they were sent, as c-toxcore stamps them" $ do
    -- Sent every 50 ms, two or three at a time.
    let sent n = 50 * ((20 * n + 69) `div` 50)
        r = simulate 5900 [(sent n + 20, sent n, tone n) | n <- [0 .. 299]] []
    stats (final r) `shouldSatisfy` \(Stats _ down made skips) -> (down, made, skips) == (0, 0, 0)
    asSent (samples [h | h@(t, _, _) <- heard r, t >= 1000]) `shouldBe` True
    let played = samples [h | h@(t, _, _) <- heard r, t >= 200]
    crossings played `shouldSatisfy` \c -> c > 450 && c < 470
    quietestPeak played `shouldSatisfy` (> 9000)
    biggestStep played `shouldSatisfy` (< 450)

  it "doesn't run out at the start of a call whose frames jitter" $ do
    -- Delays of 20 to 49 ms.
    let r = simulate 2900 (frames 150 (\n -> 20 + n * 7 `mod` 30)) []
    madeUp (stats (final r)) `shouldBe` 0

  it "keeps steady when a pull now and then is further ahead" $ do
    -- 40 ms ahead, and 70 once a second.
    let statsBy end = stats (final (simulateFrom 0 (\t -> if t `mod` 1000 == 500 then 70 else 40) end (frames 300 (const 30)) []))
    -- Slowed down once for the first, then steady.
    statsBy 5900 `shouldBe` statsBy 1500
    madeUp (statsBy 5900) `shouldBe` 0

  it "makes up audio for frames that never came, and fades back into the rest" $ do
    -- Frames captured from 1 s to 1.06 s never come.
    let r = simulate 2900 [(if n >= 50 && n <= 52 then 10000 else 20 * n + 30, 20 * n, tone n) | n <- [0 .. 149]] []
    -- 60 ms: 6 blocks of 10 ms, or 7 if the gap doesn't line up with them.
    madeUp (stats (final r)) `shouldSatisfy` (`elem` [6, 7])
    delaysFrom 1200 r `shouldSatisfy` steadyWithin 50
    biggestStep (samples (heard r)) `shouldSatisfy` (< 450)

  it "keeps up with a sender whose sound card runs fast" $ do
    -- 960 samples every 19.8 ms of our time.
    let fast = [(round (19.8 * fromIntegral n :: Double) + 20, round (19.8 * fromIntegral n :: Double), tone n) | n <- [0 .. 599 :: Int]]
        r = simulate 11500 fast []
    spedUp (stats (final r)) `shouldSatisfy` (> 0)
    last (delaysFrom 11000 r) `shouldSatisfy` (<= 45)

  it "shows each picture with the audio captured with it" $ do
    -- Audio comes in 20 ms, pictures in 60.
    let r = simulate 3900 (frames 200 (const 20)) [(50 * n + 60, 50 * n) | n <- [0 .. 79]]
        delays = Map.fromList [(t, d) | (t, d, _) <- heard r]
    [ abs (d - Map.findWithDefault 0 t delays) | (t, d) <- shown r, t >= 2000 ] `shouldSatisfy` all (<= 10)
    delaysFrom 2000 r `shouldSatisfy` all (>= 60)

  it "shows each picture with its audio when they're stamped as c-toxcore stamps them" $ do
    -- Audio stamped once its 20 ms are captured, pictures once encoded, 15 ms
    -- after their capture.
    let r = simulate 3900 [(20 * n + 40, 20 * n + 20, tone n) | n <- [0 .. 199]]
                          [(50 * n + 35, 50 * n + 15) | n <- [0 .. 79]]
        heardCaptured = Map.fromList [(t, d + 20) | (t, d, _) <- heard r]
    [ abs (d + 15 - Map.findWithDefault 0 t heardCaptured) | (t, d) <- shown r, t >= 2000 ] `shouldSatisfy` all (<= 10)

  it "shows pictures after their own delay when there's no audio" $ do
    let r = simulate 1900 [] [(50 * n + 40, 50 * n) | n <- [0 .. 39]]
    [d | (t, d) <- shown r, t >= 1000] `shouldSatisfy` all (== 40)

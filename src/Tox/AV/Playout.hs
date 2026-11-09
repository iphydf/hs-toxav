{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | When to play a call's audio and show its video, as WebRTC does: one delay
-- behind the sender for both, as long as the slower of the two needs. Audio
-- plays without breaks, a pitch period faster or slower at a time to keep to
-- that delay, and is made up when it runs out. Each picture shows when the
-- audio captured with it is heard.
module Tox.AV.Playout
  ( Playout
  , empty
  , audio
  , picture
  , pull
  , pictureAt
  , Stats (..)
  , stats
  ) where

import qualified Data.IntMap.Strict   as IntMap
import           Data.List.NonEmpty   (nonEmpty)
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Sequence        (Seq (..), ViewL (..), (|>))
import qualified Data.Sequence        as Seq
import qualified Data.Vector.Storable as V
import           Data.Word            (Word16)

import           Tox.AV.Codec.Types   (AudioFrame (..), Pcm, Picture)
import           Tox.AV.RTP           (MediaTime, ahead, later, since)

-- | Milliseconds from the first frame, on the sender's clock or ours.
type Ms = Double

data Playout = Playout
  { bases       :: Maybe (MediaTime, MediaTime) -- ^ The first frame's capture, and when it came.
  , audioDelays :: Delays
  , videoDelays :: Delays
  , pending     :: Seq (Ms, Ms, AudioFrame)
    -- ^ Where each frame goes, and how many ms of audio before it never came.
  , lastAudio   :: Maybe (Word16, Ms, Ms)
    -- ^ The last frame's number, where it went, and how long it was.
  , stream      :: Maybe Stream
  , pictures    :: Seq (Ms, MediaTime, Picture)
  , audioLeads  :: Leads -- ^ How far ahead audio was pulled.
  , videoLeads  :: Leads -- ^ How far ahead pictures were asked for.
  , stats       :: Stats
  }

-- | How the audio got by so far.
data Stats = Stats
  { spedUp     :: Int -- ^ Pitch periods left out.
  , slowedDown :: Int -- ^ Pitch periods played twice.
  , madeUp     :: Int -- ^ Blocks we made up, as audio never came or ran out.
  , skipped    :: Int -- ^ Jumps ahead when too far behind.
  } deriving (Eq, Show)

data Stream = Stream
  { streamRate     :: Int
  , streamChannels :: Int
  , position       :: Ms  -- ^ When the first buffered sample was captured.
  , buffered       :: Pcm
  , history        :: Pcm -- ^ What played last.
  , concealing     :: Maybe (Pcm, Int)
    -- ^ The pitch period we repeat, and the samples per channel made up so far.
  , heardBehind    :: Ms  -- ^ How long after its capture the last block is heard.
  }

empty :: Playout
empty = Playout
  { bases = Nothing
  , audioDelays = noDelays
  , videoDelays = noDelays
  , pending = Seq.empty
  , lastAudio = Nothing
  , stream = Nothing
  , pictures = Seq.empty
  , audioLeads = Seq.empty
  , videoLeads = Seq.empty
  , stats = Stats 0 0 0 0
  }

-- | Audio is pulled and played this many ms at a time.
blockMs :: Ms
blockMs = 10

-- | Pitch periods of voices: 66 to 400 Hz.
minPeriodMs, maxPeriodMs :: Ms
minPeriodMs = 2.5
maxPeriodMs = 15

-- | Longer than this behind, we jump ahead: speeding up would take too long.
-- Gaps longer than this start the audio over.
tooFarBehind :: Ms
tooFarBehind = 200

-- | How long made-up audio takes to fade out.
fadeMs :: Ms
fadeMs = 60

-- | At most this many pictures wait, as they're big.
maxPictures :: Int
maxPictures = 30

-- | At most this many audio frames wait to be pulled, so that a client that
-- doesn't pull audio doesn't fill its memory.
maxPending :: Int
maxPending = 100

-- | Numbers count up by one a frame, so each frame follows the last, after
-- any that never came. Times only nudge where frames go: they jitter,
-- c-toxcore's are when they were sent, and the sender's clock drifts from its
-- samples.
audio :: MediaTime -> Word16 -> MediaTime -> AudioFrame -> Playout -> Playout
audio came number captured frame p0 = p
  { audioDelays = addDelay (c - t) (audioDelays p)
  , pending = bounded (pending p |> (t, missing, frame))
  , lastAudio = Just (number, t, frameMs frame)
  }
  where
    p = based captured came p0
    (s, c) = (senderMs p captured, ourMs p came)
    (t, missing) = case lastAudio p of
      Just (n, at, len)
        | k > 0, abs (s - e) <= tooFarBehind -> (e + (s - e) / 10, fromIntegral (k - 1) * len)
        where
          k = ahead n number
          e = at + fromIntegral k * len
      _ -> (s, 0)

-- | Leaves out the oldest frames past 'maxPending', as if they never came.
bounded :: Seq (Ms, Ms, AudioFrame) -> Seq (Ms, Ms, AudioFrame)
bounded = \case
  (_, missing, frame) :<| (t, missing', next) :<| rest
    | Seq.length rest + 2 > maxPending -> bounded ((t, missing + frameMs frame + missing', next) :<| rest)
  waiting -> waiting

frameMs :: AudioFrame -> Ms
frameMs (AudioFrame rate channels pcm) = fromIntegral (V.length pcm `div` channels) * 1000 / fromIntegral rate

picture :: MediaTime -> MediaTime -> Picture -> Playout -> Playout
picture came captured pic p0 = p
  { videoDelays = addDelay (c - s) (videoDelays p)
  , pictures = Seq.drop (Seq.length waiting - maxPictures) waiting
  }
  where
    p = based captured came p0
    (s, c) = (senderMs p captured, ourMs p came)
    waiting = pictures p |> (s, captured, pic)

based :: MediaTime -> MediaTime -> Playout -> Playout
based captured came p = maybe p { bases = Just (captured, came) } (const p) (bases p)

senderMs, ourMs :: Playout -> MediaTime -> Ms
senderMs p t = maybe 0 (fromIntegral . since t . fst) (bases p)
ourMs p t = maybe 0 (fromIntegral . since t . snd) (bases p)

-- | The delay both audio and video aim for, as long as the slower needs, and
-- the least audio may fall to: three quarters of its delay (as in NetEQ)
-- plus how far ahead it's pulled, but never under video's need (as WebRTC
-- does). Each needs to have come by the time it's pulled or asked for.
target :: Playout -> Maybe (Ms, Ms)
target p = aim <$> nonEmpty (catMaybes [audioNeed, videoNeed])
  where
    (audioAhead, videoAhead) = (furthest (audioLeads p), furthest (videoLeads p))
    audioDelay = enough (audioDelays p)
    audioNeed = (+ audioAhead) <$> audioDelay
    videoNeed = (+ videoAhead) <$> enough (videoDelays p)
    aim needs = (maximum needs, max (maybe 0 ((+ audioAhead) . (* 0.75)) audioDelay) (fromMaybe 0 videoNeed))

-- | The next 10 ms of audio, pulled at one time to be heard from another,
-- and when its first sample was captured. Nothing until the audio starts.
pull :: MediaTime -> MediaTime -> Playout -> (Maybe (MediaTime, AudioFrame), Playout)
pull at heardAt p0 = case (bases p, target p, stream p) of
  (Just (first, _), Just aim, Just st) ->
    let (captured, block, st', counts) = play now aim st (stats p)
    in (Just (later (round captured) first, block), p { stream = Just st', stats = counts })
  _ -> (Nothing, p)
  where
    timed = p0 { audioLeads = lead (ourMs p0 heardAt) (ourMs p0 heardAt - ourMs p0 at) (audioLeads p0) }
    now = ourMs timed heardAt
    p = maybe timed (\(d, _) -> admit now d timed) (target timed)

-- | Moves waiting frames into the audio, starting it once the first has
-- waited the delay.
admit :: Ms -> Ms -> Playout -> Playout
admit now d p = case (stream p, Seq.viewl (pending p)) of
  (_, EmptyL) -> p
  (Nothing, (t, _, frame) :< rest)
    | now - t >= d -> admit now d p { stream = Just (start t frame), pending = rest }
    | otherwise -> p
  (Just st, (t, missing, frame) :< rest) -> case append t missing frame st of
    Just (st', made) -> admit now d p { stream = Just st', pending = rest, stats = madeUpFor made (stats p) }
    Nothing -> admit now d p { stream = Nothing }
  where
    start s (AudioFrame rate channels pcm) = Stream rate channels s pcm V.empty Nothing 0
    madeUpFor made counts = counts { madeUp = madeUp counts + ceiling (made / blockMs) }

-- | Adds a frame after audio made up for what never came before it, and
-- returns how many ms it made up. Nothing to start over, after a long gap or
-- with a new sampling rate.
append :: Ms -> Ms -> AudioFrame -> Stream -> Maybe (Stream, Ms)
append t missing (AudioFrame rate channels pcm) st
  | (rate, channels) /= (streamRate st, streamChannels st) || abs gap > tooFarBehind = Nothing
  | otherwise = Just
      ( st { position = position st + gap - durationMs st made
           , buffered = buffered st <> made <> pcm'
           , concealing = if V.null made then concealing st else Nothing
           }
      , durationMs st made )
  where
    gap = t - (position st + durationMs st (buffered st))
    -- Audio made up as it ran out stands in for some of it.
    stoodIn = case concealing st of
      Just (_, done) | V.null (buffered st) -> fromIntegral done * 1000 / fromIntegral (streamRate st)
      _ -> 0
    (made, pcm')
      | missing > stoodIn = bridge st (samplesIn st (missing - stoodIn)) pcm
      | otherwise = (V.empty, pcm)

-- | Audio made up from the end of the audio so far for samples that never
-- came, and the frame after them faded in from more of it.
bridge :: Stream -> Int -> Pcm -> (Pcm, Pcm)
bridge st n pcm = (made, crossfade st more (V.take (m * c) pcm) <> V.drop (m * c) pcm)
  where
    c = streamChannels st
    m = min (samplesIn st minPeriodMs) (frames st pcm)
    before = st
      { history = lastFrames st (2 * samplesIn st maxPeriodMs) (history st <> buffered st)
      , concealing = if V.null (buffered st) then concealing st else Nothing
      }
    (made, going) = conceal before n
    (more, _) = conceal before { concealing = going } m

durationMs :: Stream -> Pcm -> Ms
durationMs st pcm = fromIntegral (frames st pcm) * 1000 / fromIntegral (streamRate st)

-- | Samples per channel.
frames :: Stream -> Pcm -> Int
frames st pcm = V.length pcm `div` streamChannels st

samplesIn :: Stream -> Ms -> Int
samplesIn st ms = round (ms * fromIntegral (streamRate st) / 1000)

-- | The next block, when its first sample was captured, and the stream after
-- it.
play :: Ms -> (Ms, Ms) -> Stream -> Stats -> (Ms, AudioFrame, Stream, Stats)
play now aim st0 counts0 = (position st2, AudioFrame (streamRate st2) (streamChannels st2) block, st', counts2)
  where
    st1 = merge st0
    (st2, counts1) = adjust now aim st1 counts0
    blockFrames = samplesIn st2 blockMs
    available = frames st2 (buffered st2)
    real = V.take (blockFrames * streamChannels st2) (buffered st2)
    (madeUpPcm, concealing')
      | available >= blockFrames = (V.empty, Nothing)
      | otherwise = conceal st2 { history = history st2 <> real } (blockFrames - available)
    block = real <> madeUpPcm
    st' = st2
      { position = position st2 + durationMs st2 real
      , buffered = V.drop (V.length real) (buffered st2)
      , history = lastFrames st2 (2 * samplesIn st2 maxPeriodMs) (history st2 <> block)
      , concealing = concealing'
      , heardBehind = now - position st2
      }
    counts2 = if V.null madeUpPcm then counts1 else counts1 { madeUp = madeUp counts1 + 1 }

lastFrames :: Stream -> Int -> Pcm -> Pcm
lastFrames st n pcm = V.drop (V.length pcm - n * streamChannels st) pcm

-- | Speeds up or slows down a pitch period to keep to the delay, or jumps
-- ahead when far behind. We speed up above the delay and slow down below
-- the low mark, at least 20 ms apart as in NetEQ.
adjust :: Ms -> (Ms, Ms) -> Stream -> Stats -> (Stream, Stats)
adjust now (d, low) st counts
  | behind - d > tooFarBehind, have > blockMs =
      let cut = min (have - blockMs) (behind - d)
      in (cutOut cut st, counts { skipped = skipped counts + 1 })
  | behind > high, Just l <- splice, have - ms l >= blockMs =
      (st { position = position st + ms l, buffered = removePeriod st l (buffered st) }, counts { spedUp = spedUp counts + 1 })
  | behind < low, Just l <- splice =
      (st { position = position st - ms l, buffered = insertPeriod st l (buffered st) }, counts { slowedDown = slowedDown counts + 1 })
  | otherwise = (st, counts)
  where
    behind = now - position st
    have = durationMs st (buffered st)
    high = max d (low + 20)
    ms l = fromIntegral l * 1000 / fromIntegral (streamRate st)
    splice = alikePeriod st (buffered st)

-- | Leaves out some milliseconds at the start, fading across the cut.
cutOut :: Ms -> Stream -> Stream
cutOut cut st = st
  { position = position st + durationMs st (V.take (n * c) pcm)
  , buffered = crossfade st (V.take (m * c) pcm) (V.take (m * c) (V.drop (n * c) pcm)) <> V.drop ((n + m) * c) pcm
  }
  where
    pcm = buffered st
    c = streamChannels st
    n = samplesIn st cut
    m = min (samplesIn st minPeriodMs) (frames st pcm - n)

-- | A pitch period at the start that repeats closely enough to leave out or
-- play twice unheard. Anything goes when it's quiet.
alikePeriod :: Stream -> Pcm -> Maybe Int
alikePeriod st pcm
  | V.length x < 2 * samplesIn st minPeriodMs = Nothing
  | quiet || alike >= 0.9 = Just l
  | otherwise = Nothing
  where
    x = mono st (V.take (2 * samplesIn st maxPeriodMs * streamChannels st) pcm)
    (l, alike) = period (streamRate st) x
    -- Below -40 dBFS.
    quiet = V.sum (V.map (^ (2 :: Int)) x) / fromIntegral (max 1 (V.length x)) < (0.01 * 32768) ^ (2 :: Int)

removePeriod, insertPeriod :: Stream -> Int -> Pcm -> Pcm
removePeriod st l pcm = crossfade st (part 0) (part l) <> V.drop (2 * l * streamChannels st) pcm
  where part i = V.slice (i * streamChannels st) (l * streamChannels st) pcm
insertPeriod st l pcm = part 0 <> crossfade st (part l) (part 0) <> V.drop (l * streamChannels st) pcm
  where part i = V.slice (i * streamChannels st) (l * streamChannels st) pcm

crossfade :: Stream -> Pcm -> Pcm -> Pcm
crossfade st from to = V.imap mix to
  where
    n = frames st to
    mix i b =
      let w = (fromIntegral (i `div` streamChannels st) + 0.5) / fromIntegral n :: Double
      in round (fromIntegral (from V.! i) * (1 - w) + fromIntegral b * w)

-- | Fades from the made-up audio into the real audio that came after it.
merge :: Stream -> Stream
merge st = case concealing st of
  Just _ | not (V.null (buffered st)) ->
    let m = min (samplesIn st minPeriodMs) (frames st (buffered st))
        (made, _) = conceal st m
        pcm = buffered st
    in st { buffered = crossfade st made (V.take (m * streamChannels st) pcm) <> V.drop (m * streamChannels st) pcm
          , concealing = Nothing }
  _ -> st

-- | Audio made up from the last pitch period heard, fading out, and how far
-- we've got.
conceal :: Stream -> Int -> (Pcm, Maybe (Pcm, Int))
conceal st n = done' `seq` (V.generate (n * c) sample, Just (repeated, done'))
  where
    c = streamChannels st
    -- Added up now, or each block's count would hold on to all before it.
    done' = done + n
    (repeated, done) = case concealing st of
      Just going -> going
      Nothing    -> (lastPeriod st, 0)
    l = max 1 (V.length repeated `div` c)
    fade = fromIntegral (samplesIn st fadeMs) :: Double
    sample i =
      let j = done + i `div` c
          gain = max 0 (1 - fromIntegral j / fade)
      in if V.null repeated then 0 else round (fromIntegral (repeated V.! ((j `mod` l) * c + i `mod` c)) * gain)

lastPeriod :: Stream -> Pcm
lastPeriod st
  | frames st h < 2 * samplesIn st maxPeriodMs = V.empty
  | otherwise = lastFrames st (fst (period (streamRate st) (mono st h))) h
  where h = history st

-- | The channels' mean.
mono :: Stream -> Pcm -> V.Vector Double
mono st pcm = V.generate (frames st pcm) $ \i ->
  sum [fromIntegral (pcm V.! (i * c + k)) | k <- [0 .. c - 1]] / fromIntegral c
  where c = streamChannels st

-- | The pitch period in samples at the start, and how alike the two periods
-- after each other are, 1 at most. Searched at 4 kHz first, as NetEQ does.
period :: Int -> V.Vector Double -> (Int, Double)
period rate x = best (similarity x) [max shortest (rough * factor - factor) .. min longest (rough * factor + factor)]
  where
    factor = max 1 (rate `div` 4000)
    down = V.generate (V.length x `div` factor) $ \i -> V.sum (V.slice (i * factor) factor x)
    rough = fst (best (similarity down) [shortest `div` factor .. longest `div` factor])
    shortest = round (minPeriodMs * fromIntegral rate / 1000)
    longest = min (V.length x `div` 2) (round (maxPeriodMs * fromIntegral rate / 1000))
    best f ls = foldr (\l b -> let v = f l in if v > snd b then (l, v) else b) (shortest, -1) ls

-- | Normalised correlation of the first two stretches of a length.
similarity :: V.Vector Double -> Int -> Double
similarity x l
  | 2 * l > V.length x || energy == 0 = 0
  | otherwise = V.sum (V.zipWith (*) a b) / energy
  where
    a = V.slice 0 l x
    b = V.slice l l x
    energy = sqrt (V.sum (V.map (^ (2 :: Int)) a) * V.sum (V.map (^ (2 :: Int)) b))

-- | The picture to show, asked for at one time to be seen from another, if
-- a new one is due, and when it was captured: with the audio captured with
-- it, or after the delay when no audio plays.
pictureAt :: MediaTime -> MediaTime -> Playout -> (Maybe (MediaTime, Picture), Playout)
pictureAt at seenAt p0 = case delay of
  Nothing -> (Nothing, p)
  Just d ->
    let (due, rest) = Seq.spanl (\(s, _, _) -> s + d <= now) (pictures p)
    in case Seq.viewr due of
      Seq.EmptyR -> (Nothing, p)
      _ Seq.:> (_, captured, pic) -> (Just (captured, pic), p { pictures = rest })
  where
    p = p0 { videoLeads = lead (ourMs p0 seenAt) (ourMs p0 seenAt - ourMs p0 at) (videoLeads p0) }
    now = ourMs p seenAt
    delay = case stream p of
      Just st | maybe True ((< samplesIn st fadeMs) . snd) (concealing st) -> Just (heardBehind st)
      _ -> fst <$> target p

-- | When each lead came and how long it was, longest first.
type Leads = Seq (Ms, Ms)

-- | Leads that come back within this long hold the delay, so that it
-- doesn't swing with each, as NetEQ holds delay peaks.
leadMemory :: Ms
leadMemory = 10000

lead :: Ms -> Ms -> Leads -> Leads
lead now l leads = Seq.dropWhileL ((< now - leadMemory) . fst) (Seq.dropWhileR ((<= l) . snd) leads |> (now, l))

furthest :: Leads -> Ms
furthest = maybe 0 snd . Seq.lookup 0

-- | Weights of the delays from capture to us, by whole milliseconds. Newer
-- ones weigh more: the first field is the weight the next one gets.
data Delays = Delays Double (IntMap.IntMap Double)

noDelays :: Delays
noDelays = Delays 1 IntMap.empty

-- | The first frame also counts as one that came this many ms later, as
-- NetEQ starts at 80 ms. Until a few have come, they don't say what's enough.
startDelay :: Int
startDelay = 80

-- | Each delay counts this much less with each that comes after it, as in
-- NetEQ: about a second of audio.
forget :: Double
forget = 0.983

addDelay :: Ms -> Delays -> Delays
addDelay d (Delays _ weights0) | IntMap.null weights0 = addDelay d (Delays (1 / forget) (IntMap.singleton (round d + startDelay) 1))
addDelay d (Delays w weights)
  | w' < 1e100 = Delays w' weights'
  -- Weigh the newest 1 again before the numbers grow too big, forgetting
  -- the delays that no longer count.
  | otherwise = Delays 1 (IntMap.filter (> 1e-9) (IntMap.map (/ w') weights'))
  where
    w' = w / forget
    weights' = IntMap.insertWith (+) (round d) w weights

-- | The delay 95% of frames came within, as NetEQ aims for.
enough :: Delays -> Maybe Ms
enough (Delays _ weights) = go 0 (IntMap.toDescList weights)
  where
    allowed = 0.05 * sum weights
    go late ((d, w) : rest)
      | late + w <= allowed = go (late + w) rest
      | otherwise = Just (fromIntegral d)
    go _ [] = Nothing

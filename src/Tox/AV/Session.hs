{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | Calls with friends, without the codecs: what to send and what to tell the
-- client for each thing that happens.
module Tox.AV.Session
  ( Session
  , new
  , Codec (..)
  , BitRates (..)
  , Output (..)
  , Event (..)
  , End (..)
  , Refusal (..)
  , Control (..)
  , losslessIds
  , lossyIds
  , connection
  , receive
  , tick
  , call
  , answer
  , control
  , setBitRate
  , sendAudio
  , sendVideo
  , canSend
  , wantsKeyFrame
  , videoCodec
  , encoderFailed
  , failedEncoders
  , bitRates
  , VideoStats (..)
  , videoStats
  , pictureDecoded
  , decodeFailed
  , kill
  ) where

import           Control.Monad           (void)
import           Data.Bits               (complement, (.&.))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe, isJust, isNothing)
import qualified Data.Set                as Set
import           Data.Word               (Word16, Word32, Word64, Word8)

import qualified Tox.AV.Assembler        as Assembler
import qualified Tox.AV.Bandwidth        as Bandwidth
import qualified Tox.AV.JitterBuffer     as JitterBuffer
import qualified Tox.AV.MSI              as MSI
import           Tox.AV.MSI              (Capabilities, Message (..),
                                          Request (..), receiveAudio,
                                          receiveVideo)
import qualified Tox.AV.RTP              as RTP
import           Tox.AV.RTP              (Codec (..), Frame (..), Media (..),
                                          MediaTime (..), Piece (..))

-- | Kilobits per second, 0 for not sending. What the client sets is the
-- most a call sends: it sends less while the friend reports loss.
data BitRates = BitRates
  { audioBitRate :: Word32
  , videoBitRate :: Word32
  } deriving (Eq, Show)

data Phase = Requesting | Requested | Active
  deriving (Eq, Show)

data Call = Call
  { callPhase      :: Phase
  , callSelf       :: Capabilities
  , callPaused     :: Maybe Capabilities -- ^ What we sent and accepted before pausing.
  , callPeer       :: Capabilities
  , callRates      :: BitRates -- ^ What the client set.
  , callCurrent    :: BitRates -- ^ What we send at, up to 'callRates'.
  , callClean      :: Int -- ^ Reports in a row with little loss.
  , callVideoMoved :: Maybe Word64 -- ^ When the video bit rate last changed.
  , callSequences  :: Map.Map Media Word16
  , callVideoSent  :: Int
  , callKeyFrame   :: Bool -- ^ The friend asked for one.
  , callAssemblers :: Map.Map Media Assembler.Assembler
  , callAudio      :: JitterBuffer.JitterBuffer (MediaTime, ByteString)
  , callLastVideo  :: Maybe Word16
  , callSmearing   :: Maybe MediaTime
    -- ^ When the first frame after a lost one was sent, until a key frame.
  , callVideoStats :: VideoStats
  , callMeter      :: Bandwidth.Meter
  , callLastReport :: Maybe Word64
  , callHeard      :: Word64
    -- ^ When the friend last reported, or the call began.
  , callPeerH264   :: Bool
  , callFailed     :: Map.Map Codec String -- ^ Encoders that didn't work, and why.
  }

-- | How the friend's video came, this call. A frame decoded after a lost one
-- looks wrong until a key frame, which decodes on its own: the smear.
data VideoStats = VideoStats
  { framesCame       :: Int
  , framesIncomplete :: Int -- ^ Frames with pieces that never came.
  , framesLost       :: Int -- ^ Including incomplete ones.
  , keyFramesCame    :: Int
  , smears           :: Int
  , smearedFrames    :: Int
  , smearedMs        :: Int -- ^ Only smears that a key frame ended.
  , picturesDecoded  :: Int
  , decodeFailures   :: Int
  } deriving (Eq, Show)

data Session f = Session
  { sessionSsrc      :: Word32
  , sessionDecoders  :: Set.Set Codec
  , sessionEncoders  :: Set.Set Codec
  , sessionConnected :: Set.Set f
  , sessionCalls     :: Map.Map f Call
  }

-- | A session with a random SSRC for the frames it sends, and the codecs the
-- client can decode and encode.
new :: Word32 -> [Codec] -> [Codec] -> Session f
new ssrc decoders encoders =
  Session ssrc (Set.fromList decoders) (Set.fromList encoders) Set.empty Map.empty

data Output f
  = SendLossless f ByteString
  | SendLossy f ByteString
  | Event (Event f)
  | AudioIn f Word16 (Maybe (MediaTime, ByteString))
    -- ^ The frame's number, and when it was sent with its sampling rate and
    -- Opus packet ('Nothing' if it was lost).
  | VideoIn f Codec MediaTime ByteString
  deriving (Eq, Show)

data Event f
  = Invited f Capabilities
  | PeerChanged f Capabilities
    -- ^ The friend answered, or changed what they send and accept.
  | Ended f End
  deriving (Eq, Show)

data End
  = HungUp
  | PeerFailed MSI.Error
  | Failed MSI.Error -- ^ The friend broke the protocol and was told so.
  | Disconnected
  deriving (Eq, Show)

data Refusal
  = InvalidBitRate
  | FriendNotConnected
  | AlreadyInCall
  | NotCalling
  | NotInCall
  | InvalidTransition
  | Disabled
  deriving (Eq, Show)

data Control
  = Resume
  | Pause
  | Cancel
  | MuteAudio
  | UnmuteAudio
  | HideVideo
  | ShowVideo
  deriving (Eq, Show, Enum, Bounded)

msiId, videoControlId :: Word8
msiId = 69
-- | Followed by 'askKeyFrame' or 'decodesH264'.
videoControlId = 172

askKeyFrame, decodesH264 :: Word8
askKeyFrame = 0
decodesH264 = 1

-- | When a call starts, we tell the friend what we decode besides VP8.
decodable :: f -> Session f -> [Output f]
decodable f s = [SendLossless f (BS.pack [videoControlId, decodesH264]) | Set.member H264 (sessionDecoders s)]

-- | The lossless and lossy packet ids the client passes to 'receive'.
losslessIds, lossyIds :: [Word8]
losslessIds = [msiId, videoControlId]
lossyIds = [RTP.packetId Audio, RTP.packetId Video, Bandwidth.packetId]

-- | Each call starts with this many key frames, as C toxcore has no way to
-- ask for one.
startKeyFrames :: Int
startKeyFrames = 7

-- | Reports closer together than this are ignored.
minReportGap :: Word64
minReportGap = 950

-- | Loss from this on lowers what we send. Loss up to 'cleanLoss' in
-- 'cleanReports' reports in a row raises it by 'raiseStep', up to what the
-- client set.
tolerableLoss, cleanLoss, raiseStep :: Double
tolerableLoss = 0.1
cleanLoss = 0.01
raiseStep = 0.1

cleanReports :: Int
cleanReports = 3

-- | toxcore reports only loss, so this long without a report counts as one
-- without loss.
quietGap :: Word64
quietGap = 2 * Bandwidth.interval

-- | Each change to the video bit rate makes a new encoder, which starts with
-- a key frame, so changes come at least this far apart and move it by at
-- least 'minVideoMove' of what it was, unless they reach what the client set.
videoMoveGap :: Word64
videoMoveGap = 5000

minVideoMove :: Double
minVideoMove = 0.1

minAudioRate, minVideoRate :: Word32
minAudioRate = 6
minVideoRate = 100

type Step f = (Session f, [Output f])

newCall :: Word64 -> Phase -> Capabilities -> Capabilities -> BitRates -> Call
newCall now phase self peer rates = Call
  { callPhase = phase
  , callSelf = self
  , callPaused = Nothing
  , callPeer = peer
  , callRates = rates
  , callCurrent = rates
  , callClean = 0
  , callVideoMoved = Nothing
  , callSequences = Map.empty
  , callVideoSent = 0
  , callKeyFrame = False
  , callAssemblers = Map.empty
  , callAudio = JitterBuffer.empty
  , callLastVideo = Nothing
  , callSmearing = Nothing
  , callVideoStats = VideoStats 0 0 0 0 0 0 0 0 0
  , callMeter = Bandwidth.meter now
  , callLastReport = Nothing
  , callHeard = now
  , callPeerH264 = False
  , callFailed = Map.empty
  }

capabilities :: BitRates -> Capabilities
capabilities (BitRates a v) = mconcat $
  [receiveAudio, receiveVideo]
  ++ [MSI.sendAudio | a > 0]
  ++ [MSI.sendVideo | v > 0]

has :: Capabilities -> Capabilities -> Bool
has flag caps = caps .&. flag == flag

without :: Capabilities -> Capabilities -> Capabilities
without flag caps = caps .&. complement flag

validRates :: BitRates -> Bool
validRates (BitRates a v) = (a == 0 || (a >= 6 && a <= 510)) && v <= 1000000

msi :: f -> Request -> Maybe MSI.Error -> Maybe Capabilities -> Output f
msi f request err caps = SendLossless f (BS.cons msiId (MSI.encode (Message request err caps)))

withCall :: Ord f => f -> Call -> Session f -> Session f
withCall f c s = s { sessionCalls = Map.insert f c (sessionCalls s) }

withoutCall :: Ord f => f -> Session f -> Session f
withoutCall f s = s { sessionCalls = Map.delete f (sessionCalls s) }

-- | The friend came online or went offline. Going offline ends the call
-- without a word, ringing or not.
connection :: Ord f => f -> Bool -> Session f -> Step f
connection f online s
  | online = (s { sessionConnected = Set.insert f (sessionConnected s) }, [])
  | Map.member f (sessionCalls s) = (withoutCall f s', [Event (Ended f Disconnected)])
  | otherwise = (s', [])
  where
    s' = s { sessionConnected = Set.delete f (sessionConnected s) }

-- | A packet from the friend with one of 'losslessIds' or 'lossyIds', its id
-- included.
receive :: Ord f => Word64 -> f -> ByteString -> Session f -> Step f
receive now f packet s = case BS.uncons packet of
  Just (ident, body)
    | ident == msiId -> receiveMsi now f body s
    | ident == videoControlId -> receiveVideoControl f body s
    | ident == Bandwidth.packetId -> receiveReport now f packet s
    | Just piece <- RTP.parse packet -> receivePiece f piece s
  _ -> (s, [])

receiveMsi :: Ord f => Word64 -> f -> ByteString -> Session f -> Step f
receiveMsi now f body s = case MSI.decode body of
  Nothing -> (s, [msi f Pop (Just MSI.InvalidMessage) Nothing])
  Just (Message request err caps) -> case (request, Map.lookup f (sessionCalls s)) of
    (Init, Nothing) -> case caps of
      Nothing -> (s, [msi f Pop (Just MSI.InvalidMessage) Nothing])
      Just peer ->
        (withCall f (newCall now Requested mempty peer (BitRates 0 0)) s, [Event (Invited f peer)])
    (Init, Just c) -> case (callPhase c, caps) of
      (_, Nothing) -> failed MSI.InvalidMessage
      (Active, Just peer) ->
        let (s', changed) = peerChanged c peer
        in (s', changed ++ [msi f Push Nothing (Just (callSelf c))])
      _ -> failed MSI.InvalidState
    -- A friend that hangs up as we do must get no answer, or the two would
    -- answer each other for ever.
    (Pop, Nothing) -> (s, [])
    (Pop, Just _) ->
      (withoutCall f s, [Event (Ended f (maybe HungUp PeerFailed err))])
    (Push, Nothing) -> (s, [msi f Pop (Just MSI.StrayMessage) Nothing])
    (Push, Just c) -> case (callPhase c, caps) of
      (_, Nothing) -> failed MSI.InvalidMessage
      (Requested, _) -> (s, [])
      (Active, Just peer) -> peerChanged c peer
      (Requesting, Just peer) ->
        (withCall f c { callPhase = Active, callPeer = peer } s, Event (PeerChanged f peer) : decodable f s)
  where
    failed e = (withoutCall f s, [msi f Pop (Just e) Nothing, Event (Ended f (Failed e))])
    peerChanged c peer
      | peer == callPeer c = (s, [])
      | otherwise = (withCall f c { callPeer = peer } s, [Event (PeerChanged f peer)])

receiveVideoControl :: Ord f => f -> ByteString -> Session f -> Step f
receiveVideoControl f body s = case (BS.unpack body, Map.lookup f (sessionCalls s)) of
  ([kind], Just c)
    | kind == askKeyFrame -> (withCall f c { callKeyFrame = True } s, [])
    | kind == decodesH264 -> (withCall f c { callPeerH264 = True } s, [])
  _ -> (s, [])

-- | How much of our media got through to the friend. What we send follows
-- it.
receiveReport :: Ord f => Word64 -> f -> ByteString -> Session f -> Step f
receiveReport now f packet s = case (Bandwidth.decode packet, Map.lookup f (sessionCalls s)) of
  (Just report, Just c)
    | callPhase c == Active
    , maybe True (\t -> now - t >= minReportGap) (callLastReport c) ->
        (withCall f (adapt now (Bandwidth.loss report) c { callLastReport = Just now, callHeard = now }) s, [])
  _ -> (s, [])

-- | Lowers video for loss, or audio if there is no video, and raises video
-- back first, then audio.
adapt :: Word64 -> Double -> Call -> Call
adapt now l c
  | l >= tolerableLoss = lower c { callClean = 0 }
  | l > cleanLoss = c { callClean = 0 }
  | callClean c + 1 < cleanReports = c { callClean = callClean c + 1 }
  | otherwise = raise c { callClean = 0 }
  where
    current = callCurrent c
    ceilings = callRates c
    scaled by rate = round (fromIntegral rate * by) :: Word32
    lower c'
      | videoBitRate current > 0 = video (scaled (1 - l)) c'
      | otherwise = audio (scaled (1 - l)) c'
    raise c'
      | videoBitRate current < videoBitRate ceilings = video (scaled (1 + raiseStep)) c'
      | otherwise = audio (scaled (1 + raiseStep)) c'
    audio f c' = case audioBitRate current of
      0 -> c'
      rate -> c' { callCurrent = current { audioBitRate = within minAudioRate (audioBitRate ceilings) (f rate) } }
    video f c' = case videoBitRate current of
      0 -> c'
      rate
        | maybe False (\t -> now - t < videoMoveGap) (callVideoMoved c') -> c'
        | moved < minVideoMove * fromIntegral rate && next /= videoBitRate ceilings -> c'
        | next == rate -> c'
        | otherwise -> c' { callCurrent = current { videoBitRate = next }, callVideoMoved = Just now }
        where
          next = within minVideoRate (videoBitRate ceilings) (f rate)
          moved = abs (fromIntegral next - fromIntegral rate) :: Double
    within low high = max (min low high) . min high

receivePiece :: Ord f => f -> Piece -> Session f -> Step f
receivePiece f piece s = case Map.lookup f (sessionCalls s) of
  Just c | callPhase c == Active, has accepting (callSelf c) ->
    let assembler = Map.findWithDefault Assembler.empty m (callAssemblers c)
        (assembler', outcomes) = Assembler.add piece assembler
        c' = c { callAssemblers = Map.insert m assembler' (callAssemblers c)
               , callMeter = Bandwidth.received (BS.length (pieceData piece)) (callMeter c)
               }
        (c'', outputs) = foldl outcome (c', []) outcomes
    in (withCall f c'' s, outputs)
  _ -> (s, [])
  where
    m = pieceMedia piece
    accepting = if m == Audio then receiveAudio else receiveVideo
    outcome (c, outs) = \case
      Assembler.Incomplete missing ->
        let c' = c { callMeter = Bandwidth.lost missing (callMeter c) }
        in (if m == Video then counted (\v -> v { framesIncomplete = framesIncomplete v + 1 }) c' else c', outs)
      Assembler.Complete sequ time flags bytes -> case m of
        -- Frames that never came count as lost, each as long as this one:
        -- audio goes at a constant bit rate.
        Audio ->
          let (ready, buffer) = JitterBuffer.drain (JitterBuffer.insert sequ (time, bytes) (callAudio c))
              missing = length [() | (_, Nothing) <- ready] * BS.length bytes
          in (c { callAudio = buffer, callMeter = Bandwidth.lost missing (callMeter c) }, outs ++ map (uncurry (AudioIn f)) ready)
        Video
          | maybe True (\l -> RTP.ahead l sequ > 0) (callLastVideo c) ->
              let c' = videoCame (maybe 0 (\l -> RTP.ahead l sequ - 1) (callLastVideo c)) (RTP.hasFlags RTP.keyFrame flags) time c { callLastVideo = Just sequ }
                  -- A key frame ends the smear. C toxcore's clients ignore
                  -- the request.
                  ask = [SendLossless f (BS.pack [videoControlId, askKeyFrame]) | isNothing (callSmearing c), isJust (callSmearing c')]
              in (c', outs ++ ask ++ [VideoIn f (RTP.codecOf flags) time bytes])
          | otherwise -> (c, outs)

counted :: (VideoStats -> VideoStats) -> Call -> Call
counted f c = c { callVideoStats = f (callVideoStats c) }

-- | A video frame came after this many that didn't, whether it's a key frame,
-- and when it was sent.
videoCame :: Int -> Bool -> MediaTime -> Call -> Call
videoCame lost key time c = counted tally c { callSmearing = smearing }
  where
    smearing
      | key = Nothing
      | lost > 0 = Just (fromMaybe time (callSmearing c))
      | otherwise = callSmearing c
    tally v = v
      { framesCame = framesCame v + 1
      , framesLost = framesLost v + lost
      , keyFramesCame = keyFramesCame v + fromEnum key
      , smears = smears v + fromEnum (isNothing (callSmearing c) && isJust smearing)
      , smearedFrames = smearedFrames v + fromEnum (isJust smearing)
      , smearedMs = smearedMs v + maybe 0 (\t -> if isNothing smearing then RTP.since time t else 0) (callSmearing c)
      }

videoStats :: Ord f => f -> Session f -> Maybe VideoStats
videoStats f s = callVideoStats <$> Map.lookup f (sessionCalls s)

pictureDecoded, decodeFailed :: Ord f => f -> Session f -> Session f
pictureDecoded f = withStats f $ \v -> v { picturesDecoded = picturesDecoded v + 1 }
decodeFailed f = withStats f $ \v -> v { decodeFailures = decodeFailures v + 1 }

withStats :: Ord f => f -> (VideoStats -> VideoStats) -> Session f -> Session f
withStats f g s = s { sessionCalls = Map.adjust (counted g) f (sessionCalls s) }

-- | Bandwidth reports that are due. A friend that stays quiet counts as
-- reporting no loss.
tick :: Ord f => Word64 -> Session f -> Step f
tick now s = (s { sessionCalls = calls }, concat reports)
  where
    (calls, reports) = Map.foldrWithKey report (Map.empty, []) (sessionCalls s)
    report f c0 (cs, rs) = case (callPhase c0, Bandwidth.due now (callMeter c)) of
      (Active, Just (r, meter')) ->
        (Map.insert f c { callMeter = meter' } cs, [SendLossy f (Bandwidth.encode r)] : rs)
      _ -> (Map.insert f c cs, rs)
      where
        c | callPhase c0 == Active, now - callHeard c0 >= quietGap = adapt now 0 c0 { callHeard = now }
          | otherwise = c0

call :: Ord f => Word64 -> f -> BitRates -> Session f -> Either Refusal (Step f)
call now f rates s
  | not (validRates rates) = Left InvalidBitRate
  | not (Set.member f (sessionConnected s)) = Left FriendNotConnected
  | Map.member f (sessionCalls s) = Left AlreadyInCall
  | otherwise = Right
      ( withCall f (newCall now Requesting self mempty rates) s
      , [msi f Init Nothing (Just self)] )
  where
    self = capabilities rates

answer :: Ord f => Word64 -> f -> BitRates -> Session f -> Either Refusal (Step f)
answer now f rates s
  | not (validRates rates) = Left InvalidBitRate
  | otherwise = case Map.lookup f (sessionCalls s) of
      Just c | callPhase c == Requested -> Right
        ( withCall f (newCall now Active self (callPeer c) rates) s
        , msi f Push Nothing (Just self) : decodable f s )
      _ -> Left NotCalling
  where
    self = capabilities rates

control :: Ord f => f -> Control -> Session f -> Either Refusal (Step f)
control f ctl s = case Map.lookup f (sessionCalls s) of
  Just _ | ctl == Cancel -> Right (withoutCall f s, [msi f Pop Nothing Nothing])
  Just c | callPhase c == Active -> case (ctl, callPaused c) of
    (Resume, Just caps) -> push c { callPaused = Nothing } caps
    (_, Just _) -> Left InvalidTransition
    (Pause, Nothing) -> push c { callPaused = Just (callSelf c) } mempty
    (MuteAudio, _) | has receiveAudio self -> push c (without receiveAudio self)
    (UnmuteAudio, _) | not (has receiveAudio self) -> push c (self <> receiveAudio)
    (HideVideo, _) | has receiveVideo self -> push c (without receiveVideo self)
    (ShowVideo, _) | not (has receiveVideo self) -> push c (self <> receiveVideo)
    _ -> Left InvalidTransition
    where
      self = callSelf c
  _ -> Left NotInCall
  where
    push c caps = Right (withCall f c { callSelf = caps } s, [msi f Push Nothing (Just caps)])

-- | Starting or stopping to send tells the friend, other changes don't.
setBitRate :: Ord f => f -> Media -> Word32 -> Session f -> Either Refusal (Step f)
setBitRate f m rate s = case Map.lookup f (sessionCalls s) of
  Just c | callPhase c == Active -> do
    let rates = callRates c
        set r = case m of
          Audio -> r { audioBitRate = rate }
          Video -> r { videoBitRate = rate }
        rates' = set rates
        old = if m == Audio then audioBitRate rates else videoBitRate rates
        flag = if m == Audio then MSI.sendAudio else MSI.sendVideo
        change caps = if rate == 0 then without flag caps else caps <> flag
        c' = c { callRates = rates', callCurrent = set (callCurrent c) }
    if not (validRates rates') then Left InvalidBitRate
    else if (old == 0) == (rate == 0) then Right (withCall f c' s, [])
    else case callPaused c of
      Just caps -> Right (withCall f c' { callPaused = Just (change caps) } s, [])
      Nothing ->
        let caps = change (callSelf c)
        in Right (withCall f c' { callSelf = caps } s, [msi f Push Nothing (Just caps)])
  _ -> Left NotInCall

canSend :: Ord f => Media -> f -> Session f -> Either Refusal ()
canSend m f s = void (sending m f s)

sending :: Ord f => Media -> f -> Session f -> Either Refusal Call
sending m f s = case Map.lookup f (sessionCalls s) of
  Just c | callPhase c == Active ->
    let rate = (if m == Audio then audioBitRate else videoBitRate) (callRates c)
        (out, into) = if m == Audio then (MSI.sendAudio, receiveAudio) else (MSI.sendVideo, receiveVideo)
    in if rate > 0 && has out (callSelf c) && has into (callPeer c) then Right c else Left Disabled
  _ -> Left NotInCall

send :: Ord f => MediaTime -> Media -> f -> Call -> RTP.Flags -> ByteString -> Session f -> Step f
send captured m f c flags bytes s =
  ( withCall f c { callSequences = Map.insert m (sequ + 1) (callSequences c) } s
  , map (SendLossy f) (RTP.packets m (Frame sequ captured (sessionSsrc s) flags bytes)) )
  where
    sequ = Map.findWithDefault 0 m (callSequences c)

-- | An Opus packet at a sampling rate.
sendAudio :: Ord f => MediaTime -> f -> Word32 -> ByteString -> Session f -> Either Refusal (Step f)
sendAudio captured f rate opus s = do
  c <- sending Audio f s
  let payload = LBS.toStrict (B.toLazyByteString (B.word32BE rate)) <> opus
  pure $ send captured Audio f c mempty payload s

-- | An encoded frame in the codec 'videoCodec' picked.
sendVideo :: Ord f => MediaTime -> f -> Codec -> Bool -> ByteString -> Session f -> Either Refusal (Step f)
sendVideo captured f codec key bytes s = do
  c <- sending Video f s
  let flags = RTP.codecFlags codec <> (if key then RTP.keyFrame else mempty)
      c' = c { callVideoSent = callVideoSent c + 1, callKeyFrame = callKeyFrame c && not key }
  pure $ send captured Video f c' flags bytes s

wantsKeyFrame :: Ord f => f -> Session f -> Bool
wantsKeyFrame f s = maybe False (\c -> callVideoSent c < startKeyFrames || callKeyFrame c)
  (Map.lookup f (sessionCalls s))

-- | H.264 for friends that said they decode it, unless it didn't work, VP8
-- for everyone else.
videoCodec :: Ord f => f -> Session f -> Codec
videoCodec f s
  | Set.member H264 (sessionEncoders s)
  , Just c <- Map.lookup f (sessionCalls s)
  , callPeerH264 c
  , not (Map.member H264 (callFailed c)) = H264
  | otherwise = VP8

-- | The encoder for the friend's call couldn't be made or didn't encode.
encoderFailed :: Ord f => f -> Codec -> String -> Session f -> Session f
encoderFailed f codec why s = s
  { sessionCalls = Map.adjust (\c -> c { callFailed = Map.insert codec why (callFailed c) }) f (sessionCalls s) }

failedEncoders :: Ord f => f -> Session f -> [(Codec, String)]
failedEncoders f = maybe [] (Map.toList . callFailed) . Map.lookup f . sessionCalls

-- | Nothing when not in a call.
bitRates :: Ord f => f -> Session f -> Maybe BitRates
bitRates f s = callCurrent <$> Map.lookup f (sessionCalls s)

-- | Hang up every call.
kill :: Session f -> Step f
kill s = (s { sessionCalls = Map.empty }, [msi f Pop Nothing Nothing | f <- Map.keys (sessionCalls s)])

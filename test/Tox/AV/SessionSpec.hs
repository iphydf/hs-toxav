{-# LANGUAGE StrictData #-}
module Tox.AV.SessionSpec (spec) where

import qualified Data.ByteString  as BS
import           Data.List        (foldl')
import qualified Data.Map.Strict  as Map
import           Test.Hspec

import qualified Tox.AV.Bandwidth as Bandwidth
import           Tox.AV.Bandwidth (Report (..))
import qualified Tox.AV.MSI       as MSI
import           Tox.AV.MSI       (Capabilities, receiveAudio, receiveVideo)
import qualified Tox.AV.RTP       as RTP
import           Tox.AV.RTP       (Frame (..), Media (..), MediaTime (..))
import           Tox.AV.Session

data Side = A | B
  deriving (Eq, Ord, Show)

other :: Side -> Side
other A = B
other B = A

-- | Two sessions that know each other as friends, and what each told its
-- client.
data World = World
  { worldSessions :: Map.Map Side (Session Side)
  , worldOutputs  :: Map.Map Side [Output Side]
  }

start :: [Codec] -> World
start codecs = foldl' (\w side -> deliver side (connect side w)) world [A, B]
  where
    world = World (Map.fromList [(A, s), (B, s)]) Map.empty
    s = new 1 codecs codecs
    connect side w =
      let (s', outs) = connection (other side) True (worldSessions w Map.! side)
      in (w { worldSessions = Map.insert side s' (worldSessions w) }, outs)

-- | Hands the packets a side sent to the other side, until nothing is left.
deliver :: Side -> (World, [Output Side]) -> World
deliver side (world, outputs) = foldl' one world outputs
  where
    one w output = case output of
      SendLossless _ packet -> arrive packet w
      SendLossy _ packet    -> arrive packet w
      _ -> w { worldOutputs = Map.insertWith (flip (++)) side [output] (worldOutputs w) }
    arrive packet w =
      let to = other side
          (s, outs) = receive 0 side packet (worldSessions w Map.! to)
      in deliver to (w { worldSessions = Map.insert to s (worldSessions w) }, outs)

act :: Side -> (Session Side -> Either Refusal (Session Side, [Output Side])) -> World -> World
act side f w = case f (worldSessions w Map.! side) of
  Left r -> error (show r)
  Right (s, outs) -> deliver side (w { worldSessions = Map.insert side s (worldSessions w) }, outs)

refusal :: Side -> (Session Side -> Either Refusal a) -> World -> Maybe Refusal
refusal side f w = either Just (const Nothing) (f (worldSessions w Map.! side))

told :: Side -> World -> [Output Side]
told side = Map.findWithDefault [] side . worldOutputs

-- | What a side sends in answer to a packet.
answers :: Side -> BS.ByteString -> World -> [Output Side]
answers side packet w = snd (receive 0 (other side) packet (worldSessions w Map.! side))

both :: BitRates
both = BitRates 48 5000

everything :: Capabilities
everything = mconcat [MSI.sendAudio, MSI.sendVideo, receiveAudio, receiveVideo]

talking :: World
talking = act B (answer 0 A both) . act A (call 0 B both) $ start [VP8]

msiPacket :: MSI.Request -> Maybe MSI.Error -> BS.ByteString
msiPacket request err = BS.cons 69 (MSI.encode (MSI.Message request err Nothing))

spec :: Spec
spec = do
  it "rings the friend and tells the caller they answered" $ do
    told B (act A (call 0 B both) (start [VP8])) `shouldBe` [Event (Invited A everything)]
    told A talking `shouldBe` [Event (PeerChanged B everything)]

  it "carries audio and video across, with when they were captured" $ do
    let opus = BS.pack [1, 2, 3]
        picture = BS.replicate 5000 7
        w = act A (sendVideo (MediaTime 30) B VP8 True picture) (act A (sendAudio (MediaTime 20) B 48000 opus) talking)
    drop 1 (told B w) `shouldBe`
      [ AudioIn A 0 (Just (MediaTime 20, BS.pack [0, 0, 0xBB, 0x80] <> opus))
      , VideoIn A VP8 (MediaTime 30) picture
      ]

  it "tells the friend when we hang up" $ do
    let w = act A (control B Cancel) talking
    drop 1 (told B w) `shouldBe` [Event (Ended A HungUp)]
    refusal A (control B Cancel) w `shouldBe` Just NotInCall

  it "never answers a hang-up without a call, but reports a stray answer" $ do
    answers B (msiPacket MSI.Pop Nothing) (start [VP8]) `shouldBe` []
    answers B (BS.cons 69 (MSI.encode (MSI.Message MSI.Push Nothing (Just everything)))) (start [VP8])
      `shouldBe` [SendLossless A (msiPacket MSI.Pop (Just MSI.StrayMessage))]

  it "tells a friend their message was broken, keeping the call" $ do
    let (s, outs) = receive 0 A (BS.pack [69, 1, 1]) (worldSessions talking Map.! B)
    outs `shouldBe` [SendLossless A (msiPacket MSI.Pop (Just MSI.InvalidMessage))]
    either Just (const Nothing) (sendAudio (MediaTime 0) A 48000 BS.empty s) `shouldBe` Nothing

  it "ends a call when the friend goes offline, also while it rings" $ do
    let ringing = act A (call 0 B both) (start [VP8])
    snd (connection A False (worldSessions talking Map.! B)) `shouldBe` [Event (Ended A Disconnected)]
    snd (connection B False (worldSessions ringing Map.! A)) `shouldBe` [Event (Ended B Disconnected)]
    snd (connection A False (worldSessions ringing Map.! B)) `shouldBe` [Event (Ended A Disconnected)]

  it "pauses and resumes" $ do
    let paused = act A (control B Pause) talking
    drop 1 (told B paused) `shouldBe` [Event (PeerChanged A mempty)]
    refusal B (sendAudio (MediaTime 0) A 48000 BS.empty) paused `shouldBe` Just Disabled
    refusal A (control B MuteAudio) paused `shouldBe` Just InvalidTransition
    drop 2 (told B (act A (control B Resume) paused)) `shouldBe` [Event (PeerChanged A everything)]

  it "tells the friend when we stop or start sending" $ do
    let quiet = act A (setBitRate B Audio 0) talking
    drop 1 (told B quiet) `shouldBe`
      [Event (PeerChanged A (mconcat [MSI.sendVideo, receiveAudio, receiveVideo]))]
    refusal A (sendAudio (MediaTime 0) B 48000 BS.empty) quiet `shouldBe` Just Disabled
    told B (act A (setBitRate B Video 100) talking) `shouldBe` told B talking

  it "sends less while much is lost, then more again, up to what was set" $ do
    let report t r s = fst (receive t B (Bandwidth.encode r) s)
        rates = bitRates B
        lossy = report 0 (Report 50 50) (worldSessions talking Map.! A)
    rates lossy `shouldBe` Just (BitRates 48 2500)
    -- Too soon after the last change for a new video encoder.
    rates (report 1000 (Report 50 50) lossy) `shouldBe` Just (BitRates 48 2500)
    let clean = foldl' (\s t -> report t (Report 0 100) s) lossy [6000, 7000, 8000]
    rates clean `shouldBe` Just (BitRates 48 2750)
    -- And as toxcore does, by not reporting at all.
    let quiet = foldl' (\s t -> fst (tick t s)) lossy [6000, 8000, 10000]
    rates quiet `shouldBe` Just (BitRates 48 2750)
    let audioOnly = act A (setBitRate B Video 0) talking
    rates (report 0 (Report 50 50) (worldSessions audioOnly Map.! A)) `shouldBe` Just (BitRates 24 0)

  it "reports what arrived" $ do
    let w = act A (sendAudio (MediaTime 0) B 48000 (BS.pack [1])) talking
        (_, outs) = tick Bandwidth.interval (worldSessions w Map.! B)
    outs `shouldBe` [SendLossy A (Bandwidth.encode (Report 0 5))]

  it "reports audio frames that never came as lost, each as long as one that did" $ do
    let frame n = RTP.packets Audio (Frame n (MediaTime (20 * fromIntegral n)) 1 mempty (BS.replicate 5 0))
        b = foldl' (\s p -> fst (receive 0 A p s)) (worldSessions talking Map.! B) (concatMap frame [0, 3, 4, 5])
        (_, outs) = tick Bandwidth.interval b
    outs `shouldBe` [SendLossy A (Bandwidth.encode (Report 10 20))]

  it "sends key frames first and when asked" $ do
    let sent = act A (sendVideo (MediaTime 0) B VP8 False BS.empty)
        w7 = iterate sent talking !! 7
    wantsKeyFrame B (worldSessions talking Map.! A) `shouldBe` True
    wantsKeyFrame B (worldSessions w7 Map.! A) `shouldBe` False
    let (asked, _) = receive 0 B (BS.pack [172, 0]) (worldSessions w7 Map.! A)
    wantsKeyFrame B asked `shouldBe` True

  it "sends H.264 to a friend that decodes it, as both say when the call starts" $ do
    let w = act B (answer 0 A both) (act A (call 0 B both) (start [VP8, H264]))
    (videoCodec B (worldSessions w Map.! A), videoCodec A (worldSessions w Map.! B)) `shouldBe` (H264, H264)
    let failed = encoderFailed B H264 "broken" (worldSessions w Map.! A)
    (videoCodec B failed, failedEncoders B failed) `shouldBe` (VP8, [(H264, "broken")])
    videoCodec B (worldSessions talking Map.! A) `shouldBe` VP8

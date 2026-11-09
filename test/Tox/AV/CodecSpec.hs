module Tox.AV.CodecSpec (spec) where

import qualified Data.ByteString      as BS
import           Data.IORef           (modifyIORef, newIORef, readIORef)
import           Data.List            (nub)
import qualified Data.Vector.Storable as V
import           Test.Hspec

import           Tox.AV.Codec
import           Tox.AV.Codec.Types   (keyFrame)
import           Tox.AV.RTP           (Codec (..), MediaTime (..))

must :: IO (Maybe a) -> IO a
must action = action >>= maybe (fail "failed") pure

-- | A decoder, and what it has decoded so far.
collecting :: (((MediaTime, Picture) -> IO ()) -> IO (Maybe VideoDecoder)) -> IO (VideoDecoder, IO [(MediaTime, Picture)])
collecting new = do
  decoded <- newIORef []
  dec <- must (new (\p -> modifyIORef decoded (p :)))
  pure (dec, reverse <$> readIORef decoded)

tone :: Int -> Int -> Pcm
tone rate samples = V.generate (2 * samples) $ \i ->
  round (8000 * sin (2 * pi * 440 * fromIntegral (i `div` 2) / fromIntegral rate :: Double))

-- | A gradient, so that the encoder has something to encode.
picture :: Int -> Int -> Picture
picture w h = Picture w h
  (BS.pack [fromIntegral (x + y) | y <- [1 .. h], x <- [1 .. w]])
  (BS.replicate (chromaSize w * chromaSize h) 100)
  (BS.replicate (chromaSize w * chromaSize h) 150)

spec :: Spec
spec = do
  it "has the codecs calls need" $ do
    encoders >>= (`shouldSatisfy` \cs -> VP8 `elem` cs && H264 `elem` cs)
    decoders `shouldReturn` [VP8, H264, H265]

  it "encodes and decodes Opus" $ do
    enc <- must (audioEncoder 48000 2 64000)
    packet <- must (encodeAudio enc (tone 48000 960))
    packetChannels packet `shouldReturn` Just 2
    dec <- must (audioDecoder 48000 2)
    V.length <$> must (decodeAudio dec packet) `shouldReturn` 1920
    V.length <$> must (concealAudio dec 960) `shouldReturn` 1920

  it "sends silence and a tone in packets of one size, which tell nothing of what is said" $ do
    enc <- must (audioEncoder 48000 2 64000)
    packets <- mapM (must . encodeAudio enc) (replicate 5 (V.replicate 1920 0) ++ replicate 5 (tone 48000 960))
    length (nub (map BS.length packets)) `shouldBe` 1

  it "refuses frames Opus can't take" $ do
    enc <- must (audioEncoder 48000 2 64000)
    encodeAudio enc (tone 48000 100) `shouldReturn` Nothing

  mapM_ videoRoundTrip [VP8, H264]

  it "refuses pictures smaller than they say" $ do
    enc <- either fail pure =<< videoEncoder VP8 64 48 500000
    encodeVideo enc 0 True (picture 64 47) { pictureHeight = 48 } `shouldReturn` Nothing

  it "decodes no pictures larger than allowed" $ do
    enc <- either fail pure =<< videoEncoder VP8 66 50 500000
    frames <- must (encodeVideo enc 0 True (picture 66 50))
    (dec, decoded) <- collecting (videoDecoder VP8 (33 * 50))
    mapM_ (decodeVideo dec (MediaTime 0) . frameBytes) frames
    decoded `shouldReturn` []

frameBytes :: (a, b, BS.ByteString) -> BS.ByteString
frameBytes (_, _, bytes) = bytes

videoRoundTrip :: Codec -> Spec
videoRoundTrip codec = it ("encodes and decodes " ++ show codec) $ do
  enc <- either fail pure =<< videoEncoder codec 66 50 500000
  frames <- must (encodeVideo enc 0 True (picture 66 50))
  later <- must (encodeVideo enc 33 False (picture 66 50))
  [(time, key) | (time, key, _) <- frames ++ later] `shouldBe` [(0, True), (33, False)]
  map (keyFrame codec . frameBytes) (frames ++ later) `shouldBe` [True, False]
  (dec, decoded) <- collecting (videoDecoder codec (3840 * 2160))
  mapM_ (decodeVideo dec (MediaTime 7) . frameBytes) frames
  pictures <- decoded
  map (\(t, p) -> (t, pictureWidth p, pictureHeight p, BS.length (pictureU p))) pictures `shouldBe` [(MediaTime 7, 66, 50, 33 * 25)]

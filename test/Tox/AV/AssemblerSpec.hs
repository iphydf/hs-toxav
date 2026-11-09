module Tox.AV.AssemblerSpec (spec) where

import qualified Data.ByteString  as BS
import           Data.List        (foldl')
import           Data.Maybe       (mapMaybe)
import           Test.Hspec
import           Test.QuickCheck  (choose, forAll, shuffle)

import           Tox.AV.Assembler
import           Tox.AV.RTP

pieces :: Int -> Int -> [Piece]
pieces sequ n = mapMaybe parse . packets Video $
  Frame (fromIntegral sequ) (MediaTime (fromIntegral sequ)) 0 mempty (BS.pack (take n (cycle [0 .. 255])))

feed :: [Piece] -> (Assembler, [Outcome])
feed = foldl' (\(a, outs) p -> (++) outs <$> add p a) (empty, [])

complete :: [Outcome] -> [BS.ByteString]
complete outcomes = [bytes | Complete _ _ _ bytes <- outcomes]

spec :: Spec
spec = do
  it "puts pieces back together in any order" $
    forAll (choose (1, 10000)) $ \n -> forAll (shuffle (pieces 0 n)) $ \shuffled ->
      complete (snd (feed shuffled)) `shouldBe` [BS.concat (map pieceData (pieces 0 n))]

  it "doesn't count a repeated piece twice" $ case pieces 0 3000 of
    [p1, _, p3] -> snd (feed [p1, p1, p3, p3]) `shouldBe` []
    other       -> expectationFailure (show other)

  it "gives up on the oldest frame for a new one" $ do
    let started = concatMap (take 1 . (`pieces` 3000)) [0 .. maxPending]
    snd (feed started) `shouldBe` [Incomplete (3000 - maxPieceSize)]

  it "ignores frames too big to keep" $
    snd (feed (take 1 (pieces 0 (maxFrameSize + 1)))) `shouldBe` []

  it "takes short pieces only at a frame's end" $ case pieces 0 3000 of
    p1 : _ -> do
      -- Had it been kept, the new frames would push it out as incomplete.
      let others = concatMap (take 1 . (`pieces` 3000)) [1 .. maxPending]
      snd (feed (p1 { pieceData = BS.take 1 (pieceData p1) } : others)) `shouldBe` []
    [] -> expectationFailure "no pieces"

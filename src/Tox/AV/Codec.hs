-- | Opus through libopus, video through the platform's codecs.
module Tox.AV.Codec
  ( Pcm
  , AudioFrame (..)
  , AudioEncoder
  , audioEncoder
  , setAudioBitRate
  , encodeAudio
  , AudioDecoder
  , audioDecoder
  , decodeAudio
  , concealAudio
  , recoverAudio
  , packetChannels
  , Picture (..)
  , chromaSize
  , complete
  , fromI420
  , keyFrame
  , keyFrameInterval
  , VideoEncoder
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
  , Close (..)
  ) where

import           Tox.AV.Codec.Audio
import           Tox.AV.Codec.Types (AudioFrame (..), Close (..), Picture (..),
                                     chromaSize, complete, fromI420, keyFrame,
                                     keyFrameInterval)
import           Tox.AV.Codec.Video

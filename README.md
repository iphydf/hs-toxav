# Tox audio and video calls in Haskell

Call signalling, RTP and the audio and video codecs of Tox calls, compatible
with c-toxcore's toxav. It depends on no Tox implementation: the client gives
`Tox.AV.new` a way to send a friend lossless and lossy packets, and passes it
the packets that arrive with `losslessIds` and `lossyIds`, and when friends
come online and go offline.

Audio is Opus through libopus. Video goes through libavcodec, or the
browser's codecs (WebCodecs) when built for wasm32: VP8 for every client, and
H.264 for clients that say they decode it, as we tell every friend we call.
H.265 that comes is decoded too. On Android 9 and later, H.264 is encoded by
the device's own encoder through MediaCodec.

`Tox.AV.Session` is the call logic without the codecs, as pure functions.
`Tox.AV.Playout` decides when to play the audio and show the pictures that
come, keeping them in step, also pure.

## In a browser

`browser/` holds a page that runs a call between two ToxAVs in the browser,
for trying the codecs without a Tox network:

```sh
nix-shell browser/shell.nix --run 'browser/build.sh out'
python3 -m http.server -d out
```

Then open <http://localhost:8000>. Browsers give a page the camera and
microphone only on localhost or over HTTPS.

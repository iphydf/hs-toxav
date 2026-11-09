// Runs toxav-browser.wasm: a call between two ToxAVs in this page.

import { ConsoleStdout, File, OpenFile, WASI } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.4.2/dist/index.js";
import jsffi from "./ghc_wasm_jsffi.js";

const longest = 640, rate = 48000, frameSamples = 960;

const logView = document.getElementById("log");
const log = line => { logView.textContent += line + "\n"; logView.scrollTop = logView.scrollHeight; };

// Everything that goes wrong shows in the log, as phones have no console to
// look at.
for (const level of ["info", "warn", "error"]) {
  const original = console[level];
  console[level] = (...args) => { log(`${level}: ${args.join(" ")}`); original.apply(console, args); };
}
addEventListener("error", e => log(`error: ${e.message}`));
addEventListener("unhandledrejection", e => log(`error: ${e.reason}`));
const copy = (what, view) => () =>
  navigator.clipboard.writeText(view.textContent).then(() => log(`${what} copied`), e => log(`copy failed: ${e}`));
document.getElementById("copy").addEventListener("click", copy("log", logView));
document.getElementById("copy-delays").addEventListener("click", copy("delays", document.getElementById("delays")));
log(navigator.userAgent);

const wasi = new WASI([], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered(log),
  ConsoleStdout.lineBuffered(log),
]);
const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("toxav-browser.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: jsffi(exports),
});
Object.assign(exports, instance.exports);
wasi.initialize(instance);
exports.hs_init(0, 0);

const stats = { sent: 0, pictures: 0, shown: 0, audio: 0, loudest: 0 };
// When sending the current picture started, so that we log a stuck encoder.
let sendingSince = null, stuck = false;
addEventListener("visibilitychange", () => log(`page ${document.hidden ? "hidden" : "shown"}`));
const statsView = document.getElementById("stats");
const playoutView = document.getElementById("playout");

// Delays in ms from capture to coming in, and to showing or being heard, the
// browser's audio output included. The camera's and microphone's own delays
// aren't counted.
const delays = {};
const delay = (name, ms) => { const d = delays[name] ??= { sum: 0, n: 0 }; d.sum += ms; d.n++; };
const delaysView = document.getElementById("delays");

// Each second's means, for the last 30 seconds.
const seconds = [];
const average = xs => xs.length === 0 ? NaN : xs.reduce((a, b) => a + b) / xs.length;
const ms = x => Number.isFinite(x) ? `${Math.round(x)}` : "?";

// The video and audio delays, each as coming in plus waiting, and the gap
// between them.
function describe(part) {
  const media = (came, played) => ({ total: part(played), text: `${ms(part(came))} + ${ms(part(played) - part(came))} = ${ms(part(played))} ms` });
  const video = media("videoCame", "videoShown");
  const sound = media("audioCame", "audioHeard");
  return { gap: video.total - sound.total, text: `video ${video.text} to show, audio ${sound.text} to hear` };
}

const ahead = gap => `video ${ms(Math.abs(gap))} ms ${gap > 0 ? "behind" : "ahead of"} audio`;
const range = gaps => {
  const low = Math.min(...gaps), high = Math.max(...gaps);
  return low >= 0 ? `between ${ms(low)} and ${ms(high)} ms behind`
    : high <= 0 ? `between ${ms(-high)} and ${ms(-low)} ms ahead`
    : `between ${ms(-low)} ms ahead and ${ms(high)} ms behind`;
};

let functions = null;
let counted = { spedUp: 0, slowedDown: 0, madeUp: 0, skipped: 0 };
let outputLatency = 0;

setInterval(async () => {
  const level = stats.loudest === 0 ? "silent" : `loudest ${Math.round(20 * Math.log10(stats.loudest / 32768))} dB`;
  statsView.textContent = `pictures/s: ${stats.sent} sent, ${stats.pictures} received, ${stats.shown} shown; ${stats.audio} audio frames/s (${level}) received`;
  stats.sent = stats.pictures = stats.shown = stats.audio = stats.loudest = 0;
  const sendingFor = sendingSince === null ? 0 : performance.now() - sendingSince;
  if (sendingFor > 2000 && !stuck) log(`video: sending a picture has taken ${Math.round(sendingFor / 1000)} s`);
  stuck = sendingFor > 2000;

  const second = Object.fromEntries(Object.entries(delays).map(([name, d]) => [name, d.sum / d.n]));
  for (const name in delays) delete delays[name];
  const now = describe(name => second[name] ?? NaN);
  second.gap = now.gap;
  seconds.push(second);
  if (seconds.length > 30) seconds.shift();

  const over = name => average(seconds.map(s => s[name]).filter(Number.isFinite));
  const gaps = seconds.map(s => s.gap).filter(Number.isFinite);
  const last = describe(over);
  delaysView.textContent = `from capture: ${now.text}${Number.isFinite(now.gap) ? `, ${ahead(now.gap)}` : ""}\n`
    + `last ${seconds.length} s: ${last.text}`
    + (gaps.length > 0 ? `, ${ahead(average(gaps))} (each second ${range(gaps)})` : "");

  if (audio !== null && Math.abs((audio.outputLatency ?? 0) - outputLatency) > 0.005) {
    outputLatency = audio.outputLatency ?? 0;
    log(`audio output delay ${Math.round(1000 * outputLatency)} ms`);
  }

  if (functions !== null) {
    const total = await functions.stats();
    const per = name => total[name] - counted[name];
    playoutView.textContent = `audio this second: ${per("spedUp")} sped up, ${per("slowedDown")} slowed down, ${per("madeUp")} made up, ${per("skipped")} jumps ahead`;
    counted = total;
  }
}, 1000);

// i420's colour space.
const bt601 = { matrix: "smpte170m", primaries: "smpte170m", transfer: "smpte170m", fullRange: false };

const received = document.getElementById("received");
const receivedContext = received.getContext("2d");
let audio = null, output = null;

const page = {
  log,
  pictureCame(ago) {
    stats.pictures++;
    delay("videoCame", ago);
  },
  audioCame(ago, loudest) {
    stats.audio++;
    delay("audioCame", ago);
    stats.loudest = Math.max(stats.loudest, loudest);
  },
};

function show({ width: w, height: h, bytes }) {
  const frame = new VideoFrame(bytes, { format: "I420", codedWidth: w, codedHeight: h, timestamp: 0, colorSpace: bt601 });
  if (received.width !== w || received.height !== h) { received.width = w; received.height = h; log(`picture ${w}x${h}`); }
  receivedContext.drawImage(frame, 0, 0);
  frame.close();
}

function play(block, at) {
  const frames = block.samples.length / block.channels;
  const buffer = audio.createBuffer(block.channels, frames, block.rate);
  for (let c = 0; c < block.channels; c++) {
    const out = buffer.getChannelData(c);
    for (let i = 0; i < frames; i++) out[i] = block.samples[i * block.channels + c] / 32768;
  }
  const source = audio.createBufferSource();
  source.buffer = buffer;
  source.connect(output);
  source.start(at);
}

// RGBA to I420 as BT.601 limited range, averaging each 2x2 block for the
// chroma.
function i420(rgba, w, h) {
  const cw = (w + 1) >> 1, ch = (h + 1) >> 1;
  const out = new Uint8Array(w * h + 2 * cw * ch);
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const p = 4 * (y * w + x);
      out[y * w + x] = (66 * rgba[p] + 129 * rgba[p + 1] + 25 * rgba[p + 2] + 128 >> 8) + 16;
    }
  }
  for (let y = 0; y < ch; y++) {
    for (let x = 0; x < cw; x++) {
      let r = 0, g = 0, b = 0, n = 0;
      for (let dy = 0; dy < 2 && 2 * y + dy < h; dy++) {
        for (let dx = 0; dx < 2 && 2 * x + dx < w; dx++) {
          const p = 4 * ((2 * y + dy) * w + 2 * x + dx);
          r += rgba[p]; g += rgba[p + 1]; b += rgba[p + 2]; n++;
        }
      }
      r /= n; g /= n; b /= n;
      out[w * h + y * cw + x] = (-38 * r - 74 * g + 112 * b + 128 >> 8) + 128;
      out[w * h + cw * ch + y * cw + x] = (112 * r - 94 * g - 18 * b + 128 >> 8) + 128;
    }
  }
  return out;
}

// Collects the microphone's samples into 20 ms frames, each with the audio
// clock's time at its first sample.
const collector = `
registerProcessor("collector", class extends AudioWorkletProcessor {
  constructor() { super(); this.frame = new Int16Array(${frameSamples}); this.n = 0; }
  process([input]) {
    const samples = input[0] || [];
    samples.forEach((s, i) => {
      if (this.n === 0) this.start = currentTime + i / sampleRate;
      this.frame[this.n++] = Math.max(-32768, Math.min(32767, Math.round(s * 32768)));
      if (this.n === this.frame.length) {
        this.port.postMessage({ samples: this.frame, start: this.start });
        this.frame = new Int16Array(${frameSamples});
        this.n = 0;
      }
    });
    return true;
  }
});`;

async function start() {
  const echo = document.getElementById("echo"), through = document.getElementById("output");
  document.getElementById("start").disabled = echo.disabled = through.disabled = true;
  audio = new AudioContext({ sampleRate: rate });
  output = audio.destination;
  // Android may play Web Audio through the earpiece while the microphone
  // cancels echo, so this tries playing it through an audio element instead.
  if (through.value === "element") {
    output = audio.createMediaStreamDestination();
    const element = new Audio();
    element.srcObject = output.stream;
    element.play().catch(e => log(`audio element: ${e}`));
  }
  audio.addEventListener("statechange", () => log(`audio output ${audio.state}`));
  log(`audio output ${audio.state} at ${audio.sampleRate} Hz`);
  const stream = await navigator.mediaDevices.getUserMedia({
    video: { width: longest },
    audio: { channelCount: 1, echoCancellation: echo.checked },
  });
  log(`microphone: echo cancellation ${stream.getAudioTracks()[0].getSettings().echoCancellation}, playing through ${through.value}`);
  // Headphones and headsets change the devices under a call.
  for (const track of stream.getTracks()) {
    log(`${track.kind}: ${track.label}`);
    for (const event of ["mute", "unmute", "ended"]) track.addEventListener(event, () => log(`${track.kind}: ${event}`));
  }
  navigator.mediaDevices.addEventListener("devicechange", async () => {
    const devices = await navigator.mediaDevices.enumerateDevices();
    log(`devices: ${devices.filter(d => d.kind !== "videoinput").map(d => `${d.kind} ${d.label || "?"}`).join(", ")}`);
  });
  const camera = document.getElementById("camera");
  camera.srcObject = stream;
  await camera.play();

  functions = await exports.start(page);

  // Audio plays from blocks pulled a little ahead, so that timers firing late
  // leave no gaps. Each is pulled with when it'll be heard.
  const pullAhead = 0.04;
  (async () => {
    let playAt = audio.currentTime;
    for (;;) {
      playAt = Math.max(playAt, audio.currentTime);
      while (playAt < audio.currentTime + pullAhead) {
        const block = await functions.pull(1000 * (playAt - audio.currentTime + (audio.outputLatency ?? 0)));
        if (block === null) {
          playAt += 0.01;
          continue;
        }
        if (document.getElementById("hear").checked) play(block, playAt);
        delay("audioHeard", block.delay);
        playAt += block.samples.length / block.channels / block.rate;
      }
      await new Promise(resolve => setTimeout(resolve, 10));
    }
  })().catch(e => log(`audio: ${e}`));

  // A picture drawn at a refresh is seen from the next refresh to the one
  // after, so each shows the latest due by then, or with lip sync off, the
  // latest that came.
  const sync = document.getElementById("sync");
  // The screen's refresh interval: the shortest of the last 30, as a late
  // callback only makes one longer.
  let lastRefresh = performance.now();
  const intervals = [];
  const refresh = async now => {
    requestAnimationFrame(refresh);
    intervals.push(now - lastRefresh);
    if (intervals.length > 30) intervals.shift();
    lastRefresh = now;
    const interval = Math.min(...intervals);
    const shown = await functions.picture(1.5 * interval, 2 * interval, sync.checked ? 1 : 0);
    if (shown !== null) {
      show(shown);
      stats.shown++;
      delay("videoShown", shown.delay);
    }
  };
  requestAnimationFrame(refresh);

  // The camera, a picture at a time: one that comes while the last is still
  // being sent is left out. Pictures keep the camera's shape (a phone held
  // upright gives portrait), with even sides as H.264 encoders want.
  const canvas = new OffscreenCanvas(1, 1);
  const context = canvas.getContext("2d", { willReadFrequently: true });
  let sending = false;
  const nextPicture = async (now, { captureTime }) => {
    if (!sending) {
      sending = true;
      const scale = Math.min(1, longest / Math.max(camera.videoWidth, camera.videoHeight));
      const w = Math.round(camera.videoWidth * scale) & ~1, h = Math.round(camera.videoHeight * scale) & ~1;
      if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }
      context.drawImage(camera, 0, 0, w, h);
      const picture = i420(context.getImageData(0, 0, w, h).data, w, h);
      sendingSince = performance.now();
      await functions.sendVideo(w, h, picture, performance.now() - (captureTime ?? now));
      sendingSince = null;
      stats.sent++;
      sending = false;
    }
    camera.requestVideoFrameCallback(nextPicture);
  };
  camera.requestVideoFrameCallback(nextPicture);

  await audio.audioWorklet.addModule(URL.createObjectURL(new Blob([collector], { type: "text/javascript" })));
  const node = new AudioWorkletNode(audio, "collector", { numberOfOutputs: 0 });
  node.port.onmessage = ({ data: { samples, start } }) =>
    functions.sendAudio(rate, 1, samples, 1000 * (audio.currentTime - start));
  audio.createMediaStreamSource(stream).connect(node);
}

document.getElementById("start").addEventListener("click", () => start().catch(e => log(`failed: ${e}`)));
log("ready");

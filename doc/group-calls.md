# Group calls

This is a design for audio and video calls with more than one other person in
hs-toxav, the Haskell implementation of Tox calls. It's a draft for review.
Once we agree on it, the protocol section becomes the spec.

## Scope

A call can be any group of people: a whole NGC group, some people from one,
friends, or a mix. Calls are built on NGC groups. We leave out conference audio
(c-toxcore's `groupav.c`): few people use it, and it can't carry video.

No other client has group calls yet, so the protocol is ours, versioned and
written down for other clients to implement.

Calls with one friend don't change, and keep working with c-toxcore's clients.
hs-toxav doesn't depend on toxcore: the client passes packets in and out, and
creates and joins the groups.

Version 1 allows at most 16 people in a call. [Growing later](#growing-later)
explains why and how calls could get larger.

## Rooms

Every call happens in a room, which is an NGC group. There are two kinds:

- a group's own room, for a call with everyone in that group;
- a call room, which is a private group created for one call, with only the
  people invited to it.

NGC already gives every two peers a direct connection, through NATs or TCP
relays, with each packet encrypted for its peer. Each peer has a key in the
group, and there are roles: founder, moderator, user and observer.

### Call rooms are private, joined directly

Call rooms are private groups, so nothing about them is announced in the DHT.
To join a private group you need the group's ID, the password, and one member's
key and address. c-toxcore's API gets these from a friend's invite, but the
protocol doesn't actually need the friend. Our invite carries them for a few
members, so anyone it reaches can join through any of them without searching
the DHT, even once the inviter has left.

c-toxcore's API can't join this way yet. hs-toxcore gets a join that takes
these details directly.

### Call rooms in the client

A call room isn't one of your groups. It doesn't show in your group list and
isn't saved. You leave it when you leave the call, or when you've been alone in
it for a while, and it's gone once everyone has left.

## Who's who

In a call room, each person has a new key and a name they chose, so names
prove nothing. Anyone with the password could join as "Alice".

So when you join a call room, you send each `Capable` friend an `IAm`: a hash
of the room's ID and your key in it. Friends who come online during the call
get one too, and receivers keep them until the call ends. A friend in the room
hashes the room's ID with each member's key and recognises you by the match. A
friend who isn't in the room doesn't know its ID, so all they learn is that
you joined some call.

The rule is simple: in a call room, you know who someone is if and only if
they're your friend. Everyone else shows as unknown, whoever invited them. The
names an `Invite` lists are what the inviter claims, until you're in the room.

## Rules are the room's

A call has no rules of its own. It follows the rules of its room, which NGC
sets and signs. In a call room, whoever started the call is the founder.

| In the room       | In its call |
| ----------------- | ----------- |
| Voice state, role | Who may send audio and video: those who may speak in the room. Observers listen and watch. |
| Kick              | Disconnects them from the call too. NGC has no ban, so in a call room the founder's client also changes the password to keep them out, and sends invites that are still pending again with the new one. |
| Peer limit        | In a call room, how many people may join it. A call never has more than 16 people, whatever the room's limit, and anyone else is told it's full. |
| Password          | In a call room, who can get in. Whoever has it can pass it on by inviting. |
| Founder leaving   | This is common, e.g. when a phone dies. So the founder's client makes the first few people it knows moderators, and they can still kick. Only the password can't change any more. If someone who was kicked keeps coming back, the call moves to a new call room. |
| Privacy           | People in a call connect to each other directly, so they learn each other's IP addresses. |

NGC delivers custom packets whatever the sender's role, so receivers enforce
this: they drop audio and video from a peer that may not speak.

## Inviting

An `Invite` can go over any way of reaching someone: a lossless friend packet
to a friend, or a private custom packet to someone in a shared group, whether
they're friends or not.

It carries the call's ID and version, whether it has video, the names of the
people in it so far, and the room, as bytes only the client reads. For a call
room that's the group's ID and password, and the keys and addresses in it of
the inviter and up to three others in the call. The person invited tries each
until one answers, and learns about everyone else from that one, so the invite
works as long as any of them is still in the call. To a friend it also carries
NGC's own group invite, so clients on c-toxcore can join with their current
API while the inviter is in the call.

Clients that support group calls announce it with a `Capable` message, and we
only invite those. Anyone else would never answer.

Ringing stops when the inviter sends `Cancel`, or after a minute. The person
invited can `Decline`, and someone already in a call declines as busy. An
invite counts once however often it arrives, we accept only a few a minute
from any one peer, and if it leads into a room without that call, the client
leaves within seconds.

How this covers the cases we care about:

- A few people from a group get a call room with just them, invited through
  the group. The rest of the group never learns about the call.
- Someone from outside is invited by anyone in the call who is their friend.
- A call with one friend that grows becomes a call room for the two of you,
  then the third person joins and the call moves there. If you both add
  someone at the same time, the room with the lower call ID wins, and the
  other side invites its guest there.
- A call with the whole group uses the group's own room, without invites.

## The experience

Invited people are rung, as for a call with a friend, and see who's in the
call before answering. A missed invite stays, so you can still join while the
call lasts. Only friends ring loudly: other invites and those through a muted
group, show quietly.

A call with a whole group doesn't ring anyone. It shows as "Alice started a
call · Join" in the group, with a notification unless the group is muted. A
call ends when the last person leaves.

You see who's in the call, who's muted, who sends video and who's speaking.
Someone whose connection drops, or who can't hear or be heard, shows as
reconnecting within seconds, not after the minute NGC takes. Video shows as
soon as you join.

Audio always comes first. On a weak link video gets smaller, then fewer
people's video comes through (you see their picture instead), but audio
doesn't break up. Everyone picks whose video they see, in the main view or as
a tile.

## Protocol

Every message starts with the three bytes `0xb0 0x01` and the message type. In
a room, messages are NGC custom packets. Other extensions use those too, so
like Green-Sky's scheme, the first byte says whose extension it is:

- Green-Sky's old one-byte packet types are 0x81 to 0xa6, and old parsers try
  those first, so no range can start with one of them.
- 0x90 is Green-Sky's range, used for its extensions such as file sharing
  (FT1), which hs-toxsync also implements.
- 0xb0 is TokTok's range, and 0x01 in it is calls.

Packets don't carry a version. A call's version is in its `CallState`, and
messages sent outside a call carry their own.

`Capable` lists every version a client speaks. A call's version is fixed when
the call starts, as the highest version everyone in it speaks. Someone who
doesn't speak it can't join that call.

After the three bytes, messages are msgpack arrays, except for audio and video,
which have a fixed binary header (see [Media](#media)). A receiver ignores
message types it doesn't know and fields after the ones it knows. New fields
only ever go at the end. This lets a message grow without a new version, e.g.
`Report` can later carry arrival times for congestion control that reacts to
delay before loss.

| Message     | Sent                                  | Contents |
| ----------- | ------------------------------------- | -------- |
| `Capable`   | lossless, to friends and in groups    | The versions of group calls I speak. |
| `Invite`    | lossless, outside the room            | See [Inviting](#inviting). |
| `Cancel`    | lossless, to whoever was invited      | The call's ID: the invite no longer stands. |
| `Decline`   | lossless, back to the inviter         | The call's ID and whether I'm busy in another call. |
| `IAm`       | lossless, to each `Capable` friend    | A hash of a room's ID and my key in it. See [Who's who](#whos-who). |
| `CallState` | lossless, to everyone in the room     | The call's ID and version, whether I'm in it, whether I send audio and video, whether I'm muted, the codecs I decode, and a revision that goes up with each change. |
| `WantVideo` | lossless, to one sender               | Whether I want your video and the largest size I'll show it at. Wanting it also asks for a key frame. |
| `KeyFrame`  | lossless, to one sender               | Send me a key frame, I lost video. |
| `Report`    | lossy, to everyone in the call, every second | Bytes of yours lost and received, and the revision of your `CallState` I have. Sent whether or not media is flowing. |
| `Audio`     | lossy, to each peer in the call       | One Opus frame. |
| `Video`     | lossy, to each peer that wants it     | One piece of a video frame. |

Ringing isn't part of the protocol: whether an invite or a call rings,
notifies or shows quietly is up to the client.

The call ID is 64 random bits, chosen by whoever starts the call, so state from
an older call can't mix into a new one. A call room only ever has one call. In
a group's own room, two calls started at the same time become one: each person
moves to the other call if it has more people who may speak, or the same
number and a lower ID, and sends a new `CallState`. This way nobody can take
over a running call just by picking a low ID.

NGC may lose a lossless packet when a peer reconnects. So each `Report` says
which revision of the receiver's `CallState` its sender has, and an old one
gets the `CallState` sent again.

Leaving is a `CallState` that says so, and a peer that leaves the room has
left the call too. A peer that has sent no audio and no `Report` for 5 seconds
is reconnecting, and gets no video until it's back. Reports go every second, so
silence doesn't look like a lost peer.

### Media

Audio and video don't use the RTP header from calls with friends. Its 80 bytes
would cost more than the audio itself, and its pieces already fill a custom
packet, leaving no room for our three bytes. Instead, `Audio` and `Video` have a
short header of their own, about 12 bytes:

- A number that counts up by one for each frame sent to this peer, not for
  each frame of the stream. A frame left out for one receiver is no gap for
  it, so it counts no loss and asks for no key frame: senders can choose frames
  per receiver without receivers knowing. Silence leaves no gap either.
- The time the frame was captured, in milliseconds. It wraps around, so
  receivers compare differences, never sizes.
- The codec, as a number. A receiver drops frames in a codec it doesn't know.
- Flags, of two kinds: hints a receiver may ignore, like "key frame" (later
  "no later frame needs this one"), and flags that change how the payload is
  read, like "encrypted", where a receiver drops a frame with one it doesn't
  know. So the header can grow safely without a new version.
- For video, the piece's offset and the frame's length.

The payload is the same for every receiver and kept apart from the header, so
it can be encrypted end to end later.

## Bandwidth and CPU

Each sender has one audio and one video encoder per room, and the same payload
goes to everyone who wants it, so CPU use stays the same as a call grows. We
measure our upload apart from each peer's path: loss on every path at once is
our upload's fault, not theirs.

Audio comes first in the upload budget:

- It uses a constant bit rate while speaking, around 24 kbps, because a
  varying one gives away what's said. On a tight upload it steps down to
  16 kbps and then 12, but changes rarely, so it still reveals nothing about
  the speech.
- Opus in-band FEC lets a lost frame be rebuilt from the next one.
- During silence it sends almost nothing (Opus DTX), a tiny packet every
  400 ms or so. Someone watching the network can see when a person makes a
  sound, but never what it is.
- Frames are 20 ms and get longer (40 or 60 ms) as the call grows, so fewer
  packets carry the same audio.
- Muted people send nothing at all.

Video gets what's left. The encoder runs at the rate most of the peers that
want it can take; the others see a picture instead, so one weak receiver
doesn't make everyone's video worse. The main view is served before tiles, and
nobody gets less than about 100 kbps. The picture size is the largest anyone
asked for that the rate allows, from a short list. Rate, size and who's served
only change after being wrong for a while, so the call doesn't swing back and
forth.

We send key frames only when asked and at most one a second however many
peers ask. Everyone shares the encoder, so each key frame costs everyone.

Each room uses one codec: H.264 if everyone in the call decodes it, VP8
otherwise.

## Growing later

### Why calls have a size limit

Whoever speaks sends their audio to every other person in the call separately.
Each copy costs the audio, our header and NGC's per-packet overhead: roughly 40
to 70 kbps per person, depending on frame length (this still needs measuring).
A speaker in a call of 16 uploads to 15 people, up to about 1 Mbps. In a call
of 200 that would be 8 to 14 Mbps, more than a phone on mobile data has.

Everything else grows more slowly: silent and muted people send almost
nothing, and video degrades gently by serving fewer receivers. Audio can't: a
speaker who can't reach everyone isn't heard by some. So version 1 limits calls
to a size where an ordinary phone still reaches everyone, 16 for now, to be
checked by measuring.

### Without a new protocol

Two improvements need changes only on the sending side:

- Layers for VP8. One encoder produces frames in layers, so a peer on a weak
  link gets every second or fourth frame instead of none. Senders only leave
  out frames that no later frame needs, and the numbers per receiver hide them.
- A second, small encoding for tiles, soon after version 1. At about 100 kbps
  per receiver a phone serves only a handful of people, so in a call of 10
  most would see pictures. A tile has a sixteenth of the pixels, so it costs
  little CPU.

### Calls of 200 and more

This needs version 2, which version 1 leaves room for. It works like Google
Meet, with some people in the call doing what Meet's servers do:

- Forwarders. A few people on good links, with their consent, pass media on.
  A speaker sends once, to one forwarder; each forwarder serves a share of the
  call and passes streams to the others. A speaker's upload no longer grows
  with the call.
- End-to-end encryption of payloads, so forwarders only see headers. Each
  sender encrypts with its own key and gives it to the others over NGC. We'd
  use SFrame (RFC 9605), whose inner counter also lets receivers notice a
  forwarder leaving frames out.
- Only what each receiver needs: audio from the few loudest speakers (so the
  header carries an audio level again, as forwarders can't decode), and the
  video each receiver asked for, in the layers its link can take.
- Forwarders number frames for each receiver, as senders already do in
  version 1, so receivers don't change.

### Can NGC hold groups that large?

Every NGC member is connected to every other one, so in a group of 200 each
keeps 199 connections. Nobody has measured whether that's too costly at 200 to
500 members, especially on a phone, so the first step is to measure it in the
simulator.

If it is, the answer is groups where each member keeps a few connections that
change as people come and go, with messages passed between neighbours and
signed by their senders. c-toxcore expects everyone connected to everyone, so
this would be a new kind of group: in effect a new version of NGC, much bigger
than group calls, best designed with c-toxcore's developers. The old
conferences worked this way and were slow and fragile, so it would have to do
better.

Calls of up to 16 people don't need any of this.

## Code

hs-toxav only knows rooms as a type parameter and the room in an invite as
bytes. Creating, joining and leaving groups is up to the client, using
hs-toxcore.

- A shared stream layer. The parts of `Tox.AV.Session` that handle one
  direction of media move into their own module: assembling frames, the jitter
  buffer, the loss meter and the statistics for receiving, and numbering and
  the rate for sending. Calls with friends and group calls both use it.
- `Tox.AV.Group.Protocol` has the messages, with `encode` and `decode`, tested
  in both directions like `Tox.AV.MSI`.
- `Tox.AV.Group.Session` is pure, like `Tox.AV.Session`. It holds the calls by
  room, their members and state, who may speak, who wants whose video, the
  meters, who's served, and the key frames owed. Sending is per room, and
  receiving is per room and peer. We test it with several simulated peers.
- `Tox.AV.Mixer` is pure as well. It mixes 10 ms from each peer's playout
  without clipping, and gives each peer's level, which shows who's speaking.
  The client's echo cancellation must use the mixed audio.
- `Tox.AV.Group` is the handle for group calls, next to `Tox.AV` for calls with
  friends, and shares its codecs. It has:
  - `RoomTransport r p`, to send to everyone in a room or to one peer;
  - inputs for packets, peers joining and leaving the room, and changes to who
    may speak, including ourselves;
  - `startCall`, `joinCall`, `leaveCall`, `wantVideo`, and sending audio and
    video to a room;
  - `invite`, `cancel`, `decline`, and the `Capable` and `IAm` messages;
  - events for being invited, an invite cancelled or declined, the call's
    members and their state, each peer's level and video, a peer not sending us
    video, and the call ending.
- hs-toxcore needs three additions: joining a private group with the details
  from an invite, without a friend; groups that aren't saved; and an event when
  a known peer reconnects.

## Order of work

1. Move out the stream layer. Nothing changes for calls with friends, and their
   tests stay as they are.
2. Picture size from bit rate, for calls with friends, since group video builds
   on it.
3. Write the protocol down, then `Tox.AV.Group.Protocol` and its tests.
4. Audio-only calls in a group's own room and the mixer.
5. Who's who and `Capable`.
6. Call rooms and invites, with hs-toxcore's direct join and unsaved groups.
   Measure how long joining takes and the audio delay through TCP relays,
   where lossy packets queue behind each other, in the simulator and on the
   real network.
7. Video: who wants what, who's served, key frames, and the codec per room.
8. Moving a call with one friend into a call room.
9. The client side: rooms, invites, ringing and the UI.

## Where the spec lives

Here, next to the code, while it settles. It can move to the spec repository
later: the protocol section only has what peers need to agree on, and our
implementation's choices go in code comments, as in hs-toxcore.

Before it moves, the wire formats of calls with friends (MSI, RTP framing and
bandwidth reports) need a spec of their own. So far, c-toxcore's code is their
only definition.

## Still to settle

- Whether Green-Sky is fine with 0xb0 as TokTok's range.
- The field layouts, including the media header, and size limits.
- The friend packet IDs for the messages that go outside a room.
- How long "a while" is for each change in video and the list of picture
  sizes.
- The size limit, from measured costs: audio per person and what phones can
  upload.

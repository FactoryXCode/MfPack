# MfPack Cast media support

The units in `Cast/Media` provide subtitle readers, timed-text storage,
composition, frame pumping, and the direct and transcoded preview implementations.
They are shared by the Cast facade without depending on an application form or
player class.

`TMfCast.Create(True)` creates the segment publisher, remux, transcode,
desktop-capture, and subtitle-compositor pipelines. The facade also creates
the direct preview player and starts Media Foundation for inspection and track
enumeration when constructed with `False`.

The automatic media planner selects one of three main file routes:

- Compatible media is served directly to the receiver.
- Frame-aligned H.264 video with optional AAC audio in a compatible Matroska
  source is remuxed to fragmented MP4 without decoding or re-encoding samples.
- Incompatible codecs or a request to burn subtitles into video use the
  transcode pipeline.

The direct and remux routes use an independent Media Foundation Media Session
with EVR for local preview. Transcoding supplies decoded video and audio samples
to the window preview sink. The subtitle compositor burns selected text tracks,
`.idx`/`.sub` VobSub sidecars, and embedded Matroska `S_VOBSUB` bitmap tracks into
transcoded video. It also supplies the selected subtitle image to the EVR
preview. Matroska text tracks are read as timed cues; embedded VobSub packets
are read as binary data, inflated when zlib-compressed, and decoded with the
same bitmap decoder used for sidecars. Embedded PGS tracks are identified but
not decoded.

The Matroska reader accepts a clipped test file whose declared Segment length
exceeds the available file prefix. It stops at an incomplete tail and retains
subtitle cues parsed before that point.

Metadata strings decode as UTF-8 with replacement for invalid byte sequences,
so malformed metadata does not raise a first-chance encoding exception in XE7.

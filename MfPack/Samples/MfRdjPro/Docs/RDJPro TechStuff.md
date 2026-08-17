# RDJPro Technical Handout

Version: 3.2.0

RDJPro is the extended MfRdj sample application. It combines MfPack, Microsoft Media Foundation, Core Audio, WASAPI, VCL, Caddy, and browser Media Source Extensions into one live audio/video DJ and broadcast application.

The project is intentionally larger than a small sample. It is meant to show how several real-time Windows media technologies can work together in one application: local DJ playback, microphone and loopback capture, audio processing, recording, playlist editing, and browser-based broadcasting.

## What RDJPro Does

RDJPro is an MDI DJ mixer application with channel decks, loopback decks, a microphone deck, an effects deck, an end deck, playlist editing, tag editing, local recording, and Caddy broadcast support.

The normal audio path is built around Windows audio devices and MfPack helpers. Deck audio is mixed inside RDJPro and sent to the selected master output. A separate PFL/cue/headphone output can be enabled for monitoring.

The broadcast path publishes the live program to a browser. The browser does not need Icecast for the RDJPro video path. Caddy serves the web page, JSON metadata, artwork, and fragmented MP4 media files.

## Main Technologies

RDJPro uses these main Windows and web technologies:

- Delphi VCL for the desktop GUI.
- MfPack for Media Foundation and WASAPI access.
- Core Audio and WASAPI for device capture, playback, and audio routing.
- Media Foundation Source Reader for video/camera sample capture.
- Media Foundation Sink Writer and MPEG-4 media sink for MP4/fMP4 encoding.
- A custom IMFByteStream proxy to observe fragmented MP4 bytes while Media Foundation writes them.
- Caddy as a static web server and reverse proxy.
- Browser Media Source Extensions, usually called MSE, for live fMP4 playback in the browser.
- JSON files for now-playing metadata and the live stream manifest.

## Local Audio And Video Flow

The local RDJPro application keeps the desktop side and browser side separate.

On the desktop side:

1. Decks, loopback inputs, and microphone input produce audio.
2. RDJPro mixes and processes the audio.
3. The result is sent to the selected master output.
4. Optional monitoring goes to the headphone/PFL output.
5. Optional local recording can save WAV, FLAC, or MP4 depending on the recorder path.

On the video side:

1. RDJPro can use a camera source or a static image source.
2. Video samples are normalized into a Media Foundation video stream.
3. Audio samples from the RDJ program output are batched and timestamped.
4. Audio and video are passed to the broadcast fMP4 recorder.

## Browser Broadcast Flow

The browser broadcast path is designed as a rolling live fMP4 stream.

The high-level flow is:

1. RDJPro receives live video samples and live program audio samples.
2. RDJPro feeds those samples to a Media Foundation Sink Writer.
3. Media Foundation encodes H.264 video and AAC audio into fragmented MP4.
4. A custom byte-stream wrapper observes the MP4 boxes as Media Foundation writes them.
5. RDJPro extracts init data and moof+mdat fragments.
6. RDJPro patches fragment sequence and decode-time metadata for browser MSE playback.
7. Several small internal Media Foundation fragments are grouped into larger public fragments.
8. RDJPro writes `init.mp4`, `patched_frag_XXXXXX.m4s`, and `live.json`.
9. A mirror thread copies those files to the Caddy server folder.
10. The browser polls `live.json`, fetches the fragment files, and appends them to a MediaSource SourceBuffer.

The public browser stream is therefore not one endlessly growing MP4 file. It is a moving set of small files:

- `init.mp4`
- `live.json`
- `patched_frag_000001.m4s`
- `patched_frag_000002.m4s`
- and so on

The browser uses `live.json` as the scoreboard for the current live window.

## Why Fragmented MP4

A normal MP4 file is not ideal for live browser playback because important metadata is often finalized at the end of the file. A browser cannot reliably play a file whose final index is not available yet.

Fragmented MP4 solves this by splitting media into independent fragments. Each fragment contains timing and sample information that the browser can append while the stream is still being produced.

RDJPro uses the typical fMP4/MSE pattern:

- The init segment contains codec and track setup information.
- Each media fragment contains one or more `moof` and `mdat` box pairs.
- `moof` describes the samples and timestamps.
- `mdat` contains the encoded media bytes.

The browser receives the init segment first, then receives new media fragments as they become available.

## Media Foundation Implementation

The broadcast implementation lives mainly in:

- `RDJ.RdjPro.BroadcastFmp4Recorder.pas`
- `dlgMediaServer.pas`

The recorder uses Media Foundation in a deliberately conservative way:

1. RDJPro creates target media types for H.264 video and AAC audio.
2. RDJPro creates a byte stream for Media Foundation output.
3. RDJPro creates an MPEG-4 media sink with `MFCreateFMPEG4MediaSink`.
4. RDJPro creates a Sink Writer from that media sink.
5. RDJPro writes video samples and audio samples to the Sink Writer.
6. Media Foundation writes fragmented MP4 bytes to the byte stream.

The important RDJPro trick is that the byte stream is not only a file stream. RDJPro wraps it in `TRdjProFmp4CaptureByteStream`, which implements `IMFByteStream`.

That wrapper forwards writes to the real Media Foundation file stream, but it also observes the bytes. This lets RDJPro parse top-level MP4 boxes while Media Foundation is writing.

The wrapper watches for:

- `ftyp`
- `moov`
- `moof`
- `mdat`

Once it has a matching `moof` and `mdat`, it builds a complete fMP4 fragment for the live browser path.

## Fragment Patching

Media Foundation produces valid fMP4, but the browser live path needs fragments to appear as a clean rolling sequence.

RDJPro therefore patches selected fragment metadata before publishing the fragment to Caddy. In particular, it tracks decode times per track and rewrites the relevant sequence/timing information so the browser can append fragments consistently.

This patching is done inside `TRdjProFmp4CaptureByteStream.PatchFragmentForMse`.

The original Media Foundation bytes are treated as the source. RDJPro produces a patched public fragment for MSE. This keeps the Media Foundation encoder path simple, while allowing the browser stream to stay live and appendable.

## Public Fragment Grouping

Media Foundation can emit small internal fragments. Very small public fragments are expensive for a browser and for a file-server mirror, because they create many file writes, many Caddy requests, and many MSE append operations.

RDJPro groups several internal source fragments into one public `.m4s` file.

The default grouping is approximately four source fragments per public fragment. The exact public fragment duration is controlled by the broadcast setup values and the constants in `dlgMediaServer.pas`.

This grouping reduces:

- file-system pressure
- Caddy request count
- browser SourceBuffer churn
- UNC mirror traffic
- debug/log noise

## The Caddy Mirror

RDJPro can publish directly to a local Caddy folder or mirror files to a Caddy folder on another machine, often through a UNC path such as:

```text
\\PCHP001\Caddy\stream
```

The mirror is handled by `TRdjProMseMirrorThread`.

The mirror thread copies media fragments and writes `live.json` safely. It does not publish a fragment in `live.json` until that fragment was successfully mirrored to the public side.

That rule is important. If `live.json` announces a fragment before the file is present, the browser can request a file that does not exist yet and stall.

The mirror thread also has recovery logic. If a fragment gap becomes stale and is already outside the public live window, the thread resynchronizes to the newest successfully mirrored fragment. This prevents the public manifest from being pinned forever to an old fragment number.

## Browser Playback

The browser page uses JavaScript and Media Source Extensions.

The basic browser behavior is:

1. Fetch `live.json`.
2. Fetch `init.mp4`.
3. Create a MediaSource.
4. Add a SourceBuffer with the codec string from `live.json`.
5. Fetch missing `patched_frag_XXXXXX.m4s` files.
6. Append them to the SourceBuffer.
7. Keep polling `live.json` and append newer fragments.
8. Drop old buffered ranges so playback stays near live.

The browser also reads `nowplaying.json` for artist, title, cover art, DJ name, and listener count.

## Now Playing And Listener Count

RDJPro writes `nowplaying.json` for the browser page.

Typical fields are:

- `onAir`
- `onAirLock`
- `djName`
- `show`
- `artist`
- `title`
- `coverUrl`
- `listeners`
- `displayListeners`

Listener count is derived from the Caddy access log. Caddy must be configured with JSON access logging, for example:

```caddyfile
log {
    output file C:\Caddy\Caddy.log
    format json
}
```

RDJPro reads recent Caddy log entries, counts unique recent client IP addresses that request live stream URLs, and writes that count into `nowplaying.json`.

## Reliability Measures

Several safeguards are built into the current RDJPro broadcast path.

### Sleep And Lid Close

RDJPro can request Windows execution-state protection to avoid accidental sleep during broadcasting. There is also a setup option for overriding lid-close sleep behavior where supported.

This matters because system sleep can stop audio and device pipelines in ways that do not always recover cleanly.

### Memory Control

The live broadcast path is designed to avoid unbounded memory growth.

Important controls include:

- bounded video and audio queues
- stale audio/video trimming
- limited MSE fragment queues
- public fragment grouping
- mirror jobs using filenames instead of copying large media payloads into queued byte arrays
- explicit shutdown of Media Foundation sinks and byte streams

### Shutdown Cleanup

Media Foundation objects are explicitly finalized and shut down. The MPEG-4 media sink is shut down so it releases its byte-stream reference. The capture byte stream releases its live queues during shutdown.

This avoids the shutdown leaks that can otherwise appear after stopping the app while broadcasting.

### Atomic File Publishing

JSON and manifest files are written through temporary files and then replaced. This prevents the browser or Caddy from reading half-written JSON.

RDJPro uses retry logic for file replacement because Caddy, browsers, SMB, or antivirus software can briefly hold a file handle.

### Debug Output Throttling

Long-running broadcasts can produce a huge amount of debug text. When running inside the Delphi IDE, the IDE debug-output buffer can become a problem by itself.

High-frequency logs are therefore throttled. Important state changes, errors, and occasional progress markers remain visible, but per-fragment debug output is reduced.

## Operational Checklist

For a stable RDJPro browser broadcast:

1. Use Windows 10 or later.
2. Configure RDJPro Caddy paths correctly.
3. Make sure Caddy serves the RDJPro web folder.
4. Enable Caddy JSON access logging if listener count is needed.
5. Make sure the Caddy stream folder is writable by RDJPro.
6. Keep the machine awake while broadcasting.
7. Use HTTPS if browsers require secure media or PWA behavior.
8. Watch `live.json` and `nowplaying.json` when diagnosing browser issues.
9. Watch RDJPro MSE memory heartbeat lines for queue growth or mirror stalls.

## Important Files

Main RDJPro files:

- `frmMainMDI.pas` handles application-level setup, now-playing JSON, listener count, and global state.
- `dlgMediaServer.pas` handles broadcast controls, Caddy file publishing, live MSE manifest generation, and the mirror thread.
- `RDJ.RdjPro.BroadcastFmp4Recorder.pas` handles Media Foundation fMP4 recording, byte-stream observation, fragment extraction, and fragment patching.
- `RDJ.JSon.pas` writes and reads browser JSON state.

Caddy/browser files:

- `Deploy\Server\Caddy\caddy.cff`
- `Deploy\Server\Caddy\index.html`
- `Deploy\Server\Caddy\nowplaying.json`
- `Deploy\Server\Caddy\stream\rdj_stream.html`

Typical runtime public stream files:

- `init.mp4`
- `live.json`
- `patched_frag_XXXXXX.m4s`

## Summary

RDJPro uses Media Foundation for the hard media work: capture, encode, mux, and write fragmented MP4. RDJPro adds the live-broadcast layer around it: observing the fMP4 byte stream, extracting and patching fragments, grouping fragments for practical browser delivery, publishing a rolling manifest, mirroring to Caddy, and feeding a browser MSE player.

The result is a Windows desktop DJ application that can broadcast synchronized audio/video to modern browsers using standard web playback technology and a simple Caddy file server.


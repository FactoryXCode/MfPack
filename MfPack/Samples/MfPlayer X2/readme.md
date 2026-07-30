# MfPlayer X2

MfPlayer X2 is an advanced Media Foundation player sample for Delphi.

The sample is based on the earlier MfPlayer X project, but replaces the floating
subtitle overlay with a real video-frame subtitle pipeline. Subtitle pixels can
now be composed into the decoded video for local playback, MP4 export, and
Chromecast streaming.

MfPlayer X2 also demonstrates how Media Foundation playback, timed text,
custom MFT processing, Source Reader/Sink Writer transcoding, `IMFByteStream`,
mDNS, TLS, Google Cast V2 messages, and a local HTTP server can work together
inside one Delphi application.

---

# Main differences from MfPlayer X

The earlier MfPlayer X sample demonstrates `IMFTimer`, language tags, media
properties, regular expressions, and subtitle formats such as SubRip,
MicroDVD, and WebVTT.

MfPlayer X2 keeps those foundations and adds a frame-based subtitle architecture:

* The floating subtitle form is no longer used for playback.
* Subtitle text is composed directly into RGB32 video frames.
* A custom synchronous `IMFTransform` inserts subtitles into the local playback
  topology before the Enhanced Video Renderer.
* The same compositor is reused by the export and Chromecast transcode paths.
* Media Session topology construction and stream selection have been revised.
* Player shutdown and COM/interface lifetime handling have been hardened.
* H.264/AAC MP4 export with burned subtitles is included.
* Chromecast device discovery, control, HTTP publication, and automatic
  transcoding fallback are integrated.

The important architectural change is:

```text
MfPlayer X
Media source -> Media Session -> Video renderer
Timed text  -> Floating subtitle window

MfPlayer X2
Media source -> Decoder -> RGB32 subtitle MFT -> EVR
                              |
                              +-> MP4 export
                              +-> Chromecast transcode stream
```

---

# Features

## Local playback

* Media Foundation Media Session based audio/video playback.
* Play, pause, resume, stop, seek, and playback-rate control.
* Audio-only, video-only, and audio/video source handling.
* Stream selection dialog.
* Corrected selection of usable audio and video streams.
* Metadata and unsupported data streams are excluded from the playback topology.
* Media properties and language-tag support.
* Presentation-clock and `IMFTimer` integration.
* Deterministic Media Session shutdown.

## Timed text and subtitles

* SubRip (`.srt`) support.
* MicroDVD (`.sub`) support.
* WebVTT (`.vtt`) support.
* Language-tag parsing and subtitle-language selection.
* Timestamp-based subtitle lookup.
* `TMfMediaTimeline` fallback when samples do not provide usable timestamps.
* Shared RGB32 subtitle compositor.
* Custom in-place subtitle video MFT for local playback.
* Subtitle bitmap caching and reset handling.
* Subtitle burn-in without modifying the original source file.

## Export

* Source Reader based video and audio decoding.
* RGB32 video-frame subtitle composition.
* Media Foundation H.264 video encoding.
* Media Foundation AAC audio encoding.
* MP4 output through a Sink Writer.
* Output to a normal file or an injected `IMFByteStream`.
* Local preview while exporting.
* Progress reporting.
* Pause, resume, and cancellation support.
* Video-only source support.

## Chromecast

* Chromecast and Google/Android TV discovery through mDNS/DNS-SD.
* Simple VLC-style device-selection dialog.
* No normal-user IP address, interface, or port configuration.
* Automatic local network-interface selection.
* Automatic HTTP listening-port selection.
* Cast V2 TLS control channel.
* Receiver launch and media-control messages.
* Local HTTP publication with `GET`, `HEAD`, and byte-range support.
* Direct-file casting route.
* Transcoded fragmented-MP4 route.
* H.264/AAC fallback for incompatible or subtitled media.
* Shared local preview and Chromecast transcode pipeline.
* Play, pause, resume, seek, volume, mute, and stop command interfaces.
* Transactional cleanup after failed Cast attempts.
* Offline-TV and repeated-cast protection.
* Reference-counted live-stream buffers to prevent stale writer access.

---

# Requirements

* Windows 10 version 1703 or later.
* Delphi XE7 or later, including Delphi 12.
* MfPack 3.2.0 headers and units.
* Windows SDK baseline used by MfPack: `10.0.26100.4654`.
* A Media Foundation compatible video and audio decoder for the selected source.
* An H.264 encoder and AAC encoder for export or transcoded casting.
* A Chromecast, Chromecast-enabled television, or Google/Android TV device for Cast tests.
* The PC and Cast receiver must be reachable on the same local network.
* Windows Firewall must permit the temporary local HTTP listener.

---

# Building the sample

1. Install or configure MfPack.
2. Make sure the MfPack source (src) directory is present in the Delphi search path.
3. Open `TMFPlayerX2.dproj`.
4. Select the Win32 target.
5. Build and run the project.

The current source is maintained with Delphi XE7 compatibility in mind. Avoid
replacing compatibility code with newer RTL methods unless the minimum compiler
version is intentionally changed.

Examples of relevant XE7 differences:

* `TDictionary.Remove` is a procedure.
* Some newer `TFile` convenience methods are unavailable.
* Inline local-variable declarations are not supported.
* Media Foundation and Windows API types must come from the MfPack headers used
  by the project.

---

# Using MfPlayer X2

## Playback

1. Open a media file.
2. Select the required streams when the source contains multiple usable streams.
3. Select a subtitle language when matching timed-text sidecar files are found.
4. Use the normal player controls for playback and seeking.

MfPlayer X2 searches for supported timed-text files associated with the selected
media source. The subtitle compositor uses the current Media Foundation
presentation time to determine which cue must be displayed.

## Exporting a subtitled MP4

1. Open the source media.
2. Select and enable the required subtitle track.
3. Choose the subtitled MP4 export command.
4. Select the output filename.
5. Use the export controls to pause, resume, or cancel the operation.

The source file is not changed. The exported video contains the subtitle pixels
permanently burned into the encoded video frames.

## Casting

1. Open a media file.
2. Choose **Cast To...**.
3. Select the Chromecast or Google TV device.
4. Start casting.

Network-interface selection, HTTP binding, publication URL construction, and
route selection are automatic. The ordinary user is not expected to configure
network details.

The controller selects one of these routes:

```text
Compatible source
    -> publish original file
    -> Chromecast reads the file through HTTP

Source requiring conversion or burned subtitles
    -> decode with Source Reader
    -> compose subtitles into RGB32 frames
    -> encode H.264/AAC fragmented MP4
    -> publish generated stream through HTTP
    -> Chromecast reads the live stream
```

Stopping a Cast session shuts down the transcode worker, live publisher, Cast
channel, and local HTTP listener.

---

# Project structure

## Main player units

### `TMFPlayerX2.dpr`

Application entry point and form creation.

### `frmMfPlayer.pas` / `frmMfPlayer.dfm`

Main VCL player form. It connects the menus and controls to local playback,
subtitle selection, export, and Chromecast operations.

### `MfPlayerClassX.pas`

Main Media Foundation player class. It owns the Media Session, media source,
topology, renderer services, playback state, stream selection, subtitle MFT
integration, presentation clock, and shutdown sequence.

The class name remains `TMfPlayerX` for compatibility with the earlier sample.

### `MFTimerCallBackClass.pas`

Presentation-clock timer callback and clock-state sink implementation.

### `MfPCXConstants.pas`

Player constants and application-specific identifiers.

### `UniThreadTimer.pas`

Threaded timer helper used by the player infrastructure.

---

# Subtitle and export units

### `TimedTextClass.pas`

Timed-text parsing, cue storage, and time-based subtitle lookup.

### `LangTags.pas`

BCP 47 and media language-tag support.

### `MfMediaTimelineX2.pas`

Media-time tracking and timestamp fallback.

### `MfSubtitleCompositorX2.pas`

Shared timed-text compositor. It converts the active subtitle cue into pixels
and blends those pixels into RGB32 video frames.

### `MfSubtitleTransformX2.pas`

Custom synchronous Media Foundation Transform used in the local playback
topology to apply the subtitle compositor before video rendering.

### `MfSubtitleFramePumpX2.pas`

Source Reader/Sink Writer export pipeline. It decodes source samples, processes
video frames, encodes H.264/AAC output, writes MP4 data, and optionally provides
a local preview.

### Subtitle dialogs

* `dlgSelectTimedTextLanguages.pas`
* `dlgTimedTextLanguages.pas`
* `dlgStreamSelect.pas`

These units provide stream and timed-text language selection.

---

# ChromeCast directory

The `ChromeCast` directory contains the complete Cast subsystem.

### `MfCastTypes.pas`

Shared enumerations, records, settings, device descriptions, media descriptions,
status values, errors, requests, and callback declarations.

### `MfCastInterfaces.pas`

Interfaces between discovery, transport, Cast channel, HTTP server, media
inspection, route planning, transcoding, preview, publishing, and the controller.

### `MfCastDiscovery.pas`

mDNS/DNS-SD discovery for `_googlecast._tcp.local`. It discovers devices and
reports additions, changes, removals, and discovery errors through callbacks.

### `MfCastTransport.pas`

TCP and TLS transport for the Google Cast control connection. It performs
socket connection, Schannel negotiation, framed reads/writes, timeout handling,
and transport shutdown.

### `MfCastChannel.pas`

Cast V2 protocol channel. It creates and parses framed protobuf messages,
handles Cast namespaces, launches the receiver, sends media commands, processes
status messages, and manages request identifiers.

### `MfCastHttpServer.pas`

Local HTTP publication layer.

It contains:

* direct-file HTTP content;
* a range-capable HTTP server;
* live-stream content;
* a reference-counted live buffer;
* an `IMFByteStream` implementation backed by that buffer;
* the Cast segment/presentation publisher.

### `MfCastMedia.pas`

Media inspection, receiver capability profiles, and automatic route planning.

### `MfCastTranscode.pas`

Adapter around the MfPlayer X2 export/frame-pump pipeline. It writes generated
fragmented MP4 data to the live Cast byte stream and coordinates start, stop,
pause, resume, and worker shutdown.

### `MfCastController.pas`

Top-level Cast state machine and orchestrator.

It coordinates:

* discovery;
* media inspection;
* route selection;
* HTTP publication;
* transport connection;
* receiver launch;
* deferred transcode start;
* media loading;
* playback commands;
* GUI callbacks;
* rollback and cleanup after failure.

### `dlgMfCastDevices.pas` / `dlgMfCastDevices.dfm`

Simple device picker. The normal user only selects a receiver. Network details
remain automatic and hidden.

---

# Chromecast data flow

```text
                     +----------------------+
                     |   MfPlayer X2 GUI    |
                     +----------+-----------+
                                |
                                v
                     +----------------------+
                     |  TMfCastController   |
                     +---+---------+--------+
                         |         |
             discovery   |         | control
                         v         v
                 +-----------+  +----------------+
                 |   mDNS    |  | Cast V2 TLS    |
                 | discovery |  | channel :8009  |
                 +-----------+  +----------------+
                                      |
                                      v
                              Chromecast receiver

Media path:

Source file
   |
   +-> Direct route ----------------------------+
   |                                            |
   +-> Source Reader                            |
       -> RGB32 subtitle compositor             |
       -> H.264/AAC encoder                     |
       -> fragmented MP4 IMFByteStream          |
                                                v
                                      Local HTTP server
                                                |
                                                v
                                      Chromecast fetches
                                      the media resource
```

The Cast control connection does not carry the video itself. It tells the
receiver which URL to load. The Chromecast then opens the URL from the local
HTTP server running inside MfPlayer X2.

---

# Cast state and failure handling

The main states include:

```text
Idle
  -> Discovering
  -> Preparing
  -> Connecting
  -> Launching receiver
  -> Connected
  -> Buffering
  -> Playing / Paused
  -> Stopping
  -> Stopped

Any active stage
  -> Error
  -> complete rollback
```

A failed connection must not leave a transcode worker or HTTP resource active.
The controller therefore performs transactional cleanup:

1. Stop and join the transcode worker.
2. Abort and unpublish the live presentation.
3. Disconnect the Cast control channel.
4. Stop the HTTP server.
5. Clear pending requests and device state.
6. Update the GUI and re-enable **Cast To...**.

The live buffer is interface-owned. An older byte stream can therefore not keep
a dangling pointer to a buffer that was freed by a later Cast attempt.

---

# Current development status

The local player, timed-text compositor, playback subtitle MFT, MP4 export, Cast
discovery, Cast transport, local HTTP publication, direct-file route, and
transcoded fragmented-MP4 route are implemented.

The Chromecast subsystem is still under active development and should be
treated as an advanced sample rather than a production Cast SDK.

Important remaining work includes:

* persistent Cast receive and heartbeat processing;
* complete codec-level source inspection;
* device-specific capability profiles;
* passing the active subtitle source and language into automatic Cast planning;
* complete external WebVTT track publication and activation;
* hardened multi-client HTTP handling;
* random protected publication paths;
* stronger TLS peer validation;
* complete HLS manifest and rolling-fragment support;
* broader automated integration testing.

The present working live route is a growing fragmented MP4 resource. The
presence of HLS-related types and settings does not mean full HLS output is
complete.

---

# Debugging and testing

The project uses HRESULT-based error reporting, `OutputDebugString`, and
madExcept during development.

Recommended minimum tests:

* Open a file and close without starting playback.
* Close while playing, paused, seeking, or exporting.
* Verify no MadExcept memory leaks.
* Play audio/video, video-only, and audio-only files.
* Test SubRip, MicroDVD, and WebVTT subtitles.
* Export with subtitles, pause/resume, and cancel.
* Discover no Cast devices, one device, and multiple devices.
* Cast with the TV on.
* Attempt to cast with the TV off.
* Retry after a failed Cast attempt.
* Stop during active transcoding.
* Close the application during an active Cast session.
* Test with Windows Firewall blocked.
* Test with Ethernet, Wi-Fi, VPN, and multiple adapters.

---

# Documentation

The full design and implementation reference is located at:

```text
Docs\MfPlayer_X2_Architecture_and_Chromecast.md
```

It contains:

* a detailed MfPlayer X versus MfPlayer X2 comparison;
* local playback and topology construction;
* subtitle timing and composition;
* export architecture;
* complete Chromecast unit documentation;
* state, threading, ownership, and lifetime models;
* known limitations;
* implementation roadmap;
* detailed test plan.

---

# Notes

This is not a sample for absolute beginners.

The project combines Media Foundation Media Sessions, Source Readers, Sink
Writers, custom MFTs, COM lifetime management, asynchronous callbacks, sockets,
TLS, mDNS, HTTP, and Google Cast protocol messages. A good understanding of
Media Foundation, Windows multimedia APIs, threading, and interface ownership
is recommended before modifying the core pipeline.

Do not replace MfPack Windows, Media Foundation, or DirectX declarations with
unrelated Delphi header translations. The project is intended to remain
consistent with the MfPack SDK baseline.

---

# Project

MfPack - Samples - MfPlayer X2

* GitHub: <https://github.com/FactoryXCode/MfPack>
* SourceForge: <https://sourceforge.net/projects/MFPack>

Copyright (c) FactoryX. All rights reserved.

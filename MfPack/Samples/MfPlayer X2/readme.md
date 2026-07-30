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
* Windows SDK: `10.0.26100.4654`.
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

The current source is maintained with Delphi XE7 compatibility in mind.

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

---

# Project

MfPack - Samples - MfPlayer X2

* GitHub: <https://github.com/FactoryXCode/MfPack>
* SourceForge: <https://sourceforge.net/projects/MFPack>

Copyright (c) FactoryX. All rights reserved.

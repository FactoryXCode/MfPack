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
* Embedded textual subtitle streams exposed by Media Foundation.
* Matroska SRT/UTF-8 and SSA/ASS subtitle import.
* Sidecar-first, embedded-subtitle fallback selection by preferred language.
* Language-tag parsing and subtitle-language selection.
* Timestamp-based subtitle lookup.
* `TMfMediaTimeline` fallback when samples do not provide usable timestamps.
* Shared RGB32 subtitle compositor.
* Custom in-place subtitle video MFT for local playback.
* Subtitle bitmap caching and reset handling.
* Subtitle burn-in without modifying the original source file.
* The same embedded cue model feeds local playback, export, and Chromecast.
  
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
  
## Embedded subtitle boundaries
  
The current embedded reader handles textual subtitle streams that the active  
Media Foundation source exposes as `MFMediaType_Subtitle`. Matroska SRT/UTF-8  
and SSA/ASS tracks are imported into `TMfTimedText`. PGS and VobSub tracks are  
detected but not decoded because they contain subtitle images rather than text.  
  
The native Microsoft MP4 source exposes audio and video streams but ignores  
non-audio/video tracks. Embedded MP4 `tx3g`, `wvtt`, and `stpp`, plus AVI text  
variants hidden by the native source, therefore require container adapters and  
are not claimed as supported by this snapshot.  
  
# Requirements
  
* Windows 10 version 1703 or later.  
* Delphi XE7 or later, including Delphi 12.  
* MfPack 4.1.0.  
* Windows SDK: `10.0.28000.2705`.  
* Chromecast, Chromecast-enabled television, Google/Android TV device or Cromecast supported speakers.  
* The PC and Cast receiver must be reachable on the same local network.  
* Windows Firewall must permit the temporary local HTTP listener.  
  
---
  
# Building the sample
  
1. Install or configure MfPack.
2. Make sure the MfPack source (src) directory is present in the Delphi search path.
3. Open `TMFPlayerX2.dproj`.
4. Select the Win32/Win64 target.
5. Build and run the project.
  
  
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
  
  
### Automatic receiver wake-up

All applications using the shared TMfCast controller try the normal Cast
connection first. After a retryable connection failure, a known receiver MAC
address enables native Wake-on-LAN recovery, followed by bounded Cast connection
retries. Receivers that connect immediately, including always-available speakers,
use the normal path without sending wake packets.

The MAC address is learned from the Windows IPv4 neighbor table during discovery
and after a successful connection. Discover or connect once while the receiver is
awake. The per-receiver cache is shared by these applications for the current
Windows user at `%LOCALAPPDATA%\MfPack\Cast\WakeTargets.ini`. It is keyed by the
Cast device identity, not just an IP address. The current local interface and
subnet broadcast address are selected again for each attempt. A missing or
unusable MAC skips wake-up and retains ordinary retries.

Wake-on-LAN uses three bursts of standard 102-byte magic packets over UDP port 9,
addressed to the receiver's MAC via subnet broadcast and unicast. The receiver
must support network wake-up. HDMI-CEC can also turn connected equipment on.
This is native Delphi/Windows code; Python and remote-control pairing are not
required. Logs distinguish wake packets sent, wake-up skipped, and connection
retry results. File, URL, desktop, and RDJ live-stream casting share this behavior.

---
  
Project: MfPack - Samples - MfPlayer X2  
   
Project location: https://github.com/FactoryXCode/MfPack   
Project location: http://sourceforge.net/projects/MFPack  
  
First release date: 01-02-2022  
Final release date: 18/11/2026  
  
Copyright © FactoryX. All rights reserved. 
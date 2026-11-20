# MfCaptureVideoFromGPU
  
Version: 4.1.0  
  
Description:  
Demonstrates how to capture from screen using your videocard GPU.  
  
NOTES:  
  
* This release is updated for compiler version 3.2.2 (64 bit).
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.1.0
  
\*\*This sample demonstrates how to capture from screen using the GPU of your video card\*\*  
  
This is how it globally works:  
  
1. TCaptureStreamEngine  
  
- Creates a \*\*D3D11\*\* device + immediate context.
- Sets up Desktop Duplication on output 0.
  
Each frame:  
- AcquireNextFrame → \*\*ID3D11Texture2D\*\* (BGRA desktop).
- Shows it via TPreviewRenderer.
- Maps a \*\*DXGI\_FORMAT\_NV12\*\* staging texture for CPU write.
- Calls TGpuNV12Converter.Convert(BGRA, mappedNV12).
- Packs NV12 into an MF buffer and feeds it into an H.264 sinkwriter.
  
2. TGpuNV12Converter  
  
- Has its own Y (R8\_UNORM) and UV (R8G8\_UNORM @ half res) render targets + staging textures.
- Draws a fullscreen quad:
  - Pass 1: BGRA → Y.
  - Pass 2: BGRA → UV (half res).
- Maps the Y/UV staging textures and copies them into the mapped NV12 buffer (Y full res, UV interleaved).
  
3. TPreviewRenderer  
  
- Keeps a DXGI swap chain bound to your panel handle.
- For each frame, CopyResource the desktop texture into the backbuffer and Present.
  
4. Form  
  
- Creates the engine in FormCreate, hardcoded 1920x1080 @ 60fps.
- Starts/stops capture, logs progress/errors on the UI thread via TThread.Queue.
  
The application is kept as simple as possible to show you the workflow and concepts of  
screen capturing to a MP4 file and preview screen.  
  
Project: Media Foundation - MFPack - Samples  
Project location: https://github.com/FactoryXCode/MfPack-FPC  
  
<<<<<<< Updated upstream
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
* Chromecast, Chromecast-enabled television, Google/Android TV device or Chromecast supported speakers.  
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
=======
First release date: 13/08/2025  
Final release date: 18/11/2026  
  
Copyright © FactoryX. All rights reserved.

>>>>>>> Stashed changes

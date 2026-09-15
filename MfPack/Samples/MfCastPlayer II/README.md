## MfCastPlayer II  (Cast V2.1) 
  
Version: 4.0.1 
  
NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version: 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 4.0.0  
  
  
This Delphi XE7-compatible VCL sample demonstrates the public `TMfCast` interface.  

**It demonstrates:**
  
- Asynchronous Chromecast discovery;
- Device selection;
- Casting a compatible local file through the built-in range-capable HTTP server;
- Casting a directly reachable HTTP/HTTPS media URL;
- Real-time DXGI desktop capture with optional WASAPI system-audio loopback;
- Automatic H.264/HEVC hardware-encoder selection;
- Play, pause, stop, seek, volume, mute and disconnect;
- Text subtitle sidecars and embedded Matroska text or VobSub bitmap tracks,
  with sidecars preferred when the same language exists in both sources;
- Exact embedded-track selection and subtitle switching while casting;
  
Please read the white paper document  MfPack/Cast/MfPack_Chromecast_API_White_Paper.txt for full details using the MfPack Cast API  
  
**How to build**  
Build `MfCastPlayer.dproj` for Win32. The project search paths point to  
`MfPack\src`, `MfPack\Cast`, and `MfPack\Cast\Media`. Shared units  
are found through those search paths and do not need to be added to the sample's  
Project Manager.  
  
**Brief workflow**  
The sample enables the optional Media Foundation conversion stack. Compatible  
MP4/H.264/AAC, WebM, MP3, M4A, and AAC sources use direct play. A compatible  
H.264/AAC MKV can be remuxed to fragmented MP4 without re-encoding; an MKV with  
incompatible codecs or selected subtitles that must be burned uses transcoding.  
Generated fragmented MP4 is published piece by piece by the local HTTP server  
while conversion continues.  
The Chromecast device and the PC must be able to reach each other on the local network;  
Windows Firewall may prompt when the server starts.  
  
### Desktop capture

Choose a discovered receiver, select Automatic, H.264, or HEVC, optionally keep
`Capture system audio` enabled, and choose `Cast desktop`. Automatic mode uses
HEVC only when both the receiver model and a local hardware encoder support it;
otherwise it uses hardware H.264. The selected codec does not change while the
stream is active. First-generation H2G2-42 receivers therefore use H.264.

The sample project has no FFmpeg or yt-dlp dependency and does not compile or
install a YouTube-specific resolver. A browser or other application can still
be captured as ordinary desktop pixels and system audio.
  
`Use subtitles` enables the selected item in the subtitle combo. Sidecars are  
listed before embedded tracks and win a duplicate-language match. Embedded  
Matroska `S_TEXT/UTF8` cues and `S_VOBSUB` bitmap tracks are selectable by exact  
track ID. Selected subtitles are composed into the cast video and shown in the  
EVR preview. Embedded PGS tracks are detected but cannot be decoded.  

For a file test in RAD Studio XE7, start the sample with Run/F9, select the  
receiver, open the media file, enable subtitles and choose a track, select an  
audio stream, then call Cast. `test5.mkv` from the Matroska test suite has  
embedded UTF-8 text subtitles. The separately downloaded  
`largeres_vobsub.mkv` has embedded VobSub bitmap tracks; its English track is  
Matroska track 8, with a cue near 40 seconds. That file was verified with video,  
audio, and English subtitles on both an Android TV and the EVR preview.  
  
The shared facade also exposes `GetMediaTracks` and `SelectAudioTrack`.  
The audio-stream combo selects a source stream before casting. Audio selection  
uses stable track IDs and restarts an active transcode at its current source  
position.  
  
## Diagnostics  
  
Cast stages and state transitions are written both to the sample's log memo and,  
through `OutputDebugString`, to Delphi's Event Log while debugging. Diagnostics  
include the inspected content/container, selected media route, local HTTP server,  
control connection, receiver launch, LOAD request, and structured error detail.  
  
An MKV selects the conversion route. The facade starts Media Foundation on the  
creating thread and owns its lifetime because both inspection and conversion use  
the platform. `TMfCast.Create(False)` disables the conversion components while  
retaining discovery, direct playback, and media-track inspection.  
  
The sender is a native implementation of the Google Cast protocol. Google does  
not provide an official Delphi/Windows Cast Sender SDK.  
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 14/09/2026  
  
Copyright © FactoryX. All rights reserved.

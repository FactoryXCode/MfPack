## MfCastPlayer
  
Version: 4.0.0  
  
NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version: 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 3.2.0  
  
  
This Delphi XE7-compatible VCL sample demonstrates the public `TMfCast` facade.  

**It demonstrates:**
  
- Asynchronous Chromecast discovery;
- Device selection;
- Casting a compatible local file through the built-in range-capable HTTP server;
- Casting a directly reachable HTTP/HTTPS media URL;
- Resolving indirect media-page URLs through an application-provided `IMfCastSourceResolver`;
- Play, pause, stop, seek, volume, mute, and disconnect;
- Sidecar and embedded text-subtitle enumeration, with sidecars preferred when
  the same language exists in both sources;
- Exact embedded-track selection and subtitle switching while casting;
  
Please read the white paper document  MfPack/Cast/MfPack_Chromecast_API_White_Paper.txt for full details using the MfPack Cast API  
  
**How to build**  
Build `MfCastPlayer.dproj` for Win32. The project search paths point to  
`MfPack\src`, `MfPack\Cast`, and `MfPack\Cast\Media`. Shared units  
are found through those search paths and do not need to be added to the sample's  
Project Manager.  
  
**Brief workflow**  
The sample enables the optional Media Foundation conversion stack. Compatible  
MP4/H.264/AAC, WebM, MP3, M4A, and AAC sources use direct play. Containers such  
as MKV are converted to fragmented MP4 and published by the local HTTP server.  
The Chromecast device and the PC must be able to reach each other on the local network;  
Windows Firewall may prompt when the server starts.  
  
### YouTube resolver  
  
`MfYouTubeSourceResolver.pas` is a sample-owned implementation of the generic  
`IMfCastSourceResolver` extension point. It recognizes `youtu.be`, YouTube  
`watch`, `shorts`, and `embed` URLs and uses `yt-dlp` to download a combined  
audio/video rendition into the user's temporary `MfCastPlayer` directory. The  
resulting local media file is then passed through the regular MfCast inspection,  
preview, conversion, and HTTP publishing pipeline. The temporary file is removed  
when playback stops, disconnects, fails, or is replaced.  
  
Extract the portable x86 ZIP into the project's persistent `Binaries` directory,  
so that `Binaries\yt-dlp_x86.exe` and its `Binaries\_internal` dependency folder  
exist. The resolver searches upward from nested Win32 Debug/Release output  
directories to find this location. PATH remains available as a fallback.  
MfCastPlayer never downloads or installs this third-party program itself. Paste  
a supported YouTube URL into the existing source field and choose Cast. The  
download completes before receiver playback starts; duration therefore depends  
on the selected video's size and network speed. Use this resolver only where  
downloading is permitted by the service terms and the content owner's rights.  
  
Most current YouTube videos expose separate video-only and audio-only tracks.  
Install/unpack FFmpeg as in `Binaries\ffmpeg.exe` The resolver selects  
an H.264 MP4 video track plus M4A audio and asks yt-dlp/FFmpeg to merge them into  
the temporary MP4 used by MfCast.  
  
`Use subtitles` enables the selected item in the subtitle combo. Sidecars are  
listed before embedded text tracks and win a duplicate-language match. Changing  
the combo while an MKV transcode is active restarts the published stream at the  
current source position with the newly selected track; clearing the checkbox  
does the same without a subtitle compositor. Bitmap subtitles remain deferred.  
  
The shared facade also exposes `GetMediaTracks` and `SelectAudioTrack`. Audio  
selection uses stable track IDs and restarts an active transcode at its current  
source position. Audio controls are intentionally not added here: this sample  
keeps one simple compact playback workflow, while API consumers can build the track UI  
appropriate to their application.  
  
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
Final release date: 22/08/2026  
  
Copyright © FactoryX. All rights reserved.

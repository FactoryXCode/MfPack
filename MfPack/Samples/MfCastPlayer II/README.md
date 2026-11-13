## MfCastPlayer II (MfPack Cast API 4.1.0)
  
Version: 4.0.1 
  
NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version: 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 4.0.0  
Minimum supported Cast API version: 4.1.0
MfCast V2.1  
  
  
This sample demonstrates the public `TMfCast` interface.  

The main project uses Cast API 4.1.0 from `MfPack\Cast`. Its Disconnect button
starts the API's `TMfCastDisconnectWorker`, which calls `TMfCast.Disconnect` and
requests standby for a registered paired Android TV. Completion and diagnostics
are posted back to the VCL window, keeping the interface responsive. The sample
does not implement Android TV protocol, TLS client authentication, or credential
storage itself. `TMfCast` also exposes pairing import, registration checks,
device power-state queries, and explicit standby requests for other clients.

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
Build `MfCastPlayer.dproj` for Win32.
The project search paths point to  `MfPack\src`, `MfPack\Cast`, and `MfPack\Cast\Media`.
Shared units  are found through those search paths and do not need to be added to the sample's Project Manager.
In Project/Options/Delphi Compiler add the following:
'Output Directory:' .\$(Platform)\$(Config)
'Search Path:' // Relative to this sample directory:
                        ..\..\src;
                        ..\..\Cast;
                        ..\..\Cast\Media
'Unit Output Directory:' .\$(Platform)\$(Config)
  
**Brief workflow**  
The sample enables the optional Media Foundation conversion stack. Compatible  
MP4/H.264/AAC, WebM, MP3, M4A, and AAC sources use direct play. A compatible  
H.264/AAC MKV can be remuxed to fragmented MP4 without re-encoding; an MKV with  
incompatible codecs or selected subtitles that must be burned uses transcoding.  
Generated fragmented MP4 is published piece by piece by the local HTTP server  
while conversion continues.  
The Chromecast device and the PC must be able to reach each other on the local network;  
Windows Firewall may prompt when the server starts.  
  
Both Browse buttons use `TLWFileBrowserExDlg` from the local sample units, adapted from the RDJ
file browser with the same layout and standard Windows controls/colors.
The media picker includes audio and video formats; the artwork picker shows an
image preview. Each picker remembers its last folder during the session.
The automatic artwork prompt uses the same browser, and Cancel keeps the current
field value.
Discovered network computers are retained for the current application session
and appear immediately when the browser reopens. Background discovery adds new
computers as they are verified, without waiting for the whole scan to finish.

### Desktop capture

The screen-selection dialog is a VCL form (`dlgCaptureMonitor.pas` and
`dlgCaptureMonitor.dfm`) editable in the Delphi Form Designer. It is created
on demand and freed after closing.

Desktop live streams retain session history in a temporary file so receivers
can reopen the HTTP stream from byte zero after a dropped connection. This keeps
encoded history out of the process heap. Temporary disk usage grows with the
stream duration; the file is deleted when casting stops or the process exits.

Choose a discovered receiver and select `Audio + video` (the default),
`Audio only`, `Video only`, or `Audio + artwork` in the capture-mode combo beside
the codec setting. Click `Cast desktop` to use that selection directly.
Desktop video modes ask which screen to capture. Audio only and Audio + artwork
start without screen selection.

Audio + artwork captures the default Windows playback device and uses the image
in the Artwork field as a static video track. If that field is empty, the artwork
file picker opens first; cancelling it cancels the cast. The picture is fitted to
a black 1280x720 canvas with its aspect ratio preserved and is shown in the local
preview. JPEG, PNG, BMP, and GIF images are supported; GIF uses its first frame.
This mode requires a video-capable receiver and a supported video encoder.
Changing the Artwork field or choosing another image while Audio + artwork is
streaming replaces the live image without restarting capture or changing volume.
The receiver shows the new image after its existing playback buffer. Invalid
images retain the previous picture.

Desktop video casting keeps the local preview enabled and excludes the entire
player window from desktop capture, preventing the preview from capturing itself.
The preview shows the same captured video frames sent to the receiver; receiver
buffering can add a delay. Capture exclusion is removed after stopping,
disconnecting, or a failed cast. File and URL casting keep their normal preview.
Window exclusion uses WDA_EXCLUDEFROMCAPTURE (Windows 10 version 2004 or later);
older Windows versions can show a blank player area in the captured video.
Desktop video with system audio supplies silent AAC input when Windows delivers
no loopback packets, so the MP4 muxer can keep publishing video during silence.
The player waits for encoded MP4 data before sending the receiver LOAD.

Audio only captures the default Windows playback device through WASAPI without
selecting a screen or requiring a video encoder. Video only captures the selected
screen without system audio and requires a video-capable receiver. Audio-only
receivers continue to use system audio when Audio + video is selected.

For modes with video, select Automatic, H.264, or HEVC. Automatic mode uses
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
Final release date: 07/11/2026  
  
Copyright © FactoryX. All rights reserved.

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


### Standard Windows GUI sample

This sample synchronizes CastPlayer functionality and form/dialog layout while
using standard VCL buttons, check boxes, track bars, and Windows system colors.
The local file-browser dialog preserves the source layout and browsing behavior,
including shell icons, network locations, file filters, and artwork preview.
No MPxpButton, MfTrackBar, MfLevelProgressBar, or RDJ custom-control dependency
is required. Shared Cast API units are resolved through relative paths to
`MfPack\Cast`, `MfPack\Cast\Media`, and `MfPack\src`.

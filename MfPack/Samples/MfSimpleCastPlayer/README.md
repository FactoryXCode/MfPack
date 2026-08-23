# MfSimpleCastPlayer
  
Version 3.2.0
  
`MfSimpleCastPlayer` is a compact Delphi VCL example for the public  
`TMfCast` facade. It demonstrates the complete path from discovering a Google  
Cast receiver to inspecting, preparing, serving, controlling, and previewing  
media.  
  
The sample is intentionally small. It is a useful starting point for adding  
Cast support to an existing MfPack application, but it is not intended to be a  
full media-library application.  
  
## Requirements
  
- Windows 10 or later;
- Delphi compiler version 17 through 35;
- MfPack 3.2.0 or later;
- A Chromecast, Google TV, Android TV, speaker, or other compatible Google Cast
  receiver;
- The sender PC and receiver must be reachable on the same local network.
  
Google does not publish an official Delphi or native Windows Cast Sender SDK.  
MfCast therefore implements the required Cast V2 discovery, TLS, protobuf,  
receiver, media, and heartbeat protocols natively.  
  
## What the sample demonstrates
  
- Asynchronous mDNS discovery and refresh;
- Selection of a discovered Cast receiver;
- Local files served through the built-in range-capable HTTP server;
- Direct HTTP and HTTPS media resources;
- Media Foundation inspection and automatic route selection;
- Local video preview;
- Play, pause, stop, seek, volume, mute, and disconnect;
- Sidecar and embedded text-subtitle discovery;
- Runtime subtitle switching;
- Thread-safe delivery of Cast events to a VCL form;
- Structured diagnostic logging and HRESULT reporting;
- Heartbeat monitoring and cleanup when a receiver is switched off;
- Restarting the previous cast from the beginning with **Play** after a
  receiver shutdown or connection failure.
  
## Building
  
Open `MfSimpleCastPlayer.dproj` and build the Win32 target.
  
The project search paths already reference:
  
- `MfPack\src`;
- `MfPack\Cast`;
- `MfPack\Cast\Media`.
  
The shared Cast units do not need to be added individually to the sample's  
Project Manager. This sample does not require `yt-dlp` or FFmpeg. Those programs  
are used only by the separate `MfCastPlayer` sample for resolving webpage URLs  
such as YouTube links.  
  
## Quick start
  
1. Switch on or wake the Cast receiver.
2. Start `MfSimpleCastPlayer`.
3. Select **Discover**.
4. Choose a receiver from the discovered-device list.
5. Browse for a local media file, or enter a direct HTTP/HTTPS media URL.
6. Optionally enable and select a subtitle language.
7. Select **Cast**.
  
Media preparation and receiver connection run in a worker thread because media  
inspection, receiver launch, and network timeouts must not block the VCL UI.  
  
Discovery only proves that a device recently advertised itself through mDNS.  
A sleeping Android TV can sometimes still be discovered even though its Cast  
control endpoint is not ready. Wake the TV and return it to its home screen  
before retrying the cast.  
  
## Controls
  
| Control | Purpose |
| --- | --- |
| **Discover** | Starts mDNS discovery for `_googlecast._tcp.local`. |
| **Refresh** | Runs another discovery response window. |
| **Browse** | Selects a local media file. A direct media URL can be entered manually. |
| **Cast** | Inspects and prepares the source, connects to the receiver, launches the Default Media Receiver, and sends `LOAD`. |
| **Play** | Resumes playback. After a stopped or failed cast, it repeats the previous cast from the beginning. |
| **Pause** | Pauses the active receiver media session. |
| **Stop** | Stops playback and releases active media resources. |
| **Disconnect** | Stops the Cast control session and closes the receiver connection. |
| **Seek** | Seeks to the position selected by the position trackbar. |
| **Volume** | Changes transcoded PCM gain or receiver/media-session volume, depending on the active route. |
| **Mute** | Mutes the active route. It cannot override a system mute imposed by the TV or physical remote. |
| **Enable subtitles** | Enables or disables the selected subtitle track. |
  
## Media sources and routing
  
The sample constructs the facade with:
  
```pascal
FCast := TMfCast.Create(True);
```
  
Passing `True` enables the optional Media Foundation conversion stack, segment  
publisher, remux pipeline, subtitle compositor, and preview sink.  
  
MfCast inspects the source and chooses a route automatically:  
  
| Source | Typical route |
| --- | --- |
| Compatible remote HTTP/HTTPS media | Direct receiver playback when no conversion feature requires decoded frames. |
| Compatible local file | Published by the local HTTP server for the receiver. |
| Matroska with compatible H.264/AAC | Can be remuxed to fragmented MP4 without re-encoding. |
| Incompatible container or codec | Decoded and transcoded to fragmented H.264/AAC MP4. |
| Video requiring burned subtitles | Transcoded with subtitle composition. |
  
### Preview and direct playback
  
`MfSimpleCastPlayer` attaches its black preview panel with  
`SetPreviewWindow`. The facade selects the preview implementation that matches  
the chosen media route:  
  
- Direct and remux routes use an independent Media Foundation Media Session,
  the EVR, and the local audio renderer.
- Transcode routes use `MfCastWindowPreview`, which receives the decoded video
  and audio samples already produced by the conversion pipeline.
  
Attaching a preview no longer forces compatible media through transcoding.  
Call `UpdatePreviewWindow` from the preview control's resize event so the EVR  
updates its destination rectangle. Use `SetPreviewWindow(0)` to detach and stop  
the local preview.  
  
### Supported source families
  
The media inspector recognizes common MP4/M4V/M4A, WebM, MP3, AAC, FLAC, WAV,  
Ogg/Opus, Matroska, MOV, AVI, MPEG-TS, and M2TS source families. Actual direct  
play support depends on the codecs in the file and receiver capabilities.  
Unsupported combinations can use the conversion route when Media Foundation  
has the required decoder.  
  
A URL must point to the media resource itself. A normal webpage URL, including  
a YouTube watch page, is not a direct media URL. URL extraction is deliberately  
kept outside the Cast API through the optional `IMfCastSourceResolver`  
extension demonstrated by `MfCastPlayer`.  
  
## Local HTTP server and firewall
  
A Cast receiver cannot read a Windows file path. MfCast starts a small local  
HTTP server and advertises an address that is reachable from the selected  
receiver. The server supports byte ranges for local files and publishes live  
fragmented MP4 output for conversion routes.  
  
Windows Firewall may ask for permission when the server starts. Allow access on  
the appropriate private network. Discovery and playback can fail independently:  
  
- mDNS discovery uses multicast UDP;
- Cast control uses the receiver's TCP port, normally 8009;
- local files and generated media require the receiver to connect back to the
  sender's HTTP listening port.
  
Guest Wi-Fi, client isolation, VPN routing, and an incorrectly classified public  
network can allow discovery while blocking control or media delivery.  
  
## Subtitles
  
When a local source is selected, the sample scans for sidecar and embedded text  
tracks.  
  
- Sidecar candidates are inserted first;
- Embedded text tracks are identified by a stable track ID and stream index;
- Language tags are normalized for display and duplicate matching;
- A sidecar wins when sidecar and embedded tracks use the same language;
- Changing the subtitle selection during conversion restarts the generated
  stream at approximately the current source position;
- Clearing **Enable subtitles** removes the compositor in the same way.
  
Text subtitles such as SRT, WebVTT, and supported embedded UTF-8 text can be  
used. Bitmap subtitle formats such as PGS and VobSub are not currently rendered.  
  
The facade also exposes `GetMediaTracks`, `SelectAudioTrack`,  
`SelectSubtitle`, and `DisableSubtitles`. The simple sample intentionally omits  
a separate audio-track selector so applications can provide a UI appropriate  
to their own media library.  
  
## Preview and audio synchronization
  
For conversion routes, the preview is produced by the same transcode pipeline  
that supplies the receiver. Receiver buffering means the TV can still appear a  
few seconds behind the local preview. The amount varies with receiver type,  
wireless conditions, and buffer state.  
  
Direct audio formats use receiver volume control. Transcoded audio uses a PCM  
gain/mute stage immediately before AAC encoding, so an audible volume change can  
be delayed by already-buffered media.  
  
## Receiver monitoring and recovery
  
After the receiver application is launched, MfCast maintains a persistent  
receive and heartbeat worker. It responds to receiver heartbeat requests, sends  
periodic PING messages, and monitors the control connection.  
  
When the TV is switched off or the receiver stops responding, MfCast:  
  
1. Detects the closed connection or heartbeat timeout;
2. Stops the transcode or remux worker;
3. Releases published media and temporary state;
4. Changes the facade state to `Stopped` or `Error`;
5. Retains a snapshot of the last public cast request.
  
Selecting **Play** in `Stopped` or `Error` state reconnects and starts that  
request again from the beginning. If the receiver is still sleeping or offline,  
the reconnect attempt will fail normally and report an HRESULT.  
  
## Threading and application integration
  
Cast callbacks originate on subsystem worker threads. VCL controls must never  
be changed directly from those callbacks. The sample posts private  
`WM_MFCAST_*` messages to the form and performs the UI work on the main thread.  
  
The initial `Cast` operation also runs in `TMfCastFileWorker`. Applications  
should follow the same pattern or use their own task/thread abstraction.  
  
A minimal integration has this shape:
  
```pascal
FCast := TMfCast.Create(True);
FCast.OnDeviceAdded := DeviceChanged;
FCast.OnStateChanged := CastStateChanged;
FCast.OnMediaStatus := CastMediaStatus;
FCast.OnError := CastError;
FCast.OnLog := CastLog;

pnlPreview.HandleNeeded;
FCast.SetPreviewWindow(pnlPreview.Handle);
FCast.Discover;
```
  
The sample also calls `FCast.UpdatePreviewWindow` from `pnlPreview.OnResize`.
  
Before destroying the form, detach the preview window and event handlers, wait   
for application-owned worker threads, and then free `TMfCast`. The sample's  
`FormDestroy` method demonstrates the required order.  
  
## Diagnostics
  
The log memo and Delphi Event Log show the same structured pipeline messages.  
Useful entries include:  
  
- Discovery interface, query, and accepted mDNS replies;
- Controller state transitions;
- Media Foundation inspection results;
- Selected direct, remux, or transcode route;
- HTTP server address and port;
- Receiver connection and application launch;
- Receiver and media volume status;
- `LOAD` acceptance and media status;
- Heartbeat timeout or connection closure;
- Stage-specific HRESULT and error detail.
  
The last successful log entry usually identifies the stage at which a failure  
occurred.  
  
## Troubleshooting
  
### The device is discovered, but Cast does nothing
  
Wake the receiver fully and verify that TCP port 8009 is reachable. Discovery  
can succeed from a cached or sleeping receiver advertisement even when the  
control endpoint is unavailable.  
  
### The receiver launches but media does not start
  
For a local file, check Windows Firewall and confirm the TV can connect back to  
the logged HTTP server address and port. Disable VPN routing or Wi-Fi client  
isolation temporarily when diagnosing the connection.  
  
### Playback repeatedly changes between Playing and Buffering  
  
This usually indicates insufficient or unstable wireless throughput. Try a  
lower-bitrate source, improve signal strength, reduce competing traffic, or use  
wired Ethernet where available.  
  
### There is a preview but no receiver picture
  
The local conversion pipeline can run even when the TV cannot reach the  
published HTTP stream. Check the receiver media status and local HTTP server  
address in the log.  
  
### There is no preview
  
Audio-only direct playback has no video preview. For video, confirm that a  
preview window was attached and inspect the selected media route and any Media  
Foundation decoder error.  
  
### Volume or mute appears ineffective
  
Check whether the TV itself is muted with its physical remote. MfCast controls  
the Cast receiver or active media stream; it cannot always override the TV's  
system audio policy. Generated streams can also contain several seconds of  
already-buffered audio.  
  
### Cast fails with an HRESULT
  
Use the stage name immediately preceding the HRESULT. The controller reports  
different stages for resolution, inspection, preparation, HTTP publication,  
connection, receiver launch, and media status, making the hexadecimal value  
considerably easier to interpret.  
  
## Related documentation
  
- `MfPack\Cast\README.md` — Cast facade and architecture overview;  
- `MfPack\Cast\MfPack_Chromecast_API_White_Paper.txt` — detailed API design and   
  protocol notes;  
- `MfPack\Cast\Media\README.md` — media conversion support;  
- `MfPack\Samples\MfCastPlayer` — extended sample with application-provided  
  webpage/YouTube resolution.
  
Project: Media Foundation — MfPack Samples  
Project location: <https://github.com/FactoryXCode/MfPack>  
SourceForge: <https://sourceforge.net/projects/MfPack>  
  
First release date: 02/08/2026  
Copyright © FactoryX. All rights reserved.

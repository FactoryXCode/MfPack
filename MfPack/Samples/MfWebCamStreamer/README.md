# MfWebCamStreamer

Version: 4.0.0
  
NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 4.0.0  
   
Description:  
`MfWebCamStreamer` is an FxServe-based follow-up to `MfSimpleWebCamStreamer`.
It captures webcam video and microphone audio with Media Foundation, encodes
one H.264/AAC fragmented-MP4 stream, and publishes that stream into an FxServe
web root for LAN or public HTTPS viewing.
  
The browser player includes:
  
- a live elapsed timer;
- PNG snapshots of the current camera frame;
- client-side recording through `MediaRecorder` (downloaded as WebM);
- live viewer count with privacy-masked IP addresses;
- sound, playback, reconnect, and live-edge controls.
  
It is also an installable Progressive Web App. The PWA caches only its HTML,
manifest, service worker, and icons. `live.json`, `viewers.json`, `init.mp4`, and
all `.m4s` fragments always use the network and are never served from the
offline cache. Each browser tab is counted separately and expires from the
viewer list about 15 seconds after its last stream request.
  
## Signal path
  
```text
Webcam + microphone
        |
        +------> EVR camera preview
        |        + optional local microphone monitoring
        |
        v
Media Foundation H.264/AAC fragmented MP4
        |
        v
MfWebCamStreamer atomic publisher
        |
        v
FxServe www\WebCam
  init.mp4
  live.json
  patched_frag_*.m4s
  webcam_stream.html + PWA assets
        |
        v
FxServe HTTP/HTTPS -> browser or installed PWA
```
  
The sample reuses `SimpleAvCapture.pas` and `SimpleFmp4ByteStream.pas` from
`MfSimpleWebCamStreamer`. No second encoder is created. Each fragment and the
manifest are written through a temporary file and atomically renamed, so
FxServe never announces a partly written media file.
  
## Quick start

1. Build and start FxServe with its normal `www` web root.
2. Build and run `MfWebCamStreamer`.
3. Select a camera and microphone.
4. Set **FxServe folder** to a dedicated folder below the FxServe web root,
   normally `C:\FxServe\www\WebCam` or `\\server\FxServe\www\WebCam`.
5. Set **Browser URL** to the matching public URL, for example
   `http://127.0.0.1:8080/WebCam/webcam_stream.html` for LAN testing or
   `https://camera.example.com/WebCam/webcam_stream.html` for Internet use.
   Alternatively, click the adjacent **Select folder...** button and select
   the remote FxServe `www\WebCam` folder. The application resolves the server
   IPv4 address and reads `www\fxserve-config.json` for the LAN protocol and
   port. Because `www` is FxServe's configured web root, for example,
   `\\server\FxServe\www\WebCam` becomes
   `http://server-ip:8080/WebCam/webcam_stream.html`.
6. Click **Start streaming**, then **Open browser**.
7. In the page, click **Start live video**.

The **FxServe folder** and **Browser URL** values are saved automatically when
the application closes and restored the next time it starts.
  
The application shows the camera locally in `pnlPreview` through the Enhanced
Video Renderer (EVR). **Monitor microphone locally** in `pnlTop` is off by
default to prevent acoustic feedback. Select it before starting when local
audio monitoring is required. This checkbox affects only the PC speakers; the
FxServe H.264/AAC stream always retains its microphone audio.
  
The publisher creates the dedicated WebCam folder when needed. Starting a new
session removes only files owned by this sample inside that selected folder:
`live.json`, `init.mp4`, and `patched_frag_*.m4s`.

## Saved settings

`MfWebCamStreamer.ini` is stored beside `MfWebCamStreamer.exe`. It contains
only the selected FxServe publication folder and browser URL:

```ini
[FxServe]
Folder=C:\FxServe\www\WebCam
BrowserUrl=http://127.0.0.1:8080/WebCam/webcam_stream.html
```

If the INI file is absent, the application uses the local defaults shown
above. Selecting a remote folder and URL replaces those values when the
application closes. Camera and microphone selections are not persisted.
  
## Browser recording and snapshots
  
**Take snapshot** renders the current video frame to a canvas and downloads a
PNG file. **Start recording** uses the browser's `captureStream` and
`MediaRecorder` APIs and downloads a WebM file when stopped. These features
work in current Chromium-based browsers and Firefox; availability and codecs
depend on the browser. They do not write files on the camera PC.
  
## FxServe and Internet access

FxServe serves the published files and provides the HTTPS endpoint. For public
access, configure FxServe WAN mode, DNS, certificate management, router port
forwarding, and firewall rules as documented in the FxServe README. Do not
expose the private port 8080 directly to the Internet.

This sample does not implement user authentication. Treat the public URL as a
camera feed URL and add access control before using it for private content.

## Install on a phone

PWA installation requires the public FxServe **HTTPS** URL. Ordinary LAN HTTP
streaming continues to work but is not considered a secure context by mobile
browsers and therefore cannot install a service worker.

- Android/Chrome: Open the HTTPS camera URL and select **Install app**.
- iPhone/iPad/Safari: Open the HTTPS camera URL, select **Share**, and then
  **Add to Home Screen**.

The installed application opens in standalone mode without normal browser
controls. Publishing a newer `webcam_stream.html` or service worker through
MfWebCamStreamer updates the installed PWA without an app-store release.

The browser stream is intentionally a little behind the local EVR preview.
The default one-second fragmented-MP4 interval normally produces about two
seconds of end-to-end latency while keeping playback stable.

## Build

Open `MfWebCamStreamer.dproj` in Delphi and build Win32.

During development the application finds the player files in the sample's
`www` folder. For a standalone deployment, place this `www` folder beside
`MfWebCamStreamer.exe`. It must contain `webcam_stream.html`,
`webcam-manifest.json`, `sw.js`, `webcam-icon-192.png`, and
`webcam-icon-512.png`. The application copies these assets into the selected
FxServe `www\WebCam` publication folder when streaming starts.

Project: Media Foundation - MFPack - Samples  
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack
First release date: 24/01/2020  
Last updated: 18/09/2026  
  
Copyright © FactoryX. All rights reserved.

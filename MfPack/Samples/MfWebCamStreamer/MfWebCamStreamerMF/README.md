# MfWebCamStreamerMF

Version: 4.1.0
  
NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 4.1.0  
   
Description:  
`MfWebCamStreamerMF` is an FxServe-based follow-up to `MfSimpleWebCamStreamer`.  
It captures webcam video and microphone audio with Media Foundation, 
encodes one H.264/AAC fragmented-MP4 stream, and publishes that stream into an FxServe web root for LAN or public HTTPS viewing.  
  
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
  
## Media Foundation camera controls  
  
Start streaming, then click **Camera controls...**.  
The modeless dialog exposes supported standard properties (including exposure, focus, zoom, brightness and
white balance), Auto/Manual modes, extended controls, live change notifications,
and inspection of per-user Windows camera defaults.  
Default writes require registered camera companion-app authorization (see the permission section below).
Unsupported controls are identified in the dialog.  
HDR is a pre-start setting and is disabled while streaming in this version.  
  
All camera-control interface access and methods are isolated in **CamControl.pas**.  
It uses IKsControl and IMFExtendedCameraController obtained from the active
Media Foundation camera source, plus the Media Foundation configuration/defaults
and notification interfaces.  
No DirectShow IAMCameraControl or IAMVideoProcAmp interfaces are used.  
  
Controls attach to the exact IMFMediaSource used by the Source Reader.  
Its same video samples feed the EVR preview and H.264 writer, so a live source setting affects both the preview and published stream.  
Remote viewers see the change after the normal stream buffering delay.  
Closing the controls dialog keeps streaming; stopping capture unbinds the dialog and releases all control interfaces before shutting down the source.  
Live settings are not automatically saved as Windows camera defaults.  
The explicit defaults buttons demonstrate the native API, which can deny writes from this unpackaged desktop sample.  
  
The original MfWebCamStreamer sample is retained separately.  
This version builds for Delphi/VCL Win32 and Win64.  
It also enables the advanced Source Reader video processor so cameras with native formats other than NV12 can feed both outputs.

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
  
The sample reuses `SimpleAvCapture.pas` and `SimpleFmp4ByteStream.pas` from `MfSimpleWebCamStreamer`.  
No second encoder is created.  
Each fragment and the manifest are written through a temporary file and atomically renamed, so FxServe never announces a partly written media file.  
  
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
  
The **FxServe folder** and **Browser URL** values are saved automatically when the application closes and restored the next time it starts.
  
The application shows the camera locally in `pnlPreview` through the Enhanced Video Renderer (EVR).  
**Monitor microphone locally** in `pnlTop` is off by default to prevent acoustic feedback.  
Select it before starting when local audio monitoring is required.  
This checkbox affects only the PC speakers; the FxServe H.264/AAC stream always retains its microphone audio.
  
The publisher creates the dedicated WebCam folder when needed.  
Starting a new session removes only files owned by this sample inside that selected folder: `live.json`, `init.mp4`, and `patched_frag_*.m4s`.  
  
## Saved settings  
  
`MfWebCamStreamerMF.ini` is stored beside `MfWebCamStreamerMF.exe`.  
It contains only the selected FxServe publication folder and browser URL:  
  
```ini
[FxServe]
Folder=C:\FxServe\www\WebCam
BrowserUrl=http://127.0.0.1:8080/WebCam/webcam_stream.html
```
  
If the executable's INI file is absent, a development build first reads MfWebCamStreamerMF.ini in the sample root.  
This copy is configured for example: "\\PCHP001\FxServe\www\WebCam" and its current LAN browser address.  
If neither file exists, the application uses the local defaults shown above.  
Selecting a remote folder and URL replaces those values when the application closes.  
Camera and microphone selections are not persisted.  
  
## Browser recording and snapshots  
  
**Take snapshot** renders the current video frame to a canvas and downloads a PNG file.  
**Start recording** uses the browser's `captureStream` and `MediaRecorder` APIs and downloads a WebM file when stopped.  
These features work in current Chromium-based browsers and Firefox; availability and codecs depend on the browser.  
They do not write files on the camera PC.  
  
## FxServe and Internet access  
  
FxServe serves the published files and provides the HTTPS endpoint.  
For public access, configure FxServe WAN mode, DNS, certificate management, router port forwarding, and firewall rules as documented in the FxServe README.  
Do not expose the private port 8080 directly to the Internet.  
  
This sample does not implement user authentication.  
Treat the public URL as a camera feed URL and add access control before using it for private content.  
  
## Install on a phone  
  
PWA installation requires the public FxServe **HTTPS** URL.  
Ordinary LAN HTTP streaming continues to work but is not considered a secure context by mobile browsers and therefore cannot install a service worker.

- Android/Chrome: Open the HTTPS camera URL and select **Install app**.
- iPhone/iPad/Safari: Open the HTTPS camera URL, select **Share**, and then **Add to Home Screen**.
  
The installed application opens in standalone mode without normal browser controls.  
Publishing a newer `webcam_stream.html` or service worker through MfWebCamStreamer updates the installed PWA without an app-store release.  
  
The browser stream is intentionally a little behind the local EVR preview.  
The default one-second fragmented-MP4 interval normally produces about two seconds of end-to-end latency while keeping playback stable.  
  
## The www folder    
During development the application finds the player files in the sample's `www` folder.  
For a standalone deployment, place this `www` folder beside `MfWebCamStreamerMF.exe`.  
It must contain `webcam_stream.html`, `webcam-manifest.json`, `sw.js`, `webcam-icon-192.png`, and `webcam-icon-512.png`.  
The application copies these assets into the selected FxServe `www\WebCam` publication folder when streaming starts.  

## INI settings or Windows defaults

On **Saved camera settings**, select a **Storage** option:

- **INI file (this sample)** saves standard live control values and Auto/Manual
  modes in a per-user file. Select a live control, then use **Save selected live
  control to INI** or **Remove selected INI setting**. **Read INI settings** reloads
  the stored list; **Apply saved INI** applies the saved entries to the current
  camera. Opening the dialog or reading the list does not write settings.
- **Windows camera defaults (companion app)** retains the original native
  save/read/remove implementation and its comments. It still requires Windows
  companion-app authorization for writes, as described below.

The storage choice is remembered in the INI file.  
INI is the initial choice.
When INI storage is selected, saved values are restored when the sample opens a camera, before preview/streaming starts.  
Selecting Windows storage prevents INI restoration on the next camera open.  
Changing storage does not immediately change the live camera or copy settings between the two stores.  
  
The file is **%LOCALAPPDATA%\MfPack\<executable name>\CameraSettings.ini**; its full path is shown in the dialog.  
Sample 3 uses the executable name `MfSimpleCapture`; the webcam sample uses `MfWebCamStreamerMF`.  
Win32/Win64 and Debug/Release builds of the same executable share this file.  
Sample 3 also share it when run by the same Windows user.  
  
Each camera is identified by its full symbolic link, encoded as an INI section name.  
Each entry uses the property-set GUID and control ID as its key and stores `value,flags` (1 = Auto, 2 = Manual).  
Changing one setting preserves other controls, cameras and the storage choice.  
File updates replace the complete file atomically.  
File errors are reported as failures.  
  
Restore uses the same support, mode, range and step validation as a live edit.  
Missing entries are left alone; invalid, unsupported or rejected entries report an error while other valid entries are still attempted.  
A restore can therefore partially succeed.  
Failure to restore does not prevent preview from starting.  
Only standard controls are stored; extended MF flag settings retain their original live-only behavior.  
INI settings apply when this sample opens the camera; they do not register system-wide Windows defaults.  
  
The INI tests use isolated temporary files and cover signed values, modes, camera isolation, 
preserving other entries, removal, malformed data, failed writes and remembering the storage choice.  
  
## Permission to save Windows camera defaults  
  
The native Save/Remove actions call `IMFCameraConfigurationManager.SaveDefaults`.  
Windows documents default writes for packaged camera companion apps associated with the camera by its manufacturer.  
These desktop samples have no package identity or companion-app registration.  
Reading defaults or applying a live control successfully does not establish permission to save Windows defaults;
`SaveDefaults` can return `E_ACCESSDENIED` (`$80070005`).  
The dialog now explains this result on the Saved camera defaults tab.  
It does not report a failed write as saved.  
  
For persistent Windows defaults, use **Settings > Bluetooth & devices > Cameras > your camera**, or the camera manufacturer's registered companion app.  
The native save/remove code remains as an API example for an appropriately registered app.  
Packaging and camera association are separate deployment requirements; the sample's ordinary EXE manifest does not provide them.  
  
Reference: [Microsoft camera companion app requirements](https://learn.microsoft.com/en-us/windows-hardware/drivers/stream/camera-companion-apps).  

  
Project: Media Foundation - MFPack - Samples  
  
Project location: https://github.com/FactoryXCode/MfPack  
  
https://sourceforge.net/projects/MFPack
  
First release date: 24/01/2026  
Last updated: 26/11/2026  
  
Copyright © FactoryX. All rights reserved.


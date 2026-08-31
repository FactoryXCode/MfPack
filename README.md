# About MfPack
  
 Delphi translations for Microsoft Media Foundation and related API's.
  
# MfPack covers the Delphi translations of:
  
- Core Audio API's:
  * Windows Audio Session API (WASAPI) 
  * DeviceTopology API
  * EndpointVolume API
  * Multimedia Device (MMDevice) API
- Microsoft Media Foundation API (successor of DirectShow).
- XAudio2 API (XBox game development, successor of DirectSound).
- Microsoft DirectX API's (D2D1, D3D9, D3D11, D3D12, DirectComposition, DXGI,  
  DirectWrite and DXVA) Note that D3D11 and above are part of Media Foundation.
- Windows Imaging Component (WIC) API.
- MPEG2 API.
- The complete Windows Media (WinMM) API.
  
# MfPack API's and Samples
  
- MfPack Cast V2 API.
- Media Foundation, WASAPI and XAudio2 samples.
  
  
# <u>Latest release:</u> 
  
# MfPack  Version 4.0.0  
Delphi XE2 (recommended Delphi XE7) up to and including Delphi 13.1  
SDK version: 10.0.28000.2705 (Windows 11).  
  
  
**Important note:**  
The latest Windows 11 versions do not support Dolby AC-3 and H.265 (hvec) codecs.  
Those you can buy from Microsoft Store or download elsewhere.
  
  
# Samples
  
**MfVideoThumbNails sample**
  
*This sample app needs the D2D1 Api. If your Delphi version doesn't have the  
latest D2D1 Api (May 2019 update), use the D2D1 MfPack version.*
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/VideoThumbNailsSample.jpg)
  
---
  
**MediaEngine Player 2 & IMFTimedText sample**
  
*MediaEngine player (IMFMediaEngine(Ex)) with support for subtitles (closed captions) using  
the IMFTimedTextNotify interface.*
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MediaEnginePlayer2.jfif)
  
---
  
**MfTranscode sample**
  
Demonstrates using the transcode API to transcode a source file (audio or video) to  
a different format (audio or video) supported by Media Foundation.  
  
Supported, but not limited, formats in this sample are:
  
- Audio
   * Waveform Audio File Format (wav)
   * MPEG Audio Layer III (mp3)
   * Free Lossless Audio Codec (flac)
   * MPEG-4 Audio (m4a)
   * Windows Media Audio (wma)
  
- Video
   * Audio Video Interleave (avi)
   * MPEG-4 Video with AAC Audio (mp4)
   * MPEG-4 Video with Dolby AC-3 Audio (mp4) 
   * Windows Media Video (wmv)
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfTranscode%20Sample%202.png)
  
---
  
**MfSimpleCapture sample**
  
This example shows, how you have to implement capturing within a session.  
Note: The old MS sample used the deprecated IMFplayer interface.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfSimpleCapture.png)
  
---
  
**MfPeakMeter, MfPeakMeterEx MfPeakMeterMmcs component samples**
  
An example about how to create a MfPeakMeter control. This sample is part of the  
Samples/MfComponents package.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfPeakmeter.jfif)
  
An example about how to create a MfAudioVisualizer control - A visual component that  
uses WASAPI loopback, Peak/RMS, optional spectrum bars or VU.  
This sample is part of the Samples/MfComponents package.  

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfAudioVisualizer.png)
  
---
  
**AudioClip, AudioClipEx and AudioClipEx2 samples**
  
Demonstrates using the IMFSourceReader API to extract uncompressed media data from  
a media file. This sample application reads audio data from a media file and writes  
the uncompressed audio to a WAVE file.  
The AudioClipEx sample demonstrates using the IMFSourceReader and 
IMFSourceReaderCallback API to extract uncompressed audiodata from a media file.  
  
The AudioClipEx2 sample demonstrates using the IMFSourceReader,  
IMFSourceReaderCallback and IMFSinkWriter API to extract uncompressed audiodata from a  
media file and write the audio to a wav. It also uses threads for better responsiveness.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/AudioClipExSample.jpg)
  
---

**MfPlayer**
  
Player samples, based on the original Microsoft CPlayer sample.  
There are 4 samples in 4 project degrees.  
  
- *MfPlayer I : The basic player sample.*
- *MfPlayer II : The extended version I sample.*
- *MfPlayer X : This is an example that shows how to use the IMFTimer, language tags,*  
  *subtitles (SubRip, MicroDvd and WebVTT), RegEx (Regular Expressions) and how to get media properties.*
- *MfPlayer X2 : This example is based on the MfPlayer X project, but replaces the floating*
  *subtitle overlay with a real video-frame subtitle pipeline. Subtitle pixels are*
  *now composed into the decoded video for local playback, MP4 export and Chromecast/Android TV/Google TV streaming.*
  *The Player also demonstrates how Media Foundation playback, timed text,*
  *MFT processing, Source Reader/Sink Writer transcoding, IMFByteStream,*
  *mDNS, TLS, MfPack Cast V2 messages, and a local HTTP server can work together*
  *inside a Delphi application, without using 3th party components.*
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfMediaPlayer_X.jfif)
  
---
  
**Ducking Media Player**
  
This sample implements a simple media player that responds to the "ducking"  
feature in Windows 7 and later. It also implements a volume control which tracks  
to the volume control in the volume mixer.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/DuckingMediaPlayerSample.jpg)
  
---
  
**Ducking Capture Sample**
  
This sample implements a simple "Chat" that demonstrates the Windows audio  
ducking feature. In capture mode, audio from the default communications  
capture endpoint is routed to the default playback endpoint.  
Select Capture in the application to hear the routed input.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ChatDemo.jfif)
  
---
  
**MfSimpleWebCamStreamer**

This sample implements a compact VCL example for capturing a webcam
and microphone with Microsoft Media Foundation, encoding the result as H.264
and AAC, and publishing the live stream directly to a web browser through
fragmented MP4 and Media Source Extensions (MSE).
The sample is intentionally small. It demonstrates the complete path from
Media Foundation capture to a browser-playable live stream without requiring
another external media server.

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfSimpleWebCamStreamer.png)
  
---
  
**MfSimpleCastPlayer Sample**
  
**This sample demonstrates:**
  
- Asynchronous Chromecast discovery;
- Device selection;
- Casting a compatible local file through the built-in range-capable HTTP server;
- Casting a directly reachable HTTP/HTTPS media URL;
- Play, pause, stop, seek, volume, mute, and disconnect;
- Sidecar (Subtitle files) and embedded text-subtitle enumeration, with sidecars preferred when
  the same language exists in both sources;
- Exact embedded-track selection and subtitle switching while casting;
  
The sample enables the optional Media Foundation conversion stack. Compatible  
MP4/H.264/AAC, WebM, MP3, M4A, and AAC sources use direct play. Containers such  
as MKV are converted to fragmented MP4 and published by the local HTTP server.  
Please read the white paper document MfPack/Cast/MfPack_Chromecast_API_White_Paper.txt for  
full details using the MfPack Cast API.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfSimpleCastPlayer.png)
  
---
  
**MfCastPlayer Sample**
  
This it the extended version of MfSimpleCastPlayer.  
  
**This sample demonstrates the following additions of MfSimpleCastPlayer:**
  
- Casting a YouTube URL through an application-provided
  `IMfCastSourceResolver` implementation;
- A fast YouTube mode that resolves a combined H.264/AAC stream for direct
  playback;
- A best-quality YouTube mode that downloads separate video and audio streams
  and merges them into an MP4 file;
- Local video and audio preview through the `TMfCast` facade;
- Audio-only playback with optional JPG, PNG, BMP, or GIF artwork;
- Heartbeat monitoring and automatic cleanup when the receiver is switched
  off or stops responding.
  
The sample enables the optional Media Foundation conversion stack.  
Compatible MP4/H.264/AAC, WebM, MP3, M4A, AAC, FLAC, WAV, and Ogg sources can use direct  
playback. Other containers and codecs, including AVI and MKV when required,  
are remuxed or converted to fragmented MP4 and published by the local HTTP  
server.  
  
Please read `MfPack/Cast/MfPack_Chromecast_API_White_Paper.txt` for full details  
about using the MfPack Cast API.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfCastPlayer.png)
  
---
  
**MFFrameCapture sample**
  
Demonstrates how to capture an image (synchronous or A-synchronous) using the IMFSourceReader.
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfFrameCapture.png)

---
  
**CameraFrameCapture sample**
  
Demonstrates how to capture a still image or so called snapshot (A-synchronous) from a    
capture device such as a webcam or camera using the IMFSourceReader.
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/CameraFrameCaptureSample.png)
  
---
  
**CaptureEngineVideoCapture sample**
  
Demonstrates how to capture a snapshot or a recording (A-synchronous) from a capture device,  
such as a webcam using the IMFCaptureEngine and IMFCapturePreviewSink.  
This sample also demonstrates how to use the MfMediaTypeDebug API.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MFCaptureEngineVideoCapture.jpg)
  
---
  
**SinkWriterToEncodeVideo Sample**
  
Example 1 creates a simple green bitmap (640x480) and store it to a file with a length of 20 seconds.  
Example 2 demonstrates how to use the SinkWriter to create a video from one or more bitmap files.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/SinkWriterSample.png)
  
*Example 3*  
Demonstrates how to use the SinkWriter to create a video from one or more image files including audio.
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ImageToVideo_3.jpg)
  
---
  
**LoopBackCapture Sample 1**
  
This sample shows, how to capture sound from your soundcard using WASAPI and save this capture  
with the quality that is supported by your soundcard.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/LoopBackCapture.png)
  
---
  
**LoopBackCapture Sample 2**
  
This sample demonstrates how to capture system audio either from a  
specific process tree or for all process except  
a process tree and the use of ActivateAudioInterfaceAsync Win32 API  
with a new initialization structure.  
The new data structure makes it possible to restrict captured  
audio data to that rendered by a specific process and any of its child processes.  
Windows 10 has always supported capturing all audio that is played on  
an audio endpoint (referred to as "system" loopback capture),  
which captures all audio from all apps that are playing sounds on  
the chosen audio endpoint.  
  
With the new structure, only audio from the specified process,  
and its children, will be captured. Audio rendered by other processes will not be captured.  
A flag is also provided to reverse the behavior, capturing all system  
audio *except* those from the the specified process (and its children).  
Furthermore, the capture is not tied to a specific audio endpoint,  
eliminating the need to create a separate IAudioClient to capture from each physical  
audio endpoint.  
  
If the processes whose audio will be captured does not have any audio rendering streams,  
then the capturing process receives silence.  
  
It also demonstrates how to get a process by using the tlhelp32 API,  
to list a snapshot of running processes and be able to pick one.  
The application is provided with a dialog to select a running process from  
the process tree you want to pick and has a button to get the current PID of your application.  
  
The application is using MMCSS and runs the rendering part in a separate thread.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/LoopbackCapture2.png)
  
---
  
**Threaded Loopback Capture Sample**
  
Demonstrates how to capture sound from a rendering or capture device (one of the soundcard's endpoints)  
using WASAPI in combination with mmio to write wav-files.  
This sample lets you to choose between different latency's and  
buffersize for better sound to eliminate buffer related gliches.  
  
*It has some more advanced features like:*  
   - *The rendering part is running in a separate thread to eliminate glitches.*
   - *Able to use MMCSS (Multimedia Class Scheduler service).
   - *Stream switch detection.*
   - *Possibility to write wav data in native format 44.1 kHz/ 16 bit PCM or* 
     *the soundcard's audio format (including Uncompressed IEEE floating-point audio).*
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ThreadedLoopbackCapture.png)
  
---
  
**XAudio2Player Basic Player**
  
XAudio2 is the long-awaited replacement for DirectSound.  
It addresses several outstanding issues and feature requests, like low latency etc.  
  
This sample demonstrates how to use XAudio2 to render different file formats like WAV, FLAC, MP3 etc.  
The sample uses the IMFSourceReader to decode the format suitable for playing in XAudio2.  
It shows you the basics of using XAudio2 without formatting the mediatypes yourself.  
The sample uses the MfPeakMeter component. This requires that you install the MfComponents.  
In your projectsettings you have to add ..MfPack\Samples\MfComponents in the project options searchpath.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio_Basic_Sample.png)
  
---
  
**XAudio2Player Sample 2**
  
This sample shows you how to implement the IXAudio2VoiceCallback.  
The sample uses the MfPeakMeter component. This requires that you install the MfComponents.  
In your projectsettings you must add ..MfPack\Samples\MfComponents in the project options searchpath.  
It also has a pitch control.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio2_Sample2.png)
  
---
  
**XAudio2Player Sample 3**
  
This sample demonstrates how to implement XAudio2 effects and  
how to go forward or backward during playing with a progressbar and keeping up progress.  
  
It shows you how to implement events and methods instead of using messages from the XaudioEngine.  
The sample uses the MfPeakMeter component. This requires that you install the MfComponents.  
In your projectsettings you have to add ..MfPack\Samples\MfComponents in the project options searchpath.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio2_Sample3.png)
  
---
  
**XAudio2Player Sample 4**
  
This sample demonstrates a Media Foundation–based streaming audio player built on top of XAudio2.  
  
Audio files (WAV, MP3, FLAC, AAC, etc.) are decoded on-the-fly using the  
Media Foundation Source Reader and rendered through XAudio2 using a multi-buffer* *streaming model.  
Playback control (Play, Pause, Stop, Replay, Seek) is fully thread-safe and  
handled by a dedicated worker thread, avoiding UI blocking and audio glitches.  
  
*The sample also shows how to:*
  
⦁	*Apply XAudio2 effects (reverb, mastering limiter).*
⦁	*Perform sample-accurate seeking during playback.*
⦁	*Update a progress bar and timing UI based on streamed audio.*
⦁	*Implement thread-safe callbacks and events instead of message-based signaling.*
⦁	*Track real-time audio levels using the MfPeakMeter component.*
  
Unlike earlier samples, this version does not load the entire audio file into memory.  
It uses Media Foundation streaming decode + multiple XAudio2 buffers,  
making it suitable for large audio files (e.g. FLAC > 100 MB).  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio2_Sample4.png)
  
---
  
**WASAPI Player Sample 1**
  
This sample demonstrates how to use the IAudioClient to render different  
audio formats like WAV, FLAC, MP3 etc.  
using the IMFSourceReader to decode the format suitable for playing in  
WASAPI's IAudioClient and renderer.  
  
The sample plays formats like WAV (pcm and floatingpoint), MP3 and FLAC.  
It shows you the basics of using the IAudioClient, iAudioRenderer,  
IAudioClock and IAudioStreamVolume interfaces.  
The sample uses the MfPeakMeter component. This requires that you install the MfComponents.  
In your projectsettings you have to add ..MfPack\Samples\MfComponents in the project options searchpath.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer1.png)
  
---
  
**WASAPI Player Sample 2**
  
This sample is based on Sample 1, but includes full threaded code,  
using threadsafe events and also uses a custom MFT for bass and treble control.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer2.png)
  
---
  
**WASAPI Player Sample 3**
  
This sample is based on Sample 2, instead of a bass/treble MFT, this one is equiped with  
a 3 band High, Mid(Peaking or Notch) and Low EQ MFT and a spectrum analizer.  
It also stores and read back settings for the MFT.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer3.png)
  
---
  
**WASAPI Player Sample 4**
  
This sample demonstrates a **real-time audio playback engine on Windows** using:
  
- **WASAPI** (event-driven, shared mode)
- **Media Foundation SourceReader**
- A custom **3-band EQ** implemented as an **IMFTransform (MFT)**
- A **Compressor / Limiter DSP**
- Thread-safe **GUI and engine** command routing
- Persistent EQ settings stored via **INI file**
  
## Features
  
- Glitch-free real-time playback.
- Low / Mid / High EQ with live control.
- Mid band supports **Peaking** or **Notch** mode.
- Adjustable **Q (bandwidth)** and shelf slopes.
- Settings dialog with persistent storage.
- Compressor / Limiter DSP with live control from the settings menu.
  
**Note:**  
This is not a sample for absolute beginners. Please read the documents included with this sample.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer4.png)
  
---
  
**WASAPI Player Sample 5**
  
This is a non-visual components application sample using WASAPI.  
The sample shows you how to build a real-time audio playback engine for  
Windows using Delphi style components.  
  
## Features  
This sample also demonstrates how to create and implement a:
  
- Compressor/Limiter component using a MFT.
- Parametric Equalizer component using a MFT.
- Flanger/Echo component using a MFT.
- WasApi effects rack that connects al MFT components to the WasApi renderer.
- PlayerEngine component, using a WasApi renderer.
  
**Note:**  
This is not a sample for absolute beginners.  
Please read the documents included with this sample.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer5.png)
  
---
  
**MfCaptureVideoFromGPU Sample 1**
  
This sample demonstrates how to capture from screen using your videocard GPU.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/CaptureVideoFromGPUSample.png)
  
---
  
**MfCaptureVideoFromGPU Sample 2**
  
This sample demonstrates how to capture a selected monitor (screen)  
connected with the graphics card GPU using DXGI Desktop Duplication,  
optionally captures system audio via WASAPI loopback, shows a live preview,  
and writes the result to a file (typically MP4 with H.264 video and AAC or FLAC audio).  
  
Audio-only recording can be done separately to WAV or FLAC using the  
dedicated recorder unit.  
  
**Note:**  
You need good knowledge of Media Foundation, DirectX and GPU processing.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfCaptureVideoFromScreen2s.png)
  
---
  
***Complex samples***
  
**MfRDJ Radio Mixer sample**  
This sample demonstrates how to build an audio mixer,  
effects and how to implement IceCast/Caddy for internet broadcasting.  
The Mixer is fully adjustable for audio endpoint assignments, mixer decks and loopback decks.  
All WASAPI sample code comes together in this sample.  
  
**Note:**  
 You have to know the principles of WASAPI, MFT's and audio manipulation.  
 This sample is large and not suitable for beginners!  
 Before using this sample make sure, you have all needed components installed (see instructions).  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/RDJ_Interface_s.png)
  
  
**MfRDJ Pro Radio Mixer sample**  
MfRDJPro is the extended version of MfRDJ.
  
Where MfRDJ mainly demonstrates DJ-style audio playback and mixing with MfPack,  
MfRDJ Pro adds a complete live broadcast layer around the mixer.  
MfRDJ Pro still provides the familiar RDJ functions: channel decks,  
loopback decks, microphone input, effects, PFL/cue monitoring, playlist editing,  
tag editing, and local recording.  
  
The Pro version expands this into an audio/video streaming application.  
It can combine the live program audio with camera video or a static video source,  
encode the result with Microsoft Media Foundation, and publish it as a browser-playable stream.  
The main technical difference is the broadcast pipeline.  
MfRDJ Pro uses Media Foundation Sink Writer and the MPEG-4 media sink to create fragmented MP4.  
MfRDJ Pro observes the generated MP4 byte stream, extracts and patches fMP4 fragments,  
writes a rolling live.json manifest, and serves the result through Caddy.  
Modern browsers can then play the stream using Media Source Extensions without needing Icecast for the video path.  
MfRDJ Pro also writes now-playing metadata, artwork links, on-air state, and  
listener counts to JSON files for the web interface.  
It includes safeguards for long-running broadcasts, such as bounded queues,  
fragment cleanup, Caddy mirroring, sleep prevention options, and clean shutdown handling.  
MfRDJ Pro supports casting to Cromecast devices on your local network using the  
MfPack Cast V2 protocols.  
  
**Note:**  
You have to know the principles of WASAPI, MFT's, MfPack Cast V2, and audio manipulation.  
This sample is large and not suitable for beginners!  
Before using this sample make sure, you have all needed components installed (see instructions).  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/RDJPro_Interface_s.png)
  
---
**MfMftExplorer sample**

This sample demonstrates how to discover Media Foundation Transforms (MFTs) installed on a  
Windows system and their properties. 

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfMftExplorer.png)
  
---
***Educational samples***
  
**MfCustomMFT samples**  
Small, beginner-oriented examples that introduce custom Media Foundation  
Transforms one step at a time.  
The project has 6 samples where each sample is lifted from the previous one.  
At sample 6 a programmer should be able to build a simple registered MFT and  
how to use it in Media Foundation.  
  
![Read about it in details](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Samples/MfCustomMFT/Readme.md)
  
  
![Sample 1](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfCustomMft_S1.png)
Sample 1
  
---
  
***Tools***
  
**HResult Lookup Tool Application**
  
This tool can be used to determine HResult or Error codes returned by the Windows OS,  
featuring a detailed build in "HResult Lookup tool", the "Windows System Error Code Lookup Tool" and  
the "System.SysUtils.SysErrorMessage" function, that provides the localized translation (language of your OS) of the HResult.  
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/HResultLookUpToolApp.png)
  
  
**(c) FactoryX. All rights reserved.**

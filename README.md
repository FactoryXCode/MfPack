# MfPack

Delphi translations for Microsoft Media Foundation and related Windows multimedia APIs.

---

## About MfPack

MfPack provides Delphi translations, helpers, and samples for modern Windows multimedia development, including audio, video, graphics, and GPU-accelerated capture.

---

## APIs Covered

### Core Audio APIs
- Windows Audio Session API (WASAPI)
- DeviceTopology API
- EndpointVolume API
- Multimedia Device (MMDevice) API

### Media & Graphics APIs
- Microsoft Media Foundation (successor of DirectShow)
- XAudio2 (successor of DirectSound)
- Microsoft DirectX APIs  
  *(D2D1, D3D9, D3D11, D3D12, DirectComposition, DXGI, DirectWrite, DXVA)*  
  > Note: D3D11 and later are part of Media Foundation

### Additional APIs
- Windows Imaging Component (WIC)
- MPEG-2 API
- Complete Windows Media (WinMM) API

---

## Latest Release

**Version:** X 3.1.9  
**Delphi:** XE2 (recommended XE7) up to Delphi 12  
**Windows SDK:** 10.0.26100.4654 (Windows 11)

---

## Samples

### MfVideoThumbNails

This sample demonstrates generating video thumbnails using Media Foundation and Direct2D.

> Requires the May 2019 D2D1 API.  
> Older Delphi versions should use the D2D1 version included with MfPack.

![MfVideoThumbNails](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/VideoThumbNailsSample.jpg)

---

### MediaEngine Player 2 & IMFTimedText

MediaEngine player (IMFMediaEngine/Ex) with subtitle support using the IMFTimedTextNotify interface.

![MediaEngine Player 2](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MediaEnginePlayer2.jfif)

---

### MfTranscode

Demonstrates using the Media Foundation Transcode API to convert audio or video files to different formats.

Supported formats include:

**Audio**
- WAV
- MP3
- FLAC
- M4A
- WMA

**Video**
- AVI
- MP4 (AAC or AC-3)
- WMV

![MfTranscode](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfTranscode%20Sample%202.png)

---

### MfSimpleCapture

Shows how to implement capturing within a Media Foundation session.

> Note: The old Microsoft sample used the deprecated `IMFPlayer` interface.

![MfSimpleCapture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfSimpleCapture.png)

---

### MfPeakMeter & MfAudioVisualizer Components

Examples demonstrating custom audio visualization components.

- MfPeakMeter / MfPeakMeterEx / MfPeakMeterMmcs
- MfAudioVisualizer (WASAPI loopback, Peak/RMS, Spectrum, VU)

![MfPeakMeter](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfPeakmeter.jfif)

![MfAudioVisualizer](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfAudioVisualizer.png)

---

### AudioClip Samples

Demonstrates extracting uncompressed audio from media files using IMFSourceReader and IMFSinkWriter.

- **AudioClip** – synchronous extraction
- **AudioClipEx** – callback-based extraction
- **AudioClipEx2** – threaded extraction with SinkWriter

![AudioClip Sample](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/AudioClipExSample.jpg)

---

### MfPlayer Series

Media Foundation player samples with increasing feature complexity.

- **MfPlayer I** – basic player
- **MfPlayer II** – extended player
- **MfPlayer X** – subtitles, language tags, regex, metadata, IMFTimer

![MfPlayer X](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfMediaPlayer_X.jfif)

---

### Ducking Media Player

Demonstrates a media player that responds to Windows audio ducking and tracks the system volume mixer.

![Ducking Media Player](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/DuckingMediaPlayerSample.jpg)

---

### Ducking Capture Sample

Demonstrates audio ducking by capturing and discarding audio samples.

![Ducking Capture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ChatDemo.jfif)

---

### MFFrameCapture

Demonstrates synchronous and asynchronous frame capture using IMFSourceReader.

![MFFrameCapture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfFrameCapture.png)

---

### CameraFrameCapture

Demonstrates capturing still images from webcams and cameras.

![CameraFrameCapture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/CameraFrameCaptureSample.png)

---

### CaptureEngineVideoCapture

Demonstrates recording and previewing video using IMFCaptureEngine and IMFCapturePreviewSink.

![CaptureEngineVideoCapture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MFCaptureEngineVideoCapture.jpg)

---

### SinkWriter Samples

Demonstrates video creation using the SinkWriter API.

- Bitmap-to-video
- Image sequence-to-video
- Image + audio-to-video

![SinkWriter Sample](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/SinkWriterSample.png)

![Image to Video](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ImageToVideo_3.jpg)

---

### LoopBackCapture Samples

Demonstrates capturing system audio using WASAPI loopback.

![LoopBackCapture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/LoopBackCapture.png)

![LoopBackCapture 2](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/LoopbackCapture2.png)

---

### Threaded Loopback Capture

Low-latency, MMCSS-aware loopback capture with stream-switch detection.

![Threaded Loopback Capture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ThreadedLoopbackCapture.png)

---

### XAudio2 Player Samples

Streaming audio playback using Media Foundation decoding and XAudio2 rendering.

![XAudio Basic Player](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio_Basic_Sample.png)

![XAudio Sample 2](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio2_Sample2.png)

![XAudio Sample 3](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/XAudio2_Sample3.png)

---

### WASAPI Player Samples

Threaded WASAPI playback with custom MFT-based EQ and spectrum analyzer.

![WASAPI Player 2](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer2.png)

![WASAPI Player 3](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/WasApiPlayer3.png)

---

### MfCaptureVideoFromGPU

GPU-based desktop capture using DXGI Desktop Duplication and Media Foundation encoding.

![GPU Capture](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/CaptureVideoFromGPUSample.png)

![GPU Capture 2](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfCaptureVideoFromScreen2s.png)

---

## Tools

### HResult Lookup Tool

Utility for decoding Windows HRESULT and system error codes.

![HResult Lookup Tool](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/HResultLookUpToolApp.png)

---

© FactoryX — All rights reserved.

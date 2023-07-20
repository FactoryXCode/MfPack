### Before going on, some policy:
- If you have a request, question, idea or need help, please commit to [Discussions](https://github.com/FactoryXCode/MfPack/discussions).

- If you encounter a bug then [Issues](https://github.com/FactoryXCode/MfPack/issues) is the one and only place to be.


# About MfPack

 Delphi translations for Microsoft Media Foundation and related API's.

# MfPack covers the Delphi translations of:

- Core Audio API's:
  * Windows Audio Session API (WASAPI, successor of DirectSound) 
  * DeviceTopology API
  * EndpointVolume API
  * Multimedia Device (MMDevice) API
- Microsoft Media Foundation API (successor of DirectShow).
- XAudio2 API (XBox game development)
- Microsoft DirectX API's (D2D1, D3D9, D3D11, D3D12, DirectComposition, DXGI, DirectWrite and DXVA) Note that D3D11 and above are part of Media Foundation.
- Windows Imaging Component (WIC) API.
- MPEG2 API.
- The COMPLETE Windows Media (WinMM) API.
- Media Foundation samples.

# <u>Latest release:</u> 


# MfPack 

Version X 3.1.5
Delphi XE2 up to and including Delphi 12 
SDK version: 10.0.22621.0 (Windows 11)


# Samples


**MfVideoThumbNails sample**

*This sample app needs the D2D1 Api. If your Delphi version doesn't have the latest D2D1 Api (May 2019 update), use the D2D1 MfPack version.*



![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/VideoThumbNailsSample.jpg)

**MediaEngine Player 2 & IMFTimedText sample**

*MediaEngine player (IMFMediaEngine(Ex)) with support for subtitles (closed captions) using the IMFTimedTextNotify interface.*



![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MediaEnginePlayer2.jfif)

**MfTranscode sample**

*Shows how to use the transcode API, to transcode a source file to Windows Media format. This sample has some more features than the original MS sample.*



![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/mftransform.jfif)

**MfSimpleCapture sample**

*This example shows, how you have to implement capturing within a session.*

*Note: The old MS sample used the deprecated IMFplayer interface.*



![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfSimpleCapture.jpg)

**MfPeakMeter and MfPeakMeterEx component sample**

*An example about how to create a MfPeakMeter control. This sample is part of the Samples/MfComponents package.*



![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfPeakmeter.jfif)

**AudioClip and AudioClipEx sample**

*Demonstrates using the IMFSourceReader API to extract uncompressed media data from a media file. This sample application reads audio data from a media file and writes the uncompressed audio to a WAVE file.*

*The AudioClipEx sample demonstrates using the IMFSourceReader and IMFSourceReaderCallback API to extract uncompressed mediadata from a media file.*



![AudioClip sample](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/AudioClipExSample.jpg)



**MfPlayer**

*Player samples, based on the CPlayer sample.*

*There are 3 samples in 3 project degrees.*

- *MfPlayer I : The basic player sample.*

- *MfPlayer II : The extended version I sample.*

- *MfPlayer X : This is an example that shows how to use the IMFTimer, language tags,*  
               *subtitles (SubRip and MicroDvd), RegEx (Regular Expressions) and how to get media properties.*



![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfMediaPlayer_X.jfif)



****

**Ducking Media Player**

  *This sample implements a simple media player that responds to the "ducking"* 
  *feature in Windows 7 and later. It also implements a volume control which tracks*
  *to the volume control in the volume mixer.* 
  
![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/DuckingMediaPlayerSample.jpg)

**Ducking Capture Sample**

  *This sample implements a simple "Chat" that demonstrates to the "ducking"* 
  *feature in Windows 7 and later. It simply captures samples from the sound card and* 
  *discards them.*

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/ChatDemo.jfif)

**MFFrameCapture sample**

*Demonstrates how to capture an image (synchronous or A-synchronous) using the IMFSourceReader.*

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MfFrameCapture.png)

**CameraFrameCapture sample**
*Demonstrates how to capture a still image or so called snapshot (A-synchronous) from a* 
*capture device such as a webcam or camera using the IMFSourceReader.*

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/CameraFrameCaptureSample.png)

**CaptureEngineVideoCapture sample**
*Demonstrates how to capture a snapshot or a recording (A-synchronous) from a capture device,*
*such as a webcam using the IMFCaptureEngine and IMFCapturePreviewSink.*
*This sample also demonstrates how to use the MfMediaTypeDebug API.*

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/MFCaptureEngineVideoCapture.jpg)

**SinkWriterToEncodeVideo Sample**

*Example 1 creates a simple green bitmap (640x480) and store it to a file with a length of 20 seconds.*
*Example 2 demonstrates how to use the SinkWriter to create a video from one or more bitmap files.*
  
 ![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/SinkWriterSample.png)

**LoopBackCapture Sample 1**

*This sample shows, how to capture sound from your soundcard using WASAPI and save this capture*
*with the quality that is supported by your soundcard.*

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/LoopBackCapture.png)


**LoopBackCapture Sample 2**

This sample demonstrates how to capture system audio either from a specific process tree or for all process except
a process tree and the use of ActivateAudioInterfaceAsync Win32 API with a new initialization structure. 
The new data structure makes  it possible to restrict captured audio data to that rendered by a specific 
process and any of its child processes. Windows 10 has always supported capturing all audio that is played on 
an audio endpoint (referred to as "system" loopback capture), which captures all audio from all apps that 
are playing sounds on the chosen audio endpoint. 

With the new structure, only audio from the specified process, and its children, will be captured. Audio rendered by
other processes will not be captured. A flag is also provided to reverse the behavior, capturing all system
audio *except* those from the the specified process (and its children). Furthermore, the capture is not tied to a 
specific audio endpoint, eliminating the need to create a separate IAudioClient to capture from each physical 
audio endpoint. 

If the processes whose audio will be captured does not have any audio rendering streams, then the capturing 
process receives silence.

It also demonstrates how to get a process by using the tlhelp32 API, to list a snapshot of running processes and be able to pick one.
The application is provided with a dialog to select a running process from the process tree you want to pick and has
a button to get the current PID of your application.

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/LoopbackCapture2.png)  
  
**© FactoryX. All rights reserved.**

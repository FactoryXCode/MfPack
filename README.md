# MfPack

 Delphi translations for Microsoft Media Foundation and related API's.

# MfPack covers the Delphi translations of:

- Core Audio API's:
  * Windows Audio Session API (WASAPI, successor of DirectSound) 
  * DeviceTopology API
  * EndpointVolume API
  * Multimedia Device (MMDevice) API
- Microsoft Media Foundation API (successor of DirectShow).
- XAudio2 API (XBox game development)
- Microsoft DirectX API's (D2D1, D3D11, DirectComposition, DXGI, DirectWrite and DXVA)
- Windows Imaging Component (WIC) API.
- MPEG2 API.
- The COMPLETE Windows Media (WinMM) API.
- Media Foundation samples.


# <u>Latest release:</u> 


# MfPack 

Version X 3.1.2 
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


**Simple Player**

*Demonstrates audio/video playback using the IMFPMediaPlayer and IMFPMediaPlayerCallback API.*
*Note: This API is deprecated, but still -partly- functional in Windows 10 version 2004 (May 2020 update).*
       It's not recommemble to use this API, because of many issues concerning playing different formats.

![](https://github.com/FactoryXCode/MfPack/blob/Master/MfPack/Pic/SimplePlayer.jfif)

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

**© FactoryX. All rights reserved.***

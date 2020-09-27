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
- Microsoft DirectX API's (D2D1, DirectComposition, DXGI, DirectWrite and DXVA)
- Windows Imaging Component (WIC) API.
- MPEG2 API.
- The COMPLETE Windows Multi Media (WinMM) API.
- Media Foundation samples.


# <u>Latest release:</u> 


# MfPack 

Version X 3.0.0 Enigma release.  
Delphi XE2 up to and including Delphi 10.3.3.  
SDK version: 10.0.19041.0, May 2020, OS Win10 update (2004)


# Samples


**MfVideoThumbNails sample**

*This sample app needs the D2D1 Api. If your Delphi version doesn't have the latest D2D1 Api (May 2019 update), use the D2D1 MfPack version.*



![](https://a.fsdn.com/con/app/proj/mfpack/screenshots/VideoThumbNailsSample.jpg/max/max/1)

**MediaEngine Player 2 & IMFTimedText sample**

*MediaEngine player (IMFMediaEngine(Ex)) with support for subtitles (closed captions) using the IMFTimedTextNotify interface.*



![](https://a.fsdn.com/con/app/proj/mfpack/screenshots/MediaEngine%20Player%202a.jpg/max/max/1)

**MfTranscode sample**

*Shows how to use the transcode API, to transcode a source file to Windows Media format. This sample has some more features than the original MS sample.*



![](https://a.fsdn.com/con/app/proj/mfpack/screenshots/mftransform.jpg/max/max/1)

**MfSimpleCapture sample**

*This example shows, how you have to implement capturing within a session.*

*Note: The old MS sample used the deprecated IMFplayer interface.*



![](https://a.fsdn.com/con/app/proj/mfpack/screenshots/MfSimpleCapture.jpg/max/max/1)

**MfPeakMeter component sample**

*An example about how to create a MfPeakMeter control. This sample is part of the Samples/MfComponents package.*



![](https://a.fsdn.com/con/app/proj/mfpack/screenshots/MfPeakMeter.jpg/max/max/1)

**AudioClip and AudioClipEx sample**

*Demonstrates using the IMFSourceReader API to extract uncompressed media data from a media file. This sample application reads audio data from a media file and writes the uncompressed audio to a WAVE file.*

*The AudioClipEx sample demonstrates using the IMFSourceReader and IMFSourceReaderCallback API to extract uncompressed mediadata from a media file.*



![AudioClip sample](https://a.fsdn.com/con/app/proj/mfpack/screenshots/AudioClipExSample.jpg/max/max/1)



**MfPlayer**

*Player samples, based on the CPlayer sample.*

*There are 3 samples in 3 project degrees.*

- *MfPlayer I : The basic player sample.*

- *MfPlayer II : The extended version I sample.*

- *MfPlayer X : This is an example that shows how to use the IMFTimer, language tags,*  
               *subtitles (SubRip and MicroDvd), RegEx (Regular Expressions) and how to get media properties.*



![](https://sourceforge.net/p/mfpack/screenshot/MfMediaPlayer%20X.jpg)



****

**Ducking Media Player**

  *This sample implements a simple media player that responds to the "ducking"* 
  *feature in Windows 7 and later. It also implements a volume control which tracks*
  *to the volume control in the volume mixer.* 
  
![](https://dc576.4shared.com/img/5_Nqbv90ea/s24/17441862340/DuckingMediaPlayerSample?async&rand=0.7740816140241176)


**Simple Player**

*Demonstrates audio/video playback using the IMFPMediaPlayer and IMFPMediaPlayerCallback API.*
*Note: This API is deprecated, but still functional in Windows 10 version 2004 (May 2020 update).*


**© FactoryX. All rights reserved.***

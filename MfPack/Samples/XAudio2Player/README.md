# XAudio2Player samples

Version: X 3.1.7

Description:
  These samples demonstrates how to use XAudio2 to render different file formats like WAV, FLAC, MP3 etc.
  The samples uses the IMFSourceReader to decode the format suitable for playing in XAudio2.
  XAudio2 is the long-awaited replacement for DirectSound. It addresses several outstanding issues and feature requests, like
  low latency etc.
  
This sample has 3 levels.
  - Sample 1 Plays formats like wav, mp3 and flac. It shows you the basics of using XAudio2.
    The sample uses the MfPeakMeter component. This requires that you install the MfComponents.
    In your projectsettings you must add ..MfPack\Samples\MfComponents in the project options searchpath.  
 -  Sample 2 implements the IXAudio2VoiceCallback and has a pitch control.
 -  Sample 3 uses threadsafe events, instead of messages.
 -  Sample 4 Shows how to implement effects and a progressbar.
           It also uses a TMemoryStream instead of a byte array to store the audio buffers.
           The rendering loop will be processed in a separate thread to avoid sound disturbtion.   

NOTES:
 - This release is updated for compiler version 17 up to 34.
 - SDK version: 10.0.26100.0 (Win 11)
 - Requires Windows 10 or later.
 - Minimum supported MfPack version: 3.1.6

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 30-03-2024
Final release date: 31-05-2024

Copyright © FactoryX. All rights reserved.





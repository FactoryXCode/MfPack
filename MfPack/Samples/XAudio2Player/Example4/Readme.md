**XAudio2Player Sample 4**

&#x20; 

Version: 4.0.0

&#x20; 

Description

&#x20; 

This sample demonstrates a Media Foundation–based streaming audio player built on top of XAudio2.

&#x20; 

Audio files (WAV, MP3, FLAC, AAC, etc.) are decoded on-the-fly using the Media Foundation Source Reader and  

rendered through XAudio2 using a multi-buffer streaming model.  

Playback control (Play, Pause, Stop, Replay, Seek) is fully thread-safe and handled by a   

dedicated worker thread, avoiding UI blocking and audio glitches.  

&#x20; 

The sample also shows how to:  

* Apply XAudio2 effects (reverb, mastering limiter).
* Perform sample-accurate seeking during playback.
* Update a progress bar and timing UI based on streamed audio.
* Implement thread-safe callbacks and events instead of message-based signaling.
* Track real-time audio levels using the MfPeakMeter component.

&#x20; 

Unlike earlier samples, this version does not load the entire audio file into memory.  

It uses Media Foundation streaming decode + multiple XAudio2 buffers,  

making it suitable for large audio files (e.g. FLAC > 100 MB).  

&#x20; 

Notes

&#x20; 

This release is updated for Delphi compiler versions 17 up to 34.  

Windows SDK: 10.0.28000.2705 (Windows 11)  

&#x20; 

Requires Windows 10 or later  

Minimum supported MfPack version: 3.1.8  

&#x20; 

Dependencies

&#x20; 

MfPeakMeter component  

Requires installing the MfComponents package.  

Add the following path to your project search path:  

&#x20; 

..\\MfPack\\Samples\\MfComponents

&#x20; 

Project Information    

Project: Media Foundation – MFPack – Samples  

&#x20; 

Project location:  

https://github.com/FactoryXCode/MfPack  

https://sourceforge.net/projects/MFPack  

&#x20; 

First release date: 13-08-2025  

Final release date: 05/05/2026  

&#x20; 

(c)  FactoryX. All rights reserved.


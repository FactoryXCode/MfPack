XAudio2Player Sample 4



Version: 3.1.9



Description



This sample demonstrates a Media Foundation–based streaming audio player built on top of XAudio2.



Audio files (WAV, MP3, FLAC, AAC, etc.) are decoded on-the-fly using the Media Foundation Source Reader and rendered through XAudio2 using a multi-buffer streaming model.

Playback control (Play, Pause, Stop, Replay, Seek) is fully thread-safe and handled by a dedicated worker thread, avoiding UI blocking and audio glitches.



The sample also shows how to:



* Apply XAudio2 effects (reverb, mastering limiter).



* Perform sample-accurate seeking during playback.



* Update a progress bar and timing UI based on streamed audio.



* Implement thread-safe callbacks and events instead of message-based signaling.



* Track real-time audio levels using the MfPeakMeter component.



Unlike earlier samples, this version does not load the entire audio file into memory.

It uses Media Foundation streaming decode + multiple XAudio2 buffers, making it suitable for large audio files (e.g. FLAC > 100 MB).



Notes



This release is updated for Delphi compiler versions 17 up to 34.

Windows SDK: 10.0.22621.4654 (Windows 11)



Requires Windows 10 or later

Minimum supported MfPack version: 3.1.8



Dependencies



MfPeakMeter component

Requires installing the MfComponents package.

Add the following path to your project search path:



..\\MfPack\\Samples\\MfComponents



Project Information



Project: Media Foundation – MFPack – Samples

Project location:



https://github.com/FactoryXCode/MfPack



https://sourceforge.net/projects/MFPack



First release date: 13-08-2025

Final release date: 08-01-2026



Copyright © FactoryX. All rights reserved.


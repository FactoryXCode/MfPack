# WASAPI Player samples

Version: 3.2.0

Description:
These samples demonstrates how to use the IAudioClient to render different audio formats like WAV, FLAC, MP3 etc.
The samples uses the IMFSourceReader to decode the format suitable for playing in WASAPI's IAudioClient and renderer.

This sample has 2 levels yet.

* Example 1 Plays formats like wav, mp3 and flac pcm or floatingpoint.
It shows you the basics of using the IAudioClient, iAudioRenderer, IAudioClock and IAudioStreamVolume interfaces.
The sample uses the MfPeakMeter component. This requires that you install the MfComponents.
In your projectsettings you must add ..MfPack\\Samples\\MfComponents in the project options searchpath.
* Example 2 is based on Sample 1, but includes full threaded code, using threadsafe events and also uses a
custom MFT for bass and treble control.
* Example 3 is based on Sample 2, but has a GUI + settings dialog with persistent configuration has a 3 band (high, Mid (Peaking / Notch) and Low EQ MFT.
* Example 4 is based on Sample 3, but has a TMfVAudioAnalizer.
* Example 5 is a sample application build with WasApi components, using DSP and MFT effects.



NOTES:

* This release is updated for compiler version 17 up to 34.
* SDK version 10.0.22621.4654 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 3.1.9



Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
https://sourceforge.net/projects/MFPack

First release date: 02/09/2024
Final release date: 05/05/2026

Copyright © FactoryX. All rights reserved.


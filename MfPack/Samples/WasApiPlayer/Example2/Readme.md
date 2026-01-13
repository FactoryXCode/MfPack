# WASAPI Player Sample 2

Version: X 3.1.9



Description:
These samples demonstrates how to use the IAudioClient to render different audio formats like WAV, FLAC, MP3 etc.
The samples uses the IMFSourceReader to decode the format suitable for playing in WASAPI's IAudioClient and renderer.



The sample is fully threaded for smooth operations and uses threadsafe events. A MFT is used for bass and treble control,

this MFT is a plug-inn type that don't needs to be registred for Windows.
It shows you the basics of using the IAudioClient, iAudioRenderer, IAudioClock and IAudioStreamVolume interfaces and how to

create a MFT that integrates in the sound stream using the Media Foundation IMFTransform interface.


The sample uses the MfPeakMeter component. This requires that you install the MfComponents.
In your projectsettings you must add ..MfPack\\Samples\\MfComponents in the project options searchpath.



NOTES:

* This release is updated for compiler version 17 up to 34.
* SDK version 10.0.22621.4654 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 3.1.7



Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
https://sourceforge.net/projects/MFPack

First release date: 02/09/2024
Final release date: 25/07/2025

Copyright © FactoryX. All rights reserved.


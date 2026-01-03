# LoopbackCapture Sample 3

Version: X 3.1.8

Description:

Demonstrates how to capture sound from a rendering or capture device (one of the soundcard's endpoints)
using WASAPI in combination with IMFSinkWriter to write wav and FLAC files.

Note that the earlier version used the MMIO Api to write wav files only. But since we added FLAC as output format that's a noop.

The sample difference from the original sample where the audioclient buffersize is fixed that leads to an oversized buffer on modern audio devices.
This sample lets you to choose between different latency's and buffersize for better sound to eliminate buffer related gliches.



It has some more advanced features like:

* The rendering part is running in a separate thread.
* Possibility to write wav data in native format 44.1 kHz/ 16 bit/ 2 channels/ PCM or the soundcard's native audio format (including Uncompressed IEEE floating-point audio).



NOTES:

* The sound you recorded can be different from what you hear on the speakers.
  This is caused by the fact Windows 11 has an internal process to "smooth" audio output (you can disable this feature in the Windows Audio settings).
* To get as less latency as possible, you might alter your computer energymode to "Best Performance" in your PC settings.
* This release is updated for compiler version 17 up to 34.
* SDK version 10.0.22621.4654 (Win 11)
* Requires Windows 10 version 22H2 or later.
* Minimum supported MfPack version: 3.1.8



Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
https://sourceforge.net/projects/MFPack

First release date: 12/06/2024
Final release date: 01/01/2026

Copyright © FactoryX. All rights reserved.


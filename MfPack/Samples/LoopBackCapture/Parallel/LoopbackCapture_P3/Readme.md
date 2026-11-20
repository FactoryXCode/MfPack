# Parallel Loopback Capture Sample 2

&#x20; 

Version: 4.1.0

&#x20; 

Description:

&#x20; 

Demonstrates how to capture sound from a rendering or capture device (one of the soundcard's endpoints) using WASAPI in combination with mmio to write wav-files.
Note that mmioOpen() is deprecated as mentioned in the documents, but this seems to be a bug in the documentation since Windows Vista.
  

The sample difference from the original sample where the audioclient buffersize is fixed that leads to an oversized buffer on modern audio devices.  
This sample lets you to choose between different latency's and buffersize for better sound to eliminate buffer related gliches.  
  

It has some more advanced features like:

* The application runs the rendering part in a parallel task, which is a much simpler and safer approach when using a TThread class.
* Able to use MMCSS (Multimedia Class Scheduler service).
* Stream switch detection.
* possibility to write wav data in native format 44.1 kHz/ 16 bit PCM or the soundcard's audio format (including Uncompressed IEEE floating-point audio).

&#x20; 

NOTES:

* The sound you recorded can be different from what you hear on the speakers.
This is caused by the fact Windows 11 has an internal process to "smooth" audio output (you can disable this feature in the Windows Audio settings).
* To get as less latency as possible, you might alter your computer energymode to "Best Performance" in your PC settings.
* This release is updated for compiler version 17 up to 34.
* SDK version 10.0.28000.2705 (Win 11)
* Requires Windows 10 version 22H2 or later.
* Minimum supported MfPack version: 4.1.0

&#x20; 

Project: Media Foundation - MFPack - Samples  

&#x20; 
Project location: https://github.com/FactoryXCode/MfPack
https://sourceforge.net/projects/MFPack

&#x20; 

First release date: 12/06/2024  
Final release date: 18/11/2026  

&#x20; 

Copyright © FactoryX. All rights reserved.

&#x20; 


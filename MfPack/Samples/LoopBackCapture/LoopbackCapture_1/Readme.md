# LoopbackCapture Sample 1

Version: X 3.1.5

Description:

  Demonstrates how to capture sound from the default rendering device (soundcard)
  using WASAPI in combination with mmio to write wav-files.
  Note that mmioOpen() is deprecated as mentioned in the documents, 
  but this seems to be a bug in the documentation since Windows Vista.
  The sample difference from the original sample where the audioclient buffersize is fixed that leads to an oversized buffer on modern audio devices.
  This sample lets you to choose between different latency's for better sound to eliminate buffer related gliches.

NOTES:
 - The sound you recorded can be different from what you hear on the speakers.
   This is caused by the fact Windows 11 has an internal process to "smooth" audio output (you can disable this feature in the Windows Audio settings)
 - This release is updated for compiler version 17 up to 34.
 - SDK version 10.0.22621.0 (Win 11)
 - Requires Windows 10 version 22H2 or later.
 - Minimum supported MfPack version: 3.1.5

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 12-03-2023
Final release date: 24/05/2023

Copyright © FactoryX. All rights reserved.
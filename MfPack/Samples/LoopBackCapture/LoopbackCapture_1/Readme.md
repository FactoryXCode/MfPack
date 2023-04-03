# LoopbackCapture Sample 1

Version: X 3.1.4

Description:

  Demonstrates how to capture sound from the default rendering device (soundcard)
  using WASAPI in combination with mmio to write wav-files.
  Note that mmioOpen() is deprecated, but still functional in Win 11.
  Instead applications should call CreateFile to create or open files.

NOTES: 
 - This release is updated for compiler version 17 up to 34.
 - SDK version 10.0.22621.0 (Win 11)
 - Requires Windows 10 or later.
 - Minimum supported MfPack version: 3.1.4

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 12/03/2023
Final release date: 03/04/2023

Copyright © FactoryX. All rights reserved.
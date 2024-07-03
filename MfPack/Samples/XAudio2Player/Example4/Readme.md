# XAudio2Player Sample 4

Version: X 3.1.7

Description:
  This sample demonstrates how to implement XAudio2 effects and
  how to go forward or backward during playing with a progressbar.
  This sample uses a TMemoryStream instead of a byte array to store the audio buffers.
  Also the rendering loop will be processed in a separate thread to avoid sounddisturbtion. 

This sample shows you how to implement threadsafe events and methods instead of using messages from the XaudioEngine.
The sample uses the MfPeakMeter component. This requires that you install the MfComponents.
In your projectsettings you must add ..MfPack\Samples\MfComponents in the project options searchpath.  

NOTES:
 - This release is updated for compiler version 17 up to 34.
 - SDK version: 10.0.26100.0 (Win 11)
 - Requires Windows 10 or later.
 - Minimum supported MfPack version: 3.1.6

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 06-04-2024
Final release date: 02-06-2024

Copyright © FactoryX. All rights reserved.

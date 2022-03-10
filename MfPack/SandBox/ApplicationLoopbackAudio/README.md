# Application loopback API Capture Sample

Version: under construction


Description:
This sample demonstrates the use of ActivateAudioInterfaceAsync Win32 API with a new initialization structure. 
The new data structure makes it possible to restrict captured audio data to that rendered by a specific 
process and any of its child processes. Windows 10 has always supported capturing all audio that is played on 
an audio endpoint (referred to as "system" loopback capture), which captures all audio from all apps that 
are playing sounds on the chosen audio endpoint. 

With the new structure, only audio from the specified process, and its children, will be captured. Audio rendered by
other processes will not be captured. A flag is also provided to reverse the behavior, capturing all system
audio *except* those from the the specified process (and its children). Furthermore, the capture is not tied to a 
specific audio endpoint, eliminating the need to create a separate IAudioClient to capture from each physical 
audio endpoint. 

If the processes whose audio will be captured does not have any audio rendering streams, then the capturing 
process receives silence.

To use this sample, obtain the process ID for the process tree you wish to capture or exclude from capture.
You can use Task Manager or the tlist program to get this ID. Run the sample with the process ID, the
desired capture mode (including the process tree or excluding it), and the output WAV file.

Examples:

* Capture audio from process 1234 and its children: `ApplicationLoopback 1234 includetree Captured.wav`
* Capture audio from all process except process 1234 and its children: `ApplicationLoopback 1234 excludetree Captured.wav`

    
NOTES: 
 - This sample is under construction
 - This release is updated for compiler version 17 up to 34.
 - SDK version 10.0.20348.0
 - Requires Windows 10 build 20348 or later.
 - Minimum supported MfPack version: 3.1.0

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 01-06-2021
Final release date: unknown

Copyright © FactoryX. All rights reserved.
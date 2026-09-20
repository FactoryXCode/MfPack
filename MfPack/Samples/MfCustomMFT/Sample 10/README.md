# Sample 10 - audio file through the delay MFT

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

Sample 9 used generated PCM bytes. This sample reads a real audio file through
an `IMFSourceReader`, passes decoded PCM samples through the same
`TMfAudioDelayMFT`, drains the effect tail, and writes the result as a PCM WAV
file. The form is a normal VCL `.pas` and `.dfm` pair.

## Run from the Delphi IDE

1. Open `MfAudioDelayFile.dproj`.
2. Select **Win32** and **Debug**, then choose **Project > Build MfAudioDelayFile**.
3. Press **F9**.
4. Click **Browse...** and choose an audio file. WAV, MP3, WMA, M4A, and AAC
   are listed; the actual decoder support depends on the installed Windows
   Media Foundation components.
5. Enter the delay in milliseconds and the wet mix percentage.
6. Click **Create delayed WAV...**, choose an output filename, then click
   **Play result** to hear it.

The project has relative search paths to MfPack `src` and Sample 9. It uses
the Sample 9 transform source directly; there is no second transform copy.

## Follow the data

1. `MFCreateSourceReaderFromURL` opens the file. The first audio stream is
   selected and requested as 16-bit PCM.
2. `GetCurrentMediaType` supplies the actual sample rate, channel count, and
   block alignment. This sample accepts mono or stereo PCM.
3. For each decoded `IMFSample`, `ProcessInput` queues it and `ProcessOutput`
   returns the same sample after the delay effect runs in place.
4. `WriteSample` locks the output buffer and writes complete PCM frames after
   the 44-byte WAV header.
5. At end of stream, `MFT_MESSAGE_COMMAND_DRAIN` emits the delayed tail. The
   WAV sizes are then filled in.

The form calls `ProcessAudioFile` synchronously so this step stays focused on
the file and transform contracts. The window can be unresponsive while a long
file is processed; a later sample can introduce a worker thread and progress
reporting.

Standard RIFF WAV limits output to about 4 GiB. This sample reports an error
instead of producing an invalid larger file.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

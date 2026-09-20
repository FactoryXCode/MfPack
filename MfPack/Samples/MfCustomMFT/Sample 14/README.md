# Sample 14 - Output sample ownership

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

This application-local step extends the Sample 9 PCM delay MFT. It advertises
`MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES`, so the client may pass a sample in
`MFT_OUTPUT_DATA_BUFFER.pSample` or leave that field nil. The MFT writes into
the supplied sample in the first case and supplies its own in the second.
`TInterfacedObject` still supplies the IUnknown methods.

## Build and run in the Delphi IDE

1. Open `MfOutputSampleDemo.dproj` in Delphi.
2. Select **Win32 Debug**, build the project, and press **F9**.
3. Read the `PASS` line in the console. It remains open for five seconds.

The project search path points to `..\..\..\src`. This console step needs no
audio file, sound device, DLL registration, or administrator rights.

## Read the code in this order

1. `GetOutputStreamInfo` advertises `CAN_PROVIDE_SAMPLES` and a one-frame
   minimum `cbSize`. The former `PROVIDES_SAMPLES` flag would prohibit caller
   samples and require `cbSize = 0`. `FIXED_SAMPLE_SIZE` is not advertised:
   input samples can contain different numbers of PCM frames.
2. `ProcessPending` checks the caller buffer capacity before changing the
   circular delay state. If it is too small, it returns
   `MF_E_BUFFERTOOSMALL`; the client can retry with a larger sample. It copies
   PCM and sample metadata while leaving the input sample unchanged.
3. `ProcessTail` can write the delayed tail into either kind of output sample.
4. `MfOutputSampleDemo.dpr` checks the stream flags, both allocation paths,
   retry, input immutability, metadata, and the delayed tail. It also retains
   Sample 9's PCM8/16 and discontinuity checks.

`cbSize` is only the minimum audio-frame size. A client that wants one output
sample for an entire pending input sample must allocate enough space for that
input block. A Media Session is free to choose either allocation path. This
sample keeps the MFT application-local so the existing registered DLL and
players continue using their prior implementation.

Microsoft reference:
[Output stream flags](https://learn.microsoft.com/en-us/windows/win32/api/mftransform/ne-mftransform-_mft_output_stream_info_flags)
and [output buffer rules](https://learn.microsoft.com/en-us/windows/win32/api/mftransform/ns-mftransform-mft_output_data_buffer).
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

# Sample 9 - application-local audio delay MFT

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

This starts an audio series alongside the grayscale samples. It translates the
core code from Microsoft's `mft_audiodelay` example into Delphi. The class
inherits from `TInterfacedObject`, so Delphi supplies `QueryInterface`,
`_AddRef`, and `_Release` automatically. The program creates the class directly;
there is no COM server or registration in this step.

## What to read in order

1. `MfAudioDelayDemo.dpr` constructs a complete PCM type and sets the two
   effect attributes.
2. `SetInputType` and `SetOutputType` validate matching formats. The accepted
   formats are mono or stereo, 8-bit unsigned or 16-bit signed PCM.
3. `ProcessInput` retains one sample. `ProcessOutput` edits that sample in
   place and returns it to the caller.
4. `ProcessPcm` mixes each input value with the value at the current position
   in the circular delay buffer. It writes the original input into that buffer
   for the next pass.
5. `MFT_MESSAGE_COMMAND_DRAIN` makes `ProcessOutput` emit the remaining delay
   buffer as a tail. `MFT_MESSAGE_COMMAND_FLUSH` discards pending input and
   fills the delay buffer with silence.

The attribute GUIDs match Microsoft's example:

| Attribute | Meaning | Default |
| --- | --- | ---: |
| `MF_AUDIODELAY_DELAY_LENGTH` | Buffer length in milliseconds | 1000 |
| `MF_AUDIODELAY_WET_DRY_MIX` | Delayed proportion, 0 to 100 percent | 25 |

The delay buffer is allocated when streaming begins or the first input arrives.
`IMfAudioDelayControl.SetEffect` updates both settings during playback under
the transform lock. A delay change retains the newest input samples in the
resized buffer; wet/dry mix is read for each output call. Clients that set the
delay attribute directly are picked up on the next input sample.

The transform also clears its delay history when an input sample has the
`MFSampleExtension_Discontinuity` flag or its timestamp jumps by more than
5 ms. This protects a seek from mixing audio left in the circular buffer at
the previous position. `MFT_MESSAGE_COMMAND_FLUSH` still clears the history
when the pipeline sends it.

## Build and run in Delphi

1. Open `MfAudioDelayDemo.dproj` in the Delphi IDE.
2. Select the **Win32** target and **Debug** configuration.
3.Add the following in Project > Options > Output directory: .\$(Platform)\$(Config), 
   Unit Output directory: .\$(Platform)\$(Config), Search Path: ..\..\..\src
4. Choose **Project > Build MfAudioDelayDemo**.
5. Press **F9** to run it from the IDE.

The console remains open for five seconds after a successful run so you can read
the result. Delphi Community Edition can build and run this project in the IDE.

The console program needs no audio device or file. It uses unsigned 8-bit,
mono PCM at 1000 frames per second, with a 1 ms delay and 100% wet mix:

```text
input:       129, 130
main output: 128, 129   (128 is unsigned PCM silence)
drain tail:  130
```

It also checks signed 16-bit PCM with a 50/50 mix. The program fails with an
exception if either output differs and prints `PASS` otherwise.

## Scope of this first step

This teaching version provides output samples itself and processes each input
sample in place. It requires whole audio frames and accepts one pending input
sample at a time. It does not yet support caller-provided output buffers,
partial output from one input sample, thread-safe concurrent calls, or COM DLL
registration. Those can be introduced separately without obscuring the basic
delay algorithm.

Sample 10 adds a VCL file reader and WAV output. Sample 11 puts the transform
in a Media Session topology for real-time playback. Sample 12 packages it as a
registered audio-effect DLL and verifies it with MfMftExplorer. Sample 13 adds
seeking and checks that delay history does not cross a seek boundary.

Microsoft reference: `Windows-classic-samples/Samples/Win7Samples/multimedia/mediafoundation/mft_audiodelay`.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

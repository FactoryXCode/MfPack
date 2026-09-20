# MfCustomMFT samples

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0


Small, beginner-oriented examples that introduce custom Media Foundation
Transforms one step at a time.

## Sample 1 - RGB32 grayscale transform

Implements a synchronous, in-place `IMFTransform` that converts an RGB32
video frame to grayscale. The VCL application creates a synthetic color
frame, passes it through the transform with `ProcessInput` and
`ProcessOutput`, and displays the result.

See [Sample 1/README.md](Sample%201/README.md) for the complete walkthrough.

## Sample 2 - Video file through the grayscale transform

Uses a synchronous `IMFSourceReader` to decode a video file to RGB32. Each
frame is displayed, passed through the Sample 1 grayscale transform, and then
displayed again in grayscale.

See [Sample 2/README.md](Sample%202/README.md) for the walkthrough.

## Sample 3 - Asynchronous playback through the grayscale transform

Uses an asynchronous `IMFSourceReader` callback for real-time, timestamp-paced
video playback. It demonstrates worker-thread callbacks, safe delivery to the
VCL thread, explicit frame ownership, and a one-frame-in-flight design that
prevents an unbounded queue.

See [Sample 3/README.md](Sample%203/README.md) for the walkthrough.

## Sample 4 - Grayscale MFT in a Media Session topology

Places the unregistered grayscale transform directly between a video source
and the Enhanced Video Renderer. It demonstrates topology construction, type
negotiation by the topology loader, Media Session events, presentation-clock
playback, and orderly asynchronous shutdown.

See [Sample 4/README.md](Sample%204/README.md) for the walkthrough.

## Sample 5 - Registered grayscale MFT

Packages the transform as a Win32 in-process COM DLL and registers it as a
synchronous Media Foundation Video Effect. It can then be discovered with
`MFTEnumEx`, activated by CLSID, and inspected with MfMftExplorer.

See [Sample 5/README.md](Sample%205/README.md) for build, registration,
inspection, and cleanup instructions.

## Sample 6 - Playback through the registered MFT DLL

Discovers the Sample 5 Video Effect with `MFTEnumEx`, activates it through its
`IMFActivate` object, and inserts the DLL-provided `IMFTransform` into a Media
Session playback topology. No transform implementation is linked into the
client.

See [Sample 6/README.md](Sample%206/README.md) for the walkthrough.

## Sample 7 - FFmpeg DTS audio decoder MFT

Implements a reusable synchronous Media Foundation audio decoder backed by a
minimal dynamically linked FFmpeg runtime. It includes Win32/Win64 COM/MFT
projects, a stable C bridge, reproducible third-party source provenance, tests,
and an installer layout.

See [Sample 7/README.md](Sample%207/README.md) for the implementation and
packaging contract.

## Sample 8 - Application-local DTS decoder

Loads the Sample 7 decoder DLL and its FFmpeg runtime beside a client
application, registers its class factory with `MFTRegisterLocal`, and decodes
through Media Foundation without installing a COM server or writing MFT
registry entries.

See [Sample 8/README.md](Sample%208/README.md) for build and run instructions.

## Sample 9 - Application-local PCM audio delay MFT

Begins the audio-delay series with a Delphi `TInterfacedObject` implementation
of `IMFTransform`. A console harness checks a one-frame PCM delay and its
drained tail without an audio device or file.

See [Sample 9/README.md](Sample%209/README.md) for the walkthrough and IDE
build instructions.

## Sample 10 - Audio file through the delay MFT

Uses a VCL form to choose an audio file, decodes it to PCM with
`IMFSourceReader`, runs the Sample 9 delay transform, saves a WAV file including
the effect tail, and plays the result.

See [Sample 10/README.md](Sample%2010/README.md) for the IDE walkthrough.

## Sample 11 - Real-time audio delay in a Media Session

Builds a source-to-delay-to-audio-renderer topology and handles asynchronous
Media Session events in a VCL form. Delay and wet mix can be changed while a
file plays. It uses the application-local Sample 9 MFT.

See [Sample 11/README.md](Sample%2011/README.md) for the IDE walkthrough.

## Sample 12 - Registered audio delay MFT

Packages the PCM delay transform as a Win32 in-process COM DLL and registers it
as a Media Foundation Audio Effect. MfMftExplorer can enumerate and inspect
the registered transform. A separate VCL player discovers the DLL with
`MFTEnumEx` and changes delay and wet mix during playback.

See [Sample 12/README.md](Sample%2012/README.md) for IDE build, registration,
Explorer verification, playback, and unregistering instructions.

## Sample 13 - Seekable audio delay player

Extends the registered-MFT player with a timeline, Pause, seeking, and replay
after playback ends. It demonstrates how the Media Session coordinates a seek
and how the delay MFT clears old audio history after a discontinuity or flush.

See [Sample 13/README.md](Sample%2013/README.md) for the IDE walkthrough and
the seek and flush behavior.

## Sample 14 - Output sample ownership

Extends the application-local delay MFT so `ProcessOutput` accepts either a
caller-provided sample or an MFT-provided sample. A console harness checks both
paths, a too-small buffer and retry, metadata, and draining the delayed tail.

See [Sample 14/README.md](Sample%2014/README.md) for the IDE walkthrough and
the output buffer contract.

## Sample 15 - Live camera and microphone delay

Combines a camera and microphone with `MFCreateAggregateSource` in one live
Media Session topology. The form previews camera video while microphone audio
passes through the Sample 14 delay MFT to the speakers. Delay and wet mix take
effect while the capture runs; no recording file is created.

See [Sample 15/README.md](Sample%2015/README.md) for the IDE walkthrough and
the aggregate-source topology.
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

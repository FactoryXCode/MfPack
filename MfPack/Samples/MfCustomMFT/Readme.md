# MfCustomMFT samples

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

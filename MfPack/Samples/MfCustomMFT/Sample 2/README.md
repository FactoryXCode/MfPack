# Sample 2 - Source Reader to grayscale MFT

This sample applies the synchronous RGB32 grayscale transform from Sample 1
to decoded frames from a video file.

The application deliberately seeks and reads one frame when the trackbar thumb
is released. That makes the Media Foundation data flow visible without adding
callback, threading, audio, or playback-clock code.

## Data flow

```text
video file
    -> IMFSourceReader (decode and convert to RGB32)
    -> IMFSample
    -> TMfGrayscaleMFT.ProcessInput
    -> TMfGrayscaleMFT.ProcessOutput
    -> grayscale RGB32 IMFSample
```

Because the transform processes in place, the application copies the original
frame to the left-hand bitmap before calling `ProcessInput`. The right-hand
bitmap is copied from the sample returned by `ProcessOutput`.

## Important steps

1. `MFCreateSourceReaderFromURL` opens the selected file.
2. All streams are deselected and the first video stream is selected.
3. `SetCurrentMediaType` asks the Source Reader for decoded RGB32 frames.
4. The actual RGB32 media type is assigned to both sides of the transform.
5. Releasing the trackbar thumb calls `SetCurrentPosition` with the requested
   presentation time.
6. `ReadSample` synchronously obtains frames until their timestamp reaches the
   requested position. This accounts for seeking to an earlier keyframe.
7. The selected sample goes through the normal MFT input/output contract.
8. Reader flags are checked with bit masks because several flags can be set
   at the same time.

The code also handles an end-of-stream result and reconfigures the transform
if the Source Reader reports a current-media-type change.

## Run it

Open `MfCustomMFTFile.dproj` in Delphi XE7, build Win32 Debug, and run it.
Choose a video with **Open video**, move the trackbar, and release its thumb to
decode and transform the frame at that position.

## Deliberate limitations

- synchronous seeking on mouse release rather than real-time playback;
- video only; audio is not selected;
- RGB32 system-memory samples only;
- no seeking, scaling controls, D3D surfaces, or hardware zero-copy path.

Those features would obscure the first useful connection between an
`IMFSourceReader` and a custom `IMFTransform`.

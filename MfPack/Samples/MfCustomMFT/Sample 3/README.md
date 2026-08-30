# Sample 3 - Asynchronous video playback through the grayscale MFT

This sample turns Sample 2 into simple real-time playback. An asynchronous
`IMFSourceReader` decodes a video to RGB32, each frame passes through the
custom grayscale MFT, and the original and transformed frames are displayed
side by side.

## Data flow

```text
video file
    -> asynchronous IMFSourceReader
    -> IMFSourceReaderCallback.OnReadSample
    -> TMfGrayscaleMFT
    -> one owned frame packet
    -> VCL window message
    -> timestamp-paced display
```

## The important asynchronous rules

1. The Source Reader is created with `MF_SOURCE_READER_ASYNC_CALLBACK`.
2. `ReadSample` returns immediately; the result arrives in `OnReadSample` on
   a Media Foundation worker thread.
3. The callback does not touch VCL controls. It transforms and copies the
   frame, then posts an owned packet to the form with `PostMessage`.
4. Only one `ReadSample` call and one posted frame can be outstanding. The UI
   requests the next sample only after it displays the current one. This
   provides simple back-pressure and prevents an unbounded frame queue.
5. Presentation timestamps are compared with `GetTickCount`; a VCL timer
   delays early frames until their display time.
6. A session number makes late callback messages from a closed reader stale,
   so the form can free them safely without displaying them.
7. Closing detaches the form window, flushes the reader, and frees any frame
   packet still owned by the UI.

The form and both video panels are double-buffered. `TImage` is a graphic
control painted by its parent, so an unbuffered parent can erase its background
between frames and produce visible flashes during playback.

The packed frame packets are top-down. RGB32 Source Reader buffers use Media
Foundation's signed-stride convention: positive stride is bottom-up and
negative stride is top-down. The copy code handles both cases explicitly.

## Run it

Open `MfCustomMFTAsync.dproj` in Delphi XE7, build Win32 Debug, and run it.
Choose **Open video**, then use **Play/Pause** and **Stop**.

## Deliberate limitations

- video only; audio is not selected or synchronized;
- RGB32 system-memory samples only;
- no seeking, frame dropping, playback-rate control, or D3D surfaces;
- the custom transform is called directly instead of being inserted in a
  Media Session topology.

These limits keep the example focused on callback threading, sample
ownership, back-pressure, and timestamp-paced playback.

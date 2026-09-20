# Sample 13 - Seekable audio delay player

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

This sample adds a playback timeline to the registered audio delay player from
Sample 12. It uses the same registered `FactoryX PCM Audio Delay MFT` DLL and
keeps the live delay and wet-mix sliders.

## Build and run from the Delphi IDE

1. Close Sample 12's player and MfMftExplorer so the DLL is not in use.
2. Rebuild `..\Sample 12\FactoryXAudioDelayMFT.dproj` as **Win32 Debug**. The
   Sample 9 MFT now also clears delay history on a stream discontinuity or a
   timestamp jump. If the DLL path has not changed, its existing registration
   still points to the rebuilt DLL. Otherwise, run Sample 12's
   `Register Win32.cmd` as administrator.
3. Open `MfSeekableAudioDelayPlayer.dproj`, build **Win32 Debug**, and press
   **F9**.
4. Open a seekable audio file, start playback, and drag the timeline thumb.
   Releasing it requests one seek. The label shows current and total time.
   Arrow, Home, End, Page Up, and Page Down keys also seek on key release.
5. Try seeking backward and forward while the echo is audible. The old echo
   must not reappear at the new position. Press **Pause**, seek, and check that
   playback remains paused at the new position. Press **Play** to resume.
6. Let the file play to the end. **Play** stays available; press it to replay
   the same loaded file from the beginning. **Stop** also leaves the file
   loaded, and the next **Play** starts from the beginning.

The timeline remains disabled when the source does not advertise seeking or
does not provide a duration. Playback, pause, stop, and live effect controls
otherwise work as in Sample 12.

## Why the seek is coordinated by the Media Session

The form sends `IMFMediaSession.Start` with a `VT_I8` position in 100-nanosecond
units. It sends the request only on mouse release or after a keyboard step, and
waits for `MESessionStarted` before accepting another seek. A timer reads the
presentation clock to update the timeline while playback continues.

`Start` also starts playback when called from a paused state. The form remembers
that state and requests `Pause` after `MESessionStarted`. It completes the seek
after `MESessionPaused`, so the timeline and Play/Pause button remain disabled
while these asynchronous state changes are pending.

`VT_EMPTY` resumes from the Media Session's current position. After Stop or
natural completion, the player sends `Start` with a `VT_I8` position of zero
instead. The form keeps the loaded topology, enables Play, and disables Stop
until playback starts again.

The form never calls `IMFTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH)`
directly. A direct flush while the Media Session is processing a sample could
race its pipeline. The MFT already handles a pipeline flush under its lock. It
also clears its circular delay history inside `ProcessInput` when the source
marks a discontinuity or timestamps jump by more than 5 ms. That reset occurs
before the first sample at the new position is mixed.

The Sample 9 console demo checks timestamp jumps, explicit discontinuity flags,
and pipeline flushes, in addition to PCM8/16 mixing and the drain tail. The
timeline has 1000 steps, so its position is approximate; a source may also
start decoding slightly before the requested time.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

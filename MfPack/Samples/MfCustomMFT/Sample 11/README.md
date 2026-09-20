# Sample 11 - audio delay in a Media Session topology

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

This sample plays an audio file in real time through the application-local
`TMfAudioDelayMFT`. It follows the topology and event pattern from grayscale
Sample 4, with only an audio branch. The transform remains a Delphi
`TInterfacedObject` and does not require COM registration.

## Run from the Delphi IDE

1. Open `MfAudioDelayTopology.dproj`.
2. Select **Win32** and **Debug**, then choose **Project > Build
   MfAudioDelayTopology**.
3. Press **F9**.
4. Set the delay (1–10000 ms) and wet mix (0–100%) with the sliders. The
   labels show their current values. The defaults are 250 ms and 50%.
5. Click **Open audio...** and choose a file.
6. Wait for **Topology ready**, then click **Play**. **Pause** and **Stop**
   control the Media Session.
7. While the file plays, drag either slider. Changes are applied without
   reopening the file.

The project contains a `.pas` form and `.dfm` layout. Its search paths to
MfPack `src` and Sample 9 are relative to the project folder. Decoder and file
support depend on the installed Media Foundation components.

## Topology

```text
audio media source
  -> source-stream topology node
  -> decoder inserted by the topology loader when needed
  -> TMfAudioDelayMFT transform node
  -> audio renderer output node
```

`AudioTopologyPlayer.CreatePlaybackTopology` selects the first audio stream,
creates the nodes, and sets the delay and mix attributes before streaming.
The Media Session negotiates a supported PCM type and drives playback with
its presentation clock. The session reports state changes asynchronously.

The `IMFAsyncCallback` runs on a Media Foundation worker thread. It posts
owned notices to the form's window; only the form thread changes VCL controls.
The player waits for `MESessionClosed` before shutting down the session and
source. The shared Sample 9 MFT now serializes its stateful `IMFTransform`
methods with `TMonitor`, so pipeline calls cannot race over its sample and
delay-buffer state.

The form calls `IAudioTopologyPlayer.UpdateEffect`, which uses the transform's
`IMfAudioDelayControl` interface. This method shares the same lock as audio
processing. A wet-mix change applies to the next output block. A delay change
resizes the circular buffer and retains the newest input history. Increasing
the delay adds silence for history that does not yet exist; an abrupt delay
change can still be audible as a jump.

The first audio stream is used. This step has no seeking, rate control,
volume control, protected-content handling, or COM registration. Open the
file again to replay after end of presentation.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

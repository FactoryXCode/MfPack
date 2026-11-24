# MfPlaylist — VCL sequencer sample
  
  
NOTES:
  
* This release is updated for compiler version 23 up to 34.  
* SDK version 10.0.28000.2705 (Win 11)  
* Minimum supported MfPack version: 4.1.0  
  
  
A Delphi VCL implementation of Microsoft's [How to Create a Playlist](https://learn.microsoft.com/en-us/windows/win32/medfound/how-to-create-a-playlist). It uses MfPack's COM and Media Foundation declarations directly, so the native source, topology and session calls remain visible for study.
  
## Try it

1. Open `MfPlaylist.dproj` in Delphi (VCL, Win32 or Win64).
2. Add two or more local media files. Use Up, Down, Remove and Clear to arrange the list.
3. Click **Build playlist**. Each entry becomes a sequencer segment. Wait for **State: Ready**.
4. Click **Play**. The event trace shows `MENewPresentation` queuing following topologies and the renderer handing off between segments.
5. Use Pause/Resume or Stop. Select a list entry and click **Play selected** to start that sequencer segment at its beginning. After Stop or completion, Play restarts at the first segment.
6. Click **Release** before changing the list. Closing the window also releases the session and sources.
  
The preview area hosts the Enhanced Video Renderer (EVR) for video.
For audio-only items, it displays a JPG, BMP or PNG from the audio file's folder, scaled to fit while preserving its proportions.
It first tries an image with the track's basename, followed by Art, Cover, Folder, Front and Album, then other images in alphabetical order.
Unreadable images are skipped. Artwork is refreshed on segment changes and cleared when the sequence is released; folders without artwork leave the preview black.
  
The progress bar shows the current segment's position, alongside its duration and the presentation clock. Click the bar to start playing the current segment at that position. Alternatively, right-click the desired position and choose **Play from this position**. This also starts playback when paused or stopped. Seeking needs a current segment with a known duration; it uses a native sequencer segment offset. All VCL controls, including the artwork image and progress menu, are in the DFM and available in the form designer.
  
## Follow the walkthrough in the code  
  
| Microsoft step | Sample implementation |
| --- | --- |
| 1. Create the Media Session | `TMfPlaylistPlayer.BuildPlaylist`: `MFCreateMediaSession`, callback subscription and presentation clock |
| 2. Create the Sequencer Source | `BuildPlaylist`: `MFCreateSequencerSource`, query `IMFMediaSource` and `IMFMediaSourceTopologyProvider` |
| 3. Create native media sources | `AddSegment`: `IMFSourceResolver.CreateObjectFromURL` and the native source's presentation descriptor |
| 4. Create partial topologies | `CreateTopology`: source-stream nodes connected to audio renderer and EVR output nodes; the topology loader inserts decoders |
| 5. Append topologies | `AddSegment`: `AppendTopology`, with `SequencerTopologyFlags_Last` on the final entry; clear the previous last flag as each entry is added |
| 6. Queue the first topology | `BuildPlaylist` obtains the **sequencer's** presentation descriptor; `QueuePresentation` gets the provider's rewritten topology and submits it to the session |
| 7. Handle session events | `ProcessEvent`: `MENewPresentation` supplies the next descriptor to `QueuePresentation`; status events drive the UI and active segment |
| 8. Shut down | `ClosePlaylist`: shut down the sequencer's media source, close and await the session's close notification, then shut down the session |
  
`MfPlaylistPlayer.pas` contains this controller.  
`frmMfPlaylist.pas/.dfm` contains the VCL interface.  
The controller does not depend on VCL or a higher-level MfPack player component.  
  
The original per-file topology must not be submitted straight to the Media Session.  
`IMFMediaSourceTopologyProvider` replaces its native-source nodes with sequencer nodes.  
The sample queues those provider topologies when `MENewPresentation` arrives, allowing Media Foundation to prepare the next segment before the current segment ends.

Topology IDs connect status events to playlist entries.  
`MF_TOPOSTATUS_SINK_SWITCHED` identifies renderer handoffs; `MF_TOPOSTATUS_STARTED_SOURCE` supplies the initial active-item cue and can otherwise describe preroll. `MESessionNotifyPresentationTime` provides the presentation-time offset used to display segment-relative time. Play selected uses `MFCreateSequencerSegmentOffset` and `MF_TIME_FORMAT_SEGMENT_OFFSET`.

## Threading and ownership

Initialize COM on the creating thread before constructing the controller, and free it before `CoUninitialize`.  
The sample balances `MFStartup`/`MFShutdown`.  
Commands and UI event handling stay on that thread.

`TMfPlaylistSessionEvents` receives events on a Media Foundation worker thread and posts owned event packets to the form.  
It never touches VCL controls. The form frees every packet, including stale packets from earlier playlist generations.  
Callback detachment is synchronized with posting, and the form drains pending packets before its window disappears.

The worker signals `MESessionClosed` directly so shutdown can wait without pumping UI messages.  
A bounded wait allows cleanup if native shutdown fails.  
Failed playlist construction shuts down sources already appended to the sequence as well as a source whose append failed.  
An error state stays latched until Release or a new build.

Set `-StudioRoot` for another Delphi installation, or `-MfPackRoot` for another source tree.
Outputs, compiler logs, test logs and generated fixtures go under `Build\<Platform>\<Config>`.
The IDE project also supports the `MfPackSource` MSBuild property when the source directory differs.
  
This additional check plays briefly, starts five seconds before the first file's end using a sequencer segment offset, verifies the handoff to the second file and its progress, then pauses, resumes, stops and releases the sequence. It plays through the local renderers at their existing volume. The files remain unchanged.

## Scope of this learning sample

- Local files and a static sequence. Release before editing, then rebuild.  
  Dynamic deletion and replacement of queued segments are outside this example.
- One selected audio stream and one selected video stream per file.  
  Other selected streams are deselected; track selection and subtitles are outside this example.
- Playback support depends on installed Media Foundation sources and decoders.  
  A filename extension in the dialog does not guarantee a decoder is installed.  
  Transitions between differing media formats are subject to the native sources and renderers; the sample makes no gapless-playback guarantee.
- Source resolution and topology construction run synchronously under the busy cursor to keep the tutorial readable.  
  A production app should consider asynchronous resolution for slow files and networks.

See also Microsoft's [sequencer source example](https://learn.microsoft.com/en-us/windows/win32/medfound/sequencer-source-example-code), [sequencer source overview](https://learn.microsoft.com/en-us/windows/win32/medfound/about-the-sequencer-source), and [sequencer source events](https://learn.microsoft.com/en-us/windows/win32/medfound/sequencer-source-events).
  
Project: MFPack - Samples - MfPlaylist  
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 05-05-2016  
Final release date: 24/11/2026  
  
Copyright © FactoryX. All rights reserved.



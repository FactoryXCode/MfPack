# Sample 4 - Grayscale MFT in a Media Session topology

This sample inserts the unregistered grayscale transform directly into a
Media Session playback topology. The Media Session owns the pipeline, drives
it with its presentation clock, and sends the transformed frames to the
Enhanced Video Renderer (EVR).

## Topology

```text
video media source
    -> source-stream topology node
    -> decoder/color converter inserted by the topology loader
    -> TMfGrayscaleMFT transform node
    -> EVR output node

audio media source
    -> source-stream topology node
    -> decoder inserted by the topology loader when needed
    -> audio renderer output node
```

The first video stream and, when present, the first audio stream are selected.
Other streams are deselected. The grayscale MFT affects only the video branch;
audio passes directly to the Media Foundation audio renderer.

## Important steps

1. `IMFSourceResolver` creates an `IMFMediaSource` for the chosen file.
2. The presentation descriptor selects its first video stream and optional
   first audio stream.
3. The source-stream node receives the source, presentation descriptor, and
   video stream descriptor attributes.
4. `TMfGrayscaleMFT` is created normally and assigned directly to an
   `MF_TOPOLOGY_TRANSFORM` node with `SetObject`. No registration is involved.
5. `MFCreateVideoRendererActivate` supplies the EVR activation object for the
   output node.
6. A separate audio source/output branch is added when the file contains
   audio. The topology is submitted with
   `IMFMediaSession.SetTopology`.
7. The topology loader negotiates media types and inserts the decoder and any
   required conversion needed to supply RGB32 to the custom transform.
8. When `MF_TOPOSTATUS_READY` arrives, the application obtains
   `IMFVideoDisplayControl` from the Media Session and enables playback.

Media Session events arrive on a Media Foundation worker thread. The callback
immediately requests the next event and posts a small owned notice to the VCL
window; it never touches controls directly.

Closing waits for `MESessionClosed`, then shuts down the Media Session and
media source. This breaks the reference cycle created when the session retains
its `IMFAsyncCallback`.

## Run it

Open `MfCustomMFTTopology.dproj` in Delphi XE7, build Win32 Debug, and run it.
Choose **Open video**, wait for **Topology ready**, and click **Play**.

## Deliberate limitations

- the first video and first audio stream only;
- one EVR window and the default audio endpoint;
- synchronous source resolution for local files;
- no seeking, rate control, volume, subtitles, or protected content;
- the transform is instantiated directly and is not registered.

Sample 5 will add COM and Media Foundation registration so the same transform
can be discovered and activated by applications such as MfMftExplorer.

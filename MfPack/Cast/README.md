# MfPack Cast V2

`MfCast.pas` is the application-facing facade. The other units contain device
discovery, Cast v2 TLS/channel handling, controller logic, media planning, and
the local range-capable HTTP server.

The facade supports compatible local media and direct HTTP/HTTPS media URLs.
Construction with `TMfCast.Create(True)` additionally enables Media Foundation
lifetime management, the segment publisher, and the transcode/subtitle-
compositor pipeline. Construction with `False` keeps conversion disabled for
direct-control applications.

Callbacks originate on the subsystem's worker threads. A VCL application must
marshal UI changes to the main thread. `MfSimpleCastPlayer` uses `PostMessage`
for this purpose. Connect, receiver launch, and initial media load can wait on
network timeouts, so the sample invokes the facade's `Cast` method from a worker
thread.

`MfCastWindowsSupport.pas` contains the optional `WM_MFCAST_*` message bridge
and reusable Cast/subtitle worker threads. Platform-neutral subtitle-choice
records and stable track/path helpers remain in `MfCastTypes.pas`. This keeps
window-thread plumbing out of application forms without making it mandatory for
non-VCL or non-windowed API consumers.

After the initial media status is received, `TMfCastChannel` starts a persistent
control worker. The worker consumes receiver and media status messages, answers
receiver heartbeat requests, and sends a heartbeat PING at the configured
interval. Cast writes are serialized so UI commands and heartbeat replies do
not overlap inside the SChannel TLS context. Disconnect stops the worker and
closes the socket before releasing channel state. A PLAY-time reconnect remains
as a fallback for genuine network interruptions.

Volume commands are routed according to the active media path. Transcoded media
uses an atomic PCM gain/mute stage immediately before AAC encoding. Direct media
uses receiver `SET_VOLUME` when device volume is adjustable, or media-session
`VOLUME` when the receiver reports `controlType: fixed`. These routes control
only MfCast playback and cannot override a Chromecast/Google TV system mute set
by its physical remote. Buffered receiver audio may make a PCM change audible a
few seconds after the command. Note: Chromecast/ Google Cast V2 does not support 
volume or mute. So, this way we can alter volume (stream volume and mute) that works
for Chromecast/ Google Cast V2.

`GetMediaTracks` returns stable opaque `Int64` track IDs together with the source
stream index, kind, source, language, name, and selection state. For an active
transcode, `SelectAudioTrack` restarts at the current source position using the
chosen Media Foundation audio stream.

`TMfCastSubtitleAsset.SourceName` identifies a selected sidecar subtitle file.
For embedded text, `TrackId`, `StreamIndex`, and `HasStreamIndex` provide exact
selection even when language metadata is absent or duplicated. `SelectSubtitle`
and `DisableSubtitles` switch the active transcoded presentation at its current
source position.

Cast V2 protobuf envelope encoding/decoding is isolated in
`MfCastProtocol.pas`; `MfCastChannel` owns transport, heartbeats, request state,
and receiver/media command policy.

Known architectural follow-ups:

- Remove Media Foundation declarations from the direct-control type/interface
  layer (the current direct path does not initialize a playback engine, but the
  units still compile against MfPack Media Foundation declarations);
- Add separate `Connect` and `Load` operations after preserving existing
  receiver behavior;
- Separate remuxing from full transcoding so compatible MKV codecs can avoid
  video re-encoding;

## MfPack Cast API 4.1.0
  
Version: 4.1.0 (Cast V2.1 wire protocol)

`TMfCast.ApiVersion()` and `MFCAST_API_VERSION` report the public API release.
Version 4.1.0 adds paired Android TV standby/power control through `TMfCast`.
  
**NOTES:**
  
- This release is updated for compiler version 17 up to 35.
- SDK version: 10.0.28000.2705 (Win 11)
- Requires Windows 10 or later.
- Minimum supported MfPack version: 4.0.0
  
---
  
MfCast is an independent Chromecast subsystem for Delphi.
It handles device discovery, receiver control, local media serving,
media preparation, subtitles, track selection,
and optional remuxing/transcoding behind a small public API.
  
MfCast can be used by:
  
- MfPack media-player samples
- Audio players
- Radio applications
- Slideshow or image applications
- Dedicated Chromecast control applications
- Third-party Delphi applications that use MfPack
- Applications that do not otherwise use MfPack player classes
  
  
***MfCast.pas*** is the application main interface (TMfCast class).  
The other units contain device discovery, Cast V2.1 TLS/channel handling, controller logic, media planning, and  
a local HTTP server.  
  
The main interface supports compatible local media and direct HTTP/HTTPS media URLs.  
Construction with TMfCast.Create(True) additionally enables Media Foundation  
lifetime management, the segment publisher, and the transcode/subtitle-  
compositor pipeline. Construction with False keeps conversion disabled for  
direct-control applications.  
  
Callbacks originate on the subsystem's worker threads. A VCL application must  
marshal UI changes to the main thread. MfSimpleCastPlayer uses PostMessage  
for this purpose. Connect, receiver launch, and initial media load can wait on  
network timeouts, so the sample invokes the main interface's Cast method from a worker  
thread.  

Diagnostic logging is enabled by default. Set `TMfCast.LoggingEnabled` to
`False` to suppress the `OnLog` callback and the facade's standard debugger
messages. Logging can be enabled or disabled while Cast workers are active;
`OnError` remains independent and is not suppressed.
  
**MfCastWindowsSupport.pas** contains the optional **WM_MFCAST_*** message bridge and  
reusable Cast/subtitle worker threads.  
Platform-neutral subtitle-choice records and stable track/path helpers remain in **MfCastTypes.pas**.  
This keeps window-thread plumbing out of application forms without making it mandatory for non-VCL or non-windowed API consumers.   
  
After the initial media status is received, **TMfCastChannel** starts a persistent  
control worker. The worker consumes receiver and media status messages, answers  
receiver heartbeat requests, and sends a heartbeat PING at the configured   
interval. Cast writes are serialized so UI commands and heartbeat replies do  
not overlap inside the SChannel TLS context. Disconnect stops the worker and  
closes the socket before releasing channel state. A PLAY-time reconnect remains  
as a fallback for genuine network interruptions.  
  
Volume commands are routed according to the active media path. Transcoded media  
uses an atomic PCM gain/mute stage immediately before AAC encoding. Direct media  
uses receiver **SET_VOLUME** when device volume is adjustable, or media-session  
**VOLUME** when the receiver reports controlType: fixed.  
These routes control only MfCast playback and cannot override a Chromecast/Google TV system mute set by its physical remote.  
Buffered receiver audio may make a PCM change audible a few seconds after the command.  
  
**GetMediaTracks**  
This method returns stable Int64 track IDs together with the source stream index, kind,  
source, language, name, and selection state.  
For an active transcode, **SelectAudioTrack** restarts at the current source position using the chosen Media Foundation audio stream.  
  
Automatic media planning treats remuxing as a separate route from transcoding.  
For a Matroska source containing frame-aligned H.264 video and optional AAC  
audio, **cmmRemuxFile** copies the compressed samples into a fragmented MP4  
presentation without decoding or re-encoding them. This preserves source video  
quality and substantially reduces CPU use. Incompatible MKV codecs and active  
burned subtitles continue to use **cmmTranscodeBurnedSubtitles**. Remuxed streams  
retain the generated-stream restart behavior for seeking, while volume remains  
receiver-controlled because there is no decoded PCM gain stage.  
  
**TMfCastSubtitleAsset.SourceName**  
Identifies the selected sidecar file or, for an embedded track, the media file.  
For embedded Matroska text and VobSub bitmap tracks, **TrackId**, **StreamIndex**,  
and **HasStreamIndex** provide exact selection even when language metadata is  
absent or duplicated. Text tracks and selected `.idx`/`.sub` sidecars or embedded  
`S_VOBSUB` tracks can be burned into transcoded video. The selected subtitles  
are also drawn in the local EVR preview. Embedded PGS tracks are identified but  
are not decoded.  
  
**SelectSubtitle** and **DisableSubtitles** switch the active transcoded presentation at its current  
source position.  
  
Cast V2.1 protobuf envelope encoding/decoding is isolated in  
**MfCastProtocol.pas**; **MfCastChannel** owns transport, heartbeats, request state,  
and receiver/media command policy.  
  
The direct-control contract is Media Foundation-neutral: **MfCastTypes.pas** and  
**MfCastInterfaces.pas** do not import MfPack Media Foundation declaration units.  
The optional byte-stream, sample, remux, and transcode contracts are isolated in  
**MfCastMediaInterfaces.pas**; only the media implementations and their composition  
layer compile against those declarations.  
  
### Automatic receiver wake-up

All applications using the shared TMfCast controller try the normal Cast
connection first. After a retryable connection failure, a known receiver MAC
address enables native Wake-on-LAN recovery, followed by bounded Cast connection
retries. Receivers that connect immediately, including always-available speakers,
use the normal path without sending wake packets.

The MAC address is learned from the Windows IPv4 neighbor table during discovery
and after a successful connection. Discover or connect once while the receiver is
awake. The per-receiver cache is shared by these applications for the current
Windows user at `%LOCALAPPDATA%\MfPack\Cast\WakeTargets.ini`. It is keyed by the
Cast device identity, not just an IP address. The current local interface and
subnet broadcast address are selected again for each attempt. A missing or
unusable MAC skips wake-up and retains ordinary retries.

Wake-on-LAN uses three bursts of standard 102-byte magic packets over UDP port 9,
addressed to the receiver's MAC via subnet broadcast and unicast. The receiver
must support network wake-up. HDMI-CEC can also turn connected equipment on.
This is native Delphi/Windows code; Python and remote-control pairing are not
required. Logs distinguish wake packets sent, wake-up skipped, and connection
retry results. File, URL, desktop, and RDJ live-stream casting share this behavior.
### Standby on explicit disconnect (paired Android TV)

The implementation unit `MfCastAndroidTvRemote.pas` is part of the Cast API in
`MfPack\Cast` and is included in `MfCastCompile.dpk`/`.dproj`. Applications use
the public `TMfCast` facade; they do not need to reference the remote unit:

```pascal
Cast.ImportAndroidTvPairing(Device, ExistingPairedPfxBytes);
if Cast.HasAndroidTvPairing(Device) then
begin
  Hr := Cast.GetDevicePowerState(Device, IsOn, Detail);
  // IsOn is valid only for S_OK. S_FALSE means no pairing is registered.
  Hr := Cast.RequestDeviceStandby(Device, Detail);
end;
```

`RequestDeviceStandby` operates on the specified paired receiver. `Disconnect`
already stops the active cast and requests standby for its paired receiver;
applications must not issue a second power command after Disconnect. Both power
operations can wait for network responses, so UI applications should call them
from a worker. Neither importing an existing pairing nor querying its presence
creates a new pairing or contacts the TV.

`TMfCast.Disconnect` first stops the Cast receiver and closes the Cast connections.
For an Android TV whose paired Remote v2 credential has been registered, it then
reads the actual TV power state and sends one remote POWER press only while the
TV reports on. The connection services power updates and heartbeats during a
five-second settling interval before that press. A subsequent power report must
confirm standby; errors and missing confirmation are logged. The power toggle is
never retried. An already-off TV is left off.

The standby target is the successfully connected Cast receiver, retained after
`Stop`; changing a sender's device selection cannot redirect the power command.
The target is cleared by Disconnect so repeated disconnects do not toggle power.
CastPlayer performs Disconnect on a worker thread to keep its window responsive.
The reusable `TMfCastDisconnectWorker` and its `WM_MFCAST_DISCONNECT_FINISHED`
completion message are part of `MfCastWindowsSupport`; the application only
starts the worker and handles its completion.

Pairing credentials are registered per stable Cast ID (with service-instance
fallback), not per IP address. They are encrypted for the current Windows user
with DPAPI under `%LOCALAPPDATA%\MfPack\Cast\AndroidTvRemotes`. Credentials and
network addresses are not embedded in source or executables. The native helper
`AndroidTvControl` can list Cast IDs, import an existing paired certificate/key
in an empty-password PFX, read power status, or request standby.

Importing an identity does not pair a new TV; the certificate must already have
been paired with that TV. Treat PFX files as private credentials and remove any
temporary conversion files after import. CastPlayer itself uses native Windows
SChannel and DPAPI and does not require Python or an external remote executable.
The current Sharp pairing can be reused without entering a new TV pairing code.

Unregistered devices receive ordinary Cast STOP/CLOSE and a log explaining that
no paired standby control is configured. This does not claim universal physical
standby: external Chromecast displays depend on their firmware/HDMI-CEC behavior,
and other receiver types require their own supported power-control mechanism.

Read the full documentation in  MfPack/Cast/MfPack_Chromecast_API_White_Paper.txt.
  
Project: MfPack - Cast  
Project location: MfPack  
  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 11/11/2026  
  
Copyright © FactoryX. All rights reserved.


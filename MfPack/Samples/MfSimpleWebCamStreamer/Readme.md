# MfSimpleWebCamStreamer

&#x20;

Version 4.0.0

&#x20;

`MfSimpleWebCamStreamer` is a compact Delphi VCL example for capturing a webcam
and microphone with Microsoft Media Foundation, encoding the result as H.264
and AAC, and publishing the live stream directly to a web browser through
fragmented MP4 and Media Source Extensions (MSE).

The sample is intentionally small. It demonstrates the complete path from
Media Foundation capture to a browser-playable live stream without requiring
IIS, Caddy, Icecast, FFmpeg, or another external media server.

It also records the same encoded stream to rolling fragmented MP4 files without
starting a second H.264 or AAC encoder.

&#x20;

## Requirements  

* Windows 10 or later;
* Delphi compiler version 23 through 35;  
* MfPack 4.0.0 or later;  
* A Media Foundation compatible webcam;  
* A Media Foundation compatible microphone;  
* A browser with Media Source Extensions and H.264/AAC MP4 support;  
* Network access between the browser and the sender PC for remote playback.  

The sample uses the Windows Media Foundation Source Reader, Sink Writer and  
fragmented MP4 media sink together with a small custom `IMFByteStream`.  

&#x20;

## What the sample demonstrates  

* Webcam enumeration and selection;
* Microphone enumeration and selection;
* Asynchronous Media Foundation Source Reader capture;
* H.264 video encoding;
* AAC audio encoding;
* Fragmented MP4 generation with `MFCreateFMPEG4MediaSink`;
* A custom writable and seekable `IMFByteStream`;
* Extraction of `ftyp`, `moov`, `moof`, and `mdat` boxes;
* Browser-compatible fMP4 fragment patching;
* A built-in Winsock HTTP server;
* Live browser playback through Media Source Extensions;
* Bounded fragment delivery and SourceBuffer pruning;
* Browser reconnect and stream-generation recovery;
* Retry and recovery after temporary HTTP interruption;
* Live-edge monitoring and correction;
* Rolling fragmented MP4 recording;
* Reuse of the already encoded live fragments for recording;
* Per-file recording timeline rebasing;
* Retention of a configurable number of recording files;
* Direct LAN and manually forwarded WAN access;
* Structured diagnostic logging and HRESULT reporting.

&#x20;

## Quick start  

1. Start `MfSimpleWebCamStreamer`.
2. Select the webcam.
3. Select the microphone.
4. Leave the HTTP port at `8080`, or enter another free TCP port.
5. Select **Start**.
6. Open a browser on the same PC:
`http://127.0.0.1:8080/`
7. Select **Start MSE** in the browser page.
8. To test from another device on the LAN, open:
`http://<sender-LAN-IP>:8080/`
9. Select **Stop** when finished.

&#x20;

The recording starts automatically after the fragmented MP4 initialization  
segment becomes available.  

&#x20;

## Controls  

|Control|Purpose|
|-|-|
|**Camera**|Selects the Media Foundation video capture device.|
|**Microphone**|Selects the Media Foundation audio capture device.|
|**Output**|Shows the current or next rolling recording filename.|
|**HTTP port**|Selects the local TCP port used by the built-in HTTP server.|
|**Start**|Starts capture, encoding, recording, and the HTTP server.|
|**Stop**|Stops HTTP publication, closes the recording, and shuts down capture.|
|**Pause HTTP**|Diagnostic control that deliberately drops HTTP requests.|
|**Resume HTTP**|Restores HTTP request handling after a diagnostic pause.|

&#x20;

The HTTP pause controls exist specifically to test browser retry and recovery.  
They do not stop Media Foundation capture or encoding.  

&#x20; 

## Capture and encoding architecture  

The live path is deliberately based on a single video encoder and a single  
audio encoder:  

&#x20; 

```text
Webcam ──> async IMFSourceReader ──> NV12 ──> H.264 ──┐
                                                     │
Microphone -> async IMFSourceReader -> PCM -> AAC ───┤
                                                     v
                                           fragmented MP4 sink
                                                     |
                                                     v
                                          custom IMFByteStream
```

&#x20; 

The video and audio Source Readers run asynchronously. Media samples are passed  
to one Sink Writer backed by `MFCreateFMPEG4MediaSink`.  

Using one encoder pair is important. An earlier rolling-recording experiment  
used a second H.264/AAC Sink Writer. Although logically separate, encoder  
competition and archive `Finalize/Create` operations could briefly disturb the  
live stream. The current design avoids that entirely.  

&#x20; 

## Fragmented MP4 byte stream  

The fragmented MP4 media sink writes to `TSimpleFmp4ByteStream`.  

For live streaming, the byte stream operates as a writable and seekable  
null-output stream. It emulates the positioning behavior expected by Media  
Foundation but does not create one continuously growing hidden MP4 file.  

The byte stream observes the generated MP4 boxes and extracts:  

&#x20;

```text
ftyp
moov
moof
mdat
```

&#x20; 

`ftyp + moov` becomes the MSE initialization segment.  

Each `moof + mdat` pair becomes one live media fragment.  

Only a bounded window of recent browser fragments is retained in memory.  

&#x20;  

## Browser-compatible MSE fragment patching  

&#x20; 

Raw fragmented MP4 output produced by Media Foundation is suitable for its own  
continuous byte stream, but individual fragments are not automatically  
self-contained for arbitrary browser delivery.  

Before a fragment is published to MSE, the sample patches its movie fragment  
metadata:  

1. `tfhd.base\\\_data\\\_offset` is removed;
2. `default-base-is-moof` is enabled;
3. a version 1 `tfdt` decode-time box is inserted;
4. `trun.data\\\_offset` is recalculated relative to the `moof`;
5. track decode times are advanced monotonically;
6. the original `mdat` payload is left unchanged.

&#x20; 

The resulting fragment is independent of the byte position at which Media   
Foundation originally generated it.  

The browser MIME type is:  

&#x20;  

```text
video/mp4; codecs="avc1.42C01F, mp4a.40.2"
```

&#x20;

## Built-in HTTP server

&#x20;

`TSimpleHttpServer` is a deliberately small raw Winsock HTTP server.

It does not require Indy or another HTTP framework.

The server listens on all local IPv4 interfaces with:



```text
INADDR\\\_ANY
```

&#x20;

The main endpoints are:

|Endpoint|Purpose|
|-|-|
|`/`|Browser MSE player and diagnostics page.|
|`/player.html`|Browser MSE player page.|
|`/status`|Current generation and fragment-window information.|
|`/init.mp4`|Current `ftyp + moov` initialization segment.|
|`/fragment/<sequence>.m4s`|Requested patched media fragment.|

&#x20;

Responses use `Cache-Control: no-store` so the browser does not cache changing  
live-stream state.

&#x20;

## MSE startup and live playback

&#x20;

The browser does not assume that the Media Foundation stream begins at media  
time zero.

At startup it:

1. Reads `/status`;
2. Chooses a fragment close to the current live edge;
3. Appends `init.mp4`;
4. Appends several retained media fragments;
5. Waits for a small startup buffer;
6. Seeks into the actual buffered timeline;
7. Starts playback.

&#x20;

The normal startup target is approximately two seconds of buffered media.

The browser deliberately does not wait synchronously for `video.play()` inside  
the fragment pump. Fragment acquisition and appending must continue while the  
browser resolves playback.

&#x20;

## Live edge and SourceBuffer management

&#x20;

The browser periodically measures its position relative to the end of the  
buffered live stream.

A large accumulated drift can be corrected by seeking back near the desired  
live edge.

The live-edge correction is intentionally conservative so normal jitter does  
not constantly move playback.

Old buffered media is pruned periodically. This prevents a long-running browser  
session from accumulating an ever-growing SourceBuffer.

The sample normally keeps roughly 30 seconds of recent buffered media with a  
small pruning margin.

&#x20;

## HTTP interruption and recovery

&#x20;

The browser contains retry and recovery logic for temporary network loss.

A fragment or status request is retried several times before the current HTTP  
path is considered unavailable.

After repeated failures, the browser continues watching for either:

* the same HTTP server and generation becoming available again; or
* a new capture generation.

&#x20;

If HTTP returns without capture having restarted, playback can resume using the  
retained fragment window.

The **Pause HTTP** and **Resume HTTP** buttons are provided to exercise this  
path without physically disconnecting a network cable.

&#x20;

## Stream generations

&#x20;

Each successful capture start receives a monotonically increasing generation.

A browser uses the generation to distinguish:

&#x20;

```text
temporary HTTP interruption
```



from:

```text
a genuinely new Media Foundation capture session
```

&#x20;

A Stop followed by Start creates a new generation. The browser then discards  
the old MediaSource state and rebuilds the MSE session using the new  
initialization segment.

Rolling recording files do **not** change the stream generation.

&#x20;

## Rolling MP4 recording

&#x20;

The sample records the live stream without creating another Media Foundation  
encoder.

&#x20;

The architecture is:

```text
camera + microphone
        |
        v
ONE H.264/AAC fragmented MP4 writer
        |
        v
custom IMFByteStream
        |
        +--> continuous patched fragments --> HTTP / MSE
        |
        +--> copied patched fragments
                 |
                 +--> recording-only tfdt rebasing
                          |
                          +--> rolling MP4 files
```

Each recording file contains:

```text
ftyp + moov
moof + mdat
moof + mdat
moof + mdat
...
```

&#x20;

The same already encoded media payload used for live playback is reused for the  
recordings.

&#x20;

### Why the recording fragments are patched

&#x20;

The original raw Media Foundation fragments can contain  
`tfhd.base\\\_data\\\_offset` values tied to positions in the original continuous  
byte stream.

Copying those raw fragments directly into another file produces invalid  
standalone fragmented MP4 files.

The recording path therefore uses the same structurally self-contained  
fragments prepared for MSE.

&#x20;

### Per-file recording timeline

&#x20;

The live browser timeline must remain continuous for the entire capture  
session.

A newly rolled MP4 file should instead begin near media time zero.

The sample therefore keeps a separate recording decode-time base for each  
track. When a new recording starts, the first video and audio `tfdt` values are  
used as that file's bases and subsequent fragment decode times are rebased  
relative to them.

This does not modify the browser's continuous live timeline.

&#x20;

### Default recording policy

&#x20;

The current defaults are:

```text
Recording file length: 60 minutes
Maximum retained files: 48
```

With the default one-hour duration, this provides up to approximately 48 hours  
of recordings.

Old timestamped recording files are deleted after the configured retention  
count is exceeded.

&#x20;

## Media Foundation shutdown order



Fragmented MP4 shutdown order matters because the media sink can still hold
references to the byte stream while the Sink Writer is being finalized.

The sample preserves this order:

1. Finalize the Sink Writer;
2. Release the Sink Writer;
3. Shut down the media sink;
4. Release the media sink;
5. Flush the byte stream;
6. Close the byte stream;
7. Release byte-stream, observer, and Source Reader interfaces.

&#x20;

Changing this order can cause shutdown errors, incomplete output, or Media  
Foundation lifetime problems.

&#x20;

## Capture restart

&#x20;

Camera and microphone `IMFActivate` objects are retained while a capture  
session is active.

Before restarting a device:

1. Release the Source Reader;
2. Call `IMFActivate.ShutdownObject`;
3. Release the activation object;
4. Create a fresh Source Reader on the next Start.

&#x20;

This avoids restarting a Source Reader whose underlying media source has  
already entered the shutdown state.

&#x20;

## Local network streaming

&#x20;

For another device on the same LAN, use the sender PC's LAN IPv4 address:

```text
http://192.168.x.x:8080/
```

&#x20;

Windows Firewall must allow inbound TCP traffic to the selected HTTP port.

VPN routing, guest Wi-Fi, wireless client isolation, or a Windows network  
classified as public can prevent another LAN device from reaching the server.

&#x20;

## Direct Internet streaming

&#x20;

Adds a configurable HTTP listen port so the same built-in server  
can be tested across the public Internet.

A typical direct IPv4 arrangement is:

```text
remote browser
      |
      | public-IP:18080
      v
home router
      |
      | TCP port forwarding
      v
sender-LAN-IP:8080
      |
      v
MfSimpleWebCamStreamer
```

For example:

```text
External TCP port: 18080
Internal IP:        192.168.50.x
Internal port:      8080
```

&#x20;

The external and internal port may also be identical.

For a real WAN test, disable Wi-Fi on a phone and connect over 4G/5G. Testing  
the public address from inside the same LAN depends on whether the router  
supports NAT loopback.

&#x20;

### NAT and CGNAT

&#x20;

TCP ports 80, 443, 8080, and similar ports are conventional service ports, but  
they are not automatically forwarded through NAT.

A router receiving a new Internet connection needs to know which private LAN  
machine should receive it.

Direct inbound IPv4 streaming therefore normally requires an explicit router  
mapping unless the sender is directly publicly addressed.

If the Internet provider uses carrier-grade NAT, ordinary home-router port  
forwarding may not be sufficient.

The simple sample deliberately does not use UPnP, NAT-PMP, PCP, a cloud relay,  
or an external rendezvous service.

Those techniques are better demonstrated by a separate advanced networking  
sample rather than complicating this starter.

&#x20;

## Internet security

&#x20;

The current direct-WAN support is a transport demonstration.

The built-in HTTP server currently has no authentication and no TLS/HTTPS.

Do not leave a router port forward permanently exposed to the public Internet  
without adding suitable access control and transport security.

Direct LAN use on a trusted private network does not have the same exposure as  
publishing the port globally.

&#x20;

## Threading

&#x20;

Media capture is callback driven. The sample does not use a polling timer to  
request camera or microphone samples.

The asynchronous Source Reader callbacks receive the samples and feed the  
Media Foundation writer.

The built-in HTTP server runs on its own worker thread.

Recording file writes are deliberately kept outside the Media Foundation  
capture callback. Encoded recording fragments are placed into a bounded queue,  
and the VCL status timer drains that queue to the current recording file.

This prevents disk I/O and recording rollover from blocking the media capture  
callback.

The VCL timer is used only for diagnostics and recording housekeeping, not for  
media capture timing.

&#x20;

## Diagnostics

&#x20;

The application's log memo reports useful state including:

* Detected camera and microphone count;
* Capture start and stop;
* Stream generation;
* HTTP server port;
* Initialization-segment size;
* Latest fragment size;
* Fragment count;
* Total observed fMP4 bytes;
* Recording file creation and rollover;
* HRESULT failures.

The browser diagnostics page reports:

* Current generation;
* Fragment window;
* Next requested sequence;
* Buffered duration;
* Live drift;
* Retry count;
* Approximate bitrate;
* Current playback health;
* Fragment append activity;
* SourceBuffer pruning;
* HTTP recovery;
* MSE rebuilds after a new generation.

&#x20;

Together, these logs make it possible to distinguish capture, encoder, HTTP,  
network, and browser-buffering problems.

&#x20;

## Troubleshooting

&#x20;

### Localhost works, but another LAN device cannot connect

Verify the sender's LAN IPv4 address and Windows Firewall rule.

Use:

```text
http://<sender-LAN-IP>:<HTTP-port>/
```

not `127.0.0.1`, which always refers to the device running the browser.

Also check VPNs, guest Wi-Fi, client isolation, and Windows network profile  
settings.

&#x20;

### The browser opens but video does not start

Check the browser log.

Confirm:

* MSE reports the H.264/AAC MIME type as supported;
* `init.mp4` is appended successfully;
* media fragments are being appended;
* the startup buffer reaches the required duration.

&#x20;

A normal startup includes an initial seek into the actual buffered media  
timeline.

&#x20;

### Playback eventually falls behind live

Inspect the reported live drift and fragment arrival rate.

Occasional network or scheduling jitter is normal. The browser performs a  
live-edge correction only after drift exceeds the configured threshold.

Frequent corrections indicate that fragment delivery is not keeping pace with  
real time.

&#x20;

### Playback stops after HTTP interruption

The browser retries failed requests and then waits for either same-generation  
HTTP recovery or a new stream generation.

Use **Resume HTTP** after the diagnostic **Pause HTTP** test.

For a real network outage, confirm that the server is reachable again and that  
the retained fragment window has not been exhausted.

&#x20;

### Capture works once but cannot Start again

Verify that the Source Readers are released and the retained `IMFActivate`  
objects receive `ShutdownObject` before the next device activation.

Reusing a shut-down Media Foundation source can return `MF\\\_E\\\_SHUTDOWN`.

&#x20;

### Recording files do not play

A recording must contain the initialization segment followed by the  
self-contained patched fragments.

Do not archive the original raw Media Foundation fragments directly because  
their `tfhd.base\\\_data\\\_offset` values can refer to positions in the original  
continuous byte stream.

Each rolled recording must also use its own rebased `tfdt` timeline.

&#x20;

### Recording rollover interrupts MSE

The recording path must not create or finalize a second H.264/AAC Sink Writer.

The current implementation uses one permanent Media Foundation writer and  
reuses its already encoded fragments for both MSE and recording.

&#x20;

### The Internet address is unreachable

Check the complete path:

```text
public-IP:external-port
        |
        v
router forwarding rule
        |
        v
sender-LAN-IP:internal-port
        |
        v
MfSimpleWebCamStreamer
```

Also verify Windows Firewall and test from a genuinely external network such as  
4G/5G.

If the router's WAN address is private or carrier-grade NAT space, direct  
inbound IPv4 may not be available.

&#x20;

## Design scope

`MfSimpleWebCamStreamer` is intentionally a starter sample.

It demonstrates:

```text
capture
encode
fragment
patch
publish
play
record
recover
```

inside one small Windows application.

It deliberately does not attempt to become a cloud camera platform.

Automatic NAT traversal, relay infrastructure, account management, TLS  
certificate provisioning, remote authentication, and rendezvous services are  
better implemented in a separate advanced sample when required.

&#x20;

## Related documentation

* `MfPack\\\\README.md` — MfPack overview and build information;
* `MfPack\\\\Samples` — additional Media Foundation examples;
* `MfSimpleWebCamStreamer` source units — capture, fMP4 byte-stream, HTTP, and
browser implementation.

&#x20;

Project: Media Foundation — MfPack Samples

&#x20;
Project location: [https://github.com/FactoryXCode/MfPack](https://github.com/FactoryXCode/MfPack)  
SourceForge: [https://sourceforge.net/projects/MFPack](https://sourceforge.net/projects/MFPack)

&#x20;

First release date: 27/08/2026

&#x20;
Copyright © FactoryX. All rights reserved.

&#x20;


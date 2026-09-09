# FxServe White Paper
  
Version 1.2  
   
Platform: Windows 10 and higher.

## Summary

FxServe is a small Windows web server made for RDJ and RDJ Pro.  
It serves the FactoryX Radio web application, artwork, JSON metadata, and live media files.  
It also provides public HTTPS and can proxy selected routes to a local media server.  
  
A third-party application like Caddy can do the same job and offers more web-server features.  
Those extra features are useful for larger web setups, but most RDJ installations only need  
a small and fixed set of functions. FxServe covers that set without trying to be  
a general-purpose web server.  
  
FxServe is part of MfPack because it completes the browser-broadcast path used by  
the RDJ sample applications. Media Foundation creates the media, RDJ publishes  
it, FxServe delivers it, and the browser plays it.  
  
## Publishing
  
RDJ and RDJ Pro can publish a broadcast to web browsers.  
The browser needs more than the audio or video encoder output:  
  
- A HTML application;
- Artwork and icons;
- Now-playing information;
- A live-stream manifest;
- Fragmented MP4 files for RDJ Pro;
- Byte-range support for media requests;
- Correct content types;
- Public HTTPS;
- A stable and protected Windows service that starts with the remote server.
  
A full web server handles all of this, but it also includes many features that a  
normal RDJ station does not use. FxServe provides the smaller set needed by the  
FactoryX Radio applications.  

## Design

FxServe has been designed for:

1. Keeping installation and configuration simple.
2. Running as a normal Windows service.
3. Serve the RDJ and RDJ Pro web applications correctly.
4. Support LAN testing without public internet access.
5. Support public HTTP and HTTPS.
6. Renew HTTPS certificates automatically.
7. Use Windows APIs and small native Delphi applications where possible.
  
## What FxServe provides
  
| Function | Purpose |
| --- | --- |
| Static files | Serves HTML, JavaScript, CSS, images, icons, and JSON files. |
| Live fMP4 files | Serves `init.mp4` and `patched_frag_*.m4s` files from RDJ Pro. |
| Byte ranges | Allows browsers to request part of a media file. |
| Content types | Sends the correct type for HTML, JSON, images, MP4, and other web files. |
| Cache control | Stops live manifests and changing media data from being held in a browser cache. |
| CORS headers | Allows supported browser clients to request stream resources. |
| LAN listener | Provides direct access on TCP port 8080. |
| WAN listener | Provides public HTTP on port 80 and HTTPS on port 443. |
| HTTPS redirect | Sends normal HTTP visitors to the secure HTTPS address. |
| Internal certificate manager | Obtains, renews, validates, installs, and binds the HTTPS certificate. |
| In-memory ACME challenge | Keeps certificate validation available on HTTP port 80 without challenge files or scripts. |
| Route proxy | Sends selected RDJ routes to a local service such as Icecast. |
| Logging | Writes server activity and errors to the configured log file. |
  
## RDJ signal path
  
Standard RDJ uses Icecast for the live audio stream. FxServe hosts the browser
application and sends the public `/live` request to Icecast.

```text
Audio devices
     |
     v
    RDJ ---------------------> nowplaying.json and artwork
     |                                      |
     | encoded audio                        v
     v                                  FxServe
 Icecast:8000/live <-------------------------|
                                            |
                                            v
                              HTTPS browser listeners
```

The default FxServe proxy routes are:

```text
/live
/status
/video.mjpg
```

The normal upstream address is `127.0.0.1:8000` when Icecast runs on the same
server as FxServe.

## RDJ Pro signal path

RDJ Pro does not need Icecast for its browser fMP4 broadcast. It writes the web
metadata, artwork, initialization segment, media fragments, and live manifest to
the shared FxServe web root.

```text
Camera, audio devices, decks, microphone, and loopback sources
                              |
                              v
                           RDJ Pro
                              |
              Media Foundation fMP4 output
                              |
                              v
            \\Server\FxServe\www\Stream
                              |
                              v
                           FxServe
                              |
                              v
                  HTTPS browser listeners
```

The main live files are:

| File | Use |
| --- | --- |
| `stream/rdj_stream.html` | Browser MSE player and diagnostic page. |
| `stream/live.json` | Current session, fragment window, codec, and timing information. |
| `stream/init.mp4` | MP4 initialization data required by Media Source Extensions. |
| `stream/patched_frag_*.m4s` | Rolling live audio/video fragments. |
| `nowplaying.json` | Artist, title, show, and station information. |
| `Artwork/hero.jpg` | Main station image used by the web application. |
| `Artwork/cover.jpg` | Current programme or track artwork. |

## Server layout

The standard server layout is:

```text
C:\FxServe
|   FxServe.exe
|   FxServe.ini
|   Install-FxServe.cmd
|
+---www
    |   index.html
    |   nowplaying.json
    |   manifest.json
    |
    +---Artwork
    |       hero.jpg
    |       cover.jpg
    |
    +---icons
    |
    +---Stream
            rdj_stream.html
            live.json
            init.mp4
            patched_frag_*.m4s
```

The shared publishing path is:

```text
\\ServerName\FxServe
```

RDJ Pro writes its stream files to:

```text
\\ServerName\FxServe\www\Stream
```

## LAN and WAN listeners

FxServe separates local access from public access.

| Listener | Port | Use |
| --- | ---: | --- |
| LAN | 8080 | Local testing and server-to-server requests. |
| HTTP | 80 | Certificate challenges and redirecting visitors to HTTPS. |
| HTTPS | 443 | Public Radio website and stream. |

The public listeners use Windows HTTP Server API, also called HTTP.sys. FxServe
stores HTTPS certificates in the Windows Local Machine certificate stores and
maintains the SNI binding for the configured public host name and HTTPS port.
  
The router forwards public TCP ports 80 and 443 to the Windows server.  
Public port 8080 should not be forwarded.  
  
## Request flow
  
```text
Internet request
       |
       v
Router ports 80/443
       |
       v
Windows HTTP.sys
       |
       v
FxServe WAN listener
       |
       +---- HTTP site request ----> HTTPS redirect
       |
       +---- ACME challenge -------> in-memory token response
       |
       +---- HTTPS request --------> LAN listener on 127.0.0.1:8080
                                          |
                                          +--> web or stream file
                                          |
                                          +--> selected proxy route
```

Using the LAN listener behind the WAN listener keeps file serving in one place.
The same web root and response rules are used for local and public requests.

## Configuration

FxServe reads `FxServe.ini`. A normal setup contains these sections:

| Section | Use |
| --- | --- |
| `[Server]` | LAN address, port, web root, limits, and timeouts. |
| `[Wan]` | Public host name, HTTP/HTTPS ports, and redirect setting. |
| `[Proxy]` | Optional upstream server and public proxy routes. |
| `[Headers]` | CORS and no-cache routes. |
| `[Logging]` | Log file path and daily retention period. |
| `[Protection]` | Optional per-address request rate, burst, concurrency, and cooldown limits. |

The INI contains ordinary server and public-listener configuration only. It
does not contain ACME account information, certificate thumbprints, private-key
material, or renewal state. RDJ and RDJ Pro therefore never need access to
certificate-management data.

Example:

```ini
[Server]
BindAddress=0.0.0.0
Port=8080
WebRoot=C:\FxServe\www
IndexFile=index.html
MaxConnections=128

[Wan]
Enabled=True
HostName=radio.example.com
HttpEnabled=True
HttpPort=80
HttpsEnabled=True
HttpsPort=443
RedirectHttp=True

[Proxy]
Enabled=True
Host=127.0.0.1
Port=8000
Routes=/live,/status,/video.mjpg

[Headers]
Cors=True
NoStoreRoutes=/stream,/video,/nowplaying.json

[Logging]
File=C:\FxServe\FxServe.log
RetentionDays=14

[Protection]
Enabled=False
RequestsPerMinute=300
Burst=60
MaxConcurrentPerAddress=12
BlockSeconds=60
```

Request protection is opt-in. When enabled, a token bucket is maintained for
each IPv4 or IPv6 peer at the public HTTP.sys listener. The bucket refills at
`RequestsPerMinute` and holds at most `Burst` requests. The concurrent limit
prevents one address from occupying too many request workers. A rate violation
starts the configured cooldown and returns HTTP `429` with `Retry-After`.
Active ACME HTTP-01 validation responses bypass this limiter. The internal
HTTP.sys-to-LAN proxy request is never charged again. If WAN mode is disabled,
the same limiter protects direct requests to the LAN listener. Independently,
`[Server] MaxConnections` caps the total number of active WAN request workers.

### Live configuration reload

Console and service operation both monitor the selected INI file. A changed file
must remain stable for one second before FxServe reads it, preventing an editor's
intermediate write from being treated as a complete configuration. The new file
is fully parsed and validated before the running listeners are stopped.

Valid changes restart the LAN listener and optional HTTP.sys/TLS front end in
the existing process, thereby applying all configuration sections consistently.
Invalid files leave the active runtime untouched. If a validated configuration
cannot acquire its requested resources, FxServe records the failure and attempts
to restore the previous runtime. A reload creates a short connection interruption;
streaming clients reconnect through their normal retry behavior.

## HTTPS certificates

FxServe contains its own ACME v2 client and manages the Let's Encrypt
certificate lifecycle inside the Windows service. No separate ACME program,
scheduled task, certificate script, or binding utility is required.

The renewal flow is:

1. FxServe checks the installed certificate on startup and every six hours.
2. When no valid certificate exists, or expiry is within 30 days, FxServe
   creates a non-exportable Windows machine key and starts an ACME order.
3. FxServe keeps the HTTP-01 token only in memory and serves it through HTTP
   port 80.
4. The certificate authority validates the public host name.
5. FxServe installs the leaf and intermediate certificates in the Windows Local
   Machine stores and verifies the DNS name, validity, and private key.
6. FxServe updates the HTTP.sys SNI binding for its configured HTTPS port.

Normal HTTP requests still move to HTTPS. Only the ACME challenge path stays on
HTTP so certificate checks can complete.

Certificate renewal does not need PowerShell, a user-accessible certificate
file, or an FxServe restart. Internal state is held below the protected
`HKLM\Software\FactoryX\FxServe\Certificate` key; its ACL permits only SYSTEM
and local Administrators. Enabling WAN HTTPS opts the installation into the
certificate authority terms of service.

The ACME account key is persistent so FxServe can continue using the same
certificate-authority account. A new certificate key is created for each
successful issue or renewal. Neither key is marked exportable. If an ACME
attempt fails before installation, FxServe removes the unused certificate-key
container and keeps any currently valid certificate and binding in service.

The first certificate request requires all of the following:

- the public DNS name resolves to the router's current public address;
- public TCP port 80 reaches the FxServe WAN HTTP listener;
- the WAN HTTP listener is enabled; and
- no other web server owns FxServe's HTTP port.

After issuance, public TCP port 443 must reach the configured FxServe WAN HTTPS
listener for browser access.

FxServe checks for a usable certificate when it starts and then at the internal
check interval. A certificate must be current, match the configured DNS name,
and have an accessible private key before FxServe binds it. The certificate
manager writes progress and errors to the normal FxServe log.

## Windows services and administration

FxServe runs as the `FxServe` Windows service. FxServeAdmin normally runs on the
RDJ workstation and manages FxServe through the Windows Service Control Manager
and the restricted FxServe share. It does not run the ACME client, receive
private keys, or modify HTTP.sys certificate bindings.

FxServeAdmin can:

- install and remove the service registration;
- start, stop, and restart FxServe;
- open the shared folder and log file;
- edit `FxServe.ini` and save a backup;
- apply the selected WAN hostname to `FxServe.ini` and publish non-sensitive
  server/public URL metadata in `www\fxserve-config.json`;
- check the LAN live manifest;
- check the public HTTPS website.

FxServeAdmin requires its application password at startup, following the same
administrative protection model as CaddyAdmin. Its saved connection profile is
separate from FxServe's protected machine-wide certificate state.

A healthy setup reports:

```text
Live manifest: OK (HTTP 200)
Public HTTPS: OK (HTTP 200)
```

## Using Caddy as an alternative

FxServe does not remove Caddy support. A station can keep Caddy installed and
choose either server.

Only one server can own ports 80 and 443 at a time. FxServe Admin manages only
FxServe, while CaddyAdmin manages Caddy and its additional settings. When
changing servers, stop the active server with its own administration tool
before starting the other one.

| FxServe | Caddy |
| --- | --- |
| Small RDJ-focused server | General-purpose web server |
| Simple INI configuration | Flexible Caddyfile configuration |
| Fixed web root and proxy routes | Many sites and advanced routing |
| Native Windows service | Cross-platform application |
| HTTPS through HTTP.sys and built-in ACME | Built-in automatic HTTPS |
| Best for a dedicated RDJ station | Best for a larger web setup |

## Security model

FxServe keeps the public surface small:

- the public host name must match the configured host;
- public requests support the methods needed for normal browser playback;
- Windows HTTP.sys handles the TLS connection;
- certificates stay in the Windows machine certificate stores and private keys
  are non-exportable machine keys;
- certificate-management registry data is readable only by SYSTEM and local
  Administrators;
- HTTP-01 tokens exist only in memory for the lifetime of an active challenge;
- FxServe verifies a certificate before binding it to HTTP.sys;
- live JSON and stream routes can use no-cache headers;
- Icecast can remain on the private network behind the selected proxy routes; and
- RDJ publishes through a Windows share that can be limited to the broadcast account.

Windows Firewall should allow the required ports only. Remote service management
should be available only to trusted administrator accounts on the local network.

## Failure handling

FxServe keeps the main failure cases visible:

| Problem | Result or check |
| --- | --- |
| RDJ Pro is not publishing | `live.json` is missing, old, or reports no live session. |
| LAN listener is unavailable | The FxServeAdmin live-manifest check fails. |
| Public HTTPS is unavailable | The FxServeAdmin public check fails. |
| Initial certificate issue fails | Check FxServe.log, public DNS, router forwarding, firewall access, and the WAN HTTP listener. |
| Wrong web root | The application or stream files return HTTP 404. |
| Wrong RDJ output folder | Files appear in another folder and the live manifest does not update. |
| Certificate renewal fails | FxServe records the error in its log and the current valid certificate remains bound. |
| WAN HTTP is disabled | FxServe can use an existing valid certificate but cannot complete a new HTTP-01 issue or renewal. |
| Protected host and WAN host differ | FxServe refuses to request or bind a certificate and records the mismatch. |
| Ports are already in use | The second web service cannot start. |

The standard health URLs are:

```text
http://ServerName:8080/stream/live.json
https://radio.example.com/
```

## Limits

FxServe is not meant to replace every web server. The current scope does not aim
to provide:

- hosting for many unrelated websites;
- a large plugin system;
- advanced load balancing;
- complex rewrite rules;
- containers or Linux service packages; or
- a general web-server administration platform.

Use Caddy, IIS, nginx, Apache, or another full web server when those features are
needed.

## Why FxServe belongs in MfPack

MfPack provides Delphi access to Microsoft Media Foundation and related Windows
media APIs. RDJ and RDJ Pro use those APIs in a complete broadcasting workflow.
The browser output is the last part of that workflow.

FxServe fits MfPack because it:

- supports the media formats produced by the RDJ samples;
- shows a practical HTTP.sys service written in Delphi;
- makes the RDJ Pro fMP4/MSE example easier to install;
- removes an oversized dependency from the basic setup;
- keeps a full web server available for users who need one; and
- stays focused on FactoryX media delivery instead of general web hosting.

## Recommended use

Use FxServe for a normal RDJ or RDJ Pro station with one FactoryX Radio website.
Use Caddy when the same server must handle more websites, custom routing, or
other web applications.

Both choices use the same public router ports and the same FactoryX Radio browser
application. The difference is the size and range of the web server behind them.

## Related documents

- [RDJ Web Server Setup](RDJ-Web-Server-Setup.md)
- [FxServe README](README.md)
- [FxServe deployment guide](Deploy/YourRemoteServer/README-Deploy.md)
- [FxServeAdmin README](FxServeAdmin/README.md)
- [RDJ User Manual](../Samples/MfRdjJ/Docs/RDJ-User-Manual.md)
- [RDJ Pro User Manual](../Samples/MfRdjPro/Docs/RDJ-Pro-User-Manual.md)

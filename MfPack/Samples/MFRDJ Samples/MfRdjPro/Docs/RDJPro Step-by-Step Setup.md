# RDJPro Step-by-Step Setup

This guide explains how to set up the RDJPro example from a clean Delphi environment.

Important: install the required Delphi components before opening the RDJPro project in Delphi. If you open the project first, the IDE may report missing controls/forms.

## 1. Requirements

- Windows 10 or later.
- Delphi XE7 or later.
- MfPack source tree available locally.
- Media Foundation available on the system.
- FxServe or Caddy, if you want to use the browser broadcast page.

FxServe is the normal choice for a small RDJ Pro server. Caddy is also supported.
See [RDJ Web Server Setup](../../../FxServe/RDJ-Web-Server-Setup.md) for a short
comparison and the current server paths.

## 2. Install Required Delphi Packages First

Before opening `MfRdjJ.dproj`, install these packages in Delphi:

1. Open Delphi.
2. Open and build/install:

```text
MfPack\Samples\MfComponents\MfComponents.dpk
```

3. Open and build/install:

```text
MfPack\Samples\MfRdjPro\Controls\RDJControls.dpk
```

4. Close Delphi after installing the packages.
5. Start Delphi again so the installed controls are loaded cleanly.

## 3. Open The RDJPro Project

Open:

```text
MfPack\Samples\MfRdjPro\MfRdjJ.dproj
```

## 4. Set The Delphi Search Paths

In the RDJPro project options, add these paths to the Delphi search path:

```text
..\..\src
..\MfComponents
.\Controls
```

If your folder layout is different, use the equivalent absolute paths, for example:

```text
D:\PROJECTS\MfPack\src
D:\PROJECTS\MfPack\Samples\MfComponents
D:\PROJECTS\MfPack\Samples\MfRdjPro\Controls
```

## 5. Check The Recommended Project Settings

Before building, check the RDJPro project options and make sure they follow the recommended settings from `readme.txt`.

In Project Options, set:

```text
Output directory: .\$(Platform)\$(Config)
Unit output directory: .\$(Platform)\$(Config)
```

These settings keep the executable and generated DCU files in the matching platform/configuration folder, for example `.\Win32\Debug`.

## 6. Build RDJPro

Build the project for Win32 first.

Recommended first build:

```text
Platform: Win32
Configuration: Debug
```

The project should build without errors. Fix missing package/search-path issues before continuing.

## 7. First Run

Run RDJPro.

On first start, RDJPro creates its setup `.ini` file and opens the setup dialog.

## 8. Configure General Audio

In the General setup page:

1. Select the main output device. This is the master/PA output.
2. Enable headphone/PFL output if needed.
3. Select the headphone output device.
4. Choose the number of channel decks.
5. Choose the number of loopback decks.
6. Set the capture buffer size.
7. Enable and select the microphone input if needed.

## 9. Configure Recording

In the recorder settings:

1. Select the recorder output folder.
2. Choose the required recording format.
3. Set capture buffer size and latency.
4. Enable stream-switch detection if needed.
5. Enable the option to avoid overwriting active files if desired.

## 10. Install FxServe or Caddy For Browser Broadcast

If you want browser broadcasting, install FxServe or Caddy. The next sections
describe the older Caddy layout. For FxServe, use this server folder:

```text
C:\FxServe
```

Set the RDJ Pro web paths to:

```text
Root path:              \\ServerName\FxServe\www
Config file:            \\ServerName\FxServe\FxServe.ini
NowPlaying JSON file:   \\ServerName\FxServe\www\nowplaying.json
Artwork path:           \\ServerName\FxServe\www\Artwork
Video/stream path:      \\ServerName\FxServe\www\Stream
```

Run `Install-FxServe.cmd`, then use FxServeAdmin to start FxServe and stop the
alternative proxy. Continue with the Caddy steps below only when Caddy is your
chosen web server.

1. Unpack the Caddy package.
2. Copy the Caddy web files to a folder such as:

```text
C:\Caddy
```

3. Make sure the folder contains at least:

```text
caddy.exe
caddy.cff
index.html
nowplaying.json
artwork\
stream\
```

## 11. Configure Caddy In RDJPro

Open the RDJPro setup dialog and configure the Caddy paths.

For a local Caddy setup:

```text
Caddy path:              C:\Caddy
Caddy config file:       C:\Caddy\caddy.cff
NowPlaying JSON file:    C:\Caddy\nowplaying.json
Artwork path:            C:\Caddy\artwork
Video/stream path:       C:\Caddy\stream
Caddy command:           caddy.exe run --config "C:\Caddy\caddy.cff" --adapter caddyfile
Log file:                Caddy.log
```

For a Caddy server on another machine, use UNC paths, for example:

```text
\\ServerName\Caddy
\\ServerName\Caddy\nowplaying.json
\\ServerName\Caddy\artwork
\\ServerName\Caddy\stream
```

## 12. Enable Caddy Access Logging

Listener count depends on the Caddy access log. The Caddy config should contain:

```caddyfile
log {
    output file C:\Caddy\Caddy.log
    format json
}
```

Restart Caddy after changing the config.

## 13. Start Caddy

Start Caddy with the configured command, for example:

```text
caddy.exe run --config "C:\Caddy\caddy.cff" --adapter caddyfile
```

If Caddy runs on another machine, start it on that machine.

## 14. Router And HTTPS Setup

For public internet access:

1. Forward HTTP port 80 to the web-server machine.
2. Forward HTTPS port 443 to the web-server machine.
3. Configure your DNS or DDNS hostname.
4. Let Caddy manage its certificate, or run `Install-Renewal.cmd` once for FxServe.

For local testing, this step is not required.

## 15. Start Broadcasting

In RDJPro:

1. Load or start your decks.
2. Open the media/broadcast server dialog.
3. Select the camera or static video source if video is required.
4. Enable broadcast.
5. Open the FxServe or Caddy web page in a browser.
6. Confirm that audio/video plays and now-playing metadata updates.

## 16. Verify The Browser Files

During broadcast, the selected stream folder should update with files like:

```text
init.mp4
live.json
patched_frag_000001.m4s
patched_frag_000002.m4s
```

The selected web root should update:

```text
nowplaying.json
```

## 17. Recommended Long-Run Settings

For long broadcasts:

1. Disable Windows sleep or enable the RDJPro sleep override option.
2. Do not close the laptop lid unless the lid action is configured correctly.
3. Keep FxServe or Caddy running.
4. Make sure the selected stream folder is writable.
5. Watch RDJPro debug/memory heartbeat messages during testing.

## 18. Common Problems

Missing controls in Delphi:

Install `MfComponents.dpk` and `RDJControls.dpk` before opening the project.

Project does not compile:

Check the Delphi search paths.

Browser shows no metadata:

Check the `nowplaying.json` path.

Browser does not play video/audio:

Check that `live.json`, `init.mp4`, and `patched_frag_*.m4s` are being written to the selected stream folder.

Listener count stays at zero:

Check that Caddy JSON access logging is enabled and that RDJPro points to the correct Caddy log file.

Broadcast stops after sleep:

Check Windows power settings and the RDJPro sleep override setting.

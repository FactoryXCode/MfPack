# MfRDJ Installation & Setup Guide

> **Web-server:** RDJ Pro can use FxServe or Caddy. FxServe is the
> smaller RDJ-focused choice; Caddy remains available for larger or more complex
> web setups. See [RDJ Web Server Setup](../../../FxServe/RDJ-Web-Server-Setup.md)
> for the current paths and short installation steps. The Caddy examples below
> remain available for Caddy users.

---

# Overview

MfRDJ supports two setup modes:

## Local Setup (Single PC)

All components run on one machine:

* MfRDJ
* Caddy

**Use this for:**

* Testing
* Development
* Private use on your local network.

---

## Server Setup (Network / Internet)

Caddy run on a **server PC**, accessible by:

* Local network (LAN)
* Internet (public users)

Use this for:

* Radio stations
* Public streaming
* Multi-device access

---

# Network Basics (IMPORTANT)

| Situation     | Host                |
| ------------- | ------------------- |
| Same PC       | `127.0.0.1`         |
| Local Network | `192.168.x.x`       |
| Internet      | Domain or Public IP |

---

## Explanation

### 127.0.0.1

* Refers to **your own PC**
* Only works locally

---

### 192.168.x.x

* Local network address
* Works on devices in same network

---

### Domain / Public IP

* Required for internet streaming
* Example:

```text
yourradio.yourdomain.com
```

---

# Caddy Setup (Web + PWA)

1. Extract:

```text
..\MfPack\Samples\MfRdjJ\Binaries\Caddy_PWA.zip
```

2. Copy to:

```text
C:\Caddy
```

---

# Windows Firewall (SERVER PC)

## Must allow:

### Applications:

* `caddy.exe`

### Ports:

| Port | Purpose |
| ---- | ------- |
| 8000 | Stream  |
| 80   | HTTP    |
| 443  | HTTPS   |

**Enable for:**

* Private
* Public

---

# Router Setup (Internet Streaming)

Enable **Port Forwarding**:

| External | Internal | IP        |
| -------- | -------- | --------- |
| 80       | 80       | Server IP |
| 443      | 443      | Server IP |

---

# DDNS (Recommended)

Example (ASUS):

```text
factoryxradio.asuscomm.com
```

Enable:

* DDNS
* Let's Encrypt SSL

---

#  Server PC vs User Device

##  Server PC

Runs:

* Caddy

Must have:

* Firewall configured
* Ports open
* Correct IP/domain

---

## User Device *(like smartphone or pc)*

Only:

* Opens webpage / PWA
* Plays stream

Note: No firewall setup needed

---

# MfRDJ Project Setup

## Install Components

* MfPack MfComponents
* RDJ Pro Controls

## Add Search Paths

```text
..\MfPack\src
..\MfPack\Samples\MfRdjPro\Controls
..\MfPack\Samples\MfComponents
```

## Output

```text
.\$(Platform)\$(Config)
```

---

# FireDAC and SQLite

RDJ Pro uses **FireDAC** to store the music library and playlists in an
**SQLite** database. FireDAC is a Delphi framework; it is not an RDJ Pro
component package and does not need to be installed into the Tool Palette.

## Install FireDAC in Delphi

FireDAC must be present in the Delphi installation used to compile RDJ Pro. If
Delphi reports that a unit such as `FireDAC.Comp.Client` or
`FireDAC.Phys.SQLite` cannot be found, close Delphi and use the Delphi installer
or feature manager to add FireDAC. Do not solve this by copying FireDAC DCU
files from a different Delphi version.

## SQLite used by the current sample

The current RDJ Pro source uses FireDAC's built-in SQLite support. Normally no
separate `sqlite3.dll` is required. On first use, RDJ Pro creates the database
and its tables automatically at:

```text
<folder containing MfRdjJ.exe>\Data\RDJLibrary.db
```

For example, a Win32 Debug build normally uses:

```text
MfRdjPro\Win32\Debug\Data\RDJLibrary.db
```

Open **Playlist Composer** and use **Scan Folder** to add audio files. Scanning
again adds new files and updates known files; it does not require recreating the
database.

## Optional external SQLite DLL

The shared RDJ redistribution folder contains SQLite 3.53.0 for applications
that are deliberately configured to load SQLite dynamically:

```text
..\Redist\sqlite-dll-win-x86-3530000.zip   Win32
..\Redist\sqlite-dll-win-x64-3530000.zip   Win64
```

Extract `sqlite3.dll` from the archive matching the application platform and
place it beside `MfRdjJ.exe`. The supplied `.def` file is for development and
does not need to be distributed. Never put the x64 DLL beside a Win32 build, or
the x86 DLL beside a Win64 build.

Copying the DLL alone does not change the current sample from built-in to
dynamic SQLite. A dynamic build must also set the FireDAC SQLite driver's
`VendorLib` to that `sqlite3.dll` before opening the connection. Keep the
built-in configuration unless there is a specific reason to maintain an
external SQLite runtime.

## Database care

RDJ Pro uses SQLite WAL mode, so `RDJLibrary.db-wal` and `RDJLibrary.db-shm` may
appear while it is running. This is normal.

* Close RDJ Pro before backing up or replacing the database.
* Back up the complete `Data` folder, not only a file that is open in RDJ Pro.
* Do not let two RDJ Pro instances write to the same database.
* Keep the live database on a reliable local disk. Do not run it directly from
  an intermittent network share.

---

#  Running MfRdjPro

First run:

* Creates INI
* Opens Setup dialog

---

# General Settings

* Select Master Output
* Enable Headphones (Cue)
* Set buffer: **60 ms**

---

# Microphone

* Enable input
* Select device

---

# Audio Recorder

* Buffer: 60 ms
* Latency: 100 ms

---

# Broadcast Setup

## Local

```text
Host: 127.0.0.1
```

---

## LAN (Server)

```text
Host: 192.168.x.x
```

---

## Internet

```text
Host: yourdomain.com
```

---

## Common Settings

```text
Port: 8000
Mount: /live
User: source
Password: your_password
```

---

# Caddy Setup

```text
C:\Caddy
C:\Caddy\Caddyfile
C:\Caddy\nowplaying.json
```

Run:

```text
 caddy.exe run --config "C:\Caddy\Caddy.cff" --adapter caddyfile
```

---

# Accessing the Stream

## Same PC

```text
http://127.0.0.1:8000/live
```

---

## LAN

```text
http://192.168.x.x:8000/live
```

---

## Internet

```text
https://yourdomain.com
```

---

# Troubleshooting

## Works on PC, not phone

**Check:**

* Firewall
* Router ports

---

## HTTPS not working

**Check:**

* Port 443
* Caddy running
* DDNS

---

## Stream reconnects / unstable

**Possible reasons:**

* Buffer too small
* Network issues

---

# Final Checklist

## Server PC

* [ ] Caddy running
* [ ] Firewall allow Caddy and RdjPro
* [ ] Ports forwarded
* [ ] Correct host

---

## User Device

* [ ] Correct URL
* [ ] Network access

---

# Summary

| Setup    | Host        |
| -------- | ----------- |
| Local    | 127.0.0.1   |
| LAN      | 192.168.x.x |
| Internet | Domain      |

---

- The server does all the work
- A user only connects

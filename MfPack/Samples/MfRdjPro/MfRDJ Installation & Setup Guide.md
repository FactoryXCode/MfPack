# MfRDJ Installation & Setup Guide

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
factoryxradio.asuscomm.com
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

#  Server PC vs 📱 User Device

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

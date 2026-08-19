# MfRDJ Installation & Setup Guide

Version 3.2 - Windows 10/11 - MfRDJ, Icecast and Caddy

This guide explains every field in the MfRDJ Setup form and gives complete working layouts for:

- local playback and testing on one PC;
- playback by listeners on the local network (LAN); and
- public internet playback through a domain name and HTTPS.

The examples use Icecast port `8000`, mount `/live`, Caddy ports `80` and `443`, and the default MfRDJ folders. Replace example names, addresses, paths and passwords with values for your installation.

## 1. Understand the signal path

MfRDJ is the **source client**. It produces audio and sends the encoded stream to Icecast. Icecast accepts the source and distributes it to listeners. Caddy serves the web player, covers and `nowplaying.json`, and can securely proxy `/live` to Icecast.

```text
Audio devices -> MfRDJ -> Icecast:8000/live -> Caddy:443 -> listeners
                         ^ source connection    ^ public web address
```

Do not confuse these three addresses:

1. **Broadcast Host** - where MfRDJ sends the encoded audio.
2. **Icecast Server Manager Host** - where MfRDJ checks Icecast readiness.
3. **Listener URL** - what a listener opens. With Caddy this is normally the HTTPS domain, not the Broadcast Host field.

If MfRDJ and Icecast are on the same PC, the first two addresses can remain `127.0.0.1` even when listeners connect from the LAN or internet.

## 2. Choose a deployment

### 2.1 Local-only test

MfRDJ, Icecast and Caddy all run on the same Windows PC. Only that PC needs to listen.

| Item | Value |
| --- | --- |
| Broadcast Host | `127.0.0.1` |
| Server Manager Host | `127.0.0.1` |
| Icecast listener URL | `http://127.0.0.1:8000/live` |
| Caddy web URL | `http://localhost` or the address configured in `caddy.cff` |
| Icecast/Caddy paths | Local paths such as `C:\Icecast` and `C:\Caddy` |
| Router forwarding | Not required |

### 2.2 LAN playback - all services on the MfRDJ PC

MfRDJ, Icecast and Caddy run on one PC, but phones and other computers listen over the local network.

| Item | Value |
| --- | --- |
| Broadcast Host | `127.0.0.1` |
| Server Manager Host | `127.0.0.1` |
| Listener address | The MfRDJ PC's fixed LAN IPv4 address, for example `192.168.1.50` |
| Direct Icecast URL | `http://192.168.1.50:8000/live` |
| Caddy web URL | For example `http://192.168.1.50` when Caddy is configured for HTTP/LAN use |
| Firewall | Allow the required inbound ports on the Private profile |
| Router forwarding | Not required |

### 2.3 LAN playback - Icecast and Caddy on another PC

MfRDJ runs on the studio PC. Icecast and Caddy run on a server such as `PCHP001`.

| Item | Example |
| --- | --- |
| Broadcast Host | Server IPv4 address, for example `192.168.1.60` |
| Server Manager Host | Same server IPv4 address |
| Icecast/Caddy local paths | Leave process-start paths empty when RDJ must not start remote programs |
| Caddy shared folder | `\\PCHP001\Caddy` |
| Caddy config | `\\PCHP001\Caddy\caddy.cff` |
| Now-playing JSON | `\\PCHP001\Caddy\nowplaying.json` |
| Covers | `\\PCHP001\Caddy\covers` or the server's shared Caddy root |
| Direct listener URL | `http://192.168.1.60:8000/live` |

Use the server's numeric LAN IPv4 address in the Host fields for the most reliable readiness check in the current MfRDJ release. UNC paths use a computer name and backslashes, for example `\\PCHP001\Caddy`; a Host field uses only a hostname or IP address, never slashes.

The Windows account running MfRDJ needs read/write permission on the remote Caddy share so it can update JSON and cover files. Do not expose Windows file sharing/SMB directly to the public internet.

### 2.4 Public internet playback

The recommended design keeps Icecast private and exposes Caddy only. Caddy serves HTTPS and reverse-proxies `/live` and `/status` to Icecast.

| Item | Recommended value |
| --- | --- |
| MfRDJ -> Icecast | `127.0.0.1` if same PC, or the server's private LAN IP |
| Public DNS/DDNS | A domain such as `radio.example.com` pointing to the public router address |
| Router forwarding | TCP `80 -> Caddy PC:80`, TCP `443 -> Caddy PC:443` |
| Public listener URL | `https://radio.example.com` |
| Direct public Icecast port | Do not forward `8000` unless direct Icecast access is deliberately required |
| Windows firewall | Allow Caddy on TCP 80/443; allow Icecast 8000 only on the networks that need it |

If Icecast is hosted outside the studio network, use its DNS name or IP in both Host fields. Use a VPN or another secure transfer method for metadata and cover files. Never publish an SMB share or TCP port 445 to the internet.

## 3. Install the server software

### 3.1 Install Icecast

1. Extract `Binaries\icecast_win64_2.5.0.zip` on the computer that will run Icecast.
2. Copy the supplied `Binaries\IceCast_defXML\icecast.xml` to the Icecast installation and adapt it before use.
3. At minimum, set these XML values:
   - `<hostname>` to the server's appropriate DNS name or IP;
   - `<source-password>` to a new strong password;
   - `<admin-user>` and `<admin-password>` to new credentials;
   - `<listen-socket><port>` to `8000`, unless you intentionally use another port;
   - `<clients>` high enough for the expected listener count.
4. The **Broadcast Password** in MfRDJ must exactly match Icecast's `<source-password>` unless a mount-specific source password is configured.
5. Start Icecast manually and confirm `http://127.0.0.1:8000/` opens on the Icecast PC.

The supplied XML contains example addresses and passwords. Do not use those unchanged on a production or internet-facing server.

### 3.2 Install Caddy and the PWA player

1. Extract `Binaries\Caddy_PWA.zip` to `C:\Caddy` on the Caddy server.
2. Edit `C:\Caddy\caddy.cff`.
3. For public HTTPS, replace the example site name with your real DNS/DDNS name.
4. Keep the web root and Icecast proxy routes aligned with your installation. A typical configuration is:

```caddyfile
radio.example.com {
    root * C:\Caddy
    file_server

    handle /live* {
        reverse_proxy 127.0.0.1:8000
    }

    handle /status* {
        reverse_proxy 127.0.0.1:8000
    }

    handle /admin* {
        respond "Forbidden" 403
    }
}
```

5. Start Caddy from an elevated terminal for the first test:

```text
C:\Caddy\caddy.exe run --config "C:\Caddy\caddy.cff" --adapter caddyfile
```

For a public domain, DNS must point to the router/server and external TCP ports 80 and 443 must reach Caddy. Caddy then obtains and renews the HTTPS certificate automatically.

## 4. General tab - every field

### 4.1 Endpoint devices

| Field | What it controls | Recommended setting |
| --- | --- | --- |
| **Main output (MASTER)** | Windows playback endpoint used for the master/program output. | Select the interface or sound card feeding the PA, monitor speakers or program output. Avoid Bluetooth for reliable low-latency operation. |
| **Enable Headphones output (PFL / CUE)** | Enables a separate pre-fade-listen/cue bus. | Enable only when a second playback endpoint is available. |
| **Cue output (PHONES)** | Windows playback endpoint for cue/PFL audio. | Select the headphone device. Prefer a device different from MASTER. |
| **Enable Microphone input** | Creates and enables the microphone deck. | Enable when live microphone input is required. |
| **Microphone input (MICROPHONE)** | Windows capture endpoint used by the microphone deck. | Select the required microphone/audio-interface input. |

MfRDJ stores Windows endpoint IDs, not only display names. If a USB device is removed, moved to another system, or reinstalled, reopen Setup and select it again.

### 4.2 Mixer layout and real-time buffer

| Field | Range/default | Meaning and advice |
| --- | --- | --- |
| **Channel decks** | 1-8; default 2 | Number of file/player decks created when the mixer is built. More decks use more screen space and resources. |
| **Loopback decks** | 0-4; default 1 | Number of decks that capture audio rendered by other Windows applications/processes. Choose 0 when loopback capture is not used. |
| **Capture buffersize** | 30-120 ms; default 60 ms | Shared real-time audio buffer. Start at 60 ms. Increase toward 80-120 ms if audio crackles or drops; reduce only after stable testing when lower latency is needed. |

Changes to the mixer layout and endpoint routing are safest after restarting MfRDJ.

### 4.3 Audio recorder

| Field | Range/default | Meaning and advice |
| --- | --- | --- |
| **Capture buffersize** | 30-180 ms; recommended 60 ms | Recorder capture buffer. In the current UI, the minimum position (30 ms) enables automatic buffer duration; values above 30 ms request a fixed duration. Use 60 ms as the normal starting point. |
| **Latency** | 0-500 ms; UI default 10 ms | Target recorder latency. Start at 10 ms. Increase gradually if recording is unstable. |
| **Don't overwrite existing files** | On/off | When enabled, MfRDJ adds a number to a duplicate filename instead of replacing the existing recording. Recommended: enabled. |
| **Disable MMCSS** | On/off | Disables Windows Multimedia Class Scheduler priority for recorder threads. Recommended: off/unchecked. Enable only for diagnosis when MMCSS causes a verified compatibility problem. |
| **Default PCM audio output format** | On/off | Requests the default PCM representation for WAV writing instead of the floating-point path. Enable for maximum WAV compatibility; FLAC input is handled as PCM. |
| **Enable stream switch detection** | On/off | Allows the recorder to react when the Windows default audio endpoint changes or the active stream is switched. Recommended when devices may be connected/disconnected while RDJ runs. |
| **Output format** | WAV or FLAC | WAV is broadly compatible and large. FLAC is lossless and uses less disk space but needs FLAC-capable software. |

### 4.4 Application directories

| Field | Default | Purpose |
| --- | --- | --- |
| **Recordings** | `Recordings` | Subfolder in which master recordings are written. The browse button stores the selected full path. The account running MfRDJ needs create/write permission. |
| **Database** | `Data` | Folder for MfRDJ data, including the playlist/library SQLite database. Back up this folder. Do not place a live SQLite database on an unreliable or intermittently connected share. |
| **Covers** | `Covers` | Local cover-art folder used by RDJ. The public Caddy cover destination is configured separately on the Broadcast tab. |

## 5. Broadcast page - simple setup

Choose one setup below and copy its values into the Broadcast page:

- **Local** - MfRDJ, IceCast and Caddy all run on this computer.
- **LAN** - MfRDJ runs on this computer. IceCast and Caddy run on `PCHP001` inside your home or studio network.
- **WAN** - people listen through the Internet. MfRDJ still sends the program to `PCHP001` inside your network.

The LAN and WAN examples use `PCHP001` at `192.168.50.83`. The WAN example uses `radio.example.com`; replace this with your own Internet name. Replace `YOUR_PASSWORD` and the example station text with your own information.

### 5.1 IceCast settings

This section tells MfRDJ where to send the music and what listeners see.

#### Local

| Field | Enter this |
| --- | --- |
| **Host:** | `127.0.0.1` |
| **Port:** | `8000` |
| **Mount:** | `/live` |
| **User name:** | `source` |
| **Password:** | `YOUR_PASSWORD` |
| **Broadcast name:** | Your station name |
| **Description:** | A short description of your station |
| **Genre:** | For example `Various`, `Rock` or `Talk` |
| **Broadcast URL:** | Leave empty |
| **Checkbox Public Stream on/off** | Off |

#### LAN

| Field | Enter this |
| --- | --- |
| **Host:** | `192.168.50.83` |
| **Port:** | `8000` |
| **Mount:** | `/live` |
| **User name:** | `source` |
| **Password:** | `YOUR_PASSWORD` |
| **Broadcast name:** | Your station name |
| **Description:** | A short description of your station |
| **Genre:** | For example `Various`, `Rock` or `Talk` |
| **Broadcast URL:** | Leave empty |
| **Checkbox Public Stream on/off** | Off |

#### WAN

| Field | Enter this |
| --- | --- |
| **Host:** | `192.168.50.83` |
| **Port:** | `8000` |
| **Mount:** | `/live` |
| **User name:** | `source` |
| **Password:** | `YOUR_PASSWORD` |
| **Broadcast name:** | Your public station name |
| **Description:** | A short public description of your station |
| **Genre:** | For example `Various`, `Rock` or `Talk` |
| **Broadcast URL:** | `https://radio.example.com` |
| **Checkbox Public Stream on/off** | Off. Turn it On only if you want the station listed in IceCast's public station directory. |

#### Brief explanation of the fields

| Field | What it means |
| --- | --- |
| **Host:** | The computer running IceCast. Use the value shown above; do not add `http://`. |
| **Port:** | The connection number used by IceCast. The normal value is `8000`. |
| **Mount:** | The short name of your audio stream. Listeners usually see this as `/live`. |
| **User name:** | The name MfRDJ uses to sign in to IceCast. This is normally `source`. |
| **Password:** | The IceCast broadcast password. It must be the same password that was set in IceCast. |
| **Broadcast name:** | The station name shown to listeners. |
| **Description:** | A short sentence describing the station or program. |
| **Genre:** | The type of music or program. |
| **Broadcast URL:** | Your station's website address. Leave it empty for Local and LAN. |
| **Checkbox Public Stream on/off** | Controls public directory listing only. It is not the switch that enables Internet listening. |

### 5.2 IceCast server manager

This section tells MfRDJ how to find IceCast and, for Local setup, how to start it.

#### Local

| Field | Enter this |
| --- | --- |
| **Host:** | `127.0.0.1` |
| **Port:** | `8000` |
| **Exe Path:** | `C:\Icecast\bin\icecast.exe` |
| **Config Path:** | `C:\Icecast\icecast.xml` |
| **HTTP Path:** | `/` |
| **Working Dir:** | `C:\Icecast` |
| **Startup Delay:** | `3000` |
| **Checkbox Auto Restart on/off** | On |

If IceCast was installed in a different folder, use that folder instead of `C:\Icecast`.

#### LAN

| Field | Enter this |
| --- | --- |
| **Host:** | `192.168.50.83` |
| **Port:** | `8000` |
| **Exe Path:** | Leave empty |
| **Config Path:** | Leave empty |
| **HTTP Path:** | `/` |
| **Working Dir:** | Leave empty |
| **Startup Delay:** | `3000` |
| **Checkbox Auto Restart on/off** | Off |

#### WAN

| Field | Enter this |
| --- | --- |
| **Host:** | `192.168.50.83` |
| **Port:** | `8000` |
| **Exe Path:** | Leave empty |
| **Config Path:** | Leave empty |
| **HTTP Path:** | `/` |
| **Working Dir:** | Leave empty |
| **Startup Delay:** | `3000` |
| **Checkbox Auto Restart on/off** | Off |

#### Brief explanation of the fields

| Field | What it means |
| --- | --- |
| **Host:** | The computer running IceCast. |
| **Port:** | The IceCast connection number. Use the same Port as in IceCast settings. |
| **Exe Path:** | Where `icecast.exe` is stored. Fill this in only for Local setup. |
| **Config Path:** | Where the IceCast settings file is stored. Fill this in only for Local setup. |
| **HTTP Path:** | The page MfRDJ checks to see if IceCast is working. Normally `/`. |
| **Working Dir:** | The IceCast program folder. Fill this in only for Local setup. |
| **Startup Delay:** | How long MfRDJ waits after starting IceCast. `3000` means 3 seconds. |
| **Checkbox Auto Restart on/off** | Lets MfRDJ restart IceCast when both programs run on the same computer. |

### 5.3 Caddy /json settings

Caddy provides the web page, current-song information and cover pictures.

#### Local

| Field | Enter this |
| --- | --- |
| **Caddy Path:** | `C:\Caddy` |
| **Caddy Config Path:** | `C:\Caddy\caddy.cff` |
| **Caddy json Path:** | `C:\Caddy\nowplaying.json` |
| **Caddy Covers Path:** | `C:\Caddy\covers` |
| **Caddy Command:** | `C:\Caddy\caddy.exe run --config "C:\Caddy\caddy.cff" --adapter caddyfile` |

#### LAN

| Field | Enter this |
| --- | --- |
| **Caddy Path:** | `\\PCHP001\Caddy` |
| **Caddy Config Path:** | `\\PCHP001\Caddy\caddy.cff` |
| **Caddy json Path:** | `\\PCHP001\Caddy\nowplaying.json` |
| **Caddy Covers Path:** | `\\PCHP001\Caddy\covers` |
| **Caddy Command:** | Leave empty |

#### WAN

| Field | Enter this |
| --- | --- |
| **Caddy Path:** | `\\PCHP001\Caddy` |
| **Caddy Config Path:** | `\\PCHP001\Caddy\caddy.cff` |
| **Caddy json Path:** | `\\PCHP001\Caddy\nowplaying.json` |
| **Caddy Covers Path:** | `\\PCHP001\Caddy\covers` |
| **Caddy Command:** | Leave empty |

#### Brief explanation of the fields

| Field | What it means |
| --- | --- |
| **Caddy Path:** | The main folder used by Caddy. |
| **Caddy Config Path:** | The location of Caddy's settings file. |
| **Caddy json Path:** | The file MfRDJ updates with the currently playing song. |
| **Caddy Covers Path:** | The folder where MfRDJ places cover pictures. |
| **Caddy Command:** | The command used to start Caddy. Fill this in only for Local setup. |

For LAN and WAN, the `\\PCHP001\Caddy` folder must open in Windows Explorer from the MfRDJ computer. If it does not open, ask the person who manages the network to give the MfRDJ computer access.

For filling in the Broadcast page, you can stop here. The following sections are optional checks and troubleshooting.

## 6. Complete setup recipes

### Recipe A - local test on one PC

1. Install Icecast and Caddy locally.
2. In **General**, select MASTER; configure CUE and MICROPHONE only if required; use 2 channels, 1 loopback deck and 60 ms.
3. Set both Icecast Host fields to `127.0.0.1`, both ports to `8000`, mount `/live`, username `source`, and enter the XML source password.
4. Set local Icecast EXE/config/working paths.
5. Set Caddy Path `C:\Caddy`, config `C:\Caddy\caddy.cff`, JSON `C:\Caddy\nowplaying.json`, and the correct covers folder.
6. Click **OK**, open Media Server, start the services, then enable Broadcast.
7. Test `http://127.0.0.1:8000/live`.

### Recipe B - listeners on the LAN, services on the MfRDJ PC

1. Complete Recipe A.
2. Give the MfRDJ PC a DHCP reservation or static private IPv4 address.
3. Allow inbound Icecast TCP 8000 and the Caddy port(s) on the Windows Private firewall profile.
4. From another device, test `http://SERVER-LAN-IP:8000/live`.
5. Configure Caddy for a LAN address if the PWA/web player is required, then test it from another device.

### Recipe C - Icecast/Caddy on a LAN server

1. Install and start Icecast/Caddy on the server. Configure service auto-start on that server.
2. Reserve the server's private IPv4 address.
3. Share only the required Caddy web folder with the MfRDJ account; grant read/write access.
4. In MfRDJ, put the server IPv4 address in both Host fields.
5. Use the server's Icecast port, source username/password and mount.
6. Leave local Icecast process paths empty because the server owns the processes.
7. Enter UNC Caddy/JSON/cover paths when RDJ must publish metadata and covers.
8. Open Media Server and click Start/Confirm. The log must report both Icecast and the Caddy web folder as reachable before Broadcast becomes available.

### Recipe D - public HTTPS radio

1. Start from Recipe B or C and make LAN playback work first.
2. Register a domain or DDNS name and point it to the public router address.
3. Put that domain on the first line of `caddy.cff`.
4. Forward TCP 80 and 443 to the Caddy server. Do not use the obsolete/example `443 -> 8080` mapping unless Caddy has explicitly been configured to listen internally on 8080.
5. Allow Caddy through Windows Firewall on TCP 80 and 443.
6. Keep Icecast port 8000 private when Caddy proxies `/live`.
7. Start Caddy and confirm it obtains a certificate without errors.
8. In MfRDJ, enable Broadcast and test `https://radio.example.com` from a phone using mobile data, not Wi-Fi.
9. Confirm the player loads, `/live` plays, `nowplaying.json` changes, and cover URLs are HTTPS URLs rather than disk/UNC paths.

## 7. Windows firewall and router checklist

| Service | Protocol/port | Local only | LAN | Public internet |
| --- | --- | --- | --- | --- |
| Icecast source/listener | TCP 8000 | No inbound rule required for loopback | Allow on Private profile when LAN clients/RDJ need it | Prefer not forwarded; expose through Caddy |
| Caddy HTTP | TCP 80 | Optional | Allow if LAN HTTP is used | Allow and forward to Caddy; also used for redirects/certificate validation |
| Caddy HTTPS | TCP 443 | Optional | Allow if LAN HTTPS is used | Allow and forward to Caddy |
| Windows file sharing | TCP 445 | Not applicable | Allow only on trusted Private LAN if UNC publishing is used | Never forward to the internet |

Do not place the MfRDJ PC or server in the router's DMZ. Forward only required ports to the fixed internal address of the Caddy server.

## 8. Operating sequence

### Local managed services

1. Start MfRDJ.
2. Open Media Server.
3. Click **Start** to start Caddy and Icecast.
4. Wait until Icecast reports ready.
5. Click **Broadcast**.
6. Verify the on-air indicator and listener URL.

### Remote managed services

1. Start Icecast and Caddy on the server or verify their Windows services are running.
2. Confirm the Caddy share opens from the MfRDJ PC.
3. Start MfRDJ and open Media Server.
4. Click **Start/Confirm**. This is a reachability check in remote mode.
5. When the log reports the remote server ready, click **Broadcast**.

## 9. Verification commands

Replace the examples with your own host and domain.

```powershell
# Resolve a server name
Resolve-DnsName PCHP001 -Type A

# Test Icecast from the RDJ PC
Test-NetConnection 192.168.1.60 -Port 8000

# Test Caddy on the server/LAN
Test-NetConnection 192.168.1.60 -Port 443

# Verify public DNS
Resolve-DnsName radio.example.com -Type A
```

Also verify these URLs where applicable:

```text
http://127.0.0.1:8000/
http://SERVER-LAN-IP:8000/live
https://radio.example.com
https://radio.example.com/live
```

## 10. Troubleshooting

### MfRDJ cannot connect as source

- Confirm Broadcast Host and Port identify the Icecast server from the MfRDJ PC.
- Confirm the Broadcast Password matches Icecast `<source-password>`.
- Confirm the mount starts with `/` and contains no spaces.
- Check Icecast `error.log` for authentication or bind errors.
- Test TCP 8000 with `Test-NetConnection`.

### Remote server is not confirmed

- Use the remote server's numeric private IPv4 address in Server Manager Host.
- Confirm Icecast is already running on the remote server.
- Open the UNC Caddy path in Explorer using the same Windows account as MfRDJ.
- Check share and NTFS permissions.
- Ensure Windows classifies the LAN as Private and the firewall permits the required traffic.

### Works on the server but not another LAN device

- Use the server's LAN address, not `127.0.0.1`.
- Verify the listener and server are on routable LAN/VLANs.
- Check inbound Windows Firewall rules.
- Confirm Icecast is not bound only to `127.0.0.1` in `icecast.xml`.

### LAN works but public internet does not

- Confirm public DNS resolves to the current public address.
- Confirm router forwards TCP 80/443 to the Caddy server's current private address.
- Test from mobile data to avoid router NAT-loopback differences.
- Check for carrier-grade NAT (CGNAT) or an ISP that blocks inbound ports.
- Read the Caddy log for ACME/certificate errors.

### Web page loads but audio does not play

- Confirm MfRDJ is broadcasting and Icecast shows the `/live` source.
- Confirm `caddy.cff` proxies the same mount prefix used in MfRDJ.
- Open `/live` directly through the Caddy domain.
- Avoid mixed content: an HTTPS page must not point the audio element to an `http://` stream.

### Metadata or covers do not update

- Verify Caddy JSON/Covers paths exist and are writable.
- In remote mode, verify the UNC share stays connected.
- Make sure JSON cover URLs are web URLs, not `C:\...` or `\\SERVER\...` paths.
- Confirm Caddy serves the folder in which the files are written.

### Audio crackles, drops or recording is unstable

- Start with the main and recorder buffers at 60 ms.
- Increase the main buffer in small steps.
- Leave **Disable MMCSS** unchecked.
- Avoid Bluetooth, power-saving USB hubs and heavily loaded network shares.
- Confirm all active audio endpoints support the selected format.

## 11. Security and backup

- Change every example Icecast password before connecting the server to a network.
- Use different strong passwords for source, relay and admin accounts.
- Do not publish Icecast admin routes through Caddy; the supplied config blocks `/admin*`.
- Expose Caddy HTTPS rather than Icecast port 8000 where practical.
- Never expose SMB/Windows file sharing to the public internet.
- Run MfRDJ and server processes as normal users where possible; elevate only when required for installation or binding.
- Back up the MfRDJ INI, `Data` database folder, Caddy configuration/web assets, and Icecast XML.
- Test restoration before relying on the backups.

## 12. Final commissioning checklist

### Audio and recording

- [ ] MASTER endpoint plays clean audio.
- [ ] CUE/PFL uses the intended headphones endpoint.
- [ ] Microphone input is correct and does not feed back.
- [ ] Main and recorder buffers are stable.
- [ ] WAV/FLAC recording succeeds and duplicate filenames are protected.

### Icecast source

- [ ] Host and port reach Icecast from the MfRDJ PC.
- [ ] Username/password match `icecast.xml`.
- [ ] Mount is `/live` (or the intended replacement).
- [ ] Stream name, description, genre and URL are correct.
- [ ] Icecast shows the source connected.

### Caddy and metadata

- [ ] Caddy starts with the intended config.
- [ ] Web player opens from the target network.
- [ ] `/live` is proxied to Icecast.
- [ ] `nowplaying.json` updates.
- [ ] Cover images load through a browser URL.

### LAN/public access

- [ ] Server has a fixed/reserved private address.
- [ ] Required Windows Firewall rules are enabled on the correct profile.
- [ ] Public DNS/DDNS points to the correct address.
- [ ] Only required router ports are forwarded.
- [ ] HTTPS certificate is valid.
- [ ] Public test succeeds from outside the LAN.

## 13. Reference documentation

- Icecast basic setup: https://icecast.org/docs/icecast-latest/basic_setup/
- Icecast configuration reference: https://icecast.org/docs/icecast-latest/config_file/
- Caddy HTTPS quick start: https://caddyserver.com/docs/quick-starts/https
- Caddy reverse proxy: https://caddyserver.com/docs/quick-starts/reverse-proxy

# FxServe V1.1
  
FxServe is the lightweight FactoryX HTTP media server for MfPack, MfRdj, and
RDJ Pro. It serves the web player and generated media files and forwards the
small set of live endpoints currently handled by Caddy.
  
FxServe can run interactively as a console application or as a native Windows
service. Its LAN listener remains on port 8080. An optional HTTP.sys front end
adds HTTPS on port 443 and redirects port 80 to HTTPS for WAN use. WAN mode is
disabled by default. When WAN HTTPS is enabled, FxServe obtains, installs,
renews, validates, and binds its certificate itself.
  
## Current capabilities
  
- Concurrent client connections.
- Static files rooted in one configured directory.
- `GET`, `HEAD`, and `OPTIONS`.
- Single HTTP byte ranges, including suffix and open-ended ranges.
- HLS/fMP4 and common web/media MIME types.
- Configurable CORS and no-cache routes.
- Streaming reverse proxy routes for `/live`, `/status`, and `/video.mjpg`.
- Explicit `403 Forbidden` responses for `/admin` and `/admin.xsl`.
- URL decoding and protection against paths escaping the web root.
- Console and file logging.
- Native Windows service start, stop, and shutdown handling.
- HTTP.sys TLS termination with an SNI certificate binding.
- Built-in ACME v2 certificate issue and renewal using an in-memory HTTP-01
  challenge.
- Host-name validation and HTTP-to-HTTPS permanent redirects.
- HTTP.sys-to-localhost forwarding, including byte ranges used by fMP4/MSE.
  
## Build
  
Open `FxServe.dproj` in Delphi and build Win32 or Win64, or compile from a
configured Delphi command prompt:
  
```text
dcc32 -B FxServe.dpr
dcc64 -B FxServe.dpr
```
  
The source currently builds with Delphi compiler version 28.0 up to 31.1.
  
### Self-contained deployment build
  
Generate the embedded default configuration and web content, then build the
portable executable with:
  
```powershell
.\Build-SelfContained.ps1 -Configuration Release -Platform Win64
```
  
Copy only `Win64\Release\FxServe.exe` to an empty folder on the target PC. On
its first normal start or `--install`, FxServe creates `FxServe.ini`, `www`, and
the bundled static website/PWA content beside the executable. Existing files are
never overwritten. Runtime media (`live.json`, `init.mp4`, fragments), logs,
backups, and update staging files are not embedded.
  
## Run
  
The default configuration uses port `8080` and the included `www` directory:
  
```text
FxServe.exe --config FxServe.ini
```
  
Then open `http://127.0.0.1:8080/`. Press Ctrl+C to stop.
  
Running interactively requires no installation. Registering FxServe as an
automatic Windows service still requires one elevated `FxServe.exe --install`
command because Windows controls service registration. WAN firewall rules,
HTTP.sys reservations, and certificate setup are likewise administrator-only
operating-system actions.
  
Paths in the INI file are resolved relative to the INI file. For an RDJ
installation, set `WebRoot` to the directory containing the web application,
artwork, JSON, video, and stream directories.
  
## Command-line reference
  
Display the built-in command-line help with any of these forms:
  
```text
FxServe.exe --help
FxServe.exe -h
FxServe.exe /?
```
  
FxServe accepts the following case-insensitive parameters:
  
| Parameter | Purpose |
| --- | --- |
| `--config <file>` or `-c <file>` | Use the specified configuration file. |
| `--install` | Install FxServe as an automatic Windows service. Run elevated. |
| `--uninstall` | Remove the FxServe Windows service. Run elevated. |
| `--service` | Run as a Windows service. This is used internally by Windows Service Control Manager. |
| `--bootstrap` | Create missing configuration and static web files, then exit. |
| `--bootstrap-refresh` | Refresh embedded static web files while preserving server/site configuration and runtime data. |
| `--certificate-setup` | Configure built-in ACME certificate management. Use it with `--host`, `--email`, and `--accept-ca-terms`. |
| `--host <DNS-name>` | Set the public DNS name during certificate setup. |
| `--email <address>` | Set the certificate authority account email address during certificate setup. |
| `--accept-ca-terms` | Confirm acceptance of the certificate authority's terms during certificate setup. |
| `--staging` | Use the certificate authority's staging environment during certificate setup. |
| `--certificate-disable` | Disable built-in certificate management. |
| `--help`, `-h`, or `/?` | Display command-line help and exit. |
  
With no mode parameter, FxServe runs interactively. It first looks for an INI
file with the same path and base name as the executable. If that file does not
exist, it looks for `FxServe.ini` in the current directory. An explicit
`--config` or `-c` parameter overrides both defaults.  
  
Note: Parameters should be typed like "path + exename" + "space" + "--parameter" Example: "C:\FxServe\FxServe.exe --install"
  
Install the service with an absolute configuration path:
  
```text
FxServe.exe --install --config "C:\FxServe\FxServe.ini"
```
  
Configure production certificate management with:
  
```text
FxServe.exe --certificate-setup --host radio.example.com --email admin@example.com --accept-ca-terms
```
  
Add `--staging` while testing certificate setup. The staging environment does
not issue a browser-trusted production certificate. Certificate state is kept
in the protected machine-wide registry and no separate FxServeCertBind utility
is required.
  
## Configuration
  
```ini
[Server]
BindAddress=0.0.0.0
Port=8080
WebRoot=.\www
IndexFile=index.html
MaxConnections=128
HeaderTimeoutMs=10000
SendTimeoutMs=30000

[Proxy]
Enabled=True
Host=127.0.0.1
Port=8000
Routes=/live,/status,/video.mjpg

[Headers]
Cors=True
NoStoreRoutes=/stream,/video,/nowplaying.json

[Logging]
File=FxServe.log

[Wan]
Enabled=False
HostName=yourradio.yourhost.com
HttpEnabled=True
HttpPort=80
HttpsEnabled=True
HttpsPort=443
RedirectHttp=True
```
  
Keep port 8080 private when FxServe operates on a WAN. Forward public router
ports 80 and 443 to the same ports on the FxServe server; HTTP.sys then forwards
accepted requests internally to `127.0.0.1:8080`.
  
For `MfWebCamStreamer`, requests to `/WebCam/live.json` carrying a `viewer`
query value update an in-memory presence registry. `/WebCam/viewers.json`
returns the active tab count and privacy-masked source addresses. Entries expire
after 15 seconds without a stream heartbeat; no viewer data is persisted.
  
No certificate utility, renewal task, web-root challenge file, or certificate
setting in `FxServe.ini` is required. On the first WAN HTTPS start, FxServe
creates protected machine-wide state below
`HKLM\Software\FactoryX\FxServe\Certificate`. Only SYSTEM and local
Administrators can read it. The account and certificate private keys are
non-exportable Windows machine keys. Enabling WAN HTTPS opts the installation
into the certificate authority's terms of service; FxServe then checks the
certificate every six hours and renews it 30 days before expiry.
  
## Windows service
  
From an elevated command prompt, install FxServe using an absolute
configuration path:
  
```text
FxServe.exe --install --config "C:\FxServe\FxServe.ini"
sc.exe start FxServe
```
  
The service name is `FxServe`, its display name is `FactoryX FxServe`, and its
startup mode is Automatic. Remove it with:
  
```text
FxServe.exe --uninstall
```
  
The service account must have read access to the web root and write access to
the configured log location. Relative web and log paths are resolved from the
directory containing `FxServe.ini`.
  
## Remote server deployment package
  
The repeatable remote server deployment is in `Deploy\YourRemoteServer`.

```text
C:\FxServe\
  FxServe.exe
  FxServe.ini
  FxServe.log
  www\
    Artwork\
    Stream\
```
  
After building Win64 Release, create a clean timestamped package with:
  
```powershell
.\Deploy\YourRemoteServer\New-Package.ps1
```

The package does not duplicate `FxServe.ini` or `www`; both defaults are
embedded in `FxServe.exe`. The installer bootstraps a new installation and
refreshes managed static assets on an update while preserving configuration and
live stream data.

See `Deploy\YourRemoteServer\README-Deploy.md` for installation, health-check, WAN,
certificate-renewal, update, and rollback instructions. RDJ Pro publishes to
`\\YourRemoteServeName\FxServe\www\Stream`.

`FxServeAdmin` is the matching VCL management application. It uses the native
remote Windows Service Control Manager, with FxServe-only defaults,
configuration editing, log/folder access, and HTTP health checks. **Save &
apply** writes the selected public hostname to `FxServe.ini` and publishes the
server/public URL metadata in `www\fxserve-config.json`; live publisher JSON is
never rewritten. It manages only FxServe; Caddy and other web servers retain
their own administration tools.

## Documentation

- [RDJ Web Server Setup](RDJ-Web-Server-Setup.md) gives short installation
  steps for FxServe and Caddy.
- [FxServe White Paper](FxServe-White-Paper.md) explains the design, RDJ and
  RDJ Pro signal paths, HTTPS renewal, security, limits, and MfPack scope.
- [Remote server deployment guide](Deploy/YourRemoteServer/README-Deploy.md) covers server
  installation, updates, health checks, and rollback.

## Next milestones

1. Add repeatable protocol and concurrency tests.
2. Add configuration reload and rotating access logs.
3. Add optional request throttling and WAN abuse protection.

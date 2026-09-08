# FxServe deployment on YourRemoteServeName

FxServe is installed entirely below `C:\FxServe`. Caddy is not used by this
deployment.

## Create a package

Build `Win64\Release\FxServe.exe`, then run from a normal PowerShell:

```powershell
.\Deploy\YourRemoteServer\New-Package.ps1
```

The command creates a timestamped directory below `Deploy\YourRemoteServer\Packages`.
It contains the self-contained server executable, deployment scripts, and
`FxServeAdmin.exe`. The default configuration and static web application are
embedded in `FxServe.exe`; live RDJ output files are not included.

## Install or update

Copy the generated package directory to YourRemoteServeName. From an elevated PowerShell
in that directory, run:

```powershell
.\Install-FxServe.cmd
```

The installer:

- installs into `C:\FxServe`;
- asks the self-contained executable to create a missing `FxServe.ini` and
  refresh embedded static website files while preserving site configuration and
  live stream data;
- preserves `C:\FxServe\www\Stream` during updates;
- creates or validates `\\YourRemoteServeName\FxServe`;
- grants authenticated publishers Modify access only below `www`;
- creates a local-subnet-only firewall rule for TCP 8080;
- registers and starts the `FxServe` Windows service;
- checks `http://127.0.0.1:8080/`;
- restores the previous executable, INI, and static web files if an update
  fails.

To require an active RDJ stream as part of deployment validation:

```powershell
.\Install-FxServe.cmd -RequireLiveStream
```

To grant publishing rights to a specific account instead of Authenticated
Users:

```powershell
.\Install-FxServe.cmd -PublisherAccount "DOMAIN\RdjPublisher"
```

RDJ Pro publishes the live stream to:

```text
\\YourRemoteServeName\FxServe\www\Stream
```

## FxServe Admin

Run `FxServeAdmin.exe` on an administrator workstation for normal local or
remote management. The supplied YourRemoteServeName profile uses service `FxServe`, share
`\\YourRemoteServeName\FxServe`, server path `C:\FxServe`, and public hostname
`yourradio.yourhost.com`.

Set the server, share, server-local path, and public hostname, then choose
**Save & apply**. FxServe Admin updates `[Wan] HostName` in `FxServe.ini` and
`www\fxserve-config.json`, making `.bak` backups and leaving live stream files
untouched. Restart FxServe afterward to apply the WAN hostname.

FxServe Admin also provides service status, start, stop, restart, registration,
folder/log access, INI editing with backup, LAN health checks, and shortcuts to
the LAN and public applications. The signed-in Windows account still needs
permission to control services on YourRemoteServeName.

FxServe Admin manages only the FxServe service. Stop or configure Caddy with
CaddyAdmin before starting FxServe if both programs would otherwise compete for
ports 80 and 443.

## Health check

Run on YourRemoteServeName:

```powershell
C:\FxServe\Test-FxServe.ps1 -RequireLiveStream
```

Without `-RequireLiveStream`, an idle RDJ recorder produces a warning but does
not make the FxServe HTTP health check fail.

## WAN and HTTPS

The intended network layout is:

| Router service | Protocol | External | Internal | Destination |
| --- | --- | ---: | ---: | --- |
| FactoryX Radio HTTP | TCP | 80 | 80 | YourRemoteServeName LAN address |
| FactoryX Radio HTTPS | TCP | 443 | 443 | YourRemoteServeName LAN address |

Do not forward public port 8080. FxServe keeps that port as its LAN/application
listener. HTTP.sys receives public HTTP/HTTPS on YourRemoteServeName and forwards requests
locally to `127.0.0.1:8080`.

The ASUS DDNS name is:

```text
yourradio.yourhost.com
```

The certificate configured in the ASUS router protects the router web GUI. The
FxServe server therefore needs its own certificate for the same public name.

### Safe cutover order

1. Install the new FxServe package while `[Wan] Enabled=False`.
2. Confirm `http://YourRemoteServeName:8080/` and the live RDJ stream still work.
3. In the router, add the two port-forwarding rows shown above. Leave Source IP
   empty.
4. Confirm DDNS resolves `yourradio.yourhost.com` to the current public IP.
5. Stop Caddy so that its process releases ports 80 and 443. Do not remove or
   alter its files. If the remaining cutover fails, Caddy can simply be started
   again.
6. Enable FxServe WAN mode:

   ```powershell
   C:\FxServe\Enable-Wan.ps1 -CertificateEmail 'you@example.com' -AcceptCaTerms
   ```

   Replace `you@example.com` with the certificate-contact email address. The
   `-AcceptCaTerms` switch explicitly accepts the certificate authority terms.
   The script registers FxServe's built-in certificate manager, reserves the
   HTTP.sys URLs for the LocalSystem service, opens Windows Firewall ports
   80/443, enables WAN, and restarts FxServe. FxServe itself then
   creates a non-exportable machine key, completes the ACME HTTP-01 validation,
   installs the returned certificate chain, and updates its HTTP.sys SNI
   binding. Its state is protected in HKLM and no certificate files, renewal
   tasks, or external binding tools are used. If FxServe fails to restart, the
   INI is restored from `C:\FxServe\FxServe.ini.before-wan`.
7. Watch `C:\FxServe\FxServe.log` for the message that a new certificate was
   obtained and bound. HTTP port 80 must remain reachable from the Internet for
   renewals.
8. On YourRemoteServeName, validate the binding without depending on router NAT loopback:

   ```powershell
   curl.exe --resolve yourradio.yourhost.com:443:127.0.0.1 https://yourradio.yourhost.com/
   curl.exe -I --resolve yourradio.yourhost.com:80:127.0.0.1 http://yourradio.yourhost.com/
   ```

   HTTPS must return the FactoryX page; HTTP must return `308 Permanent
   Redirect` with an `https://yourradio.yourhost.com/` location.
9. Finally test `https://yourradio.yourhost.com/` from a phone with Wi-Fi
   disabled. This proves the DDNS, router forwarding, firewall, certificate,
   HTTP.sys binding, and FxServe path together.

If certificate issuance or WAN activation fails during steps 6-7, leave WAN
disabled and start Caddy again. This restores the former public path without
mixing either application's directories.

## Rollback

Every update from a separate package directory creates a timestamped backup in
`C:\FxServe\Backup`. Failed updates are rolled back automatically. For a manual
rollback, stop the service, copy `FxServe.exe`, `FxServe.ini`, and the backed-up
`www` contents from the selected timestamp directory into `C:\FxServe`, then
start the service again. Never replace or delete the live
`C:\FxServe\www\Stream` directory during rollback.

# Caddy Admin

Caddy Admin is a standalone Windows/VCL utility for managing the Caddy Windows services used by both RDJ and RDJ Pro. It can install, uninstall, start, stop, restart, inspect, and configure Caddy locally or on another Windows computer like a remote server.

The server, service name, shared Caddy folder, server-local Caddy path, and optional application INI are saved after pressing **Save**. CaddyAdmin never guesses an `MfRdjJ.ini` location: **Application INI** contains exactly the path entered by the user. A non-empty path must exist and must be recognized as RDJ or RDJ Pro before it is stored. New versions use `HKEY_CURRENT_USER\Software\FactoryX\CaddyAdmin`; settings and password data created by the earlier RDJ Pro version are read automatically from the legacy registry key.

## Local Caddy

Leave **Server** empty when Caddy runs on the same computer as CaddyAdmin.

```text
Server:
Service:             RDJCaddy or RDJProCaddy
Caddy share:         C:\Caddy_RDJ or C:\Caddy
Server local path:   C:\Caddy_RDJ or C:\Caddy
Application INI:     optional path to MfRdjJ.ini (Note that both RDJ and RDJ Pro have the same ini names.)
```

## Remote Caddy

Enter the Windows server name when Caddy runs on another computer.

RDJ example:

```text
Server:              PCHP001
Service:             RDJCaddy
Caddy share:         \\PCHP001\Caddy_RDJ
Server local path:   C:\Caddy_RDJ
Application INI:     optional path to the RDJ MfRdjJ.ini
```

RDJ Pro example:

```text
Server:              PCHP001
Service:             RDJProCaddy
Caddy share:         \\PCHP001\Caddy
Server local path:   C:\Caddy
Application INI:     optional path to the RDJ Pro MfRdjJ.ini
```

**Caddy share** is the path visible to the computer running CaddyAdmin. **Server local path** is the same folder as seen by the server and is used in the Windows service command.

## Separate RDJ and RDJ Pro services

RDJ and RDJ Pro may have separate service registrations and folders. Normally only one of them can run at a time because both use public ports 80 and 443.

To switch remotely:

1. Enter the name of the currently running service: `RDJCaddy` or `RDJProCaddy`.
2. Press **Stop** and answer **Yes** to the confirmation.
3. Do not switch the fields yet. Wait until the log confirms:

   ```text
   Stopping Caddy service...
   Stop command sent.
   Service status: Stopped
   ```

4. Enter the other service name and its Caddy paths.
5. Press **Start** and wait for `Service status: Running`.

Changing the service name or Caddy paths does not stop the previously selected service. If the log contains no **Stopping Caddy service** entry, the old service was not stopped.

### Start command sent, but service remains Stopped

When CaddyAdmin logs **Start command sent** followed by **Service status: Stopped**, first check whether the other Caddy service is still running. A typical example is:

```text
RDJCaddy:    Running
RDJProCaddy: Stopped, Windows exit code 1067
```

CaddyAdmin reads the Windows service exit code and projects it into both the log memo and status bar. For example:

```text
Service status: Stopped - Windows exit code 1067: The process terminated unexpectedly.
```

For Windows exit code 1066, the separate service-specific exit code is displayed as well.

This normally means the running service already owns ports 80 and 443. The second Caddy process receives the start command but exits immediately because it cannot use those ports. Select the running service, stop it, wait for confirmed **Stopped** status, and only then start the required service.

This failure does not by itself mean that `caddy.cff` or **Application INI** is incorrect. Validate the Caddy configuration separately if the required service still stops after the other service has been confirmed stopped.

To serve both applications simultaneously, use one running Caddy service, two distinct public hostnames, and two site blocks in one Caddy configuration.

## Configuration editor

Press **Configuration** to edit `caddy.cff` in the configured **Caddy share**. The editor changes:

- Site address: the public hostname on the first site block.
- Caddy root path: every `root * ...` directive.
- Log file: the existing `output file ...` directive, when present. Logging is optional.
- Upstream host/IP and port: every `reverse_proxy ...` target.

Use upstream `127.0.0.1:8000` when Caddy and Icecast/RDJ Pro run on the same server. If the stream server runs on another computer, use its stable LAN hostname or IP and listening port.

### Optional application INI update

When **Application INI** is filled in, saving the Caddy configuration identifies the application automatically:

- RDJ is recognized by `[Icecast]` or `[SetupBroadcast]`. Caddy paths are written to the RDJ `[Icecast]` fields.
- RDJ Pro is recognized by `[Caddy]`. Caddy paths, artwork, video, command, and log settings are written to `[Caddy]`.
- An unrecognized INI is rejected without inventing a new application section.

Leave **Application INI** empty when no application configuration should be changed.

## Installation

If the selected service does not exist, press **Install**. CaddyAdmin ensures `caddy.exe` and `caddy.cff` exist in the target folder and registers this command using **Server local path**:

```text
"C:\Caddy_RDJ\caddy.exe" run --config "C:\Caddy_RDJ\caddy.cff" --adapter caddyfile
```

Deployment copies missing files only; it does not overwrite existing configuration, web assets, certificates, logs, or media. **Uninstall** removes only the Windows service registration, not the Caddy folder.

## Password and permissions

The first run asks for an admin password. A random salt and SHA-256 hash are stored; the plain password is not saved.

CaddyAdmin controls services through the Windows Service Control Manager. The signed-in Windows account must still have permission to control services on the selected machine. For remote management, Windows Firewall must allow Remote Service Management/RPC and the Caddy share must be reachable.

## Troubleshooting

- Leave **Server** empty for local management.
- Use the Windows computer name, such as `PCHP001`, for remote management.
- Ensure **Service** exactly matches the installed service name.
- Use a UNC path such as `\\PCHP001\Caddy_RDJ` for a remote **Caddy share**.
- Use a local server path such as `C:\Caddy_RDJ` for **Server local path**.
- Do not start two Caddy services that both bind to ports 80 and 443.

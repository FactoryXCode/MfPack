# RDJ Pro Caddy Admin

Standalone Windows/VCL admin utility for controlling a local or remote Caddy Windows service used by RDJ Pro.

The server, service name, shared Caddy folder, and server-local Caddy path are user settings. They are saved in the Windows registry under `HKEY_CURRENT_USER\Software\RDJPro\CaddyAdmin` after pressing **Save**. First-run values are intentionally empty, so the app does not assume a fixed server name or install directory.

## Local Caddy

Leave **Server** empty when Caddy runs on the same PC as CaddyAdmin. An empty server value makes the app use the local Windows Service Control Manager.

Example local settings:

```text
Server:              
Service:             RDJProCaddy
Caddy share/folder:  C:\Caddy
Server local path:   C:\Caddy
```

For local use, **Caddy share/folder** can be a normal local path. It does not need to be a network share.

## Remote Caddy

Enter the server name when Caddy runs on another Windows machine.

Example remote settings:

```text
Server:              PCHP001
Service:             RDJProCaddy
Caddy share/folder:  \\PCHP001\Caddy
Server local path:   C:\Caddy
```

For remote use, **Caddy share/folder** is the path opened from the client PC, usually a UNC path. **Server local path** is the path as seen by the server itself, used when installing the Windows service.


## Caddy.cff Editor

Use **Configuration** to edit `caddy.cff` from the configured **Caddy share/folder**. For local Caddy this is usually `C:\Caddy\caddy.cff`; for remote Caddy this is usually something like `\\PCHP001\Caddy\caddy.cff`.
The editor intentionally exposes only values that normally change when Caddy is installed or moved to another machine:

- Site address, the first Caddy site label such as `factoryxradio.asuscomm.com`.
- Caddy root path, written to every `root * ...` line.
- Log file path, written to the `output file ... {` line. The editor manages
  the file-writer braces automatically; they are Caddy syntax and are not part
  of the path shown in the editor.
- RDJPRO host/IP and port, written to every `reverse_proxy ...` line.

All other Caddy directives are preserved.

### RDJPRO host/IP and port

These fields define the upstream RDJ Pro HTTP server to which Caddy forwards
requests such as `/live` and `/status`. Enter the address as seen from the
machine running the Caddy service. They do not specify the Caddy server, the
CaddyAdmin workstation, the public site address, or Caddy's HTTPS port.

**RDJPRO host/IP**:

- Use `127.0.0.1` when RDJ Pro and the Caddy service run on the same computer.
- Use the LAN hostname or a stable LAN IP address of the RDJ Pro computer when
  RDJ Pro and Caddy run on different computers, for example `RDJPRO01` or
  `192.168.1.50`.
- Do not enter `http://`, `https://`, a path such as `/live`, or a port in this
  field.

**RDJPRO port**:

- Enter the port on which RDJ Pro's HTTP/stream server listens. The normal
  value is `8000`.
- This is not Caddy's public HTTP/HTTPS port (`80` or `443`) and not Caddy's
  administration API port (`2019`).

Typical configurations:

| Scenario | Caddy service | RDJ Pro | RDJPRO host/IP | RDJPRO port |
| --- | --- | --- | --- | --- |
| Local, all on one PC | Local PC | Same local PC | `127.0.0.1` | `8000` |
| Remote administration, same server | `PCHP001` | `PCHP001` | `127.0.0.1` | `8000` |
| Separate LAN computers | `PCHP001` | `RDJPRO01` | `RDJPRO01` or its LAN IP | `8000` |

The second example remains `127.0.0.1` even when CaddyAdmin itself is running
on another workstation and remotely manages `PCHP001`: from Caddy's point of
view, RDJ Pro is still on the same computer.

For a separate RDJ Pro computer, its HTTP server must listen on a LAN-accessible
interface and Windows Firewall must allow the selected port. Test the upstream
from the Caddy computer before starting Caddy, for example:

```text
http://RDJPRO01:8000/status
```

With host `RDJPRO01` and port `8000`, the editor writes this target to every
applicable route:

```caddyfile
reverse_proxy RDJPRO01:8000
```

If **RDJPro ini** is filled in on the main CaddyAdmin form, saving `caddy.cff` also updates the `[Caddy]` section in that RDJ Pro setup INI. The editor updates only the Caddy file/path fields:

- `CaddyDir`
- `ConfigFile`
- `NowPlayingJsonFile`
- `Artwork`
- `Video`
- `Command`
- `LogFile`

The RDJ Pro paths are based on **Caddy share/folder**, so remote installs keep UNC paths such as `\\PCHP001\Caddy`. The Caddy command is based on the server-local Caddy root from `caddy.cff`, because that command runs on the Caddy machine.

## Password

The first run asks you to create an admin password. The password is not stored as plain text; the tool stores a random salt and SHA-256 hash in `HKEY_CURRENT_USER\Software\RDJPro\CaddyAdmin\Security`.

The tool stores its own settings in the current user registry hive and controls Caddy through the Windows Service Control Manager APIs. The logged-in Windows user still needs permission to control services on the selected machine, so the app password is an extra gate, not the only security layer.

## Install

If the service does not exist yet, enter the correct service name and server-local Caddy path, then use **Install**.
Before creating the Windows service, the app checks whether the Caddy folder and `caddy.exe` are already present.If they are missing, it prepares the destination by copying the bundled deployment files from `Deploy\Server\Caddy` into the configured Caddy folder.
The copy step creates missing folders and copies missing files only. Existing files are not overwritten, so an existing `caddy.cff`, log folder, web assets, certificates, or other local changes are preserved.

For local installs, the destination is usually the local **Caddy share/folder**, such as `C:\Caddy`. For remote installs, the destination should be a reachable UNC path such as `\\PCHP001\Caddy`. If the share/folder is empty and a remote server-local path such as `C:\Caddy` is configured, the app tries to use the administrative share path, for example `\\PCHP001\C$\Caddy`.

After the deployment files are present, the app installs the service command using the **Server local path**. For example, with server-local path `C:\Caddy`, the app installs this service command:
```text
"C:\Caddy\caddy.exe" run --config "C:\Caddy\caddy.cff" --adapter caddyfile
```

Use **Uninstall** only for removing the Windows service registration. It does not delete the Caddy files.

## Troubleshooting

If status/start/stop fails, check:

- For local Caddy, leave **Server** empty.
- For remote Caddy, enter the reachable Windows server name.
- The service name is filled in and matches the installed service.
- Windows Firewall allows Remote Service Management/RPC for remote control.
- The Windows account has rights to control services on the selected machine.
- The Caddy folder/share is reachable from the client PC.


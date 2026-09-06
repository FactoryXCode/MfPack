# FxServe Admin
  
Version: 4.0.0
  
**NOTES:**
  
- This release is updated for compiler version 17 up to 35.  
- SDK version: 10.0.28000.2705 (Win 11)  
- Requires Windows 10 or later.  
- Minimum supported MfPack version: 4.0.0  
  
FxServe Admin is the admin tool for managing only the `FxServe` Windows
service locally or on a remote Windows server. Caddy and other web servers must
be configured and managed with their own administration tools.

FxServe Admin is intended to run on the PC where RDJ is running. Certificate
issue, renewal, validation, installation, and HTTP.sys binding are handled
inside the FxServe service on the server. FxServe Admin neither handles private
keys nor exposes certificate settings.  
  
Default PCHP001 profile:
  
```text
Server:                   Your remote server name, like: PCHP001  
Service:                  The service name like: FxServe  
FxServe share:       Your remote server path, like: \\PCHP001\FxServe  
Server local path:   Your remote server local path, like: C:\FxServe  
Public hostname:   Your public hostname, like: factoryxradio.asuscomm.com  
```
  
## Capabilities
  
- Query service state, process ID and service exit codes through the Windows
  Service Control Manager.
- Start, stop and restart FxServe locally or remotely.
- Install or uninstall only the Windows service registration. Files are never
  deleted.
- Open the configured FxServe share and log.
- Edit `FxServe.ini` through the share. A `.bak` copy is made before saving.
- Check the LAN root and `/stream/live.json` on port 8080.
- Open the LAN or public HTTPS application in the default browser.
- Save each user's connection profile below
  `HKEY_CURRENT_USER\Software\FactoryX\FxServeAdmin`.

## Password protection

The first run asks the user to create an admin password. FxServe Admin stores a
random salt and SHA-256 hash below
`HKEY_CURRENT_USER\Software\FactoryX\FxServeAdmin\Security`; the plain password
is never saved. The password is required each time FxServe Admin starts and can
be changed with the **Password** button.

If the password is forgotten, close FxServe Admin and remove the `Salt` and
`Hash` values from that registry key. The next start asks for a new password.
  
The logged-in Windows account must have permission to use the Service Control  
Manager on the target server. Remote Service Management/RPC must be allowed by  
Windows Firewall and the FxServe share must be reachable. FxServe Admin requests  
elevation (Admin rights) at startup because Windows service operations require administrator  
rights.  
  
The Install command expects `FxServe.exe` and `FxServe.ini` to exist in the  
configured share. It registers this server-local command:  
  
```text
"C:\FxServe\FxServe.exe" --service --config "C:\FxServe\FxServe.ini"  
```
  
The service buttons use the native Windows Service Control Manager and manage
only the configured FxServe service. FxServe Admin does not stop, start, or
reconfigure Caddy or another web server. If another server uses ports 80 or 443,
manage it separately before starting FxServe WAN.
  
When FxServe WAN HTTPS is enabled, the server obtains and binds its certificate
automatically. Keep public port 80 forwarded to the server for ACME validation
and allow the first certificate request to finish before testing public HTTPS.  

Project: MFPack - FxServe/FxServe Admin  
Project location: MfPack  
  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 13/08/2026  
  
Copyright © FactoryX. All rights reserved.

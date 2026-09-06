# RDJ Web Server Setup
  
Apps like the RDJ or RDJ Pro samples, can use either **Caddy** or **FxServe** for internet streaming.
Only one of them can run at a time because both use ports 80 and 443.  
  
## Which server should I use?
  
### Use FxServe when
  
- The server is mainly used for RDJ or RDJ Pro or likewise apps.
- You want a small web server with a simple setup.
- You only need your radio website and live stream.
- You want to use less memory and disk space.

FxServe is the normal choice for a new RDJ or other media streaming installation.

### Use 3th party servers like Caddy when

- It is already installed and working.
- The server hosts other websites or services.
- You need more routing or proxy options.
- You often change the web-server configuration.

3th party servers like Caddy have more features, but they offer much more, than needed for a basic media stream.

## RDJ with FxServe

1. Copy the FxServe package to:

   ```text
   C:\FxServe
   ```

2. Run this file as Administrator:

   ```text
   C:\FxServe\Install-FxServe.cmd
   ```

3. Open **FxServeAdmin** and enter:

   ```text
   Server:              YOUR_SERVER1
   Service:             FxServe
   FxServe share:       \\YOUR_SERVER1\FxServe
   Server local path:   C:\FxServe
   Public hostname:     yourradiostream.yourhost.com
   ```

4. Press **Start**.

5. In RDJ or RDJ Pro Setup, set the streaming output folder to:

   ```text
   \\PCHP001\FxServe\www\Stream
   ```

6. Start broadcasting in RDJ.

7. Test the LAN address:

   ```text
   http://YOUR_SERVER1:8080/
   ```

8. Test the public address:

   ```text
   https://fyourradiostream.yourhost.com/
   ```

FxServe handles the complete HTTPS certificate lifecycle internally. There is
no certificate script or separate binding utility to run. Public TCP port 80
must remain forwarded to FxServe so its in-memory ACME HTTP-01 response can be
validated during initial issue and later renewals.

## RDJ Pro or similar apps with Caddy

1. Copy the Caddy files and radio website to:

   ```text
   C:\Caddy
   ```

2. Open **CaddyAdmin** and enter:

   ```text
   Server:              YOUR_SERVER1
   Service:             RDJProCaddy
   Caddy share:         \\YOUR_SERVER1\Caddy
   Server local path:   C:\Caddy
   Application INI:     path to the RDJ Pro MfRdjJ.ini file
   ```

3. Press **Install** if the Caddy service is not installed yet.

4. Stop FxServe, then press **Start** in CaddyAdmin.

5. In RDJ Pro Setup, set the streaming output folder to:

   ```text
   \\YOUR_SERVER1\Caddy\stream
   ```

6. Start broadcasting in RDJ Pro.

7. Test the public address:

   ```text
   https://factoryxradio.asuscomm.com/
   ```

### Standard RDJ with Caddy

For standard RDJ, use these Caddy settings instead:

```text
Service:             RDJCaddy
Caddy share:         \\YOUR_SERVER1\Caddy_RDJ
Server local path:   C:\Caddy_RDJ
Streaming folder:    \\YOUR_SERVER1\Caddy_RDJ\stream
```

## Router setup

The router settings are the same for Caddy and FxServe:

| Service | Protocol | External port | Internal port | Destination |
| --- | --- | ---: | ---: | --- |
| Radio HTTP | TCP | 80 | 80 | Server IP address |
| Radio HTTPS | TCP | 443 | 443 | Server IP address |

Leave **Source IP** empty.

Do not forward public port 8080. FxServe uses port 8080 only for the local
network.

## Changing between FxServe and Caddy

Each server has its own administration tool because their settings differ.
Stop Caddy with CaddyAdmin before starting FxServe with FxServeAdmin. To return
to Caddy, stop FxServe first and then start Caddy with CaddyAdmin. Only one can
own ports 80 and 443 at a time.

## More information

See the [FxServe White Paper](FxServe-White-Paper.md) for the architecture,
security model, HTTPS renewal flow, limits, and MfPack scope.

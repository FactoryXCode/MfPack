## **MfRdj Install/Setup instructions**

**Icecast**
Install IceCast on the computer that will be the broadcast server (local network) or just your own computer.
The install file can be found at ..\MfPack\Samples\MfRdjJ\Binaries\icecast_win64_2.5.0.zip
When done, replace the icecast.xml with the icecast.xml,
located at: ..\MfPack\Samples\MfRdjJ\Binaries\IceCast_defXML\icecast.xml  

**Caddy**
Install Caddy by unpacking the zip (..\MfPack\Samples\MfRdjJ\Binaries\Caddy_PWA.zip)
By default we advise you to copy the contents to C:\Caddy on the same machine IceCast is installed.

To make all work correctly, you should activate port forwarding in your router.

*Service Name	External Port	Internal Port	Internal IP Address  Protocol*
CaddyHTTP	    80	                80	               192.168.xxx.xxx	    TCP		
CaddyHTTPS     443                  443                  192.168.xxx.xxx	    TCP

**When your router supports DDNS, do as follows:**
DDNS: Enable
// Note that these are depended of your router brand (in this case we have an Asus router). 
Server: For example www.asus.com
Host Name: YourRadioStationName.asuscomm.com
// You need this for secure https connections.
HTTPS/SSL Certificate: Free Certificate from Let's Encrypt (enable) 

Now register and Apply to your router settings.


## MfRdj Project setup
Before opening the project, first install the MfPack components (MfPack\Samples\Components) and the RDJ components (MfPack\Samples\MfRdjJ\Controls).
After installing the components, open the  project and add the following Search Paths: 
  ..\MfPack\src
  ..\MfPack\Samples\MfRdjJ\Controls
  ..\MfPack\Samples\MfComponents

Set 
  Output Directory: .\$(Platform)\$(Config)
  Unit Output Directory: .\$(Platform)\$(Config)
  
  
Build the RDJ project.
No hints, Warnings or error should occure.

## MfRDJ Applicationsetup

**Run MfRDJ**
At first time MfRDJ will create the setup ini file and opens the Setup dialog.

**Start with tab "General"**
Select Main output (Master) This endpoint should be wired with your PA speakers.
Click Enable Headphones output (PFL/Cue) this should be wired to your headphone.

Choose the number of channel decks (max = 8)
Choose the number of loopback decks (max = 2)
Set capture buffersize (default is 60 ms)
Enable microphone input and choose your listed microphone.

**Audio recorder**
set capture buffersize (default is 60 ms)
Set Latency (Default is 100 ms)

Enable "Don't overwrite files to active"
Enable Sstreamswitch detection".

**Open tab "Broadcast"**
If you don't want to use IceCast streaming, you can skip this (close the setup dialog) 

**IceCast Settings**
Host: 127.0.0.1                Port: 8000
Mount: /live
User name: Source
Password: Your_Password
Broadcast name: Your broadcast station name
Description: A brief description, like show title.
Genre: Various
Broadcast URL: Can be empty
Enable Public Stream if you want to stream over the Internet. 

**IceCast Server Manager**
Host: 127.0.0.1         Port: 8000
EXE Path: C:\Icecast\bin\icecast.exe (Or where ever you installed IceCast)
Config Path: C:\Icecast\icecast.xml (Or where ever IceCast's configfile is (XML))
HTTP PATH: /
Working Dir: C:\Icecast (Or where ever you installed IceCast)
Startup Delay: 3000 (default 3 seconds)

**Caddy/json settings**
Caddy Path: C:\Caddy (Or where ever you installed Caddy)
Caddy Config Path: C:\Caddy\caddy.cff (Caddy's config file)
Caddy json Path: C:\Caddy\nowplaying.json
Caddy Command: caddy.exe run --config "C:\Caddy\Caddy.cff" --adapter caddyfile (leave this untouched, unless you know what you are doing).

Close Setup (**OK**)

<eof>       
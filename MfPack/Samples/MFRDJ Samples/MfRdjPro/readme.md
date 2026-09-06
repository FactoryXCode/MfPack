MfRdj Pro
  
Version: 4.0.0
  
Description:  
Requires Windows 10 with latest updates or later.  
  
NOTES:  
This release is updated for compiler version 34 (Delphi 12).  
The minimum version for your compiler is Delphi XE7.  
SDK version 10.0.28000.2705 (Win 11)  
Minimum supported MfPack version: 3.2.0  
  
A dj mixer app based on MfPack/Media foundation/core audio/WasApi.  
  
This is the extended MfRdjJ that features audio and video streaming.  
This version does not need Icecast. FxServe or Caddy can serve the browser broadcast.  
  
What it does:  
Uses MDI adding channeldecks, loopbackdecks, a microphonedeck,  
effectsdeck (mft's) and enddeck (pa \& pfl, recorder (saves to WAV or FLAC) and browser broadcasting support),  
playlisteditor and a tag editor (currently mp3 only).  

## Web server choice

**FxServe** is the normal choice for a small RDJ Pro server. It serves the web
application, artwork, metadata, and live fMP4 stream without Icecast. **Caddy**
remains available when the server hosts more websites or needs extra routing and
proxy features.

For setup paths, router settings, and switching instructions, see
[RDJ Web Server Setup](../../FxServe/RDJ-Web-Server-Setup.md).
  
The aim of this sample project is: Combine all kinds of MfPack (coreAudio WASAPI ) challenges in a single project.  
Now, the example has become pretty large for a sample but usefull for audio/video knights..  
  
Advised minimum Delphi version: Delphi XE7 and higher.
  
NOTES:  
  
* The code is not suitable for starters and knowledge of audio manipulation/algo's is also required.
* Before opening the project, install the MfRdjJ controls (..\\MfPack\\Samples\\MfRdjPro\\Controls\\RDJControls.dpk)
and MfPack MfComponents (..\\MfPack\\Samples\\MfComponents\\MfComponents.dpk)
Add "..\\MfPack\\Samples\\MfComponents", "..\\MfPack\\Samples\\MfRdjPro\\Controls" and
"..\\MfPack\\src" to the RDJ Pro project search path.
  
Project: MFPack - Samples - MfRdjPro  
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 05-02-2026  
Final release date: 22/06/2026  
  
Copyright  FactoryX. All rights reserved.


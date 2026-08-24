**WASAPI Player Sample 4**

&#x20; 

WASAPI High / Mid / Low EQ and Compressor / Limiter DSP Sample  

&#x20; 
Version: 4.0.0

&#x20; 

Description  
This sample demonstrates a real-time audio playback engine on Windows using:  

* WASAPI (event-driven, shared mode)
* Media Foundation SourceReader
* A custom 3-band EQ implemented as an IMFTransform (MFT)
* A Compressor / Limiter DSP
* Thread-safe GUI → engine command routing
* Persistent EQ settings stored via INI file

\---  

Features  

* Real-time playback
* Low / Mid / High EQ with live control
* Mid band supports Peaking or Notch mode
* Adjustable Q (bandwidth) and shelf slopes
* Settings dialog with persistent storage
* Compressor / Limiter DSP with live control from the settings menu

&#x20; 

Architecture  
GUI → Engine Thread → EQ MFT → DSP → WASAPI Render  

The engine thread owns all real-time audio resources.  

&#x20; 

Notes  
Updated for Delphi compiler versions 17 through 34.  
Windows SDK 10.0.28000.2705 (Windows 11).  
Requires Windows 10 or later.  
Minimum supported MfPack version: 3.1.9  

&#x20; 

Project Information  
Project: Media Foundation – MfPack – Samples  

&#x20; 

Repository:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  

&#x20; 

Release Information  
First release: 13/08/2025  
Final release: 05/05/2026  

&#x20; 

(c) FactoryX. All rights reserved.


WASAPI Player Sample 3

&#x20; 
WASAPI High / Mid / Low EQ Sample (Delphi)

&#x20; 
Version: 4.0.0

&#x20; 

Description:

&#x20; 
This sample demonstrates a real-time audio playback engine on Windows using:  

* WASAPI (event-driven, shared mode).
* Media Foundation SourceReader.
* A custom 3-band EQ implemented as an IMFTransform (MFT).
* Thread-safe GUI → engine command routing.
* Persistent EQ settings via INI file.

&#x20; 

Features  

* Glitch-free real-time playback.
* Low / Mid / High EQ with live control.
* Mid band supports Peaking or Notch mode.
* Adjustable Q (bandwidth) and shelf slopes.
* Settings dialog with persistent storage.

&#x20; 

Architecture  
GUI → Engine Thread → EQ MFT → WASAPI Render  
The engine owns all real-time resources and  
the GUI never touches audio objects directly.  

&#x20; 

NOTES:  
This release is updated for compiler version 17 up to 34.  
SDK version 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 3.1.8  

&#x20; 
Project: Media Foundation - MFPack - Samples  

&#x20; 
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  

&#x20; 

First release date: 13/08/2025  
Final release date: 05/05/2026  

&#x20; 

Copyright © FactoryX. All rights reserved.


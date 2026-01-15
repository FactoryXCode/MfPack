***WASAPI Player Sample 3***
 WASAPI High / Mid / Low EQ Sample (Delphi)
Version: X 3.1.9

Description:

This sample demonstrates a real-time audio playback engine on Windows using:

⦁ WASAPI (event-driven, shared mode).
⦁ Media Foundation SourceReader.
⦁ A custom 3-band EQ implemented as an IMFTransform (MFT).
⦁ Thread-safe GUI → engine command routing.
⦁ Persistent EQ settings via INI file.

**Features**

- Glitch-free real-time playback.
- Low / Mid / High EQ with live control.
- Mid band supports Peaking or Notch mode.
- Adjustable Q (bandwidth) and shelf slopes.
- Settings dialog with persistent storage.
- Clean separation between GUI, engine, and DSP.

**Architecture**

GUI → Engine Thread → EQ MFT → WASAPI Render

The engine owns all real-time resources.
The GUI never touches audio objects directly.

***NOTES:***

- This release is updated for compiler version 17 up to 34.
- SDK version 10.0.22621.4654 (Win 11)
- Requires Windows 10 or later.
- Minimum supported MfPack version: 3.1.7

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
https://sourceforge.net/projects/MFPack
First release date: 13/08/2025
Final release date: 15/01/2026
Copyright © FactoryX. All rights reserved.

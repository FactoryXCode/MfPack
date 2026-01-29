# WASAPI Player Sample 4

**WASAPI High / Mid / Low EQ and Compressor / Limiter DSP Sample**  
**Version:** X 3.1.9

---

## Description

This sample demonstrates a **real-time audio playback engine on Windows** using:

- **WASAPI** (event-driven, shared mode)
- **Media Foundation SourceReader**
- A custom **3-band EQ** implemented as an **IMFTransform (MFT)**
- A **Compressor / Limiter DSP**
- Thread-safe **GUI → engine** command routing
- Persistent EQ settings stored via **INI file**

---

## Features

- Glitch-free real-time playback
- Low / Mid / High EQ with live control
- Mid band supports **Peaking** or **Notch** mode
- Adjustable **Q (bandwidth)** and shelf slopes
- Settings dialog with persistent storage
- Compressor / Limiter DSP with live control from the settings menu
- Clean separation between **GUI**, **engine**, **EQ**, and **DSP**

---

## Architecture

```
GUI → Engine Thread → EQ MFT → DSP → WASAPI Render
```

- The **engine thread** owns all real-time audio resources  
- The **GUI never touches audio objects directly**

---

## Notes

- Updated for **Delphi compiler versions 17 through 34**
- Built against **Windows SDK 10.0.22621.4654 (Windows 11)**
- Requires **Windows 10 or later**
- Minimum supported **MfPack version: 3.1.9**

---

## Project Information

- **Project:** Media Foundation – MfPack – Samples  
- **Repository:**  
  - https://github.com/FactoryXCode/MfPack  
  - https://sourceforge.net/projects/MFPack  

---

## Release Information

- **First release:** 13/08/2025  
- **Final release:** 23/01/2026  

---

## Copyright

© FactoryX. All rights reserved.

# WASAPI Player Sample 5
Version: X 3.1.9

This is a non-visual components application sample using WASAPI.
The sample shows you how to build a real-time audio playback engine for Windows using Delphi style components.  

---

## Description

This sample also demonstrates how to create and implement a:

- Compressor/Limiter component using a MFT.
- Parametric Equalizer component using a MFT.
- Flanger/Echo component using a MFT.
- WasApi effects rack that connects al MFT components to the WasApi renderer.
- PlayerEngine component, using a WasApi renderer.
---

## Features

A set of components based on DSP and MFT's. Before using this sample, you have to install the MfComponents ()

---

## Architecture

```
GUI → Engine Thread → EQ MFT → DSP → WASAPI Renderer
```
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
- **Final release:** 05/05/2026  

---

## Copyright

© FactoryX. All rights reserved.

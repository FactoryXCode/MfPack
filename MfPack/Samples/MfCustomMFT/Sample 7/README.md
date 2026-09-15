# Sample 7 - The super trick - FFmpeg DTS audio decoder MFT.
  
This sample packages a DTS decoder as a synchronous Media Foundation audio  
decoder. The public component is a Delphi `IMFTransform`; a small C bridge  
keeps FFmpeg's version-dependent structures out of the Delphi API.  
  
## Layout
  
```text
Bridge/        Stable C API and FFmpeg implementation
Delphi/         Media Foundation transform and COM server
Installer/        Inno Setup package for Win32 and Win64
Runtime/      Staged architecture-specific DLLs (generated, not hand-edited)
ThirdParty/   Exact FFmpeg source archive, license, configure line and diff
```
  
The installed component is intended for Cast V2.1, MfPlayerX, and any other
MfPack program that asks Media Foundation for decoded DTS/PCM.
  
## Decoder contract
  
- Input: DTS core, raw DTS, DTS-HD, DTS-XLL and DTS-LBR.
- Output: signed 16-bit interleaved stereo PCM at 48 kHz.
- Downmixing and resampling are performed by `libswresample`.
- Win32 and Win64 DLL sets are built from the same pinned FFmpeg source.
  
The MFT rejects activation when its matching FFmpeg runtime is absent. It must
never claim support and then silently select a different audio stream.
  
## Registration and packaging
  
The installer registers both COM servers and both MFT registry views.  
In sample 8 application-local deployment can use `MFTRegisterLocal` with the same class  
factory without changing the decoder.  
  
The FFmpeg build uses shared libraries without GPL or non-free components.  
The package includes the exact source archive, configure commands, no-changes  
declaration, and LGPL text. `Installer/Build-Package.ps1` refuses incomplete  
packages. Its current output is `Installer/Output/MfPackDtsDecoderMFT-1.0.0.exe`.  
Once the exe is build, you can install it on your system, and when done,  
you have a registered DTS MFT.  
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 14/09/2026  
  
Copyright © FactoryX. All rights reserved.


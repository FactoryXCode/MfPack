# Sample 12 - Registered audio delay MFT

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

This sample packages the Sample 9 `TMfAudioDelayMFT` as a Win32 in-process COM
DLL and registers it as a synchronous Media Foundation audio effect. The delay
implementation still derives from `TInterfacedObject`; Delphi supplies its
`QueryInterface`, `_AddRef`, and `_Release` methods. A small `IClassFactory`
creates one transform per activation.

## Registered identity

```text
Friendly name: FactoryX PCM Audio Delay MFT
CLSID:         {A63BF709-564A-4F5B-A6B4-9EA48E0FC9AE}
Category:      MFT_CATEGORY_AUDIO_EFFECT
Input:         Audio / PCM
Output:        Audio / PCM
Flags:         MFT_ENUM_FLAG_SYNCMFT
```

The PCM registration describes the major type and subtype. The transform
checks the complete runtime media type and supports mono or stereo, unsigned
8-bit or signed 16-bit PCM. Registration uses the same CLSID for the COM class
and the MFT entry. `DllUnregisterServer` removes both entries.

## Build and verify in the IDE

1. Open `FactoryXAudioDelayMFT.dproj` in Delphi.
2. Select **Win32 Debug** and choose **Project > Build FactoryXAudioDelayMFT**.
3. Run `Register Win32.cmd` as administrator. It uses 32-bit `regsvr32` to
   register `Win32\Debug\FactoryXAudioDelayMFT.dll`.
4. Open `..\..\MfMftExplorer\MfMftExplorer.dproj` in the IDE, build its **Win32
   Debug** target, and press **F9**.
5. Select **Audio effects** and **All registered MFTs** or **Synchronous
   software MFTs**, then enumerate.
6. Select **FactoryX PCM Audio Delay MFT** and click **Activate and inspect**.
7. Open `MfRegisteredAudioDelayPlayer.dproj`, build **Win32 Debug**, and press
   **F9**. Open an audio file and play it. The player enumerates registered
   audio effects, finds this CLSID, and activates the DLL. The delay and wet
   mix sliders continue to update the effect during playback.

MfMftExplorer should show the friendly name, CLSID, Audio/PCM registered input
and output types, successful `IMFTransform` activation, and one input and one
output stream. The Explorer already supports audio effects; it needs no code
change for this sample. Use its Win32 build to inspect this Win32 DLL. The
player uses the same registered transform without linking the delay
implementation into its executable.

Close MfMftExplorer and other applications using the DLL before rebuilding or
unregistering it. Registration stores the absolute DLL path. Keep the DLL at
that path while registered.

## Remove the registration

Run `Unregister Win32.cmd` as administrator. Enumerate **Audio effects** again
in MfMftExplorer; the FactoryX entry should be absent.

This sample changes machine-wide COM and Media Foundation registration. Sample
11 continues to create its application-local MFT; this sample has its own VCL
form and player project to demonstrate registered activation.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.

# Sample 8 - Application-local DTS decoder

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

Sample 7 installs its DTS decoder as a system-wide COM server and Media
Foundation Transform. This sample uses the same `MfPackDtsMFT.dll` without an
installer or registry changes. The application loads the DLL from its own
directory, obtains its exported `IClassFactory`, and calls `MFTRegisterLocal`.
Only this process can discover the registration. The decoder implementation
and its FFmpeg bridge are unchanged.

`MfLocalDtsClient.exe --check` enumerates the decoder with
`MFT_ENUM_FLAG_LOCALMFT`, activates it, and negotiates DTS input to PCM output.
With a DTS media filename instead, the program also asks Media Foundation's
Source Reader to decode the selected audio stream to 48 kHz, 16-bit stereo PCM
and reports the number of samples and bytes. It releases reader objects,
calls `MFTUnregisterLocal` with the same class factory, then unloads the DLL.

## Build and run

Build Sample 7's Win32 and Win64 runtime sets first. Then build this Delphi
console project in the IDE, or stage both architectures with:

```powershell
.\Build-LocalPackage.ps1
```

The script uses `dcc32.exe` and `dcc64.exe` from the path and copies the app
and the five matching Sample 7 DLLs into `Deployment/Win32` and
`Deployment/Win64`. Run the desired architecture from its deployment folder:

```powershell
.\Deployment\Win64\MfLocalDtsClient.exe --check
.\Deployment\Win64\MfLocalDtsClient.exe 'C:\media\movie-with-dts.mkv'
```

The application and all five runtime DLLs must stay together. A Win32 host
needs Win32 DLLs; a Win64 host needs Win64 DLLs. The local registration ends
when the process exits, so other applications do not see this decoder. The
Sample 7 installer remains available when machine-wide discovery is wanted.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.
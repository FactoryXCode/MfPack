# HResult Lookup Tools (WinApi.Dbg.WinHResultTools.pas)

&#x20; 

Version 3.1.9

&#x20; 

The application can be used to determine HResult or Error codes returned by the Windows OS,  
featuring a detailed build in "HResult Lookup tool", the "Windows System Error Code Lookup Tool" and  
the "System.SysUtils.SysErrorMessage" function.  
A tool to create a new or translate a new HRESULT using Severety, Facility and Error code is also provided.  

You can use procedure RunFxErrLookUpTool in this unit to use the HResult Lookup Tools Application,  
located at ../MfPack/Tools.  

&#x20; 

The Windows System Error Code Lookup Tool can be downloaded at:  
https://learn.microsoft.com/en-us/windows/win32/debug/system-error-code-lookup-tool.  
When you need a version > 6.4.5, that is allready present, copy this tool  
to ..\\MfPack\\Tools\\MicrosoftErrorLookupTool and adjust the WIN\_ERROR\_LOOKUP\_TOOL constant in  
the WinApi.Dbg.WinHResultTools.pas unit.  

To build the tools application, you have to add ..\\MfPack\\src\\dbg to your project's Searchpath.  

&#x20; 

NOTES:  

* This release is updated for compiler version 17 up to 34.
* SDK version 10.0.22621.4654 (Win 11)
* Requires Windows 10 build 20348 or later.
* Minimum supported MfPack version: 3.1.5

&#x20; 

Project: Media Foundation - MFPack - Samples

&#x20; 
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  

&#x20; 

First release date: 09/07/2023  
Final release date: 05/05/2026  

&#x20; 

Copyright © FactoryX. All rights reserved.


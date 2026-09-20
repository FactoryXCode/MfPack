# MfProtectedPlayback  sample

Version: 4.0.0

NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version 10.0.28000.2705 (Win 11)  
Requires Windows Vista or later.  
Minimum supported MfPack version: 4.0.0  
  
`MfProtectedPlayback` demonstrates playback of clear and protected media with
Microsoft Media Foundation and the Protected Media Path (PMP). It is a close
Delphi translation of Microsoft's `ProtectedPlayback` C++ sample and uses the
Media Foundation declarations supplied by MfPack.
  
The port intentionally preserves the structure, class names, method names, and
control flow of the original sample wherever Delphi permits. The application
uses a conventional VCL form consisting of `MainForm.pas` and `MainForm.dfm`.
  
## Features

- Creates an `IMFMediaSession` inside the Protected Media Path with
  `MFCreatePMPMediaSession`.
- Builds a partial playback topology for every selected source stream.
- Uses the Media Foundation audio renderer and Enhanced Video Renderer (EVR).
- Plays clear media through the same PMP session used for protected content.
- Implements `IMFContentProtectionManager` and `IMFContentEnabler` handling.
- Supports silent and legacy non-silent license acquisition.
- Opens local files and URLs without starting playback automatically.
- Provides Play, Pause, and Stop controls; Space pauses or resumes playback.
- Displays Media Foundation events, HRESULT values, stream information, and
  content-enabler activity in a log at the bottom of the form.
  
## Source files
  
| File | Purpose |
| --- | --- |
| `MfProtectedPlayback.dpr` | Application entry point and COM initialization. |
| `MfProtectedPlayback.dproj` | Delphi XE7 Win32 project. |
| `MainForm.pas` | VCL application logic and window-message handlers. |
| `MainForm.dfm` | VCL form, menu, video panel, open dialog, and log layout. |
| `Player.pas` | PMP session, media source, topology, playback, and EVR handling. |
| `ContentEnabler.pas` | Content-protection manager and license-acquisition state machine. |
| `WebHelper.pas` | Internet Explorer automation used for legacy non-silent acquisition. |
| `SampleLog.pas` | Thread-safe UI logging and Media Foundation event names. |
  
The corresponding Microsoft C++ sources are located in
`../CPP/protectedplayback`.
  
## Requirements
  
- Delphi XE7 or a later supported Delphi compiler.
- MfPack source units.
- Windows Vista or later; Windows 10 or Windows 11 is recommended for testing.
- Win32 target for compatibility with the legacy Windows Media DRM path.
- Media Foundation codecs appropriate for the files being played.
- Genuine WMDRM-protected WMA or WMV content to exercise license acquisition.
- A functioning license-acquisition service for end-to-end DRM testing.
  
This sample does not implement modern streaming DRM systems such as PlayReady
DASH, Widevine, or FairPlay.
  
## Building with Delphi
  
1. Open `MfProtectedPlayback.dproj` in Delphi.
2. Select the **Win32** target platform.
3. Confirm that the unit search path contains `../../../src`.
4. Build and run the project.
  
The relative search path assumes that the sample remains in its current MfPack
directory. Update the path if the sample is moved elsewhere.
  
The project is a Windows GUI application and does not create a console window.
  
## Using the application
  
1. Choose **File > Open File** to select a local media file, or choose
   **File > Open URL** to enter a media URL. The source is opened and its
   topology is prepared, but playback does not start automatically.
2. Click **Play** to start or resume, **Pause** to pause, or **Stop** to stop.
   Space can also be used to pause or resume.
3. Resize the window to verify EVR video resizing.
4. Review the log pane for topology, session, playback, and DRM events.
  
Clear MP3, WMA, WMV, AVI, and MP4 files can be used to test ordinary playback,
subject to the codecs installed on the machine. Successful clear playback does
not exercise the content-enabler code.
  
## Testing protected playback
  
Protected playback requires a compatible WMDRM-protected WMA or WMV file. Three
important scenarios should be tested:
  
1. A valid license is already installed. Playback should start without showing
   a browser.
2. Silent acquisition is supported. The log should show the content enabler,
   automatic acquisition, `MEEnablerCompleted`, and completion of the operation.
3. Silent acquisition does not obtain a license. The application should attempt
   non-silent acquisition and open the license URL with its HTTP POST data.
  
Closing the browser during non-silent acquisition causes the application to
cancel the pending content-enable operation.
  
## Internet Explorer automation
  
For fidelity with the original Microsoft sample, `WebHelper.pas` creates the
legacy `InternetExplorer.Application` COM automation object. This is used only
for non-silent DRM acquisition. Ordinary playback and silent acquisition do not
require it.
  
The IE desktop application is retired, but the automation COM class remains
available on the latest Windows 10 and Windows 11 systems, except some corporate versions.
Its availability can also be affected by Windows updates and enterprise policy.
It can be checked from PowerShell:

```powershell
$ie = New-Object -ComObject InternetExplorer.Application
$ie.Visible = $true
$ie.Navigate('about:blank')
$ie.Quit()
```
  
If creation of the COM object fails, non-silent acquisition will fail with the
corresponding COM activation HRESULT.
Note: The sample does not automate Microsoft Edge or Edge IE mode.
  
## Diagnostic log
  
The log records:
  
- Media Foundation startup and PMP session creation.
- Source resolution and topology construction.
- Stream selection and renderer activation.
- Media Session events and their HRESULT status.
- EVR service discovery and playback state changes.
- Content-enabler events and silent/non-silent acquisition decisions.
- License URL trust status and POST-data size.
- Browser startup, cancellation, and completion.
  
License POST contents and the complete license URL are deliberately not written
to the log because they can contain sensitive information.
  
## Notes and limitations
  
- `MFCreatePMPMediaSession` is used for both clear and protected media, matching
  the Microsoft sample.
- The media source is created synchronously. Opening a network URL can therefore
  temporarily block the UI.
- Available formats depend on the Media Foundation codecs installed in Windows.
- The sample demonstrates the legacy Windows Media DRM content-enabler model;
  obtaining suitable test content and a working license server can be difficult.
  
## Origin and license
  
This Delphi sample is derived from Microsoft's Media Foundation
`ProtectedPlayback` and `CPlayer` examples.
  
Project: Media Foundation - MFPack - Samples  
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 05-02-2016  
Final release date: 18/09/2026  
  
Copyright © FactoryX. All rights reserved.



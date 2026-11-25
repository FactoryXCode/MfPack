# MfSimpleCapture — Sample 3: Media Foundation camera controls

NOTES:
  
* This release is updated for compiler version 23 up to 34.  
* SDK version 10.0.28000.2705 (Win 11)  
* Minimum supported MfPack version: 4.1.0
  
  
This sample is based on **MfSimpleCapture Sample 2**.  
It uses the Media Foundation camera source, Media Session, partial capture topology and EVR preview and uses native source controls and the newer MF camera interfaces for configuration.  
  
| Sample | Camera-control implementation |
| --- | --- |
| Sample 2 | Existing `IAMCameraControl` and `IAMVideoProcAmp` example |
| Sample 3 | MF camera source + `IKsControl`, `IMFExtendedCameraController`, camera-defaults interfaces and control-change notifications |
  
  
## Running the sample  
  
Open `MfSimpleCapture.dproj` in Delphi and select Win32 or Win64, Debug or Release.  
  
1. Click **Select capture device**, choose a camera, and click OK. With **Start preview automatically** checked, the MF Media Session starts the EVR preview.
2. Click **MF camera controls**. The dialog stays open beside the preview.
3. On **Live controls**, select a property. Its current value, range, step and auto/manual capabilities come from the selected device. Enter a valid value or choose automatic mode, then click **Apply live value**. Unsupported controls remain unavailable.
4. On **Extended MF controls**, inspect the driver's support, flags and capabilities for HDR, optical stabilization, eye-gaze correction and background segmentation. Only supported flag modes can be selected. These features are hardware/driver dependent.
5. On **Saved camera defaults**, reload the current user's saved settings. **Save selected live control as default** saves the selected standard control's current value and mode for future camera opens. **Remove selected saved default** removes only that selected standard control. Other saved controls are preserved.
6. **Close camera** shuts down preview, notifications and controls. The camera can then be reopened or changed.
  
  
## For indepth information, please read:  
  
- [IMFCameraConfigurationManager](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameraconfigurationmanager)
- [IMFCameraControlDefaults](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameracontroldefaults)
- [IMFCameraControlDefaultsCollection](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameracontroldefaultscollection)
- [IMFExtendedCameraController](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfextendedcameracontroller)
- [IMFExtendedCameraControl](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfextendedcameracontrol)
- [IMFCameraControlMonitor](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameracontrolmonitor)
- [Camera companion apps](https://learn.microsoft.com/en-us/windows-hardware/drivers/stream/camera-companion-apps)

  
Project: MFPack - Samples - MfSimpleCapure 3  
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 05-05-2016  
Final release date: 25/11/2026  
  
Copyright © FactoryX. All rights reserved.


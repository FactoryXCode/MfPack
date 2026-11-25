# MfSimpleCapture — Sample 3: Media Foundation camera controls

This Delphi/VCL learning sample is based on **MfSimpleCapture Sample 2**. It retains the Media Foundation camera source, Media Session, partial capture topology and EVR preview, and uses native source controls and the newer MF camera interfaces for configuration.

| Sample | Camera-control implementation |
| --- | --- |
| Sample 2 | Existing `IAMCameraControl` and `IAMVideoProcAmp` example |
| Sample 3 | MF camera source + `IKsControl`, `IMFExtendedCameraController`, camera-defaults interfaces and control-change notifications |

Sample 2's files are unchanged. Sample 3 contains no calls to `IAMCameraControl` or `IAMVideoProcAmp`, and creates no DirectShow graph. Basic UVC controls such as brightness and exposure are kernel-streaming properties obtained from the **MF media source**. `IKsControl` handles those standard property sets. Extended properties use the dedicated MF extended-controller interfaces, as Microsoft requires.

## Try the sample

Open `MfSimpleCapture.dproj` in Delphi and select Win32 or Win64, Debug or Release.

1. Click **Select capture device**, choose a camera, and click OK. With **Start preview automatically** checked, the MF Media Session starts the EVR preview.
2. Click **MF camera controls**. The dialog stays open beside the preview.
3. On **Live controls**, select a property. Its current value, range, step and auto/manual capabilities come from the selected device. Enter a valid value or choose automatic mode, then click **Apply live value**. Unsupported controls remain unavailable.
4. On **Extended MF controls**, inspect the driver's support, flags and capabilities for HDR, optical stabilization, eye-gaze correction and background segmentation. Only supported flag modes can be selected. These features are hardware/driver dependent.
5. On **Saved camera defaults**, reload the current user's saved settings. **Save selected live control as default** saves the selected standard control's current value and mode for future camera opens. **Remove selected saved default** removes only that selected standard control. Other saved controls are preserved.
6. **Close camera** shuts down preview, notifications and controls. The camera can then be reopened or changed.

The main form also includes Sample 1's **Rotation** selector (0, 90, 180 and 270 degrees). It sets `MF_MT_VIDEO_ROTATION` before topology resolution. Changing the angle reopens the selected source and preserves the automatic/deferred preview choice. This is video-frame rotation; the separate **Roll** entry in Live controls is a hardware-dependent camera property.

To configure a pre-start feature such as HDR, close the camera, uncheck **Start preview automatically**, then reopen it. Set the supported feature before clicking **Start preview**. With automatic preview disabled, Sample 3 postpones topology resolution as well as streaming, since pre-start controls can change the available media types. Driver-specific constraints and unsupported combinations are reported through HRESULT results.

Live changes and persistent defaults are separate actions. Merely opening the dialog, refreshing capabilities or reading saved defaults does not save settings. Changing a saved default does not automatically replace a live value in an already-running capture session.

## Native APIs in the code

| Code | Purpose |
| --- | --- |
| `MfDeviceCaptureClass.pas` | Sample 2's capture pipeline, connected to the MF camera-control helper; supports deferred preview for pre-start controls |
| `MfCameraControls.Attach` | Gets `IKsControl` from the MF source and requests `IMFExtendedCameraController` through `IMFGetService` with `GUID_NULL` |
| `ReadStandard` / `ReadRange` | Reads standard camera/video controls and parses bounded `KSPROPERTY_TYPE_BASICSUPPORT` data |
| `ReadStoreRange` | Uses a temporary `IMFCameraControlDefaultsCollection` and `IMFCameraControlDefaults.GetRangeInfo` to retrieve the reported range/default; does not call `SaveDefaults` |
| `ApplyStandard` | Sends a validated absolute value/auto-manual mode to the source's `IKsControl` interface |
| `ApplyExtended` | Gets `IMFExtendedCameraControl`, checks capabilities, calls `SetFlags`, then `CommitSettings` |
| `DescribeDefaults` | `IMFCameraConfigurationManager.LoadDefaults`, collection enumeration, `LockControlData`/`UnlockControlData`, and pre/post-start type inspection |
| `SaveStandardDefault` | Loads a fresh collection, updates one standard control's payload, unlocks it and calls `SaveDefaults`, preserving other entries |
| `StartMonitor` / `StopMonitor` | `MFCreateCameraControlMonitor`, `IMFCameraControlMonitor` and `IMFCameraControlNotify` for changes made by this or another camera client |

`dlgMfCameraControls.pas/.dfm` defines all editor controls in the VCL designer. It uses a dedicated notification window. The monitor callback posts a scalar message under a lock; it never accesses VCL controls. Detachment prevents posting after the window is destroyed, and generation checks reject messages belonging to an earlier source. The monitor is stopped before camera shutdown.

COM and MF startup are paired with shutdown after the form and capture engine have been destroyed. Device enumeration reads MF activation metadata without opening the camera. A source is opened by its symbolic link, with one source instance per capture session.

## Requirements and scope

- Delphi/VCL; Win32 and Win64; MfPack headers from the current source tree.
- Windows 10 or later for this sample. Optional extended controls depend on the Windows version and camera driver.
- Saved defaults and control-change monitoring require **Windows 11 build 22621 or later**. They are detected at runtime. Older systems can still use supported source-level live controls; their unavailable optional features are reported in the dialog.
- This example edits standard absolute controls and selected known extended flag modes. It enumerates saved controls of other types for inspection, but does not edit arbitrary vendor payloads or save extended-control payloads.
- The local test camera supports exposure and several video-proc-amp controls. It does not advertise the four demonstrated extended features, so those writes require testing on hardware that supports them.

## MfPack declaration corrections used by Sample 3

The SDK audit and Win32 hardware tests identified these corrections in the Delphi header tree:

- `IMFCameraControlMonitor.AddControlSubscription` and `RemoveControlSubscription` take a **GUID by value**, unlike the notification callback's `REFGUID`.
- `IMFCameraControlMonitor.Shutdown` uses `stdcall` and returns `void`.
- `KSPROPERTY_CAMERACONTROL_S` and `KSPROPERTY_VIDEOPROCAMP_S` require 40-byte native wire layouts. Win32 needs explicit trailing padding under these units' packed layout. The relevant field offsets are preserved.

The sample uses MfPack declarations and formatting throughout.

## Build and verification

```powershell
.\Build.ps1 -Platform Win64 -Config Debug -Smoke -Tests
.\Build.ps1 -Platform Win32 -Config Release -Smoke -Hardware
```

Set `-StudioRoot` or `-MfPackRoot` for another installation. Executables and logs are written under `Build\<Platform>\<Config>`, matching the IDE project.

`-Smoke` creates and destroys the forms without opening a camera. `-Tests` checks native payload sizes, range parsing and validation, and unbound dialog initialization. `-Hardware` additionally probes the local camera, exercises MF notifications and EVR preview, pre-start configuration, start/stop/restart and repeated shutdown/reopen. It applies the existing brightness setting unchanged to check the live write path. It verifies that saved defaults remain unchanged and does not call the persistent-save/remove actions.

The native GUI and hardware paths are tested on Win32 and Win64. Unsupported optional controls are a supported result, not a capture failure.

## Microsoft references

- [IMFCameraConfigurationManager](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameraconfigurationmanager)
- [IMFCameraControlDefaults](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameracontroldefaults)
- [IMFCameraControlDefaultsCollection](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameracontroldefaultscollection)
- [IMFExtendedCameraController](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfextendedcameracontroller)
- [IMFExtendedCameraControl](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfextendedcameracontrol)
- [IMFCameraControlMonitor](https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfcameracontrolmonitor)
- [Camera companion apps](https://learn.microsoft.com/en-us/windows-hardware/drivers/stream/camera-companion-apps)

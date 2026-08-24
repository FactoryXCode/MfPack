# WASAPI Stream Switch Detection
  
This document describes how **stream switch detection and recovery** is implemented in the WASAPI playback engine.
  
Stream switching occurs when the active audio endpoint becomes invalid during playback, for example:  
- Bluetooth headphones disconnect or power off.
- The default render device changes (Bluetooth ↔ speakers)
- The Windows audio service restarts.
- The user changes the default device or role (eMultimedia / eConsole)
  
Without handling this explicitly, WASAPI will return  
`AUDCLNT_E_DEVICE_INVALIDATED` and audio playback will stop.  
  
---
  
## Detection mechanisms  
  
The engine detects stream switches using **two complementary mechanisms**:
  
### 1. Endpoint notifications (primary)
An `IMMNotificationClient` is registered on the engine thread.
  
Relevant callbacks:  
- `OnDefaultDeviceChanged(eRender, Role, DeviceId)`
- `OnDeviceStateChanged`
- `OnDeviceRemoved`
  
When a relevant notification arrives:  
- The new device ID is stored (if provided)
- A stream switch is requested
- The engine thread is woken via `FCmdEvent`
  
No WASAPI calls are made from the callback thread.
  
---
  
### 2. Runtime invalidation (fallback)
Even with notifications, the audio client may be invalidated during playback.
  
The render loop explicitly checks for:  
- `AUDCLNT_E_DEVICE_INVALIDATED` returned from  
  `Start`, `GetCurrentPadding`, `GetBuffer`, or `ReleaseBuffer`  
  
When detected:  
- A stream switch is requested
- The engine thread rebuilds the audio client
  
This guarantees recovery even if notifications arrive late or not at all.
  
---
  
## Stream switch handling
  
When a switch is requested, the engine performs the following steps on the engine thread:
  
1. Capture the current playback timeline position (100-ns units)
2. Stop and reset the old `IAudioClient` (best effort)
3. Release all audio client interfaces
4. Resolve the new endpoint:
   - Use the pending device ID if available
   - Otherwise reopen the default render endpoint for the selected role
5. Activate a new `IAudioClient`
6. Reinitialize using the existing source format (`SetFormat`)
7. Rebind the event handle and reacquire service interfaces
8. Restore timeline continuity
9. Restart playback
  
The switch flag is cleared **only after** the new client starts successfully.
  
---
  
## Role awareness
  
The engine tracks an explicit `DeviceRole` (`ERole`):  
- `eMultimedia`
- `eConsole`
- `eCommunications`
  
Stream switch notifications are filtered by role, and all default-endpoint  
resolution uses the currently selected role.  
Changing the role at runtime triggers a controlled stream rebuild.  
  
---
  
## Threading model
  
- Endpoint notifications are received on COM callback threads
- All WASAPI teardown and reinitialization occurs on the engine thread
- Cross-thread signaling is done using atomic flags and events
- No WASAPI APIs are called from notification callbacks
  
---
  
## Result
  
With this design:  
- Bluetooth ↔ speakers switching is seamless
- Playback recovers automatically from device loss
- Timeline continuity is preserved
- No GUI freezes or invalid pointer operations occur
  
This behavior matches professional media players and DAW-style audio engines.  
  

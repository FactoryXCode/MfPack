// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - EndpointVolume
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.EndPointVolume.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Audio Endpoint Volume API
//              Provides Volume and muting control for an audio endpoint,
//              either per-channel or total volume.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
// 
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
// 
//          This unit has a different BOOL behaviour! See: Remarks for BOOL in
//          MfPack.MfTypes.pas
// 
//          Requires Windows Vista or later.
// 
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: endpointvolume.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
// 
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
// 
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
// 
//==============================================================================
unit WinApi.CoreAudioApi.EndPointVolume;

  {$HPPEMIT '#include "endpointvolume.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  {CoreAudioApi}
  WinApi.CoreAudioApi.DeviceTopology;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  // Hardware support mask - if the endpoint provides hardware support
  // for volume/mute/meter, this bit will be on in the
  // DWORD returned by the IAudioEndpointVolume QueryHardwareSupport and
  // the IAudioEndpointMeter QueryHardwareSupport API
  ENDPOINT_HARDWARE_SUPPORT_VOLUME    = $00000001;
  {$EXTERNALSYM ENDPOINT_HARDWARE_SUPPORT_VOLUME}
  ENDPOINT_HARDWARE_SUPPORT_MUTE      = $00000002;
  {$EXTERNALSYM ENDPOINT_HARDWARE_SUPPORT_MUTE}
  ENDPOINT_HARDWARE_SUPPORT_METER     = $00000004;
  {$EXTERNALSYM ENDPOINT_HARDWARE_SUPPORT_METER}


type

  PAUDIO_VOLUME_NOTIFICATION_DATA = ^AUDIO_VOLUME_NOTIFICATION_DATA;
  cwAUDIO_VOLUME_NOTIFICATION_DATA = record
    guidEventContext: TGUID;  // Context associated with the originator of the event.
    bMuted: BOOL;
    fMasterVolume: Single;
    nChannels: UINT;
    afChannelVolumes: array [0..128] of Single;
  end;
  {$EXTERNALSYM cwAUDIO_VOLUME_NOTIFICATION_DATA}
  AUDIO_VOLUME_NOTIFICATION_DATA = cwAUDIO_VOLUME_NOTIFICATION_DATA;
  {$EXTERNALSYM AUDIO_VOLUME_NOTIFICATION_DATA}


  // INTERFACES
  // ==========


  // Interface IAudioEndpointVolumeCallback
  // ======================================
  // Description:
  //
  //  Callback interface to be implemented by clients to respond to changes in
  //  the volume/mute state of an endpoint.
  //  Called when the endpoint volume changes.
  //
  // Parameters:
  //
  //  pNotify - [in] A pointer (PAUDIO_VOLUME_NOTIFICATION_DATA) to the structure
  //                 containing information about the new endpoint volume.
  //
  // Return values:
  //      S_OK if successful
  //      <other error> If registration for notification failed.
  //
  // Remarks:
  //      Please note: The caller of this function ignores all return
  //      codes from the OnNotify method.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointVolumeCallback);'}
  {$EXTERNALSYM IAudioEndpointVolumeCallback}
  IAudioEndpointVolumeCallback = interface(IUnknown)
  ['{657804FA-D6AD-4496-8A60-352752AF4F89}']

    function OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;

  end;
  IID_IAudioEndpointVolumeCallback = IAudioEndpointVolumeCallback;
  {$EXTERNALSYM IID_IAudioEndpointVolumeCallback}


  // >= Vista
  // Interface IAudioEndpointVolume
  // ==============================
  // Description:
  //
  //  Volume control interface for a device endpoint.
  //  Registers the client for change notifications on this volume control.
  //
  // Parameters:
  //
  //  pNotify - [in] Notification interface that gets called when volume changes.
  //
  // Remarks:
  //
  // Return values:
  //      S_OK if successful
  //      <other error> If registration for notification failed.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointVolume);'}
  {$EXTERNALSYM IAudioEndpointVolume}
  IAudioEndpointVolume = interface(IUnknown)
  ['{5CDF2C82-841E-4546-9722-0CF74078229A}']

    function RegisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;

    function UnregisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;

    function GetChannelCount(out pnChannelCount: UINT): HRESULT; stdcall;

    function SetMasterVolumeLevel(fLevelDB: Single;
                                  const pguidEventContext: LPCGUID): HRESULT; stdcall;

    function SetMasterVolumeLevelScalar(fLevel: Single;
                                        const pguidEventContext: LPCGUID): HRESULT; stdcall;

    function GetMasterVolumeLevel(out pfLevelDB: Single): HRESULT; stdcall;

    function GetMasterVolumeLevelScalar(out pfLevel: Single): HRESULT; stdcall;

    function SetChannelVolumeLevel(nChannel: UINT;
                                   fLevelDB: Single;
                                   const pguidEventContext: LPCGUID): HRESULT; stdcall;

    function SetChannelVolumeLevelScalar(nChannel: UINT;
                                         fLevel: Single;
                                         const pguidEventContext: LPCGUID): HRESULT; stdcall;

    function GetChannelVolumeLevel(nChannel: UINT;
                                   out fLevelDB: Single): HRESULT; stdcall;

    function GetChannelVolumeLevelScalar(nChannel: UINT;
                                         out fLevel: Single): HRESULT; stdcall;

    function SetMute(const bMute: INT {BOOL};  // See BOOL comments in MfpTypes.pas (See for a workaround example MfpMixer)
                     const pguidEventContext: TGUID): HRESULT; stdcall;

    function GetMute(out pbMute: INT {BOOL}): HRESULT; stdcall; // See BOOL comments in MfpTypes.pas (See for a workaround example MfpMixer)

    function GetVolumeStepInfo(out pnStep: UINT;
                               out pnStepCount: UINT): HRESULT; stdcall;

    function VolumeStepUp(const pguidEventContext: LPCGUID): HRESULT; stdcall;

    function VolumeStepDown(const pguidEventContext: LPCGUID): HRESULT; stdcall;

    function QueryHardwareSupport(pdwHardwareSupportMask: DWord): HRESULT; stdcall;

    function GetVolumeRange(out pflVolumeMindB: Single;
                            out pflVolumeMaxdB: Single;
                            out pflVolumeIncrementdB: Single): HRESULT; stdcall;
  end;
  IID_IAudioEndpointVolume = IAudioEndpointVolume;
  {$EXTERNALSYM IID_IAudioEndpointVolume}


  // Interface IAudioEndpointVolumeEx
  // ================================
  // >= Windows 7
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointVolumeEx);'}
  {$EXTERNALSYM IAudioEndpointVolumeEx}
  IAudioEndpointVolumeEx = interface(IAudioEndpointVolume)
  ['{66E11784-F695-4F28-A505-A7080081A78F}']

    function GetVolumeRangeChannel(iChannel: UINT;
                                   out pflVolumeMinDB: Single;
                                   out pflVolumeMaxDB: Single;
                                   out pflVolumeIncrementDB: Single): HRESULT; stdcall;
  end;
  IID_IAudioEndpointVolumeEx = IAudioEndpointVolumeEx;
  {$EXTERNALSYM IID_IAudioEndpointVolumeEx}


  // Interface IAudioMeterInformation
  // ================================
  //  Volume control interface for a device endpoint.
  //  Gets the volume range for a particular channel
  //
  // Parameters:
  //      iChannel - [in] Channel number
  //      pflVolumeMinDB - [out] Minimum dB for the channel.
  //      pflVolumeMaxDB - [out] Maximum dB for the channel.
  //      pflVolumeIncrementDB - [out] dB step for the channel.
  //
  // Remarks:
  //
  // Return values:
  //      S_OK if successful
  //      <other error> If failed.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioMeterInformation);'}
  {$EXTERNALSYM IAudioMeterInformation}
  IAudioMeterInformation = interface(IUnknown)
  ['{C02216F6-8C67-4B5B-9D00-D008E73E0064}']

    function GetPeakValue(out pfPeak: Single): HResult; stdcall;
    // The GetPeakValue method gets the peak sample value for the channels in the audio stream.
    // Parameters
    // pfPeak [out]
    // Pointer to a float variable into which the method writes the peak sample value
    // for the audio stream.
    // The peak value is a number in the normalized range from 0.0 to 1.0.

    function GetMeteringChannelCount(out pnChannelCount: UINT): HResult; stdcall;
    // The GetMeteringChannelCount method gets the number of channels in the
    // audio stream that are monitored by peak meters.
    // Parameters
    // pnChannelCount [out]
    //  Pointer to a UINT variable into which the method writes the number of channels.
    
    function GetChannelsPeakValues(u32ChannelCount: UINT32;
                                   afPeakValues: PFloat): HResult; stdcall;
    // The GetChannelsPeakValues method gets the peak sample values for all the channels in the audio stream.
    // Parameters
    // u32ChannelCount [in]
    // The channel count. This parameter also specifies the number of elements in
    // the afPeakValues array. If the specified count does not match the number of
    // channels in the stream, the method returns error code E_INVALIDARG.

    // afPeakValues [out]
    // Array of peak sample values.
    // The method writes the peak values for the channels into the array.
    // The array contains one element for each channel in the stream.
    // The peak values are numbers in the normalized range from 0.0 to 1.0.
    // NOTE: Parameter afPeakValues points to a caller-allocated float array.

    function QueryHardwareSupport(pdwHardwareSupportMask: PDWORD): HResult; stdcall;
    // Use this on special needs ie you want to create and use a custom software peakmeter.
    // By default: If hardware does not support a peakmeter,
    // the system will create and use a software peakmeter automaticly.
    // Parameters
    // pdwHardwareSupportMask [out]
    //    Pointer to a DWORD variable into which the method writes a hardware support mask
    //    that indicates the hardware capabilities of the audio endpoint device.
    //    The method can set the mask to 0 or to the bitwise-OR combination of one or
    //    more ENDPOINT_HARDWARE_SUPPORT_XXX constants.
    //
    // Return code          Description
    //  E_POINTER           Parameter pdwHardwareSupportMask is Nil.
    //
    // Remarks
    //  This method indicates whether the audio endpoint device implements
    //  the following functions in hardware:
    //    Volume control
    //    Mute control
    //    Peak meter

  end;
  IID_IAudioMeterInformation = IAudioMeterInformation;
  {$EXTERNALSYM IID_IAudioMeterInformation}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

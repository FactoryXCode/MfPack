// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudioApi
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioEndpoints.pas
// Kind: Pascal / Delphi unit
// Release date: 28-09-2021
// Language: ENU
//
// Revision Version: 3.1.1
// Description: Audio Endpoint related interfaces.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: -
//
//==============================================================================
// Source: AudioEndpoints.h
// Author: Soccerl
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

unit WinApi.CoreAudioApi.AudioEndpoints;

  {$HPPEMIT '#include "AudioEndpoints.h"'}

interface

uses
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}


const
  ENDPOINT_FORMAT_RESET_MIX_ONLY      = $00000001 ;
  {$EXTERNALSYM ENDPOINT_FORMAT_RESET_MIX_ONLY}

  // ----------------------------------------------------------------------
  // IAudioEndpointFormatControl
  //
  // Description:
  //
  //  Interface for resetting the current audio endpoint device format.
  //  This setting is exposed to the user through the "Sounds" control panel
  //  and can be read from the endpoint propertystore using
  //  PKEY_AudioEngine_DeviceFormat
  //

type

  // Interface IAudioEndpointFormatControl
  // =====================================
  // Description:
  //
  //  Resets the format to the default setting provided by the device manufacturer.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointFormatControl);'}
  {$EXTERNALSYM IAudioEndpointFormatControl}
  IAudioEndpointFormatControl = interface(IUnknown)
  ['{784CFD40-9F89-456E-A1A6-873B006A664E}']
    function ResetToDefault({in} ResetFlags: DWORD): HResult; stdcall;
    // Parameters:
    //
    //  ResetFlags - [in] Allows the application to specify which formats are reset.  If
    //                  no flags are set, then this method reevaluates both the endpoint's
    //                  device format and mix format and sets them to their default values.
    //
    //      ENDPOINT_FORMAT_RESET_MIX_ONLY - Only reset the mix format.  The endpoint's device
    //          format will not be reset if this flag is set.
    //
    // Return values:
    //      S_OK if successful
    //      <other error>
  end;
  IID_IAudioEndpointFormatControl = IAudioEndpointFormatControl;
  {$EXTERNALSYM IID_IAudioEndpointFormatControl}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

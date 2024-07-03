// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudioApi
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioClientActivationParams.pas
// Kind: Pascal / Delphi unit
// Release date: 20-08-2021
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This header is used by Core Audio APIs.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 build 20348 or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: audioclientactivationparams.h
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================

unit WinApi.CoreAudioApi.AudioClientActivationParams;

interface

uses
  WinApi.Windows;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


  {NTDDI_VERSION >= NTDDI_WIN10_FE}

const
  // Identifier for virtual audio device that supports audio loopback based on
  // a process ID instead of the device interface path of a physical audio device.
  // Use this for the deviceInterfacePath parameter of ActivateAudioInterfaceAsync when
  // AUDIOCLIENT_ACTIVATION_PARAMS.ActivationType is set to AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK.
  VIRTUAL_AUDIO_DEVICE_PROCESS_LOOPBACK = 'VAD\Process_Loopback';
  {$EXTERNALSYM VIRTUAL_AUDIO_DEVICE_PROCESS_LOOPBACK}

type

  // Specifies the loopback mode for an AUDIOCLIENT_ACTIVATION_PARAMS structure passed into a call to ActivateAudioInterfaceAsync.
  PPROCESS_LOOPBACK_MODE = ^PROCESS_LOOPBACK_MODE;
  PROCESS_LOOPBACK_MODE = (
    PROCESS_LOOPBACK_MODE_INCLUDE_TARGET_PROCESS_TREE,
    PROCESS_LOOPBACK_MODE_EXCLUDE_TARGET_PROCESS_TREE
   );
  {$EXTERNALSYM PROCESS_LOOPBACK_MODE}

  // This structure is used when creating an IAudioClient using ActivateAudioInterfaceAsync
  // for process-based loopback capture. The captured audio either includes or excludes audio rendered
  // by the specified process and its child processes, based on how the ProcessLoopbackMode field is set.
  PAUDIOCLIENT_PROCESS_LOOPBACK_PARAMS = ^AUDIOCLIENT_PROCESS_LOOPBACK_PARAMS;
  AUDIOCLIENT_PROCESS_LOOPBACK_PARAMS = record
    TargetProcessId: DWord;
    ProcessLoopbackMode: PROCESS_LOOPBACK_MODE;
  end;
  {$EXTERNALSYM AUDIOCLIENT_PROCESS_LOOPBACK_PARAMS}

  // Specifies the activation type for an AUDIOCLIENT_ACTIVATION_PARAMS structure passed into a call to ActivateAudioInterfaceAsync.
  PAUDIOCLIENT_ACTIVATION_TYPE = ^AUDIOCLIENT_ACTIVATION_TYPE;
  AUDIOCLIENT_ACTIVATION_TYPE = (
    AUDIOCLIENT_ACTIVATION_TYPE_DEFAULT,
    AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK  //AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK
  );
  {$EXTERNALSYM AUDIOCLIENT_ACTIVATION_TYPE}


  // Activation parameter structure that can be used with ActivateAudioInterfaceAsync
  // to create an IAudioClient.
  PAUDIOCLIENT_ACTIVATION_PARAMS = AUDIOCLIENT_ACTIVATION_TYPE;
  AUDIOCLIENT_ACTIVATION_PARAMS = record
    ActivationType: AUDIOCLIENT_ACTIVATION_TYPE;
    case Integer of   // Used when ActivationType is AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK.
      0: (ProcessLoopbackParams: AUDIOCLIENT_PROCESS_LOOPBACK_PARAMS);
    end;
  {$EXTERNALSYM PAUDIOCLIENT_ACTIVATION_PARAMS}


  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes

implementation

  //Implement Additional functions here.

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.StiErr
// Kind: Pascal / Delphi unit
// Release date: 24/11/2023
// Language: ENU
//
// Revision Version: 3.1.6
// Description: This module contains the user mode still image APIs error and status codes.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: stierr.h
//
// Author:
// Copyright (c) 1986-1997  Microsoft Corporation
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
unit WinApi.StiErr;

interface

uses
  WinApi.Windows,
  WinApi.WinError;



//
// Error codes are constructed as compound COM status codes
//

const

//
// The operation completed successfully
//
  STI_OK                              = S_OK;
  {$EXTERNALSYM STI_OK}
  STI_ERROR_NO_ERROR                  = STI_OK;
  {$EXTERNALSYM STI_ERROR_NO_ERROR}

//
// The device exists but not currently attached to the system
//
  STI_NOTCONNECTED                    = S_FALSE;
  {$EXTERNALSYM STI_NOTCONNECTED}

//
// The requested change in device mode settings had no effect
//
  STI_CHANGENOEFFECT                  = S_FALSE;
  {$EXTERNALSYM STI_CHANGENOEFFECT}


//
// The application requires newer version
//
  STIERR_OLD_VERSION                  = HRESULT($8007047E);
  {$EXTERNALSYM STIERR_OLD_VERSION}

//
// The application was written for pre-release version of provider DLL
//
  STIERR_BETA_VERSION                 = HRESULT($80070481);
  {$EXTERNALSYM STIERR_BETA_VERSION}


//
// The requested object could not be created due to incompatible or mismatched driver
//
  STIERR_BADDRIVER                    = HRESULT($80070077);
  {$EXTERNALSYM STIERR_BADDRIVER}

//
// The device is not registered
//
  STIERR_DEVICENOTREG                 = REGDB_E_CLASSNOTREG;
  {$EXTERNALSYM STIERR_DEVICENOTREG}

//
// The requested container does not exist
//
  STIERR_OBJECTNOTFOUND               = HRESULT($80070002);
  {$EXTERNALSYM STIERR_OBJECTNOTFOUND}

//
// An invalid or not state matching parameter was passed to the API
//
  STIERR_INVALID_PARAM                = E_INVALIDARG;
  {$EXTERNALSYM STIERR_INVALID_PARAM}

//
// The specified interface is not supported
//
  STIERR_NOINTERFACE                  = E_NOINTERFACE;
  {$EXTERNALSYM STIERR_NOINTERFACE}

//
// The undetermined error occured
//
  STIERR_GENERIC                      = E_FAIL;
  {$EXTERNALSYM STIERR_GENERIC}

//
// There is not enough memory to perform requested operation
//
  STIERR_OUTOFMEMORY                  = E_OUTOFMEMORY;
  {$EXTERNALSYM STIERR_OUTOFMEMORY}

//
// The application called unsupported (at this time) function
//
  STIERR_UNSUPPORTED                  = E_NOTIMPL;
  {$EXTERNALSYM STIERR_UNSUPPORTED}

//
// The application requires newer version
//
  STIERR_NOT_INITIALIZED              = HRESULT($80070015);
  {$EXTERNALSYM STIERR_NOT_INITIALIZED}

//
// The application requires newer version
//
  STIERR_ALREADY_INITIALIZED          = HRESULT($800704DF);
  {$EXTERNALSYM STIERR_ALREADY_INITIALIZED}

//
// The operation can not performed while device is locked
//
  STIERR_DEVICE_LOCKED                = HRESULT($80070021);
  {$EXTERNALSYM STIERR_DEVICE_LOCKED}

//
// The specified propery can not be changed for this device
//
  STIERR_READONLY                     = E_ACCESSDENIED;
  {$EXTERNALSYM STIERR_READONLY}

//
// The device already has notification handle associated with it
//
  STIERR_NOTINITIALIZED               = E_ACCESSDENIED;
  {$EXTERNALSYM STIERR_NOTINITIALIZED}


//
// The device needs to be locked before attempting this operation
//
  STIERR_NEEDS_LOCK                   = HRESULT($8007009E);
  {$EXTERNALSYM STIERR_NEEDS_LOCK}

//
// The device is opened by another application in data mode
//
  STIERR_SHARING_VIOLATION            = HRESULT($80070020);
  {$EXTERNALSYM STIERR_SHARING_VIOLATION}

//
// Handle already set for this context
//
  STIERR_HANDLEEXISTS                 = HRESULT($800700B7);
  {$EXTERNALSYM STIERR_HANDLEEXISTS}

//
// Device name is not recognized
//
  STIERR_INVALID_DEVICE_NAME          = HRESULT($8007007B);
  {$EXTERNALSYM STIERR_INVALID_DEVICE_NAME}

//
// Device hardware type is not valid
//
  STIERR_INVALID_HW_TYPE              = HRESULT($8007000D);
  {$EXTERNALSYM STIERR_INVALID_HW_TYPE}

//
// No events available
//
  STIERR_NOEVENTS                     = HRESULT($80070103);
  {$EXTERNALSYM STIERR_NOEVENTS}

//
// Device appears as not ready
//
  STIERR_DEVICE_NOTREADY              = HRESULT($80070015);
  {$EXTERNALSYM STIERR_DEVICE_NOTREADY}


implementation


end.

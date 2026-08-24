// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.FunctionDiscoveryApi.FunctionDiscoveryError.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: functiondiscoveryerror.h
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
unit WinApi.FunctionDiscoveryApi.FunctionDiscoveryError;

 {$HPPEMIT '#include "functiondiscoveryerror.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

  {$MINENUMSIZE 4}

const

// Error codes
//
//

// MessageId: E_FDPAIRING_NOCONNECTION
//
// MessageText:
//
// The device has rejected the connection.
//
  E_FDPAIRING_NOCONNECTION = HRESULT($8FD00001);
  {$EXTERNALSYM E_FDPAIRING_NOCONNECTION}

// MessageId: E_FDPAIRING_NOCONNECTION
//
// MessageText:
//
// The device has indicated a hardware failure.
//
  E_FDPAIRING_HWFAILURE = HRESULT($8FD00002);
  {$EXTERNALSYM E_FDPAIRING_HWFAILURE}

// MessageId: E_FDPAIRING_AUTHFAILURE
//
// MessageText:
//
// The device authentication has failed.  Either the device has rejected the authentication or you rejected the authentication.
//
  E_FDPAIRING_AUTHFAILURE = HRESULT($8FD00003);
  {$EXTERNALSYM E_FDPAIRING_AUTHFAILURE}

// MessageId: E_FDPAIRING_CONNECTTIMEOUT
//
// MessageText:
//
// The time to finish the authentication has expired on the device.
//
  E_FDPAIRING_CONNECTTIMEOUT = HRESULT($8FD00004);
  {$EXTERNALSYM E_FDPAIRING_CONNECTTIMEOUT}

// MessageId: E_FDPAIRING_TOOMANYCONNECTIONS
//
// MessageText:
//
// The device has indicated that it cannot accept more incoming connections.
//
  E_FDPAIRING_TOOMANYCONNECTIONS = HRESULT($8FD00005);
  {$EXTERNALSYM E_FDPAIRING_TOOMANYCONNECTIONS}

// MessageId: E_FDPAIRING_AUTHNOTALLOWED
//
// MessageText:
//
// The device has indicated that the authentication is not allowed.
//
  E_FDPAIRING_AUTHNOTALLOWED = HRESULT($8FD00006);
  {$EXTERNALSYM E_FDPAIRING_AUTHNOTALLOWED}

// MessageId: E_FDPAIRING_AUTHNOTALLOWED
//
// MessageText:
//
// The Pnp-X Bus Enumerator service is disabled.
//
  E_FDPAIRING_IPBUSDISABLED = HRESULT($8FD00007);
  {$EXTERNALSYM E_FDPAIRING_IPBUSDISABLED}

// MessageId: E_FDPAIRING_NOPROFILES
//
// MessageText:
//
// Windows does not have any network profiles for this device to use.
//
  E_FDPAIRING_NOPROFILES = HRESULT($8FD00008);
  {$EXTERNALSYM E_FDPAIRING_NOPROFILES}


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.

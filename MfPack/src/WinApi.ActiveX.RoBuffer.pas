// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.RoBuffer.pas
// Kind: Pascal / Delphi unit
// Release date: 03-11-2022
// Language: ENU
//
// Revision Version: 3.1.3
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 04/11/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: robuffer.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
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
unit WinApi.ActiveX.RoBuffer;

  {$HPPEMIT '#include "robuffer.h"'}

interface

uses
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.ObjIdl;

  {$WEAKPACKAGEUNIT ON}


  // Provides a standard IBuffer marshaler to implement the semantics
  // associated with the IBuffer interface when it is marshaled.
  function RoGetBufferMarshaler(bufferMarshaler: IMarshal): HRESULT; stdcall;
  {$EXTERNALSYM RoGetBufferMarshaler}

type

  // Interface IBufferByteAccess
  // ============================
  // Represents a buffer as an array of bytes.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBufferByteAccess);'}
  {$EXTERNALSYM IBufferByteAccess}
  IBufferByteAccess = Interface(IUnknown)
    ['{905a0fef-bc53-11df-8c49-001e4fc686da}']

    // An IBuffer object is created by a client, and the buffer is provided by IBufferByteAccess.Buffer.
    function Buffer(out value: Pbyte): HRESULT; stdcall;

  end;
  IID_IBufferByteAccess = IBufferByteAccess;
  {$EXTERNALSYM IID_IBufferByteAccess}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  RoBufferLib = 'Wintypes.dll';


{$WARN SYMBOL_PLATFORM OFF}

  function RoGetBufferMarshaler; external RoBufferLib name 'RoGetBufferMarshaler' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.

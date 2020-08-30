// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXVA2Trace.pas
// Kind: Pascal / Delphi unit
// Release date: 11-24-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: DirectX Video Acceleration 2 header file for ETW data.
//              https://docs.microsoft.com/en-us/windows/win32/medfound/directx-video-acceleration-2-0
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
// Remarks: -
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
// Source: dxva2Trace.h
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
//==============================================================================
unit WinApi.DirectX.DXVA2Trace;

  {$HPPEMIT '#include "dxva2Trace.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.EvnTrace,
  {System}
  System.Types,
  {DirectX}
  WinApi.DirectX.DXVA9Typ;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  DXVA2Trace_Control                  : TGUID = '{a0386e75-f70c-464c-a9ce-33c44e091623}';
  {$EXTERNALSYM DXVA2Trace_Control}
  DXVA2Trace_DecodeDevCreated         : TGUID = '{b4de17a1-c5b2-44fe-86d5-d97a648114ff}';
  {$EXTERNALSYM DXVA2Trace_DecodeDevCreated}
  DXVA2Trace_DecodeDevDestroyed       : TGUID = '{853ebdf2-4160-421d-8893-63dcea4f18bb}';
  {$EXTERNALSYM DXVA2Trace_DecodeDevDestroyed}
  DXVA2Trace_DecodeDevBeginFrame      : TGUID = '{9fd1acf6-44cb-4637-bc62-2c11a9608f90}';
  {$EXTERNALSYM DXVA2Trace_DecodeDevBeginFrame}
  DXVA2Trace_DecodeDevExecute         : TGUID = '{850aeb4c-d19a-4609-b3b4-bcbf0e22121e}';
  {$EXTERNALSYM DXVA2Trace_DecodeDevExecute}
  DXVA2Trace_DecodeDevGetBuffer       : TGUID = '{57b128fb-72cb-4137-a575-d91fa3160897}';
  {$EXTERNALSYM DXVA2Trace_DecodeDevGetBuffer}
  DXVA2Trace_DecodeDevEndFrame        : TGUID = '{9fb3cb33-47dc-4899-98c8-c0c6cd7cd3cb}';
  {$EXTERNALSYM DXVA2Trace_DecodeDevEndFrame}
  DXVA2Trace_VideoProcessDevCreated   : TGUID = '{895508c6-540d-4c87-98f8-8dcbf2dabb2a}';
  {$EXTERNALSYM DXVA2Trace_VideoProcessDevCreated}
  DXVA2Trace_VideoProcessDevDestroyed : TGUID = '{f97f30b1-fb49-42c7-8ee8-88bdfa92d4e2}';
  {$EXTERNALSYM DXVA2Trace_VideoProcessDevDestroyed}
  DXVA2Trace_VideoProcessBlt          : TGUID = '{69089cc0-71ab-42d0-953a-2887bf05a8af}';
  {$EXTERNALSYM DXVA2Trace_VideoProcessBlt}

type

  // -------------------------------------------------------------------------
  // DXVA2 Video Decoder ETW definitions
  //
  // There are event for:
  //      Device creation
  //      Device destruction
  //
  // When the device is being used there are events for:
  //      Begin frame
  //      Begin execute
  //      End execute
  //      End frame
  // -------------------------------------------------------------------------

  PDXVA2Trace_DecodeDevCreatedData = ^DXVA2Trace_DecodeDevCreatedData;
  DXVA2Trace_DecodeDevCreatedData = record

{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}
    pObject: ULONGLONG;
    pD3DDevice: ULONGLONG;
    DeviceGuid: TGUID;
    Width: ULONG;
    Height: ULONG;
    Enter: LongBool;
  end;
  {$EXTERNALSYM DXVA2Trace_DecodeDevCreatedData}

  PDXVA2Trace_DecodeDeviceData = ^DXVA2Trace_DecodeDeviceData;
  DXVA2Trace_DecodeDeviceData = record
{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}
    pObject: ULONGLONG;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVA2Trace_DecodeDeviceData}

  DXVA2Trace_DecodeDevDestroyedData = DXVA2Trace_DecodeDeviceData;
  {$EXTERNALSYM DXVA2Trace_DecodeDevDestroyedData}
  DXVA2Trace_DecodeDevExecuteData = DXVA2Trace_DecodeDeviceData;
  {$EXTERNALSYM DXVA2Trace_DecodeDevExecuteData}
  DXVA2Trace_DecodeDevEndFrameData = DXVA2Trace_DecodeDeviceData;
  {$EXTERNALSYM DXVA2Trace_DecodeDevEndFrameData}

  PDXVA2Trace_DecodeDevBeginFrameData = ^DXVA2Trace_DecodeDevBeginFrameData;
  DXVA2Trace_DecodeDevBeginFrameData = record

{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}
    pObject: ULONGLONG;
    pRenderTarget: ULONGLONG;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVA2Trace_DecodeDevBeginFrameData}


  PDXVA2Trace_DecodeDevGetBufferData = ^DXVA2Trace_DecodeDevGetBufferData;
  DXVA2Trace_DecodeDevGetBufferData = record

{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}
    pObject: ULONGLONG;
    BufferType: UINT;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVA2Trace_DecodeDevGetBufferData}


  // -------------------------------------------------------------------------
  // DXVA2 Video Processing ETW definitions
  //
  // There are event for:
  //      Device creation
  //      Device destruction
  //
  // When the device is being used there are events for:
  //      Begin VideoProcessBlt
  //      End VideoProcessBlt
  // -------------------------------------------------------------------------

  PDXVA2Trace_VideoProcessDevCreatedData = ^DXVA2Trace_VideoProcessDevCreatedData;
  DXVA2Trace_VideoProcessDevCreatedData = record
{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}

    pObject: ULONGLONG;
    pD3DDevice: ULONGLONG;
    DeviceGuid: TGUID;
    RTFourCC: ULONG;
    Width: ULONG;
    Height: ULONG;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVA2Trace_VideoProcessDevCreatedData}

  PDXVA2Trace_VideoProcessDeviceData = ^DXVA2Trace_VideoProcessDeviceData;
  DXVA2Trace_VideoProcessDeviceData = record
{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}
    pObject: ULONGLONG;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVA2Trace_VideoProcessDeviceData}

  DXVA2Trace_VideoProcessDevDestroyedData = DXVA2Trace_VideoProcessDeviceData;
  {$EXTERNALSYM DXVA2Trace_VideoProcessDevDestroyedData}
  DXVA2Trace_VideoProcessBltEndData = DXVA2Trace_VideoProcessDeviceData;
  {$EXTERNALSYM DXVA2Trace_VideoProcessBltEndData}

  PDXVA2TraceVideoProcessBltData = ^DXVA2TraceVideoProcessBltData;
  DXVA2TraceVideoProcessBltData = record
{$IFNDEF DXVA2Trace_PostProcessing}
    wmiHeader: EVENT_TRACE_HEADER;
{$ENDIF}
    pObject: ULONGLONG;
    pRenderTarget: ULONGLONG;
    TargetFrameTime: ULONGLONG;
    TargetRect: TRECT;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVA2TraceVideoProcessBltData}


  DXVA2TraceVideoProcessBltDataData = DXVA2TraceVideoProcessBltData;
  {$EXTERNALSYM DXVA2TraceVideoProcessBltDataData}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

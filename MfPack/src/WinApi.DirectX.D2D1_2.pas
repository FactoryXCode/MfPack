// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1_2.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
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
// Source: d2d1_2.h
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
unit WinApi.DirectX.D2D1_2;

  {$HPPEMIT '#include "d2d1_2.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  {WinApi.DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1Effects_1,
  WinApi.DirectX.DXGI;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}


type
  // Specifies the extent to which D2D will throttle work sent to the GPU.
  PD2D1_RENDERING_PRIORITY = ^D2D1_RENDERING_PRIORITY;
  D2D1_RENDERING_PRIORITY = DWord;
  {$EXTERNALSYM D2D1_RENDERING_PRIORITY}
const
  D2D1_RENDERING_PRIORITY_NORMAL      = D2D1_RENDERING_PRIORITY(0);
  {$EXTERNALSYM D2D1_RENDERING_PRIORITY_NORMAL}
  D2D1_RENDERING_PRIORITY_LOW         = D2D1_RENDERING_PRIORITY(1);
  {$EXTERNALSYM D2D1_RENDERING_PRIORITY_LOW}
  //D2D1_RENDERING_PRIORITY_FORCE_DWORD = FORCEDWORD;


type

  // INTERFACES ////////////////////////////////////////////////////////////////


  // Interface ID2D1GeometryRealization
  // ==================================
  // Encapsulates a device- and transform-dependent representation of a filled or
  // stroked geometry.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GeometryRealization);'}
  {$EXTERNALSYM ID2D1GeometryRealization}
  ID2D1GeometryRealization = interface(ID2D1Resource)
  ['{a16907d7-bc02-4801-99e8-8cf7f485f774}']

  end;
  IID_ID2D1GeometryRealization = ID2D1GeometryRealization;
  {$EXTERNALSYM IID_ID2D1GeometryRealization}



  // Interface ID2D1DeviceContext1
  // =============================
  // Enables creation and drawing of geometry realization objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext1);'}
  {$EXTERNALSYM ID2D1DeviceContext1}
  ID2D1DeviceContext1 = interface(ID2D1DeviceContext)
  ['{d37f57e4-6908-459f-a199-e72f24f79987}']

    function CreateFilledGeometryRealization(geometry: ID2D1Geometry;
                                             flatteningTolerance: Single;
                                             out geometryRealization: ID2D1GeometryRealization): HResult; stdcall;

    function CreateStrokedGeometryRealization(geometry: ID2D1Geometry;
                                              flatteningTolerance: Single;
                                              strokeWidth: Single;
                                              {in_opt} strokeStyle: ID2D1StrokeStyle;
                                              out geometryRealization: ID2D1GeometryRealization): HResult; stdcall;

    procedure DrawGeometryRealization(geometryRealization: ID2D1GeometryRealization;
                                      brush: ID2D1Brush) stdcall;

  end;
  IID_ID2D1DeviceContext1 = ID2D1DeviceContext1;
  {$EXTERNALSYM IID_ID2D1DeviceContext1}



  // Interface ID2D1Device1
  // ======================
  // Represents a resource domain whose objects and device contexts can be used
  // together.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device1);'}
  {$EXTERNALSYM ID2D1Device1}
  ID2D1Device1 = interface(ID2D1Device)
  ['{d21768e1-23a4-4823-a14b-7c3eba85d658}']


    // Retrieves the rendering priority currently set on the device.
    function GetRenderingPriority(): D2D1_RENDERING_PRIORITY; stdcall;


    // Sets the rendering priority of the device.
    procedure SetRenderingPriority(renderingPriority: D2D1_RENDERING_PRIORITY); stdcall;


    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext1: ID2D1DeviceContext1): HResult; stdcall;

  end;
  IID_ID2D1Device1 = ID2D1Device1;
  {$EXTERNALSYM IID_ID2D1Device1}


  // Interface ID2D1Factory2
  // =======================
  // Creates Direct2D resources. This interface also enables the creation of
  // ID2D1Device1 objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory2);'}
  {$EXTERNALSYM ID2D1Factory2}
  ID2D1Factory2 = interface(ID2D1Factory1)
  ['{94f81a73-9212-4376-9c58-b16a3a0d3992}']

    // This creates a new Direct2D device from the given IDXGIDevice.
    function CreateDevice(dxgiDevice: IDXGIDevice;
                          out d2dDevice1: ID2D1Device1): HResult; stdcall;

  end;
  IID_ID2D1Factory2 = ID2D1Factory2;
  {$EXTERNALSYM IID_ID2D1Factory2}


  // Interface ID2D1CommandSink1
  // ===========================
  // This interface performs all the same functions as the existing ID2D1CommandSink
  // interface. It also enables access to the new primitive blend modes, MIN and ADD,
  // through its SetPrimitiveBlend1 method.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink1);'}
  {$EXTERNALSYM ID2D1CommandSink1}
  ID2D1CommandSink1 = interface(ID2D1CommandSink)
  ['{9eb767fd-4269-4467-b8c2-eb30cb305743}']

    // This method is called if primitiveBlend value was added after Windows 8.
    // SetPrimitiveBlend method is used for Win8 values (_SOURCE_OVER and _COPY).
    function SetPrimitiveBlend1(primitiveBlend: D2D1_PRIMITIVE_BLEND): HResult; stdcall;

  end;
  IID_ID2D1CommandSink1 = ID2D1CommandSink1;
  {$EXTERNALSYM IID_ID2D1CommandSink1}


//#if NTDDI_VERSION >= NTDDI_WINBLUE

  // Computes the maximum factor by which a given transform can stretch any vector.
  // See: https://docs.microsoft.com/en-us/windows/desktop/api/d2d1_2/nf-d2d1_2-d2d1computemaximumscalefactor
  function D2D1ComputeMaximumScaleFactor(matrix: D2D1_MATRIX_3X2_F): Single; stdcall;
  {$EXTERNALSYM D2D1ComputeMaximumScaleFactor}

//#endif // #if NTDDI_VERSION >= NTDDI_WINBLUE


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation


const
  D2D1_2Lib = 'D2d1.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function D2D1ComputeMaximumScaleFactor; external D2D1_2Lib name 'D2D1ComputeMaximumScaleFactor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.

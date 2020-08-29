// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMediaCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 09-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (TopPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8.1 or later.
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
// Source: mfmediacapture.h
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
unit WinApi.MediaFoundationApi.MfMediaCapture;

  {$HPPEMIT '#include "mfmediacapture.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


type

  // IAdvancedMediaCaptureInitializationSettings Interface
  // =====================================================
  // This interface is used to set advanced MediaCapture Initialization settings on
  // MediaCapture object.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAdvancedMediaCaptureInitializationSettings);'}
  {$EXTERNALSYM IAdvancedMediaCaptureInitializationSettings}
  IAdvancedMediaCaptureInitializationSettings = interface(IUnknown)
	['{3DE21209-8BA6-4f2a-A577-2819B56FF14D}']

    function SetDirectxDeviceManager(value: IMFDXGIDeviceManager): HResult; stdcall;
    // Sets DX manager to be used for the video capture session.
    // <param name = "IMFDXGIDeviceManager">
    // DX device manager interface pointer.

  end;
  IID_IAdvancedMediaCaptureInitializationSettings = IAdvancedMediaCaptureInitializationSettings;
  {$EXTERNALSYM IID_IAdvancedMediaCaptureInitializationSettings}


  // IAdvancedMediaCaptureSettings Interface
  // =======================================
  // This interface is used to set/get advanced MediaCapture object settings
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAdvancedMediaCaptureSettings);'}
  {$EXTERNALSYM IAdvancedMediaCaptureSettings}
  IAdvancedMediaCaptureSettings = interface(IUnknown)
	['{24E0485F-A33E-4aa1-B564-6019B1D14F65}']

    function GetDirectxDeviceManager(out value: IMFDXGIDeviceManager): HResult; stdcall;
    // Gets DirectX manager that is being used in MediaCapture object
     // <param name = "IMFDXGIDeviceManager">
     // Pointer to receive DX manager interface pointer

  end;
  IID_IAdvancedMediaCaptureSettings = IAdvancedMediaCaptureSettings;
  {$EXTERNALSYM IID_IAdvancedMediaCaptureSettings}


  // Interface IAdvancedMediaCapture
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAdvancedMediaCapture);'}
  {$EXTERNALSYM IAdvancedMediaCapture}
  IAdvancedMediaCapture = interface(IUnknown)
	['{D0751585-D216-4344-B5BF-463B68F977BB}']

    function GetAdvancedMediaCaptureSettings(out value: IAdvancedMediaCaptureSettings): HResult; stdcall;

  end;
  IID_IAdvancedMediaCapture = IAdvancedMediaCapture;
  {$EXTERNALSYM IID_IAdvancedMediaCapture}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

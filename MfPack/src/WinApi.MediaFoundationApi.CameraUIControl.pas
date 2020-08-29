// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation MediaFoundationApi
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.CameraUIControl.pas
// Kind: Pascal / Delphi unit
// Release date: 31-07-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: .
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (topPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
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
// Source: CameraUIControl.h
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
unit WinApi.MediaFoundationApi.CameraUIControl;

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.OaIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  PICameraUIControlEventCallback = ^ICameraUIControlEventCallback;

  PCameraUIControlMode = ^CameraUIControlMode;
  CameraUIControlMode = (
    Browse = 0,
    Linear = (Browse + 1)
  );
  {$EXTERNALSYM CameraUIControlMode}

  PCameraUIControlLinearSelectionMode = ^CameraUIControlLinearSelectionMode;
  CameraUIControlLinearSelectionMode = (
    Single   = 0,
    Multiple = (Single + 1)
  );
  {$EXTERNALSYM CameraUIControlLinearSelectionMode}

  PCameraUIControlCaptureMode = ^CameraUIControlCaptureMode;
  CameraUIControlCaptureMode = (
    PhotoOrVideo = 0,
    Photo        = (PhotoOrVideo + 1),
    Video        = (Photo + 1)
  );
  {$EXTERNALSYM CameraUIControlCaptureMode}


  PCameraUIControlPhotoFormat = ^CameraUIControlPhotoFormat;
  CameraUIControlPhotoFormat = (
    Jpeg   = 0,
    Png    = (Jpeg + 1),
    JpegXR = (Png + 1)
  );
  {$EXTERNALSYM CameraUIControlPhotoFormat}

  PCameraUIControlVideoFormat = ^CameraUIControlVideoFormat;
  CameraUIControlVideoFormat = (
    Mp4 = 0,
    Wmv = (Mp4 + 1)
  );
  {$EXTERNALSYM CameraUIControlVideoFormat}

  PCameraUIControlViewType = ^CameraUIControlViewType;
  CameraUIControlViewType = (
    SingleItem = 0,
    ItemList   = (SingleItem + 1)
  );
  {$EXTERNALSYM CameraUIControlViewType}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICameraUIControlEventCallback);'}
  {$EXTERNALSYM ICameraUIControlEventCallback}
  ICameraUIControlEventCallback = interface(IUnknown)
  ['{1BFA0C2C-FBCD-4776-BDA4-88BF974E74F4}']

    procedure OnStartupComplete(); stdcall;

    procedure OnSuspendComplete(); stdcall;

    procedure OnItemCaptured(var pszPath: LPCWSTR); stdcall;

    procedure OnItemDeleted(var pszPath: LPCWSTR); stdcall;

    procedure OnClosed(); stdcall;

  end;


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICameraUIControl);'}
  {$EXTERNALSYM ICameraUIControl}
  ICameraUIControl = interface(IUnknown)
  ['{B8733ADF-3D68-4b8f-BB08-E28A0BED0376}']

    function Show(const pWindow: IUnknown;
                  mode: CameraUIControlMode;
                  selectionMode: CameraUIControlLinearSelectionMode;
                  captureMode: CameraUIControlCaptureMode;
                  photoFormat: CameraUIControlPhotoFormat;
                  videoFormat: CameraUIControlVideoFormat;
                  bHasCloseButton: BOOL;
                  {optional} pEventCallback: PICameraUIControlEventCallback = Nil): HResult; stdcall;

    function Close(): HResult; stdcall;

    function Suspend(out pbDeferralRequired: BOOL): HResult; stdcall;

    function Resume(): HResult; stdcall;

    function GetCurrentViewType(out pViewType: CameraUIControlViewType): HResult; stdcall;

    function GetActiveItem(out pbstrActiveItemPath: BSTR): HResult; stdcall;

    function GetSelectedItems({out} ppSelectedItemPaths: PSAFEARRAY): HResult; stdcall;

    function RemoveCapturedItem(pszPath: LPCWSTR): HResult; stdcall;

  end;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

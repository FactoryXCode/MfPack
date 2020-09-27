// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MCIApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Constants and function prototypes needed to create IHV's extension DLL
//              and constants used to programatically open a target capture device.
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
// Remarks:
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
// Source: VfWExt.h
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
unit WinApi.WinMM.VfwExt;

interface

uses
  WinApi.Windows,
  Winapi.CommCtrl;

const

  VFW_HIDE_SETTINGS_PAGE              = $00000001;
  {$EXTERNALSYM VFW_HIDE_SETTINGS_PAGE}
  VFW_HIDE_VIDEOSRC_PAGE              = $00000002;
  {$EXTERNALSYM VFW_HIDE_VIDEOSRC_PAGE}
  VFW_HIDE_CAMERACONTROL_PAGE         = $00000004;
  {$EXTERNALSYM VFW_HIDE_CAMERACONTROL_PAGE}
  VFW_HIDE_ALL_PAGES                  = (VFW_HIDE_SETTINGS_PAGE or
                                         VFW_HIDE_VIDEOSRC_PAGE or
                                         VFW_HIDE_CAMERACONTROL_PAGE);
  {$EXTERNALSYM VFW_HIDE_ALL_PAGES}

  VFW_OEM_ADD_PAGE                    = $80000000;  // If OEM has added any page
  {$EXTERNALSYM VFW_OEM_ADD_PAGE}


  VFW_USE_DEVICE_HANDLE               = $00000001;
  {$EXTERNALSYM VFW_USE_DEVICE_HANDLE}
  VFW_USE_STREAM_HANDLE               = $00000002;
  {$EXTERNALSYM VFW_USE_STREAM_HANDLE}
  VFW_QUERY_DEV_CHANGED               = $00000100;  // if selected_dev == streaming_dev
  {$EXTERNALSYM VFW_QUERY_DEV_CHANGED}


  //
  // This is the function pointer that vfwwdm mapper calls to add an page
  //

type

  VFWWDMExtensionProc = function(pfnDeviceIoControl: Pointer;
	                               pfnAddPropertyPage: LPFNADDPROPSHEETPAGE;
	                               lParam: LPARAM): DWord; stdcall;
  {$EXTERNALSYM VFWWDMExtensionProc}

  //
  // This is the function pointer that you can call to make DeviceIoControl() calls.
  //

  LPFNEXTDEVIO = function(lParam: LPARAM;
					                dwFlags: DWORD;
					                dwIoControlCode: DWORD;
					                lpInBuffer: Pointer;
				                	nInBufferSize: DWORD;
					                lpOutBuffer: Pointer;
					                nOutBufferSize: DWORD;
					                lpBytesReturned: PDWORD;
					                lpOverlapped: POVERLAPPED): BOOL; stdcall;
  {$EXTERNALSYM LPFNEXTDEVIO}


  //
  // HLM\System\CurrentControlSet\Control\MediaResources\msvideo\MSVideo.VFWWDM
  //
  // Registry values used to allow a VfW client application to programatically
  // open a target capture device.  The first is the FriendlyName of the capture
  // device; and the 2nd flag if set, vfwwdm mapper will open only; if failed,
  // no attempt will be made by VfWWDM mapper to open other WDM capture device.
  //
  // Both registry value should be clear after capDriverConnect().  VfWWDM mapper
  // will not clear them unless video source dialog box is chosen.
  //

const

  {$EXTERNALSYM TARGET_DEVICE_FRIENDLY_NAME}
  TARGET_DEVICE_FRIENDLY_NAME         = 'TargetDeviceFriendlyName';  // REG_SZ
  {$EXTERNALSYM TARGET_DEVICE_OPEN_EXCLUSIVELY}
  TARGET_DEVICE_OPEN_EXCLUSIVELY      = 'TargetDeviceOpenExclusively';  // REG_DWORD

  // Additional Prototypes for ALL Interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

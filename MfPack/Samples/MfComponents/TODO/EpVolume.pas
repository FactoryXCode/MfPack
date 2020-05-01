// FactoryX
//
// Copyright ©2003 - 2018 by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: EpVolume.pas
// Kind: Pascal Unit
// Release date: 04-08-2016
// Language: ENU
//
// Version: 3.4.1
//
// Description: Implementation of IAudioEndpointVolumeCallback interface.
//
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
//
// ----------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 21
// Todo: -
// SDK version: 10.0.16299.15
// =============================================================================
// Source: Epvolume.h -- Implementation of IAudioEndpointVolumeCallback interface
//         example from MSDN.
//
// Copyright (c) 1997-2018 Microsoft Corporation. All rights reserved
// =============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
//                                                  Ramyses De Macedo Rodrigues
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit EpVolume;

interface

uses
  // WinApi
  WinApi.Windows, WinApi.ActiveX, WinApi.CommCtrl,
  WinApi.Messages,
  // System
  System.Classes, System.Win.ComObj,
  // MfPack
  MfPack.MfpUtils, MfPack.PropSys,
  MfPack.PropIdl, MfPack.MMDeviceApi, MfPack.Endpointvolume;


const
  // Maximum volume level on trackbar
  MAX_VOL = 100;

  // Defines the control identifiers
  IDC_SLIDER_VOLUME = 1001;
  IDC_CHECK_MUTE    = 1002;
  IDC_STATIC_MINVOL = 1003;
  IDC_STATIC_MAXVOL = 1004;

var
  // Dialog handle from dialog box procedure
  g_hDlg: HWND;
  // Client's proprietary event-context GUID
  g_guidMyContext: TGUID;

type

  //-----------------------------------------------------------
  // Client implementation of IAudioEndpointVolumeCallback
  // interface. When a method in the IAudioEndpointVolume
  // interface changes the volume level or muting state of the
  // endpoint device, the change initiates a call to the
  // client's IAudioEndpointVolumeCallback.OnNotify method.
  //-----------------------------------------------------------
  TAudioEndpointVolumeCallback = class(TInterfacedObject, IAudioEndpointVolumeCallback)

  protected

    // Callback method for endpoint-volume-change notifications.
    function OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;

  end;

implementation


function TAudioEndpointVolumeCallback.OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT;
begin

  if (pNotify = Nil) then
    begin
      Result:= E_INVALIDARG;
      Exit;
    end;

  if (g_hDlg <> 0) And (IsEqualGuid(pNotify.guidEventContext, g_guidMyContext) = True) then
    begin
      PostMessage(GetDlgItem(g_hDlg, IDC_CHECK_MUTE),
                  BM_SETCHECK,
                  UINT(pNotify.bMuted) {? BST_CHECKED : BST_UNCHECKED},
                  0);

      PostMessage(GetDlgItem(g_hDlg, IDC_SLIDER_VOLUME),
                  TBM_SETPOS,
                  1,
                  LPARAM( Trunc(MAX_VOL * pNotify.fMasterVolume + 0.5)) );
    end;

  Result:= S_OK;

end;

end.

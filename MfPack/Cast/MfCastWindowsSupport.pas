// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastWindowsSupport.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Optional Windows-message bridge and command workers for Cast
//              applications that marshal asynchronous results to a window.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 10/08/2026 All                 Extracted reusable Windows UI support.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: MfCast.pas, MfCastTypes.pas
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: -
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/MPL/2.0/
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
unit MfCastWindowsSupport;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  {Cast}
  MfCast,
  MfCastTypes;

const
  WM_MFCAST_DEVICES = WM_APP + 101;
  WM_MFCAST_STATE = WM_APP + 102;
  WM_MFCAST_STATUS = WM_APP + 103;
  WM_MFCAST_ERROR = WM_APP + 104;
  WM_MFCAST_FINISHED = WM_APP + 105;
  WM_MFCAST_LOG = WM_APP + 106;
  WM_MFCAST_SUBTITLE_FINISHED = WM_APP + 107;

type
  TMfCastFileWorker = class(TThread)
  private
    FCast: TMfCast;
    FDevice: TMfCastDevice;
    FSource: string;
    FSubtitle: TMfCastSubtitleAsset;
    FTargetWindow: HWND;
  protected
    procedure Execute(); override;
  public
    constructor Create(const ACast: TMfCast;
                       const ADevice: TMfCastDevice;
                       const ASource: string;
                       const ASubtitle: TMfCastSubtitleAsset;
                       const ATargetWindow: HWND);
  end;

  TMfCastSubtitleWorker = class(TThread)
  private
    FCast: TMfCast;
    FSubtitle: TMfCastSubtitleAsset;
    FEnabled: Boolean;
    FTargetWindow: HWND;
  protected
    procedure Execute(); override;
  public
    constructor Create(const ACast: TMfCast;
                       const ASubtitle: TMfCastSubtitleAsset;
                       const AEnabled: Boolean;
                       const ATargetWindow: HWND);
  end;

  TMfCastUiLogMessage = class
  public
    Text: string;
    constructor Create(const AText: string);
  end;

implementation


constructor TMfCastFileWorker.Create(const ACast: TMfCast;
                                     const ADevice: TMfCastDevice;
                                     const ASource: string;
                                     const ASubtitle: TMfCastSubtitleAsset;
                                     const ATargetWindow: HWND);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FCast := ACast;
  FDevice := ADevice;
  FSource := ASource;
  FSubtitle := ASubtitle;
  FTargetWindow := ATargetWindow;
end;


procedure TMfCastFileWorker.Execute();
var
  hr: HRESULT;

begin

  hr := FCast.Cast(FDevice,
                   FSource,
                   FSubtitle,
                   cmmAutomatic,
                   csmAutomatic,
                   0.0);

  PostMessage(FTargetWindow,
              WM_MFCAST_FINISHED,
              WPARAM(hr),
              0);
end;


constructor TMfCastSubtitleWorker.Create(const ACast: TMfCast;
                                         const ASubtitle: TMfCastSubtitleAsset;
                                         const AEnabled: Boolean;
                                         const ATargetWindow: HWND);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FCast := ACast;
  FSubtitle := ASubtitle;
  FEnabled := AEnabled;
  FTargetWindow := ATargetWindow;
end;


procedure TMfCastSubtitleWorker.Execute();
var
  Hr: HRESULT;

begin

  if FEnabled then
    Hr := FCast.SelectSubtitle(FSubtitle)
  else
    Hr := FCast.DisableSubtitles();

  PostMessage(FTargetWindow,
              WM_MFCAST_SUBTITLE_FINISHED,
              WPARAM(Hr),
              LPARAM(Ord(FEnabled)));
end;


constructor TMfCastUiLogMessage.Create(const AText: string);
begin

  inherited Create();

  Text := AText;
end;

end.

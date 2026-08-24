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
// Revision Version: 4.0.0
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
// Remarks: Requires Windows 10 or higher.
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
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
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

  TMfCastLiveFmp4Worker = class(TThread)
  private
    FCast: TMfCast;
    FDevice: TMfCastDevice;
    FInitSegment: TBytes;
    FByteStream: IMFByteStream;
    FTargetWindow: HWND;

  protected
    procedure Execute(); override;

  public

    constructor Create(const ACast: TMfCast;
                       const ADevice: TMfCastDevice;
                       const AInitSegment: TBytes;
                       const ATargetWindow: HWND);

    property ByteStream: IMFByteStream read FByteStream;
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
  HrCom: HRESULT;
  ComInitialized: Boolean;
  LogMessage: TMfCastUiLogMessage;

begin

  hr := E_FAIL;
  ComInitialized := False;

  try
    try
      HrCom := CoInitializeEx(nil,
                              COINIT_MULTITHREADED);

      if SUCCEEDED(HrCom) then
        ComInitialized := True
      else
        if (HrCom <> RPC_E_CHANGED_MODE) then
          hr := HrCom
        else
          HrCom := S_OK;

      if SUCCEEDED(HrCom) then
        hr := FCast.Cast(FDevice,
                         FSource,
                         FSubtitle,
                         cmmAutomatic,
                         csmAutomatic,
                         0.0);
    except
      on E: Exception do
        begin
          OutputDebugString(PChar('MfCast file worker exception: ' + E.Message));

          LogMessage := TMfCastUiLogMessage.Create('Cast worker exception: ' +
                                                   E.Message);
          if not PostMessage(FTargetWindow,
                             WM_MFCAST_LOG,
                             WPARAM(LogMessage),
                             0) then
            LogMessage.Free();

          hr := E_FAIL;
        end;
    end;
  finally
    if ComInitialized then
      CoUninitialize();

    // Always release the sample UI from its busy state, including when a
    // Media Foundation or Cast implementation raises inside the worker.
    PostMessage(FTargetWindow,
                WM_MFCAST_FINISHED,
                WPARAM(hr),
                0);
  end;
end;


constructor TMfCastLiveFmp4Worker.Create(const ACast: TMfCast;
                                         const ADevice: TMfCastDevice;
                                         const AInitSegment: TBytes;
                                         const ATargetWindow: HWND);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FCast := ACast;
  FDevice := ADevice;

  FInitSegment := Copy(AInitSegment,
                       0,
                       Length(AInitSegment));

  FByteStream := nil;
  FTargetWindow := ATargetWindow;
end;


procedure TMfCastLiveFmp4Worker.Execute();
var
  hr: HRESULT;

begin

  hr := FCast.CastLiveFragmentedMp4(FDevice,
                                    FInitSegment,
                                    FByteStream);

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
  hr: HRESULT;

begin

  if FEnabled then
    hr := FCast.SelectSubtitle(FSubtitle)
  else
    hr := FCast.DisableSubtitles();

  PostMessage(FTargetWindow,
              WM_MFCAST_SUBTITLE_FINISHED,
              WPARAM(hr),
              LPARAM(Ord(FEnabled)));
end;


constructor TMfCastUiLogMessage.Create(const AText: string);
begin

  inherited Create();

  Text := AText;
end;

end.

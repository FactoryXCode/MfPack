// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: ContentEnabler.pas
// Kind: Pascal Unit
// Release date: 19-09-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Implements IMFContentProtectionManager and manages silent and non-silent content enabling.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
// 19/09/2026 Tony                Delphi translation of ProtectedPlayback.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or higher.
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
// =============================================================================
// Source: Parts of Microsoft ProtectedPlayback and CPlayer examples
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

unit ContentEnabler;

interface

uses
  Winapi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ComBaseApi,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WebHelper;

const
  WM_APP_CONTENT_ENABLER = WM_APP + 3;
  WM_APP_BROWSER_DONE = WM_APP + 4;
  { nserror.h: silent DRM license acquisition did not obtain a license. }
  NS_E_DRM_LICENSE_NOTACQUIRED = HRESULT($C00D2759);

type
  TEnablerState = (
    Enabler_Ready,
    Enabler_SilentInProgress,
    Enabler_NonSilentInProgress,
    Enabler_Complete
  );

  TEnablerFlags = (
    SilentOrNonSilent,
    ForceNonSilent
  );

  IContentProtectionManagerApp = interface(IMFContentProtectionManager)
    ['{6A142825-DC61-4AC7-BD88-FBF029D65F63}']
    function DoEnable(Flags: TEnablerFlags = SilentOrNonSilent): HRESULT;
    function CancelEnable: HRESULT;
    function CompleteEnable: HRESULT;
    function GetState: TEnablerState;
    function GetStatus: HRESULT;
  end;

  TContentProtectionManager = class(TDispatchCallback, IMFAsyncCallback,
                                    IMFContentProtectionManager, IContentProtectionManagerApp)
  private
    FRefCount: Integer;
    FState: TEnablerState;
    FStatus: HRESULT;
    FNotifyWindow: HWND;
    FEnabler: IMFContentEnabler;
    FMediaEventGenerator: IMFMediaEventGenerator;
    FResult: IMFAsyncResult;
    FWebHelper: TWebHelper;

    function DoNonSilentEnable(): HRESULT;

  protected

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetParameters(out Flags, Queue: DWORD): HRESULT; stdcall;
    function Invoke(AsyncResult: IMFAsyncResult): HRESULT; stdcall;
    function BeginEnableContent(EnablerActivate: IMFActivate;
                                Topology: IMFTopology; Callback: IMFAsyncCallback;
                                State: IUnknown): HRESULT; stdcall;
    function EndEnableContent(AsyncResult: IMFAsyncResult): HRESULT; stdcall;

  public

    constructor Create(NotifyWindow: HWND);
    destructor Destroy; override;

    class function CreateInstance(NotifyWindow: HWND;
                                  out Manager: IContentProtectionManagerApp): HRESULT; static;
    function DoEnable(Flags: TEnablerFlags = SilentOrNonSilent): HRESULT;
    function CancelEnable(): HRESULT;
    function CompleteEnable(): HRESULT;
    function GetState(): TEnablerState;
    function GetStatus(): HRESULT;
    procedure OnDispatchInvoke(DispIdMember: DISPID); override;
  end;

implementation

uses

  {System}
  System.SysUtils,
  {Application}
  SampleLog;

const
  DISPID_ONQUIT = 253;

constructor TContentProtectionManager.Create(NotifyWindow: HWND);
begin

  inherited Create();
  FRefCount := 1;
  FNotifyWindow := NotifyWindow;
  FState := Enabler_Ready;
  FStatus := S_OK;
  FWebHelper := TWebHelper.Create();
end;


destructor TContentProtectionManager.Destroy();
begin

  FWebHelper.Free;
  FMediaEventGenerator := nil;
  FResult := nil;
  FEnabler := nil;

  inherited;
end;


class function TContentProtectionManager.CreateInstance(NotifyWindow: HWND;
                                                        out Manager: IContentProtectionManagerApp): HRESULT;
var
  Instance: TContentProtectionManager;

begin

  Manager := nil;
  if (NotifyWindow = 0) then
    Exit(E_INVALIDARG);

  Instance := TContentProtectionManager.Create(NotifyWindow);
  try
    Result := Instance.QueryInterface(IContentProtectionManagerApp,
                                      Manager);
  finally
    Instance._Release;
  end;
end;


function TContentProtectionManager.QueryInterface(const IID: TGUID;
                                                  out Obj): HResult;
begin

  if GetInterface(IID,
                  Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;


function TContentProtectionManager._AddRef(): Integer;
begin

  Result := InterlockedIncrement(FRefCount);
end;


function TContentProtectionManager._Release(): Integer;
begin

  Result := InterlockedDecrement(FRefCount);
  if (Result = 0) then
    Destroy();
end;


function TContentProtectionManager.GetParameters(out Flags,
                                                 Queue: DWORD): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TContentProtectionManager.BeginEnableContent(EnablerActivate: IMFActivate;
                                                      Topology: IMFTopology;
                                                      Callback: IMFAsyncCallback;
                                                      State: IUnknown): HRESULT;
begin

  if Assigned(FEnabler) then
    begin
      PostLogMessage(FNotifyWindow,
                     'BeginEnableContent rejected: a previous operation is pending.');
      Exit(E_FAIL);
    end;

  PostLogMessage(FNotifyWindow,
                 'BeginEnableContent called by the PMP session.');

  Result := MFCreateAsyncResult(nil,
                                Callback,
                                State,
                                FResult);

  PostLogMessage(FNotifyWindow,
                 'MFCreateAsyncResult: ' + FormatHR(Result));

  if Succeeded(Result) then
    begin
      Result := EnablerActivate.ActivateObject(IMFContentEnabler,
                                               FEnabler);
      PostLogMessage(FNotifyWindow,
                     'Activate IMFContentEnabler: ' + FormatHR(Result));
    end;

  if Succeeded(Result) then
    begin
      FState := Enabler_Ready;
      PostMessage(FNotifyWindow,
                  WM_APP_CONTENT_ENABLER,
                  0,
                  0);
    end;
end;

function TContentProtectionManager.EndEnableContent(AsyncResult: IMFAsyncResult): HRESULT;
begin

  if not Assigned(AsyncResult) then
    Exit(E_POINTER);

  PostLogMessage(FNotifyWindow,
                 'EndEnableContent: ' + FormatHR(FStatus));

  FResult := nil;
  FEnabler := nil;
  FMediaEventGenerator := nil;
  Result := FStatus;
end;


function TContentProtectionManager.Invoke(AsyncResult: IMFAsyncResult): HRESULT;
var
  MediaEvent: IMFMediaEvent;
  Callback: IMFAsyncCallback;
  EventType: MediaEventType;
  EventData: PROPVARIANT;

begin

  Result := S_OK;
  PropVariantInit(EventData);

  try
    if not Assigned(FMediaEventGenerator) then
      Exit;

    Result := FMediaEventGenerator.EndGetEvent(AsyncResult,
                                               MediaEvent);
    if Succeeded(Result) then
      Result := MediaEvent.GetType(EventType);

    if Succeeded(Result) then
      Result := MediaEvent.GetStatus(FStatus);

    if Succeeded(Result) then
      Result := MediaEvent.GetValue(EventData);

    if Succeeded(Result) then
      begin
        PostLogMessage(FNotifyWindow,
                       Format('Content-enabler event: %s, status %s',
                              [MediaEventName(EventType), FormatHR(FStatus)]));

        if (EventType = MEEnablerCompleted) then
          PostMessage(FNotifyWindow, WM_APP_CONTENT_ENABLER, 0, 0)
      else
      begin
        QueryInterface(IMFAsyncCallback,
                       Callback);
        FMediaEventGenerator.BeginGetEvent(Callback,
                                           nil);
      end;
    end;

  finally
    PropVariantClear(EventData);
  end;

  { As in the C++ sample, callback failures are reported through FStatus. }
  Result := S_OK;
end;


function TContentProtectionManager.DoEnable(Flags: TEnablerFlags): HRESULT;
var
  Automatic: BOOL;
  EnableType: TGUID;
  Callback: IMFAsyncCallback;

begin

  if not Assigned(FEnabler) then
    Exit(E_UNEXPECTED);

  PostLogMessage(FNotifyWindow,
                 'Starting content-enable operation.');
  Result := FEnabler.GetEnableType(EnableType);

  if Succeeded(Result) then
    PostLogMessage(FNotifyWindow,
                   'Enable type: ' + GUIDToString(EnableType))
  else
    PostLogMessage(FNotifyWindow, 'GetEnableType failed: ' + FormatHR(Result));

  if Succeeded(Result) then
    Result := FEnabler.QueryInterface(IMFMediaEventGenerator,
                                      FMediaEventGenerator);
  if Succeeded(Result) then
    begin
      QueryInterface(IMFAsyncCallback,
                     Callback);
      Result := FMediaEventGenerator.BeginGetEvent(Callback,
                                                   nil);
    end;

  Automatic := False;
  if Succeeded(Result) then
    begin
      if (Flags <> ForceNonSilent) then
        Result := FEnabler.IsAutomaticSupported(Automatic);
    end;

  if Succeeded(Result) then
    PostLogMessage(FNotifyWindow,
                   'Automatic enable supported: ' + BoolToStr(Automatic, True));

  if Succeeded(Result) then
    begin
      if Automatic then
        begin
          FState := Enabler_SilentInProgress;
          PostLogMessage(FNotifyWindow,
                         'Starting silent license acquisition.');
          Result := FEnabler.AutomaticEnable();
        end
      else
        begin
          FState := Enabler_NonSilentInProgress;
          PostLogMessage(FNotifyWindow,
                         'Starting non-silent license acquisition.');
          Result := DoNonSilentEnable();
        end;
    end;

  if Failed(Result) then
    FStatus := Result;
end;


function TContentProtectionManager.CancelEnable(): HRESULT;
var
  EmptyValue: PROPVARIANT;

begin

  Result := S_OK;
  PostLogMessage(FNotifyWindow,
                 'Cancelling content-enable operation.');

  if (FState <> Enabler_Complete) and Assigned(FEnabler) then
    begin
      Result := FEnabler.Cancel;
      if Failed(Result) and Assigned(FMediaEventGenerator) then
        begin
          PropVariantInit(EmptyValue);
          FMediaEventGenerator.QueueEvent(MEEnablerCompleted,
                                          GUID_NULL,
                                          Result,
                                          EmptyValue);
    end;
  end;
end;


function TContentProtectionManager.CompleteEnable(): HRESULT;
begin

  FState := Enabler_Complete;
  PostLogMessage(FNotifyWindow,
                 'Completing content-enable operation: ' + FormatHR(FStatus));

  if Assigned(FResult) then
    begin
      FResult.SetStatus(FStatus);
      MFInvokeCallback(FResult);
    end;
  Result := S_OK;
end;


function TContentProtectionManager.DoNonSilentEnable(): HRESULT;
var
  TrustStatus: MF_URL_TRUST_STATUS;
  URL: LPWSTR;
  URLLength: DWORD;
  PostData: PByte;
  PostDataSize: DWORD;

begin

  URL := nil;
  URLLength := 0;
  PostData := nil;
  PostDataSize := 0;
  TrustStatus := MF_LICENSE_URL_UNTRUSTED;

  Result := FEnabler.GetEnableURL(URL,
                                  URLLength,
                                  TrustStatus);

  try
    if Succeeded(Result) then
      PostLogMessage(FNotifyWindow,
                     Format('Enable URL received: %d character(s), trust status=%d.',
                            [URLLength, Ord(TrustStatus)]))
    else
      PostLogMessage(FNotifyWindow,
                     'GetEnableURL failed: ' + FormatHR(Result));

    if Succeeded(Result) and (TrustStatus <> MF_LICENSE_URL_TRUSTED) then
      Result := E_FAIL;

    if Succeeded(Result) then
      Result := FEnabler.MonitorEnable();

    if Succeeded(Result) then
      Result := FEnabler.GetEnableData(PostData,
                                       PostDataSize);

    if Succeeded(Result) then
      PostLogMessage(FNotifyWindow,
                     Format('Enable POST data: %d byte(s).',
                            [PostDataSize]));
    if Succeeded(Result) then
      begin
        Result := FWebHelper.Init(Self);
        PostLogMessage(FNotifyWindow,
                       'Initialize IE automation: ' + FormatHR(Result));
      end;

    if Succeeded(Result) then
      begin
        Result := FWebHelper.OpenURLWithData(URL,
                                             PostData,
                                             PostDataSize);
        PostLogMessage(FNotifyWindow, 'Navigate license URL: ' + FormatHR(Result));
    end;

  finally
    CoTaskMemFree(PostData);
    CoTaskMemFree(URL);
  end;
end;


function TContentProtectionManager.GetState(): TEnablerState;
begin

  Result := FState;
end;


function TContentProtectionManager.GetStatus(): HRESULT;
begin

  Result := FStatus;
end;


procedure TContentProtectionManager.OnDispatchInvoke(DispIdMember: DISPID);
begin

  if (DispIdMember = DISPID_ONQUIT) then
    begin
      PostLogMessage(FNotifyWindow,
                     'Internet Explorer automation window closed.');

      PostMessage(FNotifyWindow,
                  WM_APP_BROWSER_DONE,
                  0,
                  0);

      FWebHelper.ExitBrowser();
    end;
end;

end.

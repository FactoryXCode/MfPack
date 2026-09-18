// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WebHelper.pas
// Kind: Pascal Unit
// Release date: 19-09-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Hosts Internet Explorer automation for legacy non-silent DRM license acquisition.
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

unit WebHelper;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {System}
  System.SysUtils,
  System.Variants,
  {}
  SHDocVw;

type
  DISPID = Integer;

  TDispatchCallback = class
  public
    procedure OnDispatchInvoke(DispIdMember: DISPID); virtual; abstract;
  end;

  { Delphi equivalent of the C++ WebHelper. It hosts the out-of-process
    Internet Explorer automation object and listens for DWebBrowserEvents2. }
  TWebHelper = class(TObject, IDispatch)
  private
    FBrowser: IWebBrowser2;
    FConnectionPoint: IConnectionPoint;
    FCookie: Longint;
    FDispatchCallback: TDispatchCallback;

  protected
    function QueryInterface(const IID: TGUID;
                            out Obj): HResult; stdcall;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;

    function GetTypeInfo(Index,
                         LocaleID: Integer; out TypeInfo): HResult; stdcall;

    function GetIDsOfNames(const IID: TGUID;
                           Names: Pointer;
                           NameCount: Integer;
                           LocaleID: Integer;
                           DispIDs: Pointer): HResult; stdcall;

    function Invoke(_DispID: Integer;
                    const IID: TGUID;
                    LocaleID: Integer;
                    Flags: Word;
                    var Params;
                    VarResult: Pointer;
                    ExcepInfo: Pointer;
                    ArgErr: Pointer): HResult; stdcall;
  public

    destructor Destroy(); override;
    function Init(Callback: TDispatchCallback): HRESULT;

    procedure ExitBrowser();
    function OpenURLWithData(const URL: PWideChar;
                             PostData: PByte;
                             DataSize: DWORD): HRESULT;
  end;

implementation

uses
  {System}
  System.Win.ComObj;

const
  POST_HEADER_DATA = 'Content-Type: application/x-www-form-urlencoded'#13#10;


destructor TWebHelper.Destroy();
begin

  ExitBrowser();

  inherited;
end;


function TWebHelper.QueryInterface(const IID: TGUID;
                                   out Obj): HResult;
begin

  if GetInterface(IID,
                  Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;


function TWebHelper._AddRef: Integer;
begin

  { The content-protection manager owns this helper, exactly as in C++. }
  Result := 1;
end;


function TWebHelper._Release: Integer;
begin

  Result := 2;
end;


function TWebHelper.Init(Callback: TDispatchCallback): HRESULT;
var
  CPContainer: IConnectionPointContainer;
  EventSink: IDispatch;
  BrowserWindow: HWND;

begin

  FDispatchCallback := Callback;

  try
    FBrowser := CreateComObject(CLASS_InternetExplorer) as IWebBrowser2;
  except
    on E: EOleSysError do
      Exit(E.ErrorCode);
  end;

  Result := FBrowser.QueryInterface(IConnectionPointContainer,
                                    CPContainer);
  if Failed(Result) then
    Exit;

  Result := CPContainer.FindConnectionPoint(DIID_DWebBrowserEvents2,
                                            FConnectionPoint);
  if Failed(Result) then
    Exit;

  Result := QueryInterface(IDispatch,
                           EventSink);
  if Succeeded(Result) then
    Result := FConnectionPoint.Advise(EventSink,
                                      FCookie);
  if Failed(Result) then
    Exit;

  try
    BrowserWindow := FBrowser.HWND;

    SetWindowPos(BrowserWindow,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NOMOVE or SWP_NOSIZE);
  except
    on E: EOleSysError do
      Exit(E.ErrorCode);
  end;
  Result := S_OK;
end;


procedure TWebHelper.ExitBrowser();
begin

  if Assigned(FConnectionPoint) and (FCookie <> 0) then
    begin
      FConnectionPoint.Unadvise(FCookie);
      FCookie := 0;
    end;

  FBrowser := nil;
  FConnectionPoint := nil;
  FDispatchCallback := nil;
end;


function TWebHelper.OpenURLWithData(const URL: PWideChar;
                                    PostData: PByte;
                                    DataSize: DWORD): HRESULT;
var
  Empty: OleVariant;
  Headers: OleVariant;
  Data: OleVariant;
  DataPointer: Pointer;

begin

  if (URL = nil) then
    Exit(E_INVALIDARG);

  if not Assigned(FBrowser) then
    Exit(E_UNEXPECTED);

  Empty := Unassigned;
  Headers := POST_HEADER_DATA;
  Data := Unassigned;

  try
    if (PostData <> nil) and (DataSize > 0) then
      begin
        Data := VarArrayCreate([0,
                                Integer(DataSize) - 1],
                                varByte);
        DataPointer := VarArrayLock(Data);

        try
          Move(PostData^,
               DataPointer^,
               DataSize);

        finally
          VarArrayUnlock(Data);
        end;
      end;

    FBrowser.Visible := True;
    FBrowser.Navigate(WideString(URL),
                      Empty,
                      Empty,
                      Data,
                      Headers);
    Result := S_OK;
  except
    on E: EOleSysError do
      Result := E.ErrorCode;
  end;
end;


function TWebHelper.GetTypeInfoCount(out Count: Integer): HResult;
begin

  Count := 0;
  Result := S_OK;
end;


function TWebHelper.GetTypeInfo(Index: Integer;
                                LocaleID: Integer;
                                out TypeInfo): HResult;
begin

  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TWebHelper.GetIDsOfNames(const IID: TGUID;
                                  Names: Pointer;
                                  NameCount: Integer;
                                  LocaleID: Integer;
                                  DispIDs: Pointer): HResult;
begin

  Result := E_NOTIMPL;
end;


function TWebHelper.Invoke(_DispID: Integer;
                           const IID: TGUID;
                           LocaleID: Integer;
                           Flags: Word;
                           var Params;
                           VarResult: Pointer;
                           ExcepInfo: Pointer;
                           ArgErr: Pointer): HResult;
begin

  if Assigned(FDispatchCallback) then
    FDispatchCallback.OnDispatchInvoke(_DispID);
  Result := S_OK;
end;


end.

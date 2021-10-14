// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: ApplicationLoopBackAudio sample
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Common.pas
// Kind: Pascal / Delphi unit
// Release date: 00-00-2021
// Language: ENU
//
// Revision Version: 3.0.2
// Description: Callback class.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
//
//------------------------------------------------------------------------------
//
// Remarks: Note that this sample requires Windows 10 build 20348 or later.
//
// Related objects: -
// Related projects: MfPackX302
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.20348.0
//
// Todo: -
//
//==============================================================================
// Source: ApplicationLoopBackAudio sample: LoopbackCapture.h, LoopbackCapture.cpp
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


unit Common;

interface

uses
  {WinApi}
  WinApi.Windows,
  //ShellObj or shlobj ?
  {system}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects;



type
  METHODASYNCCALLBACK = procedure(Parent; AsyncCallback; pfnCallback);


type

  Callback_AsyncCallback = class(TInterfacedPersistent, IMFAsyncCallback)
  protected
    _parent: Parent;
    _dwQueueID: DWORD;

  private


    // Implementation of IMFAsyncCallback
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;

    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;
    //

  public

    constructor Create(hParent: HWND);
    destructor Destroy(); override;

    procedure SetQueueID(dwQueueID: DWORD);

  end;

  var
    m_xAsyncCallback: Callback_AsyncCallback;


implementation


constructor Callback_AsyncCallback.Create(hParent: HWND);
begin
  inherited Create();
  _parent :=  hParent;

end;


destructor Callback_AsyncCallback.Destroy();
begin

  inherited Destroy();
end;


function Callback_AsyncCallback.GetParameters(out pdwFlags: DWord;
                                              out pdwQueue: DWord): HResult;
begin
  pdwFlags := 0;
  pdwQueue := _dwQueueID;
  Result   := S_OK;
end;


function Callback_AsyncCallback.Invoke(pAsyncResult: IMFAsyncResult): HResult;
begin
  _parent.pfnCallback(pResult);
  Result := S_OK;
end;


procedure Callback_AsyncCallback.SetQueueID(dwQueueID: DWORD);
begin
  _dwQueueID := dwQueueID;
end;


end.

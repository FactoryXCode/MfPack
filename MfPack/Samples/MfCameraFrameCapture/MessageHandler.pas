// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  MessageHandler.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description:
//   This unit handles the messages between the mainform and sync modules.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX317/Samples/CameraFrameCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit MessageHandler;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes;

type
  TOnHandleMessage = reference to procedure(var AMessage: TMessage;
                                            var AHandled: boolean);

  TMessageHandler = class(TObject)
  private
    FWinHandle: HWND;
    FOnHandleMessage: TOnHandleMessage;

  protected
    procedure HandleWindowsMessage(var AMessage: TMessage); virtual;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure AllocateHandle();
    procedure RemoveHandle();

    property Handle: HWND read FWinHandle;
    property OnMessage: TOnHandleMessage read FOnHandleMessage write FOnHandleMessage;
  end;

implementation

{ TMessageHandler }

constructor TMessageHandler.Create();
begin
  inherited;
  FWinHandle := INVALID_HANDLE_VALUE;
  AllocateHandle;
end;


destructor TMessageHandler.Destroy();
begin
  RemoveHandle;
  inherited;
end;


procedure TMessageHandler.AllocateHandle();
begin
  RemoveHandle;
  FWinHandle := AllocateHWnd(HandleWindowsMessage);
end;


procedure TMessageHandler.RemoveHandle();
begin
  DeallocateHWnd(FWinHandle);
  FWinHandle := INVALID_HANDLE_VALUE;
end;


procedure TMessageHandler.HandleWindowsMessage(var AMessage: TMessage);
var
  bHandled: boolean;

begin
  bHandled := False;

  if Assigned(FOnHandleMessage) then
    FOnHandleMessage(AMessage,
                     bHandled);

  if bHandled then
    AMessage.Result := 0
  else
    AMessage.Result := DefWindowProc(FWinHandle,
                                     AMessage.Msg,
                                     AMessage.WParam,
                                     AMessage.LParam);
end;

end.

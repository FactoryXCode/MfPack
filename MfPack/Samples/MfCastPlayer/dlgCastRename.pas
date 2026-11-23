// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgCastRename.pas
// Kind: Pascal Unit
// Release date: 22-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Worker-based receiver rename dialog.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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

unit dlgCastRename;
{$IFDEF FPC}{$I MfPack.Fpc.inc}{$ENDIF}
interface
uses
 {$IFDEF FPC}Windows,SysUtils,Classes,Forms,Controls,StdCtrls,ExtCtrls,
 {$ELSE}Winapi.Windows,System.SysUtils,System.Classes,Vcl.Forms,Vcl.Controls,Vcl.StdCtrls,Vcl.ExtCtrls,{$ENDIF}
 MfCast,MfCastTypes,MfCastEureka;
type
 TCastNameWorker = class(TThread)
 private
  FCast:TMfCast;
  FOptions:TMfCastEurekaOptions;
  FName:string;
  FRename:Boolean;
  FDone:Longint;
 protected
  procedure Execute;override;
 public
  Device:TMfCastDevice;
  Detail:string;
  ResultCode:HRESULT;
  constructor Create(ACast:TMfCast;const ADevice:TMfCastDevice;
   const AOptions:TMfCastEurekaOptions;const AName:string;ARename:Boolean);
  function Done:Boolean;
 end;
 TCastRenameDialog = class(TForm)
  lblDevice:TLabel;
  lblNewName:TLabel;
  lblPort:TLabel;
  lblToken:TLabel;
  lblStatus:TLabel;
  edtName:TEdit;
  edtToken:TEdit;
  edtPort:TEdit;
  chkHttps:TCheckBox;
  chkTrustCertificate:TCheckBox;
  btnRename:TButton;
  btnReadName:TButton;
  btnClose:TButton;
  grpConnection:TGroupBox;
  tmrPoll:TTimer;
  procedure RenameClick(Sender:TObject);
  procedure ReadClick(Sender:TObject);
  procedure CloseClick(Sender:TObject);
  procedure InputChanged(Sender:TObject);
  procedure SchemeChanged(Sender:TObject);
  procedure Poll(Sender:TObject);
  procedure QueryClose(Sender:TObject;var CanClose:Boolean);
 private
  FCast:TMfCast;
  FDevice:TMfCastDevice;
  FVerified:Boolean;
  FWorker:TCastNameWorker;
  procedure BeginRequest(ARename:Boolean);
  procedure UpdateActions;
 public
  constructor CreateDialog(AOwner:TComponent;ACast:TMfCast;
   const ADevice:TMfCastDevice;const AOptions:TMfCastEurekaOptions);
  destructor Destroy;override;
  class function ExecuteDialog(AOwner:TComponent;ACast:TMfCast;
   var ADevice:TMfCastDevice):Boolean;static;
 end;
var CastRenameDialog:TCastRenameDialog;

implementation
{$IFDEF FPC}{$R *.lfm}{$ELSE}{$R *.dfm}{$ENDIF}
constructor TCastNameWorker.Create(ACast:TMfCast;const ADevice:TMfCastDevice;
 const AOptions:TMfCastEurekaOptions;const AName:string;ARename:Boolean);
begin
 inherited Create(True);FreeOnTerminate:=False;
 FCast:=ACast;Device:=ADevice;FOptions:=AOptions;FName:=AName;FRename:=ARename;
 ResultCode:=E_PENDING;
end;
procedure TCastNameWorker.Execute;
var Name:string;
begin
 try
  try
   if FCast=nil then begin ResultCode:=E_POINTER;Detail:='Cast API is unavailable.';end
   else if FRename then ResultCode:=FCast.RenameDevice(Device,FName,FOptions,Detail)
   else begin
    ResultCode:=FCast.GetDeviceName(Device,FOptions,Name,Detail);
    if Succeeded(ResultCode) then Device.FriendlyName:=Name;
   end;
  except on E:Exception do begin ResultCode:=E_FAIL;Detail:='The device request failed. Read the current name before retrying.';end;end;
 finally InterlockedExchange(FDone,1);end;
end;
function TCastNameWorker.Done:Boolean;
begin Result:=InterlockedCompareExchange(FDone,0,0)<>0;end;
constructor TCastRenameDialog.CreateDialog(AOwner:TComponent;ACast:TMfCast;
 const ADevice:TMfCastDevice;const AOptions:TMfCastEurekaOptions);
begin
 inherited Create(AOwner);
 FCast:=ACast;FDevice:=ADevice;
 lblDevice.Caption:=ADevice.FriendlyName+'  ('+ADevice.Address+')';
 edtName.Text:=ADevice.FriendlyName;
 chkHttps.Checked:=AOptions.UseHttps;
 edtPort.Text:=IntToStr(AOptions.Port);
 chkTrustCertificate.Checked:=AOptions.AllowSelfSignedCertificate;
 edtToken.Text:=AOptions.LocalAuthorizationToken;
 edtToken.Enabled:=chkHttps.Checked;
 chkTrustCertificate.Enabled:=chkHttps.Checked;
 tmrPoll.Enabled:=True;
 UpdateActions;
end;
destructor TCastRenameDialog.Destroy;
begin
 if tmrPoll<>nil then tmrPoll.Enabled:=False;
 if FWorker<>nil then begin FWorker.WaitFor;FreeAndNil(FWorker);end;
 if edtToken<>nil then edtToken.Clear;
 inherited;
end;
procedure TCastRenameDialog.UpdateActions;
var Idle:Boolean;Port:Integer;
begin
 if btnRename=nil then Exit;
 Idle:=FWorker=nil;
 btnRename.Enabled:=Idle and (FCast<>nil) and (Trim(edtName.Text)<>'') and
  (Trim(edtName.Text)<>FDevice.FriendlyName) and TryStrToInt(edtPort.Text,Port) and (Port>0) and (Port<=65535);
 btnReadName.Enabled:=Idle and (FCast<>nil);
 btnClose.Enabled:=Idle;edtName.Enabled:=Idle;grpConnection.Enabled:=Idle;
end;
procedure TCastRenameDialog.InputChanged(Sender:TObject);
begin UpdateActions;end;
procedure TCastRenameDialog.SchemeChanged(Sender:TObject);
begin
 if chkHttps.Checked then edtPort.Text:='8443' else edtPort.Text:='8008';
 edtToken.Enabled:=chkHttps.Checked;chkTrustCertificate.Enabled:=chkHttps.Checked;
 UpdateActions;
end;
procedure TCastRenameDialog.BeginRequest(ARename:Boolean);
var Options:TMfCastEurekaOptions;Port:Integer;
begin
 if (FWorker<>nil) or (FCast=nil) then Exit;
 if not TryStrToInt(edtPort.Text,Port) or (Port<1) or (Port>65535) then
 begin lblStatus.Caption:='Enter a port from 1 to 65535.';Exit;end;
 Options:=TMfCastEurekaOptions.CreateDefault;Options.UseHttps:=chkHttps.Checked;Options.Port:=Port;
 Options.AllowSelfSignedCertificate:=chkTrustCertificate.Checked;
 Options.AllowDeviceCertificateUsage:=chkTrustCertificate.Checked;
 if Options.UseHttps then Options.LocalAuthorizationToken:=edtToken.Text;
 FWorker:=TCastNameWorker.Create(FCast,FDevice,Options,edtName.Text,ARename);
 if ARename then lblStatus.Caption:='Changing and verifying the receiver name...'
 else lblStatus.Caption:='Reading the receiver name...';
 UpdateActions;FWorker.Start;
end;
procedure TCastRenameDialog.RenameClick(Sender:TObject);
begin BeginRequest(True);end;
procedure TCastRenameDialog.ReadClick(Sender:TObject);
begin BeginRequest(False);end;
procedure TCastRenameDialog.Poll(Sender:TObject);
begin
 if (FWorker=nil) or not FWorker.Done then Exit;
 FWorker.WaitFor;
 if Succeeded(FWorker.ResultCode) then begin
  FDevice:=FWorker.Device;FVerified:=True;edtName.Text:=FDevice.FriendlyName;
 end;
 lblStatus.Caption:=FWorker.Detail;
 FreeAndNil(FWorker);UpdateActions;
end;
procedure TCastRenameDialog.QueryClose(Sender:TObject;var CanClose:Boolean);
begin CanClose:=FWorker=nil;if CanClose and FVerified then ModalResult:=mrOk;end;
procedure TCastRenameDialog.CloseClick(Sender:TObject);
begin if FWorker<>nil then Exit;if FVerified then ModalResult:=mrOk else ModalResult:=mrCancel;end;
class function TCastRenameDialog.ExecuteDialog(AOwner:TComponent;ACast:TMfCast;
 var ADevice:TMfCastDevice):Boolean;
var D:TCastRenameDialog;O:TMfCastEurekaOptions;
begin
 O:=TMfCastEurekaOptions.CreateDefault;O.AllowSelfSignedCertificate:=True;O.AllowDeviceCertificateUsage:=True;
 D:=TCastRenameDialog.CreateDialog(AOwner,ACast,ADevice,O);
 try
  Result:=D.ShowModal=mrOk;
  if Result then ADevice:=D.FDevice;
 finally D.Free;end;
end;
end.

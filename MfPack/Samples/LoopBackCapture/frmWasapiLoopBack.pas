
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmWasapiLoopBack.pas
// Kind: Pascal Unit
// Release date: 12-03-2023
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description:
//   Mainform of the app.
//
// Organisation: FactoryX
// Initiator(s): maXcomX
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 12/03/2023 Tony                PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX314/Samples/CaptureLoopBack
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Rita Han / FactoryX
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
unit frmWasapiLoopBack;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Winapi.ShellAPI,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  {WinMM}
  WinApi.WinMM.MMiscApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  WasapiLoopback;

type
  TfrmLoopBackCapture = class(TForm)
    butStart: TButton;
    butStop: TButton;
    edFileName: TEdit;
    Label1: TLabel;
    lblFileExt: TLabel;
    sbMsg: TStatusBar;
    butPlayData: TButton;
    procedure FormCreate(Sender: TObject);
    procedure butStartClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayDataClick(Sender: TObject);

  private
    { Private declarations }
    oAudioSink: TAudioSink;
    aFileName: LPWSTR;
    iProgress: Int64;

    function StartCapture(): HResult;

    procedure OnAudioSinkCaptureStopped(var aMessage: TMessage); message WM_CAPTURINGSTOPPED;
    procedure OnAudioSinkProgressEvent(var AMessage: TMessage); message WM_PROGRESSNOTIFY;

  public
    { Public declarations }
  end;

var
  frmLoopBackCapture: TfrmLoopBackCapture;


implementation

{$R *.dfm}

uses
  WinApi.MediaFoundationApi.MfUtils;

procedure TfrmLoopBackCapture.OnAudioSinkCaptureStopped(var aMessage: TMessage);
begin
  if (aMessage.WParam = S_OK) then
    begin
      sbMsg.SimpleText := Format('Capturing stopped. Captured %d bytes.', [iProgress]);
      butPlayData.Enabled := True;
    end
  else if (aMessage.WParam <> S_OK) then
    begin
      sbMsg.SimpleText := Format('Capturing stopped because of an error (hr = %d). Captured %d bytes.', [iProgress, aMessage.WParam]);
      butPlayData.Enabled := False;
    end;

  butStop.Enabled := False;
  butStart.Enabled := True;
end;


procedure TfrmLoopBackCapture.OnAudioSinkProgressEvent(var aMessage: TMessage);
begin
  inc(iProgress, aMessage.WParam);
  sbMsg.SimpleText := Format('Bytes processed: %d',[iProgress]);
end;


function TfrmLoopBackCapture.StartCapture(): HResult;
var
  hr: HResult;

label
  done;

begin
  hr := S_OK;
  iProgress := 0;
  // Create the AudioSink object.
  oAudioSink := TAudioSink.Create(Handle);

  if not Assigned(oAudioSink) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  if SUCCEEDED(hr) then
    begin
      aFileName := StrToPWideChar(edFileName.Text + lblFileExt.Caption);
      butStop.Enabled := True;
      butStart.Enabled := False;
      butPlayData.Enabled := False;
      hr := oAudioSink.RecordAudioStream(oAudioSink,
                                         aFileName);
      if FAILED(hr) then
        begin
          butStop.Enabled := False;
          butStart.Enabled := True;
          goto done;
        end;
    end;
done:
  Result := hr;
end;


procedure TfrmLoopBackCapture.butPlayDataClick(Sender: TObject);
begin
  ShellExecute(Handle,
               'open',
               StrToPWideChar(edFileName.Text + lblFileExt.Caption),
               nil,
               nil,
               SW_SHOWNORMAL) ;
end;

procedure TfrmLoopBackCapture.butStartClick(Sender: TObject);
begin
  StartCapture();
end;


procedure TfrmLoopBackCapture.butStopClick(Sender: TObject);
begin
  SendMessage(oAudioSink.hwHWND, WM_STOPREQUEST, 1, 0);
end;


procedure TfrmLoopBackCapture.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(oAudioSink);
  MFShutdown();
  CoUninitialize();
  CanClose := True;
end;


procedure TfrmLoopBackCapture.FormCreate(Sender: TObject);
var
  hr: HResult;

begin
  hr := CoInitializeEx(nil,
                       COINIT_MULTITHREADED);

  if SUCCEEDED(hr) then
    // Check if the current MF version match user's
    if FAILED(MFStartup(MF_VERSION, 0)) then
      begin
        MessageBox(0,
                   LPCWSTR('Your computer does not support this Media Foundation API version' +
                           IntToStr(MF_VERSION) + '.'),
                   LPCWSTR('MFStartup Failure!'),
                   MB_ICONSTOP);
        Abort();
      end;
end;

end.

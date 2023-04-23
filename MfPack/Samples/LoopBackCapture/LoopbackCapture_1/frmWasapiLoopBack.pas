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
// Revision Version: 3.1.5
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
  Winapi.ShellAPI,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  {Application}
  WasapiLoopback,
  Utils,
  dlgDevices;


type
  TfrmLoopBackCapture = class(TForm)
    butStart: TButton;
    butStop: TButton;
    edFileName: TEdit;
    Label1: TLabel;
    lblFileExt: TLabel;
    sbMsg: TStatusBar;
    butPlayData: TButton;
    cbxDontOverWrite: TCheckBox;
    Panel1: TPanel;
    rbRenderingDevice: TRadioButton;
    rbCaptureDevice: TRadioButton;
    Panel2: TPanel;
    rbConsole: TRadioButton;
    rbMultimedia: TRadioButton;
    rbCommunications: TRadioButton;
    cbxStayOnTop: TCheckBox;
    Bevel1: TBevel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure butStartClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayDataClick(Sender: TObject);
    procedure cbxStayOnTopClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private
    { Private declarations }
    oAudioSink: TAudioSink;
    iProgress: Int64;
    sFileName: string;
    oDataFlow: EDataFlow;
    oRole: ERole;

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
      sbMsg.SimpleText := Format('Capturing stopped. Captured %s bytes.', [iProgress.ToString()]);
      butPlayData.Enabled := True;
    end
  else if (aMessage.WParam <> S_OK) then
    begin
      sbMsg.SimpleText := Format('Capturing stopped because of an error (hr = %d). Captured %s bytes.', [aMessage.WParam, iProgress.ToString()]);
      butPlayData.Enabled := False;
    end;

  butStop.Enabled := False;
  butStart.Enabled := True;
  Panel1.Enabled := True;
  Panel2.Enabled := True;
end;


procedure TfrmLoopBackCapture.OnAudioSinkProgressEvent(var aMessage: TMessage);
begin
  inc(iProgress, aMessage.WParam);
  sbMsg.SimpleText := Format('Capturing from source: Bytes processed: %s',[iProgress.ToString]);
end;


function TfrmLoopBackCapture.StartCapture(): HResult;
var
  hr: HResult;
  i: Integer;
  bFileExists: Boolean;

label
  done;

begin
  hr := S_OK;
  iProgress := 0;

  if not Assigned(oAudioSink) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  if SUCCEEDED(hr) then
    begin

      sFileName := Format('%s%s', [edFileName.Text, lblFileExt.Caption]);

      if cbxDontOverWrite.Checked then
        begin
          bFileExists := True;
          i := 0;
          while (bFileExists = True) do
            begin
              if FileExists(sFileName) then
                begin
                  sFileName := Format('%s(%d)%s', [edFileName.Text, i, lblFileExt.Caption]);
                  Inc(i);
                end
              else
               bFileExists := False;
            end;
        end;

      butStop.Enabled := True;
      butStart.Enabled := False;
      butPlayData.Enabled := False;
      Panel1.Enabled := False;
      Panel2.Enabled := False;

      // user settings from dlg
      if oDataFlow = eDataFlow(-1) then
        begin
          oDataFlow := eRender;
          oRole := eMultimedia;
        end;

      // Capture the audio stream from the default rendering device.
      hr := oAudioSink.RecordAudioStream(oDataFlow,
                                         oRole,
                                         LPWSTR(sFileName));
      if FAILED(hr) then
        begin
          butStop.Enabled := False;
          butStart.Enabled := True;
          Panel1.Enabled := True;
          Panel2.Enabled := true;
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
               LPWSTR(sFileName),
               nil,
               nil,
               SW_SHOWNORMAL) ;
end;


procedure TfrmLoopBackCapture.butStartClick(Sender: TObject);
begin
  Self.BorderIcons := [biMinimize];
  StartCapture();
end;


procedure TfrmLoopBackCapture.butStopClick(Sender: TObject);
begin
  oAudioSink.StopRecording := True;
  Panel1.Enabled := True;
  Panel2.Enabled := True;
  Self.BorderIcons := [biSystemMenu, biMinimize];
end;


procedure TfrmLoopBackCapture.Button2Click(Sender: TObject);
begin
  // Create the dialog if it's not allready done.
  if not Assigned(DevicesDlg) then
    begin
      Application.CreateForm(TDevicesDlg,
                             DevicesDlg);
      DevicesDlg.Visible := False;
    end;

  // Ask the user to select one.
  if (DevicesDlg.ShowModal = mrOk) then
    begin
      oDataFlow := DevicesDlg.oDataFlow;
      rbRenderingDevice.Checked := (oDataFlow = eRender);
      rbCaptureDevice.Checked := (oDataFlow = eCapture);
      sbMsg.SimpleText := Format('Please select a%s.',[Panel2.Caption]);
    end
  else
    begin
      // User canceled.
      // Set radiobuttons to default.
      rbRenderingDevice.Checked := True;
      rbMultimedia.Checked := True;
      sbMsg.SimpleText := 'Start Capture';
    end;
end;


procedure TfrmLoopBackCapture.cbxStayOnTopClick(Sender: TObject);
begin
  if cbxStayOnTop.Checked then
    SetWindowPos(Handle,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NoMove or SWP_NoSize)
  else
    SetWindowPos(Handle,
                 HWND_NOTOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NoMove or SWP_NoSize);

end;


procedure TfrmLoopBackCapture.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(oAudioSink);
  CanClose := True;
end;


procedure TfrmLoopBackCapture.FormCreate(Sender: TObject);
begin
  // Create the AudioSink object.
  oAudioSink := TAudioSink.Create(Handle);
  oDataFlow := eDataFlow(-1);
end;

end.

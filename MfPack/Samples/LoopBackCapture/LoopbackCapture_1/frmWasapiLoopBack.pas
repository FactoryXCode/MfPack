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
// Revision Version: 3.1.7
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
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
// 25/04/2004 Tony                Updated to a more stable and crack free version.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPack/Samples/LoopbackCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Rita Han / Tony Kalf / FactoryX
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
  {ActiveX}
  Winapi.ActiveX,
  {System}
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  {CoreAudioApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  {Application}
  WasapiLoopback,
  Utils,
  dlgDevices,
  UniThreadTimer;


type
  TfrmLoopBackCapture = class(TForm)
    butStart: TButton;
    butStop: TButton;
    butPlayData: TButton;
    Panel1: TPanel;
    rbRenderingDevice: TRadioButton;
    rbCaptureDevice: TRadioButton;
    Panel2: TPanel;
    rbConsole: TRadioButton;
    rbMultimedia: TRadioButton;
    rbCommunications: TRadioButton;
    Panel3: TPanel;
    lblBufferDuration: TLabel;
    Panel4: TPanel;
    Label1: TLabel;
    lblFileExt: TLabel;
    edFileName: TEdit;
    cbxDontOverWrite: TCheckBox;
    Bevel1: TBevel;
    lblStatus: TLabel;
    butResetEngine: TButton;
    lblCaptureBufferDuration: TLabel;
    cbxAutoBufferSize: TCheckBox;
    sedBufferSize: TSpinEdit;
    butShowdlgDevices: TButton;
    cbxStayOnTop: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure butStartClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayDataClick(Sender: TObject);
    procedure cbxStayOnTopClick(Sender: TObject);
    procedure butShowdlgDevicesClick(Sender: TObject);
    procedure tbBufferDurationChange(Sender: TObject);
    procedure rbConsoleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure butResetEngineClick(Sender: TObject);
    procedure cbxAutoBufferSizeClick(Sender: TObject);
    procedure edFileNameKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    { Private declarations }
    prAudioSink: TAudioSink;
    prFileName: TFileName;
    prOrgFileName: TFileName;
    prEdited: Boolean;
    prDataFlow: EDataFlow;
    prRole: ERole;
    prBufferDuration: REFERENCE_TIME;

    // We use timers here, to prevent distortions when quering the capturethread for timing and processed data.
    // The timer must be set to 1 millisecond resolution.
    thrTimer: TUniThreadedTimer;
    aStopWatch: TStopwatch;
    BufferDurationCaptionSet: Boolean;

    procedure EnablePanels(aEnabled: Boolean);

    procedure CreateNewAudioSink();
    procedure RemoveAudioSink();
    function StartCapture(): HResult;

    // Event handlers.
    procedure OnCapturingStoppedEvent(Sender: TObject);
    procedure OnTimer(Sender: TObject);

    procedure SetBufferDuration();

  public
    { Public declarations }

  end;

var
  frmLoopBackCapture: TfrmLoopBackCapture;


implementation

{$R *.dfm}


procedure TfrmLoopBackCapture.EnablePanels(aEnabled: Boolean);
begin

  Panel1.Enabled := aEnabled;
  Panel2.Enabled := aEnabled;
  Panel3.Enabled := aEnabled;
  Panel4.Enabled := aEnabled;
end;


procedure TfrmLoopBackCapture.OnCapturingStoppedEvent(Sender: TObject);
var
  errorStatus: HResult;

begin
  if not Assigned(prAudioSink) then
    Exit;

  // Stop the timer and stopwatch.
  thrTimer.Enabled := False;
  aStopWatch.Stop;
  aStopWatch.Reset;

  errorStatus := prAudioSink.ErrorStatus;

  if (errorStatus = S_OK) or (errorStatus = HResult(AUDCLNT_E_OUT_OF_ORDER)) then
    begin
      lblStatus.Caption := Format('Capturing stopped. Captured %f Mb.',
                                  [prAudioSink.BytesWritten / (1000 * 1000)]);
      butPlayData.Enabled := True;
    end
  else if (errorStatus <> S_OK) then
    begin
      lblStatus.Caption := Format('Capturing stopped because of an error (hr = %d).', [errorStatus]);
      butPlayData.Enabled := False;
    end;

  butStop.Enabled := False;
  butStart.Enabled := True;
  EnablePanels(True);
end;


procedure TfrmLoopBackCapture.rbConsoleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  lblStatus.Caption := 'Start Capture';
end;


procedure TfrmLoopBackCapture.OnTimer(sender: TObject);
begin

  if not Assigned(prAudioSink) then
    Exit;

  if prAudioSink.StopRecording then
    Exit;

  // We only probe this once.
  if not BufferDurationCaptionSet then
    begin
      lblCaptureBufferDuration.Caption := Format('Capture buffer duration: %d ms.',
                                                 [prAudioSink.BufferDuration div 10000]);
      BufferDurationCaptionSet := True;
    end;

  lblStatus.Caption := 'Capturing from source: ' + FormatDateTime('hh:nn:ss:zzz',
                                                                  aStopWatch.ElapsedMilliseconds / MSecsPerDay);

  Application.ProcessMessages;
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

  if not Assigned(prAudioSink) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  if SUCCEEDED(hr) then
    begin

      prFileName := Format('%s', [edFileName.Text]);
      if (prOrgFileName = '') or prEdited then
        begin
          prOrgFileName := prFileName;
          prEdited := False;
        end;

      if cbxDontOverWrite.Checked then
        begin
          bFileExists := True;
          i := 0;
          while (bFileExists = True) do
            begin
              if FileExists(prFileName + lblFileExt.Caption) then
                begin
                  if (prOrgFileName = prFileName) then
                    prFileName := Format('%s(%d)',
                                         [edFileName.Text,
                                          i])
                  else
                    begin
                      prFileName := Format('%s(%d)',
                                           [prOrgFileName,
                                            i]);
                      edFileName.Text := prFileName;
                    end;
                  Inc(i);
                end
              else
                bFileExists := False;
            end;
        end;

      // Show new filename to user.
      edFileName.Text := prFileName;

      butStop.Enabled := True;
      butStart.Enabled := False;
      butPlayData.Enabled := False;
      EnablePanels(False);

      // User did not choose settings from dlg
      if prDataFlow = eDataFlow(-1) then
        begin
          prDataFlow := eRender;
          prRole := eMultimedia;
        end;

      // Buffersize depends on latency and bitrate
      SetBufferDuration();

      // Enable the timer.
      BufferDurationCaptionSet := False;
      thrTimer.Enabled := True;
      aStopWatch.Start;
      aStopWatch.StartNew;

      // Capture the audio stream from the default rendering device.
      hr := prAudioSink.RecordAudioStream(prDataFlow,
                                          prRole,
                                          prBufferDuration,
                                          StrToPWideChar(prFileName + lblFileExt.Caption));
      if FAILED(hr) then
        begin
          butStop.Enabled := False;
          butStart.Enabled := True;
          EnablePanels(True);
          thrTimer.Enabled := False;
          aStopWatch.Reset;
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
               StrToPWideChar(prFileName + lblFileExt.Caption),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmLoopBackCapture.butStartClick(Sender: TObject);
begin

  StartCapture();
end;


procedure TfrmLoopBackCapture.butStopClick(Sender: TObject);
begin

  prAudioSink.StopRecording := True;

  EnablePanels(True);
  Self.BorderIcons := [biSystemMenu, biMinimize];
end;


procedure TfrmLoopBackCapture.butResetEngineClick(Sender: TObject);
begin

  RemoveAudioSink();
  CreateNewAudioSink();
  EnablePanels(True);
  Self.BorderIcons := [biSystemMenu,
                       biMinimize];
  butPlayData.Enabled := False;
  butStop.Enabled := False;
  butStart.Enabled := True;
  lblStatus.Caption := 'The engine has been reset.'
end;


procedure TfrmLoopBackCapture.butShowdlgDevicesClick(Sender: TObject);
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
      prDataFlow := DevicesDlg.puDataFlow;
      rbRenderingDevice.Checked := (prDataFlow = eRender);
      rbCaptureDevice.Checked := (prDataFlow = eCapture);
      lblStatus.Caption := Format('Please select a%s.',[Panel2.Caption]);
    end
  else
    begin
      // User canceled.
      // Set radiobuttons to default.
      rbRenderingDevice.Checked := True;
      rbMultimedia.Checked := True;
      lblStatus.Caption := 'Start capture';
    end;
end;


procedure TfrmLoopBackCapture.cbxAutoBufferSizeClick(Sender: TObject);
begin
  if cbxAutoBufferSize.Checked then
    begin
      prBufferDuration := 0;
      sedBufferSize.Value := 0;
      sedBufferSize.Enabled := False;
      SetBufferDuration();
    end
  else
    sedBufferSize.Enabled := True;
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
  aStopWatch.Stop;
  thrTimer.Enabled := False;
  FreeAndNil(thrTimer);
  RemoveAudioSink();

  CanClose := True;
end;


procedure TfrmLoopBackCapture.FormCreate(Sender: TObject);
begin
  //
  lblStatus.ControlStyle := lblStatus.ControlStyle + [csOpaque];
  aStopWatch := TStopwatch.Create;
  thrTimer := TUniThreadedTimer.Create(nil);
  thrTimer.Period := 10;  // Ten millisecond resolution.
  thrTimer.Enabled := False;
  thrTimer.OnTimerEvent := OnTimer;
  CreateNewAudioSink();
  prEdited := False;
end;


procedure TfrmLoopBackCapture.CreateNewAudioSink();
begin

  // Create the AudioSink object.
  prAudioSink := TAudioSink.Create();
  // Set event handlers.
  prAudioSink.OnStoppedCapturing := OnCapturingStoppedEvent;
  prDataFlow := eDataFlow(-1);
  sedBufferSize.Value := 10;
  SetBufferDuration();
end;


procedure TfrmLoopBackCapture.edFileNameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  prEdited := True;
end;


procedure TfrmLoopBackCapture.RemoveAudioSink();
begin
  if Assigned(prAudioSink) then
    begin
      if not prAudioSink.StopRecording then
        prAudioSink.StopRecording := True;

      FreeAndNil(prAudioSink);
    end;
end;


procedure TfrmLoopBackCapture.tbBufferDurationChange(Sender: TObject);
begin
  SetBufferDuration();
end;


procedure TfrmLoopBackCapture.SetBufferDuration();
var
  sms: string;

begin

  prBufferDuration := (REFTIMES_PER_MILLISEC) * sedBufferSize.Value;
  if (prBufferDuration > REFTIMES_PER_MILLISEC) then
    sms := 'milliseconds'
  else
    sms := 'millisecond';

  if (prBufferDuration = 0) then
    lblBufferDuration.Caption := 'The audioclient will automaticly adjust the buffer duration.'
  else
    lblBufferDuration.Caption := Format('Capture buffer duration(%d %s)',
                                        [sedBufferSize.Value,
                                         sms])
end;


// initialization and finalization =============================================


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_LITE)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();

end.

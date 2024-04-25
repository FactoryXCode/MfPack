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
// Revision Version: 3.1.6
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
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  {CoreAudioApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.CoreAudioApi.MMDeviceApi,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  {Application}
  WasapiLoopback,
  Utils,
  dlgDevices;


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
    cbxStayOnTop: TCheckBox;
    butShowdlgDevices: TButton;
    Panel3: TPanel;
    tbBufferDuration: TTrackBar;
    lblBufferDuration: TLabel;
    Panel4: TPanel;
    Label1: TLabel;
    lblFileExt: TLabel;
    edFileName: TEdit;
    cbxDontOverWrite: TCheckBox;
    Bevel1: TBevel;
    lblStatus: TLabel;
    butResetEngine: TButton;
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

  private
    { Private declarations }
    oAudioSink: TAudioSink;
    iTotalBytesProcessed: Int64;
    sFileName: string;
    oDataFlow: EDataFlow;
    oRole: ERole;
    oBufferDuration: REFERENCE_TIME;

    procedure EnablePanels(aEnabled: Boolean);

    ///
    procedure CreateNewAudioSink();
    procedure RemoveAudioSink();

    function StartCapture(): HResult;

    // Event handlers.
    procedure OnCapturingStoppedEvent(Sender: TObject);
    procedure OnAudioSinkProgressEvent(Sender: TObject);

    procedure SetBufferDuration();

  public
    { Public declarations }


  end;

var
  frmLoopBackCapture: TfrmLoopBackCapture;


implementation

{$R *.dfm}

uses
  WinApi.MediaFoundationApi.MfUtils;


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
  if not Assigned(oAudioSink) then
    Exit;

  errorStatus := oAudioSink.ErrorStatus;

  if (errorStatus = S_OK) or (errorStatus = HResult(AUDCLNT_E_OUT_OF_ORDER)) then
    begin
      lblStatus.Caption := Format('Capturing stopped. Captured %f Mb.', [iTotalBytesProcessed / (1000 * 1000)]);
      butPlayData.Enabled := True;
    end
  else if (errorStatus <> S_OK) then
    begin
      lblStatus.Caption := Format('Capturing stopped because of an error (hr = %d). Captured %s bytes.', [errorStatus, iTotalBytesProcessed.ToString()]);
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

// Helper.
function BytesToTimeStr(pBytesWritten: Int64;
                        pSampleRate: Integer;
                        pChannels: Integer;
                        pBitsPerSecond: Integer;
                        pShowMilliSeconds: Boolean = True): string; inline;
const
  Bit32 = 4;
  Bit16 = 2;

var
  hns: Int64;

begin

  // Calculate time in 100-nanosecond units.
  if (pBitsPerSecond = 16) then
    hns := Trunc((pBytesWritten / (pSampleRate * 2 * pChannels)) * 10000000) // 16-bit audio.
  else if (pBitsPerSecond = 32) then
    hns := Trunc((pBytesWritten / (pSampleRate * 4 * pChannels)) * 10000000); // 32-bit audio.

  // Call HnsTimeToStr function to format the time string
  Result := HnsTimeToStr(hns,
                         pShowMilliSeconds);
end;


procedure TfrmLoopBackCapture.OnAudioSinkProgressEvent(Sender: TObject);
var
  latency: NativeInt;
  timePlayed: Int64;
  hnsStr: string;
  bitsPerSample: Integer;
  sampleRate: Integer;
  channels: Integer;

begin

  if not Assigned(oAudioSink) then
    Exit;

  latency := NativeInt(oAudioSink.Latency);
  sampleRate := oAudioSink.WaveFmtEx.nSamplesPerSec;
  channels := oAudioSink.WaveFmtEx.nChannels;
  bitsPerSample := oAudioSink.WaveFmtEx.wBitsPerSample;

  Inc(iTotalBytesProcessed,
      oAudioSink.BytesWritten);

  // Format to string (00:00:00,000)
  hnsStr := BytesToTimeStr(iTotalBytesProcessed,
                           sampleRate,
                           channels,
                           bitsPerSample,
                           True);

  lblStatus.Caption := Format('Capturing from source: %s (Latency: %d)',
                              [hnsStr,
                               latency]);

  Application.ProcessMessages();
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
  iTotalBytesProcessed := 0;

  if not Assigned(oAudioSink) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  if SUCCEEDED(hr) then
    begin

      sFileName := Format('%s%s',
                          [edFileName.Text,
                           lblFileExt.Caption]);

      if cbxDontOverWrite.Checked then
        begin
          bFileExists := True;
          i := 0;
          while (bFileExists = True) do
            begin
              if FileExists(sFileName) then
                begin
                  sFileName := Format('%s(%d)%s',
                                      [edFileName.Text,
                                      i,
                                      lblFileExt.Caption]);
                  Inc(i);
                end
              else
                bFileExists := False;
            end;
        end;

      butStop.Enabled := True;
      butStart.Enabled := False;
      butPlayData.Enabled := False;
      EnablePanels(False);

      // User did not choose settings from dlg
      if oDataFlow = eDataFlow(-1) then
        begin
          oDataFlow := eRender;
          oRole := eMultimedia;
        end;

      // Buffersize depends on latency and bitrate
      SetBufferDuration();

      // Capture the audio stream from the default rendering device.
      hr := oAudioSink.RecordAudioStream(oDataFlow,
                                         oRole,
                                         oBufferDuration,
                                         LPWSTR(sFileName));
      if FAILED(hr) then
        begin
          butStop.Enabled := False;
          butStart.Enabled := True;
          EnablePanels(True);
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
  StartCapture();
end;


procedure TfrmLoopBackCapture.butStopClick(Sender: TObject);
begin
  oAudioSink.StopRecording := True;
  EnablePanels(True);
  Self.BorderIcons := [biSystemMenu, biMinimize];
end;


procedure TfrmLoopBackCapture.butResetEngineClick(Sender: TObject);
begin
  RemoveAudioSink();
  CreateNewAudioSink();
  EnablePanels(True);
  Self.BorderIcons := [biSystemMenu, biMinimize];
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
      oDataFlow := DevicesDlg.oDataFlow;
      rbRenderingDevice.Checked := (oDataFlow = eRender);
      rbCaptureDevice.Checked := (oDataFlow = eCapture);
      lblStatus.Caption := Format('Please select a%s.',[Panel2.Caption]);
    end
  else
    begin
      // User canceled.
      // Set radiobuttons to default.
      rbRenderingDevice.Checked := True;
      rbMultimedia.Checked := True;
      lblStatus.Caption := 'Start Capture';
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
  RemoveAudioSink();
  CanClose := True;
end;


procedure TfrmLoopBackCapture.FormCreate(Sender: TObject);
begin
  //
  lblStatus.ControlStyle := lblStatus.ControlStyle + [csOpaque];

  CreateNewAudioSink();

  oDataFlow := eDataFlow(-1);
  tbBufferDuration.Position := 10;
  SetBufferDuration();
end;


procedure TfrmLoopBackCapture.CreateNewAudioSink();
begin

  // Create the AudioSink object.
  oAudioSink := TAudioSink.Create();
  // Set event handlers.
  oAudioSink.OnProcessingData := OnAudioSinkProgressEvent;
  oAudioSink.OnStoppedCapturing := OnCapturingStoppedEvent;
end;


procedure TfrmLoopBackCapture.RemoveAudioSink();
begin
  if Assigned(oAudioSink) then
    begin
      if not oAudioSink.StopRecording then
        oAudioSink.StopRecording := True;

      FreeAndNil(oAudioSink);
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
  oBufferDuration := (REFTIMES_PER_SEC) * tbBufferDuration.Position;
  if (oBufferDuration > REFTIMES_PER_SEC) then
    sms := 'milliseconds'
  else
    sms := 'millisecond';
  lblBufferDuration.Caption := Format('Capture Buffer Length(%d %s)',
                                      [tbBufferDuration.Position,
                                       sms])
end;



// initialization and finalization =============================================


initialization
  // A gui app should always use COINIT_APARTMENTTHREADED in stead of COINIT_MULTITHREADED
  CoInitializeEx(nil,
                COINIT_APARTMENTTHREADED);

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();
  CoUnInitialize();
end.

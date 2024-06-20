//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
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
// 12/06/2024 Tony                Updated to render in a separate thread.
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
unit frmMain;

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
  ThreadedWASLoopbackCapture,
  Common,
  dlgDevices,
  UniThreadTimer;


type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    lblBufferDuration: TLabel;
    Panel4: TPanel;
    Label1: TLabel;
    lblFileExt: TLabel;
    edFileName: TEdit;
    cbxDontOverWrite: TCheckBox;
    Bevel1: TBevel;
    lblStatus: TLabel;
    lblCaptureBufferDuration: TLabel;
    cbxAutoBufferSize: TCheckBox;
    Label2: TLabel;
    rbConsole: TRadioButton;
    rbMultimedia: TRadioButton;
    rbCommunications: TRadioButton;
    Label4: TLabel;
    cbxDisableMmcss: TCheckBox;
    Panel2: TPanel;
    Label3: TLabel;
    rbRenderingDevice: TRadioButton;
    rbCaptureDevice: TRadioButton;
    butShowdlgDevices: TButton;
    cbxEnableStreamSwitch: TCheckBox;
    cbxUseDeviceAudioFmt: TCheckBox;
    Label5: TLabel;
    spedLatency: TSpinEdit;
    sedBufferSize: TSpinEdit;
    cbxStayOnTop: TCheckBox;
    butStartStop: TButton;
    butPlayData: TButton;
    butResetEngine: TButton;
    procedure FormCreate(Sender: TObject);
    procedure butStartStopClick(Sender: TObject);
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
    prWASCapture: TWASCapture;
    prEndPoint: IMMDevice;
    prDeviceName: LPWSTR;
    prFileName: TFileName;
    prOrgFileName: TFileName;
    prEdited: Boolean;

    prEndPointDataFlow: EDataFlow;
    prEndPointRole: ERole;

    prEnableStreamSwitch: Boolean;
    prDisableMmcss: Boolean;

    pvBufferDuration: REFERENCE_TIME;
    pvTargetLatency: REFERENCE_TIME;
    BufferDurationCaptionSet: Boolean;

    // We use timers here, to prevent distortions when quering the capturethread for timing and processed data.
    // The timer must be set to 1 millisecond resolution.
    thrTimer: TUniThreadedTimer;
    aStopWatch: TStopwatch;
    prTimerDestroyed: Boolean;

    procedure CreateTimer();
    procedure KillTimer();

    procedure EnablePanels(aEnabled: Boolean);

    procedure SetParameters();
    function CreateEngine(): Boolean;
    procedure RemoveEngine();
    procedure SetBufferDuration();

    // Event handlers.
    procedure OnCapturingStoppedEvent(Sender: TObject);
    procedure OnTimer(Sender: TObject);

  public
    { Public declarations }

  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}


procedure TMainForm.EnablePanels(aEnabled: Boolean);
begin

  Panel1.Enabled := aEnabled;
  Panel3.Enabled := aEnabled;
  Panel4.Enabled := aEnabled;
end;


procedure TMainForm.CreateTimer();
begin

  if(thrTimer <> nil) then
    KillTimer();

  thrTimer := TUniThreadedTimer.Create(nil);
  thrTimer.Period := 100;  // 100 millisecond resolution.
  thrTimer.Enabled := False;
  thrTimer.OnTimerEvent := OnTimer;
  prTimerDestroyed := False;
end;


procedure TMainForm.KillTimer();
begin
  if (thrTimer <> nil) then
    begin
      thrTimer.Enabled := False;
      FreeAndNil(thrTimer);
    end;
  prTimerDestroyed := True;
end;


procedure TMainForm.OnCapturingStoppedEvent(Sender: TObject);
var
  Status: TDeviceState;

begin
  if not Assigned(prWASCapture) then
    Exit;

  // Stop the timer and stopwatch.
  KillTimer();
  aStopWatch.Stop;
  aStopWatch.Reset;

  Status := prWASCapture.DeviceState;

  if (Status = Stopped) or (Status = Error) then
    begin
      lblStatus.Caption := Format('Capturing stopped. Captured %f Mb.',
                                  [prWASCapture.BytesCaptured / (1000 * 1000)]);
      butPlayData.Enabled := True;
    end
  else if (Status = Error) then
    begin
      lblStatus.Caption := Format('Capturing stopped because of an error (hr = %d).', [E_FAIL]);
      butPlayData.Enabled := False;
    end;

  butStartStop.Enabled := True;
  EnablePanels(True);
end;


procedure TMainForm.rbConsoleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  lblStatus.Caption := 'Start Capture';
end;


procedure TMainForm.OnTimer(Sender: TObject);
begin

  if Assigned(prWASCapture) then
    if (prWASCapture.DeviceState = Capturing) then
      begin

        // We only probe this once.
        if not BufferDurationCaptionSet then
          begin
            lblCaptureBufferDuration.Caption := Format('Capture buffer duration: %d ms.',
                                                       [prWASCapture.FrameSize div 10000]);
            BufferDurationCaptionSet := True;
          end;

        lblStatus.Caption := 'Capturing from source: ' + FormatDateTime('hh:nn:ss.zzz',
                                                                        aStopWatch.ElapsedMilliseconds / MSecsPerDay);
      end;

  Application.ProcessMessages;
end;


procedure TMainForm.butPlayDataClick(Sender: TObject);
begin

  ShellExecute(Handle,
               'open',
               StrToPWideChar(prFileName + lblFileExt.Caption),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TMainForm.butStartStopClick(Sender: TObject);
var
  bFileExists: Boolean;
  i: Integer;
  bSuccess: Boolean;

begin
 if (tag = 1) then
   begin

     // Stop capture
     prWASCapture.Stop();
     // Stop the timer.
     //if prWASCapture.DeviceState in [Stopping, Stopped] then
     //  KillTimer(Self);

     // Destroy the engine
     prWASCapture.Shutdown();

     tag := 0;
     butStartStop.Caption := 'Start capture';
     EnablePanels(True);
     Self.BorderIcons := [biSystemMenu, biMinimize];
   end
 else
   begin
     pvTargetLatency := spedLatency.Value * REFTIMES_PER_MILLISEC;

     // Create the engine with setup-parameters
     if not CreateEngine() then
       Exit;

     // Check filename.
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

     butStartStop.Enabled := False;
     butPlayData.Enabled := False;
     EnablePanels(False);

     // Initialize.
     bSuccess := prWASCapture.Initialize(pvBufferDuration,
                                         pvTargetLatency,
                                         cbxUseDeviceAudioFmt.Checked);

     if bSuccess then
       // Capture the audio stream from the choosen rendering device.
       bSuccess := prWASCapture.Start(prFileName + lblFileExt.Caption);

     if not bSuccess then
         begin
           InfoMsg(optIDE,
                   'Unable to start capture.',
                   E_FAIL,
                   Handle);

           butStartStop.Enabled := True;
           EnablePanels(True);
           thrTimer.Enabled := False;
           aStopWatch.Reset;
           Exit;
         end
       else
         begin
           // Enable the timer.
           BufferDurationCaptionSet := False;
           CreateTimer();
           thrTimer.Enabled := True;
           aStopWatch.Start;
           aStopWatch.StartNew;
           butStartStop.Enabled := True;
           butStartStop.Caption := 'Stop capture';
           tag := 1;
         end;
   end; // Start
  Sleep(10);
end;


procedure TMainForm.butResetEngineClick(Sender: TObject);
begin

  RemoveEngine();

  CreateEngine();

  EnablePanels(True);
  Self.BorderIcons := [biSystemMenu,
                       biMinimize];
  butPlayData.Enabled := False;
  butStartStop.Enabled := True;
  lblStatus.Caption := 'The engine has been reset.'
end;


procedure TMainForm.butShowdlgDevicesClick(Sender: TObject);
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
      prEndPointDataFlow := DevicesDlg.DataFlow;
      prEndPoint := DevicesDlg.EndPointDevice;
      rbRenderingDevice.Checked := (prEndPointDataFlow = eRender);
      rbCaptureDevice.Checked := (prEndPointDataFlow = eCapture);

      lblStatus.Caption := 'Please select a device role.';
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


procedure TMainForm.cbxAutoBufferSizeClick(Sender: TObject);
begin

  if cbxAutoBufferSize.Checked then
    begin
      sedBufferSize.Value := 0;
      sedBufferSize.Enabled := False;
    end
  else
    sedBufferSize.Enabled := True;
  SetBufferDuration();
end;


procedure TMainForm.cbxStayOnTopClick(Sender: TObject);
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


procedure TMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin

  CanClose := False;
  aStopWatch.Stop;

  KillTimer();
  // Wait until the timer signals destroyed.
  while True do
    begin
      if prTimerDestroyed then
        Break;
      Sleep(1);
    end;

  RemoveEngine();
  CanClose := True;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Prevents flickering.
  lblStatus.ControlStyle := lblStatus.ControlStyle + [csOpaque];
  aStopWatch := TStopwatch.Create;
  prEdited := False;
  cbxUseDeviceAudioFmt.Hint := 'Use the default audio format (44.1 khz/ 16 bit/ PCM).' + #13 +
                               'If you disable this option, the endpoint''s audio format will be used.';
end;


procedure TMainForm.SetParameters();
begin

  // Set dataflow.
  if rbRenderingDevice.Checked then
    prEndPointDataFlow := eRender;
  if rbCaptureDevice.Checked then
    prEndPointDataFlow := eCapture;

  // Set role.
  if rbConsole.Checked then
    prEndPointRole := eConsole;
  if rbMultimedia.Checked then
    prEndPointRole := eMultimedia;
  if rbCommunications.Checked then
    prEndPointRole := eCommunications;

  prEnableStreamSwitch := cbxEnableStreamSwitch.Checked;
  prDisableMmcss := cbxDisableMmcss.Checked;

  prEndPoint := DevicesDlg.EndPointDevice;
end;


function TMainForm.CreateEngine(): Boolean;
begin

  // Destroy an existing capture engine.
  if Assigned(prWASCapture) then
    RemoveEngine();

  // Create the capture object.
  prWASCapture := TWASCapture.Create(Handle,
                                     prEndPoint,
                                     prEnableStreamSwitch,
                                     prDisableMmcss,
                                     prEndPointRole);
  if not Assigned(prWASCapture) then
    begin
      InfoMsg(optIDE,
              'Unable create the WASCapture engine.',
              E_POINTER,
              Handle);
      Exit(False);
    end
  else
    begin
      // Set event handler.  Shutdown();
      prWASCapture.OnStoppedCapturing := OnCapturingStoppedEvent;
      InfoMsg(optIDE,
              Format('The WASCapture engine "%s" successfully initialized.', [WideCharToString(prDeviceName)]),
              S_OK,
              Handle);
      SetParameters();
      SetBufferDuration();
      Result := True;
    end;
end;


procedure TMainForm.edFileNameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  prEdited := True;
end;


procedure TMainForm.RemoveEngine();
begin

  if Assigned(prWASCapture) then
    begin
      prWASCapture.Stop();
      prWASCapture.OnStoppedCapturing := nil;
      FreeAndNil(prWASCapture);
    end;
end;


procedure TMainForm.tbBufferDurationChange(Sender: TObject);
begin

  SetBufferDuration();
end;


procedure TMainForm.SetBufferDuration();
var
  sms: string;

begin

  if cbxAutoBufferSize.Checked then
    pvBufferDuration := 0
  else
    pvBufferDuration := (REFTIMES_PER_MILLISEC) * sedBufferSize.Value;

  if (pvBufferDuration > REFTIMES_PER_MILLISEC) then
    sms := 'milliseconds'
  else
    sms := 'millisecond';

  if (pvBufferDuration = 0) then
    lblBufferDuration.Caption := 'The audioclient will automaticly adjust the buffer duration.'
  else
    lblBufferDuration.Caption := Format('Capture buffer duration(%d %s)',
                                        [sedBufferSize.Value,
                                         sms])
end;


// initialization and finalization =============================================


initialization
  // Initialize the COM library.
  //CoInitializeEx(nil,
  //               COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);

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
  //CoUnInitialize;
end.

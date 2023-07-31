// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmLoopBackCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: The audio loopbackcapture engine.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Jacob C.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 31/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: LoopBackAudio Capture example.
//
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
unit frmLoopBackCapture;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.ComBaseApi,
  Winapi.ShellAPI,
  {ActiveX}
  WinApi.ActiveX,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClient,
  {Application}
  Common,
  LoopbackCapture,
  ProcessInfoDlg;

type
  TfrmMain = class(TForm)
    edPID: TEdit;
    Label3: TLabel;
    rb2: TRadioButton;
    rb1: TRadioButton;
    butGetPID: TButton;
    Button1: TButton;
    Bevel2: TBevel;
    Label2: TLabel;
    edProcName: TEdit;
    cbxStayOnTop: TCheckBox;
    Panel1: TPanel;
    Label1: TLabel;
    lblFileExt: TLabel;
    edFileName: TEdit;
    cbxDontOverWrite: TCheckBox;
    butStart: TButton;
    butStop: TButton;
    butPlayData: TButton;
    lblMsg: TLabel;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Panel3: TPanel;
    lblDeviceBufferDuration: TLabel;
    tbDeviceBufferDuration: TTrackBar;
    Label4: TLabel;
    rb441b16: TRadioButton;
    rb48b24: TRadioButton;
    rb48b32: TRadioButton;
    rb96b24: TRadioButton;
    rb96b32: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject;
                             var CanClose: Boolean);
    procedure butPlayDataClick(Sender: TObject);
    procedure butStartClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure butGetPIDClick(Sender: TObject);
    procedure edFileNameKeyUp(Sender: TObject;
                              var Key: Word;
                              Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure edPIDKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxStayOnTopClick(Sender: TObject);
    procedure tbDeviceBufferDurationChange(Sender: TObject);

  private
    { Private declarations }
    sFileName: string;
    sOrgFileName: string;
    bEdited: Boolean;
    iProgress: Int64;
    bIncludeProcessTree: Boolean;
    oLoopbackCapture: TLoopbackCapture;
    aprocessId: Integer;
    aWavFmt: TWavFormat;
    aDeviceBufferDuration: REFERENCE_TIME;

    procedure OnProgressEvent(var AMessage: TMessage); message WM_PROGRESSNOTIFY;
    procedure OnRecordingStopped(var AMessage: TMessage); message WM_RECORDINGSTOPPEDNOTYFY;
    function StartCapture(): HResult;
    procedure SetDeviceBufferDuration();

  public
    { Public declarations }

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure TfrmMain.butPlayDataClick(Sender: TObject);
begin
  ShellExecute(Handle,
               'open',
               StrToPWideChar(sFileName + lblFileExt.Caption),
               nil,
               nil,
               SW_SHOWNORMAL) ;
end;


procedure TfrmMain.butStartClick(Sender: TObject);
var
  hr: HResult;

begin
  // Set to default, if user selected nothing.
  if (aprocessId = 0) then
    begin
      butGetPIDClick(Self);
      rb1.Checked := true;
    end;

  hr := StartCapture();

  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('Your computer does not support this Media Foundation API version' +
                         IntToStr(MF_VERSION) + '.'),
                 LPCWSTR('MFStartup Failure!'),
                 MB_ICONSTOP);
    end;

  butStart.Enabled := not SUCCEEDED(hr);
  butStop.Enabled := SUCCEEDED(hr);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
var
  hr: HResult;

begin
  hr := oLoopbackCapture.StopCaptureAsync();

  butStart.Enabled := SUCCEEDED(hr);
  butStop.Enabled := not SUCCEEDED(hr);
end;


procedure TfrmMain.Button1Click(Sender: TObject);
begin
  // Create the dialog if it's not allready done.
  if not Assigned(dlgProcessInfo) then
    begin
      Application.CreateForm(TdlgProcessInfo,
                             dlgProcessInfo);
      dlgProcessInfo.Visible := False;
    end;

  // Ask the user to select one.
  if (dlgProcessInfo.ShowModal = mrOk) then
    begin
      aprocessId := dlgProcessInfo.SelectedPID;
      edPID.Text := IntToStr(aprocessId);
      edProcName.Text := dlgProcessInfo.SelectedProcName;
    end
  else
    begin
      // User canceled.
    end;
end;


procedure TfrmMain.cbxStayOnTopClick(Sender: TObject);
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


procedure TfrmMain.edFileNameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  bEdited := True;
end;


procedure TfrmMain.edPIDKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if TryStrToInt(edPID.Text, i) and (i >= 0) then
   aprocessId := i;
end;


// Get the PID from this application
procedure TfrmMain.butGetPIDClick(Sender: TObject);
begin
  edPID.Text := IntToStr(GetCurrentProcessId());
  edProcName.Text := Application.Title;
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(oLoopbackCapture);
  CanClose := True;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  oLoopbackCapture := TLoopbackCapture.Create(Handle);
  butGetPID.OnClick(Self);
  SetDeviceBufferDuration();
  bEdited := False;
end;


function TfrmMain.StartCapture(): HResult;
var
  hr: HResult;
  i: Integer;
  bFileExists: Boolean;

label
  done;

begin
  hr := S_OK;
  iProgress := 0;

  if not Assigned(oLoopbackCapture) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  // Check for valid inputs
  aprocessId := StrToInt(edPID.Text);
  if (aprocessId <= 0) then
    begin
      aprocessId := 0;
      edPID.Text := IntToStr(aprocessId);
    end;

  if rb1.Checked then
    bIncludeProcessTree := False
  else if rb2.Checked then
    bIncludeProcessTree := True;

  // Buffersize depends on latency and bitrate
  SetDeviceBufferDuration();

  // Bitrate and resolution.
  if rb441b16.Checked then
    aWavFmt := fmt44100b16
  else if rb48b24.Checked then
    aWavFmt := fmt48000b24
  else if rb48b32.Checked then
    aWavFmt := fmt48000b32
  else if rb96b24.Checked then
    aWavFmt := fmt96000b24
  else if rb96b32.Checked then
    aWavFmt := fmt96000b32;

  if SUCCEEDED(hr) then
    begin
      sFileName := Format('%s', [edFileName.Text]);
      if (sOrgFileName = '') or bEdited then
        begin
          sOrgFileName := sFileName;
          bEdited := False;
        end;

      if cbxDontOverWrite.Checked then
        begin
          bFileExists := True;
          i := 0;
          while (bFileExists = True) do
            begin
              if FileExists(sFileName + lblFileExt.Caption) then
                begin
                  if (sOrgFileName = sFileName) then
                    sFileName := Format('%s(%d)', [edFileName.Text, i])
                  else
                    begin
                      sFileName := Format('%s(%d)', [sOrgFileName, i]);
                      edFileName.Text := sFileName;
                    end;
                  Inc(i);
                end
              else
                bFileExists := False;
            end;
        end;

      // Show new filename to user.
      edFileName.Text := sFileName;

      butStop.Enabled := True;
      butStart.Enabled := False;
      butPlayData.Enabled := False;

      // Capture the audio stream from the default rendering device.
      hr := oLoopbackCapture.StartCaptureAsync(Handle,
                                               aprocessId,
                                               bIncludeProcessTree,
                                               aWavFmt,
                                               aDeviceBufferDuration,
                                               LPCWSTR(sFileName + lblFileExt.Caption));
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


procedure TfrmMain.OnProgressEvent(var aMessage: TMessage);
begin
  iProgress := aMessage.WParam;
  lblMsg.Caption := Format('Capturing from source: Bytes processed: %d',[iProgress]);
end;


procedure TfrmMain.OnRecordingStopped(var AMessage: TMessage);
begin
  butPlayData.Enabled := True;
  lblMsg.Caption := Format('Capturing Stopped: %s bytes processed.', [iProgress.ToString()]);
end;


procedure TfrmMain.tbDeviceBufferDurationChange(Sender: TObject);
begin
  SetDeviceBufferDuration();
end;


procedure TfrmMain.SetDeviceBufferDuration();
begin
  lblDeviceBufferDuration.Caption := Format('Device buffer (%d MilliSeconds)', [tbDeviceBufferDuration.Position]);
  aDeviceBufferDuration := tbDeviceBufferDuration.Position * AUDIO_BUFFER_FMT;
end;


end.

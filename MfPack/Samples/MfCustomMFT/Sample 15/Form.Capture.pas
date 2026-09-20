// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.Capture.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: VCL form for selecting capture devices, previewing video,
//              and changing the live microphone delay.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
// Source: Microsoft Learn.
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
unit Form.Capture;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  {Application}
  CaptureDevices,
  CaptureSession;

type
  TfrmCapture = class(TForm)
    pnlPreview: TPanel;
    Panel1: TPanel;
    lblDelay: TLabel;
    lblWet: TLabel;
    lblHeadphones: TLabel;
    tbDelay: TTrackBar;
    tbWet: TTrackBar;
    btnStart: TButton;
    btnStop: TButton;
    Panel2: TPanel;
    lblCamera: TLabel;
    lblMicrophone: TLabel;
    cbCamera: TComboBox;
    cbMicrophone: TComboBox;
    btnRefresh: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tbEffectChange(Sender: TObject);
  private
    FCameras: TCaptureDeviceList;
    FMicrophones: TCaptureDeviceList;
    FSession: ICaptureSession;
    FSessionNumber: Cardinal;
    FStarting: Boolean;
    FRunning: Boolean;
    procedure RefreshDevices();
    procedure UpdateControls();
    procedure StopLive();
    procedure CaptureEvent(var AMessage: TMessage); message WM_CAPTURE_EVENT;
  end;

var
  frmCapture: TfrmCapture;

implementation

{$R *.dfm}

uses
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfIdl;

procedure TfrmCapture.FormCreate(Sender: TObject);
begin
  FCameras := TCaptureDeviceList.Create(
                MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
  FMicrophones := TCaptureDeviceList.Create(
                    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID);
  tbDelay.Position := 500;
  tbWet.Position := 35;
  tbEffectChange(nil);
  RefreshDevices();
end;

procedure TfrmCapture.FormDestroy(Sender: TObject);
begin
  if Assigned(FSession) then
    begin
      FSession.Close(True);
      FSession := nil;
    end;
  FMicrophones.Free();
  FCameras.Free();
end;

procedure TfrmCapture.RefreshDevices();
var
  I: Integer;
  Hr: HResult;
begin
  cbCamera.Items.Clear();
  cbMicrophone.Items.Clear();
  Hr := FCameras.Refresh();
  if FAILED(Hr) then
    begin
      lblStatus.Caption := 'Camera enumeration: ' +
                           Format('$%.8x: %s',
                                  [Cardinal(Hr), SysErrorMessage(Cardinal(Hr))]);
      Exit;
    end;
  Hr := FMicrophones.Refresh();
  if FAILED(Hr) then
    begin
      lblStatus.Caption := 'Microphone enumeration: ' +
                           Format('$%.8x: %s',
                                  [Cardinal(Hr), SysErrorMessage(Cardinal(Hr))]);
      Exit;
    end;
  for I := 0 to FCameras.Count() - 1 do
    cbCamera.Items.Add(FCameras.Name(I));
  for I := 0 to FMicrophones.Count() - 1 do
    cbMicrophone.Items.Add(FMicrophones.Name(I));
  if cbCamera.Items.Count > 0 then
    cbCamera.ItemIndex := 0;
  if cbMicrophone.Items.Count > 0 then
    cbMicrophone.ItemIndex := 0;
  if (cbCamera.Items.Count = 0) or (cbMicrophone.Items.Count = 0) then
    lblStatus.Caption := 'Connect a camera and microphone, then click Refresh.'
  else
    lblStatus.Caption := 'Ready for live preview.';
  UpdateControls();
end;

procedure TfrmCapture.UpdateControls();
begin
  btnStart.Enabled := not FStarting and not FRunning and
                      (cbCamera.ItemIndex >= 0) and
                      (cbMicrophone.ItemIndex >= 0);
  btnStop.Enabled := FStarting or FRunning;
  btnRefresh.Enabled := not FStarting and not FRunning;
  cbCamera.Enabled := btnRefresh.Enabled;
  cbMicrophone.Enabled := btnRefresh.Enabled;
end;

procedure TfrmCapture.btnRefreshClick(Sender: TObject);
begin
  RefreshDevices();
end;

procedure TfrmCapture.btnStartClick(Sender: TObject);
begin
  Inc(FSessionNumber);
  FSession := CreateCaptureSession(Handle,
                                   FSessionNumber);
  try
    FSession.Open(FCameras.Activate(cbCamera.ItemIndex),
                  FMicrophones.Activate(cbMicrophone.ItemIndex),
                  pnlPreview.Handle,
                  tbDelay.Position,
                  tbWet.Position);
    FStarting := True;
    lblStatus.Caption := 'Preparing live camera and microphone...';
    UpdateControls();
  except
    on E: Exception do
      begin
        FSession.Close(False);
        FSession := nil;
        Inc(FSessionNumber);
        lblStatus.Caption := E.Message;
        UpdateControls();
      end;
  end;
end;

procedure TfrmCapture.StopLive();
begin
  // Invalidate notices already queued by this session before closing it.
  Inc(FSessionNumber);
  try
    if Assigned(FSession) then
      begin
        try
          if FRunning then
            FSession.Stop();
        finally
          FSession.Close(False);
          FSession := nil;
        end;
      end;
  finally
    FStarting := False;
    FRunning := False;
    UpdateControls();
  end;
end;

procedure TfrmCapture.btnStopClick(Sender: TObject);
begin
  try
    StopLive();
    lblStatus.Caption := 'Live preview stopped.';
  except
    on E: Exception do
      lblStatus.Caption := E.Message;
  end;
end;

procedure TfrmCapture.tbEffectChange(Sender: TObject);
begin
  lblDelay.Caption := Format('Delay: %d ms', [tbDelay.Position]);
  lblWet.Caption := Format('Wet mix: %d%%', [tbWet.Position]);
  // This changes the MFT in the running topology without stopping capture.
  if Assigned(FSession) then
    try
      FSession.UpdateEffect(tbDelay.Position,
                            tbWet.Position);
    except
      on E: Exception do
        lblStatus.Caption := E.Message;
    end;
end;

procedure TfrmCapture.CaptureEvent(var AMessage: TMessage);
var
  Notice: TCaptureNotice;
begin
  Notice := TCaptureNotice(AMessage.LParam);
  try
    if Notice.SessionNumber <> FSessionNumber then
      Exit;
    case Notice.Kind of
      cekReady:
        begin
          if Assigned(FSession) then
            try
              FSession.Start();
            except
              on E: Exception do
                begin
                  StopLive();
                  lblStatus.Caption := E.Message;
                end;
            end;
        end;
      cekStarted:
        begin
          FStarting := False;
          FRunning := True;
          lblStatus.Caption := Notice.Text;
          UpdateControls();
        end;
      cekError, cekEnded:
        begin
          try
            StopLive();
          except
            // Preserve the original Media Session error in the status label.
          end;
          lblStatus.Caption := Notice.Text;
        end;
    end;
  finally
    Notice.Free();
  end;
end;

end.

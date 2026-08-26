// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
//
// Revision Version: 4.0.0
//
// Description:
//   MfSimpleWebCamStreamer - simple webcam + microphone A/V sample.
//
// Remarks:
//   Milestone 1: capture camera video and microphone audio with two asynchronous
//   IMFSourceReaders and write synchronized H.264/AAC to one MP4 SinkWriter.
//   The network/fMP4 output layer is intentionally the next milestone.
//
// Compiler version: Delphi XE7 and later.
// SDK version: MfPack 4.0.0.
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License.
//
//==============================================================================

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.Main.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: MfSimpleWebCamStreamer - simple webcam + microphone A/V sample.
//              Capture camera video and microphone audio with two asynchronous
//              IMFSourceReaders and write synchronized H.264/AAC to one MP4 SinkWriter.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
unit Form.Main;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  {Application}
  SimpleAvCapture,
  SimpleHttpServer;

type
  TfrmMain = class(TForm)
    lblCamera: TLabel;
    lblMicrophone: TLabel;
    cbCamera: TComboBox;
    cbMicrophone: TComboBox;
    btnStart: TButton;
    btnStop: TButton;
    btnPauseHttp: TButton;
    btnResumeHttp: TButton;
    lblOutput: TLabel;
    edOutput: TEdit;
    lblHttpPort: TLabel;
    edHttpPort: TEdit;
    memStatus: TMemo;
    tmrStatus: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPauseHttpClick(Sender: TObject);
    procedure btnResumeHttpClick(Sender: TObject);
    procedure tmrStatusTimer(Sender: TObject);

  private

    FVideoDevices: TSimpleDeviceList;
    FAudioDevices: TSimpleDeviceList;
    FCapture: TSimpleAvCapture;
    FHttpServer: TSimpleHttpServer;
    FLastFragmentCount: UInt64;
    FStreamGeneration: UInt64;

    FRecordingStream: TFileStream;
    FRecordingStarted: TDateTime;
    FRecordingFolder: string;
    FRecordingBaseName: string;
    FRecordingFileName: string;

    procedure FillDeviceLists();
    procedure AddStatus(const S: string);
    procedure UpdateControls();

    function BuildRecordingFileName(): string;
    function StartRecordingFile(): Boolean;
    procedure StopRecordingFile();
    procedure ServiceRecordingArchive();
    procedure CheckRecordingRoll();
    procedure DeleteOldRecordings();

    function HttpGetInit(out AData: TBytes): Boolean;
    function HttpGetFragment(const ASequence: UInt64;
                             out AData: TBytes): Boolean;
    function HttpGetStatus(out AText: AnsiString): Boolean;
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

const
  RECORDING_FILE_MINUTES = 60;
  RECORDING_RETENTION_FILES = 48;  // We do a cycle of every 48 hours before the oldest mp4 will be deleted.


procedure TfrmMain.AddStatus(const S: string);
var
  OldSelStart: Integer;
  OldSelLength: Integer;
  HadSelection: Boolean;

begin

  OldSelStart := memStatus.SelStart;
  OldSelLength := memStatus.SelLength;
  HadSelection := (OldSelLength > 0);

  memStatus.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);

  // Do not steal the selection while the user is copying diagnostic text.
  // When there is no active selection, keep the normal tail-follow behavior.
  if HadSelection then
    begin
      memStatus.SelStart := OldSelStart;
      memStatus.SelLength := OldSelLength;
    end
  else
    begin
      memStatus.SelStart := Length(memStatus.Text);
      memStatus.Perform(EM_SCROLLCARET,
                        0,
                        0);
    end;
end;

function TfrmMain.BuildRecordingFileName(): string;
begin

  Result := IncludeTrailingPathDelimiter(FRecordingFolder) +
            FRecordingBaseName + '_' +
            FormatDateTime('yyyymmdd_hhnnss',
                           Now) +
            '.mp4';
end;


function TfrmMain.StartRecordingFile(): Boolean;
var
  InitSegment: TBytes;

begin

  Result := False;

  if Assigned(FRecordingStream) then
    Exit(True);

  if not Assigned(FCapture) or
     not FCapture.GetInitSegment(InitSegment) or
     (Length(InitSegment) = 0) then
    Exit;

  FRecordingFileName := BuildRecordingFileName;

  // Every archive file must start its fragmented-MP4 decode timeline at zero.
  // The observer rebases tfdt independently for video and audio from the first
  // fragment written to this new file.
  FCapture.ResetArchiveTimeline;

  try
    FRecordingStream := TFileStream.Create(FRecordingFileName,
                                           fmCreate or fmShareDenyWrite);

    FRecordingStream.WriteBuffer(InitSegment[0],
                                 Length(InitSegment));

    FRecordingStarted := Now;
    edOutput.Text := FRecordingFileName;

    AddStatus(Format('Recording file started: %s',
                     [ExtractFileName(FRecordingFileName)]));

    Result := True;

  except
    on E: Exception do
      begin
        FreeAndNil(FRecordingStream);
        AddStatus('Recording file start failed: ' + E.Message);
      end;
  end;

  InitSegment := nil;
end;


procedure TfrmMain.StopRecordingFile();
begin

  if Assigned(FRecordingStream) then
    begin
      FreeAndNil(FRecordingStream);

      if (FRecordingFileName <> '') then
        AddStatus(Format('Recording file closed: %s',
                         [ExtractFileName(FRecordingFileName)]));
    end;

  FRecordingFileName := '';
  FRecordingStarted := 0;
end;


procedure TfrmMain.ServiceRecordingArchive;
var
  Fragment: TBytes;

begin

  if not Assigned(FCapture) or (FCapture.State <> csCapturing) then
    Exit;

  if not Assigned(FRecordingStream) then
    if not StartRecordingFile then
      Exit;

  while FCapture.TryPopArchiveFragment(Fragment) do
    begin
      if (Length(Fragment) > 0) then
        FRecordingStream.WriteBuffer(Fragment[0],
                                     Length(Fragment));

      Fragment := nil;
    end;
end;


procedure TfrmMain.CheckRecordingRoll;
begin

  if not Assigned(FRecordingStream) or
     (FRecordingStarted = 0) then
    Exit;

  if (MinutesBetween(Now,
                    FRecordingStarted) < RECORDING_FILE_MINUTES) then
    Exit;

  // Drain everything already produced before closing this archive file.
  // The single MF live writer itself is completely untouched.
  ServiceRecordingArchive();

  FreeAndNil(FRecordingStream);

  AddStatus(Format('Recording rolled from %s. Live stream unchanged.',
                   [ExtractFileName(FRecordingFileName)]));

  FRecordingFileName := '';
  FRecordingStarted := 0;

  // The next ServiceRecordingArchive call writes the same init segment to the
  // new file, followed by subsequent already-encoded raw moof+mdat fragments.
  StartRecordingFile();

  DeleteOldRecordings();
end;


procedure TfrmMain.DeleteOldRecordings();
var
  SearchRec: TSearchRec;
  Files: TStringList;
  FileName: string;
  I: Integer;

begin

  Files := TStringList.Create();

  try
    if FindFirst(IncludeTrailingPathDelimiter(FRecordingFolder) +
                 FRecordingBaseName + '_*.mp4',
                 faAnyFile and not faDirectory,
                 SearchRec) = 0 then
      begin
        try
          repeat
            Files.Add(SearchRec.Name);
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;

    Files.Sort;

    for I := 0 to Files.Count - RECORDING_RETENTION_FILES - 1 do
      begin
        FileName := IncludeTrailingPathDelimiter(FRecordingFolder) +
                    Files[I];

        if not SameText(FileName,
                        FRecordingFileName) then
          DeleteFile(FileName);
      end;
  finally
    Files.Free;
  end;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FVideoDevices := TSimpleDeviceList.Create(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  FAudioDevices := TSimpleDeviceList.Create(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID);

  FCapture := TSimpleAvCapture.Create();

  FHttpServer := TSimpleHttpServer.Create();
  FHttpServer.OnGetInit := HttpGetInit();
  FHttpServer.OnGetFragment := HttpGetFragment();
  FHttpServer.OnGetStatus := HttpGetStatus();

  FLastFragmentCount := 0;
  FStreamGeneration := 0;

  FRecordingStream := nil;
  FRecordingStarted := 0;
  FRecordingFolder := ExtractFilePath(Application.ExeName);
  FRecordingBaseName := 'SimpleWebCamStream';
  FRecordingFileName := '';

  edOutput.Text := BuildRecordingFileName;
  edHttpPort.Text := '8080';

  FillDeviceLists();
  UpdateControls();
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin

  tmrStatus.Enabled := False;

  StopRecordingFile;

  FreeAndNil(FHttpServer);
  FreeAndNil(FCapture);
  FreeAndNil(FAudioDevices);
  FreeAndNil(FVideoDevices);
end;


procedure TfrmMain.FillDeviceLists();
var
  hr: HRESULT;
  I: UINT32;
  Name: string;

begin

  cbCamera.Items.BeginUpdate;
  cbMicrophone.Items.BeginUpdate;

  try
    cbCamera.Clear;
    cbMicrophone.Clear;

    hr := FVideoDevices.Enumerate;
    if SUCCEEDED(HR) and (FVideoDevices.Count > 0) then
      for I := 0 to FVideoDevices.Count - 1 do
        if SUCCEEDED(FVideoDevices.GetFriendlyName(I, Name)) then
          cbCamera.Items.Add(Name);

    hr := FAudioDevices.Enumerate();

    if SUCCEEDED(hr) and (FAudioDevices.Count > 0) then
      for I := 0 to FAudioDevices.Count - 1 do
        if SUCCEEDED(FAudioDevices.GetFriendlyName(I,
                                                   Name)) then
          cbMicrophone.Items.Add(Name);

    if (cbCamera.Items.Count > 0) then
      cbCamera.ItemIndex := 0;

    if (cbMicrophone.Items.Count > 0) then
      cbMicrophone.ItemIndex := 0;

    AddStatus(Format('Found %d camera(s), %d microphone(s).',
                     [cbCamera.Items.Count,
                      cbMicrophone.Items.Count]));
  finally
    cbCamera.Items.EndUpdate;
    cbMicrophone.Items.EndUpdate;
  end;
end;


procedure TfrmMain.UpdateControls();
var
  Capturing: Boolean;

begin

  Capturing := Assigned(FCapture) and
               (FCapture.State = csCapturing);

  btnStart.Enabled := not Capturing and
                      (cbCamera.ItemIndex >= 0) and
                      (cbMicrophone.ItemIndex >= 0);

  btnStop.Enabled := Capturing;

  btnPauseHttp.Enabled := Capturing and
                          Assigned(FHttpServer) and
                          FHttpServer.Running and
                          not FHttpServer.DebugDropRequests;

  btnResumeHttp.Enabled := Capturing and
                           Assigned(FHttpServer) and
                           FHttpServer.Running and
                           FHttpServer.DebugDropRequests;

  cbCamera.Enabled := not Capturing;
  cbMicrophone.Enabled := not Capturing;
  edOutput.Enabled := not Capturing;
  edHttpPort.Enabled := not Capturing;
end;


procedure TfrmMain.btnStartClick(Sender: TObject);
var
  VideoActivate: IMFActivate;
  AudioActivate: IMFActivate;
  HttpPortValue: Integer;
  HttpPort: Word;
  HR: HRESULT;

begin

  if not TryStrToInt(Trim(edHttpPort.Text),
                     HttpPortValue) or
     (HttpPortValue < 1) or
     (HttpPortValue > 65535) then
    begin
      MessageDlg('HTTP port must be a number from 1 through 65535.',
                 mtError,
                 [mbOK],
                 0);
      edHttpPort.SetFocus;
      Exit;
    end;

  HttpPort := Word(HttpPortValue);

  HR := FVideoDevices.GetActivate(cbCamera.ItemIndex,
                                  VideoActivate);

  if SUCCEEDED(HR) then
    HR := FAudioDevices.GetActivate(cbMicrophone.ItemIndex,
                                    AudioActivate);

  if SUCCEEDED(HR) then
    begin
      FLastFragmentCount := 0;
      FRecordingFileName := '';
      FRecordingStarted := 0;
      edOutput.Text := BuildRecordingFileName;

      HR := FCapture.Start(VideoActivate,
                           AudioActivate,
                           edOutput.Text);
    end;

  if FAILED(HR) then
    begin
      AddStatus(Format('Start failed: 0x%.8x', [Cardinal(HR)]));
      MessageDlg(Format('Unable to start capture.'#13#10'HRESULT = 0x%.8x',
                        [Cardinal(HR)]),
                 mtError,
                 [mbOK],
                 0);
    end
  else
    begin
      Inc(FStreamGeneration);

      AddStatus(Format('Capture started. Stream generation=%d.',
                       [FStreamGeneration]));
      AddStatus('Video: H.264, Audio: AAC, fragmented MP4.');
      AddStatus('Recording waits for init.mp4, then writes encoded fragments.');

      DeleteOldRecordings();

      FHttpServer.DebugDropRequests := False;

      if FHttpServer.Start(HttpPort) then
        begin
          AddStatus(Format('HTTP server started on port %d, listening on all IPv4 interfaces.',
                           [HttpPort]));
          AddStatus(Format('Local test: http://127.0.0.1:%d/',
                           [HttpPort]));
          AddStatus('LAN/WAN path uses the same port unless the router maps another external port.');
          AddStatus(Format('Status: http://127.0.0.1:%d/status',
                            [HttpPort]));
          AddStatus(Format('Init: http://127.0.0.1:%d/init.mp4',
                           [HttpPort]));
          AddStatus(Format('Fragments: http://127.0.0.1:%d/fragment/<sequence>.m4s',
                           [HttpPort]));
        end
      else
        AddStatus(Format('HTTP server failed to start on port %d.',
                         [HttpPort]));
  end;

  UpdateControls();
end;


procedure TfrmMain.btnStopClick(Sender: TObject);
var
  HR: HRESULT;

begin

  FHttpServer.DebugDropRequests := False;
  FHttpServer.Stop;

  ServiceRecordingArchive;
  StopRecordingFile;

  HR := FCapture.Stop;

  if FAILED(HR) then
    AddStatus(Format('Stop/finalize failed: 0x%.8x', [Cardinal(HR)]))
  else
    AddStatus('Capture stopped and file finalized.');

  UpdateControls;
end;

procedure TfrmMain.btnPauseHttpClick(Sender: TObject);
begin

  if Assigned(FHttpServer) and FHttpServer.Running then
    begin
      FHttpServer.DebugDropRequests := True;
      AddStatus('HTTP test pause enabled. Requests are dropped.');
      UpdateControls;
    end;
end;


procedure TfrmMain.btnResumeHttpClick(Sender: TObject);
begin

  if Assigned(FHttpServer) and
     FHttpServer.Running then
    begin
      FHttpServer.DebugDropRequests := False;
      AddStatus('HTTP test pause disabled. Requests are served again.');
      UpdateControls;
    end;
end;


function TfrmMain.HttpGetInit(out AData: TBytes): Boolean;
begin
  AData := nil;
  Result := Assigned(FCapture) and FCapture.GetInitSegment(AData);
end;


function TfrmMain.HttpGetFragment(const ASequence: UInt64;
                                      out AData: TBytes): Boolean;
begin

  AData := nil;

  Result := Assigned(FCapture) and
            FCapture.GetFragment(ASequence,
                                 AData);
end;


function TfrmMain.HttpGetStatus(out AText: AnsiString): Boolean;
var
  InitBytes: Integer;
  FragmentBytes: Integer;
  FragmentCount: UInt64;
  TotalBytes: UInt64;
  FirstSequence: UInt64;
  LastSequence: UInt64;
  WindowCount: Integer;

begin

  AText := 'MfSimpleWebCamStreamer';

  FirstSequence := 0;
  LastSequence := 0;
  WindowCount := 0;

  Result := Assigned(FCapture);

  if Result then
    FCapture.GetFragmentWindow(FirstSequence,
                               LastSequence,
                               WindowCount);

  if Result and
     FCapture.GetFmp4Diagnostics(InitBytes,
                                 FragmentBytes,
                                 FragmentCount,
                                 TotalBytes) then
    AText := AnsiString(
      Format('generation=%d'#13#10 +
             'state=%d'#13#10 +
             'videoSamples=%d'#13#10 +
             'audioSamples=%d'#13#10 +
             'initBytes=%d'#13#10 +
             'lastFragmentBytes=%d'#13#10 +
             'fragmentCount=%d'#13#10 +
             'first=%d'#13#10 +
             'last=%d'#13#10 +
             'windowCount=%d'#13#10 +
             'observedBytes=%d'#13#10,
             [FStreamGeneration,
              Ord(FCapture.State),
              FCapture.VideoSamples,
              FCapture.AudioSamples,
              InitBytes,
              FragmentBytes,
              FragmentCount,
              FirstSequence,
              LastSequence,
              WindowCount,
              TotalBytes]));
end;


procedure TfrmMain.tmrStatusTimer(Sender: TObject);
var
  InitBytes: Integer;
  FragmentBytes: Integer;
  FragmentCount: UInt64;
  TotalBytes: UInt64;

begin

  ServiceRecordingArchive;
  CheckRecordingRoll;

  if Assigned(FCapture) and
     (FCapture.State = csCapturing) then
  begin
    Caption := Format('MfSimpleWebCamStreamer - Video %d / Audio %d',
                      [FCapture.VideoSamples,
                       FCapture.AudioSamples]);

    if FCapture.GetFmp4Diagnostics(InitBytes,
                                   FragmentBytes,
                                   FragmentCount,
                                   TotalBytes) and
       (FragmentCount <> FLastFragmentCount) then
    begin
      FLastFragmentCount := FragmentCount;

      AddStatus(Format('fMP4: init=%d bytes, fragment=%d bytes, count=%d, observed=%d',
                       [InitBytes,
                        FragmentBytes,
                        FragmentCount,
                        TotalBytes]));
    end;
  end
  else
    Caption := 'MfSimpleWebCamStreamer';
end;

end.

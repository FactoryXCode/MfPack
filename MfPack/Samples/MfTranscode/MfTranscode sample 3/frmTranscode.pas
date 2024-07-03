// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfTranscode.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This is a modified translation of the Microsoft Transcoder example.
//              The original is a commandline app.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
// =============================================================================
// Source: Transcoding Example
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
unit frmTranscode;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ShellAPI,
  WinApi.WinError,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  System.DateUtils,
  System.Variants,
  System.Classes,
  System.Diagnostics,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.WmCodecDsp,
  {Project}
  Transcoder,
  dlgAudioFormats,
  Common,
  // REST
  VideoMftClass;

type
  TfrmTranscoder = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    mnuOpenSourceFile: TMenuItem;
    mnuExit: TMenuItem;
    lblChooseSourceFile: TLabel;
    lblChooseTargetfile: TLabel;
    stxtSourceFile: TEdit;
    stxtTargetFile: TEdit;
    Panel1: TPanel;
    butExecute: TButton;
    butStop: TButton;
    dlgSource: TOpenDialog;
    dlgTarget: TSaveDialog;
    mnuTargetfile: TMenuItem;
    ProgressBar: TProgressBar;
    lblProgress: TLabel;
    lblEstFinish: TLabel;
    N1: TMenuItem;
    butPlay: TButton;
    sbMsg: TStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    lblVideo: TLabel;
    lblAudio: TLabel;
    Label3: TLabel;
    lblContainer: TLabel;
    Bevel1: TBevel;
    Button1: TButton;
    procedure mnuOpenSourceFileClick(Sender: TObject);
    procedure mnuTargetfileClick(Sender: TObject);
    procedure butExecuteClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    // We could use the TLinkLabel, but this works as well.
    procedure lblChooseSourceFileMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure lblChooseTargetfileMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    //

  private
    { Private declarations }
    gVideoMediaFmt: TGuid;
    gAudioMediaFmt: TGuid;
    gContainerFormat: TGuid;
    sSourceFile: TFileName;   // Audio source file name
    sTargetFile: TFileName;   // Output file name

    FStopwatch: TStopWatch;
    bElapsedSet: Boolean;
    iSelectedAudioFormat: Integer;
    iSelectedContainerFmt: Integer;

    procedure Start();
    procedure Reset(const hr: HResult);
    procedure ResetFormats();
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;
    procedure WMStatusEvent(var Msg: TMessage); message WM_STATUSNOTIFY;

  public
    { Public declarations }
    FTranscoder: TTranscoder; // transcoder object
  end;

var
  frmTranscoder: TfrmTranscoder;

implementation

{$R *.dfm}

procedure TfrmTranscoder.mnuTargetfileClick(Sender: TObject);
begin

  dlgTarget.FileName := ExtractFileName(sSourceFile);
  dlgTarget.FilterIndex := 1;

  if dlgTarget.Execute then
    begin
      // Global var can be used to print details of the choosen formats (not implemented)
      iSelectedContainerFmt := dlgTarget.FilterIndex;

      // Set the outputformat
      case iSelectedContainerFmt of
      // Video /////////////////////////////////////////////////////////////
      1: begin
           // avi
           gVideoMediaFmt := MFVideoFormat_H264;
           gAudioMediaFmt := MFAudioFormat_AAC;
           gContainerFormat := MFTranscodeContainerType_AVI;
           // We keep it simpel; show user the target formats.
           lblContainer.Caption := 'Audio Video Interleave (AVI)';
           lblVideo.Caption := 'H.264 video encoder';
           lblAudio.Caption := 'Advanced Audio Coding (AAC)';
         end;
      2: begin
           // mp4 AAC
           gVideoMediaFmt := MFVideoFormat_HEVC;
           gAudioMediaFmt := MFAudioFormat_AAC;
           gContainerFormat := MFTranscodeContainerType_MPEG4;
           lblContainer.Caption := 'MPEG 4 (mp4)';
           lblVideo.Caption := 'H.265/HEVC video encoder';
           lblAudio.Caption := 'Advanced Audio Coding (AAC)';
         end;
      3: begin
           // MPEG-4 Video and Dolby AC-3 audio (Dolby Digital Audio Encoder)
           gVideoMediaFmt := MFVideoFormat_H264;
           gAudioMediaFmt := MFAudioFormat_Dolby_AC3;
           gContainerFormat := MFTranscodeContainerType_MPEG4;
           lblContainer.Caption := 'MPEG 4 (mp4)';
           lblVideo.Caption := 'H.264 video encoder';
           lblAudio.Caption := 'Dolby AC-3 audio';
         end;
      4: begin
           // wmv
           gVideoMediaFmt := MFVideoFormat_WMV3;
           gAudioMediaFmt := MFAudioFormat_WMAudioV9;
           gContainerFormat := MFTranscodeContainerType_ASF;
           lblContainer.Caption := 'Advanced Systems Format (ASF)';
           lblVideo.Caption := 'Windows Media Video (WMV)';
           lblAudio.Caption := 'Windows Media Audio (WMA)';
         end;

      // Audio /////////////////////////////////////////////////////////////////

      5: begin
           // wav
           gAudioMediaFmt := MFAudioFormat_PCM;
           gContainerFormat := MFTranscodeContainerType_WAVE;
           lblContainer.Caption := 'Waveform Audio File Format (wav)';
           lblVideo.Caption := 'none';
           lblAudio.Caption := 'Uncompressed PCM audio';
         end;
      6: begin
           // mp3
           // Note: The encoder supports only 16-bit integer PCM input.
           gAudioMediaFmt := MFAudioFormat_MP3;
           gContainerFormat := MFTranscodeContainerType_MP3;
           lblContainer.Caption := 'MPEG Audio Layer-3 (mp3)';
           lblVideo.Caption := 'none';
           lblAudio.Caption := 'MPEG Audio Layer-3';
         end;
      7: begin
           // ac3
           gAudioMediaFmt := MFAudioFormat_Dolby_AC3;
           gContainerFormat := MFTranscodeContainerType_AC3;
           lblContainer.Caption := 'Dolby Audio Codec 3';
           lblVideo.Caption := 'none';
           lblAudio.Caption := 'Dolby AC-3 audio';
         end;
      8: begin
           // flac
           gAudioMediaFmt := MFAudioFormat_FLAC;
           gContainerFormat := MFTranscodeContainerType_FLAC;
           lblContainer.Caption := 'Free Lossless Audio Codec (flac)';
           lblVideo.Caption := 'none';
           lblAudio.Caption := 'Free Lossless Audio Codec';
         end;
      9: begin
           // aac
           gAudioMediaFmt := MFAudioFormat_AAC;
           gContainerFormat := MFTranscodeContainerType_MPEG4;
           lblContainer.Caption := 'MPEG-4 Part 3 Audio';
           lblVideo.Caption := 'none';
           lblAudio.Caption := 'Advanced Audio Coding (AAC)';
         end;
      10: begin
            // m4a Note: Same as .aac
            gAudioMediaFmt := MFAudioFormat_AAC;
            gContainerFormat := MFTranscodeContainerType_MPEG4;
            lblContainer.Caption := 'MPEG 4 Part 3 Audio (m4a)';
            lblVideo.Caption := 'none';
            lblAudio.Caption := 'Advanced Audio Coding (AAC)';
          end;
      11: begin
            // wma
            gAudioMediaFmt := MFAudioFormat_WMAudioV9;
            gContainerFormat := MFTranscodeContainerType_ASF;
            lblContainer.Caption := 'Windows Media Audio Format (wma)';
            lblVideo.Caption := 'none';
            lblAudio.Caption := 'Windows Media Audio 9 (WMA)';
          end;
      end;  // case

      // If source is target, warn user.
      if (sSourceFile = dlgTarget.FileName) then
        begin
          MessageBox(0,
                     lpcwstr('The target file is the same as the source file.'),
                     lpcwstr('Error!'),
                     MB_ICONERROR);

          ResetFormats();
          sTargetFile := '';
          Exit;
        end;

      // Delete existing targetfile.
      if FileExists(sTargetFile) then
        DeleteFile(sTargetFile);

      // Get the audioformat parameters supported by the encoder.
      AudioFormatDlg.GetAudioFormats(gAudioMediaFmt);

      // Show the setAudio dialog to select the output encoding parameters of the choosen format.
      if (AudioFormatDlg.ShowModal = mrOk) then
        begin
          iSelectedAudioFormat := AudioFormatDlg.iSelectedFormat;
        end
      else
        begin
          // User canceled audio selection.
          sSourceFile := '';
          sTargetFile := '';
          ResetFormats();
          Exit;
        end;

      // Get the videoformat parameters supported by the encoder.



      stxtTargetFile.Text := dlgTarget.FileName;
      sTargetFile := PWideChar(dlgTarget.FileName);
      butExecute.Enabled := True;
      sbMsg.SimpleText := 'Please click Transcode to start.';
    end
  else
    begin
      dlgTarget.FileName := '';
      Exit;
    end;
end;


procedure TfrmTranscoder.butExecuteClick(Sender: TObject);
begin
  Start();
end;


// The "main" method
procedure TfrmTranscoder.Start();
var
  hr: HResult;

begin
  // Create the transcoder object
  FTranscoder := TTranscoder.Create(Handle);
  if not Assigned(FTranscoder) then
    Exit;

  // Create a stopwatch to calculate the estimated rendering time.
  FStopwatch := TStopWatch.Create();
  bElapsedSet := False;

  // Create a media source for the input file.
  hr := FTranscoder.OpenFile(sSourceFile);
  if SUCCEEDED(hr) then
    begin
      sbMsg.SimpleText := Format('Opened file: %s.', [sSourceFile]);
      // Configure the audio profile and add to topology.
      hr := FTranscoder.ConfigureAudioOutput(gAudioMediaFmt,
                                             iSelectedAudioFormat);
    end;

  if SUCCEEDED(hr) then
    begin
      // Configure the video profile and add to topology.
      if not IsEqualGUID(gVideoMediaFmt,
                         GUID_NULL) then
        hr := FTranscoder.ConfigureVideoOutput(gVideoMediaFmt);
    end;

  // Configure the containertype
  if SUCCEEDED(hr) then
    begin
      if not IsEqualGUID(gContainerFormat,
                         GUID_NULL) then
        hr := FTranscoder.ConfigureContainer(gContainerFormat);
    end;

  butStop.Enabled := True;
  butExecute.Enabled := False;

  // Start the stopwatch
  FStopwatch.Start();
  lblEstFinish.Caption := 'Calculating estimated time, one moment please...';

  // Transcode and generate the output file.
  if SUCCEEDED(hr) then
    hr := FTranscoder.EncodeToFile(sTargetFile);

  FStopwatch.Stop();
  Sleep(1000);

  if SUCCEEDED(hr) then
    sbMsg.SimpleText := Format('Output file created: %s', [ExtractFileName(sTargetFile)]);

  if FAILED(hr) then
    if hr = E_ABORT then
      sbMsg.SimpleText := 'Rendering aborted by user.'
    else
      sbMsg.SimpleText := Format('Could not create the output file (Error %d).', [hr]);

  Reset(hr);

end;


procedure TfrmTranscoder.Reset(const hr: HResult);
begin
  // When finished, release the engine.
  if Assigned(FTranscoder) then
    FreeAndNil(FTranscoder);
  // When the process encountered an error or abortion, delete the targetfile.
  if (hr <> S_OK) then
    DeleteFile(sTargetFile);

  ResetFormats();
  butPlay.Enabled := True;
end;


procedure TfrmTranscoder.ResetFormats();
begin
  gAudioMediaFmt := GUID_NULL;
  gVideoMediaFmt := GUID_NULL;
  gContainerFormat := GUID_NULL;
  iSelectedAudioFormat := -1;
  butPlay.Enabled := False;
  butExecute.Enabled := False;
  butStop.Enabled := False;
  mnuTargetfile.Enabled := False;
  mnuTargetfile.Enabled := False;
  lblChooseTargetfile.Enabled := false;
  stxtSourceFile.Text := '-';
  stxtTargetFile.Text := '-';
  lblProgress.Caption := 'Progress: -';
  lblEstFinish.Caption := 'Estimated finish at: -';
  lblContainer.Caption := '-';
  lblVideo.Caption := '-';
  lblAudio.Caption := '-';
  ProgressBar.Position := 0;
end;


procedure TfrmTranscoder.butStopClick(Sender: TObject);
begin
  {void} FTranscoder.Stop();
end;


//TEST
procedure TfrmTranscoder.Button1Click(Sender: TObject);
begin

  FVideoMft := TVideoMft.Create;
  // Get and set the desired MFT.
  // Supported MFT's
  // CLSID_CMPEG2EncoderDS
  // CLSID_CMSAC3Enc
  // CLSID_CMPEG2VideoEncoderMFT
  // CLSID_CMSH264EncoderMFT
  // CLSID_CMSH265EncoderMFT
  //
  //
  //
  //
  //
  //
  //
  //
  //

  //FVideoMft.GetVideoMftFormats(CLSID_CMSH264EncoderMFT,
  //                             MFT_ENUM_FLAG_ALL);
  if (AudioFormatDlg.ShowModal = mrOk) then
        begin
          iSelectedAudioFormat := AudioFormatDlg.iSelectedFormat;
        end;
end;


procedure TfrmTranscoder.butPlayClick(Sender: TObject);
begin
  // Play the selected sourcefile
  if not FileExists(sTargetFile) and FileExists(sSourceFile) then
    begin
      if FileExists(stxtSourceFile.Text) then
        begin
          ShellExecute(Handle,
                       'open',
                       PWideChar(sSourceFile),
                       nil,
                       nil,
                       SW_SHOWNORMAL);
        end;
    end
  else  // Play the targetfile
    if FileExists(sTargetFile) then
      begin
        ShellExecute(Handle,
                     'open',
                     PWideChar(sTargetFile),
                     nil,
                     nil,
                     SW_SHOWNORMAL);
      end
end;


procedure TfrmTranscoder.mnuExitClick(Sender: TObject);
begin
  Close();
end;


procedure TfrmTranscoder.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  // Terminate a running Transcode process.
  if Assigned(FTranscoder) then
      FTranscoder.Stop();
  sSourceFile := '';   // Audio source file name
  sTargetFile := '';
  FreeAndNil(FVideoMft);
  MFShutdown();
  CoUninitialize();
  CanClose := True;
end;


procedure TfrmTranscoder.FormCreate(Sender: TObject);
begin
  ResetFormats();

  // Set the fileextensionfilters for the dialogs.
  dlgTarget.Filter := VIDEO_FILE_FILTER + '|' +
                      AUDIO_FILE_FILTER;

  dlgSource.Filter := ALL_FILE_FILTER + '|' +
                      VIDEO_FILE_FILTER + '|' +
                      AUDIO_FILE_FILTER;

  //CoInitializeEx(nil,
  //               COINIT_APARTMENTTHREADED);

  // Startup Media Foundation
  if FAILED(MFStartup(MF_VERSION, 0)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                   MB_ICONSTOP);
        Application.Terminate;
     end;

  FVideoMft := TVideoMft.Create;
end;


procedure TfrmTranscoder.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Cursor := crDefault;
end;


procedure TfrmTranscoder.lblChooseSourceFileMouseMove(Sender: TObject;
                                                      Shift: TShiftState; X, Y: Integer);
begin
  Cursor := crHandPoint;
end;


procedure TfrmTranscoder.lblChooseTargetfileMouseMove(Sender: TObject;
                                                      Shift: TShiftState; X, Y: Integer);
begin
  Cursor := crHandPoint;
end;


procedure TfrmTranscoder.mnuOpenSourceFileClick(Sender: TObject);
begin
  ResetFormats();
  sSourceFile := '';
  dlgSource.FileName := sSourceFile;
  // Pick a file to extract the audio from
  butExecute.Enabled := False;
  butStop.Enabled := False;

  if dlgSource.execute then
    begin
      if FileExists(dlgSource.Filename) then
        begin
          sSourceFile := dlgSource.Filename;
          stxtSourceFile.Text := dlgSource.Filename;
          mnuTargetfile.Enabled := True;
          lblChooseTargetfile.Enabled := True;
          sbMsg.SimpleText := 'Please select a sourcefile.';
          butPlay.Enabled := True;
        end;
    end;
end;


procedure TfrmTranscoder.WMProgressEvent(var Msg: TMessage);
var
  CurrPosition: Integer;

begin
  // WParam 1 is a progress msg event
  if (Msg.WParam = 1) then
    begin
      // Progress percentage calculation: (100 * position) / totallength; Where progressbar.Max = 100 and Min = 0
      CurrPosition := Round((100 * FTranscoder.Position) / FTranscoder.Duration);

      // We wait until we reached 1st 1% to calculate the estimated rendering end time
      if (CurrPosition = 1) and (bElapsedSet = False) then  // calculate 1st 1%
        begin
          bElapsedSet := True;
          FStopwatch.Stop;
          lblEstFinish.Caption := Format('Estimated finish at: %s', [MSecToStr((FStopwatch.ElapsedMilliseconds * 100) + MillisecondOfTheDay(Now), False)]);
        end;

      ProgressBar.Position := CurrPosition;
      lblProgress.Caption := Format('Processed: %d%%', [CurrPosition]);
      HandleThreadMessages(Handle);
    end;
end;


procedure TfrmTranscoder.WMStatusEvent(var Msg: TMessage);
begin
  if (Msg.WParam = 0) then
    begin
      sbMsg.SimpleText := LPCWSTR(Msg.LParam);
      HandleThreadMessages(Handle);
    end;
end;

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  frmAudioClipEx.pas
// Kind: Pascal Unit
// Release date: 21-12-2019
// Language: ENU
//
// Revision Version: 4.1.0
//
// Description:
//   This application demonstrates using the Media Foundation
//   source reader to extract decoded audio from an audio/video file.
//
//   The application reads audio data from an input file and writes
//   uncompressed PCM audio to a WAVE file.
//
//   The input file must be a media format supported by Media Foundation,
//   and must have  an audio stream. The audio stream can be an encoded
//   format, such as Windows Media Audio.
//   Note: The original application was a console app. running in synchrone mode.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
// 07/11/2025 Tony                Addded support for WMA and FLAC.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: >= MfPackX400
// Known Issues: -
//
// Compiler version: 28 up to 36
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: Parts of the AudioClip sample
//         https://docs.microsoft.com/en-us/windows/win32/medfound/tutorial--decoding-audio
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit frmAudioClipEx;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  AudioClipEngine;

type
  TAudioClipExfrm = class(TForm)
    butStart: TButton;
    butCancel: TButton;
    pbProgress: TProgressBar;
    lblStatus: TLabel;
    lblGetSourceFile: TLabel;
    lblSourceFile: TLabel;
    lblTargetFile: TLabel;
    lblSetTartgetFile: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    lblTime: TLabel;
    tbPriority: TTrackBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    
    procedure butStartClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure lblGetSourceFileClick(Sender: TObject);
    procedure lblSetTartgetFileClick(Sender: TObject);
    procedure tbPriorityChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);

  private
    FWorker: TThread;
    procedure ExtractionDone(var Message: TMessage); message WM_APP + 901;
  private
    FEngine: TAudioClipClass;
    FCancelEvent: TEvent;
    FThrottle: Integer;
    FDuration: LONGLONG;

    procedure UpdateUIBusy(ABusy: Boolean);

    procedure WorkerComplete(Sender: TObject;
                             Success: Boolean;
                             HResultCode: HResult);
    procedure Reset();

  end;

var
  AudioClipExfrm: TAudioClipExfrm;


implementation
uses WinApi.ComBaseApi, WinApi.ActiveX.ObjBase, System.Math;
type
 TClipWorker = class(TThread)
 private
  FEngine: TAudioClipClass;
  FCancel: THandle;
  FWindow: HWND;
 protected
  procedure Execute; override;
 public
  Status: HResult;
  constructor Create(Engine: TAudioClipClass;Cancel: THandle;Window: HWND);
 end;
constructor TClipWorker.Create(Engine: TAudioClipClass;Cancel: THandle;Window: HWND);
begin inherited Create(True);FEngine:=Engine;FCancel:=Cancel;FWindow:=Window;FreeOnTerminate:=False;end;
procedure TClipWorker.Execute;
var Mask: TFPUExceptionMask; HR: HResult;
begin
 Mask:=GetExceptionMask;SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 HR:=CoInitializeEx(nil,COINIT_MULTITHREADED);Status:=HR;
 try
  if SUCCEEDED(HR) then begin
   try Status:=FEngine.ExtractSoundClip(FCancel,nil);except Status:=E_UNEXPECTED;end;
  end;
 finally
  if SUCCEEDED(HR) then CoUninitialize;
  ClearExceptions(False);SetExceptionMask(Mask);
 end;
 PostMessage(FWindow,WM_APP+901,0,0);
end;
procedure TAudioClipExfrm.ExtractionDone(var Message: TMessage);
var HR: HResult;
begin
 if FWorker=nil then Exit;
 FWorker.WaitFor;HR:=TClipWorker(FWorker).Status;
 FreeAndNil(FWorker);FreeAndNil(FEngine);
 if SUCCEEDED(HR) then lblStatus.Caption:='Extraction complete!'
 else lblStatus.Caption:=Format('Extraction ended (0x%x)',[HR]);
 UpdateUIBusy(False);
end;

{$R *.dfm}



procedure TAudioClipExfrm.FormCloseQuery(Sender: TObject;var CanClose: Boolean);
begin
 if Assigned(FCancelEvent) then FCancelEvent.SetEvent;
 CanClose:=True;
end;


procedure TAudioClipExfrm.FormCreate(Sender: TObject);
begin

  FCancelEvent := TEvent.Create(nil,
                                True,
                                False,
                                '');
  Reset();
end;


procedure TAudioClipExfrm.FormDestroy(Sender: TObject);
begin
 if Assigned(FCancelEvent) then FCancelEvent.SetEvent;
 if FWorker<>nil then begin FWorker.WaitFor;FreeAndNil(FWorker);end;
 FreeAndNil(FEngine);FreeAndNil(FCancelEvent);
end;


procedure TAudioClipExfrm.lblGetSourceFileClick(Sender: TObject);
var
  dlg: TOpenDialog;

begin

  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'Video/Audio files|*.mp4;*.avi;*.mkv;*.mov;*.mp3;*.wav;*.wma;*.flac;|All files|*.*';
    if dlg.Execute then
      lblSourceFile.Caption := dlg.FileName;
  finally
    dlg.Free;
  end;
end;


procedure TAudioClipExfrm.lblSetTartgetFileClick(Sender: TObject);
var
  dlg: TSaveDialog;

begin

  dlg := TSaveDialog.Create(Self);
  try
    dlg.Filter := 'WAV files|*.wav';
    if dlg.Execute then
      lblTargetFile.Caption := dlg.FileName;

    if (dlg.FileName = '') then
      lblTargetFile.Caption := ChangeFileExt(lblSourceFile.Caption, '.wav');

  finally
    dlg.Free;
  end;
end;


procedure TAudioClipExfrm.tbPriorityChange(Sender: TObject);
begin
  FThrottle := tbPriority.Position;
  if Assigned(FEngine) then
    FEngine.SamplingPriority := FThrottle;
end;


procedure TAudioClipExfrm.Timer1Timer(Sender: TObject);
begin
  if Assigned(FEngine) then
    begin
      pbProgress.Position := FEngine.ProgressPercent;
      lblStatus.Caption := Format('Processing... %d%% (%d KB)', [FEngine.ProgressPercent, FEngine.ProgressBytes div 1024]);
    end;
end;


procedure TAudioClipExfrm.UpdateUIBusy(ABusy: Boolean);
begin
  butStart.Enabled := not ABusy;
  butCancel.Enabled := ABusy;
  lblGetSourceFile.Enabled := not ABusy;
  lblSetTartgetFile.Enabled := not ABusy;
end;


procedure TAudioClipExfrm.butStartClick(Sender: TObject);
begin
 if FWorker<>nil then Exit;
 if (Trim(lblSourceFile.Caption)='') or (Trim(lblTargetFile.Caption)='') then Exit;
 FCancelEvent.ResetEvent;FreeAndNil(FEngine);
 FEngine:=TAudioClipClass.Create;
 FEngine.SourceFile:=lblSourceFile.Caption;FEngine.OutputFile:=lblTargetFile.Caption;FEngine.SamplingPriority:=FThrottle;
 pbProgress.Position:=0;lblStatus.Caption:='Extracting...';UpdateUIBusy(True);
 FWorker:=TClipWorker.Create(FEngine,FCancelEvent.Handle,Handle);
 FWorker.Start;
end;


procedure TAudioClipExfrm.butCancelClick(Sender: TObject);
begin
  if Assigned(FCancelEvent) then
    begin
      FCancelEvent.SetEvent;
      lblStatus.Caption := 'Cancelling...';
    end;
end;


procedure TAudioClipExfrm.WorkerComplete(Sender: TObject;Success: Boolean;HResultCode: HResult);
begin
 // The owned worker posts completion only after extraction has fully returned.
end;


procedure TAudioClipExfrm.Reset();
begin
  lblTime.Caption := '00:00:00';
  lblStatus.Caption := 'Ready.';
  pbProgress.Position := 0;
  UpdateUIBusy(False);
end;

end.

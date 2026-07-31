// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastTranscode.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: The adapter point for the existing subtitle burn-in/export
//              pipeline and local preview.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Parts of CPlayer Examples
//
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
unit MfCastTranscode;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  {Project}
  MfSubtitleCompositorX2,
  MfSubtitleFramePumpX2,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type
  TMfCastTranscodePipeline = class(TInterfacedObject,
                                   IMfCastTranscodePipeline)
  private
    FSettings: TMfCastEncodingSettings;
    FLogger: IMfCastLogger;
    FState: TMfCastState;
    FPublisher: IMfCastSegmentPublisher;
    FPreviewSink: IMfCastPreviewSink;
    FRequest: TMfCastTranscodeRequest;
    FByteStream: IMFByteStream;
    FWorker: TThread;
    FWorkerResult: HRESULT;

    function PrepareMediaFoundationPipeline: HRESULT;
    function StartWorkers: HRESULT;
    function StopWorkers: HRESULT;
    procedure SetState(const AState: TMfCastState);

  public

    constructor Create();
    destructor Destroy(); override;

    function Configure(const ASettings: TMfCastEncodingSettings): HRESULT;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(const ARequest: TMfCastTranscodeRequest;
                   const APublisher: IMfCastSegmentPublisher;
                   const APreviewSink: IMfCastPreviewSink): HRESULT;
    function Pause(): HRESULT;
    function Resume(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function GetState(): TMfCastState;
  end;

implementation


type
  TMfCastTranscodeWorker = class(TThread)
  private
    FOwner: TMfCastTranscodePipeline;
    FPump: TMfSubtitleFramePump;
    procedure PumpProgress(Sender: TObject;
                           FramesWritten: Int64;
                           SampleTime: MFTIME;
                           var Cancel: Boolean);
  protected
    procedure Execute(); override;
  public
    constructor Create(AOwner: TMfCastTranscodePipeline);
    procedure CancelTranscode();
    procedure PauseTranscode();
    procedure ResumeTranscode();
  end;


constructor TMfCastTranscodeWorker.Create(AOwner: TMfCastTranscodePipeline);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Priority := tpLower;
  FOwner := AOwner;
  FPump := nil;
end;


procedure TMfCastTranscodeWorker.CancelTranscode();
begin
  Terminate();
  if Assigned(FPump) then
    FPump.Cancel();
end;


procedure TMfCastTranscodeWorker.PauseTranscode();
begin
  if Assigned(FPump) then
    FPump.Pause();
end;


procedure TMfCastTranscodeWorker.ResumeTranscode();
begin
  if Assigned(FPump) then
    FPump.Resume();
end;


procedure TMfCastTranscodeWorker.PumpProgress(Sender: TObject;
                                              FramesWritten: Int64;
                                              SampleTime: MFTIME;
                                              var Cancel: Boolean);
begin
  Cancel := Terminated;
end;


procedure TMfCastTranscodeWorker.Execute();
var
  HrCom: HRESULT;
  ComInitialized: Boolean;
  Compositor: TMfSubtitleCompositor;
  SubtitleSourceName: WideString;
  Bitrate: UINT32;
begin
  if not Assigned(FOwner) then
    Exit;

  ComInitialized := False;
  Compositor := nil;
  FPump := nil;
  FOwner.FWorkerResult := E_FAIL;

  HrCom := CoInitializeEx(nil,
                          COINIT_MULTITHREADED);
  if SUCCEEDED(HrCom) then
    ComInitialized := True
  else
    if HrCom <> RPC_E_CHANGED_MODE then
      begin
        FOwner.FWorkerResult := HrCom;
        FOwner.SetState(csError);
        Exit;
      end;

  try
    try
      OutputDebugString(PChar('MfCast Transcode: worker started'));
      Compositor := TMfSubtitleCompositor.Create();
      FOwner.FWorkerResult := S_OK;
      if FOwner.FRequest.SubtitleMode = csmBurnIntoVideo then
        begin
          if FOwner.FRequest.SubtitleAspectRatio > 0.0 then
            Compositor.SubtitleAspectRatio := FOwner.FRequest.SubtitleAspectRatio;
          SubtitleSourceName := FOwner.FRequest.SubtitleSourceName;
          if SubtitleSourceName = '' then
            SubtitleSourceName := FOwner.FRequest.SourceName;
          FOwner.FWorkerResult := Compositor.OpenTimedTextFile(
                                   SubtitleSourceName,
                                   FOwner.FRequest.SubtitleLanguage);
          if FOwner.FWorkerResult <> S_OK then
            begin
              if FOwner.FWorkerResult = S_FALSE then
                FOwner.FWorkerResult := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
              Exit;
            end;
        end;

      FPump := TMfSubtitleFramePump.Create(Compositor);
      FPump.OnProgress := PumpProgress;

      Bitrate := FOwner.FRequest.Encoding.VideoBitrate;
      if Bitrate = 0 then
        Bitrate := 4000000;

      FOwner.FWorkerResult := FPump.BurnSubtitlesToFile(FOwner.FRequest.SourceName,
                                                        'mfcast.mp4',
                                                        Bitrate,
                                                        FOwner.FByteStream,
                                                        FOwner.FRequest.Encoding.OutputMode = comFragmentedMp4);
    except
      on E: Exception do
        begin
          OutputDebugString(PChar('MfCast Transcode exception: ' + E.Message));
          FOwner.FWorkerResult := E_FAIL;
        end;
    end;
  finally
    FreeAndNil(FPump);
    FreeAndNil(Compositor);
    if Assigned(FOwner.FPublisher) then
      begin
        if SUCCEEDED(FOwner.FWorkerResult) then
          FOwner.FPublisher.CompletePresentation()
        else
          FOwner.FPublisher.AbortPresentation(FOwner.FWorkerResult);
      end;
    if SUCCEEDED(FOwner.FWorkerResult) then
      FOwner.SetState(csStopped)
    else
      FOwner.SetState(csError);
    if ComInitialized then
      CoUninitialize();
    OutputDebugString(PChar(Format('MfCast Transcode: worker done hr=%.8x',
                                   [DWORD(FOwner.FWorkerResult)])));
  end;
end;


constructor TMfCastTranscodePipeline.Create();
begin

  inherited Create;
  FState := csIdle;
  FWorker := nil;
  FWorkerResult := S_OK;
end;


destructor TMfCastTranscodePipeline.Destroy();
begin

  Stop();
  FByteStream := nil;
  FPublisher := nil;
  FPreviewSink := nil;

  inherited Destroy;
end;


function TMfCastTranscodePipeline.Configure(const ASettings: TMfCastEncodingSettings): HRESULT;
begin

  if not (FState in [csIdle, csStopped]) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  FSettings := ASettings;
  Result := S_OK;
end;


procedure TMfCastTranscodePipeline.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastTranscodePipeline.Start(const ARequest: TMfCastTranscodeRequest;
                                        const APublisher: IMfCastSegmentPublisher;
                                        const APreviewSink: IMfCastPreviewSink): HRESULT;
begin

  if not Assigned(APublisher) then
    begin

      Result := E_POINTER;
      Exit;
    end;

  if not (FState in [csIdle, csStopped]) then
    begin

      Result := E_UNEXPECTED;
      Exit;
    end;

  if Assigned(FWorker) then
    begin

      Result := StopWorkers();
      if FAILED(Result) then
        Exit;
    end;

  FByteStream := nil;
  FPublisher := nil;
  FPreviewSink := nil;

  FRequest := ARequest;
  FPublisher := APublisher;
  FPreviewSink := APreviewSink;
  FByteStream := nil;
  FWorkerResult := S_OK;
  SetState(csPreparingMedia);

  Result := PrepareMediaFoundationPipeline;
  if FAILED(Result) then
    begin

      SetState(csError);
      Exit;
    end;

  Result := StartWorkers;
  if FAILED(Result) then
    begin

      SetState(csError);
      Exit;
    end;

  SetState(csBuffering);
end;


function TMfCastTranscodePipeline.Pause(): HRESULT;
begin

  if Assigned(FWorker) then
    TMfCastTranscodeWorker(FWorker).PauseTranscode();

  if FState in [csPreparingMedia, csBuffering, csPlaying] then
    SetState(csPaused);
  Result := S_OK;
end;


function TMfCastTranscodePipeline.Resume(): HRESULT;
begin

  if Assigned(FWorker) then
    TMfCastTranscodeWorker(FWorker).ResumeTranscode();

  if FState = csPaused then
    SetState(csBuffering);
  Result := S_OK;
end;


function TMfCastTranscodePipeline.Stop(): HRESULT;
begin

  if (FState = csIdle) and
     not Assigned(FWorker) and
     not Assigned(FByteStream) then
    begin

      FPublisher := nil;
      FPreviewSink := nil;
      FRequest.Reset();
      Result := S_OK;
      Exit;
    end;

  SetState(csStopping);
  Result := StopWorkers();

  FByteStream := nil;
  FPublisher := nil;
  FPreviewSink := nil;
  FRequest.Reset();

  if SUCCEEDED(Result) then
    SetState(csStopped)
  else
    SetState(csError);
end;


function TMfCastTranscodePipeline.Seek(const APosition100ns: Int64): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TMfCastTranscodePipeline.GetState(): TMfCastState;
begin

  Result := FState;
end;


function TMfCastTranscodePipeline.PrepareMediaFoundationPipeline(): HRESULT;
begin

  Result := S_OK;
end;


function TMfCastTranscodePipeline.StartWorkers(): HRESULT;
begin
  if not Assigned(FPublisher) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FPublisher.GetByteStream(FByteStream);
  if FAILED(Result) then
    Exit;

  FWorker := TMfCastTranscodeWorker.Create(Self);
  TMfCastTranscodeWorker(FWorker).Start();
  Result := S_OK;
end;


function TMfCastTranscodePipeline.StopWorkers(): HRESULT;
begin
  if Assigned(FWorker) then
    begin
      TMfCastTranscodeWorker(FWorker).CancelTranscode();
      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;

  if Assigned(FPublisher) and (FState <> csError) then
    FPublisher.AbortPresentation(E_ABORT);
  FByteStream := nil;
  Result := S_OK;
end;


procedure TMfCastTranscodePipeline.SetState(const AState: TMfCastState);
begin

  FState := AState;
end;

end.

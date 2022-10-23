// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  FileCapture.Asynchronous.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 3.1.3
//
// Description:
//   This unit shows how to get a videoframe from a file source in A-sync mode.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX313/Samples/MFFrameSample
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit FileCapture.Asynchronous;

interface

uses
  {Winapi}
  WinAPI.Messages,
  WinAPI.Windows,
  {System}
  System.TimeSpan,
  {MediaFoundationApi}
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.MediaFoundationApi.MfObjects,
  {Application}
  FileCapture,
  MessageHandler,
  Support;

const
  WM_FLUSH_COMPLETE = WM_USER + 1001;
  WM_MEDIA_FORMAT_CHANGED = WM_USER + 1002;
  WM_SAMPLE_FOUND = WM_USER + 1003;

type
  TPositionRequest = record
    bPending: Boolean;
    bRequestNextSample: Boolean;
    oPosition: TTimeSpan;

    procedure Reset;
    class function New(APosition: TTimeSpan;
                       ARequestSample: Boolean): TPositionRequest; static;
  end;

  PSampleReply = ^TSampleReply;
  TSampleReply = record
    oSampleTimeStamp: TTimeSpan;
    oSample: IMFSample;
  end;

  TFileCaptureAsync = class(TFileCapture, IMFSourceReaderCallback)
  private
    FMessageHandler: TMessageHandler;
    FPositionRequest: TPositionRequest;
    FCritSec: TMFCritSec;
    FFindingSample: Boolean;
    oSampleReply: TSampleReply;

    procedure HandleMessages(var AMessage: TMessage;
                             var AHandled: Boolean);
    procedure NotifyMediaFormatChanged;
    procedure HandleSampleFoundMessage(var AMessage: TMessage);

    function ReadNextSample: Boolean;

    {$region 'IMFSourceReaderCallback methods'}
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWord;
                          dwStreamFlags: DWord;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HRESULT; stdcall;

    function OnFlush(dwStreamIndex: DWord): HRESULT; stdcall;
    function OnEvent(dwStreamIndex: DWord;
                     pEvent: IMFMediaEvent): HRESULT; stdcall;
    procedure ProcessPendingRequest;
    {$endregion}

  protected

    procedure ProcessSample(ASample: IMFSample;
                            ATimeStamp: TTimeSpan); override;
    procedure ConfigureSourceReader(const AAttributes: IMFAttributes); override;
    procedure HandleMediaFormatChanged; override;
    procedure ResetVariables; override;
    procedure HandleFlushComplete; override;
    procedure Flush; override;

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure RequestFrame(APosition: TTimeSpan); override;

    property FindingSample: Boolean read FFindingSample;
  end;

implementation

uses
  {WinApi}
  WinAPI.MediaFoundationApi.MfUtils,
  {System}
  System.SysUtils;

{ TFileCaptureAsync }

constructor TFileCaptureAsync.Create;
begin
  inherited;
  FCritSec := TMFCritSec.Create;

  FMessageHandler := TMessageHandler.Create;
  FMessageHandler.OnMessage := HandleMessages;

  FPositionRequest.Reset;
end;


destructor TFileCaptureAsync.Destroy;
begin
  SafeDelete(FCritSec);
  FMessageHandler.RemoveHandle;
  FreeAndNil(FMessageHandler);
  inherited;
end;


procedure TFileCaptureAsync.HandleFlushComplete;
begin
  inherited;
  FFindingSample := False;

  if SourceOpen and FPositionRequest.bPending then
    ProcessPendingRequest;
end;


procedure TFileCaptureAsync.ProcessPendingRequest;
begin
  RequestFrame(FPositionRequest.oPosition);
  FPositionRequest.bPending := False;
end;


procedure TFileCaptureAsync.HandleMediaFormatChanged;
begin
  inherited;
  ReadNextSample;
end;


procedure TFileCaptureAsync.HandleMessages(var AMessage: TMessage;
                                           var AHandled: Boolean);
begin
  if AMessage.Msg = WM_FLUSH_COMPLETE then
    begin
      AHandled := True;
      HandleFlushComplete;
    end
  else if AMessage.Msg = WM_MEDIA_FORMAT_CHANGED then
    begin
      AHandled := True;
      HandleMediaFormatChanged;
    end
  else if AMessage.Msg = WM_SAMPLE_FOUND then
    begin
      AHandled := True;
      HandleSampleFoundMessage(AMessage);
    end
  else
    AHandled := False;
end;


// This method will be triggered in A-Sync mode.
// hide message H2077, this is true in sync mode, not in a-sync mode
{$HINTS OFF}
procedure TFileCaptureAsync.HandleSampleFoundMessage(var AMessage: TMessage);
var
  oSampleReply: PSampleReply;

begin
  oSampleReply := PSampleReply(AMessage.LPARAM);

  ReturnSample(oSampleReply.oSample,
               oSampleReply.oSampleTimeStamp);
end;
{$HINTS ON}


procedure TFileCaptureAsync.Flush;
begin
  inherited;
  Log('Flush - Begin',
      ltInfo);
  SourceReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
end;


procedure TFileCaptureAsync.RequestFrame(APosition: TTimeSpan);
begin
  inherited;

  if FindingSample then
    begin
      // We need to flush before changing position if a sample request is in progress.
      FPositionRequest := TPositionRequest.New(APosition,
                                               True);

    if not AwaitingFlush then
      Flush;
    end
  else
    ReadNextSample;
end;


procedure TFileCaptureAsync.ResetVariables;
begin
  inherited;
  FFindingSample := False;
  FPositionRequest.Reset;
end;


function TFileCaptureAsync.ReadNextSample: Boolean;
var
  oResult: HRESULT;

begin
  Result := Assigned(SourceReader);

  if Result then
  begin
    FFindingSample := True;
    oResult := SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                       0,
                                       nil,
                                       nil,
                                       nil,
                                       nil);
    Result := SUCCEEDED(oResult);
    if not Result then
      HandleSampleReadError(oResult);
  end;
end;


procedure TFileCaptureAsync.ConfigureSourceReader(const AAttributes: IMFAttributes);
var
  oResult: HRESULT;

begin
  oResult := AAttributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                                    Self);
  if not SUCCEEDED(oResult) then
    Log('Failed to configure source reader callback',
        ltError);
end;


function TFileCaptureAsync.OnEvent(dwStreamIndex: DWord;
                                   pEvent: IMFMediaEvent): HRESULT;
begin
  // Note: This will be called in a worker thread.
  Result := S_OK;
end;


function TFileCaptureAsync.OnFlush(dwStreamIndex: DWord): HRESULT;
begin
  // Note: This will be called in a worker thread.
  Result := S_OK;

  PostMessage(FMessageHandler.Handle,
              WM_FLUSH_COMPLETE,
              0,
              0);
  HandleThreadMessages(GetCurrentThread());
end;


procedure TFileCaptureAsync.NotifyMediaFormatChanged;
begin
  // Note: This will be called in a worker thread.
  PostMessage(FMessageHandler.Handle,
              WM_MEDIA_FORMAT_CHANGED,
              0,
              0);
  HandleThreadMessages(GetCurrentThread());
end;


procedure TFileCaptureAsync.ProcessSample(ASample: IMFSample;
                                          ATimeStamp: TTimeSpan);
begin
  // Note: This will be called in a worker thread.

  // _AddRef will be called, so the sample will not be released until the message is handled.

  oSampleReply.oSample := ASample;
  oSampleReply.oSampleTimeStamp := ATimeStamp;

  if not PostMessage(FMessageHandler.Handle,
                     WM_SAMPLE_FOUND,
                     0,
                     LPARAM(@oSampleReply)) then
    begin
      SafeRelease(oSampleReply.oSample);
    end;

  HandleThreadMessages(GetCurrentThread());
end;


function TFileCaptureAsync.OnReadSample(hrStatus: HRESULT;
                                        dwStreamIndex: DWord;
                                        dwStreamFlags: DWord;
                                        llTimestamp: LONGLONG;
                                        pSample: IMFSample): HRESULT;
var
  bEndOfStream: Boolean;
  dSampleTimeStamp: LONGLONG;
  tsSampleTime: TTimeSpan;
  bReachedMaxFrames: Boolean;

begin
  // Note: This will be called in a worker thread. Be careful of accessing anything outside of this method.
  Result := S_OK;

try
  FCritSec.Lock;
try
  if SUCCEEDED(hrStatus) then
    begin

      bEndOfStream := (dwStreamFlags = MF_SOURCE_READERF_ENDOFSTREAM);

      if (dwStreamFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
        NotifyMediaFormatChanged
      else if Assigned(pSample) and SUCCEEDED(pSample.GetSampleTime(dSampleTimeStamp)) then
        begin
          bReachedMaxFrames := (FramesSkipped >= MaxFramesToSkip);

          tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
          if bEndOfStream or SampleWithinTolerance(RequestedTime,
                                                   tsSampleTime) or bReachedMaxFrames then
            begin
              if bReachedMaxFrames then
                Log(Format('Reached maximum frames to skip %d. Using last frame returned at: %s',
                           [FramesSkipped, TimeSpanToDisplay(tsSampleTime, True)]), ltWarning);

            ProcessSample(pSample,
                          tsSampleTime);
            FFindingSample := False;
            end
          else
            begin
              // Read the next sample
              if (FramesSkipped < MaxFramesToSkip) then
                begin
                  ReadNextSample;
                  HandleFrameSkipped;
                end;
            end;
        end;
    end;
finally
  FCritSec.Unlock;
end;
finally
  SafeRelease(pSample);
end;

end;


{ TPositionRequest }

class function TPositionRequest.New(APosition: TTimeSpan;
                                    ARequestSample: Boolean): TPositionRequest;
begin
  Result.oPosition := APosition;
  Result.bRequestNextSample := ARequestSample;
  Result.bPending := True;
end;


procedure TPositionRequest.Reset;
begin
  oPosition := TTimeSpan.Zero;
  bRequestNextSample := False;
  bPending := False;
end;

end.

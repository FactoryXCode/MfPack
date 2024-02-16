// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  CameraCapture.Asynchronous.pas
// Kind: Pascal Unit
// Release date: 29-03-2022
// Language: ENU
//
// Revision Version: 3.1.6
//
// Description:
//   This unit shows how to get a videoframe from a camera in A-sync mode.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX316/Samples/CameraFrameCapture
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
unit CameraCapture.Asynchronous;

interface

uses
  {Winapi}
  WinAPI.Messages,
  WinAPI.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  {MediaFoundationApi}
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  CameraCapture,
  MessageHandler,
  Support;

const
  WM_FLUSH_COMPLETE = WM_USER + 1001;
  WM_MEDIA_FORMAT_CHANGED = WM_USER + 1002;
  WM_SAMPLE_FOUND = WM_USER + 1003;
  WM_REQUEST_FLUSH = WM_USER + 1004;

type
  PSampleReply = ^TSampleReply;
  TSampleReply = record
    oSample: IMFSample;
  end;

  TCameraCaptureAsync = class(TCameraCapture, IMFSourceReaderCallback)
  private
    FMessageHandler: TMessageHandler;
    FFindingSample: Boolean;
    FSampleReply: TSampleReply;
    FMaxCalcStartTime: TDateTime;
    FSampleReadCount: Integer;
    FCancelBurst: Boolean;
    FBurstEnabled: Boolean;

    procedure HandleMessages(var AMessage: TMessage;
                             var AHandled: Boolean);
    procedure NotifyMediaFormatChanged();
    procedure HandleSampleFoundMessage(var AMessage: TMessage);

    {$region 'IMFSourceReaderCallback methods'}
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWord;
                          dwStreamFlags: DWord;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HRESULT; stdcall;

    function OnFlush(dwStreamIndex: DWord): HRESULT; stdcall;
    function OnEvent(dwStreamIndex: DWord;
                     pEvent: IMFMediaEvent): HRESULT; stdcall;

    function ReadNextSample(): Boolean;
    {$endregion}

  protected
    function ConfigureSourceReader(const AAttributes: IMFAttributes) : Boolean; override;
    procedure ProcessSample(ASample: IMFSample); override;
    procedure Flush(); override;
    procedure ResetVariables(); override;

  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure StartBurst();
    procedure StopBurst();

    procedure CalculateMaxFrameRate(AOnComplete : TOnCalculateComplete); override;

    procedure RequestFrame();
    property BurstEnabled: Boolean read FBurstEnabled;
  end;


implementation


constructor TCameraCaptureAsync.Create();
begin
  inherited;
  FMaxCalcStartTime := 0;
  FSampleReadCount := 0;
  FMessageHandler := TMessageHandler.Create();
  FMessageHandler.OnMessage := HandleMessages;
  FCancelBurst := False;
  FBurstEnabled := False;
end;


destructor TCameraCaptureAsync.Destroy();
begin
  StopBurst();
  FMessageHandler.RemoveHandle();
  FreeAndNil(FMessageHandler);
  inherited;
end;


procedure TCameraCaptureAsync.Flush;
begin
  inherited;
  Log('Flushing source', ltInfo);
  SourceReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
end;


function TCameraCaptureAsync.OnEvent(dwStreamIndex: DWord;
                                     pEvent: IMFMediaEvent): HRESULT;
begin
  // Note: This will be called in a worker thread.

  Result := S_OK;
end;


function TCameraCaptureAsync.OnFlush(dwStreamIndex: DWord): HRESULT;
begin
  // Note: This will be called in a worker thread.
  Result := S_OK;

  PostMessage(FMessageHandler.Handle,
              WM_FLUSH_COMPLETE,
              0,
              0);
  HandleThreadMessages(GetCurrentThread());
end;


function TCameraCaptureAsync.OnReadSample(hrStatus: HRESULT;
                                          dwStreamIndex: DWord;
                                          dwStreamFlags: DWord;
                                          llTimestamp: LONGLONG;
                                          pSample: IMFSample): HRESULT;
var
  bEndOfStream: Boolean;

begin
  // Note: This will be called in a worker thread. Be careful of accessing anything outside of this method.
  Result := hrStatus;
  try
    if SUCCEEDED(Result) then
      begin
        CritSec.Lock();

        try
          bEndOfStream := (dwStreamFlags = MF_SOURCE_READERF_ENDOFSTREAM);

          if (dwStreamFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
            NotifyMediaFormatChanged
          else
            if not Assigned(pSample) and (FramesSkipped < MaxFramesToSkip) and not bEndOfStream then
              begin
                ReadNextSample();
                HandleFrameSkipped();
              end
          else
            if Assigned(pSample) then
              begin
                FCalculatingMax := SecondsBetween(Now,
                                                  FMaxCalcStartTime) <= 10;
                if FCalculatingMax then
                  begin
                    // Exclude the time taken to read the first sample
                    if (FSampleReadCount = 0) then
                      FMaxCalcStartTime := Now();

                    inc(FSampleReadCount);
                    ReadNextSample();
                  end
                else
                  begin
                    if BurstEnabled then
                      ReadNextSample();

                    ProcessSample(pSample);
                  end;

                FFindingSample := False;
              end;
        finally
          CritSec.Unlock();
        end;

      end;

  finally
    SafeRelease(pSample);
  end;
end;


procedure TCameraCaptureAsync.ProcessSample(ASample: IMFSample);
begin
  // Note: This will be called in a worker thread.
  // _AddRef will be called, so the sample will not be released until the message is handled.
  FSampleReply.oSample := ASample;

  if not PostMessage(FMessageHandler.Handle,
                     WM_SAMPLE_FOUND,
                     0,
                     LPARAM(@FSampleReply)) then
    begin
      SafeRelease(FSampleReply.oSample);
    end;

  HandleThreadMessages(GetCurrentThread());
end;


procedure TCameraCaptureAsync.CalculateMaxFrameRate(AOnComplete: TOnCalculateComplete);
var
  iDurationSec : Integer;
  iSampleReadCount : Integer;

begin
  inherited;
  FMaxCalcStartTime := Now();

  FSampleReadCount := 0;
  ReadNextSample();

  while FCalculatingMax do
  begin
    HandleThreadMessages(GetCurrentThread());
    Sleep(1000);
  end;

  iSampleReadCount := FSampleReadCount;
  iDurationSec := SecondsBetween(FMaxCalcStartTime,
                                 Now);

  Flush();

  if Assigned(OnCalculateComplete) then
    OnCalculateComplete(Round(iSampleReadCount / iDurationSec));
end;


function TCameraCaptureAsync.ConfigureSourceReader(const AAttributes: IMFAttributes): Boolean;
begin
  Result := inherited;

  if Result then
    Result := SUCCEEDED(AAttributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                                               Self));
  if not Result then
    Log('Failed to configure source reader callback',
        ltError);
end;


procedure TCameraCaptureAsync.StartBurst();
var
  oCurrentFormat : TVideoFormat;

begin
  StopBurst;
  if not FBurstEnabled then
    begin
      FBurstEnabled := True;
      FCancelBurst := False;

      GetCurrentFormat(oCurrentFormat);

      RequestFrame();
    end;
end;


procedure TCameraCaptureAsync.StopBurst();
begin
 if FBurstEnabled then
  begin
    FCancelBurst := True;
    FBurstEnabled := False;
  end;
end;


procedure TCameraCaptureAsync.HandleMessages(var AMessage: TMessage;
                                             var AHandled: Boolean);
begin
  if AMessage.Msg = WM_REQUEST_FLUSH then
    begin
      AHandled := True;
      Flush();
    end
  else if AMessage.Msg = WM_FLUSH_COMPLETE then
    begin
      AHandled := True;
      HandleFlushComplete();
    end
  else if AMessage.Msg = WM_MEDIA_FORMAT_CHANGED then
    begin
      AHandled := True;
      HandleMediaFormatChanged();
    end
  else if AMessage.Msg = WM_SAMPLE_FOUND then
    begin
      AHandled := True;
      HandleSampleFoundMessage(AMessage);
    end
  else
    AHandled := False;
end;


procedure TCameraCaptureAsync.HandleSampleFoundMessage(var AMessage: TMessage);
var
  oSampleReply: PSampleReply;
begin
  oSampleReply := PSampleReply(AMessage.LPARAM);

  if FCancelBurst then
  begin
    if Assigned(oSampleReply.oSample) then
    begin
      oSampleReply.oSample.RemoveAllBuffers;
      SafeRelease(oSampleReply.oSample);
    end;
  end
  else
    ReturnDataFromSample(oSampleReply.oSample)
end;


procedure TCameraCaptureAsync.NotifyMediaFormatChanged();
begin
  // Note: This will be called in a worker thread.
  PostMessage(FMessageHandler.Handle,
              WM_MEDIA_FORMAT_CHANGED,
              0,
              0);
  HandleThreadMessages(GetCurrentThread());
end;


procedure TCameraCaptureAsync.RequestFrame();
begin
  FCancelBurst := False;
  ResetFramesSkipped();
  ReadNextSample();
end;


function TCameraCaptureAsync.ReadNextSample(): Boolean;
var
  oResult: HRESULT;

begin

  Result := Assigned(SourceReader);

  if Result then
    begin
      StartTimer;
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


procedure TCameraCaptureAsync.ResetVariables();
begin
  inherited;
  MaxFramesToSkip := 1;
end;

end.

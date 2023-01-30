// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  FileCapture.Synchronous.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description:
//   This unit shows how to get a videoframe from a file source in sync mode.
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
// Related projects: MfPackX314/Samples/MFFrameSample
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
unit FileCapture.Synchronous;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfObjects,
  {System}
  System.TimeSpan,
  {Application}
  FileCapture;

type
  TFileCaptureSync = class(TFileCapture)
  protected
    procedure ProcessSample(ASample: IMFSample;
                            ATimeStamp: TTimeSpan); override;
    function ReadNextSample(out AFlags: DWord;
                            out ASample: IMFSample): Boolean;
  public
    procedure RequestFrame(APosition: TTimeSpan); override;
    procedure Flush; override;
  end;

implementation

uses
  {Winapi}
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Types,
  {VCL}
  VCL.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  Support;

{ TFileCaptureSync }

procedure TFileCaptureSync.RequestFrame(APosition: TTimeSpan);
var
  pSample: IMFSample;
  dSampleTimeStamp: LONGLONG;
  tsSampleTime: TTimeSpan;
  bEndOfStream: Boolean;
  bReachedMaxFrames: Boolean;
  bFound: Boolean;
  dwFlags: DWord;

begin
  inherited;
  dSampleTimeStamp := 0;

  bFound := False;
  dwFlags := 0;
  bEndOfStream := False;
  bReachedMaxFrames := False;

  while not bFound and not bEndOfStream and (FramesSkipped < MaxFramesToSkip) and ReadNextSample(dwFlags, pSample) do
  begin
    bEndOfStream := (dwFlags = MF_SOURCE_READERF_ENDOFSTREAM);

    if (dwFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
      begin
        // Type change. Get the new format.
        Log('Media Format has changed, getting new format',
            ltInfo);
        HandleMediaFormatChanged;
      end
    else if Assigned(pSample) and SUCCEEDED(pSample.GetSampleTime(dSampleTimeStamp)) then
      begin
        tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
        bFound := bEndOfStream or SampleWithinTolerance(APosition, tsSampleTime);
        if bFound then
          ProcessSample(pSample, tsSampleTime)
        else
          begin
            HandleFrameSkipped;
            Log(Format('Skipped frame. Not within requested accuracy. Sample time stamp: %s. Frames Skipped: %d',
                       [TimeSpanToDisplay(tsSampleTime, True), FramesSkipped]), ltDebug);
          end;
      end;

    bReachedMaxFrames := (FramesSkipped = MaxFramesToSkip);

    if not bReachedMaxFrames then
      SafeRelease(pSample);

  end;

  if not bFound then
    begin
      if bReachedMaxFrames and Assigned(pSample) then
        begin
          Log(Format('Reached maximum frames to skip %d. Using last frame returned at: %s',
                     [FramesSkipped, TimeSpanToDisplay(tsSampleTime, True)]), ltWarning);
          ProcessSample(pSample,
                        tsSampleTime);
        end
      else
        Log(Format('Frame not found. Frames Skipped: %d', [FramesSkipped]), ltWarning);
  end;
end;


function TFileCaptureSync.ReadNextSample(out AFlags: DWord;
                                         out ASample: IMFSample): Boolean;
var
  hr: HResult;

begin
try
  Result := True;

  if Assigned(SourceReader) then
    hr := SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                  0,
                                  Nil,
                                  @AFlags,
                                  Nil,
                                  @ASample)
  else
    hr := E_POINTER;

  // Handle normal exceptions.
  if FAILED(hr) then
    HandleSampleReadError(hr);
except
  // Do nothing, proceed with E_ABORT.
  Result := False;
end;
end;


procedure TFileCaptureSync.ProcessSample(ASample: IMFSample;
                                         ATimeStamp: TTimeSpan);
begin
  inherited;
  ReturnSample(ASample,
               ATimeStamp);
end;


procedure TFileCaptureSync.Flush;
var
  hr: HRESULT;

begin
  inherited;

  Log('Flush - Begin',
      ltInfo);

  hr := SourceReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
  if SUCCEEDED(hr) then
    Log('Flush - End',
        ltInfo)
  else
    Log('Failed to flush source',
        ltError);

  HandleFlushComplete;
end;

end.

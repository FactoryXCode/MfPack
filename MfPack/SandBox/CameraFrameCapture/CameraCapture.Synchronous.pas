// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  CameraCapture.Synchronous.pas
// Kind: Pascal Unit
// Release date: 18-03-2022
// Language: ENU
//
// Revision Version: 3.1.1
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
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX311/Samples/MFFrameSample
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
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
unit CameraCapture.Synchronous;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfObjects,
  {System}
  System.TimeSpan,
  System.DateUtils,
  {Application}
  CameraCapture;

type
  TCameraCaptureSync = class(TCameraCapture)
  protected
    procedure ProcessSample(ASample: IMFSample); override;
    function ReadNextSample(out AFlags: DWord;
                            out ASample: IMFSample): Boolean;
  public
    procedure RequestFrame; override;
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

{ TCameraCaptureSync }

procedure TCameraCaptureSync.RequestFrame;
var
  pSample: IMFSample;
  bEndOfStream: Boolean;
  bFound: Boolean;
  dwFlags: DWord;
begin
  inherited;
  bFound := False;
  dwFlags := 0;
  bEndOfStream := False;

  while (not bFound or BurstEnabled) and not bEndOfStream and ReadNextSample(dwFlags, pSample) do
  begin
    bEndOfStream := (dwFlags = MF_SOURCE_READERF_ENDOFSTREAM);
    bFound := Assigned(pSample);

    if (dwFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
      begin
        // Type change. Get the new format.
        Log('Media Format has changed, getting new format',
            ltInfo);
        HandleMediaFormatChanged;
      end
    else if bFound then
        ProcessSample(pSample);

    if bFound then
      SafeRelease(pSample);

    HandleThreadMessages(GetCurrentThread());
  end;
end;

function TCameraCaptureSync.ReadNextSample(out AFlags: DWord;
                                         out ASample: IMFSample): Boolean;
var
  hr: HResult;
begin
  StartTimer;
  try
    Result := Assigned(SourceReader);

    if Result then
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


procedure TCameraCaptureSync.ProcessSample(ASample: IMFSample);
begin
  ReturnSample(ASample);
end;


procedure TCameraCaptureSync.Flush;
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

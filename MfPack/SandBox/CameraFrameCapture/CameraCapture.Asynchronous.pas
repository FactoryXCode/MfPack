// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  CameraCapture.Asynchronous.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 3.1.1
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
unit CameraCapture.Asynchronous;

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
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  FileCapture.Asynchronous;

type
  TCameraCaptureAsync = class(TFileCaptureAsync)
  protected
    function SampleWithinTolerance(ARequestedTime: TTimeSpan;
                                   AActualTime: TTimeSpan): Boolean; override;
    procedure ResetVariables; override;
  public
    function OpenDeviceSource(const ADevice: IMFActivate): Boolean;

    procedure RequestFrame; reintroduce;
  end;

implementation

function TCameraCaptureAsync.OpenDeviceSource(const ADevice: IMFActivate): Boolean;
var
  pSource: IMFMediaSource;
  oAttributes: IMFAttributes;

begin
  CloseSource;

  Result := Assigned(ADevice);

  if Result then
  begin
     CritSec.Lock;
     try
        // Create the media source for the device.
        Result := SUCCEEDED(ADevice.ActivateObject(IID_IMFMediaSource,
                                         pSource));

        if Result then
        // Configure the source reader to perform video processing.
          Result := SUCCEEDED(MFCreateAttributes(oAttributes,
                                                 1));

        if Result then
          Result := SUCCEEDED(oAttributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                                                          1));
        if Result then
          Result := ConfigureSourceReader(oAttributes);

        if Result then
          Result := SUCCEEDED(MFCreateSourceReaderFromMediaSource(pSource,
                                                    oAttributes,
                                                    FSourceReader));
        if Result then
          Result := SelectVideoStream and GetVideoFormat(False);
     finally
       CritSec.Unlock;
     end;
  end;
end;

procedure TCameraCaptureAsync.RequestFrame;
begin
  ResetFramesSkipped;
  ReadNextSample;
end;

procedure TCameraCaptureAsync.ResetVariables;
begin
  inherited;
  MaxFramesToSkip := 1;
end;

function TCameraCaptureAsync.SampleWithinTolerance(ARequestedTime, AActualTime: TTimeSpan): Boolean;
begin
  // No sample tolerance - we want the first frame we find for live capture
  Result := True;
end;

end.

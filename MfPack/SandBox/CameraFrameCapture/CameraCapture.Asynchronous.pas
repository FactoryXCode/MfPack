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
  System.SysUtils,
  {MediaFoundationApi}
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  FileCapture.Asynchronous;

type
  TVideoFormat = record
    iFrameHeigth: Integer;
    iFrameWidth: Integer;
    iFramesPerSecond : Integer;
    oSubType : TGUID;
  end;
  TVideoFormats = TArray<TVideoFormat>;


  TCameraCaptureAsync = class(TFileCaptureAsync)
  private
    FVideoFormats : TVideoFormats;

    function PopulateStreamFormats: Boolean;
    function PopulateFormatDetails(const AMediaFormat : IMFMediaType; var ADetails : TVideoFormat) : Boolean;
    function ActiveDevice(const ADeviceSymbolicLink: PWideChar; out AMediaSource: IMFMediaSource): Boolean;
    function SetMediaType(const AMediaType : IMFMediaType) : Boolean;
  protected
    function SampleWithinTolerance(ARequestedTime: TTimeSpan; AActualTime: TTimeSpan): Boolean; override;
    function ConfigureSourceReader(const AAttributes: IMFAttributes) : Boolean; override;
    function SetMediaFormat: Boolean; override;

    procedure ResetVariables; override;
  public
    constructor Create; override;

    function OpenDeviceSource(const ADeviceSymbolicLink : PWideChar): Boolean;
    function GetCurrentFormat(var AFormat : TVideoFormat) : Boolean;
    function FormatSupported(AFormatIndex: Integer): Boolean;

    function SetVideoFormat(AFormatIndex : Integer) : Boolean;

    procedure RequestFrame; reintroduce;

    property VideoFormats : TVideoFormats read FVideoFormats;
  end;

implementation

uses
  Support;

constructor TCameraCaptureAsync.Create;
begin
  inherited;
  SetLength(FVideoFormats, 0);
end;

function TCameraCaptureAsync.OpenDeviceSource(const ADeviceSymbolicLink : PWideChar): Boolean;
var
  pSource: IMFMediaSource;
  oAttributes: IMFAttributes;
begin
  CloseSource;

  Result := ADeviceSymbolicLink <> '';

  if Result then
  begin
     CritSec.Lock;
     try
      // Create the media source for the device.
      Result := ActiveDevice(ADeviceSymbolicLink, pSource);
      try
       if Result then
          Result := SUCCEEDED(MFCreateAttributes(oAttributes,
                                                 1));

         if Result then
            Result := ConfigureSourceReader(oAttributes);

          if Result then
            Result := SUCCEEDED(MFCreateSourceReaderFromMediaSource(pSource,
                                                     oAttributes,
                                                      FSourceReader));
      finally
        pSource := nil;
      end;

      if Result then
        Result := PopulateStreamFormats;

      if Result then
        // By default, select the first stream
        Result := SelectVideoStream and GetVideoFormat(False);
     finally
       CritSec.Unlock;
     end;
  end;
end;


function TCameraCaptureAsync.ConfigureSourceReader(const AAttributes: IMFAttributes): Boolean;
begin
  inherited;
  // Enables advanced video processing by the Source Reader, including color space conversion, deinterlacing, video resizing,
  // and frame-rate conversion.
  Result := SUCCEEDED(AAttributes.SetUINT32(MF_SOURCE_READER_ENABLE_ADVANCED_VIDEO_PROCESSING, 1));

  if Result then
    Result := SUCCEEDED(AAttributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, 1));

  // This must be disable with Advanced Video Processing enabled
  if Result then
    Result := SUCCEEDED(AAttributes.SetUINT32(MF_READWRITE_DISABLE_CONVERTERS, 0));

end;


function TCameraCaptureAsync.ActiveDevice(const ADeviceSymbolicLink : PWideChar; out AMediaSource : IMFMediaSource) : Boolean;
var
  oAttributes: IMFAttributes;
  ppDevices : PIMFActivate;
  iCount : UINT32;
  iDeviceIndex : Integer;
  i : Integer;
  pcchLength: UINT32;
  m_pwszSymbolicLink: PWideChar;
begin
  Result := SUCCEEDED(MFCreateAttributes(oAttributes,
                     1));

  // Ask for source type to be video capture devices
  if Result then
    Result := SUCCEEDED(oAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                              MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID));

  if Result then
  begin
    try
      // Enumerate the devices.
      Result := SUCCEEDED(MFEnumDeviceSources(oAttributes,
                              ppDevices,
                              iCount));

      iDeviceIndex := -1;
      for i := 0 to iCount - 1 do
      begin
        {$POINTERMATH ON}
        ppDevices[i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                             m_pwszSymbolicLink,
                                             pcchLength);
        {$POINTERMATH OFF}
        // Check the symbolic link to see if this is the device we want.
        if SameText(m_pwszSymbolicLink, ADeviceSymbolicLink) then
           iDeviceIndex := i;
      end;

      if iDeviceIndex > - 1 then
      {$POINTERMATH ON}
        // Active the device based on the index found
        Result := SUCCEEDED(ppDevices[iDeviceIndex].ActivateObject(IID_IMFMediaSource,
                                         AMediaSource));
      {$POINTERMATH OFF}
    finally
      ppDevices := nil;
    end;
  end;
end;

function TCameraCaptureAsync.GetCurrentFormat(var AFormat : TVideoFormat) : Boolean;
var
  pCurrentType : IMFMediaType;
begin
  Result := SourceOpen and SUCCEEDED(SourceReader.GetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM), pCurrentType));
  if Result then
    Result := PopulateFormatDetails(pCurrentType, AFormat);
end;

function TCameraCaptureAsync.PopulateStreamFormats : Boolean;
var
  pMediaType : IMFMediaType;
  bTypeFound : Boolean;
  iTypeIndex : Integer;
  oMajorType : TGUID;
  iCount : Integer;
begin
  SetLength(FVideoFormats, 0);
  Result := SourceOpen;

  if Result then
  begin
    iTypeIndex := 0;
    iCount := 0;
    bTypeFound := True;

    while bTypeFound do
    begin
      bTypeFound := SUCCEEDED(SourceReader.GetNativeMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                   iTypeIndex,
                                   pMediaType));
      try
        if bTypeFound and SUCCEEDED(pMediaType.GetMajorType(oMajorType)) and (oMajorType = MFMediaType_Video) then
        begin
          inc(iCount);
          SetLength(FVideoFormats, iCount);
          PopulateFormatDetails(pMediaType, FVideoFormats[iCount - 1]);
        end;

        inc(iTypeIndex);
      finally
        pMediaType := nil;
      end;
    end;

  end;
end;

function TCameraCaptureAsync.FormatSupported(AFormatIndex: Integer): Boolean;
var
  pMediaType : IMFMediaType;
  oSubType : TGUID;
begin
  Result := SUCCEEDED(SourceReader.GetNativeMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                   AFormatIndex,
                                   pMediaType)) and SUCCEEDED(pMediaType.GetGUID(MF_MT_SUBTYPE,
                                           oSubType))
                                           and SampleConverter.IsInputSupported(oSubType);
end;

function TCameraCaptureAsync.SetVideoFormat(AFormatIndex: Integer): Boolean;
var
  pMediaType : IMFMediaType;
begin
  Result := SUCCEEDED(SourceReader.GetNativeMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                   AFormatIndex,
                                   pMediaType)) and SetMediaType(pMediaType);
end;


function TCameraCaptureAsync.PopulateFormatDetails(const AMediaFormat : IMFMediaType; var ADetails : TVideoFormat) : Boolean;
var
  uiHeigth : UINT32;
  uiWidth : UINT32;
  uiNumerator: UINT32;
  uiDenominator : UINT32;
  oSubType : TGUID;
begin
   // Get the video frame size
   Result := SUCCEEDED(MFGetAttributeSize(AMediaFormat,
                       MF_MT_FRAME_SIZE,
                       uiWidth,
                       uiHeigth));
   if Result then
   begin
     ADetails.iFrameWidth := uiWidth;
     ADetails.iFrameHeigth := uiHeigth;
   end;

   // Get the frame rate
   if Result then
   begin
     Result := SUCCEEDED(MFGetAttributeRatio(AMediaFormat,
                        MF_MT_FRAME_RATE,
                        uiNumerator,
                        uiDenominator));
     ADetails.iFramesPerSecond := Round(uiNumerator / uiDenominator);
   end;

   if SUCCEEDED(AMediaFormat.GetGUID(MF_MT_SUBTYPE, oSubType)) then
     ADetails.oSubType := oSubType;
end;

function TCameraCaptureAsync.SetMediaFormat: Boolean;
var
  pMediaType: IMFMediaType;
begin
  // Configure the source reader to give us progressive RGB32 frames.
  Result := SUCCEEDED(MFCreateMediaType(pMediaType));

  if Result then
    Result := SUCCEEDED(pMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                           MFMediaType_Video));

  if Result then
    Result := SUCCEEDED(pMediaType.SetGUID(MF_MT_SUBTYPE,
                                           MFVideoFormat_RGB32));


  if Result then
    Result := SUCCEEDED(FSourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                                          0,
                                                          pMediaType));

end;

function TCameraCaptureAsync.SetMediaType(const AMediaType : IMFMediaType) : Boolean;
var
  pMediaType : IMFMediaType;
begin
  // Configure the source reader to give us progressive RGB32 frames.
  Result := SUCCEEDED(MFCreateMediaType(pMediaType));

  if Result then
    Result := SUCCEEDED(AMediaType.CopyAllItems(pMediaType));

  if Result then
    Result := SUCCEEDED(FSourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                                         0,
                                                          pMediaType));

  FSourceReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   True);

  GetVideoFormat(False);
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

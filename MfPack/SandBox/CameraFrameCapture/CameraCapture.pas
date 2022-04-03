// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: CameraCapture.pas
// Kind: Pascal Unit
// Release date: 18-03-2022
// Language: ENU
//
// Revision Version: 3.1.1
//
// Description:
//   This unit contains the TCameraCapture class for project CameraFrameCapture.
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
unit CameraCapture;

interface

uses
  {Winapi}
  WinAPI.Windows,
  WinAPI.Messages,
  {System}
  System.Classes,
  System.TimeSpan,
  {MediaFoundationApi}
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.MediaFoundationApi.MfApi,
  WinAPI.MediaFoundationApi.MfObjects,
  WinAPI.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfIdl,
  {Application}
  Support,
  SampleConverter;

type
  TVideoFormat = record
    iMediaIndex : Integer;
    iFrameHeigth: Integer;
    iFrameWidth: Integer;
    iFramesPerSecond : Integer;
    oSubType : TGUID;
  end;
  TVideoFormats = TArray<TVideoFormat>;

  TOnCalculateComplete = reference to procedure(const AFramesPerSecond : Integer);

  TCameraCapture = class;

  TBurstThread = class(TThread)
  private
    FOwner : TCameraCapture;
    FTimerFrequency : Int64;
    FCurrentFPS : Integer;
  public
    constructor Create(AOwner : TCameraCapture); overload;
    procedure Execute; override;
    property CurrentFPS : Integer read FCurrentFPS write FCurrentFPS;
  end;

  TCameraCapture = class(TInterfacedPersistent)
  private
    FMaxFramesToSkip: Integer;
    FOnLog: TLogEvent;
    FOnFrameDataFound : TFrameDataEvent;
    FAwaitingFlush: Boolean;
    FFramesSkipped: Integer;
    FVideoInfo: TVideoFormatInfo;
    FSupportsSeek: Boolean;
    FSampleConverter: TSampleConverter;
    FBurstEnabled : Boolean;
    FCancelBurst : Boolean;
    FVideoFormats : TVideoFormats;
    FCritSec: TMFCritSec;
    FTimerStart: int64;
    FTimerEnd : int64;
    FMinimumFrameRate : Integer;
    FMaxFrameRateReadable : Integer;

    procedure SetMaxFramesToSkip(const AValue: Integer);

    function GetMaxFramesToSkip: Integer;
    function GetSourceOpen: Boolean;
    function IsFormatAvailable(const AMediaType: IMFMediaType): Boolean;
    function GetFrameRate(const AMediaFormat: IMFMediaType): Integer;

    procedure SetOnLog(const Value: TLogEvent);
    procedure ReturnDataFromSample(ASample: IMFSample);
  protected
    FSourceReader: IMFSourceReader;
    FOnCalculateComplete : TOnCalculateComplete;
    FCalculatingMax : Boolean;
    FBurstThread : TBurstThread;
    FThreadBurst : Boolean;

    procedure HandleFlushComplete; virtual;
    procedure HandleFrameSkipped;
    procedure ResetFramesSkipped;

    function PopulateStreamFormats: Boolean;
    function SetMediaType(const AMediaType : IMFMediaType) : Boolean;
    function PopulateFormatDetails(const AMediaFormat : IMFMediaType; var ADetails : TVideoFormat) : Boolean;
    function ActiveDevice(const ADeviceSymbolicLink: PWideChar; out AMediaSource: IMFMediaSource): Boolean;

    procedure ResetVariables; virtual;

    procedure HandleSampleReadError(AResult: HResult);

    procedure Flush; virtual;
    procedure HandleMediaFormatChanged; virtual;

    function SelectVideoStream: Boolean;

    function GetVideoFormat(AMediaTypeChanged: Boolean): Boolean;

    procedure ProcessSample(ASample: IMFSample); virtual; abstract;
    procedure ReturnSample(ASample: IMFSample);

    procedure Log(const AMessage: string;
                  const AType: TLogType);
    function ConfigureSourceReader(const AAttributes: IMFAttributes) : Boolean; virtual;

    property SourceReader: IMFSourceReader read FSourceReader;
    property AwaitingFlush: Boolean read FAwaitingFlush;
    property CritSec : TMFCritSec read FCritSec;
    property OnCalculateComplete : TOnCalculateComplete read FOnCalculateComplete write FOnCalculateComplete;
    property CalculatingMax : Boolean read FCalculatingMax write FCalculatingMax;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function OpenDeviceSource(const ADeviceSymbolicLink : PWideChar): Boolean; overload;
    function OpenDeviceSource(const AMediaSource : IMFMediaSource) : Boolean; overload;

    property MaxFramesToSkip: Integer read GetMaxFramesToSkip write SetMaxFramesToSkip;

    procedure StartTimer;
    procedure StopTimer;
    function GetTimerMs : Double;

    procedure CloseSource;
    procedure CancelCapture;

    procedure RequestFrame; virtual;
    procedure StartBurst; virtual;
    procedure StopBurst; virtual;

    procedure StartThreadBurst;
    procedure StopThreadBurst;

    function SetVideoFormat(AFormatIndex : Integer) : Boolean;
    function GetCurrentFormat(var AFormat : TVideoFormat) : Boolean;

    // Event hooks
    property OnLog: TLogEvent read FOnLog write SetOnLog;
    property OnFrameDataFound : TFrameDataEvent read FOnFrameDataFound write FOnFrameDataFound;

    property FramesSkipped: Integer read FFramesSkipped;
    property MaxFrameRateReadable : Integer read FMaxFrameRateReadable;

    // For debugging/testing purpose.
    // Calculate the current readable frame read within 5 seconds
    procedure CalculateMaxFrameRate(AOnComplete : TOnCalculateComplete); virtual;

    property SourceOpen: Boolean read GetSourceOpen;
    property VideoInfo: TVideoFormatInfo read FVideoInfo;
    property SupportsSeek: Boolean read FSupportsSeek;
    property BurstEnabled : Boolean read FBurstEnabled;

    property VideoFormats : TVideoFormats read FVideoFormats;
    property SampleConverter: TSampleConverter read FSampleConverter;
    property MinimumFrameRate : Integer read FMinimumFrameRate write FMinimumFrameRate;
  end;

implementation

uses
  {Winapi}
  WinAPI.ActiveX.PropIdl,
  WinAPI.WinApiTypes,
  WinAPI.MediaFoundationApi.MfError,
  WinAPI.ActiveX.PropVarUtil,
  {System}
  System.Math,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  {VCL}
  VCL.Graphics;


{ TCameraCapture }

constructor TCameraCapture.Create;
begin
  Inherited;
  ResetVariables;
  SetLength(FVideoFormats, 0);
  FThreadBurst := False;
  FTimerStart := 0;
  FTimerEnd := 0;
  FCritSec := TMFCritSec.Create;

  FCalculatingMax := False;
  FMinimumFrameRate := 24;
  FCancelBurst := False;

  FSampleConverter := TSampleConverter.Create;
end;


destructor TCameraCapture.Destroy;
begin
  StopBurst;
  SafeDelete(FCritSec);
  FreeAndNil(FSampleConverter);
  inherited;
end;

procedure TCameraCapture.CalculateMaxFrameRate(AOnComplete: TOnCalculateComplete);
begin
  FCalculatingMax := True;
  FMaxFrameRateReadable := 0;
  FOnCalculateComplete := AOnComplete;
end;

procedure TCameraCapture.CancelCapture;
begin
  Flush;
end;

procedure TCameraCapture.CloseSource;
begin
  if SourceOpen then
  begin
    Flush;
    Log('Destroy source reader - Begin',
        ltInfo);
    SafeRelease(FSourceReader);
    Log('Destroy source reader - End',
        ltInfo);
  end;

  ResetVariables;
end;

procedure TCameraCapture.ResetVariables;
begin
  FVideoInfo.Reset;
  FBurstEnabled := False;
  FAwaitingFlush := False;
  FSupportsSeek := False;
  FMaxFramesToSkip := 40;
end;

procedure TCameraCapture.ReturnSample(ASample: IMFSample);
begin
  if FCancelBurst then
    SafeRelease(ASample)
  else
    ReturnDataFromSample(ASample)
end;

procedure TCameraCapture.ReturnDataFromSample(ASample : IMFSample);
var
  oData : TMemoryStream;
  sError : string;
begin
  if SampleConverter.DataFromSample(ASample, VideoInfo, sError, oData) then
  begin
    SafeRelease(ASample);

    if Assigned(OnFrameDataFound) then
      OnFrameDataFound(oData);
  end
  else
  begin
    SafeRelease(ASample);

    Log('Failed to return data from frame sample: ' + sError, ltError);
  end;
end;

procedure TCameraCapture.Flush;
begin
  FAwaitingFlush := True;
end;

function TCameraCapture.GetMaxFramesToSkip: Integer;
begin
  Result := FMaxFramesToSkip;
end;

function TCameraCapture.GetSourceOpen: Boolean;
begin
  Result := Assigned(FSourceReader);
end;

procedure TCameraCapture.HandleFlushComplete;
begin
  Log('Flush complete', ltInfo);
  FAwaitingFlush := False;
end;

procedure TCameraCapture.HandleFrameSkipped;
begin
  inc(FFramesSkipped);
end;

procedure TCameraCapture.HandleMediaFormatChanged;
begin
  Log('Media Format Changed', ltInfo);
  GetVideoFormat(True);
  SelectVideoStream;
end;

function TCameraCapture.SetVideoFormat(AFormatIndex: Integer): Boolean;
var
  pMediaType : IMFMediaType;
  iMediaIndex : Integer;
begin
  iMediaIndex := FVideoFormats[AFormatIndex].iMediaIndex;

  Result := SUCCEEDED(SourceReader.GetNativeMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                   iMediaIndex,
                                   pMediaType)) and SetMediaType(pMediaType);
end;

function TCameraCapture.SetMediaType(const AMediaType : IMFMediaType) : Boolean;
var
  pMediaType : IMFMediaType;
begin
  Result := SUCCEEDED(MFCreateMediaType(pMediaType));

  if Result then
    Result := SUCCEEDED(AMediaType.CopyAllItems(pMediaType));

  if Result then
    Result := SUCCEEDED(FSourceReader.SetCurrentMediaType(DWord(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                                         0,
                                                          pMediaType));
  if Result then
    Result := SelectVideoStream;

  GetVideoFormat(False);
end;

function TCameraCapture.GetCurrentFormat(var AFormat : TVideoFormat) : Boolean;
var
  pCurrentType : IMFMediaType;
begin
  Result := SourceOpen and SUCCEEDED(SourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM, pCurrentType));
  if Result then
    Result := PopulateFormatDetails(pCurrentType, AFormat);
end;

function TCameraCapture.OpenDeviceSource(const AMediaSource : IMFMediaSource) : Boolean;
var
  oAttributes: IMFAttributes;
begin
  Result := SUCCEEDED(MFCreateAttributes(oAttributes,
                                             1));
  if Result then
    Result := ConfigureSourceReader(oAttributes);

  if Result then
    Result := SUCCEEDED(MFCreateSourceReaderFromMediaSource(AMediaSource,
                                             oAttributes,
                                              FSourceReader));
  if Result then
    Result := PopulateStreamFormats;

  if Result then
    // By default, select the first stream
    Result := SelectVideoStream and GetVideoFormat(False);
end;

function TCameraCapture.OpenDeviceSource(const ADeviceSymbolicLink : PWideChar): Boolean;
var
  pSource: IMFMediaSource;
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
          Result := OpenDeviceSource(pSource);
      finally
        pSource := nil;
      end;
     finally
       CritSec.Unlock;
     end;
  end;
end;

function TCameraCapture.ActiveDevice(const ADeviceSymbolicLink : PWideChar; out AMediaSource : IMFMediaSource) : Boolean;
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

function TCameraCapture.PopulateStreamFormats : Boolean;
var
  pMediaType : IMFMediaType;
  bTypeFound : Boolean;
  iMediaTypeIndex : Integer;

  iCount : Integer;
begin
  SetLength(FVideoFormats, 0);
  Result := SourceOpen;

  if Result then
  begin
    iMediaTypeIndex := 0;
    iCount := 0;
    bTypeFound := True;

    while bTypeFound do
    begin
      bTypeFound := SUCCEEDED(SourceReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   iMediaTypeIndex,
                                   pMediaType));
      try
        if bTypeFound and IsFormatAvailable(pMediaType) then
        begin
          inc(iCount);
          SetLength(FVideoFormats, iCount);
          PopulateFormatDetails(pMediaType, FVideoFormats[iCount - 1]);
          FVideoFormats[iCount - 1].iMediaIndex := iMediaTypeIndex;
        end;

        inc(iMediaTypeIndex);
      finally
        pMediaType := nil;
      end;
    end;

  end;
end;

function TCameraCapture.IsFormatAvailable(const AMediaType : IMFMediaType) : Boolean;
var
  oMajorType : TGUID;
  oSubType : TGUID;
begin
  Result := SUCCEEDED(AMediaType.GetMajorType(oMajorType)) and (oMajorType = MFMediaType_Video)
            and SUCCEEDED(AMediaType.GetGUID(MF_MT_SUBTYPE, oSubType))
            and SampleConverter.IsInputSupported(oSubType)
            and (GetFrameRate(AMediaType) >= FMinimumFrameRate);
end;

function TCameraCapture.PopulateFormatDetails(const AMediaFormat : IMFMediaType; var ADetails : TVideoFormat) : Boolean;
var
  uiHeigth : UINT32;
  uiWidth : UINT32;
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
   ADetails.iFramesPerSecond := GetFrameRate(AMediaFormat);

   if SUCCEEDED(AMediaFormat.GetGUID(MF_MT_SUBTYPE, oSubType)) then
     ADetails.oSubType := oSubType;
end;

function TCameraCapture.GetFrameRate(const AMediaFormat : IMFMediaType) : Integer;
var
  uiNumerator: UINT32;
  uiDenominator : UINT32;
begin
  if SUCCEEDED(MFGetAttributeRatio(AMediaFormat,
                        MF_MT_FRAME_RATE,
                        uiNumerator,
                        uiDenominator)) then
    Result := Round(uiNumerator / uiDenominator)
  else
    Result := 0;
end;

procedure TCameraCapture.HandleSampleReadError(AResult: HResult);
var
  sError: string;

begin
  case AResult of
    E_POINTER:
      sError := 'Object not initialized.';
    MF_E_INVALIDREQUEST:
      sError := 'Invalid request';
    MF_E_INVALIDSTREAMNUMBER:
      sError := 'The dwStreamIndex parameter is invalid.';
    MF_E_NOTACCEPTING:
      sError := 'A flush operation is pending';
    E_INVALIDARG:
      sError := 'Invalid argument.';
  else
    sError := 'Unknown error';
  end;

  Log('ReadSample call failed: ' + sError, ltError);
end;


procedure TCameraCapture.Log(const AMessage: string; const AType: TLogType);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage, AType);
end;

function TCameraCapture.ConfigureSourceReader(const AAttributes: IMFAttributes) : Boolean;
begin
  // Enables advanced video processing by the Source Reader, including color space conversion, deinterlacing, video resizing,
  // and frame-rate conversion.
  Result := SUCCEEDED(AAttributes.SetUINT32(MF_SOURCE_READER_ENABLE_ADVANCED_VIDEO_PROCESSING, 1));

 if Result then
   Result := SUCCEEDED(AAttributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, 1));

  // This must be disable with Advanced Video Processing enabled
 if Result then
   Result := SUCCEEDED(AAttributes.SetUINT32(MF_READWRITE_DISABLE_CONVERTERS, 0));
end;

function TCameraCapture.GetVideoFormat(AMediaTypeChanged: Boolean): Boolean;
var
  oSubType: TGUID;
  uHeight: UINT32;
  uWidth: UINT32;
  pInputType: IMFMediaType;
begin
  Result := SUCCEEDED(FSourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                                        pInputType));

  if Result and SUCCEEDED(pInputType.GetGUID(MF_MT_SUBTYPE,
                                             oSubType)) then
  begin
      FVideoInfo.oSubType := oSubType;

      Result := SUCCEEDED(MFGetAttributeSize(pInputType,
                                             MF_MT_FRAME_SIZE,
                                             uWidth,
                                             uHeight));


      if Result then
        begin
          FVideoInfo.iBufferWidth := uWidth;
          FVideoInfo.iBufferHeight := uHeight;

          // If the source type has changed the video buffer dimensions have changed.
          // We still want to use the original video dimensions for the full frame capture, not the buffer dimensions.
          if not AMediaTypeChanged then
            begin
              FVideoInfo.iVideoWidth := uWidth;
              FVideoInfo.iVideoHeight := uHeight;
            end;
        end;

       FVideoInfo.iStride := MFGetAttributeUINT32(pInputType,
                                                   MF_MT_DEFAULT_STRIDE,
                                                   1);

      SampleConverter.UpdateConverter(pInputType);
    end;
end;


function TCameraCapture.SelectVideoStream: Boolean;
begin
  Result := SUCCEEDED(FSourceReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS, False));

  // Select the first video stream
  if Result then
    Result := SUCCEEDED(FSourceReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                                         True));

  if not Result then
    Log('SelectVideoStream failed',
        ltError);
end;

procedure TCameraCapture.SetOnLog(const Value: TLogEvent);
begin
  FOnLog := Value;
  if Assigned(FSampleConverter) then
   FSampleConverter.OnLog := FOnLog;
end;

procedure TCameraCapture.StartBurst;
begin
  FThreadBurst := False;

  StopBurst;
  if not FBurstEnabled then
  begin
    FBurstEnabled := True;
    FCancelBurst := False;
    RequestFrame;
  end;
end;

procedure TCameraCapture.StopBurst;
begin
  if FBurstEnabled then
  begin
    FCancelBurst := True;
    FBurstEnabled := False;
  end;
end;

procedure TCameraCapture.StartThreadBurst;
var
  oCurrentFormat : TVideoFormat;
begin
  FThreadBurst := True;

  StopThreadBurst;
  if not FBurstEnabled then
  begin
    FBurstEnabled := True;
    FCancelBurst := False;

    GetCurrentFormat(oCurrentFormat);

    FBurstThread := TBurstThread.Create(Self);
    FBurstThread.CurrentFPS := oCurrentFormat.iFramesPerSecond;
    FBurstThread.FreeOnTerminate := True;
  end;
end;

procedure TCameraCapture.StopThreadBurst;
begin
 if FBurstEnabled then
  begin
    FCancelBurst := True;
    FBurstEnabled := False;

    if Assigned(FBurstThread) then
      FBurstThread.Terminate;
  end;
end;

procedure TCameraCapture.RequestFrame;
begin
  FCancelBurst := False;
end;


procedure TCameraCapture.StartTimer;
begin
  QueryPerformanceCounter(FTimerStart);
end;


procedure TCameraCapture.StopTimer;
begin
  QueryPerformanceCounter(FTimerEnd);
end;

function TCameraCapture.GetTimerMs: Double;
begin
  if TimerFrequency > 0 then
    Result := (FTimerEnd - FTimerStart) / TimerFrequency * 1000
  else
    Result := 0;
end;

procedure TCameraCapture.ResetFramesSkipped;
begin
  FFramesSkipped := 0;
end;

procedure TCameraCapture.SetMaxFramesToSkip(const AValue: Integer);
begin
  FMaxFramesToSkip := AValue;
end;

{ TBurstThread }

constructor TBurstThread.Create(AOwner: TCameraCapture);
begin
  Create;
  FOwner := AOwner;
  QueryPerformanceFrequency(FTimerFrequency);
end;

procedure TBurstThread.Execute;
var
  bContinue : Boolean;
  iFrameRequestCount : Integer;
  iCaptureStartMs : Int64;
const
  MAX_FPS_BUFFER = 5;
begin
  bContinue := True;
  iFrameRequestCount := 0;
  iCaptureStartMs := PerformanceCounterMilliseconds(FTimerFrequency);

  while bContinue and (not Terminated) and Assigned(FOwner.SourceReader) do
  begin
    // Do not keeping requesting frames more than needed to obtain the max FPS.
    // Hammering ReadSample inside a loop can cause memory leaks within the sample cache.
    if iFrameRequestCount < (FCurrentFPS + MAX_FPS_BUFFER) then
    begin
     bContinue := SUCCEEDED(FOwner.SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                       0,
                                       nil,
                                       nil,
                                       nil,
                                       nil));
      inc(iFrameRequestCount);
    end;

    if PerformanceCounterMilliseconds(FTimerFrequency) > (iCaptureStartMs + 1000) then
    begin
      FOwner.OnLog(Format('Thread frame request count %d', [iFrameRequestCount]), ltDebug1);

      iFrameRequestCount := 0;
      iCaptureStartMs := PerformanceCounterMilliseconds(FTimerFrequency);
    end;
  end;
end;

end.

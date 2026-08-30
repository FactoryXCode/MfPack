// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
//
// Module: AsyncVideoEngine.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: A minimal asynchronous Source Reader pump. Exactly one read and
//              one UI frame packet can be outstanding at a time.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//
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
unit AsyncVideoEngine;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite;

const
  WM_ASYNC_FRAME_READY = WM_APP + 301;
  WM_ASYNC_END_OF_STREAM = WM_APP + 302;
  WM_ASYNC_PLAYBACK_ERROR = WM_APP + 303;

type

  TVideoFramePacket = class
  public
    Session: Cardinal;
    TimeStamp: LONGLONG;
    Width: UINT32;
    Height: UINT32;
    ColorPixels: TBytes;
    GrayPixels: TBytes;
  end;

  TPlaybackNotice = class
  public
    Session: Cardinal;
    Text: string;

    constructor Create(const ASession: Cardinal;
                       const AText: string);
  end;

  IAsyncVideoEngine = interface
  ['{4526BEB5-C06C-4790-8939-D2C912180A67}']
    function Open(const AFileName: string): LONGLONG;
    procedure Start();
    procedure Pause();
    procedure RequestNext();

    procedure Close(const ADetachWindow: Boolean);
  end;

function CreateAsyncVideoEngine(const ANotifyWindow: HWND;
                                const ASession: Cardinal): IAsyncVideoEngine;


implementation

uses
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfGrayscaleMFT;

const
  BYTES_PER_PIXEL = 4;

type
  TAsyncVideoEngine = class(TInterfacedObject, IAsyncVideoEngine, IMFSourceReaderCallback)
  private
    FLock: TRTLCriticalSection;
    FNotifyWindow: HWND;
    FSession: Cardinal;
    FReader: IMFSourceReader;
    FReaderType: IMFMediaType;
    FTransform: IMFTransform;
    FWidth: UINT32;
    FHeight: UINT32;
    FStride: Integer;
    FRunning: Boolean;
    FReadPending: Boolean;

    procedure Check(const AOperation: string;
                    const AHr: HRESULT);

    procedure ConfigureCurrentType();

    function CopySampleToPacked(const ASample: IMFSample;
                                out APixels: TBytes): HRESULT;

    function CreatePacket(const ASample: IMFSample;
                          const ATimeStamp: LONGLONG;
                          out APacket: TVideoFramePacket): HRESULT;

    procedure PostError(const AText: string);

    // IMFSourceReaderCallback implementation.
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWORD;
                          dwStreamFlags: DWORD; llTimestamp: HNSTIME;
                          pSample: IMFSample): HRESULT; stdcall;

    function OnFlush(dwStreamIndex: DWORD): HRESULT; stdcall;

    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HRESULT; stdcall;
  public

    constructor Create(const ANotifyWindow: HWND;
                       const ASession: Cardinal);
    destructor Destroy(); override;

    function Open(const AFileName: string): LONGLONG;
    procedure Start();
    procedure Pause();
    procedure RequestNext();
    procedure Close(const ADetachWindow: Boolean);
  end;

constructor TPlaybackNotice.Create(const ASession: Cardinal;
                                   const AText: string);
begin

  inherited Create;

  Session := ASession;
  Text := AText;
end;


function CreateAsyncVideoEngine(const ANotifyWindow: HWND;
                                const ASession: Cardinal): IAsyncVideoEngine;
begin

  Result := TAsyncVideoEngine.Create(ANotifyWindow, ASession);
end;


constructor TAsyncVideoEngine.Create(const ANotifyWindow: HWND;
                                     const ASession: Cardinal);
begin

  inherited Create;

  InitializeCriticalSection(FLock);
  FNotifyWindow := ANotifyWindow;
  FSession := ASession;
end;


destructor TAsyncVideoEngine.Destroy;
begin

  Close(True);
  DeleteCriticalSection(FLock);

  inherited;
end;


procedure TAsyncVideoEngine.Check(const AOperation: string;
                                  const AHr: HRESULT);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: HRESULT 0x%.8x',
                              [AOperation, Cardinal(AHr)]);
end;


procedure TAsyncVideoEngine.ConfigureCurrentType;
var
  StrideValue: UINT32;

begin

  FReaderType := nil;

  Check('GetCurrentMediaType',
        FReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    @FReaderType));

  Check('MFGetAttributeSize',
        MFGetAttributeSize(FReaderType,
                           MF_MT_FRAME_SIZE,
                           FWidth,
                           FHeight));

  FStride := Integer(FWidth) * BYTES_PER_PIXEL;

  if SUCCEEDED(FReaderType.GetUINT32(MF_MT_DEFAULT_STRIDE,
                                     StrideValue)) then
    FStride := Integer(StrideValue);

  if FStride = 0 then
    FStride := Integer(FWidth) * BYTES_PER_PIXEL;

  FTransform := TMfGrayscaleMFT.Create as IMFTransform;

  Check('SetInputType',
         FTransform.SetInputType(0,
                                 FReaderType,
                                 0));

  Check('SetOutputType',
        FTransform.SetOutputType(0,
                                 FReaderType,
                                 0));

  Check('Begin streaming',
        FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                  0));

  Check('Start stream',
        FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                  0));
end;


function TAsyncVideoEngine.Open(const AFileName: string): LONGLONG;
var
  Attributes: IMFAttributes;
  RequestedType: IMFMediaType;
  DurationValue: PROPVARIANT;
  Callback: IMFSourceReaderCallback;

begin

  Close(False);
  Result := 0;

  Callback := Self as IMFSourceReaderCallback;

  Check('MFCreateAttributes',
        MFCreateAttributes(Attributes,
                           2));

  Check('Set asynchronous callback',
        Attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                              Callback));

  Check('Enable video processing',
         Attributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                              1));

  Check('MFCreateSourceReaderFromURL',
        MFCreateSourceReaderFromURL(PWideChar(AFileName),
                                    Attributes,
                                    FReader));

  Check('Deselect streams',
        FReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                   False));

  Check('Select video',
        FReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   True));

  Check('MFCreateMediaType',
        MFCreateMediaType(RequestedType));

  Check('Set major type',
        RequestedType.SetGUID(MF_MT_MAJOR_TYPE,
                              MFMediaType_Video));

  Check('Request RGB32',
        RequestedType.SetGUID(MF_MT_SUBTYPE,
                              MFVideoFormat_RGB32));

  Check('SetCurrentMediaType',
        FReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    0,
                                    RequestedType));

  ConfigureCurrentType();

  PropVariantInit(DurationValue);

  try
    Check('Get duration',
          FReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                           MF_PD_DURATION, DurationValue));

    if (DurationValue.vt <> VT_UI8) then
      raise Exception.Create('The source did not report a duration.');

    Result := DurationValue.hVal.QuadPart;

  finally
    PropVariantClear(DurationValue);
  end;
end;


procedure TAsyncVideoEngine.Close(const ADetachWindow: Boolean);
var
  Reader: IMFSourceReader;

begin

  EnterCriticalSection(FLock);

  try
    FRunning := False;
    Reader := FReader;
    FReader := nil;
    FReaderType := nil;

    if Assigned(FTransform) then
      FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                0);

    FTransform := nil;
    FReadPending := False;

    if ADetachWindow then
      FNotifyWindow := 0;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(Reader) then
    Reader.Flush(MF_SOURCE_READER_ALL_STREAMS);
end;


procedure TAsyncVideoEngine.Start();
begin

  EnterCriticalSection(FLock);

  try
    FRunning := True;
  finally
    LeaveCriticalSection(FLock);
  end;

  RequestNext();
end;


procedure TAsyncVideoEngine.Pause();
begin

  EnterCriticalSection(FLock);

  try
    FRunning := False;
  finally
    LeaveCriticalSection(FLock);
  end;
end;


procedure TAsyncVideoEngine.RequestNext();
var
  Reader: IMFSourceReader;
  Hr: HRESULT;

begin

  Reader := nil;
  EnterCriticalSection(FLock);

  try
    if FRunning and Assigned(FReader) and not FReadPending then
      begin
        Reader := FReader;
        FReadPending := True;
      end;

  finally
    LeaveCriticalSection(FLock);
  end;

  if not Assigned(Reader) then
    Exit;

  Hr := Reader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                          0,
                          nil,
                          nil,
                          nil,
                          nil);
  if FAILED(Hr) then
    begin
      EnterCriticalSection(FLock);

      try
        FReadPending := False;
      finally
        LeaveCriticalSection(FLock);
      end;

      PostError(Format('ReadSample failed: HRESULT 0x%.8x',
                       [Cardinal(Hr)]));
    end;
end;


function TAsyncVideoEngine.CopySampleToPacked(const ASample: IMFSample;
                                              out APixels: TBytes): HRESULT;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  RowBytes: Integer;
  MemoryStride: Integer;
  RequiredLength: UInt64;
  Y: Integer;
  SourceY: Integer;
  SourceRow: Pointer;

begin

  SetLength(APixels,
            0);

  Buffer := nil;
  Result := ASample.ConvertToContiguousBuffer(@Buffer);

  if FAILED(Result) then
    Exit;

  Data := nil;
  MaxLength := 0;
  CurrentLength := 0;

  Result := Buffer.Lock(Data,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    RowBytes := Integer(FWidth) * BYTES_PER_PIXEL;
    MemoryStride := FStride;

    if (MemoryStride < 0) then
      MemoryStride := -MemoryStride;

    RequiredLength := UInt64(MemoryStride) * UInt64(FHeight);

    if (MemoryStride < RowBytes) or
       (UInt64(CurrentLength) < RequiredLength) then
      Exit(E_UNEXPECTED);

    SetLength(APixels,
              RowBytes * Integer(FHeight));

    for Y := 0 to Integer(FHeight) - 1 do
      begin
        if (FStride > 0) then
          SourceY := Integer(FHeight) - 1 - Y
        else
          SourceY := Y;

        SourceRow := Pointer(NativeUInt(Data) + NativeUInt(SourceY * MemoryStride));

        Move(SourceRow^,
             APixels[Y * RowBytes],
             RowBytes);
      end;
    Result := S_OK;
  finally
    Buffer.Unlock;
  end;
end;


function TAsyncVideoEngine.CreatePacket(const ASample: IMFSample;
                                        const ATimeStamp: LONGLONG;
                                        out APacket: TVideoFramePacket): HRESULT;
var
  OutputData: MFT_OUTPUT_DATA_BUFFER;
  OutputStatus: DWORD;

begin

  FillChar(OutputData,
           SizeOf(OutputData),
           0);

  APacket := TVideoFramePacket.Create();
  APacket.Session := FSession;
  APacket.TimeStamp := ATimeStamp;
  APacket.Width := FWidth;
  APacket.Height := FHeight;

  Result := CopySampleToPacked(ASample,
                               APacket.ColorPixels);
  if SUCCEEDED(Result) then
    Result := FTransform.ProcessInput(0,
                                      ASample,
                                      0);

  if SUCCEEDED(Result) then
    begin
      OutputStatus := 0;
      Result := FTransform.ProcessOutput(0,
                                         1,
                                         @OutputData,
                                         OutputStatus);
    end;

  if SUCCEEDED(Result) and not Assigned(OutputData.pSample) then
    Result := E_UNEXPECTED;

  if SUCCEEDED(Result) then
    Result := CopySampleToPacked(OutputData.pSample,
                                 APacket.GrayPixels);

  if FAILED(Result) then
    FreeAndNil(APacket);
end;


procedure TAsyncVideoEngine.PostError(const AText: string);
var
  Notice: TPlaybackNotice;
  WindowHandle: HWND;

begin

  EnterCriticalSection(FLock);

  try
    WindowHandle := FNotifyWindow;
  finally
    LeaveCriticalSection(FLock);
  end;


  if (WindowHandle = 0) then
    Exit;

  Notice := TPlaybackNotice.Create(FSession,
                                   AText);

  if not PostMessage(WindowHandle,
                     WM_ASYNC_PLAYBACK_ERROR,
                     0,
                     LPARAM(Notice)) then
    Notice.Free;
end;


function TAsyncVideoEngine.OnReadSample(hrStatus: HRESULT;
                                        dwStreamIndex: DWORD;
                                        dwStreamFlags: DWORD;
                                        llTimestamp: HNSTIME;
                                        pSample: IMFSample): HRESULT;
var
  Packet: TVideoFramePacket;
  WindowHandle: HWND;

begin

  Result := S_OK;
  Packet := nil;
  WindowHandle := 0;

  try
    EnterCriticalSection(FLock);

    try
      FReadPending := False;
      if not FRunning or not Assigned(FReader) then
        Exit;

      if FAILED(hrStatus) then
        raise Exception.CreateFmt('Asynchronous read failed: HRESULT 0x%.8x',
                                  [Cardinal(hrStatus)]);

      if ((dwStreamFlags and MF_SOURCE_READERF_ERROR) <> 0) then
        raise Exception.Create('The Source Reader reported a streaming error.');

      if ((dwStreamFlags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0) then
        ConfigureCurrentType;

      if ((dwStreamFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
        begin
          FRunning := False;
          WindowHandle := FNotifyWindow;

          if (WindowHandle <> 0) then
            PostMessage(WindowHandle,
                        WM_ASYNC_END_OF_STREAM,
                        WPARAM(FSession),
                        0);
          Exit;
        end;


      if not Assigned(pSample) then
        begin
          // A stream tick has no sample. Ask again outside this callback.
          WindowHandle := FNotifyWindow;

          if (WindowHandle <> 0) then
            PostMessage(WindowHandle,
                        WM_ASYNC_FRAME_READY,
                        WPARAM(FSession),
                        0);
          Exit;
        end;

      Check('Process asynchronous frame',
            CreatePacket(pSample,
                         llTimestamp,
                         Packet));

      WindowHandle := FNotifyWindow;

    finally
      LeaveCriticalSection(FLock);
    end;

    if (WindowHandle = 0) or
       not PostMessage(WindowHandle,
                       WM_ASYNC_FRAME_READY,
                       WPARAM(FSession),
                       LPARAM(Packet)) then
      Packet.Free;

  except
    on E: Exception do
      begin
        Packet.Free;
        PostError(E.Message);
      end;
  end;
end;


function TAsyncVideoEngine.OnFlush(dwStreamIndex: DWORD): HRESULT;
begin

  Result := S_OK;
end;


function TAsyncVideoEngine.OnEvent(dwStreamIndex: DWORD;
                                   pEvent: IMFMediaEvent): HRESULT;
begin

  Result := S_OK;
end;

end.

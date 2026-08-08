// FactoryX
//
// Copyright ? FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastHttpServer.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: This unit handles direct files, byte ranges, WebVTT resources,
//              and later fragmented output.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// Source: -
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
unit MfCastHttpServer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  {MfCastTypes}
  MfPCXConstants,
  MfCastTypes,
  MfCastInterfaces;

type

  IMfCastByteStreamAsyncState = interface
    ['{68C87C4B-26D6-4FF4-B734-B539122361D0}']
    function GetBytesTransferred(): ULONG;
  end;


  TMfCastByteStreamAsyncState = class(TInterfacedObject,
                                      IMfCastByteStreamAsyncState)
  private
    FBytesTransferred: ULONG;

  public

    constructor Create(const ABytesTransferred: ULONG);

   function GetBytesTransferred(): ULONG;
  end;


  IMfCastLiveBuffer = interface
    ['{5CA9780A-F3D6-4923-86A1-485F746DD40A}']
    function WriteAt(const AOffset: UInt64;
                     ABuffer: Pointer;
                     const ASize: Cardinal): HRESULT;
    function ReadAt(const AOffset: UInt64;
                    ABuffer: Pointer;
                    const ABufferSize: Cardinal;
                    out ABytesRead: Cardinal): HRESULT;
    function WaitForData(const AOffset: UInt64;
                         const ATimeoutMs: Cardinal): HRESULT;
    function GetLength(): UInt64;
    function SetLengthValue(const ALength: UInt64): HRESULT;
    function Flush(): HRESULT;
    function IsComplete(): Boolean;
    procedure Complete();
    procedure Close();
  end;


  TMfCastLiveBuffer = class(TInterfacedObject, IMfCastLiveBuffer)
  private
    FLock: TCriticalSection;
    FData: TBytes;
    FBaseOffset: UInt64;
    FLength: UInt64;
    FLastWriteDebugLength: UInt64;
    FLastPruneDebugOffset: UInt64;
    FComplete: Boolean;
    FClosed: Boolean;

    procedure EnsureCapacity(const ARequired: UInt64);
    procedure DiscardBeforeLocked(const AOffset: UInt64);

  public

    constructor Create();
    destructor Destroy(); override;

    function WriteAt(const AOffset: UInt64;
                     ABuffer: Pointer;
                     const ASize: Cardinal): HRESULT;

    function ReadAt(const AOffset: UInt64;
                    ABuffer: Pointer;
                    const ABufferSize: Cardinal;
                    out ABytesRead: Cardinal): HRESULT;

    function WaitForData(const AOffset: UInt64;
                         const ATimeoutMs: Cardinal): HRESULT;

    function GetLength(): UInt64;
    function SetLengthValue(const ALength: UInt64): HRESULT;
    function Flush(): HRESULT;
    function IsComplete(): Boolean;
    procedure Complete();
    procedure Close();
  end;


  TMfCastLiveByteStream = class(TInterfacedObject, IMFByteStream)
  private
    FBuffer: IMfCastLiveBuffer;
    FPosition: UInt64;
    FClosed: Boolean;

  public

    constructor Create(const ABuffer: IMfCastLiveBuffer);

    function GetCapabilities(out pdwCapabilities: DWord): HResult; stdcall;

    function GetLength(out pqwLength: QWORD): HResult; stdcall;

    function SetLength(qwLength: QWORD): HResult; stdcall;

    function GetCurrentPosition(out pqwPosition: QWORD): HResult; stdcall;

    function SetCurrentPosition(const qwPosition: QWORD): HResult; stdcall;

    function IsEndOfStream(out pfEndOfStream: BOOL): HResult; stdcall;

    function Read(pb: PByte;
                  cb: ULONG;
                  out pcbRead: ULONG): HResult; stdcall;

    function BeginRead(pb: PByte;
                       cb: ULONG;
                       pCallback: IMFAsyncCallback;
                       punkState: IUnknown): HResult; stdcall;

    function EndRead(pResult: IMFAsyncResult;
                     out pcbRead: ULONG): HResult; stdcall;

    function Write(pb: PByte;
                   cb: ULONG;
                   out pcbWritten: ULONG): HResult; stdcall;

    function BeginWrite(pb: PByte;
                        cb: ULONG;
                        pCallback: IMFAsyncCallback;
                        punkState: IUnknown): HResult; stdcall;

    function EndWrite(pResult: IMFAsyncResult;
                      out pcbWritten: ULONG): HResult; stdcall;

    function Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                  llSeekOffset: LONGLONG;
                  dwSeekFlags: DWord;
                  out pqwCurrentPosition: QWORD): HResult; stdcall;

    function Flush(): HResult; stdcall;
    function Close(): HResult; stdcall;
  end;


  TMfCastLiveStreamContent = class(TInterfacedObject,
                                   IMfCastHttpContent,
                                   IMfCastLiveHttpContent)
  private
    FBuffer: IMfCastLiveBuffer;
    FContentType: string;

  public

    constructor Create(const ABuffer: IMfCastLiveBuffer;
                       const AContentType: string);

    function GetContentType(): string;
    function GetLength(out ALength: UInt64): HRESULT;
    function CanSeek(): Boolean;
    function IsComplete(): Boolean;

    function ReadAt(const AOffset: UInt64;
                    ABuffer: Pointer;
                    const ABufferSize: Cardinal;
                    out ABytesRead: Cardinal): HRESULT;

    function WaitForData(const AOffset: UInt64;
                         const ATimeoutMs: Cardinal): HRESULT;
  end;


  TMfCastHttpServer = class(TInterfacedObject, IMfCastHttpServer)
  private

    FSettings: TMfCastHttpSettings;
    FLogger: IMfCastLogger;
    FRunning: Boolean;
    FListenPort: Word;
    FListenSocket: TSocket;
    FClientSocket: TSocket;
    FServerThread: TThread;
    FWSAStarted: Boolean;
    FRequestCount: Integer;
    FResources: TDictionary<string, IMfCastHttpContent>;
    FLock: TCriticalSection;

    function NormalizeBasePath(const APath: string): string;
    function CreateResourcePath(const AResourceName: string): string;
    function ResolveAdvertisedAddress(out AAddress: string): HRESULT;
    procedure AcceptLoop();
    procedure HandleClient(const AClient: TSocket);
    function TryGetResource(const APath: string;
                            out AContent: IMfCastHttpContent): Boolean;

  public

    constructor Create();
    destructor Destroy(); override;

    function Configure(const ASettings: TMfCastHttpSettings): HRESULT;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(): HRESULT;
    function Stop(): HRESULT;

    function Publish(const AResourceName: string;
                     const AContent: IMfCastHttpContent;
                     out APublishedPath: string): HRESULT;

    function Unpublish(const APublishedPath: string): HRESULT;
    function BuildUrl(const APublishedPath: string;
                      out AUrl: string): HRESULT;

    function IsRunning(): Boolean;
    function GetListenPort(): Word;
    function GetRequestCount(): Cardinal;
  end;


  TMfCastFileContent = class(TInterfacedObject, IMfCastHttpContent)
  private
    FFileName: string;
    FContentType: string;

  public

    constructor Create(const AFileName: string;
                       const AContentType: string);

    function GetContentType(): string;
    function GetLength(out ALength: UInt64): HRESULT;
    function CanSeek(): Boolean;
    function IsComplete(): Boolean;

    function ReadAt(const AOffset: UInt64;
                    ABuffer: Pointer;
                    const ABufferSize: Cardinal;
                    out ABytesRead: Cardinal): HRESULT;

  end;


  TMfCastMemoryContent = class(TInterfacedObject, IMfCastHttpContent)
  private
    FData: TBytes;
    FContentType: string;

  public

    constructor Create(const AData: TBytes;
                       const AContentType: string);

    function GetContentType(): string;
    function GetLength(out ALength: UInt64): HRESULT;
    function CanSeek(): Boolean;
    function IsComplete(): Boolean;
    function ReadAt(const AOffset: UInt64;
                    ABuffer: Pointer;
                    const ABufferSize: Cardinal;
                    out ABytesRead: Cardinal): HRESULT;

  end;

  TMfCastSegmentPublisher = class(TInterfacedObject, IMfCastSegmentPublisher)
  private
    FServer: IMfCastHttpServer;
    FLogger: IMfCastLogger;
    FEntryPath: string;
    FBuffer: IMfCastLiveBuffer;
    FContent: IMfCastHttpContent;
    FByteStream: IMFByteStream;

  public

    constructor Create(const AServer: IMfCastHttpServer);
    destructor Destroy(); override;

    procedure SetLogger(const ALogger: IMfCastLogger);
    function BeginPresentation(const AContentType: string;
                               out AEntryPath: string): HRESULT;
    function GetByteStream(out AByteStream: IMFByteStream): HRESULT;
    function CompletePresentation: HRESULT;
    function AbortPresentation(const AReason: HRESULT): HRESULT;
  end;


implementation


type
  TMfCastHttpServerThread = class(TThread)
  private
    FOwner: TMfCastHttpServer;

  protected
    procedure Execute(); override;

  public

    constructor Create(AOwner: TMfCastHttpServer);
  end;


function MfCastCorsResponseHeaders(): AnsiString;
begin

  Result := AnsiString('Access-Control-Allow-Origin: *' + ULBR +
                       'Access-Control-Allow-Methods: GET, HEAD, OPTIONS' + ULBR +
                       'Access-Control-Allow-Headers: Content-Type, Accept-Encoding, Range' + ULBR +
                       'Access-Control-Expose-Headers: Accept-Ranges, Content-Length, Content-Range' + ULBR);
end;


constructor TMfCastByteStreamAsyncState.Create(const ABytesTransferred: ULONG);
begin

  inherited Create;

  FBytesTransferred := ABytesTransferred;
end;


function TMfCastByteStreamAsyncState.GetBytesTransferred(): ULONG;
begin

  Result := FBytesTransferred;
end;


constructor TMfCastLiveBuffer.Create();
begin

  inherited Create();

  FLock := TCriticalSection.Create;
  FBaseOffset := 0;
  FLength := 0;
  FLastWriteDebugLength := 0;
  FLastPruneDebugOffset := 0;
  FComplete := False;
  FClosed := False;
end;


destructor TMfCastLiveBuffer.Destroy();
begin

  FLock.Free;

  inherited Destroy;
end;


procedure TMfCastLiveBuffer.EnsureCapacity(const ARequired: UInt64);
var
  RequiredFromBase: UInt64;
  NewCapacity: NativeInt;

begin

  if (ARequired <= FBaseOffset) then
    Exit;

  RequiredFromBase := ARequired - FBaseOffset;

  if (RequiredFromBase <= UInt64(Length(FData))) then
    Exit;

  if (RequiredFromBase > UInt64(MaxInt)) then
    raise EOutOfMemory.Create('MfCast live buffer address range is too large');

  NewCapacity := Length(FData);
  if (NewCapacity < 65536) then
    NewCapacity := 65536;

  while UInt64(NewCapacity) < RequiredFromBase do
    begin
      if (NewCapacity > (MaxInt div 2)) then
        NewCapacity := MaxInt
      else
        NewCapacity := NewCapacity * 2;

      if (NewCapacity = MaxInt) and (UInt64(NewCapacity) < RequiredFromBase) then
        raise EOutOfMemory.Create('MfCast live buffer address range is too large');
    end;

  SetLength(FData,
            NewCapacity);
end;


procedure TMfCastLiveBuffer.DiscardBeforeLocked(const AOffset: UInt64);
const
  PruneGranularity = UInt64(8 * 1024 * 1024);

var
  NewBaseOffset: UInt64;
  DropBytes: UInt64;
  KeepBytes: UInt64;

begin

  if FComplete then
    Exit;

  if (AOffset <= FBaseOffset) then
    Exit;

  if (AOffset - FBaseOffset) < PruneGranularity then
    Exit;

  NewBaseOffset := AOffset;

  if (NewBaseOffset > FLength) then
    NewBaseOffset := FLength;

  DropBytes := NewBaseOffset - FBaseOffset;
  KeepBytes := FLength - NewBaseOffset;

  if (KeepBytes > 0) then
    Move(FData[NativeInt(DropBytes)],
         FData[0],
         NativeInt(KeepBytes));

  SetLength(FData,
            NativeInt(KeepBytes));
  FBaseOffset := NewBaseOffset;

  if (FBaseOffset = 0) or
     (FBaseOffset >= FLastPruneDebugOffset + PruneGranularity) then
    begin

      FLastPruneDebugOffset := FBaseOffset;
      OutputDebugString(PChar(Format('MfCast live buffer pruned base=%d kept=%d',
                                     [FBaseOffset, KeepBytes])));
    end;
end;


function TMfCastLiveBuffer.WriteAt(const AOffset: UInt64;
                                   ABuffer: Pointer;
                                   const ASize: Cardinal): HRESULT;
var
  Required: UInt64;

begin

  if (ABuffer = nil) and (ASize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  FLock.Acquire;
  try

    if FClosed then
      begin
        Result := E_ABORT;
        Exit;
      end;

    if AOffset < FBaseOffset then
      begin
        OutputDebugString(PChar(Format('MfCast live buffer write before base offset=%d base=%d',
                                       [AOffset, FBaseOffset])));
        Result := E_FAIL;
        Exit;
      end;

    Required := AOffset + ASize;
    if (Required < AOffset) then
      begin
        Result := E_INVALIDARG;
        Exit;
      end;

    try

      EnsureCapacity(Required);

      if (ASize > 0) then
        Move(ABuffer^,
             FData[NativeInt(AOffset - FBaseOffset)],
             ASize);
    except
      on E: EOutOfMemory do
        begin
          OutputDebugString(PChar(Format('MfCast live buffer out of memory offset=%d size=%d base=%d length=%d capacity=%d',
                                         [AOffset, ASize, FBaseOffset, FLength, Length(FData)])));
          Result := E_OUTOFMEMORY;
          Exit;
        end;
    end;

    if (Required > FLength) then
      FLength := Required;

    if (FLength > 0) and
       ((FLastWriteDebugLength = 0) or
        (FLength >= FLastWriteDebugLength + 262144)) then
      begin
        FLastWriteDebugLength := FLength;
        OutputDebugString(PChar(Format('MfCast live buffer bytes=%d',
                                       [FLength])));
      end;

    Result := S_OK;

  finally
    FLock.Release;
  end;
end;


function TMfCastLiveBuffer.ReadAt(const AOffset: UInt64;
                                  ABuffer: Pointer;
                                  const ABufferSize: Cardinal;
                                  out ABytesRead: Cardinal): HRESULT;
var
  Available: UInt64;

begin

  ABytesRead := 0;
  if (ABuffer = nil) and (ABufferSize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  FLock.Acquire;

  try
    if (AOffset < FBaseOffset) then
      begin
        OutputDebugString(PChar(Format('MfCast live buffer read before base offset=%d base=%d',
                                       [AOffset, FBaseOffset])));
        Result := E_FAIL;
        Exit;
      end;

    if (AOffset >= FLength) then
      begin
        Result := S_OK;
        Exit;
      end;

    Available := FLength - AOffset;
    if (Available > ABufferSize) then
      Available := ABufferSize;
    if (Available > 0) then
      begin
        Move(FData[NativeInt(AOffset - FBaseOffset)],
             ABuffer^,
             NativeInt(Available));
        ABytesRead := Cardinal(Available);
        DiscardBeforeLocked(AOffset + Available);
      end;
    Result := S_OK;
  finally
    FLock.Release;
  end;
end;


function TMfCastLiveBuffer.WaitForData(const AOffset: UInt64;
                                       const ATimeoutMs: Cardinal): HRESULT;
var
  StartTick: DWORD;
begin
  StartTick := GetTickCount();
  repeat
    FLock.Acquire;
    try
      if FClosed then
        begin
          Result := E_ABORT;
          Exit;
        end;
      if AOffset < FBaseOffset then
        begin
          Result := E_FAIL;
          Exit;
        end;
      if (AOffset < FLength) or FComplete then
        begin
          Result := S_OK;
          Exit;
        end;
    finally
      FLock.Release;
    end;

    if (ATimeoutMs > 0) and ((GetTickCount() - StartTick) >= ATimeoutMs) then
      begin
        Result := S_FALSE;
        Exit;
      end;
    Sleep(20);
  until False;
end;


function TMfCastLiveBuffer.GetLength(): UInt64;
begin

  FLock.Acquire;

  try
    Result := FLength;
  finally
    FLock.Release;
  end;
end;


function TMfCastLiveBuffer.SetLengthValue(const ALength: UInt64): HRESULT;
begin

  FLock.Acquire;

  try
    if FClosed then
      begin
        Result := E_ABORT;
        Exit;
      end;

    if (ALength < FBaseOffset) then
      begin
        SetLength(FData, 0);
        FBaseOffset := ALength;
      end
    else
      EnsureCapacity(ALength);

    FLength := ALength;
    Result := S_OK;

  finally
    FLock.Release;
  end;
end;


function TMfCastLiveBuffer.Flush(): HRESULT;
begin

  // WriteAt is synchronous. Taking the same lock is the commit barrier for
  // every byte written before IMFByteStream.Flush was called.
  FLock.Acquire;

  try
    if FClosed then
      Result := E_ABORT
    else
      Result := S_OK;
  finally
    FLock.Release;
  end;
end;


function TMfCastLiveBuffer.IsComplete(): Boolean;
begin

  FLock.Acquire;

  try
    Result := FComplete;
  finally
    FLock.Release;
  end;
end;


procedure TMfCastLiveBuffer.Complete();
begin

  FLock.Acquire;

  try
    FComplete := True;
  finally
    FLock.Release;
  end;
end;


procedure TMfCastLiveBuffer.Close();
begin

  FLock.Acquire;

  try
    FClosed := True;
    FComplete := True;
  finally
    FLock.Release;
  end;
end;


constructor TMfCastLiveByteStream.Create(const ABuffer: IMfCastLiveBuffer);
begin

 inherited Create();

  FBuffer := ABuffer;
  FPosition := 0;
  FClosed := False;
end;


function TMfCastLiveByteStream.GetCapabilities(out pdwCapabilities: DWord): HResult;
begin

  pdwCapabilities := MFBYTESTREAM_IS_READABLE or
                     MFBYTESTREAM_IS_WRITABLE or
                     MFBYTESTREAM_IS_SEEKABLE or
                     MFBYTESTREAM_DOES_NOT_USE_NETWORK;
  Result := S_OK;
end;


function TMfCastLiveByteStream.GetLength(out pqwLength: QWORD): HResult;
begin

  if Assigned(FBuffer) then
    pqwLength := FBuffer.GetLength()
  else
    pqwLength := 0;

  Result := S_OK;
end;


function TMfCastLiveByteStream.SetLength(qwLength: QWORD): HResult;
begin

  if not Assigned(FBuffer) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := FBuffer.SetLengthValue(qwLength);
end;


function TMfCastLiveByteStream.GetCurrentPosition(out pqwPosition: QWORD): HResult;
begin

  pqwPosition := FPosition;
  Result := S_OK;
end;


function TMfCastLiveByteStream.SetCurrentPosition(const qwPosition: QWORD): HResult;
begin

  FPosition := qwPosition;
  Result := S_OK;
end;


function TMfCastLiveByteStream.IsEndOfStream(out pfEndOfStream: BOOL): HResult;
begin

  pfEndOfStream := BOOL(FClosed or
                        (Assigned(FBuffer) and FBuffer.IsComplete() and
                         (FPosition >= FBuffer.GetLength())));
  Result := S_OK;
end;


function TMfCastLiveByteStream.Read(pb: PByte;
                                    cb: ULONG;
                                    out pcbRead: ULONG): HResult;
begin

  pcbRead := 0;

  if not Assigned(FBuffer) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := FBuffer.ReadAt(FPosition,
                           pb,
                           cb,
                           pcbRead);
  if SUCCEEDED(Result) then
    Inc(FPosition, pcbRead);
end;


function TMfCastLiveByteStream.BeginRead(pb: PByte;
                                         cb: ULONG;
                                         pCallback: IMFAsyncCallback;
                                         punkState: IUnknown): HResult;
begin
  Result := E_NOTIMPL;
end;


function TMfCastLiveByteStream.EndRead(pResult: IMFAsyncResult;
                                       out pcbRead: ULONG): HResult;
begin
  pcbRead := 0;
  Result := E_NOTIMPL;
end;


function TMfCastLiveByteStream.Write(pb: PByte;
                                     cb: ULONG;
                                     out pcbWritten: ULONG): HResult;
begin

  pcbWritten := 0;
  if not Assigned(FBuffer) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if FClosed then
    begin
      Result := E_ABORT;
      Exit;
    end;

  Result := FBuffer.WriteAt(FPosition,
                            pb,
                            cb);
  if SUCCEEDED(Result) then
    begin
      pcbWritten := cb;
      Inc(FPosition, cb);
    end;
end;


function TMfCastLiveByteStream.BeginWrite(pb: PByte;
                                          cb: ULONG;
                                          pCallback: IMFAsyncCallback;
                                          punkState: IUnknown): HResult;
var
  BytesWritten: ULONG;
  TransferState: IUnknown;
  AsyncResult: IMFAsyncResult;

begin

  BytesWritten := 0;
  Result := Write(pb,
                  cb,
                  BytesWritten);

  TransferState := TMfCastByteStreamAsyncState.Create(BytesWritten) as IUnknown;
  if SUCCEEDED(Result) then
    Result := MFCreateAsyncResult(TransferState,
                                  pCallback,
                                  punkState,
                                  AsyncResult);
  if SUCCEEDED(Result) then
    Result := AsyncResult.SetStatus(S_OK);

  if SUCCEEDED(Result) then
    Result := MFInvokeCallback(AsyncResult);
end;


function TMfCastLiveByteStream.EndWrite(pResult: IMFAsyncResult;
                                        out pcbWritten: ULONG): HResult;
var
  ResultObject: IUnknown;
  TransferState: IMfCastByteStreamAsyncState;

begin

  pcbWritten := 0;

  if not Assigned(pResult) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := pResult.GetObject(ResultObject);
  if FAILED(Result) then
    Exit;

  if Supports(ResultObject,
              IMfCastByteStreamAsyncState,
              TransferState) then
    begin
      pcbWritten := TransferState.GetBytesTransferred();
      Result := pResult.GetStatus();
    end
  else
    Result := E_UNEXPECTED;
end;


function TMfCastLiveByteStream.Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                                    llSeekOffset: LONGLONG;
                                    dwSeekFlags: DWord;
                                    out pqwCurrentPosition: QWORD): HResult;
var
  NewPosition: Int64;

begin

  if (SeekOrigin = msoCurrent) then
    NewPosition := Int64(FPosition) + llSeekOffset
  else
    NewPosition := llSeekOffset;

  if (NewPosition < 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  FPosition := UInt64(NewPosition);
  pqwCurrentPosition := FPosition;
  Result := S_OK;
end;


function TMfCastLiveByteStream.Flush(): HResult;
begin

  if FClosed then
    begin
      Result := E_ABORT;
      Exit;
    end;

  if not Assigned(FBuffer) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FBuffer.Flush();
end;


function TMfCastLiveByteStream.Close(): HResult;
begin
  FClosed := True;
  if Assigned(FBuffer) then
    FBuffer.Complete();
  Result := S_OK;
end;


constructor TMfCastLiveStreamContent.Create(const ABuffer: IMfCastLiveBuffer;
                                            const AContentType: string);
begin

  inherited Create();

  FBuffer := ABuffer;
  FContentType := AContentType;
end;


function TMfCastLiveStreamContent.GetContentType(): string;
begin

  Result := FContentType;
end;


function TMfCastLiveStreamContent.GetLength(out ALength: UInt64): HRESULT;
begin

  if Assigned(FBuffer) then
    ALength := FBuffer.GetLength()
  else
    ALength := 0;

  Result := S_OK;
end;


function TMfCastLiveStreamContent.CanSeek(): Boolean;
begin

  Result := False;
end;


function TMfCastLiveStreamContent.IsComplete(): Boolean;
begin

  Result := Assigned(FBuffer) and FBuffer.IsComplete();
end;


function TMfCastLiveStreamContent.ReadAt(const AOffset: UInt64;
                                         ABuffer: Pointer;
                                         const ABufferSize: Cardinal;
                                         out ABytesRead: Cardinal): HRESULT;
begin

  if Assigned(FBuffer) then
    Result := FBuffer.ReadAt(AOffset,
                             ABuffer,
                             ABufferSize,
                             ABytesRead)
  else
    begin
      ABytesRead := 0;
      Result := E_POINTER;
    end;
end;


function TMfCastLiveStreamContent.WaitForData(const AOffset: UInt64;
                                              const ATimeoutMs: Cardinal): HRESULT;
begin

  if Assigned(FBuffer) then
    Result := FBuffer.WaitForData(AOffset,
                                  ATimeoutMs)
  else
    Result := E_POINTER;
end;


function MfCastWinSockHResult(): HRESULT;
var
  ErrorCode: Integer;

begin

  ErrorCode := WSAGetLastError();

  if (ErrorCode = 0) then
    Result := E_FAIL
  else
    Result := HRESULT($80070000 or DWORD(ErrorCode));
end;


function MfCastHttpDecodePath(const APath: string): string;
var
  I: Integer;
  Hex: string;
  Value: Integer;

begin

  Result := '';
  I := 1;

  while I <= Length(APath) do
    begin
      if (APath[I] = '%') and (I + 2 <= Length(APath)) then
        begin
          Hex := Copy(APath, I + 1, 2);
          Value := StrToIntDef('$' + Hex, -1);
          if (Value >= 0) then
            begin
              Result := Result + Char(Value);
              Inc(I,
                  3);
              Continue;
            end;
        end;

      if (APath[I] = '+') then
        Result := Result + ' '
      else
        Result := Result + APath[I];
      Inc(I);
    end;
end;


function MfCastResolveLocalIPv4(out AAddress: string): Boolean;
var
  HostName: array[0..255] of AnsiChar;
  HostEntry: PHostEnt;
  I: Integer;
  Addr: PInAddr;

begin

  Result := False;
  AAddress := '';

  FillChar(HostName,
           SizeOf(HostName),
           0);

  if gethostname(@HostName[0],
                 SizeOf(HostName)) <> 0 then
    Exit;

  HostEntry := gethostbyname(@HostName[0]);
  if not Assigned(HostEntry) then
    Exit;

  I := 0;
  while Assigned(HostEntry^.h_addr_list[I]) do
    begin
      Addr := PInAddr(HostEntry^.h_addr_list[I]);
      if Assigned(Addr) and (Integer(Addr^.S_un_b.s_b1) <> 127) then
        begin
          AAddress := Format('%d.%d.%d.%d',
                             [Integer(Addr^.S_un_b.s_b1),
                              Integer(Addr^.S_un_b.s_b2),
                              Integer(Addr^.S_un_b.s_b3),
                              Integer(Addr^.S_un_b.s_b4)]);
          Result := True;
          Exit;
        end;
      Inc(I);
    end;
end;


function MfCastHttpStatusText(const AStatusCode: Integer): string;
begin

  case AStatusCode of
    200: Result := 'OK';
    206: Result := 'Partial Content';
    400: Result := 'Bad Request';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    416: Result := 'Range Not Satisfiable';
    500: Result := 'Internal Server Error';
  else
    Result := 'Error';
  end;
end;


function MfCastSendAll(const ASocket: TSocket; const ABuffer: Pointer;
                       const ASize: Integer): Boolean;
var
  Sent: Integer;
  TotalSent: Integer;
  Ptr: PAnsiChar;

begin

  Result := False;
  TotalSent := 0;
  Ptr := PAnsiChar(ABuffer);

  while (TotalSent < ASize) do
    begin
      Sent := send(ASocket,
                   Ptr[TotalSent],
                   ASize - TotalSent,
                   0);
      if (Sent <= 0) then
        Exit;

      Inc(TotalSent,
          Sent);
    end;
  Result := True;
end;


procedure MfCastSendText(const ASocket: TSocket;
                         const AText: AnsiString);
begin

  if (AText <> '') then
    MfCastSendAll(ASocket,
                  @AText[1],
                  Length(AText));
end;


procedure MfCastSendSimpleResponse(const ASocket: TSocket;
                                   const AStatusCode: Integer;
                                   const AMessage: string);
var
  Body: AnsiString;
  Header: AnsiString;

begin

  Body := AnsiString(AMessage + ULBR);
  Header := AnsiString('HTTP/1.1 ' + IntToStr(AStatusCode) + ' ' +
                       MfCastHttpStatusText(AStatusCode) + ULBR +
                       'Content-Type: text/plain; charset=utf-8' + ULBR +
                       'Content-Length: ' + IntToStr(Length(Body)) + ULBR +
                       'Connection: close' + ULBR + ULBR);

  MfCastSendText(ASocket,
                 Header);

  MfCastSendText(ASocket,
                 Body);
end;


function MfCastHeaderValue(const ARequest: string;
                           const AHeaderName: string): string;
var
  Lines: TStringList;
  I: Integer;
  Prefix: string;

begin

  Result := '';
  Lines := TStringList.Create;

  try

    Lines.Text := StringReplace(ARequest,
                                ULBR,
                                LFEED,
                                [rfReplaceAll]);

    Prefix := LowerCase(AHeaderName) + ':';

    for I := 0 to Lines.Count - 1 do
      if SameText(Copy(Trim(Lines[I]),
                       1,
                       Length(Prefix)),
                       Prefix) then
        begin
          Result := Trim(Copy(Trim(Lines[I]),
                              Length(Prefix) + 1,
                              MaxInt));
          Exit;
        end;
  finally
    Lines.Free;
  end;
end;


function MfCastParseRange(const ARangeHeader: string;
                          const ATotalLength: UInt64;
                          out AStart: UInt64;
                          out AEnd: UInt64): Boolean;
var
  RangeText: string;
  DashPos: Integer;
  StartText: string;
  EndText: string;
  StartValue: Int64;
  EndValue: Int64;

begin

  Result := False;
  AStart := 0;

  if (ATotalLength = 0) then
    AEnd := 0
  else
    AEnd := ATotalLength - 1;

  RangeText := Trim(ARangeHeader);

  if not SameText(Copy(RangeText,
                       1,
                       6),
                  'bytes=') then
    Exit;

  Delete(RangeText,
         1,
         6);

  DashPos := Pos('-',
                 RangeText);

  if (DashPos <= 0) then
    Exit;

  StartText := Trim(Copy(RangeText,
                         1,
                         DashPos - 1));

  EndText := Trim(Copy(RangeText,
                       DashPos + 1,
                       MaxInt));

  StartValue := StrToInt64Def(StartText,
                              -1);

  if (StartValue < 0) then
    Exit;
  AStart := UInt64(StartValue);

  if (EndText <> '') then
    begin

      EndValue := StrToInt64Def(EndText,
                                -1);

      if (EndValue < 0) then
        Exit;
      AEnd := UInt64(EndValue);
    end;

  Result := (ATotalLength > 0) and (AStart <= AEnd) and (AEnd < ATotalLength);
end;


constructor TMfCastHttpServerThread.Create(AOwner: TMfCastHttpServer);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


procedure TMfCastHttpServerThread.Execute;
begin

  if Assigned(FOwner) then
    FOwner.AcceptLoop();
end;


constructor TMfCastHttpServer.Create();
begin

  inherited Create;

  FResources := TDictionary<string, IMfCastHttpContent>.Create;
  FLock := TCriticalSection.Create();
  FRunning := False;
  FListenPort := 0;
  FListenSocket := INVALID_SOCKET;
  FClientSocket := INVALID_SOCKET;
  FServerThread := nil;
  FWSAStarted := False;
  FRequestCount := 0;
end;


destructor TMfCastHttpServer.Destroy();
begin

  Stop();
  FResources.Free();
  FLock.Free();

  inherited Destroy();
end;


function TMfCastHttpServer.Configure(const ASettings: TMfCastHttpSettings): HRESULT;
begin

  if FRunning then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  FSettings := ASettings;
  FSettings.BasePath := NormalizeBasePath(FSettings.BasePath);
  Result := S_OK;
end;


procedure TMfCastHttpServer.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastHttpServer.Start(): HRESULT;
var
  WsaData: TWSAData;
  BindAddr: TSockAddrIn;
  SockName: TSockAddr;
  SockNameLen: Integer;
  ReuseAddr: Integer;
  BindAddress: AnsiString;

begin

  if FRunning then
    begin
      Result := S_OK;
      Exit;
    end;

  if FSettings.UseTls then
    begin
      Result := E_NOTIMPL;
      Exit;
    end;

  if not FWSAStarted then
    begin
      if WSAStartup($0202,
                    WsaData) <> 0 then
        begin
          Result := MfCastWinSockHResult();
          Exit;
        end;

      FWSAStarted := True;
    end;

  FListenSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if (FListenSocket = INVALID_SOCKET) then
    begin
      Result := MfCastWinSockHResult();
      Exit;
    end;

  ReuseAddr := 1;

  setsockopt(FListenSocket,
             SOL_SOCKET,
             SO_REUSEADDR,
             PAnsiChar(@ReuseAddr),
             SizeOf(ReuseAddr));

  FillChar(BindAddr,
           SizeOf(BindAddr),
           0);

  BindAddr.sin_family := AF_INET;
  BindAddr.sin_port := htons(FSettings.ListenPort);

  if (Trim(FSettings.BindAddress) <> '') then
    begin
      BindAddress := AnsiString(Trim(FSettings.BindAddress));
      BindAddr.sin_addr.S_addr := inet_addr(PAnsiChar(BindAddress));
    end
  else
    BindAddr.sin_addr.S_addr := INADDR_ANY;

  if (bind(FListenSocket,
           TSockAddr(BindAddr),
           SizeOf(BindAddr)) = SOCKET_ERROR) then
    begin
      Result := MfCastWinSockHResult();
      WinApi.WinSock.closesocket(FListenSocket);
      FListenSocket := INVALID_SOCKET;

      Exit;
    end;

  if (listen(FListenSocket,
            SOMAXCONN) = SOCKET_ERROR) then
    begin
      Result := MfCastWinSockHResult();
      WinApi.WinSock.closesocket(FListenSocket);
      FListenSocket := INVALID_SOCKET;

      Exit;
    end;

  SockNameLen := SizeOf(SockName);
  FillChar(SockName,
           SizeOf(SockName),
           0);

  if (getsockname(FListenSocket,
                  SockName,
                  SockNameLen) = SOCKET_ERROR) then
    begin
      Result := MfCastWinSockHResult();
      WinApi.WinSock.closesocket(FListenSocket);
      FListenSocket := INVALID_SOCKET;
      Exit;
    end;

  FListenPort := ntohs(TSockAddrIn(SockName).sin_port);
  InterlockedExchange(FRequestCount,
                      0);
  FRunning := True;
  FServerThread := TMfCastHttpServerThread.Create(Self);

  Result := S_OK;
end;


function TMfCastHttpServer.Stop(): HRESULT;
var
  ClientSocket: TSocket;

begin

  FRunning := False;

  if (FListenSocket <> INVALID_SOCKET) then
    begin
      shutdown(FListenSocket,
               SD_BOTH);
      WinApi.WinSock.closesocket(FListenSocket);
      FListenSocket := INVALID_SOCKET;
    end;

  // HandleClient runs on the server thread. Closing its established socket
  // releases recv/send immediately so the thread can leave before WaitFor.
  ClientSocket := INVALID_SOCKET;
  FLock.Acquire;
  try
    if FClientSocket <> INVALID_SOCKET then
      begin
        ClientSocket := FClientSocket;
        FClientSocket := INVALID_SOCKET;
      end;
  finally
    FLock.Release;
  end;

  if ClientSocket <> INVALID_SOCKET then
    begin
      shutdown(ClientSocket, SD_BOTH);
      WinApi.WinSock.closesocket(ClientSocket);
    end;

  if Assigned(FServerThread) then
    begin
      FServerThread.WaitFor;
      FServerThread.Free;
      FServerThread := nil;
    end;

  FListenPort := 0;
  Result := S_OK;
end;


function TMfCastHttpServer.Publish(const AResourceName: string;
                                   const AContent: IMfCastHttpContent;
                                   out APublishedPath: string): HRESULT;
begin

  APublishedPath := '';

  if not Assigned(AContent) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  APublishedPath := CreateResourcePath(AResourceName);

  FLock.Acquire;

  try
    FResources.AddOrSetValue(APublishedPath, AContent);
  finally
    FLock.Release;
  end;

  Result := S_OK;
end;


function TMfCastHttpServer.Unpublish(const APublishedPath: string): HRESULT;
begin

  FLock.Acquire;

  try

    if FResources.ContainsKey(APublishedPath) then
      begin
        FResources.Remove(APublishedPath);
        Result := S_OK;
      end
    else
      Result := HRESULT_FROM_WIN32(ERROR_NOT_FOUND);
  finally
    FLock.Release;
  end;
end;


function TMfCastHttpServer.BuildUrl(const APublishedPath: string;
                                    out AUrl: string): HRESULT;
var
  Address: string;
  Scheme: string;

begin

  AUrl := '';

  Result := ResolveAdvertisedAddress(Address);
  if FAILED(Result) then
    Exit;

  if FSettings.UseTls then
    Scheme := 'https'
  else
    Scheme := 'http';

  AUrl := Scheme + '://' + Address + ':' +
          IntToStr(FListenPort) + APublishedPath;

  Result := S_OK;
end;


function TMfCastHttpServer.IsRunning: Boolean;
begin

  Result := FRunning;
end;


function TMfCastHttpServer.GetListenPort: Word;
begin

  Result := FListenPort;
end;


function TMfCastHttpServer.GetRequestCount(): Cardinal;
begin

  Result := Cardinal(InterlockedCompareExchange(FRequestCount,
                                                 0,
                                                 0));
end;


procedure TMfCastHttpServer.AcceptLoop();
var
  Client: TSocket;
  CloseClient: Boolean;

begin

  while FRunning do
    begin

      Client := accept(FListenSocket,
                       nil,
                       nil);

      if (Client = INVALID_SOCKET) then
        begin
          if FRunning then
            Sleep(10);
          Continue;
        end;

      CloseClient := False;
      FLock.Acquire;
      try
        if FRunning then
          FClientSocket := Client
        else
          CloseClient := True;
      finally
        FLock.Release;
      end;

      if CloseClient then
        begin
          shutdown(Client, SD_BOTH);
          WinApi.WinSock.closesocket(Client);
          Break;
        end;

      try
        HandleClient(Client);
      finally
        FLock.Acquire;
        try
          if FClientSocket = Client then
            begin
              FClientSocket := INVALID_SOCKET;
              shutdown(Client, SD_BOTH);
              WinApi.WinSock.closesocket(Client);
            end;
        finally
          FLock.Release;
        end;
      end;
    end;
end;


function TMfCastHttpServer.TryGetResource(const APath: string;
                                          out AContent: IMfCastHttpContent): Boolean;
begin

  FLock.Acquire;

  try
    Result := FResources.TryGetValue(APath,
                                     AContent);
  finally
    FLock.Release;
  end;
end;


procedure TMfCastHttpServer.HandleClient(const AClient: TSocket);
var
  Hr: HRESULT;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  Request: AnsiString;
  RequestChunk: AnsiString;
  RequestText: string;
  FirstLine: string;
  MethodName: string;
  TargetPath: string;
  SpacePos: Integer;
  QueryPos: Integer;
  Content: IMfCastHttpContent;
  LiveContent: IMfCastLiveHttpContent;
  TotalLength: UInt64;
  StartOffset: UInt64;
  EndOffset: UInt64;
  ChunkOffset: UInt64;
  ChunkSize: Cardinal;
  ChunkRead: Cardinal;
  Header: AnsiString;
  IsHead: Boolean;
  IsPartial: Boolean;
  RangeHeader: string;

begin

  Request := '';

  repeat
    BytesRead := recv(AClient,
                      Buffer,
                      SizeOf(Buffer),
                      0);

    if (BytesRead <= 0) then
      Exit;

    SetString(RequestChunk,
              PAnsiChar(@Buffer[0]),
              BytesRead);

    Request := Request + RequestChunk;

  until (Pos(ULBR + ULBR,
             string(Request)) > 0) or (Length(Request) > 16384);

  RequestText := string(Request);
  SpacePos := Pos(ULBR,
                  RequestText);

  if (SpacePos <= 0) then
    FirstLine := RequestText
  else
    FirstLine := Copy(RequestText,
                      1,
                      SpacePos - 1);

  OutputDebugString(PChar('MfCast HTTP request: ' + FirstLine));
  InterlockedIncrement(FRequestCount);

  SpacePos := Pos(' ',
                  FirstLine);

  if (SpacePos <= 0) then
    begin
      MfCastSendSimpleResponse(AClient,
                               400,
                               'Bad request');
      Exit;
    end;

  MethodName := UpperCase(Copy(FirstLine,
                               1,
                               SpacePos - 1));

  Delete(FirstLine,
         1,
         SpacePos);

  SpacePos := Pos(' ',
                  FirstLine);

  if (SpacePos <= 0) then
    TargetPath := FirstLine
  else
    TargetPath := Copy(FirstLine,
                       1,
                       SpacePos - 1);

  QueryPos := Pos('?',
                  TargetPath);

  if (QueryPos > 0) then
    TargetPath := Copy(TargetPath,
                       1,
                       QueryPos - 1);

  TargetPath := MfCastHttpDecodePath(TargetPath);

  IsHead := MethodName = 'HEAD';

  if (MethodName = 'OPTIONS') then
    begin
      Header := AnsiString('HTTP/1.1 204 No Content' + ULBR);

      if FSettings.EnableCors then
        Header := Header + MfCastCorsResponseHeaders();

      Header := Header + AnsiString('Content-Length: 0' + ULBR + 'Connection: close' + ULBR + ULBR);
      MfCastSendText(AClient,
                     Header);
      Exit;
    end;

  if (MethodName <> 'GET') and (not IsHead) then
    begin
      MfCastSendSimpleResponse(AClient,
                               405,
                               'Method not allowed');
      Exit;
    end;

  if not TryGetResource(TargetPath,
                        Content) then
    begin
      MfCastSendSimpleResponse(AClient,
                               404,
                               'Not found');
      Exit;
    end;

  if FAILED(Content.GetLength(TotalLength)) then
    begin
      MfCastSendSimpleResponse(AClient,
                               500,
                               'Could not read content length');
      Exit;
    end;

  StartOffset := 0;

  if (TotalLength = 0) then
    EndOffset := 0
  else
    EndOffset := TotalLength - 1;

  RangeHeader := MfCastHeaderValue(RequestText,
                                   'Range');
  IsPartial := False;

  if (RangeHeader <> '') and Content.IsComplete() then
    begin
      if not MfCastParseRange(RangeHeader,
                              TotalLength,
                              StartOffset,
                              EndOffset) then
        begin

          MfCastSendSimpleResponse(AClient,
                                   416,
                                   'Invalid range');
          Exit;
        end;

      IsPartial := True;
    end;

  if (not Content.IsComplete()) and (not IsHead) then
    begin
      if not Supports(Content,
                      IMfCastLiveHttpContent,
                      LiveContent) then
        begin
          MfCastSendSimpleResponse(AClient,
                                   500,
                                   'Incomplete resource is not live content');
          Exit;
        end;

      Header := AnsiString('HTTP/1.1 200 OK' + ULBR +
                'Content-Type: ' + Content.GetContentType() + ULBR +
                'Transfer-Encoding: chunked' + ULBR +
                'Cache-Control: no-store' + ULBR);

      if FSettings.EnableCors then
        Header := Header + MfCastCorsResponseHeaders();

      Header := Header + AnsiString('Connection: close' + ULBR + ULBR);

      MfCastSendText(AClient,
                     Header);
      OutputDebugString(PChar('MfCast HTTP live chunked start: ' + TargetPath));

      ChunkOffset := 0;

      while True do
        begin
          Hr := LiveContent.WaitForData(ChunkOffset,
                                           FSettings.IdleTimeoutMs);
          if FAILED(Hr) or ((Hr = S_FALSE) and (not Content.IsComplete())) then
            begin

              OutputDebugString(PChar(Format('MfCast HTTP live wait ended hr=%.8x offset=%d',
                                             [DWORD(Hr), ChunkOffset])));
              Exit;
            end;

          ChunkSize := SizeOf(Buffer);
          if FAILED(Content.ReadAt(ChunkOffset,
                                   @Buffer[0],
                                   ChunkSize,
                                   ChunkRead)) then
            Exit;

          if (ChunkRead = 0) then
            begin
              if Content.IsComplete() then
                Break;
              Continue;
            end;

          if (ChunkOffset = 0) or ((ChunkOffset mod 262144) < ChunkRead) then
            OutputDebugString(PChar(Format('MfCast HTTP live chunk offset=%d size=%d',
                                           [ChunkOffset, ChunkRead])));
          MfCastSendText(AClient,
                         AnsiString(IntToHex(ChunkRead, 1) + ULBR));

          if not MfCastSendAll(AClient,
                               @Buffer[0],
                               ChunkRead) then
            begin
              OutputDebugString(PChar(Format('MfCast HTTP live send failed offset=%d error=%d',
                                             [ChunkOffset, WSAGetLastError()])));
              Exit;
            end;

          MfCastSendText(AClient,
                         AnsiString(ULBR));

          Inc(ChunkOffset,
              ChunkRead);
        end;

      MfCastSendText(AClient,
                     AnsiString('0' + ULBR + ULBR));
      Exit;
    end;

  if IsPartial then
    Header := AnsiString('HTTP/1.1 206 Partial Content' + ULBR)
  else
    Header := AnsiString('HTTP/1.1 200 OK' + ULBR);

  Header := Header + AnsiString('Content-Type: ' + Content.GetContentType() + ULBR +
            'Accept-Ranges: bytes' + ULBR +
            'Content-Length: ' + IntToStr(EndOffset - StartOffset + 1) + ULBR +
            'Cache-Control: no-store, no-cache, must-revalidate' + ULBR +
            'Pragma: no-cache' + ULBR);

  if IsPartial then
    Header := Header + AnsiString('Content-Range: bytes ' + IntToStr(StartOffset) +
              '-' + IntToStr(EndOffset) + '/' + IntToStr(TotalLength) + ULBR);

  if FSettings.EnableCors then
    Header := Header + MfCastCorsResponseHeaders();

  Header := Header + AnsiString('Connection: close' + ULBR + ULBR);

  MfCastSendText(AClient,
                 Header);
  if IsHead then
    Exit;

  ChunkOffset := StartOffset;
  while (ChunkOffset <= EndOffset) do
    begin
      ChunkSize := SizeOf(Buffer);

      if (UInt64(ChunkSize) > (EndOffset - ChunkOffset + 1)) then
        ChunkSize := Cardinal(EndOffset - ChunkOffset + 1);

      if FAILED(Content.ReadAt(ChunkOffset,
                               @Buffer[0],
                               ChunkSize,
                               ChunkRead)) then
        Exit;

      if (ChunkRead = 0) then
        Exit;

      if not MfCastSendAll(AClient,
                           @Buffer[0],
                           ChunkRead) then
        Exit;

      Inc(ChunkOffset,
          ChunkRead);
    end;
end;


function TMfCastHttpServer.NormalizeBasePath(const APath: string): string;
begin

  Result := Trim(APath);

  if (Result = '') then
    Result := '/';

  if (Result[1] <> '/') then
    Result := '/' + Result;

  while (Length(Result) > 1) and (Result[Length(Result)] = '/') do
    Delete(Result,
           Length(Result),
           1);
end;


function TMfCastHttpServer.CreateResourcePath(const AResourceName: string): string;
begin

  Result := FSettings.BasePath + '/' + AResourceName;
end;


function TMfCastHttpServer.ResolveAdvertisedAddress(out AAddress: string): HRESULT;
begin

  AAddress := Trim(FSettings.AdvertisedAddress);

  if (AAddress = '') then
    AAddress := Trim(FSettings.BindAddress);
  if ((AAddress = '') or (AAddress = '0.0.0.0')) and
     (not MfCastResolveLocalIPv4(AAddress)) then
    AAddress := '127.0.0.1';

  Result := S_OK;
end;


constructor TMfCastFileContent.Create(const AFileName: string;
                                      const AContentType: string);
begin

  inherited Create;

  FFileName := AFileName;
  FContentType := AContentType;
end;


function TMfCastFileContent.GetContentType(): string;
begin

  Result := FContentType;
end;


function TMfCastFileContent.GetLength(out ALength: UInt64): HRESULT;
var
  Stream: TFileStream;

begin

  ALength := 0;

  if not FileExists(FFileName) then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  Stream := TFileStream.Create(FFileName,
                               fmOpenRead or fmShareDenyNone);

  try
    ALength := Stream.Size;
  finally
    Stream.Free;
  end;

  Result := S_OK;
end;


function TMfCastFileContent.CanSeek(): Boolean;
begin

  Result := True;
end;


function TMfCastFileContent.IsComplete(): Boolean;
begin

  Result := True;
end;


function TMfCastFileContent.ReadAt(const AOffset: UInt64;
                                   ABuffer: Pointer;
                                   const ABufferSize: Cardinal;
                                   out ABytesRead: Cardinal): HRESULT;
var
  Stream: TFileStream;

begin

  ABytesRead := 0;

  if (ABuffer = nil) and (ABufferSize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not FileExists(FFileName) then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  Stream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);

  try
    if (AOffset >= UInt64(Stream.Size)) then
      begin
        Result := S_OK;
        Exit;
      end;

    Stream.Position := AOffset;
    ABytesRead := Stream.Read(ABuffer^,
                              ABufferSize);
  finally
    Stream.Free;
  end;

  Result := S_OK;
end;


constructor TMfCastMemoryContent.Create(const AData: TBytes;
                                        const AContentType: string);
begin

  inherited Create();

  FData := Copy(AData,
                0,
                Length(AData));
  FContentType := AContentType;
end;


function TMfCastMemoryContent.GetContentType(): string;
begin

  Result := FContentType;
end;


function TMfCastMemoryContent.GetLength(out ALength: UInt64): HRESULT;
begin

  ALength := UInt64(Length(FData));
  Result := S_OK;
end;


function TMfCastMemoryContent.CanSeek(): Boolean;
begin

  Result := True;
end;


function TMfCastMemoryContent.IsComplete(): Boolean;
begin

  Result := True;
end;


function TMfCastMemoryContent.ReadAt(const AOffset: UInt64;
                                     ABuffer: Pointer;
                                     const ABufferSize: Cardinal;
                                     out ABytesRead: Cardinal): HRESULT;
var
  Remaining: UInt64;
  BytesToRead: Cardinal;

begin

  ABytesRead := 0;

  if (ABuffer = nil) and (ABufferSize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if (AOffset >= UInt64(Length(FData))) then
    begin
      Result := S_OK;
      Exit;
    end;

  Remaining := UInt64(Length(FData)) - AOffset;
  BytesToRead := ABufferSize;

  if (UInt64(BytesToRead) > Remaining) then
    BytesToRead := Cardinal(Remaining);

  if (BytesToRead > 0) then
    begin
      Move(FData[Integer(AOffset)],
                 ABuffer^,
                 BytesToRead);
      ABytesRead := BytesToRead;
    end;
  Result := S_OK;
end;


constructor TMfCastSegmentPublisher.Create(const AServer: IMfCastHttpServer);
begin

  inherited Create();

  FServer := AServer;
end;


destructor TMfCastSegmentPublisher.Destroy();
begin

  if Assigned(FBuffer) then
    FBuffer.Close();

  if Assigned(FServer) and (FEntryPath <> '') then
    FServer.Unpublish(FEntryPath);

  FByteStream := nil;
  FContent := nil;
  FBuffer := nil;
  FEntryPath := '';

  inherited Destroy;
end;


procedure TMfCastSegmentPublisher.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastSegmentPublisher.BeginPresentation(const AContentType: string;
                                                   out AEntryPath: string): HRESULT;
begin

  AEntryPath := '';

  if Assigned(FBuffer) then
    FBuffer.Close();

  if Assigned(FServer) and (FEntryPath <> '') then
    FServer.Unpublish(FEntryPath);

  FByteStream := nil;
  FContent := nil;
  FBuffer := nil;
  FEntryPath := '';

  if not Assigned(FServer) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  FBuffer := TMfCastLiveBuffer.Create();

  FContent := TMfCastLiveStreamContent.Create(FBuffer,
                                              AContentType);

  Result := FServer.Publish('stream.mp4',
                            FContent,
                            FEntryPath);
  if FAILED(Result) then
    begin
      FContent := nil;
      FBuffer := nil;
      FEntryPath := '';
      Exit;
    end;

  FByteStream := TMfCastLiveByteStream.Create(FBuffer) as IMFByteStream;

  AEntryPath := FEntryPath;
  OutputDebugString(PChar('MfCast stream published: ' + AEntryPath));
  Result := S_OK;
end;


function TMfCastSegmentPublisher.GetByteStream(out AByteStream: IMFByteStream): HRESULT;
begin

  AByteStream := FByteStream;

  if Assigned(AByteStream) then
    Result := S_OK
  else
    Result := E_POINTER;
end;


function TMfCastSegmentPublisher.CompletePresentation(): HRESULT;
begin

  if Assigned(FBuffer) then
    FBuffer.Complete();
  Result := S_OK;
end;


function TMfCastSegmentPublisher.AbortPresentation(const AReason: HRESULT): HRESULT;
begin

  if Assigned(FBuffer) then
    FBuffer.Close();

  if Assigned(FServer) and (FEntryPath <> '') then
    FServer.Unpublish(FEntryPath);

  FByteStream := nil;
  FContent := nil;
  FBuffer := nil;
  FEntryPath := '';

  Result := S_OK;
end;

end.

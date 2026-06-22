// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfIcecastBroadcastEngine.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: IceCast Engine only AAC output (yet).
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
//          Please, read documentation carefully!
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
// Source: FactoryX.Code.
// =============================================================================
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
unit MfIcecastBroadcastEngine;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Winsock2,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Math,
  System.Generics.Collections,
  System.NetEncoding,
  System.JSON,
  Winapi.WinInet,
  {ActiveX}
  Winapi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {Application}
  RDJ.Setup,
  RDJ.FilenameParser;


type

  TMfBroadcastState = (bsStopped,
                       bsConnecting,
                       bsLive,
                       bsReconnecting,
                       bsError);

  TMfPcmFloatBlock = record
    Data: TBytes;
    Frames: Integer;
    Channels: Integer;
    SampleRate: Integer;
  end;

  TMfBroadcastStateChangedEvent = procedure(Sender: TObject;
                                            NewState: TMfBroadcastState;
                                            const Msg: string) of object;

  TMfBroadcastErrorEvent = procedure(Sender: TObject;
                                     hr: HResult;
                                     const Msg: string) of object;

  TMfBroadcastEncoderBase = class(TObject)
  public

    function Initialize(const ASettings: TRDJBroadcastSetup): HRESULT; virtual; abstract;
    function EncodeInterleavedFloat32(const pData: PSingle;
                                      AFrames: Integer;
                                      out AOutBuf: TBytes): HRESULT; virtual; abstract;
    procedure Flush(out AOutBuf: TBytes); virtual; abstract;
  end;

  TMfBroadcastEncoderNull = class(TMfBroadcastEncoderBase)

  public

    function Initialize(const ASettings: TRDJBroadcastSetup): HRESULT; override;
    function EncodeInterleavedFloat32(const pData: PSingle;
                                      AFrames: Integer;
                                      out AOutBuf: TBytes): HRESULT; override;
    procedure Flush(out AOutBuf: TBytes); override;
  end;



  TIcecastSourceClient = class
  private

    FSocket: TSocket;
    FConnected: Boolean;
    FWinsockReady: Boolean;
    FUseChunkedTransfer: Boolean;
    FBytesSent: Int64;
    FHost: string;
    FPort: Integer;
    FMount: string;
    FUsername: string;
    FPassword: string;

    function EnsureWinsock: Boolean;
    function SendAll(const AData;
                     ASize: Integer): Boolean;
    function RecvHeader(out AHeader: AnsiString): Boolean;
    function BuildBasicAuth(const AUser,
                            APassword: string): AnsiString;

    function ParseStatusCode(const AHeader: AnsiString): Integer;

    function OpenSocket(const ASettings: TRDJBroadcastSetup): Boolean;
    function BuildLegacySourceRequest(const ASettings: TRDJBroadcastSetup;
                                      const AContentType: string): AnsiString;

    function BuildPutRequest(const ASettings: TRDJBroadcastSetup;
                             const AContentType: string): AnsiString;

  public

    constructor Create();
    destructor Destroy(); override;

    function Connect(const ASettings: TRDJBroadcastSetup;
                     const AContentType: string): HRESULT;

    procedure Disconnect(); virtual;
    function SendData(const AData;
                      ASize: Integer): HRESULT; virtual;
    function SendDataParts(const AData1;
                           ASize1: Integer;
                           const AData2;
                           ASize2: Integer): HRESULT; virtual;
    function UpdateMetadata(const ASong: string): HRESULT; virtual;

    property Connected: Boolean read FConnected;
    property BytesSent: Int64 read FBytesSent;

  end;


  TMfPcmBlockQueue = class
  private

    FLock: TCriticalSection;
    FEvent: TEvent;
    FItems: array of TMfPcmFloatBlock;
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FClosed: Boolean;

  public

    constructor Create(ACapacity: Integer);
    destructor Destroy(); override;

    procedure Clear();
    procedure Close();
    function Push(const ABlock: TMfPcmFloatBlock): Boolean;
    function Pop(out ABlock: TMfPcmFloatBlock;
                 ATimeoutMs: Cardinal): Boolean;
    function Count(): Integer;
  end;


  TMfIcecastBroadcastEngine = class;

  TMfIcecastBroadcastWorker = class(TThread)
  private

    FOwner: TMfIcecastBroadcastEngine;
  protected

    procedure Execute; override;
  public

    constructor Create(AOwner: TMfIcecastBroadcastEngine);
  end;


  TMfIcecastBroadcastEngine = class(TComponent)
  private

    FSettings: TRDJBroadcastSetup;
    FState: TMfBroadcastState;
    FOnStateChanged: TMfBroadcastStateChangedEvent;
    FOnError: TMfBroadcastErrorEvent;

    FQueue: TMfPcmBlockQueue;
    FEncoder: TMfBroadcastEncoderBase;
    FClient: TIcecastSourceClient;
    FWorker: TMfIcecastBroadcastWorker;

    FGainMul: Single;
    FLastNowPlaying: string;

    FDropCount: Integer;
    FLastDropTick: Cardinal;

    FFileNameParser: TFileNameParser;

    FListenerCount: Integer;

    procedure SetState(ANewState: TMfBroadcastState;
                       const AMsg: string);
    procedure DoError(Ahr: HResult;
                      const AMsg: string);
    function CreateEncoder(): TMfBroadcastEncoderBase;
    function ContentTypeFromCodec(): string;
    procedure FreeRuntimeObjects();
    procedure ApplyGainToInterleavedStereoFloat32(pData: PSingle;
                                                  AFrames: Integer);
    function BuildSilenceBlock(const ADurationMs: Integer;
                               out ABlock: TMfPcmFloatBlock): Boolean;

    function GetIcecastListenerCount(const AMount: string): Integer;

  protected

    function WorkerConnect(): HRESULT;
    procedure WorkerDisconnect();
    procedure WorkerLoop();

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function Start(): HRESULT;
    procedure Stop();

    function PushPcmFloat32(const pData: PSingle;
                            const AFrames: Integer;
                            const pwfx: PWAVEFORMATEX): HRESULT;

    procedure UpdateNowPlaying(const AArtist: string;
                               ATitle: string;
                               ADefault: string = 'RDJ Broadcast Live');
    procedure UpdateNowPlayingFromFileName(const AFileName: string);

    property Settings: TRDJBroadcastSetup read FSettings write FSettings;
    property State: TMfBroadcastState read FState;

  published

    property OnStateChanged: TMfBroadcastStateChangedEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TMfBroadcastErrorEvent read FOnError write FOnError;
  end;


implementation

uses
  MfBroadcastEncoderAac,
  frmMainMdi;

const

  CLSID_CMSAACEncMFT: TGUID = '{93AF0C51-2275-45D2-A35B-F2BA21CAED00}';

// Helpers
function ClampInt(const X,
                       AMin,
                       AMax: Integer): Integer; inline;
begin

  if (X < AMin) then
    Exit(AMin);
  if (X > AMax) then
    Exit(AMax);
  Result := X;
end;


function DbToMul(const ADb: Single): Single; inline;
begin

  Result := Power(10.0,
                  ADb / 20.0);
end;


function IsSilentStereoFloat32(const pData: PSingle;
                               const AFrames: Integer;
                               const AEpsilon: Single = 1.0E-8 {Note: reduce 1.0E-8 to 1.0E-6 or 1.0E-5 to fine tune near silence}): Boolean;
var
  I: Integer;
  P: PSingle;
  SampleCount: Integer;

begin

  Result := True;

  if (pData = nil) or (AFrames <= 0) then
    Exit;

  P := pData;
  SampleCount := AFrames * 2; // stereo interleaved

  for I := 0 to SampleCount - 1 do
    begin

      if (Abs(P^) > AEpsilon) then
        Exit(False);
      Inc(P);
    end;
end;

// Helpers end -----------------------------------------------------------------


{ TMfBroadcastEncoderNull }

function TMfBroadcastEncoderNull.Initialize(const ASettings: TRDJBroadcastSetup): HRESULT;
begin

  Result := S_OK;
end;


function TMfBroadcastEncoderNull.EncodeInterleavedFloat32(const pData: PSingle;
                                                          AFrames: Integer;
                                                          out AOutBuf: TBytes): HRESULT;
begin

  SetLength(AOutBuf,
            0);
  Result := S_FALSE;
end;


procedure TMfBroadcastEncoderNull.Flush(out AOutBuf: TBytes);
begin

  SetLength(AOutBuf,
            0);
end;


{ TIcecastSourceClient }

constructor TIcecastSourceClient.Create;
begin

  inherited Create;

  FSocket := INVALID_SOCKET;
  FWinsockReady := False;
end;


destructor TIcecastSourceClient.Destroy();
begin

  Disconnect();

  if FWinsockReady then
    begin

      WSACleanup();
      FWinsockReady := False;
    end;

  inherited;
end;


function TIcecastSourceClient.EnsureWinsock(): Boolean;
var
  WsaData: TWSAData;

begin

  if FWinsockReady then
    Exit(True);

  Result := (WSAStartup($0202,
                        WsaData) = 0);
  FWinsockReady := Result;
end;


function _Base64EncodeBytes(const AData: TBytes): AnsiString;
const
  CTable: PAnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

var
  I: Integer;
  B0,
  B1,
  B2: Integer;
  OutLen: Integer;
  P: PAnsiChar;

begin

  if Length(AData) = 0 then
    Exit('');

  OutLen := ((Length(AData) + 2) div 3) * 4;
  SetLength(Result, OutLen);
  P := PAnsiChar(Result);
  I := 0;

  while (I < Length(AData)) do
    begin

      B0 := AData[I];
      Inc(I);

      if (I < Length(AData)) then
        begin

          B1 := AData[I];
          Inc(I);
        end
      else
        B1 := -1;

      if (I < Length(AData)) then
        begin

          B2 := AData[I];
          Inc(I);
        end
      else
        B2 := -1;

      P^ := CTable[(B0 shr 2) + 1];
      Inc(P);

      if (B1 >= 0) then
        P^ := CTable[(((B0 and $03) shl 4) or (B1 shr 4)) + 1]
      else
        P^ := CTable[((B0 and $03) shl 4) + 1];

      Inc(P);

      if (B1 >= 0) then
        begin

          if (B2 >= 0) then
            P^ := CTable[(((B1 and $0F) shl 2) or (B2 shr 6)) + 1]
          else
            P^ := CTable[((B1 and $0F) shl 2) + 1];
        end
      else
        P^ := '=';

      Inc(P);

      if (B2 >= 0) then
        P^ := CTable[(B2 and $3F) + 1]
      else
        P^ := '=';
      Inc(P);
    end;
end;


function TIcecastSourceClient.BuildBasicAuth(const AUser,
                                             APassword: string): AnsiString;
var
  Raw: TBytes;
  Enc: string;

begin

  Raw := TEncoding.ASCII.GetBytes(AUser + ':' + APassword);
  Enc := TNetEncoding.Base64.EncodeBytesToString(Raw);
  Enc := StringReplace(Enc, sLineBreak, '', [rfReplaceAll]);
  Enc := StringReplace(Enc, #13, '', [rfReplaceAll]);
  Enc := StringReplace(Enc, #10, '', [rfReplaceAll]);
  Result := AnsiString('Basic ' + Enc);
end;


function TIcecastSourceClient.SendAll(const AData;
                                      ASize: Integer): Boolean;
var
  P: PAnsiChar;
  SentNow: Integer;
  Left: Integer;

begin

  if (FSocket = INVALID_SOCKET) or (ASize <= 0) then
    Exit(False);

  P := @AData;
  Left := ASize;

  while (Left > 0) do
    begin

      SentNow := send(FSocket,
                      P^,
                      Left,
                      0);
      if (SentNow = SOCKET_ERROR) then
        Exit(False);

      Inc(P,
          SentNow);
      Dec(Left,
          SentNow);
    end;

  Result := True;
end;


function TIcecastSourceClient.RecvHeader(out AHeader: AnsiString): Boolean;
var

  Buf: array[0..2047] of AnsiChar;
  N: Integer;
  Chunk: AnsiString;
  Err: Integer;

begin

  AHeader := '';

  repeat

    N := recv(FSocket,
              Buf,
              SizeOf(Buf),
              0);

    if (N > 0) then
      begin

        SetString(Chunk,
                  PAnsiChar(@Buf[0]),
                  N);

        AHeader := AHeader + Chunk;

        if Pos(#13#10#13#10,
               string(AHeader)) > 0 then
          begin

            Result := True;
            Exit;
          end;

        Continue;
      end;

    if (N = 0) then
      begin

        // orderly close
        Exit(False);
      end;

    Err := WSAGetLastError();

    // timeout / would block: no header arrived in time
    if (Err = WSAETIMEDOUT) or
       (Err = WSAEWOULDBLOCK) then
      begin

        Exit(False);
      end;

    Exit(False);
  until False;
end;


function TIcecastSourceClient.ParseStatusCode(const AHeader: AnsiString): Integer;
var
  S: string;

begin

  Result := 0;

  S := UpperCase(Trim(string(AHeader)));

  if Pos('HTTP/1.0 100',
         S) = 1 then
    Exit(100);
  if Pos('HTTP/1.1 100',
         S) = 1 then
    Exit(100);

  if Pos('HTTP/1.0 200',
         S) = 1 then
    Exit(200);
  if Pos('HTTP/1.1 200',
         S) = 1 then
    Exit(200);

  if Pos('HTTP/1.0 401',
         S) = 1 then
    Exit(401);
  if Pos('HTTP/1.1 401',
         S) = 1 then
    Exit(401);

  if Pos('HTTP/1.0 403',
         S) = 1 then
    Exit(403);
  if Pos('HTTP/1.1 403',
         S) = 1 then
    Exit(403);
end;


function TIcecastSourceClient.OpenSocket(const ASettings: TRDJBroadcastSetup): Boolean;
var
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  IpAnsi: AnsiString;
  HostAnsi: AnsiString;
  TimeOutMs: Integer;

begin

  Result := False;

  FSocket := socket(AF_INET,
                    SOCK_STREAM,
                    IPPROTO_TCP);

  if (FSocket = INVALID_SOCKET) then
    Exit;

  Addr := Default(TSockAddrIn);

  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(ASettings.Port);

  IpAnsi := AnsiString(ASettings.Host);
  Addr.sin_addr.S_addr := inet_addr(PAnsiChar(IpAnsi));

  if (Addr.sin_addr.S_addr = INADDR_NONE) then
    begin

      HostAnsi := AnsiString(ASettings.Host);
      HostEnt := gethostbyname(PAnsiChar(HostAnsi));
      if (HostEnt = nil) then
        Exit(False);

      Move(HostEnt^.h_addr_list^^,
           Addr.sin_addr,
           HostEnt^.h_length);
    end;

  if Winapi.Winsock2.connect(FSocket,
                             PSockAddr(@Addr)^,
                             SizeOf(Addr)) <> 0 then
    Exit(False);

  TimeOutMs := 1500;
  setsockopt(FSocket,
             SOL_SOCKET,
             SO_RCVTIMEO,
             PAnsiChar(@TimeOutMs),
             SizeOf(TimeOutMs));

  TimeOutMs := 1500;
  setsockopt(FSocket,
             SOL_SOCKET,
             SO_SNDTIMEO,
             PAnsiChar(@TimeOutMs),
             SizeOf(TimeOutMs));

  Result := True;

end;


function TIcecastSourceClient.BuildLegacySourceRequest(const ASettings: TRDJBroadcastSetup;
                                                       const AContentType: string): AnsiString;

var
  Auth: AnsiString;
  Mount: string;

begin

  Mount := Trim(ASettings.Mount);
  if (Mount = '') then
    Mount := '/live'
  else
    if (Mount[1] <> '/') then
      Mount := '/' + Mount;

  Auth := BuildBasicAuth(ASettings.Username,
                         ASettings.Password);

  Result := 'SOURCE ' + AnsiString(Mount) + ' ICE/1.0' + #13#10 +
            'Authorization: ' + Auth + #13#10 +
            'Content-Type: ' + AnsiString(AContentType) + #13#10 +
            'Ice-Name: ' + AnsiString(ASettings.StreamName) + #13#10 +
            'Ice-Description: ' + AnsiString(ASettings.Description) + #13#10 +
            'Ice-Genre: ' + AnsiString(ASettings.Genre) + #13#10 +
            'Ice-URL: ' + AnsiString(ASettings.Url) + #13#10 +
            'Ice-Public: ' + AnsiString(IntToStr(Ord(ASettings.PublicStream))) + #13#10 +
            'User-Agent: RDJ/1.0' + #13#10 + #13#10;

end;


function TIcecastSourceClient.BuildPutRequest(const ASettings: TRDJBroadcastSetup;
                                              const AContentType: string): AnsiString;
var
  Auth: AnsiString;
  Mount: string;

begin

  Mount := Trim(ASettings.Mount);

  if (Mount = '') then
    Mount := '/live'
  else
    if (Mount[1] <> '/') then
      Mount := '/' + Mount;

  Auth := BuildBasicAuth(ASettings.Username,
                         ASettings.Password);

  Result := 'PUT ' + AnsiString(Mount) + ' HTTP/1.1'#13#10 +
            'Host: ' + AnsiString(ASettings.Host) + ':' + AnsiString(IntToStr(ASettings.Port)) + #13#10 +
            'Authorization: ' + Auth + #13#10 +
            'Content-Type: ' + AnsiString(AContentType) + #13#10 +
            'User-Agent: RDJ/1.0'#13#10 +
            'Ice-Name: ' + AnsiString(ASettings.StreamName) + #13#10 +
            'Ice-Description: ' + AnsiString(ASettings.Description) + #13#10 +
            'Ice-Genre: ' + AnsiString(ASettings.Genre) + #13#10 +
            'Ice-URL: ' + AnsiString(ASettings.Url) + #13#10 +
            'Ice-Public: ' + AnsiString(IntToStr(Ord(ASettings.PublicStream))) + #13#10 +
            'Transfer-Encoding: chunked'#13#10 +
            'Expect: 100-continue'#13#10 + #13#10;
end;


function TIcecastSourceClient.Connect(const ASettings: TRDJBroadcastSetup;
                                      const AContentType: string): HRESULT;
var
  Resp: AnsiString;
  Code: Integer;
  Req: AnsiString;

begin

  Disconnect;

  if not EnsureWinsock() then
    Exit(E_FAIL);

  FHost := ASettings.Host;
  FPort := ASettings.Port;
  FMount := ASettings.Mount;
  FUsername := ASettings.Username;
  FPassword := ASettings.Password;

  // Preferred path: legacy SOURCE stream login with raw continuous bytes.
  if not OpenSocket(ASettings) then
    Exit(E_FAIL);

  Req := BuildLegacySourceRequest(ASettings,
                                  AContentType);

  if (Req = '') or (not SendAll(Req[1],
                                Length(Req))) then
    begin

      Disconnect;
      Exit(E_FAIL);
    end;

  Resp := '';

  if RecvHeader(Resp) then
    begin

      Code := ParseStatusCode(Resp);

      if (Code = 200) then
        begin

          FConnected := True;
          FUseChunkedTransfer := False;
          Result := S_OK;
          Exit;
        end;

      if (Code = 401) or (Code = 403) then
        begin

          Disconnect;
          // continue below with PUT fallback, because some Icecast builds
          // accept PUT but reject SOURCE-style login.
        end
      else
        begin

          Disconnect;
          // continue below with PUT fallback
        end;
    end
  else
    begin

      // No immediate header: for SOURCE live streaming this can still be OK.
      FConnected := True;
      FUseChunkedTransfer := False;
      Result := S_OK;
      Exit;
    end;

  // Fallback path: HTTP PUT with chunked transfer.
  if not OpenSocket(ASettings) then
    Exit(E_FAIL);

  Req := BuildPutRequest(ASettings,
                         AContentType);

  if (Req = '') or (not SendAll(Req[1], Length(Req))) then
    begin

      Disconnect;
      Exit(E_FAIL);
    end;

  Resp := '';

  if not RecvHeader(Resp) then
    begin

      Disconnect;
      Exit(E_FAIL);
    end;

  Code := ParseStatusCode(Resp);

  if (Code = 100) or (Code = 200) then
    begin

      FConnected := True;
      FUseChunkedTransfer := True;
      Result := S_OK;
      Exit;
    end;

  if (Code = 401) or (Code = 403) then
    begin

      Disconnect;
      Result := HRESULT_FROM_WIN32(ERROR_ACCESS_DENIED);
      Exit;
    end;

  Disconnect;
  Result := E_FAIL;
end;


procedure TIcecastSourceClient.Disconnect();
begin

  if (FSocket <> INVALID_SOCKET) then
    begin

      shutdown(FSocket,
               SD_BOTH);
      closesocket(FSocket);
      FSocket := INVALID_SOCKET;
    end;

  FUseChunkedTransfer := False;
  FConnected := False;
end;


function TIcecastSourceClient.SendData(const AData;
                                       ASize: Integer): HRESULT;
var
  ChunkHdr: AnsiString;
  ChunkEnd: AnsiString;

begin

  Result := E_FAIL;

  if not FConnected then
    Exit;

  if (ASize <= 0) then
    Exit(S_FALSE);

  if FUseChunkedTransfer then
    begin

      ChunkHdr := AnsiString(IntToHex(ASize,
                                      1) + #13#10);
      ChunkEnd := #13#10;

      if not SendAll(ChunkHdr[1],
                     Length(ChunkHdr)) then
        begin

          Disconnect();
          Exit(E_FAIL);
        end;

    if not SendAll(AData,
                   ASize) then
      begin

        Disconnect();
        Exit(E_FAIL);
      end;

    if not SendAll(ChunkEnd[1],
                   Length(ChunkEnd)) then
      begin

        Disconnect();
        Exit(E_FAIL);
      end;
    end
  else
    begin

      if not SendAll(AData,
                     ASize) then
        begin

          Disconnect();
          Exit(E_FAIL);
        end;
    end;

  Inc(FBytesSent,
      ASize);

  Result := S_OK;
end;


function TIcecastSourceClient.SendDataParts(const AData1;
                                            ASize1: Integer;
                                            const AData2;
                                            ASize2: Integer): HRESULT;
var
  TotalSize: Integer;
  ChunkHdr: AnsiString;
  ChunkEnd: AnsiString;
begin
  Result := E_FAIL;
  if not FConnected then
    Exit;

  TotalSize := ASize1 + ASize2;
  if (TotalSize <= 0) then
    Exit(S_FALSE);

  if FUseChunkedTransfer then
  begin
    ChunkHdr := AnsiString(IntToHex(TotalSize,
                                    1) + #13#10);
    ChunkEnd := #13#10;

    if not SendAll(ChunkHdr[1],
                   Length(ChunkHdr)) then
    begin
      Disconnect();
      Exit(E_FAIL);
    end;

    if (ASize1 > 0) and not SendAll(AData1,
                                    ASize1) then
    begin
      Disconnect();
      Exit(E_FAIL);
    end;

    if (ASize2 > 0) and not SendAll(AData2,
                                    ASize2) then
    begin
      Disconnect();
      Exit(E_FAIL);
    end;

    if not SendAll(ChunkEnd[1],
                   Length(ChunkEnd)) then
    begin
      Disconnect();
      Exit(E_FAIL);
    end;
  end
  else
  begin
    if (ASize1 > 0) and not SendAll(AData1,
                                    ASize1) then
    begin
      Disconnect();
      Exit(E_FAIL);
    end;

    if (ASize2 > 0) and not SendAll(AData2,
                                    ASize2) then
    begin
      Disconnect();
      Exit(E_FAIL);
    end;
  end;

  Inc(FBytesSent,
      TotalSize);

  Result := S_OK;
end;


function _UrlEncode(const S: UTF8String): AnsiString;
var
  I: Integer;
  B: Byte;

begin

  Result := '';

  for I := 1 to Length(S) do
    begin

      B := Ord(S[I]);

      if ((B >= Ord('A')) and (B <= Ord('Z'))) or
         ((B >= Ord('a')) and (B <= Ord('z'))) or
         ((B >= Ord('0')) and (B <= Ord('9'))) or
         (B = Ord('-')) or
         (B = Ord('_')) or
         (B = Ord('.')) or
         (B = Ord('~')) then
        Result := Result + AnsiChar(B)
      else if B = Ord(' ') then
        Result := Result + '+'
      else
        Result := Result + '%' + AnsiString(IntToHex(B, 2));
    end;
end;


function TIcecastSourceClient.UpdateMetadata(const ASong: string): HRESULT;
var
  MetaSocket: TSocket;
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  HostAnsi: AnsiString;
  IpAnsi: AnsiString;
  TimeOutMs: Integer;
  Mount: string;
  SongUtf8: UTF8String;
  SongEnc: AnsiString;
  Req: AnsiString;
  Resp: AnsiString;
  Auth: AnsiString;
  Code: Integer;

  function SendAllLocal(const AData;
                        ASize: Integer): Boolean;
  var
    P: PAnsiChar;
    SentNow: Integer;
    Remaining: Integer;

  begin

    P := PAnsiChar(@AData);
    Remaining := ASize;

    while (Remaining > 0) do
      begin

        SentNow := send(MetaSocket,
                        P^,
                        Remaining,
                        0);
        if SentNow <= 0 then
          Exit(False);

        Inc(P, SentNow);
        Dec(Remaining,
            SentNow);
      end;

    Result := True;
  end;

  function RecvHeaderLocal(out AHeader: AnsiString): Boolean;
  var
    Buf: array[0..2047] of AnsiChar;
    R: Integer;
    S: AnsiString;

  begin

    AHeader := '';
    S := '';

    repeat

      R := recv(MetaSocket,
                Buf,
                SizeOf(Buf),
                0);

      if (R <= 0) then
        Exit(False);

      SetString(AHeader,
                Buf,
                R);
      S := S + AHeader;

      if (Pos(#13#10#13#10,
              string(S)) > 0) then
        begin

          AHeader := S;
          Exit(True);
        end;
    until False;
  end;

// Main body
begin

  Result := E_FAIL;

  Mount := Trim(FMount);
  if (Mount = '') then
    Mount := '/live'
  else
    if (Mount[1] <> '/') then
      Mount := '/' + Mount;

  MetaSocket := socket(AF_INET,
                       SOCK_STREAM,
                       IPPROTO_TCP);
  if (MetaSocket = INVALID_SOCKET) then
    Exit;

  try

    //FillChar(Addr,
    //         SizeOf(Addr),
    //         0);
    Addr := Default(TSockAddrIn);

    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FPort);

    IpAnsi := AnsiString(FHost);
    Addr.sin_addr.S_addr := inet_addr(PAnsiChar(IpAnsi));

    if (Addr.sin_addr.S_addr = INADDR_NONE) then
      begin

        HostAnsi := AnsiString(FHost);
        HostEnt := gethostbyname(PAnsiChar(HostAnsi));

        if (HostEnt = nil) then
          Exit;

        Move(HostEnt^.h_addr_list^^,
             Addr.sin_addr,
             HostEnt^.h_length);
      end;

    if Winapi.Winsock2.connect(MetaSocket,
                               PSockAddr(@Addr)^,
                               SizeOf(Addr)) <> 0 then
      Exit;

    TimeOutMs := 1500;

    setsockopt(MetaSocket,
               SOL_SOCKET,
               SO_RCVTIMEO,
               PAnsiChar(@TimeOutMs),
               SizeOf(TimeOutMs));

    TimeOutMs := 1500;

    setsockopt(MetaSocket,
               SOL_SOCKET,
               SO_SNDTIMEO,
               PAnsiChar(@TimeOutMs),
               SizeOf(TimeOutMs));

    SongUtf8 := UTF8String(ASong);
    SongEnc := _UrlEncode(SongUtf8);
    Auth := BuildBasicAuth(FUsername,
                           FPassword);

    Req := 'GET /admin/metadata?mount=' + AnsiString(Mount) +
           '&mode=updinfo&song=' + SongEnc + ' HTTP/1.0' + #13#10 +
           'Host: ' + AnsiString(FHost) + ':' + AnsiString(IntToStr(FPort)) + #13#10 +
           'Authorization: ' + Auth + #13#10 +
           'User-Agent: RDJ/1.0' + #13#10 +
           'Connection: close' + #13#10 + #13#10;

    if not SendAllLocal(Req[1],
                        Length(Req)) then
      Exit;

    if not RecvHeaderLocal(Resp) then
      Exit;

    Code := ParseStatusCode(Resp);
    if (Code = 200) then
      Result := S_OK
    else
      if (Code = 401) then
        Result := E_ACCESSDENIED
      else
        Result := E_FAIL;

  finally

    if (MetaSocket <> INVALID_SOCKET) then
      closesocket(MetaSocket);
  end;
end;


{ TMfPcmBlockQueue }

constructor TMfPcmBlockQueue.Create(ACapacity: Integer);
begin

  inherited Create;

  FLock := TCriticalSection.Create();
  FEvent := TEvent.Create(nil,
                          False,
                          False,
                          '');
  FCapacity := Max(8,
                   ACapacity);
  SetLength(FItems,
            FCapacity);
end;


destructor TMfPcmBlockQueue.Destroy();
begin

  Close();
  Clear();
  FreeAndNil(FEvent);
  FreeAndNil(FLock);

  inherited;
end;


procedure TMfPcmBlockQueue.Clear();
var
  I: Integer;

begin

  FLock.Acquire;

  try

    for I := 0 to High(FItems) do
      FItems[I] := Default(TMfPcmFloatBlock);

    FHead := 0;
    FTail := 0;
    FCount := 0;
  finally

    FLock.Release;
  end;
end;


procedure TMfPcmBlockQueue.Close();
begin

  FLock.Acquire;

  try

    FClosed := True;
    FEvent.SetEvent;
  finally

    FLock.Release;
  end;
end;


function TMfPcmBlockQueue.Count(): Integer;
begin

  FLock.Acquire;

  try

    Result := FCount;
  finally

    FLock.Release;
  end;
end;


function TMfPcmBlockQueue.Push(const ABlock: TMfPcmFloatBlock): Boolean;
begin

  FLock.Acquire;

  try

    if FClosed then
      Exit(False);

    if FCount >= FCapacity then
      Exit(False);

    FItems[FTail] := ABlock;
    FTail := (FTail + 1) mod FCapacity;
    Inc(FCount);
    FEvent.SetEvent;
    Result := True;
  finally

    FLock.Release;
  end;
end;


function TMfPcmBlockQueue.Pop(out ABlock: TMfPcmFloatBlock;
                              ATimeoutMs: Cardinal): Boolean;
var

  WaitRes: TWaitResult;
begin

  ABlock := Default(TMfPcmFloatBlock);

  while True do
    begin

      FLock.Acquire;

      try

        if (FCount > 0) then
          begin

            ABlock := FItems[FHead];
            FItems[FHead] := Default(TMfPcmFloatBlock);
            FHead := (FHead + 1) mod FCapacity;
            Dec(FCount);
            Exit(True);
          end;

        if FClosed then
          Exit(False);
      finally

        FLock.Release;
      end;

    WaitRes := FEvent.WaitFor(ATimeoutMs);
    if (WaitRes <> wrSignaled) then
      Exit(False);
  end;
end;


{ TMfIcecastBroadcastWorker }

constructor TMfIcecastBroadcastWorker.Create(AOwner: TMfIcecastBroadcastEngine);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


procedure TMfIcecastBroadcastWorker.Execute();
begin

  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);

  try

    FOwner.WorkerLoop();
  finally

    CoUninitialize();
  end;
end;


{ TMfIcecastBroadcastEngine }

// Helpers ---------------------------------------------------------------------

function AacSampleRateIndex(const ASampleRate: Integer): Integer;
begin
  case ASampleRate of
    96000: Result := 0;
    88200: Result := 1;
    64000: Result := 2;
    48000: Result := 3;
    44100: Result := 4;
    32000: Result := 5;
    24000: Result := 6;
    22050: Result := 7;
    16000: Result := 8;
    12000: Result := 9;
    11025: Result := 10;
    8000:  Result := 11;
    7350:  Result := 12;
  else
    Result := 4; // default 44100
  end;
end;


function BuildAdtsHeader(const AFrameDataLen: Integer;
                         const ASampleRate: Integer;
                         const AChannels: Integer): TBytes;
var
  Profile: Integer;
  FreqIdx: Integer;
  ChanCfg: Integer;
  FullLen: Integer;

begin

  SetLength(Result,
            7);

  // AAC LC
  Profile := 1;
  FreqIdx := AacSampleRateIndex(ASampleRate);
  ChanCfg := AChannels;
  FullLen := AFrameDataLen + 7;

  Result[0] := $FF;
  Result[1] := $F1; // syncword + MPEG-4 + layer + no CRC
  Result[2] := Byte(((Profile and $03) shl 6) or
                    ((FreqIdx and $0F) shl 2) or
                    ((ChanCfg and $04) shr 2));
  Result[3] := Byte(((ChanCfg and $03) shl 6) or
                    ((FullLen and $1800) shr 11));
  Result[4] := Byte((FullLen and $07F8) shr 3);
  Result[5] := Byte(((FullLen and $0007) shl 5) or $1F);
  Result[6] := $FC;
end;


function PrependAdtsToAacFrame(const AFrame: TBytes;
                               const ASampleRate: Integer;
                               const AChannels: Integer): TBytes;
var
  Hdr: TBytes;
  HdrLen: Integer;
  FrameLen: Integer;

begin

  FrameLen := Length(AFrame);
  if (FrameLen <= 0) then
    Exit(nil);

  Hdr := BuildAdtsHeader(FrameLen,
                         ASampleRate,
                         AChannels);

  HdrLen := Length(Hdr);

  SetLength(Result,
            HdrLen + FrameLen);

  Move(Hdr[0],
       Result[0],
       HdrLen);

  Move(AFrame[0],
       Result[HdrLen],
       FrameLen);
end;

// Helpers end -----------------------------------------------------------------

constructor TMfIcecastBroadcastEngine.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FState := bsStopped;
  FSettings.Port := 8000;
  FSettings.SampleRate := 44100;
  FSettings.Channels := 2;
  FSettings.BitrateKbps := 128;
  FSettings.Codec := bcAac;
  FSettings.TapPoint := btpPostMasterFx;
  FSettings.AutoReconnect := True;
  FSettings.BroadcastGainDb := 0.0;

  FDropCount := 0;
  FLastDropTick := 0;

  FFileNameParser := TFileNameParser.Create;

  FListenerCount := -1;
end;


destructor TMfIcecastBroadcastEngine.Destroy();
begin

  Stop();
  FreeRuntimeObjects();
  FreeAndNil(FFileNameParser);

  inherited;
end;


procedure TMfIcecastBroadcastEngine.DoError(Ahr: HResult;
                                            const AMsg: string);
begin

  if Assigned(FOnError) then
    FOnError(Self,
             AHR,
             AMsg);
end;


procedure TMfIcecastBroadcastEngine.SetState(ANewState: TMfBroadcastState;
                                             const AMsg: string);
begin

  FState := ANewState;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self,
                    FState,
                    AMsg);
end;


function TMfIcecastBroadcastEngine.ContentTypeFromCodec(): string;
begin

  case FSettings.Codec of
    bcAac: Result := 'audio/aac';
    bcMp3: Result := 'audio/mpeg';
  else
    Result := 'application/octet-stream';
  end;
end;


function TMfIcecastBroadcastEngine.CreateEncoder(): TMfBroadcastEncoderBase;
begin

  case FSettings.Codec of
    bcAac: Result := TMfBroadcastEncoderAac.Create;
    bcMp3: Result := TMfBroadcastEncoderNull.Create;
    else
      Result := TMfBroadcastEncoderNull.Create;
  end;
end;


procedure TMfIcecastBroadcastEngine.FreeRuntimeObjects();
begin

  if Assigned(FWorker) then
    begin

      FWorker.Terminate();

      if Assigned(FQueue) then
        FQueue.Close();

      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;

  FreeAndNil(FClient);
  FreeAndNil(FEncoder);
  FreeAndNil(FQueue);

  SetState(bsStopped,
           'Broadcast stopped');
end;


function TMfIcecastBroadcastEngine.Start(): HResult;
var
  hr: HResult;

begin

  if (FState <> bsStopped) then
    Exit(S_FALSE);

  if (Trim(FSettings.Host) = '') then
    Exit(E_INVALIDARG);

  if (Trim(FSettings.Mount) = '') then
    Exit(E_INVALIDARG);

  if (FSettings.Port <= 0) or (FSettings.Port > 65535) then
    Exit(E_INVALIDARG);

  if (FSettings.Channels <> 2) then
    Exit(E_INVALIDARG);

  FreeRuntimeObjects();

  FQueue := TMfPcmBlockQueue.Create(64);
  FEncoder := CreateEncoder;
  FClient := TIcecastSourceClient.Create();

  hr := FEncoder.Initialize(FSettings);
  if Failed(hr) then
    begin

      DoError(hr,
              'Broadcast encoder initialization failed');
      FreeRuntimeObjects();
      Exit(hr);
    end;

  FGainMul := DbToMul(FSettings.BroadcastGainDb);

  SetState(bsConnecting,
           'Starting broadcast worker');

  FWorker := TMfIcecastBroadcastWorker.Create(Self);

  Result := S_OK;
end;


procedure TMfIcecastBroadcastEngine.Stop();
begin

  if (FState = bsStopped) then
    Exit;

  if Assigned(FQueue) then
    FQueue.Close;

  if Assigned(FWorker) then
    begin

      FWorker.Terminate;
      FWorker.WaitFor;
      FreeAndNil(FWorker);
    end;

  WorkerDisconnect();

  FreeAndNil(FClient);
  FreeAndNil(FEncoder);
  FreeAndNil(FQueue);

  SetState(bsStopped,
           'Broadcast stopped');
end;


procedure TMfIcecastBroadcastEngine.ApplyGainToInterleavedStereoFloat32(pData: PSingle;
                                                                        AFrames: Integer);
var
  I: Integer;
  PS: PSingle;
  SampleCount: Integer;

begin

  if (pData = nil) or (AFrames <= 0) then
    Exit;

  if SameValue(FGainMul,
               1.0,
               1E-6) then
    Exit;

  PS := pData;
  SampleCount := AFrames * 2;

  for I := 0 to SampleCount - 1 do
    begin

      PS^ := PS^ * FGainMul;
      Inc(PS);
    end;
end;


function TMfIcecastBroadcastEngine.BuildSilenceBlock(const ADurationMs: Integer;
                                                     out ABlock: TMfPcmFloatBlock): Boolean;
var
  Frames: Integer;
  ByteCount: Integer;

begin

  Result := False;

  ABlock := Default(TMfPcmFloatBlock);

  if (FSettings.SampleRate <= 0) or
     (FSettings.Channels <= 0) then
    Exit;

  Frames := Round((Int64(FSettings.SampleRate) * ADurationMs) / 1000);
  if (Frames <= 0) then
    Frames := 1024;

  ABlock.Frames := Frames;
  ABlock.Channels := FSettings.Channels;
  ABlock.SampleRate := FSettings.SampleRate;

  ByteCount := Frames * ABlock.Channels * SizeOf(Single);
  if (ByteCount <= 0) then
    Exit;

  SetLength(ABlock.Data,
            ByteCount);

  FillChar(ABlock.Data[0],
           ByteCount,
           0);

  Result := True;
end;


function TMfIcecastBroadcastEngine.PushPcmFloat32(const pData: PSingle;
                                                  const AFrames: Integer;
                                                  const pwfx: PWAVEFORMATEX): HRESULT;
var
  Block: TMfPcmFloatBlock;
  ByteCount: Integer;

begin

  Result := S_FALSE;

  if (FState <> bsLive) then
    Exit;

  if (pData = nil) or
     (pwfx = nil) or
     (AFrames <= 0) then
    Exit(E_INVALIDARG);

  if (pwfx.nChannels <> 2) then
    Exit(E_INVALIDARG);

  if (pwfx.wBitsPerSample <> 32) then
    Exit(E_INVALIDARG);

  ByteCount := AFrames * pwfx.nBlockAlign;
  if (ByteCount <= 0) then
    Exit(E_INVALIDARG);

  // Drop silent PCM completely.
  if IsSilentStereoFloat32(pData,
                           AFrames) then
    Exit(S_FALSE);

  if not Assigned(FQueue) then
    Exit(E_FAIL);

  Block := Default(TMfPcmFloatBlock);
  Block.Frames := AFrames;
  Block.Channels := pwfx.nChannels;
  Block.SampleRate := pwfx.nSamplesPerSec;

  SetLength(Block.Data,
            ByteCount);

  Move(pData^,
       Block.Data[0],
       ByteCount);

  ApplyGainToInterleavedStereoFloat32(PSingle(@Block.Data[0]),
                                      AFrames);

  if not FQueue.Push(Block) then
    begin

      Inc(FDropCount);

      if ((GetTickCount - FLastDropTick) >= 1000) then
        begin

          FLastDropTick := GetTickCount;
          FDropCount := 0;
        end;

      Exit(S_FALSE);
    end;

  Result := S_OK;
end;


procedure TMfIcecastBroadcastEngine.UpdateNowPlaying(const AArtist: string;
                                                     ATitle: string;
                                                     ADefault: string = 'RDJ Broadcast Live');
var
  S: string;
  ListenerCount: Integer;

begin

  S := FFileNameParser.BuildIceCastSongText(AArtist,
                                            ATitle,
                                            ADefault);

  FLastNowPlaying := S;

  if Assigned(FClient) and FClient.Connected and (S <> '') then
    FClient.UpdateMetadata(S);

  if not Assigned(MainMDIFrm) then
    Exit;

  ListenerCount := GetIcecastListenerCount('/live');

  MainMDIFrm.jsonUpdate.WriteRadioStatusJson(MainMDIFrm.Setup.IcecastNowPlayingJsonFile,
                                             '',
                                             '',
                                             AArtist,
                                             ATitle,
                                             MainMDIFrm.CoverJpg,
                                             ListenerCount,
                                             1,  // onAir
                                             Trim(MainMDIFrm.DjName)); // lock owner);
end;


procedure TMfIcecastBroadcastEngine.UpdateNowPlayingFromFileName(const AFileName: string);
var
  Artist: string;
  Title: string;

begin

  FFileNameParser.ParseArtistTitleFromFileName(AFileName,
                                               Artist,
                                               Title);

  UpdateNowPlaying(Artist,
                   Title);
end;


function TMfIcecastBroadcastEngine.GetIcecastListenerCount(const AMount: string): Integer;
var
  Internet: HINTERNET;
  Request: HINTERNET;
  Root: TJSONObject;
  IceStats: TJSONObject;
  SourceValue: TJSONValue;
  SourceObj: TJSONObject;
  SourceArr: TJSONArray;
  I: Integer;
  ListenUrl: string;
  Mount: string;
  StatusUrl: string;
  ResponseText: UTF8String;
  Buffer: array[0..4095] of Byte;
  BytesRead: DWORD;
  OldLength: Integer;
  TimeoutMs: DWORD;

  function ReadListeners(const Obj: TJSONObject): Integer;
  var
    V: TJSONValue;
  begin
    Result := -1;

    if Obj = nil then
      Exit;

    V := Obj.GetValue('listeners');
    if V <> nil then
      Result := StrToIntDef(V.Value, -1);
  end;

  function SourceMatches(const Obj: TJSONObject): Boolean;
  var
    V: TJSONValue;

  begin

    Result := False;

    if (Obj = nil) then
      Exit;

    V := Obj.GetValue('listenurl');

    if (V <> nil) then
      begin

        ListenUrl := V.Value;
        Result := SameText(Copy(ListenUrl,
                                Length(ListenUrl) - Length(Mount) + 1,
                                MaxInt),
                           Mount);
      end;
  end;

begin
  Result := -1;
  Internet := nil;
  Request := nil;
  Root := nil;

  if Trim(FSettings.Host) = '' then
    Exit;

  if FSettings.Port = 0 then
    Exit;

  Mount := AMount;
  if (Mount <> '') and (Mount[1] <> '/') then
    Mount := '/' + Mount;

  try
    try
      Internet := InternetOpen('RDJ/1.0',
                               INTERNET_OPEN_TYPE_PRECONFIG,
                               nil,
                               nil,
                               0);
      if Internet = nil then
        Exit;

      TimeoutMs := 1500;
      InternetSetOption(Internet,
                        INTERNET_OPTION_CONNECT_TIMEOUT,
                        @TimeoutMs,
                        SizeOf(TimeoutMs));
      InternetSetOption(Internet,
                        INTERNET_OPTION_RECEIVE_TIMEOUT,
                        @TimeoutMs,
                        SizeOf(TimeoutMs));

      StatusUrl := Format('http://%s:%d/status-json.xsl',
                          [Trim(FSettings.Host),
                           FSettings.Port]);
      Request := InternetOpenUrl(Internet,
                                 PChar(StatusUrl),
                                 nil,
                                 0,
                                 INTERNET_FLAG_RELOAD or
                                 INTERNET_FLAG_NO_CACHE_WRITE,
                                 0);
      if Request = nil then
        Exit;

      ResponseText := '';
      repeat
        BytesRead := 0;
        if not InternetReadFile(Request,
                                @Buffer[0],
                                SizeOf(Buffer),
                                BytesRead) then
          Exit;

        if BytesRead > 0 then
          begin
            OldLength := Length(ResponseText);
            SetLength(ResponseText,
                      OldLength + Integer(BytesRead));
            Move(Buffer[0],
                 ResponseText[OldLength + 1],
                 BytesRead);
          end;
      until BytesRead = 0;

      Root := TJSONObject.ParseJSONValue(UTF8ToString(ResponseText)) as TJSONObject;

        if (Root = nil) then
          Exit;

        IceStats := Root.GetValue('icestats') as TJSONObject;
        if (IceStats = nil) then
          Exit;

        SourceValue := IceStats.GetValue('source');
        if (SourceValue = nil) then
          Exit;

        if (SourceValue is TJSONObject) then
          begin

            SourceObj := TJSONObject(SourceValue);
            Result := ReadListeners(SourceObj);
            Exit;
          end;

        if (SourceValue is TJSONArray) then
          begin

            SourceArr := TJSONArray(SourceValue);

            for I := 0 to SourceArr.Count - 1 do
              if (SourceArr.Items[I] is TJSONObject) then
                begin

                  SourceObj := TJSONObject(SourceArr.Items[I]);

                  if SourceMatches(SourceObj) then
                    begin

                      Result := ReadListeners(SourceObj);
                      Exit;
                    end;
                end;
          end;
    except

      // Listener count is optional. Never let a failed status probe break
      // now-playing updates or broadcast startup, especially in remote mode.
      Result := -1;
    end;
  finally
    Root.Free;
    if Request <> nil then
      InternetCloseHandle(Request);
    if Internet <> nil then
      InternetCloseHandle(Internet);
  end;
end;


function TMfIcecastBroadcastEngine.WorkerConnect(): HRESULT;
begin

  Result := E_FAIL;

  if not Assigned(FClient) then
    Exit;

  Result := FClient.Connect(FSettings,
                            ContentTypeFromCodec);

  if Succeeded(Result) then
    begin

      SetState(bsLive,
               'Broadcast live');
      if (FLastNowPlaying <> '') then
        FClient.UpdateMetadata(FLastNowPlaying);
    end
  else
    begin

      SetState(bsError,
               'Broadcast connect failed');
      DoError(Result,
              'Broadcast source connect failed');
    end;
end;


procedure TMfIcecastBroadcastEngine.WorkerDisconnect();
begin

  if Assigned(FClient) then
    FClient.Disconnect;

  if Assigned(FQueue) then
    FQueue.Clear;
end;


procedure TMfIcecastBroadcastEngine.WorkerLoop();
var
  hr: HResult;
  Block: TMfPcmFloatBlock;
  Encoded: TBytes;
  AdtsHdr: TBytes;

begin

  while not TThread.CurrentThread.CheckTerminated do
    begin

    hr := WorkerConnect();
    if Failed(hr) then
      begin

        if not FSettings.AutoReconnect then
          Exit;

        if Assigned(FQueue) then
          FQueue.Clear();

        SetState(bsReconnecting,
                 'Reconnect in progress');
        Sleep(2000);
        Continue;
      end;

    while not TThread.CurrentThread.CheckTerminated do
      begin

        Block := Default(TMfPcmFloatBlock);
        Encoded := nil;

        try

          if not Assigned(FQueue) then
            Break;

          if not FQueue.Pop(Block,
                            50) then
          begin

            if not BuildSilenceBlock(50,
                                     Block) then
            Continue;
          end;

          if not Assigned(FEncoder) then
            Continue;

          if (Block.Frames <= 0) or (Length(Block.Data) = 0) then
            Continue;

          hr := FEncoder.EncodeInterleavedFloat32(PSingle(@Block.Data[0]),
                                                  Block.Frames,
                                                  Encoded);
          if Failed(hr) then
            begin

              SetState(bsError,
                       'Broadcast encoder error');
              DoError(hr,
                      'Broadcast encoder returned failure');
              Exit;
            end;

          if (Length(Encoded) > 0) and
             Assigned(FClient) and
             FClient.Connected then
          begin
            if (FSettings.Codec = bcAac) then
            begin
              AdtsHdr := BuildAdtsHeader(Length(Encoded),
                                         FSettings.SampleRate,
                                         FSettings.Channels);
              hr := FClient.SendDataParts(AdtsHdr[0],
                                          Length(AdtsHdr),
                                          Encoded[0],
                                          Length(Encoded));
            end
            else
              hr := FClient.SendData(Encoded[0],
                                     Length(Encoded));

            if Failed(hr) then
            begin
              WorkerDisconnect();

              if not FSettings.AutoReconnect then
              begin
                SetState(bsError,
                         'Broadcast send failed');

                DoError(hr,
                        'Broadcast send failed');
                Exit;
              end;

              if Assigned(FQueue) then
                FQueue.Clear;

              SetState(bsReconnecting,
                       'Broadcast reconnecting');
              Break;
            end;
          end;
        finally

          Encoded := nil;
          Block := Default(TMfPcmFloatBlock);
        end;
    end;

    if not FSettings.AutoReconnect then
      Break;

    Sleep(2000);
  end;

  WorkerDisconnect();
end;

end.

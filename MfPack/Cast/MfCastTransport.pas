// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastTransport.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: WinSock + SChannel transport for the Cast control channel.
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
// Remarks: Requires Windows 7 or higher.
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
unit MfCastTransport;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type

  TSecHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;

  TMfCastTcpTransport = class(TInterfacedObject, IMfCastTransport)
  private
    FSettings: TMfCastProtocolSettings;
    FLogger: IMfCastLogger;
    FSocket: TSocket;
    FWSAStarted: Boolean;
    FCredHandle: TSecHandle;
    FCtxtHandle: TSecHandle;
    FHaveCredHandle: Boolean;
    FHaveCtxtHandle: Boolean;
    FTlsActive: Boolean;
    FStreamSizes: record
      cbHeader: Cardinal;
      cbTrailer: Cardinal;
      cbMaximumMessage: Cardinal;
      cBuffers: Cardinal;
      cbBlockSize: Cardinal;
    end;
    FEncryptedBuffer: TBytes;
    FPlainBuffer: TBytes;

    function ResolveHost(const AHost: string; out AAddress: TInAddr): HRESULT;
    function LastSocketError(): HRESULT;
    function RawSendBuffer(const ABuffer: Pointer;
                           const ASize: Cardinal): HRESULT;
    function RawReceiveBuffer(ABuffer: Pointer;
                              const ABufferSize: Cardinal;
                              out ABytesRead: Cardinal): HRESULT;
    function AcquireTlsCredentials(): HRESULT;
    function DoTlsHandshake(const AHost: string): HRESULT;
    function DecryptNextRecord(): HRESULT;
    procedure AppendBytes(var ATarget: TBytes;
                          const ASource: Pointer;
                          const ASize: Cardinal);
    procedure ConsumeBytes(var ATarget: TBytes;
                           const ACount: Cardinal);
    procedure ClearTls();

  public
    constructor Create();
    destructor Destroy(); override;

    function Configure(const ASettings: TMfCastProtocolSettings): HRESULT;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Connect(const AHost: string;
                     const APort: Word): HRESULT;
    function Disconnect(): HRESULT;
    function SendBuffer(const ABuffer: Pointer;
                        const ASize: Cardinal): HRESULT;
    function ReceiveBuffer(ABuffer: Pointer;
                           const ABufferSize: Cardinal;
                           out ABytesRead: Cardinal): HRESULT;
    function IsConnected(): Boolean;
  end;

implementation

const
  SECURITY_NATIVE_DREP = $00000010;
  SECPKG_CRED_OUTBOUND = 2;
  UNISP_NAME_A = 'Microsoft Unified Security Protocol Provider';

  ISC_REQ_SEQUENCE_DETECT = $00000008;
  ISC_REQ_REPLAY_DETECT = $00000004;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ISC_REQ_STREAM = $00008000;
  ISC_REQ_EXTENDED_ERROR = $00004000;

  SEC_E_OK = HRESULT($00000000);
  SEC_I_CONTINUE_NEEDED = HRESULT($00090312);
  SEC_E_INCOMPLETE_MESSAGE = HRESULT($80090318);
  SEC_I_CONTEXT_EXPIRED = HRESULT($00090317);
  SEC_I_RENEGOTIATE = HRESULT($00090321);

  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_EXTRA = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER = 7;

  SECPKG_ATTR_STREAM_SIZES = 4;
  SCHANNEL_CRED_VERSION = 4;
  SCH_CRED_MANUAL_CRED_VALIDATION = $00000008;
  SCH_CRED_NO_DEFAULT_CREDS = $00000010;
  SCH_CRED_IGNORE_NO_REVOCATION_CHECK = $00000800;
  SCH_CRED_IGNORE_REVOCATION_OFFLINE = $00001000;

type
  TSecurityStatus = HRESULT;
  TTimeStamp = Int64;

  PSecHandle = ^TSecHandle;

  PSecBuffer = ^TSecBuffer;
  TSecBuffer = record
    cbBuffer: Cardinal;
    BufferType: Cardinal;
    pvBuffer: Pointer;
  end;

  PSecBufferDesc = ^TSecBufferDesc;
  TSecBufferDesc = record
    ulVersion: Cardinal;
    cBuffers: Cardinal;
    pBuffers: PSecBuffer;
  end;

  PSChannelCred = ^TSChannelCred;
  TSChannelCred = record
    dwVersion: DWORD;
    cCreds: DWORD;
    paCred: Pointer;
    hRootStore: Pointer;
    cMappers: DWORD;
    aphMappers: Pointer;
    cSupportedAlgs: DWORD;
    palgSupportedAlgs: Pointer;
    grbitEnabledProtocols: DWORD;
    dwMinimumCipherStrength: DWORD;
    dwMaximumCipherStrength: DWORD;
    dwSessionLifespan: DWORD;
    dwFlags: DWORD;
    dwCredFormat: DWORD;
  end;

  PSecPkgContextStreamSizes = ^TSecPkgContextStreamSizes;
  TSecPkgContextStreamSizes = record
    cbHeader: Cardinal;
    cbTrailer: Cardinal;
    cbMaximumMessage: Cardinal;
    cBuffers: Cardinal;
    cbBlockSize: Cardinal;
  end;

function AcquireCredentialsHandleA(pszPrincipal: PAnsiChar;
                                   pszPackage: PAnsiChar;
                                   fCredentialUse: Cardinal;
                                   pvLogonID: Pointer;
                                   pAuthData: Pointer;
                                   pGetKeyFn: Pointer;
                                   pvGetKeyArgument: Pointer;
                                   var phCredential: TSecHandle;
                                   var ptsExpiry: TTimeStamp): TSecurityStatus; stdcall; external 'secur32.dll' name 'AcquireCredentialsHandleA';
function InitializeSecurityContextA(phCredential: PSecHandle;
                                    phContext: PSecHandle;
                                    pszTargetName: PAnsiChar;
                                    fContextReq: Cardinal;
                                    Reserved1: Cardinal;
                                    TargetDataRep: Cardinal;
                                    pInput: PSecBufferDesc;
                                    Reserved2: Cardinal;
                                    var phNewContext: TSecHandle;
                                    pOutput: PSecBufferDesc;
                                    var pfContextAttr: Cardinal;
                                    var ptsExpiry: TTimeStamp): TSecurityStatus; stdcall; external 'secur32.dll' name 'InitializeSecurityContextA';
function FreeCredentialsHandle(var phCredential: TSecHandle): TSecurityStatus; stdcall; external 'secur32.dll' name 'FreeCredentialsHandle';
function DeleteSecurityContext(var phContext: TSecHandle): TSecurityStatus; stdcall; external 'secur32.dll' name 'DeleteSecurityContext';
function FreeContextBuffer(pvContextBuffer: Pointer): TSecurityStatus; stdcall; external 'secur32.dll' name 'FreeContextBuffer';
function QueryContextAttributesA(var phContext: TSecHandle;
                                 ulAttribute: Cardinal;
                                 pBuffer: Pointer): TSecurityStatus; stdcall; external 'secur32.dll' name 'QueryContextAttributesA';
function EncryptMessage(var phContext: TSecHandle;
                        fQOP: Cardinal;
                        pMessage: PSecBufferDesc;
                        MessageSeqNo: Cardinal): TSecurityStatus; stdcall; external 'secur32.dll' name 'EncryptMessage';
function DecryptMessage(var phContext: TSecHandle;
                        pMessage: PSecBufferDesc;
                        MessageSeqNo: Cardinal;
                        var pfQOP: Cardinal): TSecurityStatus; stdcall; external 'secur32.dll' name 'DecryptMessage';


constructor TMfCastTcpTransport.Create();
begin

  inherited Create();
  FSocket := INVALID_SOCKET;
  FWSAStarted := False;
  FHaveCredHandle := False;
  FHaveCtxtHandle := False;
  FTlsActive := False;
end;


destructor TMfCastTcpTransport.Destroy();
begin

  Disconnect();
  if FWSAStarted then
    WSACleanup();
  inherited Destroy();
end;


function TMfCastTcpTransport.Configure(
  const ASettings: TMfCastProtocolSettings): HRESULT;
begin

  FSettings := ASettings;
  Result := S_OK;
end;


procedure TMfCastTcpTransport.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


procedure TMfCastTcpTransport.AppendBytes(var ATarget: TBytes;
                                          const ASource: Pointer;
                                          const ASize: Cardinal);
var
  OldLength: Integer;
begin

  if (ASource = nil) or (ASize = 0) then
    Exit;

  OldLength := Length(ATarget);
  SetLength(ATarget, OldLength + Integer(ASize));
  Move(ASource^, ATarget[OldLength], ASize);
end;


procedure TMfCastTcpTransport.ConsumeBytes(var ATarget: TBytes;
                                           const ACount: Cardinal);
var
  Remaining: Integer;
begin

  if ACount = 0 then
    Exit;

  if ACount >= Cardinal(Length(ATarget)) then
    begin
      SetLength(ATarget, 0);
      Exit;
    end;

  Remaining := Length(ATarget) - Integer(ACount);
  Move(ATarget[Integer(ACount)], ATarget[0], Remaining);
  SetLength(ATarget, Remaining);
end;


procedure TMfCastTcpTransport.ClearTls();
begin

  if FHaveCtxtHandle then
    begin
      DeleteSecurityContext(FCtxtHandle);
      FillChar(FCtxtHandle, SizeOf(FCtxtHandle), 0);
      FHaveCtxtHandle := False;
    end;

  if FHaveCredHandle then
    begin
      FreeCredentialsHandle(FCredHandle);
      FillChar(FCredHandle, SizeOf(FCredHandle), 0);
      FHaveCredHandle := False;
    end;

  FTlsActive := False;
  SetLength(FEncryptedBuffer, 0);
  SetLength(FPlainBuffer, 0);
  FillChar(FStreamSizes, SizeOf(FStreamSizes), 0);
end;


function TMfCastTcpTransport.LastSocketError(): HRESULT;
var
  ErrorCode: Integer;
begin

  ErrorCode := WSAGetLastError();
  if ErrorCode = 0 then
    Result := E_FAIL
  else
    Result := HRESULT($80070000 or DWORD(ErrorCode));
end;


function TMfCastTcpTransport.ResolveHost(const AHost: string;
  out AAddress: TInAddr): HRESULT;
var
  HostAnsi: AnsiString;
  HostEntry: PHostEnt;
begin

  FillChar(AAddress, SizeOf(AAddress), 0);
  HostAnsi := AnsiString(Trim(AHost));
  if HostAnsi = '' then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  AAddress.S_addr := inet_addr(PAnsiChar(HostAnsi));
  if AAddress.S_addr <> u_long(INADDR_NONE) then
    begin
      Result := S_OK;
      Exit;
    end;

  HostEntry := gethostbyname(PAnsiChar(HostAnsi));
  if not Assigned(HostEntry) or not Assigned(HostEntry^.h_addr_list[0]) then
    begin
      Result := LastSocketError();
      Exit;
    end;

  AAddress := PInAddr(HostEntry^.h_addr_list[0])^;
  Result := S_OK;
end;


function TMfCastTcpTransport.AcquireTlsCredentials(): HRESULT;
var
  Cred: TSChannelCred;
  Expiry: TTimeStamp;
begin

  FillChar(FCredHandle, SizeOf(FCredHandle), 0);
  FillChar(Cred, SizeOf(Cred), 0);
  Cred.dwVersion := SCHANNEL_CRED_VERSION;
  Cred.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or
                  SCH_CRED_IGNORE_NO_REVOCATION_CHECK or
                  SCH_CRED_IGNORE_REVOCATION_OFFLINE;
  if not FSettings.VerifyTlsPeer then
    Cred.dwFlags := Cred.dwFlags or SCH_CRED_MANUAL_CRED_VALIDATION;

  Result := AcquireCredentialsHandleA(nil,
                                      PAnsiChar(AnsiString(UNISP_NAME_A)),
                                      SECPKG_CRED_OUTBOUND,
                                      nil,
                                      @Cred,
                                      nil,
                                      nil,
                                      FCredHandle,
                                      Expiry);
  FHaveCredHandle := Result = SEC_E_OK;
end;


function TMfCastTcpTransport.RawSendBuffer(const ABuffer: Pointer;
                                           const ASize: Cardinal): HRESULT;
var
  TotalSent: Cardinal;
  Sent: Integer;
  Ptr: PAnsiChar;
begin

  if (ABuffer = nil) and (ASize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not IsConnected() then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  TotalSent := 0;
  Ptr := PAnsiChar(ABuffer);
  while TotalSent < ASize do
    begin
      Sent := send(FSocket, Ptr[TotalSent], ASize - TotalSent, 0);
      if Sent = SOCKET_ERROR then
        begin
          Result := LastSocketError();
          Exit;
        end;
      if Sent = 0 then
        begin
          Result := E_FAIL;
          Exit;
        end;
      Inc(TotalSent, Cardinal(Sent));
    end;

  Result := S_OK;
end;


function TMfCastTcpTransport.RawReceiveBuffer(ABuffer: Pointer;
                                              const ABufferSize: Cardinal;
                                              out ABytesRead: Cardinal): HRESULT;
var
  ReadCount: Integer;
begin

  ABytesRead := 0;
  if (ABuffer = nil) and (ABufferSize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not IsConnected() then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  ReadCount := recv(FSocket, ABuffer^, ABufferSize, 0);
  if ReadCount = SOCKET_ERROR then
    begin
      Result := LastSocketError();
      Exit;
    end;

  ABytesRead := Cardinal(ReadCount);
  Result := S_OK;
end;


function TMfCastTcpTransport.DoTlsHandshake(const AHost: string): HRESULT;
var
  ContextReq: Cardinal;
  ContextAttr: Cardinal;
  Expiry: TTimeStamp;
  TargetName: AnsiString;
  OutBuffers: array[0..0] of TSecBuffer;
  OutDesc: TSecBufferDesc;
  InBuffers: array[0..1] of TSecBuffer;
  InDesc: TSecBufferDesc;
  Work: TBytes;
  Temp: array[0..8191] of Byte;
  BytesRead: Cardinal;
  ExtraSize: Cardinal;
  Status: HRESULT;
  HaveContext: Boolean;
begin

  Result := AcquireTlsCredentials();
  if FAILED(Result) then
    Exit;

  ContextReq := ISC_REQ_SEQUENCE_DETECT or
                ISC_REQ_REPLAY_DETECT or
                ISC_REQ_CONFIDENTIALITY or
                ISC_REQ_EXTENDED_ERROR or
                ISC_REQ_ALLOCATE_MEMORY or
                ISC_REQ_STREAM;
  TargetName := AnsiString(AHost);
  HaveContext := False;
  Status := SEC_I_CONTINUE_NEEDED;

  while Status = SEC_I_CONTINUE_NEEDED do
    begin
      FillChar(OutBuffers, SizeOf(OutBuffers), 0);
      OutBuffers[0].BufferType := SECBUFFER_TOKEN;
      OutDesc.ulVersion := SECBUFFER_VERSION;
      OutDesc.cBuffers := 1;
      OutDesc.pBuffers := @OutBuffers[0];

      if HaveContext then
        begin
          if Length(FEncryptedBuffer) = 0 then
            begin
              Result := RawReceiveBuffer(@Temp[0], SizeOf(Temp), BytesRead);
              if FAILED(Result) then
                Exit;
              if BytesRead = 0 then
                begin
                  Result := E_FAIL;
                  Exit;
                end;
              AppendBytes(FEncryptedBuffer, @Temp[0], BytesRead);
            end;

          Work := Copy(FEncryptedBuffer, 0, Length(FEncryptedBuffer));
          FillChar(InBuffers, SizeOf(InBuffers), 0);
          InBuffers[0].BufferType := SECBUFFER_TOKEN;
          InBuffers[0].cbBuffer := Length(Work);
          InBuffers[0].pvBuffer := @Work[0];
          InBuffers[1].BufferType := SECBUFFER_EMPTY;
          InDesc.ulVersion := SECBUFFER_VERSION;
          InDesc.cBuffers := 2;
          InDesc.pBuffers := @InBuffers[0];

          Status := InitializeSecurityContextA(@FCredHandle,
                                               @FCtxtHandle,
                                               PAnsiChar(TargetName),
                                               ContextReq,
                                               0,
                                               SECURITY_NATIVE_DREP,
                                               @InDesc,
                                               0,
                                               FCtxtHandle,
                                               @OutDesc,
                                               ContextAttr,
                                               Expiry);

          if Status = SEC_E_INCOMPLETE_MESSAGE then
            begin
              Result := RawReceiveBuffer(@Temp[0], SizeOf(Temp), BytesRead);
              if FAILED(Result) then
                Exit;
              if BytesRead = 0 then
                begin
                  Result := E_FAIL;
                  Exit;
                end;
              AppendBytes(FEncryptedBuffer, @Temp[0], BytesRead);
              Continue;
            end;

          SetLength(FEncryptedBuffer, 0);
          if InBuffers[1].BufferType = SECBUFFER_EXTRA then
            begin
              ExtraSize := InBuffers[1].cbBuffer;
              if ExtraSize > 0 then
                AppendBytes(FEncryptedBuffer,
                            Pointer(NativeUInt(InBuffers[1].pvBuffer)),
                            ExtraSize);
            end;
        end
      else
        begin
          Status := InitializeSecurityContextA(@FCredHandle,
                                               nil,
                                               PAnsiChar(TargetName),
                                               ContextReq,
                                               0,
                                               SECURITY_NATIVE_DREP,
                                               nil,
                                               0,
                                               FCtxtHandle,
                                               @OutDesc,
                                               ContextAttr,
                                               Expiry);
          if (Status = SEC_E_OK) or (Status = SEC_I_CONTINUE_NEEDED) then
            begin
              FHaveCtxtHandle := True;
              HaveContext := True;
            end;
        end;

      if Assigned(OutBuffers[0].pvBuffer) and (OutBuffers[0].cbBuffer > 0) then
        begin
          Result := RawSendBuffer(OutBuffers[0].pvBuffer,
                                  OutBuffers[0].cbBuffer);
          FreeContextBuffer(OutBuffers[0].pvBuffer);
          if FAILED(Result) then
            Exit;
        end;

      if (Status <> SEC_E_OK) and (Status <> SEC_I_CONTINUE_NEEDED) then
        begin
          Result := Status;
          Exit;
        end;
    end;

  if Status <> SEC_E_OK then
    begin
      Result := Status;
      Exit;
    end;

  Result := QueryContextAttributesA(FCtxtHandle,
                                    SECPKG_ATTR_STREAM_SIZES,
                                    @FStreamSizes);
  if FAILED(Result) then
    Exit;

  FTlsActive := True;
  Result := S_OK;
end;


function TMfCastTcpTransport.Connect(const AHost: string;
  const APort: Word): HRESULT;
var
  WsaData: TWSAData;
  Addr: TSockAddrIn;
  ResolvedAddress: TInAddr;
  Timeout: Integer;
begin

  if IsConnected() then
    Disconnect();

  if not FWSAStarted then
    begin
      if WSAStartup($0202, WsaData) <> 0 then
        begin
          Result := LastSocketError();
          Exit;
        end;
      FWSAStarted := True;
    end;

  Result := ResolveHost(AHost, ResolvedAddress);
  if FAILED(Result) then
    Exit;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    begin
      Result := LastSocketError();
      Exit;
    end;

  Timeout := Integer(FSettings.ReadTimeoutMs);
  if Timeout > 0 then
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO,
               PAnsiChar(@Timeout), SizeOf(Timeout));
  Timeout := Integer(FSettings.WriteTimeoutMs);
  if Timeout > 0 then
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO,
               PAnsiChar(@Timeout), SizeOf(Timeout));

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Addr.sin_addr := ResolvedAddress;

  if WinApi.WinSock.connect(FSocket, TSockAddr(Addr), SizeOf(Addr)) = SOCKET_ERROR then
    begin
      Result := LastSocketError();
      Disconnect();
      Exit;
    end;

  Result := DoTlsHandshake(AHost);
  if FAILED(Result) then
    Disconnect();
end;


function TMfCastTcpTransport.Disconnect(): HRESULT;
begin

  ClearTls();

  if FSocket <> INVALID_SOCKET then
    begin
      shutdown(FSocket, SD_BOTH);
      WinApi.WinSock.closesocket(FSocket);
      FSocket := INVALID_SOCKET;
    end;

  Result := S_OK;
end;


function TMfCastTcpTransport.SendBuffer(const ABuffer: Pointer;
  const ASize: Cardinal): HRESULT;
var
  Offset: Cardinal;
  ChunkSize: Cardinal;
  PlainPtr: PAnsiChar;
  Packet: TBytes;
  Buffers: array[0..3] of TSecBuffer;
  Desc: TSecBufferDesc;
  PacketSize: Cardinal;
begin

  if (ABuffer = nil) and (ASize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not FTlsActive then
    begin
      Result := RawSendBuffer(ABuffer, ASize);
      Exit;
    end;

  Offset := 0;
  PlainPtr := PAnsiChar(ABuffer);
  while Offset < ASize do
    begin
      ChunkSize := ASize - Offset;
      if (FStreamSizes.cbMaximumMessage > 0) and
         (ChunkSize > FStreamSizes.cbMaximumMessage) then
        ChunkSize := FStreamSizes.cbMaximumMessage;

      SetLength(Packet, FStreamSizes.cbHeader + ChunkSize + FStreamSizes.cbTrailer);
      if ChunkSize > 0 then
        Move(PlainPtr[Offset], Packet[FStreamSizes.cbHeader], ChunkSize);

      FillChar(Buffers, SizeOf(Buffers), 0);
      Buffers[0].BufferType := SECBUFFER_STREAM_HEADER;
      Buffers[0].cbBuffer := FStreamSizes.cbHeader;
      Buffers[0].pvBuffer := @Packet[0];
      Buffers[1].BufferType := SECBUFFER_DATA;
      Buffers[1].cbBuffer := ChunkSize;
      Buffers[1].pvBuffer := @Packet[FStreamSizes.cbHeader];
      Buffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
      Buffers[2].cbBuffer := FStreamSizes.cbTrailer;
      Buffers[2].pvBuffer := @Packet[FStreamSizes.cbHeader + ChunkSize];
      Buffers[3].BufferType := SECBUFFER_EMPTY;

      Desc.ulVersion := SECBUFFER_VERSION;
      Desc.cBuffers := 4;
      Desc.pBuffers := @Buffers[0];

      Result := EncryptMessage(FCtxtHandle, 0, @Desc, 0);
      if FAILED(Result) then
        Exit;

      PacketSize := Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer;
      Result := RawSendBuffer(@Packet[0], PacketSize);
      if FAILED(Result) then
        Exit;

      Inc(Offset, ChunkSize);
    end;

  Result := S_OK;
end;


function TMfCastTcpTransport.DecryptNextRecord(): HRESULT;
var
  Work: TBytes;
  Temp: array[0..8191] of Byte;
  Buffers: array[0..3] of TSecBuffer;
  Desc: TSecBufferDesc;
  BytesRead: Cardinal;
  I: Integer;
  Qop: Cardinal;
  DataCopied: Boolean;
begin

  Result := S_OK;
  DataCopied := False;

  while not DataCopied do
    begin
      if Length(FEncryptedBuffer) = 0 then
        begin
          Result := RawReceiveBuffer(@Temp[0], SizeOf(Temp), BytesRead);
          if FAILED(Result) then
            Exit;
          if BytesRead = 0 then
            begin
              Result := E_FAIL;
              Exit;
            end;
          AppendBytes(FEncryptedBuffer, @Temp[0], BytesRead);
        end;

      Work := Copy(FEncryptedBuffer, 0, Length(FEncryptedBuffer));
      FillChar(Buffers, SizeOf(Buffers), 0);
      Buffers[0].BufferType := SECBUFFER_DATA;
      Buffers[0].cbBuffer := Length(Work);
      Buffers[0].pvBuffer := @Work[0];
      Buffers[1].BufferType := SECBUFFER_EMPTY;
      Buffers[2].BufferType := SECBUFFER_EMPTY;
      Buffers[3].BufferType := SECBUFFER_EMPTY;

      Desc.ulVersion := SECBUFFER_VERSION;
      Desc.cBuffers := 4;
      Desc.pBuffers := @Buffers[0];

      Result := DecryptMessage(FCtxtHandle, @Desc, 0, Qop);
      if Result = SEC_E_INCOMPLETE_MESSAGE then
        begin
          Result := RawReceiveBuffer(@Temp[0], SizeOf(Temp), BytesRead);
          if FAILED(Result) then
            Exit;
          if BytesRead = 0 then
            begin
              Result := E_FAIL;
              Exit;
            end;
          AppendBytes(FEncryptedBuffer, @Temp[0], BytesRead);
          Continue;
        end;

      if Result = SEC_I_CONTEXT_EXPIRED then
        begin
          Disconnect();
          Result := E_UNEXPECTED;
          Exit;
        end;

      if Result = SEC_I_RENEGOTIATE then
        begin
          Result := E_NOTIMPL;
          Exit;
        end;

      if FAILED(Result) then
        Exit;

      SetLength(FEncryptedBuffer, 0);
      for I := 0 to 3 do
        begin
          if (Buffers[I].BufferType = SECBUFFER_DATA) and
             Assigned(Buffers[I].pvBuffer) and
             (Buffers[I].cbBuffer > 0) then
            begin
              AppendBytes(FPlainBuffer,
                          Buffers[I].pvBuffer,
                          Buffers[I].cbBuffer);
              DataCopied := True;
            end;

          if (Buffers[I].BufferType = SECBUFFER_EXTRA) and
             Assigned(Buffers[I].pvBuffer) and
             (Buffers[I].cbBuffer > 0) then
            AppendBytes(FEncryptedBuffer,
                        Buffers[I].pvBuffer,
                        Buffers[I].cbBuffer);
        end;
    end;
end;


function TMfCastTcpTransport.ReceiveBuffer(ABuffer: Pointer;
  const ABufferSize: Cardinal; out ABytesRead: Cardinal): HRESULT;
var
  ToCopy: Cardinal;
begin

  ABytesRead := 0;
  if (ABuffer = nil) and (ABufferSize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not FTlsActive then
    begin
      Result := RawReceiveBuffer(ABuffer, ABufferSize, ABytesRead);
      Exit;
    end;

  if Length(FPlainBuffer) = 0 then
    begin
      Result := DecryptNextRecord();
      if FAILED(Result) then
        Exit;
    end;

  ToCopy := ABufferSize;
  if ToCopy > Cardinal(Length(FPlainBuffer)) then
    ToCopy := Length(FPlainBuffer);

  if ToCopy > 0 then
    begin
      Move(FPlainBuffer[0], ABuffer^, ToCopy);
      ConsumeBytes(FPlainBuffer, ToCopy);
    end;

  ABytesRead := ToCopy;
  Result := S_OK;
end;


function TMfCastTcpTransport.IsConnected(): Boolean;
begin

  Result := FSocket <> INVALID_SOCKET;
end;

end.

// FactoryX
//
// Copyright � FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Server.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Server unit.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
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
unit FxServe.Server;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  FxServe.Config,
  FxServe.Logging,
  FxServe.Protection,
  FxServe.Viewers;

type
  TFxServeServer = class;

  TFxServeAcceptThread = class(TThread)
  private
    FServer: TFxServeServer;

  protected

    procedure Execute(); override;

  public

    constructor Create(AServer: TFxServeServer);

  end;

  TFxServeClientThread = class(TThread)
  private
    FServer: TFxServeServer;
    FSocket: TSocket;
    FPeerAddress: string;

  protected

    procedure Execute(); override;

  public

    constructor Create(AServer: TFxServeServer;
                       ASocket: TSocket;
                       const APeerAddress: string);
  end;


  TFxServeServer = class
  private
    FConfig: TFxServeConfig;
    FLogger: TFxServeLogger;
    FListenSocket: TSocket;
    FAcceptThread: TFxServeAcceptThread;
    FClients: TList<TSocket>;
    FClientsLock: TCriticalSection;
    FRunning: Boolean;
    FActiveConnections: Integer;
    FWSAStarted: Boolean;
    FProtection: TFxServeRequestProtection;

    procedure AcceptClients;
    procedure HandleClient(ASocket: TSocket;
                           const APeerAddress: string);

    procedure RegisterClient(ASocket: TSocket);
    procedure UnregisterClient(ASocket: TSocket);

    function ReceiveHeader(ASocket: TSocket;
                           out AHeader: AnsiString): Boolean;

    function SendAll(ASocket: TSocket;
                     const ABuffer;
                     const ASize: Integer): Boolean;

    function SendText(ASocket: TSocket;
                      AText: RawByteString): Boolean;

    procedure SendError(ASocket: TSocket;
                        AStatus: Integer;
                        const AReason: string;
                        const AMessage: string;
                        const AExtraHeaderName: string = '';
                        const AExtraHeaderValue: string = '');

    procedure SendViewerSnapshot(ASocket: TSocket;
                                 const AMethod: string);

    procedure ServeStatic(ASocket: TSocket;
                          const AMethod, APath,
                          ARequestHeader: string);

    procedure ProxyRequest(ASocket: TSocket;
                           const AMethod: string;
                           const ATarget: string;
                           const ARequestHeader: string);
  public

    constructor Create(AConfig: TFxServeConfig;
                       ALogger: TFxServeLogger);

    destructor Destroy; override;

    procedure Start();
    procedure Stop();

    property Running: Boolean read FRunning;
  end;


implementation

const
  MAX_HEADER_SIZE = 65536;
  FILE_BUFFER_SIZE = 65536;


function HeaderValue(const AHeader: string;
                     const AName: string): string;
var
  Lines: TStringList;
  I: Integer;
  P: Integer;
  NameText: string;

begin

  Result := '';
  Lines := TStringList.Create();

  try
    Lines.Text := StringReplace(AHeader,
                                #13#10,
                                sLineBreak,
                                [rfReplaceAll]);

    for I := 1 to Lines.Count - 1 do
      begin
        P := Pos(':',
                 Lines[I]);

        if (P <= 0) then
          Continue;

        NameText := Trim(Copy(Lines[I],
                              1,
                              P - 1));

        if SameText(NameText,
                    AName) then
          Exit(Trim(Copy(Lines[I],
                         P + 1,
                         MaxInt)));
      end;

  finally
    Lines.Free;
  end;
end;


function PrepareProxyResponseHeader(const ARawHeader: AnsiString;
                                    ACorsEnabled: Boolean): AnsiString;
var
  Lines: TStringList;
  I, P: Integer;
  NameText: string;
  HasCors: Boolean;

begin

  Result := '';
  HasCors := False;
  Lines := TStringList.Create();

  try
    Lines.Text := StringReplace(string(ARawHeader),
                                #13#10,
                                sLineBreak,
                                [rfReplaceAll]);
    if (Lines.Count = 0) then
      Exit;

    Result := AnsiString(Lines[0] + #13#10);

    for I := 1 to Lines.Count - 1 do
    begin
      if (Lines[I] = '') then
        Continue;

      P := Pos(':',
               Lines[I]);

      if (P <= 0) then
        Continue;

      NameText := Trim(Copy(Lines[I],
                            1,
                            P - 1));

      if SameText(NameText,
                  'Connection') or
         SameText(NameText,
                  'Keep-Alive') then
        Continue;

      if SameText(NameText,
                  'Access-Control-Allow-Origin') then
        HasCors := True;

      Result := Result + AnsiString(Lines[I] + #13#10);
    end;

    if ACorsEnabled and not HasCors then
      Result := Result + 'Access-Control-Allow-Origin: *'#13#10;

    Result := Result + 'Connection: close'#13#10#13#10;

  finally
    Lines.Free;
  end;
end;


function HttpReason(AStatus: Integer): string;
begin

  case AStatus of
    200: Result := 'OK';
    206: Result := 'Partial Content';
    400: Result := 'Bad Request';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    416: Result := 'Range Not Satisfiable';
    500: Result := 'Internal Server Error';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
  else
    Result := 'Error';
  end;
end;


function MimeTypeForFile(const AFileName: string): string;
var
  Ext: string;

begin

  Ext := LowerCase(ExtractFileExt(AFileName));

  if (Ext = '.html') or (Ext = '.htm') then
    Result := 'text/html; charset=utf-8'
  else
    if (Ext = '.css') then
      Result := 'text/css; charset=utf-8'
    else
      if (Ext = '.js') then
        Result := 'application/javascript; charset=utf-8'
      else
        if (Ext = '.json') then
          Result := 'application/json; charset=utf-8'
        else
          if (Ext = '.webmanifest') then
            Result := 'application/manifest+json; charset=utf-8'
          else
          if (Ext = '.m3u8') then
            Result := 'application/vnd.apple.mpegurl'
          else
            if (Ext = '.m4s') then
              Result := 'video/mp4'
            else
              if (Ext = '.mp4') then
                Result := 'video/mp4'
              else
                if (Ext = '.mp3') then
                  Result := 'audio/mpeg'
                else
                  if (Ext = '.aac') then
                    Result := 'audio/aac'
                  else
                    if (Ext = '.flac') then
                      Result := 'audio/flac'
                    else
                      if (Ext = '.wav') then
                        Result := 'audio/wav'
                      else
                        if (Ext = '.jpg') then
                          Result := 'image/jpeg'
                        else
                          if (Ext = '.jpeg') then
                            Result := 'image/jpeg'
                          else
                            if (Ext = '.png') then
                              Result := 'image/png'
                            else
                              if (Ext = '.gif') then
                                Result := 'image/gif'
                            else
                              if (Ext = '.svg') then
                                Result := 'image/svg+xml'
                              else
                                if (Ext = '.ico') then
                                  Result := 'image/x-icon'
                                else
                                  if (Ext = '.webp') then
                                    Result := 'image/webp'
                                  else
                                    if (Ext = '.txt') then
                                      Result := 'text/plain; charset=utf-8'
                                    else
                                      if (Ext = '.xml') then
                                        Result := 'application/xml; charset=utf-8'
                                      else Result := 'application/octet-stream';
end;


function HexValue(AChar: Char;
                  out AValue: Byte): Boolean;
begin

  Result := True;

  case AChar of
    '0'..'9': AValue := Ord(AChar) - Ord('0');
    'a'..'f': AValue := Ord(AChar) - Ord('a') + 10;
    'A'..'F': AValue := Ord(AChar) - Ord('A') + 10;
  else
    AValue := 0;

    Result := False;
  end;
end;


function DecodeUrlPath(const AValue: string;
                       out ADecoded: string): Boolean;
var
  Bytes: TBytes;
  I: Integer;
  Count: Integer;
  HiValue: Byte;
  LoValue: Byte;
  Ch: Char;

begin

  Result := False;
  ADecoded := '';
  SetLength(Bytes,
            Length(AValue) * 3);
  I := 1;
  Count := 0;

  while (I <= Length(AValue)) do
    begin
      Ch := AValue[I];

      if (Ch = '%') then
        begin
          if (I + 2 > Length(AValue)) or not
              HexValue(AValue[I + 1], HiValue) or not
              HexValue(AValue[I + 2], LoValue) then
            Exit;

          Bytes[Count] := (HiValue shl 4) or LoValue;
          Inc(Count);
          Inc(I,
              3);
        end
      else
        begin
          if (Ord(Ch) > 127) then
            Exit;
          Bytes[Count] := Byte(Ord(Ch));
          Inc(Count);
          Inc(I);
        end;
    end;

  SetLength(Bytes,
            Count);
  ADecoded := TEncoding.UTF8.GetString(Bytes);
  Result := (Pos(#0,
                ADecoded) = 0);
end;


function IsBlockedPath(const APath: string): Boolean;
var
  PathText: string;

begin

  PathText := LowerCase(APath);

  Result := (PathText = '/admin') or
            (Copy(PathText,
                  1,
                  7) = '/admin/') or
            (PathText = '/admin.xsl') or
            (Copy(PathText,
                  1,
                  11) = '/admin.xsl/');
end;


function ParseByteRange(const AValue:
                        string; AFileSize: Int64;
                        out AStart: Int64;
                        out AFinish: Int64): Boolean;
var
  Text: string;
  StartText: string;
  EndText: string;
  P: Integer;
  Suffix: Int64;

begin

  Result := False;
  AStart := 0;
  AFinish := AFileSize - 1;
  Text := Trim(AValue);

  if not SameText(Copy(Text,
                       1,
                       6), 'bytes=') then
    Exit;

  Delete(Text,
         1,
         6);

  if Pos(',',
         Text) > 0 then
    Exit;

  P := Pos('-',
           Text);
  if (P = 0) then
    Exit;

  StartText := Trim(Copy(Text,
                         1,
                         P - 1));

  EndText := Trim(Copy(Text,
                       P + 1,
                       MaxInt));

  if (StartText = '') then
    begin
      if not TryStrToInt64(EndText,
                           Suffix) or (Suffix <= 0) then
        Exit;

      if (Suffix > AFileSize) then
        Suffix := AFileSize;

      AStart := AFileSize - Suffix;
    end
  else
    begin
      if not TryStrToInt64(StartText,
                           AStart) then
        Exit;

      if (EndText <> '') then
        begin
          if not TryStrToInt64(EndText,
                               AFinish) then
            Exit;
        end;
    end;

  if (AStart < 0) or (AStart >= AFileSize) then
    Exit;

  if (AFinish >= AFileSize) then
    AFinish := AFileSize - 1;

  Result := (AFinish >= AStart);
end;


function ResolveIPv4(const AHost: string;
                     out AAddress: u_long): Boolean;
var
  HostEntry: PHostEnt;
  HostName: AnsiString;

begin

  HostName := AnsiString(AHost);
  AAddress := inet_addr(PAnsiChar(HostName));

  if (AAddress <> High(u_long)) then
    Exit(True);

  HostEntry := gethostbyname(PAnsiChar(HostName));

  Result := Assigned(HostEntry) and Assigned(HostEntry^.h_addr_list) and Assigned(HostEntry^.h_addr_list^);

  if Result then
    Move(HostEntry^.h_addr_list^^,
         AAddress,
         SizeOf(AAddress));
end;


constructor TFxServeAcceptThread.Create(AServer: TFxServeServer);
begin

  inherited Create(True);

  FServer := AServer;
  FreeOnTerminate := False;
end;


procedure TFxServeAcceptThread.Execute();
begin

  FServer.AcceptClients();
end;

constructor TFxServeClientThread.Create(AServer: TFxServeServer;
                                        ASocket: TSocket;
                                        const APeerAddress: string);
begin

  inherited Create(True);

  FServer := AServer;
  FSocket := ASocket;
  FPeerAddress := APeerAddress;
  FreeOnTerminate := True;
end;


procedure TFxServeClientThread.Execute();
begin

  try
    FServer.HandleClient(FSocket,
                         FPeerAddress);

  finally
    FServer.UnregisterClient(FSocket);
    shutdown(FSocket,
             SD_BOTH);
    closesocket(FSocket);
  end;
end;


constructor TFxServeServer.Create(AConfig: TFxServeConfig;
                                  ALogger: TFxServeLogger);
begin

  inherited Create();

  FConfig := AConfig;
  FLogger := ALogger;
  if FConfig.ProtectionEnabled and not FConfig.WanEnabled then
    FProtection := TFxServeRequestProtection.Create(
      FConfig.ProtectionRequestsPerMinute,
      FConfig.ProtectionBurst,
      FConfig.ProtectionMaxConcurrentPerAddress,
      FConfig.ProtectionBlockSeconds);
  FListenSocket := INVALID_SOCKET;
  FClients := TList<TSocket>.Create;
  FClientsLock := TCriticalSection.Create;
end;


destructor TFxServeServer.Destroy();
begin

  Stop();

  FProtection.Free;
  FClientsLock.Free;
  FClients.Free;

  inherited Destroy();
end;


procedure TFxServeServer.Start();
var
  WsaData: TWSAData;
  Address: TSockAddrIn;
  BindIp: u_long;
  ReuseAddress: Integer;

begin

  if FRunning then
    Exit;

  if not DirectoryExists(FConfig.WebRoot) then
    raise Exception.CreateFmt('Web root not found: %s',
                              [FConfig.WebRoot]);

  if (WSAStartup($0202,
                WsaData) <> 0) then
    raise Exception.Create('WSAStartup failed.');

  FWSAStarted := True;

  try
    if not ResolveIPv4(FConfig.BindAddress,
                       BindIp) then
      raise Exception.CreateFmt('Cannot resolve bind address: %s',
                                [FConfig.BindAddress]);

    FListenSocket := socket(AF_INET,
                            SOCK_STREAM,
                            IPPROTO_TCP);

    if (FListenSocket = INVALID_SOCKET) then
      raise Exception.CreateFmt('socket failed (%d).',
                                [WSAGetLastError]);

    ReuseAddress := 1;

    setsockopt(FListenSocket,
               SOL_SOCKET,
               SO_REUSEADDR,
               PAnsiChar(@ReuseAddress),
               SizeOf(ReuseAddress));

    FillChar(Address,
             SizeOf(Address),
             0);

    Address.sin_family := AF_INET;
    Address.sin_port := htons(FConfig.Port);
    Address.sin_addr.S_addr := BindIp;

    if (bind(FListenSocket,
             Address,
             SizeOf(Address)) = SOCKET_ERROR) then
      raise Exception.CreateFmt('bind failed on %s:%d (%d).',
                                [FConfig.BindAddress, FConfig.Port, WSAGetLastError]);

    if (listen(FListenSocket,
              SOMAXCONN) = SOCKET_ERROR) then
      raise Exception.CreateFmt('listen failed (%d).',
        [WSAGetLastError]);

    FRunning := True;
    FAcceptThread := TFxServeAcceptThread.Create(Self);
    FAcceptThread.Start();

    FLogger.Info(Format('FxServe listening on http://%s:%d; root=%s',
                        [FConfig.BindAddress, FConfig.Port, FConfig.WebRoot]));
  except

    FRunning := False;
    if (FListenSocket <> INVALID_SOCKET) then
      begin
        shutdown(FListenSocket,
                 SD_BOTH);
        closesocket(FListenSocket);
        FListenSocket := INVALID_SOCKET;
      end;

    if Assigned(FAcceptThread) then
      begin
        FAcceptThread.WaitFor;
        FreeAndNil(FAcceptThread);
      end;

    if FWSAStarted then
      begin
        WSACleanup();
        FWSAStarted := False;
      end;

    raise;
  end;
end;


procedure TFxServeServer.Stop();
var
  I: Integer;

begin

  if not FRunning and not Assigned(FAcceptThread) then
    Exit;

  FRunning := False;

  if (FListenSocket <> INVALID_SOCKET) then
    begin
      shutdown(FListenSocket,
               SD_BOTH);
      closesocket(FListenSocket);
      FListenSocket := INVALID_SOCKET;
    end;

  if Assigned(FAcceptThread) then
    begin
      FAcceptThread.WaitFor;
      FreeAndNil(FAcceptThread);
    end;

  FClientsLock.Acquire;

  try
    for I := 0 to FClients.Count - 1 do
      shutdown(FClients[I],
               SD_BOTH);
  finally
    FClientsLock.Release();
  end;

  while (InterlockedCompareExchange(FActiveConnections,
                                    0,
                                    0) <> 0) do
    begin
      Sleep(50);
    end;

  if FWSAStarted then
    begin
      WSACleanup;
      FWSAStarted := False;
    end;

  FLogger.Info('FxServe stopped.');
end;


procedure TFxServeServer.RegisterClient(ASocket: TSocket);
begin

  FClientsLock.Acquire;

  try
    FClients.Add(ASocket);
    InterlockedIncrement(FActiveConnections);

  finally
    FClientsLock.Release;
  end;
end;


procedure TFxServeServer.UnregisterClient(ASocket: TSocket);
begin

  FClientsLock.Acquire;

  try
    FClients.Remove(ASocket);
    InterlockedDecrement(FActiveConnections);

  finally
    FClientsLock.Release;
  end;
end;


procedure TFxServeServer.AcceptClients();
var
  ClientSocket: TSocket;
  ClientThread: TFxServeClientThread;
  Peer: TSockAddrIn;
  PeerSize: Integer;
  PeerAddress: string;
  TimeoutValue: Integer;

begin

  while FRunning do
    begin
      PeerSize := SizeOf(Peer);
      ClientSocket := accept(FListenSocket,
                             @Peer,
                             @PeerSize);

    if (ClientSocket = INVALID_SOCKET) then
    begin
      if FRunning then
        FLogger.Error(Format('accept failed (%d).',
                             [WSAGetLastError]));
      Break;
    end;

    PeerAddress := string(AnsiString(inet_ntoa(Peer.sin_addr)));

    if InterlockedCompareExchange(FActiveConnections,
                                  0,
                                  0) >= FConfig.MaxConnections then
      begin
        SendError(ClientSocket,
                  503,
                  HttpReason(503),
                  'Too many connections.');
        closesocket(ClientSocket);

        Continue;
      end;

    TimeoutValue := FConfig.HeaderTimeoutMs;

    setsockopt(ClientSocket,
               SOL_SOCKET,
               SO_RCVTIMEO,
               PAnsiChar(@TimeoutValue),
               SizeOf(TimeoutValue));

    TimeoutValue := FConfig.SendTimeoutMs;

    setsockopt(ClientSocket,
               SOL_SOCKET,
               SO_SNDTIMEO,
               PAnsiChar(@TimeoutValue),
                         SizeOf(TimeoutValue));

    RegisterClient(ClientSocket);

    ClientThread := TFxServeClientThread.Create(Self,
                                                ClientSocket,
                                                PeerAddress);
    ClientThread.Start();
  end;
end;


function TFxServeServer.ReceiveHeader(ASocket: TSocket;
                                      out AHeader: AnsiString): Boolean;
var
  Buffer: array[0..4095] of AnsiChar;
  Count, OldLength: Integer;

begin

  Result := False;
  AHeader := '';

  repeat
    Count := recv(ASocket,
                  Buffer,
                  SizeOf(Buffer),
                  0);
    if (Count <= 0) then
      Exit;

    OldLength := Length(AHeader);
    SetLength(AHeader,
              OldLength + Count);

    Move(Buffer[0],
         AHeader[OldLength + 1],
         Count);

    if (Pos(#13#10#13#10,
            string(AHeader)) > 0) then
      Exit(True);

  until (Length(AHeader) >= MAX_HEADER_SIZE);
end;


function TFxServeServer.SendAll(ASocket: TSocket;
                                const ABuffer;
                                const ASize: Integer): Boolean;
var
  Sent, Total: Integer;
  Data: PAnsiChar;

begin

  Result := False;
  Data := @ABuffer;
  Total := 0;

  while (Total < ASize) do
  begin
    Sent := send(ASocket,
                 Data[Total],
                 ASize - Total,
                 0);
    if (Sent <= 0) then
      Exit;

    Inc(Total,
        Sent);
  end;

  Result := True;
end;


function TFxServeServer.SendText(ASocket: TSocket;
                                 AText: RawByteString): Boolean;
begin

  Result := (Length(AText) = 0) or SendAll(ASocket,
                                           AText[1],
                                           Length(AText));
end;


procedure TFxServeServer.SendError(ASocket: TSocket;
                                   AStatus: Integer;
                                   const AReason: string;
                                   const AMessage: string;
                                   const AExtraHeaderName: string;
                                   const AExtraHeaderValue: string);
var
  Body: UTF8String;
  Header: UTF8String;
  ExtraHeader: string;

begin

  Body := UTF8String(AMessage + #13#10);

  ExtraHeader := '';
  if AExtraHeaderName <> '' then
    ExtraHeader := AExtraHeaderName + ': ' + AExtraHeaderValue + #13#10;

  Header := UTF8String(Format('HTTP/1.1 %d %s'#13#10 +
    'Content-Type: text/plain; charset=utf-8'#13#10 +
    'Content-Length: %d'#13#10 +
    'Cache-Control: no-store'#13#10 +
    '%s' +
    'Connection: close'#13#10#13#10,
    [AStatus, AReason, Length(Body), ExtraHeader]));

  SendText(ASocket,
           Header);

  SendText(ASocket,
           Body);
end;


procedure TFxServeServer.SendViewerSnapshot(ASocket: TSocket;
                                            const AMethod: string);
var
  Body: UTF8String;
  Header: UTF8String;
begin
  Body := WebCamViewerSnapshotJson();
  Header := UTF8String(Format('HTTP/1.1 200 OK'#13#10 +
    'Content-Type: application/json; charset=utf-8'#13#10 +
    'Content-Length: %d'#13#10 +
    'Cache-Control: no-store'#13#10 +
    'Access-Control-Allow-Origin: *'#13#10 +
    'Connection: close'#13#10#13#10,
    [Length(Body)]));

  SendText(ASocket, Header);
  if AMethod <> 'HEAD' then
    SendText(ASocket, Body);
end;


procedure TFxServeServer.HandleClient(ASocket: TSocket;
                                      const APeerAddress: string);
var
  RawHeader: AnsiString;
  Header: string;
  FirstLine: string;
  MethodName: string;
  Target: string;
  Path: string;
  P1: Integer;
  P2: Integer;
  QueryPos: Integer;
  RetryAfterSeconds: Integer;
  ProtectionResult: TFxServeProtectionResult;

begin

  if not ReceiveHeader(ASocket,
                       RawHeader) then
    Exit;

  Header := string(RawHeader);
  P1 := Pos(#13#10, Header);

  if (P1 = 0) then
    begin
      SendError(ASocket,
                400,
                HttpReason(400),
                'Malformed request.');
      Exit;
    end;

  FirstLine := Copy(Header,
                    1,
                    P1 - 1);
  P1 := Pos(' ',
            FirstLine);

  if (P1 = 0) then
    begin
      SendError(ASocket,
                400,
                HttpReason(400),
                'Malformed request line.');
      Exit;
    end;

  MethodName := UpperCase(Copy(FirstLine,
                               1,
                               P1 - 1));
  Delete(FirstLine,
         1,
         P1);

  P2 := Pos(' ',
            FirstLine);

  if (P2 = 0) then
    begin
      SendError(ASocket,
                400,
                HttpReason(400),
                'Malformed request target.');
      Exit;
    end;

  Target := Copy(FirstLine,
                 1,
                 P2 - 1);

  if (Target = '') or (Target[1] <> '/') then
    begin
      SendError(ASocket,
                400,
                HttpReason(400),
                'Only origin-form targets are accepted.');
      Exit;
    end;

  QueryPos := Pos('?',
                  Target);

  if (QueryPos > 0) then
    Path := Copy(Target,
                 1,
                 QueryPos - 1)
  else
    Path := Target;

  if Assigned(FProtection) then
    begin
      ProtectionResult := FProtection.TryBeginRequest(APeerAddress,
                                                       RetryAfterSeconds);
      if ProtectionResult <> prAllowed then
        begin
          FLogger.Info(Format('Request limited peer=%s', [APeerAddress]));
          SendError(ASocket,
                    429,
                    'Too Many Requests',
                    'Too many requests. Try again later.',
                    'Retry-After',
                    IntToStr(RetryAfterSeconds));
          Exit;
        end;
    end;

  try
    FLogger.Info(Format('%s %s %s',
                        [APeerAddress, MethodName, Path]));

    if SameText(Path, '/WebCam/live.json') then
      TouchWebCamViewer(Target, APeerAddress);

    if IsBlockedPath(Path) then
      begin
      SendError(ASocket,
                403,
                HttpReason(403),
                'Forbidden.');
        Exit;
      end;

    if (MethodName <> 'GET') and
       (MethodName <> 'HEAD') and
       (MethodName <> 'OPTIONS') then
      begin
      SendError(ASocket,
                405,
                HttpReason(405),
                'Method not allowed.');
        Exit;
      end;

    if (MethodName = 'OPTIONS') then
      begin
      SendText(ASocket,
               'HTTP/1.1 204 No Content'#13#10 +
               'Access-Control-Allow-Origin: *'#13#10 +
               'Access-Control-Allow-Methods: GET, HEAD, OPTIONS'#13#10 +
               'Access-Control-Allow-Headers: Content-Type, Accept-Encoding, Range'#13#10 +
               'Content-Length: 0'#13#10'Connection: close'#13#10#13#10);
        Exit;
      end;

    if SameText(Path, '/WebCam/viewers.json') then
      begin
      SendViewerSnapshot(ASocket, MethodName);
        Exit;
      end;

    if FConfig.IsProxyRoute(Path) then
      ProxyRequest(ASocket,
                   MethodName,
                   Target,
                   Header)
    else
      ServeStatic(ASocket,
                  MethodName,
                  Path,
                  Header);
  finally
    if Assigned(FProtection) then
      FProtection.EndRequest(APeerAddress);
  end;
end;


procedure TFxServeServer.ServeStatic(ASocket: TSocket;
                                     const AMethod: string;
                                     const APath: string;
                                     const ARequestHeader: string);
var
  DecodedPath: string;
  RelativePath: string;
  FileName: string;
  RootPrefix: string;
  Segments: TStringList;
  I: Integer;
  Stream: TFileStream;
  FileSize: Int64;
  StartOffset: Int64;
  EndOffset: Int64;

  Remaining: Int64;
  RangeText: string;
  IsPartial: Boolean;
  Header: UTF8String;
  Buffer: TBytes;
  Count: Integer;

begin

  if not DecodeUrlPath(APath,
                       DecodedPath) then
    begin
      SendError(ASocket,
                400,
                HttpReason(400),
                'Invalid URL encoding.');
      Exit;
    end;

  if (Pos('\',
         DecodedPath) > 0) then
    begin
      SendError(ASocket,
                403,
                HttpReason(403),
                'Invalid path.');
      Exit;
    end;

  Segments := TStringList.Create();

  try
    Segments.StrictDelimiter := True;
    Segments.Delimiter := '/';
    Segments.DelimitedText := DecodedPath;
    RelativePath := '';

    for I := 0 to Segments.Count - 1 do
    begin
      if (Segments[I] = '') then
        Continue;

      if (Segments[I] = '.') or
         (Segments[I] = '..') or
         (Pos(':', Segments[I]) > 0) or
         (Pos(#0, Segments[I]) > 0) then
        begin
          SendError(ASocket,
                    403,
                    HttpReason(403),
                    'Invalid path.');
          Exit;
        end;

      RelativePath := IncludeTrailingPathDelimiter(RelativePath) + Segments[I];
    end;

  finally
    Segments.Free;
  end;

  if (RelativePath = '') then
    RelativePath := FConfig.IndexFile;

  FileName := ExpandFileName(IncludeTrailingPathDelimiter(FConfig.WebRoot) + RelativePath);

  if DirectoryExists(FileName) then
    FileName := IncludeTrailingPathDelimiter(FileName) + FConfig.IndexFile;

  RootPrefix := IncludeTrailingPathDelimiter(ExpandFileName(FConfig.WebRoot));

  if not SameText(Copy(FileName,
                       1,
                       Length(RootPrefix)),
                       RootPrefix) then
    begin
      SendError(ASocket,
                403,
                HttpReason(403),
                'Invalid path.');
      Exit;
    end;

  if not FileExists(FileName) then
    begin
      SendError(ASocket,
                404,
                HttpReason(404),
                'Not found.');
      Exit;
    end;

  Stream := TFileStream.Create(FileName,
                               fmOpenRead or fmShareDenyNone);
  try
    FileSize := Stream.Size;
    StartOffset := 0;
    EndOffset := FileSize - 1;
    RangeText := HeaderValue(ARequestHeader,
                             'Range');
    IsPartial := False;

    if (RangeText <> '') then
      begin
        if not ParseByteRange(RangeText,
                              FileSize,
                              StartOffset,
                              EndOffset) then
          begin
            SendText(ASocket,
                     UTF8String(Format('HTTP/1.1 416 Range Not Satisfiable'#13#10 +
                                       'Content-Range: bytes */%d'#13#10 +
                                       'Content-Length: 0'#13#10'Connection: close'#13#10#13#10,
                                       [FileSize])));
            Exit;
          end;

        IsPartial := True;
      end;

    Remaining := EndOffset - StartOffset + 1;

    if IsPartial then
      Header := UTF8String(Format('HTTP/1.1 206 Partial Content'#13#10 +
                                  'Content-Range: bytes %d-%d/%d'#13#10,
                                  [StartOffset, EndOffset, FileSize]))
    else
      Header := 'HTTP/1.1 200 OK'#13#10;


    Header := Header + UTF8String('Content-Type: ' + MimeTypeForFile(FileName) + #13#10 +
                                  'Accept-Ranges: bytes'#13#10 +
                                  'Content-Length: ' + IntToStr(Remaining) + #13#10);

    if FConfig.CorsEnabled then
      Header := Header + 'Access-Control-Allow-Origin: *'#13#10 +
                         'Access-Control-Expose-Headers: Accept-Ranges, Content-Length, Content-Range'#13#10;

    if FConfig.IsNoStoreRoute(APath) then
      Header := Header + 'Cache-Control: no-store, no-cache, must-revalidate'#13#10;

    Header := Header + 'Connection: close'#13#10#13#10;

    if not SendText(ASocket,
                    Header) or SameText(AMethod,
                                        'HEAD') then
      Exit;

    Stream.Position := StartOffset;
    SetLength(Buffer,
              FILE_BUFFER_SIZE);

    while (Remaining > 0) do
      begin
        if (Remaining < Length(Buffer)) then
          Count := Remaining
        else
          Count := Length(Buffer);

        Count := Stream.Read(Buffer[0],
                             Count);
        if (Count <= 0) or not SendAll(ASocket,
                                       Buffer[0],
                                       Count) then
          Break;

        Dec(Remaining,
            Count);
      end;

  finally
    Stream.Free;
  end;
end;


procedure TFxServeServer.ProxyRequest(ASocket: TSocket;
                                      const AMethod: string;
                                      const ATarget: string;
                                      const ARequestHeader: string);
var
  Upstream: TSocket;
  Address: TSockAddrIn;
  IpAddress: u_long;
  RequestText: AnsiString;
  ResponseHeader: AnsiString;
  OutputHeader: AnsiString;
  Buffer: array[0..32767] of AnsiChar;
  Count: Integer;
  HeaderEnd: Integer;
  Value: string;

begin

  if not ResolveIPv4(FConfig.ProxyHost,
                     IpAddress) then
    begin
      SendError(ASocket,
                502,
                HttpReason(502),
                'Cannot resolve upstream host.');
      Exit;
    end;

  Upstream := socket(AF_INET,
                     SOCK_STREAM,
                     IPPROTO_TCP);

  if (Upstream = INVALID_SOCKET) then
    begin
      SendError(ASocket,
                502,
                HttpReason(502),
                'Cannot create upstream socket.');
      Exit;
    end;

  try
    FillChar(Address,
             SizeOf(Address),
             0);

    Address.sin_family := AF_INET;
    Address.sin_port := htons(FConfig.ProxyPort);
    Address.sin_addr.S_addr := IpAddress;

    if (connect(Upstream,
                Address,
                SizeOf(Address)) = SOCKET_ERROR) then
      begin
        SendError(ASocket,
                  502,
                  HttpReason(502),
                  'Upstream is unavailable.');
        Exit;
      end;

    RequestText := AnsiString(AMethod + ' ' +
                              ATarget + ' HTTP/1.1'#13#10 +
                              'Host: ' + FConfig.ProxyHost + ':' +
                              IntToStr(FConfig.ProxyPort) + #13#10);
    Value := HeaderValue(ARequestHeader,
                         'User-Agent');

    if (Value <> '') then
      RequestText := RequestText + AnsiString('User-Agent: ' + Value + #13#10);
    Value := HeaderValue(ARequestHeader, 'Accept');

    if (Value <> '') then
      RequestText := RequestText + AnsiString('Accept: ' + Value + #13#10);

    Value := HeaderValue(ARequestHeader, 'Range');

    if (Value <> '') then
      RequestText := RequestText + AnsiString('Range: ' + Value + #13#10);
    Value := HeaderValue(ARequestHeader, 'Icy-MetaData');

    if (Value <> '') then
      RequestText := RequestText + AnsiString('Icy-MetaData: ' + Value + #13#10);
    Value := HeaderValue(ARequestHeader, 'Origin');

    if (Value <> '') then
      RequestText := RequestText + AnsiString('Origin: ' + Value + #13#10);
    RequestText := RequestText + 'Connection: close'#13#10#13#10;

    if not SendAll(Upstream,
                   RequestText[1],
                   Length(RequestText)) then
      begin
        SendError(ASocket,
                  502,
                  HttpReason(502),
                  'Cannot send upstream request.');
        Exit;
      end;

    ResponseHeader := '';

    repeat
      Count := recv(Upstream,
                    Buffer,
                    SizeOf(Buffer),
                    0);

      if (Count <= 0) then
      begin
        SendError(ASocket,
                  502,
                  HttpReason(502),
                  'Upstream closed without a response.');
        Exit;
      end;

      SetLength(ResponseHeader,
                Length(ResponseHeader) + Count);

      Move(Buffer[0],
           ResponseHeader[Length(ResponseHeader) - Count + 1],
           Count);

      HeaderEnd := Pos(#13#10#13#10,
                       string(ResponseHeader));

    until (HeaderEnd > 0) or (Length(ResponseHeader) >= MAX_HEADER_SIZE);

    if (HeaderEnd = 0) then
    begin
      SendError(ASocket,
                502,
                HttpReason(502),
                'Upstream response header is too large.');
      Exit;
    end;

    OutputHeader := PrepareProxyResponseHeader(Copy(ResponseHeader,
                                                    1,
                                                    HeaderEnd - 1),
                                                    FConfig.CorsEnabled);
    if not SendText(ASocket,
                    OutputHeader) then
      Exit;

    if (Length(ResponseHeader) > HeaderEnd + 3) then
      if not SendAll(ASocket,
                     ResponseHeader[HeaderEnd + 4],
                     Length(ResponseHeader) - HeaderEnd - 3) then
        Exit;

    repeat
      Count := recv(Upstream,
                    Buffer,
                    SizeOf(Buffer),
                    0);

      if (Count > 0) then
        if not SendAll(ASocket,
                       Buffer[0],
                       Count) then
          Break;
    until (Count <= 0);

  finally
    shutdown(Upstream,
             SD_BOTH);
    closesocket(Upstream);
  end;
end;


end.

// FactoryX
//
// Copyright � FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Wan.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Wan unit.
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
unit FxServe.Wan;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {FxServe}
  FxServe.Config,
  FxServe.Logging,
  FxServe.HttpApi,
  FxServe.Certificate;

type
  TFxServeWanServer = class;

  TFxServeWanAcceptThread = class(TThread)
  private
    FServer: TFxServeWanServer;

  protected
    procedure Execute; override;

  public

    constructor Create(AServer: TFxServeWanServer);
  end;

  TFxServeWanRequestThread = class(TThread)
  private
    FServer: TFxServeWanServer;
    FRequestId: UInt64;
    FMethod: string;
    FTarget: string;
    FHeaders: string;
    FSecure: Boolean;

  protected
    procedure Execute(); override;

  public

    constructor Create(AServer: TFxServeWanServer;
                       ARequestId: UInt64;
                       const AMethod: string;
                       const ATarget: string;
                       const AHeaders: string;
                       const ASecure: Boolean);
  end;

  TFxServeWanServer = class
  private
    FConfig: TFxServeConfig;
    FLogger: TFxServeLogger;
    FChallengeSource: IFxServeAcmeChallengeSource;
    FRequestQueue: THandle;
    FAcceptThread: TFxServeWanAcceptThread;
    FHttpPrefix: string;
    FHttpsPrefix: string;
    FRunning: Boolean;
    FWSAStarted: Boolean;
    FHttpInitialized: Boolean;
    FActiveRequests: Integer;
    FIdleEvent: TEvent;

    procedure AcceptRequests();

    procedure HandleRequest(ARequestId: UInt64;
                            const AMethod, ATarget,
                            AHeaders: string;
                            ASecure: Boolean);

    procedure RequestStarted();
    procedure RequestFinished();

    function ProxyToLan(ARequestId: UInt64;
                        const AMethod: string;
                        const ATarget: string;
                        const AHeaders: string): Boolean;

    function SendSimple(ARequestId: UInt64;
                        AStatus: Word;
                        const AReason: string;
                        const ABody: string;
                        const AExtraHeaderName: string = '';
                        const AExtraHeaderValue: string = ''): Boolean;
  public

    constructor Create(AConfig: TFxServeConfig;
                       ALogger: TFxServeLogger;
                       const AChallengeSource: IFxServeAcmeChallengeSource);
    destructor Destroy(); override;

    procedure Start();
    procedure Stop();

    property Running: Boolean read FRunning;
  end;


implementation

const
  WAN_HEADER_BUFFER_SIZE = 65536;
  WAN_IO_BUFFER_SIZE = 65536;
  WAN_MAX_UPSTREAM_HEADER = 65536;

  REQUEST_HEADER_NAMES: array[0..40] of string = ('Cache-Control',
                                                  'Connection',
                                                  'Date',
                                                  'Keep-Alive',
                                                  'Pragma',
                                                  'Trailer',
                                                  'Transfer-Encoding',
                                                  'Upgrade',
                                                  'Via',
                                                  'Warning',
                                                  'Allow',
                                                  'Content-Length',
                                                  'Content-Type',
                                                  'Content-Encoding',
                                                  'Content-Language',
                                                  'Content-Location',
                                                  'Content-MD5',
                                                  'Content-Range',
                                                  'Expires',
                                                  'Last-Modified',
                                                  'Accept',
                                                  'Accept-Charset',
                                                  'Accept-Encoding',
                                                  'Accept-Language',
                                                  'Authorization',
                                                  'Cookie',
                                                  'Expect',
                                                  'From',
                                                  'Host',
                                                  'If-Match',
                                                  'If-Modified-Since',
                                                  'If-None-Match',
                                                  'If-Range',
                                                  'If-Unmodified-Since',
                                                  'Max-Forwards',
                                                  'Proxy-Authorization',
                                                  'Referer',
                                                  'Range',
                                                  'TE',
                                                  'Translate',
                                                  'User-Agent');

function HttpApiErrorText(AError: Cardinal): string;
begin

  Result := Format('%s (%d)',
                   [SysErrorMessage(AError), AError]);
end;


function AnsiField(APointer: PAnsiChar;
                   ALength: Word): string;
var
  Value: AnsiString;

begin
  if (APointer = nil) or (ALength = 0) then
    Exit('');

  SetString(Value,
            APointer,
            ALength);

  Result := string(Value);
end;


function WideField(APointer: PWideChar;
                   ALengthInBytes: Word): string;
begin

  if (APointer = nil) or (ALengthInBytes = 0) then
    Exit('');

  SetString(Result,
            APointer,
            ALengthInBytes div SizeOf(Char));
end;


function RequestVerbName(ARequest: PHttpRequest): string;
begin

  case ARequest^.Verb of
    HttpVerbOPTIONS: Result := 'OPTIONS';
    HttpVerbGET:     Result := 'GET';
    HttpVerbHEAD:    Result := 'HEAD';
  else
    Result := UpperCase(AnsiField(ARequest^.pUnknownVerb,
                                  ARequest^.UnknownVerbLength));
  end;
end;


function CopyRequestHeaders(ARequest: PHttpRequest): string;
var
  I: Integer;
  NameText: string;
  ValueText: string;

begin

  Result := '';

  for I := 0 to HttpHeaderRequestMaximum - 1 do
    begin
     ValueText := AnsiField(ARequest^.Headers.KnownHeaders[I].pRawValue,
                             ARequest^.Headers.KnownHeaders[I].RawValueLength);

      if (ValueText <> '') then
        Result := Result + REQUEST_HEADER_NAMES[I] + ': ' + ValueText + #13#10;
    end;

  for I := 0 to ARequest^.Headers.UnknownHeaderCount - 1 do
    begin
      NameText := AnsiField(PHttpUnknownHeaderArray(ARequest^.Headers.pUnknownHeaders)^[I].pName,
                                                    PHttpUnknownHeaderArray(ARequest^.Headers.pUnknownHeaders)^[I].NameLength);

    ValueText := AnsiField(PHttpUnknownHeaderArray(ARequest^.Headers.pUnknownHeaders)^[I].pRawValue,
                                                   PHttpUnknownHeaderArray(ARequest^.Headers.pUnknownHeaders)^[I].RawValueLength);
    if (NameText <> '') then
      Result := Result + NameText + ': ' + ValueText + #13#10;
    end;
end;


function HeaderValue(const AHeaders: string;
                     const AName: string): string;
var
  Lines: TStringList;
  I: Integer;
  P: Integer;

begin

  Result := '';
  Lines := TStringList.Create();

  try
    Lines.Text := StringReplace(AHeaders,
                                #13#10,
                                sLineBreak,
                                [rfReplaceAll]);

    for I := 0 to Lines.Count - 1 do
      begin
        P := Pos(':', Lines[I]);

        if (P > 0) and SameText(Trim(Copy(Lines[I],
                                          1,
                                          P - 1)),
                                AName) then
          Exit(Trim(Copy(Lines[I],
                         P + 1,
                         MaxInt)));
      end;

  finally
    Lines.Free;
  end;
end;


function HostWithoutPort(const AHost: string): string;
var
  P: Integer;

begin

  Result := Trim(AHost);

  if (Result <> '') and (Result[1] = '[') then
    begin
      P := Pos(']',
               Result);
      if (P > 0) then
        Result := Copy(Result,
                       2,
                       P - 2);
    end
  else
    begin
      P := Pos(':', Result);
      if (P > 0) then
        Result := Copy(Result,
                       1,
                       P - 1);
    end;

  Result := LowerCase(Result);
end;


function SendSocketAll(ASocket: TSocket;
                       const ABuffer;
                       ASize: Integer): Boolean;
var
  Sent: Integer;
  Total: Integer;
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


function ResponseKnownHeaderIndex(const AName: string): Integer;
begin

  if SameText(AName,
              'Cache-Control') then
    Result := HttpHeaderCacheControl
  else
    if SameText(AName,
                'Content-Length') then
      Result := HttpHeaderContentLength
    else
      if SameText(AName,
                  'Content-Type') then
        Result := HttpHeaderContentType
      else
        if SameText(AName,
                    'Content-Range') then
          Result := HttpHeaderContentRange
        else
          if SameText(AName,
                      'Last-Modified') then
            Result := HttpHeaderLastModified
          else
            if SameText(AName,
                        'Accept-Ranges') then
              Result := HttpHeaderAcceptRanges
            else
              if SameText(AName,
                          'ETag') then
                Result := HttpHeaderEtag
              else
                if SameText(AName,
                            'Location') then
                  Result := HttpHeaderLocation
                else
                  if SameText(AName,
                              'Server') then
                    Result := HttpHeaderServer
                  else
                    Result := -1;
end;


function ParseStatusLine(const ALine: string;
                         out AStatus: Word;
                         out AReason: string): Boolean;
var
  P1: Integer;
  P2: Integer;
  Code: Integer;

begin

  Result := False;
  P1 := Pos(' ',
            ALine);

  if (P1 = 0) then
    Exit;

  P2 := Pos(' ',
            Copy(ALine,
                 P1 + 1,
                 MaxInt));

  if (P2 = 0) then
    P2 := Length(ALine) - P1 + 1;

  if not TryStrToInt(Copy(ALine,
                          P1 + 1,
                          P2 - 1),
                     Code) or
     (Code < 100) or (Code > 999) then
    Exit;

  AStatus := Code;
  AReason := Trim(Copy(ALine,
                       P1 + P2 + 1,
                       MaxInt));
  Result := True;
end;


function SendEntity(AQueue: THandle;
                    ARequestId: UInt64;
                    const AData: RawByteString;
                    AMoreData: Boolean;
                    AAutomaticChunking: Boolean): Boolean;
var
  Chunk: THttpDataChunk;
  Flags: Cardinal;
  Sent: Cardinal;
  Error: Cardinal;

begin

  if (Length(AData) = 0) then
    Exit(True);

  FillChar(Chunk,
           SizeOf(Chunk),
           0);

  Chunk.DataChunkType := HttpDataChunkFromMemory;
  Chunk.pBuffer := @AData[1];
  Chunk.BufferLength := Length(AData);
  Flags := 0;

  if AMoreData then
    Flags := Flags or HTTP_SEND_RESPONSE_FLAG_MORE_DATA;


  if AAutomaticChunking and AMoreData then
    Flags := Flags or HTTP_SEND_RESPONSE_FLAG_AUTOMATIC_CHUNKING;

  Sent := 0;

  Error := HttpSendResponseEntityBody(AQueue,
                                      ARequestId,
                                      Flags,
                                      1,
                                      @Chunk,
                                      @Sent,
                                      nil,
                                      0,
                                      nil,
                                      nil);
  Result := Error = NO_ERROR;
end;


function FinishUnknownLengthResponse(AQueue: THandle;
                                     ARequestId: UInt64): Boolean;
var
  Sent: Cardinal;
  Error: Cardinal;

begin

  Sent := 0;

  Error := HttpSendResponseEntityBody(AQueue,
                                      ARequestId,
                                      0,
                                      0,
                                      nil,
                                      @Sent,
                                      nil,
                                      0,
                                      nil,
                                      nil);
  Result := Error = NO_ERROR;
end;


constructor TFxServeWanAcceptThread.Create(AServer: TFxServeWanServer);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FServer := AServer;
end;


procedure TFxServeWanAcceptThread.Execute();
begin

  FServer.AcceptRequests();
end;


constructor TFxServeWanRequestThread.Create(AServer: TFxServeWanServer;
                                            ARequestId: UInt64;
                                            const AMethod: string;
                                            const ATarget: string;
                                            const AHeaders: string;
                                            const ASecure: Boolean);
begin

  inherited Create(True);

  FreeOnTerminate := True;
  FServer := AServer;
  FRequestId := ARequestId;
  FMethod := AMethod;
  FTarget := ATarget;
  FHeaders := AHeaders;
  FSecure := ASecure;
end;


procedure TFxServeWanRequestThread.Execute();
begin

  try
    FServer.HandleRequest(FRequestId,
                          FMethod,
                          FTarget,
                          FHeaders,
                          FSecure);
  finally
    FServer.RequestFinished();
  end;
end;


constructor TFxServeWanServer.Create(AConfig: TFxServeConfig;
                                     ALogger: TFxServeLogger;
                                     const AChallengeSource: IFxServeAcmeChallengeSource);
begin

  inherited Create;

  FConfig := AConfig;
  FLogger := ALogger;
  FChallengeSource := AChallengeSource;
  FRequestQueue := 0;
  FIdleEvent := TEvent.Create(nil,
                              True,
                              True,
                              '');
end;


destructor TFxServeWanServer.Destroy;
begin

  Stop();
  FIdleEvent.Free;

  inherited Destroy();
end;


procedure TFxServeWanServer.RequestStarted();
begin

  FIdleEvent.ResetEvent();
  InterlockedIncrement(FActiveRequests);
end;


procedure TFxServeWanServer.RequestFinished();
begin

  if (InterlockedDecrement(FActiveRequests) = 0) then
    FIdleEvent.SetEvent;
end;


procedure TFxServeWanServer.Start;
var
  Version: THttpApiVersion;
  Error: Cardinal;
  WsaData: TWSAData;

begin

  if FRunning or not FConfig.WanEnabled then
    Exit;

  if (WSAStartup($0202,
                WsaData) <> 0) then
    raise Exception.Create('WAN front end could not initialize Winsock.');

  FWSAStarted := True;
  Version.HttpApiMajorVersion := 1;
  Version.HttpApiMinorVersion := 0;

  Error := HttpInitialize(Version,
                          HTTP_INITIALIZE_SERVER,
                          nil);

  if (Error <> NO_ERROR) then
    begin
      WSACleanup();
      FWSAStarted := False;

      raise Exception.Create('HTTP.sys initialization failed: ' +
      HttpApiErrorText(Error));
    end;

  FHttpInitialized := True;

  try
    Error := HttpCreateHttpHandle(FRequestQueue,
                                  0);
    if (Error <> NO_ERROR) then
      raise Exception.Create('HTTP.sys request queue creation failed: ' + HttpApiErrorText(Error));

    if FConfig.WanHttpEnabled then
      begin
        FHttpPrefix := Format('http://+:%d/',
                              [FConfig.WanHttpPort]);

        Error := HttpAddUrl(FRequestQueue,
                            PWideChar(FHttpPrefix),
                            nil);

        if (Error <> NO_ERROR) then
          raise Exception.Create('Cannot register ' + FHttpPrefix + ': ' + HttpApiErrorText(Error));
      end;

    if FConfig.WanHttpsEnabled then
      begin
        FHttpsPrefix := Format('https://+:%d/', [FConfig.WanHttpsPort]);

        Error := HttpAddUrl(FRequestQueue,
                            PWideChar(FHttpsPrefix),
                            nil);

        if (Error <> NO_ERROR) then
          raise Exception.Create('Cannot register ' + FHttpsPrefix + ': ' + HttpApiErrorText(Error));
      end;

    FRunning := True;
    FAcceptThread := TFxServeWanAcceptThread.Create(Self);
    FAcceptThread.Start();

    FLogger.Info(Format('WAN front end active for %s; HTTP=%s HTTPS=%s',
                        [FConfig.WanHostName, FHttpPrefix, FHttpsPrefix]));
  except
    Stop();
    raise;
  end;
end;


procedure TFxServeWanServer.Stop();
begin

  FRunning := False;

  if (FRequestQueue <> 0) and (FHttpPrefix <> '') then
    HttpRemoveUrl(FRequestQueue, PWideChar(FHttpPrefix));

  if (FRequestQueue <> 0) and (FHttpsPrefix <> '') then
    HttpRemoveUrl(FRequestQueue, PWideChar(FHttpsPrefix));

  if (FRequestQueue <> 0) then
    begin
      CloseHandle(FRequestQueue);
      FRequestQueue := 0;
    end;

  if Assigned(FAcceptThread) then
    begin
      FAcceptThread.WaitFor();
      FreeAndNil(FAcceptThread);
    end;

  FIdleEvent.WaitFor(35000);

  if FHttpInitialized then
    begin
      HttpTerminate(HTTP_INITIALIZE_SERVER,
                    nil);
      FHttpInitialized := False;
    end;

  FHttpPrefix := '';
  FHttpsPrefix := '';

  if FWSAStarted then
    begin
      WSACleanup();
      FWSAStarted := False;
    end;
end;


procedure TFxServeWanServer.AcceptRequests;
var
  Buffer: TBytes;
  Request: PHttpRequest;
  BytesReturned: Cardinal;
  Error: Cardinal;
  RequestId: UInt64;
  MethodName: string;
  Target: string;
  Headers: string;
  Secure: Boolean;
  Worker: TFxServeWanRequestThread;

begin

  SetLength(Buffer,
            WAN_HEADER_BUFFER_SIZE);
  RequestId := HTTP_NULL_ID;

  while FRunning do
    begin
    FillChar(Buffer[0],
              Length(Buffer), 0);
    BytesReturned := 0;

    Error := HttpReceiveHttpRequest(FRequestQueue,
                                    RequestId,
                                    0,
                                    PHttpRequest(@Buffer[0]),
                                    Length(Buffer),
                                    @BytesReturned,
                                    nil);

    if (Error = ERROR_MORE_DATA) and (BytesReturned > Cardinal(Length(Buffer))) then
      begin
        Request := PHttpRequest(@Buffer[0]);
        RequestId := Request^.RequestId;

        SetLength(Buffer,
                  BytesReturned);
        Continue;
      end;

    RequestId := HTTP_NULL_ID;

    if (Error <> NO_ERROR) then
      begin
        if FRunning then
          FLogger.Error('HTTP.sys receive failed: ' + HttpApiErrorText(Error));

        Break;
      end;

    Request := PHttpRequest(@Buffer[0]);
    MethodName := RequestVerbName(Request);
    Target := AnsiField(Request^.pRawUrl,
                        Request^.RawUrlLength);

    if (Target = '') then
      Target := WideField(Request^.CookedUrl.pAbsPath,
                          Request^.CookedUrl.AbsPathLength) + WideField(Request^.CookedUrl.pQueryString,
                                                                        Request^.CookedUrl.QueryStringLength);
    Headers := CopyRequestHeaders(Request);
    Secure := (Request^.pSslInfo <> nil);

    RequestStarted();

    try
      Worker := TFxServeWanRequestThread.Create(Self,
                                                Request^.RequestId,
                                                MethodName,
                                                Target,
                                                Headers,
                                                Secure);
      try
        Worker.Start();
      except
        Worker.Free;
        raise;
      end;

    except
      RequestFinished();
      raise;
    end;

    SetLength(Buffer,
              WAN_HEADER_BUFFER_SIZE);
  end;
end;


procedure TFxServeWanServer.HandleRequest(ARequestId: UInt64;
                                          const AMethod: string;
                                          const ATarget: string;
                                          const AHeaders: string;
                                          ASecure: Boolean);
var
  HostName: string;
  Location: string;
  ChallengeResponse: string;

begin

  HostName := HostWithoutPort(HeaderValue(AHeaders,
                              'Host'));

  FLogger.Info(Format('WAN %s %s host=%s secure=%s',
                      [AMethod, ATarget, HostName, BoolToStr(ASecure, True)]));

  if (FConfig.WanHostName <> '') and
     not SameText(HostName,
                  FConfig.WanHostName) then
    begin
      SendSimple(ARequestId,
                 404,
                 'Not Found',
                 'Not found.');
      Exit;
    end;

  if (AMethod <> 'GET') and
     (AMethod <> 'HEAD') and
     (AMethod <> 'OPTIONS') then
    begin
      SendSimple(ARequestId,
                 405,
                 'Method Not Allowed',
                 'Method not allowed.');
      Exit;
    end;

  if not ASecure and
     Assigned(FChallengeSource) and
     FChallengeSource.TryGetAcmeChallenge(ATarget,
                                          ChallengeResponse) then
    begin
      SendSimple(ARequestId,
                 200,
                 'OK',
                 ChallengeResponse,
                 'Cache-Control',
                 'no-store');
      Exit;
    end;

  // ACME HTTP-01 validators always enter on plain HTTP port 80. Everything
  // except an active in-memory challenge is redirected normally.
  if not ASecure and
     FConfig.WanRedirectHttp and
     FConfig.WanHttpsEnabled and
     (Pos('/.well-known/acme-challenge/',
          LowerCase(ATarget)) <> 1) then
    begin
      Location := 'https://' + FConfig.WanHostName;

      if (FConfig.WanHttpsPort <> 443) then
        Location := Location + ':' + IntToStr(FConfig.WanHttpsPort);

      Location := Location + ATarget;

      SendSimple(ARequestId,
                 308,
                 'Permanent Redirect',
                 '',
                 'Location',
                 Location);
      Exit;
    end;

  if not ProxyToLan(ARequestId,
                    AMethod,
                    ATarget,
                    AHeaders) then
    SendSimple(ARequestId,
               502,
               'Bad Gateway',
               'FxServe LAN listener is unavailable.');
end;


function TFxServeWanServer.SendSimple(ARequestId: UInt64;
                                      AStatus: Word;
                                      const AReason: string;
                                      const ABody: string;
                                      const AExtraHeaderName: string;
                                      const AExtraHeaderValue: string): Boolean;
var
  Response: THttpResponse;
  Chunk: THttpDataChunk;
  ReasonText: RawByteString;
  BodyText: RawByteString;
  ContentTypeText: RawByteString;
  LengthText: RawByteString;
  ExtraNameText: RawByteString;
  ExtraValueText: RawByteString;
  Unknown: THttpUnknownHeader;
  Flags: Cardinal;
  Sent: Cardinal;
  Error: Cardinal;
  ExtraKnownIndex: Integer;

begin

  FillChar(Response,
           SizeOf(Response),
           0);

  FillChar(Chunk,
           SizeOf(Chunk),
           0);

  FillChar(Unknown,
           SizeOf(Unknown),
           0);

  ReasonText := UTF8String(AReason);
  BodyText := UTF8String(ABody);
  ContentTypeText := 'text/plain; charset=utf-8';
  LengthText := AnsiString(IntToStr(Length(BodyText)));

  Response.Version.MajorVersion := 1;
  Response.Version.MinorVersion := 1;
  Response.StatusCode := AStatus;
  Response.ReasonLength := Length(ReasonText);

  if (ReasonText <> '') then
    Response.pReason := @ReasonText[1];

  Response.Headers.KnownHeaders[HttpHeaderContentType].RawValueLength := Length(ContentTypeText);
  Response.Headers.KnownHeaders[HttpHeaderContentType].pRawValue := @ContentTypeText[1];
  Response.Headers.KnownHeaders[HttpHeaderContentLength].RawValueLength := Length(LengthText);
  Response.Headers.KnownHeaders[HttpHeaderContentLength].pRawValue := @LengthText[1];

  if (AExtraHeaderName <> '') then
    begin
      ExtraNameText := UTF8String(AExtraHeaderName);
      ExtraValueText := UTF8String(AExtraHeaderValue);
      ExtraKnownIndex := ResponseKnownHeaderIndex(AExtraHeaderName);

      if (ExtraKnownIndex >= 0) then
        begin
          Response.Headers.KnownHeaders[ExtraKnownIndex].RawValueLength := Length(ExtraValueText);

          if (ExtraValueText <> '') then
            Response.Headers.KnownHeaders[ExtraKnownIndex].pRawValue := @ExtraValueText[1];
        end
      else
        begin
          Unknown.NameLength := Length(ExtraNameText);
          Unknown.RawValueLength := Length(ExtraValueText);
          Unknown.pName := @ExtraNameText[1];
          Unknown.pRawValue := @ExtraValueText[1];
          Response.Headers.UnknownHeaderCount := 1;
          Response.Headers.pUnknownHeaders := @Unknown;
        end;
    end;

  if (BodyText <> '') then
    begin
      Chunk.DataChunkType := HttpDataChunkFromMemory;
      Chunk.pBuffer := @BodyText[1];
      Chunk.BufferLength := Length(BodyText);
      Response.EntityChunkCount := 1;
      Response.pEntityChunks := @Chunk;
    end;

  Flags := HTTP_SEND_RESPONSE_FLAG_DISCONNECT;
  Sent := 0;

  Error := HttpSendHttpResponse(FRequestQueue,
                                ARequestId,
                                Flags,
                                @Response,
                                nil,
                                @Sent,
                                nil,
                                0,
                                nil,
                                nil);
  Result := Error = NO_ERROR;
end;


function TFxServeWanServer.ProxyToLan(ARequestId: UInt64;
                                      const AMethod: string;
                                      const ATarget: string;
                                      const AHeaders: string): Boolean;
var
  Upstream: TSocket;
  Address: TSockAddrIn;
  Timeout: Integer;
  RequestText: RawByteString;
  ResponseData: RawByteString;
  BodyData: RawByteString;
  Part: RawByteString;

  Buffer: array[0..WAN_IO_BUFFER_SIZE - 1] of AnsiChar;
  Count: Integer;
  HeaderEnd: Integer;
  I: Integer;
  P: Integer;
  KnownIndex: Integer;
  UnknownCount: Integer;
  Lines: TStringList;
  Names: TStringList;
  Values: TStringList;
  Status: Word;
  Reason: string;
  NameText: string;
  ValueText: string;
  Response: THttpResponse;
  ReasonBytes: RawByteString;
  HeaderNames: array of RawByteString;
  HeaderValues: array of RawByteString;
  UnknownHeaders: array of THttpUnknownHeader;
  ContentLength: Int64;
  Remaining: Int64;
  TransferChunked: Boolean;
  HasBody: Boolean;
  AutoChunk: Boolean;
  Flags: Cardinal;
  Sent: Cardinal;
  Error: Cardinal;

  // Helpers
  procedure AddForwardHeader(const AName: string);
  var
    V: string;

  begin
    V := HeaderValue(AHeaders,
                     AName);
    if (V <> '') then
      RequestText := RequestText + UTF8String(AName + ': ' + V + #13#10);
  end;

  function SendBodyPart(const AData: RawByteString;
                        AFinal: Boolean): Boolean;
  begin
    Result := SendEntity(FRequestQueue,
                         ARequestId,
                         AData,
                         not AFinal,
                         AutoChunk);
  end;

begin

  Result := False;
  Upstream := socket(AF_INET,
                     SOCK_STREAM,
                     IPPROTO_TCP);

  if (Upstream = INVALID_SOCKET) then
    Exit;

  try
    Timeout := FConfig.SendTimeoutMs;

    setsockopt(Upstream,
               SOL_SOCKET,
               SO_RCVTIMEO,
               PAnsiChar(@Timeout),
               SizeOf(Timeout));

    setsockopt(Upstream,
               SOL_SOCKET,
               SO_SNDTIMEO,
               PAnsiChar(@Timeout),
               SizeOf(Timeout));
               FillChar(Address,
                        SizeOf(Address),
                        0);
    Address.sin_family := AF_INET;
    Address.sin_port := htons(FConfig.Port);
    Address.sin_addr.S_addr := inet_addr('127.0.0.1');

    if (connect(Upstream,
               Address,
               SizeOf(Address)) = SOCKET_ERROR) then
      Exit;

    RequestText := UTF8String(AMethod + ' ' + ATarget +
                              ' HTTP/1.1'#13#10 +
                              'Host: 127.0.0.1:' + IntToStr(FConfig.Port) + #13#10);

    AddForwardHeader('Accept');
    AddForwardHeader('Accept-Encoding');
    AddForwardHeader('Accept-Language');
    AddForwardHeader('Cache-Control');
    AddForwardHeader('Pragma');
    AddForwardHeader('Range');
    AddForwardHeader('If-Match');
    AddForwardHeader('If-Modified-Since');
    AddForwardHeader('If-None-Match');
    AddForwardHeader('If-Range');
    AddForwardHeader('Origin');
    AddForwardHeader('Referer');
    AddForwardHeader('User-Agent');

    RequestText := RequestText + 'Connection: close'#13#10#13#10;
    if not SendSocketAll(Upstream,
                         RequestText[1],
                         Length(RequestText)) then
      Exit;

    ResponseData := '';

    repeat
      Count := recv(Upstream,
                    Buffer,
                    SizeOf(Buffer),
                    0);

      if (Count <= 0) then
        Exit;

      SetLength(ResponseData,
                Length(ResponseData) + Count);
      Move(Buffer[0],
           ResponseData[Length(ResponseData) - Count + 1],
           Count);

      HeaderEnd := Pos(#13#10#13#10, string(ResponseData));
    until (HeaderEnd > 0) or (Length(ResponseData) >= WAN_MAX_UPSTREAM_HEADER);

    if (HeaderEnd = 0) then
      Exit;

    Lines := TStringList.Create();
    Names := TStringList.Create();
    Values := TStringList.Create();

    try
      Lines.Text := StringReplace(string(Copy(ResponseData,
                                              1,
                                              HeaderEnd - 1)),
                                  #13#10,
                                  sLineBreak,
                                  [rfReplaceAll]);

      if (Lines.Count = 0) or not ParseStatusLine(Lines[0],
                                                  Status,
                                                  Reason) then
        Exit;

      ContentLength := -1;
      TransferChunked := False;

      for I := 1 to Lines.Count - 1 do
        begin
          P := Pos(':',
                   Lines[I]);
          if (P <= 0) then
            Continue;

          NameText := Trim(Copy(Lines[I],
                                1,
                                P - 1));

          ValueText := Trim(Copy(Lines[I],
                                 P + 1,
                                 MaxInt));

          if SameText(NameText,
                      'Connection') or SameText(NameText,
                                                'Keep-Alive') then
            Continue;

          if SameText(NameText,
                      'Transfer-Encoding') then
            begin
              TransferChunked := Pos('chunked',
                                     LowerCase(ValueText)) > 0;
              Continue;
            end;

          if SameText(NameText,
                      'Content-Length') then
            TryStrToInt64(ValueText,
                          ContentLength);

          Names.Add(NameText);
          Values.Add(ValueText);
        end;

      FillChar(Response,
               SizeOf(Response),
               0);

      ReasonBytes := UTF8String(Reason);
      Response.Version.MajorVersion := 1;
      Response.Version.MinorVersion := 1;
      Response.StatusCode := Status;
      Response.ReasonLength := Length(ReasonBytes);

      if (ReasonBytes <> '') then
        Response.pReason := @ReasonBytes[1];

      SetLength(HeaderNames,
                Names.Count);

      SetLength(HeaderValues,
                Names.Count);

      UnknownCount := 0;

      for I := 0 to Names.Count - 1 do
      begin
        HeaderNames[I] := UTF8String(Names[I]);
        HeaderValues[I] := UTF8String(Values[I]);
        KnownIndex := ResponseKnownHeaderIndex(Names[I]);

        if (KnownIndex >= 0) then
          begin
            Response.Headers.KnownHeaders[KnownIndex].RawValueLength := Length(HeaderValues[I]);

            if (HeaderValues[I] <> '') then
              Response.Headers.KnownHeaders[KnownIndex].pRawValue := @HeaderValues[I][1];
          end
        else
          Inc(UnknownCount);
      end;

      SetLength(UnknownHeaders,
                UnknownCount);
      UnknownCount := 0;

      for I := 0 to Names.Count - 1 do
        if (ResponseKnownHeaderIndex(Names[I]) < 0) then
          begin
            FillChar(UnknownHeaders[UnknownCount],
                     SizeOf(THttpUnknownHeader),
                     0);

            UnknownHeaders[UnknownCount].NameLength := Length(HeaderNames[I]);
            UnknownHeaders[UnknownCount].RawValueLength := Length(HeaderValues[I]);

            if (HeaderNames[I] <> '') then
              UnknownHeaders[UnknownCount].pName := @HeaderNames[I][1];

            if (HeaderValues[I] <> '') then
              UnknownHeaders[UnknownCount].pRawValue := @HeaderValues[I][1];

            Inc(UnknownCount);
        end;

      Response.Headers.UnknownHeaderCount := UnknownCount;

      if (UnknownCount > 0) then
        Response.Headers.pUnknownHeaders := @UnknownHeaders[0];

      HasBody := not SameText(AMethod,
                              'HEAD') and
                (Status <> 204) and
                (Status <> 304) and
                ((ContentLength <> 0) or TransferChunked);

      AutoChunk := TransferChunked or (ContentLength < 0);
      Flags := HTTP_SEND_RESPONSE_FLAG_DISCONNECT;

      if HasBody then
        begin
          Flags := Flags or HTTP_SEND_RESPONSE_FLAG_MORE_DATA;

          if AutoChunk then
            Flags := Flags or HTTP_SEND_RESPONSE_FLAG_AUTOMATIC_CHUNKING;
        end;

      Sent := 0;

      Error := HttpSendHttpResponse(FRequestQueue,
                                    ARequestId,
                                    Flags,
                                    @Response,
                                    nil,
                                    @Sent,
                                    nil,
                                    0,
                                    nil,
                                    nil);
      if (Error <> NO_ERROR) then
        Exit(True);

      if not HasBody then
        Exit(True);

      BodyData := Copy(ResponseData,
                       HeaderEnd + 4,
                       MaxInt);

      if TransferChunked then
        begin
          // FxServe's current proxy routes normally stream until close rather
          // than chunking. Reject a chunk-framed upstream response instead of
          // forwarding its framing as media bytes.
          FLogger.Error('WAN upstream returned chunked transfer encoding.');
          FinishUnknownLengthResponse(FRequestQueue,
                                      ARequestId);
          Exit(True);
        end;

      if (ContentLength >= 0) then
        begin
          Remaining := ContentLength;

          if (Length(BodyData) > Remaining) then
            SetLength(BodyData,
                      Remaining);

          Dec(Remaining,
              Length(BodyData));

          if (BodyData <> '') then
            if not SendBodyPart(BodyData,
                                Remaining = 0) then
              Exit(True);

          while (Remaining > 0) do
            begin
              Count := recv(Upstream,
                            Buffer,
                            SizeOf(Buffer),
                            0);

              if (Count <= 0) then
                Break;

              if (Count > Remaining) then
                Count := Remaining;

              SetString(Part,
                        PAnsiChar(@Buffer[0]),
                        Count);

              Dec(Remaining,
                  Count);

              if not SendBodyPart(Part,
                                  Remaining = 0) then
                Break;
            end;
        end
      else
        begin
          if (BodyData <> '') then
            if not SendBodyPart(BodyData,
                                False) then
              Exit(True);

            repeat
              Count := recv(Upstream,
                            Buffer,
                            SizeOf(Buffer),
                            0);

              if (Count > 0) then
                begin
                  SetString(Part,
                            PAnsiChar(@Buffer[0]),
                            Count);

                  if not SendBodyPart(Part,
                                      False) then
                    Break;
                end;
            until (Count <= 0);

        FinishUnknownLengthResponse(FRequestQueue,
                                    ARequestId);
      end;

      Result := True;

    finally
      Values.Free;
      Names.Free;
      Lines.Free;
    end;

  finally
    shutdown(Upstream, SD_BOTH);
    closesocket(Upstream);
  end;
end;

end.

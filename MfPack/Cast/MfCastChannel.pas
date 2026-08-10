// FactoryX
//
// Copyright ? FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastChannel.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: TLS, protobuf framing, receiver launch, heartbeat, media loading,
//              playback commands, and status messages.
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
unit MfCastChannel;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.SyncObjs,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type

  TMfCastChannel = class;

  TMfCastChannelWorker = class(TThread)
  private
    FOwner: TMfCastChannel;

  protected
    procedure Execute(); override;

  public
    constructor Create(const AOwner: TMfCastChannel);
    function IsStopping(): Boolean;
  end;

  TMfCastChannel = class(TInterfacedObject, IMfCastChannel)
  private
    FSettings: TMfCastProtocolSettings;
    FTransport: IMfCastTransport;
    FCallbacks: TMfCastChannelCallbacks;
    FLogger: IMfCastLogger;
    FState: TMfCastState;
    FRequestId: Cardinal;
    FSessionId: string;
    FTransportId: string;
    FMediaSessionId: Int64;
    FLastNamespace: string;
    FLastPayload: string;
    FReceivedMediaStatus: Boolean;
    FStopInProgress: Integer;
    FPendingLoadContentId: string;
    FWorker: TMfCastChannelWorker;
    FWorkerLock: TCriticalSection;
    FSendLock: TCriticalSection;
    FLastReportedPlayerState: string;
    FLastMediaCallbackTick: Cardinal;
    FReceiverVolumeControlType: string;
    FLastReceiverVolumeStatus: string;
    FLastMediaVolumeStatus: string;

    function NextRequestId(): Cardinal;
    function SendLaunchReceiverRequest(): HRESULT;
    function SendJson(const ADestinationId: string;
                      const ANamespace: string;
                      const AJsonPayload: string): HRESULT;
    function SendConnect(const ADestinationId: string): HRESULT;
    function ProcessIncomingMessage(const AData: TBytes): HRESULT;
    function ProcessReceiverMessage(const AJsonPayload: string): HRESULT;
    function ProcessMediaMessage(const AJsonPayload: string): HRESULT;
    function ReadExact(ABuffer: Pointer;
                       const ASize: Cardinal): HRESULT;
    function ReadFrame(out AMessage: TBytes): HRESULT;
    function WaitForReceiverReady(const ATimeoutMs: Cardinal): HRESULT;
    function WaitForMediaStatus(const ATimeoutMs: Cardinal): HRESULT;
    procedure StartWorker();
    procedure StopWorker(const ADisconnectTransport: Boolean);
    procedure WorkerExecute(const AWorker: TMfCastChannelWorker);
    procedure SetState(const AState: TMfCastState);

  public

    constructor Create(const ATransport: IMfCastTransport);
    destructor Destroy(); override;

    function Configure(const ASettings: TMfCastProtocolSettings): HRESULT;
    procedure SetCallbacks(const ACallbacks: TMfCastChannelCallbacks);
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Connect(const ADevice: TMfCastDevice): HRESULT;
    function Disconnect: HRESULT;
    function LaunchReceiver: HRESULT;
    function LoadMedia(const ARequest: TMfCastLoadRequest): HRESULT;
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function RequestReceiverStatus(): HRESULT;
    function RequestMediaStatus(): HRESULT;
    function SynchronizeMediaStatus(const ATimeoutMs: Cardinal): HRESULT;
    function GetState(): TMfCastState;
  end;


implementation

uses
  MfCastProtocol;

const
  LFEED = #13;


procedure MfCastAppendByte(var AData: TBytes;
                           const AValue: Byte);
var
  Index: Integer;

begin

  Index := Length(AData);

  SetLength(AData,
            Index + 1);
  AData[Index] := AValue;
end;


procedure MfCastAppendBytes(var AData: TBytes;
                            const ABytes: TBytes);
var
  OldLength: Integer;

begin

  if (Length(ABytes) = 0) then
    Exit;

  OldLength := Length(AData);
  SetLength(AData,
            OldLength + Length(ABytes));

  Move(ABytes[0],
       AData[OldLength],
       Length(ABytes));
end;


procedure MfCastAppendVarUInt(var AData: TBytes;
                              AValue: Cardinal);
begin

  repeat
    if (AValue >= $80) then
      MfCastAppendByte(AData,
                       Byte((AValue and $7F) or $80))
    else
      MfCastAppendByte(AData,
                       Byte(AValue));

    AValue := AValue shr 7;
  until (AValue = 0);
end;


procedure MfCastAppendProtoString(var AData: TBytes;
                                  const AFieldNumber: Cardinal;
                                  const AValue: string);
var
  Bytes: TBytes;

begin

  Bytes := TEncoding.UTF8.GetBytes(AValue);
  MfCastAppendVarUInt(AData,
                      (AFieldNumber shl 3) or 2);

  MfCastAppendVarUInt(AData,
                      Length(Bytes));
  MfCastAppendBytes(AData,
                    Bytes);
end;


procedure MfCastAppendProtoVarUInt(var AData: TBytes;
                                   const AFieldNumber: Cardinal;
                                   const AValue: Cardinal);
begin

  MfCastAppendVarUInt(AData,
                     (AFieldNumber shl 3) or 0);
  MfCastAppendVarUInt(AData,
                      AValue);
end;


function MfCastStreamTypeToString(const AStreamType: TMfCastStreamType): string;
begin

  case AStreamType of
    cstLive: Result := 'LIVE';
    cstNone: Result := 'NONE';
  else
    Result := 'BUFFERED';
  end;
end;


function MfCastJsonEscape(const AValue: string): string;
var
  I: Integer;

begin

  Result := '';
  for I := 1 to Length(AValue) do
    case AValue[I] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      LFEED: Result := Result + '\r';
    else
      if (Ord(AValue[I]) < 32) then
        Result := Result + '\u' + IntToHex(Ord(AValue[I]),
                                           4)
      else
        Result := Result + AValue[I];
    end;
end;


function MfCastBuildMessage(const ASourceId: string;
                            const ADestinationId: string;
                            const ANamespace: string;
                            const AJsonPayload: string): TBytes;
begin

  SetLength(Result,
            0);

  MfCastAppendProtoVarUInt(Result,
                           1,
                           0);

  MfCastAppendProtoString(Result,
                          2,
                          ASourceId);

  MfCastAppendProtoString(Result,
                          3,
                          ADestinationId);

  MfCastAppendProtoString(Result,
                          4,
                          ANamespace);

  MfCastAppendProtoVarUInt(Result,
                           5,
                           0);

  MfCastAppendProtoString(Result,
                          6,
                          AJsonPayload);
end;


function MfCastFrameMessage(const AMessage: TBytes): TBytes;
var
  LengthValue: Cardinal;

begin

  SetLength(Result,
            Length(AMessage) + 4);
  LengthValue := htonl(Cardinal(Length(AMessage)));

  Move(LengthValue,
       Result[0],
       4);

  if (Length(AMessage) > 0) then
    Move(AMessage[0],
         Result[4],
         Length(AMessage));
end;


function MfCastReadVarUInt(const AData: TBytes;
                           var AIndex: Integer;
                           out AValue: Cardinal): Boolean;
var
  Shift: Integer;
  B: Byte;

begin

  Result := False;
  AValue := 0;
  Shift := 0;

  while (AIndex < Length(AData)) do
    begin

      B := AData[AIndex];
      Inc(AIndex);
      AValue := AValue or (Cardinal(B and $7F) shl Shift);

      if (B and $80) = 0 then
        begin
          Result := True;
          Exit;
        end;

      Inc(Shift,
          7);
      if (Shift > 28) then
        Exit;
    end;
end;


function MfCastReadProtoString(const AData: TBytes;
                               var AIndex: Integer;
                               out AValue: string): Boolean;
var
  ValueLength: Cardinal;

begin

  Result := False;
  AValue := '';

  if not MfCastReadVarUInt(AData,
                           AIndex,
                           ValueLength) then
    Exit;

  if (ValueLength > Cardinal(Length(AData) - AIndex)) then
    Exit;

  AValue := TEncoding.UTF8.GetString(AData,
                                     AIndex,
                                     Integer(ValueLength));
  Inc(AIndex,
      Integer(ValueLength));
  Result := True;
end;


function MfCastExtractJsonString(const AJson: string;
                                 const AName: string): string;
var
  Pattern: string;
  I: Integer;
  Escaped: Boolean;

begin

  Result := '';
  Pattern := '"' + AName + '"';
  I := Pos(Pattern,
           AJson);
  if (I = 0) then
    Exit;

  I := PosEx(':',
             AJson,
             I + Length(Pattern));

  if (I = 0) then
    Exit;

  I := PosEx('"',
             AJson,
             I + 1);
  if (I = 0) then
    Exit;
  Inc(I);

  Escaped := False;

  while (I <= Length(AJson)) do
    begin
      if Escaped then
        begin
          case AJson[I] of
            '"': Result := Result + '"';
            '\': Result := Result + '\';
            '/': Result := Result + '/';
            'b': Result := Result + #8;
            't': Result := Result + #9;
            'n': Result := Result + #10;
            'f': Result := Result + #12;
            'r': Result := Result + LFEED;
          else
            Result := Result + AJson[I];
          end;

          Escaped := False;
        end
      else
        if (AJson[I] = '\') then
          Escaped := True
        else
          if (AJson[I] = '"') then
            Exit
          else
            Result := Result + AJson[I];
      Inc(I);
    end;
end;


function MfCastExtractJsonObject(const AJson: string;
                                 const AName: string): string;
var
  Pattern: string;
  I: Integer;
  Start: Integer;
  Depth: Integer;
  InString: Boolean;
  Escaped: Boolean;

begin

  Result := '';
  Pattern := '"' + AName + '"';

  I := Pos(Pattern,
           AJson);
  if (I = 0) then
    Exit;

  I := PosEx(':',
             AJson,
             I + Length(Pattern));

  if (I = 0) then
    Exit;

  Inc(I);

  while (I <= Length(AJson)) and CharInSet(AJson[I], [' ', #9, #10, LFEED]) do
    Inc(I);

  if (I > Length(AJson)) or (AJson[I] <> '{') then
    Exit;

  Start := I;
  Depth := 0;
  InString := False;
  Escaped := False;

  while (I <= Length(AJson)) do
    begin
      if InString then
        begin

          if Escaped then
            Escaped := False
          else
            if (AJson[I] = '\') then
              Escaped := True
            else
              if (AJson[I] = '"') then
                InString := False;
        end
      else
        case AJson[I] of
          '"': InString := True;
          '{': Inc(Depth);
          '}': begin
                 Dec(Depth);
                 if (Depth = 0) then
                   begin
                     Result := Copy(AJson,
                                    Start,
                                    I - Start + 1);
                     Exit;
                   end;
               end;
        end;
      Inc(I);
    end;
end;


function MfCastExtractJsonInt64(const AJson: string;
                                const AName: string;
                                const ADefault: Int64): Int64;
var
  Pattern: string;
  I: Integer;
  Start: Integer;
  TextValue: string;

begin

  Result := ADefault;
  Pattern := '"' + AName + '"';
  I := Pos(Pattern,
           AJson);
  if (I = 0) then
    Exit;

  I := PosEx(':',
             AJson,
             I + Length(Pattern));
  if (I = 0) then
    Exit;

  Inc(I);
  while (I <= Length(AJson)) and CharInSet(AJson[I], [' ', #9, #10, LFEED]) do
    Inc(I);

  Start := I;

  while (I <= Length(AJson)) and CharInSet(AJson[I], ['0'..'9', '-']) do
    Inc(I);

  TextValue := Copy(AJson,
                    Start,
                    I - Start);

  if (TextValue <> '') then
    TryStrToInt64(TextValue,
                  Result);
end;


function MfCastExtractJsonTime100ns(const AJson: string;
                                     const AName: string;
                                     const ADefault: Int64): Int64;
var
  Pattern: string;
  I: Integer;
  Digit: Integer;
  FractionScale: Int64;
  WholeSeconds: Int64;
  Fraction100ns: Int64;
  Negative: Boolean;
  HasDigits: Boolean;

begin

  Result := ADefault;
  Pattern := '"' + AName + '"';
  I := Pos(Pattern,
           AJson);
  if (I = 0) then
    Exit;

  I := PosEx(':',
             AJson,
             I + Length(Pattern));
  if (I = 0) then
    Exit;

  Inc(I);
  while (I <= Length(AJson)) and CharInSet(AJson[I], [' ', #9, #10, LFEED]) do
    Inc(I);

  Negative := False;
  if (I <= Length(AJson)) and (AJson[I] = '-') then
    begin
      Negative := True;
      Inc(I);
    end;

  WholeSeconds := 0;
  HasDigits := False;

  while (I <= Length(AJson)) and CharInSet(AJson[I], ['0'..'9']) do
    begin
      HasDigits := True;
      Digit := Ord(AJson[I]) - Ord('0');
      if (WholeSeconds > (High(Int64) - Digit) div 10) then
        Exit;

      WholeSeconds := WholeSeconds * 10 + Digit;
      Inc(I);
    end;

  if not HasDigits then
    Exit;

  Fraction100ns := 0;
  FractionScale := 1000000;

  if (I <= Length(AJson)) and (AJson[I] = '.') then
    begin
      Inc(I);

      while (I <= Length(AJson)) and CharInSet(AJson[I], ['0'..'9']) do
        begin
          if (FractionScale > 0) then
            begin
              Digit := Ord(AJson[I]) - Ord('0');
              Inc(Fraction100ns,
                  Digit * FractionScale);
              FractionScale := FractionScale div 10;
            end;

          Inc(I);
        end;
    end;

  if (WholeSeconds > (High(Int64) - Fraction100ns) div 10000000) then
    Exit;

  Result := WholeSeconds * 10000000 + Fraction100ns;
  if Negative then
    Result := -Result;
end;


constructor TMfCastChannelWorker.Create(const AOwner: TMfCastChannel);
begin

  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;


procedure TMfCastChannelWorker.Execute();
begin

  if Assigned(FOwner) then
    FOwner.WorkerExecute(Self);
end;


function TMfCastChannelWorker.IsStopping(): Boolean;
begin

  Result := Terminated;
end;


constructor TMfCastChannel.Create(const ATransport: IMfCastTransport);
begin

  inherited Create;

  FTransport := ATransport;
  FState := csIdle;
  FRequestId := 0;
  FMediaSessionId := 0;
  FStopInProgress := 0;
  FWorker := nil;
  FWorkerLock := TCriticalSection.Create();
  FSendLock := TCriticalSection.Create();
  FLastReportedPlayerState := '';
  FLastMediaCallbackTick := 0;
  FReceiverVolumeControlType := '';
  FLastReceiverVolumeStatus := '';
  FLastMediaVolumeStatus := '';
  FCallbacks.Reset;
end;


destructor TMfCastChannel.Destroy();
begin

  StopWorker(True);
  FSendLock.Free();
  FWorkerLock.Free();
  inherited Destroy();
end;


function TMfCastChannel.Configure(const ASettings: TMfCastProtocolSettings): HRESULT;
begin

  if (FState <> csIdle) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  FSettings := ASettings;

  if Assigned(FTransport) then
    Result := FTransport.Configure(ASettings)
  else
    Result := S_OK;
end;


procedure TMfCastChannel.SetCallbacks(const ACallbacks: TMfCastChannelCallbacks);
begin

  FCallbacks := ACallbacks;
end;


procedure TMfCastChannel.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
  if Assigned(FTransport) then
    FTransport.SetLogger(ALogger);
end;


function TMfCastChannel.Connect(const ADevice: TMfCastDevice): HRESULT;
var
  Host: string;
  Port: Word;

begin

  if not Assigned(FTransport) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Host := Trim(ADevice.Address);

  if (Host = '') then
    Host := Trim(ADevice.HostName);

  if (Host = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Port := ADevice.Port;
  if (Port = 0) then
    Port := FSettings.ControlPort;

  SetState(csConnecting);
  Result := FTransport.Connect(Host,
                               Port);
  if FAILED(Result) then
    begin
      SetState(csError);
      Exit;
    end;

  FSessionId := '';
  FTransportId := FSettings.ReceiverId;

  if (FTransportId = '') then
    FTransportId := 'receiver-0';

  Result := SendConnect(FTransportId);
  if FAILED(Result) then
    begin
      Disconnect();
      SetState(csError);
      Exit;
    end;

  SetState(csConnected);
end;


function TMfCastChannel.Disconnect(): HRESULT;
begin

  StopWorker(True);
  InterlockedExchange(FStopInProgress, 0);

  FSessionId := '';
  FTransportId := '';
  FMediaSessionId := 0;
  FLastReportedPlayerState := '';
  FLastMediaCallbackTick := 0;
  FReceiverVolumeControlType := '';
  FLastReceiverVolumeStatus := '';
  FLastMediaVolumeStatus := '';

  if Assigned(FTransport) then
    Result := FTransport.Disconnect
  else
    Result := S_OK;

  SetState(csIdle);
end;


function TMfCastChannel.LaunchReceiver(): HRESULT;
begin

  if (FTransportId = '') then
    FTransportId := FSettings.ReceiverId;

  if (FTransportId = '') then
    FTransportId := 'receiver-0';

  FSessionId := '';
  SetState(csLaunchingReceiver);
  Result := SendLaunchReceiverRequest();
  if FAILED(Result) then
    begin
      SetState(csError);
      Exit;
    end;

  Result := RequestReceiverStatus();
  if FAILED(Result) then
    begin
      SetState(csError);
      Exit;
    end;

  Result := WaitForReceiverReady(FSettings.ReceiverLaunchTimeoutMs);
  if SUCCEEDED(Result) then
    SetState(csConnected)
  else
    SetState(csError);
end;


function TMfCastChannel.LoadMedia(const ARequest: TMfCastLoadRequest): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;
  WaitResult: HRESULT;
  I: Integer;
  Track: TMfCastTrackInfo;
  TrackType: string;

begin

  InterlockedExchange(FStopInProgress, 0);

  if (Trim(ARequest.ContentId) = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  // LOAD waits synchronously for the status carrying its ContentId. The
  // persistent worker must not read Cast frames from the same TLS transport
  // during that transaction.
  StopWorker(False);

  if (FTransportId = '') then
    FTransportId := FSettings.ReceiverId;

  if (FTransportId = '') then
    FTransportId := 'receiver-0';

  RequestId := NextRequestId();
  Payload := '{"type":"LOAD","requestId":' + IntToStr(RequestId) +
             ',"media":{"contentId":"' + MfCastJsonEscape(ARequest.ContentId) +
             '","streamType":"' + MfCastStreamTypeToString(ARequest.StreamType) +
             '","contentType":"' + MfCastJsonEscape(ARequest.ContentType) + '"';

  if (Trim(ARequest.Title) <> '') then
    Payload := Payload + ',"metadata":{"metadataType":0,"title":"' +
               MfCastJsonEscape(ARequest.Title) + '"}';

  if (Length(ARequest.Tracks) > 0) then
    begin
      Payload := Payload + ',"tracks":[';

      for I := Low(ARequest.Tracks) to High(ARequest.Tracks) do
        begin

          if (I > Low(ARequest.Tracks)) then
            Payload := Payload + ',';

          Track := ARequest.Tracks[I];
          TrackType := Trim(Track.TrackType);

          if (TrackType = '') then
            TrackType := 'TEXT';

          Payload := Payload + '{"trackId":' + IntToStr(Track.TrackId) +
                     ',"type":"' + MfCastJsonEscape(TrackType) + '"' +
                     ',"trackContentId":"' + MfCastJsonEscape(Track.ContentId) + '"' +
                     ',"trackContentType":"' + MfCastJsonEscape(Track.ContentType) + '"';

          if (Trim(Track.Name) <> '') then
            Payload := Payload + ',"name":"' + MfCastJsonEscape(Track.Name) + '"';

          if (Trim(Track.Language) <> '') then
            Payload := Payload + ',"language":"' + MfCastJsonEscape(Track.Language) + '"';

          if (Trim(Track.SubType) <> '') then
            Payload := Payload + ',"subtype":"' + MfCastJsonEscape(Track.SubType) + '"';
          Payload := Payload + '}';
        end;

      Payload := Payload + ']';
    end;

  if Length(ARequest.Tracks) > 0 then
    Payload := Payload +
               ',"textTrackStyle":{' +
               '"foregroundColor":"#FFFFFFFF",' +
               '"backgroundColor":"#00000000",' +
               '"edgeType":"OUTLINE",' +
               '"edgeColor":"#000000FF",' +
               '"windowType":"NONE",' +
               '"windowColor":"#00000000"}';

  Payload := Payload + '}';

  if (Length(ARequest.ActiveTrackIds) > 0) then
    begin
      Payload := Payload + ',"activeTrackIds":[';

      for I := Low(ARequest.ActiveTrackIds) to High(ARequest.ActiveTrackIds) do
        begin
          if (I > Low(ARequest.ActiveTrackIds)) then
            Payload := Payload + ',';

          Payload := Payload + IntToStr(ARequest.ActiveTrackIds[I]);
        end;

      Payload := Payload + ']';
    end;

  Payload := Payload + ',"autoplay":';

  if ARequest.AutoPlay then
    Payload := Payload + 'true'
  else
    Payload := Payload + 'false';

  if (ARequest.StartTime100ns > 0) then
    Payload := Payload + ',"currentTime":' + StringReplace(FloatToStr(ARequest.StartTime100ns / 10000000.0),
                                                           ',',
                                                           '.',
                                                           []);
  Payload := Payload + '}';

  OutputDebugString(PChar('MfCast LOAD JSON: ' + Payload));

  // LOAD creates a new media session. Do not attach an ID learned from the
  // receiver's previous media item to the status request that follows.
  FMediaSessionId := 0;
  FReceivedMediaStatus := False;
  FPendingLoadContentId := ARequest.ContentId;
  FLastReportedPlayerState := '';
  FLastMediaCallbackTick := 0;
  FLastMediaVolumeStatus := '';
  Result := SendJson(FTransportId,
                     FSettings.NamespaceMedia,
                     Payload);

  if SUCCEEDED(Result) then
    begin
      SetState(csBuffering);
      WaitResult := WaitForMediaStatus(10000);
      StartWorker();

      if (WaitResult = S_OK) then
        Result := S_OK
      else
        if (WaitResult = S_FALSE) then
          begin
            Result := HRESULT(DWORD($800705B4));
            if Assigned(FLogger) then
              FLogger.Log(cllWarning,
                          'Channel',
                          Format('Media LOAD was not acknowledged for contentId="%s".',
                                 [ARequest.ContentId]));
          end
        else
          Result := WaitResult;
    end
  else
    SetState(csError);
end;


function TMfCastChannel.Play(): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;

begin

  if (FTransportId = '') then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  RequestId := NextRequestId();
  Payload := '{"type":"PLAY","requestId":' + IntToStr(RequestId);

  if (FMediaSessionId <> 0) then
    Payload := Payload + ',"mediaSessionId":' + IntToStr(FMediaSessionId);
  Payload := Payload + '}';

  Result := SendJson(FTransportId,
                     FSettings.NamespaceMedia,
                     Payload);

  if SUCCEEDED(Result) then
    SetState(csPlaying)
  else
    SetState(csError);
end;


function TMfCastChannel.Pause(): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;

begin

  if (FTransportId = '') then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  RequestId := NextRequestId();
  Payload := '{"type":"PAUSE","requestId":' + IntToStr(RequestId);

  if (FMediaSessionId <> 0) then
    Payload := Payload + ',"mediaSessionId":' + IntToStr(FMediaSessionId);
  Payload := Payload + '}';

  Result := SendJson(FTransportId,
                     FSettings.NamespaceMedia,
                     Payload);

  if SUCCEEDED(Result) then
    SetState(csPaused)
  else
    SetState(csError);
end;


function TMfCastChannel.Stop(): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;
  MediaStopResult: HRESULT;
  ReceiverStopResult: HRESULT;

begin

  if InterlockedCompareExchange(FStopInProgress, 1, 0) <> 0 then
    begin
      Result := S_OK;
      Exit;
    end;

  if (FTransportId = '') then
    begin
      Result := S_OK;
      Exit;
    end;

  RequestId := NextRequestId();
  Payload := '{"type":"STOP","requestId":' + IntToStr(RequestId);

  if (FMediaSessionId <> 0) then
    Payload := Payload + ',"mediaSessionId":' + IntToStr(FMediaSessionId);
  Payload := Payload + '}';

  MediaStopResult := SendJson(FTransportId,
                              FSettings.NamespaceMedia,
                              Payload);

  ReceiverStopResult := S_FALSE;
  if (FSessionId <> '') then
    begin
      RequestId := NextRequestId();
      Payload := '{"type":"STOP","requestId":' + IntToStr(RequestId) +
                 ',"sessionId":"' + MfCastJsonEscape(FSessionId) + '"}';
      ReceiverStopResult := SendJson(FSettings.ReceiverId,
                                     FSettings.NamespaceReceiver,
                                     Payload);
    end;

  OutputDebugString(PChar(Format(
    'MfCast STOP media=%.8x receiver=%.8x mediaSession=%d session=%s transport=%s',
    [DWORD(MediaStopResult), DWORD(ReceiverStopResult), FMediaSessionId,
     FSessionId, FTransportId])));

  // Keep TLS alive briefly so the receiver can process both encrypted STOP
  // messages before controller cleanup closes the control connection.
  if SUCCEEDED(MediaStopResult) or SUCCEEDED(ReceiverStopResult) then
    Sleep(1500);

  if SUCCEEDED(MediaStopResult) or SUCCEEDED(ReceiverStopResult) then
    begin
      SetState(csStopped);
      Result := S_OK;
    end
  else
    begin
      SetState(csError);
      Result := MediaStopResult;
    end;
end;


function TMfCastChannel.Seek(const APosition100ns: Int64): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;
  SecondsText: string;

begin

  if (FTransportId = '') then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  RequestId := NextRequestId();
  SecondsText := StringReplace(FloatToStr(APosition100ns / 10000000.0),
                               ',', '.', []);
  Payload := '{"type":"SEEK","requestId":' + IntToStr(RequestId) + ',"currentTime":' + SecondsText;

  if (FMediaSessionId <> 0) then
    Payload := Payload + ',"mediaSessionId":' + IntToStr(FMediaSessionId);
  Payload := Payload + '}';

  Result := SendJson(FTransportId,
                     FSettings.NamespaceMedia,
                     Payload);

  if FAILED(Result) then
    SetState(csError);
end;


function TMfCastChannel.SetVolume(const AVolume: Single): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;
  LevelText: string;

begin

  if (AVolume < 0.0) or (AVolume > 1.0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  RequestId := NextRequestId();
  LevelText := StringReplace(FloatToStr(AVolume),
                             ',',
                             '.',
                             []);

  if SameText(FReceiverVolumeControlType,
              'fixed') then
    begin
      if (FMediaSessionId = 0) or (FTransportId = '') then
        begin
          Result := E_UNEXPECTED;
          Exit;
        end;

      Payload := '{"type":"VOLUME","requestId":' + IntToStr(RequestId) +
                 ',"mediaSessionId":' + IntToStr(FMediaSessionId) +
                 ',"volume":{"level":' + LevelText + '}}';

      if Assigned(FLogger) then
        FLogger.Log(cllDebug,
                    'Channel',
                    'Fixed device volume detected; routing Volume to the active media stream.');

      Result := SendJson(FTransportId,
                         FSettings.NamespaceMedia,
                         Payload);
    end
  else
    begin
      Payload := '{"type":"SET_VOLUME","requestId":' + IntToStr(RequestId) +
                 ',"volume":{"level":' + LevelText + '}}';

      Result := SendJson(FSettings.ReceiverId,
                         FSettings.NamespaceReceiver,
                         Payload);
    end;

  if FAILED(Result) then
    SetState(csError);
end;


function TMfCastChannel.SetMuted(const AMuted: Boolean): HRESULT;
var
  Payload: string;
  RequestId: Cardinal;

begin

  if SameText(FReceiverVolumeControlType,
              'fixed') then
    begin
      if (FMediaSessionId = 0) or (FTransportId = '') then
        begin
          Result := E_UNEXPECTED;
          Exit;
        end;

      RequestId := NextRequestId();
      Payload := '{"type":"VOLUME","requestId":' + IntToStr(RequestId) +
                 ',"mediaSessionId":' + IntToStr(FMediaSessionId) +
                 ',"volume":{"muted":';

      if AMuted then
        Payload := Payload + 'true'
      else
        Payload := Payload + 'false';

      Payload := Payload + '}}';

      if Assigned(FLogger) then
        FLogger.Log(cllDebug,
                    'Channel',
                    'Fixed device volume detected; routing Mute to the active media stream.');

      Result := SendJson(FTransportId,
                         FSettings.NamespaceMedia,
                         Payload);

      if FAILED(Result) then
        SetState(csError);

      Exit;
    end;

  RequestId := NextRequestId();
  Payload := '{"type":"SET_VOLUME","requestId":' + IntToStr(RequestId) + ',"volume":{"muted":';

  if AMuted then
    Payload := Payload + 'true'
  else
    Payload := Payload + 'false';

  Payload := Payload + '}}';

  Result := SendJson(FSettings.ReceiverId,
                     FSettings.NamespaceReceiver,
                     Payload);
  if FAILED(Result) then
    SetState(csError);
end;


function TMfCastChannel.RequestReceiverStatus(): HRESULT;
var
  Payload: string;

begin

  Payload := '{"type":"GET_STATUS","requestId":' + IntToStr(NextRequestId()) + '}';

  Result := SendJson(FSettings.ReceiverId,
                     FSettings.NamespaceReceiver,
                     Payload);
end;


function TMfCastChannel.RequestMediaStatus(): HRESULT;
var
  Payload: string;

begin

  if (FTransportId = '') then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  Payload := '{"type":"GET_STATUS","requestId":' + IntToStr(NextRequestId());

  if (FMediaSessionId <> 0) then
    Payload := Payload + ',"mediaSessionId":' + IntToStr(FMediaSessionId);

  Payload := Payload + '}';

  Result := SendJson(FTransportId,
                     FSettings.NamespaceMedia,
                     Payload);
end;


function TMfCastChannel.SendLaunchReceiverRequest(): HRESULT;
var
  Payload: string;

begin

  Payload := '{"type":"LAUNCH","appId":"' +
             MfCastJsonEscape(FSettings.ReceiverApplicationId) +
             '","requestId":' + IntToStr(NextRequestId()) + '}';

  Result := SendJson(FSettings.ReceiverId,
                     FSettings.NamespaceReceiver,
                     Payload);
end;


function TMfCastChannel.SynchronizeMediaStatus(
  const ATimeoutMs: Cardinal): HRESULT;
begin

  FReceivedMediaStatus := False;
  Result := RequestMediaStatus();

  if SUCCEEDED(Result) then
    begin
      Result := WaitForMediaStatus(ATimeoutMs);
      StartWorker();
    end;
end;


procedure TMfCastChannel.StartWorker();
begin

  FWorkerLock.Acquire();
  try
    if Assigned(FWorker) or
       not Assigned(FTransport) or
       not FTransport.IsConnected() then
      Exit;

    FWorker := TMfCastChannelWorker.Create(Self);

    if Assigned(FLogger) then
      FLogger.Log(cllDebug,
                  'Channel',
                  'Persistent receive and heartbeat worker started.');

    FWorker.Start();
  finally
    FWorkerLock.Release();
  end;
end;


procedure TMfCastChannel.StopWorker(const ADisconnectTransport: Boolean);
var
  CalledFromWorker: Boolean;
  Worker: TMfCastChannelWorker;

begin

  FWorkerLock.Acquire();
  try
    Worker := FWorker;
    CalledFromWorker := Assigned(Worker) and
                        (GetCurrentThreadId() = Worker.ThreadID);

    if not CalledFromWorker then
      FWorker := nil;
  finally
    FWorkerLock.Release();
  end;

  if not Assigned(Worker) then
    begin
      if ADisconnectTransport and Assigned(FTransport) then
        FTransport.Disconnect();

      Exit;
    end;

  Worker.Terminate();

  if ADisconnectTransport and Assigned(FTransport) then
    begin
      FSendLock.Acquire();
      try
        FTransport.Disconnect();
      finally
        FSendLock.Release();
      end;
    end;

  if CalledFromWorker then
    begin
      // Media-status and error callbacks run on this worker. Cleanup initiated
      // by such a callback must not wait for, or free, the current thread. Keep
      // the object attached so the next foreground cleanup can join it safely.

      if Assigned(FLogger) then
        FLogger.Log(cllDebug,
                    'Channel',
                    'Persistent receive worker will be joined by foreground cleanup.');

      Exit;
    end;

  Worker.WaitFor();
  Worker.Free();

  if Assigned(FLogger) then
    FLogger.Log(cllDebug,
                'Channel',
                'Persistent receive and heartbeat worker stopped.');
end;


procedure TMfCastChannel.WorkerExecute(const AWorker: TMfCastChannelWorker);
var
  MessageData: TBytes;
  ReadResult: HRESULT;
  HeartbeatResult: HRESULT;
  HeartbeatIntervalMs: Cardinal;
  LastHeartbeatTick: Cardinal;

begin

  HeartbeatIntervalMs := FSettings.HeartbeatIntervalMs;
  if (HeartbeatIntervalMs = 0) then
    HeartbeatIntervalMs := 5000;

  LastHeartbeatTick := GetTickCount();

  while not AWorker.IsStopping() do
    begin
      ReadResult := ReadFrame(MessageData);

      if AWorker.IsStopping() then
        Break;

      if (ReadResult = S_OK) then
        begin
          ReadResult := ProcessIncomingMessage(MessageData);

          if FAILED(ReadResult) then
            begin
              if Assigned(FLogger) then
                FLogger.Log(cllWarning,
                            'Channel',
                            Format('Control message processing failed: HRESULT $%.8x.',
                                   [DWORD(ReadResult)]));

              Break;
            end;
        end
      else
        if (ReadResult <> S_FALSE) then
          begin
            if Assigned(FLogger) then
              FLogger.Log(cllWarning,
                          'Channel',
                          Format('Persistent control receive stopped: HRESULT $%.8x.',
                                 [DWORD(ReadResult)]));

            Break;
          end;

      if AWorker.IsStopping() then
        Break;

      if (GetTickCount() - LastHeartbeatTick) >= HeartbeatIntervalMs then
        begin
          HeartbeatResult := SendJson(FSettings.ReceiverId,
                                      FSettings.NamespaceHeartbeat,
                                      '{"type":"PING"}');
          LastHeartbeatTick := GetTickCount();

          if FAILED(HeartbeatResult) then
            begin
              if Assigned(FLogger) then
                FLogger.Log(cllWarning,
                            'Channel',
                            Format('Heartbeat PING failed: HRESULT $%.8x.',
                                   [DWORD(HeartbeatResult)]));

              Break;
            end;
        end;
    end;
end;


function TMfCastChannel.GetState(): TMfCastState;
begin

  Result := FState;
end;


function TMfCastChannel.NextRequestId(): Cardinal;
begin

  Inc(FRequestId);
  if (FRequestId = 0) then
    Inc(FRequestId);
  Result := FRequestId;
end;


function TMfCastChannel.SendJson(const ADestinationId: string;
                                 const ANamespace: string;
                                 const AJsonPayload: string): HRESULT;
var
  SourceId: string;
  Frame: TBytes;

begin

  if not Assigned(FTransport) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  FSendLock.Acquire();
  try
    SourceId := FSettings.SenderId;
    if (SourceId = '') then
      SourceId := 'sender-0';

    Frame := MfCastProtocolEncodeFrame(SourceId,
                                       ADestinationId,
                                       ANamespace,
                                       AJsonPayload);

    if (Length(Frame) = 0) then
      Result := E_FAIL
    else
      Result := FTransport.SendBuffer(@Frame[0], Length(Frame));
  finally
    FSendLock.Release();
  end;
end;


function TMfCastChannel.SendConnect(const ADestinationId: string): HRESULT;
begin

  Result := SendJson(ADestinationId,
                     FSettings.NamespaceConnection,
                     '{"type":"CONNECT"}');
end;


function TMfCastChannel.ProcessIncomingMessage(const AData: TBytes): HRESULT;
var
  Namespace: string;
  Payload: string;

begin

  FLastNamespace := '';
  FLastPayload := '';
  Result := MfCastProtocolDecodeMessage(AData, Namespace, Payload);
  if FAILED(Result) then
    Exit;

  FLastNamespace := Namespace;
  FLastPayload := Payload;

  if SameText(Namespace, FSettings.NamespaceHeartbeat) then
    begin
      if (Pos('PING',
             UpperCase(Payload)) > 0) then
        Result := SendJson(FSettings.ReceiverId,
                           FSettings.NamespaceHeartbeat,
                           '{"type":"PONG"}');
    end
  else
    if SameText(Namespace, FSettings.NamespaceReceiver) then
      Result := ProcessReceiverMessage(Payload)
    else
      if SameText(Namespace, FSettings.NamespaceMedia) then
        Result := ProcessMediaMessage(Payload);
end;


function TMfCastChannel.ProcessReceiverMessage(const AJsonPayload: string): HRESULT;
var
  NewSessionId: string;
  NewTransportId: string;
  VolumeStatus: string;

begin

  Result := S_OK;
  if (Pos('RECEIVER_STATUS',
          AJsonPayload) > 0) then
    begin
      VolumeStatus := MfCastExtractJsonObject(AJsonPayload,
                                              'volume');

      if (VolumeStatus <> '') then
        begin
          FReceiverVolumeControlType := MfCastExtractJsonString(
            VolumeStatus,
            'controlType');

          if (VolumeStatus <> FLastReceiverVolumeStatus) and
             Assigned(FLogger) then
            begin
              FLastReceiverVolumeStatus := VolumeStatus;
              FLogger.Log(cllDebug,
                          'Channel',
                          'Receiver volume status: ' + VolumeStatus + '.');

              if SameText(FReceiverVolumeControlType,
                          'fixed') then
                FLogger.Log(cllWarning,
                            'Channel',
                            'Chromecast reports fixed device volume; Volume and Mute will target the active media stream.');
            end;
        end;

      NewTransportId := MfCastExtractJsonString(AJsonPayload,
                                                'transportId');
      if (NewTransportId = '') then
        Exit;

      NewSessionId := MfCastExtractJsonString(AJsonPayload, 'sessionId');

      if (NewSessionId = '') then
        NewSessionId := FSettings.ReceiverApplicationId;

      if (FSessionId <> '') and SameText(FTransportId, NewTransportId) then
        Exit;

      FSessionId := NewSessionId;
      FTransportId := NewTransportId;

      Result := SendConnect(FTransportId);
      if FAILED(Result) then
        Exit;

      if Assigned(FCallbacks.OnReceiverReady) then
        FCallbacks.OnReceiverReady(FSessionId,
                                   FTransportId);
    end
  else
    if (Pos('CLOSE',
           AJsonPayload) > 0) then
      begin
        FSessionId := '';
        FTransportId := FSettings.ReceiverId;
        FMediaSessionId := 0;
        if Assigned(FCallbacks.OnReceiverClosed) then
          FCallbacks.OnReceiverClosed();
      end;
end;


function TMfCastChannel.ProcessMediaMessage(const AJsonPayload: string): HRESULT;
var
  Status: TMfCastMediaStatus;
  Error: TMfCastErrorInfo;
  ExtendedStatus: string;
  ExtendedPlayerState: string;
  MediaVolumeStatus: string;
  CurrentTick: Cardinal;
  NotifyCallback: Boolean;

begin

  Result := S_OK;
  if (Pos('LOAD_FAILED',
          AJsonPayload) > 0) or
     (Pos('LOAD_CANCELLED',
          AJsonPayload) > 0) or
     (Pos('INVALID_REQUEST',
          AJsonPayload) > 0) then
    begin
      Error.Reset();
      Error.HResult := E_FAIL;
      Error.Stage := 'Media status';
      Error.MessageText := MfCastExtractJsonString(AJsonPayload,
                                                   'type');
      Error.Detail := AJsonPayload;
      SetState(csError);

      if Assigned(FCallbacks.OnError) then
        FCallbacks.OnError(Error);
      Exit;
    end;

  if Pos('MEDIA_STATUS', AJsonPayload) = 0 then
    Exit;

  if (Pos('"status":[]',
          StringReplace(AJsonPayload,
                        ' ',
                        '',
                        [rfReplaceAll])) > 0) then
    Exit;

  if (FPendingLoadContentId <> '') and
     (Pos(FPendingLoadContentId, AJsonPayload) = 0) then
    begin
      OutputDebugString(PChar('MfCast MEDIA_STATUS ignored stale status while waiting for ' +
                              FPendingLoadContentId));
      Exit;
    end;

  if (FPendingLoadContentId <> '') then
    FPendingLoadContentId := '';

  FReceivedMediaStatus := True;
  Status.Reset();

  Status.MediaSessionId := MfCastExtractJsonInt64(AJsonPayload,
                                                 'mediaSessionId',
                                                 0);

  Status.PlayerState := MfCastExtractJsonString(AJsonPayload,
                                                'playerState');

  Status.IdleReason := MfCastExtractJsonString(AJsonPayload,
                                               'idleReason');

  MediaVolumeStatus := MfCastExtractJsonObject(AJsonPayload,
                                               'volume');

  if (MediaVolumeStatus <> '') and
     (MediaVolumeStatus <> FLastMediaVolumeStatus) and
     Assigned(FLogger) then
    begin
      FLastMediaVolumeStatus := MediaVolumeStatus;
      FLogger.Log(cllDebug,
                  'Channel',
                  'Media stream volume status: ' + MediaVolumeStatus + '.');
    end;

  ExtendedStatus := MfCastExtractJsonObject(AJsonPayload,
                                            'extendedStatus');

  ExtendedPlayerState := MfCastExtractJsonString(ExtendedStatus,
                                                 'playerState');

  if SameText(Status.PlayerState,
              'IDLE') and
     ((SameText(ExtendedPlayerState,
                'LOADING')) or
      (SameText(ExtendedPlayerState,
                'BUFFERING'))) then
    begin
      if SameText(ExtendedPlayerState,
                  'LOADING') then
        Status.PlayerState := 'BUFFERING'
      else
        Status.PlayerState := ExtendedPlayerState;

      Status.IdleReason := '';

      OutputDebugString(PChar('MfCast MEDIA_STATUS extended playerState=' +
                              ExtendedPlayerState + ' mapped=' +
                              Status.PlayerState));
    end;
  Status.CurrentTime100ns := MfCastExtractJsonTime100ns(AJsonPayload,
                                                       'currentTime',
                                                       0);
  Status.Duration100ns := MfCastExtractJsonTime100ns(AJsonPayload,
                                                    'duration',
                                                    0);

  if Status.MediaSessionId <> 0 then
    FMediaSessionId := Status.MediaSessionId;
  if SameText(Status.PlayerState, 'PLAYING') then
    SetState(csPlaying)
  else
    if SameText(Status.PlayerState, 'PAUSED') then
      SetState(csPaused)
    else
      if SameText(Status.PlayerState, 'BUFFERING') or
         SameText(Status.PlayerState, 'LOADING') then
        SetState(csBuffering)
      else
        if SameText(Status.PlayerState, 'IDLE') then
          SetState(csStopped);

  CurrentTick := GetTickCount();
  NotifyCallback := not SameText(Status.PlayerState,
                                 FLastReportedPlayerState) or
                    (FLastMediaCallbackTick = 0) or
                    ((CurrentTick - FLastMediaCallbackTick) >= 500);

  if NotifyCallback then
    begin
      FLastReportedPlayerState := Status.PlayerState;
      FLastMediaCallbackTick := CurrentTick;

      if (InterlockedCompareExchange(FStopInProgress, 0, 0) = 0) and
         Assigned(FCallbacks.OnMediaStatus) and
         ((Status.MediaSessionId <> 0) or (Status.PlayerState <> '')) then
        FCallbacks.OnMediaStatus(Status);
    end;
end;


function TMfCastChannel.ReadExact(ABuffer: Pointer;
                                  const ASize: Cardinal): HRESULT;
var
  TotalRead: Cardinal;
  BytesRead: Cardinal;
  Ptr: PAnsiChar;

begin

  if (ABuffer = nil) and (ASize > 0) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  TotalRead := 0;
  Ptr := PAnsiChar(ABuffer);

  while TotalRead < ASize do
    begin
      Result := FTransport.ReceiveBuffer(@Ptr[TotalRead],
                                         ASize - TotalRead,
                                         BytesRead);
      if FAILED(Result) then
        begin
          if (TotalRead = 0) and
             (Result = HRESULT(DWORD($8007274C))) then
            Result := S_FALSE;
          Exit;
        end;

      if (BytesRead = 0) then
        begin
          if (TotalRead = 0) then
            Result := S_FALSE
          else
            Result := E_FAIL;
          Exit;
        end;

      Inc(TotalRead,
          BytesRead);
    end;

  Result := S_OK;
end;


function TMfCastChannel.ReadFrame(out AMessage: TBytes): HRESULT;
var
  Header: array[0..3] of Byte;
  MessageLength: Cardinal;

begin

  SetLength(AMessage,
            0);
  Result := ReadExact(@Header[0],
                      SizeOf(Header));
  if (Result <> S_OK) then
    Exit;

  MessageLength := (Cardinal(Header[0]) shl 24) or
                   (Cardinal(Header[1]) shl 16) or
                   (Cardinal(Header[2]) shl 8) or
                   Cardinal(Header[3]);

  if (MessageLength > Cardinal(16 * 1024 * 1024)) then
    begin
      Result := E_FAIL;
      Exit;
    end;

  SetLength(AMessage,
            Integer(MessageLength));


  if (MessageLength > 0) then
    begin
      Result := ReadExact(@AMessage[0], MessageLength);
      if (Result = S_FALSE) then
        Result := E_FAIL;
    end
  else
    Result := S_OK;
end;


function TMfCastChannel.WaitForReceiverReady(const ATimeoutMs: Cardinal): HRESULT;
var
  StartTick: DWORD;
  LastStatusTick: DWORD;
  LastLaunchTick: DWORD;
  TimeoutMs: Cardinal;
  msgMessage: TBytes;

begin

  TimeoutMs := ATimeoutMs;
  if (TimeoutMs = 0) then
    TimeoutMs := 10000;

  StartTick := GetTickCount();
  LastStatusTick := StartTick;
  LastLaunchTick := StartTick;

  repeat
    Result := ReadFrame(msgMessage);
    if (Result = S_FALSE) then
      begin
        if (GetTickCount() - LastStatusTick) >= 3000 then
          begin
            RequestReceiverStatus();
            LastStatusTick := GetTickCount();
          end;

        if (GetTickCount() - LastLaunchTick) >= 6000 then
          begin
            if Assigned(FLogger) then
              FLogger.Log(cllDebug,
                          'Channel',
                          'Receiver application is still absent; retransmitting LAUNCH.');

            Result := SendLaunchReceiverRequest();
            if FAILED(Result) then
              Exit;

            LastLaunchTick := GetTickCount();
          end;

        Continue;
      end;
    if FAILED(Result) then
      Exit;

    Result := ProcessIncomingMessage(msgMessage);
    if FAILED(Result) then
      Exit;

    if (FSessionId <> '') and
       (FTransportId <> '') and
       not SameText(FTransportId, FSettings.ReceiverId) then
      begin
        Result := S_OK;
        Exit;
      end;
  until (GetTickCount() - StartTick) >= TimeoutMs;

  if Assigned(FLogger) then
    FLogger.Log(cllWarning,
                'Channel',
                Format('Receiver-ready wait timed out: session="%s" transport="%s" lastNamespace="%s" lastPayload="%s".',
                       [FSessionId,
                        FTransportId,
                        FLastNamespace,
                        Copy(FLastPayload, 1, 512)]));

  Result := HRESULT(DWORD($800705B4));
end;


function TMfCastChannel.WaitForMediaStatus(const ATimeoutMs: Cardinal): HRESULT;
var
  StartTick: DWORD;
  LastStatusTick: DWORD;
  TimeoutMs: Cardinal;
  msgMessage: TBytes;

begin

  TimeoutMs := ATimeoutMs;
  if (TimeoutMs = 0) then
    TimeoutMs := 10000;

  StartTick := GetTickCount();
  LastStatusTick := StartTick;

  repeat
    Result := ReadFrame(msgMessage);
    if (Result = S_FALSE) then
      begin
        if (GetTickCount() - LastStatusTick) >= 3000 then
          begin
            RequestMediaStatus();
            LastStatusTick := GetTickCount();
          end;
        Continue;
      end;
    if FAILED(Result) then
      Exit;

    Result := ProcessIncomingMessage(msgMessage);
    if FAILED(Result) then
      Exit;

    if (FState = csError) then
      begin
        Result := E_FAIL;
        Exit;
      end;

    if FReceivedMediaStatus and
       (FState in [csPlaying, csPaused, csStopped]) then
      begin
        Result := S_OK;
        Exit;
      end;
  until (GetTickCount() - StartTick) >= TimeoutMs;

  Result := S_FALSE;
end;


procedure TMfCastChannel.SetState(const AState: TMfCastState);
begin

  FState := AState;
end;

end.

// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastAndroidTvRemote.pas
// Kind: Pascal Unit
// Release date: 11-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Native paired Android TV Remote v2 standby control,
//              with per-device DPAPI-protected credentials.
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
// Source: Google Cast V2 wire protocol
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
unit MfCastAndroidTvRemote;

// Paired Android TV Remote v2 power control. Pairing is opt-in per stable Cast
// identity. Credentials are protected with Windows DPAPI for the current user.
// This is separate from the Cast protocol.

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type

  TMfCastAndroidTvSession = class
  private
    FTransport: IMfCastTransport;
    FInput: TBytes;
    FFeatures: Cardinal;
    FSettleMs, FTimeoutMs: Cardinal;

    function Send(const AMessage: TBytes): HRESULT;
    function ReadMessage(out AMessage: TBytes;
                         const ADeadline: DWORD): HRESULT;

    function ProcessMessage(const AMessage: TBytes;
                            out AHasPower: Boolean;
                            out AIsOn: Boolean): HRESULT;

  public

    constructor Create(const ATransport: IMfCastTransport;
                       const ASettleMs: Cardinal = 5000;
                       const ATimeoutMs: Cardinal = 8000);

    function ReadPower(out AIsOn: Boolean): HRESULT;
    function Standby(out ADetail: string): HRESULT;
  end;

  TRemoteBlob = record
    Size: DWORD;
    Data: Pointer;
  end;


  function MfCastAndroidTvPairingFile(const ADevice: TMfCastDevice): string;

  function MfCastImportAndroidTvPairing(const ADevice: TMfCastDevice;
                                        const APfx: TBytes): HRESULT;

  function MfCastAndroidTvPower(const ADevice: TMfCastDevice;
                                const AStandby: Boolean;
                                out AIsOn: Boolean;
                                out ADetail: string): HRESULT;

  function CryptProtectData(var Input: TRemoteBlob;
                            Description: PWideChar;
                            Entropy: Pointer;
                            Reserved: Pointer;
                            Prompt: Pointer;
                            Flags: DWORD;
                            out Output: TRemoteBlob): BOOL; stdcall;

  function CryptUnprotectData(var Input: TRemoteBlob;
                              Description: Pointer;
                              Entropy,
                              Reserved,
                              Prompt: Pointer;
                              Flags: DWORD;
                              out Output: TRemoteBlob): BOOL; stdcall;

  function PFXImportCertStore(var Data: TRemoteBlob;
                              Password: PWideChar;
                              Flags: DWORD): Pointer; stdcall;

  function CertEnumCertificatesInStore(Store: Pointer;
                                       Previous: Pointer): Pointer; stdcall;

  function CertFreeCertificateContext(Cert: Pointer): BOOL; stdcall;
  function CertCloseStore(Store: Pointer;
                          Flags: DWORD): BOOL; stdcall;


implementation

uses

  {System}
  System.Classes,
  {Cast}
  MfCastTransport;

const
  MaxMessageSize = 65536;
  TimeoutResult = HRESULT($800705B4);
  Crypt32Lib = 'crypt32.dll';


function LastErrorResult(): HRESULT;
var
  Error: DWORD;

begin

  Error := GetLastError;

  if (Error = S_OK) then
    Error := ERROR_GEN_FAILURE;

  Result := HRESULT($80070000 or (Error and $FFFF));
end;


procedure Append(var AData: TBytes;
                 const ABytes: TBytes);
var
  Size: Integer;

begin

  Size := Length(AData);
  SetLength(AData, Size + Length(ABytes));

  if (Length(ABytes) > 0) then
    Move(ABytes[0], AData[Size], Length(ABytes));
end;

function Varint(AValue: Cardinal): TBytes;
var
  Count: Integer;

begin

  SetLength(Result,
            5);
  Count := 0;

  repeat
    Result[Count] := Byte(AValue and $7F);
    AValue := AValue shr 7;

    if (AValue <> 0) then
      Result[Count] := Result[Count] or $80;

    Inc(Count);

  until (AValue = 0);

  SetLength(Result, Count);
end;


function Scalar(AField: Cardinal;
                AValue: Cardinal): TBytes;
begin

  Result := Varint(AField shl 3);
  Append(Result,
         Varint(AValue));
end;


function MessageField(AField: Cardinal;
                      const AValue: TBytes): TBytes;
begin

  Result := Varint((AField shl 3) or 2);

  Append(Result,
         Varint(Length(AValue)));

  Append(Result,
         AValue);
end;


function ReadVarint(const AData: TBytes;
                    var AOffset: Integer;
                    out AValue: Cardinal): Boolean;
var
  Shift: Integer;
  B: Byte;

begin

  AValue := 0;

  for Shift := 0 to 4 do
    begin
      if (AOffset >= Length(AData))then
        Exit(False);

      B := AData[AOffset];
      Inc(AOffset);

      if (Shift = 4) and (B > $0F) then
        Exit(False);

      AValue := AValue or (Cardinal(B and $7F) shl (Shift * 7));
      if (B and $80) = 0 then
        Exit(True);
    end;

  Result := False;
end;


function Field(const AData: TBytes;
               AField: Cardinal;
               out AValue: Cardinal;
               out AMessage: TBytes): Boolean;
var
  Offset: Integer;
  Tag: Cardinal;
  Value: Cardinal;
  Count: Cardinal;

begin

  Offset := 0;
  AValue := 0;
  AMessage := nil;

  while (Offset < Length(AData)) do
    begin
      if not ReadVarint(AData,
                        Offset,
                        Tag) or (Tag = 0) then
        Exit(False);

      case Tag and 7 of
        0: begin
             if not ReadVarint(AData,
                               Offset,
                               Value) then
               Exit(False);

             if (Tag shr 3 = AField) then
                begin
                  AValue := Value;
                  Exit(True);
                end;
           end;

      1: Inc(Offset,
             8);

      2: begin
           if not ReadVarint(AData,
                             Offset,
                             Count) or (Count > Cardinal(Length(AData) - Offset)) then
             Exit(False);

            if (Tag shr 3 = AField) then
              begin
                AMessage := Copy(AData,
                                 Offset,
                                 Count);
                Exit(True);
              end;

            Inc(Offset,
                Count);
          end;

      5: Inc(Offset,
             4);
      else
        Exit(False);
      end;
    end;

  Result := False;
end;


constructor TMfCastAndroidTvSession.Create(const ATransport: IMfCastTransport;
                                           const ASettleMs: Cardinal;
                                           const ATimeoutMs: Cardinal);
begin

  inherited Create;

  FTransport := ATransport;
  FFeatures := 1 or 2 or 32; // Ping, key injection, power reporting.
  FSettleMs := ASettleMs;
  FTimeoutMs := ATimeoutMs;
end;


function TMfCastAndroidTvSession.Send(const AMessage: TBytes): HRESULT;
var
  Frame: TBytes;

begin

  Frame := Varint(Length(AMessage));

  Append(Frame,
         AMessage);

  Result := FTransport.SendBuffer(@Frame[0],
                                  Length(Frame));
end;


function TMfCastAndroidTvSession.ReadMessage(out AMessage: TBytes;
                                             const ADeadline: DWORD): HRESULT;
var
  Offset: Integer;
  Count, Received: Cardinal;
  Buffer: array[0..4095] of Byte;
  Extra: TBytes;

begin

  AMessage := nil;

  while (Integer(ADeadline - GetTickCount) > 0) do
    begin
      Offset := 0;

      if ReadVarint(FInput,
                    Offset,
                    Count) then
        begin
          if (Count > MaxMessageSize) then
            Exit(E_INVALIDARG);

          if (Count <= Cardinal(Length(FInput) - Offset)) then
            begin
              AMessage := Copy(FInput,
                               Offset,
                               Count);

              FInput := Copy(FInput,
                             Offset + Integer(Count),
                             Length(FInput));
              Exit(S_OK);
            end;
        end
      else
        if (Length(FInput) >= 5) then
          Exit(E_INVALIDARG);

    Result := FTransport.ReceiveBuffer(@Buffer[0],
                                       SizeOf(Buffer),
                                       Received);

    if Result = HRESULT($8007274C) then
      Continue; // Poll timeout, not disconnection.

    if Failed(Result) then
      Exit;

    if (Received > 0) then
      begin
        SetLength(Extra,
                  Received);

        Move(Buffer[0],
             Extra[0],
             Received);

        Append(FInput,
          Extra);

        if (Length(FInput) > MaxMessageSize + 5) then
          Exit(E_INVALIDARG);
      end;
  end;

  Result := TimeoutResult;
end;


function TMfCastAndroidTvSession.ProcessMessage(const AMessage: TBytes;
                                                out AHasPower: Boolean;
                                                out AIsOn: Boolean): HRESULT;
var
  Value: Cardinal;
  Body: TBytes;
  Ignored: TBytes;
  Reply: TBytes;
  Info: TBytes;

begin

  AHasPower := False;
  AIsOn := False;
  Result := S_OK;

  if Field(AMessage,
           1,
           Value,
           Body) then // Configure.
    begin
      if not Field(Body,
                   1,
                   Value,
                   Ignored) then
        Exit(E_INVALIDARG);

      FFeatures := FFeatures and Value;

      if (FFeatures and (2 or 32)) <> (2 or 32) then
        Exit(E_NOTIMPL);

      Info := Scalar(3,
                     1);

      Append(Info,
             MessageField(4,
                          TEncoding.UTF8.GetBytes('1')));

      Append(Info,
             MessageField(5,
                          TEncoding.UTF8.GetBytes('MfCastPlayer')));

      Append(Info,
             MessageField(6,
                          TEncoding.UTF8.GetBytes('1.0')));

      Reply := Scalar(1,
                      FFeatures);

      Append(Reply,
             MessageField(2,
                          Info));

      Result := Send(MessageField(1,
                                Reply));
    end
  else
    if Field(AMessage,
             2,
             Value,
             Body) then // Set active.
      Result := Send(MessageField(2,
                                  Scalar(1,
                                         FFeatures)))
    else
      if Field(AMessage,
               8,
               Value,
               Body) then // Ping.
        begin
          Field(Body,
                1,
                Value,
                Ignored);

          Result := Send(MessageField(9,
                                      Scalar(1,
                                             Value)));
       end
     else
       if Field(AMessage,
                40,
                Value,
                Body) then // Authoritative power report.
         begin

           // Proto3 omits started=false, so an empty remote_start means standby.
           Field(Body,
                 1,
                 Value,
                 Ignored);

           AHasPower := True;
           AIsOn := Value <> 0;
         end
       else
         if Field(AMessage,
                  3,
                  Value,
                  Body) then
           Result := E_FAIL;
end;


function TMfCastAndroidTvSession.ReadPower(out AIsOn: Boolean): HRESULT;
var
  Deadline: DWORD;
  Message: TBytes;
  HasPower: Boolean;

begin

  AIsOn := False;
  Deadline := GetTickCount + FTimeoutMs;

  repeat
    Result := ReadMessage(Message,
                          Deadline);

    if Failed(Result) then
      Exit;

    Result := ProcessMessage(Message,
                             HasPower,
                             AIsOn);

    if Failed(Result) or HasPower then
      Exit;

  until False;
end;


function TMfCastAndroidTvSession.Standby(out ADetail: string): HRESULT;
var
  IsOn: Boolean;
  HasPower: Boolean;
  ReportedOn: Boolean;
  Deadline: DWORD;
  Message,
  Key: TBytes;

begin

  ADetail := '';
  Result := ReadPower(IsOn);

  if Failed(Result) then
  begin
    ADetail := 'Cannot read TV power state; no power key was sent.';
    Exit;
  end;

  if not IsOn then
  begin
    ADetail := 'TV is already in standby; no power key was sent.';
    Exit(S_OK);
  end;

  // This receiver accepts its power key after the remote handshake has settled.
  // Keep servicing pings and power updates during the same five-second interval
  // used in the successful physical test. Recheck state before the toggle.
  Deadline := GetTickCount + FSettleMs;

  repeat
    Result := ReadMessage(Message,
                          Deadline);

    if (Result = TimeoutResult) then
      Break;

    if Failed(Result) then
      begin
        ADetail := 'TV remote disconnected before the power key; no key was sent.';
        Exit;
      end;

    Result := ProcessMessage(Message, HasPower, ReportedOn);

    if Failed(Result) then
      Exit;

    if HasPower then
      IsOn := ReportedOn;
  until False;


  if not IsOn then
    begin
      ADetail := 'TV entered standby before the power key; no key was sent.';
      Exit(S_OK);
    end;

  Key := Scalar(1,
                26); // KEYCODE_POWER. Never retry this toggle.
  Append(Key,
         Scalar(2,
         3)); // SHORT press, matching the verified paired test.

  Result := Send(MessageField(10,
                              Key));

  if Failed(Result) then
    begin
      ADetail := 'TV power command failed; it will not be retried.';
      Exit;
    end;

  Deadline := GetTickCount + FTimeoutMs;

  repeat
    Result := ReadMessage(Message,
                          Deadline);

    if Failed(Result) then
      begin
        ADetail := 'TV power command sent once; standby was not confirmed.';
        Exit;
      end;

    Result := ProcessMessage(Message,
                             HasPower,
                             IsOn);

    if Failed(Result) then
      begin
        ADetail := 'TV power command sent once; remote reported an error.';
        Exit;
      end;

    if HasPower and not IsOn then
      begin
        ADetail := 'TV standby confirmed by the paired remote.';
        Exit(S_OK);
      end;

  until False;
end;


function MfCastAndroidTvPairingFile(const ADevice: TMfCastDevice): string;
var
  Identity: string;
  Root: string;
  Name: string;
  Bytes: TBytes;
  I: Integer;

begin

  Result := '';
  Identity := Trim(ADevice.Id);

  if (Identity = '') then
    Identity := Trim(ADevice.ServiceInstance);
  Root := GetEnvironmentVariable('LOCALAPPDATA');

  if (Identity = '') or (Root = '') then
    Exit;

  Bytes := TEncoding.UTF8.GetBytes(LowerCase(Identity));
  Name := '';

  for I := 0 to High(Bytes) do
    Name := Name + IntToHex(Bytes[I],
                            2);

  if Length(Name) > 180 then
    Exit; // Bound paths from discovery data.

  Result := IncludeTrailingPathDelimiter(Root) + 'MfPack\Cast\AndroidTvRemotes\' + Name + '.pairing';
end;


function Protect(const AData: TBytes;
                 AEncrypt: Boolean;
                 out AOutput: TBytes): HRESULT;
var
  Input: TRemoteBlob;
  Output: TRemoteBlob;
  Ok: BOOL;

begin

  AOutput := nil;

  if Length(AData) = 0 then
    Exit(E_INVALIDARG);

  Input.Size := Length(AData);
  Input.Data := @AData[0];
  FillChar(Output,
           SizeOf(Output),
           0);

  if AEncrypt then
    Ok := CryptProtectData(Input,
                           'MfCast Android TV pairing',
                           nil,
                           nil,
                           nil,
                           1,
                           Output)
  else
    Ok := CryptUnprotectData(Input,
                             nil,
                             nil,
                             nil,
                             nil,
                             1,
                             Output);


  if not Ok then
    Exit(LastErrorResult);

  try
    SetLength(AOutput,
              Output.Size);

    if (Output.Size > 0) then
      Move(Output.Data^,
           AOutput[0],
           Output.Size);

    Result := S_OK;

  finally
    if (not AEncrypt) and (Output.Size > 0) then
      FillChar(Output.Data^,
               Output.Size,
               0);

    LocalFree(HLOCAL(Output.Data));
  end;
end;


function MfCastImportAndroidTvPairing(const ADevice: TMfCastDevice;
                                      const APfx: TBytes): HRESULT;
var
  FileName: string;
  ProtectedData: TBytes;
  Stream: TFileStream;

begin

  FileName := MfCastAndroidTvPairingFile(ADevice);

  if FileName = '' then
    Exit(E_INVALIDARG);

  Result := Protect(APfx,
                    True,
                    ProtectedData);

  if Failed(Result) then
    Exit;

  try
    if not ForceDirectories(ExtractFilePath(FileName)) then
      Exit(E_FAIL);

    Stream := TFileStream.Create(FileName,
                                 fmCreate);

    try
      Stream.WriteBuffer(ProtectedData[0],
                         Length(ProtectedData));

    finally
      Stream.Free;
    end;

    Result := S_OK;

  except
    Result := E_FAIL;
  end;
end;


function MfCastAndroidTvPower(const ADevice: TMfCastDevice;
                              const AStandby: Boolean;
                              out AIsOn: Boolean;
                              out ADetail: string): HRESULT;
var
  FileName, Host: string;
  Stream: TFileStream;
  Data, Pfx: TBytes;
  Blob: TRemoteBlob;
  Store, Certificate: Pointer;
  Concrete: TMfCastTcpTransport;
  Transport: IMfCastTransport;
  Session: TMfCastAndroidTvSession;
  Settings: TMfCastSettings;

begin

  AIsOn := False;
  ADetail := 'No paired Android TV standby control is configured for this device.';
  FileName := MfCastAndroidTvPairingFile(ADevice);

  if (FileName = '') or not FileExists(FileName) then
    Exit(S_FALSE);

  ADetail := 'Cannot load the paired Android TV credential.';

  try
    Stream := TFileStream.Create(FileName,
                                 fmOpenRead or fmShareDenyWrite);

    try
      if (Stream.Size = 0) or (Stream.Size > 1024 * 1024) then
        Exit(E_INVALIDARG);

      SetLength(Data,
                Stream.Size);

      Stream.ReadBuffer(Data[0],
                        Length(Data));

    finally
      Stream.Free;
    end;

    Result := Protect(Data, False, Pfx);

    if Failed(Result) then
      Exit;

    Blob.Size := Length(Pfx);
    Blob.Data := @Pfx[0];
    Store := PFXImportCertStore(Blob,
                                '',
                                $1000 or $8000); // User, ephemeral key.

    FillChar(Pfx[0],
             Length(Pfx),
             0);

    if Store = nil then
      Exit(LastErrorResult);

    try
      Certificate := CertEnumCertificatesInStore(Store,
                                                 nil);

      if (Certificate = nil) then
        Exit(E_FAIL);

      try
        Concrete := TMfCastTcpTransport.Create();
        Transport := Concrete;

        try
          Settings := TMfCastSettings.CreateDefault();
          Settings.Protocol.ConnectTimeoutMs := 5000;
          Settings.Protocol.ReadTimeoutMs := 1000;
          Settings.Protocol.WriteTimeoutMs := 1000;
          Settings.Protocol.VerifyTlsPeer := False;
          Result := Transport.Configure(Settings.Protocol);

          if Failed(Result) then
            Exit;
          Result := Concrete.SetClientCertificate(Certificate);

          if Failed(Result) then
            Exit;

          Host := Trim(ADevice.Address);

          if (Host = '') then
            Host := Trim(ADevice.HostName);

          if (Host = '') then
            Exit(E_INVALIDARG);

          ADetail := 'Cannot connect to the paired Android TV remote.';
          Result := Transport.Connect(Host,
                                      6466);

          if Failed(Result) then
            Exit;

          Session := TMfCastAndroidTvSession.Create(Transport);

          try
            if AStandby then
              Result := Session.Standby(ADetail)
            else
              begin
                Result := Session.ReadPower(AIsOn);
                if Succeeded(Result) then
                  ADetail := 'Paired TV power state read successfully.';
              end;

          finally
             Session.Free;
          end;

        finally
          Transport.Disconnect;
          Transport := nil;
        end;

      finally
        CertFreeCertificateContext(Certificate);
      end;

    finally
      CertCloseStore(Store, 0);
    end;

  except
    on E: Exception do
      begin
        ADetail := 'Android TV standby failed: ' + E.Message;
        Result := E_FAIL;
     end;
  end;
end;


// External

{$WARN SYMBOL_PLATFORM OFF}

function CryptProtectData;            external Crypt32Lib name 'CryptProtectData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CryptUnprotectData;          external Crypt32Lib name 'CryptUnprotectData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PFXImportCertStore;          external Crypt32Lib name 'PFXImportCertStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CertEnumCertificatesInStore; external Crypt32Lib name 'CertEnumCertificatesInStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CertFreeCertificateContext;  external Crypt32Lib name 'CertFreeCertificateContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CertCloseStore;              external Crypt32Lib name 'CertCloseStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}
end.

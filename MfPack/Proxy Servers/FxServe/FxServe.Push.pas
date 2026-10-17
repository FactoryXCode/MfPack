// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Push.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Standards-based Web Push support for FxAlert.
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

unit FxServe.Push;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  FxServe.Config,
  FxServe.Logging;

type
  TFxWebPushManager = class;

  TFxWebPushThread = class(TThread)
  private
    FOwner: TFxWebPushManager;
  protected
    procedure Execute(); override;
  public
    constructor Create(AOwner: TFxWebPushManager);
  end;

  TFxWebPushManager = class
  private
    FConfig: TFxServeConfig;
    FLogger: TFxServeLogger;
    FLock: TCriticalSection;
    FStopEvent: TEvent;
    FThread: TFxWebPushThread;
    FSubscriptions: TStringList;
    FPrivateKeyBlob: TBytes;
    FPublicKeyText: string;
    FLastAlertId: string;
    FStaleReported: Boolean;
    FStatusMissingSince: Cardinal;

    procedure LoadOrCreateKey();
    procedure LoadSubscriptions();
    procedure SaveSubscriptions();
    procedure PollStatus();
    procedure SendWarningToAll(const AAlertId: string);
    function SendPush(const AEndpoint: string; out AStatusCode: Cardinal): Boolean;
  public
    constructor Create(AConfig: TFxServeConfig; ALogger: TFxServeLogger);
    destructor Destroy(); override;
    procedure Start();
    procedure Stop();
    function PublicKey: string;
    function Subscribe(const AJson: string; out AError: string): Boolean;
    function Unsubscribe(const AJson: string; out AError: string): Boolean;
  end;

implementation

uses
  System.DateUtils,
  System.IOUtils,
  System.JSON;

type
  BCRYPT_ALG_HANDLE = Pointer;
  BCRYPT_KEY_HANDLE = Pointer;
  NTSTATUS = LongInt;
  HINTERNET = Pointer;
  INTERNET_PORT = Word;
  HCRYPTPROV = NativeUInt;
  HCRYPTHASH = NativeUInt;

  PBcryptEccKeyBlob = ^TBcryptEccKeyBlob;
  TBcryptEccKeyBlob = packed record
    Magic: Cardinal;
    KeySize: Cardinal;
  end;

  PDataBlob = ^TDataBlob;
  TDataBlob = record
    Size: Cardinal;
    Data: PByte;
  end;

const
  BCRYPT_ECDSA_P256_ALGORITHM = 'ECDSA_P256';
  BCRYPT_ECCPRIVATE_BLOB = 'ECCPRIVATEBLOB';
  BCRYPT_ECCPUBLIC_BLOB = 'ECCPUBLICBLOB';
  BCRYPT_ECDSA_PRIVATE_P256_MAGIC = $32534345;
  BCRYPT_ECDSA_PUBLIC_P256_MAGIC = $31534345;
  CRYPTPROTECT_LOCAL_MACHINE = $00000004;
  MOVEFILE_REPLACE_EXISTING = $00000001;
  MOVEFILE_WRITE_THROUGH = $00000008;
  WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY = 4;
  WINHTTP_FLAG_SECURE = $00800000;
  WINHTTP_QUERY_STATUS_CODE = 19;
  WINHTTP_QUERY_FLAG_NUMBER = $20000000;
  PUSH_MAX_JSON_BYTES = 16384;
  PUSH_STALE_SECONDS = 60;
  PROV_RSA_AES = 24;
  CRYPT_VERIFYCONTEXT = $F0000000;
  CALG_SHA_256 = $0000800C;
  HP_HASHVAL = 2;

function BCryptOpenAlgorithmProvider(out AHandle: BCRYPT_ALG_HANDLE;
  AAlgorithm, AImplementation: PWideChar; AFlags: Cardinal): NTSTATUS; stdcall;
  external 'bcrypt.dll';
function BCryptCloseAlgorithmProvider(AHandle: BCRYPT_ALG_HANDLE;
  AFlags: Cardinal): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptGenerateKeyPair(AAlgorithm: BCRYPT_ALG_HANDLE;
  out AKey: BCRYPT_KEY_HANDLE; ALength, AFlags: Cardinal): NTSTATUS; stdcall;
  external 'bcrypt.dll';
function BCryptFinalizeKeyPair(AKey: BCRYPT_KEY_HANDLE;
  AFlags: Cardinal): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptExportKey(AKey, AExportKey: BCRYPT_KEY_HANDLE;
  ABlobType: PWideChar; AOutput: PByte; AOutputSize: Cardinal;
  out AResultSize: Cardinal; AFlags: Cardinal): NTSTATUS; stdcall;
  external 'bcrypt.dll';
function BCryptImportKeyPair(AAlgorithm: BCRYPT_ALG_HANDLE;
  AImportKey: BCRYPT_KEY_HANDLE; ABlobType: PWideChar;
  out AKey: BCRYPT_KEY_HANDLE; AInput: PByte; AInputSize,
  AFlags: Cardinal): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptDestroyKey(AKey: BCRYPT_KEY_HANDLE): NTSTATUS; stdcall;
  external 'bcrypt.dll';
function BCryptSignHash(AKey: BCRYPT_KEY_HANDLE; APaddingInfo: Pointer;
  AInput: PByte; AInputSize: Cardinal; AOutput: PByte;
  AOutputSize: Cardinal; out AResultSize: Cardinal;
  AFlags: Cardinal): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptVerifySignature(AKey: BCRYPT_KEY_HANDLE; APaddingInfo: Pointer;
  AHash: PByte; AHashSize: Cardinal; ASignature: PByte;
  ASignatureSize, AFlags: Cardinal): NTSTATUS; stdcall; external 'bcrypt.dll';

function CryptProtectData(const AInput: TDataBlob; ADescription: PWideChar;
  AEntropy: PDataBlob; AReserved: Pointer; APrompt: Pointer; AFlags: Cardinal;
  out AOutput: TDataBlob): BOOL; stdcall; external 'crypt32.dll' name 'CryptProtectData';
function CryptUnprotectData(const AInput: TDataBlob; ADescription: PPWideChar;
  AEntropy: PDataBlob; AReserved: Pointer; APrompt: Pointer; AFlags: Cardinal;
  out AOutput: TDataBlob): BOOL; stdcall; external 'crypt32.dll' name 'CryptUnprotectData';

function WinHttpOpen(AUserAgent: PWideChar; AAccessType: Cardinal;
  AProxyName, AProxyBypass: PWideChar; AFlags: Cardinal): HINTERNET; stdcall;
  external 'winhttp.dll';
function WinHttpConnect(ASession: HINTERNET; AServerName: PWideChar;
  AServerPort: INTERNET_PORT; AReserved: Cardinal): HINTERNET; stdcall;
  external 'winhttp.dll';
function WinHttpOpenRequest(AConnect: HINTERNET; AVerb, AObjectName,
  AVersion, AReferrer: PWideChar; AAcceptTypes: Pointer;
  AFlags: Cardinal): HINTERNET; stdcall; external 'winhttp.dll';
function WinHttpSendRequest(ARequest: HINTERNET; AHeaders: PWideChar;
  AHeadersLength: Cardinal; AOptional: Pointer; AOptionalLength,
  ATotalLength: Cardinal; AContext: NativeUInt): BOOL; stdcall;
  external 'winhttp.dll';
function WinHttpReceiveResponse(ARequest: HINTERNET;
  AReserved: Pointer): BOOL; stdcall; external 'winhttp.dll';
function WinHttpQueryHeaders(ARequest: HINTERNET; AInfoLevel: Cardinal;
  AName: PWideChar; ABuffer: Pointer; var ABufferLength: Cardinal;
  var AIndex: Cardinal): BOOL; stdcall; external 'winhttp.dll';
function WinHttpSetTimeouts(AInternet: HINTERNET; AResolveTimeout,
  AConnectTimeout, ASendTimeout, AReceiveTimeout: Integer): BOOL; stdcall;
  external 'winhttp.dll';
function WinHttpCloseHandle(AInternet: HINTERNET): BOOL; stdcall;
  external 'winhttp.dll';
function CryptAcquireContextW(var phProv: HCRYPTPROV; pszContainer,
  pszProvider: PWideChar; dwProvType, dwFlags: Cardinal): BOOL; stdcall;
  external 'advapi32.dll';
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: Cardinal): BOOL; stdcall;
  external 'advapi32.dll';
function CryptCreateHash(hProv: HCRYPTPROV; Algid: Cardinal; hKey: NativeUInt;
  dwFlags: Cardinal; var phHash: HCRYPTHASH): BOOL; stdcall;
  external 'advapi32.dll';
function CryptHashData(hHash: HCRYPTHASH; pbData: PByte; dwDataLen,
  dwFlags: Cardinal): BOOL; stdcall; external 'advapi32.dll';
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: Cardinal;
  pbData: PByte; var pdwDataLen: Cardinal; dwFlags: Cardinal): BOOL; stdcall;
  external 'advapi32.dll';
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;
  external 'advapi32.dll';

procedure CheckStatus(const AStatus: NTSTATUS; const AOperation: string);
begin
  if AStatus <> 0 then
    raise Exception.CreateFmt('%s failed (CNG status 0x%.8x).',
                              [AOperation, Cardinal(AStatus)]);
end;

function BytesOfUtf8(const AText: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(AText);
end;

function Base64Encode(const ABytes: TBytes): string;
const
  TABLE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I, Remaining: Integer;
  Value: Cardinal;
begin
  Result := '';
  I := 0;
  while I < Length(ABytes) do
    begin
      Remaining := Length(ABytes) - I;
      Value := Cardinal(ABytes[I]) shl 16;
      if Remaining > 1 then
        Value := Value or (Cardinal(ABytes[I + 1]) shl 8);
      if Remaining > 2 then
        Value := Value or Cardinal(ABytes[I + 2]);
      Result := Result + TABLE[((Value shr 18) and $3F) + 1] +
                         TABLE[((Value shr 12) and $3F) + 1];
      if Remaining > 1 then
        Result := Result + TABLE[((Value shr 6) and $3F) + 1]
      else
        Result := Result + '=';
      if Remaining > 2 then
        Result := Result + TABLE[(Value and $3F) + 1]
      else
        Result := Result + '=';
      Inc(I, 3);
    end;
end;

function Base64UrlEncode(const ABytes: TBytes): string;
begin
  Result := StringReplace(Base64Encode(ABytes), '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  while (Result <> '') and (Result[Length(Result)] = '=') do
    Delete(Result, Length(Result), 1);
end;

function ProtectBytes(const ABytes: TBytes): TBytes;
var
  Input, Output: TDataBlob;
begin
  FillChar(Input, SizeOf(Input), 0);
  FillChar(Output, SizeOf(Output), 0);
  Input.Size := Length(ABytes);
  if Input.Size > 0 then
    Input.Data := @ABytes[0];
  if not CryptProtectData(Input, 'FxServe FxAlert Web Push', nil, nil, nil,
                          CRYPTPROTECT_LOCAL_MACHINE, Output) then
    RaiseLastOSError();
  try
    SetLength(Result, Output.Size);
    if Output.Size > 0 then
      Move(Output.Data^, Result[0], Output.Size);
  finally
    if Assigned(Output.Data) then
      LocalFree(HLOCAL(Output.Data));
  end;
end;

function UnprotectBytes(const ABytes: TBytes): TBytes;
var
  Input, Output: TDataBlob;
begin
  FillChar(Input, SizeOf(Input), 0);
  FillChar(Output, SizeOf(Output), 0);
  Input.Size := Length(ABytes);
  if Input.Size > 0 then
    Input.Data := @ABytes[0];
  if not CryptUnprotectData(Input, nil, nil, nil, nil,
                            CRYPTPROTECT_LOCAL_MACHINE, Output) then
    RaiseLastOSError();
  try
    SetLength(Result, Output.Size);
    if Output.Size > 0 then
      Move(Output.Data^, Result[0], Output.Size);
  finally
    if Assigned(Output.Data) then
      LocalFree(HLOCAL(Output.Data));
  end;
end;

procedure AtomicWriteProtected(const AFileName: string; const ABytes: TBytes);
var
  ProtectedData: TBytes;
  TempName: string;
begin
  ForceDirectories(ExtractFileDir(AFileName));
  ProtectedData := ProtectBytes(ABytes);
  TempName := AFileName + '.tmp';
  TFile.WriteAllBytes(TempName, ProtectedData);
  if not MoveFileEx(PChar(TempName), PChar(AFileName),
                    MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    begin
      DeleteFile(PChar(TempName));
      RaiseLastOSError();
    end;
end;

function ReadProtected(const AFileName: string): TBytes;
begin
  Result := UnprotectBytes(TFile.ReadAllBytes(AFileName));
end;

function Sha256(const AData: TBytes): TBytes;
var
  Provider: HCRYPTPROV;
  Hash: HCRYPTHASH;
  Size: Cardinal;
begin
  Provider := 0;
  Hash := 0;
  if not CryptAcquireContextW(Provider, nil, nil, PROV_RSA_AES,
                               CRYPT_VERIFYCONTEXT) then
    RaiseLastOSError();
  try
    if not CryptCreateHash(Provider, CALG_SHA_256, 0, 0, Hash) then
      RaiseLastOSError();
    try
      if (Length(AData) > 0) and
         not CryptHashData(Hash, @AData[0], Length(AData), 0) then
        RaiseLastOSError();
      Size := 32;
      SetLength(Result, Size);
      if not CryptGetHashParam(Hash, HP_HASHVAL, @Result[0], Size, 0) then
        RaiseLastOSError();
      SetLength(Result, Size);
    finally
      CryptDestroyHash(Hash);
    end;
  finally
    CryptReleaseContext(Provider, 0);
  end;
end;

function JsonQuote(const AValue: string): string;
var
  Value: TJSONString;
begin
  Value := TJSONString.Create(AValue);
  try
    Result := Value.ToJSON();
  finally
    Value.Free();
  end;
end;

function JsonText(const AObject: TJSONObject; const AName: string): string;
var
  Pair: TJSONPair;
begin
  Result := '';
  Pair := AObject.Get(AName);
  if Assigned(Pair) and Assigned(Pair.JsonValue) then
    Result := Pair.JsonValue.Value;
end;

function ParseEndpoint(const AJson: string; out AEndpoint,
  AError: string): Boolean;
var
  Value: TJSONValue;
begin
  Result := False;
  AEndpoint := '';
  AError := '';
  if Length(BytesOfUtf8(AJson)) > PUSH_MAX_JSON_BYTES then
    begin
      AError := 'Subscription is too large.';
      Exit;
    end;
  Value := TJSONObject.ParseJSONValue(AJson);
  try
    if not (Value is TJSONObject) then
      begin
        AError := 'Invalid subscription JSON.';
        Exit;
      end;
    AEndpoint := Trim(JsonText(TJSONObject(Value), 'endpoint'));
    if (Length(AEndpoint) < 12) or (Length(AEndpoint) > 4096) or
       (Copy(LowerCase(AEndpoint), 1, 8) <> 'https://') then
      begin
        AError := 'The push endpoint must be an HTTPS URL.';
        Exit;
      end;
    if (Pos(#13, AEndpoint) > 0) or (Pos(#10, AEndpoint) > 0) then
      begin
        AError := 'Invalid push endpoint.';
        Exit;
      end;
    Result := True;
  finally
    Value.Free();
  end;
end;

procedure ParseHttpsUrl(const AUrl: string; out AHost: string;
  out APort: INTERNET_PORT; out APath, AAudience: string);
var
  Rest, Authority, PortText: string;
  P, CloseBracket: Integer;
begin
  if Copy(LowerCase(AUrl), 1, 8) <> 'https://' then
    raise Exception.Create('Push endpoint is not HTTPS.');
  Rest := Copy(AUrl, 9, MaxInt);
  P := Pos('/', Rest);
  if P = 0 then
    begin
      Authority := Rest;
      APath := '/';
    end
  else
    begin
      Authority := Copy(Rest, 1, P - 1);
      APath := Copy(Rest, P, MaxInt);
    end;
  if (Authority = '') or (Pos('@', Authority) > 0) then
    raise Exception.Create('Invalid push endpoint authority.');
  AAudience := 'https://' + Authority;
  APort := 443;
  if Authority[1] = '[' then
    begin
      CloseBracket := Pos(']', Authority);
      if CloseBracket = 0 then
        raise Exception.Create('Invalid IPv6 push endpoint.');
      AHost := Copy(Authority, 2, CloseBracket - 2);
      if (Length(Authority) > CloseBracket) and
         (Authority[CloseBracket + 1] = ':') then
        begin
          PortText := Copy(Authority, CloseBracket + 2, MaxInt);
          APort := StrToInt(PortText);
        end;
    end
  else
    begin
      P := LastDelimiter(':', Authority);
      if P > 0 then
        begin
          AHost := Copy(Authority, 1, P - 1);
          PortText := Copy(Authority, P + 1, MaxInt);
          APort := StrToInt(PortText);
        end
      else
        AHost := Authority;
    end;
  if AHost = '' then
    raise Exception.Create('Push endpoint host is empty.');
end;

function UnixNow: Int64;
begin
  Result := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now));
end;

{ TFxWebPushThread }

constructor TFxWebPushThread.Create(AOwner: TFxWebPushManager);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

procedure TFxWebPushThread.Execute();
begin
  while not Terminated do
    begin
      try
        FOwner.PollStatus();
      except
        on E: Exception do
          FOwner.FLogger.Error('FxAlert push monitor: ' + E.Message);
      end;
      if FOwner.FStopEvent.WaitFor(FOwner.FConfig.PushPollIntervalMs) = wrSignaled then
        Exit;
    end;
end;

{ TFxWebPushManager }

constructor TFxWebPushManager.Create(AConfig: TFxServeConfig;
  ALogger: TFxServeLogger);
begin
  inherited Create();
  FConfig := AConfig;
  FLogger := ALogger;
  FLock := TCriticalSection.Create();
  FStopEvent := TEvent.Create(nil, True, False, '');
  FSubscriptions := TStringList.Create();
  FSubscriptions.Sorted := True;
  FSubscriptions.Duplicates := dupIgnore;
  LoadOrCreateKey();
  LoadSubscriptions();
end;

destructor TFxWebPushManager.Destroy();
begin
  Stop();
  FSubscriptions.Free();
  FStopEvent.Free();
  FLock.Free();
  inherited Destroy();
end;

procedure TFxWebPushManager.LoadOrCreateKey();
var
  Algorithm: BCRYPT_ALG_HANDLE;
  Key, PublicKey: BCRYPT_KEY_HANDLE;
  Size, SignatureSize: Cardinal;
  PublicBytes, PublicBlob, Hash, Signature: TBytes;
begin
  if FileExists(FConfig.PushVapidKeyFile) then
    FPrivateKeyBlob := ReadProtected(FConfig.PushVapidKeyFile)
  else
    begin
      Algorithm := nil;
      Key := nil;
      CheckStatus(BCryptOpenAlgorithmProvider(Algorithm,
        BCRYPT_ECDSA_P256_ALGORITHM, nil, 0), 'Opening the P-256 provider');
      try
        CheckStatus(BCryptGenerateKeyPair(Algorithm, Key, 256, 0),
                    'Generating the VAPID key');
        try
          CheckStatus(BCryptFinalizeKeyPair(Key, 0), 'Finalizing the VAPID key');
          Size := 0;
          CheckStatus(BCryptExportKey(Key, nil, BCRYPT_ECCPRIVATE_BLOB,
            nil, 0, Size, 0), 'Sizing the VAPID key');
          SetLength(FPrivateKeyBlob, Size);
          CheckStatus(BCryptExportKey(Key, nil, BCRYPT_ECCPRIVATE_BLOB,
            @FPrivateKeyBlob[0], Size, Size, 0), 'Exporting the VAPID key');
          SetLength(FPrivateKeyBlob, Size);
          AtomicWriteProtected(FConfig.PushVapidKeyFile, FPrivateKeyBlob);
        finally
          BCryptDestroyKey(Key);
        end;
      finally
        BCryptCloseAlgorithmProvider(Algorithm, 0);
      end;
      FLogger.Info('Generated a machine-protected FxAlert VAPID key.');
    end;

  if (Length(FPrivateKeyBlob) <> 104) or
     (PBcryptEccKeyBlob(@FPrivateKeyBlob[0])^.Magic <>
      BCRYPT_ECDSA_PRIVATE_P256_MAGIC) or
     (PBcryptEccKeyBlob(@FPrivateKeyBlob[0])^.KeySize <> 32) then
    raise Exception.Create('The FxAlert VAPID key file is invalid.');
  SetLength(PublicBytes, 65);
  PublicBytes[0] := 4;
  Move(FPrivateKeyBlob[8], PublicBytes[1], 64);
  FPublicKeyText := Base64UrlEncode(PublicBytes);

  { Fail startup immediately if the persisted private/public pair cannot create
    and verify the raw 64-byte ES256 signature required by VAPID. }
  Algorithm := nil;
  Key := nil;
  PublicKey := nil;
  SetLength(PublicBlob, 72);
  PBcryptEccKeyBlob(@PublicBlob[0])^.Magic := BCRYPT_ECDSA_PUBLIC_P256_MAGIC;
  PBcryptEccKeyBlob(@PublicBlob[0])^.KeySize := 32;
  Move(FPrivateKeyBlob[8], PublicBlob[8], 64);
  Hash := Sha256(BytesOfUtf8('FxServe VAPID key self-test'));
  CheckStatus(BCryptOpenAlgorithmProvider(Algorithm,
    BCRYPT_ECDSA_P256_ALGORITHM, nil, 0), 'Opening the VAPID self-test provider');
  try
    CheckStatus(BCryptImportKeyPair(Algorithm, nil, BCRYPT_ECCPRIVATE_BLOB,
      Key, @FPrivateKeyBlob[0], Length(FPrivateKeyBlob), 0),
      'Importing the VAPID self-test private key');
    CheckStatus(BCryptImportKeyPair(Algorithm, nil, BCRYPT_ECCPUBLIC_BLOB,
      PublicKey, @PublicBlob[0], Length(PublicBlob), 0),
      'Importing the VAPID self-test public key');
    try
      SignatureSize := 0;
      CheckStatus(BCryptSignHash(Key, nil, @Hash[0], Length(Hash), nil, 0,
        SignatureSize, 0), 'Sizing the VAPID self-test signature');
      SetLength(Signature, SignatureSize);
      CheckStatus(BCryptSignHash(Key, nil, @Hash[0], Length(Hash),
        @Signature[0], SignatureSize, SignatureSize, 0),
        'Creating the VAPID self-test signature');
      CheckStatus(BCryptVerifySignature(PublicKey, nil, @Hash[0], Length(Hash),
        @Signature[0], SignatureSize, 0), 'Verifying the VAPID self-test signature');
    finally
      BCryptDestroyKey(PublicKey);
      BCryptDestroyKey(Key);
    end;
  finally
    BCryptCloseAlgorithmProvider(Algorithm, 0);
  end;
end;

procedure TFxWebPushManager.LoadSubscriptions();
var
  Bytes: TBytes;
  Text: string;
  I: Integer;
begin
  if not FileExists(FConfig.PushSubscriptionFile) then
    Exit;
  Bytes := ReadProtected(FConfig.PushSubscriptionFile);
  Text := TEncoding.UTF8.GetString(Bytes);
  FSubscriptions.Text := Text;
  for I := FSubscriptions.Count - 1 downto 0 do
    if (Copy(LowerCase(Trim(FSubscriptions[I])), 1, 8) <> 'https://') or
       (Length(FSubscriptions[I]) > 4096) then
      FSubscriptions.Delete(I);
  FLogger.Info(Format('Loaded %d FxAlert push subscription(s).',
                      [FSubscriptions.Count]));
end;

procedure TFxWebPushManager.SaveSubscriptions();
var
  Text: string;
begin
  Text := FSubscriptions.Text;
  { DPAPI rejects a zero-length input buffer. A blank line round-trips as an
    empty TStringList while keeping the protected payload non-empty. }
  if Text = '' then
    Text := sLineBreak;
  AtomicWriteProtected(FConfig.PushSubscriptionFile,
                       BytesOfUtf8(Text));
end;

procedure TFxWebPushManager.Start();
begin
  if Assigned(FThread) then
    Exit;
  FStopEvent.ResetEvent();
  FThread := TFxWebPushThread.Create(Self);
  FThread.Start();
  FLogger.Info('FxAlert secure Web Push monitoring started.');
end;

procedure TFxWebPushManager.Stop();
begin
  if not Assigned(FThread) then
    Exit;
  FThread.Terminate();
  FStopEvent.SetEvent();
  FThread.WaitFor();
  FreeAndNil(FThread);
end;

function TFxWebPushManager.PublicKey(): string;
begin
  Result := FPublicKeyText;
end;

function TFxWebPushManager.Subscribe(const AJson: string;
  out AError: string): Boolean;
var
  Endpoint: string;
begin
  Result := ParseEndpoint(AJson, Endpoint, AError);
  if not Result then
    Exit;
  FLock.Acquire();
  try
    if FSubscriptions.IndexOf(Endpoint) < 0 then
      begin
        FSubscriptions.Add(Endpoint);
        SaveSubscriptions();
        FLogger.Info(Format('FxAlert push device subscribed; total=%d.',
                            [FSubscriptions.Count]));
      end;
  finally
    FLock.Release();
  end;
end;

function TFxWebPushManager.Unsubscribe(const AJson: string;
  out AError: string): Boolean;
var
  Endpoint: string;
  Index: Integer;
begin
  Result := ParseEndpoint(AJson, Endpoint, AError);
  if not Result then
    Exit;
  FLock.Acquire();
  try
    Index := FSubscriptions.IndexOf(Endpoint);
    if Index >= 0 then
      begin
        FSubscriptions.Delete(Index);
        SaveSubscriptions();
      end;
  finally
    FLock.Release();
  end;
end;

function TFxWebPushManager.SendPush(const AEndpoint: string;
  out AStatusCode: Cardinal): Boolean;
var
  Algorithm: BCRYPT_ALG_HANDLE;
  Key: BCRYPT_KEY_HANDLE;
  Session, Connection, Request: HINTERNET;
  Host, Path, Audience: string;
  Port: INTERNET_PORT;
  HeaderJson, PayloadJson, SigningInput, Token, Headers: string;
  Hash, Signature: TBytes;
  SignatureSize, StatusSize, HeaderIndex: Cardinal;
begin
  Result := False;
  AStatusCode := 0;
  ParseHttpsUrl(AEndpoint, Host, Port, Path, Audience);
  HeaderJson := '{"typ":"JWT","alg":"ES256"}';
  PayloadJson := '{"aud":' + JsonQuote(Audience) +
                 ',"exp":' + IntToStr(UnixNow() + 12 * 60 * 60) +
                 ',"sub":' + JsonQuote(FConfig.PushContact) + '}';
  SigningInput := Base64UrlEncode(BytesOfUtf8(HeaderJson)) + '.' +
                  Base64UrlEncode(BytesOfUtf8(PayloadJson));
  Hash := Sha256(BytesOfUtf8(SigningInput));

  Algorithm := nil;
  Key := nil;
  CheckStatus(BCryptOpenAlgorithmProvider(Algorithm,
    BCRYPT_ECDSA_P256_ALGORITHM, nil, 0), 'Opening the VAPID signing provider');
  try
    CheckStatus(BCryptImportKeyPair(Algorithm, nil, BCRYPT_ECCPRIVATE_BLOB,
      Key, @FPrivateKeyBlob[0], Length(FPrivateKeyBlob), 0),
      'Importing the VAPID key');
    try
      SignatureSize := 0;
      CheckStatus(BCryptSignHash(Key, nil, @Hash[0], Length(Hash), nil, 0,
        SignatureSize, 0), 'Sizing the VAPID signature');
      SetLength(Signature, SignatureSize);
      CheckStatus(BCryptSignHash(Key, nil, @Hash[0], Length(Hash),
        @Signature[0], SignatureSize, SignatureSize, 0),
        'Creating the VAPID signature');
      SetLength(Signature, SignatureSize);
    finally
      BCryptDestroyKey(Key);
    end;
  finally
    BCryptCloseAlgorithmProvider(Algorithm, 0);
  end;
  if Length(Signature) <> 64 then
    raise Exception.CreateFmt('Unexpected VAPID signature size: %d.',
                              [Length(Signature)]);
  Token := SigningInput + '.' + Base64UrlEncode(Signature);
  Headers := 'TTL: 60'#13#10 +
             'Urgency: high'#13#10 +
             'Authorization: vapid t=' + Token + ', k=' + FPublicKeyText + #13#10 +
             'Content-Length: 0'#13#10;

  Session := WinHttpOpen('FxServe FxAlert Web Push/1.0',
                         WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY, nil, nil, 0);
  if Session = nil then
    RaiseLastOSError();
  try
    WinHttpSetTimeouts(Session, 10000, 10000, 20000, 20000);
    Connection := WinHttpConnect(Session, PWideChar(Host), Port, 0);
    if Connection = nil then
      RaiseLastOSError();
    try
      Request := WinHttpOpenRequest(Connection, 'POST', PWideChar(Path),
                                    nil, nil, nil, WINHTTP_FLAG_SECURE);
      if Request = nil then
        RaiseLastOSError();
      try
        if not WinHttpSendRequest(Request, PWideChar(Headers), Length(Headers),
                                  nil, 0, 0, 0) then
          RaiseLastOSError();
        if not WinHttpReceiveResponse(Request, nil) then
          RaiseLastOSError();
        StatusSize := SizeOf(AStatusCode);
        HeaderIndex := 0;
        if not WinHttpQueryHeaders(Request,
          WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER, nil,
          @AStatusCode, StatusSize, HeaderIndex) then
          RaiseLastOSError();
        Result := (AStatusCode >= 200) and (AStatusCode < 300);
      finally
        WinHttpCloseHandle(Request);
      end;
    finally
      WinHttpCloseHandle(Connection);
    end;
  finally
    WinHttpCloseHandle(Session);
  end;
end;

procedure TFxWebPushManager.SendWarningToAll(const AAlertId: string);
var
  Snapshot, Expired: TStringList;
  I, Index: Integer;
  StatusCode: Cardinal;
begin
  Snapshot := TStringList.Create();
  Expired := TStringList.Create();
  try
    FLock.Acquire();
    try
      Snapshot.Assign(FSubscriptions);
    finally
      FLock.Release();
    end;
    for I := 0 to Snapshot.Count - 1 do
      try
        if not SendPush(Snapshot[I], StatusCode) then
          begin
            if (StatusCode = 404) or (StatusCode = 410) then
              Expired.Add(Snapshot[I])
            else
              FLogger.Error(Format('FxAlert push rejected with HTTP %d.',
                                   [StatusCode]));
          end;
      except
        on E: Exception do
          FLogger.Error('FxAlert push delivery failed: ' + E.Message);
      end;
    if Expired.Count > 0 then
      begin
        FLock.Acquire();
        try
          for I := 0 to Expired.Count - 1 do
            begin
              Index := FSubscriptions.IndexOf(Expired[I]);
              if Index >= 0 then
                FSubscriptions.Delete(Index);
            end;
          SaveSubscriptions();
        finally
          FLock.Release();
        end;
      end;
    FLogger.Info(Format('FxAlert warning %s pushed to %d device(s).',
                        [AAlertId, Snapshot.Count]));
  finally
    Expired.Free();
    Snapshot.Free();
  end;
end;

procedure TFxWebPushManager.PollStatus();
var
  FileName, Text, State, AlertId: string;
  Value: TJSONValue;
  AlertPair: TJSONPair;
  FileTime: TDateTime;
  CurrentTick: Cardinal;
begin
  FileName := TPath.Combine(TPath.Combine(FConfig.WebRoot, 'FxAlert'),
                            'status.json');
  if not FileExists(FileName) then
    begin
      CurrentTick := GetTickCount();
      if FStatusMissingSince = 0 then
        FStatusMissingSince := CurrentTick
      else if (Cardinal(CurrentTick - FStatusMissingSince) >=
               PUSH_STALE_SECONDS * 1000) and not FStaleReported then
        begin
          FStaleReported := True;
          SendWarningToAll('heartbeat-missing');
        end;
      Exit;
    end;
  FStatusMissingSince := 0;
  FileTime := TFile.GetLastWriteTime(FileName);
  if SecondsBetween(Now, FileTime) > PUSH_STALE_SECONDS then
    begin
      if not FStaleReported then
        begin
          FStaleReported := True;
          SendWarningToAll('heartbeat-lost');
        end;
      Exit;
    end;
  FStaleReported := False;
  Text := TFile.ReadAllText(FileName, TEncoding.UTF8);
  Value := TJSONObject.ParseJSONValue(Text);
  try
    if not (Value is TJSONObject) then
      Exit;
    State := LowerCase(JsonText(TJSONObject(Value), 'state'));
    if (State <> 'warning') and (State <> 'critical') and
       (State <> 'stopped') then
      begin
        if Pos('state-', FLastAlertId) = 1 then
          FLastAlertId := '';
        Exit;
      end;
    AlertId := 'state-' + State;
    AlertPair := TJSONObject(Value).Get('alert');
    if Assigned(AlertPair) and (AlertPair.JsonValue is TJSONObject) and
       (JsonText(TJSONObject(AlertPair.JsonValue), 'id') <> '') then
      AlertId := JsonText(TJSONObject(AlertPair.JsonValue), 'id');
    if AlertId = FLastAlertId then
      Exit;
    FLastAlertId := AlertId;
    SendWarningToAll(AlertId);
  finally
    Value.Free();
  end;
end;

end.

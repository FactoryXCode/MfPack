// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Certificate.pas
// Kind: Pascal Unit
// Release date: 04-09-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Internal ACME certificate lifecycle manager for FxServe.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 04/09/2026 All                 Added internal ACME and HTTP.sys management.
//------------------------------------------------------------------------------
//
// Remarks: Persistent settings are protected below HKLM. Private keys are
//          non-exportable machine keys and are never stored in configuration.
//
//==============================================================================
unit FxServe.Certificate;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Win.Registry,
  {FxServe}
  FxServe.Logging;

const
  FXSERVE_CERTIFICATE_REGISTRY_KEY = '\Software\FactoryX\FxServe\Certificate';
  FXSERVE_ACME_PRODUCTION_DIRECTORY = 'https://acme-v02.api.letsencrypt.org/directory';
  FXSERVE_ACME_STAGING_DIRECTORY = 'https://acme-staging-v02.api.letsencrypt.org/directory';

type
  TFxServeCertificateSettings = record
    Enabled: Boolean;
    HostName: string;
    ContactEmail: string;
    TermsAccepted: Boolean;
    DirectoryUrl: string;
    RenewBeforeDays: Integer;
    CheckIntervalMinutes: Integer;
    AccountUrl: string;
  end;

  IFxServeAcmeChallengeSource = interface
    ['{D81C50D3-A645-4E90-BE7C-D2154ED558F8}']
    function TryGetAcmeChallenge(const ATarget: string;
                                 out AResponse: string): Boolean;
  end;

  TFxServeCertificateManager = class;

  TFxServeCertificateThread = class(TThread)
  private
    FManager: TFxServeCertificateManager;
  protected
    procedure Execute(); override;
  public
    constructor Create(AManager: TFxServeCertificateManager);
  end;

  TFxServeCertificateManager = class(TInterfacedObject,
                                     IFxServeAcmeChallengeSource)
  private
    FLogger: TFxServeLogger;
    FLock: TCriticalSection;
    FStopEvent: TEvent;
    FThread: TFxServeCertificateThread;
    FChallengeToken: string;
    FChallengeResponse: string;
    FHostName: string;
    FHttpsPort: Word;
    FHttpChallengeAvailable: Boolean;

    procedure Execute();
    procedure EnsureCertificate();
    procedure SetChallenge(const AToken: string;
                           const AResponse: string);
    procedure ClearChallenge();
  public
    constructor Create(ALogger: TFxServeLogger;
                       const AHostName: string;
                       const AHttpsPort: Word;
                       const AHttpChallengeAvailable: Boolean);
    destructor Destroy(); override;

    procedure Start();
    procedure Stop();

    function TryGetAcmeChallenge(const ATarget: string;
                                 out AResponse: string): Boolean;
  end;

procedure ConfigureFxServeCertificate(const AHostName: string;
                                      const AContactEmail: string;
                                      const ATermsAccepted: Boolean;
                                      const AUseStaging: Boolean = False);

procedure DisableFxServeCertificate();
function LoadFxServeCertificateSettings(out ASettings: TFxServeCertificateSettings): Boolean;

implementation

const
  FXSERVE_HTTP_SYS_APP_ID: TGUID = '{B44BA4CF-2D33-49C4-B8E6-46B16AB7E37E}';
  FXSERVE_ACME_ACCOUNT_CONTAINER = 'FactoryX FxServe ACME Account';
  FXSERVE_CERTIFICATE_CONTAINER_PREFIX = 'FactoryX FxServe Certificate ';
  MICROSOFT_RSA_AES_PROVIDER = 'Microsoft Enhanced RSA and AES Cryptographic Provider';

  PROV_RSA_AES = 24;
  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $00000008;
  CRYPT_DELETEKEYSET = $00000010;
  CRYPT_MACHINE_KEYSET = $00000020;
  AT_KEYEXCHANGE = 1;
  AT_SIGNATURE = 2;
  CALG_SHA_256 = $0000800C;
  PUBLICKEYBLOB = 6;
  CUR_BLOB_VERSION = 2;
  RSA1_MAGIC = $31415352;

  X509_ASN_ENCODING = $00000001;
  PKCS_7_ASN_ENCODING = $00010000;
  CERT_STORE_PROV_SYSTEM_W = 10;
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;
  CERT_STORE_OPEN_EXISTING_FLAG = $00004000;
  CERT_STORE_READONLY_FLAG = $00008000;
  CERT_STORE_ADD_REPLACE_EXISTING = 3;
  CERT_HASH_PROP_ID = 3;
  CERT_KEY_PROV_INFO_PROP_ID = 2;
  CERT_NAME_DNS_TYPE = 6;
  CRYPT_ACQUIRE_SILENT_FLAG = $00000040;
  CERT_NCRYPT_KEY_SPEC = $FFFFFFFF;

  HTTP_INITIALIZE_CONFIG = $00000002;
  HTTP_SERVICE_CONFIG_SSL_SNI_CERT_INFO = 3;
  HTTP_SERVICE_CONFIG_QUERY_EXACT = 0;

  WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY = 4;
  WINHTTP_FLAG_SECURE = $00800000;
  WINHTTP_QUERY_STATUS_CODE = 19;
  WINHTTP_QUERY_LOCATION = 33;
  WINHTTP_QUERY_CUSTOM = 65535;
  WINHTTP_QUERY_FLAG_NUMBER = $20000000;
  INTERNET_DEFAULT_HTTPS_PORT = 443;

type
  HCRYPTPROV = NativeUInt;
  HCRYPTKEY = NativeUInt;
  HCRYPTHASH = NativeUInt;
  HCERTSTORE = Pointer;
  HINTERNET = Pointer;
  INTERNET_PORT = Word;

  TBLOBHEADER = packed record
    bType: Byte;
    bVersion: Byte;
    reserved: Word;
    aiKeyAlg: DWORD;
  end;

  TRSAPUBKEY = packed record
    magic: DWORD;
    bitlen: DWORD;
    pubexp: DWORD;
  end;

  TCryptAlgorithmIdentifier = record
    pszObjId: PAnsiChar;
    Parameters: record
      cbData: DWORD;
      pbData: PByte;
    end;
  end;

  TCertInfoPrefix = record
    dwVersion: DWORD;
    SerialNumber: record
      cbData: DWORD;
      pbData: PByte;
    end;
    SignatureAlgorithm: TCryptAlgorithmIdentifier;
    Issuer: record
      cbData: DWORD;
      pbData: PByte;
    end;
    NotBefore: TFileTime;
    NotAfter: TFileTime;
  end;
  PCertInfoPrefix = ^TCertInfoPrefix;

  TCertContext = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: Pointer;
    hCertStore: HCERTSTORE;
  end;
  PCertContext = ^TCertContext;

  TCryptKeyProvInfo = record
    pwszContainerName: PWideChar;
    pwszProvName: PWideChar;
    dwProvType: DWORD;
    dwFlags: DWORD;
    cProvParam: DWORD;
    rgProvParam: Pointer;
    dwKeySpec: DWORD;
  end;

  TSockAddrStorage = record
    ss_family: SmallInt;
    Padding1: array[0..5] of Byte;
    Alignment: Int64;
    Padding2: array[0..111] of Byte;
  end;

  THttpServiceConfigSslSniKey = record
    IpPort: TSockAddrStorage;
    Host: PWideChar;
  end;

  THttpServiceConfigSslParam = record
    SslHashLength: DWORD;
    pSslHash: Pointer;
    AppId: TGUID;
    pSslCertStoreName: PWideChar;
    DefaultCertCheckMode: DWORD;
    DefaultRevocationFreshnessTime: DWORD;
    DefaultRevocationUrlRetrievalTimeout: DWORD;
    pDefaultSslCtlIdentifier: PWideChar;
    pDefaultSslCtlStoreName: PWideChar;
    DefaultFlags: DWORD;
  end;

  THttpServiceConfigSslSniSet = record
    KeyDesc: THttpServiceConfigSslSniKey;
    ParamDesc: THttpServiceConfigSslParam;
  end;
  PHttpServiceConfigSslSniSet = ^THttpServiceConfigSslSniSet;

  THttpServiceConfigSslSniQuery = record
    QueryDesc: DWORD;
    KeyDesc: THttpServiceConfigSslSniKey;
    dwToken: DWORD;
  end;

  TAcmeHttpResponse = record
    StatusCode: DWORD;
    Body: UTF8String;
    Location: string;
    ReplayNonce: string;
  end;

  TAcmeDirectory = record
    NewNonce: string;
    NewAccount: string;
    NewOrder: string;
  end;

  TAcmeKey = record
    Provider: HCRYPTPROV;
    Key: HCRYPTKEY;
    ContainerName: string;
    KeySpec: DWORD;
    Modulus: TBytes;
    Exponent: TBytes;
  end;

function CryptAcquireContextW(var phProv: HCRYPTPROV;
                              pszContainer: PWideChar;
                              pszProvider: PWideChar;
                              dwProvType: DWORD;
                              dwFlags: DWORD): BOOL; stdcall;
                              external 'advapi32.dll' name 'CryptAcquireContextW';

function CryptReleaseContext(hProv: HCRYPTPROV;
                             dwFlags: DWORD): BOOL; stdcall;
                             external 'advapi32.dll' name 'CryptReleaseContext';

function CryptGetUserKey(hProv: HCRYPTPROV;
                         dwKeySpec: DWORD;
                         var phUserKey: HCRYPTKEY): BOOL; stdcall;
                         external 'advapi32.dll' name 'CryptGetUserKey';

function CryptGenKey(hProv: HCRYPTPROV;
                     Algid: DWORD;
                     dwFlags: DWORD;
                     var phKey: HCRYPTKEY): BOOL; stdcall;
                     external 'advapi32.dll' name 'CryptGenKey';

function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall;
                         external 'advapi32.dll' name 'CryptDestroyKey';

function CryptExportKey(hKey: HCRYPTKEY;
                        hExpKey: HCRYPTKEY;
                        dwBlobType: DWORD;
                        dwFlags: DWORD;
                        pbData: PByte;
                        var pdwDataLen: DWORD): BOOL; stdcall;
                        external 'advapi32.dll' name 'CryptExportKey';

function CryptCreateHash(hProv: HCRYPTPROV;
                         Algid: DWORD;
                         hKey: HCRYPTKEY;
                         dwFlags: DWORD;
                         var phHash: HCRYPTHASH): BOOL; stdcall;
                         external 'advapi32.dll' name 'CryptCreateHash';

function CryptHashData(hHash: HCRYPTHASH;
                       pbData: PByte;
                       dwDataLen: DWORD;
                       dwFlags: DWORD): BOOL; stdcall;
                       external 'advapi32.dll' name 'CryptHashData';

function CryptGetHashParam(hHash: HCRYPTHASH;
                           dwParam: DWORD;
                           pbData: PByte;
                           var pdwDataLen: DWORD;
                           dwFlags: DWORD): BOOL; stdcall;
                           external 'advapi32.dll' name 'CryptGetHashParam';

function CryptSignHashW(hHash: HCRYPTHASH;
                        dwKeySpec: DWORD;
                        sDescription: PWideChar;
                        dwFlags: DWORD;
                        pbSignature: PByte;
                        var pdwSigLen: DWORD): BOOL; stdcall;
                        external 'advapi32.dll' name 'CryptSignHashW';

function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;
                          external 'advapi32.dll' name 'CryptDestroyHash';

function CertOpenStore(lpszStoreProvider: PAnsiChar;
                       dwMsgAndCertEncodingType: DWORD;
                       hCryptProv: NativeUInt;
                       dwFlags: DWORD;
                       pvPara: Pointer): HCERTSTORE; stdcall;
                       external 'crypt32.dll' name 'CertOpenStore';

function CertCloseStore(hCertStore: HCERTSTORE;
                        dwFlags: DWORD): BOOL; stdcall;
                        external 'crypt32.dll' name 'CertCloseStore';

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
                                     pPrevCertContext: PCertContext): PCertContext; stdcall;
                                     external 'crypt32.dll' name 'CertEnumCertificatesInStore';

function CertCreateCertificateContext(dwCertEncodingType: DWORD;
                                      pbCertEncoded: PByte;
                                      cbCertEncoded: DWORD): PCertContext; stdcall;
                                      external 'crypt32.dll' name 'CertCreateCertificateContext';

function CertFreeCertificateContext(pCertContext: PCertContext): BOOL; stdcall;
                                      external 'crypt32.dll' name 'CertFreeCertificateContext';

function CertAddCertificateContextToStore(hCertStore: HCERTSTORE;
                                          pCertContext: PCertContext;
                                          dwAddDisposition: DWORD;
                                          ppStoreContext: Pointer): BOOL; stdcall;
                                          external 'crypt32.dll' name 'CertAddCertificateContextToStore';

function CertSetCertificateContextProperty(pCertContext: PCertContext;
                                           dwPropId: DWORD;
                                           dwFlags: DWORD;
                                           pvData: Pointer): BOOL; stdcall;
                                           external 'crypt32.dll' name 'CertSetCertificateContextProperty';

function CertGetCertificateContextProperty(pCertContext: PCertContext;
                                           dwPropId: DWORD;
                                           pvData: Pointer;
                                           var pcbData: DWORD): BOOL; stdcall;
                                           external 'crypt32.dll' name 'CertGetCertificateContextProperty';

function CertGetNameStringW(pCertContext: PCertContext;
                            dwType: DWORD;
                            dwFlags: DWORD;
                            pvTypePara: Pointer;
                            pszNameString: PWideChar;
                            cchNameString: DWORD): DWORD; stdcall;
                            external 'crypt32.dll' name 'CertGetNameStringW';

function CryptAcquireCertificatePrivateKey(pCert: PCertContext;
                                           dwFlags: DWORD;
                                           pvReserved: Pointer;
                                           var phCryptProvOrNCryptKey: NativeUInt;
                                           var pdwKeySpec: DWORD;
                                           var pfCallerFreeProvOrNCryptKey: BOOL): BOOL; stdcall;
                                           external 'crypt32.dll' name 'CryptAcquireCertificatePrivateKey';

function NCryptFreeObject(hObject: NativeUInt): Longint; stdcall;
                          external 'ncrypt.dll' name 'NCryptFreeObject';

function HttpInitialize(Version: UInt64;
                        Flags: DWORD;
                        pReserved: Pointer): DWORD; stdcall;
                        external 'httpapi.dll' name 'HttpInitialize';

function HttpTerminate(Flags: DWORD;
                       pReserved: Pointer): DWORD; stdcall;
                       external 'httpapi.dll' name 'HttpTerminate';

function HttpSetServiceConfiguration(ServiceHandle: THandle;
                                     ConfigId: DWORD;
                                     pConfigInformation: Pointer;
                                     ConfigInformationLength: DWORD;
                                     pOverlapped: Pointer): DWORD; stdcall;
                                     external 'httpapi.dll' name 'HttpSetServiceConfiguration';

function HttpDeleteServiceConfiguration(ServiceHandle: THandle;
                                        ConfigId: DWORD;
                                        pConfigInformation: Pointer;
                                        ConfigInformationLength: DWORD;
                                        pOverlapped: Pointer): DWORD; stdcall;
                                        external 'httpapi.dll' name 'HttpDeleteServiceConfiguration';

function HttpQueryServiceConfiguration(ServiceHandle: THandle;
                                       ConfigId: DWORD;
                                       pInputConfigInformation: Pointer;
                                       InputConfigInformationLength: DWORD;
                                       pOutputConfigInformation: Pointer;
                                       OutputConfigInformationLength: DWORD;
                                       var pReturnLength: DWORD;
                                       pOverlapped: Pointer): DWORD; stdcall;
                                       external 'httpapi.dll' name 'HttpQueryServiceConfiguration';

function ConvertStringSecurityDescriptorToSecurityDescriptorW(
  StringSecurityDescriptor: PWideChar;
  StringSDRevision: DWORD;
  var SecurityDescriptor: Pointer;
  SecurityDescriptorSize: PDWORD): BOOL; stdcall;
  external 'advapi32.dll' name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';

function WinHttpOpen(pwszUserAgent: PWideChar;
                     dwAccessType: DWORD;
                     pwszProxyName: PWideChar;
                     pwszProxyBypass: PWideChar;
                     dwFlags: DWORD): HINTERNET; stdcall;
                     external 'winhttp.dll' name 'WinHttpOpen';

function WinHttpConnect(hSession: HINTERNET;
                        pswzServerName: PWideChar;
                        nServerPort: INTERNET_PORT;
                        dwReserved: DWORD): HINTERNET; stdcall;
                        external 'winhttp.dll' name 'WinHttpConnect';

function WinHttpOpenRequest(hConnect: HINTERNET;
                            pwszVerb: PWideChar;
                            pwszObjectName: PWideChar;
                            pwszVersion: PWideChar;
                            pwszReferrer: PWideChar;
                            ppwszAcceptTypes: Pointer;
                            dwFlags: DWORD): HINTERNET; stdcall;
                            external 'winhttp.dll' name 'WinHttpOpenRequest';

function WinHttpSendRequest(hRequest: HINTERNET;
                            pwszHeaders: PWideChar;
                            dwHeadersLength: DWORD;
                            lpOptional: Pointer;
                            dwOptionalLength: DWORD;
                            dwTotalLength: DWORD;
                            dwContext: NativeUInt): BOOL; stdcall;
                            external 'winhttp.dll' name 'WinHttpSendRequest';

function WinHttpReceiveResponse(hRequest: HINTERNET;
                                lpReserved: Pointer): BOOL; stdcall;
                                external 'winhttp.dll' name 'WinHttpReceiveResponse';

function WinHttpQueryHeaders(hRequest: HINTERNET;
                             dwInfoLevel: DWORD;
                             pwszName: PWideChar;
                             lpBuffer: Pointer;
                             var lpdwBufferLength: DWORD;
                             var lpdwIndex: DWORD): BOOL; stdcall;
                             external 'winhttp.dll' name 'WinHttpQueryHeaders';

function WinHttpReadData(hRequest: HINTERNET;
                         lpBuffer: Pointer;
                         dwNumberOfBytesToRead: DWORD;
                         var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
                         external 'winhttp.dll' name 'WinHttpReadData';

function WinHttpSetTimeouts(hInternet: HINTERNET;
                            nResolveTimeout: Integer;
                            nConnectTimeout: Integer;
                            nSendTimeout: Integer;
                            nReceiveTimeout: Integer): BOOL; stdcall;
                            external 'winhttp.dll' name 'WinHttpSetTimeouts';

function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall;
                            external 'winhttp.dll' name 'WinHttpCloseHandle';

procedure AppendBytes(var ADestination: TBytes;
                      const ASource: TBytes);
var
  OldLength: Integer;

begin

  if (Length(ASource) = 0) then
    Exit;

  OldLength := Length(ADestination);
  SetLength(ADestination,
            OldLength + Length(ASource));
  Move(ASource[0],
       ADestination[OldLength],
       Length(ASource));
end;


function BytesOf(const AValues: array of Byte): TBytes;
var
  I: Integer;

begin

  SetLength(Result,
            Length(AValues));

  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;


function Utf8Bytes(const AText: string): TBytes;
var
  Value: UTF8String;

begin

  Value := UTF8String(AText);
  SetLength(Result,
            Length(Value));

  if (Length(Value) > 0) then
    Move(Value[1],
         Result[0],
         Length(Value));
end;


function BytesToUtf8(const ABytes: TBytes): UTF8String;
begin

  SetLength(Result,
            Length(ABytes));

  if (Length(ABytes) > 0) then
    Move(ABytes[0],
         Result[1],
         Length(ABytes));
end;


function ReverseBytes(const ABytes: TBytes): TBytes;
var
  I: Integer;

begin

  SetLength(Result,
            Length(ABytes));

  for I := 0 to High(ABytes) do
    Result[I] := ABytes[High(ABytes) - I];
end;


function Base64Encode(const ABytes: TBytes): string;
const
  TABLE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I: Integer;
  Value: Cardinal;
  Remaining: Integer;

begin

  Result := '';
  I := 0;

  while I < Length(ABytes) do
    begin
      Remaining := Length(ABytes) - I;
      Value := Cardinal(ABytes[I]) shl 16;

      if (Remaining > 1) then
        Value := Value or (Cardinal(ABytes[I + 1]) shl 8);

      if (Remaining > 2) then
        Value := Value or Cardinal(ABytes[I + 2]);

      Result := Result + TABLE[((Value shr 18) and $3F) + 1] +
                         TABLE[((Value shr 12) and $3F) + 1];

      if (Remaining > 1) then
        Result := Result + TABLE[((Value shr 6) and $3F) + 1]
      else
        Result := Result + '=';

      if (Remaining > 2) then
        Result := Result + TABLE[(Value and $3F) + 1]
      else
        Result := Result + '=';

      Inc(I,
          3);
    end;
end;


function Base64UrlEncode(const ABytes: TBytes): string;
begin

  Result := Base64Encode(ABytes);
  Result := StringReplace(Result,
                          '+',
                          '-',
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          '/',
                          '_',
                          [rfReplaceAll]);

  while (Result <> '') and
        (Result[Length(Result)] = '=') do
    Delete(Result,
           Length(Result),
           1);
end;


function Base64Decode(const AText: string): TBytes;
var
  Clean: string;
  I: Integer;
  Count: Integer;
  Value: Cardinal;

  function DecodeChar(const C: Char): Integer;
  begin
    if CharInSet(C,
                 ['A'..'Z']) then
      Exit(Ord(C) - Ord('A'));

    if CharInSet(C,
                 ['a'..'z']) then
      Exit(Ord(C) - Ord('a') + 26);

    if CharInSet(C,
                 ['0'..'9']) then
      Exit(Ord(C) - Ord('0') + 52);

    if (C = '+') then
      Exit(62);

    if (C = '/') then
      Exit(63);

    Result := 0;
  end;

begin

  Clean := StringReplace(AText,
                         #13,
                         '',
                         [rfReplaceAll]);
  Clean := StringReplace(Clean,
                         #10,
                         '',
                         [rfReplaceAll]);
  Clean := StringReplace(Clean,
                         ' ',
                         '',
                         [rfReplaceAll]);

  if (Length(Clean) mod 4 <> 0) then
    raise Exception.Create('Invalid Base64 data.');

  SetLength(Result,
            (Length(Clean) div 4) * 3);
  Count := 0;
  I := 1;

  while I <= Length(Clean) do
    begin
      Value := Cardinal(DecodeChar(Clean[I])) shl 18;
      Value := Value or (Cardinal(DecodeChar(Clean[I + 1])) shl 12);
      Value := Value or (Cardinal(DecodeChar(Clean[I + 2])) shl 6);
      Value := Value or Cardinal(DecodeChar(Clean[I + 3]));

      Result[Count] := Byte(Value shr 16);
      Inc(Count);

      if (Clean[I + 2] <> '=') then
        begin
          Result[Count] := Byte(Value shr 8);
          Inc(Count);
        end;

      if (Clean[I + 3] <> '=') then
        begin
          Result[Count] := Byte(Value);
          Inc(Count);
        end;

      Inc(I,
          4);
    end;

  SetLength(Result,
            Count);
end;


function JsonEscape(const AText: string): string;
var
  I: Integer;
  C: Char;

begin

  Result := '';

  for I := 1 to Length(AText) do
    begin
      C := AText[I];

      case C of
        '"': Result := Result + '\"';
        '\': Result := Result + '\\';
        #8: Result := Result + '\b';
        #9: Result := Result + '\t';
        #10: Result := Result + '\n';
        #12: Result := Result + '\f';
        #13: Result := Result + '\r';
      else
        if (Ord(C) < 32) then
          Result := Result + Format('\u%.4x',
                                    [Ord(C)])
        else
          Result := Result + C;
      end;
    end;
end;


function JsonStringAt(const AJson: string;
                      const AName: string;
                      const AStart: Integer;
                      out AValue: string;
                      out AEndPosition: Integer): Boolean;
var
  P: Integer;
  I: Integer;
  Hex: string;

begin

  Result := False;
  AValue := '';
  AEndPosition := 0;
  P := Pos('"' + AName + '"',
           Copy(AJson,
                AStart,
                MaxInt));

  if (P = 0) then
    Exit;

  P := P + AStart - 1 + Length(AName) + 2;

  while (P <= Length(AJson)) and
        (AJson[P] <> ':') do
    Inc(P);

  Inc(P);

  while (P <= Length(AJson)) and
        CharInSet(AJson[P],
                  [' ', #9, #13, #10]) do
    Inc(P);

  if (P > Length(AJson)) or
     (AJson[P] <> '"') then
    Exit;

  Inc(P);
  I := P;

  while I <= Length(AJson) do
    begin
      if (AJson[I] = '"') then
        begin
          AEndPosition := I + 1;
          Exit(True);
        end;

      if (AJson[I] = '\') then
        begin
          Inc(I);

          if (I > Length(AJson)) then
            Exit;

          case AJson[I] of
            '"', '\', '/': AValue := AValue + AJson[I];
            'b': AValue := AValue + #8;
            't': AValue := AValue + #9;
            'n': AValue := AValue + #10;
            'f': AValue := AValue + #12;
            'r': AValue := AValue + #13;
            'u':
              begin
                Hex := Copy(AJson,
                            I + 1,
                            4);
                if (Length(Hex) <> 4) then
                  Exit;
                AValue := AValue + Char(StrToInt('$' + Hex));
                Inc(I,
                    4);
              end;
          end;
        end
      else
        AValue := AValue + AJson[I];

      Inc(I);
    end;
end;


function JsonString(const AJson: string;
                    const AName: string): string;
var
  EndPosition: Integer;

begin

  if not JsonStringAt(AJson,
                      AName,
                      1,
                      Result,
                      EndPosition) then
    Result := '';
end;


function JsonStringArray(const AJson: string;
                         const AName: string): TStringList;
var
  P: Integer;
  BracketPosition: Integer;
  Value: string;

begin

  Result := TStringList.Create;
  P := Pos('"' + AName + '"',
           AJson);

  if (P = 0) then
    Exit;

  BracketPosition := Pos('[',
                         Copy(AJson,
                              P,
                              MaxInt));

  if (BracketPosition = 0) then
    Exit;

  Inc(P,
      BracketPosition - 1);

  Inc(P);

  while (P <= Length(AJson)) and
        (AJson[P] <> ']') do
    begin
      while (P <= Length(AJson)) and
            (AJson[P] <> '"') and
            (AJson[P] <> ']') do
        Inc(P);

      if (P > Length(AJson)) or
         (AJson[P] = ']') then
        Break;

      Inc(P);
      Value := '';

      while (P <= Length(AJson)) and
            (AJson[P] <> '"') do
        begin
          if (AJson[P] = '\') and
             (P < Length(AJson)) then
            Inc(P);

          Value := Value + AJson[P];
          Inc(P);
        end;

      Result.Add(Value);
      Inc(P);
    end;
end;


function ReadRegistryString(const AName: string;
                            const ADefault: string): string;
var
  Registry: TRegistry;

begin

  Result := ADefault;
  Registry := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);

  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    if Registry.OpenKeyReadOnly(FXSERVE_CERTIFICATE_REGISTRY_KEY) and
       Registry.ValueExists(AName) then
      Result := Registry.ReadString(AName);

  finally
    Registry.Free;
  end;
end;


function ReadRegistryInteger(const AName: string;
                             const ADefault: Integer): Integer;
var
  Registry: TRegistry;

begin

  Result := ADefault;
  Registry := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);

  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    if Registry.OpenKeyReadOnly(FXSERVE_CERTIFICATE_REGISTRY_KEY) and
       Registry.ValueExists(AName) then
      Result := Registry.ReadInteger(AName);

  finally
    Registry.Free;
  end;
end;


function CertificateSettingsExist(): Boolean;
var
  Registry: TRegistry;

begin

  Registry := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);

  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Result := Registry.OpenKeyReadOnly(FXSERVE_CERTIFICATE_REGISTRY_KEY) and
              Registry.ValueExists('Enabled');

  finally
    Registry.Free;
  end;
end;


procedure ApplyRegistrySecurity(const ARegistry: TRegistry);
const
  SDDL_REVISION_1 = 1;
  DACL_SECURITY_INFORMATION = $00000004;
var
  SecurityDescriptor: Pointer;
  ErrorCode: LongInt;

begin

  SecurityDescriptor := nil;

  if not ConvertStringSecurityDescriptorToSecurityDescriptorW(
           'D:P(A;;KA;;;SY)(A;;KA;;;BA)',
           SDDL_REVISION_1,
           SecurityDescriptor,
           nil) then
    RaiseLastOSError();

  try
    ErrorCode := RegSetKeySecurity(ARegistry.CurrentKey,
                                   DACL_SECURITY_INFORMATION,
                                   SecurityDescriptor);

    if (ErrorCode <> ERROR_SUCCESS) then
      raise EOSError.CreateFmt('Could not protect the FxServe certificate registry key (%d).',
                               [ErrorCode]);

  finally
    LocalFree(HLOCAL(SecurityDescriptor));
  end;
end;


procedure WriteCertificateSettings(const ASettings: TFxServeCertificateSettings);
var
  Registry: TRegistry;

begin

  Registry := TRegistry.Create(KEY_READ or KEY_WRITE or KEY_WOW64_64KEY);

  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    if not Registry.OpenKey(FXSERVE_CERTIFICATE_REGISTRY_KEY,
                            True) then
      raise Exception.Create('Could not create the protected FxServe certificate registry key.');

    ApplyRegistrySecurity(Registry);
    Registry.WriteBool('Enabled',
                       ASettings.Enabled);
    Registry.WriteString('HostName',
                         LowerCase(Trim(ASettings.HostName)));
    Registry.WriteString('ContactEmail',
                         Trim(ASettings.ContactEmail));
    Registry.WriteBool('TermsAccepted',
                       ASettings.TermsAccepted);
    Registry.WriteString('DirectoryUrl',
                         ASettings.DirectoryUrl);
    Registry.WriteInteger('RenewBeforeDays',
                          ASettings.RenewBeforeDays);
    Registry.WriteInteger('CheckIntervalMinutes',
                          ASettings.CheckIntervalMinutes);

    if (ASettings.AccountUrl <> '') then
      Registry.WriteString('AccountUrl',
                           ASettings.AccountUrl);

  finally
    Registry.Free;
  end;
end;


procedure WriteAccountUrl(const AAccountUrl: string);
var
  Settings: TFxServeCertificateSettings;

begin

  if LoadFxServeCertificateSettings(Settings) then
    begin
      Settings.AccountUrl := AAccountUrl;
      WriteCertificateSettings(Settings);
    end;
end;


function LoadFxServeCertificateSettings(out ASettings: TFxServeCertificateSettings): Boolean;
begin

  ASettings.Enabled := False;
  ASettings.HostName := '';
  ASettings.ContactEmail := '';
  ASettings.TermsAccepted := False;
  ASettings.DirectoryUrl := '';
  ASettings.RenewBeforeDays := 0;
  ASettings.CheckIntervalMinutes := 0;
  ASettings.AccountUrl := '';

  ASettings.Enabled := ReadRegistryInteger('Enabled',
                                           0) <> 0;
  ASettings.HostName := LowerCase(Trim(ReadRegistryString('HostName',
                                                         '')));
  ASettings.ContactEmail := Trim(ReadRegistryString('ContactEmail',
                                                    ''));
  ASettings.TermsAccepted := ReadRegistryInteger('TermsAccepted',
                                                 0) <> 0;
  ASettings.DirectoryUrl := Trim(ReadRegistryString('DirectoryUrl',
                                                    FXSERVE_ACME_PRODUCTION_DIRECTORY));
  ASettings.RenewBeforeDays := ReadRegistryInteger('RenewBeforeDays',
                                                   30);
  ASettings.CheckIntervalMinutes := ReadRegistryInteger('CheckIntervalMinutes',
                                                        360);
  ASettings.AccountUrl := Trim(ReadRegistryString('AccountUrl',
                                                  ''));

  if (ASettings.RenewBeforeDays < 7) then
    ASettings.RenewBeforeDays := 7;

  if (ASettings.RenewBeforeDays > 60) then
    ASettings.RenewBeforeDays := 60;

  if (ASettings.CheckIntervalMinutes < 15) then
    ASettings.CheckIntervalMinutes := 15;

  if (ASettings.CheckIntervalMinutes > 1440) then
    ASettings.CheckIntervalMinutes := 1440;

  Result := ASettings.Enabled and
            (ASettings.HostName <> '') and
            ASettings.TermsAccepted and
            (ASettings.DirectoryUrl <> '');
end;


procedure ConfigureFxServeCertificate(const AHostName: string;
                                      const AContactEmail: string;
                                      const ATermsAccepted: Boolean;
                                      const AUseStaging: Boolean);
var
  Settings: TFxServeCertificateSettings;

begin

  Settings.Enabled := True;
  Settings.HostName := LowerCase(Trim(AHostName));
  Settings.ContactEmail := Trim(AContactEmail);
  Settings.TermsAccepted := ATermsAccepted;
  Settings.RenewBeforeDays := 30;
  Settings.CheckIntervalMinutes := 360;
  Settings.AccountUrl := '';

  if AUseStaging then
    Settings.DirectoryUrl := FXSERVE_ACME_STAGING_DIRECTORY
  else
    Settings.DirectoryUrl := FXSERVE_ACME_PRODUCTION_DIRECTORY;

  if (Settings.HostName = '') or
     (Pos('.', Settings.HostName) = 0) then
    raise Exception.Create('A fully-qualified certificate host name is required.');

  if not ATermsAccepted then
    raise Exception.Create('The certificate authority terms must be accepted explicitly.');

  WriteCertificateSettings(Settings);
end;


function DerLength(const ALength: Integer): TBytes;
var
  Value: Integer;
  Count: Integer;
  I: Integer;

begin

  if (ALength < 128) then
    Exit(BytesOf([Byte(ALength)]));

  Value := ALength;
  Count := 0;

  while (Value > 0) do
    begin
      Inc(Count);
      Value := Value shr 8;
    end;

  SetLength(Result,
            Count + 1);
  Result[0] := $80 or Count;
  Value := ALength;

  for I := Count downto 1 do
    begin
      Result[I] := Byte(Value);
      Value := Value shr 8;
    end;
end;


function DerWrap(const ATag: Byte;
                 const AContent: TBytes): TBytes;
var
  LengthBytes: TBytes;

begin

  Result := BytesOf([ATag]);
  LengthBytes := DerLength(Length(AContent));
  AppendBytes(Result,
              LengthBytes);
  AppendBytes(Result,
              AContent);
end;


function DerConcat(const AItems: array of TBytes): TBytes;
var
  I: Integer;

begin

  Result := nil;

  for I := 0 to High(AItems) do
    AppendBytes(Result,
                AItems[I]);
end;


function DerInteger(const AValue: TBytes): TBytes;
var
  Value: TBytes;

begin

  Value := Copy(AValue,
                0,
                Length(AValue));

  while (Length(Value) > 1) and
        (Value[0] = 0) and
        ((Value[1] and $80) = 0) do
    Delete(Value,
           0,
           1);

  if (Length(Value) = 0) then
    Value := BytesOf([0])
  else
    if ((Value[0] and $80) <> 0) then
      begin
        SetLength(Result,
                  Length(Value) + 1);
        Result[0] := 0;
        Move(Value[0],
             Result[1],
             Length(Value));
        Value := Result;
      end;

  Result := DerWrap($02,
                    Value);
end;


function DerOidRsaEncryption(): TBytes;
begin
  Result := BytesOf([$06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $01, $01]);
end;


function DerOidSha256WithRsa(): TBytes;
begin
  Result := BytesOf([$06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $01, $0B]);
end;


function DerAlgorithm(const AOid: TBytes): TBytes;
begin
  Result := DerWrap($30,
                    DerConcat([AOid,
                               BytesOf([$05, $00])]));
end;


function DerSubject(const AHostName: string): TBytes;
var
  CommonName: TBytes;

begin

  CommonName := DerConcat([BytesOf([$06, $03, $55, $04, $03]),
                           DerWrap($0C,
                                   Utf8Bytes(AHostName))]);
  Result := DerWrap($30,
                    DerWrap($31,
                            DerWrap($30,
                                    CommonName)));
end;


function DerSubjectPublicKeyInfo(const AKey: TAcmeKey): TBytes;
var
  RsaPublicKey: TBytes;
  BitString: TBytes;

begin

  RsaPublicKey := DerWrap($30,
                          DerConcat([DerInteger(AKey.Modulus),
                                     DerInteger(AKey.Exponent)]));
  BitString := BytesOf([0]);
  AppendBytes(BitString,
              RsaPublicKey);
  Result := DerWrap($30,
                    DerConcat([DerAlgorithm(DerOidRsaEncryption()),
                               DerWrap($03,
                                       BitString)]));
end;


function DerExtensionRequest(const AHostName: string): TBytes;
var
  GeneralName: TBytes;
  GeneralNames: TBytes;
  Extension: TBytes;
  Extensions: TBytes;
  Attribute: TBytes;

begin

  GeneralName := DerWrap($82,
                         Utf8Bytes(AHostName));
  GeneralNames := DerWrap($30,
                          GeneralName);
  Extension := DerWrap($30,
                       DerConcat([BytesOf([$06, $03, $55, $1D, $11]),
                                  DerWrap($04,
                                          GeneralNames)]));
  Extensions := DerWrap($30,
                        Extension);
  Attribute := DerWrap($30,
                       DerConcat([BytesOf([$06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $09, $0E]),
                                  DerWrap($31,
                                          Extensions)]));
  Result := DerWrap($A0,
                    Attribute);
end;


procedure CloseAcmeKey(var AKey: TAcmeKey);
begin

  if (AKey.Key <> 0) then
    CryptDestroyKey(AKey.Key);

  if (AKey.Provider <> 0) then
    CryptReleaseContext(AKey.Provider,
                        0);

  AKey.Provider := 0;
  AKey.Key := 0;
  AKey.KeySpec := 0;
  AKey.ContainerName := '';
  AKey.Modulus := nil;
  AKey.Exponent := nil;
end;


procedure DeleteKeyContainer(const AContainerName: string);
var
  Provider: HCRYPTPROV;

begin

  Provider := 0;
  CryptAcquireContextW(Provider,
                       PWideChar(AContainerName),
                       PWideChar(MICROSOFT_RSA_AES_PROVIDER),
                       PROV_RSA_AES,
                       CRYPT_MACHINE_KEYSET or CRYPT_DELETEKEYSET);
end;


procedure ReadPublicKey(var AKey: TAcmeKey);
var
  Blob: TBytes;
  BlobLength: DWORD;
  Header: TBLOBHEADER;
  PublicKey: TRSAPUBKEY;
  ModulusLittleEndian: TBytes;
  ExponentValue: DWORD;
  ExponentLength: Integer;

begin

  BlobLength := 0;

  if not CryptExportKey(AKey.Key,
                        0,
                        PUBLICKEYBLOB,
                        0,
                        nil,
                        BlobLength) then
    RaiseLastOSError();

  SetLength(Blob,
            BlobLength);

  if not CryptExportKey(AKey.Key,
                        0,
                        PUBLICKEYBLOB,
                        0,
                        @Blob[0],
                        BlobLength) then
    RaiseLastOSError();

  if (BlobLength < SizeOf(Header) + SizeOf(PublicKey)) then
    raise Exception.Create('The Windows RSA public key is invalid.');

  Move(Blob[0],
       Header,
       SizeOf(Header));
  Move(Blob[SizeOf(Header)],
       PublicKey,
       SizeOf(PublicKey));

  if (Header.bType <> PUBLICKEYBLOB) or
     (Header.bVersion <> CUR_BLOB_VERSION) or
     (PublicKey.magic <> RSA1_MAGIC) then
    raise Exception.Create('The Windows key is not an RSA public key.');

  SetLength(ModulusLittleEndian,
            PublicKey.bitlen div 8);
  Move(Blob[SizeOf(Header) + SizeOf(PublicKey)],
       ModulusLittleEndian[0],
       Length(ModulusLittleEndian));
  AKey.Modulus := ReverseBytes(ModulusLittleEndian);

  ExponentValue := PublicKey.pubexp;
  ExponentLength := 4;

  while (ExponentLength > 1) and
        ((ExponentValue shr ((ExponentLength - 1) * 8)) = 0) do
    Dec(ExponentLength);

  SetLength(AKey.Exponent,
            ExponentLength);

  while (ExponentLength > 0) do
    begin
      AKey.Exponent[Length(AKey.Exponent) - ExponentLength] :=
        Byte(ExponentValue shr ((ExponentLength - 1) * 8));
      Dec(ExponentLength);
    end;
end;


procedure OpenOrCreateKey(const AContainerName: string;
                          const AKeySpec: DWORD;
                          const ACreateNew: Boolean;
                          out AKey: TAcmeKey);
var
  ErrorCode: DWORD;

begin

  AKey.Provider := 0;
  AKey.Key := 0;
  AKey.KeySpec := 0;
  AKey.ContainerName := '';
  AKey.Modulus := nil;
  AKey.Exponent := nil;
  AKey.ContainerName := AContainerName;
  AKey.KeySpec := AKeySpec;

  if not CryptAcquireContextW(AKey.Provider,
                              PWideChar(AContainerName),
                              PWideChar(MICROSOFT_RSA_AES_PROVIDER),
                              PROV_RSA_AES,
                              CRYPT_MACHINE_KEYSET) then
    begin
      ErrorCode := GetLastError;

      if (ErrorCode <> Cardinal($80090016)) then
        RaiseLastOSError(ErrorCode);

      if not CryptAcquireContextW(AKey.Provider,
                                  PWideChar(AContainerName),
                                  PWideChar(MICROSOFT_RSA_AES_PROVIDER),
                                  PROV_RSA_AES,
                                  CRYPT_MACHINE_KEYSET or CRYPT_NEWKEYSET) then
        RaiseLastOSError();
    end;

  try
    if ACreateNew then
      begin
        if CryptGetUserKey(AKey.Provider,
                           AKeySpec,
                           AKey.Key) then
          begin
            CryptDestroyKey(AKey.Key);
            AKey.Key := 0;
            raise Exception.Create('The new certificate key container already exists.');
          end;

        if not CryptGenKey(AKey.Provider,
                           AKeySpec,
                           DWORD(2048) shl 16,
                           AKey.Key) then
          RaiseLastOSError();
      end
    else
      if not CryptGetUserKey(AKey.Provider,
                             AKeySpec,
                             AKey.Key) then
        if not CryptGenKey(AKey.Provider,
                           AKeySpec,
                           DWORD(2048) shl 16,
                           AKey.Key) then
          RaiseLastOSError();

    ReadPublicKey(AKey);

  except
    CloseAcmeKey(AKey);
    raise;
  end;
end;


function HashSha256(const AProvider: HCRYPTPROV;
                    const AData: TBytes): TBytes;
const
  HP_HASHVAL = 2;
var
  Hash: HCRYPTHASH;
  HashLength: DWORD;

begin

  Hash := 0;

  if not CryptCreateHash(AProvider,
                         CALG_SHA_256,
                         0,
                         0,
                         Hash) then
    RaiseLastOSError();

  try
    if (Length(AData) > 0) and
       not CryptHashData(Hash,
                         @AData[0],
                         Length(AData),
                         0) then
      RaiseLastOSError();

    HashLength := 32;
    SetLength(Result,
              HashLength);

    if not CryptGetHashParam(Hash,
                             HP_HASHVAL,
                             @Result[0],
                             HashLength,
                             0) then
      RaiseLastOSError();

    SetLength(Result,
              HashLength);

  finally
    CryptDestroyHash(Hash);
  end;
end;


function SignSha256(const AKey: TAcmeKey;
                    const AData: TBytes): TBytes;
var
  Hash: HCRYPTHASH;
  SignatureLength: DWORD;

begin

  Hash := 0;

  if not CryptCreateHash(AKey.Provider,
                         CALG_SHA_256,
                         0,
                         0,
                         Hash) then
    RaiseLastOSError();

  try
    if (Length(AData) > 0) and
       not CryptHashData(Hash,
                         @AData[0],
                         Length(AData),
                         0) then
      RaiseLastOSError();

    SignatureLength := 0;

    if not CryptSignHashW(Hash,
                          AKey.KeySpec,
                          nil,
                          0,
                          nil,
                          SignatureLength) then
      RaiseLastOSError();

    SetLength(Result,
              SignatureLength);

    if not CryptSignHashW(Hash,
                          AKey.KeySpec,
                          nil,
                          0,
                          @Result[0],
                          SignatureLength) then
      RaiseLastOSError();

    SetLength(Result,
              SignatureLength);
    Result := ReverseBytes(Result);

  finally
    CryptDestroyHash(Hash);
  end;
end;


function CreateCertificateRequest(const AHostName: string;
                                  const AKey: TAcmeKey): TBytes;
var
  RequestInfo: TBytes;
  Signature: TBytes;
  SignatureBits: TBytes;

begin

  RequestInfo := DerWrap($30,
                         DerConcat([BytesOf([$02, $01, $00]),
                                    DerSubject(AHostName),
                                    DerSubjectPublicKeyInfo(AKey),
                                    DerExtensionRequest(AHostName)]));
  Signature := SignSha256(AKey,
                          RequestInfo);
  SignatureBits := BytesOf([0]);
  AppendBytes(SignatureBits,
              Signature);
  Result := DerWrap($30,
                    DerConcat([RequestInfo,
                               DerAlgorithm(DerOidSha256WithRsa()),
                               DerWrap($03,
                                       SignatureBits)]));
end;


function AccountJwk(const AKey: TAcmeKey): string;
begin

  Result := '{"e":"' + Base64UrlEncode(AKey.Exponent) +
            '","kty":"RSA","n":"' + Base64UrlEncode(AKey.Modulus) + '"}';
end;


function AccountThumbprint(const AKey: TAcmeKey): string;
begin

  Result := Base64UrlEncode(HashSha256(AKey.Provider,
                                       Utf8Bytes(AccountJwk(AKey))));
end;


procedure ParseHttpsUrl(const AUrl: string;
                        out AHost: string;
                        out APort: INTERNET_PORT;
                        out APath: string);
var
  Value: string;
  P: Integer;
  HostPort: string;

begin

  Value := Trim(AUrl);

  if not SameText(Copy(Value,
                       1,
                       8),
                  'https://') then
    raise Exception.Create('ACME returned a non-HTTPS URL.');

  Delete(Value,
         1,
         8);
  P := Pos('/',
           Value);

  if (P = 0) then
    begin
      HostPort := Value;
      APath := '/';
    end
  else
    begin
      HostPort := Copy(Value,
                       1,
                       P - 1);
      APath := Copy(Value,
                    P,
                    MaxInt);
    end;

  P := LastDelimiter(':',
                     HostPort);

  if (P > 0) and
     (Pos(']', HostPort) = 0) then
    begin
      AHost := Copy(HostPort,
                    1,
                    P - 1);
      APort := StrToInt(Copy(HostPort,
                             P + 1,
                             MaxInt));
    end
  else
    begin
      AHost := HostPort;
      APort := INTERNET_DEFAULT_HTTPS_PORT;
    end;

  if (AHost = '') then
    raise Exception.Create('ACME returned an invalid URL.');
end;


function QueryWinHttpHeader(ARequest: HINTERNET;
                            const AQuery: DWORD;
                            const AName: string = ''): string;
var
  Size: DWORD;
  Index: DWORD;
  Buffer: array of WideChar;
  NamePointer: PWideChar;

begin

  Result := '';
  Size := 0;
  Index := 0;

  if (AName = '') then
    NamePointer := nil
  else
    NamePointer := PWideChar(AName);

  WinHttpQueryHeaders(ARequest,
                      AQuery,
                      NamePointer,
                      nil,
                      Size,
                      Index);

  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or
     (Size = 0) then
    Exit;

  SetLength(Buffer,
            (Size div SizeOf(WideChar)) + 1);
  Index := 0;

  if WinHttpQueryHeaders(ARequest,
                         AQuery,
                         NamePointer,
                         @Buffer[0],
                         Size,
                         Index) then
    Result := PWideChar(@Buffer[0]);
end;


function AcmeHttpRequest(const AMethod: string;
                         const AUrl: string;
                         const ABody: UTF8String;
                         out AResponse: TAcmeHttpResponse): Boolean;
var
  Session: HINTERNET;
  Connection: HINTERNET;
  Request: HINTERNET;
  Host: string;
  Path: string;
  Port: INTERNET_PORT;
  Headers: string;
  StatusCode: DWORD;
  StatusSize: DWORD;
  HeaderIndex: DWORD;
  Buffer: array[0..8191] of Byte;
  BytesRead: DWORD;
  BodyBytes: TBytes;
  OldLength: Integer;
  BodyPointer: Pointer;

begin

  AResponse.StatusCode := 0;
  AResponse.Location := '';
  AResponse.ReplayNonce := '';
  AResponse.Body := '';
  ParseHttpsUrl(AUrl,
                Host,
                Port,
                Path);

  Session := WinHttpOpen('FxServe ACME/1.0',
                         WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY,
                         nil,
                         nil,
                         0);

  if (Session = nil) then
    RaiseLastOSError();

  try
    WinHttpSetTimeouts(Session,
                       10000,
                       10000,
                       30000,
                       30000);

    Connection := WinHttpConnect(Session,
                                 PWideChar(Host),
                                 Port,
                                 0);

    if (Connection = nil) then
      RaiseLastOSError();

    try
      Request := WinHttpOpenRequest(Connection,
                                    PWideChar(AMethod),
                                    PWideChar(Path),
                                    nil,
                                    nil,
                                    nil,
                                    WINHTTP_FLAG_SECURE);

      if (Request = nil) then
        RaiseLastOSError();

      try
        Headers := '';

        if SameText(AMethod,
                    'POST') then
          Headers := 'Content-Type: application/jose+json'#13#10;

        if (Length(ABody) > 0) then
          BodyPointer := @ABody[1]
        else
          BodyPointer := nil;

        if not WinHttpSendRequest(Request,
                                  PWideChar(Headers),
                                  Length(Headers),
                                  BodyPointer,
                                  Length(ABody),
                                  Length(ABody),
                                  0) then
          RaiseLastOSError();

        if not WinHttpReceiveResponse(Request,
                                      nil) then
          RaiseLastOSError();

        StatusCode := 0;
        StatusSize := SizeOf(StatusCode);
        HeaderIndex := 0;

        if not WinHttpQueryHeaders(Request,
                                   WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
                                   nil,
                                   @StatusCode,
                                   StatusSize,
                                   HeaderIndex) then
          RaiseLastOSError();

        AResponse.StatusCode := StatusCode;
        AResponse.Location := QueryWinHttpHeader(Request,
                                                 WINHTTP_QUERY_LOCATION);
        AResponse.ReplayNonce := QueryWinHttpHeader(Request,
                                                    WINHTTP_QUERY_CUSTOM,
                                                    'Replay-Nonce');
        BodyBytes := nil;

        repeat
          BytesRead := 0;

          if not WinHttpReadData(Request,
                                 @Buffer[0],
                                 SizeOf(Buffer),
                                 BytesRead) then
            RaiseLastOSError();

          if (BytesRead > 0) then
            begin
              OldLength := Length(BodyBytes);
              SetLength(BodyBytes,
                        OldLength + Integer(BytesRead));
              Move(Buffer[0],
                   BodyBytes[OldLength],
                   BytesRead);
            end;

        until (BytesRead = 0);

        AResponse.Body := BytesToUtf8(BodyBytes);
        Result := True;

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


procedure LoadAcmeDirectory(const AUrl: string;
                            out ADirectory: TAcmeDirectory);
var
  Response: TAcmeHttpResponse;
  Json: string;

begin

  if not AcmeHttpRequest('GET',
                         AUrl,
                         '',
                         Response) then
    raise Exception.Create('Could not read the ACME directory.');

  if (Response.StatusCode < 200) or
     (Response.StatusCode >= 300) then
    raise Exception.CreateFmt('ACME directory request failed (HTTP %d).',
                              [Response.StatusCode]);

  Json := string(Response.Body);
  ADirectory.NewNonce := JsonString(Json,
                                    'newNonce');
  ADirectory.NewAccount := JsonString(Json,
                                      'newAccount');
  ADirectory.NewOrder := JsonString(Json,
                                    'newOrder');

  if (ADirectory.NewNonce = '') or
     (ADirectory.NewAccount = '') or
     (ADirectory.NewOrder = '') then
    raise Exception.Create('The ACME directory response is incomplete.');
end;


function GetAcmeNonce(const ADirectory: TAcmeDirectory): string;
var
  Response: TAcmeHttpResponse;

begin

  if not AcmeHttpRequest('HEAD',
                         ADirectory.NewNonce,
                         '',
                         Response) then
    raise Exception.Create('Could not request an ACME nonce.');

  Result := Response.ReplayNonce;

  if (Result = '') then
    raise Exception.Create('The ACME server did not return a replay nonce.');
end;


function CreateJws(const AUrl: string;
                   const APayload: string;
                   const ANonce: string;
                   const AAccountUrl: string;
                   const AKey: TAcmeKey): UTF8String;
var
  ProtectedJson: string;
  ProtectedText: string;
  PayloadText: string;
  SigningInput: string;
  Signature: TBytes;

begin

  ProtectedJson := '{"alg":"RS256","nonce":"' + JsonEscape(ANonce) +
                   '","url":"' + JsonEscape(AUrl) + '"';

  if (AAccountUrl = '') then
    ProtectedJson := ProtectedJson + ',"jwk":' + AccountJwk(AKey)
  else
    ProtectedJson := ProtectedJson + ',"kid":"' + JsonEscape(AAccountUrl) + '"';

  ProtectedJson := ProtectedJson + '}';
  ProtectedText := Base64UrlEncode(Utf8Bytes(ProtectedJson));
  PayloadText := Base64UrlEncode(Utf8Bytes(APayload));
  SigningInput := ProtectedText + '.' + PayloadText;
  Signature := SignSha256(AKey,
                          Utf8Bytes(SigningInput));

  Result := UTF8String('{"protected":"' + ProtectedText +
                       '","payload":"' + PayloadText +
                       '","signature":"' + Base64UrlEncode(Signature) + '"}');
end;


procedure AcmePost(const ADirectory: TAcmeDirectory;
                   const AUrl: string;
                   const APayload: string;
                   const AAccountUrl: string;
                   const AKey: TAcmeKey;
                   var ANonce: string;
                   out AResponse: TAcmeHttpResponse);
var
  Attempt: Integer;
  Body: UTF8String;

begin

  for Attempt := 1 to 2 do
    begin
      if (ANonce = '') then
        ANonce := GetAcmeNonce(ADirectory);

      Body := CreateJws(AUrl,
                        APayload,
                        ANonce,
                        AAccountUrl,
                        AKey);

      if not AcmeHttpRequest('POST',
                             AUrl,
                             Body,
                             AResponse) then
        raise Exception.Create('The ACME request failed.');

      ANonce := AResponse.ReplayNonce;

      if (AResponse.StatusCode <> 400) or
         (Pos('badNonce', string(AResponse.Body)) = 0) then
        Exit;

      ANonce := '';
    end;
end;


procedure RequireAcmeStatus(const AResponse: TAcmeHttpResponse;
                            const AMinimum: DWORD;
                            const AMaximum: DWORD;
                            const AOperation: string);
var
  Detail: string;

begin

  if (AResponse.StatusCode >= AMinimum) and
     (AResponse.StatusCode <= AMaximum) then
    Exit;

  Detail := JsonString(string(AResponse.Body),
                       'detail');

  if (Detail = '') then
    Detail := Copy(string(AResponse.Body),
                   1,
                   500);

  raise Exception.CreateFmt('%s failed (HTTP %d): %s',
                            [AOperation, AResponse.StatusCode, Detail]);
end;


function ExtractPemCertificateAt(const APem: string;
                                 var APosition: Integer): TBytes;
const
  BEGIN_CERTIFICATE = '-----BEGIN CERTIFICATE-----';
  END_CERTIFICATE = '-----END CERTIFICATE-----';
var
  StartPosition: Integer;
  EndPosition: Integer;
  Encoded: string;

begin

  Result := nil;
  StartPosition := Pos(BEGIN_CERTIFICATE,
                       Copy(APem,
                            APosition,
                            MaxInt));

  if (StartPosition = 0) then
    Exit;

  Inc(StartPosition,
      APosition - 1);

  Inc(StartPosition,
      Length(BEGIN_CERTIFICATE));
  EndPosition := Pos(END_CERTIFICATE,
                     Copy(APem,
                          StartPosition,
                          MaxInt));

  if (EndPosition = 0) then
    raise Exception.Create('The ACME certificate response is incomplete.');

  Encoded := Copy(APem,
                  StartPosition,
                  EndPosition - 1);
  Result := Base64Decode(Encoded);
  APosition := StartPosition + EndPosition - 1 + Length(END_CERTIFICATE);
end;


procedure InstallChainCertificates(const APem: string;
                                   const APosition: Integer);
var
  Position: Integer;
  CertificateBytes: TBytes;
  Context: PCertContext;
  Store: HCERTSTORE;

begin

  Position := APosition;
  Store := CertOpenStore(PAnsiChar(CERT_STORE_PROV_SYSTEM_W),
                         0,
                         0,
                         CERT_SYSTEM_STORE_LOCAL_MACHINE or CERT_STORE_OPEN_EXISTING_FLAG,
                         PChar('CA'));

  if (Store = nil) then
    RaiseLastOSError();

  try
    repeat
      CertificateBytes := ExtractPemCertificateAt(APem,
                                                  Position);

      if (Length(CertificateBytes) = 0) then
        Break;

      Context := CertCreateCertificateContext(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
                                              @CertificateBytes[0],
                                              Length(CertificateBytes));

      if (Context = nil) then
        RaiseLastOSError();

      try
        if not CertAddCertificateContextToStore(Store,
                                                Context,
                                                CERT_STORE_ADD_REPLACE_EXISTING,
                                                nil) then
          RaiseLastOSError();

      finally
        CertFreeCertificateContext(Context);
      end;
    until False;

  finally
    CertCloseStore(Store,
                   0);
  end;
end;


function InstallCertificate(const APem: string;
                            const AKey: TAcmeKey): string;
var
  CertificateBytes: TBytes;
  Position: Integer;
  Context: PCertContext;
  Store: HCERTSTORE;
  KeyInfo: TCryptKeyProvInfo;
  Hash: TBytes;
  HashLength: DWORD;
  I: Integer;

begin

  Position := 1;
  CertificateBytes := ExtractPemCertificateAt(APem,
                                              Position);

  if (Length(CertificateBytes) = 0) then
    raise Exception.Create('The ACME certificate response does not contain a certificate.');

  Context := CertCreateCertificateContext(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
                                          @CertificateBytes[0],
                                          Length(CertificateBytes));

  if (Context = nil) then
    RaiseLastOSError();

  try
    FillChar(KeyInfo,
             SizeOf(KeyInfo),
             0);
    KeyInfo.pwszContainerName := PWideChar(AKey.ContainerName);
    KeyInfo.pwszProvName := PWideChar(MICROSOFT_RSA_AES_PROVIDER);
    KeyInfo.dwProvType := PROV_RSA_AES;
    KeyInfo.dwFlags := CRYPT_MACHINE_KEYSET;
    KeyInfo.dwKeySpec := AKey.KeySpec;

    if not CertSetCertificateContextProperty(Context,
                                             CERT_KEY_PROV_INFO_PROP_ID,
                                             0,
                                             @KeyInfo) then
      RaiseLastOSError();

    Store := CertOpenStore(PAnsiChar(CERT_STORE_PROV_SYSTEM_W),
                           0,
                           0,
                           CERT_SYSTEM_STORE_LOCAL_MACHINE or CERT_STORE_OPEN_EXISTING_FLAG,
                           PChar('MY'));

    if (Store = nil) then
      RaiseLastOSError();

    try
      if not CertAddCertificateContextToStore(Store,
                                              Context,
                                              CERT_STORE_ADD_REPLACE_EXISTING,
                                              nil) then
        RaiseLastOSError();

    finally
      CertCloseStore(Store,
                     0);
    end;

    HashLength := 0;

    if not CertGetCertificateContextProperty(Context,
                                             CERT_HASH_PROP_ID,
                                             nil,
                                             HashLength) then
      RaiseLastOSError();

    SetLength(Hash,
              HashLength);

    if not CertGetCertificateContextProperty(Context,
                                             CERT_HASH_PROP_ID,
                                             @Hash[0],
                                             HashLength) then
      RaiseLastOSError();

    Result := '';

    for I := 0 to High(Hash) do
      Result := Result + IntToHex(Hash[I],
                                  2);

    InstallChainCertificates(APem,
                             Position);

  finally
    CertFreeCertificateContext(Context);
  end;
end;


function CertificateDnsName(AContext: PCertContext): string;
var
  Length: DWORD;

begin

  Result := '';
  Length := CertGetNameStringW(AContext,
                               CERT_NAME_DNS_TYPE,
                               0,
                               nil,
                               nil,
                               0);

  if (Length <= 1) then
    Exit;

  SetLength(Result,
            Length);

  if (CertGetNameStringW(AContext,
                         CERT_NAME_DNS_TYPE,
                         0,
                         nil,
                         PChar(Result),
                         Length) <> Length) then
    Exit('');

  SetLength(Result,
            Length - 1);
end;


function CertificateHasPrivateKey(AContext: PCertContext): Boolean;
var
  KeyHandle: NativeUInt;
  KeySpec: DWORD;
  MustFree: BOOL;

begin

  KeyHandle := 0;
  KeySpec := 0;
  MustFree := False;
  Result := CryptAcquireCertificatePrivateKey(AContext,
                                              CRYPT_ACQUIRE_SILENT_FLAG,
                                              nil,
                                              KeyHandle,
                                              KeySpec,
                                              MustFree);

  if Result and MustFree then
    if (KeySpec = CERT_NCRYPT_KEY_SPEC) then
      NCryptFreeObject(KeyHandle)
    else
      CryptReleaseContext(KeyHandle,
                          0);
end;


function FileTimeValue(const AFileTime: TFileTime): Int64;
begin

  Result := (Int64(AFileTime.dwHighDateTime) shl 32) or
            AFileTime.dwLowDateTime;
end;


function FindCurrentCertificate(const AHostName: string;
                                out AThumbprint: string;
                                out AExpires: TFileTime): Boolean;
var
  Store: HCERTSTORE;
  Context: PCertContext;
  Info: PCertInfoPrefix;
  NowFileTime: TFileTime;
  BestExpiry: Int64;
  Hash: TBytes;
  HashLength: DWORD;
  I: Integer;

begin

  Result := False;
  AThumbprint := '';
  FillChar(AExpires,
           SizeOf(AExpires),
           0);
  BestExpiry := 0;
  GetSystemTimeAsFileTime(NowFileTime);

  Store := CertOpenStore(PAnsiChar(CERT_STORE_PROV_SYSTEM_W),
                         0,
                         0,
                         CERT_SYSTEM_STORE_LOCAL_MACHINE or CERT_STORE_READONLY_FLAG,
                         PChar('MY'));

  if (Store = nil) then
    RaiseLastOSError();

  try
    Context := nil;

    repeat
      Context := CertEnumCertificatesInStore(Store,
                                             Context);

      if (Context = nil) then
        Break;

      Info := PCertInfoPrefix(Context^.pCertInfo);

      if SameText(CertificateDnsName(Context),
                  AHostName) and
         (FileTimeValue(Info^.NotBefore) <= FileTimeValue(NowFileTime)) and
         (FileTimeValue(Info^.NotAfter) > FileTimeValue(NowFileTime)) and
         (FileTimeValue(Info^.NotAfter) > BestExpiry) and
         CertificateHasPrivateKey(Context) then
        begin
          HashLength := 0;

          if CertGetCertificateContextProperty(Context,
                                               CERT_HASH_PROP_ID,
                                               nil,
                                               HashLength) then
            begin
              SetLength(Hash,
                        HashLength);

              if CertGetCertificateContextProperty(Context,
                                                   CERT_HASH_PROP_ID,
                                                   @Hash[0],
                                                   HashLength) then
                begin
                  AThumbprint := '';

                  for I := 0 to High(Hash) do
                    AThumbprint := AThumbprint + IntToHex(Hash[I],
                                                          2);

                  AExpires := Info^.NotAfter;
                  BestExpiry := FileTimeValue(AExpires);
                  Result := True;
                end;
            end;
        end;

    until False;

  finally
    CertCloseStore(Store,
                   0);
  end;
end;


function ThumbprintBytes(const AThumbprint: string): TBytes;
var
  I: Integer;

begin

  if (Length(AThumbprint) mod 2 <> 0) then
    raise Exception.Create('Invalid certificate thumbprint.');

  SetLength(Result,
            Length(AThumbprint) div 2);

  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(AThumbprint,
                                     (I * 2) + 1,
                                     2));
end;


procedure InitializeSniKey(var AKey: THttpServiceConfigSslSniKey;
                           const AHostName: string;
                           const APort: Word);
var
  Address: PSockAddrIn;

begin

  FillChar(AKey,
           SizeOf(AKey),
           0);
  Address := PSockAddrIn(@AKey.IpPort);
  Address^.sin_family := AF_INET;
  Address^.sin_port := htons(APort);
  Address^.sin_addr.S_addr := 0;
  AKey.Host := PWideChar(AHostName);
end;


procedure BindHttpSysCertificate(const AHostName: string;
                                 const APort: Word;
                                 const AThumbprint: string);
var
  Version: UInt64;
  Query: THttpServiceConfigSslSniQuery;
  ExistingBuffer: TBytes;
  Existing: PHttpServiceConfigSslSniSet;
  ExistingLength: DWORD;
  Error: DWORD;
  NewBinding: THttpServiceConfigSslSniSet;
  Hash: TBytes;
  StoreName: string;
  ExistingMatches: Boolean;

begin

  Version := 1;
  Error := HttpInitialize(Version,
                          HTTP_INITIALIZE_CONFIG,
                          nil);

  if (Error <> ERROR_SUCCESS) then
    raise Exception.CreateFmt('HTTP.sys configuration initialization failed (%d).',
                              [Error]);

  try
    FillChar(Query,
             SizeOf(Query),
             0);
    Query.QueryDesc := HTTP_SERVICE_CONFIG_QUERY_EXACT;
    InitializeSniKey(Query.KeyDesc,
                     AHostName,
                     APort);
    ExistingLength := 0;
    Error := HttpQueryServiceConfiguration(0,
                                           HTTP_SERVICE_CONFIG_SSL_SNI_CERT_INFO,
                                           @Query,
                                           SizeOf(Query),
                                           nil,
                                           0,
                                           ExistingLength,
                                           nil);
    Existing := nil;

    if (Error = ERROR_INSUFFICIENT_BUFFER) and
       (ExistingLength > 0) then
      begin
        SetLength(ExistingBuffer,
                  ExistingLength);
        Error := HttpQueryServiceConfiguration(0,
                                               HTTP_SERVICE_CONFIG_SSL_SNI_CERT_INFO,
                                               @Query,
                                               SizeOf(Query),
                                               @ExistingBuffer[0],
                                               ExistingLength,
                                               ExistingLength,
                                               nil);

        if (Error = ERROR_SUCCESS) then
          Existing := PHttpServiceConfigSslSniSet(@ExistingBuffer[0]);
      end;

    Hash := ThumbprintBytes(AThumbprint);
    ExistingMatches := Assigned(Existing) and
                       (Existing^.ParamDesc.SslHashLength = DWORD(Length(Hash))) and
                       CompareMem(Existing^.ParamDesc.pSslHash,
                                  @Hash[0],
                                  Length(Hash));

    if ExistingMatches then
      Exit;

    FillChar(NewBinding,
             SizeOf(NewBinding),
             0);
    InitializeSniKey(NewBinding.KeyDesc,
                     AHostName,
                     APort);
    NewBinding.ParamDesc.SslHashLength := Length(Hash);
    NewBinding.ParamDesc.pSslHash := @Hash[0];
    NewBinding.ParamDesc.AppId := FXSERVE_HTTP_SYS_APP_ID;
    StoreName := 'MY';
    NewBinding.ParamDesc.pSslCertStoreName := PWideChar(StoreName);

    if Assigned(Existing) then
      begin
        Error := HttpDeleteServiceConfiguration(0,
                                                HTTP_SERVICE_CONFIG_SSL_SNI_CERT_INFO,
                                                @Query.KeyDesc,
                                                SizeOf(Query.KeyDesc),
                                                nil);

        if (Error <> ERROR_SUCCESS) and
           (Error <> ERROR_FILE_NOT_FOUND) then
          raise Exception.CreateFmt('Could not replace the HTTP.sys certificate binding (%d).',
                                    [Error]);
      end;

    Error := HttpSetServiceConfiguration(0,
                                         HTTP_SERVICE_CONFIG_SSL_SNI_CERT_INFO,
                                         @NewBinding,
                                         SizeOf(NewBinding),
                                         nil);

    if (Error <> ERROR_SUCCESS) then
      begin
        if Assigned(Existing) then
          HttpSetServiceConfiguration(0,
                                      HTTP_SERVICE_CONFIG_SSL_SNI_CERT_INFO,
                                      Existing,
                                      SizeOf(Existing^),
                                      nil);

        raise Exception.CreateFmt('Could not set the HTTP.sys certificate binding (%d).',
                                  [Error]);
      end;

  finally
    HttpTerminate(HTTP_INITIALIZE_CONFIG,
                  nil);
  end;
end;


function FindHttpChallenge(const AJson: string;
                           out AUrl: string;
                           out AToken: string): Boolean;
var
  P: Integer;
  EndPosition: Integer;

begin

  Result := False;
  AUrl := '';
  AToken := '';
  P := Pos('"http-01"',
           AJson);

  if (P = 0) then
    Exit;

  if not JsonStringAt(AJson,
                      'url',
                      P,
                      AUrl,
                      EndPosition) then
    Exit;

  if not JsonStringAt(AJson,
                      'token',
                      P,
                      AToken,
                      EndPosition) then
    Exit;

  Result := (AUrl <> '') and
            (AToken <> '');
end;


function WaitForAcmeObject(const ADirectory: TAcmeDirectory;
                           const AUrl: string;
                           const AAccountUrl: string;
                           const AKey: TAcmeKey;
                           var ANonce: string;
                           const AExpectedStatus: string;
                           const AStopEvent: TEvent;
                           out AJson: string): Boolean;
var
  Response: TAcmeHttpResponse;
  Status: string;
  Attempt: Integer;

begin

  Result := False;
  AJson := '';

  for Attempt := 1 to 60 do
    begin
      if (AStopEvent.WaitFor(2000) = wrSignaled) then
        Exit;

      AcmePost(ADirectory,
               AUrl,
               '',
               AAccountUrl,
               AKey,
               ANonce,
               Response);
      RequireAcmeStatus(Response,
                        200,
                        200,
                        'ACME status query');
      AJson := string(Response.Body);
      Status := JsonString(AJson,
                           'status');

      if SameText(Status,
                  AExpectedStatus) then
        Exit(True);

      if SameText(Status,
                  'invalid') then
        raise Exception.Create('The certificate authority rejected the ACME request: ' +
                               JsonString(AJson,
                                          'detail'));
    end;

  raise Exception.Create('Timed out waiting for the certificate authority.');
end;


function EnsureAcmeAccount(const ASettings: TFxServeCertificateSettings;
                           const ADirectory: TAcmeDirectory;
                           const AKey: TAcmeKey;
                           var ANonce: string): string;
var
  Payload: string;
  Response: TAcmeHttpResponse;

begin

  Result := ASettings.AccountUrl;

  if (Result <> '') then
    begin
      AcmePost(ADirectory,
               Result,
               '',
               Result,
               AKey,
               ANonce,
               Response);

      if (Response.StatusCode = 200) then
        Exit;

      Result := '';
    end;

  Payload := '{"termsOfServiceAgreed":true';

  if (ASettings.ContactEmail <> '') then
    Payload := Payload + ',"contact":["mailto:' +
               JsonEscape(ASettings.ContactEmail) + '"]';

  Payload := Payload + '}';
  AcmePost(ADirectory,
           ADirectory.NewAccount,
           Payload,
           '',
           AKey,
           ANonce,
           Response);
  RequireAcmeStatus(Response,
                    200,
                    201,
                    'ACME account creation');
  Result := Response.Location;

  if (Result = '') then
    raise Exception.Create('The certificate authority did not return an account URL.');

  WriteAccountUrl(Result);
end;


function ObtainCertificate(const ASettings: TFxServeCertificateSettings;
                           const AManager: TFxServeCertificateManager): string;
var
  Directory: TAcmeDirectory;
  AccountKey: TAcmeKey;
  CertificateKey: TAcmeKey;
  AccountUrl: string;
  Nonce: string;
  Response: TAcmeHttpResponse;
  OrderUrl: string;
  FinalizeUrl: string;
  CertificateUrl: string;
  AuthorizationUrls: TStringList;
  AuthorizationJson: string;
  ChallengeUrl: string;
  ChallengeToken: string;
  KeyAuthorization: string;
  Payload: string;
  Status: string;
  Request: TBytes;
  I: Integer;
  ContainerName: string;
  CertificateInstalled: Boolean;

begin

  Result := '';
  Nonce := '';
  AccountKey.Provider := 0;
  AccountKey.Key := 0;
  AccountKey.KeySpec := 0;
  AccountKey.ContainerName := '';
  AccountKey.Modulus := nil;
  AccountKey.Exponent := nil;
  CertificateKey.Provider := 0;
  CertificateKey.Key := 0;
  CertificateKey.KeySpec := 0;
  CertificateKey.ContainerName := '';
  CertificateKey.Modulus := nil;
  CertificateKey.Exponent := nil;
  CertificateInstalled := False;
  LoadAcmeDirectory(ASettings.DirectoryUrl,
                    Directory);
  OpenOrCreateKey(FXSERVE_ACME_ACCOUNT_CONTAINER,
                  AT_SIGNATURE,
                  False,
                  AccountKey);

  try
    AccountUrl := EnsureAcmeAccount(ASettings,
                                    Directory,
                                    AccountKey,
                                    Nonce);
    Payload := '{"identifiers":[{"type":"dns","value":"' +
               JsonEscape(ASettings.HostName) + '"}]}';
    AcmePost(Directory,
             Directory.NewOrder,
             Payload,
             AccountUrl,
             AccountKey,
             Nonce,
             Response);
    RequireAcmeStatus(Response,
                      201,
                      201,
                      'ACME certificate order');
    OrderUrl := Response.Location;
    FinalizeUrl := JsonString(string(Response.Body),
                              'finalize');
    AuthorizationUrls := JsonStringArray(string(Response.Body),
                                         'authorizations');

    try
      if (OrderUrl = '') or
         (FinalizeUrl = '') or
         (AuthorizationUrls.Count = 0) then
        raise Exception.Create('The ACME order response is incomplete.');

      for I := 0 to AuthorizationUrls.Count - 1 do
        begin
          AcmePost(Directory,
                   AuthorizationUrls[I],
                   '',
                   AccountUrl,
                   AccountKey,
                   Nonce,
                   Response);
          RequireAcmeStatus(Response,
                            200,
                            200,
                            'ACME authorization request');
          AuthorizationJson := string(Response.Body);
          Status := JsonString(AuthorizationJson,
                               'status');

          if SameText(Status,
                      'valid') then
            Continue;

          if not FindHttpChallenge(AuthorizationJson,
                                   ChallengeUrl,
                                   ChallengeToken) then
            raise Exception.Create('The certificate authority did not offer an HTTP-01 challenge.');

          KeyAuthorization := ChallengeToken + '.' + AccountThumbprint(AccountKey);
          AManager.SetChallenge(ChallengeToken,
                                KeyAuthorization);

          try
            AcmePost(Directory,
                     ChallengeUrl,
                     '{}',
                     AccountUrl,
                     AccountKey,
                     Nonce,
                     Response);
            RequireAcmeStatus(Response,
                              200,
                              200,
                              'ACME challenge acknowledgement');

            if not WaitForAcmeObject(Directory,
                                     AuthorizationUrls[I],
                                     AccountUrl,
                                     AccountKey,
                                     Nonce,
                                     'valid',
                                     AManager.FStopEvent,
                                     AuthorizationJson) then
              raise Exception.Create('Certificate renewal was stopped.');

          finally
            AManager.ClearChallenge();
          end;
        end;

    finally
      AuthorizationUrls.Free;
    end;

    ContainerName := FXSERVE_CERTIFICATE_CONTAINER_PREFIX +
                     FormatDateTime('yyyymmddhhnnsszzz',
                                    Now);
    OpenOrCreateKey(ContainerName,
                    AT_KEYEXCHANGE,
                    True,
                    CertificateKey);

    try
      Request := CreateCertificateRequest(ASettings.HostName,
                                          CertificateKey);
      Payload := '{"csr":"' + Base64UrlEncode(Request) + '"}';
      AcmePost(Directory,
               FinalizeUrl,
               Payload,
               AccountUrl,
               AccountKey,
               Nonce,
               Response);
      RequireAcmeStatus(Response,
                        200,
                        200,
                        'ACME order finalization');
      AuthorizationJson := string(Response.Body);
      Status := JsonString(AuthorizationJson,
                           'status');

      if not SameText(Status,
                      'valid') then
        if not WaitForAcmeObject(Directory,
                                 OrderUrl,
                                 AccountUrl,
                                 AccountKey,
                                 Nonce,
                                 'valid',
                                 AManager.FStopEvent,
                                 AuthorizationJson) then
          raise Exception.Create('Certificate renewal was stopped.');

      CertificateUrl := JsonString(AuthorizationJson,
                                   'certificate');

      if (CertificateUrl = '') then
        CertificateUrl := JsonString(string(Response.Body),
                                     'certificate');

      if (CertificateUrl = '') then
        raise Exception.Create('The certificate authority did not return a certificate URL.');

      AcmePost(Directory,
               CertificateUrl,
               '',
               AccountUrl,
               AccountKey,
               Nonce,
               Response);
      RequireAcmeStatus(Response,
                        200,
                        200,
                        'ACME certificate download');
      Result := InstallCertificate(string(Response.Body),
                                   CertificateKey);
      CertificateInstalled := True;

    finally
      CloseAcmeKey(CertificateKey);

      if not CertificateInstalled and
         (ContainerName <> '') then
        DeleteKeyContainer(ContainerName);
    end;

  finally
    CloseAcmeKey(AccountKey);
  end;
end;


constructor TFxServeCertificateThread.Create(AManager: TFxServeCertificateManager);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FManager := AManager;
end;


procedure TFxServeCertificateThread.Execute();
begin

  FManager.Execute();
end;


constructor TFxServeCertificateManager.Create(ALogger: TFxServeLogger;
                                               const AHostName: string;
                                               const AHttpsPort: Word;
                                               const AHttpChallengeAvailable: Boolean);
begin

  inherited Create();

  FLogger := ALogger;
  FHostName := LowerCase(Trim(AHostName));
  FHttpsPort := AHttpsPort;
  FHttpChallengeAvailable := AHttpChallengeAvailable;
  FLock := TCriticalSection.Create();
  FStopEvent := TEvent.Create(nil,
                              True,
                              False,
                              '');
end;


destructor TFxServeCertificateManager.Destroy();
begin

  Stop();
  FStopEvent.Free;
  FLock.Free;

  inherited Destroy();
end;


procedure TFxServeCertificateManager.Start();
begin

  if Assigned(FThread) then
    Exit;

  FStopEvent.ResetEvent();
  FThread := TFxServeCertificateThread.Create(Self);
  FThread.Start();
end;


procedure TFxServeCertificateManager.Stop();
var
  Thread: TFxServeCertificateThread;

begin

  Thread := FThread;

  if not Assigned(Thread) then
    Exit;

  FThread := nil;
  FStopEvent.SetEvent();
  WaitForSingleObject(Thread.Handle,
                      INFINITE);
  Thread.Free;
  ClearChallenge();
end;


procedure TFxServeCertificateManager.SetChallenge(const AToken: string;
                                                  const AResponse: string);
begin

  FLock.Acquire;

  try
    FChallengeToken := AToken;
    FChallengeResponse := AResponse;

  finally
    FLock.Release;
  end;

  FLogger.Info('ACME HTTP-01 challenge is ready.');
end;


procedure TFxServeCertificateManager.ClearChallenge();
begin

  FLock.Acquire;

  try
    FChallengeToken := '';
    FChallengeResponse := '';

  finally
    FLock.Release;
  end;
end;


function TFxServeCertificateManager.TryGetAcmeChallenge(const ATarget: string;
                                                        out AResponse: string): Boolean;
const
  PREFIX = '/.well-known/acme-challenge/';
var
  Token: string;

begin

  Result := False;
  AResponse := '';

  if (Pos(PREFIX,
          LowerCase(ATarget)) <> 1) then
    Exit;

  Token := Copy(ATarget,
                Length(PREFIX) + 1,
                MaxInt);

  if (Pos('?', Token) > 0) then
    Token := Copy(Token,
                  1,
                  Pos('?', Token) - 1);

  FLock.Acquire;

  try
    Result := (FChallengeToken <> '') and
              (Token = FChallengeToken);

    if Result then
      AResponse := FChallengeResponse;

  finally
    FLock.Release;
  end;
end;


procedure TFxServeCertificateManager.EnsureCertificate();
var
  Settings: TFxServeCertificateSettings;
  Thumbprint: string;
  ResultThumbprint: string;
  Expires: TFileTime;
  NowFileTime: TFileTime;
  RenewAt: Int64;

begin

  if not LoadFxServeCertificateSettings(Settings) then
    begin
      if CertificateSettingsExist() then
        begin
          if not Settings.Enabled then
            Exit;

          raise Exception.Create('The protected FxServe certificate settings are incomplete.');
        end;

      ConfigureFxServeCertificate(FHostName,
                                  '',
                                  True,
                                  False);

      if not LoadFxServeCertificateSettings(Settings) then
        Exit;

      FLogger.Info('FxServe initialized protected internal certificate management for ' +
                   FHostName + '.');
    end;

  if not SameText(Settings.HostName,
                  FHostName) then
    raise Exception.CreateFmt('The protected certificate host name (%s) does not match the FxServe WAN host name (%s).',
                              [Settings.HostName, FHostName]);

  if FindCurrentCertificate(Settings.HostName,
                            Thumbprint,
                            Expires) then
    begin
      BindHttpSysCertificate(Settings.HostName,
                              FHttpsPort,
                              Thumbprint);
      GetSystemTimeAsFileTime(NowFileTime);
      RenewAt := FileTimeValue(Expires) -
                 (Int64(Settings.RenewBeforeDays) * 24 * 60 * 60 * 10000000);

      if (FileTimeValue(NowFileTime) < RenewAt) then
        Exit;

      FLogger.Info('The FxServe HTTPS certificate is inside its renewal window.');
    end
  else
    FLogger.Info('FxServe does not have a valid HTTPS certificate; requesting one.');

  if not FHttpChallengeAvailable then
    raise Exception.Create('FxServe cannot obtain or renew its HTTPS certificate because its WAN HTTP listener is disabled.');

  Thumbprint := ObtainCertificate(Settings,
                                  Self);

  if not FindCurrentCertificate(Settings.HostName,
                                ResultThumbprint,
                                Expires) or
     not SameText(ResultThumbprint,
                  Thumbprint) then
    raise Exception.Create('The new FxServe certificate could not be verified in the Local Machine certificate store.');

  BindHttpSysCertificate(Settings.HostName,
                         FHttpsPort,
                         Thumbprint);
  FLogger.Info('FxServe obtained and bound a new HTTPS certificate for ' +
               Settings.HostName + '.');
end;


procedure TFxServeCertificateManager.Execute();
var
  Settings: TFxServeCertificateSettings;
  WaitMinutes: Integer;

begin

  while (FStopEvent.WaitFor(0) <> wrSignaled) do
    begin
      WaitMinutes := 60;

      try
        if LoadFxServeCertificateSettings(Settings) then
          begin
            WaitMinutes := Settings.CheckIntervalMinutes;
            EnsureCertificate();
          end;

      except
        on E: Exception do
          begin
            FLogger.Error('Certificate manager: ' + E.Message);
            WaitMinutes := 60;
          end;
      end;

      if (FStopEvent.WaitFor(WaitMinutes * 60 * 1000) = wrSignaled) then
        Break;
    end;
end;

procedure DisableFxServeCertificate();
var
  Settings: TFxServeCertificateSettings;

begin

  Settings.Enabled := False;
  Settings.HostName := '';
  Settings.ContactEmail := '';
  Settings.TermsAccepted := False;
  Settings.DirectoryUrl := FXSERVE_ACME_PRODUCTION_DIRECTORY;
  Settings.RenewBeforeDays := 30;
  Settings.CheckIntervalMinutes := 360;
  Settings.AccountUrl := '';
  WriteCertificateSettings(Settings);
end;

end.

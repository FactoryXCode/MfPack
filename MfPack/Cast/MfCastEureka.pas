// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastEureka.pas
// Kind: Pascal Unit
// Release date: 22-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Eureka device-name queries and verified receiver renaming.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: -
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

unit MfCastEureka;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  {Cast}
  MfCastTypes;

type
  // The setup service is independent of the Cast V2 control port (8009).
  TMfCastEurekaOptions = record
    UseHttps: Boolean;
    Port: Word;
    TimeoutMs: Integer;
    VerificationTimeoutMs: Integer;
    LocalAuthorizationToken: string;
    // Explicit receiver-local opt-in: ignore unknown CA / host-name mismatch.
    // Expired certificates are still rejected.
    AllowSelfSignedCertificate: Boolean;
    // Some Cast receivers (e.g. JBL Link) reuse a client-authentication
    // device certificate for their local HTTPS setup endpoint. Explicit opt-in.
    AllowDeviceCertificateUsage: Boolean;
    class function CreateDefault: TMfCastEurekaOptions; static;
  end;

  TMfCastEurekaInfo = record
    Name: string;
    DeviceId: string;
    ModelName: string;
    HttpStatus: Cardinal;
  end;

  TMfCastEurekaRenameResult = record
    Info: TMfCastEurekaInfo;
    WriteAttempted: Boolean;
    WriteAccepted: Boolean;
    Verified: Boolean;
  end;

// Blocking operations. Use a worker in GUI applications. No Google account
// credentials are collected or persisted. Tokens are supplied by the caller.
function MfCastEurekaGetInfo(const ADevice: TMfCastDevice;
                             const AOptions: TMfCastEurekaOptions;
                             out AInfo: TMfCastEurekaInfo;
                             out ADetail: string): HRESULT;
function MfCastEurekaRenameDevice(const ADevice: TMfCastDevice;
                                  const AName: string;
                                  const AOptions: TMfCastEurekaOptions;
                                  out AResult: TMfCastEurekaRenameResult;
                                  out ADetail: string): HRESULT;

implementation

uses
  System.Classes,
  System.JSON,
  Winapi.WinError;

const
  WinHttpLib = 'winhttp.dll';
  MaxResponseBytes = 262144;

  function WHOpen(Agent: PWideChar;
                  Access: DWORD;
                  Proxy: PWideChar;
                  Bypass: PWideChar;
                  Flags: DWORD): Pointer; stdcall; external WinHttpLib name 'WinHttpOpen';
                
  function WHConnect(Session: Pointer;
                     Host: PWideChar;
                     Port: Word;
                     Reserved: DWORD): Pointer; stdcall; external WinHttpLib name 'WinHttpConnect';
                   
  function WHRequest(Connection: Pointer;
                     Verb: PWideChar;
                     Path: PWideChar;
                     Version: PWideChar;
                     Referrer: PWideChar;
                     Accept: Pointer;
                     Flags: DWORD): Pointer; stdcall; external WinHttpLib name 'WinHttpOpenRequest';
                   
  function WHTimeouts(Handle: Pointer;
                      Resolve: Integer;
                      Connect: Integer;
                      Send: Integer;
                      Receive: Integer): BOOL; stdcall; external WinHttpLib name 'WinHttpSetTimeouts';
                    
  function WHOption(Handle: Pointer;
                    Option: DWORD;
                    Buffer: Pointer;
                    Size: DWORD): BOOL; stdcall; external WinHttpLib name 'WinHttpSetOption'; 
                  
  function WHSend(Handle: Pointer;
                  Headers: PWideChar;
                  HeaderLength: DWORD;
                  Data: Pointer;
                  DataLength, Total: DWORD;
                  Context: NativeUInt): BOOL; stdcall; external WinHttpLib name 'WinHttpSendRequest';
                
  function WHReceive(Handle: Pointer;
                     Reserved: Pointer): BOOL; stdcall; external WinHttpLib name 'WinHttpReceiveResponse';

  function WHHeaders(Handle: Pointer;
                     Level: DWORD;
                     Name: PWideChar;
                     Buffer: Pointer;
                     var Size: DWORD;
                     Index: Pointer): BOOL; stdcall; external WinHttpLib name 'WinHttpQueryHeaders';
                   
  function WHRead(Handle, Buffer: Pointer;
                  Size: DWORD;
                  out Read: DWORD): BOOL; stdcall; external WinHttpLib name 'WinHttpReadData';
                
  function WHClose(Handle: Pointer): BOOL; stdcall; external WinHttpLib name 'WinHttpCloseHandle';

  function Tick64(): UInt64; stdcall; external 'kernel32.dll' name 'GetTickCount64';


class function TMfCastEurekaOptions.CreateDefault: TMfCastEurekaOptions;

begin

  Result := Default(TMfCastEurekaOptions);
  Result.UseHttps := True;
  Result.Port := 8443;
  Result.TimeoutMs := 5000;
  Result.VerificationTimeoutMs := 5000;
end;


function NormalizeId(const S: string): string;

begin

  Result := LowerCase(Trim(S));
  
  if (Copy(Result,
           1,
           5) = 'uuid:') then
    Delete(Result,
           1,
           5);

  Result := StringReplace(Result,
                          '-',
                          '',
                          [rfReplaceAll]);

  Result := StringReplace(Result,
                          '{',
                          '',
                          [rfReplaceAll]);

  Result := StringReplace(Result,
                          '}',
                          '',
                          [rfReplaceAll]);
end;


function ValidOptions(const D: TMfCastDevice;
                      const O: TMfCastEurekaOptions;
                      out Host, Detail: string): Boolean;
var
  I: Integer;

begin

  Result := False;
  Detail := 'Invalid Eureka endpoint or options.';
  Host := Trim(D.Address);
  
  if (Host = '') then
    Host := Trim(D.HostName);
  
  if (Host = '') or (O.Port = 0) or (O.TimeoutMs < 1) or (O.TimeoutMs > 30000) or
    (O.VerificationTimeoutMs < 0) or (O.VerificationTimeoutMs > 30000) then
    Exit;
  
  if (Host[1] = '[') and (Host[Length(Host)] = ']') then
    Host := Copy(Host,
                 2,
                 Length(Host) - 2);
  
  if (Host = '') then
    Exit;
  
  for I := 1 to Length(Host) do
    if (Ord(Host[I]) <= 32) or CharInSet(Host[I],
                                         ['/', '\', '?', '#', '@', '[', ']']) then
      Exit;
  if (Length(O.LocalAuthorizationToken) > 4096) then
    Exit;
  
  for I := 1 to Length(O.LocalAuthorizationToken) do
    if (Ord(O.LocalAuthorizationToken[I]) < 33) or (Ord(O.LocalAuthorizationToken[I]) > 126) then
      Exit;
  
  if (O.LocalAuthorizationToken <> '') and not O.UseHttps then
    begin
      Detail := 'A local authorization token requires HTTPS.';
      Exit;
    end;
  
  Result := True;
  Detail := '';
end;


function NativeFailure(out Detail: string): HRESULT;
var
  Code: DWORD;

begin

  Code := GetLastError;
  
  if (Code = 0) then
    Code := ERROR_GEN_FAILURE;
  
  Result := HRESULT_FROM_WIN32(Code);
  
  // Never include request headers, tokens or receiver-supplied response bodies.
  if (Code = 12175) then
    Detail := 'Eureka TLS certificate validation failed (Windows error 12175). Check the receiver certificate trust settings.'
  else
    Detail := Format('Eureka transport failed (Windows error %d).',
                     [Code]);
end;


function Http(const D: TMfCastDevice;
              const O: TMfCastEurekaOptions;
              const Verb, Path, Body: string;
              out Status: Cardinal;
              out Reply, Detail: string;
              out Attempted: Boolean): HRESULT;
var
  Host: string;
  Headers: string;
  S: Pointer;
  C: Pointer;
  R: Pointer;
  Data: Pointer;
  Flags: DWORD;
  Size: DWORD;
  N: DWORD;
  Bytes: TBytes;
  Buffer: array[0..8191] of Byte;
  Stream: TMemoryStream;
  Started: UInt64;
  Elapsed: UInt64;

begin

  Status := 0;
  Reply := '';
  Detail := '';
  Attempted := False;
  S := nil;
  C := nil;
  R := nil;
  
  if not ValidOptions(D,
                      O,
                      Host,
                      Detail) then
    Exit(E_INVALIDARG);
  
  Stream := TMemoryStream.Create();
 
  try
    try
      S := WHOpen('MfPack-Eureka/4.1.0',
                  1,
                  nil,
                  nil,
                  0); // direct, no system proxy
      
      if (S = nil) then
        Exit(NativeFailure(Detail));
      
      if not WHTimeouts(S,
                        O.TimeoutMs,
                        O.TimeoutMs,
                        O.TimeoutMs,
                        O.TimeoutMs) then
        Exit(NativeFailure(Detail));
      
      if O.UseHttps then
        begin
          Flags := $800;
          if not WHOption(S,
                          84,
                          @Flags,
                          SizeOf(Flags)) then
            Exit(NativeFailure(Detail));
        end;
      
      C := WHConnect(S,
                     PWideChar(Host),
                     O.Port,
                     0);
      
      if (C = nil) then
        Exit(NativeFailure(Detail));
      
      Flags := 0;
      
      if O.UseHttps then
        Flags := $00800000;
      
      R := WHRequest(C,
                     PWideChar(Verb),
                     PWideChar(Path),
                     nil,
                     nil,
                     nil,
                     Flags);
     
      if (R = nil) then
        Exit(NativeFailure(Detail));
      
      Flags := $1 or $2 or $4; // no cookies, redirects, or automatic Windows authentication
      
      if not WHOption(R,
                      63,
                      @Flags,
                      SizeOf(Flags)) then
        Exit(NativeFailure(Detail));
      
      if O.UseHttps then
        begin
          Flags := 0;
          
          if O.AllowSelfSignedCertificate then
            Flags := Flags or $100 or $1000;
          
          if O.AllowDeviceCertificateUsage then
            Flags := Flags or $200;
          
          if (Flags <> 0) and not WHOption(R,
                                           31,
                                           @Flags,
                                           SizeOf(Flags)) then
            Exit(NativeFailure(Detail));
        end;
        
      // Cast setup servers may reject media-type parameters. The body is UTF-8.
      Headers := 'Content-Type: application/json'#13#10'Cache-Control: no-cache'#13#10;
      
      if (O.LocalAuthorizationToken <> '') then
        Headers := Headers + 'cast-local-authorization-token: ' + O.LocalAuthorizationToken + #13#10;
      
      Bytes := TEncoding.UTF8.GetBytes(Body);
      Data := nil;
      
      if (Length(Bytes) > 0) then
        Data := @Bytes[0];
      
      Started := Tick64;
      Attempted := True;
      
      if not WHSend(R,
                    PWideChar(Headers),
                    Length(Headers),
                    Data,
                    Length(Bytes),
                    Length(Bytes),
                    0) then
        Exit(NativeFailure(Detail));
      
      if not WHReceive(R,
                       nil) then
        Exit(NativeFailure(Detail));
      
      Size := SizeOf(Status);
      
      if not WHHeaders(R,
                       19 or $20000000,
                       nil,
                       @Status,
                       Size,
                       nil) then
        Exit(NativeFailure(Detail));
      
      case Status of
          401, 403: begin
            Detail := 'Eureka authorization denied; supply a valid receiver local authorization token.';
            Exit(E_ACCESSDENIED);
          end;
          
          404, 405, 501: begin
                       Detail := 'This receiver does not support the requested Eureka setup operation.';
            Exit(E_NOTIMPL);
          end;
      end;
      
      if (Status < 200) or (Status >= 300) then
        begin
          if (Status >= 300) and (Status < 400) then
            Detail := Format('Eureka %s %s returned HTTP %d; redirects are not followed.',
                             [Verb, Path, Status])
          else
            Detail := Format('Eureka rejected %s %s (HTTP %d).',
                             [Verb, Path, Status]);
          Exit(E_FAIL);
        end;
        
      repeat
        Elapsed := Tick64 - Started;
        
        if (Elapsed >= UInt64(O.TimeoutMs)) then
          begin
            Detail := 'Eureka response timed out.';
            Exit(HRESULT_FROM_WIN32(ERROR_TIMEOUT));
          end;
        
        Flags := DWORD(UInt64(O.TimeoutMs) - Elapsed);
        
        if not WHOption(R,
                        6,
                        @Flags,
                        SizeOf(Flags)) then
          Exit(NativeFailure(Detail));
        
        if not WHRead(R,
                      @Buffer[0],
                      SizeOf(Buffer),
                      N) then
          Exit(NativeFailure(Detail));
        
        if (Stream.Size + N > MaxResponseBytes) then
          begin
            Detail := 'Eureka response exceeds the size limit.';
            Exit(HRESULT_FROM_WIN32(ERROR_INVALID_DATA));
          end;
        
        if (N <> 0) then
          Stream.WriteBuffer(Buffer[0],
                             N);
      until (N = 0);
      SetLength(Bytes,
                Stream.Size);
      
      if (Length(Bytes) > 0) then
        Move(Stream.Memory^,
             Bytes[0],
             Length(Bytes));
      
      Reply := TEncoding.UTF8.GetString(Bytes);
      Result := S_OK;
    
    except
      on E: EOutOfMemory do
        begin
          Result := E_OUTOFMEMORY;
          Detail := 'Not enough memory for Eureka request.';
        end;
        
      on E: Exception do
        begin
          Result := E_FAIL;
          Detail := 'Eureka request failed locally.';
        end;
    end;
    
  finally
    if (R <> nil) then
      WHClose(R);
      
    if (C <> nil) then
      WHClose(C);
      
    if (S <> nil) then
      WHClose(S);
      
    Stream.Free;
  end;
end;


function JsonString(const O: TJSONObject;
                    const Key: string): string;
var
  V: TJSONValue;

begin

  Result := '';
  
  if (O = nil) then
    Exit;
 
  V := O.GetValue(Key);
  
  if (V <> nil) and (V.ClassType = TJSONString) then
    Result := V.Value;
end;


function MfCastEurekaGetInfo(const ADevice: TMfCastDevice;
                             const AOptions: TMfCastEurekaOptions;
                             out AInfo: TMfCastEurekaInfo;
                             out ADetail: string): HRESULT;
var
  Text: string;
  Sent: Boolean;
  Value: TJSONValue; 
  V: TJSONValue;
  Obj: TJSONObject; 
  DeviceInfo: TJSONObject;

begin

  AInfo := Default(TMfCastEurekaInfo);
  
  Result := Http(ADevice,
                 AOptions,
                 'GET',
                 '/setup/eureka_info?params=name,device_info',
                 '',
                 AInfo.HttpStatus,
                 Text,
                 ADetail,
                 Sent);
  
  if Failed(Result) then
    Exit;
 
  Value := nil;
  
  try
    try
      Value := TJSONObject.ParseJSONValue(Text);
      
      if not (Value is TJSONObject) then
        begin
          ADetail := 'Eureka information is not a JSON object.';
          Exit(HRESULT_FROM_WIN32(ERROR_INVALID_DATA));
        end;
      
      Obj := TJSONObject(Value);
      DeviceInfo := nil;
      V := Obj.GetValue('device_info');
      
      if V is TJSONObject then
        DeviceInfo := TJSONObject(V);
      
      AInfo.Name := JsonString(Obj,
                               'name');
      if (AInfo.Name = '') then
        AInfo.Name := JsonString(DeviceInfo,
                                 'name');
      
      AInfo.DeviceId := JsonString(DeviceInfo,
                                   'ssdp_udn');
      if (AInfo.DeviceId = '') then
        AInfo.DeviceId := JsonString(Obj,
                                     'ssdp_udn');
      
      AInfo.ModelName := JsonString(DeviceInfo,
                                    'model_name');
      if (AInfo.Name = '') then
        begin
          ADetail := 'Eureka information contains no device name.';
          Exit(HRESULT_FROM_WIN32(ERROR_INVALID_DATA));
        end;
      
      if (Trim(ADevice.Id) <> '') and ((AInfo.DeviceId = '') or (NormalizeId(ADevice.Id) <> NormalizeId(AInfo.DeviceId))) then
        begin
          ADetail := 'Eureka identity is missing or does not match the selected Cast device.';
          Exit(HRESULT_FROM_WIN32(ERROR_INVALID_DATA));
        end;
      
      ADetail := 'Eureka device name read successfully.';
      Result := S_OK;
      
    except
      on E: Exception do
        begin
          Result := HRESULT_FROM_WIN32(ERROR_INVALID_DATA);
          ADetail := 'Invalid Eureka device information.';
        end;
    end;
    
  finally
    Value.Free;
  end;
end;


function ValidName(const S: string): Boolean;
var
  I: Integer;
  C: Word;

begin

  Result := False;
  
  if (S = '') or (Length(TEncoding.UTF8.GetBytes(S)) > 256) then
    Exit;
  
  I := 1;
  
  while (I <= Length(S)) do
    begin
      C := Ord(S[I]);
      
      if (C < 32) or (C = 127) then
        Exit;
      
      if (C >= $D800) and (C <= $DBFF) then
        begin
          Inc(I);
          if (I > Length(S)) or (Ord(S[I]) < $DC00) or (Ord(S[I]) > $DFFF) then
            Exit;
        end
      else
        if (C >= $DC00) and (C <= $DFFF) then
          Exit;
          
      Inc(I);
    end;
    
  Result := True;
end;


function MfCastEurekaRenameDevice(const ADevice: TMfCastDevice;
                                  const AName: string;
                                  const AOptions: TMfCastEurekaOptions;
                                  out AResult: TMfCastEurekaRenameResult;
                                  out ADetail: string): HRESULT;
var
  Name: string;
  Body: string;
  Reply: string;
  LastDetail: string;
  Obj: TJSONObject;
  Status: Cardinal;
  Started: UInt64;
  Elapsed: UInt64;
  ReadOptions: TMfCastEurekaOptions;
  Info: TMfCastEurekaInfo;

begin

  AResult := Default(TMfCastEurekaRenameResult);
  Name := Trim(AName);
  
  if not ValidName(Name) then
    begin
      ADetail := 'Device name must contain 1 to 256 UTF-8 bytes, without control characters or invalid Unicode.';
      Exit(E_INVALIDARG);
    end;
    
  Result := MfCastEurekaGetInfo(ADevice,
                                AOptions,
                                AResult.Info,
                                ADetail);
  
  if Failed(Result) then
    Exit;
    
  if (AResult.Info.Name = Name) then
    begin
      AResult.Verified := True;
      ADetail := 'The receiver already has the requested name.';
      Exit(S_OK);
    end;
    
  Obj := TJSONObject.Create;
  
  try
    Obj.AddPair('name',
                Name);
    Body := Obj.ToJSON;
    
  finally
    Obj.Free;
  end;
  
  // Exactly one POST. A transport failure may happen after the receiver commits.
  Result := Http(ADevice,
                 AOptions,
                 'POST',
                 '/setup/set_eureka_info',
                 Body,
                 Status,
                 Reply,
                 ADetail,
                 AResult.WriteAttempted);
                 
  AResult.Info.HttpStatus := Status;
  AResult.WriteAccepted := (Status >= 200) and (Status < 300);
  
  if Failed(Result) then
    begin
      if AResult.WriteAttempted then
        ADetail := ADetail + ' Rename was not confirmed; re-read the name before retrying.';
      Exit;
    end;
    
  AResult.WriteAccepted := True;
  Started := Tick64;
  ReadOptions := AOptions;
  
  repeat
    Elapsed := Tick64 - Started;
    if (AOptions.VerificationTimeoutMs > 0) then
      begin
        if (Elapsed >= UInt64(AOptions.VerificationTimeoutMs)) then
          Break;
        
        if (ReadOptions.TimeoutMs > AOptions.VerificationTimeoutMs - Integer(Elapsed)) then
          ReadOptions.TimeoutMs := AOptions.VerificationTimeoutMs - Integer(Elapsed);
      end;
      
    Result := MfCastEurekaGetInfo(ADevice,
                                  ReadOptions,
                                  Info,
                                  LastDetail);
    if Succeeded(Result) then
      begin
        AResult.Info := Info;
        
        if (Info.Name = Name) then
          begin
            AResult.Verified := True;
            ADetail := 'Receiver name changed and verified.';
            Exit(S_OK);
          end;
      end
    else
      if (Result = E_ACCESSDENIED) or (Result = E_NOTIMPL) or (Result = HRESULT_FROM_WIN32(ERROR_INVALID_DATA)) then
        Break;
        
    if (AOptions.VerificationTimeoutMs = 0) then
      Break;
      
    Sleep(100);
  until False;
  
  ADetail := 'The receiver accepted the rename, but the new name could not be verified. Re-read before retrying.';
  
  if Failed(Result) then
    ADetail := ADetail + ' ' + LastDetail
  else
    Result := HRESULT_FROM_WIN32(ERROR_TIMEOUT);
end;

end.

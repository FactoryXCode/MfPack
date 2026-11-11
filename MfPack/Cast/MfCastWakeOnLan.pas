// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastWakeOnLan.pas
// Kind: Pascal Unit
// Release date: 09-11-2026
// Language: ENU
//
// Revision Version: 4.0.1
// Description: Native Wake-on-LAN packets, per-receiver MAC caching,
//              and selection of the receiver's local IPv4 network interface.
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
unit MfCastWakeOnLan;

interface

uses
  Winapi.Windows, System.SysUtils, MfCastTypes;

type
  TMfCastWakeTarget = record
    Address: string;
    MacAddress: string;
    LocalAddress: string;
    BroadcastAddress: string;
  end;

function MfCastBuildWakePacket(const AMacAddress: string;
                               out APacket: TBytes): Boolean;
function MfCastGetWakeTarget(const ADevice: TMfCastDevice;
                             out ATarget: TMfCastWakeTarget;
                             const ACacheFileName: string = ''): Boolean;
procedure MfCastRememberWakeTarget(const ADevice: TMfCastDevice);
function MfCastSendWakeOnLan(const ATarget: TMfCastWakeTarget;
                             const APort: Word = 9): HRESULT;

implementation

uses
  Winapi.WinSock, Winapi.IpHlpApi, Winapi.IpTypes, Winapi.IpRtrMib,
  Winapi.WinError;

function MfCastBuildWakePacket(const AMacAddress: string;
                               out APacket: TBytes): Boolean;
var
  Text: string;
  Mac: array[0..5] of Byte;
  I, Value: Integer;
  Nonzero: Boolean;
begin
  Result := False;
  APacket := nil;
  Text := StringReplace(StringReplace(Trim(AMacAddress), ':', '',
                         [rfReplaceAll]), '-', '', [rfReplaceAll]);
  if Length(Text) <> 12 then
    Exit;
  Nonzero := False;
  for I := 0 to 5 do
    begin
      if not TryStrToInt('$' + Copy(Text, I * 2 + 1, 2), Value) then
        Exit;
      Mac[I] := Byte(Value);
      Nonzero := Nonzero or (Mac[I] <> 0);
    end;
  // Only an individual device address is a valid wake target.
  if not Nonzero or ((Mac[0] and 1) <> 0) then
    Exit;
  SetLength(APacket, 102);
  FillChar(APacket[0], 6, $FF);
  for I := 0 to 15 do
    Move(Mac[0], APacket[6 + I * 6], 6);
  Result := True;
end;

function CacheSection(const ADevice: TMfCastDevice): string;
var
  Identity: string;
  Bytes: TBytes;
  I: Integer;
begin
  Identity := Trim(ADevice.Id);
  if Identity = '' then
    Identity := Trim(ADevice.ServiceInstance);
  // Do not persist an IP-only association: DHCP can give it to another device.
  Result := '';
  if Identity = '' then
    Exit;
  Bytes := TEncoding.UTF8.GetBytes(LowerCase(Identity));
  Result := 'Device-';
  for I := 0 to Length(Bytes) - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
end;

function CacheFileName(const AOverride: string): string;
var
  Root: string;
begin
  Result := AOverride;
  if Result <> '' then
    Exit;
  Root := GetEnvironmentVariable('LOCALAPPDATA');
  if Root <> '' then
    Result := IncludeTrailingPathDelimiter(Root) + 'MfPack\Cast\WakeTargets.ini';
end;

function ReadMac(const AFileName, ASection: string): string;
var
  Buffer: array[0..63] of Char;
begin
  Result := '';
  if (AFileName = '') or (ASection = '') then
    Exit;
  SetString(Result, Buffer, GetPrivateProfileString(PChar(ASection),
    'MacAddress', '', Buffer, Length(Buffer), PChar(AFileName)));
end;

function FindNetwork(const AAddress: DWORD; out AInterfaceIndex: DWORD;
                     out ALocal, ABroadcast: string): Boolean;
var
  Buffer: TBytes;
  Size: ULONG;
  Adapter: PIP_ADAPTER_INFO;
  Ip: PIP_ADDR_STRING;
  LocalIp, Mask: DWORD;
  BroadcastIp: TInAddr;
begin
  Result := False;
  AInterfaceIndex := 0;
  ALocal := '';
  ABroadcast := '';
  if GetBestInterface(AAddress, AInterfaceIndex) <> NO_ERROR then
    Exit;
  Size := 0;
  if GetAdaptersInfo(nil, Size) <> ERROR_BUFFER_OVERFLOW then
    Exit;
  SetLength(Buffer, Size);
  if GetAdaptersInfo(PIP_ADAPTER_INFO(@Buffer[0]), Size) <> NO_ERROR then
    Exit;
  Adapter := PIP_ADAPTER_INFO(@Buffer[0]);
  while Adapter <> nil do
    begin
      if Adapter.Index = AInterfaceIndex then
        begin
          Ip := @Adapter.IpAddressList;
          while Ip <> nil do
            begin
              LocalIp := inet_addr(@Ip.IpAddress.S[0]);
              Mask := inet_addr(@Ip.IpMask.S[0]);
              if (LocalIp <> 0) and (LocalIp <> INADDR_NONE) and
                 (Mask <> 0) and (Mask <> INADDR_NONE) and
                 ((AAddress and Mask) = (LocalIp and Mask)) then
                begin
                  ALocal := string(AnsiString(PAnsiChar(@Ip.IpAddress.S[0])));
                  BroadcastIp.S_addr := AAddress or not Mask;
                  ABroadcast := string(AnsiString(inet_ntoa(BroadcastIp)));
                  Exit(True);
                end;
              Ip := Ip.Next;
            end;
        end;
      Adapter := Adapter.Next;
    end;
end;

function NeighborMac(const AAddress, AInterfaceIndex: DWORD): string;
var
  Buffer: TBytes;
  Size: ULONG;
  Table: PMIB_IPNETTABLE;
  Row: PMIB_IPNETROW;
  I, J: Cardinal;
begin
  Result := '';
  Size := 0;
  if GetIpNetTable(nil, Size, False) <> ERROR_INSUFFICIENT_BUFFER then
    Exit;
  SetLength(Buffer, Size);
  Table := PMIB_IPNETTABLE(@Buffer[0]);
  if GetIpNetTable(Table, Size, False) <> NO_ERROR then
    Exit;
  if Table.dwNumEntries = 0 then
    Exit;
  for I := 0 to Table.dwNumEntries - 1 do
    begin
      Row := PMIB_IPNETROW(NativeUInt(@Table.table[0]) + I * SizeOf(MIB_IPNETROW));
      if (Row.dwAddr = AAddress) and (Row.dwIndex = AInterfaceIndex) and
         (Row.dwPhysAddrLen = 6) and (Row.dwType in [3, 4]) then
        begin
          for J := 0 to 5 do
            begin
              if J > 0 then
                Result := Result + ':';
              Result := Result + IntToHex(Row.bPhysAddr[J], 2);
            end;
          Exit;
        end;
    end;
end;

function MfCastGetWakeTarget(const ADevice: TMfCastDevice;
                             out ATarget: TMfCastWakeTarget;
                             const ACacheFileName: string): Boolean;
var
  AddressText: AnsiString;
  Address, InterfaceIndex: DWORD;
  FileName, Section, LearnedMac: string;
  Packet: TBytes;
begin
  Result := False;
  ATarget.Address := Trim(ADevice.Address);
  ATarget.MacAddress := '';
  ATarget.LocalAddress := '';
  ATarget.BroadcastAddress := '';
  AddressText := AnsiString(ATarget.Address);
  Address := inet_addr(PAnsiChar(AddressText));
  if (Address = 0) or (Address = INADDR_NONE) then
    Exit;
  if not FindNetwork(Address, InterfaceIndex, ATarget.LocalAddress,
                      ATarget.BroadcastAddress) then
    Exit;
  FileName := CacheFileName(ACacheFileName);
  Section := CacheSection(ADevice);
  LearnedMac := NeighborMac(Address, InterfaceIndex);
  if MfCastBuildWakePacket(LearnedMac, Packet) then
    begin
      ATarget.MacAddress := LearnedMac;
      // Cache failure must never prevent discovery or ordinary playback.
      if (FileName <> '') and (Section <> '') and
         not SameText(ReadMac(FileName, Section), LearnedMac) then
        try
          if ForceDirectories(ExtractFilePath(FileName)) then
            WritePrivateProfileString(PChar(Section), 'MacAddress',
                                       PChar(LearnedMac), PChar(FileName));
        except
          on E: Exception do ;
        end;
    end
  else
    ATarget.MacAddress := ReadMac(FileName, Section);
  Result := MfCastBuildWakePacket(ATarget.MacAddress, Packet);
end;

procedure MfCastRememberWakeTarget(const ADevice: TMfCastDevice);
var
  Target: TMfCastWakeTarget;
begin
  MfCastGetWakeTarget(ADevice, Target);
end;

function MfCastSendWakeOnLan(const ATarget: TMfCastWakeTarget;
                             const APort: Word): HRESULT;
var
  Packet: TBytes;
  Wsa: TWSAData;
  Sock: TSocket;
  Local, Destination: TSockAddrIn;
  EnableBroadcast, I, J, Sent, ErrorCode: Integer;
  AnySent: Boolean;
  AddressText: AnsiString;
begin
  if not MfCastBuildWakePacket(ATarget.MacAddress, Packet) or (APort = 0) then
    Exit(E_INVALIDARG);
  ErrorCode := WSAStartup($0202, Wsa);
  if ErrorCode <> 0 then
    Exit(HRESULT_FROM_WIN32(ErrorCode));
  try
    Sock := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if Sock = INVALID_SOCKET then
      Exit(HRESULT_FROM_WIN32(WSAGetLastError()));
    try
      EnableBroadcast := 1;
      if setsockopt(Sock, SOL_SOCKET, SO_BROADCAST, PAnsiChar(@EnableBroadcast),
                     SizeOf(EnableBroadcast)) = SOCKET_ERROR then
        Exit(HRESULT_FROM_WIN32(WSAGetLastError()));
      FillChar(Local, SizeOf(Local), 0);
      Local.sin_family := AF_INET;
      AddressText := AnsiString(ATarget.LocalAddress);
      Local.sin_addr.S_addr := inet_addr(PAnsiChar(AddressText));
      if (Local.sin_addr.S_addr = 0) or (DWORD(Local.sin_addr.S_addr) = INADDR_NONE) then
        Exit(E_INVALIDARG);
      if bind(Sock, TSockAddr(Local), SizeOf(Local)) = SOCKET_ERROR then
        Exit(HRESULT_FROM_WIN32(WSAGetLastError()));
      AnySent := False;
      ErrorCode := WSAEHOSTUNREACH;
      for I := 0 to 2 do
        begin
          for J := 0 to 1 do
            begin
              FillChar(Destination, SizeOf(Destination), 0);
              Destination.sin_family := AF_INET;
              Destination.sin_port := htons(APort);
              if J = 0 then
                AddressText := AnsiString(ATarget.BroadcastAddress)
              else
                AddressText := AnsiString(ATarget.Address);
              Destination.sin_addr.S_addr := inet_addr(PAnsiChar(AddressText));
              if (Destination.sin_addr.S_addr = 0) or
                 (DWORD(Destination.sin_addr.S_addr) = INADDR_NONE) then
                Continue;
              Sent := sendto(Sock, Packet[0], Length(Packet), 0,
                               TSockAddr(Destination), SizeOf(Destination));
              if Sent = Length(Packet) then
                AnySent := True
              else
                ErrorCode := WSAGetLastError();
            end;
          if I < 2 then
            Sleep(250);
        end;
      if AnySent then
        Result := S_OK
      else
        Result := HRESULT_FROM_WIN32(ErrorCode);
    finally
      closesocket(Sock);
    end;
  finally
    WSACleanup();
  end;
end;

end.

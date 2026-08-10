// FactoryX
//
// Copyright ? FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastDiscovery.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Unit for _googlecast._tcp.local discovery.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Carmen (carmenh).
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
unit MfCastDiscovery;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type

  TMfCastIPv4AddressArray = array of u_long;

  TMfCastMdnsDiscovery = class(TInterfacedObject, IMfCastDiscovery)
  private
    FProtocol: TMfCastProtocolSettings;
    FSettings: TMfCastDiscoverySettings;
    FCallbacks: TMfCastDiscoveryCallbacks;
    FLogger: IMfCastLogger;
    FDevices: TMfCastDeviceArray;
    FLock: TCriticalSection;
    FRunning: Boolean;
    FSocket: TSocket;
    FWSAStarted: Boolean;
    FInterfaceAddresses: TMfCastIPv4AddressArray;

    procedure Log(const ALevel: TMfCastLogLevel;
                  const AMessage: string);
    function EnsureSocket(): HRESULT;
    procedure CloseSocket();
    function SendQuery(): HRESULT;
    function ReceiveResponses(): HRESULT;
    function ParseResponse(const AData: TBytes;
                           out ADevice: TMfCastDevice): HRESULT;
    function UpsertDevice(const ADevice: TMfCastDevice): HRESULT;
    procedure RemoveExpiredDevices();

  public

    constructor Create();
    destructor Destroy(); override;

    function Configure(const AProtocol: TMfCastProtocolSettings;
                       const ASettings: TMfCastDiscoverySettings): HRESULT;

    procedure SetCallbacks(const ACallbacks: TMfCastDiscoveryCallbacks);
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(): HRESULT;
    function Stop(): HRESULT;
    function Refresh(): HRESULT;
    function GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
    function IsRunning(): Boolean;
  end;


implementation

const
  MFCAST_MDNS_IPV4_ADDRESS = '224.0.0.251';
  MFCAST_MDNS_PORT = 5353;
  MFCAST_DNS_TYPE_A = 1;
  MFCAST_DNS_TYPE_PTR = 12;
  MFCAST_DNS_TYPE_TXT = 16;
  MFCAST_DNS_TYPE_SRV = 33;
  MFCAST_DNS_CLASS_IN = 1;
  MFCAST_MSECS_PER_DAY = 86400000.0;

type
  TMfCastIpMreq = record
    imr_multiaddr: TInAddr;
    imr_interface: TInAddr;
  end;


function MfCastWinSockHResult(): HRESULT;
var
  ErrorCode: Integer;

begin

  ErrorCode := WSAGetLastError();
  if (ErrorCode = 0) then
    Result := E_FAIL
  else
    Result := HRESULT($80070000 or DWORD(ErrorCode));
end;


procedure MfCastAppendByte(var AData: TBytes;
                           AValue: Byte);
var
  Index: Integer;

begin

  Index := Length(AData);
  SetLength(AData,
            Index + 1);
  AData[Index] := AValue;
end;


procedure MfCastAppendUInt16(var AData: TBytes;
                             AValue: Word);
begin

  MfCastAppendByte(AData,
                   Byte((AValue shr 8) and $FF));

  MfCastAppendByte(AData,
                   Byte(AValue and $FF));
end;


procedure MfCastAppendDnsName(var AData: TBytes;
                              const AName: string);
var
  LabelStart: Integer;
  DotPos: Integer;
  LabelText: AnsiString;
  Remaining: string;
  I: Integer;

begin

  Remaining := AName;

  while (Remaining <> '') do
    begin

      DotPos := Pos('.',
                    Remaining);
      if (DotPos = 0) then
        begin
          LabelText := AnsiString(Remaining);
          Remaining := '';
        end
      else
        begin
          LabelText := AnsiString(Copy(Remaining,
                                       1,
                                       DotPos - 1));
          Delete(Remaining,
                 1,
                 DotPos);
        end;

      LabelStart := Length(AData);
      MfCastAppendByte(AData, Byte(Length(LabelText)));

      for I := 1 to Length(LabelText) do
        MfCastAppendByte(AData,
                         Byte(LabelText[I]));

      if Length(AData) = LabelStart + 1 then
        Break;
    end;

  MfCastAppendByte(AData, 0);
end;


function MfCastBuildPtrQuery(const AServiceName: string): TBytes;
begin

  SetLength(Result,
            0);

  MfCastAppendUInt16(Result,
                     0);
  MfCastAppendUInt16(Result,
                     0);
  MfCastAppendUInt16(Result,
                     1);
  MfCastAppendUInt16(Result,
                     0);
  MfCastAppendUInt16(Result,
                     0);
  MfCastAppendUInt16(Result,
                     0);

  MfCastAppendDnsName(Result,
                      AServiceName);

  MfCastAppendUInt16(Result,
                     MFCAST_DNS_TYPE_PTR);
  MfCastAppendUInt16(Result,
                     MFCAST_DNS_CLASS_IN);
end;


function MfCastReadUInt16(const AData: TBytes; var AOffset: Integer;
                          out AValue: Word): Boolean;
begin

  Result := (AOffset + 1 < Length(AData));
  if not Result then
    Exit;

  AValue := Word((Word(AData[AOffset]) shl 8) or Word(AData[AOffset + 1]));
  Inc(AOffset, 2);
end;


function MfCastReadUInt32(const AData: TBytes; var AOffset: Integer;
                          out AValue: Cardinal): Boolean;
begin

  Result := AOffset + 3 < Length(AData);
  if not Result then
    Exit;

  AValue := (Cardinal(AData[AOffset]) shl 24) or
            (Cardinal(AData[AOffset + 1]) shl 16) or
            (Cardinal(AData[AOffset + 2]) shl 8) or
             Cardinal(AData[AOffset + 3]);
  Inc(AOffset,
      4);
end;


function MfCastReadDnsName(const AData: TBytes;
                           var AOffset: Integer;
                           out AName: string): Boolean;
var
  Position: Integer;
  LabelLength: Integer;
  LabelText: AnsiString;
  I: Integer;
  PointerOffset: Integer;
  Jumped: Boolean;
  JumpCount: Integer;

begin

  Result := False;
  AName := '';
  Position := AOffset;
  Jumped := False;
  JumpCount := 0;

  while True do
    begin
      if (Position >= Length(AData)) then
        Exit;

      LabelLength := AData[Position];
      if (LabelLength and $C0) = $C0 then
        begin

          if (Position + 1 >= Length(AData)) then
            Exit;

          PointerOffset := ((LabelLength and $3F) shl 8) or AData[Position + 1];
          if (PointerOffset >= Length(AData)) then
            Exit;

          if not Jumped then
            AOffset := Position + 2;

          Position := PointerOffset;
          Jumped := True;
          Inc(JumpCount);

          if (JumpCount > 32) then
            Exit;

          Continue;
        end;

      if ((LabelLength and $C0) <> 0) then
        Exit;

      Inc(Position);
      if (LabelLength = 0) then
        begin
          if not Jumped then
            AOffset := Position;
          Result := True;
          Exit;
        end;

      if (Position + LabelLength > Length(AData)) then
        Exit;

      SetLength(LabelText,
                LabelLength);

      for I := 0 to LabelLength - 1 do
        LabelText[I + 1] := AnsiChar(AData[Position + I]);

      if (AName <> '') then
        AName := AName + '.';

      AName := AName + string(LabelText);
      Inc(Position,
          LabelLength);
    end;
end;


procedure MfCastAppendTxtEntry(var AEntries: TMfCastTxtEntryArray;
                               const AName: string;
                               const AValue: string);
var
  Index: Integer;

begin

  Index := Length(AEntries);
  SetLength(AEntries, Index + 1);
  AEntries[Index].Name := AName;
  AEntries[Index].Value := AValue;
end;


procedure MfCastMergeDevice(var ATarget: TMfCastDevice;
                            const ASource: TMfCastDevice);
begin

  if (ASource.Id <> '') then
    ATarget.Id := ASource.Id;

  if (ASource.ServiceInstance <> '') then
    ATarget.ServiceInstance := ASource.ServiceInstance;

  if (ASource.FriendlyName <> '') then
    ATarget.FriendlyName := ASource.FriendlyName;

  if (ASource.ModelName <> '') then
    ATarget.ModelName := ASource.ModelName;

  if (ASource.HostName <> '') then
    ATarget.HostName := ASource.HostName;

  if (ASource.Address <> '') then
    ATarget.Address := ASource.Address;

  if (ASource.Port <> 0) then
    ATarget.Port := ASource.Port;

 if (ASource.RawCapabilities <> 0) then
    ATarget.RawCapabilities := ASource.RawCapabilities;

  if (Length(ASource.TxtEntries) > 0) then
    ATarget.TxtEntries := Copy(ASource.TxtEntries);
  ATarget.LastSeenUtc := Now();
end;


function MfCastSameDevice(const ALeft: TMfCastDevice;
                          const ARight: TMfCastDevice): Boolean;
begin

  Result := ((ALeft.Id <> '') and
            (ARight.Id <> '') and
             SameText(ALeft.Id,
                      ARight.Id)) or
            ((ALeft.ServiceInstance <> '') and
            (ARight.ServiceInstance <> '') and
             SameText(ALeft.ServiceInstance,
                      ARight.ServiceInstance)) or
            ((ALeft.HostName <> '') and
            (ARight.HostName <> '') and
             SameText(ALeft.HostName,
                      ARight.HostName));
end;


function MfCastIsServiceInstance(const AName: string;
                                 const AServiceName: string): Boolean;
var
  Suffix: string;

begin

  Suffix := '.' + AServiceName;
  Result := SameText(AName,
                     AServiceName) or
            ((Length(AName) > Length(Suffix)) and
            SameText(Copy(AName,
                          Length(AName) - Length(Suffix) + 1,
                          MaxInt),
                          Suffix));
end;

constructor TMfCastMdnsDiscovery.Create();
begin

  inherited Create;

  FLock := TCriticalSection.Create;
  FRunning := False;
  FSocket := INVALID_SOCKET;
  FWSAStarted := False;
  FCallbacks.Reset;
  SetLength(FDevices, 0);
end;


destructor TMfCastMdnsDiscovery.Destroy();
begin

  Stop();
  CloseSocket();
  if FWSAStarted then
    WSACleanup();

  FLock.Free();

  inherited Destroy();
end;


function TMfCastMdnsDiscovery.Configure(const AProtocol: TMfCastProtocolSettings;
                                        const ASettings: TMfCastDiscoverySettings): HRESULT;
begin

  if FRunning then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  FProtocol := AProtocol;
  FSettings := ASettings;
  Result := S_OK;
end;


procedure TMfCastMdnsDiscovery.SetCallbacks(const ACallbacks: TMfCastDiscoveryCallbacks);
begin

  FCallbacks := ACallbacks;
end;


procedure TMfCastMdnsDiscovery.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function MfCastIPv4AddressText(const AAddress: u_long): string;
var
  Address: TInAddr;

begin

  Address.S_addr := AAddress;
  Result := Format('%d.%d.%d.%d',
                   [Integer(Address.S_un_b.s_b1),
                    Integer(Address.S_un_b.s_b2),
                    Integer(Address.S_un_b.s_b3),
                    Integer(Address.S_un_b.s_b4)]);
end;


function MfCastGetLocalIPv4Addresses(): TMfCastIPv4AddressArray;
var
  HostName: array[0..255] of AnsiChar;
  HostEntry: PHostEnt;
  Address: PInAddr;
  AddressValue: u_long;
  I: Integer;
  J: Integer;
  Duplicate: Boolean;

begin

  SetLength(Result,
            0);
  FillChar(HostName,
           SizeOf(HostName),
           0);

  if gethostname(@HostName[0],
                 SizeOf(HostName)) <> 0 then
    Exit;

  HostEntry := gethostbyname(@HostName[0]);
  if not Assigned(HostEntry) then
    Exit;

  I := 0;
  while Assigned(HostEntry^.h_addr_list[I]) do
    begin
      Address := PInAddr(HostEntry^.h_addr_list[I]);
      if Assigned(Address) and
         (Integer(Address^.S_un_b.s_b1) <> 0) and
         (Integer(Address^.S_un_b.s_b1) <> 127) then
        begin
          AddressValue := Address^.S_addr;
          Duplicate := False;

          for J := 0 to Length(Result) - 1 do
            if (Result[J] = AddressValue) then
              begin
                Duplicate := True;
                Break;
              end;

          if not Duplicate then
            begin
              SetLength(Result,
                        Length(Result) + 1);
              Result[Length(Result) - 1] := AddressValue;
            end;
        end;

      Inc(I);
    end;
end;


procedure TMfCastMdnsDiscovery.Log(const ALevel: TMfCastLogLevel;
                                   const AMessage: string);
begin

  if Assigned(FLogger) then
    FLogger.Log(ALevel,
                'Discovery',
                AMessage)
  else
    OutputDebugString(PChar('[MfCast][Discovery] ' + AMessage));
end;


function TMfCastMdnsDiscovery.Start(): HRESULT;
begin

  if FRunning then
    begin
      Log(cllDebug,
          'Discover called while discovery is active; refreshing mDNS query.');
      Result := Refresh();
      Exit;
    end;

  FRunning := True;
  Log(cllDebug,
      Format('Starting mDNS discovery: service="%s" responseWindow=%d ms interface="%s".',
             [FProtocol.DiscoveryServiceName,
              FSettings.ResponseWindowMs,
              FSettings.LocalInterfaceAddress]));
  if Assigned(FCallbacks.OnStarted) then
    FCallbacks.OnStarted();

  Result := Refresh();
  if FAILED(Result) then
    begin
      FRunning := False;
      if Assigned(FCallbacks.OnStopped) then
        FCallbacks.OnStopped();
    end;
end;


function TMfCastMdnsDiscovery.Stop(): HRESULT;
begin

  FRunning := False;
  Result := S_OK;
end;


function TMfCastMdnsDiscovery.Refresh(): HRESULT;
begin

  if not FRunning then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  RemoveExpiredDevices();

  Result := SendQuery();
  if FAILED(Result) then
    Exit;

  Result := ReceiveResponses();
  if SUCCEEDED(Result) then
    RemoveExpiredDevices();
end;


function TMfCastMdnsDiscovery.GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
var
  I: Integer;

begin

  FLock.Acquire;

  try

   SetLength(ADevices,
              Length(FDevices));

    for I := 0 to Length(FDevices) - 1 do
      ADevices[I] := FDevices[I];

  finally
    FLock.Release;
  end;

  Result := S_OK;
end;


function TMfCastMdnsDiscovery.IsRunning(): Boolean;
begin

  Result := FRunning;
end;


function TMfCastMdnsDiscovery.EnsureSocket(): HRESULT;
var
  WsaData: TWSAData;
  BindAddr: TSockAddrIn;
  ReuseAddr: Integer;
  NonBlocking: u_long;
  MulticastTtl: Integer;
  Membership: TMfCastIpMreq;
  BindAddress: AnsiString;
  InterfaceAddress: u_long;
  InterfaceIndex: Integer;
  JoinedCount: Integer;

begin

  if (FSocket <> INVALID_SOCKET) then
    begin
      Result := S_OK;
      Exit;
    end;

  if not FWSAStarted then
    begin
      if (WSAStartup($0202,
                    WsaData) <> 0) then
        begin
          Result := MfCastWinSockHResult();
          Exit;
        end;

      FWSAStarted := True;
    end;

  FSocket := socket(AF_INET,
                    SOCK_DGRAM,
                    IPPROTO_UDP);

  if (FSocket = INVALID_SOCKET) then
    begin
      Result := MfCastWinSockHResult();
      Exit;
    end;

  ReuseAddr := 1;
  setsockopt(FSocket,
             SOL_SOCKET,
             SO_REUSEADDR,
             PAnsiChar(@ReuseAddr),
             SizeOf(ReuseAddr));

  MulticastTtl := 255;
  if setsockopt(FSocket,
                IPPROTO_IP,
                IP_MULTICAST_TTL,
                PAnsiChar(@MulticastTtl),
                SizeOf(MulticastTtl)) = SOCKET_ERROR then
    begin
      Result := MfCastWinSockHResult();
      Log(cllWarning,
          Format('Setting the required mDNS multicast TTL failed: HRESULT $%.8x.',
                 [DWORD(Result)]));
      CloseSocket();
      Exit;
    end;

  FillChar(BindAddr,
           SizeOf(BindAddr),
           0);

  BindAddr.sin_family := AF_INET;
  BindAddr.sin_port := htons(MFCAST_MDNS_PORT);

  if (FSettings.LocalInterfaceAddress <> '') then
    begin
      BindAddress := AnsiString(FSettings.LocalInterfaceAddress);
      BindAddr.sin_addr.S_addr := inet_addr(PAnsiChar(BindAddress));
    end
  else
    BindAddr.sin_addr.S_addr := INADDR_ANY;

  if (bind(FSocket,
           TSockAddr(BindAddr),
           SizeOf(BindAddr)) = SOCKET_ERROR) then
    begin
      Result := MfCastWinSockHResult();
      CloseSocket();
      Exit;
    end;

  if (FSettings.LocalInterfaceAddress <> '') then
    begin
      SetLength(FInterfaceAddresses,
                1);
      FInterfaceAddresses[0] := inet_addr(PAnsiChar(AnsiString(FSettings.LocalInterfaceAddress)));
    end
  else
    FInterfaceAddresses := MfCastGetLocalIPv4Addresses();

  if (Length(FInterfaceAddresses) = 0) then
    begin
      SetLength(FInterfaceAddresses,
                1);
      FInterfaceAddresses[0] := INADDR_ANY;
    end;

  JoinedCount := 0;
  for InterfaceIndex := 0 to Length(FInterfaceAddresses) - 1 do
    begin
      InterfaceAddress := FInterfaceAddresses[InterfaceIndex];
      FillChar(Membership,
               SizeOf(Membership),
               0);
      Membership.imr_multiaddr.S_addr := inet_addr(PAnsiChar(AnsiString(MFCAST_MDNS_IPV4_ADDRESS)));
      Membership.imr_interface.S_addr := InterfaceAddress;

      if setsockopt(FSocket,
                    IPPROTO_IP,
                    IP_ADD_MEMBERSHIP,
                    PAnsiChar(@Membership),
                    SizeOf(Membership)) = SOCKET_ERROR then
        Log(cllWarning,
            Format('Joining mDNS on interface %s failed: HRESULT $%.8x.',
                   [MfCastIPv4AddressText(InterfaceAddress),
                    DWORD(MfCastWinSockHResult())]))
      else
        begin
          Inc(JoinedCount);
          Log(cllDebug,
              Format('Joined mDNS multicast group on interface %s.',
                     [MfCastIPv4AddressText(InterfaceAddress)]));
        end;
    end;

  if (JoinedCount = 0) then
    begin
      Result := MfCastWinSockHResult();
      CloseSocket();
      Exit;
    end;

  NonBlocking := 1;
  if ioctlsocket(FSocket,
                 FIONBIO,
                 NonBlocking) = SOCKET_ERROR then
    begin
      Result := MfCastWinSockHResult();
      CloseSocket();
      Exit;
    end;

  Result := S_OK;
end;


procedure TMfCastMdnsDiscovery.CloseSocket();
begin

  if (FSocket <> INVALID_SOCKET) then
    begin
      WinApi.WinSock.closesocket(FSocket);
      FSocket := INVALID_SOCKET;
    end;

  SetLength(FInterfaceAddresses,
            0);
end;


function TMfCastMdnsDiscovery.SendQuery(): HRESULT;
var
  Query: TBytes;
  Addr: TSockAddrIn;
  ServiceName: string;
  InterfaceAddress: u_long;
  InterfaceIndex: Integer;
  SentCount: Integer;
  SendResult: Integer;

begin

  if not FSettings.IncludeIPv4 then
    begin
      Result := S_OK;
      Exit;
    end;

  Result := EnsureSocket();
  if FAILED(Result) then
    Exit;

  ServiceName := FProtocol.DiscoveryServiceName;
  if (ServiceName = '') then
    ServiceName := '_googlecast._tcp.local';

  Query := MfCastBuildPtrQuery(ServiceName);

  FillChar(Addr,
           SizeOf(Addr),
           0);

  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(MFCAST_MDNS_PORT);
  Addr.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(MFCAST_MDNS_IPV4_ADDRESS)));

  SentCount := 0;
  Result := E_FAIL;

  for InterfaceIndex := 0 to Length(FInterfaceAddresses) - 1 do
    begin
      InterfaceAddress := FInterfaceAddresses[InterfaceIndex];

      if setsockopt(FSocket,
                    IPPROTO_IP,
                    IP_MULTICAST_IF,
                    PAnsiChar(@InterfaceAddress),
                    SizeOf(InterfaceAddress)) = SOCKET_ERROR then
        begin
          Result := MfCastWinSockHResult();
          Log(cllWarning,
              Format('Selecting mDNS interface %s failed: HRESULT $%.8x.',
                     [MfCastIPv4AddressText(InterfaceAddress), DWORD(Result)]));
          Continue;
        end;

      SendResult := sendto(FSocket,
                           Query[0],
                           Length(Query),
                           0,
                           TSockAddr(Addr),
                           SizeOf(Addr));

      if (SendResult = SOCKET_ERROR) then
        begin
          Result := MfCastWinSockHResult();
          Log(cllWarning,
              Format('Sending mDNS on interface %s failed: HRESULT $%.8x.',
                     [MfCastIPv4AddressText(InterfaceAddress), DWORD(Result)]));
        end
      else
        begin
          Inc(SentCount);
          Result := S_OK;
          Log(cllDebug,
              Format('mDNS query sent: service="%s" interface=%s bytes=%d.',
                     [ServiceName,
                      MfCastIPv4AddressText(InterfaceAddress),
                      Length(Query)]));
        end;
    end;

  if (SentCount > 0) then
    Result := S_OK;
end;


function TMfCastMdnsDiscovery.ReceiveResponses(): HRESULT;
var
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
  FromAddr: TSockAddrIn;
  FromLen: Integer;
  Packet: TBytes;
  Device: TMfCastDevice;
  StartTick: DWORD;
  WindowMs: Cardinal;
  LastError: Integer;
  ParseResult: HRESULT;
  ElapsedMs: Cardinal;
  NextQueryMs: Cardinal;
  PacketCount: Cardinal;
  AcceptedCount: Cardinal;

begin

  Result := EnsureSocket();
  if FAILED(Result) then
    Exit;

  WindowMs := FSettings.ResponseWindowMs;
  if (WindowMs = 0) then
    WindowMs := 1500;

  StartTick := GetTickCount();
  NextQueryMs := 500;
  PacketCount := 0;
  AcceptedCount := 0;

  repeat

    ElapsedMs := GetTickCount() - StartTick;
    if (NextQueryMs <= 1000) and (ElapsedMs >= NextQueryMs) then
      begin
        Result := SendQuery();
        if FAILED(Result) then
          Exit;
        Inc(NextQueryMs, 500);
      end;

    FromLen := SizeOf(FromAddr);
    FillChar(FromAddr,
             SizeOf(FromAddr),
             0);

    BytesRead := recvfrom(FSocket,
                          Buffer,
                          SizeOf(Buffer),
                          0,
                          TSockAddr(FromAddr),
                          FromLen);

    if (BytesRead = SOCKET_ERROR) then
      begin
        LastError := WSAGetLastError();
        if (LastError = WSAEWOULDBLOCK) then
          begin
            Sleep(25);
            Continue;
          end;

        Result := MfCastWinSockHResult();
        Exit;
      end;

    if (BytesRead > 0) then
      begin
        Inc(PacketCount);
        SetLength(Packet,
                  BytesRead);

        Move(Buffer[0],
             Packet[0],
             BytesRead);

        ParseResult := ParseResponse(Packet,
                                     Device);
        if ParseResult = S_OK then
          begin
            Inc(AcceptedCount);
            Log(cllDebug,
                Format('Accepted mDNS device reply: name="%s" address=%s:%d bytes=%d.',
                       [Device.FriendlyName,
                        Device.Address,
                        Device.Port,
                        BytesRead]));
            UpsertDevice(Device);
          end
        else
          if (ParseResult = S_FALSE) then
            begin
              if (Length(Packet) >= 4) and ((Packet[2] and $80) = 0) then
                Log(cllTrace,
                    Format('Observed mDNS query: source=%s bytes=%d.',
                           [MfCastIPv4AddressText(FromAddr.sin_addr.S_addr),
                            BytesRead]))
              else
                Log(cllTrace,
                    Format('Ignored incomplete or unrelated mDNS response: source=%s bytes=%d.',
                           [MfCastIPv4AddressText(FromAddr.sin_addr.S_addr),
                            BytesRead]));
            end
          else
            Log(cllTrace,
                Format('Ignored malformed mDNS packet: source=%s bytes=%d HRESULT $%.8x.',
                       [MfCastIPv4AddressText(FromAddr.sin_addr.S_addr),
                        BytesRead,
                        DWORD(ParseResult)]));

      end;
  until ((GetTickCount() - StartTick) >= WindowMs);

  Log(cllDebug,
      Format('mDNS response window finished: packets=%d accepted=%d.',
             [PacketCount, AcceptedCount]));
  Result := S_OK;
end;


function TMfCastMdnsDiscovery.ParseResponse(const AData: TBytes;
                                            out ADevice: TMfCastDevice): HRESULT;
var
  Offset: Integer;
  Flags: Word;
  QuestionCount: Word;
  AnswerCount: Word;
  AuthorityCount: Word;
  AdditionalCount: Word;
  RecordCount: Integer;
  I: Integer;
  RecordName: string;
  RecordType: Word;
  RecordClass: Word;
  RecordTtl: Cardinal;
  RecordLength: Word;
  RecordEnd: Integer;
  DataOffset: Integer;
  ServiceInstance: string;
  HostName: string;
  TxtText: AnsiString;
  TxtName: string;
  TxtValue: string;
  TxtLength: Integer;
  EqPos: Integer;
  EntryOffset: Integer;
  ServiceName: string;

begin

  ADevice.Reset;
  Result := E_FAIL;

  if (Length(AData) < 12) then
    Exit;

  Offset := 2;
  if not MfCastReadUInt16(AData,
                          Offset,
                          Flags) then
    Exit;

  if ((Flags and $8000) = 0) then
    begin
      Result := S_FALSE;
      Exit;
    end;

  ServiceName := FProtocol.DiscoveryServiceName;
  if (ServiceName = '') then
    ServiceName := '_googlecast._tcp.local';

  Offset := 4;

  if not MfCastReadUInt16(AData,
                          Offset,
                          QuestionCount) then
    Exit;

  if not MfCastReadUInt16(AData,
                          Offset,
                          AnswerCount) then
    Exit;

  if not MfCastReadUInt16(AData,
                          Offset,
                          AuthorityCount) then
    Exit;

  if not MfCastReadUInt16(AData,
                          Offset,
                          AdditionalCount) then
    Exit;

  for I := 0 to QuestionCount - 1 do
    begin
      if not MfCastReadDnsName(AData,
                               Offset,
                               RecordName) then
        Exit;

      Inc(Offset,
          4);

      if (Offset > Length(AData)) then
        Exit;
    end;

  RecordCount := AnswerCount + AuthorityCount + AdditionalCount;

  for I := 0 to RecordCount - 1 do
    begin
      if not MfCastReadDnsName(AData,
                               Offset,
                               RecordName) then
        Exit;

      if not MfCastReadUInt16(AData,
                              Offset,
                              RecordType) then
        Exit;

      if not MfCastReadUInt16(AData,
                              Offset,
                              RecordClass) then
        Exit;

      if not MfCastReadUInt32(AData,
                              Offset,
                              RecordTtl) then
        Exit;

      if not MfCastReadUInt16(AData,
                              Offset,
                              RecordLength) then
        Exit;

      RecordEnd := Offset + RecordLength;

      if (RecordEnd > Length(AData)) then
        Exit;

      DataOffset := Offset;

      case RecordType of
        MFCAST_DNS_TYPE_PTR:
          begin
            if SameText(RecordName,
                        ServiceName) and
               MfCastReadDnsName(AData,
                                 DataOffset,
                                 ServiceInstance) and
               MfCastIsServiceInstance(ServiceInstance,
                                       ServiceName) then
              ADevice.ServiceInstance := ServiceInstance;
          end;

        MFCAST_DNS_TYPE_SRV:
          begin

            if ((ADevice.ServiceInstance <> '') and SameText(RecordName,
                                                             ADevice.ServiceInstance)) or
               ((ADevice.ServiceInstance = '') and MfCastIsServiceInstance(RecordName,
                                                                           ServiceName)) then
              begin
                ADevice.ServiceInstance := RecordName;
                Inc(DataOffset,
                    4);

                if MfCastReadUInt16(AData,
                                    DataOffset,
                                    ADevice.Port) and
                   MfCastReadDnsName(AData,
                                     DataOffset,
                                     HostName) then
                  ADevice.HostName := HostName;
              end;
          end;

        MFCAST_DNS_TYPE_TXT:
          begin
            if ((ADevice.ServiceInstance <> '') and SameText(RecordName,
                                                             ADevice.ServiceInstance)) or
               ((ADevice.ServiceInstance = '') and MfCastIsServiceInstance(RecordName,
                                                                           ServiceName)) then
              begin
                ADevice.ServiceInstance := RecordName;
                EntryOffset := Offset;

                while (EntryOffset < RecordEnd) do
                  begin
                    TxtLength := AData[EntryOffset];
                    Inc(EntryOffset);

                    if (EntryOffset + TxtLength > RecordEnd) then
                      Break;

                    SetLength(TxtText,
                              TxtLength);

                    if (TxtLength > 0) then
                      Move(AData[EntryOffset],
                           TxtText[1],
                           TxtLength);

                    Inc(EntryOffset,
                        TxtLength);

                    EqPos := Pos('=', string(TxtText));

                    if (EqPos > 0) then
                      begin

                        TxtName := Copy(string(TxtText),
                                        1,
                                        EqPos - 1);

                        TxtValue := Copy(string(TxtText),
                                         EqPos + 1,
                                         MaxInt);
                      end
                    else
                      begin
                        TxtName := string(TxtText);
                        TxtValue := '';
                      end;

                    MfCastAppendTxtEntry(ADevice.TxtEntries, TxtName, TxtValue);

                    if SameText(TxtName,
                                'fn') then
                      ADevice.FriendlyName := TxtValue
                    else
                      if SameText(TxtName,
                                  'id') then
                        ADevice.Id := TxtValue
                      else
                        if SameText(TxtName,
                                    'md') then
                          ADevice.ModelName := TxtValue
                        else
                          if SameText(TxtName,
                                      'ca') then
                            ADevice.RawCapabilities := Cardinal(StrToIntDef(TxtValue,
                                                                            0));
                  end;
              end;
          end;

        MFCAST_DNS_TYPE_A:
          begin
            if (ADevice.HostName <> '') and SameText(RecordName,
                                                     ADevice.HostName) then
              begin
                ADevice.HostName := RecordName;

                if (RecordLength = 4) then
                  ADevice.Address := Format('%d.%d.%d.%d',
                                            [AData[Offset], AData[Offset + 1],
                                             AData[Offset + 2], AData[Offset + 3]]);
              end;
          end;
      end;

      Offset := RecordEnd;
    end;

  if (ADevice.Port = 0) then
    ADevice.Port := FProtocol.ControlPort;

  if (ADevice.Id = '') then
    ADevice.Id := ADevice.ServiceInstance;

  if (ADevice.FriendlyName = '') then
    ADevice.FriendlyName := ADevice.ServiceInstance;

  ADevice.LastSeenUtc := Now();
  Result := S_FALSE;

  if (ADevice.ServiceInstance <> '') and MfCastIsServiceInstance(ADevice.ServiceInstance,
                                                                 ServiceName) then
    Result := S_OK;
end;


function TMfCastMdnsDiscovery.UpsertDevice(const ADevice: TMfCastDevice): HRESULT;
var
  I: Integer;
  Index: Integer;
  Added: Boolean;
  CallbackDevice: TMfCastDevice;

begin

  Result := S_OK;
  Index := -1;
  Added := False;

  FLock.Acquire;

  try

    for I := 0 to Length(FDevices) - 1 do
      if MfCastSameDevice(FDevices[I],
                          ADevice) then
        begin
          Index := I;
          Break;
        end;

    if (Index < 0) then
      begin
        Index := Length(FDevices);
        SetLength(FDevices,
                  Index + 1);
        FDevices[Index].Reset();
        Added := True;
      end;

    MfCastMergeDevice(FDevices[Index],
                      ADevice);
    CallbackDevice := FDevices[Index];
  finally
    FLock.Release;
  end;

  if Added then
    begin

      if Assigned(FCallbacks.OnDeviceAdded) then
        FCallbacks.OnDeviceAdded(CallbackDevice);
    end
  else
    if Assigned(FCallbacks.OnDeviceUpdated) then
      FCallbacks.OnDeviceUpdated(CallbackDevice);
end;


procedure TMfCastMdnsDiscovery.RemoveExpiredDevices();
var
  I: Integer;
  J: Integer;
  RemovedId: string;
  AgeMs: Double;

begin

  if (FSettings.DeviceExpiryMs = 0) then
    Exit;

  I := 0;

  while (I < Length(FDevices)) do
    begin
      AgeMs := (Now() - FDevices[I].LastSeenUtc) * MFCAST_MSECS_PER_DAY;

      if (AgeMs > FSettings.DeviceExpiryMs) then
        begin
          RemovedId := FDevices[I].Id;
          if (RemovedId = '') then
            RemovedId := FDevices[I].ServiceInstance;

          FLock.Acquire;

          try

            for J := I to Length(FDevices) - 2 do
              FDevices[J] := FDevices[J + 1];

            SetLength(FDevices,
                      Length(FDevices) - 1);
          finally
            FLock.Release;
          end;

          if Assigned(FCallbacks.OnDeviceRemoved) then
            FCallbacks.OnDeviceRemoved(RemovedId);
        end
      else
        Inc(I);
    end;
end;

end.

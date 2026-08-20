// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack CaddyAdmin
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmCaddyAdmin.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Dialog to discover IPV4 devices on a local network.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//          Works with RDJ and RDJ Pro Caddy configurations on local or remote servers.
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
//==============================================================================
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
unit dlgLanDiscovery;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinSock,
  WinApi.WinSock2,
  WinApi.WinInet,
  WinApi.ShellAPI,
  WinApi.IpTypes,
  {System}
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Forms;

type

  TLanDiscoveryDialog = class;

  TLanDiscoveryThread = class(TThread)
  private
    FResults: TStringList;
    FAliases: TStringList;
    FMacAddresses: TStringList;
    FManufacturers: TStringList;
    FProgress: Integer;
    FTotal: Integer;
    FFinishedFlag: Integer;
    FErrorText: string;
    procedure AddAddress(const AAddress: string);

  protected
    procedure Execute(); override;

  public

    constructor Create();
    destructor Destroy(); override;

    function IsFinished(): Boolean;
    function Progress(): Integer;
    function Total(): Integer;

    property Results: TStringList read FResults;
    property Aliases: TStringList read FAliases;
    property MacAddresses: TStringList read FMacAddresses;
    property Manufacturers: TStringList read FManufacturers;
    property ErrorText: string read FErrorText;
  end;

  TLanDiscoveryDialog = class(TForm)
    lblStatus: TLabel;
    pbDiscovery: TProgressBar;
    lvDevices: TListView;
    btnRefresh: TButton;
    btnSaveList: TButton;
    btnOpenList: TButton;
    btnUseIP: TButton;
    btnCancel: TButton;
    tmrDiscovery: TTimer;

    procedure tmrDiscoveryTimer(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSaveListClick(Sender: TObject);
    procedure btnOpenListClick(Sender: TObject);
    procedure btnUseIPClick(Sender: TObject);
    procedure lvDevicesDblClick(Sender: TObject);
    procedure lvDevicesSelectItem(Sender: TObject; Item: TListItem;
                                  Selected: Boolean);
  private
    FWorker: TLanDiscoveryThread;
    FAddress: string;
    FLastSavedList: string;
    procedure StartDiscovery();
    procedure FinishDiscovery();

  public

    destructor Destroy(); override;
    class function Execute(AOwner: TComponent;
                           var AAddress: string): Boolean;
  end;


  TLanPingThread = class(TThread)
  private
    FSubnets: TStringList;
    FLocalAddresses: TStringList;
    FWorkerIndex: Integer;
    FWorkerCount: Integer;
    FProgressTarget: PInteger;
    FResults: TStringList;
    FFinishedFlag: Integer;

  protected
    procedure Execute(); override;

  public

    constructor Create(const ASubnets: TStringList;
                       const ALocalAddresses: TStringList;
                       const AWorkerIndex: Integer;
                       const AWorkerCount: Integer;
                       const AProgressTarget: PInteger);

    destructor Destroy(); override;

    function IsFinished(): Boolean;
    property Results: TStringList read FResults;
  end;


  //
  function IcmpCreateFile(): THandle; stdcall;
  function IcmpCloseHandle(const AIcmpHandle: THandle): BOOL; stdcall;
  function IcmpSendEcho(const AIcmpHandle: THandle;
                        const ADestinationAddress: ULONG;
                        const ARequestData: Pointer;
                        const ARequestSize: Word;
                        const ARequestOptions: Pointer;
                        const AReplyBuffer: Pointer;
                        const AReplySize: DWORD;
                        const ATimeout: DWORD): DWORD; stdcall;
  function GetIpNetTable(const ATable: Pointer;
                         var ASize: ULONG;
                         const AOrder: BOOL): DWORD; stdcall;
  function GetAdaptersAddresses(const AFamily: ULONG;
                                const AFlags: ULONG;
                                const AReserved: Pointer;
                                const AAdapters: PIP_ADAPTER_ADDRESSES;
                                const ASize: PULONG): ULONG; stdcall;

  function RawSendTo(const ASocket: TSocket;
                     const ABuffer: Pointer;
                     const ALength,
                           AFlags: Integer;
                     const AAddress: Pointer;
                     const AAddressLength: Integer): Integer; stdcall;
  function RawReceiveFrom(const ASocket: TSocket;
                          const ABuffer: Pointer;
                          const ALength,
                                AFlags: Integer;
                          const AAddress: Pointer;
                          const AAddressLength: PInteger): Integer; stdcall;


implementation

uses

  {Cast}
  MfCastTypes,
  MfCastDiscovery,
  {Application}
  MacVendorDb;

{$R *.dfm}

const
  ICMP_TIMEOUT_MS = 40;
  IF_TYPE_ETHERNET_CSMACD = 6;
  IF_TYPE_IEEE80211 = 71;
  IF_OPER_STATUS_UP = 1;
  GAA_FLAG_SKIP_ANYCAST = $0002;
  GAA_FLAG_SKIP_MULTICAST = $0004;
  GAA_FLAG_SKIP_DNS_SERVER = $0008;
  PING_WORKER_COUNT = 32;
  MDNS_PORT = 5353;
  LLMNR_PORT = 5355;
  MDNS_MULTICAST = '224.0.0.251';
  LLMNR_MULTICAST = '224.0.0.252';
  MULTICAST_DISCOVERY_MS = 350;
  GOOGLECAST_SERVICE: AnsiString = '_googlecast._tcp.local';
  GOOGLECAST_DISCOVERY_MS = 1500;
  SSDP_PORT = 1900;
  SSDP_MULTICAST = '239.255.255.250';
  SSDP_DISCOVERY_MS = 1100;
  SSDP_HTTP_TIMEOUT_MS = 450;
  SSDP_MAX_DESCRIPTION_BYTES = 262144;

  iphlpapiLib = 'iphlpapi.dll';
  ws2_32Lib = 'ws2_32.dll';


type

  TIpOptionInformation = packed record
    Ttl: Byte;
    Tos: Byte;
    Flags: Byte;
    OptionsSize: Byte;
    OptionsData: PByte;
  end;

  TIcmpEchoReply = packed record
    Address: ULONG;
    Status: ULONG;
    RoundTripTime: ULONG;
    DataSize: Word;
    Reserved: Word;
    Data: Pointer;
    Options: TIpOptionInformation;
  end;
  PIcmpEchoReply = ^TIcmpEchoReply;

  TMibIpNetRow = record
    Index: DWORD;
    PhysicalAddressLength: DWORD;
    PhysicalAddress: array[0..7] of Byte;
    Address: DWORD;
    EntryType: DWORD;
  end;
  PMibIpNetRow = ^TMibIpNetRow;

  TMdnsIpMreq = packed record
    imr_multiaddr: TInAddr;
    imr_interface: TInAddr;
  end;


function IPv4ToString(const AAddress: TInAddr): string;
begin

  Result := Format('%d.%d.%d.%d',
                   [Integer(AAddress.S_un_b.s_b1),
                    Integer(AAddress.S_un_b.s_b2),
                    Integer(AAddress.S_un_b.s_b3),
                    Integer(AAddress.S_un_b.s_b4)]);
end;


function IsPrivateIPv4(const AAddress: TInAddr): Boolean;
var
  A: Integer;
  B: Integer;

begin

  A := Integer(AAddress.S_un_b.s_b1);
  B := Integer(AAddress.S_un_b.s_b2);

  Result := (A = 10) or
            ((A = 172) and (B >= 16) and (B <= 31)) or
            ((A = 192) and (B = 168));
end;


function SubnetPrefix(const AAddress: TInAddr): string;
begin

  Result := Format('%d.%d.%d',
                   [Integer(AAddress.S_un_b.s_b1),
                    Integer(AAddress.S_un_b.s_b2),
                    Integer(AAddress.S_un_b.s_b3)]);
end;


function ReadNetworkWord(const ABuffer: array of Byte;
                         const AOffset: Integer): Word;
begin

  Result := (Word(ABuffer[AOffset]) shl 8) or
            Word(ABuffer[AOffset + 1]);
end;


function CleanResolvedName(const AName: string): string;
begin

  Result := Trim(AName);

  while (Length(Result) > 0) and
        (Result[Length(Result)] = '.') do
    Delete(Result,
           Length(Result),
           1);
end;


procedure AddUniqueName(const ANames: TStrings;
                        const AName: string);
var
  Candidate: string;
  I: Integer;

begin

  Candidate := CleanResolvedName(AName);

  if (Candidate = '') then
    Exit;

  for I := 0 to ANames.Count - 1 do
    if SameText(ANames[I],
                Candidate) then
      Exit;

  ANames.Add(Candidate);
end;


function JoinNames(const ANames: TStrings): string;
var
  I: Integer;

begin

  Result := '';

  for I := 0 to ANames.Count - 1 do
    begin
      if (Result <> '') then
        Result := Result + '; ';

      Result := Result + ANames[I];
    end;
end;



function IPv4FromReverseName(const AName: string;
                             out AAddress: string): Boolean;
var
  S: string;
  Parts: TStringList;

begin

  Result := False;
  AAddress := '';
  S := LowerCase(CleanResolvedName(AName));

  if (Pos('.in-addr.arpa',
          S) <> Length(S) - 12) then
    Exit;

  Delete(S,
         Length(S) - 12,
         13);

  Parts := TStringList.Create();

  try
    ExtractStrings(['.'],
                   [],
                   PChar(S),
                   Parts);

    if (Parts.Count <> 4) then
      Exit;

    AAddress := Parts[3] + '.' + Parts[2] + '.' + Parts[1] + '.' + Parts[0];
    Result := inet_addr(PAnsiChar(AnsiString(AAddress))) <> $FFFFFFFF;
  finally
    Parts.Free;
  end;
end;


procedure WriteNetworkWord(var ABuffer: array of Byte;
                           const AOffset: Integer;
                           const AValue: Word);
begin

  ABuffer[AOffset] := Byte(AValue shr 8);
  ABuffer[AOffset + 1] := Byte(AValue and $FF);
end;


function EncodeDnsName(const AName: AnsiString;
                       var ABuffer: array of Byte;
                       var APosition: Integer): Boolean;
var
  StartPos: Integer;
  DotPos: Integer;
  LabelText: AnsiString;
  LabelLength: Integer;

begin

  Result := False;
  StartPos := 1;

  while (StartPos <= Length(AName)) do
    begin
      DotPos := StartPos;

      while (DotPos <= Length(AName)) and (AName[DotPos] <> '.') do
        Inc(DotPos);

      LabelText := Copy(AName,
                        StartPos,
                        DotPos - StartPos);

      LabelLength := Length(LabelText);

      if (LabelLength = 0) or (LabelLength > 63) or
         (APosition + 1 + LabelLength >= Length(ABuffer)) then
        Exit;

      ABuffer[APosition] := Byte(LabelLength);
      Inc(APosition);

      Move(LabelText[1],
           ABuffer[APosition],
           LabelLength);

      Inc(APosition,
          LabelLength);
      StartPos := DotPos + 1;
    end;

  if (APosition >= Length(ABuffer)) then
    Exit;

  ABuffer[APosition] := 0;
  Inc(APosition);
  Result := True;
end;


function DecodeDnsName(const ABuffer: array of Byte;
                       const ASize: Integer;
                       var APosition: Integer;
                       out AName: string): Boolean;
var
  PosNow: Integer;
  LengthByte: Integer;
  LabelLength: Integer;
  PointerOffset: Integer;
  Jumped: Boolean;
  Guard: Integer;
  LabelText: AnsiString;

begin

  Result := False;
  AName := '';
  PosNow := APosition;
  Jumped := False;
  Guard := 0;

  while (PosNow < ASize) and (Guard < 64) do
    begin
      Inc(Guard);
      LengthByte := ABuffer[PosNow];

      if (LengthByte = 0) then
        begin
          Inc(PosNow);

          if not Jumped then
            APosition := PosNow;

          Result := True;
          Exit;
        end;

      if ((LengthByte and $C0) = $C0) then
        begin
          if PosNow + 1 >= ASize then
            Exit;

          PointerOffset := ((LengthByte and $3F) shl 8) or ABuffer[PosNow + 1];

          if (PointerOffset >= ASize) then
            Exit;

          if not Jumped then
            APosition := PosNow + 2;

          PosNow := PointerOffset;
          Jumped := True;
          Continue;
        end;

      LabelLength := LengthByte;
      Inc(PosNow);

      if (LabelLength <= 0) or (PosNow + LabelLength > ASize) then
        Exit;

      SetString(LabelText,
                PAnsiChar(@ABuffer[PosNow]),
                LabelLength);

      if (AName <> '') then
        AName := AName + '.';

      AName := AName + string(LabelText);

      Inc(PosNow,
          LabelLength);

      if not Jumped then
        APosition := PosNow;
    end;
end;


procedure ParseDnsNameResponse(const ABuffer: array of Byte;
                               const ASize: Integer;
                               const ANames: TStrings;
                               const AAliases: TStrings);
var
  QuestionCount: Integer;
  AnswerCount: Integer;
  AuthorityCount: Integer;
  AdditionalCount: Integer;
  RecordCount: Integer;
  Position: Integer;
  I: Integer;
  OwnerName: string;
  TargetName: string;
  AddressText: string;
  RecordType: Word;
  DataLength: Integer;
  DataStart: Integer;
  DummyName: string;
  ExistingName: string;

begin

  if (ASize < 12) then
    Exit;

  QuestionCount := ReadNetworkWord(ABuffer, 4);
  AnswerCount := ReadNetworkWord(ABuffer, 6);
  AuthorityCount := ReadNetworkWord(ABuffer, 8);
  AdditionalCount := ReadNetworkWord(ABuffer, 10);
  Position := 12;

  for I := 0 to QuestionCount - 1 do
    begin

      if not DecodeDnsName(ABuffer,
                           ASize,
                           Position,
                           DummyName) then
        Exit;

      if (Position + 4 > ASize) then
        Exit;

      Inc(Position,
          4);
    end;

  RecordCount := AnswerCount + AuthorityCount + AdditionalCount;
  for I := 0 to RecordCount - 1 do
    begin
      if not DecodeDnsName(ABuffer,
                           ASize,
                           Position,
                           OwnerName) then
        Exit;

      if (Position + 10 > ASize) then
        Exit;

      RecordType := ReadNetworkWord(ABuffer,
                                    Position);
      DataLength := ReadNetworkWord(ABuffer,
                                    Position + 8);
      Inc(Position,
          10);

      DataStart := Position;

      if (Position + DataLength > ASize) then
        Exit;

      if (RecordType = $000C) and IPv4FromReverseName(OwnerName,
                                                      AddressText) then
        begin
          if DecodeDnsName(ABuffer,
                           ASize,
                           Position,
                           TargetName) then
            begin
              TargetName := CleanResolvedName(TargetName);

              if (TargetName <> '') then
                begin
                  ExistingName := ANames.Values[AddressText];

                  if (ExistingName = '') then
                    ANames.Values[AddressText] := TargetName
                  else
                    if not SameText(ExistingName,
                                    TargetName) then
                      begin

                        if (AAliases.Values[AddressText] = '') then
                          AAliases.Values[AddressText] := TargetName
                        else
                          if Pos(';' + LowerCase(TargetName) + ';',
                                 ';' + LowerCase(StringReplace(AAliases.Values[AddressText],
                                                               '; ',
                                                               ';',
                                                               [rfReplaceAll])) + ';') = 0 then
                        AAliases.Values[AddressText] := AAliases.Values[AddressText] + '; ' + TargetName;
                    end;
                end;
            end;
        end;
      Position := DataStart + DataLength;
    end;
end;


procedure DiscoverMulticastReverseNames(const AAddresses: TStrings;
                                        const AMulticastAddress: AnsiString;
                                        const APort: Word;
                                        const AWaitMs: Integer;
                                        const ANames,
                                              AAliases: TStrings);
var
  Sock: TSocket;
  Destination: TSockAddrIn;
  Source: TSockAddrIn;
  SourceSize: Integer;
  Query: array[0..511] of Byte;
  Reply: array[0..2047] of Byte;
  Position: Integer;
  I: Integer;
  Address: TInAddr;
  ReverseName: AnsiString;
  TransactionId: Word;
  BytesRead: Integer;
  Timeout: Integer;
  StartTick: DWORD;
  Remaining: Integer;

begin

  if (AAddresses.Count = 0) then
    Exit;

  Sock := socket(AF_INET,
                 SOCK_DGRAM,
                 IPPROTO_UDP);

  if (Sock = INVALID_SOCKET) then
    Exit;

  try
    FillChar(Destination, SizeOf(Destination), 0);
    Destination.sin_family := AF_INET;
    Destination.sin_port := htons(APort);
    Destination.sin_addr.S_addr := inet_addr(PAnsiChar(AMulticastAddress));

    for I := 0 to AAddresses.Count - 1 do
      begin
        FillChar(Query, SizeOf(Query), 0);

        if (APort = MDNS_PORT) then
          TransactionId := 0
        else
          TransactionId := Word((GetTickCount() + DWORD(I * 131)) and $FFFF);

        WriteNetworkWord(Query,
                         0,
                         TransactionId);

        WriteNetworkWord(Query,
                         4,
                         1);
        Position := 12;

        Address.S_addr := inet_addr(PAnsiChar(AnsiString(AAddresses[I])));

        ReverseName := AnsiString(Format('%d.%d.%d.%d.in-addr.arpa',
                                        [Integer(Address.S_un_b.s_b4),
                                         Integer(Address.S_un_b.s_b3),
                                         Integer(Address.S_un_b.s_b2),
                                         Integer(Address.S_un_b.s_b1)]));

        if not EncodeDnsName(ReverseName,
                             Query,
                             Position) then
          Continue;

        WriteNetworkWord(Query,
                         Position,
                         $000C);

        if (APort = MDNS_PORT) then
          WriteNetworkWord(Query,
                           Position + 2,
                           $8001)
        else
          WriteNetworkWord(Query,
                           Position + 2,
                           $0001);

        Inc(Position,
            4);

        RawSendTo(Sock,
                  @Query[0],
                  Position,
                  0,
                  @Destination,
                  SizeOf(Destination));
      end;

    StartTick := GetTickCount();
    repeat
      Remaining := AWaitMs - Integer(GetTickCount() - StartTick);

      if (Remaining <= 0) then
        Break;

      Timeout := Remaining;

      setsockopt(Sock,
                 SOL_SOCKET,
                 SO_RCVTIMEO,
                 PAnsiChar(@Timeout),
                 SizeOf(Timeout));

      SourceSize := SizeOf(Source);
      FillChar(Source,
               SizeOf(Source),
               0);

      BytesRead := RawReceiveFrom(Sock,
                                  @Reply[0],
                                  SizeOf(Reply),
                                  0,
                                  @Source,
                                  @SourceSize);
      if (BytesRead > 0) then
        ParseDnsNameResponse(Reply,
                             BytesRead,
                             ANames,
                             AAliases)
      else
        Break;
    until False;

  finally
    closesocket(Sock);
  end;
end;


function HeaderValue(const AText,
                           AHeader: string): string;
var
  Lines: TStringList;
  I: Integer;
  P: Integer;
  LineText: string;

begin

  Result := '';
  Lines := TStringList.Create();

  try
    Lines.Text := StringReplace(AText,
                                #13#10,
                                #10,
                                [rfReplaceAll]);

    for I := 0 to Lines.Count - 1 do
      begin
        LineText := Trim(Lines[I]);
        P := Pos(':', LineText);

        if (P > 0) and SameText(Trim(Copy(LineText,
                                          1,
                                          P - 1)),
                                AHeader) then
          begin
            Result := Trim(Copy(LineText,
                                P + 1,
                                MaxInt));
            Exit;
          end;
      end;
  finally
    Lines.Free;
  end;
end;


procedure AddAliasValue(const AAliases: TStrings;
                        const AAddress,
                              AAlias: string);
var
  Existing: string;

begin

  if (CleanResolvedName(AAlias) = '') then
    Exit;

  Existing := AAliases.Values[AAddress];

  if (Existing = '') then
    AAliases.Values[AAddress] := CleanResolvedName(AAlias)
  else
    if Pos(';' + LowerCase(CleanResolvedName(AAlias)) + ';',
           ';' + LowerCase(StringReplace(Existing,
                                         '; ',
                                         ';',
                                         [rfReplaceAll])) + ';') = 0 then
    AAliases.Values[AAddress] := Existing + '; ' + CleanResolvedName(AAlias);
end;


function IsGoogleCastServiceInstance(const AName: string): Boolean;
var
  NameText: string;
  Suffix: string;

begin

  NameText := LowerCase(CleanResolvedName(AName));
  Suffix := '.' + LowerCase(string(GOOGLECAST_SERVICE));

  Result := SameText(NameText, string(GOOGLECAST_SERVICE)) or
            ((Length(NameText) > Length(Suffix)) and
             SameText(Copy(NameText,
                           Length(NameText) - Length(Suffix) + 1,
                           MaxInt),
                      Suffix));
end;


procedure ParseGoogleCastResponse(const ABuffer: array of Byte;
                                  const ASize: Integer;
                                  const ASourceAddress: string;
                                  const ANames: TStrings;
                                  const AAliases: TStrings);
var
  QuestionCount: Integer;
  AnswerCount: Integer;
  AuthorityCount: Integer;
  AdditionalCount: Integer;
  RecordCount: Integer;
  Position: Integer;
  I: Integer;
  OwnerName: string;
  RecordType: Word;
  DataLength: Integer;
  DataStart: Integer;
  RecordEnd: Integer;
  DataPosition: Integer;
  DummyName: string;
  ServiceInstance: string;
  HostName: string;
  FriendlyName: string;
  ModelName: string;
  TxtLength: Integer;
  TxtText: AnsiString;
  TxtName: string;
  TxtValue: string;
  EqPos: Integer;
  Port: Word;
  AliasText: string;

begin

  if (ASize < 12) or (ASourceAddress = '') then
    Exit;

  QuestionCount := ReadNetworkWord(ABuffer,
                                   4);

  AnswerCount := ReadNetworkWord(ABuffer,
                                 6);

  AuthorityCount := ReadNetworkWord(ABuffer,
                                    8);

  AdditionalCount := ReadNetworkWord(ABuffer,
                                     10);

  Position := 12;

  for I := 0 to QuestionCount - 1 do
    begin
      if not DecodeDnsName(ABuffer,
                           ASize,
                           Position,
                           DummyName) then
        Exit;

      if (Position + 4 > ASize) then
        Exit;

      Inc(Position,
          4);
    end;

  ServiceInstance := '';
  HostName := '';
  FriendlyName := '';
  ModelName := '';
  Port := 0;
  RecordCount := AnswerCount + AuthorityCount + AdditionalCount;

  for I := 0 to RecordCount - 1 do
    begin
      if not DecodeDnsName(ABuffer,
                           ASize,
                           Position,
                           OwnerName) then
        Exit;

      if (Position + 10 > ASize) then
        Exit;

      RecordType := ReadNetworkWord(ABuffer,
                                    Position);

      DataLength := ReadNetworkWord(ABuffer,
                                    Position + 8);
      Inc(Position,
          10);

      DataStart := Position;
      RecordEnd := DataStart + DataLength;

      if (RecordEnd > ASize) then
        Exit;

      case RecordType of
        $000C:  { PTR }
          begin

            if SameText(CleanResolvedName(OwnerName),
                        string(GOOGLECAST_SERVICE)) then
              begin
                DataPosition := DataStart;

                if DecodeDnsName(ABuffer,
                                 ASize,
                                 DataPosition,
                                 DummyName) and
                   IsGoogleCastServiceInstance(DummyName) then
                  ServiceInstance := DummyName;
              end;
          end;

        $0021:  { SRV }
          begin

            if IsGoogleCastServiceInstance(OwnerName) then
              begin
                if (ServiceInstance = '') then
                  ServiceInstance := OwnerName;

                DataPosition := DataStart + 4;  { priority + weight }

                if (DataPosition + 2 <= RecordEnd) then
                  begin
                    Port := ReadNetworkWord(ABuffer, DataPosition);
                    Inc(DataPosition, 2);

                    if (DataPosition < RecordEnd) then
                      DecodeDnsName(ABuffer,
                                    ASize,
                                    DataPosition,
                                    HostName);
                  end;
              end;
          end;

        $0010:  { TXT }
          begin

            if IsGoogleCastServiceInstance(OwnerName) then
              begin
                if (ServiceInstance = '') then
                  ServiceInstance := OwnerName;

                DataPosition := DataStart;

                while (DataPosition < RecordEnd) do
                  begin
                    TxtLength := ABuffer[DataPosition];
                    Inc(DataPosition);

                    if DataPosition + TxtLength > RecordEnd then
                      Break;

                    SetLength(TxtText, TxtLength);

                    if (TxtLength > 0) then
                      Move(ABuffer[DataPosition],
                           TxtText[1],
                           TxtLength);

                    Inc(DataPosition,
                        TxtLength);

                    EqPos := Pos('=',
                                 string(TxtText));

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

                    if SameText(TxtName,
                                'fn') then
                      FriendlyName := TxtValue
                    else
                      if SameText(TxtName,
                                  'md') then
                        ModelName := TxtValue;
                  end;
              end;
          end;
      end;

      Position := RecordEnd;
    end;

  FriendlyName := CleanResolvedName(FriendlyName);
  HostName := CleanResolvedName(HostName);
  ModelName := CleanResolvedName(ModelName);

  if (FriendlyName <> '') then
    ANames.Values[ASourceAddress] := FriendlyName;

  if (HostName <> '') then
    AddAliasValue(AAliases, ASourceAddress, HostName);

  if (ModelName <> '') then
    begin
      AliasText := 'Chromecast: ' + ModelName;
      AddAliasValue(AAliases, ASourceAddress, AliasText);
    end
  else
    if (ServiceInstance <> '') then
      AddAliasValue(AAliases,
                    ASourceAddress,
                    'Chromecast');

  if (Port <> 0) then
    AddAliasValue(AAliases,
                  ASourceAddress,
                  Format('Cast port %d',
                         [Port]));
end;


procedure DiscoverGoogleCastDevices(const AAddresses,
                                    ALocalAddresses: TStrings;
                                    const ANames,
                                          AAliases: TStrings);
var
  Discovery: TMfCastMdnsDiscovery;
  Protocol: TMfCastProtocolSettings;
  Settings: TMfCastDiscoverySettings;
  Devices: TMfCastDeviceArray;
  Device: TMfCastDevice;
  I: Integer;
  AddressText: string;
  FriendlyName: string;
  HostName: string;
  ModelName: string;
  AliasText: string;
  Hr: HRESULT;

begin

  // Use the actual MfPack Chromecast discovery implementation used by
  // MfRdjPro/MfSimpleCastPlayer instead of maintaining a second mDNS parser here.
  // Start() performs one synchronous discovery refresh and GetDevices()
  // returns the merged PTR/SRV/TXT/A result, including the Chromecast fn= and md= TXT values.
  Protocol.Reset();
  Protocol.DiscoveryServiceName := string(GOOGLECAST_SERVICE);

  Settings.Reset();
  Settings.LocalInterfaceAddress := '';
  Settings.ResponseWindowMs := GOOGLECAST_DISCOVERY_MS;
  Settings.IncludeIPv4 := True;
  Settings.IncludeIPv6 := False;

  Discovery := TMfCastMdnsDiscovery.Create();
  try
    Hr := Discovery.Configure(Protocol,
                              Settings);
    if FAILED(Hr) then
      Exit;

    Hr := Discovery.Start();
    if FAILED(Hr) then
      Exit;

    Hr := Discovery.GetDevices(Devices);
    if FAILED(Hr) then
      Exit;

    for I := 0 to Length(Devices) - 1 do
      begin
        Device := Devices[I];
        AddressText := Trim(Device.Address);

        if (AddressText = '') or
           (AAddresses.IndexOf(AddressText) < 0) then
          Continue;

        FriendlyName := CleanResolvedName(Device.FriendlyName);
        HostName := CleanResolvedName(Device.HostName);
        ModelName := CleanResolvedName(Device.ModelName);

        if (FriendlyName <> '') then
          ANames.Values[AddressText] := FriendlyName
        else
          if (HostName <> '') then
            ANames.Values[AddressText] := HostName;

        if (HostName <> '') and
           not SameText(HostName, FriendlyName) then
          AddAliasValue(AAliases,
                        AddressText,
                        HostName);

        if (ModelName <> '') then
          begin
            AliasText := 'Chromecast: ' + ModelName;
            AddAliasValue(AAliases,
                          AddressText,
                          AliasText);
          end
        else
          AddAliasValue(AAliases,
                        AddressText,
                        'Chromecast');

        if (Device.Port <> 0) then
          AddAliasValue(AAliases,
                        AddressText,
                        Format('Cast port %d',
                               [Device.Port]));
      end;
  finally
    Discovery.Stop();
    Discovery.Free();
  end;
end;


function FormatMacAddress(const ARow: TMibIpNetRow): string;
var
  I: Integer;

begin

  Result := '';

  if (ARow.PhysicalAddressLength < 6) then
    Exit;

  for I := 0 to 5 do
    begin
      if (Result <> '') then
        Result := Result + '-';
      Result := Result + IntToHex(ARow.PhysicalAddress[I], 2);
    end;
end;


function IsLocallyAdministeredMac(const AMacAddress: string): Boolean;
var
  FirstOctet: Integer;

begin

  Result := False;

  if (Length(AMacAddress) < 2) then
    Exit;

  FirstOctet := StrToIntDef('$' + Copy(AMacAddress,
                                       1,
                                       2),
                            -1);

  Result := (FirstOctet >= 0) and ((FirstOctet and $02) <> 0);
end;


function ExtractXmlTagText(const AXml: string;
                           const ATagName: string): string;
var
  LowerXml: string;
  LowerTag: string;
  StartPos: Integer;
  OpenEnd: Integer;
  EndPos: Integer;
  CloseTag: string;

begin

  Result := '';
  LowerXml := LowerCase(AXml);
  LowerTag := LowerCase(ATagName);
  StartPos := Pos('<' + LowerTag,
                  LowerXml);

  if (StartPos = 0) then
    Exit;

  OpenEnd := PosEx('>',
                   LowerXml,
                   StartPos);

  if (OpenEnd = 0) then
    Exit;

  CloseTag := '</' + LowerTag + '>';
  EndPos := PosEx(CloseTag,
                  LowerXml,
                  OpenEnd + 1);

  if (EndPos = 0) then
    Exit;

  Result := Trim(Copy(AXml,
                      OpenEnd + 1,
                      EndPos - OpenEnd - 1));

  Result := StringReplace(Result,
                          '&amp;',
                          '&',
                          [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result,
                          '&lt;',
                          '<',
                          [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result,
                          '&gt;',
                          '>',
                          [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result,
                          '&quot;',
                          '"',
                          [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result,
                          '&apos;',
                          '''',
                          [rfReplaceAll, rfIgnoreCase]);
end;


function DownloadLanText(const AUrl: string;
                         out AText: string): Boolean;
var
  InternetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[0..4095] of Byte;
  BytesRead: DWORD;
  TotalBytes: Integer;
  Data: TBytes;
  Timeout: DWORD;
  ChunkStart: Integer;

begin

  Result := False;
  AText := '';

  if (Pos('http://', LowerCase(AUrl)) <> 1) and
     (Pos('https://', LowerCase(AUrl)) <> 1) then
    Exit;

  InternetHandle := InternetOpen('CaddyAdmin LAN discovery',
                                 INTERNET_OPEN_TYPE_PRECONFIG,
                                 nil,
                                 nil,
                                 0);

  if not Assigned(InternetHandle) then
    Exit;

  try
    Timeout := SSDP_HTTP_TIMEOUT_MS;

    InternetSetOption(InternetHandle,
                      INTERNET_OPTION_CONNECT_TIMEOUT,
                      @Timeout,
                      SizeOf(Timeout));

    InternetSetOption(InternetHandle,
                      INTERNET_OPTION_SEND_TIMEOUT,
                      @Timeout,
                      SizeOf(Timeout));

    InternetSetOption(InternetHandle,
                      INTERNET_OPTION_RECEIVE_TIMEOUT,
                      @Timeout,
                      SizeOf(Timeout));

    UrlHandle := InternetOpenUrl(InternetHandle,
                                 PChar(AUrl),
                                 nil,
                                 0,
                                 INTERNET_FLAG_RELOAD or
                                 INTERNET_FLAG_NO_CACHE_WRITE,
                                 0);

    if not Assigned(UrlHandle) then
      Exit;

    try
      SetLength(Data, 0);
      TotalBytes := 0;

      repeat
        BytesRead := 0;
        if not InternetReadFile(UrlHandle,
                                @Buffer[0],
                                SizeOf(Buffer),
                                BytesRead) then
          Exit;

        if BytesRead = 0 then
          Break;

        if TotalBytes + Integer(BytesRead) > SSDP_MAX_DESCRIPTION_BYTES then
          Exit;

        ChunkStart := Length(Data);

        SetLength(Data,
                  ChunkStart + Integer(BytesRead));

        Move(Buffer[0],
             Data[ChunkStart],
             BytesRead);

        Inc(TotalBytes,
            Integer(BytesRead));
      until False;

      if (TotalBytes = 0) then
        Exit;

      AText := TEncoding.UTF8.GetString(Data);
      Result := True;
    finally
      InternetCloseHandle(UrlHandle);
    end;
  finally
    InternetCloseHandle(InternetHandle);
  end;
end;


procedure ApplySsdpDeviceDescription(const AAddress,
                                           AXml: string;
                                     const ANames,
                                           AAliases,
                                           AManufacturers: TStrings);
var
  FriendlyName: string;
  Manufacturer: string;
  ModelName: string;
  ModelNumber: string;
  DeviceType: string;
  ModelText: string;

begin

  FriendlyName := ExtractXmlTagText(AXml, 'friendlyName');
  Manufacturer := ExtractXmlTagText(AXml, 'manufacturer');
  ModelName := ExtractXmlTagText(AXml, 'modelName');
  ModelNumber := ExtractXmlTagText(AXml, 'modelNumber');
  DeviceType := ExtractXmlTagText(AXml, 'deviceType');

  if (FriendlyName <> '') and (ANames.Values[AAddress] = '') then
    ANames.Values[AAddress] := FriendlyName;

  if (Manufacturer <> '') then
    AManufacturers.Values[AAddress] := Manufacturer;

  ModelText := ModelName;

  if (ModelNumber <> '') then
    begin
      if (ModelText <> '') then
        ModelText := ModelText + ' ';

      ModelText := ModelText + ModelNumber;
    end;

  if (ModelText <> '') then
    AddAliasValue(AAliases,
                  AAddress,
                  ModelText);

  if (FriendlyName <> '') and
     not SameText(FriendlyName,
                  ANames.Values[AAddress]) then
    AddAliasValue(AAliases,
                  AAddress,
                  FriendlyName);

  if (DeviceType <> '') then
    AddAliasValue(AAliases,
                  AAddress,
                  DeviceType);
end;


procedure DiscoverSsdpAliases(const AAddresses: TStrings;
                              const ANames,
                                    AAliases,
                                    AManufacturers: TStrings);
const
  REQUEST_TEXT: AnsiString = 'M-SEARCH * HTTP/1.1'#13#10 +
                             'HOST: 239.255.255.250:1900'#13#10 +
                             'MAN: "ssdp:discover"'#13#10 +
                             'MX: 1'#13#10 +
                             'ST: ssdp:all'#13#10#13#10;
var
  Sock: TSocket;
  Destination: TSockAddrIn;
  Source: TSockAddrIn;
  SourceSize: Integer;
  Reply: array[0..4095] of AnsiChar;
  BytesRead: Integer;
  Timeout: Integer;
  StartTick: DWORD;
  Remaining: Integer;
  AddressText: string;
  ResponseText: string;
  ResponseAnsi: AnsiString;
  ServerText: string;
  StText: string;
  LocationText: string;
  Locations: TStringList;
  I: Integer;
  DescriptionText: string;

begin

  Locations := TStringList.Create();
  Locations.NameValueSeparator := '=';
  Sock := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

  if (Sock = INVALID_SOCKET) then
    begin
      Locations.Free;
      Exit;
    end;

  try
    FillChar(Destination, SizeOf(Destination), 0);
    Destination.sin_family := AF_INET;
    Destination.sin_port := htons(SSDP_PORT);
    Destination.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(SSDP_MULTICAST)));

    RawSendTo(Sock,
              PAnsiChar(REQUEST_TEXT),
              Length(REQUEST_TEXT),
              0,
              @Destination,
              SizeOf(Destination));

    StartTick := GetTickCount();

    repeat
      Remaining := SSDP_DISCOVERY_MS - Integer(GetTickCount() - StartTick);

      if (Remaining <= 0) then
        Break;

      Timeout := Remaining;

      setsockopt(Sock,
                 SOL_SOCKET,
                 SO_RCVTIMEO,
                 PAnsiChar(@Timeout),
                 SizeOf(Timeout));

      FillChar(Reply,
               SizeOf(Reply),
               0);

      SourceSize := SizeOf(Source);

      FillChar(Source,
               SizeOf(Source),
               0);

      BytesRead := RawReceiveFrom(Sock,
                                  @Reply[0],
                                  SizeOf(Reply) - 1,
                                  0,
                                  @Source,
                                  @SourceSize);
      if (BytesRead <= 0) then
        Break;

      Reply[BytesRead] := #0;
      AddressText := IPv4ToString(Source.sin_addr);

      if (AAddresses.IndexOf(AddressText) >= 0) then
        begin
          SetString(ResponseAnsi,
                    PAnsiChar(@Reply[0]),
                    BytesRead);

          ResponseText := string(ResponseAnsi);

          ServerText := HeaderValue(ResponseText,
                                    'SERVER');

          StText := HeaderValue(ResponseText,
                                'ST');

          LocationText := HeaderValue(ResponseText,
                                      'LOCATION');

          if (LocationText <> '') and (Locations.Values[AddressText] = '') then
            Locations.Values[AddressText] := LocationText;

          if (ServerText <> '') then
            AddAliasValue(AAliases,
                          AddressText,
                          ServerText)
          else
            if StText <> '' then
              AddAliasValue(AAliases,
                            AddressText,
                            StText);
        end;
    until False;
  finally
    closesocket(Sock);
  end;

  try
    for I := 0 to Locations.Count - 1 do
      begin
        AddressText := Locations.Names[I];
        LocationText := Locations.ValueFromIndex[I];

        if DownloadLanText(LocationText,
                           DescriptionText) then
          ApplySsdpDeviceDescription(AddressText,
                                     DescriptionText,
                                     ANames,
                                     AAliases,
                                     AManufacturers);
      end;
  finally
    Locations.Free;
  end;
end;


procedure DiscoverNetBiosNames(const AAddresses: TStrings;
                                const ANames,
                                      AAliases: TStrings);
const
  NETBIOS_PORT = 137;
  NETBIOS_DISCOVERY_MS = 220;

var
  Sock: TSocket;
  Destination: TSockAddrIn;
  Source: TSockAddrIn;
  SourceSize: Integer;
  Timeout: Integer;
  Query: array[0..49] of Byte;
  Reply: array[0..1023] of Byte;
  TransactionId: Word;
  I: Integer;
  RawNameByte: Byte;
  BytesRead: Integer;
  Position: Integer;
  LabelLength: Integer;
  DataLength: Integer;
  DataOffset: Integer;
  NameCount: Integer;
  NameOffset: Integer;
  NameSuffix: Byte;
  NameFlags: Word;
  Candidate: AnsiString;
  AddressText: string;
  ExistingName: string;
  StartTick: DWORD;
  Remaining: Integer;

begin

  if (AAddresses.Count = 0) then
    Exit;

  Sock := socket(AF_INET,
                 SOCK_DGRAM,
                 IPPROTO_UDP);

  if (Sock = INVALID_SOCKET) then
    Exit;

  try
    for I := 0 to AAddresses.Count - 1 do
      begin
        FillChar(Query,
                 SizeOf(Query),
                 0);

        TransactionId := Word((GetTickCount() + DWORD(I * 977)) and $FFFF);
        Query[0] := Byte(TransactionId shr 8);
        Query[1] := Byte(TransactionId and $FF);
        Query[5] := 1;
        Query[12] := 32;

        for Position := 0 to 15 do
          begin
            if (Position = 0) then
              RawNameByte := Ord('*')
            else
              RawNameByte := 0;

            Query[13 + (Position * 2)] := Ord('A') + (RawNameByte shr 4);
            Query[14 + (Position * 2)] := Ord('A') + (RawNameByte and $0F);
          end;

        Query[45] := 0;
        Query[46] := 0;
        Query[47] := $21;
        Query[48] := 0;
        Query[49] := 1;

        FillChar(Destination,
                 SizeOf(Destination),
                 0);

        Destination.sin_family := AF_INET;
        Destination.sin_port := htons(NETBIOS_PORT);
        Destination.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(AAddresses[I])));

        RawSendTo(Sock,
                  @Query[0],
                  SizeOf(Query),
                  0,
                  @Destination,
                  SizeOf(Destination));
      end;

    StartTick := GetTickCount();

    repeat
      Remaining := NETBIOS_DISCOVERY_MS - Integer(GetTickCount() - StartTick);

      if (Remaining <= 0) then
        Break;

      Timeout := Remaining;
      setsockopt(Sock,
                 SOL_SOCKET,
                 SO_RCVTIMEO,
                 PAnsiChar(@Timeout),
                 SizeOf(Timeout));

      SourceSize := SizeOf(Source);

      FillChar(Source,
               SizeOf(Source),
               0);

      BytesRead := RawReceiveFrom(Sock,
                                  @Reply[0],
                                  SizeOf(Reply),
                                  0,
                                  @Source,
                                  @SourceSize);
      if (BytesRead <= 0) then
        Break;

      if (BytesRead < 63) then
        Continue;

      AddressText := IPv4ToString(Source.sin_addr);
      if (AAddresses.IndexOf(AddressText) < 0) then
        Continue;

      Position := 50;

      if (Position >= BytesRead) then
        Continue;

      if (Reply[Position] and $C0) = $C0 then
        Inc(Position,
            2)
      else
        begin
          while (Position < BytesRead) and (Reply[Position] <> 0) do
            begin
              LabelLength := Reply[Position];
              Inc(Position,
                  LabelLength + 1);
            end;
          Inc(Position);
        end;

      if (Position + 10 > BytesRead) or
         (ReadNetworkWord(Reply,
                          Position) <> $21) then
        Continue;

      DataLength := ReadNetworkWord(Reply,
                                    Position + 8);
      DataOffset := Position + 10;

      if (DataOffset + DataLength > BytesRead) or (DataLength < 1) then
        Continue;

      NameCount := Reply[DataOffset];

      for I := 0 to NameCount - 1 do
        begin
          NameOffset := DataOffset + 1 + (I * 18);

          if (NameOffset + 18 > BytesRead) then
            Break;

          NameSuffix := Reply[NameOffset + 15];
          NameFlags := ReadNetworkWord(Reply,
                                       NameOffset + 16);

          if (NameSuffix in [$00, $20]) and ((NameFlags and $8000) = 0) then
            begin
              SetString(Candidate,
                        PAnsiChar(@Reply[NameOffset]),
                        15);

              Candidate := AnsiString(Trim(string(Candidate)));

              if (Candidate <> '') then
                begin
                  ExistingName := ANames.Values[AddressText];

                  if (ExistingName = '') then
                    ANames.Values[AddressText] := string(Candidate)
                  else
                    if not SameText(ExistingName,
                                    string(Candidate)) then
                      AddAliasValue(AAliases,
                                    AddressText,
                                    string(Candidate));
                end;
            end;
        end;
    until False;
  finally
    closesocket(Sock);
  end;
end;



// TLanPingThread ==============================================================

constructor TLanPingThread.Create(const ASubnets: TStringList;
                                  const ALocalAddresses: TStringList;
                                  const AWorkerIndex: Integer;
                                  const AWorkerCount: Integer;
                                  const AProgressTarget: PInteger);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FSubnets := ASubnets;
  FLocalAddresses := ALocalAddresses;
  FWorkerIndex := AWorkerIndex;
  FWorkerCount := AWorkerCount;
  FProgressTarget := AProgressTarget;
  FResults := TStringList.Create();
  FFinishedFlag := 0;
end;


destructor TLanPingThread.Destroy();
begin

  FResults.Free;

  inherited Destroy();
end;


function TLanPingThread.IsFinished(): Boolean;
begin

  Result := InterlockedCompareExchange(FFinishedFlag,
                                       0,
                                       0) <> 0;
end;


procedure TLanPingThread.Execute();
var
  IcmpHandle: THandle;
  ReplyBuffer: array[0..255] of Byte;
  TargetNumber: Integer;
  TotalTargets: Integer;
  SubnetIndex: Integer;
  HostIndex: Integer;
  IpText: string;
  TargetText: AnsiString;
  TargetAddress: ULONG;
  ReplyCount: DWORD;

begin

  IcmpHandle := INVALID_HANDLE_VALUE;

  try
    IcmpHandle := IcmpCreateFile();
    if (IcmpHandle = INVALID_HANDLE_VALUE) then
      Exit;

    TotalTargets := FSubnets.Count * 254;
    TargetNumber := FWorkerIndex;

    while (TargetNumber < TotalTargets) do
      begin
        if Terminated then
          Exit;

        SubnetIndex := TargetNumber div 254;
        HostIndex := (TargetNumber mod 254) + 1;
        IpText := FSubnets[SubnetIndex] + '.' + IntToStr(HostIndex);

        if (FLocalAddresses.IndexOf(IpText) < 0) then
          begin
            TargetText := AnsiString(IpText);
            TargetAddress := inet_addr(PAnsiChar(TargetText));
            FillChar(ReplyBuffer, SizeOf(ReplyBuffer), 0);

            ReplyCount := IcmpSendEcho(IcmpHandle,
                                       TargetAddress,
                                       nil,
                                       0,
                                       nil,
                                       @ReplyBuffer[0],
                                       SizeOf(ReplyBuffer),
                                       ICMP_TIMEOUT_MS);
            if (ReplyCount > 0) and
               (PIcmpEchoReply(@ReplyBuffer[0])^.Status = 0) then
              FResults.Add(IpText);
          end;

        InterlockedIncrement(FProgressTarget^);
        Inc(TargetNumber,
            FWorkerCount);
      end;
  finally
    if (IcmpHandle <> INVALID_HANDLE_VALUE) then
      IcmpCloseHandle(IcmpHandle);

    InterlockedExchange(FFinishedFlag, 1);
  end;
end;

// =============================================================================

constructor TLanDiscoveryThread.Create();
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FResults := TStringList.Create();
  FResults.NameValueSeparator := '=';
  FAliases := TStringList.Create();
  FAliases.NameValueSeparator := '=';
  FMacAddresses := TStringList.Create();
  FMacAddresses.NameValueSeparator := '=';
  FManufacturers := TStringList.Create();
  FManufacturers.NameValueSeparator := '=';
  FProgress := 0;
  FTotal := 0;
  FFinishedFlag := 0;
end;


destructor TLanDiscoveryThread.Destroy();
begin

  FManufacturers.Free;
  FMacAddresses.Free;
  FAliases.Free;
  FResults.Free;

  inherited Destroy();
end;


procedure TLanDiscoveryThread.AddAddress(const AAddress: string);
begin

  if (AAddress <> '') and (FResults.IndexOfName(AAddress) < 0) then
    FResults.Add(AAddress + '=');
end;


function TLanDiscoveryThread.IsFinished(): Boolean;
begin

  Result := InterlockedCompareExchange(FFinishedFlag,
                                       0,
                                       0) <> 0;
end;


function TLanDiscoveryThread.Progress(): Integer;
begin

  Result := InterlockedCompareExchange(FProgress,
                                       0,
                                       0);
end;


function TLanDiscoveryThread.Total(): Integer;
begin

  Result := InterlockedCompareExchange(FTotal,
                                       0,
                                       0);
end;


procedure TLanDiscoveryThread.Execute();
var
  WsaData: TWSAData;
  AdapterBuffer: PIP_ADAPTER_ADDRESSES;
  AdapterBufferSize: ULONG;
  Adapter: PIP_ADAPTER_ADDRESSES;
  UnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
  SocketAddress: PSockAddrIn;
  Address: TInAddr;
  LocalAddresses: TStringList;
  Subnets: TStringList;
  TableBuffer: Pointer;
  TableSize: ULONG;
  TableCount: DWORD;
  Row: PMibIpNetRow;
  I: Integer;
  IpText: string;
  Prefix: string;
  ErrorCode: DWORD;
  PingWorkers: array of TLanPingThread;
  WorkerCount: Integer;
  WorkerIndex: Integer;
  AllWorkersFinished: Boolean;
  AddressList: TStringList;
  MulticastNames: TStringList;
  MulticastAliases: TStringList;
  ExistingAlias: string;
  VendorDb: TMacVendorDatabase;
  VendorName: string;

begin

  LocalAddresses := TStringList.Create();
  Subnets := TStringList.Create();
  TableBuffer := nil;
  AdapterBuffer := nil;
  VendorDb := TMacVendorDatabase.Create();

  if VendorDb.LoadNearExecutable() then
    OutputDebugString(PChar(Format('[LAN VendorDb] loaded %d entries from %s',
                                   [VendorDb.EntryCount(),
                                    VendorDb.LoadedFileName])))
  else
    OutputDebugString(PChar('[LAN VendorDb] no usable vendor database found'));
  try
    try
      if WSAStartup($0202, WsaData) <> 0 then
        begin
          FErrorText := 'Windows networking could not be initialized.';
          Exit;
        end;

      try
        AdapterBufferSize := 0;
        ErrorCode := GetAdaptersAddresses(AF_INET,
                                          GAA_FLAG_SKIP_ANYCAST or
                                          GAA_FLAG_SKIP_MULTICAST or
                                          GAA_FLAG_SKIP_DNS_SERVER,
                                          nil,
                                          nil,
                                          @AdapterBufferSize);
        if (ErrorCode <> ERROR_BUFFER_OVERFLOW) or
           (AdapterBufferSize = 0) then
          begin
            FErrorText := 'No local IPv4 network adapter was found.';
            Exit;
          end;

        GetMem(AdapterBuffer, AdapterBufferSize);
        ErrorCode := GetAdaptersAddresses(AF_INET,
                                          GAA_FLAG_SKIP_ANYCAST or
                                          GAA_FLAG_SKIP_MULTICAST or
                                          GAA_FLAG_SKIP_DNS_SERVER,
                                          nil,
                                          AdapterBuffer,
                                          @AdapterBufferSize);
        if ErrorCode <> NO_ERROR then
          begin
            FErrorText := Format('Local network adapters could not be enumerated (error %d).',
                                 [ErrorCode]);
            Exit;
          end;

        Adapter := AdapterBuffer;

        while Assigned(Adapter) do
          begin
            if (Ord(Adapter^.OperStatus) = IF_OPER_STATUS_UP) and
               ((Adapter^.IfType = IF_TYPE_ETHERNET_CSMACD) or
                (Adapter^.IfType = IF_TYPE_IEEE80211)) then
              begin
                UnicastAddress := Adapter^.FirstUnicastAddress;

                while Assigned(UnicastAddress) do
                  begin
                    if Assigned(UnicastAddress^.Address.lpSockaddr) and
                       (UnicastAddress^.Address.lpSockaddr^.sa_family = AF_INET) then
                      begin
                        SocketAddress := PSockAddrIn(UnicastAddress^.Address.lpSockaddr);
                        Address := SocketAddress^.sin_addr;

                        if IsPrivateIPv4(Address) then
                          begin
                            IpText := IPv4ToString(Address);

                            if (LocalAddresses.IndexOf(IpText) < 0) then
                              LocalAddresses.Add(IpText);

                            Prefix := SubnetPrefix(Address);

                            if (Subnets.IndexOf(Prefix) < 0) then
                              Subnets.Add(Prefix);
                          end;
                      end;
                    UnicastAddress := UnicastAddress^.Next;
                  end;
              end;
            Adapter := Adapter^.Next;
          end;

        if (Subnets.Count = 0) then
          begin
            FErrorText := 'No active private IPv4 network was found.';
            Exit;
          end;

        InterlockedExchange(FTotal,
                            Subnets.Count * 254);
        WorkerCount := PING_WORKER_COUNT;

        if (WorkerCount > FTotal) then
          WorkerCount := FTotal;

        SetLength(PingWorkers,
                  WorkerCount);

        try
          for WorkerIndex := 0 to WorkerCount - 1 do
            begin
              PingWorkers[WorkerIndex] := TLanPingThread.Create(Subnets,
                                                                LocalAddresses,
                                                                WorkerIndex,
                                                                WorkerCount,
                                                                @FProgress);
              PingWorkers[WorkerIndex].Start();
            end;

          repeat
            AllWorkersFinished := True;

            for WorkerIndex := 0 to WorkerCount - 1 do
              begin
                if Terminated then
                  PingWorkers[WorkerIndex].Terminate();

                if not PingWorkers[WorkerIndex].IsFinished() then
                  AllWorkersFinished := False;
              end;

            if not AllWorkersFinished then
              Sleep(20);
          until AllWorkersFinished;

          for WorkerIndex := 0 to WorkerCount - 1 do
            begin
              PingWorkers[WorkerIndex].WaitFor();

              for I := 0 to PingWorkers[WorkerIndex].Results.Count - 1 do
                AddAddress(PingWorkers[WorkerIndex].Results[I]);

              PingWorkers[WorkerIndex].Free;
              PingWorkers[WorkerIndex] := nil;
            end;
        finally

          // Also clean up workers when creation, startup, or discovery raises.
          for WorkerIndex := 0 to Length(PingWorkers) - 1 do
            if Assigned(PingWorkers[WorkerIndex]) then
              PingWorkers[WorkerIndex].Terminate();

          for WorkerIndex := 0 to Length(PingWorkers) - 1 do
            if Assigned(PingWorkers[WorkerIndex]) then
              begin
                PingWorkers[WorkerIndex].WaitFor();
                PingWorkers[WorkerIndex].Free;
                PingWorkers[WorkerIndex] := nil;
              end;

          SetLength(PingWorkers,
                    0);
        end;

        if Terminated then
          Exit;

        { Include local-layer neighbors that answered ARP but block ICMP echo. }
        TableSize := 0;
        ErrorCode := GetIpNetTable(nil, TableSize, False);

        if (ErrorCode = ERROR_INSUFFICIENT_BUFFER) and (TableSize > 0) then
          begin
            GetMem(TableBuffer, TableSize);

            if GetIpNetTable(TableBuffer,
                             TableSize,
                             True) = NO_ERROR then
              begin
                TableCount := PDWORD(TableBuffer)^;

                for I := 0 to Integer(TableCount) - 1 do
                  begin
                    Row := PMibIpNetRow(PByte(TableBuffer) +
                                        SizeOf(DWORD) +
                                        (I * SizeOf(TMibIpNetRow)));

                    if (Row^.PhysicalAddressLength >= 6) and
                       (Row^.EntryType in [3, 4]) and
                       ((Row^.PhysicalAddress[0] and $01) = 0) then
                      begin
                        Address.S_addr := Row^.Address;

                        if IsPrivateIPv4(Address) and
                           (Subnets.IndexOf(SubnetPrefix(Address)) >= 0) then
                          begin
                            IpText := IPv4ToString(Address);

                            if (LocalAddresses.IndexOf(IpText) < 0) then
                              begin
                                AddAddress(IpText);
                                FMacAddresses.Values[IpText] := FormatMacAddress(Row^);

                                if IsLocallyAdministeredMac(FMacAddresses.Values[IpText]) and
                                   (FManufacturers.Values[IpText] = '') then
                                  FManufacturers.Values[IpText] := 'Locally administered MAC';
                              end;
                          end;
                      end;
                  end;
              end;
          end;

        InterlockedExchange(FTotal,
                            (Subnets.Count * 254) + 5);

        AddressList := TStringList.Create();
        MulticastNames := TStringList.Create();
        MulticastAliases := TStringList.Create();

        try
          MulticastNames.NameValueSeparator := '=';
          MulticastAliases.NameValueSeparator := '=';

          for I := 0 to FResults.Count - 1 do
            AddressList.Add(FResults.Names[I]);

          { Query modern local-name protocols directly.  These calls use raw
            UDP multicast and never enter Windows' namespace-provider chain. }
          DiscoverMulticastReverseNames(AddressList,
                                        AnsiString(MDNS_MULTICAST),
                                        MDNS_PORT,
                                        MULTICAST_DISCOVERY_MS,
                                        MulticastNames,
                                        MulticastAliases);
          InterlockedIncrement(FProgress);
          if Terminated then
            Exit;

          // Chromecast publishes its user-visible device name in the TXT
          // record of _googlecast._tcp.local (fn=...), not in a reverse PTR reply.
          // Query that DNS-SD service explicitly, just like MfPack's cast discovery unit does.
          DiscoverGoogleCastDevices(AddressList,
                                    LocalAddresses,
                                    MulticastNames,
                                    MulticastAliases);

          InterlockedIncrement(FProgress);

          if Terminated then
            Exit;

          DiscoverMulticastReverseNames(AddressList,
                                        AnsiString(LLMNR_MULTICAST),
                                        LLMNR_PORT,
                                        MULTICAST_DISCOVERY_MS,
                                        MulticastNames,
                                        MulticastAliases);

          InterlockedIncrement(FProgress);

          if Terminated then
            Exit;

          DiscoverSsdpAliases(AddressList,
                              MulticastNames,
                              MulticastAliases,
                              FManufacturers);

          InterlockedIncrement(FProgress);

          if Terminated then
            Exit;

          { One batched NetBIOS pass for the whole LAN.  This avoids creating
            and timing out one socket per device. }
          DiscoverNetBiosNames(AddressList,
                               MulticastNames,
                               MulticastAliases);

          InterlockedIncrement(FProgress);

          for I := 0 to FResults.Count - 1 do
            begin
              if Terminated then
                Exit;

              IpText := FResults.Names[I];

              if (MulticastNames.Values[IpText] <> '') then
                FResults.ValueFromIndex[I] := MulticastNames.Values[IpText];

              ExistingAlias := MulticastAliases.Values[IpText];

              if (ExistingAlias <> '') then
                FAliases.Values[IpText] := ExistingAlias;

              { UPnP/SSDP manufacturer data has priority because it describes
                the actual device.  If the device did not publish that field,
                fall back to the offline IEEE MAC allocation database. }
              if (FManufacturers.Values[IpText] = '') and
                 (FMacAddresses.Values[IpText] <> '') and not
                 IsLocallyAdministeredMac(FMacAddresses.Values[IpText]) then
                begin
                  VendorName := VendorDb.Lookup(FMacAddresses.Values[IpText]);

                  if (VendorName <> '') then
                    FManufacturers.Values[IpText] := VendorName;
                end;
            end;
        finally
          MulticastAliases.Free;
          MulticastNames.Free;
          AddressList.Free;
        end;

      finally
        WSACleanup();
      end;
    except
      on E: Exception do
        FErrorText := E.Message;
    end;
  finally
    if Assigned(TableBuffer) then
      FreeMem(TableBuffer);

    if Assigned(AdapterBuffer) then
      FreeMem(AdapterBuffer);

    VendorDb.Free();
    Subnets.Free;
    LocalAddresses.Free;
    InterlockedExchange(FFinishedFlag,
                        1);
  end;
end;


destructor TLanDiscoveryDialog.Destroy();
begin

  tmrDiscovery.Enabled := False;

  if Assigned(FWorker) then
    begin
      FWorker.Terminate();
      FWorker.WaitFor();
      FWorker.Free;
    end;

  inherited Destroy();
end;


class function TLanDiscoveryDialog.Execute(AOwner: TComponent;
                                           var AAddress: string): Boolean;
var
  Dialog: TLanDiscoveryDialog;

begin

  Dialog := TLanDiscoveryDialog.Create(AOwner);

  try
    Dialog.StartDiscovery();
    Result := (Dialog.ShowModal() = mrOK);

    if Result then
      AAddress := Dialog.FAddress;
  finally
    Dialog.Free;
  end;
end;


procedure TLanDiscoveryDialog.StartDiscovery();
begin

  if Assigned(FWorker) then
    begin
      FWorker.Terminate();
      FWorker.WaitFor();
      FWorker.Free;
    end;

  lvDevices.Items.Clear();
  btnUseIP.Enabled := False;
  btnSaveList.Enabled := False;
  btnOpenList.Enabled := (FLastSavedList <> '') and FileExists(FLastSavedList);
  btnRefresh.Enabled := False;
  lblStatus.Caption := 'Searching LAN IPv4 devices...';
  pbDiscovery.Position := 0;
  FWorker := TLanDiscoveryThread.Create();
  tmrDiscovery.Enabled := True;
  FWorker.Start();
end;


procedure TLanDiscoveryDialog.FinishDiscovery();
var
  I: Integer;
  Item: TListItem;
  DeviceName: string;
  AliasNames: string;
  MacAddress: string;
  Manufacturer: string;

begin

  tmrDiscovery.Enabled := False;
  lvDevices.Items.BeginUpdate();

  try
    for I := 0 to FWorker.Results.Count - 1 do
      begin
        Item := lvDevices.Items.Add();
        DeviceName := FWorker.Results.ValueFromIndex[I];

        if (DeviceName = '') then
          DeviceName := '(name unavailable)';

        Item.Caption := DeviceName;
        AliasNames := FWorker.Aliases.Values[FWorker.Results.Names[I]];
        Item.SubItems.Add(AliasNames);
        Item.SubItems.Add(FWorker.Results.Names[I]);
        MacAddress := FWorker.MacAddresses.Values[FWorker.Results.Names[I]];
        Manufacturer := FWorker.Manufacturers.Values[FWorker.Results.Names[I]];
        Item.SubItems.Add(MacAddress);
        Item.SubItems.Add(Manufacturer);
      end;
  finally
    lvDevices.Items.EndUpdate();
  end;

  if (FWorker.ErrorText <> '') then
    lblStatus.Caption := FWorker.ErrorText
  else
    lblStatus.Caption := Format('%d LAN device(s) found. Names use mDNS/DNS-SD (including Chromecast), LLMNR and NetBIOS; SSDP/UPnP adds friendly names/model data; the offline MAC database supplies vendor names when available.',
                                [lvDevices.Items.Count]);

  pbDiscovery.Position := pbDiscovery.Max;
  btnRefresh.Enabled := True;
  btnSaveList.Enabled := (lvDevices.Items.Count > 0);
end;


procedure TLanDiscoveryDialog.tmrDiscoveryTimer(Sender: TObject);
var
  TotalCount: Integer;

begin

  if not Assigned(FWorker) then
    Exit;

  TotalCount := FWorker.Total();

  if (TotalCount > 0) then
    pbDiscovery.Position := (FWorker.Progress() * pbDiscovery.Max) div TotalCount;

  if FWorker.IsFinished() then
    FinishDiscovery();
end;


procedure TLanDiscoveryDialog.btnRefreshClick(Sender: TObject);
begin

  StartDiscovery();
end;


procedure TLanDiscoveryDialog.btnSaveListClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  Lines: TStringList;
  I: Integer;
  Item: TListItem;
  DeviceName: string;
  AliasNames: string;
  IpAddress: string;
  MacAddress: string;
  Manufacturer: string;

begin

  if (lvDevices.Items.Count = 0) then
    Exit;

  SaveDialog := TSaveDialog.Create(Self);

  try
    SaveDialog.Title := 'Save LAN device list';
    SaveDialog.Filter := 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt, ofPathMustExist];
    SaveDialog.FileName := Format('LAN_devices_%s.txt',
                                  [FormatDateTime('yyyymmdd_hhnnss', Now())]);
    if not SaveDialog.Execute() then
      Exit;

    Lines := TStringList.Create();

    try
      Lines.Add('Computer/device name' + #9 +
                'Alias(es)' + #9 +
                'IPv4 address' + #9 +
                'MAC address' + #9 +
                'Manufacturer');

      for I := 0 to lvDevices.Items.Count - 1 do
        begin
          Item := lvDevices.Items[I];
          DeviceName := Item.Caption;
          AliasNames := '';
          IpAddress := '';
          MacAddress := '';
          Manufacturer := '';

          if (Item.SubItems.Count > 0) then
            AliasNames := Item.SubItems[0];

          if (Item.SubItems.Count > 1) then
            IpAddress := Item.SubItems[1];

          if (Item.SubItems.Count > 2) then
            MacAddress := Item.SubItems[2];

          if (Item.SubItems.Count > 3) then
            Manufacturer := Item.SubItems[3];

          DeviceName := StringReplace(DeviceName,
                                      #9,
                                      ' ',
                                      [rfReplaceAll]);

          AliasNames := StringReplace(AliasNames,
                                      #9,
                                      ' ',
                                      [rfReplaceAll]);

          IpAddress := StringReplace(IpAddress,
                                     #9,
                                     ' ',
                                     [rfReplaceAll]);

          MacAddress := StringReplace(MacAddress,
                                      #9,
                                      ' ',
                                      [rfReplaceAll]);

          Manufacturer := StringReplace(Manufacturer,
                                        #9,
                                        ' ',
                                        [rfReplaceAll]);

          Lines.Add(DeviceName + #9 +
                    AliasNames + #9 +
                    IpAddress + #9 +
                    MacAddress + #9 +
                    Manufacturer);
        end;

      Lines.SaveToFile(SaveDialog.FileName,
                       TEncoding.UTF8);

      FLastSavedList := SaveDialog.FileName;
      btnOpenList.Enabled := FileExists(FLastSavedList);
    finally
      Lines.Free();
    end;
  finally
    SaveDialog.Free();
  end;
end;


procedure TLanDiscoveryDialog.btnOpenListClick(Sender: TObject);
var
  ShellResult: HINST;

begin

  if (FLastSavedList = '') or not FileExists(FLastSavedList) then
    begin
      btnOpenList.Enabled := False;
      MessageDlg('The saved LAN device list can no longer be found.',
                 mtInformation,
                 [mbOK],
                 0);
      Exit;
    end;

  ShellResult := ShellExecute(Handle,
                              'open',
                              PChar(FLastSavedList),
                              nil,
                              nil,
                              SW_SHOWNORMAL);

  if NativeInt(ShellResult) <= 32 then
    MessageDlg('Windows could not open the saved LAN device list with its default application.',
               mtError,
               [mbOK],
               0);
end;


procedure TLanDiscoveryDialog.btnUseIPClick(Sender: TObject);
begin

  if Assigned(lvDevices.Selected) and
     (lvDevices.Selected.SubItems.Count > 1) then
    begin
      FAddress := lvDevices.Selected.SubItems[1];
      ModalResult := mrOK;
    end;
end;


procedure TLanDiscoveryDialog.lvDevicesDblClick(Sender: TObject);
begin

  btnUseIPClick(Sender);
end;


procedure TLanDiscoveryDialog.lvDevicesSelectItem(Sender: TObject;
                                                  Item: TListItem;
                                                  Selected: Boolean);
begin

  btnUseIP.Enabled := Assigned(lvDevices.Selected);
end;


// External methods
//=================
{$WARN SYMBOL_PLATFORM OFF}

function IcmpCreateFile; external iphlpapiLib name 'IcmpCreateFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IcmpCloseHandle; external iphlpapiLib name 'IcmpCloseHandle' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IcmpSendEcho; external iphlpapiLib name 'IcmpSendEcho' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetIpNetTable; external iphlpapiLib name 'GetIpNetTable' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetAdaptersAddresses; external iphlpapiLib name 'GetAdaptersAddresses' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function RawSendTo; external ws2_32Lib name 'sendto' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function RawReceiveFrom; external ws2_32Lib name 'recvfrom' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

end.

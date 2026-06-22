// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ_NetWorkStationsScanner.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 3.2.0
//
// Description: Network scanner for lightweight filebrowser dialog.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: LWFileBrowserExDlg.pas
// Related projects: MfPackX320/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
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
unit RDJ_NetWorkStationsScanner;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  //WinApi.ShellAPI,
  WinApi.WinShellApi.ShlObj,
  WinApi.WinShellApi.ShlObjIdl_Core,
  WinApi.WinShellApi.ShlObj_Core,
  WinApi.WinShellApi.ShlGuid,
  {System}
  System.SysUtils,
  System.Win.ComObj,
  System.Classes,
  {NetManApi}
  WinApi.NetManApi.LMShare,
  WinApi.NetManApi.LMCons,
  WinApi.NetManApi.LMApiBuf,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.ObjIdlbase;

type

  TDiscoveryEvent = procedure(const AStation: string) of object;

  TRDJNetworkStationsScanner = class(TObject)
  private
    class procedure AddUnique(AItems: TStrings;
                              const AValue: string); static;

    class function NormalizeStationName(const AStation: string): string; static;

  public
    class procedure DiscoverNetworkStations(AItems: TStrings); static;

    class function StationExists(const AStation: string): Boolean; static;

    class procedure DiscoverStationShares(const AStation: string;
                                          AItems: TStrings); static;
  end;

  TNetworkDiscoveryThread = class(TThread)
  private

    FResultList: TStringList;
    FOnStationFound: TDiscoveryEvent;
    FOnFinished: TNotifyEvent;

    procedure AddStationName(const AStationName: string);
    procedure AddStationToTarget(const AStationName: string);
    procedure FindStations;
    procedure FindStationsShellNetworkFolder;
    procedure DoFinished;

  protected
    procedure Execute; override;

  public

    constructor Create();
    destructor Destroy; override;

    property OnStationFound: TDiscoveryEvent read FOnStationFound write FOnStationFound;
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
  end;


implementation


const

  NERR_Success          = 0;
  FOLDERID_NetworkFolder: TGUID = '{D20BEEC4-5CA8-4905-AE3B-BF251EA09B53}';


function IsIgnoredNetworkStationName(const AStationName: string): Boolean;
var
  StationName: string;

begin

  StationName := Trim(AStationName);

  Result := (StationName = '') or
            SameText(StationName,
                     '\Microsoft Terminal Services') or
            SameText(StationName,
                     '\Microsoft Windows Network') or
            SameText(StationName,
                     '\Plan 9 Network Provider') or
            SameText(StationName,
                     '\Web Client Network') or
            SameText(StationName,
                     '\Network') or
            SameText(StationName,
                     '\Provider') or
            StationName.StartsWith('Provider\',
                                   True) or
            StationName.StartsWith('Layered\',
                                   True) or
            StationName.StartsWith('::{',
                                   True) or
            StationName.StartsWith('\\Provider\',
                                   True) or
            StationName.StartsWith('\\Layered\',
                                   True);
end;


function ExtractComputerNameFromShellName(const AShellName: string): string;
var
  S: string;
  P: Integer;

begin

  Result := '';

  S := Trim(AShellName);

  if (S = '') then
    Exit;

  S := StringReplace(S,
                     '/',
                     '\',
                     [rfReplaceAll]);

  if IsIgnoredNetworkStationName(S) then
    Exit;

  P := Pos('\\',
           S);

  if (P > 0) then
    S := Copy(S,
              P,
              MaxInt);

  while S.StartsWith('\') do
    Delete(S,
           1,
           1);

  P := Pos('\',
           S);

  if (P > 0) then
    S := Copy(S,
              1,
              P - 1);

  Result := Trim(S);
end;


function IsValidComputerNameCandidate(const AComputerName: string): Boolean;
var
  ComputerName: string;
  I: Integer;
  C: Char;

begin

  Result := False;

  ComputerName := Trim(AComputerName);

  if (ComputerName = '') then
    Exit;

  if (Length(ComputerName) > 255) then
    Exit;

  if ComputerName.StartsWith('-') or
     ComputerName.EndsWith('-') or
     ComputerName.StartsWith('.') or
     ComputerName.EndsWith('.') then
    Exit;

  for I := 1 to Length(ComputerName) do
    begin

      C := ComputerName[I];

      if not CharInSet(C,
                       ['A'..'Z',
                        'a'..'z',
                        '0'..'9',
                        '-',
                        '_',
                        '.']) then
        Exit;
    end;

  Result := True;
end;


function NormalizeNetworkStationName(const AStationName: string): string;
var
  ComputerName: string;

begin

  Result := '';

  ComputerName := ExtractComputerNameFromShellName(AStationName);

  if not IsValidComputerNameCandidate(ComputerName) then
    Exit;

  Result := '\\' + ComputerName;

  if IsIgnoredNetworkStationName(Result) then
    Result := '';
end;


function EnumerateDiskShares(const AStationName: string;
                             AItems: TStrings;
                             const AStopAfterFirst: Boolean): Boolean;
var
  StationName: string;
  Buffer: PBYTE;
  EntriesRead: DWORD;
  TotalEntries: DWORD;
  ResumeHandle: DWORD;
  Status: NET_API_STATUS;
  I: DWORD;
  ShareInfo: PSHARE_INFO_1;
  ShareName: string;
  ShareType: DWORD;

begin

  Result := False;

  StationName := NormalizeNetworkStationName(AStationName);

  if (StationName = '') then
    Exit;

  ResumeHandle := 0;

  repeat

    Buffer := nil;
    EntriesRead := 0;
    TotalEntries := 0;

    Status := NetShareEnum(PWideChar(StationName),
                           1,
                           Buffer,
                           MAX_PREFERRED_LENGTH,
                           @EntriesRead,
                           @TotalEntries,
                           @ResumeHandle);

    if (Status <> NERR_Success) and
       (Status <> ERROR_MORE_DATA) then
      Exit;

    try

      if Assigned(Buffer) and
         (EntriesRead > 0) then
        begin

          I := 0;

          while (I < EntriesRead) do
            begin

              ShareInfo := PSHARE_INFO_1(NativeUInt(Buffer) + (NativeUInt(I) * SizeOf(SHARE_INFO_1)));

              ShareType := ShareInfo^.shi1_type and not STYPE_SPECIAL;

              if (ShareType = STYPE_DISKTREE) and
                 (ShareInfo^.shi1_netname <> nil) then
                begin

                  ShareName := ShareInfo^.shi1_netname;
                  ShareName := Trim(ShareName);

                  if (ShareName <> '') and
                     not ShareName.EndsWith('$') then
                    begin

                      Result := True;

                      if Assigned(AItems) then
                        TRDJNetworkStationsScanner.AddUnique(AItems,
                                                             StationName + '\' + ShareName);

                      if AStopAfterFirst then
                        Exit;
                    end;
                end;

              Inc(I);
            end;
        end;

    finally

      if Assigned(Buffer) then
        NetApiBufferFree(Buffer);
    end;

  until (Status <> ERROR_MORE_DATA);
end;


function StationHasDiskShares(const AStationName: string): Boolean;
begin

  Result := EnumerateDiskShares(AStationName,
                                nil,
                                True);
end;


class procedure TRDJNetworkStationsScanner.AddUnique(AItems: TStrings;
                                                     const AValue: string);
begin

  if (AItems = nil) then
    Exit;

  if (AValue = '') then
    Exit;

  if (AItems.IndexOf(AValue) < 0) then
    AItems.Add(AValue);
end;


class function TRDJNetworkStationsScanner.NormalizeStationName(const AStation: string): string;
begin

  Result := NormalizeNetworkStationName(AStation);
end;


class procedure TRDJNetworkStationsScanner.DiscoverNetworkStations(AItems: TStrings);
var
  Finder: TNetworkDiscoveryThread;

begin

  if (AItems = nil) then
    Exit;

  AItems.Clear();

  Finder := TNetworkDiscoveryThread.Create();
  Finder.FreeOnTerminate := False;

  try

    Finder.Start();
    Finder.WaitFor();

    AItems.Assign(Finder.FResultList);
  finally

    Finder.Free();
  end;
end;


class function TRDJNetworkStationsScanner.StationExists(const AStation: string): Boolean;
var
  StationName: string;

begin

  StationName := NormalizeStationName(AStation);

  Result := (StationName <> '') and
            StationHasDiskShares(StationName);
end;


class procedure TRDJNetworkStationsScanner.DiscoverStationShares(const AStation: string;
                                                                 AItems: TStrings);
begin

  if (AItems = nil) then
    Exit;

  AItems.Clear();

  EnumerateDiskShares(AStation,
                      AItems,
                      False);
end;


constructor TNetworkDiscoveryThread.Create();
begin

  inherited Create(True);

  FreeOnTerminate := True;

  FResultList := TStringList.Create();
  FResultList.Sorted := True;
  FResultList.Duplicates := dupIgnore;
  FResultList.CaseSensitive := False;
end;


destructor TNetworkDiscoveryThread.Destroy();
begin

  FResultList.Free();
  OnStationFound := nil;
  OnFinished := nil;

  inherited Destroy();
end;


procedure TNetworkDiscoveryThread.Execute();
var
  CoInitHr: HRESULT;

begin

  NameThreadForDebugging('NetworkDiscovery');

  CoInitHr := CoInitializeEx(nil,
                             COINIT_APARTMENTTHREADED);

  try

    try

      if Succeeded(CoInitHr) or
         (CoInitHr = RPC_E_CHANGED_MODE) then
        FindStations();
    finally

      if Succeeded(CoInitHr) then
        CoUninitialize();
    end;

  finally

    if Assigned(FOnFinished) then
      Synchronize(DoFinished);
  end;
end;


procedure TNetworkDiscoveryThread.FindStations();
var
  I: Integer;

begin

  FResultList.Clear();

  FindStationsShellNetworkFolder();

  if Assigned(FOnStationFound) then
    begin

      for I := 0 to FResultList.Count - 1 do
        begin

          if Terminated then
            Exit;

          Synchronize(procedure
                      begin

                        AddStationToTarget(FResultList[I]);
                      end);
        end;
    end;
end;


procedure TNetworkDiscoveryThread.AddStationToTarget(const AStationName: string);
begin

  if (AStationName = '') then
    Exit;

  if Assigned(FOnStationFound) then
    FOnStationFound(AStationName);
end;


procedure TNetworkDiscoveryThread.AddStationName(const AStationName: string);
var
  StationName: string;

begin

  StationName := NormalizeNetworkStationName(AStationName);

  if (StationName = '') then
    Exit;

  if not StationHasDiskShares(StationName) then
    Exit;

  if FResultList.IndexOf(StationName) < 0 then
    FResultList.Add(StationName);
end;


procedure TNetworkDiscoveryThread.DoFinished();
begin

  if Assigned(FOnFinished) then
    FOnFinished(Self);
end;


procedure TNetworkDiscoveryThread.FindStationsShellNetworkFolder();
var
  NetworkFolder: IShellItem;
  EnumItems: IEnumShellItems;
  Item: IShellItem;
  NamePtr: LPWSTR;
  DisplayName: string;
  Fetched: ULONG;
  hr: HRESULT;

begin

  NetworkFolder := nil;
  EnumItems := nil;

  hr := SHGetKnownFolderItem(FOLDERID_NetworkFolder,
                             KF_FLAG_DEFAULT,
                             0,
                             IID_IShellItem,
                             Pointer(NetworkFolder));

  if Failed(hr) or
     (NetworkFolder = nil) then
    Exit;

  hr := NetworkFolder.BindToHandler(nil,
                                    BHID_EnumItems,
                                    IID_IEnumShellItems,
                                    EnumItems);

  if Failed(hr) or
     (EnumItems = nil) then
    Exit;

  while not Terminated do
    begin

      Item := nil;
      Fetched := 0;

      hr := EnumItems.Next(1,
                           @Item,
                           @Fetched);

      if (hr <> S_OK) or
         (Fetched = 0) or
         (Item = nil) then
        Break;

      NamePtr := nil;

      if Succeeded(Item.GetDisplayName(SIGDN_NORMALDISPLAY,
                                        NamePtr)) then
        try

          if (NamePtr <> nil) then
            begin

              DisplayName := NamePtr;
              AddStationName(DisplayName);
            end;
        finally

          CoTaskMemFree(NamePtr);
        end;

      NamePtr := nil;

      if Succeeded(Item.GetDisplayName(SIGDN_DESKTOPABSOLUTEPARSING,
                                        NamePtr)) then
        try

          if (NamePtr <> nil) then
            begin

              DisplayName := NamePtr;
              AddStationName(DisplayName);
            end;
        finally

          CoTaskMemFree(NamePtr);
        end;
    end;
end;

end.

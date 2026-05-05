// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: ProcessAudioPickerUtils.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Tools to pick audio processes.
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
//          Please, read documentation carefully!
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
// Source: FactoryX.Code.
// =============================================================================
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
unit ProcessAudioPickerUtils;

interface

uses
  Winapi.Windows,
  Winapi.TlHelp32,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  Winapi.CoreAudioApi.MMDeviceApi,
  Winapi.CoreAudioApi.AudioPolicy;

type
  TProcessAudioState = (pasUnknown, pasSilent, pasActive);

  TProcessChildItem = class
  public

    ProcessId: DWORD;
    ParentProcessId: DWORD;
    DisplayName: string;
    ExeName: string;
    ImagePath: string;
    Peak: Single;
    AudioState: TProcessAudioState;
    RoleHint: string;
    SessionCount: Integer;
    BrowserFamily: string;
    IsBrowserLike: Boolean;
    IsAudioProcess: Boolean;
  end;

  TProcessGroupItem = class
  public

    RootProcessId: DWORD;
    DisplayName: string;
    ExeName: string;
    ImagePath: string;
    Peak: Single;
    AudioState: TProcessAudioState;
    AudioChildCount: Integer;
    IsBrowserLike: Boolean;
    BrowserFamily: string;
    Children: TObjectList<TProcessChildItem>;

    constructor Create;
    destructor Destroy; override;
  end;

  procedure BuildLiveProcessGroups(AGroups: TObjectList<TProcessGroupItem>);


implementation

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

  AUDIO_SESSION_STATE_INACTIVE = 0;
  AUDIO_SESSION_STATE_ACTIVE   = 1;
  AUDIO_SESSION_STATE_EXPIRED  = 2;

type

  TProcessInfo = record
    ProcessId: DWORD;
    ParentProcessId: DWORD;
    ExeName: string;
    ImagePath: string;
  end;

  TAudioSessionSummary = record
    ProcessId: DWORD;
    SessionCount: Integer;
    AudioState: TProcessAudioState;
  end;

// WinApi
function QueryFullProcessImageNameW(hProcess: THandle;
                                    dwFlags: DWORD;
                                    lpExeName: PWideChar;
                                    var lpdwSize: DWORD): BOOL; stdcall;
  external kernel32 name 'QueryFullProcessImageNameW';


constructor TProcessGroupItem.Create();
begin

  inherited Create;

  Children := TObjectList<TProcessChildItem>.Create(True);
end;


destructor TProcessGroupItem.Destroy;
begin

  Children.Free;

  inherited Destroy;
end;


function TryGetProcessImagePath(APid: DWORD;
                                out AImagePath: string): Boolean;
var
  H: THandle;
  Buf: array[0..MAX_PATH - 1] of WideChar;
  Size: DWORD;

begin

  Result := False;
  AImagePath := '';

  H := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
                   False,
                   APid);
  if (H = 0) then
    H := OpenProcess(PROCESS_QUERY_INFORMATION,
                     False,
                     APid);
  if (H = 0) then
    Exit;

  try

    Size := Length(Buf);

    if QueryFullProcessImageNameW(H,
                                  0,
                                  @Buf[0],
                                  Size) then
      begin

        AImagePath := Buf;
        Result := True;
      end;
  finally

    CloseHandle(H);
  end;
end;


function GetExeName(const APath: string): string;
begin

  Result := ExtractFileName(APath);
end;


function NormalizeExeName(const AExeName: string): string;
begin

  Result := LowerCase(Trim(AExeName));
end;


function NormalizePath(const APath: string): string;
begin

  Result := LowerCase(Trim(APath));
end;


function BrowserFamilyFromProcess(const AExeName,
                                        AImagePath: string): string;
var
  Exe: string;
  Path: string;

begin

  Exe := NormalizeExeName(AExeName);
  Path := NormalizePath(AImagePath);

  if (Pos('tor browser',
          Path) > 0) or
     ((Exe = 'firefox.exe') and (Pos('\tor browser\',
                                     Path) > 0)) then
    Exit('Tor Browser');

  if (Exe = 'librewolf.exe') or (Pos('librewolf',
                                     Path) > 0) then
    Exit('LibreWolf');

  if (Exe = 'mullvad-browser.exe') or (Pos('mullvad browser',
                                           Path) > 0) or
     (Pos('mullvad-browser',
          Path) > 0) then
    Exit('Mullvad Browser');

  if (Exe = 'duckduckgo.exe') or (Pos('duckduckgo',
                                      Path) > 0) then
    Exit('DuckDuckGo');

  if (Exe = 'chrome.exe') then
    Exit('Chrome');

  if (Exe = 'msedge.exe') then
    Exit('Edge');

  if (Exe = 'msedgewebview2.exe') or (Pos('webview2',
                                          Path) > 0) then
    Exit('WebView2');

  if (Exe = 'firefox.exe') then
    Exit('Firefox');

  if (Exe = 'brave.exe') or (Pos('brave',
                                 Path) > 0) then
    Exit('Brave');

  if (Exe = 'opera.exe') or (Exe = 'opera_gx.exe') or (Pos('opera',
                                                           Path) > 0) then
    Exit('Opera');

  if (Exe = 'vivaldi.exe') or (Pos('vivaldi',
                                   Path) > 0) then
    Exit('Vivaldi');

  Result := '';
end;


function IsBrowserLikeProcess(const AExeName,
                                    AImagePath: string): Boolean;
begin

  Result := BrowserFamilyFromProcess(AExeName, AImagePath) <> '';
end;


function SameProcessImage(const ALeftPath, ARightPath: string): Boolean;
begin

  if (ALeftPath <> '') and (ARightPath <> '') then
    Exit(SameText(ALeftPath,
         ARightPath));

  Result := True;
end;


function AudioStateFromSessionState(const AState: UINT): TProcessAudioState;
begin

  case AState of
    AUDIO_SESSION_STATE_ACTIVE:
      Result := pasActive;
    AUDIO_SESSION_STATE_INACTIVE:
      Result := pasSilent;
  else

    Result := pasUnknown;
  end;
end;


function MergeAudioState(const ACurrent,
                               ANew: TProcessAudioState): TProcessAudioState;
begin

  Result := ACurrent;

  if (ANew = pasActive) or (ACurrent = pasUnknown) then
    Result := ANew;
end;


function FindSessionSummary(const AItems: TList<TAudioSessionSummary>;
                            const APid: DWORD): Integer;
var
  I: Integer;

begin

  Result := -1;
  for I := 0 to AItems.Count - 1 do
    if AItems[I].ProcessId = APid then
      Exit(I);
end;


procedure EnumerateAudioSessions(AItems: TList<TAudioSessionSummary>);
var
  Enumerator: IMMDeviceEnumerator;
  Device: IMMDevice;
  SessionMgr: IAudioSessionManager2;
  SessionEnum: IAudioSessionEnumerator;
  Count: Integer;
  I: Integer;
  SessionCtrl: IAudioSessionControl;
  SessionCtrl2: IAudioSessionControl2;
  State: UINT;
  Pid: DWORD;
  Index: Integer;
  Summary: TAudioSessionSummary;

begin

  AItems.Clear;

  if Failed(CoCreateInstance(CLSID_MMDeviceEnumerator,
                             nil,
                             CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,
                             Enumerator)) then
    Exit;

  if Failed(Enumerator.GetDefaultAudioEndpoint(eRender,
                                               eMultimedia,
                                               Device)) then
    Exit;

  if Failed(Device.Activate(IID_IAudioSessionManager2,
                            CLSCTX_ALL,
                            nil,
                            Pointer(SessionMgr))) then
    Exit;

  if Failed(SessionMgr.GetSessionEnumerator(SessionEnum)) then
    Exit;

  if Failed(SessionEnum.GetCount(Count)) then
    Exit;

  for I := 0 to Count - 1 do
    begin

      SessionCtrl := nil;
      SessionCtrl2 := nil;
      State := AUDIO_SESSION_STATE_EXPIRED;
      Pid := 0;

      if Failed(SessionEnum.GetSession(I,
                                       SessionCtrl)) or (SessionCtrl = nil) then
        Continue;

      if Failed(SessionCtrl.GetState(State)) then
        State := AUDIO_SESSION_STATE_EXPIRED;

      if (State = AUDIO_SESSION_STATE_EXPIRED) then
        Continue;

      if Supports(SessionCtrl,
                  IAudioSessionControl2,
                  SessionCtrl2) then
        begin

          if Failed(SessionCtrl2.GetProcessId(Pid)) then
            Pid := 0;

          if (Pid <> 0) then
            begin

              Index := FindSessionSummary(AItems, Pid);

              if (Index >= 0) then
                begin

                  Summary := AItems[Index];
                  Inc(Summary.SessionCount);
                  Summary.AudioState := MergeAudioState(Summary.AudioState,
                                                        AudioStateFromSessionState(State));
                  AItems[Index] := Summary;
                end
              else
                begin

                  Summary.ProcessId := Pid;
                  Summary.SessionCount := 1;
                  Summary.AudioState := AudioStateFromSessionState(State);
                  AItems.Add(Summary);
                end;
            end;
        end;
    end;
end;


procedure BuildProcessSnapshot(AMap: TDictionary<DWORD, TProcessInfo>);
var
  Snap: THandle;
  Entry: TProcessEntry32;
  Info: TProcessInfo;
  Path: string;

begin

  AMap.Clear;

  Snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (Snap = INVALID_HANDLE_VALUE) then
    Exit;

  try

    ZeroMemory(@Entry, SizeOf(Entry));
    Entry.dwSize := SizeOf(Entry);

    if Process32First(Snap,
                      Entry) then
      repeat

        Info.ProcessId := Entry.th32ProcessID;
        Info.ParentProcessId := Entry.th32ParentProcessID;
        Info.ExeName := Entry.szExeFile;
        if TryGetProcessImagePath(Info.ProcessId,
                                  Path) then
          Info.ImagePath := Path
        else
          Info.ImagePath := '';

        AMap.AddOrSetValue(Info.ProcessId, Info);
      until not Process32Next(Snap,
                              Entry);
  finally

    CloseHandle(Snap);
  end;
end;


function TryGetProcessInfo(const AProcMap: TDictionary<DWORD, TProcessInfo>;
                           const APid: DWORD;
                           out AInfo: TProcessInfo): Boolean;
begin

  Result := AProcMap.TryGetValue(APid,
                                 AInfo);

  if not Result then
    begin

      AInfo.ProcessId := APid;
      AInfo.ParentProcessId := 0;
      AInfo.ExeName := '';
      AInfo.ImagePath := '';
    end;
end;


function ResolveGroupRootPid(const AProcMap: TDictionary<DWORD, TProcessInfo>;
                             const APid: DWORD): DWORD;
var
  CurInfo: TProcessInfo;
  ParentInfo: TProcessInfo;
  CurFamily: string;
  ParentFamily: string;

begin

  Result := APid;

  if not TryGetProcessInfo(AProcMap,
                           APid,
                           CurInfo) then
    Exit;

  while (CurInfo.ParentProcessId <> 0) and
        (CurInfo.ParentProcessId <> 4) and
        TryGetProcessInfo(AProcMap,
                          CurInfo.ParentProcessId,
                          ParentInfo) do
    begin

      CurFamily := BrowserFamilyFromProcess(CurInfo.ExeName,
                                            CurInfo.ImagePath);

      ParentFamily := BrowserFamilyFromProcess(ParentInfo.ExeName,
                                               ParentInfo.ImagePath);

      if (ParentInfo.ExeName = '') then
        Break;

      if not SameProcessImage(ParentInfo.ImagePath,
                              CurInfo.ImagePath) then
        begin

          if (CurFamily = '') or (ParentFamily = '') or
             (not SameText(CurFamily,
                           ParentFamily)) then
            Break;
        end;

      if (not SameText(ParentInfo.ExeName,
                       CurInfo.ExeName)) and
         ((CurFamily = '') or (ParentFamily = '') or
          (not SameText(CurFamily,
                        ParentFamily))) then
        Break;

      Result := ParentInfo.ProcessId;
      CurInfo := ParentInfo;
    end;
end;


function FindGroupByRootPid(const AGroups: TObjectList<TProcessGroupItem>;
                            const ARootPid: DWORD): TProcessGroupItem;
var
  I: Integer;

begin

  Result := nil;

  for I := 0 to AGroups.Count - 1 do
    if (AGroups[I].RootProcessId = ARootPid) then
      Exit(AGroups[I]);
end;


procedure InitializeGroupFromInfo(AGroup: TProcessGroupItem;
                                  const AInfo: TProcessInfo);
begin

  AGroup.RootProcessId := AInfo.ProcessId;
  AGroup.ImagePath := AInfo.ImagePath;
  AGroup.ExeName := AInfo.ExeName;

  if (AGroup.ExeName <> '') then
    AGroup.DisplayName := AGroup.ExeName
  else
    if (AGroup.ImagePath <> '') then
      AGroup.DisplayName := GetExeName(AGroup.ImagePath)
    else
      AGroup.DisplayName := Format('PID %d',
                                   [AInfo.ProcessId]);

  AGroup.AudioState := pasUnknown;
  AGroup.Peak := 0.0;
  AGroup.AudioChildCount := 0;
  AGroup.BrowserFamily := BrowserFamilyFromProcess(AGroup.ExeName,
                                                   AGroup.ImagePath);
  AGroup.IsBrowserLike := (AGroup.BrowserFamily <> '');
end;


function DetermineChildRoleHint(const AGroup: TProcessGroupItem;
                                const AChild: TProcessChildItem): string;
begin

  if (AChild.ProcessId = AGroup.RootProcessId) then
    begin

      if AChild.IsAudioProcess then
        begin

          if AGroup.IsBrowserLike then
            Result := 'browser root audio process'
          else
            Result := 'root audio process';
        end
      else
        begin

          if AGroup.IsBrowserLike then
            Result := 'browser root process'
          else
            Result := 'root process';
        end;
      Exit;
    end;

  if AGroup.IsBrowserLike then
    begin

      if SameText(AGroup.BrowserFamily, 'WebView2') then
        begin

          if AChild.IsAudioProcess then
            Result := 'WebView audio process'
          else
            Result := 'WebView helper process';
        end
      else
        if (AChild.ParentProcessId = AGroup.RootProcessId) then
          begin
            if AChild.IsAudioProcess then
              Result := 'tab / utility audio process'
            else
              Result := 'tab / utility process';
          end
        else
          begin

            if AChild.IsAudioProcess then
              Result := 'nested tab / utility audio process'
            else
              Result := 'nested tab / utility process';
          end;
      Exit;
    end;

  if (AChild.ParentProcessId = AGroup.RootProcessId) then
    begin

      if AChild.IsAudioProcess then
        Result := 'child audio process'
      else
        Result := 'child process';
    end
  else
    begin

      if AChild.IsAudioProcess then
        Result := 'nested audio process'
      else
        Result := 'nested process';
    end;
end;


function IsDescendantOf(const AProcMap: TDictionary<DWORD, TProcessInfo>;
                        const AChildPid, ARootPid: DWORD): Boolean;
var
  CurInfo: TProcessInfo;
  Safety: Integer;

begin

  Result := False;

  if (AChildPid = ARootPid) then
    Exit(True);

  if not TryGetProcessInfo(AProcMap,
                           AChildPid,
                           CurInfo) then
    Exit(False);

  Safety := 0;

  while (CurInfo.ParentProcessId <> 0) and (CurInfo.ParentProcessId <> 4) do
    begin

      if (CurInfo.ParentProcessId = ARootPid) then
        Exit(True);

      if not TryGetProcessInfo(AProcMap,
                               CurInfo.ParentProcessId,
                               CurInfo) then
        Break;

      Inc(Safety);
      if (Safety > 256) then
        Break;
    end;
end;


function FindSessionSummaryForPid(const ASummaries: TList<TAudioSessionSummary>;
                                  const APid: DWORD;
                                  out ASummary: TAudioSessionSummary): Boolean;
var
  I: Integer;

begin

  for I := 0 to ASummaries.Count - 1 do
    if (ASummaries[I].ProcessId = APid) then
      begin

        ASummary := ASummaries[I];
        Exit(True);
      end;

  ASummary.ProcessId := APid;
  ASummary.SessionCount := 0;
  ASummary.AudioState := pasUnknown;
  Result := False;
end;


procedure AddDescendantProcessesToGroup(AGroup: TProcessGroupItem;
                                        const AProcMap: TDictionary<DWORD, TProcessInfo>;
                                        const ASummaries: TList<TAudioSessionSummary>);
var
  Pair: TPair<DWORD, TProcessInfo>;
  Child: TProcessChildItem;
  Summary: TAudioSessionSummary;

begin

  for Pair in AProcMap do
    begin

      if not IsDescendantOf(AProcMap,
                            Pair.Key,
                            AGroup.RootProcessId) then
        Continue;

      Child := TProcessChildItem.Create;
      Child.ProcessId := Pair.Value.ProcessId;
      Child.ParentProcessId := Pair.Value.ParentProcessId;
      Child.ExeName := Pair.Value.ExeName;
      Child.ImagePath := Pair.Value.ImagePath;
      Child.Peak := 0.0;
      Child.BrowserFamily := BrowserFamilyFromProcess(Child.ExeName,
                                                      Child.ImagePath);
      Child.IsBrowserLike := (Child.BrowserFamily <> '');

      if (Child.ExeName <> '') then
        Child.DisplayName := Child.ExeName
      else
        if (Child.ImagePath <> '') then
          Child.DisplayName := GetExeName(Child.ImagePath)
        else
          Child.DisplayName := Format('PID %d',
                                      [Child.ProcessId]);

      if FindSessionSummaryForPid(ASummaries,
                                  Child.ProcessId,
                                  Summary) then
        begin

          Child.SessionCount := Summary.SessionCount;
          Child.AudioState := Summary.AudioState;
          Child.IsAudioProcess := Summary.SessionCount > 0;
        end
      else
        begin

          Child.SessionCount := 0;
          Child.AudioState := pasUnknown;
          Child.IsAudioProcess := False;
        end;

      Child.RoleHint := DetermineChildRoleHint(AGroup,
                                               Child);
      AGroup.Children.Add(Child);

      if Child.IsAudioProcess then
        begin

          Inc(AGroup.AudioChildCount);
          AGroup.AudioState := MergeAudioState(AGroup.AudioState,
                                               Child.AudioState);
        end;
    end;
end;


procedure SortGroupsAndChildren(AGroups: TObjectList<TProcessGroupItem>);
var
  GroupComparer: IComparer<TProcessGroupItem>;
  ChildComparer: IComparer<TProcessChildItem>;
  I: Integer;

begin

  GroupComparer := TComparer<TProcessGroupItem>.Construct(function(const Left,
                                                                         Right: TProcessGroupItem): Integer
                                                          begin

                                                            Result := CompareText(Left.DisplayName,
                                                                                  Right.DisplayName);
                                                            if (Result = 0) then
                                                              Result := Integer(Left.RootProcessId) - Integer(Right.RootProcessId);
                                                          end);

  ChildComparer := TComparer<TProcessChildItem>.Construct(function(const Left,
                                                                         Right: TProcessChildItem): Integer
                                                          begin

                                                            if (Left.IsAudioProcess <> Right.IsAudioProcess) then
                                                              begin

                                                                if Left.IsAudioProcess then
                                                                  Exit(-1)
                                                                else
                                                                  Exit(1);
                                                              end;

                                                            Result := CompareText(Left.DisplayName,
                                                                                  Right.DisplayName);
                                                            if (Result = 0) then
                                                              Result := Integer(Left.ProcessId) - Integer(Right.ProcessId);
                                                          end);

  AGroups.Sort(GroupComparer);

  for I := 0 to AGroups.Count - 1 do
    AGroups[I].Children.Sort(ChildComparer);
end;


procedure BuildLiveProcessGroups(AGroups: TObjectList<TProcessGroupItem>);
var
  Sessions: TList<TAudioSessionSummary>;
  ProcMap: TDictionary<DWORD, TProcessInfo>;
  Summary: TAudioSessionSummary;
  RootPid: DWORD;
  RootInfo: TProcessInfo;
  Group: TProcessGroupItem;

begin

  AGroups.Clear;

  Sessions := TList<TAudioSessionSummary>.Create;
  ProcMap := TDictionary<DWORD,
                         TProcessInfo>.Create;
  try

    EnumerateAudioSessions(Sessions);
    BuildProcessSnapshot(ProcMap);

    for Summary in Sessions do
      begin

        RootPid := ResolveGroupRootPid(ProcMap,
                                       Summary.ProcessId);

        Group := FindGroupByRootPid(AGroups,
                                    RootPid);
        if (Group = nil) then
          begin

            Group := TProcessGroupItem.Create;

            if not TryGetProcessInfo(ProcMap,
                                     RootPid,
                                     RootInfo) then
              begin

                RootInfo.ProcessId := RootPid;
                RootInfo.ParentProcessId := 0;
                RootInfo.ExeName := '';
                RootInfo.ImagePath := '';
              end;

            InitializeGroupFromInfo(Group,
                                    RootInfo);
            AGroups.Add(Group);
          end;
      end;

    for Group in AGroups do
      AddDescendantProcessesToGroup(Group,
                                    ProcMap,
                                    Sessions);

    SortGroupsAndChildren(AGroups);
  finally

    ProcMap.Free;
    Sessions.Free;
  end;
end;

end.

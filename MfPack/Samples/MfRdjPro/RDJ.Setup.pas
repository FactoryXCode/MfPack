// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.Setup.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Read/Write setup fields to ini unit.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
unit RDJ.Setup;

interface

uses

  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.IniFiles,
  {Vcl}
  Vcl.Forms;


const

  MAX_CHANNELS = 8;
  MAX_LOOPBACK_DECKS = 4;
  MAX_FX_SLOTS = 16;

  // Live MSE/fMP4 public fragment duration setup.
  // Stored in milliseconds so the setup GUI can use a trackbar.
  // Default 2000 ms was proven stable for old Windows 10 servers.
  RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS = 50;
  RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS = 6000;
  RDJ_MSE_PUBLIC_FRAGMENT_TARGET_DEFAULT_MS = 2000;
  AUDIO_BUFFER_MIN_MS = 30;
  AUDIO_BUFFER_MAX_MS = 120;
  AUDIO_BUFFER_DEF_MS = 60;

type

  TRDJFxKind = (fxkUnknown,
                fxkParametricEq,
                fxkCompressorLimiter,
                fxkFlangerEcho,
                fxkPitchTempo);

  TRDJFxSlotSetup = record
    Enabled: Boolean;
    FxKind: TRDJFxKind;
  end;

  TRDJFxRackSetup = record
    Count: Integer; // 0..MAX_FX_SLOTS
    Slots: array[0..MAX_FX_SLOTS - 1] of TRDJFxSlotSetup;
  end;


  TRDJSetup = record

    ChannelCount: Integer; // 1..MAX_CHANNELS
    LoopbackDeckCount: Integer; // 0..MAX_LOOPBACK_DECKS

    // Audio device routing (persisted via RDJ.SetupIni).
    // These are endpoint IDs as returned by IMMDevice.GetId (or empty for default).
    MasterDeviceId: string;
    MicDeckEnabled: Boolean;

    // Live microphone input endpoint ID (or empty for default capture device).
    MicDeviceId: string;
    MicDeviceCbItemIndex: Integer;

    // Dual output buses (MASTER + PHONES/CUE).
    PFLEnabled: Boolean;
    PFLDeviceId: string;

    SystemOverrideSleepMode: Boolean;

    // AudioClient bufferlength (default = 60 ms).
    AudioBufferMs: Integer;

    // Audio recorder
    AudioRecorderCaptureBufferMs: Integer;
    AudioRecorderSystemLatency: Integer;
    AudioRecorderAutoBufferSize: LongBool;
    AudioRecorderDisableMMCSS: Boolean;
    AudioRecorderDontOverWriteAudioFiles: Boolean;
    AudioRecorderUsePCMFormat: Boolean;
    AudioRecorderEnableStreamSwitchDetection: Boolean;
    AudioRecorderAudioFormat: Integer;
    AudioRecorderRecordPreFx: Boolean;
    AudioRecorderRecordPostFx: Boolean;

    // Caddy/json settings
    CaddyDir: string;
    CaddyConfigFile: string;
    CaddyNowPlayingJsonFile: string;
    CaddyArtworkPath: string;
    CaddyVideoPath: string;
    CaddyLanAddress: string;
    CaddyMount: string;
    CaddyContentTypeURL: string;
    CaddyCommand: string;
    CaddyLogFile: string;

    // FX racks.
    MasterFxRack: TRDJFxRackSetup;
    ChannelFxRacks: array[0..MAX_CHANNELS - 1] of TRDJFxRackSetup;

    // Recording from tap point.
    AudioRecorderTapPoint: Integer;

    // Paths
    AudioRecordingsDir: string;   // text
    AudioRecordingsPath: string;  // hint
    VideoRecordingsDir: string;   // text
    VideoRecordingsPath: string;  // hint
    DatabaseDir: string;  // text
    DatabasePath: string; // hint
    LocalArtworkDir: string; // text
    LocalArtworkPath: string; // hint

    // Media server
    MediaServerEnabled: Boolean;

    // Live MSE/fMP4 public fragment target duration in milliseconds.
    // The media server groups small internal MF fragments until this target is
    // reached approximately. Default: 2000 ms.
    MsePublicSegmentTargetMs: Integer;
    CameraName: string;
    CameraSymbolicLink: string;
  end;

  TRecordTapPoint = (rtpPreFx,
                     rtpPostFx);

  procedure SetGlobalSetupOnce(const ASetupRec: TRDJSetup);
  procedure LockGlobalSetup();
  function GetGlobalSetup(): TRDJSetup;
  function IsGlobalSetupLocked: Boolean;

  function GetDefaultSetupFileName(): string;

  function FxKindToIniName(const AFxKind: TRDJFxKind): string;
  function IniNameToFxKind(const S: string): TRDJFxKind;

  procedure InitDefaultFxRack(var ARack: TRDJFxRackSetup);
  procedure InitDefaultSetup(var ASetupRec: TRDJSetup);

  procedure LoadSetupFromIni(const FileName: string;
                             var ASetupRec: TRDJSetup);

  procedure SaveSetupToIni(const FileName: string;
                           const ASetupRec: TRDJSetup);

var
  FRDJSetup: TRDJSetup;


implementation

var
  gSetupIsSet: Boolean = False;
  gSetupLocked: Boolean = False;


function FxKindToIniName(const AFxKind: TRDJFxKind): string;
begin

  case AFxKind of
    fxkParametricEq:
      Result := 'ParametricEq';

    fxkCompressorLimiter:
      Result := 'CompressorLimiter';

    fxkFlangerEcho:
      Result := 'FlangerEcho';

    fxkPitchTempo:
      Result := 'PitchTempo';
  else
    Result := 'Unknown';
  end;
end;


function IniNameToFxKind(const S: string): TRDJFxKind;
begin

  if SameText(S,
              'ParametricEq') then
    Exit(fxkParametricEq);

  if SameText(S,
              'CompressorLimiter') then
    Exit(fxkCompressorLimiter);

  if SameText(S,
              'FlangerEcho') then
    Exit(fxkFlangerEcho);

  if SameText(S,
              'PitchTempo') then
    Exit(fxkPitchTempo);

  Result := fxkUnknown;
end;


procedure InitDefaultFxRack(var ARack: TRDJFxRackSetup);
var
  i: Integer;

begin

  ARack.Count := 0;

  for i := 0 to MAX_FX_SLOTS - 1 do
    begin
      ARack.Slots[i].Enabled := False;
      ARack.Slots[i].FxKind := fxkUnknown;
    end;
end;


function ClampInteger(const AValue, AMin, AMax: Integer): Integer;
begin

  Result := AValue;

  if Result < AMin then
    Result := AMin
  else
  if Result > AMax then
    Result := AMax;
end;
procedure InitDefaultSetup(var ASetupRec: TRDJSetup);
var
  i: Integer;

begin

  // Default settings.
  ASetupRec.AudioBufferMs := AUDIO_BUFFER_DEF_MS;
  ASetupRec.ChannelCount := 2;
  ASetupRec.LoopbackDeckCount := 1;
  ASetupRec.MasterDeviceId := '';
  ASetupRec.PFLEnabled := False;
  ASetupRec.PFLDeviceId := '';
  ASetupRec.MicDeviceId := '';

  ASetupRec.SystemOverrideSleepMode := True;

  // Caddy/json.
  // Default settings.
  ASetupRec.CaddyDir := 'C:\Caddy';
  ASetupRec.CaddyConfigFile := 'C:\Caddy\caddy.cff';
  ASetupRec.CaddyNowPlayingJsonFile := 'C:\Caddy\nowplaying.json';
  ASetupRec.CaddyArtworkPath := 'C:\Caddy\Artwork';
  ASetupRec.CaddyVideoPath := 'C:\Caddy\Video';
  ASetupRec.CaddyLanAddress := '';
  ASetupRec.CaddyMount := '';
  ASetupRec.CaddyContentTypeURL := 'video/mp4';
  ASetupRec.CaddyCommand := 'C:\caddy.exe run --config "C:\Caddy\Caddy.cff" --adapter caddyfile'; // We need to avoid false uri, better to place Caddy under dir RDJ?
  ASetupRec.CaddyLogFile := 'Caddy.log';

  // Master rack starts empty by default.
  InitDefaultFxRack(ASetupRec.MasterFxRack);

  // Each channel gets a default EQ rack entry.
  // Default settings.
  for i := 0 to MAX_CHANNELS - 1 do
    begin

      InitDefaultFxRack(ASetupRec.ChannelFxRacks[i]);
      ASetupRec.ChannelFxRacks[i].Count := 1;
      ASetupRec.ChannelFxRacks[i].Slots[0].Enabled := True;
      ASetupRec.ChannelFxRacks[i].Slots[0].FxKind := fxkParametricEq;
    end;

  ASetupRec.AudioRecorderTapPoint := 0; // Pre-FX

  ASetupRec.AudioRecorderRecordPreFx := True;
  ASetupRec.AudioRecorderRecordPostFx := False;

  // local audio
  ASetupRec.AudioRecordingsDir := 'AudioRecordings';
  ASetupRec.AudioRecordingsPath := Format('%s\%s\', [ExtractFileDir(Application.ExeName),
                                                     ASetupRec.AudioRecordingsDir]);

  if not DirectoryExists(ASetupRec.AudioRecordingsPath) then
    if not CreateDir(ASetupRec.AudioRecordingsPath) then
      ASetupRec.AudioRecordingsPath := ExpandFileName(ExtractFileDir(Application.ExeName));

  // Local video
  ASetupRec.VideoRecordingsDir := 'VideoRecordings';
  ASetupRec.VideoRecordingsPath := Format('%s\%s\', [ExtractFileDir(Application.ExeName),
                                                     ASetupRec.VideoRecordingsDir]);

  if not DirectoryExists(ASetupRec.VideoRecordingsPath) then
    if not CreateDir(ASetupRec.VideoRecordingsPath) then
      ASetupRec.VideoRecordingsPath := ExpandFileName(ExtractFileDir(Application.ExeName));

  ASetupRec.DatabaseDir := 'Data';
  ASetupRec.DatabasePath := Format('%s\%s\', [ExtractFileDir(Application.ExeName),
                                              ASetupRec.DatabaseDir]);
  // Database
  if not DirectoryExists(ASetupRec.DatabasePath) then
    if not CreateDir(ASetupRec.DatabasePath) then
      ASetupRec.DatabasePath := ExpandFileName(ExtractFileDir(Application.ExeName));

  // Artwork is published from Caddy's web root. Keep the legacy local fields
  // synchronized for older code and INI files that still use Paths\Artwork.
  ASetupRec.LocalArtworkDir := ExtractFileName(ExcludeTrailingPathDelimiter(ASetupRec.CaddyArtworkPath));
  ASetupRec.LocalArtworkPath := ASetupRec.CaddyArtworkPath;

  // Media server camera
  ASetupRec.MediaServerEnabled := False;
  ASetupRec.MsePublicSegmentTargetMs := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_DEFAULT_MS;
  ASetupRec.CameraName := '';
  ASetupRec.CameraSymbolicLink := '';
end;


procedure SetGlobalSetupOnce(const ASetupRec: TRDJSetup);
begin

  if gSetupLocked then
    Exit;

  FRDJSetup := ASetupRec;
  gSetupIsSet := True;
end;


procedure LockGlobalSetup();
begin

  gSetupLocked := True;
end;


function GetGlobalSetup(): TRDJSetup;
begin

  if not gSetupIsSet then
    begin

      InitDefaultSetup(Result);
      Exit;
    end;

  Result := FRDJSetup;
end;


function IsGlobalSetupLocked: Boolean;
begin

  Result := gSetupLocked;
end;


function GetDefaultSetupFileName(): string;
begin

  Result := ChangeFileExt(ParamStr(0),
                          '.ini');
end;


procedure LoadRackFromIni(iniFile: TIniFile;
                          const RackSectionBase: string;
                          var ARack: TRDJFxRackSetup);
var
  i: Integer;
  sectionName: string;

begin

  InitDefaultFxRack(ARack);

  ARack.Count := iniFile.ReadInteger(RackSectionBase,
                                     'Count',
                                     ARack.Count);
  if (ARack.Count < 0) then
    ARack.Count := 0;
  if (ARack.Count > MAX_FX_SLOTS) then
    ARack.Count := MAX_FX_SLOTS;

  for i := 0 to ARack.Count - 1 do
    begin

      sectionName := Format('%s.%d',
                            [RackSectionBase, i]);

      ARack.Slots[i].Enabled := iniFile.ReadBool(sectionName,
                                                 'Enabled',
                                                 True);

      ARack.Slots[i].FxKind := IniNameToFxKind(
        iniFile.ReadString(sectionName,
                           'Kind',
                           'Unknown'));
    end;
end;


procedure SaveRackToIni(iniFile: TIniFile;
                        const RackSectionBase: string;
                        const ARack: TRDJFxRackSetup);
var
  i: Integer;
  sectionName: string;

begin

  iniFile.WriteInteger(RackSectionBase,
                       'Count',
                       ARack.Count);

  for i := 0 to ARack.Count - 1 do
    begin

      sectionName := Format('%s.%d',
                            [RackSectionBase, i]);

      iniFile.WriteString(sectionName,
                          'Kind',
                          FxKindToIniName(ARack.Slots[i].FxKind));

      iniFile.WriteBool(sectionName,
                        'Enabled',
                        ARack.Slots[i].Enabled);
    end;
end;


procedure LoadSetupFromIni(const FileName: string;
                           var ASetupRec: TRDJSetup);
var
  iniFile: TIniFile;
  i: Integer;
  sectionName: string;

begin

  InitDefaultSetup(ASetupRec);

  if not FileExists(FileName) then
    Exit;

  iniFile := TIniFile.Create(FileName);

  try

    ASetupRec.AudioBufferMs := iniFile.ReadInteger('Setup',
                                                   'AudioBufferSize',
                                                   ASetupRec.AudioBufferMs);

    ASetupRec.ChannelCount := iniFile.ReadInteger('Setup',
                                                  'ChannelCount',
                                                  ASetupRec.ChannelCount);
    if (ASetupRec.ChannelCount < 1) then
      ASetupRec.ChannelCount := 1;
    if (ASetupRec.ChannelCount > MAX_CHANNELS) then
      ASetupRec.ChannelCount := MAX_CHANNELS;

    ASetupRec.LoopbackDeckCount := iniFile.ReadInteger('Setup',
                                                       'LoopbackDeckCount',
                                                       ASetupRec.LoopbackDeckCount);
    if (ASetupRec.LoopbackDeckCount < 0) then
      ASetupRec.LoopbackDeckCount := 0;
    if (ASetupRec.LoopbackDeckCount > MAX_LOOPBACK_DECKS) then
      ASetupRec.LoopbackDeckCount := MAX_LOOPBACK_DECKS;

    ASetupRec.MasterDeviceId := iniFile.ReadString('Setup',
                                                   'MasterDeviceId',
                                                   ASetupRec.MasterDeviceId);

    ASetupRec.MicDeckEnabled := iniFile.ReadBool('Setup',
                                                 'MicDeckEnabled',
                                                 ASetupRec.MicDeckEnabled);

    ASetupRec.MicDeviceId := iniFile.ReadString('Setup',
                                                'MicDeviceId',
                                                ASetupRec.MicDeviceId);

    ASetupRec.MicDeviceCbItemIndex := iniFile.ReadInteger('Setup',
                                                          'MicDeviceCbItemIndex',
                                                          ASetupRec.MicDeviceCbItemIndex);

    ASetupRec.PFLEnabled := iniFile.ReadBool('Setup',
                                             'PFLEnabled',
                                             ASetupRec.PFLEnabled);

    ASetupRec.PFLDeviceId := iniFile.ReadString('Setup',
                                                'PFLDeviceId',
                                                ASetupRec.PFLDeviceId);

    // Backward compatibility
    if (ASetupRec.PFLDeviceId = '') then
      ASetupRec.PFLDeviceId := iniFile.ReadString('Setup',
                                                  'PhonesDeviceId',
                                                  ASetupRec.PFLDeviceId);

    // Master rack
    LoadRackFromIni(iniFile,
                    'MasterFX',
                    ASetupRec.MasterFxRack);

    // Per-channel racks
    for i := 0 to MAX_CHANNELS - 1 do
      begin
        sectionName := Format('ChannelFX.%d',
                              [i]);

        LoadRackFromIni(iniFile,
                        sectionName,
                        ASetupRec.ChannelFxRacks[i]);
      end;

    // Override system sleep mode setting.
    ASetupRec.SystemOverrideSleepMode := iniFile.ReadBool('Setup',
                                                          'SystemOverrideSleepMode',
                                                          ASetupRec.SystemOverrideSleepMode);

    // Audio recorder
    ASetupRec.AudioRecorderCaptureBufferMs := iniFile.ReadInteger('SetupAudioRecorder',
                                                                'CaptureBufferMs',
                                                                ASetupRec.AudioRecorderCaptureBufferMs);

    ASetupRec.AudioRecorderAutoBufferSize := iniFile.ReadBool('SetupAudioRecorder',
                                                              'AutoBufferSize',
                                                              ASetupRec.AudioRecorderAutoBufferSize);

    ASetupRec.AudioRecorderSystemLatency := iniFile.ReadInteger('SetupAudioRecorder',
                                                                'SystemLatency',
                                                                ASetupRec.AudioRecorderSystemLatency);

    ASetupRec.AudioRecorderDisableMMCSS := iniFile.ReadBool('SetupAudioRecorder',
                                                            'DisableMMCSS',
                                                            ASetupRec.AudioRecorderDisableMMCSS);

    ASetupRec.AudioRecorderDontOverWriteAudioFiles := iniFile.ReadBool('SetupAudioRecorder',
                                                                       'DontOverWriteAudioFiles',
                                                                       ASetupRec.AudioRecorderDontOverWriteAudioFiles);

    ASetupRec.AudioRecorderUsePCMFormat := iniFile.ReadBool('SetupAudioRecorder',
                                                            'UsePCMFormat',
                                                            ASetupRec.AudioRecorderUsePCMFormat);

    ASetupRec.AudioRecorderEnableStreamSwitchDetection := iniFile.ReadBool('SetupAudioRecorder',
                                                                           'EnableStreamSwitchDetection',
                                                                           ASetupRec.AudioRecorderEnableStreamSwitchDetection);

    ASetupRec.AudioRecorderAudioFormat := iniFile.ReadInteger('SetupAudioRecorder',
                                                              'AudioFormat',
                                                              ASetupRec.AudioRecorderAudioFormat);

    ASetupRec.AudioRecorderTapPoint := iniFile.ReadInteger('SetupAudioRecorder',
                                                           'TapPoint',
                                                           ASetupRec.AudioRecorderTapPoint);

    ASetupRec.AudioRecorderRecordPreFx := iniFile.ReadBool('SetupAudioRecorder',
                                                           'RecordPreFx',
                                                           ASetupRec.AudioRecorderRecordPreFx);

    ASetupRec.AudioRecorderRecordPostFx := iniFile.ReadBool('SetupAudioRecorder',
                                                            'RecordPostFx',
                                                            ASetupRec.AudioRecorderRecordPostFx);

    ASetupRec.AudioRecordingsDir := iniFile.ReadString('Dir',
                                                       'Recordings',
                                                       ASetupRec.AudioRecordingsDir);

    ASetupRec.AudioRecordingsPath := iniFile.ReadString('Paths',
                                                         'Recordings',
                                                          ASetupRec.AudioRecordingsPath);

    ASetupRec.VideoRecordingsDir := iniFile.ReadString('Dir',
                                                        'VideoRecordings',
                                                        ASetupRec.VideoRecordingsDir);

    ASetupRec.VideoRecordingsPath := iniFile.ReadString('Paths',
                                                         'VideoRecordings',
                                                         ASetupRec.VideoRecordingsPath);

    // Database
    ASetupRec.DatabaseDir := iniFile.ReadString('Dir',
                                                'Data',
                                                ASetupRec.DatabaseDir);

    ASetupRec.DatabasePath := iniFile.ReadString('Paths',
                                                 'Data',
                                                 ASetupRec.DatabasePath);

    // Covers
    ASetupRec.LocalArtworkDir := iniFile.ReadString('Dir',
                                                   'Artwork',
                                                   ASetupRec.LocalArtworkDir);

    ASetupRec.LocalArtworkPath := iniFile.ReadString('Paths',
                                                     'Artwork',
                                                     ASetupRec.LocalArtworkPath);


    // CaddyConfigFile
    ASetupRec.CaddyDir := iniFile.ReadString('Caddy',
                                             'CaddyDir',
                                             ASetupRec.CaddyDir);

    ASetupRec.CaddyConfigFile := iniFile.ReadString('Caddy',
                                                    'ConfigFile',
                                                    ASetupRec.CaddyConfigFile);

    ASetupRec.CaddyNowPlayingJsonFile := iniFile.ReadString('Caddy',
                                                            'NowPlayingJsonFile',
                                                            ASetupRec.CaddyNowPlayingJsonFile);

    ASetupRec.CaddyArtworkPath := iniFile.ReadString('Caddy',
                                                     'Artwork',
                                                     ASetupRec.CaddyArtworkPath);

    ASetupRec.CaddyVideoPath := iniFile.ReadString('Caddy',
                                                   'Video',
                                                   ASetupRec.CaddyVideoPath);

    // CaddyArtworkPath is authoritative. Paths\Artwork remains a compatibility
    // alias so an old installation cannot silently publish to a second folder.
    ASetupRec.LocalArtworkPath := ASetupRec.CaddyArtworkPath;
    ASetupRec.LocalArtworkDir := ExtractFileName(ExcludeTrailingPathDelimiter(ASetupRec.CaddyArtworkPath));

    ASetupRec.CaddyLanAddress := iniFile.ReadString('Caddy',
                                                    'LanAddress',
                                                    ASetupRec.CaddyLanAddress);

    ASetupRec.CaddyMount := iniFile.ReadString('Caddy',
                                               'Mount',
                                               ASetupRec.CaddyMount);

    ASetupRec.CaddyContentTypeURL := iniFile.ReadString('Caddy',
                                                        'ContentTypeURL',
                                                        ASetupRec.CaddyContentTypeURL);

    ASetupRec.CaddyCommand := iniFile.ReadString('Caddy',
                                                 'Command',
                                                 ASetupRec.CaddyCommand);

    ASetupRec.CaddyLogFile := iniFile.ReadString('Caddy',
                                                 'LogFile',
                                                 ASetupRec.CaddyLogFile); // << FIXED, Not part of the setup GUI.

    // Media server.
    // New unified section name is MediaServer. For older ini files we still read
    // the old RdjPro section once, but SaveSetupToIni writes MediaServer only.
    sectionName := 'MediaServer';
    if (not iniFile.SectionExists(sectionName)) and
       iniFile.SectionExists('RdjPro') then
      sectionName := 'RdjPro';

    ASetupRec.MediaServerEnabled := iniFile.ReadBool(sectionName,
                                                     'Enabled',
                                                     False);

    ASetupRec.MsePublicSegmentTargetMs := ClampInteger(iniFile.ReadInteger(sectionName,
                                                                           'MsePublicFragmentTargetMs',
                                                                           ASetupRec.MsePublicSegmentTargetMs),
                                                                           RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS,
                                                                           RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS);
    // Camera
    ASetupRec.CameraName := iniFile.ReadString('Camera',
                                               'CameraName',
                                               '');

    ASetupRec.CameraSymbolicLink := iniFile.ReadString('Camera',
                                                       'CameraSymbolicLink',
                                                       '');
  finally

    iniFile.Free;
  end;
end;


procedure SaveSetupToIni(const FileName: string;
                         const ASetupRec: TRDJSetup);
var
  iniFile: TIniFile;
  i: Integer;
  sectionName: string;

begin

  iniFile := TIniFile.Create(FileName);
  try

    iniFile.WriteInteger('Setup',
                         'AudioBufferSize',
                         ASetupRec.AudioBufferMs);

    iniFile.WriteInteger('Setup',
                         'ChannelCount',
                         ASetupRec.ChannelCount);

    iniFile.WriteInteger('Setup',
                         'LoopbackDeckCount',
                         ASetupRec.LoopbackDeckCount);

    iniFile.WriteString('Setup',
                        'MasterDeviceId',
                        ASetupRec.MasterDeviceId);

    iniFile.WriteBool('Setup',
                      'PFLEnabled',
                      ASetupRec.PFLEnabled);

    iniFile.WriteString('Setup',
                        'PFLDeviceId',
                        ASetupRec.PFLDeviceId);

    iniFile.WriteBool('Setup',
                      'MicDeckEnabled',
                      ASetupRec.MicDeckEnabled);

    iniFile.WriteString('Setup',
                        'MicDeviceId',
                        ASetupRec.MicDeviceId);

    iniFile.WriteInteger('Setup',
                         'MicDeviceCbItemIndex',
                         ASetupRec.MicDeviceCbItemIndex);

    // Override system sleep mode setting.
    iniFile.WriteBool('Setup',
                      'SystemOverrideSleepMode',
                      ASetupRec.SystemOverrideSleepMode);

    // Audio recorder
    iniFile.WriteInteger('SetupAudioRecorder',
                         'CaptureBufferMs',
                         ASetupRec.AudioRecorderCaptureBufferMs);

    iniFile.WriteBool('SetupAudioRecorder',
                      'AutoBufferSize',
                      ASetupRec.AudioRecorderAutoBufferSize);

    iniFile.WriteInteger('SetupAudioRecorder',
                         'SystemLatency',
                         ASetupRec.AudioRecorderSystemLatency);

    iniFile.WriteBool('SetupAudioRecorder',
                      'DisableMMCSS',
                      ASetupRec.AudioRecorderDisableMMCSS);

    iniFile.WriteBool('SetupAudioRecorder',
                      'DontOverWriteAudioFiles',
                      ASetupRec.AudioRecorderDontOverWriteAudioFiles);

    iniFile.WriteBool('SetupAudioRecorder',
                      'UsePCMFormat',
                      ASetupRec.AudioRecorderUsePCMFormat);

    iniFile.WriteBool('SetupAudioRecorder',
                      'EnableStreamSwitchDetection',
                      ASetupRec.AudioRecorderEnableStreamSwitchDetection);

    iniFile.WriteInteger('SetupAudioRecorder',
                         'AudioFormat',
                         ASetupRec.AudioRecorderAudioFormat);

    iniFile.WriteBool('SetupAudioRecorder',
                      'RecordPreFx',
                      ASetupRec.AudioRecorderRecordPreFx);

    iniFile.WriteBool('SetupAudioRecorder',
                      'RecordPostFx',
                      ASetupRec.AudioRecorderRecordPostFx);

    // Master rack.
    SaveRackToIni(iniFile,
                  'MasterFX',
                  ASetupRec.MasterFxRack);

    // Per-channel racks.
    for i := 0 to MAX_CHANNELS - 1 do
      begin

        sectionName := Format('ChannelFX.%d',
                              [i]);

        SaveRackToIni(iniFile,
                      sectionName,
                      ASetupRec.ChannelFxRacks[i]);
      end;

    // Mixer tap point for recorder.
    iniFile.WriteInteger('SetupAudioRecorder',
                         'TapPoint',
                         ASetupRec.AudioRecorderTapPoint);

    // Recordings dir + path
    iniFile.WriteString('Dir',
                        'Recordings',
                        ASetupRec.AudioRecordingsDir);

    iniFile.WriteString('Paths',
                        'Recordings',
                        ASetupRec.AudioRecordingsPath);

    iniFile.WriteString('Dir',
                        'VideoRecordings',
                        ASetupRec.VideoRecordingsDir);

    iniFile.WriteString('Paths',
                        'VideoRecordings',
                        ASetupRec.VideoRecordingsPath);

    // Database dir + path
    iniFile.WriteString('Dir',
                        'Data',
                        ASetupRec.DatabaseDir);

    iniFile.WriteString('Paths',
                        'Data',
                        ASetupRec.DatabasePath);

    // Covers dir + path
    iniFile.WriteString('Dir',
                        'Artwork',
                        ExtractFileName(ExcludeTrailingPathDelimiter(ASetupRec.CaddyArtworkPath)));

    iniFile.WriteString('Paths',
                        'Artwork',
                        ASetupRec.CaddyArtworkPath);


    // Caddy/json
    iniFile.WriteString('Caddy',
                        'CaddyDir',
                        ASetupRec.CaddyDir);

    iniFile.WriteString('Caddy',
                        'ConfigFile',
                        ASetupRec.CaddyConfigFile);

    iniFile.WriteString('Caddy',
                        'NowPlayingJsonFile',
                        ASetupRec.CaddyNowPlayingJsonFile);

    iniFile.WriteString('Caddy',
                        'Artwork',
                        ASetupRec.CaddyArtworkPath);

    iniFile.WriteString('Caddy',
                        'Video',
                        ASetupRec.CaddyVideoPath);

    iniFile.WriteString('Caddy',
                        'LanAddress',
                        ASetupRec.CaddyLanAddress);

    iniFile.WriteString('Caddy',
                        'Mount',
                        ASetupRec.CaddyMount);

    iniFile.WriteString('Caddy',
                        'ContentTypeURL',
                        ASetupRec.CaddyContentTypeURL);

    iniFile.WriteString('Caddy',
                        'Command',
                        ASetupRec.CaddyCommand);

    iniFile.WriteString('Caddy',
                        'LogFile',
                        ASetupRec.CaddyLogFile);  // Fixed, not part of the setup GUI.

    // Media server

    iniFile.WriteBool('MediaServer',
                      'Enabled',
                      ASetupRec.MediaServerEnabled);

    iniFile.WriteInteger('MediaServer',
                         'MsePublicFragmentTargetMs',
                         ClampInteger(ASetupRec.MsePublicSegmentTargetMs,
                                      RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS,
                                      RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS));

    // Camera
    iniFile.WriteString('Camera',
                        'CameraName',
                        ASetupRec.CameraName);

    iniFile.WriteString('Camera',
                        'CameraSymbolicLink',
                        ASetupRec.CameraSymbolicLink);
  finally

    iniFile.Free;
  end;
end;

end.

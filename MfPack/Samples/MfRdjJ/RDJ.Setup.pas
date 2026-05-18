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

  // Icecast
  TRDJBroadcastCodec = (bcAac,
                        bcMp3);

  TRDJBroadcastTapPoint = (btpPreMasterFx,
                           btpPostMasterFx);

  TRDJBroadcastSetup = record
    Enabled: Boolean; // Do not store in ini!

    Host: string;
    Port: Integer;
    Mount: string;
    Username: string;
    Password: string;

    StreamName: string;
    Description: string;
    Genre: string;
    Url: string;
    PublicStream: Boolean;

    Codec: TRDJBroadcastCodec;
    BitrateKbps: Integer;
    SampleRate: Integer;
    Channels: Integer;

    TapPoint: TRDJBroadcastTapPoint;
    AutoReconnect: Boolean;
    BroadcastGainDb: Single;
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

    // AudioClient buffersize (default = 60 ms).
    AudioBufferMs: Integer;

    // Audio recorder
    AudioRecorderCaptureBufferSize: LONGLONG;
    AudioRecorderSystemLatency: LONGLONG;
    AudioRecorderAutoBufferSize: LongBool;
    AudioRecorderDisableMMCSS: Boolean;
    AudioRecorderDontOverWriteAudioFiles: Boolean;
    AudioRecorderUsePCMFormat: Boolean;
    AudioRecorderEnableStreamSwitchDetection: Boolean;
    AudioRecorderAudioFormat: Integer;
    AudioRecorderRecordPreFx: Boolean;
    AudioRecorderRecordPostFx: Boolean;

    // Icecast broadcast
    Broadcast: TRDJBroadcastSetup;
    // Icecast server process manager settings.
    IcecastExePath: string;
    IcecastConfigPath: string;
    IcecastWorkingDir: string;
    IcecastHost: string;
    IcecastPort: Word;
    IcecastHttpPath: string;
    IcecastAutoRestart: Boolean;
    IcecastRestartDelayMs: Cardinal;
    // IceCast/Caddy/json settings
    IcecastCaddyDir: string;
    IcecastCaddyConfigFile: string;
    IcecastNowPlayingJsonFile: string;
    IcecastCaddyCoversPath: string;
    IcecastCaddyCommand: string;
    IcecastCaddyLogFile: string;

    // FX racks.
    MasterFxRack: TRDJFxRackSetup;
    ChannelFxRacks: array[0..MAX_CHANNELS - 1] of TRDJFxRackSetup;

    // Recording from tap point.
    AudioRecorderTapPoint: Integer;

    // Paths
    AudioRecordingsDir: string;   // text
    AudioRecordingsPath: string;  // hint
    DatabaseDir: string;  // text
    DatabasePath: string; // hint
    LocalCoversDir: string; // text
    LocalCoversPath: string; // hint

    // CarmenH
    CarmenProEnabled: Boolean;
    CarmenProObsExePath: string;
    CarmenProObsWorkingDir: string;
    CarmenProObsProfileName: string;
    CarmenProObsSceneCollection: string;
    CarmenProMetadataJsonFile: string;
    CarmenProCameraName: string;
    CarmenProCameraSymbolicLink: string;
    CarmenProAutoStartObs: Boolean;
    CarmenProAutoWriteMetadata: Boolean;

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


implementation

var
  gSetup: TRDJSetup;
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

  // Icecast
  // Default settings.

  //ASetupRec.Broadcast.Enabled := False;  do not use it.

  ASetupRec.Broadcast.Host := '127.0.0.1';
  ASetupRec.Broadcast.Port := 8000;
  ASetupRec.Broadcast.Mount := '/live';
  ASetupRec.Broadcast.Username := 'source';
  ASetupRec.Broadcast.Password := '';

  ASetupRec.Broadcast.StreamName := 'RDJ FactoryX Live';
  ASetupRec.Broadcast.Description := '';
  ASetupRec.Broadcast.Genre := '';
  ASetupRec.Broadcast.Url := '';
  ASetupRec.Broadcast.PublicStream := False;

  ASetupRec.Broadcast.Codec := bcAac;
  ASetupRec.Broadcast.BitrateKbps := 128;
  ASetupRec.Broadcast.SampleRate := 44100;
  ASetupRec.Broadcast.Channels := 2;

  ASetupRec.Broadcast.TapPoint := btpPostMasterFx;
  ASetupRec.Broadcast.AutoReconnect := True;
  ASetupRec.Broadcast.BroadcastGainDb := 0.0;

  // Icecast server manager
  // Default settings.
  ASetupRec.IcecastExePath := Format('%s\icecast\icecast.exe',
                                     [ExtractFileDir(Application.ExeName)]);

  ASetupRec.IcecastConfigPath := Format('%s\icecast\icecast.xml',
                                        [ExtractFileDir(Application.ExeName)]);

  ASetupRec.IcecastWorkingDir := Format('%s\icecast\',
                                        [ExtractFileDir(Application.ExeName)]);

  // NOTE: Host and port belongs to the server and can be different from the client,
  //       because the client can login to other servers too.
  // Default settings.
  ASetupRec.IcecastHost := '127.0.0.1';
  ASetupRec.IcecastPort := 8000;
  ASetupRec.IcecastHttpPath := '/';
  ASetupRec.IcecastAutoRestart := True;
  ASetupRec.IcecastRestartDelayMs := 3000;

  // IceCast/Caddy/json.
  // Default settings.
  ASetupRec.IcecastCaddyDir := 'C:\Caddy';
  ASetupRec.IcecastCaddyConfigFile := 'C:\Caddy\caddy.cff';
  ASetupRec.IcecastNowPlayingJsonFile := 'C:\Caddy\nowplaying.json';
  ASetupRec.IcecastCaddyCoversPath := 'C:\Caddy';
  ASetupRec.IcecastCaddyCommand := 'C:\caddy.exe run --config "C:\Caddy\Caddy.cff" --adapter caddyfile'; // We need to avoid false uri, better to place Caddy under dir RDJ?
  ASetupRec.IcecastCaddyLogFile := 'Caddy.log';

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

  ASetupRec.AudioRecordingsDir := 'Recordings';
  ASetupRec.AudioRecordingsPath := Format('%s\%s\', [ExtractFileDir(Application.ExeName),
                                          ASetupRec.AudioRecordingsDir]);

  if not DirectoryExists(ASetupRec.AudioRecordingsPath) then
    if not CreateDir(ASetupRec.AudioRecordingsPath) then
      ASetupRec.AudioRecordingsPath := ExpandFileName(ExtractFileDir(Application.ExeName));

  ASetupRec.DatabaseDir := 'Data';
  ASetupRec.DatabasePath := Format('%s\%s\', [ExtractFileDir(Application.ExeName),
                                          ASetupRec.DatabaseDir]);

  if not DirectoryExists(ASetupRec.DatabasePath) then
    if not CreateDir(ASetupRec.DatabasePath) then
      ASetupRec.DatabasePath := ExpandFileName(ExtractFileDir(Application.ExeName));

  ASetupRec.LocalCoversDir := 'Covers';
  ASetupRec.LocalCoversPath := Format('%s\%s\', [ExtractFileDir(Application.ExeName),
                                                 ASetupRec.LocalCoversDir]);

  if not DirectoryExists(ASetupRec.LocalCoversPath) then
    if not CreateDir(ASetupRec.LocalCoversPath) then
      ASetupRec.LocalCoversPath := ExpandFileName(ExtractFileDir(Application.ExeName));

  // CarmenPro
  ASetupRec.CarmenProEnabled := False;
  ASetupRec.CarmenProObsExePath := '';
  ASetupRec.CarmenProObsWorkingDir := '';
  ASetupRec.CarmenProObsProfileName := 'CarmenPro';
  ASetupRec.CarmenProObsSceneCollection := 'CarmenPro';
  ASetupRec.CarmenProMetadataJsonFile := IncludeTrailingPathDelimiter(ASetupRec.IcecastCaddyDir) + 'carmenpro.json';
  ASetupRec.CarmenProCameraName := '';
  ASetupRec.CarmenProCameraSymbolicLink := '';
  ASetupRec.CarmenProAutoStartObs := False;
  ASetupRec.CarmenProAutoWriteMetadata := True;
end;


procedure SetGlobalSetupOnce(const ASetupRec: TRDJSetup);
begin

  if gSetupLocked then
    Exit;

  gSetup := ASetupRec;
  gSetupIsSet := True;
end;


procedure LockGlobalSetup();
begin

  gSetupLocked := True;
end;


function GetGlobalSetup: TRDJSetup;
begin

  if not gSetupIsSet then
    begin

      InitDefaultSetup(Result);
      Exit;
    end;

  Result := gSetup;
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



    // Audio recorder
    ASetupRec.AudioRecorderCaptureBufferSize := iniFile.ReadInt64('SetupAudioRecorder',
                                                                  'CaptureBufferSize',
                                                                  ASetupRec.AudioRecorderCaptureBufferSize);

    ASetupRec.AudioRecorderAutoBufferSize := iniFile.ReadBool('SetupAudioRecorder',
                                                              'AutoBufferSize',
                                                              ASetupRec.AudioRecorderAutoBufferSize);

    ASetupRec.AudioRecorderSystemLatency := iniFile.ReadInt64('SetupAudioRecorder',
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

    // Database
    ASetupRec.DatabaseDir := iniFile.ReadString('Dir',
                                                'Data',
                                                ASetupRec.DatabaseDir);

    ASetupRec.DatabasePath := iniFile.ReadString('Paths',
                                                 'Data',
                                                 ASetupRec.DatabasePath);

    // Covers
    ASetupRec.LocalCoversDir := iniFile.ReadString('Dir',
                                                   'Covers',
                                                   ASetupRec.LocalCoversDir);

    ASetupRec.LocalCoversPath := iniFile.ReadString('Paths',
                                                    'Covers',
                                                    ASetupRec.LocalCoversPath);

    // Icecast
    //ASetupRec.Broadcast.Enabled := iniFile.ReadBool('SetupBroadcast',
    //                                                'Enabled',
    //                                                ASetupRec.Broadcast.Enabled);

    ASetupRec.Broadcast.Host := iniFile.ReadString('SetupBroadcast',
                                                   'Host',
                                                   ASetupRec.Broadcast.Host);

    ASetupRec.Broadcast.Port := iniFile.ReadInteger('SetupBroadcast',
                                                    'Port',
                                                    ASetupRec.Broadcast.Port);

    ASetupRec.Broadcast.Mount := iniFile.ReadString('SetupBroadcast',
                                                    'Mount',
                                                    ASetupRec.Broadcast.Mount);

    ASetupRec.Broadcast.Username := iniFile.ReadString('SetupBroadcast',
                                                       'Username',
                                                       ASetupRec.Broadcast.Username);

    ASetupRec.Broadcast.Password := iniFile.ReadString('SetupBroadcast',
                                                       'Password',
                                                       ASetupRec.Broadcast.Password);

    ASetupRec.Broadcast.StreamName := iniFile.ReadString('SetupBroadcast',
                                                         'StreamName',
                                                         ASetupRec.Broadcast.StreamName);

    ASetupRec.Broadcast.Description := iniFile.ReadString('SetupBroadcast',
                                                          'Description',
                                                          ASetupRec.Broadcast.Description);

    ASetupRec.Broadcast.Genre := iniFile.ReadString('SetupBroadcast',
                                                    'Genre',
                                                    ASetupRec.Broadcast.Genre);

    ASetupRec.Broadcast.Url := iniFile.ReadString('SetupBroadcast',
                                                  'Url',
                                                  ASetupRec.Broadcast.Url);

    ASetupRec.Broadcast.PublicStream := iniFile.ReadBool('SetupBroadcast',
                                                         'PublicStream',
                                                         ASetupRec.Broadcast.PublicStream);

    ASetupRec.Broadcast.Codec := TRDJBroadcastCodec(iniFile.ReadInteger('SetupBroadcast',
                                                                        'Codec',
                                                                        Ord(ASetupRec.Broadcast.Codec)));

    ASetupRec.Broadcast.BitrateKbps := iniFile.ReadInteger('SetupBroadcast',
                                                           'BitrateKbps',
                                                           ASetupRec.Broadcast.BitrateKbps);

    ASetupRec.Broadcast.SampleRate := iniFile.ReadInteger('SetupBroadcast',
                                                          'SampleRate',
                                                          ASetupRec.Broadcast.SampleRate);

    ASetupRec.Broadcast.Channels := iniFile.ReadInteger('SetupBroadcast',
                                                        'Channels',
                                                        ASetupRec.Broadcast.Channels);

    ASetupRec.Broadcast.TapPoint := TRDJBroadcastTapPoint(iniFile.ReadInteger('SetupBroadcast',
                                                          'TapPoint',
                                                          Ord(ASetupRec.Broadcast.TapPoint)));

    ASetupRec.Broadcast.AutoReconnect := iniFile.ReadBool('SetupBroadcast',
                                                          'AutoReconnect',
                                                          ASetupRec.Broadcast.AutoReconnect);

    ASetupRec.Broadcast.BroadcastGainDb := iniFile.ReadFloat('SetupBroadcast',
                                                             'BroadcastGainDb',
                                                             ASetupRec.Broadcast.BroadcastGainDb);

    // Icecast server manager
    ASetupRec.IcecastExePath := iniFile.ReadString('SetupIcecastServer',
                                                   'ExePath',
                                                   ASetupRec.IcecastExePath);

    ASetupRec.IcecastConfigPath := iniFile.ReadString('SetupIcecastServer',
                                                      'ConfigPath',
                                                      ASetupRec.IcecastConfigPath);

    ASetupRec.IcecastWorkingDir := iniFile.ReadString('SetupIcecastServer',
                                                      'WorkingDir',
                                                      ASetupRec.IcecastWorkingDir);

    ASetupRec.IcecastHost := iniFile.ReadString('SetupIcecastServer',
                                                'Host',
                                                ASetupRec.IcecastHost);

    ASetupRec.IcecastPort := Word(iniFile.ReadInteger('SetupIcecastServer',
                                                      'Port',
                                                      ASetupRec.IcecastPort));

    ASetupRec.IcecastHttpPath := iniFile.ReadString('SetupIcecastServer',
                                                    'HttpPath',
                                                    ASetupRec.IcecastHttpPath);

    ASetupRec.IcecastAutoRestart := iniFile.ReadBool('SetupIcecastServer',
                                                     'AutoRestart',
                                                     ASetupRec.IcecastAutoRestart);

    ASetupRec.IcecastRestartDelayMs := Cardinal(iniFile.ReadInteger('SetupIcecastServer',
                                                                    'RestartDelayMs',
                                                                    Integer(ASetupRec.IcecastRestartDelayMs)));

    //  IcecastCaddyConfigFile
    ASetupRec.IcecastCaddyDir := iniFile.ReadString('Icecast',
                                                    'CaddyDir',
                                                    ASetupRec.IcecastCaddyDir);

    ASetupRec.IcecastCaddyConfigFile := iniFile.ReadString('Icecast',
                                                           'CaddyConfigFile',
                                                           ASetupRec.IcecastCaddyConfigFile);

    ASetupRec.IcecastNowPlayingJsonFile := iniFile.ReadString('Icecast',
                                                              'NowPlayingJsonFile',
                                                              ASetupRec.IcecastNowPlayingJsonFile);

    ASetupRec.IcecastCaddyCoversPath := iniFile.ReadString('Icecast',
                                                           'Covers',
                                                           ASetupRec.IcecastCaddyCoversPath);

    ASetupRec.IcecastCaddyCommand := iniFile.ReadString('Icecast',
                                                        'CaddyCommand',
                                                        ASetupRec.IcecastCaddyCoversPath);

    ASetupRec.IcecastCaddyLogFile := iniFile.ReadString('Icecast',
                                                        'CaddyLogFile',
                                                        ASetupRec.IcecastCaddyLogFile); // << FIXED, Not part of the setup GUI.
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

    // Audio recorder
    iniFile.WriteInt64('SetupAudioRecorder',
                       'CaptureBufferSize',
                       ASetupRec.AudioRecorderCaptureBufferSize);

    iniFile.WriteBool('SetupAudioRecorder',
                      'AutoBufferSize',
                      ASetupRec.AudioRecorderAutoBufferSize);

    iniFile.WriteInt64('SetupAudioRecorder',
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

    // Database dir + path
    iniFile.WriteString('Dir',
                        'Data',
                        ASetupRec.DatabaseDir);

    iniFile.WriteString('Paths',
                        'Data',
                        ASetupRec.DatabasePath);

    // Covers dir + path
    iniFile.WriteString('Dir',
                        'Covers',
                        ASetupRec.LocalCoversDir);

    iniFile.WriteString('Paths',
                        'Covers',
                        ASetupRec.LocalCoversPath);

    // Icecast
    //iniFile.WriteBool('SetupBroadcast',
    //                  'Enabled',
    //                  ASetupRec.Broadcast.Enabled);

    iniFile.WriteString('SetupBroadcast',
                        'Host',
                        ASetupRec.Broadcast.Host);

    iniFile.WriteInteger('SetupBroadcast',
                         'Port',
                         ASetupRec.Broadcast.Port);

    iniFile.WriteString('SetupBroadcast',
                        'Mount',
                        ASetupRec.Broadcast.Mount);

    iniFile.WriteString('SetupBroadcast',
                        'Username',
                        ASetupRec.Broadcast.Username);

    iniFile.WriteString('SetupBroadcast',
                        'Password',
                        ASetupRec.Broadcast.Password);

    iniFile.WriteString('SetupBroadcast',
                        'StreamName',
                        ASetupRec.Broadcast.StreamName);

    iniFile.WriteString('SetupBroadcast',
                        'Description',
                        ASetupRec.Broadcast.Description);

    iniFile.WriteString('SetupBroadcast',
                        'Genre',
                        ASetupRec.Broadcast.Genre);

    iniFile.WriteString('SetupBroadcast',
                        'Url',
                        ASetupRec.Broadcast.Url);

    iniFile.WriteBool('SetupBroadcast',
                      'PublicStream',
                      ASetupRec.Broadcast.PublicStream);

    iniFile.WriteInteger('SetupBroadcast',
                         'Codec',
                         Ord(ASetupRec.Broadcast.Codec));

    iniFile.WriteInteger('SetupBroadcast',
                         'BitrateKbps',
                         ASetupRec.Broadcast.BitrateKbps);

    iniFile.WriteInteger('SetupBroadcast',
                         'SampleRate',
                         ASetupRec.Broadcast.SampleRate);

    iniFile.WriteInteger('SetupBroadcast',
                         'Channels',
                         ASetupRec.Broadcast.Channels);

    iniFile.WriteInteger('SetupBroadcast',
                         'TapPoint',
                         Ord(ASetupRec.Broadcast.TapPoint));

    iniFile.WriteBool('SetupBroadcast',
                      'AutoReconnect',
                      ASetupRec.Broadcast.AutoReconnect);

    iniFile.WriteFloat('SetupBroadcast',
                       'BroadcastGainDb',
                       ASetupRec.Broadcast.BroadcastGainDb);

    // Icecast server manager
    iniFile.WriteString('SetupIcecastServer',
                        'ExePath',
                        ASetupRec.IcecastExePath);

    iniFile.WriteString('SetupIcecastServer',
                        'ConfigPath',
                        ASetupRec.IcecastConfigPath);

    iniFile.WriteString('SetupIcecastServer',
                        'WorkingDir',
                        ASetupRec.IcecastWorkingDir);

    iniFile.WriteString('SetupIcecastServer',
                        'Host',
                        ASetupRec.IcecastHost);

    iniFile.WriteInteger('SetupIcecastServer',
                         'Port',
                         ASetupRec.IcecastPort);

    iniFile.WriteString('SetupIcecastServer',
                        'HttpPath',
                        ASetupRec.IcecastHttpPath);

    iniFile.WriteBool('SetupIcecastServer',
                      'AutoRestart',
                      ASetupRec.IcecastAutoRestart);

    iniFile.WriteInteger('SetupIcecastServer',
                         'RestartDelayMs',
                         Integer(ASetupRec.IcecastRestartDelayMs));

    // IceCast/Caddy/json
    iniFile.WriteString('Icecast',
                        'CaddyDir',
                        ASetupRec.IcecastCaddyDir);

    iniFile.WriteString('Icecast',
                        'CaddyConfigFile',
                        ASetupRec.IcecastCaddyConfigFile);

    iniFile.WriteString('Icecast',
                        'NowPlayingJsonFile',
                        ASetupRec.IcecastNowPlayingJsonFile);

    iniFile.WriteString('Icecast',
                        'Covers',
                        ASetupRec.IcecastCaddyCoversPath);

    iniFile.WriteString('Icecast',
                        'CaddyCommand',
                        ASetupRec.IcecastCaddyCommand);

    iniFile.WriteString('Icecast',
                        'CaddyLogFile',
                        ASetupRec.IcecastCaddyLogFile);  // Fixed, not part of the setup GUI.

  finally

    iniFile.Free;
  end;
end;

end.

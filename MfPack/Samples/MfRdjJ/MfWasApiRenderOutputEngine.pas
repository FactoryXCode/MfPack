// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfWasApiRenderOutputEngine.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: The main WASAPI output renderer.
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
unit MfWasApiRenderOutputEngine;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Services.Avrt,
  {MediaFoundationApi}
  Winapi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  Winapi.CoreAudioApi.MMDeviceApi,
  Winapi.CoreAudioApi.AudioClient,
  Winapi.CoreAudioApi.AudioSessionTypes,
  Winapi.CoreAudioApi.AudioPolicy,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  MfWasApiEffectsRack;

type

  TDeviceState = (dsUninitialized,
                  dsInitializing,
                  dsReady,
                  dsPlaying,
                  dsPaused,
                  dsStopping,
                  dsStopped,
                  dsError);

  TWasApiStateEvent = procedure(Sender: TObject;
                                const NewState: TDeviceState) of object;

  TWasApiErrorEvent = procedure(Sender: TObject;
                                const Hr: HRESULT;
                                const Msg: string) of object;

  TOnFillRenderPcm = function(Sender: TObject;
                              pData: PByte;
                              const ByteCount: DWORD;
                              pwfx: PWAVEFORMATEX;
                              out Flags: DWORD): HRESULT of object;

  TWasApiProcessedEvent = procedure(Sender: TObject;
                                    const Position100ns: Int64;
                                    const RawPosition: UInt64) of object;

  TMfWasApiRenderOutputEngine = class

  private

    FStopping: LongBool;
    FDestroying: LongBool;

    FOnFillPcm: TOnFillRenderPcm;
    FOnStateChanged: TWasApiStateEvent;
    FOnError: TWasApiErrorEvent;
    FOnProcessed: TWasApiProcessedEvent;

    pvMMDevice: IMMDevice;
    pvAudioClient: IAudioClient;
    pvRenderClient: IAudioRenderClient;
    pvAudioClock: IAudioClock;
    pvAudioStreamVolume: IAudioStreamVolume;
    pvSimpleVol: ISimpleAudioVolume;

    pvAudioClockFrequency: UInt64;

    pvAudioSamplesReadyEvent: THandle;
    pvShutdownEvent: THandle;
    pvRenderThread: THandle;
    pvRenderThreadId: DWORD;

    pvDeviceState: TDeviceState;
    pvBufferFrameCount: UINT32;
    pvSoundChannels: Word;

    FClientBlockAlign: Integer;
    FBytesPerSample: Integer;
    FWaveFormat: TWAVEFORMATEX;
    FHasPreparedFormat: Boolean;

    FUseDefaultDevice: Boolean;
    FDeviceRole: ERole;
    FOutputDeviceId: string;
    FLastQueuedPosition100ns: Int64;

    // Needed for MMCSS.
    FAvrtHandle: THandle;
    FAvrtIndex: DWORD;

    // FX Rack.
    FAudioRack : TMfWasApiEffectsRack;

    function WaitForRenderThreadToExit(const ATimeoutMs: DWORD = 3000): HRESULT;
    procedure SetDeviceState(const AState: TDeviceState);
    procedure DoError(const Hr: HRESULT;
                      const Msg: string);
    procedure RaiseProcessed(const Position100ns: Int64;
                             const RawPosition: UInt64);

    procedure ReleaseAudioInterfaces();
    function CreateRenderDevice(): HRESULT;
    function InitializeAudioEngine(): HRESULT;
    function SetFormatInternal(pwfx: PWAVEFORMATEX): HRESULT;
    function PlayAudioStreamInternal(): HRESULT;
    function StopAudioStreamInternal(): HRESULT;
    function PauseAudioStreamInternal(): HRESULT;
    function RenderLoop(): HRESULT;

    class function RenderThreadProc(Parameter: Pointer): DWORD; stdcall; static;

    procedure EnableMmcss();
    procedure DisableMmcss();

  public

    constructor Create();
    destructor Destroy(); override;

    function Prepare(pwfx: PWAVEFORMATEX): HRESULT;
    function Start(): HRESULT;
    function Stop(): HRESULT;
    function Pause(): HRESULT;

    procedure SetUseDefaultOutputDevice(const ARole: ERole = eMultimedia);
    procedure SetOutputDeviceId(const ADeviceId: string);

    function Mute(pActive: Boolean): Boolean;
    function SetVolumes(pVolLeft,
                        pVolRight: Single): HRESULT;

    property DeviceState: TDeviceState read pvDeviceState;
    property SoundChannels: Word read pvSoundChannels;
    property BufferFrameCount: UInt32 read pvBufferFrameCount;
    property WaveFormat: WaveFormatEx read FWaveFormat;

    // FX rack.
    property AudioRack: TMfWasApiEffectsRack read FAudioRack;

    property OnFillPcm: TOnFillRenderPcm read FOnFillPcm write FOnFillPcm;
    property OnStateChanged: TWasApiStateEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TWasApiErrorEvent read FOnError write FOnError;
    property OnProcessed: TWasApiProcessedEvent read FOnProcessed write FOnProcessed;
  end;


implementation

uses
  frmMainMDI,
  RDJ.Setup;


function WideStringToLPWSTR(const S: string): LPWSTR;
begin

  Result := LPWSTR(WideString(S));
end;


{ TMfWasApiRenderOutputEngine }

constructor TMfWasApiRenderOutputEngine.Create();
begin

  inherited Create;

  pvAudioSamplesReadyEvent := CreateEvent(nil,
                                          False,
                                          False,
                                          nil);
  pvShutdownEvent := CreateEvent(nil,
                                 True,
                                 False,
                                 nil);

  pvRenderThread := 0;
  pvRenderThreadId := 0;
  pvDeviceState := dsUninitialized;
  FStopping := False;
  FDestroying := False;

  FUseDefaultDevice := True;
  FDeviceRole := eMultimedia;
  FOutputDeviceId := '';
  FHasPreparedFormat := False;
  FLastQueuedPosition100ns := -1;
  pvAudioClockFrequency := 0;

  ZeroMemory(@FWaveFormat,
             SizeOf(FWaveFormat));
  // FX
  FAudioRack := TMfWasApiEffectsRack.Create(nil);
end;


destructor TMfWasApiRenderOutputEngine.Destroy();
begin

  FDestroying := True;
  FStopping := True;

  FOnFillPcm := nil;
  FOnStateChanged := nil;
  FOnError := nil;
  FOnProcessed := nil;

  Stop();

  ReleaseAudioInterfaces();

  if (pvAudioSamplesReadyEvent <> 0) then
    begin

      CloseHandle(pvAudioSamplesReadyEvent);
      pvAudioSamplesReadyEvent := 0;
    end;

  if (pvShutdownEvent <> 0) then
    begin

      CloseHandle(pvShutdownEvent);
      pvShutdownEvent := 0;
    end;

  FreeAndNil(FAudioRack);

  inherited Destroy;
end;


procedure TMfWasApiRenderOutputEngine.DoError(const Hr: HRESULT;
                                              const Msg: string);
begin

  if FDestroying then
    Exit;

  pvDeviceState := dsError;

  if Assigned(FOnError) then
    FOnError(Self,
             Hr,
             Msg);

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self,
                    pvDeviceState);
end;


procedure TMfWasApiRenderOutputEngine.RaiseProcessed(const Position100ns: Int64;
                                                     const RawPosition: UInt64);
const
  MIN_PROCESS_NOTIFY_100NS = REFTIMES_PER_MILLISEC * 50;

var
  QueuedPosition100ns: Int64;

begin

  if FDestroying or FStopping then
    Exit;

  if not Assigned(FOnProcessed) then
    Exit;

  if (Position100ns <> 0) and
     (FLastQueuedPosition100ns >= 0) and
     ((Position100ns - FLastQueuedPosition100ns) < MIN_PROCESS_NOTIFY_100NS) then
    Exit;

  FLastQueuedPosition100ns := Position100ns;
  QueuedPosition100ns := Position100ns;

  TThread.Queue(nil,
                procedure
                begin

                  if FDestroying or FStopping then
                    Exit;

                  if Assigned(FOnProcessed) then
                    FOnProcessed(Self,
                                 QueuedPosition100ns,
                                 RawPosition);
                end);
end;


function TMfWasApiRenderOutputEngine.WaitForRenderThreadToExit(const ATimeoutMs: DWORD): HRESULT;
var
  WaitRes: DWORD;

begin

  Result := S_OK;

  if (pvRenderThread = 0) then
    Exit;

  WaitRes := WaitForSingleObject(pvRenderThread,
                                 ATimeoutMs);

  case WaitRes of
    WAIT_OBJECT_0:
      begin

        CloseHandle(pvRenderThread);
        pvRenderThread := 0;
        pvRenderThreadId := 0;
        Result := S_OK;
      end;

    WAIT_TIMEOUT:
      Result := HRESULT_FROM_WIN32(WAIT_TIMEOUT);
  else
    Result := HRESULT_FROM_WIN32(GetLastError);
  end;
end;


procedure TMfWasApiRenderOutputEngine.SetDeviceState(const AState: TDeviceState);
begin

  if FDestroying then
    Exit;

  if (pvDeviceState = AState) then
    Exit;

  pvDeviceState := AState;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self,
                    pvDeviceState);
end;


procedure TMfWasApiRenderOutputEngine.ReleaseAudioInterfaces();
begin

  pvSimpleVol := nil;
  pvAudioStreamVolume := nil;
  pvAudioClock := nil;
  pvRenderClient := nil;
  pvAudioClient := nil;
  pvMMDevice := nil;
  pvSoundChannels := 0;
  pvBufferFrameCount := 0;
  FClientBlockAlign := 0;
  FBytesPerSample := 0;
  pvAudioClockFrequency := 0;
  FLastQueuedPosition100ns := -1;
end;


procedure TMfWasApiRenderOutputEngine.SetUseDefaultOutputDevice(const ARole: ERole);
begin

  FUseDefaultDevice := True;
  FDeviceRole := ARole;
  FOutputDeviceId := '';
end;


procedure TMfWasApiRenderOutputEngine.SetOutputDeviceId(const ADeviceId: string);
begin

  FUseDefaultDevice := False;
  FOutputDeviceId := ADeviceId;
end;


function TMfWasApiRenderOutputEngine.CreateRenderDevice(): HRESULT;
var
  pEnumerator: IMMDeviceEnumerator;

begin

  Result := CoCreateInstance(CLSID_MMDeviceEnumerator,
                             nil,
                             CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,
                             pEnumerator);
  if FAILED(Result) then
    Exit;

  if FUseDefaultDevice then
    Result := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                                  FDeviceRole,
                                                  pvMMDevice)
  else
    Result := pEnumerator.GetDevice(WideStringToLPWSTR(FOutputDeviceId),
                                    pvMMDevice);
end;


function TMfWasApiRenderOutputEngine.InitializeAudioEngine(): HRESULT;
begin

  ReleaseAudioInterfaces();

  Result := CreateRenderDevice();
  if FAILED(Result) then
    Exit;

  Result := pvMMDevice.Activate(IID_IAudioClient,
                                CLSCTX_ALL,
                                nil,
                                Pointer(pvAudioClient));
end;


function TMfWasApiRenderOutputEngine.SetFormatInternal(pwfx: PWAVEFORMATEX): HRESULT;
var
  hr: HRESULT;
  hnsRequestedDuration: REFERENCE_TIME;
  RequestedBufferMs: REFERENCE_TIME;
  bufferFrameCount: UINT32;
  ch: Integer;

begin

  if (pvAudioClient = nil) then
    Exit(E_POINTER);

  if (pwfx = nil) then
    Exit(E_POINTER);

  RequestedBufferMs := EnsureRange(MainMDIFrm.Setup.AudioBufferMs,
                                   AUDIO_BUFFER_MIN_MS,
                                   AUDIO_BUFFER_MAX_MS);

  hnsRequestedDuration := Int64(RequestedBufferMs) * 10000; // 60 ms buffer = default.

  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                                 AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY or
                                 AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM,
                                 hnsRequestedDuration,
                                 0,
                                 pwfx,
                                 nil);
  if FAILED(hr) then
    Exit(hr);

  FClientBlockAlign := pwfx.nBlockAlign;
  pvSoundChannels := pwfx.nChannels;

  hr := pvAudioClient.SetEventHandle(pvAudioSamplesReadyEvent);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetBufferSize(bufferFrameCount);
  if FAILED(hr) then
    Exit(hr);

  pvBufferFrameCount := bufferFrameCount;

  hr := pvAudioClient.GetService(IID_IAudioRenderClient,
                                 pvRenderClient);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetService(IID_IAudioStreamVolume,
                                 pvAudioStreamVolume);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetService(IID_IAudioClock,
                                 pvAudioClock);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClock.GetFrequency(pvAudioClockFrequency);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetService(IID_ISimpleAudioVolume,
                                 pvSimpleVol);
  if FAILED(hr) then
    Exit(hr);

  if Assigned(pvSimpleVol) then
    begin

      pvSimpleVol.SetMute(False,
                          nil);

      pvSimpleVol.SetMasterVolume(1.0,
                                  nil);
    end;

  if Assigned(pvAudioStreamVolume) and (pvSoundChannels > 0) then
    begin

      for ch := 0 to Integer(pvSoundChannels) - 1 do
        pvAudioStreamVolume.SetChannelVolume(ch,
                                             1.0);
    end;

  case pwfx.wBitsPerSample of
    16: FBytesPerSample := 2;
    24: FBytesPerSample := 3;
    32: FBytesPerSample := 4;
  else
    FBytesPerSample := 2;
  end;

  Move(pwfx^,
       FWaveFormat,
       SizeOf(TWAVEFORMATEX));
  FHasPreparedFormat := True;

  Result := S_OK;
end;


function TMfWasApiRenderOutputEngine.Prepare(pwfx: PWAVEFORMATEX): HRESULT;
begin

  if (pwfx = nil) then
    Exit(E_POINTER);

  SetDeviceState(dsInitializing);

  Result := InitializeAudioEngine();
  if FAILED(Result) then
    begin

      DoError(Result,
              'InitializeAudioEngine failed');
      Exit;
    end;

  Result := SetFormatInternal(pwfx);
  if FAILED(Result) then
    begin

      DoError(Result,
              'SetFormatInternal failed');
      Exit;
    end;

  SetDeviceState(dsReady);
end;


function TMfWasApiRenderOutputEngine.PlayAudioStreamInternal(): HRESULT;
begin

  if (pvAudioClient = nil) or
     (pvRenderClient = nil) or
     (pvAudioClock = nil) then
    Exit(E_POINTER);

  Result := pvAudioClient.Start();

  if SUCCEEDED(Result) then
    SetDeviceState(dsPlaying);
end;


function TMfWasApiRenderOutputEngine.StopAudioStreamInternal(): HRESULT;
begin

  if (pvAudioClient = nil) then
    Exit(S_FALSE);

  Result := pvAudioClient.Stop();
  if SUCCEEDED(Result) then
    begin
      FLastQueuedPosition100ns := -1;
      RaiseProcessed(0, 0);
      SetDeviceState(dsStopped);
    end;
end;


function TMfWasApiRenderOutputEngine.PauseAudioStreamInternal(): HRESULT;
begin

  if (pvAudioClient = nil) then
    Exit(S_FALSE);

  Result := pvAudioClient.Stop();
  if SUCCEEDED(Result) then
    SetDeviceState(dsPaused);
end;


function TMfWasApiRenderOutputEngine.Start(): HRESULT;
begin

  if FDestroying then
    Exit(E_ABORT);

  if not FHasPreparedFormat then
    Exit(E_FAIL);

  FStopping := False;

  if (pvRenderThread = 0) then
    begin

      ResetEvent(pvShutdownEvent);
      pvRenderThread := CreateThread(nil,
                                     0,
                                     @TMfWasApiRenderOutputEngine.RenderThreadProc,
                                     Self,
                                     0,
                                     pvRenderThreadId);
      if (pvRenderThread = 0) then
        Exit(HRESULT_FROM_WIN32(GetLastError));
    end;

  Result := PlayAudioStreamInternal();
end;


function TMfWasApiRenderOutputEngine.Stop(): HRESULT;
var
  HrStop: HRESULT;
  HrWait: HRESULT;

begin

  FStopping := True;

  // Detach callbacks first so a late render wakeup cannot call back into
  // mixer / UI objects that are already shutting down.
  FOnFillPcm := nil;
  FOnProcessed := nil;

  HrStop := StopAudioStreamInternal();

  if (pvShutdownEvent <> 0) then
    SetEvent(pvShutdownEvent);

  HrWait := WaitForRenderThreadToExit(3000);

  if FAILED(HrStop) then
    Result := HrStop
  else
    Result := HrWait;
end;


function TMfWasApiRenderOutputEngine.Pause(): HRESULT;
begin
  Result := PauseAudioStreamInternal();
end;


function TMfWasApiRenderOutputEngine.Mute(pActive: Boolean): Boolean;
begin

  Result := Assigned(pvSimpleVol) and
            SUCCEEDED(pvSimpleVol.SetMute(pActive,
                                          nil));
end;


function TMfWasApiRenderOutputEngine.SetVolumes(pVolLeft,
                                                pVolRight: Single): HRESULT;
begin

  if not Assigned(pvAudioStreamVolume) then
    Exit(E_POINTER);

  if pvSoundChannels = 0 then
    Exit(E_FAIL);

  Result := pvAudioStreamVolume.SetChannelVolume(0,
                                                 EnsureRange(pVolLeft,
                                                             0.0,
                                                             1.0));
  if FAILED(Result) then
    Exit;

  if pvSoundChannels > 1 then
    Result := pvAudioStreamVolume.SetChannelVolume(1,
                                                   EnsureRange(pVolRight,
                                                               0.0,
                                                               1.0));
end;


function TMfWasApiRenderOutputEngine.RenderLoop(): HRESULT;
var
  Handles: array[0..1] of THandle;
  waitRes: DWORD;
  hr: HRESULT;
  padding: UINT32;
  framesAvailable: UINT32;
  pData: PByte;
  byteCount: DWORD;
  flags: DWORD;
  u64Position: UInt64;
  u64QpcPosition: UInt64;

begin

  Handles[0] := pvShutdownEvent;
  Handles[1] := pvAudioSamplesReadyEvent;

  while True do
    begin

      if FStopping or FDestroying then
        Exit(S_OK);

      waitRes := WaitForMultipleObjects(Length(Handles),
                                        @Handles[0],
                                        False,
                                        INFINITE);

      case waitRes of
        WAIT_OBJECT_0:
          Exit(S_OK);

        WAIT_OBJECT_0 + 1:
          begin

            if FStopping or FDestroying then
              Exit(S_OK);

            if (pvDeviceState <> dsPlaying) then
              Continue;

            if (pvAudioClient = nil) or
               (pvRenderClient = nil) then
              Exit(S_OK);

            hr := pvAudioClient.GetCurrentPadding(padding);
            if FAILED(hr) then
              Exit(hr);

            if FStopping or FDestroying then
              Exit(S_OK);

            if (pvBufferFrameCount < padding) then
              Continue;

            framesAvailable := pvBufferFrameCount - padding;
            if (framesAvailable = 0) then
              Continue;

            hr := pvRenderClient.GetBuffer(framesAvailable,
                                           pData);
            if FAILED(hr) then
              Exit(hr);

            flags := 0;
            byteCount := framesAvailable * DWORD(FClientBlockAlign);

            try
              if FStopping or FDestroying then
                begin

                  FillChar(pData^,
                           byteCount,
                           0);
                  flags := AUDCLNT_BUFFERFLAGS_SILENT;
                end
              else
                begin

                  if Assigned(FOnFillPcm) then
                    begin

                      hr := FOnFillPcm(Self,
                                       pData,
                                       byteCount,
                                       @FWaveFormat,
                                       flags);
                      if FAILED(hr) then
                        begin

                          FillChar(pData^,
                                   byteCount,
                                   0);
                          flags := AUDCLNT_BUFFERFLAGS_SILENT;
                        end;
                    end
                  else
                    begin

                      FillChar(pData^,
                               byteCount,
                               0);
                      flags := AUDCLNT_BUFFERFLAGS_SILENT;
                    end;
                end;
            finally
              hr := pvRenderClient.ReleaseBuffer(framesAvailable,
                                                 flags);
            end;

            if FAILED(hr) then
                Exit(hr);

            if FStopping or FDestroying then
              Exit(S_OK);

            if Assigned(FOnProcessed) and
               Assigned(pvAudioClock) and
               (pvAudioClockFrequency <> 0) and
               not FStopping and
               not FDestroying then
              begin

                hr := pvAudioClock.GetPosition(@u64Position,
                                               @u64QpcPosition);
                if SUCCEEDED(hr) then
                  RaiseProcessed(Int64((u64Position * UInt64(REFTIMES_PER_SEC)) div pvAudioClockFrequency),
                                 u64Position);
              end;
          end;
      else
        Exit(HRESULT_FROM_WIN32(GetLastError));
      end;
    end;
end;


class function TMfWasApiRenderOutputEngine.RenderThreadProc(Parameter: Pointer): DWORD;
var
  Engine: TMfWasApiRenderOutputEngine;
  hr: HRESULT;
  ComInitialized: Boolean;

begin

  Result := 0;

  Engine := TMfWasApiRenderOutputEngine(Parameter);
  if (Engine = nil) then
    Exit;

  ComInitialized := False;

  hr := CoInitializeEx(nil,
                       COINIT_MULTITHREADED);

  if SUCCEEDED(hr) then
    ComInitialized := True
  else
    if (hr <> RPC_E_CHANGED_MODE) then
      begin

        Engine.DoError(hr,
                       'CoInitializeEx failed in render thread');
        Exit;
      end;

  // Enable Mmcss.
  Engine.EnableMmcss;

  try

    hr := Engine.RenderLoop();
    if FAILED(hr) then
      Engine.DoError(hr,
                     'RenderLoop failed');
  finally

    // Disable Mmcss.
    Engine.DisableMmcss;

    if ComInitialized then
      CoUninitialize;
  end;
end;


procedure TMfWasApiRenderOutputEngine.EnableMmcss();
begin

  FAvrtIndex := 0;
  FAvrtHandle := AvSetMmThreadCharacteristics(PWideChar('Pro Audio'),
                                              @FAvrtIndex);

  if (FAvrtHandle <> 0) then
    begin

      AvSetMmThreadPriority(FAvrtHandle,
                            AVRT_PRIORITY_HIGH);
    end;
end;


procedure TMfWasApiRenderOutputEngine.DisableMmcss();
begin

  if (FAvrtHandle <> 0) then
    begin

      AvRevertMmThreadCharacteristics(FAvrtHandle);
      FAvrtHandle := 0;
      FAvrtIndex := 0;
    end;
end;

end.


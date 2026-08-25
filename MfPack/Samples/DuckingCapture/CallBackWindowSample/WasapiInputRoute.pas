unit WasapiInputRoute;

interface

uses
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.WinMM.MMeApi,
  WinApi.CoreAudioApi.MMDeviceAPI,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes;

type
  CWasapiInputRoute = class
  public
    AppWindow: HWND;
    CaptureAudioClient: IAudioClient;
    RenderAudioClient: IAudioClient;
    CaptureClient: IAudioCaptureClient;
    RenderClient: IAudioRenderClient;
    ShutdownEvent: THandle;
    SamplesReadyEvent: THandle;
    WorkerThread: THandle;
    RenderBufferFrames: UINT32;
    BlockAlign: Word;
    constructor Create(const AAppWindow: HWND);
    function Start(const HideFromVolumeMixer: Boolean): Boolean;
    procedure Stop;
  end;

implementation

function WasapiRouteThread(Parameter: Pointer): Integer;
var
  Route: CWasapiInputRoute;
  WaitHandles: array[0..1] of THandle;
  PacketFrames, Frames, Padding, FramesToWrite: UINT32;
  Flags: AUDCLNT_BUFFERFLAGS;
  CaptureData, RenderData: PByte;
  Hr: HResult;
begin
  Result := 0;
  Route := CWasapiInputRoute(Parameter);
  WaitHandles[0] := Route.ShutdownEvent;
  WaitHandles[1] := Route.SamplesReadyEvent;
  while WaitForMultipleObjects(2, @WaitHandles[0], False, INFINITE) = WAIT_OBJECT_0 + 1 do
  begin
    PacketFrames := 0;
    Hr := Route.CaptureClient.GetNextPacketSize(PacketFrames);
    while SUCCEEDED(Hr) and (PacketFrames > 0) do
    begin
      Hr := Route.CaptureClient.GetBuffer(CaptureData, Frames, Flags, nil, nil);
      if FAILED(Hr) then Break;
      Padding := 0;
      if SUCCEEDED(Route.RenderAudioClient.GetCurrentPadding(Padding)) then
      begin
        FramesToWrite := Frames;
        if FramesToWrite > Route.RenderBufferFrames - Padding then
          FramesToWrite := Route.RenderBufferFrames - Padding;
        if (FramesToWrite > 0) and SUCCEEDED(Route.RenderClient.GetBuffer(FramesToWrite, RenderData)) then
        begin
          if (Flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0 then
            Route.RenderClient.ReleaseBuffer(FramesToWrite, AUDCLNT_BUFFERFLAGS_SILENT)
          else
          begin
            CopyMemory(RenderData, CaptureData, FramesToWrite * Route.BlockAlign);
            Route.RenderClient.ReleaseBuffer(FramesToWrite, 0);
          end;
        end;
      end;
      Route.CaptureClient.ReleaseBuffer(Frames);
      PacketFrames := 0;
      Hr := Route.CaptureClient.GetNextPacketSize(PacketFrames);
    end;
  end;
end;

constructor CWasapiInputRoute.Create(const AAppWindow: HWND);
begin
  inherited Create;
  AppWindow := AAppWindow;
end;

function CWasapiInputRoute.Start(const HideFromVolumeMixer: Boolean): Boolean;
var
  Enumerator: IMMDeviceEnumerator;
  CaptureEndpoint, RenderEndpoint: IMMDevice;
  MixFormat: PWAVEFORMATEX;
  CaptureGuid, RenderGuid: TGUID;
  CaptureFlags, RenderFlags: DWord;
  RenderData: PByte;
  ThreadId: TThreadID;
  Hr: HResult;
begin
  Result := False;
  MixFormat := nil;
  Hr := CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_INPROC_SERVER, IID_IMMDeviceEnumerator, Enumerator);
  if FAILED(Hr) then Exit;
  Hr := Enumerator.GetDefaultAudioEndpoint(eCapture, eCommunications, CaptureEndpoint);
  if FAILED(Hr) then Exit;
  Hr := Enumerator.GetDefaultAudioEndpoint(eRender, eConsole, RenderEndpoint);
  if FAILED(Hr) then Exit;
  Hr := CaptureEndpoint.Activate(IID_IAudioClient, CLSCTX_INPROC_SERVER, nil, Pointer(CaptureAudioClient));
  if FAILED(Hr) then Exit;
  Hr := RenderEndpoint.Activate(IID_IAudioClient, CLSCTX_INPROC_SERVER, nil, Pointer(RenderAudioClient));
  if FAILED(Hr) then Exit;
  Hr := CaptureAudioClient.GetMixFormat(MixFormat);
  if FAILED(Hr) then Exit;
  BlockAlign := MixFormat.nBlockAlign;
  CoCreateGuid(CaptureGuid);
  CoCreateGuid(RenderGuid);
  CaptureFlags := AUDCLNT_STREAMFLAGS_EVENTCALLBACK or AUDCLNT_STREAMFLAGS_NOPERSIST;
  RenderFlags := AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY or AUDCLNT_STREAMFLAGS_NOPERSIST;
  if HideFromVolumeMixer then RenderFlags := RenderFlags or AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE;
  Hr := CaptureAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, CaptureFlags, 500000, 0, MixFormat, @CaptureGuid);
  if SUCCEEDED(Hr) then
    Hr := RenderAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, RenderFlags, 500000, 0, MixFormat, @RenderGuid);
  CoTaskMemFree(MixFormat);
  if FAILED(Hr) then begin Stop; Exit; end;
  ShutdownEvent := CreateEvent(nil, False, False, nil);
  SamplesReadyEvent := CreateEvent(nil, False, False, nil);
  if (ShutdownEvent = 0) or (SamplesReadyEvent = 0) then begin Stop; Exit; end;
  Hr := CaptureAudioClient.SetEventHandle(SamplesReadyEvent);
  if SUCCEEDED(Hr) then Hr := CaptureAudioClient.GetService(IID_IAudioCaptureClient, CaptureClient);
  if SUCCEEDED(Hr) then Hr := RenderAudioClient.GetService(IID_IAudioRenderClient, RenderClient);
  if SUCCEEDED(Hr) then Hr := RenderAudioClient.GetBufferSize(RenderBufferFrames);
  if FAILED(Hr) then begin Stop; Exit; end;
  Hr := RenderClient.GetBuffer(RenderBufferFrames, RenderData);
  if SUCCEEDED(Hr) then Hr := RenderClient.ReleaseBuffer(RenderBufferFrames, AUDCLNT_BUFFERFLAGS_SILENT);
  if FAILED(Hr) then begin Stop; Exit; end;
  ThreadId := 0;
  WorkerThread := BeginThread(nil, 0, Addr(WasapiRouteThread), Self, 0, ThreadId);
  if WorkerThread = 0 then begin Stop; Exit; end;
  Hr := RenderAudioClient.Start;
  if SUCCEEDED(Hr) then Hr := CaptureAudioClient.Start;
  if FAILED(Hr) then begin Stop; Exit; end;
  Result := True;
end;

procedure CWasapiInputRoute.Stop;
begin
  if ShutdownEvent <> 0 then SetEvent(ShutdownEvent);
  if WorkerThread <> 0 then
  begin
    WaitForSingleObject(WorkerThread, INFINITE);
    CloseHandle(WorkerThread);
    WorkerThread := 0;
  end;
  if Assigned(CaptureAudioClient) then CaptureAudioClient.Stop;
  if Assigned(RenderAudioClient) then RenderAudioClient.Stop;
  CaptureClient := nil;
  RenderClient := nil;
  CaptureAudioClient := nil;
  RenderAudioClient := nil;
  if SamplesReadyEvent <> 0 then begin CloseHandle(SamplesReadyEvent); SamplesReadyEvent := 0; end;
  if ShutdownEvent <> 0 then begin CloseHandle(ShutdownEvent); ShutdownEvent := 0; end;
end;

end.

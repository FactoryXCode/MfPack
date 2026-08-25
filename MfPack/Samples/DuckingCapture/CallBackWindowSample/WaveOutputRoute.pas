unit WaveOutputRoute;

interface

uses
  WinApi.Windows,
  System.SysUtils,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMeApi;

type
  CWaveOutputRoute = class
  private
    FHandle: HWAVEOUT;
    FHeaders: array[0..1] of WAVEHDR;
    FBuffers: array[0..1] of TBytes;
  public
    function Initialize(const WaveFormat: WAVEFORMATEX;
      const BufferBytes: Cardinal): Boolean;
    procedure Submit(const BufferIndex: Integer; const Data: Pointer;
      const DataBytes: Cardinal);
    procedure Shutdown;
  end;

implementation

function CWaveOutputRoute.Initialize(const WaveFormat: WAVEFORMATEX;
  const BufferBytes: Cardinal): Boolean;
var
  I: Integer;
  Mmr: MMRESULT;
begin
  Result := False;
  Mmr := waveOutOpen(@FHandle, WAVE_MAPPER, @WaveFormat, 0, 0, CALLBACK_NULL);
  if Mmr <> MMSYSERR_NOERROR then
    Exit;

  for I := Low(FHeaders) to High(FHeaders) do
  begin
    ZeroMemory(@FHeaders[I], SizeOf(WAVEHDR));
    SetLength(FBuffers[I], BufferBytes);
    FHeaders[I].lpData := Pointer(FBuffers[I]);
    FHeaders[I].dwBufferLength := BufferBytes;
    Mmr := waveOutPrepareHeader(FHandle, @FHeaders[I], SizeOf(WAVEHDR));
    if Mmr <> MMSYSERR_NOERROR then
    begin
      Shutdown;
      Exit;
    end;
  end;

  Result := True;
end;

procedure CWaveOutputRoute.Submit(const BufferIndex: Integer;
  const Data: Pointer; const DataBytes: Cardinal);
var
  BytesToWrite: Cardinal;
begin
  if (FHandle = 0) or (BufferIndex < Low(FHeaders)) or
     (BufferIndex > High(FHeaders)) then
    Exit;
  if (FHeaders[BufferIndex].dwFlags and WHDR_INQUEUE) <> 0 then
    Exit;

  BytesToWrite := DataBytes;
  if BytesToWrite > Cardinal(Length(FBuffers[BufferIndex])) then
    BytesToWrite := Cardinal(Length(FBuffers[BufferIndex]));
  if BytesToWrite = 0 then
    Exit;

  CopyMemory(Pointer(FBuffers[BufferIndex]), Data, BytesToWrite);
  FHeaders[BufferIndex].dwBufferLength := BytesToWrite;
  waveOutWrite(FHandle, @FHeaders[BufferIndex], SizeOf(WAVEHDR));
end;

procedure CWaveOutputRoute.Shutdown;
var
  I: Integer;
begin
  if FHandle = 0 then
    Exit;

  waveOutReset(FHandle);
  for I := Low(FHeaders) to High(FHeaders) do
  begin
    if (FHeaders[I].dwFlags and WHDR_PREPARED) <> 0 then
      waveOutUnprepareHeader(FHandle, @FHeaders[I], SizeOf(WAVEHDR));
    SetLength(FBuffers[I], 0);
    ZeroMemory(@FHeaders[I], SizeOf(WAVEHDR));
  end;
  waveOutClose(FHandle);
  FHandle := 0;
end;

end.

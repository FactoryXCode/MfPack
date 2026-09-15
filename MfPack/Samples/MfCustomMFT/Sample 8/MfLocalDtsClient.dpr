program MfLocalDtsClient;

{$APPTYPE CONSOLE}

uses
  WinApi.Windows,
  WinApi.WinError,
  System.SysUtils,
  WinApi.ActiveX,
  WinApi.Unknwn,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfTransform;

const
  CLSID_MfPackDtsDecoderMFT: TGUID = '{DAE940F8-11A6-42D1-963C-4C87E171D471}';
  IID_IClassFactory_: TGUID = '{00000001-0000-0000-C000-000000000046}';
  DecoderName = 'MfPack FFmpeg DTS Audio Decoder';

type
  TDllGetClassObject = function(const AClsid, AIid: TGUID;
    out AObject): HRESULT; stdcall;

function RegisterDecoder(out AModule: HMODULE;
  out AFactory: WinApi.Unknwn.IClassFactory): HRESULT;
var
  DllPath: string;
  GetClassObject: TDllGetClassObject;
  Inputs: array[0..4] of MFT_REGISTER_TYPE_INFO;
  Output: MFT_REGISTER_TYPE_INFO;
  FriendlyName: WideString;
begin
  // Keep the MFT and its bridge/FFmpeg dependencies beside this executable.
  DllPath := ExtractFilePath(ParamStr(0)) + 'MfPackDtsMFT.dll';
  AModule := LoadLibraryEx(PChar(DllPath), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if AModule = 0 then
    Exit(HRESULT_FROM_WIN32(GetLastError()));

  GetClassObject := TDllGetClassObject(GetProcAddress(AModule, 'DllGetClassObject'));
  if not Assigned(GetClassObject) then
    Exit(HRESULT_FROM_WIN32(ERROR_PROC_NOT_FOUND));
  Result := GetClassObject(CLSID_MfPackDtsDecoderMFT, IID_IClassFactory_, AFactory);
  if FAILED(Result) then
    Exit;

  Inputs[0].guidMajorType := MFMediaType_Audio; Inputs[0].guidSubtype := MFAudioFormat_DTS;
  Inputs[1].guidMajorType := MFMediaType_Audio; Inputs[1].guidSubtype := MFAudioFormat_DTS_RAW;
  Inputs[2].guidMajorType := MFMediaType_Audio; Inputs[2].guidSubtype := MFAudioFormat_DTS_HD;
  Inputs[3].guidMajorType := MFMediaType_Audio; Inputs[3].guidSubtype := MFAudioFormat_DTS_XLL;
  Inputs[4].guidMajorType := MFMediaType_Audio; Inputs[4].guidSubtype := MFAudioFormat_DTS_LBR;
  Output.guidMajorType := MFMediaType_Audio;
  Output.guidSubtype := MFAudioFormat_PCM;
  FriendlyName := DecoderName;
  Result := MFTRegisterLocal(AFactory, MFT_CATEGORY_AUDIO_DECODER,
    PWideChar(FriendlyName), UINT32(MFT_ENUM_FLAG_SYNCMFT),
    Length(Inputs), @Inputs[0], 1, @Output);
end;

function IsDts(const ASubtype: TGUID): Boolean;
begin
  Result := IsEqualGUID(ASubtype, MFAudioFormat_DTS) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_RAW) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_HD) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_XLL) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_LBR);
end;

function CheckLocalDecoder(): HRESULT;
var
  InputInfo, OutputInfo: MFT_REGISTER_TYPE_INFO;
  Activates: PIMFActivateArray;
  Count: UINT32;
  Index: Integer;
  Transform: IMFTransform;
  InputType, OutputType: IMFMediaType;
begin
  InputInfo.guidMajorType := MFMediaType_Audio;
  InputInfo.guidSubtype := MFAudioFormat_DTS;
  OutputInfo.guidMajorType := MFMediaType_Audio;
  OutputInfo.guidSubtype := MFAudioFormat_PCM;
  Activates := nil;
  Count := 0;
  Result := MFTEnumEx(MFT_CATEGORY_AUDIO_DECODER,
    UINT32(MFT_ENUM_FLAG_SYNCMFT or MFT_ENUM_FLAG_LOCALMFT or
      MFT_ENUM_FLAG_SORTANDFILTER), @InputInfo, @OutputInfo, Activates, Count);
  if FAILED(Result) then
    Exit;
  try
    if Count = 0 then
      Exit(MF_E_TOPO_CODEC_NOT_FOUND);
    // Sorted local registrations precede system registrations.
    Transform := nil;
    Result := Activates^[0].ActivateObject(IID_IMFTransform, Pointer(Transform));
  finally
    if Assigned(Activates) then
    begin
      for Index := 0 to Integer(Count) - 1 do
        Activates^[Index] := nil;
      CoTaskMemFree(Activates);
    end;
  end;
  if FAILED(Result) then
    Exit;

  InputType := nil;
  OutputType := nil;
  Result := Transform.GetInputAvailableType(0, 0, InputType);
  if SUCCEEDED(Result) then Result := Transform.SetInputType(0, InputType, 0);
  if SUCCEEDED(Result) then Result := Transform.GetOutputAvailableType(0, 0, OutputType);
  if SUCCEEDED(Result) then Result := Transform.SetOutputType(0, OutputType, 0);
  if SUCCEEDED(Result) then
    Writeln('Application-local DTS decoder enumerated, activated and negotiated to PCM.');
end;

function DecodeFile(const AFileName: string): HRESULT;
var
  Reader: IMFSourceReader;
  NativeType, PcmType: IMFMediaType;
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  MajorType, Subtype: TGUID;
  Stream, DtsStream, ActualStream, Flags, BufferLength: DWORD;
  SampleTime: LONGLONG;
  PcmBytes: UInt64;
  PcmSamples: Integer;
begin
  Reader := nil;
  Result := MFCreateSourceReaderFromURL(PWideChar(WideString(AFileName)), nil, Reader);
  if FAILED(Result) then
    Exit;

  DtsStream := DWORD(-1);
  for Stream := 0 to 1023 do
  begin
    NativeType := nil;
    Result := Reader.GetNativeMediaType(Stream, 0, @NativeType);
    if FAILED(Result) then
      Break;
    if SUCCEEDED(NativeType.GetGUID(MF_MT_MAJOR_TYPE, MajorType)) and
       IsEqualGUID(MajorType, MFMediaType_Audio) and
       SUCCEEDED(NativeType.GetGUID(MF_MT_SUBTYPE, Subtype)) and
       IsDts(Subtype) then
    begin
      DtsStream := Stream;
      Break;
    end;
  end;
  if DtsStream = DWORD(-1) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := Reader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS, False);
  if SUCCEEDED(Result) then
    Result := Reader.SetStreamSelection(DtsStream, True);
  PcmType := nil;
  if SUCCEEDED(Result) then Result := MFCreateMediaType(PcmType);
  if SUCCEEDED(Result) then Result := PcmType.SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
  if SUCCEEDED(Result) then Result := PcmType.SetGUID(MF_MT_SUBTYPE, MFAudioFormat_PCM);
  if SUCCEEDED(Result) then Result := PcmType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, 16);
  if SUCCEEDED(Result) then Result := PcmType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, 48000);
  if SUCCEEDED(Result) then Result := PcmType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, 2);
  if SUCCEEDED(Result) then Result := PcmType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK, 3);
  if SUCCEEDED(Result) then Result := PcmType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT, 4);
  if SUCCEEDED(Result) then Result := PcmType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND, 192000);
  if SUCCEEDED(Result) then Result := Reader.SetCurrentMediaType(DtsStream, 0, PcmType);
  if FAILED(Result) then
    Exit;

  PcmBytes := 0;
  PcmSamples := 0;
  repeat
    Sample := nil;
    Flags := 0;
    ActualStream := 0;
    SampleTime := 0;
    Result := Reader.ReadSample(DtsStream, 0, @ActualStream, @Flags,
      @SampleTime, @Sample);
    if FAILED(Result) then
      Exit;
    if Assigned(Sample) then
    begin
      Buffer := nil;
      Result := Sample.ConvertToContiguousBuffer(@Buffer);
      if SUCCEEDED(Result) then
        Result := Buffer.GetCurrentLength(BufferLength);
      if FAILED(Result) then
        Exit;
      Inc(PcmBytes, BufferLength);
      Inc(PcmSamples);
    end;
  until (Flags and DWORD(MF_SOURCE_READERF_ENDOFSTREAM)) <> 0;

  if PcmBytes = 0 then
    Exit(MF_E_END_OF_STREAM);
  Writeln(Format('Decoded stream %d: %d PCM samples, %d bytes (48 kHz, 16-bit stereo).',
    [DtsStream, PcmSamples, PcmBytes]));
  Result := S_OK;
end;

var
  Status: HRESULT;
  Module: HMODULE;
  Factory: WinApi.Unknwn.IClassFactory;
  Registered: Boolean;
begin
  if (ParamCount <> 1) then
  begin
    Writeln('Usage: MfLocalDtsClient.exe --check | <DTS media file>');
    Halt(1);
  end;

  Module := 0;
  Factory := nil;
  Registered := False;
  Status := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if FAILED(Status) then
    Halt(2);
  try
    Status := MFStartup(MF_VERSION, MFSTARTUP_LITE);
    if SUCCEEDED(Status) then
    try
      Status := RegisterDecoder(Module, Factory);
      Registered := SUCCEEDED(Status);
      if Registered then
      begin
        Status := CheckLocalDecoder();
        if SUCCEEDED(Status) and (ParamStr(1) <> '--check') then
          Status := DecodeFile(ParamStr(1));
      end;
    finally
      if Registered then
        MFTUnregisterLocal(Factory);
      MFShutdown();
    end;
  finally
    Factory := nil;
    if Module <> 0 then
      FreeLibrary(Module);
    CoUninitialize();
  end;

  if FAILED(Status) then
  begin
    Writeln(Format('Application-local DTS decode failed: 0x%.8x', [Cardinal(Status)]));
    Halt(2);
  end;
end.

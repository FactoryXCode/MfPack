program MfDtsMftSmoke;

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
  MFPACK_DTS_MFT_NAME = 'MfPack FFmpeg DTS Audio Decoder';

type
  TDllGetClassObject = function(const AClsid, AIid: TGUID;
    out AObject): HRESULT; stdcall;

function RegisterLocalTransform(const ADllName: string; out AModule: HMODULE;
  out AFactory: WinApi.Unknwn.IClassFactory): HRESULT;
var
  GetClassObject: TDllGetClassObject;
  InputInfo: array[0..4] of MFT_REGISTER_TYPE_INFO;
  OutputInfo: MFT_REGISTER_TYPE_INFO;
  FriendlyName: WideString;
begin
  AModule := LoadLibraryEx(PChar(ADllName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if AModule = 0 then
    Exit(HRESULT_FROM_WIN32(GetLastError()));
  GetClassObject := TDllGetClassObject(GetProcAddress(AModule,
    'DllGetClassObject'));
  if not Assigned(GetClassObject) then
    Exit(HRESULT_FROM_WIN32(GetLastError()));
  Result := GetClassObject(CLSID_MfPackDtsDecoderMFT, IID_IClassFactory_,
    AFactory);
  if FAILED(Result) then
    Exit;
  InputInfo[0].guidMajorType := MFMediaType_Audio; InputInfo[0].guidSubtype := MFAudioFormat_DTS;
  InputInfo[1].guidMajorType := MFMediaType_Audio; InputInfo[1].guidSubtype := MFAudioFormat_DTS_RAW;
  InputInfo[2].guidMajorType := MFMediaType_Audio; InputInfo[2].guidSubtype := MFAudioFormat_DTS_HD;
  InputInfo[3].guidMajorType := MFMediaType_Audio; InputInfo[3].guidSubtype := MFAudioFormat_DTS_XLL;
  InputInfo[4].guidMajorType := MFMediaType_Audio; InputInfo[4].guidSubtype := MFAudioFormat_DTS_LBR;
  OutputInfo.guidMajorType := MFMediaType_Audio;
  OutputInfo.guidSubtype := MFAudioFormat_PCM;
  FriendlyName := MFPACK_DTS_MFT_NAME;
  Result := MFTRegisterLocal(AFactory, MFT_CATEGORY_AUDIO_DECODER,
    PWideChar(FriendlyName), UINT32(MFT_ENUM_FLAG_SYNCMFT), Length(InputInfo), @InputInfo[0],
    1, @OutputInfo);
  Writeln(Format('MFTRegisterLocal status=0x%.8x', [Cardinal(Result)]));
end;

function RunSmokeTest(): HRESULT;
var
  InputInfo: MFT_REGISTER_TYPE_INFO;
  OutputInfo: MFT_REGISTER_TYPE_INFO;
  Activates: PIMFActivateArray;
  Count: UINT32;
  Index: Integer;
  Transform: IMFTransform;
  InputType: IMFMediaType;
  OutputType: IMFMediaType;
begin
  Activates := nil;
  Count := 0;
  Transform := nil;
  InputType := nil;
  OutputType := nil;
  InputInfo.guidMajorType := MFMediaType_Audio;
  InputInfo.guidSubtype := MFAudioFormat_DTS;
  OutputInfo.guidMajorType := MFMediaType_Audio;
  OutputInfo.guidSubtype := MFAudioFormat_PCM;

  Result := MFTEnumEx(MFT_CATEGORY_AUDIO_DECODER,
    UINT32(MFT_ENUM_FLAG_SYNCMFT or MFT_ENUM_FLAG_LOCALMFT), @InputInfo,
    @OutputInfo, Activates, Count);
  Writeln(Format('MFTEnumEx status=0x%.8x count=%d',
    [Cardinal(Result), Count]));
  if FAILED(Result) then
    Exit;

  try
    // This process registered exactly one local transform immediately before
    // enumeration. Local IMFActivate objects do not expose a CLSID because
    // registration supplies an IClassFactory rather than a class identifier.
    if Count = 0 then
      Exit(MF_E_TOPO_CODEC_NOT_FOUND);
    Result := Activates^[0].ActivateObject(IID_IMFTransform,
      Pointer(Transform));
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
  Result := Transform.GetInputAvailableType(0, 0, InputType);
  if SUCCEEDED(Result) then
    Result := Transform.SetInputType(0, InputType, 0);
  if SUCCEEDED(Result) then
    Result := Transform.GetOutputAvailableType(0, 0, OutputType);
  if SUCCEEDED(Result) then
    Result := Transform.SetOutputType(0, OutputType, 0);
end;

function IsDtsSubtype(const ASubtype: TGUID): Boolean;
begin
  Result := IsEqualGUID(ASubtype, MFAudioFormat_DTS) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_RAW) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_HD) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_XLL) or
    IsEqualGUID(ASubtype, MFAudioFormat_DTS_LBR);
end;

function DecodeFirstDtsSample(const AFileName: string): HRESULT;
var
  Reader: IMFSourceReader;
  NativeType: IMFMediaType;
  RequestedType: IMFMediaType;
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  MajorType: TGUID;
  Subtype: TGUID;
  StreamIndex: DWORD;
  DtsStream: DWORD;
  ActualStream: DWORD;
  Flags: DWORD;
  SampleTime: LONGLONG;
  SampleDuration: LONGLONG;
  PreviousEnd: LONGLONG;
  Gap: LONGLONG;
  MaximumGap: LONGLONG;
  TotalBytes: UInt64;
  BufferLength: DWORD;
  Attempt: Integer;
  SampleCount: Integer;
  Discontinuities: Integer;
begin
  Reader := nil;
  Result := MFCreateSourceReaderFromURL(PWideChar(WideString(AFileName)), nil,
    Reader);
  if FAILED(Result) then
    Exit;
  DtsStream := DWORD(-1);
  for StreamIndex := 0 to 1023 do
  begin
    NativeType := nil;
    Result := Reader.GetNativeMediaType(StreamIndex, 0, @NativeType);
    if Result <> S_OK then
      Break;
    if SUCCEEDED(NativeType.GetGUID(MF_MT_MAJOR_TYPE, MajorType)) and
       IsEqualGUID(MajorType, MFMediaType_Audio) and
       SUCCEEDED(NativeType.GetGUID(MF_MT_SUBTYPE, Subtype)) then
    begin
      Writeln(Format('Native audio stream %d subtype=%s',
        [StreamIndex, GUIDToString(Subtype)]));
      if IsDtsSubtype(Subtype) then
      begin
        DtsStream := StreamIndex;
        Break;
      end;
    end;
  end;
  if DtsStream = DWORD(-1) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := Reader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS, False);
  if SUCCEEDED(Result) then
    Result := Reader.SetStreamSelection(DtsStream, True);
  RequestedType := nil;
  if SUCCEEDED(Result) then
    Result := MFCreateMediaType(RequestedType);
  if SUCCEEDED(Result) then Result := RequestedType.SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
  if SUCCEEDED(Result) then Result := RequestedType.SetGUID(MF_MT_SUBTYPE, MFAudioFormat_PCM);
  if SUCCEEDED(Result) then Result := RequestedType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, 16);
  if SUCCEEDED(Result) then Result := RequestedType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, 48000);
  if SUCCEEDED(Result) then Result := RequestedType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, 2);
  if SUCCEEDED(Result) then Result := RequestedType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK, 3);
  if SUCCEEDED(Result) then Result := RequestedType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT, 4);
  if SUCCEEDED(Result) then Result := RequestedType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND, 192000);
  if SUCCEEDED(Result) then
    Result := Reader.SetCurrentMediaType(DtsStream, 0, RequestedType);
  if FAILED(Result) then
    Exit;

  PreviousEnd := -1;
  MaximumGap := 0;
  TotalBytes := 0;
  SampleCount := 0;
  Discontinuities := 0;
  for Attempt := 1 to 10000 do
  begin
    Sample := nil;
    ActualStream := 0;
    Flags := 0;
    SampleTime := 0;
    Result := Reader.ReadSample(DtsStream, 0, @ActualStream, @Flags,
      @SampleTime, @Sample);
    if FAILED(Result) then
      Exit;
    if (Flags and DWORD(MF_SOURCE_READERF_ENDOFSTREAM)) <> 0 then
      Exit(MF_E_END_OF_STREAM);
    if Assigned(Sample) then
    begin
      SampleDuration := 0;
      Sample.GetSampleDuration(@SampleDuration);
      Result := Sample.ConvertToContiguousBuffer(@Buffer);
      if SUCCEEDED(Result) then
        Result := Buffer.GetCurrentLength(BufferLength);
      if SUCCEEDED(Result) and (BufferLength = 0) then
        Result := E_FAIL;
      if FAILED(Result) then
        Exit;
      Inc(SampleCount);
      Inc(TotalBytes, BufferLength);
      if PreviousEnd >= 0 then
      begin
        Gap := SampleTime - PreviousEnd;
        if Abs(Gap) > 1 then
        begin
          Inc(Discontinuities);
          if Abs(Gap) > MaximumGap then
            MaximumGap := Abs(Gap);
        end;
      end;
      PreviousEnd := SampleTime + SampleDuration;
      if (SampleTime >= 300000000) then
      begin
        Writeln(Format('Decoded DTS stream %d: samples=%d PCM bytes=%d end=%d discontinuities=%d maxGap=%d.',
          [DtsStream, SampleCount, TotalBytes, PreviousEnd, Discontinuities,
           MaximumGap]));
        Exit(S_OK);
      end;
    end;
  end;
  Result := MF_E_TRANSFORM_NEED_MORE_INPUT;
end;

var
  Status: HRESULT;
  Module: HMODULE;
  Factory: WinApi.Unknwn.IClassFactory;
begin
  Module := 0;
  Factory := nil;
  Status := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if FAILED(Status) then
    Halt(1);
  try
    Status := MFStartup(MF_VERSION, MFSTARTUP_LITE);
    if SUCCEEDED(Status) then
    try
      if ParamCount > 0 then
        Status := RegisterLocalTransform(ParamStr(1), Module, Factory);
      if SUCCEEDED(Status) then
      try
        Status := RunSmokeTest();
        if SUCCEEDED(Status) and (ParamCount > 1) then
          Status := DecodeFirstDtsSample(ParamStr(2));
      finally
        if Assigned(Factory) then
          MFTUnregisterLocal(Factory);
      end;
    finally
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
    Writeln(Format('MFT smoke test failed: 0x%.8x', [Cardinal(Status)]));
    Halt(2);
  end;
  Writeln('MfPack DTS MFT enumerated, activated, and negotiated DTS to PCM.');
end.

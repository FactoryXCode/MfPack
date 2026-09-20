program MfAudioDelayDemo;

{$APPTYPE CONSOLE}

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  {MfAudioDelayMFT}
  MfAudioDelayMFT in 'MfAudioDelayMFT.pas';


procedure Check(const AName: string;
                const AHr: HResult);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: %.8x',
                              [AName, Cardinal(AHr)]);
end;


function MakeSample(const AData: array of Byte): IMFSample;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  Capacity: DWORD;
  I: Integer;

begin

  Check('MFCreateSample',
        MFCreateSample(Result));

  Check('MFCreateMemoryBuffer',
        MFCreateMemoryBuffer(Length(AData),
                             Buffer));
  Check('Lock input',
        Buffer.Lock(Data,
                    @Capacity,
                    nil));

  try
    for I := 0 to High(AData) do
      Data[I] := AData[I];

  finally
    Buffer.Unlock();
  end;

  Check('SetCurrentLength',
        Buffer.SetCurrentLength(Length(AData)));
  Check('AddBuffer',
        Result.AddBuffer(Buffer));
end;


procedure CheckBytes(const AName: string;
                     const ASample: IMFSample;
                     const AExpected: array of Byte);
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  Capacity: DWORD;
  Count: DWORD;
  I: Integer;

begin

  Check('ConvertToContiguousBuffer',
        ASample.ConvertToContiguousBuffer(@Buffer));

  Check('Lock output',
        Buffer.Lock(Data,
                    @Capacity,
                    @Count));

  try
    if (Count <> DWORD(Length(AExpected))) then
      raise Exception.CreateFmt('%s length: %d',
                                [AName, Count]);

    for I := 0 to High(AExpected) do
      if (Data[I] <> AExpected[I]) then
        raise Exception.CreateFmt('%s byte %d: %d',
                                  [AName, I, Data[I]]);

  finally
    Buffer.Unlock;
  end;
end;


procedure Run();
var
  Transform: IMFTransform;
  MediaType: IMFMediaType;
  Attributes: IMFAttributes;
  Input: IMFSample;
  Output: MFT_OUTPUT_DATA_BUFFER;
  Status: DWORD;

  procedure CheckOneByte(const AName: string;
                         const AInput: Byte;
                         const ATime: Int64;
                         const ADiscontinuous: Boolean;
                         const AExpected: Byte);
  var
    OneSample: IMFSample;

  begin
    OneSample := MakeSample([AInput]);
    Check(AName + ' time',
          OneSample.SetSampleTime(ATime));
    if ADiscontinuous then
      Check(AName + ' discontinuity',
            OneSample.SetUINT32(MFSampleExtension_Discontinuity,
                                1));

    FillChar(Output,
             SizeOf(Output),
             0);
    Check(AName + ' input',
          Transform.ProcessInput(0,
                                 OneSample,
                                 0));
    Check(AName + ' output',
          Transform.ProcessOutput(0,
                                  1,
                                  @Output,
                                  Status));
    CheckBytes(AName,
               Output.pSample,
               [AExpected]);
    Output.pSample := nil;
  end;

begin

  Transform := TMfAudioDelayMFT.Create as IMFTransform;

  Check('GetAttributes',
        Transform.GetAttributes(Attributes));

  Check('Set delay to 1 ms',
        Attributes.SetUINT32(MF_AUDIODELAY_DELAY_LENGTH,
                             1));

  Check('Set wet mix to 100%',
        Attributes.SetUINT32(MF_AUDIODELAY_WET_DRY_MIX,
                             100));

  Check('MFCreateMediaType',
        MFCreateMediaType(MediaType));

  Check('Set major type',
        MediaType.SetGUID(MF_MT_MAJOR_TYPE,
                          MFMediaType_Audio));

  Check('Set PCM subtype',
        MediaType.SetGUID(MF_MT_SUBTYPE,
                          MFAudioFormat_PCM));

  Check('Set channels',
        MediaType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                            1));

  Check('Set sample rate',
        MediaType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            1000));

  Check('Set bits',
        MediaType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                            8));

  Check('Set alignment',
        MediaType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                            1));

  Check('Set byte rate',
        MediaType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                            1000));

  Check('Test input',
        Transform.SetInputType(0,
                               MediaType,
                               MFT_SET_TYPE_TEST_ONLY));

  Check('Set input',
        Transform.SetInputType(0,
                               MediaType,
                               0));

  Check('Test output',
        Transform.SetOutputType(0,
                                MediaType,
                                MFT_SET_TYPE_TEST_ONLY));

  Check('Set output',
        Transform.SetOutputType(0,
                                MediaType,
                                0));

  Input := MakeSample([129,
                       130]);

  FillChar(Output,
           SizeOf(Output),
           0);

  Check('ProcessInput',
        Transform.ProcessInput(0,
                               Input,
                               0));

  Check('ProcessOutput',
        Transform.ProcessOutput(0,
                                1,
                                @Output,
                                Status));

  CheckBytes('main output',
             Output.pSample, [128,
                              129]);
  Output.pSample := nil;

  Check('Drain',
        Transform.ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN,
                                 0));

  Check('Tail output',
        Transform.ProcessOutput(0,
                                1,
                                @Output,
                                Status));

  CheckBytes('tail output',
             Output.pSample,
             [130]);

  Output.pSample := nil;

  if (Transform.ProcessOutput(0,
                              1,
                              @Output,
                              Status) <> MF_E_TRANSFORM_NEED_MORE_INPUT) then
    raise Exception.Create('Expected MF_E_TRANSFORM_NEED_MORE_INPUT after drain');

  // Each new segment starts with silence in the one-frame delay buffer.
  // Test a timestamp jump, an explicit discontinuity, and a pipeline flush.
  CheckOneByte('new segment', 140, 0, False, 128);
  CheckOneByte('timestamp jump', 141, 10000000, False, 128);
  CheckOneByte('discontinuity flag', 142, 10010000, True, 128);
  Check('Flush',
        Transform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                 0));
  CheckOneByte('after flush', 143, 10020000, False, 128);

  // Exercise the 16-bit branch and a 50/50 mix. Samples are 10000, -10000.
  Transform := TMfAudioDelayMFT.Create as IMFTransform;

  Check('GetAttributes 16-bit',
         Transform.GetAttributes(Attributes));

  Check('Set delay 16-bit',
        Attributes.SetUINT32(MF_AUDIODELAY_DELAY_LENGTH,
                             1));

  Check('Set mix 16-bit',
        Attributes.SetUINT32(MF_AUDIODELAY_WET_DRY_MIX,
                             50));

  Check('MFCreateMediaType 16-bit',
        MFCreateMediaType(MediaType));

  Check('Set major 16-bit',
        MediaType.SetGUID(MF_MT_MAJOR_TYPE,
                          MFMediaType_Audio));

  Check('Set subtype 16-bit',
        MediaType.SetGUID(MF_MT_SUBTYPE,
                          MFAudioFormat_PCM));

  Check('Set channels 16-bit',
        MediaType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                            1));

  Check('Set rate 16-bit',
        MediaType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            1000));

  Check('Set bits 16-bit',
        MediaType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                            16));

  Check('Set alignment 16-bit',
        MediaType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                            2));

  Check('Set byte rate 16-bit',
        MediaType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                            2000));

  Check('Set input 16-bit',
        Transform.SetInputType(0,
                               MediaType,
                               0));

  Check('Set output 16-bit',
        Transform.SetOutputType(0,
                                MediaType,
                                0));

  Input := MakeSample([$10,
                       $27,
                       $F0,
                       $D8]);

  Check('ProcessInput 16-bit',
        Transform.ProcessInput(0,
                               Input,
                               0));

  Check('ProcessOutput 16-bit',
        Transform.ProcessOutput(0,
                                1,
                                @Output,
                                Status));

  CheckBytes('main output 16-bit',
             Output.pSample, [$88,
                              $13,
                              0,
                              0]);

  Output.pSample := nil;

  Check('Drain 16-bit',
        Transform.ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN,
                                 0));

  Check('Tail output 16-bit',
        Transform.ProcessOutput(0,
                                1,
                                @Output,
                                Status));

  CheckBytes('tail output 16-bit',
             Output.pSample,
             [$78,
              $EC]);

  Output.pSample := nil;

  Writeln('PASS: PCM8/16 delay, wet/dry mix, drain, discontinuity, flush');
end;

begin

  Check('CoInitializeEx',
        CoInitializeEx(nil,
                       COINIT_APARTMENTTHREADED));
  try
    Check('MFStartup',
          MFStartup(MF_VERSION,
                    MFSTARTUP_FULL));
    try
      Run();
      Sleep(5000); // Wait before closing when running from the IDE.
    finally
      MFShutdown();
    end;

  finally
    CoUninitialize();
  end;
end.

unit MfDtsDecoderMFT;

interface

uses
  WinApi.Windows, WinApi.WinApiTypes,
  System.SysUtils, System.Win.ComObj, System.Win.ComServ,
  WinApi.MediaFoundationApi.MfApi, WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects, WinApi.MediaFoundationApi.MfTransform,
  MfDtsBridgeApi;

const
  CLSID_MfPackDtsDecoderMFT: TGUID = '{DAE940F8-11A6-42D1-963C-4C87E171D471}';
  MFPACK_DTS_MFT_NAME = 'MfPack FFmpeg DTS Audio Decoder';

type
  TMfDtsDecoderMFT = class(TComObject, IMFTransform)
  private
    FInputType, FOutputType: IMFMediaType;
    FBridge: TMfDtsBridge;
    FDecoder: TMfDtsDecoderHandle;
    FPending: TBytes;
    FPendingTime, FPendingDuration: Int64;
    function CreateOutputType(out AType: IMFMediaType): HRESULT;
    function CreateInputType(AIndex: DWORD; out AType: IMFMediaType): HRESULT;
    function ValidInput(const AType: IMFMediaType): Boolean;
    function ValidOutput(const AType: IMFMediaType): Boolean;
    procedure ResetDecoder;
  public
    destructor Destroy; override;
    function GetStreamLimits(out a,b,c,d: DWORD): HRESULT; stdcall;
    function GetStreamCount(out a,b: DWORD): HRESULT; stdcall;
    function GetStreamIDs(dwInputIDArraySize: DWORD;
      pdwInputIDs: WinApi.WinApiTypes.PDWORD; dwOutputIDArraySize: DWORD;
      pdwOutputIDs: PDWORD): HRESULT; stdcall;
    function GetInputStreamInfo(const dwInputStreamID: DWORD;
      out pStreamInfo: MFT_INPUT_STREAM_INFO): HRESULT; stdcall;
    function GetOutputStreamInfo(const dwOutputStreamID: DWORD;
      out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HRESULT; stdcall;
    function GetAttributes(out attrs: IMFAttributes): HRESULT; stdcall;
    function GetInputStreamAttributes(const dwInputStreamID: DWORD;
      out pAttributes: IMFAttributes): HRESULT; stdcall;
    function GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
      out pAttributes: IMFAttributes): HRESULT; stdcall;
    function DeleteInputStream(id: DWORD): HRESULT; stdcall;
    function AddInputStreams(count: DWORD; ids: PDWORD): HRESULT; stdcall;
    function GetInputAvailableType(const dwInputStreamID: DWORD;
      dwTypeIndex: DWORD; out pType: IMFMediaType): HRESULT; stdcall;
    function GetOutputAvailableType(const dwOutputStreamID: DWORD;
      dwTypeIndex: DWORD; out pType: IMFMediaType): HRESULT; stdcall;
    function SetInputType(const dwInputStreamID: DWORD; pType: IMFMediaType;
      dwFlags: DWORD): HRESULT; stdcall;
    function SetOutputType(dwOutputStreamID: DWORD; pType: IMFMediaType;
      dwFlags: DWORD): HRESULT; stdcall;
    function GetInputCurrentType(const dwInputStreamID: DWORD;
      out pType: IMFMediaType): HRESULT; stdcall;
    function GetOutputCurrentType(const dwOutputStreamID: DWORD;
      out pType: IMFMediaType): HRESULT; stdcall;
    function GetInputStatus(const dwInputStreamID: DWORD;
      out pdwFlags: DWORD): HRESULT; stdcall;
    function GetOutputStatus(out flags: DWORD): HRESULT; stdcall;
    function SetOutputBounds(lower,upper: LONGLONG): HRESULT; stdcall;
    function ProcessEvent(const dwInputStreamID: DWORD;
      pEvent: IMFMediaEvent): HRESULT; stdcall;
    function ProcessMessage(message: MFT_MESSAGE_TYPE; param: ULONG_PTR): HRESULT; stdcall;
    function ProcessInput(const dwInputStreamID: DWORD;
      const pSample: IMFSample; dwFlags: DWORD = 0): HRESULT; stdcall;
    function ProcessOutput(flags,count: DWORD; outputs: PMFT_OUTPUT_DATA_BUFFER;
      out status: DWORD): HRESULT; stdcall;
  end;

implementation

function CloneType(const Source: IMFMediaType; out Dest: IMFMediaType): HRESULT;
begin
  Dest := nil; Result := MFCreateMediaType(Dest);
  if SUCCEEDED(Result) then Result := Source.CopyAllItems(Dest);
end;

destructor TMfDtsDecoderMFT.Destroy;
begin
  ResetDecoder; FBridge.Free; FInputType := nil; FOutputType := nil; inherited;
end;

procedure TMfDtsDecoderMFT.ResetDecoder;
begin
  if Assigned(FBridge) then FBridge.DestroyDecoder(FDecoder);
  SetLength(FPending,0); FPendingTime := 0; FPendingDuration := 0;
end;

function TMfDtsDecoderMFT.CreateOutputType(out AType: IMFMediaType): HRESULT;
begin
  AType := nil; Result := MFCreateMediaType(AType);
  if SUCCEEDED(Result) then Result := AType.SetGUID(MF_MT_MAJOR_TYPE,MFMediaType_Audio);
  if SUCCEEDED(Result) then Result := AType.SetGUID(MF_MT_SUBTYPE,MFAudioFormat_PCM);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,2);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,48000);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,16);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,4);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,192000);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK,3);
  if SUCCEEDED(Result) then Result := AType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,1);
end;

function TMfDtsDecoderMFT.CreateInputType(AIndex: DWORD; out AType: IMFMediaType): HRESULT;
const
  Types: array[0..4] of PGUID = (@MFAudioFormat_DTS,@MFAudioFormat_DTS_RAW,
    @MFAudioFormat_DTS_HD,@MFAudioFormat_DTS_XLL,@MFAudioFormat_DTS_LBR);
begin
  AType := nil; if AIndex > High(Types) then Exit(MF_E_NO_MORE_TYPES);
  Result := MFCreateMediaType(AType);
  if SUCCEEDED(Result) then Result := AType.SetGUID(MF_MT_MAJOR_TYPE,MFMediaType_Audio);
  if SUCCEEDED(Result) then Result := AType.SetGUID(MF_MT_SUBTYPE,Types[AIndex]^);
end;

function TMfDtsDecoderMFT.ValidInput(const AType: IMFMediaType): Boolean;
var major,sub: TGUID; i: DWORD; candidate: IMFMediaType; csub: TGUID;
begin
  Result := Assigned(AType) and SUCCEEDED(AType.GetGUID(MF_MT_MAJOR_TYPE,major)) and
    IsEqualGUID(major,MFMediaType_Audio) and SUCCEEDED(AType.GetGUID(MF_MT_SUBTYPE,sub));
  if not Result then Exit;
  Result := False;
  for i := 0 to 4 do begin CreateInputType(i,candidate); candidate.GetGUID(MF_MT_SUBTYPE,csub);
    if IsEqualGUID(sub,csub) then Exit(True); end;
end;

function TMfDtsDecoderMFT.ValidOutput(const AType: IMFMediaType): Boolean;
var major,sub: TGUID; value: UINT32;
begin
  Result := Assigned(AType) and SUCCEEDED(AType.GetGUID(MF_MT_MAJOR_TYPE,major)) and
    IsEqualGUID(major,MFMediaType_Audio) and SUCCEEDED(AType.GetGUID(MF_MT_SUBTYPE,sub)) and
    IsEqualGUID(sub,MFAudioFormat_PCM) and
    SUCCEEDED(AType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,value)) and (value=2) and
    SUCCEEDED(AType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,value)) and (value=48000) and
    SUCCEEDED(AType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,value)) and (value=16);
end;

function TMfDtsDecoderMFT.GetStreamLimits(out a,b,c,d: DWORD): HRESULT;
begin a:=1;b:=1;c:=1;d:=1;Result:=S_OK;end;
function TMfDtsDecoderMFT.GetStreamCount(out a,b: DWORD): HRESULT;
begin a:=1;b:=1;Result:=S_OK;end;
function TMfDtsDecoderMFT.GetStreamIDs(dwInputIDArraySize: DWORD;
  pdwInputIDs: WinApi.WinApiTypes.PDWORD; dwOutputIDArraySize: DWORD;
  pdwOutputIDs: PDWORD): HRESULT;
begin Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.GetInputStreamInfo(const dwInputStreamID: DWORD;
  out pStreamInfo:MFT_INPUT_STREAM_INFO):HRESULT;
begin if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER); ZeroMemory(@pStreamInfo,SizeOf(pStreamInfo));
 pStreamInfo.dwFlags:=MFT_INPUT_STREAM_WHOLE_SAMPLES or MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER; Result:=S_OK;end;
function TMfDtsDecoderMFT.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
  out pStreamInfo:MFT_OUTPUT_STREAM_INFO):HRESULT;
begin if dwOutputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER); ZeroMemory(@pStreamInfo,SizeOf(pStreamInfo));
 pStreamInfo.dwFlags:=MFT_OUTPUT_STREAM_WHOLE_SAMPLES or MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or MFT_OUTPUT_STREAM_PROVIDES_SAMPLES;
 pStreamInfo.cbSize:=65536; Result:=S_OK;end;
function TMfDtsDecoderMFT.GetAttributes(out attrs:IMFAttributes):HRESULT;
begin attrs:=nil;Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.GetInputStreamAttributes(const dwInputStreamID:DWORD;
  out pAttributes:IMFAttributes):HRESULT;
begin pAttributes:=nil;if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.GetOutputStreamAttributes(const dwOutputStreamID:DWORD;
  out pAttributes:IMFAttributes):HRESULT;
begin pAttributes:=nil;if dwOutputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.DeleteInputStream(id:DWORD):HRESULT;begin Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.AddInputStreams(count:DWORD;ids:PDWORD):HRESULT;begin Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.GetInputAvailableType(const dwInputStreamID:DWORD;
  dwTypeIndex:DWORD;out pType:IMFMediaType):HRESULT;
begin pType:=nil;if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);Result:=CreateInputType(dwTypeIndex,pType);end;
function TMfDtsDecoderMFT.GetOutputAvailableType(const dwOutputStreamID:DWORD;
  dwTypeIndex:DWORD;out pType:IMFMediaType):HRESULT;
begin pType:=nil;if dwOutputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);if dwTypeIndex<>0 then Exit(MF_E_NO_MORE_TYPES);
 Result:=CreateOutputType(pType);end;

function TMfDtsDecoderMFT.SetInputType(const dwInputStreamID:DWORD;
  pType:IMFMediaType;dwFlags:DWORD):HRESULT;
var copy:IMFMediaType;
begin
 if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER); if not Assigned(pType) then begin if dwFlags=0 then begin ResetDecoder;FInputType:=nil;end;Exit(S_OK);end;
 if not ValidInput(pType) then Exit(MF_E_INVALIDMEDIATYPE);
 if (dwFlags and MFT_SET_TYPE_TEST_ONLY)<>0 then Exit(S_OK);
 if not Assigned(FBridge) then FBridge:=TMfDtsBridge.Create;
 if not FBridge.Load then Exit(MF_E_TOPO_CODEC_NOT_FOUND);
 ResetDecoder; if FBridge.CreateDecoder(FDecoder)<0 then Exit(MF_E_TOPO_CODEC_NOT_FOUND);
 Result:=CloneType(pType,copy);if SUCCEEDED(Result) then FInputType:=copy;
end;

function TMfDtsDecoderMFT.SetOutputType(dwOutputStreamID:DWORD;
  pType:IMFMediaType;dwFlags:DWORD):HRESULT;
var copy:IMFMediaType;
begin if dwOutputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);if not Assigned(pType) then begin if dwFlags=0 then FOutputType:=nil;Exit(S_OK);end;
 if not ValidOutput(pType) then Exit(MF_E_INVALIDMEDIATYPE);if (dwFlags and MFT_SET_TYPE_TEST_ONLY)<>0 then Exit(S_OK);
 Result:=CloneType(pType,copy);if SUCCEEDED(Result) then FOutputType:=copy;end;
function TMfDtsDecoderMFT.GetInputCurrentType(const dwInputStreamID:DWORD;
  out pType:IMFMediaType):HRESULT;
begin pType:=nil;if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);if not Assigned(FInputType) then Exit(MF_E_TRANSFORM_TYPE_NOT_SET);Result:=CloneType(FInputType,pType);end;
function TMfDtsDecoderMFT.GetOutputCurrentType(const dwOutputStreamID:DWORD;
  out pType:IMFMediaType):HRESULT;
begin pType:=nil;if dwOutputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);if not Assigned(FOutputType) then Exit(MF_E_TRANSFORM_TYPE_NOT_SET);Result:=CloneType(FOutputType,pType);end;
function TMfDtsDecoderMFT.GetInputStatus(const dwInputStreamID:DWORD;
  out pdwFlags:DWORD):HRESULT;
begin pdwFlags:=0;if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);if Length(FPending)=0 then pdwFlags:=MFT_INPUT_STATUS_ACCEPT_DATA;Result:=S_OK;end;
function TMfDtsDecoderMFT.GetOutputStatus(out flags:DWORD):HRESULT;
begin flags:=0;if Length(FPending)>0 then flags:=MFT_OUTPUT_STATUS_SAMPLE_READY;Result:=S_OK;end;
function TMfDtsDecoderMFT.SetOutputBounds(lower,upper:LONGLONG):HRESULT;begin Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.ProcessEvent(const dwInputStreamID:DWORD;
  pEvent:IMFMediaEvent):HRESULT;begin Result:=E_NOTIMPL;end;
function TMfDtsDecoderMFT.ProcessMessage(message:MFT_MESSAGE_TYPE;param:ULONG_PTR):HRESULT;
begin if message=MFT_MESSAGE_COMMAND_FLUSH then begin SetLength(FPending,0);if Assigned(FBridge) then FBridge.Flush(FDecoder);end;Result:=S_OK;end;

function TMfDtsDecoderMFT.ProcessInput(const dwInputStreamID:DWORD;
  const pSample:IMFSample;dwFlags:DWORD):HRESULT;
var buffer:IMFMediaBuffer;data:PByte;maxLen,currentLen:DWORD;available,read:NativeUInt;
begin
 if dwInputStreamID<>0 then Exit(MF_E_INVALIDSTREAMNUMBER);if dwFlags<>0 then Exit(E_INVALIDARG);if not Assigned(pSample) then Exit(E_POINTER);
 if not Assigned(FInputType) or not Assigned(FOutputType) then Exit(MF_E_TRANSFORM_TYPE_NOT_SET);
 if Length(FPending)>0 then Exit(MF_E_NOTACCEPTING);
 Result:=pSample.ConvertToContiguousBuffer(@buffer);if FAILED(Result) then Exit;
 data:=nil;Result:=buffer.Lock(data,@maxLen,@currentLen);if FAILED(Result) then Exit;
 try if FBridge.Send(FDecoder,data,currentLen)<0 then Exit(MF_E_INVALID_STREAM_DATA);finally buffer.Unlock;end;
 available:=FBridge.Available(FDecoder);if available=0 then Exit(S_OK);
 SetLength(FPending,available);read:=FBridge.Read(FDecoder,@FPending[0],available);
 if read<>available then begin SetLength(FPending,0);Exit(E_FAIL);end;
  FPendingTime:=0;pSample.GetSampleTime(@FPendingTime);FPendingDuration:=(Int64(available)*10000000) div 192000;Result:=S_OK;
end;

function TMfDtsDecoderMFT.ProcessOutput(flags,count:DWORD;outputs:PMFT_OUTPUT_DATA_BUFFER;out status:DWORD):HRESULT;
var sample:IMFSample;buffer:IMFMediaBuffer;data:PByte;maxLen,currentLen:DWORD;
begin
 status:=0;if flags<>0 then Exit(E_INVALIDARG);if (count<>1) or not Assigned(outputs) then Exit(E_INVALIDARG);
 if Length(FPending)=0 then Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);
 Result:=MFCreateSample(sample);if FAILED(Result) then Exit;
 Result:=MFCreateMemoryBuffer(Length(FPending),buffer);if FAILED(Result) then Exit;
 data:=nil;Result:=buffer.Lock(data,@maxLen,@currentLen);if FAILED(Result) then Exit;
 try Move(FPending[0],data^,Length(FPending));finally buffer.Unlock;end;
 Result:=buffer.SetCurrentLength(Length(FPending));if SUCCEEDED(Result) then Result:=sample.AddBuffer(buffer);
 if SUCCEEDED(Result) then Result:=sample.SetSampleTime(FPendingTime);
 if SUCCEEDED(Result) then Result:=sample.SetSampleDuration(FPendingDuration);
 if SUCCEEDED(Result) then begin outputs^.dwStreamID:=0;outputs^.pSample:=sample;outputs^.dwStatus:=0;outputs^.pEvents:=nil;SetLength(FPending,0);end;
end;

initialization
  TComObjectFactory.Create(ComServer,TMfDtsDecoderMFT,CLSID_MfPackDtsDecoderMFT,
    'MfPackDtsDecoderMFT',MFPACK_DTS_MFT_NAME,ciMultiInstance,tmBoth);

end.

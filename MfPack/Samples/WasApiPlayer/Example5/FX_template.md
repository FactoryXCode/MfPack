// Short

// Here’s a drop-in template you can copy to create a new FX that fits your project rules:

// - Slot-only bypass (no internal Enabled gating)

// - Float32 in-place processing

// - RT-safe settings update (double-buffered, no heap alloc/free on slider moves)

// - Parameter smoothing (one-pole)

//

// You need to create two units:

// - FX component (what the user drops on a form, implements IMfWasApiFxProvider)

// - MFT (the actual DSP, IMFTransform)

//

// You can rename the units/classes to your naming scheme.

// =============================================================================





// Component template ==========================================================



// Notes:

// This component does not have an Enabled property for bypass. The rack slot does that.

// ApplyToMft pushes settings to the DSP side.





unit MfMyFxComponent;



interface



uses



&nbsp; {WinApi}

  WinApi.Windows,

&nbsp; {System}

&nbsp; System.Classes,

&nbsp; {MediaFoundationApi}

&nbsp; WinApi.MediaFoundationApi.MfApi,

  WinApi.MediaFoundationApi.MfTransform,

  WinApi.MediaFoundationApi.MfError,

&nbsp; {Application}

&nbsp; MfWasApiFxComponentBase,

&nbsp; MfMyFxMFT;



type

&nbsp; // Keep Settings POD (no strings/dyn arrays) so it can be copied atomically.

&nbsp; TMyFxSettings = packed record

&nbsp;   Drive: Single;   // example

&nbsp;   Mix: Single;     // 0..1

&nbsp; end;



&nbsp; TMfMyFxEffect = class(TMfWasApiFxComponentBase)

&nbsp; private

&nbsp;   FSettings: TMyFxSettings;

&nbsp; protected

&nbsp;   function GetMftInstance: IMFTransform; override;

&nbsp;   procedure CheckForMft; override;

&nbsp; public

&nbsp;   constructor Create(AOwner: TComponent); override;



&nbsp;   procedure SetSettings(const S: TMyFxSettings);

&nbsp;   function GetSettings: TMyFxSettings;



&nbsp; published

&nbsp;   property Settings: TMyFxSettings read GetSettings write SetSettings;

&nbsp; end;



implementation



{ TMfMyFxEffect }



constructor TMfMyFxEffect.Create(AOwner: TComponent);

begin

&nbsp; inherited;



&nbsp; // Defaults

&nbsp; FSettings.Drive := 1.0;

&nbsp; FSettings.Mix := 0.5;

end;



procedure TMfMyFxEffect.CheckForMft;

begin

&nbsp; // Create once, keep it.

&nbsp; if FMft = nil then

&nbsp;   FMft := TMfMyFxMFT.Create; // FMft field assumed in base; if not, store your own field.

end;



function TMfMyFxEffect.GetMftInstance: IMFTransform;

begin

&nbsp; // Base class will call CheckForMft first.

&nbsp; Result := FMft as IMFTransform;

end;



function TMfMyFxEffect.GetSettings: TMyFxSettings;

begin

&nbsp; Result := FSettings;

end;



procedure TMfMyFxEffect.SetSettings(const S: TMyFxSettings);

begin

&nbsp; FSettings := S;



&nbsp; // Push settings into MFT (thread-safe; MFT must accept UI-thread calls).

&nbsp; if not IsDesigning then

&nbsp; begin

&nbsp;   CheckForMft;

&nbsp;   (FMft as TMfMyFxMFT).SetSettings(FSettings);

&nbsp; end;

end;



end.





// MFT template ====================================================================



MFT template (works with ConfigureMftFloat32 + ProcessMftInPlace)



This MFT:



* Accepts Float32 input/output type set by the rack.
* Stores sample rate/channels extracted from IMFMediaType.
* Uses double-buffered settings (no heap alloc/free during slider drags).
* Implements classic ProcessInput (store sample) + ProcessOutput (process sample and return it).



unit MfMyFxMFT;



interface



uses

&nbsp; 

&nbsp; {WinApi}

  WinApi.Windows,

  {System}

  System.SysUtils,

&nbsp; System.Math,

&nbsp; {ActiveX}

&nbsp; Winapi.ActiveX,

&nbsp; {MediaFoundationApi}

  WinApi.MediaFoundationApi.MfApi,

  WinApi.MediaFoundationApi.MfObjects,

  WinApi.MediaFoundationApi.MfError;



type

&nbsp; TMyFxSettings = packed record

&nbsp;   Drive: Single;  // example

&nbsp;   Mix: Single;    // 0..1

&nbsp; end;



&nbsp; TMfMyFxMFT = class(TInterfacedObject, IMFTransform)

&nbsp; private

&nbsp;   // Media types (set by rack)

&nbsp;   FInType: IMFMediaType;

&nbsp;   FOutType: IMFMediaType;

&nbsp;   FSampleRate: Integer;

&nbsp;   FChannels: Integer;



&nbsp;   // Settings snapshots (double buffered)

&nbsp;   FSetA: TMyFxSettings;

&nbsp;   FSetB: TMyFxSettings;

&nbsp;   FSetPtr: Pointer;         // points to A or B

&nbsp;   FWriteSel: Integer;



&nbsp;   // Smoothed params (audio thread)

&nbsp;   FDriveSm: Single;

&nbsp;   FMixSm: Single;



&nbsp;   // Input sample cached between ProcessInput/ProcessOutput

&nbsp;   FCurSample: IMFSample;



&nbsp;   procedure UpdateFormatFromType(const AType: IMFMediaType);

&nbsp;   function SnapshotSettings: TMyFxSettings;



&nbsp;   function SmoothCoefPerSample(const SampleRate: Integer; const TimeMs: Single): Single;

&nbsp;   procedure ProcessAudioFloat32(pData: PSingle; Frames, Channels, SampleRate: Integer);



&nbsp; public

&nbsp;   constructor Create;



&nbsp;   // called from GUI thread via component

&nbsp;   procedure SetSettings(const S: TMyFxSettings);



&nbsp;   // IMFTransform

&nbsp;   function GetStreamLimits(out pdwInputMinimum, pdwInputMaximum,

&nbsp;     pdwOutputMinimum, pdwOutputMaximum: DWORD): HResult; stdcall;

&nbsp;   function GetStreamCount(out pcInputStreams, pcOutputStreams: DWORD): HResult; stdcall;

&nbsp;   function GetStreamIDs(dwInputIDArraySize: DWORD; pdwInputIDs: PDWORD;

&nbsp;     dwOutputIDArraySize: DWORD; pdwOutputIDs: PDWORD): HResult; stdcall;

&nbsp;   function GetInputStreamInfo(dwInputStreamID: DWORD;

&nbsp;     out pStreamInfo: MFT\_INPUT\_STREAM\_INFO): HResult; stdcall;

&nbsp;   function GetOutputStreamInfo(dwOutputStreamID: DWORD;

&nbsp;     out pStreamInfo: MFT\_OUTPUT\_STREAM\_INFO): HResult; stdcall;

&nbsp;   function GetAttributes(out pAttributes: IMFAttributes): HResult; stdcall;

&nbsp;   function GetInputStreamAttributes(dwInputStreamID: DWORD;

&nbsp;     out pAttributes: IMFAttributes): HResult; stdcall;

&nbsp;   function GetOutputStreamAttributes(dwOutputStreamID: DWORD;

&nbsp;     out pAttributes: IMFAttributes): HResult; stdcall;

&nbsp;   function DeleteInputStream(dwStreamID: DWORD): HResult; stdcall;

&nbsp;   function AddInputStreams(cStreams: DWORD; adwStreamIDs: PDWORD): HResult; stdcall;

&nbsp;   function GetInputAvailableType(dwInputStreamID, dwTypeIndex: DWORD;

&nbsp;     out pType: IMFMediaType): HResult; stdcall;

&nbsp;   function GetOutputAvailableType(dwOutputStreamID, dwTypeIndex: DWORD;

&nbsp;     out pType: IMFMediaType): HResult; stdcall;

&nbsp;   function SetInputType(dwInputStreamID: DWORD; pType: IMFMediaType;

&nbsp;     dwFlags: DWORD): HResult; stdcall;

&nbsp;   function SetOutputType(dwOutputStreamID: DWORD; pType: IMFMediaType;

&nbsp;     dwFlags: DWORD): HResult; stdcall;

&nbsp;   function GetInputCurrentType(dwInputStreamID: DWORD;

&nbsp;     out pType: IMFMediaType): HResult; stdcall;

&nbsp;   function GetOutputCurrentType(dwOutputStreamID: DWORD;

&nbsp;     out pType: IMFMediaType): HResult; stdcall;

&nbsp;   function GetInputStatus(dwInputStreamID: DWORD;

&nbsp;     out pdwFlags: DWORD): HResult; stdcall;

&nbsp;   function GetOutputStatus(out pdwFlags: DWORD): HResult; stdcall;

&nbsp;   function SetOutputBounds(llLowerBound, llUpperBound: LONGLONG): HResult; stdcall;

&nbsp;   function ProcessEvent(dwInputStreamID: DWORD; pEvent: IMFMediaEvent): HResult; stdcall;

&nbsp;   function ProcessMessage(eMessage: MFT\_MESSAGE\_TYPE; ulParam: ULONG\_PTR): HResult; stdcall;

&nbsp;   function ProcessInput(dwInputStreamID: DWORD; pSample: IMFSample;

&nbsp;     dwFlags: DWORD): HResult; stdcall;

&nbsp;   function ProcessOutput(dwFlags: DWORD; cOutputBufferCount: DWORD;

&nbsp;     pOutputSamples: PMFT\_OUTPUT\_DATA\_BUFFER; out pdwStatus: DWORD): HResult; stdcall;

&nbsp; end;



implementation



{ TMfMyFxMFT }



constructor TMfMyFxMFT.Create;

begin

&nbsp; inherited;



&nbsp; // defaults

&nbsp; FSetA.Drive := 1.0; FSetA.Mix := 0.5;

&nbsp; FSetB := FSetA;



&nbsp; FSetPtr := @FSetA;

&nbsp; FWriteSel := 0;



&nbsp; FDriveSm := FSetA.Drive;

&nbsp; FMixSm := FSetA.Mix;



&nbsp; FSampleRate := 0;

&nbsp; FChannels := 0;

end;



procedure TMfMyFxMFT.SetSettings(const S: TMyFxSettings);

var

&nbsp; tgt: ^TMyFxSettings;

&nbsp; tmp: TMyFxSettings;

begin

&nbsp; // Clamp in GUI thread (safe)

&nbsp; tmp := S;

&nbsp; if tmp.Drive < 0 then tmp.Drive := 0;

&nbsp; if tmp.Mix < 0 then tmp.Mix := 0;

&nbsp; if tmp.Mix > 1 then tmp.Mix := 1;



&nbsp; if InterlockedCompareExchange(FWriteSel, 0, 0) = 0 then

&nbsp;   tgt := @FSetB

&nbsp; else

&nbsp;   tgt := @FSetA;



&nbsp; tgt^ := tmp;



&nbsp; InterlockedExchangePointer(FSetPtr, tgt);

&nbsp; InterlockedExchange(FWriteSel, 1 - FWriteSel);

end;



function TMfMyFxMFT.SnapshotSettings: TMyFxSettings;

var

&nbsp; p: Pointer;

begin

&nbsp; p := InterlockedCompareExchangePointer(FSetPtr, nil, nil);

&nbsp; if p <> nil then

&nbsp;   Result := TMyFxSettings(p^)

&nbsp; else

&nbsp;   begin

&nbsp;     Result.Drive := 1.0;

&nbsp;     Result.Mix := 0.5;

&nbsp;   end;

end;





procedure TMfMyFxMFT.UpdateFormatFromType(const AType: IMFMediaType);

var

&nbsp; v: UINT32;

begin

&nbsp; 

&nbsp;if (AType = nil) then

&nbsp;   Exit;



&nbsp; // sample rate

&nbsp; if Succeeded(AType.GetUINT32(MF\_MT\_AUDIO\_SAMPLES\_PER\_SECOND, v)) then

&nbsp;   FSampleRate := Integer(v);



&nbsp; // channels

&nbsp; if Succeeded(AType.GetUINT32(MF\_MT\_AUDIO\_NUM\_CHANNELS, v)) then

&nbsp;   FChannels := Integer(v);

end;





function TMfMyFxMFT.SmoothCoefPerSample(const SampleRate: Integer; const TimeMs: Single): Single;

var

&nbsp; tau: Double;



begin

&nbsp; 

&nbsp; if (SampleRate <= 0) or (TimeMs <= 0) then

&nbsp;   Exit(0.0);



&nbsp; tau := TimeMs / 1000.0;

&nbsp; Result := Single(Exp(-1.0 / (tau \* SampleRate)));

end;



procedure TMfMyFxMFT.ProcessAudioFloat32(pData: PSingle; Frames, Channels, SampleRate: Integer);

var

&nbsp; s: TMyFxSettings;

&nbsp; a, oneMinus: Single;

&nbsp; drive, mix: Single;

&nbsp; n, ch: Integer;

&nbsp; x, y: Single;



begin



&nbsp; if (pData = nil) or (Frames <= 0) or (Channels <= 0) then Exit;



&nbsp; // Snapshot settings (lock-free)

&nbsp; s := SnapshotSettings;



&nbsp; // Smooth (15ms is a good starting point)

&nbsp; a := SmoothCoefPerSample(SampleRate, 15.0);

&nbsp; if (a = 0) then

&nbsp; begin

&nbsp;   FDriveSm := s.Drive;

&nbsp;   FMixSm := s.Mix;

&nbsp; end

&nbsp; else

&nbsp; begin

&nbsp;   oneMinus := 1.0 - a;

&nbsp;   FDriveSm := FDriveSm \* a + s.Drive \* oneMinus;

&nbsp;   FMixSm := FMixSm \* a + s.Mix \* oneMinus;

&nbsp; end;



&nbsp; drive := FDriveSm;

&nbsp; mix := FMixSm;



&nbsp; // Placeholder DSP: soft clip + wet/dry mix

&nbsp; for n := 0 to Frames - 1 do

&nbsp; begin

&nbsp;   for ch := 0 to Channels - 1 do

&nbsp;   begin

&nbsp;     x := pData^;

&nbsp;     y := Tanh(x \* drive);

&nbsp;     pData^ := x \* (1.0 - mix) + y \* mix;

&nbsp;     Inc(pData);

&nbsp;   end;

&nbsp; end;

end;



//

// IMFTransform minimal implementation for your rack

//



function TMfMyFxMFT.GetStreamLimits(out pdwInputMinimum, pdwInputMaximum,

&nbsp; pdwOutputMinimum, pdwOutputMaximum: DWORD): HResult; stdcall;

begin

&nbsp; pdwInputMinimum := 1;

&nbsp; pdwInputMaximum := 1;

&nbsp; pdwOutputMinimum := 1;

&nbsp; pdwOutputMaximum := 1;

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.GetStreamCount(out pcInputStreams, pcOutputStreams: DWORD): HResult; stdcall;

begin

&nbsp; pcInputStreams := 1;

&nbsp; pcOutputStreams := 1;

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.GetStreamIDs(dwInputIDArraySize: DWORD; pdwInputIDs: PDWORD;

&nbsp; dwOutputIDArraySize: DWORD; pdwOutputIDs: PDWORD): HResult; stdcall;

begin

&nbsp; Result := E\_NOTIMPL; // default IDs (0)

end;





function TMfMyFxMFT.GetInputStreamInfo(dwInputStreamID: DWORD;

&nbsp; out pStreamInfo: MFT\_INPUT\_STREAM\_INFO): HResult; stdcall;

begin

&nbsp; ZeroMemory(@pStreamInfo, SizeOf(pStreamInfo));

&nbsp; pStreamInfo.dwFlags := MFT\_INPUT\_STREAM\_WHOLE\_SAMPLES or MFT\_INPUT\_STREAM\_SINGLE\_SAMPLE\_PER\_BUFFER;

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.GetOutputStreamInfo(dwOutputStreamID: DWORD;

&nbsp; out pStreamInfo: MFT\_OUTPUT\_STREAM\_INFO): HResult; stdcall;

begin

&nbsp; ZeroMemory(@pStreamInfo, SizeOf(pStreamInfo));

&nbsp; pStreamInfo.dwFlags := MFT\_OUTPUT\_STREAM\_WHOLE\_SAMPLES or MFT\_OUTPUT\_STREAM\_SINGLE\_SAMPLE\_PER\_BUFFER;

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.GetAttributes(out pAttributes: IMFAttributes): HResult; stdcall;

begin pAttributes := nil; Result := E\_NOTIMPL; end;



function TMfMyFxMFT.GetInputStreamAttributes(dwInputStreamID: DWORD;

&nbsp; out pAttributes: IMFAttributes): HResult; stdcall;

begin pAttributes := nil; Result := E\_NOTIMPL; end;





function TMfMyFxMFT.GetOutputStreamAttributes(dwOutputStreamID: DWORD;

&nbsp; out pAttributes: IMFAttributes): HResult; stdcall;

begin pAttributes := nil; Result := E\_NOTIMPL; end;





function TMfMyFxMFT.DeleteInputStream(dwStreamID: DWORD): HResult; stdcall;

begin Result := E\_NOTIMPL; end;





function TMfMyFxMFT.AddInputStreams(cStreams: DWORD; adwStreamIDs: PDWORD): HResult; stdcall;

begin Result := E\_NOTIMPL; end;





function TMfMyFxMFT.GetInputAvailableType(dwInputStreamID, dwTypeIndex: DWORD;

&nbsp; out pType: IMFMediaType): HResult; stdcall;

begin pType := nil; Result := MF\_E\_NO\_MORE\_TYPES; end;





function TMfMyFxMFT.GetOutputAvailableType(dwOutputStreamID, dwTypeIndex: DWORD;

&nbsp; out pType: IMFMediaType): HResult; stdcall;

begin pType := nil; Result := MF\_E\_NO\_MORE\_TYPES; end;





function TMfMyFxMFT.SetInputType(dwInputStreamID: DWORD; pType: IMFMediaType;

&nbsp; dwFlags: DWORD): HResult; stdcall;

begin

&nbsp; 

&nbsp; FInType := pType;

&nbsp; UpdateFormatFromType(pType);

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.SetOutputType(dwOutputStreamID: DWORD; pType: IMFMediaType;

&nbsp; dwFlags: DWORD): HResult; stdcall;

begin

&nbsp; FOutType := pType;

&nbsp; UpdateFormatFromType(pType);

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.GetInputCurrentType(dwInputStreamID: DWORD;

&nbsp; out pType: IMFMediaType): HResult; stdcall;

begin pType := FInType; Result := S\_OK; end;





function TMfMyFxMFT.GetOutputCurrentType(dwOutputStreamID: DWORD;

&nbsp; out pType: IMFMediaType): HResult; stdcall;

begin pType := FOutType; Result := S\_OK; end;





function TMfMyFxMFT.GetInputStatus(dwInputStreamID: DWORD;

&nbsp; out pdwFlags: DWORD): HResult; stdcall;

begin pdwFlags := MFT\_INPUT\_STATUS\_ACCEPT\_DATA; Result := S\_OK; end;





function TMfMyFxMFT.GetOutputStatus(out pdwFlags: DWORD): HResult; stdcall;

begin pdwFlags := 0; Result := S\_OK; end;





function TMfMyFxMFT.SetOutputBounds(llLowerBound, llUpperBound: LONGLONG): HResult; stdcall;

begin Result := E\_NOTIMPL; end;





function TMfMyFxMFT.ProcessEvent(dwInputStreamID: DWORD; pEvent: IMFMediaEvent): HResult; stdcall;

begin Result := E\_NOTIMPL; end;





function TMfMyFxMFT.ProcessMessage(eMessage: MFT\_MESSAGE\_TYPE; ulParam: ULONG\_PTR): HResult; stdcall;

begin

&nbsp; // Your rack sends begin/start streaming. Accept them.

&nbsp; // If you need flush/reset on MFT\_MESSAGE\_COMMAND\_FLUSH, do it here.

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.ProcessInput(dwInputStreamID: DWORD; pSample: IMFSample;

&nbsp; dwFlags: DWORD): HResult; stdcall;

begin

&nbsp; // Cache the sample for ProcessOutput.

&nbsp; FCurSample := pSample;

&nbsp; Result := S\_OK;

end;





function TMfMyFxMFT.ProcessOutput(dwFlags: DWORD; cOutputBufferCount: DWORD;

&nbsp; pOutputSamples: PMFT\_OUTPUT\_DATA\_BUFFER; out pdwStatus: DWORD): HResult; stdcall;

var

&nbsp; buf: IMFMediaBuffer;

&nbsp; p: PByte;

&nbsp; cbMax, cbCur: DWORD;

&nbsp; frames: Integer;



begin

&nbsp; 

&nbsp; pdwStatus := 0;



&nbsp; if (pOutputSamples = nil) or (cOutputBufferCount < 1) then

&nbsp;   Exit(E\_INVALIDARG);



&nbsp; if FCurSample = nil then

&nbsp;   Exit(MF\_E\_TRANSFORM\_NEED\_MORE\_INPUT);



&nbsp; // In-place: output is the same sample

&nbsp; pOutputSamples\[0].pSample := FCurSample;

&nbsp; pOutputSamples\[0].dwStatus := 0;

&nbsp; pOutputSamples\[0].pEvents := nil;



&nbsp; Result := FCurSample.ConvertToContiguousBuffer(buf);

&nbsp; if Failed(Result) then Exit;



&nbsp; Result := buf.Lock(p, cbMax, cbCur);

&nbsp; if Failed(Result) then Exit;



&nbsp; try

&nbsp;   if (FChannels > 0) then

&nbsp;     frames := Integer(cbCur div DWORD(SizeOf(Single) \* DWORD(FChannels)))

&nbsp;   else

&nbsp;     frames := 0;



&nbsp;   if frames > 0 then

&nbsp;     ProcessAudioFloat32(PSingle(p), frames, FChannels, FSampleRate);



&nbsp;   // Keep same length

&nbsp;   buf.SetCurrentLength(cbCur);

&nbsp; finally

&nbsp;   buf.Unlock;

&nbsp; end;



&nbsp; // Consume input sample

&nbsp; FCurSample := nil;



&nbsp; Result := S\_OK;

end;



end.














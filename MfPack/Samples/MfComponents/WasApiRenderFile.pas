//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WasApiRenderFile.pas
// Kind: Pascal Unit Component
// Release date: 13-08-2020
// Language: ENU
//
// Version: 3.0.2
//
// Description: WasApi component to play wav files (See: Remarks)
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: WasApi support is limited to use WAV-(raw)formats only.
//
// Related objects: -
// Related projects: MfPackX302
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit WasApiRenderFile;

interface

uses
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.Avrt,
  System.SysUtils,
  System.Classes,
  WinApi.KsMedia,
  WinApi.ComBaseApi,
  WinApi.Coml2Api,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjBase,
  {CoreAudioApi}
  WinApi.CoreAudioApi.Audioclient,
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.MMDevApiUtils,
  WinApi.CoreAudioApi.Endpointvolume,
  WinApi.CoreAudioApi.Mmdeviceapi,
  {WinMM}
  WinApi.WinMM.MMReg;

const
  // By default the maximum filesize of a wavefile can't be larger than 4294967295 bytes (approx. 4 GB)
  MAX_WAV_FILESIZE = MAXDWORD - (44 + SizeOf(WAVEFORMATEX));
  // REFERENCE_TIME time units per second and per millisecond
  REFTIMES_PER_SEC                    = 10000000;
  REFTIMES_PER_MILLISEC               = 10000;


type
  tCh4 = array [0..3] of AnsiChar;


  // WAV header structure //////////////////////////////////////////////////////
  //
  TWaveHeader = record
    tagRiff    : FOURCC;   // "RIFF", 4 bytes
    szRiff     : DWord;    // Total file size, not including the first 8 bytes, 4 bytes
    tagWave    : FOURCC;   // "WAV " 4 bytes
  end;
  //
  // After the first 12 bytes, other tags than the expected formattag can appear.
  //
  TFormatChunk = record
    // Start of 'fmt ' chunk
    tagFmt     : FOURCC;   // "fmt " 4 bytes
    szFmt      : DWord;    // Size of the WAVEFORMATEX data that follows.
  end;
  //
  // Here the WAVEFORMATEX structure will be placed, that may vary in size.
  // It's size is defined in TWaveHeader.szFmt
  //
  TDataHeader = record
    tagData : FOURCC;  // "data"
    szData  : DWord;   // Size of the audio data.
  end;
  // The sound data follows after this header in the form of
  // array of bytes
  //////////////////////////////////////////////////////////////////////////////

  // NOTE:  This could be a usefull function for here to force a valid waveformatex structure:
  //
  // Converts a Media Foundation audio media type to a WAVEFORMATEX structure.
  //function MFCreateWaveFormatExFromMFMediaType(pMFType: IMFMediaType; // Pointer to the IMFMediaType interface of the media type.
  //                                             var ppWF: PWAVEFORMATEX; // Receives a pointer to the WAVEFORMATEX structure. The caller must release the memory allocated for the structure by calling CoTaskMemFree.
  //                                             out pcbSize: UINT32; // Receives the size of the WAVEFORMATEX structure.
  //                                             Flags: UINT32 = 0): HResult; stdcall; // Contains a flag from the MFWaveFormatExConvertFlags enumeration.
  //


  TWavFileReader = class(TFileStream)
  private
    fr_WaveFormat: WAVEFORMATEX;

  public

    constructor Create(const AFileName: string; Mode: Word);
    destructor Destroy(); override;

    // Reads the wavefile header
    function ReadWaveHeader(out Header: TWaveHeader;
                            out WaveFormat: PWAVEFORMATEX;
                            out Data: TDataHeader;
                            out DataPos: Int64): HResult;

    // Reads the data after the wavefile header "data"
    function LoadData(const FilePointer: Int64;
                      FrameCount: DWord;
                      var Data: PByte;
                      var Flags: DWord): Integer;

    // Set format (used when writing to a wav-file)
    function SetFormat(const WaveFormat: WAVEFORMATEX): HResult;

  end;


  TWasApiFileRenderer = class(TComponent)
  private

    ar_endpointdevices: TEndPointDeviceArray;
    ar_EndPointNames: TArray<string>;
    b_useDefaultDevice: Boolean;
    b_HideFromMixer: Boolean;
    dw_HideFromMixer: DWord;
    i_CurrentEndPointDeviceId: Integer;
    ev_ShutdownEvent: THandle;
    ev_AudioSamplesReadyEvent: THandle;
    ws_InputFile: PWideChar;
    wf_pMixFormat: PWAVEFORMATEX;  // shared format
    wf_sourceMixFormat: WAVEFORMATEX;

    // Interfaces
    if_DeviceEnumerator: IMMDeviceEnumerator;
    if_Endpoint: IMMDevice;
    if_AudioClient: IAudioClient2;

    // source
    cl_WavFileReader: TWavFileReader;

    //
    procedure SetDefaultDevice(value: Boolean);
    //
    function GetFileName(): string;
    procedure SetFileName(value: string);
    //
    procedure SetHideFromMixer(value: Boolean);

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function Initialize(): HResult;

    // New
    procedure SetMixFormat(const srcMixFormat: WAVEFORMATEX);
    function AdjustFormatTo16Bits(pwfx: PWAVEFORMATEX): Boolean;
    function IsEventDrivenModeSupported(): Boolean;
    function IsWavFormatSupported(): Boolean;

    function RenderAudioStream(): HResult;
    function GetStreamFormat(AudioClient: IAudioClient;
                             var ppwfx: PWAVEFORMATEX): HResult;

    // The caller can populate the active endpoints.
    function GetActiveEndPoints(): HResult;

    // Non visual properties
    property CurrentEndPointDeviceId: Integer read i_CurrentEndPointDeviceId;
    property SourceMixFormat: WAVEFORMATEX read wf_sourceMixFormat;

  published
    // Visual properties
    property UseDefaultDevice: Boolean read b_useDefaultDevice write SetDefaultDevice default True;
    property Endpoints: TArray<string> read ar_EndPointNames;
    property HideFromMixer: Boolean read b_HideFromMixer write SetHideFromMixer default False;
    property FileName: string read GetFileName write SetFileName;
  end;

  // Helpers
  function FCC(ch4: TCh4): DWord; inline;

  procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('MfPack Core Audio Samples', [TWasApiFileRenderer]);
end;


constructor TWasApiFileRenderer.Create(AOwner: TComponent);
var
  hr: HResult;

begin
  inherited Create(AOwner);

  UseDefaultDevice := True;
  HideFromMixer := False;

try
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED);

  if Failed(hr) then
    raise Exception.CreateFmt('CoInitializeEx failed with resultcode %d ', [hr]);
except
  //
end;
end;

destructor TWasApiFileRenderer.Destroy();
begin
  FreeMem(ws_InputFile);
  CoUninitialize();
  inherited Destroy;
end;


// Property methods ////////////////////////////////////////////////////////////

procedure TWasApiFileRenderer.SetDefaultDevice(value: Boolean);
begin
  if (value <> b_useDefaultDevice) then
    b_useDefaultDevice := value;
end;


function TWasApiFileRenderer.GetFileName(): string;
begin
  if Assigned(ws_InputFile) then
    Result := WideCharToString(ws_InputFile)
  else
    Result := '';
end;


procedure TWasApiFileRenderer.SetFileName(value: string);
begin

  if (Length(value) > 0) then
    begin
      GetMem(ws_InputFile,
             (Length(value) + 1) * SizeOf(WideChar));

      StringToWideChar(value,
                       ws_InputFile,
                       Length(value) + 1);
    end
  else
    ws_InputFile := Nil;

  // Note: You could implement the TOpenDialog here if value is empty.

end;


procedure TWasApiFileRenderer.SetHideFromMixer(value: Boolean);
begin
  if (value = True) then
    dw_HideFromMixer := AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or
                        AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE or
                        AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                        AUDCLNT_STREAMFLAGS_NOPERSIST

  else
    dw_HideFromMixer := AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or
                        AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                        AUDCLNT_STREAMFLAGS_NOPERSIST;

  b_HideFromMixer := value;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TWasApiFileRenderer.SetMixFormat(const srcMixFormat: WAVEFORMATEX);
begin

  CopyMemory(wf_pMixFormat,
             @srcMixFormat,
             SizeOf(WAVEFORMATEX));

end;

//
// Return a list of rendering (hardware) endpoints on this system.
//
function TWasApiFileRenderer.GetActiveEndPoints(): HResult;
var
  hr: HResult;
  i: Integer;
  num: DWord;

begin

  hr := GetEndpointDevices(EDataFlowEx.eRender,
                           DEVICE_STATE_ACTIVE,
                           ar_endpointdevices,  // do not forget to release from memory when done!
                           num);

  // populate the devicename and description array
  SetLength(ar_EndPointNames, num);
  for i := 0 to num do
    ar_EndPointNames[i] := Format('%s (%s)',[WideCharToString(ar_endpointdevices[i].DeviceName),
                                             WideCharToString(ar_endpointdevices[i].DeviceDesc)]);

  Result := hr;
end;


function TWasApiFileRenderer.Initialize(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
try
try
  // The function CoCreateInstance to get the device enumerator is handled in
  // GetDefaultEndPointAudioDevice or GetEndPointDeviceByID.

  if UseDefaultDevice then
    begin
      hr := GetDefaultEndPointAudioDevice(if_Endpoint,
                                          eConsole,
                                          EDataFlowEx.eRender);
      if FAILED(hr) then
        raise Exception.Create('Initialize Failure: Unable to retrieve default endpoint');
    end
  else
    begin
      hr := GetEndPointDeviceByID(ar_endpointdevices[CurrentEndPointDeviceId].pwszID,
                                  if_Endpoint);
      if FAILED(hr) then
        raise Exception.Create('Initialize Failure: Unable to retrieve endpoint');
    end;

  if Succeeded(hr) then
    hr := if_Endpoint.Activate(IID_IAudioClient2,
                               CLSCTX_ALL,
                               Nil,
                               Pointer(if_AudioClient));
  if FAILED(hr) then
    raise Exception.Create('Initialize Failure: Unable to activate endpoint');

  //
  cl_WavFileReader := TWavFileReader.Create(FileName, fmOpenRead + fmShareDenyNone);


  //
  //  Create our shutdown event - we want an auto reset event that starts in the not-signaled state.
  //
  ev_ShutdownEvent := CreateEventEx(Nil,
                                    Nil,
                                    0,
                                    EVENT_MODIFY_STATE or SYNCHRONIZE);
  if ev_ShutdownEvent = 0 then
    raise Exception.Create('Initialize Failure: Unable to create shutdown event');

  ev_AudioSamplesReadyEvent := CreateEventEx(Nil,
                                             Nil,
                                             0,
                                             EVENT_MODIFY_STATE or SYNCHRONIZE);
  if ev_AudioSamplesReadyEvent = 0 then
    raise Exception.Create('Initialize Failure: Unable to create samples ready event');

except
  // Do nothing. Caller is responsible for further error handling.
end;
finally
  Result := hr;
end;
end;


function TWasApiFileRenderer.AdjustFormatTo16Bits(pwfx: PWAVEFORMATEX): Boolean;
var
  bRet: Boolean;
  pEx: WAVEFORMATEXTENSIBLE;

begin

  bRet := False;

  if (pwfx.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) then
    pwfx.wFormatTag := WAVE_FORMAT_PCM
  else if (pwfx.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
    begin

      pEx.Format := pwfx^;

      if (IsEqualGUID(KSDATAFORMAT_SUBTYPE_IEEE_FLOAT, pEx.SubFormat)) then
        begin
          pEx.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
          pEx.Samples.wValidBitsPerSample := 16;
        end;
    end
  else
    begin
      Result := bRet;
      Exit;
    end;

  pwfx.wBitsPerSample  := 16;
  pwfx.nBlockAlign     := (pwfx.nChannels * pwfx.wBitsPerSample) div 8;
  pwfx.nAvgBytesPerSec := pwfx.nBlockAlign * pwfx.nSamplesPerSec;

  bRet := True;
  Result := bRet;
end;


function TWasApiFileRenderer.IsEventDrivenModeSupported(): Boolean;
var
  hr: HResult;
  propst: IPropertyStore;
  vvar: PROPVARIANT;

begin

try
  PropVariantInit(vvar);

  hr := if_Endpoint.OpenPropertyStore(STGM_READ,
                                      propst);
  if Failed(hr) then
    Exit;

  hr := propst.GetValue(PKEY_AudioEndpoint_Supports_EventDriven_Mode,
                        vvar);
  if Failed(hr) then
    Exit;

  if vvar.boolVal then
    hr := S_OK
  else
    hr := S_FALSE;

finally
  PropVariantClear(vvar);
  Result := (hr = S_OK);
end;
end;


function TWasApiFileRenderer.IsWavFormatSupported(): Boolean;
var
  hr: HResult;
  propst: IPropertyStore;
  vvar: PROPVARIANT;

begin

try
  PropVariantInit(vvar);

  hr :=  if_Endpoint.OpenPropertyStore(STGM_READ,
                                       propst);
  if Failed(hr) then
    Exit;

  hr := propst.GetValue(PKEY_AudioEngine_DeviceFormat,
                        vvar);
  if Failed(hr) then
    Exit;

  hr := if_Endpoint.Activate(IID_IAudioClient2,
                             CLSCTX_ALL,
                             Nil,
                             Pointer(if_AudioClient));
  if Failed(hr) then
    Exit;

  hr := if_AudioClient.IsFormatSupported(AUDCLNT_SHAREMODE_EXCLUSIVE,
                                         PWAVEFORMATEX(vvar.blob.pBlobData),
                                         Nil);
finally
  PropVariantClear(vvar);
  Result := (hr = S_OK);
end;
end;


function TWasApiFileRenderer.RenderAudioStream(): HResult;
var
  hr: HResult;
  hnsDefaultDevicePeriod: REFERENCE_TIME;
  hnsActualDuration: REFERENCE_TIME;
  pRenderClient: IAudioRenderClient;
  pwfx: PWAVEFORMATEX;
  bufferFrameCount: UINT32;
  numFramesAvailable: UINT32;
  numFramesPadding: UINT32;
  pData: PByte;
  flags: DWORD;
  evnt: HEVENT;
  task: HTASK;
  taskIndex: DWORD;
  AudioClntProps: TAudioClientProperties;
  iResbuf: Integer;

begin

  hnsDefaultDevicePeriod := REFTIMES_PER_SEC;
  hr := 0;
  flags := 0;
  taskIndex := 0;
  ZeroMemory(@pwfx,
             SizeOf(WAVEFORMATEX));

try

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         if_DeviceEnumerator);

  if Failed(hr) then
    Exit;

  hr := if_DeviceEnumerator.GetDefaultAudioEndpoint(eRender,
                                                    eConsole,
                                                    if_Endpoint);
  if Failed(hr) then
    Exit;

  hr := if_Endpoint.Activate(IID_IAudioClient2,
                             CLSCTX_ALL,
                             Nil,
                             Pointer(if_AudioClient));
  if Failed(hr) then
    Exit;

  // Set AudioClientProperties struct.
  AudioClntProps.eCategory := AudioCategory_Media;
  {Determine if Offload is supported}
  hr := if_AudioClient.IsOffloadCapable(AudioClntProps.eCategory,
                                        AudioClntProps.bIsOffload);
  AudioClntProps.Options := AUDCLNT_STREAMOPTIONS_MATCH_FORMAT;
  AudioClntProps.cbSize := SizeOf(AudioClntProps);

  // Set the audioclient properties, must do this before all other setting!
  hr := if_AudioClient.SetClientProperties(AudioClntProps);

  // Test if format is supported otherwise force the format to supported format.
  if not IsWavFormatSupported() then
    AdjustFormatTo16Bits(pwfx);

  // Call a helper function to negotiate with the audio
  // device for an exclusive-mode stream format.
  hr := GetStreamFormat(if_AudioClient, pwfx);
  if Failed(hr) then
    Exit;

  hr := if_AudioClient.GetMixFormat(pwfx);
  if Failed(hr) then
    Exit;


  // Initialize the stream to play at the minimum latency.
  hr := if_AudioClient.GetDevicePeriod(Nil,
                                       @hnsDefaultDevicePeriod);
  if Failed(hr) then
    Exit;

  hr := if_AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                  AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                                  AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM {or
                                  AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY},
                                  hnsDefaultDevicePeriod,
                                  0,
                                  pwfx,
                                  Nil);
  if Failed(hr) then
    Exit;


  // Tell the audio source which format to use.
  hr := cl_WavFileReader.SetFormat(pwfx^);
  if Failed(hr) then
    Exit;

  // Events ///////////
  if not IsEventDrivenModeSupported() then
    begin
      hr := E_FAIL;
      Exit;
    end;

  // Create an event handle and register it for
  // buffer-event notifications.
  evnt := CreateEventEx(Nil,
                        Nil,
                        0,
                        EVENT_MODIFY_STATE or SYNCHRONIZE);

  if (evnt = 0) then
    begin
      hr := E_FAIL;
      Exit;
    end;

  hr := if_AudioClient.SetEventHandle(evnt);
  if Failed(hr) then
    begin
      hr := AUDCLNT_E_DEVICE_INVALIDATED;
      Exit;
    end;

  // Get the actual size of the allocated buffer.
  hr := if_AudioClient.GetBufferSize(bufferFrameCount);
  if Failed(hr) then
    Exit;

   hr := if_AudioClient.GetService(IID_IAudioRenderClient,
                                   pRenderClient);
  if Failed(hr) then
    Exit;

  // Grab the entire buffer for the initial fill operation.
  // To reduce latency, load the first buffer with data
  // from the audio source before starting the stream.
  hr := pRenderClient.GetBuffer(bufferFrameCount,
                                pData);
  if Failed(hr) then
    Exit;


  // Load the initial data into the shared buffer.
  iResbuf := cl_WavFileReader.LoadData(0,
                                       bufferFrameCount,
                                       pData,
                                       flags);
  if iResBuf = 0 then
    Exit;

  hr := pRenderClient.ReleaseBuffer(bufferFrameCount,
                                    flags);
  if Failed(hr) then
    Exit;

  // Ask MMCSS to temporarily boost the thread priority
  // to reduce glitches while the low-latency stream plays.
  task := AvSetMmThreadCharacteristics('Pro Audio',
                                       taskIndex);
  if (task = 0) then
    begin
      hr := E_FAIL;
      Exit;
    end;


  // Calculate the actual duration of the allocated buffer.
  hnsActualDuration := (REFTIMES_PER_SEC * bufferFrameCount) div pwfx.nSamplesPerSec;

  // Start playing.
  hr := if_AudioClient.Start();
  if Failed(hr) then
    Exit;

  // Each loop fills about half of the shared buffer.
  while (flags <> AUDCLNT_BUFFERFLAGS_SILENT) do
    begin
      Sleep(DWORD(hnsActualDuration div REFTIMES_PER_MILLISEC div 2));

      // Wait for next buffer event to be signaled.
      {waitval := WaitForSingleObject(evnt, 2000);
      if (waitval <> WAIT_OBJECT_0) then
        begin
          // Event handle timed out after a 2-second wait.
          if_AudioClient.Stop();
          hr := ERROR_TIMEOUT;
          break;
        end; }

      // See how much buffer space is available.
      hr := if_AudioClient.GetCurrentPadding(numFramesPadding);
      if Failed(hr) then
        Exit;

      numFramesAvailable := bufferFrameCount - numFramesPadding;

      // Grab the next empty buffer from the audio device.
      hr := pRenderClient.GetBuffer(numFramesAvailable,
                                    pData);

      // Grab all the available space in the shared buffer.
      //hr := pRenderClient.GetBuffer(numFramesAvailable,
      //                              pData);
      if Failed(hr) then
        Exit;

      // Load the buffer with data from the audio source.
      iResBuf := cl_WavFileReader.LoadData(0,
                                           numFramesAvailable,
                                           pData,
                                           flags);
      if iResBuf = 0 then
        break;

      hr := pRenderClient.ReleaseBuffer(numFramesAvailable,
                                        flags);
      if Failed(hr) then
        Exit;

      //Application.ProcessMessages;  //needs vcl
    end;

  // Wait for last data in buffer to play before stopping.
  Sleep(DWORD(hnsActualDuration div REFTIMES_PER_MILLISEC div 2));

  if (hr = S_OK) then
    hr := if_AudioClient.Stop();  // Stop playing.

  if Failed(hr) then
    Exit;

finally

  if evnt <> 0 then
    CloseHandle(evnt);

  if task <> 0 then
    AvRevertMmThreadCharacteristics(task);

  CoTaskMemFree(pwfx);
  Result := hr;
end;
end;


function TWasApiFileRenderer.GetStreamFormat(AudioClient: IAudioClient;
                                             var ppwfx: PWAVEFORMATEX): HResult;
var
  hr: HResult;
  wfx: WAVEFORMATEXTENSIBLE;
  pwfxStream: PWAVEFORMATEX;
  wHeader: TWaveHeader;
  wDataHdr: TDataHeader;
  bytepos: Int64;

begin
  ZeroMemory(@wfx,
             SizeOf(WAVEFORMATEXTENSIBLE));

  cl_WavFileReader.ReadWaveHeader(wHeader,
                                  pwfxStream,
                                  wDataHdr,
                                  bytepos);




  wfx.Format.cbSize               := 22; // minimum required, and all that is needed (skips the SubFormat)
  wfx.Format.wFormatTag           := WAVE_FORMAT_EXTENSIBLE;
  wfx.Format.nChannels            := pwfxStream.nChannels; //2;
  wfx.Format.nSamplesPerSec       := pwfxStream.nSamplesPerSec; // 48000;
  wfx.Format.wBitsPerSample       := pwfxStream.wBitsPerSample; // 32;
  wfx.Format.nBlockAlign          := pwfxStream.nBlockAlign; // 8;
  wfx.Format.nAvgBytesPerSec      := pwfxStream.nAvgBytesPerSec; //384000;
  wfx.Samples.wValidBitsPerSample := 24;
  wfx.dwChannelMask := KSAUDIO_SPEAKER_STEREO; // KSAUDIO_SPEAKER_5POINT1;
  wfx.SubFormat     := KSDATAFORMAT_SUBTYPE_WAVEFORMATEX; // KSDATAFORMAT_SUBTYPE_PCM;

  hr := AudioClient.IsFormatSupported(AUDCLNT_SHAREMODE_SHARED, // AUDCLNT_SHAREMODE_EXCLUSIVE,
                                      @wfx.Format,
                                      ppwfx);

  if (hr <> S_OK) then
    begin
      // IAudioClient.IsFormatSupported failed
      // We need to do some result checks and process them
      // Given format is not supported but ppClosestMatch is
      if (hr = S_FALSE) then
        begin
          // Force the proposed WAVEFORMAT
          //ppwfx := wfx.Format;

          CopyMemory(ppwfx,
                     @wfx.Format,
                     SizeOf(wfx));

          hr := S_OK;
        end
      // ppClosestMatch is Nil & AUDCLNT_SHAREMODE_SHARED
      else if (hr = E_POINTER) then
        raise Exception.Create('No closest format has been found.')
      // The Windows audio service is not running.
      else if (hr = HResult(AUDCLNT_E_SERVICE_NOT_RUNNING)) then
        raise Exception.Create('The Windows audio service is not running.')
      // WAS device was removed
      else if (hr = HResult(AUDCLNT_E_DEVICE_INVALIDATED)) then
        raise Exception.Create('WAS device is removed.');
    end;

  if (hr = S_OK) then
    begin
       // Returning the proposed WAVEFORMAT anyway
       // Force it
       // ppwfx = reinterpret_cast<WAVEFORMATEX *>(CoTaskMemAlloc(sizeof(wfx)));
      ppwfx^ := wfx.Format;

      ppwfx := CoTaskMemAlloc(SizeOf(wfx));

      CopyMemory(ppwfx,
                 @wfx.Format,
                 SizeOf(wfx));

    end;
  ppwfx := CoTaskMemAlloc(SizeOf(pwfxStream));
  CopyMemory(ppwfx,
             pwfxStream,
             SizeOf(pwfxStream));
  ppwfx := pwfxStream;
  Result := S_OK;
end;





// TWavFileReader //////////////////////////////////////////////////////////////

constructor TWavFileReader.Create(const AFileName: string; Mode: Word);
begin
  inherited;
  //
end;

destructor TWavFileReader.Destroy();
begin
  //
  inherited;
end;



function TWavFileReader.LoadData(const FilePointer: Int64;
                                 FrameCount: DWord;
                                 var Data: PByte;
                                 var Flags: DWord): Integer;
var
  aByte: array of Byte;
  rResult: Integer;
begin


try
  SetLength(aByte, FrameCount);

  if (FilePointer > 0) and (Self.Position <> FilePointer) then
    Self.Position := FilePointer;

  rResult := Self.Read(aByte[0], FrameCount * SizeOf(Byte));
  //rResult := Self.Read(aByte, Length(aByte));

  Data := CoTaskMemAlloc(Length(aByte));

  CopyMemory(Data,
             @aByte[0],
             FrameCount);

  // check if read result = the requested number of frames
  if rResult > FrameCount then
    Flags := 0
  else
    Flags := AUDCLNT_BUFFERFLAGS_SILENT;

finally
  SetLength(aByte, 0);
  Result := rResult;
end;

end;


function TWavFileReader.SetFormat(const WaveFormat: WAVEFORMATEX): HResult;
var
  fmt: PWAVEFORMATEX;
begin

  fmt := @fr_WaveFormat;

  CopyMemory(fmt,
             @WaveFormat,
             SizeOf(WAVEFORMATEX));

  if fmt = Nil then
    begin
      fr_WaveFormat := fmt^;
      Result := E_POINTER;
    end
  else
    Result := S_OK;

end;


function TWavFileReader.ReadWaveHeader(out Header: TWaveHeader;
                                       out WaveFormat: PWAVEFORMATEX;
                                       out Data: TDataHeader;
                                       out DataPos: Int64): HResult;
var
  hr: HResult;
  wHeader: TWaveHeader;
  wFmtChunk: TFormatChunk;
  wFmtEx: WAVEFORMATEX;
  wDataHdr: TDataHeader;
  rCount: DWord;
  PSearchBuffer: array of DWord;
  bTagFound: Boolean;
  i: Integer;

begin
  hr := E_UNEXPECTED;
  rCount := 0;

try

  if not FileExists(Filename) then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      Exit;
    end;

   Self.Seek(0, soBeginning);

   // Read the RIFF-header
   Self.Read(wHeader, Sizeof(wHeader));

   // We do some checks if this is a valid WAV-file
   if wHeader.tagRiff <> FCC('RIFF') then
     begin
       hr := ERROR_INVALID_DATA;
       Exit;
     end;

   if wHeader.tagWave <> FCC('WAVE') then
   begin
     hr := ERROR_INVALID_DATA;
     Exit;
   end;

   Self.Read(wFmtChunk, Sizeof(wFmtChunk));
   if (wFmtChunk.tagFmt <> FCC('fmt ')) then  // We found one of the many infotags, so, we need to find the tag
     begin
       Self.Position := 0;
       SetLength(PSearchBuffer, 1024); // Set length of the filebuffer
       bTagFound := False;
       // readout the buffer
       while (bTagFound = False) do
         begin
           Self.Read(PSearchBuffer[0], 1024 * Sizeof(PSearchBuffer));
           for i := 0 to Length(PSearchBuffer)-1 do
             begin
               inc(rCount, 4);
               if PSearchBuffer[i] = FCC('fmt ') then
                 begin
                   bTagFound := True;
                   Self.Seek(rCount - 4, soBeginning);
                   Break;
                 end;
             end;
         end;

       if bTagFound then
         begin
           Self.Read(wFmtChunk, Sizeof(wFmtChunk));
           // Check format chunk & length. If format chunk is less than 16,
           // we are not dealing with PCM data and we can't play this file.
           if (wFmtChunk.tagFmt <> FCC('fmt ')) or (wFmtChunk.szFmt <16 ) then
             begin
               hr := ERROR_INVALID_DATA;
               Exit;
             end;
         end;
     end;

   // Ok, export the header
   header := wHeader;

   // Get the Waveformatex structure
   Self.Read(wFmtEx, wFmtChunk.szFmt);

   if wFmtChunk.szFmt <> SizeOf(WAVEFORMATEX) then
     begin
       hr := ERROR_INVALID_DATA;
       Exit;
     end;

   WaveFormat := @wFmtEx;

   // Finaly get the dataheader
   Self.Read(wDataHdr, SizeOf(wDataHdr));

   if wDataHdr.szData = 0 then
     wDataHdr.szData := DWord(Self.Size - Self.Position);

   // Check if datasize complies with size given in szData-tag
   if (wDataHdr.szData <> DWord(Self.Size - Self.Position)) then
     begin
       hr := ERROR_DATA_CHECKSUM_ERROR;
       Exit;
     end;

   Data := wDataHdr;
   DataPos := Dword(Self.Position);  // Return the current fileposition what also is th startingpoint of the audiodata.

   hr := S_OK;

finally
  SetLength(PSearchBuffer, 0);
  Result := hr;
end;
end;


// Helpers /////////////////////////////////////////////////////////////////////

function FCC(ch4: TCh4): DWord; inline;
begin
  Result :=  DWord(Ord(ch4[0])) or
             DWord(Ord(ch4[1])) shl 8 or
             DWord(Ord(ch4[2])) shl 16 or
             DWord(Ord(ch4[3])) shl 24;
end;

end.

 unit CaptureEngine;


interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.Classes,
  System.Win.ComObj,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropVarUtil,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfCaptureEngine,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib,
  WinAPI.MediaFoundationApi.MfUtils,
  {$IFDEF DEBUG}
  WinApi.MediaFoundationApi.MfMediaTypeDebug,
  {$ENDIF}
  {WIC}
  WinApi.WIC.WinCodec,
  {DirectX Note: Do not use d3d11 <= Delphi 10.4}
  Winapi.DirectX.D3DCommon,
  WinApi.DirectX.D3D11,
  {Application}
  SampleConverter_V1,
  DeviceExplorer,
  Utils;

{$DEFINE LOGDEBUG}


const
  WM_APP_CAPTURE_EVENT = WM_APP + 1001;
  WM_RECIEVED_SAMPLE_FROM_CALLBACK = WM_APP + 1002;

  IDD_CHOOSE_DEVICE                   = 101;
  IDS_ERR_SET_DEVICE                  = 102;
  IDS_ERR_INITIALIZE                  = 103;
  IDS_ERR_PREVIEW                     = 104;
  IDS_ERR_RECORD                      = 105;
  IDS_ERR_CAPTURE                     = 106;
  IDS_ERR_PHOTO                       = 107;
  IDS_ERR_BADREQUEST                  = 108;
  IDS_ERR_CAPTURE_SINK_PREPARED       = 109;

var
  // DXGI DevManager support
  g_pDXGIMan: IMFDXGIDeviceManager;
  g_pDX11Device: ID3D11Device;
  g_ResetToken: UINT;

type
  PSampleFromCallBack = ^TSampleFromCallBack;
  TSampleFromCallBack = record
    pSample: IMFSample;
  end;

  TSnapShotOptions = (ssoFile,
                      ssoCallBack,
                      ssoStream);


  // CaptureManager class
  // Wraps the capture engine and implements the event callback and OnSampleCallback in a nested class.
type
  TCaptureManager = class(TInterfacedPersistent)

{$REGION TCaptureManagerCallBack}
  // The event callback object.
  type TCaptureManagerCallBack = class(TInterfacedPersistent, IMFCaptureEngineOnEventCallback)
    private
      hwndHandle: HWND;

      // Implementation of IMFCaptureEngineOnEventCallback
      function OnEvent(pEvent: IMFMediaEvent): HResult; stdcall;

    public

      FCapMan: TCaptureManager;

      constructor Create(_hwnd: HWND); //virtual;
      destructor Destroy(); override;

    end;
{$ENDREGION}


{$REGION TCaptureEngineOnSampleCallback}
  // OnSampleCallBack object
  type TCaptureEngineOnSampleCallback = class(TInterfacedPersistent, IMFCaptureEngineOnSampleCallback)
    private
      hwndDest: HWnd;
      m_pManager2: TCaptureManager;
      // Implementation of IMFCaptureEngineOnEventCallback
      function OnSample(pSample: IMFSample): HResult; stdcall;
      //
    public
      constructor Create(_hwnd: HWND); //virtual;
      destructor Destroy(); override;

    end;
{$ENDREGION}


  private

    FCaptureEngine: IMFCaptureEngine;
    FCapturePreviewSink: IMFCapturePreviewSink;

    {$IFDEF DEBUG}
      FMediaTypeDebug: TMediaTypeDebug;
    {$ENDIF}

    hwndMainForm: HWND;
    hwndPreviewWindow: HWND;

    FCaptureManagerCallBack: TCaptureManagerCallBack;
    FCaptureEngineOnSampleCallback: TCaptureEngineOnSampleCallback;

    bPreviewing: Boolean;
    bRecording: Boolean;
    bPhotoPending: Boolean;
    bPhotoTaken: Boolean;
    bEngineIsInitialized: Boolean;
    bCaptureSinkReady: Boolean;

    FSnapShotOptions: TSnapShotOptions;

    FSampleFromCallBack: TSampleFromCallBack;
    FCritSec: TMFCritSec;

    constructor Create(pEvent: HWND); virtual;
    procedure DestroyCaptureEngine();

    // Capture Event Handlers
    procedure OnCaptureEngineInitialized(hrStatus: HResult);
    procedure OnPreviewStarted(hrStatus: HResult);
    procedure OnPreviewStopped(hrStatus: HResult);
    procedure OnCaptureEngineOutputMediaTypeSet(hrStatus: HResult);
    procedure OnRecordStarted(hrStatus: HResult);
    procedure OnRecordStopped(hrStatus: HResult);
    procedure OnPhotoTaken(hrStatus: HResult);
    procedure OnCaptureSinkPrepared(hrStatus: HResult);

    function SetSnapShotOption(pPhotoSink: IMFCapturePhotoSink): HResult;

  public
    FhEvent: THandle;
    FSampleConverter: TSampleConverter;

    destructor Destroy(); override;

    class function CreateCaptureEngine(hEvent: HWND;
                                       out pEngine: TCaptureManager): HResult;

    function InitializeCaptureManager(const hPreviewObject: HWND;
                                      const hMainForm: HWND;
                                      const Unk: IUnknown): HResult;

    function OnCaptureEvent(var pwParam: WPARAM;
                            var plParam: LPARAM): HResult;

    function UpdateVideo(pSrc: PMFVideoNormalizedRect): HResult;


    function SetVideoFormat(): HResult;
    function SetMediaType(pMediaType: IMFMediaType): HResult;
    function GetCurrentFormat(): TVideoFormatInfo;

    function StartPreview(): HResult;
    function StopPreview(): HResult;
    function StartRecording(pszDestinationFile: PCWSTR): HResult;
    function StopRecording(): HResult;
    function TakePhoto(ASnapShotOption: TSnapShotOptions): HResult;

    property IsRecording: Boolean read bRecording write bRecording;
    property IsPreviewing: Boolean read bPreviewing write bPreviewing;
    property IsPhotoPending: Boolean read bPhotoPending write bPhotoPending;
    property IsPhotoTaken: Boolean read bPhotoTaken write bPhotoTaken;
    property IsInitialized: Boolean read bEngineIsInitialized;

    property SnapShotOption: TSnapShotOptions read FSnapShotOptions write FSnapShotOptions;
    property PreviewHandle: HWND read hwndPreviewWindow write hwndPreviewWindow;
    property EventHandle: HWND read hwndMainForm;

  end;

var
  FCaptureManager: TCaptureManager;


implementation


uses
  WinApi.D3D10,
  WinApi.Unknwn;


// D3D11 ///////////////////////////////////////////////////////////////////////

function CreateDX11Device(out ppDevice: ID3D11Device;
                          out ppDeviceContext: ID3D11DeviceContext;
                          out pFeatureLevel: D3D_FEATURE_LEVEL): HResult;
var
  hr: HResult;
  aFeatureLevels: array[0..6] of D3D_FEATURE_LEVEL;
  pMultithread: ID3D10Multithread;

const
   // This should be in the D3D11_CREATE_DEVICE_FLAG enumeration of WinApi.D3D11.pas
   D3D11_CREATE_DEVICE_VIDEO_SUPPORT = $800;

begin

  aFeatureLevels[0] := D3D_FEATURE_LEVEL_11_1;
  aFeatureLevels[1] := D3D_FEATURE_LEVEL_11_0;
  aFeatureLevels[2] := D3D_FEATURE_LEVEL_10_1;
  aFeatureLevels[3] := D3D_FEATURE_LEVEL_10_0;
  aFeatureLevels[4] := D3D_FEATURE_LEVEL_9_3;
  aFeatureLevels[5] := D3D_FEATURE_LEVEL_9_2;
  aFeatureLevels[6] := D3D_FEATURE_LEVEL_9_1;

  hr := D3D11CreateDevice(nil,
                          D3D_DRIVER_TYPE_HARDWARE,
                          0,
                          D3D11_CREATE_DEVICE_VIDEO_SUPPORT,  // Not implemented in the D3Dii API Delphi 10.3  !
                          @aFeatureLevels,
                          Length(aFeatureLevels),
                          D3D11_SDK_VERSION,
                          ppDevice,
                          pFeatureLevel,
                          ppDeviceContext
                         );

  if SUCCEEDED(hr) then
    begin
      hr := ppDevice.QueryInterface(IID_ID3D10Multithread,
                                    pMultithread);

      if SUCCEEDED(hr) then
        pMultithread.SetMultithreadProtected(True);

      SafeRelease(pMultithread);
    end;

  Result := hr;
end;


//
function CreateD3DManager(): HResult;
var
  hr: HResult;
  FeatureLevel: D3D_FEATURE_LEVEL;
  pDX11DeviceContext: ID3D11DeviceContext;

begin

  hr := CreateDX11Device(g_pDX11Device,
                         pDX11DeviceContext,
                         FeatureLevel);

  if SUCCEEDED(hr) then
    hr := MFCreateDXGIDeviceManager(g_ResetToken,
                                    g_pDXGIMan);


  if SUCCEEDED(hr) then
    hr := g_pDXGIMan.ResetDevice(g_pDX11Device,
                                 g_ResetToken);

  Result := hr;
end;



// CaptureManagerCallBack routines /////////////////////////////////////////////
//
constructor TCaptureManager.TCaptureManagerCallBack.Create(_hwnd: HWND);
begin
  inherited Create();
  hwndHandle := _hwnd;
end;

//
destructor TCaptureManager.TCaptureManagerCallBack.Destroy();
begin
  SafeRelease(FCapMan);
  inherited Destroy();
end;

//
class function TCaptureManager.CreateCaptureEngine(hEvent: HWND;
                                                   out pEngine: TCaptureManager): HResult;
var
  hr: HResult;
  cmEngine: TCaptureManager;

label
  Done;

begin
  hr := S_OK;
  pEngine := nil;

  cmEngine := TCaptureManager.Create(hEvent);
  if not Assigned(cmEngine) then
    begin
      hr := E_OUTOFMEMORY;
      goto Done;
    end;

  pEngine := cmEngine;

Done:
  SafeRelease(cmEngine);
  Result := hr;
end;


// Callback method to receive events from the capture engine.
function TCaptureManager.TCaptureManagerCallBack.OnEvent(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
  i: Integer;

begin

  // Post a message to the application window, so the event is handled
  // on the application's main thread.
  // The application will release the pointer when it handles the message.

  { if PostMessage(FCapMan.hwndMainForm,
                 WM_APP_CAPTURE_EVENT,
                 WParam(IMFMediaEvent(Pointer(pEvent))),
                 LPARAM(0)) then
    hr := S_OK
  else
    hr := E_FAIL; }

  i := SendMessage(FCapMan.hwndMainForm,
                   WM_APP_CAPTURE_EVENT,
                   WParam(IMFMediaEvent(pEvent)),
                  LPARAM(0));
  if (i > 0) then
    hr := S_OK
  else
    hr := E_FAIL;

  Result := hr;
end;


// TCaptureEngineOnSampleCallback //////////////////////////////////////////////

constructor TCaptureManager.TCaptureEngineOnSampleCallback.Create(_hwnd: HWND);
begin
  inherited Create();
  hwndDest := _hwnd;
  m_pManager2 := TCaptureManager.Create(_hwnd);
end;


destructor TCaptureManager.TCaptureEngineOnSampleCallback.Destroy();
begin
  SafeRelease(m_pManager2);
  inherited Destroy();
end;


function TCaptureManager.TCaptureEngineOnSampleCallback.OnSample(pSample: IMFSample): HResult;
var
  //pData: TMemoryStream;
  hr: HResult;

begin
  hr := S_OK;

  { TODO : convert sample to bitmap send bitmap to mainform snapshot rect (TPaintbox) }
  if (pSample <> nil) then
    begin






    //  m_SampleConverter.DataFromSample(pSample,
    //
    //                                   pData);



     m_pManager2.FSampleFromCallBack.pSample := pSample;

     SendMessage(hwndDest,
                        WM_RECIEVED_SAMPLE_FROM_CALLBACK,
                        WPARAM(m_pManager2.FSampleFromCallBack),
                        0);

     // SafeRelease(m_pManager2.FSampleFromCallBack.pSample);

    end;

  Result := hr;
end;



// CaptureManager routines /////////////////////////////////////////////////////


function TCaptureManager.InitializeCaptureManager(const hPreviewObject: HWND;
                                                  const hMainForm: HWND;
                                                  const Unk: IUnknown): HResult;
var
  hr: HResult;
  mfAttributes: IMFAttributes;
  mfFactory: IMFCaptureEngineClassFactory;

label
  Done;

begin
  if (Unk = nil) then
    begin
      hr := E_POINTER;
      goto Done;
    end;

  DestroyCaptureEngine();

  FhEvent := CreateEvent(nil,
                         False,
                         False,
                         nil);

  if FhEvent = 0 then
    begin
      hr := HRESULT_FROM_WIN32(GetLastError());
      goto Done;
    end;

  // Create the CaptureManagerCallBack class
  FCaptureManagerCallBack := TCaptureManagerCallBack.Create(hwndMainForm);
  if not Assigned(FCaptureManagerCallBack) then
    begin
      hr := E_OUTOFMEMORY;
      goto Done;
    end;

  FCaptureManagerCallBack.FCapMan := Self;
  hwndPreviewWindow := hPreviewObject;

  // Create the OnSampleCallBack class
  FCaptureEngineOnSampleCallback := TCaptureEngineOnSampleCallback.Create(hwndMainForm);
  if not Assigned(FCaptureEngineOnSampleCallback) then
    begin
      hr := E_OUTOFMEMORY;
      goto Done;
    end;

  // Create a D3D Manager
  hr := CreateD3DManager();
  if FAILED(hr) then
    goto Done;

  hr := MFCreateAttributes(mfAttributes,
                           1);
  if FAILED(hr) then
    goto Done;

  hr := mfAttributes.SetUnknown(MF_CAPTURE_ENGINE_D3D_MANAGER,
                                g_pDXGIMan);
  if FAILED(hr) then
    goto Done;


  // Create the captureengine
  // Create the factory object for the capture engine.
  // pr_MediaEngineClassFactory is a reference to the IMFMediaEngineClassFactory interface.

  // C++ style:
  hr := CoCreateInstance(CLSID_MFCaptureEngineClassFactory,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMFCaptureEngineClassFactory,
                         mfFactory);
  if FAILED(hr) then
    goto Done;

  // Or Delphi style:
  //mfFactory := CreateCOMObject(CLSID_MFCaptureEngineClassFactory) as IMFCaptureEngineClassFactory;
  //if not Assigned(mfFactory) then
  //  goto Done;

  // Create and initialize the CaptureEngine.
  hr := mfFactory.CreateInstance(CLSID_MFCaptureEngine,
                                 IID_IMFCaptureEngine,
                                 Pointer(FCaptureEngine));
  if FAILED(hr) then
    goto Done;


  // Initialize the captureengine
  hr := FCaptureEngine.Initialize(FCaptureManagerCallBack,
                                  mfAttributes,
                                  nil,
                                  {IMFActivate} Unk);


Done:
  // Ignore returnvalue MF_E_INVALIDREQUEST in case user did not select a device .
  if (hr = MF_E_INVALIDREQUEST) then
    hr := S_OK;

  if FAILED(hr) then
    ErrMsg(ERR_INITIALIZE,
           hr);

  Result := hr;
end;


// Create
constructor TCaptureManager.Create(pEvent: HWND);
begin
  inherited Create();

  {$IFDEF DEBUG}
  FMediaTypeDebug := TMediaTypeDebug.Create();
  {$ENDIF}

  FCritSec := TMFCritSec.Create;
  FSampleConverter := TSampleConverter.Create();
  hwndMainForm := pEvent;
  bRecording := False;
  bPreviewing := False;
  bPhotoPending := False;
  bEngineIsInitialized := False;
  FhEvent := 0;
end;


// Destroy
destructor TCaptureManager.Destroy();
begin
  DestroyCaptureEngine();
  FreeAndNil(FSampleConverter);
  SafeDelete(FCritSec);
  {$IFDEF DEBUG}
  FMediaTypeDebug.Free();
  {$ENDIF}

  inherited Destroy();
end;


// DestroyCaptureEngine
// Callers: Destroy, OnCaptureEvent, InitializeCaptureManager
procedure TCaptureManager.DestroyCaptureEngine();
begin
  if FhEvent <> 0 then
     begin
       CloseHandle(FhEvent);
       FhEvent := 0;
     end;

  if Assigned(FCapturePreviewSink) then
    SafeRelease(FCapturePreviewSink);

  if Assigned(FCaptureEngine) then
    SafeRelease(FCaptureEngine);

  if Assigned(FCaptureManagerCallBack) then
   SafeRelease(FCaptureManagerCallBack);

  if Assigned(FCaptureEngineOnSampleCallback) then
      SafeRelease(FCaptureEngineOnSampleCallback);
  // ========================================================

  if Assigned(g_pDXGIMan) then
    begin
      g_pDXGIMan.ResetDevice(g_pDX11Device,
                             g_ResetToken);
    end;

  // release the D3D11 interfaces
  if Assigned(g_pDX11Device) then
    SafeRelease(g_pDX11Device);
  if Assigned(g_pDXGIMan) then
    SafeRelease(g_pDXGIMan);

  IsPreviewing := False;
  IsRecording := False;
  IsPhotoPending := False;
  bEngineIsInitialized := False;
end;


// OnCaptureEvent
// Handle an event from the capture engine.
// NOTE: This method is called from the application's UI thread.
function TCaptureManager.OnCaptureEvent(var pwParam: WPARAM;
                                        var plParam: LPARAM): HResult;
var
  guidType: TGUID;
  hrStatus: HResult;
  hr: HResult;
  pEvent: IMFMediaEvent;

begin

  pEvent := IMFMediaEvent(pwParam);

  hr := pEvent.GetStatus(hrStatus);
  if FAILED(hr) then
    hrStatus := hr;

  hr := pEvent.GetExtendedType(guidType);
  if SUCCEEDED(hr) then
    begin

      if (guidType = MF_CAPTURE_ENGINE_INITIALIZED) then
        OnCaptureEngineInitialized(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_PREVIEW_STARTED) then
        OnPreviewStarted(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_PREVIEW_STOPPED) then
        OnPreviewStopped(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_OUTPUT_MEDIA_TYPE_SET) then
        OnCaptureEngineOutputMediaTypeSet(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_RECORD_STARTED)  then
        OnRecordStarted(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_RECORD_STOPPED) then
        OnRecordStopped(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_PHOTO_TAKEN) then
        OnPhotoTaken(hrStatus)
      else if guidType = MF_CAPTURE_SINK_PREPARED then
        OnCaptureSinkPrepared(hrStatus)
      else if (guidType = MF_CAPTURE_ENGINE_ERROR) then
        DestroyCaptureEngine()
      else if (hrstatus = MF_E_INVALIDREQUEST) then
        ErrMsg('OnCaptureEvent: Invalid request',
               hr)
      else if (FAILED(hrStatus)) then
        ErrMsg('OnCaptureEvent: Unexpected error',
               hr);
    end;

   SetEvent(FhEvent);
   Result := hrStatus;
end;


// OnCaptureEngineInitialized
// Capture Engine Event Handlers
procedure TCaptureManager.OnCaptureEngineInitialized(hrStatus: HResult);
begin
  if (hrStatus = MF_E_NO_CAPTURE_DEVICES_AVAILABLE) then
    hrStatus := S_OK;  // No capture device. Not an application error.
  bEngineIsInitialized := SUCCEEDED(hrStatus);

  if FAILED(hrStatus) then
    ErrMsg('OnCaptureEngineInitialized ' + ERR_INITIALIZE,
           hrStatus);
end;


// OnPreviewStarted
procedure TCaptureManager.OnPreviewStarted(hrStatus: HResult);
begin
  IsPreviewing := SUCCEEDED(hrStatus);
  if FAILED(hrStatus) then
    ErrMsg('OnPreviewStarted: ' + ERR_PREVIEW,
           hrStatus);
end;


// OnPreviewStopped
procedure TCaptureManager.OnPreviewStopped(hrStatus: HResult);
begin
  IsPreviewing := not SUCCEEDED(hrStatus);
  if FAILED(hrStatus) then
    ErrMsg('OnPreviewStopped: ' + ERR_PREVIEW,
           hrStatus);
end;

// OnCaptureEngineOutputMediaTypeSet
procedure TCaptureManager.OnCaptureEngineOutputMediaTypeSet(hrStatus: HResult);
begin
  if FAILED(hrStatus) then
    ErrMsg('OnCaptureEngineOutputMediaTypeSet: ' + ERR_OUTPUT_MEDIATYPE_SET,
           hrStatus);
end;


// OnRecordStarted
procedure TCaptureManager.OnRecordStarted(hrStatus: HResult);
begin
  IsRecording := SUCCEEDED(hrStatus);
  if FAILED(hrStatus) then
    ErrMsg('OnRecordStarted: ' + ERR_RECORD,
           hrStatus);
end;


// OnRecordStopped
procedure TCaptureManager.OnRecordStopped(hrStatus: HResult);
begin
  IsRecording := not SUCCEEDED(hrStatus);
  if FAILED(hrStatus) then
    ErrMsg('OnRecordStopped: ' + ERR_RECORD,
           hrStatus);

end;


// OnPhotoTaken
procedure TCaptureManager.OnPhotoTaken(hrStatus: HResult);
begin
  bPhotoTaken := SUCCEEDED(hrStatus);
  if FAILED(hrStatus) then
    ErrMsg('OnPhotoTaken: ' + ERR_PHOTO,
           hrStatus)
  else //S_OK
    begin
      //FSampleConverter.UpdateConverter()
    end;

end;


// OnCaptureSinkPrepared
procedure TCaptureManager.OnCaptureSinkPrepared(hrStatus: HResult);
begin
  bCaptureSinkReady :=  SUCCEEDED(hrStatus);
  if FAILED(hrStatus) then
    ErrMsg('OnCaptureSinkPrepared: ' + ERR_CAPTURE,
           hrStatus);
end;


// StartPreview
function TCaptureManager.StartPreview(): HResult;
var
  pCaptureSink: IMFCaptureSink;
  pMediaType: IMFMediaType;
  pMediaType2: IMFMediaType;
  pCaptureSource: IMFCaptureSource;
  dwSinkStreamIndex: DWord;
  hr: HResult;

label
  Done;

begin
  FCritSec.Lock;

  if not Assigned(FCaptureEngine) then
    begin
      hr := MF_E_NOT_INITIALIZED;
      goto Done;
    end;

  if IsPreviewing then
    begin
      hr := S_OK;
      goto Done;
    end;

  // Get a pointer to the preview sink.
  if not Assigned(FCapturePreviewSink) then
    begin
      hr := FCaptureEngine.GetSink(MF_CAPTURE_ENGINE_SINK_TYPE_PREVIEW,
                                   pCaptureSink);
      if FAILED(hr) then
        goto Done;


      hr := pCaptureSink.QueryInterface(IID_IMFCapturePreviewSink,
                                        FCapturePreviewSink);
      if FAILED(hr) then
        goto Done;

      hr := FCapturePreviewSink.SetRenderHandle(hwndPreviewWindow);
      if FAILED(hr) then
        goto Done;

      hr := FCaptureEngine.GetSource(pCaptureSource);
      if FAILED(hr) then
        goto Done;

      // Create an empty MediaType.
      // It is advised to do so when passing a mediatype interface to a method.
      hr := MFCreateMediaType(pMediaType);
      if FAILED(hr) then
        goto Done;

      // Configure the video format for the preview sink.
      hr := pCaptureSource.GetCurrentDeviceMediaType(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_PREVIEW,
                                                     pMediaType);
      if FAILED(hr) then
        goto Done;

      {$IFDEF DEBUG}
      FMediaTypeDebug.LogMediaType(pMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('StartPreview');
      {$ENDIF}

      hr := MFCreateMediaType(pMediaType2);
      if FAILED(hr) then
        goto Done;

      hr := CloneVideoMediaType(pMediaType,
                                MFVideoFormat_RGB32,
                                pMediaType2);
      if FAILED(hr) then
        goto Done;

      hr := pMediaType2.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                                  1 { = UINT(True)});
      if FAILED(hr) then
        goto Done;


      {$IFDEF DEBUG}
      FMediaTypeDebug.LogMediaType(pMediaType2);
      FMediaTypeDebug.SafeDebugResultsToFile('StartPreview');
      {$ENDIF}

      // Connect the video stream to the preview sink.
      hr := FCapturePreviewSink.AddStream(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_PREVIEW,
                                          pMediaType2,
                                          nil,
                                          dwSinkStreamIndex);
      if FAILED(hr) then
        goto Done;
    end;

  hr := FCaptureEngine.StartPreview();


Done:
  FCritSec.Unlock;
  if FAILED(hr) then
    ErrMsg('StartPreview',
            hr);

  Result := hr;
end;


// StopPreview
function TCaptureManager.StopPreview(): HResult;
var
  hr: HResult;

label
  Done;

begin

  if not Assigned(FCaptureEngine) then
    begin
      hr := MF_E_NOT_INITIALIZED;
      goto Done;
    end;

  if not IsPreviewing then
    begin
      hr := S_OK;
      goto Done;
    end;

  hr := FCaptureEngine.StopPreview();

  if FAILED(hr) then
    goto Done;

  HandleMessages(GetCurrentThread());

done:
  if FAILED(hr) then
    ErrMsg('StopPreview',
            hr);

  Result := hr;
end;


// StartRecord
// Start recording to file
function TCaptureManager.StartRecording(pszDestinationFile: PCWSTR): HResult;
var
  pszExt: string;
  guidVideoEncoding: TGUID;
  guidAudioEncoding: TGUID;
  pSink: IMFCaptureSink;
  pRecord: IMFCaptureRecordSink;
  pSource: IMFCaptureSource;
  hr: HResult;

label
  Done;

begin

  if not Assigned(FCaptureEngine) then
    begin
      hr := MF_E_NOT_INITIALIZED;
      goto Done;
    end;

  if (IsRecording = True) then
    begin
      hr := MF_E_INVALIDREQUEST;
      goto Done;
    end;

  pszExt := ExtractFileExt(pszDestinationFile);

  // Check extension to match the proper formats
  if LowerCase(pszExt) = '.mp4' then
    begin
      guidVideoEncoding := MFVideoFormat_H264;
      guidAudioEncoding := MFAudioFormat_AAC;
    end
  else if LowerCase(pszExt) = '.wmv' then
    begin
      guidVideoEncoding := MFVideoFormat_WMV3;
      guidAudioEncoding := MFAudioFormat_WMAudioV9;
    end
  else if LowerCase(pszExt) = '.wma' then
    begin
      guidVideoEncoding := GUID_NULL;
      guidAudioEncoding := MFAudioFormat_WMAudioV9;
    end
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE;
      goto Done;
    end;


  hr := FCaptureEngine.GetSink(MF_CAPTURE_ENGINE_SINK_TYPE_RECORD,
                               pSink);
  if FAILED(hr) then
    goto Done;

  hr := pSink.QueryInterface(IID_IMFCaptureRecordSink,
                             pRecord);
  if FAILED(hr) then
    goto Done;

  hr := FCaptureEngine.GetSource(pSource);
  if FAILED(hr) then
    goto Done;

  // Clear any existing streams from previous recordings.
  hr := pRecord.RemoveAllStreams();
  if FAILED(hr) then
    goto Done;

  hr := pRecord.SetOutputFileName(pszDestinationFile);
  if FAILED(hr) then
    goto Done;

  // Configure the video and/or audio streams.
  if (guidVideoEncoding <> GUID_NULL) then
    begin
      hr := ConfigureVideoEncoding(pSource,
                                   pRecord,
                                   guidVideoEncoding);
      if FAILED(hr) then
        goto Done;
    end;

  if (guidAudioEncoding <> GUID_NULL) then
    begin
      hr := ConfigureAudioEncoding(pSource,
                                   pRecord,
                                   guidAudioEncoding);
      if FAILED(hr) then
        goto Done;
    end;


  hr := FCaptureEngine.StartRecord();
  if FAILED(hr) then
    goto Done;

  IsRecording := True;

done:
  if FAILED(hr) then
    ErrMsg('StartRecording',
            hr);

  Result := hr;
end;


function TCaptureManager.StopRecording(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  if IsRecording then
    begin
      hr := FCaptureEngine.StopRecord(True,
                                      False);
      HandleMessages(GetCurrentThread());
    end;

  if FAILED(hr) then
    ErrMsg('StopRecording',
            hr);

  Result := hr;
end;


function TCaptureManager.TakePhoto(ASnapShotOption: TSnapShotOptions): HResult;
var
  hr: HResult;
   pCaptureSink: IMFCaptureSink;
   pCapturePhotoSink: IMFCapturePhotoSink;
   pCaptureSource: IMFCaptureSource;
   pSinkWriter: IMFSinkWriter;
   pMediaType: IMFMediaType;
   pMediaType2: IMFMediaType;
   bHasPhotoStream: Boolean;
   dwSinkStreamIndex: DWORD;

label
  Done;

begin
  SnapShotOption := ASnapShotOption;
  bHasPhotoStream := True;

  // Get a pointer to the photo sink.
  hr := FCaptureEngine.GetSink(MF_CAPTURE_ENGINE_SINK_TYPE_PHOTO,
                               pCaptureSink);
  if FAILED(hr) then
    goto Done;

  hr := pCaptureSink.QueryInterface(IID_IMFCapturePhotoSink,
                                    pCapturePhotoSink);
  if FAILED(hr) then
    goto Done;

  hr := FCaptureEngine.GetSource(pCaptureSource);
  if FAILED(hr) then
    goto Done;

  hr := MfCreateMediaType(pMediaType);
  if FAILED(hr) then
    goto Done;

  hr := pCaptureSource.GetCurrentDeviceMediaType(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_PHOTO,
                                                 pMediaType);
  if FAILED(hr) then
    goto Done;

  {$IFDEF DEBUG}
  FMediaTypeDebug.LogMediaType(pMediaType);
  FMediaTypeDebug.SafeDebugResultsToFile('TakePhoto');
  {$ENDIF}

  hr := MfCreateMediaType(pMediaType2);
  if FAILED(hr) then
    goto Done;

  // Configure the photo format
  // NOTE: When parameter pSubTypeGuid is wrong or not supported the result value in OnCaptureEvent.MF_CAPTURE_ENGINE_PHOTO_TAKEN will be WINCODEC_ERR_COMPONENTNOTFOUND
  //       Valid subformats are:
  //         MFImageFormat_JPEG, GUID_ContainerFormatBmp, GUID_ContainerFormatJpeg etc.
  //         DON'T USE MFImageFormat_RGB32! (This will end with a WINCODEC_ERR_COMPONENTNOTFOUND)
  hr := CreatePhotoMediaType(GUID_ContainerFormatBmp,
                             pMediaType{2});
  if FAILED(hr) then
    goto Done;

  {$IFDEF DEBUG}
  FMediaTypeDebug.LogMediaType(pMediaType2);
  FMediaTypeDebug.SafeDebugResultsToFile('TakePhoto');
  {$ENDIF}

  hr := pCapturePhotoSink.RemoveAllStreams();
  if FAILED(hr) then
    goto Done;


  // Try to connect the first still image stream to the photo sink returns the streamindex for added stream.
  if bHasPhotoStream then
    hr := pCapturePhotoSink.AddStream(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_PHOTO,
                                      pMediaType,
                                      nil,
                                      {out} dwSinkStreamIndex);
  if FAILED(hr) then
    goto Done;

  hr := SetSnapShotOption(pCapturePhotoSink);
  if FAILED(hr) then
    goto Done;

  // E_NOTIMPL
  //hr := pCapturePhotoSink.GetService(dwSinkStreamIndex,
  //                                   GUID_NULL,
  //                                   IID_IMFSinkWriter,
  //                                   IUnknown(pSinkWriter));
  //if FAILED(hr) then
  //  goto Done;


  hr := FCaptureEngine.TakePhoto();
  if FAILED(hr) then
    goto Done;

  IsPhotoPending := True;

Done:
  SafeRelease(pCapturePhotoSink);
  SafeRelease(pCaptureSink);
  if FAILED(hr) then
    ErrMsg('TakePhoto',
            hr);
  Result := hr;
end;

//
function TCaptureManager.UpdateVideo(psrc: PMFVideoNormalizedRect): HResult;
var
  rdest: TRect;

begin
  CopyPVNRectToTRect(psrc, rdest);
  if Assigned(FCapturePreviewSink) then
    Result := FCapturePreviewSink.UpdateVideo(nil,
                                              rdest,
                                              nil)
  else
    Result := S_OK;
end;


//
function TCaptureManager.SetVideoFormat(): HResult;
var
  hr: HResult;
  uiDeviceIndex: UINT32;
  uiFormatIndex: UINT32;

begin
  uiDeviceIndex := FDeviceExplorer.DeviceIndex;
  uiFormatIndex := FDeviceExplorer.FormatIndex;
  hr := SetMediaType(FDeviceExplorer.DeviceProperties[uiDeviceIndex].aVideoFormats[uiFormatIndex].mfMediaType);
  Result := hr;
end;


// SetMediaType
function TCaptureManager.SetMediaType(pMediaType: IMFMediaType): HResult;
var
  mfCaptureSource: IMFCAptureSource;
  hr: HResult;

begin

  {$IFDEF DEBUG}
  FMediaTypeDebug.LogMediaType(pMediaType);
  FMediaTypeDebug.SafeDebugResultsToFile('SetMediaType');
  {$ENDIF}

  hr := FCaptureEngine.GetSource(mfCaptureSource);
  if SUCCEEDED(hr) then
    hr := mfCaptureSource.SetCurrentDeviceMediaType(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_PREVIEW, //MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                                    pMediaType);
  Result := hr;

end;


// GetCurrentFormat
function TCaptureManager.GetCurrentFormat(): TVideoFormatInfo;
begin
  Result := FDeviceExplorer.VideoFormat;
end;


// SetSnapShotOption
function TCaptureManager.SetSnapShotOption(pPhotoSink: IMFCapturePhotoSink): HResult;
var
  hr: HResult;
  pszFileName,
  pszPicPath,
  pszOutputFileName: PWideChar;

const
  pExt = '.bmp';

label
  Done;

begin
  hr := S_OK;
  // Decide where the snapshot should be send through
  case SnapShotOption of
    ssoFile:     begin
                   // ToDo: implement save to file (eg: My Photo's)
                   // to file

                   // get the My Pictures Folder path use fPath.GetSharedPicturesPath for the shared folder
                   pszPicPath := StrToPWideChar(TPath.GetPicturesPath);
                   pszFileName := StrToPWideChar(Format('MyPicture_%s%s',
                                                        [DateToStr(Now()),
                                                         pExt]));

                   pszOutputFileName := StrToPWideChar(Format('%s%s',
                                                              [pszPicPath,
                                                               pszFileName]));

                   hr := pPhotoSink.SetOutputFileName(pszOutputFileName);
                   if FAILED(hr) then
                     goto Done;

                 end;

    ssoCallBack: begin

                   // ToDo: implement save to ISample > preview window
                   // to callback

                   // Note:
                   //   Calling this method overrides any previous call to IMFCapturePhotoSink.SetOutputByteStream or
                   //   IMFCapturePhotoSink.SetOutputFileName.
                   // When called the event will be handled by OnSampleCallBack
                   hr := pPhotoSink.SetSampleCallback(FCaptureEngineOnSampleCallback);
                   if FAILED(hr) then
                     goto Done;
                   // Give MF time to assemble the pipeline.
                   // When succeeded, call IMFPhotoSink.GetService, to configure individual components.
                   hr :=  pPhotoSink.Prepare();
                   if FAILED(hr) then
                     goto Done;


                 end;

    ssoStream:   begin
                   // Save to stream
                   // NOT IMPLEMENTED
                 end;
  end;

Done:
  if FAILED(hr) then
    ErrMsg('OnPhotoTaken',
           hr);
  Result := hr;
end;

end.


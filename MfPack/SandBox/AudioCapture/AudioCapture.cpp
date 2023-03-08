/*
AudioCapture.cpp
Written by Matthew Fisher

AudioCapture class used by VideoCompressor to capture the current audio stream.
*/

//
// This code is modified from the WASAPICaptureSharedTimerDriven sample in the Windows SDK.
//

#ifdef USE_WMF
#include <MMDeviceAPI.h>
#include <AudioClient.h>
#include <AudioPolicy.h>
#include <avrt.h>
#include <functiondiscoverykeys.h>

const bool DisableMMCSS = false;

String GetDeviceName(IMMDeviceCollection *DeviceCollection, UINT DeviceIndex);
bool PickDevice(IMMDevice **DeviceToUse, bool *IsDefaultDevice, ERole *DefaultDeviceRole);

template <class T> void SafeRelease(T **ppT)
{
    if (*ppT)
    {
        (*ppT)->Release();
        *ppT = NULL;
    }
}

//
//  WASAPI Capture class
//
class CWASAPICapture : public IAudioSessionEvents, IMMNotificationClient
{
public:
    CWASAPICapture(IMMDevice *Endpoint, VideoCompressor *Compressor);
    bool Initialize(UINT32 EngineLatency);
    void Shutdown();
    bool Start(BYTE *CaptureBuffer, size_t BufferSize);
    void Stop();
    WORD ChannelCount() { return _MixFormat->Format.nChannels; }
    UINT32 SamplesPerSecond() { return _MixFormat->Format.nSamplesPerSec; }
    UINT32 BytesPerSample() { return _MixFormat->Format.wBitsPerSample / 8; }
    size_t FrameSize() { return _FrameSize; }
    WAVEFORMATEX *MixFormat() { return &(_MixFormat->Format); }
    size_t BytesCaptured() { return _CurrentCaptureIndex; }
    STDMETHOD_(ULONG, AddRef)();
    STDMETHOD_(ULONG, Release)();

private:
    ~CWASAPICapture(void);
    LONG                 _RefCount;
    
    //
    //  Core Audio Capture member variables.
    //
    IMMDevice*            _Endpoint;
    IAudioClient*         _AudioClient;
    IAudioCaptureClient*  _CaptureClient;

    HANDLE                _CaptureThread;
    HANDLE                _ShutdownEvent;
    WAVEFORMATEXTENSIBLE* _MixFormat;
    size_t                _FrameSize;
    UINT32                _BufferSize;

    //
    //  Capture buffer management.
    //
    BYTE*  _CaptureBuffer;
    size_t _CaptureBufferSize;
    size_t _CurrentCaptureIndex;
    
    VideoCompressor* _Compressor;

    static DWORD __stdcall WASAPICaptureThread(LPVOID Context);
    DWORD CWASAPICapture::DoCaptureThread();
    
    //
    //  Stream switch related members and methods.
    //
    bool                    _EnableStreamSwitch;
    HANDLE                  _StreamSwitchEvent;          // Set when the current session is disconnected.
    HANDLE                  _StreamSwitchCompleteEvent;  // Set when the default device changed.
    IAudioSessionControl*   _AudioSessionControl;
    IMMDeviceEnumerator*    _DeviceEnumerator;
    LONG                    _EngineLatencyInMS;
    bool                    _InStreamSwitch;

    STDMETHOD(OnDisplayNameChanged) (LPCWSTR /*NewDisplayName*/, LPCGUID /*EventContext*/) { return S_OK; };
    STDMETHOD(OnIconPathChanged) (LPCWSTR /*NewIconPath*/, LPCGUID /*EventContext*/) { return S_OK; };
    STDMETHOD(OnSimpleVolumeChanged) (float /*NewSimpleVolume*/, BOOL /*NewMute*/, LPCGUID /*EventContext*/) { return S_OK; }
    STDMETHOD(OnChannelVolumeChanged) (DWORD /*ChannelCount*/, float /*NewChannelVolumes*/[], DWORD /*ChangedChannel*/, LPCGUID /*EventContext*/) { return S_OK; };
    STDMETHOD(OnGroupingParamChanged) (LPCGUID /*NewGroupingParam*/, LPCGUID /*EventContext*/) {return S_OK; };
    STDMETHOD(OnStateChanged) (AudioSessionState /*NewState*/) { return S_OK; };
    STDMETHOD(OnSessionDisconnected) (AudioSessionDisconnectReason DisconnectReason);
    STDMETHOD(OnDeviceStateChanged) (LPCWSTR /*DeviceId*/, DWORD /*NewState*/) { return S_OK; }
    STDMETHOD(OnDeviceAdded) (LPCWSTR /*DeviceId*/) { return S_OK; };
    STDMETHOD(OnDeviceRemoved) (LPCWSTR /*DeviceId(*/) { return S_OK; };
    STDMETHOD(OnDefaultDeviceChanged) (EDataFlow Flow, ERole Role, LPCWSTR NewDefaultDeviceId);
    STDMETHOD(OnPropertyValueChanged) (LPCWSTR /*DeviceId*/, const PROPERTYKEY /*Key*/){return S_OK; };
    
    //
    //  IUnknown
    //
    STDMETHOD(QueryInterface)(REFIID iid, void **pvObject);

    //
    //  Utility functions.
    //
    bool InitializeAudioEngine();
    bool LoadFormat();
};

//
//  A simple WASAPI Capture client.
//

CWASAPICapture::CWASAPICapture(IMMDevice *Endpoint, VideoCompressor *Compressor) : 
    _RefCount(1),
    _Endpoint(Endpoint),
    _AudioClient(NULL),
    _CaptureClient(NULL),
    _CaptureThread(NULL),
    _ShutdownEvent(NULL),
    _MixFormat(NULL),
    _CurrentCaptureIndex(0),
    _StreamSwitchEvent(NULL),
    _StreamSwitchCompleteEvent(NULL),
    _AudioSessionControl(NULL),
    _DeviceEnumerator(NULL),
    _Compressor(Compressor),
    _InStreamSwitch(false)
{
    _Endpoint->AddRef();    // Since we're holding a copy of the endpoint, take a reference to it.  It'll be released in Shutdown();
}

//
//  Empty destructor - everything should be released in the Shutdown() call.
//
CWASAPICapture::~CWASAPICapture(void) 
{
}


//
//  Initialize WASAPI in event driven mode, associate the audio client with our samples ready event handle, retrieve 
//  a capture client for the transport, create the capture thread and start the audio engine.
//
bool CWASAPICapture::InitializeAudioEngine()
{
    HRESULT hr = _AudioClient->Initialize(AUDCLNT_SHAREMODE_SHARED, AUDCLNT_STREAMFLAGS_NOPERSIST, _EngineLatencyInMS*10000, 0, MixFormat(), NULL);
    PersistentAssert(SUCCEEDED(hr), "_AudioClient->Initialize failed");
    
    //
    //  Retrieve the buffer size for the audio client.
    //
    hr = _AudioClient->GetBufferSize(&_BufferSize);
    PersistentAssert(SUCCEEDED(hr), "_AudioClient->GetBufferSize failed");

    hr = _AudioClient->GetService(IID_PPV_ARGS(&_CaptureClient));
    PersistentAssert(SUCCEEDED(hr), "_AudioClient->GetService failed");

    return true;
}

//
//  Retrieve the format we'll use to capture samples.
//
//  We use the Mix format since we're capturing in shared mode.
//
bool CWASAPICapture::LoadFormat()
{
    HRESULT hr = _AudioClient->GetMixFormat((WAVEFORMATEX**)&_MixFormat);
    PersistentAssert(SUCCEEDED(hr), "_AudioClient->GetMixFormat failed");
    //-     SubFormat   {00000003-0000-0010-8000-00AA00389B71}  _GUID, KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
    _FrameSize = (_MixFormat->Format.wBitsPerSample / 8) * _MixFormat->Format.nChannels;
    return true;
}

//
//  Initialize the capturer.
//
bool CWASAPICapture::Initialize(UINT32 EngineLatency)
{
    //
    //  Create our shutdown event - we want auto reset events that start in the not-signaled state.
    //
    _ShutdownEvent = CreateEventEx(NULL, NULL, 0, EVENT_MODIFY_STATE | SYNCHRONIZE);
    PersistentAssert(_ShutdownEvent != NULL, "CreateEventEx failed");
    
    //
    //  Create our stream switch event- we want auto reset events that start in the not-signaled state.
    //  Note that we create this event even if we're not going to stream switch - that's because the event is used
    //  in the main loop of the capturer and thus it has to be set.
    //
    _StreamSwitchEvent = CreateEventEx(NULL, NULL, 0, EVENT_MODIFY_STATE | SYNCHRONIZE);
    PersistentAssert(_StreamSwitchEvent != NULL, "CreateEventEx failed");
    
    //
    //  Now activate an IAudioClient object on our preferred endpoint and retrieve the mix format for that endpoint.
    //
    HRESULT hr = _Endpoint->Activate(__uuidof(IAudioClient), CLSCTX_INPROC_SERVER, NULL, reinterpret_cast<void **>(&_AudioClient));
    PersistentAssert(SUCCEEDED(hr), "_Endpoint->Activate failed");

    hr = CoCreateInstance(__uuidof(MMDeviceEnumerator), NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&_DeviceEnumerator));
    PersistentAssert(SUCCEEDED(hr), "CoCreateInstance failed");

    //
    // Load the MixFormat.  This may differ depending on the shared mode used
    //
    LoadFormat();
    
    //
    //  Remember our configured latency in case we'll need it for a stream switch later.
    //
    _EngineLatencyInMS = EngineLatency;

    InitializeAudioEngine();
    return true;
}

//
//  Shut down the capture code and free all the resources.
//
void CWASAPICapture::Shutdown()
{
    if (_CaptureThread)
    {
        SetEvent(_ShutdownEvent);
        WaitForSingleObject(_CaptureThread, INFINITE);
        CloseHandle(_CaptureThread);
        _CaptureThread = NULL;
    }

    if (_ShutdownEvent)
    {
        CloseHandle(_ShutdownEvent);
        _ShutdownEvent = NULL;
    }

    if (_StreamSwitchEvent)
    {
        CloseHandle(_StreamSwitchEvent);
        _StreamSwitchEvent = NULL;
    }

    SafeRelease(&_Endpoint);
    SafeRelease(&_AudioClient);
    SafeRelease(&_CaptureClient);

    if (_MixFormat)
    {
        CoTaskMemFree(_MixFormat);
        _MixFormat = NULL;
    }
}


//
//  Start capturing...
//
bool CWASAPICapture::Start(BYTE *CaptureBuffer, size_t CaptureBufferSize)
{
    HRESULT hr;

    _CaptureBuffer = CaptureBuffer;
    _CaptureBufferSize = CaptureBufferSize;

    //
    //  Now create the thread which is going to drive the capture.
    //
    _CaptureThread = CreateThread(NULL, 0, WASAPICaptureThread, this, 0, NULL);
    PersistentAssert(_CaptureThread != NULL, "CreateThread failed");
    
    //
    //  We're ready to go, start capturing!
    //
    hr = _AudioClient->Start();
    PersistentAssert(SUCCEEDED(hr), "_AudioClient->Start failed");
    
    return true;
}

//
//  Stop the capturer.
//
void CWASAPICapture::Stop()
{
    HRESULT hr;

    //
    //  Tell the capture thread to shut down, wait for the thread to complete then clean up all the stuff we 
    //  allocated in Start().
    //
    if (_ShutdownEvent)
    {
        SetEvent(_ShutdownEvent);
    }

    hr = _AudioClient->Stop();
    PersistentAssert(SUCCEEDED(hr), "_AudioClient->Stop failed");
    
    if (_CaptureThread)
    {
        WaitForSingleObject(_CaptureThread, INFINITE);

        CloseHandle(_CaptureThread);
        _CaptureThread = NULL;
    }
}


//
//  Capture thread - processes samples from the audio engine
//
DWORD CWASAPICapture::WASAPICaptureThread(LPVOID Context)
{
    CWASAPICapture *capturer = static_cast<CWASAPICapture *>(Context);
    return capturer->DoCaptureThread();
}

DWORD CWASAPICapture::DoCaptureThread()
{
    bool stillPlaying = true;
    HANDLE waitArray[2] = {_ShutdownEvent, _StreamSwitchEvent};
    HANDLE mmcssHandle = NULL;
    DWORD mmcssTaskIndex = 0;

    HRESULT hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
    PersistentAssert(SUCCEEDED(hr), "CoInitializeEx failed");
    
    if (!DisableMMCSS)
    {
        mmcssHandle = AvSetMmThreadCharacteristics("Audio", &mmcssTaskIndex);
        PersistentAssert(mmcssHandle != NULL, "AvSetMmThreadCharacteristics failed");
    }
    while (stillPlaying)
    {
        HRESULT hr;
        //
        //  In Timer Driven mode, we want to wait for half the desired latency in milliseconds.
        //
        //  That way we'll wake up half way through the processing period to pull the 
        //  next set of samples from the engine.
        //
        DWORD waitResult = WaitForMultipleObjects(2, waitArray, FALSE, _EngineLatencyInMS / 2);
        switch (waitResult)
        {
        case WAIT_OBJECT_0 + 0:     // _ShutdownEvent
            stillPlaying = false;       // We're done, exit the loop.
            break;
        case WAIT_OBJECT_0 + 1:     // _StreamSwitchEvent
            PersistentSignalError("StreamSwitch event unexpected");
            stillPlaying = false;
            break;
        case WAIT_TIMEOUT:          // Timeout
            //
            //  We need to retrieve the next buffer of samples from the audio capturer.
            //
            BYTE *pData;
            UINT32 framesAvailable;
            DWORD  flags;

            //
            //  Find out how much capture data is available.  We need to make sure we don't run over the length
            //  of our capture buffer.  We'll discard any samples that don't fit in the buffer.
            //
            UINT64 CaptureStartTime;
            hr = _CaptureClient->GetBuffer(&pData, &framesAvailable, &flags, NULL, &CaptureStartTime);
            if (SUCCEEDED(hr))
            {
                UINT32 framesToCopy = min(framesAvailable, static_cast<UINT32>((_CaptureBufferSize - _CurrentCaptureIndex) / _FrameSize));
                const UINT BytesToCopy = framesToCopy * _FrameSize;
                if (framesToCopy != 0)
                {
                    //
                    //  The flags on capture tell us information about the data.
                    //
                    //  We only really care about the silent flag since we want to put frames of silence into the buffer
                    //  when we receive silence.  We rely on the fact that a logical bit 0 is silence for both float and int formats.
                    //
                    if (flags & AUDCLNT_BUFFERFLAGS_SILENT)
                    {
                        //
                        //  Fill 0s from the capture buffer to the output buffer.
                        //
                        ZeroMemory(&_CaptureBuffer[_CurrentCaptureIndex], BytesToCopy);
                    }
                    else
                    {
                        //
                        //  Copy data from the audio engine buffer to the output buffer.
                        //
                        CopyMemory(&_CaptureBuffer[_CurrentCaptureIndex], pData, BytesToCopy);
                    }
                    //
                    //  Bump the capture buffer pointer.
                    //
                    if(_Compressor == NULL)
                    {
                        _CurrentCaptureIndex += BytesToCopy;
                    }
                }

                hr = _CaptureClient->ReleaseBuffer(framesAvailable);
                PersistentAssert(SUCCEEDED(hr), "_CaptureClient->ReleaseBuffer failed");
                if(_Compressor && framesToCopy != 0)
                {
                    _Compressor->AudioSample32Bit2Channel((float *)_CaptureBuffer, framesToCopy, CaptureStartTime);
                }
            }
            break;
        }
    }
    if (!DisableMMCSS)
    {
        AvRevertMmThreadCharacteristics(mmcssHandle);
    }

    CoUninitialize();
    return 0;
}

//
//  Called when an audio session is disconnected.  
//
//  When a session is disconnected because of a device removal or format change event, we just want 
//  to let the capture thread know that the session's gone away
//
HRESULT CWASAPICapture::OnSessionDisconnected(AudioSessionDisconnectReason DisconnectReason)
{
    if (DisconnectReason == DisconnectReasonDeviceRemoval)
    {
        //
        //  The stream was disconnected because the device we're capturing to was removed.
        //
        //  We want to reset the stream switch complete event (so we'll block when the HandleStreamSwitchEvent function
        //  waits until the default device changed event occurs).
        //
        //  Note that we don't set the _StreamSwitchCompleteEvent - that will be set when the OnDefaultDeviceChanged event occurs.
        //
        _InStreamSwitch = true;
        SetEvent(_StreamSwitchEvent);
    }
    if (DisconnectReason == DisconnectReasonFormatChanged)
    {
        //
        //  The stream was disconnected because the format changed on our capture device.
        //
        //  We want to flag that we're in a stream switch and then set the stream switch event (which breaks out of the capturer).  We also
        //  want to set the _StreamSwitchCompleteEvent because we're not going to see a default device changed event after this.
        //
        _InStreamSwitch = true;
        SetEvent(_StreamSwitchEvent);
        SetEvent(_StreamSwitchCompleteEvent);
    }
    return S_OK;
}
//
//  Called when the default capture device changed.  We just want to set an event which lets the stream switch logic know that it's ok to 
//  continue with the stream switch.
//
HRESULT CWASAPICapture::OnDefaultDeviceChanged(EDataFlow Flow, ERole Role, LPCWSTR /*NewDefaultDeviceId*/)
{
    if (Flow == eCapture)
    {
        //
        //  The default capture device for our configuredf role was changed.  
        //
        //  If we're not in a stream switch already, we want to initiate a stream switch event.  
        //  We also we want to set the stream switch complete event.  That will signal the capture thread that it's ok to re-initialize the
        //  audio capturer.
        //
        if (!_InStreamSwitch)
        {
            _InStreamSwitch = true;
            SetEvent(_StreamSwitchEvent);
        }
        SetEvent(_StreamSwitchCompleteEvent);
    }
    return S_OK;
}

//
//  IUnknown
//
HRESULT CWASAPICapture::QueryInterface(REFIID Iid, void **Object)
{
    if (Object == NULL)
    {
        return E_POINTER;
    }
    *Object = NULL;

    if (Iid == IID_IUnknown)
    {
        *Object = static_cast<IUnknown *>(static_cast<IAudioSessionEvents *>(this));
        AddRef();
    }
    else if (Iid == __uuidof(IMMNotificationClient))
    {
        *Object = static_cast<IMMNotificationClient *>(this);
        AddRef();
    }
    else if (Iid == __uuidof(IAudioSessionEvents))
    {
        *Object = static_cast<IAudioSessionEvents *>(this);
        AddRef();
    }
    else
    {
        return E_NOINTERFACE;
    }
    return S_OK;
}
ULONG CWASAPICapture::AddRef()
{
    return InterlockedIncrement(&_RefCount);
}
ULONG CWASAPICapture::Release()
{
    ULONG returnValue = InterlockedDecrement(&_RefCount);
    if (returnValue == 0)
    {
        delete this;
    }
    return returnValue;
}

//
//  Retrieves the device friendly name for a particular device in a device collection.  
//
//  The returned string was allocated using malloc() so it should be freed using free();
//
String GetDeviceName(IMMDeviceCollection *DeviceCollection, UINT DeviceIndex)
{
    IMMDevice *device;
    LPWSTR deviceId;
    HRESULT hr;

    hr = DeviceCollection->Item(DeviceIndex, &device);
    PersistentAssert(SUCCEEDED(hr), "DeviceCollection->Item failed");
    
    hr = device->GetId(&deviceId);
    PersistentAssert(SUCCEEDED(hr), "device->GetId failed");

    IPropertyStore *propertyStore;
    hr = device->OpenPropertyStore(STGM_READ, &propertyStore);
    SafeRelease(&device);
    PersistentAssert(SUCCEEDED(hr), "device->OpenPropertyStore failed");

    PROPVARIANT friendlyName;
    PropVariantInit(&friendlyName);
    hr = propertyStore->GetValue(PKEY_Device_FriendlyName, &friendlyName);
    SafeRelease(&propertyStore);
    PersistentAssert(SUCCEEDED(hr), "propertyStore->GetValue failed");

    String Result = String(UnicodeString(friendlyName.pwszVal)); // + String(" (") + String( UnicodeString(deviceId) ) + String(")")
    
    PropVariantClear(&friendlyName);
    CoTaskMemFree(deviceId);

    return Result;
}

//
//  Based on the input switches, pick the specified device to use.
//
bool PickDevice(IMMDevice **DeviceToUse, UINT AudioDeviceIndex)
{
    IMMDeviceEnumerator *deviceEnumerator = NULL;
    IMMDeviceCollection *deviceCollection = NULL;

    HRESULT hr = CoCreateInstance(__uuidof(MMDeviceEnumerator), NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&deviceEnumerator));
    PersistentAssert(SUCCEEDED(hr), "CoCreateInstance failed");
    
    IMMDevice *device = NULL;

    //
    //  The user didn't specify an output device, prompt the user for a device and use that.
    //
    hr = deviceEnumerator->EnumAudioEndpoints(eCapture, DEVICE_STATE_ACTIVE, &deviceCollection);
    PersistentAssert(SUCCEEDED(hr), "deviceEnumerator->EnumAudioEndpoints failed");
    
    UINT deviceCount;
    hr = deviceCollection->GetCount(&deviceCount);
    PersistentAssert(SUCCEEDED(hr), "deviceCollection->GetCount failed");
    for (UINT DeviceIndex = 0 ; DeviceIndex < deviceCount; DeviceIndex++)
    {
        String deviceName = GetDeviceName(deviceCollection, DeviceIndex);
        //Console::WriteLine(String(DeviceIndex) + String(": ") + deviceName);
    }
    
    int deviceIndex = 0;
    if(AudioDeviceIndex < deviceCount)
    {
        deviceIndex = AudioDeviceIndex;
    }
    hr = deviceCollection->Item(deviceIndex, DeviceToUse);
    PersistentAssert(DeviceToUse != NULL && SUCCEEDED(hr), "deviceCollection->Item failed");
    
    SafeRelease(&deviceCollection);
    SafeRelease(&deviceEnumerator);
    return true;
}

struct WAVEHEADER
{
    DWORD   dwRiff;                     // "RIFF"
    DWORD   dwSize;                     // Size
    DWORD   dwWave;                     // "WAVE"
    DWORD   dwFmt;                      // "fmt "
    DWORD   dwFmtSize;                  // Wave Format Size
};

//  Static RIFF header, we'll append the format to it.
const BYTE WaveHeader[] = 
{
    'R',   'I',   'F',   'F',  0x00,  0x00,  0x00,  0x00, 'W',   'A',   'V',   'E',   'f',   'm',   't',   ' ', 0x00, 0x00, 0x00, 0x00
};

//  Static wave DATA tag.
const BYTE WaveData[] = { 'd', 'a', 't', 'a'};

//
//  Write the contents of a WAV file.  We take as input the data to write and the format of that data.
//
bool WriteWaveFile(HANDLE FileHandle, const BYTE *Buffer, const size_t BufferSize, const WAVEFORMATEX *WaveFormat)
{
    DWORD waveFileSize = sizeof(WAVEHEADER) + sizeof(WAVEFORMATEX) + WaveFormat->cbSize + sizeof(WaveData) + sizeof(DWORD) + static_cast<DWORD>(BufferSize);
    BYTE *waveFileData = new (std::nothrow) BYTE[waveFileSize];
    BYTE *waveFilePointer = waveFileData;
    WAVEHEADER *waveHeader = reinterpret_cast<WAVEHEADER *>(waveFileData);

    if (waveFileData == NULL)
    {
        printf("Unable to allocate %d bytes to hold output wave data\n", waveFileSize);
        return false;
    }

    //
    //  Copy in the wave header - we'll fix up the lengths later.
    //
    CopyMemory(waveFilePointer, WaveHeader, sizeof(WaveHeader));
    waveFilePointer += sizeof(WaveHeader);

    //
    //  Update the sizes in the header.
    //
    waveHeader->dwSize = waveFileSize - (2 * sizeof(DWORD));
    waveHeader->dwFmtSize = sizeof(WAVEFORMATEX) + WaveFormat->cbSize;

    //
    //  Next copy in the WaveFormatex structure.
    //
    CopyMemory(waveFilePointer, WaveFormat, sizeof(WAVEFORMATEX) + WaveFormat->cbSize);
    waveFilePointer += sizeof(WAVEFORMATEX) + WaveFormat->cbSize;


    //
    //  Then the data header.
    //
    CopyMemory(waveFilePointer, WaveData, sizeof(WaveData));
    waveFilePointer += sizeof(WaveData);
    *(reinterpret_cast<DWORD *>(waveFilePointer)) = static_cast<DWORD>(BufferSize);
    waveFilePointer += sizeof(DWORD);

    //
    //  And finally copy in the audio data.
    //
    CopyMemory(waveFilePointer, Buffer, BufferSize);

    //
    //  Last but not least, write the data to the file.
    //
    DWORD bytesWritten;
    if (!WriteFile(FileHandle, waveFileData, waveFileSize, &bytesWritten, NULL))
    {
        printf("Unable to write wave file: %d\n", GetLastError());
        delete []waveFileData;
        return false;
    }

    if (bytesWritten != waveFileSize)
    {
        printf("Failed to write entire wave file\n");
        delete []waveFileData;
        return false;
    }
    delete []waveFileData;
    return true;
}

//
//  Write the captured wave data to an output file so that it can be examined later.
//
void SaveWaveData(BYTE *CaptureBuffer, size_t BufferSize, const WAVEFORMATEX *WaveFormat, const String &Filename)
{
    HANDLE waveHandle = CreateFile(Filename.CString(), GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, 
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, 
        NULL);
    if (waveHandle != INVALID_HANDLE_VALUE)
    {
        bool Success = WriteWaveFile(waveHandle, CaptureBuffer, BufferSize, WaveFormat);
        PersistentAssert(Success, "WriteWaveFile failed");
        CloseHandle(waveHandle);
    }
}

bool AudioCapture::StartCapture(VideoCompressor *Compressor, UINT AudioDeviceIndex)
{
    _Compressor = Compressor;
    _Filename.FreeMemory();
    _CaptureBuffer.FreeMemory();
    return StartCaptureInternal(AudioDeviceIndex);
}

bool AudioCapture::StartCapture(const String &Filename, UINT AudioDeviceIndex)
{
    _Compressor = NULL;
    _Filename = Filename;
    _CaptureBuffer.FreeMemory();
    return StartCaptureInternal(AudioDeviceIndex);
}

void AudioCapture::StopCapture()
{
    _Capturer->Stop();

    if(_Compressor == NULL && _Filename.Length() > 0 && _CaptureBuffer.Length() > 0)
    {
        Console::WriteLine(String("Saving WAV file: ") + _Filename);
        //
        //  We've now captured our wave data.  Now write it out in a wave file.
        //
        SaveWaveData(_CaptureBuffer.CArray(), _Capturer->BytesCaptured(), _Capturer->MixFormat(), _Filename);
    }

    //
    //  Now shut down the capturer and release it we're done.
    //
    _Capturer->Shutdown();
    SafeRelease(&_Capturer);
    
    SafeRelease(&_CaptureDevice);
    //CoUninitialize();
}

bool AudioCapture::StartCaptureInternal(UINT AudioDeviceIndex)
{
    PersistentAssert(_CaptureDevice == NULL && _Capturer == NULL, "StartCapture called without StopCapture");

    const int TargetLatency = 20;
    int TargetDurationInSec = 10;

    //
    //  A GUI application should use COINIT_APARTMENTTHREADED instead of COINIT_MULTITHREADED.
    //
    HRESULT hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
    //PersistentAssert(SUCCEEDED(hr), "CoInitializeEx failed");
    
    //
    //  Now that we've parsed our command line, pick the device to capture.
    //
    bool Success = PickDevice(&_CaptureDevice, AudioDeviceIndex);
    PersistentAssert(Success, "PickDevice failed");
    
    //
    //  Instantiate a capturer and capture sounds for TargetDuration seconds
    //
    //  Configure the capturer to enable stream switching on the specified role if the user specified one of the default devices.
    //
    _Capturer = new (std::nothrow) CWASAPICapture(_CaptureDevice, _Compressor);
    PersistentAssert(_Capturer != NULL, "Allocate CWASAPICapture failed");
        
    if (_Capturer->Initialize(TargetLatency))
    {
        //
        //  We've initialized the capturer.  Once we've done that, we know some information about the
        //  mix format and we can allocate the buffer that we're going to capture.
        //
        //
        //  The buffer is going to contain "TargetDuration" seconds worth of PCM data.  That means 
        //  we're going to have TargetDuration*samples/second frames multiplied by the frame size.
        //
        size_t captureBufferSize = _Capturer->SamplesPerSecond() * TargetDurationInSec * _Capturer->FrameSize();
        _CaptureBuffer.Allocate(captureBufferSize);
        bool Success = _Capturer->Start(_CaptureBuffer.CArray(), captureBufferSize);
        PersistentAssert(Success, "_Capturer->Start failed");
    }

    return true;
}
#endif
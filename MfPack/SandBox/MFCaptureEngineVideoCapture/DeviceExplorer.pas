unit DeviceExplorer;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.Classes,
  System.Win.ComObj,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  {Vcl}
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfCaptureEngine,
  WinApi.MediaFoundationApi.MfMetLib,
  WinAPI.MediaFoundationApi.MfUtils,
  {$IFDEF DEBUG}
  WinApi.MediaFoundationApi.MfMediaTypeDebug,
  {$ENDIF}
  {Application}
  Utils;

type

  // ChooseDeviceParam class
  //
  // Holds an array that represent video
  // capture device properties and capabilities
  //

  TDeviceExplorer = class(TObject)
  private

    FDeviceProperties: TDevicePropertiesArray;  // Holds an array that represent video
                                                // capture device properties and capabilities
                                                // Extended array of TDeviceProperties.

    FVideoFormatInfo: TVideoFormatInfo;         // Current video format.
    iDevices: Integer;              // Number of elements in the array.
    iSelection: Integer;            // Selected device, by array index.
    iVideoFormatIndex: Integer ;     // Selected video format.
    lpSelectedDeviceName: LPWSTR;   // Selected device name.
    lpSelectedSymbolicLink: LPWSTR; // Selected device symbolic link.
    gSelectedDeviceGuid: TGuid;     // Guid of the selected device.
    dwSupportedFormats: DWord;
    dwNativeFormats: DWord;
    bIsSelected: Boolean;           // Did user select a device?

    //FMediaSource: IMFMediaSource;
    FMediaType: IMFMediaType;      // Selected mediatype.
    FActivate: IMFActivate;        // Selected Activation interface

    FCritSec: TMFCritSec;

    {$IFDEF DEBUG}
      FMediaTypeDebug: TMediaTypeDebug;
    {$ENDIF}

    function Initialize(out pCount: UINT32): HResult;
    function GetCaptureDevices(out pCount: UINT32): HRESULT;
    // Clear all.
    procedure ResetCaptureDeviceCaps();

  public

    constructor Create(out pResult: HResult);  //reintroduce;
    destructor Destroy(); override;

    /// Steps to manage this class
    ///  1 - Call Create, this initializes the class.
    ///  2 - The application should pick te device and videoformat.
    ///  3 - Call ActivateDevice. Activates the selected device and selected video format.
    ///
    ///  On a device lost, call GetCaptureDeviceCaps first and the application should select another device.
    ///  after this call ActivateDevice again.


    // Use this function to get and set all devices and params.
    function SetCurrentDeviceProperties(pDeviceIndex: Integer;   // = zero based
                                        pFormatsIndex: Integer;  // = zero based
                                        pSetFormat: Boolean = False): HRESULT;

    // Gets the activate interfaces from all capturedevices found on this system.
    function GetActivationObjects(out ppActivate: PIMFActivate;
                                  out pCount: Integer): HResult;

    property DevicesCount: Integer read iDevices;
    property DeviceIndex: Integer read iSelection;
    property FormatIndex: Integer read iVideoFormatIndex;
    property DeviceProperties: TDevicePropertiesArray read FDeviceProperties;
    property SupportedFormats: DWord read dwSupportedFormats;
    property NativeFormats: DWord read dwNativeFormats;
    property DeviceDisplayName: PWideChar read lpSelectedDeviceName;
    property DeviceGuid: TGuid read gSelectedDeviceGuid;
    property DeviceIsSelected: Boolean read bIsSelected;
    property DeviceSymbolicLink: PWideChar read lpSelectedSymbolicLink;
    property VideoFormat: TVideoFormatInfo read FVideoFormatInfo;
    property MediaType: IMFMediaType read FMediaType;
    property DeviceActivationObject: IMFActivate read FActivate;
  end;

var
  FDeviceExplorer: TDeviceExplorer;


implementation


//
constructor TDeviceExplorer.Create(out pResult: HResult);
var
  uiCount: UINT32;
  hr: HResult;

begin
  inherited Create();
  FCritSec := TMFCritSec.Create;

  {$IFDEF DEBUG}
  FMediaTypeDebug := TMediaTypeDebug.Create();
  {$ENDIF}

  hr := Initialize(uiCount);

  if FAILED(hr) then
    ShowMessage(format('No capture devices found. (count = %d hr = %d)',
                       [uiCount, hr]));
  pResult := hr;
end;


//
function TDeviceExplorer.Initialize(out pCount: UINT32): HResult;
var
  uiC: UINT32;
  hr: HResult;

begin
  hr := GetCaptureDevices(uiC);
  if SUCCEEDED(hr) then
    begin
      if (uiC = 0) then
        begin
{$IFDEF DEBUG}
          OutputDebugString(StrToPWideChar(format('No devices found. (%d)',
                                           [uiC])));
{$ENDIF}
          hr := MF_E_NO_CAPTURE_DEVICES_AVAILABLE;
        end
      else  // Set the first device as default device.
        SetCurrentDeviceProperties(0,
                                   0,
                                   False);
    end;

  pCount := uiC;
  Result := hr;
end;


//
destructor TDeviceExplorer.Destroy();
begin
  // Clear selected device record and arrays
  ResetCaptureDeviceCaps();
  SafeDelete(FCritSec);
  {$IFDEF DEBUG}
  FMediaTypeDebug.Free();
  {$ENDIF}
  inherited Destroy();
end;


//
function TDeviceExplorer.GetCaptureDevices(out pCount: UINT32): HResult;
var
  hr: HResult;

begin

  // Clear existing content.
  ResetCaptureDeviceCaps();

  // Get the device properties from each video capture device
  hr := EnumCaptureDeviceSources(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID,
                                 FDeviceProperties);

  pCount := Length(FDeviceProperties);
  Result := hr;
end;


// Clears the device array
procedure TDeviceExplorer.ResetCaptureDeviceCaps();
var
  i: Integer;

begin
  FCritSec.Lock;

  for i := 0 to Length(FDeviceProperties) -1 do
    FDeviceProperties[i].Reset;
  FDeviceProperties := nil;

  // Clear class properties
  iDevices := -1;
  iSelection := -1;
  iVideoFormatIndex := -1;
  lpSelectedDeviceName := nil;
  lpSelectedSymbolicLink := nil;
  gSelectedDeviceGuid := GUID_NULL;
  bIsSelected := False;
  SafeRelease(FMediaType);
  SafeRelease(FActivate);
  //SafeRelease(FMediaSource);

  FCritSec.Unlock;
end;

//
function TDeviceExplorer.SetCurrentDeviceProperties(pDeviceIndex: Integer;
                                                    pFormatsIndex: Integer;
                                                    pSetFormat: Boolean = False): HResult;
var
  hr: HResult;
  pMediaSource: IMFMediaSource;
  mfActivate: PIMFActivate;
  iCount: Integer;

label
  Done;

begin

  FCritSec.Lock;

  if (pDeviceIndex < FDeviceProperties[pDeviceIndex].iCount) then
    begin

      iSelection := pDeviceIndex;
      bIsSelected := True;
      iVideoFormatIndex := pFormatsIndex;
      iDevices := FDeviceProperties[pDeviceIndex].iCount;
      lpSelectedDeviceName := FDeviceProperties[pDeviceIndex].lpDisplayName;
      gSelectedDeviceGuid := FDeviceProperties[pDeviceIndex].riId;
      lpSelectedSymbolicLink := FDeviceProperties[pDeviceIndex].lpSymbolicLink;
      dwNativeFormats := FDeviceProperties[pDeviceIndex].dwNativeFormats;
      dwSupportedFormats := FDeviceProperties[pDeviceIndex].dwSupportedFormats;

      {$IFDEF DEBUG}
      FMediaTypeDebug.LogMediaType(DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('TDeviceExplorer.SetCurrentDeviceProperties');
      {$ENDIF}

      CloneVideoMediaType(DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType,
                          GUID_NULL,
                          FMediaType);

      {$IFDEF DEBUG}
      FMediaTypeDebug.LogMediaType(FMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('TDeviceExplorer.SetCurrentDeviceProperties');
      {$ENDIF}


{$POINTERMATH ON}
      hr := GetActivationObjects(mfActivate,
                                 iCount);
      if (iCount > 0) and (pDeviceIndex > -1) then
        FActivate := mfActivate[pDeviceIndex];
{$POINTERMATH OFF}

      // Set the videoformat that has been selected by user.
      FVideoFormatInfo.Reset();
      FVideoFormatInfo := FDeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex];

      // Optionally set the device video format
      if pSetFormat then
        begin

          hr := mfActivate.DetachObject();

          if SUCCEEDED(hr) then
            hr := mfActivate.ActivateObject(IID_IMFMediaSource,
                                            Pointer(pMediaSource));

          if SUCCEEDED(hr) then
            begin
              HandleMessages(GetCurrentThread());
              hr := SetDeviceFormat(pMediaSource,
                                    DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType,
                                    pFormatsIndex);
            end;
          if FAILED(hr) then
            goto Done;
        end
      else
        hr := S_OK;
    end
  else
    hr := ERROR_INVALID_PARAMETER;

Done:
  //SafeRelease(FMediaSource);
  FCritSec.Unlock;
  Result := hr;
end;


function TDeviceExplorer.GetActivationObjects(out ppActivate: PIMFActivate;
                                              out pCount: Integer): HResult;
var
  mfAttributes: IMFAttributes;
  hr: HResult;

begin
  hr := MFCreateAttributes(mfAttributes,
                           1);

  if SUCCEEDED(hr) then
    // Ask for source type = video capture devices
    hr := mfAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                               MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  if SUCCEEDED(hr) then
    // Enumerate devices.
    hr := MFEnumDeviceSources(mfAttributes,
                              ppActivate,
                              pCount);
  Result := hr;
end;

end.

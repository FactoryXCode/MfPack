// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: DeviceExplorer.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 3.1.3
//
// Description:
//   This unit contains the DeviceExplorer class to discover video devices like webcams etc.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX313/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
//==============================================================================
//
// LICENSE
//
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product without implicit permission.
//
//==============================================================================
unit DeviceExplorer;

interface

  // Undefine this when not needed!
  {$DEFINE SAVE_DEBUG_REPORT}


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
  {$IFDEF SAVE_DEBUG_REPORT}
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
    iDevices: Integer;                          // Number of elements in the array.
    iSelection: Integer;                        // Selected device, by array index.
    iVideoFormatIndex: Integer ;                // Selected video format.
    lpSelectedDeviceName: LPWSTR;               // Selected device name.
    lpSelectedSymbolicLink: LPWSTR;             // Selected device symbolic link.
    gSelectedDeviceGuid: TGuid;                 // Guid of the selected device.
    dwSupportedFormats: DWord;                  // Number of supported input formats.
    dwNativeFormats: DWord;                     // Number of all native formats found on a device.
    bIsSelected: Boolean;                       // Did user select a device?

    FMediaType: IMFMediaType;                   // Selected mediatype.
    FActivate: IMFActivate;                     // Selected Activation interface

    {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug: TMediaTypeDebug;
    {$ENDIF}

    function Initialize(out pCount: UINT32): HResult;
    function GetCaptureDevices(out pCount: UINT32): HRESULT;
    // Clear all.
    procedure ResetCaptureDeviceCaps();

  public

    constructor Create(out pResult: HResult);
    destructor Destroy(); override;

    /// Steps to manage this class
    ///  1 - Call Create, this initializes the class.
    ///  2 - The application should pick te device and videoformat.
    ///  3 - Set DeviceParam properties (SetCurrentDeviceProperties). This will activate the selected device and selected video format.
    ///  4 - Call GetActivationObjects to get the activate object.
    ///  5 - Call FCaptureManager.InitializeCaptureManager to activate the capture manager.
    ///  6 - Call FCaptureManager.StartPreview to start previewing.
    ///  On a device lost, call GetCaptureDeviceCaps first and the application should select another device.
    ///  after this call ActivateDevice again.


    // Use this function to get and set all devices and params.
    function SetCurrentDeviceProperties(pDeviceIndex: Integer;   // = zero based
                                        pFormatsIndex: Integer;  // = zero based
                                        pSetFormat: Boolean = False): HRESULT;

    // Gets the activate interfaces from all capturedevices found on this system.
    function GetActivationObjects(out ppActivate: PIMFActivate;
                                  out pCount: UINT32): HResult;

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

  {$IFDEF SAVE_DEBUG_REPORT}
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
{$IFDEF SAVE_DEBUG_REPORT}
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
  {$IFDEF SAVE_DEBUG_REPORT}
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

end;

//
function TDeviceExplorer.SetCurrentDeviceProperties(pDeviceIndex: Integer;
                                                    pFormatsIndex: Integer;
                                                    pSetFormat: Boolean = False): HResult;
var
  hr: HResult;
  pMediaSource: IMFMediaSource;
  mfActivate: PIMFActivate;
  uiCount: UINT32;

label
  Done;

begin

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

      {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug.LogMediaType(DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('TDeviceExplorer.SetCurrentDeviceProperties');
      {$ENDIF}

      CloneVideoMediaType(DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType,
                          GUID_NULL,
                          FMediaType);

      {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug.LogMediaType(FMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('TDeviceExplorer.SetCurrentDeviceProperties');
      {$ENDIF}


{$POINTERMATH ON}
      hr := GetActivationObjects(mfActivate,
                                 uiCount);
      if FAILED(hr) then
        goto done;

      if (uiCount > 0) and (pDeviceIndex > -1) then
        FActivate := mfActivate[pDeviceIndex]
      else
        begin
          hr := ERROR_INVALID_PARAMETER;
          goto done;
        end;
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
  Result := hr;
end;


function TDeviceExplorer.GetActivationObjects(out ppActivate: PIMFActivate;
                                              out pCount: UINT32): HResult;
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

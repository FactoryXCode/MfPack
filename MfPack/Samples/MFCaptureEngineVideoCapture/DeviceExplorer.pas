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
// Revision Version: 3.1.6
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
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX316/Samples/MFCaptureEngineVideoCapture
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
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
    FActivation: IMFActivate;

    {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug: TMediaTypeDebug;
    {$ENDIF}

    function Initialize(out pCount: UINT32): HResult;
    function GetCaptureDevices(out pCount: UINT32): HRESULT;
    // Clear all.
    procedure ResetCaptureDeviceCaps();

    procedure SetMediaType(aMediaType: IMFMediaType);

  public

    constructor Create(out pResult: HResult); reintroduce;
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
                                        pFormatsIndex: Integer): HRESULT;   // = zero based


    // Gets the activate interfaces from all capturedevices found on this system.
    function GetActivationObjects(out ppActivate: PIMFActivate;
                                  out pCount: UINT32): HResult;

    // Current (selected) DeviceProperties
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
    property MediaType: IMFMediaType read FMediaType write SetMediaType;

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
  iDevices := uiCount;
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
                                   0);
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
  FMediaTypeDebug := nil;
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

  CoTaskMemFree(FDeviceProperties);
  FVideoFormatInfo.Reset();

  // Clear class properties
  iDevices := -1;
  iSelection := -1;
  iVideoFormatIndex := -1;
  lpSelectedDeviceName := nil;
  lpSelectedSymbolicLink := nil;
  gSelectedDeviceGuid := GUID_NULL;
  bIsSelected := False;
  SafeRelease(FMediaType);

end;

//
function TDeviceExplorer.SetCurrentDeviceProperties(pDeviceIndex: Integer;
                                                    pFormatsIndex: Integer): HResult;
var
  hr: HResult;

label
  Done;

begin

  if (pDeviceIndex < Length(FDeviceProperties)) then
    begin

      iSelection := pDeviceIndex;
      bIsSelected := True;
      iVideoFormatIndex := pFormatsIndex;
      iDevices := Length(FDeviceProperties);
      lpSelectedDeviceName := FDeviceProperties[pDeviceIndex].lpDisplayName;
      gSelectedDeviceGuid := FDeviceProperties[pDeviceIndex].riId;
      lpSelectedSymbolicLink := FDeviceProperties[pDeviceIndex].lpSymbolicLink;
      dwNativeFormats := FDeviceProperties[pDeviceIndex].dwNativeFormats;
      dwSupportedFormats := FDeviceProperties[pDeviceIndex].dwSupportedFormats;


      {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug.LogMediaType(DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('TDeviceExplorer.SetCurrentDeviceProperties');
      {$ENDIF}

      SafeRelease(FMediaType);
      SafeRelease(FActivation);

      hr := CloneVideoMediaType(DeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex].mfMediaType,
                                MFVideoFormat_RGB32 {GUID_NULL},
                                FMediaType);
      if FAILED(hr) then
        goto done;

      {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug.LogMediaType(FMediaType);
      FMediaTypeDebug.SafeDebugResultsToFile('TDeviceExplorer.SetCurrentDeviceProperties');
      {$ENDIF}

      // Set the videoformat that has been selected by user.
      FVideoFormatInfo.Reset();
      FVideoFormatInfo := FDeviceProperties[pDeviceIndex].aVideoFormats[pFormatsIndex];
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
  pActivate: PIMFActivate;
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
                              pActivate,
                              pCount);

  ppActivate := pActivate;
  SafeRelease(pActivate);
  Result := hr;
end;


procedure TDeviceExplorer.SetMediaType(aMediaType: IMFMediaType);
begin
  SafeRelease(FMediaType);
  FMediaType := aMediaType;
end;

end.

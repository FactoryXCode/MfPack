// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Capture.pas
// Kind: Pascal Unit
// Release date: 01-02-2022
// Language: ENU
//
// Revision Version: 3.1.6
// Description: Manages video capture.
//              This sample demonstrates how to capture video from camera to a file.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft docs
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit Capture;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ComBaseApi,
  WinApi.ActiveX.PropIdl,
  {System}
  System.Services.Dbt,
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite;

//////////////////////////////////////////////////////////////////////////
//
// capture.h: Manages video capture.
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//
//////////////////////////////////////////////////////////////////////////



const
  WM_APP_PREVIEW_ERROR = WM_APP + 1;    // wparam = HRESULT

type

  TDeviceList = class(TObject)
   private
     m_cDevices: UINT32;
     m_ppDevices: PIMFActivate;

   public
    constructor Create(); overload;
    destructor Destroy(); override;

    procedure Clear();
    function EnumerateDevices(): HResult;
    function GetDevice(index: UINT32;
                       out pActivate: IMFActivate): HResult;
    function GetFriendlyName(index: UINT32;
                             out pszFriendlyName: LPWSTR): HResult;

    property Count: UINT32 read m_cDevices;
  end;


  EncodingParameters = record
    subtype: TGUID;
    bitrate: UINT32;
  end;


  TState = (State_NotReady = 0,
            State_Ready,
            State_Capturing,
            State_DeviceLost,
            State_SessionEnded
           );

  TMfCaptureEngine = class(TInterfacedObject, IMFSourceReaderCallback)

  protected

    State: TState;
    m_critsec: TCriticalSection;
    m_hwndEvent: HWND;         // Application window to receive events.
    m_pReader: IMFSourceReader;
    m_pWriter: IMFSinkWriter;
    m_bFirstSample: Boolean;
    m_llBaseTime: LONGLONG;
    m_DeviceFriendlyName: PWideChar;
    m_DeviceSymbolicLink: PWideChar;
    m_SubTypes: array of TGuid;

    procedure NotifyError(hr: HResult);
    function OpenMediaSource(pSource: IMFMediaSource): HResult;
    function ConfigureCapture(const param: EncodingParameters): HResult;
    procedure SetSubtypes();

  private

    // Constructor is private. Use static CreateInstance method to instantiate.
    constructor Create(hwWindow: HWND); overload;

    function ConfigureSourceReader(pReader: IMFSourceReader): HResult;
    function ConfigureEncoder(const params: EncodingParameters;
                              pType: IMFMediaType;
                              pWriter: IMFSinkWriter;
                              pdwStreamIndex: DWORD): HResult;
    function CopyAttribute(Src: IMFAttributes;
                           Dest: PIMFAttributes;
                           const key: TGUID): Hresult;

    // IMFSourceReaderCallback methods =========================================
    function OnReadSample(hrStatus: HResult;
                          dwStreamIndex: DWORD;
                          dwStreamFlags: DWORD;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HResult; stdcall;

    function OnFlush(dwStreamIndex: DWORD): HResult; stdcall;

    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HResult; stdcall;
    // =========================================================================

  public
    // Destructor.
    destructor Destroy(); override;

    // Use this function to create the capture engine !
    class function CreateInstance(hwWindow: HWND; {Handle to the window to receive events}
                                  out cCaptureEngine: TMfCaptureEngine {Receives a pointer to the CCapture object.}): HRESULT;
    function StartCapture(pActivate: IMFActivate;
                          const pwszFileName: PWideChar;
                          const param: EncodingParameters): HResult;
    function EndCaptureSession(): HResult;
    function IsCapturing(): TState;
    procedure DeviceLost(bDeviceLost: Boolean);

    property DeviceFriendlyName: PWideChar read m_DeviceFriendlyName;
    property DeviceSymbolicLink: PWideChar read m_DeviceSymbolicLink;
end;



implementation


// TDeviceList

constructor TDeviceList.Create();
begin
  inherited Create();
  m_ppDevices := nil;
  m_cDevices := 0;
end;


destructor TDeviceList.Destroy();
begin
  Clear();
  SafeRelease(m_ppDevices);
  inherited Destroy();
end;


{$POINTERMATH ON}
procedure TDeviceList.Clear();
var
  i: Integer;

begin
  if Assigned(m_ppDevices) then
    begin
      for i := 0 to m_cDevices -1 do
        m_ppDevices[i] := nil;

      CoTaskMemFree(m_ppDevices);
      m_ppDevices := nil;
      m_cDevices := 0;
    end;
end;


function TDeviceList.EnumerateDevices(): HResult;
var
  hr: HResult;
  pAttributes: IMFAttributes;

begin
  Clear();

  // Initialize an attribute store. We will use this to
  // specify the enumeration parameters.

  hr := MFCreateAttributes(pAttributes,
                           2);

  // Ask for source type = video capture devices
  if SUCCEEDED(hr) then
    hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                              MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  // Enumerate devices.
  if SUCCEEDED(hr) then
    hr := MFEnumDeviceSources(pAttributes,
                              m_ppDevices,
                              m_cDevices);

  Result := hr;
end;


{$POINTERMATH ON}
function TDeviceList.GetDevice(index: UINT32;
                               out pActivate: IMFActivate): HResult;
begin
  if index >= Count then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  pActivate := m_ppDevices[index];

  if Assigned(pActivate) then
    Result := S_OK
  else
    Result := E_POINTER;
end;


{$POINTERMATH ON}
function TDeviceList.GetFriendlyName(index: UINT32;
                                     out pszFriendlyName: LPWSTR): HResult;
var
  hr: HResult;
  chLength: UINT32;

begin
  if index >= Count then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  hr := m_ppDevices[index].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                              pszFriendlyName,
                                              chLength);
  Result := hr;
end;



// TMfCaptureEngine

//-------------------------------------------------------------------
//  CreateInstance
//
//  Static class method to create the CCapture object.
//-------------------------------------------------------------------
class function TMfCaptureEngine.CreateInstance(hwWindow: HWND; {Handle to the window to receive events}
                                               out cCaptureEngine: TMfCaptureEngine {Receives a pointer to the CCapture object.}): HRESULT;
var
  pCaptureEngine: TMfCaptureEngine;

begin

  pCaptureEngine := TMfCaptureEngine.Create(hwWindow);

  if not Assigned(PCaptureEngine) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  cCaptureEngine := pCaptureEngine;

  Result := S_OK;
end;

//-------------------------------------------------------------------
//  constructor
//-------------------------------------------------------------------
constructor TMfCaptureEngine.Create(hwWindow: HWND);
begin
  inherited Create();
  m_hwndEvent := hwWindow;
  m_bFirstSample := False;
  m_llBaseTime := 0;
  SetSubTypes();
  m_critsec := TCriticalSection.Create();
end;


//-------------------------------------------------------------------
//  destructor
//-------------------------------------------------------------------
destructor TMfCaptureEngine.Destroy();
begin
  if Assigned(m_pReader) then
    SafeRelease(m_pReader);

  if Assigned(m_pWriter) then
    SafeRelease(m_pWriter);

  FreeAndnil(m_critsec);

  inherited Destroy();
end;


// Sends a message to the owner with corresponding hwnd
procedure TMfCaptureEngine.NotifyError(hr: HResult);
begin
  PostMessage(m_hwndEvent,
              WM_APP_PREVIEW_ERROR,
              WPARAM(hr),
              0);
end;


function TMfCaptureEngine.OpenMediaSource(pSource: IMFMediaSource): HResult;
var
  hr: HResult;
  pAttributes: IMFAttributes;

begin
  hr := MFCreateAttributes(pAttributes,
                           1);

  if SUCCEEDED(hr) then
    hr := pAttributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                                 Self);

  if SUCCEEDED(hr) then
    hr := MFCreateSourceReaderFromMediaSource(pSource,
                                              pAttributes,
                                              m_pReader);

  Result := hr;
end;


//-------------------------------------------------------------------
// ConfigureCapture
//
// Configures the capture session.
//
//-------------------------------------------------------------------
function TMfCaptureEngine.ConfigureCapture(const param: EncodingParameters): HResult;
var
  hr: HResult;
  sink_stream: DWORD;
  pType: IMFMediaType;


begin
  sink_stream := 0;

  hr := ConfigureSourceReader(m_pReader);

  if SUCCEEDED(hr) then
    hr := m_pReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                        pType);

  if SUCCEEDED(hr) then
    hr := ConfigureEncoder(param,
                           pType,
                           m_pWriter,
                           sink_stream);

  if SUCCEEDED(hr) then
    // Register the color converter DSP for this process, in the video
    // processor category. This will enable the sink writer to enumerate
    // the color converter when the sink writer attempts to match the
    // media types.

    hr := MFTRegisterLocalByCLSID(CLSID_CColorConvertDMO,
                                  MFT_CATEGORY_VIDEO_PROCESSOR,
                                  LPCWSTR(''),
                                  MFT_ENUM_FLAG_SYNCMFT,
                                  0,
                                  nil,
                                  0,
                                  nil);

  if SUCCEEDED(hr) then
    hr := m_pWriter.SetInputMediaType(sink_stream,
                                      pType,
                                      nil);

  if SUCCEEDED(hr) then
    hr := m_pWriter.BeginWriting();

  Result := hr;
end;



// IMFSourceReaderCallback methods
//-------------------------------------------------------------------
// OnReadSample
//
// Called when the IMFMediaSource.ReadSample method completes.
//-------------------------------------------------------------------
function TMfCaptureEngine.OnReadSample(hrStatus: HResult;
                                       dwStreamIndex: DWORD;
                                       dwStreamFlags: DWORD;
                                       llTimeStamp: LONGLONG;
                                       pSample: IMFSample): HResult; stdcall;
var
  hr: HResult;

label
  done;

begin
  m_critsec.Enter;

  if (IsCapturing() <> State_Capturing) then
    begin
      m_critsec.Leave;
      Result := S_OK;
      Exit;
    end;

  if FAILED(hrStatus) then
    begin
      hr := hrStatus;
      goto done;
    end;

  if Assigned(pSample) then
    begin
      if m_bFirstSample then
        begin
          m_llBaseTime := llTimeStamp;
          m_bFirstSample := False;
        end;

      // rebase the time stamp
      dec(llTimeStamp,
          m_llBaseTime);

      hr := pSample.SetSampleTime(llTimeStamp);

      if FAILED(hr) then
        goto done;

      hr := m_pWriter.WriteSample(0,
                                  pSample);

      if FAILED(hr) then
        goto done;
    end;

  // Read another sample.
  hr := m_pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                             0,
                             nil,
                             nil,
                             nil,
                             nil);

done:
  if FAILED(hr) then
    NotifyError(hr);

  m_critsec.Leave;
  Result := hr;
end;


function TMfCaptureEngine.OnFlush(dwStreamIndex: DWORD): HResult; stdcall;
begin
  Result := S_OK;
end;

function TMfCaptureEngine.OnEvent(dwStreamIndex: DWORD;
                                  pEvent: IMFMediaEvent): HResult; stdcall;
begin
  Result := S_OK;
end;

// End IMFSourceReaderCallback methods

//-------------------------------------------------------------------
// StartCapture
//
// Start capturing.
//-------------------------------------------------------------------
function TMfCaptureEngine.StartCapture(pActivate: IMFActivate;
                                       const pwszFileName: PWideChar;
                                       const param: EncodingParameters): HResult;
var
  hr: HResult;
  pSource: IMFMediaSource;
  chLength: UINT32;

begin

  m_critsec.Enter;

  // Create the media source for the device.
  hr := pActivate.ActivateObject(IID_IMFMediaSource,
                                 Pointer(pSource));

  // Get the friendly name and symbolic link. This is needed to handle device-
  // loss notifications. (See CheckDeviceLost.)

  if SUCCEEDED(hr) then
    hr := pActivate.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                       m_DeviceFriendlyName,
                                       chLength);
  if SUCCEEDED(hr) then
    hr := pActivate.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                       m_DeviceSymbolicLink,
                                       chLength);

  if SUCCEEDED(hr) then
    hr := OpenMediaSource(pSource);

  // Create the sink writer
  if SUCCEEDED(hr) then
    hr := MFCreateSinkWriterFromURL(pwszFileName,
                                    nil,
                                    nil,
                                    m_pWriter);

  // Set up the encoding parameters.
  if SUCCEEDED(hr) then
    hr := ConfigureCapture(param);

  if SUCCEEDED(hr) then
    begin
      m_bFirstSample := TRUE;
      m_llBaseTime := 0;

      // Request the first video frame.
      hr := m_pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                 0,
                                 nil,
                                 nil,
                                 nil,
                                 nil);
    end;

  if SUCCEEDED(hr) then
    State := State_Capturing
  else
    State := State_NotReady;

  m_critsec.Leave;

  Result := hr;
end;

//-------------------------------------------------------------------
// EndCaptureSession
//
// Stop the capture session.
//
// NOTE: This method resets the object's state to State_NotReady.
// To start another capture session, call SetCaptureFile.
//-------------------------------------------------------------------
function TMfCaptureEngine.EndCaptureSession(): HResult;
var
  hr: HResult;

begin

  hr := S_OK;

  if Assigned(m_pWriter) then
    hr := m_pWriter.Finalize();

  SafeRelease(m_pWriter);
  SafeRelease(m_pReader);

  CoTaskMemFree(m_DeviceFriendlyName);
  m_DeviceFriendlyName := nil;
  CoTaskMemFree(m_DeviceSymbolicLink);
  m_DeviceSymbolicLink := nil;

  State := State_NotReady;

  Result := hr;
end;


function TMfCaptureEngine.IsCapturing(): TState;
var
  bIsCapturing: Boolean;

begin
  m_critsec.Enter;

  bIsCapturing := (m_pWriter <> nil);

  if bIsCapturing then
    State := State_Capturing
  else
    State := State_NotReady;

  m_critsec.Leave;
  Result := State;
end;


procedure TMfCaptureEngine.DeviceLost(bDeviceLost: Boolean);
begin
  if bDeviceLost then
    State := State_DeviceLost;
end;


/////////////// Private/protected class methods ///////////////



//-------------------------------------------------------------------
//  ConfigureSourceReader
//
//  Sets the media type on the source reader.
//-------------------------------------------------------------------

function TMfCaptureEngine.ConfigureSourceReader(pReader: IMFSourceReader): HResult;
var
  hr: HResult;
  bUseNativeType: Boolean;
  pType: IMFMediaType;
  subtype: TGUID;
  i: Integer;

label
  done;

begin
  bUseNativeType := False;

  // If the source's native format matches any of the formats in
  // the list, prefer the native format.

  // Note: The camera might support multiple output formats,
  // including a range of frame dimensions. The application could
  // provide a list to the user and have the user select the
  // camera's output format. That is outside the scope of this
  // sample, however.

  hr := pReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   0,  // Type index
                                   pType);

  if FAILED(hr) then
    goto done;

  hr := pType.GetGUID(MF_MT_SUBTYPE,
                      subtype);

  if FAILED(hr) then
    goto done;

  for i := 0 to length(m_SubTypes) do
    begin
      if (subtype = m_SubTypes[i]) then
        begin
          hr := pReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                            0,
                                            pType);

          bUseNativeType := True;
          break;
        end;
    end;

  if not bUseNativeType then
    begin

      // None of the native types worked. The camera might offer
      // output a compressed type such as MJPEG or DV.

      // Try adding a decoder.

      for i := 0 to Length(m_SubTypes) -1 do
        begin
          hr := pType.SetGUID(MF_MT_SUBTYPE,
                              m_SubTypes[i]);

          if FAILED(hr) then
            goto done;

          hr := pReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                            0,
                                            pType);
{$IF DEBUG}
    if hr = $C00D5212 then
      OutputDebugString(PChar('No suitable transform was found to encode or decode the content.'));
{$ENDIF}

          if SUCCEEDED(hr) then
            break;
        end;
    end;

done:
  // SafeRelease(pType); will be done by compiler
  Result := hr;
end;


function TMfCaptureEngine.ConfigureEncoder(const params: EncodingParameters;
                                           pType: IMFMediaType;
                                           pWriter: IMFSinkWriter;
                                           pdwStreamIndex: DWORD): HResult;
var
  hr: HResult;
  pType2: IMFMediaType;

begin

  hr := MFCreateMediaType(pType2);

  if SUCCEEDED(hr) then
    hr := pType2.SetGUID(MF_MT_MAJOR_TYPE,
                         MFMediaType_Video );

  if SUCCEEDED(hr) then
    hr := pType2.SetGUID(MF_MT_SUBTYPE,
                         params.subtype);

  if SUCCEEDED(hr) then
    hr := pType2.SetUINT32(MF_MT_AVG_BITRATE,
                           params.bitrate);

  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        @pType2,
                        MF_MT_FRAME_SIZE);

  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        @pType2,
                        MF_MT_FRAME_RATE);


  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        @pType2,
                        MF_MT_PIXEL_ASPECT_RATIO);


  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        @pType2,
                        MF_MT_INTERLACE_MODE);

  if SUCCEEDED(hr) then
    hr := pWriter.AddStream(pType2,
                            pdwStreamIndex);

  // SafeRelease(pType2); will be done by compiler
  Result := hr;
end;


//-------------------------------------------------------------------
// CopyAttribute
//
// Copy an attribute value from one attribute store to another.
//-------------------------------------------------------------------
function TMfCaptureEngine.CopyAttribute(Src: IMFAttributes;
                                        Dest: PIMFAttributes;
                                        const key: TGUID): Hresult;
var
  hr: HResult;
  propvar: PROPVARIANT;

begin
  PropVariantInit(propvar);

  hr := Src.GetItem(key,
                    propvar);

  if SUCCEEDED(hr) then
    hr := Dest.SetItem(key,
                       propvar);

  PropVariantClear(propvar);
  Result := hr;
end;


procedure TMfCaptureEngine.SetSubTypes();
begin
  SetLength(m_SubTypes, 5);
  m_SubTypes[0] := MFVideoFormat_NV12;
  m_SubTypes[1] := MFVideoFormat_YUY2;
  m_SubTypes[2] := MFVideoFormat_UYVY;
  m_SubTypes[3] := MFVideoFormat_RGB32;
  m_SubTypes[4] := MFVideoFormat_RGB24;
end;

end.

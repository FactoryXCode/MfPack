// FactoryX
//
// Copyright © by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCaptureToFileClass.pas
// Kind: Pascal Unit
// Release date: 09-02-2018
// Language: ENU
//
// Revision Version: 4.1.0
//
// Description: Device capture class based on MFCaptureToFile example
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (TopPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: FSimpleCapture Example.
//
// https://github.com/Microsoft/Windows-classic-samples/tree/master/Samples/Win7Samples/multimedia/mediafoundation/MFCaptureToFile
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
unit MfCaptureToFileClass;

interface
uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Ks,
  WinApi.WinError,
  {System}
  System.Services.Dbt,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  {MfPack}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9;

  {$TYPEINFO ON}

const

  WM_APP_PREVIEW_ERROR = WM_APP + 1;    // wparam = HRESULT


type


  TDeviceList = class
  protected
    m_cDevices: UINT32;
    m_ppDevices: PIMFActivate; // Pointer to IMFActivate array

  public
    // Contructor & destructor
    //
    constructor Create(); overload;
    // Call cleanups from here
    procedure BeforeDestruction(); override;
    // Caller should call Release.
    destructor Destroy(); override;

    procedure Clear();
    function Count(): UINT32; // { return m_cDevices; }
    function EnumerateDevices(): HRESULT;
    function GetDevice(const index: UINT32;
                       out ppActivate: IMFActivate): HRESULT;
    function GetDeviceName(const index: UINT32;
                           out strName: string): HRESULT;
    function GetDeviceSymLink(const index: UINT32;
                              out ppszSymLink: PWideChar): HRESULT;
  end;


  EncodingParameters = record
    subtype: TGUID;
    bitrate: UINT32;
  end;


  TState = (State_NotReady = 0,
            State_Ready,
            State_Capturing);

  // MediaFoundation status.
  TMfStatus = (MfStarted,
               MfStopped);



type

  TCaptureToFile = class(TInterfacedPersistent, IMFSourceReaderCallback)
  private
    FLock: System.SyncObjs.TCriticalSection;
    FFlushEvent: THandle;
    FSource: IMFMediaSource;
    FActivation: IMFActivate;
    FLastError: HResult;
    m_State: TState;
    m_hwndEvent: HWND;         // Application window to receive events.

    m_pReader: IMFSourceReader;
    m_pWriter: IMFSinkWriter;

    m_bFirstSample: BOOL;
    m_llBaseTime: LongLong;

    m_strSymbolicLink: string;
    m_strName: string;

    m_VideoStats: MF_SINK_WRITER_STATISTICS;

    function ConfigureCapture(var param: EncodingParameters): HResult;
    function EndCaptureInternal(): HResult;

    // IMFSourceReaderCallback /////////////////////////////////////////////////
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWORD;
                          dwStreamFlags: DWORD;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HResult; stdcall;

    function OnFlush(dwStreamIndex: DWORD): HResult; stdcall;

    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HResult; stdcall;

    ////////////////////////////////////////////////////////////////////////////

  public

    // Constructor & destuctor
    //
    constructor Create(hMainForm: HWND); reintroduce;
    // Clean up al objects here, before destroy is called
    procedure BeforeDestruction(); override;
    // Caller should call Release.
    destructor Destroy(); override;

    // Clear and reset all objects by calling EndCaptureInternal()
    procedure Clear();
    function StartCapture(pActivate: IMFActivate;
                          const wszFileName: WideString;
                          param: EncodingParameters): HResult;
    function EndCaptureSession(): HResult;
    function IsCapturing(): BOOL;

  published
    property LastError: HResult read FLastError;
    property State: TState read m_State write m_State;
    property DeviceName: string read m_strName;
    property DeviceSymbolicLink: string read m_strSymbolicLink;
    property VideoStatistics: MF_SINK_WRITER_STATISTICS read m_VideoStats;

end;

var
  MfDeviceList: TDeviceList;
  MfStatus: TMfStatus;       // Status of MediaFoundation


// Helpers
//========

  function ConfigureSourceReader(pReader: IMFSourceReader): HRESULT;

  function ConfigureEncoder(const params: EncodingParameters;
                            pType: IMFMediaType;
                            pWriter: IMFSinkWriter;
                            pdwStreamIndex: DWORD): HRESULT;

  function CopyAttribute(pSrc: IMFAttributes;
                         pDest: IMFAttributes;
                         const key: REFGUID): HRESULT;

  // Register for Device Notification.
  // Before you start capturing from a device, call the RegisterDeviceNotification
  // function to register for device notifications.
  // Register for the KSCATEGORY_CAPTURE device class, as shown in this function.


implementation


// TCaptureToFile
////////////////////////////////////////////////////////////////////////////////

procedure TCaptureToFile.Clear();
begin
  m_hwndEvent := 0;
  m_bFirstSample := False;
  m_llBaseTime := 0;
  EndCaptureInternal();
end;


// Constructor & destructor section ////////////////////////////////////////////
constructor TCaptureToFile.Create(hMainForm: HWND);
begin
 inherited Create;m_hwndEvent:=hMainForm;
 FLock:=System.SyncObjs.TCriticalSection.Create;
 FFlushEvent:=CreateEvent(nil,True,False,nil);
 if FFlushEvent=0 then RaiseLastOSError;
end;

//
procedure TCaptureToFile.BeforeDestruction();
begin EndCaptureInternal;inherited;end;


// Caller should call Release!!
destructor TCaptureToFile.Destroy();
begin
 if FFlushEvent<>0 then CloseHandle(FFlushEvent);
 FLock.Free;inherited;
end;

// End Constructor & destructor section ////////////////////////////////////////


// Event methods ///////////////////////////////////////////////////////////////

function TCaptureToFile.OnReadSample(hrStatus: HRESULT;dwStreamIndex,dwStreamFlags: DWORD;llTimestamp: LONGLONG;pSample: IMFSample): HResult;
begin
 Result:=S_OK;FLock.Enter;
 try
  if (m_State<>State_Capturing) or (m_pReader=nil) or (m_pWriter=nil) then Exit;
  Result:=hrStatus;
  if SUCCEEDED(Result) and (pSample<>nil) then begin
   if m_bFirstSample then begin m_llBaseTime:=llTimestamp;m_bFirstSample:=False;end;
   Result:=pSample.SetSampleTime(llTimestamp-m_llBaseTime);
   if SUCCEEDED(Result) then Result:=m_pWriter.WriteSample(0,pSample);
   if SUCCEEDED(Result) then begin m_VideoStats:=InitializeStatistics;m_pWriter.GetStatistics(0,m_VideoStats);end;
  end;
  if SUCCEEDED(Result) and ((dwStreamFlags and MF_SOURCE_READERF_ENDOFSTREAM)=0) then
   Result:=m_pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,0,nil,nil,nil,nil);
  if FAILED(Result) then begin
   FLastError:=Result;m_State:=State_NotReady;
   PostMessage(m_hwndEvent,WM_APP_PREVIEW_ERROR,WPARAM(DWORD(Result)),0);
  end;
 finally FLock.Leave;end;
end;


function TCaptureToFile.OnFlush(dwStreamIndex: DWORD): HResult;
begin SetEvent(FFlushEvent);Result:=S_OK;end;


function TCaptureToFile.OnEvent(dwStreamIndex: DWORD;
                                pEvent: IMFMediaEvent): HRESULT;
var
  hr: HRESULT;
  pmet: MediaEventType;

begin
  // For example...
  hr := pEvent.GetType(pmet);

  // Possible pEvent values
  //========================
  // MEBufferingStarted
  // MEBufferingStopped
  // MEConnectEnd
  // MEConnectStart
  // MEExtendedType
  // MESourceCharacteristicsChanged
  // MESourceMetadataChanged

  Result := hr;
end;


// End Event methods ///////////////////////////////////////////////////////////


// Public methods //////////////////////////////////////////////////////////////


//-------------------------------------------------------------------
// ConfigureCapture
//
// Configures the capture session.
//
//-------------------------------------------------------------------
function TCaptureToFile.ConfigureCapture(var param: EncodingParameters): HRESULT;
var
  hr: HRESULT;
  sink_stream: DWORD;
  pType: IMFMediaType;

begin

  sink_stream := 0;

  hr:= ConfigureSourceReader(m_pReader);

  if SUCCEEDED(hr) then
    hr:= m_pReader.GetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),@pType);

  if SUCCEEDED(hr) then
    hr := ConfigureEncoder(param,
                           pType,
                           m_pWriter,
                           sink_stream);

  if SUCCEEDED(hr) then
    begin
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
    end;

  if SUCCEEDED(hr) then
    hr := m_pWriter.SetInputMediaType(sink_stream,
                                      pType,
                                      nil);


  Result := hr;
end;


//------------------------------------------------------------------------------
// EndCaptureInternal
//
// Stops capture.
//------------------------------------------------------------------------------
function TCaptureToFile.EndCaptureInternal(): HResult;
var Reader: IMFSourceReader;Writer: IMFSinkWriter;Source: IMFMediaSource;Activation: IMFActivate;
begin
 Result:=S_OK;if FLock=nil then Exit;
 FLock.Enter;
 try
  m_State:=State_NotReady;Reader:=m_pReader;m_pReader:=nil;
  Writer:=m_pWriter;m_pWriter:=nil;Source:=FSource;FSource:=nil;
  Activation:=FActivation;FActivation:=nil;
 finally FLock.Leave;end;
 if Reader<>nil then begin
  ResetEvent(FFlushEvent);
  if SUCCEEDED(Reader.Flush(MF_SOURCE_READER_ALL_STREAMS)) then WaitForSingleObject(FFlushEvent,5000);
 end;
 if Source<>nil then Source.Shutdown;
 if Activation<>nil then Activation.ShutdownObject;
 Reader:=nil;
 if Writer<>nil then Result:=Writer.Finalize;
end;



//------------------------------------------------------------------------------
// StartCapture
//
// Start capturing.
//------------------------------------------------------------------------------
function TCaptureToFile.StartCapture(pActivate: IMFActivate;const wszFileName: WideString;param: EncodingParameters): HResult;
var Attributes: IMFAttributes;Name,Link: PWideChar;Count: UINT32;
begin
 if (pActivate=nil) or (wszFileName='') then Exit(E_INVALIDARG);
 EndCaptureInternal;Name:=nil;Link:=nil;FLock.Enter;
 try
  FLastError:=S_OK;FActivation:=pActivate;
  Result:=pActivate.ActivateObject(IID_IMFMediaSource,Pointer(FSource));
  if SUCCEEDED(Result) then Result:=pActivate.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,Name,Count);
  if SUCCEEDED(Result) then Result:=pActivate.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,Link,Count);
  if SUCCEEDED(Result) then begin m_strName:=UnicodeString(Name);m_strSymbolicLink:=UnicodeString(Link);end;
  if SUCCEEDED(Result) then Result:=MFCreateAttributes(Attributes,1);
  if SUCCEEDED(Result) then Result:=Attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,Self);
  if SUCCEEDED(Result) then Result:=MFCreateSourceReaderFromMediaSource(FSource,Attributes,m_pReader);
  if SUCCEEDED(Result) then Result:=MFCreateSinkWriterFromURL(PWideChar(wszFileName),nil,nil,m_pWriter);
  if SUCCEEDED(Result) then Result:=ConfigureCapture(param);
  if SUCCEEDED(Result) then Result:=m_pWriter.BeginWriting;
  if SUCCEEDED(Result) then begin
   m_bFirstSample:=True;m_llBaseTime:=0;m_State:=State_Capturing;
   Result:=m_pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,0,nil,nil,nil,nil);
  end;
  if FAILED(Result) then m_State:=State_NotReady;
 finally CoTaskMemFree(Name);CoTaskMemFree(Link);FLock.Leave;end;
 if FAILED(Result) then EndCaptureInternal;
end;

//------------------------------------------------------------------------------
// EndCaptureSession
//
// Stop the capture session.
//
// NOTE: This method resets the object's state to State_NotReady.
// To start another capture session, call SetCaptureFile.
//------------------------------------------------------------------------------
function TCaptureToFile.EndCaptureSession(): HResult;
begin Result:=EndCaptureInternal;end;


function TCaptureToFile.IsCapturing(): BOOL;
var
  bIsCapturing: BOOL;

begin
  bIsCapturing := (m_pWriter <> nil) ;
  Result := bIsCapturing;
end;


//-------------------------------------------------------------------
//  CheckDeviceLost
//  Checks whether the video capture device was removed.
//
//  The application calls this method when it receives a
//  WM_DEVICECHANGE message.
//
//  Compare the device notification message against the symbolic link of your device, as follows:
//  1. Check the dbch_devicetype member of the DEV_BROADCAST_HDR structure.
//     If the value is DBT_DEVTYP_DEVICEINTERFACE, cast the structure pointer to a DEV_BROADCAST_DEVICEINTERFACE structure.
//  2. Compare the dbcc_name member of this structure to the symbolic link of the device.
//-------------------------------------------------------------------




// TDeviceList section /////////////////////////////////////////////////////////


constructor TDeviceList.Create();
begin
  inherited Create();
end;

// Before Destroy is called, release the interfaces.
procedure TDeviceList.BeforeDestruction();
begin
  Clear();
  inherited;
end;

// Caller should call Release.
destructor TDeviceList.Destroy();
begin
  inherited;
end;


procedure TDeviceList.Clear();
var I: Integer;
begin
 if m_ppDevices<>nil then begin
  {$POINTERMATH ON}
  for I:=0 to Integer(m_cDevices)-1 do m_ppDevices[I]:=nil;
  {$POINTERMATH OFF}
  CoTaskMemFree(m_ppDevices);
 end;
 m_ppDevices:=nil;m_cDevices:=0;
end;


function TDeviceList.Count(): UINT32;
begin
  Result := m_cDevices;
end;


function TDeviceList.EnumerateDevices(): HRESULT;
var
  hr: HRESULT;
  pAttributes: IMFAttributes;

begin

  Clear();

  // Initialize an attribute store. We will use this to
  // specify the enumeration parameters.

  hr := MFCreateAttributes(pAttributes,
                           1);

  // Ask for source type = video capture devices
  if SUCCEEDED(hr) then
    hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                              MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  // Enumerate devices.

  if SUCCEEDED(hr) then
    begin
      hr := MFEnumDeviceSources(pAttributes, {in}
                                m_ppDevices, {out}
                                m_cDevices); {out}
    end;
  Result := hr;
end;


//
function TDeviceList.GetDevice(const index: UINT32;
                               out ppActivate: IMFActivate): HRESULT;
begin

  if (index >= Count()) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

{$POINTERMATH ON}

  ppActivate := m_ppDevices[index];


{$POINTERMATH OFF}
  Result :=  S_OK;
end;

//
function TDeviceList.GetDeviceName(const index: UINT32;
                                   out strName: string): HRESULT;
var
  hr: HRESULT;
  pcchLength: UINT32;
  ppszName: PWideChar;

label
  Done;

begin

  if (index >= Count()) then
    begin
      hr := E_INVALIDARG;
      goto Done;
    end;

{$POINTERMATH ON}
  hr := m_ppDevices[index].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                              ppszName,
                                              pcchLength);
{$POINTERMATH OFF}

  // Convert to string strName
  if SUCCEEDED(hr) then
    begin strName := WideCharToString(ppszName);CoTaskMemFree(ppszName);end
  else
    strName := 'Unknown';

done:
  Result := hr;
end;


function TDeviceList.GetDeviceSymLink(const index: UINT32;
                                      out ppszSymLink: PWideChar): HRESULT;
var
  hr: HRESULT;
  pcchLength: UINT32;

label
  Done;

begin

  if (index >= Count()) then
    begin
      hr := E_INVALIDARG;
      goto Done;
    end;

{$POINTERMATH ON}
  hr:= m_ppDevices[index].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                             ppszSymLink,
                                             pcchLength);
{$POINTERMATH OFF}

done:
  Result := hr;
end;


// End TDeviceList section /////////////////////////////////////////////////////

// Helpers /////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
//  ConfigureSourceReader
//
//  Sets the media type on the source reader.
//------------------------------------------------------------------------------
function ConfigureSourceReader(pReader: IMFSourceReader): HRESULT;
var
  hr: HRESULT;
  _i: Integer;
  bUseNativeType: BOOL;
  subtype: TGuid;
  pType: IMFMediaType;
  // The list of acceptable types.
 const subtypes : array[0..5] of ^Tguid = (@MFVideoFormat_NV12, @MFVideoFormat_YUY2, @MFVideoFormat_UYVY,
                                           @MFVideoFormat_RGB32, @MFVideoFormat_RGB24, @MFVideoFormat_IYUV);


label
  Done;

begin
  bUseNativeType := False;
  subtype := GUID_NULL;

  // If the source's native format matches any of the formats in
  // the list, prefer the native format.

  // Note: The camera might support multiple output formats,
  // including a range of frame dimensions. The application could
  // provide a list to the user and have the user select the
  // camera's output format. That is outside the scope of this
  // sample, however.
  // You may look in to the MFCaptureEngineVideoCapture sample on how to.

  hr := pReader.GetNativeMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                   0,  // Type index
                                   @pType);

  if FAILED(hr) then
    goto Done;

  hr := pType.GetGUID(MF_MT_SUBTYPE,
                      subtype);

  if FAILED(hr) then
    goto Done;
    // The list of acceptable types.
    // MFVideoFormat_NV12, MFVideoFormat_YUY2, MFVideoFormat_UYVY,
    // MFVideoFormat_RGB32, MFVideoFormat_RGB24, MFVideoFormat_IYUV);


  for _i := 0 to High(subtypes) do
    begin
      if IsEqualGuid(subtype, subtypes[_i]^) then
        begin
          hr := pReader.SetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                            0,
                                            pType);

          bUseNativeType := True;
          Break;
        end;
    end;

  if not bUseNativeType then
    begin
      // None of the native types worked. The camera might offer
      // output a compressed type such as MJPEG or DV.

      // Try adding a decoder.
      for _i := 0 to Length(subtypes) - 1 do
        begin
            hr := pType.SetGUID(MF_MT_SUBTYPE,
                                subtypes[_i]^);

            if FAILED(hr) then
              Break;

            hr := pReader.SetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                              0,
                                              pType);

            if SUCCEEDED(hr) then
              Break;
        end;
    end;

done:
    Result:= hr;
end;


function ConfigureEncoder(const params: EncodingParameters;
                          pType: IMFMediaType;
                          pWriter: IMFSinkWriter;
                          pdwStreamIndex: DWORD): HRESULT;
var
  hr: HRESULT;
  pType2: IMFMediaType;


begin

  hr := MFCreateMediaType(pType2);

  if SUCCEEDED(hr) then
    hr := pType2.SetGUID(MF_MT_MAJOR_TYPE,
                         MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pType2.SetGUID(MF_MT_SUBTYPE,
                         params.subtype);

  if SUCCEEDED(hr) then
    hr := pType2.SetUINT32(MF_MT_AVG_BITRATE,
                           params.bitrate);

  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        pType2,
                        MF_MT_FRAME_SIZE);

  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        pType2,
                        MF_MT_FRAME_RATE);

  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        pType2,
                        MF_MT_PIXEL_ASPECT_RATIO);

  if SUCCEEDED(hr) then
    hr := CopyAttribute(pType,
                        pType2,
                        MF_MT_INTERLACE_MODE);

  if SUCCEEDED(hr) then
    hr := pWriter.AddStream(pType2,
                            pdwStreamIndex);

  Result := hr;
end;


//------------------------------------------------------------------------------
// CopyAttribute
//
// Copy an attribute value from one attribute store to another.
//------------------------------------------------------------------------------
function CopyAttribute(pSrc: IMFAttributes;
                       pDest: IMFAttributes;
                       const key: REFGUID): HRESULT;
var
  hr: HRESULT;
  pvar: PROPVARIANT;  // Don't use the ActiveX one, the ActiveX PROPVARIANT is Delphi version depended!

begin

  PropVariantInit(pvar);

  hr := pSrc.GetItem(key,
                     pvar);

  if SUCCEEDED(hr) then
    hr := pDest.SetItem(key,
                        pvar);

  PropVariantClear(pvar);
  Result := hr;
end;



// End Helpers /////////////////////////////////////////////////////////////////



// initialization and  finalizationsection /////////////////////////////////////

initialization

  // Possible values are: MfStarted, MfStopped
  MfStatus := MfStopped;

  // Initialize Media Foundation
  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' +
                         IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort;
    end;

  MfStatus := MfStarted;

finalization
  MFShutdown();

end.

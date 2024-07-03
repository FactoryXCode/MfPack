// FactoryX
//
// Copyright © by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: Preview.pas
// Kind: Pascal Unit
// Release date: 08-03-2019
// Language: ENU
//
// Version: 3.1.4
//
// Description: Manages video preview.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
// =============================================================================
// Source: MFCaptureD3D Sample
//         preview.h : Direct3D preview class.
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
unit Preview;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Services.Dbt,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.Mfapi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.Mferror,
  {Project}
  Device,
  VideoBufferLock;

  {$MINENUMSIZE 4}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

const
  WM_APP_PREVIEW_ERROR = WM_APP + 1;    // wparam = HRESULT
  WM_APP_DRAWDEV_RESTORE = WM_APP + 2;

type

  //
  // ChooseDeviceParam structure
  //
  // Holds an array of IMFActivate pointers that represent video
  // capture devices.
  //

  ChooseDeviceParam = record
    ppDevices: PIMFActivate; // Pointer to array of IMFActivate pointers.
    count: UINT32;           // Number of elements in the array.
    selection: UINT32;       // Selected device, by array index.
    pwcSelection: LPWSTR;    // Selected devicename
  end;

  // request commands for a-syncrone handling
  TRequest = (reqNone, reqResize, reqSample);
  TState = (stInitializing, stError, stCapturing, stStopped);

  TCPreview = class(TInterfacedPersistent, IMFSourceReaderCallback)
  private

    m_pReader: IMFSourceReader;
    m_pSource: IMFMediaSource;
    m_draw: TDrawDevice;           // Manages the Direct3D device.

    m_hwndVideo: HWND;             // Video window.
    m_hwndEvent: HWND;             // Application window to receive events.
    m_state: TState;

    m_bFirstSample: BOOL;
    m_llBaseTime: LongLong;
    m_strSymbolicLink: string;
    m_strDeviceName: string;

    // Implementation of the interface /////////////////////////////////////////
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWord;
                          dwStreamFlags: DWord;
                          llTimestamp: LongLong;
                          pSample: IMFSample): HResult; stdcall;

    function OnFlush(dwStreamIndex: DWord): HRESULT; stdcall;

    function OnEvent(dwStreamIndex: DWord;
                     pEvent: IMFMediaEvent): HResult; stdcall;

    ////////////////////////////////////////////////////////////////////////////

    // Constructor
    constructor Create(hVideo: HWND;
                       hEvent: HWND); reintroduce; virtual;


    function Initialize(): HResult;
    procedure NotifyError(hr: HResult);
    function TryMediaType(pType: IMFMediaType): HResult;
    function ResizeVideo(): HResult;

  public

    m_Request: TRequest;
    FCritSec: TCriticalSection;

    // Constructor is private. Use static CreateInstance method to create.
    class function CreateInstance(hVideo: HWND;
                                  hEvent: HWND;
                                  out pPlayer: TCPreview): HResult; static;
    // Destructor
    destructor Destroy(); override;
    //
    // Handle stuff before reaching Destroy
    procedure BeforeDestruction(); override;
    ////////////////////////////////////////////////////////////////////////////

    function SetDevice(pActivate: IMFActivate): HResult;
    function CloseDevice(): HResult;

    property DeviceSymbolicLink: string read m_strSymbolicLink;
    property DeviceName: string read m_strDeviceName;
    property State: TState read m_state;
  end;

  // helper
  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);

var
  param: ChooseDeviceParam;
  g_pPreview: TCPreview;


implementation



//-------------------------------------------------------------------
//  CreateInstance
//
//  Static class method to create the CPreview object.
//-------------------------------------------------------------------
class function TCPreview.CreateInstance(hVideo: HWND;         // Handle to the video window.
                                        hEvent: HWND;         // Handle to the window to receive notifications.
                                        out pPlayer: TCPreview): HResult; // Receives the CPreview object.
var
  hr: HResult;

begin

// Debug only
{$IFDEF DEBUG}
  assert(hVideo <> 0);
  assert(hEvent <> 0);
{$ENDIF}

  pPLayer := TCPreview.Create(hVideo,
                              hEvent);

  // The CPlayer constructor sets the ref count to 1.

  if (pPlayer = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  hr := pPlayer.Initialize();

  Result := hr;
end;

// Constructor & destuctor /////////////////////////////////////////////////////
//
// Constructor
constructor TCPreview.Create(hVideo: HWND;
                             hEvent: HWND);
begin
  inherited Create();
  m_pReader := nil;
  m_draw := nil;
  m_hwndVideo := hVideo;     // Handle to the video window.
  m_hwndEvent := hEvent;     // Handle to the window to receive notifications.
  m_State := stInitializing;

  // Create the callback
  FCritSec := TCriticalSection.Create();

  m_bFirstSample := FALSE;
  m_llBaseTime := 0;
  m_request := reqNone;
  inherited Create();
end;
//
//
procedure TCPreview.BeforeDestruction();
begin
  CloseDevice();
  inherited BeforeDestruction();
end;
//
// Destructor
destructor TCPreview.Destroy();
begin
  // CloseDevice() is called in BeforeDestruction()

  if Assigned(m_pSource) then
    SafeRelease(m_pSource);

  if Assigned(m_pReader) then
    SafeRelease(m_pReader);

  if Assigned(m_draw) then
    begin
      m_draw.DestroyDevice();
      FreeAndnil(m_draw);
    end;

  FCritSec.Destroy;
  inherited Destroy();
end;
//
////////////////////////////////////////////////////////////////////////////////


//-------------------------------------------------------------------
//  Initialize
//
//  Initializes the draw object.
//-------------------------------------------------------------------
function TCPreview.Initialize(): HResult;
var
  hr: HResult;

begin
  // Create the DrawDevice
  m_draw := TDrawDevice.Create();
  // Create the Direct3D device.
  hr := m_draw.CreateDevice(m_hwndVideo);

  Result := hr;
end;


//-------------------------------------------------------------------
//  CloseDevice
//
//  Releases all resources held by this object.
//-------------------------------------------------------------------
function TCPreview.CloseDevice(): HResult;
begin
  FCritSec.Enter;

  if Assigned(m_pReader) then
    begin
      if Assigned(m_pSource) then
        begin
          m_pSource.Shutdown();
        end;
      m_pReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
    end;

  m_strSymbolicLink := '';
  m_strDeviceName := '';
  m_State := stStopped;
  FCritSec.Leave;
  Result := S_OK;
end;


procedure TCPreview.NotifyError(hr: HResult);
begin
  SendMessage(m_hwndEvent,
              WM_APP_PREVIEW_ERROR,
              WPARAM(hr),
              LPARAM(0));
end;


//------------------------------------------------------------------------------
// TryMediaType
//
// Test a proposed video format.
//------------------------------------------------------------------------------
function TCPreview.TryMediaType(pType: IMFMediaType): HResult;
var
  hr: HRESULT;
  bFound: BOOL;
  subtype: TGuid;
  i: DWORD;

begin

  bFound := False;
  subtype := GUID_NULL;

  hr := pType.GetGUID(MF_MT_SUBTYPE,
                      subtype);

  if FAILED(hr) then
    begin
      Result := hr;
      Exit;
    end;

  // Do we support this type directly?
  if m_draw.IsFormatSupported(subtype) then
    bFound := True
  else
    begin
      // Can we decode this media type to one of our supported
      // output formats?
      i := 0;
      repeat
        // Get the i'th format.
        m_draw.GetFormat(i,
                         subtype);

        hr := pType.SetGUID(MF_MT_SUBTYPE,
                            subtype);

        if FAILED(hr) then
          Break;

        // Try to set this type on the source reader.
        // If no suitable transform found then hr = -1072868846 ($C00D5212)
        // Try find the next transform (SUCCEEDED(hr) = True)
        hr := m_pReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                            0,
                                            pType);

        if SUCCEEDED(hr) then
          begin
            bFound := True;
            Break;
          end;

        inc(i);

      until (i > g_cFormats);   // Defined in unit DrawDeviceClass: Holds the number of supported media types
    end;  // Repeat

  if bFound = True then
    hr := m_draw.SetVideoType(pType);

  Result := hr;
end;


/////////////// TMFSourceReaderCallback methods ////////////////////////////////

//------------------------------------------------------------------------------
// OnReadSample
//
// Called when the IMFMediaSource.ReadSample method completes.
//------------------------------------------------------------------------------
function TCPreview.OnReadSample(hrStatus: HRESULT;
                                dwStreamIndex: DWord;
                                dwStreamFlags: DWord;
                                llTimestamp: LongLong;
                                pSample: IMFSample): HResult;
var
  hr: HResult;
  pBuffer: IMFMediaBuffer;

label
  done;

begin

  hr := S_OK;
  FCritSec.Enter();

  if not Assigned(g_pPreview) then
    goto done;

  if (g_pPreview.m_pReader = nil) and (pSample = nil) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  if FAILED(hrStatus) then
    hr := hrStatus;

  if SUCCEEDED(hr) then
    begin
      if Assigned(pSample) then
        begin
          if g_pPreview.m_bFirstSample then
            begin
              g_pPreview.m_llBaseTime := llTimeStamp;
              g_pPreview.m_bFirstSample := FALSE;
            end;

          // rebase the time stamp
          llTimeStamp := llTimeStamp - g_pPreview.m_llBaseTime;
          hr := pSample.SetSampleTime(llTimeStamp);

          // Get the video frame buffer from the sample.
          // Like this
          if SUCCEEDED(hr) then
            hr := pSample.GetBufferByIndex(0,
                                           @pBuffer);
          // or like this (both are permitted)
          //   hr := pSample.ConvertToContiguousBuffer(pBuffer);

          // Draw the frame and create the lockbuffer (buffer)
          if SUCCEEDED(hr) then
            hr := g_pPreview.m_draw.DrawFrame(pBuffer);
        end;
    end;

  // Request the next frame.
  if SUCCEEDED(hr) then
    begin
     if Assigned(g_pPreview.m_pReader) then
       hr := g_pPreview.m_pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                             0,
                                             nil,   // actual
                                             nil,   // flags
                                             nil,   // timestamp
                                             nil)   // sample optional
     else
       begin
         hr := E_POINTER;
         goto done;
       end;
    end;

  // Here you could implement the stream flags, like:
  // check the StreamFlags

  // Show timeframes in Mainwindow caption
  SetWindowText(g_pPreview.m_hwndEvent,
                'Capturing: ' + HnsTimeToStr(llTimestamp, false));

  // On resize, resize the picture to destination size.
  // Note: On critical events, like for example OnResize, we need to process these when a sample
  //       is ready. (Note: Remember we are dealing with asynchronous mode event handling)
  if (g_pPreview.m_Request = ReqResize) then
    begin
      g_pPreview.ResizeVideo();
      g_pPreview.m_Request := ReqNone;
    end;


done:

  if FAILED(hr) then
    begin
      g_pPreview.NotifyError(hr);
      m_State := stError;
    end
  else
    m_State := stCapturing;

  pBuffer := nil;

  FCritSec.Leave();
  SetEvent(m_hwndEvent);
  Result := hr;
end;


function TCPreview.OnFlush(dwStreamIndex: DWord): HRESULT;
begin
  Result := S_OK;
end;


function TCPreview.OnEvent(dwStreamIndex: DWord;
                           pEvent: IMFMediaEvent): HResult; stdcall;
begin
  Result := S_OK;
end;

// END TMFSourceReaderCallback


//-------------------------------------------------------------------
// SetDevice
//
// Set up preview for a specified video capture device.
//-------------------------------------------------------------------
function TCPreview.SetDevice(pActivate: IMFActivate): HResult;
var
  hr: HRESULT;
  pAttributes: IMFAttributes;
  pType: IMFMediaType;
  i: Integer;
  m_pwszSymbolicLink: PWideChar;
  m_pwszDeviceName: PWideChar;
  m_cchAllocStr: UINT32;

label
  done;

begin
  hr := S_OK;

  // Release the current device, if any.
  if Assigned(m_pReader) then
    hr := CloseDevice();
  if FAILED(hr) then
    goto done;

  // Create the media source for the device.
  hr := pActivate.ActivateObject(IID_IMFMediaSource,
                                 {Pointer} m_pSource);
  if FAILED(hr) then
    goto done;

  // Get the symbolic link.
  hr := pActivate.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                     m_pwszSymbolicLink,
                                     m_cchAllocStr);
  if FAILED(hr) or (m_cchAllocStr = 0) then
    goto done;

  m_strSymbolicLink := WideCharToString(m_pwszSymbolicLink);

  hr := pActivate.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                     m_pwszDeviceName,
                                     m_cchAllocStr);
  if FAILED(hr) or (m_cchAllocStr = 0) then
    goto done;

  m_strDeviceName := WideCharToString(m_pwszSymbolicLink);



  //
  // Create the source reader.
  //

  // Create an attribute store to hold initialization settings.

  hr := MFCreateAttributes(pAttributes,
                           2);
  if FAILED(hr) then
    goto done;

  hr := pAttributes.SetUINT32(MF_READWRITE_DISABLE_CONVERTERS,
                             {TRUE}UINT32(1));
  if FAILED(hr) then
    goto done;

  // Set the callback pointer.
  hr := pAttributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                               Self);
  if FAILED(hr) then
    goto done;

  hr := MFCreateSourceReaderFromMediaSource(m_pSource,
                                            pAttributes,
                                            m_pReader);
  if FAILED(hr) then
    goto done;

  // Try to find a suitable output type.

  for i := 0 to g_cFormats -1 do
    begin
      hr := m_pReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                         i,
                                         pType);

      if FAILED(hr) then
        Break;

      hr := TryMediaType(pType);

      SafeRelease(pType);

      if SUCCEEDED(hr) then
        Break;  // Found an output type.
    end;
  if FAILED(hr) then
    goto done;

  // Ask for the first sample. From here the IMFSourceReaderCallback.OnReadSample will be activated
  hr := m_pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                             0,
                             nil,
                             nil,
                             nil,
                             nil);

  m_bFirstSample := True;
  m_llBaseTime := 0;

  if FAILED(hr) then
    begin
      if Assigned(m_pSource) then
        begin
          m_pSource.Shutdown();
          // NOTE: The source reader shuts down the media source
          // by default, but we might not have gotten that far.
        end;
      CloseDevice();
    end;

done:
  Result := hr;
end;


//-------------------------------------------------------------------
//  ResizeVideo
//  Resizes the video rectangle.
//
//  The application should call this method if the size of the video
//  window changes; e.g., when the application receives WM_SIZE.
//-------------------------------------------------------------------
function TCPreview.ResizeVideo(): HResult;
var
  hr : HRESULT;

begin
  hr := S_OK;
  FCritSec.Enter;
  if Assigned(m_draw) then
    begin
      hr := m_draw.ResetDevice();
      if FAILED(hr) then
        MessageBox(0,
                   PWideChar('ResetDevice failed!'),
                   nil,
                   MB_OK);
    end;
  FCritSec.Leave;
  Result := hr;
end;


// Helpers
procedure HandleThreadMessages(AThread: THandle;
                               AWait: Cardinal = INFINITE);
var
  oMsg: TMsg;

begin

  while (MsgWaitForMultipleObjects(1,
                                   AThread,
                                   False,
                                   AWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(oMsg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if oMsg.Message = WM_QUIT then
        Exit;

      TranslateMessage(oMsg);
      DispatchMessage(oMsg);
    end;
end;

end.

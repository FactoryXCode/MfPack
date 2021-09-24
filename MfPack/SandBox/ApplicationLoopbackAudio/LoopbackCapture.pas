// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: ApplicationLoopBackAudio sample
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: LoopbackCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 20-08-2021
// Language: ENU
//
// Revision Version: 3.0.2
// Description: CLoopbackCapture class.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
//
//------------------------------------------------------------------------------
//
// Remarks: Note that this sample requires Windows 10 build 20348 or later.
//
// Related objects: -
// Related projects: MfPackX301
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: ApplicationLoopBackAudio sample: LoopbackCapture.h, LoopbackCapture.cpp
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit LoopbackCapture;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ComBaseApi,
  {system}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.WinApiTypes,
  WinApi.MmReg,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClientActivationParams,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  {Application}
  Helpers;

const
  m_SampleReadyEvent = WM_USER + 221;
  m_hActivateCompleted = WM_USER + 222;
  m_hCaptureStopped = WM_USER + 223;
  BITS_PER_BYTE = 8;


type

  // NB: All states >= Initialized will allow some methods
  // to be called successfully on the Audio Client
  DeviceState = (
        Uninitialized,
        Error,
        Initialized,
        Starting,
        Capturing,
        Stopping,
        Stopped
                );

  CallbackAsyncCallback = class(IMFAsyncCallback)
  public
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;

    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;
  end;


  CLoopbackCapture = class(TInterfacedPersistent, IActivateAudioInterfaceCompletionHandler)
  private

    m_AudioClient: IAudioClient;
    m_AudioCaptureClient: IAudioCaptureClient;
    m_SampleReadyAsyncResult: IMFAsyncResult;
    m_CaptureFormat: WAVEFORMATEX;
    m_BufferFrames: UINT32;

    m_SampleReadyKey: MFWORKITEM_KEY;
    m_hFile: HFILE;
    m_CritSec: TMFCritSec;
    m_dwQueueID: DWORD;
    m_cbHeaderSize: DWORD;
    m_cbDataSize: DWORD;

    // These two members are used to communicate between the main thread
    // and the ActivateCompleted callback.
    m_outputFileName: PCWSTR;
    m_activateResult: HResult;

    m_DeviceState: DeviceState; // DeviceState = Uninitialized


    // IActivateAudioInterfaceCompletionHandler implementation =================
    function ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HResult; stdcall;
    // =========================================================================


    // Event handlers
    function OnStartCapture(pResult: IMFAsyncResult): HResult;
    function OnStopCapture(pResult: IMFAsyncResult): HResult;
    function OnFinishCapture(pResult: IMFAsyncResult): HResult;
    function OnSampleReady(pResult: IMFAsyncResult): HResult;

    function InitializeLoopbackCapture(): HResult;
    function CreateWAVFile(): HResult;
    function FixWAVHeader(): HResult;
    function OnAudioSampleRequested(): HResult;

    function ActivateAudioInterface(processId: DWORD; includeProcessTree: Boolean): HResult;
    function FinishCaptureAsync();

    function SetDeviceStateErrorIfFailed(hr: HResult): HResult;



  public

    constructor Create();
    destructor Destroy(); override;

    function StartCaptureAsync(processId: DWORD; includeProcessTree: Boolean; outputFileName: PCWSTR): HResult;
    function StopCaptureAsync(): HResult;

    //METHODASYNCCALLBACK(CLoopbackCapture, StartCapture, OnStartCapture);
    //METHODASYNCCALLBACK(CLoopbackCapture, StopCapture, OnStopCapture);
    //METHODASYNCCALLBACK(CLoopbackCapture, SampleReady, OnSampleReady);
    //METHODASYNCCALLBACK(CLoopbackCapture, FinishCapture, OnFinishCapture);


  end;


implementation


constructor CLoopbackCapture.Create();
begin
  inherited Create();
  m_BufferFrames := 0;
  m_SampleReadyKey := 0;
  m_dwQueueID := 0;
  m_cbHeaderSize := 0;
  m_cbDataSize := 0;
  m_outputFileName := Nil;
  m_activateResult := E_UNEXPECTED;

end;

destructor CLoopbackCapture.Destroy();
begin
  if (m_dwQueueID <> 0) then
    MFUnlockWorkQueue(m_dwQueueID);

  inherited Destroy();
end;


function CLoopbackCapture.SetDeviceStateErrorIfFailed(hr: HResult): HResult;
begin
  if Failed(hr) then
    m_DeviceState := DeviceState.Error;
  Result := hr;
end;


function CLoopbackCapture.InitializeLoopbackCapture(): HResult;
var
  dwTaskID: DWord;

begin
  dwTaskID := 0;

  // Create events for sample ready or user stop
  //RETURN_IF_FAILED(m_SampleReadyEvent.create(wil::EventOptions::None));

  // Initialize MF
  //RETURN_IF_FAILED(MFStartup(MF_VERSION, MFSTARTUP_LITE));
  InitMF();

  // Register MMCSS work queue
  if Failed(MFLockSharedWorkQueue('Capture',
                                  0,
                                  dwTaskID,
                                  m_dwQueueID)) then Exit;

  // Set the capture event work queue to use the MMCSS queue
  m_xSampleReady.SetQueueID(m_dwQueueID);

  // Create the completion event as auto-reset
  m_hActivateCompleted := EventOptions.None;

  // Create the capture-stopped event as auto-reset
  if Failed(m_hCaptureStopped = EventOptions.None) then Exit;

  Result := S_OK;
end;


function CLoopbackCapture.ActivateAudioInterface(processId: DWORD; includeProcessTree: Boolean): HResult;
var
  audioclientActivationParams: AUDIOCLIENT_ACTIVATION_PARAMS;
  ActivationType: DWord;
  activateParams: PROPVARIANT;

begin

  ActivationType := AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK;
  ProcessLoopbackParams.TargetProcessId := processId;

    Result := SetDeviceStateErrorIfFailed([&]()// -> HRESULT
        {
            //AUDIOCLIENT_ACTIVATION_PARAMS audioclientActivationParams = {};
            //audioclientActivationParams.ActivationType = AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK;
            audioclientActivationParams.ProcessLoopbackParams.ProcessLoopbackMode = includeProcessTree ?
                PROCESS_LOOPBACK_MODE_INCLUDE_TARGET_PROCESS_TREE : PROCESS_LOOPBACK_MODE_EXCLUDE_TARGET_PROCESS_TREE;
            audioclientActivationParams.ProcessLoopbackParams.TargetProcessId = processId;

            PROPVARIANT activateParams = {};
            activateParams.vt = VT_BLOB;
            activateParams.blob.cbSize = sizeof(audioclientActivationParams);
            activateParams.blob.pBlobData = (BYTE*)&audioclientActivationParams;

            wil::com_ptr_nothrow<IActivateAudioInterfaceAsyncOperation> asyncOp;
            RETURN_IF_FAILED(ActivateAudioInterfaceAsync(VIRTUAL_AUDIO_DEVICE_PROCESS_LOOPBACK, __uuidof(IAudioClient), &activateParams, this, &asyncOp));

            // Wait for activation completion
            m_hActivateCompleted.wait();

            return m_activateResult;
        }());

end;


function CLoopbackCapture.ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HResult;
begin

end;


function CLoopbackCapture.OnStartCapture(pResult: IMFAsyncResult): HResult;
begin

  Result :=  S_OK;
end;

function CLoopbackCapture.OnStopCapture(pResult: IMFAsyncResult): HResult;
begin

  Result :=  S_OK;
end;

function CLoopbackCapture.OnFinishCapture(pResult: IMFAsyncResult): HResult;
begin

  Result :=  S_OK;
end;

function CLoopbackCapture.OnSampleReady(pResult: IMFAsyncResult): HResult;
begin

  Result :=  S_OK;
end;




function CallbackAsyncCallback.GetParameters(out pdwFlags: DWord;
                                             out pdwQueue: DWord): HResult;
begin

end;

function CallbackAsyncCallback.Invoke(pAsyncResult: IMFAsyncResult): HResult;
begin

end;


end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - AudioEndPoints
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioEngineEndpoint.pas
// Kind: Pascal / Delphi unit
// Release date: 04-09-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//

//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
// 
//          The interface and type definitions for base APO functionality.
//          Requires Windows Vista or later.
// 
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: audioengineendpoint.h
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
// 
//==============================================================================
unit WinApi.CoreAudioApi.AudioEngineEndpoint;

  {$HPPEMIT '#include "audioengineendpoint.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.AudioAPOTypes,
  WinApi.WinMM.MMReg,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MmDeviceApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  // GUID_DEVINTERFACE_AUDIOENDPOINTPLUGIN: The PnP interface for audio endpoint plugin
  DEVINTERFACE_AUDIOENDPOINTPLUGIN      : TGUID = '{9F2F7B66-65AC-4FA6-8AE4-123C78B89313}';
  {$EXTERNALSYM DEVINTERFACE_AUDIOENDPOINTPLUGIN}

  // DEVPKEY_AudioEndpointPlugin_FactoryCLSID: Specifies the CLSID of the audio endpoint plugin factory
  // vartype = VT_CLSID
  DEVPKEY_AudioEndpointPlugin_FactoryCLSID   :	PROPERTYKEY = (fmtid: (D1: $12d83bd7; D2: $cf12; D3: $46be;
                                                                       D4: ($85, $40, $81, $27, $10, $d3, $2, $1c));
                                                                       pid: 1);
  {$EXTERNALSYM DEVPKEY_AudioEndpointPlugin_FactoryCLSID}

  // DEVPKEY_AudioEndpointPlugin_DataFlow: Specifies type of data flow. 0 = render, 1 = capture.
  // vartype = VT_UI4
  DEVPKEY_AudioEndpointPlugin_DataFlow       :	PROPERTYKEY = (fmtid: (D1: $12d83bd7; D2: $cf12; D3: $46be;
                                                                       D4: ($85, $40, $81, $27, $10, $d3, $2, $1c));
                                                                       pid: 2);
  {$EXTERNALSYM DEVPKEY_AudioEndpointPlugin_DataFlow}

  // DEVPKEY_AudioEndpointPlugin_PnPInterface: Specifies name of PnP Interface for audio endpoint plugin.
  // vartype = VT_LPWSTR
  DEVPKEY_AudioEndpointPlugin_PnPInterface   :	PROPERTYKEY = (fmtid: (D1: $12d83bd7; D2: $cf12; D3: $46be;
                                                                       D4: ($85, $40, $81, $27, $10, $d3, $2, $1c));
                                                                       pid: 3);
  {$EXTERNALSYM DEVPKEY_AudioEndpointPlugin_PnPInterface}


type
  // Endpoint pin type
  PEndpointConnectorType = ^EndpointConnectorType;
  EndpointConnectorType = (
    eHostProcessConnector	    = 0,
    eOffloadConnector	        = (eHostProcessConnector + 1),
    eLoopbackConnector	      = (eOffloadConnector + 1),
    eKeywordDetectorConnector	= (eLoopbackConnector + 1),
    eConnectorCount	          = (eKeywordDetectorConnector + 1)
  );
  {$EXTERNALSYM EndpointConnectorType}


  // AUDIO_ENDPOINT_SHARED_CREATE_PARAMS
  // ===================================
  // Description:
  // Contains creation parameters for the endpoint used in shared mode.
  // Remarks:
  // The Remote Desktop Services AudioEndpoint API is for use in Remote Desktop scenarios;
  // it is not for client applications.
  // Requirements:
  // Minimum supported client   Windows 7
  // Minimum supported server   Windows Server 2008 R2
  AUDIO_ENDPOINT_SHARED_CREATE_PARAMS = record
    // The size of the structure.
    u32Size: UINT32;
    // SessionId
    u32TSSessionId: UINT32;
    // Target endpoint connector type
    targetEndpointConnectorType: EndpointConnectorType;
    // The format of the endpoint.
    wfxDeviceFormat: WAVEFORMATEX;
  end;
  {$EXTERNALSYM AUDIO_ENDPOINT_SHARED_CREATE_PARAMS}
  PAudioEndpointSharedCreateParams = ^AudioEndpointSharedCreateParams;
  AudioEndpointSharedCreateParams = AUDIO_ENDPOINT_SHARED_CREATE_PARAMS;

  // AE_POSITION_FLAGS
  //==================
  // Description:
  // Each AE_CURRENT_POSITION structure has a flag associated with it.
  // The flag for each structure may be either invalid, valid, or discontinuous...
    AE_POSITION_FLAGS      = (
    // POSITION_INVALID means that the position is invalid
    // and should not be used.
    POSITION_INVALID       = 0,
    // Position is valid. However there has been
    // a disruption such as a glitch or state transition.
    // This position is not correlated with the previous one.
    POSITION_DISCONTINUOUS = 1,
    // Position is valid. The previous packet
    // and this packet aligns perfectly on the timeline.
    POSITION_CONTINUOUS    = 2,
    // The QPC value associated with this position is not accurate
    // within 300 Microseconds.
    POSITION_QPC_ERROR     = 4
  );
  {$EXTERNALSYM AE_POSITION_FLAGS}
  AePositionFlags = AE_POSITION_FLAGS;
  {$EXTERNALSYM AePositionFlags}


  // AE_CURRENT_POSITION
  //====================
  // Description:
  // Structure used to report the current frame position from the device to the clients.
  PAeCurrentPosition = ^AeCurrentPosition;
  AE_CURRENT_POSITION = record
    // Device position in frames.
    u64DevicePosition: UINT64;
    // Stream position in frames used for capture to determine starting point.
    u64StreamPosition: UINT64;
    // Current amount of padding (in frames) between the current position and the stream fill point.
    u64PaddingFrames: UINT64;
    // Translated QPC Timer value taken at the time the frame position was checked.
    hnsQPCPosition: HNSTIME;
    // Calculated value of the data rate at the point when position was set.
    f32FramesPerSecond: FLOAT32;
    // Indicates the validity of the position information.
    Flag: AE_POSITION_FLAGS;
  end;
  {$EXTERNALSYM AE_CURRENT_POSITION}
  AeCurrentPosition = AE_CURRENT_POSITION;
  {$EXTERNALSYM AeCurrentPosition}


  //  An Audio Endpoint object abstracts the audio device, audio API and any other
  //  data source/sink from the AudioProcessor. An Audio Endpoint must implement the
  //  IAudioEndpoint, IAudioEndpointRT, and one or both of IAudioInputEndpointRT and
  //  IAudioOutputEndpointRT interfaces. The clients of the processor must attach at least
  //  two Audio Endpoint objects to the Audio Processor, one for input and one for output
  //  for useful work to occur. An endpoint may only be connected to one connection, and
  //  each connection may only have one endpoint connected to it.
  //
  //  The audio endpoint object is designed to be a client-extendable object that is used
  //  to get data into or out of an audio engine instance. Data transfer is handled by
  //  connecting an audio endpoint to an audio connection.
  //  See:
  //    IAudioProcessor.AttachInputEndpointToConnection
  //    and IAudioProcessor.AttachOutputEndpointToConnection for more information on attaching
  //    endpoints to connections, and the sections on IAudioEndpoint, IAudioEndpointRT,
  //    IAudioInputEndpointRT, and IAudioOutputEndpointRT interfaces for info on the audio
  //    engine side of the endpoint interfaces.
  //
  //  The client-facing side of the endpoint objects is unspecified and may be written in any
  //  fashion convenient to the needs of the client of the endpoint. The specified interfaces
  //  must be implemented as stated above. If the endpoint is not being used to drive a graph,
  //  skip to the next paragraph. Otherwise, if the endpoint is being used to drive a processing
  //  graph, the processor will call the GetCurrentPadding interface before every processing
  //  pass. At that time, the endpoint should return the amount of data that is held in the
  //  endpoint. For an output endpoint, that would be the amount of data queued on the device.
  //  For an input endpoint, that would be the amount of data captured from the device but not
  //  yet delivered to the engine.
  //
  //  The processor will call either IAudioInputEndpointRT.GetInputDataPointer or
  //  IAudioOutputEndpointRT.GetOutputDataPointer (depending on
  //  whether the endpoint is input or output to the engine), at which time the endpoint should
  //  return a data pointer of the requested size. After processing occurs, the processor will
  //  call IAudioInputEndpointRT.ReleaseInputDataPointer or
  //  IAudioOutputEndpointRT.ReleaseOutputDataPointer, at which time the endpoint can
  //  handle the data appropriately. See the various interface calls given above for more
  //  detailed information.
  //
  //  On the client-facing side of the endpoint object, data just must be capable of being
  //  delivered to or gotten from the endpoint as appropriate. If that means that the client
  //  wants to use an interface called WriteData to write data on the endpoint, that is fine.
  //  The client side could use a callback mechanism, such that there is a method called
  //  RegisterCallback(myFuncPtr) that causes myFuncPtr to be called each time
  //  GetInputDataPointer is called (however, the myFuncPtr would have to conform to the
  //  realtime requirements if that were the case).
  //
  //  This is what is meant by a client-extendable object: the interface used by the client
  //  can be anything, as long as the interfaces required by the engine are present and operate
  //  to deliver or consume data to or from the engine as specified in the required interfaces.
  //  The "interface" to the client side doesn't even need to be a COM interface: It could be a
  //  simple set of C calls or C++ object methods.
  //
  //  The AudioProcessor uses this interface to get information about the endpoint(s)
  //  attached to it.


type

//======================= INTERFACES ===========================================


  // Interface IAudioEndpoint
  // ========================
  // Provides information to the audio engine about an audio endpoint.
  // This interface is implemented by an audio endpoint.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpoint);'}
  {$EXTERNALSYM IAudioEndpoint}
  IAudioEndpoint = interface(IUnknown)
  ['{30A99515-1527-4451-AF9F-00C5F0234DAF}']
    function GetFrameFormat(out ppFormat: WAVEFORMATEX): HResult; stdcall;
    // Description:
    //  Returns the format of the endpoint.
    // Parameters:
    //     ppFormat - [out] If S_OK, returns the format of the AudioEndpoint.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // For example, an AudioEndpoint that is abstracting an audio device
    // should return the device format. An AudioEndpoint that is abstracting a
    // file should return the file format.
    // Clients must release ppFormat by calling CoTaskMemFree.
    // This method may not be called from a real-time processing thread.

    function GetFramesPerPacket(out pFramesPerPacket: UINT32): HResult; stdcall;
    // Description:
    //  Returns the maximum frame count of the endpoint.
    // Parameters:
    //
    //     pFramesPerPacket - [out] If S_OK, returns the maximum number of frames
    //                              that may be requested in GetInputDataPointer
    //                              or GetOutputDataPointer.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method returns the maximum frame count that the endpoint in question
    // is capable of supporting. The endpoint must accept any request for input
    // or output data pointers as long as the requested number of frames is less
    // than or equal to the value returned by this function. Requesting a frame
    // count from IAudioInputEndpointRT::GetInputDataPointer or
    // IAudioOutputEndpointRT::GetOutputDataPointer that is greater than
    // the value returned by this function will result in undefined behavior (in
    // the debug build, an assert is likely; in retail, the audio data may glitch).
    // This is often referred to as the "periodicity" of the engine or pump.
    // Note:
    // This method may be not be called from a real-time processing thread.

    function GetLatency(out pLatency: HNSTIME): HResult; stdcall;
    // Description:
    //  Returns the latency of the endpoint.
    // Parameters:
    //     pLatency   - [out] If S_OK, returns the latency introduced by the endpoint.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method returns the latency that the endpoint inserts into the stream.
    // The latency for a typical buffer endpoint will be zero (i.e. there is no
    // latency writing data to a RAM buffer). The latency for a typical hardware
    // endpoint will be how ever much buffer latency the endpoint is using to stay
    // ahead of the hardware. As an example, if an endpoint is double buffering on
    // top of hardware using 5 ms buffers, the latency reported by the endpoint would
    // be 5 ms.
    //
    // Note:
    // This method may not be called from a real-time processing thread.

    function SetStreamFlags(const streamFlags: DWORD): HResult; stdcall;
    // Description:
    // Pass the IAudioClient.Initialize streamFlags to the endpoint
    // Parameters:
    //   streamFlags - the stream flags passed to IAudioClient.Initialize
    // Return values:
    //   S_OK         Successful completion.
    // Remarks: -
    // Note:
    // This method may not be called from a real-time processing thread.

    function SetEventHandle(eventHandle: THandle): HResult; stdcall;
    // Description:
    //  A WASAPI client supplies this event handle for a buffer completion
    //  callback
    // Parameters:
    //     eventHandle - event handle to be signaled when a buffer completes
    // Return values:
    //     S_OK         Successful completion.
    //     E_INVALIDARG eventHandle is Nil or invalid
    // Remarks:
    //     This is the event handle passed in IAudioClient.SetEventHandle
    // Note:
    // This method may not be called from a real-time processing thread.
  end;
  // IAudioEndpoint
  IID_IAudioEndpoint = IAudioEndpoint;
  {$EXTERNALSYM IID_IAudioEndpoint}


  // Interface IAudioProcessingObjectRT
  // ==================================
  // The AudioProcessor uses this interface to discover the current padding between read
  // and write positions.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectRT);'}
  {$EXTERNALSYM IAudioProcessingObjectRT}
  IAudioProcessingObjectRT = interface(IUnknown)
  ['{DFD2005F-A6E5-4d39-A265-939ADA9FBB4D}']
    procedure GetCurrentPadding(out pPadding: HNSTIME;
                                out pAeCurrentPosition: AeCurrentPosition); stdcall;
    // Description:
    //  Returns the amount of data queued up in the endpoint.
    // Parameters:
    //     pPadding - [out] Returns the current difference between
    //                  the read and write pointers.
    //     pAeCurrentPosition - [out] Returns the current position information
    //                  for the endpoint.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method returns the difference between the read and write positions.
    // The Audio Pump will use this information to decide how much more processing
    // is required.
    // Different AudioEndpoints will calculate this differently.
    // For example an AudioEndpoint that is abstracting a looped-render buffer
    // on a HW device will return the difference between the read and write offsets.
    //
    // Note:
    // This method may be called from a real-time processing thread.
    // The implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.

    procedure ProcessingComplete(); stdcall;
    // Description:
    //  Calls into the endpoint to indicate that a processing pass has been completed
    // Parameters:
    //     none
    // Return values:
    //     none
    // Remarks:
    // This method allows the Audio Pump to call into the Endpoint to set an event indicating
    // that a processing pass had been completed and that there is audio data to be passed to/from
    // the endpoint device. This is necessary for Standard streaming on a multiprocessor
    // system to ensure that the sequence of events prevents glitching. It may be ignored by an
    // RT thread.
    //
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    //  paged memory, or call any blocking system routines.

    function SetPinInactive(): HResult; stdcall;
    // Description:
    //  Calls into the endpoint to tell it to change the state of the underlying KS
    //  pin to inactive
    // Parameters:
    //     none
    // Return values:
    //     S_OK
    //     FAILURECODE
    // Remarks:
    // This method allows the Audio Pump to call into the Endpoint to tell it to set the state
    // of the underlying KS pin to "pause" or "not running".  If the endpoint is virtual or does not
    // otherwise sit on top of physical hardware, this method may simply return S_OK
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.

    function SetPinActive(): HResult; stdcall;
    // Description:
    //  Calls into the endpoint to tell it to change the state of the underlying KS
    //  pin to active
    // Parameters:
    //     none
    // Return values:
    //     S_OK
    //     FAILURECODE
    // Remarks:
    // This method allows the Audio Pump to call into the Endpoint to tell it to set the state
    // of the underlying KS pin to "running".  If the endpoint is virtual or does not
    // otherwise sit on top of physical hardware, this method may simply return S_OK
    //
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.
  end;
  // IAudioProcessingObjectRT
  IID_IAudioProcessingObjectRT = IAudioProcessingObjectRT;
  {$EXTERNALSYM IID_IAudioProcessingObjectRT}


  // Interface IAudioInputEndpointRT
  // ===============================
  // The AudioProcessor uses this interface to get appropriate input buffers for
  // each processing pass.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioInputEndpointRT);'}
  {$EXTERNALSYM IAudioInputEndpointRT}
  IAudioInputEndpointRT = interface(IUnknown)
  ['{8026AB61-92B2-43c1-A1DF-5C37EBD08D82}']
    procedure GetInputDataPointer(pConnectionProperty: APO_CONNECTION_PROPERTY;
                                  pAeTimeStamp: PAeCurrentPosition = Nil); stdcall;
    // Description:
    // Returns pointer to the data that will be read.
    // Parameters:
    //     pConnectionProperty - [var] A pointer to an APO_CONNECTION_PROPERTY
    //          structure. Upon entry, the fields are set as follows:
    //          pBuffer - NULL.
    //          u32ValidFrameCount - Used to determine how many frames need to be
    //          in the returned data pointer. AudioEndpoint should not cache this
    //          information. AudioProcessor can change this number depending on
    //          its processing needs.
    //          u32BufferFlags - BUFFER_INVALID.
    //          Upon exit, the endpoint should set the fields as follows:
    //          pBuffer - pointer to valid memory where either real data is or
    //          silence could be placed, depending on the value of u32BufferFlags.
    //          u32ValidFrameCount - remains unchanged.
    //          u32BufferFlags - set to BUFFER_VALID if data pointer contains
    //          valid audio data that is not silent; BUFFER_SILENT if the data
    //          pointer would contain only silent data. The data in the buffer does
    //          not actually need to be silence, but the buffer given in pBuffer
    //          must be capable of holding the u32ValidFrameCount frames worth of
    //          silence.
    //      pAeTimeStamp - [var] The time-stamp of the data that is captured.
    //                     This parameter is optional.
    // comment: solved the var issue.
    //
    // Remarks:
    // This method returns a pointer to the data (in pConnectionProperty.pBuffer) that
    // needs to be input into the engine.
    //
    // The data should be valid in the buffer until the
    // ReleaseInputDataPointer method is called. The object should return
    // the requested amount of information, inserting silence if there is no
    // valid data.
    //
    // The returned buffer pointer pConnectionProperty.pBuffer must be frame-aligned.
    //
    // Endpoints do not support the 'extraBuffer' space which may be available in
    // the APO_CONNECTION_DESCRIPTOR associated with the Connection Properties
    // passed to it.
    //
    // A pConnectionBuffer->u32ValidFrameCount of 0 is a valid request. In this case,
    // the input pointer should be valid but will not be read from. u32ValidFrameCount
    // must be <= to the frame count returned in IAudioEndpoint.GetFramesPerPacket.
    //
    // In the case where there is no or not enough valid data to satisfy the
    // u32ValidFrameCount request, a glitch should be logged with the wmi services
    // available and silence should be returned.
    //
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.

    procedure ReleaseInputDataPointer(u32FrameCount: UINT32;
                                      pDataPointer: UINT_PTR); stdcall;
    // Description:
    //  Releases the acquired data pointer.
    // Parameters:
    //     u32FrameCount - [const] Used to determine how many frames have been
    //                  consumed by the AudioProcessor. This count might not
    //                  be the same as GetInputDataPointer
    //     pDataPointer  - [const] The pointer that has been returned from
    //                  GetInputDataPointer.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method tells the AudioEndpoint that the AudioProcessor is done
    // with the input data pointer and also tells how many frames have been
    // consumed.
    //
    // For example, an AudioEndpoint that is attached to the input of the
    // AudioProcessor and that abstracts a looped buffer can advance its read
    // cursor.
    //
    // An u32FrameCount of 0 means that the client did not utilize any data
    // from the given input buffer. The u32FrameCount must be <=  to the request
    // size from GetInputDataPointer.
    //
    //  Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.

    procedure PulseEndpoint(); stdcall;
    // Description:
    //  "Pulses" the endpoint, normally by signaling a client-supplied event handle
    // Parameters:
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method is normally called at the end of a pump pass and signals
    // a client event handle.
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.
    //
    // See Also:
    //     IAudioProcessor.EndProcess
  end;
  // IAudioInputEndpointRT
  IID_IAudioInputEndpointRT = IAudioInputEndpointRT;
  {$EXTERNALSYM IID_IAudioInputEndpointRT}



  // Interface IAudioOutputEndpointRT
  // ================================
  // The AudioProcessor uses this interface to get appropriate output buffers
  // for each processing pass.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioOutputEndpointRT);'}
  {$EXTERNALSYM IAudioOutputEndpointRT}
  IAudioOutputEndpointRT = interface(IUnknown)
  ['{8FA906E4-C31C-4e31-932E-19A66385E9AA}']
    function GetOutputDataPointer(u32FrameCount: UINT32;
                                  pAeTimeStamp: AE_CURRENT_POSITION): UINT_PTR; stdcall;
    // Description:
    // Returns pointer to a buffer that will be written to.
    // Parameters:
    //     u32FrameCount - [const] Used to determine how many frames need to be
    //                  in the returned data pointer. AudioEndpoint should not
    //                  cache this information. AudioProcessor can change this
    //                  number depending on its processing needs
    //      pAeTimeStamp - [const] The time-stamp of the data that is rendered.
    //                  This parameter is optional.
    // Return values:
    //     The data pointer that needs to be played/recorded.
    // Remarks:
    // This method returns a pointer to a buffer into which to place the data
    // that is output from the engine.
    //
    // The data won’t be valid except during the duration of the
    // ReleaseOutputDataPointer method.
    //
    // The returned pointer must be frame-aligned.
    //
    // A u32FrameCount of 0 is a valid request. In this case, the output pointer
    // will not be written into, but it should be a valid pointer. u32FrameCount
    // must be <= to the frame count returned in IAudioEndpoint.GetFramesPerPacket.
    //
    // In the case where there is internally not enough
    // room to satisfy the client’s request for a buffer pointer, a glitch may
    // occur. In this case, the endpoint should still return an address which can
    // be written to, however, when ReleaseOutputDataPointer is called this
    // data will be lost. The glitch should be logged with the wmi logging
    // services available.
    //
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.

    procedure ReleaseOutputDataPointer(pConnectionProperty: PAPO_CONNECTION_PROPERTY); stdcall;
    // Description:
    // Releases the acquired data pointer.
    // Parameters:
    //     pConnectionProperty - [const] A pointer to an APO_CONNECTION_PROPERTY
    //      structure. This structure is only an input parameter and cannot be
    //      changed. The fields are set as follows:
    //      pBuffer - The pointer that has been returned from GetOutputDataPointer.
    //      u32ValidFrameCount - Used to determine how many frames have been
    //      generated by the AudioProcessor. This count might not be the same
    //      as zGetOutputDataPointer.
    //      u32BufferFlags - BUFFER_VALID or BUFFER_SILENT. If the flags are
    //      BUFFER_VALID, the pBuffer pointer contains valid audio data. If the
    //      flags are BUFFER_SILENT, the endpoint should write silence into the
    //      destination buffer where the audio data is to end up.
    // Remarks:
    // This method tells the AudioEndpoint that the AudioProcessor is done
    // with the data pointer, what time corresponds to the samples in the
    // data pointer, how many frames have been generated, and if the buffer
    // is full of valid data or silent.
    //
    // For example, an AudioEndpoint that is attached to the output of the
    // AudioProcessor and that abstracts a looped buffer can advance its
    // write cursor.
    //
    // An u32FrameCount of 0 means that the client did not generate any
    // valid data into the buffer. The u32FrameCount must be <= to the frame
    // count requested in GetOutputDataPointer. Note that the only valid data
    // in the buffer is denoted by the u32FrameCount. The endpoint should not
    // assume that all data requested was written.
    //
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.

    procedure PulseEndpoint(); stdcall;
    // Description:
    // "Pulses" the endpoint, normally by signaling a client-supplied event handle
    // Parameters: -
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method is normally called at the end of a pump pass and signals
    // a client event handle
    // Note:
    // This method may be called from a real-time processing thread. The
    // implementation of this method does not and should not block, touch
    // paged memory, or call any blocking system routines.
    //
    // See Also: IAudioProcessor.EndProcess
  end;
  // IAudioOutputEndpointRT
  IID_IAudioOutputEndpointRT = IAudioOutputEndpointRT;
  {$EXTERNALSYM IID_IAudioOutputEndpointRT}


  // Interface IAudioDeviceEndpoint
  // ==============================
  // This interface is used to initialize a Device Endpoint.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioDeviceEndpoint);'}
  {$EXTERNALSYM IAudioDeviceEndpoint}
  IAudioDeviceEndpoint = interface(IUnknown)
  ['{D4952F5A-A0B2-4cc4-8B82-9358488DD8AC}']
    function SetBuffer(MaxPeriod: HNSTIME;
                       u32LatencyCoefficient: UINT32): HResult; stdcall;
    // Description:
    //  Allocates data buffers internally.
    // Parameters:
    //     MaxPeriod - [const] Processing period of the device in
    //                      HNSTIME units.
    //     u32LatencyCoefficient - [const] The latency coefficient for this
    //          device. This coefficient will be multiplied with MaxPeriod to
    //          calculate latency. Note that each device has a minimum latency
    //          and if the coefficient is less than the minimum latency, the
    //          endpoint will apply minimum latency. Clients can obtain the
    //          actual latency using the IAudioEndpoint interface.
    //          0 as the coefficient, applies the minimum latency coefficient.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    //      Clients should not call SetBuffer when the endpoint is utilized
    //  in exclusive mode.

    function GetRTCaps(out pbIsRTCapable: BOOL): HResult; stdcall;
    // Description:
    // Returns the RT capabilities of the device endpoints.
    // Parameters:
    //     pbIsRTCapable - [out] TRUE, if the device is an RT capable device.
    // Return values:
    //     S_OK        Successful completion.

    function GetEventDrivenCapable(out pbisEventCapable: BOOL): HResult; stdcall;
    // Description:
    // Determine the endpoint's capability to be event driven.
    // Parameters:
    //     pbisEventCapable [out] pointer to a BOOL value which indicates if the
    //                        endpoint can be event driven. TRUE means it can.
    // Return values:
    //     S_OK         Successful completion.
    // Remarks:
    // Note:
    // This method may not be called from a real-time processing thread.

    function WriteExclusiveModeParametersToSharedMemory(hTargetProcess: UINT_PTR;
                                                        hnsPeriod: HNSTIME;
                                                        hnsBufferDuration: HNSTIME;
                                                        u32LatencyCoefficient: UINT32;
                                                        out pu32SharedMemorySize: UINT32;
                                                        out phSharedMemory: UINT_PTR): HResult; stdcall;
    // Description:
    // Creates and writes the exclusive mode parameters to shared memory
    // Parameters:
    //     hTargetProcess - [const] Handle of the process for which the handles
    //              will be duplicated.
    //     hnsPeriod - [const] period (packet size) to use in 100-ns units, taking into
    //              account device min period and client requested periodicity
    //     hnsBufferDuration - [const] client requested buffer duration in 100-ns units
    //     u32LatencyCoefficient - [const] device latency coefficient
    //     pu32SharedMemorySize - [out] The size of the shared memory region.
    //     phSharedMemory - [out] The handle to the shared memory region between
    //              the service and the process.
    // Return values:
    //     S_OK        Successful completion.
    // Remarks:
    // This method is used in exlusive mode to extract endpoint properties.
    // It will fail if the endpoint is fully initialized with SetBuffer.
    //
    // The shared memory region has a
    // AUDIO_ENDPOINT_EXCLUSIVE_CREATE_PARAMS structure in it. The handles
    // in the structure are duplicated for the calling process.
    //
    // Note:
    // The shared region and handles are owned by the endpoint
    // and will be released when the endpoint is released.
  end;
  // IAudioDeviceEndpoint
  IID_IAudioDeviceEndpoint = IAudioDeviceEndpoint;
  {$EXTERNALSYM IID_IAudioDeviceEndpoint}




  // Interface IAudioEndpointOffloadStreamVolume
  // ===========================================
  // This interface is used to allow per stream offloaded control of volume.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointOffloadStreamVolume);'}
  {$EXTERNALSYM IAudioEndpointOffloadStreamVolume}
  IAudioEndpointOffloadStreamVolume = interface(IUnknown)
  ['{64F1DD49-71CA-4281-8672-3A9EDDD1D0B6}']
    function GetVolumeChannelCount(out pu32ChannelCount: UINT32): HResult; stdcall;

    function SetChannelVolumes(u32ChannelCount: UINT32;
                               pf32Volumes: PFloat32;
                               u32CurveType: AUDIO_CURVE_TYPE;   // Used to be in KsMedia.h, since win 8 it's re-defined in AudioAPOTypes.h  (Tony)
                               pCurveDuration: HNSTIME): HResult; stdcall;

    function GetChannelVolumes(u32ChannelCount: UINT32;
                               {out} pf32Volumes: PFloat32): HResult; stdcall;

  end;
  // IAudioEndpointOffloadStreamVolume
  IID_IAudioEndpointOffloadStreamVolume = IAudioEndpointOffloadStreamVolume;
  {$EXTERNALSYM IID_IAudioEndpointOffloadStreamVolume}


  // Interface IAudioEndpointOffloadStreamMute
  // =========================================
  // This interface is used to allow per stream offloaded control of mute.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointOffloadStreamMute);'}
  {$EXTERNALSYM IAudioEndpointOffloadStreamMute}
  IAudioEndpointOffloadStreamMute = interface(IUnknown)
  ['{DFE21355-5EC2-40E0-8D6B-710AC3C00249}']

    function SetMute(bMuted: BOOL): HResult; stdcall;

    function GetMute(out pbMuted: BOOL): HResult; stdcall;

  end;
  // IAudioEndpointOffloadStreamMute
  IID_IAudioEndpointOffloadStreamMute = IAudioEndpointOffloadStreamMute;
  {$EXTERNALSYM IID_IAudioEndpointOffloadStreamMute}


  // Interface IAudioEndpointOffloadStreamMeter
  // ==========================================
  // This interface is used to allow per stream offloaded control of metering.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointOffloadStreamMeter);'}
  {$EXTERNALSYM IAudioEndpointOffloadStreamMeter}
  IAudioEndpointOffloadStreamMeter = interface(IUnknown)
  ['{E1546DCE-9DD1-418B-9AB2-348CED161C86}']

    function GetMeterChannelCount(out pu32ChannelCount: UINT32): HResult; stdcall;

    function GetMeteringData(u32ChannelCount: UINT32;
                             {out} pf32PeakValues: PFloat32): HResult; stdcall;

  end;
  // IAudioEndpointOffloadStreamMeter
  IID_IAudioEndpointOffloadStreamMeter = IAudioEndpointOffloadStreamMeter;
  {$EXTERNALSYM IID_IAudioEndpointOffloadStreamMeter}


  // Interface IAudioEndpointLastBufferControl
  // =========================================
  // This interface was added to allow an offload stream client to notify
  // the endpoint the last buffer has been sent only partially filled,
  // which an audio driver is required to signal a buffer completion when
  // the very last valid byte of data is read instead of doing that until
  // it reaches the end of a buffer.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointLastBufferControl);'}
  {$EXTERNALSYM IAudioEndpointLastBufferControl}
  IAudioEndpointLastBufferControl = interface(IUnknown)
  ['{F8520DD3-8F9D-4437-9861-62F584C33DD6}']

    function IsLastBufferControlSupported(): BOOL; stdcall;

    procedure ReleaseOutputDataPointerForLastBuffer(pConnectionProperty: APO_CONNECTION_PROPERTY); stdcall;

  end;
  // IAudioEndpointLastBufferControl
  IID_IAudioEndpointLastBufferControl = IAudioEndpointLastBufferControl;
  {$EXTERNALSYM IID_IAudioEndpointLastBufferControl}



  // Interface IAudioLfxControl
  // ==========================
  // This interface is used to query and enable/disable Lfx on each stream.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioLfxControl);'}
  {$EXTERNALSYM IAudioLfxControl}
  IAudioLfxControl = interface(IUnknown)
  ['{076A6922-D802-4F83-BAF6-409D9CA11BFE}']

    function SetLocalEffectsState(bEnabled: BOOL): HResult; stdcall;

    function GetLocalEffectsState(out pbEnabled: BOOL): HResult; stdcall;

  end;
  // IAudioLfxControl
  IID_IAudioLfxControl = IAudioLfxControl;
  {$EXTERNALSYM IID_IAudioLfxControl}



  // Interface IHardwareAudioEngineBase
  // ==================================
  // This interface is used to query the potential render hardware audio engine feature
  // exposed on an endpoint.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHardwareAudioEngineBase);'}
  {$EXTERNALSYM IHardwareAudioEngineBase}
  IHardwareAudioEngineBase = interface(IUnknown)
  ['{EDDCE3E4-F3C1-453a-B461-223563CBD886}']
    function GetAvailableOffloadConnectorCount(_pwstrDeviceId: PWideChar;
                                               _uConnectorId: UINT32;
                                               out _pAvailableConnectorInstanceCount: UINT32): HResult; stdcall;

    function GetEngineFormat(pDevice: IMMDevice;
                             _bRequestDeviceFormat: BOOL;
                             out _ppwfxFormat: PWAVEFORMATEX): HResult; stdcall;

    function SetEngineDeviceFormat(pDevice: IMMDevice;
                                   _pwfxFormat: WAVEFORMATEX): HResult; stdcall;

    function SetGfxState(pDevice: IMMDevice;
                         _bEnable: BOOL): HResult; stdcall;

    function GetGfxState(pDevice: IMMDevice;
                         out _pbEnable: BOOL): HResult; stdcall;

  end;
  // IHardwareAudioEngineBase
  IID_IHardwareAudioEngineBase = IHardwareAudioEngineBase;
  {$EXTERNALSYM IID_IHardwareAudioEngineBase}



  // Interface IAudioEndpointControl
  // ===============================
  // This interface is used to control an endpoint.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioEndpointControl);'}
  {$EXTERNALSYM IAudioEndpointControl}
  IAudioEndpointControl = interface(IUnknown)
  ['{C684B72A-6DF4-4774-BDF9-76B77509B653}']
    function Start(): HResult; stdcall;
    // Description:
    // Starts an endpoint immediately
    // Return values:
    //      S_OK                    Successful completion.
    // Remarks:
    // The implementation of this method may differ for various endpoints.
    // For example a RenderDevice endpoint will put the underlying pin to
    // RUN state or a Client-Side crossprocess endpoint will set the Start
    // bit in the shared memory region.
    // Note:
    // This method may not be called from a real-time processing thread.

    function Reset(): HResult; stdcall;
    // Description:
    // Resets an endpoint immediately.
    // Return values:
    //      S_OK                    Successful completion.
    // Remarks:
    // Reset discards all data that has not been processed yet.
    // The implementation of this method may differ for various endpoints.
    // For example an Output-Client-Side crossprocess endpoint will set the
    // write cursor to the beginning of the buffer and queue a reset
    // request for the Input-Server-Side crossprocess endpoint.
    // Note:
    // This method may not be called from a real-time processing thread.

    function Stop(): HResult; stdcall;
    // Description:
    // Stops an endpoint immediately.
    // Return values:
    //      S_OK                    Successful completion.
    // Remarks:
    // The implementation of this method may differ for various endpoints.
    // Note:
    // This method may not be called from a real-time processing thread.
  end;
  // IAudioEndpointControl
  IID_IAudioEndpointControl = IAudioEndpointControl;
  {$EXTERNALSYM IID_IAudioEndpointControl}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

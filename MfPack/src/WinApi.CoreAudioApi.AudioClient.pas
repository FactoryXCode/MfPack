// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPack.AudioClient.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: AudioClient buffer flags
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Jacob C.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
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
// Source: audioclient.h
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
unit WinApi.CoreAudioApi.AudioClient;

  {$HPPEMIT '#include "audioclient.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.WinMM.MMReg,
  WinApi.AudioMediaType,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioSessionTypes;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  AUDIOCLOCK_CHARACTERISTIC_FIXED_FREQ = $00000001;
  {$EXTERNALSYM AUDIOCLOCK_CHARACTERISTIC_FIXED_FREQ}
  // Description.IAudioClock characteristics (returned by IAudioClock.GetCharacteristics):
  //
  //  AUDIOCLOCK_CHARACTERISTIC_FIXED_FREQ
  //    The clock exposed by this object runs at a fixed frequency.
  //


type

	//Private
	PReferenceTime = ^REFERENCE_TIME;
	REFERENCE_TIME = LONGLONG;
	{$EXTERNALSYM REFERENCE_TIME}

type
	//-------------------------------------------------------------------------
    // Description: AudioClient buffer flags
    //
    // AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY - The data for this buffer is not correlated
    //                                         with the data from the previous buffer.
    // AUDCLNT_BUFFERFLAGS_SILENT             - This data in this buffer should be treated as silence.
    //
    // AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR    - The QPC based timestamp reading for this data
    //                                          buffer does not correlate with the data position.
    //
  PAUDCLNT_BUFFERFLAGS = ^AUDCLNT_BUFFERFLAGS;
	_AUDCLNT_BUFFERFLAGS = DWord;
	{$EXTERNALSYM _AUDCLNT_BUFFERFLAGS}
  AUDCLNT_BUFFERFLAGS = _AUDCLNT_BUFFERFLAGS;
  {$EXTERNALSYM AUDCLNT_BUFFERFLAGS}
const
  AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY	= AUDCLNT_BUFFERFLAGS($00000001);
  AUDCLNT_BUFFERFLAGS_SILENT	            = AUDCLNT_BUFFERFLAGS($00000002);
  AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR	    = AUDCLNT_BUFFERFLAGS($00000004);

type
  //-------------------------------------------------------------------------
  // Description: Flags describing the characteristics of an audio stream
  //
  //     AUDCLNT_STREAMOPTIONS_RAW - The audio stream is a 'raw' stream that bypasses
  //                                 all signal processing except for endpoint specific,
  //                                 always-on processing in the APO, driver and hardware
  //
  //     AUDCLNT_STREAMOPTIONS_MATCH_FORMAT - The client is requesting the audio engine to
  //                                 match the format proposed by the client. The audio engine
  //                                 may match this format only if the format can be accepted
  //                                 the audio driver and associated APOs.
  //
  //     AUDCLNT_STREAMOPTIONS_AMBISONICS - The client is requesting the audio client to insert  
  //                                  Ambisonics renderer and configure the pipeline to match Ambisonics format types 
  //

  PAudclntStreamoptions = ^AUDCLNT_STREAMOPTIONS;
  PAUDCLNT_STREAMOPTIONS = ^AUDCLNT_STREAMOPTIONS;
  AUDCLNT_STREAMOPTIONS        = (
    AUDCLNT_STREAMOPTIONS_NONE         = $00,
    AUDCLNT_STREAMOPTIONS_RAW          = $01,
    AUDCLNT_STREAMOPTIONS_MATCH_FORMAT = $02,
    AUDCLNT_STREAMOPTIONS_AMBISONICS   = $04
  );
  {$EXTERNALSYM AUDCLNT_STREAMOPTIONS}

    //-------------------------------------------------------------------------
    // Description: AudioClient properties structure that must precede
    //             other properties in IAudioClient.SetClientProperties
    //
    //  cbSize     - UINT32 size in bytes of this structure.
    //  bIsOffload - BOOL indicating whether or not to use offlod mode.
    //  eCategory  - AUDIO_STREAM_CATEGORY to be used.
    //  Options    - A bitfield describing the characteristics of the stream
    //

  PAudioClientProperties = ^AudioClientProperties;
  AudioClientProperties = record
    cbSize: UINT32;
    bIsOffload: BOOL;
    eCategory: AUDIO_STREAM_CATEGORY;
    Options: AUDCLNT_STREAMOPTIONS;
  end;
  {$EXTERNALSYM AudioClientProperties}



  //-------------------------------------------------------------------------
  // Description: Enumeration of ambisonics type
  //
  //      AMBISONICS_TYPE_FULL3D - Periphonics (Full 3D).
  //                               Only option supported by AMBIX_BASIC format.
  //
  PAMBISONICS_TYPE = ^AMBISONICS_TYPE;
  AMBISONICS_TYPE          = (
    AMBISONICS_TYPE_FULL3D = 0);
  {$EXTERNALSYM AMBISONICS_TYPE}

  //-------------------------------------------------------------------------
  // Description: Enumeration of ambisonics channel ordering
  //
  //      AMBISONICS_CHANNEL_ORDERING_ACN - Ambisonics channel number.
  //                                        Only option supported by AMBIX_BASIC format.
  //
  PAMBISONICS_CHANNEL_ORDERING =^AMBISONICS_CHANNEL_ORDERING;
  AMBISONICS_CHANNEL_ORDERING = (
    AMBISONICS_CHANNEL_ORDERING_ACN = 0);
  {$EXTERNALSYM AMBISONICS_CHANNEL_ORDERING}

  //-------------------------------------------------------------------------
  // Description: Enumeration of ambisonics normalization
  //
  //     AMBISONICS_NORMALIZATION_SN3D - Schmidt semi-normalized spherical harmonics.
  //                                     Only option supported by AMBIX_BASIC format.
  //     AMBISONICS_NORMALIZATION_N3D - Schmidt normalized spherical harmonics.
  //                                    N3D is supported by core decoder.
  //
  PAMBISONICS_NORMALIZATION = ^AMBISONICS_NORMALIZATION;
  AMBISONICS_NORMALIZATION = (
    AMBISONICS_NORMALIZATION_SN3D = 0,
    AMBISONICS_NORMALIZATION_N3D
  );
  {$EXTERNALSYM AMBISONICS_NORMALIZATION}

  // AMBISONICS_PARAM_VERSION_1 1
  // The AMBISONICS_PARAMS initialization structure should be completely filled out
  // and then passed into the SetData API of IAmbisonicsControl Service on IAudioClient
  // unsigned int(32) size of AMBISONICS_PARAMS
  // unsigned int(32)  version of AMBISONICS_PARAMS struct
  // unsigned int(32) ambisonics_type is the enumeration of ambisonics types
  // unsigned int(32) ambisonics_channel_ordering is the enumeration of ambisonics channel ordering
  // unsigned int(32) ambisonics_normalization is the enumeration of ambisonics normaliztion
  // unsigned int(32) ambisonics_order
  // unsigned int(32) ambisonics_num_channels
  // unsigned int(32) ambisonics_channel_map is a sequence of 32-bit unsigned integers that maps audio channels in a given audio track to ambisonic components,
  // given the defined ambisonics_channel_ordering. The sequence of channel_map values should match the channel sequence within the given audio track.

  PAMBISONICS_PARAMS = ^AMBISONICS_PARAMS;
  AMBISONICS_PARAMS = record
    u32Size: UINT32;
    u32Version: UINT32;
    u32Type: AMBISONICS_TYPE;
    u32ChannelOrdering: AMBISONICS_CHANNEL_ORDERING;
    u32Normalization: AMBISONICS_NORMALIZATION;
    u32Order: UINT32;
    u32NumChannels: UINT32;
    pu32ChannelMap: UINT32;
  end;
  {$EXTERNALSYM AMBISONICS_PARAMS}


  // Interfaces ////////////////////////////////////////////////////////////////


  // Interface IAudioClient
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioClient);'}
  {$EXTERNALSYM IAudioClient}
  IAudioClient = interface(IUnknown)
  ['{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}']
    function Initialize(ShareMode: AUDCLNT_SHAREMODE;
                        StreamFlags: DWord;
                        hnsBufferDuration: REFERENCE_TIME;
                        hnsPeriodicity: REFERENCE_TIME;
                        pFormat: WaveFormatEx;
                        {optional} AudioSessionGuid: LPCGUID): HResult; stdcall;
    // Description:
    //
    //  Initializes the audio stream by creating a connection to the Windows Audio System (WAS)
    //  using the specified format, the requested shared buffer duration and the desired audio session
    //  category.
    //
    // Parameters:
    //
    //  ShareMode - [in]
    //    Allows the application to determine how the WAS should create its connection point. The ShareMode must be one of the following:
    //
    //    AUDCLNT_SHAREMODE_SHARED - The device will be opened in shared mode and use the WAS format. pFormat must be non-NULL if this mode is
    //    specified, otherwise the call will fail.
    //
    //    AUDCLNT_SHAREMODE_EXCLUSIVE - the WAS will attempt to prepare an exclusive mode connection to the audio device in the specified format.
    //    No local stream APOs will be inserted by the WAS to aid in the creation of the connection point. pFormat must be non-NULL in this mode,
    //    otherwise the call will fail.
    //
    //  StreamFlags - [in]
    //    Optional flags that can be specified to control stream creation.
    //
    //    Possible flags are:
    //
    //    AUDCLNT_STREAMFLAGS_CROSSPROCESS - Treats a non-NULL audio session guid specified for this stream as a
    //    cross-process session, for purposes of volume and policy control. Otherwise, the default is for audio
    //    sessions to be local to the current process.
    //
    //    AUDCLNT_STREAMFLAGS_LOOPBACK - Initializes a renderer endpoint for a loopback audio application.
    //    In this mode, a capture stream will be opened on the specified renderer endpoint. Shared mode
    //    and a renderer endpoint is required. Otherwise this Initialize call will fail. On successful
    //    initialization, a capture stream will be available from this IAudioClient object.
    //
    //    AUDCLNT_STREAMFLAGS_EVENTCALLBACK - specifies that a client will supply an event handle
    //    to be signaled for "pull model" render or capture.
    //
    //  hnsBufferDuration - [in]
    //    Duration to use for the buffer that the audio application will
    //    share with the WAS, in 100-nanosecond (hns) units. The minimum allowed duration for
    //    this buffer is the WAS's processing period plus any latency introduced by the WAS stream,
    //    so if a value lower than this is specified that minimum size will be used.
    //
    //    The application is guaranteed that the underlying buffer created in response to this duration
    //    request will be at least this size, with any device latency added in. The exact shared
    //    buffer size created can be retrieved after this method was successfully called by calling
    //    the GetBufferSize() method.
    //
    //    Clients wishing to use a shared buffer that's exactly equal to the WAS's processing
    //    quantum size should specify 0 for this argument and run their processing thread using
    //    the period returned in the GetDevicePeriod() method. This would yield the lowest latency
    //    and avoid any unnecessary buffering in the audio client.
    //
    //    Applications wishing to use a larger shared buffer, with the goal of either:
    //    a) processing less often at the price of a higher latency or
    //    b) running with minimum latency and highest periodicity but filling less of the shared
    //    buffer per-pass (double-buffering, for instance),
    //    can do this by passing in the desired value for this parameter, noting the minimum
    //    size requirement.
    //
    //    If the time requested doesn't fall on a frame boundary, a duration of the next higher
    //    frame size will be used. The client must call the GetBufferSize() method after
    //    Initialize to find out the exact frame size of the shared buffer. This value will be
    //    needed during streaming to compute render buffer request sizes.
    //
    //  hnsPeriodicity - [in]
    //    The length in 100-nanosecond (hns) units of a single packet. A packet is
    //    a single unit of transfer from the client to the KS endpoint. A certain number of "frames" or
    //    "samples" will be contained in a packet, based on the number of samples / second designated in
    //    pFormat.  In a similar manner to hnsBufferDuration, if the time requested doesn't fall on a frame
    //    boundary, the duration will be rounded up to the next higher frame. This value cannot be less
    //    than the minimum periodicity reported by the GetDevicePeriod() method.
    //
    //  pFormat - [in]
    //    pointer to the application's desired audio stream format.
    //
    //  AudioSessionGuid - [in]
    //    GUID that identifies this audio session. This GUID represents an audio policy
    //    "class" and is used to indicate what type of audio this application should be associated
    //    with. Typically, it's expected that the application would either use the default class
    //    (by setting this pointer to NULL or the contents to GUID_NULL) or one of the pre-defined
    //    Windows Audio Policy classes defined in the AudioPolicy.idl public header file.
    //
    // Return values:
    //
    //    S_OK   If successful, failure otherwise.
    //    AUDCLNT_E_INITIALIZED, if already initialized.
    //    AUDCLNT_E_WRONG_ENDPOINT_TYPE, if loopback flag was set but endpoint isn't a render endpoint.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device was removed.
    //    AUDCLNT_E_BUFDURATION_PERIOD_NOT_EQUAL, if AUDCLNT_STREAMFLAGS_EVENTCALLBACK StreamFlag is
    //                                            supplied and share mode is AUDCLNT_SHAREMODE_EXCLUSIVE,
    //                                            hnsBufferDuration and hnsPeriodicity must be equal
    //    AUDCLNT_E_CPUUSAGE_EXCEEDED, if there is not enough CPU to add this stream.
    //
    // Remarks:
    //
    //  The IAudioClient methods IsFormatSupported(), GetDevicePeriod() and GetMixFormat()
    //  don't require that this method be called first. All other methods do.
    //
    //  Applications wishing to run in SharedMode should be prepared to be able to supply or accept
    //  data in the format returned by IsFormatSupported before calling Initialize. Basically, the
    //  format the application has to support is either the source format or a "closest match" format
    //  returned by IsFormatSupported. The Assumption that the mix format returned by GetMixFormat
    //  can be streamed is not always true. To stream the closest match to the mix format, call
    //  first GetMixFormat followed by IsFormatSupported passing in the mix format. If streaming the
    //  mix format is supported by the system effect, IsFormatSupported will return S_OK.
    //
    //  If AUDCLNT_STREAMFLAGS_EVENTCALLBACK is specified and the ShareMode is AUDCLNT_SHAREMODE_EXCLUSIVE,
    //  hnsBufferDuration and hnsPeriodicity must be equal.  This causes an exclusive KSEndpoint to create
    //  a double buffer pull model cycle.  The endpoint starts playing with two full buffers "A" and "B".
    //  When buffer "A" finishes playing, the client event handle is signaled, buffer "B" starts playing,
    //  and the client fills buffer "A".  Buffers "A" and "B" are both of duration hnsBufferDuration and the
    //  client thread will be woken up every hnsPeriodicity to fill.  This supports a very low latency pull
    //  model render or capture cycle.
    //
    //  If AUDCLNT_STREAMFLAGS_EVENTCALLBACK is specified and the ShareMode is AUDCLNT_SHAREMODE_SHARED,
    //  hnsBufferDuration is the client's desired buffer size and hnsPeriodicity must be 0.  At the end of
    //  each processing pass of the audio engine pump, the client's supplied event handle will be signaled.
    //
    //  In either case, specifying the AUDCLNT_STREAMFLAGS_EVENTCALLBACK stream flag requires that the client
    //  supply an event handle using the SetEventHandle() method prior to calling Start().
    //
    //  An IAudioClient object supports exactly one WAS connection which lasts for the lifetime
    //  of the IAudioClient object.
    //

    function GetBufferSize(out pNumBufferFrames: UINT32): HResult; stdcall;
    // Description:
    //
    //  Returns the maximum size of the shared buffer between the application and the WAS, in
    //  frames. This size is determined by the hnsBufferDuration parameter passed to the Initialize
    //  call. For render clients, this value determines the maximum amount of application data
    //  that can be written and stored in the shared buffer at a time. For capture clients,
    //  this determines the maximum amount of WAS capture data that can be stored for the application.
    //
    // Parameters:
    //
    //  pNumBufferFrames - [out]
    //    pointer for returning the size of the buffer shared between
    //   application and WAS, in frames.
    //
    // Return Values:
    //
    //     S_OK if successful, failure otherwise.
    //     AUDCLNT_E_NOT_INITIALIZED, if audio stream hasn't been successfully initialized.
    //     E_POINTER, if pNumBufferFrames is NULL.
    //     AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //
    // Remarks:
    //
    //  Render clients can use this value to compute the largest render buffer size that can be
    //  requested from IAudioRenderClient::GetBuffer() during each processing pass. (For more
    //  information, see the IAudioRenderClient section.)
    //

    function GetStreamLatency(out phnsLatency: REFERENCE_TIME): HResult; stdcall;
    // Description:
    //
    //  This method returns the maximum latency for the current stream and device and can be called
    //  anytime after the stream has been initialized.
    //
    // Parameters:
    //
    //  phnsLatency - [out]
    //    pointer to stream latency in 100-nanosecond (hns) units.
    //
    // Return Values:
    //
    //     S_OK if successful, failure otherwise.
    //     AUDCLNT_E_NOT_INITIALIZED if audio stream hasn't been successfully initialized.
    //     AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //
    // Remarks:
    //
    //  This method returns the maximum latency for the current stream. This method can be called
    //  once the AudioClient has been initialized.
    //
    //  The value will not change for the life of the object.
    //
    //  Render clients can use this value to compute a minimum amount of data to write during any
    //  single processing pass to prevent glitching.
    //
    //  See the IAudioRenderClient section for more details.
    //

    function GetCurrentPadding(out pNumPaddingFrames: UINT32): HResult; stdcall;
    // Description:
    //
    //  Returns the number of frames queued up to play or captured in the buffer shared between
    //  the WAS and the client.
    //
    // Parameters:
    //
    //  pNumPaddingFrames - [out]
    //    pointer for returning the number of frames currently queued
    //    to play or capture.
    //
    // Return Values:
    //
    //     S_OK if successful, failure otherwise.
    //     AUDCLNT_E_NOT_INITIALIZED if audio stream hasn't been successfully initialized.
    //     AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed,
    //
    // Remarks:
    //
    //  Render applications can use this value to compute a minimum on the amount of free space
    //  available in the render buffer to write into on the next processing pass. See the details
    //  in the IAudioRenderClient section for how to compute this value.
    //
    //  For capture applications, individual reads of capture data are done based on packet sizes
    //  (described in the IAudioCaptureClient section), but this value represent the total frame
    //  count of all the capture packets ready to read.
    //
    //  Once the audio stream has been successfully initialized, this call should always succeed.
    //

    function IsFormatSupported(ShareMode: AUDCLNT_SHAREMODE;
                               pFormat: WaveFormatEx;
                               out ppClosestMatch: PWaveFormatEx): HResult; stdcall;
    // Description:
    //
    //  Provides a way for the user to determine, prior to initialization, whether a given format
    //  will be supported or not by the AudioClient API and if so, whether the format requires
    //  exclusive mode. This is a device method which doesn't require prior audio stream
    //  initialization.
    //
    // Parameters:
    //
    //  ShareMode - [in]
    //    allows the application to determine how the WAS should create its connection point. The ShareMode must be one of the following:
    //
    //     AUDCLNT_SHAREMODE_SHARED - The device will be opened in shared mode.
    //
    //     AUDCLNT_SHAREMODE_EXCLUSIVE - the WAS will attempt to prepare an exclusive mode connection to the audio device in the specified format.
    //     No local stream APOs will be inserted by the WAS to aid in the creation of the connection point.
    //
    //  pFormat - [in]
    //    Pointer to buffer containing the application's audio format.
    //
    //  ppClosestMatch - [out] Pointer to WAVEFORMATEX pointer containing the audio format
    //      of the closest match in case the format requested cannot be supported natively.
    //      The closest match is only returned if the SharedMode parameter is
    //      AUDCLNT_SHAREMODE_SHARED and the return code is S_FALSE. No closest match can be
    //      provided for AUDCLNT_SHAREMODE_EXCLUSIVE.
    //
    // Return Values:
    //
    //     S_OK                         if format is supported.
    //     S_FALSE                      if input format is not supported but ppClosestMatch is.
    //     E_POINTER                    if ppClosestMatch is NULL & AUDCLNT_SHAREMODE_SHARED.
    //     AUDCLNT_E_INVALIDTYPE        if type isn't supported.
    //     AUDCLNT_E_DEVICEINVALIDATED  if WAS device was removed.
    //
    // Remarks:
    //
    //  This method does not require that the Initialize method be called first.
    //

    function GetMixFormat(out ppDeviceFormat: WaveFormatEx): HResult; stdcall;
    // Description:
    //
    //  Returns the current format of the WAS for this device. This is a device method
    //  which doesn't require prior audio stream initialization.
    //
    // Parameters:
    //
    //  ppDeviceFormat - [out]
    //    Address for returning a pointer to the current audio device format.
    //    This is the format the WAS will use to communicate with the device and is determined
    //    by the preferred device format set in the control panel. The memory returned should
    //    be freed by the caller using CoTaskMemFree() on the returned memory pointer.
    //
    // Return Values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device was removed.
    //
    // Remarks:
    //
    //  This method may be called at any time and will always return the same format.
    //
    //  For all cases where the format type contains > 2 channels, the WAVEFORMATEXTENSIBLE type
    //  will be used and the returned dwChannelMask field will be set correctly.
    //

    function GetDevicePeriod(out phnsDefaultDevicePeriod: REFERENCE_TIME;
                             phnsMinimumDevicePeriod: REFERENCE_TIME): HResult; stdcall;
    // Description:
    //
    //  Returns the periodicity of the WAS engine, in 100-nanosecond units.
    //  See Remarks section for more details about this value. This is a device method
    //  which doesn't require prior audio stream initialization.
    //
    // Parameters:
    //
    //  phnsDefaultDevicePeriod - [out]
    //    Returns pointer to duration of the WAS period, in
    //    100-nanosecond units. This is a device method which doesn't require prior audio
    //    stream initialization.
    //
    //    phnsMinDevicePeriod - [out]
    //    Returns pointer to duration of the minimum WAS period,
    //    in 100-nanosecond units.  This is the minimum periodicity (frames/ packet) that the
    //    driver supports. This value is the minimum periodicity that is supported in the
    //    hnsPeriodicity parameter to the IAudioClient.Initialize() call.
    //
    // Return Values:
    //
    //     S_OK if successful, failure otherwise.
    //     AUDCLNT_E_DEVICEINVALIDATED, if WAS device was removed.
    //
    // Remarks:
    //
    //  This method may be called at any time and will always return the same value.
    //

    function Start(): HResult; stdcall;
    // Description:
    //
    //  Control method used to start running the audio stream. This causes the
    //  AudioClient API to start streaming between the shared buffer and the WAS.
    //  This also starts the underlying audio clock running from its current position.
    //  If this is the first time this method was called on the stream, the AudioClient's
    //  IAudioClock position will start from 0. Otherwise, the clock will start from its
    //  last position.
    //
    // Parameters:
    //
    //  none
    //
    // Return values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_NOT_INITIALIZED if client hasn't been successfully initialized.
    //    AUDCLNT_E_NOT_STOPPED if client hasn't been first stopped.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //    AUDCLNT_E_EVENTHANDLE_NOT_SET, if event callback stream flag is specified and the event
    //                                    handle was not set with SetEventHandle
    //
    // Remarks:
    //
    //  To avoid a startup glitch for render clients, applications shouldn't call Start
    //  until the AudioClient engine has been pre-filled with data using the
    //  GetBuffer()/ReleaseBuffer() methods on the render interface.
    //

    function Stop(): HResult; stdcall;
    // Description:
    //
    //  Control method used to stop running the audio stream. This stops the data streaming
    //  between the WAS and client connection. This also stops the underlying audio clock at
    //  its current stream position. The stream position won't be reset until the Reset method
    //  is called.
    //
    // Parameters:
    //
    //  none
    //
    // Return values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_NOT_INITIALIZED if client hasn't been successfully initialized.
    //    AUDCLNT_E_STOPPED if client is already stopped.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //
    // Remarks:
    //

    function Reset(): HResult; stdcall;
    // Description:
    //
    //  Control method which resets a stopped audio stream by flushing all pending data and
    //  resetting the audio clock stream position to 0. This method will fail if called on
    //  a stream that is not stopped.
    //
    // Parameters:
    //
    //  none
    //
    // Return values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_NOT_INITIALIZED if audio stream hasn't been successfully initialized.
    //    AUDCLNT_E_NOT_STOPPED if audio stream hasn't been stopped.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //
    // Remarks:
    //

    function SetEventHandle(eventHandle: THANDLE): HResult; stdcall;
    // Description:
    //
    //  Method through which an audio client supplies an event handle used
    //  to call back for a "pull model" buffer fill.
    //
    // Parameters:
    //
    //  eventHandle - [in]
    //    event handle that will be signaled when a previous buffer has
    //    completed and that the audio client should fill the
    //    next buffer
    //
    // Return values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_NOT_INITIALIZED if audio stream hasn't been successfully initialized.
    //    AUDCLNT_E_NOT_STOPPED if audio stream hasn't been stopped.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //    AUDCLNT_E_EVENTHANDLE_NOT_EXPECTED, if Initialize was not called with the
    //                                          AUDCLNT_STREAMFLAGS_EVENTCALLBACK flag
    //
    // Remarks:
    //
    //    The event handle should be "auto reset" and the client is responsible for closing / freeing
    //    this event handle
    //

    function GetService(riid: TGUID;
                        out ppv): HResult; stdcall;
    // Description:
    //
    //  Method used to exposed additional services off the AudioClient API, including
    //  services for render and capture, clock services, volume control and audio session control services.
    //
    // Parameters:
    //
    //  riid - [in]
    //    service interface id
    //
    //  ppv - [out]
    //    address for returning service interface pointer
    //
    // Return values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_NOT_INITIALIZED if audio stream hasn't been successfully initialized.
    //    AUDCLNT_E_NOT_STOPPED if audio stream hasn't been stopped.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //
    // Remarks:
    //
    //  The services supported via the method are:
    //
    //  IAudioRenderClient
    //  IAudioCaptureClient
    //  IAudioClock
    //  IAudioSessionControl
    //  ISimpleAudioVolume
    //  IChannelAudioVolume
    //
  end;


  // Interface IAudioClient2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioClient2);'}
  {$EXTERNALSYM IAudioClient2}
  IAudioClient2 = interface(IAudioClient)
  ['{726778CD-F60A-4eda-82DE-E47610CD78AA}']
    function IsOffloadCapable(Category: AUDIO_STREAM_CATEGORY;
                              out pbOffloadCapable: BOOL): HResult; stdcall;
    // Description:
    //
    //  This method is called to find out whether the endpoint a stream is created on
    //  is capable of supporting offload
    //
    // Parameters:
    //
    //  Category - [in]
    //    an AUDIO_STREAM_CATEGORY value
    //    specifies the category for the stream
    //
    //  pbOffloadCapable - [out]
    //    pointer to a boolean value
    //      TRUE: offload capable
    //      FALSE: Not offload capable
    //
    // Return Values:
    //
    // Returns:  HRESULT code
    //
    // Remarks:
    //

    function SetClientProperties(pProperties: AudioClientProperties): HResult; stdcall;
    // Description:
    //
    //  This method is called to set an audio stream's properties.
    //
    //
    // Parameters:
    //
    //  Properties - [in]
    //    AudioClientProperties structure specifying the
    //    stream properties to set.
    //
    // Return Values:
    //
    // Returns:     S_OK: if the mode setting was done successfully
    //              AUDCLNT_E_ENDPOINT_OFFLOAD_NOT_CAPABLE: if the endpoint does not support offloading
    //              otherwise, return appropriate HRESULT failure code
    // Remarks:
    //

    function GetBufferSizeLimits(pFormat: WAVEFORMATEX;
                                 bEventDriven: BOOL;
                                 out phnsMinBufferDuration: REFERENCE_TIME;
                                 out phnsMaxBufferDuration: REFERENCE_TIME): HResult; stdcall;
    // Description:
    //
    //  Returns the buffer size limits of the hardware audio engine, in 100-nanosecond units.
    //  See Remarks section for more details about this value. This is a device method
    //  which doesn't require prior audio stream initialization.
    //
    // Parameters:
    //
    //  pFormat - [in]
    //    A pointer to the target format being queried for the buffer size limit
    //  bEventDriven - [in]
    //    Indicates whether or not the request is for the buffer limits of an
    //    event driven stream or non-event driven stream.
    //  phnsMinBufferDuration - [out]
    //    Returns pointer to minimum buffer (100-nanosecond units) that's
    //    required for the underlying  audio engine to operate, at the given
    //    format specified  in "pFormat" parameter, without frequent audio glitching.
    //  phnsMaxBufferDuration - [out]
    //    Returns pointer to maximum buffer that the underlying hardware
    //    audio engine can support for the given format specified  in "pFormat"
    //    parameter 100-nanosecond units.
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_DEVICEINVALIDATED, if a device was removed.
    //
    // Remarks:
    //
    //  This method may be called at any time but depending on the resource usage situation, it  might not return the same value
    //

  end;

  // AudioClient3ActivationParams is an optional activation parameter for IAudioClient3
  //
  // IAudioClient3 implementations log various things via ETW tracing
  // including a "context" identifier
  //
  // In situations where there are multiple active audio clients,
  // the "tracing context" identifier can ease correlation of which audio client instance belongs to which application context
  //
  // Sample app code:
  // PPROPVARIANT var;
  // PropVariantInit(&var);
  // auto p = reinterpret_cast<AudioClient3ActivationParams *>CoTaskMemAlloc(sizeof(AudioClient3ActivationParams));
  // if (nullptr == p) { ... }
  // p->tracingContextId = /* app-specific context identifier */;
  // var.vt = VT_BLOB;
  // var.blob.cbSize = sizeof(*p);
  // var.blob.pBlobData = reinterpret_cast<BYTE *>(p);
  // hr = ActivateAudioInterfaceAsync(device, __uuidof(IAudioClient3), &var, ...);
  // ...
  // PropVariantClear(&var);

  // Interface IAudioClient3
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioClient3);'}
  {$EXTERNALSYM IAudioClient3}
  IAudioClient3 = interface(IAudioClient2)
  ['{7ED4EE07-8E67-4CD4-8C1A-2B7A5987AD42}']
    function GetSharedModeEnginePeriod(pFormat: WAVEFORMATEX;
                                       out pDefaultPeriodInFrames: UINT32;
                                       out pFundamentalPeriodInFrames: UINT32;
                                       out pMinPeriodInFrames: UINT32;
                                       out pMaxPeriodInFrames: UINT32): HResult; stdcall;

    function GetCurrentSharedModeEnginePeriod(out ppFormat: WAVEFORMATEX;
                                              out pCurrentPeriodInFrames: UINT32): HResult; stdcall;

    function InitializeSharedAudioStream(StreamFlags: DWORD;
                                         PeriodInFrames: UINT32;
                                         pFormat: WAVEFORMATEX;
                                         AudioSessionGuid: TGUID): HResult; stdcall;

  end;
  IID_IAudioClient = IAudioClient;
  {$EXTERNALSYM IID_IAudioClient}



  // Interface IAudioRenderClient
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioRenderClient);'}
  {$EXTERNALSYM IAudioRenderClient}
  IAudioRenderClient = interface(IUnknown)
  ['{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}']
    function GetBuffer(const NumFramesRequested: UINT;
                       out ppData: PByte): HResult; stdcall;    // modified by Jacob C

    function ReleaseBuffer(const NumFramesWritten: UINT32;
                           const dwFlags: DWord): HResult; stdcall;

  end;
  IID_IAudioRenderClient = IAudioRenderClient;
  {$EXTERNALSYM IID_IAudioRenderClient}


  // Interface IAudioCaptureClient
  // =============================
  // Enables a client to read input data from a capture endpoint buffer.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioCaptureClient);'}
  {$EXTERNALSYM IAudioCaptureClient}
	IAudioCaptureClient = interface(IUnknown)
	['{C8ADBD64-E71E-48a0-A4DE-185C395CD317}']
		function GetBuffer(out ppData: PByte;
                       out pNumFramesToRead: UINT32;
                       out pdwFlags: PAUDCLNT_BUFFERFLAGS;  // DWord
                       out pu64DevicePosition: Int64;
                       out pu64QPCPosition: Int64): HResult; stdcall;
    // Description:
    //
    //  Returns a pointer to the shared render buffer of the requested size for the
    //  application to write its output data into for rendering to the WAS.
    //
    // Parameters:
    //
    //  NumFramesRequested - [in]
    //    Number of frames requested in the returned data pointer.
    //
    //  ppData - [out]
    //    If call was successful, this address contains a pointer to a data buffer of
    //    requested size, which application can write into.
    //
    // Return Values:
    //
    //     S_OK if successful, error otherwise.
    //     AUDCLNT_E_BUFFERTOOLARGE, if NumFramesRequested > (GetBufferSize() - GetCurrentPadding())
    //     AUDCLNT_E_OUTOFORDER, if called while a previous IAudioRenderClient.GetBuffer() is still
    //     in effect.
    //     AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed,
    //     E_POINTER, if ppData is NULL.
    //
    // Remarks:
    //
    //  Maximum buffer size to request - The application shouldn't ask for a larger buffer than
    //  the size currently available in the requested shared buffer region and if this value is
    //  exceeded GetBuffer() will fail. The total available space at any given time can be
    //  determined by calling the GetCurrentPadding() method and subtracting that frame count
    //  size from the shared buffer size (returned in the GetBufferSize() method).
    //
    //  Minimum buffer size to request - As far as determining the minimum amount of data to write
    //  per-processing pass, it is left up to the application to ensure that it writes enough data
    //  to prevent glitching. However, the minimum recommended size for a buffer request to prevent
    //  glitching is: latency + device period.
    //
    //  The client is required to serialize the GetBuffer()/ReleaseBuffer() sequence of calls.
    //  For instance, consecutive calls to either GetBuffer() or ReleaseBuffer() aren't permitted and
    //  will fail.
    //
    // 'pFormat' in the annotation below refers to the WAVEFORMATEX structure used to
    // initialize IAudioClient.
    //

		function GetNextPacketSize(out pNumFramesInNextPacket: UINT32): HResult; stdcall;
    // Description:
    //
    //  Returns the number of frames in the next capture buffer packet. Capture applications must read in frames on a packet-by-packet basis.
    //
    // Parameters:
    //
    //  pNumFramesInNextPacket - [out]
    //    Pointer for returning the number of frames in the next capture packet.
    //    When all capture packets have been read this value will be 0.
    //
    // Return values:
    //
    //    S_OK if successful, failure otherwise.
    //    AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //    E_POINTER, if pNumFramesInNextPacket is NULL.
    //
    // Remarks:
    //
    //  This method returns the size of the next capture packet. To determine the size of all
    //  the captured data currently in the shared buffer (accounting for all current packets)
    //  use the IAudioClient::GetCurrentPadding() method.
    //

		function ReleaseBuffer(NumFramesRead: UINT32): HResult; stdcall;
    // Description:
    //
    //  Releases the render data buffer acquired in the GetBuffer call.
    //
    // Parameters:
    //
    //  NumFramesWritten - [in]
    //     Count of application frames written into the render buffer. Must be
    //     less than or equal to the requested amount.
    //
    //  dwFlags - [in]
    //     This value is used to allow the application to flag the return buffer specially,
    //     if necessary.
    //     The following flags are supported on render buffers:
    //
    //          AUDCLNT_BUFFERFLAGS_SILENT - buffer data should be treated as silence. This flag
    //          frees a render client from needing to explicitly write silence data to the output
    //          buffer. Note that a loopback client reading capture data from this render buffer
    //          shouldn't be required to do any silence filling.
    //
    //     Otherwise, the dwFlags value must be set to 0.
    //
    //
    // Return values:
    //
    //      S_OK if successful, error otherwise.
    //      E_FAIL, if FramesWritten > count requested in previous GetBuffer() call.
    //      E_INVALIDARG, if invalid flag was used.
    //      AUDCLNT_E_OUTOFORDER, if previous IAudioRenderClient streaming call wasn't GetBuffer().
    //      AUDCLNT_E_DEVICEINVALIDATED, if WAS device format was changed or device was removed.
    //
    // Remarks:
    //      Please note: This function is a "finalizer". As such,
    //      except for invalid argument errors as called out above,
    //      this function has no valid failure modes.
    //

	end;
  IID_IAudioCaptureClient = IAudioCaptureClient;
  {$EXTERNALSYM IID_IAudioCaptureClient}


  // Interface IAudioClock
  // =====================
  //>=Vista
  //Enables a client to monitor a stream's data rate and the current position in the stream.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioClock);'}
  {$EXTERNALSYM IAudioClock}
	IAudioClock = interface(IUnknown)
	['{CD63314F-3FBA-4a1b-812C-EF96358728E7}']
    function GetFrequency(out pu64Frequency: Int64): HResult; stdcall;
    // Description:
    //
    //  Returns the frequency for the clock
    //
    // Parameters:
    //
    //  pu64Frequency - [out]
    //    If S_OK, returns the clock frequency.
    //
    // See Also:
    //
    //  IAudioClock.GetPosition
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     E_POINTER, if pu64Frequency is NULL.
    //
    // Remarks:
    //
    //  Reports the average frequency in units of the clock position.
    //  For example, for an IAudioClock object that is reporting the clock position in bytes,
    //  this method will return the average number of bytes per second consumed.
    //  See the description below for GetPosition, for how to
    //  use this value to compute the clock position in seconds.
    //

    function GetPosition(out pu64Position: Int64;
                         out pu64QPCPosition: Int64): HResult; stdcall;
    // Description:
    //
    //  Returns the current clock position
    //
    // Parameters:
    //
    //  pu64Position - [out]
    //    If S_OK, returns the clock position in ticks-per-second. To compute a time position from
    //    this value, the following formula must be used:
    //
    //      Clock Position (in seconds) = *pu64Position / *pu64Frequency
    //
    //      where *pu64Position is the value returned from this GetPosition call
    //      and *pu64Frequency is the value returned from IAudioClock.GetFrequency (called at the same time).
    //
    //      NOTE: If the clock is fixed frequency (i.e. supports the AUDIOCLOCK_CHARACTERISTIC_FIXED_FREQ flag
    //            via the  GetCharacteristics call), the GetFrequency call need only be done once up front and the returned value
    //            used for the life of the clock's position computations.
    //
    //  pu64QPCPosition - [out]
    //    If S_OK, returns the QueryPerformanceCounter position corresponding to the
    //    position argument. This value may be NULL if a correlated system position isn't needed.
    //
    // See Also:
    //
    //  IAudioClock.GetFrequency
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     E_POINTER, if pu64Position is NIL.
    //
    // Remarks:
    //
    //  The unit of the clock position depends on the outcome of the
    //  GetFrequency call. For example, if the frequency is average number of
    //  bytes, then the clock position will be byte count.
    //

		function GetCharacteristics(out pdwCharacteristics: DWord): HResult; stdcall;
    // Description:
    //
    //  Returns the current clock position
    //
    // Parameters:
    //
    //  pdwCharacteristics - [out]
    //    If S_OK, returns a DWORD value containing 0 or any supported clock characteristics flags.
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     E_POINTER, if pdwCharacteristics is NULL.
    //
    // Remarks:
    //
    //  Returns clock characteristics. Supported characteristics (see AUDIOCLOCK_CHARACTERISTIC flags description for more info):
    //
    //  AUDIOCLOCK_CHARACTERISTIC_FIXED_FREQUENCY - if set, the clock frequency need only be obtained once for use in
    //  clock position computations.
    //

	end;
  IID_IAudioClock = IAudioClock;
  {$EXTERNALSYM IID_IAudioClock}


  // Interface IAudioClock2
  // ======================
  // >= Windows 7
  // Is used to get the current device position.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioClock2);'}
  {$EXTERNALSYM IAudioClock2}
  IAudioClock2 = interface(IUnknown)
	['{6f49ff73-6727-49ac-a008-d98cf5e70048}']
		function GetDevicePosition(out DevicePosition: UINT64;
                               out QPCPosition: UINT64): HResult; stdcall;
    // Description:
    //
    //  Returns the current device position
    //
    // Parameters:
    //
    //  DevicePosition - [out]
    //    If S_OK, returns the device position in frames.
    //
    //  QPCPosition - [out]
    //    If S_OK, returns the QueryPerformanceCounter position corresponding to the
    //    position argument.
    //    This value may be NULL if a correlated system position isn't needed.
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     E_POINTER, if pu64Position is NULL.
    //
    // Remarks:
    //
    //    This method returns a "raw" position, directly from the hardware.
    //    It is not capped or adjusted. Special care should be taken interpreting the
    //    position because the sampling rate of the device endpoint may be different from
    //    the mix format used by the client.
    //
	end;
  IID_IAudioClock2 = IAudioClock2;
  {$EXTERNALSYM IID_IAudioClock2}


  // Interface IAudioClockAdjustment
  // ===============================
  // >= Windows 7
  // Is used to adjust the sample rate of a stream.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioClockAdjustment);'}
  {$EXTERNALSYM IAudioClockAdjustment}
	IAudioClockAdjustment = interface(IUnknown)
	['{f6e4c0a0-46d9-4fb8-be21-57a3ef2b626c}']
		//Sets the sample rate of a stream, in frames per second.
		function SetSampleRate(flSampleRate: Float): HResult; stdcall;
    // Description:
    //
    //  Sets the current sample rate
    //
    // Parameters:
    //
    //  flSampleRate - [in]
    //    Sets the new sample rate for the audio stream.
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     E_INVALIDARG, if the IAudioClient wasn't initialized with
    //                  the AUDCLNT_STREAMFLGS_RATEADJUST flag.
    //
    // Remarks:
    //      The new sample rate adjustment will take place on the
    //      processing pass that follows the call to SetSampleRate().
    //
	end;
  IID_IAudioClockAdjustment = IAudioClockAdjustment;
  {$EXTERNALSYM IID_IAudioClockAdjustment}


  // Interface ISimpleAudioVolume
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISimpleAudioVolume);'}
  {$EXTERNALSYM ISimpleAudioVolume}
  ISimpleAudioVolume = interface(IUnknown)
	['{87CE5498-68D6-44E5-9215-6DA47EF883D8}']
		function SetMasterVolume(fLevel: Single;
                             EventContext: LPCGUID): HResult; stdcall;
    // Description:
    //
    //  Set the master volume of the current audio client.
    //
    // Parameters:
    //
    //  fLevel - [in]
    //    New amplitude for the audio stream.
    //  EventContext - [in]
    //    Context passed to notification routine, GUID_NULL if NULL.
    //
    // See Also:
    //
    //  ISimpleAudioVolume.GetMasterVolume
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //
    //

    function GetMasterVolume(out pfLevel: Single): HResult; stdcall;
    // Description:
    //
    //  Get the master volume of the current audio client.
    //
    // Parameters:
    //
    //  pfLevel - [out]
    //    New amplitude for the audio stream.
    //
    // See Also:
    //
    //  ISimpleAudioVolume.SetMasterVolume
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //

    function SetMute(bMute: BOOL;
                     EventContext: LPCGUID): HResult; stdcall;
    // Description:
    //
    //  Set the mute state of the current audio client.
    //
    // Parameters:
    //
    //  bMute - [in]
    //    New mute for the audio stream.
    //  EventContext - [in]
    //    Context passed to notification routine, GUID_NULL if NULL.
    //
    // See Also:
    //
    //  ISimpleAudioVolume.SetMute
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

    function GetMute(out pbMute: BOOL): HResult; stdcall;
    // Description:
    //
    //  Get the mute state of the current audio client.
    //
    // Parameters:
    //
    //  bMute - [out]
    //    Current mute for the audio stream.
    //
    // See Also:
    //
    //  ISimpleAudioVolume.GetMute
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //
  end;
  IID_ISimpleAudioVolume = ISimpleAudioVolume;
  {$EXTERNALSYM IID_ISimpleAudioVolume}


  // Interface IAudioStreamVolume
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioStreamVolume);'}
  {$EXTERNALSYM IAudioStreamVolume}
	IAudioStreamVolume = interface(IUnknown)
	['{93014887-242D-4068-8A15-CF5E93B90FE3}']
		function GetChannelCount(out pdwCount: UINT32): HResult; stdcall;
    // Description:
    //
    //  Get the channel count for the audio stream.
    //
    // Parameters:
    //
    //  pdwCount - [out]
    //    The current channel count.
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

		function SetChannelVolume(dwIndex: UINT32;
                              fLevel: Single): HResult; stdcall;
    // Description:
    //
    //  Set the volume for a particular channel on the current stream.
    //
    // Parameters:
    //
    //  dwIndex - [in]
    //    The channel # to set
    //  fLevel -  [in]
    //    The volume level for that channel
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //

		function GetChannelVolume(dwIndex: UINT32;
                              out pfLevel: Single): HResult; stdcall;
    // Description:
    //
    //  Get the volume for a particular channel on the current stream.
    //
    // Parameters:
    //
    //  dwIndex - [in]
    //    The channel # to get
    //  pfLevel -  [out]
    //    The volume level for that channel
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

    function SetAllVolumes(const dwCount: UINT32;
                           pfVolumes: PFloat): HResult; stdcall;

    // Description:
    //
    //  Set the volume for all audio channels.
    //
    // Parameters:
    //
    //  dwCount - [in]
    //    Number of entries in the pfVolumes array.  Must
    //                      be the same as IStreamAudioVolume.GetChannelCount
    //  pfVolumes - [in]
    //    Array of volumes.
    //
    // See Also:
    //
    //  IStreamAudioVolume.GetAllVolumes
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

    function GetAllVolumes(const dwCount: UINT32;
                           out pfVolumes: PFLOAT): HResult; stdcall;
    // Description:
    //
    //  Get the volume for all audio channels.
    //
    // Parameters:
    //
    //  dwCount - [in]
    //    Number of entries in the pfVolumes array.
    //    Must be the same as IStreamAudioVolume.GetChannelCount
    //  pfVolumes - [out]
    //    Array of volumes filled in with the current channel volumes.
    //
    // See Also:
    //
    //  IStreamAudioVolume.SetAllVolumes
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
	end;
  IID_IAudioStreamVolume = IAudioStreamVolume;
  {$EXTERNALSYM IID_IAudioStreamVolume}


  // Interface IChannelAudioVolume
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IChannelAudioVolume);'}
  {$EXTERNALSYM IChannelAudioVolume}
	IChannelAudioVolume = interface(IUnknown)
	['{1C158861-B533-4B30-B1CF-E853E51C59B8}']
    function GetChannelCount(out pdwCount: UINT32): HResult; stdcall;
    // Description:
    //
    //  Get the channel count for the audio session associated with
    //  this client.
    //
    // Parameters:
    //
    //  pdwCount - [out]
    //    The current channel count.
    //
    // See Also:
    //
    //  IChannelAudioVolume.GetChannelCount
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //

    function SetChannelVolume(const dwIndex: UINT32;
                              const fLevel: Single;
                              EventContext: LPCGUID): HResult; stdcall;
    // Description:
    //
    //  Set the volume for a particular channel on the audio session
    //  associated with this client.
    //
    // Parameters:
    //
    //  dwIndex - [in]
    //    The channel # to set.
    //  fLevel -  [in]
    //    The volume level for that channel.
    //  EventContext - [in]
    //    Context passed to notification routine, GUID_NULL if NULL.
    //
    // See Also:
    //
    //  IChannelAudioVolume.GetChannelVolume
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

    function GetChannelVolume(const dwIndex: UINT32;
                              out pfLevel: FLOAT): HResult; stdcall;
    // Description:
    //
    //  Get the volume for a particular channel.
    //
    // Parameters:
    //
    //  dwIndex - [in]
    //    The channel # to get.
    //  pfLevel -  [out]
    //    The volume level for that channel
    //
    // See Also:
    //
    //  IChannelAudioVolume.GetChannelVolume
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

    function SetAllVolumes(const dwCount: UINT32;
                           pfVolumes: PFLOAT;
                           EventContext: LPCGUID): HResult; stdcall;
    // Description:
    //
    //  Set the volume for all audio channels.
    //
    // Parameters:
    //
    //  dwCount - [in]
    //    Number of entries in the pfVolumes array.
    //    Must be the same as IChannelAudioVolume.GetChannelCount
    //  pfVolumes - [in]
    //    Array of volumes.
    //  EventContext - [in]
    //    Context passed to notification routine, GUID_NULL if NULL.
    //
    // See Also:
    //
    //  IChannelAudioVolume.GetAllVolumes
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

    function GetAllVolumes(dwCount: UINT32;
                           {out} pfVolumes: PFLOAT): HResult; stdcall;
    // Description:
    //
    //  Get the volume for all audio channels.
    //
    // Parameters:
    //
    //  dwCount - [in]
    //    Number of entries in the pfVolumes array.
    //    Must be the same as IChannelAudioVolume.GetChannelCount
    //  pfVolumes - [out]
    //    Array of volumes filled in with the current channel volumes.
    //
    // See Also:
    //
    //  IChannelAudioVolume.SetAllVolumes
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //

	end;
  IID_IChannelAudioVolume = IChannelAudioVolume;
  {$EXTERNALSYM IID_IChannelAudioVolume}


  // Interface IAudioAmbisonicsControl
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioAmbisonicsControl);'}
  {$EXTERNALSYM IAudioAmbisonicsControl}
  IAudioAmbisonicsControl = interface(IUnknown)
	['{28724C91-DF35-4856-9F76-D6A26413F3DF}']
    //-------------------------------------------------------------------------
    // Description:
    //
    //  Sets the Ambisonics Metadata for the audio stream.
    //
    // Parameters:
    //
    //     cbAmbisonicsParams - [in] complete size of the buffer point by AMBISONICS_PARAMS.
    //     pAmbisonicsParams  - [in] buffer containing metadata.
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //
    function SetData(pAmbisonicsParams: AMBISONICS_PARAMS;
                     cbAmbisonicsParams: UINT32): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    // Description:
    //
    //  Enables/Disables head tracking
    //
    // Parameters:
    //
    //     bEnableHeadTracking - [in] bool to decide whether the Audio Ambisonics renderer does head tracking.
    //                            Default value is set to TRUE  if endpoint like HMD is capable of head tracking otherwise FALSE
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //
    function SetHeadTracking(bEnableHeadTracking: BOOL): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    // Description:
    //
    //  Gets if head tracking is enabled or not
    //
    // Parameters:
    //
    //     pbEnableHeadTracking - [out] bool indicating if Audio Ambisonic renderer does head tracking.
    //                             Default value is set to TRUE if endpoint like HMD is capable of head tracking otherwise FALSE
    //
    // Return values:
    //
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //
    function GetHeadTracking(pbEnableHeadTracking: BOOL): HRESULT; stdcall;


    //-------------------------------------------------------------------------
    // Description:
    //
    //  Sets rotation values in Quaternion  
    //  Quaternion represents orientation and rotation of objects in 3D 
    //  These Rotation values corresponds to head rotation/head tracking.
    //  For e.g. a user panning 360 degree video to 90 degrees on the right will hear the audio as if the user’s head is rotated to 90 degrees on the right. 
    //  This method will return error AUDCLNT_E_HEADTRACKING_ENABLED if the SetHeadTracking is set to TRUE (default is TRUE) i.e., the user will have to first call SetHeadTracking(FALSE) before using SetRotation. 
    //
    // Parameters:
    //
    //    X - [in] X component of Quaternion
    //    Y - [in] Y component of Quaternion
    //    Z - [in] Z component of Quaternion
    //    W - [in] W component of Quaternion
    //
    // Return values:
    //
    //     S_OK        Successful completion.
    //     OTHER       Other error.
    //
    //
    function SetRotation(X: FLOAT;
                         Y: FLOAT;
                         Z: FLOAT;
                         W: FLOAT): HRESULT; stdcall;

  end;
  IID_IAudioAmbisonicsControl = IAudioAmbisonicsControl;
  {$EXTERNALSYM IID_IAudioAmbisonicsControl}


const
  // error codes
	FACILITY_AUDCLNT                        = $889;
	{$EXTERNALSYM FACILITY_AUDCLNT}

  // Since XE2 you have to hardcode this.

  AUDCLNT_E_NOT_INITIALIZED               = $88890001;  //AUDCLNT_ERR($001);
  {$EXTERNALSYM AUDCLNT_E_NOT_INITIALIZED}
	AUDCLNT_E_ALREADY_INITIALIZED           = $88890002;  //AUDCLNT_ERR($002);
	{$EXTERNALSYM AUDCLNT_E_ALREADY_INITIALIZED}
  AUDCLNT_E_WRONG_ENDPOINT_TYPE           = $88890003;  //AUDCLNT_ERR($003);
  {$EXTERNALSYM AUDCLNT_E_WRONG_ENDPOINT_TYPE}
  AUDCLNT_E_DEVICE_INVALIDATED            = $88890004;  //AUDCLNT_ERR($004);
  {$EXTERNALSYM AUDCLNT_E_DEVICE_INVALIDATED}
	AUDCLNT_E_NOT_STOPPED                   = $88890005;  //AUDCLNT_ERR($005);
	{$EXTERNALSYM AUDCLNT_E_NOT_STOPPED}
  AUDCLNT_E_BUFFER_TOO_LARGE              = $88890006;  //AUDCLNT_ERR($006);
  {$EXTERNALSYM AUDCLNT_E_BUFFER_TOO_LARGE}
  AUDCLNT_E_OUT_OF_ORDER                  = $88890007;  //AUDCLNT_ERR($007);
  {$EXTERNALSYM AUDCLNT_E_OUT_OF_ORDER}
  AUDCLNT_E_UNSUPPORTED_FORMAT            = $88890008;  //AUDCLNT_ERR($008);
  {$EXTERNALSYM AUDCLNT_E_UNSUPPORTED_FORMAT}
  AUDCLNT_E_INVALID_SIZE                  = $88890009;  //AUDCLNT_ERR($009);
  {$EXTERNALSYM AUDCLNT_E_INVALID_SIZE}
  AUDCLNT_E_DEVICE_IN_USE                 = $8889000a;  //AUDCLNT_ERR($00A);
  {$EXTERNALSYM AUDCLNT_E_DEVICE_IN_USE}
  AUDCLNT_E_BUFFER_OPERATION_PENDING      = $8889000b;  //AUDCLNT_ERR($00B);
  {$EXTERNALSYM AUDCLNT_E_BUFFER_OPERATION_PENDING}
  AUDCLNT_E_THREAD_NOT_REGISTERED         = $8889000c;  //AUDCLNT_ERR($00C);
  {$EXTERNALSYM AUDCLNT_E_THREAD_NOT_REGISTERED}
  AUDCLNT_E_EXCLUSIVE_MODE_NOT_ALLOWED    = $8889000e;  //AUDCLNT_ERR($00E);
  {$EXTERNALSYM AUDCLNT_E_EXCLUSIVE_MODE_NOT_ALLOWED}
  AUDCLNT_E_ENDPOINT_CREATE_FAILED        = $8889000f;  //AUDCLNT_ERR($00F);
  {$EXTERNALSYM AUDCLNT_E_ENDPOINT_CREATE_FAILED}
  AUDCLNT_E_SERVICE_NOT_RUNNING           = $88890010;  //AUDCLNT_ERR($010);
  {$EXTERNALSYM AUDCLNT_E_SERVICE_NOT_RUNNING}
  AUDCLNT_E_EVENTHANDLE_NOT_EXPECTED      = $88890011;  //AUDCLNT_ERR($011);
  {$EXTERNALSYM AUDCLNT_E_EVENTHANDLE_NOT_EXPECTED}
  AUDCLNT_E_EXCLUSIVE_MODE_ONLY           = $88890012;  //AUDCLNT_ERR($012);
  {$EXTERNALSYM AUDCLNT_E_EXCLUSIVE_MODE_ONLY}
  AUDCLNT_E_BUFDURATION_PERIOD_NOT_EQUAL  = $88890013;  //AUDCLNT_ERR($013);
  {$EXTERNALSYM AUDCLNT_E_BUFDURATION_PERIOD_NOT_EQUAL}
  AUDCLNT_E_EVENTHANDLE_NOT_SET           = $88890014;  //AUDCLNT_ERR($014);
  {$EXTERNALSYM AUDCLNT_E_EVENTHANDLE_NOT_SET}
  AUDCLNT_E_INCORRECT_BUFFER_SIZE         = $88890015;  //AUDCLNT_ERR($015);
  {$EXTERNALSYM AUDCLNT_E_INCORRECT_BUFFER_SIZE}
  AUDCLNT_E_BUFFER_SIZE_ERROR             = $88890016;  //AUDCLNT_ERR($016);
  {$EXTERNALSYM AUDCLNT_E_BUFFER_SIZE_ERROR}
  AUDCLNT_E_CPUUSAGE_EXCEEDED             = $88890017;  //AUDCLNT_ERR($017);
  {$EXTERNALSYM AUDCLNT_E_CPUUSAGE_EXCEEDED}
  AUDCLNT_E_BUFFER_ERROR                  = $88890018;  //AUDCLNT_ERR($018);
  {$EXTERNALSYM AUDCLNT_E_BUFFER_ERROR}
  AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED       = $88890019;  //AUDCLNT_ERR($019);
  {$EXTERNALSYM AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED}
  AUDCLNT_E_INVALID_DEVICE_PERIOD         = $88890020;  //AUDCLNT_ERR($020);
  {$EXTERNALSYM AUDCLNT_E_INVALID_DEVICE_PERIOD}
  AUDCLNT_E_INVALID_STREAM_FLAG           = $88890021;  //AUDCLNT_ERR($021);
  {$EXTERNALSYM AUDCLNT_E_INVALID_STREAM_FLAG}
  AUDCLNT_E_ENDPOINT_OFFLOAD_NOT_CAPABLE  = $88890022;  //AUDCLNT_ERR($022)
  {$EXTERNALSYM AUDCLNT_E_ENDPOINT_OFFLOAD_NOT_CAPABLE}
  AUDCLNT_E_OUT_OF_OFFLOAD_RESOURCES      = $88890023;  //AUDCLNT_ERR($023)
  {$EXTERNALSYM AUDCLNT_E_OUT_OF_OFFLOAD_RESOURCES}
  AUDCLNT_E_OFFLOAD_MODE_ONLY             = $88890024;  //AUDCLNT_ERR($024)
  {$EXTERNALSYM AUDCLNT_E_OFFLOAD_MODE_ONLY}
  AUDCLNT_E_NONOFFLOAD_MODE_ONLY          = $88890025;  //AUDCLNT_ERR($025)
  {$EXTERNALSYM AUDCLNT_E_NONOFFLOAD_MODE_ONLY}
  AUDCLNT_E_RESOURCES_INVALIDATED         = $88890026;  //AUDCLNT_ERR($026)
  {$EXTERNALSYM AUDCLNT_E_RESOURCES_INVALIDATED}
  AUDCLNT_E_RAW_MODE_UNSUPPORTED          = $88890027;  //AUDCLNT_ERR($027)
  {$EXTERNALSYM AUDCLNT_E_RAW_MODE_UNSUPPORTED}
  AUDCLNT_E_ENGINE_PERIODICITY_LOCKED     = $88890028;  //AUDCLNT_ERR($028)
  {$EXTERNALSYM AUDCLNT_E_ENGINE_PERIODICITY_LOCKED}
  AUDCLNT_E_ENGINE_FORMAT_LOCKED          = $88890029;  //AUDCLNT_ERR($029)
  {$EXTERNALSYM AUDCLNT_E_ENGINE_FORMAT_LOCKED}

  AUDCLNT_S_BUFFER_EMPTY                  = $8890001;  //AUDCLNT_SUCCESS($001);
  {$EXTERNALSYM AUDCLNT_S_BUFFER_EMPTY}
  AUDCLNT_S_THREAD_ALREADY_REGISTERED     = $8890002;  //AUDCLNT_SUCCESS($002);
  {$EXTERNALSYM AUDCLNT_S_THREAD_ALREADY_REGISTERED}
  AUDCLNT_S_POSITION_STALLED              = $8890003;  //AUDCLNT_SUCCESS($003);
  {$EXTERNALSYM AUDCLNT_S_POSITION_STALLED}



  // Additional Prototypes for ALL interfaces

  // Remarks: HRESULTs are signed 4-byte integers.
  //          AUDCLNT_ERR is a macro that returns an integer of the format $88890000 + x.
  //          Example: AUDCLNT_ERR($001) will result in $88890001.
  function AUDCLNT_ERR(n: Longint): HRESULT;
  function AUDCLNT_SUCCESS(n: Longint): HRESULT;

  // End of Additional Prototypes


implementation
  //Implement Additional functions here.


  //ERROR HANDLING
  function AUDCLNT_ERR(n: Longint): HRESULT;
  begin
    Result:= MAKE_HRESULT(SEVERITY_ERROR, FACILITY_AUDCLNT, n);
  end;

  function AUDCLNT_SUCCESS(n: Longint): HRESULT;
  begin
    Result:= MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_AUDCLNT, n);
  end;


end.

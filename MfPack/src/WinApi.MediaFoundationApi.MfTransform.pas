// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfTransform.pas
// Kind: Pascal / Delphi unit
// Release date: 11-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Define the interfaces for Media Foundation Transforms.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran).
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
//         Delphi : The IUnknown entries of functions should be casted like this:
//         IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.18362.0 (19H1)
//
// Todo: -
//
//==============================================================================
// Source: MFTransform.h
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
unit WinApi.MediaFoundationApi.MfTransform;

  {$HPPEMIT '#include "MFTransform.h"'}

interface

uses

  {WinApi}
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects;


  {$WEAKPACKAGEUNIT ON}
  {$ALIGN ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$WARN SYMBOL_PLATFORM OFF}

  {$I 'WinApiTypes.inc'}

// BaseTsd.h:
// #define MAXULONGLONG ((ULONGLONG)~((ULONGLONG)0))
// #define MINLONGLONG ((LONGLONG)~MAXLONGLONG)

const

  MAXLONGLONG    = 9223372036854775807;
  {$EXTERNALSYM MAXLONGLONG}
  MINLONGLONG    = -9223372036854775808;
  {$EXTERNALSYM MINLONGLONG}


  // MF_DMFT_FRAME_BUFFER_INFO
  // Data type: BLOB
  // Always part of IMFDeviceTransformCallback.OnBufferSent pCallbackAttributes
  // 396CE1C9-67A9-454C-8797-95A45799D804
  MF_DMFT_FRAME_BUFFER_INFO            : TGUID = '{396CE1C9-67A9-454C-8797-95A45799D804}';
  {$EXTERNALSYM MF_DMFT_FRAME_BUFFER_INFO}


type
  // Per-buffer flags that apply to input buffers
  PMFT_INPUT_DATA_BUFFER_FLAGS = ^MFT_INPUT_DATA_BUFFER_FLAGS;
  _MFT_INPUT_DATA_BUFFER_FLAGS = DWord;
  {$EXTERNALSYM _MFT_INPUT_DATA_BUFFER_FLAGS}
  MFT_INPUT_DATA_BUFFER_FLAGS = _MFT_INPUT_DATA_BUFFER_FLAGS;
  {$EXTERNALSYM MFT_INPUT_DATA_BUFFER_FLAGS}
const
  MFT_INPUT_DATA_BUFFER_PLACEHOLDER	= MAXDW;  // Reserved. Do not use.

type
  PMFT_OUTPUT_DATA_BUFFER_FLAGS = ^MFT_OUTPUT_DATA_BUFFER_FLAGS;
  _MFT_OUTPUT_DATA_BUFFER_FLAGS = DWord;
  {$EXTERNALSYM _MFT_OUTPUT_DATA_BUFFER_FLAGS}
  MFT_OUTPUT_DATA_BUFFER_FLAGS = _MFT_OUTPUT_DATA_BUFFER_FLAGS;
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER_FLAGS}
const
  MFT_OUTPUT_DATA_BUFFER_INCOMPLETE	= MFT_OUTPUT_DATA_BUFFER_FLAGS($1000000);
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER_INCOMPLETE}
  MFT_OUTPUT_DATA_BUFFER_FORMAT_CHANGE	= MFT_OUTPUT_DATA_BUFFER_FLAGS($100);
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER_FORMAT_CHANGE}
  MFT_OUTPUT_DATA_BUFFER_STREAM_END	= MFT_OUTPUT_DATA_BUFFER_FLAGS($200);
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER_STREAM_END}
  MFT_OUTPUT_DATA_BUFFER_NO_SAMPLE	= MFT_OUTPUT_DATA_BUFFER_FLAGS($300);
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER_NO_SAMPLE}


type
  // Flags returned by GetInputStatusFlags()
  PMFT_INPUT_STATUS_FLAGS = ^MFT_INPUT_STATUS_FLAGS;
  _MFT_INPUT_STATUS_FLAGS = DWord;
  {$EXTERNALSYM _MFT_INPUT_STATUS_FLAGS}
  MFT_INPUT_STATUS_FLAGS = _MFT_INPUT_STATUS_FLAGS;
  {$EXTERNALSYM MFT_INPUT_STATUS_FLAGS}
const
  MFT_INPUT_STATUS_ACCEPT_DATA = MFT_INPUT_STATUS_FLAGS($1);
    {$EXTERNALSYM MFT_INPUT_STATUS_ACCEPT_DATA}

type
  PMFT_OUTPUT_STATUS_FLAGS = ^MFT_OUTPUT_STATUS_FLAGS;
  _MFT_OUTPUT_STATUS_FLAGS = DWord;
  {$EXTERNALSYM _MFT_OUTPUT_STATUS_FLAGS}
  MFT_OUTPUT_STATUS_FLAGS = _MFT_OUTPUT_STATUS_FLAGS;
  {$EXTERNALSYM MFT_OUTPUT_STATUS_FLAGS}
const
  MFT_OUTPUT_STATUS_SAMPLE_READY	= MFT_OUTPUT_STATUS_FLAGS($1);
  {$EXTERNALSYM MFT_OUTPUT_STATUS_SAMPLE_READY}

type
  // Flags returned by GetInputStreamInfo()
  PMFT_INPUT_STREAM_INFO_FLAGS = ^MFT_INPUT_STREAM_INFO_FLAGS;
  _MFT_INPUT_STREAM_INFO_FLAGS = DWord;
  {$EXTERNALSYM _MFT_INPUT_STREAM_INFO_FLAGS}
  MFT_INPUT_STREAM_INFO_FLAGS = _MFT_INPUT_STREAM_INFO_FLAGS;
  {$EXTERNALSYM MFT_INPUT_STREAM_INFO_FLAGS}
const
  MFT_INPUT_STREAM_WHOLE_SAMPLES	          = MFT_INPUT_STREAM_INFO_FLAGS($1);
  {$EXTERNALSYM MFT_INPUT_STREAM_WHOLE_SAMPLES}
  MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER	= MFT_INPUT_STREAM_INFO_FLAGS($2);
  {$EXTERNALSYM MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER}
  MFT_INPUT_STREAM_FIXED_SAMPLE_SIZE	      = MFT_INPUT_STREAM_INFO_FLAGS($4);
  {$EXTERNALSYM MFT_INPUT_STREAM_FIXED_SAMPLE_SIZE}
  MFT_INPUT_STREAM_HOLDS_BUFFERS	          = MFT_INPUT_STREAM_INFO_FLAGS($8);
  {$EXTERNALSYM MFT_INPUT_STREAM_HOLDS_BUFFERS}
  MFT_INPUT_STREAM_DOES_NOT_ADDREF	        = MFT_INPUT_STREAM_INFO_FLAGS($100);
  {$EXTERNALSYM MFT_INPUT_STREAM_DOES_NOT_ADDREF}
  MFT_INPUT_STREAM_REMOVABLE	              = MFT_INPUT_STREAM_INFO_FLAGS($200);
  {$EXTERNALSYM MFT_INPUT_STREAM_REMOVABLE}
  MFT_INPUT_STREAM_OPTIONAL	                = MFT_INPUT_STREAM_INFO_FLAGS($400);
  {$EXTERNALSYM MFT_INPUT_STREAM_OPTIONAL}
  MFT_INPUT_STREAM_PROCESSES_IN_PLACE	      = MFT_INPUT_STREAM_INFO_FLAGS($800);
  {$EXTERNALSYM MFT_INPUT_STREAM_PROCESSES_IN_PLACE}

type
  // Flags returned by GetOutputStreamInfo()
  PMFT_OUTPUT_STREAM_INFO_FLAGS = ^MFT_OUTPUT_STREAM_INFO_FLAGS;
  _MFT_OUTPUT_STREAM_INFO_FLAGS = DWord;
  {$EXTERNALSYM _MFT_OUTPUT_STREAM_INFO_FLAGS}
  MFT_OUTPUT_STREAM_INFO_FLAGS = _MFT_OUTPUT_STREAM_INFO_FLAGS;
  {$EXTERNALSYM MFT_OUTPUT_STREAM_INFO_FLAGS}
const
  //
  // Carried over from DMO (IMediaObject)
  //
  MFT_OUTPUT_STREAM_WHOLE_SAMPLES	            = MFT_OUTPUT_STREAM_INFO_FLAGS($1);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_WHOLE_SAMPLES}
  MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER	= MFT_OUTPUT_STREAM_INFO_FLAGS($2);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER}
  MFT_OUTPUT_STREAM_FIXED_SAMPLE_SIZE	        = MFT_OUTPUT_STREAM_INFO_FLAGS($4);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_FIXED_SAMPLE_SIZE}
  MFT_OUTPUT_STREAM_DISCARDABLE	              = MFT_OUTPUT_STREAM_INFO_FLAGS($8);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_DISCARDABLE}
  MFT_OUTPUT_STREAM_OPTIONAL	                = MFT_OUTPUT_STREAM_INFO_FLAGS($10);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_OPTIONAL}
  MFT_OUTPUT_STREAM_PROVIDES_SAMPLES	        = MFT_OUTPUT_STREAM_INFO_FLAGS($100);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_PROVIDES_SAMPLES}
  MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES	      = MFT_OUTPUT_STREAM_INFO_FLAGS($200);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES}
  MFT_OUTPUT_STREAM_LAZY_READ	                = MFT_OUTPUT_STREAM_INFO_FLAGS($400);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_LAZY_READ}
  MFT_OUTPUT_STREAM_REMOVABLE	                = MFT_OUTPUT_STREAM_INFO_FLAGS($800);
  {$EXTERNALSYM MFT_OUTPUT_STREAM_REMOVABLE}

type
  // SetType flags
  PMFT_SET_TYPE_FLAGS = ^MFT_SET_TYPE_FLAGS;
  _MFT_SET_TYPE_FLAGS = DWord;
  {$EXTERNALSYM _MFT_SET_TYPE_FLAGS}
  MFT_SET_TYPE_FLAGS = _MFT_SET_TYPE_FLAGS;
  {$EXTERNALSYM MFT_SET_TYPE_FLAGS}
const
  //
  // Carried over from DMO (IMediaObject)
  //
  MFT_SET_TYPE_TEST_ONLY	= MFT_SET_TYPE_FLAGS($00000001); // check but don't set

  //
  // not carried over from DMO - use Nil type to unset.
  //
  MFT_SET_TYPE_CLEAR      = MFT_SET_TYPE_FLAGS($00000002); // unset


type
  // ProcessOutput() dwFlags (signals from caller to MFT)
  PMFT_PROCESS_OUTPUT_FLAGS = ^MFT_PROCESS_OUTPUT_FLAGS;
  _MFT_PROCESS_OUTPUT_FLAGS = DWord;
  {$EXTERNALSYM _MFT_PROCESS_OUTPUT_FLAGS}
  MFT_PROCESS_OUTPUT_FLAGS = _MFT_PROCESS_OUTPUT_FLAGS;
  {$EXTERNALSYM MFT_PROCESS_OUTPUT_FLAGS}
const
  MFT_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER	= MFT_PROCESS_OUTPUT_FLAGS($1); // discard this sample if pSample ptr is Nil.
  {$EXTERNALSYM MFT_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER}
  MFT_PROCESS_OUTPUT_REGENERATE_LAST_OUTPUT = MFT_PROCESS_OUTPUT_FLAGS($2); // New flags for MFTs
  {$EXTERNALSYM MFT_PROCESS_OUTPUT_REGENERATE_LAST_OUTPUT}


type
  // ProcessOutput() pdwStatus (output from MFT to caller)
  PMFT_PROCESS_OUTPUT_STATUS = ^MFT_PROCESS_OUTPUT_STATUS;
  _MFT_PROCESS_OUTPUT_STATUS = DWord;
  {$EXTERNALSYM _MFT_PROCESS_OUTPUT_STATUS}
  MFT_PROCESS_OUTPUT_STATUS = _MFT_PROCESS_OUTPUT_STATUS;
  {$EXTERNALSYM MFT_PROCESS_OUTPUT_STATUS}
const
  MFT_PROCESS_OUTPUT_STATUS_NEW_STREAMS	= MFT_PROCESS_OUTPUT_STATUS($100);  // Output flag
  {$EXTERNALSYM MFT_PROCESS_OUTPUT_STATUS_NEW_STREAMS}


type
  PMFT_DRAIN_TYPE = ^MFT_DRAIN_TYPE;
  _MFT_DRAIN_TYPE = (
    MFT_DRAIN_PRODUCE_TAILS	= 0,
	  MFT_DRAIN_NO_TAILS	    = $1
  );
  {$EXTERNALSYM _MFT_DRAIN_TYPE}
  MFT_DRAIN_TYPE = _MFT_DRAIN_TYPE;
  {$EXTERNALSYM MFT_DRAIN_TYPE}


const

  MFT_STREAMS_UNLIMITED               = $7FFFFFFF;
  {$EXTERNALSYM MFT_STREAMS_UNLIMITED}
  // used by GetStreamLimits - the transform returns it in ^pdwInputMaximum or ^pdwOutputMaximum
  // to mean that the MFT has no theoretical stream limit.
  MFT_OUTPUT_BOUND_LOWER_UNBOUNDED    = MINLONGLONG;
  {$EXTERNALSYM MFT_OUTPUT_BOUND_LOWER_UNBOUNDED}
  // Used by SetOutputBounds when the lower or upper limit are unbounded.
  MFT_OUTPUT_BOUND_UPPER_UNBOUNDED    = MAXLONGLONG;
  {$EXTERNALSYM MFT_OUTPUT_BOUND_UPPER_UNBOUNDED}


type

  PMFT_MESSAGE_TYPE = ^MFT_MESSAGE_TYPE;
  _MFT_MESSAGE_TYPE                    = (
    MFT_MESSAGE_COMMAND_FLUSH          = 0,
    MFT_MESSAGE_COMMAND_DRAIN          = $1,
    MFT_MESSAGE_SET_D3D_MANAGER        = $2,

//{$if WINVER >= _WIN32_WINNT_WIN7}
    MFT_MESSAGE_DROP_SAMPLES           = $3,
//{$endif}

//{$if WINVER >= _WIN32_WINNT_WIN8}
    MFT_MESSAGE_COMMAND_TICK           = $4,
//{$endif}

    // notifications - no action required; effect is transform-dependent
    MFT_MESSAGE_NOTIFY_BEGIN_STREAMING = $10000000,
    MFT_MESSAGE_NOTIFY_END_STREAMING   = $10000001,
    MFT_MESSAGE_NOTIFY_END_OF_STREAM   = $10000002,
    MFT_MESSAGE_NOTIFY_START_OF_STREAM = $10000003, // send by pipeline before processing the first sample

//{$if WINVER >= _WIN32_WINNT_WINTHRESHOLD}
    MFT_MESSAGE_NOTIFY_RELEASE_RESOURCES        = $10000004,
    MFT_MESSAGE_NOTIFY_REACQUIRE_RESOURCES      = $10000005,
    MFT_MESSAGE_NOTIFY_EVENT                    = $10000006,
    MFT_MESSAGE_COMMAND_SET_OUTPUT_STREAM_STATE	= $10000007,
    MFT_MESSAGE_COMMAND_FLUSH_OUTPUT_STREAM	    = $10000008,
//{$endif}

//{$if WINVER >= _WIN32_WINNT_WIN7}
    MFT_MESSAGE_COMMAND_MARKER                  = $20000000  // commands (applicable to async MFTs only) - must be acted on
//{$endif}

  );
  {$EXTERNALSYM _MFT_MESSAGE_TYPE}
  MFT_MESSAGE_TYPE = _MFT_MESSAGE_TYPE;
  {$EXTERNALSYM MFT_MESSAGE_TYPE}


  PMFT_INPUT_STREAM_INFO = ^MFT_INPUT_STREAM_INFO;
  _MFT_INPUT_STREAM_INFO = record
    hnsMaxLatency: LONGLONG;    // maximum time latency in 100ns increments
    dwFlags: DWORD;             // MFT_INPUT_STREAM_INFO_FLAGS
    cbSize: DWORD;              // size of each sample's buffer
    cbMaxLookahead: DWORD;      // max total bytes held
    cbAlignment: DWORD;         // buffer alignment requirement
  end;
  {$EXTERNALSYM _MFT_INPUT_STREAM_INFO}
  MFT_INPUT_STREAM_INFO = _MFT_INPUT_STREAM_INFO;
  {$EXTERNALSYM MFT_INPUT_STREAM_INFO}


  PMFT_OUTPUT_STREAM_INFO = ^MFT_OUTPUT_STREAM_INFO;
  _MFT_OUTPUT_STREAM_INFO = record
    dwFlags: DWORD;             // MFT_INPUT_STREAM_INFO_FLAGS
    cbSize: DWORD;              // size of each sample's buffer
    cbAlignment: DWORD;         // buffer alignment requirement
  end;
  {$EXTERNALSYM _MFT_OUTPUT_STREAM_INFO}
  MFT_OUTPUT_STREAM_INFO = _MFT_OUTPUT_STREAM_INFO;
  {$EXTERNALSYM MFT_OUTPUT_STREAM_INFO}


  // Output buffer info structure: one of these must be passed in for each
  // output stream with every ProcessOutput() call
  // All [out] fields should be assumed undefined if ProcessOutput() failed
  PMFT_OUTPUT_DATA_BUFFER = ^MFT_OUTPUT_DATA_BUFFER;
  _MFT_OUTPUT_DATA_BUFFER = record
    dwStreamID:   DWORD;           // [in] which stream is this for
    pSample:      IMFSample;       // [in/out] can be Nil
    dwStatus:     DWORD;           // [out] MFT_OUTPUT_DATA_BUFFER_FLAGS (INCOMPLETE, etc.)
    pEvents:      IMFCollection;   // [out] Can be Nil.  Zero or more events produced by the MFT
  end;
  {$EXTERNALSYM _MFT_OUTPUT_DATA_BUFFER}
  MFT_OUTPUT_DATA_BUFFER = _MFT_OUTPUT_DATA_BUFFER;
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER}


  PSTREAM_MEDIUM = ^STREAM_MEDIUM;
  _STREAM_MEDIUM = record
    gidMedium: TGUID;
    unMediumInstance: UINT32;
  end;
  {$EXTERNALSYM _STREAM_MEDIUM}
  STREAM_MEDIUM = _STREAM_MEDIUM;
  {$EXTERNALSYM STREAM_MEDIUM}


  PDeviceStreamState = ^DeviceStreamState;
  _DeviceStreamState           = (
    DeviceStreamState_Stop     = 0,
    {$EXTERNALSYM DeviceStreamState_Stop}
    DeviceStreamState_Pause    = (DeviceStreamState_Stop  + 1),
    {$EXTERNALSYM DeviceStreamState_Pause}
    DeviceStreamState_Run      = (DeviceStreamState_Pause  + 1),
    {$EXTERNALSYM DeviceStreamState_Run}
    DeviceStreamState_Disabled = (DeviceStreamState_Run  + 1)
  );
    {$EXTERNALSYM DeviceStreamState_Disabled}
  {$EXTERNALSYM _DeviceStreamState}
  DeviceStreamState = _DeviceStreamState;
  {$EXTERNALSYM DeviceStreamState}


  // redefine all the method names to have MFT at the beginning so they don't class with DMO methods.
{$ifdef MFT_UNIQUE_METHOD_NAMES}
  GetStreamLimits                     = MFTGetStreamLimits;
  {$EXTERNALSYM GetStreamLimits}
  GetStreamCount                      = MFTGetStreamCount;
  {$EXTERNALSYM GetStreamCount}
  GetStreamIDs                        = MFTGetStreamIDs;
  {$EXTERNALSYM GetStreamIDs}
  GetInputStreamInfo                  = MFTGetInputStreamInfo;
  {$EXTERNALSYM GetInputStreamInfo}
  GetOutputStreamInfo                 = MFTGetOutputStreamInfo;
  {$EXTERNALSYM GetOutputStreamInfo}
  DeleteInputStream                   = MFTDeleteInputStream;
  {$EXTERNALSYM DeleteInputStream}
  AddInputStreams                     = MFTAddInputStreams;
  {$EXTERNALSYM AddInputStreams}
  GetInputAvailableType               = MFTGetInputAvailableType;
  {$EXTERNALSYM GetInputAvailableType}
  GetOutputAvailableType              = MFTGetOutputAvailableType;
  {$EXTERNALSYM GetOutputAvailableType}
  SetInputType                        = MFTSetInputType;
  {$EXTERNALSYM SetInputType}
  SetOutputType                       = MFTSetOutputType;
  {$EXTERNALSYM SetOutputType}
  GetInputCurrentType                 = MFTGetInputCurrentType;
  {$EXTERNALSYM GetInputCurrentType}
  GetOutputCurrentType                = MFTGetOutputCurrentType;
  {$EXTERNALSYM GetOutputCurrentType}
  GetInputStatus                      = MFTGetInputStatus;
  {$EXTERNALSYM GetInputStatus}
  GetOutputStatus                     = MFTGetOutputStatus;
  {$EXTERNALSYM GetOutputStatus}
  SetOutputBounds                     = MFTSetOutputBounds;
  {$EXTERNALSYM SetOutputBounds}
  ProcessEvent                        = MFTProcessEvent;
  {$EXTERNALSYM ProcessEvent}
  ProcessMessage                      = MFTProcessMessage;
  {$EXTERNALSYM ProcessMessage}
  ProcessInput                        = MFTProcessInput;
  {$EXTERNALSYM ProcessInput}
  ProcessOutput                       = MFTProcessOutput;
  {$EXTERNALSYM ProcessOutput}
{$endif}

type

  // Interface IMFTransform
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTransform);'}
  {$EXTERNALSYM IMFTransform}
  IMFTransform = interface(IUnknown)
    ['{bf94c121-5b05-4e6f-8000-ba598961414d}']

    function GetStreamLimits(out pdwInputMinimum: DWORD;
                             out pdwInputMaximum: DWORD;
                             out pdwOutputMinimum: DWORD;
                             out pdwOutputMaximum: DWORD): HResult; stdcall;

    function GetStreamCount(out pcInputStreams: DWORD;
                            out pcOutputStreams: DWORD): HResult; stdcall;

    function GetStreamIDs(dwInputIDArraySize: DWORD;
                          out pdwInputIDs: PDWORD;
                          dwOutputIDArraySize: DWORD;
                          pdwOutputIDs: PDWORD): HResult; stdcall;

    function GetInputStreamInfo(dwInputStreamID: DWORD;
                                out pStreamInfo: MFT_INPUT_STREAM_INFO): HResult; stdcall;

    function GetOutputStreamInfo(dwOutputStreamID: DWORD;
                                 out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HResult; stdcall;

    function GetAttributes(out pAttributes: IMFAttributes): HResult; stdcall;

    function GetInputStreamAttributes(dwInputStreamID: DWORD;
                                      out pAttributes: IMFAttributes): HResult; stdcall;

    function GetOutputStreamAttributes(dwOutputStreamID: DWORD;
                                       out pAttributes: IMFAttributes): HResult; stdcall;

    function DeleteInputStream(dwStreamID: DWORD): HResult; stdcall;

    function AddInputStreams(cStreams: DWORD;
                             adwStreamIDs: PDWORD): HResult; stdcall;

    function GetInputAvailableType(dwInputStreamID: DWORD;
                                   dwTypeIndex: DWORD;
                                   out ppType: IMFMediaType): HResult; stdcall;

    function GetOutputAvailableType(dwOutputStreamID: DWORD;
                                    dwTypeIndex: DWORD;
                                    var ppType: IMFMediaType): HResult; stdcall;

    function SetInputType(dwInputStreamID: DWORD;
                          pType: IMFMediaType;
                          dwFlags: DWORD): HResult; stdcall;

    function SetOutputType(dwOutputStreamID: DWORD;
                           pType: IMFMediaType;
                           dwFlags: DWORD): HResult; stdcall;

    function GetInputCurrentType(dwInputStreamID: DWORD;
                                 out ppType: IMFMediaType): HResult; stdcall;

    function GetOutputCurrentType(dwOutputStreamID: DWORD;
                                  out ppType: IMFMediaType): HResult; stdcall;

    function GetInputStatus(dwInputStreamID: DWORD;
                            out pdwFlags: DWORD): HResult; stdcall;

    function GetOutputStatus(out pdwFlags: DWORD): HResult; stdcall;

    function SetOutputBounds(hnsLowerBound: LONGLONG;
                             hnsUpperBound: LONGLONG): HResult; stdcall;

    function ProcessEvent(dwInputStreamID: DWORD;
                          pEvent: IMFMediaEvent): HResult; stdcall;

    function ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                            ulParam: ULONG_PTR): HResult; stdcall;

    function ProcessInput(dwInputStreamID: DWORD;
                          pSample: IMFSample;
                          dwFlags: DWORD): HResult; stdcall;

    function ProcessOutput(dwFlags: DWORD;
                           cOutputBufferCount: DWORD;
                           pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HResult; stdcall;

  end;
  IID_IMFTransform = IMFTransform;
  {$EXTERNALSYM IID_IMFTransform}


  // Interface IMFDeviceTransform
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDeviceTransform);'}
  {$EXTERNALSYM IMFDeviceTransform}
  IMFDeviceTransform = interface(IUnknown)
    ['{D818FBD8-FC46-42F2-87AC-1EA2D1F9BF32}']

    function InitializeTransform(pAttributes: IMFAttributes): HResult; stdcall;

    function GetInputAvailableType(dwInputStreamID: DWORD;
                                   dwTypeIndex: DWORD;
                                   out pMediaType: IMFMediaType): HResult; stdcall;

    function GetInputCurrentType(dwInputStreamID: DWORD;
                                 out pMediaType: IMFMediaType): HResult; stdcall;

    function GetInputStreamAttributes(dwInputStreamID: DWORD;
                                      out ppAttributes: IMFAttributes): HResult; stdcall;

    function GetOutputAvailableType(dwOutputStreamID: DWORD;
                                    dwTypeIndex: DWORD;
                                    out pMediaType: IMFMediaType): HResult; stdcall;

    function GetOutputCurrentType(dwOutputStreamID: DWORD;
                                  out pMediaType: IMFMediaType): HResult; stdcall;

    function GetOutputStreamAttributes(dwOutputStreamID: DWORD;
                                       out ppAttributes: IMFAttributes): HResult; stdcall;

    function GetStreamCount(out pcInputStreams: DWORD;
                            out pcOutputStreams: DWORD): HResult; stdcall;

    function GetStreamIDs(dwInputIDArraySize: DWORD;
                          out pdwInputStreamIds: PDWORD;
                          dwOutputIDArraySize: DWORD;
                          out pdwOutputStreamIds: PDWORD): HResult; stdcall;

    function ProcessEvent(dwInputStreamID: DWORD;
                          out pEvent: IMFMediaEvent): HResult; stdcall;

    function ProcessInput(dwInputStreamID: DWORD;
                          out pSample: IMFSample;
                          dwFlags: DWORD): HResult; stdcall;

    function ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                            ulParam: ULONG_PTR): HResult; stdcall;

    function ProcessOutput(dwFlags: DWORD;
                           cOutputBufferCount: DWORD;
                           var pOutputSample: MFT_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HResult; stdcall;

    function SetInputStreamState(dwStreamID: DWORD;
                                 pMediaType: IMFMediaType;
                                 value: DeviceStreamState;
                                 dwFlags: DWORD): HResult; stdcall;

    function GetInputStreamState(dwStreamID: DWORD;
                                 out value: DeviceStreamState): HResult; stdcall;

    function SetOutputStreamState(dwStreamID: DWORD;
                                  pMediaType: IMFMediaType;
                                  value: DeviceStreamState;
                                  dwFlags: DWORD): HResult; stdcall;

    function GetOutputStreamState(dwStreamID: DWORD;
                                  out value: DeviceStreamState): HResult; stdcall;

    function GetInputStreamPreferredState(dwStreamID: DWORD;
                                          out value: DeviceStreamState;
                                          out ppMediaType: IMFMediaType): HResult; stdcall;

    function FlushInputStream(dwStreamIndex: DWORD;
                              dwFlags: DWORD): HResult; stdcall;

    function FlushOutputStream(dwStreamIndex: DWORD;
                               dwFlags: DWORD): HResult; stdcall;

  end;
  IID_IMFDeviceTransform = IMFDeviceTransform;
  {$EXTERNALSYM IID_IMFDeviceTransform}


  // Interface IMFDeviceTransformCallback
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDeviceTransformCallback);'}
  {$EXTERNALSYM IMFDeviceTransformCallback}
  IMFDeviceTransformCallback = interface(IUnknown)
    ['{6D5CB646-29EC-41FB-8179-8C4C6D750811}']

    function OnBufferSent(pCallbackAttributes: IMFAttributes;
                          pinId: DWORD): HResult; stdcall;
    // Provides information of buffers allocated and sent to driver
    //
    // PKSSTREAM_HEADER that is inside pInfo is meant only for READ-ONLY usage
    // i.e., user shouldn't try to allocate/deallocate, open/close
    // anything within the header
    //
  end;
  IID_IMFDeviceTransformCallback = IMFDeviceTransformCallback;
  {$EXTERNALSYM IID_IMFDeviceTransformCallback}


const

  MEDeviceStreamCreated : TGUID = '{0252a1cf-3540-43b4-9164-d72eb405fa40}';
  {$EXTERNALSYM MEDeviceStreamCreated}


  // GENERIC DMO PROPERTY KEYS
  // =========================

  MFPKEY_CLSID                     : PROPERTYKEY = (fmtid: (D1: $57a84c0;
                                                            D2: $1a80;
                                                            D3: $40a3;
                                                            D4: ($97, $b5, $92, $72, $a4, $3, $c8, $ae));
                                                            pid: $01);
  {$EXTERNALSYM MFPKEY_CLSID}

  MFPKEY_CATEGORY                  : PROPERTYKEY = (fmtid: (D1: $c57a84c0;
                                                            D2: $1a80;
                                                            D3: $40a3;
                                                            D4: ($97, $b5, $92, $72, $a4, $3, $c8, $ae));
                                                            pid: $02);
  {$EXTERNALSYM MFPKEY_CATEGORY}

  MFPKEY_EXATTRIBUTE_SUPPORTED     : PROPERTYKEY = (fmtid: (D1: $456fe843;
                                                            D2: $3c87;
                                                            D3: $40c0;
                                                            D4: ($94, $9d, $14, $9, $c9, $7d, $ab, $2c));
                                                            pid: $01);
  {$EXTERNALSYM MFPKEY_EXATTRIBUTE_SUPPORTED}

  MFPKEY_MULTICHANNEL_CHANNEL_MASK : PROPERTYKEY = (fmtid: (D1: $58bdaf8c;
                                                            D2: $3224;
                                                            D3: $4692;
                                                            D4: ($86, $d0, $44, $d6, $5c, $5b, $f8, $2b));
                                                            pid: $01);
  {$EXTERNALSYM MFPKEY_MULTICHANNEL_CHANNEL_MASK}


  MF_SA_D3D_AWARE              : TGUID =  (D1: $eaa35c29;
                                           D2: $775e;
                                           D3: $488e;
                                           D4: ($9b, $61, $b3, $28, $3e, $49, $58, $3b));
  {$EXTERNALSYM MF_SA_D3D_AWARE}

  MF_SA_REQUIRED_SAMPLE_COUNT  : TGUID =  (D1: $18802c61;
                                           D2: $324b;
                                           D3: $4952;
                                           D4: ($ab, $d0, $17, $6f, $f5, $c6, $96, $ff));
  {$EXTERNALSYM MF_SA_REQUIRED_SAMPLE_COUNT}

  MFT_END_STREAMING_AWARE      : TGUID =  (D1: $70fbc845;
                                           D2: $b07e;
                                           D3: $4089;
                                           D4: ($b0, $64, $39, $9d, $c6, $11, $f, $29));
  {$EXTERNALSYM MFT_END_STREAMING_AWARE}

  MF_SA_AUDIO_ENDPOINT_AWARE   : TGUID =  (D1: $c0381701;
                                           D2: $805c;
                                           D3: $42b2;
                                           D4: ($ac, $8d, $e2, $b4, $bf, $21, $f4, $f8));
  {$EXTERNALSYM MF_SA_AUDIO_ENDPOINT_AWARE}

  MFT_AUDIO_DECODER_AUDIO_ENDPOINT_ID        : TGUID =  (D1: $c7ccdd6e;
                                                         D2: $5398;
                                                         D3: $4695;
                                                         D4: ($8b, $e7, $51, $b3, $e9, $51, $11, $bd));
  {$EXTERNALSYM MFT_AUDIO_DECODER_AUDIO_ENDPOINT_ID}

  MFT_AUDIO_DECODER_SPATIAL_METADATA_CLIENT  : TGUID =  (D1: $5987df4;
                                                         D2: $1270;
                                                         D3: $4999;
                                                         D4: ($92, $5f, $8e, $93, $9a, $7c, $0a, $f7));
  {$EXTERNALSYM MFT_AUDIO_DECODER_SPATIAL_METADATA_CLIENT}


  // MF_SA_REQUIRED_SAMPLE_COUNT_PROGRESSIVE
  // Data type: UINT32
  // If present, indicates the number of samples that an MFT requires to be allocated
  // for progressive content.  This value is used if the next node downstream has an
  // IMFVideoSampleAllocator.
  // {B172D58E-FA77-4e48-8D2A-1DF2D850EAC2}
  MF_SA_REQUIRED_SAMPLE_COUNT_PROGRESSIVE   : TGuid = '{B172D58E-FA77-4e48-8D2A-1DF2D850EAC2}';
  {$EXTERNALSYM MF_SA_REQUIRED_SAMPLE_COUNT_PROGRESSIVE}
  // MF_SA_MINIMUM_OUTPUT_SAMPLE_COUNT
  // Data type: UINT32
  // If present, indicates the minimum number of samples that the MFT should allow
  //      to be oustanding (i.e. provided to the pipeline via ProcessOutput()) at any given time.
  // This value is only applicable to MFTs that allocate output samples themselves and use
  //      a circular allocator. Other MFTs can ignore this attribute.
  MF_SA_MINIMUM_OUTPUT_SAMPLE_COUNT        : TGuid = '{851745d5-c3d6-476d-9527-498ef2d10d18}';
  {$EXTERNALSYM MF_SA_MINIMUM_OUTPUT_SAMPLE_COUNT}
  // MF_SA_MINIMUM_OUTPUT_SAMPLE_COUNT_PROGRESSIVE
  // Data type: UINT32
  // If present, indicates the minimum number of progressive samples that the MFT should allow
  //      to be oustanding (i.e. provided to the pipeline via ProcessOutput()) at any given time.
  // This value is only applicable to MFTs that allocate output samples themselves and use
  //      a circular allocator. Other MFTs can ignore this attribute.
  MF_SA_MINIMUM_OUTPUT_SAMPLE_COUNT_PROGRESSIVE  : TGuid = '{b172d58e-fa77-4e48-8d2a-1df2d850eac2}';
  {$EXTERNALSYM MF_SA_MINIMUM_OUTPUT_SAMPLE_COUNT_PROGRESSIVE}


  // Attributes related to 3D video
  // ==============================

  // MFT_SUPPORT_3DVIDEO
  // Data Type: UINT32; treat as Boolean
  // If TRUE, the transform understands 3D Video. Default is FALSE
  // This attribute should be exposed via attribute store obtained from IMFTransform::GetAttributes
  MFT_SUPPORT_3DVIDEO  : TGuid = '{093f81b1-4f2e-4631-8168-7934032a01d3}';
  {$EXTERNALSYM MFT_SUPPORT_3DVIDEO}


type
  // Enum that controls how components should deal with 3D video input that they receive
  // In case of transforms, only applies to those that advertise MFT_SUPPORT_3DVIDEO attribute
  PMF3DVideoOutputType = ^_MF3DVideoOutputType;
  _MF3DVideoOutputType           = UINT32;
  {$EXTERNALSYM _MF3DVideoOutputType}
  MF3DVideoOutputType = _MF3DVideoOutputType;
  {$EXTERNALSYM MF3DVideoOutputType}
const
  MF3DVideoOutputType_BaseView = MF3DVideoOutputType(0);
  MF3DVideoOutputType_Stereo   = MF3DVideoOutputType(1);


const

  // MF_ENABLE_3DVIDEO_OUTPUT
  // Data Type: UINT32; treat as one of MFVideoProcessor3DOutput
  // This attribute is SET on components via the attribute store obtained from IMFTransform::GetAttributes
  // If MF3DVideoOutputType_BaseView, component should output only the base view. This is the default.
  // If MF3DVideoOutputType_Stereo, component should output both left and right views in separate buffers
  MF_ENABLE_3DVIDEO_OUTPUT : TGuid = '{bdad7bca-0e5f-4b10-ab16-26de381b6293}';
  {$EXTERNALSYM MF_ENABLE_3DVIDEO_OUTPUT}


  // Attributes for D3D11 support
  // ============================

  MF_SA_D3D11_BINDFLAGS                 : TGuid = '{eacf97ad-065c-4408-bee3-fdcbfd128be2}';
  {$EXTERNALSYM MF_SA_D3D11_BINDFLAGS}
  MF_SA_D3D11_USAGE                     : TGuid = '{e85fe442-2ca3-486e-a9c7-109dda609880}';
  {$EXTERNALSYM MF_SA_D3D11_USAGE}
  MF_SA_D3D11_AWARE                     : TGuid = '{206b4fc8-fcf9-4c51-afe3-9764369e33a0}';
  {$EXTERNALSYM MF_SA_D3D11_AWARE}
  MF_SA_D3D11_SHARED                    : TGuid = '{7b8f32c3-6d96-4b89-9203-dd38b61414f3}';
  {$EXTERNALSYM MF_SA_D3D11_SHARED}
  MF_SA_D3D11_SHARED_WITHOUT_MUTEX      : TGuid = '{39dbd44d-2e44-4931-a4c8-352d3dc42115}';
  {$EXTERNALSYM MF_SA_D3D11_SHARED_WITHOUT_MUTEX}
  MF_SA_D3D11_ALLOW_DYNAMIC_YUV_TEXTURE : TGuid = '{ce06d49f-0613-4b9d-86a6-d8c4f9c10075}';
  {$EXTERNALSYM MF_SA_D3D11_ALLOW_DYNAMIC_YUV_TEXTURE}
{#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)}
  MF_SA_D3D11_HW_PROTECTED              : TGuid = '{3a8ba9d9-92ca-4307-a391-6999dbf3b6ce}';
  {$EXTERNALSYM MF_SA_D3D11_HW_PROTECTED}
{#endif}

  // MF_SA_BUFFERS_PER_SAMPLE
  // Data Type: UINT32
  // Specifies the # of buffers (of the same size, etc) that should be allocated for every sample.
  MF_SA_BUFFERS_PER_SAMPLE              : TGuid = '{873c5171-1e3d-4e25-988d-b433ce041983}';
  {$EXTERNALSYM MF_SA_BUFFERS_PER_SAMPLE}

  // MFT_DECODER_EXPOSE_OUTPUT_TYPES_IN_NATIVE_ORDER
  // Data Type: UINT32 (treat as BOOL)
  // Specifies whether the codec should expose the types in native preferred order instead of reordering to expose NV12 (for example) first
  MFT_DECODER_EXPOSE_OUTPUT_TYPES_IN_NATIVE_ORDER : TGuid = '{ef80833f-f8fa-44d9-80d8-41ed6232670c}';
  {$EXTERNALSYM MFT_DECODER_EXPOSE_OUTPUT_TYPES_IN_NATIVE_ORDER}

  // MFT_DECODER_QUALITY_MANAGEMENT_CUSTOM_CONTROL {A24E30D7-DE25-4558-BBFB-71070A2D332E}
  // Data Type: UINT32 (treat as BOOL)
  // by default it's false
  // Specifies whether QM in video decoder mft should have the following two features for custom control externally by app or pipeline
  // 1. faster reaction time
  // 2. drop non-reference frames only, don't drop reference frames and let pipeline or app drop decoded/raw reference frames if needed
  MFT_DECODER_QUALITY_MANAGEMENT_CUSTOM_CONTROL : TGuid = '{A24E30D7-DE25-4558-BBFB-71070A2D332E}';
  {$EXTERNALSYM MFT_DECODER_QUALITY_MANAGEMENT_CUSTOM_CONTROL}

  // MFT_DECODER_QUALITY_MANAGEMENT_RECOVERY_WITHOUT_ARTIFACTS
  // Data Type: UINT32 (treat as BOOL)
  // Specifies whether video decoder mft should have fast recovery from quality management with potential corruptions/artifacts (the attribute value equal to FALSE)
  // or should have slower recovery from quality management without any corruptions/artifacts (the attribute value equal to TRUE)
  MFT_DECODER_QUALITY_MANAGEMENT_RECOVERY_WITHOUT_ARTIFACTS : TGuid = '{d8980deb-0a48-425f-8623-611db41d3810}';
  {$EXTERNALSYM MFT_DECODER_QUALITY_MANAGEMENT_RECOVERY_WITHOUT_ARTIFACTS}

  // MFT_REMUX_MARK_I_PICTURE_AS_CLEAN_POINT  {364e8f85-3f2e-436c-b2a2-4440a012a9e8}
  // Data Type: UINT32 (treat as BOOL)
  // Specifies whether (H.264) video REMUX mft should mark I pictures as clean point for better seek-ability with potential corruptions on seeks and non-conformant final MP4 files
  // Clean point picture should be compressed IDR pictures by defintion
  // Such an exercise is not recommended for MP4 file recording or remuxing, unless such an exercise is to produce some intermediate psedo MP4 files for video editing
  MFT_REMUX_MARK_I_PICTURE_AS_CLEAN_POINT : TGuid = '{364e8f85-3f2e-436c-b2a2-4440a012a9e8}';
  {$EXTERNALSYM MFT_REMUX_MARK_I_PICTURE_AS_CLEAN_POINT}

  // MFT_DECODER_FINAL_VIDEO_RESOLUTION_HINT {dc2f8496-15c4-407a-b6f0-1b66ab5fbf53}
  // Data Type: UINT64
  // Width and height for the final output resolution (after video processing).
  // Allows decoder to optimize the processing (e.g. disable loop filter if the output resolution is much smaller than content resolution)
  MFT_DECODER_FINAL_VIDEO_RESOLUTION_HINT : TGuid = '{dc2f8496-15c4-407a-b6f0-1b66ab5fbf53}';
  {$EXTERNALSYM MFT_DECODER_FINAL_VIDEO_RESOLUTION_HINT}

  // MFT_ENCODER_SUPPORTS_CONFIG_EVENT
  // Data Type: UINT32 (treat as BOOL)
  // Advertised by encoder MFTs that support receiving MEEncodingParameters event while streaming (via IMFTransform::ProcessEvent)
  // {86A355AE-3A77-4EC4-9F31-01149A4E92DE}
  MFT_ENCODER_SUPPORTS_CONFIG_EVENT : TGuid = '{86A355AE-3A77-4EC4-9F31-01149A4E92DE}';
  {$EXTERNALSYM MFT_ENCODER_SUPPORTS_CONFIG_EVENT}

  // MFT_ENUM_HARDWARE_VENDOR_ID_Attribute {3aecb0cc-035b-4bcc-8185-2b8d551ef3af}
  // Data Type: WSTR
  // For hardware MFTs, this attribute specifies the vendor ID of the hardware
  // that the HMFT is using for processing. This is only for reference and
  // is not used/verified by the topology
  MFT_ENUM_HARDWARE_VENDOR_ID_Attribute : TGuid = '{3aecb0cc-035b-4bcc-8185-2b8d551ef3af}';
  {$EXTERNALSYM MFT_ENUM_HARDWARE_VENDOR_ID_Attribute}

  // MF_TRANSFORM_ASYNC  {f81a699a-649a-497d-8c73-29f8fed6ad7a}
  // Data type: UINT32
  // If present and set to a nonzero value, indicates that this MFT functions
  // as an asynchronous MFT.  Only callers that understand how to call
  // an asynchronous MFT can use this MFT; those callers need to set the
  // MF_TRANSFORM_ASYNC_UNLOCK before making any IMFTransform calls.
  MF_TRANSFORM_ASYNC                            : TGuid = '{f81a699a-649a-497d-8c73-29f8fed6ad7a}';
  {$EXTERNALSYM MF_TRANSFORM_ASYNC}

  MF_TRANSFORM_ASYNC_UNLOCK                     : TGuid = '{e5666d6b-3422-4eb6-a421-da7db1f8e207}';
  {$EXTERNALSYM MF_TRANSFORM_ASYNC_UNLOCK}
  MF_TRANSFORM_FLAGS_Attribute                  : TGuid = '{9359bb7e-6275-46c4-a025-1c01e45f1a86}';
  {$EXTERNALSYM MF_TRANSFORM_FLAGS_Attribute}
  MF_TRANSFORM_CATEGORY_Attribute               : TGuid = '{ceabba49-506d-4757-a6ff-66c184987e4e}';
  {$EXTERNALSYM MF_TRANSFORM_CATEGORY_Attribute}
  MFT_TRANSFORM_CLSID_Attribute                 : TGuid = '{6821c42b-65a4-4e82-99bc-9a88205ecd0c}';
  {$EXTERNALSYM MFT_TRANSFORM_CLSID_Attribute}
  MFT_INPUT_TYPES_Attributes                    : TGuid = '{4276c9b1-759d-4bf3-9cd0-0d723d138f96}';
  {$EXTERNALSYM MFT_INPUT_TYPES_Attributes}
  MFT_OUTPUT_TYPES_Attributes                   : TGuid = '{8eae8cf3-a44f-4306-ba5c-bf5dda242818}';
  {$EXTERNALSYM MFT_OUTPUT_TYPES_Attributes}
  MFT_ENUM_HARDWARE_URL_Attribute               : TGuid = '{2fb866ac-b078-4942-ab6c-003d05cda674}';
  {$EXTERNALSYM MFT_ENUM_HARDWARE_URL_Attribute}
  MFT_FRIENDLY_NAME_Attribute                   : TGuid = '{314ffbae-5b41-4c95-9c19-4e7d586face3}';
  {$EXTERNALSYM MFT_FRIENDLY_NAME_Attribute}
  MFT_CONNECTED_STREAM_ATTRIBUTE                : TGuid = '{71eeb820-a59f-4de2-bcec-38db1dd611a4}';
  {$EXTERNALSYM MFT_CONNECTED_STREAM_ATTRIBUTE}
  MFT_CONNECTED_TO_HW_STREAM                    : TGuid = '{34e6e728-06d6-4491-a553-4795650db912}';
  {$EXTERNALSYM MFT_CONNECTED_TO_HW_STREAM}
  MFT_PREFERRED_OUTPUTTYPE_Attribute            : TGuid = '{7e700499-396a-49ee-b1b4-f628021e8c9d}';
  {$EXTERNALSYM MFT_PREFERRED_OUTPUTTYPE_Attribute}
  MFT_PROCESS_LOCAL_Attribute                   : TGuid = '{543186e4-4649-4e65-b588-4aa352aff379}';
  {$EXTERNALSYM MFT_PROCESS_LOCAL_Attribute}
  MFT_PREFERRED_ENCODER_PROFILE                 : TGuid = '{53004909-1ef5-46d7-a18e-5a75f8b5905f}';
  {$EXTERNALSYM MFT_PREFERRED_ENCODER_PROFILE}
  MFT_HW_TIMESTAMP_WITH_QPC_Attribute           : TGuid = '{8d030fb8-cc43-4258-a22e-9210bef89be4}';
  {$EXTERNALSYM MFT_HW_TIMESTAMP_WITH_QPC_Attribute}
  MFT_FIELDOFUSE_UNLOCK_Attribute               : TGuid = '{8ec2e9fd-9148-410d-831e-702439461a8e}';
  {$EXTERNALSYM MFT_FIELDOFUSE_UNLOCK_Attribute}
  MFT_CODEC_MERIT_Attribute                     : TGuid = '{88a7cb15-7b07-4a34-9128-e64c6703c4d3}';
  {$EXTERNALSYM MFT_CODEC_MERIT_Attribute}
  MFT_ENUM_TRANSCODE_ONLY_ATTRIBUTE             : TGuid = '{111ea8cd-b62a-4bdb-89f6-67ffcdc2458b}';
  {$EXTERNALSYM MFT_ENUM_TRANSCODE_ONLY_ATTRIBUTE}
  MFT_AUDIO_DECODER_DEGRADATION_INFO_ATTRIBUTE  : TGUID =  '{6c3386ad-ec20-430d-b2a5-505c7178d9c4}';
  {$EXTERNALSYM MFT_AUDIO_DECODER_DEGRADATION_INFO_ATTRIBUTE}


type

  PMFT_AUDIO_DECODER_DEGRADATION_REASON = ^MFT_AUDIO_DECODER_DEGRADATION_REASON;
  MFT_AUDIO_DECODER_DEGRADATION_REASON                         = (
    MFT_AUDIO_DECODER_DEGRADATION_REASON_NONE                  = 0,
    MFT_AUDIO_DECODER_DEGRADATION_REASON_LICENSING_REQUIREMENT = 1
  );
  {$EXTERNALSYM MFT_AUDIO_DECODER_DEGRADATION_REASON}

  PMFT_AUDIO_DECODER_DEGRADATION_TYPE = ^MFT_AUDIO_DECODER_DEGRADATION_TYPE;
  MFT_AUDIO_DECODER_DEGRADATION_TYPE                   = (
    MFT_AUDIO_DECODER_DEGRADATION_TYPE_NONE            = 0,
    MFT_AUDIO_DECODER_DEGRADATION_TYPE_DOWNMIX2CHANNEL = 1,
    MFT_AUDIO_DECODER_DEGRADATION_TYPE_DOWNMIX6CHANNEL = 2,
    MFT_AUDIO_DECODER_DEGRADATION_TYPE_DOWNMIX8CHANNEL = 3
  );
  {$EXTERNALSYM MFT_AUDIO_DECODER_DEGRADATION_TYPE}

  PMFAudioDecoderDegradationInfo = ^MFAudioDecoderDegradationInfo;
  MFAudioDecoderDegradationInfo = record
    eDegradationReason: MFT_AUDIO_DECODER_DEGRADATION_REASON;
    eType: MFT_AUDIO_DECODER_DEGRADATION_TYPE;
  end;
  {$EXTERNALSYM MFAudioDecoderDegradationInfo}

  PMFT_STREAM_STATE_PARAM = ^_MFT_STREAM_STATE_PARAM;
  _MFT_STREAM_STATE_PARAM = record
    StreamId: DWORD;
    State: MF_STREAM_STATE;
  end;
  {$EXTERNALSYM _MFT_STREAM_STATE_PARAM}
  MFT_STREAM_STATE_PARAM = _MFT_STREAM_STATE_PARAM;
  {$EXTERNALSYM MFT_STREAM_STATE_PARAM}


// #if NTDDI_VERSION >= NTDDI_WIN10_VB

const
  //
  // Attributes that can be used by an IMFInputTrustAuthority Decrypter.
  //
  // An implementation of a Content Decryption Module may include an implementation of
  // IMFInputTrustAuthority. The IMFInputTrustAuthority object is accessed through
  // IMFContentDecryptionModule.CreateTrustedInput.
  //

  // MFT_POLICY_SET_AWARE
  // Data type: UINT32 (treat as BOOL)
  // If nonzero, indicates that this IMFTransform wants to receive MEPolicySet completion notifications
  // {5A633B19-CC39-4FA8-8CA5-59981B7A0018}
  MFT_POLICY_SET_AWARE    : TGuid =  '{5A633B19-CC39-4FA8-8CA5-59981B7A0018}';
  {$EXTERNALSYM MFT_POLICY_SET_AWARE}

  // MFT_USING_HARDWARE_DRM
  // Data type: UINT32 (treat as BOOL)
  // If an MFT decrypter specifies this attribute set to 1, then it is using HARDWARE DRM.
  // If an MFT decrypter specifies this attribute set to 0, then it is not using HARDWARE DRM.
  // If an MFT decrypter does not specify this attribute or specifies it with a different value,
  // then it does not (or is unable to) indicate whether it is using HARDWARE DRM or not.
  MFT_USING_HARDWARE_DRM  : TGUID = '{34faa77d-d79e-4957-b8ce-362b2684996c}';
  {$EXTERNALSYM MFT_USING_HARDWARE_DRM}

// endif  NTDDI_VERSION >= NTDDI_WIN10_VB


  // Media Foundation MFT Activate creation functions
  // ================================================
  function MFCreateTransformActivate(out ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateTransformActivate}
  // ppActivate [out]
  //  Receives a pointer to the IMFActivate interface. The caller must release the interface.
  // Remarks:
  //   Most applications will not use this function; it is used internally by the MFTEnumEx function.


  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

const
  MfTransformLib = 'Mfplat.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateTransformActivate; external MfTransformLib name 'MFCreateTransformActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.

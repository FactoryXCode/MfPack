//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.WinMfError.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Error codes from Media Foundation.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/debug/error-handling-reference
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
unit WinApi.Dbg.WinMfError;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  {WinApiDebug}
  WinApi.Dbg.WinHResultTools;


  function GetMFFacilityDescription(const HResultCode: LongInt;
                                    out FacilityVal: LongInt;
                                    out FacilityTag: string;
                                    out FacilityDescr: string): HResult;

  function GetMFHResultDescription(const aHResult: HResult;
                                   out hrStr: string;
                                   out hrDescr: string;
                                   out RegionDescr: string;
                                   out HeaderFile: string;
                                   out Reference: TReferenceArray): HResult;

  function GetMFRegion(aHResult: HResult;
                       out aRegion: string): HResult;


implementation

uses
  System.SysUtils,
  System.StrUtils;


function GetMFFacilityDescription(const HResultCode: LongInt;
                                  out FacilityVal: LongInt;
                                  out FacilityTag: string;
                                  out FacilityDescr: string): HResult;
const
  FACILITYSOURCE = 'The source of the HResult code is the ';

var
  hr: HResult;
  FacilityCode: Integer;

begin
  hr := S_OK;
  // Get the facility code
  // Note: The facility is the originating API.
  FacilityCode := HRESULT_FACILITY(HResultCode);

  // Determen the code.
  case FacilityCode of
    FACILITY_MF:          begin
                            FacilityTag := 'FACILITY_MF';
                            FacilityDescr := FACILITYSOURCE + 'Media Foundation API.';
                          end;
    FACILITY_MF_WIN32:    begin
                            FacilityTag := 'FACILITY_MF_WIN32';
                            FacilityDescr := FACILITYSOURCE + 'Win32 Media Foundation API.';
                          end;
    else
      begin
        FacilityTag := 'UNKNOWN';
        FacilityDescr := FACILITYSOURCE + ' unknown.';
        hr := ERROR_NOT_FOUND;
      end;
  end;
  Result := hr;
end;


function GetMFHResultDescription(const aHResult: HResult;
                                 out hrStr: string;
                                 out hrDescr: string;
                                 out RegionDescr: string;
                                 out HeaderFile: string;
                                 out Reference: TReferenceArray): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  HeaderFile := 'mferror.h';
  Reference[0] := 'https://learn.microsoft.com/en-us/windows/win32/medfound/media-foundation-programming-reference';
  Reference[1] := 'https://learn.microsoft.com/en-us/windows/win32/api/winerror';
  Reference[2] := '';

  case aHResult of

    _HRESULT_TYPEDEF_($C00D36B0)  : begin
                                      HrStr := 'MF_E_PLATFORM_NOT_INITIALIZED';
                                      HrDescr := 'Platform not initialized. Please call MFStartup().';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B1)  : begin
                                      HrStr := 'MF_E_BUFFERTOOSMALL';
                                      HrDescr := 'The buffer was too small to carry out the requested action.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B2)  : begin
                                      HrStr := 'MF_E_INVALIDREQUEST';
                                      HrDescr := 'The request is invalid in the current state.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B3)  : begin
                                      HrStr := 'MF_E_INVALIDSTREAMNUMBER';
                                      HrDescr := 'The stream number provided was invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B4)  : begin
                                      HrStr := 'MF_E_INVALIDMEDIATYPE';
                                      HrDescr := 'The data specified for the media type is invalid, inconsistent, or not supported by this object.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B5)  : begin
                                      HrStr := 'MF_E_NOTACCEPTING';
                                      HrDescr := 'The caller is currently not accepting further input.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B6)  : begin
                                      HrStr := 'MF_E_NOT_INITIALIZED';
                                      HrDescr := 'This object needs to be initialized before the requested operation can be carried out.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B7)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_REPRESENTATION';
                                      HrDescr := 'The requested representation is not supported by this object.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B8)  : begin
                                      HrStr := 'MF_E_NO_MORE_TYPES';
                                      HrDescr := 'An object ran out of media types to suggest therefore the requested chain of streaming objects cannot be completed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36B9)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_SERVICE';
                                      HrDescr := 'The object does not support the specified service.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36BA)  : begin
                                      HrStr := 'MF_E_UNEXPECTED';
                                      HrDescr := 'An unexpected error has occurred in the operation requested.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36BB)  : begin
                                      HrStr := 'MF_E_INVALIDNAME';
                                      HrDescr := 'Invalid name.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36BC)  : begin
                                      HrStr := 'MF_E_INVALIDTYPE';
                                      HrDescr := 'Invalid type.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36BD)  : begin
                                      HrStr := 'MF_E_INVALID_FILE_FORMAT';
                                      HrDescr := 'The file does not conform to the relevant file format specification.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36BE)  : begin
                                      HrStr := 'MF_E_INVALIDINDEX';
                                      HrDescr := 'Invalid index.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36BF)  : begin
                                      HrStr := 'MF_E_INVALID_TIMESTAMP';
                                      HrDescr := 'An invalid timestamp was given.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36C0)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_SCHEME';
                                      HrDescr := 'The scheme of the given URL is unsupported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36C3)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_BYTESTREAM_TYPE';
                                      HrDescr := 'The byte stream type of the given URL is unsupported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36C4)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_TIME_FORMAT';
                                      HrDescr := 'The given time format is unsupported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36C5)  : begin
                                      HrStr := 'MF_E_NO_SAMPLE_TIMESTAMP';
                                      HrDescr := 'The Media Sample does not have a timestamp.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36C8)  : begin
                                      HrStr := 'MF_E_NO_SAMPLE_DURATION';
                                      HrDescr := 'The Media Sample does not have a duration.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36C9)  : begin
                                      HrStr := 'MF_E_INVALID_STREAM_DATA';
                                      HrDescr := 'The request failed because the data in the stream is corrupt.\n.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36CB)  : begin
                                      HrStr := 'MF_E_RT_UNAVAILABLE';
                                      HrDescr := 'Real time services are not available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36CF)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_RATE';
                                      HrDescr := 'The specified rate is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D0)  : begin
                                      HrStr := 'MF_E_THINNING_UNSUPPORTED';
                                      HrDescr := 'This component does not support stream-thinning.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D1)  : begin
                                      HrStr := 'MF_E_REVERSE_UNSUPPORTED';
                                      HrDescr := 'The call failed because no reverse playback rates are available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D2)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_RATE_TRANSITION';
                                      HrDescr := 'The requested rate transition cannot occur in the current state.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D3)  : begin
                                      HrStr := 'MF_E_RATE_CHANGE_PREEMPTED';
                                      HrDescr := 'The requested rate change has been pre-empted and will not occur.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D4)  : begin
                                      HrStr := 'MF_E_NOT_FOUND';
                                      HrDescr := 'The specified object or value does not exist.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D5)  : begin
                                      HrStr := 'MF_E_NOT_AVAILABLE';
                                      HrDescr := 'The requested value is not available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D6)  : begin
                                      HrStr := 'MF_E_NO_CLOCK';
                                      HrDescr := 'The specified operation requires a clock and no clock is available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D7)  : begin
                                      HrStr := 'MF_S_MULTIPLE_BEGIN';
                                      HrDescr := 'This callback and state had already been passed in to this event generator earlier.';
                                    end;
    _HRESULT_TYPEDEF_($000D36D8)  : begin
                                      HrStr := 'MF_E_MULTIPLE_BEGIN';
                                      HrDescr := 'This callback has already been passed in to this event generator.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36D9)  : begin
                                      HrStr := 'MF_E_MULTIPLE_SUBSCRIBERS';
                                      HrDescr := 'Some component is already listening to events on this event generator.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36DA)  : begin
                                      HrStr := 'MF_E_TIMER_ORPHANED';
                                      HrDescr := 'This timer was orphaned before its callback time arrived.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36DB)  : begin
                                      HrStr := 'MF_E_STATE_TRANSITION_PENDING';
                                      HrDescr := 'A state transition is already pending.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36DC)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_STATE_TRANSITION';
                                      HrDescr := 'The requested state transition is unsupported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36DD)  : begin
                                      HrStr := 'MF_E_UNRECOVERABLE_ERROR_OCCURRED';
                                      HrDescr := 'An unrecoverable error has occurred.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36DE)  : begin
                                      HrStr := 'MF_E_SAMPLE_HAS_TOO_MANY_BUFFERS';
                                      HrDescr := 'The provided sample has too many buffers.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36DF)  : begin
                                      HrStr := 'MF_E_SAMPLE_NOT_WRITABLE';
                                      HrDescr := 'The provided sample is not writable.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E0)  : begin
                                      HrStr := 'MF_E_INVALID_KEY';
                                      HrDescr := 'The specified key is not valid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E2)  : begin
                                      HrStr := 'MF_E_BAD_STARTUP_VERSION';
                                      HrDescr := 'You are calling MFStartup with the wrong MF_VERSION. Mismatched bits?';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E3)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_CAPTION';
                                      HrDescr := 'The caption of the given URL is unsupported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E4)  : begin
                                      HrStr := 'MF_E_INVALID_POSITION';
                                      HrDescr := 'The operation on the current offset is not permitted.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E5)  : begin
                                      HrStr := 'MF_E_ATTRIBUTENOTFOUND';
                                      HrDescr := 'The requested attribute was not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E6)  : begin
                                      HrStr := 'MF_E_PROPERTY_TYPE_NOT_ALLOWED';
                                      HrDescr := 'The specified property type is not allowed in this context.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E7)  : begin
                                      HrStr := 'MF_E_PROPERTY_TYPE_NOT_SUPPORTED';
                                      HrDescr := 'The specified property type is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E8)  : begin
                                      HrStr := 'MF_E_PROPERTY_EMPTY';
                                      HrDescr := 'The specified property is empty.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36E9)  : begin
                                      HrStr := 'MF_E_PROPERTY_NOT_EMPTY';
                                      HrDescr := 'The specified property is not empty.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36EA)  : begin
                                      HrStr := 'MF_E_PROPERTY_VECTOR_NOT_ALLOWED';
                                      HrDescr := 'The vector property specified is not allowed in this context.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36EB)  : begin
                                      HrStr := 'MF_E_PROPERTY_VECTOR_REQUIRED';
                                      HrDescr := 'A vector property is required in this context.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36EC)  : begin
                                      HrStr := 'MF_E_OPERATION_CANCELLED';
                                      HrDescr := 'The operation is cancelled.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36ED)  : begin
                                      HrStr := 'MF_E_BYTESTREAM_NOT_SEEKABLE';
                                      HrDescr := 'The provided bytestream was expected to be seekable and it is not.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36EE)  : begin
                                      HrStr := 'MF_E_DISABLED_IN_SAFEMODE';
                                      HrDescr := 'The Media Foundation platform is disabled when the system is running in Safe Mode.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36EF)  : begin
                                      HrStr := 'MF_E_CANNOT_PARSE_BYTESTREAM';
                                      HrDescr := 'The Media Source could not parse the byte stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F0)  : begin
                                      HrStr := 'MF_E_SOURCERESOLVER_MUTUALLY_EXCLUSIVE_FLAGS';
                                      HrDescr := 'Mutually exclusive flags have been specified to source resolver. This flag combination is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F1)  : begin
                                      HrStr := 'MF_E_MEDIAPROC_WRONGSTATE';
                                      HrDescr := 'MediaProc is in the wrong state';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F2)  : begin
                                      HrStr := 'MF_E_RT_THROUGHPUT_NOT_AVAILABLE';
                                      HrDescr := 'Real time I/O service can not provide requested throughput.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F3)  : begin
                                      HrStr := 'MF_E_RT_TOO_MANY_CLASSES';
                                      HrDescr := 'The workqueue cannot be registered with more classes.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F4)  : begin
                                      HrStr := 'MF_E_RT_WOULDBLOCK';
                                      HrDescr := 'This operation cannot succeed because another thread owns this object.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F5)  : begin
                                      HrStr := 'MF_E_NO_BITPUMP';
                                      HrDescr := 'Internal. Bitpump not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F6)  : begin
                                      HrStr := 'MF_E_RT_OUTOFMEMORY';
                                      HrDescr := 'No more RT memory available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F7)  : begin
                                      HrStr := 'MF_E_RT_WORKQUEUE_CLASS_NOT_SPECIFIED';
                                      HrDescr := 'An MMCSS class has not been set for this work queue.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36F8)  : begin
                                      HrStr := 'MF_E_INSUFFICIENT_BUFFER';
                                      HrDescr := 'Insufficient memory for response.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7170)  : begin
                                      HrStr := 'MF_E_CANNOT_CREATE_SINK';
                                      HrDescr := 'Activate failed to create mediasink. Call OutputNode.GetUINT32(MF_TOPONODE_MAJORTYPE) for more information. ';
                                    end;
    _HRESULT_TYPEDEF_($C00D36FA)  : begin
                                      HrStr := 'MF_E_BYTESTREAM_UNKNOWN_LENGTH';
                                      HrDescr := 'The length of the provided bytestream is unknown.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36FB)  : begin
                                      HrStr := 'MF_E_SESSION_PAUSEWHILESTOPPED';
                                      HrDescr := 'The media session cannot pause from a stopped state.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36FC)  : begin
                                      HrStr := 'MF_S_ACTIVATE_REPLACED';
                                      HrDescr := 'The activate could not be created in the remote process for some reason it was replaced with empty one.';
                                    end;
    _HRESULT_TYPEDEF_($000D36FD)  : begin
                                      HrStr := 'MF_E_FORMAT_CHANGE_NOT_SUPPORTED';
                                      HrDescr := 'The data specified for the media type is supported, but would require a format change, which is not supported by this object.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36FE)  : begin
                                      HrStr := 'MF_E_INVALID_WORKQUEUE';
                                      HrDescr := 'The operation failed because an invalid combination of workqueue ID and flags was specified.';
                                    end;
    _HRESULT_TYPEDEF_($C00D36FF)  : begin
                                      HrStr := 'MF_E_DRM_UNSUPPORTED';
                                      HrDescr := 'No DRM support is available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3700)  : begin
                                      HrStr := 'MF_E_UNAUTHORIZED';
                                      HrDescr := 'This operation is not authorized.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3701)  : begin
                                      HrStr := 'MF_E_OUT_OF_RANGE';
                                      HrDescr := 'The value is not in the specified or valid range.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3702)  : begin
                                      HrStr := 'MF_E_INVALID_CODEC_MERIT';
                                      HrDescr := 'The registered codec merit is not valid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3703)  : begin
                                      HrStr := 'MF_E_HW_MFT_FAILED_START_STREAMING';
                                      HrDescr := 'Hardware MFT failed to start streaming due to lack of hardware resources.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3704)  : begin
                                      HrStr := 'MF_E_OPERATION_IN_PROGRESS';
                                      HrDescr := 'The operation is already in progress.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3705)  : begin
                                      HrStr := 'MF_E_HARDWARE_DRM_UNSUPPORTED';
                                      HrDescr := 'No Hardware DRM support is available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3706)  : begin
                                      HrStr := 'MF_E_DURATION_TOO_LONG';
                                      HrDescr := 'The specified duration is too long.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3707)  : begin
                                      HrStr := 'MF_E_OPERATION_UNSUPPORTED_AT_D3D_FEATURE_LEVEL';
                                      HrDescr := 'The attempted call or command is not supported with the DirectX version used by the component.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3708)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_MEDIATYPE_AT_D3D_FEATURE_LEVEL';
                                      HrDescr := 'The specified media type is not supported with the DirectX version used by the component.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3709)  : begin
                                      HrStr := 'MF_S_ASF_PARSEINPROGRESS';
                                      HrDescr := 'Parsing is still in progress and is not yet complete.';
                                    end;
    _HRESULT_TYPEDEF_($400D3A98)  : begin
                                      HrStr := 'MF_E_ASF_PARSINGINCOMPLETE';
                                      HrDescr := 'Not enough data have been parsed to carry out the requested action.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A98)  : begin
                                      HrStr := 'MF_E_ASF_MISSINGDATA';
                                      HrDescr := 'There is a gap in the ASF data provided.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A99)  : begin
                                      HrStr := 'MF_E_ASF_INVALIDDATA';
                                      HrDescr := 'The data provided are not valid ASF.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A9A)  : begin
                                      HrStr := 'MF_E_ASF_OPAQUEPACKET';
                                      HrDescr := 'The packet is opaque, so the requested information cannot be returned.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A9B)  : begin
                                      HrStr := 'MF_E_ASF_NOINDEX';
                                      HrDescr := 'The requested operation failed since there is no appropriate ASF index.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A9C)  : begin
                                      HrStr := 'MF_E_ASF_OUTOFRANGE';
                                      HrDescr := 'The value supplied is out of range for this operation.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A9D)  : begin
                                      HrStr := 'MF_E_ASF_INDEXNOTLOADED';
                                      HrDescr := 'The index entry requested needs to be loaded before it can be available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A9E)  : begin
                                      HrStr := 'MF_E_ASF_TOO_MANY_PAYLOADS';
                                      HrDescr := 'The packet has reached the maximum number of payloads.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3A9F)  : begin
                                      HrStr := 'MF_E_ASF_UNSUPPORTED_STREAM_TYPE';
                                      HrDescr := 'Stream type is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3AA0)  : begin
                                      HrStr := 'MF_E_ASF_DROPPED_PACKET';
                                      HrDescr := 'One or more ASF packets were dropped.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3AA1)  : begin
                                      HrStr := 'MF_E_NO_EVENTS_AVAILABLE';
                                      HrDescr := 'There are no events available in the queue.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E80)  : begin
                                      HrStr := 'MF_E_INVALID_STATE_TRANSITION';
                                      HrDescr := 'A media source cannot go from the stopped state to the paused state.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E82)  : begin
                                      HrStr := 'MF_E_END_OF_STREAM';
                                      HrDescr := 'The media stream cannot process any more samples because there are no more samples in the stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E84)  : begin
                                      HrStr := 'MF_E_SHUTDOWN';
                                      HrDescr := 'The request is invalid because Shutdown() has been called.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E85)  : begin
                                      HrStr := 'MF_E_MP3_NOTFOUND';
                                      HrDescr := 'The MP3 object was not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E86)  : begin
                                      HrStr := 'MF_E_MP3_OUTOFDATA';
                                      HrDescr := 'The MP3 parser ran out of data before finding the MP3 object.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E87)  : begin
                                      HrStr := 'MF_E_MP3_NOTMP3';
                                      HrDescr := 'The file is not really a MP3 file.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E88)  : begin
                                      HrStr := 'MF_E_MP3_NOTSUPPORTED';
                                      HrDescr := 'The MP3 file is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E89)  : begin
                                      HrStr := 'MF_E_NO_DURATION';
                                      HrDescr := 'The Media stream has no duration.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E8A)  : begin
                                      HrStr := 'MF_E_INVALID_FORMAT';
                                      HrDescr := 'The Media format is recognized but is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E8C)  : begin
                                      HrStr := 'MF_E_PROPERTY_NOT_FOUND';
                                      HrDescr := 'The property requested was not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E8D)  : begin
                                      HrStr := 'MF_E_PROPERTY_READ_ONLY';
                                      HrDescr := 'The property is read only.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E8E)  : begin
                                      HrStr := 'MF_E_PROPERTY_NOT_ALLOWED';
                                      HrDescr := 'The specified property is not allowed in this context.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E8F)  : begin
                                      HrStr := 'MF_E_MEDIA_SOURCE_NOT_STARTED';
                                      HrDescr := 'The media source is not started.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E91)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_FORMAT';
                                      HrDescr := 'The Media format is recognized but not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E98)  : begin
                                      HrStr := 'MF_E_MP3_BAD_CRC';
                                      HrDescr := 'The MPEG frame has bad CRC.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E99)  : begin
                                      HrStr := 'MF_E_NOT_PROTECTED';
                                      HrDescr := 'The file is not protected.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E9A)  : begin
                                      HrStr := 'MF_E_MEDIA_SOURCE_WRONGSTATE';
                                      HrDescr := 'The media source is in the wrong state';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E9B)  : begin
                                      HrStr := 'MF_E_MEDIA_SOURCE_NO_STREAMS_SELECTED';
                                      HrDescr := 'No streams are selected in source presentation descriptor.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E9C)  : begin
                                      HrStr := 'MF_E_CANNOT_FIND_KEYFRAME_SAMPLE';
                                      HrDescr := 'No key frame sample was found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D3E9D)  : begin
                                      HrStr := 'MF_E_NETWORK_RESOURCE_FAILURE';
                                      HrDescr := 'An attempt to acquire a network resource failed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4268)  : begin
                                      HrStr := 'MF_E_NET_WRITE';
                                      HrDescr := 'Error writing to the network.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4269)  : begin
                                      HrStr := 'MF_E_NET_READ';
                                      HrDescr := 'Error reading from the network.';
                                    end;
    _HRESULT_TYPEDEF_($C00D426A)  : begin
                                      HrStr := 'MF_E_NET_REQUIRE_NETWORK';
                                      HrDescr := 'Internal. Entry cannot complete operation without network.';
                                    end;
    _HRESULT_TYPEDEF_($C00D426B)  : begin
                                      HrStr := 'MF_E_NET_REQUIRE_ASYNC';
                                      HrDescr := 'Internal. Async op is required.';
                                    end;
    _HRESULT_TYPEDEF_($C00D426C)  : begin
                                      HrStr := 'MF_E_NET_BWLEVEL_NOT_SUPPORTED';
                                      HrDescr := 'Internal. Bandwidth levels are not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D426D)  : begin
                                      HrStr := 'MF_E_NET_STREAMGROUPS_NOT_SUPPORTED';
                                      HrDescr := 'Internal. Stream groups are not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D426E)  : begin
                                      HrStr := 'MF_E_NET_MANUALSS_NOT_SUPPORTED';
                                      HrDescr := 'Manual stream selection is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D426F)  : begin
                                      HrStr := 'MF_E_NET_INVALID_PRESENTATION_DESCRIPTOR';
                                      HrDescr := 'Invalid presentation descriptor.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4270)  : begin
                                      HrStr := 'MF_E_NET_CACHESTREAM_NOT_FOUND';
                                      HrDescr := 'Cannot find cache stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4271)  : begin
                                      HrStr := 'MF_I_MANUAL_PROXY';
                                      HrDescr := 'The proxy setting is manual.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4274)  : begin
                                      HrStr := 'MF_E_NET_REQUIRE_INPUT';
                                      HrDescr := 'Internal. Entry cannot complete operation without input.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4275)  : begin
                                      HrStr := 'MF_E_NET_REDIRECT';
                                      HrDescr := 'The client redirected to another server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4276)  : begin
                                      HrStr := 'MF_E_NET_REDIRECT_TO_PROXY';
                                      HrDescr := 'The client is redirected to a proxy server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4277)  : begin
                                      HrStr := 'MF_E_NET_TOO_MANY_REDIRECTS';
                                      HrDescr := 'The client reached maximum redirection limit.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4278)  : begin
                                      HrStr := 'MF_E_NET_TIMEOUT';
                                      HrDescr := 'The server, a computer set up to offer multimedia content to other computers, could not handle your request for multimedia content in a timely manner.  Please try again later.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4279)  : begin
                                      HrStr := 'MF_E_NET_CLIENT_CLOSE';
                                      HrDescr := 'The control socket is closed by the client.';
                                    end;
    _HRESULT_TYPEDEF_($C00D427A)  : begin
                                      HrStr := 'MF_E_NET_BAD_CONTROL_DATA';
                                      HrDescr := 'The server received invalid data from the client on the control connection.';
                                    end;
    _HRESULT_TYPEDEF_($C00D427B)  : begin
                                      HrStr := 'MF_E_NET_INCOMPATIBLE_SERVER';
                                      HrDescr := 'The server is not a compatible streaming media server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D427C)  : begin
                                      HrStr := 'MF_E_NET_UNSAFE_URL';
                                      HrDescr := 'Url.';
                                    end;
    _HRESULT_TYPEDEF_($C00D427D)  : begin
                                      HrStr := 'MF_E_NET_CACHE_NO_DATA';
                                      HrDescr := 'Data is not available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D427E)  : begin
                                      HrStr := 'MF_E_NET_EOL';
                                      HrDescr := 'End of line.';
                                    end;
    _HRESULT_TYPEDEF_($C00D427F)  : begin
                                      HrStr := 'MF_E_NET_BAD_REQUEST';
                                      HrDescr := 'The request could not be understood by the server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4280)  : begin
                                      HrStr := 'MF_E_NET_INTERNAL_SERVER_ERROR';
                                      HrDescr := 'The server encountered an unexpected condition which prevented it from fulfilling the request.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4281)  : begin
                                      HrStr := 'MF_E_NET_SESSION_NOT_FOUND';
                                      HrDescr := 'Session not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4282)  : begin
                                      HrStr := 'MF_E_NET_NOCONNECTION';
                                      HrDescr := 'There is no connection established with the Windows Media server. The operation failed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4283)  : begin
                                      HrStr := 'MF_E_NET_CONNECTION_FAILURE';
                                      HrDescr := 'The network connection has failed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4284)  : begin
                                      HrStr := 'MF_E_NET_INCOMPATIBLE_PUSHSERVER';
                                      HrDescr := 'The Server service that received the HTTP push request is not a compatible version of Windows Media Services (WMS).' +
                                                 'This error may indicate the push request was received by IIS instead of WMS.' +
                                                 'Ensure WMS is started and has the HTTP Server control protocol properly enabled and try again.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4285)  : begin
                                      HrStr := 'MF_E_NET_SERVER_ACCESSDENIED';
                                      HrDescr := 'The Windows Media server is denying access.  The username and/or password might be incorrect.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4286)  : begin
                                      HrStr := 'MF_E_NET_PROXY_ACCESSDENIED';
                                      HrDescr := 'The proxy server is denying access.  The username and/or password might be incorrect.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4287)  : begin
                                      HrStr := 'MF_E_NET_CANNOTCONNECT';
                                      HrDescr := 'Unable to establish a connection to the server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4288)  : begin
                                      HrStr := 'MF_E_NET_INVALID_PUSH_TEMPLATE';
                                      HrDescr := 'The specified push template is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4289)  : begin
                                      HrStr := 'MF_E_NET_INVALID_PUSH_PUBLISHING_POINT';
                                      HrDescr := 'The specified push publishing point is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D428A)  : begin
                                      HrStr := 'MF_E_NET_BUSY';
                                      HrDescr := 'The requested resource is in use.';
                                    end;
    _HRESULT_TYPEDEF_($C00D428B)  : begin
                                      HrStr := 'MF_E_NET_RESOURCE_GONE';
                                      HrDescr := 'The Publishing Point or file on the Windows Media Server is no longer available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D428C)  : begin
                                      HrStr := 'MF_E_NET_ERROR_FROM_PROXY';
                                      HrDescr := 'The proxy experienced an error while attempting to contact the media server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D428D)  : begin
                                      HrStr := 'MF_E_NET_PROXY_TIMEOUT';
                                      HrDescr := 'The proxy did not receive a timely response while attempting to contact the media server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D428E)  : begin
                                      HrStr := 'MF_E_NET_SERVER_UNAVAILABLE';
                                      HrDescr := 'The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.';
                                    end;
    _HRESULT_TYPEDEF_($C00D428F)  : begin
                                      HrStr := 'MF_E_NET_TOO_MUCH_DATA';
                                      HrDescr := 'The encoding process was unable to keep up with the amount of supplied data.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4290)  : begin
                                      HrStr := 'MF_E_NET_SESSION_INVALID';
                                      HrDescr := 'Session not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4291)  : begin
                                      HrStr := 'MF_E_OFFLINE_MODE';
                                      HrDescr := 'The requested URL is not available in offline mode.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4292)  : begin
                                      HrStr := 'MF_E_NET_UDP_BLOCKED';
                                      HrDescr := 'A device in the network is blocking UDP traffic.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4293)  : begin
                                      HrStr := 'MF_E_NET_UNSUPPORTED_CONFIGURATION';
                                      HrDescr := 'The specified configuration value is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4294)  : begin
                                      HrStr := 'MF_E_NET_PROTOCOL_DISABLED';
                                      HrDescr := 'The networking protocol is disabled.';
                                    end;

    _HRESULT_TYPEDEF_($C00D4650)  : begin
                                      HrStr := 'MF_E_ALREADY_INITIALIZED';
                                      HrDescr := 'This object has already been initialized and cannot be re-initialized at this time.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4651)  : begin
                                      HrStr := 'MF_E_BANDWIDTH_OVERRUN';
                                      HrDescr := 'The amount of data passed in exceeds the given bitrate and buffer window.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4652)  : begin
                                      HrStr := 'MF_E_LATE_SAMPLE';
                                      HrDescr := 'The sample was passed in too late to be correctly processed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4653)  : begin
                                      HrStr := 'MF_E_FLUSH_NEEDED';
                                      HrDescr := 'The requested action cannot be carried out until the object is flushed and the queue is emptied.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4654)  : begin
                                      HrStr := 'MF_E_INVALID_PROFILE';
                                      HrDescr := 'The profile is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4655)  : begin
                                      HrStr := 'MF_E_INDEX_NOT_COMMITTED';
                                      HrDescr := 'The index that is being generated needs to be committed before the requested action can be carried out.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4656)  : begin
                                      HrStr := 'MF_E_NO_INDEX';
                                      HrDescr := 'The index that is necessary for the requested action is not found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4657)  : begin
                                      HrStr := 'MF_E_CANNOT_INDEX_IN_PLACE';
                                      HrDescr := 'The requested index cannot be added in-place to the specified ASF content.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4658)  : begin
                                      HrStr := 'MF_E_MISSING_ASF_LEAKYBUCKET';
                                      HrDescr := 'The ASF leaky bucket parameters must be specified in order to carry out this request.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4659)  : begin
                                      HrStr := 'MF_E_INVALID_ASF_STREAMID';
                                      HrDescr := 'The stream id is invalid. The valid range for ASF stream id is from 1 to 127.';
                                    end;

    _HRESULT_TYPEDEF_($C00D4A38)  : begin
                                      HrStr := 'MF_E_STREAMSINK_REMOVED';
                                      HrDescr := 'The requested Stream Sink has been removed and cannot be used.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A3A)  : begin
                                      HrStr := 'MF_E_STREAMSINKS_OUT_OF_SYNC';
                                      HrDescr := 'The various Stream Sinks in this Media Sink are too far out of sync for the requested action to take place.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A3B)  : begin
                                      HrStr := 'MF_E_STREAMSINKS_FIXED';
                                      HrDescr := 'Stream Sinks cannot be added to or removed from this Media Sink because its set of streams is fixed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A3C)  : begin
                                      HrStr := 'MF_E_STREAMSINK_EXISTS';
                                      HrDescr := 'The given Stream Sink already exists.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A3D)  : begin
                                      HrStr := 'MF_E_SAMPLEALLOCATOR_CANCELED';
                                      HrDescr := 'Sample allocations have been canceled.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A3E)  : begin
                                      HrStr := 'MF_E_SAMPLEALLOCATOR_EMPTY';
                                      HrDescr := 'The sample allocator is currently empty, due to outstanding requests.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A3F)  : begin
                                      HrStr := 'MF_E_SINK_ALREADYSTOPPED';
                                      HrDescr := 'When we try to sopt a stream sink, it is already stopped ';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A40)  : begin
                                      HrStr := 'MF_E_ASF_FILESINK_BITRATE_UNKNOWN';
                                      HrDescr := 'The ASF file sink could not reserve AVIO because the bitrate is unknown.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A41)  : begin
                                      HrStr := 'MF_E_SINK_NO_STREAMS';
                                      HrDescr := 'No streams are selected in sink presentation descriptor.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A42)  : begin
                                      HrStr := 'MF_S_SINK_NOT_FINALIZED';
                                      HrDescr := 'The sink has not been finalized before shut down. This may cause sink generate a corrupted content.';
                                    end;
    _HRESULT_TYPEDEF_($000D4A43)  : begin
                                      HrStr := 'MF_E_METADATA_TOO_LONG';
                                      HrDescr := 'A metadata item was too long to write to the output container.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A44)  : begin
                                      HrStr := 'MF_E_SINK_NO_SAMPLES_PROCESSED';
                                      HrDescr := 'The operation failed because no samples were processed by the sink.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4A45)  : begin
                                      HrStr := 'MF_E_SINK_HEADERS_NOT_FOUND';
                                      HrDescr := 'Sink could not create valid output file because required headers were not provided to the sink.';
                                    end;

    _HRESULT_TYPEDEF_($C00D4E20)  : begin
                                      HrStr := 'MF_E_VIDEO_REN_NO_PROCAMP_HW';
                                      HrDescr := 'There is no available procamp hardware with which to perform color correction.';
                                    end;

    _HRESULT_TYPEDEF_($C00D4E21)  : begin
                                      HrStr := 'MF_E_VIDEO_REN_NO_DEINTERLACE_HW';
                                      HrDescr := 'There is no available deinterlacing hardware with which to deinterlace the video stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E22)  : begin
                                      HrStr := 'MF_E_VIDEO_REN_COPYPROT_FAILED';
                                      HrDescr := 'A video stream requires copy protection to be enabled, but there was a failure in attempting to enable copy protection.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E23)  : begin
                                      HrStr := 'MF_E_VIDEO_REN_SURFACE_NOT_SHARED';
                                      HrDescr := 'A component is attempting to access a surface for sharing that is not shared.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E24)  : begin
                                      HrStr := 'MF_E_VIDEO_DEVICE_LOCKED';
                                      HrDescr := 'A component is attempting to access a shared device that is already locked by another component.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E25)  : begin
                                      HrStr := 'MF_E_NEW_VIDEO_DEVICE';
                                      HrDescr := 'The device is no longer available. The handle should be closed and a new one opened.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E26)  : begin
                                      HrStr := 'MF_E_NO_VIDEO_SAMPLE_AVAILABLE';
                                      HrDescr := 'A video sample is not currently queued on a stream that is required for mixing.';
                                    end;

    _HRESULT_TYPEDEF_($C00D4E84)  : begin
                                      HrStr := 'MF_E_NO_AUDIO_PLAYBACK_DEVICE';
                                      HrDescr := 'No audio playback device was found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E85)  : begin
                                      HrStr := 'MF_E_AUDIO_PLAYBACK_DEVICE_IN_USE';
                                      HrDescr := 'The requested audio playback device is currently in use.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E86)  : begin
                                      HrStr := 'MF_E_AUDIO_PLAYBACK_DEVICE_INVALIDATED';
                                      HrDescr := 'The audio playback device is no longer present.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E87)  : begin
                                      HrStr := 'MF_E_AUDIO_SERVICE_NOT_RUNNING';
                                      HrDescr := 'The audio service is not running.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E88)  : begin
                                      HrStr := 'MF_E_AUDIO_BUFFER_SIZE_ERROR';
                                      HrDescr := 'The audio renderer encountered an error trying to set the render buffer size.';
                                    end;
    _HRESULT_TYPEDEF_($C00D4E89)  : begin
                                      HrStr := 'MF_E_AUDIO_CLIENT_WRAPPER_SPOOF_ERROR';
                                      HrDescr := 'The audio renderer encountered an error trying spoof the invalidated audio client.';
                                    end;

    _HRESULT_TYPEDEF_($C00D520E)  : begin
                                      HrStr := 'MF_E_TOPO_INVALID_OPTIONAL_NODE';
                                      HrDescr := 'The topology contains an invalid optional node.  Possible reasons are incorrect number of outputs and inputs or optional node is at the beginning or end of a segment. ';
                                    end;
    _HRESULT_TYPEDEF_($C00D5211)  : begin
                                      HrStr := 'MF_E_TOPO_CANNOT_FIND_DECRYPTOR';
                                      HrDescr := 'No suitable transform was found to decrypt the content. ';
                                    end;
    _HRESULT_TYPEDEF_($C00D5212)  : begin
                                      HrStr := 'MF_E_TOPO_CODEC_NOT_FOUND';
                                      HrDescr := 'No suitable transform was found to encode or decode the content. ';
                                    end;
    _HRESULT_TYPEDEF_($C00D5213)  : begin
                                      HrStr := 'MF_E_TOPO_CANNOT_CONNECT';
                                      HrDescr := 'Unable to find a way to connect nodes';
                                    end;
    _HRESULT_TYPEDEF_($C00D5214)  : begin
                                      HrStr := 'MF_E_TOPO_UNSUPPORTED';
                                      HrDescr := 'Unsupported operations in topoloader';
                                    end;
    _HRESULT_TYPEDEF_($C00D5215)  : begin
                                      HrStr := 'MF_E_TOPO_INVALID_TIME_ATTRIBUTES';
                                      HrDescr := 'The topology or its nodes contain incorrectly set time attributes';
                                    end;
    _HRESULT_TYPEDEF_($C00D5216)  : begin
                                      HrStr := 'MF_E_TOPO_LOOPS_IN_TOPOLOGY';
                                      HrDescr := 'The topology contains loops, which are unsupported in media foundation topologies';
                                    end;
    _HRESULT_TYPEDEF_($C00D5217)  : begin
                                      HrStr := 'MF_E_TOPO_MISSING_PRESENTATION_DESCRIPTOR';
                                      HrDescr := 'A source stream node in the topology does not have a presentation descriptor';
                                    end;
    _HRESULT_TYPEDEF_($C00D5218)  : begin
                                      HrStr := 'MF_E_TOPO_MISSING_STREAM_DESCRIPTOR';
                                      HrDescr := 'A source stream node in the topology does not have a stream descriptor';
                                    end;
    _HRESULT_TYPEDEF_($C00D5219)  : begin
                                      HrStr := 'MF_E_TOPO_STREAM_DESCRIPTOR_NOT_SELECTED';
                                      HrDescr := 'A stream descriptor was set on a source stream node but it was not selected on the presentation descriptor';
                                    end;
    _HRESULT_TYPEDEF_($C00D521A)  : begin
                                      HrStr := 'MF_E_TOPO_MISSING_SOURCE';
                                      HrDescr := 'A source stream node in the topology does not have a source';
                                    end;
    _HRESULT_TYPEDEF_($C00D521B)  : begin
                                      HrStr := 'MF_E_TOPO_SINK_ACTIVATES_UNSUPPORTED';
                                      HrDescr := 'The topology loader does not support sink activates on output nodes.';
                                    end;

    _HRESULT_TYPEDEF_($C00D61AC)  : begin
                                      HrStr := 'MF_E_SEQUENCER_UNKNOWN_SEGMENT_ID';
                                      HrDescr := 'The sequencer cannot find a segment with the given ID.\n.';
                                    end;
    _HRESULT_TYPEDEF_($C00D61AD)  : begin
                                      HrStr := 'MF_S_SEQUENCER_CONTEXT_CANCELED';
                                      HrDescr := 'The context was canceled.\n.';
                                    end;
    _HRESULT_TYPEDEF_($C00D61AE)  : begin
                                      HrStr := 'MF_E_NO_SOURCE_IN_CACHE';
                                      HrDescr := 'Cannot find source in source cache.\n.';
                                    end;
    _HRESULT_TYPEDEF_($C00D61AF)  : begin
                                      HrStr := 'MF_S_SEQUENCER_SEGMENT_AT_END_OF_STREAM';
                                      HrDescr := 'Cannot update topology flags.\n.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D60)  : begin
                                      HrStr := 'MF_E_TRANSFORM_TYPE_NOT_SET';
                                      HrDescr := 'A valid type has not been set for this stream or a stream that it depends on.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D61)  : begin
                                      HrStr := 'MF_E_TRANSFORM_STREAM_CHANGE';
                                      HrDescr := 'A stream change has occurred. Output cannot be produced until the streams have been renegotiated.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D62)  : begin
                                      HrStr := 'MF_E_TRANSFORM_INPUT_REMAINING';
                                      HrDescr := 'The transform cannot take the requested action until all of the input data it currently holds is processed or flushed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D63)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROFILE_MISSING';
                                      HrDescr := 'The transform requires a profile but no profile was supplied or found.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D64)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROFILE_INVALID_OR_CORRUPT';
                                      HrDescr := 'The transform requires a profile but the supplied profile was invalid or corrupt.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D65)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROFILE_TRUNCATED';
                                      HrDescr := 'The transform requires a profile but the supplied profile ended unexpectedly while parsing.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D66)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_PID_NOT_RECOGNIZED';
                                      HrDescr := 'The property ID does not match any property supported by the transform.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D67)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_VARIANT_TYPE_WRONG';
                                      HrDescr := 'The variant does not have the type expected for this property ID.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D68)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_NOT_WRITEABLE';
                                      HrDescr := 'An attempt was made to set the value on a read-only property.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D69)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_ARRAY_VALUE_WRONG_NUM_DIM';
                                      HrDescr := 'The array property value has an unexpected number of dimensions.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D6A)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_VALUE_SIZE_WRONG';
                                      HrDescr := 'The array or blob property value has an unexpected size.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D6B)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_VALUE_OUT_OF_RANGE';
                                      HrDescr := 'The property value is out of range for this transform.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D6C)  : begin
                                      HrStr := 'MF_E_TRANSFORM_PROPERTY_VALUE_INCOMPATIBLE';
                                      HrDescr := 'The property value is incompatible with some other property or mediatype set on the transform.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D6D)  : begin
                                      HrStr := 'MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_OUTPUT_MEDIATYPE';
                                      HrDescr := 'The requested operation is not supported for the currently set output mediatype.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D6E)  : begin
                                      HrStr := 'MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_INPUT_MEDIATYPE';
                                      HrDescr := 'The requested operation is not supported for the currently set input mediatype.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D6F)  : begin
                                      HrStr := 'MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_MEDIATYPE_COMBINATION';
                                      HrDescr := 'The requested operation is not supported for the currently set combination of mediatypes.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D70)  : begin
                                      HrStr := 'MF_E_TRANSFORM_CONFLICTS_WITH_OTHER_CURRENTLY_ENABLED_FEATURES';
                                      HrDescr := 'The requested feature is not supported in combination with some other currently enabled feature.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D72)  : begin
                                      HrStr := 'MF_E_TRANSFORM_NEED_MORE_INPUT';
                                      HrDescr := 'The transform cannot produce output until it gets more input samples.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D73)  : begin
                                      HrStr := 'MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_SPKR_CONFIG';
                                      HrDescr := 'The requested operation is not supported for the current speaker configuration.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D74)  : begin
                                      HrStr := 'MF_E_TRANSFORM_CANNOT_CHANGE_MEDIATYPE_WHILE_PROCESSING';
                                      HrDescr := 'The transform cannot accept mediatype changes in the middle of processing.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D75)  : begin
                                      HrStr := 'MF_S_TRANSFORM_DO_NOT_PROPAGATE_EVENT';
                                      HrDescr := 'The caller should not propagate this event to downstream components.';
                                    end;
    _HRESULT_TYPEDEF_($000D6D76)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_D3D_TYPE';
                                      HrDescr := 'The input type is not supported for D3D device.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D77)  : begin
                                      HrStr := 'MF_E_TRANSFORM_ASYNC_LOCKED';
                                      HrDescr := 'The caller does not appear to support this transform''s asynchronous capabilities.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D78)  : begin
                                      HrStr := 'MF_E_TRANSFORM_CANNOT_INITIALIZE_ACM_DRIVER';
                                      HrDescr := 'An audio compression manager driver could not be initialized by the transform.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D79)  : begin
                                      HrStr := 'MF_E_TRANSFORM_STREAM_INVALID_RESOLUTION';
                                      HrDescr := 'The input stream has invalid and illegal resolution. Output should stop on next ProcessOutput call after the invalid and illegal resolution is detected.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D7A)  : begin
                                      HrStr := 'MF_E_TRANSFORM_ASYNC_MFT_NOT_SUPPORTED';
                                      HrDescr := 'The transform cannot be asynchronous in current context.';
                                    end;
    _HRESULT_TYPEDEF_($C00D6D7C)  : begin
                                      HrStr := 'MF_E_TRANSFORM_EXATTRIBUTE_NOT_SUPPORTED';
                                      HrDescr := 'It is not supported in the current context to have the transform copy attributes from an input sample to an output sample.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7148)  : begin
                                      HrStr := 'MF_E_LICENSE_INCORRECT_RIGHTS';
                                      HrDescr := 'You are not allowed to open this file. Contact the content provider for further assistance.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7149)  : begin
                                      HrStr := 'MF_E_LICENSE_OUTOFDATE';
                                      HrDescr := 'The license for this media file has expired. Get a new license or contact the content provider for further assistance.';
                                    end;
    _HRESULT_TYPEDEF_($C00D714A)  : begin
                                      HrStr := 'MF_E_LICENSE_REQUIRED';
                                      HrDescr := 'You need a license to perform the requested operation on this media file.';
                                    end;
    _HRESULT_TYPEDEF_($C00D714B)  : begin
                                      HrStr := 'MF_E_DRM_HARDWARE_INCONSISTENT';
                                      HrDescr := 'The licenses for your media files are corrupted. Contact Microsoft product support.';
                                    end;
    _HRESULT_TYPEDEF_($C00D714C)  : begin
                                      HrStr := 'MF_E_NO_CONTENT_PROTECTION_MANAGER';
                                      HrDescr := 'The APP needs to provide IMFContentProtectionManager callback to access the protected media file.';
                                    end;
    _HRESULT_TYPEDEF_($C00D714D)  : begin
                                      HrStr := 'MF_E_LICENSE_RESTORE_NO_RIGHTS';
                                      HrDescr := 'Client does not have rights to restore licenses.';
                                    end;
    _HRESULT_TYPEDEF_($C00D714E)  : begin
                                      HrStr := 'MF_E_BACKUP_RESTRICTED_LICENSE';
                                      HrDescr := 'Licenses are restricted and hence can not be backed up.';
                                    end;
    _HRESULT_TYPEDEF_($C00D714F)  : begin
                                      HrStr := 'MF_E_LICENSE_RESTORE_NEEDS_INDIVIDUALIZATION';
                                      HrDescr := 'License restore requires machine to be individualized.';
                                    end;
    _HRESULT_TYPEDEF_($000D7150)  : begin
                                      HrStr := 'MF_S_PROTECTION_NOT_REQUIRED';
                                      HrDescr := 'Protection for stream is not required.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7151)  : begin
                                      HrStr := 'MF_E_COMPONENT_REVOKED';
                                      HrDescr := 'Component is revoked.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7152)  : begin
                                      HrStr := 'MF_E_TRUST_DISABLED';
                                      HrDescr := 'Trusted functionality is currently disabled on this component.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7153)  : begin
                                      HrStr := 'MF_E_WMDRMOTA_NO_ACTION';
                                      HrDescr := 'No Action is set on WMDRM Output Trust Authority.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7154)  : begin
                                      HrStr := 'MF_E_WMDRMOTA_ACTION_ALREADY_SET';
                                      HrDescr := 'Action is already set on WMDRM Output Trust Authority.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7155)  : begin
                                      HrStr := 'MF_E_WMDRMOTA_DRM_HEADER_NOT_AVAILABLE';
                                      HrDescr := 'DRM Heaader is not available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7156)  : begin
                                      HrStr := 'MF_E_WMDRMOTA_DRM_ENCRYPTION_SCHEME_NOT_SUPPORTED';
                                      HrDescr := 'Current encryption scheme is not supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7157)  : begin
                                      HrStr := 'MF_E_WMDRMOTA_ACTION_MISMATCH';
                                      HrDescr := 'Action does not match with current configuration.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7158)  : begin
                                      HrStr := 'MF_E_WMDRMOTA_INVALID_POLICY';
                                      HrDescr := 'Invalid policy for WMDRM Output Trust Authority.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7159)  : begin
                                      HrStr := 'MF_E_POLICY_UNSUPPORTED';
                                      HrDescr := 'The policies that the Input Trust Authority requires to be enforced are unsupported by the outputs.';
                                    end;
    _HRESULT_TYPEDEF_($C00D715A)  : begin
                                      HrStr := 'MF_E_OPL_NOT_SUPPORTED';
                                      HrDescr := 'The OPL that the license requires to be enforced are not supported by the Input Trust Authority.';
                                    end;
    _HRESULT_TYPEDEF_($C00D715B)  : begin
                                      HrStr := 'MF_E_TOPOLOGY_VERIFICATION_FAILED';
                                      HrDescr := 'The topology could not be successfully verified.';
                                    end;
    _HRESULT_TYPEDEF_($C00D715C)  : begin
                                      HrStr := 'MF_E_SIGNATURE_VERIFICATION_FAILED';
                                      HrDescr := 'Signature verification could not be completed successfully for this component.';
                                    end;
    _HRESULT_TYPEDEF_($C00D715D)  : begin
                                      HrStr := 'MF_E_DEBUGGING_NOT_ALLOWED';
                                      HrDescr := 'Running this process under a debugger while using protected content is not allowed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D715E)  : begin
                                      HrStr := 'MF_E_CODE_EXPIRED';
                                      HrDescr := 'MF component has expired.';
                                    end;
    _HRESULT_TYPEDEF_($C00D715F)  : begin
                                      HrStr := 'MF_E_GRL_VERSION_TOO_LOW';
                                      HrDescr := 'The current GRL on the machine does not meet the minimum version requirements.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7160)  : begin
                                      HrStr := 'MF_E_GRL_RENEWAL_NOT_FOUND';
                                      HrDescr := 'The current GRL on the machine does not contain any renewal entries for the specified revocation.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7161)  : begin
                                      HrStr := 'MF_E_GRL_EXTENSIBLE_ENTRY_NOT_FOUND';
                                      HrDescr := 'The current GRL on the machine does not contain any extensible entries for the specified extension GUID.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7162)  : begin
                                      HrStr := 'MF_E_KERNEL_UNTRUSTED';
                                      HrDescr := 'The kernel isn''t secure for high security level content.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7163)  : begin
                                      HrStr := 'MF_E_PEAUTH_UNTRUSTED';
                                      HrDescr := 'The response from protected environment driver isn''t valid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7165)  : begin
                                      HrStr := 'MF_E_NON_PE_PROCESS';
                                      HrDescr := 'A non-PE process tried to talk to PEAuth.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7167)  : begin
                                      HrStr := 'MF_E_REBOOT_REQUIRED';
                                      HrDescr := 'We need to reboot the machine.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7168)  : begin
                                      HrStr := 'MF_S_WAIT_FOR_POLICY_SET';
                                      HrDescr := 'Protection for this stream is not guaranteed to be enforced until the MEPolicySet event is fired.';
                                    end;
    _HRESULT_TYPEDEF_($000D7169)  : begin
                                      HrStr := 'MF_S_VIDEO_DISABLED_WITH_UNKNOWN_SOFTWARE_OUTPUT';
                                      HrDescr := 'This video stream is disabled because it is being sent to an unknown software output.';
                                    end;
    _HRESULT_TYPEDEF_($C00D716A)  : begin
                                      HrStr := 'MF_E_GRL_INVALID_FORMAT';
                                      HrDescr := 'The GRL file is not correctly formed, it may have been corrupted or overwritten.';
                                    end;
    _HRESULT_TYPEDEF_($C00D716B)  : begin
                                      HrStr := 'MF_E_GRL_UNRECOGNIZED_FORMAT';
                                      HrDescr := 'The GRL file is in a format newer than those recognized by this GRL Reader.';
                                    end;
    _HRESULT_TYPEDEF_($C00D716C)  : begin
                                      HrStr := 'MF_E_ALL_PROCESS_RESTART_REQUIRED';
                                      HrDescr := 'The GRL was reloaded and required all processes that can run protected media to restart.';
                                    end;
    _HRESULT_TYPEDEF_($C00D716D)  : begin
                                      HrStr := 'MF_E_PROCESS_RESTART_REQUIRED';
                                      HrDescr := 'The GRL was reloaded and the current process needs to restart.';
                                    end;
    _HRESULT_TYPEDEF_($C00D716E)  : begin
                                      HrStr := 'MF_E_USERMODE_UNTRUSTED';
                                      HrDescr := 'The user space is untrusted for protected content play.';
                                    end;
    _HRESULT_TYPEDEF_($C00D716F)  : begin
                                      HrStr := 'MF_E_PEAUTH_SESSION_NOT_STARTED';
                                      HrDescr := 'PEAuth communication session hasn''t been started.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7171)  : begin
                                      HrStr := 'MF_E_PEAUTH_PUBLICKEY_REVOKED';
                                      HrDescr := 'PEAuth''s public key is revoked.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7172)  : begin
                                      HrStr := 'MF_E_GRL_ABSENT';
                                      HrDescr := 'The GRL is absent.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7173)  : begin
                                      HrStr := 'MF_S_PE_TRUSTED';
                                      HrDescr := 'The Protected Environment is trusted.';
                                    end;
    _HRESULT_TYPEDEF_($000D7174)  : begin
                                      HrStr := 'MF_E_PE_UNTRUSTED';
                                      HrDescr := 'The Protected Environment is untrusted.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7175)  : begin
                                      HrStr := 'MF_E_PEAUTH_NOT_STARTED';
                                      HrDescr := 'The Protected Environment Authorization service (PEAUTH) has not been started.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7176)  : begin
                                      HrStr := 'MF_E_INCOMPATIBLE_SAMPLE_PROTECTION';
                                      HrDescr := 'The sample protection algorithms supported by components are not compatible.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7177)  : begin
                                      HrStr := 'MF_E_PE_SESSIONS_MAXED';
                                      HrDescr := 'No more protected environment sessions can be supported.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7178)  : begin
                                      HrStr := 'MF_E_HIGH_SECURITY_LEVEL_CONTENT_NOT_ALLOWED';
                                      HrDescr := 'WMDRM ITA does not allow protected content with high security level for this release.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7179)  : begin
                                      HrStr := 'MF_E_TEST_SIGNED_COMPONENTS_NOT_ALLOWED';
                                      HrDescr := 'WMDRM ITA cannot allow the requested action for the content as one or more components is not properly signed.';
                                    end;
    _HRESULT_TYPEDEF_($C00D717A)  : begin
                                      HrStr := 'MF_E_ITA_UNSUPPORTED_ACTION';
                                      HrDescr := 'WMDRM ITA does not support the requested action.';
                                    end;
    _HRESULT_TYPEDEF_($C00D717B)  : begin
                                      HrStr := 'MF_E_ITA_ERROR_PARSING_SAP_PARAMETERS';
                                      HrDescr := 'WMDRM ITA encountered an error in parsing the Secure Audio Path parameters.';
                                    end;
    _HRESULT_TYPEDEF_($C00D717C)  : begin
                                      HrStr := 'MF_E_POLICY_MGR_ACTION_OUTOFBOUNDS';
                                      HrDescr := 'The Policy Manager action passed in is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D717D)  : begin
                                      HrStr := 'MF_E_BAD_OPL_STRUCTURE_FORMAT';
                                      HrDescr := 'The structure specifying Output Protection Level is not the correct format.';
                                    end;
    _HRESULT_TYPEDEF_($C00D717E)  : begin
                                      HrStr := 'MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_PROTECTION_GUID';
                                      HrDescr := 'WMDRM ITA does not recognize the Explicite Analog Video Output Protection guid specified in the license.';
                                    end;
    _HRESULT_TYPEDEF_($C00D717F)  : begin
                                      HrStr := 'MF_E_NO_PMP_HOST';
                                      HrDescr := 'IMFPMPHost object not available.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7180)  : begin
                                      HrStr := 'MF_E_ITA_OPL_DATA_NOT_INITIALIZED';
                                      HrDescr := 'WMDRM ITA could not initialize the Output Protection Level data.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7181)  : begin
                                      HrStr := 'MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_OUTPUT';
                                      HrDescr := 'WMDRM ITA does not recognize the Analog Video Output specified by the OTA.';
                                    end;
    _HRESULT_TYPEDEF_($C00D7182)  : begin
                                      HrStr := 'MF_E_ITA_UNRECOGNIZED_DIGITAL_VIDEO_OUTPUT';
                                      HrDescr := 'WMDRM ITA does not recognize the Digital Video Output specified by the OTA.';
                                    end;
    _HRESULT_TYPEDEF_($C00D9C40)  : begin
                                      HrStr := 'MF_E_CLOCK_INVALID_CONTINUITY_KEY';
                                      HrDescr := 'The continuity key supplied is not currently valid.';
                                    end;
    _HRESULT_TYPEDEF_($C00D9C41)  : begin
                                      HrStr := 'MF_E_CLOCK_NO_TIME_SOURCE';
                                      HrDescr := 'No Presentation Time Source has been specified.';
                                    end;
    _HRESULT_TYPEDEF_($C00D9C42)  : begin
                                      HrStr := 'MF_E_CLOCK_STATE_ALREADY_SET';
                                      HrDescr := 'The clock is already in the requested state.';
                                    end;
    _HRESULT_TYPEDEF_($C00D9C43)  : begin
                                      HrStr := 'MF_E_CLOCK_NOT_SIMPLE';
                                      HrDescr := 'The clock has too many advanced features to carry out the request.';
                                    end;
    _HRESULT_TYPEDEF_($000D9C44)  : begin
                                      HrStr := 'MF_S_CLOCK_STOPPED';
                                      HrDescr := 'Timer.SetTimer returns this success code if called happened while timer is stopped. Timer is not going to be dispatched until clock is running';
                                    end;
    _HRESULT_TYPEDEF_($000D9C45)  : begin
                                      HrStr := 'MF_E_CLOCK_AUDIO_DEVICE_POSITION_UNEXPECTED';
                                      HrDescr := 'The clock can''t return a valid time because the audio position returned from the audio playback device is unexpected.';
                                    end;
    _HRESULT_TYPEDEF_($000D9C46)  : begin
                                      HrStr := 'MF_E_CLOCK_AUDIO_RENDER_POSITION_UNEXPECTED';
                                      HrDescr := 'The clock can''t return a valid render time because the audio position specified to the clock is unexpected.';
                                    end;
    _HRESULT_TYPEDEF_($000D9C47)  : begin
                                      HrStr := 'MF_E_CLOCK_AUDIO_RENDER_TIME_UNEXPECTED';
                                      HrDescr := 'The clock can''t return a valid render position because the timestamp specified to the clock is unexpected.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA028)  : begin
                                      HrStr := 'MF_E_NO_MORE_DROP_MODES';
                                      HrDescr := 'The component does not support any more drop modes.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA029)  : begin
                                      HrStr := 'MF_E_NO_MORE_QUALITY_LEVELS';
                                      HrDescr := 'The component does not support any more quality levels.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA02A)  : begin
                                      HrStr := 'MF_E_DROPTIME_NOT_SUPPORTED';
                                      HrDescr := 'The component does not support drop time functionality.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA02B)  : begin
                                      HrStr := 'MF_E_QUALITYKNOB_WAIT_LONGER';
                                      HrDescr := 'Quality Manager needs to wait longer before bumping the Quality Level up.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA02C)  : begin
                                      HrStr := 'MF_E_QM_INVALIDSTATE';
                                      HrDescr := 'Quality Manager is in an invalid state. Quality Management is off at this moment.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA410)  : begin
                                      HrStr := 'MF_E_TRANSCODE_NO_CONTAINERTYPE';
                                      HrDescr := 'No transcode output container type is specified.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA411)  : begin
                                      HrStr := 'MF_E_TRANSCODE_PROFILE_NO_MATCHING_STREAMS';
                                      HrDescr := 'The profile does not have a media type configuration for any selected source streams.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA412)  : begin
                                      HrStr := 'MF_E_TRANSCODE_NO_MATCHING_ENCODER';
                                      HrDescr := 'Cannot find an encoder MFT that accepts the user preferred output type.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA7F8)  : begin
                                      HrStr := 'MF_E_ALLOCATOR_NOT_INITIALIZED';
                                      HrDescr := 'Memory allocator is not initialized.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA7F9)  : begin
                                      HrStr := 'MF_E_ALLOCATOR_NOT_COMMITED';
                                      HrDescr := 'Memory allocator is not committed yet.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA7FA)  : begin
                                      HrStr := 'MF_E_ALLOCATOR_ALREADY_COMMITED';
                                      HrDescr := 'Memory allocator has already been committed.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA7FB)  : begin
                                      HrStr := 'MF_E_STREAM_ERROR';
                                      HrDescr := 'An error occurred in media stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA7FC)  : begin
                                      HrStr := 'MF_E_INVALID_STREAM_STATE';
                                      HrDescr := 'Stream is not in a state to handle the request.';
                                    end;
    _HRESULT_TYPEDEF_($C00DA7FD)  : begin
                                      HrStr := 'MF_E_HW_STREAM_NOT_CONNECTED';
                                      HrDescr := 'Hardware stream is not connected yet.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE0)  : begin
                                      HrStr := 'MF_E_NO_CAPTURE_DEVICES_AVAILABLE';
                                      HrDescr := 'No capture devices are available.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE1)  : begin
                                      HrStr := 'MF_E_CAPTURE_SINK_OUTPUT_NOT_SET';
                                      HrDescr := 'No output was set for recording.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE2)  : begin
                                      HrStr := 'MF_E_CAPTURE_SINK_MIRROR_ERROR';
                                      HrDescr := 'The current capture sink configuration does not support mirroring.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE3)  : begin
                                      HrStr := 'MF_E_CAPTURE_SINK_ROTATE_ERROR';
                                      HrDescr := 'The current capture sink configuration does not support rotation.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE4)  : begin
                                      HrStr := 'MF_E_CAPTURE_ENGINE_INVALID_OP';
                                      HrDescr := 'The op is invalid.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE5)  : begin
                                      HrStr := 'MF_E_CAPTURE_ENGINE_ALL_EFFECTS_REMOVED';
                                      HrDescr := 'The effects previously added were incompatible with the new topology which caused all effects to be removed.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE6)  : begin
                                      HrStr := 'MF_E_CAPTURE_SOURCE_NO_INDEPENDENT_PHOTO_STREAM_PRESENT';
                                      HrDescr := 'The current capture source does not have an independent photo stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE7)  : begin
                                      HrStr := 'MF_E_CAPTURE_SOURCE_NO_VIDEO_STREAM_PRESENT';
                                      HrDescr := 'The current capture source does not have a video stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE8)  : begin
                                      HrStr := 'MF_E_CAPTURE_SOURCE_NO_AUDIO_STREAM_PRESENT';
                                      HrDescr := 'The current capture source does not have an audio stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABE9)  : begin
                                      HrStr := 'MF_E_CAPTURE_SOURCE_DEVICE_EXTENDEDPROP_OP_IN_PROGRESS';
                                      HrDescr := 'The capture source device has an asynchronous extended property operation in progress.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABEA)  : begin
                                      HrStr := 'MF_E_CAPTURE_PROPERTY_SET_DURING_PHOTO';
                                      HrDescr := 'A property cannot be set because a photo or photo sequence is in progress.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABEB)  : begin
                                      HrStr := 'MF_E_CAPTURE_NO_SAMPLES_IN_QUEUE';
                                      HrDescr := 'No more samples in queue.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABEC)  : begin
                                      HrStr := 'MF_E_HW_ACCELERATED_THUMBNAIL_NOT_SUPPORTED';
                                      HrDescr := 'Hardware accelerated thumbnail generation is not supported for the currently selected mediatype on the mediacapture stream.';
                                    end;
    _HRESULT_TYPEDEF_($C00DABED)  : begin
                                      HrStr := 'MF_E_UNSUPPORTED_CAPTURE_DEVICE_PRESENT';
                                      HrDescr := 'Capture device that is present on the system is not supported by Media Foundation.';
                                    end;
    _HRESULT_TYPEDEF_($C00DAFC8)  : begin
                                      HrStr := 'MF_E_TIMELINECONTROLLER_UNSUPPORTED_SOURCE_TYPE';
                                      HrDescr := 'Media Source type is not supported in Media Timeline Controller scenarios.';
                                    end;
    _HRESULT_TYPEDEF_($C00DAFC9)  : begin
                                      HrStr := 'MF_E_TIMELINECONTROLLER_NOT_ALLOWED';
                                      HrDescr := 'Operation is not allowed when Media Timeline Controller is attached.';
                                    end;
    _HRESULT_TYPEDEF_($C00DAFCA)  : begin
                                      HrStr := 'MF_E_TIMELINECONTROLLER_CANNOT_ATTACH';
                                      HrDescr := 'Attaching Media Timeline Controller is blocked because of the current state of the object.';
                                    end;
    _HRESULT_TYPEDEF_($C00DB3B0)  : begin
                                      HrStr := 'MF_E_MEDIA_EXTENSION_APPSERVICE_CONNECTION_FAILED';
                                      HrDescr := 'Connection to app service providing a media extension failed.';
                                    end;
    _HRESULT_TYPEDEF_($C00DB3B1)  : begin
                                      HrStr := 'MF_E_MEDIA_EXTENSION_APPSERVICE_REQUEST_FAILED';
                                      HrDescr := 'App service providing a media extension failed to process the request.';
                                    end;
    _HRESULT_TYPEDEF_($C00DB3B2)  : begin
                                      HrStr := 'MF_E_MEDIA_EXTENSION_PACKAGE_INTEGRITY_CHECK_FAILED';
                                      HrDescr := 'Package integrity check for app failed.';
                                    end;
    _HRESULT_TYPEDEF_($C00DB3B3)  : begin
                                      HrStr := 'MF_E_MEDIA_EXTENSION_PACKAGE_LICENSE_INVALID';
                                      HrDescr := 'License check for app failed.';
                                    end;

     // MF Media Engine errors - see W3C definitions for details

    _HRESULT_TYPEDEF_($80700001)  : begin
                                      HrStr := 'MF_INDEX_SIZE_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($80700008)  : begin
                                      HrStr := 'MF_NOT_FOUND_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($80700009)  : begin
                                      HrStr := 'MF_NOT_SUPPORTED_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($8070000B)  : begin
                                      HrStr := 'MF_INVALID_STATE_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($8070000C)  : begin
                                      HrStr := 'MF_SYNTAX_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($8070000F)  : begin
                                      HrStr := 'MF_INVALID_ACCESS_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($80700016)  : begin
                                      HrStr := 'MF_QUOTA_EXCEEDED_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($80700051)  : begin
                                      HrStr := 'MF_PARSE_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    _HRESULT_TYPEDEF_($80704005)  : begin
                                      HrStr := 'MF_TYPE_ERR';
                                      HrDescr := 'MF Media Engine error - See W3C definitions for details.';
                                    end;
    else
      begin
        HrStr := 'Unknown identifier.';
        HrDescr := 'Unknown HResult code.';
        HeaderFile := 'Unknown.';
        FWinHResultCracker.ClearResults();
        Result := aHResult;
        Exit;
      end;
  end;
  Result := hr;
end;


function GetMfRegion(aHResult: HResult;
                     out aRegion: string): HResult;
const
  MF = 'Media Foundation ';

var
  hr: HResult;
  li: LongInt;
  s: string;

begin
  hr := S_OK;

  // Get the regioncode
  s := IntToHex(aHResult, 8);
  s := '$' + RightStr(s, 4);
  li := StrToInt(s);

  // Find the error region.
  if (li >= 14000) and (li <= 14999) then
    aRegion := MF + 'General API.'
  else if (li >= 15000) and (li <= 15999) then
    aRegion := MF + 'ASF parsing API.'
  else if (li >= 16000) and (li <= 16999) then
    aRegion := MF + 'Media source API.'
  else if (li >= 17000) and (li <= 17999) then
    aRegion := MF + 'Network API event.'
  else if (li >= 18000) and (li <= 18999) then
    aRegion := MF + 'WMContainer API event.'
  else if (li >= 19000) and (li <= 19999) then
    aRegion := MF + 'Mediasink API event.'
  else if (li >= 20000) and (li <= 20999) then
    aRegion := MF + 'Renderer API.'
  else if (li >= 21000) and (li <= 21999) then
    aRegion := MF + 'Topology API.'
  else if (li >= 25000) and (li <= 25999) then
    aRegion := MF + 'Timeline API.'
  else if (li >= 26000) and (li <= 26999) then
    aRegion := MF + 'Unused.'
  else if (li >= 28000) and (li <= 28999) then
    aRegion := MF + 'Transform API.'
  else if (li >= 29000) and (li <= 29999) then
    aRegion := MF + 'Content protection API.'
  else if (li >= 40000) and (li <= 40999) then
    aRegion := MF + 'Clock API.'
  else if (li >= 41000) and (li <= 41999) then
    aRegion := MF + 'Quality management API.'
  else if (li >= 42000) and (li <= 42999) then
    aRegion := MF + 'Transcode API.'
  else if (li >= 43000) and (li <= 43999) then
    aRegion := MF + 'HW Device Proxy API.'
  else if (li >= 44000) and (li <= 44999) then
    aRegion := MF + 'Capture Engine API.'
  else if (li >= 45000) and (li <= 45999) then
    aRegion := MF + 'Media Timeline Controller API.'
  else if (li >= 46000) and (li <= 46999) then
    aRegion := MF + 'MediaExtensions via appService API.'
  else
    begin
      hr := ERROR_NOT_FOUND;
      aRegion := MF + 'Unknown Region.';
    end;
  Result := hr;
end;

end.

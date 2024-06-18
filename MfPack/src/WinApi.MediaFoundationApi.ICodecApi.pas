// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation MediaFoundationApi
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.ICodecApi.pas
// Kind: Pascal / Delphi unit
// Release date: 31-07-2020
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Defines icodecapi interface.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (topPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX317 / Media Foundation
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: icodecapi.h
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
unit WinApi.MediaFoundationApi.ICodecApi;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
  WinApi.Windows,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.ActiveX;
  {$ELSE}
  WinApi.ActiveX,
  WinApi.ActiveX.OaIdl;
  {$ENDIF}


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  ///  <summary>
  ///    The CodecAPIEventData structure contains event data for the EC_CODECAPI_EVENT event.
  ///    This event is sent by codecs that support the ICodecAPI interface.
  ///  </summary>
  /// <param name="guid">
  ///   A GUID that identifies the codec event.
  /// </param>
  /// <param name="dataLength">
  ///   The length of the additional data that follows this structure, in bytes.
  ///   The value can be zero.
  /// </param>
  /// <param name="reserved">
  ///   Reserved; do not use.
  /// </param>
  CodecAPIEventData = record
    guid: TGUID;
    dataLength: DWORD;
    reserved: array [0..2] of DWORD;
  end;

  /// <summary>
  ///   Interface ICodecAPI
  ///   ===================
  ///   The ICodecAPI interface sets and retrieves settings on an encoder or decoder filter,
  ///   and defines a generic mechanism for setting properties on a codec.
  ///
  ///   Applications can pass the CODECAPI_VIDEO_ENCODER to IsSupported to test for video encoders
  ///   Similarly, the GUIDs for audio encoders, video decoders, audio decoders and muxes can be
  ///   used to test for the codec classification
  ///
  ///   See uuids.h for a more detailed list.
  /// </summary>
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICodecAPI);'}
  {$EXTERNALSYM ICodecAPI}
  ICodecAPI = interface(IUnknown)
  ['{901db4c7-31ce-41a2-85dc-8fa0bf41b8da}']
    ///  <summary> Query whether a given parameter is supported.</summary>
    function IsSupported (const Api: TGUID): HResult; stdcall;

    /// <summary> Query whether a given parameter can be changed given the codec selection
    ///   and other parameter selections.
    /// </summary>
    /// <param name="Api">
    ///   Pointer to a GUID that specifies the property to query.
    /// </param>
    /// <see href="https://learn.microsoft.com/en-us/windows/desktop/DirectShow/codec-api-properties">
    /// [See Codec API Properties]
    /// </see>
    function IsModifiable(const Api: TGUID): HResult; stdcall;

    /// <summary>
    ///   Returns the valid range of values that the parameter supports should
    ///   the parameter support a stepped range as opposed to a list of specific
    ///   values. The support is [ValueMin .. ValueMax] by SteppingDelta.
    ///
    ///   Ranged variant types must fall into one of the below types.  Each
    ///   parameter will, by definition, return a specific type.
    ///
    ///   If the range has no stepping delta (any delta will do), the Stepping
    ///   delta will be empty (VT_EMPTY).
    /// </summary>
    /// <param name="Api">
    ///   Pointer to a GUID that specifies the property to query.
    /// </param>
    /// <param name="ValueMin">
    ///   Pointer to a VARIANT that receives the minimum value of the property.
    ///   The caller must free the VARIANT by calling VariantClear.
    /// </param>
    /// <param name="ValueMax">
    ///   Pointer to a VARIANT that sets the maximum value of the property.
    /// </param>
    /// <param name="SteppingDelta">
    ///   Pointer to a VARIANT that receives the stepping delta,
    ///   which defines the valid increments from ValueMin to ValueMax.
    ///   The caller must free the VARIANT by calling VariantClear.
    ///
    ///   If the VARIANT type is VT_EMPTY, any increment is valid.
    /// </param>
    function GetParameterRange(const Api: TGUID;
                               out ValueMin: VARIANT;
                               out ValueMax: VARIANT;
                               out SteppingDelta: VARIANT): HResult; stdcall;

    /// <summary>
    ///   Returns the list of values supported by the given parameter as a
    ///   COM allocated array. The total number of values will be placed in
    ///   the ValuesCount parameter and the Values array will contain the
    ///   individual values. This array must be freed by the caller through
    ///   CoTaskMemFree().
    /// </summary>
    function GetParameterValues (const Api: TGUID;
                                 {out} Values: PVARIANT;
                                 out ValuesCount: ULONG): HResult; stdcall;

    /// <summary>
    ///   Get the default value for a parameter, if one exists. Otherwise,
    ///   an error will be returned.
    /// </summary>
    function GetDefaultValue(const Api: TGUID;
                             out Value: PVARIANT): HResult; stdcall;

    /// <summary>
    ///   Get the current value of a parameter.
    /// </summary>
    function GetValue (const Api: TGUID;
                       out Value: VARIANT): HResult; stdcall;

    /// <summary>
    ///   Set the current value of a parameter.
    /// </summary>
    function SetValue(const Api: TGUID;
                      Value: VARIANT): HResult; stdcall;

    // new methods beyond IEncoderAPI

    /// <summary>
    ///   Enable events to be reported for the given event GUID.  For DShow
    ///   events, the event is returned as
    ///        (EC_CODECAPI_EVENT, lParam=userData, lParam2=CodecAPIEventData* Data)
    ///   where
    ///      - the CodecAPIEventData is COM allocated memory and must be handled and freed
    ///        by the application using CoTaskMemFree().
    ///      - the userData is the same pointer passed to RegisterForEvent
    ///
    ///   Each data block starts with the following structure:
    ///      struct CodecAPIEventData
    ///      {
    ///          GUID guid;
    ///          DWORD dataLength;
    ///          DWORD reserved[3];     // pad to 16 byte alignment
    ///          BYTE data[dataLength];
    ///      }
    ///   The guid parameter identifies the event. The data associated with the event follows the
    ///   structure (represented by the variable length BYTE data[dataLength] array).
    ///
    ///   If guid is equal to CODECAPI_CHANGELISTS, then data is an array of GUIDs that changed as
    ///   a result of setting the parameter, as follows:
    ///      GUID    changedGuids[ header.dataLength / sizeof(GUID) ]
    ///
    ///   The current array is limited, so a driver may send multiple messages if the array size is
    ///   exceeded.
    /// </summary>
    function RegisterForEvent(const Api: TGUID;
                              userData: LONG_PTR): HResult; stdcall;

    /// <summary>
    ///   Disable event reporting for the given event GUID.
    /// </summary>
    function UnregisterForEvent(const Api: TGUID): HResult; stdcall;

    /// <summary>
    ///   SetAllDefaults
    /// </summary>
    function SetAllDefaults(): HResult; stdcall;

    /// <summary>
    ///   Extended SetValue & SetAllDefaults:
    ///
    ///   Changes the current value of a parameter and returns back an alteration list
    ///
    ///   The secondary arguments return back a list of other settings
    ///   that changed as a result of the SetValue() call (for UI updates etc)
    ///   The client must free the buffer.
    /// </summary>
    function SetValueWithNotify(const Api: TGUID;
                                Value: PVARIANT;
                                out ChangedParam: PGUID;
                                out ChangedParamCount: ULONG): HResult; stdcall;

    /// <summary>
    ///   SetAllDefaultsWithNotify
    /// </summary>
    function SetAllDefaultsWithNotify({out} ChangedParam: PGUID;
                                      out ChangedParamCount: ULONG): HResult; stdcall;
    /// <summary>
    ///   Load the current settings from a stream.
    /// </summary>
    function GetAllSettings(__MIDL__ICodecAPI0000: IStream): HResult; stdcall;

    /// <summary>
    ///   Save the current settings to a stream.
    /// </summary>
    function SetAllSettings(__MIDL__ICodecAPI0001: IStream ): HResult; stdcall;

    /// <summary>
    ///   The SetAllSettingsWithNotify method reads codec properties from a stream,
    ///   sets them on the codec, and returns a list of the properties that changed.
    /// </summary>
    function SetAllSettingsWithNotify(__MIDL__ICodecAPI0002: IStream;
                                      out ChangedParam: TGUID;
                                      out ChangedParamCount: ULONG): HResult; stdcall;
  end;


  // Additional Prototypes for ALL Interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

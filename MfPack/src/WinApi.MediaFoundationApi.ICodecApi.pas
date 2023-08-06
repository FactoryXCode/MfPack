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
// Revision Version: 3.1.5
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
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX315 / Media Foundation
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

  CodecAPIEventData = record
    guid: TGUID;
    dataLength: DWORD;
    reserved: array [0..2] of DWORD;
  end;


  // Interface ICodecAPI
  // ===================
  //  Applications can pass the CODECAPI_VIDEO_ENCODER to IsSupported to test for video encoders
  //  Similarly, the GUIDs for audio encoders, video decoders, audio decoders and muxes can be
  //  used to test for the codec classification
  //
  //  See uuids.h for a more detailed list.//
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICodecAPI);'}
  {$EXTERNALSYM ICodecAPI}
  ICodecAPI = interface(IUnknown)
  ['{901db4c7-31ce-41a2-85dc-8fa0bf41b8da}']
    // Query whether a given parameter is supported.
    function IsSupported (const Api: TGUID): HResult; stdcall;

    // Query whether a given parameter can be changed given the codec selection
    // and other parameter selections.
    //
    function IsModifiable(const Api: TGUID): HResult; stdcall;

    // Returns the valid range of values that the parameter supports should
    // the parameter support a stepped range as opposed to a list of specific
    // values.  The support is [ValueMin .. ValueMax] by SteppingDelta.
    //
    // Ranged variant types must fall into one of the below types.  Each
    // parameter will, by definition, return a specific type.
    //
    // If the range has no stepping delta (any delta will do), the Stepping
    // delta will be empty (VT_EMPTY).
    //
    function GetParameterRange(const Api: TGUID;
                               out ValueMin: VARIANT;
                               out ValueMax: VARIANT;
                               out SteppingDelta: VARIANT): HResult; stdcall;

    // Returns the list of values supported by the given parameter as a
    // COM allocated array.  The total number of values will be placed in
    // the ValuesCount parameter and the Values array will contain the
    // individual values.  This array must be freed by the caller through
    // CoTaskMemFree().
    //
    function GetParameterValues (const Api: TGUID;
                                 {out} Values: PVARIANT;
                                 out ValuesCount: ULONG): HResult; stdcall;

    // Get the default value for a parameter, if one exists.  Otherwise,
    // an error will be returned.
    //
    function GetDefaultValue(const Api: TGUID;
                             out Value: PVARIANT): HResult; stdcall;

    // Get the current value of a parameter.
    //
    function GetValue (const Api: TGUID;
                       out Value: VARIANT): HResult; stdcall;

    //
    // SetValue():
    //
    // Set the current value of a parameter.
    //
    function SetValue(const Api: TGUID;
                      Value: VARIANT): HResult; stdcall;

    // new methods beyond IEncoderAPI

    //
    // RegisterForEvent():
    //
    // Enable events to be reported for the given event GUID.  For DShow
    // events, the event is returned as
    //      (EC_CODECAPI_EVENT, lParam=userData, lParam2=CodecAPIEventData* Data)
    // where
    //      - the CodecAPIEventData is COM allocated memory and must be handled and freed
    //        by the application using CoTaskMemFree().
    //      - the userData is the same pointer passed to RegisterForEvent
    //
    // Each data block starts with the following structure:
    //      struct CodecAPIEventData
    //      {
    //          GUID guid;
    //          DWORD dataLength;
    //          DWORD reserved[3];     // pad to 16 byte alignment
    //          BYTE data[dataLength];
    //      }
    // The guid parameter identifies the event. The data associated with the event follows the
    // structure (represented by the variable length BYTE data[dataLength] array).
    //
    // If guid is equal to CODECAPI_CHANGELISTS, then data is an array of GUIDs that changed as
    // a result of setting the parameter, as follows:
    //      GUID    changedGuids[ header.dataLength / sizeof(GUID) ]
    //
    // The current array is limited, so a driver may send multiple messages if the array size is
    // exceeded.
    //
    function RegisterForEvent(const Api: TGUID;
                              userData: LONG_PTR): HResult; stdcall;

    //
    // UnregisterForEvent():
    //
    // Disable event reporting for the given event GUID.
    //
    function UnregisterForEvent(const Api: TGUID): HResult; stdcall;

    //
    // SetAllDefaults
    //
    function SetAllDefaults(): HResult; stdcall;

    //
    // Extended SetValue & SetAllDefaults:
    //
    // Changes the current value of a parameter and returns back an alteration list
    //
    //  The secondary arguments return back a list of other settings
    //  that changed as a result of the SetValue() call (for UI updates etc)
    //  The client must free the buffer.
    //
    function SetValueWithNotify(const Api: TGUID;
                                Value: PVARIANT;
                                out ChangedParam: PGUID;
                                out ChangedParamCount: ULONG): HResult; stdcall;

    //
    // SetAllDefaultsWithNotify
    //
    function SetAllDefaultsWithNotify({out} ChangedParam: PGUID;
                                      out ChangedParamCount: ULONG): HResult; stdcall;
    //
    //  GetAllSettings
    //      Load the current settings from a stream
    //
    function GetAllSettings(__MIDL__ICodecAPI0000: IStream): HResult; stdcall;

    //
    //  SetAllSettings
    //      Save the current settings to a stream
    //
    function SetAllSettings(__MIDL__ICodecAPI0001: IStream ): HResult; stdcall;

    //
    //  SetAllSettingsWithNotify
    //
    function SetAllSettingsWithNotify(__MIDL__ICodecAPI0002: IStream;
                                      out ChangedParam: TGUID;
                                      out ChangedParamCount: ULONG): HResult; stdcall;
  end;


  // Additional Prototypes for ALL Interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

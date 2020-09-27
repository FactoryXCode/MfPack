// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.AudioMediaType.pas
// Kind: Pascal / Delphi unit
// Release date: 24-09-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: IAudioMediaType definition.
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
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
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
// Source: audiomediatype.h
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
unit WinApi.AudioMediaType;

  {$HPPEMIT '#include "audiomediatype.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinMM.MMReg,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  // IsEqual flags
  // =============
  // subtype match = format types match = iscompressed for both matches
  // format_data match = format blocks match exactly

  AUDIOMEDIATYPE_EQUAL_FORMAT_TYPES               = $00000002;
  {$EXTERNALSYM AUDIOMEDIATYPE_EQUAL_FORMAT_TYPES}
  AUDIOMEDIATYPE_EQUAL_FORMAT_DATA                = $00000004;
  {$EXTERNALSYM AUDIOMEDIATYPE_EQUAL_FORMAT_DATA}
  AUDIOMEDIATYPE_EQUAL_FORMAT_USER_DATA           = $00000008;
  {$EXTERNALSYM AUDIOMEDIATYPE_EQUAL_FORMAT_USER_DATA}


//
// UNCOMPRESSEDAUDIOFORMAT
//

type

  PUNCOMPRESSEDAUDIOFORMAT = ^UNCOMPRESSEDAUDIOFORMAT;
   _UNCOMPRESSEDAUDIOFORMAT = record
    guidFormatType: TGUID;
    dwSamplesPerFrame: DWORD;
    dwBytesPerSampleContainer: DWORD;
    dwValidBitsPerSample: DWORD;
    fFramesPerSecond: Single;
    dwChannelMask: DWORD;
  end;
  {$EXTERNALSYM _UNCOMPRESSEDAUDIOFORMAT}
  UNCOMPRESSEDAUDIOFORMAT  = _UNCOMPRESSEDAUDIOFORMAT;


type

  // Interface IAudioMediaType
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioMediaType);'}
  {$EXTERNALSYM IAudioMediaType}
  IAudioMediaType = interface(IUnknown)
  ['{4E997F73-B71F-4798-873B-ED7DFCF15B4D}']
    function IsCompressedFormat(out pfCompressed: BOOL): HResult; stdcall;

    function IsEqual(pIAudioType: IAudioMediaType;
                     out pdwFlags: DWORD): HResult; stdcall;

    function GetAudioFormat(): WAVEFORMATEX; stdcall;

    function GetUncompressedAudioFormat(out pUncompressedAudioFormat: UNCOMPRESSEDAUDIOFORMAT): HResult; stdcall;

  end;
  IID_IAudioMediaType = IAudioMediaType;
  {$EXTERNALSYM IID_IAudioMediaType}



  function CreateAudioMediaType(pAudioFormat: WAVEFORMATEX;
                                cbAudioFormatSize: UINT32;
                                var ppIAudioMediaType: IAudioMediaType): HResult; stdcall;
  // The CreateAudioMediaType function uses the format specified by the caller to create
  // a media type object that describes the audio format.
  // Parameters:
  // pAudioFormat         Specifies a pointer to a WAVEFORMATEX structure.
  // cbAudioFormatSize    Specifies the size of the WAVEFORMATEX structure pointed to by the pAudioFormat parameter.
  // ppIAudioMediaType    Specifies a pointer to an IAudioMediaType interface.
  // Return value:

  // The CreateAudioMediaType function returns S_OK if the call to the function is successful.
  // Otherwise, it returns an appropriate HRESULT error code.
  // Remarks:
  // When you implement custom audio system effects, the CreateAudioMediaType function works with
  // IAudioSystemEffectsCustomFormats.GetFormat to represent a custom audio data format and to
  // provide information about the custom format.
  {$EXTERNALSYM CreateAudioMediaType}



  function CreateAudioMediaTypeFromUncompressedAudioFormat(pUncompressedAudioFormat: UNCOMPRESSEDAUDIOFORMAT;
                                                           var ppIAudioMediaType: IAudioMediaType): HResult; stdcall;
  {$EXTERNALSYM CreateAudioMediaTypeFromUncompressedAudioFormat}





  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

const
  AudioMediaTypelib = 'mf.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function CreateAudioMediaType; external AudioMediaTypelib name 'CreateAudioMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CreateAudioMediaTypeFromUncompressedAudioFormat; external AudioMediaTypelib name 'CreateAudioMediaTypeFromUncompressedAudioFormat' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.

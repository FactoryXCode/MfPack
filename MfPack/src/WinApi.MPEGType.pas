 // FactoryX
  //
  // Copyright: © FactoryX. All rights reserved.
  //
  // Project: MfPack - Shared
  // Project location: https:  //sourceforge.net/projects/MFPack
  //                   https:  //github.com/FactoryXCode/MfPack
  // Module: WinApi.MPEGType.pas
  // Kind: Pascal / Delphi unit
  // Release date: 17-05-2020
  // Language: ENU
  //
  // Revision Version: 3.0.0
  // Description: MPEG system stream compound type definition.
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
  // Remarks: -
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
  // Source: MPEGType.h
  //
  // Copyright (c) Microsoft Corporation. All rights reserved.
  //==============================================================================
  //
  // LICENSE
  //
  // The contents of this file are subject to the Mozilla Public License
  // Version 2.0 (the "License"); you may not use this file except in
  // compliance with the License. You may obtain a copy of the License at
  // https:  //www.mozilla.org/en-US/MPL/2.0/
  //
  // Software distributed under the License is distributed on an "AS IS"
  // basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  // License for the specific language governing rights and limitations
  // under the License.
  //
  // Users may distribute this source code provided that this header is included
  // in full at the top of the file.
  //==============================================================================
unit WinApi.MPEGType;

interface

uses
  WinApi.Windows,
  WinApi.StrmIf,
  WinApi.WinMM.MMReg;

//
//  AM_MPEGSYSTEMTYPE defines the format block contents for
//  data of type MEDIATYPE_MPEG1System when the format
//  block GUID is FORMAT_MPEG1System
//
//  The format block consists of elements of type
//  AM_MPEGSYSTEMTYPE up to the length of the format block
//  Each format block is 8-byte aligned from the start of
//  the format block
//

type

  PAM_MPEGSTREAMTYPE = ^AM_MPEGSTREAMTYPE;
  tagAM_MPEGSTREAMTYPE = record
    dwStreamId: DWORD;               // Stream id of stream to process
    dwReserved: DWORD;               // 8-byte alignment
    mt: AM_MEDIA_TYPE;               // Type for substream - pbFormat is Nil
    bFormat: array[0..0] of Byte;    // Format data
  end;
  {$EXTERNALSYM tagAM_MPEGSTREAMTYPE}
  AM_MPEGSTREAMTYPE = tagAM_MPEGSTREAMTYPE;
  {$EXTERNALSYM AM_MPEGSTREAMTYPE}

  PAM_MPEGSYSTEMTYPE = ^AM_MPEGSYSTEMTYPE;
  tagAM_MPEGSYSTEMTYPE = record
    dwBitRate: DWORD;                // Bits per second
    cStreams: DWORD;                 // Number of streams
    Streams: array[0..0] of AM_MPEGSTREAMTYPE;
  end;
  {$EXTERNALSYM tagAM_MPEGSYSTEMTYPE}
  AM_MPEGSYSTEMTYPE = tagAM_MPEGSYSTEMTYPE;
  {$EXTERNALSYM AM_MPEGSYSTEMTYPE}

  //
  //  Helper macros for AM_MPEGSTREAMTYPE
  //

  {#define AM_MPEGSTREAMTYPE_ELEMENTLENGTH(pStreamType)  \
      FIELD_OFFSET(AM_MPEGSTREAMTYPE, bFormat[(pStreamType)->mt.cbFormat])
   #define AM_MPEGSTREAMTYPE_NEXT(pStreamType)
      ((AM_MPEGSTREAMTYPE *)((PBYTE)(pStreamType) + ((AM_MPEGSTREAMTYPE_ELEMENTLENGTH(pStreamType) + 7) & ~7)))
  }

  //
  // IMpegAudioDecoder
  //

  // Values for DualMode

const
  AM_MPEG_AUDIO_DUAL_MERGE            = 0;
  {$EXTERNALSYM AM_MPEG_AUDIO_DUAL_MERGE}
  AM_MPEG_AUDIO_DUAL_LEFT             = 1;
  {$EXTERNALSYM AM_MPEG_AUDIO_DUAL_LEFT}
  AM_MPEG_AUDIO_DUAL_RIGHT            = 2;
  {$EXTERNALSYM AM_MPEG_AUDIO_DUAL_RIGHT}

type

  {$HPPEMIT 'typedef System::DelphiInterface<IMpegAudioDecoder> _di_IMpegAudioDecoder;'}
  {$EXTERNALSYM IMpegAudioDecoder}
  IMpegAudioDecoder = interface(IUnknown)
  ['{B45DD570-3C77-11D1-ABE1-00A0C905F375}']
  (*** IMpegAudioDecoder methods ***)
    function get_FrequencyDivider(out pDivider: LongWord): HResult; stdcall;

    function put_FrequencyDivider(Divider: LongWord): HResult; stdcall;

    function get_DecoderAccuracy(out pAccuracy: LongWord): HResult; stdcall;

    function put_DecoderAccuracy(Accuracy: LongWord): HResult; stdcall;

    function get_Stereo(out pStereo: LongWord): HResult; stdcall;

    function put_Stereo(Stereo: LongWord): HResult; stdcall;

    function get_DecoderWordSize(out pWordSize: LongWord): HResult; stdcall;

    function put_DecoderWordSize(WordSize: LongWord): HResult; stdcall;

    function get_IntegerDecode(out pIntDecode: LongWord): HResult; stdcall;

    function put_IntegerDecode(IntDecode: LongWord): HResult; stdcall;

    function get_DualMode(out pIntDecode: LongWord): HResult; stdcall;

    function put_DualMode(IntDecode: LongWord): HResult; stdcall;

    function get_AudioFormat(out lpFmt: MPEG1WaveFormat): HResult; stdcall;

  end;
  IID_IMpegAudioDecoder = IMpegAudioDecoder;

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DvdMedia.pas
// Kind: Pascal / Delphi unit
// Release date: 06-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Contains typedefs and defines necessary for user mode (ring 3) DVD
//              filters and applications.
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
// Remarks: This unit is also included in DsPack.DirectShow9.pas / Winapi.DirectShow9.pas
//          If you need DirectShow, please, don't add this file to your project to prevent name-mangling.
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
// Source: dvdmedia.h
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
unit WinApi.DvdMedia;

  {$HPPEMIT '#include "dvdmedia.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


type

// -----------------------------------------------------------------------
// AC-3 definition for the AM_KSPROPSETID_AC3 property set
// -----------------------------------------------------------------------
  PAM_PROPERTY_AC3 = ^AM_PROPERTY_AC3;
  AM_PROPERTY_AC3 = (
    AM_PROPERTY_AC3_INVALID_0         = 0,
    AM_PROPERTY_AC3_ERROR_CONCEALMENT = 1,
    AM_PROPERTY_AC3_ALTERNATE_AUDIO   = 2,
    AM_PROPERTY_AC3_DOWNMIX           = 3,
    AM_PROPERTY_AC3_BIT_STREAM_MODE   = 4,
    AM_PROPERTY_AC3_DIALOGUE_LEVEL    = 5,
    AM_PROPERTY_AC3_LANGUAGE_CODE     = 6,
    AM_PROPERTY_AC3_ROOM_TYPE         = 7
  );
  {$EXTERNALSYM AM_PROPERTY_AC3}

  PAM_AC3_ERROR_CONCEALMENT = ^AM_AC3_ERROR_CONCEALMENT;
  AM_AC3_ERROR_CONCEALMENT = record
    fRepeatPreviousBlock: BOOL;
    fErrorInCurrentBlock: BOOL;
  end;
  {$EXTERNALSYM AM_AC3_ERROR_CONCEALMENT}


  PAM_AC3_ALTERNATE_AUDIO = ^AM_AC3_ALTERNATE_AUDIO;
  AM_AC3_ALTERNATE_AUDIO = record
    fStereo: BOOL;
    DualMode: ULONG;
  end;
  {$EXTERNALSYM AM_AC3_ALTERNATE_AUDIO}

const

  AM_AC3_ALTERNATE_AUDIO_1     = 1;
  {$EXTERNALSYM AM_AC3_ALTERNATE_AUDIO_1}
  AM_AC3_ALTERNATE_AUDIO_2     = 2;
  {$EXTERNALSYM AM_AC3_ALTERNATE_AUDIO_2}
  AM_AC3_ALTERNATE_AUDIO_BOTH  = 3;
  {$EXTERNALSYM AM_AC3_ALTERNATE_AUDIO_BOTH}


type

  PAM_AC3_DOWNMIX = ^AM_AC3_DOWNMIX;
  AM_AC3_DOWNMIX = record
    fDownMix       : BOOL;
    fDolbySurround : BOOL;
  end;
  {$EXTERNALSYM AM_AC3_DOWNMIX}

  PAM_AC3_BIT_STREAM_MODE = ^AM_AC3_BIT_STREAM_MODE;
  AM_AC3_BIT_STREAM_MODE = record
    BitStreamMode: Longint;
  end;
  {$EXTERNALSYM AM_AC3_BIT_STREAM_MODE}


const

  AM_AC3_SERVICE_MAIN_AUDIO        = 0;
  {$EXTERNALSYM AM_AC3_SERVICE_MAIN_AUDIO}
  AM_AC3_SERVICE_NO_DIALOG         = 1;
  {$EXTERNALSYM AM_AC3_SERVICE_NO_DIALOG}
  AM_AC3_SERVICE_VISUALLY_IMPAIRED = 2;
  {$EXTERNALSYM AM_AC3_SERVICE_VISUALLY_IMPAIRED}
  AM_AC3_SERVICE_HEARING_IMPAIRED  = 3;
  {$EXTERNALSYM AM_AC3_SERVICE_HEARING_IMPAIRED}
  AM_AC3_SERVICE_DIALOG_ONLY       = 4;
  {$EXTERNALSYM AM_AC3_SERVICE_DIALOG_ONLY}
  AM_AC3_SERVICE_COMMENTARY        = 5;
  {$EXTERNALSYM AM_AC3_SERVICE_COMMENTARY}
  AM_AC3_SERVICE_EMERGENCY_FLASH   = 6;
  {$EXTERNALSYM AM_AC3_SERVICE_EMERGENCY_FLASH}
  AM_AC3_SERVICE_VOICE_OVER        = 7;
  {$EXTERNALSYM AM_AC3_SERVICE_VOICE_OVER}

type

  PAM_AC3_DIALOGUE_LEVEL = ^AM_AC3_DIALOGUE_LEVEL;
  AM_AC3_DIALOGUE_LEVEL = record
    DialogueLevel: ULONG;
  end;
  {$EXTERNALSYM AM_AC3_DIALOGUE_LEVEL}

  PAM_AC3_ROOM_TYPE = ^AM_AC3_ROOM_TYPE;
  AM_AC3_ROOM_TYPE = record
    fLargeRoom: BOOL;
  end;
  {$EXTERNALSYM AM_AC3_ROOM_TYPE}


// -----------------------------------------------------------------------
// subpicture definition for the AM_KSPROPSETID_DvdSubPic property set
// -----------------------------------------------------------------------
  PAM_PROPERTY_DVDSUBPIC = ^AM_PROPERTY_DVDSUBPIC;
  AM_PROPERTY_DVDSUBPIC = (
    AM_PROPERTY_DVDSUBPIC_PALETTE     = 0,
    AM_PROPERTY_DVDSUBPIC_HLI         = 1,
    AM_PROPERTY_DVDSUBPIC_COMPOSIT_ON = 2 // TRUE for subpicture is displayed
  );
  {$EXTERNALSYM AM_PROPERTY_DVDSUBPIC}


  PAM_DVD_YUV = ^AM_DVD_YUV;
  _AM_DVD_YUV = record
    Reserved: Byte;
    Y: byte;
    U: Byte;
    V: Byte;
  end;
  {$EXTERNALSYM _AM_DVD_YUV}
  AM_DVD_YUV = _AM_DVD_YUV;
  {$EXTERNALSYM AM_DVD_YUV}


  PAM_PROPERTY_SPPAL = ^AM_PROPERTY_SPPAL;
  _AM_PROPERTY_SPPAL = record
    sppal: array[0..15] of AM_DVD_YUV;
  end;
  {$EXTERNALSYM _AM_PROPERTY_SPPAL}
  AM_PROPERTY_SPPAL = _AM_PROPERTY_SPPAL;
  {$EXTERNALSYM AM_PROPERTY_SPPAL}

  PAM_COLCON = ^AM_COLCON;
  _AM_COLCON = record
    case Integer of
      0: (
        emph1col : Byte;
        backcol  : Byte;
        emph1con : Byte;
        backcon  : Byte;
      );
      1: (
        emph2col : Byte;
        patcol   : Byte;
        emph2con : Byte;
        patcon   : Byte;
      );
  end;
  {$EXTERNALSYM _AM_COLCON}
  AM_COLCON = _AM_COLCON;
  {$EXTERNALSYM AM_COLCON}


  PAM_PROPERTY_COMPOSIT_ON = ^AM_PROPERTY_COMPOSIT_ON;
  _AM_PROPERTY_SPHLI = record
    HLISS    : Word;      //
    Reserved : Word;
    StartPTM : ULONG;     // start presentation time in x/90000
    EndPTM   : ULONG;     // end PTM in x/90000
    StartX   : Word;
    StartY   : Word;
    StopX    : Word;
    StopY    : Word;
    ColCon   : AM_COLCON; // color contrast description (4 bytes as given in HLI)
  end;
  {$EXTERNALSYM _AM_PROPERTY_SPHLI}
  AM_PROPERTY_SPHLI = _AM_PROPERTY_SPHLI;
  {$EXTERNALSYM AM_PROPERTY_SPHLI}
  AM_PROPERTY_COMPOSIT_ON = {$IFDEF TYPE_IDENTITY} type {$ENDIF} BOOL;
  {$EXTERNALSYM AM_PROPERTY_COMPOSIT_ON}


// -----------------------------------------------------------------------
// copy protection definitions
// -----------------------------------------------------------------------


const

  AM_UseNewCSSKey                     = $1;
  {$EXTERNALSYM AM_UseNewCSSKey}
  // AM_UseNewCSSKey for the dwTypeSpecificFlags in IMediaSample2 to indicate
  // the exact point in a stream after which to start applying a new CSS key.
  // This is typically sent on an empty media sample just before attempting
  // to renegotiate a CSS key.
  AM_ReverseBlockStart                = $2;
  {$EXTERNALSYM AM_ReverseBlockStart}
  AM_ReverseBlockEnd                  = $4;
  {$EXTERNALSYM AM_ReverseBlockEnd}


type

  //
  // AM_KSPROPSETID_CopyProt property set definitions
  //
  PAmPropertyDvdcopyprot = ^AM_PROPERTY_DVDCOPYPROT;
  AM_PROPERTY_DVDCOPYPROT                     = (
    AM_PROPERTY_DVDCOPY_CHLG_KEY              = $01,
    AM_PROPERTY_DVDCOPY_DVD_KEY1              = $02,
    AM_PROPERTY_DVDCOPY_DEC_KEY2              = $03,
    AM_PROPERTY_DVDCOPY_TITLE_KEY             = $04,
    AM_PROPERTY_COPY_MACROVISION              = $05,
    AM_PROPERTY_DVDCOPY_REGION                = $06,
    AM_PROPERTY_DVDCOPY_SET_COPY_STATE        = $07,
    AM_PROPERTY_COPY_ANALOG_COMPONENT         = $08,  // GetOnly property, return data is a BOOL (Delphi LongBool)
    AM_PROPERTY_COPY_DIGITAL_CP               = $09,
    AM_PROPERTY_COPY_DVD_SRM                  = $0A,
    AM_PROPERTY_DVDCOPY_SUPPORTS_NEW_KEYCOUNT = $0B,  // read only, BOOL (Delphi LongBool)
                                                      // gap
    AM_PROPERTY_DVDCOPY_DISC_KEY              = $80
  );
  {$EXTERNALSYM AM_PROPERTY_DVDCOPYPROT}



type

  PAM_DIGITAL_CP = ^AM_DIGITAL_CP;
  _AM_DIGITAL_CP                = (
    AM_DIGITAL_CP_OFF           = 0,
    AM_DIGITAL_CP_ON            = 1,
    AM_DIGITAL_CP_DVD_COMPLIANT = 2
  );
  {$EXTERNALSYM _AM_DIGITAL_CP}
  AM_DIGITAL_CP = _AM_DIGITAL_CP;
  {$EXTERNALSYM AM_DIGITAL_CP}


  PAM_DVDCOPY_CHLGKEY = ^AM_DVDCOPY_CHLGKEY;
  _AM_DVDCOPY_CHLGKEY = record
    ChlgKey: array[0..9] of Byte;
    Reserved: array[0..1] of Byte;
  end;
  {$EXTERNALSYM _AM_DVDCOPY_CHLGKEY}
  AM_DVDCOPY_CHLGKEY = _AM_DVDCOPY_CHLGKEY;
  {$EXTERNALSYM AM_DVDCOPY_CHLGKEY}


  PAM_DVDCOPY_BUSKEY = ^AM_DVDCOPY_BUSKEY;
  _AM_DVDCOPY_BUSKEY = record
    BusKey: array[0..4] of Byte;
    Reserved: array [0..1] of Byte;
  end;
  {$EXTERNALSYM _AM_DVDCOPY_BUSKEY}
  AM_DVDCOPY_BUSKEY = _AM_DVDCOPY_BUSKEY;
  {$EXTERNALSYM AM_DVDCOPY_BUSKEY}


  PAM_DVDCOPY_DISCKEY = ^AM_DVDCOPY_DISCKEY;
  _AM_DVDCOPY_DISCKEY = record
    DiscKey: array[0..2047] of Byte;
  end;
  {$EXTERNALSYM _AM_DVDCOPY_DISCKEY}
  AM_DVDCOPY_DISCKEY = _AM_DVDCOPY_DISCKEY;
  {$EXTERNALSYM AM_DVDCOPY_DISCKEY}


  PAM_DVDCOPY_TITLEKEY = ^AM_DVDCOPY_TITLEKEY;
  AM_DVDCOPY_TITLEKEY = record
    KeyFlags: ULONG;
    Reserved1: array[0..1] of ULONG;
    TitleKey: array[0..5] of Byte;
    Reserved2: array[0..1] of Byte;
  end;
  {$EXTERNALSYM AM_DVDCOPY_TITLEKEY}


  PAM_COPY_MACROVISION = ^AM_COPY_MACROVISION;
  _AM_COPY_MACROVISION = record
    MACROVISIONLevel: ULONG;
  end;
  {$EXTERNALSYM _AM_COPY_MACROVISION}
  AM_COPY_MACROVISION = _AM_COPY_MACROVISION;
  {$EXTERNALSYM AM_COPY_MACROVISION}

  PAM_DVDCOPY_SET_COPY_STATE = ^AM_DVDCOPY_SET_COPY_STATE;
  AM_DVDCOPY_SET_COPY_STATE = record
    DVDCopyState: ULONG;
  end;
  {$EXTERNALSYM AM_DVDCOPY_SET_COPY_STATE}


  PAM_DVDCOPYSTATE = ^AM_DVDCOPYSTATE;
  AM_DVDCOPYSTATE                               = (
    AM_DVDCOPYSTATE_INITIALIZE                  = 0,
    AM_DVDCOPYSTATE_INITIALIZE_TITLE            = 1,  // indicates we are starting a title
    AM_DVDCOPYSTATE_AUTHENTICATION_NOT_REQUIRED = 2,  // key copy protection sequence
    AM_DVDCOPYSTATE_AUTHENTICATION_REQUIRED     = 3,
    AM_DVDCOPYSTATE_DONE                        = 4
  );
  {$EXTERNALSYM AM_DVDCOPYSTATE}


  PAM_COPY_MACROVISION_LEVEL = ^AM_COPY_MACROVISION_LEVEL;
  AM_COPY_MACROVISION_LEVEL = (
    AM_MACROVISION_DISABLED = 0,
    AM_MACROVISION_LEVEL1   = 1,
    AM_MACROVISION_LEVEL2   = 2,
    AM_MACROVISION_LEVEL3   = 3
  );
  {$EXTERNALSYM AM_COPY_MACROVISION_LEVEL}


  // CSS region stucture
  PDVD_REGION = ^_DVD_REGION;
  _DVD_REGION = record
    CopySystem: UCHAR;
    RegionData: UCHAR;
    SystemRegion: UCHAR;
    ResetCount: UCHAR;
  end;
  {$EXTERNALSYM _DVD_REGION}
  DVD_REGION = _DVD_REGION;
  {$EXTERNALSYM DVD_REGION}



  //
  // CGMS Copy Protection Flags
  //

const

  AM_DVD_CGMS_RESERVED_MASK      = $00000078;
  {$EXTERNALSYM AM_DVD_CGMS_RESERVED_MASK}

  AM_DVD_CGMS_COPY_PROTECT_MASK  = $00000018;
  {$EXTERNALSYM AM_DVD_CGMS_COPY_PROTECT_MASK}
  AM_DVD_CGMS_COPY_PERMITTED     = $00000000;
  {$EXTERNALSYM AM_DVD_CGMS_COPY_PERMITTED}
  AM_DVD_CGMS_COPY_ONCE          = $00000010;
  {$EXTERNALSYM AM_DVD_CGMS_COPY_ONCE}
  AM_DVD_CGMS_NO_COPY            = $00000018;
  {$EXTERNALSYM AM_DVD_CGMS_NO_COPY}

  AM_DVD_COPYRIGHT_MASK          = $00000040;
  {$EXTERNALSYM AM_DVD_COPYRIGHT_MASK}
  AM_DVD_NOT_COPYRIGHTED         = $00000000;
  {$EXTERNALSYM AM_DVD_NOT_COPYRIGHTED}
  AM_DVD_COPYRIGHTED             = $00000040;
  {$EXTERNALSYM AM_DVD_COPYRIGHTED}

  AM_DVD_SECTOR_PROTECT_MASK     = $00000020;
  {$EXTERNALSYM AM_DVD_SECTOR_PROTECT_MASK}
  AM_DVD_SECTOR_NOT_PROTECTED    = $00000000;
  {$EXTERNALSYM AM_DVD_SECTOR_NOT_PROTECTED}
  AM_DVD_SECTOR_PROTECTED        = $00000020;
  {$EXTERNALSYM AM_DVD_SECTOR_PROTECTED}



// -----------------------------------------------------------------------
// video format blocks
// -----------------------------------------------------------------------

type

  PAM_MPEG2Level = ^AM_MPEG2Level;
  AM_MPEG2Level = (
    AM_MPEG2Level_Low      = 1,
    AM_MPEG2Level_Main     = 2,
    AM_MPEG2Level_High1440 = 3,
    AM_MPEG2Level_High     = 4
  );
  {$EXTERNALSYM AM_MPEG2Level}


  PAM_MPEG2Profile = ^AM_MPEG2Profile;
  AM_MPEG2Profile = (
    AM_MPEG2Profile_Simple            = 1,
    AM_MPEG2Profile_Main              = 2,
    AM_MPEG2Profile_SNRScalable       = 3,
    AM_MPEG2Profile_SpatiallyScalable = 4,
    AM_MPEG2Profile_High              = 5
  );
  {$EXTERNALSYM AM_MPEG2Profile}


const

  AMINTERLACE_IsInterlaced             = $00000001;  // if 0, other interlace bits are irrelevent
  {$EXTERNALSYM AMINTERLACE_IsInterlaced}
  AMINTERLACE_1FieldPerSample          = $00000002;  // else 2 fields per media sample
  {$EXTERNALSYM AMINTERLACE_1FieldPerSample}
  AMINTERLACE_Field1First              = $00000004;  // else Field 2 is first;  top field in PAL is field 1, top field in NTSC is field 2?
  {$EXTERNALSYM AMINTERLACE_Field1First}
  AMINTERLACE_UNUSED                   = $00000008;  //
  {$EXTERNALSYM AMINTERLACE_UNUSED}
  AMINTERLACE_FieldPatternMask         = $00000030;  // use this mask with AMINTERLACE_FieldPat*
  {$EXTERNALSYM AMINTERLACE_FieldPatternMask}
  AMINTERLACE_FieldPatField1Only       = $00000000;  // stream never contains a Field2
  {$EXTERNALSYM AMINTERLACE_FieldPatField1Only}
  AMINTERLACE_FieldPatField2Only       = $00000010;  // stream never contains a Field1
  {$EXTERNALSYM AMINTERLACE_FieldPatField2Only}
  AMINTERLACE_FieldPatBothRegular      = $00000020;  // There will be a Field2 for every Field1 (required for Weave?)
  {$EXTERNALSYM AMINTERLACE_FieldPatBothRegular}
  AMINTERLACE_FieldPatBothIrregular    = $00000030;  // Random pattern of Field1s and Field2s
  {$EXTERNALSYM AMINTERLACE_FieldPatBothIrregular}
  AMINTERLACE_DisplayModeMask          = $000000c0;
  {$EXTERNALSYM AMINTERLACE_DisplayModeMask}
  AMINTERLACE_DisplayModeBobOnly       = $00000000;
  {$EXTERNALSYM AMINTERLACE_DisplayModeBobOnly}
  AMINTERLACE_DisplayModeWeaveOnly     = $00000040;
  {$EXTERNALSYM AMINTERLACE_DisplayModeWeaveOnly}
  AMINTERLACE_DisplayModeBobOrWeave    = $00000080;
  {$EXTERNALSYM AMINTERLACE_DisplayModeBobOrWeave}

  AMCOPYPROTECT_RestrictDuplication    = $00000001;  // duplication of this stream should be restricted
  {$EXTERNALSYM AMCOPYPROTECT_RestrictDuplication}

  AMMPEG2_DoPanScan                    = $00000001;  // If set, the MPEG-2 video decoder should crop output image
  {$EXTERNALSYM AMMPEG2_DoPanScan}
                                                     // based on pan-scan vectors in picture_display_extension
                                                     // and change the picture aspect ratio accordingly.
  AMMPEG2_DVDLine21Field1              = $00000002;  // If set, the MPEG-2 decoder must be able to produce an output
  {$EXTERNALSYM AMMPEG2_DVDLine21Field1}
                                                     // pin for DVD style closed caption data found in GOP layer of field 1
  AMMPEG2_DVDLine21Field2              = $00000004;  // If set, the MPEG-2 decoder must be able to produce an output
  {$EXTERNALSYM AMMPEG2_DVDLine21Field2}
                                                     // pin for DVD style closed caption data found in GOP layer of field 2
  AMMPEG2_SourceIsLetterboxed          = $00000008;  // If set, indicates that black bars have been encoded in the top
  {$EXTERNALSYM AMMPEG2_SourceIsLetterboxed}
                                                     // and bottom of the video.
  AMMPEG2_FilmCameraMode               = $00000010;  // If set, indicates "film mode" used for 625/50 content.  If cleared,
  {$EXTERNALSYM AMMPEG2_FilmCameraMode}
                                                     // indicates that "camera mode" was used.
  AMMPEG2_LetterboxAnalogOut           = $00000020;  // If set and this stream is sent to an analog output, it should
  {$EXTERNALSYM AMMPEG2_LetterboxAnalogOut}
                                                     // be letterboxed.  Streams sent to VGA should be letterboxed only by renderers.
  AMMPEG2_DSS_UserData                 = $00000040;  // If set, the MPEG-2 decoder must process DSS style user data
  {$EXTERNALSYM AMMPEG2_DSS_UserData}
  AMMPEG2_DVB_UserData                 = $00000080;  // If set, the MPEG-2 decoder must process DVB style user data
  {$EXTERNALSYM AMMPEG2_DVB_UserData}
  AMMPEG2_27MhzTimebase                = $00000100;  // If set, the PTS,DTS timestamps advance at 27MHz rather than 90KHz
  {$EXTERNALSYM AMMPEG2_27MhzTimebase}

  AMMPEG2_WidescreenAnalogOut          = $00000200;  //if set and this stream is sent to an analog output, it should
  {$EXTERNALSYM AMMPEG2_WidescreenAnalogOut}
                                                     // Be in widescreen format (4x3 content should be centered on a 16x9 output).
                                                     // Streams sent to VGA should be widescreened only by renderers.

  // PRESENT in dwReserved1 field in VIDEOINFOHEADER2
  AMCONTROL_USED                       = $00000001;  // Used to test if these flags are supported.  Set and test for AcceptMediaType.
  {$EXTERNALSYM AMCONTROL_USED}
                                                     // If rejected, then you cannot use the AMCONTROL flags (send 0 for dwReserved1)
  AMCONTROL_PAD_TO_4x3                 = $00000002;  // If set means display the image in a 4x3 area
  {$EXTERNALSYM AMCONTROL_PAD_TO_4x3}
  AMCONTROL_PAD_TO_16x9                = $00000004;  // If set means display the image in a 16x9 area
  {$EXTERNALSYM AMCONTROL_PAD_TO_16x9}


type

  // union part
  PVideoInfoHeader2ControlFlags = ^TVideoInfoHeader2ControlFlags;
  TVideoInfoHeader2ControlFlags = record
     case byte of
      0: (dwControlFlags : DWORD);   // use AMCONTROL_* defines, use this from now on
      1: (dwReserved1    : DWORD);   // for backward compatiblity (was "must be 0";  connection rejected otherwise)
  end;
  {$EXTERNALSYM TVideoInfoHeader2ControlFlags}


  PVIDEOINFOHEADER2 = ^tagVIDEOINFOHEADER2;
  tagVIDEOINFOHEADER2 = record
    rcSource: TRect;
    rcTarget: TRect;
    dwBitRate: DWORD;
    dwBitErrorRate: DWORD;
    AvgTimePerFrame: LONGLONG;
    dwInterlaceFlags: DWORD;         // use AMINTERLACE_* defines. Reject connection if undefined bits are not 0
    dwCopyProtectFlags: DWORD;       // use AMCOPYPROTECT_* defines. Reject connection if undefined bits are not 0
    dwPictAspectRatioX: DWORD;       // X dimension of picture aspect ratio, e.g. 16 for 16x9 display
    dwPictAspectRatioY: DWORD;       // Y dimension of picture aspect ratio, e.g.  9 for 16x9 display
    ControlFlags: TVideoInfoHeader2ControlFlags;
    dwReserved2: DWORD;              // must be 0; reject connection otherwise
    bmiHeader: TBitmapInfoHeader;
  end;
  {$EXTERNALSYM tagVIDEOINFOHEADER2}
  VIDEOINFOHEADER2 = tagVIDEOINFOHEADER2;
  {$EXTERNALSYM VIDEOINFOHEADER2}


 PMPEG2VIDEOINFO = ^tagMPEG2VIDEOINFO;
 tagMPEG2VIDEOINFO = record
     hdr: VIDEOINFOHEADER2;
     dwStartTimeCode: DWORD;                 //  ?? not used for DVD ??
     cbSequenceHeader: DWORD;                // is 0 for DVD (no sequence header)
     dwProfile: DWORD;                       // use enum MPEG2Profile
     dwLevel: DWORD;                         // use enum MPEG2Level
     dwFlags: DWORD;                         // use AMMPEG2_* defines.  Reject connection if undefined bits are not 0
     dwSequenceHeader: array [0..254] of DWORD; // DWORD instead of Byte for alignment purposes
                                                // For MPEG-2, if a sequence_header is included, the sequence_extension
                                                // should also be included.
  end;
 {$EXTERNALSYM tagMPEG2VIDEOINFO}
  MPEG2VIDEOINFO = tagMPEG2VIDEOINFO;
  {$EXTERNALSYM MPEG2VIDEOINFO}



{
#define SIZE_MPEG2VIDEOINFO(pv) (FIELD_OFFSET(MPEG2VIDEOINFO, dwSequenceHeader[0]) + (pv)->cbSequenceHeader)

// do not use >>>>>> So, why publish this in the first place?????
#define MPEG1_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->bSequenceHeader)

// use this macro instead, the previous only works for MPEG1VIDEOINFO structures
#define MPEG2_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->dwSequenceHeader)
 }


//===================================================================================
// flags for dwTypeSpecificFlags in AM_SAMPLE2_PROPERTIES which define type specific
// data in IMediaSample2
//===================================================================================

const

  AM_VIDEO_FLAG_FIELD_MASK          = $0003;    // Use this mask to check whether the sample is field1 or field2 or frame
  {$EXTERNALSYM AM_VIDEO_FLAG_FIELD_MASK}
  AM_VIDEO_FLAG_INTERLEAVED_FRAME   = $0000;    // The sample is a frame (remember to use AM_VIDEO_FLAG_FIELD_MASK when using this)
  {$EXTERNALSYM AM_VIDEO_FLAG_INTERLEAVED_FRAME}
  AM_VIDEO_FLAG_FIELD1              = $0001;    // The sample is field1 (remember to use AM_VIDEO_FLAG_FIELD_MASK when using this)
  {$EXTERNALSYM AM_VIDEO_FLAG_FIELD1}
  AM_VIDEO_FLAG_FIELD2              = $0002;    // The sample is the field2 (remember to use AM_VIDEO_FLAG_FIELD_MASK when using this)
  {$EXTERNALSYM AM_VIDEO_FLAG_FIELD2}
  AM_VIDEO_FLAG_FIELD1FIRST         = $0004;    // If set means display field1 first, else display field2 first.
  {$EXTERNALSYM AM_VIDEO_FLAG_FIELD1FIRST}

  AM_VIDEO_FLAG_WEAVE               = $0008;    // If set use bob display mode else weave
  {$EXTERNALSYM AM_VIDEO_FLAG_WEAVE}
  AM_VIDEO_FLAG_IPB_MASK            = $0030;    // Use this mask to check whether the sample is I, P or B
  {$EXTERNALSYM AM_VIDEO_FLAG_IPB_MASK}
  AM_VIDEO_FLAG_I_SAMPLE            = $0000;    // I Sample (remember to use AM_VIDEO_FLAG_IPB_MASK when using this)
  {$EXTERNALSYM AM_VIDEO_FLAG_I_SAMPLE}
  AM_VIDEO_FLAG_P_SAMPLE            = $0010;    // P Sample (remember to use AM_VIDEO_FLAG_IPB_MASK when using this)
  {$EXTERNALSYM AM_VIDEO_FLAG_P_SAMPLE}
  AM_VIDEO_FLAG_B_SAMPLE            = $0020;    // B Sample (remember to use AM_VIDEO_FLAG_IPB_MASK when using this)
  {$EXTERNALSYM AM_VIDEO_FLAG_B_SAMPLE}
  AM_VIDEO_FLAG_REPEAT_FIELD        = $0040;    // If set means display the field which has been displayed first again after displaying
  {$EXTERNALSYM AM_VIDEO_FLAG_REPEAT_FIELD}
                                                // both fields first. This bit is irrelavant for 1FieldPerSample mode.




// -----------------------------------------------------------------------
// AM_KSPROPSETID_DvdKaraoke property set definitions
// -----------------------------------------------------------------------
type

  PAM_DvdKaraokeData = ^AM_DvdKaraokeData;
  tagAM_DvdKaraokeData = record
    dwDownmix           : DWORD;    // bitwise OR of AM_DvdKaraoke_Downmix flags
    dwSpeakerAssignment : DWORD;    // AM_DvdKaraoke_SpeakerAssignment
  end;
  {$EXTERNALSYM tagAM_DvdKaraokeData}
  AM_DvdKaraokeData = tagAM_DvdKaraokeData;
  {$EXTERNALSYM AM_DvdKaraokeData}

  PAM_PROPERTY_DVDKARAOKE = ^AM_PROPERTY_DVDKARAOKE;
  AM_PROPERTY_DVDKARAOKE = (
    AM_PROPERTY_DVDKARAOKE_ENABLE = 0,  // BOOL (Delphi LongBool)
    AM_PROPERTY_DVDKARAOKE_DATA   = 1
  );
  {$EXTERNALSYM AM_PROPERTY_DVDKARAOKE}


// -----------------------------------------------------------------------
// AM_KSPROPSETID_TSRateChange property set definitions for time stamp
// rate changes.
// -----------------------------------------------------------------------
type

  PAM_PROPERTY_TS_RATE_CHANGE = ^AM_PROPERTY_TS_RATE_CHANGE;
  AM_PROPERTY_TS_RATE_CHANGE       = (
    AM_RATE_SimpleRateChange       = 1,           // rw, use AM_SimpleRateChange
    AM_RATE_ExactRateChange        = 2,           // rw, use AM_ExactRateChange
    AM_RATE_MaxFullDataRate        = 3,           // r,  use AM_MaxFullDataRate
    AM_RATE_Step                   = 4,           // w,  use AM_Step
    AM_RATE_UseRateVersion         = 5,           // w, use WORD
    AM_RATE_QueryFullFrameRate     = 6,           // r, use AM_QueryRate
    AM_RATE_QueryLastRateSegPTS    = 7,           // r, use REFERENCE_TIME
    AM_RATE_CorrectTS              = 8,           // w,  use LONG
    AM_RATE_ReverseMaxFullDataRate = 9,           // r,  use AM_MaxFullDataRate
    AM_RATE_ResetOnTimeDisc        = 10,          // rw, use DWORD - indicates supports new 'timeline reset on time discontinuity' sample
    AM_RATE_QueryMapping           = 11
  );
  {$EXTERNALSYM AM_PROPERTY_TS_RATE_CHANGE}


  // -------------------------------------------------------------------
  // AM_KSPROPSETID_DVD_RateChange property set definitions for new DVD
  // rate change scheme.
  // -------------------------------------------------------------------


  PAM_PROPERTY_DVD_RATE_CHANGE = ^AM_PROPERTY_DVD_RATE_CHANGE;
  AM_PROPERTY_DVD_RATE_CHANGE = (
    AM_RATE_ChangeRate      = 1,         // w,  use AM_DVD_ChangeRate
    AM_RATE_FullDataRateMax = 2,         // r,  use AM_MaxFullDataRate
    AM_RATE_ReverseDecode   = 3,         // r,  use LONG
    AM_RATE_DecoderPosition = 4,         // r,  use AM_DVD_DecoderPosition
    AM_RATE_DecoderVersion  = 5          // r,  use LONG
  );
  {$EXTERNALSYM AM_PROPERTY_DVD_RATE_CHANGE}

  // this is the simplest mechanism to set a time stamp rate change on
  // a filter (simplest for the person setting the rate change, harder
  // for the filter doing the rate change).
  PAM_SimpleRateChange = ^AM_SimpleRateChange;
  AM_SimpleRateChange = record
    StartTime: LONGLONG;    //stream time at which to start this rate
    Rate: Longint;          //new rate * 10000 (decimal)
  end;
  {$EXTERNALSYM AM_SimpleRateChange}


  PAM_QueryRate = ^AM_QueryRate;
  AM_QueryRate = record
    lMaxForwardFullFrame: LONG; // rate * 10000
    lMaxReverseFullFrame: LONG; // rate * 10000
  end;
  {$EXTERNALSYM AM_QueryRate}


  PAM_ExactRateChange = ^AM_ExactRateChange;
  AM_ExactRateChange = record
    OutputZeroTime: LONGLONG;   // input TS that maps to zero output TS
    Rate: LongInt;              // new rate * 10000 (decimal)
  end;
  {$EXTERNALSYM AM_ExactRateChange}


  AM_MaxFullDateRate = LONG;    // rate * 10000 (decimal)
  {$EXTERNALSYM AM_MaxFullDateRate}
  AM_Step = DWORD;              // number of frame to step
  {$EXTERNALSYM AM_Step}


  // New rate change property set, structs. enums etc.
  PAM_DVD_ChangeRate = ^AM_DVD_ChangeRate;
  AM_DVD_ChangeRate = record
     StartInTime  : LONGLONG;  // stream time (input) at which to start decoding at this rate
     StartOutTime : LONGLONG;  // reference time (output) at which to start showing at this rate
     Rate         : LongInt;   // new rate * 10000 (decimal)
  end;
  {$EXTERNALSYM AM_DVD_ChangeRate}

  AM_DVD_DecoderPosition = Int64 ;
  {$EXTERNALSYM AM_DVD_DecoderPosition}

  PDVD_PLAY_DIRECTION = ^DVD_PLAY_DIRECTION;
  DVD_PLAY_DIRECTION = (
    DVD_DIR_FORWARD  = 0,
    DVD_DIR_BACKWARD = 1
  );
  {$EXTERNALSYM DVD_PLAY_DIRECTION}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

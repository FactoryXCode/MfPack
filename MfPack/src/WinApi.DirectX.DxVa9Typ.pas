// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXVA9Typ.pas
// Kind: Pascal / Delphi unit
// Release date: 28-07-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Direct3D include file.
//              https://docs.microsoft.com/en-us/windows/win32/medfound/directx-video-acceleration-2-0
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
// Source: dxva9typ.h
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
unit WinApi.DirectX.DXVA9Typ;

  {$HPPEMIT '#include "dxva9typ.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.D3D9Types;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


{$IFNDEF DIRECT3D_VERSION}
const
  DIRECT3D_VERSION = $0900;
{$ENDIF}  //DIRECT3D_VERSION


  function DXVABit(__x: DWord): DWord; inline;
  {$EXTERNALSYM DXVABit}


  // -------------------------------------------------------------------------
  //
  // The definitions that follow describe the DirectX Video Acceleration
  // decoding interface.
  // This interface is accessable via the IAMVideoAccelerator interface.
  //
  // -------------------------------------------------------------------------
  //

{$IFNDEF __DIRECTX_VA_DECODER__}
{$DEFINE __DIRECTX_VA_DECODER__}

  // AYUV sample for 16-entry YUV palette or graphic surface

type
  PDXVA_AYUVsample2 = ^DXVA_AYUVsample2;
  LPDXVA_AYUVsample2 = ^_DXVA_AYUVsample2;
  {$EXTERNALSYM LPDXVA_AYUVsample2}
  _DXVA_AYUVsample2 = record
    bCrValue: Byte;
    bCbValue: Byte;
    bY_Value: Byte;
    bSampleAlpha8: Byte;
  end;
  {$EXTERNALSYM _DXVA_AYUVsample2}
  DXVA_AYUVsample2 = _DXVA_AYUVsample2;
  {$EXTERNALSYM DXVA_AYUVsample2}

const
  DXVAp_ModeMPEG2_A  : TGUID = '{1b81be0A-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVAp_ModeMPEG2_A}
  DXVAp_ModeMPEG2_C  : TGUID = '{1b81be0C-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVAp_ModeMPEG2_C}
  DXVAp_NoEncrypt    : TGUID = '{1b81beD0-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVAp_NoEncrypt}

// #pragma pack(push, BeforeDXVApacking, 1)
{$Z1}
{$A1}

type

  LPDXVA_BufferDescription = ^_DXVA_BufferDescription;
  {$EXTERNALSYM LPDXVA_BufferDescription}
  PDXVA_BufferDescription = ^DXVA_BufferDescription;
  _DXVA_BufferDescription = record
    dwTypeIndex: DWORD;
    dwBufferIndex: DWORD;
    dwDataOffset: DWORD;
    dwDataSize: DWORD;
    dwFirstMBaddress: DWORD;
    dwNumMBsInBuffer: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwStride: DWORD;
    dwReservedBits: DWORD;
  end;
  {$EXTERNALSYM _DXVA_BufferDescription}
  DXVA_BufferDescription = _DXVA_BufferDescription;
  {$EXTERNALSYM DXVA_BufferDescription}


  LPDXVA_ConfigQueryOrReplyFunc = ^DWORD;
  {$EXTERNALSYM LPDXVA_ConfigQueryOrReplyFunc}
  PDXVA_ConfigQueryOrReplyFunc = ^DWORD;
  DXVA_ConfigQueryOrReplyFunc = DWORD;
  {$EXTERNALSYM DXVA_ConfigQueryOrReplyFunc}


const

  DXVA_QUERYORREPLYFUNCFLAG_DECODER_PROBE_QUERY= $FFFFF1;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_DECODER_PROBE_QUERY}
  DXVA_QUERYORREPLYFUNCFLAG_DECODER_LOCK_QUERY= $FFFFF5;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_DECODER_LOCK_QUERY}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_COPY= $FFFFF8;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_COPY}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_PLUS= $FFFFF9;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_PLUS}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_OK_COPY= $FFFFFC;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_OK_COPY}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_FALSE_PLUS= $FFFFFB;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_FALSE_PLUS}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_FALSE_PLUS= $FFFFFF;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_FALSE_PLUS}

  DXVA_PICTURE_DECODE_BUFFER          = 1;
  {$EXTERNALSYM DXVA_PICTURE_DECODE_BUFFER}
  DXVA_MACROBLOCK_CONTROL_BUFFER      = 2;
  {$EXTERNALSYM DXVA_MACROBLOCK_CONTROL_BUFFER}
  DXVA_RESIDUAL_DIFFERENCE_BUFFER     = 3;
  {$EXTERNALSYM DXVA_RESIDUAL_DIFFERENCE_BUFFER}
  DXVA_DEBLOCKING_CONTROL_BUFFER      = 4;
  {$EXTERNALSYM DXVA_DEBLOCKING_CONTROL_BUFFER}
  DXVA_INVERSE_QUANTIZATION_MATRIX_BUFFER= 5;
  {$EXTERNALSYM DXVA_INVERSE_QUANTIZATION_MATRIX_BUFFER}
  DXVA_SLICE_CONTROL_BUFFER           = 6;
  {$EXTERNALSYM DXVA_SLICE_CONTROL_BUFFER}
  DXVA_BITSTREAM_DATA_BUFFER          = 7;
  {$EXTERNALSYM DXVA_BITSTREAM_DATA_BUFFER}
  DXVA_AYUV_BUFFER                    = 8;
  {$EXTERNALSYM DXVA_AYUV_BUFFER}
  DXVA_IA44_SURFACE_BUFFER            = 9;
  {$EXTERNALSYM DXVA_IA44_SURFACE_BUFFER}
  DXVA_DPXD_SURFACE_BUFFER            = 10;
  {$EXTERNALSYM DXVA_DPXD_SURFACE_BUFFER}
  DXVA_HIGHLIGHT_BUFFER               = 11;
  {$EXTERNALSYM DXVA_HIGHLIGHT_BUFFER}
  DXVA_DCCMD_SURFACE_BUFFER           = 12;
  {$EXTERNALSYM DXVA_DCCMD_SURFACE_BUFFER}
  DXVA_ALPHA_BLEND_COMBINATION_BUFFER = 13;
  {$EXTERNALSYM DXVA_ALPHA_BLEND_COMBINATION_BUFFER}
  DXVA_PICTURE_RESAMPLE_BUFFER        = 14;
  {$EXTERNALSYM DXVA_PICTURE_RESAMPLE_BUFFER}
  DXVA_READ_BACK_BUFFER               = 15;
  {$EXTERNALSYM DXVA_READ_BACK_BUFFER}

type

  LPDXVA_ConfigPictureDecode = ^_DXVA_ConfigPictureDecode;
  {$EXTERNALSYM LPDXVA_ConfigPictureDecode}
  PDXVA_ConfigPictureDecode = ^DXVA_ConfigPictureDecode;
  _DXVA_ConfigPictureDecode = record
    // Operation Indicated
    dwFunction: DXVA_ConfigQueryOrReplyFunc;
    // Alignment
    dwReservedBits: array[0..2] of DWORD;
    // Encryption GUIDs
    guidConfigBitstreamEncryption: TGUID;
    guidConfigMBcontrolEncryption: TGUID;
    guidConfigResidDiffEncryption: TGUID;
    // Bitstream Processing Indicator
    bConfigBitstreamRaw: Byte;
    // Macroblock Control Config
    bConfigMBcontrolRasterOrder: Byte;
    // Host Resid Diff Config
    bConfigResidDiffHost: Byte;
    bConfigSpatialResid8: Byte;
    bConfigResid8Subtraction: Byte;
    bConfigSpatialHost8or9Clipping: Byte;
    bConfigSpatialResidInterleaved: Byte;
    bConfigIntraResidUnsigned: Byte;
    // Accelerator Resid Diff Config
    bConfigResidDiffAccelerator: Byte;
    bConfigHostInverseScan: Byte;
    bConfigSpecificIDCT: Byte;
    bConfig4GroupedCoefs: Byte;
  end;
  {$EXTERNALSYM _DXVA_ConfigPictureDecode}
  DXVA_ConfigPictureDecode = _DXVA_ConfigPictureDecode;
  {$EXTERNALSYM DXVA_ConfigPictureDecode}


  LPDXVA_PictureParameters = ^_DXVA_PictureParameters;
  {$EXTERNALSYM LPDXVA_PictureParameters}
  PDXVA_PictureParameters = ^DXVA_PictureParameters;
  _DXVA_PictureParameters = record
    wDecodedPictureIndex: WORD;
    wDeblockedPictureIndex: WORD;
    wForwardRefPictureIndex: WORD;
    wBackwardRefPictureIndex: WORD;
    wPicWidthInMBminus1: WORD;
    wPicHeightInMBminus1: WORD;
    bMacroblockWidthMinus1: Byte;
    bMacroblockHeightMinus1: Byte;
    bBlockWidthMinus1: Byte;
    bBlockHeightMinus1: Byte;
    bBPPminus1: Byte;
    bPicStructure: Byte;
    bSecondField: Byte;
    bPicIntra: Byte;
    bPicBackwardPrediction: Byte;
    bBidirectionalAveragingMode: Byte;
    bMVprecisionAndChromaRelation: Byte;
    bChromaFormat: Byte;
    bPicScanFixed: Byte;
    bPicScanMethod: Byte;
    bPicReadbackRequests: Byte;
    bRcontrol: Byte;
    bPicSpatialResid8: Byte;
    bPicOverflowBlocks: Byte;
    bPicExtrapolation: Byte;
    bPicDeblocked: Byte;
    bPicDeblockConfined: Byte;
    bPic4MVallowed: Byte;
    bPicOBMC: Byte;
    bPicBinPB: Byte;
    bMV_RPS: Byte;
    bReservedBits: Byte;
    wBitstreamFcodes: WORD;
    wBitstreamPCEelements: WORD;
    bBitstreamConcealmentNeed: Byte;
    bBitstreamConcealmentMethod: Byte;
  end;
  {$EXTERNALSYM _DXVA_PictureParameters}
  DXVA_PictureParameters = _DXVA_PictureParameters;
  {$EXTERNALSYM DXVA_PictureParameters}

// #pragma pack(pop, BeforeDXVApacking)
{$Z4}
{$A4}

{$ENDIF}  //* __DIRECTX_VA_DECODER__ *//


{$IFNDEF __DIRECTX_VA_DECODER9__}
{$DEFINE __DIRECTX_VA_DECODER9__}

  // -------------------------------------------------------------------------
  // Decoding data types used with RenderMoComp
  // -------------------------------------------------------------------------
  //

  PDXVAUncompDataInfo = ^DXVAUncompDataInfo;
  _DXVAUncompDataInfo = record
    UncompWidth: DWORD;              { Width of uncompressed data }
    UncompHeight: DWORD;             { Height of uncompressed data }
    UncompFormat: D3DFORMAT;         { Format of uncompressed data }
  end;
  {$EXTERNALSYM _DXVAUncompDataInfo}
  DXVAUncompDataInfo = _DXVAUncompDataInfo;
  {$EXTERNALSYM DXVAUncompDataInfo}


  PDXVACompBufferInfo = ^DXVACompBufferInfo;
  _DXVACompBufferInfo = record
    NumCompBuffers: DWORD;           { Number of buffers reqd for compressed data }
    WidthToCreate: DWORD;            { Width of surface to create }
    HeightToCreate: DWORD;           { Height of surface to create }
    BytesToAllocate: DWORD;          { Total number of bytes used by each surface }
    Usage: DWORD;                    { Usage used to create the compressed buffer }
    Pool: D3DPOOL;                   { Pool where the compressed buffer belongs }
    Format: D3DFORMAT;               { Format used to create the compressed buffer }
  end;
  {$EXTERNALSYM _DXVACompBufferInfo}
  DXVACompBufferInfo = _DXVACompBufferInfo;
  {$EXTERNALSYM DXVACompBufferInfo}


  PDXVABufferInfo = ^DXVABufferInfo;
  _DXVABufferInfo = record
    pCompSurface: Pointer;           { Pointer to buffer containing compressed data }
    DataOffset: DWORD;               { Offset of relevant data from the beginning of buffer }
    DataSize: DWORD;                 { Size of relevant data }
  end;
  {$EXTERNALSYM _DXVABufferInfo}
  DXVABufferInfo = _DXVABufferInfo;
  {$EXTERNALSYM DXVABufferInfo}

{$ENDIF}  //* __DIRECTX_VA_DECODER9__ *//


  // -------------------------------------------------------------------------
  //
  // D3DFORMAT describes a pixel memory layout, DXVA sample format contains
  // additional information that describes how the pixels should be interpreted.
  //
  // DXVA Extended color data - occupies the SampleFormat DWORD
  // data fields.
  // -------------------------------------------------------------------------

{$IFNDEF __DIRECTX_VA_SAMPLEFORMAT__}
{$DEFINE __DIRECTX_VA_SAMPLEFORMAT__}

  PDXVA_SampleFormat = ^DXVA_SampleFormat;
  _DXVA_SampleFormat                     = (
    DXVA_SampleFormatMask                = $FF,    // 8 bits used for DXVA Sample format
    DXVA_SampleUnknown                   = 0,
    DXVA_SamplePreviousFrame             = 1,
    DXVA_SampleProgressiveFrame          = 2,
    DXVA_SampleFieldInterleavedEvenFirst = 3,
    DXVA_SampleFieldInterleavedOddFirst  = 4,
    DXVA_SampleFieldSingleEven           = 5,
    DXVA_SampleFieldSingleOdd            = 6,
    DXVA_SampleSubStream                 = 7
  );
  {$EXTERNALSYM _DXVA_SampleFormat}
  DXVA_SampleFormat = _DXVA_SampleFormat;
  {$EXTERNALSYM DXVA_SampleFormat}

  function DXVA_ExtractSampleFormat(_sf: DWORD): DWORD; inline;
  {$EXTERNALSYM DXVA_ExtractSampleFormat}

  function DXVA_ExtractExtColorData(_sf: DWORD; _Mask: DWORD; _Shift: DWORD): DWORD; inline;
  {$EXTERNALSYM DXVA_ExtractExtColorData}

  function DXVABitMask(__n: DWORD): DWORD; inline;
  {$EXTERNALSYM DXVABitMask}

const
  DXVA_ExtColorData_ShiftBase         = 8;
  {$EXTERNALSYM DXVA_ExtColorData_ShiftBase}

  function DXVAColorMask(__bits: DWORD; __base: DWORD): DWORD; inline;
  {$EXTERNALSYM DXVAColorMask}


type

  _DXVA_VideoTransferFunction = (
    DXVA_VideoTransFuncShift = (DXVA_ExtColorData_ShiftBase + 19),
    DXVA_VideoTransFuncMask = ((($FFFFFFFF shl 5) xor $FFFFFFFF) shl ord(DXVA_VideoTransFuncShift)), // DXVAColorMask(5, DXVA_VideoTransFuncShift),

    DXVA_VideoTransFunc_Unknown      = 0,
    DXVA_VideoTransFunc_10           = 1,
    DXVA_VideoTransFunc_18           = 2,
    DXVA_VideoTransFunc_20           = 3,
    DXVA_VideoTransFunc_22           = 4,
    DXVA_VideoTransFunc_22_709       = 5,
    DXVA_VideoTransFunc_22_240M      = 6,
    DXVA_VideoTransFunc_22_8bit_sRGB = 7,
    DXVA_VideoTransFunc_28           = 8
    );
  {$EXTERNALSYM _DXVA_VideoTransferFunction}
  DXVA_VideoTransferFunction = _DXVA_VideoTransferFunction;
  {$EXTERNALSYM DXVA_VideoTransferFunction}


  _DXVA_VideoPrimaries = (
    DXVA_VideoPrimariesShift = (DXVA_ExtColorData_ShiftBase + 14),
    DXVA_VideoPrimariesMask = ((($FFFFFFFF shl 5) xor $FFFFFFFF) shl ord(DXVA_VideoPrimariesShift)), // DXVAColorMask(5, DXVA_VideoPrimariesShift),

    DXVA_VideoPrimaries_Unknown       = 0,
    DXVA_VideoPrimaries_reserved      = 1,
    DXVA_VideoPrimaries_BT709         = 2,
    DXVA_VideoPrimaries_BT470_2_SysM  = 3,
    DXVA_VideoPrimaries_BT470_2_SysBG = 4,
    DXVA_VideoPrimaries_SMPTE170M     = 5,
    DXVA_VideoPrimaries_SMPTE240M     = 6,
    DXVA_VideoPrimaries_EBU3213       = 7,
    DXVA_VideoPrimaries_SMPTE_C       = 8
  );
  {$EXTERNALSYM _DXVA_VideoPrimaries}
  DXVA_VideoPrimaries = _DXVA_VideoPrimaries;
  {$EXTERNALSYM DXVA_VideoPrimaries}

  _DXVA_VideoLighting = (
    DXVA_VideoLightingShift = (DXVA_ExtColorData_ShiftBase + 10),
    DXVA_VideoLightingMask = ((($FFFFFFFF shl 4) xor $FFFFFFFF) shl ord(DXVA_VideoLightingShift)), // DXVAColorMask(4, DXVA_VideoLightingShift),

    DXVA_VideoLighting_Unknown = 0,
    DXVA_VideoLighting_bright  = 1,
    DXVA_VideoLighting_office  = 2,
    DXVA_VideoLighting_dim     = 3,
    DXVA_VideoLighting_dark    = 4
  );
  {$EXTERNALSYM _DXVA_VideoLighting}
  DXVA_VideoLighting = _DXVA_VideoLighting;
  {$EXTERNALSYM DXVA_VideoLighting}


  _DXVA_VideoTransferMatrix = (
    DXVA_VideoTransferMatrixShift = (DXVA_ExtColorData_ShiftBase + 7),
    DXVA_VideoTransferMatrixMask = ((($FFFFFFFF shl 3) xor $FFFFFFFF) shl ord(DXVA_VideoTransferMatrixShift)), // DXVAColorMask(3, DXVA_VideoTransferMatrixShift),

    DXVA_VideoTransferMatrix_Unknown   = 0,
    DXVA_VideoTransferMatrix_BT709     = 1,
    DXVA_VideoTransferMatrix_BT601     = 2,
    DXVA_VideoTransferMatrix_SMPTE240M = 3
  );
  {$EXTERNALSYM _DXVA_VideoTransferMatrix}
  DXVA_VideoTransferMatrix = _DXVA_VideoTransferMatrix;
  {$EXTERNALSYM DXVA_VideoTransferMatrix}


  _DXVA_NominalRange = (
    DXVA_NominalRangeShift = (DXVA_ExtColorData_ShiftBase + 4),
    DXVA_NominalRangeMask = ((($FFFFFFFF shl 3) xor $FFFFFFFF) shl ord(DXVA_NominalRangeShift)), // DXVAColorMask(3, DXVA_NominalRangeShift),

    DXVA_NominalRange_Unknown = 0,
    DXVA_NominalRange_Normal  = 1,
    DXVA_NominalRange_Wide    = 2,

    DXVA_NominalRange_0_255   = 1,
    DXVA_NominalRange_16_235  = 2,
    DXVA_NominalRange_48_208  = 3
  );
  {$EXTERNALSYM _DXVA_NominalRange}
  DXVA_NominalRange = _DXVA_NominalRange;
  {$EXTERNALSYM DXVA_NominalRange}

type
  _DXVA_VideoChromaSubsampling = DWord;
  {$EXTERNALSYM _DXVA_VideoChromaSubsampling}
  DXVA_VideoChromaSubsampling = _DXVA_VideoChromaSubsampling;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling}
const
  DXVA_VideoChromaSubsamplingShift = (DXVA_ExtColorData_ShiftBase + 0);
  {$EXTERNALSYM DXVA_VideoChromaSubsamplingShift}
  DXVA_VideoChromaSubsamplingMask = ((($FFFFFFFF shl 4) xor $FFFFFFFF) shl ord(DXVA_VideoChromaSubsamplingShift)); // DXVAColorMask(4, DXVA_VideoChromaSubsamplingShift),
  {$EXTERNALSYM DXVA_VideoChromaSubsamplingMask}

  DXVA_VideoChromaSubsampling_Unknown                        = 0;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Unknown}
  DXVA_VideoChromaSubsampling_ProgressiveChroma              = $8;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_ProgressiveChroma}
  DXVA_VideoChromaSubsampling_Horizontally_Cosited           = $4;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Horizontally_Cosited}
  DXVA_VideoChromaSubsampling_Vertically_Cosited             = $2;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Vertically_Cosited}
  DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes = $1;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes}

  // 4:2:0 variations
  DXVA_VideoChromaSubsampling_MPEG2   = (DXVA_VideoChromaSubsampling_Horizontally_Cosited or
                                         DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes);
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_MPEG2}

  DXVA_VideoChromaSubsampling_MPEG1   = (DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes);
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_MPEG1}

  DXVA_VideoChromaSubsampling_DV_PAL  = (DXVA_VideoChromaSubsampling_Horizontally_Cosited or
                                         DXVA_VideoChromaSubsampling_Vertically_Cosited);
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_DV_PAL}

  // 4:4:4, 4:2:2, 4:1:1
  DXVA_VideoChromaSubsampling_Cosited = (DXVA_VideoChromaSubsampling_Horizontally_Cosited or
                                         DXVA_VideoChromaSubsampling_Vertically_Cosited or
                                         DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes);
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Cosited}


type
  // See also MfPack.DxVa _DXVA_ExtendedFormat record and comments.

  PDXVA_ExtendedFormat = ^_DXVA_ExtendedFormat;
  _DXVA_ExtendedFormat = record
    public
      value: integer;
    private
      function ReadBits(const iIndex: Integer): Integer;
      procedure WriteBits(const iIndex: Integer; const iValue: Integer);
    public
      property SampleFormat:           Integer index $0008 read ReadBits write WriteBits; // 8 bits at offset 0, See DXVA_SampleFormat
      property VideoChromaSubsampling: Integer index $0804 read ReadBits write WriteBits; // 4 bits at offset 8, See DXVA_VideoChromaSubSampling
      property NominalRange:           Integer index $0c03 read ReadBits write WriteBits; // 3 bits at offset 12, See DXVA_NominalRange
      property VideoTransferMatrix:    Integer index $0f03 read ReadBits write WriteBits; // 3 bits at offset 15, See DXVA_VideoTransferMatrix
      property VideoLighting:          Integer index $1204 read ReadBits write WriteBits; // 4 bits at offset 18, See DXVA_VideoLighting
      property VideoPrimaries:         Integer index $1605 read ReadBits write WriteBits; // 5 bits at offset 22, See DXVA_VideoPrimaries
      property VideoTransferFunction:  Integer index $1b01 read ReadBits write WriteBits; // 5 bits at offset 27, See DXVA_VideoTransferFunction
  end;
  {$EXTERNALSYM _DXVA_ExtendedFormat}
 DXVA_ExtendedFormat = _DXVA_ExtendedFormat;
 {$EXTERNALSYM DXVA_ExtendedFormat}

{$ENDIF}  //* __DIRECTX_VA_SAMPLEFORMAT__ *//


  // -------------------------------------------------------------------------
  //
  // The definitions that follow describe the video de-interlace interface
  // between the VMR and the graphics device driver.  This interface is not
  // accessable via the IAMVideoAccelerator interface.
  //
  // -------------------------------------------------------------------------
  //

{$IFNDEF __DIRECTX_VA_DEINTERLACE__}
{$DEFINE __DIRECTX_VA_DEINTERLACE__}


{$IFNDEF MFP_REFERENCE_TIME}
    type REFERENCE_TIME = LONGLONG;
    {$EXTERNALSYM REFERENCE_TIME}
{$ENDIF}

const

  DXVAp_DeinterlaceBobDevice       : TGUID = '{335aa36e-7884-43a4-9c91-7f87faf3e37e}';
  {$EXTERNALSYM DXVAp_DeinterlaceBobDevice}
  DXVAp_DeinterlaceContainerDevice : TGUID = '{0e85cb93-3046-4ff0-aecc-d58cb5f035fd}';
  {$EXTERNALSYM DXVAp_DeinterlaceContainerDevice}


  {$EXTERNALSYM DXVA_DeinterlaceBobDevice}
  DXVA_DeinterlaceBobDevice        : TGUID = '{335aa36e-7884-43a4-9c91-7f87faf3e37e}'; // = DXVAp_DeinterlaceBobDevice
  {$EXTERNALSYM DXVA_DeinterlaceContainerDevice}
  DXVA_DeinterlaceContainerDevice  : TGUID = '{0e85cb93-3046-4ff0-aecc-d58cb5f035fd}'; // = DXVAp_DeinterlaceContainerDevice

//#if (DIRECT3D_VERSION < 0x0800) || !defined(DIRECT3D_VERSION)
type D3DFORMAT = DWORD;
const
  D3DPOOL_DEFAULT                 = 0;
  {$EXTERNALSYM D3DPOOL_DEFAULT}
  D3DPOOL_MANAGED                 = 1;
  {$EXTERNALSYM D3DPOOL_MANAGED}
  D3DPOOL_SYSTEMMEM               = 2;
  {$EXTERNALSYM D3DPOOL_SYSTEMMEM}
  D3DPOOL_SCRATCH                 = 3;
  {$EXTERNALSYM D3DPOOL_SCRATCH}
  D3DPOOL_LOCALVIDMEM             = 4;
  {$EXTERNALSYM D3DPOOL_LOCALVIDMEM}
  D3DPOOL_NONLOCALVIDMEM          = 5;
  {$EXTERNALSYM D3DPOOL_NONLOCALVIDMEM}
  //D3DPOOL_FORCE_DWORD             = $7fffffff;


  // -------------------------------------------------------------------------
  // data structures shared by User mode and Kernel mode.
  // -------------------------------------------------------------------------
  //

type

  PDXVAFrequency = ^DXVA_Frequency;
  _DXVA_Frequency = record
    Numerator: DWORD;
    Denominator: DWORD;
  end;
  {$EXTERNALSYM _DXVA_Frequency}
  DXVA_Frequency = _DXVA_Frequency;
  {$EXTERNALSYM DXVA_Frequency}


  LPDXVA_VideoDesc = ^_DXVA_VideoDesc;
  {$EXTERNALSYM LPDXVA_VideoDesc}
  PDXVAVideoDesc = ^DXVA_VideoDesc;
  _DXVA_VideoDesc = record
    Size: DWORD;
    SampleWidth: DWORD;
    SampleHeight: DWORD;
    SampleFormat: DWORD;            // also contains extend color data
    d3dFormat: D3DFORMAT;
    InputSampleFreq: DXVA_Frequency;
    OutputFrameFreq: DXVA_Frequency;
  end;
  {$EXTERNALSYM _DXVA_VideoDesc}
  DXVA_VideoDesc = _DXVA_VideoDesc;
  {$EXTERNALSYM DXVA_VideoDesc}


type
  PDXVAVideoProcessCaps = ^DXVA_VideoProcessCaps;
  {$EXTERNALSYM _DXVA_VideoProcessCaps}
  _DXVA_VideoProcessCaps = DWord;
  DXVA_VideoProcessCaps = _DXVA_VideoProcessCaps;
  {$EXTERNALSYM DXVA_VideoProcessCaps}
const
  DXVA_VideoProcess_None               : _DXVA_VideoProcessCaps = $0000;
  {$EXTERNALSYM DXVA_VideoProcess_None}
  DXVA_VideoProcess_YUV2RGB            : _DXVA_VideoProcessCaps = $0001;
  {$EXTERNALSYM DXVA_VideoProcess_YUV2RGB}
  DXVA_VideoProcess_StretchX           : _DXVA_VideoProcessCaps = $0002;
  {$EXTERNALSYM DXVA_VideoProcess_StretchX}
  DXVA_VideoProcess_StretchY           : _DXVA_VideoProcessCaps = $0004;
  {$EXTERNALSYM DXVA_VideoProcess_StretchY}
  DXVA_VideoProcess_AlphaBlend         : _DXVA_VideoProcessCaps = $0008;
  {$EXTERNALSYM DXVA_VideoProcess_AlphaBlend}
  DXVA_VideoProcess_SubRects           : _DXVA_VideoProcessCaps = $0010;
  {$EXTERNALSYM DXVA_VideoProcess_SubRects}
  DXVA_VideoProcess_SubStreams         : _DXVA_VideoProcessCaps = $0020;
  {$EXTERNALSYM DXVA_VideoProcess_SubStreams}
  DXVA_VideoProcess_SubStreamsExtended : _DXVA_VideoProcessCaps = $0040;
  {$EXTERNALSYM DXVA_VideoProcess_SubStreamsExtended}
  DXVA_VideoProcess_YUV2RGBExtended    : _DXVA_VideoProcessCaps = $0080;
  {$EXTERNALSYM DXVA_VideoProcess_YUV2RGBExtended}
  DXVA_VideoProcess_AlphaBlendExtended : _DXVA_VideoProcessCaps = $0100;
  {$EXTERNALSYM DXVA_VideoProcess_AlphaBlendExtended}


type

  PDXVADeinterlaceTech = ^DXVA_DeinterlaceTech;
  _DXVA_DeinterlaceTech                         = (
    // the algorithm is unknown or proprietary
    DXVA_DeinterlaceTech_Unknown                = $0000,
    // the algorithm creates the missing lines by repeating
    // the line either above or below it - this method will look very jaggy and
    // isn't recommended
    DXVA_DeinterlaceTech_BOBLineReplicate       = $0001,
    // The algorithm creates the missing lines by vertically stretching each
    // video field by a factor of two by averaging two lines
    DXVA_DeinterlaceTech_BOBVerticalStretch     = $0002,
    // or using a [-1, 9, 9, -1]/16 filter across four lines.
    DXVA_DeinterlaceTech_BOBVerticalStretch4Tap = $0100,
    // the pixels in the missing line are recreated by a median filtering operation
    DXVA_DeinterlaceTech_MedianFiltering        = $0004,
    // the pixels in the missing line are recreated by an edge filter.
    // In this process, spatial directional filters are applied to determine
    // the orientation of edges in the picture content, and missing
    // pixels are created by filtering along (rather than across) the
    // detected edges.
    DXVA_DeinterlaceTech_EdgeFiltering          = $0010,
    // the pixels in the missing line are recreated by switching on a field by
    // field basis between using either spatial or temporal interpolation
    // depending on the amount of motion.
    DXVA_DeinterlaceTech_FieldAdaptive          = $0020,
    // the pixels in the missing line are recreated by switching on a pixel by pixel
    // basis between using either spatial or temporal interpolation depending on
    // the amount of motion..
    DXVA_DeinterlaceTech_PixelAdaptive          = $0040,
    // Motion Vector Steering  identifies objects within a sequence of video
    // fields.  The missing pixels are recreated after first aligning the
    // movement axes of the individual objects in the scene to make them
    // parallel with the time axis.
    DXVA_DeinterlaceTech_MotionVectorSteered    = $0080
  );
  {$EXTERNALSYM _DXVA_DeinterlaceTech}
  DXVA_DeinterlaceTech = _DXVA_DeinterlaceTech;
  {$EXTERNALSYM DXVA_DeinterlaceTech}


  LPDXVA_VideoSample = ^_DXVA_VideoSample;
  {$EXTERNALSYM LPDXVA_VideoSample}
  PDXVAVideoSample = ^DXVA_VideoSample;
  _DXVA_VideoSample = record
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    SampleFormat: DXVA_SampleFormat;  // only lower 8 bits used
    lpDDSSrcSurface: Pointer;
  end;
  {$EXTERNALSYM _DXVA_VideoSample}
  DXVA_VideoSample = _DXVA_VideoSample;
  {$EXTERNALSYM DXVA_VideoSample}

  // -------------------------------------------------------------------------
  // DeinterlaceBltEx declarations
  // -------------------------------------------------------------------------
  //

type
  PDXVASampleFlags = ^DXVA_SampleFlags;
  _DXVA_SampleFlags  = DWord;
  {$EXTERNALSYM _DXVA_SampleFlags}
  DXVA_SampleFlags = _DXVA_SampleFlags;
  {$EXTERNALSYM DXVA_SampleFlags}
const
  DXVA_SampleFlagsMask              = (1 shl 3) or (1 shl 2) or (1 shl 1) or (1 shl 1); // DXVABit(3)|DXVABit(2)|DXVABit(1)|DXVABit(0)
  {$EXTERNALSYM DXVA_SampleFlagsMask}
  DXVA_SampleFlag_Palette_Changed   = $0001;
  {$EXTERNALSYM DXVA_SampleFlag_Palette_Changed}
  DXVA_SampleFlag_SrcRect_Changed   = $0002;
  {$EXTERNALSYM DXVA_SampleFlag_SrcRect_Changed}
  DXVA_SampleFlag_DstRect_Changed   = $0004;
  {$EXTERNALSYM DXVA_SampleFlag_DstRect_Changed}
  DXVA_SampleFlag_ColorData_Changed = $0008;
  {$EXTERNALSYM DXVA_SampleFlag_ColorData_Changed}


type
  PDXVADestinationFlags = ^DXVA_DestinationFlags;
  _DXVA_DestinationFlags = DWord;
  {$EXTERNALSYM _DXVA_DestinationFlags}
  DXVA_DestinationFlags = _DXVA_DestinationFlags;
  {$EXTERNALSYM DXVA_DestinationFlags}
const
  DXVA_DestinationFlagMask                = (1 shl 3) or (1 shl 2) or (1 shl 1) or (1 shl 1); // DXVABit(3)|DXVABit(2)|DXVABit(1)|DXVABit(0)
  {$EXTERNALSYM DXVA_DestinationFlagMask}
  DXVA_DestinationFlag_Background_Changed = $0001;
  {$EXTERNALSYM DXVA_DestinationFlag_Background_Changed}
  DXVA_DestinationFlag_TargetRect_Changed = $0002;
  {$EXTERNALSYM DXVA_DestinationFlag_TargetRect_Changed}
  DXVA_DestinationFlag_ColorData_Changed  = $0004;
  {$EXTERNALSYM DXVA_DestinationFlag_ColorData_Changed}
  DXVA_DestinationFlag_Alpha_Changed      = $0008;
  {$EXTERNALSYM DXVA_DestinationFlag_Alpha_Changed}

type

  LPDXVA_VideoSample2 = ^_DXVA_VideoSample2;
  {$EXTERNALSYM LPDXVA_VideoSample2}
  PDXVA_VideoSample2 = ^DXVA_VideoSample2;
  _DXVA_VideoSample2 = record
{$IFDEF WIN64}
    Size: DWORD;
    Reserved: DWORD;
{$ENDIF}
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    SampleFormat: DWORD;    // cast to DXVA_ExtendedFormat, or use Extract macros
    SampleFlags: DWORD;
    lpDDSSrcSurface: Pointer;
    rcSrc: TRect;
    rcDst: TRect;
    Palette: array [0..15] of DXVA_AYUVsample2;
  end;
  {$EXTERNALSYM _DXVA_VideoSample2}
  DXVA_VideoSample2 = _DXVA_VideoSample2;
  {$EXTERNALSYM DXVA_VideoSample2}


  LPDXVA_DeinterlaceCaps = ^_DXVA_DeinterlaceCaps;
  {$EXTERNALSYM LPDXVA_DeinterlaceCaps}
  PDXVADeinterlaceCaps = ^DXVA_DeinterlaceCaps;
  _DXVA_DeinterlaceCaps = record
    Size: DWORD;
    NumPreviousOutputFrames: DWORD;
    InputPool: DWORD;
    NumForwardRefSamples: DWORD;
    NumBackwardRefSamples: DWORD;
    d3dOutputFormat: D3DFORMAT;
    VideoProcessingCaps: DXVA_VideoProcessCaps;
    DeinterlaceTechnology: DXVA_DeinterlaceTech;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceCaps}
  DXVA_DeinterlaceCaps = _DXVA_DeinterlaceCaps;
  {$EXTERNALSYM DXVA_DeinterlaceCaps}

  // -------------------------------------------------------------------------
  // Data types used with RenderMoComp in kernel mode
  // -------------------------------------------------------------------------
  //

  // Function codes for RenderMoComp

const
  {$EXTERNALSYM MAX_DEINTERLACE_SURFACES}
  MAX_DEINTERLACE_SURFACES            = 32;


{$IFDEF WIN64}
  //
  // These structures are used for thunking 32 bit DeinterlaceBltEx calls on
  // 64 bit drivers.
  //
type

  PDXVAVideoSample32 = ^DXVA_VideoSample32;
  _DXVA_VideoSample32 = record
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    SampleFormat: DWORD;
    SampleFlags: DWORD;
    lpDDSSrcSurface: DWORD;         // 32 bit pointer size
    rcSrc: RECT;
    rcDst: RECT;
    Palette: array[0..15] of DXVA_AYUVsample2;
    // DWORD Pad;
    // 4 bytes of padding added by the compiler to align the struct to 8 bytes.
  end;
  {$EXTERNALSYM _DXVA_VideoSample32}
  DXVA_VideoSample32 = _DXVA_VideoSample32;
  {$EXTERNALSYM DXVA_VideoSample32}

  PDXVADeinterlaceBltEx32 = ^DXVA_DeinterlaceBltEx32;
  _DXVA_DeinterlaceBltEx32 = record
    Size: DWORD;
    BackgroundColor: DXVA_AYUVsample2;
    rcTarget: RECT;
    rtTarget: REFERENCE_TIME;
    NumSourceSurfaces: DWORD;
    Alpha: Single;
    Source: array[0..MAX_DEINTERLACE_SURFACES - 1] of DXVA_VideoSample32;
    DestinationFormat: DWORD;
    DestinationFlags: DWORD;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceBltEx32}
  DXVA_DeinterlaceBltEx32 = _DXVA_DeinterlaceBltEx32;
  {$EXTERNALSYM DXVA_DeinterlaceBltEx32}

{$ENDIF} //* WIN64 *//

type

  PDXVADeinterlaceBlt = ^DXVA_DeinterlaceBlt;
  _DXVA_DeinterlaceBlt = record
    Size: DWORD;
    Reserved: DWORD;
    rtTarget: REFERENCE_TIME;
    DstRect: TRect;
    SrcRect: TRect;
    NumSourceSurfaces: DWORD;
    Alpha: Single;
    Source: array[0..MAX_DEINTERLACE_SURFACES - 1] of DXVA_VideoSample;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceBlt}
  DXVA_DeinterlaceBlt = _DXVA_DeinterlaceBlt;
  {$EXTERNALSYM DXVA_DeinterlaceBlt}

const
  DXVA_DeinterlaceBltFnCode           = $01;
  {$EXTERNALSYM DXVA_DeinterlaceBltFnCode}
  // lpInput => DXVA_DeinterlaceBlt*
  // lpOuput => NULL /* not currently used */

type

  PDXVADeinterlaceBltEx = ^DXVA_DeinterlaceBltEx;
  _DXVA_DeinterlaceBltEx = record
    Size: DWORD;
    BackgroundColor: DXVA_AYUVsample2;
    rcTarget: TRect;
    rtTarget: REFERENCE_TIME;
    NumSourceSurfaces: DWORD;
    Alpha: Single;
    Source: array[0..MAX_DEINTERLACE_SURFACES - 1] of DXVA_VideoSample2;
    DestinationFormat: DWORD;
    DestinationFlags: DWORD;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceBltEx}
  DXVA_DeinterlaceBltEx = _DXVA_DeinterlaceBltEx;
  {$EXTERNALSYM DXVA_DeinterlaceBltEx}

const
  DXVA_DeinterlaceBltExFnCode         = $02;
  {$EXTERNALSYM DXVA_DeinterlaceBltExFnCode}
  // lpInput => DXVA_DeinterlaceBltEx*
  // lpOuput => NULL /* not currently used */

  MAX_DEINTERLACE_DEVICE_GUIDS        = 32;
  {$EXTERNALSYM MAX_DEINTERLACE_DEVICE_GUIDS}

type

  PDXVADeinterlaceQueryAvailableModes = ^DXVA_DeinterlaceQueryAvailableModes;
  _DXVA_DeinterlaceQueryAvailableModes = record
    Size: DWORD;
    NumGuids: DWORD;
    Guids: array[0..MAX_DEINTERLACE_DEVICE_GUIDS - 1] of TGUID;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceQueryAvailableModes}
  DXVA_DeinterlaceQueryAvailableModes = _DXVA_DeinterlaceQueryAvailableModes;
  {$EXTERNALSYM DXVA_DeinterlaceQueryAvailableModes}

const
  DXVA_DeinterlaceQueryAvailableModesFnCode = $01;
  {$EXTERNALSYM DXVA_DeinterlaceQueryAvailableModesFnCode}
  // lpInput => DXVA_VideoDesc*
  // lpOuput => DXVA_DeinterlaceQueryAvailableModes*


type

  PDXVADeinterlaceQueryModeCaps = ^DXVA_DeinterlaceQueryModeCaps;
  _DXVA_DeinterlaceQueryModeCaps = record
    Size: DWORD;
    Guid: TGUID;
    VideoDesc: DXVA_VideoDesc;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceQueryModeCaps}
  DXVA_DeinterlaceQueryModeCaps = _DXVA_DeinterlaceQueryModeCaps;
  {$EXTERNALSYM DXVA_DeinterlaceQueryModeCaps}

const
  DXVA_DeinterlaceQueryModeCapsFnCode = $02;
  {$EXTERNALSYM DXVA_DeinterlaceQueryModeCapsFnCode}
  // lpInput => DXVA_DeinterlaceQueryModeCaps*
  // lpOuput => DXVA_DeinterlaceCaps*

{$ENDIF} //* __DIRECTX_VA_DEINTERLACE__ *//


  // -------------------------------------------------------------------------
  //
  // The definitions that follow describe the video ProcAmp interface
  // between the VMR and the graphics device driver.  This interface is not
  // accessable via the IAMVideoAccelerator interface.
  //
  // -------------------------------------------------------------------------
  //
{$IFNDEF __DIRECTX_VA_PROCAMPCONTROL__}
{$DEFINE __DIRECTX_VA_PROCAMPCONTROL__}

const
  DXVA_ProcAmpControlDevice : TGUID = '{9f200913-2ffd-4056-9f1e-e1b508f22dcf}';

type

  PDXVAProcAmpControlProp = ^DXVA_ProcAmpControlProp;
  _DXVA_ProcAmpControlProp  = (
    DXVA_ProcAmp_None       = $0000,
    DXVA_ProcAmp_Brightness = $0001,
    DXVA_ProcAmp_Contrast   = $0002,
    DXVA_ProcAmp_Hue        = $0004,
    DXVA_ProcAmp_Saturation = $0008
  );
  {$EXTERNALSYM _DXVA_ProcAmpControlProp}
  DXVA_ProcAmpControlProp = _DXVA_ProcAmpControlProp;
  {$EXTERNALSYM DXVA_ProcAmpControlProp}


  LPDXVA_ProcAmpControlCaps = ^_DXVA_ProcAmpControlCaps;
  {$EXTERNALSYM LPDXVA_ProcAmpControlCaps}
  PDXVAProcAmpControlCaps = ^DXVA_ProcAmpControlCaps;
  _DXVA_ProcAmpControlCaps = record
    Size: DWORD;
    InputPool: DWORD;
    d3dOutputFormat: D3DFORMAT;
    ProcAmpControlProps: DWORD;     // see DXVA_ProcAmpControlProp
    VideoProcessingCaps: DWORD;     // see DXVA_VideoProcessCaps
  end;
  {$EXTERNALSYM _DXVA_ProcAmpControlCaps}
  DXVA_ProcAmpControlCaps = _DXVA_ProcAmpControlCaps;
  {$EXTERNALSYM DXVA_ProcAmpControlCaps}

const

  DXVA_ProcAmpControlQueryCapsFnCode  = $03;
  {$EXTERNALSYM DXVA_ProcAmpControlQueryCapsFnCode}
  // lpInput => DXVA_VideoDesc*
  // lpOuput => DXVA_ProcAmpControlCaps*

type
  LPDXVA_ProcAmpControlQueryRange = ^_DXVA_ProcAmpControlQueryRange;
  {$EXTERNALSYM LPDXVA_ProcAmpControlQueryRange}
  PDXVAProcAmpControlQueryRange = ^DXVA_ProcAmpControlQueryRange;
  _DXVA_ProcAmpControlQueryRange = record
    Size: DWORD;
    ProcAmpControlProp: DXVA_ProcAmpControlProp;
    VideoDesc: DXVA_VideoDesc;
  end;
  {$EXTERNALSYM _DXVA_ProcAmpControlQueryRange}
  DXVA_ProcAmpControlQueryRange = _DXVA_ProcAmpControlQueryRange;
  {$EXTERNALSYM DXVA_ProcAmpControlQueryRange}

  LPDXVA_VideoPropertyRange = ^_DXVA_VideoPropertyRange;
  {$EXTERNALSYM LPDXVA_VideoPropertyRange}
  PDXVAVideoPropertyRange = ^DXVA_VideoPropertyRange;
  _DXVA_VideoPropertyRange = record
    MinValue: Single;
    MaxValue: Single;
    DefaultValue: Single;
    StepSize: Single;
  end;
  {$EXTERNALSYM _DXVA_VideoPropertyRange}
  DXVA_VideoPropertyRange = _DXVA_VideoPropertyRange;
  {$EXTERNALSYM DXVA_VideoPropertyRange}

const

  {$EXTERNALSYM DXVA_ProcAmpControlQueryRangeFnCode}
  DXVA_ProcAmpControlQueryRangeFnCode = $04;
  // lpInput => DXVA_ProcAmpControlQueryRange*
  // lpOuput => DXVA_VideoPropertyRange*

type

  PDXVAProcAmpControlBlt = ^DXVA_ProcAmpControlBlt;
  _DXVA_ProcAmpControlBlt = record
    Size: DWORD;
    DstRect: TRect;
    SrcRect: TRect;
    Alpha: Single;
    Brightness: Single;
    Contrast: Single;
    Hue: Single;
    Saturation: Single;
  end;
  {$EXTERNALSYM _DXVA_ProcAmpControlBlt}
  DXVA_ProcAmpControlBlt = _DXVA_ProcAmpControlBlt;
  {$EXTERNALSYM DXVA_ProcAmpControlBlt}

const
  DXVA_ProcAmpControlBltFnCode        = $01;
  {$EXTERNALSYM DXVA_ProcAmpControlBltFnCode}
  // lpInput => DXVA_ProcAmpControlBlt*
  // lpOuput => NULL /* not currently used */

{$ENDIF} //* __DIRECTX_VA_PROCAMPCONTROL__ *//


  // -------------------------------------------------------------------------
  //
  // The definitions that follow describe the Certified Output Protection
  // Protocol between the VMR and the graphics device driver.  This interface
  // is not accessable via the IAMVideoAccelerator interface.
  //
  // -------------------------------------------------------------------------
  //

{$IFNDEF __DIRECTX_VA_CERTOUTPUTPROTECT__}
{$DEFINE __DIRECTX_VA_CERTOUTPUTPROTECT__}

const
  DXVA_COPPDevice : TGUID = '{d2457add-8999-45ed-8a8a-d1aa047ba4d5}';

  // -------------------------------------------------------------------------
  // COPPGetCertificateLength
  // -------------------------------------------------------------------------
  DXVA_COPPGetCertificateLengthFnCode = $01;
  {$EXTERNALSYM DXVA_COPPGetCertificateLengthFnCode}
  // lpInput => NULL
  // lpOuput => DWORD*

  // -------------------------------------------------------------------------
  // COPPKeyExchange
  // -------------------------------------------------------------------------
  DXVA_COPPKeyExchangeFnCode          = $02;
  {$EXTERNALSYM DXVA_COPPKeyExchangeFnCode}
  // lpInputData => NULL
  // lpOuputData => GUID*


  // -------------------------------------------------------------------------
  // COPPSequenceStart
  // -------------------------------------------------------------------------

type

  LPDXVA_COPPSignature = ^_DXVA_COPPSignature;
  {$EXTERNALSYM LPDXVA_COPPSignature}
  PDXVACOPPSignature = ^DXVA_COPPSignature;
  _DXVA_COPPSignature = record
    Signature: array[0..255] of UCHAR;
  end;
  {$EXTERNALSYM _DXVA_COPPSignature}
  DXVA_COPPSignature = _DXVA_COPPSignature;
  {$EXTERNALSYM DXVA_COPPSignature}

const
  DXVA_COPPSequenceStartFnCode        = $03;
  {$EXTERNALSYM DXVA_COPPSequenceStartFnCode}
  // lpInputData => DXVA_COPPSignature*
  // lpOuputData => NULL

  // -------------------------------------------------------------------------
  // COPPCommand
  // -------------------------------------------------------------------------

type

  LPDXVA_COPPCommand = ^_DXVA_COPPCommand;
  {$EXTERNALSYM LPDXVA_COPPCommand}
  PDXVACOPPCommand = ^DXVA_COPPCommand;
  _DXVA_COPPCommand = record
    macKDI: TGUID;                         //   16 bytes
    guidCommandID: TGUID;                  //   16 bytes
    dwSequence: ULONG;                     //    4 bytes
    cbSizeData: ULONG;                     //    4 bytes
    CommandData: array[0..4055] of UCHAR;  // 4056 bytes (4056+4+4+16+16 = 4096)
  end;
  {$EXTERNALSYM _DXVA_COPPCommand}
  DXVA_COPPCommand = _DXVA_COPPCommand;
  {$EXTERNALSYM DXVA_COPPCommand}

const
  {$EXTERNALSYM DXVA_COPPCommandFnCode}
  DXVA_COPPCommandFnCode              = $04;
  // lpInputData => DXVA_COPPCommand*
  // lpOuputData => NULL

  DXVA_COPPSetProtectionLevel : TGUID = '{9bb9327c-4eb5-4727-9f00-b42b0919c0da}';

type

  PDXVACOPPSetProtectionLevelCmdData = ^DXVA_COPPSetProtectionLevelCmdData;
  _DXVA_COPPSetProtectionLevelCmdData = record
    ProtType: ULONG;
    ProtLevel: ULONG;
    ExtendedInfoChangeMask: ULONG;
    ExtendedInfoData: ULONG;
  end;
  {$EXTERNALSYM _DXVA_COPPSetProtectionLevelCmdData}
  DXVA_COPPSetProtectionLevelCmdData = _DXVA_COPPSetProtectionLevelCmdData;
  {$EXTERNALSYM DXVA_COPPSetProtectionLevelCmdData}

  // Set the HDCP protection level - (0 - 1 DWORD, 4 bytes)

type
  PCOPPHDCPProtectionLevel = ^_COPP_HDCP_Protection_Level;
  _COPP_HDCP_Protection_Level = DWord;
  {$EXTERNALSYM _COPP_HDCP_Protection_Level}
  COPP_HDCP_Protection_Level = _COPP_HDCP_Protection_Level;
  {$EXTERNALSYM COPP_HDCP_Protection_Level}
const
  COPP_HDCP_Level0     = COPP_HDCP_Protection_Level(0);
  {$EXTERNALSYM COPP_HDCP_Level0}
  COPP_HDCP_LevelMin   = COPP_HDCP_Protection_Level(COPP_HDCP_Level0);
  {$EXTERNALSYM COPP_HDCP_LevelMin}
  COPP_HDCP_Level1     = COPP_HDCP_Protection_Level(1);
  {$EXTERNALSYM COPP_HDCP_Level1}
  COPP_HDCP_LevelMax   = COPP_HDCP_Protection_Level(COPP_HDCP_Level1);
  {$EXTERNALSYM COPP_HDCP_LevelMax}
  //COPP_HDCP_ForceDWORD = $7FFFFFFF

type
  PCOPPCGMSAProtectionLevel = ^_COPP_CGMSA_Protection_Level;
  _COPP_CGMSA_Protection_Level = DWord;
  {$EXTERNALSYM _COPP_CGMSA_Protection_Level}
  COPP_CGMSA_Protection_Level = _COPP_CGMSA_Protection_Level;
  {$EXTERNALSYM COPP_CGMSA_Protection_Level}
const
  COPP_CGMSA_Disabled                      = COPP_CGMSA_Protection_Level(0);
  {$EXTERNALSYM COPP_CGMSA_Disabled}
  COPP_CGMSA_LevelMin                      = COPP_CGMSA_Protection_Level(COPP_CGMSA_Disabled);
  {$EXTERNALSYM COPP_CGMSA_LevelMin}
  COPP_CGMSA_CopyFreely                    = COPP_CGMSA_Protection_Level(1);
  {$EXTERNALSYM COPP_CGMSA_CopyFreely}
  COPP_CGMSA_CopyNoMore                    = COPP_CGMSA_Protection_Level(2);
  {$EXTERNALSYM COPP_CGMSA_CopyNoMore}
  COPP_CGMSA_CopyOneGeneration             = COPP_CGMSA_Protection_Level(3);
  {$EXTERNALSYM COPP_CGMSA_CopyOneGeneration}
  COPP_CGMSA_CopyNever                     = COPP_CGMSA_Protection_Level(4);
  {$EXTERNALSYM COPP_CGMSA_CopyNever}
  COPP_CGMSA_RedistributionControlRequired = COPP_CGMSA_Protection_Level($08);
  {$EXTERNALSYM COPP_CGMSA_RedistributionControlRequired}
  COPP_CGMSA_LevelMax                      = COPP_CGMSA_Protection_Level(COPP_CGMSA_RedistributionControlRequired + COPP_CGMSA_CopyNever);
  {$EXTERNALSYM COPP_CGMSA_LevelMax}
  //COPP_CGMSA_ForceDWORD                  = $7FFFFFFF;

type
  PCOPPACPProtectionLevel = ^_COPP_ACP_Protection_Level;
  _COPP_ACP_Protection_Level = DWord;
  {$EXTERNALSYM _COPP_ACP_Protection_Level}
  COPP_ACP_Protection_Level = _COPP_ACP_Protection_Level;
  {$EXTERNALSYM COPP_ACP_Protection_Level}
const
  COPP_ACP_Level0     = COPP_ACP_Protection_Level(0);
  {$EXTERNALSYM COPP_ACP_Level0}
  COPP_ACP_LevelMin   = COPP_ACP_Protection_Level(COPP_ACP_Level0);
  {$EXTERNALSYM COPP_ACP_LevelMin}
  COPP_ACP_Level1     = COPP_ACP_Protection_Level(1);
  {$EXTERNALSYM COPP_ACP_Level1}
  COPP_ACP_Level2     = COPP_ACP_Protection_Level(2);
  {$EXTERNALSYM COPP_ACP_Level2}
  COPP_ACP_Level3     = COPP_ACP_Protection_Level(3);
  {$EXTERNALSYM COPP_ACP_Level3}
  COPP_ACP_LevelMax   = COPP_ACP_Protection_Level(COPP_ACP_Level3);
  {$EXTERNALSYM COPP_ACP_LevelMax}
  //COPP_ACP_ForceDWORD = $7FFFFFFF;


  COPP_NoProtectionLevelAvailable     = - 1;
  {$EXTERNALSYM COPP_NoProtectionLevelAvailable}
  COPP_DefaultProtectionLevel         = 0;
  {$EXTERNALSYM COPP_DefaultProtectionLevel}

  //
  // Bit flags of possible protection types.  Note that it is possible to apply
  // different protection settings to a single connector.
  //

  COPP_ProtectionType_Unknown      = $80000000;
  {$EXTERNALSYM COPP_ProtectionType_Unknown}
  COPP_ProtectionType_None         = $00000000;
  {$EXTERNALSYM COPP_ProtectionType_None}
  COPP_ProtectionType_HDCP         = $00000001;
  {$EXTERNALSYM COPP_ProtectionType_HDCP}
  COPP_ProtectionType_ACP          = $00000002;
  {$EXTERNALSYM COPP_ProtectionType_ACP}
  COPP_ProtectionType_CGMSA        = $00000004;
  {$EXTERNALSYM COPP_ProtectionType_CGMSA}
  COPP_ProtectionType_Mask         = $80000007;
  {$EXTERNALSYM COPP_ProtectionType_Mask}
  COPP_ProtectionType_Reserved     = $7FFFFFF8;
  {$EXTERNALSYM COPP_ProtectionType_Reserved}


  DXVA_COPPSetSignaling : TGUID = '{09a631a5-d684-4c60-8e4d-d3bb0f0be3ee}';
  {$EXTERNALSYM DXVA_COPPSetSignaling}

type

  PDXVACOPPSetSignalingCmdData = ^_DXVA_COPPSetSignalingCmdData;
  {$EXTERNALSYM _DXVA_COPPSetSignalingCmdData}
  _DXVA_COPPSetSignalingCmdData = record
    ActiveTVProtectionStandard: ULONG;  // See COPP_TVProtectionStandard
    AspectRatioChangeMask1: ULONG;
    AspectRatioData1: ULONG;            // See COPP_ImageAspectRatio_EN300294 for ETSI EN 300 294 values
    AspectRatioChangeMask2: ULONG;
    AspectRatioData2: ULONG;
    AspectRatioChangeMask3: ULONG;
    AspectRatioData3: ULONG;
    ExtendedInfoChangeMask: array[0..3] of ULONG;
    ExtendedInfoData: array[0..3] of ULONG;
    Reserved: ULONG;
  end;
  {$EXTERNALSYM DXVA_COPPSetSignalingCmdData}
  DXVA_COPPSetSignalingCmdData = _DXVA_COPPSetSignalingCmdData;

type
  // Add format enum and data enum
  PCOPPTVProtectionStandard = ^_COPP_TVProtectionStandard;
  _COPP_TVProtectionStandard = DWord;
  {$EXTERNALSYM _COPP_TVProtectionStandard}
  COPP_TVProtectionStandard = _COPP_TVProtectionStandard;
  {$EXTERNALSYM COPP_TVProtectionStandard}
const
  COPP_ProtectionStandard_Unknown             = COPP_TVProtectionStandard($80000000);
  {$EXTERNALSYM COPP_ProtectionStandard_Unknown}
  COPP_ProtectionStandard_None                = COPP_TVProtectionStandard($00000000);
  {$EXTERNALSYM COPP_ProtectionStandard_None}
  COPP_ProtectionStandard_IEC61880_525i       = COPP_TVProtectionStandard($00000001);
  {$EXTERNALSYM COPP_ProtectionStandard_IEC61880_525i}
  COPP_ProtectionStandard_IEC61880_2_525i     = COPP_TVProtectionStandard($00000002);
  {$EXTERNALSYM COPP_ProtectionStandard_IEC61880_2_525i}
  COPP_ProtectionStandard_IEC62375_625p       = COPP_TVProtectionStandard($00000004);
  {$EXTERNALSYM COPP_ProtectionStandard_IEC62375_625p}
  COPP_ProtectionStandard_EIA608B_525         = COPP_TVProtectionStandard($00000008);
  {$EXTERNALSYM COPP_ProtectionStandard_EIA608B_525}
  COPP_ProtectionStandard_EN300294_625i       = COPP_TVProtectionStandard($00000010);
  {$EXTERNALSYM COPP_ProtectionStandard_EN300294_625i}
  COPP_ProtectionStandard_CEA805A_TypeA_525p  = COPP_TVProtectionStandard($00000020);
  {$EXTERNALSYM COPP_ProtectionStandard_CEA805A_TypeA_525p}
  COPP_ProtectionStandard_CEA805A_TypeA_750p  = COPP_TVProtectionStandard($00000040);
  {$EXTERNALSYM COPP_ProtectionStandard_CEA805A_TypeA_750p}
  COPP_ProtectionStandard_CEA805A_TypeA_1125i = COPP_TVProtectionStandard($00000080);
  {$EXTERNALSYM COPP_ProtectionStandard_CEA805A_TypeA_1125i}
  COPP_ProtectionStandard_CEA805A_TypeB_525p  = COPP_TVProtectionStandard($00000100);
  {$EXTERNALSYM COPP_ProtectionStandard_CEA805A_TypeB_525p}
  COPP_ProtectionStandard_CEA805A_TypeB_750p  = COPP_TVProtectionStandard($00000200);
  {$EXTERNALSYM COPP_ProtectionStandard_CEA805A_TypeB_750p}
  COPP_ProtectionStandard_CEA805A_TypeB_1125i = COPP_TVProtectionStandard($00000400);
  {$EXTERNALSYM COPP_ProtectionStandard_CEA805A_TypeB_1125i}
  COPP_ProtectionStandard_ARIBTRB15_525i      = COPP_TVProtectionStandard($00000800);
  {$EXTERNALSYM COPP_ProtectionStandard_ARIBTRB15_525i}
  COPP_ProtectionStandard_ARIBTRB15_525p      = COPP_TVProtectionStandard($00001000);
  {$EXTERNALSYM COPP_ProtectionStandard_ARIBTRB15_525p}
  COPP_ProtectionStandard_ARIBTRB15_750p      = COPP_TVProtectionStandard($00002000);
  {$EXTERNALSYM COPP_ProtectionStandard_ARIBTRB15_750p}
  COPP_ProtectionStandard_ARIBTRB15_1125i     = COPP_TVProtectionStandard($00004000);
  {$EXTERNALSYM COPP_ProtectionStandard_ARIBTRB15_1125i}
  COPP_ProtectionStandard_Mask                = COPP_TVProtectionStandard($80007FFF);
  {$EXTERNALSYM COPP_ProtectionStandard_Mask}
  COPP_ProtectionStandard_Reserved            = COPP_TVProtectionStandard($7FFF8000);
  {$EXTERNALSYM COPP_ProtectionStandard_Reserved}


const
  COPP_ImageAspectRatio_EN300294_Mask = $00000007;
  {$EXTERNALSYM COPP_ImageAspectRatio_EN300294_Mask}


type
  PCOPPImageAspectRatioEN300294 = ^_COPP_ImageAspectRatio_EN300294;
  _COPP_ImageAspectRatio_EN300294 = DWord;
  {$EXTERNALSYM _COPP_ImageAspectRatio_EN300294}
  COPP_ImageAspectRatio_EN300294 = _COPP_ImageAspectRatio_EN300294;
  {$EXTERNALSYM COPP_ImageAspectRatio_EN300294}
const
  COPP_AspectRatio_EN300294_FullFormat4by3                = COPP_ImageAspectRatio_EN300294(0);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_FullFormat4by3}
  COPP_AspectRatio_EN300294_Box14by9Center                = COPP_ImageAspectRatio_EN300294(1);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_Box14by9Center}
  COPP_AspectRatio_EN300294_Box14by9Top                   = COPP_ImageAspectRatio_EN300294(2);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_Box14by9Top}
  COPP_AspectRatio_EN300294_Box16by9Center                = COPP_ImageAspectRatio_EN300294(3);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_Box16by9Center}
  COPP_AspectRatio_EN300294_Box16by9Top                   = COPP_ImageAspectRatio_EN300294(4);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_Box16by9Top}
  COPP_AspectRatio_EN300294_BoxGT16by9Center              = COPP_ImageAspectRatio_EN300294(5);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_BoxGT16by9Center}
  COPP_AspectRatio_EN300294_FullFormat4by3ProtectedCenter = COPP_ImageAspectRatio_EN300294(6);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_FullFormat4by3ProtectedCenter}
  COPP_AspectRatio_EN300294_FullFormat16by9Anamorphic     = COPP_ImageAspectRatio_EN300294(7);
  {$EXTERNALSYM COPP_AspectRatio_EN300294_FullFormat16by9Anamorphic}
  //COPP_AspectRatio_ForceDWORD                             = $7FFFFFFF


type
  // -------------------------------------------------------------------------
  // COPPQueryStatus
  // -------------------------------------------------------------------------
  LPDXVA_COPPStatusInput = ^_DXVA_COPPStatusInput;
  {$EXTERNALSYM LPDXVA_COPPStatusInput}
  PDXVACOPPStatusInput = ^_DXVA_COPPStatusInput;
  _DXVA_COPPStatusInput = record
    rApp: TGUID;                           //   16 bytes
    guidStatusRequestID: TGUID;            //   16 bytes
    dwSequence: ULONG;                     //    4 bytes
    cbSizeData: ULONG;                     //    4 bytes
    StatusData: array[0..4055] of UCHAR;   // 4056 bytes (4056+4+4+16+16 = 4096)
  end;
  {$EXTERNALSYM _DXVA_COPPStatusInput}
  DXVA_COPPStatusInput = _DXVA_COPPStatusInput;
  {$EXTERNALSYM DXVA_COPPStatusInput}


  LPDXVA_COPPStatusOutput = ^_DXVA_COPPStatusOutput;
  {$EXTERNALSYM LPDXVA_COPPStatusOutput}
  PDXVACOPPStatusOutput = ^_DXVA_COPPStatusOutput;
  _DXVA_COPPStatusOutput = record
    macKDI: TGUID;                         //   16 bytes
    cbSizeData: ULONG;                     //    4 bytes
    COPPStatus: array[0..4075] of UCHAR;  // 4076 bytes (4076+16+4 = 4096)
  end;
  {$EXTERNALSYM _DXVA_COPPStatusOutput}
  DXVA_COPPStatusOutput = _DXVA_COPPStatusOutput;
  {$EXTERNALSYM DXVA_COPPStatusOutput}


type
  PCOPPStatusFlags = ^_COPP_StatusFlags;
  _COPP_StatusFlags = DWORD;
  {$EXTERNALSYM _COPP_StatusFlags}
  COPP_StatusFlags = _COPP_StatusFlags;
  {$EXTERNALSYM COPP_StatusFlags}
const
  COPP_StatusNormal          = COPP_StatusFlags($00);
  COPP_LinkLost              = COPP_StatusFlags($01);
  COPP_RenegotiationRequired = COPP_StatusFlags($02);
  COPP_StatusFlagsReserved   = COPP_StatusFlags($FFFFFFFC);


type
  PDXVACOPPStatusData = ^_DXVA_COPPStatusData;
  _DXVA_COPPStatusData = record
    rApp: TGUID;
    dwFlags: ULONG;                 // See COPP_StatusFlags above
    dwData: ULONG;
    ExtendedInfoValidMask: ULONG;
    ExtendedInfoData: ULONG;
  end;
  {$EXTERNALSYM _DXVA_COPPStatusData}
  DXVA_COPPStatusData = _DXVA_COPPStatusData;
  {$EXTERNALSYM DXVA_COPPStatusData}


  PDXVACOPPStatusDisplayData = ^_DXVA_COPPStatusDisplayData;
  _DXVA_COPPStatusDisplayData = record
    rApp: TGUID;
    dwFlags: ULONG;                  // See COPP_StatusFlags above
    DisplayWidth: ULONG;
    DisplayHeight: ULONG;
    Format: ULONG;                   // also contains extended color data
    d3dFormat: ULONG;
    FreqNumerator: ULONG;
    FreqDenominator: ULONG;
  end;
  {$EXTERNALSYM _DXVA_COPPStatusDisplayData}
  DXVA_COPPStatusDisplayData = _DXVA_COPPStatusDisplayData;
  {$EXTERNALSYM DXVA_COPPStatusDisplayData}


type
  PCOPPStatusHDCPFlags = ^_COPP_StatusHDCPFlags;
  _COPP_StatusHDCPFlags    = DWord;
  {$EXTERNALSYM _COPP_StatusHDCPFlags}
  COPP_StatusHDCPFlags = _COPP_StatusHDCPFlags;
  {$EXTERNALSYM COPP_StatusHDCPFlags}
const
  COPP_HDCPRepeater      = COPP_StatusHDCPFlags($01);
  COPP_HDCPFlagsReserved = COPP_StatusHDCPFlags($FFFFFFFE);


type
  PDXVACOPPStatusHDCPKeyData = ^_DXVA_COPPStatusHDCPKeyData;
  _DXVA_COPPStatusHDCPKeyData = record
    rApp: TGUID;
    dwFlags: ULONG;                  // See COPP_StatusFlags above
    dwHDCPFlags: ULONG;              // See COPP_StatusHDCPFlags above
    BKey: TGUID;                     // Lower 40 bits
    Reserved1: TGUID;
    Reserved2: TGUID;
  end;
  {$EXTERNALSYM _DXVA_COPPStatusHDCPKeyData}
  DXVA_COPPStatusHDCPKeyData = _DXVA_COPPStatusHDCPKeyData;
  {$EXTERNALSYM DXVA_COPPStatusHDCPKeyData}

const

  DXVA_COPPQueryStatusFnCode          = $05;
  {$EXTERNALSYM DXVA_COPPQueryStatusFnCode}
  // lpInputData => DXVA_COPPStatusInput*
  // lpOuputData => DXVA_COPPStatusOutput*

  //
  // Status GUID and enumerations
  //
  DXVA_COPPQueryConnectorType : TGUID = '{81d0bfd5-6afe-48c2-99c0-95a08f97c5da}';

type
  PCOPPConnectorType = ^_COPP_ConnectorType;
  _COPP_ConnectorType = DWord;
  {$EXTERNALSYM COPP_ConnectorType}
  COPP_ConnectorType = _COPP_ConnectorType;
  {$EXTERNALSYM _COPP_ConnectorType}
const
  COPP_ConnectorType_Unknown        = COPP_ConnectorType(-1);
  COPP_ConnectorType_VGA            = COPP_ConnectorType(0);
  COPP_ConnectorType_SVideo         = COPP_ConnectorType(1);
  COPP_ConnectorType_CompositeVideo = COPP_ConnectorType(2);
  COPP_ConnectorType_ComponentVideo = COPP_ConnectorType(3);
  COPP_ConnectorType_DVI            = COPP_ConnectorType(4);
  COPP_ConnectorType_HDMI           = COPP_ConnectorType(5);
  COPP_ConnectorType_LVDS           = COPP_ConnectorType(6);
  COPP_ConnectorType_TMDS           = COPP_ConnectorType(7);
  COPP_ConnectorType_D_JPN          = COPP_ConnectorType(8);
  COPP_ConnectorType_Internal       = COPP_ConnectorType($80000000);  // can be combined with the other connector types
  COPP_ConnectorType_ForceDWORD     = COPP_ConnectorType($7FFFFFFF);   { force 32-bit size enum }


const
  DXVA_COPPQueryProtectionType        : TGUID = '{38f2a801-9a6c-48bb-9107-b6696e6f1797}';
  DXVA_COPPQueryLocalProtectionLevel  : TGUID = '{b2075857-3eda-4d5d-88db-748f8c1a0549}';
  DXVA_COPPQueryGlobalProtectionLevel : TGUID = '{1957210a-7766-452a-b99a-d27aed54f03a}';
  DXVA_COPPQueryDisplayData           : TGUID = '{d7bf1ba3-ad13-4f8e-af98-0dcb3ca204cc}';
  DXVA_COPPQueryHDCPKeyData           : TGUID = '{0db59d74-a992-492e-a0bd-c23fda564e00}';
  DXVA_COPPQueryBusData               : TGUID = '{c6f4d673-6174-4184-8e35-f6db5200bcba}';

type

  PCOPPBusType = ^_COPP_BusType;
  _COPP_BusType = DWord;
  {$EXTERNALSYM _COPP_BusType}
  COPP_BusType = _COPP_BusType;
  {$EXTERNALSYM COPP_BusType}
const
  COPP_BusType_Unknown    = COPP_BusType(0);
  COPP_BusType_PCI        = COPP_BusType(1);
  COPP_BusType_PCIX       = COPP_BusType(2);
  COPP_BusType_PCIExpress = COPP_BusType(3);
  COPP_BusType_AGP        = COPP_BusType(4);
  COPP_BusType_Integrated = COPP_BusType($80000000);  // can be combined with the other bus types
  //COPP_BusType_ForceDWORD = $7FFFFFFF;   { force 32-bit size enum }


const
  DXVA_COPPQuerySignaling            : TGUID = '{6629a591-3b79-4cf3-924a-11e8e7811671}';

type

  PDXVACOPPStatusSignalingCmdData = ^_DXVA_COPPStatusSignalingCmdData;
  _DXVA_COPPStatusSignalingCmdData = record
    rApp: TGUID;
    dwFlags: ULONG;                          // See COPP_StatusFlags above
    AvailableTVProtectionStandards: ULONG;   // See COPP_TVProtectionStandard
    ActiveTVProtectionStandard: ULONG;       // See COPP_TVProtectionStandard
    TVType: ULONG;
    AspectRatioValidMask1: ULONG;
    AspectRatioData1: ULONG;                 // See COPP_AspectRatio_EN300294 for ETSI EN 300 294 values
    AspectRatioValidMask2: ULONG;
    AspectRatioData2: ULONG;
    AspectRatioValidMask3: ULONG;
    AspectRatioData3: ULONG;
    ExtendedInfoValidMask: array[0..3] of ULONG;
    ExtendedInfoData: array[0..3] of ULONG;
  end;
  {$EXTERNALSYM _DXVA_COPPStatusSignalingCmdData}
  DXVA_COPPStatusSignalingCmdData = _DXVA_COPPStatusSignalingCmdData;
  {$EXTERNALSYM DXVA_COPPStatusSignalingCmdData}

{$ENDIF} //* __DIRECTX_VA_CERTOUTPUTPROTECT__ *//

  // Additional Prototypes for ALL interfaces

{$IFNDEF MFP_LONGLONG}
  PULONGLONG = ^ULONGLONG;
  PLONGLONG = ^LONGLONG;
  LONGLONG = Int64;
  {$IF COMPILERVERSION >= 11.0}
    {$IFDEF WIN64}
      ULONGLONG = UInt64;
      {$EXTERNALSYM ULONGLONG}
    {$ENDIF}
    {$IFDEF WIN32}
      ULONGLONG = Int64;
      {$EXTERNALSYM ULONGLONG}
    {$ENDIF}
  {$ELSE}
    {$IFDEF WIN64}
      ULONGLONG = Int64;
      {$EXTERNALSYM ULONGLONG}
   {$ENDIF}
    {$IFDEF WIN32}
      ULONGLONG = Int64;
      {$EXTERNALSYM ULONGLONG}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// End of Additional Prototypes

implementation


function DXVABit(__x: DWord): DWord;
begin
  Result := 1 shl __x;
end;


function DXVA_ExtractSampleFormat(_sf: DWORD): DWORD;
begin
  Result := _sf and ord(DXVA_SampleFormatMask);
end;


function DXVA_ExtractExtColorData(_sf: DWORD; _Mask: DWORD; _Shift: DWORD): DWORD;
begin
  Result := (_sf and _Mask) shr _Shift;
end;


function DXVABitMask(__n: DWORD): DWORD;
begin
  Result := ($FFFFFFFF shl __n) xor $FFFFFFFF;
end;


function DXVAColorMask(__bits: DWORD; __base: DWORD): DWORD;
begin
  Result := (DXVABitMask(__bits) shl (__base));
end;



/////// DXVA2_ExtendedFormat ///////////////////////////////////////////////////

  function DXVA_ExtendedFormat.ReadBits(const iIndex: Integer): Integer;
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;

  begin
    NrBits := iIndex and $FF;
    Offset := iIndex shr 8;
    Mask := ((1 shl NrBits) - 1);
    Result := (Value shr Offset) and Mask;
  end;

  procedure DXVA_ExtendedFormat.WriteBits(const iIndex: Integer; const iValue: Integer);
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;

  begin
    NrBits := iIndex and $FF;
    Offset := iIndex shr 8;
    Mask := ((1 shl NrBits) - 1);
{$IF DEBUG}
    Assert(Value <= Mask);
{$ENDIF}
    Value := (iValue and (not (Mask shl Offset))) or (iValue shl Offset);
  end;

// END DXVA2_ExtendedFormat ////////////////////////////////////////////////////

// Implement Additional functions here.

end.

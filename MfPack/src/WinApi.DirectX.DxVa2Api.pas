// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXVA2Api.pas
// Kind: Pascal / Delphi unit
// Release date: 20-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Public Interfaces for DXVA2.
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
// Source: dxva2api.h
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
unit WinApi.DirectX.DXVA2Api;

  {$HPPEMIT '#include "dxva2api.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  {Use rtl, Clootie Dx or MfPack}
  WinApi.DirectX.D3D9,
  WinApi.DirectX.DXVA,
  WinApi.DirectX.DXVAHd,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const
  {$EXTERNALSYM DxVa2ApiLib}
  DxVa2ApiLib  = 'Dxva2.dll';

  GUID_NULL  : TGUID = '{00000000-0000-0000-0000-000000000000}';
  {$EXTERNALSYM GUID_NULL}

type

  // pointers to Direct3D9 interfaces
  LPDIRECT3DDEVICE9 = ^IDirect3DDevice9;
  PDIRECT3DDEVICE9 = ^IDirect3DDevice9;
  PIDirect3DDevice9 = ^IDirect3DDevice9;

  // IDirect3DSurface9 = DWORD;
  PIDirect3DSurface9 = ^IDirect3DSurface9;

  PD3DFORMAT = ^D3DFORMAT;
  D3DFORMAT = DWORD;
  {$EXTERNALSYM D3DFORMAT}

  PD3DPOOL = ^D3DPOOL;
  D3DPOOL = DWORD;
  {$EXTERNALSYM D3DPOOL}


const

  // Description:
  //
  // Decode guids potentially supported by the underlying device hardware.
  //
  DXVA2_ModeMPEG2_MoComp  : TGUID = '{e6a9f44b-61b0-4563-9ea4-63d2a3c6fe66}';
  {$EXTERNALSYM DXVA2_ModeMPEG2_MoComp}
  DXVA2_ModeMPEG2_IDCT    : TGUID = '{bf22ad00-03ea-4690-8077-473346209b7e}';
  {$EXTERNALSYM DXVA2_ModeMPEG2_IDCT}
  DXVA2_ModeMPEG2_VLD     : TGUID = '{ee27417f-5e28-4e65-beea-1d26b508adc9}';
  {$EXTERNALSYM DXVA2_ModeMPEG2_VLD}
  DXVA2_ModeMPEG1_VLD     : TGUID = '{6f3ec719-3735-42cc-8063-65cc3cb36616}';
  {$EXTERNALSYM DXVA2_ModeMPEG1_VLD}
  DXVA2_ModeMPEG2and1_VLD : TGUID = '{86695f12-340e-4f04-9fd3-9253dd327460}';
  {$EXTERNALSYM DXVA2_ModeMPEG2and1_VLD}

  DXVA2_ModeH264_A        : TGUID = '{1b81be64-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeH264_A}
  DXVA2_ModeH264_B        : TGUID = '{1b81be65-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeH264_B}
  DXVA2_ModeH264_C        : TGUID = '{1b81be66-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeH264_C}
  DXVA2_ModeH264_D        : TGUID = '{1b81be67-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeH264_D}
  DXVA2_ModeH264_E        : TGUID = '{1b81be68-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeH264_E}
  DXVA2_ModeH264_F        : TGUID = '{1b81be69-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeH264_F}
  DXVA2_ModeH264_VLD_WithFMOASO_NoFGT         : TGUID = '{d5f04ff9-3418-45d8-9561-32a76aae2ddd}';
  {$EXTERNALSYM DXVA2_ModeH264_VLD_WithFMOASO_NoFGT}

  DXVA2_ModeH264_VLD_Stereo_Progressive_NoFGT : TGUID = '{d79be8da-0cf1-4c81-b82a-69a4e236f43d}';
  {$EXTERNALSYM DXVA2_ModeH264_VLD_Stereo_Progressive_NoFGT}
  DXVA2_ModeH264_VLD_Stereo_NoFGT             : TGUID = '{f9aaccbb-c2b6-4cfc-8779-5707b1760552}';
  {$EXTERNALSYM DXVA2_ModeH264_VLD_Stereo_NoFGT}
  DXVA2_ModeH264_VLD_Multiview_NoFGT          : TGUID = '{705b9d82-76cf-49d6-b7e6-ac8872db013c}';
  {$EXTERNALSYM DXVA2_ModeH264_VLD_Multiview_NoFGT}

  DXVA2_ModeWMV8_A      : TGUID = '{1b81be80-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeWMV8_A}
  DXVA2_ModeWMV8_B      : TGUID = '{1b81be81-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeWMV8_B}

  DXVA2_ModeWMV9_A      : TGUID = '{1b81be90-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeWMV9_A}
  DXVA2_ModeWMV9_B      : TGUID = '{1b81be91-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeWMV9_B}
  DXVA2_ModeWMV9_C      : TGUID = '{1b81be94-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeWMV9_C}

  DXVA2_ModeVC1_A       : TGUID = '{1b81beA0-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeVC1_A}
  DXVA2_ModeVC1_B       : TGUID = '{1b81beA1-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeVC1_B}
  DXVA2_ModeVC1_C       : TGUID = '{1b81beA2-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeVC1_C}
  DXVA2_ModeVC1_D       : TGUID = '{1b81beA3-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeVC1_D}
  DXVA2_ModeVC1_D2010   : TGUID = '{1b81beA4-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_ModeVC1_D2010}

  DXVA2_NoEncrypt       : TGUID = '{1b81beD0-a0c7-11d3-b984-00c04f2e73c5}';
  {$EXTERNALSYM DXVA2_NoEncrypt}

  DXVA2_VideoProcProgressiveDevice  : TGUID = '{5a54a0c9-c7ec-4bd9-8ede-f3c75dc4393b}';
  {$EXTERNALSYM DXVA2_VideoProcProgressiveDevice}
  DXVA2_VideoProcBobDevice          : TGUID = '{335aa36e-7884-43a4-9c91-7f87faf3e37e}';
  {$EXTERNALSYM DXVA2_VideoProcBobDevice}
  DXVA2_VideoProcSoftwareDevice     : TGUID = '{4553d47f-ee7e-4e3f-9475-dbf1376c4810}';
  {$EXTERNALSYM DXVA2_VideoProcSoftwareDevice}

  DXVA2_ModeMPEG4pt2_VLD_Simple         : TGUID = '{efd64d74-c9e8-41d7-a5e9-e9b0e39fa319}';
  {$EXTERNALSYM DXVA2_ModeMPEG4pt2_VLD_Simple}
  DXVA2_ModeMPEG4pt2_VLD_AdvSimple_NoGMC: TGUID = '{ed418a9f-010d-4eda-9ae3-9a65358d8d2e}';
  {$EXTERNALSYM DXVA2_ModeMPEG4pt2_VLD_AdvSimple_NoGMC}
  DXVA2_ModeMPEG4pt2_VLD_AdvSimple_GMC  : TGUID = '{ab998b5b-4258-44a9-9feb-94e597a6baae}';
  {$EXTERNALSYM DXVA2_ModeMPEG4pt2_VLD_AdvSimple_GMC}

  DXVA2_ModeHEVC_VLD_Main   : TGUID = '{5b11d51b-2f4c-4452-bcc3-09f2a1160cc0}';
  {$EXTERNALSYM DXVA2_ModeHEVC_VLD_Main}
  DXVA2_ModeHEVC_VLD_Main10 : TGUID = '{107af0e0-ef1a-4d19-aba8-67a163073d13}';
  {$EXTERNALSYM DXVA2_ModeHEVC_VLD_Main10}

  DXVA2_ModeVP9_VLD_Profile0       : TGUID = '{463707f8-a1d0-4585-876d-83aa6d60b89e}';
  {$EXTERNALSYM DXVA2_ModeVP9_VLD_Profile0}
  DXVA2_ModeVP9_VLD_10bit_Profile2 : TGUID = '{a4c749ef-6ecf-48aa-8448-50a7a1165ff7}';
  {$EXTERNALSYM DXVA2_ModeVP9_VLD_10bit_Profile2}
  DXVA2_ModeVP8_VLD                : TGUID = '{90b899ea-3a62-4705-88b3-8df04b2744e7}';
  {$EXTERNALSYM DXVA2_ModeVP8_VLD}



  //Re-defined
  //to let it work in all versions

  // Delphi is not case sensitive like C/C++, so we skip this one.
  // DXVA2_ModeMPEG2_MOCOMP            : TGUID = '{e6a9f44b-61b0-4563-9ea4-63d2a3c6fe66}'; // = DXVA2_ModeMPEG2_MoComp

  DXVA2_ModeWMV8_PostProc              : TGUID = '{1b81be80-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeWMV8_A
  {$EXTERNALSYM DXVA2_ModeWMV8_PostProc}
  DXVA2_ModeWMV8_MoComp                : TGUID = '{1b81be81-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeWMV8_B
  {$EXTERNALSYM DXVA2_ModeWMV8_MoComp}

  DXVA2_ModeWMV9_PostProc              : TGUID = '{1b81be90-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeWMV9_A
  {$EXTERNALSYM DXVA2_ModeWMV9_PostProc}
  DXVA2_ModeWMV9_MoComp                : TGUID = '{1b81be91-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeWMV9_B
  {$EXTERNALSYM DXVA2_ModeWMV9_MoComp}
  DXVA2_ModeWMV9_IDCT                  : TGUID = '{1b81be94-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeWMV9_C
  {$EXTERNALSYM DXVA2_ModeWMV9_IDCT}

  DXVA2_ModeVC1_PostProc               : TGUID = '{1b81beA0-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeVC1_A
  {$EXTERNALSYM DXVA2_ModeVC1_PostProc}
  DXVA2_ModeVC1_MoComp                 : TGUID = '{1b81beA1-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeVC1_B
  {$EXTERNALSYM DXVA2_ModeVC1_MoComp}
  DXVA2_ModeVC1_IDCT                   : TGUID = '{1b81beA2-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeVC1_C;
  {$EXTERNALSYM DXVA2_ModeVC1_IDCT}
  DXVA2_ModeVC1_VLD                    : TGUID = '{1b81beA3-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeVC1_D
  {$EXTERNALSYM DXVA2_ModeVC1_VLD}

  DXVA2_ModeH264_MoComp_NoFGT          : TGUID = '{1b81be64-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeH264_A
  {$EXTERNALSYM DXVA2_ModeH264_MoComp_NoFGT}
  DXVA2_ModeH264_MoComp_FGT            : TGUID = '{1b81be65-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeH264_B
  {$EXTERNALSYM DXVA2_ModeH264_MoComp_FGT}
  DXVA2_ModeH264_IDCT_NoFGT            : TGUID = '{1b81be66-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeH264_C
  {$EXTERNALSYM DXVA2_ModeH264_IDCT_NoFGT}
  DXVA2_ModeH264_IDCT_FGT              : TGUID = '{1b81be67-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeH264_D
  {$EXTERNALSYM DXVA2_ModeH264_IDCT_FGT}
  DXVA2_ModeH264_VLD_NoFGT             : TGUID = '{1b81be68-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeH264_E
  {$EXTERNALSYM DXVA2_ModeH264_VLD_NoFGT}
  DXVA2_ModeH264_VLD_FGT               : TGUID = '{1b81be69-a0c7-11d3-b984-00c04f2e73c5}'; // = DXVA2_ModeH264_F
  {$EXTERNALSYM DXVA2_ModeH264_VLD_FGT}


  // Description:
  //
  //  COM objects.
  //
  //  IID_IDirect3DDeviceManager9
  //
  //  IID_IDirectXVideoAccelerationService
  //  IID_IDirectXVideoDecoderService
  //  IID_IDirectXVideoProcessorService
  //
  //  IID_IDirectXVideoDecoder
  //  IID_IDirectXVideoProcessor
  //  IID_IDirectXVideoMemoryConfiguration
  //
  //  See interface declarations for the specific IID's.


  // Description:
  //
  //  DXVA2-specific error codes.
  //
  DXVA2_E_NOT_INITIALIZED             = HRESULT($80041000);
  {$EXTERNALSYM DXVA2_E_NOT_INITIALIZED}
  DXVA2_E_NEW_VIDEO_DEVICE            = HRESULT($80041001);
  {$EXTERNALSYM DXVA2_E_NEW_VIDEO_DEVICE}
  DXVA2_E_VIDEO_DEVICE_LOCKED         = HRESULT($80041002);
  {$EXTERNALSYM DXVA2_E_VIDEO_DEVICE_LOCKED}
  DXVA2_E_NOT_AVAILABLE               = HRESULT($80041003);
  {$EXTERNALSYM DXVA2_E_NOT_AVAILABLE}


  // Description:
  //
  //  Structures and enums used by the DXVA2 Video Processor API.
  //

  MAX_DEINTERLACE_SURFACES            = 32;
  {$EXTERNALSYM MAX_DEINTERLACE_SURFACES}
  MAX_SUBSTREAMS                      = 15;
  {$EXTERNALSYM MAX_SUBSTREAMS}



type

//typedef struct _DXVA2_ExtendedFormat
//{
//    union {
//        struct {
//            UINT SampleFormat : 8;           // See DXVA2_SampleFormat
//            UINT VideoChromaSubsampling : 4; // See DXVA2_VideoChromaSubSampling
//            UINT NominalRange : 3;           // See DXVA2_NominalRange
//            UINT VideoTransferMatrix : 3;    // See DXVA2_VideoTransferMatrix
//            UINT VideoLighting : 4;          // See DXVA2_VideoLighting
//            UINT VideoPrimaries : 5;         // See DXVA2_VideoPrimaries
//            UINT VideoTransferFunction : 5;  // See DXVA2_VideoTransferFunction
//        };
//        UINT value;
//    };
//} DXVA2_ExtendedFormat;

  PDXVA2_ExtendedFormat = ^DXVA2_ExtendedFormat;
  DXVA2_ExtendedFormat = record
  public
    Value: Integer; // Same as 'flags' used to acces all bitvalues
  private
    function ReadBits(const aIndex: Integer): Integer;
    procedure WriteBits(const aIndex: Integer; const aValue: Integer);
  public
    property SampleFormat: Integer index $0008 read ReadBits write WriteBits;            // 8 bits at offset 0
    property VideoChromaSubsampling: Integer index $0804 read ReadBits write WriteBits;  // 4 bits at offset 8
    property NominalRange: Integer index $0c03 read ReadBits write WriteBits;            // 3 bits at offset 12
    property VideoTransferMatrix: Integer index $0f03 read ReadBits write WriteBits;     // 3 bits at offset 15
    property VideoLighting: Integer index $1204 read ReadBits write WriteBits;           // 4 bits at offset 18
    property VideoPrimaries: Integer index $1605 read ReadBits write WriteBits;          // 5 bits at offset 22
    property VideoTransferFunction: Integer index $1b01 read ReadBits write WriteBits;   // 5 bits at offset 27
  end;
  {$EXTERNALSYM DXVA2_ExtendedFormat}

  // Members
  // =======
  // property SampleFormat
  //    Describes the interlacing of the video frames. Contains a value from the DXVA2_SampleFormat enumeration.
  // property VideoChromaSubsampling
  //    Describes the chroma siting. Contains a value from the DXVA2_VideoChromaSubSampling enumeration.
  // property NominalRange
  //    Describes the nominal range of the Y'CbCr or RGB color data. Contains a value from the DXVA2_NominalRange enumeration.
  // property VideoTransferMatrix
  //    Describes the transform from Y'PbPr (component video) to studio R'G'B'. Contains a value from the DXVA2_VideoTransferMatrix enumeration.
  // property VideoLighting
  //    Describes the intended viewing conditions. Contains a value from the DXVA2_VideoLighting enumeration.
  // property VideoPrimaries
  //    Describes the color primaries. Contains a value from the DXVA2_VideoPrimaries enumeration.
  // property VideoTransferFunction
  //    Describes the gamma correction transfer function. Contains a value from the DXVA2_VideoTransferFunction enumeration.
  // property value
  //    Use this member to access all of the bits in the union.


type
  PDXVA2_SampleFormat = ^_DXVA2_SampleFormat;
  _DXVA2_SampleFormat                     = (
    DXVA2_SampleFormatMask                = $FF, // 8 bits used for DXVA Sample format
    DXVA2_SampleUnknown                   = 0,
    DXVA2_SampleProgressiveFrame          = 2,
    DXVA2_SampleFieldInterleavedEvenFirst = 3,
    DXVA2_SampleFieldInterleavedOddFirst  = 4,
    DXVA2_SampleFieldSingleEven           = 5,
    DXVA2_SampleFieldSingleOdd            = 6,
    DXVA2_SampleSubStream                 = 7
  );
  {$EXTERNALSYM _DXVA2_SampleFormat}
  DXVA2_SampleFormat = _DXVA2_SampleFormat;
  {$EXTERNALSYM DXVA2_SampleFormat}


  _DXVA2_VideoChromaSubSampling = (
    DXVA2_VideoChromaSubsamplingMask                                   = $f,
    DXVA2_VideoChromaSubsampling_Unknown                               = $0,

    // base values
    DXVA2_VideoChromaSubsampling_ProgressiveChroma                     = $8,
    DXVA2_VideoChromaSubsampling_Horizontally_Cosited                  = $4,
    DXVA2_VideoChromaSubsampling_Vertically_Cosited                    = $2,
    DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes        = $1,

    // 4:2:0 variations
    // Make Ordinary to avoid combining signed and unsigned types
    DXVA2_VideoChromaSubsampling_MPEG2   = Ord(DXVA2_VideoChromaSubsampling_Horizontally_Cosited) or
                                           Ord(DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes),

    DXVA2_VideoChromaSubsampling_MPEG1   = DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes,

    DXVA2_VideoChromaSubsampling_DV_PAL  = Ord(DXVA2_VideoChromaSubsampling_Horizontally_Cosited) or
                                           Ord(DXVA2_VideoChromaSubsampling_Vertically_Cosited),

    // 4:4:4, 4:2:2, 4:1:1
    DXVA2_VideoChromaSubsampling_Cosited = Ord(DXVA2_VideoChromaSubsampling_Horizontally_Cosited) or
                                           Ord(DXVA2_VideoChromaSubsampling_Vertically_Cosited) or
                                           Ord(DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes)
  );
  {$EXTERNALSYM _DXVA2_VideoChromaSubSampling}
  DXVA2_VideoChromaSubSampling = _DXVA2_VideoChromaSubSampling;
  {$EXTERNALSYM DXVA2_VideoChromaSubSampling}


  PDXVA2_NominalRange = ^_DXVA2_NominalRange;
  _DXVA2_NominalRange          = (
    DXVA2_NominalRangeMask     = $7,
    DXVA2_NominalRange_Unknown = 0,
    DXVA2_NominalRange_Normal  = 1,
    DXVA2_NominalRange_Wide    = 2,
    DXVA2_NominalRange_0_255   = 1,
    DXVA2_NominalRange_16_235  = 2,
    DXVA2_NominalRange_48_208  = 3
  );
  {$EXTERNALSYM _DXVA2_NominalRange}
  DXVA2_NominalRange = _DXVA2_NominalRange;
  {$EXTERNALSYM DXVA2_NominalRange}

  PDXVA2_VideoTransferMatrix = ^_DXVA2_VideoTransferMatrix;
  _DXVA2_VideoTransferMatrix            = (
    DXVA2_VideoTransferMatrixMask       = $7,
    DXVA2_VideoTransferMatrix_Unknown   = 0,
    DXVA2_VideoTransferMatrix_BT709     = 1,
    DXVA2_VideoTransferMatrix_BT601     = 2,
    DXVA2_VideoTransferMatrix_SMPTE240M = 3
  );
  {$EXTERNALSYM _DXVA2_VideoTransferMatrix}
  DXVA2_VideoTransferMatrix = _DXVA2_VideoTransferMatrix;
  {$EXTERNALSYM DXVA2_VideoTransferMatrix}


  PDXVA2VideoLighting = ^DXVA2_VideoLighting;
  _DXVA2_VideoLighting          = (
    DXVA2_VideoLightingMask     = $F,
    DXVA2_VideoLighting_Unknown = 0,
    DXVA2_VideoLighting_bright  = 1,
    DXVA2_VideoLighting_office  = 2,
    DXVA2_VideoLighting_dim     = 3,
    DXVA2_VideoLighting_dark    = 4
  );
  {$EXTERNALSYM _DXVA2_VideoLighting}
  DXVA2_VideoLighting = _DXVA2_VideoLighting;
  {$EXTERNALSYM DXVA2_VideoLighting}

  PDXVA2_VideoPrimaries = ^_DXVA2_VideoPrimaries;
  _DXVA2_VideoPrimaries                = (
    DXVA2_VideoPrimariesMask           = $1F,
    DXVA2_VideoPrimaries_Unknown       = 0,
    DXVA2_VideoPrimaries_reserved      = 1,
    DXVA2_VideoPrimaries_BT709         = 2,
    DXVA2_VideoPrimaries_BT470_2_SysM  = 3,
    DXVA2_VideoPrimaries_BT470_2_SysBG = 4,
    DXVA2_VideoPrimaries_SMPTE170M     = 5,
    DXVA2_VideoPrimaries_SMPTE240M     = 6,
    DXVA2_VideoPrimaries_EBU3213       = 7,
    DXVA2_VideoPrimaries_SMPTE_C       = 8
  );
  {$EXTERNALSYM _DXVA2_VideoPrimaries}
  DXVA2_VideoPrimaries = _DXVA2_VideoPrimaries;
  {$EXTERNALSYM DXVA2_VideoPrimaries}


  PDXVA2_VideoTransferFunction = ^_DXVA2_VideoTransferFunction;
  _DXVA2_VideoTransferFunction   = (
    DXVA2_VideoTransFuncMask     = $1F,
    DXVA2_VideoTransFunc_Unknown = 0,
    DXVA2_VideoTransFunc_10      = 1,
    DXVA2_VideoTransFunc_18      = 2,
    DXVA2_VideoTransFunc_20      = 3,
    DXVA2_VideoTransFunc_22      = 4,
    DXVA2_VideoTransFunc_709     = 5,
    DXVA2_VideoTransFunc_240M    = 6,
    DXVA2_VideoTransFunc_sRGB    = 7,
    DXVA2_VideoTransFunc_28      = 8
  );
  {$EXTERNALSYM _DXVA2_VideoTransferFunction}
  DXVA2_VideoTransferFunction = _DXVA2_VideoTransferFunction;
  {$EXTERNALSYM DXVA2_VideoTransferFunction}


  //////////////////////////////////////////////////////////////////////////////////////
  // Deprecated labels - please use the ones in the DXVA2_VideoTransferFunction enum. //
  //////////////////////////////////////////////////////////////////////////////////////

const

  DXVA2_VideoTransFunc_22_709         = DXVA2_VideoTransFunc_709;
  {$EXTERNALSYM DXVA2_VideoTransFunc_22_709}
  DXVA2_VideoTransFunc_22_240M        = DXVA2_VideoTransFunc_240M;
  {$EXTERNALSYM DXVA2_VideoTransFunc_22_240M}
  DXVA2_VideoTransFunc_22_8bit_sRGB   = DXVA2_VideoTransFunc_sRGB;
  {$EXTERNALSYM DXVA2_VideoTransFunc_22_8bit_sRGB}

  //////////////////////////////////////////////////////////////////////////////////////


type

  PDXVA2_Frequency = ^_DXVA2_Frequency;
  _DXVA2_Frequency = record
    Numerator: UINT;
    Denominator: UINT;
  end;
  {$EXTERNALSYM _DXVA2_Frequency}
  DXVA2_Frequency = _DXVA2_Frequency;
  {$EXTERNALSYM DXVA2_Frequency}


  PDXVA2VideoDesc = ^_DXVA2_VideoDesc;
  _DXVA2_VideoDesc = record
    SampleWidth: UINT;
    SampleHeight: UINT;
    SampleFormat: DXVA2_ExtendedFormat;
    Format: D3DFORMAT;
    InputSampleFreq: DXVA2_Frequency;
    OutputFrameFreq: DXVA2_Frequency;
    UABProtectionLevel: UINT;
    Reserved: UINT;
  end;
  {$EXTERNALSYM _DXVA2_VideoDesc}
  DXVA2_VideoDesc = _DXVA2_VideoDesc;
  {$EXTERNALSYM DXVA2_VideoDesc}
  TDXVA2VideoDesc = DXVA2_VideoDesc;
  {$EXTERNALSYM TDXVA2VideoDesc}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0003
  DXVA2_DeinterlaceTech_Unknown                = UINT(0);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_Unknown}
  DXVA2_DeinterlaceTech_BOBLineReplicate       = UINT($1);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_BOBLineReplicate}
  DXVA2_DeinterlaceTech_BOBVerticalStretch     = UINT($2);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_BOBVerticalStretch}
  DXVA2_DeinterlaceTech_BOBVerticalStretch4Tap = UINT($4);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_BOBVerticalStretch4Tap}
  DXVA2_DeinterlaceTech_MedianFiltering        = UINT($8);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_MedianFiltering}
  DXVA2_DeinterlaceTech_EdgeFiltering          = UINT($10);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_EdgeFiltering}
  DXVA2_DeinterlaceTech_FieldAdaptive          = UINT($20);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_FieldAdaptive}
  DXVA2_DeinterlaceTech_PixelAdaptive          = UINT($40);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_PixelAdaptive}
  DXVA2_DeinterlaceTech_MotionVectorSteered    = UINT($80);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_MotionVectorSteered}
  DXVA2_DeinterlaceTech_InverseTelecine        = UINT($100);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_InverseTelecine}
  DXVA2_DeinterlaceTech_Mask                   = UINT($1FF);
  {$EXTERNALSYM DXVA2_DeinterlaceTech_Mask}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0004
  DXVA2_NoiseFilterLumaLevel              = UINT(1);
  {$EXTERNALSYM DXVA2_NoiseFilterLumaLevel}
  DXVA2_NoiseFilterLumaThreshold          = UINT(2);
  {$EXTERNALSYM DXVA2_NoiseFilterLumaThreshold}
  DXVA2_NoiseFilterLumaRadius             = UINT(3);
  {$EXTERNALSYM DXVA2_NoiseFilterLumaRadius}
  DXVA2_NoiseFilterChromaLevel            = UINT(4);
  {$EXTERNALSYM DXVA2_NoiseFilterChromaLevel}
  DXVA2_NoiseFilterChromaThreshold        = UINT(5);
  {$EXTERNALSYM DXVA2_NoiseFilterChromaThreshold}
  DXVA2_NoiseFilterChromaRadius           = UINT(6);
  {$EXTERNALSYM DXVA2_NoiseFilterChromaRadius}
  DXVA2_DetailFilterLumaLevel             = UINT(7);
  {$EXTERNALSYM DXVA2_DetailFilterLumaLevel}
  DXVA2_DetailFilterLumaThreshold         = UINT(8);
  {$EXTERNALSYM DXVA2_DetailFilterLumaThreshold}
  DXVA2_DetailFilterLumaRadius            = UINT(9);
  {$EXTERNALSYM DXVA2_DetailFilterLumaRadius}
  DXVA2_DetailFilterChromaLevel           = UINT(10);
  {$EXTERNALSYM DXVA2_DetailFilterChromaLevel}
  DXVA2_DetailFilterChromaThreshold       = UINT(11);
  {$EXTERNALSYM DXVA2_DetailFilterChromaThreshold}
  DXVA2_DetailFilterChromaRadius          = UINT(12);
  {$EXTERNALSYM DXVA2_DetailFilterChromaRadius}


const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0005
  DXVA2_NoiseFilterTech_Unsupported       = UINT(0);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_Unsupported}
  DXVA2_NoiseFilterTech_Unknown           = UINT($1);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_Unknown}
  DXVA2_NoiseFilterTech_Median            = UINT($2);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_Median}
  DXVA2_NoiseFilterTech_Temporal          = UINT($4);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_Temporal}
  DXVA2_NoiseFilterTech_BlockNoise        = UINT($8);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_BlockNoise}
  DXVA2_NoiseFilterTech_MosquitoNoise     = UINT($10);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_MosquitoNoise}
  DXVA2_NoiseFilterTech_Mask              = UINT($1F);
  {$EXTERNALSYM DXVA2_NoiseFilterTech_Mask}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0006
  DXVA2_DetailFilterTech_Unsupported      = UINT(0);
  DXVA2_DetailFilterTech_Unknown          = UINT($1);
  DXVA2_DetailFilterTech_Edge             = UINT($2);
  DXVA2_DetailFilterTech_Sharpening       = UINT($4);
  DXVA2_DetailFilterTech_Mask             = UINT($7);

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0007
  DXVA2_ProcAmp_None                      = UINT(0);
  {$EXTERNALSYM DXVA2_ProcAmp_None}
  DXVA2_ProcAmp_Brightness                = UINT($1);
  {$EXTERNALSYM DXVA2_ProcAmp_Brightness}
  DXVA2_ProcAmp_Contrast                  = UINT($2);
  {$EXTERNALSYM DXVA2_ProcAmp_Contrast}
  DXVA2_ProcAmp_Hue                       = UINT($4);
  {$EXTERNALSYM DXVA2_ProcAmp_Hue}
  DXVA2_ProcAmp_Saturation                = UINT($8);
  {$EXTERNALSYM DXVA2_ProcAmp_Saturation}
  DXVA2_ProcAmp_Mask                      = UINT($F);
  {$EXTERNALSYM DXVA2_ProcAmp_Mask}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0008
  DXVA2_VideoProcess_None                       = UINT(0);
  {$EXTERNALSYM DXVA2_VideoProcess_None}
  DXVA2_VideoProcess_YUV2RGB                    = UINT($1);
  {$EXTERNALSYM DXVA2_VideoProcess_YUV2RGB}
  DXVA2_VideoProcess_StretchX                   = UINT($2);
  {$EXTERNALSYM DXVA2_VideoProcess_StretchX}
  DXVA2_VideoProcess_StretchY                   = UINT($4);
  {$EXTERNALSYM DXVA2_VideoProcess_StretchY}
  DXVA2_VideoProcess_AlphaBlend                 = UINT($8);
  {$EXTERNALSYM DXVA2_VideoProcess_AlphaBlend}
  DXVA2_VideoProcess_SubRects                   = UINT($10);
  {$EXTERNALSYM DXVA2_VideoProcess_SubRects}
  DXVA2_VideoProcess_SubStreams                 = UINT($20);
  {$EXTERNALSYM DXVA2_VideoProcess_SubStreams}
  DXVA2_VideoProcess_SubStreamsExtended         = UINT($40);
  {$EXTERNALSYM DXVA2_VideoProcess_SubStreamsExtended}
  DXVA2_VideoProcess_YUV2RGBExtended            = UINT($80);
  {$EXTERNALSYM DXVA2_VideoProcess_YUV2RGBExtended}
  DXVA2_VideoProcess_AlphaBlendExtended         = UINT($100);
  {$EXTERNALSYM DXVA2_VideoProcess_AlphaBlendExtended}
  DXVA2_VideoProcess_Constriction               = UINT($200);
  {$EXTERNALSYM DXVA2_VideoProcess_Constriction}
  DXVA2_VideoProcess_NoiseFilter                = UINT($400);
  {$EXTERNALSYM DXVA2_VideoProcess_NoiseFilter}
  DXVA2_VideoProcess_DetailFilter               = UINT($800);
  {$EXTERNALSYM DXVA2_VideoProcess_DetailFilter}
  DXVA2_VideoProcess_PlanarAlpha                = UINT($1000);
  {$EXTERNALSYM DXVA2_VideoProcess_PlanarAlpha}
  DXVA2_VideoProcess_LinearScaling              = UINT($2000);
  {$EXTERNALSYM DXVA2_VideoProcess_LinearScaling}
  DXVA2_VideoProcess_GammaCompensated           = UINT($4000);
  {$EXTERNALSYM DXVA2_VideoProcess_GammaCompensated}
  DXVA2_VideoProcess_MaintainsOriginalFieldData = UINT($8000);
  {$EXTERNALSYM DXVA2_VideoProcess_MaintainsOriginalFieldData}
  DXVA2_VideoProcess_Mask                       = UINT($FFFF);
  {$EXTERNALSYM DXVA2_VideoProcess_Mask}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0009
  DXVA2_VPDev_HardwareDevice              = UINT($1);
  {$EXTERNALSYM DXVA2_VPDev_HardwareDevice}
  DXVA2_VPDev_EmulatedDXVA1               = UINT($2);
  {$EXTERNALSYM DXVA2_VPDev_EmulatedDXVA1}
  DXVA2_VPDev_SoftwareDevice              = UINT($4);
  {$EXTERNALSYM DXVA2_VPDev_SoftwareDevice}
  DXVA2_VPDev_Mask                        = UINT($7);
  {$EXTERNALSYM DXVA2_VPDev_Mask}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0010
  DXVA2_SampleData_RFF                    = UINT($1);
  {$EXTERNALSYM DXVA2_SampleData_RFF}
  DXVA2_SampleData_TFF                    = UINT($2);
  {$EXTERNALSYM DXVA2_SampleData_TFF}
  DXVA2_SampleData_RFF_TFF_Present        = UINT($4);
  {$EXTERNALSYM DXVA2_SampleData_RFF_TFF_Present}
  DXVA2_SampleData_Mask                   = UINT($FFFF);
  {$EXTERNALSYM DXVA2_SampleData_Mask}

const
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0011
  DXVA2_DestData_RFF                      = UINT($1);
  {$EXTERNALSYM DXVA2_DestData_RFF}
  DXVA2_DestData_TFF                      = UINT($2);
  {$EXTERNALSYM DXVA2_DestData_TFF}
  DXVA2_DestData_RFF_TFF_Present          = UINT($4);
  {$EXTERNALSYM DXVA2_DestData_RFF_TFF_Present}
  DXVA2_DestData_Mask                     = UINT($FFFF);
  {$EXTERNALSYM DXVA2_DestData_Mask}

type

  PDXVA2_VideoProcessorCaps = ^_DXVA2_VideoProcessorCaps;
  _DXVA2_VideoProcessorCaps = record
    DeviceCaps: UINT;
    InputPool: D3DPOOL;
    NumForwardRefSamples: UINT;
    NumBackwardRefSamples: UINT;
    Reserved: UINT;
    DeinterlaceTechnology: UINT;
    ProcAmpControlCaps: UINT;
    VideoProcessorOperations: UINT;
    NoiseFilterTechnology: UINT;
    DetailFilterTechnology: UINT;
  end;
  {$EXTERNALSYM _DXVA2_VideoProcessorCaps}
  DXVA2_VideoProcessorCaps = _DXVA2_VideoProcessorCaps;
  {$EXTERNALSYM DXVA2_VideoProcessorCaps}


  __MIDL_PDXVA2Fixed32 = ^DXVA2_Fixed32;
  __MIDL_DXVA2_Fixed32 = LONG;
  {$EXTERNALSYM __MIDL_DXVA2_Fixed32}


  PDXVA2_Fixed32 = ^_DXVA2_Fixed32;
  _DXVA2_Fixed32 = record
    case integer of
      0: (Fraction: USHORT;
          Value: SHORT);
      1: (ll: LONG);
    end;
  {$EXTERNALSYM _DXVA2_Fixed32}
  DXVA2_Fixed32 = _DXVA2_Fixed32;
  {$EXTERNALSYM DXVA2_Fixed32}
  // Defines a 32-bit fixed-point number.
  // Members:
  //   Fraction     Fractional part of the number.
  //   Value        Integer part of the number.
  //   ll           Accesses the entire 32 bits of the number.
  //                You can use this member to compare DXVA2_Fixed32 values.
  // Remarks:
  // To convert between floating-point numbers and DXVA2_Fixed32 values,
  // use the DXVA2FixedToFloat and DXVA2FloatToFixed functions.

  PDXVA2_AYUVSample8 = ^_DXVA2_AYUVSample8;
  _DXVA2_AYUVSample8 = record
    Cr: UCHAR;   // V
    Cb: UCHAR;   // U
    Y: UCHAR;
    Alpha: UCHAR;
  end;
  {$EXTERNALSYM _DXVA2_AYUVSample8}
  DXVA2_AYUVSample8 = _DXVA2_AYUVSample8;
  {$EXTERNALSYM DXVA2_AYUVSample8}

  PDXVA2_AYUVSample16 = ^_DXVA2_AYUVSample16;
  _DXVA2_AYUVSample16 = record
    Cr: USHORT;
    Cb: USHORT;
    Y: USHORT;
    Alpha: USHORT;
  end;
  {$EXTERNALSYM _DXVA2_AYUVSample16}
  DXVA2_AYUVSample16 = _DXVA2_AYUVSample16;
  {$EXTERNALSYM DXVA2_AYUVSample16}

//  REFERENCE_TIME = LONGLONG;  //reintroduced, is also declared in MfpTypes

  PDXVA2VideoSample = ^_DXVA2_VideoSample;
  _DXVA2_VideoSample = record
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    SampleFormat: DXVA2_ExtendedFormat;
    SrcSurface: ^IDirect3DSurface9;
    SrcRect: TRect;
    DstRect: TRect;
    Pal: array[0..15] of DXVA2_AYUVSample8;  // Palette used with AI44 surface types
    PlanarAlpha: DXVA2_Fixed32;
    SampleData: DWORD;                       // Sample metadata
  end;
  {$EXTERNALSYM _DXVA2_VideoSample}
  DXVA2_VideoSample = _DXVA2_VideoSample;
  {$EXTERNALSYM DXVA2_VideoSample}

  PDXVA2_ValueRange = ^_DXVA2_ValueRange;
  _DXVA2_ValueRange = record
    MinValue: DXVA2_Fixed32;
    MaxValue: DXVA2_Fixed32;
    DefaultValue: DXVA2_Fixed32;
    StepSize: DXVA2_Fixed32;
  end;
  {$EXTERNALSYM _DXVA2_ValueRange}
  DXVA2_ValueRange = _DXVA2_ValueRange;
  {$EXTERNALSYM DXVA2_ValueRange}


  PDXVA2_ProcAmpValues = ^_DXVA2_ProcAmpValues;
  _DXVA2_ProcAmpValues = record
    Brightness: DXVA2_Fixed32;
    Contrast: DXVA2_Fixed32;
    Hue: DXVA2_Fixed32;
    Saturation: DXVA2_Fixed32;
  end;
  {$EXTERNALSYM _DXVA2_ProcAmpValues}
  DXVA2_ProcAmpValues = _DXVA2_ProcAmpValues;
  {$EXTERNALSYM DXVA2_ProcAmpValues}

  PDXVA2_FilterValues = ^_DXVA2_FilterValues;
  _DXVA2_FilterValues = record
    Level: DXVA2_Fixed32;
    Threshold: DXVA2_Fixed32;
    Radius: DXVA2_Fixed32;
  end;
  {$EXTERNALSYM _DXVA2_FilterValues}
  DXVA2_FilterValues = _DXVA2_FilterValues;
  {$EXTERNALSYM DXVA2_FilterValues}

  PDXVA2_VideoProcessBltParams = ^_DXVA2_VideoProcessBltParams;
  _DXVA2_VideoProcessBltParams = record
    TargetFrame: REFERENCE_TIME;
    TargetRect: TRect;
    ConstrictionSize: SIZE;
    StreamingFlags: UINT;
    BackgroundColor: DXVA2_AYUVSample16;
    DestFormat: DXVA2_ExtendedFormat;
    ProcAmpValues: DXVA2_ProcAmpValues;
    Alpha: DXVA2_Fixed32;
    NoiseFilterLuma: DXVA2_FilterValues;
    NoiseFilterChroma: DXVA2_FilterValues;
    DetailFilterLuma: DXVA2_FilterValues;
    DetailFilterChroma: DXVA2_FilterValues;
    DestData: DWORD;
  end;
  {$EXTERNALSYM _DXVA2_VideoProcessBltParams}
  DXVA2_VideoProcessBltParams = _DXVA2_VideoProcessBltParams;
  {$EXTERNALSYM DXVA2_VideoProcessBltParams}


  // Description:
  //
  //  Structures and enums used by the DXVA2 Video Decoding API.
  //

const
  // Compressed buffer types.
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0012
  DXVA2_PictureParametersBufferType         = UINT(0);
  {$EXTERNALSYM DXVA2_PictureParametersBufferType}
  DXVA2_MacroBlockControlBufferType         = UINT(1);
  {$EXTERNALSYM DXVA2_MacroBlockControlBufferType}
  DXVA2_ResidualDifferenceBufferType        = UINT(2);
  {$EXTERNALSYM DXVA2_ResidualDifferenceBufferType}
  DXVA2_DeblockingControlBufferType         = UINT(3);
  {$EXTERNALSYM DXVA2_DeblockingControlBufferType}
  DXVA2_InverseQuantizationMatrixBufferType = UINT(4);
  {$EXTERNALSYM DXVA2_InverseQuantizationMatrixBufferType}
  DXVA2_SliceControlBufferType              = UINT(5);
  {$EXTERNALSYM DXVA2_SliceControlBufferType}
  DXVA2_BitStreamDateBufferType             = UINT(6);
  {$EXTERNALSYM DXVA2_BitStreamDateBufferType}
  DXVA2_MotionVectorBuffer                  = UINT(7);
  {$EXTERNALSYM DXVA2_MotionVectorBuffer}
  DXVA2_FilmGrainBuffer                     = UINT(8);
  {$EXTERNALSYM DXVA2_FilmGrainBuffer}

const
  // Uncompressed buffer types
  // enum __MIDL___MIDL_itf_dxva2api_0000_0000_0013
  DXVA2_VideoDecoderRenderTarget          = UINT(0);
  {$EXTERNALSYM DXVA2_VideoDecoderRenderTarget}
  DXVA2_VideoProcessorRenderTarget        = UINT(1);
  {$EXTERNALSYM DXVA2_VideoProcessorRenderTarget}
  DXVA2_VideoSoftwareRenderTarget         = UINT(2);
  {$EXTERNALSYM DXVA2_VideoSoftwareRenderTarget}


type
   PDXVA2_ConfigPictureDecode = ^_DXVA2_ConfigPictureDecode;
  _DXVA2_ConfigPictureDecode = record
    guidConfigBitstreamEncryption: TGUID;
    guidConfigMBcontrolEncryption: TGUID;
    guidConfigResidDiffEncryption: TGUID;
    ConfigBitstreamRaw: UINT;
    ConfigMBcontrolRasterOrder: UINT;
    ConfigResidDiffHost: UINT;
    ConfigSpatialResid8: UINT;
    ConfigResid8Subtraction: UINT;
    ConfigSpatialHost8or9Clipping: UINT;
    ConfigSpatialResidInterleaved: UINT;
    ConfigIntraResidUnsigned: UINT;
    ConfigResidDiffAccelerator: UINT;
    ConfigHostInverseScan: UINT;
    ConfigSpecificIDCT: UINT;
    Config4GroupedCoefs: UINT;
    ConfigMinRenderTargetBuffCount: USHORT;
    ConfigDecoderSpecific: USHORT;
  end;
  {$EXTERNALSYM _DXVA2_ConfigPictureDecode}
  DXVA2_ConfigPictureDecode = _DXVA2_ConfigPictureDecode;
  {$EXTERNALSYM DXVA2_ConfigPictureDecode}
  TDXVA2_ConfigPictureDecode = DXVA2_ConfigPictureDecode;
  {$EXTERNALSYM TDXVA2_ConfigPictureDecode}

  PDXVA2_DecodeBufferDesc = ^_DXVA2_DecodeBufferDesc;
  _DXVA2_DecodeBufferDesc = record
    CompressedBufferType: DWORD;
    BufferIndex: UINT;             // reserved
    DataOffset: UINT;
    DataSize: UINT;
    FirstMBaddress: UINT;
    NumMBsInBuffer: UINT;
    Width: UINT;                   // reserved
    Height: UINT;                  // reserved
    Stride: UINT;                  // reserved
    ReservedBits: UINT;
    pvPVPState: Pvoid;
  end;
  {$EXTERNALSYM _DXVA2_DecodeBufferDesc}
  DXVA2_DecodeBufferDesc = _DXVA2_DecodeBufferDesc;
  {$EXTERNALSYM DXVA2_DecodeBufferDesc}

  //The value in pvPVPState depends on the type of crypo used. For
  //D3DCRYPTOTYPE_AES128_CTR, pvPState points to the following structure:
  PDXVA2_AES_CTR_IV = ^_DXVA2_AES_CTR_IV;
  _DXVA2_AES_CTR_IV = record
    IV: UINT64;       // Big-Endian IV
    Count: UINT64;    // Big-Endian Block Count
  end;
  {$EXTERNALSYM _DXVA2_AES_CTR_IV}
  DXVA2_AES_CTR_IV = _DXVA2_AES_CTR_IV;
  {$EXTERNALSYM DXVA2_AES_CTR_IV}

  PDXVA2_DecodeExtensionData = ^_DXVA2_DecodeExtensionData;
  _DXVA2_DecodeExtensionData = record
    Funktion: UINT;
    pPrivateInputData: Pvoid;
    PrivateInputDataSize: UINT;
    pPrivateOutputData: Pvoid;
    PrivateOutputDataSize: UINT;
  end;
  {$EXTERNALSYM _DXVA2_DecodeExtensionData}
  DXVA2_DecodeExtensionData = _DXVA2_DecodeExtensionData;
  {$EXTERNALSYM DXVA2_DecodeExtensionData}


// DXVA2_DECODE_GET_DRIVER_HANDLE is an extension function that allows the
// driver to return a handle for the DXVA2 decode device that can be used to
// associate it with a IDirect3DCryptoSession9 interface.  When this function
// is used:
//     pPrivateInputData = Nil
//     pPrivateInputDataSize = 0
//     pPrivateOutputData = ^HANDLE
//     pPrivateOutputDataSize = SizeOf(PHANDLE)
const

  DXVA2_DECODE_GET_DRIVER_HANDLE      = $725;
  {$EXTERNALSYM DXVA2_DECODE_GET_DRIVER_HANDLE}


// DXVA2_DECODE_SPECIFY_ENCRYPTED_BLOCKS is an extension function that that allows
// the decoder to specify which portions of the compressed buffers are encrypted.
// If this fucntion is not used to specify this information, it is assumed that
// the entire buffer is encrypted.
//     pPrivateInputData = ^D3DENCRYPTED_BLOCK_INFO;
//     PrivateInputDataSize = SizeOf(D3DENCRYPTED_BLOCK_INFO);
//     pPrivateOutputData = Nil;
//     PrivateOutputDataSize = 0;
  DXVA2_DECODE_SPECIFY_ENCRYPTED_BLOCKS= $724;
  {$EXTERNALSYM DXVA2_DECODE_SPECIFY_ENCRYPTED_BLOCKS}


type

  PDXVA2DecodeExecuteParams = ^_DXVA2_DecodeExecuteParams;
  _DXVA2_DecodeExecuteParams = record
    NumCompBuffers: UINT;
    pCompressedBuffers: ^DXVA2_DecodeBufferDesc;
    pExtensionData: ^DXVA2_DecodeExtensionData;
  end;
  {$EXTERNALSYM _DXVA2_DecodeExecuteParams}
  DXVA2_DecodeExecuteParams = _DXVA2_DecodeExecuteParams;
  {$EXTERNALSYM DXVA2_DecodeExecuteParams}


  // Description:
  //
  //  Public interfaces supported by the DXVA2 API.
  //
  PIDirect3DDeviceManager9 = ^IDirect3DDeviceManager9;
  IDirect3DDeviceManager9 = interface;

  PIDirectXVideoAccelerationService = ^IDirectXVideoAccelerationService;
  IDirectXVideoAccelerationService = interface;

  PIDirectXVideoDecoderService = ^IDirectXVideoDecoderService;
  IDirectXVideoDecoderService = interface;

  PIDirectXVideoProcessorService = ^IDirectXVideoProcessorService;
  IDirectXVideoProcessorService = interface;

  PIDirectXVideoDecoder   = ^IDirectXVideoDecoder;
  IDirectXVideoDecoder = interface;

  PIDirectXVideoProcessor = ^IDirectXVideoProcessor;
  IDirectXVideoProcessor = interface;

  PIDirectXVideoMemoryConfiguration = ^IDirectXVideoMemoryConfiguration;
  IDirectXVideoMemoryConfiguration = interface;



  // INTERFACES ////////////////////////////////////////////////////////////////


  // Interface IDirect3DDeviceManager9
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DDeviceManager9);'}
  {$EXTERNALSYM IDirect3DDeviceManager9}
  IDirect3DDeviceManager9 = interface(IUnknown)
  ['{a0cade0f-06d5-4cf4-a1c7-f3cdd725aa75}']
    function ResetDevice(const pDevice: IDirect3DDevice9;
                         const resetToken: UINT): HResult; stdcall;

    function OpenDeviceHandle(out phDevice: THANDLE): HResult; stdcall;

    function CloseDeviceHandle(const hDevice: THANDLE): HResult; stdcall;

    function TestDevice(const hDevice: THANDLE): HResult; stdcall;

    function LockDevice(const hDevice: THANDLE;
                        out ppDevice: PIDirect3DDevice9;
                        const fBlock: BOOL): HResult; stdcall;

    function UnlockDevice(const hDevice: THANDLE;
                          const fSaveState: BOOL): HResult; stdcall;

    function GetVideoService(const hDevice: THANDLE;
                             const riid: TGuid;
                             out ppService: Pvoid): HResult; stdcall;
  end;
  IID_IDirect3DDeviceManager9 = IDirect3DDeviceManager9;
  {$EXTERNALSYM IID_IDirect3DDeviceManager9}


  // Interface IDirectXVideoAccelerationService
  // ==========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectXVideoAccelerationService);'}
  {$EXTERNALSYM IDirectXVideoAccelerationService}
  IDirectXVideoAccelerationService  = interface(IUnknown)
  ['{fc51a550-d5e7-11d9-af55-00054e43ff02}']
    function CreateSurface(const Width: UINT;
                           const Height: UINT;
                           const BackBuffers: UINT;
                           const Format: D3DFORMAT;
                           const Pool: D3DPOOL;
                           const Usage: DWORD;
                           const DxvaType: DWORD;
                           out ppSurface: PIDirect3DSurface9;
                           var pSharedHandle: THANDLE): HResult; stdcall;

  end;
  IID_IDirectXVideoAccelerationService = IDirectXVideoAccelerationService;
  {$EXTERNALSYM IID_IDirectXVideoAccelerationService}



  // Interface IDirectXVideoDecoderService
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectXVideoDecoderService);'}
  {$EXTERNALSYM IDirectXVideoDecoderService}
  IDirectXVideoDecoderService = interface(IUnknown)
  ['{fc51a551-d5e7-11d9-af55-00054e43ff02}']
    function GetDecoderDeviceGuids(out pCount: UINT;
                                   out pGuids: PGUID): HResult; stdcall;

    function GetDecoderRenderTargets(const Guid: TGuid;
                                     out pCount: UINT;
                                     out pFormats: PD3DFORMAT): HResult; stdcall;

    function GetDecoderConfigurations(const Guid: TGuid;
                                      const pVideoDesc: DXVA2_VideoDesc;
                                      const pReserved: Pointer;
                                      out pCount: UINT;
                                      out ppConfigs: PDXVA2_ConfigPictureDecode): HResult; stdcall;

    function CreateVideoDecoder(const Guid: TGuid;
                                const pVideoDesc: DXVA2_VideoDesc;
                                const pConfig: DXVA2_ConfigPictureDecode;
                                const ppDecoderRenderTargets: PIDirect3DSurface9;
                                const NumRenderTargets: UINT;
                                out ppDecode: PIDirectXVideoDecoder): HResult; stdcall;
  end;
  IID_IDirectXVideoDecoderService = IDirectXVideoDecoderService;
  {$EXTERNALSYM IID_IDirectXVideoDecoderService}



  // Interface IDirectXVideoProcessorService
  // =======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectXVideoProcessorService);'}
  {$EXTERNALSYM IDirectXVideoProcessorService}
  IDirectXVideoProcessorService = interface(IDirectXVideoAccelerationService)
  ['{fc51a552-d5e7-11d9-af55-00054e43ff02}']

    function RegisterVideoProcessorSoftwareDevice(const pCallbacks: Pointer): HResult; stdcall;

    function GetVideoProcessorDeviceGuids(pVideoDesc: DXVA2_VideoDesc;
                                          out pCount: UINT;
                                          out pGuids: PGUID): HResult; stdcall;

    function GetVideoProcessorRenderTargets(const VideoProcDeviceGuid: TGuid;
                                            pVideoDesc: DXVA2_VideoDesc;
                                            out pCount: UINT;
                                            out pFormats: PD3DFORMAT): HResult; stdcall;

    function GetVideoProcessorSubStreamFormats(const VideoProcDeviceGuid: TGuid;
                                               pVideoDesc: DXVA2_VideoDesc;
                                               RenderTargetFormat: D3DFORMAT;
                                               out pCount: UINT;
                                               out pFormats: PD3DFORMAT): HResult; stdcall;

    function GetVideoProcessorCaps(const VideoProcDeviceGuid: TGuid;
                                   pVideoDesc: DXVA2_VideoDesc;
                                   RenderTargetFormat: D3DFORMAT;
                                   out pCaps: DXVA2_VideoProcessorCaps): HResult; stdcall;

    function GetProcAmpRange(const VideoProcDeviceGuid: TGuid;
                             pVideoDesc: DXVA2_VideoDesc;
                             RenderTargetFormat: D3DFORMAT;
                             ProcAmpCap: UINT;
                             out pRange: DXVA2_ValueRange): HResult; stdcall;

    function GetFilterPropertyRange(const VideoProcDeviceGuid: TGuid;
                                    pVideoDesc: DXVA2_VideoDesc;
                                    RenderTargetFormat: D3DFORMAT;
                                    FilterSetting: UINT;
                                    out pRange: DXVA2_ValueRange): HResult; stdcall;

    function CreateVideoProcessor(const VideoProcDeviceGuid: TGuid;
                                  pVideoDesc: DXVA2_VideoDesc;
                                  RenderTargetFormat: D3DFORMAT;
                                  MaxNumSubStreams: UINT;
                                  out ppVidProcess: PIDirectXVideoProcessor): HResult; stdcall;
  end;
  IID_IDirectXVideoProcessorService = IDirectXVideoProcessorService;
  {$EXTERNALSYM IID_IDirectXVideoProcessorService}


  // Interface IDirectXVideoDecoder
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectXVideoDecoder);'}
  {$EXTERNALSYM IDirectXVideoDecoder}
  IDirectXVideoDecoder = interface(IUnknown)
  ['{f2b0810a-fd00-43c9-918c-df94e2d8ef7d}']

    function GetVideoDecoderService(out ppService: PIDirectXVideoDecoderService): HResult; stdcall;

    function GetCreationParameters(out pDeviceGuid: TGUID;
                                   out pVideoDesc: DXVA2_VideoDesc;
                                   out pConfig: DXVA2_ConfigPictureDecode;
                                   out pDecoderRenderTargets: PIDirect3DSurface9;
                                   out pNumSurfaces: UINT): HResult; stdcall;

    function GetBuffer(BufferType: UINT;
                       out ppBuffer: Pointer;
                       out pBufferSize: UINT): HResult; stdcall;

    function ReleaseBuffer(BufferType: UINT): HResult; stdcall;

    function BeginFrame(pRenderTarget: IDirect3DSurface9;
                        pvPVPData: pointer): HResult; stdcall;

    function EndFrame(out pHandleComplete: THANDLE): HResult; stdcall;

    function Execute(pExecuteParams: DXVA2_DecodeExecuteParams): HResult; stdcall;

  end;
  IID_IDirectXVideoDecoder = IDirectXVideoDecoder;
  {$EXTERNALSYM IID_IDirectXVideoDecoder}


  // Interface IDirectXVideoProcessor
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectXVideoProcessor);'}
  {$EXTERNALSYM IDirectXVideoProcessor}
  IDirectXVideoProcessor = interface(IUnknown)
  ['{8c3a39f0-916e-4690-804f-4c8001355d25}']

    function GetVideoProcessorService(out ppService: PIDirectXVideoProcessorService): HResult; stdcall;

    function GetCreationParameters(out pDeviceGuid: TGuid;
                                   out pVideoDesc: DXVA2_VideoDesc;
                                   out pRenderTargetFormat: D3DFORMAT;
                                   out pMaxNumSubStreams: UINT): HResult; stdcall;

    function GetVideoProcessorCaps(out pCaps: DXVA2_VideoProcessorCaps): HResult; stdcall;

    function GetProcAmpRange(ProcAmpCap: UINT;
                             out pRange: DXVA2_ValueRange): HResult; stdcall;

    function GetFilterPropertyRange(FilterSetting: UINT;
                                    out DXVA2_ValueRange): HResult; stdcall;

    function VideoProcessBlt(pRenderTarget: IDirect3DSurface9;
                             pBltParams: DXVA2_VideoProcessBltParams;
                             pSamples: DXVA2_VideoSample;
                             NumSamples: UINT;
                             out pHandleComplete: THANDLE): HResult; stdcall;

  end;
  IID_IDirectXVideoProcessor = IDirectXVideoProcessor;
  {$EXTERNALSYM IID_IDirectXVideoProcessor}



  PDXVA2SurfaceType = ^DXVA2_SurfaceType;
  DXVA2_SurfaceType                          = (
    DXVA2_SurfaceType_DecoderRenderTarget    = 0,
    DXVA2_SurfaceType_ProcessorRenderTarget  = 1,
    DXVA2_SurfaceType_D3DRenderTargetTexture = 2
  );
  {$EXTERNALSYM DXVA2_SurfaceType}
  TDXVA2SurfaceType = DXVA2_SurfaceType;
  {$EXTERNALSYM TDXVA2SurfaceType}


  // Interface IDirectXVideoMemoryConfiguration
  // ==========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectXVideoMemoryConfiguration);'}
  {$EXTERNALSYM IDirectXVideoMemoryConfiguration}
  IDirectXVideoMemoryConfiguration = interface(IUnknown)
  ['{b7f916dd-db3b-49c1-84d7-e45ef99ec726}']

    function GetAvailableSurfaceTypeByIndex(const dwTypeIndex: DWORD; out pdwType: DXVA2_SurfaceType): HResult; stdcall;
    // Description:
    //  Returns acceptable surface types in preference order starting at index 0
    //  for the most preferred surface type.
    //
    // Parameters:
    //  dwTypeIndex -  0-based index saying which preferred surface type to
    //                 return.
    //  pdwType     -  Surface type returned for that index.
    //
    // Return Values:
    //
    //  S_OK - returned valid acceptable type.
    //  Failure code - dwIndex beyond range of acceptable surface type indexes.
    //

    function SetSurfaceType(const dwType: DXVA2_SurfaceType): HResult; stdcall;
    // Description:
    //     Selects the surface type to be used.
    //
    //
    // Parameters:
    //    dwType - Type to be used.
    //
    // Return Values:
    //    S_OK         - surface type accepted.
    //    Failure code - surface type rejected.
    //
  end;
  // IDirectXVideoMemoryConfiguration
  IID_IDirectXVideoMemoryConfiguration = IDirectXVideoMemoryConfiguration;
  {$EXTERNALSYM IID_IDirectXVideoMemoryConfiguration}

  //=============================================================================
  // Description:
  //
  //  IDirectXVideoMemoryConfiguration is used by video decoders
  //  and transforms to agree the type of video memory to be allocated
  //  for uncompressed surfaces used as inputs or outputs
  //
  //  For example, a video decoder will acquire a service implementing
  //  this interface through a service provider such as the Enhanced Video
  //  renderer.  The decoder enumerates the service's preferred surface types
  //  and chooses the first acceptable one.  Then the decoder creates output
  //  surfaces of this type and passes the results of the decode (often created
  //  using DirectX Video Acceleration Services) as its output
  //
  //  This interface is used along with other information, for example pixel
  //  format information and required buffer queue depth to determine the
  //  surfaces to allocate
  //
  //  There is no way to 'unset' the type of surface to use.  Other context
  //  in the contract between objects may invalidate the type selected.  For example
  //  in DirectShow disconnecting 2 filters invalidates any prior agreement between
  //  the two filters about the surface type to use for media samples.
  //


  function DXVA2CreateDirect3DDeviceManager9(out pResetToken: UINT;
                                             out ppDeviceManager: PIDirect3DDeviceManager9): HResult; stdcall;
  {$EXTERNALSYM DXVA2CreateDirect3DDeviceManager9}
  // Parameters
  //  pResetToken [out]
  //    Receives a token that identifies this instance of the Direct3D device manager. Use this token when calling IDirect3DDeviceManager9::ResetDevice.
  //  ppDXVAManager [out]
  //    Receives a pointer to the IDirect3DDeviceManager9 interface. The caller must release the interface.
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.


  function DXVA2CreateVideoService(pDD: IDirect3DDevice9;
                                   const riid: TGUID;
                                   out ppService): HResult; stdcall;
  {$EXTERNALSYM DXVA2CreateVideoService}
  //Parameters
  // pDD [in]
  //    A pointer to the IDirect3DDevice9 interface of a Direct3D device.
  // riid [in]
  //    The interface identifier (IID) of the requested interface.
  //    Any of the following interfaces might be supported by the
  //    Direct3D device:
  //         - IDirectXVideoAccelerationService
  //         - IDirectXVideoDecoderService
  //         - IDirectXVideoProcessorService
  // ppService [out]
  //    Receives a pointer to the interface. The caller must release the interface.
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.



  // Description:
  //
  //  DXVA2_Fixed32 helper inline functions.
  //  see:
  function DXVA2FloatToFixed(_float_: Single): DXVA2_Fixed32; inline;
  // Converts a floating-point number to a DXVA2_Fixed32 value.
  // Parameters:
  // _float_  [const] Floating-point number to convert to a fixed-point value.
  // Return value:
  // The function returns a DXVA2_Fixed32 structure that contains the converted fixed-point value.


  function DXVA2FixedToFloat(_fixed_: DXVA2_Fixed32): Single; inline;
  // Converts a DXVA2_Fixed32 value to a floating-point number.
  // Parameters:
  // _fixed_  [const]  DXVA2_Fixed32 structure that contains a fixed-point value.
  // Return value:
  // The function returns the converted floating-point number.


  function DXVA2_Fixed32TransparentAlpha(): DXVA2_Fixed32; inline;
  // Returns a DXVA2_Fixed32 structure that contains a transparent alpha value.
  // You can use this function for DirectX Video Acceleration (DXVA) operations that
  // require alpha values expressed as fixed-point numbers.
  // Parameters: -
  // Return value:
  // The function returns a DXVA2_Fixed32 structure that contains a transparent alpha value.


  function DXVA2_Fixed32OpaqueAlpha(): DXVA2_Fixed32; inline;
  // Returns a DXVA2_Fixed32 structure that contains an opaque alpha value.
  // You can use this function for DirectX Video Acceleration (DXVA) operations that
  // require alpha values expressed as fixed-point numbers.
  // Parameters: -
  // Return value:
  // The function returns a DXVA2_Fixed32 structure that contains an opaque alpha value.


  // Additional Prototypes for ALL interfaces

  function ConvertMFTypeToDXVAType(pType: IMFAttributes;
                                   pDesc: PDXVA2VideoDesc): HRESULT;

  procedure GetDXVA2ExtendedFormatFromMFMediaType(pType: IMFAttributes;
                                                  pFormat: DXVA2_ExtendedFormat);

  function CreateD3DDeviceManager(pDevice: IDirect3DDevice9;
                                  out pReset: UINT;
                                  out ppManager: PIDirect3DDeviceManager9): HRESULT;

  // End of Additional Prototypes


implementation


{$WARN SYMBOL_PLATFORM OFF}

  function DXVA2CreateDirect3DDeviceManager9; external DxVa2ApiLib name 'DXVA2CreateDirect3DDeviceManager9' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function DXVA2CreateVideoService; external DxVa2ApiLib name 'DXVA2CreateVideoService' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}


// Converters
// ==========

function DXVA2FloatToFixed(_float_: Single): DXVA2_Fixed32; inline;
var
    lTemp: longint;
begin
    lTemp:= Trunc(_float_ * $10000);
    Result.Fraction:= LOWORD(lTemp);
    Result.Value:= HIWORD(lTemp);
end;
//Parameters
//
// _float_ [in]
//    Floating-point number to convert to a fixed-point value.
//
// Return value
//
//    The function returns a DXVA2_Fixed32 structure that contains the converted fixed-point value.


function DXVA2FixedToFloat(_fixed_: DXVA2_Fixed32): Single; inline;
begin
  Result:= (_fixed_.Value) + (_fixed_.Fraction / $10000);
end;
//Parameters
//
//_fixed_ [in]
//   DXVA2_Fixed32 structure that contains a fixed-point value.
//
// Return value
//
//   The function returns the converted floating-point number.


function DXVA2_Fixed32TransparentAlpha(): DXVA2_Fixed32; inline;
begin
  Result.Fraction:= 0;
  Result.Value:= 0;
end;
// Returns a DXVA2_Fixed32 structure that contains a transparent alpha value.
// You can use this function for DirectX Video Acceleration (DXVA) operations that
// require alpha values expressed as fixed-point numbers.
// Parameters
//    This function has no parameters.
// Return value
//    The function returns a DXVA2_Fixed32 structure that contains a transparent alpha value.


function DXVA2_Fixed32OpaqueAlpha(): DXVA2_Fixed32; inline;
begin
  Result.Fraction:= 0;
  Result.Value:= 1;
end;
// Returns a DXVA2_Fixed32 structure that contains an opaque alpha value.
// You can use this function for DirectX Video Acceleration (DXVA) operations that
// require alpha values expressed as fixed-point numbers.
// Parameters
//    This function has no parameters.
// Return value
//    The function returns a DXVA2_Fixed32 structure that contains an opaque alpha value.

//------------------------------------------------------------------------------



/////// DXVA2_ExtendedFormat ///////////////////////////////////////////////////

{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}

function DXVA2_ExtendedFormat.ReadBits(const aIndex: Integer): Integer;
var
  Offset: Integer;
  NrBits: Integer;
  Mask: Integer;
begin
  NrBits := aIndex and $FF;
  Offset := aIndex shr 8;
  Mask := ((1 shl NrBits) - 1);
  Result := (Value shr Offset) and Mask;
end;

procedure DXVA2_ExtendedFormat.WriteBits(const aIndex: Integer; const aValue: Integer);
var
  Offset: Integer;
  NrBits: Integer;
  Mask: Integer;
begin
  NrBits := aIndex and $FF;
  Offset := aIndex shr 8;
  Mask := ((1 shl NrBits) - 1);
  Assert(aValue <= Mask);
  Value := (Value and (not (Mask shl Offset))) or (aValue shl Offset);
end;
// END DXVA2_ExtendedFormat ////////////////////////////////////////////////////



/// Helpers ///

// Fills in the DXVA2_ExtendedFormat structure.
procedure GetDXVA2ExtendedFormatFromMFMediaType(pType: ImfAttributes;
                                                pFormat: DXVA2_ExtendedFormat);
var
  interlace: UINT32;
  RetVal: UINT32;

begin     //
  // Get the interlace mode. An example from M$ that didn't work :-)
  interlace:= MFGetAttributeUINT32(pType, MF_MT_INTERLACE_MODE, ord(MFVideoInterlace_Unknown));
  // The values for interlace mode translate directly, except for mixed
  // interlace or progressive mode.

  if (interlace = UINT(MFVideoInterlace_MixedInterlaceOrProgressive)) then
    // Default to interleaved fields.
    pFormat.SampleFormat := UINT(DXVA2_SampleFieldInterleavedEvenFirst)
  else
    pFormat.SampleFormat := UINT(interlace);

  // The remaining values translate directly.
  // Use the "no-fail" attribute functions and default to "unknown."
  // Same Issue
  RetVal:= MFGetAttributeUINT32(pType, MF_MT_VIDEO_CHROMA_SITING, ord(MFVideoChromaSubsampling_Unknown));
  if not (FAILED(RetVal)) then
    pFormat.VideoChromaSubsampling := RetVal
  else
    Abort;

  RetVal:= MFGetAttributeUINT32(pType, MF_MT_VIDEO_NOMINAL_RANGE, ord(MFNominalRange_Unknown));
  if not (FAILED(RetVal)) then
    pFormat.NominalRange := RetVal
  else
    Abort;

  RetVal:= MFGetAttributeUINT32(pType, MF_MT_YUV_MATRIX, ord(MFVideoTransferMatrix_Unknown));
  if not (FAILED(RetVal)) then
     pFormat.VideoTransferMatrix := RetVal
  else
    Abort;

  RetVal:= MFGetAttributeUINT32(pType, MF_MT_VIDEO_LIGHTING, ord(MFVideoLighting_Unknown));
  if not (FAILED(RetVal)) then
    pFormat.VideoLighting := RetVal
  else
    Abort;

  RetVal:= MFGetAttributeUINT32(pType, MF_MT_VIDEO_PRIMARIES, ord(MFVideoPrimaries_Unknown));
  if not (FAILED(RetVal)) then
    pFormat.VideoPrimaries := RetVal
  else
    Abort;

  RetVal:= MFGetAttributeUINT32(pType, MF_MT_TRANSFER_FUNCTION, ord(MFVideoTransFunc_Unknown));
  if not (FAILED(RetVal)) then
    pFormat.VideoTransferFunction := RetVal
  else
    Abort;
end;


// helper ConvertMFTypeToDXVAType
//===============================
function ConvertMFTypeToDXVAType(pType: IMFAttributes;
                                 pDesc: PDXVA2VideoDesc): HRESULT;
var
  subtype: TGuid;
  width: UINT32;
  height: UINT32;
  fpsNumerator: UINT32;
  fpsDenominator: UINT32;
  hr: HRESULT;

begin
  subtype:= GUID_NULL;
  width := 0;
  height := 0;
  fpsNumerator := 0;
  fpsDenominator := 0;
  hr := 0;
  ZeroMemory(pDesc, sizeof(pDesc));
try
  // The D3D format is the first DWORD of the subtype GUID.
  hr:= pType.GetGUID(MF_MT_SUBTYPE, subtype);
  if (FAILED(hr)) then
    Abort;

  pDesc.Format := subtype.D1;

  // Frame size.
  hr:= MFGetAttributeSize(pType, MF_MT_FRAME_SIZE, width, height);
  if (FAILED(hr)) then
    Abort;

  pDesc.SampleWidth := width;
  pDesc.SampleHeight := height;

  // Frame rate.
  hr:= MFGetAttributeRatio(pType, MF_MT_FRAME_RATE, fpsNumerator, fpsDenominator);
  if (FAILED(hr)) then
    Abort;

  pDesc.InputSampleFreq.Numerator := fpsNumerator;
  pDesc.InputSampleFreq.Denominator := fpsDenominator;

  // Extended format information.
  GetDXVA2ExtendedFormatFromMFMediaType(pType, pDesc.SampleFormat);

  // For progressive or single-field types, the output frequency is the same as
  // the input frequency. For interleaved-field types, the output frequency is
  // twice the input frequency.
  pDesc.OutputFrameFreq:= pDesc.InputSampleFreq;

  if (pDesc.SampleFormat.SampleFormat = ord(DXVA2_SampleFieldInterleavedEvenFirst)) or
     (pDesc.SampleFormat.SampleFormat = ord(DXVA2_SampleFieldInterleavedOddFirst)) then
    pDesc.OutputFrameFreq.Numerator := 2;

finally
    Result:= hr;
end;
end;


function CreateD3DDeviceManager(pDevice: IDirect3DDevice9;
                                out pReset: UINT;
                                out ppManager: PIDirect3DDeviceManager9): HRESULT;
var
  resetToken: UINT;
  PD3DManager: PIDirect3DDeviceManager9;

  hr: HRESULT;
  label Done;

begin
    resetToken:= 0;
    PD3DManager:= Nil;

    hr:= DXVA2CreateDirect3DDeviceManager9(resetToken, PD3DManager);
    if (FAILED(hr)) then
      goto done;

    hr:= PD3DManager.ResetDevice(pDevice, resetToken);
    if (FAILED(hr)) then
      goto Done;

    ppManager:= PD3DManager;
    pReset:= resetToken;

    goto Done;

Done:
  PD3DManager := Nil;
  Result:= hr;
end;


end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
// Module: WinApi.DirectX.DXGIFormat.pas
// Kind: Pascal / Delphi unit
// Release date: 12-10-2015
// Language: ENU
//
// Revision Version: 3.1.2
//
// Description: Microsoft DirectX Graphics Infrastructure API
//              Direct3D include file DXGI_FORMAT enumeration for the D3D11 API.
//              See: https://msdn.microsoft.com/en-us/library/windows/desktop/bb173059
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/06/2022 All                 Mercury release  SDK 10.0.22621.0 (Windows 11)
// 24/04/2022 All                 General Update to version 3.1.2
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX312
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: dxgiformat.h
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
unit WinApi.DirectX.DXGIFormat;

  {$HPPEMIT '#include "dxgiformat.h"'}

interface

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$WARN BOUNDS_ERROR OFF}

const
  DXGI_FORMAT_DEFINED = 1;
  {$EXTERNALSYM DXGI_FORMAT_DEFINED}

//
// Enum DXGI_FORMAT
//=================

type
  PDxGiFormat = ^DXGI_FORMAT;
  PDXGI_FORMAT = ^DXGI_FORMAT;
  DXGI_FORMAT = Cardinal {UINT};
  {$EXTERNALSYM DXGI_FORMAT}
const
    DXGI_FORMAT_UNKNOWN                                 = DXGI_FORMAT(0);
    DXGI_FORMAT_R32G32B32A32_TYPELESS                   = DXGI_FORMAT(1);
    DXGI_FORMAT_R32G32B32A32_FLOAT                      = DXGI_FORMAT(2);
    DXGI_FORMAT_R32G32B32A32_UINT                       = DXGI_FORMAT(3);
    DXGI_FORMAT_R32G32B32A32_SINT                       = DXGI_FORMAT(4);
    DXGI_FORMAT_R32G32B32_TYPELESS                      = DXGI_FORMAT(5);
    DXGI_FORMAT_R32G32B32_FLOAT                         = DXGI_FORMAT(6);
    DXGI_FORMAT_R32G32B32_UINT                          = DXGI_FORMAT(7);
    DXGI_FORMAT_R32G32B32_SINT                          = DXGI_FORMAT(8);
    DXGI_FORMAT_R16G16B16A16_TYPELESS                   = DXGI_FORMAT(9);
    DXGI_FORMAT_R16G16B16A16_FLOAT                      = DXGI_FORMAT(10);
    DXGI_FORMAT_R16G16B16A16_UNORM                      = DXGI_FORMAT(11);
    DXGI_FORMAT_R16G16B16A16_UINT                       = DXGI_FORMAT(12);
    DXGI_FORMAT_R16G16B16A16_SNORM                      = DXGI_FORMAT(13);
    DXGI_FORMAT_R16G16B16A16_SINT                       = DXGI_FORMAT(14);
    DXGI_FORMAT_R32G32_TYPELESS                         = DXGI_FORMAT(15);
    DXGI_FORMAT_R32G32_FLOAT                            = DXGI_FORMAT(16);
    DXGI_FORMAT_R32G32_UINT                             = DXGI_FORMAT(17);
    DXGI_FORMAT_R32G32_SINT                             = DXGI_FORMAT(18);
    DXGI_FORMAT_R32G8X24_TYPELESS                       = DXGI_FORMAT(19);
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT                    = DXGI_FORMAT(20);
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS                = DXGI_FORMAT(21);
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT                 = DXGI_FORMAT(22);
    DXGI_FORMAT_R10G10B10A2_TYPELESS                    = DXGI_FORMAT(23);
    DXGI_FORMAT_R10G10B10A2_UNORM                       = DXGI_FORMAT(24);
    DXGI_FORMAT_R10G10B10A2_UINT                        = DXGI_FORMAT(25);
    DXGI_FORMAT_R11G11B10_FLOAT                         = DXGI_FORMAT(26);
    DXGI_FORMAT_R8G8B8A8_TYPELESS                       = DXGI_FORMAT(27);
    DXGI_FORMAT_R8G8B8A8_UNORM                          = DXGI_FORMAT(28);
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB                     = DXGI_FORMAT(29);
    DXGI_FORMAT_R8G8B8A8_UINT                           = DXGI_FORMAT(30);
    DXGI_FORMAT_R8G8B8A8_SNORM                          = DXGI_FORMAT(31);
    DXGI_FORMAT_R8G8B8A8_SINT                           = DXGI_FORMAT(32);
    DXGI_FORMAT_R16G16_TYPELESS                         = DXGI_FORMAT(33);
    DXGI_FORMAT_R16G16_FLOAT                            = DXGI_FORMAT(34);
    DXGI_FORMAT_R16G16_UNORM                            = DXGI_FORMAT(35);
    DXGI_FORMAT_R16G16_UINT                             = DXGI_FORMAT(36);
    DXGI_FORMAT_R16G16_SNORM                            = DXGI_FORMAT(37);
    DXGI_FORMAT_R16G16_SINT                             = DXGI_FORMAT(38);
    DXGI_FORMAT_R32_TYPELESS                            = DXGI_FORMAT(39);
    DXGI_FORMAT_D32_FLOAT                               = DXGI_FORMAT(40);
    DXGI_FORMAT_R32_FLOAT                               = DXGI_FORMAT(41);
    DXGI_FORMAT_R32_UINT                                = DXGI_FORMAT(42);
    DXGI_FORMAT_R32_SINT                                = DXGI_FORMAT(43);
    DXGI_FORMAT_R24G8_TYPELESS                          = DXGI_FORMAT(44);
    DXGI_FORMAT_D24_UNORM_S8_UINT                       = DXGI_FORMAT(45);
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS                   = DXGI_FORMAT(46);
    DXGI_FORMAT_X24_TYPELESS_G8_UINT                    = DXGI_FORMAT(47);
    DXGI_FORMAT_R8G8_TYPELESS                           = DXGI_FORMAT(48);
    DXGI_FORMAT_R8G8_UNORM                              = DXGI_FORMAT(49);
    DXGI_FORMAT_R8G8_UINT                               = DXGI_FORMAT(50);
    DXGI_FORMAT_R8G8_SNORM                              = DXGI_FORMAT(51);
    DXGI_FORMAT_R8G8_SINT                               = DXGI_FORMAT(52);
    DXGI_FORMAT_R16_TYPELESS                            = DXGI_FORMAT(53);
    DXGI_FORMAT_R16_FLOAT                               = DXGI_FORMAT(54);
    DXGI_FORMAT_D16_UNORM                               = DXGI_FORMAT(55);
    DXGI_FORMAT_R16_UNORM                               = DXGI_FORMAT(56);
    DXGI_FORMAT_R16_UINT                                = DXGI_FORMAT(57);
    DXGI_FORMAT_R16_SNORM                               = DXGI_FORMAT(58);
    DXGI_FORMAT_R16_SINT                                = DXGI_FORMAT(59);
    DXGI_FORMAT_R8_TYPELESS                             = DXGI_FORMAT(60);
    DXGI_FORMAT_R8_UNORM                                = DXGI_FORMAT(61);
    DXGI_FORMAT_R8_UINT                                 = DXGI_FORMAT(62);
    DXGI_FORMAT_R8_SNORM                                = DXGI_FORMAT(63);
    DXGI_FORMAT_R8_SINT                                 = DXGI_FORMAT(64);
    DXGI_FORMAT_A8_UNORM                                = DXGI_FORMAT(65);
    DXGI_FORMAT_R1_UNORM                                = DXGI_FORMAT(66);
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP                      = DXGI_FORMAT(67);
    DXGI_FORMAT_R8G8_B8G8_UNORM                         = DXGI_FORMAT(68);
    DXGI_FORMAT_G8R8_G8B8_UNORM                         = DXGI_FORMAT(69);
    DXGI_FORMAT_BC1_TYPELESS                            = DXGI_FORMAT(70);
    DXGI_FORMAT_BC1_UNORM                               = DXGI_FORMAT(71);
    DXGI_FORMAT_BC1_UNORM_SRGB                          = DXGI_FORMAT(72);
    DXGI_FORMAT_BC2_TYPELESS                            = DXGI_FORMAT(73);
    DXGI_FORMAT_BC2_UNORM                               = DXGI_FORMAT(74);
    DXGI_FORMAT_BC2_UNORM_SRGB                          = DXGI_FORMAT(75);
    DXGI_FORMAT_BC3_TYPELESS                            = DXGI_FORMAT(76);
    DXGI_FORMAT_BC3_UNORM                               = DXGI_FORMAT(77);
    DXGI_FORMAT_BC3_UNORM_SRGB                          = DXGI_FORMAT(78);
    DXGI_FORMAT_BC4_TYPELESS                            = DXGI_FORMAT(79);
    DXGI_FORMAT_BC4_UNORM                               = DXGI_FORMAT(80);
    DXGI_FORMAT_BC4_SNORM                               = DXGI_FORMAT(81);
    DXGI_FORMAT_BC5_TYPELESS                            = DXGI_FORMAT(82);
    DXGI_FORMAT_BC5_UNORM                               = DXGI_FORMAT(83);
    DXGI_FORMAT_BC5_SNORM                               = DXGI_FORMAT(84);
    DXGI_FORMAT_B5G6R5_UNORM                            = DXGI_FORMAT(85);
    DXGI_FORMAT_B5G5R5A1_UNORM                          = DXGI_FORMAT(86);
    DXGI_FORMAT_B8G8R8A8_UNORM                          = DXGI_FORMAT(87);
    DXGI_FORMAT_B8G8R8X8_UNORM                          = DXGI_FORMAT(88);
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM              = DXGI_FORMAT(89);
    DXGI_FORMAT_B8G8R8A8_TYPELESS                       = DXGI_FORMAT(90);
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB                     = DXGI_FORMAT(91);
    DXGI_FORMAT_B8G8R8X8_TYPELESS                       = DXGI_FORMAT(92);
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB                     = DXGI_FORMAT(93);
    DXGI_FORMAT_BC6H_TYPELESS                           = DXGI_FORMAT(94);
    DXGI_FORMAT_BC6H_UF16                               = DXGI_FORMAT(95);
    DXGI_FORMAT_BC6H_SF16                               = DXGI_FORMAT(96);
    DXGI_FORMAT_BC7_TYPELESS                            = DXGI_FORMAT(97);
    DXGI_FORMAT_BC7_UNORM                               = DXGI_FORMAT(98);
    DXGI_FORMAT_BC7_UNORM_SRGB                          = DXGI_FORMAT(99);
    DXGI_FORMAT_AYUV                                    = DXGI_FORMAT(100);
    DXGI_FORMAT_Y410                                    = DXGI_FORMAT(101);
    DXGI_FORMAT_Y416                                    = DXGI_FORMAT(102);
    DXGI_FORMAT_NV12                                    = DXGI_FORMAT(103);
    DXGI_FORMAT_P010                                    = DXGI_FORMAT(104);
    DXGI_FORMAT_P016                                    = DXGI_FORMAT(105);
    DXGI_FORMAT_420_OPAQUE                              = DXGI_FORMAT(106);
    DXGI_FORMAT_YUY2                                    = DXGI_FORMAT(107);
    DXGI_FORMAT_Y210                                    = DXGI_FORMAT(108);
    DXGI_FORMAT_Y216                                    = DXGI_FORMAT(109);
    DXGI_FORMAT_NV11                                    = DXGI_FORMAT(110);
    DXGI_FORMAT_AI44                                    = DXGI_FORMAT(111);
    DXGI_FORMAT_IA44                                    = DXGI_FORMAT(112);
    DXGI_FORMAT_P8                                      = DXGI_FORMAT(113);
    DXGI_FORMAT_A8P8                                    = DXGI_FORMAT(114);
    DXGI_FORMAT_B4G4R4A4_UNORM                          = DXGI_FORMAT(115);

    DXGI_FORMAT_P208                                    = DXGI_FORMAT(130);
    DXGI_FORMAT_V208                                    = DXGI_FORMAT(131);
    DXGI_FORMAT_V408                                    = DXGI_FORMAT(132);

    // >= DXGI_FORMAT(Win10 20H3  ! NOT PRESENT IN Win64 version 10/1909 D2D1.dll
    DXGI_FORMAT_SAMPLER_FEEDBACK_MIN_MIP_OPAQUE         = DXGI_FORMAT(189);
    DXGI_FORMAT_SAMPLER_FEEDBACK_MIP_REGION_USED_OPAQUE = DXGI_FORMAT(190);
    // DXGI_FORMAT_FORCE_UINT                              = DXGI_FORMAT($7FFFFFFF);

 {$EXTERNALSYM DXGI_FORMAT}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  //Implement Additional Prototypes here.

end.

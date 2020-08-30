// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXVA.pas
// Kind: Pascal / Delphi unit
// Release date: 02-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: DirectX Video Acceleration (DXVA 2.0) header file.
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
// Remarks: Requires Windows Vista or later.
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
// Source: dxva.h
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
unit WinApi.DirectX.DXVA;

  {$HPPEMIT '#include "dxva.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.D3d9Types,
  WinApi.DirectX.D3D9;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$WARN BOUNDS_ERROR OFF}

const
  // -------------------------------------------------------------------------
  //
  // The definitions that follow describe the DirectX Video Acceleration
  // decoding interface.
  // This interface is accessable via the IAMVideoAccelerator interface.
  //
  // -------------------------------------------------------------------------
  //

  DXVA_ModeNone             : TGUID = (D1 : $1b81be00; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeNone}
  DXVA_ModeH261_A           : TGUID = (D1 : $1b81be01; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH261_A}
  DXVA_ModeH261_B           : TGUID = (D1 : $1b81be02; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH261_B}


  DXVA_ModeH263_A           : TGUID = (D1 : $1b81be03; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH263_A}
  DXVA_ModeH263_B           : TGUID = (D1 : $1b81be04; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH263_B}
  DXVA_ModeH263_C           : TGUID = (D1 : $1b81be05; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH263_C}
  DXVA_ModeH263_D           : TGUID = (D1 : $1b81be06; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH263_D}
  DXVA_ModeH263_E           : TGUID = (D1 : $1b81be07; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH263_E}
  DXVA_ModeH263_F           : TGUID = (D1 : $1b81be08; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH263_F}

  DXVA_ModeMPEG1_A          :	TGUID = (D1: $1b81be09; D2: $a0c7; D3: $11d3; D4: ($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeMPEG1_A}
  {$EXTERNALSYM DXVA_ModeMPEG1_VLD}
  DXVA_ModeMPEG1_VLD        :	TGUID = (D1: $6f3ec719; D2: $3735; D3: $42cc; D4: ($80, $63, $65, $cc, $3c, $b3, $66, $16));

  DXVA_ModeMPEG2_A          : TGUID = (D1 : $1b81be0A; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeMPEG2_A}
  DXVA_ModeMPEG2_B          : TGUID = (D1 : $1b81be0B; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeMPEG2_B}
  DXVA_ModeMPEG2_C          : TGUID = (D1 : $1b81be0C; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeMPEG2_C}
  DXVA_ModeMPEG2_D          : TGUID = (D1 : $1b81be0D; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeMPEG2_D}
  DXVA_ModeMPEG2and1_VLD    : TGUID = (D1 : $86695f12; D2 : $340e; D3 : $4f04; D4:($9f, $d3, $92, $53, $dd, $32, $74, $60));
  {$EXTERNALSYM DXVA_ModeMPEG2and1_VLD}

  DXVA_ModeH264_A           : TGUID = (D1 : $1b81be64; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_A}
  DXVA_ModeH264_B           : TGUID = (D1 : $1b81be65; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_B}
  DXVA_ModeH264_C           : TGUID = (D1 : $1b81be66; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_C}
  DXVA_ModeH264_D           : TGUID = (D1 : $1b81be67; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_D}
  DXVA_ModeH264_E           : TGUID = (D1 : $1b81be68; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_E}
  DXVA_ModeH264_F           : TGUID = (D1 : $1b81be69; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_F}
  DXVA_ModeH264_VLD_WithFMOASO_NoFGT            : TGUID = (D1 : $d5f04ff9; D2 : $3418; D3 : $45d8; D4:($95, $61, $32, $a7, $6a, $ae, $2d, $dd));
  {$EXTERNALSYM DXVA_ModeH264_VLD_WithFMOASO_NoFGT}

  DXVA_ModeH264_VLD_Stereo_Progressive_NoFGT    : TGUID = (D1 : $d79be8da; D2 : $0cf1; D3 : $4c81; D4:($b8, $2a, $69, $a4, $e2, $36, $f4, $3d));
  {$EXTERNALSYM DXVA_ModeH264_VLD_Stereo_Progressive_NoFGT}
  DXVA_ModeH264_VLD_Stereo_NoFGT                : TGUID = (D1 : $f9aaccbb; D2 : $c2b6; D3 : $4cfc; D4:($87, $79, $57, $07, $b1, $76, $05, $52));
  {$EXTERNALSYM DXVA_ModeH264_VLD_Stereo_NoFGT}
  DXVA_ModeH264_VLD_Multiview_NoFGT             : TGUID = (D1 : $705b9d82; D2 : $76cf; D3 : $49d6; D4:($b7, $e6, $ac, $88, $72, $db, $01, $3c));
  {$EXTERNALSYM DXVA_ModeH264_VLD_Multiview_NoFGT}

  DXVA_ModeWMV8_A           : TGUID = (D1 : $1b81be80; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV8_A}
  DXVA_ModeWMV8_B           : TGUID = (D1 : $1b81be81; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV8_B}

  DXVA_ModeWMV9_A           : TGUID = (D1 : $1b81be90; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV9_A}
  DXVA_ModeWMV9_B           : TGUID = (D1 : $1b81be91; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV9_B}
  DXVA_ModeWMV9_C           : TGUID = (D1 : $1b81be94; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV9_C}

  DXVA_ModeVC1_A           : TGUID = (D1 : $1b81beA0; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_A}
  DXVA_ModeVC1_B           : TGUID = (D1 : $1b81beA1; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_B}
  DXVA_ModeVC1_C           : TGUID = (D1 : $1b81beA2; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_C}
  DXVA_ModeVC1_D           : TGUID = (D1 : $1b81beA3; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_D}
  DXVA_ModeVC1_D2010       : TGUID = (D1 : $1b81beA4; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_D2010}

  DXVA_ModeMPEG4pt2_VLD_Simple            : TGUID = (D1 : $efd64d74; D2 : $c9e8; D3 : $41d7; D4:($a5, $e9, $e9, $b0, $e3, $9f, $a3, $19));
  {$EXTERNALSYM DXVA_ModeMPEG4pt2_VLD_Simple}
  DXVA_ModeMPEG4pt2_VLD_AdvSimple_NoGMC   : TGUID = (D1 : $ed418a9f; D2 : $010d; D3 : $4eda; D4:($9a, $e3, $9a, $65, $35, $8d, $8d, $2e));
  {$EXTERNALSYM DXVA_ModeMPEG4pt2_VLD_AdvSimple_NoGMC}
  DXVA_ModeMPEG4pt2_VLD_AdvSimple_GMC     : TGUID = (D1 : $ab998b5b; D2 : $4258; D3 : $44a9; D4:($9f, $eb, $94, $e5, $97, $a6, $ba, $ae));
  {$EXTERNALSYM DXVA_ModeMPEG4pt2_VLD_AdvSimple_GMC}

  DXVA_ModeHEVC_VLD_Main                  : TGUID = (D1 : $5b11d51b; D2 : $2f4c; D3 : $4452; D4:($bc, $c3, $09, $f2, $a1, $16, $0c, $c0));
  {$EXTERNALSYM DXVA_ModeHEVC_VLD_Main}
  DXVA_ModeHEVC_VLD_Main10                : TGUID = (D1 : $107af0e0; D2 : $ef1a; D3 : $4d19; D4:($ab, $a8, $67, $a1, $63, $07, $3d, $13));
  {$EXTERNALSYM DXVA_ModeHEVC_VLD_Main10}

  DXVA_NoEncrypt                          : TGUID = (D1 : $1b81beD0; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_NoEncrypt}


  // Delphi 10++ compiler introduction
  // that generates a [dcc32 Error] : E2026 Constant expression expected,
  // that only can be solved with hard coding.
  // Not possible any more, is something like this: DXVA_ModeWMV8_PostProc = DXVA_ModeWMV8_A;


  DXVA_ModeWMV8_PostProc              : TGUID = (D1 : $1b81be80; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV8_PostProc}
  DXVA_ModeWMV8_MoComp                : TGUID = (D1 : $1b81be81; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV8_MoComp}

  DXVA_ModeWMV9_PostProc              : TGUID = (D1 : $1b81be90; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV9_PostProc}
  DXVA_ModeWMV9_MoComp                : TGUID = (D1 : $1b81be91; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV9_MoComp}
  DXVA_ModeWMV9_IDCT                  : TGUID = (D1 : $1b81be94; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeWMV9_IDCT}

  DXVA_ModeVC1_PostProc               : TGUID = (D1 : $1b81beA0; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_PostProc}
  DXVA_ModeVC1_MoComp                 : TGUID = (D1 : $1b81beA1; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_MoComp}
  DXVA_ModeVC1_IDCT                   : TGUID = (D1 : $1b81beA2; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_IDCT}
  DXVA_ModeVC1_VLD                    : TGUID = (D1 : $1b81beA3; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeVC1_VLD}

  DXVA_ModeH264_MoComp_NoFGT          : TGUID = (D1 : $1b81be64; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_MoComp_NoFGT}
  DXVA_ModeH264_MoComp_FGT            : TGUID = (D1 : $1b81be65; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_MoComp_FGT}
  DXVA_ModeH264_IDCT_NoFGT            : TGUID = (D1 : $1b81be66; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_IDCT_NoFGT}
  DXVA_ModeH264_IDCT_FGT              : TGUID = (D1 : $1b81be67; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_IDCT_FGT}
  DXVA_ModeH264_VLD_NoFGT             : TGUID = (D1 : $1b81be68; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_VLD_NoFGT}
  DXVA_ModeH264_VLD_FGT               : TGUID = (D1 : $1b81be69; D2 : $a0c7; D3 : $11d3; D4:($b9, $84, $00, $c0, $4f, $2e, $73, $c5));
  {$EXTERNALSYM DXVA_ModeH264_VLD_FGT}

  DXVA_RESTRICTED_MODE_UNRESTRICTED                  = $FFFF;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_UNRESTRICTED}
  DXVA_RESTRICTED_MODE_H261_A                        = 1;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H261_A}
  DXVA_RESTRICTED_MODE_H261_B                        = 2;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H261_B}
  DXVA_RESTRICTED_MODE_H263_A                        = 3;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H263_A}
  DXVA_RESTRICTED_MODE_H263_B                        = 4;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H263_B}
  DXVA_RESTRICTED_MODE_H263_C                        = 5;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H263_C}
  DXVA_RESTRICTED_MODE_H263_D                        = 6;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H263_D}
  DXVA_RESTRICTED_MODE_H263_E                        = 7;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H263_E}
  DXVA_RESTRICTED_MODE_H263_F                        = 8;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H263_F}
  DXVA_RESTRICTED_MODE_MPEG1_A                       = 9;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG1_A}
  DXVA_RESTRICTED_MODE_MPEG2_A                       = $A;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG2_A}
  DXVA_RESTRICTED_MODE_MPEG2_B                       = $B;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG2_B}
  DXVA_RESTRICTED_MODE_MPEG2_C                       = $C;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG2_C}
  DXVA_RESTRICTED_MODE_MPEG2_D                       = $D;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG2_D}
  DXVA_RESTRICTED_MODE_MPEG1_VLD                     = $10;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG1_VLD}
  DXVA_RESTRICTED_MODE_MPEG2and1_VLD                 = $11;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG2and1_VLD}
  DXVA_RESTRICTED_MODE_H264_A                        = $64;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_A}
  DXVA_RESTRICTED_MODE_H264_B                        = $65;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_B}
  DXVA_RESTRICTED_MODE_H264_C                        = $66;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_C}
  DXVA_RESTRICTED_MODE_H264_D                        = $67;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_D}
  DXVA_RESTRICTED_MODE_H264_E                        = $68;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_E}
  DXVA_RESTRICTED_MODE_H264_F                        = $69;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_F}
  DXVA_RESTRICTED_MODE_H264_VLD_WITHFMOASO_NOFGT     = $70;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_VLD_WITHFMOASO_NOFGT}
  DXVA_RESTRICTED_MODE_H264_VLD_STEREO_PROGRESSIVE_NOFGT = $71;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_VLD_STEREO_PROGRESSIVE_NOFGT}
  DXVA_RESTRICTED_MODE_H264_VLD_STEREO_NOFGT         = $72;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_VLD_STEREO_NOFGT}
  DXVA_RESTRICTED_MODE_H264_VLD_MULTIVIEW_NOFGT      = $73;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_VLD_MULTIVIEW_NOFGT}
  DXVA_RESTRICTED_MODE_WMV8_A                        = $80;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV8_A}
  DXVA_RESTRICTED_MODE_WMV8_B                        = $81;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV8_B}
  DXVA_RESTRICTED_MODE_WMV9_A                        = $90;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV9_A}
  DXVA_RESTRICTED_MODE_WMV9_B                        = $91;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV9_B}
  DXVA_RESTRICTED_MODE_WMV9_C                        = $94;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV9_C}
  DXVA_RESTRICTED_MODE_VC1_A                         = $A0;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_A}
  DXVA_RESTRICTED_MODE_VC1_B                         = $A1;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_B}
  DXVA_RESTRICTED_MODE_VC1_C                         = $A2;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_C}
  DXVA_RESTRICTED_MODE_VC1_D                         = $A3;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_D}
  DXVA_RESTRICTED_MODE_VC1_D2010                     = $A4;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_D2010}
  DXVA_RESTRICTED_MODE_MPEG4PT2_VLD_SIMPLE           = $B0;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG4PT2_VLD_SIMPLE}
  DXVA_RESTRICTED_MODE_MPEG4PT2_VLD_ADV_SIMPLE_NOGMC = $B1;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG4PT2_VLD_ADV_SIMPLE_NOGMC}
  DXVA_RESTRICTED_MODE_MPEG4PT2_VLD_ADV_SIMPLE_GMC   = $B2;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_MPEG4PT2_VLD_ADV_SIMPLE_GMC}


  DXVA_RESTRICTED_MODE_WMV8_POSTPROC     = DXVA_RESTRICTED_MODE_WMV8_A;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV8_POSTPROC}
  DXVA_RESTRICTED_MODE_WMV8_MOCOMP       = DXVA_RESTRICTED_MODE_WMV8_B;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV8_MOCOMP}

  DXVA_RESTRICTED_MODE_WMV9_POSTPROC     = DXVA_RESTRICTED_MODE_WMV9_A;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV9_POSTPROC}
  DXVA_RESTRICTED_MODE_WMV9_MOCOMP       = DXVA_RESTRICTED_MODE_WMV9_B;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV9_MOCOMP}
  DXVA_RESTRICTED_MODE_WMV9_IDCT         = DXVA_RESTRICTED_MODE_WMV9_C;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_WMV9_IDCT}

  DXVA_RESTRICTED_MODE_VC1_POSTPROC      = DXVA_RESTRICTED_MODE_VC1_A;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_POSTPROC}
  DXVA_RESTRICTED_MODE_VC1_MOCOMP        = DXVA_RESTRICTED_MODE_VC1_B;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_MOCOMP}
  DXVA_RESTRICTED_MODE_VC1_IDCT          = DXVA_RESTRICTED_MODE_VC1_C;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_IDCT}
  DXVA_RESTRICTED_MODE_VC1_VLD           = DXVA_RESTRICTED_MODE_VC1_D;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_VC1_VLD}

  DXVA_RESTRICTED_MODE_H264_MOCOMP_NOFGT = DXVA_RESTRICTED_MODE_H264_A;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_MOCOMP_NOFGT}
  DXVA_RESTRICTED_MODE_H264_MOCOMP_FGT   = DXVA_RESTRICTED_MODE_H264_B;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_MOCOMP_FGT}
  DXVA_RESTRICTED_MODE_H264_IDCT_NOFGT   = DXVA_RESTRICTED_MODE_H264_C;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_IDCT_NOFGT}
  DXVA_RESTRICTED_MODE_H264_IDCT_FGT     = DXVA_RESTRICTED_MODE_H264_D;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_IDCT_FGT}
  DXVA_RESTRICTED_MODE_H264_VLD_NOFGT    = DXVA_RESTRICTED_MODE_H264_E;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_VLD_NOFGT}
  DXVA_RESTRICTED_MODE_H264_VLD_FGT      = DXVA_RESTRICTED_MODE_H264_F;
  {$EXTERNALSYM DXVA_RESTRICTED_MODE_H264_VLD_FGT}


  DXVA_COMPBUFFER_TYPE_THAT_IS_NOT_USED   = 0;
  {$EXTERNALSYM DXVA_COMPBUFFER_TYPE_THAT_IS_NOT_USED}
  DXVA_PICTURE_DECODE_BUFFER              = 1;
  {$EXTERNALSYM DXVA_PICTURE_DECODE_BUFFER}
  DXVA_MACROBLOCK_CONTROL_BUFFER          = 2;
  {$EXTERNALSYM DXVA_MACROBLOCK_CONTROL_BUFFER}
  DXVA_RESIDUAL_DIFFERENCE_BUFFER         = 3;
  {$EXTERNALSYM DXVA_RESIDUAL_DIFFERENCE_BUFFER}
  DXVA_DEBLOCKING_CONTROL_BUFFER          = 4;
  {$EXTERNALSYM DXVA_DEBLOCKING_CONTROL_BUFFER}
  DXVA_INVERSE_QUANTIZATION_MATRIX_BUFFER = 5;
  {$EXTERNALSYM DXVA_INVERSE_QUANTIZATION_MATRIX_BUFFER}
  DXVA_SLICE_CONTROL_BUFFER               = 6;
  {$EXTERNALSYM DXVA_SLICE_CONTROL_BUFFER}
  DXVA_BITSTREAM_DATA_BUFFER              = 7;
  {$EXTERNALSYM DXVA_BITSTREAM_DATA_BUFFER}
  DXVA_AYUV_BUFFER                        = 8;
  {$EXTERNALSYM DXVA_AYUV_BUFFER}
  DXVA_IA44_SURFACE_BUFFER                = 9;
  {$EXTERNALSYM DXVA_IA44_SURFACE_BUFFER}
  DXVA_DPXD_SURFACE_BUFFER                = 10;
  {$EXTERNALSYM DXVA_DPXD_SURFACE_BUFFER}
  DXVA_HIGHLIGHT_BUFFER                   = 11;
  {$EXTERNALSYM DXVA_HIGHLIGHT_BUFFER}
  DXVA_DCCMD_SURFACE_BUFFER               = 12;
  {$EXTERNALSYM DXVA_DCCMD_SURFACE_BUFFER}
  DXVA_ALPHA_BLEND_COMBINATION_BUFFER     = 13;
  {$EXTERNALSYM DXVA_ALPHA_BLEND_COMBINATION_BUFFER}
  DXVA_PICTURE_RESAMPLE_BUFFER            = 14;
  {$EXTERNALSYM DXVA_PICTURE_RESAMPLE_BUFFER}
  DXVA_READ_BACK_BUFFER                   = 15;
  {$EXTERNALSYM DXVA_READ_BACK_BUFFER}

// H.264/AVC Additional buffer types

  DXVA_MOTION_VECTOR_BUFFER           = 16;
  {$EXTERNALSYM DXVA_MOTION_VECTOR_BUFFER}
  DXVA_FILM_GRAIN_BUFFER              = 17;
  {$EXTERNALSYM DXVA_FILM_GRAIN_BUFFER}
  DXVA_NUM_TYPES_COMP_BUFFERS         = 18;
  {$EXTERNALSYM DXVA_NUM_TYPES_COMP_BUFFERS}

// values for bDXVA_Func

  DXVA_PICTURE_DECODING_FUNCTION        = 1;
  {$EXTERNALSYM DXVA_PICTURE_DECODING_FUNCTION}
  DXVA_ALPHA_BLEND_DATA_LOAD_FUNCTION   = 2;
  {$EXTERNALSYM DXVA_ALPHA_BLEND_DATA_LOAD_FUNCTION}
  DXVA_ALPHA_BLEND_COMBINATION_FUNCTION = 3;
  {$EXTERNALSYM DXVA_ALPHA_BLEND_COMBINATION_FUNCTION}
  DXVA_PICTURE_RESAMPLE_FUNCTION        = 4;
  {$EXTERNALSYM DXVA_PICTURE_RESAMPLE_FUNCTION}
  DXVA_DEBLOCKING_FILTER_FUNCTION       = 5;
  {$EXTERNALSYM DXVA_DEBLOCKING_FILTER_FUNCTION}
  DXVA_FILM_GRAIN_SYNTHESIS_FUNCTION    = 6;
  {$EXTERNALSYM DXVA_FILM_GRAIN_SYNTHESIS_FUNCTION}
  DXVA_STATUS_REPORTING_FUNCTION        = 7;
  {$EXTERNALSYM DXVA_STATUS_REPORTING_FUNCTION}

//* values returned from Execute command in absence of read-back */

  DXVA_EXECUTE_RETURN_OK                 = 0;
  {$EXTERNALSYM DXVA_EXECUTE_RETURN_OK}
  DXVA_EXECUTE_RETURN_DATA_ERROR_MINOR   = 1;
  {$EXTERNALSYM DXVA_EXECUTE_RETURN_DATA_ERROR_MINOR}
  DXVA_EXECUTE_RETURN_DATA_ERROR_SIGNIF  = 2;
  {$EXTERNALSYM DXVA_EXECUTE_RETURN_DATA_ERROR_SIGNIF}
  DXVA_EXECUTE_RETURN_DATA_ERROR_SEVERE  = 3;
  {$EXTERNALSYM DXVA_EXECUTE_RETURN_DATA_ERROR_SEVERE}
  DXVA_EXECUTE_RETURN_OTHER_ERROR_SEVERE = 4;
  {$EXTERNALSYM DXVA_EXECUTE_RETURN_OTHER_ERROR_SEVERE}


type

  PDXVA_ConnectMode = ^_DXVA_ConnectMode;
  LPDXVA_ConnectMode = ^_DXVA_ConnectMode;
  _DXVA_ConnectMode = record
     guidMode        : TGUID;
     wRestrictedMode : WORD;
  end;
  {$EXTERNALSYM _DXVA_ConnectMode}
  DXVA_ConnectMode = _DXVA_ConnectMode;
  {$EXTERNALSYM DXVA_ConnectMode}

  LPDXVA_ConfigQueryOrReplyFunc = ^DXVA_ConfigQueryOrReplyFunc;
  DXVA_ConfigQueryOrReplyFunc = {$IFDEF TYPE_IDENTITY} type {$ENDIF} DWORD;
  {$EXTERNALSYM DXVA_ConfigQueryOrReplyFunc}


const

  DXVA_QUERYORREPLYFUNCFLAG_DECODER_PROBE_QUERY    = $FFFFF1;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_DECODER_PROBE_QUERY}
  DXVA_QUERYORREPLYFUNCFLAG_DECODER_LOCK_QUERY     = $FFFFF5;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_DECODER_LOCK_QUERY}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_COPY    = $FFFFF8;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_COPY}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_PLUS    = $FFFFF9;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_PLUS}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_OK_COPY     = $FFFFFC;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_OK_COPY}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_FALSE_PLUS = $FFFFFB;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_FALSE_PLUS}
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_FALSE_PLUS  = $FFFFFF;
  {$EXTERNALSYM DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_FALSE_PLUS}



// Internal use
// #define readDXVA_QueryOrReplyFuncFlag(ptr)        ((*(ptr)) >> 8)
// #define readDXVA_QueryOrReplyFuncFlag_ACCEL(ptr)  (((*(ptr)) >> 11) & 1)
// #define readDXVA_QueryOrReplyFuncFlag_LOCK(ptr)   (((*(ptr)) >> 10) & 1)
// #define readDXVA_QueryOrReplyFuncFlag_BAD(ptr)    (((*(ptr)) >>  9) & 1)
// #define readDXVA_QueryOrReplyFuncFlag_PLUS(ptr)   (((*(ptr)) >>  8) & 1)
// #define readDXVA_QueryOrReplyFuncFunc(ptr)        ((*(ptr)) & 0xFF)
// #define writeDXVA_QueryOrReplyFunc(ptr, flg, fnc) ((*(ptr)) = ((flg) << 8) | (fnc))
// #define setDXVA_QueryOrReplyFuncFlag(ptr, flg) ((*(ptr)) |= ((flg) << 8))
// #define setDXVA_QueryOrReplyFuncFunc(ptr, fnc) ((*(ptr)) |= (fnc));




type

  LPDXVA_EncryptProtocolFunc = ^DXVA_EncryptProtocolFunc;
  DXVA_EncryptProtocolFunc = {$IFDEF TYPE_IDENTITY} type {$ENDIF} DWORD;
  {$EXTERNALSYM DXVA_EncryptProtocolFunc}


const
  DXVA_ENCRYPTPROTOCOLFUNCFLAG_HOST  = $FFFF00;
  {$EXTERNALSYM DXVA_ENCRYPTPROTOCOLFUNCFLAG_HOST}
  DXVA_ENCRYPTPROTOCOLFUNCFLAG_ACCEL = $FFFF08;
  {$EXTERNALSYM DXVA_ENCRYPTPROTOCOLFUNCFLAG_ACCEL}


// Internal use
//  #define readDXVA_EncryptProtocolFuncFlag(ptr)        ((*(ptr)) >> 8)
//  #define readDXVA_EncryptProtocolFuncFlag_ACCEL(ptr)  (((*(ptr)) >> 11) & 1)

//  #define readDXVA_EncryptProtocolFuncFunc(ptr)        ((*(ptr)) & 0xFF)

//  #define writeDXVA_EncryptProtocolFunc(ptr, flg, fnc) ((*(ptr)) = ((flg) << 8) | (fnc))

//  #define setDXVA_EncryptProtocolFuncFlag(ptr, flg) ((*(ptr)) |= ((flg) << 8))
//  #define setDXVA_EncryptProtocolFuncFunc(ptr, fnc) ((*(ptr)) |= (fnc));


type

  PDXVA_EncryptProtocolHeader  = ^_DXVA_EncryptProtocolHeader;
  LPDXVA_EncryptProtocolHeader  = ^_DXVA_EncryptProtocolHeader;
  _DXVA_EncryptProtocolHeader = record
    dwFunction: DXVA_EncryptProtocolFunc;
    ReservedBits: array[0..2] of DWORD;
    guidEncryptProtocol: TGUID;
  end;
  {$EXTERNALSYM _DXVA_EncryptProtocolHeader}
  DXVA_EncryptProtocolHeader    = _DXVA_EncryptProtocolHeader;
  {$EXTERNALSYM DXVA_EncryptProtocolHeader}



  PDXVA_ConfigPictureDecode = ^DXVA_ConfigPictureDecode;
  LPDXVA_ConfigPictureDecode = ^DXVA_ConfigPictureDecode;
  _DXVA_ConfigPictureDecode = record
    // Operation Indicated
    dwFunction: DXVA_ConfigQueryOrReplyFunc;
    // Alignment
    dwReservedBits                 : array[0..2] of DWORD;
    // Encryption GUIDs
    guidConfigBitstreamEncryption  : TGUID;
    guidConfigMBcontrolEncryption  : TGUID;
    guidConfigResidDiffEncryption  : TGUID;
    // Bitstream Processing Indicator
    bConfigBitstreamRaw            : BYTE;
    // Macroblock Control Config
    bConfigMBcontrolRasterOrder    : BYTE;
    // Host Resid Diff Config
    bConfigResidDiffHost           : BYTE;
    bConfigSpatialResid8           : BYTE;
    bConfigResid8Subtraction       : BYTE;
    bConfigSpatialHost8or9Clipping : BYTE;
    bConfigSpatialResidInterleaved : BYTE;
    bConfigIntraResidUnsigned      : BYTE;
    // Accelerator Resid Diff Config
    bConfigResidDiffAccelerator    : BYTE;
    bConfigHostInverseScan         : BYTE;
    bConfigSpecificIDCT            : BYTE;
    bConfig4GroupedCoefs           : BYTE;
  end;
  {$EXTERNALSYM _DXVA_ConfigPictureDecode}
  DXVA_ConfigPictureDecode = _DXVA_ConfigPictureDecode;
  {$EXTERNALSYM DXVA_ConfigPictureDecode}


  // Picture Decoding Parameters
  PDXVA_PictureParameters = ^DXVA_PictureParameters;
  LPDXVA_PictureParameters = ^DXVA_PictureParameters;
  _DXVA_PictureParameters = record
    wDecodedPictureIndex          : WORD;
    wDeblockedPictureIndex        : WORD;
    wForwardRefPictureIndex       : WORD;
    wBackwardRefPictureIndex      : WORD;
    wPicWidthInMBminus1           : WORD;
    wPicHeightInMBminus1          : WORD;
    bMacroblockWidthMinus1        : BYTE;
    bMacroblockHeightMinus1       : BYTE;
    bBlockWidthMinus1             : BYTE;
    bBlockHeightMinus1            : BYTE;
    bBPPminus1                    : BYTE;
    bPicStructure                 : BYTE;
    bSecondField                  : BYTE;
    bPicIntra                     : BYTE;
    bPicBackwardPrediction        : BYTE;
    bBidirectionalAveragingMode   : BYTE;
    bMVprecisionAndChromaRelation : BYTE;
    bChromaFormat                 : BYTE;
    bPicScanFixed                 : BYTE;
    bPicScanMethod                : BYTE;
    bPicReadbackRequests          : BYTE;
    bRcontrol                     : BYTE;
    bPicSpatialResid8             : BYTE;
    bPicOverflowBlocks            : BYTE;
    bPicExtrapolation             : BYTE;
    bPicDeblocked                 : BYTE;
    bPicDeblockConfined           : BYTE;
    bPic4MVallowed                : BYTE;
    bPicOBMC                      : BYTE;
    bPicBinPB                     : BYTE;
    bMV_RPS                       : BYTE;
    bReservedBits                 : BYTE;
    wBitstreamFcodes              : WORD;
    wBitstreamPCEelements         : WORD;
    bBitstreamConcealmentNeed     : BYTE;
    bBitstreamConcealmentMethod   : BYTE;
  end;
  {$EXTERNALSYM _DXVA_PictureParameters}
  DXVA_PictureParameters = _DXVA_PictureParameters;
  {$EXTERNALSYM DXVA_PictureParameters}


  // Picture Resampling
  PDXVA_PicResample = ^DXVA_PicResample;
  LPDXVA_PicResample = ^DXVA_PicResample;
  _DXVA_PicResample = record
    wPicResampleSourcePicIndex  : WORD;
    wPicResampleDestPicIndex    : WORD;
    wPicResampleRcontrol        : WORD;
    bPicResampleExtrapWidth     : BYTE;
    bPicResampleExtrapHeight    : BYTE;
    dwPicResampleSourceWidth    : DWORD;
    dwPicResampleSourceHeight   : DWORD;
    dwPicResampleDestWidth      : DWORD;
    dwPicResampleDestHeight     : DWORD;
    dwPicResampleFullDestWidth  : DWORD;
    dwPicResampleFullDestHeight : DWORD;
  end;
  {$EXTERNALSYM _DXVA_PicResample}
  DXVA_PicResample = _DXVA_PicResample;
  {$EXTERNALSYM DXVA_PicResample}


const

  DXVA_CHROMA_FORMAT_420 = 1;
  {$EXTERNALSYM DXVA_CHROMA_FORMAT_420}
  DXVA_CHROMA_FORMAT_422 = 2;
  {$EXTERNALSYM DXVA_CHROMA_FORMAT_422}
  DXVA_CHROMA_FORMAT_444 = 3;
  {$EXTERNALSYM DXVA_CHROMA_FORMAT_444}

  DXVA_PICTURE_STRUCTURE_TOP_FIELD    = 1;
  {$EXTERNALSYM DXVA_PICTURE_STRUCTURE_TOP_FIELD}
  DXVA_PICTURE_STRUCTURE_BOTTOM_FIELD = 2;
  {$EXTERNALSYM DXVA_PICTURE_STRUCTURE_BOTTOM_FIELD}
  DXVA_PICTURE_STRUCTURE_FRAME        = 3;
  {$EXTERNALSYM DXVA_PICTURE_STRUCTURE_FRAME}

  DXVA_BIDIRECTIONAL_AVERAGING_MPEG2_ROUND = 0;
  {$EXTERNALSYM DXVA_BIDIRECTIONAL_AVERAGING_MPEG2_ROUND}
  DXVA_BIDIRECTIONAL_AVERAGING_H263_TRUNC  = 1;
  {$EXTERNALSYM DXVA_BIDIRECTIONAL_AVERAGING_H263_TRUNC}

  DXVA_MV_PRECISION_AND_CHROMA_RELATION_MPEG2 = 0;
  {$EXTERNALSYM DXVA_MV_PRECISION_AND_CHROMA_RELATION_MPEG2}
  DXVA_MV_PRECISION_AND_CHROMA_RELATION_H263  = 1;
  {$EXTERNALSYM DXVA_MV_PRECISION_AND_CHROMA_RELATION_H263}
  DXVA_MV_PRECISION_AND_CHROMA_RELATION_H261  = 2;
  {$EXTERNALSYM DXVA_MV_PRECISION_AND_CHROMA_RELATION_H261}

  DXVA_SCAN_METHOD_ZIG_ZAG              = 0;
  {$EXTERNALSYM DXVA_SCAN_METHOD_ZIG_ZAG}
  DXVA_SCAN_METHOD_ALTERNATE_VERTICAL   = 1;
  {$EXTERNALSYM DXVA_SCAN_METHOD_ALTERNATE_VERTICAL}
  DXVA_SCAN_METHOD_ALTERNATE_HORIZONTAL = 2;
  {$EXTERNALSYM DXVA_SCAN_METHOD_ALTERNATE_HORIZONTAL}
  DXVA_SCAN_METHOD_ARBITRARY            = 3;
  {$EXTERNALSYM DXVA_SCAN_METHOD_ARBITRARY}

  DXVA_BITSTREAM_CONCEALMENT_NEED_UNLIKELY = 0;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_NEED_UNLIKELY}
  DXVA_BITSTREAM_CONCEALMENT_NEED_MILD     = 1;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_NEED_MILD}
  DXVA_BITSTREAM_CONCEALMENT_NEED_LIKELY   = 2;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_NEED_LIKELY}
  DXVA_BITSTREAM_CONCEALMENT_NEED_SEVERE   = 3;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_NEED_SEVERE}

  DXVA_BITSTREAM_CONCEALMENT_METHOD_UNSPECIFIED = 0;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_METHOD_UNSPECIFIED}
  DXVA_BITSTREAM_CONCEALMENT_METHOD_INTRA       = 1;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_METHOD_INTRA}
  DXVA_BITSTREAM_CONCEALMENT_METHOD_FORWARD     = 2;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_METHOD_FORWARD}
  DXVA_BITSTREAM_CONCEALMENT_METHOD_BACKWARD    = 3;
  {$EXTERNALSYM DXVA_BITSTREAM_CONCEALMENT_METHOD_BACKWARD}


type

  // Buffer Description Data
  PDXVA_BufferDescription = ^DXVA_BufferDescription;
  {$EXTERNALSYM _DXVA_BufferDescription}
  LPDXVA_BufferDescription = ^DXVA_BufferDescription;
  _DXVA_BufferDescription = record
    dwTypeIndex      : DWORD;
    dwBufferIndex    : DWORD;
    dwDataOffset     : DWORD;
    dwDataSize       : DWORD;
    dwFirstMBaddress : DWORD;
    dwNumMBsInBuffer : DWORD;
    dwWidth          : DWORD;
    dwHeight         : DWORD;
    dwStride         : DWORD;
    dwReservedBits   : DWORD;
  end;
  {$EXTERNALSYM _DXVA_BufferDescription}
  DXVA_BufferDescription = _DXVA_BufferDescription;
  {$EXTERNALSYM DXVA_BufferDescription}


  // Off-Host IDCT Coefficient Data Structures

  PDXVA_TCoef4Group = ^DXVA_TCoef4Group;
  LPDXVA_TCoef4Group = ^DXVA_TCoef4Group;
  _DXVA_TCoef4Group = record
    TCoefIDX   : array [0..3] of BYTE;
    TCoefValue : array [0..3] of SHORT;
  end;
  {$EXTERNALSYM _DXVA_TCoef4Group}
  DXVA_TCoef4Group = _DXVA_TCoef4Group;
  {$EXTERNALSYM DXVA_TCoef4Group}

// TCoefSingle struct with macro's

  PDXVA_TCoefSingle = ^DXVA_TCoefSingle;
  LPDXVA_TCoefSingle = ^DXVA_TCoefSingle;
  _DXVA_TCoefSingle = record
    wIndexWithEOB : WORD;
    TCoefValue    : smallint;
  end;
  {$EXTERNALSYM _DXVA_TCoefSingle}
  DXVA_TCoefSingle = _DXVA_TCoefSingle;
  {$EXTERNALSYM DXVA_TCoefSingle}

// Macros for Reading EOB and Index Values
{
#define readDXVA_TCoefSingleIDX(ptr) ((ptr)->wIndexWithEOB >> 1)
#define readDXVA_TCoefSingleEOB(ptr) ((ptr)->wIndexWithEOB & 1)
}
// Macro for Writing EOB and Index Values
{
#define writeDXVA_TCoefSingleIndexWithEOB(ptr, idx, eob) ((ptr)->wIndexWithEOB = ((idx) << 1) | (eob))
#define setDXVA_TCoefSingleIDX(ptr, idx) ((ptr)->wIndexWithEOB |= ((idx) << 1))
#define setDXVA_TCoefSingleEOB(ptr)      ((ptr)->wIndexWithEOB |= 1)
}

// End TCoefSingle struct


const

  // Spatial-Domain Residual Difference Blocks

  DXVA_USUAL_BLOCK_WIDTH  = 8;
  {$EXTERNALSYM DXVA_USUAL_BLOCK_WIDTH}
  DXVA_USUAL_BLOCK_HEIGHT = 8;
  {$EXTERNALSYM DXVA_USUAL_BLOCK_HEIGHT}
  DXVA_USUAL_BLOCK_SIZE   = (DXVA_USUAL_BLOCK_WIDTH * DXVA_USUAL_BLOCK_HEIGHT);
  {$EXTERNALSYM DXVA_USUAL_BLOCK_SIZE}


type

  DXVA_Sample16 = array[0..DXVA_USUAL_BLOCK_SIZE - 1] of SmallInt;
  {$EXTERNALSYM DXVA_Sample16}
  DXVA_Sample8  = array[0..DXVA_USUAL_BLOCK_SIZE - 1] of ShortInt;
  {$EXTERNALSYM DXVA_Sample8}

  // Deblocking Filter Control Structure
  DXVA_DeblockingEdgeControl = {$IFDEF TYPE_IDENTITY} type {$ENDIF} BYTE;
  {$EXTERNALSYM DXVA_DeblockingEdgeControl}
  LPDXVA_DeblockingEdgeControl = ^DXVA_DeblockingEdgeControl;
  {$EXTERNALSYM LPDXVA_DeblockingEdgeControl}


(* Macros for Reading STRENGTH and FilterOn *)
(*
#define readDXVA_EdgeFilterStrength(ptr) ((*(ptr)) >> 1)
#define readDXVA_EdgeFilterOn(ptr)       ((*(ptr)) & 1)

/* Macro for Writing STRENGTH and FilterOn */

#define writeDXVA_DeblockingEdgeControl(ptr, str, fon) ((*(ptr)) = ((str) << 1) | (fon))
#define setDXVA_EdgeFilterStrength(ptr, str)           ((*(ptr)) |= ((str) << 1))
#define setDXVA_EdgeFilterOn(ptr)                      ((*(ptr)) |= 1)
*)


  // Macroblock Control Command Data Structures

  PDXVA_MVvalue = ^DXVA_MVvalue;
  {$EXTERNALSYM _DXVA_MVvalue}
  LPDXVA_MVvalue = ^DXVA_MVvalue;
  _DXVA_MVvalue = record
    horz: SmallInt;
    vert: SmallInt;
   end;
  {$EXTERNALSYM _DXVA_MVvalue}
  DXVA_MVvalue = _DXVA_MVvalue;
  {$EXTERNALSYM DXVA_MVvalue}


  // Inverse Quantization Matrices
  PDXVA_QmatrixData = ^_DXVA_QmatrixData;
  LPDXVA_QmatrixData = ^_DXVA_QmatrixData;
  PDXVAQmatrixData = ^_DXVA_QmatrixData;
  _DXVA_QmatrixData = record
    bNewQmatrix : array [0..3] of BYTE;
    // intra Y, inter Y, intra chroma, inter chroma
    Qmatrix : array [0..3, 0..(DXVA_USUAL_BLOCK_WIDTH*DXVA_USUAL_BLOCK_HEIGHT) - 1] of WORD;
  end;
  {$EXTERNALSYM _DXVA_QmatrixData}
  DXVA_QmatrixData = _DXVA_QmatrixData;
  {$EXTERNALSYM DXVA_QmatrixData}



  // Slice Control Buffer Data
  PDXVA_SliceInfo = ^_DXVA_SliceInfo;
  LPDXVA_SliceInfo = ^_DXVA_SliceInfo;
  _DXVA_SliceInfo = record
    wHorizontalPosition : WORD;
    wVerticalPosition   : WORD;
    dwSliceBitsInBuffer : DWORD;
    dwSliceDataLocation : DWORD;
    bStartCodeBitOffset : BYTE;
    bReservedBits       : BYTE;
    wMBbitOffset        : WORD;
    wNumberMBsInSlice   : WORD;
    wQuantizerScaleCode : WORD;
    wBadSliceChopping   : WORD;
  end;
  {$EXTERNALSYM _DXVA_SliceInfo}
  DXVA_SliceInfo = _DXVA_SliceInfo;
  {$EXTERNALSYM DXVA_SliceInfo}


const

  DXVA_NumMV_OBMC_off_BinPBwith4MV_off = 4;
  {$EXTERNALSYM DXVA_NumMV_OBMC_off_BinPBwith4MV_off}
  DXVA_NumMV_OBMC_off_BinPBwith4MV_on  = (4 + 1);
  {$EXTERNALSYM DXVA_NumMV_OBMC_off_BinPBwith4MV_on}
  DXVA_NumMV_OBMC_on__BinPB_off        = (10);
  {$EXTERNALSYM DXVA_NumMV_OBMC_on__BinPB_off}
  DXVA_NumMV_OBMC_on__BinPB_on         = (11); // not current standards
  {$EXTERNALSYM DXVA_NumMV_OBMC_on__BinPB_on}


  DXVA_NumBlocksPerMB_420 = (4 + 2 + 0);
  {$EXTERNALSYM DXVA_NumBlocksPerMB_420}
  DXVA_NumBlocksPerMB_422 = (4 + 2 + 2);
  {$EXTERNALSYM DXVA_NumBlocksPerMB_422}
  DXVA_NumBlocksPerMB_444 = (4 + 4 + 4);
  {$EXTERNALSYM DXVA_NumBlocksPerMB_444}


type

  // Basic form for I pictures
  // Host Residual Differences
  PDXVA_MBctrl_I_HostResidDiff_1 = ^_DXVA_MBctrl_I_HostResidDiff_1;
  _DXVA_MBctrl_I_HostResidDiff_1 = record
    wMBaddress      : WORD;
    wMBtype         : WORD;
    dwMB_SNL        : DWORD;
    wPatternCode    : WORD;
    wPC_Overflow    : WORD;
    // zero if not overflow format
    dwReservedBits2 : DWORD;
  end;
  {$EXTERNALSYM _DXVA_MBctrl_I_HostResidDiff_1}
  DXVA_MBctrl_I_HostResidDiff_1 = _DXVA_MBctrl_I_HostResidDiff_1;
  {$EXTERNALSYM DXVA_MBctrl_I_HostResidDiff_1}

  // Basic form for I pictures
  // Off-Host IDCT, 4:2:0 sampling
  PDXVA_MBctrl_I_OffHostIDCT_1 = ^_DXVA_MBctrl_I_OffHostIDCT_1;
  _DXVA_MBctrl_I_OffHostIDCT_1 = record
    wMBaddress   : WORD;
    wMBtype      : WORD;
    dwMB_SNL     : DWORD;
    wPatternCode : WORD;
    bNumCoef     : array [0..DXVA_NumBlocksPerMB_420 - 1] of BYTE;
  end;
  {$EXTERNALSYM _DXVA_MBctrl_I_OffHostIDCT_1}
  DXVA_MBctrl_I_OffHostIDCT_1 = _DXVA_MBctrl_I_OffHostIDCT_1;
  {$EXTERNALSYM DXVA_MBctrl_I_OffHostIDCT_1}


  // Basic form for P and B pictures
  // Should also be used for concealment MVs in MPEG-2 I pictures
  // Without OBMC, without BinPB and 4MV together, without MV RPS
  // Host Residual Differences
  PDXVA_MBctrl_P_HostResidDiff_1 = ^_DXVA_MBctrl_P_HostResidDiff_1;
  _DXVA_MBctrl_P_HostResidDiff_1 = record
    wMBaddress   : WORD;
    wMBtype      : WORD;
    dwMB_SNL     : DWORD;
    wPatternCode : WORD;
    wPC_Overflow : WORD;
    // zero if not overflow format
    dwReservedBits2 : DWORD;
    MVector : array [0..DXVA_NumMV_OBMC_off_BinPBwith4MV_off-1] of DXVA_MVvalue;
  end;
  {$EXTERNALSYM _DXVA_MBctrl_P_HostResidDiff_1}
  DXVA_MBctrl_P_HostResidDiff_1 = _DXVA_MBctrl_P_HostResidDiff_1;
  {$EXTERNALSYM DXVA_MBctrl_P_HostResidDiff_1}


  // Basic form for P and B pictures
  // Without OBMC, without BinPB and 4MV together, without MV RPS
  // Off-Host IDCT, 4:2:0 sampling
  PDXVA_MBctrl_P_OffHostIDCT_1 = ^_DXVA_MBctrl_P_OffHostIDCT_1;
  _DXVA_MBctrl_P_OffHostIDCT_1 = record
    wMBaddress   : WORD;
    wMBtype      : WORD;
    dwMB_SNL     : DWORD;
    wPatternCode : WORD;
    bNumCoef : array [0..DXVA_NumBlocksPerMB_420 - 1] of BYTE;
    MVector  : array [0..DXVA_NumMV_OBMC_off_BinPBwith4MV_off - 1] of DXVA_MVvalue;
  end;
  {$EXTERNALSYM _DXVA_MBctrl_P_OffHostIDCT_1}
  DXVA_MBctrl_P_OffHostIDCT_1 = _DXVA_MBctrl_P_OffHostIDCT_1;
  {$EXTERNALSYM DXVA_MBctrl_P_OffHostIDCT_1}

  // How to load alpha blending graphic data
  LPDXVA_ConfigAlphaLoad = ^_DXVA_ConfigAlphaLoad;
  _DXVA_ConfigAlphaLoad = record
    // Operation Indicated
    dwFunction: DXVA_ConfigQueryOrReplyFunc ;
    // Alignment
    dwReservedBits: array[0..2] of DWORD;
    bConfigDataType: BYTE;
  end;
  {$EXTERNALSYM _DXVA_ConfigAlphaLoad}
  DXVA_ConfigAlphaLoad = _DXVA_ConfigAlphaLoad;
  {$EXTERNALSYM DXVA_ConfigAlphaLoad}


const

  DXVA_CONFIG_DATA_TYPE_IA44 = 0;
  {$EXTERNALSYM DXVA_CONFIG_DATA_TYPE_IA44}
  DXVA_CONFIG_DATA_TYPE_AI44 = 1;
  {$EXTERNALSYM DXVA_CONFIG_DATA_TYPE_AI44}
  DXVA_CONFIG_DATA_TYPE_DPXD = 2;
  {$EXTERNALSYM DXVA_CONFIG_DATA_TYPE_DPXD}
  DXVA_CONFIG_DATA_TYPE_AYUV = 3;
  {$EXTERNALSYM DXVA_CONFIG_DATA_TYPE_AYUV}



// How to combine alpha blending graphic data
type

  LPDXVA_ConfigAlphaCombine = ^_DXVA_ConfigAlphaCombine;
  _DXVA_ConfigAlphaCombine = record
    // Operation Indicated
    dwFunction: DXVA_ConfigQueryOrReplyFunc;
    // Alignment
    dwReservedBits               : array[0..2] of DWORD;
    bConfigBlendType             : BYTE;
    bConfigPictureResizing       : BYTE;
    bConfigOnlyUsePicDestRectArea: BYTE;
    bConfigGraphicResizing       : BYTE;
    bConfigWholePlaneAlpha       : BYTE;
  end;
  {$EXTERNALSYM _DXVA_ConfigAlphaCombine}
  DXVA_ConfigAlphaCombine = _DXVA_ConfigAlphaCombine;
  {$EXTERNALSYM DXVA_ConfigAlphaCombine}


const

  DXVA_CONFIG_BLEND_TYPE_FRONT_BUFFER  = 0;
  {$EXTERNALSYM DXVA_CONFIG_BLEND_TYPE_FRONT_BUFFER}
  DXVA_CONFIG_BLEND_TYPE_BACK_HARDWARE = 1;
  {$EXTERNALSYM DXVA_CONFIG_BLEND_TYPE_BACK_HARDWARE}


// AYUV sample for 16-entry YUV palette or graphic surface
type

  LPDXVA_AYUVsample2 = ^_DXVA_AYUVsample2;
  _DXVA_AYUVsample2 = record
    bCrValue      : BYTE;
    bCbValue      : BYTE;
    bY_Value      : BYTE;
    bSampleAlpha8 : BYTE;
  end;
  {$EXTERNALSYM _DXVA_AYUVsample2}
  DXVA_AYUVsample2 = _DXVA_AYUVsample2;
  {$EXTERNALSYM DXVA_AYUVsample2}

  // Macros for IA44 alpha blending surface samples
  DXVA_AI44sample   = {$IFDEF TYPE_IDENTITY} type {$ENDIF} BYTE;
  {$EXTERNALSYM DXVA_AI44sample}
  LPDXVA_AI44sample = ^DXVA_AI44sample;
  {$EXTERNALSYM LPDXVA_AI44sample}



//#define readDXVA_AI44index(ptr)  ((*(ptr)) & 0x0F)
//#define readDXVA_AI44alpha(ptr) (((*(ptr)) & 0xF0) >> 4)
//#define writeDXVA_AI44(ptr, idx, alpha) ((*(ptr)) = (((alpha) << 4) | (idx)))
//#define setDXVA_AI44index(ptr, idx)    ((*(ptr)) |= (idx))
//#define setDXVA_AI44alpha(ptr, alpha)  ((*(ptr)) |= ((alpha) << 4))


  (* Highlight data structure *)
  LPDXVA_Highlight = ^DXVA_Highlight;
  _DXVA_Highlight = record
    wHighlightActive  : WORD;
    wHighlightIndices : WORD;
    wHighlightAlphas  : WORD;
    HighlightRect     : TRect;
  end;
  {$EXTERNALSYM _DXVA_Highlight}
  DXVA_Highlight = _DXVA_Highlight;
  {$EXTERNALSYM DXVA_Highlight}


  LPDXVA_DPXD = ^DXVA_DPXD;
  DXVA_DPXD = type BYTE;
  {$EXTERNALSYM DXVA_DPXD}
  LPDXVA_DCCMD = ^DXVA_DCCMD;
  DXVA_DCCMD = type WORD;
  {$EXTERNALSYM DXVA_DCCMD}


  // Alpha blend combination
  PDXVA_BlendCombination = ^_DXVA_BlendCombination;
  LPDXVA_BlendCombination = ^_DXVA_BlendCombination;
  _DXVA_BlendCombination = record
    wPictureSourceIndex      : WORD;
    wBlendedDestinationIndex : WORD;
    PictureSourceRect16thPel : TRECT;
    PictureDestinationRect   : TRECT;
    GraphicSourceRect        : TRECT;
    GraphicDestinationRect   : TRECT;
    wBlendDelay              : WORD;
    bBlendOn                 : BYTE;
    bWholePlaneAlpha         : BYTE;
    OutsideYUVcolor          : DXVA_AYUVsample2;
  end;
  {$EXTERNALSYM _DXVA_BlendCombination}
  DXVA_BlendCombination = _DXVA_BlendCombination;
  {$EXTERNALSYM DXVA_BlendCombination}



  (* H.264/AVC-specific structures *)


  (* H.264/AVC picture entry data structure *)
  LPDXVA_PicEntry_H264 = ^_DXVA_PicEntry_H264;
  _DXVA_PicEntry_H264 = record
    private
      bPicEntry: byte;
      function ReadBytes(const aIndex: integer): Byte;
      procedure WriteBytes(const aIndex: integer; const aValue: Byte);

    public
      property Index7Bits: Byte index $07 read ReadBytes write WriteBytes;      // 7 bits at offset 0
      property AssociatedFlag: Byte index $71 read ReadBytes write WriteBytes;  // 1 bit at offset 7 ($7)
  end;
  {$EXTERNALSYM _DXVA_PicEntry_H264}
  DXVA_PicEntry_H264 = _DXVA_PicEntry_H264;
  {$EXTERNALSYM DXVA_PicEntry_H264}



  (* H.264/AVC picture parameters structure *)
  LPDXVA_PicParams_H264 = ^_DXVA_PicParams_H264;
  _DXVA_PicParams_H264 = record
    wFrameWidthInMbsMinus1: USHORT;
    wFrameHeightInMbsMinus1: USHORT;
    CurrPic: DXVA_PicEntry_H264;  // flag is bot field flag
    num_ref_frames: UCHAR;

    union : record
      case wBitFields: USHORT of                     // USHORT = Unsigned 16-bit integer

        0: (field_pic_flag: USHORT);                 // 1
        1: (MbaffFrameFlag: USHORT);                 // 1
        2: (residual_colour_transform_flag: USHORT); // 1
        3: (sp_for_switch_flag: USHORT);             // 1
        4: (chroma_format_idc: USHORT);              // 2
        5: (RefPicFlag: USHORT);                     // 1
        6: (constrained_intra_pred_flag: USHORT);    // 1

        7: (weighted_pred_flag: USHORT);             // 1
        8: (weighted_bipred_idc: USHORT);            // 2
        9: (MbsConsecutiveFlag: USHORT);             // 1
        10: (frame_mbs_only_flag: USHORT);           // 1
        11: (transform_8x8_mode_flag: USHORT);       // 1
        12: (MinLumaBipredSize8x8Flag: USHORT);      // 1
        13: (IntraPicFlag: USHORT);                  // 1
      end;                                           // = 16 bits
  {$EXTERNALSYM _DXVA_PicParams_H264}


      bit_depth_luma_minus8: UCHAR;
      bit_depth_chroma_minus8: UCHAR;

      Reserved16Bits: USHORT;
      StatusReportFeedbackNumber: UINT;

      RefFrameList: array [0..15] of DXVA_PicEntry_H264;  // flag LT
      CurrFieldOrderCnt: array [0..1] of Integer;
      FieldOrderCntList: array [0..15, 0..1] of Integer;

      pic_init_qs_minus26: AnsiChar;
      chroma_qp_index_offset: AnsiChar;    // also used for QScb */
      second_chroma_qp_index_offset: AnsiChar;  // also for QScr */
      ContinuationFlag: UCHAR;

      (* remainder for parsing *)
      pic_init_qp_minus26: AnsiChar;
      num_ref_idx_l0_active_minus1: UCHAR;
      num_ref_idx_l1_active_minus1: UCHAR;
      Reserved8BitsA: UCHAR;

      FrameNumList: array [0..15] of USHORT;
      UsedForReferenceFlags: UINT;
      NonExistingFrameFlags: USHORT;
      frame_num: USHORT;

      log2_max_frame_num_minus4: UCHAR;
      pic_order_cnt_type: UCHAR;
      log2_max_pic_order_cnt_lsb_minus4: UCHAR;
      delta_pic_order_always_zero_flag: UCHAR;

      direct_8x8_inference_flag: UCHAR;
      entropy_coding_mode_flag: UCHAR;
      pic_order_present_flag: UCHAR;
      num_slice_groups_minus1: UCHAR;

      slice_group_map_type: UCHAR;
      deblocking_filter_control_present_flag: UCHAR;
      redundant_pic_cnt_present_flag: UCHAR;
      Reserved8BitsB: UCHAR;

      slice_group_change_rate_minus1: USHORT;

      SliceGroupMap: array [0..809] of UCHAR;  // 4b/sgmu, Size BT.601

  end;
  DXVA_PicParams_H264 = _DXVA_PicParams_H264;
  {$EXTERNALSYM DXVA_PicParams_H264}



  //#pragma pack(push, 16)
  {$ALIGN 16}

  LPDXVA_MBctrl_I_HostResidDiff_1 = ^DXVA_MBctrl_I_HostResidDiff_1;
  {$EXTERNALSYM LPDXVA_MBctrl_I_HostResidDiff_1}
  LPDXVA_MBctrl_I_OffHostIDCT_1   = ^DXVA_MBctrl_I_OffHostIDCT_1;
  {$EXTERNALSYM LPDXVA_MBctrl_I_OffHostIDCT_1}
  LPDXVA_MBctrl_P_HostResidDiff_1 = ^DXVA_MBctrl_P_HostResidDiff_1;
  {$EXTERNALSYM LPDXVA_MBctrl_P_HostResidDiff_1}
  LPDXVA_MBctrl_P_OffHostIDCT_1   = ^DXVA_MBctrl_P_OffHostIDCT_1;
  {$EXTERNALSYM LPDXVA_MBctrl_P_OffHostIDCT_1}


  //#pragma pack(pop)
  {$ALIGN 8}  //Set back to default

//
// Other forms of pictures are constructed in the obvious way
// from the above by adjusting the number of residual difference
// blocks, the number of motion vectors per macroblock, etc.
//


function readDXVA_MBskipsFollowing(dwMB_SNL: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MBskipsFollowing}

function readDXVA_MBdataLocation(dwMB_SNL: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MBdataLocation}

function writeDXVA_MB_SNL(dwMB_SNL: DWord ; skips: DWord; dloc: DWord): DWORD; inline;
{$EXTERNALSYM writeDXVA_MB_SNL}

function setDXVA_MBskipsFollowing(dwMB_SNL: DWord; skips: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MBskipsFollowing}

function setDXVA_MBdataLocation(dwMB_SNL: DWord; dloc: Dword): DWORD; inline;
{$EXTERNALSYM setDXVA_MBdataLocation}

function readDXVA_MvertFieldSel_3(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MvertFieldSel_3}

function readDXVA_MvertFieldSel_2(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MvertFieldSel_2}

function readDXVA_MvertFieldSel_1(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MvertFieldSel_1}

function readDXVA_MvertFieldSel_0(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MvertFieldSel_0}

function readDXVA_ReservedBits(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_ReservedBits}

function readDXVA_HostResidDiff(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_HostResidDiff}

function readDXVA_MotionType(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MotionType}

function readDXVA_MBscanMethod(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MBscanMethod}

function readDXVA_FieldResidual(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_FieldResidual}

function readDXVA_H261LoopFilter(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_H261LoopFilter}

function readDXVA_Motion4MV(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Motion4MV}

function readDXVA_MotionBackward(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MotionBackward}

function readDXVA_MotionForward(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_MotionForward}

function readDXVA_IntraMacroblock(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_IntraMacroblock}

function setDXVA_MvertFieldSel_3(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MvertFieldSel_3}

function setDXVA_MvertFieldSel_2(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MvertFieldSel_2}

function setDXVA_MvertFieldSel_1(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MvertFieldSel_1}

function setDXVA_MvertFieldSel_0(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MvertFieldSel_0}

function setDXVA_ReservedBits(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_ReservedBits}

function setDXVA_HostResidDiff(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_HostResidDiff}

function setDXVA_MotionType(wMBtype: DWord; value: DWord ): DWORD; inline;
{$EXTERNALSYM setDXVA_MotionType}

function setDXVA_MBscanMethod(wMBtype: DWord; value: DWord ): DWORD; inline;
{$EXTERNALSYM setDXVA_MBscanMethod}

function setDXVA_FieldResidual(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_FieldResidual}

function setDXVA_H261LoopFilter(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_H261LoopFilter}

function setDXVA_Motion4MV(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_Motion4MV}

function setDXVA_MotionBackward(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MotionBackward}

function setDXVA_MotionForward(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_MotionForward}

function setDXVA_IntraMacroblock(wMBtype: DWord): DWORD; inline;
{$EXTERNALSYM setDXVA_IntraMacroblock}

function readDXVA_Y___0coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___0coded}

function readDXVA_Y___1coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___1coded}

function readDXVA_Y___2coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___2coded}

function readDXVA_Y___3coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___3coded}

function readDXVA_Cb__4coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__4coded}

function readDXVA_Cr__5coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr__5coded}

function readDXVA_Cb__6coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__6coded}

function readDXVA_Cr__7coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr__7coded}

function readDXVA_Cb__8coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__8coded}

function readDXVA_Cb__9coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__9coded}

function readDXVA_Cr_10coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr_10coded}

function readDXVA_Cr_11coded(wPatternCode: Dword): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr_11coded}

function readDXVA_Y___0oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___0oflow}

function readDXVA_Y___1oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___1oflow}

function readDXVA_Y___2oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___2oflow}

function readDXVA_Y___3oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Y___3oflow}

function readDXVA_Cb__4oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__4oflow}

function readDXVA_Cr__5oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr__5oflow}

function readDXVA_Cb__6oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__6oflow}

function readDXVA_Cr__7oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr__7oflow}

function readDXVA_Cb__8oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__8oflow}

function readDXVA_Cb__9oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cb__9oflow}

function readDXVA_Cr_10oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr_10oflow}

function readDXVA_Cr_11oflow(wPC_Overflow: DWord): DWORD; inline;
{$EXTERNALSYM readDXVA_Cr_11oflow}


// -------------------------------------------------------------------------
//
// D3DFORMAT describes a pixel memory layout, DXVA sample format contains
// additional information that describes how the pixels should be interpreted.
//
// -------------------------------------------------------------------------

const

  DXVA_SampleFormatMask = $FF;   // 8 bits used for DXVA Sample format
  {$EXTERNALSYM DXVA_SampleFormatMask}


type

  PDXVA_SampleFormat = ^DXVA_SampleFormat;
  DXVA_SampleFormat = (
    DXVA_SampleUnknown,
    DXVA_SamplePreviousFrame,
    DXVA_SampleProgressiveFrame,
    DXVA_SampleFieldInterleavedEvenFirst,
    DXVA_SampleFieldInterleavedOddFirst,
    DXVA_SampleFieldSingleEven,
    DXVA_SampleFieldSingleOdd,
    DXVA_SampleSubStream
  );
  {$EXTERNALSYM DXVA_SampleFormat}


const

  DXVA_ExtColorData_ShiftBase  = 8;
  {$EXTERNALSYM DXVA_ExtColorData_ShiftBase}
  DXVA_VideoTransFuncShift = (DXVA_ExtColorData_ShiftBase + 19);
  {$EXTERNALSYM DXVA_VideoTransFuncShift}
  DXVA_VideoTransFuncMask = (not((not 0) shl 5)) shl ( DXVA_VideoTransFuncShift - DXVA_ExtColorData_ShiftBase);
  {$EXTERNALSYM DXVA_VideoTransFuncMask}


type

  PDXVA_VideoTransferFunction = ^DXVA_VideoTransferFunction;
  DXVA_VideoTransferFunction = (
    DXVA_VideoTransFunc_Unknown,
    DXVA_VideoTransFunc_10,
    DXVA_VideoTransFunc_18,
    DXVA_VideoTransFunc_20,
    DXVA_VideoTransFunc_22,
    DXVA_VideoTransFunc_22_709,
    DXVA_VideoTransFunc_22_240M,
    DXVA_VideoTransFunc_22_8bit_sRGB,
    DXVA_VideoTransFunc_28
  );
  {$EXTERNALSYM DXVA_VideoTransferFunction}

const

  DXVA_VideoPrimariesShift = (DXVA_ExtColorData_ShiftBase + 14);
  {$EXTERNALSYM DXVA_VideoPrimariesShift}
  DXVA_VideoPrimariesMask = (not((not 0) shl 5)) shl ( DXVA_VideoPrimariesShift - DXVA_ExtColorData_ShiftBase);
  {$EXTERNALSYM DXVA_VideoPrimariesMask}


type

  PDXVA_VideoPrimaries = ^DXVA_VideoPrimaries;
  DXVA_VideoPrimaries = (
    DXVA_VideoPrimaries_Unknown,
    DXVA_VideoPrimaries_reserved,
    DXVA_VideoPrimaries_BT709,
    DXVA_VideoPrimaries_BT470_2_SysM,
    DXVA_VideoPrimaries_BT470_2_SysBG,
    DXVA_VideoPrimaries_SMPTE170M,
    DXVA_VideoPrimaries_SMPTE240M,
    DXVA_VideoPrimaries_EBU3213,
    DXVA_VideoPrimaries_SMPTE_C
  );
  {$EXTERNALSYM DXVA_VideoPrimaries}

const

  // DXVA_VideoLightingShift = (DXVA_ExtColorData_ShiftBase + 10),
  DXVA_VideoLightingShift = (DXVA_ExtColorData_ShiftBase + 10);
  {$EXTERNALSYM DXVA_VideoLightingShift}
  // DXVA_VideoLightingMask = DXVAColorMask(4, DXVA_VideoLightingShift),
  DXVA_VideoLightingMask = (not((not 0) shl 4)) shl ( DXVA_VideoLightingShift - DXVA_ExtColorData_ShiftBase);
  {$EXTERNALSYM DXVA_VideoLightingMask}


type

  PDXVA_VideoLighting = ^DXVA_VideoLighting;
  DXVA_VideoLighting = (
    DXVA_VideoLighting_Unknown,
    DXVA_VideoLighting_bright,
    DXVA_VideoLighting_office,
    DXVA_VideoLighting_dim,
    DXVA_VideoLighting_dark
  );
  {$EXTERNALSYM DXVA_VideoLighting}

const

  DXVA_VideoTransferMatrixShift = (DXVA_ExtColorData_ShiftBase + 7);
  {$EXTERNALSYM DXVA_VideoTransferMatrixShift}
  DXVA_VideoTransferMatrixMask = (not((not 0) shl 3)) shl (DXVA_VideoTransferMatrixShift - DXVA_ExtColorData_ShiftBase);
  {$EXTERNALSYM DXVA_VideoTransferMatrixMask}


type

  PDXVA_VideoTransferMatrix = ^DXVA_VideoTransferMatrix;
  DXVA_VideoTransferMatrix = (
    DXVA_VideoTransferMatrix_Unknown,
    DXVA_VideoTransferMatrix_BT709,
    DXVA_VideoTransferMatrix_BT601,
    DXVA_VideoTransferMatrix_SMPTE240M
  );
  {$EXTERNALSYM DXVA_VideoTransferMatrix}

const

  DXVA_NominalRangeShift = (DXVA_ExtColorData_ShiftBase + 4);
  {$EXTERNALSYM DXVA_NominalRangeShift}
  DXVA_NominalRangeMask = (not((not 0) shl 3)) shl (DXVA_NominalRangeShift - DXVA_ExtColorData_ShiftBase);
  {$EXTERNALSYM DXVA_NominalRangeMask}


type

  PDXVA_NominalRange = ^DXVA_NominalRange;
  DXVA_NominalRange = (
    DXVA_NominalRange_Unknown,
    DXVA_NominalRange_Normal,
    DXVA_NominalRange_Wide
  );
  {$EXTERNALSYM DXVA_NominalRange}

  DXVA_VideoChromaSubsampling = DWORD;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling}


const

  DXVA_VideoChromaSubsamplingShift = (DXVA_ExtColorData_ShiftBase + 0);
  {$EXTERNALSYM DXVA_VideoChromaSubsamplingShift}
  DXVA_VideoChromaSubsamplingMask = (not((not 0) shl 4)) shl (DXVA_VideoChromaSubsamplingShift - DXVA_ExtColorData_ShiftBase);
  {$EXTERNALSYM DXVA_VideoChromaSubsamplingMask}
  DXVA_VideoChromaSubsampling_Unknown = 0;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Unknown}
  DXVA_VideoChromaSubsampling_ProgressiveChroma = $8;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_ProgressiveChroma}
  DXVA_VideoChromaSubsampling_Horizontally_Cosited = $4;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Horizontally_Cosited}
  DXVA_VideoChromaSubsampling_Vertically_Cosited = $2;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Vertically_Cosited}
  DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes = $1;
  {$EXTERNALSYM DXVA_VideoChromaSubsampling_Vertically_AlignedChromaPlanes}


type

  //typedef struct _DXVA_ExtendedFormat
  //{
  //    UINT                        SampleFormat : 8;           // See DXVA_SampleFormat
  //    UINT                        VideoChromaSubsampling : 4; // See DXVA_VideoChromaSubSampling
  //    DXVA_NominalRange           NominalRange : 3;           // See DXVA_NominalRange
  //    DXVA_VideoTransferMatrix    VideoTransferMatrix : 3;    // See DXVA_VideoTransferMatrix
  //    DXVA_VideoLighting          VideoLighting : 4;          // See DXVA_VideoLighting
  //    DXVA_VideoPrimaries         VideoPrimaries : 5;         // See DXVA_VideoPrimaries
  //    DXVA_VideoTransferFunction  VideoTransferFunction : 5;  // See DXVA_VideoTransferFunction
  //} DXVA_ExtendedFormat;

  // Note: Delphi doesn't support bit-bashing the way it does in C/C++.
  // So, we do it like this (method without passing the mask)
  PDXVA_ExtendedFormat = ^_DXVA_ExtendedFormat;
  _DXVA_ExtendedFormat = record
    public
      Value: Integer;
    private
      function ReadBits(const iIndex: Integer): Integer;
      procedure WriteBits(const iIndex: Integer; const iValue: Integer);
    public
      property SampleFormat: Integer index $0008 read ReadBits write WriteBits;           // 8 bits at offset 0
      property VideoChromaSubsampling: Integer index $0804 read ReadBits write WriteBits; // 4 bits at offset 8
      property NominalRange: Integer index $0c03 read ReadBits write WriteBits;           // 3 bits at offset 12
      property VideoTransferMatrix: Integer index $0f03 read ReadBits write WriteBits;    // 3 bits at offset 15
      property VideoLighting: Integer index $1204 read ReadBits write WriteBits;          // 4 bits at offset 18
      property VideoPrimaries: Integer index $1605 read ReadBits write WriteBits;         // 5 bits at offset 22
      property VideoTransferFunction: Integer index $1b01 read ReadBits write WriteBits;  // 5 bits at offset 27
  end;
  {$EXTERNALSYM _DXVA_ExtendedFormat}
  DXVA_ExtendedFormat = _DXVA_ExtendedFormat;
  {$EXTERNALSYM DXVA_ExtendedFormat}

  //Or passing a mask (Peter)
  // Explanation:
  // Early Microsoft records included a lot of bit data and what you are looking at is 32 bits of data.
  // (the total UINT values = 32). C supports the concept of bitsets,
  // which also allow for integers in the bitset.
  // You could set up a Delphi "set" type property and force the size of the set to 32 bits.
  // However, this is not exactly what C can do and if you ever strike a struct that has UINT 2+,
  // they won't work.

  //_DXVA_ExtendedFormat = Record
  //  Private
  //    Value: UINT;
  //    Function GetFlag(Index: Integer): DWORD;
  //    Procedure SetFlag(Index: Integer; DWORD);
  //  Public
  //    Property SampleFormat: Integer Index Integer($80000000)  Read GetFlag Write SetFlag;   //8
  //    Property VideoChromaSubsampling: Integer index $40000000 Read GetFlag  Write SetFlag;  //4
  //    Property NominalRange: Integer index $30000000 Read GetFlag  Write SetFlag;            //3
  //    Property VideoTransferMatrix: Integer index $30000000 Read GetFlag  Write SetFlag;     //3
  //    Property VideoLighting: Integer index $40000000 Read GetFlag  Write SetFlag;           //4
  //    Property VideoPrimaries: Integer index $50000000 Read GetFlag  Write SetFlag;          //5
  //    Property VideoTransferFunction: Integer index $60000000 Read GetFlag  Write SetFlag;   //6
  //  End;


// -------------------------------------------------------------------------
//
// The definitions that follow describe the video de-interlace interface
// between the VMR and the graphics device driver.  This interface is not
// accessable via the IAMVideoAccelerator interface.
//
// -------------------------------------------------------------------------
//



  // REFERENCE_TIME  See also: mfpTypes.pas

{$IFNDEF MFP_REFERENCE_TIME}
  PREFERENCE_TIME = ^REFERENCE_TIME;
  REFERENCE_TIME = LONGLONG;
  {$EXTERNALSYM REFERENCE_TIME}
  PReferenceTime = ^ReferenceTime;
  ReferenceTime = REFERENCE_TIME;
{$EXTERNALSYM ReferenceTime}
{$ENDIF}

const

  DXVA_DeinterlaceBobDevice       : TGUID = '{335aa36e-7884-43a4-9c91-7f87faf3e37e}';
  {$EXTERNALSYM DXVA_DeinterlaceBobDevice}
  DXVA_DeinterlaceContainerDevice : TGUID = '{0e85cb93-3046-4ff0-aecc-d58cb5f035fd}';
  {$EXTERNALSYM DXVA_DeinterlaceContainerDevice}


  // -------------------------------------------------------------------------
  // data structures shared by User mode and Kernel mode.
  // -------------------------------------------------------------------------
  //
type

  PDXVA_Frequency =  ^_DXVA_Frequency;
  _DXVA_Frequency = record
    Numerator   : DWORD;
    Denominator : DWORD;
  end;
  {$EXTERNALSYM _DXVA_Frequency}
  DXVA_Frequency = _DXVA_Frequency;
  {$EXTERNALSYM DXVA_Frequency}


  LPDXVA_VideoDesc = ^_DXVA_VideoDesc;
  PDXVAVideoDesc = ^_DXVA_VideoDesc;
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
  PDXVAVideoProcessCaps = ^_DXVA_VideoProcessCaps;
  _DXVA_VideoProcessCaps = DWord;
  {$EXTERNALSYM _DXVA_VideoProcessCaps}
  DXVA_VideoProcessCaps = _DXVA_VideoProcessCaps;
  {$EXTERNALSYM DXVA_VideoProcessCaps}
const
  DXVA_VideoProcess_None               = DXVA_VideoProcessCaps($0000);
  {$EXTERNALSYM DXVA_VideoProcess_None}
  DXVA_VideoProcess_YUV2RGB            = DXVA_VideoProcessCaps($0001);
  {$EXTERNALSYM DXVA_VideoProcess_YUV2RGB}
  DXVA_VideoProcess_StretchX           = DXVA_VideoProcessCaps($0002);
  {$EXTERNALSYM DXVA_VideoProcess_StretchX}
  DXVA_VideoProcess_StretchY           = DXVA_VideoProcessCaps($0004);
  {$EXTERNALSYM DXVA_VideoProcess_StretchY}
  DXVA_VideoProcess_AlphaBlend         = DXVA_VideoProcessCaps($0008);
  {$EXTERNALSYM DXVA_VideoProcess_AlphaBlend}
  DXVA_VideoProcess_SubRects           = DXVA_VideoProcessCaps($0010);
  {$EXTERNALSYM DXVA_VideoProcess_SubRects}
  DXVA_VideoProcess_SubStreams         = DXVA_VideoProcessCaps($0020);
  {$EXTERNALSYM DXVA_VideoProcess_SubStreams}
  DXVA_VideoProcess_SubStreamsExtended = DXVA_VideoProcessCaps($0040);
  {$EXTERNALSYM DXVA_VideoProcess_SubStreamsExtended}
  DXVA_VideoProcess_YUV2RGBExtended    = DXVA_VideoProcessCaps($0080);
  {$EXTERNALSYM DXVA_VideoProcess_YUV2RGBExtended}
  DXVA_VideoProcess_AlphaBlendExtended = DXVA_VideoProcessCaps($0100);
  {$EXTERNALSYM DXVA_VideoProcess_AlphaBlendExtended}


type
  PDXVADeinterlaceTech = ^_DXVA_DeinterlaceTech;
  _DXVA_DeinterlaceTech = DWord;
  {$EXTERNALSYM _DXVA_DeinterlaceTech}
  DXVA_DeinterlaceTech = _DXVA_DeinterlaceTech;
  {$EXTERNALSYM DXVA_DeinterlaceTech}
const
  // the algorithm is unknown or proprietary
  DXVA_DeinterlaceTech_Unknown                = DXVA_DeinterlaceTech($0000);
  {$EXTERNALSYM DXVA_DeinterlaceTech_Unknown}
  // the algorithm creates the missing lines by repeating
  // the line either above or below it - this method will look very jaggy and
  // isn't recommended
  DXVA_DeinterlaceTech_BOBLineReplicate       = DXVA_DeinterlaceTech($0001);
  {$EXTERNALSYM DXVA_DeinterlaceTech_BOBLineReplicate}
  // The algorithm creates the missing lines by vertically stretching each
  // video field by a factor of two by averaging two lines
  DXVA_DeinterlaceTech_BOBVerticalStretch     = DXVA_DeinterlaceTech($0002);
  {$EXTERNALSYM DXVA_DeinterlaceTech_BOBVerticalStretch}
  // or using a [-1); 9); 9); -1]/16 filter across four lines.
  DXVA_DeinterlaceTech_BOBVerticalStretch4Tap = DXVA_DeinterlaceTech($0100);
  {$EXTERNALSYM DXVA_DeinterlaceTech_BOBVerticalStretch4Tap}
  // the pixels in the missing line are recreated by a median filtering operation
  DXVA_DeinterlaceTech_MedianFiltering        = DXVA_DeinterlaceTech($0004);
  {$EXTERNALSYM DXVA_DeinterlaceTech_MedianFiltering}
  // the pixels in the missing line are recreated by an edge filter.
  // In this process); spatial directional filters are applied to determine
  // the orientation of edges in the picture content); and missing
  // pixels are created by filtering along (rather than across) the
  // detected edges.
  DXVA_DeinterlaceTech_EdgeFiltering          = DXVA_DeinterlaceTech($0010);
  {$EXTERNALSYM DXVA_DeinterlaceTech_EdgeFiltering}
  // the pixels in the missing line are recreated by switching on a field by
  // field basis between using either spatial or temporal interpolation
  // depending on the amount of motion.
  DXVA_DeinterlaceTech_FieldAdaptive          = DXVA_DeinterlaceTech($0020);
  {$EXTERNALSYM DXVA_DeinterlaceTech_FieldAdaptive}
  // the pixels in the missing line are recreated by switching on a pixel by pixel
  // basis between using either spatial or temporal interpolation depending on
  // the amount of motion..
  DXVA_DeinterlaceTech_PixelAdaptive          = DXVA_DeinterlaceTech($0040);
  {$EXTERNALSYM DXVA_DeinterlaceTech_PixelAdaptive}
  // Motion Vector Steering  identifies objects within a sequence of video
  // fields.  The missing pixels are recreated after first aligning the
  // movement axes of the individual objects in the scene to make them
  // parallel with the time axis.
  DXVA_DeinterlaceTech_MotionVectorSteered    = DXVA_DeinterlaceTech($0080);
  {$EXTERNALSYM DXVA_DeinterlaceTech_MotionVectorSteered}

type
  LPDXVA_VideoSample = ^_DXVA_VideoSample;
  PDXVAVideoSample = ^_DXVA_VideoSample;
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
  PDXVASampleFlags = ^_DXVA_SampleFlags;
  _DXVA_SampleFlags = DWord;
  {$EXTERNALSYM _DXVA_SampleFlags}
  DXVA_SampleFlags = _DXVA_SampleFlags;
  {$EXTERNALSYM DXVA_SampleFlags}
const
  DXVA_SampleFlagsMask              = DXVA_SampleFlags((1 shl 3) or (1 shl 2) or (1 shl 1) or (1 shl 0));
  {$EXTERNALSYM DXVA_SampleFlagsMask}
  DXVA_SampleFlag_Palette_Changed   = DXVA_SampleFlags($0001);
  {$EXTERNALSYM DXVA_SampleFlag_Palette_Changed}
  DXVA_SampleFlag_SrcRect_Changed   = DXVA_SampleFlags($0002);
  {$EXTERNALSYM DXVA_SampleFlag_SrcRect_Changed}
  DXVA_SampleFlag_DstRect_Changed   = DXVA_SampleFlags($0004);
  {$EXTERNALSYM DXVA_SampleFlag_DstRect_Changed}
  DXVA_SampleFlag_ColorData_Changed = DXVA_SampleFlags($0008);
  {$EXTERNALSYM DXVA_SampleFlag_ColorData_Changed}

  // The DXVA_SampleFlags enumeration type contains a collection of flags that identify
  // changes in the current sample frame from the previous sample frame.
  // Constants:
  // DXVA_SampleFlagsMask
  //   Specifies the sample-flag mask, which consists of the first 4 bits of a DWORD.
  // DXVA_SampleFlag_Palette_Changed
  //   Indicates that the palette of the sample frame changed.
  // DXVA_SampleFlag_SrcRect_Changed
  //   Indicates that the source rectangle of the sample frame changed.
  // DXVA_SampleFlag_DstRect_Changed
  //   Indicates that the destination rectangle of the sample frame changed.
  // DXVA_SampleFlag_ColorData_Changed
  //   Indicates that the color data of the sample frame changed.

type
  PDXVADestinationFlags = ^_DXVA_DestinationFlags;
  _DXVA_DestinationFlags = DWord;
  {$EXTERNALSYM _DXVA_DestinationFlags}
  DXVA_DestinationFlags = _DXVA_DestinationFlags;
  {$EXTERNALSYM DXVA_DestinationFlags}
const
  DXVA_DestinationFlagMask                = DXVA_DestinationFlags((1 shl 3) or (1 shl 2) or (1 shl 1) or (1 shl 0));
  {$EXTERNALSYM DXVA_DestinationFlagMask}
  DXVA_DestinationFlag_Background_Changed = DXVA_DestinationFlags($0001);
  {$EXTERNALSYM DXVA_DestinationFlag_Background_Changed}
  DXVA_DestinationFlag_TargetRect_Changed = DXVA_DestinationFlags($0002);
  {$EXTERNALSYM DXVA_DestinationFlag_TargetRect_Changed}
  DXVA_DestinationFlag_ColorData_Changed  = DXVA_DestinationFlags($0004);
  {$EXTERNALSYM DXVA_DestinationFlag_ColorData_Changed}
  DXVA_DestinationFlag_Alpha_Changed      = DXVA_DestinationFlags($0008);
  {$EXTERNALSYM DXVA_DestinationFlag_Alpha_Changed}

  // The DXVA_DestinationFlags enumeration type contains a collection of flags that identify
  // changes in the current destination surface from the previous destination surface.
  // Constants:
  // DXVA_DestinationFlagMask
  //   Specifies the destination-flag mask, which consists of the first 4 bits of a DWORD.
  // DXVA_DestinationFlag_Background_Changed
  //   Indicates that the background color of the destination surface changed.
  // DXVA_DestinationFlag_TargetRect_Changed
  //   Indicates that the target rectangle of the destination surface changed.
  // DXVA_DestinationFlag_ColorData_Changed
  //   Indicates that format information for the destination surface changed.
  // DXVA_DestinationFlag_Alpha_Changed
  //   Indicates that the planar alpha value for the destination surface changed.


type

  LPDXVA_VideoSample2 = ^DXVA_VideoSample2;
  DXVA_VideoSample2 = record
{$IFDEF WIN64}
    Size: DWORD;
    Reserved: DWORD;
{$ENDIF}
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    SampleFormat: DWORD; // cast to DXVA_ExtendedFormat, or use Extract macros
    SampleFlags: DWORD;
    lpDDSSrcSurface: Pointer;
    rcSrc: TRect;
    rcDst: TRect;
    Palette: array[0..15] of DXVA_AYUVsample2;
  end;
  {$EXTERNALSYM DXVA_VideoSample2}
  PDXVA_VideoSample2 = ^DXVA_VideoSample2;
  {$EXTERNALSYM PDXVA_VideoSample2}


  PDXVA_DeinterlaceCaps = ^_DXVA_DeinterlaceCaps;
  LPDXVA_DeinterlaceCaps = ^DXVA_DeinterlaceCaps;
  _DXVA_DeinterlaceCaps = record
     Size                    : DWORD;
     NumPreviousOutputFrames : DWORD;
     InputPool               : DWORD;
     NumForwardRefSamples    : DWORD;
     NumBackwardRefSamples   : DWORD;
     d3dOutputFormat         : D3DFORMAT;
     VideoProcessingCaps     : DXVA_VideoProcessCaps;
     DeinterlaceTechnology   : DXVA_DeinterlaceTech;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceCaps}
  DXVA_DeinterlaceCaps = _DXVA_DeinterlaceCaps;
  {$EXTERNALSYM DXVA_DeinterlaceCaps}


// -------------------------------------------------------------------------
// Data types used with RenderMoComp in kernel mode
// -------------------------------------------------------------------------

const

  // Function codes for RenderMoComp
  MAX_DEINTERLACE_SURFACES = 32;
  {$EXTERNALSYM MAX_DEINTERLACE_SURFACES}


  //
  // These structures are used for thunking 32 bit DeinterlaceBltEx calls on
  // 64 bit drivers.
  // Only compiles for a 64-bit version of the operating system.

{$IFDEF WIN64}

type

  PDXVA_VideoSample32 = ^DXVA_VideoSample32;
  _DXVA_VideoSample32 = record
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    SampleFormat: DWORD;
    SampleFlags: DWORD;
    lpDDSSrcSurface: DWORD;         // 32 bit pointer size
    rcSrc: TRect; // RECT
    rcDst: TRect; // RECT
    Palette: array[0..15] of DXVA_AYUVsample2;
    // Pad: DWORD;
    // 4 bytes of padding added by the compiler to align the struct to 8 bytes.
  end;
  {$EXTERNALSYM _DXVA_VideoSample32}
  DXVA_VideoSample32 = _DXVA_VideoSample32;
  {$EXTERNALSYM DXVA_VideoSample32}


  PDXVA_DeinterlaceBltEx32 = ^_DXVA_DeinterlaceBltEx32;
  _DXVA_DeinterlaceBltEx32 = record
    Size: DWORD;
    BackgroundColor: DXVA_AYUVsample2;
    rcTarget: TRect; // RECT
    rtTarget: REFERENCE_TIME;
    NumSourceSurfaces: DWORD;
    Alpha: FLOAT;
    Source: array[0..MAX_DEINTERLACE_SURFACES - 1] of DXVA_VideoSample32;
    DestinationFormat: DWORD;
    DestinationFlags: DWORD;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceBltEx32}
  DXVA_DeinterlaceBltEx32 = _DXVA_DeinterlaceBltEx32;
  {$EXTERNALSYM DXVA_DeinterlaceBltEx32}

{$ENDIF}

type
  PDXVA_DeinterlaceBlt = ^_DXVA_DeinterlaceBlt;
  _DXVA_DeinterlaceBlt = record
    Size              : DWORD;
    Reserved          : DWORD;
    rtTarget          : REFERENCE_TIME;
    DstRect           : TRECT;
    SrcRect           : TRECT;
    NumSourceSurfaces : DWORD;
    Alpha             : Single;
    Source: array[0..MAX_DEINTERLACE_SURFACES-1] of DXVA_VideoSample;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceBlt}
  DXVA_DeinterlaceBlt = _DXVA_DeinterlaceBlt;
  {$EXTERNALSYM DXVA_DeinterlaceBlt}


const

  DXVA_DeinterlaceBltFnCode = $01;
  {$EXTERNALSYM DXVA_DeinterlaceBltFnCode}
  // lpInput => DXVA_DeinterlaceBlt*
  // lpOuput => NULL /* not currently used */

type

  PDXVA_DeinterlaceBltEx = ^DXVA_DeinterlaceBltEx;
  DXVA_DeinterlaceBltEx = record
    Size: DWORD;
    BackgroundColor   : DXVA_AYUVsample2;
    rcTarget          : TRect;
    rtTarget          : REFERENCE_TIME;
    NumSourceSurfaces : DWORD;
    Alpha             : Single;
    Source            : array[0..MAX_DEINTERLACE_SURFACES-1] of DXVA_VideoSample2;
    DestinationFormat : DWORD;
    DestinationFlags  : DWORD;
  end;
  {$EXTERNALSYM DXVA_DeinterlaceBltEx}


const

  MAX_DEINTERLACE_DEVICE_GUIDS = 32;
  {$EXTERNALSYM MAX_DEINTERLACE_DEVICE_GUIDS}


  DXVA_DeinterlaceQueryAvailableModesFnCode= $01;
  {$EXTERNALSYM DXVA_DeinterlaceQueryAvailableModesFnCode}
  // lpInput => DXVA_VideoDesc*
  // lpOuput => DXVA_DeinterlaceQueryAvailableModes*

type

  PDXVA_DeinterlaceQueryAvailableModes = ^_DXVA_DeinterlaceQueryAvailableModes;
  _DXVA_DeinterlaceQueryAvailableModes = record
    Size     : DWORD;
    NumGuids : DWORD;
    Guids: array[0..MAX_DEINTERLACE_DEVICE_GUIDS-1] of TGUID;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceQueryAvailableModes}
  DXVA_DeinterlaceQueryAvailableModes = _DXVA_DeinterlaceQueryAvailableModes;
  {$EXTERNALSYM DXVA_DeinterlaceQueryAvailableModes}


  PDXVA_DeinterlaceQueryModeCaps = ^_DXVA_DeinterlaceQueryModeCaps;
  _DXVA_DeinterlaceQueryModeCaps = record
    Size      : DWORD;
    Guid      : TGUID;
    VideoDesc : DXVA_VideoDesc;
  end;
  {$EXTERNALSYM _DXVA_DeinterlaceQueryModeCaps}
  DXVA_DeinterlaceQueryModeCaps = _DXVA_DeinterlaceQueryModeCaps;
  {$EXTERNALSYM DXVA_DeinterlaceQueryModeCaps}

const

  DXVA_DeinterlaceQueryModeCapsFnCode = $02;
  {$EXTERNALSYM DXVA_DeinterlaceQueryModeCapsFnCode}
  // lpInput => DXVA_DeinterlaceQueryModeCaps*
  // lpOuput => DXVA_DeinterlaceCaps*

// -------------------------------------------------------------------------
//
// The definitions that follow describe the video ProcAmp interface
// between the VMR and the graphics device driver.  This interface is not
// accessable via the IAMVideoAccelerator interface.
//
// -------------------------------------------------------------------------
//


const

  DXVA_ProcAmpControlDevice         : TGUID = '{9f200913-2ffd-4056-9f1e-e1b508f22dcf}';
  {$EXTERNALSYM DXVA_ProcAmpControlDevice}


type

  DXVA_ProcAmpControlProp = {$IFDEF TYPE_IDENTITY} type {$ENDIF} DWORD;
  {$EXTERNALSYM DXVA_ProcAmpControlProp}

const

  DXVA_ProcAmp_None       = $0000;
  {$EXTERNALSYM DXVA_ProcAmp_None}
  DXVA_ProcAmp_Brightness = $0001;
  {$EXTERNALSYM DXVA_ProcAmp_Brightness}
  DXVA_ProcAmp_Contrast   = $0002;
  {$EXTERNALSYM DXVA_ProcAmp_Contrast}
  DXVA_ProcAmp_Hue        = $0004;
  {$EXTERNALSYM DXVA_ProcAmp_Hue}
  DXVA_ProcAmp_Saturation = $0008;
  {$EXTERNALSYM DXVA_ProcAmp_Saturation}

type

  PDXVA_ProcAmpControlCaps = ^_DXVA_ProcAmpControlCaps;
  LPDXVA_ProcAmpControlCaps = ^_DXVA_ProcAmpControlCaps;
  _DXVA_ProcAmpControlCaps = record
    Size                : DWORD;
    InputPool           : DWORD;
    d3dOutputFormat     : D3DFORMAT;
    ProcAmpControlProps : DWORD; // see DXVA_ProcAmpControlProp
    VideoProcessingCaps : DWORD; // see DXVA_VideoProcessCaps
  end;
  {$EXTERNALSYM _DXVA_ProcAmpControlCaps}
  DXVA_ProcAmpControlCaps = _DXVA_ProcAmpControlCaps;
  {$EXTERNALSYM DXVA_ProcAmpControlCaps}


const

  DXVA_ProcAmpControlQueryCapsFnCode = $03;
  {$EXTERNALSYM DXVA_ProcAmpControlQueryCapsFnCode}

  // lpInput => DXVA_VideoDesc*
  // lpOuput => DXVA_ProcAmpControlCaps*

type

  PDXVA_ProcAmpControlQueryRange = ^_DXVA_ProcAmpControlQueryRange;
  LPDXVA_ProcAmpControlQueryRange = ^_DXVA_ProcAmpControlQueryRange;
  _DXVA_ProcAmpControlQueryRange = record
    Size: DWORD;
    ProcAmpControlProp: DXVA_ProcAmpControlProp;
    VideoDesc: DXVA_VideoDesc;
  end;
  {$EXTERNALSYM _DXVA_ProcAmpControlQueryRange}
  DXVA_ProcAmpControlQueryRange = _DXVA_ProcAmpControlQueryRange;
  {$EXTERNALSYM DXVA_ProcAmpControlQueryRange}


  PDXVA_VideoPropertyRange = ^_DXVA_VideoPropertyRange;
  LPDXVA_VideoPropertyRange = ^_DXVA_VideoPropertyRange;
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

  DXVA_ProcAmpControlQueryRangeFnCode = $04;
  {$EXTERNALSYM DXVA_ProcAmpControlQueryRangeFnCode}
  // lpInput => DXVA_ProcAmpControlQueryRange*
  // lpOuput => DXVA_VideoPropertyRange*

type

  PDXVA_ProcAmpControlBlt = ^DXVA_ProcAmpControlBlt;
  _DXVA_ProcAmpControlBlt = record
    Size       : DWORD;
    DstRect    : TRECT;
    SrcRect    : TRECT;
    Alpha      : Single;
    Brightness : Single;
    Contrast   : Single;
    Hue        : Single;
    Saturation : Single;
  end;
  {$EXTERNALSYM _DXVA_ProcAmpControlBlt}
  DXVA_ProcAmpControlBlt = _DXVA_ProcAmpControlBlt;
  {$EXTERNALSYM DXVA_ProcAmpControlBlt}


const

  DXVA_ProcAmpControlBltFnCode = $01;
  {$EXTERNALSYM DXVA_ProcAmpControlBltFnCode}
  // lpInput => DXVA_ProcAmpControlBlt*
  // lpOuput => NULL /* not currently used */

  // -------------------------------------------------------------------------
  //
  // The definitions that follow describe the Certified Output Protection
  // Protocol between the VMR and the graphics device driver.  This interface
  // is not accessable via the IAMVideoAccelerator interface.
  //
  // -------------------------------------------------------------------------
  //

  DXVA_COPPDevice : TGUID = '{D2457ADD-8999-45ED-8A8A-D1AA047BA4D5}';
  {$EXTERNALSYM DXVA_COPPDevice}


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
  DXVA_COPPKeyExchangeFnCode = $02;
  {$EXTERNALSYM DXVA_COPPKeyExchangeFnCode}
  // lpInputData => NULL
  // lpOuputData => GUID*


  // -------------------------------------------------------------------------
  // COPPSequenceStart
  // -------------------------------------------------------------------------
type

  PDXVA_COPPSignature = ^DXVA_COPPSignature;
  LPDXVA_COPPSignature = ^DXVA_COPPSignature;
  DXVA_COPPSignature = record
    Signature: array[0..255] of AnsiChar;
  end;
  {$EXTERNALSYM DXVA_COPPSignature}

const

  DXVA_COPPSequenceStartFnCode = $03;
  {$EXTERNALSYM DXVA_COPPSequenceStartFnCode}

  // lpInputData => DXVA_COPPSignature*
  // lpOuputData => NULL



  // -------------------------------------------------------------------------
  // COPPCommand
  // -------------------------------------------------------------------------
type

  PDXVA_COPPCommand = ^DXVA_COPPCommand;
  LPDXVA_COPPCommand = ^DXVA_COPPCommand;
  DXVA_COPPCommand = record
    macKDI: TGUID;                 //   16 bytes
    guidCommandID: TGUID;          //   16 bytes
    dwSequence: ULONG;            //    4 bytes
    cbSizeData: ULONG;            //    4 bytes
    CommandData: array[0..4055] of AnsiChar;  // 4056 bytes (4056+4+4+16+16 = 4096)
  end;
  {$EXTERNALSYM DXVA_COPPCommand}

const

  DXVA_COPPCommandFnCode = $04;
  {$EXTERNALSYM DXVA_COPPCommandFnCode}
  // lpInputData => DXVA_COPPCommand*
  // lpOuputData => NULL
  DXVA_COPPSetProtectionLevel : TGUID = '{9bb9327c-4eb5-4727-9f00-b42b0919c0da}';
  {$EXTERNALSYM DXVA_COPPSetProtectionLevel}


type

  PDXVA_COPPSetProtectionLevelCmdData = ^DXVA_COPPSetProtectionLevelCmdData;
  DXVA_COPPSetProtectionLevelCmdData = record
    ProtType: ULONG;
    ProtLevel: ULONG;
    ExtendedInfoChangeMask: ULONG;
    ExtendedInfoData: ULONG;
  end;
  {$EXTERNALSYM DXVA_COPPSetProtectionLevelCmdData}


  // Set the HDCP protection level - (0 - 1 DWORD, 4 bytes)
  COPP_HDCP_Protection_Level = DWORD;
  {$EXTERNALSYM COPP_HDCP_Protection_Level}

const

  COPP_HDCP_Level0    = 0;
  {$EXTERNALSYM COPP_HDCP_Level0}
  COPP_HDCP_LevelMin  = COPP_HDCP_Level0;
  {$EXTERNALSYM COPP_HDCP_LevelMin}
  COPP_HDCP_Level1    = 1;
  {$EXTERNALSYM COPP_HDCP_Level1}
  COPP_HDCP_LevelMax  = COPP_HDCP_Level1;
  {$EXTERNALSYM COPP_HDCP_LevelMax}
  COPP_HDCP_ForceDWORD = $7FFFFFFF;
  {$EXTERNALSYM COPP_HDCP_ForceDWORD}


type

  COPP_CGMSA_Protection_Level = DWORD;
  {$EXTERNALSYM COPP_CGMSA_Protection_Level}


const

  COPP_CGMSA_Disabled = 0;
  {$EXTERNALSYM COPP_CGMSA_Disabled}
  COPP_CGMSA_LevelMin = COPP_CGMSA_Disabled;
  {$EXTERNALSYM COPP_CGMSA_LevelMin}
  COPP_CGMSA_CopyFreely = 1;
  {$EXTERNALSYM COPP_CGMSA_CopyFreely}
  COPP_CGMSA_CopyNoMore = 2;
  {$EXTERNALSYM COPP_CGMSA_CopyNoMore}
  COPP_CGMSA_CopyOneGeneration = 3;
  {$EXTERNALSYM COPP_CGMSA_CopyOneGeneration}
  COPP_CGMSA_CopyNever = 4;
  {$EXTERNALSYM COPP_CGMSA_CopyNever}
  COPP_CGMSA_RedistributionControlRequired = $08;
  {$EXTERNALSYM COPP_CGMSA_RedistributionControlRequired}
  COPP_CGMSA_LevelMax = (COPP_CGMSA_RedistributionControlRequired + COPP_CGMSA_CopyNever);
  {$EXTERNALSYM COPP_CGMSA_LevelMax}
  COPP_CGMSA_ForceDWORD = $7FFFFFFF;
  {$EXTERNALSYM COPP_CGMSA_ForceDWORD}

type
  COPP_ACP_Protection_Level = DWORD;
  {$EXTERNALSYM COPP_ACP_Protection_Level}

const
  COPP_ACP_Level0     = 0;
  {$EXTERNALSYM COPP_ACP_Level0}
  COPP_ACP_LevelMin   = COPP_ACP_Level0;
  {$EXTERNALSYM COPP_ACP_LevelMin}
  COPP_ACP_Level1     = 1;
  {$EXTERNALSYM COPP_ACP_Level1}
  COPP_ACP_Level2     = 2;
  {$EXTERNALSYM COPP_ACP_Level2}
  COPP_ACP_Level3     = 3;
  {$EXTERNALSYM COPP_ACP_Level3}
  COPP_ACP_LevelMax   = COPP_ACP_Level3;
  {$EXTERNALSYM COPP_ACP_LevelMax}
  COPP_ACP_ForceDWORD = $7FFFFFFF;
  {$EXTERNALSYM COPP_ACP_ForceDWORD}


  COPP_NoProtectionLevelAvailable = -1;
  {$EXTERNALSYM COPP_NoProtectionLevelAvailable}
  COPP_DefaultProtectionLevel = 0;
  {$EXTERNALSYM COPP_DefaultProtectionLevel}

  //
  // Bit flags of possible protection types. Note that it is possible to apply
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
  COPP_ProtectionType_Reserved     = $FFFFFFF8;
  {$EXTERNALSYM COPP_ProtectionType_Reserved}

  DXVA_COPPSetSignaling : TGUID = '{09A631A5-D684-4C60-8E4D-D3BB0F0BE3EE}';
  {$EXTERNALSYM DXVA_COPPSetSignaling}


type

  PDXVA_COPPSetSignalingCmdData = ^DXVA_COPPSetSignalingCmdData;
  DXVA_COPPSetSignalingCmdData = record
    ActiveTVProtectionStandard: ULONG;           // See COPP_TVProtectionStandard
    AspectRatioChangeMask1: ULONG;
    AspectRatioData1: ULONG;                     // See COPP_ImageAspectRatio_EN300294 for ETSI EN 300 294 values
    AspectRatioChangeMask2: ULONG;
    AspectRatioData2: ULONG;
    AspectRatioChangeMask3: ULONG;
    AspectRatioData3: ULONG;
    ExtendedInfoChangeMask: array[0..3] of ULONG;
    ExtendedInfoData: array[0..3] of ULONG;
    Reserved: ULONG;
  end;
  {$EXTERNALSYM DXVA_COPPSetSignalingCmdData}

type
  // Add format enum and data enum
  PCOPP_TVProtectionStandard = ^COPP_TVProtectionStandard;
  COPP_TVProtectionStandard = DWORD;
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
  COPP_ImageAspectRatio_EN300294_Mask         = COPP_TVProtectionStandard($00000007);
  {$EXTERNALSYM COPP_ImageAspectRatio_EN300294_Mask}


type

  PCOPP_ImageAspectRatio_EN300294 = ^COPP_ImageAspectRatio_EN300294;
  COPP_ImageAspectRatio_EN300294 = (
    COPP_AspectRatio_EN300294_FullFormat4by3                = 0,
    COPP_AspectRatio_EN300294_Box14by9Center                = 1,
    COPP_AspectRatio_EN300294_Box14by9Top                   = 2,
    COPP_AspectRatio_EN300294_Box16by9Center                = 3,
    COPP_AspectRatio_EN300294_Box16by9Top                   = 4,
    COPP_AspectRatio_EN300294_BoxGT16by9Center              = 5,
    COPP_AspectRatio_EN300294_FullFormat4by3ProtectedCenter = 6,
    COPP_AspectRatio_EN300294_FullFormat16by9Anamorphic     = 7
  );
  {$EXTERNALSYM COPP_ImageAspectRatio_EN300294}

  // -------------------------------------------------------------------------
  // COPPQueryStatus
  // -------------------------------------------------------------------------

  PDXVA_COPPStatusInput = ^DXVA_COPPStatusInput;
  LPDXVA_COPPStatusInput = ^DXVA_COPPStatusInput;
  DXVA_COPPStatusInput = record
    rApp                : TGUID;              //   16 bytes
    guidStatusRequestID : TGUID;              //   16 bytes
    dwSequence          : ULONG;              //    4 bytes
    cbSizeData          : ULONG;              //    4 bytes
    StatusData: array[0..4055] of AnsiChar;   // 4056 bytes (4056+4+4+16+16 = 4096)
  end;
  {$EXTERNALSYM DXVA_COPPStatusInput}


  PDXVA_COPPStatusOutput = ^DXVA_COPPStatusOutput;
  LPDXVA_COPPStatusOutput = ^DXVA_COPPStatusOutput;
  DXVA_COPPStatusOutput = record
    macKDI     : TGUID;         //   16 bytes
    cbSizeData : ULONG;         //    4 bytes
    COPPStatus : array[0..4075] of AnsiChar; // 4076 bytes (4076+16+4 = 4096)
  end;
  {$EXTERNALSYM DXVA_COPPStatusOutput}


  PCOPP_StatusFlags = ^COPP_StatusFlags;
  COPP_StatusFlags = (
    COPP_StatusNormal,
    COPP_LinkLost,
    COPP_RenegotiationRequired);
  {$EXTERNALSYM COPP_StatusFlags}

const

  COPP_StatusFlagsReserved = $FFFFFFFC;
  {$EXTERNALSYM COPP_StatusFlagsReserved}


type

  PDXVACOPPStatusData = ^DXVA_COPPStatusData;
  DXVA_COPPStatusData = record
    rApp             : TGUID;
    dwFlags          : ULONG; // See COPP_StatusFlags above
    dwData           : ULONG;
    ExtendedInfoValidMask : ULONG;
    ExtendedInfoData : ULONG;
  end;
  {$EXTERNALSYM DXVA_COPPStatusData}


  PDXVA_COPPStatusDisplayData = ^DXVA_COPPStatusDisplayData;
  DXVA_COPPStatusDisplayData = record
    rApp: TGUID;
    dwFlags: ULONG;    // See COPP_StatusFlags above
    DisplayWidth: ULONG;
    DisplayHeight: ULONG;
    Format: ULONG;     // also contains extended color data
    d3dFormat: ULONG;
    FreqNumerator: ULONG;
    FreqDenominator: ULONG;
  end;
  {$EXTERNALSYM DXVA_COPPStatusDisplayData}

  COPP_StatusHDCPFlags    = DWORD;
  {$EXTERNALSYM COPP_StatusHDCPFlags}

const

  COPP_HDCPRepeater       = $01;
  {$EXTERNALSYM COPP_HDCPRepeater}
  COPP_HDCPFlagsReserved  = $FFFFFFFE;
  {$EXTERNALSYM COPP_HDCPFlagsReserved}


type

  PDXVA_COPPStatusHDCPKeyData = ^DXVA_COPPStatusHDCPKeyData;
  DXVA_COPPStatusHDCPKeyData = record
    rApp: TGUID;
    dwFlags: ULONG;        // See COPP_StatusFlags above
    dwHDCPFlags: ULONG;    // See COPP_StatusHDCPFlags above
    BKey: TGUID;           // Lower 40 bits
    Reserved1: TGUID;
    Reserved2: TGUID;
  end;
  {$EXTERNALSYM DXVA_COPPStatusHDCPKeyData}

const

  DXVA_COPPQueryStatusFnCode = $05;
  {$EXTERNALSYM DXVA_COPPQueryStatusFnCode}

  // lpInputData => DXVA_COPPStatusInput*
  // lpOuputData => DXVA_COPPStatusOutput*


  //
  // Status GUID and enumerations
  //
  DXVA_COPPQueryConnectorType: TGUID =  '{81d0bfd5-6afe-48c2-99c0-95a08f97c5da}';
  {$EXTERNALSYM DXVA_COPPQueryConnectorType}


type

  PCOPP_ConnectorType = ^_COPP_ConnectorType;
  _COPP_ConnectorType                      = (
    COPP_ConnectorType_Unknown             = - 1,
    COPP_ConnectorType_VGA                 = 0,
    COPP_ConnectorType_SVideo              = 1,
    COPP_ConnectorType_CompositeVideo      = 2,
    COPP_ConnectorType_ComponentVideo      = 3,
    COPP_ConnectorType_DVI                 = 4,
    COPP_ConnectorType_HDMI                = 5,
    COPP_ConnectorType_LVDS                = 6,
    COPP_ConnectorType_TMDS                = 7,
    COPP_ConnectorType_D_JPN               = 8,
    COPP_ConnectorType_SDI                 = 9,
    COPP_ConnectorType_DisplayPortExternal = 10,
    COPP_ConnectorType_DisplayPortEmbedded = 11,
    COPP_ConnectorType_UDIExternal         = 12,
    COPP_ConnectorType_UDIEmbedded         = 13,
    COPP_ConnectorType_Internal            = $80000000 -1,  // can be combined with the other connector types
    COPP_ConnectorType_ForceDWORD          = $7FFFFFFF     // force 32-bit size enum
  );
  {$EXTERNALSYM _COPP_ConnectorType}
  COPP_ConnectorType = _COPP_ConnectorType;
  {$EXTERNALSYM COPP_ConnectorType}


const

  DXVA_COPPQueryProtectionType        : TGUID = '{38f2a801-9a6c-48bb-9107-b6696e6f1797}';
  {$EXTERNALSYM DXVA_COPPQueryProtectionType}
  DXVA_COPPQueryLocalProtectionLevel  : TGUID = '{b2075857-3eda-4d5d-88db-748f8c1a0549}';
  {$EXTERNALSYM DXVA_COPPQueryLocalProtectionLevel}
  DXVA_COPPQueryGlobalProtectionLevel : TGUID = '{1957210a-7766-452a-b99a-d27aed54f03a}';
  {$EXTERNALSYM DXVA_COPPQueryGlobalProtectionLevel}
  DXVA_COPPQueryDisplayData           : TGUID = '{d7bf1ba3-ad13-4f8e-af98-0dcb3ca204cc}';
  {$EXTERNALSYM DXVA_COPPQueryDisplayData}
  DXVA_COPPQueryHDCPKeyData           : TGUID = '{0db59d74-a992-492e-a0bd-c23fda564e00}';
  {$EXTERNALSYM DXVA_COPPQueryHDCPKeyData}
  DXVA_COPPQueryBusData               : TGUID = '{c6f4d673-6174-4184-8e35-f6db5200bcba}';
  {$EXTERNALSYM DXVA_COPPQueryBusData}


type

  PCOPP_BusType = ^_COPP_BusType;
  {$EXTERNALSYM _COPP_BusType}
  _COPP_BusType             = (
    COPP_BusType_Unknown    = 0,
    COPP_BusType_PCI        = 1,
    COPP_BusType_PCIX       = 2,
    COPP_BusType_PCIExpress = 3,
    COPP_BusType_AGP        = 4,
    COPP_BusType_Integrated = $80000000 -1,  // can be combined with the other bus types
    COPP_BusType_ForceDWORD = $7FFFFFFF     // force 32-bit size enum
  );
  COPP_BusType = _COPP_BusType;
  {$EXTERNALSYM COPP_BusType}

const

  DXVA_COPPQuerySignaling : TGUID = '{6629A591-3B79-4CF3-924A-11E8E7811671}';
  {$EXTERNALSYM DXVA_COPPQuerySignaling}

type

  PDXVA_COPPStatusSignalingCmdData = ^_DXVA_COPPStatusSignalingCmdData;
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

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.



{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}

// _DXVA_PicEntry_H264 /////////////////////////////////////////////////////////
function _DXVA_PicEntry_H264.ReadBytes(const aIndex: integer): Byte;
var
  Offset: Byte;
  NrBits: Byte;
  Mask: Byte;

begin
  NrBits := aIndex and $FF;
  Offset := aIndex shr 8;
  Mask := ((1 shl NrBits) - 1);
  Result := (bPicEntry shr Offset) and Mask;
end;


procedure _DXVA_PicEntry_H264.WriteBytes(const aIndex: integer; const aValue: Byte);
var
  Offset: Byte;
  NrBits: Byte;
  Mask: Byte;

begin
  NrBits := aIndex and $FF;
  Offset := aIndex shr 8;
  Mask := ((1 shl NrBits) - 1);
{$IF DEBUG}
    Assert(Value <= Mask);
{$ENDIF}
    bPicEntry := (aValue and (not (Mask shl Offset))) or (aValue shl Offset);
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


  // Peter:
  //Function __DXVA_ExtendedFormat.GetFlag(Index: Integer): Boolean;
  //  Begin
  //    Result:= (Value and DWORD(Index)) = DWORD(Index);
  //  End;
  //
  //Procedure __DXVA_ExtendedFormat.SetFlag(Index: Integer);
  //  Begin
  //    Value:= Value and DWORD(Index);
  //  End;

{$OPTIMIZATION OFF}

// inline

function readDXVA_MBskipsFollowing(dwMB_SNL: DWord): DWORD;
begin
  Result := ((dwMB_SNL and $FF000000) shr 24);
end;

function readDXVA_MBdataLocation(dwMB_SNL: DWord): DWORD;
begin
  Result := (dwMB_SNL and $00FFFFFF);
end;

function writeDXVA_MB_SNL(dwMB_SNL: DWord ; skips: DWord; dloc: DWord): DWORD;
begin
  Result := (dwMB_SNL or (skips shl 24) or dloc);
end;

function setDXVA_MBskipsFollowing(dwMB_SNL: DWord; skips: DWord): DWORD;
begin
  Result := dwMB_SNL or (skips shl 24);
end;

function setDXVA_MBdataLocation(dwMB_SNL: DWord; dloc: Dword): DWORD;
begin
  Result := (dwMB_SNL or dloc);
end;

function readDXVA_MvertFieldSel_3(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $8000) shr 15);
end;

function readDXVA_MvertFieldSel_2(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $4000) shr 14);
end;

function readDXVA_MvertFieldSel_1(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $2000) shr 13);
end;

function readDXVA_MvertFieldSel_0(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $1000) shr 12);
end;

function readDXVA_ReservedBits(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0800) shr 11);
end;

function readDXVA_HostResidDiff(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0400) shr 10);
end;

function readDXVA_MotionType(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0300) shr 8);
end;

function readDXVA_MBscanMethod(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $00C0) shr 6);
end;

function readDXVA_FieldResidual(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0020) shr 5);
end;

function readDXVA_H261LoopFilter(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0010) shr 4);
end;

function readDXVA_Motion4MV(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0008) shr 3);
end;

function readDXVA_MotionBackward(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0004) shr 2);
end;

function readDXVA_MotionForward(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0002) shr 1);
end;

function readDXVA_IntraMacroblock(wMBtype: DWord): DWORD;
begin
  Result := ((wMBtype and $0001));
end;

function setDXVA_MvertFieldSel_3(wMBtype: DWord): DWORD;
begin
  Result := (wMBtype or $8000);
end;

function setDXVA_MvertFieldSel_2(wMBtype: DWord): DWORD;
begin
  Result := (wMBtype or $4000);
end;

function setDXVA_MvertFieldSel_1(wMBtype: DWord): DWORD;
begin
  Result := (wMBtype or $2000);
end;

function setDXVA_MvertFieldSel_0(wMBtype: DWord): DWORD;
begin
  Result := (wMBtype or $1000);
end;

function setDXVA_ReservedBits(wMBtype: DWord): DWORD;
begin
  Result := (wMBtype or $0800);
end;

function setDXVA_HostResidDiff(wMBtype: DWord): DWORD;
begin
  Result := (wMBtype or $0400);
end;

function setDXVA_MotionType(wMBtype: DWord; value: DWord ): DWORD;
begin
  Result := wMBtype or (value shl 8);
end;

function setDXVA_MBscanMethod(wMBtype: DWord; value: DWord ): DWORD;
begin
  Result := wMBtype or (value shl 6);
end;

function setDXVA_FieldResidual(wMBtype: DWord): DWORD;
begin
  Result := wMBtype or $0020;
end;

function setDXVA_H261LoopFilter(wMBtype: DWord): DWORD;
begin
  Result := wMBtype or $0010;
end;

function setDXVA_Motion4MV(wMBtype: DWord): DWORD;
begin
  Result := wMBtype or $0008;
end;

function setDXVA_MotionBackward(wMBtype: DWord): DWORD;
begin
  Result := wMBtype or $0004;
end;

function setDXVA_MotionForward(wMBtype: DWord): DWORD;
begin
  Result := wMBtype or $0002;
end;

function setDXVA_IntraMacroblock(wMBtype: DWord): DWORD;
begin
  Result := wMBtype or $0001;
end;

function readDXVA_Y___0coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0800) shr 11);
end;

function readDXVA_Y___1coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0400) shr 10);
end;

function readDXVA_Y___2coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0200) shr 9);
end;

function readDXVA_Y___3coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0100) shr 8);
end;

function readDXVA_Cb__4coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0080) shr 7);
end;

function readDXVA_Cr__5coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0040) shr 6);
end;

function readDXVA_Cb__6coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0020) shr 5);
end;

function readDXVA_Cr__7coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0010) shr 4);
end;

function readDXVA_Cb__8coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0008) shr 3);
end;

function readDXVA_Cb__9coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0004) shr 2);
end;

function readDXVA_Cr_10coded(wPatternCode: Dword): DWORD;
begin
  Result := ((wPatternCode and $0002) shr 1);
end;

function readDXVA_Cr_11coded(wPatternCode: Dword): DWORD;
begin
  Result := (wPatternCode and $0001);
end;


function readDXVA_Y___0oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0800) shr 11);
end;

function readDXVA_Y___1oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0400) shr 10);
end;

function readDXVA_Y___2oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0200) shr 9);
end;

function readDXVA_Y___3oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0100) shr 8);
end;

function readDXVA_Cb__4oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0080) shr 7);
end;

function readDXVA_Cr__5oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0040) shr 6);
end;

function readDXVA_Cb__6oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0020) shr 5);
end;

function readDXVA_Cr__7oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0010) shr 4);
end;

function readDXVA_Cb__8oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0008) shr 3);
end;

function readDXVA_Cb__9oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0004) shr 2);
end;

function readDXVA_Cr_10oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := ((wPC_Overflow and $0002) shr 1);
end;

function readDXVA_Cr_11oflow(wPC_Overflow: DWord): DWORD;
begin
  Result := wPC_Overflow and $0001;
end;

end.

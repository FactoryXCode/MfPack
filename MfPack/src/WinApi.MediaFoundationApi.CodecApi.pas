// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.CodecApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: CodecAPI Definitions.
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
//          Maybe we need the Macro's later (see the original headerfile.)
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
// Source: codecapi.h
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
unit WinApi.MediaFoundationApi.CodecApi;

  {$HPPEMIT '#include "codecapi.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

// Windows CodecAPI Properties

// Legend
//  Reference       VariantType   VariantField
//  UINT8           VT_UI1        bVal
//  UINT16          VT_UI2        uiVal
//  UINT32          VT_UI4        ulVal
//  UINT64          VT_UI8        ullVal
//  INT8            VT_I1         eVal
//  INT16           VT_I2         iVal
//  INT32           VT_I4         lVal
//  INT64           VT_I8         llVal
//  BOOL            VT_BOOL       boolVal
//  GUID            VT_BSTR       bstrVal (guid string)
//  UINT32/UNINT32  VT_UI8        ullVal  (ratio)


// Static definitions
const

  AVENC_H264V_LEVELCOUNT              = 16;
  {$EXTERNALSYM AVENC_H264V_LEVELCOUNT}
  AVENC_H264V_MAX_MBBITS              = 3200;  // Only applies to Baseline, Main, Extended profiles
  {$EXTERNALSYM AVENC_H264V_MAX_MBBITS}

  // See: https://docs.microsoft.com/en-us/windows/win32/directshow/codec-api-properties

  STATIC_CODECAPI_AVEncCommonFormatConstraint         :  TGUID = (D1: $57CBB9B8; D2: $116F; D3: $4951; D4: ($B4, $0C, $C2, $A0, $35, $ED, $8F, $17));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonFormatConstraint}
  STATIC_CODECAPI_GUID_AVEncCommonFormatUnSpecified   :  TGUID = (D1: $AF46A35A; D2: $6024; D3: $4525; D4: ($A4, $8A, $09, $4B, $97, $F5, $B3, $C2));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatUnSpecified}
  STATIC_CODECAPI_GUID_AVEncCommonFormatDVD_V         :  TGUID = (D1: $cc9598c4; D2: $e7fe; D3: $451d; D4: ($b1, $ca, $76, $1b, $c8, $40, $b7, $f3));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatDVD_V}
  STATIC_CODECAPI_GUID_AVEncCommonFormatDVD_DashVR    :  TGUID = (D1: $e55199d6; D2: $044c; D3: $4dae; D4: ($a4, $88, $53, $1e, $d3, $06, $23, $5b));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatDVD_DashVR}
  STATIC_CODECAPI_GUID_AVEncCommonFormatDVD_PlusVR    :  TGUID = (D1: $e74c6f2e; D2: $ec37; D3: $478d; D4: ($9a, $f4, $a5, $e1, $35, $b6, $27, $1c));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatDVD_PlusVR}
  STATIC_CODECAPI_GUID_AVEncCommonFormatVCD           :  TGUID = (D1: $95035bf7; D2: $9d90; D3: $40ff; D4: ($ad, $5c, $5c, $f8, $cf, $71, $ca, $1d));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatVCD}
  STATIC_CODECAPI_GUID_AVEncCommonFormatSVCD          :  TGUID = (D1: $51d85818; D2: $8220; D3: $448c; D4: ($80, $66, $d6, $9b, $ed, $16, $c9, $ad));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatSVCD}
  STATIC_CODECAPI_GUID_AVEncCommonFormatATSC          :  TGUID = (D1: $8d7b897c; D2: $a019; D3: $4670; D4: ($aa, $76, $2e, $dc, $ac, $7a, $c2, $96));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatATSC}
  STATIC_CODECAPI_GUID_AVEncCommonFormatDVB           :  TGUID = (D1: $71830d8f; D2: $6c33; D3: $430d; D4: ($84, $4b, $c2, $70, $5b, $aa, $e6, $db));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatDVB}
  STATIC_CODECAPI_GUID_AVEncCommonFormatMP3           :  TGUID = (D1: $349733cd; D2: $eb08; D3: $4dc2; D4: ($81, $97, $e4, $98, $35, $ef, $82, $8b));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatMP3}
  STATIC_CODECAPI_GUID_AVEncCommonFormatHighMAT       :  TGUID = (D1: $1eabe760; D2: $fb2b; D3: $4928; D4: ($90, $d1, $78, $db, $88, $ee, $e8, $89));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatHighMAT}
  STATIC_CODECAPI_GUID_AVEncCommonFormatHighMPV       :  TGUID = (D1: $a2d25db8; D2: $b8f9; D3: $42c2; D4: ($8b, $c7, $0b, $93, $cf, $60, $47, $88));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncCommonFormatHighMPV}
  STATIC_CODECAPI_AVEncCodecType                      :  TGUID = (D1: $08af4ac1; D2: $f3f2; D3: $4c74; D4: ($9d, $cf, $37, $f2, $ec, $79, $f8, $26));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCodecType}
  STATIC_CODECAPI_GUID_AVEncMPEG1Video                :  TGUID = (D1: $c8dafefe; D2: $da1e; D3: $4774; D4: ($b2, $7d, $11, $83, $0c, $16, $b1, $fe));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncMPEG1Video}
  STATIC_CODECAPI_GUID_AVEncMPEG2Video                :  TGUID = (D1: $046dc19a; D2: $6677; D3: $4aaa; D4: ($a3, $1d, $c1, $ab, $71, $6f, $45, $60));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncMPEG2Video}
  STATIC_CODECAPI_GUID_AVEncMPEG1Audio                :  TGUID = (D1: $d4dd1362; D2: $cd4a; D3: $4cd6; D4: ($81, $38, $b9, $4d, $b4, $54, $2b, $04));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncMPEG1Audio}
  STATIC_CODECAPI_GUID_AVEncMPEG2Audio                :  TGUID = (D1: $ee4cbb1f; D2: $9c3f; D3: $4770; D4: ($92, $b5, $fc, $b7, $c2, $a8, $d3, $81));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncMPEG2Audio}
  STATIC_CODECAPI_GUID_AVEncWMV                       :  TGUID = (D1: $4e0fef9b; D2: $1d43; D3: $41bd; D4: ($b8, $bd, $4d, $7b, $f7, $45, $7a, $2a));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncWMV}
  STATIC_CODECAPI_GUID_AVEndMPEG4Video                :  TGUID = (D1: $dd37b12a; D2: $9503; D3: $4f8b; D4: ($b8, $d0, $32, $4a, $00, $c0, $a1, $cf));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEndMPEG4Video}
  STATIC_CODECAPI_GUID_AVEncH264Video                 :  TGUID = (D1: $95044eab; D2: $31b3; D3: $47de; D4: ($8e, $75, $38, $a4, $2b, $b0, $3e, $28));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncH264Video}
  STATIC_CODECAPI_GUID_AVEncDV                        :  TGUID = (D1: $09b769c7; D2: $3329; D3: $44fb; D4: ($89, $54, $fa, $30, $93, $7d, $3d, $5a));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncDV}
  STATIC_CODECAPI_GUID_AVEncWMAPro                    :  TGUID = (D1: $1955f90c; D2: $33f7; D3: $4a68; D4: ($ab, $81, $53, $f5, $65, $71, $25, $c4));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncWMAPro}
  STATIC_CODECAPI_GUID_AVEncWMALossless               :  TGUID = (D1: $55ca7265; D2: $23d8; D3: $4761; D4: ($90, $31, $b7, $4f, $be, $12, $f4, $c1));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncWMALossless}
  STATIC_CODECAPI_GUID_AVEncWMAVoice                  :  TGUID = (D1: $13ed18cb; D2: $50e8; D3: $4276; D4: ($a2, $88, $a6, $aa, $22, $83, $82, $d9));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncWMAVoice}
  STATIC_CODECAPI_GUID_AVEncDolbyDigitalPro           :  TGUID = (D1: $f5be76cc; D2: $0ff8; D3: $40eb; D4: ($9c, $b1, $bb, $a9, $40, $04, $d4, $4f));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncDolbyDigitalPro}
  STATIC_CODECAPI_GUID_AVEncDolbyDigitalConsumer      :  TGUID = (D1: $c1a7bf6c; D2: $0059; D3: $4bfa; D4: ($94, $ef, $ef, $74, $7a, $76, $8d, $52));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncDolbyDigitalConsumer}
  STATIC_CODECAPI_GUID_AVEncDolbyDigitalPlus          :  TGUID = (D1: $698d1b80; D2: $f7dd; D3: $415c; D4: ($97, $1c, $42, $49, $2a, $20, $56, $c6));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncDolbyDigitalPlus}
  STATIC_CODECAPI_GUID_AVEncDTSHD                     :  TGUID = (D1: $2052e630; D2: $469d; D3: $4bfb; D4: ($80, $ca, $1d, $65, $6e, $7e, $91, $8f));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncDTSHD}
  STATIC_CODECAPI_GUID_AVEncDTS                       :  TGUID = (D1: $45fbcaa2; D2: $5e6e; D3: $4ab0; D4: ($88, $93, $59, $03, $be, $e9, $3a, $cf));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncDTS}
  STATIC_CODECAPI_GUID_AVEncMLP                       :  TGUID = (D1: $05f73e29; D2: $f0d1; D3: $431e; D4: ($a4, $1c, $a4, $74, $32, $ec, $5a, $66));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncMLP}
  STATIC_CODECAPI_GUID_AVEncPCM                       :  TGUID = (D1: $844be7f4; D2: $26cf; D3: $4779; D4: ($b3, $86, $cc, $05, $d1, $87, $99, $0c));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncPCM}
  STATIC_CODECAPI_GUID_AVEncSDDS                      :  TGUID = (D1: $1dc1b82f; D2: $11c8; D3: $4c71; D4: ($b7, $b6, $ee, $3e, $b9, $bc, $2b, $94));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVEncSDDS}
  STATIC_CODECAPI_AVEncCommonRateControlMode          :  TGUID = (D1: $1c0608e9; D2: $370c; D3: $4710; D4: ($8a, $58, $cb, $61, $81, $c4, $24, $23));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonRateControlMode}
  STATIC_CODECAPI_AVEncCommonLowLatency               :  TGUID = (D1: $9d3ecd55; D2: $89e8; D3: $490a; D4: ($97, $0a, $0c, $95, $48, $d5, $a5, $6e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonLowLatency}
  STATIC_CODECAPI_AVEncCommonMultipassMode            :  TGUID = (D1: $22533d4c; D2: $47e1; D3: $41b5; D4: ($93, $52, $a2, $b7, $78, $0e, $7a, $c4));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonMultipassMode}
  STATIC_CODECAPI_AVEncCommonPassStart                :  TGUID = (D1: $6a67739f; D2: $4eb5; D3: $4385; D4: ($99, $28, $f2, $76, $a9, $39, $ef, $95));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonPassStart}
  STATIC_CODECAPI_AVEncCommonPassEnd                  :  TGUID = (D1: $0e3d01bc; D2: $c85c; D3: $467d; D4: ($8b, $60, $c4, $10, $12, $ee, $3b, $f6));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonPassEnd}
  STATIC_CODECAPI_AVEncCommonRealTime                 :  TGUID = (D1: $143a0ff6; D2: $a131; D3: $43da; D4: ($b8, $1e, $98, $fb, $b8, $ec, $37, $8e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonRealTime}
  STATIC_CODECAPI_AVEncCommonQuality                  :  TGUID = (D1: $fcbf57a3; D2: $7ea5; D3: $4b0c; D4: ($96, $44, $69, $b4, $0c, $39, $c3, $91));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonQuality}
  STATIC_CODECAPI_AVEncCommonQualityVsSpeed           :  TGUID = (D1: $98332df8; D2: $03cd; D3: $476b; D4: ($89, $fa, $3f, $9e, $44, $2d, $ec, $9f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonQualityVsSpeed}
  STATIC_CODECAPI_AVEncCommonTranscodeEncodingProfile :  TGUID = (D1: $6947787C; D2: $F508; D3: $4EA9; D4: ($B1, $E9, $A1, $FE, $3A, $49, $FB, $C9));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonTranscodeEncodingProfile}
  STATIC_CODECAPI_AVEncCommonMeanBitRate              :  TGUID = (D1: $f7222374; D2: $2144; D3: $4815; D4: ($b5, $50, $a3, $7f, $8e, $12, $ee, $52));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonMeanBitRate}
  STATIC_CODECAPI_AVEncCommonMeanBitRateInterval      :  TGUID = (D1: $bfaa2f0c; D2: $cb82; D3: $4bc0; D4: ($84, $74, $f0, $6a, $8a, $0d, $02, $58));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonMeanBitRateInterval}
  STATIC_CODECAPI_AVEncCommonMaxBitRate               :  TGUID = (D1: $9651eae4; D2: $39b9; D3: $4ebf; D4: ($85, $ef, $d7, $f4, $44, $ec, $74, $65));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonMaxBitRate}
  STATIC_CODECAPI_AVEncCommonMinBitRate               :  TGUID = (D1: $101405b2; D2: $2083; D3: $4034; D4: ($a8, $06, $ef, $be, $dd, $d7, $c9, $ff));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonMinBitRate}
  STATIC_CODECAPI_AVEncCommonBufferSize               :  TGUID = (D1: $0db96574; D2: $b6a4; D3: $4c8b; D4: ($81, $06, $37, $73, $de, $03, $10, $cd));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonBufferSize}
  STATIC_CODECAPI_AVEncCommonBufferInLevel            :  TGUID = (D1: $d9c5c8db; D2: $fc74; D3: $4064; D4: ($94, $e9, $cd, $19, $f9, $47, $ed, $45));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonBufferInLevel}
  STATIC_CODECAPI_AVEncCommonBufferOutLevel           :  TGUID = (D1: $ccae7f49; D2: $d0bc; D3: $4e3d; D4: ($a5, $7e, $fb, $57, $40, $14, $00, $69));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonBufferOutLevel}
  STATIC_CODECAPI_AVEncCommonStreamEndHandling        :  TGUID = (D1: $6aad30af; D2: $6ba8; D3: $4ccc; D4: ($8f, $ca, $18, $d1, $9b, $ea, $eb, $1c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonStreamEndHandling}
  STATIC_CODECAPI_AVEncStatCommonCompletedPasses      :  TGUID = (D1: $3e5de533; D2: $9df7; D3: $438c; D4: ($85, $4f, $9f, $7d, $d3, $68, $3d, $34));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatCommonCompletedPasses}
  STATIC_CODECAPI_AVEncVideoOutputFrameRate           :  TGUID = (D1: $ea85e7c3; D2: $9567; D3: $4d99; D4: ($87, $c4, $02, $c1, $c2, $78, $ca, $7c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputFrameRate}
  STATIC_CODECAPI_AVEncVideoOutputFrameRateConversion :  TGUID = (D1: $8c068bf4; D2: $369a; D3: $4ba3; D4: ($82, $fd, $b2, $51, $8f, $b3, $39, $6e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputFrameRateConversion}
  STATIC_CODECAPI_AVEncVideoPixelAspectRatio          :  TGUID = (D1: $3cdc718f; D2: $b3e9; D3: $4eb6; D4: ($a5, $7f, $cf, $1f, $1b, $32, $1b, $87));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoPixelAspectRatio}
  STATIC_CODECAPI_AVEncVideoForceSourceScanType       :  TGUID = (D1: $1ef2065f; D2: $058a; D3: $4765; D4: ($a4, $fc, $8a, $86, $4c, $10, $30, $12));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoForceSourceScanType}
  STATIC_CODECAPI_AVEncVideoNoOfFieldsToEncode        :  TGUID = (D1: $61e4bbe2; D2: $4ee0; D3: $40e7; D4: ($80, $ab, $51, $dd, $ee, $be, $62, $91));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoNoOfFieldsToEncode}
  STATIC_CODECAPI_AVEncVideoNoOfFieldsToSkip          :  TGUID = (D1: $a97e1240; D2: $1427; D3: $4c16; D4: ($a7, $f7, $3d, $cf, $d8, $ba, $4c, $c5));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoNoOfFieldsToSkip}

  STATIC_CODECAPI_AVEncVideoEncodeDimension           :  TGUID = (D1: $1074df28; D2: $7e0f; D3: $47a4; D4: ($a4, $53, $cd, $d7, $38, $70, $f5, $ce));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoEncodeDimension}

  STATIC_CODECAPI_AVEncVideoEncodeOffsetOrigin        :  TGUID = (D1: $6bc098fe; D2: $a71a; D3: $4454; D4: ($85, $2e, $4d, $2d, $de, $b2, $cd, $24));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoEncodeOffsetOrigin}
  STATIC_CODECAPI_AVEncVideoDisplayDimension          :  TGUID = (D1: $de053668; D2: $f4ec; D3: $47a9; D4: ($86, $d0, $83, $67, $70, $f0, $c1, $d5));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoDisplayDimension}
  STATIC_CODECAPI_AVEncVideoOutputScanType            :  TGUID = (D1: $460b5576; D2: $842e; D3: $49ab; D4: ($a6, $2d, $b3, $6f, $73, $12, $c9, $db));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputScanType}
  STATIC_CODECAPI_AVEncVideoInverseTelecineEnable     :  TGUID = (D1: $2ea9098b; D2: $e76d; D3: $4ccd; D4: ($a0, $30, $d3, $b8, $89, $c1, $b6, $4c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInverseTelecineEnable}
  STATIC_CODECAPI_AVEncVideoInverseTelecineThreshold  :  TGUID = (D1: $40247d84; D2: $e895; D3: $497f; D4: ($b4, $4c, $b7, $45, $60, $ac, $fe, $27));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInverseTelecineThreshold}
  STATIC_CODECAPI_AVEncVideoSourceFilmContent         :  TGUID = (D1: $1791c64b; D2: $ccfc; D3: $4827; D4: ($a0, $ed, $25, $57, $79, $3b, $2b, $1c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoSourceFilmContent}
  STATIC_CODECAPI_AVEncVideoSourceIsBW                :  TGUID = (D1: $42ffc49b; D2: $1812; D3: $4fdc; D4: ($8d, $24, $70, $54, $c5, $21, $e6, $eb));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoSourceIsBW}
  STATIC_CODECAPI_AVEncVideoFieldSwap                 :  TGUID = (D1: $fefd7569; D2: $4e0a; D3: $49f2; D4: ($9f, $2b, $36, $0e, $a4, $8c, $19, $a2));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoFieldSwap}
  STATIC_CODECAPI_AVEncVideoInputChromaResolution     :  TGUID = (D1: $bb0cec33; D2: $16f1; D3: $47b0; D4: ($8a, $88, $37, $81, $5b, $ee, $17, $39));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputChromaResolution}
  STATIC_CODECAPI_AVEncVideoOutputChromaResolution    :  TGUID = (D1: $6097b4c9; D2: $7c1d; D3: $4e64; D4: ($bf, $cc, $9e, $97, $65, $31, $8a, $e7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputChromaResolution}
  STATIC_CODECAPI_AVEncVideoInputChromaSubsampling    :  TGUID = (D1: $a8e73a39; D2: $4435; D3: $4ec3; D4: ($a6, $ea, $98, $30, $0f, $4b, $36, $f7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputChromaSubsampling}
  STATIC_CODECAPI_AVEncVideoOutputChromaSubsampling   :  TGUID = (D1: $fa561c6c; D2: $7d17; D3: $44f0; D4: ($83, $c9, $32, $ed, $12, $e9, $63, $43));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputChromaSubsampling}
  STATIC_CODECAPI_AVEncVideoInputColorPrimaries       :  TGUID = (D1: $c24d783f; D2: $7ce6; D3: $4278; D4: ($90, $ab, $28, $a4, $f1, $e5, $f8, $6c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputColorPrimaries}
  STATIC_CODECAPI_AVEncVideoOutputColorPrimaries      :  TGUID = (D1: $be95907c; D2: $9d04; D3: $4921; D4: ($89, $85, $a6, $d6, $d8, $7d, $1a, $6c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputColorPrimaries}
  STATIC_CODECAPI_AVEncVideoInputColorTransferFunction:  TGUID = (D1: $8c056111; D2: $a9c3; D3: $4b08; D4: ($a0, $a0, $ce, $13, $f8, $a2, $7c, $75));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputColorTransferFunction}
  STATIC_CODECAPI_AVEncVideoOutputColorTransferFunction :  TGUID = (D1: $4a7f884a; D2: $ea11; D3: $460d; D4: ($bf, $57, $b8, $8b, $c7, $59, $00, $de));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputColorTransferFunction}
  STATIC_CODECAPI_AVEncVideoInputColorTransferMatrix  :  TGUID = (D1: $52ed68b9; D2: $72d5; D3: $4089; D4: ($95, $8d, $f5, $40, $5d, $55, $08, $1c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputColorTransferMatrix}
  STATIC_CODECAPI_AVEncVideoOutputColorTransferMatrix :  TGUID = (D1: $a9b90444; D2: $af40; D3: $4310; D4: ($8f, $be, $ed, $6d, $93, $3f, $89, $2b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputColorTransferMatrix}
  STATIC_CODECAPI_AVEncVideoInputColorLighting        :  TGUID = (D1: $46a99549; D2: $0015; D3: $4a45; D4: ($9c, $30, $1d, $5c, $fa, $25, $83, $16));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputColorLighting}
  STATIC_CODECAPI_AVEncVideoOutputColorLighting       :  TGUID = (D1: $0e5aaac6; D2: $ace6; D3: $4c5c; D4: ($99, $8e, $1a, $8c, $9c, $6c, $0f, $89));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputColorLighting}
  STATIC_CODECAPI_AVEncVideoInputColorNominalRange    :  TGUID = (D1: $16cf25c6; D2: $a2a6; D3: $48e9; D4: ($ae, $80, $21, $ae, $c4, $1d, $42, $7e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInputColorNominalRange}
  STATIC_CODECAPI_AVEncVideoOutputColorNominalRange   :  TGUID = (D1: $972835ed; D2: $87b5; D3: $4e95; D4: ($95, $00, $c7, $39, $58, $56, $6e, $54));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoOutputColorNominalRange}
  STATIC_CODECAPI_AVEncInputVideoSystem               :  TGUID = (D1: $bede146d; D2: $b616; D3: $4dc7; D4: ($92, $b2, $f5, $d9, $fa, $92, $98, $f7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncInputVideoSystem}
  STATIC_CODECAPI_AVEncVideoHeaderDropFrame           :  TGUID = (D1: $6ed9e124; D2: $7925; D3: $43fe; D4: ($97, $1b, $e0, $19, $f6, $22, $22, $b4));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoHeaderDropFrame}
  STATIC_CODECAPI_AVEncVideoHeaderHours               :  TGUID = (D1: $2acc7702; D2: $e2da; D3: $4158; D4: ($bf, $9b, $88, $88, $01, $29, $d7, $40));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoHeaderHours}
  STATIC_CODECAPI_AVEncVideoHeaderMinutes             :  TGUID = (D1: $dc1a99ce; D2: $0307; D3: $408b; D4: ($88, $0b, $b8, $34, $8e, $e8, $ca, $7f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoHeaderMinutes}
  STATIC_CODECAPI_AVEncVideoHeaderSeconds             :  TGUID = (D1: $4a2e1a05; D2: $a780; D3: $4f58; D4: ($81, $20, $9a, $44, $9d, $69, $65, $6b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoHeaderSeconds}
  STATIC_CODECAPI_AVEncVideoHeaderFrames              :  TGUID = (D1: $afd5f567; D2: $5c1b; D3: $4adc; D4: ($bd, $af, $73, $56, $10, $38, $14, $36));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoHeaderFrames}
  STATIC_CODECAPI_AVEncVideoDefaultUpperFieldDominant :  TGUID = (D1: $810167c4; D2: $0bc1; D3: $47ca; D4: ($8f, $c2, $57, $05, $5a, $14, $74, $a5));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoDefaultUpperFieldDominant}
  STATIC_CODECAPI_AVEncVideoCBRMotionTradeoff         :  TGUID = (D1: $0d49451e; D2: $18d5; D3: $4367; D4: ($a4, $ef, $32, $40, $df, $16, $93, $c4));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoCBRMotionTradeoff}
  STATIC_CODECAPI_AVEncVideoCodedVideoAccessUnitSize  :  TGUID = (D1: $b4b10c15; D2: $14a7; D3: $4ce8; D4: ($b1, $73, $dc, $90, $a0, $b4, $fc, $db));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoCodedVideoAccessUnitSize}
  STATIC_CODECAPI_AVEncVideoMaxKeyframeDistance       :  TGUID = (D1: $2987123a; D2: $ba93; D3: $4704; D4: ($b4, $89, $ec, $1e, $5f, $25, $29, $2c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMaxKeyframeDistance}
  STATIC_CODECAPI_AVEncH264CABACEnable                :  TGUID = (D1: $ee6cad62; D2: $d305; D3: $4248; D4: ($a5, $e, $e1, $b2, $55, $f7, $ca, $f8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncH264CABACEnable}
  STATIC_CODECAPI_AVEncVideoContentType               :  TGUID = (D1: $66117aca; D2: $eb77; D3: $459d; D4: ($93, $c, $a4, $8d, $9d, $6, $83, $fc));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoContentType}
  STATIC_CODECAPI_AVEncNumWorkerThreads               :  TGUID = (D1: $b0c8bf60; D2: $16f7; D3: $4951; D4: ($a3, $b, $1d, $b1, $60, $92, $93, $d6));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncNumWorkerThreads}
  STATIC_CODECAPI_AVEncVideoEncodeQP                  :  TGUID = (D1: $2cb5696b; D2: $23fb; D3: $4ce1; D4: ($a0, $f9, $ef, $5b, $90, $fd, $55, $ca));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoEncodeQP}
  STATIC_CODECAPI_AVEncVideoMinQP                     :  TGUID = (D1: $0ee22c6a; D2: $a37c; D3: $4568; D4: ($b5, $f1, $9d, $4c, $2b, $3a, $b8, $86));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMinQP}
  STATIC_CODECAPI_AVEncVideoForceKeyFrame             :  TGUID = (D1: $398c1b98; D2: $8353; D3: $475a; D4: ($9e, $f2, $8f, $26, $5d, $26, $3, $45));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoForceKeyFrame}
  STATIC_CODECAPI_AVEncH264SPSID                      :  TGUID = (D1: $50f38f51; D2: $2b79; D3: $40e3; D4: ($b3, $9c, $7e, $9f, $a0, $77, $5, $1));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncH264SPSID}
  STATIC_CODECAPI_AVEncH264PPSID                      :  TGUID = (D1: $bfe29ec2; D2: $56c; D3: $4d68; D4: ($a3, $8d, $ae, $59, $44, $c8, $58, $2e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncH264PPSID}
  STATIC_CODECAPI_AVEncAdaptiveMode                   :  TGUID = (D1: $4419b185; D2: $da1f; D3: $4f53; D4: ($bc, $76, $9, $7d, $c, $1e, $fb, $1e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAdaptiveMode}
  STATIC_CODECAPI_AVEncVideoTemporalLayerCount        :  TGUID = (D1: $19caebff; D2: $b74d; D3: $4cfd; D4: ($8c, $27, $c2, $f9, $d9, $7d, $5f, $52));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoTemporalLayerCount}
  STATIC_CODECAPI_AVEncVideoUsage                     :  TGUID = (D1: $1f636849; D2: $5dc1; D3: $49f1; D4: ($b1, $d8, $ce, $3c, $f6, $2e, $a3, $85));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoUsage}
  STATIC_CODECAPI_AVEncVideoSelectLayer               :  TGUID = (D1: $eb1084f5; D2: $6aaa; D3: $4914; D4: ($bb, $2f, $61, $47, $22, $7f, $12, $e7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoSelectLayer}
  STATIC_CODECAPI_AVEncVideoRateControlParams         :  TGUID = (D1: $87d43767; D2: $7645; D3: $44ec; D4: ($b4, $38, $d3, $32, $2f, $bc, $a2, $9f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoRateControlParams}
  STATIC_CODECAPI_AVEncVideoSupportedControls         :  TGUID = (D1: $d3f40fdd; D2: $77b9; D3: $473d; D4: ($81, $96, $6, $12, $59, $e6, $9c, $ff));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoSupportedControls}
  STATIC_CODECAPI_AVEncVideoEncodeFrameTypeQP         :  TGUID = (D1: $aa70b610; D2: $e03f; D3: $450c; D4: ($ad, $07, $07, $31, $4e, $63, $9c, $e7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoEncodeFrameTypeQP}
  STATIC_CODECAPI_AVEncSliceControlMode               :  TGUID = (D1: $e9e782ef; D2: $5f18; D3: $44c9; D4: ($a9, $0b, $e9, $c3, $c2, $c1, $7b, $0b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncSliceControlMode}
  STATIC_CODECAPI_AVEncSliceControlSize               :  TGUID = (D1: $92f51df3; D2: $07a5; D3: $4172; D4: ($ae, $fe, $c6, $9c, $a3, $b6, $0e, $35));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncSliceControlSize}
  STATIC_CODECAPI_AVEncSliceGenerationMode            :  TGUID = (D1: $8a6bc67f; D2: $9497; D3: $4286; D4: ($b4, $6b, $02, $db, $8d, $60, $ed, $bc));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncSliceGenerationMode}
  STATIC_CODECAPI_AVEncVideoMaxNumRefFrame            :  TGUID = (D1: $964829ed; D2: $94f9; D3: $43b4; D4: ($b7, $4d, $ef, $40, $94, $4b, $69, $a0));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMaxNumRefFrame}
  STATIC_CODECAPI_AVEncVideoMeanAbsoluteDifference    :  TGUID = (D1: $e5c0c10f; D2: $81a4; D3: $422d; D4: ($8c, $3f, $b4, $74, $a4, $58, $13, $36));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMeanAbsoluteDifference}
  STATIC_CODECAPI_AVEncVideoMaxQP                     :  TGUID = (D1: $3daf6f66; D2: $a6a7; D3: $45e0; D4: ($a8, $e5, $f2, $74, $3f, $46, $a3, $a2));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMaxQP}
  STATIC_CODECAPI_AVEncVideoLTRBufferControl          :  TGUID = (D1: $a4a0e93d; D2: $4cbc; D3: $444c; D4: ($89, $f4, $82, $6d, $31, $0e, $92, $a7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoLTRBufferControl}
  STATIC_CODECAPI_AVEncVideoMarkLTRFrame              :  TGUID = (D1: $e42f4748; D2: $a06d; D3: $4ef9; D4: ($8c, $ea, $3d, $05, $fd, $e3, $bd, $3b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMarkLTRFrame}
  STATIC_CODECAPI_AVEncVideoUseLTRFrame               :  TGUID = (D1: $00752db8; D2: $55f7; D3: $4f80; D4: ($89, $5b, $27, $63, $91, $95, $f2, $ad));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoUseLTRFrame}
  STATIC_CODECAPI_AVEncVideoROIEnabled                :  TGUID = (D1: $d74f7f18; D2: $44dd; D3: $4b85; D4: ($ab, $a3, $5, $d9, $f4, $2a, $82, $80));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoROIEnabled}
  STATIC_CODECAPI_AVEncVideoDirtyRectEnabled          :  TGUID = (D1: $8acb8fdd; D2: $5e0c; D3: $4c66; D4: ($87, $29, $b8, $f6, $29, $ab, $04, $fb));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoDirtyRectEnabled}
  STATIC_CODECAPI_AVScenarioInfo                      :  TGUID = (D1: $b28a6e64; D2: $3ff9; D3: $446a; D4: ($8a, $4b, $0d, $7a, $53, $41, $32, $36));
  {$EXTERNALSYM STATIC_CODECAPI_AVScenarioInfo}
  STATIC_CODECAPI_AVEncMPVGOPSizeMin                  :  TGUID = (D1: $7155cf20; D2: $d440; D3: $4852; D4: ($ad, $0f, $9c, $4a, $bf, $e3, $7a, $6a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGOPSizeMin}
  STATIC_CODECAPI_AVEncMPVGOPSizeMax                  :  TGUID = (D1: $fe7de4c4; D2: $1936; D3: $4fe2; D4: ($bd, $f7, $1f, $18, $ca, $1d, $00, $1f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGOPSizeMax}
  STATIC_CODECAPI_AVEncVideoMaxCTBSize                :  TGUID = (D1: $822363ff; D2: $cec8; D3: $43e5; D4: ($92, $fd, $e0, $97, $48, $84, $85, $e9));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMaxCTBSize}
  STATIC_CODECAPI_AVEncVideoCTBSize                   :  TGUID = (D1: $d47db8b2; D2: $e73b; D3: $4cb9; D4: ($8c, $3e, $bd, $87, $7d, $06, $d7, $7b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoCTBSize}
  STATIC_CODECAPI_VideoEncoderDisplayContentType      :  TGUID = (D1: $79b90b27; D2: $f4b1; D3: $42dc; D4: ($9d, $d7, $cd, $af, $81, $35, $c4, $00));
  {$EXTERNALSYM STATIC_CODECAPI_VideoEncoderDisplayContentType}
  STATIC_CODECAPI_AVEncEnableVideoProcessing          :  TGUID = (D1: $006f4bf6; D2: $0ea3; D3: $4d42; D4: ($87, $02, $b5, $d8, $be, $0f, $7a, $92));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncEnableVideoProcessing}
  STATIC_CODECAPI_AVEncVideoGradualIntraRefresh       :  TGUID = (D1: $8f347dee; D2: $cb0d; D3: $49ba; D4: ($b4, $62, $db, $69, $27, $ee, $21, $01));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoGradualIntraRefresh}
  STATIC_CODECAPI_GetOPMContext                       :  TGUID = (D1: $2f036c05; D2: $4c14; D3: $4689; D4: ($88, $39, $29, $4c, $6d, $73, $e0, $53));
  {$EXTERNALSYM STATIC_CODECAPI_GetOPMContext}
  STATIC_CODECAPI_SetHDCPManagerContext               :  TGUID = (D1: $6d2d1fc8; D2: $3dc9; D3: $47eb; D4: ($a1, $a2, $47, $1c, $80, $cd, $60, $d0));
  {$EXTERNALSYM STATIC_CODECAPI_SetHDCPManagerContext}
  STATIC_CODECAPI_AVEncVideoMaxTemporalLayers         :  TGUID = (D1: $9c668cfe; D2: $08e1; D3: $424a; D4: ($93, $4e, $b7, $64, $b0, $64, $80, $2a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoMaxTemporalLayers}
  STATIC_CODECAPI_AVEncVideoNumGOPsPerIDR             :  TGUID = (D1: $83bc5bdb; D2: $5b89; D3: $4521; D4: ($8f, $66, $33, $15, $1c, $37, $31, $76));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoNumGOPsPerIDR}
  STATIC_CODECAPI_AVEncCommonAllowFrameDrops          :  TGUID = (D1: $d8477dcb; D2: $9598; D3: $48e3; D4: ($8d, $0c, $75, $2b, $f2, $06, $09, $3e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncCommonAllowFrameDrops}
  STATIC_CODECAPI_AVEncVideoIntraLayerPrediction      :  TGUID = (D1: $d3af46b8; D2: $bf47; D3: $44bb; D4: ($a2, $83, $69, $f0, $b0, $22, $8f, $f9));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoIntraLayerPrediction}
  STATIC_CODECAPI_AVEncVideoInstantTemporalUpSwitching  :	TGUID = (D1: $a3308307; D2: $0d96; D3: $4ba4; D4: ($b1, $f0, $b9, $1a, $5e, $49, $df, $10));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncVideoInstantTemporalUpSwitching}
  STATIC_CODECAPI_AVEncLowPowerEncoder                :	TGUID = (D1: $b668d582; D2: $8bad; D3: $4f6a; D4: ($91, $41, $37, $5a, $95, $35, $8b, $6d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncLowPowerEncoder}
  STATIC_CODECAPI_AVEnableInLoopDeblockFilter         :	TGUID = (D1: $d2e8e399; D2: $0623; D3: $4bf3; D4: ($92, $a8, $4d, $18, $18, $52, $9d, $ed));
  {$EXTERNALSYM STATIC_CODECAPI_AVEnableInLoopDeblockFilter}

  STATIC_CODECAPI_AVEncMuxOutputStreamType            :  TGUID = (D1: $cedd9e8f; D2: $34d3; D3: $44db; D4: ($a1, $d8, $f8, $15, $20, $25, $4f, $3e));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMuxOutputStreamType}

  STATIC_CODECAPI_AVEncStatVideoOutputFrameRate       :  TGUID = (D1: $be747849; D2: $9ab4; D3: $4a63; D4: ($98, $fe, $f1, $43, $f0, $4f, $8e, $e9));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatVideoOutputFrameRate}
  STATIC_CODECAPI_AVEncStatVideoCodedFrames           :  TGUID = (D1: $d47f8d61; D2: $6f5a; D3: $4a26; D4: ($bb, $9f, $cd, $95, $18, $46, $2b, $cd));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatVideoCodedFrames}
  STATIC_CODECAPI_AVEncStatVideoTotalFrames           :  TGUID = (D1: $fdaa9916; D2: $119a; D3: $4222; D4: ($9a, $d6, $3f, $7c, $ab, $99, $cc, $8b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatVideoTotalFrames}
  STATIC_CODECAPI_AVEncAudioIntervalToEncode          :  TGUID = (D1: $866e4b4d; D2: $725a; D3: $467c; D4: ($bb, $01, $b4, $96, $b2, $3b, $25, $f9));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioIntervalToEncode}
  STATIC_CODECAPI_AVEncAudioIntervalToSkip            :  TGUID = (D1: $88c15f94; D2: $c38c; D3: $4796; D4: ($a9, $e8, $96, $e9, $67, $98, $3f, $26));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioIntervalToSkip}
  STATIC_CODECAPI_AVEncAudioDualMono                  :  TGUID = (D1: $3648126b; D2: $a3e8; D3: $4329; D4: ($9b, $3a, $5c, $e5, $66, $a4, $3b, $d3));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioDualMono}
  STATIC_CODECAPI_AVEncAudioMeanBitRate               :  TGUID = (D1: $921295bb; D2: $4fca; D3: $4679; D4: ($aa, $b8, $9e, $2a, $1d, $75, $33, $84));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMeanBitRate}

  STATIC_CODECAPI_AVEncAudioMapDestChannel0           :  TGUID = (D1: $bc5d0b60; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel0}
  STATIC_CODECAPI_AVEncAudioMapDestChannel1           :  TGUID = (D1: $bc5d0b61; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel1}
  STATIC_CODECAPI_AVEncAudioMapDestChannel2           :  TGUID = (D1: $bc5d0b62; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel2}
  STATIC_CODECAPI_AVEncAudioMapDestChannel3           :  TGUID = (D1: $bc5d0b63; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel3}
  STATIC_CODECAPI_AVEncAudioMapDestChannel4           :  TGUID = (D1: $bc5d0b64; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel4}
  STATIC_CODECAPI_AVEncAudioMapDestChannel5           :  TGUID = (D1: $bc5d0b65; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel5}
  STATIC_CODECAPI_AVEncAudioMapDestChannel6           :  TGUID = (D1: $bc5d0b66; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel6}
  STATIC_CODECAPI_AVEncAudioMapDestChannel7           :  TGUID = (D1: $bc5d0b67; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel7}
  STATIC_CODECAPI_AVEncAudioMapDestChannel8           :  TGUID = (D1: $bc5d0b68; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel8}
  STATIC_CODECAPI_AVEncAudioMapDestChannel9           :  TGUID = (D1: $bc5d0b69; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel9}
  STATIC_CODECAPI_AVEncAudioMapDestChannel10          :  TGUID = (D1: $bc5d0b6a; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel10}
  STATIC_CODECAPI_AVEncAudioMapDestChannel11          :  TGUID = (D1: $bc5d0b6b; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel11}
  STATIC_CODECAPI_AVEncAudioMapDestChannel12          :  TGUID = (D1: $bc5d0b6c; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel12}
  STATIC_CODECAPI_AVEncAudioMapDestChannel13          :  TGUID = (D1: $bc5d0b6d; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel13}
  STATIC_CODECAPI_AVEncAudioMapDestChannel14          :  TGUID = (D1: $bc5d0b6e; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel14}
  STATIC_CODECAPI_AVEncAudioMapDestChannel15          :  TGUID = (D1: $bc5d0b6f; D2: $df6a; D3: $4e16; D4: ($98, $03, $b8, $20, $07, $a3, $0c, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioMapDestChannel15}

  STATIC_CODECAPI_AVEncAudioInputContent              :  TGUID = (D1: $3e226c2b; D2: $60b9; D3: $4a39; D4: ($b0, $0b, $a7, $b4, $0f, $70, $d5, $66));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncAudioInputContent}
  STATIC_CODECAPI_AVEncStatAudioPeakPCMValue          :  TGUID = (D1: $dce7fd34; D2: $dc00; D3: $4c16; D4: ($82, $1b, $35, $d9, $eb, $00, $fb, $1a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatAudioPeakPCMValue}
  STATIC_CODECAPI_AVEncStatAudioAveragePCMValue       :  TGUID = (D1: $979272f8; D2: $d17f; D3: $4e32; D4: ($bb, $73, $4e, $73, $1c, $68, $ba, $2d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatAudioAveragePCMValue}
  STATIC_CODECAPI_AVEncStatAudioAverageBPS            :  TGUID = (D1: $ca6724db; D2: $7059; D3: $4351; D4: ($8b, $43, $f8, $21, $98, $82, $6a, $14));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatAudioAverageBPS}
  STATIC_CODECAPI_AVEncStatAverageBPS                 :  TGUID = (D1: $ca6724db; D2: $7059; D3: $4351; D4: ($8b, $43, $f8, $21, $98, $82, $6a, $14));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatAverageBPS}

  STATIC_CODECAPI_AVEncStatHardwareProcessorUtilitization :  TGUID = (D1: $995dc027;
                                                                      D2: $cb95;
                                                                      D3: $49e6;
                                                                      D4: ($b9, $1b, $59, $67, $75, $3c, $dc, $b8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatHardwareProcessorUtilitization}

  STATIC_CODECAPI_AVEncStatHardwareBandwidthUtilitization :  TGUID = (D1: $0124ba9b;
                                                                      D2: $dc41;
                                                                      D3: $4826;
                                                                      D4: ($b4, $5f, $18, $ac, $01, $b3, $d5, $a8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatHardwareBandwidthUtilitization}

  STATIC_CODECAPI_AVEncMPVGOPSize                     :  TGUID = (D1: $95f31b26; D2: $95a4; D3: $41aa; D4: ($93, $03, $24, $6a, $7f, $c6, $ee, $f1));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGOPSize}
  STATIC_CODECAPI_AVEncMPVGOPOpen                     :  TGUID = (D1: $b1d5d4a6; D2: $3300; D3: $49b1; D4: ($ae, $61, $a0, $99, $37, $ab, $0e, $49));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGOPOpen}
  STATIC_CODECAPI_AVEncMPVDefaultBPictureCount        :  TGUID = (D1: $8d390aac; D2: $dc5c; D3: $4200; D4: ($b5, $7f, $81, $4d, $04, $ba, $ba, $b2));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVDefaultBPictureCount}
  STATIC_CODECAPI_AVEncMPVProfile                     :  TGUID = (D1: $dabb534a; D2: $1d99; D3: $4284; D4: ($97, $5a, $d9, $0e, $22, $39, $ba, $a1));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVProfile}
  STATIC_CODECAPI_AVEncMPVLevel                       :  TGUID = (D1: $6ee40c40; D2: $a60c; D3: $41ef; D4: ($8f, $50, $37, $c2, $24, $9e, $2c, $b3));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVLevel}
  STATIC_CODECAPI_AVEncMPVFrameFieldMode              :  TGUID = (D1: $acb5de96; D2: $7b93; D3: $4c2f; D4: ($88, $25, $b0, $29, $5f, $a9, $3b, $f4));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVFrameFieldMode}
  STATIC_CODECAPI_AVEncMPVAddSeqEndCode               :  TGUID = (D1: $a823178f; D2: $57df; D3: $4c7a; D4: ($b8, $fd, $e5, $ec, $88, $87, $70, $8d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVAddSeqEndCode}
  STATIC_CODECAPI_AVEncMPVGOPSInSeq                   :  TGUID = (D1: $993410d4; D2: $2691; D3: $4192; D4: ($99, $78, $98, $dc, $26, $03, $66, $9f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGOPSInSeq}
  STATIC_CODECAPI_AVEncMPVUseConcealmentMotionVectors :  TGUID = (D1: $ec770cf3; D2: $6908; D3: $4b4b; D4: ($aa, $30, $7f, $b9, $86, $21, $4f, $ea));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVUseConcealmentMotionVectors}
  STATIC_CODECAPI_AVEncMPVSceneDetection              :  TGUID = (D1: $552799f1; D2: $db4c; D3: $405b; D4: ($8a, $3a, $c9, $3f, $2d, $06, $74, $dc));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVSceneDetection}
  STATIC_CODECAPI_AVEncMPVGenerateHeaderSeqExt        :  TGUID = (D1: $d5e78611; D2: $082d; D3: $4e6b; D4: ($98, $af, $0f, $51, $ab, $13, $92, $22));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGenerateHeaderSeqExt}
  STATIC_CODECAPI_AVEncMPVGenerateHeaderSeqDispExt    :  TGUID = (D1: $6437aa6f; D2: $5a3c; D3: $4de9; D4: ($8a, $16, $53, $d9, $c4, $ad, $32, $6f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGenerateHeaderSeqDispExt}
  STATIC_CODECAPI_AVEncMPVGenerateHeaderPicExt        :  TGUID = (D1: $1b8464ab; D2: $944f; D3: $45f0; D4: ($b7, $4e, $3a, $58, $da, $d1, $1f, $37));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGenerateHeaderPicExt}
  STATIC_CODECAPI_AVEncMPVGenerateHeaderPicDispExt    :  TGUID = (D1: $c6412f84; D2: $c03f; D3: $4f40; D4: ($a0, $0c, $42, $93, $df, $83, $95, $bb));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGenerateHeaderPicDispExt}
  STATIC_CODECAPI_AVEncMPVGenerateHeaderSeqScaleExt   :  TGUID = (D1: $0722d62f; D2: $dd59; D3: $4a86; D4: ($9c, $d5, $64, $4f, $8e, $26, $53, $d8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVGenerateHeaderSeqScaleExt}
  STATIC_CODECAPI_AVEncMPVScanPattern                 :  TGUID = (D1: $7f8a478e; D2: $7bbb; D3: $4ae2; D4: ($b2, $fc, $96, $d1, $7f, $c4, $a2, $d6));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVScanPattern}
  STATIC_CODECAPI_AVEncMPVIntraDCPrecision            :  TGUID = (D1: $a0116151; D2: $cbc8; D3: $4af3; D4: ($97, $dc, $d0, $0c, $ce, $b8, $2d, $79));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVIntraDCPrecision}
  STATIC_CODECAPI_AVEncMPVQScaleType                  :  TGUID = (D1: $2b79ebb7; D2: $f484; D3: $4af7; D4: ($bb, $58, $a2, $a1, $88, $c5, $cb, $be));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVQScaleType}
  STATIC_CODECAPI_AVEncMPVIntraVLCTable               :  TGUID = (D1: $a2b83ff5; D2: $1a99; D3: $405a; D4: ($af, $95, $c5, $99, $7d, $55, $8d, $3a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVIntraVLCTable}
  STATIC_CODECAPI_AVEncMPVQuantMatrixIntra            :  TGUID = (D1: $9bea04f3; D2: $6621; D3: $442c; D4: ($8b, $a1, $3a, $c3, $78, $97, $96, $98));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVQuantMatrixIntra}
  STATIC_CODECAPI_AVEncMPVQuantMatrixNonIntra         :  TGUID = (D1: $87f441d8; D2: $0997; D3: $4beb; D4: ($a0, $8e, $85, $73, $d4, $09, $cf, $75));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVQuantMatrixNonIntra}
  STATIC_CODECAPI_AVEncMPVQuantMatrixChromaIntra      :  TGUID = (D1: $9eb9ecd4; D2: $018d; D3: $4ffd; D4: ($8f, $2d, $39, $e4, $9f, $07, $b1, $7a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVQuantMatrixChromaIntra}
  STATIC_CODECAPI_AVEncMPVQuantMatrixChromaNonIntra   :  TGUID = (D1: $1415b6b1; D2: $362a; D3: $4338; D4: ($ba, $9a, $1e, $f5, $87, $03, $c0, $5b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPVQuantMatrixChromaNonIntra}
  STATIC_CODECAPI_AVEncMPALayer                       :  TGUID = (D1: $9d377230; D2: $f91b; D3: $453d; D4: ($9c, $e0, $78, $44, $54, $14, $c2, $2d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPALayer}
  STATIC_CODECAPI_AVEncMPACodingMode                  :  TGUID = (D1: $b16ade03; D2: $4b93; D3: $43d7; D4: ($a5, $50, $90, $b4, $fe, $22, $45, $37));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPACodingMode}
  STATIC_CODECAPI_AVEncDDService                      :  TGUID = (D1: $d2e1bec7; D2: $5172; D3: $4d2a; D4: ($a5, $0e, $2f, $3b, $82, $b1, $dd, $f8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDService}
  STATIC_CODECAPI_AVEncDDDialogNormalization          :  TGUID = (D1: $d7055acf; D2: $f125; D3: $437d; D4: ($a7, $04, $79, $c7, $9f, $04, $04, $a8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDDialogNormalization}
  STATIC_CODECAPI_AVEncDDCentreDownMixLevel           :  TGUID = (D1: $e285072c; D2: $c958; D3: $4a81; D4: ($af, $d2, $e5, $e0, $da, $f1, $b1, $48));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDCentreDownMixLevel}
  STATIC_CODECAPI_AVEncDDSurroundDownMixLevel         :  TGUID = (D1: $7b20d6e5; D2: $0bcf; D3: $4273; D4: ($a4, $87, $50, $6b, $04, $79, $97, $e9));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDSurroundDownMixLevel}
  STATIC_CODECAPI_AVEncDDProductionInfoExists         :  TGUID = (D1: $b0b7fe5f; D2: $b6ab; D3: $4f40; D4: ($96, $4d, $8d, $91, $f1, $7c, $19, $e8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDProductionInfoExists}
  STATIC_CODECAPI_AVEncDDProductionRoomType           :  TGUID = (D1: $dad7ad60; D2: $23d8; D3: $4ab7; D4: ($a2, $84, $55, $69, $86, $d8, $a6, $fe));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDProductionRoomType}
  STATIC_CODECAPI_AVEncDDProductionMixLevel           :  TGUID = (D1: $301d103a; D2: $cbf9; D3: $4776; D4: ($88, $99, $7c, $15, $b4, $61, $ab, $26));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDProductionMixLevel}
  STATIC_CODECAPI_AVEncDDCopyright                    :  TGUID = (D1: $8694f076; D2: $cd75; D3: $481d; D4: ($a5, $c6, $a9, $04, $dc, $c8, $28, $f0));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDCopyright}
  STATIC_CODECAPI_AVEncDDOriginalBitstream            :  TGUID = (D1: $966ae800; D2: $5bd3; D3: $4ff9; D4: ($95, $b9, $d3, $05, $66, $27, $38, $56));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDOriginalBitstream}
  STATIC_CODECAPI_AVEncDDDigitalDeemphasis            :  TGUID = (D1: $e024a2c2; D2: $947c; D3: $45ac; D4: ($87, $d8, $f1, $03, $0c, $5c, $00, $82));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDDigitalDeemphasis}
  STATIC_CODECAPI_AVEncDDDCHighPassFilter             :  TGUID = (D1: $9565239f; D2: $861c; D3: $4ac8; D4: ($bf, $da, $e0, $0c, $b4, $db, $85, $48));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDDCHighPassFilter}
  STATIC_CODECAPI_AVEncDDChannelBWLowPassFilter       :  TGUID = (D1: $e197821d; D2: $d2e7; D3: $43e2; D4: ($ad, $2c, $00, $58, $2f, $51, $85, $45));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDChannelBWLowPassFilter}
  STATIC_CODECAPI_AVEncDDLFELowPassFilter             :  TGUID = (D1: $d3b80f6f; D2: $9d15; D3: $45e5; D4: ($91, $be, $01, $9c, $3f, $ab, $1f, $01));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDLFELowPassFilter}
  STATIC_CODECAPI_AVEncDDSurround90DegreeePhaseShift  :  TGUID = (D1: $25ecec9d; D2: $3553; D3: $42c0; D4: ($bb, $56, $d2, $57, $92, $10, $4f, $80));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDSurround90DegreeePhaseShift}
  STATIC_CODECAPI_AVEncDDSurround3dBAttenuation       :  TGUID = (D1: $4d43b99d; D2: $31e2; D3: $48b9; D4: ($bf, $2e, $5c, $bf, $1a, $57, $27, $84));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDSurround3dBAttenuation}
  STATIC_CODECAPI_AVEncDDDynamicRangeCompressionControl :  TGUID = (D1: $cfc2ff6d; D2: $79b8; D3: $4b8d; D4: ($a8, $aa, $a0, $c9, $bd, $1c, $29, $40));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDDynamicRangeCompressionControl}
  STATIC_CODECAPI_AVEncDDRFPreEmphasisFilter          :  TGUID = (D1: $21af44c0; D2: $244e; D3: $4f3d; D4: ($a2, $cc, $3d, $30, $68, $b2, $e7, $3f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDRFPreEmphasisFilter}
  STATIC_CODECAPI_AVEncDDSurroundExMode               :  TGUID = (D1: $91607cee; D2: $dbdd; D3: $4eb6; D4: ($bc, $a2, $aa, $df, $af, $a3, $dd, $68));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDSurroundExMode}
  STATIC_CODECAPI_AVEncDDPreferredStereoDownMixMode   :  TGUID = (D1: $7f4e6b31; D2: $9185; D3: $403d; D4: ($b0, $a2, $76, $37, $43, $e6, $f0, $63));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDPreferredStereoDownMixMode}
  STATIC_CODECAPI_AVEncDDLtRtCenterMixLvl_x10         :  TGUID = (D1: $dca128a2; D2: $491f; D3: $4600; D4: ($b2, $da, $76, $e3, $34, $4b, $41, $97));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDLtRtCenterMixLvl_x10}
  STATIC_CODECAPI_AVEncDDLtRtSurroundMixLvl_x10       :  TGUID = (D1: $212246c7; D2: $3d2c; D3: $4dfa; D4: ($bc, $21, $65, $2a, $90, $98, $69, $0d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDLtRtSurroundMixLvl_x10}
  STATIC_CODECAPI_AVEncDDLoRoCenterMixLvl_x10         :  TGUID = (D1: $1cfba222; D2: $25b3; D3: $4bf4; D4: ($9b, $fd, $e7, $11, $12, $67, $85, $8c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDLoRoCenterMixLvl_x10}
  STATIC_CODECAPI_AVEncDDLoRoSurroundMixLvl_x10       :  TGUID = (D1: $e725cff6; D2: $eb56; D3: $40c7; D4: ($84, $50, $2b, $93, $67, $e9, $15, $55));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDLoRoSurroundMixLvl_x10}
  STATIC_CODECAPI_AVEncDDAtoDConverterType            :  TGUID = (D1: $719f9612; D2: $81a1; D3: $47e0; D4: ($9a, $05, $d9, $4a, $d5, $fc, $a9, $48));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDAtoDConverterType}
  STATIC_CODECAPI_AVEncDDHeadphoneMode                :  TGUID = (D1: $4052dbec; D2: $52f5; D3: $42f5; D4: ($9b, $00, $d1, $34, $b1, $34, $1b, $9d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncDDHeadphoneMode}
  STATIC_CODECAPI_AVEncWMVKeyFrameDistance            :  TGUID = (D1: $5569055e; D2: $e268; D3: $4771; D4: ($b8, $3e, $95, $55, $ea, $28, $ae, $d3));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncWMVKeyFrameDistance}
  STATIC_CODECAPI_AVEncWMVInterlacedEncoding          :  TGUID = (D1: $e3d00f8a; D2: $c6f5; D3: $4e14; D4: ($a5, $88, $0e, $c8, $7a, $72, $6f, $9b));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncWMVInterlacedEncoding}
  STATIC_CODECAPI_AVEncWMVDecoderComplexity           :  TGUID = (D1: $f32c0dab; D2: $f3cb; D3: $4217; D4: ($b7, $9f, $87, $62, $76, $8b, $5f, $67));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncWMVDecoderComplexity}
  STATIC_CODECAPI_AVEncWMVKeyFrameBufferLevelMarker   :  TGUID = (D1: $51ff1115; D2: $33ac; D3: $426c; D4: ($a1, $b1, $09, $32, $1b, $df, $96, $b4));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncWMVKeyFrameBufferLevelMarker}
  STATIC_CODECAPI_AVEncWMVProduceDummyFrames          :  TGUID = (D1: $d669d001; D2: $183c; D3: $42e3; D4: ($a3, $ca, $2f, $45, $86, $d2, $39, $6c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncWMVProduceDummyFrames}
  STATIC_CODECAPI_AVEncStatWMVCBAvg                   :  TGUID = (D1: $6aa6229f; D2: $d602; D3: $4b9d; D4: ($b6, $8c, $c1, $ad, $78, $88, $4b, $ef));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatWMVCBAvg}
  STATIC_CODECAPI_AVEncStatWMVCBMax                   :  TGUID = (D1: $e976bef8; D2: $00fe; D3: $44b4; D4: ($b6, $25, $8f, $23, $8b, $c0, $34, $99));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatWMVCBMax}
  STATIC_CODECAPI_AVEncStatWMVDecoderComplexityProfile:  TGUID = (D1: $89e69fc3; D2: $0f9b; D3: $436c; D4: ($97, $4a, $df, $82, $12, $27, $c9, $0d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatWMVDecoderComplexityProfile}
  STATIC_CODECAPI_AVEncStatMPVSkippedEmptyFrames      :  TGUID = (D1: $32195fd3; D2: $590d; D3: $4812; D4: ($a7, $ed, $6d, $63, $9a, $1f, $97, $11));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncStatMPVSkippedEmptyFrames}
  STATIC_CODECAPI_AVEncMP12PktzSTDBuffer              :  TGUID = (D1: $0b751bd0; D2: $819e; D3: $478c; D4: ($94, $35, $75, $20, $89, $26, $b3, $77));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12PktzSTDBuffer}
  STATIC_CODECAPI_AVEncMP12PktzStreamID               :  TGUID = (D1: $c834d038; D2: $f5e8; D3: $4408; D4: ($9b, $60, $88, $f3, $64, $93, $fe, $df));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12PktzStreamID}
  STATIC_CODECAPI_AVEncMP12PktzInitialPTS             :  TGUID = (D1: $2a4f2065; D2: $9a63; D3: $4d20; D4: ($ae, $22, $0a, $1b, $c8, $96, $a3, $15));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12PktzInitialPTS}
  STATIC_CODECAPI_AVEncMP12PktzPacketSize             :  TGUID = (D1: $ab71347a; D2: $1332; D3: $4dde; D4: ($a0, $e5, $cc, $f7, $da, $8a, $0f, $22));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12PktzPacketSize}
  STATIC_CODECAPI_AVEncMP12PktzCopyright              :  TGUID = (D1: $c8f4b0c1; D2: $094c; D3: $43c7; D4: ($8e, $68, $a5, $95, $40, $5a, $6e, $f8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12PktzCopyright}
  STATIC_CODECAPI_AVEncMP12PktzOriginal               :  TGUID = (D1: $6b178416; D2: $31b9; D3: $4964; D4: ($94, $cb, $6b, $ff, $86, $6c, $df, $83));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12PktzOriginal}
  STATIC_CODECAPI_AVEncMP12MuxPacketOverhead          :  TGUID = (D1: $e40bd720; D2: $3955; D3: $4453; D4: ($ac, $f9, $b7, $91, $32, $a3, $8f, $a0));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxPacketOverhead}
  STATIC_CODECAPI_AVEncMP12MuxNumStreams              :  TGUID = (D1: $f7164a41; D2: $dced; D3: $4659; D4: ($a8, $f2, $fb, $69, $3f, $2a, $4c, $d0));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxNumStreams}
  STATIC_CODECAPI_AVEncMP12MuxEarliestPTS             :  TGUID = (D1: $157232b6; D2: $f809; D3: $474e; D4: ($94, $64, $a7, $f9, $30, $14, $a8, $17));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxEarliestPTS}
  STATIC_CODECAPI_AVEncMP12MuxLargestPacketSize       :  TGUID = (D1: $35ceb711; D2: $f461; D3: $4b92; D4: ($a4, $ef, $17, $b6, $84, $1e, $d2, $54));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxLargestPacketSize}
  STATIC_CODECAPI_AVEncMP12MuxInitialSCR              :  TGUID = (D1: $3433ad21; D2: $1b91; D3: $4a0b; D4: ($b1, $90, $2b, $77, $06, $3b, $63, $a4));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxInitialSCR}
  STATIC_CODECAPI_AVEncMP12MuxMuxRate                 :  TGUID = (D1: $ee047c72; D2: $4bdb; D3: $4a9d; D4: ($8e, $21, $41, $92, $6c, $82, $3d, $a7));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxMuxRate}
  STATIC_CODECAPI_AVEncMP12MuxPackSize                :  TGUID = (D1: $f916053a; D2: $1ce8; D3: $4faf; D4: ($aa, $0b, $ba, $31, $c8, $00, $34, $b8));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxPackSize}
  STATIC_CODECAPI_AVEncMP12MuxSysSTDBufferBound       :  TGUID = (D1: $35746903; D2: $b545; D3: $43e7; D4: ($bb, $35, $c5, $e0, $a7, $d5, $09, $3c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxSysSTDBufferBound}
  STATIC_CODECAPI_AVEncMP12MuxSysRateBound            :  TGUID = (D1: $05f0428a; D2: $ee30; D3: $489d; D4: ($ae, $28, $20, $5c, $72, $44, $67, $10));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxSysRateBound}
  STATIC_CODECAPI_AVEncMP12MuxTargetPacketizer        :  TGUID = (D1: $d862212a; D2: $2015; D3: $45dd; D4: ($9a, $32, $1b, $3a, $a8, $82, $05, $a0));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxTargetPacketizer}
  STATIC_CODECAPI_AVEncMP12MuxSysFixed                :  TGUID = (D1: $cefb987e; D2: $894f; D3: $452e; D4: ($8f, $89, $a4, $ef, $8c, $ec, $06, $3a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxSysFixed}
  STATIC_CODECAPI_AVEncMP12MuxSysCSPS                 :  TGUID = (D1: $7952ff45; D2: $9c0d; D3: $4822; D4: ($bc, $82, $8a, $d7, $72, $e0, $29, $93));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxSysCSPS}
  STATIC_CODECAPI_AVEncMP12MuxSysVideoLock            :  TGUID = (D1: $b8296408; D2: $2430; D3: $4d37; D4: ($a2, $a1, $95, $b3, $e4, $35, $a9, $1d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxSysVideoLock}
  STATIC_CODECAPI_AVEncMP12MuxSysAudioLock            :  TGUID = (D1: $0fbb5752; D2: $1d43; D3: $47bf; D4: ($bd, $79, $f2, $29, $3d, $8c, $e3, $37));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxSysAudioLock}
  STATIC_CODECAPI_AVEncMP12MuxDVDNavPacks             :  TGUID = (D1: $c7607ced; D2: $8cf1; D3: $4a99; D4: ($83, $a1, $ee, $54, $61, $be, $35, $74));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMP12MuxDVDNavPacks}

  STATIC_CODECAPI_AVEncMPACopyright                   :  TGUID = (D1: $a6ae762a; D2: $d0a9; D3: $4454; D4: ($b8, $ef, $f2, $db, $ee, $fd, $d3, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPACopyright}
  STATIC_CODECAPI_AVEncMPAOriginalBitstream           :  TGUID = (D1: $3cfb7855; D2: $9cc9; D3: $47ff; D4: ($b8, $29, $b3, $67, $86, $c9, $23, $46));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPAOriginalBitstream}
  STATIC_CODECAPI_AVEncMPAEnableRedundancyProtection  :  TGUID = (D1: $5e54b09e; D2: $b2e7; D3: $4973; D4: ($a8, $9b, $0b, $36, $50, $a3, $be, $da));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPAEnableRedundancyProtection}
  STATIC_CODECAPI_AVEncMPAPrivateUserBit              :  TGUID = (D1: $afa505ce; D2: $c1e3; D3: $4e3d; D4: ($85, $1b, $61, $b7, $00, $e5, $e6, $cc));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPAPrivateUserBit}
  STATIC_CODECAPI_AVEncMPAEmphasisType                :  TGUID = (D1: $2d59fcda; D2: $bf4e; D3: $4ed6; D4: ($b5, $df, $5b, $03, $b3, $6b, $0a, $1f));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMPAEmphasisType}


  STATIC_CODECAPI_AVDecCommonMeanBitRate              :  TGUID = (D1: $59488217; D2: $007a; D3: $4f7a; D4: ($8e, $41, $5c, $48, $b1, $ea, $c5, $c6));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecCommonMeanBitRate}
  STATIC_CODECAPI_AVDecCommonMeanBitRateInterval      :  TGUID = (D1: $0ee437c6; D2: $38a7; D3: $4c5c; D4: ($94, $4c, $68, $ab, $42, $11, $6b, $85));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecCommonMeanBitRateInterval}
  STATIC_CODECAPI_AVDecCommonInputFormat              :  TGUID = (D1: $e5005239; D2: $bd89; D3: $4be3; D4: ($9c, $0f, $5d, $de, $31, $79, $88, $cc));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecCommonInputFormat}
  STATIC_CODECAPI_AVDecCommonOutputFormat             :  TGUID = (D1: $3c790028; D2: $c0ce; D3: $4256; D4: ($b1, $a2, $1b, $0f, $c8, $b1, $dc, $dc));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecCommonOutputFormat}

  STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_MatrixEncoded  :  TGUID = (D1: $696e1d30; D2: $548f; D3: $4036; D4: ($82, $5f, $70, $26, $c6, $00, $11, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_MatrixEncoded}
  STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM                       :  TGUID = (D1: $696e1d31; D2: $548f; D3: $4036; D4: ($82, $5f, $70, $26, $c6, $00, $11, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM}
  STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_PCM                 :  TGUID = (D1: $696e1d32; D2: $548f; D3: $4036; D4: ($82, $5f, $70, $26, $c6, $00, $11, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_PCM}
  STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_Bitstream           :  TGUID = (D1: $696e1d33; D2: $548f; D3: $4036; D4: ($82, $5f, $70, $26, $c6, $00, $11, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_Bitstream}
  STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Headphones            :  TGUID = (D1: $696e1d34; D2: $548f; D3: $4036; D4: ($82, $5f, $70, $26, $c6, $00, $11, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Headphones}
  STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_Auto           :  TGUID = (D1: $696e1d35; D2: $548f; D3: $4036; D4: ($82, $5f, $70, $26, $c6, $00, $11, $bd));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_Auto}

  STATIC_CODECAPI_AVDecVideoImageSize                                   :  TGUID = (D1: $5ee5747c; D2: $6801; D3: $4cab; D4: ($aa, $f1, $62, $48, $fa, $84, $1b, $a4));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoImageSize}
  STATIC_CODECAPI_AVDecVideoInputScanType                               :  TGUID = (D1: $38477e1f; D2: $0ea7; D3: $42cd; D4: ($8c, $d1, $13, $0c, $ed, $57, $c5, $80));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoInputScanType}
  STATIC_CODECAPI_AVDecVideoPixelAspectRatio                            :  TGUID = (D1: $b0cf8245; D2: $f32d; D3: $41df; D4: ($b0, $2c, $87, $bd, $30, $4d, $12, $ab));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoPixelAspectRatio}
  STATIC_CODECAPI_AVDecVideoAcceleration_MPEG2                          :  TGUID = (D1: $f7db8a2e; D2: $4f48; D3: $4ee8; D4: ($ae, $31, $8b, $6e, $be, $55, $8a, $e2));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoAcceleration_MPEG2}
  STATIC_CODECAPI_AVDecVideoAcceleration_H264                           :  TGUID = (D1: $f7db8a2f; D2: $4f48; D3: $4ee8; D4: ($ae, $31, $8b, $6e, $be, $55, $8a, $e2));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoAcceleration_H264}
  STATIC_CODECAPI_AVDecVideoAcceleration_VC1                            :  TGUID = (D1: $f7db8a30; D2: $4f48; D3: $4ee8; D4: ($ae, $31, $8b, $6e, $be, $55, $8a, $e2));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoAcceleration_VC1}
  STATIC_CODECAPI_AVDecVideoProcDeinterlaceCSC                          :  TGUID = (D1: $f7db8a31; D2: $4f48; D3: $4ee8; D4: ($ae, $31, $8b, $6e, $be, $55, $8a, $e2));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoProcDeinterlaceCSC}

  STATIC_CODECAPI_AVDecVideoThumbnailGenerationMode       :  TGUID = (D1: $2efd8eee; D2: $1150; D3: $4328; D4: ($9c, $f5, $66, $dc, $e9, $33, $fc, $f4));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoThumbnailGenerationMode}
  STATIC_CODECAPI_AVDecVideoDropPicWithMissingRef         :  TGUID = (D1: $f8226383; D2: $14c2; D3: $4567; D4: ($97, $34, $50, $04, $e9, $6f, $f8, $87));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoDropPicWithMissingRef}
  STATIC_CODECAPI_AVDecVideoSoftwareDeinterlaceMode       :  TGUID = (D1: $0c08d1ce; D2: $9ced; D3: $4540; D4: ($ba, $e3, $ce, $b3, $80, $14, $11, $09));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoSoftwareDeinterlaceMode}
  STATIC_CODECAPI_AVDecVideoFastDecodeMode                :  TGUID = (D1: $6b529f7d; D2: $d3b1; D3: $49c6; D4: ($a9, $99, $9e, $c6, $91, $1b, $ed, $bf));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoFastDecodeMode}
  STATIC_CODECAPI_AVLowLatencyMode                        :  TGUID = (D1: $9c27891a; D2: $ed7a; D3: $40e1; D4: ($88, $e8, $b2, $27, $27, $a0, $24, $ee));
  {$EXTERNALSYM STATIC_CODECAPI_AVLowLatencyMode}
  STATIC_CODECAPI_AVDecVideoH264ErrorConcealment          :  TGUID = (D1: $ececace8; D2: $3436; D3: $462c; D4: ($92, $94, $cd, $7b, $ac, $d7, $58, $a9));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoH264ErrorConcealment}
  STATIC_CODECAPI_AVDecVideoMPEG2ErrorConcealment         :  TGUID = (D1: $9d2bfe18; D2: $728d; D3: $48d2; D4: ($b3, $58, $bc, $7e, $43, $6c, $66, $74));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoMPEG2ErrorConcealment}
  STATIC_CODECAPI_AVDecVideoCodecType                     :  TGUID = (D1: $434528e5; D2: $21f0; D3: $46b6; D4: ($b6, $2c, $9b, $1b, $6b, $65, $8c, $d1));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoCodecType}
  STATIC_CODECAPI_AVDecVideoDXVAMode                      :  TGUID = (D1: $f758f09e; D2: $7337; D3: $4ae7; D4: ($83, $87, $73, $dc, $2d, $54, $e6, $7d));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoDXVAMode}
  STATIC_CODECAPI_AVDecVideoDXVABusEncryption             :  TGUID = (D1: $42153c8b; D2: $fd0b; D3: $4765; D4: ($a4, $62, $dd, $d9, $e8, $bc, $c3, $88));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoDXVABusEncryption}
  STATIC_CODECAPI_AVDecVideoSWPowerLevel                  :  TGUID = (D1: $fb5d2347; D2: $4dd8; D3: $4509; D4: ($ae, $d0, $db, $5f, $a9, $aa, $93, $f4));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoSWPowerLevel}
  STATIC_CODECAPI_AVDecVideoMaxCodedWidth                 :  TGUID = (D1: $5ae557b8; D2: $77af; D3: $41f5; D4: ($9f, $a6, $4d, $b2, $fe, $1d, $4b, $ca));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoMaxCodedWidth}
  STATIC_CODECAPI_AVDecVideoMaxCodedHeight                :  TGUID = (D1: $7262a16a; D2: $d2dc; D3: $4e75; D4: ($9b, $a8, $65, $c0, $c6, $d3, $2b, $13));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecVideoMaxCodedHeight}
  STATIC_CODECAPI_AVDecNumWorkerThreads                   :  TGUID = (D1: $9561c3e8; D2: $ea9e; D3: $4435; D4: ($9b, $1e, $a9, $3e, $69, $18, $94, $d8));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecNumWorkerThreads}
  STATIC_CODECAPI_AVDecSoftwareDynamicFormatChange        :  TGUID = (D1: $862e2f0a; D2: $507b; D3: $47ff; D4: ($af, $47, $01, $e2, $62, $42, $98, $b7));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecSoftwareDynamicFormatChange}
  STATIC_CODECAPI_AVDecDisableVideoPostProcessing         :	 TGUID = (D1: $f8749193; D2: $667a; D3: $4f2c; D4: ($a9, $e8, $5d, $4a, $f9, $24, $f0, $8f));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecDisableVideoPostProcessing}

  STATIC_CODECAPI_GUID_AVDecAudioInputWMA                 :  TGUID = (D1: $c95e8dcf; D2: $4058; D3: $4204; D4: ($8c, $42, $cb, $24, $d9, $1e, $4b, $9b));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputWMA}
  STATIC_CODECAPI_GUID_AVDecAudioInputWMAPro              :  TGUID = (D1: $0128b7c7; D2: $da72; D3: $4fe3; D4: ($be, $f8, $5c, $52, $e3, $55, $77, $04));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputWMAPro}
  STATIC_CODECAPI_GUID_AVDecAudioInputDolby               :  TGUID = (D1: $8e4228a0; D2: $f000; D3: $4e0b; D4: ($8f, $54, $ab, $8d, $24, $ad, $61, $a2));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputDolby}
  STATIC_CODECAPI_GUID_AVDecAudioInputDTS                 :  TGUID = (D1: $600bc0ca; D2: $6a1f; D3: $4e91; D4: ($b2, $41, $1b, $be, $b1, $cb, $19, $e0));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputDTS}
  STATIC_CODECAPI_GUID_AVDecAudioInputPCM                 :  TGUID = (D1: $f2421da5; D2: $bbb4; D3: $4cd5; D4: ($a9, $96, $93, $3c, $6b, $5d, $13, $47));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputPCM}
  STATIC_CODECAPI_GUID_AVDecAudioInputMPEG                :  TGUID = (D1: $91106f36; D2: $02c5; D3: $4f75; D4: ($97, $19, $3b, $7a, $bf, $75, $e1, $f6));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputMPEG}
  STATIC_CODECAPI_GUID_AVDecAudioInputAAC                 :  TGUID = (D1: $97df7828; D2: $b94a; D3: $47e2; D4: ($a4, $bc, $51, $19, $4d, $b2, $2a, $4d));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputAAC}
  STATIC_CODECAPI_GUID_AVDecAudioInputHEAAC               :  TGUID = (D1: $16efb4aa; D2: $330e; D3: $4f5c; D4: ($98, $a8, $cf, $6a, $c5, $5c, $be, $60));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputHEAAC}
  STATIC_CODECAPI_GUID_AVDecAudioInputDolbyDigitalPlus    :  TGUID = (D1: $0803e185; D2: $8f5d; D3: $47f5; D4: ($99, $08, $19, $a5, $bb, $c9, $fe, $34));
  {$EXTERNALSYM STATIC_CODECAPI_GUID_AVDecAudioInputDolbyDigitalPlus}

  STATIC_CODECAPI_AVDecAACDownmixMode                     :  TGUID = (D1: $01274475; D2: $f6bb; D3: $4017; D4: ($b0, $84, $81, $a7, $63, $c9, $42, $d4));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecAACDownmixMode}
  STATIC_CODECAPI_AVDecHEAACDynamicRangeControl           :  TGUID = (D1: $287c8abe; D2: $69a4; D3: $4d39; D4: ($80, $80, $d3, $d9, $71, $21, $78, $a0));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecHEAACDynamicRangeControl}

  STATIC_CODECAPI_AVDecAudioDualMono                      :  TGUID = (D1: $4a52cda8; D2: $30f8; D3: $4216; D4: ($be, $0f, $ba, $0b, $20, $25, $92, $1d));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecAudioDualMono}
  STATIC_CODECAPI_AVDecAudioDualMonoReproMode             :  TGUID = (D1: $a5106186; D2: $cc94; D3: $4bc9; D4: ($8c, $d9, $aa, $2f, $61, $f6, $80, $7e));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecAudioDualMonoReproMode}

  STATIC_CODECAPI_AVAudioChannelCount                     :  TGUID = (D1: $1d3583c4; D2: $1583; D3: $474e; D4: ($b7, $1a, $5e, $e4, $63, $c1, $98, $e4));
  {$EXTERNALSYM STATIC_CODECAPI_AVAudioChannelCount}
  STATIC_CODECAPI_AVAudioChannelConfig                    :  TGUID = (D1: $17f89cb3; D2: $c38d; D3: $4368; D4: ($9e, $de, $63, $b9, $4d, $17, $7f, $9f));
  {$EXTERNALSYM STATIC_CODECAPI_AVAudioChannelConfig}
  STATIC_CODECAPI_AVAudioSampleRate                       :  TGUID = (D1: $971d2723; D2: $1acb; D3: $42e7; D4: ($85, $5c, $52, $0a, $4b, $70, $a5, $f2));
  {$EXTERNALSYM STATIC_CODECAPI_AVAudioSampleRate}

  STATIC_CODECAPI_AVDDSurroundMode                        :  TGUID = (D1: $99f2f386; D2: $98d1; D3: $4452; D4: ($a1, $63, $ab, $c7, $8a, $6e, $b7, $70));
  {$EXTERNALSYM STATIC_CODECAPI_AVDDSurroundMode}
  STATIC_CODECAPI_AVDecDDOperationalMode                  :  TGUID = (D1: $d6d6c6d1; D2: $064e; D3: $4fdd; D4: ($a4, $0e, $3e, $cb, $fc, $b7, $eb, $d0));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecDDOperationalMode}
  STATIC_CODECAPI_AVDecDDMatrixDecodingMode               :  TGUID = (D1: $ddc811a5; D2: $04ed; D3: $4bf3; D4: ($a0, $ca, $d0, $04, $49, $f9, $35, $5f));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecDDMatrixDecodingMode}
  STATIC_CODECAPI_AVDecDDDynamicRangeScaleHigh            :  TGUID = (D1: $50196c21; D2: $1f33; D3: $4af5; D4: ($b2, $96, $11, $42, $6d, $6c, $87, $89));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecDDDynamicRangeScaleHigh}
  STATIC_CODECAPI_AVDecDDDynamicRangeScaleLow             :  TGUID = (D1: $044e62e4; D2: $11a5; D3: $42d5; D4: ($a3, $b2, $3b, $b2, $c7, $c2, $d7, $cf));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecDDDynamicRangeScaleLow}
  STATIC_CODECAPI_AVDecDDStereoDownMixMode                :  TGUID = (D1: $6ce4122c; D2: $3ee9; D3: $4182; D4: ($b4, $ae, $c1, $0f, $c0, $88, $64, $9d));
  {$EXTERNALSYM STATIC_CODECAPI_AVDecDDStereoDownMixMode}

  STATIC_CODECAPI_AVDSPLoudnessEqualization               :  TGUID = (D1: $8afd1a15; D2: $1812; D3: $4cbf; D4: ($93, $19, $43, $3a, $5b, $2a, $3b, $27));
  {$EXTERNALSYM STATIC_CODECAPI_AVDSPLoudnessEqualization}
  STATIC_CODECAPI_AVDSPSpeakerFill                        :  TGUID = (D1: $5612bca1; D2: $56da; D3: $4582; D4: ($8d, $a1, $ca, $80, $90, $f9, $27, $68));
  {$EXTERNALSYM STATIC_CODECAPI_AVDSPSpeakerFill}

  STATIC_CODECAPI_AVPriorityControl                       :  TGUID = (D1: $54ba3dc8; D2: $bdde; D3: $4329; D4: ($b1, $87, $20, $18, $bc, $5c, $2b, $a1));
  {$EXTERNALSYM STATIC_CODECAPI_AVPriorityControl}
  STATIC_CODECAPI_AVRealtimeControl                       :  TGUID = (D1: $6f440632; D2: $c4ad; D3: $4bf7; D4: ($9e, $52, $45, $69, $42, $b4, $54, $b0));
  {$EXTERNALSYM STATIC_CODECAPI_AVRealtimeControl}
  STATIC_CODECAPI_AVEncMaxFrameRate                       :  TGUID = (D1: $b98e1b31; D2: $19fa; D3: $4d4f; D4: ($99, $31, $d6, $a5, $b8, $aa, $b9, $3c));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncMaxFrameRate}

  STATIC_CODECAPI_AVEncNoInputCopy                        :  TGUID = (D1: $d2b46a2a;
                                                                      D2: $e8ee;
                                                                      D3: $4ec5;
                                                                      D4: ($86, $9e, $44, $9b, $6c, $62, $c8, $1a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncNoInputCopy}

  STATIC_CODECAPI_AVEncChromaEncodeMode                   :  TGUID = (D1: $8a47ab5a;
                                                                      D2: $4798;
                                                                      D3: $4c93;
                                                                      D4: ($b5, $a5, $55, $4f, $9a, $3b, $9f, $50));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncChromaEncodeMode}


  STATIC_CODECAPI_AVEncProgressiveUpdateTime              :  TGUID = (D1: $649faf66;
                                                                      D2: $afc6;
                                                                      D3: $4828;
                                                                      D4: ($8f, $dc, $07, $71, $cd, $9a, $b1, $7d));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncProgressiveUpdateTime}


  STATIC_CODECAPI_AVEncChromaUpdateTime                   :  TGUID = (D1: $4b4fd998;
                                                                      D2: $4274;
                                                                      D3: $40bb;
                                                                      D4: ($8e, $e4, $07, $55, $3e, $7e, $2d, $3a));
  {$EXTERNALSYM STATIC_CODECAPI_AVEncChromaUpdateTime}
// end of static definitions }


//
// Common Parameters
//

// AVEncCommonFormatConstraint (GUID)

  CODECAPI_AVEncCommonFormatConstraint        :  TGUID = '{57cbb9b8-116f-4951-b40c-c2a035ed8f17}';
  {$EXTERNALSYM CODECAPI_AVEncCommonFormatConstraint}
  CODECAPI_GUID_AVEncCommonFormatUnSpecified  :  TGUID = '{af46a35a-6024-4525-a48a-094b97f5b3c2}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatUnSpecified}
  CODECAPI_GUID_AVEncCommonFormatDVD_V        :  TGUID = '{cc9598c4-e7fe-451d-b1ca-761bc840b7f3}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatDVD_V}
  CODECAPI_GUID_AVEncCommonFormatDVD_DashVR   :  TGUID = '{e55199d6-044c-4dae-a488-531ed306235b}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatDVD_DashVR}
  CODECAPI_GUID_AVEncCommonFormatDVD_PlusVR   :  TGUID = '{e74c6f2e-ec37-478d-9af4-a5e135b6271c}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatDVD_PlusVR}
  CODECAPI_GUID_AVEncCommonFormatVCD          :  TGUID = '{95035bf7-9d90-40ff-ad5c-5cf8cf71ca1d}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatVCD}
  CODECAPI_GUID_AVEncCommonFormatSVCD         :  TGUID = '{51d85818-8220-448c-8066-d69bed16c9ad}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatSVCD}
  CODECAPI_GUID_AVEncCommonFormatATSC         :  TGUID = '{8d7b897c-a019-4670-aa76-2edcac7ac296}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatATSC}
  CODECAPI_GUID_AVEncCommonFormatDVB          :  TGUID = '{71830d8f-6c33-430d-844b-c2705baae6db}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatDVB}
  CODECAPI_GUID_AVEncCommonFormatMP3          :  TGUID = '{349733cd-eb08-4dc2-8197-e49835ef828b}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatMP3}
  CODECAPI_GUID_AVEncCommonFormatHighMAT      :  TGUID = '{1eabe760-fb2b-4928-90d1-78db88eee889}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatHighMAT}
  CODECAPI_GUID_AVEncCommonFormatHighMPV      :  TGUID = '{a2d25db8-b8f9-42c2-8bc7-0b93cf604788}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncCommonFormatHighMPV}


// AVEncCodecType (GUID)
  CODECAPI_AVEncCodecType                     :  TGUID = '{08af4ac1-f3f2-4c74-9dcf-37f2ec79f826}';
  {$EXTERNALSYM CODECAPI_AVEncCodecType}
  CODECAPI_GUID_AVEncMPEG1Video               :  TGUID = '{c8dafefe-da1e-4774-b27d-11830c16b1fe}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncMPEG1Video}
  CODECAPI_GUID_AVEncMPEG2Video               :  TGUID = '{046dc19a-6677-4aaa-a31d-c1ab716f4560}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncMPEG2Video}
  CODECAPI_GUID_AVEncMPEG1Audio               :  TGUID = '{d4dd1362-cd4a-4cd6-8138-b94db4542b04}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncMPEG1Audio}
  CODECAPI_GUID_AVEncMPEG2Audio               :  TGUID = '{ee4cbb1f-9c3f-4770-92b5-fcb7c2a8d381}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncMPEG2Audio}
  CODECAPI_GUID_AVEncWMV                      :  TGUID = '{4e0fef9b-1d43-41bd-b8bd-4d7bf7457a2a}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncWMV}
  CODECAPI_GUID_AVEndMPEG4Video               :  TGUID = '{dd37b12a-9503-4f8b-b8d0-324a00c0a1cf}';
  {$EXTERNALSYM CODECAPI_GUID_AVEndMPEG4Video}
  CODECAPI_GUID_AVEncH264Video                :  TGUID = '{95044eab-31b3-47de-8e75-38a42bb03e28}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncH264Video}
  CODECAPI_GUID_AVEncDV                       :  TGUID = '{09b769c7-3329-44fb-8954-fa30937d3d5a}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncDV}
  CODECAPI_GUID_AVEncWMAPro                   :  TGUID = '{1955f90c-33f7-4a68-ab81-53f5657125c4}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncWMAPro}
  CODECAPI_GUID_AVEncWMALossless              :  TGUID = '{55ca7265-23d8-4761-9031-b74fbe12f4c1}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncWMALossless}
  CODECAPI_GUID_AVEncWMAVoice                 :  TGUID = '{13ed18cb-50e8-4276-a288-a6aa228382d9}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncWMAVoice}
  CODECAPI_GUID_AVEncDolbyDigitalPro          :  TGUID = '{f5be76cc-0ff8-40eb-9cb1-bba94004d44f}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncDolbyDigitalPro}
  CODECAPI_GUID_AVEncDolbyDigitalConsumer     :  TGUID = '{c1a7bf6c-0059-4bfa-94ef-ef747a768d52}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncDolbyDigitalConsumer}
  CODECAPI_GUID_AVEncDolbyDigitalPlus         :  TGUID = '{698d1b80-f7dd-415c-971c-42492a2056c6}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncDolbyDigitalPlus}
  CODECAPI_GUID_AVEncDTSHD                    :  TGUID = '{2052e630-469d-4bfb-80ca-1d656e7e918f}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncDTSHD}
  CODECAPI_GUID_AVEncDTS                      :  TGUID = '{45fbcaa2-5e6e-4ab0-8893-5903bee93acf}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncDTS}
  CODECAPI_GUID_AVEncMLP                      :  TGUID = '{05f73e29-f0d1-431e-a41c-a47432ec5a66}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncMLP}
  CODECAPI_GUID_AVEncPCM                      :  TGUID = '{844be7f4-26cf-4779-b386-cc05d187990c}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncPCM}
  CODECAPI_GUID_AVEncSDDS                     :  TGUID = '{1dc1b82f-11c8-4c71-b7b6-ee3eb9bc2b94}';
  {$EXTERNALSYM CODECAPI_GUID_AVEncSDDS}

  // AVEncCommonRateControlMode (UINT32)
  CODECAPI_AVEncCommonRateControlMode           :  TGUID = '{1c0608e9-370c-4710-8a58-cb6181c42423}';
  {$EXTERNALSYM CODECAPI_AVEncCommonRateControlMode}

  // AVEncCommonLowLatency (BOOL)
  CODECAPI_AVEncCommonLowLatency                :  TGUID = '{9d3ecd55-89e8-490a-970a-0c9548d5a56e}';
  {$EXTERNALSYM CODECAPI_AVEncCommonLowLatency}

  // AVEncCommonMultipassMode (UINT32)
  CODECAPI_AVEncCommonMultipassMode             :  TGUID = '{22533d4c-47e1-41b5-9352-a2b7780e7ac4}';
  {$EXTERNALSYM CODECAPI_AVEncCommonMultipassMode}

  // AVEncCommonPassStart (UINT32)
  CODECAPI_AVEncCommonPassStart                 :  TGUID = '{6a67739f-4eb5-4385-9928-f276a939ef95}';
  {$EXTERNALSYM CODECAPI_AVEncCommonPassStart}

  // AVEncCommonPassEnd (UINT32)
  CODECAPI_AVEncCommonPassEnd                   :  TGUID = '{0e3d01bc-c85c-467d-8b60-c41012ee3bf6}';
  {$EXTERNALSYM CODECAPI_AVEncCommonPassEnd}

  // AVEncCommonRealTime (BOOL)
  CODECAPI_AVEncCommonRealTime                  :  TGUID = '{143a0ff6-a131-43da-b81e-98fbb8ec378e}';
  {$EXTERNALSYM CODECAPI_AVEncCommonRealTime}

  // AVEncCommonQuality (UINT32)
  CODECAPI_AVEncCommonQuality                   :  TGUID = '{fcbf57a3-7ea5-4b0c-9644-69b40c39c391}';
  {$EXTERNALSYM CODECAPI_AVEncCommonQuality}

  // AVEncCommonQualityVsSpeed (UINT32)
  CODECAPI_AVEncCommonQualityVsSpeed            :  TGUID = '{98332df8-03cd-476b-89fa-3f9e442dec9f}';
  {$EXTERNALSYM CODECAPI_AVEncCommonQualityVsSpeed}

  // AVEncCommonTranscodeEncodingProfile (BSTR)
  CODECAPI_AVEncCommonTranscodeEncodingProfile  :  TGUID = '{6947787C-F508-4EA9-B1E9-A1FE3A49FBC9}';
  {$EXTERNALSYM CODECAPI_AVEncCommonTranscodeEncodingProfile}

  // AVEncCommonMeanBitRate (UINT32)
  CODECAPI_AVEncCommonMeanBitRate               :  TGUID = '{f7222374-2144-4815-b550-a37f8e12ee52}';
  {$EXTERNALSYM CODECAPI_AVEncCommonMeanBitRate}

  // AVEncCommonMeanBitRateInterval (UINT64)
  CODECAPI_AVEncCommonMeanBitRateInterval       :  TGUID = '{bfaa2f0c-cb82-4bc0-8474-f06a8a0d0258}';
  {$EXTERNALSYM CODECAPI_AVEncCommonMeanBitRateInterval}

  // AVEncCommonMaxBitRate (UINT32)
  CODECAPI_AVEncCommonMaxBitRate                :  TGUID = '{9651eae4-39b9-4ebf-85ef-d7f444ec7465}';
  {$EXTERNALSYM CODECAPI_AVEncCommonMaxBitRate}

  // AVEncCommonMinBitRate (UINT32)
  CODECAPI_AVEncCommonMinBitRate                :  TGUID = '{101405b2-2083-4034-a806-efbeddd7c9ff}';

  // AVEncCommonBufferSize (UINT32)
  CODECAPI_AVEncCommonBufferSize                :  TGUID = '{0db96574-b6a4-4c8b-8106-3773de0310cd}';
  {$EXTERNALSYM CODECAPI_AVEncCommonBufferSize}

  // AVEncCommonBufferInLevel (UINT32)
  CODECAPI_AVEncCommonBufferInLevel             :  TGUID = '{d9c5c8db-fc74-4064-94e9-cd19f947ed45}';

  // AVEncCommonBufferOutLevel (UINT32)
  CODECAPI_AVEncCommonBufferOutLevel            :  TGUID = '{ccae7f49-d0bc-4e3d-a57e-fb5740140069}';
  {$EXTERNALSYM CODECAPI_AVEncCommonBufferOutLevel}

  // AVEncCommonStreamEndHandling (UINT32)
  CODECAPI_AVEncCommonStreamEndHandling         :  TGUID = '{6aad30af-6ba8-4ccc-8fca-18d19beaeb1c}';
  {$EXTERNALSYM CODECAPI_AVEncCommonStreamEndHandling}

  // Common Post Encode Statistical Parameters
  // AVEncStatCommonCompletedPasses (UINT32)
  CODECAPI_AVEncStatCommonCompletedPasses       :  TGUID = '{3e5de533-9df7-438c-854f-9f7dd3683d34}';
  {$EXTERNALSYM CODECAPI_AVEncStatCommonCompletedPasses}

  // AVEncVideoOutputFrameRate (UINT32)
  CODECAPI_AVEncVideoOutputFrameRate            :  TGUID = '{ea85e7c3-9567-4d99-87c4-02c1c278ca7c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputFrameRate}

  // AVEncVideoOutputFrameRateConversion (UINT32)
  CODECAPI_AVEncVideoOutputFrameRateConversion  :  TGUID = '{8c068bf4-369a-4ba3-82fd-b2518fb3396e}';

  // AVEncVideoPixelAspectRatio (UINT32 as UINT16/UNIT16) <---- You have WORD in the doc
  CODECAPI_AVEncVideoPixelAspectRatio           :  TGUID = '{3cdc718f-b3e9-4eb6-a57f-cf1f1b321b87}';
  {$EXTERNALSYM CODECAPI_AVEncVideoPixelAspectRatio}

  // AVDecVideoAcceleration_MPEG2 (UINT32)
  CODECAPI_AVDecVideoAcceleration_MPEG2         :  TGUID = '{f7db8a2e-4f48-4ee8-ae31-8b6ebe558ae2}';
  {$EXTERNALSYM CODECAPI_AVDecVideoAcceleration_MPEG2}
  CODECAPI_AVDecVideoAcceleration_H264          :  TGUID = '{f7db8a2f-4f48-4ee8-ae31-8b6ebe558ae2}';
  {$EXTERNALSYM CODECAPI_AVDecVideoAcceleration_H264}
  CODECAPI_AVDecVideoAcceleration_VC1           :  TGUID = '{f7db8a30-4f48-4ee8-ae31-8b6ebe558ae2}';
  {$EXTERNALSYM CODECAPI_AVDecVideoAcceleration_VC1}

  // AVDecVideoProcDeinterlaceCSC (UINT32)
  CODECAPI_AVDecVideoProcDeinterlaceCSC         :  TGUID = '{f7db8a31-4f48-4ee8-ae31-8b6ebe558ae2}';
  {$EXTERNALSYM CODECAPI_AVDecVideoProcDeinterlaceCSC}


  // AVDecVideoThumbnailGenerationMode (BOOL)
  // Related to video thumbnail generation.
  // Video decoders can have special configurations for fast thumbnail generation.
  // For example:
  //   - They can use only one decoding thread so that multiple instances can be used at the same time.
  //   - They can also decode I frames only.
  CODECAPI_AVDecVideoThumbnailGenerationMode    :  TGUID = '{2efd8eee-1150-4328-9cf5-66dce933fcf4}';
  {$EXTERNALSYM CODECAPI_AVDecVideoThumbnailGenerationMode}

  // AVDecVideoMaxCodedWidth and AVDecVideoMaxCodedHeight
  // Maximum codec width and height for current stream.
  // This is used to optimize memory usage for a particular stream.
  CODECAPI_AVDecVideoMaxCodedWidth              :  TGUID = '{5ae557b8-77af-41f5-9fa6-4db2fe1d4bca}';
  {$EXTERNALSYM CODECAPI_AVDecVideoMaxCodedWidth}
  CODECAPI_AVDecVideoMaxCodedHeight             :  TGUID = '{7262a16a-d2dc-4e75-9ba8-65c0c6d32b13}';
  {$EXTERNALSYM CODECAPI_AVDecVideoMaxCodedHeight}

  // AVDecNumWorkerThreads (INT32)
  // Number of worker threads used in decoder core.
  // If this number is set to -1, it means that the decoder will decide how many threads to create.
  CODECAPI_AVDecNumWorkerThreads                :  TGUID = '{9561c3e8-ea9e-4435-9b1e-a93e691894d8}';
  {$EXTERNALSYM CODECAPI_AVDecNumWorkerThreads}

  // AVDecSoftwareDynamicFormatChange (BOOL)
  // Set whether to use software dynamic format change to internal resizing
  CODECAPI_AVDecSoftwareDynamicFormatChange     :  TGUID = '{862e2f0a-507b-47ff-af47-01e2624298b7}';
  {$EXTERNALSYM CODECAPI_AVDecSoftwareDynamicFormatChange}

  // AVDecDisableVideoPostProcessing (UINT32)
  // Default value is 0
  // If this is non-zero, decoder should not do post processing like deblocking/deringing. This only controls the out of loop post processing
  // all processing required by video standard (like in-loop deblocking) should still be performed.
  CODECAPI_AVDecDisableVideoPostProcessing      :  TGUID = '{F8749193-667A-4F2C-A9E8-5D4AF924F08F}';
  {$EXTERNALSYM CODECAPI_AVDecDisableVideoPostProcessing}

  // AVDecVideoDropPicWithMissingRef (BOOL)
  // Related to Video decoding mode of whether to drop pictures with missing references.
  // For DVD playback, we may want to do so to avoid bad blocking.  For Digital TV, we may
  // want to decode all pictures no matter what.
  CODECAPI_AVDecVideoDropPicWithMissingRef      :  TGUID = '{f8226383-14c2-4567-9734-5004e96ff887}';
  {$EXTERNALSYM CODECAPI_AVDecVideoDropPicWithMissingRef}

  // AVDecSoftwareVideoDeinterlaceMode (UINT32)
  CODECAPI_AVDecVideoSoftwareDeinterlaceMode    :  TGUID = '{0c08d1ce-9ced-4540-bae3-ceb380141109}';
  {$EXTERNALSYM CODECAPI_AVDecVideoSoftwareDeinterlaceMode}

  // AVDecVideoFastDecodeMode (UINT32)
  // 0: normal decoding
  // 1-32 : Where 32 is fastest decoding. Any value between (and including) 1 to 32 is valid
  CODECAPI_AVDecVideoFastDecodeMode             :  TGUID = '{6b529f7d-d3b1-49c6-a999-9ec6911bedbf}';
  {$EXTERNALSYM CODECAPI_AVDecVideoFastDecodeMode}

  // AVLowLatencyMode (DWORD)
  // Related to low latency processing/decoding.
  // This GUID lets the application to decrease latency.
  CODECAPI_AVLowLatencyMode                     :  TGUID = '{9c27891a-ed7a-40e1-88e8-b22727a024ee}';
  {$EXTERNALSYM CODECAPI_AVLowLatencyMode}

  // AVDecVideoH264ErrorConcealment (UINT32)
  // Related to Video decoding mode of whether to conceal pictures with corruptions.
  // For DVD playback, we may not want to do so to avoid unnecessary computation.  For Digital TV, we may
  // want to perform error concealment.
  CODECAPI_AVDecVideoH264ErrorConcealment       :  TGUID = '{ececace8-3436-462c-9294-cd7bacd758a9}';
  {$EXTERNALSYM CODECAPI_AVDecVideoH264ErrorConcealment}

  // AVDecVideoMPEG2ErrorConcealment (UINT32)
  // Related to Video decoding mode of whether to conceal pictures with corruptions.
  // For DVD playback, we may not want to do so to avoid unnecessary computation.  For Digital TV, we may
  // want to perform error concealment.
  CODECAPI_AVDecVideoMPEG2ErrorConcealment      :  TGUID = '{9d2bfe18-728d-48d2-b358-bc7e436c6674}';
  {$EXTERNALSYM CODECAPI_AVDecVideoMPEG2ErrorConcealment}

  // CODECAPI_AVDecVideoCodecType (UINT32)
  CODECAPI_AVDecVideoCodecType                  :  TGUID = '{434528e5-21f0-46b6-b62c-9b1b6b658cd1}';
  {$EXTERNALSYM CODECAPI_AVDecVideoCodecType}

  // CODECAPI_AVDecVideoDXVAMode (UINT32)
  CODECAPI_AVDecVideoDXVAMode                   :  TGUID = '{f758f09e-7337-4ae7-8387-73dc2d54e67d}';
  {$EXTERNALSYM CODECAPI_AVDecVideoDXVAMode}

  // CODECAPI_AVDecVideoDXVABusEncryption (UINT32)
  CODECAPI_AVDecVideoDXVABusEncryption          :  TGUID = '{42153c8b-fd0b-4765-a462-ddd9e8bcc388}';
  {$EXTERNALSYM CODECAPI_AVDecVideoDXVABusEncryption}

  // AVEncVideoForceSourceScanType (UINT32)
  CODECAPI_AVEncVideoForceSourceScanType        :  TGUID = '{1ef2065f-058a-4765-a4fc-8a864c103012}';
  {$EXTERNALSYM CODECAPI_AVEncVideoForceSourceScanType}

  // AVEncVideoNoOfFieldsToEncode (UINT64)
  CODECAPI_AVEncVideoNoOfFieldsToEncode         :  TGUID = '{61e4bbe2-4ee0-40e7-80ab-51ddeebe6291}';
  {$EXTERNALSYM CODECAPI_AVEncVideoNoOfFieldsToEncode}

  // AVEncVideoNoOfFieldsToSkip (UINT64)
  CODECAPI_AVEncVideoNoOfFieldsToSkip           :  TGUID = '{a97e1240-1427-4c16-a7f7-3dcfd8ba4cc5}';
  {$EXTERNALSYM CODECAPI_AVEncVideoNoOfFieldsToSkip}

  // AVEncVideoEncodeDimension (UINT32)
  CODECAPI_AVEncVideoEncodeDimension            :  TGUID = '{1074df28-7e0f-47a4-a453-cdd73870f5ce}';
  {$EXTERNALSYM CODECAPI_AVEncVideoEncodeDimension}

  // AVEncVideoEncodeOffsetOrigin (UINT32)
  CODECAPI_AVEncVideoEncodeOffsetOrigin         :  TGUID = '{6bc098fe-a71a-4454-852e-4d2ddeb2cd24}';
  {$EXTERNALSYM CODECAPI_AVEncVideoEncodeOffsetOrigin}

  // AVEncVideoDisplayDimension (UINT32)
  CODECAPI_AVEncVideoDisplayDimension           :  TGUID = '{de053668-f4ec-47a9-86d0-836770f0c1d5}';
  {$EXTERNALSYM CODECAPI_AVEncVideoDisplayDimension}

  // AVEncVideoOutputScanType (UINT32)
  CODECAPI_AVEncVideoOutputScanType             :  TGUID = '{460b5576-842e-49ab-a62d-b36f7312c9db}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputScanType}

  // AVEncVideoInverseTelecineEnable (BOOL)
  CODECAPI_AVEncVideoInverseTelecineEnable      :  TGUID = '{2ea9098b-e76d-4ccd-a030-d3b889c1b64c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInverseTelecineEnable}

  // AVEncVideoInverseTelecineThreshold (UINT32)
  CODECAPI_AVEncVideoInverseTelecineThreshold   :  TGUID = '{40247d84-e895-497f-b44c-b74560acfe27}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInverseTelecineThreshold}

  // AVEncVideoSourceFilmContent (UINT32)
  CODECAPI_AVEncVideoSourceFilmContent          :  TGUID = '{1791c64b-ccfc-4827-a0ed-2557793b2b1c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoSourceFilmContent}

  // AVEncVideoSourceIsBW (BOOL)
  CODECAPI_deoSourceIsBW                        :  TGUID = '{42ffc49b-1812-4fdc-8d24-7054c521e6eb}';
  {$EXTERNALSYM CODECAPI_deoSourceIsBW}

  // AVEncVideoFieldSwap (BOOL)
  CODECAPI_deoFieldSwap                         :  TGUID = '{fefd7569-4e0a-49f2-9f2b-360ea48c19a2}';
  {$EXTERNALSYM CODECAPI_deoFieldSwap}

  // AVEncVideoInputChromaResolution (UINT32)
  // AVEncVideoOutputChromaSubsamplingFormat (UINT32)
  CODECAPI_deoInputChromaResolution             :  TGUID = '{bb0cec33-16f1-47b0-8a88-37815bee1739}';
  {$EXTERNALSYM CODECAPI_deoInputChromaResolution}
  CODECAPI_deoOutputChromaResolution            :  TGUID = '{6097b4c9-7c1d-4e64-bfcc-9e9765318ae7}';
  {$EXTERNALSYM CODECAPI_deoOutputChromaResolution}

  // AVEncVideoInputChromaSubsampling (UINT32)
  // AVEncVideoOutputChromaSubsampling (UINT32)
  CODECAPI_AVEncVideoInputChromaSubsampling     :  TGUID = '{a8e73a39-4435-4ec3-a6ea-98300f4b36f7}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInputChromaSubsampling}
  CODECAPI_AVEncVideoOutputChromaSubsampling    :  TGUID = '{fa561c6c-7d17-44f0-83c9-32ed12e96343}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputChromaSubsampling}
  // AVEncVideoInputColorPrimaries (UINT32)
  // AVEncVideoOutputColorPrimaries (UINT32)
  CODECAPI_AVEncVideoInputColorPrimaries        :  TGUID = '{c24d783f-7ce6-4278-90ab-28a4f1e5f86c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInputColorPrimaries}
  CODECAPI_AVEncVideoOutputColorPrimaries       :  TGUID = '{be95907c-9d04-4921-8985-a6d6d87d1a6c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputColorPrimaries}

  // AVEncVideoInputColorTransferFunction (UINT32)
  // AVEncVideoOutputColorTransferFunction (UINT32)
  CODECAPI_AVEncVideoInputColorTransferFunction   :  TGUID = '{8c056111-a9c3-4b08-a0a0-ce13f8a27c75}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInputColorTransferFunction}
  CODECAPI_AVEncVideoOutputColorTransferFunction  :  TGUID = '{4a7f884a-ea11-460d-bf57-b88bc75900de}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputColorTransferFunction}
  // AVEncVideoInputColorTransferMatrix (UINT32)
  // AVEncVideoOutputColorTransferMatrix (UINT32)
  CODECAPI_AVEncVideoInputColorTransferMatrix   :  TGUID = '{52ed68b9-72d5-4089-958d-f5405d55081c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInputColorTransferMatrix}
  CODECAPI_AVEncVideoOutputColorTransferMatrix  :  TGUID = '{a9b90444-af40-4310-8fbe-ed6d933f892b}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputColorTransferMatrix}

  // AVEncVideoInputColorLighting (UINT32)
  // AVEncVideoOutputColorLighting (UINT32)
  CODECAPI_AVEncVideoInputColorLighting         :  TGUID = '{46a99549-0015-4a45-9c30-1d5cfa258316}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInputColorLighting}
  CODECAPI_AVEncVideoOutputColorLighting        :  TGUID = '{0e5aaac6-ace6-4c5c-998e-1a8c9c6c0f89}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputColorLighting}
  // AVEncVideoInputColorNominalRange (UINT32)
  // AVEncVideoOutputColorNominalRange (UINT32)
  CODECAPI_AVEncVideoInputColorNominalRange     :  TGUID = '{16cf25c6-a2a6-48e9-ae80-21aec41d427e}';
  {$EXTERNALSYM CODECAPI_AVEncVideoInputColorNominalRange}
  CODECAPI_AVEncVideoOutputColorNominalRange    :  TGUID = '{972835ed-87b5-4e95-9500-c73958566e54}';
  {$EXTERNALSYM CODECAPI_AVEncVideoOutputColorNominalRange}
  // AVEncInputVideoSystem (UINT32)
  CODECAPI_AVEncInputVideoSystem                :  TGUID = '{bede146d-b616-4dc7-92b2-f5d9fa9298f7}';
  {$EXTERNALSYM CODECAPI_AVEncInputVideoSystem}

  // AVEncVideoHeaderDropFrame (UINT32)
  CODECAPI_AVEncVideoHeaderDropFrame            :  TGUID = '{6ed9e124-7925-43fe-971b-e019f62222b4}';
  {$EXTERNALSYM CODECAPI_AVEncVideoHeaderDropFrame}

  // AVEncVideoHeaderHours (UINT32)
  CODECAPI_AVEncVideoHeaderHours                :  TGUID = '{2acc7702-e2da-4158-bf9b-88880129d740}';
  {$EXTERNALSYM CODECAPI_AVEncVideoHeaderHours}

  // AVEncVideoHeaderMinutes (UINT32)
  CODECAPI_AVEncVideoHeaderMinutes              :  TGUID = '{dc1a99ce-0307-408b-880b-b8348ee8ca7f}';
  {$EXTERNALSYM CODECAPI_AVEncVideoHeaderMinutes}

  // AVEncVideoHeaderSeconds (UINT32)
  CODECAPI_AVEncVideoHeaderSeconds              :  TGUID = '{4a2e1a05-a780-4f58-8120-9a449d69656b}';
  {$EXTERNALSYM CODECAPI_AVEncVideoHeaderSeconds}

  // AVEncVideoHeaderFrames (UINT32)
  CODECAPI_AVEncVideoHeaderFrames               :  TGUID = '{afd5f567-5c1b-4adc-bdaf-735610381436}';
  {$EXTERNALSYM CODECAPI_AVEncVideoHeaderFrames}

  // AVEncVideoDefaultUpperFieldDominant (BOOL)
  CODECAPI_AVEncVideoDefaultUpperFieldDominant  :  TGUID = '{810167c4-0bc1-47ca-8fc2-57055a1474a5}';
  {$EXTERNALSYM CODECAPI_AVEncVideoDefaultUpperFieldDominant}

  // AVEncVideoCBRMotionTradeoff (UINT32)
  CODECAPI_AVEncVideoCBRMotionTradeoff          :  TGUID = '{0d49451e-18d5-4367-a4ef-3240df1693c4}';
  {$EXTERNALSYM CODECAPI_AVEncVideoCBRMotionTradeoff}

  // AVEncVideoCodedVideoAccessUnitSize (UINT32)
  CODECAPI_AVEncVideoCodedVideoAccessUnitSize   :  TGUID = '{b4b10c15-14a7-4ce8-b173-dc90a0b4fcdb}';
  {$EXTERNALSYM CODECAPI_AVEncVideoCodedVideoAccessUnitSize}

  // AVEncVideoMaxKeyframeDistance (UINT32)
  CODECAPI_AVEncVideoMaxKeyframeDistance        :  TGUID = '{2987123a-ba93-4704-b489-ec1e5f25292c}';
  {$EXTERNALSYM CODECAPI_AVEncVideoMaxKeyframeDistance}

  // AVEncH264CABACEnable (BOOL)
  CODECAPI_AVEncH264CABACEnable                 :  TGUID = '{ee6cad62-d305-4248-a50e-e1b255f7caf8}';
  {$EXTERNALSYM CODECAPI_AVEncH264CABACEnable}

  // AVEncVideoContentType (UINT32)
  CODECAPI_AVEncVideoContentType                :  TGUID = '{66117aca-eb77-459d-930c-a48d9d0683fc}';
  {$EXTERNALSYM CODECAPI_AVEncVideoContentType}

  // AVEncNumWorkerThreads (UINT32)
  CODECAPI_AVEncNumWorkerThreads                :  TGUID = '{b0c8bf60-16f7-4951-a30b-1db1609293d6}';
  {$EXTERNALSYM CODECAPI_AVEncNumWorkerThreads}

  // AVEncVideoEncodeQP (UINT64)
  CODECAPI_AVEncVideoEncodeQP                   :  TGUID = '{2cb5696b-23fb-4ce1-a0f9-ef5b90fd55ca}';
  {$EXTERNALSYM CODECAPI_AVEncVideoEncodeQP}

  // AVEncVideoMinQP (UINT32)
  CODECAPI_AVEncVideoMinQP                      :  TGUID = '{0ee22c6a-a37c-4568-b5f1-9d4c2b3ab886}';
  {$EXTERNALSYM CODECAPI_AVEncVideoMinQP}

  // AVEncVideoForceKeyFrame (UINT32)
  CODECAPI_AVEncVideoForceKeyFrame              :  TGUID = '{398c1b98-8353-475a-9ef2-8f265d260345}';
  {$EXTERNALSYM CODECAPI_AVEncVideoForceKeyFrame}

  // AVEncH264SPSID (UINT32)
  CODECAPI_AVEncH264SPSID                       :  TGUID = '{50f38f51-2b79-40e3-b39c-7e9fa0770501}';
  {$EXTERNALSYM CODECAPI_AVEncH264SPSID}

  // AVEncH264PPSID (UINT32)
  CODECAPI_AVEncH264PPSID                       :  TGUID = '{bfe29ec2-056c-4d68-a38d-ae5944c8582e}';
  {$EXTERNALSYM CODECAPI_AVEncH264PPSID}

  // AVEncAdaptiveMode (UINT32)
  CODECAPI_AVEncAdaptiveMode                    :  TGUID = '{4419b185-da1f-4f53-bc76-097d0c1efb1e}';
  {$EXTERNALSYM CODECAPI_AVEncAdaptiveMode}


  // AVEncVideoSelectLayer (UINT32)
  CODECAPI_AVEncVideoSelectLayer                :  TGUID = '{eb1084f5-6aaa-4914-bb2f-6147227f12e7}';
  {$EXTERNALSYM CODECAPI_AVEncVideoSelectLayer}

  // AVEncVideoTemporalLayerCount (UINT32)
  CODECAPI_AVEncVideoTemporalLayerCount         :  TGUID = '{19caebff-b74d-4cfd-8c27-c2f9d97d5f52}';
  {$EXTERNALSYM CODECAPI_AVEncVideoTemporalLayerCount}

  // AVEncVideoUsage (UINT32)
  CODECAPI_AVEncVideoUsage                      :  TGUID = '{1f636849-5dc1-49f1-b1d8-ce3cf62ea385}';
  {$EXTERNALSYM CODECAPI_AVEncVideoUsage}

  // AVEncVideoRateControlParams (UINT64)
  CODECAPI_AVEncVideoRateControlParams          :  TGUID = '{87d43767-7645-44ec-b438-d3322fbca29f}';
  {$EXTERNALSYM CODECAPI_AVEncVideoRateControlParams}

  // AVEncVideoSupportedControls (UINT64)
  CODECAPI_AVEncVideoSupportedControls          :  TGUID = '{d3f40fdd-77b9-473d-8196-061259e69cff}';
  {$EXTERNALSYM CODECAPI_AVEncVideoSupportedControls}

  // AVEncVideoEncodeFrameTypeQP (UINT64)
  CODECAPI_AVEncVideoEncodeFrameTypeQP          :  TGUID = '{aa70b610-e03f-450c-ad07-07314e639ce7}';
  {$EXTERNALSYM CODECAPI_AVEncVideoEncodeFrameTypeQP}

  // AVEncSliceControlMode (UINT32)
  CODECAPI_AVEncSliceControlMode                :  TGUID = '{e9e782ef-5f18-44c9-a90b-e9c3c2c17b0b}';
  {$EXTERNALSYM CODECAPI_AVEncSliceControlMode}

  // AVEncSliceControlSize (UINT32)
  CODECAPI_AVEncSliceControlSize                :  TGUID = '{92f51df3-07a5-4172-aefe-c69ca3b60e35}';
  {$EXTERNALSYM CODECAPI_AVEncSliceControlSize}

  // CODECAPI_AVEncSliceGenerationMode (UINT32)
  CODECAPI_AVEncSliceGenerationMode         :  TGUID = '{8a6bc67f-9497-4286-b46b-02db8d60edbc}';
  {$EXTERNALSYM CODECAPI_AVEncSliceGenerationMode}

  // AVEncVideoMaxNumRefFrame (UINT32)
  CODECAPI_AVEncVideoMaxNumRefFrame         :  TGUID = '{964829ed-94f9-43b4-b74d-ef40944b69a0}';
  {$EXTERNALSYM CODECAPI_AVEncVideoMaxNumRefFrame}

  // AVEncVideoMeanAbsoluteDifference (UINT32)
  CODECAPI_AVEncVideoMeanAbsoluteDifference :  TGUID = '{e5c0c10f-81a4-422d-8c3f-b474a4581336}';
  {$EXTERNALSYM CODECAPI_AVEncVideoMeanAbsoluteDifference}

  // AVEncVideoMaxQP (UINT32)
  CODECAPI_AVEncVideoMaxQP            :  TGUID = '{3daf6f66-a6a7-45e0-a8e5-f2743f46a3a2}';
  {$EXTERNALSYM CODECAPI_AVEncVideoMaxQP}

  // AVEncVideoLTRBufferControl (UINT32)
  CODECAPI_AVEncVideoLTRBufferControl :  TGUID = '{a4a0e93d-4cbc-444c-89f4-826d310e92a7}';
  {$EXTERNALSYM CODECAPI_AVEncVideoLTRBufferControl}

  // AVEncVideoMarkLTRFrame (UINT32)
  CODECAPI_AVEncVideoMarkLTRFrame     :  TGUID = '{e42f4748-a06d-4ef9-8cea-3d05fde3bd3b}';
  {$EXTERNALSYM CODECAPI_AVEncVideoMarkLTRFrame}

  // AVEncVideoUseLTRFrame (UINT32)
  CODECAPI_AVEncVideoUseLTRFrame      :  TGUID = '{00752db8-55f7-4f80-895b-27639195f2ad}';
  {$EXTERNALSYM CODECAPI_AVEncVideoUseLTRFrame}

  // AVEncVideoROIEnabled (UINT32)
  CODECAPI_AVEncVideoROIEnabled       :  TGUID = '{d74f7f18-44dd-4b85-aba3-05d9f42a8280}';
  {$EXTERNALSYM CODECAPI_AVEncVideoROIEnabled}

  // AVEncMaxFrameRate (UINT64)
  CODECAPI_AVEncMaxFrameRate          :  TGUID = '{B98E1B31-19FA-4D4F-9931-D6A5B8AAB93C}';
  {$EXTERNALSYM CODECAPI_AVEncMaxFrameRate}



  // Audio/Video Mux

  // AVEncMuxOutputStreamType (UINT32)
  CODECAPI_AVEncMuxOutputStreamType :  TGUID = '{cedd9e8f-34d3-44db-a1d8-f81520254f3e}';
  {$EXTERNALSYM CODECAPI_AVEncMuxOutputStreamType}

  //
  // Common Post-Encode Video Statistical Parameters
  //

  // AVEncStatVideoOutputFrameRate (UINT32/UINT32)
  CODECAPI_AVEncStatVideoOutputFrameRate :  TGUID = '{be747849-9ab4-4a63-98fe-f143f04f8ee9}';
  {$EXTERNALSYM CODECAPI_AVEncStatVideoOutputFrameRate}

  // AVEncStatVideoCodedFrames (UINT32)
  CODECAPI_AVEncStatVideoCodedFrames :  TGUID = '{d47f8d61-6f5a-4a26-bb9f-cd9518462bcd}';
  {$EXTERNALSYM CODECAPI_AVEncStatVideoCodedFrames}

  // AVEncStatVideoTotalFrames (UINT32)
  CODECAPI_AVEncStatVideoTotalFrames :  TGUID = '{fdaa9916-119a-4222-9ad6-3f7cab99cc8b}';
  {$EXTERNALSYM CODECAPI_AVEncStatVideoTotalFrames}

  //
  // Common Audio Parameters
  //

  // AVEncAudioIntervalToEncode (UINT64)
  CODECAPI_AVEncAudioIntervalToEncode :  TGUID = '{866e4b4d-725a-467c-bb01-b496b23b25f9}';
  {$EXTERNALSYM CODECAPI_AVEncAudioIntervalToEncode}

  // AVEncAudioIntervalToSkip (UINT64)
  CODECAPI_AVEncAudioIntervalToSkip :  TGUID = '{88c15f94-c38c-4796-a9e8-96e967983f26}';
  {$EXTERNALSYM CODECAPI_AVEncAudioIntervalToSkip}

  // AVEncAudioDualMono (UINT32) - Read/Write
  // Some audio encoders can encode 2 channel input as "dual mono". Use this
  // property to set the appropriate field in the bitstream header to indicate that the
  // 2 channel bitstream is or isn't dual mono.
  // For encoding MPEG audio, use the DualChannel option in AVEncMPACodingMode instead
  CODECAPI_AVEncAudioDualMono :  TGUID = '{3648126b-a3e8-4329-9b3a-5ce566a43bd3}';
  {$EXTERNALSYM CODECAPI_AVEncAudioDualMono}

  // AVEncAudioMeanBitRate (UINT32) - Read/Write - Used to specify audio bitrate (in bits per second) when the encoder is instantiated as an audio+video encoder.
  CODECAPI_AVEncAudioMeanBitRate :  TGUID = '{921295bb-4fca-4679-aab8-9e2a1d753384}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMeanBitRate}

  // AVEncAudioMapDestChannel0..15 (UINT32)
  CODECAPI_AVEncAudioMapDestChannel0 :  TGUID = '{bc5d0b60-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel0}
  CODECAPI_AVEncAudioMapDestChannel1 :  TGUID = '{bc5d0b61-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel1}
  CODECAPI_AVEncAudioMapDestChannel2 :  TGUID = '{bc5d0b62-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel2}
  CODECAPI_AVEncAudioMapDestChannel3 :  TGUID = '{bc5d0b63-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel3}
  CODECAPI_AVEncAudioMapDestChannel4 :  TGUID = '{bc5d0b64-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel4}
  CODECAPI_AVEncAudioMapDestChannel5 :  TGUID = '{bc5d0b65-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel5}
  CODECAPI_AVEncAudioMapDestChannel6 :  TGUID = '{bc5d0b66-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel6}
  CODECAPI_AVEncAudioMapDestChannel7 :  TGUID = '{bc5d0b67-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel7}
  CODECAPI_AVEncAudioMapDestChannel8 :  TGUID = '{bc5d0b68-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel8}
  CODECAPI_AVEncAudioMapDestChannel9 :  TGUID = '{bc5d0b69-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel9}
  CODECAPI_AVEncAudioMapDestChannel10 :  TGUID = '{bc5d0b6a-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel10}
  CODECAPI_AVEncAudioMapDestChannel11 :  TGUID = '{bc5d0b6b-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel11}
  CODECAPI_AVEncAudioMapDestChannel12 :  TGUID = '{bc5d0b6c-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel12}
  CODECAPI_AVEncAudioMapDestChannel13 :  TGUID = '{bc5d0b6d-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel13}
  CODECAPI_AVEncAudioMapDestChannel14 :  TGUID = '{bc5d0b6e-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel14}
  CODECAPI_AVEncAudioMapDestChannel15 :  TGUID = '{bc5d0b6f-df6a-4e16-9803-b82007a30c8d}';
  {$EXTERNALSYM CODECAPI_AVEncAudioMapDestChannel15}

  // AVEncAudioInputContent (UINT32) <---- You have ENUM in the doc
  CODECAPI_AVEncAudioInputContent :  TGUID = '{3e226c2b-60b9-4a39-b00b-a7b40f70d566}';
  {$EXTERNALSYM CODECAPI_AVEncAudioInputContent}

  // Common Post-Encode Audio Statistical Parameters

  // AVEncStatAudioPeakPCMValue (UINT32)
  CODECAPI_AVEncStatAudioPeakPCMValue :  TGUID = '{dce7fd34-dc00-4c16-821b-35d9eb00fb1a}';
  {$EXTERNALSYM CODECAPI_AVEncStatAudioPeakPCMValue}

  // AVEncStatAudioAveragePCMValue (UINT32)
  CODECAPI_AVEncStatAudioAveragePCMValue :  TGUID = '{979272f8-d17f-4e32-bb73-4e731c68ba2d}';
  {$EXTERNALSYM CODECAPI_AVEncStatAudioAveragePCMValue}

  // AVEncStatAverageBPS (UINT32)
  CODECAPI_AVEncStatAudioAverageBPS :  TGUID = '{ca6724db-7059-4351-8b43-f82198826a14}';
  {$EXTERNALSYM CODECAPI_AVEncStatAudioAverageBPS}
  CODECAPI_AVEncStatAverageBPS :  TGUID = '{ca6724db-7059-4351-8b43-f82198826a14}';
  {$EXTERNALSYM CODECAPI_AVEncStatAverageBPS}

  // AVEncStatHardwareProcessorUtilitization (UINT32)
  // HW usage % x 1000
  CODECAPI_AVEncStatHardwareProcessorUtilitization :  TGUID = '{995dc027-cb95-49e6-b91b-5967753cdcb8}';

  // AVEncStatHardwareBandwidthUtilitization (UINT32)
  // HW usage % x 1000
  CODECAPI_AVEncStatHardwareBandwidthUtilitization :  TGUID = '{0124ba9b-dc41-4826-b45f-18ac01b3d5a8}';
  {$EXTERNALSYM CODECAPI_AVEncStatHardwareBandwidthUtilitization}

  //
  // MPV Encoder Specific Parameters
  //

  // AVEncMPVGOPSize (UINT32)
  CODECAPI_AVEncMPVGOPSize :  TGUID = '{95f31b26-95a4-41aa-9303-246a7fc6eef1}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGOPSize}

  // AVEncMPVGOPOpen (BOOL)
  CODECAPI_AVEncMPVGOPOpen :  TGUID = '{b1d5d4a6-3300-49b1-ae61-a09937ab0e49}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGOPOpen}

  // AVEncMPVDefaultBPictureCount (UINT32)
  CODECAPI_AVEncMPVDefaultBPictureCount :  TGUID = '{8d390aac-dc5c-4200-b57f-814d04babab2}';
  {$EXTERNALSYM CODECAPI_AVEncMPVDefaultBPictureCount}

  // AVEncMPVProfile (UINT32) <---- You have GUID in the doc
  CODECAPI_AVEncMPVProfile :  TGUID = '{dabb534a-1d99-4284-975a-d90e2239baa1}';
  {$EXTERNALSYM CODECAPI_AVEncMPVProfile}

  // AVEncMPVLevel (UINT32) <---- You have GUID in the doc
  CODECAPI_AVEncMPVLevel :  TGUID = '{6ee40c40-a60c-41ef-8f50-37c2249e2cb3}';
  {$EXTERNALSYM CODECAPI_AVEncMPVLevel}

  //
  // Advanced MPV Encoder Specific Parameters
  //

  // AVEncMPVAddSeqEndCode (BOOL)
  CODECAPI_AVEncMPVAddSeqEndCode :  TGUID = '{a823178f-57df-4c7a-b8fd-e5ec8887708d}';
  {$EXTERNALSYM CODECAPI_AVEncMPVAddSeqEndCode}

  // AVEncMPVGOPSInSeq (UINT32)
  CODECAPI_AVEncMPVGOPSInSeq :  TGUID = '{993410d4-2691-4192-9978-98dc2603669f}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGOPSInSeq}

  // AVEncMPVUseConcealmentMotionVectors (BOOL)
  CODECAPI_AVEncMPVUseConcealmentMotionVectors :  TGUID = '{ec770cf3-6908-4b4b-aa30-7fb986214fea}';
  {$EXTERNALSYM CODECAPI_AVEncMPVUseConcealmentMotionVectors}

  // AVEncMPVSceneDetection (UINT32)
  CODECAPI_AVEncMPVSceneDetection :  TGUID = '{552799f1-db4c-405b-8a3a-c93f2d0674dc}';
  {$EXTERNALSYM CODECAPI_AVEncMPVSceneDetection}

  // Dolby Digital(TM) Audio Specific Parameters
  //
  // AVEncDDService (UINT)
  CODECAPI_AVEncDDService :  TGUID = '{d2e1bec7-5172-4d2a-a50e-2f3b82b1ddf8}';
  {$EXTERNALSYM CODECAPI_AVEncDDService}

  // AVEncMPVFrameFieldMode (UINT32)
  CODECAPI_AVEncMPVFrameFieldMode :  TGUID = '{acb5de96-7b93-4c2f-8825-b0295fa93bf4}';
  {$EXTERNALSYM CODECAPI_AVEncMPVFrameFieldMode}

  // AVEncMPVGenerateHeaderSeqExt (BOOL)
  CODECAPI_AVEncMPVGenerateHeaderSeqExt :  TGUID = '{d5e78611-082d-4e6b-98af-0f51ab139222}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGenerateHeaderSeqExt}

  // AVEncMPVGenerateHeaderSeqDispExt (BOOL)
  CODECAPI_AVEncMPVGenerateHeaderSeqDispExt :  TGUID = '{6437aa6f-5a3c-4de9-8a16-53d9c4ad326f}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGenerateHeaderSeqDispExt}

  // AVEncMPVGenerateHeaderPicExt (BOOL)
  CODECAPI_AVEncMPVGenerateHeaderPicExt :  TGUID = '{1b8464ab-944f-45f0-b74e-3a58dad11f37}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGenerateHeaderPicExt}

  // AVEncMPVGenerateHeaderPicDispExt (BOOL)
  CODECAPI_AVEncMPVGenerateHeaderPicDispExt :  TGUID = '{c6412f84-c03f-4f40-a00c-4293df8395bb}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGenerateHeaderPicDispExt}

  // AVEncMPVGenerateHeaderSeqScaleExt (BOOL)
  CODECAPI_AVEncMPVGenerateHeaderSeqScaleExt :  TGUID = '{0722d62f-dd59-4a86-9cd5-644f8e2653d8}';
  {$EXTERNALSYM CODECAPI_AVEncMPVGenerateHeaderSeqScaleExt}

  // AVEncMPVScanPattern (UINT32)
  CODECAPI_AVEncMPVScanPattern :  TGUID = '{7f8a478e-7bbb-4ae2-b2fc-96d17fc4a2d6}';
  {$EXTERNALSYM CODECAPI_AVEncMPVScanPattern}
  // AVEncMPVIntraDCPrecision (UINT32)
  CODECAPI_AVEncMPVIntraDCPrecision :  TGUID = '{a0116151-cbc8-4af3-97dc-d00cceb82d79}';
  {$EXTERNALSYM CODECAPI_AVEncMPVIntraDCPrecision}

  // AVEncMPVQScaleType (UINT32)
  CODECAPI_AVEncMPVQScaleType :  TGUID = '{2b79ebb7-f484-4af7-bb58-a2a188c5cbbe}';
  {$EXTERNALSYM CODECAPI_AVEncMPVQScaleType}

  // AVEncMPVIntraVLCTable (UINT32)
  CODECAPI_AVEncMPVIntraVLCTable :  TGUID = '{a2b83ff5-1a99-405a-af95-c5997d558d3a}';
  {$EXTERNALSYM CODECAPI_AVEncMPVIntraVLCTable}

  // AVEncMPVQuantMatrixIntra (BYTE[64] encoded as a string of 128 hex digits)
  CODECAPI_AVEncMPVQuantMatrixIntra :  TGUID = '{9bea04f3-6621-442c-8ba1-3ac378979698}';
  {$EXTERNALSYM CODECAPI_AVEncMPVQuantMatrixIntra}

  // AVEncMPVQuantMatrixNonIntra (BYTE[64] encoded as a string of 128 hex digits)
  CODECAPI_AVEncMPVQuantMatrixNonIntra :  TGUID = '{87f441d8-0997-4beb-a08e-8573d409cf75}';
  {$EXTERNALSYM CODECAPI_AVEncMPVQuantMatrixNonIntra}

  // AVEncMPVQuantMatrixChromaIntra (BYTE[64] encoded as a string of 128 hex digits)
  CODECAPI_AVEncMPVQuantMatrixChromaIntra :  TGUID = '{9eb9ecd4-018d-4ffd-8f2d-39e49f07b17a}';
  {$EXTERNALSYM CODECAPI_AVEncMPVQuantMatrixChromaIntra}

  // AVEncMPVQuantMatrixChromaNonIntra (BYTE[64] encoded as a string of 128 hex digits)
  CODECAPI_AVEncMPVQuantMatrixChromaNonIntra :  TGUID = '{1415b6b1-362a-4338-ba9a-1ef58703c05b}';
  {$EXTERNALSYM CODECAPI_AVEncMPVQuantMatrixChromaNonIntra}

  // AVEncMPALayer (UINT)
  CODECAPI_AVEncMPALayer :  TGUID = '{9d377230-f91b-453d-9ce0-78445414c22d}';
  {$EXTERNALSYM CODECAPI_AVEncMPALayer}
  // AVEncMPACodingMode (UINT)
  CODECAPI_AVEncMPACodingMode :  TGUID = '{b16ade03-4b93-43d7-a550-90b4fe224537}';
  {$EXTERNALSYM CODECAPI_AVEncMPACodingMode}

  // AVEncMPACopyright (BOOL) - default state to encode into the stream (may be overridden by input)
  // 1 (true)  - copyright protected
  // 0 (false) - not copyright protected
  CODECAPI_AVEncMPACopyright :  TGUID = '{a6ae762a-d0a9-4454-b8ef-f2dbeefdd3bd}';
  {$EXTERNALSYM CODECAPI_AVEncMPACopyright}

  // AVEncMPAOriginalBitstream (BOOL) - default value to encode into the stream (may be overridden by input)
  // 1 (true)  - for original bitstream
  // 0 (false) - for copy bitstream
  CODECAPI_AVEncMPAOriginalBitstream :  TGUID = '{3cfb7855-9cc9-47ff-b829-b36786c92346}';
  {$EXTERNALSYM CODECAPI_AVEncMPAOriginalBitstream}

  // AVEncMPAEnableRedundancyProtection (BOOL)
  // 1 (true)  -  Redundancy should be added to facilitate error detection and concealment (CRC)
  // 0 (false) -  No redundancy should be added
  CODECAPI_AVEncMPAEnableRedundancyProtection :  TGUID = '{5e54b09e-b2e7-4973-a89b-0b3650a3beda}';
  {$EXTERNALSYM CODECAPI_AVEncMPAEnableRedundancyProtection}

  // AVEncMPAPrivateUserBit (UINT) - User data bit value to encode in the stream
  CODECAPI_AVEncMPAPrivateUserBit :  TGUID = '{afa505ce-c1e3-4e3d-851b-61b700e5e6cc}';
  {$EXTERNALSYM CODECAPI_AVEncMPAPrivateUserBit}

  // AVEncMPAEmphasisType (UINT)
  // Indicates type of de-emphasis filter to be used
  CODECAPI_AVEncMPAEmphasisType :  TGUID = '{2d59fcda-bf4e-4ed6-b5df-5b03b36b0a1f}';
  {$EXTERNALSYM CODECAPI_AVEncMPAEmphasisType}

  // AVEncDDDialogNormalization (UINT32)
  CODECAPI_AVEncDDDialogNormalization :  TGUID = '{d7055acf-f125-437d-a704-79c79f0404a8}';
  {$EXTERNALSYM CODECAPI_AVEncDDDialogNormalization}

  // AVEncDDCentreDownMixLevel (UINT32)
  CODECAPI_AVEncDDCentreDownMixLevel :  TGUID = '{e285072c-c958-4a81-afd2-e5e0daf1b148}';
  {$EXTERNALSYM CODECAPI_AVEncDDCentreDownMixLevel}

  // AVEncDDSurroundDownMixLevel (UINT32)
  CODECAPI_AVEncDDSurroundDownMixLevel :  TGUID = '{7b20d6e5-0bcf-4273-a487-506b047997e9}';
  {$EXTERNALSYM CODECAPI_AVEncDDSurroundDownMixLevel}

  // AVEncDDProductionInfoExists (BOOL)
  CODECAPI_AVEncDDProductionInfoExists :  TGUID = '{b0b7fe5f-b6ab-4f40-964d-8d91f17c19e8}';
  {$EXTERNALSYM CODECAPI_AVEncDDProductionInfoExists}

  // AVEncDDProductionRoomType (UINT32)
  CODECAPI_AVEncDDProductionRoomType :  TGUID = '{dad7ad60-23d8-4ab7-a284-556986d8a6fe}';
  {$EXTERNALSYM CODECAPI_AVEncDDProductionRoomType}

  // AVEncDDProductionMixLevel (UINT32)
  CODECAPI_AVEncDDProductionMixLevel :  TGUID = '{301d103a-cbf9-4776-8899-7c15b461ab26}';
  {$EXTERNALSYM CODECAPI_AVEncDDProductionMixLevel}

  // AVEncDDCopyright (BOOL)
  CODECAPI_AVEncDDCopyright :  TGUID = '{8694f076-cd75-481d-a5c6-a904dcc828f0}';
  {$EXTERNALSYM CODECAPI_AVEncDDCopyright}

  // AVEncDDOriginalBitstream (BOOL)
  CODECAPI_AVEncDDOriginalBitstream :  TGUID = '{966ae800-5bd3-4ff9-95b9-d30566273856}';
  {$EXTERNALSYM CODECAPI_AVEncDDOriginalBitstream}

  // AVEncDDDigitalDeemphasis (BOOL)
  CODECAPI_AVEncDDDigitalDeemphasis :  TGUID = '{e024a2c2-947c-45ac-87d8-f1030c5c0082}';
  {$EXTERNALSYM CODECAPI_AVEncDDDigitalDeemphasis}

  // AVEncDDDCHighPassFilter (BOOL)
  CODECAPI_AVEncDDDCHighPassFilter :  TGUID = '{9565239f-861c-4ac8-bfda-e00cb4db8548}';
  {$EXTERNALSYM CODECAPI_AVEncDDDCHighPassFilter}

  // AVEncDDChannelBWLowPassFilter (BOOL)
  CODECAPI_AVEncDDChannelBWLowPassFilter :  TGUID = '{e197821d-d2e7-43e2-ad2c-00582f518545}';
  {$EXTERNALSYM CODECAPI_AVEncDDChannelBWLowPassFilter}

  // AVEncDDLFELowPassFilter (BOOL)
  CODECAPI_AVEncDDLFELowPassFilter :  TGUID = '{d3b80f6f-9d15-45e5-91be-019c3fab1f01}';
  {$EXTERNALSYM CODECAPI_AVEncDDLFELowPassFilter}

  // AVEncDDSurround90DegreeePhaseShift (BOOL)
  CODECAPI_AVEncDDSurround90DegreeePhaseShift :  TGUID = '{25ecec9d-3553-42c0-bb56-d25792104f80}';
  {$EXTERNALSYM CODECAPI_AVEncDDSurround90DegreeePhaseShift}

  // AVEncDDSurround3dBAttenuation (BOOL)
  CODECAPI_AVEncDDSurround3dBAttenuation :  TGUID = '{4d43b99d-31e2-48b9-bf2e-5cbf1a572784}';
  {$EXTERNALSYM CODECAPI_AVEncDDSurround3dBAttenuation}

  // AVEncDDDynamicRangeCompressionControl (UINT32)
  CODECAPI_AVEncDDDynamicRangeCompressionControl :  TGUID = '{cfc2ff6d-79b8-4b8d-a8aa-a0c9bd1c2940}';
  {$EXTERNALSYM CODECAPI_AVEncDDDynamicRangeCompressionControl}

  // AVEncDDRFPreEmphasisFilter (BOOL)
  CODECAPI_AVEncDDRFPreEmphasisFilter :  TGUID = '{21af44c0-244e-4f3d-a2cc-3d3068b2e73f}';
  {$EXTERNALSYM CODECAPI_AVEncDDRFPreEmphasisFilter}

  // AVEncDDSurroundExMode (UINT32)
  CODECAPI_AVEncDDSurroundExMode :  TGUID = '{91607cee-dbdd-4eb6-bca2-aadfafa3dd68}';
  {$EXTERNALSYM CODECAPI_AVEncDDSurroundExMode}

  // AVEncDDPreferredStereoDownMixMode (UINT32)
  CODECAPI_AVEncDDPreferredStereoDownMixMode :  TGUID = '{7f4e6b31-9185-403d-b0a2-763743e6f063}';
  {$EXTERNALSYM CODECAPI_AVEncDDPreferredStereoDownMixMode}

  // AVEncDDLtRtCenterMixLvl_x10 (INT32)
  CODECAPI_AVEncDDLtRtCenterMixLvl_x10 :  TGUID = '{dca128a2-491f-4600-b2da-76e3344b4197}';
  {$EXTERNALSYM CODECAPI_AVEncDDLtRtCenterMixLvl_x10}

  // AVEncDDLtRtSurroundMixLvl_x10 (INT32)
  CODECAPI_AVEncDDLtRtSurroundMixLvl_x10 :  TGUID = '{212246c7-3d2c-4dfa-bc21-652a9098690d}';
  {$EXTERNALSYM CODECAPI_AVEncDDLtRtSurroundMixLvl_x10}

  // AVEncDDLoRoCenterMixLvl (INT32)
  CODECAPI_AVEncDDLoRoCenterMixLvl_x10 :  TGUID = '{1cfba222-25b3-4bf4-9bfd-e7111267858c}';
  {$EXTERNALSYM CODECAPI_AVEncDDLoRoCenterMixLvl_x10}

  // AVEncDDLoRoSurroundMixLvl_x10 (INT32)
  CODECAPI_AVEncDDLoRoSurroundMixLvl_x10 :  TGUID = '{e725cff6-eb56-40c7-8450-2b9367e91555}';
  {$EXTERNALSYM CODECAPI_AVEncDDLoRoSurroundMixLvl_x10}

  // AVEncDDAtoDConverterType (UINT32)
  CODECAPI_AVEncDDAtoDConverterType :  TGUID = '{719f9612-81a1-47e0-9a05-d94ad5fca948}';
  {$EXTERNALSYM CODECAPI_AVEncDDAtoDConverterType}

  // AVEncDDHeadphoneMode (UINT32)
  CODECAPI_AVEncDDHeadphoneMode :  TGUID = '{4052dbec-52f5-42f5-9b00-d134b1341b9d}';
  {$EXTERNALSYM CODECAPI_AVEncDDHeadphoneMode}

  // AVEncWMVKeyFrameDistance (UINT32)
  CODECAPI_AVEncWMVKeyFrameDistance :  TGUID = '{5569055e-e268-4771-b83e-9555ea28aed3}';
  {$EXTERNALSYM CODECAPI_AVEncWMVKeyFrameDistance}

  // AVEncWMVInterlacedEncoding (UINT32)
  CODECAPI_AVEncWMVInterlacedEncoding :  TGUID = '{e3d00f8a-c6f5-4e14-a588-0ec87a726f9b}';
  {$EXTERNALSYM CODECAPI_AVEncWMVInterlacedEncoding}

  // AVEncWMVDecoderComplexity (UINT32)
  CODECAPI_AVEncWMVDecoderComplexity :  TGUID = '{f32c0dab-f3cb-4217-b79f-8762768b5f67}';
  {$EXTERNALSYM CODECAPI_AVEncWMVDecoderComplexity}

  // AVEncWMVHasKeyFrameBufferLevelMarker (BOOL)
  CODECAPI_AVEncWMVKeyFrameBufferLevelMarker :  TGUID = '{51ff1115-33ac-426c-a1b1-09321bdf96b4}';
  {$EXTERNALSYM CODECAPI_AVEncWMVKeyFrameBufferLevelMarker}

  // AVEncWMVProduceDummyFrames (UINT32)
  CODECAPI_AVEncWMVProduceDummyFrames :  TGUID = '{d669d001-183c-42e3-a3ca-2f4586d2396c}';
  {$EXTERNALSYM CODECAPI_AVEncWMVProduceDummyFrames}

  //
  // WMV Post-Encode Statistical Parameters
  //

  // AVEncStatWMVCBAvg (UINT32/UINT32)
  CODECAPI_AVEncStatWMVCBAvg :  TGUID = '{6aa6229f-d602-4b9d-b68c-c1ad78884bef}';
  {$EXTERNALSYM CODECAPI_AVEncStatWMVCBAvg}

  // AVEncStatWMVCBMax (UINT32/UINT32)
  CODECAPI_AVEncStatWMVCBMax :  TGUID = '{e976bef8-00fe-44b4-b625-8f238bc03499}';
  {$EXTERNALSYM CODECAPI_AVEncStatWMVCBMax}

  // AVEncStatWMVDecoderComplexityProfile (UINT32)
  CODECAPI_AVEncStatWMVDecoderComplexityProfile :  TGUID = '{89e69fc3-0f9b-436c-974a-df821227c90d}';
  {$EXTERNALSYM CODECAPI_AVEncStatWMVDecoderComplexityProfile}

  // AVEncStatMPVSkippedEmptyFrames (UINT32)
  CODECAPI_AVEncStatMPVSkippedEmptyFrames :  TGUID = '{32195fd3-590d-4812-a7ed-6d639a1f9711}';
  {$EXTERNALSYM CODECAPI_AVEncStatMPVSkippedEmptyFrames}

  //
  // MPEG1/2 Multiplexer Interfaces
  //

  //
  // MPEG1/2 Packetizer Interface
  //

  // Shared with Mux:
  // AVEncMP12MuxEarliestPTS (UINT32)
  // AVEncMP12MuxLargestPacketSize (UINT32)
  // AVEncMP12MuxSysSTDBufferBound (UINT32)

  // AVEncMP12PktzSTDBuffer (UINT32)
  CODECAPI_AVEncMP12PktzSTDBuffer :  TGUID = '{0b751bd0-819e-478c-9435-75208926b377}';
  {$EXTERNALSYM CODECAPI_AVEncMP12PktzSTDBuffer}

  // AVEncMP12PktzStreamID (UINT32)
  CODECAPI_AVEncMP12PktzStreamID :  TGUID = '{c834d038-f5e8-4408-9b60-88f36493fedf}';
  {$EXTERNALSYM CODECAPI_AVEncMP12PktzStreamID}

  // AVEncMP12PktzInitialPTS (UINT32)
  CODECAPI_AVEncMP12PktzInitialPTS :  TGUID = '{2a4f2065-9a63-4d20-ae22-0a1bc896a315}';
  {$EXTERNALSYM CODECAPI_AVEncMP12PktzInitialPTS}

  // AVEncMP12PktzPacketSize (UINT32)
  CODECAPI_AVEncMP12PktzPacketSize :  TGUID = '{ab71347a-1332-4dde-a0e5-ccf7da8a0f22}';
  {$EXTERNALSYM CODECAPI_AVEncMP12PktzPacketSize}

  // AVEncMP12PktzCopyright (BOOL)
  CODECAPI_AVEncMP12PktzCopyright :  TGUID = '{c8f4b0c1-094c-43c7-8e68-a595405a6ef8}';
  {$EXTERNALSYM CODECAPI_AVEncMP12PktzCopyright}

  // AVEncMP12PktzOriginal (BOOL)
  CODECAPI_AVEncMP12PktzOriginal :  TGUID = '{6b178416-31b9-4964-94cb-6bff866cdf83}';
  {$EXTERNALSYM CODECAPI_AVEncMP12PktzOriginal}

  //
  // MPEG1/2 Multiplexer Interface
  //

  // AVEncMP12MuxPacketOverhead (UINT32)
  CODECAPI_AVEncMP12MuxPacketOverhead :  TGUID = '{e40bd720-3955-4453-acf9-b79132a38fa0}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxPacketOverhead}

  // AVEncMP12MuxNumStreams (UINT32)
  CODECAPI_AVEncMP12MuxNumStreams :  TGUID = '{f7164a41-dced-4659-a8f2-fb693f2a4cd0}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxNumStreams}

  // AVEncMP12MuxEarliestPTS (UINT32)
  CODECAPI_AVEncMP12MuxEarliestPTS :  TGUID = '{157232b6-f809-474e-9464-a7f93014a817}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxEarliestPTS}

  // AVEncMP12MuxLargestPacketSize (UINT32)
  CODECAPI_AVEncMP12MuxLargestPacketSize :  TGUID = '{35ceb711-f461-4b92-a4ef-17b6841ed254}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxLargestPacketSize}

  // AVEncMP12MuxInitialSCR (UINT32)
  CODECAPI_AVEncMP12MuxInitialSCR :  TGUID = '{3433ad21-1b91-4a0b-b190-2b77063b63a4}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxInitialSCR}

  // AVEncMP12MuxMuxRate (UINT32)
  CODECAPI_AVEncMP12MuxMuxRate :  TGUID = '{ee047c72-4bdb-4a9d-8e21-41926c823da7}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxMuxRate}

  // AVEncMP12MuxPackSize (UINT32)
  CODECAPI_AVEncMP12MuxPackSize :  TGUID = '{f916053a-1ce8-4faf-aa0b-ba31c80034b8}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxPackSize}

  // AVEncMP12MuxSysSTDBufferBound (UINT32)
  CODECAPI_AVEncMP12MuxSysSTDBufferBound :  TGUID = '{35746903-b545-43e7-bb35-c5e0a7d5093c}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxSysSTDBufferBound}

  // AVEncMP12MuxSysRateBound (UINT32)
  CODECAPI_AVEncMP12MuxSysRateBound :  TGUID = '{05f0428a-ee30-489d-ae28-205c72446710}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxSysRateBound}

  // AVEncMP12MuxTargetPacketizer (UINT32)
  CODECAPI_AVEncMP12MuxTargetPacketizer :  TGUID = '{d862212a-2015-45dd-9a32-1b3aa88205a0}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxTargetPacketizer}

  // AVEncMP12MuxSysFixed (UINT32)
  CODECAPI_AVEncMP12MuxSysFixed :  TGUID = '{cefb987e-894f-452e-8f89-a4ef8cec063a}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxSysFixed}

  // AVEncMP12MuxSysCSPS (UINT32)
  CODECAPI_AVEncMP12MuxSysCSPS :  TGUID = '{7952ff45-9c0d-4822-bc82-8ad772e02993}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxSysCSPS}

  // AVEncMP12MuxSysVideoLock (BOOL)
  CODECAPI_AVEncMP12MuxSysVideoLock :  TGUID = '{b8296408-2430-4d37-a2a1-95b3e435a91d}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxSysVideoLock}

  // AVEncMP12MuxSysAudioLock (BOOL)
  CODECAPI_AVEncMP12MuxSysAudioLock :  TGUID = '{0fbb5752-1d43-47bf-bd79-f2293d8ce337}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxSysAudioLock}

  // AVEncMP12MuxDVDNavPacks (BOOL)
  CODECAPI_AVEncMP12MuxDVDNavPacks :  TGUID = '{c7607ced-8cf1-4a99-83a1-ee5461be3574}';
  {$EXTERNALSYM CODECAPI_AVEncMP12MuxDVDNavPacks}

  //
  // Decoding Interface
  //

  // format values are GUIDs as VARIANT BSTRs
  CODECAPI_AVDecCommonInputFormat :  TGUID = '{e5005239-bd89-4be3-9c0f-5dde317988cc}';
  {$EXTERNALSYM CODECAPI_AVDecCommonInputFormat}
  CODECAPI_AVDecCommonOutputFormat :  TGUID = '{3c790028-c0ce-4256-b1a2-1b0fc8b1dcdc}';
  {$EXTERNALSYM CODECAPI_AVDecCommonOutputFormat}

  // AVDecCommonMeanBitRate - Mean bitrate in mbits/sec (UINT32)
  CODECAPI_AVDecCommonMeanBitRate :  TGUID = '{59488217-007a-4f7a-8e41-5c48b1eac5c6}';
  {$EXTERNALSYM CODECAPI_AVDecCommonMeanBitRate}
  // AVDecCommonMeanBitRateInterval - Mean bitrate interval (in 100ns) (UINT64)
  CODECAPI_AVDecCommonMeanBitRateInterval :  TGUID = '{0ee437c6-38a7-4c5c-944c-68ab42116b85}';
  {$EXTERNALSYM CODECAPI_AVDecCommonMeanBitRateInterval}

  //
  // Audio Decoding Interface
  //

  // Value GUIDS
  // The following 6 GUIDs are values of the AVDecCommonOutputFormat property
  //
  // Stereo PCM output using matrix-encoded stereo down mix (aka Lt/Rt)
  CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_MatrixEncoded :  TGUID = '{696e1d30-548f-4036-825f-7026c60011bd}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_MatrixEncoded}
  //
  // Regular PCM output (any number of channels)
  CODECAPI_GUID_AVDecAudioOutputFormat_PCM :  TGUID = '{696e1d31-548f-4036-825f-7026c60011bd}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioOutputFormat_PCM}
  //
  // SPDIF PCM (IEC 60958) stereo output. Type of stereo down mix should
  // be specified by the application.
  CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_PCM :  TGUID = '{696e1d32-548f-4036-825f-7026c60011bd}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_PCM}
  //
  // SPDIF bitstream (IEC 61937) output, such as AC3, MPEG or DTS.
  CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_Bitstream :  TGUID = '{696e1d33-548f-4036-825f-7026c60011bd}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioOutputFormat_SPDIF_Bitstream}
  //
  // Stereo PCM output using regular stereo down mix (aka Lo/Ro)
  CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Headphones :  TGUID = '{696e1d34-548f-4036-825f-7026c60011bd}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Headphones}

  // Stereo PCM output using automatic selection of stereo down mix
  // mode (Lo/Ro or Lt/Rt). Use this when the input stream includes
  // information about the preferred downmix mode (such as Annex D of AC3).
  // Default down mix mode should be specified by the application.
  CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_Auto :  TGUID = '{696e1d35-548f-4036-825f-7026c60011bd}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioOutputFormat_PCM_Stereo_Auto}

  //
  // Video Decoder properties
  //

  // AVDecVideoImageSize (UINT32) - High UINT16 width, low UINT16 height
  CODECAPI_AVDecVideoImageSize :  TGUID = '{5ee5747c-6801-4cab-aaf1-6248fa841ba4}';
  {$EXTERNALSYM CODECAPI_AVDecVideoImageSize}

  // AVDecVideoPixelAspectRatio (UINT32 as UINT16/UNIT16) - High UINT16 width, low UINT16 height
  CODECAPI_AVDecVideoPixelAspectRatio :  TGUID = '{b0cf8245-f32d-41df-b02c-87bd304d12ab}';
  {$EXTERNALSYM CODECAPI_AVDecVideoPixelAspectRatio}

  // AVDecVideoInputScanType (UINT32)
  CODECAPI_AVDecVideoInputScanType :  TGUID = '{38477e1f-0ea7-42cd-8cd1-130ced57c580}';
  {$EXTERNALSYM CODECAPI_AVDecVideoInputScanType}

  // AVDecVideoSWPowerLevel (UINT32)
  // Related to video decoder software power saving level in MPEG4 Part 2, VC1 and H264.
  // "SW Power Level" will take a range from 0 to 100 to indicate the current power saving level.
  // 0 - Optimize for battery life, 50 - balanced, 100 - Optimize for video quality.
  CODECAPI_AVDecVideoSWPowerLevel :  TGUID = '{fb5d2347-4dd8-4509-aed0-db5fa9aa93f4}';
  {$EXTERNALSYM CODECAPI_AVDecVideoSWPowerLevel}

  CODECAPI_GUID_AVDecAudioInputWMA :  TGUID = '{c95e8dcf-4058-4204-8c42-cb24d91e4b9b}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputWMA}
  CODECAPI_GUID_AVDecAudioInputWMAPro :  TGUID = '{0128b7c7-da72-4fe3-bef8-5c52e3557704}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputWMAPro}
  CODECAPI_GUID_AVDecAudioInputDolby :  TGUID = '{8e4228a0-f000-4e0b-8f54-ab8d24ad61a2}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputDolby}
  CODECAPI_GUID_AVDecAudioInputDTS :  TGUID = '{600bc0ca-6a1f-4e91-b241-1bbeb1cb19e0}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputDTS}
  CODECAPI_GUID_AVDecAudioInputPCM :  TGUID = '{f2421da5-bbb4-4cd5-a996-933c6b5d1347}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputPCM}
  CODECAPI_GUID_AVDecAudioInputMPEG :  TGUID = '{91106f36-02c5-4f75-9719-3b7abf75e1f6}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputMPEG}
  CODECAPI_GUID_AVDecAudioInputAAC :  TGUID = '{97df7828-b94a-47e2-a4bc-51194db22a4d}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputAAC}
  CODECAPI_GUID_AVDecAudioInputHEAAC :  TGUID = '{16efb4aa-330e-4f5c-98a8-cf6ac55cbe60}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputHEAAC}
  CODECAPI_GUID_AVDecAudioInputDolbyDigitalPlus :  TGUID = '{0803e185-8f5d-47f5-9908-19a5bbc9fe34}';
  {$EXTERNALSYM CODECAPI_GUID_AVDecAudioInputDolbyDigitalPlus}

  // AVDecAACDownmixMode (UINT32)
  // AAC/HE-AAC Decoder uses standard ISO/IEC MPEG-2/MPEG-4 stereo downmix equations or uses
  // non-standard downmix. An example of a non standard downmix would be the one defined by ARIB document STD-B21 version 4.4.
  CODECAPI_AVDecAACDownmixMode :  TGUID = '{01274475-f6bb-4017-b084-81a763c942d4}';
  {$EXTERNALSYM CODECAPI_AVDecAACDownmixMode}

  // AVDecHEAACDynamicRangeControl (UINT32)
  // Set this property on an AAC/HE-AAC decoder to select whether Dynamic Range Control (DRC) should be applied or not.
  // If DRC is ON and the AAC/HE-AAC stream includes extension payload of type EXT_DYNAMIC_RANGE, DRC will be applied.
  CODECAPI_AVDecHEAACDynamicRangeControl :  TGUID = '{287c8abe-69a4-4d39-8080-d3d9712178a0}';
  {$EXTERNALSYM CODECAPI_AVDecHEAACDynamicRangeControl}

  // AVDecAudioDualMono (UINT32) - Read only
  // The input bitstream header might have a field indicating whether the 2-ch bitstream
  // is dual mono or not. Use this property to read this field.
  // If it's dual mono, the application can set AVDecAudioDualMonoReproMode to determine
  // one of 4 reproduction modes
  CODECAPI_AVDecAudioDualMono :  TGUID = '{4a52cda8-30f8-4216-be0f-ba0b2025921d}';
  {$EXTERNALSYM CODECAPI_AVDecAudioDualMono}

  // AVDecAudioDualMonoReproMode (UINT32)
  // Reproduction modes for programs containing two independent mono channels (Ch1 & Ch2).
  // In case of 2-ch input, the decoder should get AVDecAudioDualMono to check if the input
  // is regular stereo or dual mono. If dual mono, the application can ask the user to set the playback
  // mode by setting AVDecAudioDualReproMonoMode. If output is not stereo, use AVDecDDMatrixDecodingMode or
  // equivalent.
  CODECAPI_AVDecAudioDualMonoReproMode :  TGUID = '{a5106186-cc94-4bc9-8cd9-aa2f61f6807e}';
  {$EXTERNALSYM CODECAPI_AVDecAudioDualMonoReproMode}

  // AVAudioChannelCount (UINT32)
  // Total number of audio channels, including LFE if it exists.
  CODECAPI_AVAudioChannelCount :  TGUID = '{1d3583c4-1583-474e-b71a-5ee463c198e4}';
  {$EXTERNALSYM CODECAPI_AVAudioChannelCount}

  // AVAudioChannelConfig (UINT32)
  // A bit-wise OR of any number of enum values specified by eAVAudioChannelConfig
  CODECAPI_AVAudioChannelConfig :  TGUID = '{17f89cb3-c38d-4368-9ede-63b94d177f9f}';
  {$EXTERNALSYM CODECAPI_AVAudioChannelConfig}

  // AVAudioSampleRate (UINT32)
  // In samples per second (Hz)
  CODECAPI_AVAudioSampleRate :  TGUID = '{971d2723-1acb-42e7-855c-520a4b70a5f2}';
  {$EXTERNALSYM CODECAPI_AVAudioSampleRate}

  //
  // Dolby Digital(TM) Audio Specific Parameters
  //

  // AVDDSurroundMode (UINT32) common to encoder/decoder
  CODECAPI_AVDDSurroundMode :  TGUID = '{99f2f386-98d1-4452-a163-abc78a6eb770}';
  {$EXTERNALSYM CODECAPI_AVDDSurroundMode}

  // AVDecDDOperationalMode (UINT32)
  CODECAPI_AVDecDDOperationalMode :  TGUID = '{d6d6c6d1-064e-4fdd-a40e-3ecbfcb7ebd0}';
  {$EXTERNALSYM CODECAPI_AVDecDDOperationalMode}

  // AVDecDDMatrixDecodingMode(UINT32)
  // A ProLogic decoder has a built-in auto-detection feature. When the Dolby Digital decoder
  // is set to the 6-channel output configuration and it is fed a 2/0 bit stream to decode, it can
  // do one of the following:
  // a) decode the bit stream and output it on the two front channels (eAVDecDDMatrixDecodingMode_OFF),
  // b) decode the bit stream followed by ProLogic decoding to create 6-channels (eAVDecDDMatrixDecodingMode_ON).
  // c) the decoder will look at the Surround bit ("dsurmod") in the bit stream to determine whether
  //    apply ProLogic decoding or not (eAVDecDDMatrixDecodingMode_AUTO).
  CODECAPI_AVDecDDMatrixDecodingMode :  TGUID = '{ddc811a5-04ed-4bf3-a0ca-d00449f9355f}';
  {$EXTERNALSYM CODECAPI_AVDecDDMatrixDecodingMode}

  // AVDecDDDynamicRangeScaleHigh (UINT32)
  // Indicates what fraction of the dynamic range compression
  // to apply. Relevant for negative values of dynrng only.
  // Linear range 0-100, where:
  //   0 - No dynamic range compression (preserve full dynamic range)
  // 100 - Apply full dynamic range compression
  CODECAPI_AVDecDDDynamicRangeScaleHigh :  TGUID = '{50196c21-1f33-4af5-b296-11426d6c8789}';
  {$EXTERNALSYM CODECAPI_AVDecDDDynamicRangeScaleHigh}

  // AVDecDDDynamicRangeScaleLow (UINT32)
  // Indicates what fraction of the dynamic range compression
  // to apply. Relevant for positive values of dynrng only.
  // Linear range 0-100, where:
  //   0 - No dynamic range compression (preserve full dynamic range)
  // 100 - Apply full dynamic range compression
  CODECAPI_AVDecDDDynamicRangeScaleLow :  TGUID = '{044e62e4-11a5-42d5-a3b2-3bb2c7c2d7cf}';
  {$EXTERNALSYM CODECAPI_AVDecDDDynamicRangeScaleLow}


  // AVDecDDStereoDownMixMode (UINT32)
  // A Dolby Digital Decoder may apply stereo downmix in one of several ways, after decoding multichannel PCM.
  // Use one of the UINT32 values specified by eAVDecDDStereoDownMixMode to set stereo downmix mode.
  // Only relevant when the decoder's output is set to stereo.
  CODECAPI_AVDecDDStereoDownMixMode :  TGUID = '{6ce4122c-3ee9-4182-b4ae-c10fc088649d}';
  {$EXTERNALSYM CODECAPI_AVDecDDStereoDownMixMode}

  // AVDSPLoudnessEqualization (UINT32)
  // Related to audio digital signal processing (DSP).
  // Apply "Loudness Equalization" to the audio stream, so users will not have to adjust volume control when audio stream changes.
  CODECAPI_AVDSPLoudnessEqualization :  TGUID = '{8afd1a15-1812-4cbf-9319-433a5b2a3b27}';
  {$EXTERNALSYM CODECAPI_AVDSPLoudnessEqualization}

  // AVDSPSpeakerFill (UINT32)
  // Related to audio digital signal processing (DSP).
  // "Speaker Fill" will take a mono or stereo audio stream and convert it to a multi channel (e.g. 5.1) audio stream.
  CODECAPI_AVDSPSpeakerFill :  TGUID = '{5612bca1-56da-4582-8da1-ca8090f92768}';
  {$EXTERNALSYM CODECAPI_AVDSPSpeakerFill}

  // AVPriorityControl (UINT32)
  // Indicates the task priority when not realtime (0..15)
  // Linear range 0-15, where:
  //   0 - idle
  // 15 - Highest
  CODECAPI_AVPriorityControl :  TGUID = '{54ba3dc8-bdde-4329-b187-2018bc5c2ba1}';
  {$EXTERNALSYM CODECAPI_AVPriorityControl}

  // AVRealtimeControl (UINT32)
  // Indicates the task is realtime or not
  // Linear range 0-1, where:
  //   0 - no realtime
  //   1 - realtime
  CODECAPI_AVRealtimeControl :  TGUID = '{6f440632-c4ad-4bf7-9e52-456942b454b0}';
  {$EXTERNALSYM CODECAPI_AVRealtimeControl}

  // AVEncNoInputCopy (UINT32)
  // Enables the encoder to avoid copying the input buffer
  // 0 - default behavior (copy input buffer to encoder internal buffer)
  // 1 - use input buffer directly
  // Input color space must be IYUV or YV12 for this to be effective.  Input buffers must be fully contiguous.  Input buffers
  // must be macroblock-aligned (width and height divisible by 16).
  CODECAPI_AVEncNoInputCopy :  TGUID = '{d2b46a2a-e8ee-4ec5-869e-449b6c62c81a}';
  {$EXTERNALSYM CODECAPI_AVEncNoInputCopy}

type
  // AVEncChromaEncodeMode (UINT32)
  // Change the mode used to encode chroma-only frames
  // A member of the eAVChromaEncodeMode structure, where:
  //   eAVChromaEncodeMode_420 - default encoding
  //   eAVChromaEncodeMode_444 - enhanced encoding of chroma for repeated input frames
  //   eAVChromaEncodeMode_444_v2 - encoder will skip non-chroma macroblocks, in addition to functionality for eAVChromaEncodeMode_444
  PeAVEncChromaEncodeMode = ^eAVEncChromaEncodeMode;
  eAVEncChromaEncodeMode = UINT32;
  {$EXTERNALSYM eAVEncChromaEncodeMode}
const
    eAVEncChromaEncodeMode_420    = UINT32(0);
    eAVEncChromaEncodeMode_444    = UINT32(1);
    eAVEncChromaEncodeMode_444_v2 = UINT32(2);

const

  AVEncChromaEncodeMode :  TGUID = '{8a47ab5a-4798-4c93-b5a5-554f9a3b9f50}';
  {$EXTERNALSYM AVEncChromaEncodeMode}

  // AVEncNextProgressiveTime (UINT32)
  // Read-only
  // Provides the time until the encoder plans to update progressive
  // areas of the video frame.
  AVEncProgressiveUpdateTime :  TGUID = '{649faf66-afc6-4828-8fdc-0771cd9ab17d}';

  // AVEncNextChromaTime (UINT32)
  // Read-only
  // Provides the time until the encoder plans to update chroma
  // in the video frame.
  AVEncChromaUpdateTime :  TGUID = '{4b4fd998-4274-40bb-8ee4-07553e7e2d3a}';

  // AVScenarioInfo (UINT32)
  AVScenarioInfo :  TGUID = '{b28a6e64-3ff9-446a-8a4b-0d7a53413236}';
  {$EXTERNALSYM AVScenarioInfo}

  // AVEncMPVGOPSizeMin (UINT32)
  AVEncMPVGOPSizeMin :  TGUID = '{7155cf20-d440-4852-ad0f-9c4abfe37a6a}';
  {$EXTERNALSYM AVEncMPVGOPSizeMin}

  //AVEncMPVGOPSizeMax (UINT32)
  AVEncMPVGOPSizeMax :  TGUID = '{fe7de4c4-1936-4fe2-bdf7-1f18ca1d001f}';
  {$EXTERNALSYM AVEncMPVGOPSizeMax}

  // AVEncVideoMaxCTBSize (UINT32)
  AVEncVideoMaxCTBSize :  TGUID = '{822363ff-cec8-43e5-92fd-e097488485e9}';
  {$EXTERNALSYM AVEncVideoMaxCTBSize}

  // AVEncVideoCTBSize (UINT32)
  AVEncVideoCTBSize :  TGUID = '{d47db8b2-e73b-4cb9-8c3e-bd877d06d77b}';
  {$EXTERNALSYM AVEncVideoCTBSize}

  // VideoEncoderDisplayContentType (UINT32)
  VideoEncoderDisplayContentType :  TGUID = '{79b90b27-f4b1-42dc-9dd7-cdaf8135c400}';
  {$EXTERNALSYM VideoEncoderDisplayContentType}

  // AVEncEnableVideoProcessing (UINT32)
  AVEncEnableVideoProcessing :  TGUID = '{006f4bf6-0ea3-4d42-8702-b5d8be0f7a92}';
  {$EXTERNALSYM AVEncEnableVideoProcessing}

  // AVEncVideoGradualIntraRefresh (UINT32)
  AVEncVideoGradualIntraRefresh :  TGUID = '{8f347dee-cb0d-49ba-b462-db6927ee2101}';
  {$EXTERNALSYM AVEncVideoGradualIntraRefresh}

  // GetOPMContext (UINT64)
  GetOPMContext :  TGUID = '{2f036c05-4c14-4689-8839-294c6d73e053}';
  {$EXTERNALSYM GetOPMContext}

  // SetHDCPManagerContext (VOID)
  SetHDCPManagerContext :  TGUID = '{6d2d1fc8-3dc9-47eb-a1a2-471c80cd60d0}';
  {$EXTERNALSYM SetHDCPManagerContext}

  // AVEncVideoMaxTemporalLayers (UINT32)
  AVEncVideoMaxTemporalLayers :  TGUID = '{9c668cfe-08e1-424a-934e-b764b064802a}';
  {$EXTERNALSYM AVEncVideoMaxTemporalLayers}

  // AVEncVideoNumGOPsPerIDR (UINT32)
  AVEncVideoNumGOPsPerIDR :  TGUID = '{83bc5bdb-5b89-4521-8f66-33151c373176}';
  {$EXTERNALSYM AVEncVideoNumGOPsPerIDR}

  // AVEncCommonAllowFrameDrops (UINT32)
  AVEncCommonAllowFrameDrops :  TGUID = '{d8477dcb-9598-48e3-8d0c-752bf206093e}';
  {$EXTERNALSYM AVEncCommonAllowFrameDrops}

  // AVEncVideoIntraLayerPrediction (UINT32)
  AVEncVideoIntraLayerPrediction :  TGUID = '{d3af46b8-bf47-44bb-a283-69f0b0228ff9}';
  {$EXTERNALSYM AVEncVideoIntraLayerPrediction}

  // AVEncVideoInstantTemporalUpSwitching (UINT32)
  AVEncVideoInstantTemporalUpSwitching :  TGUID = '{a3308307-0d96-4ba4-b1f0-b91a5e49df10}';
  {$EXTERNALSYM AVEncVideoInstantTemporalUpSwitching}

  // AVEncLowPowerEncoder (UINT32)
  AVEncLowPowerEncoder :  TGUID = '{b668d582-8bad-4f6a-9141-375a95358b6d}';
  {$EXTERNALSYM AVEncLowPowerEncoder}

  // AVEnableInLoopDeblockFilter (UINT32)
  AVEnableInLoopDeblockFilter :  TGUID = '{d2e8e399-0623-4bf3-92a8-4d1818529ded}';
  {$EXTERNALSYM AVEnableInLoopDeblockFilter}


type
  PeAVEncCommonStreamEndHandling = ^eAVEncCommonStreamEndHandling;
  eAVEncCommonStreamEndHandling = UINT32;
  {$EXTERNALSYM eAVEncCommonStreamEndHandling}
const
  eAVEncCommonStreamEndHandling_DiscardPartial = UINT32(0);
  {$EXTERNALSYM eAVEncCommonStreamEndHandling_DiscardPartial}
  eAVEncCommonStreamEndHandling_EnsureComplete = UINT32(1);
  {$EXTERNALSYM eAVEncCommonStreamEndHandling_EnsureComplete}


type
  PeAVEncVideoOutputFrameRateConversion = ^eAVEncVideoOutputFrameRateConversion;
  eAVEncVideoOutputFrameRateConversion = UINT32;
  {$EXTERNALSYM eAVEncVideoOutputFrameRateConversion}
const
  eAVEncVideoOutputFrameRateConversion_Disable = eAVEncVideoOutputFrameRateConversion(0);
  {$EXTERNALSYM eAVEncVideoOutputFrameRateConversion_Disable}
  eAVEncVideoOutputFrameRateConversion_Enable  = eAVEncVideoOutputFrameRateConversion(1);
  {$EXTERNALSYM eAVEncVideoOutputFrameRateConversion_Enable}
  eAVEncVideoOutputFrameRateConversion_Alias   = eAVEncVideoOutputFrameRateConversion(2);
  {$EXTERNALSYM eAVEncVideoOutputFrameRateConversion_Alias}


type
  PeAVDecVideoSoftwareDeinterlaceMode = ^eAVDecVideoSoftwareDeinterlaceMode;
  eAVDecVideoSoftwareDeinterlaceMode = UINT32;
  {$EXTERNALSYM eAVDecVideoSoftwareDeinterlaceMode}
const
  eAVDecVideoSoftwareDeinterlaceMode_NoDeinterlacing          = eAVDecVideoSoftwareDeinterlaceMode(0); // do not use software deinterlace
  {$EXTERNALSYM eAVDecVideoSoftwareDeinterlaceMode_NoDeinterlacing}
  eAVDecVideoSoftwareDeinterlaceMode_ProgressiveDeinterlacing = eAVDecVideoSoftwareDeinterlaceMode(1); // Use progressive deinterlace
  {$EXTERNALSYM eAVDecVideoSoftwareDeinterlaceMode_ProgressiveDeinterlacing}
  eAVDecVideoSoftwareDeinterlaceMode_BOBDeinterlacing         = eAVDecVideoSoftwareDeinterlaceMode(2); // BOB deinterlacing
  {$EXTERNALSYM eAVDecVideoSoftwareDeinterlaceMode_BOBDeinterlacing}
  eAVDecVideoSoftwareDeinterlaceMode_SmartBOBDeinterlacing    = eAVDecVideoSoftwareDeinterlaceMode(3); // Smart BOB deinterlacing
  {$EXTERNALSYM eAVDecVideoSoftwareDeinterlaceMode_SmartBOBDeinterlacing}


type
  PeAVFastDecodeMode = ^eAVFastDecodeMode;
  eAVFastDecodeMode = UINT32;
  {$EXTERNALSYM eAVFastDecodeMode}
const
  eVideoDecodeCompliant  = eAVFastDecodeMode(0);
  {$EXTERNALSYM eVideoDecodeCompliant}
  eVideoDecodeOptimalLF  = eAVFastDecodeMode(1); // Optimal Loop Filter
  {$EXTERNALSYM eVideoDecodeOptimalLF}
  eVideoDecodeDisableLF  = eAVFastDecodeMode(2); // Disable Loop Filter
  {$EXTERNALSYM eVideoDecodeDisableLF}
  eVideoDecodeFastest    = eAVFastDecodeMode(32);
  {$EXTERNALSYM eVideoDecodeFastest}


type
  PeAVDecVideoH264ErrorConcealment = ^eAVDecVideoH264ErrorConcealment;
  eAVDecVideoH264ErrorConcealment = UINT32;
  {$EXTERNALSYM eAVDecVideoH264ErrorConcealment}
const
  eErrorConcealmentTypeDrop          = eAVDecVideoH264ErrorConcealment(0);  // ERR_CONCEALMENT_TYPE_DROP
  {$EXTERNALSYM eErrorConcealmentTypeDrop}
  eErrorConcealmentTypeBasic         = eAVDecVideoH264ErrorConcealment(1);  // ERR_CONCEALMENT_TYPE_BASIC  (the default, and good mode used most of the time)
  {$EXTERNALSYM eErrorConcealmentTypeBasic}
  eErrorConcealmentTypeAdvanced      = eAVDecVideoH264ErrorConcealment(2);  // ERR_CONCEALMENT_TYPE_ADVANCED
  {$EXTERNALSYM eErrorConcealmentTypeAdvanced}
  eErrorConcealmentTypeDXVASetBlack  = eAVDecVideoH264ErrorConcealment(3);  // ERR_CONCEALMENT_TYPE_DXVA_SET_BLACK
  {$EXTERNALSYM eErrorConcealmentTypeDXVASetBlack}


type
  PeAVDecVideoMPEG2ErrorConcealment = ^eAVDecVideoMPEG2ErrorConcealment;
  eAVDecVideoMPEG2ErrorConcealment = UINT32;
  {$EXTERNALSYM eAVDecVideoMPEG2ErrorConcealment}
const
  eErrorConcealmentOff          = eAVDecVideoMPEG2ErrorConcealment(0);   //
  {$EXTERNALSYM eErrorConcealmentOff}
  eErrorConcealmentOn           = eAVDecVideoMPEG2ErrorConcealment(1);   //  the default and good mode used most of the time
  {$EXTERNALSYM eErrorConcealmentOn}


type
  PeAVDecVideoCodecType = ^eAVDecVideoCodecType;
  eAVDecVideoCodecType = UINT32;
  {$EXTERNALSYM eAVDecVideoCodecType}
const
  eAVDecVideoCodecType_NOTPLAYING   = eAVDecVideoCodecType(0);
  {$EXTERNALSYM eAVDecVideoCodecType_NOTPLAYING}
  eAVDecVideoCodecType_MPEG2        = eAVDecVideoCodecType(1);
  {$EXTERNALSYM eAVDecVideoCodecType_MPEG2}
  eAVDecVideoCodecType_H264         = eAVDecVideoCodecType(2);
  {$EXTERNALSYM eAVDecVideoCodecType_H264}


type
  PeAVDecVideoDXVAMode = ^eAVDecVideoDXVAMode;
  eAVDecVideoDXVAMode = UINT32;
  {$EXTERNALSYM eAVDecVideoDXVAMode}
const
  eAVDecVideoDXVAMode_NOTPLAYING = eAVDecVideoDXVAMode(0);
  {$EXTERNALSYM eAVDecVideoDXVAMode_NOTPLAYING}
  eAVDecVideoDXVAMode_SW     = eAVDecVideoDXVAMode(1);
  {$EXTERNALSYM eAVDecVideoDXVAMode_SW}
  eAVDecVideoDXVAMode_MC     = eAVDecVideoDXVAMode(2);
  {$EXTERNALSYM eAVDecVideoDXVAMode_MC}
  eAVDecVideoDXVAMode_IDCT   = eAVDecVideoDXVAMode(3);
  {$EXTERNALSYM eAVDecVideoDXVAMode_IDCT}
  eAVDecVideoDXVAMode_VLD    = eAVDecVideoDXVAMode(4);
  {$EXTERNALSYM eAVDecVideoDXVAMode_VLD}


type
  PeAVDecVideoDXVABusEncryption = ^eAVDecVideoDXVABusEncryption;
  eAVDecVideoDXVABusEncryption = UINT32;
  {$EXTERNALSYM eAVDecVideoDXVABusEncryption}
const
  eAVDecVideoDXVABusEncryption_NONE     = eAVDecVideoDXVABusEncryption(0);
  {$EXTERNALSYM eAVDecVideoDXVABusEncryption_NONE}
  eAVDecVideoDXVABusEncryption_PRIVATE  = eAVDecVideoDXVABusEncryption(1);
  {$EXTERNALSYM eAVDecVideoDXVABusEncryption_PRIVATE}
  eAVDecVideoDXVABusEncryption_AES      = eAVDecVideoDXVABusEncryption(2);
  {$EXTERNALSYM eAVDecVideoDXVABusEncryption_AES}


type
  PeAVEncVideoSourceScanType = ^eAVEncVideoSourceScanType;
  eAVEncVideoSourceScanType = UINT32;
  {$EXTERNALSYM eAVEncVideoSourceScanType}
const
  eAVEncVideoSourceScan_Automatic         = eAVEncVideoSourceScanType(0);
  {$EXTERNALSYM eAVEncVideoSourceScan_Automatic}
  eAVEncVideoSourceScan_Interlaced        = eAVEncVideoSourceScanType(1);
  {$EXTERNALSYM eAVEncVideoSourceScan_Interlaced}
  eAVEncVideoSourceScan_Progressive       = eAVEncVideoSourceScanType(2);
  {$EXTERNALSYM eAVEncVideoSourceScan_Progressive}


type
  PeAVEncVideoOutputScanType = ^eAVEncVideoOutputScanType;
  eAVEncVideoOutputScanType = UINT32;
  {$EXTERNALSYM eAVEncVideoOutputScanType}
const
  eAVEncVideoOutputScan_Progressive       = eAVEncVideoOutputScanType(0);
  {$EXTERNALSYM eAVEncVideoOutputScan_Progressive}
  eAVEncVideoOutputScan_Interlaced        = eAVEncVideoOutputScanType(1);
  {$EXTERNALSYM eAVEncVideoOutputScan_Interlaced}
  eAVEncVideoOutputScan_SameAsInput       = eAVEncVideoOutputScanType(2);
  {$EXTERNALSYM eAVEncVideoOutputScan_SameAsInput}
  eAVEncVideoOutputScan_Automatic         = eAVEncVideoOutputScanType(3);
  {$EXTERNALSYM eAVEncVideoOutputScan_Automatic}


type
  PeAVEncVideoFilmContent = ^eAVEncVideoFilmContent;
  eAVEncVideoFilmContent = UINT32;
  {$EXTERNALSYM eAVEncVideoFilmContent}
const
  eAVEncVideoFilmContent_VideoOnly = eAVEncVideoFilmContent(0);
  {$EXTERNALSYM eAVEncVideoFilmContent_VideoOnly}
  eAVEncVideoFilmContent_FilmOnly  = eAVEncVideoFilmContent(1);
  {$EXTERNALSYM eAVEncVideoFilmContent_FilmOnly}
  eAVEncVideoFilmContent_Mixed     = eAVEncVideoFilmContent(2);
  {$EXTERNALSYM eAVEncVideoFilmContent_Mixed}


type
  PeAVEncVideoChromaResolution = ^eAVEncVideoChromaResolution;
  eAVEncVideoChromaResolution = UINT32;
  {$EXTERNALSYM eAVEncVideoChromaResolution}
const
  eAVEncVideoChromaResolution_SameAsSource = eAVEncVideoChromaResolution(0);
  {$EXTERNALSYM eAVEncVideoChromaResolution_SameAsSource}
  eAVEncVideoChromaResolution_444          = eAVEncVideoChromaResolution(1);
  {$EXTERNALSYM eAVEncVideoChromaResolution_444}
  eAVEncVideoChromaResolution_422          = eAVEncVideoChromaResolution(2);
  {$EXTERNALSYM eAVEncVideoChromaResolution_422}
  eAVEncVideoChromaResolution_420          = eAVEncVideoChromaResolution(3);
  {$EXTERNALSYM eAVEncVideoChromaResolution_420}
  eAVEncVideoChromaResolution_411          = eAVEncVideoChromaResolution(4);
  {$EXTERNALSYM eAVEncVideoChromaResolution_411}


type
  PeAVEncVideoChromaSubsampling = ^eAVEncVideoChromaSubsampling;
  eAVEncVideoChromaSubsampling = UINT32;
  {$EXTERNALSYM eAVEncVideoChromaSubsampling}
const
  eAVEncVideoChromaSubsamplingFormat_SameAsSource                   = eAVEncVideoChromaSubsampling($0);
  {$EXTERNALSYM eAVEncVideoChromaSubsamplingFormat_SameAsSource}
  eAVEncVideoChromaSubsamplingFormat_ProgressiveChroma              = eAVEncVideoChromaSubsampling($8);
  {$EXTERNALSYM eAVEncVideoChromaSubsamplingFormat_ProgressiveChroma}
  eAVEncVideoChromaSubsamplingFormat_Horizontally_Cosited           = eAVEncVideoChromaSubsampling($4);
  {$EXTERNALSYM eAVEncVideoChromaSubsamplingFormat_Horizontally_Cosited}
  eAVEncVideoChromaSubsamplingFormat_Vertically_Cosited             = eAVEncVideoChromaSubsampling($2);
  {$EXTERNALSYM eAVEncVideoChromaSubsamplingFormat_Vertically_Cosited}
  eAVEncVideoChromaSubsamplingFormat_Vertically_AlignedChromaPlanes = eAVEncVideoChromaSubsampling($1);
  {$EXTERNALSYM eAVEncVideoChromaSubsamplingFormat_Vertically_AlignedChromaPlanes}


type
  PeAVEncVideoColorPrimaries = ^eAVEncVideoColorPrimaries;
  eAVEncVideoColorPrimaries = UINT32;
  {$EXTERNALSYM eAVEncVideoColorPrimaries}
const
  eAVEncVideoColorPrimaries_SameAsSource  = eAVEncVideoColorPrimaries(0);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_SameAsSource}
  eAVEncVideoColorPrimaries_Reserved      = eAVEncVideoColorPrimaries(1);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_Reserved}
  eAVEncVideoColorPrimaries_BT709         = eAVEncVideoColorPrimaries(2);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_BT709}
  eAVEncVideoColorPrimaries_BT470_2_SysM  = eAVEncVideoColorPrimaries(3);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_BT470_2_SysM}
  eAVEncVideoColorPrimaries_BT470_2_SysBG = eAVEncVideoColorPrimaries(4);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_BT470_2_SysBG}
  eAVEncVideoColorPrimaries_SMPTE170M     = eAVEncVideoColorPrimaries(5);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_SMPTE170M}
  eAVEncVideoColorPrimaries_SMPTE240M     = eAVEncVideoColorPrimaries(6);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_SMPTE240M}
  eAVEncVideoColorPrimaries_EBU3231       = eAVEncVideoColorPrimaries(7);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_EBU3231}
  eAVEncVideoColorPrimaries_SMPTE_C       = eAVEncVideoColorPrimaries(8);
  {$EXTERNALSYM eAVEncVideoColorPrimaries_SMPTE_C}


type
  PeAVEncVideoColorTransferFunction = ^eAVEncVideoColorTransferFunction;
  eAVEncVideoColorTransferFunction = UINT32;
  {$EXTERNALSYM eAVEncVideoColorTransferFunction}
const
  eAVEncVideoColorTransferFunction_SameAsSource = eAVEncVideoColorTransferFunction(0);
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_SameAsSource}
  eAVEncVideoColorTransferFunction_10           = eAVEncVideoColorTransferFunction(1);  // (Linear); scRGB)
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_10}
  eAVEncVideoColorTransferFunction_18           = eAVEncVideoColorTransferFunction(2);
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_18}
  eAVEncVideoColorTransferFunction_20           = eAVEncVideoColorTransferFunction(3);
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_20}
  eAVEncVideoColorTransferFunction_22           = eAVEncVideoColorTransferFunction(4);  // (BT470-2 SysM)
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_22}
  eAVEncVideoColorTransferFunction_22_709       = eAVEncVideoColorTransferFunction(5);  // (BT709);  SMPTE296M); SMPTE170M); BT470); SMPTE274M); BT.1361)
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_22_709}
  eAVEncVideoColorTransferFunction_22_240M      = eAVEncVideoColorTransferFunction(6);  // (SMPTE240M); interim 274M)
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_22_240M}
  eAVEncVideoColorTransferFunction_22_8bit_sRGB = eAVEncVideoColorTransferFunction(7);  // (sRGB)
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_22_8bit_sRGB}
  eAVEncVideoColorTransferFunction_28           = eAVEncVideoColorTransferFunction(8);
  {$EXTERNALSYM eAVEncVideoColorTransferFunction_28}


type
  PeAVEncVideoColorTransferMatrix = ^eAVEncVideoColorTransferMatrix;
  eAVEncVideoColorTransferMatrix = UINT32;
  {$EXTERNALSYM eAVEncVideoColorTransferMatrix}
const
  eAVEncVideoColorTransferMatrix_SameAsSource = eAVEncVideoColorTransferMatrix(0);
  {$EXTERNALSYM eAVEncVideoColorTransferMatrix_SameAsSource}
  eAVEncVideoColorTransferMatrix_BT709        = eAVEncVideoColorTransferMatrix(1);
  {$EXTERNALSYM eAVEncVideoColorTransferMatrix_BT709}
  eAVEncVideoColorTransferMatrix_BT601        = eAVEncVideoColorTransferMatrix(2);  // (601, BT470-2 B,B, 170M)
  {$EXTERNALSYM eAVEncVideoColorTransferMatrix_BT601}
  eAVEncVideoColorTransferMatrix_SMPTE240M    = eAVEncVideoColorTransferMatrix(3);
  {$EXTERNALSYM eAVEncVideoColorTransferMatrix_SMPTE240M}


type
  PeAVEncVideoColorLighting = ^eAVEncVideoColorLighting;
  eAVEncVideoColorLighting = UINT32;
  {$EXTERNALSYM eAVEncVideoColorLighting}
const
  eAVEncVideoColorLighting_SameAsSource = eAVEncVideoColorLighting(0);
  {$EXTERNALSYM eAVEncVideoColorLighting_SameAsSource}
  eAVEncVideoColorLighting_Unknown      = eAVEncVideoColorLighting(1);
  {$EXTERNALSYM eAVEncVideoColorLighting_Unknown}
  eAVEncVideoColorLighting_Bright       = eAVEncVideoColorLighting(2);
  {$EXTERNALSYM eAVEncVideoColorLighting_Bright}
  eAVEncVideoColorLighting_Office       = eAVEncVideoColorLighting(3);
  {$EXTERNALSYM eAVEncVideoColorLighting_Office}
  eAVEncVideoColorLighting_Dim          = eAVEncVideoColorLighting(4);
  {$EXTERNALSYM eAVEncVideoColorLighting_Dim}
  eAVEncVideoColorLighting_Dark         = eAVEncVideoColorLighting(5);
  {$EXTERNALSYM eAVEncVideoColorLighting_Dark}


type
  PeAVEncVideoColorNominalRange = ^eAVEncVideoColorNominalRange;
  eAVEncVideoColorNominalRange = UINT32;
  {$EXTERNALSYM eAVEncVideoColorNominalRange}
const
  eAVEncVideoColorNominalRange_SameAsSource = eAVEncVideoColorNominalRange(0);
  {$EXTERNALSYM eAVEncVideoColorNominalRange_SameAsSource}
  eAVEncVideoColorNominalRange_0_255        = eAVEncVideoColorNominalRange(1); // (8 bit: 0..255, 10 bit: 0..1023)
  {$EXTERNALSYM eAVEncVideoColorNominalRange_0_255}
  eAVEncVideoColorNominalRange_16_235       = eAVEncVideoColorNominalRange(2); // (16..235, 64..940 (16*4...235*4)
  {$EXTERNALSYM eAVEncVideoColorNominalRange_16_235}
  eAVEncVideoColorNominalRange_48_208       = eAVEncVideoColorNominalRange(3); // (48..208)
  {$EXTERNALSYM eAVEncVideoColorNominalRange_48_208}


type
  PeAVEncInputVideoSystem = ^eAVEncInputVideoSystem;
  eAVEncInputVideoSystem = UINT32;
  {$EXTERNALSYM eAVEncInputVideoSystem}
const
  eAVEncInputVideoSystem_Unspecified = eAVEncInputVideoSystem(0);
  {$EXTERNALSYM eAVEncInputVideoSystem_Unspecified}
  eAVEncInputVideoSystem_PAL         = eAVEncInputVideoSystem(1);
  {$EXTERNALSYM eAVEncInputVideoSystem_PAL}
  eAVEncInputVideoSystem_NTSC        = eAVEncInputVideoSystem(2);
  {$EXTERNALSYM eAVEncInputVideoSystem_NTSC}
  eAVEncInputVideoSystem_SECAM       = eAVEncInputVideoSystem(3);
  {$EXTERNALSYM eAVEncInputVideoSystem_SECAM}
  eAVEncInputVideoSystem_MAC         = eAVEncInputVideoSystem(4);
  {$EXTERNALSYM eAVEncInputVideoSystem_MAC}
  eAVEncInputVideoSystem_HDV         = eAVEncInputVideoSystem(5);
  {$EXTERNALSYM eAVEncInputVideoSystem_HDV}
  eAVEncInputVideoSystem_Component   = eAVEncInputVideoSystem(6);
  {$EXTERNALSYM eAVEncInputVideoSystem_Component}


type
  PeAVEncVideoContentType = ^eAVEncVideoContentType;
  eAVEncVideoContentType = UINT32;
  {$EXTERNALSYM eAVEncVideoContentType}
const
  eAVEncVideoContentType_Unknown          = eAVEncVideoContentType(0);
  {$EXTERNALSYM eAVEncVideoContentType_Unknown}
  eAVEncVideoContentType_FixedCameraAngle = eAVEncVideoContentType(1);
  {$EXTERNALSYM eAVEncVideoContentType_FixedCameraAngle}


type
  PeAVEncAdaptiveMode = ^eAVEncAdaptiveMode;
  eAVEncAdaptiveMode = UINT32;
  {$EXTERNALSYM eAVEncAdaptiveMode}
const
  eAVEncAdaptiveMode_None         = eAVEncAdaptiveMode(0);
  {$EXTERNALSYM eAVEncAdaptiveMode_None}
  eAVEncAdaptiveMode_Resolution   = eAVEncAdaptiveMode(1);
  {$EXTERNALSYM eAVEncAdaptiveMode_Resolution}
  eAVEncAdaptiveMode_FrameRate    = eAVEncAdaptiveMode(2);
  {$EXTERNALSYM eAVEncAdaptiveMode_FrameRate}


type
  PeAVScenarioInfo = ^eAVScenarioInfo;
  eAVScenarioInfo = UINT32;
  {$EXTERNALSYM eAVScenarioInfo}
const
  eAVScenarioInfo_Unknown                       = eAVScenarioInfo(0);
  {$EXTERNALSYM eAVScenarioInfo_Unknown}
  eAVScenarioInfo_DisplayRemoting               = eAVScenarioInfo(1);
  {$EXTERNALSYM eAVScenarioInfo_DisplayRemoting}
  eAVScenarioInfo_VideoConference               = eAVScenarioInfo(2);
  {$EXTERNALSYM eAVScenarioInfo_VideoConference}
  eAVScenarioInfo_Archive                       = eAVScenarioInfo(3);
  {$EXTERNALSYM eAVScenarioInfo_Archive}
  eAVScenarioInfo_LiveStreaming                 = eAVScenarioInfo(4);
  {$EXTERNALSYM eAVScenarioInfo_LiveStreaming}
  eAVScenarioInfo_CameraRecord                  = eAVScenarioInfo(5);
  {$EXTERNALSYM eAVScenarioInfo_CameraRecord}
  eAVScenarioInfo_DisplayRemotingWithFeatureMap = eAVScenarioInfo(6);
  {$EXTERNALSYM eAVScenarioInfo_DisplayRemotingWithFeatureMap}


type
  PeVideoEncoderDisplayContentType = ^eVideoEncoderDisplayContentType;
  eVideoEncoderDisplayContentType = UINT32;
  {$EXTERNALSYM eVideoEncoderDisplayContentType}
const
  eVideoEncoderDisplayContent_Unknown         = eVideoEncoderDisplayContentType(0);
  {$EXTERNALSYM eVideoEncoderDisplayContent_Unknown}
  eVideoEncoderDisplayContent_FullScreenVideo = eVideoEncoderDisplayContentType(1);
  {$EXTERNALSYM eVideoEncoderDisplayContent_FullScreenVideo}



  //
  // Audio/Video Mux
  //

type
  PeAVEncMuxOutput = ^eAVEncMuxOutput;
  eAVEncMuxOutput = UINT32;
  {$EXTERNALSYM eAVEncMuxOutput}
const
  eAVEncMuxOutputAuto = eAVEncMuxOutput(0);    // Decision is made automatically be the mux (elementary stream, program stream or transport stream)
  {$EXTERNALSYM eAVEncMuxOutputAuto}
  eAVEncMuxOutputPS   = eAVEncMuxOutput(1);    // Program stream
  {$EXTERNALSYM eAVEncMuxOutputPS}
  eAVEncMuxOutputTS   = eAVEncMuxOutput(2);    // Transport stream
  {$EXTERNALSYM eAVEncMuxOutputTS}


type
  PeAVEncAudioDualMono = ^eAVEncAudioDualMono;
  eAVEncAudioDualMono = UINT32;
  {$EXTERNALSYM eAVEncAudioDualMono}
const
  eAVEncAudioDualMono_SameAsInput = eAVEncAudioDualMono(0);    // As indicated by input media type
  {$EXTERNALSYM eAVEncAudioDualMono_SameAsInput}
  eAVEncAudioDualMono_Off         = eAVEncAudioDualMono(1);    // 2-ch output bitstream should not be dual mono
  {$EXTERNALSYM eAVEncAudioDualMono_Off}
  eAVEncAudioDualMono_On          = eAVEncAudioDualMono(2);    // 2-ch output bitstream should be dual mono
  {$EXTERNALSYM eAVEncAudioDualMono_On}


type
  PeAVEncAudioInputContent = ^eAVEncAudioInputContent;
  eAVEncAudioInputContent = UINT32;
  {$EXTERNALSYM eAVEncAudioInputContent}
const
  AVEncAudioInputContent_Unknown = eAVEncAudioInputContent(0);
  {$EXTERNALSYM AVEncAudioInputContent_Unknown}
  AVEncAudioInputContent_Voice   = eAVEncAudioInputContent(1);
  {$EXTERNALSYM AVEncAudioInputContent_Voice}
  AVEncAudioInputContent_Music   = eAVEncAudioInputContent(2);
  {$EXTERNALSYM AVEncAudioInputContent_Music}


type
  PeAVEncMPVProfile = ^eAVEncMPVProfile;
  eAVEncMPVProfile = UINT32;
  {$EXTERNALSYM eAVEncMPVProfile}
const
  eAVEncMPVProfile_unknown = eAVEncMPVProfile(0);
  {$EXTERNALSYM eAVEncMPVProfile_unknown}
  eAVEncMPVProfile_Simple  = eAVEncMPVProfile(1);
  {$EXTERNALSYM eAVEncMPVProfile_Simple}
  eAVEncMPVProfile_Main    = eAVEncMPVProfile(2);
  {$EXTERNALSYM eAVEncMPVProfile_Main}
  eAVEncMPVProfile_High    = eAVEncMPVProfile(3);
  {$EXTERNALSYM eAVEncMPVProfile_High}
  eAVEncMPVProfile_422     = eAVEncMPVProfile(4);
  {$EXTERNALSYM eAVEncMPVProfile_422}


type
  PeAVEncMPVLevel = ^eAVEncMPVLevel;
  eAVEncMPVLevel = UINT32;
  {$EXTERNALSYM eAVEncMPVLevel}
const
  eAVEncMPVLevel_Low      = eAVEncMPVLevel(1);
  {$EXTERNALSYM eAVEncMPVLevel_Low}
  eAVEncMPVLevel_Main     = eAVEncMPVLevel(2);
  {$EXTERNALSYM eAVEncMPVLevel_Main}
  eAVEncMPVLevel_High1440 = eAVEncMPVLevel(3);
  {$EXTERNALSYM eAVEncMPVLevel_High1440}
  eAVEncMPVLevel_High     = eAVEncMPVLevel(4);
  {$EXTERNALSYM eAVEncMPVLevel_High}


type
  PeAVEncH263VProfile = ^eAVEncH263VProfile;
  eAVEncH263VProfile = UINT32;
  {$EXTERNALSYM eAVEncH263VProfile}
const
  eAVEncH263VProfile_Base            = eAVEncH263VProfile(0);
  {$EXTERNALSYM eAVEncH263VProfile_Base}
  eAVEncH263VProfile_CompatibilityV2 = eAVEncH263VProfile(1);
  {$EXTERNALSYM eAVEncH263VProfile_CompatibilityV2}
  eAVEncH263VProfile_CompatibilityV1 = eAVEncH263VProfile(2);
  {$EXTERNALSYM eAVEncH263VProfile_CompatibilityV1}
  eAVEncH263VProfile_WirelessV2      = eAVEncH263VProfile(3);
  {$EXTERNALSYM eAVEncH263VProfile_WirelessV2}
  eAVEncH263VProfile_WirelessV3      = eAVEncH263VProfile(4);
  {$EXTERNALSYM eAVEncH263VProfile_WirelessV3}
  eAVEncH263VProfile_HighCompression = eAVEncH263VProfile(5);
  {$EXTERNALSYM eAVEncH263VProfile_HighCompression}
  eAVEncH263VProfile_Internet        = eAVEncH263VProfile(6);
  {$EXTERNALSYM eAVEncH263VProfile_Internet}
  eAVEncH263VProfile_Interlace       = eAVEncH263VProfile(7);
  {$EXTERNALSYM eAVEncH263VProfile_Interlace}
  eAVEncH263VProfile_HighLatency     = eAVEncH263VProfile(8);
  {$EXTERNALSYM eAVEncH263VProfile_HighLatency}


type
  PeAVEncH264VProfile = ^eAVEncH264VProfile;
  eAVEncH264VProfile = UINT32;
  {$EXTERNALSYM eAVEncH264VProfile}
const
  eAVEncH264VProfile_unknown                   = eAVEncH264VProfile(0);
  {$EXTERNALSYM eAVEncH264VProfile_unknown}
  eAVEncH264VProfile_Simple                    = eAVEncH264VProfile(66);
  {$EXTERNALSYM eAVEncH264VProfile_Simple}
  eAVEncH264VProfile_Base                      = eAVEncH264VProfile(66);
  {$EXTERNALSYM eAVEncH264VProfile_Base}
  eAVEncH264VProfile_Main                      = eAVEncH264VProfile(77);
  {$EXTERNALSYM eAVEncH264VProfile_Main}
  eAVEncH264VProfile_High                      = eAVEncH264VProfile(100);
  {$EXTERNALSYM eAVEncH264VProfile_High}
  eAVEncH264VProfile_422                       = eAVEncH264VProfile(122);
  {$EXTERNALSYM eAVEncH264VProfile_422}
  eAVEncH264VProfile_High10                    = eAVEncH264VProfile(110);
  {$EXTERNALSYM eAVEncH264VProfile_High10}
  eAVEncH264VProfile_444                       = eAVEncH264VProfile(244);
  {$EXTERNALSYM eAVEncH264VProfile_444}
  eAVEncH264VProfile_Extended                  = eAVEncH264VProfile(88);
  {$EXTERNALSYM eAVEncH264VProfile_Extended}
  // UVC 1.2 H.264 extension
  eAVEncH264VProfile_ScalableBase              = eAVEncH264VProfile(83);
  {$EXTERNALSYM eAVEncH264VProfile_ScalableBase}
  eAVEncH264VProfile_ScalableHigh              = eAVEncH264VProfile(86);
  {$EXTERNALSYM eAVEncH264VProfile_ScalableHigh}
  eAVEncH264VProfile_MultiviewHigh             = eAVEncH264VProfile(118);
  {$EXTERNALSYM eAVEncH264VProfile_MultiviewHigh}
  eAVEncH264VProfile_StereoHigh                = eAVEncH264VProfile(128);
  {$EXTERNALSYM eAVEncH264VProfile_StereoHigh}
  eAVEncH264VProfile_ConstrainedBase           = eAVEncH264VProfile(256);
  {$EXTERNALSYM eAVEncH264VProfile_ConstrainedBase}
  eAVEncH264VProfile_UCConstrainedHigh         = eAVEncH264VProfile(257);
  {$EXTERNALSYM eAVEncH264VProfile_UCConstrainedHigh}
  eAVEncH264VProfile_UCScalableConstrainedBase = eAVEncH264VProfile(258);
  {$EXTERNALSYM eAVEncH264VProfile_UCScalableConstrainedBase}
  eAVEncH264VProfile_UCScalableConstrainedHigh = eAVEncH264VProfile(259);
  {$EXTERNALSYM eAVEncH264VProfile_UCScalableConstrainedHigh}

  eAVEncH264VProfile_ConstrainedHigh = eAVEncH264VProfile_UCConstrainedHigh;
  {$EXTERNALSYM eAVEncH264VProfile_ConstrainedHigh}


type
  PeAVEncH265VProfile = ^eAVEncH265VProfile;
  eAVEncH265VProfile = UINT32;
  {$EXTERNALSYM eAVEncH265VProfile}
const
  eAVEncH265VProfile_unknown           = eAVEncH265VProfile(0);
  {$EXTERNALSYM eAVEncH265VProfile_unknown}
  eAVEncH265VProfile_Main_420_8        = eAVEncH265VProfile(1);
  {$EXTERNALSYM eAVEncH265VProfile_Main_420_8}
  eAVEncH265VProfile_Main_420_10       = eAVEncH265VProfile(2);
  {$EXTERNALSYM eAVEncH265VProfile_Main_420_10}
  eAVEncH265VProfile_Main_420_12       = eAVEncH265VProfile(3);
  {$EXTERNALSYM eAVEncH265VProfile_Main_420_12}
  eAVEncH265VProfile_Main_422_10       = eAVEncH265VProfile(4);
  {$EXTERNALSYM eAVEncH265VProfile_Main_422_10}
  eAVEncH265VProfile_Main_422_12       = eAVEncH265VProfile(5);
  {$EXTERNALSYM eAVEncH265VProfile_Main_422_12}
  eAVEncH265VProfile_Main_444_8        = eAVEncH265VProfile(6);
  {$EXTERNALSYM eAVEncH265VProfile_Main_444_8}
  eAVEncH265VProfile_Main_444_10       = eAVEncH265VProfile(7);
  {$EXTERNALSYM eAVEncH265VProfile_Main_444_10}
  eAVEncH265VProfile_Main_444_12       = eAVEncH265VProfile(8);
  {$EXTERNALSYM eAVEncH265VProfile_Main_444_12}
  eAVEncH265VProfile_Monochrome_12     = eAVEncH265VProfile(9);
  {$EXTERNALSYM eAVEncH265VProfile_Monochrome_12}
  eAVEncH265VProfile_Monochrome_16     = eAVEncH265VProfile(10);
  {$EXTERNALSYM eAVEncH265VProfile_Monochrome_16}
  eAVEncH265VProfile_MainIntra_420_8   = eAVEncH265VProfile(11);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_420_8}
  eAVEncH265VProfile_MainIntra_420_10  = eAVEncH265VProfile(12);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_420_10}
  eAVEncH265VProfile_MainIntra_420_12  = eAVEncH265VProfile(13);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_420_12}
  eAVEncH265VProfile_MainIntra_422_10  = eAVEncH265VProfile(14);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_422_10}
  eAVEncH265VProfile_MainIntra_422_12  = eAVEncH265VProfile(15);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_422_12}
  eAVEncH265VProfile_MainIntra_444_8   = eAVEncH265VProfile(16);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_444_8}
  eAVEncH265VProfile_MainIntra_444_10  = eAVEncH265VProfile(17);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_444_10}
  eAVEncH265VProfile_MainIntra_444_12  = eAVEncH265VProfile(18);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_444_12}
  eAVEncH265VProfile_MainIntra_444_16  = eAVEncH265VProfile(19);
  {$EXTERNALSYM eAVEncH265VProfile_MainIntra_444_16}
  eAVEncH265VProfile_MainStill_420_8   = eAVEncH265VProfile(20);
  {$EXTERNALSYM eAVEncH265VProfile_MainStill_420_8}
  eAVEncH265VProfile_MainStill_444_8   = eAVEncH265VProfile(21);
  {$EXTERNALSYM eAVEncH265VProfile_MainStill_444_8}
  eAVEncH265VProfile_MainStill_444_16  = eAVEncH265VProfile(22);
  {$EXTERNALSYM eAVEncH265VProfile_MainStill_444_16}


type
  PeAVEncVP9VProfile = ^eAVEncVP9VProfile;
  eAVEncVP9VProfile = UINT32;
  {$EXTERNALSYM eAVEncVP9VProfile}
const
  eAVEncVP9VProfile_unknown  = eAVEncVP9VProfile(0);
  {$EXTERNALSYM eAVEncVP9VProfile_unknown}
  eAVEncVP9VProfile_420_8    = eAVEncVP9VProfile(1);
  {$EXTERNALSYM eAVEncVP9VProfile_420_8}
  eAVEncVP9VProfile_420_10   = eAVEncVP9VProfile(2);
  {$EXTERNALSYM eAVEncVP9VProfile_420_10}
  eAVEncVP9VProfile_420_12   = eAVEncVP9VProfile(3);
  {$EXTERNALSYM eAVEncVP9VProfile_420_12}


type
  PeAVEncH263PictureType = ^eAVEncH263PictureType;
  eAVEncH263PictureType = UINT32;
  {$EXTERNALSYM eAVEncH263PictureType}
const
  eAVEncH263PictureType_I = eAVEncH263PictureType(0);
  {$EXTERNALSYM eAVEncH263PictureType_I}
  eAVEncH263PictureType_P = eAVEncH263PictureType(1);
  {$EXTERNALSYM eAVEncH263PictureType_P}
  eAVEncH263PictureType_B = eAVEncH263PictureType(2);
  {$EXTERNALSYM eAVEncH263PictureType_B}


type
  PeAVEncH264PictureType = ^eAVEncH264PictureType;
  eAVEncH264PictureType = UINT32;
  {$EXTERNALSYM eAVEncH264PictureType}
const
  eAVEncH264PictureType_IDR = eAVEncH264PictureType(0);
  {$EXTERNALSYM eAVEncH264PictureType_IDR}
  eAVEncH264PictureType_P   = eAVEncH264PictureType(1);
  {$EXTERNALSYM eAVEncH264PictureType_P}
  eAVEncH264PictureType_B   = eAVEncH264PictureType(2);
  {$EXTERNALSYM eAVEncH264PictureType_B}


type
  PeAVEncH264VLevel = ^eAVEncH264VLevel;
  eAVEncH264VLevel = UINT32;
  {$EXTERNALSYM eAVEncH264VLevel}
const
  eAVEncH264VLevel1         = eAVEncH264VLevel(10);
  {$EXTERNALSYM eAVEncH264VLevel1}
  eAVEncH264VLevel1_b       = eAVEncH264VLevel(11);
  {$EXTERNALSYM eAVEncH264VLevel1_b}
  eAVEncH264VLevel1_1       = eAVEncH264VLevel(11);
  {$EXTERNALSYM eAVEncH264VLevel1_1}
  eAVEncH264VLevel1_2       = eAVEncH264VLevel(12);
  {$EXTERNALSYM eAVEncH264VLevel1_2}
  eAVEncH264VLevel1_3       = eAVEncH264VLevel(13);
  {$EXTERNALSYM eAVEncH264VLevel1_3}
  eAVEncH264VLevel2         = eAVEncH264VLevel(20);
  {$EXTERNALSYM eAVEncH264VLevel2}
  eAVEncH264VLevel2_1       = eAVEncH264VLevel(21);
  {$EXTERNALSYM eAVEncH264VLevel2_1}
  eAVEncH264VLevel2_2       = eAVEncH264VLevel(22);
  {$EXTERNALSYM eAVEncH264VLevel2_2}
  eAVEncH264VLevel3         = eAVEncH264VLevel(30);
  {$EXTERNALSYM eAVEncH264VLevel3}
  eAVEncH264VLevel3_1       = eAVEncH264VLevel(31);
  {$EXTERNALSYM eAVEncH264VLevel3_1}
  eAVEncH264VLevel3_2       = eAVEncH264VLevel(32);
  {$EXTERNALSYM eAVEncH264VLevel3_2}
  eAVEncH264VLevel4         = eAVEncH264VLevel(40);
  {$EXTERNALSYM eAVEncH264VLevel4}
  eAVEncH264VLevel4_1       = eAVEncH264VLevel(41);
  {$EXTERNALSYM eAVEncH264VLevel4_1}
  eAVEncH264VLevel4_2       = eAVEncH264VLevel(42);
  {$EXTERNALSYM eAVEncH264VLevel4_2}
  eAVEncH264VLevel5         = eAVEncH264VLevel(50);
  {$EXTERNALSYM eAVEncH264VLevel5}
  eAVEncH264VLevel5_1       = eAVEncH264VLevel(51);
  {$EXTERNALSYM eAVEncH264VLevel5_1}
  eAVEncH264VLevel5_2       = eAVEncH264VLevel(52);
  {$EXTERNALSYM eAVEncH264VLevel5_2}


type
  PeAVEncH265VLevel = ^eAVEncH265VLevel;
  eAVEncH265VLevel = UINT32;
  {$EXTERNALSYM eAVEncH265VLevel}
const
  eAVEncH265VLevel1        = eAVEncH265VLevel(30);
  {$EXTERNALSYM eAVEncH265VLevel1}
  eAVEncH265VLevel2        = eAVEncH265VLevel(60);
  {$EXTERNALSYM eAVEncH265VLevel2}
  eAVEncH265VLevel2_1      = eAVEncH265VLevel(63);
  {$EXTERNALSYM eAVEncH265VLevel2_1}
  eAVEncH265VLevel3        = eAVEncH265VLevel(90);
  {$EXTERNALSYM eAVEncH265VLevel3}
  eAVEncH265VLevel3_1      = eAVEncH265VLevel(93);
  {$EXTERNALSYM eAVEncH265VLevel3_1}
  eAVEncH265VLevel4        = eAVEncH265VLevel(120);
  {$EXTERNALSYM eAVEncH265VLevel4}
  eAVEncH265VLevel4_1      = eAVEncH265VLevel(123);
  {$EXTERNALSYM eAVEncH265VLevel4_1}
  eAVEncH265VLevel5        = eAVEncH265VLevel(150);
  {$EXTERNALSYM eAVEncH265VLevel5}
  eAVEncH265VLevel5_1      = eAVEncH265VLevel(153);
  {$EXTERNALSYM eAVEncH265VLevel5_1}
  eAVEncH265VLevel5_2      = eAVEncH265VLevel(156);
  {$EXTERNALSYM eAVEncH265VLevel5_2}
  eAVEncH265VLevel6        = eAVEncH265VLevel(180);
  {$EXTERNALSYM eAVEncH265VLevel6}
  eAVEncH265VLevel6_1      = eAVEncH265VLevel(183);
  {$EXTERNALSYM eAVEncH265VLevel6_1}
  eAVEncH265VLevel6_2      = eAVEncH265VLevel(186);
  {$EXTERNALSYM eAVEncH265VLevel6_2}


type
  // AVEncMPVFrameFieldMode (UINT32)
  PeAVEncMPVFrameFieldMode = ^eAVEncMPVFrameFieldMode;
  eAVEncMPVFrameFieldMode = UINT32;
  {$EXTERNALSYM eAVEncMPVFrameFieldMode}
const
  eAVEncMPVFrameFieldMode_FieldMode = eAVEncMPVFrameFieldMode(0);
  {$EXTERNALSYM eAVEncMPVFrameFieldMode_FieldMode}
  eAVEncMPVFrameFieldMode_FrameMode = eAVEncMPVFrameFieldMode(1);
  {$EXTERNALSYM eAVEncMPVFrameFieldMode_FrameMode}


  //
  // Advanced MPV Encoder Specific Parameters
  //

type
  PeAVEncMPVSceneDetection = ^eAVEncMPVSceneDetection;
  eAVEncMPVSceneDetection = UINT32;
  {$EXTERNALSYM eAVEncMPVSceneDetection}
const
  eAVEncMPVSceneDetection_None                 = eAVEncMPVSceneDetection(0);
  {$EXTERNALSYM eAVEncMPVSceneDetection_None}
  eAVEncMPVSceneDetection_InsertIPicture       = eAVEncMPVSceneDetection(1);
  {$EXTERNALSYM eAVEncMPVSceneDetection_InsertIPicture}
  eAVEncMPVSceneDetection_StartNewGOP          = eAVEncMPVSceneDetection(2);
  {$EXTERNALSYM eAVEncMPVSceneDetection_StartNewGOP}
  eAVEncMPVSceneDetection_StartNewLocatableGOP = eAVEncMPVSceneDetection(3);
  {$EXTERNALSYM eAVEncMPVSceneDetection_StartNewLocatableGOP}


type
  PeAVEncMPVScanPattern = ^eAVEncMPVScanPattern;
  eAVEncMPVScanPattern = UINT32;
  {$EXTERNALSYM eAVEncMPVScanPattern}
const
  eAVEncMPVScanPattern_Auto          = eAVEncMPVScanPattern(0);
  {$EXTERNALSYM eAVEncMPVScanPattern_Auto}
  eAVEncMPVScanPattern_ZigZagScan    = eAVEncMPVScanPattern(1);
  {$EXTERNALSYM eAVEncMPVScanPattern_ZigZagScan}
  eAVEncMPVScanPattern_AlternateScan = eAVEncMPVScanPattern(2);
  {$EXTERNALSYM eAVEncMPVScanPattern_AlternateScan}


type
  PeAVEncMPVQScaleType = ^eAVEncMPVQScaleType;
  eAVEncMPVQScaleType = UINT32;
  {$EXTERNALSYM eAVEncMPVQScaleType}
const
  eAVEncMPVQScaleType_Auto      = eAVEncMPVQScaleType(0);
  {$EXTERNALSYM eAVEncMPVQScaleType_Auto}
  eAVEncMPVQScaleType_Linear    = eAVEncMPVQScaleType(1);
  {$EXTERNALSYM eAVEncMPVQScaleType_Linear}
  eAVEncMPVQScaleType_NonLinear = eAVEncMPVQScaleType(2);
  {$EXTERNALSYM eAVEncMPVQScaleType_NonLinear}


type
  PeAVEncMPVIntraVLCTable = ^eAVEncMPVIntraVLCTable;
  eAVEncMPVIntraVLCTable = UINT32;
  {$EXTERNALSYM eAVEncMPVIntraVLCTable}
const
  eAVEncMPVIntraVLCTable_Auto      = eAVEncMPVIntraVLCTable(0);
  {$EXTERNALSYM eAVEncMPVIntraVLCTable_Auto}
  eAVEncMPVIntraVLCTable_MPEG1     = eAVEncMPVIntraVLCTable(1);
  {$EXTERNALSYM eAVEncMPVIntraVLCTable_MPEG1}
  eAVEncMPVIntraVLCTable_Alternate = eAVEncMPVIntraVLCTable(2);
  {$EXTERNALSYM eAVEncMPVIntraVLCTable_Alternate}


  //
  // MPEG1 Audio Encoding Interface
  //

  //
  // MPEG1 Audio Specific Parameters
  //

type
  PeAVEncMPALayer = ^eAVEncMPALayer;
  eAVEncMPALayer = UINT32;
  {$EXTERNALSYM eAVEncMPALayer}
const
  eAVEncMPALayer_1 = eAVEncMPALayer(1);
  {$EXTERNALSYM eAVEncMPALayer_1}
  eAVEncMPALayer_2 = eAVEncMPALayer(2);
  {$EXTERNALSYM eAVEncMPALayer_2}
  eAVEncMPALayer_3 = eAVEncMPALayer(3);
  {$EXTERNALSYM eAVEncMPALayer_3}


type
  PeAVEncMPACodingMode = ^eAVEncMPACodingMode;
  eAVEncMPACodingMode = UINT32;
  {$EXTERNALSYM eAVEncMPACodingMode}
const
  eAVEncMPACodingMode_Mono        = eAVEncMPACodingMode(0);
  {$EXTERNALSYM eAVEncMPACodingMode_Mono}
  eAVEncMPACodingMode_Stereo      = eAVEncMPACodingMode(1);
  {$EXTERNALSYM eAVEncMPACodingMode_Stereo}
  eAVEncMPACodingMode_DualChannel = eAVEncMPACodingMode(2);
  {$EXTERNALSYM eAVEncMPACodingMode_DualChannel}
  eAVEncMPACodingMode_JointStereo = eAVEncMPACodingMode(3);
  {$EXTERNALSYM eAVEncMPACodingMode_JointStereo}
  eAVEncMPACodingMode_Surround    = eAVEncMPACodingMode(4);
  {$EXTERNALSYM eAVEncMPACodingMode_Surround}


type
  PeAVEncMPAEmphasisType = ^eAVEncMPAEmphasisType;
  eAVEncMPAEmphasisType = UINT32;
  {$EXTERNALSYM eAVEncMPAEmphasisType}
const
  eAVEncMPAEmphasisType_None        = eAVEncMPAEmphasisType(0);
  {$EXTERNALSYM eAVEncMPAEmphasisType_None}
  eAVEncMPAEmphasisType_50_15       = eAVEncMPAEmphasisType(1);
  {$EXTERNALSYM eAVEncMPAEmphasisType_50_15}
  eAVEncMPAEmphasisType_Reserved    = eAVEncMPAEmphasisType(2);
  {$EXTERNALSYM eAVEncMPAEmphasisType_Reserved}
  eAVEncMPAEmphasisType_CCITT_J17   = eAVEncMPAEmphasisType(3);
  {$EXTERNALSYM eAVEncMPAEmphasisType_CCITT_J17}


  //
  // Dolby Digital(TM) Audio Encoding Interface
  //

  //
  // Dolby Digital(TM) Audio Specific Parameters
  //
type
  PeAVEncDDService = ^eAVEncDDService;
  eAVEncDDService = UINT32;
  {$EXTERNALSYM eAVEncDDService}
const
  eAVEncDDService_CM = eAVEncDDService(0);   // (Main Service: Complete Main)
  {$EXTERNALSYM eAVEncDDService_CM}
  eAVEncDDService_ME = eAVEncDDService(1);   // (Main Service: Music and Effects (ME))
  {$EXTERNALSYM eAVEncDDService_ME}
  eAVEncDDService_VI = eAVEncDDService(2);   // (Associated Service: Visually-Impaired (VI)
  {$EXTERNALSYM eAVEncDDService_VI}
  eAVEncDDService_HI = eAVEncDDService(3);   // (Associated Service: Hearing-Impaired (HI))
  {$EXTERNALSYM eAVEncDDService_HI}
  eAVEncDDService_D  = eAVEncDDService(4);   // (Associated Service: Dialog (D))
  {$EXTERNALSYM eAVEncDDService_D}
  eAVEncDDService_C  = eAVEncDDService(5);   // (Associated Service: Commentary (C))
  {$EXTERNALSYM eAVEncDDService_C}
  eAVEncDDService_E  = eAVEncDDService(6);   // (Associated Service: Emergency (E))
  {$EXTERNALSYM eAVEncDDService_E}
  eAVEncDDService_VO = eAVEncDDService(7);   // (Associated Service: Voice Over (VO) / Karaoke)
  {$EXTERNALSYM eAVEncDDService_VO}

type
  PeAVEncDDProductionRoomType = ^eAVEncDDProductionRoomType;
  eAVEncDDProductionRoomType = UINT32;
  {$EXTERNALSYM eAVEncDDProductionRoomType}
const
  eAVEncDDProductionRoomType_NotIndicated = eAVEncDDProductionRoomType(0);
  {$EXTERNALSYM eAVEncDDProductionRoomType_NotIndicated}
  eAVEncDDProductionRoomType_Large        = eAVEncDDProductionRoomType(1);
  {$EXTERNALSYM eAVEncDDProductionRoomType_Large}
  eAVEncDDProductionRoomType_Small        = eAVEncDDProductionRoomType(2);
  {$EXTERNALSYM eAVEncDDProductionRoomType_Small}


type
  PeAVEncDDDynamicRangeCompressionControl = ^eAVEncDDDynamicRangeCompressionControl;
  eAVEncDDDynamicRangeCompressionControl = UINT32;
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl}
const
  eAVEncDDDynamicRangeCompressionControl_None          = eAVEncDDDynamicRangeCompressionControl(0);
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl_None}
  eAVEncDDDynamicRangeCompressionControl_FilmStandard  = eAVEncDDDynamicRangeCompressionControl(1);
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl_FilmStandard}
  eAVEncDDDynamicRangeCompressionControl_FilmLight     = eAVEncDDDynamicRangeCompressionControl(2);
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl_FilmLight}
  eAVEncDDDynamicRangeCompressionControl_MusicStandard = eAVEncDDDynamicRangeCompressionControl(3);
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl_MusicStandard}
  eAVEncDDDynamicRangeCompressionControl_MusicLight    = eAVEncDDDynamicRangeCompressionControl(4);
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl_MusicLight}
  eAVEncDDDynamicRangeCompressionControl_Speech        = eAVEncDDDynamicRangeCompressionControl(5);
  {$EXTERNALSYM eAVEncDDDynamicRangeCompressionControl_Speech}


type
  PeAVEncDDSurroundExMode = ^eAVEncDDSurroundExMode;
  eAVEncDDSurroundExMode = UINT32;
  {$EXTERNALSYM eAVEncDDSurroundExMode}
const
  eAVEncDDSurroundExMode_NotIndicated = eAVEncDDSurroundExMode(0);
  {$EXTERNALSYM eAVEncDDSurroundExMode_NotIndicated}
  eAVEncDDSurroundExMode_No           = eAVEncDDSurroundExMode(1);
  {$EXTERNALSYM eAVEncDDSurroundExMode_No}
  eAVEncDDSurroundExMode_Yes          = eAVEncDDSurroundExMode(2);
  {$EXTERNALSYM eAVEncDDSurroundExMode_Yes}


type
  PeAVEncDDPreferredStereoDownMixMode = ^eAVEncDDPreferredStereoDownMixMode;
  eAVEncDDPreferredStereoDownMixMode = UINT32;
  {$EXTERNALSYM eAVEncDDPreferredStereoDownMixMode}
const
  eAVEncDDPreferredStereoDownMixMode_LtRt = eAVEncDDPreferredStereoDownMixMode(0);
  {$EXTERNALSYM eAVEncDDPreferredStereoDownMixMode_LtRt}
  eAVEncDDPreferredStereoDownMixMode_LoRo = eAVEncDDPreferredStereoDownMixMode(1);
  {$EXTERNALSYM eAVEncDDPreferredStereoDownMixMode_LoRo}


type
  PeAVEncDDAtoDConverterType = ^eAVEncDDAtoDConverterType;
  eAVEncDDAtoDConverterType = UINT32;
  {$EXTERNALSYM eAVEncDDAtoDConverterType}
const
  eAVEncDDAtoDConverterType_Standard = eAVEncDDAtoDConverterType(0);
  {$EXTERNALSYM eAVEncDDAtoDConverterType_Standard}
  eAVEncDDAtoDConverterType_HDCD     = eAVEncDDAtoDConverterType(1);
  {$EXTERNALSYM eAVEncDDAtoDConverterType_HDCD}


type
  PeAVEncDDHeadphoneMode = ^eAVEncDDHeadphoneMode;
  eAVEncDDHeadphoneMode = UINT32;
  {$EXTERNALSYM eAVEncDDHeadphoneMode}
const
  eAVEncDDHeadphoneMode_NotIndicated = eAVEncDDHeadphoneMode(0);
  {$EXTERNALSYM eAVEncDDHeadphoneMode_NotIndicated}
  eAVEncDDHeadphoneMode_NotEncoded   = eAVEncDDHeadphoneMode(1);
  {$EXTERNALSYM eAVEncDDHeadphoneMode_NotEncoded}
  eAVEncDDHeadphoneMode_Encoded      = eAVEncDDHeadphoneMode(2);
  {$EXTERNALSYM eAVEncDDHeadphoneMode_Encoded}

  //
  // WMV Video Encoding Interface
  //

  //
  // WMV Video Specific Parameters
  //

type
  PeAVDecVideoInputScanType = ^eAVDecVideoInputScanType;
  eAVDecVideoInputScanType = UINT32;
  {$EXTERNALSYM eAVDecVideoInputScanType}
const
  eAVDecVideoInputScan_Unknown                    = eAVDecVideoInputScanType(0);
  {$EXTERNALSYM eAVDecVideoInputScan_Unknown}
  eAVDecVideoInputScan_Progressive                = eAVDecVideoInputScanType(1);
  {$EXTERNALSYM eAVDecVideoInputScan_Progressive}
  eAVDecVideoInputScan_Interlaced_UpperFieldFirst = eAVDecVideoInputScanType(2);
  {$EXTERNALSYM eAVDecVideoInputScan_Interlaced_UpperFieldFirst}
  eAVDecVideoInputScan_Interlaced_LowerFieldFirst = eAVDecVideoInputScanType(3);
  {$EXTERNALSYM eAVDecVideoInputScan_Interlaced_LowerFieldFirst}


type
  PeAVDecVideoSWPowerLevel = ^eAVDecVideoSWPowerLevel;
  eAVDecVideoSWPowerLevel = UINT32;
  {$EXTERNALSYM eAVDecVideoSWPowerLevel}
const
  eAVDecVideoSWPowerLevel_BatteryLife  = eAVDecVideoSWPowerLevel(0);
  {$EXTERNALSYM eAVDecVideoSWPowerLevel_BatteryLife}
  eAVDecVideoSWPowerLevel_Balanced     = eAVDecVideoSWPowerLevel(50);
  {$EXTERNALSYM eAVDecVideoSWPowerLevel_Balanced}
  eAVDecVideoSWPowerLevel_VideoQuality = eAVDecVideoSWPowerLevel(100);
  {$EXTERNALSYM eAVDecVideoSWPowerLevel_VideoQuality}


  //
  // Audio Decoder properties
  //

type
  PeAVDecAACDownmixMode = ^eAVDecAACDownmixMode;
  eAVDecAACDownmixMode = UINT32;
  {$EXTERNALSYM eAVDecAACDownmixMode}
const
  eAVDecAACUseISODownmix  = eAVDecAACDownmixMode(0);
  {$EXTERNALSYM eAVDecAACUseISODownmix}
  eAVDecAACUseARIBDownmix = eAVDecAACDownmixMode(1);
  {$EXTERNALSYM eAVDecAACUseARIBDownmix}


type
  PeAVDecHEAACDynamicRangeControl = ^eAVDecHEAACDynamicRangeControl;
  eAVDecHEAACDynamicRangeControl = UINT32;
  {$EXTERNALSYM eAVDecHEAACDynamicRangeControl}
const
  eAVDecHEAACDynamicRangeControl_OFF  = eAVDecHEAACDynamicRangeControl(0);
  {$EXTERNALSYM eAVDecHEAACDynamicRangeControl_OFF}
  eAVDecHEAACDynamicRangeControl_ON   = eAVDecHEAACDynamicRangeControl(1);
  {$EXTERNALSYM eAVDecHEAACDynamicRangeControl_ON}


type
  PeAVDecAudioDualMono = ^eAVDecAudioDualMono;
  eAVDecAudioDualMono = UINT32;
  {$EXTERNALSYM eAVDecAudioDualMono}
const
  eAVDecAudioDualMono_IsNotDualMono = eAVDecAudioDualMono(0);    // 2-ch bitstream input is not dual mono
  {$EXTERNALSYM eAVDecAudioDualMono_IsNotDualMono}
  eAVDecAudioDualMono_IsDualMono    = eAVDecAudioDualMono(1);    // 2-ch bitstream input is dual mono
  {$EXTERNALSYM eAVDecAudioDualMono_IsDualMono}
  eAVDecAudioDualMono_UnSpecified   = eAVDecAudioDualMono(2);    // There is no indication in the bitstream
  {$EXTERNALSYM eAVDecAudioDualMono_UnSpecified}


type
  PeAVDecAudioDualMonoReproMode = ^eAVDecAudioDualMonoReproMode;
  eAVDecAudioDualMonoReproMode = UINT32;
  {$EXTERNALSYM eAVDecAudioDualMonoReproMode}
const
  eAVDecAudioDualMonoReproMode_STEREO      = eAVDecAudioDualMonoReproMode(0);   // Ch1+Ch2 for mono output, (Ch1 left,     Ch2 right) for stereo output
  {$EXTERNALSYM eAVDecAudioDualMonoReproMode_STEREO}
  eAVDecAudioDualMonoReproMode_LEFT_MONO   = eAVDecAudioDualMonoReproMode(1);   // Ch1 for mono output,     (Ch1 left,     Ch1 right) for stereo output
  {$EXTERNALSYM eAVDecAudioDualMonoReproMode_LEFT_MONO}
  eAVDecAudioDualMonoReproMode_RIGHT_MONO  = eAVDecAudioDualMonoReproMode(2);   // Ch2 for mono output,     (Ch2 left,     Ch2 right) for stereo output
  {$EXTERNALSYM eAVDecAudioDualMonoReproMode_RIGHT_MONO}
  eAVDecAudioDualMonoReproMode_MIX_MONO    = eAVDecAudioDualMonoReproMode(3);   // Ch1+Ch2 for mono output, (Ch1+Ch2 left, Ch1+Ch2 right) for stereo output
  {$EXTERNALSYM eAVDecAudioDualMonoReproMode_MIX_MONO}


  //
  // Audio Common Properties
  //

  // Enumerated values for  AVAudioChannelConfig are identical
  // to the speaker positions defined in ksmedia.h and used
  // in WAVE_FORMAT_EXTENSIBLE. Configurations for 5.1 and
  // 7.1 channels should be identical to KSAUDIO_SPEAKER_5POINT1_SURROUND
  // and KSAUDIO_SPEAKER_7POINT1_SURROUND in ksmedia.h. This means:
  // 5.1 ch -> LOW_FREQUENCY | FRONT_LEFT | FRONT_RIGHT | FRONT_CENTER | SIDE_LEFT | SIDE_RIGHT
  // 7.1 ch -> LOW_FREQUENCY | FRONT_LEFT | FRONT_RIGHT | FRONT_CENTER | SIDE_LEFT | SIDE_RIGHT | BACK_LEFT | BACK_RIGHT
  //
type
  PeAVAudioChannelConfig = ^eAVAudioChannelConfig;
  eAVAudioChannelConfig = UINT32;
  {$EXTERNALSYM eAVAudioChannelConfig}
const
  eAVAudioChannelConfig_FRONT_LEFT    = eAVAudioChannelConfig($1);
  {$EXTERNALSYM eAVAudioChannelConfig_FRONT_LEFT}
  eAVAudioChannelConfig_FRONT_RIGHT   = eAVAudioChannelConfig($2);
  {$EXTERNALSYM eAVAudioChannelConfig_FRONT_RIGHT}
  eAVAudioChannelConfig_FRONT_CENTER  = eAVAudioChannelConfig($4);
  {$EXTERNALSYM eAVAudioChannelConfig_FRONT_CENTER}
  eAVAudioChannelConfig_LOW_FREQUENCY = eAVAudioChannelConfig($8);  // aka LFE
  {$EXTERNALSYM eAVAudioChannelConfig_LOW_FREQUENCY}
  eAVAudioChannelConfig_BACK_LEFT     = eAVAudioChannelConfig($10);
  {$EXTERNALSYM eAVAudioChannelConfig_BACK_LEFT}
  eAVAudioChannelConfig_BACK_RIGHT    = eAVAudioChannelConfig($20);
  {$EXTERNALSYM eAVAudioChannelConfig_BACK_RIGHT}
  eAVAudioChannelConfig_FRONT_LEFT_OF_CENTER  = eAVAudioChannelConfig($40);
  {$EXTERNALSYM eAVAudioChannelConfig_FRONT_LEFT_OF_CENTER}
  eAVAudioChannelConfig_FRONT_RIGHT_OF_CENTER = eAVAudioChannelConfig($80);
  {$EXTERNALSYM eAVAudioChannelConfig_FRONT_RIGHT_OF_CENTER}
  eAVAudioChannelConfig_BACK_CENTER = eAVAudioChannelConfig($100);  // aka Mono Surround
  {$EXTERNALSYM eAVAudioChannelConfig_BACK_CENTER}
  eAVAudioChannelConfig_SIDE_LEFT   = eAVAudioChannelConfig($200);  // aka Left Surround
  {$EXTERNALSYM eAVAudioChannelConfig_SIDE_LEFT}
  eAVAudioChannelConfig_SIDE_RIGHT  = eAVAudioChannelConfig($400);  // aka Right Surround
  {$EXTERNALSYM eAVAudioChannelConfig_SIDE_RIGHT}
  eAVAudioChannelConfig_TOP_CENTER  = eAVAudioChannelConfig($800);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_CENTER}
  eAVAudioChannelConfig_TOP_FRONT_LEFT   = eAVAudioChannelConfig($1000);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_FRONT_LEFT}
  eAVAudioChannelConfig_TOP_FRONT_CENTER = eAVAudioChannelConfig($2000);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_FRONT_CENTER}
  eAVAudioChannelConfig_TOP_FRONT_RIGHT  = eAVAudioChannelConfig($4000);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_FRONT_RIGHT}
  eAVAudioChannelConfig_TOP_BACK_LEFT    = eAVAudioChannelConfig($8000);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_BACK_LEFT}
  eAVAudioChannelConfig_TOP_BACK_CENTER  = eAVAudioChannelConfig($10000);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_BACK_CENTER}
  eAVAudioChannelConfig_TOP_BACK_RIGHT   = eAVAudioChannelConfig($20000);
  {$EXTERNALSYM eAVAudioChannelConfig_TOP_BACK_RIGHT}


type
  PeAVDDSurroundMode = ^eAVDDSurroundMode;
  eAVDDSurroundMode = UINT32;
  {$EXTERNALSYM eAVDDSurroundMode}
const
  eAVDDSurroundMode_NotIndicated = eAVDDSurroundMode(0);
  {$EXTERNALSYM eAVDDSurroundMode_NotIndicated}
  eAVDDSurroundMode_No           = eAVDDSurroundMode(1);
  {$EXTERNALSYM eAVDDSurroundMode_No}
  eAVDDSurroundMode_Yes          = eAVDDSurroundMode(2);
  {$EXTERNALSYM eAVDDSurroundMode_Yes}


type
  PeAVDecDDOperationalMode = ^eAVDecDDOperationalMode;
  eAVDecDDOperationalMode  = UINT32;
  {$EXTERNALSYM eAVDecDDOperationalMode}
const
  eAVDecDDOperationalMode_NONE       = eAVDecDDOperationalMode(0);
  {$EXTERNALSYM eAVDecDDOperationalMode_NONE}
  eAVDecDDOperationalMode_LINE       = eAVDecDDOperationalMode(1);    // Dialnorm enabled, dialogue at -31dBFS, dynrng used, high/low scaling allowed
  {$EXTERNALSYM eAVDecDDOperationalMode_LINE}
  eAVDecDDOperationalMode_RF         = eAVDecDDOperationalMode(2);    // Dialnorm enabled, dialogue at -20dBFS, dynrng & compr used, high/low scaling NOT allowed (always fully compressed)
  {$EXTERNALSYM eAVDecDDOperationalMode_RF}
  eAVDecDDOperationalMode_CUSTOM0    = eAVDecDDOperationalMode(3);    // Analog dialnorm (dialogue normalization not part of the decoder)
  {$EXTERNALSYM eAVDecDDOperationalMode_CUSTOM0}
  eAVDecDDOperationalMode_CUSTOM1    = eAVDecDDOperationalMode(4);    // Digital dialnorm (dialogue normalization is part of the decoder)
  {$EXTERNALSYM eAVDecDDOperationalMode_CUSTOM1}
  eAVDecDDOperationalMode_PORTABLE8  = eAVDecDDOperationalMode(5);    // Dialnorm enabled, dialogue at -8dBFS, dynrng & compr used, high/low scaling NOT allowed (always fully compressed)
  {$EXTERNALSYM eAVDecDDOperationalMode_PORTABLE8}
  eAVDecDDOperationalMode_PORTABLE11 = eAVDecDDOperationalMode(6);    // Dialnorm enabled, dialogue at -11dBFS, dynrng & compr used, high/low scaling NOT allowed (always fully compressed)
  {$EXTERNALSYM eAVDecDDOperationalMode_PORTABLE11}
  eAVDecDDOperationalMode_PORTABLE14 = eAVDecDDOperationalMode(7);    // Dialnorm enabled, dialogue at -14dBFS, dynrng & compr used, high/low scaling NOT allowed (always fully compressed)
  {$EXTERNALSYM eAVDecDDOperationalMode_PORTABLE14}


type
  PeAVDecDDMatrixDecodingMode = ^eAVDecDDMatrixDecodingMode;
  eAVDecDDMatrixDecodingMode = UINT32;
  {$EXTERNALSYM eAVDecDDMatrixDecodingMode}
const
  eAVDecDDMatrixDecodingMode_OFF  = eAVDecDDMatrixDecodingMode(0);
  {$EXTERNALSYM eAVDecDDMatrixDecodingMode_OFF}
  eAVDecDDMatrixDecodingMode_ON   = eAVDecDDMatrixDecodingMode(1);
  {$EXTERNALSYM eAVDecDDMatrixDecodingMode_ON}
  eAVDecDDMatrixDecodingMode_AUTO = eAVDecDDMatrixDecodingMode(2);
  {$EXTERNALSYM eAVDecDDMatrixDecodingMode_AUTO}


type
  PeAVDecDDStereoDownMixMode = ^eAVDecDDStereoDownMixMode;
  eAVDecDDStereoDownMixMode = UINT32;
  {$EXTERNALSYM eAVDecDDStereoDownMixMode}
const
  eAVDecDDStereoDownMixMode_Auto  = eAVDecDDStereoDownMixMode(0);   // Automatic detection
  {$EXTERNALSYM eAVDecDDStereoDownMixMode_Auto}
  eAVDecDDStereoDownMixMode_LtRt  = eAVDecDDStereoDownMixMode(1);   // Surround compatible (Lt/Rt)
  {$EXTERNALSYM eAVDecDDStereoDownMixMode_LtRt}
  eAVDecDDStereoDownMixMode_LoRo  = eAVDecDDStereoDownMixMode(2);   // Stereo (Lo/Ro)
  {$EXTERNALSYM eAVDecDDStereoDownMixMode_LoRo}


type
  PeAVDSPLoudnessEqualization = ^eAVDSPLoudnessEqualization;
  eAVDSPLoudnessEqualization = UINT32;
  {$EXTERNALSYM eAVDSPLoudnessEqualization}
const
  eAVDSPLoudnessEqualization_OFF  = eAVDSPLoudnessEqualization(0);
  {$EXTERNALSYM eAVDSPLoudnessEqualization_OFF}
  eAVDSPLoudnessEqualization_ON   = eAVDSPLoudnessEqualization(1);
  {$EXTERNALSYM eAVDSPLoudnessEqualization_ON}
  eAVDSPLoudnessEqualization_AUTO = eAVDSPLoudnessEqualization(2);
  {$EXTERNALSYM eAVDSPLoudnessEqualization_AUTO}


type
  PeAVDSPSpeakerFill = ^eAVDSPSpeakerFill;
  eAVDSPSpeakerFill = UINT32;
  {$EXTERNALSYM eAVDSPSpeakerFill}
const
  eAVDSPSpeakerFill_OFF  = eAVDSPSpeakerFill(0);
  {$EXTERNALSYM eAVDSPSpeakerFill_OFF}
  eAVDSPSpeakerFill_ON   = eAVDSPSpeakerFill(1);
  {$EXTERNALSYM eAVDSPSpeakerFill_ON}
  eAVDSPSpeakerFill_AUTO = eAVDSPSpeakerFill(2);
  {$EXTERNALSYM eAVDSPSpeakerFill_AUTO}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  //Implement Additional functions here.

end.

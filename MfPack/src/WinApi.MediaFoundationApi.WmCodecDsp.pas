// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.WmCodecDsp.pas
// Kind: Pascal / Delphi unit
// Release date: 25-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Contains the definitions for the speech interfaces.
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
// =============================================================================
// Source: wmcodecdsp.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// =============================================================================
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
// =============================================================================
unit WinApi.MediaFoundationApi.WmCodecDsp;

  {$HPPEMIT '#include "MfError.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  WinApi.MediaObj,
  {ActiveX}
  WinApi.ActiveX.PropKeyDef,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

  // MP43DECD.dll

  // header files for imported files


  // ========================================================================
  //
  //  THIS SOFTWARE HAS BEEN LICENSED FROM MICROSOFT CORPORATION PURSUANT
  //  TO THE TERMS OF AN END USER LICENSE AGREEMENT ("EULA").
  //  PLEASE REFER TO THE TEXT OF THE EULA TO DETERMINE THE RIGHTS TO USE THE SOFTWARE.
  //
  // Copyright (C) Microsoft Corporation, All Rights Reserved.
  //
  // ========================================================================


type
  PWmtPropDatatype = ^TWmtPropDatatype;
  WMT_PROP_DATATYPE      = (
    WMT_PROP_TYPE_DWORD  = 0,
    WMT_PROP_TYPE_STRING = 1,
    WMT_PROP_TYPE_BINARY = 2,
    WMT_PROP_TYPE_BOOL   = 3,
    WMT_PROP_TYPE_QWORD  = 4,
    WMT_PROP_TYPE_WORD   = 5,
    WMT_PROP_TYPE_GUID   = 6
  );
  {$EXTERNALSYM WMT_PROP_DATATYPE}
  TWmtPropDatatype = WMT_PROP_DATATYPE;
  {$EXTERNALSYM TWmtPropDatatype}

  ////////////////////////////////////////////////////////////////
  //
  // The Speech code supports the following format property.
  //

const
  //static const WCHAR *g_wszSpeechFormatCaps = L"SpeechFormatCap";
  //static const WCHAR *g_wszWMCPCodecName = L"_CODECNAME";
  //static const WCHAR *g_wszWMCPSupportedVBRModes = L"_SUPPORTEDVBRMODES";

  g_wszSpeechFormatCaps      = 'SpeechFormatCap';
  {$EXTERNALSYM g_wszSpeechFormatCaps}
  g_wszWMCPCodecName         = '_CODECNAME';
  {$EXTERNALSYM g_wszWMCPCodecName}
  g_wszWMCPSupportedVBRModes = '_SUPPORTEDVBRMODES';
  {$EXTERNALSYM g_wszWMCPSupportedVBRModes}


  WM_CODEC_ONEPASS_CBR                 = 1;
  {$EXTERNALSYM WM_CODEC_ONEPASS_CBR}
  WM_CODEC_ONEPASS_VBR                 = 2;
  {$EXTERNALSYM WM_CODEC_ONEPASS_VBR}
  WM_CODEC_TWOPASS_CBR                 = 4;
  {$EXTERNALSYM WM_CODEC_TWOPASS_CBR}
  WM_CODEC_TWOPASS_VBR_UNCONSTRAINED   = 8;
  {$EXTERNALSYM WM_CODEC_TWOPASS_VBR_UNCONSTRAINED}
  WM_CODEC_TWOPASS_VBR_PEAKCONSTRAINED = 16;
  {$EXTERNALSYM WM_CODEC_TWOPASS_VBR_PEAKCONSTRAINED}


  g_wszWMCPAudioVBRSupported = '_VBRENABLED';
  {$EXTERNALSYM g_wszWMCPAudioVBRSupported}
  g_wszWMCPAudioVBRQuality   = '_VBRQUALITY';
  {$EXTERNALSYM g_wszWMCPAudioVBRQuality}
  g_wszWMCPMaxPasses         = '_PASSESRECOMMENDED';
  {$EXTERNALSYM g_wszWMCPMaxPasses}
  g_wszWMCPDefaultCrisp      = '_DEFAULTCRISP';
  {$EXTERNALSYM g_wszWMCPDefaultCrisp}

type

  // Interface IWMValidate
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMValidate);'}
  {$EXTERNALSYM IWMValidate}
  IWMValidate = interface(IUnknown)
    ['{CEE3DEF2-3808-414d-BE66-FAFD472210BC}']

      function SetIdentifier(guidValidationID: TGuid): HResult; stdcall;

  end;
  IID_IWMValidate = IWMValidate;
  {$EXTERNALSYM IID_IWMValidate}


  // Interface IValidateBinding
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IValidateBinding);'}
  {$EXTERNALSYM IValidateBinding}
  IValidateBinding = interface(IUnknown)
    ['{04A578B2-E778-422a-A805-B3EE54D90BD9}']

    function GetIdentifier(guidLicensorID: TGUID;
                           pbEphemeron: PByte;
                           cbEphemeron: DWORD;
                           out ppbBlobValidationID: PByte;
                           out pcbBlobSize: DWORD): HResult; stdcall;

    end;
  IID_IValidateBinding = IValidateBinding;
  {$EXTERNALSYM IID_IValidateBinding}


  // Interface IWMVideoDecoderHurryup
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMVideoDecoderHurryup);'}
  {$EXTERNALSYM IWMVideoDecoderHurryup}
  IWMVideoDecoderHurryup = interface(IUnknown)
    ['{352bb3bd-2d4d-4323-9e71-dcdcfbd53ca6}']

      function SetHurryup(lHurryup: LONG): HResult; stdcall;

      function GetHurryup(out plHurryup: LONG): HResult; stdcall;

    end;
  IID_IWMVideoDecoderHurryup = IWMVideoDecoderHurryup;
  {$EXTERNALSYM IID_IWMVideoDecoderHurryup}


  // Interface IWMVideoForceKeyFrame
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMVideoForceKeyFrame);'}
  {$EXTERNALSYM IWMVideoForceKeyFrame}
  IWMVideoForceKeyFrame = interface(IUnknown)
    ['{9F8496BE-5B9A-41b9-A9E8-F21CD80596C2}']

      function SetKeyFrame(): HResult; stdcall;

  end;
  IID_IWMVideoForceKeyFrame = IWMVideoForceKeyFrame;
  {$EXTERNALSYM IID_IWMVideoForceKeyFrame}


  // Interface IWMCodecStrings
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMCodecStrings);'}
  {$EXTERNALSYM IWMCodecStrings}
  IWMCodecStrings = interface(IUnknown)
    ['{A7B2504B-E58A-47fb-958B-CAC7165A057D}']

      function GetName(pmt: DMO_MEDIA_TYPE;
                       cchLength: ULONG;
                       var szName: PWideChar;
                       out pcchLength: ULONG): HResult; stdcall;

      function GetDescription(pmt: DMO_MEDIA_TYPE;
                              cchLength: ULONG;
                              var szDescription: PWideChar;
                              out pcchLength: ULONG): HResult; stdcall;
  end;
  // IWMCodecStrings
  IID_IWMCodecStrings = IWMCodecStrings;
  {$EXTERNALSYM IID_IWMCodecStrings}


  // Interface IWMCodecProps
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMCodecProps);'}
  {$EXTERNALSYM IWMCodecProps}
  IWMCodecProps = interface(IUnknown)
    ['{2573e11a-f01a-4fdd-a98d-63b8e0ba9589}']
      function GetFormatProp(pmt: DMO_MEDIA_TYPE;
                             pszName: PWideChar;
                             out pType: WMT_PROP_DATATYPE;
                             var pValue: PByte;
                             var pdwSize: DWORD): HResult; stdcall;


      function GetCodecProp(dwFormat: DWORD;
                            pszName: PWideChar;
                            out pType: WMT_PROP_DATATYPE;
                            pValue: PByte;
                            var pdwSize: DWORD): HResult; stdcall;
  end;
  // IWMCodecProps
  IID_IWMCodecProps = IWMCodecProps;
  {$EXTERNALSYM IID_IWMCodecProps}


  // Interface IWMCodecLeakyBucket
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMCodecLeakyBucket);'}
  {$EXTERNALSYM IWMCodecLeakyBucket}
  IWMCodecLeakyBucket = interface(IUnknown)
    ['{A81BA647-6227-43b7-B231-C7B15135DD7D}']

      function SetBufferSizeBits(ulBufferSize: ULONG): HResult; stdcall;

      function GetBufferSizeBits(out pulBufferSize: ULONG): HResult; stdcall;

      function SetBufferFullnessBits(ulBufferFullness: ULONG): HResult; stdcall;

      function GetBufferFullnessBits(out pulBufferFullness: ULONG): HResult; stdcall;

  end;
  // IWMCodecLeakyBucket
  IID_IWMCodecLeakyBucket = IWMCodecLeakyBucket;
  {$EXTERNALSYM IID_IWMCodecLeakyBucket}


  // Interface IWMCodecOutputTimestamp
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMCodecOutputTimestamp);'}
  {$EXTERNALSYM IWMCodecOutputTimestamp}
  IWMCodecOutputTimestamp = interface(IUnknown)
    ['{B72ADF95-7ADC-4a72-BC05-577D8EA6BF68}']

      function GetNextOutputTime(out prtTime: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IWMCodecOutputTimestamp = IWMCodecOutputTimestamp;
  {$EXTERNALSYM IID_IWMCodecOutputTimestamp}


  // Interface IWMVideoDecoderReconBuffer
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMVideoDecoderReconBuffer);'}
  {$EXTERNALSYM IWMVideoDecoderReconBuffer}
  IWMVideoDecoderReconBuffer = interface(IUnknown)
    ['{45BDA2AC-88E2-4923-98BA-3949080711A3}']

      function GetReconstructedVideoFrameSize(out pdwSize: DWORD): HResult; stdcall;

      function GetReconstructedVideoFrame(pBuf: IMediaBuffer): HResult; stdcall;

      function SetReconstructedVideoFrame(pBuf: IMediaBuffer): HResult; stdcall;

  end;
  IID_IWMVideoDecoderReconBuffer = IWMVideoDecoderReconBuffer;
  {$EXTERNALSYM IID_IWMVideoDecoderReconBuffer}


  // Interface IWMCodecPrivateData
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMCodecPrivateData);'}
  {$EXTERNALSYM IWMCodecPrivateData}
  IWMCodecPrivateData = interface(IUnknown)
    ['{73F0BE8E-57F7-4f01-AA66-9F57340CFE0E}']

      function SetPartialOutputType(pmt: DMO_MEDIA_TYPE): HResult; stdcall;

      function GetPrivateData(pbData: PByte;
                              var pcbData: ULONG): HResult; stdcall;

  end;
  IID_IWMCodecPrivateData = IWMCodecPrivateData;
  {$EXTERNALSYM IID_IWMCodecPrivateData}


  // Interface IWMSampleExtensionSupport
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMSampleExtensionSupport);'}
  {$EXTERNALSYM IWMSampleExtensionSupport}
  IWMSampleExtensionSupport = interface(IUnknown)
    ['{9bca9884-0604-4c2a-87da-793ff4d586c3}']

      function SetUseSampleExtensions(fUseExtensions: BOOL): HResult; stdcall;

  end;
  IID_IWMSampleExtensionSupport = IWMSampleExtensionSupport;
  {$EXTERNALSYM IID_IWMSampleExtensionSupport}


  PChMtxType = ^ChMtxType;
  ChMtxType = Single;
  {$EXTERNALSYM ChMtxType}

  // Interface IWMResamplerProps
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMResamplerProps);'}
  {$EXTERNALSYM IWMResamplerProps}
  IWMResamplerProps = interface(IUnknown)
    ['{E7E9984F-F09F-4da4-903F-6E2E0EFE56B5}']

      function SetHalfFilterLength(lhalfFilterLen: LONG): HResult; stdcall;

      function SetUserChannelMtx(userChannelMtx: ChMtxType): HResult; stdcall;

  end;
  IID_IWMResamplerProps = IWMResamplerProps;
  {$EXTERNALSYM IID_IWMResamplerProps}


  // Interface IWMResizerProps
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMResizerProps);'}
  {$EXTERNALSYM IWMResizerProps}
  IWMResizerProps = interface(IUnknown)
    ['{57665D4C-0414-4faa-905B-10E546F81C33}']

      function SetResizerQuality(lquality: LONG): HResult; stdcall;

      function SetInterlaceMode(lmode: LONG): HResult; stdcall;

      function SetClipRegion(lClipOriXSrc: LONG;
                             lClipOriYSrc: LONG;
                             lClipWidthSrc: LONG;
                             lClipHeightSrc: LONG): HResult; stdcall;

      function SetFullCropRegion(lClipOriXSrc: LONG;
                                 lClipOriYSrc: LONG;
                                 lClipWidthSrc: LONG;
                                 lClipHeightSrc: LONG;
                                 lClipOriXDst: LONG;
                                 lClipOriYDst: LONG;
                                 lClipWidthDst: LONG;
                                 lClipHeightDst: LONG): HResult; stdcall;

      function GetFullCropRegion(out lClipOriXSrc: LONG;
                                 out lClipOriYSrc: LONG;
                                 out lClipWidthSrc: LONG;
                                 out lClipHeightSrc: LONG;
                                 out lClipOriXDst: LONG;
                                 out lClipOriYDst: LONG;
                                 out lClipWidthDst: LONG;
                                 out lClipHeightDst: LONG): HResult; stdcall;
  end;
  IID_IWMResizerProps = IWMResizerProps;
  {$EXTERNALSYM IID_IWMResizerProps}


  // Interface IWMColorLegalizerProps
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMColorLegalizerProps);'}
  {$EXTERNALSYM IWMColorLegalizerProps}
  IWMColorLegalizerProps = interface(IUnknown)
    ['{776C93B3-B72D-4508-B6D0-208785F553E7}']

      function SetColorLegalizerQuality(lquality: LONG): HResult; stdcall;

  end;
  IID_IWMColorLegalizerProps = IWMColorLegalizerProps;
  {$EXTERNALSYM IID_IWMColorLegalizerProps}


  // Interface IWMInterlaceProps
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMInterlaceProps);'}
  {$EXTERNALSYM IWMInterlaceProps}
  IWMInterlaceProps = interface(IUnknown)
    ['{7B12E5D1-BD22-48ea-BC06-98E893221C89}']

      function SetProcessType(iProcessType: Integer): HResult; stdcall;

      function SetInitInverseTeleCinePattern(iInitPattern: Integer): HResult; stdcall;

      function SetLastFrame(): HResult; stdcall;

  end;
  IID_IWMInterlaceProps = IWMInterlaceProps;
  {$EXTERNALSYM IID_IWMInterlaceProps}


  // Interface IWMFrameInterpProps
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMFrameInterpProps);'}
  {$EXTERNALSYM IWMFrameInterpProps}
  IWMFrameInterpProps = interface(IUnknown)
    ['{4C06BB9B-626C-4614-8329-CC6A21B93FA0}']

      function SetFrameRateIn(lFrameRate: LONG;
                              lScale: LONG): HResult; stdcall;

      function SetFrameRateOut(lFrameRate: LONG;
                               lScale: LONG): HResult; stdcall;

      function SetFrameInterpEnabled(bFIEnabled: BOOL): HResult; stdcall;

      function SetComplexityLevel(iComplexity: Integer): HResult; stdcall;

  end;
  IID_IWMFrameInterpProps = IWMFrameInterpProps;
  {$EXTERNALSYM IID_IWMFrameInterpProps}


  // Interface IWMColorConvProps
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWMColorConvProps);'}
  {$EXTERNALSYM IWMColorConvProps}
  IWMColorConvProps = interface(IUnknown)
    ['{E6A49E22-C099-421d-AAD3-C061FB4AE85B}']
      function SetMode(const lMode: LONG): HResult; stdcall;

      function SetFullCroppingParam(lSrcCropLeft: LONG;
                                    lSrcCropTop: LONG;
                                    lDstCropLeft: LONG;
                                    lDstCropTop: LONG;
                                    lCropWidth: LONG;
                                    lCropHeight: LONG): HResult; stdcall;
  end;
  IID_IWMColorConvProps = IWMColorConvProps;
  {$EXTERNALSYM IID_IWMColorConvProps}



const

  MFPKEY_STARTTIME                      :  PROPERTYKEY = (fmtid: (D1: $5cefee10; D2: $e210; D3: $45c6; D4: ($9e, $28, $f5, $a8, $73, $1c, $96, $c7)); pid: $01);
  {$EXTERNALSYM MFPKEY_STARTTIME}
  MFPKEY_STOPTIME                       :  PROPERTYKEY = (fmtid: (D1: $5cefee10; D2: $e210; D3: $45c6; D4: ($9e, $28, $f5, $a8, $73, $1c, $96, $c7)); pid: $02);
  {$EXTERNALSYM MFPKEY_STOPTIME}
  MFPKEY_PROGRESS                       :  PROPERTYKEY = (fmtid: (D1: $5cefee10; D2: $e210; D3: $45c6; D4: ($9e, $28, $f5, $a8, $73, $1c, $96, $c7)); pid: $03);
  {$EXTERNALSYM MFPKEY_PROGRESS}
  MFPKEY_PHANTOMING_ON                  :  PROPERTYKEY = (fmtid: (D1: $12b53cb2; D2: $e12e; D3: $4579; D4: ($8a, $c3, $d0, $2f, $94, $f1, $e8, $9e)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_PHANTOMING_ON}

  PHANTOMING_ENABLED_KEY_GUID           :  PROPERTYKEY = (fmtid: (D1: $12b53cb2; D2: $e12e; D3: $4579; D4: ($8a, $c3, $d0, $2f, $94, $f1, $e8, $9e)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM PHANTOMING_ENABLED_KEY_GUID}

  MFPKEY_ROOMCORR_PROFILE               :  PROPERTYKEY = (fmtid: (D1: $f311cdc7; D2: $f45f; D3: $4eb7; D4: ($a8, $64, $9d, $c1, $ae, $eb, $7e, $6d)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_ROOMCORR_PROFILE}

  ROOM_PROFILE_KEY_GUID                 :  PROPERTYKEY = (fmtid: (D1: $f311cdc7; D2: $f45f; D3: $4eb7; D4: ($a8, $64, $9d, $c1, $ae, $eb, $7e, $6d)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM ROOM_PROFILE_KEY_GUID}

  MFPKEY_BASSMGMT_CROSSOVER_FREQ        :  PROPERTYKEY = (fmtid: (D1: $61e8acb9; D2: $f04f; D3: $4f40; D4: ($a6, $5f, $8f, $49, $fa, $b3, $ba, $10)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_BASSMGMT_CROSSOVER_FREQ}

  CROSSOVER_FREQ_KEY_GUID               :  PROPERTYKEY = (fmtid: (D1: $61e8acb9; D2: $f04f; D3: $4f40; D4: ($a6, $5f, $8f, $49, $fa, $b3, $ba, $10)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM CROSSOVER_FREQ_KEY_GUID}

  MFPKEY_BASSMGMT_SPKRBASSCONFIG        :  PROPERTYKEY = (fmtid: (D1: $7bfd170d; D2: $4770; D3: $4dc5; D4: ($92, $4d, $0b, $7b, $25, $2e, $e9, $18)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_BASSMGMT_SPKRBASSCONFIG}

  FULL_RANGE_SPEAKERS_KEY_GUID          :  PROPERTYKEY = (fmtid: (D1: $7bfd170d; D2: $4770; D3: $4dc5; D4: ($92, $4d, $0b, $7b, $25, $2e, $e9, $18)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM FULL_RANGE_SPEAKERS_KEY_GUID}

  MFPKEY_BASSMGMT_BIGROOM               :  PROPERTYKEY = (fmtid: (D1: $c816a1a7; D2: $a119; D3: $48a5; D4: ($9a, $d2, $85, $45, $1f, $4b, $5a, $2e)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_BASSMGMT_BIGROOM}

  BIG_ROOM_KEY_GUID                     :  PROPERTYKEY = (fmtid: (D1: $c816a1a7; D2: $a119; D3: $48a5; D4: ($9a, $d2, $85, $45, $1f, $4b, $5a, $2e)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM BIG_ROOM_KEY_GUID}

  MFPKEY_BASSMGMT_NO_SUB                :  PROPERTYKEY = (fmtid: (D1: $5c3fd32e; D2: $0d40; D3: $4e2d; D4: ($99, $fb, $c9, $1e, $96, $42, $0b, $e7)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_BASSMGMT_NO_SUB}

  NO_SUB_KEY_GUID                       :  PROPERTYKEY = (fmtid: (D1: $5c3fd32e; D2: $0d40; D3: $4e2d; D4: ($99, $fb, $c9, $1e, $96, $42, $0b, $e7)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM NO_SUB_KEY_GUID}

  MFPKEY_BASSMGMT_INVERT_SUB            :  PROPERTYKEY = (fmtid: (D1: $b1103003; D2: $c191; D3: $4275; D4: ($9f, $a0, $8c, $28, $2c, $72, $4b, $ce)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_BASSMGMT_INVERT_SUB}

  INVERTED_SUB_KEY_GUID                 :  PROPERTYKEY = (fmtid: (D1: $b1103003; D2: $c191; D3: $4275; D4: ($9f, $a0, $8c, $28, $2c, $72, $4b, $ce)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM INVERTED_SUB_KEY_GUID}

  MFPKEY_CORR_HEADPHONE                 :  PROPERTYKEY = (fmtid: (D1: $445f3559; D2: $b43f; D3: $4b67; D4: ($b0, $f8, $32, $b6, $7c, $f9, $4b, $48)); pid: PID_FIRST_USABLE + 0);
  {$EXTERNALSYM MFPKEY_CORR_HEADPHONE}

  MFPKEY_CORR_BASS_MANAGEMENT_MODE      :  PROPERTYKEY = (fmtid: (D1: $1864a4e0; D2: $efc1; D3: $45e6; D4: ($a6, $75, $57, $86, $cb, $f3, $b9, $f0)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_CORR_BASS_MANAGEMENT_MODE}

  BASSMGMT_MODE_KEY_GUID                :  PROPERTYKEY = (fmtid: (D1: $1864a4e0; D2: $efc1; D3: $45e6; D4: ($a6, $75, $57, $86, $cb, $f3, $b9, $f0)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM BASSMGMT_MODE_KEY_GUID}

  MFPKEY_CORR_MULTICHANNEL_MODE         :  PROPERTYKEY = (fmtid: (D1: $1b5c2483; D2: $0839; D3: $4523; D4: ($ba, $87, $95, $f8, $9d, $27, $bd, $8c)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_CORR_MULTICHANNEL_MODE}

  MULTICHANNEL_MODE_KEY_GUID            :  PROPERTYKEY = (fmtid: (D1: $1b5c2483; D2: $0839; D3: $4523; D4: ($ba, $87, $95, $f8, $9d, $27, $bd, $8c)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MULTICHANNEL_MODE_KEY_GUID}

  MFPKEY_CORR_LOUDNESS_EQUALIZATION_ON  :  PROPERTYKEY = (fmtid: (D1: $fc52a749; D2: $4be9; D3: $4510; D4: ($89, $6e, $96, $6b, $a6, $52, $59, $80)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_CORR_LOUDNESS_EQUALIZATION_ON}

  LEQ_ENABLED_KEY_GUID                  :  PROPERTYKEY = (fmtid: (D1: $fc52a749; D2: $4be9; D3: $4510; D4: ($89, $6e, $96, $6b, $a6, $52, $59, $80)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM LEQ_ENABLED_KEY_GUID}

  MFPKEY_CORR_ROOM_CORRECTION_ON        :  PROPERTYKEY = (fmtid: (D1: $01fb17e3; D2: $796c; D3: $4451; D4: ($81, $63, $68, $cd, $c1, $32, $1a, $60)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_CORR_ROOM_CORRECTION_ON}

  ROOM_CORRECTION_ENABLED_KEY_GUID      :  PROPERTYKEY = (fmtid: (D1: $01fb17e3; D2: $796c; D3: $4451; D4: ($81, $63, $68, $cd, $c1, $32, $1a, $60)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM ROOM_CORRECTION_ENABLED_KEY_GUID}

  MFPKEY_CORR_SPKRMASK                  :  PROPERTYKEY = (fmtid: (D1: $d328d8fb; D2: $d49f; D3: $4aa9; D4: ($b7, $21, $e1, $71, $e9, $3a, $d5, $63)); pid: PID_FIRST_USABLE + 5);
  {$EXTERNALSYM MFPKEY_CORR_SPKRMASK}

  MFPKEY_CORR_NORMALIZATION_GAIN        :  PROPERTYKEY = (fmtid: (D1: $d61b266c; D2: $5aee; D3: $456b; D4: ($84, $24, $72, $25, $47, $7d, $ae, $77)); pid: PID_FIRST_USABLE + 0);
  {$EXTERNALSYM MFPKEY_CORR_NORMALIZATION_GAIN}

  MFPKEY_BASS_BOOST_AMOUNT              :  PROPERTYKEY = (fmtid: (D1: $ae7f0b2a; D2: $96fc; D3: $493a; D4: ($92, $47, $a0, $19, $f1, $f7, $01, $e1)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_BASS_BOOST_AMOUNT}

  BOOST_LEVEL_KEY_GUID                  :  PROPERTYKEY = (fmtid: (D1: $ae7f0b2a; D2: $96fc; D3: $493a; D4: ($92, $47, $a0, $19, $f1, $f7, $01, $e1)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM BOOST_LEVEL_KEY_GUID}

  MFPKEY_LOUDNESS_EQUALIZATION_RELEASE  :  PROPERTYKEY = (fmtid: (D1: $9c00eeed; D2: $edce; D3: $4cd8; D4: ($ae, $08, $cb, $05, $e8, $ef, $57, $a0)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_LOUDNESS_EQUALIZATION_RELEASE}

  LEQ_RELEASE_KEY_GUID                  :  PROPERTYKEY = (fmtid: (D1: $9c00eeed; D2: $edce; D3: $4cd8; D4: ($ae, $08, $cb, $05, $e8, $ef, $57, $a0)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM LEQ_RELEASE_KEY_GUID}

  PKEY_SYSFXUI_HIDE_MASK                :  PROPERTYKEY = (fmtid: (D1: $cb9c6bce; D2: $7a25; D3: $47aa; D4: ($b2, $be, $6a, $d8, $44, $31, $ed, $de)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM PKEY_SYSFXUI_HIDE_MASK}


  SYSFXUI_DONOTSHOW_LOUDNESSEQUALIZATION    = $01;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_LOUDNESSEQUALIZATION}
  SYSFXUI_DONOTSHOW_ROOMCORRECTION          = $02;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_ROOMCORRECTION}
  SYSFXUI_DONOTSHOW_BASSMANAGEMENT          = $04;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_BASSMANAGEMENT}
  SYSFXUI_DONOTSHOW_BASSBOOST               = $08;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_BASSBOOST}
  SYSFXUI_DONOTSHOW_HEADPHONEVIRTUALIZATION = $10;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_HEADPHONEVIRTUALIZATION}
  SYSFXUI_DONOTSHOW_VIRTUALSURROUND         = $20;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_VIRTUALSURROUND}
  SYSFXUI_DONOTSHOW_SPEAKERFILLING          = $40;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_SPEAKERFILLING}
  SYSFXUI_DONOTSHOW_CHANNELPHANTOMING       = $80;
  {$EXTERNALSYM SYSFXUI_DONOTSHOW_CHANNELPHANTOMING}


  MFPKEY_AUVRHP_SKIPHRTFREVERB           :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $01);
  {$EXTERNALSYM MFPKEY_AUVRHP_SKIPHRTFREVERB}
  MFPKEY_AUVRHP_SKIPPOSTREVERB           :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $02);
  {$EXTERNALSYM MFPKEY_AUVRHP_SKIPPOSTREVERB}
  MFPKEY_AUVRHP_ROOMMODEL                :  PROPERTYKEY = (fmtid: (D1: $73ae880e; D2: $8258; D3: $4e57; D4: ($b8, $5f, $7d, $aa, $6b, $7d, $5e, $f0)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_AUVRHP_ROOMMODEL}

  VIRTUALIZATION_MODE_KEY_GUID           :  PROPERTYKEY = (fmtid: (D1: $73ae880e; D2: $8258; D3: $4e57; D4: ($b8, $5f, $7d, $aa, $6b, $7d, $5e, $f0)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM VIRTUALIZATION_MODE_KEY_GUID}

  MFPKEY_AUVRHP_LFWEIGHT                 :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $04);
  {$EXTERNALSYM MFPKEY_AUVRHP_LFWEIGHT}
  MFPKEY_AUVRHP_DOHRTFREVERB             :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $05);
  {$EXTERNALSYM MFPKEY_AUVRHP_DOHRTFREVERB}
  MFPKEY_AUVRHP_DOPOSTREVERB             :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $06);
  {$EXTERNALSYM MFPKEY_AUVRHP_DOPOSTREVERB}
  MFPKEY_AUVRHP_POSTREVERB_START         :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $07);
  {$EXTERNALSYM MFPKEY_AUVRHP_POSTREVERB_START}
  MFPKEY_AUVRHP_POSTREVERB_LEN           :  PROPERTYKEY = (fmtid: (D1: $30bbfebf; D2: $24b4; D3: $4198; D4: ($89, $ba, $ad, $11, $a2, $ac, $d6, $01)); pid: $08);
  {$EXTERNALSYM MFPKEY_AUVRHP_POSTREVERB_LEN}

  MFPKEY_WMRESAMP_FILTERQUALITY          :  PROPERTYKEY = (fmtid: (D1: $af1adc73; D2: $a210; D3: $4b05; D4: ($96, $6e, $54, $91, $cf, $f4, $8b, $1d)); pid: $01);
  {$EXTERNALSYM MFPKEY_WMRESAMP_FILTERQUALITY}
  MFPKEY_WMRESAMP_CHANNELMTX             :  PROPERTYKEY = (fmtid: (D1: $af1adc73; D2: $a210; D3: $4b05; D4: ($96, $6e, $54, $91, $cf, $f4, $8b, $1d)); pid: $02);
  {$EXTERNALSYM MFPKEY_WMRESAMP_CHANNELMTX}
  MFPKEY_WMRESAMP_LOWPASS_BANDWIDTH      :  PROPERTYKEY = (fmtid: (D1: $af1adc73; D2: $a210; D3: $4b05; D4: ($96, $6e, $54, $91, $cf, $f4, $8b, $1d)); pid: $03);
  {$EXTERNALSYM MFPKEY_WMRESAMP_LOWPASS_BANDWIDTH}

  MFPKEY_WMAENC_AVGBYTESPERSEC           :  PROPERTYKEY = (fmtid: (D1: $11caf780; D2: $921b; D3: $42ef; D4: ($b7, $55, $f3, $a0, $53, $ea, $1a, $41)); pid: $00);
  {$EXTERNALSYM MFPKEY_WMAENC_AVGBYTESPERSEC}

  MFPKEY_WMAENC_ORIGWAVEFORMAT           :  PROPERTYKEY = (fmtid: (D1: $f5c760a2; D2: $3635; D3: $48e1; D4: ($8f, $bd, $0e, $49, $81, $24, $e0, $a2)); pid: $00);
  {$EXTERNALSYM MFPKEY_WMAENC_ORIGWAVEFORMAT}

  MFPKEY_PEAKCONSTRAINED                 :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00);
  {$EXTERNALSYM MFPKEY_PEAKCONSTRAINED}

  MFPKEY_STAT_RAVG                       :  PROPERTYKEY = (fmtid: (D1: $23a0e3b5; D2: $fc62; D3: $4ab8; D4: ($b7, $7c, $6e, $0c, $28, $ab, $30, $16)); pid: $00);
  {$EXTERNALSYM MFPKEY_STAT_RAVG}
  MFPKEY_STAT_BAVG                       :  PROPERTYKEY = (fmtid: (D1: $036f6b60; D2: $ad43; D3: $485c; D4: ($86, $c6, $21, $a6, $db, $2c, $1b, $a3)); pid: $00);
  {$EXTERNALSYM MFPKEY_STAT_BAVG}
  MFPKEY_STAT_RMAX                       :  PROPERTYKEY = (fmtid: (D1: $82ff7c67; D2: $6554; D3: $4749; D4: ($a3, $2b, $36, $90, $dd, $1a, $e8, $de)); pid: $00);
  {$EXTERNALSYM MFPKEY_STAT_RMAX}
  MFPKEY_STAT_BMAX                       :  PROPERTYKEY = (fmtid: (D1: $cd95e5b7; D2: $9143; D3: $47fb; D4: ($a9, $d2, $9d, $b7, $5f, $2e, $74, $be)); pid: $00);
  {$EXTERNALSYM MFPKEY_STAT_BMAX}
  MFPKEY_CONSTRAINENCLATENCY             :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 1);
  {$EXTERNALSYM MFPKEY_CONSTRAINENCLATENCY}
  MFPKEY_CONSTRAINDECLATENCY             :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 2);
  {$EXTERNALSYM MFPKEY_CONSTRAINDECLATENCY}
  MFPKEY_CONSTRAINENCCOMPLEXITY          :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 3);
  {$EXTERNALSYM MFPKEY_CONSTRAINENCCOMPLEXITY}
  MFPKEY_MAXENCLATENCYMS                 :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 4);
  {$EXTERNALSYM MFPKEY_MAXENCLATENCYMS}
  MFPKEY_MAXDECLATENCYMS                 :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 5);
  {$EXTERNALSYM MFPKEY_MAXDECLATENCYMS}
  MFPKEY_ENCCOMPLEXITY                   :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 6);
  {$EXTERNALSYM MFPKEY_ENCCOMPLEXITY}
  MFPKEY_CHECKDATACONSISTENCY2P          :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 7);
  {$EXTERNALSYM MFPKEY_CHECKDATACONSISTENCY2P}
  MFPKEY_AVGCONSTRAINED                  :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 8);
  {$EXTERNALSYM MFPKEY_AVGCONSTRAINED}

  MFPKEY_ENHANCED_WMA                    :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 20);
  {$EXTERNALSYM MFPKEY_ENHANCED_WMA}
  MFPKEY_REQUESTING_A_FRAMESIZE          :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 21);
  {$EXTERNALSYM MFPKEY_REQUESTING_A_FRAMESIZE}
  MFPKEY_PREFERRED_FRAMESIZE             :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 22);
  {$EXTERNALSYM MFPKEY_PREFERRED_FRAMESIZE}
  MFPKEY_WMA_ELEMENTARY_STREAM           :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 23);
  {$EXTERNALSYM MFPKEY_WMA_ELEMENTARY_STREAM}
  MFPKEY_MOST_RECENTLY_ENUMERATED_VBRQUALITY :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 24);
  {$EXTERNALSYM MFPKEY_MOST_RECENTLY_ENUMERATED_VBRQUALITY}
  MFPKEY_DESIRED_VBRQUALITY              :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 25);
  {$EXTERNALSYM MFPKEY_DESIRED_VBRQUALITY}
  MFPKEY_CONSTRAIN_ENUMERATED_VBRQUALITY :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 26);
  {$EXTERNALSYM MFPKEY_CONSTRAIN_ENUMERATED_VBRQUALITY}

  MFPKEY_WMAENC_GENERATE_DRC_PARAMS      :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 28);
  {$EXTERNALSYM MFPKEY_WMAENC_GENERATE_DRC_PARAMS}
  MFPKEY_WMAENC_BUFFERLESSCBR            :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 29);
  {$EXTERNALSYM MFPKEY_WMAENC_BUFFERLESSCBR}
  MFPKEY_WMAENC_RTSPDIF                  :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 30);
  {$EXTERNALSYM MFPKEY_WMAENC_RTSPDIF}

  MFPKEY_DYN_VBR_RAVG                    :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 9);
  {$EXTERNALSYM MFPKEY_DYN_VBR_RAVG}
  MFPKEY_DYN_BANDTRUNCATION              :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 10);
  {$EXTERNALSYM MFPKEY_DYN_BANDTRUNCATION}
  MFPKEY_DYN_BANDTRUNC_QFLOOR            :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 11);
  {$EXTERNALSYM MFPKEY_DYN_BANDTRUNC_QFLOOR}
  MFPKEY_DYN_BANDTRUNC_QCEIL             :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 12);
  {$EXTERNALSYM MFPKEY_DYN_BANDTRUNC_QCEIL}
  MFPKEY_DYN_BANDTRUNC_BWFLOOR           :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 13);
  {$EXTERNALSYM MFPKEY_DYN_BANDTRUNC_BWFLOOR}
  MFPKEY_DYN_BANDTRUNC_BWCEIL            :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 14);
  {$EXTERNALSYM MFPKEY_DYN_BANDTRUNC_BWCEIL}
  MFPKEY_DYN_SIMPLEMASK                  :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 15);
  {$EXTERNALSYM MFPKEY_DYN_SIMPLEMASK}
  MFPKEY_DYN_STEREO_PREPROC              :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 16);
  {$EXTERNALSYM MFPKEY_DYN_STEREO_PREPROC}

  MFPKEY_DYN_VBR_BAVG                    :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 18);
  {$EXTERNALSYM MFPKEY_DYN_VBR_BAVG}
  MFPKEY_DYN_ALLOW_NOISESUB              :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 19);
  {$EXTERNALSYM MFPKEY_DYN_ALLOW_NOISESUB}

  MFPKEY_DYN_ALLOW_PCMRANGELIMITING      :  PROPERTYKEY = (fmtid: (D1: $6dbdf03b; D2: $b05c; D3: $4a03; D4: ($8e, $c1, $bb, $e6, $3d, $b1, $0c, $b4)); pid: $00 + 27);
  {$EXTERNALSYM MFPKEY_DYN_ALLOW_PCMRANGELIMITING}

  MFPKEY_WMADEC_HIRESOUTPUT              :  PROPERTYKEY = (fmtid: (D1: $8d3fe592; D2: $eecc; D3: $4f4e; D4: ($9a, $ff, $5a, $f1, $67, $9d, $38, $d2)); pid: $00);
  {$EXTERNALSYM MFPKEY_WMADEC_HIRESOUTPUT}
  MFPKEY_WMADEC_SPKRCFG                  :  PROPERTYKEY = (fmtid: (D1: $8fff67be; D2: $977f; D3: $41dc; D4: ($8f, $af, $23, $ba, $c9, $a6, $df, $73)); pid: $00);
  {$EXTERNALSYM MFPKEY_WMADEC_SPKRCFG}
  MFPKEY_WMADEC_FOLDDOWN_MATRIX          :  PROPERTYKEY = (fmtid: (D1: $51647e9b; D2: $6a7f; D3: $4739; D4: ($9e, $0b, $29, $4b, $27, $89, $69, $eb)); pid: $00);
  {$EXTERNALSYM MFPKEY_WMADEC_FOLDDOWN_MATRIX}

  MFPKEY_WMADEC_DRCMODE                  :  PROPERTYKEY = (fmtid: (D1: $7b613713; D2: $3d38; D3: $4cda; D4: ($aa, $61, $04, $78, $b1, $bc, $fc, $42)); pid: $00);
  {$EXTERNALSYM MFPKEY_WMADEC_DRCMODE}
  MFPKEY_WMADRC_AVGTARGET                :  PROPERTYKEY = (fmtid: (D1: $7b613713; D2: $3d38; D3: $4cda; D4: ($aa, $61, $04, $78, $b1, $bc, $fc, $42)); pid: $01);
  {$EXTERNALSYM MFPKEY_WMADRC_AVGTARGET}
  MFPKEY_WMADRC_PEAKTARGET               :  PROPERTYKEY = (fmtid: (D1: $7b613713; D2: $3d38; D3: $4cda; D4: ($aa, $61, $04, $78, $b1, $bc, $fc, $42)); pid: $02);
  {$EXTERNALSYM MFPKEY_WMADRC_PEAKTARGET}
  MFPKEY_WMADRC_AVGREF                   :  PROPERTYKEY = (fmtid: (D1: $7b613713; D2: $3d38; D3: $4cda; D4: ($aa, $61, $04, $78, $b1, $bc, $fc, $42)); pid: $03);
  {$EXTERNALSYM MFPKEY_WMADRC_AVGREF}
  MFPKEY_WMADRC_PEAKREF                  :  PROPERTYKEY = (fmtid: (D1: $7b613713; D2: $3d38; D3: $4cda; D4: ($aa, $61, $04, $78, $b1, $bc, $fc, $42)); pid: $04);
  {$EXTERNALSYM MFPKEY_WMADRC_PEAKREF}
  MFPKEY_WMADEC_LTRTOUTPUT               :  PROPERTYKEY = (fmtid: (D1: $7b613713; D2: $3d38; D3: $4cda; D4: ($aa, $61, $04, $78, $b1, $bc, $fc, $42)); pid: $05);
  {$EXTERNALSYM MFPKEY_WMADEC_LTRTOUTPUT}


  MFPKEY_WMAVOICE_ENC_MusicSpeechClassMode :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_MusicSpeechClassMode}
  MFPKEY_WMAVOICE_ENC_BufferWindow         :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 1);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_BufferWindow}
  MFPKEY_WMAVOICE_ENC_DecoderDelay         :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 2);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_DecoderDelay}
  MFPKEY_WMAVOICE_ENC_EDL                  :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 3);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_EDL}

  MFPKEY_WMAVOICE_ENC_RT_VariableRate      :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 4);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_RT_VariableRate}
  MFPKEY_WMAVOICE_ENC_RT_BandWidth         :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 5);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_RT_BandWidth}
  MFPKEY_WMAVOICE_ENC_RT_PacketLossMode    :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 6);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_RT_PacketLossMode}
  MFPKEY_WMAVOICE_ENC_RT_MinBufferSize     :  PROPERTYKEY = (fmtid: (D1: $d9c8f5fe; D2: $8682; D3: $4347; D4: ($85, $7, $60, $a3, $f5, $1f, $33, $f1)); pid: PID_FIRST_USABLE + 7);
  {$EXTERNALSYM MFPKEY_WMAVOICE_ENC_RT_MinBufferSize}

  MFPKEY_WMAVOICE_DEC_RT_JitterControl     :  PROPERTYKEY = (fmtid: (D1: $165f69b; D2: $80a1; D3: $4ef8; D4: ($a4, $a9, $ad, $a3, $b8, $a6, $89, $dd)); pid: PID_FIRST_USABLE);
  {$EXTERNALSYM MFPKEY_WMAVOICE_DEC_RT_JitterControl}
  MFPKEY_WMAVOICE_DEC_RT_JitterMode        :  PROPERTYKEY = (fmtid: (D1: $165f69b; D2: $80a1; D3: $4ef8; D4: ($a4, $a9, $ad, $a3, $b8, $a6, $89, $dd)); pid: PID_FIRST_USABLE + 1);
  {$EXTERNALSYM MFPKEY_WMAVOICE_DEC_RT_JitterMode}
  MFPKEY_WMAVOICE_DEC_RT_PacketLossMode    :  PROPERTYKEY = (fmtid: (D1: $165f69b; D2: $80a1; D3: $4ef8; D4: ($a4, $a9, $ad, $a3, $b8, $a6, $89, $dd)); pid: PID_FIRST_USABLE + 2);
  {$EXTERNALSYM MFPKEY_WMAVOICE_DEC_RT_PacketLossMode}


  MFPKEY_Decoder_MaxNumPCMSamplesWithPaddedSilence  :  PROPERTYKEY = (fmtid: (D1: $c678ba85; D2: $1212; D3: $43da; D4: (90, $c3, $e7, $48, $b9, $24, $49, $ec)); pid: $00);
  {$EXTERNALSYM MFPKEY_Decoder_MaxNumPCMSamplesWithPaddedSilence}

  MFPKEY_WMAAECMA_SYSTEM_MODE          :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 0);
  {$EXTERNALSYM MFPKEY_WMAAECMA_SYSTEM_MODE}
  MFPKEY_WMAAECMA_DMO_SOURCE_MODE      :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 1);
  {$EXTERNALSYM MFPKEY_WMAAECMA_DMO_SOURCE_MODE}
  MFPKEY_WMAAECMA_DEVICE_INDEXES       :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 2);
  {$EXTERNALSYM MFPKEY_WMAAECMA_DEVICE_INDEXES}
  MFPKEY_WMAAECMA_FEATURE_MODE         :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 3);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATURE_MODE}
  MFPKEY_WMAAECMA_FEATR_FRAME_SIZE     :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 4);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_FRAME_SIZE}
  MFPKEY_WMAAECMA_FEATR_ECHO_LENGTH    :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 5);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_ECHO_LENGTH}
  MFPKEY_WMAAECMA_FEATR_NS             :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 6);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_NS}
  MFPKEY_WMAAECMA_FEATR_AGC            :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 7);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_AGC}
  MFPKEY_WMAAECMA_FEATR_AES            :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 8);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_AES}
  MFPKEY_WMAAECMA_FEATR_VAD            :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 9);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_VAD}
  MFPKEY_WMAAECMA_FEATR_CENTER_CLIP    :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 10);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_CENTER_CLIP}
  MFPKEY_WMAAECMA_FEATR_NOISE_FILL     :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 11);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_NOISE_FILL}
  MFPKEY_WMAAECMA_RETRIEVE_TS_STATS    :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 12);
  {$EXTERNALSYM MFPKEY_WMAAECMA_RETRIEVE_TS_STATS}
  MFPKEY_WMAAECMA_QUALITY_METRICS      :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 13);
  {$EXTERNALSYM MFPKEY_WMAAECMA_QUALITY_METRICS}
  MFPKEY_WMAAECMA_MICARRAY_DESCPTR     :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 14);
  {$EXTERNALSYM MFPKEY_WMAAECMA_MICARRAY_DESCPTR}
  MFPKEY_WMAAECMA_DEVICEPAIR_GUID      :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 15);
  {$EXTERNALSYM MFPKEY_WMAAECMA_DEVICEPAIR_GUID}
  MFPKEY_WMAAECMA_FEATR_MICARR_MODE    :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 16);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_MICARR_MODE}
  MFPKEY_WMAAECMA_FEATR_MICARR_BEAM    :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 17);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_MICARR_BEAM}
  MFPKEY_WMAAECMA_FEATR_MICARR_PREPROC :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 18);
  {$EXTERNALSYM MFPKEY_WMAAECMA_FEATR_MICARR_PREPROC}
  MFPKEY_WMAAECMA_MIC_GAIN_BOUNDER     :  PROPERTYKEY = (fmtid: (D1: $6f52c567; D2: $360; D3: $4bd2; D4: ($96, $17, $cc, $bf, $14, $21, $c9, $39)); pid: PID_FIRST_USABLE + 19);
  {$EXTERNALSYM MFPKEY_WMAAECMA_MIC_GAIN_BOUNDER}

  MFPKEY_COLOR_BRIGHTNESS              :  PROPERTYKEY = (fmtid: (D1: $174fb0ec; D2: $2695; D3: $476c; D4: ($88, $aa, $d2, $b4, $1c, $e7, $5e, $67)); pid: $01);
  {$EXTERNALSYM MFPKEY_COLOR_BRIGHTNESS}
  MFPKEY_COLOR_CONTRAST                :  PROPERTYKEY = (fmtid: (D1: $174fb0ec; D2: $2695; D3: $476c; D4: ($88, $aa, $d2, $b4, $1c, $e7, $5e, $67)); pid: $02);
  {$EXTERNALSYM MFPKEY_COLOR_CONTRAST}
  MFPKEY_COLOR_HUE                     :  PROPERTYKEY = (fmtid: (D1: $174fb0ec; D2: $2695; D3: $476c; D4: ($88, $aa, $d2, $b4, $1c, $e7, $5e, $67)); pid: $03);
  {$EXTERNALSYM MFPKEY_COLOR_HUE}
  MFPKEY_COLOR_SATURATION              :  PROPERTYKEY = (fmtid: (D1: $174fb0ec; D2: $2695; D3: $476c; D4: ($88, $aa, $d2, $b4, $1c, $e7, $5e, $67)); pid: $04);
  {$EXTERNALSYM MFPKEY_COLOR_SATURATION}

  MFPKEY_COLORLEGALIZER_COMPLEXITY        :  PROPERTYKEY = (fmtid: (D1: $add0e6c1; D2: $cc30; D3: $475d; D4: ($9e, $5a, $f1, $b1, $9f, $58, $7b, $e0)); pid: $01);
  {$EXTERNALSYM MFPKEY_COLORLEGALIZER_COMPLEXITY}
  MFPKEY_COLORLEGALIZER_COMPLEXITYEX      :  PROPERTYKEY = (fmtid: (D1: $add0e6c1; D2: $cc30; D3: $475d; D4: ($9e, $5a, $f1, $b1, $9f, $58, $7b, $e0)); pid: $02);
  {$EXTERNALSYM MFPKEY_COLORLEGALIZER_COMPLEXITYEX}
  MFPKEY_COLORLEGALIZER_COMPLEXITYMAX     :  PROPERTYKEY = (fmtid: (D1: $add0e6c1; D2: $cc30; D3: $475d; D4: ($9e, $5a, $f1, $b1, $9f, $58, $7b, $e0)); pid: $03);
  {$EXTERNALSYM MFPKEY_COLORLEGALIZER_COMPLEXITYMAX}
  MFPKEY_COLORLEGALIZER_COMPLEXITYLIVE    :  PROPERTYKEY = (fmtid: (D1: $add0e6c1; D2: $cc30; D3: $475d; D4: ($9e, $5a, $f1, $b1, $9f, $58, $7b, $e0)); pid: $04);
  {$EXTERNALSYM MFPKEY_COLORLEGALIZER_COMPLEXITYLIVE}
  MFPKEY_COLORLEGALIZER_COMPLEXITYOFFLINE :  PROPERTYKEY = (fmtid: (D1: $add0e6c1; D2: $cc30; D3: $475d; D4: ($9e, $5a, $f1, $b1, $9f, $58, $7b, $e0)); pid: $05);
  {$EXTERNALSYM MFPKEY_COLORLEGALIZER_COMPLEXITYOFFLINE}
  MFPKEY_COLORLEGALIZER_bSVideo           :  PROPERTYKEY = (fmtid: (D1: $add0e6c1; D2: $cc30; D3: $475d; D4: ($9e, $5a, $f1, $b1, $9f, $58, $7b, $e0)); pid: $06);
  {$EXTERNALSYM MFPKEY_COLORLEGALIZER_bSVideo}

  MFPKEY_CONV_INPUTFRAMERATE  :  PROPERTYKEY = (fmtid: (D1: $52f8d29b; D2: $2e76; D3: $43f7; D4: ($a4, $f6, $17, $17, $90, $4e, $35, $df)); pid: $01);
  {$EXTERNALSYM MFPKEY_CONV_INPUTFRAMERATE}
  MFPKEY_CONV_OUTPUTFRAMERATE :  PROPERTYKEY = (fmtid: (D1: $52f8d29b; D2: $2e76; D3: $43f7; D4: ($a4, $f6, $17, $17, $90, $4e, $35, $df)); pid: $02);
  {$EXTERNALSYM MFPKEY_CONV_OUTPUTFRAMERATE}
  MFPKEY_CONV_REVERSEPLAYBACK :  PROPERTYKEY = (fmtid: (D1: $52f8d29b; D2: $2e76; D3: $43f7; D4: ($a4, $f6, $17, $17, $90, $4e, $35, $df)); pid: $03);
  {$EXTERNALSYM MFPKEY_CONV_REVERSEPLAYBACK}
  MFPKEY_SMPTE_MASKNUM        :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $01);
  {$EXTERNALSYM MFPKEY_SMPTE_MASKNUM}
  MFPKEY_SMPTE_OFFSETX        :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $02);
  {$EXTERNALSYM MFPKEY_SMPTE_OFFSETX}
  MFPKEY_SMPTE_OFFSETY        :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $03);
  {$EXTERNALSYM MFPKEY_SMPTE_OFFSETY}
  MFPKEY_SMPTE_REPLICATEX     :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $04);
  {$EXTERNALSYM MFPKEY_SMPTE_REPLICATEX}
  MFPKEY_SMPTE_REPLICATEY     :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $05);
  {$EXTERNALSYM MFPKEY_SMPTE_REPLICATEY}
  MFPKEY_SMPTE_REVERSE        :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $06);
  {$EXTERNALSYM MFPKEY_SMPTE_REVERSE}
  MFPKEY_SMPTE_BORDERSOFTNESS :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $07);
  {$EXTERNALSYM MFPKEY_SMPTE_BORDERSOFTNESS}
  MFPKEY_SMPTE_BORDERWIDTH    :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $08);
  {$EXTERNALSYM MFPKEY_SMPTE_BORDERWIDTH}
  MFPKEY_SMPTE_BORDERCOLOR    :  PROPERTYKEY = (fmtid: (D1: $427ce859; D2: $d55c; D3: $4f8e; D4: ($b0, $0e, $9c, $df, $76, $15, $48, $a6)); pid: $09);
  {$EXTERNALSYM MFPKEY_SMPTE_BORDERCOLOR}

  MFPKEY_DENOISE_FILTER      :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $01);
  {$EXTERNALSYM MFPKEY_DENOISE_FILTER}
  MFPKEY_DENOISE_CACHEFRAMES :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $02);
  {$EXTERNALSYM MFPKEY_DENOISE_CACHEFRAMES}
  MFPKEY_DENOISE_PROCFRAMES  :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $03);
  {$EXTERNALSYM MFPKEY_DENOISE_PROCFRAMES}
  MFPKEY_DENOISE_CAUSAL      :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $04);
  {$EXTERNALSYM MFPKEY_DENOISE_CAUSAL}
  MFPKEY_DENOISE_ITERATIVE   :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $05);
  {$EXTERNALSYM MFPKEY_DENOISE_ITERATIVE}
  MFPKEY_DENOISE_PARA1       :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $06);
  {$EXTERNALSYM MFPKEY_DENOISE_PARA1}
  MFPKEY_DENOISE_PARA2       :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $07);
  {$EXTERNALSYM MFPKEY_DENOISE_PARA2}
  MFPKEY_DENOISE_PARA3       :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $08);
  {$EXTERNALSYM MFPKEY_DENOISE_PARA3}
  MFPKEY_DENOISE_PARA4       :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $09);
  {$EXTERNALSYM MFPKEY_DENOISE_PARA4}
  MFPKEY_DENOISE_PARA5       :  PROPERTYKEY = (fmtid: (D1: $7213c6ef; D2: $cdd4; D3: $4d09; D4: ($a8, $9e, $f3, $eb, $eb, $e5, $f5, $65)); pid: $10);
  {$EXTERNALSYM MFPKEY_DENOISE_PARA5}

  MFPKEY_CLUSTERDETECTOR_MAXCLUSTERS        :  PROPERTYKEY = (fmtid: (D1: $b79a666d; D2: $8a9d; D3: $463c; D4: ($9d, $97, $e1, $b1, $0, $45, $c1, $3a)); pid: $01);
  {$EXTERNALSYM MFPKEY_CLUSTERDETECTOR_MAXCLUSTERS}
  MFPKEY_CLUSTERDETECTOR_MINCLUSTERDURATION :  PROPERTYKEY = (fmtid: (D1: $b79a666d; D2: $8a9d; D3: $463c; D4: ($9d, $97, $e1, $b1, $0, $45, $c1, $3a)); pid: $02);
  {$EXTERNALSYM MFPKEY_CLUSTERDETECTOR_MINCLUSTERDURATION}
  MFPKEY_CLUSTERDETECTOR_MAXCLUSTERDURATION :  PROPERTYKEY = (fmtid: (D1: $b79a666d; D2: $8a9d; D3: $463c; D4: ($9d, $97, $e1, $b1, $0, $45, $c1, $3a)); pid: $03);
  {$EXTERNALSYM MFPKEY_CLUSTERDETECTOR_MAXCLUSTERDURATION}

  MFPKEY_FACEDETECTOR_SKIPFRAMES            :  PROPERTYKEY = (fmtid: (D1: $e1a124a6; D2: $4fa8; D3: $4ba5; D4: ($a2, $d8, $dc, $34, $53, $6f, $74, $26)); pid: $01);
  {$EXTERNALSYM MFPKEY_FACEDETECTOR_SKIPFRAMES}

  MFPKEY_SHOTDETECTOR_TYPE                 :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $01);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_TYPE}
  MFPKEY_SHOTDETECTOR_CLASSIFICATIONMETHOD :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $02);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_CLASSIFICATIONMETHOD}
  MFPKEY_SHOTDETECTOR_GLOBALTHRESHOLD      :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $03);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_GLOBALTHRESHOLD}
  MFPKEY_SHOTDETECTOR_ADAPTIVETHRESHOLD    :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $04);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_ADAPTIVETHRESHOLD}
  MFPKEY_SHOTDETECTOR_ADAPTIVEWINDOWSIZE   :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $05);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_ADAPTIVEWINDOWSIZE}
  MFPKEY_SHOTDETECTOR_MINCLIPWEIGHT        :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $06);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_MINCLIPWEIGHT}
  MFPKEY_SHOTDETECTOR_MINCLIPDURATION      :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $07);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_MINCLIPDURATION}
  MFPKEY_SHOTDETECTOR_MAXCLIPDURATION      :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $08);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_MAXCLIPDURATION}
  MFPKEY_SHOTDETECTOR_MAXCLIPSPERHOUR      :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $09);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_MAXCLIPSPERHOUR}
  MFPKEY_SHOTDETECTOR_PRESERVEMONOCLIPS    :  PROPERTYKEY = (fmtid: (D1: $7bcc7b0f; D2: $dedf; D3: $4a68; D4: ($96, $a2, $fc, $e0, $19, $ed, $95, $6f)); pid: $0a);
  {$EXTERNALSYM MFPKEY_SHOTDETECTOR_PRESERVEMONOCLIPS}

  MFPKEY_THUMBNAILGENERATOR_SEARCHWINDOWSIZE  :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $01);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_SEARCHWINDOWSIZE}
  MFPKEY_THUMBNAILGENERATOR_MINCOLORENTROPY   :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $02);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_MINCOLORENTROPY}
  MFPKEY_THUMBNAILGENERATOR_MAXMOTIONACTIVITY :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $03);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_MAXMOTIONACTIVITY}
  MFPKEY_THUMBNAILGENERATOR_THUMBNAILWIDTH    :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $04);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_THUMBNAILWIDTH}
  MFPKEY_THUMBNAILGENERATOR_THUMBNAILHEIGHT   :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $05);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_THUMBNAILHEIGHT}
  MFPKEY_THUMBNAILGENERATOR_THUMBNAILREADY    :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $06);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_THUMBNAILREADY}
  MFPKEY_THUMBNAILGENERATOR_THUMBNAILFILENAME :  PROPERTYKEY = (fmtid: (D1: $d9d7473f; D2: $7d68; D3: $4226; D4: ($98, $5c, $31, $85, $2c, $4c, $9e, $74)); pid: $ff);
  {$EXTERNALSYM MFPKEY_THUMBNAILGENERATOR_THUMBNAILFILENAME}

  MFPKEY_TOCGENERATOR_TOCREADY              :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $01);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_TOCREADY}
  MFPKEY_TOCGENERATOR_TOCOBJECT             :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $02);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_TOCOBJECT}
  MFPKEY_TOCGENERATOR_SHOTDETECTOR_ON       :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $03);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_SHOTDETECTOR_ON}
  MFPKEY_TOCGENERATOR_CLUSTERDETECTOR_ON    :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $04);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_CLUSTERDETECTOR_ON}
  MFPKEY_TOCGENERATOR_THUMBNAILGENERATOR_ON :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $05);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_THUMBNAILGENERATOR_ON}
  MFPKEY_TOCGENERATOR_FEATUREEXTRACTOR_ON   :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $06);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_FEATUREEXTRACTOR_ON}
  MFPKEY_TOCGENERATOR_FACEDETECTOR_ON       :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $07);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_FACEDETECTOR_ON}
  MFPKEY_TOCGENERATOR_USEENDSIGNAL          :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $fa);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_USEENDSIGNAL}
  MFPKEY_TOCGENERATOR_ENDSIGNAL             :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $fb);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_ENDSIGNAL}
  MFPKEY_TOCGENERATOR_ENDTIME               :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $fc);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_ENDTIME}
  MFPKEY_TOCGENERATOR_CURRENTTIME           :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $fd);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_CURRENTTIME}
  MFPKEY_TOCGENERATOR_PROCESSEDFRAMES       :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $fe);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_PROCESSEDFRAMES}
  MFPKEY_TOCGENERATOR_INDEXFILENAME         :  PROPERTYKEY = (fmtid: (D1: $7c109759; D2: $3c27; D3: $42ef; D4: ($a6, $a3, $ce, $de, $f7, $5a, $35, $e9)); pid: $ff);
  {$EXTERNALSYM MFPKEY_TOCGENERATOR_INDEXFILENAME}

  MFPKEY_DXVA_ENABLED               :  PROPERTYKEY = (fmtid: (D1: $58e28605; D2: $1d51; D3: $48ed; D4: ($a3, $eb, $0f, $9b, $af, $78, $5f, $bd)); pid: $01);
  {$EXTERNALSYM MFPKEY_DXVA_ENABLED}
  MFPKEY_FI_SUPPORTED               :  PROPERTYKEY = (fmtid: (D1: $5ce18788; D2: $b992; D3: $49a7; D4: ($a4, $f1, $60, $ea, $21, $ca, $a5, $5f)); pid: $02);
  {$EXTERNALSYM MFPKEY_FI_SUPPORTED}
  MFPKEY_FI_ENABLED                 :  PROPERTYKEY = (fmtid: (D1: $e020c4ca; D2: $3bdd; D3: $40ec; D4: ($bc, $f4, $40, $39, $b1, $45, $0e, $b8)); pid: $03);
  {$EXTERNALSYM MFPKEY_FI_ENABLED}

  MFPKEY_DECODER_DEINTERLACING      :  PROPERTYKEY = (fmtid: (D1: $00c6281f; D2: $4be6; D3: $4e44; D4: ($9e, $d8, $9e, $c5, $42, $23, $16, $e4)); pid: $04);
  {$EXTERNALSYM MFPKEY_DECODER_DEINTERLACING}
  MFPKEY_POSTPROCESSMODE            :  PROPERTYKEY = (fmtid: (D1: $00c6281f; D2: $4be6; D3: $4e44; D4: ($9e, $d8, $9e, $c5, $42, $23, $16, $e4)); pid: $05);
  {$EXTERNALSYM MFPKEY_POSTPROCESSMODE}
  MFPKEY_NUMTHREADSDEC              :  PROPERTYKEY = (fmtid: (D1: $00c6281f; D2: $4be6; D3: $4e44; D4: ($9e, $d8, $9e, $c5, $42, $23, $16, $e4)); pid: $06);
  {$EXTERNALSYM MFPKEY_NUMTHREADSDEC}
  MFPKEY_AVDecVideoSWPowerLevel     :  PROPERTYKEY = (fmtid: (D1: $fb5d2347; D2: $4dd8; D3: $4509; D4: ($ae, $d0, $db, $5f, $a9, $aa, $93, $f4)); pid: $08);
  {$EXTERNALSYM MFPKEY_AVDecVideoSWPowerLevel}

  MFPKEY_AVGFRAMERATE               :  PROPERTYKEY = (fmtid: (D1: $41d700d6; D2: $95b1; D3: $4e3f; D4: ($b7, $59, $2d, $66, $d8, $c7, $ad, $a2)); pid: $01);
  {$EXTERNALSYM MFPKEY_AVGFRAMERATE}
  MFPKEY_BUFFERFULLNESSINFIRSTBYTE  :  PROPERTYKEY = (fmtid: (D1: $b69dc3c5; D2: $64c4; D3: $4757; D4: ($99, $cb, $5d, $58, $0f, $d5, $65, $9e)); pid: $02);
  {$EXTERNALSYM MFPKEY_BUFFERFULLNESSINFIRSTBYTE}
  MFPKEY_PASSESRECOMMENDED          :  PROPERTYKEY = (fmtid: (D1: $38bdceea; D2: $393e; D3: $4f9a; D4: ($8d, $c3, $80, $2c, $c4, $05, $83, $8f)); pid: $03);
  {$EXTERNALSYM MFPKEY_PASSESRECOMMENDED}
  MFPKEY_DECODERCOMPLEXITYPROFILE   :  PROPERTYKEY = (fmtid: (D1: $c0d912d6; D2: $14da; D3: $4d31; D4: ($8d, $83, $d1, $08, $91, $5e, $8d, $77)); pid: $04);
  {$EXTERNALSYM MFPKEY_DECODERCOMPLEXITYPROFILE}
  MFPKEY_TOTALFRAMES                :  PROPERTYKEY = (fmtid: (D1: $ce5f1e3c; D2: $d3d4; D3: $4c3f; D4: ($88, $c8, $01, $e9, $89, $d9, $98, $d2)); pid: $05);
  {$EXTERNALSYM MFPKEY_TOTALFRAMES}
  MFPKEY_CODEDFRAMES                :  PROPERTYKEY = (fmtid: (D1: $62872b55; D2: $fe0e; D3: $4930; D4: ($a6, $d2, $cc, $aa, $37, $e8, $f5, $35)); pid: $06);
  {$EXTERNALSYM MFPKEY_CODEDFRAMES}
  MFPKEY_ZEROBYTEFRAMES             :  PROPERTYKEY = (fmtid: (D1: $8f04aba4; D2: $313d; D3: $40fb; D4: ($80, $31, $31, $51, $78, $13, $d9, $ef)); pid: $07);
  {$EXTERNALSYM MFPKEY_ZEROBYTEFRAMES}
  MFPKEY_ENDOFPASS                  :  PROPERTYKEY = (fmtid: (D1: $b2030f2a; D2: $8bbc; D3: $46f8; D4: ($a6, $4b, $a9, $8f, $f7, $fc, $f0, $2a)); pid: $08);
  {$EXTERNALSYM MFPKEY_ENDOFPASS}
  MFPKEY_DATARATE                   :  PROPERTYKEY = (fmtid: (D1: $e0db0807; D2: $8003; D3: $4880; D4: ($ac, $11, $61, $b7, $3f, $33, $dc, $60)); pid: $09);
  {$EXTERNALSYM MFPKEY_DATARATE}
  MFPKEY_KEYDIST                    :  PROPERTYKEY = (fmtid: (D1: $18d6f8c5; D2: $2416; D3: $4d7b; D4: ($90, $d7, $9f, $3f, $21, $e7, $52, $b4)); pid: $0a);
  {$EXTERNALSYM MFPKEY_KEYDIST}
  MFPKEY_CRISP                      :  PROPERTYKEY = (fmtid: (D1: $2985f772; D2: $3af2; D3: $4d15; D4: ($8c, $fa, $8a, $96, $2f, $f3, $20, $40)); pid: $0b);
  {$EXTERNALSYM MFPKEY_CRISP}
  MFPKEY_FOURCC                     :  PROPERTYKEY = (fmtid: (D1: $593e3f2e; D2: $f84d; D3: $4e85; D4: ($b6, $8d, $f6, $69, $40, $0e, $da, $bc)); pid: $0c);
  {$EXTERNALSYM MFPKEY_FOURCC}
  MFPKEY_VIDEOWINDOW                :  PROPERTYKEY = (fmtid: (D1: $c1c96060; D2: $76f0; D3: $47d4; D4: ($a8, $75, $5b, $dd, $a9, $0d, $f5, $e9)); pid: $0d);
  {$EXTERNALSYM MFPKEY_VIDEOWINDOW}
  MFPKEY_FRAMECOUNT                 :  PROPERTYKEY = (fmtid: (D1: $75028eb4; D2: $4853; D3: $44d3; D4: ($88, $a3, $e4, $99, $f8, $9d, $22, $7f)); pid: $0e);
  {$EXTERNALSYM MFPKEY_FRAMECOUNT}
  MFPKEY_LIVEENCODE                 :  PROPERTYKEY = (fmtid: (D1: $3ffa1e60; D2: $5514; D3: $4634; D4: ($86, $e6, $1f, $3b, $7c, $54, $51, $43)); pid: $0f);
  {$EXTERNALSYM MFPKEY_LIVEENCODE}
  MFPKEY_COMPLEXITY                 :  PROPERTYKEY = (fmtid: (D1: $44fa08c7; D2: $92f5; D3: $45dc; D4: ($83, $76, $8d, $1d, $32, $4c, $65, $2a)); pid: $10);
  {$EXTERNALSYM MFPKEY_COMPLEXITY}
  MFPKEY_COMPLEXITYEX               :  PROPERTYKEY = (fmtid: (D1: $d6e48f93; D2: $fd47; D3: $47a3; D4: ($92, $62, $8a, $ef, $b5, $53, $03, $32)); pid: $11);
  {$EXTERNALSYM MFPKEY_COMPLEXITYEX}
  MFPKEY_ASFOVERHEADPERFRAME        :  PROPERTYKEY = (fmtid: (D1: $0eac7502; D2: $1957; D3: $4beb; D4: ($91, $4d, $88, $5f, $85, $e7, $54, $36)); pid: $12);
  {$EXTERNALSYM MFPKEY_ASFOVERHEADPERFRAME}
  MFPKEY_PASSESUSED                 :  PROPERTYKEY = (fmtid: (D1: $b1653ac1; D2: $cb7d; D3: $43ee; D4: ($84, $54, $3f, $9d, $81, $1b, $03, $31)); pid: $13);
  {$EXTERNALSYM MFPKEY_PASSESUSED}
  MFPKEY_VBRENABLED                 :  PROPERTYKEY = (fmtid: (D1: $e48d9459; D2: $6abe; D3: $4eb5; D4: ($92, $11, $60, $08, $0c, $1a, $b9, $84)); pid: $14);
  {$EXTERNALSYM MFPKEY_VBRENABLED}
  MFPKEY_VBRQUALITY                 :  PROPERTYKEY = (fmtid: (D1: $f97b3f3a; D2: $9eff; D3: $4ac9; D4: ($82, $47, $35, $b3, $0e, $b9, $25, $f4)); pid: $15);
  {$EXTERNALSYM MFPKEY_VBRQUALITY}
  MFPKEY_RAVG                       :  PROPERTYKEY = (fmtid: (D1: $14b2aae6; D2: $2987; D3: $460a; D4: ($8b, $22, $9c, $07, $7c, $55, $d0, $5e)); pid: $16);
  {$EXTERNALSYM MFPKEY_RAVG}
  MFPKEY_BAVG                       :  PROPERTYKEY = (fmtid: (D1: $10174e76; D2: $e0ca; D3: $4a39; D4: ($94, $8c, $85, $10, $c2, $32, $32, $76)); pid: $17);
  {$EXTERNALSYM MFPKEY_BAVG}
  MFPKEY_RMAX                       :  PROPERTYKEY = (fmtid: (D1: $7d8dd246; D2: $aaf4; D3: $4a24; D4: ($81, $66, $19, $39, $6b, $06, $ef, $69)); pid: $18);
  {$EXTERNALSYM MFPKEY_RMAX}
  MFPKEY_BMAX                       :  PROPERTYKEY = (fmtid: (D1: $ff365211; D2: $21b6; D3: $4134; D4: ($ab, $7c, $52, $39, $3a, $8f, $80, $f6)); pid: $19);
  {$EXTERNALSYM MFPKEY_BMAX}
  MFPKEY_INTERLACEDCODINGENABLED    :  PROPERTYKEY = (fmtid: (D1: $56976073; D2: $06c3; D3: $4b3b; D4: ($ad, $41, $b7, $41, $7f, $ce, $84, $74)); pid: $1a);
  {$EXTERNALSYM MFPKEY_INTERLACEDCODINGENABLED}
  MFPKEY_PRODUCEDUMMYFRAMES         :  PROPERTYKEY = (fmtid: (D1: $61714bc6; D2: $08a1; D3: $49d1; D4: ($b8, $27, $a3, $3a, $ad, $a9, $55, $26)); pid: $1b);
  {$EXTERNALSYM MFPKEY_PRODUCEDUMMYFRAMES}
  MFPKEY_DECODERCOMPLEXITYREQUESTED :  PROPERTYKEY = (fmtid: (D1: $b0d7d4a7; D2: $422b; D3: $44c3; D4: ($97, $b4, $b9, $76, $0c, $ce, $ee, $a9)); pid: $1c);
  {$EXTERNALSYM MFPKEY_DECODERCOMPLEXITYREQUESTED}
  MFPKEY_DROPPEDFRAMES              :  PROPERTYKEY = (fmtid: (D1: $bdb5afd3; D2: $4027; D3: $4882; D4: ($80, $6c, $41, $62, $e4, $a7, $a3, $f0)); pid: $1d);
  {$EXTERNALSYM MFPKEY_DROPPEDFRAMES}
  MFPKEY_CODEDNONZEROFRAMES         :  PROPERTYKEY = (fmtid: (D1: $7adf5b69; D2: $1e3f; D3: $42d3; D4: ($99, $1d, $f8, $1e, $0c, $eb, $e9, $3e)); pid: $1e);
  {$EXTERNALSYM MFPKEY_CODEDNONZEROFRAMES}
  MFPKEY_QPPERFRAME                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $1f);
  {$EXTERNALSYM MFPKEY_QPPERFRAME}
  MFPKEY_VOLHEADERFORREENCODE       :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $21);
  {$EXTERNALSYM MFPKEY_VOLHEADERFORREENCODE}
  MFPKEY_REENCDURATION              :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $22);
  {$EXTERNALSYM MFPKEY_REENCDURATION}
  MFPKEY_REENCSTARTBUFFERSIZE       :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $23);
  {$EXTERNALSYM MFPKEY_REENCSTARTBUFFERSIZE}
  MFPKEY_REENCENDBUFFERSIZE         :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $24);
  {$EXTERNALSYM MFPKEY_REENCENDBUFFERSIZE}
  MFPKEY_REENCQPREF                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $25);
  {$EXTERNALSYM MFPKEY_REENCQPREF}
  MFPKEY_DENOISEOPTION              :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $26);
  {$EXTERNALSYM MFPKEY_DENOISEOPTION}
  MFPKEY_FULLFRAMERATE              :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $2d);
  {$EXTERNALSYM MFPKEY_FULLFRAMERATE}
  MFPKEY_MOTIONSEARCHRANGE          :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $2e);
  {$EXTERNALSYM MFPKEY_MOTIONSEARCHRANGE}
  MFPKEY_DELTAMVRANGEINDEX          :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $2f);
  {$EXTERNALSYM MFPKEY_DELTAMVRANGEINDEX}
  MFPKEY_NUMBFRAMES                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $30);
  {$EXTERNALSYM MFPKEY_NUMBFRAMES}
  MFPKEY_RDSUBPIXELSEARCH           :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $31);
  {$EXTERNALSYM MFPKEY_RDSUBPIXELSEARCH}
  MFPKEY_BDELTAQP                   :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $32);
  {$EXTERNALSYM MFPKEY_BDELTAQP}
  MFPKEY_FORCEFRAMEWIDTH            :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $33);
  {$EXTERNALSYM MFPKEY_FORCEFRAMEWIDTH}
  MFPKEY_FORCEFRAMEHEIGHT           :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $34);
  {$EXTERNALSYM MFPKEY_FORCEFRAMEHEIGHT}
  MFPKEY_RANGEREDUX                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $35);
  {$EXTERNALSYM MFPKEY_RANGEREDUX}
  MFPKEY_LOOKAHEAD                  :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $36);
  {$EXTERNALSYM MFPKEY_LOOKAHEAD}
  MFPKEY_VIDEOSCALING               :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $37);
  {$EXTERNALSYM MFPKEY_VIDEOSCALING}
  MFPKEY_PERCEPTUALOPTLEVEL         :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $38);
  {$EXTERNALSYM MFPKEY_PERCEPTUALOPTLEVEL}
  MFPKEY_FORCEMEDIANSETTING         :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $39);
  {$EXTERNALSYM MFPKEY_FORCEMEDIANSETTING}
  MFPKEY_NUMTHREADS                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $3a);
  {$EXTERNALSYM MFPKEY_NUMTHREADS}
  MFPKEY_LOOPFILTER                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $3b);
  {$EXTERNALSYM MFPKEY_LOOPFILTER}
  MFPKEY_NOISEEDGEREMOVAL           :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $3c);
  {$EXTERNALSYM MFPKEY_NOISEEDGEREMOVAL}
  MFPKEY_VTYPE                      :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $3d);
  {$EXTERNALSYM MFPKEY_VTYPE}
  MFPKEY_CLOSEDENTRYPOINT            :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $3f);
  {$EXTERNALSYM MFPKEY_CLOSEDENTRYPOINT}
  MFPKEY_MOTIONSEARCHLEVEL           :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $40);
  {$EXTERNALSYM MFPKEY_MOTIONSEARCHLEVEL}
  MFPKEY_MOTIONMATCHMETHOD           :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $41);
  {$EXTERNALSYM MFPKEY_MOTIONMATCHMETHOD}
  MFPKEY_MACROBLOCKMODECOSTMETHOD    :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $42);
  {$EXTERNALSYM MFPKEY_MACROBLOCKMODECOSTMETHOD}
  MFPKEY_COMPRESSIONOPTIMIZATIONTYPE :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $43);
  {$EXTERNALSYM MFPKEY_COMPRESSIONOPTIMIZATIONTYPE}
  MFPKEY_PERIODICALSPDISTANCE        :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $46);
  {$EXTERNALSYM MFPKEY_PERIODICALSPDISTANCE}
  MFPKEY_ENCODERCOMPLEXITY           :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $47);
  {$EXTERNALSYM MFPKEY_ENCODERCOMPLEXITY}
  MFPKEY_USERDATASIZE                :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $48);
  {$EXTERNALSYM MFPKEY_USERDATASIZE}
  MFPKEY_LETTERBOXPRESENT            :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $49);
  {$EXTERNALSYM MFPKEY_LETTERBOXPRESENT}
  MFPKEY_SCENECHANGE                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $4a);
  {$EXTERNALSYM MFPKEY_SCENECHANGE}
  MFPKEY_VARIABLEGOP                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $4b);
  {$EXTERNALSYM MFPKEY_VARIABLEGOP}
  MFPKEY_SCENECHANGEI                :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $4c);
  {$EXTERNALSYM MFPKEY_SCENECHANGEI}
  MFPKEY_LOOKAHEADRC                 :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $4d);
  {$EXTERNALSYM MFPKEY_LOOKAHEADRC}
  MFPKEY_DQUANTOPTION                :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $4e);
  {$EXTERNALSYM MFPKEY_DQUANTOPTION}
  MFPKEY_DQUANTSTRENGTH              :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $4f);
  {$EXTERNALSYM MFPKEY_DQUANTSTRENGTH}
  MFPKEY_FORCEOVERLAP                :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $50);
  {$EXTERNALSYM MFPKEY_FORCEOVERLAP}
  MFPKEY_MOTIONVECTORCOSTMETHOD      :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $51);
  {$EXTERNALSYM MFPKEY_MOTIONVECTORCOSTMETHOD}
  MFPKEY_DYNCOMPLEXLEVEL             :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $52);
  {$EXTERNALSYM MFPKEY_DYNCOMPLEXLEVEL}
  MFPKEY_TARGETENCRATE               :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $53);
  {$EXTERNALSYM MFPKEY_TARGETENCRATE}
  MFPKEY_DYNENCMODE                  :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $54);
  {$EXTERNALSYM MFPKEY_DYNENCMODE}
  MFPKEY_TARGETENCDELTA              :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $55);
  {$EXTERNALSYM MFPKEY_TARGETENCDELTA}
  MFPKEY_ADAPTIVERESOLUTION          :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $56);
  {$EXTERNALSYM MFPKEY_ADAPTIVERESOLUTION}
  MFPKEY_SETDYNVIDRES                :  PROPERTYKEY = (fmtid: (D1: $4e91bf89; D2: $665a; D3: $49da; D4: ($bb, $94, $88, $c5, $50, $cf, $cd, $28)); pid: $57);
  {$EXTERNALSYM MFPKEY_SETDYNVIDRES}

  MFPKEY_CLIP_XORIG                 :  PROPERTYKEY = (fmtid: (D1: $716fe5c8; D2: $755c; D3: $482f; D4: ($8d, $f3, $b3, $1d, $53, $59, $f0, $d6)); pid: $01);
  {$EXTERNALSYM MFPKEY_CLIP_XORIG}
  MFPKEY_CLIP_YORIG                 :  PROPERTYKEY = (fmtid: (D1: $716fe5c8; D2: $755c; D3: $482f; D4: ($8d, $f3, $b3, $1d, $53, $59, $f0, $d6)); pid: $02);
  {$EXTERNALSYM MFPKEY_CLIP_YORIG}
  MFPKEY_CLIP_WIDTH                 :  PROPERTYKEY = (fmtid: (D1: $716fe5c8; D2: $755c; D3: $482f; D4: ($8d, $f3, $b3, $1d, $53, $59, $f0, $d6)); pid: $03);
  {$EXTERNALSYM MFPKEY_CLIP_WIDTH}
  MFPKEY_CLIP_HEIGHT                :  PROPERTYKEY = (fmtid: (D1: $716fe5c8; D2: $755c; D3: $482f; D4: ($8d, $f3, $b3, $1d, $53, $59, $f0, $d6)); pid: $04);
  {$EXTERNALSYM MFPKEY_CLIP_HEIGHT}

  MFPKEY_FI_FRAMERATE_VALU_SRC      :  PROPERTYKEY = (fmtid: (D1: $305bca55; D2: $1e5b; D3: $428e; D4: ($a9, $4c, $65, $b9, $4d, $2, $64, $ed)); pid: $01);
  {$EXTERNALSYM MFPKEY_FI_FRAMERATE_VALU_SRC}
  MFPKEY_FI_FRAMERATE_SCAL_SRC      :  PROPERTYKEY = (fmtid: (D1: $305bca55; D2: $1e5b; D3: $428e; D4: ($a9, $4c, $65, $b9, $4d, $2, $64, $ed)); pid: $02);
  {$EXTERNALSYM MFPKEY_FI_FRAMERATE_SCAL_SRC}
  MFPKEY_FI_FRAMERATE_VALU_DST      :  PROPERTYKEY = (fmtid: (D1: $305bca55; D2: $1e5b; D3: $428e; D4: ($a9, $4c, $65, $b9, $4d, $2, $64, $ed)); pid: $03);
  {$EXTERNALSYM MFPKEY_FI_FRAMERATE_VALU_DST}
  MFPKEY_FI_FRAMERATE_SCAL_DST      :  PROPERTYKEY = (fmtid: (D1: $305bca55; D2: $1e5b; D3: $428e; D4: ($a9, $4c, $65, $b9, $4d, $2, $64, $ed)); pid: $04);
  {$EXTERNALSYM MFPKEY_FI_FRAMERATE_SCAL_DST}
  MFPKEY_FI_ALLOWED                 :  PROPERTYKEY = (fmtid: (D1: $305bca55; D2: $1e5b; D3: $428e; D4: ($a9, $4c, $65, $b9, $4d, $2, $64, $ed)); pid: $05);
  {$EXTERNALSYM MFPKEY_FI_ALLOWED}
  MFPKEY_FI_COMPLEXITY              :  PROPERTYKEY = (fmtid: (D1: $305bca55; D2: $1e5b; D3: $428e; D4: ($a9, $4c, $65, $b9, $4d, $2, $64, $ed)); pid: $06);
  {$EXTERNALSYM MFPKEY_FI_COMPLEXITY}

  MFPKEY_RESIZE_SRC_LEFT        :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $01);
  {$EXTERNALSYM MFPKEY_RESIZE_SRC_LEFT}
  MFPKEY_RESIZE_SRC_TOP         :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $02);
  {$EXTERNALSYM MFPKEY_RESIZE_SRC_TOP}
  MFPKEY_RESIZE_SRC_WIDTH       :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $03);
  {$EXTERNALSYM MFPKEY_RESIZE_SRC_WIDTH}
  MFPKEY_RESIZE_SRC_HEIGHT      :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $04);
  {$EXTERNALSYM MFPKEY_RESIZE_SRC_HEIGHT}
  MFPKEY_RESIZE_DST_LEFT        :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $05);
  {$EXTERNALSYM MFPKEY_RESIZE_DST_LEFT}
  MFPKEY_RESIZE_DST_TOP         :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $06);
  {$EXTERNALSYM MFPKEY_RESIZE_DST_TOP}
  MFPKEY_RESIZE_DST_WIDTH       :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $07);
  {$EXTERNALSYM MFPKEY_RESIZE_DST_WIDTH}
  MFPKEY_RESIZE_DST_HEIGHT      :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $08);
  {$EXTERNALSYM MFPKEY_RESIZE_DST_HEIGHT}
  MFPKEY_RESIZE_QUALITY         :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $09);
  {$EXTERNALSYM MFPKEY_RESIZE_QUALITY}
  MFPKEY_RESIZE_INTERLACE       :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $0a);
  {$EXTERNALSYM MFPKEY_RESIZE_INTERLACE}
  MFPKEY_RESIZE_PANSCANAPX      :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $0b);
  {$EXTERNALSYM MFPKEY_RESIZE_PANSCANAPX}
  MFPKEY_RESIZE_PANSCANAPY      :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $0c);
  {$EXTERNALSYM MFPKEY_RESIZE_PANSCANAPY}
  MFPKEY_RESIZE_PANSCANAPWIDTH  :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $0d);
  {$EXTERNALSYM MFPKEY_RESIZE_PANSCANAPWIDTH}
  MFPKEY_RESIZE_PANSCANAPHEIGHT :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $0e);
  {$EXTERNALSYM MFPKEY_RESIZE_PANSCANAPHEIGHT}
  MFPKEY_RESIZE_GEOMAPX         :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $0f);
  {$EXTERNALSYM MFPKEY_RESIZE_GEOMAPX}
  MFPKEY_RESIZE_GEOMAPY         :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $10);
  {$EXTERNALSYM MFPKEY_RESIZE_GEOMAPY}
  MFPKEY_RESIZE_GEOMAPWIDTH     :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $11);
  {$EXTERNALSYM MFPKEY_RESIZE_GEOMAPWIDTH}
  MFPKEY_RESIZE_GEOMAPHEIGHT    :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $12);
  {$EXTERNALSYM MFPKEY_RESIZE_GEOMAPHEIGHT}
  MFPKEY_RESIZE_MINAPX          :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $13);
  {$EXTERNALSYM MFPKEY_RESIZE_MINAPX}
  MFPKEY_RESIZE_MINAPY          :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $14);
  {$EXTERNALSYM MFPKEY_RESIZE_MINAPY}
  MFPKEY_RESIZE_MINAPWIDTH      :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $15);
  {$EXTERNALSYM MFPKEY_RESIZE_MINAPWIDTH}
  MFPKEY_RESIZE_MINAPHEIGHT     :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $16);
  {$EXTERNALSYM MFPKEY_RESIZE_MINAPHEIGHT}
  MFPKEY_PIXELASPECTRATIO       :  PROPERTYKEY = (fmtid: (D1: $6612a6bc; D2: $e57d; D3: $407d; D4: ($a9, $58, $28, $5d, $f0, $d9, $b4, $00)); pid: $17);
  {$EXTERNALSYM MFPKEY_PIXELASPECTRATIO}

  MFPKEY_COLORCONV_SRCLEFT      :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $01);
  {$EXTERNALSYM MFPKEY_COLORCONV_SRCLEFT}
  MFPKEY_COLORCONV_SRCTOP       :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $02);
  {$EXTERNALSYM MFPKEY_COLORCONV_SRCTOP}
  MFPKEY_COLORCONV_DSTLEFT      :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $03);
  {$EXTERNALSYM MFPKEY_COLORCONV_DSTLEFT}
  MFPKEY_COLORCONV_DSTTOP       :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $04);
  {$EXTERNALSYM MFPKEY_COLORCONV_DSTTOP}
  MFPKEY_COLORCONV_WIDTH        :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $05);
  {$EXTERNALSYM MFPKEY_COLORCONV_WIDTH}
  MFPKEY_COLORCONV_HEIGHT       :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $06);
  {$EXTERNALSYM MFPKEY_COLORCONV_HEIGHT}
  MFPKEY_COLORCONV_MODE         :  PROPERTYKEY = (fmtid: (D1: $dc9100be; D2: $1228; D3: $416c; D4: ($99, $48, $6f, $38, $f4, $79, $65, $4f)); pid: $07);
  {$EXTERNALSYM MFPKEY_COLORCONV_MODE}

  MFPKEY_DEINTERLACE_PROCESSTYPE     :  PROPERTYKEY = (fmtid: (D1: $6141f4c3; D2: $d3a2; D3: $48a9; D4: ($96, $ac, $2f, $0, $14, $17, $6c, $26)); pid: 01);
  {$EXTERNALSYM MFPKEY_DEINTERLACE_PROCESSTYPE}
  MFPKEY_DEINTERLACE_TELECINEPATTERN :  PROPERTYKEY = (fmtid: (D1: $6141f4c3; D2: $d3a2; D3: $48a9; D4: ($96, $ac, $2f, $0, $14, $17, $6c, $26)); pid: 02);
  {$EXTERNALSYM MFPKEY_DEINTERLACE_TELECINEPATTERN}
  MFPKEY_DEINTERLACE_LASTFRAME       :  PROPERTYKEY = (fmtid: (D1: $6141f4c3; D2: $d3a2; D3: $48a9; D4: ($96, $ac, $2f, $0, $14, $17, $6c, $26)); pid: 03);
  {$EXTERNALSYM MFPKEY_DEINTERLACE_LASTFRAME}
  MFPKEY_DEINTERLACE_DETELECINE_FLAG :  PROPERTYKEY = (fmtid: (D1: $6141f4c3; D2: $d3a2; D3: $48a9; D4: ($96, $ac, $2f, $0, $14, $17, $6c, $26)); pid: 04);
  {$EXTERNALSYM MFPKEY_DEINTERLACE_DETELECINE_FLAG}
  MFPKEY_DEINTERLACE_SMOOTHLEVEL     :  PROPERTYKEY = (fmtid: (D1: $6141f4c3; D2: $d3a2; D3: $48a9; D4: ($96, $ac, $2f, $0, $14, $17, $6c, $26)); pid: 05);
  {$EXTERNALSYM MFPKEY_DEINTERLACE_SMOOTHLEVEL}
  MFPKEY_DEINTERLACE_EDGETHRESHOLD   :  PROPERTYKEY = (fmtid: (D1: $6141f4c3; D2: $d3a2; D3: $48a9; D4: ($96, $ac, $2f, $0, $14, $17, $6c, $26)); pid: 06);
  {$EXTERNALSYM MFPKEY_DEINTERLACE_EDGETHRESHOLD}

  MFPKEY_DVDEC_SIZE   :  PROPERTYKEY = (fmtid: (D1: $75f2421a; D2: $e73a; D3: $45b3; D4: ($ae, $f0, $91, $3c, $66, $84, $64, $61)); pid: $00);
  {$EXTERNALSYM MFPKEY_DVDEC_SIZE}
  MFPKEY_DVENC_FORMAT :  PROPERTYKEY = (fmtid: (D1: $f449a927; D2: $7b22; D3: $46ef; D4: ($b2, $e7, $4d, $2b, $72, $8b, $69, $9c)); pid: $00);
  {$EXTERNALSYM MFPKEY_DVENC_FORMAT}


  g_wszWMVCDatarate                   = ('_DATARATE');
  {$EXTERNALSYM g_wszWMVCDatarate}
  g_wszWMVCKeyframeDistance           = ('_KEYDIST');
  {$EXTERNALSYM g_wszWMVCKeyframeDistance}
  g_wszWMVCCrisp                      = ('_CRISP');
  {$EXTERNALSYM g_wszWMVCCrisp}
  g_wszWMVCDefaultCrisp               = ('_DEFAULTCRISP');
  {$EXTERNALSYM g_wszWMVCDefaultCrisp}
  g_wszWMVCFOURCC                     = ('_FOURCC');
  {$EXTERNALSYM g_wszWMVCFOURCC}
  g_wszWMVCTotalWindow                = ('_TOTALWINDOW');
  {$EXTERNALSYM g_wszWMVCTotalWindow}
  g_wszWMVCVideoWIndow                = ('_VIDEOWINDOW');
  {$EXTERNALSYM g_wszWMVCVideoWIndow}
  g_wszWMVCFrameCount                 = ('_FRAMECOUNT');
  {$EXTERNALSYM g_wszWMVCFrameCount}
  g_wszWMVCLiveEncode                 = ('_LIVEENCODE');
  {$EXTERNALSYM g_wszWMVCLiveEncode}
  g_wszWMVCComplexityMode             = ('_COMPLEXITY');
  {$EXTERNALSYM g_wszWMVCComplexityMode}
  g_wszWMVCComplexityEx               = ('_COMPLEXITYEX');
  {$EXTERNALSYM g_wszWMVCComplexityEx}
  g_wszWMVCComplexityMax              = ('_COMPLEXITYEXMAX');
  {$EXTERNALSYM g_wszWMVCComplexityMax}
  g_wszWMVCComplexityLive             = ('_COMPLEXITYEXLIVE');
  {$EXTERNALSYM g_wszWMVCComplexityLive}
  g_wszWMVCComplexityOffline          = ('_COMPLEXITYEXOFFLINE');
  {$EXTERNALSYM g_wszWMVCComplexityOffline}
  g_wszWMVCPacketOverhead             = ('_ASFOVERHEADPERFRAME');
  {$EXTERNALSYM g_wszWMVCPacketOverhead}
  g_wszWMVCLegacy411InterlacedFormat  = ('_LEGACY411INTERLACEDFORMAT');
  {$EXTERNALSYM g_wszWMVCLegacy411InterlacedFormat}
  g_wszWMVCPassesRecommended          = ('_PASSESRECOMMENDED');
  {$EXTERNALSYM g_wszWMVCPassesRecommended}
  g_wszWMVCPassesUsed                 = ('_PASSESUSED');
  {$EXTERNALSYM g_wszWMVCPassesUsed}
  g_wszWMVCEndOfPass                  = ('_ENDOFPASS');
  {$EXTERNALSYM g_wszWMVCEndOfPass}
  g_wszWMVCFrameInterpolationSupported= ('_FRAMEINTERPOLATIONSUPPORTED');
  {$EXTERNALSYM g_wszWMVCFrameInterpolationSupported}
  g_wszWMVCFrameInterpolationEnabled  = ('_FRAMEINTERPOLATIONENABLED');
  {$EXTERNALSYM g_wszWMVCFrameInterpolationEnabled}
  g_wszWMVCQPPerFrame                 = ('_QPPERFRAME');
  {$EXTERNALSYM g_wszWMVCQPPerFrame}
  g_wszWMVCReencDuration              = ('_REENCDURATION');
  {$EXTERNALSYM g_wszWMVCReencDuration}
  g_wszWMVCReencStartBufferSize       = ('_REENCSTARTBUFFERSIZE');
  {$EXTERNALSYM g_wszWMVCReencStartBufferSize}
  g_wszWMVCReencEndBufferSize         = ('_REENCENDBUFFERSIZE');
  {$EXTERNALSYM g_wszWMVCReencEndBufferSize}
  g_wszWMVCReencQPRef                 = ('_REENCQPREF');
  {$EXTERNALSYM g_wszWMVCReencQPRef}
  g_wszWMVCDenoiseOption              = ('_DENOISEOPTION');
  {$EXTERNALSYM g_wszWMVCDenoiseOption}
  g_wszWMVCMirrorDisplayOn            = ('_MIRRORDISPLAYON');
  {$EXTERNALSYM g_wszWMVCMirrorDisplayOn}
  g_wszWMVCChangeFrameRate            = ('_CHANGEFRAMERATE');
  {$EXTERNALSYM g_wszWMVCChangeFrameRate}
  g_wszWMVCChangeBitRate              = ('_CHANGEBITRATE');
  {$EXTERNALSYM g_wszWMVCChangeBitRate}
  g_wszWMVCChangeMaxBitRate           = ('_CHANGEMAXBITRATE');
  {$EXTERNALSYM g_wszWMVCChangeMaxBitRate}
  g_wszWMVCFullFrameRate              = ('_FULLFRAMERATE');
  {$EXTERNALSYM g_wszWMVCFullFrameRate}
  g_wszWMVCMotionSearchRange          = ('_MOTIONSEARCHRANGE');
  {$EXTERNALSYM g_wszWMVCMotionSearchRange}
  g_wszWMVCDeltaMVRangeIndex          = ('_DELTAMVRANGEINDEX');
  {$EXTERNALSYM g_wszWMVCDeltaMVRangeIndex}
  g_wszWMVCNumBFrames                 = ('_NUMBFRAMES');
  {$EXTERNALSYM g_wszWMVCNumBFrames}
  g_wszWMVCRDSubpixelSearch           = ('_RDSUBPIXELSEARCH');
  {$EXTERNALSYM g_wszWMVCRDSubpixelSearch}
  g_wszWMVCBDeltaQP                   = ('_BDELTAQP');
  {$EXTERNALSYM g_wszWMVCBDeltaQP}
  g_wszWMVCForceFrameWidth            = ('_FORCEFRAMEWIDTH');
  {$EXTERNALSYM g_wszWMVCForceFrameWidth}
  g_wszWMVCForceFrameHeight           = ('_FORCEFRAMEHEIGHT');
  {$EXTERNALSYM g_wszWMVCForceFrameHeight}
  g_wszWMVCRangeRedux                 = ('_RANGEREDUX');
  {$EXTERNALSYM g_wszWMVCRangeRedux}
  g_wszWMVCLookAhead                  = ('_LOOKAHEAD');
  {$EXTERNALSYM g_wszWMVCLookAhead}
  g_wszWMVCVideoScaling               = ('_VIDEOSCALING');
  {$EXTERNALSYM g_wszWMVCVideoScaling}
  g_wszWMVCPerceptualOptLevel         = ('_PERCEPTUALOPTLEVEL');
  {$EXTERNALSYM g_wszWMVCPerceptualOptLevel}
  g_wszWMVCForceMedianSetting         = ('_FORCEMEDIANSETTING');
  {$EXTERNALSYM g_wszWMVCForceMedianSetting}
  g_wszWMVCNumThreads                 = ('_NUMTHREADS');
  {$EXTERNALSYM g_wszWMVCNumThreads}
  g_wszWMVCLoopFilter                 = ('_LOOPFILTER');
  {$EXTERNALSYM g_wszWMVCLoopFilter}
  g_wszWMVCNoiseEdgeRemoval           = ('_NOISEEDGEREMOVAL');
  {$EXTERNALSYM g_wszWMVCNoiseEdgeRemoval}
  g_wszWMVCVType                      = ('_VTYPE');
  {$EXTERNALSYM g_wszWMVCVType}
  g_wszWMVCMotionSearchLevel          = ('_MOTIONSEARCHLEVEL');
  {$EXTERNALSYM g_wszWMVCMotionSearchLevel}
  g_wszWMVCMotionMatchMethod          = ('_MOTIONMATCHMETHOD');
  {$EXTERNALSYM g_wszWMVCMotionMatchMethod}
  g_wszWMVCMacroblockModeCostMethod   = ('_MACROBLOCKMODECOSTMETHOD');
  {$EXTERNALSYM g_wszWMVCMacroblockModeCostMethod}
  g_wszWMVCCompressionOptimizationType= ('_COMPRESSIONOPTIMIZATIONTYPE');
  {$EXTERNALSYM g_wszWMVCCompressionOptimizationType}
  g_wszWMVCPeriodicalSPDistance       = ('_PERIODICALSPDISTANCE');
  {$EXTERNALSYM g_wszWMVCPeriodicalSPDistance}
  g_wszWMVCQueryTimeStampTag          = ('_QUERYTIMESTAMPTAG');
  {$EXTERNALSYM g_wszWMVCQueryTimeStampTag}
  g_wszWMVCSupportOneInMultiOut       = ('_SUPPORTONEINMULTIOUT');
  {$EXTERNALSYM g_wszWMVCSupportOneInMultiOut}
  g_wszWMVCEncodingWidth              = ('_ENCODINGWIDTH');
  {$EXTERNALSYM g_wszWMVCEncodingWidth}
  g_wszWMVCEncodingHeight             = ('_ENCODINGHEIGHT');
  {$EXTERNALSYM g_wszWMVCEncodingHeight}
  g_wszWMVCThreadAffinityMask         = ('_THREADAFFINITYMASK');
  {$EXTERNALSYM g_wszWMVCThreadAffinityMask}
  g_wszWMVCDecoderForceNoResizeOutput = ('_FORCENORESIZE');
  {$EXTERNALSYM g_wszWMVCDecoderForceNoResizeOutput}
  g_wszWMVCDynComplexityLevel         = ('_DYNCOMPLEXLEVEL');
  {$EXTERNALSYM g_wszWMVCDynComplexityLevel}
  g_wszWMVCTargetEncodeRate           = ('_TARGETENCRATE');
  {$EXTERNALSYM g_wszWMVCTargetEncodeRate}
  g_wszWMVCDynamicEncoderMode         = ('_DYNENCMODE');
  {$EXTERNALSYM g_wszWMVCDynamicEncoderMode}
  g_wszWMVCTargetEncodeDelta          = ('_TARGETENCDELTA');
  {$EXTERNALSYM g_wszWMVCTargetEncodeDelta}
  g_wszWMVCAdaptiveResolution         = ('_ADAPTIVERESOLUTION');
  {$EXTERNALSYM g_wszWMVCAdaptiveResolution}
  g_wszWMVCDynamicVideoResolution     = ('_SETDYNVIDRES');
  {$EXTERNALSYM g_wszWMVCDynamicVideoResolution}
  g_wszWMACInputFormatName            = ('_INPUTFORMATNAME');
  {$EXTERNALSYM g_wszWMACInputFormatName}
  g_wszWMACSourceFormatTag            = ('_SOURCEFORMATTAG');
  {$EXTERNALSYM g_wszWMACSourceFormatTag}
  g_wszWMVCVBREnabled                 = ('_VBRENABLED');
  {$EXTERNALSYM g_wszWMVCVBREnabled}
  g_wszWMVCVBRQuality                 = ('_VBRQUALITY');
  {$EXTERNALSYM g_wszWMVCVBRQuality}
  g_wszWMVCAvgBitrate                 = ('_RAVG');
  {$EXTERNALSYM g_wszWMVCAvgBitrate}
  g_wszWMVCMaxBitrate                 = ('_RMAX');
  {$EXTERNALSYM g_wszWMVCMaxBitrate}
  g_wszWMVCBAvg                       = ('_BAVG');
  {$EXTERNALSYM g_wszWMVCBAvg}
  g_wszWMVCBMax                       = ('_BMAX');
  {$EXTERNALSYM g_wszWMVCBMax}
  g_wszWMVCTotalFrames                = ('_TOTALFRAMES');
  {$EXTERNALSYM g_wszWMVCTotalFrames}
  g_wszWMVCCodedFrames                = ('_CODEDFRAMES');
  {$EXTERNALSYM g_wszWMVCCodedFrames}
  g_wszWMVCAvgFrameRate               = ('_AVGFRAMERATE');
  {$EXTERNALSYM g_wszWMVCAvgFrameRate}
  g_wszWMVCDecoderComplexityProfile   = ('_DECODERCOMPLEXITYPROFILE');
  {$EXTERNALSYM g_wszWMVCDecoderComplexityProfile}
  g_wszWMVCDecoderComplexityRequested = ('_DECODERCOMPLEXITYREQUESTED');
  {$EXTERNALSYM g_wszWMVCDecoderComplexityRequested}
  g_wszWMVCBufferFullnessInFirstByte  = ('_BUFFERFULLNESSINFIRSTBYTE');
  {$EXTERNALSYM g_wszWMVCBufferFullnessInFirstByte}
  g_wszWMACPeakPCMValue               = ('PeakValue');
  {$EXTERNALSYM g_wszWMACPeakPCMValue}
  g_wszWMACAvgPCMValue                = ('AverageLevel');
  {$EXTERNALSYM g_wszWMACAvgPCMValue}
  g_wszWMADRCAverageReference         = ('WMADRCAverageReference');
  {$EXTERNALSYM g_wszWMADRCAverageReference}
  g_wszWMADRCPeakReference            = ('WMADRCPeakReference');
  {$EXTERNALSYM g_wszWMADRCPeakReference}
  g_wszWMADRCAverageTarget            = ('WMADRCAverageTarget');
  {$EXTERNALSYM g_wszWMADRCAverageTarget}
  g_wszWMADRCPeakTarget               = ('WMADRCPeakTarget');
  {$EXTERNALSYM g_wszWMADRCPeakTarget}
  g_wszWMACHiResOutput                = ('_HIRESOUTPUT');
  {$EXTERNALSYM g_wszWMACHiResOutput}
  g_wszWMACAvgBytesPerSec             = ('AvgBytesPerSec');
  {$EXTERNALSYM g_wszWMACAvgBytesPerSec}
  g_wszWMACSpeakerConfig              = ('SpeakerConfig');
  {$EXTERNALSYM g_wszWMACSpeakerConfig}
  g_wszWMACMixTable                   = ('MixTable');
  {$EXTERNALSYM g_wszWMACMixTable}
  g_wszWMACDRCSetting                 = ('DynamicRangeControl');
  {$EXTERNALSYM g_wszWMACDRCSetting}
  g_wszWMVCWatermarkConfig            = ('WatermarkConfig');
  {$EXTERNALSYM g_wszWMVCWatermarkConfig}
  g_wszWMVCWatermarkDelay             = ('WatermarkDelay');
  {$EXTERNALSYM g_wszWMVCWatermarkDelay}
  g_wszWMVCInterlacedCodingEnabled    = ('_INTERLACEDCODINGENABLED');
  {$EXTERNALSYM g_wszWMVCInterlacedCodingEnabled}
  g_wszWMVCProduceDummyFrames         = ('_PRODUCEDUMMYFRAMES');
  {$EXTERNALSYM g_wszWMVCProduceDummyFrames}
  g_wszWMVCDecoderDeinterlacing       = ('_DECODERDEINTERLACING');
  {$EXTERNALSYM g_wszWMVCDecoderDeinterlacing}
  g_wszWMACOriginalWaveFormat         = ('_ORIGINALWAVEFORMAT');
  {$EXTERNALSYM g_wszWMACOriginalWaveFormat}
  g_wszWMACIncludeNumPasses           = ('_INCLUDENUMPASSES');
  {$EXTERNALSYM g_wszWMACIncludeNumPasses}
  g_wszWMVCInverseTelecinedInput      = ('_INVERSETELECINEDINPUT');
  {$EXTERNALSYM g_wszWMVCInverseTelecinedInput}
  g_wszWMVCForcePostProcessMode       = ('_POSTPROCESSMODE');
  {$EXTERNALSYM g_wszWMVCForcePostProcessMode}
  g_wszWMVCNumThreadsDec              = ('_NUMTHREADSDEC');
  {$EXTERNALSYM g_wszWMVCNumThreadsDec}
  g_wszWMVCClosedEntryPoint           = ('_CLOSEDENTRYPOINT');
  {$EXTERNALSYM g_wszWMVCClosedEntryPoint}
  g_wszWMVCQueryTimeStampTagDec       = ('_QUERYTIMESTAMPTAGDEC');
  {$EXTERNALSYM g_wszWMVCQueryTimeStampTagDec}
  g_wszWMVCSupportOneInMultiOut_Dec   = ('_SUPPORTONEINMULTIOUT_DEC');
  {$EXTERNALSYM g_wszWMVCSupportOneInMultiOut_Dec}
  g_wszWMVCEncodedWidth_Dec           = ('_ENCODEDWIDTH_DEC');
  {$EXTERNALSYM g_wszWMVCEncodedWidth_Dec}
  g_wszWMVCEncodedHeight_Dec          = ('_ENCODEDHEIGHT_DEC');
  {$EXTERNALSYM g_wszWMVCEncodedHeight_Dec}
  g_wszWMVCNeedsDrain                 = ('_DECODERNEEDSDRAIN');
  {$EXTERNALSYM g_wszWMVCNeedsDrain}
  g_wszWMVCEncodercomplexity          = ('_ENCODERCOMPLEXITY');
  {$EXTERNALSYM g_wszWMVCEncodercomplexity}
  g_wszWMVCUserdatasize               = ('_USERDATASIZE');
  {$EXTERNALSYM g_wszWMVCUserdatasize}
  g_wszWMVCLetterboxpresent           = ('_LETTERBOXPRESENT');
  {$EXTERNALSYM g_wszWMVCLetterboxpresent}
  g_wszWMVCScenechange                = ('_SCENECHANGE');
  {$EXTERNALSYM g_wszWMVCScenechange}
  g_wszWMVCVariableGOP                = ('_VARIABLEGOP');
  {$EXTERNALSYM g_wszWMVCVariableGOP}
  g_wszWMVCSceneChangeI               = ('_SCENECHANGEI');
  {$EXTERNALSYM g_wszWMVCSceneChangeI}
  g_wszWMVCLookaheadRC                = ('_LOOKAHEADRC');
  {$EXTERNALSYM g_wszWMVCLookaheadRC}
  g_wszWMVCDquantOption               = ('_DQUANTOPTION');
  {$EXTERNALSYM g_wszWMVCDquantOption}
  g_wszWMVCDquantStrength             = ('_DQUANTSTRENGTH');
  {$EXTERNALSYM g_wszWMVCDquantStrength}
  g_wszWMVCForceOverlap               = ('_FORCEOVERLAP');
  {$EXTERNALSYM g_wszWMVCForceOverlap}
  g_wszWMVCMotionVectorCostMethod     = ('_MOTIONVECTORCOSTMETHOD');
  {$EXTERNALSYM g_wszWMVCMotionVectorCostMethod}
  g_wszWMVForceStartCode              = ('_FORCESTARTCODE');
  {$EXTERNALSYM g_wszWMVForceStartCode}
  g_wszWMVCHonorKeyFrameSettings      = ('_HONORKEYSETTINGS');
  {$EXTERNALSYM g_wszWMVCHonorKeyFrameSettings}
  g_wszWMVCHonorTSFrameQP             = ('_HONORTSFRAMEQP');
  {$EXTERNALSYM g_wszWMVCHonorTSFrameQP}
  g_wszWMVDisplayWidth                = ('_DisplayWidth');
  {$EXTERNALSYM g_wszWMVDisplayWidth}
  g_wszWMVDisplayHeight               = ('_DisplayHeight');
  {$EXTERNALSYM g_wszWMVDisplayHeight}
  g_wszWMVEncodeWidth                 = ('_EncodeWidth');
  {$EXTERNALSYM g_wszWMVEncodeWidth}
  g_wszWMVEncodeHeight                = ('_EncodeHeight');
  {$EXTERNALSYM g_wszWMVEncodeHeight}
  g_wszWMVTranscodeMode               = ('_TranscodeMode');
  {$EXTERNALSYM g_wszWMVTranscodeMode}
  g_wszWMVAspectHorizSize             = ('_AspectHorizSize');
  {$EXTERNALSYM g_wszWMVAspectHorizSize}
  g_wszWMVAspectVertSize              = ('_AspectVertSize');
  {$EXTERNALSYM g_wszWMVAspectVertSize}
  g_wszWMVTimeStampFixed              = ('TSFixed');
  {$EXTERNALSYM g_wszWMVTimeStampFixed}
  g_wszWMACMusicSpeechClassMode       = ('MusicSpeechClassMode');
  {$EXTERNALSYM g_wszWMACMusicSpeechClassMode}
  g_wszWMACVoiceBuffer                = ('BufferWindow');
  {$EXTERNALSYM g_wszWMACVoiceBuffer}
  g_wszWMACVoiceEDL                   = ('_EDL');
  {$EXTERNALSYM g_wszWMACVoiceEDL}
  //g_wszSpeechFormatCaps               = ('SpeechFormatCap');   //re-declared

type
  PWmvDynamicFlags = ^WMV_DYNAMIC_FLAGS;
  WMV_DYNAMIC_FLAGS        = DWord;
  {$EXTERNALSYM WMV_DYNAMIC_FLAGS}
const
  WMV_DYNAMIC_BITRATE    = WMV_DYNAMIC_FLAGS($1);
  {$EXTERNALSYM WMV_DYNAMIC_BITRATE}
  WMV_DYNAMIC_RESOLUTION = WMV_DYNAMIC_FLAGS($2);
  {$EXTERNALSYM WMV_DYNAMIC_RESOLUTION}
  WMV_DYNAMIC_COMPLEXITY = WMV_DYNAMIC_FLAGS($4);
  {$EXTERNALSYM WMV_DYNAMIC_COMPLEXITY}

type
  PMF_AUVRHP_ROOMMODEL = ^MF_AUVRHP_ROOMMODEL;
  PMfAuvrhpRoommodel = ^MF_AUVRHP_ROOMMODEL;
  MF_AUVRHP_ROOMMODEL   = DWord;
  {$EXTERNALSYM MF_AUVRHP_ROOMMODEL}
const
  VRHP_SMALLROOM      = MF_AUVRHP_ROOMMODEL(0);
  {$EXTERNALSYM VRHP_SMALLROOM}
  VRHP_MEDIUMROOM     = VRHP_SMALLROOM + 1;
  {$EXTERNALSYM VRHP_MEDIUMROOM}
  VRHP_BIGROOM        = VRHP_MEDIUMROOM + 1;
  {$EXTERNALSYM VRHP_BIGROOM}
  VRHP_CUSTUMIZEDROOM = VRHP_BIGROOM + 1;
  {$EXTERNALSYM VRHP_CUSTUMIZEDROOM}


const

  AEC_MAX_SYSTEM_MODES                = 6;
  {$EXTERNALSYM AEC_MAX_SYSTEM_MODES}

type
  PAEC_SYSTEM_MODE = ^AEC_SYSTEM_MODE;
  PAecSystemMode = ^AEC_SYSTEM_MODE;
  AEC_SYSTEM_MODE          = DWord; // VT_I4
  {$EXTERNALSYM AEC_SYSTEM_MODE}
const
  SINGLE_CHANNEL_AEC     = AEC_SYSTEM_MODE(0);
  {$EXTERNALSYM SINGLE_CHANNEL_AEC}
  ADAPTIVE_ARRAY_ONLY    = SINGLE_CHANNEL_AEC + 1;
  {$EXTERNALSYM ADAPTIVE_ARRAY_ONLY}
  OPTIBEAM_ARRAY_ONLY    = ADAPTIVE_ARRAY_ONLY + 1;
  {$EXTERNALSYM OPTIBEAM_ARRAY_ONLY}
  ADAPTIVE_ARRAY_AND_AEC = OPTIBEAM_ARRAY_ONLY + 1;
  {$EXTERNALSYM ADAPTIVE_ARRAY_AND_AEC}
  OPTIBEAM_ARRAY_AND_AEC = ADAPTIVE_ARRAY_AND_AEC + 1;
  {$EXTERNALSYM OPTIBEAM_ARRAY_AND_AEC}
  SINGLE_CHANNEL_NSAGC   = OPTIBEAM_ARRAY_AND_AEC + 1;
  {$EXTERNALSYM SINGLE_CHANNEL_NSAGC}
  MODE_NOT_SET           = SINGLE_CHANNEL_NSAGC + 1;
  {$EXTERNALSYM MODE_NOT_SET}


type
  PAecQualityMetricsStruct = ^tagAecQualityMetrics_Struct;
  tagAecQualityMetrics_Struct = record
    i64Timestamp: LONGLONG;
    ConvergenceFlag: Byte;
    MicClippedFlag: Byte;
    MicSilenceFlag: Byte;
    PstvFeadbackFlag: Byte;
    SpkClippedFlag: Byte;
    SpkMuteFlag: Byte;
    GlitchFlag: Byte;
    DoubleTalkFlag: Byte;
    uGlitchCount: ULONG;
    uMicClipCount: ULONG;
    fDuration: Single;
    fTSVariance: Single;
    fTSDriftRate: Single;
    fVoiceLevel: Single;
    fNoiseLevel: Single;
    fERLE: Single;
    fAvgERLE: Single;
    dwReserved: DWORD;
  end;
  {$EXTERNALSYM tagAecQualityMetrics_Struct}
  AecQualityMetrics_Struct = tagAecQualityMetrics_Struct;
  {$EXTERNALSYM AecQualityMetrics_Struct}

type
  PAEC_VAD_MODE = ^AEC_VAD_MODE;
  PAecVadMode = ^AEC_VAD_MODE;
  AEC_VAD_MODE = DWord;
  {$EXTERNALSYM AEC_VAD_MODE}
const
    AEC_VAD_DISABLED                = AEC_VAD_MODE(0);
    {$EXTERNALSYM AEC_VAD_DISABLED}
    AEC_VAD_NORMAL                  = AEC_VAD_DISABLED + 1;
    {$EXTERNALSYM AEC_VAD_NORMAL}
    AEC_VAD_FOR_AGC                 = AEC_VAD_NORMAL + 1;
    {$EXTERNALSYM AEC_VAD_FOR_AGC}
    AEC_VAD_FOR_SILENCE_SUPPRESSION = AEC_VAD_FOR_AGC + 1;
    {$EXTERNALSYM AEC_VAD_FOR_SILENCE_SUPPRESSION}


type
  PAEC_INPUT_STREAM = ^AEC_INPUT_STREAM;
  PAecInputStream = ^AEC_INPUT_STREAM;
  AEC_INPUT_STREAM = DWord;
  {$EXTERNALSYM AEC_INPUT_STREAM}
const
  AEC_CAPTURE_STREAM   = AEC_INPUT_STREAM(0);
  AEC_REFERENCE_STREAM = AEC_INPUT_STREAM(1);


type
  PMIC_ARRAY_MODE = ^MIC_ARRAY_MODE;
  PMicArrayMode = ^MIC_ARRAY_MODE;
  MIC_ARRAY_MODE = DWord;
  {$EXTERNALSYM MIC_ARRAY_MODE}
const
  MICARRAY_SINGLE_CHAN = MIC_ARRAY_MODE(0);
  {$EXTERNALSYM MICARRAY_SINGLE_CHAN}
  MICARRAY_SIMPLE_SUM  = MIC_ARRAY_MODE($100);
  {$EXTERNALSYM MICARRAY_SIMPLE_SUM}
  MICARRAY_SINGLE_BEAM = MIC_ARRAY_MODE($200);
  {$EXTERNALSYM MICARRAY_SINGLE_BEAM}
  MICARRAY_FIXED_BEAM  = MIC_ARRAY_MODE($400);
  {$EXTERNALSYM MICARRAY_FIXED_BEAM}
  MICARRAY_EXTERN_BEAM = MIC_ARRAY_MODE($800);
  {$EXTERNALSYM MICARRAY_EXTERN_BEAM}


const

  WMAAECMA_E_NO_ACTIVE_RENDER_STREAM  = $87CC000A;
  {$EXTERNALSYM WMAAECMA_E_NO_ACTIVE_RENDER_STREAM}


  MEDIASUBTYPE_Y41T             :  TGUID = '{54313459-0000-0010-8000-00aa00389b70}';
  {$EXTERNALSYM MEDIASUBTYPE_Y41T}
  MEDIASUBTYPE_Y42T             :  TGUID = '{54323459-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_Y42T}

{$IFNDEF MEDIASUBTYPE_NV11_DEFINED}
  MEDIASUBTYPE_NV11             :  TGUID = '{3131564E-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_NV11}
{$DEFINE MEDIASUBTYPE_NV11_DEFINED}
{$ENDIF}

  // The programming language of C++ and C are case sensitive.
  // In both commands and variable names are case sensitive.
  // Syntax Example:
  // The following standard C++ codesnippet works:
  // printf("test\n"); > Ok
  // Printf("test\n"); > Will not work!
  // So here we have to tackle this problem, because Delphi is not case sensitive, and
  // will generate an error (Identifier redeclared)

  MEDIASUBTYPE_V216                     :  TGUID = '{36313256-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_V216}
  MEDIASUBTYPE_V410                     :  TGUID = '{30313456-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_V410}
  MEDIASUBTYPE_v210                     :  TGUID = '{30313276-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_v210}
  MEDIASUBTYPE_I420                     :  TGUID = '{30323449-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_I420}

  MEDIASUBTYPE_WVC1                     :  TGUID = '{31435657-0000-0010-8000-00aa00389b71}'; // or '{31637677-0000-0010-8000-00aa00389b71}'
  {$EXTERNALSYM MEDIASUBTYPE_WVC1}
  _MEDIASUBTYPE_wvc1                    :  TGUID = '{31637677-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wvc1}

  MEDIASUBTYPE_WMVA                     :  TGUID = '{41564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMVA}
  _MEDIASUBTYPE_wmva                    :  TGUID = '{61766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmva}
  MEDIASUBTYPE_WMVB                     :  TGUID = '{42564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMVB}
  _MEDIASUBTYPE_wmvb                    :  TGUID = '{62766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmvb}
  MEDIASUBTYPE_WMVR                     :  TGUID = '{52564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMVR}
  _MEDIASUBTYPE_wmvr                    :  TGUID = '{72766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmvr}
  MEDIASUBTYPE_WMVP                     :  TGUID = '{50564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMVP}
  _MEDIASUBTYPE_wmvp                    :  TGUID = '{70766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmvp}
  MEDIASUBTYPE_WVP2                     :  TGUID = '{32505657-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WVP2}
  _MEDIASUBTYPE_wvp2                    :  TGUID = '{32707677-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wvp2}
  MEDIASUBTYPE_WMV3                     :  TGUID = '{33564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMV3}
  _MEDIASUBTYPE_wmv3                    :  TGUID = '{33766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmv3}
  MEDIASUBTYPE_WMV2                     :  TGUID = '{32564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMV2}
  _MEDIASUBTYPE_wmv2                    :  TGUID = '{32766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmv2}
  MEDIASUBTYPE_WMV1                     :  TGUID = '{31564D57-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMV1}
  _MEDIASUBTYPE_wmv1                    :  TGUID = '{31766D77-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_wmv1}
  MEDIASUBTYPE_MPG4                     :  TGUID = '{3447504D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MPG4}
  _MEDIASUBTYPE_mpg4                    :  TGUID = '{3467706D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_mpg4}
  MEDIASUBTYPE_MP42                     :  TGUID = '{3234504D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MP42}
  _MEDIASUBTYPE_mp42                    :  TGUID = '{3234706D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_mp42}
  MEDIASUBTYPE_MP43                     :  TGUID = '{3334504D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MP43}
  _MEDIASUBTYPE_mp43                    :  TGUID = '{3334706D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_mp43}
  MEDIASUBTYPE_MP4S                     :  TGUID = '{5334504D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MP4S}
  _MEDIASUBTYPE_mp4s                    :  TGUID = '{7334706D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_mp4s}
  MEDIASUBTYPE_M4S2                     :  TGUID = '{3253344D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_M4S2}
  _MEDIASUBTYPE_m4s2                    :  TGUID = '{3273346D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_m4s2}
  MEDIASUBTYPE_MSS1                     :  TGUID = '{3153534D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MSS1}
  MEDIASUBTYPE_MSS2                     :  TGUID = '{3253534D-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MSS2}
  MEDIASUBTYPE_MSAUDIO1                 :  TGUID = '{00000160-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MSAUDIO1}
  MEDIASUBTYPE_WMAUDIO2                 :  TGUID = '{00000161-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMAUDIO2}
  MEDIASUBTYPE_WMAUDIO3                 :  TGUID = '{00000162-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMAUDIO3}
  MEDIASUBTYPE_WMAUDIO_LOSSLESS         :  TGUID = '{00000163-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMAUDIO_LOSSLESS}
  MEDIASUBTYPE_WMASPDIF                 :  TGUID = '{00000164-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMASPDIF}
  MEDIASUBTYPE_WMAUDIO4                 :  TGUID = '{00000168-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_WMAUDIO4}
  MEDIASUBTYPE_MPEG_ADTS_AAC            :  TGUID = '{00001600-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MPEG_ADTS_AAC}
  MEDIASUBTYPE_MPEG_RAW_AAC             :  TGUID = '{00001601-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MPEG_RAW_AAC}
  MEDIASUBTYPE_MPEG_LOAS                :  TGUID = '{00001602-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MPEG_LOAS}
  MEDIASUBTYPE_NOKIA_MPEG_ADTS_AAC      :  TGUID = '{00001608-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_NOKIA_MPEG_ADTS_AAC}
  MEDIASUBTYPE_NOKIA_MPEG_RAW_AAC       :  TGUID = '{00001609-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_NOKIA_MPEG_RAW_AAC}
  MEDIASUBTYPE_VODAFONE_MPEG_ADTS_AAC   :  TGUID = '{0000160A-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_VODAFONE_MPEG_ADTS_AAC}
  MEDIASUBTYPE_VODAFONE_MPEG_RAW_AAC    :  TGUID = '{0000160B-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_VODAFONE_MPEG_RAW_AAC}
  MEDIASUBTYPE_MPEG_HEAAC               :  TGUID = '{00001610-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_MPEG_HEAAC}
  MEDIASUBTYPE_RAW_AAC1                 :  TGUID = '{000000FF-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_RAW_AAC1}
  MEDIASUBTYPE_DVM                      :  TGUID = '{00002000-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_DVM}
  MEDIASUBTYPE_DTS2                     :  TGUID = '{00002001-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_DTS2}
  MEDIASUBTYPE_DOLBY_DDPLUS             :  TGUID = '{a7fb87af-2d02-42fb-a4d4-05cd93843bdd}';
  {$EXTERNALSYM MEDIASUBTYPE_DOLBY_DDPLUS}
  MEDIASUBTYPE_DOLBY_TRUEHD             :  TGUID = '{eb27cec4-163e-4ca3-8b74-8e25f91b517e}';
  {$EXTERNALSYM MEDIASUBTYPE_DOLBY_TRUEHD}
  MEDIASUBTYPE_DTS_HD                   :  TGUID = '{a2e58eb7-0fa9-48bb-a40c-fa0e156d0645}';
  {$EXTERNALSYM MEDIASUBTYPE_DTS_HD}
  MEDIASUBTYPE_DTS_HD_HRA               :  TGUID = '{A61AC364-AD0E-4744-89FF-213CE0DF8804}';
  {$EXTERNALSYM MEDIASUBTYPE_DTS_HD_HRA}
  MEDIASUBTYPE_h264                     :  TGUID = '{34363268-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_h264}
  MEDIASUBTYPE_AVC1                     :  TGUID = '{31435641-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_AVC1}
  MEDIASUBTYPE_X264                     :  TGUID = '{34363258-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MEDIASUBTYPE_X264}
  _MEDIASUBTYPE_x264                    :  TGUID = '{34363278-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM _MEDIASUBTYPE_x264}


type
  PMFVideoDSPMode = ^MFVideoDSPMode;
  _MFVideoDSPMode                = UINT32;
  {$EXTERNALSYM _MFVideoDSPMode}
  MFVideoDSPMode = _MFVideoDSPMode;
  {$EXTERNALSYM MFVideoDSPMode}
const
  MFVideoDSPMode_Passthrough   = MFVideoDSPMode(1);
  {$EXTERNALSYM MFVideoDSPMode_Passthrough}
  MFVideoDSPMode_Stabilization = MFVideoDSPMode(4);
  {$EXTERNALSYM MFVideoDSPMode_Stabilization}


const

  MF_VIDEODSP_MODE                :  TGUID = '{16d720f0-768c-11de-8a39-0800200c9a66}';
  {$EXTERNALSYM MF_VIDEODSP_MODE}
  MFSampleExtension_VideoDSPMode  :  TGUID = '{c12d55cb-d7d9-476d-81f3-69117f163ea0}';
  {$EXTERNALSYM MFSampleExtension_VideoDSPMode}
  CLSID_CTocEntry                 :  TGUID = '{F22F5E05-585C-4def-8523-6555CFBC0CB3}';
  {$EXTERNALSYM CLSID_CTocEntry}
  CLSID_CTocEntryList             :  TGUID = '{3A8CCCBC-0EFD-43a3-B838-F38A552BA237}';
  {$EXTERNALSYM CLSID_CTocEntryList}
  CLSID_CToc                      :  TGUID = '{4FE24495-28CE-4920-A4C4-E556E1F0DF2A}';
  {$EXTERNALSYM CLSID_CToc}
  CLSID_CTocCollection            :  TGUID = '{5058292D-A244-4840-AB44-480975C4FFE4}';
  {$EXTERNALSYM CLSID_CTocCollection}
  CLSID_CTocParser                :  TGUID = '{499EAEEA-2737-4849-8BB6-47F107EAF358}';
  {$EXTERNALSYM CLSID_CTocParser}
  CLSID_CAsfTocParser             :  TGUID = '{9B77C0F2-8735-46c5-B90F-5F0B303EF6AB}';
  {$EXTERNALSYM CLSID_CAsfTocParser}
  CLSID_CAviTocParser             :  TGUID = '{3ADCE5CC-13C8-4573-B328-ED438EB694F9}';
  {$EXTERNALSYM CLSID_CAviTocParser}
  CLSID_CFileIo                   :  TGUID = '{11993195-1244-4840-AB44-480975C4FFE4}';
  {$EXTERNALSYM CLSID_CFileIo}
  CLSID_CFileClient               :  TGUID = '{BFCCD195-1244-4840-AB44-480975C4FFE4}';
  {$EXTERNALSYM CLSID_CFileClient}
  CLSID_CClusterDetectorEx        :  TGUID = '{47354492-827E-4b8a-B318-C80EBA1381F0}';
  {$EXTERNALSYM CLSID_CClusterDetectorEx}

type

{$IFNDEF _QWORD_DEFINED}
  PQword = ^QWORD;
  QWORD = UInt64;
  {$EXTERNALSYM QWORD}
{$DEFINE _QWORD_DEFINED}
{$ENDIF} //QWORD

const

{$IFNDEF E_TOCPARSER_INVALIDASFFILE_DEFINED}
  E_TOCPARSER_INVALIDASFFILE          = _HRESULT_TYPEDEF_ ($99000001);
  {$EXTERNALSYM E_TOCPARSER_INVALIDASFFILE}
{$DEFINE E_TOCPARSER_INVALIDASFFILE_DEFINED}
{$ENDIF} //E_TOCPARSER_INVALIDASFFILE

{$IFNDEF E_TOCPARSER_INVALIDRIFFFILE_DEFINED}
 E_TOCPARSER_INVALIDRIFFFILE         = _HRESULT_TYPEDEF_ ($99000002);
 {$EXTERNALSYM E_TOCPARSER_INVALIDRIFFFILE}
{$DEFINE E_TOCPARSER_INVALIDRIFFFILE_DEFINED}
{$ENDIF} //E_TOCPARSER_INVALIDRIFFFILE

{$IFNDEF TOC_MAX_DESCRIPTION_SIZE_DEFINED}
  TOC_MAX_DESCRIPTION_SIZE           = 65535;
{$DEFINE TOC_MAX_DESCRIPTION_SIZE_DEFINED}
{$ENDIF} //TOC_MAX_DESCRIPTION_SIZE

{$IFNDEF TOC_ENTRY_MAX_TITLE_SIZE_DEFINED}
  TOC_ENTRY_MAX_TITLE_SIZE           = 65535;
  {$EXTERNALSYM TOC_ENTRY_MAX_TITLE_SIZE}
{$DEFINE TOC_ENTRY_MAX_TITLE_SIZE_DEFINED}
{$ENDIF} //TOC_ENTRY_MAX_TITLE_SIZE


const

  CLSID_CMpeg4DecMediaObject    :  TGUID = '{f371728a-6052-4d47-827c-d039335dfe0a}';
  {$EXTERNALSYM CLSID_CMpeg4DecMediaObject}
  CLSID_CMpeg43DecMediaObject   :  TGUID = '{cba9e78b-49a3-49ea-93d4-6bcba8c4de07}';
  {$EXTERNALSYM CLSID_CMpeg43DecMediaObject}
  CLSID_CMpeg4sDecMediaObject   :  TGUID = '{2a11bae2-fe6e-4249-864b-9e9ed6e8dbc2}';
  {$EXTERNALSYM CLSID_CMpeg4sDecMediaObject}
  CLSID_CMpeg4sDecMFT           :  TGUID = '{5686a0d9-fe39-409f-9dff-3fdbc849f9f5}';
  {$EXTERNALSYM CLSID_CMpeg4sDecMFT}
  CLSID_CZuneM4S2DecMediaObject :  TGUID = '{C56FC25C-0FC6-404a-9503-B10BF51A8AB9}';
  {$EXTERNALSYM CLSID_CZuneM4S2DecMediaObject}
  CLSID_CMpeg4EncMediaObject    :  TGUID = '{24f258d8-c651-4042-93e4-ca654abb682c}';
  {$EXTERNALSYM CLSID_CMpeg4EncMediaObject}
  CLSID_CMpeg4sEncMediaObject   :  TGUID = '{6ec5a7be-d81e-4f9e-ada3-cd1bf262b6d8}';
  {$EXTERNALSYM CLSID_CMpeg4sEncMediaObject}
  CLSID_CMSSCDecMediaObject     :  TGUID = '{7bafb3b1-d8f4-4279-9253-27da423108de}';
  {$EXTERNALSYM CLSID_CMSSCDecMediaObject}
  CLSID_CMSSCEncMediaObject     :  TGUID = '{8cb9cc06-d139-4ae6-8bb4-41e612e141d5}';
  {$EXTERNALSYM CLSID_CMSSCEncMediaObject}
  CLSID_CMSSCEncMediaObject2    :  TGUID = '{f7ffe0a0-a4f5-44b5-949e-15ed2bc66f9d}';
  {$EXTERNALSYM CLSID_CMSSCEncMediaObject2}
  CLSID_CWMADecMediaObject      :  TGUID = '{2eeb4adf-4578-4d10-bca7-bb955f56320a}';
  {$EXTERNALSYM CLSID_CWMADecMediaObject}
  CLSID_CWMAEncMediaObject      :  TGUID = '{70f598e9-f4ab-495a-99e2-a7c4d3d89abf}';
  {$EXTERNALSYM CLSID_CWMAEncMediaObject}
  CLSID_CWMATransMediaObject    :  TGUID = '{edcad9cb-3127-40df-b527-0152ccb3f6f5}';
  {$EXTERNALSYM CLSID_CWMATransMediaObject}
  CLSID_CWMSPDecMediaObject     :  TGUID = '{874131cb-4ecc-443b-8948-746b89595d20}';
  {$EXTERNALSYM CLSID_CWMSPDecMediaObject}
  CLSID_CWMSPEncMediaObject     :  TGUID = '{67841b03-c689-4188-ad3f-4c9ebeec710b}';
  {$EXTERNALSYM CLSID_CWMSPEncMediaObject}
  CLSID_CWMSPEncMediaObject2    :  TGUID = '{1f1f4e1a-2252-4063-84bb-eee75f8856d5}';
  {$EXTERNALSYM CLSID_CWMSPEncMediaObject2}
  CLSID_CWMTDecMediaObject      :  TGUID = '{F9DBC64E-2DD0-45dd-9B52-66642EF94431}';
  {$EXTERNALSYM CLSID_CWMTDecMediaObject}
  CLSID_CWMTEncMediaObject      :  TGUID = '{60B67652-E46B-4e44-8609-F74BFFDC083C}';
  {$EXTERNALSYM CLSID_CWMTEncMediaObject}
  CLSID_CWMVDecMediaObject      :  TGUID = '{82d353df-90bd-4382-8bc2-3f6192b76e34}';
  {$EXTERNALSYM CLSID_CWMVDecMediaObject}
  CLSID_CWMVEncMediaObject2     :  TGUID = '{96b57cdd-8966-410c-bb1f-c97eea765c04}';
  {$EXTERNALSYM CLSID_CWMVEncMediaObject2}
  CLSID_CWMVXEncMediaObject     :  TGUID = '{7e320092-596a-41b2-bbeb-175d10504eb6}';
  {$EXTERNALSYM CLSID_CWMVXEncMediaObject}
  CLSID_CWMV9EncMediaObject     :  TGUID = '{d23b90d0-144f-46bd-841d-59e4eb19dc59}';
  {$EXTERNALSYM CLSID_CWMV9EncMediaObject}
  CLSID_CWVC1DecMediaObject     :  TGUID = '{c9bfbccf-e60e-4588-a3df-5a03b1fd9585}';
  {$EXTERNALSYM CLSID_CWVC1DecMediaObject}
  CLSID_CWVC1EncMediaObject     :  TGUID = '{44653D0D-8CCA-41e7-BACA-884337B747AC}';
  {$EXTERNALSYM CLSID_CWVC1EncMediaObject}
  CLSID_CDeColorConvMediaObject :  TGUID = '{49034c05-f43c-400f-84c1-90a683195a3a}';
  {$EXTERNALSYM CLSID_CDeColorConvMediaObject}
  CLSID_CDVEncoderMediaObject   :  TGUID = '{c82ae729-c327-4cce-914d-8171fefebefb}';
  {$EXTERNALSYM CLSID_CDVEncoderMediaObject}
  CLSID_CMpeg2DecMediaObject    :  TGUID = '{863d66cd-cdce-4617-b47f-c8929cfc28a6}';
  {$EXTERNALSYM CLSID_CMpeg2DecMediaObject}
  CLSID_CPK_DS_MPEG2Decoder     :  TGUID = '{9910c5cd-95c9-4e06-865a-efa1c8016bf4}';
  {$EXTERNALSYM CLSID_CPK_DS_MPEG2Decoder}
  CLSID_CAC3DecMediaObject      :  TGUID = '{03d7c802-ecfa-47d9-b268-5fb3e310dee4}';
  {$EXTERNALSYM CLSID_CAC3DecMediaObject}
  CLSID_CPK_DS_AC3Decoder       :  TGUID = '{6c9c69d6-0ffc-4481-afdb-cdf1c79c6f3e}';
  {$EXTERNALSYM CLSID_CPK_DS_AC3Decoder}
  CLSID_CMP3DecMediaObject      :  TGUID = '{bbeea841-0a63-4f52-a7ab-a9b3a84ed38a}';
  {$EXTERNALSYM CLSID_CMP3DecMediaObject}
  CLSID_CResamplerMediaObject   :  TGUID = '{f447b69e-1884-4a7e-8055-346f74d6edb3}';
  {$EXTERNALSYM CLSID_CResamplerMediaObject}
  CLSID_CResizerMediaObject     :  TGUID = '{d3ec8b8b-7728-4fd8-9fe0-7b67d19f73a3}';
  {$EXTERNALSYM CLSID_CResizerMediaObject}
  CLSID_CInterlaceMediaObject   :  TGUID = '{b5a89c80-4901-407b-9abc-90d9a644bb46}';
  {$EXTERNALSYM CLSID_CInterlaceMediaObject}
  CLSID_CWMAudioLFXAPO          :  TGUID = '{62dc1a93-ae24-464c-a43e-452f824c4250}';
  {$EXTERNALSYM CLSID_CWMAudioLFXAPO}
  CLSID_CWMAudioGFXAPO          :  TGUID = '{637c490d-eee3-4c0a-973f-371958802da2}';
  {$EXTERNALSYM CLSID_CWMAudioGFXAPO}
  CLSID_CWMAudioSpdTxDMO        :  TGUID = '{5210f8e4-b0bb-47c3-a8d9-7b2282cc79ed}';
  {$EXTERNALSYM CLSID_CWMAudioSpdTxDMO}
  CLSID_CWMAudioAEC             :  TGUID = '{745057c7-f353-4f2d-a7ee-58434477730e}';
  {$EXTERNALSYM CLSID_CWMAudioAEC}
  CLSID_CClusterDetectorDmo     :  TGUID = '{36e820c4-165a-4521-863c-619e1160d4d4}';
  {$EXTERNALSYM CLSID_CClusterDetectorDmo}
  CLSID_CColorControlDmo        :  TGUID = '{798059f0-89ca-4160-b325-aeb48efe4f9a}';
  {$EXTERNALSYM CLSID_CColorControlDmo}
  CLSID_CColorConvertDMO        :  TGUID = '{98230571-0087-4204-b020-3282538e57d3}';
  {$EXTERNALSYM CLSID_CColorConvertDMO}
  CLSID_CColorLegalizerDmo      :  TGUID = '{fdfaa753-e48e-4e33-9c74-98a27fc6726a}';
  {$EXTERNALSYM CLSID_CColorLegalizerDmo}
  CLSID_CFrameInterpDMO         :  TGUID = '{0a7cfe1b-6ab5-4334-9ed8-3f97cb37daa1}';
  {$EXTERNALSYM CLSID_CFrameInterpDMO}
  CLSID_CFrameRateConvertDmo    :  TGUID = '{01f36ce2-0907-4d8b-979d-f151be91c883}';
  {$EXTERNALSYM CLSID_CFrameRateConvertDmo}
  CLSID_CResizerDMO             :  TGUID = '{1ea1ea14-48f4-4054-ad1a-e8aee10ac805}';
  {$EXTERNALSYM CLSID_CResizerDMO}
  CLSID_CShotDetectorDmo        :  TGUID = '{56aefacd-110c-4397-9292-b0a0c61b6750}';
  {$EXTERNALSYM CLSID_CShotDetectorDmo}
  CLSID_CSmpteTransformsDmo     :  TGUID = '{bde6388b-da25-485d-ba7f-fabc28b20318}';
  {$EXTERNALSYM CLSID_CSmpteTransformsDmo}
  CLSID_CThumbnailGeneratorDmo  :  TGUID = '{559c6bad-1ea8-4963-a087-8a6810f9218b}';
  {$EXTERNALSYM CLSID_CThumbnailGeneratorDmo}
  CLSID_CTocGeneratorDmo        :  TGUID = '{4dda1941-77a0-4fb1-a518-e2185041d70c}';
  {$EXTERNALSYM CLSID_CTocGeneratorDmo}
  CLSID_CMPEGAACDecMediaObject  :  TGUID = '{8DDE1772-EDAD-41c3-B4BE-1F30FB4EE0D6}';
  {$EXTERNALSYM CLSID_CMPEGAACDecMediaObject}
  CLSID_CNokiaAACDecMediaObject :  TGUID = '{3CB2BDE4-4E29-4c44-A73E-2D7C2C46D6EC}';
  {$EXTERNALSYM CLSID_CNokiaAACDecMediaObject}
  CLSID_CVodafoneAACDecMediaObject   :  TGUID = '{7F36F942-DCF3-4d82-9289-5B1820278F7C}';
  {$EXTERNALSYM CLSID_CVodafoneAACDecMediaObject}
  CLSID_CZuneAACCCDecMediaObject     :  TGUID = '{A74E98F2-52D6-4b4e-885B-E0A6CA4F187A}';
  {$EXTERNALSYM CLSID_CZuneAACCCDecMediaObject}
  CLSID_CNokiaAACCCDecMediaObject    :  TGUID = '{EABF7A6F-CCBA-4d60-8620-B152CC977263}';
  {$EXTERNALSYM CLSID_CNokiaAACCCDecMediaObject}
  CLSID_CVodafoneAACCCDecMediaObject :  TGUID = '{7E76BF7F-C993-4e26-8FAB-470A70C0D59C}';
  {$EXTERNALSYM CLSID_CVodafoneAACCCDecMediaObject}
  CLSID_CMPEG2EncoderDS          :  TGUID = '{5F5AFF4A-2F7F-4279-88C2-CD88EB39D144}';
  {$EXTERNALSYM CLSID_CMPEG2EncoderDS}
  CLSID_CMPEG2EncoderVideoDS     :  TGUID = '{42150cd9-ca9a-4ea5-9939-30ee037f6e74}';
  {$EXTERNALSYM CLSID_CMPEG2EncoderVideoDS}
  CLSID_CMPEG2EncoderAudioDS     :  TGUID = '{acd453bc-c58a-44d1-bbf5-bfb325be2d78}';
  {$EXTERNALSYM CLSID_CMPEG2EncoderAudioDS}
  CLSID_CMPEG2AudDecoderDS       :  TGUID = '{E1F1A0B8-BEEE-490d-BA7C-066C40B5E2B9}';
  {$EXTERNALSYM CLSID_CMPEG2AudDecoderDS}
  CLSID_CMPEG2VidDecoderDS       :  TGUID = '{212690FB-83E5-4526-8FD7-74478B7939CD}';
  {$EXTERNALSYM CLSID_CMPEG2VidDecoderDS}
  CLSID_CDTVAudDecoderDS         :  TGUID = '{8E269032-FE03-4753-9B17-18253C21722E}';
  {$EXTERNALSYM CLSID_CDTVAudDecoderDS}
  CLSID_CDTVVidDecoderDS         :  TGUID = '{64777DC8-4E24-4beb-9D19-60A35BE1DAAF}';
  {$EXTERNALSYM CLSID_CDTVVidDecoderDS}
  CLSID_CMSAC3Enc                :  TGUID = '{C6B400E2-20A7-4e58-A2FE-24619682CE6C}';
  {$EXTERNALSYM CLSID_CMSAC3Enc}
  CLSID_CMSH264DecoderMFT        :  TGUID = '{62CE7E72-4C71-4d20-B15D-452831A87D9D}';
  {$EXTERNALSYM CLSID_CMSH264DecoderMFT}
  CLSID_CMSH264EncoderMFT        : TGUID = '{6ca50344-051a-4ded-9779-a43305165e35}';
  {$EXTERNALSYM CLSID_CMSH264EncoderMFT}
  CLSID_CMSH265EncoderMFT        : TGUID = '{f2f84074-8bca-40bd-9159-e880f673dd3b}';
  {$EXTERNALSYM CLSID_CMSH265EncoderMFT}
  CLSID_CMSVPXEncoderMFT         : TGUID = '{aeb6c755-2546-4881-82cc-e15ae5ebff3d}';
  {$EXTERNALSYM CLSID_CMSVPXEncoderMFT}
  CLSID_CMSH264RemuxMFT          : TGUID = '{05A47EBB-8BF0-4CBF-AD2F-3B71D75866F5}';
  {$EXTERNALSYM CLSID_CMSH264RemuxMFT}
  CLSID_CMSAACDecMFT             : TGUID = '{32d186a7-218f-4c75-8876-dd77273a8999}';
  {$EXTERNALSYM CLSID_CMSAACDecMFT}
  CLSID_AACMFTEncoder            : TGUID = '{93AF0C51-2275-45d2-A35B-F2BA21CAED00}';
  {$EXTERNALSYM CLSID_AACMFTEncoder}
  CLSID_CMSDDPlusDecMFT          : TGUID = '{177C0AFE-900B-48d4-9E4C-57ADD250B3D4}';
  {$EXTERNALSYM CLSID_CMSDDPlusDecMFT}
  CLSID_CMPEG2VideoEncoderMFT    : TGUID = '{E6335F02-80B7-4dc4-ADFA-DFE7210D20D5}';
  {$EXTERNALSYM CLSID_CMPEG2VideoEncoderMFT}
  CLSID_CMPEG2AudioEncoderMFT    : TGUID = '{46A4DD5C-73F8-4304-94DF-308F760974F4}';
  {$EXTERNALSYM CLSID_CMPEG2AudioEncoderMFT}
  CLSID_CMSMPEGDecoderMFT        : TGUID = '{2D709E52-123F-49b5-9CBC-9AF5CDE28FB9}';
  {$EXTERNALSYM CLSID_CMSMPEGDecoderMFT}
  CLSID_CMSMPEGAudDecMFT         : TGUID = '{70707B39-B2CA-4015-ABEA-F8447D22D88B}';
  {$EXTERNALSYM CLSID_CMSMPEGAudDecMFT}
  CLSID_CMSDolbyDigitalEncMFT    : TGUID = '{AC3315C9-F481-45D7-826C-0B406C1F64B8}';
  {$EXTERNALSYM CLSID_CMSDolbyDigitalEncMFT}
  CLSID_MP3ACMCodecWrapper       : TGUID = '{11103421-354c-4cca-a7a3-1aff9a5b6701}';
  {$EXTERNALSYM CLSID_MP3ACMCodecWrapper}

  CLSID_ALawCodecWrapper         : TGUID = '{36CB6E0C-78C1-42B2-9943-846262F31786}';
  {$EXTERNALSYM CLSID_ALawCodecWrapper}
  CLSID_MULawCodecWrapper        : TGUID = '{92b66080-5e2d-449e-90c4-c41f268e5514}';
  {$EXTERNALSYM CLSID_MULawCodecWrapper}

  CLSID_CMSVideoDSPMFT           : TGUID = '{51571744-7FE4-4ff2-A498-2DC34FF74F1B}';
  {$EXTERNALSYM CLSID_CMSVideoDSPMFT}

  CLSID_VorbisDecoderMFT         : TGUID = '{1A198EF2-60E5-4EA8-90D8-DA1F2832C288}';
  {$EXTERNALSYM CLSID_VorbisDecoderMFT}
  CLSID_CMSFLACDecMFT            : TGUID = '{6B0B3E6B-A2C5-4514-8055-AFE8A95242D9}';
  {$EXTERNALSYM CLSID_CMSFLACDecMFT}
  CLSID_CMSFLACEncMFT            : TGUID = '{128509e9-c44e-45dc-95e9-c255b8f466a6}';
  {$EXTERNALSYM CLSID_CMSFLACEncMFT}
  CLSID_MFFLACBytestreamHandler  : TGUID = '{0E41CFB8-0506-40F4-A516-77CC23642D91}';
  {$EXTERNALSYM CLSID_MFFLACBytestreamHandler}
  CLSID_MFFLACSinkClassFactory   : TGUID = '{7d39c56f-6075-47c9-9bae-8cf9e531b5f5}';
  {$EXTERNALSYM CLSID_MFFLACSinkClassFactory}
  CLSID_CMSALACDecMFT            : TGUID = '{C0CD7D12-31FC-4BBC-B363-7322EE3E1879}';
  {$EXTERNALSYM CLSID_CMSALACDecMFT}
  CLSID_CMSALACEncMFT            : TGUID = '{9AB6A28C-748E-4B6A-BFFF-CC443B8E8FB4}';
  {$EXTERNALSYM CLSID_CMSALACEncMFT}
  CLSID_CMSOpusDecMFT            : TGUID = '{63e17c10-2d43-4c42-8fe3-8d8b63e46a6a}';
  {$EXTERNALSYM CLSID_CMSOpusDecMFT}
  CLSID_MSAMRNBDecoder           : TGUID = '{265011AE-5481-4f77-A295-ABB6FFE8D63E}';
  {$EXTERNALSYM CLSID_MSAMRNBDecoder}
  CLSID_MSAMRNBEncoder           : TGUID = '{2FAE8AFE-04A3-423a-A814-85DB454712B0}';
  {$EXTERNALSYM CLSID_MSAMRNBEncoder}
  CLSID_MFAMRNBByteStreamHandler : TGUID = '{EFE6208A-0A2C-49FA-8A01-3768B559B6DA}';
  {$EXTERNALSYM CLSID_MFAMRNBByteStreamHandler}
  CLSID_MFAMRNBSinkClassFactory  : TGUID = '{B0271158-70D2-4C5B-9F94-76F549D90FDF}';
  {$EXTERNALSYM CLSID_MFAMRNBSinkClassFactory}


type

  POC_DESCRIPTOR = ^TOC_DESCRIPTOR;
  PTocDescriptor = ^TOC_DESCRIPTOR;
  _TOC_DESCRIPTOR = record
    guidID: TGUID;
    wStreamNumber: WORD;
    guidType: TGUID;
    wLanguageIndex: WORD;
  end;
  {$EXTERNALSYM _TOC_DESCRIPTOR}
  TOC_DESCRIPTOR = _TOC_DESCRIPTOR;
  {$EXTERNALSYM TOC_DESCRIPTOR}


  POC_ENTRY_DESCRIPTOR = ^TOC_ENTRY_DESCRIPTOR;
  PTocEntryDescriptor = ^TOC_ENTRY_DESCRIPTOR;
  _TOC_ENTRY_DESCRIPTOR = record
    qwStartTime: QWORD;
    qwEndTime: QWORD;
    qwStartPacketOffset: QWORD;
    qwEndPacketOffset: QWORD;
    qwRepresentativeFrameTime: QWORD;
  end;
  {$EXTERNALSYM _TOC_ENTRY_DESCRIPTOR}
  TOC_ENTRY_DESCRIPTOR = _TOC_ENTRY_DESCRIPTOR;
  {$EXTERNALSYM TOC_ENTRY_DESCRIPTOR}


type
  POC_POS_TYPE = ^TOC_POS_TYPE;
  TOC_POS_TYPE = DWord;
  {$EXTERNALSYM TOC_POS_TYPE}
const
  TOC_POS_INHEADER  = DWord(0);
  {$EXTERNALSYM TOC_POS_INHEADER}
  TOC_POS_TOPLEVELOBJECT  = TOC_POS_INHEADER + 1;
  {$EXTERNALSYM TOC_POS_TOPLEVELOBJECT}

type
  // Interface ITocEntry
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITocEntry);'}
  {$EXTERNALSYM ITocEntry}
  ITocEntry = interface(IUnknown)
    ['{F22F5E06-585C-4def-8523-6555CFBC0CB3}']

      function SetTitle(pwszTitle: PWideChar): HResult; stdcall;

      function GetTitle(var pwTitleSize: WORD;
                        out pwszTitle: PWideChar): HResult; stdcall;

      function SetDescriptor(pDescriptor: TOC_ENTRY_DESCRIPTOR): HResult; stdcall;

      function GetDescriptor(out pDescriptor: TOC_ENTRY_DESCRIPTOR): HResult; stdcall;

      function SetSubEntries(dwNumSubEntries: DWORD;
                             pwSubEntryIndices: WORD): HResult; stdcall;

      function GetSubEntries(var pdwNumSubEntries: DWORD;
                             out pwSubEntryIndices: WORD): HResult; stdcall;

      function SetDescriptionData(dwDescriptionDataSize: DWORD;
                                  pbtDescriptionData: PByte;
                                  pguidType: TGUID): HResult; stdcall;

      function GetDescriptionData(var pdwDescriptionDataSize: DWORD;
                                  out pbtDescriptionData: PByte;
                                  out pGuidType: TGUID): HResult; stdcall;

  end;
  // ITocEntry
  IID_ITocEntry = ITocEntry;
  {$EXTERNALSYM IID_ITocEntry}


  // Interface ITocEntryList
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITocEntryList);'}
  {$EXTERNALSYM ITocEntryList}
  ITocEntryList = interface(IUnknown)
    ['{3A8CCCBD-0EFD-43a3-B838-F38A552BA237}']

      function GetEntryCount(out pdwEntryCount: DWORD): HResult; stdcall;

      function GetEntryByIndex(dwEntryIndex: DWORD;
                               out ppEntry: ITocEntry): HResult; stdcall;

      function AddEntry(pEntry: ITocEntry;
                        out pdwEntryIndex: DWORD): HResult; stdcall;

      function AddEntryByIndex(dwEntryIndex: DWORD;
                               pEntry: ITocEntry): HResult; stdcall;

      function RemoveEntryByIndex(dwEntryIndex: DWORD): HResult; stdcall;

  end;
  // ITocEntryList
  IID_ITocEntryList = ITocEntryList;
  {$EXTERNALSYM IID_ITocEntryList}


  // Interface IToc
  // ==============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IToc);'}
  {$EXTERNALSYM IToc}
  IToc = interface(IUnknown)
    ['{D6F05441-A919-423b-91A0-89D5B4A8AB77}']

      function SetDescriptor(pDescriptor: TOC_DESCRIPTOR): HResult; stdcall;

      function GetDescriptor(out pDescriptor: TOC_DESCRIPTOR): HResult; stdcall;

      function SetDescription(pwszDescription: PWideChar): HResult; stdcall;

      function GetDescription(var pwDescriptionSize: WORD;
                              out pwszDescription: PWideChar): HResult; stdcall;

      function SetContext(dwContextSize: DWORD;
                          pbtContext: PByte): HResult; stdcall;

      function GetContext(var pdwContextSize: DWORD;
                          out pbtContext: PByte): HResult; stdcall;

      function GetEntryListCount(out pwCount: WORD): HResult; stdcall;

      function GetEntryListByIndex(wEntryListIndex: WORD;
                                   out ppEntryList: ITocEntryList): HResult; stdcall;

      function AddEntryList(pEntryList: ITocEntryList;
                            out pwEntryListIndex: WORD): HResult; stdcall;

      function AddEntryListByIndex(wEntryListIndex: WORD;
                                   out pEntryList: ITocEntryList): HResult; stdcall;

      function RemoveEntryListByIndex(wEntryListIndex: WORD): HResult; stdcall;

  end;
  // IToc
  IID_IToc = IToc;
  {$EXTERNALSYM IID_IToc}


  // Interface ITocCollection
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITocCollection);'}
  {$EXTERNALSYM ITocCollection}
  ITocCollection = interface(IUnknown)
    ['{23fee831-ae96-42df-b170-25a04847a3ca}']

      function GetEntryCount(out pdwEntryCount: DWORD): HResult; stdcall;

      function GetEntryByIndex(dwEntryIndex: DWORD;
                               out ppToc: IToc): HResult; stdcall;

      function AddEntry(pToc: IToc;
                        out pdwEntryIndex: DWORD): HResult; stdcall;

      function AddEntryByIndex(dwEntryIndex: DWORD;
                               pToc: IToc): HResult; stdcall;

      function RemoveEntryByIndex(dwEntryIndex: DWORD): HResult; stdcall;

  end;
  // ITocCollection
  IID_ITocCollection = ITocCollection;
  {$EXTERNALSYM IID_ITocCollection}


  // Interface ITocParser
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITocParser);'}
  {$EXTERNALSYM ITocParser}
  ITocParser = interface(IUnknown)
    ['{ECFB9A55-9298-4f49-887F-0B36206599D2}']

      function Init(pwszFileName: PWideChar): HResult; stdcall;

      function GetTocCount(enumTocPosType: TOC_POS_TYPE;
                           out pdwTocCount: DWORD): HResult; stdcall;

      function GetTocByIndex(enumTocPosType: TOC_POS_TYPE;
                             dwTocIndex: DWORD;
                             out ppToc: IToc): HResult; stdcall;

      function GetTocByType(enumTocPosType: TOC_POS_TYPE;
                            guidTocType: TGUID;
                            out ppTocs: ITocCollection): HResult; stdcall;

      function AddToc(enumTocPosType: TOC_POS_TYPE;
                      out pToc: IToc;
                      out pdwTocIndex: DWORD): HResult; stdcall;

      function RemoveTocByIndex(enumTocPosType: TOC_POS_TYPE;
                                dwTocIndex: DWORD): HResult; stdcall;

      function RemoveTocByType(enumTocPosType: TOC_POS_TYPE;
                               guidTocType: TGUID): HResult; stdcall;

      function Commit(): HResult; stdcall;

  end;
  // ITocParser
  IID_ITocParser = ITocParser;
  {$EXTERNALSYM IID_ITocParser}



  PFILE_OPENMODE = ^FILE_OPENMODE;
  __MIDL___MIDL_itf_mfobjects_0000_0017_0002 = (
    OPENMODE_FAIL_IF_NOT_EXIST  = 0,
    OPENMODE_FAIL_IF_EXIST      = 1,
    OPENMODE_RESET_IF_EXIST     = 2,
    OPENMODE_APPEND_IF_EXIST    = 3,
    OPENMODE_DELETE_IF_EXIST    = 4
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0017_0002}
  FILE_OPENMODE = __MIDL___MIDL_itf_mfobjects_0000_0017_0002;
  {$EXTERNALSYM FILE_OPENMODE}


  PSEEK_ORIGIN = ^SEEK_ORIGIN;
  SEEK_ORIGIN = (
    _msoBegin    = 0,
    _msoCurrent  = 1);
  {$EXTERNALSYM SEEK_ORIGIN}

  PFILE_ACCESSMODE = ^FILE_ACCESSMODE;
  FILE_ACCESSMODE = (
    ACCESSMODE_READ             = 1,
    ACCESSMODE_WRITE            = 2,
    ACCESSMODE_READWRITE        = 3,
    ACCESSMODE_WRITE_EXCLUSIVE  = 4
  );
  {$EXTERNALSYM FILE_ACCESSMODE}


  // Interface IFileIo
  // =================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileIo);'}
  {$EXTERNALSYM IFileIo}
  IFileIo = interface(IUnknown)
    ['{11993196-1244-4840-AB44-480975C4FFE4}']

      function Initialize(eAccessMode: FILE_ACCESSMODE;
                          eOpenMode: FILE_OPENMODE;
                          pwszFileName: PWideChar): HResult; stdcall;

      function GetLength(out pqwLength: QWORD): HResult; stdcall;

      function SetLength(qwLength: QWORD): HResult; stdcall;

      function GetCurrentPosition(out pqwPosition: QWORD): HResult; stdcall;

      function SetCurrentPosition(qwPosition: QWORD): HResult; stdcall;

      function IsEndOfStream(out pbEndOfStream: BOOL): HResult; stdcall;

      function Read(pbt: PByte;
                    ul: ULONG;
                    out pulRead: ULONG): HResult; stdcall;

      function Write(pbt: PByte;
                     ul: ULONG;
                     out pulWritten: ULONG): HResult; stdcall;

      function Seek(eSeekOrigin: SEEK_ORIGIN;
                    qwSeekOffset: QWORD;
                    dwSeekFlags: DWORD;
                    out pqwCurrentPosition: QWORD): HResult; stdcall;

      function Close(): HResult; stdcall;

  end;
  // IFileIo
  IID_IFileIo = IFileIo;
  {$EXTERNALSYM IID_IFileIo}


  // Interface IFileClient
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileClient);'}
  {$EXTERNALSYM IFileClient}
  IFileClient = interface(IUnknown)
    ['{BFCCD196-1244-4840-AB44-480975C4FFE4}']

      function GetObjectDiskSize(out pqwSize: QWORD): HResult; stdcall;

      function Write(pFio: IFileIo): HResult; stdcall;

      function Read(pFio: IFileIo): HResult; stdcall;

  end;
  // IFileClient
  IID_IFileClient = IFileClient;
  {$EXTERNALSYM IID_IFileClient}


  // Interface IClusterDetector
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IClusterDetector);'}
  {$EXTERNALSYM IClusterDetector}
  IClusterDetector = interface(IUnknown)
    ['{3F07F7B7-C680-41d9-9423-915107EC9FF9}']

      function Initialize(wBaseEntryLevel: WORD;
                          wClusterEntryLevel: WORD): HResult; stdcall;

      function Detect(dwMaxNumClusters: DWORD;
                      fMinClusterDuration: FLOAT;
                      fMaxClusterDuration: FLOAT;
                      pSrcToc: IToc;
                      out ppDstToc: IToc): HResult; stdcall;

  end;
  // IClusterDetector
  IID_IClusterDetector = IClusterDetector;
  {$EXTERNALSYM IID_IClusterDetector}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

const

  WmCodecDspLib = 'WMSPDMOE.DLL';

  // Implement Additional Prototypes here.

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Media Foundation - MfpMediaTypeDebug.pas
// Kind: Pascal Unit
// Release date: 04-06-2016
// Language: ENU
//
// Version: 2.6.5
// Description: Media Type Debugging Code Methods
//
// Company: FactoryX
// Intiator(s): Ramyses De Macedo Rodrigues, Tony (maXcomX), Peter (OzShips).
// Contributor(s): Ramyses De Macedo Rodrigues, Tony Kalf (maXcomX), Peter Larson (ozships).
//

//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 2004)
//                                #1 Autobahn
// 10/08/2020 All                 #2 => #2b The Model
//------------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
// Todo: -
// =============================================================================
// Source: MSDN - Media Type Debugging Code
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit WinApi.MediaFoundationApi.MfpMediaTypeDebug;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,  
  {Vcl}
  Vcl.Dialogs,
  {System}
  System.SysUtils,
  {WinApi.ActiveX}
  WinApi.ActiveX.PropIdl,
  {WinApi.MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,  
  WinApi.MediaFoundationApi.MfApi,  
  WinApi.MediaFoundationApi.MfError;


const
  ug = 'Unkown guid';

type
  TGuidProp = record
    gName: ShortString;
    gIID: TGUID;
  end;
  {$EXTERNALSYM TGuidProp}

  function GetGUIDName(const gguid: TGUID;
                       out Gprop: TGuidProp): HRESULT;
  {$EXTERNALSYM GetGUIDName}

  function LogAttributeValueByIndex(pAttr: IMFAttributes;
                                    index: DWORD): HRESULT;
  {$EXTERNALSYM LogAttributeValueByIndex}

  function LogMediaType(pType: IMFMediaType): HRESULT;
  {$EXTERNALSYM LogMediaType}

  // Handle certain known special cases.
  function SpecialCaseAttributeValue(guid: TGUID;
                                     pvar: PROPVARIANT): HRESULT;
  {$EXTERNALSYM SpecialCaseAttributeValue}



implementation


procedure DBGMSG(const Msg: String);
begin
  OutputDebugString(PWideChar(Msg));
end;


function OffsetToFloat(offset: MFOffset): float;
begin
  Result := ((offset.value + offset.fract) / 65536.0);
end;


function LogVideoArea(pvar: PROPVARIANT): HRESULT;
var
  pArea: PMFVideoArea;

begin
  if (pvar.caub.cElems < SizeOf(MFVideoArea)) then
    begin
      Result := MF_E_BUFFERTOOSMALL;
      Exit;
    end;

    pArea := PMFVideoArea(pvar.caub.pElems);

    DBGMSG( format('(%g,%g) (%d,%d)',
                 [OffsetToFloat(pArea.OffsetX), OffsetToFloat(pArea.OffsetY),
                  pArea.Area.cx, pArea.Area.cy]) );

   Result := S_OK;

end;



procedure LogUINT32AsUINT64(pvar: PROPVARIANT);
var
  uHigh: UINT32;
  uLow:  UINT32;

begin
  uHigh := 0;
  uLow := 0;
  Unpack2UINT32AsUINT64(pvar.uhVal.QuadPart,
                        uHigh,
                        uLow);
  DBGMSG(format('%d,%d', [uHigh, uLow]));
end;


function GetGUIDName(const gguid: TGUID;
                     out Gprop: TGuidProp): HRESULT;
var
  ns: ShortString;

begin

  if IsEqualguid(gguid, MF_MT_MAJOR_TYPE) then ns := 'MF_MT_MAJOR_TYPE'
  else if IsEqualguid(gguid, MF_MT_MAJOR_TYPE) then ns := 'MF_MT_MAJOR_TYPE'
  else if IsEqualguid(gguid, MF_MT_SUBTYPE) then ns := 'MF_MT_SUBTYPE'
  else if IsEqualguid(gguid, MF_MT_ALL_SAMPLES_INDEPENDENT) then ns := 'MF_MT_ALL_SAMPLES_INDEPENDENT'
  else if IsEqualguid(gguid, MF_MT_FIXED_SIZE_SAMPLES) then  ns := 'MF_MT_FIXED_SIZE_SAMPLES'
  else if IsEqualguid(gguid, MF_MT_COMPRESSED) then ns := 'MF_MT_COMPRESSED'
  else if IsEqualguid(gguid, MF_MT_SAMPLE_SIZE) then ns := 'MF_MT_SAMPLE_SIZE'
  else if IsEqualguid(gguid, MF_MT_WRAPPED_TYPE) then ns := 'MF_MT_WRAPPED_TYPE'
  else if IsEqualguid(gguid, MF_MT_AUDIO_NUM_CHANNELS) then ns := 'MF_MT_AUDIO_NUM_CHANNELS'
  else if IsEqualguid(gguid, MF_MT_AUDIO_SAMPLES_PER_SECOND) then ns := 'MF_MT_AUDIO_SAMPLES_PER_SECOND'
  else if IsEqualguid(gguid, MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND) then ns := 'MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND'
  else if IsEqualguid(gguid, MF_MT_AUDIO_AVG_BYTES_PER_SECOND) then ns := 'MF_MT_AUDIO_AVG_BYTES_PER_SECOND'
  else if IsEqualguid(gguid, MF_MT_AUDIO_BLOCK_ALIGNMENT) then ns := 'MF_MT_AUDIO_BLOCK_ALIGNMENT'
  else if IsEqualguid(gguid, MF_MT_AUDIO_BITS_PER_SAMPLE) then ns := 'MF_MT_AUDIO_BITS_PER_SAMPLE'
  else if IsEqualguid(gguid, MF_MT_AUDIO_VALID_BITS_PER_SAMPLE) then ns := 'MF_MT_AUDIO_VALID_BITS_PER_SAMPLE'
  else if IsEqualguid(gguid, MF_MT_AUDIO_SAMPLES_PER_BLOCK) then ns := 'MF_MT_AUDIO_SAMPLES_PER_BLOCK'
  else if IsEqualguid(gguid, MF_MT_AUDIO_CHANNEL_MASK) then ns := 'MF_MT_AUDIO_CHANNEL_MASK'
  else if IsEqualguid(gguid, MF_MT_AUDIO_FOLDDOWN_MATRIX) then ns := 'MF_MT_AUDIO_FOLDDOWN_MATRIX'
  else if IsEqualguid(gguid, MF_MT_AUDIO_WMADRC_PEAKREF) then ns := 'MF_MT_AUDIO_WMADRC_PEAKREF'
  else if IsEqualguid(gguid, MF_MT_AUDIO_WMADRC_PEAKTARGET) then ns := 'MF_MT_AUDIO_WMADRC_PEAKTARGET'
  else if IsEqualguid(gguid, MF_MT_AUDIO_WMADRC_AVGREF) then ns := 'MF_MT_AUDIO_WMADRC_AVGREF'
  else if IsEqualguid(gguid, MF_MT_AUDIO_WMADRC_AVGTARGET) then ns := 'MF_MT_AUDIO_WMADRC_AVGTARGET'
  else if IsEqualguid(gguid, MF_MT_AUDIO_PREFER_WAVEFORMATEX) then ns := 'MF_MT_AUDIO_PREFER_WAVEFORMATEX'
  else if IsEqualguid(gguid, MF_MT_AAC_PAYLOAD_TYPE) then ns := 'MF_MT_AAC_PAYLOAD_TYPE'
  else if IsEqualguid(gguid, MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION) then ns := 'MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION'
  else if IsEqualguid(gguid, MF_MT_FRAME_SIZE) then ns := 'MF_MT_FRAME_SIZE'
  else if IsEqualguid(gguid, MF_MT_FRAME_RATE) then ns := 'MF_MT_FRAME_RATE'
  else if IsEqualguid(gguid, MF_MT_FRAME_RATE_RANGE_MAX) then ns := 'MF_MT_FRAME_RATE_RANGE_MAX'
  else if IsEqualguid(gguid, MF_MT_FRAME_RATE_RANGE_MIN) then ns := 'MF_MT_FRAME_RATE_RANGE_MIN'
  else if IsEqualguid(gguid, MF_MT_PIXEL_ASPECT_RATIO) then ns := 'MF_MT_PIXEL_ASPECT_RATIO'
  else if IsEqualguid(gguid, MF_MT_DRM_FLAGS) then ns := 'MF_MT_DRM_FLAGS'
  else if IsEqualguid(gguid, MF_MT_PAD_CONTROL_FLAGS) then ns := 'MF_MT_PAD_CONTROL_FLAGS'
  else if IsEqualguid(gguid, MF_MT_SOURCE_CONTENT_HINT) then ns := 'MF_MT_SOURCE_CONTENT_HINT'
  else if IsEqualguid(gguid, MF_MT_VIDEO_CHROMA_SITING) then ns := 'MF_MT_VIDEO_CHROMA_SITING'
  else if IsEqualguid(gguid, MF_MT_INTERLACE_MODE) then ns := 'MF_MT_INTERLACE_MODE'
  else if IsEqualguid(gguid, MF_MT_TRANSFER_FUNCTION) then ns := 'MF_MT_TRANSFER_FUNCTION'
  else if IsEqualguid(gguid, MF_MT_VIDEO_PRIMARIES) then ns := 'MF_MT_VIDEO_PRIMARIES'
  else if IsEqualguid(gguid, MF_MT_CUSTOM_VIDEO_PRIMARIES) then ns := 'MF_MT_CUSTOM_VIDEO_PRIMARIES'
  else if IsEqualguid(gguid, MF_MT_YUV_MATRIX) then ns := 'MF_MT_YUV_MATRIX'
  else if IsEqualguid(gguid, MF_MT_VIDEO_LIGHTING) then ns := 'MF_MT_VIDEO_LIGHTING'
  else if IsEqualguid(gguid, MF_MT_VIDEO_NOMINAL_RANGE) then ns := 'MF_MT_VIDEO_NOMINAL_RANGE'
  else if IsEqualguid(gguid, MF_MT_GEOMETRIC_APERTURE) then ns := 'MF_MT_GEOMETRIC_APERTURE'
  else if IsEqualguid(gguid, MF_MT_MINIMUM_DISPLAY_APERTURE) then ns := 'MF_MT_MINIMUM_DISPLAY_APERTURE'
  else if IsEqualguid(gguid, MF_MT_PAN_SCAN_APERTURE) then ns := 'MF_MT_PAN_SCAN_APERTURE'
  else if IsEqualguid(gguid, MF_MT_PAN_SCAN_ENABLED) then ns := 'MF_MT_PAN_SCAN_ENABLED'
  else if IsEqualguid(gguid, MF_MT_AVG_BITRATE) then ns := 'MF_MT_AVG_BITRATE'
  else if IsEqualguid(gguid, MF_MT_AVG_BIT_ERROR_RATE) then ns := 'MF_MT_AVG_BIT_ERROR_RATE'
  else if IsEqualguid(gguid, MF_MT_MAX_KEYFRAME_SPACING) then ns := 'MF_MT_MAX_KEYFRAME_SPACING'
  else if IsEqualguid(gguid, MF_MT_DEFAULT_STRIDE) then ns := 'MF_MT_DEFAULT_STRIDE'
  else if IsEqualguid(gguid, MF_MT_PALETTE) then ns := 'MF_MT_PALETTE'
  else if IsEqualguid(gguid, MF_MT_USER_DATA) then ns := 'MF_MT_USER_DATA'
  else if IsEqualguid(gguid, MF_MT_AM_FORMAT_TYPE) then ns := 'MF_MT_AM_FORMAT_TYPE'
  else if IsEqualguid(gguid, MF_MT_MPEG_START_TIME_CODE) then ns := 'MF_MT_MPEG_START_TIME_CODE'
  else if IsEqualguid(gguid, MF_MT_MPEG2_PROFILE) then ns := 'MF_MT_MPEG2_PROFILE'
  else if IsEqualguid(gguid, MF_MT_MPEG2_LEVEL) then ns := 'MF_MT_MPEG2_LEVEL'
  else if IsEqualguid(gguid, MF_MT_MPEG2_FLAGS) then ns := 'MF_MT_MPEG2_FLAGS'
  else if IsEqualguid(gguid, MF_MT_MPEG_SEQUENCE_HEADER) then ns := 'MF_MT_MPEG_SEQUENCE_HEADER'
  else if IsEqualguid(gguid, MF_MT_DV_AAUX_SRC_PACK_0) then ns := 'MF_MT_DV_AAUX_SRC_PACK_0'
  else if IsEqualguid(gguid, MF_MT_DV_AAUX_CTRL_PACK_0) then ns := 'MF_MT_DV_AAUX_CTRL_PACK_0'
  else if IsEqualguid(gguid, MF_MT_DV_AAUX_SRC_PACK_1) then ns := 'MF_MT_DV_AAUX_SRC_PACK_1'
  else if IsEqualguid(gguid, MF_MT_DV_AAUX_CTRL_PACK_1) then ns := 'MF_MT_DV_AAUX_CTRL_PACK_1'
  else if IsEqualguid(gguid, MF_MT_DV_VAUX_SRC_PACK) then ns := 'MF_MT_DV_VAUX_SRC_PACK'
  else if IsEqualguid(gguid, MF_MT_DV_VAUX_CTRL_PACK) then ns := 'MF_MT_DV_VAUX_CTRL_PACK'
  else if IsEqualguid(gguid, MF_MT_ARBITRARY_HEADER) then ns := 'MF_MT_ARBITRARY_HEADER'
  else if IsEqualguid(gguid, MF_MT_ARBITRARY_FORMAT) then ns := 'MF_MT_ARBITRARY_FORMAT'
  else if IsEqualguid(gguid, MF_MT_IMAGE_LOSS_TOLERANT) then ns := 'MF_MT_IMAGE_LOSS_TOLERANT'
  else if IsEqualguid(gguid, MF_MT_MPEG4_SAMPLE_DESCRIPTION) then ns := 'MF_MT_MPEG4_SAMPLE_DESCRIPTION'
  else if IsEqualguid(gguid, MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY) then ns := 'MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY'
  else if IsEqualguid(gguid, MF_MT_ORIGINAL_4CC) then ns := 'MF_MT_ORIGINAL_4CC'
  else if IsEqualguid(gguid, MF_MT_ORIGINAL_WAVE_FORMAT_TAG) then ns := 'MF_MT_ORIGINAL_WAVE_FORMAT_TAG'

  // Media types

  else if IsEqualguid(gguid, MFMediaType_Audio) then ns := 'MFMediaType_Audio'
  else if IsEqualguid(gguid, MFMediaType_Video) then ns := 'MFMediaType_Video'
  else if IsEqualguid(gguid, MFMediaType_Protected) then ns := 'MFMediaType_Protected'
  else if IsEqualguid(gguid, MFMediaType_SAMI) then ns := 'MFMediaType_SAMI'
  else if IsEqualguid(gguid, MFMediaType_Script) then ns := 'MFMediaType_Script'
  else if IsEqualguid(gguid, MFMediaType_Image) then ns := 'MFMediaType_Image'
  else if IsEqualguid(gguid, MFMediaType_HTML) then ns := 'MFMediaType_HTML'
  else if IsEqualguid(gguid, MFMediaType_Binary) then ns := 'MFMediaType_Binary'
  else if IsEqualguid(gguid, MFMediaType_FileTransfer) then ns := 'MFMediaType_FileTransfer'

  else if IsEqualguid(gguid, MFVideoFormat_AI44) then ns := 'MFVideoFormat_AI44' //     FCC('AI44')
  else if IsEqualguid(gguid, MFVideoFormat_ARGB32) then ns := 'MFVideoFormat_ARGB32' //   D3DFMT_A8R8G8B8
  else if IsEqualguid(gguid, MFVideoFormat_AYUV) then ns := 'MFVideoFormat_AYUV' //     FCC('AYUV')
  else if IsEqualguid(gguid, MFVideoFormat_DV25) then ns := 'MFVideoFormat_DV25' //     FCC('dv25')
  else if IsEqualguid(gguid, MFVideoFormat_DV50) then ns := 'MFVideoFormat_DV50' //     FCC('dv50')
  else if IsEqualguid(gguid, MFVideoFormat_DVH1) then ns := 'MFVideoFormat_DVH1' //     FCC('dvh1')
  else if IsEqualguid(gguid, MFVideoFormat_DVSD) then ns := 'MFVideoFormat_DVSD' //     FCC('dvsd')
  else if IsEqualguid(gguid, MFVideoFormat_DVSL) then ns := 'MFVideoFormat_DVSL' //     FCC('dvsl')
  else if IsEqualguid(gguid, MFVideoFormat_H264) then ns := 'MFVideoFormat_H264' //     FCC('H264')
  else if IsEqualguid(gguid, MFVideoFormat_I420) then ns := 'MFVideoFormat_I420' //     FCC('I420')
  else if IsEqualguid(gguid, MFVideoFormat_IYUV) then ns := 'MFVideoFormat_IYUV' //     FCC('IYUV')
  else if IsEqualguid(gguid, MFVideoFormat_M4S2) then ns := 'MFVideoFormat_M4S2' //     FCC('M4S2')
  else if IsEqualguid(gguid, MFVideoFormat_MJPG) then ns := 'MFVideoFormat_MJPG'
  else if IsEqualguid(gguid, MFVideoFormat_MP43) then ns := 'MFVideoFormat_MP43' //     FCC('MP43')
  else if IsEqualguid(gguid, MFVideoFormat_MP4S) then ns := 'MFVideoFormat_MP4S' //     FCC('MP4S')
  else if IsEqualguid(gguid, MFVideoFormat_MP4V) then ns := 'MFVideoFormat_MP4V' //     FCC('MP4V')
  else if IsEqualguid(gguid, MFVideoFormat_MPG1) then ns := 'MFVideoFormat_MPG1' //     FCC('MPG1')
  else if IsEqualguid(gguid, MFVideoFormat_MSS1) then ns := 'MFVideoFormat_MSS1' //     FCC('MSS1')
  else if IsEqualguid(gguid, MFVideoFormat_MSS2) then ns := 'MFVideoFormat_MSS2' //     FCC('MSS2')
  else if IsEqualguid(gguid, MFVideoFormat_NV11) then ns := 'MFVideoFormat_NV11' //     FCC('NV11')
  else if IsEqualguid(gguid, MFVideoFormat_NV12) then ns := 'MFVideoFormat_NV12' //     FCC('NV12')
  else if IsEqualguid(gguid, MFVideoFormat_P010) then ns := 'MFVideoFormat_P010' //     FCC('P010')
  else if IsEqualguid(gguid, MFVideoFormat_P016) then ns := 'MFVideoFormat_P016' //     FCC('P016')
  else if IsEqualguid(gguid, MFVideoFormat_P210) then ns := 'MFVideoFormat_P210' //     FCC('P210')
  else if IsEqualguid(gguid, MFVideoFormat_P216) then ns := 'MFVideoFormat_P216' //     FCC('P216')
  else if IsEqualguid(gguid, MFVideoFormat_RGB24) then ns := 'MFVideoFormat_RGB24' //    D3DFMT_R8G8B8
  else if IsEqualguid(gguid, MFVideoFormat_RGB32) then ns := 'MFVideoFormat_RGB32' //    D3DFMT_X8R8G8B8
  else if IsEqualguid(gguid, MFVideoFormat_RGB555) then ns := 'MFVideoFormat_RGB555' //   D3DFMT_X1R5G5B5
  else if IsEqualguid(gguid, MFVideoFormat_RGB565) then ns := 'MFVideoFormat_RGB565' //   D3DFMT_R5G6B5
  else if IsEqualguid(gguid, MFVideoFormat_RGB8) then ns := 'MFVideoFormat_RGB8'
  else if IsEqualguid(gguid, MFVideoFormat_UYVY) then ns := 'MFVideoFormat_UYVY' //     FCC('UYVY')
  else if IsEqualguid(gguid, MFVideoFormat_v210) then ns := 'MFVideoFormat_v210' //     FCC('v210')
  else if IsEqualguid(gguid, MFVideoFormat_v410) then ns := 'MFVideoFormat_v410' //     FCC('v410')
  else if IsEqualguid(gguid, MFVideoFormat_WMV1) then ns := 'MFVideoFormat_WMV1' //     FCC('WMV1')
  else if IsEqualguid(gguid, MFVideoFormat_WMV2) then ns := 'MFVideoFormat_WMV2' //     FCC('WMV2')
  else if IsEqualguid(gguid, MFVideoFormat_WMV3) then ns := 'MFVideoFormat_WMV3' //     FCC('WMV3')
  else if IsEqualguid(gguid, MFVideoFormat_WVC1) then ns := 'MFVideoFormat_WVC1' //     FCC('WVC1')
  else if IsEqualguid(gguid, MFVideoFormat_Y210) then ns := 'MFVideoFormat_Y210' //     FCC('Y210')
  else if IsEqualguid(gguid, MFVideoFormat_Y216) then ns := 'MFVideoFormat_Y216' //     FCC('Y216')
  else if IsEqualguid(gguid, MFVideoFormat_Y410) then ns := 'MFVideoFormat_Y410' //     FCC('Y410')
  else if IsEqualguid(gguid, MFVideoFormat_Y416) then ns := 'MFVideoFormat_Y416' //     FCC('Y416')
  else if IsEqualguid(gguid, MFVideoFormat_Y41P) then ns := 'MFVideoFormat_Y41P'
  else if IsEqualguid(gguid, MFVideoFormat_Y41T) then ns := 'MFVideoFormat_Y41T'
  else if IsEqualguid(gguid, MFVideoFormat_YUY2) then ns := 'MFVideoFormat_YUY2' //     FCC('YUY2')
  else if IsEqualguid(gguid, MFVideoFormat_YV12) then ns := 'MFVideoFormat_YV12' //     FCC('YV12')
  else if IsEqualguid(gguid, MFVideoFormat_YVYU) then ns := 'MFVideoFormat_YVYU'

  else if IsEqualguid(gguid, MFAudioFormat_PCM) then ns := 'MFAudioFormat_PCM' //              WAVE_FORMAT_PCM
  else if IsEqualguid(gguid, MFAudioFormat_Float) then ns := 'MFAudioFormat_Float' //            WAVE_FORMAT_IEEE_FLOAT
  else if IsEqualguid(gguid, MFAudioFormat_DTS) then ns := 'MFAudioFormat_DTS' //              WAVE_FORMAT_DTS
  else if IsEqualguid(gguid, MFAudioFormat_Dolby_AC3_SPDIF) then ns := 'MFAudioFormat_Dolby_AC3_SPDIF' //  WAVE_FORMAT_DOLBY_AC3_SPDIF
  else if IsEqualguid(gguid, MFAudioFormat_DRM) then ns := 'MFAudioFormat_DRM' //              WAVE_FORMAT_DRM
  else if IsEqualguid(gguid, MFAudioFormat_WMAudioV8) then ns := 'MFAudioFormat_WMAudioV8' //        WAVE_FORMAT_WMAUDIO2
  else if IsEqualguid(gguid, MFAudioFormat_WMAudioV9) then ns := 'MFAudioFormat_WMAudioV9' //        WAVE_FORMAT_WMAUDIO3
  else if IsEqualguid(gguid, MFAudioFormat_WMAudio_Lossless) then ns := 'MFAudioFormat_WMAudio_Lossless' // WAVE_FORMAT_WMAUDIO_LOSSLESS
  else if IsEqualguid(gguid, MFAudioFormat_WMASPDIF) then ns := 'MFAudioFormat_WMASPDIF' //         WAVE_FORMAT_WMASPDIF
  else if IsEqualguid(gguid, MFAudioFormat_MSP1) then ns := 'MFAudioFormat_MSP1' //             WAVE_FORMAT_WMAVOICE9
  else if IsEqualguid(gguid, MFAudioFormat_MP3) then ns := 'MFAudioFormat_MP3' //              WAVE_FORMAT_MPEGLAYER3
  else if IsEqualguid(gguid, MFAudioFormat_MPEG) then ns := 'MFAudioFormat_MPEG' //             WAVE_FORMAT_MPEG
  else if IsEqualguid(gguid, MFAudioFormat_AAC) then ns := 'MFAudioFormat_AAC' //              WAVE_FORMAT_MPEG_HEAAC
  else if IsEqualguid(gguid, MFAudioFormat_ADTS) then ns := 'MFAudioFormat_ADTS' //             WAVE_FORMAT_MPEG_ADTS_AAC
  else
    begin
      Gprop.gName := ug;
      Gprop.gIID := GUID_NULL;
      Result := E_FAIL;
      Exit;
    end;

  Gprop.gName := ns;
  Gprop.gIID := gguid;
  Result := S_OK;

end;


function LogAttributeValueByIndex(pAttr: IMFAttributes;
                                  index: DWORD): HRESULT;
var
  pGuidp: TGuidProp;
  gguid: TGUID;
  pvar: PROPVARIANT;
  hr: HResult;
  gp: TGuidProp;

label
  done;

begin

  PropVariantInit(pvar);

  hr := pAttr.GetItemByIndex(index,
                            gguid,
                            pvar);
  if FAILED(hr) then
    goto done;


  GetGUIDName(gguid,
              gp);
  if FAILED(hr) then
    goto done;

  DBGMSG(String(gp.gName));

  hr := SpecialCaseAttributeValue(gguid,
                                 pvar);
  if FAILED(hr) then
    goto done;

  if (hr = S_FALSE) then
    begin
      case (pvar.vt) of
        VT_UI4: DBGMSG(format('%d', [pvar.ulVal]));

        VT_UI8: DBGMSG(format('%d', [pvar.uhVal.QuadPart]));

        VT_R8: DBGMSG(format('%g', [pvar.dblVal]));

        VT_CLSID: begin
                    hr := GetGUIDName(pvar.puuid^,
                                     pGuidp);
                    if SUCCEEDED(hr) then
                      DBGMSG(format('%s, %s', [pGuidp.gName, GuidToString(pGuidp.gIID)]))
                    else
                      DBGMSG('Could not retrieve a valid Guidname');
                  end;

        VT_LPWSTR: DBGMSG(String(pvar.pwszVal));

        VT_VECTOR, VT_UI1: DBGMSG('<<byte array>>');

        VT_UNKNOWN: DBGMSG('IUnknown')

        else
          DBGMSG(format('Unexpected attribute type (%d)', [pvar.vt]));
      end;
    end;

done:
  DBGMSG(#13);
  PropVariantClear(pvar);
  Result := hr;

end;


function LogMediaType(pType: IMFMediaType): HRESULT;
var
  i: Integer;
  count: UINT32;
  hr: HResult;

begin
  count := 0;

  hr := pType.GetCount(count);
  if FAILED(hr) then
    begin
      Result := hr;
      Exit;
    end;

  if (count = 0) then
    begin
      DBGMSG('Empty media type.');
    end;

  for i := 0 to count - 1 do
    begin
      hr := LogAttributeValueByIndex(pType,
                                    i);
      if FAILED(hr) then
        Break;
    end;
  Result := hr;
end;

//
function SpecialCaseAttributeValue(guid: TGUID;
                                   pvar: PROPVARIANT): HRESULT;
begin
  if ((guid = MF_MT_FRAME_RATE) or (guid = MF_MT_FRAME_RATE_RANGE_MAX) or
      (guid = MF_MT_FRAME_RATE_RANGE_MIN) or (guid = MF_MT_FRAME_SIZE) or
      (guid = MF_MT_PIXEL_ASPECT_RATIO)) then
    begin
      // Attributes that contain two packed 32-bit values.
      LogUINT32AsUINT64(pvar);
    end
  else if ((guid = MF_MT_GEOMETRIC_APERTURE) or
           (guid = MF_MT_MINIMUM_DISPLAY_APERTURE) or
           (guid = MF_MT_PAN_SCAN_APERTURE)) then
    begin
      // Attributes that an MFVideoArea structure.
      Result := LogVideoArea(pvar);
      Exit;
    end
  else
    begin
      Result := S_FALSE;
      Exit;
    end;

  Result := S_OK;

end;

end.

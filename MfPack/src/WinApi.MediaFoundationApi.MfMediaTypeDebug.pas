// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMediaTypeDebug.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2022
// Language: ENU
//
// Revision Version: 3.1.3
// Description: Code to view the contents of a media type (IMFMediaType) while debugging.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/08/2022 Tony                Mercury release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: How to use:
//            1 Create the class.
//             {$IFDEF DEBUG}
//             FMediaTypeDebug := TMediaTypeDebug.Create();
//             {$ENDIF}
//
//            2 Check the contents and store to file.
//              {$IFDEF DEBUG}
//              FMediaTypeDebug.LogMediaType(pYourMediaType);
//              FMediaTypeDebug.SafeDebugResultsToFile();
//              {$ENDIF}
//
//            3 When done Destroy the class.
//              {$IFDEF DEBUG}
//              FMediaTypeDebug.Free();
//              {$ENDIF}
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
// Source: Microsoft
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
unit WinApi.MediaFoundationApi.MfMediaTypeDebug;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  WinApi.ActiveX.PropVarUtil,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.ComBaseApi,
  WinApi.ActiveX.PropSys,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  {Vcl}
  Vcl.Forms;


type

  TMediaTypeDebug = class(TObject)
  private

    slDebug: TStringlist;

    function GetGuidName(const pGuid: TGUID;
                         out ppwsz: string): HRESULT;
    function GetGuidNameConst(const pGuid: TGUID): string;
    function LogAttributeValueByIndex(pAttributes: IMFAttributes;
                                      pIndex: DWord): HRESULT;
    function SpecialCaseAttributeValue(const pGuid: TGUID;
                                       const pPropVar: PROPVARIANT): HRESULT;
    procedure LogUINT32AsUINT64(const pPropVar: PROPVARIANT);
    function LogVideoArea(const pPropVar: PROPVARIANT): HRESULT;

    procedure DebugMsg(pFormat: string);

  public

    constructor Create();
    destructor Destroy(); override;

    // Call this method to log
    function LogMediaType(pType: IMFMediaType): HRESULT;
    // Call this method to save the log to file.
    procedure SafeDebugResultsToFile(const pFileName: string = 'MFMediaTypeDebug';
                                     const pExt: string = '.txt');

    property DebugResults: TStringList read slDebug;
  end;


implementation


{PUBLIC}
constructor TMediaTypeDebug.Create();
begin
  inherited Create();
  slDebug := TStringlist.Create();
end;


destructor TMediaTypeDebug.Destroy();
begin
  slDebug.Free();
  inherited Destroy();
end;


function TMediaTypeDebug.LogMediaType(pType: IMFMediaType): HRESULT;
var
  hr: HResult;
  unCount: UINT32;
  i: Integer;

label
  Return;

begin
  hr := pType.GetCount(unCount);
  if FAILED(hr) then
    goto Return;

  if (unCount = 0) then
    begin
        DebugMsg(StrToPWideChar(Format('Empty media type. %d',
                                       [unCount])));
        goto Return;
    end;

  for i := 0 to unCount -1 do
   begin
     hr := LogAttributeValueByIndex(pType,
                                    i);
     if FAILED(hr) then
       Break;
   end;

Return:
  Result := hr;
end;


procedure TMediaTypeDebug.SafeDebugResultsToFile(const pFileName: string = 'MFMediaTypeDebug';
                                                 const pExt: string = '.txt');
var
  i: Integer;
  sFileName: string;
  sDir: string;
  bFileExists: Boolean;

begin
  i := 0;
  bFileExists := True;
  sDir := ExtractFileDir(Application.ExeName);
  sFileName := Format('%s/%s(%d)%s',
                      [sDir, pFileName, i, pExt]);

  while (bFileExists = False) do
    begin
      if FileExists(sFileName) then
        sFileName := Format('%s/%s(%d)%s',
                            [sDir, pFileName, i, pExt])
      else
        bFileExists := False;
      Inc(i);
    end;

  slDebug.SaveToFile(sFileName);
  slDebug.Clear();
end;


{PRIVATE}

//
function TMediaTypeDebug.GetGuidName(const pGuid: TGUID;
                                     out ppwSz: string): HRESULT;
begin
  ppwSz := GetGUIDNameConst(pGuid);

  if (ppwSz <> '') then
    Result := S_OK
  else
    Result := E_FAIL;
end;

//
function TMediaTypeDebug.GetGuidNameConst(const pGuid: TGUID): string;
begin

  if IsEqualGuid(pGuid,
                 MF_MT_MAJOR_TYPE)  then
      Result := 'MF_MT_MAJOR_TYPE'
  else if IsEqualGuid(pGuid,
                       MF_MT_SUBTYPE) then
    Result := 'MF_MT_SUBTYPE'
  else if IsEqualGuid(pGuid,
                       MF_MT_ALL_SAMPLES_INDEPENDENT) then
    Result := 'MF_MT_ALL_SAMPLES_INDEPENDENT'
  else if IsEqualGuid(pGuid,
                       MF_MT_FIXED_SIZE_SAMPLES) then
    Result := 'MF_MT_FIXED_SIZE_SAMPLES'
  else if IsEqualGuid(pGuid,
                       MF_MT_COMPRESSED) then
    Result := 'MF_MT_COMPRESSED'
  else if IsEqualGuid(pGuid,
                       MF_MT_SAMPLE_SIZE) then
    Result := 'MF_MT_SAMPLE_SIZE'
  else if IsEqualGuid(pGuid,
                       MF_MT_WRAPPED_TYPE) then
    Result := 'MF_MT_WRAPPED_TYPE'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_NUM_CHANNELS) then
    Result := 'MF_MT_AUDIO_NUM_CHANNELS'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_SAMPLES_PER_SECOND) then
    Result := 'MF_MT_AUDIO_SAMPLES_PER_SECOND'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND) then
    Result := 'MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_AVG_BYTES_PER_SECOND) then
    Result := 'MF_MT_AUDIO_AVG_BYTES_PER_SECOND'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_BLOCK_ALIGNMENT) then
    Result := 'MF_MT_AUDIO_BLOCK_ALIGNMENT'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_BITS_PER_SAMPLE) then
    Result := 'MF_MT_AUDIO_BITS_PER_SAMPLE'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_VALID_BITS_PER_SAMPLE) then
    Result := 'MF_MT_AUDIO_BITS_PER_SAMPLE'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_SAMPLES_PER_BLOCK) then
    Result := 'MF_MT_AUDIO_SAMPLES_PER_BLOCK'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_CHANNEL_MASK) then
    Result := 'MF_MT_AUDIO_CHANNEL_MASK'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_FOLDDOWN_MATRIX) then
    Result := 'MF_MT_AUDIO_FOLDDOWN_MATRIX'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_WMADRC_PEAKREF) then
    Result := 'MF_MT_AUDIO_WMADRC_PEAKREF'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_WMADRC_PEAKTARGET) then
    Result := 'MF_MT_AUDIO_WMADRC_PEAKTARGET'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_WMADRC_AVGREF) then
    Result := 'MF_MT_AUDIO_WMADRC_AVGREF'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_WMADRC_AVGTARGET) then
    Result := 'MF_MT_AUDIO_WMADRC_AVGTARGET'
  else if IsEqualGuid(pGuid,
                       MF_MT_AUDIO_PREFER_WAVEFORMATEX) then
    Result := 'MF_MT_AUDIO_PREFER_WAVEFORMATEX'
  else if IsEqualGuid(pGuid,
                       MF_MT_AAC_PAYLOAD_TYPE) then
    Result := 'MF_MT_AAC_PAYLOAD_TYPE'
  else if IsEqualGuid(pGuid,
                       MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION) then
    Result := 'MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION'
  else if IsEqualGuid(pGuid,
                       MF_MT_FRAME_SIZE) then
    Result := 'MF_MT_FRAME_SIZE'
  else if IsEqualGuid(pGuid,
                       MF_MT_FRAME_RATE) then
    Result := 'MF_MT_FRAME_RATE'
  else if IsEqualGuid(pGuid,
                       MF_MT_FRAME_RATE_RANGE_MAX) then
    Result := 'MF_MT_FRAME_RATE_RANGE_MAX'
  else if IsEqualGuid(pGuid,
                       MF_MT_FRAME_RATE_RANGE_MIN) then
    Result := 'MF_MT_FRAME_RATE_RANGE_MIN'
  else if IsEqualGuid(pGuid,
                       MF_MT_PIXEL_ASPECT_RATIO) then
    Result := 'MF_MT_PIXEL_ASPECT_RATIO'
  else if IsEqualGuid(pGuid,
                       MF_MT_DRM_FLAGS) then
    Result := 'MF_MT_DRM_FLAGS'
  else if IsEqualGuid(pGuid,
                       MF_MT_PAD_CONTROL_FLAGS) then
    Result := 'MF_MT_PAD_CONTROL_FLAGS'
  else if IsEqualGuid(pGuid,
                       MF_MT_SOURCE_CONTENT_HINT) then
    Result := 'MF_MT_SOURCE_CONTENT_HINT'
  else if IsEqualGuid(pGuid,
                       MF_MT_VIDEO_CHROMA_SITING) then
    Result := 'MF_MT_VIDEO_CHROMA_SITING'
  else if IsEqualGuid(pGuid,
                       MF_MT_INTERLACE_MODE) then
    Result := 'MF_MT_INTERLACE_MODE'
  else if IsEqualGuid(pGuid,
                       MF_MT_TRANSFER_FUNCTION) then
    Result := 'MF_MT_TRANSFER_FUNCTION'
  else if IsEqualGuid(pGuid,
                       MF_MT_VIDEO_PRIMARIES) then
    Result := 'MF_MT_VIDEO_PRIMARIES'
  else if IsEqualGuid(pGuid,
                       MF_MT_CUSTOM_VIDEO_PRIMARIES) then
    Result := 'MF_MT_CUSTOM_VIDEO_PRIMARIES'
  else if IsEqualGuid(pGuid,
                       MF_MT_YUV_MATRIX) then
    Result := 'MF_MT_YUV_MATRIX'
  else if IsEqualGuid(pGuid,
                       MF_MT_VIDEO_LIGHTING) then
    Result := 'MF_MT_VIDEO_LIGHTING'
  else if IsEqualGuid(pGuid,
                       MF_MT_VIDEO_NOMINAL_RANGE) then
    Result := 'MF_MT_VIDEO_NOMINAL_RANGE'
  else if IsEqualGuid(pGuid,
                       MF_MT_GEOMETRIC_APERTURE) then
    Result := 'MF_MT_GEOMETRIC_APERTURE'
  else if IsEqualGuid(pGuid,
                       MF_MT_MINIMUM_DISPLAY_APERTURE) then
    Result := 'MF_MT_MINIMUM_DISPLAY_APERTURE'
  else if IsEqualGuid(pGuid,
                       MF_MT_PAN_SCAN_APERTURE) then
    Result := 'MF_MT_PAN_SCAN_APERTURE'
  else if IsEqualGuid(pGuid,
                       MF_MT_PAN_SCAN_ENABLED) then
    Result := 'MF_MT_PAN_SCAN_ENABLED'
  else if IsEqualGuid(pGuid,
                       MF_MT_AVG_BITRATE) then
    Result := 'MF_MT_AVG_BITRATE'
  else if IsEqualGuid(pGuid,
                       MF_MT_AVG_BIT_ERROR_RATE) then
    Result := 'MF_MT_AVG_BIT_ERROR_RATE'
  else if IsEqualGuid(pGuid,
                       MF_MT_MAX_KEYFRAME_SPACING) then
    Result := 'MF_MT_MAX_KEYFRAME_SPACING'
  else if IsEqualGuid(pGuid,
                       MF_MT_DEFAULT_STRIDE) then
    Result := 'MF_MT_DEFAULT_STRIDE'
  else if IsEqualGuid(pGuid,
                       MF_MT_PALETTE) then
    Result := 'MF_MT_PALETTE'
  else if IsEqualGuid(pGuid,
                       MF_MT_USER_DATA) then
    Result := 'MF_MT_USER_DATA'
  else if IsEqualGuid(pGuid,
                       MF_MT_AM_FORMAT_TYPE) then
    Result := 'MF_MT_AM_FORMAT_TYPE'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG_START_TIME_CODE) then
    Result := 'MF_MT_MPEG_START_TIME_CODE'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG2_PROFILE) then
    Result := 'MF_MT_MPEG2_PROFILE'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG2_LEVEL) then
    Result := 'MF_MT_MPEG2_LEVEL'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG2_FLAGS) then
    Result := 'MF_MT_MPEG2_FLAGS'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG_SEQUENCE_HEADER) then
    Result := 'MF_MT_MPEG_SEQUENCE_HEADER'
  else if IsEqualGuid(pGuid,
                       MF_MT_DV_AAUX_SRC_PACK_0) then
    Result := 'MF_MT_DV_AAUX_SRC_PACK_0'
  else if IsEqualGuid(pGuid,
                       MF_MT_DV_AAUX_CTRL_PACK_0) then
    Result := 'MF_MT_DV_AAUX_CTRL_PACK_0'
  else if IsEqualGuid(pGuid,
                       MF_MT_DV_AAUX_SRC_PACK_1) then
    Result := 'MF_MT_DV_AAUX_SRC_PACK_1'
  else if IsEqualGuid(pGuid,
                       MF_MT_DV_AAUX_CTRL_PACK_1) then
    Result := 'MF_MT_DV_AAUX_CTRL_PACK_1'
  else if IsEqualGuid(pGuid,
                       MF_MT_DV_VAUX_SRC_PACK) then
    Result := 'MF_MT_DV_VAUX_SRC_PACK'
  else if IsEqualGuid(pGuid,
                       MF_MT_DV_VAUX_CTRL_PACK) then
    Result := 'MF_MT_DV_VAUX_CTRL_PACK'
  else if IsEqualGuid(pGuid,
                       MF_MT_ARBITRARY_HEADER) then
    Result := 'MF_MT_ARBITRARY_HEADER'
  else if IsEqualGuid(pGuid,
                       MF_MT_ARBITRARY_FORMAT) then
    Result := 'MF_MT_ARBITRARY_FORMAT'
  else if IsEqualGuid(pGuid,
                       MF_MT_IMAGE_LOSS_TOLERANT) then
    Result := 'MF_MT_IMAGE_LOSS_TOLERANT'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG4_SAMPLE_DESCRIPTION) then
    Result := 'MF_MT_MPEG4_SAMPLE_DESCRIPTION'
  else if IsEqualGuid(pGuid,
                       MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY) then
    Result := 'MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY'
  else if IsEqualGuid(pGuid,
                       MF_MT_ORIGINAL_4CC) then
    Result := 'MF_MT_ORIGINAL_4CC'
  else if IsEqualGuid(pGuid,
                       MF_MT_ORIGINAL_WAVE_FORMAT_TAG) then
    Result := 'MF_MT_ORIGINAL_WAVE_FORMAT_TAG'


  // Media types

  else if IsEqualGuid(pGuid,
                       MFMediaType_Audio) then
    Result := 'MFMediaType_Audio'
  else if IsEqualGuid(pGuid,
                       MFMediaType_Video) then
    Result := 'MFMediaType_Video'
  else if IsEqualGuid(pGuid,
                       MFMediaType_Protected) then
    Result := 'MFMediaType_Protected'

  else if IsEqualGuid(pGuid,
                       MFMediaType_SAMI) then
    Result := 'MFMediaType_SAMI'
  else if IsEqualGuid(pGuid,
                       MFMediaType_Script) then
    Result := 'MFMediaType_Script'
  else if IsEqualGuid(pGuid,
                       MFMediaType_Image) then
    Result := 'MFMediaType_Image'
  else if IsEqualGuid(pGuid,
                       MFMediaType_HTML) then
    Result := 'MFMediaType_HTML'
  else if IsEqualGuid(pGuid,
                       MFMediaType_Binary) then
    Result := 'MFMediaType_Binary'
  else if IsEqualGuid(pGuid,
                       MFMediaType_FileTransfer) then
    Result := 'MFMediaType_FileTransfer'


  else if IsEqualGuid(pGuid,
                       MFVideoFormat_AI44) then //   FCC('AI44')
    Result := 'MFVideoFormat_AI44'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_ARGB32) then //   D3DFMT_A8R8G8B8
    Result := 'MFVideoFormat_ARGB32'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_AYUV) then //   FCC('AYUV')
    Result := 'MFVideoFormat_AYUV'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_DV25) then //   FCC('dv25')
    Result := 'MFVideoFormat_DV25'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_DV50) then //   FCC('dv50')
    Result := 'MFVideoFormat_DV50'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_DVH1) then //   FCC('dvh1')
    Result := 'MFVideoFormat_DVH1'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_DVSD) then //   FCC('dvsd')
    Result := 'MFVideoFormat_DVSD'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_DVSL) then //   FCC('dvsl')
    Result := 'MFVideoFormat_DVSL'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_H264) then //   FCC('H264')
    Result := 'MFVideoFormat_H264'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_I420) then //   FCC('I420')
    Result := 'MFVideoFormat_I420'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_IYUV) then //   FCC('IYUV')
    Result := 'MFVideoFormat_IYUV'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_M4S2) then //   FCC('M4S2')
    Result := 'MFVideoFormat_M4S2'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MJPG) then
    Result := 'MFVideoFormat_MJPG'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MP43) then //   FCC('MP43')
    Result := 'MFVideoFormat_MP43'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MP4S) then //   FCC('MP4S')
    Result := 'MFVideoFormat_MP4S'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MP4V) then //   FCC('MP4V')
    Result := 'MFVideoFormat_MP4V'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MPG1) then //   FCC('MPG1')
    Result := 'MFVideoFormat_MPG1'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MSS1) then //   FCC('MSS1')
    Result := 'MFVideoFormat_MSS1'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_MSS2) then //   FCC('MSS2')
    Result := 'MFVideoFormat_MSS2'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_NV11) then //   FCC('NV11')
    Result := 'MFVideoFormat_NV11'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_NV12) then //   FCC('NV12')
    Result := 'MFVideoFormat_NV12'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_P010) then //   FCC('P010')
    Result := 'MFVideoFormat_P010'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_P016) then //   FCC('P016')
    Result := 'MFVideoFormat_P016'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_P210) then //   FCC('P210')
    Result := 'MFVideoFormat_P210'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_P216) then //   FCC('P216')
    Result := 'MFVideoFormat_P216'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_RGB24) then //  D3DFMT_R8G8B8
    Result := 'MFVideoFormat_RGB24'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_RGB32) then //  D3DFMT_X8R8G8B8
    Result := 'MFVideoFormat_RGB32'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_RGB555) then //   D3DFMT_X1R5G5B5
    Result := 'MFVideoFormat_RGB555'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_RGB565) then //   D3DFMT_R5G6B5
    Result := 'MFVideoFormat_RGB565'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_RGB8) then
    Result := 'MFVideoFormat_RGB8'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_UYVY) then //   FCC('UYVY')
    Result := 'MFVideoFormat_UYVY'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_v210) then //   FCC('v210')
    Result := 'MFVideoFormat_v210'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_v410) then //   FCC('v410')
    Result := 'MFVideoFormat_v410'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_WMV1) then //   FCC('WMV1')
    Result := 'MFVideoFormat_WMV1'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_WMV2) then //   FCC('WMV2')
    Result := 'MFVideoFormat_WMV2'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_WMV3) then //   FCC('WMV3')
    Result := 'MFVideoFormat_WMV3'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_WVC1) then //   FCC('WVC1')
    Result := 'MFVideoFormat_WVC1'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_Y210) then //   FCC('Y210')
    Result := 'MFVideoFormat_Y210'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_Y216) then //   FCC('Y216')
    Result := 'MFVideoFormat_Y216'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_Y410) then //   FCC('Y410')
    Result := 'MFVideoFormat_Y410'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_Y416) then //   FCC('Y416')
    Result := 'MFVideoFormat_Y416'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_Y41P) then
    Result := 'MFVideoFormat_Y41P'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_Y41T) then
    Result := 'MFVideoFormat_Y41T'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_YUY2) then //   FCC('YUY2')
    Result := 'MFVideoFormat_YUY2'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_YV12) then //   FCC('YV12')
    Result := 'MFVideoFormat_YV12'
  else if IsEqualGuid(pGuid,
                       MFVideoFormat_YVYU) then
    Result := 'MFVideoFormat_YVYU'


  else if IsEqualGuid(pGuid,
                       MFAudioFormat_PCM) then //        WAVE_FORMAT_PCM
    Result := 'MFAudioFormat_PCM'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_Float) then //      WAVE_FORMAT_IEEE_FLOAT
    Result := 'MFAudioFormat_Float'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_DTS) then //        WAVE_FORMAT_DTS
    Result := 'MFAudioFormat_DTS'
  else if IsEqualGuid(pGuid,
                        MFAudioFormat_Dolby_AC3_SPDIF) then//  WAVE_FORMAT_DOLBY_AC3_SPDIF
    Result := 'MFAudioFormat_Dolby_AC3_SPDIF'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_DRM) then //        WAVE_FORMAT_DRM
    Result := 'MFAudioFormat_DRM'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_WMAudioV8) then //    WAVE_FORMAT_WMAUDIO2
    Result := 'MFAudioFormat_WMAudioV8'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_WMAudioV9) then //    WAVE_FORMAT_WMAUDIO3
    Result := 'MFAudioFormat_WMAudioV9'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_WMAudio_Lossless) then // WAVE_FORMAT_WMAUDIO_LOSSLESS
    Result := 'MFAudioFormat_WMAudio_Lossless'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_WMASPDIF) then //     WAVE_FORMAT_WMASPDIF
    Result := 'MFAudioFormat_WMASPDIF'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_MSP1) then //       WAVE_FORMAT_WMAVOICE9
    Result := 'MFAudioFormat_MSP1'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_MP3) then //        WAVE_FORMAT_MPEGLAYER3
    Result := 'MFAudioFormat_MP3'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_MPEG) then //       WAVE_FORMAT_MPEG
    Result := 'MFAudioFormat_MPEG'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_AAC) then //        WAVE_FORMAT_MPEG_HEAAC
    Result := 'MFAudioFormat_AAC'
  else if IsEqualGuid(pGuid,
                       MFAudioFormat_ADTS) then //       WAVE_FORMAT_MPEG_ADTS_AAC
    Result := 'MFAudioFormat_ADTS'
  else
    Result := 'Unknown format.';
end;

//
function TMediaTypeDebug.LogAttributeValueByIndex(pAttributes: IMFAttributes;
                                                  pIndex: DWord): HRESULT;
var
  hr: HResult;
  pwcGuidName: string;
  pwcGuidValName: string;
  gdGuid: TGUID;
  pvVar: PROPVARIANT;

label
  Return;

begin
  pwcGuidName := '';
  pwcGuidValName := '';
  gdGuid := GUID_NULL;

  PropVariantInit(pvVar);

  hr := pAttributes.GetItemByIndex(pIndex,
                                   gdGuid,
                                   pvVar);
  if FAILED(hr) then
    goto Return;


  hr := GetGUIDName(gdGuid,
                    pwcGuidName);
  if FAILED(hr) then
    begin
       DebugMsg(Format('Unexpected error in function GetGUIDName. (HResult = %d)',
                       [hr]));
       goto Return;
    end;

  DebugMsg(Format('Guidname: %s',
                  [pwcGuidName]));

  hr := SpecialCaseAttributeValue(gdGuid,
                                  pvVar);
  if FAILED(hr) then
    goto Return;

  if (hr = S_FALSE) then
    begin
      case pvVar.vt of

        VT_UI4: DebugMsg(Format('%d',
                                [pvVar.ulVal]));

        VT_UI8: DebugMsg(Format('%d',
                               [pvVar.uhVal.QuadPart]));

        VT_R8:  DebugMsg(Format('%f',
                                [pvVar.dblVal]));

        VT_CLSID: begin
                    hr := GetGuidName(pvVar.puuid^,
                                      pwcGuidValName);
                    if SUCCEEDED(hr) then
                      DebugMsg(Format('%s',
                                      [pwcGuidValName]));

                  end;

        VT_LPWSTR: DebugMsg(Format('%s',
                                   [PChar(@pvVar.pwszVal)]));

        VT_VECTOR or VT_UI1 : DebugMsg('Byte Array');

        VT_UNKNOWN: DebugMsg('IUnknown');



        else
          DebugMsg(Format('Unexpected attribute type (vt = %d)',
                          [pvVar.vt]));

        end;
    end;

Return:
  PropVariantClear(pvVar);
  Result := hr;
end;


// Handle certain known special cases.
function TMediaTypeDebug.SpecialCaseAttributeValue(const pGuid: TGUID;
                                                   const pPropVar: PROPVARIANT): HRESULT;
var
  hr: HResult;

begin
  hr := S_OK;

  if (isEqualGuid(pGuid,
                  MF_MT_FRAME_RATE) or
      isEqualGuid(pGuid,
                  MF_MT_FRAME_RATE_RANGE_MAX) or
      isEqualGuid(pGuid,
                  MF_MT_FRAME_RATE_RANGE_MIN) or
      isEqualGuid(pGuid,
                  MF_MT_FRAME_SIZE) or
      isEqualGuid(pGuid,
                  MF_MT_PIXEL_ASPECT_RATIO)) then
    begin
      // Attributes that contain two packed 32-bit values.
      LogUINT32AsUINT64(pPropVar);
    end
  else if (isEqualGuid(pGuid,
                       MF_MT_GEOMETRIC_APERTURE) or
           isEqualGuid(pGuid,
                       MF_MT_MINIMUM_DISPLAY_APERTURE) or
           isEqualGuid(pGuid,
                       MF_MT_PAN_SCAN_APERTURE)) then
    begin
      // Attributes that has an MFVideoArea structure.
      hr := LogVideoArea(pPropVar);
    end
  else
    hr := S_FALSE;

  Result := hr;
end;

//
procedure TMediaTypeDebug.LogUINT32AsUINT64(const pPropVar: PROPVARIANT);
var
  uHigh: UINT32;
  uLow: UINT32;

begin
  Unpack2UINT32AsUINT64(pPropVar.uhVal.QuadPart,
                        uHigh,
                        uLow);

  DebugMsg(Format('%d x %d',
                  [uHigh, uLow]));
end;

//
function TMediaTypeDebug.LogVideoArea(const pPropVar: PROPVARIANT): HRESULT;
var
  pArea: MFVideoArea;
  i: Integer;
  ix, iy: SmallInt;


begin

  if (pPropVar.caub.cElems < SizeOf(MFVideoArea)) then
    Result := MF_E_BUFFERTOOSMALL
  else
    begin
{$POINTERMATH ON}
      // test  to check elements
      for i := 0 to pPropVar.caub.cElems -1 do
        begin
          ix := pPropVar.caub.pElems[i];
        end;

      pArea := MakeArea(pPropVar.caub.pElems[0],
                        pPropVar.caub.pElems[1],
                        pPropVar.caub.pElems[2],
                        pPropVar.caub.pElems[3]);
{$POINTERMATH OFF}

      DebugMsg(StrToPWideChar(Format('MFVideoArea: (OffsetX: %f, OffsetY: %f) (cx: %d, cy: %d)',
                                     [MFOffsetToFloat(pArea.OffsetX),
                                      MFOffsetToFloat(pArea.OffsetY),
                                      pArea.Area.cx,
                                      pArea.Area.cy])));
      Result := S_OK;
    end;
end;


//
procedure TMediaTypeDebug.DebugMsg(pFormat: string);
begin
{$IFDEF DEBUG}
  OutputDebugString(StrToPWideChar(Format('%s',
                                     [pFormat])));
{$ENDIF}
  slDebug.Append(Format('%d  %s',
                        [slDebug.Count, pFormat]))

end;






end.

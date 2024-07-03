//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.MFTUtils
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This file contains a class that attempts to list the Media
//              Foundation Transforms (MFTs) available on the system.
//              See https://learn.microsoft.com/en-us/windows/win32/medfound/registering-and-enumerating-mfts.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: MFListTransforms.cpp
// Author: Aaron Clauson (aaron@sipsorcery.com)
// Copyright (c) Aaron Clauson	  Created, Hobart, Australia. All rights reserved.
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.Dbg.MFTUtils;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Unknwn,
  ActiveX,
  System.SysUtils,
  VCl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib;


//#include "../Common/MFUtility.h"


type

  TMftInput = (YUV2,
               RGB24,
               PCM);

  TMftOutput = (all,
                // Video
                H264,
                H265,
                VP8,
                VP9,
                // Audio
                MP3,
                FLAC,
                ALAC,
                OPUS);

  TReadable  = record
    pwcGuid: PWideChar;
    pwcFriendlyName: PWideChar;
  end;
  TReadableArray = array of TReadable;

  TListMFTs = class
  private
    FReadableArray: TReadableArray;

    procedure initializeRegTypeInfo();

  public
    // Video MFT's
    videoYUV   : MFT_REGISTER_TYPE_INFO; // YUY2
    videoRGB24 : MFT_REGISTER_TYPE_INFO; // RGB24
    videoRGB32 : MFT_REGISTER_TYPE_INFO; // RGB32
    videoNV11  : MFT_REGISTER_TYPE_INFO; // NV11
    videoNV12  : MFT_REGISTER_TYPE_INFO; // NV12
    videoNV21  : MFT_REGISTER_TYPE_INFO; // NV21
    videoYV12  : MFT_REGISTER_TYPE_INFO; // YV12
    videoMPEG2 : MFT_REGISTER_TYPE_INFO; // MPEG2 (H.262)
    videoH263  : MFT_REGISTER_TYPE_INFO; // H263
    videoH264  : MFT_REGISTER_TYPE_INFO; // H264
    videoH265  : MFT_REGISTER_TYPE_INFO; // H265
    videoHEVC  : MFT_REGISTER_TYPE_INFO; // HEVC Main profile and Main Still Picture profile. >= Windows 8.1
    videoVP8   : MFT_REGISTER_TYPE_INFO; // VP80
    videoVP9   : MFT_REGISTER_TYPE_INFO; // VP90
    videoWMV1  : MFT_REGISTER_TYPE_INFO; // Windows Media Video codec version 7.
    videoWMV2  : MFT_REGISTER_TYPE_INFO; // Windows Media Video 8 codec.
    videoWMV3  : MFT_REGISTER_TYPE_INFO; // Windows Media Video 9 codec.

    // Audio MFT's
    audioPCM   : MFT_REGISTER_TYPE_INFO; // PCM
	  audioMP3   : MFT_REGISTER_TYPE_INFO; // MP3
    audioFLAC  : MFT_REGISTER_TYPE_INFO; // FLAC
    audioALAC  : MFT_REGISTER_TYPE_INFO; // Apple Lossless Audio Codec, Supported in Windows 10 and later.
    audioDAC3  : MFT_REGISTER_TYPE_INFO; // Dolby AC-3, Supported in Windows 10 and later.
    audioOPUS  : MFT_REGISTER_TYPE_INFO; // Opus, Supported in Windows 10 and later.
    audioAAC   : MFT_REGISTER_TYPE_INFO; // AAC Note: Equivalent to MEDIASUBTYPE_MPEG_HEAAC
    audioMPEG  : MFT_REGISTER_TYPE_INFO; // MPEG-1 audio.
    audioWMAL  : MFT_REGISTER_TYPE_INFO; // Windows Media Audio 9 Lossless codec or Windows Media Audio 9.1 codec.
    audioWMA8  : MFT_REGISTER_TYPE_INFO; // Windows Media Audio 8 codec, Windows Media Audio 9 codec or Windows Media Audio 9.1 codec.
    audioWMA9  : MFT_REGISTER_TYPE_INFO; // Windows Media Audio 9 Professional codec or Windows Media Audio 9.1 Professional codec.


    constructor Create();
    destructor Destroy(); override;

    procedure ListTansforms(pInput: MFT_REGISTER_TYPE_INFO;
                            pOutput: PMFT_REGISTER_TYPE_INFO);

    function GetMFT(pMFActivate: IMFActivate;
                    out pwcGuid: PWideChar;
                    out pwcFriendlyName: PWideChar): HResult;

    property FriendlyNames: TReadableArray read FReadableArray;

  end;

  // Audio subtype GUIDS : https://learn.microsoft.com/en-us/windows/win32/medfound/audio-subtype-guids
  // Video subtype GUIDS : https://learn.microsoft.com/en-us/windows/win32/medfound/video-subtype-guids


implementation


constructor TListMFTs.Create();
begin
  inherited;

  InitializeRegTypeInfo();
end;

destructor TListMFTs.Destroy();
begin

  inherited;
end;


//
// Prints out a list of all the Media Foundation Transforms that match for
// the input media type and the optional output media type.
// @param[in] pInput: pointer to the MFT input type to match.
// @param[in] pOutput: Optional. Pointer to the MFT output type to match. If
//                     nil all MFT's for the input type will be listed.
//
procedure TListMFTs.ListTansforms(pInput: MFT_REGISTER_TYPE_INFO;
                                  pOutput: PMFT_REGISTER_TYPE_INFO);
var
  hr: HResult;
  ppActivate: PIMFActivate;
  category: TGUID;
  mftCount: UINT32;
  i: Integer;

label
  done;

begin

  if IsEqualGuid(pInput.guidMajorType,
                 MFMediaType_Audio) then
    category := MFMediaType_Audio
  else if IsEqualGuid(pInput.guidMajorType,
                      MFMediaType_Video) then
    category := MFMediaType_Video;
 // else if .. if you want to get more mft types.

  hr := MFTEnumEx(category,
                  MFT_ENUM_FLAG_SYNCMFT or
                  MFT_ENUM_FLAG_ASYNCMFT or
                  MFT_ENUM_FLAG_HARDWARE or
                  MFT_ENUM_FLAG_SORTANDFILTER,
                  @pInput,
                  @pOutput,
                  ppActivate,
                  mftCount);

  if FAILED(hr) then
    begin
      ShowMessage(Format('MFTEnumEx failed with error: %d.',
                         [hr]));
      goto done;
    end;

  {$POINTERMATH ON}
  SetLength(FReadableArray,
            mftCount);

  for i := 0 to mftCount - 1 do
    begin
      GetMFT(ppActivate[i],
             FReadableArray[i].pwcGuid,
             FReadableArray[i].pwcFriendlyName);
    end;

done:

  for i := 0 to mftCount - 1 do
    SafeRelease(ppActivate[i]);

  CoTaskMemFree(ppActivate);
  ppActivate := nil;
end;


//
// Gets the friendly name and guid for a Media Foundation Transform.
//
function TListMFTs.GetMFT(pMFActivate: IMFActivate;
                          out pwcGuid: PWideChar;
                          out pwcFriendlyName: PWideChar): HResult;
var
  hr: HResult;
  guidMFT: TGUID;
  len: UINT;

begin

  // get the CLSID GUID from the IMFAttributes of the activation object
  hr := pMFActivate.GetGUID(MFT_TRANSFORM_CLSID_Attribute,
                            guidMFT);

  if SUCCEEDED(hr) then
    hr := StringFromIID(guidMFT,
                        pwcGuid);

  if SUCCEEDED(hr) then
    // get the friendly name string from the IMFAttributes of the activation object
    hr := pMFActivate.GetAllocatedString(MFT_FRIENDLY_NAME_Attribute,
                                         pwcFriendlyName,
                                         len);

  if SUCCEEDED(hr) then
    begin
      CoTaskMemFree(pwcGuid);
      CoTaskMemFree(pwcFriendlyName);
    end;

  Result := hr;
end;


procedure TListMFTs.InitializeRegTypeInfo();
begin
  // Video
  videoYuv.guidMajorType   := MFMediaType_Video;
  videoYuv.guidSubtype     := MFVideoFormat_YUY2;

  videoRgb24.guidMajorType := MFMediaType_Video;
  videoRgb24.guidSubtype   := MFVideoFormat_RGB24;

  videoRgb32.guidMajorType := MFMediaType_Video;
  videoRgb32.guidSubtype   := MFVideoFormat_RGB32;

  videoNV11.guidMajorType  := MFMediaType_Video;
  videoNV11.guidSubtype    := MFVideoFormat_NV11;

  videoNV12.guidMajorType  := MFMediaType_Video;
  videoNV12.guidSubtype    := MFVideoFormat_NV12;

  videoNV21.guidMajorType  := MFMediaType_Video;
  videoNV21.guidSubtype    := MFVideoFormat_NV21;

  videoYV12.guidMajorType  := MFMediaType_Video;
  videoYV12.guidSubtype    := MFVideoFormat_YV12;

  videoMPEG2.guidMajorType  := MFMediaType_Video;
  videoMPEG2.guidSubtype    := MFVideoFormat_MPEG2;

  videoH263.guidMajorType  := MFMediaType_Video;
  videoH263.guidSubtype    := MFVideoFormat_H263;

  videoH264.guidMajorType  := MFMediaType_Video;
  videoH264.guidSubtype    := MFVideoFormat_H264;

  videoH265.guidMajorType  := MFMediaType_Video;
  videoH265.guidSubtype    := MFVideoFormat_H265;

  videoHEVC.guidMajorType  := MFMediaType_Video;
  videoHEVC.guidSubtype    := MFVideoFormat_HEVC;

  videoVP8.guidMajorType   := MFMediaType_Video;
  videoVP8.guidSubtype     := MFVideoFormat_VP80;

  videoVP9.guidMajorType   := MFMediaType_Video;
  videoVP9.guidSubtype     := MFVideoFormat_VP90;

  videoWMV1.guidMajorType   := MFMediaType_Video;
  videoWMV1.guidSubtype     := MFVideoFormat_WMV1;

  videoWMV2.guidMajorType   := MFMediaType_Video;
  videoWMV2.guidSubtype     := MFVideoFormat_WMV2;

  videoWMV3.guidMajorType   := MFMediaType_Video;
  videoWMV3.guidSubtype     := MFVideoFormat_WMV3;


  // Audio
  audioPcm.guidMajorType   := MFMediaType_Audio;
  audioPcm.guidSubtype     := MFAudioFormat_PCM;

  audioMp3.guidMajorType   := MFMediaType_Audio;
  audioMp3.guidSubtype     := MFAudioFormat_MP3;

  audioFLAC.guidMajorType  := MFMediaType_Audio;
  audioFLAC.guidSubtype    := MFAudioFormat_FLAC;

  audioALAC.guidMajorType  := MFMediaType_Audio;
  audioALAC.guidSubType    := MFAudioFormat_ALAC;

  audioOPUS.guidMajorType  := MFMediaType_Audio;
  audioOPUS.guidSubType    := MFAudioFormat_Opus;

  audioDAC3.guidMajorType  := MFMediaType_Audio;
  audioDAC3.guidSubType    := MFAudioFormat_Dolby_AC3;

  audioAAC.guidMajorType   := MFMediaType_Video;
  audioAAC.guidSubtype     := MFAudioFormat_AAC;

  audioMPEG.guidMajorType   := MFMediaType_Video;
  audioMPEG.guidSubtype     := MFAudioFormat_MPEG;

  audioWMAL.guidMajorType   := MFMediaType_Video;
  audioWMAL.guidSubtype     := MFAudioFormat_WMAudio_Lossless;

  audioWMA8.guidMajorType   := MFMediaType_Video;
  audioWMA8.guidSubtype     := MFAudioFormat_WMAudioV8;

  audioWMA9.guidMajorType   := MFMediaType_Video;
  audioWMA9.guidSubtype     := MFAudioFormat_WMAudioV9;

end;

end.

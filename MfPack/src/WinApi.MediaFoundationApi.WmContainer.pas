// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.WmContainer.pas
// Kind: Pascal / Delphi unit
// Release date: 23-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
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
// Source: wmcontainer.h
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
unit WinApi.MediaFoundationApi.WmContainer;

  {$HPPEMIT '#include "wmcontainer.h"'}

interface

uses

  {WinApi}
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  MFASF_MIN_HEADER_BYTES  = (SizeOf(TGUID) + SizeOf(QWORD));
  {$EXTERNALSYM MFASF_MIN_HEADER_BYTES}

  MF_PD_ASF_FILEPROPERTIES_FILE_ID               : TGUID = '{3de649b4-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_FILE_ID}
  MF_PD_ASF_FILEPROPERTIES_CREATION_TIME         : TGUID = '{3de649b6-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_CREATION_TIME}
  MF_PD_ASF_FILEPROPERTIES_PACKETS               : TGUID = '{3de649b7-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_PACKETS}
  MF_PD_ASF_FILEPROPERTIES_PLAY_DURATION         : TGUID = '{3de649b8-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_PLAY_DURATION}
  MF_PD_ASF_FILEPROPERTIES_SEND_DURATION         : TGUID = '{3de649b9-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_SEND_DURATION}
  MF_PD_ASF_FILEPROPERTIES_PREROLL               : TGUID = '{3de649ba-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_PREROLL}
  MF_PD_ASF_FILEPROPERTIES_FLAGS                 : TGUID = '{3de649bb-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_FLAGS}
  MF_PD_ASF_FILEPROPERTIES_MIN_PACKET_SIZE       : TGUID = '{3de649bc-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_MIN_PACKET_SIZE}
  MF_PD_ASF_FILEPROPERTIES_MAX_PACKET_SIZE       : TGUID = '{3de649bd-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_MAX_PACKET_SIZE}
  MF_PD_ASF_FILEPROPERTIES_MAX_BITRATE           : TGUID = '{3de649be-d76d-4e66-9ec9-78120fb4c7e3}';
  {$EXTERNALSYM MF_PD_ASF_FILEPROPERTIES_MAX_BITRATE}
  MF_PD_ASF_CONTENTENCRYPTION_TYPE               : TGUID = '{8520fe3d-277e-46ea-99e4-e30a86db12be}';
  {$EXTERNALSYM MF_PD_ASF_CONTENTENCRYPTION_TYPE}
  MF_PD_ASF_CONTENTENCRYPTION_KEYID              : TGUID = '{8520fe3e-277e-46ea-99e4-e30a86db12be}';
  {$EXTERNALSYM MF_PD_ASF_CONTENTENCRYPTION_KEYID}
  MF_PD_ASF_CONTENTENCRYPTION_SECRET_DATA        : TGUID = '{8520fe3f-277e-46ea-99e4-e30a86db12be}';
  {$EXTERNALSYM MF_PD_ASF_CONTENTENCRYPTION_SECRET_DATA}
  MF_PD_ASF_CONTENTENCRYPTION_LICENSE_URL        : TGUID = '{8520fe40-277e-46ea-99e4-e30a86db12be}';
  {$EXTERNALSYM MF_PD_ASF_CONTENTENCRYPTION_LICENSE_URL}
  MF_PD_ASF_CONTENTENCRYPTIONEX_ENCRYPTION_DATA  : TGUID = '{62508be5-ecdf-4924-a359-72bab3397b9d}';
  {$EXTERNALSYM MF_PD_ASF_CONTENTENCRYPTIONEX_ENCRYPTION_DATA}
  MF_PD_ASF_LANGLIST                             : TGUID = '{f23de43c-9977-460d-a6ec-32937f160f7d}';
  {$EXTERNALSYM MF_PD_ASF_LANGLIST}

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MF_PD_ASF_LANGLIST_LEGACYORDER                 : TGUID = '{f23de43d-9977-460d-a6ec-32937f160f7d}';
  {$EXTERNALSYM MF_PD_ASF_LANGLIST_LEGACYORDER}
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MF_PD_ASF_MARKER                               : TGUID = '{5134330e-83a6-475e-a9d5-4fb875fb2e31}';
  {$EXTERNALSYM MF_PD_ASF_MARKER}
  MF_PD_ASF_SCRIPT                               : TGUID = '{e29cd0d7-d602-4923-a7fe-73fd97ecc650}';
  {$EXTERNALSYM MF_PD_ASF_SCRIPT}
  MF_PD_ASF_CODECLIST                            : TGUID = '{e4bb3509-c18d-4df1-bb99-7a36b3cc4119}';
  {$EXTERNALSYM MF_PD_ASF_CODECLIST}
  MF_PD_ASF_METADATA_IS_VBR                      : TGUID = '{5fc6947a-ef60-445d-b449-442ecc78b4c1}';
  {$EXTERNALSYM MF_PD_ASF_METADATA_IS_VBR}
  MF_PD_ASF_METADATA_V8_VBRPEAK                  : TGUID = '{5fc6947b-ef60-445d-b449-442ecc78b4c1}';
  {$EXTERNALSYM MF_PD_ASF_METADATA_V8_VBRPEAK}
  MF_PD_ASF_METADATA_V8_BUFFERAVERAGE            : TGUID = '{5fc6947c-ef60-445d-b449-442ecc78b4c1}';
  {$EXTERNALSYM MF_PD_ASF_METADATA_V8_BUFFERAVERAGE}
  MF_PD_ASF_METADATA_LEAKY_BUCKET_PAIRS          : TGUID = '{5fc6947d-ef60-445d-b449-442ecc78b4c1}';
  {$EXTERNALSYM MF_PD_ASF_METADATA_LEAKY_BUCKET_PAIRS}
  MF_PD_ASF_DATA_START_OFFSET                    : TGUID = '{e7d5b3e7-1f29-45d3-8822-3e78fae272ed}';
  {$EXTERNALSYM MF_PD_ASF_DATA_START_OFFSET}
  MF_PD_ASF_DATA_LENGTH                          : TGUID = '{e7d5b3e8-1f29-45d3-8822-3e78fae272ed}';
  {$EXTERNALSYM MF_PD_ASF_DATA_LENGTH}
  MF_SD_ASF_EXTSTRMPROP_LANGUAGE_ID_INDEX        : TGUID = '{48f8a522-305d-422d-8524-2502dda33680}';
  {$EXTERNALSYM MF_SD_ASF_EXTSTRMPROP_LANGUAGE_ID_INDEX}
  MF_SD_ASF_EXTSTRMPROP_AVG_DATA_BITRATE         : TGUID = '{48f8a523-305d-422d-8524-2502dda33680}';
  {$EXTERNALSYM MF_SD_ASF_EXTSTRMPROP_AVG_DATA_BITRATE}
  MF_SD_ASF_EXTSTRMPROP_AVG_BUFFERSIZE           : TGUID = '{48f8a524-305d-422d-8524-2502dda33680}';
  {$EXTERNALSYM MF_SD_ASF_EXTSTRMPROP_AVG_BUFFERSIZE}
  MF_SD_ASF_EXTSTRMPROP_MAX_DATA_BITRATE         : TGUID = '{48f8a525-305d-422d-8524-2502dda33680}';
  {$EXTERNALSYM MF_SD_ASF_EXTSTRMPROP_MAX_DATA_BITRATE}
  MF_SD_ASF_EXTSTRMPROP_MAX_BUFFERSIZE           : TGUID = '{48f8a526-305d-422d-8524-2502dda33680}';
  {$EXTERNALSYM MF_SD_ASF_EXTSTRMPROP_MAX_BUFFERSIZE}
  MF_SD_ASF_STREAMBITRATES_BITRATE               : TGUID = '{a8e182ed-afc8-43d0-b0d1-f65bad9da558}';
  {$EXTERNALSYM MF_SD_ASF_STREAMBITRATES_BITRATE}
  MF_SD_ASF_METADATA_DEVICE_CONFORMANCE_TEMPLATE : TGUID = '{245e929d-c44e-4f7e-bb3c-77d4dfd27f8a}';
  {$EXTERNALSYM MF_SD_ASF_METADATA_DEVICE_CONFORMANCE_TEMPLATE}
  MF_PD_ASF_INFO_HAS_AUDIO                       : TGUID = '{80e62295-2296-4a44-b31c-d103c6fed23c}';
  {$EXTERNALSYM MF_PD_ASF_INFO_HAS_AUDIO}
  MF_PD_ASF_INFO_HAS_VIDEO                       : TGUID = '{80e62296-2296-4a44-b31c-d103c6fed23c}';
  {$EXTERNALSYM MF_PD_ASF_INFO_HAS_VIDEO}
  MF_PD_ASF_INFO_HAS_NON_AUDIO_VIDEO             : TGUID = '{80e62297-2296-4a44-b31c-d103c6fed23c}';
  {$EXTERNALSYM MF_PD_ASF_INFO_HAS_NON_AUDIO_VIDEO}


  MF_ASFPROFILE_MINPACKETSIZE                    : TGUID = '{22587626-47de-4168-87f5-b5aa9b12a8f0}';
  {$EXTERNALSYM MF_ASFPROFILE_MINPACKETSIZE}
  MF_ASFPROFILE_MAXPACKETSIZE                    : TGUID = '{22587627-47de-4168-87f5-b5aa9b12a8f0}';
  {$EXTERNALSYM MF_ASFPROFILE_MAXPACKETSIZE}


  MF_ASFSTREAMCONFIG_LEAKYBUCKET1                : TGUID = '{c69b5901-ea1a-4c9b-b692-e2a0d29a8add}';
  {$EXTERNALSYM MF_ASFSTREAMCONFIG_LEAKYBUCKET1}
  MF_ASFSTREAMCONFIG_LEAKYBUCKET2                : TGUID = '{c69b5902-ea1a-4c9b-b692-e2a0d29a8add}';
  {$EXTERNALSYM MF_ASFSTREAMCONFIG_LEAKYBUCKET2}
  MFASFSampleExtension_SampleDuration            : TGUID = '{c6bd9450-867f-4907-83a3-c77921b733ad}';
  {$EXTERNALSYM MFASFSampleExtension_SampleDuration}
  MFASFSampleExtension_OutputCleanPoint          : TGUID = '{f72a3c6f-6eb4-4ebc-b192-09ad9759e828}';
  {$EXTERNALSYM MFASFSampleExtension_OutputCleanPoint}
  MFASFSampleExtension_SMPTE                     : TGUID = '{399595ec-8667-4e2d-8fdb-98814ce76c1e}';
  {$EXTERNALSYM MFASFSampleExtension_SMPTE}
  MFASFSampleExtension_FileName                  : TGUID = '{e165ec0e-19ed-45d7-b4a7-25cbd1e28e9b}';
  {$EXTERNALSYM MFASFSampleExtension_FileName}
  MFASFSampleExtension_ContentType               : TGUID = '{d590dc20-07bc-436c-9cf7-f3bbfbf1a4dc}';
  {$EXTERNALSYM MFASFSampleExtension_ContentType}
  MFASFSampleExtension_PixelAspectRatio          : TGUID = '{1b1ee554-f9ea-4bc8-821a-376b74e4c4b8}';
  {$EXTERNALSYM MFASFSampleExtension_PixelAspectRatio}
  MFASFSampleExtension_Encryption_SampleID       : TGUID = '{6698B84E-0AFA-4330-AEB2-1C0A98D7A44D}';
  {$EXTERNALSYM MFASFSampleExtension_Encryption_SampleID}
  MFASFSampleExtension_Encryption_KeyID          : TGUID = '{76376591-795f-4da1-86ed-9d46eca109a9}';
  {$EXTERNALSYM MFASFSampleExtension_Encryption_KeyID}


  MFASFMutexType_Language                        : TGUID = '{72178C2B-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFASFMutexType_Language}
  MFASFMutexType_Bitrate                         : TGUID = '{72178C2C-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFASFMutexType_Bitrate}
  MFASFMutexType_Presentation                    : TGUID = '{72178C2D-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFASFMutexType_Presentation}
  MFASFMutexType_Unknown                         : TGUID = '{72178C2E-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFASFMutexType_Unknown}


  MFASFSPLITTER_PACKET_BOUNDARY                  : TGUID = '{fe584a05-e8d6-42e3-b176-f1211705fb6f}';
  {$EXTERNALSYM MFASFSPLITTER_PACKET_BOUNDARY}


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MFASFINDEXER_TYPE_TIMECODE                     : TGUID = '{49815231-6bad-44fd-810a-3f60984ec7fd}';
  {$EXTERNALSYM MFASFINDEXER_TYPE_TIMECODE}
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MFASFINDEXER_PER_ENTRY_BYTES_DYNAMIC                   = $FFFF;
  {$EXTERNALSYM MFASFINDEXER_PER_ENTRY_BYTES_DYNAMIC}
  MFASFINDEXER_NO_FIXED_INTERVAL                         = MAXDW; // 0xFFFFFFFF
  {$EXTERNALSYM MFASFINDEXER_NO_FIXED_INTERVAL}
  MFASFINDEXER_READ_FOR_REVERSEPLAYBACK_OUTOFDATASEGMENT = $FFFFFFFFFFFFFFFF;
  {$EXTERNALSYM MFASFINDEXER_READ_FOR_REVERSEPLAYBACK_OUTOFDATASEGMENT}
  MFASFINDEXER_APPROX_SEEK_TIME_UNKNOWN                  = $FFFFFFFFFFFFFFFF;
  {$EXTERNALSYM MFASFINDEXER_APPROX_SEEK_TIME_UNKNOWN}


  MFPKEY_ASFMEDIASINK_BASE_SENDTIME          : PROPERTYKEY = (fmtid: (D1: $cddcbc82;
                                                                      D2: $3411;
                                                                      D3: $4119;
                                                                      D4: ($91, $35, $84, $23, $c4, $1b, $39, $57));
                                                                      pid: 3);
  {$EXTERNALSYM MFPKEY_ASFMEDIASINK_BASE_SENDTIME}

  MFPKEY_ASFMEDIASINK_AUTOADJUST_BITRATE     : PROPERTYKEY = (fmtid: (D1: $cddcbc82;
                                                                      D2: $3411; D3: $4119;
                                                                      D4: ($91, $35, $84, $23, $c4, $1b, $39, $57));
                                                                      pid: 4);
  {$EXTERNALSYM MFPKEY_ASFMEDIASINK_AUTOADJUST_BITRATE}

  MFPKEY_ASFMEDIASINK_DRMACTION              : PROPERTYKEY = (fmtid: (D1: $a1db6f6c;
                                                                      D2: $1d0a;
                                                                      D3: $4cb6;
                                                                      D4: ($82, $54, $cb, $36, $be, $ed, $bc, $48));
                                                                      pid: 5);
  {$EXTERNALSYM MFPKEY_ASFMEDIASINK_DRMACTION}

  MFPKEY_ASFSTREAMSINK_CORRECTED_LEAKYBUCKET : PROPERTYKEY = (fmtid: (D1: $a2f152fb;
                                                                      D2: $8ad9;
                                                                      D3: $0a11;
                                                                      D4: ($b3, $45, $2c, $e2, $fa, $d8, $72, $3d));
                                                                      pid: 1);
  {$EXTERNALSYM MFPKEY_ASFSTREAMSINK_CORRECTED_LEAKYBUCKET}


  // Define WMContainer constants
  MFASF_MAX_STREAM_NUMBER             = 127;
  {$EXTERNALSYM MFASF_MAX_STREAM_NUMBER}
  MFASF_INVALID_STREAM_NUMBER         = (MFASF_MAX_STREAM_NUMBER + 1);
  {$EXTERNALSYM MFASF_INVALID_STREAM_NUMBER}
  MFASF_PAYLOADEXTENSION_MAX_SIZE     = $FF;
  {$EXTERNALSYM MFASF_PAYLOADEXTENSION_MAX_SIZE}
  MFASF_PAYLOADEXTENSION_VARIABLE_SIZE= $FFFF;
  {$EXTERNALSYM MFASF_PAYLOADEXTENSION_VARIABLE_SIZE}
  MFASF_DEFAULT_BUFFER_WINDOW_MS      = 3000;
  {$EXTERNALSYM MFASF_DEFAULT_BUFFER_WINDOW_MS}


type
  PMFASF_SPLITTERFLAGS = ^MFASF_SPLITTERFLAGS;
  MFASF_SPLITTERFLAGS      = DWord;
  {$EXTERNALSYM MFASF_SPLITTERFLAGS}
  TMfasfSplitterflags = MFASF_SPLITTERFLAGS;
  {$EXTERNALSYM TMfasfSplitterflags}
const
  MFASF_SPLITTER_REVERSE = TMfasfSplitterflags($1);
  {$EXTERNALSYM MFASF_SPLITTER_REVERSE}
  MFASF_SPLITTER_WMDRM   = TMfasfSplitterflags($2);
  {$EXTERNALSYM MFASF_SPLITTER_WMDRM}


type
  PASF_STATUSFLAGS = ^ASF_STATUSFLAGS;
  cwASF_STATUSFLAGS                  = DWord;
  {$EXTERNALSYM cwASF_STATUSFLAGS}
  ASF_STATUSFLAGS = cwASF_STATUSFLAGS;
  {$EXTERNALSYM ASF_STATUSFLAGS}
const
  ASF_STATUSFLAGS_INCOMPLETE     = ASF_STATUSFLAGS($1);
  {$EXTERNALSYM ASF_STATUSFLAGS_INCOMPLETE}
  ASF_STATUSFLAGS_NONFATAL_ERROR = ASF_STATUSFLAGS($2);
  {$EXTERNALSYM ASF_STATUSFLAGS_NONFATAL_ERROR}


type
  PMFASF_MULTIPLEXERFLAGS = ^MFASF_MULTIPLEXERFLAGS;
  cwMFASF_MULTIPLEXERFLAGS = DWord;
  {$EXTERNALSYM cwMFASF_MULTIPLEXERFLAGS}
  MFASF_MULTIPLEXERFLAGS = cwMFASF_MULTIPLEXERFLAGS;
  {$EXTERNALSYM MFASF_MULTIPLEXERFLAGS}
const
  MFASF_MULTIPLEXER_AUTOADJUST_BITRATE = MFASF_MULTIPLEXERFLAGS($1);
  {$EXTERNALSYM MFASF_MULTIPLEXER_AUTOADJUST_BITRATE}


type

  PASF_MUX_STATISTICS = ^ASF_MUX_STATISTICS;
  cwASF_MUX_STATISTICS = record
    cFramesWritten: DWORD;
    cFramesDropped: DWORD;
  end;
  {$EXTERNALSYM cwASF_MUX_STATISTICS}
  ASF_MUX_STATISTICS = cwASF_MUX_STATISTICS;
  {$EXTERNALSYM ASF_MUX_STATISTICS}


type
  PMFASF_INDEXER_FLAGS = ^MFASF_INDEXER_FLAGS;
  MFASF_INDEXERFLAGS = DWord;
  {$EXTERNALSYM MFASF_INDEXERFLAGS}
  MFASF_INDEXER_FLAGS = MFASF_INDEXERFLAGS;
  {$EXTERNALSYM MFASF_INDEXER_FLAGS}
const
  MFASF_INDEXER_WRITE_NEW_INDEX          = MFASF_INDEXER_FLAGS($1);
  {$EXTERNALSYM MFASF_INDEXER_WRITE_NEW_INDEX}
  MFASF_INDEXER_READ_FOR_REVERSEPLAYBACK = MFASF_INDEXER_FLAGS($2);
  {$EXTERNALSYM MFASF_INDEXER_READ_FOR_REVERSEPLAYBACK}
  MFASF_INDEXER_WRITE_FOR_LIVEREAD       = MFASF_INDEXER_FLAGS($4);
  {$EXTERNALSYM MFASF_INDEXER_WRITE_FOR_LIVEREAD}


type

  PASF_INDEX_IDENTIFIER = ^ASF_INDEX_IDENTIFIER;
  _ASF_INDEX_IDENTIFIER = record
    guidIndexType: TGUID;
    wStreamNumber: WORD;
  end;
  {$EXTERNALSYM _ASF_INDEX_IDENTIFIER}
  ASF_INDEX_IDENTIFIER = _ASF_INDEX_IDENTIFIER;
  {$EXTERNALSYM ASF_INDEX_IDENTIFIER}


  PASF_INDEX_DESCRIPTOR = ^ASF_INDEX_DESCRIPTOR;
  _ASF_INDEX_DESCRIPTOR = record
    Identifier: ASF_INDEX_IDENTIFIER;
    cPerEntryBytes: WORD;
    szDescription: array[0..31] of WideChar;
    dwInterval: DWORD;
  end;
  {$EXTERNALSYM _ASF_INDEX_DESCRIPTOR}
  ASF_INDEX_DESCRIPTOR = _ASF_INDEX_DESCRIPTOR;
  {$EXTERNALSYM ASF_INDEX_DESCRIPTOR}

type
  PMFASF_STREAMSELECTOR_FLAGS = ^MFASF_STREAMSELECTOR_FLAGS;
  MFASF_STREAMSELECTORFLAGS                  = DWord;
  {$EXTERNALSYM MFASF_STREAMSELECTORFLAGS}
  MFASF_STREAMSELECTOR_FLAGS = MFASF_STREAMSELECTORFLAGS;
  {$EXTERNALSYM MFASF_STREAMSELECTOR_FLAGS}
const
  MFASF_STREAMSELECTOR_DISABLE_THINNING    = MFASF_STREAMSELECTOR_FLAGS($1);
  {$EXTERNALSYM MFASF_STREAMSELECTOR_DISABLE_THINNING}
  MFASF_STREAMSELECTOR_USE_AVERAGE_BITRATE = MFASF_STREAMSELECTOR_FLAGS($2);
  {$EXTERNALSYM MFASF_STREAMSELECTOR_USE_AVERAGE_BITRATE}


type
  PASF_SELECTION_STATUS = ^ASF_SELECTION_STATUS;
  cwASF_SELECTION_STATUS = DWord;
  {$EXTERNALSYM cwASF_SELECTION_STATUS}
  ASF_SELECTION_STATUS = cwASF_SELECTION_STATUS;
  {$EXTERNALSYM ASF_SELECTION_STATUS}
const
  ASF_STATUS_NOTSELECTED     = ASF_SELECTION_STATUS(0);
  {$EXTERNALSYM ASF_STATUS_NOTSELECTED}
  ASF_STATUS_CLEANPOINTSONLY = ASF_SELECTION_STATUS(1);
  {$EXTERNALSYM ASF_STATUS_CLEANPOINTSONLY}
  ASF_STATUS_ALLDATAUNITS    = ASF_SELECTION_STATUS(2);
  {$EXTERNALSYM ASF_STATUS_ALLDATAUNITS}

  //
  // ASF Media Sink configuration properties
  // When creating the ASF Media Sink inproc (MFCreateASFMediaSink), QI the
  // ASF Media Sink for IPropertyStore and set these properties there.
  // When creating the ASF Media Sink via IMFActivate
  // (MFCreateASFMediaSinkActivate), set them on the Property Store obtained from
  // IMFASFContentInfo::GetEncodingConfigurationProperties.
  //

type
  //
  // WMDRM can be configured on the ASF Media Sink via the property
  // MFPKEY_ASFMEDIASINK_DRMACTION.
  //
  PMFSINK_WMDRMACTION = ^MFSINK_WMDRMACTION;
  _MFSINK_WMDRMACTION             = (
    MFSINK_WMDRMACTION_UNDEFINED  = 0, // Undefined action.
    MFSINK_WMDRMACTION_ENCODE     = 1, // Encode the content using Windows Media DRM. Use this flag if the source content does not have DRM protection.
    MFSINK_WMDRMACTION_TRANSCODE  = 2, // Transcode the content using Windows Media DRM. Use this flag if the source content has Windows Media DRM protection and you want to change the encoding parameters but not the DRM protection.
    MFSINK_WMDRMACTION_TRANSCRYPT = 3, // Transcrypt the content. Use this flag if the source content has DRM protection and you want to change the DRM protection; for example, if you want to convert from Windows Media DRM version 1 to Windows Media DRM version 7 or later.
    MFSINK_WMDRMACTION_LAST       = 3   // Reserved. Do not use.
  );
  {$EXTERNALSYM _MFSINK_WMDRMACTION}
  MFSINK_WMDRMACTION = _MFSINK_WMDRMACTION;
  {$EXTERNALSYM MFSINK_WMDRMACTION}

type

  // Forward Interface Declarations

  IMFASFProfile = interface;
  IMFASFStreamConfig = interface;
  IMFASFMutualExclusion = interface;
  IMFASFStreamPrioritization = interface;


  // INTERFACES ////////////////////////////////////////////////////////////////

  // Interface IMFASFContentInfo
  // ===========================
  // Provides methods to work with the header section of files conforming to the Advanced Systems Format (ASF) specification.
  // The ASF ContentInfo Object exposes this interface.
  // To create the get a pointer to the IMFASFContentInfo interface, call MFCreateASFContentInfo.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFContentInfo);'}
  {$EXTERNALSYM IMFASFContentInfo}
  IMFASFContentInfo = interface(IUnknown)
  ['{B1DCA5CD-D5DA-4451-8E9E-DB5C59914EAD}']

    function GetHeaderSize(pIStartOfContent: IMFMediaBuffer;
                           out cbHeaderSize: QWORD): HResult; stdcall;

    function ParseHeader(pIHeaderBuffer: IMFMediaBuffer;
                         const cbOffsetWithinHeader: QWORD): HResult; stdcall;

    function GenerateHeader(var pIHeader: IMFMediaBuffer;
                            out pcbHeader: DWORD): HResult; stdcall;

    function GetProfile(out ppIProfile: IMFASFProfile): HResult; stdcall;

    function SetProfile(pIProfile: IMFASFProfile): HResult; stdcall;

    function GeneratePresentationDescriptor(out ppIPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;

    function GetEncodingConfigurationPropertyStore(const wStreamNumber: WORD;
                                                   out ppIStore: IPropertyStore): HResult; stdcall;
  end;
  IID_IMFASFContentInfo = IMFASFContentInfo;
  {$EXTERNALSYM IID_IMFASFContentInfo}


  // Interface IMFASFProfile
  // =======================
  // Manages an Advanced Systems Format (ASF) profile.
  // A profile is a collection of information that describes the configuration of streams that will be
  // included in an ASF file. Information about the relationships between streams is also included in the profile.
  // An IMFASFProfile interface exists for every ASF profile object.
  // To create an ASF profile object, call MFCreateASFProfile or MFCreateASFProfileFromPresentationDescriptor.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFProfile);'}
  {$EXTERNALSYM IMFASFProfile}
  IMFASFProfile = interface(IUnknown)
  ['{D267BF6A-028B-4e0d-903D-43F0EF82D0D4}']

    function GetStreamCount(out pcStreams: DWORD): HResult; stdcall;

    function GetStream(const dwStreamIndex: DWORD;
                       out pwStreamNumber: WORD;
                       out ppIStream: IMFASFStreamConfig): HResult; stdcall;

    function GetStreamByNumber(const wStreamNumber: WORD;
                               out ppIStream: IMFASFStreamConfig): HResult; stdcall;

    function SetStream(pIStream: IMFASFStreamConfig): HResult; stdcall;

    function RemoveStream(const wStreamNumber: WORD): HResult; stdcall;

    function CreateStream(pIMediaType: IMFMediaType;
                          out ppIStream: IMFASFStreamConfig): HResult; stdcall;

    function GetMutualExclusionCount(out pcMutexs: DWORD): HResult; stdcall;

    function GetMutualExclusion(const dwMutexIndex: DWORD;
                                out ppIMutex: IMFASFMutualExclusion): HResult; stdcall;

    function AddMutualExclusion(pIMutex: IMFASFMutualExclusion): HResult; stdcall;

    function RemoveMutualExclusion(dwMutexIndex: DWORD): HResult; stdcall;

    function CreateMutualExclusion(out ppIMutex: IMFASFMutualExclusion): HResult; stdcall;

    function GetStreamPrioritization(out ppIStreamPrioritization: IMFASFStreamPrioritization): HResult; stdcall;

    function AddStreamPrioritization(pIStreamPrioritization: IMFASFStreamPrioritization): HResult; stdcall;

    function RemoveStreamPrioritization(): HResult; stdcall;

    function CreateStreamPrioritization(out ppIStreamPrioritization: IMFASFStreamPrioritization): HResult; stdcall;

    function Clone(out ppIProfile: IMFASFProfile): HResult; stdcall;

  end;
  IID_IMFASFProfile = IMFASFProfile;
  {$EXTERNALSYM IID_IMFASFProfile}


  // Interface IMFASFStreamConfig
  // ============================
  // Configures the settings of a stream in an ASF file.
  // The ASF stream configuration object exposes this interface.
  // To obtain a pointer to this interface, call the IMFASFProfile.CreateStream method.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFStreamConfig);'}
  {$EXTERNALSYM IMFASFStreamConfig}
  IMFASFStreamConfig = interface(IMFAttributes)
  ['{9E8AE8D2-DBBD-4200-9ACA-06E6DF484913}']

    function GetStreamType(out pguidStreamType: TGUID): HResult; stdcall;

    function GetStreamNumber(): WORD; stdcall;

    function SetStreamNumber(const wStreamNum: WORD): HResult; stdcall;

    function GetMediaType(out ppIMediaType: IMFMediaType): HResult; stdcall;

    function SetMediaType(pIMediaType: IMFMediaType): HResult; stdcall;

    function GetPayloadExtensionCount(out pcPayloadExtensions: WORD): HResult; stdcall;

    function GetPayloadExtension(wPayloadExtensionNumber: WORD;
                                 out pguidExtensionSystemID: TGUID;
                                 out pcbExtensionDataSize: WORD;
                                 out pbExtensionSystemInfo: PByte;
                                 var pcbExtensionSystemInfo: DWORD): HResult; stdcall;

    function AddPayloadExtension(const guidExtensionSystemID: TGUID;
                                 cbExtensionDataSize: WORD;
                                 pbExtensionSystemInfo: PByte;
                                 cbExtensionSystemInfo: DWORD): HResult; stdcall;

    function RemoveAllPayloadExtensions(): HResult; stdcall;

    function Clone(out ppIStreamConfig: IMFASFStreamConfig): HResult; stdcall;

  end;
  IID_IMFASFStreamConfig = IMFASFStreamConfig;
  {$EXTERNALSYM IID_IMFASFStreamConfig}


  // Interface IMFASFMutualExclusion
  // ===============================
  // Configures an Advanced Systems Format (ASF) mutual exclusion object,
  // which manages information about a group of streams in an ASF profile that are mutually exclusive.
  // When streams or groups of streams are mutually exclusive, only one of them is read at a time, they are not read concurrently.
  // A common example of mutual exclusion is a set of streams that each include the same content encoded at a different bit rate.
  // The stream that is used is determined by the available bandwidth to the reader.
  // An IMFASFMutualExclusion interface exists for every ASF mutual exclusion object.
  // A pointer to this interface is obtained when you create the object using the IMFASFProfile.CreateMutualExclusion method.
  // NOTE: An ASF profile object can support multiple mutual exclusions. Each must be configured using a separate ASF mutual exclusion object.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFMutualExclusion);'}
  {$EXTERNALSYM IMFASFMutualExclusion}
  IMFASFMutualExclusion = interface(IUnknown)
  ['{9E8AE8D2-DBBD-4200-9ACA-06E6DF484913}']

    function GetType(out pguidType: TGUID): HResult; stdcall;

    function SetType(const guidType: REFGUID): HResult; stdcall;

    function GetRecordCount(out pdwRecordCount: DWORD): HResult; stdcall;

    function GetStreamsForRecord(dwRecordNumber: DWORD;
                                 out pwStreamNumArray: WORD;
                                 var pcStreams: DWORD): HResult; stdcall;

    function AddStreamForRecord(dwRecordNumber: DWORD;
                                wStreamNumber: WORD): HResult; stdcall;

    function RemoveStreamFromRecord(dwRecordNumber: DWORD;
                                    wStreamNumber: WORD): HResult; stdcall;

    function RemoveRecord(dwRecordNumber: DWORD): HResult; stdcall;

    function AddRecord(out pdwRecordNumber: DWORD): HResult; stdcall;

    function Clone(out ppIMutex: IMFASFMutualExclusion): HResult; stdcall;

  end;
  IID_IMFASFMutualExclusion = IMFASFMutualExclusion;
  {$EXTERNALSYM IID_IMFASFMutualExclusion}


  // Interface IMFASFStreamPrioritization (not implemented)
  // ======================================================
  // Manages information about the relative priorities of a group of streams in an Advanced Systems Format (ASF) profile.
  // This interface manages information about the relative priorities of a group of streams in an ASF profile.
  // Priority is used in streaming to determine which streams should be dropped first when available bandwidth decreases.
  // The ASF stream prioritization object exposes this interface.
  // The stream prioritization object maintains a list of stream numbers in priority order.
  // The methods of this interface manipulate and interrogate that list.
  // To obtain a pointer to this interface, call the IMFASFProfile.CreateStreamPrioritization method.
  // NOTE: Date 6/4/2012: This interface is not implemented.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFStreamPrioritization);'}
  {$EXTERNALSYM IMFASFStreamPrioritization}
  IMFASFStreamPrioritization = interface(IUnknown)
  ['{699bdc27-bbaf-49ff-8e38-9c39c9b5e088}']

    function GetStreamCount(out pdwStreamCount: DWORD): HResult; stdcall;

    function GetStream(dwStreamIndex: DWORD;
                       out pwStreamNumber: WORD;
                       out pwStreamFlags: WORD): HResult; stdcall;

    function AddStream(wStreamNumber: WORD;
                       wStreamFlags: WORD): HResult; stdcall;

    function RemoveStream(dwStreamIndex: DWORD): HResult; stdcall;

    function Clone(out ppIStreamPrioritization: IMFASFStreamPrioritization): HResult; stdcall;

  end;
  IID_IMFASFStreamPrioritization = IMFASFStreamPrioritization;
  {$EXTERNALSYM IID_IMFASFStreamPrioritization}


  // Interface IMFASFSplitter
  // ========================
  // Provides methods to read data from an Advanced Systems Format (ASF) file.
  // The ASF splitter object exposes this interface.
  // To create the ASF splitter, MFCreateASFSplitter.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFSplitter);'}
  {$EXTERNALSYM IMFASFSplitter}
  IMFASFSplitter = interface(IUnknown)
  ['{12558295-E399-11D5-BC2A-00B0D0F3F4AB}']

    function Initialize(pIContentInfo: IMFASFContentInfo): HResult; stdcall;

    function SetFlags(const dwFlags: DWORD): HResult; stdcall;

    function GetFlags(pdwFlags: DWORD): HResult; stdcall;

    function SelectStreams(pwStreamNumbers: WORD;
                           wNumStreams: WORD): HResult; stdcall;

    function GetSelectedStreams(out pwStreamNumbers: WORD;
                                var pwNumStreams: WORD): HResult; stdcall;

    function ParseData(pIBuffer: IMFMediaBuffer;
                       cbBufferOffset: DWORD;
                       cbLength: DWORD): HResult; stdcall;

    function GetNextSample(out pdwStatusFlags: DWORD;
                           out pwStreamNumber: WORD;
                           out ppISample: IMFSample): HResult; stdcall;

    function Flush(): HResult; stdcall;

    function GetLastSendTime(pdwLastSendTime: DWORD): HResult; stdcall;

  end;
  IID_IMFASFSplitter = IMFASFSplitter;
  {$EXTERNALSYM IID_IMFASFSplitter}


  // Interface IMFASFMultiplexer
  // ===========================
  // Provides methods to create Advanced Systems Format (ASF) data packets.
  // The methods of this interface process input samples into the packets that make up an ASF data section.
  // The ASF multiplexer exposes this interface.
  // To create the ASF multiplexer, call MFCreateASFMultiplexer defined in this unit.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFMultiplexer);'}
  {$EXTERNALSYM IMFASFMultiplexer}
  IMFASFMultiplexer = interface(IUnknown)
  ['{57BDD80A-9B38-4838-B737-C58F670D7D4F}']

    function Initialize(pIContentInfo: IMFASFContentInfo): HResult; stdcall;

    function SetFlags(dwFlags: DWORD): HResult; stdcall;

    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;

    function ProcessSample(wStreamNumber: WORD;
                           pISample: IMFSample;
                           hnsTimestampAdjust: LONGLONG): HResult; stdcall;

    function GetNextPacket(out pdwStatusFlags: DWORD;
                           out ppIPacket: IMFSample): HResult; stdcall;

    function Flush(): HResult; stdcall;

    function _End(var pIContentInfo: IMFASFContentInfo): HResult; stdcall;

    function GetStatistics(wStreamNumber: WORD;
                           out pMuxStats: ASF_MUX_STATISTICS): HResult; stdcall;

    function SetSyncTolerance(msSyncTolerance: DWORD): HResult; stdcall;

  end;
  IID_IMFASFMultiplexer = IMFASFMultiplexer;
  {$EXTERNALSYM IID_IMFASFMultiplexer}


  // Interface IMFASFIndexer
  // =======================
  // Provides methods to work with indexes in Systems Format (ASF) files.
  // The ASF indexer object exposes this interface.
  // To create the ASF indexer, call MFCreateASFIndexer defined in this unit
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFIndexer);'}
  {$EXTERNALSYM IMFASFIndexer}
  IMFASFIndexer = interface(IUnknown)
  ['{53590F48-DC3B-4297-813F-787761AD7B3E}']

    function SetFlags(dwFlags: DWORD): HResult; stdcall;

    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;

    function Initialize(pIContentInfo: IMFASFContentInfo): HResult; stdcall;

    function GetIndexPosition(pIContentInfo: IMFASFContentInfo;
                              out pcbIndexOffset: QWORD): HResult; stdcall;

    function SetIndexByteStreams(ppIByteStreams: PIMFByteStream;
                                 const cByteStreams: DWORD): HResult; stdcall;

    function GetIndexByteStreamCount(out pcByteStreams: DWORD): HResult; stdcall;

    function GetIndexStatus(pIndexIdentifier: ASF_INDEX_IDENTIFIER;
                            out pfIsIndexed: BOOL;
                            out pbIndexDescriptor: PByte;
                            var pcbIndexDescriptor: DWORD): HResult; stdcall;

    function SetIndexStatus(pbIndexDescriptor: PByte;
                            cbIndexDescriptor: DWORD;
                            fGenerateIndex: BOOL): HResult; stdcall;

    function GetSeekPositionForValue(pvarValue: PROPVARIANT;
                                     pIndexIdentifier: ASF_INDEX_IDENTIFIER;
                                     out pcbOffsetWithinData: QWORD;
                                     out phnsApproxTime: MFTIME;
                                     out pdwPayloadNumberOfStreamWithinPacket: DWORD): HResult; stdcall;

    function GenerateIndexEntries(pIASFPacketSample: IMFSample): HResult; stdcall;

    function CommitIndex(pIContentInfo: IMFASFContentInfo): HResult; stdcall;

    function GetIndexWriteSpace(out pcbIndexWriteSpace: QWORD): HResult; stdcall;

    function GetCompletedIndex(pIIndexBuffer: IMFMediaBuffer;
                               cbOffsetWithinIndex: QWORD): HResult; stdcall;

  end;
  IID_IMFASFIndexer = IMFASFIndexer;
  {$EXTERNALSYM IID_IMFASFIndexer}


  // Interface IMFASFStreamSelector
  // ==============================
  // Selects streams in an Advanced Systems Format (ASF) file, based on the mutual exclusion information in the ASF header.
  // The ASF stream selector object exposes this interface.
  // To create the ASF stream selector, call MFCreateASFStreamSelector defined in this unit
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFASFStreamSelector);'}
  {$EXTERNALSYM IMFASFStreamSelector}
  IMFASFStreamSelector = interface(IUnknown)
  ['{d01bad4a-4fa0-4a60-9349-c27e62da9d41}']

    function GetStreamCount(out pcStreams: DWORD): HResult; stdcall;

    function GetOutputCount(out pcOutputs: DWORD): HResult; stdcall;

    function GetOutputStreamCount(dwOutputNum: DWORD;
                                  out pcStreams: DWORD): HResult; stdcall;

    function GetOutputStreamNumbers(dwOutputNum: DWORD;
                                    out rgwStreamNumbers: WORD): HResult; stdcall;

    function GetOutputFromStream(wStreamNum: WORD;
                                 out pdwOutput: DWORD): HResult; stdcall;

    function GetOutputOverride(dwOutputNum: DWORD;
                               out pSelection: ASF_SELECTION_STATUS): HResult; stdcall;

    function SetOutputOverride(dwOutputNum: DWORD;
                               Selection: ASF_SELECTION_STATUS): HResult; stdcall;

    function GetOutputMutexCount(dwOutputNum: DWORD;
                                 out pcMutexes: DWORD): HResult; stdcall;

    function GetOutputMutex(dwOutputNum: DWORD;
                            dwMutexNum: DWORD;
                            out ppMutex: IUnknown): HResult; stdcall;

    function SetOutputMutexSelection(dwOutputNum: DWORD;
                                     dwMutexNum: DWORD;
                                     wSelectedRecord: WORD): HResult; stdcall;

    function GetBandwidthStepCount(out pcStepCount: DWORD): HResult; stdcall;

    function GetBandwidthStep(dwStepNum: DWORD;
                              out pdwBitrate: DWORD;
                              out rgwStreamNumbers: WORD;
                              out rgSelections: ASF_SELECTION_STATUS): HResult; stdcall;

    function BitrateToStepNumber(dwBitrate: DWORD;
                                 out pdwStepNum: DWORD): HResult; stdcall;

    function SetStreamSelectorFlags(dwStreamSelectorFlags: DWORD): HResult; stdcall;

  end;
  IID_IMFASFStreamSelector = IMFASFStreamSelector;
  {$EXTERNALSYM IID_IMFASFStreamSelector}


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // Interface IMFDRMNetHelper
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDRMNetHelper);'}
  {$EXTERNALSYM IMFDRMNetHelper}
  IMFDRMNetHelper = interface(IUnknown)
  ['{3D1FF0EA-679A-4190-8D46-7FA69E8C7E15}']

    function ProcessLicenseRequest(pLicenseRequest: PByte;
                                   cbLicenseRequest: DWORD;
                                   out ppLicenseResponse: PByte;
                                   out pcbLicenseResponse: DWORD;
                                   out pbstrKID: BSTR): HResult; stdcall;

    function GetChainedLicenseResponse(out ppLicenseResponse: PByte;
                                       out pcbLicenseResponse: DWORD): HResult; stdcall;

  end;
  IID_IMFDRMNetHelper = IMFDRMNetHelper;
  {$EXTERNALSYM IID_IMFDRMNetHelper}
  //#endif (WINVER >= _WIN32_WINNT_WIN7)


  function MFCreateASFContentInfo(out ppIContentInfo: IMFASFContentInfo): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFContentInfo}

  function MFCreateASFProfile(out ppIProfile: IMFASFProfile): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFProfile}

  function MFCreateASFProfileFromPresentationDescriptor(pIPD: IMFPresentationDescriptor;
                                                        out ppIProfile: IMFASFProfile): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFProfileFromPresentationDescriptor}

  function MFCreatePresentationDescriptorFromASFProfile(pIProfile: IMFASFProfile;
                                                        out ppIPD: IMFPresentationDescriptor): HResult; stdcall;
  {$EXTERNALSYM MFCreatePresentationDescriptorFromASFProfile}

  function MFCreateASFMultiplexer(out ppIMultiplexer: IMFASFMultiplexer): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFMultiplexer}

  function MFCreateASFIndexer(out ppIIndexer: IMFASFIndexer): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFIndexer}

  function MFCreateASFIndexerByteStream(pIContentByteStream: IMFByteStream;
                                        cbIndexStartOffset: QWORD;
                                        out pIIndexByteStream: IMFByteStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFIndexerByteStream}

  function MFCreateASFStreamSelector(pIASFProfile: IMFASFProfile;
                                     out ppSelector: IMFASFStreamSelector): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFStreamSelector}

  function MFCreateASFMediaSink(var pIByteStream: IMFByteStream;
                                var ppIMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFMediaSink}

  function MFCreateASFMediaSinkActivate(pwszFileName: PWideChar;
                                        var pContentInfo: IMFASFContentInfo;
                                        var ppIActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFMediaSinkActivate}

  function MFCreateWMVEncoderActivate(var pMediaType: IMFMediaType;
                                      var pEncodingConfigurationProperties: IPropertyStore;
                                      var ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateWMVEncoderActivate}

  function MFCreateWMAEncoderActivate(var pMediaType: IMFMediaType;
                                      var pEncodingConfigurationProperties: IPropertyStore;
                                      var ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateWMAEncoderActivate}

  function MFCreateASFStreamingMediaSink(var pIByteStream: IMFByteStream;
                                         var ppIMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFStreamingMediaSink}

  function MFCreateASFStreamingMediaSinkActivate(var pByteStreamActivate: IMFActivate;
                                                 var pContentInfo: IMFASFContentInfo;
                                                 var ppIActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFStreamingMediaSinkActivate}

  function MFCreateASFSplitter(out ppISplitter: IMFASFSplitter): HResult; stdcall;
  {$EXTERNALSYM MFCreateASFSplitter}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  WmContainerLib = 'Mf.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateASFContentInfo; external WmContainerLib name 'MFCreateASFContentInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFProfile; external WmContainerLib name 'MFCreateASFProfile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFProfileFromPresentationDescriptor; external WmContainerLib name 'MFCreateASFProfileFromPresentationDescriptor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreatePresentationDescriptorFromASFProfile; external WmContainerLib name 'MFCreatePresentationDescriptorFromASFProfile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFMultiplexer; external WmContainerLib name 'MFCreateASFMultiplexer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFIndexer; external WmContainerLib name 'MFCreateASFIndexer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFIndexerByteStream; external WmContainerLib name 'MFCreateASFIndexerByteStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFStreamSelector; external WmContainerLib name 'MFCreateASFStreamSelector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFMediaSink; external WmContainerLib name 'MFCreateASFMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFMediaSinkActivate; external WmContainerLib name 'MFCreateASFMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateWMVEncoderActivate; external WmContainerLib name 'MFCreateWMVEncoderActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateWMAEncoderActivate; external WmContainerLib name 'MFCreateWMAEncoderActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFStreamingMediaSink; external WmContainerLib name 'MFCreateASFStreamingMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFStreamingMediaSinkActivate; external WmContainerLib name 'MFCreateASFStreamingMediaSinkActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateASFSplitter; external WmContainerLib name 'MFCreateASFSplitter' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

//Implement Additional functions here.

end.

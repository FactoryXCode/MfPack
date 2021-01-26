// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfIdl.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.1
// Description: Media Foundation basic control-layer interfaces.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (TopPlay)
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
// Source: Mfidl.h
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
unit WinApi.MediaFoundationApi.MfIdl;

  {$HPPEMIT '#include "Mfidl.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjIdlbase,
  {MfPack}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  MF_SOURCE_PRESENTATION_PROVIDER_SERVICE   : TGUID = '{e002aadc-f4af-4ee5-9847-053edf840426}';
  {$EXTERNALSYM MF_SOURCE_PRESENTATION_PROVIDER_SERVICE}
  MF_WVC1_PROG_SINGLE_SLICE_CONTENT         : TGUID = '{67EC2559-0F2F-4420-A4DD-2F8EE7A5738B}';
  {$EXTERNALSYM MF_WVC1_PROG_SINGLE_SLICE_CONTENT}
  MF_PROGRESSIVE_CODING_CONTENT             : TGUID = '{8F020EEA-1508-471F-9DA6-507D7CFA40DB}';
  {$EXTERNALSYM MF_PROGRESSIVE_CODING_CONTENT}
  MF_NALU_LENGTH_SET                        : TGUID = '{A7911D53-12A4-4965-AE70-6EADD6FF0551}';
  {$EXTERNALSYM MF_NALU_LENGTH_SET}
  MF_NALU_LENGTH_INFORMATION                : TGUID = '{19124E7C-AD4B-465F-BB18-20186287B6AF}';
  {$EXTERNALSYM MF_NALU_LENGTH_INFORMATION}
  MF_USER_DATA_PAYLOAD                      : TGUID = '{d1d4985d-dc92-457a-b3a0-651a33a31047}';
  {$EXTERNALSYM MF_USER_DATA_PAYLOAD}

  MF_MPEG4SINK_SPSPPS_PASSTHROUGH           : TGUID = '{5601a134-2005-4ad2-b37d-22a6c554deb2}';
  {$EXTERNALSYM MF_MPEG4SINK_SPSPPS_PASSTHROUGH}
  MF_MPEG4SINK_MOOV_BEFORE_MDAT             : TGUID = '{f672e3ac-e1e6-4f10-b5ec-5f3b30828816}';
  {$EXTERNALSYM MF_MPEG4SINK_MOOV_BEFORE_MDAT}
  MF_MPEG4SINK_MINIMUM_PROPERTIES_SIZE      : TGUID = '{dca1ed52-450e-4a22-8c62-4ed452f7a187}';
  {$EXTERNALSYM MF_MPEG4SINK_MINIMUM_PROPERTIES_SIZE}
  MF_MPEG4SINK_MIN_FRAGMENT_DURATION        : TGUID = '{a30b570c-8efd-45e8-94fe-27c84b5bdff6}';
  {$EXTERNALSYM MF_MPEG4SINK_MIN_FRAGMENT_DURATION}
  MF_MPEG4SINK_MAX_CODED_SEQUENCES_PER_FRAGMENT  : TGUID = '{fc1b3bd6-692d-4ce5-9299-738aa5463e9a}';
  {$EXTERNALSYM MF_MPEG4SINK_MAX_CODED_SEQUENCES_PER_FRAGMENT}


  MF_SESSION_TOPOLOADER                     : TGUID = '{1e83d482-1f1c-4571-0845-88f4b2181f71}';
  {$EXTERNALSYM MF_SESSION_TOPOLOADER}
  MF_SESSION_GLOBAL_TIME                    : TGUID = '{1e83d482-1f1c-4571-0845-88f4b2181f72}';
  {$EXTERNALSYM MF_SESSION_GLOBAL_TIME}
  MF_SESSION_QUALITY_MANAGER                : TGUID = '{1e83d482-1f1c-4571-0845-88f4b2181f73}';
  {$EXTERNALSYM MF_SESSION_QUALITY_MANAGER}
  MF_SESSION_CONTENT_PROTECTION_MANAGER     : TGUID = '{1e83d482-1f1c-4571-0845-88f4b2181f74}';
  {$EXTERNALSYM MF_SESSION_CONTENT_PROTECTION_MANAGER}
  MF_SESSION_SERVER_CONTEXT                 : TGUID = '{afe5b291-50fa-46e8-b9be-0c0c3ce4b3a5}';
  {$EXTERNALSYM MF_SESSION_SERVER_CONTEXT}
  MF_SESSION_REMOTE_SOURCE_MODE             : TGUID = '{f4033ef4-9bb3-4378-941f-85a0856bc244}';
  {$EXTERNALSYM MF_SESSION_REMOTE_SOURCE_MODE}
  MF_SESSION_APPROX_EVENT_OCCURRENCE_TIME   : TGUID = '{190e852f-6238-42d1-b5af-69ea338ef850}';
  {$EXTERNALSYM MF_SESSION_APPROX_EVENT_OCCURRENCE_TIME}
  MF_PMP_SERVER_CONTEXT                     : TGUID = '{2f00c910-d2cf-4278-8b6a-d077fac3a25f}';
  {$EXTERNALSYM MF_PMP_SERVER_CONTEXT}


  // enum __MIDL___MIDL_itf_mfidl_0000_0001_0001
  //
  // Source Resolver Flags
  // Defines the behavior of the source resolver.
  // These flags are also used by scheme and byte stream handlers.
type
  PMF_RESOLUTION = ^__MIDL___MIDL_itf_mfidl_0000_0001_0001;
  __MIDL___MIDL_itf_mfidl_0000_0001_0001 = DWord;
  MF_RESOLUTION = __MIDL___MIDL_itf_mfidl_0000_0001_0001;
const
  MF_RESOLUTION_MEDIASOURCE = MF_RESOLUTION($1);
  {$EXTERNALSYM MF_RESOLUTION_MEDIASOURCE}
  MF_RESOLUTION_BYTESTREAM  = MF_RESOLUTION($2);
  {$EXTERNALSYM MF_RESOLUTION_BYTESTREAM}
  MF_RESOLUTION_CONTENT_DOES_NOT_HAVE_TO_MATCH_EXTENSION_OR_MIME_TYPE  = MF_RESOLUTION($10);
  {$EXTERNALSYM MF_RESOLUTION_CONTENT_DOES_NOT_HAVE_TO_MATCH_EXTENSION_OR_MIME_TYPE}
  MF_RESOLUTION_KEEP_BYTE_STREAM_ALIVE_ON_FAIL  = MF_RESOLUTION($20);
  {$EXTERNALSYM MF_RESOLUTION_KEEP_BYTE_STREAM_ALIVE_ON_FAIL}
  MF_RESOLUTION_DISABLE_LOCAL_PLUGINS  = MF_RESOLUTION($40);
  {$EXTERNALSYM MF_RESOLUTION_DISABLE_LOCAL_PLUGINS}
  MF_RESOLUTION_PLUGIN_CONTROL_POLICY_APPROVED_ONLY  = MF_RESOLUTION($80);
  {$EXTERNALSYM MF_RESOLUTION_PLUGIN_CONTROL_POLICY_APPROVED_ONLY}
  MF_RESOLUTION_PLUGIN_CONTROL_POLICY_WEB_ONLY  = MF_RESOLUTION($100);
  {$EXTERNALSYM MF_RESOLUTION_PLUGIN_CONTROL_POLICY_WEB_ONLY}
  MF_RESOLUTION_PLUGIN_CONTROL_POLICY_WEB_ONLY_EDGEMODE	= MF_RESOLUTION($200);
  {$EXTERNALSYM MF_RESOLUTION_PLUGIN_CONTROL_POLICY_WEB_ONLY_EDGEMODE}
  MF_RESOLUTION_READ = MF_RESOLUTION($10000);
  {$EXTERNALSYM MF_RESOLUTION_READ}
  MF_RESOLUTION_WRITE = MF_RESOLUTION($20000);
  {$EXTERNALSYM MF_RESOLUTION_WRITE}


  MFPKEY_SourceOpenMonitor          : PROPERTYKEY = (fmtid: (D1: $074d4637;
                                                     D2: $b5ae;
                                                     D3: $465d;
                                                     D4: ($af, $17, $1a, $53, $8d, $28, $59, $dd));
                                                     pid: $02);
  {$EXTERNALSYM MFPKEY_SourceOpenMonitor}

  MFPKEY_ASFMediaSource_ApproxSeek  : PROPERTYKEY = (fmtid: (D1: $b4cd270f;
                                                     D2: $244d;
                                                     D3: $4969;
                                                     D4: ($bb, $92, $3f, $0f, $b8, $31, $6f, $10));
                                                     pid: $01);
  {$EXTERNALSYM MFPKEY_ASFMediaSource_ApproxSeek}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MFPKEY_ASFMediaSource_IterativeSeekIfNoIndex: PROPERTYKEY = (fmtid: (D1: $170b65dc;
                                                                       D2: $4a4e;
                                                                       D3: $407a;
                                                                       D4: ($ac, $22, $57, $7f, $50, $e4, $a3, $7c));
                                                                       pid: $01);
  {$EXTERNALSYM MFPKEY_ASFMediaSource_IterativeSeekIfNoIndex}

  MFPKEY_ASFMediaSource_IterativeSeek_Max_Count: PROPERTYKEY = (fmtid: (D1: $170b65dc;
                                                                        D2: $4a4e;
                                                                        D3: $407a;
                                                                        D4: ($ac, $22, $57, $7f, $50, $e4, $a3, $7c));
                                                                        pid: $02);
  {$EXTERNALSYM MFPKEY_ASFMediaSource_IterativeSeek_Max_Count}

  MFPKEY_ASFMediaSource_IterativeSeek_Tolerance_In_MilliSecond: PROPERTYKEY = (fmtid: (D1: $170b65dc;
                                                                                       D2: $4a4e;
                                                                                       D3: $407a;
                                                                                       D4: ($ac, $22, $57, $7f, $50, $e4, $a3, $7c));
                                                                                       pid: $03);
   {$EXTERNALSYM MFPKEY_ASFMediaSource_IterativeSeek_Tolerance_In_MilliSecond}

  MFPKEY_Content_DLNA_Profile_ID: PROPERTYKEY = (fmtid: (D1: $cfa31b45;
                                                         D2: $525d;
                                                         D3: $4998;
                                                         D4: ($bb, $44, $3f, $7d, $81, $54, $2f, $a4));
                                                         pid: $01);
  {$EXTERNALSYM MFPKEY_Content_DLNA_Profile_ID}

  MFPKEY_MediaSource_DisableReadAhead : PROPERTYKEY = (fmtid: (D1: $26366c14;
                                                               D2: $c5bf;
                                                               D3: $4c76;
                                                               D4: ($88, $7b, $9f, $17, $54, $db, $5f, $9));
                                                               pid: $01);
  {$EXTERNALSYM MFPKEY_MediaSource_DisableReadAhead}

  MFPKEY_SBESourceMode :  PROPERTYKEY = (fmtid: ( D1: $3fae10bb;
                                                  D2: $f859;
                                                  D3: $4192;
                                                  D4: ($b5, $62, $18, $68, $d3, $da, $3a, $02));
                                                  pid: 01);
  {$EXTERNALSYM MFPKEY_SBESourceMode}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

//#if (WINVER >= _WIN32_WINNT_WIN8)

  MFPKEY_PMP_Creation_Callback                  :  PROPERTYKEY = (fmtid: (D1: $28bb4de2;
                                                                          D2: $26a2;
                                                                          D3: $4870;
                                                                          D4: ($b7, $20, $d2, $6b, $be, $b1, $49, $42));
                                                                          pid: 01);
  {$EXTERNALSYM MFPKEY_PMP_Creation_Callback}

  MFPKEY_HTTP_ByteStream_Enable_Urlmon          :  PROPERTYKEY = (fmtid: (D1: $eda8afdf;
                                                                          D2: $c171;
                                                                          D3: $417f;
                                                                          D4: ($8d, $17, $2e, $09, $18, $30, $32, $92));
                                                                          pid: 01);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Enable_Urlmon}

  MFPKEY_HTTP_ByteStream_Urlmon_Bind_Flags      :  PROPERTYKEY = (fmtid: (D1: $eda8afdf;
                                                                          D2: $c171;
                                                                          D3: $417f;
                                                                          D4: ($8d, $17, $2e, $09, $18, $30, $32, $92));
                                                                          pid: 02);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Urlmon_Bind_Flags}

  MFPKEY_HTTP_ByteStream_Urlmon_Security_Id     :  PROPERTYKEY = (fmtid: (D1: $eda8afdf;
                                                                          D2: $c171;
                                                                          D3: $417f;
                                                                          D4: ($8d, $17, $2e, $09, $18, $30, $32, $92));
                                                                          pid: 03);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Urlmon_Security_Id}

  MFPKEY_HTTP_ByteStream_Urlmon_Window          :  PROPERTYKEY = (fmtid: (D1: $eda8afdf;
                                                                          D2: $c171;
                                                                          D3: $417f;
                                                                          D4: ($8d, $17, $2e, $09, $18, $30, $32, $92));
                                                                          pid: 04);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Urlmon_Window}

  MFPKEY_HTTP_ByteStream_Urlmon_Callback_QueryService :  PROPERTYKEY = (fmtid: (D1: $eda8afdf;
                                                                                D2: $c171;
                                                                                D3: $417f;
                                                                                D4: ($8d, $17, $2e, $09, $18, $30, $32, $92));
                                                                                pid: 05);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Urlmon_Callback_QueryService}

  MFPKEY_MediaProtectionSystemId                :  PROPERTYKEY = (fmtid: (D1: $636b271d;
                                                                          D2: $ddc7;
                                                                          D3: $49e9;
                                                                          D4: ($a6, $c6, $47, $38, $59, $62, $e5, $bd));
                                                                          pid: 01);
  {$EXTERNALSYM MFPKEY_MediaProtectionSystemId}

  MFPKEY_MediaProtectionSystemContext           :  PROPERTYKEY = (fmtid: (D1: $636b271d;
                                                                          D2: $ddc7;
                                                                          D3: $49e9;
                                                                          D4: ($a6, $c6, $47, $38, $59, $62, $e5, $bd));
                                                                          pid: 02);
  {$EXTERNALSYM MFPKEY_MediaProtectionSystemContext}

  MFPKEY_MediaProtectionSystemIdMapping         :  PROPERTYKEY = (fmtid: (D1: $636b271d;
                                                                          D2: $ddc7;
                                                                          D3: $49e9;
                                                                          D4: ($a6, $c6, $47, $38, $59, $62, $e5, $bd));
                                                                          pid: 03);
  {$EXTERNALSYM MFPKEY_MediaProtectionSystemIdMapping}

  MFPKEY_MediaProtectionContainerGuid           :  PROPERTYKEY = (fmtid: (D1: $42af3d7c;
                                                                          D2: $00cf;
                                                                          D3: $4a0f;
                                                                          D4: ($81, $f0, $ad, $f5, $24, $a5, $a5, $b5));
                                                                          pid: 1);
  {$EXTERNALSYM MFPKEY_MediaProtectionContainerGuid}

  MFPKEY_MediaProtectionSystemContextsPerTrack  :  PROPERTYKEY = (fmtid: (D1: $4454b092;
                                                                          D2: $d3da;
                                                                          D3: $49b0;
                                                                          D4: ($84, $52, $68, $50, $c7, $db, $76, $4d));
                                                                          pid: 03);
  {$EXTERNALSYM MFPKEY_MediaProtectionSystemContextsPerTrack}

  MFPKEY_HTTP_ByteStream_Download_Mode          :  PROPERTYKEY = (fmtid: (D1: $817f11b7;
                                                                          D2: $a982;
                                                                          D3: $46ec;
                                                                          D4: ($a4, $49, $ef, $58, $ae, $d5, $3c, $a8));
                                                                          pid: 01);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Download_Mode}

  MFPKEY_HTTP_ByteStream_Caching_Mode           :	 PROPERTYKEY = (fmtid: (D1: $86a2403e;
                                                                          D2: $c78b;
                                                                          D3: $44d7;
                                                                          D4: ($8b, $c8, $ff, $72, $58, $11, $75, $08));
                                                                          pid: 01);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Caching_Mode}

  MFPKEY_HTTP_ByteStream_Cache_Limit            :	 PROPERTYKEY = (fmtid: (D1: $86a2403e;
                                                                          D2: $c78b;
                                                                          D3: $44d7;
                                                                          D4: ($8b, $c8, $ff, $72, $58, $11, $75, $08));
                                                                          pid: 02);
  {$EXTERNALSYM MFPKEY_HTTP_ByteStream_Cache_Limit}






//#endif // (WINVER >= _WIN32_WINNT_WIN8)

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MF_TIME_FORMAT_ENTRY_RELATIVE                 :  TGUID = '{4399f178-46d3-4504-afda-20d32e9ba360}';
  {$EXTERNALSYM MF_TIME_FORMAT_ENTRY_RELATIVE}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

//#if (WINVER >= _WIN32_WINNT_WIN8)
  MF_SOURCE_STREAM_SUPPORTS_HW_CONNECTION       :  TGUID = '{a38253aa-6314-42fd-a3ce-bb27b6859946}';
  {$EXTERNALSYM MF_SOURCE_STREAM_SUPPORTS_HW_CONNECTION}
  MF_STREAM_SINK_SUPPORTS_HW_CONNECTION         :  TGUID = '{9b465cbf-0597-4f9e-9f3c-b97eeef90359}';
  {$EXTERNALSYM MF_STREAM_SINK_SUPPORTS_HW_CONNECTION}
  MF_STREAM_SINK_SUPPORTS_ROTATION              :  TGUID = '{b3e96280-bd05-41a5-97ad-8a7fee24b912}';
  {$EXTERNALSYM MF_STREAM_SINK_SUPPORTS_ROTATION}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

  MEDIASINK_FIXED_STREAMS               = $00000001;
  {$EXTERNALSYM MEDIASINK_FIXED_STREAMS}
  MEDIASINK_CANNOT_MATCH_CLOCK          = $00000002;
  {$EXTERNALSYM MEDIASINK_CANNOT_MATCH_CLOCK}
  MEDIASINK_RATELESS                    = $00000004;
  {$EXTERNALSYM MEDIASINK_RATELESS}
  MEDIASINK_CLOCK_REQUIRED              = $00000008;
  {$EXTERNALSYM MEDIASINK_CLOCK_REQUIRED}
  MEDIASINK_CAN_PREROLL                 = $00000010;
  {$EXTERNALSYM MEDIASINK_CAN_PREROLL}
  MEDIASINK_REQUIRE_REFERENCE_MEDIATYPE = $00000020;
  {$EXTERNALSYM MEDIASINK_REQUIRE_REFERENCE_MEDIATYPE}


type
  MF_TRANSFER_VIDEO_FRAME_FLAGS = DWord;
  {$EXTERNALSYM MF_TRANSFER_VIDEO_FRAME_FLAGS}
const
  {$EXTERNALSYM MF_TRANSFER_VIDEO_FRAME_DEFAULT}
  MF_TRANSFER_VIDEO_FRAME_DEFAULT	   = MF_TRANSFER_VIDEO_FRAME_FLAGS(0);
  {$EXTERNALSYM MF_TRANSFER_VIDEO_FRAME_STRETCH}
  MF_TRANSFER_VIDEO_FRAME_STRETCH	   = MF_TRANSFER_VIDEO_FRAME_FLAGS(1);
  {$EXTERNALSYM MF_TRANSFER_VIDEO_FRAME_IGNORE_PAR}
  MF_TRANSFER_VIDEO_FRAME_IGNORE_PAR = MF_TRANSFER_VIDEO_FRAME_FLAGS(2);


const
  MF_SINK_VIDEO_PTS                              :	TGUID = '{2162bde7-421e-4b90-9b33-e58fbf1d58b6}';
  {$EXTERNALSYM MF_SINK_VIDEO_PTS}
  MF_SINK_VIDEO_NATIVE_WIDTH                     :	TGUID = '{e6d6a707-1505-4747-9b10-72d2d158cb3a}';
  {$EXTERNALSYM MF_SINK_VIDEO_NATIVE_WIDTH}
  MF_SINK_VIDEO_NATIVE_HEIGHT                    :	TGUID = '{f0ca6705-490c-43e8-941c-c0b3206b9a65}';
  {$EXTERNALSYM MF_SINK_VIDEO_NATIVE_HEIGHT}
  MF_SINK_VIDEO_DISPLAY_ASPECT_RATIO_NUMERATOR   :	TGUID = '{d0f33b22-b78a-4879-b455-f03ef3fa82cd}';
  {$EXTERNALSYM MF_SINK_VIDEO_DISPLAY_ASPECT_RATIO_NUMERATOR}
  MF_SINK_VIDEO_DISPLAY_ASPECT_RATIO_DENOMINATOR :	TGUID = '{6ea1eb97-1fe0-4f10-a6e4-1f4f661564e0}';
  {$EXTERNALSYM MF_SINK_VIDEO_DISPLAY_ASPECT_RATIO_DENOMINATOR}
  MF_BD_MVC_PLANE_OFFSET_METADATA                :	TGUID = '{62a654e4-b76c-4901-9823-2cb615d47318}';
  {$EXTERNALSYM MF_BD_MVC_PLANE_OFFSET_METADATA}
  MF_LUMA_KEY_ENABLE                             :	TGUID = '{7369820f-76de-43ca-9284-47b8f37e0649}';
  {$EXTERNALSYM MF_LUMA_KEY_ENABLE}
  MF_LUMA_KEY_LOWER                              :	TGUID = '{93d7b8d5-0b81-4715-aea0-8725871621e9}';
  {$EXTERNALSYM MF_LUMA_KEY_LOWER}
  MF_LUMA_KEY_UPPER                              :	TGUID = '{d09f39bb-4602-4c31-a706-a12171a5110a}';
  {$EXTERNALSYM MF_LUMA_KEY_UPPER}
  MF_USER_EXTENDED_ATTRIBUTES                    :	TGUID = '{c02abac6-feb2-4541-922f-920b43702722}';
  {$EXTERNALSYM MF_USER_EXTENDED_ATTRIBUTES}
  MF_INDEPENDENT_STILL_IMAGE                     :	TGUID = '{ea12af41-0710-42c9-a127-daa3e78483a5}';
  {$EXTERNALSYM MF_INDEPENDENT_STILL_IMAGE}



  MF_TOPOLOGY_PROJECTSTART                      : TGUID = '{7ed3f802-86bb-4b3f-b7e4-7cb43afd4b80}';
  {$EXTERNALSYM MF_TOPOLOGY_PROJECTSTART}
  MF_TOPOLOGY_PROJECTSTOP                       : TGUID = '{7ed3f803-86bb-4b3f-b7e4-7cb43afd4b80}';
  {$EXTERNALSYM MF_TOPOLOGY_PROJECTSTOP}
  MF_TOPOLOGY_NO_MARKIN_MARKOUT                 : TGUID = '{7ed3f804-86bb-4b3f-b7e4-7cb43afd4b80}';
  {$EXTERNALSYM MF_TOPOLOGY_NO_MARKIN_MARKOUT}

  MF_TOPOLOGY_DXVA_MODE                         : TGUID = '{1e8d34f6-f5ab-4e23-bb88-874aa3a1a74d}';
  {$EXTERNALSYM MF_TOPOLOGY_DXVA_MODE}
  MF_TOPOLOGY_ENABLE_XVP_FOR_PLAYBACK           : TGUID = '{1967731f-cd78-42fc-b026-0992a56e5693}';
  {$EXTERNALSYM MF_TOPOLOGY_ENABLE_XVP_FOR_PLAYBACK}
  MF_TOPOLOGY_STATIC_PLAYBACK_OPTIMIZATIONS     : TGUID = '{b86cac42-f5ab-4e23-bb88-874aa3a1a74d}';
  {$EXTERNALSYM MF_TOPOLOGY_STATIC_PLAYBACK_OPTIMIZATIONS}
  MF_TOPOLOGY_PLAYBACK_MAX_DIMS                 : TGUID = '{5715cf19-5768-44aa-ad6e-8721f1b0f9bb}';
  {$EXTERNALSYM MF_TOPOLOGY_PLAYBACK_MAX_DIMS}

  MF_TOPOLOGY_HARDWARE_MODE                     : TGUID = '{d2d362fd-4e4f-4191-a579-c618b66706af}';
  {$EXTERNALSYM MF_TOPOLOGY_HARDWARE_MODE}
  MF_TOPOLOGY_PLAYBACK_FRAMERATE                : TGUID = '{c164737a-c2b1-4553-83bb-5a526072448f}';
  {$EXTERNALSYM MF_TOPOLOGY_PLAYBACK_FRAMERATE}
  MF_TOPOLOGY_DYNAMIC_CHANGE_NOT_ALLOWED        : TGUID = '{d529950b-d484-4527-a9cd-b1909532b5b0}';
  {$EXTERNALSYM MF_TOPOLOGY_DYNAMIC_CHANGE_NOT_ALLOWED}
  MF_TOPOLOGY_ENUMERATE_SOURCE_TYPES            : TGUID = '{6248c36d-5d0b-4f40-a0bb-b0b305f77698}';
  {$EXTERNALSYM MF_TOPOLOGY_ENUMERATE_SOURCE_TYPES}
  MF_TOPOLOGY_START_TIME_ON_PRESENTATION_SWITCH : TGUID = '{c8cc113f-7951-4548-aad6-9ed6202e62b3}';
  {$EXTERNALSYM MF_TOPOLOGY_START_TIME_ON_PRESENTATION_SWITCH}

//#if (WINVER >= _WIN32_WINNT_WIN8)

  MF_DISABLE_LOCALLY_REGISTERED_PLUGINS         :  TGUID = '{66b16da9-add4-47e0-a16b-5af1fb483634}';
  {$EXTERNALSYM MF_DISABLE_LOCALLY_REGISTERED_PLUGINS}
  MF_LOCAL_PLUGIN_CONTROL_POLICY                :  TGUID = '{d91b0085-c86d-4f81-8822-8c68e1d7fa04}';
  {$EXTERNALSYM MF_LOCAL_PLUGIN_CONTROL_POLICY}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

  MF_TOPONODE_FLUSH                             :  TGUID = '{494bbce8-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_FLUSH}

  MF_TOPONODE_DRAIN                             :  TGUID = '{494bbce9-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_DRAIN}
  MF_TOPONODE_D3DAWARE                          :  TGUID = '{494bbced-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_D3DAWARE}
  MF_TOPOLOGY_RESOLUTION_STATUS                 :  TGUID = '{494bbcde-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPOLOGY_RESOLUTION_STATUS}
  MF_TOPONODE_ERRORCODE                         :  TGUID = '{494bbcee-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_ERRORCODE}
  MF_TOPONODE_CONNECT_METHOD                    :  TGUID = '{494bbcf1-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_CONNECT_METHOD}
  MF_TOPONODE_LOCKED                            :  TGUID = '{494bbcf7-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_LOCKED}
  MF_TOPONODE_WORKQUEUE_ID                      :  TGUID = '{494bbcf8-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_WORKQUEUE_ID}
  MF_TOPONODE_WORKQUEUE_MMCSS_CLASS             :  TGUID = '{494bbcf9-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_WORKQUEUE_MMCSS_CLASS}
  MF_TOPONODE_DECRYPTOR                         :  TGUID = '{494bbcfa-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_DECRYPTOR}
  MF_TOPONODE_DISCARDABLE                       :  TGUID = '{494bbcfb-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_DISCARDABLE}
  MF_TOPONODE_ERROR_MAJORTYPE                   :  TGUID = '{494bbcfd-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_ERROR_MAJORTYPE}
  MF_TOPONODE_ERROR_SUBTYPE                     :  TGUID = '{494bbcfe-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_ERROR_SUBTYPE}
  MF_TOPONODE_WORKQUEUE_MMCSS_TASKID            :  TGUID = '{494bbcff-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_WORKQUEUE_MMCSS_TASKID}
  MF_TOPONODE_WORKQUEUE_MMCSS_PRIORITY          :  TGUID = '{5001f840-2816-48f4-9364-ad1ef661a123}';
  {$EXTERNALSYM MF_TOPONODE_WORKQUEUE_MMCSS_PRIORITY}
  MF_TOPONODE_WORKQUEUE_ITEM_PRIORITY           :  TGUID = '{a1ff99be-5e97-4a53-b494-568c642c0ff3}';
  {$EXTERNALSYM MF_TOPONODE_WORKQUEUE_ITEM_PRIORITY}
  MF_TOPONODE_MARKIN_HERE                       :  TGUID = '{494bbd00-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_MARKIN_HERE}
  MF_TOPONODE_MARKOUT_HERE                      :  TGUID = '{494bbd01-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_MARKOUT_HERE}
  MF_TOPONODE_DECODER                           :  TGUID = '{494bbd02-b031-4e38-97c4-d5422dd618dc}';
  {$EXTERNALSYM MF_TOPONODE_DECODER}
  MF_TOPONODE_MEDIASTART                        :  TGUID = '{835c58ea-e075-4bc7-bcba-4de000df9ae6}';
  {$EXTERNALSYM MF_TOPONODE_MEDIASTART}
  MF_TOPONODE_MEDIASTOP                         :  TGUID = '{835c58eb-e075-4bc7-bcba-4de000df9ae6}';
  {$EXTERNALSYM MF_TOPONODE_MEDIASTOP}
  MF_TOPONODE_SOURCE                            :  TGUID = '{835c58ec-e075-4bc7-bcba-4de000df9ae6}';
  {$EXTERNALSYM MF_TOPONODE_SOURCE}
  MF_TOPONODE_PRESENTATION_DESCRIPTOR           :  TGUID = '{835c58ed-e075-4bc7-bcba-4de000df9ae6}';
  {$EXTERNALSYM MF_TOPONODE_PRESENTATION_DESCRIPTOR}
  MF_TOPONODE_STREAM_DESCRIPTOR                 :  TGUID = '{835c58ee-e075-4bc7-bcba-4de000df9ae6}';
  {$EXTERNALSYM MF_TOPONODE_STREAM_DESCRIPTOR}
  MF_TOPONODE_SEQUENCE_ELEMENTID                :  TGUID = '{835c58ef-e075-4bc7-bcba-4de000df9ae6}';
  {$EXTERNALSYM MF_TOPONODE_SEQUENCE_ELEMENTID}
  MF_TOPONODE_TRANSFORM_OBJECTID                :  TGUID = '{88dcc0c9-293e-4e8b-9aeb-0ad64cc016b0}';
  {$EXTERNALSYM MF_TOPONODE_TRANSFORM_OBJECTID}
  MF_TOPONODE_STREAMID                          :  TGUID = '{14932f9b-9087-4bb4-8412-5167145cbe04}';
  {$EXTERNALSYM MF_TOPONODE_STREAMID}
  MF_TOPONODE_NOSHUTDOWN_ON_REMOVE              :  TGUID = '{14932f9c-9087-4bb4-8412-5167145cbe04}';
  {$EXTERNALSYM MF_TOPONODE_NOSHUTDOWN_ON_REMOVE}
  MF_TOPONODE_RATELESS                          :  TGUID = '{14932f9d-9087-4bb4-8412-5167145cbe04}';
  {$EXTERNALSYM MF_TOPONODE_RATELESS}
  MF_TOPONODE_DISABLE_PREROLL                   :  TGUID = '{14932f9e-9087-4bb4-8412-5167145cbe04}';
  {$EXTERNALSYM MF_TOPONODE_DISABLE_PREROLL}
  MF_TOPONODE_PRIMARYOUTPUT                     :  TGUID = '{6304ef99-16b2-4ebe-9d67-e4c539b3a259}';
  {$EXTERNALSYM MF_TOPONODE_PRIMARYOUTPUT}

  MFCLOCK_FREQUENCY_HNS         = 10000000;
  {$EXTERNALSYM MFCLOCK_FREQUENCY_HNS}
  MFCLOCK_TOLERANCE_UNKNOWN     = 50000;
  {$EXTERNALSYM MFCLOCK_TOLERANCE_UNKNOWN}
  MFCLOCK_JITTER_ISR            = 1000;
  {$EXTERNALSYM MFCLOCK_JITTER_ISR}
  MFCLOCK_JITTER_DPC            = 4000;
  {$EXTERNALSYM MFCLOCK_JITTER_DPC}
  MFCLOCK_JITTER_PASSIVE        = 10000;
  {$EXTERNALSYM MFCLOCK_JITTER_PASSIVE}

  PRESENTATION_CURRENT_POSITION = $7FFFFFFFFFFFFFFF;
  {$EXTERNALSYM PRESENTATION_CURRENT_POSITION}

  MF_PD_PMPHOST_CONTEXT                         : TGUID = '{6c990d31-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_PMPHOST_CONTEXT}
  MF_PD_APP_CONTEXT                             : TGUID = '{6c990d32-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_APP_CONTEXT}

  MF_PD_DURATION                                : TGUID = '{6c990d33-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_DURATION}
  MF_PD_TOTAL_FILE_SIZE                         : TGUID = '{6c990d34-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_TOTAL_FILE_SIZE}
  MF_PD_AUDIO_ENCODING_BITRATE                  : TGUID = '{6c990d35-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_AUDIO_ENCODING_BITRATE}
  MF_PD_VIDEO_ENCODING_BITRATE                  : TGUID = '{6c990d36-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_VIDEO_ENCODING_BITRATE}
  MF_PD_MIME_TYPE                               : TGUID = '{6c990d37-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_MIME_TYPE}
  MF_PD_LAST_MODIFIED_TIME                      : TGUID = '{6c990d38-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_LAST_MODIFIED_TIME}

//#if (WINVER >= _WIN32_WINNT_WIN7)
  MF_PD_PLAYBACK_ELEMENT_ID                     : TGUID = '{6c990d39-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_PLAYBACK_ELEMENT_ID}
  MF_PD_PREFERRED_LANGUAGE                      : TGUID = '{6c990d3A-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_PREFERRED_LANGUAGE}
  MF_PD_PLAYBACK_BOUNDARY_TIME                  : TGUID = '{6c990d3b-bb8e-477a-8598-0d5d96fcd88a}';
  {$EXTERNALSYM MF_PD_PLAYBACK_BOUNDARY_TIME}
  MF_PD_AUDIO_ISVARIABLEBITRATE                 : TGUID = '{33026ee0-e387-4582-ae0a-34a2ad3baa18}';
  {$EXTERNALSYM MF_PD_AUDIO_ISVARIABLEBITRATE}
//#endif // (WINVER >= _WIN32_WINNT_WIN7)

//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)
  MF_PD_ADAPTIVE_STREAMING                      : TGUID = '{EA0D5D97-29F9-488B-AE6B-7D6B4136112B}';
  {$EXTERNALSYM MF_PD_ADAPTIVE_STREAMING}
//#endif // (WINVER >= _WIN32_WINNT_WINTHRESHOLD)


  MF_SD_LANGUAGE                                : TGUID = '{00af2180-bdc2-423c-abca-f503593bc121}';
  {$EXTERNALSYM MF_SD_LANGUAGE}
  MF_SD_PROTECTED                               : TGUID = '{00af2181-bdc2-423c-abca-f503593bc121}';
  {$EXTERNALSYM MF_SD_PROTECTED}
  MF_SD_STREAM_NAME                             : TGUID = '{4f1b099d-d314-41e5-a781-7fefaa4c501f}';
  {$EXTERNALSYM MF_SD_STREAM_NAME}
  MF_SD_MUTUALLY_EXCLUSIVE                      : TGUID = '{023ef79c-388d-487f-ac17-696cd6e3c6f5}';
  {$EXTERNALSYM MF_SD_MUTUALLY_EXCLUSIVE}

  MF_ACTIVATE_CUSTOM_VIDEO_MIXER_CLSID          : TGUID = '{ba491360-be50-451e-95ab-6d4accc7dad8}';
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_VIDEO_MIXER_CLSID}
  MF_ACTIVATE_CUSTOM_VIDEO_MIXER_ACTIVATE       : TGUID = '{ba491361-be50-451e-95ab-6d4accc7dad8}';
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_VIDEO_MIXER_ACTIVATE}
  MF_ACTIVATE_CUSTOM_VIDEO_MIXER_FLAGS          : TGUID = '{ba491362-be50-451e-95ab-6d4accc7dad8}';
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_VIDEO_MIXER_FLAGS}
  MF_ACTIVATE_CUSTOM_VIDEO_PRESENTER_CLSID      : TGUID = '{ba491364-be50-451e-95ab-6d4accc7dad8}';
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_VIDEO_PRESENTER_CLSID}
  MF_ACTIVATE_CUSTOM_VIDEO_PRESENTER_ACTIVATE   : TGUID = '{ba491365-be50-451e-95ab-6d4accc7dad8}';
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_VIDEO_PRESENTER_ACTIVATE}
  MF_ACTIVATE_CUSTOM_VIDEO_PRESENTER_FLAGS      : TGUID = '{ba491366-be50-451e-95ab-6d4accc7dad8}';
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_VIDEO_PRESENTER_FLAGS}

  MF_ACTIVATE_CUSTOM_MIXER_ALLOWFAIL      = $1;
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_MIXER_ALLOWFAIL}
  MF_ACTIVATE_CUSTOM_PRESENTER_ALLOWFAIL  = $1;
  {$EXTERNALSYM MF_ACTIVATE_CUSTOM_PRESENTER_ALLOWFAIL}

  MF_ACTIVATE_MFT_LOCKED                        : TGUID = '{c1f6093c-7f65-4fbd-9e39-5faec3c4fbd7}';
  {$EXTERNALSYM MF_ACTIVATE_MFT_LOCKED}
  MF_ACTIVATE_VIDEO_WINDOW                      : TGUID = '{9a2dbbdd-f57e-4162-82b9-6831377682d3}';
  {$EXTERNALSYM MF_ACTIVATE_VIDEO_WINDOW}

  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS             : TGUID = '{ede4b5e0-f805-4d6c-99b3-db01bf95dfab}';
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS}


  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_CROSSPROCESS = $1;
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_CROSSPROCESS}
  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_NOPERSIST    = $2;
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_NOPERSIST}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_DONT_ALLOW_FORMAT_CHANGES = $4;
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_DONT_ALLOW_FORMAT_CHANGES}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  MF_AUDIO_RENDERER_ATTRIBUTE_SESSION_ID        : TGUID = '{ede4b5e3-f805-4d6c-99b3-db01bf95dfab}';
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_SESSION_ID}
  MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID       : TGUID = '{b10aaec3-ef71-4cc3-b873-05a9a08b9f8e}';
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID}
  MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ROLE     : TGUID = '{6ba644ff-27c5-4d02-9887-c28619fdb91b}';
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ROLE}
  MF_AUDIO_RENDERER_ATTRIBUTE_STREAM_CATEGORY   : TGUID = '{a9770471-92ec-4df4-94fe-81c36f0c3a7a}';
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_STREAM_CATEGORY}

  MFENABLETYPE_WMDRMV1_LicenseAcquisition       : TGUID = '{4ff6eeaf-0b43-4797-9b85-abf31815e7b0}';
  {$EXTERNALSYM MFENABLETYPE_WMDRMV1_LicenseAcquisition}
  MFENABLETYPE_WMDRMV7_LicenseAcquisition       : TGUID = '{003306df-4a06-4884-a097-ef6d22ec84a3}';
  {$EXTERNALSYM MFENABLETYPE_WMDRMV7_LicenseAcquisition}
  MFENABLETYPE_WMDRMV7_Individualization        : TGUID = '{acd2c84a-b303-4f65-bc2c-2c848d01a989}';
  {$EXTERNALSYM MFENABLETYPE_WMDRMV7_Individualization}
  MFENABLETYPE_MF_UpdateRevocationInformation   : TGUID = '{e558b0b5-b3c4-44a0-924c-50d178932385}';
  {$EXTERNALSYM MFENABLETYPE_MF_UpdateRevocationInformation}
  MFENABLETYPE_MF_UpdateUntrustedComponent      : TGUID = '{9879f3d6-cee2-48e6-b573-9767ab172f16}';
  {$EXTERNALSYM MFENABLETYPE_MF_UpdateUntrustedComponent}
  MFENABLETYPE_MF_RebootRequired                : TGUID = '{6d4d3d4b-0ece-4652-8b3a-f2d24260d887}';
  {$EXTERNALSYM MFENABLETYPE_MF_RebootRequired}


  // Structs that contain information about revoked or unsigned binaries,
  // returned by the IMFContentEnabler.GetEnableData() method of
  // the Revocation content enabler

{$IFNDEF MFRR_INFO_VERSION}
  MFRR_INFO_VERSION = 0;
  {$EXTERNALSYM MFRR_INFO_VERSION}
{$ENDIF}

  // The values for MFRR_COMPONENT_HASH_INFO.ulReason
  //=================================================
  MF_USER_MODE_COMPONENT_LOAD         = $00000001;
  {$EXTERNALSYM MF_USER_MODE_COMPONENT_LOAD}
  MF_KERNEL_MODE_COMPONENT_LOAD       = $00000002;
  {$EXTERNALSYM MF_KERNEL_MODE_COMPONENT_LOAD}
  MF_GRL_LOAD_FAILED                  = $00000010;
  {$EXTERNALSYM MF_GRL_LOAD_FAILED}
  MF_INVALID_GRL_SIGNATURE            = $00000020;
  {$EXTERNALSYM MF_INVALID_GRL_SIGNATURE}
  MF_GRL_ABSENT                       = $00001000;
  {$EXTERNALSYM MF_GRL_ABSENT}
  MF_COMPONENT_REVOKED                = $00002000;
  {$EXTERNALSYM MF_COMPONENT_REVOKED}
  MF_COMPONENT_INVALID_EKU            = $00004000;
  {$EXTERNALSYM MF_COMPONENT_INVALID_EKU}
  MF_COMPONENT_CERT_REVOKED           = $00008000;
  {$EXTERNALSYM MF_COMPONENT_CERT_REVOKED}
  MF_COMPONENT_INVALID_ROOT           = $00010000;
  {$EXTERNALSYM MF_COMPONENT_INVALID_ROOT}
  MF_COMPONENT_HS_CERT_REVOKED        = $00020000;
  {$EXTERNALSYM MF_COMPONENT_HS_CERT_REVOKED}
  MF_COMPONENT_LS_CERT_REVOKED        = $00040000;
  {$EXTERNALSYM MF_COMPONENT_LS_CERT_REVOKED}
  MF_BOOT_DRIVER_VERIFICATION_FAILED  = $00100000;
  {$EXTERNALSYM MF_BOOT_DRIVER_VERIFICATION_FAILED}
  MF_TEST_SIGNED_COMPONENT_LOADING    = $01000000;
  {$EXTERNALSYM MF_TEST_SIGNED_COMPONENT_LOADING}
  MF_MINCRYPT_FAILURE                 = $10000000;
  {$EXTERNALSYM MF_MINCRYPT_FAILURE}


  //STR_HASH_LEN: Number of characters required to represent a SHA-1 hash
  //(RTL_MAX_HASH_LEN_V1) as a string of the form "$5a3b53463b672a4f..."
  //Each byte of a SHA-1 hash takes two characters to represent, and
  //we add in two leading characters "$" as well as the NULL terminator

  SHA_HASH_LEN                        = 20;
  {$EXTERNALSYM SHA_HASH_LEN}
  STR_HASH_LEN                        = ((SHA_HASH_LEN * 2) + 3);
  {$EXTERNALSYM STR_HASH_LEN}


  MF_METADATA_PROVIDER_SERVICE                  : TGUID = '{db214084-58a4-4d2e-b84f-6f755b2f7a0d}';
  {$EXTERNALSYM MF_METADATA_PROVIDER_SERVICE}
  MF_PROPERTY_HANDLER_SERVICE                   : TGUID = '{a3face02-32b8-41dd-90e7-5fef7c8991b5}';
  {$EXTERNALSYM MF_PROPERTY_HANDLER_SERVICE}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MF_TIMECODE_SERVICE                           : TGUID = '{a0d502a7-0eb3-4885-b1b9-9feb0d083454}';
  {$EXTERNALSYM MF_TIMECODE_SERVICE}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MR_POLICY_VOLUME_SERVICE                      : TGUID = '{1abaa2ac-9d3b-47c6-ab48-c59506de784d}';
  {$EXTERNALSYM MR_POLICY_VOLUME_SERVICE}

//#if (WINVER >= _WIN32_WINNT_WIN8)

  MR_CAPTURE_POLICY_VOLUME_SERVICE              : TGUID = '{24030acd-107a-4265-975c-414e33e65f2a}';
  {$EXTERNALSYM MR_CAPTURE_POLICY_VOLUME_SERVICE}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

  MR_AUDIO_POLICY_SERVICE                       : TGUID = '{911fd737-6775-4ab0-a614-297862fdac88}';
  {$EXTERNALSYM MR_AUDIO_POLICY_SERVICE}

  MF_SAMPLEGRABBERSINK_SAMPLE_TIME_OFFSET       : TGUID = '{62e3d776-8100-4e03-a6e8-bd3857ac9c47}';
  {$EXTERNALSYM MF_SAMPLEGRABBERSINK_SAMPLE_TIME_OFFSET}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MF_SAMPLEGRABBERSINK_IGNORE_CLOCK             : TGUID = '{0efda2c0-0b69-4e2e-ab8d-46dcbff7d25d}';
  {$EXTERNALSYM MF_SAMPLEGRABBERSINK_IGNORE_CLOCK}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MF_QUALITY_SERVICES                           : TGUID = '{b7e2be11-2f96-4640-b52c-282365bdf16c}';
  {$EXTERNALSYM MF_QUALITY_SERVICES}

  MF_WORKQUEUE_SERVICES                         : TGUID = '{8e37d489-41e0-413a-9068-287c886d8dda}';
  {$EXTERNALSYM MF_WORKQUEUE_SERVICES}

  MF_QUALITY_NOTIFY_PROCESSING_LATENCY          : TGUID = '{f6b44af8-604d-46fe-a95d-45479b10c9bc}';
  {$EXTERNALSYM MF_QUALITY_NOTIFY_PROCESSING_LATENCY}
  MF_QUALITY_NOTIFY_SAMPLE_LAG                  : TGUID = '{30d15206-ed2a-4760-be17-eb4a9f12295c}';
  {$EXTERNALSYM MF_QUALITY_NOTIFY_SAMPLE_LAG}


  MFSEQUENCER_INVALID_ELEMENT_ID      = MAXDW; //$ffffffff;
  {$EXTERNALSYM MFSEQUENCER_INVALID_ELEMENT_ID}


  // Sequencer constants
  //////////////////////

  MF_TIME_FORMAT_SEGMENT_OFFSET                 : TGUID = '{c8b8be77-869c-431d-812e-169693f65a39}';
  {$EXTERNALSYM MF_TIME_FORMAT_SEGMENT_OFFSET}
  // MF_TIME_FORMAT_SEGMENT_OFFSET can be used as the pguidTimeFormat argument
  // to IMFMediaSession.Start and IMFMediaSource.Start to indicate that
  // playback should start at a given offset relative to a sequencer element.
  // The associated PROPVARIANT (TMfPPROPVARIANT) can be created by calling
  // MFCreateSequencerSegmentOffset.


  MF_TOPONODE_ATTRIBUTE_EDITOR_SERVICE          : TGUID = '{65656e1a-077f-4472-83ef-316f11d5087a}';
  {$EXTERNALSYM MF_TOPONODE_ATTRIBUTE_EDITOR_SERVICE}

  MFNETSOURCE_SSLCERTIFICATE_MANAGER            : TGUID = '{55e6cb27-e69b-4267-940c-2d7ec5bb8a0f}';
  {$EXTERNALSYM MFNETSOURCE_SSLCERTIFICATE_MANAGER}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  MFNETSOURCE_RESOURCE_FILTER                   :  TGUID = '{815d0ff6-265a-4477-9e46-7b80ad80b5fb}';
  {$EXTERNALSYM MFNETSOURCE_RESOURCE_FILTER}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MFNET_SAVEJOB_SERVICE                         : TGUID = '{b85a587f-3d02-4e52-9565-55d3ec1e7ff7}';
  {$EXTERNALSYM MFNET_SAVEJOB_SERVICE}

  MFNETSOURCE_STATISTICS_SERVICE                : TGUID = '{3cb1f275-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_STATISTICS_SERVICE}
  MFNETSOURCE_STATISTICS                        : TGUID = '{3cb1f274-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_STATISTICS}

  MFNETSOURCE_BUFFERINGTIME                     : TGUID = '{3cb1f276-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_BUFFERINGTIME}
  MFNETSOURCE_ACCELERATEDSTREAMINGDURATION      : TGUID = '{3cb1f277-0505-0c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ACCELERATEDSTREAMINGDURATION}
  MFNETSOURCE_MAXUDPACCELERATEDSTREAMINGDURATION: TGUID = '{4aab2879-bbe1-4994-9ff0-5495bd250129}';
  {$EXTERNALSYM MFNETSOURCE_MAXUDPACCELERATEDSTREAMINGDURATION}
  MFNETSOURCE_MAXBUFFERTIMEMS                   : TGUID = '{408b24e6-4038-4401-b5b2-fe701a9ebf10}';
  {$EXTERNALSYM MFNETSOURCE_MAXBUFFERTIMEMS}
  MFNETSOURCE_CONNECTIONBANDWIDTH               : TGUID = '{3cb1f278-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_CONNECTIONBANDWIDTH}
  MFNETSOURCE_CACHEENABLED                      : TGUID = '{3cb1f279-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_CACHEENABLED}
  MFNETSOURCE_AUTORECONNECTLIMIT                : TGUID = '{3cb1f27a-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_AUTORECONNECTLIMIT}
  MFNETSOURCE_RESENDSENABLED                    : TGUID = '{3cb1f27b-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_RESENDSENABLED}
  MFNETSOURCE_THINNINGENABLED                   : TGUID = '{3cb1f27c-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_THINNINGENABLED}
  MFNETSOURCE_PROTOCOL                          : TGUID = '{3cb1f27d-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROTOCOL}
  MFNETSOURCE_TRANSPORT                         : TGUID = '{3cb1f27e-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_TRANSPORT}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MFNETSOURCE_PREVIEWMODEENABLED                : TGUID = '{3cb1f27f-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PREVIEWMODEENABLED}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MFNETSOURCE_CREDENTIAL_MANAGER                : TGUID = '{3cb1f280-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_CREDENTIAL_MANAGER}
  MFNETSOURCE_PPBANDWIDTH                       : TGUID = '{3cb1f281-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PPBANDWIDTH}
  MFNETSOURCE_AUTORECONNECTPROGRESS             : TGUID = '{3cb1f282-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_AUTORECONNECTPROGRESS}
  MFNETSOURCE_PROXYLOCATORFACTORY               : TGUID = '{3cb1f283-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYLOCATORFACTORY}
  MFNETSOURCE_BROWSERUSERAGENT                  : TGUID = '{3cb1f28b-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_BROWSERUSERAGENT}
  MFNETSOURCE_BROWSERWEBPAGE                    : TGUID = '{3cb1f28c-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_BROWSERWEBPAGE}
  MFNETSOURCE_PLAYERVERSION                     : TGUID = '{3cb1f28d-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PLAYERVERSION}
  MFNETSOURCE_PLAYERID                          : TGUID = '{3cb1f28e-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PLAYERID}
  MFNETSOURCE_HOSTEXE                           : TGUID = '{3cb1f28f-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_HOSTEXE}
  MFNETSOURCE_HOSTVERSION                       : TGUID = '{3cb1f291-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_HOSTVERSION}
  MFNETSOURCE_PLAYERUSERAGENT                   : TGUID = '{3cb1f292-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PLAYERUSERAGENT}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MFNETSOURCE_CLIENTGUID                        : TGUID = '{60a2c4a6-f197-4c14-a5bf-88830d2458af}';

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MFNETSOURCE_LOGURL                            : TGUID = '{3cb1f293-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_LOGURL}
  MFNETSOURCE_ENABLE_UDP                        : TGUID = '{3cb1f294-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_UDP}
  MFNETSOURCE_ENABLE_TCP                        : TGUID = '{3cb1f295-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_TCP}
  MFNETSOURCE_ENABLE_MSB                        : TGUID = '{3cb1f296-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_MSB}
  MFNETSOURCE_ENABLE_RTSP                       : TGUID = '{3cb1f298-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_RTSP}
  MFNETSOURCE_ENABLE_HTTP                       : TGUID = '{3cb1f299-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_HTTP}
  MFNETSOURCE_ENABLE_STREAMING                  : TGUID = '{3cb1f290-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_STREAMING}
  MFNETSOURCE_ENABLE_DOWNLOAD                   : TGUID = '{3cb1f29d-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_ENABLE_DOWNLOAD}
  MFNETSOURCE_UDP_PORT_RANGE                    : TGUID = '{3cb1f29a-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_UDP_PORT_RANGE}
  MFNETSOURCE_PROXYINFO                         : TGUID = '{3cb1f29b-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYINFO}
  MFNETSOURCE_DRMNET_LICENSE_REPRESENTATION     : TGUID = '{47eae1bd-bdfe-42e2-82f3-54a48c17962d}';
  {$EXTERNALSYM MFNETSOURCE_DRMNET_LICENSE_REPRESENTATION}
  MFNETSOURCE_PROXYSETTINGS                     : TGUID = '{3cb1f287-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYSETTINGS}
  MFNETSOURCE_PROXYHOSTNAME                     : TGUID = '{3cb1f284-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYHOSTNAME}
  MFNETSOURCE_PROXYPORT                         : TGUID = '{3cb1f288-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYPORT}
  MFNETSOURCE_PROXYEXCEPTIONLIST                : TGUID = '{3cb1f285-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYEXCEPTIONLIST}
  MFNETSOURCE_PROXYBYPASSFORLOCAL               : TGUID = '{3cb1f286-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYBYPASSFORLOCAL}
  MFNETSOURCE_PROXYRERUNAUTODETECTION           : TGUID = '{3cb1f289-0505-4c5d-ae71-0a556344efa1}';
  {$EXTERNALSYM MFNETSOURCE_PROXYRERUNAUTODETECTION}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MFNETSOURCE_STREAM_LANGUAGE                   : TGUID = '{9ab44318-f7cd-4f2d-8d6d-fa35b492cecb}';
  {$EXTERNALSYM MFNETSOURCE_STREAM_LANGUAGE}
  MFNETSOURCE_LOGPARAMS                         : TGUID = '{64936ae8-9418-453a-8cda-3e0a668b353b}';
  {$EXTERNALSYM MFNETSOURCE_LOGPARAMS}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

//#if (WINVER >= _WIN32_WINNT_WIN8)

  MFNETSOURCE_PEERMANAGER                       : TGUID = '{48b29adb-febf-45ee-a9bf-efb81c492efc}';
  {$EXTERNALSYM MFNETSOURCE_PEERMANAGER}
  MFNETSOURCE_FRIENDLYNAME                      : TGUID = '{5b2a7757-bc6b-447e-aa06-0dda1c646e2f}';
  {$EXTERNALSYM MFNETSOURCE_FRIENDLYNAME}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

//#if (WINVER >= _WIN32_WINNT_WIN7)

  MF_BYTESTREAMHANDLER_ACCEPTS_SHARE_WRITE      : TGUID = '{a6e1f733-3001-4915-8150-1558a2180ec8}';
  {$EXTERNALSYM MF_BYTESTREAMHANDLER_ACCEPTS_SHARE_WRITE}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MF_BYTESTREAM_SERVICE                         : TGUID = '{ab025e2b-16d9-4180-a127-ba6c70156161}';
  {$EXTERNALSYM MF_BYTESTREAM_SERVICE}

  MFOUTPUTATTRIBUTE_DIGITAL                   = DWORD($00000001);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_DIGITAL}
  MFOUTPUTATTRIBUTE_NONSTANDARDIMPLEMENTATION = DWORD($00000002);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_NONSTANDARDIMPLEMENTATION}
  MFOUTPUTATTRIBUTE_VIDEO                     = DWORD($00000004);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_VIDEO}
  MFOUTPUTATTRIBUTE_COMPRESSED                = DWORD($00000008);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_COMPRESSED}
  MFOUTPUTATTRIBUTE_SOFTWARE                  = DWORD($00000010);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_SOFTWARE}
  MFOUTPUTATTRIBUTE_BUS                       = DWORD($00000020);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_BUS}
  MFOUTPUTATTRIBUTE_BUSIMPLEMENTATION         = DWORD($0000FF00);
  {$EXTERNALSYM MFOUTPUTATTRIBUTE_BUSIMPLEMENTATION}


  MFCONNECTOR_SPDIF                             : TGUID = '{0b94a712-ad3e-4cee-83ce-ce32e3db6522}';
  {$EXTERNALSYM MFCONNECTOR_SPDIF}
  MFCONNECTOR_UNKNOWN                           : TGUID = '{ac3aef5c-ce43-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_UNKNOWN}
  MFCONNECTOR_PCI                               : TGUID = '{ac3aef5d-ce43-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_PCI}
  MFCONNECTOR_PCIX                              : TGUID = '{ac3aef5e-ce43-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_PCIX}
  MFCONNECTOR_PCI_Express                       : TGUID = '{ac3aef5f-ce43-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_PCI_Express}
  MFCONNECTOR_AGP                               : TGUID = '{ac3aef60-ce43-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_AGP}


  MFCONNECTOR_VGA                               : TGUID = '{57cd5968-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_VGA}
  MFCONNECTOR_SVIDEO                            : TGUID = '{57cd5969-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_SVIDEO}
  MFCONNECTOR_COMPOSITE                         : TGUID = '{57cd596a-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_COMPOSITE}
  MFCONNECTOR_COMPONENT                         : TGUID = '{57cd596b-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_COMPONENT}
  MFCONNECTOR_DVI                               : TGUID = '{57cd596c-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_DVI}
  MFCONNECTOR_HDMI                              : TGUID = '{57cd596d-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_HDMI}
  MFCONNECTOR_LVDS                              : TGUID = '{57cd596e-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_LVDS}
  // The GUID 57cd596f-ce47-11d9-92db-000bdb28ff98 is reserved for TMDS
  MFCONNECTOR_D_JPN                             : TGUID = '{57cd5970-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_D_JPN}
  MFCONNECTOR_SDI                               : TGUID = '{57cd5971-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_SDI}
  MFCONNECTOR_DISPLAYPORT_EXTERNAL              : TGUID = '{57cd5972-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_DISPLAYPORT_EXTERNAL}
  MFCONNECTOR_DISPLAYPORT_EMBEDDED              : TGUID = '{57cd5973-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_DISPLAYPORT_EMBEDDED}
  MFCONNECTOR_UDI_EXTERNAL                      : TGUID = '{57cd5974-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_UDI_EXTERNAL}
  MFCONNECTOR_UDI_EMBEDDED                      : TGUID = '{57cd5975-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_UDI_EMBEDDED}
  // The GUID 57cd5976-ce47-11d9-92db-000bdb28ff98 is reserved
  MFCONNECTOR_MIRACAST                          : TGUID = '{57cd5977-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_MIRACAST}
  MFCONNECTOR_TRANSPORT_AGNOSTIC_DIGITAL_MODE_A :	TGUID = '{57cd5978-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_TRANSPORT_AGNOSTIC_DIGITAL_MODE_A}
  MFCONNECTOR_TRANSPORT_AGNOSTIC_DIGITAL_MODE_B :	TGUID = '{57cd5979-ce47-11d9-92db-000bdb28ff98}';
  {$EXTERNALSYM MFCONNECTOR_TRANSPORT_AGNOSTIC_DIGITAL_MODE_B}

// #if (NTDDI_VERSION >= NTDDI_WIN10_VB)
  // MF_POLICY_ID
  // Data type: UINT32 (treat as BOOL)
  // An identifier that can be set on an IMFOutputPolicy.
  // The value can be recieved from the MEPolicySet media event. It is stored as a VT_UI4
  // type payload in the MEPolicySet event.
  // {B160C24D-C059-48F1-A901-9EE298A9A8C3}
  MF_POLICY_ID                                  : TGUID = '{B160C24D-C059-48F1-A901-9EE298A9A8C3}';
  {$EXTERNALSYM MF_POLICY_ID}
// #endif // (NTDDI_VERSION >= NTDDI_WIN10_VB)

  MFPROTECTION_DISABLE                          : TGUID = '{8cc6d81b-fec6-4d8f-964b-cfba0b0dad0d}';
  {$EXTERNALSYM MFPROTECTION_DISABLE}
  MFPROTECTION_CONSTRICTVIDEO                   : TGUID = '{193370ce-c5e4-4c3a-8a66-6959b4da4442}';
  {$EXTERNALSYM MFPROTECTION_CONSTRICTVIDEO}
  MFPROTECTION_CONSTRICTVIDEO_NOOPM             :	TGUID = '{a580e8cd-c247-4957-b983-3c2eebd1ff59}';
  {$EXTERNALSYM MFPROTECTION_CONSTRICTVIDEO_NOOPM}

  MFPROTECTION_CONSTRICTAUDIO                   : TGUID = '{ffc99b44-df48-4e16-8e66-096892c1578a}';
  {$EXTERNALSYM MFPROTECTION_CONSTRICTAUDIO}
  MFPROTECTION_TRUSTEDAUDIODRIVERS              : TGUID = '{65bdf3d2-0168-4816-a533-55d47b027101}';
  {$EXTERNALSYM MFPROTECTION_TRUSTEDAUDIODRIVERS}
  MFPROTECTION_HDCP                             : TGUID = '{AE7CC03D-C828-4021-acb7-d578d27aaf13}';
  {$EXTERNALSYM MFPROTECTION_HDCP}
  MFPROTECTION_CGMSA                            : TGUID = '{E57E69E9-226B-4d31-B4E3-D3DB008736DD}';
  {$EXTERNALSYM MFPROTECTION_CGMSA}
  MFPROTECTION_ACP                              : TGUID = '{c3fd11c6-f8b7-4d20-b008-1db17d61f2da}';
  {$EXTERNALSYM MFPROTECTION_ACP}
  MFPROTECTION_WMDRMOTA                         : TGUID = '{a267a6a1-362e-47d0-8805-4628598a23e4}';
  {$EXTERNALSYM MFPROTECTION_WMDRMOTA}
  MFPROTECTION_FFT                              : TGUID = '{462a56b2-2866-4bb6-980d-6d8d9edb1a8c}';
  {$EXTERNALSYM MFPROTECTION_FFT}
  MFPROTECTION_PROTECTED_SURFACE                : TGUID = '{4f5d9566-e742-4a25-8d1f-d287b5fa0ade}';
  {$EXTERNALSYM MFPROTECTION_PROTECTED_SURFACE}
  MFPROTECTION_DISABLE_SCREEN_SCRAPE            : TGUID = '{a21179a4-b7cd-40d8-9614-8ef2371ba78d}';
  {$EXTERNALSYM MFPROTECTION_DISABLE_SCREEN_SCRAPE}
  MFPROTECTION_VIDEO_FRAMES                     :	TGUID = '{36a59cbc-7401-4a8c-bc20-46a7c9e597f0}';
  {$EXTERNALSYM MFPROTECTION_VIDEO_FRAMES}
  MFPROTECTION_HARDWARE                         :	TGUID = '{4ee7f0c1-9ed7-424f-b6be-996b33528856}';
  {$EXTERNALSYM MFPROTECTION_HARDWARE}
  MFPROTECTION_HDCP_WITH_TYPE_ENFORCEMENT       :	TGUID = '{a4a585e8-ed60-442d-814d-db4d4220a06d}';
  {$EXTERNALSYM MFPROTECTION_HDCP_WITH_TYPE_ENFORCEMENT}


  MFPROTECTIONATTRIBUTE_CONSTRICTVIDEO_IMAGESIZE: TGUID = '{008476fc-4b58-4d80-a790-e7297673161d}';
  {$EXTERNALSYM MFPROTECTIONATTRIBUTE_CONSTRICTVIDEO_IMAGESIZE}
  MFPROTECTIONATTRIBUTE_HDCP_SRM                : TGUID = '{6f302107-3477-4468-8a08-eef9db10e20f}';
  {$EXTERNALSYM MFPROTECTIONATTRIBUTE_HDCP_SRM}

  MF_SampleProtectionSalt                       : TGUID = '{5403deee-b9ee-438f-aa83-3804997e569d}';
  {$EXTERNALSYM MF_SampleProtectionSalt}

  MF_SAMI_SERVICE                               : TGUID = '{49a89ae7-b4d9-4ef2-aa5c-f65a3e05ae4e}';
  {$EXTERNALSYM MF_SAMI_SERVICE}
  MF_PD_SAMI_STYLELIST                          : TGUID = '{e0b73c7f-486d-484e-9872-4de5192a7bf8}';
  {$EXTERNALSYM MF_PD_SAMI_STYLELIST}
  // Contains the Synchronized Accessible Media Interchange (SAMI) language name that is defined for the stream.
  // This attribute is present in the stream descriptors returned from the SAMI media source.
  MF_SD_SAMI_LANGUAGE                           : TGUID = '{36fcb98a-6cd0-44cb-acb9-a8f5600dd0bb}';
  {$EXTERNALSYM MF_SD_SAMI_LANGUAGE}

  MF_TRANSCODE_CONTAINERTYPE                    : TGUID = '{150ff23f-4abc-478b-ac4f-e1916fba1cca}';
  {$EXTERNALSYM MF_TRANSCODE_CONTAINERTYPE}
  MFTranscodeContainerType_ASF                  : TGUID = '{430f6f6e-b6bf-4fc1-a0bd-9ee46eee2afb}';
  {$EXTERNALSYM MFTranscodeContainerType_ASF}
  MFTranscodeContainerType_MPEG4                : TGUID = '{dc6cd05d-b9d0-40ef-bd35-fa622c1ab28a}';
  {$EXTERNALSYM MFTranscodeContainerType_MPEG4}
  MFTranscodeContainerType_MP3                  : TGUID = '{e438b912-83f1-4de6-9e3a-9ffbc6dd24d1}';
  {$EXTERNALSYM MFTranscodeContainerType_MP3}
  MFTranscodeContainerType_FLAC                 :	TGUID = '{31344aa3-05a9-42b5-901b-8e9d4257f75e}';
  {$EXTERNALSYM MFTranscodeContainerType_FLAC}
  MFTranscodeContainerType_3GP                  : TGUID = '{34c50167-4472-4f34-9ea0-c49fbacf037d}';
  {$EXTERNALSYM MFTranscodeContainerType_3GP}
  MFTranscodeContainerType_AC3                  : TGUID = '{6d8d91c3-8c91-4ed1-8742-8c347d5b44d0}';
  {$EXTERNALSYM MFTranscodeContainerType_AC3}
  MFTranscodeContainerType_ADTS                 : TGUID = '{132fd27d-0f02-43de-a301-38fbbbb3834e}';
  {$EXTERNALSYM MFTranscodeContainerType_ADTS}
  MFTranscodeContainerType_MPEG2                : TGUID = '{bfc2dbf9-7bb4-4f8f-afde-e112c44ba882}';
  {$EXTERNALSYM MFTranscodeContainerType_MPEG2}
  MFTranscodeContainerType_WAVE                 : TGUID = '{64c3453c-0f26-4741-be63-87bdf8bb935b}';
  {$EXTERNALSYM MFTranscodeContainerType_WAVE}
  MFTranscodeContainerType_AVI                  : TGUID = '{7edfe8af-402f-4d76-a33c-619fd157d0f1}';
  {$EXTERNALSYM MFTranscodeContainerType_AVI}

//#if (WINVER >= _WIN32_WINNT_WIN8)

  FTranscodeContainerType_FMPEG4                : TGUID = '{9ba876f1-419f-4b77-a1e0-35959d9d4004}';
  {$EXTERNALSYM FTranscodeContainerType_FMPEG4}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

  MF_TRANSCODE_SKIP_METADATA_TRANSFER           : TGUID = '{4e4469ef-b571-4959-8f83-3dcfba33a393}';
  {$EXTERNALSYM MF_TRANSCODE_SKIP_METADATA_TRANSFER}
  MF_TRANSCODE_TOPOLOGYMODE                     : TGUID = '{3e3df610-394a-40b2-9dea-3bab650bebf2}';
  {$EXTERNALSYM MF_TRANSCODE_TOPOLOGYMODE}

  MF_TRANSCODE_ADJUST_PROFILE                   : TGUID = '{9c37c21b-060f-487c-a690-80d7f50d1c72}';
  {$EXTERNALSYM MF_TRANSCODE_ADJUST_PROFILE}

  MF_TRANSCODE_ENCODINGPROFILE                  : TGUID = '{6947787c-f508-4ea9-b1e9-a1fe3a49fbc9}';
  {$EXTERNALSYM MF_TRANSCODE_ENCODINGPROFILE}
  MF_TRANSCODE_QUALITYVSSPEED                   : TGUID = '{98332df8-03cd-476b-89fa-3f9e442dec9f}';
  {$EXTERNALSYM MF_TRANSCODE_QUALITYVSSPEED}
  MF_TRANSCODE_DONOT_INSERT_ENCODER             : TGUID = '{f45aa7ce-ab24-4012-a11b-dc8220201410}';
  {$EXTERNALSYM MF_TRANSCODE_DONOT_INSERT_ENCODER}

  MF_VIDEO_PROCESSOR_ALGORITHM                  : TGUID = '{4a0a1e1f-272c-4fb6-9eb1-db330cbc97ca}';
  {$EXTERNALSYM MF_VIDEO_PROCESSOR_ALGORITHM}
  MF_XVP_DISABLE_FRC                            : TGUID = '{2c0afa19-7a97-4d5a-9ee8-16d4fc518d8c}';
  {$EXTERNALSYM MF_XVP_DISABLE_FRC}

//#if (WINVER >= _WIN32_WINNT_WINBLUE)
  MF_XVP_CALLER_ALLOCATES_OUTPUT                :	TGUID = '{4a2cabc0-0cab-40b1-a1b9-75bc3658f000}';
  {$EXTERNALSYM MF_XVP_CALLER_ALLOCATES_OUTPUT}
//#endif // (WINVER >= _WIN32_WINNT_WINBLUE)

  MF_LOCAL_MFT_REGISTRATION_SERVICE             : TGUID = '{ddf5cf9c-4506-45aa-abf0-6d5d94dd1b4a}';
  {$EXTERNALSYM MF_LOCAL_MFT_REGISTRATION_SERVICE}

//#if (WINVER >= _WIN32_WINNT_WINBLUE)
  MF_WRAPPED_BUFFER_SERVICE                     : TGUID = '{ab544072-c269-4ebc-a552-1c3b32bed5ca}';
  {$EXTERNALSYM MF_WRAPPED_BUFFER_SERVICE}
  MF_WRAPPED_SAMPLE_SERVICE                     : TGUID = '{31f52bf2-d03e-4048-80d0-9c1046d87c61}';
  {$EXTERNALSYM MF_WRAPPED_SAMPLE_SERVICE}
//#endif // (WINVER >= _WIN32_WINNT_WINBLUE)

  MF_WRAPPED_OBJECT                             :  TGUID = '{2b182c4c-d6ac-49f4-8915-f71887db70cd}';
  {$EXTERNALSYM MF_WRAPPED_OBJECT}
  CLSID_HttpSchemePlugin                        :  TGUID = '{44cb442b-9da9-49df-b3fd-023777b16e50}';
  {$EXTERNALSYM CLSID_HttpSchemePlugin}
  CLSID_UrlmonSchemePlugin                      :  TGUID = '{9ec4b4f9-3029-45ad-947b-344de2a249e2}';
  {$EXTERNALSYM CLSID_UrlmonSchemePlugin}

  CLSID_NetSchemePlugin                         :  TGUID = '{e9f4ebab-d97b-463e-a2b1-c54ee3f9414d}';
  {$EXTERNALSYM CLSID_NetSchemePlugin}

  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE                        : TGUID = '{c60ac5fe-252a-478f-a0ef-bc8fa5f7cad3}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_HW_SOURCE       : TGUID = '{de7046ba-54d6-4487-a2a4-ec7c0d1bd163}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_HW_SOURCE}
  MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME                      : TGUID = '{60d0e559-52f8-4fa2-bbce-acdb34a8ec01}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME}
  MF_DEVSOURCE_ATTRIBUTE_MEDIA_TYPE                         : TGUID = '{56a819ca-0c78-4de4-a0a7-3ddaba0f24d4}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_MEDIA_TYPE}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_CATEGORY        : TGUID = '{77f0ae69-c3bd-4509-941d-467e4d24899e}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_CATEGORY}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK   : TGUID = '{58f0aad8-22bf-4f8a-bb3d-d2c4978c6e2f}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK   : TGUID = '{98d24b5e-5930-4614-b5a1-f600f9355a78}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK}

  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_MAX_BUFFERS     : TGUID = '{7dd9b730-4f2d-41d5-8f95-0cc9a912ba26}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_MAX_BUFFERS}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID     : TGUID = '{30da9258-feb9-47a7-a453-763a7a8e1c5f}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ROLE            : TGUID = '{bc9d118e-8c67-4a18-85d4-12d300400552}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ROLE}
  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_PROVIDER_DEVICE_ID
  // Data type: STRING
  // Contains a device Id for a provider
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_PROVIDER_DEVICE_ID :	TGUID = '{36689d42-a06c-40ae-84cf-f5a034067cc4}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_PROVIDER_DEVICE_ID}


  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_XADDRESS
  // {BCA0BE52-C327-44C7-9B7D-7FA8D9B5BCDA}
  // Data type: STRING
  // Contains a XADDRESS of KSCATEGORY_NETWORK_CAMERA that
  // can be obtained from the PKEY_PNPX_XAddrs from a KSCATEGORY_NETWORK_CAMERA
  // or Web Services on Devices (WSD) xaddrs parameter, currently only supports ONVIF cameras.
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_XADDRESS :	TGUID = '{BCA0BE52-C327-44C7-9B7D-7FA8D9B5BCDA}';

  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_RTSP_URL
  // {9D7B40D2-3617-4043-93E3-8D6DA9BB3492}
  // Data type: STRING
  // Currently only RTSP urls are supported as an initialization parameter.
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_STREAM_URL :	TGUID = '{9D7B40D2-3617-4043-93E3-8D6DA9BB3492}';

  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_USERNAME
  // {05D01ADD-949F-46EB-BC8E-8B0D2B32D79D}
  // Data type: STRING
  // Attribute contains a username to use for authentication.
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_USERNAME :	TGUID = '{05D01ADD-949F-46EB-BC8E-8B0D2B32D79D}';

  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_PASSWORD
  // {A0FD7E16-42D9-49DF-84C0-E82C5EAB8874}
  // Data type: STRING
  // Attribute contains a password to use for authentication.
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_PASSWORD :	TGUID = '{A0FD7E16-42D9-49DF-84C0-E82C5EAB8874}';

  // {7A213AA7-866F-414A-8C1A-275C7283A395}
  // CLSID_FrameServerNetworkCameraSource
  // CLSID for KSCATEGORY_NETWORK_CAMERA Source that controls the MediaSource,
  // provides functionality to stream and control KSCATEGORY_NETWORK_CAMERA.
  // CLSID_FrameServerNetworkCameraSource is only supported using the Windows service FrameServer
  // and requires FrameServer for control and media flow, all other uses are not supported.
  CLSID_FrameServerNetworkCameraSource :	TGUID = '{7A213AA7-866F-414A-8C1A-275C7283A395}';

  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID            : TGUID = '{14dd9a1c-7cff-41be-b1b9-ba1ac6ecb571}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID}
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID            : TGUID = '{8ac3587a-4ae7-42d8-99e0-0a6013eef90f}';
  {$EXTERNALSYM MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID}

  MF_DEVICESTREAM_IMAGE_STREAM                              : TGUID = '{a7ffb865-e7b2-43b0-9f6f-9af2a0e50fc0}';
  {$EXTERNALSYM MF_DEVICESTREAM_IMAGE_STREAM}
  MF_DEVICESTREAM_INDEPENDENT_IMAGE_STREAM                  : TGUID = '{03eeec7e-d605-4576-8b29-6580b490d7d3}';
  {$EXTERNALSYM MF_DEVICESTREAM_INDEPENDENT_IMAGE_STREAM}
  MF_DEVICESTREAM_STREAM_ID                                 : TGUID = '{11bd5120-d124-446b-88e6-17060257fff9}';
  {$EXTERNALSYM MF_DEVICESTREAM_STREAM_ID}
  MF_DEVICESTREAM_STREAM_CATEGORY                           : TGUID = '{2939e7b8-a62e-4579-b674-d4073dfabbba}';
  {$EXTERNALSYM MF_DEVICESTREAM_STREAM_CATEGORY}

  // {1CB378E9-B279-41D4-AF97-34A243E68320}
  // MF_DEVICESTREAM_FRAMESERVER_SHARED
  // Data type: UINT32
  // this attribute, when set on a stream, explicitly marks the stream as shared by the frame server.
  MF_DEVICESTREAM_FRAMESERVER_SHARED                        :	TGUID = '{1CB378E9-B279-41D4-AF97-34A243E68320}';
  {$EXTERNALSYM MF_DEVICESTREAM_FRAMESERVER_SHARED}
  // MF_DEVICESTREAM_TRANSFORM_STREAM_ID
  // Data type: UINT32
  // This attribute represents the stream's MFT stream id
  // {e63937b7-daaf-4d49-815f-d826f8ad31e7}
  MF_DEVICESTREAM_TRANSFORM_STREAM_ID                       : TGUID = '{e63937b7-daaf-4d49-815f-d826f8ad31e7}';
  {$EXTERNALSYM MF_DEVICESTREAM_TRANSFORM_STREAM_ID}

  MF_DEVICESTREAM_EXTENSION_PLUGIN_CLSID                    : TGUID = '{048e6558-60c4-4173-bd5b-6a3ca2896aee}';
  {$EXTERNALSYM MF_DEVICESTREAM_EXTENSION_PLUGIN_CLSID}
  MF_DEVICEMFT_EXTENSION_PLUGIN_CLSID                       :	TGUID = '{844dbae0-34fa-48a0-a783-8e696fb1c9a8}';
  {$EXTERNALSYM MF_DEVICEMFT_EXTENSION_PLUGIN_CLSID}

  MF_DEVICESTREAM_EXTENSION_PLUGIN_CONNECTION_POINT         : TGUID = '{37f9375c-e664-4ea4-aae4-cb6d1daca1f4}';
  {$EXTERNALSYM MF_DEVICESTREAM_EXTENSION_PLUGIN_CONNECTION_POINT}
  MF_DEVICESTREAM_TAKEPHOTO_TRIGGER                         : TGUID = '{1d180e34-538c-4fbb-a75a-859af7d261a6}';
  {$EXTERNALSYM MF_DEVICESTREAM_TAKEPHOTO_TRIGGER}
  MF_DEVICESTREAM_MAX_FRAME_BUFFERS                         : TGUID = '{1684cebe-3175-4985-882c-0efd3e8ac11e}';
  {$EXTERNALSYM MF_DEVICESTREAM_MAX_FRAME_BUFFERS}

  MF_DEVICEMFT_CONNECTED_FILTER_KSCONTROL                   :	TGUID = '{6a2c4fa6-d179-41cd-9523-822371ea40e5}';
  {$EXTERNALSYM MF_DEVICEMFT_CONNECTED_FILTER_KSCONTROL}
  MF_DEVICEMFT_CONNECTED_PIN_KSCONTROL                      :	TGUID = '{e63310f7-b244-4ef8-9a7d-24c74e32ebd0}';
  {$EXTERNALSYM MF_DEVICEMFT_CONNECTED_PIN_KSCONTROL}
  MF_DEVICE_THERMAL_STATE_CHANGED                           :	TGUID = '{70ccd0af-fc9f-4deb-a875-9fecd16c5bd4}';
  {$EXTERNALSYM MF_DEVICE_THERMAL_STATE_CHANGED}
  MFSampleExtension_DeviceTimestamp                         : TGUID = '{8f3e35e7-2dcd-4887-8622-2a58baa652b0}';
  {$EXTERNALSYM MFSampleExtension_DeviceTimestamp}
  MFSampleExtension_Spatial_CameraViewTransform             :	TGUID = '{4e251fa4-830f-4770-859a-4b8d99aa809b}';
  {$EXTERNALSYM MFSampleExtension_Spatial_CameraViewTransform}
  MFSampleExtension_Spatial_CameraCoordinateSystem          :	TGUID = '{9d13c82f-2199-4e67-91cd-d1a4181f2534}';
  {$EXTERNALSYM MFSampleExtension_Spatial_CameraCoordinateSystem}
  MFSampleExtension_Spatial_CameraProjectionTransform       :	TGUID = '{47f9fcb5-2a02-4f26-a477-792fdf95886a}';
  {$EXTERNALSYM MFSampleExtension_Spatial_CameraProjectionTransform}


  // {40871C59-AB40-471F-8DC3-1F259D862479}
  CLSID_MPEG2ByteStreamPlugin                               : TGUID = '{40871C59-AB40-471F-8DC3-1F259D862479}';
  {$EXTERNALSYM CLSID_MPEG2ByteStreamPlugin}

  // {f09992f7-9fba-4c4a-a37f-8c47b4e1dfe7}
  MF_MEDIASOURCE_SERVICE                                    : TGUID = '{f09992f7-9fba-4c4a-a37f-8c47b4e1dfe7}';
  {$EXTERNALSYM MF_MEDIASOURCE_SERVICE}

  // {014A5031-2F05-4C6A-9F9C-7D0DC4EDA5F4}
  MF_ACCESS_CONTROLLED_MEDIASOURCE_SERVICE                  :	TGUID = '{14a50310-2f05-4c6a-9f9c-7d0dc4eda5f4}';
  {$EXTERNALSYM MF_ACCESS_CONTROLLED_MEDIASOURCE_SERVICE}


type

  PMFCONTENTPROTECTIONDEVICE_INPUT_DATA = ^_MFCONTENTPROTECTIONDEVICE_INPUT_DATA;
  _MFCONTENTPROTECTIONDEVICE_INPUT_DATA = record
    HWProtectionFunctionID: DWORD;
    PrivateDataByteCount: DWORD;
    HWProtectionDataByteCount: DWORD;
    Reserved: DWORD;
    InputData: array[0..3] of PByte;
  end;
  {$EXTERNALSYM _MFCONTENTPROTECTIONDEVICE_INPUT_DATA}
  MFCONTENTPROTECTIONDEVICE_INPUT_DATA = _MFCONTENTPROTECTIONDEVICE_INPUT_DATA;
  {$EXTERNALSYM MFCONTENTPROTECTIONDEVICE_INPUT_DATA}

  PMFCONTENTPROTECTIONDEVICE_OUTPUT_DATA = ^_MFCONTENTPROTECTIONDEVICE_OUTPUT_DATA;
  _MFCONTENTPROTECTIONDEVICE_OUTPUT_DATA = record
    PrivateDataByteCount: DWORD;
    MaxHWProtectionDataByteCount: DWORD;
    HWProtectionDataByteCount: DWORD;
    Status: HResult;
    TransportTimeInHundredsOfNanoseconds: LONGLONG;
    ExecutionTimeInHundredsOfNanoseconds: LONGLONG;
    OutputData: array[0..3] of PByte;
  end;
  {$EXTERNALSYM _MFCONTENTPROTECTIONDEVICE_OUTPUT_DATA}
  MFCONTENTPROTECTIONDEVICE_OUTPUT_DATA = _MFCONTENTPROTECTIONDEVICE_OUTPUT_DATA;
  {$EXTERNALSYM MFCONTENTPROTECTIONDEVICE_OUTPUT_DATA}

const

  MFCONTENTPROTECTIONDEVICE_FUNCTIONID_START = $04000000;
  {$EXTERNALSYM MFCONTENTPROTECTIONDEVICE_FUNCTIONID_START}
  MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA_FUNCTIONID = $04000000; // hardcode
  {$EXTERNALSYM MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA_FUNCTIONID}

type

  PMFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA = ^_MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA;
  _MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA = record
    TaskIndex: DWORD;
    ClassName: array[0..MAX_PATH - 1] of LPWSTR;
    BasePriority: LONG;
  end;
  {$EXTERNALSYM _MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA}
  MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA = _MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA;
  {$EXTERNALSYM MFCONTENTPROTECTIONDEVICE_REALTIMECLIENT_DATA}




  // [local]
  // MFPROTECTION_DISABLE schema data
  //
  //  Schema data is a 32 bit value defined as follows:
  //
  //   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  //   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  //  +-------------------------------------------------------------+-+
  //  |                         Reserved                            |S|
  //  +-------------------------------------------------------------+-+
  //
  //  where
  //
  //      Reserved - must be zero
  //
  //      S - Status
  //          0 = off, i.e. do not disable
  //          1 = on, i.e. disable
  //
  //  The MAKE macro is useful to ITA policy objects for building the DWORD
  //  schema data. Note it does not validate the parameters.
  //
  //  The EXTRACT macros are normally useful only to implementors of this type of
  //  protection for breaking out parameters from the DWORD schema data.
  //  Extracting the Reserved data is useful only to verify that it is zero.
  //
  //  Source:  T. Forsberg, tforsberg@gmail.com
  //

const

  MF_RATE_CONTROL_SERVICE                       : TGUID = '{866fa297-b802-4bf8-9dc9-5e3b6a9f53c9}';
  {$EXTERNALSYM MF_RATE_CONTROL_SERVICE}
  MR_STREAM_VOLUME_SERVICE                      : TGUID = '{f8b5fa2f-32ef-46f5-b172-1321212fb2c4}';
  {$EXTERNALSYM MR_STREAM_VOLUME_SERVICE}

  MFNETSOURCE_HTTP_DOWNLOAD_SESSION_PROVIDER    : TGUID = '{7d55081e-307d-4d6d-a663-a93be97c4b5c}';
  {$EXTERNALSYM MFNETSOURCE_HTTP_DOWNLOAD_SESSION_PROVIDER}

  MF_UNKNOWN_DURATION                 = 0;
  {$EXTERNALSYM MF_UNKNOWN_DURATION}

  MF_SD_VIDEO_SPHERICAL                         : TGUID =  '{a51da449-3fdc-478c-bcb5-30be76595f55}';
  {$EXTERNALSYM MF_SD_VIDEO_SPHERICAL}
  MF_SD_VIDEO_SPHERICAL_FORMAT                  : TGUID =  '{4a8fc407-6ea1-46c8-b567-6971d4a139c3}';
  {$EXTERNALSYM MF_SD_VIDEO_SPHERICAL_FORMAT}
  MF_SD_VIDEO_SPHERICAL_INITIAL_VIEWDIRECTION   : TGUID =  '{11d25a49-bb62-467f-9db1-c17165716c49}';
  {$EXTERNALSYM MF_SD_VIDEO_SPHERICAL_INITIAL_VIEWDIRECTION}

//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)

  MF_ST_MEDIASOURCE_COLLECTION          : TGUID = '{616DE972-83AD-4950-8170-630D19CBE307}';
  {$EXTERNALSYM MF_ST_MEDIASOURCE_COLLECTION}
  MF_DEVICESTREAM_FILTER_KSCONTROL      : TGUID = '{46783CCA-3DF5-4923-A9EF-36B7223EDDE0}';
  {$EXTERNALSYM MF_DEVICESTREAM_FILTER_KSCONTROL}
  MF_DEVICESTREAM_PIN_KSCONTROL         : TGUID = '{EF3EF9A7-87F2-48CA-BE02-674878918E98}';
  {$EXTERNALSYM MF_DEVICESTREAM_PIN_KSCONTROL}
  MF_DEVICESTREAM_SOURCE_ATTRIBUTES     : TGUID = '{2F8CB617-361B-434F-85EA-99A03E1CE4E0}';
  {$EXTERNALSYM MF_DEVICESTREAM_SOURCE_ATTRIBUTES}
  MF_DEVICESTREAM_FRAMESERVER_HIDDEN    : TGUID = '{F402567B-4D91-4179-96D1-74C8480C2034}';
  {$EXTERNALSYM MF_DEVICESTREAM_FRAMESERVER_HIDDEN}
  MF_STF_VERSION_INFO                   : TGUID = '{6770BD39-EF82-44EE-A49B-934BEB24AEF7}';
  {$EXTERNALSYM MF_STF_VERSION_INFO}
  MF_STF_VERSION_DATE                   : TGUID = '{31A165D5-DF67-4095-8E44-8868FC20DBFD}';
  {$EXTERNALSYM MF_STF_VERSION_DATE}
  MF_DEVICESTREAM_REQUIRED_CAPABILITIES : TGUID = '{6D8B957E-7CF6-43F4-AF56-9C0E1E4FCBE1}';
  {$EXTERNALSYM MF_DEVICESTREAM_REQUIRED_CAPABILITIES}
  MF_DEVICESTREAM_REQUIRED_SDDL         : TGUID = '{331AE85D-C0D3-49BA-83BA-82A12D63CDD6}';
  {$EXTERNALSYM MF_DEVICESTREAM_REQUIRED_SDDL}
//#end if

  MF_XVP_SAMPLE_LOCK_TIMEOUT            : TGUID = '{aa4ddb29-5134-4363-ac72-83ec4bc10426}';
  {$EXTERNALSYM MF_XVP_SAMPLE_LOCK_TIMEOUT}

type

  // ActiveX pointers
  // ================

{$IFNDEF _PIStream_DEFINED}
  PIStream = ^IStream;
{$DEFINE _PIStream_DEFINED}
{$ENDIF}

  // MFTIME
  // ======
  // DECLARED IN MfpTypes


//--------------------- Types and records ------------------------------------

  // MFMEDIASOURCE_CHARACTERISTICS
  // Defines the characteristics of a media source.
  // These flags are retrieved by the IMFMediaSource.GetCharacteristics method.

  PMFMEDIASOURCE_CHARACTERISTICS = ^MFMEDIASOURCE_CHARACTERISTICS;
  _MFMEDIASOURCE_CHARACTERISTICS = type DWord;
  {$EXTERNALSYM _MFMEDIASOURCE_CHARACTERISTICS}
  MFMEDIASOURCE_CHARACTERISTICS = _MFMEDIASOURCE_CHARACTERISTICS;
  {$EXTERNALSYM MFMEDIASOURCE_CHARACTERISTICS}
const
  MFMEDIASOURCE_IS_LIVE                    = MFMEDIASOURCE_CHARACTERISTICS($1);
  {$EXTERNALSYM MFMEDIASOURCE_IS_LIVE}
  MFMEDIASOURCE_CAN_SEEK                   = MFMEDIASOURCE_CHARACTERISTICS($2);
  {$EXTERNALSYM MFMEDIASOURCE_CAN_SEEK}
  MFMEDIASOURCE_CAN_PAUSE                  = MFMEDIASOURCE_CHARACTERISTICS($4);
  {$EXTERNALSYM MFMEDIASOURCE_CAN_PAUSE}
  MFMEDIASOURCE_HAS_SLOW_SEEK              = MFMEDIASOURCE_CHARACTERISTICS($8);
  {$EXTERNALSYM MFMEDIASOURCE_HAS_SLOW_SEEK}
  MFMEDIASOURCE_HAS_MULTIPLE_PRESENTATIONS = MFMEDIASOURCE_CHARACTERISTICS($10);    // >= Win 7
  {$EXTERNALSYM MFMEDIASOURCE_HAS_MULTIPLE_PRESENTATIONS}
  MFMEDIASOURCE_CAN_SKIPFORWARD            = MFMEDIASOURCE_CHARACTERISTICS($20);    // >= Win 7
  {$EXTERNALSYM MFMEDIASOURCE_CAN_SKIPFORWARD}
  MFMEDIASOURCE_CAN_SKIPBACKWARD           = MFMEDIASOURCE_CHARACTERISTICS($40);    // >= Win 7
  {$EXTERNALSYM MFMEDIASOURCE_CAN_SKIPBACKWARD}
  MFMEDIASOURCE_DOES_NOT_USE_NETWORK       = MFMEDIASOURCE_CHARACTERISTICS($80);    // >= Win 8
  {$EXTERNALSYM MFMEDIASOURCE_DOES_NOT_USE_NETWORK}

  // MFCLOCK_RELATIONAL_FLAGS
  // Jitter values are always negative.
  // In other words, the time returned by IMFClock.GetCorrelatedTime might jitter behind the actual clock time,
  // but will never jitter ahead of the actual time.
  // If this flag is not present, the clock might jitter in either direction.
type
  PMFCLOCK_RELATIONAL_FLAGS = ^_MFCLOCK_RELATIONAL_FLAGS;
  _MFCLOCK_RELATIONAL_FLAGS = DWord;
  {$EXTERNALSYM _MFCLOCK_RELATIONAL_FLAGS}
  MFCLOCK_RELATIONAL_FLAGS = _MFCLOCK_RELATIONAL_FLAGS;
  {$EXTERNALSYM MFCLOCK_RELATIONAL_FLAGS}
const
  MFCLOCK_RELATIONAL_FLAG_JITTER_NEVER_AHEAD = MFCLOCK_RELATIONAL_FLAGS($1);
  {$EXTERNALSYM MFCLOCK_RELATIONAL_FLAG_JITTER_NEVER_AHEAD}

type
  PMFCLOCK_CHARACTERISTICS_FLAGS = ^MFCLOCK_CHARACTERISTICS_FLAGS;
  PMfClockCharacteristicsFlags = ^MFCLOCK_CHARACTERISTICS_FLAGS;
  _MFCLOCK_CHARACTERISTICS_FLAGS = DWord;
  {$EXTERNALSYM _MFCLOCK_CHARACTERISTICS_FLAGS}
  MFCLOCK_CHARACTERISTICS_FLAGS = _MFCLOCK_CHARACTERISTICS_FLAGS;
  {$EXTERNALSYM MFCLOCK_CHARACTERISTICS_FLAGS}
const
  MFCLOCK_CHARACTERISTICS_FLAG_FREQUENCY_10MHZ = MFCLOCK_CHARACTERISTICS_FLAGS($2);
  {$EXTERNALSYM MFCLOCK_CHARACTERISTICS_FLAG_FREQUENCY_10MHZ}
  MFCLOCK_CHARACTERISTICS_FLAG_ALWAYS_RUNNING  = MFCLOCK_CHARACTERISTICS_FLAGS($4);
  {$EXTERNALSYM MFCLOCK_CHARACTERISTICS_FLAG_ALWAYS_RUNNING}
  MFCLOCK_CHARACTERISTICS_FLAG_IS_SYSTEM_CLOCK = MFCLOCK_CHARACTERISTICS_FLAGS($8);
  {$EXTERNALSYM MFCLOCK_CHARACTERISTICS_FLAG_IS_SYSTEM_CLOCK}

type
  PMFSESSION_SETTOPOLOGY_FLAGS = ^MFSESSION_SETTOPOLOGY_FLAGS;
  MFSESSION_SETTOPOLOGY_FLAGS = DWord;
  {$EXTERNALSYM MFSESSION_SETTOPOLOGY_FLAGS}
const
    MFSESSION_SETTOPOLOGY_IMMEDIATE     = MFSESSION_SETTOPOLOGY_FLAGS($1);
    {$EXTERNALSYM MFSESSION_SETTOPOLOGY_IMMEDIATE}
    MFSESSION_SETTOPOLOGY_NORESOLUTION  = MFSESSION_SETTOPOLOGY_FLAGS($2);
    {$EXTERNALSYM MFSESSION_SETTOPOLOGY_NORESOLUTION}
    MFSESSION_SETTOPOLOGY_CLEAR_CURRENT = MFSESSION_SETTOPOLOGY_FLAGS($4);
    {$EXTERNALSYM MFSESSION_SETTOPOLOGY_CLEAR_CURRENT}

type
  PMFSESSION_GETFULLTOPOLOGY_FLAG = ^MFSESSION_GETFULLTOPOLOGY_FLAG;
  MFSESSION_GETFULLTOPOLOGY_FLAG = DWord;
  {$EXTERNALSYM MFSESSION_GETFULLTOPOLOGY_FLAG}
const
  MFSESSION_GETFULLTOPOLOGY_CURRENT = MFSESSION_GETFULLTOPOLOGY_FLAG($1);
  {$EXTERNALSYM MFSESSION_GETFULLTOPOLOGY_CURRENT}


type
  PMFPMPSESSION_CREATION_FLAGS = ^MFPMPSESSION_CREATION_FLAGS;
  MFPMPSESSION_CREATION_FLAGS = DWord;
  {$EXTERNALSYM MFPMPSESSION_CREATION_FLAGS}
const
    MFPMPSESSION_UNPROTECTED_PROCESS = MFPMPSESSION_CREATION_FLAGS($1);
    {$EXTERNALSYM MFPMPSESSION_UNPROTECTED_PROCESS}
    MFPMPSESSION_IN_PROCESS	         = MFPMPSESSION_CREATION_FLAGS($2);
    {$EXTERNALSYM MFPMPSESSION_IN_PROCESS}

//#if (WINVER >= _WIN32_WINNT_WIN7)
type
  PMF_QUALITY_ADVISE_FLAGS = ^MF_QUALITY_ADVISE_FLAGS;
  PMfQualityAdviseFlags = ^MF_QUALITY_ADVISE_FLAGS;
  _MF_QUALITY_ADVISE_FLAGS    = DWord;
  {$EXTERNALSYM _MF_QUALITY_ADVISE_FLAGS}
  MF_QUALITY_ADVISE_FLAGS = _MF_QUALITY_ADVISE_FLAGS;
  {$EXTERNALSYM MF_QUALITY_ADVISE_FLAGS}
const
  MF_QUALITY_CANNOT_KEEP_UP = MF_QUALITY_ADVISE_FLAGS($1);
  {$EXTERNALSYM MF_QUALITY_CANNOT_KEEP_UP}
//#endif // (WINVER >= _WIN32_WINNT_WIN7)


type
  PMFSequencerTopologyFlags = ^MFSequencerTopologyFlags;
  _MFSequencerTopologyFlags     = DWord;
  {$EXTERNALSYM _MFSequencerTopologyFlags}
  MFSequencerTopologyFlags = _MFSequencerTopologyFlags;
  {$EXTERNALSYM MFSequencerTopologyFlags}
const
    SequencerTopologyFlags_Last = MFSequencerTopologyFlags($1);
    {$EXTERNALSYM SequencerTopologyFlags_Last}


  // IMFNetCredentialCache
  ////////////////////////

type
  PMFNetCredentialRequirements = ^MFNetCredentialRequirements;
  _MFNetCredentialRequirements = DWord;
  {$EXTERNALSYM _MFNetCredentialRequirements}
  MFNetCredentialRequirements = _MFNetCredentialRequirements;
  {$EXTERNALSYM MFNetCredentialRequirements}
const
    REQUIRE_PROMPT        = MFNetCredentialRequirements($1);
    {$EXTERNALSYM REQUIRE_PROMPT}
    REQUIRE_SAVE_SELECTED = MFNetCredentialRequirements($2);
    {$EXTERNALSYM REQUIRE_SAVE_SELECTED}


type
  PMFNetCredentialOptions = ^MFNetCredentialOptions;
  _MFNetCredentialOptions = DWord;
  {$EXTERNALSYM _MFNetCredentialOptions}
  MFNetCredentialOptions = _MFNetCredentialOptions;
  {$EXTERNALSYM MFNetCredentialOptions}
const
  MFNET_CREDENTIAL_SAVE             = MFNetCredentialOptions($1);
  {$EXTERNALSYM MFNET_CREDENTIAL_SAVE}
  MFNET_CREDENTIAL_DONT_CACHE       = MFNetCredentialOptions($2);
  {$EXTERNALSYM MFNET_CREDENTIAL_DONT_CACHE}
  MFNET_CREDENTIAL_ALLOW_CLEAR_TEXT = MFNetCredentialOptions($4);
  {$EXTERNALSYM MFNET_CREDENTIAL_ALLOW_CLEAR_TEXT}


type
  PMFNetAuthenticationFlags = ^MFNetAuthenticationFlags;
  _MFNetAuthenticationFlags = DWord;
  {$EXTERNALSYM _MFNetAuthenticationFlags}
  MFNetAuthenticationFlags = _MFNetAuthenticationFlags;
  {$EXTERNALSYM MFNetAuthenticationFlags}
const
  MFNET_AUTHENTICATION_PROXY          = MFNetAuthenticationFlags($1);
  {$EXTERNALSYM MFNET_AUTHENTICATION_PROXY}
  MFNET_AUTHENTICATION_CLEAR_TEXT     = MFNetAuthenticationFlags($2);
  {$EXTERNALSYM MFNET_AUTHENTICATION_CLEAR_TEXT}
  MFNET_AUTHENTICATION_LOGGED_ON_USER = MFNetAuthenticationFlags($4);
  {$EXTERNALSYM MFNET_AUTHENTICATION_LOGGED_ON_USER}


  // MF_RESOLUTION
  // =============

type
  PMF_CONNECT_METHOD = ^MF_CONNECT_METHOD;
  _MF_CONNECT_METHOD = UINT32;
  {$EXTERNALSYM _MF_CONNECT_METHOD}
  MF_CONNECT_METHOD = _MF_CONNECT_METHOD;
  {$EXTERNALSYM MF_CONNECT_METHOD}
const
    MF_CONNECT_DIRECT                          = MF_CONNECT_METHOD(0);
    {$EXTERNALSYM MF_CONNECT_DIRECT}
    MF_CONNECT_ALLOW_CONVERTER                 = MF_CONNECT_METHOD($1);
    {$EXTERNALSYM MF_CONNECT_ALLOW_CONVERTER}
    MF_CONNECT_ALLOW_DECODER                   = MF_CONNECT_METHOD($3);
    {$EXTERNALSYM MF_CONNECT_ALLOW_DECODER}
    MF_CONNECT_RESOLVE_INDEPENDENT_OUTPUTTYPES = MF_CONNECT_METHOD($4);
    {$EXTERNALSYM MF_CONNECT_RESOLVE_INDEPENDENT_OUTPUTTYPES}
    MF_CONNECT_AS_OPTIONAL                     = MF_CONNECT_METHOD($10000);
    {$EXTERNALSYM MF_CONNECT_AS_OPTIONAL}
    MF_CONNECT_AS_OPTIONAL_BRANCH              = MF_CONNECT_METHOD($20000);
    {$EXTERNALSYM MF_CONNECT_AS_OPTIONAL_BRANCH}

type
  PMF_TOPOLOGY_RESOLUTION_STATUS_FLAGS = ^MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS;
  _MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS = UINT32;
  {$EXTERNALSYM _MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS}
  MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS = _MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS;
  {$EXTERNALSYM MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS}
const
    MF_TOPOLOGY_RESOLUTION_SUCCEEDED            = MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS(0);
    {$EXTERNALSYM MF_TOPOLOGY_RESOLUTION_SUCCEEDED}
    MF_OPTIONAL_NODE_REJECTED_MEDIA_TYPE        = MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS($1);
    {$EXTERNALSYM MF_OPTIONAL_NODE_REJECTED_MEDIA_TYPE}
    MF_OPTIONAL_NODE_REJECTED_PROTECTED_PROCESS = MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS($2);
    {$EXTERNALSYM MF_OPTIONAL_NODE_REJECTED_PROTECTED_PROCESS}

//




type
  // typedef unsigned __int64 TOPOID; >> MfpTypes

  // Forward interface definitions

  IMFTopology = interface;
  IMFClock = interface;
  IMFPresentationDescriptor = interface;
  IMFPresentationClock = interface;
  PIMFStreamDescriptor = ^IMFStreamDescriptor;
  IMFStreamDescriptor = interface;
  IMFStreamSink = interface;
  IMFMediaTypeHandler = interface;
  IMFTopologyNode = interface;
  IMFPresentationTimeSource = interface;
  IMFClockStateSink = interface;
  IMFOutputPolicy = interface;
  IMFOutputTrustAuthority = interface;
  PIMFHttpDownloadRequest = ^IMFHttpDownloadRequest;
  IMFHttpDownloadRequest = interface;
  PIMFHttpDownloadSession = ^IMFHttpDownloadSession;
  IMFHttpDownloadSession = interface;

  // Interface IMFMediaSession
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSession);'}
  {$EXTERNALSYM IMFMediaSession}
  IMFMediaSession = interface(IMFMediaEventGenerator)
  ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function SetTopology(const dwSetTopologyFlags: MFSESSION_SETTOPOLOGY_FLAGS;
                         Topology: IMFTopology): HResult; stdcall;

    // When the topologies are deleted, the assiociated callback (referencecount will be
    // set to 0) and will be released.
    // So, if the sessionmanager's topology contains a callbackinterface,
    // don't forget to call this function to prevent pointer errors.
    //
    // A normal session-end where all resources, references and queued presentations are cleared,
    // would go like this to make sure no memory leaks will be created.
    //
    //  1 MySession.Stop()      This method is asynchronous. When the operation completes, the Media Session sends an MESessionStopped event.
    //  2 MySession.ClearTopologies()  See comments on ClearTopologies.
    //  3 MySession.Shutdown()  See comments on ShutDown.
    //  4 MySession := Nil or SafeRelease(MySession)  Calls IUnknown._Release.
    //
    function ClearTopologies(): HResult; stdcall; // Clears all of the presentations that are queued for playback in the Media Session.

    function Start(const pguidTimeFormat: TGUID;
                   const pvarStartPosition: PROPVARIANT): HResult; stdcall;

    function Pause(): HResult; stdcall;

    function Stop(): HResult; stdcall;

    function Close(): HResult; stdcall;  // Closes the Media Session and releases all of the resources it is using.

    function Shutdown(): HResult; stdcall;  // Shuts down the Media Session and releases all the resources used by the Media Session.
                                            // Call this method when you are done using the Media Session, before the final call to IUnknown._Release.
                                            // Otherwise, your application will leak memory.
    function GetClock(out ppClock: IMFClock): HResult; stdcall;

    function GetSessionCapabilities(out pdwCaps: DWord): HResult; stdcall;

    function GetFullTopology(const dwGetFullTopologyFlags: DWord;
                             const TopId: TOPOID;
                             out ppFullTopology: IMFTopology): HResult; stdcall;

  end;
  IID_IMFMediaSession = IMFMediaSession;
  {$EXTERNALSYM IID_IMFMediaSession}


  PMfObjectType = ^MF_OBJECT_TYPE;
  PMF_OBJECT_TYPE = ^MF_OBJECT_TYPE;
  MF_OBJECT_TYPE          = (
    MF_OBJECT_MEDIASOURCE = 0,
    MF_OBJECT_BYTESTREAM  = (MF_OBJECT_MEDIASOURCE + 1),
    MF_OBJECT_INVALID     = (MF_OBJECT_BYTESTREAM + 1)
  );
  {$EXTERNALSYM MF_OBJECT_TYPE}


  // interface IMFSourceResolver
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceResolver);'}
  {$EXTERNALSYM IMFSourceResolver}
  IMFSourceResolver = interface(IUnknown)
    ['{FBE5A32D-A497-4b61-BB85-97B1A848A6E3}']

      function CreateObjectFromURL(const pwszURL: LPCWSTR;
                                   dwFlags: DWord;
                                   pProps: IPropertyStore; // can be nil
                                   var pObjectType: MF_OBJECT_TYPE;
                                   out ppObject: IUnknown): HResult; stdcall;

      function CreateObjectFromByteStream(pByteStream: IMFByteStream;
                                          const pwszURL: LPCWSTR; // can be nil
                                          dwFlags: DWord;
                                          pProps: IPropertyStore; // can be nil
                                          var pObjectType: MF_OBJECT_TYPE;
                                          out ppObject: IUnknown): HResult; stdcall;

      function BeginCreateObjectFromURL(const pwszURL: LPCWSTR;
                                        dwFlags: DWord;
                                        pProps: IPropertyStore; // can be nil
                                        var ppIUnknownCancelCookie: IUnknown;
                                        pCallback: IMFAsyncCallback;
                                        punkState: IUnknown): HResult; stdcall;

      function EndCreateObjectFromURL(pResult: IMFAsyncResult;
                                      out pObjectType: MF_OBJECT_TYPE;
                                      out ppObject: IUnknown): HResult; stdcall;

      function BeginCreateObjectFromByteStream(pByteStream: IMFByteStream;
                                               const pwszURL: LPCWSTR;
                                               dwFlags: DWord;
                                               pProps: IPropertyStore; // can be nil
                                               out ppIUnknownCancelCookie: IUnknown;
                                               pCallback: IMFAsyncCallback;
                                               punkState: IUnknown): HResult; stdcall;

      function EndCreateObjectFromByteStream(pResult: IMFAsyncResult;
                                             out pObjectType: MF_OBJECT_TYPE;
                                             out ppObject: IUnknown): HResult; stdcall;

      function CancelObjectCreation(pIUnknownCancelCookie: IUnknown): HResult; stdcall;
  end;
  IID_IMFSourceResolver = IMFSourceResolver;
  {$EXTERNALSYM IID_IMFSourceResolver}




  // Interface IMFMediaSource
  // ========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSource);'}
  {$EXTERNALSYM IMFMediaSource}
  IMFMediaSource = interface(IMFMediaEventGenerator)
    ['{3C9B2EB9-86D5-4514-A394-F56664F9F0D8}']

      function GetCharacteristics(out pdwCharacteristics: PDWord): HResult; stdcall;
      // Receives a bitwise OR of zero or more flags from the MFMEDIASOURCE_CHARACTERISTICS enumeration.

      function CreatePresentationDescriptor(out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;

      function Start(pPresentationDescriptor: IMFPresentationDescriptor;
                     const pguidTimeFormat: TGuid;
                     const pvarStartPosition: PROPVARIANT): HResult; stdcall;

      function Stop(): HResult; stdcall;

      function Pause(): HResult; stdcall;

      function Shutdown(): HResult; stdcall;

  end;
  IID_IMFMediaSource = IMFMediaSource;
  {$EXTERNALSYM IID_IMFMediaSource}

//#if (WINVER >= _WIN32_WINNT_WIN8)
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourceEx);'}
  {$EXTERNALSYM IMFMediaSourceEx}
  // Interface IMFMediaSourceEx
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourceEx);'}
  {$EXTERNALSYM IMFMediaSourceEx}
  IMFMediaSourceEx = interface(IMFMediaSource)
    ['{3C9B2EB9-86D5-4514-A394-F56664F9F0D8}']

      function GetSourceAttributes(out ppAttributes: IMFAttributes): HResult; stdcall;

      function GetStreamAttributes(const dwStreamIdentifier: DWORD;
                                   out ppAttributes: IMFAttributes): HResult; stdcall;

      function SetD3DManager(var pManager: IUnknown): HResult; stdcall;

   end;
  IID_IMFMediaSourceEx = IMFMediaSourceEx;
  {$EXTERNALSYM IID_IMFMediaSourceEx}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)


  // Interface IMFClockConsumer
  //===========================
  // Implemented by an app in order to get access to the IMFPresentationClock.
  //
  // The media pipeline checks for the presence of this interface by calling QueryInterface.
  // Components can use the presentation clock supplied through this interface to determine how much buffering there
  // is in the pipeline after the component.
  // You can do this in the IMFTransform.ProcessInput method by calculating the difference between the
  // value returned by IMFPresentationClock.GetTime and the value returned by IMFSample.GetSampleTime.
  // This difference represents the amount of buffered data after the MFT in the pipeline.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFClockConsumer);'}
  {$EXTERNALSYM IMFClockConsumer}
  IMFClockConsumer = interface(IUnknown)
    ['{6ef2a662-47c0-4666-b13d-cbb717f2fa2c}']

      function SetPresentationClock(pPresentationClock: IMFPresentationClock): HResult; stdcall;
      // Called by the media pipeline to get an instance of IMFPresentationClock.

      function GetPresentationClock(out ppPresentationClock: IMFPresentationClock): HResult; stdcall;
      // Called by the media pipeline to provide the app with an instance of IMFPresentationClock.
  end;
  IID_IMFClockConsumer = IMFClockConsumer;
  {$EXTERNALSYM IID_IMFClockConsumer}


  // Interface IMFMediaStream
  // ========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaStream);'}
  {$EXTERNALSYM IMFMediaStream}
  IMFMediaStream = interface(IMFMediaEventGenerator)
  ['{D182108F-4EC6-443f-AA42-A71106EC825F}']

    function GetMediaSource(out ppMediaSource: IMFMediaSource): HResult; stdcall;

    function GetStreamDescriptor(out ppStreamDescriptor: IMFStreamDescriptor): HResult; stdcall;

    function RequestSample(pToken: IUnknown): HResult; stdcall;

  end;
  IID_IMFMediaStream = IMFMediaStream;
  {$EXTERNALSYM IID_IMFMediaStream}


  // Interface IMFMediaSink
  // ======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSink);'}
  {$EXTERNALSYM IMFMediaSink}
  IMFMediaSink = interface(IUnknown)
  ['{6ef2a660-47c0-4666-b13d-cbb717f2fa2c}']

    function GetCharacteristics(out pdwCharacteristics: PDWord): HResult; stdcall;

    function AddStreamSink(dwStreamSinkIdentifier: DWord;
                           pMediaType: IMFMediaType;
                           out ppStreamSink: IMFStreamSink): HResult; stdcall;

    function RemoveStreamSink(dwStreamSinkIdentifier: DWord): HResult; stdcall;

    function GetStreamSinkCount(out pcStreamSinkCount: DWord): HResult; stdcall;

    function GetStreamSinkByIndex(dwIndex: DWord;
                                  out ppStreamSink: IMFStreamSink): HResult; stdcall;

    function GetStreamSinkById(dwStreamSinkIdentifier: DWord;
                               out ppStreamSink: IMFStreamSink): HResult; stdcall;

    function SetPresentationClock(pPresentationClock: IMFPresentationClock): HResult; stdcall;

    function GetPresentationClock(out ppPresentationClock: IMFPresentationClock): HResult; stdcall;

    function Shutdown: HResult; stdcall;

  end;
  IID_IMFMediaSink = IMFMediaSink;
  {$EXTERNALSYM IID_IMFMediaSink}


  PMFSTREAMSINK_MARKER_TYPE = ^MFSTREAMSINK_MARKER_TYPE;
  _MFSTREAMSINK_MARKER_TYPE          = (
    MFSTREAMSINK_MARKER_DEFAULT      = 0,
    MFSTREAMSINK_MARKER_ENDOFSEGMENT = (MFSTREAMSINK_MARKER_DEFAULT  + 1),
    MFSTREAMSINK_MARKER_TICK         = (MFSTREAMSINK_MARKER_ENDOFSEGMENT  + 1),
    MFSTREAMSINK_MARKER_EVENT        = (MFSTREAMSINK_MARKER_TICK  + 1)
  );
  {$EXTERNALSYM _MFSTREAMSINK_MARKER_TYPE}
  MFSTREAMSINK_MARKER_TYPE = _MFSTREAMSINK_MARKER_TYPE;
  {$EXTERNALSYM MFSTREAMSINK_MARKER_TYPE}

  // Interface IMFStreamSink
  // =======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFStreamSink);'}
  {$EXTERNALSYM IMFStreamSink}
  IMFStreamSink = interface(IMFMediaEventGenerator)
  ['{0A97B3CF-8E7C-4a3d-8F8C-0C843DC247FB}']
    function GetMediaSink(out ppMediaSink: IMFMediaSink): HResult; stdcall;

    function GetIdentifier(out pdwIdentifier: DWord): HResult; stdcall;

    function GetMediaTypeHandler(out ppHandler: IMFMediaTypeHandler): HResult; stdcall;

    function ProcessSample(pSample: IMFSample): HResult; stdcall;

    function PlaceMarker(const eMarkerType: MFSTREAMSINK_MARKER_TYPE;
                         const pvarMarkerValue: PROPVARIANT;
                         const pvarContextValue: PROPVARIANT): HResult; stdcall;

    function Flush(): HResult; stdcall;

  end;
  IID_IMFStreamSink = IMFStreamSink;
  {$EXTERNALSYM IID_IMFStreamSink}


  // Interface IMFVideoSampleAllocator
  // =================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoSampleAllocator);'}
  {$EXTERNALSYM IMFVideoSampleAllocator}
  IMFVideoSampleAllocator = interface(IUnknown)
  ['{86cbc910-e533-4751-8e3b-f19b5b806a03}']

    function SetDirectXManager(pManager: IUnknown): HResult; stdcall;

    function UninitializeSampleAllocator(): HResult; stdcall;

    function InitializeSampleAllocator(cRequestedFrames: DWord;
                                       pMediaType: IMFMediaType): HResult; stdcall;

    function AllocateSample(out ppSample: IMFSample): HResult; stdcall;

  end;
  IID_IMFVideoSampleAllocator = IMFVideoSampleAllocator;
  {$EXTERNALSYM IID_IMFVideoSampleAllocator}


  // Interface IMFVideoSampleAllocatorNotify
  // =======================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoSampleAllocatorNotify);'}
  {$EXTERNALSYM IMFVideoSampleAllocatorNotify}
  IMFVideoSampleAllocatorNotify = interface(IUnknown)
  ['{A792CDBE-C374-4e89-8335-278E7B9956A4}']

    function NotifyRelease(): HResult; stdcall;

  end;
  IID_IMFVideoSampleAllocatorNotify = IMFVideoSampleAllocatorNotify;
  {$EXTERNALSYM IID_IMFVideoSampleAllocatorNotify}


  // Interface IMFVideoSampleAllocatorNotifyEx
  // =========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoSampleAllocatorNotifyEx);'}
  {$EXTERNALSYM IMFVideoSampleAllocatorNotifyEx}
  IMFVideoSampleAllocatorNotifyEx = interface(IMFVideoSampleAllocatorNotify)
  ['{3978AA1A-6D5B-4B7F-A340-90899189AE34}']

    function NotifyPrune(sample: IMFSample): HResult; stdcall;

  end;
  IID_IMFVideoSampleAllocatorNotifyEx = IMFVideoSampleAllocatorNotifyEx;
  {$EXTERNALSYM IID_IMFVideoSampleAllocatorNotifyEx}



  // Interface IMFVideoSampleAllocatorCallback
  // =========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoSampleAllocatorCallback);'}
  {$EXTERNALSYM IMFVideoSampleAllocatorCallback}
  IMFVideoSampleAllocatorCallback = interface(IUnknown)
  ['{992388B4-3372-4f67-8B6F-C84C071F4751}']

    function SetCallback(pNotify: IMFVideoSampleAllocatorNotify): HResult; stdcall;

    function GetFreeSampleCount(out plSamples: LONG): HResult; stdcall;

  end;
  IID_IMFVideoSampleAllocatorCallback = IMFVideoSampleAllocatorCallback;
  {$EXTERNALSYM IID_IMFVideoSampleAllocatorCallback}


  // Interface IMFVideoSampleAllocatorEx
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoSampleAllocatorEx);'}
  {$EXTERNALSYM IMFVideoSampleAllocatorEx}
  IMFVideoSampleAllocatorEx = interface(IMFVideoSampleAllocator)
  ['{545b3a48-3283-4f62-866f-a62d8f598f9f}']
    function InitializeSampleAllocatorEx(cInitialSamples: DWORD;
                                         cMaximumSamples: DWORD;
                                         pAttributes: IMFAttributes;
                                         pMediaType: IMFMediaType): HResult; stdcall;

  end;
  IID_IMFVideoSampleAllocatorEx = IMFVideoSampleAllocatorEx;
  {$EXTERNALSYM IID_IMFVideoSampleAllocatorEx}

//#if (WINVER >= _WIN32_WINNT_WINBLUE) // = WIN10

  // Interface IMFDXGIDeviceManagerSource
  // ====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDXGIDeviceManagerSource);'}
  {$EXTERNALSYM IMFDXGIDeviceManagerSource}
  IMFDXGIDeviceManagerSource = interface(IUnknown)
  ['{20bc074b-7a8d-4609-8c3b-64a0a3b5d7ce}']

    function GetManager(out ppManager: IMFDXGIDeviceManager): HResult; stdcall;

  end;
  IID_IMFDXGIDeviceManagerSource = IMFDXGIDeviceManagerSource;
  {$EXTERNALSYM IID_IMFDXGIDeviceManagerSource}

//#endif // (WINVER >= _WIN32_WINNT_WINBLUE) // = WIN10

//#if (WINVER >= _WIN32_WINNT_WIN8)

  // Specifies how to rotate a video image.
  PMF_VIDEO_PROCESSOR_ROTATION = ^MF_VIDEO_PROCESSOR_ROTATION;
  _MF_VIDEO_PROCESSOR_ROTATION = (
    ROTATION_NONE   = 0,                 // Do not rotate the image.
    ROTATION_NORMAL = 1                  // Rotate the image to the correct viewing orientation.
  );
  {$EXTERNALSYM _MF_VIDEO_PROCESSOR_ROTATION}
  MF_VIDEO_PROCESSOR_ROTATION = _MF_VIDEO_PROCESSOR_ROTATION;
  {$EXTERNALSYM MF_VIDEO_PROCESSOR_ROTATION}

  // Specifies how to flip a video image.
  PMF_VIDEO_PROCESSOR_MIRROR = ^_MF_VIDEO_PROCESSOR_MIRROR;
  _MF_VIDEO_PROCESSOR_MIRROR = (
  {$EXTERNALSYM _MF_VIDEO_PROCESSOR_MIRROR}
    MIRROR_NONE       = 0,            // Do not flip the image.
    MIRROR_HORIZONTAL = 1,            // Flip the image horizontally.
    MIRROR_VERTICAL   = 2             // Flip the image vertically.
  );
  MF_VIDEO_PROCESSOR_MIRROR = _MF_VIDEO_PROCESSOR_MIRROR;
  {$EXTERNALSYM MF_VIDEO_PROCESSOR_MIRROR}


  // Interface IMFVideoProcessorControl
  // ==================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoProcessorControl);'}
  {$EXTERNALSYM IMFVideoProcessorControl}
  IMFVideoProcessorControl = interface(IUnknown)
    ['{A3F675D5-6119-4f7f-A100-1D8B280F0EFB}']

      function SetBorderColor(pBorderColor: MFARGB): HResult; stdcall;

      function SetSourceRectangle(pSrcRect: TRect): HResult; stdcall;

      function SetDestinationRectangle(pDstRect: TRect): HResult; stdcall;

      function SetMirror(eMirror: MF_VIDEO_PROCESSOR_MIRROR): HResult; stdcall;

      function SetRotation(eRotation: MF_VIDEO_PROCESSOR_ROTATION): HResult; stdcall;

      function SetConstrictionSize(pConstrictionSize: SIZE): HResult; stdcall;

  end;
  IID_IMFVideoProcessorControl = IMFVideoProcessorControl;
  {$EXTERNALSYM IID_IMFVideoProcessorControl}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

//#if (WINVER >= _WIN32_WINNT_WINBLUE)

  // Interface IMFVideoProcessorControl2
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoProcessorControl2);'}
  {$EXTERNALSYM IMFVideoProcessorControl2}
  IMFVideoProcessorControl2 = interface(IMFVideoProcessorControl)
    ['{BDE633D3-E1DC-4a7f-A693-BBAE399C4A20}']

      function SetRotationOverride(uiRotation: UINT): HResult; stdcall;

      function EnableHardwareEffects(fEnabled: BOOL): HResult; stdcall;

      function GetSupportedHardwareEffects(out puiSupport: UINT): HResult; stdcall;

  end;
  IID_IMFVideoProcessorControl2 = IMFVideoProcessorControl2;
  {$EXTERNALSYM IID_IMFVideoProcessorControl2}

// #endif // (WINVER >= _WIN32_WINNT_WINBLUE)


// (WINVER >= _WIN32_WINNT_WIN10)

  PMFVideoSphericalFormat = ^MFVideoSphericalFormat;
  {$EXTERNALSYM _MFVideoSphericalFormat}
  _MFVideoSphericalFormat = (
    MFVideoSphericalFormat_Unsupported	    = 0,
    MFVideoSphericalFormat_Equirectangular	= 1,
    MFVideoSphericalFormat_CubeMap	        = 2,
    MFVideoSphericalFormat_3DMesh	          = 3
  );
  {$EXTERNALSYM MFVideoSphericalFormat}
  MFVideoSphericalFormat = _MFVideoSphericalFormat;


  PMFVideoSphericalProjectionMode = ^MFVideoSphericalProjectionMode;
  MFVideoSphericalProjectionMode             = (
    MFVideoSphericalProjectionMode_Spherical = 0,
    MFVideoSphericalProjectionMode_Flat      = (MFVideoSphericalProjectionMode_Spherical  + 1)
  );
  {$EXTERNALSYM MFVideoSphericalProjectionMode}


  // Interface IMFVideoProcessorControl3
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoProcessorControl3);'}
  {$EXTERNALSYM IMFVideoProcessorControl3}
  IMFVideoProcessorControl3 = interface(IMFVideoProcessorControl2)
    ['{2424B3F2-EB23-40f1-91AA-74BDDEEA0883}']

      function GetNaturalOutputType(ppType: IMFMediaType): HRESULT; stdcall;

      function EnableSphericalVideoProcessing(fEnable: BOOL;
                                              eFormat: MFVideoSphericalFormat;
                                              eProjectionMode: MFVideoSphericalProjectionMode): HResult; stdcall;

      function SetSphericalVideoProperties(X: Single;
                                           Y: Single;
                                           Z: Single;
                                           W: Single;
                                           fieldOfView: Single): HResult; stdcall;

      function SetOutputDevice(var pOutputDevice: IUnknown): HResult; stdcall;

  end;
  IID_IMFVideoProcessorControl3 = IMFVideoProcessorControl3;
  {$EXTERNALSYM IID_IMFVideoProcessorControl3}


// #if (NTDDI_VERSION >= NTDDI_WIN10_VB)

  // Interface IMFVideoRendererEffectControl
  // Configuration interface for Video Renderer Effects
  // ==================================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoProcessorControl3);'}
  {$EXTERNALSYM IMFVideoProcessorControl3}
  IMFVideoRendererEffectControl = interface(IUnknown)
    ['{604D33D7-CF23-41d5-8224-5BBBB1A87475}']
    // <summary>
    // When the Renderer Effect requests a communication channel using the
    // MF_VIDEO_RENDERER_EFFECT_APP_SERVICE_NAME MFT attribute, this method will
    // be called asynchronously by the platform on successful establishment of the requested
    // communication channel. The Video Renderer Effect's App Service implements this
    // interface.
    // </summary>
    // <param name=pAppServiceConnection>
    // IUnknown of the app service connection object.
    // </param>
    function OnAppServiceConnectionEstablished(pAppServiceConnection: IUnknown): HResult; stdcall;
  end;
  IID_IMFVideoRendererEffectControl = IMFVideoRendererEffectControl;
  {$EXTERNALSYM IID_IMFVideoRendererEffectControl}
// #endif // (WINVER >= NTDDI_WIN10_VB)


  // Interface IMFTopology
  // =====================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTopology);'}
  {$EXTERNALSYM IMFTopology}
  IMFTopology = interface(IMFAttributes)
    ['{83CF873A-F6DA-4bc8-823F-BACFD55DC433}']

    function GetTopologyID(out pID: TOPOID): HResult; stdcall;

    function AddNode(pNode: IMFTopologyNode): HResult; stdcall;

    function RemoveNode(pNode: IMFTopologyNode): HResult; stdcall;

    function GetNodeCount(out pwNodes: Word): HResult; stdcall;

    function GetNode(wIndex: Word;
                     out ppNode: IMFTopologyNode): HResult; stdcall;

    function Clear(): HResult; stdcall;

    function CloneFrom(pTopology: IMFTopology): HResult; stdcall;

    function GetNodeByID(qwTopoNodeID: TOPOID;
                         out ppNode: IMFTopologyNode): HResult; stdcall;

    function GetSourceNodeCollection(out ppCollection: IMFCollection): HResult; stdcall;

    function GetOutputNodeCollection(out ppCollection: IMFCollection): HResult; stdcall;

  end;
  IID_IMFTopology = IMFTopology;
  {$EXTERNALSYM IID_IMFTopology}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  PMFTOPOLOGY_DXVA_MODE = ^MFTOPOLOGY_DXVA_MODE;
  cwMFTOPOLOGY_DXVA_MODE    = (
    MFTOPOLOGY_DXVA_DEFAULT = 0,
    MFTOPOLOGY_DXVA_NONE    = 1,
    MFTOPOLOGY_DXVA_FULL    = 2
  );
  {$EXTERNALSYM cwMFTOPOLOGY_DXVA_MODE}
  MFTOPOLOGY_DXVA_MODE = cwMFTOPOLOGY_DXVA_MODE;
  {$EXTERNALSYM MFTOPOLOGY_DXVA_MODE}


  PMFTOPOLOGY_HARDWARE_MODE = ^MFTOPOLOGY_HARDWARE_MODE;
  cwMFTOPOLOGY_HARDWARE_MODE = (
    MFTOPOLOGY_HWMODE_SOFTWARE_ONLY     = 0,
    MFTOPOLOGY_HWMODE_USE_HARDWARE      = 1,
    MFTOPOLOGY_HWMODE_USE_ONLY_HARDWARE = 2
  );
  {$EXTERNALSYM cwMFTOPOLOGY_HARDWARE_MODE}
  MFTOPOLOGY_HARDWARE_MODE = cwMFTOPOLOGY_HARDWARE_MODE;
  {$EXTERNALSYM MFTOPOLOGY_HARDWARE_MODE}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

{$WARN BOUNDS_ERROR OFF}

  PMF_TOPOLOGY_TYPE = ^MF_TOPOLOGY_TYPE;
  MF_TOPOLOGY_TYPE                = (
    MF_TOPOLOGY_OUTPUT_NODE       = 0,   // Output node. Represents a media sink in the topology.
    MF_TOPOLOGY_SOURCESTREAM_NODE = (MF_TOPOLOGY_OUTPUT_NODE + 1), // Source node. Represents a media stream in the topology.
    MF_TOPOLOGY_TRANSFORM_NODE    = (MF_TOPOLOGY_SOURCESTREAM_NODE + 1), // Transform node. Represents a Media Foundation Transform (MFT) in the topology.
    MF_TOPOLOGY_TEE_NODE          = (MF_TOPOLOGY_TRANSFORM_NODE + 1), // Tee node.
                                                                      // A tee node does not hold a pointer to an object.
                                                                      // Instead, it represents a fork in the stream.
                                                                      // A tee node has one input and multiple outputs,
                                                                      // and samples from the upstream node are delivered to
                                                                      // all of the downstream nodes.

    MF_TOPOLOGY_MAX               = MAXDWORD // $ffffffff   Reserved.
  );
  {$EXTERNALSYM MF_TOPOLOGY_TYPE}

{$WARN BOUNDS_ERROR ON}


  // Interface IMFTopologyNode
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTopologyNode);'}
  {$EXTERNALSYM IMFTopologyNode}
  IMFTopologyNode = interface(IMFAttributes)
    ['{83CF873A-F6DA-4bc8-823F-BACFD55DC430}']

    function SetObject(pObject: IUnknown): HResult; stdcall;
    //Sets the object associated with this node.
    //Parameters
    // pObject [in]
    //   A pointer to the object's IUnknown interface.
    //   Use the value Nil to clear an object that was previous set.

    function GetObject(out ppObject: IUnknown): HResult; stdcall;

    function GetNodeType(out pType: MF_TOPOLOGY_TYPE): HResult; stdcall;

    function GetTopoNodeID(out pID: TOPOID): HResult; stdcall;

    function SetTopoNodeID(ullTopoID: TOPOID): HResult; stdcall;

    function GetInputCount(out pcInputs: DWord): HResult; stdcall;

    function GetOutputCount(out pcOutputs: DWord): HResult; stdcall;

    function ConnectOutput(dwOutputIndex: DWord;
                           pDownstreamNode: IMFTopologyNode;
                           const dwInputIndexOnDownstreamNode: DWord): HResult; stdcall;

    function DisconnectOutput(const dwOutputIndex: DWord): HResult; stdcall;

    function GetInput(dwInputIndex: DWord;
                      out ppUpstreamNode: IMFTopologyNode;
                      out pdwOutputIndexOnUpstreamNode: DWord): HResult; stdcall;

    function GetOutput(dwOutputIndex : DWord;
                       out ppDownstreamNode: IMFTopologyNode;
                       out pdwInputIndexOnDownstreamNode: DWord): HResult; stdcall;

    function SetOutputPrefType(dwOutputIndex: DWord;
                               pType: IMFMediaType): HResult; stdcall;

    function GetOutputPrefType(dwOutputIndex: DWord;
                               out ppType: IMFMediaType): HResult; stdcall;

    function SetInputPrefType(dwInputIndex: DWord;
                              pType: IMFMediaType): HResult; stdcall;

    function GetInputPrefType(dwInputIndex: DWord;
                              out ppType: IMFMediaType): HResult; stdcall;

    function CloneFrom(pNode: IMFTopologyNode): HResult; stdcall;

  end;
  IID_IMFTopologyNode = IMFTopologyNode;
  {$EXTERNALSYM IID_IMFTopologyNode}


  PMF_TOPONODE_FLUSH_MODE = ^MF_TOPONODE_FLUSH_MODE;
  _MF_TOPONODE_FLUSH_MODE    = (
    MF_TOPONODE_FLUSH_ALWAYS = 0,
    MF_TOPONODE_FLUSH_SEEK   = (MF_TOPONODE_FLUSH_ALWAYS  + 1),
    MF_TOPONODE_FLUSH_NEVER  = (MF_TOPONODE_FLUSH_SEEK  + 1)
  );
  {$EXTERNALSYM _MF_TOPONODE_FLUSH_MODE}
  MF_TOPONODE_FLUSH_MODE = _MF_TOPONODE_FLUSH_MODE;
  {$EXTERNALSYM MF_TOPONODE_FLUSH_MODE}


  PMF_TOPONODE_DRAIN_MODE = ^MF_TOPONODE_DRAIN_MODE;
  _MF_TOPONODE_DRAIN_MODE     = (
    MF_TOPONODE_DRAIN_DEFAULT = 0,
    MF_TOPONODE_DRAIN_ALWAYS  = (MF_TOPONODE_DRAIN_DEFAULT  + 1),
    MF_TOPONODE_DRAIN_NEVER   = (MF_TOPONODE_DRAIN_ALWAYS  + 1)
  );
  {$EXTERNALSYM _MF_TOPONODE_DRAIN_MODE}
  MF_TOPONODE_DRAIN_MODE = _MF_TOPONODE_DRAIN_MODE;
  {$EXTERNALSYM MF_TOPONODE_DRAIN_MODE}


  // IID_IMFGetService : TGUID = '{fa993888-4383-415a-a930-dd472a8cf6f7}';
  // Interface IMFGetService
  // =======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFGetService);'}
  {$EXTERNALSYM IMFGetService}
  IMFGetService = interface(IUnknown)
  ['{fa993888-4383-415a-a930-dd472a8cf6f7}']

    function GetService(const guidService: TGUID;
                        const riid: TGUID;
                        out ppvObject): HResult; stdcall;

  end;
  IID_IMFGetService = IMFGetService;
  {$EXTERNALSYM IID_IMFGetService}


  PMF_CLOCK_STATE = ^MF_CLOCK_STATE;
  PMfclockState = ^MFCLOCK_STATE;
  _MFCLOCK_STATE          = (
    MFCLOCK_STATE_INVALID = 0,
    MFCLOCK_STATE_RUNNING = (MFCLOCK_STATE_INVALID + 1),
    MFCLOCK_STATE_STOPPED = (MFCLOCK_STATE_RUNNING + 1),
    MFCLOCK_STATE_PAUSED  = (MFCLOCK_STATE_STOPPED + 1)
  );
  {$EXTERNALSYM _MFCLOCK_STATE}
  MFCLOCK_STATE = _MFCLOCK_STATE;
  {$EXTERNALSYM MFCLOCK_STATE}
  MF_CLOCK_STATE = MFCLOCK_STATE;   // An issue as defined in SDK 7.0
  {$EXTERNALSYM MF_CLOCK_STATE}


  PMFCLOCK_PROPERTIES = ^MFCLOCK_PROPERTIES;
  _MFCLOCK_PROPERTIES = record
    qwCorrelationRate: UInt64;
    guidClockId: TGuid;
    dwClockFlags: DWORD;
    qwClockFrequency: UInt64;
    dwClockTolerance: DWORD;
    dwClockJitter: DWORD;
    public
      procedure Copy(out destProps: _MFCLOCK_PROPERTIES);
  end;
  {$EXTERNALSYM _MFCLOCK_PROPERTIES}
  MFCLOCK_PROPERTIES = _MFCLOCK_PROPERTIES;
  {$EXTERNALSYM MFCLOCK_PROPERTIES}


  // Interface IMFClock
  // ==================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFClock);'}
  {$EXTERNALSYM IMFClock}
  IMFClock = interface(IUnknown)
    ['{2eb1e945-18b8-4139-9b1a-d5d584818530}']

    function GetClockCharacteristics(out pdwCharacteristics: PDWord): HResult; stdcall;

    function GetCorrelatedTime(dwReserved: DWord;
                               out pllClockTime: LongLong;
                               out phnsSystemTime: MFTIME): HResult; stdcall;

    function GetContinuityKey(out pdwContinuityKey: Dword): HResult; stdcall;

    function GetState(dwReserved: DWord;
                      out peClockState: MFCLOCK_STATE): HResult; stdcall;

    function GetProperties(out pClockProperties: MFCLOCK_PROPERTIES): HResult; stdcall;

  end;
  IID_IMFClock = IMFClock;
  {$EXTERNALSYM IID_IMFClock}


  // Interface IMFPresentationClock
  // ==============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPresentationClock);'}
  {$EXTERNALSYM IMFPresentationClock}
  IMFPresentationClock = interface(IMFClock)
  ['{868CE85C-8EA9-4f55-AB82-B009A910A805}']

    function SetTimeSource(pTimeSource: IMFPresentationTimeSource): HResult; stdcall;

    function GetTimeSource(out ppTimeSource: IMFPresentationTimeSource): HResult; stdcall;

    function GetTime(out phnsClockTime: MFTIME): HResult; stdcall;

    function AddClockStateSink(pStateSink: IMFClockStateSink): HResult; stdcall;

    function RemoveClockStateSink(pStateSink: IMFClockStateSink): HResult; stdcall;

    // Don't use the control functions directly, when starting a Session.
    // The session will handle those automaticly.

    function Start(llClockStartOffset: LongLong): HResult; stdcall;

    function Stop(): HResult; stdcall;

    function Pause(): HResult; stdcall;

  end;
  IID_IMFPresentationClock = IMFPresentationClock;
  {$EXTERNALSYM IID_IMFPresentationClock}


  // Interface IMFPresentationTimeSource
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPresentationTimeSource);'}
  {$EXTERNALSYM IMFPresentationTimeSource}
  IMFPresentationTimeSource = interface(IUnknown)
  ['{7FF12CCE-F76F-41c2-863B-1666C8E5E139}']

    function GetUnderlyingClock(out ppClock: IMFClock): HResult; stdcall;

  end;
  IID_IMFPresentationTimeSource = IMFPresentationTimeSource;
  {$EXTERNALSYM IID_IMFPresentationTimeSource}


  // Interface IMFClockStateSink
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFClockStateSink);'}
  {$EXTERNALSYM IMFClockStateSink}
  IMFClockStateSink = interface(IUnknown)
  ['{F6696E82-74F7-4f3d-A178-8A5E09C3659F}']

    function OnClockStart(hnsSystemTime: MFTIME;
                          llClockStartOffset: LongLong): HResult; stdcall;

    function OnClockStop(hnsSystemTime: MFTIME): HResult; stdcall;

    function OnClockPause(hnsSystemTime: MFTIME): HResult; stdcall;

    function OnClockRestart(hnsSystemTime: MFTIME): HResult; stdcall;

    function OnClockSetRate(hnsSystemTime: MFTIME;
                            flRate: Single): HResult; stdcall;

  end;
  IID_IMFClockStateSink = IMFClockStateSink;
  {$EXTERNALSYM IID_IMFClockStateSink}


  // Interface IMFPresentationDescriptor
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPresentationDescriptor);'}
  {$EXTERNALSYM IMFPresentationDescriptor}
  IMFPresentationDescriptor = interface(IMFAttributes)
  ['{03cb2711-24d7-4db6-a17f-f3a7a479a536}']

    function GetStreamDescriptorCount(out pdwDescriptorCount: DWord): HResult; stdcall;

    function GetStreamDescriptorByIndex(const dwIndex: DWord;
                                        out pfSelected: BOOL;
                                        out ppDescriptor: IMFStreamDescriptor): HResult; stdcall;

    function SelectStream(dwDescriptorIndex: Int32): HResult; stdcall;

    function DeselectStream(dwDescriptorIndex: Int32): HResult; stdcall;

    function Clone(out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;

  end;
  IID_IMFPresentationDescriptor = IMFPresentationDescriptor;
  {$EXTERNALSYM IID_IMFPresentationDescriptor}


  // Interface IMFStreamDescriptor
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFStreamDescriptor);'}
  {$EXTERNALSYM IMFStreamDescriptor}
  IMFStreamDescriptor = interface(IMFAttributes)
  ['{56c03d9c-9dbb-45f5-ab4b-d80f47c05938}']

    function GetStreamIdentifier(out pdwStreamIdentifier: DWord): HResult; stdcall;

    function GetMediaTypeHandler(out ppMediaTypeHandler: IMFMediaTypeHandler): HResult; stdcall;

  end;
  IID_IMFStreamDescriptor = IMFStreamDescriptor;
  {$EXTERNALSYM IID_IMFStreamDescriptor}


  // Interface IMFMediaTypeHandler
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaTypeHandler);'}
  {$EXTERNALSYM IMFMediaTypeHandler}
  IMFMediaTypeHandler = interface(IUnknown)
  ['{e93dcf6c-4b07-4e1e-8123-aa16ed6eadf5}']

    function IsMediaTypeSupported(pMediaType: IMFMediaType;
                                  {out} ppMediaType: IMFMediaType): HResult; stdcall;

    function GetMediaTypeCount(out pdwTypeCount: DWord): HResult; stdcall;

    function GetMediaTypeByIndex(dwIndex: DWord;
                                 out ppType: IMFMediaType): HResult; stdcall;

    function SetCurrentMediaType(pMediaType: IMFMediaType): HResult; stdcall;

    function GetCurrentMediaType(out ppMediaType: IMFMediaType): HResult; stdcall;

    function GetMajorType(out pguidMajorType: TGuid): HResult; stdcall;

  end;
  IID_IMFMediaTypeHandler = IMFMediaTypeHandler;
  {$EXTERNALSYM IID_IMFMediaTypeHandler}


  // Interface IMFTimer
  // ==================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimer);'}
  {$EXTERNALSYM IMFTimer}
  IMFTimer = interface(IUnknown)
  ['{e56e4cbd-8f70-49d8-a0f8-edb3d6ab9bf2}']

    function SetTimer(dwFlags: DWord;
                      llClockTime: LONGLONG;
                      pCallback: IMFAsyncCallback;
                      punkState: IUnknown;
                      ppunkKey: IUnknown): HResult; stdcall;

    function CancelTimer(punkKey: IUnknown): HResult; stdcall;

  end;
  IID_IMFTimer = IMFTimer;
  {$EXTERNALSYM IID_IMFTimer}


  PMFSHUTDOWN_STATUS = ^MFSHUTDOWN_STATUS;
  _MFSHUTDOWN_STATUS = (
    MFSHUTDOWN_INITIATED = 0,
    MFSHUTDOWN_COMPLETED = ( MFSHUTDOWN_INITIATED + 1 )
  );
  {$EXTERNALSYM _MFSHUTDOWN_STATUS}
  MFSHUTDOWN_STATUS=_MFSHUTDOWN_STATUS;
  {$EXTERNALSYM MFSHUTDOWN_STATUS}


  // Interface IMFShutdown
  // =====================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFShutdown);'}
  {$EXTERNALSYM IMFShutdown}
  IMFShutdown = interface(IUnknown)
  ['{97ec2ea4-0e42-4937-97ac-9d6d328824e1}']

    function Shutdown(): HResult; stdcall;

    function GetShutdownStatus(out pStatus: MFSHUTDOWN_STATUS): HResult; stdcall;

  end;
  IID_IMFShutdown = IMFShutdown;
  {$EXTERNALSYM IID_IMFShutdown}


  // Interface IMFTopoLoader
  // =======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTopoLoader);'}
  {$EXTERNALSYM IMFTopoLoader}
  IMFTopoLoader = interface(IUnknown)
  ['{DE9A6157-F660-4643-B56A-DF9F7998C7CD}']

    function Load(pInputTopo: IMFTopology;
                  out ppOutputTopo: IMFTopology;
                  pCurrentTopo: IMFTopology): HResult; stdcall;
  end;
  IID_IMFTopoLoader = IMFTopoLoader;
  {$EXTERNALSYM IID_IMFTopoLoader}


  // Interface IMFContentProtectionManager
  // =====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentProtectionManager);'}
  {$EXTERNALSYM IMFContentProtectionManager}
  IMFContentProtectionManager = interface(IUnknown)
  ['{ACF92459-6A61-42bd-B57C-B43E51203CB0}']

    function BeginEnableContent(pEnablerActivate: IMFActivate;
                                pTopo: IMFTopology;
                                pCallback: IMFAsyncCallback;
                                punkState: IUnknown): HResult; stdcall;

    function EndEnableContent(pResult: IMFAsyncResult): HResult; stdcall;

  end;
  IID_IMFContentProtectionManager = IMFContentProtectionManager;
  {$EXTERNALSYM IID_IMFContentProtectionManager}


  PMF_URL_TRUST_STATUS = ^MF_URL_TRUST_STATUS;
  cwMF_URL_TRUST_STATUS = (
    MF_LICENSE_URL_UNTRUSTED = 0,
    MF_LICENSE_URL_TRUSTED   = (MF_LICENSE_URL_UNTRUSTED + 1),
    MF_LICENSE_URL_TAMPERED  = (MF_LICENSE_URL_TRUSTED + 1)
  );
  {$EXTERNALSYM cwMF_URL_TRUST_STATUS}
  MF_URL_TRUST_STATUS = cwMF_URL_TRUST_STATUS;
  {$EXTERNALSYM MF_URL_TRUST_STATUS}


  // Interface IMFContentEnabler
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentEnabler);'}
  {$EXTERNALSYM IMFContentEnabler}
  IMFContentEnabler = interface(IUnknown)
  ['{D3C4EF59-49CE-4381-9071-D5BCD044C770}']

    function GetEnableType(out pType: TGuid): HResult; stdcall;

    function GetEnableURL(out ppwszURL: LPWSTR;
                          out pcchURL: DWord;
                          var pTrustStatus: MF_URL_TRUST_STATUS): HResult; stdcall;

    function GetEnableData(out ppbData: PByte;
                           out pcbData: Dword): HResult; stdcall;

    function IsAutomaticSupported(out pfAutomatic: BOOL): HResult; stdcall;

    function AutomaticEnable(): HResult; stdcall;

    function MonitorEnable(): HResult; stdcall;

    function Cancel(): HResult; stdcall;

  end;
  IID_IMFContentEnabler = IMFContentEnabler;
  {$EXTERNALSYM IID_IMFContentEnabler}


  PMFRR_COMPONENT_HASH_INFO = ^MFRR_COMPONENT_HASH_INFO;
  _MFRR_COMPONENT_HASH_INFO = record
    ulReason: DWORD; // Reason for failure (revoked or unsigned or badly signed).
    rgHeaderHash: array[0..STR_HASH_LEN - 1] of LPWSTR; // Header hash of the component
    rgPublicKeyHash: array[0..STR_HASH_LEN - 1] of LPWSTR; // Hash of public key if one of the certificates
                                                           // in the signing certificate chain is revoked
    wszName: array[0..MAX_PATH - 1] of LPWSTR; // Component name (full path name)
  end;
  {$EXTERNALSYM _MFRR_COMPONENT_HASH_INFO}
  MFRR_COMPONENT_HASH_INFO = _MFRR_COMPONENT_HASH_INFO;
  {$EXTERNALSYM MFRR_COMPONENT_HASH_INFO}


  PMFRR_COMPONENTS = ^MFRR_COMPONENTS;
  _MFRR_COMPONENTS = record
    dwRRInfoVersion: DWORD; // Version number
    dwRRComponents: DWORD; // Number of components in list
    pRRComponents: MFRR_COMPONENT_HASH_INFO;  // points to the end of this structure that has
                                              // allocated memory for the array of component info structures
  end;
  {$EXTERNALSYM _MFRR_COMPONENTS}
  MFRR_COMPONENTS = _MFRR_COMPONENTS;
  {$EXTERNALSYM MFRR_COMPONENTS}

  PASF_FLAT_PICTURE = ^ASF_FLAT_PICTURE;
  _ASFFlatPicture = record
    //
    // Direct mapped fields
    //
    bPictureType: PByte;
    dwDataLen: DWORD;
  end;
  {$EXTERNALSYM _ASFFlatPicture}
  ASF_FLAT_PICTURE = _ASFFlatPicture;
  {$EXTERNALSYM ASF_FLAT_PICTURE}


  // interface IMFMetadata
  // =====================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMetadata);'}
  {$EXTERNALSYM IMFMetadata}
  IMFMetadata = interface(IUnknown)
  ['{F88CFB8C-EF16-4991-B450-CB8C69E51704}']

    function SetLanguage(pwszRFC1766: LPCWSTR): HResult; stdcall;

    function GetLanguage(out ppwszRFC1766: LPCWSTR): HResult; stdcall;

    function GetAllLanguages(out ppvLanguages: PROPVARIANT): HResult; stdcall;

    function SetProperty(const pwszName: LPCWSTR;
                         const ppvValue: PROPVARIANT): HResult; stdcall;

    function GetProperty(pwszName: LPCWSTR;
                         out ppvValue: PROPVARIANT): HResult; stdcall;

    function DeleteProperty(pwszName: LPCWSTR): HResult; stdcall;

    function GetAllPropertyNames(out ppvNames: PROPVARIANT): HResult; stdcall;

  end;
  IID_IMFMetadata = IMFMetadata;
  {$EXTERNALSYM IID_IMFMetadata}


  // Interface IMFMetadataProvider
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMetadataProvider);'}
  {$EXTERNALSYM IMFMetadataProvider}
  IMFMetadataProvider = interface(IUnknown)
  ['{56181D2D-E221-4adb-B1C8-3CEE6A53F76F}']

    function GetMFMetadata(pPresentationDescriptor: IMFPresentationDescriptor;
                           dwStreamIdentifier: DWord;
                           dwFlags: DWord;
                           out ppMFMetadata: IMFMetadata): HResult; stdcall;
  end;
  IID_IMFMetadataProvider = IMFMetadataProvider;
  {$EXTERNALSYM IID_IMFMetadataProvider}


  PMFRATE_DIRECTION = ^MFRATE_DIRECTION;
  _MFRATE_DIRECTION = (
    MFRATE_FORWARD = 0,
    MFRATE_REVERSE = (MFRATE_FORWARD + 1)
  );
  {$EXTERNALSYM _MFRATE_DIRECTION}
  MFRATE_DIRECTION = _MFRATE_DIRECTION;
  {$EXTERNALSYM MFRATE_DIRECTION}


  // Interface IMFRateSupport
  // ========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRateSupport);'}
  {$EXTERNALSYM IMFRateSupport}
  IMFRateSupport = interface(IUnknown)
  ['{0a9ccdbc-d797-4563-9667-94ec5d79292d}']

    function GetSlowestRate(eDirection: MFRATE_DIRECTION;
                            const fThin: BOOL;
                            out pflRate: FLOAT): HResult; stdcall;

    function GetFastestRate(eDirection: MFRATE_DIRECTION;
                            const fThin: BOOL;
                            out pflRate: FLOAT): HResult; stdcall;

    function IsRateSupported(const fThin: BOOL;
                             const flRate: FLOAT;
                             pflNearestSupportedRate: FLOAT = 0): HResult; stdcall;

  end;
  IID_IMFRateSupport = IMFRateSupport;
  {$EXTERNALSYM IID_IMFRateSupport}


  // interface IMFRateControl
  // ========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRateControl);'}
  {$EXTERNALSYM IMFRateControl}
  IMFRateControl = interface(IUnknown)
  ['{88ddcd21-03c3-4275-91ed-55ee3929328f}']

    function SetRate(const fThin: BOOL;
                     flRate: FLOAT): HRESULT; stdcall;

    function GetRate(var fThin: BOOL;
                     var flRate: FLOAT): HRESULT; stdcall;

  end;
  IID_IMFRateControl = IMFRateControl;
  {$EXTERNALSYM IID_IMFRateControl}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  // Interface IMFTimecodeTranslate
  // ==============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimecodeTranslate);'}
  {$EXTERNALSYM IMFTimecodeTranslate}
  IMFTimecodeTranslate = interface(IUnknown)
  ['{ab9d8661-f7e8-4ef4-9861-89f334f94e74}']

    function BeginConvertTimecodeToHNS(const pPropVarTimecode: PROPVARIANT;
                                       pCallback: IMFAsyncCallback;
                                       punkState: IUnknown): HResult; stdcall;

    function EndConvertTimecodeToHNS(pResult: IMFAsyncResult;
                                     out phnsTime: MFTIME): HResult; stdcall;

    function BeginConvertHNSToTimecode(const hnsTime: MFTIME;
                                       pCallback: IMFAsyncCallback;
                                       punkState: IUnknown): HResult; stdcall;

    function EndConvertHNSToTimecode(pResult: IMFAsyncResult;
                                     out pPropVarTimecode: PROPVARIANT): HResult; stdcall;

  end;
  IID_IMFTimecodeTranslate = IMFTimecodeTranslate;
  {$EXTERNALSYM IID_IMFTimecodeTranslate}


//#endif // (WINVER >= _WIN32_WINNT_WIN7)

//#if (WINVER >= _WIN32_WINNT_WIN8)

  // Interface IMFSeekInfo
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSeekInfo);'}
  {$EXTERNALSYM IMFSeekInfo}
  IMFSeekInfo = interface(IUnknown)
  ['{26AFEA53-D9ED-42B5-AB80-E64F9EE34779}']

    function GetNearestKeyFrames(const pguidTimeFormat: TGUID;
                                 const pvarStartPosition: PROPVARIANT;
                                 out pvarPreviousKeyFrame: PROPVARIANT;
                                 out pvarNextKeyFrame: PROPVARIANT): HResult; stdcall;

  end;
  IID_IMFSeekInfo = IMFSeekInfo;
  {$EXTERNALSYM IID_IMFSeekInfo}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)


  // Interface IMFSimpleAudioVolume
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSimpleAudioVolume);'}
  {$EXTERNALSYM IMFSimpleAudioVolume}
  IMFSimpleAudioVolume = interface(IUnknown)
  ['{089EDF13-CF71-4338-8D13-9E569DBDC319}']

    function SetMasterVolume(fLevel: FLOAT): HResult; stdcall;

    function GetMasterVolume(out pfLevel: FLOAT): HResult; stdcall;

    function SetMute(const bMute: BOOL): HResult; stdcall;  //No need to convert to 4 byte BOOL

    function GetMute(out pbMute: BOOL): HResult; stdcall;   //No need to convert to 4 byte BOOL
  end;
  IID_IMFSimpleAudioVolume = IMFSimpleAudioVolume;
  {$EXTERNALSYM IID_IMFSimpleAudioVolume}



  // Interface IMFAudioStreamVolume
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFAudioStreamVolume);'}
  {$EXTERNALSYM IMFAudioStreamVolume}
  IMFAudioStreamVolume = interface(IUnknown)
  ['{76B1BBDB-4EC8-4f36-B106-70A9316DF593}']
    function GetChannelCount(out pdwCount: UINT32): HResult; stdcall;

    function SetChannelVolume(dwIndex: UINT32;
                              fLevel: FLOAT): HResult; stdcall;

    function GetChannelVolume(dwIndex: UINT32;
                              out pfLevel: FLOAT): HResult; stdcall;

    function SetAllVolumes(dwCount: UINT32;
                           pfVolumes: PFloat): HResult; stdcall;
    // The SetAllVolumes function sets the individual volume levels for all the channels in the audio stream.
    // Parameters
    // dwCount [const, in]
    // The number of elements in the pfVolumes array.
    // This parameter must equal the number of channels in the stream format.
    // To get the number of channels, call the IAudioStreamVolume.GetChannelCount method.
    // pfVolumes [const, in]
    // Array of volume levels for the channels in the audio stream.
    // The number of elements in the pfVolumes array is specified by the dwCount parameter.
    // The caller writes the volume level for each channel to the array element whose
    // index matches the channel number.
    // If the stream format has N channels, the channels are numbered from 0 to N– 1.
    // Valid volume levels are in the range 0.0 to 1.0.


    function GetAllVolumes(dwCount: UINT32;
                           {out} pfVolumes: PFloat): HResult; stdcall;
    // This function fills the array with the volume level for each channel in the stream.
    // Parameters
    // dwCount [in, out]
    // The number of elements in the pfVolumes array.
    // The dwCount parameter must equal the number of channels in the stream format.
    // To get the number of channels, call the IAudioStreamVolume.GetChannelCount function.
    // pfVolumes [out]
    // Array of volume levels for the channels in the audio stream.
    // This parameter points to a caller-allocated float array into which the
    // method writes the volume levels for the individual channels.
    // Volume levels are in the range 0.0 to 1.0.
  end;
  IID_IMFAudioStreamVolume = IMFAudioStreamVolume;
  {$EXTERNALSYM IID_IMFAudioStreamVolume}



  // Interface IMFAudioPolicy
  // ========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFAudioPolicy);'}
  {$EXTERNALSYM IMFAudioPolicy}
  IMFAudioPolicy = interface(IUnknown)
  ['{a0638c2b-6465-4395-9ae7-a321a9fd2856}']

    function SetGroupingParam(const rguidClass: REFGUID): HResult; stdcall;

    function GetGroupingParam(out pguidClass: TGuid): HResult; stdcall;

    function SetDisplayName(pszName: LPCWSTR): HResult; stdcall;

    function GetDisplayName(out pszName: LPWSTR): HResult; stdcall;

    function SetIconPath(pszPath: LPCWSTR): HResult; stdcall;

    function GetIconPath(out pszPath: LPWSTR): HResult; stdcall;

  end;
  IID_IMFAudioPolicy = IMFAudioPolicy;
  {$EXTERNALSYM IID_IMFAudioPolicy}


  // Interface IMFSampleGrabberSinkCallback
  // ======================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSampleGrabberSinkCallback);'}
  {$EXTERNALSYM IMFSampleGrabberSinkCallback}
  IMFSampleGrabberSinkCallback = interface(IUnknown)
  ['{8C7B80BF-EE42-4b59-B1DF-55668E1BDCA8}']

    function OnSetPresentationClock(pPresentationClock:  IMFPresentationClock): HResult; stdcall;

    function OnProcessSample(const guidMajorMediaType: REFGUID;
                             dwSampleFlags: DWord;
                             llSampleTime: LONGLONG;
                             llSampleDuration: LONGLONG;
                             pSampleBuffer: PByte;
                             dwSampleSize: DWord): HResult; stdcall;

    function OnShutdown(): HResult; stdcall;

  end;
  IID_IMFSampleGrabberSinkCallback = IMFSampleGrabberSinkCallback;
  {$EXTERNALSYM IID_IMFSampleGrabberSinkCallback}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  // Interface IMFSampleGrabberSinkCallback2
  // =======================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSampleGrabberSinkCallback2);'}
  {$EXTERNALSYM IMFSampleGrabberSinkCallback2}
  IMFSampleGrabberSinkCallback2 = interface(IMFSampleGrabberSinkCallback)   //modified t19122012b, Reported by eric.c.fortier, Oct 5, 2012
  ['{ca86aa50-c46e-429e-ab27-16d6ac6844cb}']

    function onProcessSampleEx(const guidMajorMediaType: REFGUID;
                               dwSampleFlags: DWord;
                               llSampleTime: LONGLONG;
                               llSampleDuration: LONGLONG;
                               pSampleBuffer: PByte;
                               pAttributes: IMFAttributes): HResult; stdcall;
  end;
  IID_IMFSampleGrabberSinkCallback2 = IMFSampleGrabberSinkCallback2;
  {$EXTERNALSYM IID_IMFSampleGrabberSinkCallback2}

//#endif (WINVER >= _WIN32_WINNT_WIN7)



  // Interface IMFWorkQueueServices
  // ==============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFWorkQueueServices);'}
  {$EXTERNALSYM IMFWorkQueueServices}
  IMFWorkQueueServices = interface(IUnknown)
  ['{35FE1BB8-A3A9-40fe-BBEC-EB569C9CCCA3}']

    function BeginRegisterTopologyWorkQueuesWithMMCSS(pCallback: IMFAsyncCallback;
                                                      pState: IUnknown): HResult; stdcall;

    function EndRegisterTopologyWorkQueuesWithMMCSS(pResult: IMFAsyncResult): HResult; stdcall;

    function BeginUnregisterTopologyWorkQueuesWithMMCSS(pCallback: IMFAsyncCallback;
                                                        pState: IUnknown): HResult; stdcall;

    function EndUnregisterTopologyWorkQueuesWithMMCSS(pResult: IMFAsyncResult): HResult; stdcall;

    function GetTopologyWorkQueueMMCSSClass(dwTopologyWorkQueueId: Dword;
                                            out pwszClass: LPWSTR;
                                            var pcchClass: DWord): HResult; stdcall;

    function GetTopologyWorkQueueMMCSSTaskId(dwTopologyWorkQueueId: Dword;
                                             out pdwTaskId: DWord): HResult; stdcall;

    function BeginRegisterPlatformWorkQueueWithMMCSS(dwPlatformWorkQueue: DWord;
                                                     const wszClass: LPCWSTR;
                                                     const dwTaskId: Dword;
                                                     pCallback: IMFAsyncCallback;
                                                     pState: IUnknown): HResult; stdcall;

    function EndRegisterPlatformWorkQueueWithMMCSS(pResult: IMFAsyncResult;
                                                   out pdwTaskId: DWord): HResult; stdcall;

    function BeginUnregisterPlatformWorkQueueWithMMCSS(dwPlatformWorkQueue: Dword;
                                                       pCallback: IMFAsyncCallback;
                                                       pState: IUnknown): HResult; stdcall;

    function EndUnregisterPlatformWorkQueueWithMMCSS(pResult: IMFAsyncResult): HResult; stdcall;

    function GetPlaftormWorkQueueMMCSSClass(dwPlatformWorkQueueId: Dword;
                                            out pwszClass: LPWSTR;
                                            var pcchClass: DWord): HResult; stdcall;

    function GetPlatformWorkQueueMMCSSTaskId(dwPlatformWorkQueueId: DWord;
                                             out pdwTaskId: DWord): HResult; stdcall;

  end;
  IID_IMFWorkQueueServices = IMFWorkQueueServices;
  {$EXTERNALSYM IID_IMFWorkQueueServices}


  // interface IMFWorkQueueServicesEx
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFWorkQueueServicesEx);'}
  {$EXTERNALSYM IMFWorkQueueServicesEx}
  IMFWorkQueueServicesEx = interface(IMFWorkQueueServices)
    ['{96bf961b-40fe-42f1-ba9d-320238b49700}']
      function GetTopologyWorkQueueMMCSSPriority(dwTopologyWorkQueueId: DWORD;
                                                 out plPriority: LONG): HResult; stdcall;

      function BeginRegisterPlatformWorkQueueWithMMCSSEx(dwPlatformWorkQueue: DWORD;
                                                         wszClass: PWideChar;
                                                         dwTaskId: DWORD;
                                                         lPriority: LONG;
                                                         pCallback: IMFAsyncCallback;
                                                         pState: IUnknown): HResult; stdcall;

      function GetPlatformWorkQueueMMCSSPriority(dwPlatformWorkQueueId: DWORD;
                                                 out plPriority: LONG): HResult; stdcall;

   end;
  IID_IMFWorkQueueServicesEx = IMFWorkQueueServicesEx;
  {$EXTERNALSYM IID_IMFWorkQueueServicesEx}


  PMF_QUALITY_DROP_MODE = ^MF_QUALITY_DROP_MODE;
  _MF_QUALITY_DROP_MODE = (
    MF_DROP_MODE_NONE = 0,
    MF_DROP_MODE_1    = $1,
    MF_DROP_MODE_2    = $2,
    MF_DROP_MODE_3    = $3,
    MF_DROP_MODE_4    = $4,
    MF_DROP_MODE_5    = $5,
    MF_NUM_DROP_MODES = $6
  );
  {$EXTERNALSYM _MF_QUALITY_DROP_MODE}
  MF_QUALITY_DROP_MODE = _MF_QUALITY_DROP_MODE;
  {$EXTERNALSYM MF_QUALITY_DROP_MODE}

  PMF_QUALITY_LEVEL = ^MF_QUALITY_LEVEL;
  _MF_QUALITY_LEVEL           = (
    MF_QUALITY_NORMAL         = 0,
    MF_QUALITY_NORMAL_MINUS_1 = $1,
    MF_QUALITY_NORMAL_MINUS_2 = $2,
    MF_QUALITY_NORMAL_MINUS_3 = $3,
    MF_QUALITY_NORMAL_MINUS_4 = $4,
    MF_QUALITY_NORMAL_MINUS_5 = $5,
    MF_NUM_QUALITY_LEVELS     = $6
  );
  {$EXTERNALSYM _MF_QUALITY_LEVEL}
  MF_QUALITY_LEVEL = _MF_QUALITY_LEVEL;
  {$EXTERNALSYM MF_QUALITY_LEVEL}



  // Interface IMFQualityManager
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFQualityManager);'}
  {$EXTERNALSYM IMFQualityManager}
  IMFQualityManager = interface(IUnknown)
  ['{8D009D86-5B9F-4115-B1FC-9F80D52AB8AB}']

    function NotifyTopology(pTopology: IMFTopology): HResult; stdcall;

    function NotifyPresentationClock(pClock: IMFPresentationClock): HResult; stdcall;

    function NotifyProcessInput(pNode: IMFTopologyNode;
                                lInputIndex: Long;
                                pSample: IMFSample): HResult; stdcall;

    function NotifyProcessOutput(pNode: IMFTopologyNode;
                                 lOutputIndex: Long;
                                 pSample: IMFSample): HResult; stdcall;

    function NotifyQualityEvent(pObject: IUnknown;
                                pEvent: IMFMediaEvent): HResult; stdcall;

    function Shutdown(): HResult; stdcall;

  end;
  IID_IMFQualityManager = IMFQualityManager;
  {$EXTERNALSYM IID_IMFQualityManager}


  // Interface IMFQualityAdvise
  // ==========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFQualityAdvise);'}
  {$EXTERNALSYM IMFQualityAdvise}
  IMFQualityAdvise = interface(IUnknown)
  ['{EC15E2E9-E36B-4f7c-8758-77D452EF4CE7}']
    function SetDropMode(eDropMode: MF_QUALITY_DROP_MODE): HResult; stdcall;

    function SetQualityLevel(eQualityLevel: MF_QUALITY_LEVEL): HResult; stdcall;

    function GetDropMode(out peDropMode: MF_QUALITY_DROP_MODE): HResult; stdcall;

    function GetQualityLevel(out peQualityLevel: MF_QUALITY_LEVEL): HResult; stdcall;

    function DropTime(hnsAmountToDrop: LONGLONG): HResult; stdcall;

  end;
  IID_IMFQualityAdvise = IMFQualityAdvise;
  {$EXTERNALSYM IID_IMFQualityAdvise}


  // interface IMFQualityAdvise2
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFQualityAdvise2);'}
  {$EXTERNALSYM IMFQualityAdvise2}
  IMFQualityAdvise2 = interface(IMFQualityAdvise)
    ['{F3706F0D-8EA2-4886-8000-7155E9EC2EAE}']

      function NotifyQualityEvent(pEvent: IMFMediaEvent;
                                  out pdwFlags: PDWORD): HResult; stdcall;

   end;
  IID_IMFQualityAdvise2 = IMFQualityAdvise2;
  {$EXTERNALSYM IID_IMFQualityAdvise2}


  // Interface IMFQualityAdviseLimits
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFQualityAdviseLimits);'}
  {$EXTERNALSYM IMFQualityAdviseLimits}
  IMFQualityAdviseLimits = interface(IUnknown)
  ['{dfcd8e4d-30b5-4567-acaa-8eb5b7853dc9}']

    function GetMaximumDropMode(out peDropMode: MF_QUALITY_DROP_MODE): HResult; stdcall;

    function GetMinimumQualityLevel(out peQualityLevel: MF_QUALITY_LEVEL): HResult; stdcall;

  end;
  IID_IMFQualityAdviseLimits = IMFQualityAdviseLimits;
  {$EXTERNALSYM IID_IMFQualityAdviseLimits}


  // Interface IMFRealTimeClient
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRealTimeClient);'}
  {$EXTERNALSYM IMFRealTimeClient}
  IMFRealTimeClient = interface(IUnknown)
  ['{2347D60B-3FB5-480c-8803-8DF3ADCD3EF0}']

    function RegisterThreads(dwTaskIndex: DWORD;
                             wszClass: LPCWSTR): HResult; stdcall;

    function UnregisterThreads(): HResult; stdcall;

    function SetWorkQueue(dwWorkQueueId: DWORD): HResult; stdcall;

  end;
  IID_IMFRealTimeClient = IMFRealTimeClient;
  {$EXTERNALSYM IID_IMFRealTimeClient}


//#if (WINVER >= _WIN32_WINNT_WIN8)

  // interface IMFRealTimeClientEx
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRealTimeClientEx);'}
  {$EXTERNALSYM IMFRealTimeClientEx}
  IMFRealTimeClientEx = interface(IUnknown)
    ['{03910848-AB16-4611-B100-17B88AE2F248}']

      function RegisterThreadsEx(var pdwTaskIndex: DWORD;
                                 wszClassName: PWideChar;
                                 lBasePriority: LONG): HResult; stdcall;

      function UnregisterThreads(): HResult; stdcall;

      function SetWorkQueueEx(dwMultithreadedWorkQueueId: DWORD;
                              lWorkItemBasePriority: LONG): HResult; stdcall;

  end;
  IID_IMFRealTimeClientEx = IMFRealTimeClientEx;
  {$EXTERNALSYM IID_IMFRealTimeClientEx}

  // Media Foundation sequence playback interfaces
  ////////////////////////////////////////////////

  PMFSequencerElementId = ^MFSequencerElementId;
  MFSequencerElementId = DWORD;
  {$EXTERNALSYM MFSequencerElementId}



  // Interface IMFSequencerSource
  // ============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSequencerSource);'}
  {$EXTERNALSYM IMFSequencerSource}
  IMFSequencerSource = interface(IUnknown)
  ['{197CD219-19CB-4de1-A64C-ACF2EDCBE59E}']

    function AppendTopology(pTopology: IMFTopology;
                            dwFlags: DWord;
                            out pdwId: MFSequencerElementId): HResult; stdcall;

    function DeleteTopology(const dwId: MFSequencerElementId): HResult; stdcall;

    function GetPresentationContext(pPD: IMFPresentationDescriptor;
                                    out pId: MFSequencerElementId;
                                    out ppTopology: IMFTopology): HResult; stdcall;

    function UpdateTopology(const dwId: MFSequencerElementId;
                            pTopology: IMFTopology): HResult; stdcall;

    function UpdateTopologyFlags(const dwId: MFSequencerElementId;
                                 dwFlags: DWord): HResult; stdcall;

  end;
  IID_IMFSequencerSource = IMFSequencerSource;
  {$EXTERNALSYM IID_IMFSequencerSource}


  // Interface IMFMediaSourceTopologyProvider
  // ========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourceTopologyProvider);'}
  {$EXTERNALSYM IMFMediaSourceTopologyProvider}
  IMFMediaSourceTopologyProvider = interface(IUnknown)
  ['{0E1D6009-C9F3-442d-8C51-A42D2D49452F}']

    function GetMediaSourceTopology(pPresentationDescriptor: IMFPresentationDescriptor;
                                    out ppTopology: IMFTopology): HResult; stdcall;

  end;
  IID_IMFMediaSourceTopologyProvider = IMFMediaSourceTopologyProvider;
  {$EXTERNALSYM IID_IMFMediaSourceTopologyProvider}


  // interface IMFMediaSourcePresentationProvider
  // ============================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourcePresentationProvider);'}
  {$EXTERNALSYM IMFMediaSourcePresentationProvider}
  IMFMediaSourcePresentationProvider = interface(IUnknown)
  ['{0E1D600a-C9F3-442d-8C51-A42D2D49452F}']

    function ForceEndOfPresentation(PresentationDescriptor: IMFPresentationDescriptor ): HRESULT; stdcall;

  end;
  IID_IMFMediaSourcePresentationProvider = IMFMediaSourcePresentationProvider;
  {$EXTERNALSYM IID_IMFMediaSourcePresentationProvider}


  // Specifies a new attribute value for a topology node.
  PMFTOPONODE_ATTRIBUTE_UPDATE = ^MFTOPONODE_ATTRIBUTE_UPDATE;
  _MFTOPONODE_ATTRIBUTE_UPDATE = record
      NodeId:           TOPOID;   // The identifier of the topology node to update. To get the identifier of a topology node, call IMFTopologyNode.GetTopoNodeID.
      guidAttributeKey: TGUID;    // GUID that specifies the attribute to update.
      attrType:         MF_ATTRIBUTE_TYPE;  // Attribute type, specified as a member of the MF_ATTRIBUTE_TYPE enumeration.
      case Integer of
        Ord(MF_ATTRIBUTE_UINT32): (u32: UINT32); // Attribute value (unsigned 32-bit integer). This member is used when attrType equals MF_ATTRIBUTE_UINT32.
        Ord(MF_ATTRIBUTE_UINT64): (u64: UINT64); // Attribute value (unsigned 32-bit integer). This member is used when attrType equals MF_ATTRIBUTE_UINT64. See Remarks.
        Ord(MF_ATTRIBUTE_DOUBLE): (d: Double);   // Attribute value (floating point). This member is used when attrType equals MF_ATTRIBUTE_DOUBLE.
      end;
  {$EXTERNALSYM _MFTOPONODE_ATTRIBUTE_UPDATE}
  MFTOPONODE_ATTRIBUTE_UPDATE = _MFTOPONODE_ATTRIBUTE_UPDATE;
  {$EXTERNALSYM MFTOPONODE_ATTRIBUTE_UPDATE}
  // Remarks
  //  Due to an error in the structure declaration, the u64 member is declared as
  //  a 32-bit integer, not a 64-bit integer.
  //  Therefore, any 64-bit value passed to the IMFTopologyNodeAttributeEditor.UpdateNodeAttributes
  //  method is truncated to 32 bits.


  // Interface IMFTopologyNodeAttributeEditor
  // ========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTopologyNodeAttributeEditor);'}
  {$EXTERNALSYM IMFTopologyNodeAttributeEditor}
  IMFTopologyNodeAttributeEditor = interface(IUnknown)
  ['{676aa6dd-238a-410d-bb99-65668d01605a}']

    function UpdateNodeAttributes(TopoId: TOPOID;
                                  cUpdates: DWORD;
                                  pUpdates: MFTOPONODE_ATTRIBUTE_UPDATE): HResult; stdcall;
  end;
  IID_IMFTopologyNodeAttributeEditor = IMFTopologyNodeAttributeEditor;
  {$EXTERNALSYM IID_IMFTopologyNodeAttributeEditor}


  PMF_LEAKY_BUCKET_PAIR = ^MF_LEAKY_BUCKET_PAIR;
  _MF_LEAKY_BUCKET_PAIR = record
    dwBitrate: DWORD;
    msBufferWindow: DWORD;
  end;
  {$EXTERNALSYM _MF_LEAKY_BUCKET_PAIR}
  MF_LEAKY_BUCKET_PAIR = _MF_LEAKY_BUCKET_PAIR;
  {$EXTERNALSYM MF_LEAKY_BUCKET_PAIR}


  PMFBYTESTREAM_BUFFERING_PARAMS = ^MFBYTESTREAM_BUFFERING_PARAMS;
  _MFBYTESTREAM_BUFFERING_PARAMS = record
    cbTotalFileSize: QWORD;
    cbPlayableDataSize: QWORD;
    prgBuckets: PMF_LEAKY_BUCKET_PAIR;
    cBuckets: DWORD;
    qwNetBufferingTime: QWORD;
    qwExtraBufferingTimeDuringSeek: QWORD;
    qwPlayDuration: QWORD;
    dRate: Single;
  end;
  {$EXTERNALSYM _MFBYTESTREAM_BUFFERING_PARAMS}
  MFBYTESTREAM_BUFFERING_PARAMS = _MFBYTESTREAM_BUFFERING_PARAMS;
  {$EXTERNALSYM MFBYTESTREAM_BUFFERING_PARAMS}


  // interface IMFByteStreamBuffering
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStreamBuffering);'}
  {$EXTERNALSYM IMFByteStreamBuffering}
  IMFByteStreamBuffering = interface(IUnknown)
  ['{6d66d782-1d4f-4db7-8c63-cb8c77f1ef5e}']

    function SetBufferingParams(pParams: MFBYTESTREAM_BUFFERING_PARAMS): HResult; stdcall;

    function EnableBuffering(fEnable: BOOL): HResult; stdcall;

    function StopBuffering(): HResult; stdcall;

  end;
  IID_IMFByteStreamBuffering = IMFByteStreamBuffering;
  {$EXTERNALSYM IID_IMFByteStreamBuffering}


  // Interface IMFByteStreamCacheControl
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStreamCacheControl);'}
  {$EXTERNALSYM IMFByteStreamCacheControl}
  IMFByteStreamCacheControl = interface(IUnknown)
    ['{F5042EA4-7A96-4a75-AA7B-2BE1EF7F88D5}']

      function StopBackgroundTransfer(): HResult; stdcall;

  end;
  IID_IMFByteStreamCacheControl = IMFByteStreamCacheControl;
  {$EXTERNALSYM IID_IMFByteStreamCacheControl}


  // interface IMFByteStreamTimeSeek
  // ===============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStreamTimeSeek);'}
  {$EXTERNALSYM IMFByteStreamTimeSeek}
  IMFByteStreamTimeSeek = interface(IUnknown)
    ['{64976BFA-FB61-4041-9069-8C9A5F659BEB}']

      function IsTimeSeekSupported(out pfTimeSeekIsSupported: BOOL): HResult; stdcall;

      function TimeSeek(const qwTimePosition: QWORD): HResult; stdcall;

      function GetTimeSeekResult(out pqwStartTime: QWORD;
                                 out pqwStopTime: QWORD;
                                 out pqwDuration: QWORD): HResult; stdcall;

  end;
  IID_IMFByteStreamTimeSeek = IMFByteStreamTimeSeek;
  {$EXTERNALSYM IID_IMFByteStreamTimeSeek}


//#if (WINVER >= _WIN32_WINNT_WIN8)


  PMF_BYTE_STREAM_CACHE_RANGE = ^MF_BYTE_STREAM_CACHE_RANGE;
  MF_BYTE_STREAM_CACHE_RANGE = record
    qwStartOffset: QWORD;
    qwEndOffset: QWORD;
  end;
  {$EXTERNALSYM MF_BYTE_STREAM_CACHE_RANGE}


  // interface IMFByteStreamCacheControl2
  // ====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStreamCacheControl2);'}
  {$EXTERNALSYM IMFByteStreamCacheControl2}
  IMFByteStreamCacheControl2 = interface(IMFByteStreamCacheControl)
    ['{71CE469C-F34B-49EA-A56B-2D2A10E51149}']

      function GetByteRanges(out pcRanges: DWORD;
                             out ppRanges: PMF_BYTE_STREAM_CACHE_RANGE): HResult; stdcall;

      function SetCacheLimit(qwBytes: QWORD): HResult; stdcall;

      function IsBackgroundTransferActive(var pfActive: BOOL): HResult; stdcall;

  end;
  IID_IMFByteStreamCacheControl2 = IMFByteStreamCacheControl2;
  {$EXTERNALSYM IID_IMFByteStreamCacheControl2}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)


  // Interface IMFNetCredential
  // ==========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetCredential);'}
  {$EXTERNALSYM IMFNetCredential}
  IMFNetCredential = interface(IUnknown)
  ['{5b87ef6a-7ed8-434f-ba0e-184fac1628d1}']

    function SetUser(pbData: PByte;
                     cbData: DWord;
                     const fDataIsEncrypted: BOOL): HResult; stdcall;

    function SetPassword(pbData: PByte;
                         cbData: DWord;
                         const fDataIsEncrypted: BOOL): HResult; stdcall;

    function GetUser(pbData: PByte;
                     var pcbData: DWord;
                     const fEncryptData: BOOL): HResult; stdcall;

    function GetPassword(out pbData: PByte;
                         var pcbData: DWord;
                         const fEncryptData: BOOL): HResult; stdcall;

    function LoggedOnUser(out pfLoggedOnUser: BOOL): HResult; stdcall;

  end;
  IID_IMFNetCredential = IMFNetCredential;
  {$EXTERNALSYM IID_IMFNetCredential}


  // IMFNetCredentialManager
  PMFNetCredentialManagerGetParam = ^MFNetCredentialManagerGetParam;
  _MFNetCredentialManagerGetParam = record
    hrOp: HResult;
    fAllowLoggedOnUser: BOOL;
    fClearTextPackage: BOOL;
    pszUrl: PWideChar;
    pszSite: PWideChar;
    pszRealm: PWideChar;
    pszPackage: PWideChar;
    nRetries: Long;
  end;
  {$EXTERNALSYM _MFNetCredentialManagerGetParam}
  MFNetCredentialManagerGetParam = _MFNetCredentialManagerGetParam;
  {$EXTERNALSYM MFNetCredentialManagerGetParam}


  // Interface IMFNetCredentialManager
  // =================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetCredentialManager);'}
  {$EXTERNALSYM IMFNetCredentialManager}
  IMFNetCredentialManager = interface(IUnknown)
  ['{5b87ef6b-7ed8-434f-ba0e-184fac1628d1}']

    function BeginGetCredentials(pParam: MFNetCredentialManagerGetParam;
                                 pCallback: IMFAsyncCallback;
                                 pState: IUnknown): HResult; stdcall;

    function EndGetCredentials(pResult: IMFAsyncResult;
                               out ppCred: IMFNetCredential): HResult; stdcall;

    function SetGood(pCred: IMFNetCredential;
                     const fGood: BOOL): HResult; stdcall;

   end;
  IID_IMFNetCredentialManager = IMFNetCredentialManager;
  {$EXTERNALSYM IID_IMFNetCredentialManager}


  // Interface IMFNetCredentialCache
  // ===============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetCredentialCache);'}
  {$EXTERNALSYM IMFNetCredentialCache}
  IMFNetCredentialCache = interface(IUnknown)
    ['{5b87ef6c-7ed8-434f-ba0e-184fac1628d1}']

      function GetCredential(pszUrl: LPCWSTR;
                             pszRealm: LPCWSTR;
                             dwAuthenticationFlags: DWORD;
                             out ppCred: IMFNetCredential;
                             out pdwRequirementsFlags: DWORD): HResult; stdcall;

      function SetGood(pCred: IMFNetCredential;
                       fGood: BOOL): HResult; stdcall;

      function SetUserOptions(pCred: IMFNetCredential;
                              dwOptionsFlags: DWORD): HResult; stdcall;

  end;
  IID_IMFNetCredentialCache = IMFNetCredentialCache;
  {$EXTERNALSYM IID_IMFNetCredentialCache}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  // Interface IMFSSLCertificateManager
  // ==================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSSLCertificateManager);'}
  {$EXTERNALSYM IMFSSLCertificateManager}
  IMFSSLCertificateManager = interface(IUnknown)
  ['{61f7d887-1230-4a8b-aeba-8ad434d1a64d}']

    function GetClientCertificate(pszURL: LPCWSTR;
                                  out ppbData: PByte;
                                  out pcbData: DWord): HResult; stdcall;

    function BeginGetClientCertificate(pszURL: LPCWSTR;
                                       pCallback: IMFAsyncCallback;
                                       pState: IUnknown): HResult; stdcall;

    function EndGetClientCertificate(pResult: IMFAsyncResult;
                                     out ppbData: PByte;
                                     out pcbData: DWord): HResult; stdcall;

    function GetCertificatePolicy(pszURL: LPCWSTR;
                                  out pfOverrideAutomaticCheck: BOOL;
                                  out pfClientCertificateAvailable: BOOL): HResult; stdcall;

    function OnServerCertificate(pszURL: LPCWSTR;
                                 pbData: PByte;
                                 out pfIsGood: BOOL): HResult; stdcall;

  end;
  IID_IMFSSLCertificateManager = IMFSSLCertificateManager;
  {$EXTERNALSYM IID_IMFSSLCertificateManager}


  // interface IMFNetResourceFilter
  // ==============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetResourceFilter);'}
  {$EXTERNALSYM IMFNetResourceFilter}
  IMFNetResourceFilter = interface(IUnknown)
    ['{091878a3-bf11-4a5c-bc9f-33995b06ef2d}']

      function OnRedirect(pszUrl: PWideChar;
                          out pvbCancel: VARIANT_BOOL): HResult; stdcall;

      function OnSendingRequest(pszUrl: PWideChar): HResult; stdcall;

  end;
  IID_IMFNetResourceFilter = IMFNetResourceFilter;
  {$EXTERNALSYM IID_IMFNetResourceFilter}


//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  // interface IMFSourceOpenMonitor
  // ==============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceOpenMonitor);'}
  {$EXTERNALSYM IMFSourceOpenMonitor}
  IMFSourceOpenMonitor = interface(IUnknown)
  ['{059054B3-027C-494C-A27D-9113291CF87F}']

    function OnSourceEvent(pEvent: IMFMediaEvent): HResult; stdcall;

  end;
  IID_IMFSourceOpenMonitor = IMFSourceOpenMonitor;
  {$EXTERNALSYM IID_IMFSourceOpenMonitor}


  // Interface IMFNetProxyLocator
  // ============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetProxyLocator);'}
  {$EXTERNALSYM IMFNetProxyLocator}
  IMFNetProxyLocator = interface(IUnknown)
  ['{e9cd0383-a268-4bb4-82de-658d53574d41}']

    function FindFirstProxy(pszHost: LPCWSTR;
                            pszUrl: LPCWSTR;
                            fReserved: BOOL): HResult; stdcall;

    function FindNextProxy(): HResult; stdcall;

    function RegisterProxyResult(hrOp: HResult): HResult; stdcall;

    function GetCurrentProxy(out pszStr: LPWSTR;
                             var pcchStr: DWord): HResult; stdcall;

    function Clone(out ppProxyLocator: IMFNetProxyLocator): HResult; stdcall;

  end;
  IID_IMFNetProxyLocator = IMFNetProxyLocator;
  {$EXTERNALSYM IID_IMFNetProxyLocator}


  // Interface IMFNetProxyLocatorFactory
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetProxyLocatorFactory);'}
  {$EXTERNALSYM IMFNetProxyLocatorFactory}
  IMFNetProxyLocatorFactory = interface(IUnknown)
  ['{e9cd0384-a268-4bb4-82de-658d53574d41}']

    function CreateProxyLocator(pszProtocol: LPCWSTR;
                                out ppProxyLocator: IMFNetProxyLocator): HResult; stdcall;

  end;
  IID_IMFNetProxyLocatorFactory = IMFNetProxyLocatorFactory;
  {$EXTERNALSYM IID_IMFNetProxyLocatorFactory}


  // Interface IMFSaveJob
  // ====================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSaveJob);'}
  {$EXTERNALSYM IMFSaveJob}
  IMFSaveJob = interface(IUnknown)
  ['{e9931663-80bf-4c6e-98af-5dcf58747d1f}']

    function BeginSave(pStream: IMFByteStream;
                       pCallback: IMFAsyncCallback;
                       pState: IUnknown): HResult; stdcall;

    function EndSave(pResult: IMFAsyncResult): HResult; stdcall;

    function CancelSave(): HResult; stdcall;

    function GetProgress(out pdwPercentComplete: DWord): HResult; stdcall;

  end;
  IID_IMFSaveJob = IMFSaveJob;
  {$EXTERNALSYM IID_IMFSaveJob}


  PMFNETSOURCE_PROTOCOL_TYPE = ^MFNETSOURCE_PROTOCOL_TYPE;
  _MFNETSOURCE_PROTOCOL_TYPE = (
    MFNETSOURCE_UNDEFINED = 0,
    MFNETSOURCE_HTTP      = $1,
    MFNETSOURCE_RTSP      = $2,
    MFNETSOURCE_FILE      = $3,
    MFNETSOURCE_MULTICAST = $4
  );
  {$EXTERNALSYM _MFNETSOURCE_PROTOCOL_TYPE}
  MFNETSOURCE_PROTOCOL_TYPE = _MFNETSOURCE_PROTOCOL_TYPE;
  {$EXTERNALSYM MFNETSOURCE_PROTOCOL_TYPE}


  // Interface IMFNetSchemeHandlerConfig
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetSchemeHandlerConfig);'}
  {$EXTERNALSYM IMFNetSchemeHandlerConfig}
  IMFNetSchemeHandlerConfig = interface(IUnknown)
  ['{7BE19E73-C9BF-468a-AC5A-A5E8653BEC87}']

    function GetNumberOfSupportedProtocols(out pcProtocols: ULONG): HResult; stdcall;

    function GetSupportedProtocolType(const nProtocolIndex: ULONG;
                                      out pnProtocolType: MFNETSOURCE_PROTOCOL_TYPE): HResult; stdcall;

    function ResetProtocolRolloverSettings(): HResult; stdcall;

  end;
  IID_IMFNetSchemeHandlerConfig = IMFNetSchemeHandlerConfig;
  {$EXTERNALSYM IID_IMFNetSchemeHandlerConfig}


  PMFNETSOURCE_TRANSPORT_TYPE = ^MFNETSOURCE_TRANSPORT_TYPE;
  _MFNETSOURCE_TRANSPORT_TYPE = (
    MFNETSOURCE_UDP = 0,
    {$EXTERNALSYM MFNETSOURCE_UDP}
    MFNETSOURCE_TCP = (MFNETSOURCE_UDP + 1)
    {$EXTERNALSYM MFNETSOURCE_TCP}
  );
  {$EXTERNALSYM _MFNETSOURCE_TRANSPORT_TYPE}
  MFNETSOURCE_TRANSPORT_TYPE = _MFNETSOURCE_TRANSPORT_TYPE;
  {$EXTERNALSYM MFNETSOURCE_TRANSPORT_TYPE}

  PMFNETSOURCE_CACHE_STATE = ^MFNETSOURCE_CACHE_STATE;
  _MFNETSOURCE_CACHE_STATE            = (
    MFNETSOURCE_CACHE_UNAVAILABLE     = 0,
    MFNETSOURCE_CACHE_ACTIVE_WRITING  = (MFNETSOURCE_CACHE_UNAVAILABLE + 1),
    MFNETSOURCE_CACHE_ACTIVE_COMPLETE = (MFNETSOURCE_CACHE_ACTIVE_WRITING + 1)
  );
  {$EXTERNALSYM _MFNETSOURCE_CACHE_STATE}
  MFNETSOURCE_CACHE_STATE = _MFNETSOURCE_CACHE_STATE;
  {$EXTERNALSYM MFNETSOURCE_CACHE_STATE}

  PMFNETSOURCE_STATISTICS_IDS = ^MFNETSOURCE_STATISTICS_IDS;
  _MFNETSOURCE_STATISTICS_IDS               = (
    MFNETSOURCE_RECVPACKETS_ID              = 0,
    MFNETSOURCE_LOSTPACKETS_ID              = (MFNETSOURCE_RECVPACKETS_ID + 1),
    MFNETSOURCE_RESENDSREQUESTED_ID         = (MFNETSOURCE_LOSTPACKETS_ID + 1),
    MFNETSOURCE_RESENDSRECEIVED_ID          = (MFNETSOURCE_RESENDSREQUESTED_ID + 1),
    MFNETSOURCE_RECOVEREDBYECCPACKETS_ID    = (MFNETSOURCE_RESENDSRECEIVED_ID + 1),
    MFNETSOURCE_RECOVEREDBYRTXPACKETS_ID    = (MFNETSOURCE_RECOVEREDBYECCPACKETS_ID + 1),
    MFNETSOURCE_OUTPACKETS_ID               = (MFNETSOURCE_RECOVEREDBYRTXPACKETS_ID + 1),
    MFNETSOURCE_RECVRATE_ID                 = (MFNETSOURCE_OUTPACKETS_ID + 1),
    MFNETSOURCE_AVGBANDWIDTHBPS_ID          = (MFNETSOURCE_RECVRATE_ID + 1),
    MFNETSOURCE_BYTESRECEIVED_ID            = (MFNETSOURCE_AVGBANDWIDTHBPS_ID + 1),
    MFNETSOURCE_PROTOCOL_ID                 = (MFNETSOURCE_BYTESRECEIVED_ID + 1),
    MFNETSOURCE_TRANSPORT_ID                = (MFNETSOURCE_PROTOCOL_ID + 1),
    MFNETSOURCE_CACHE_STATE_ID              = (MFNETSOURCE_TRANSPORT_ID + 1),
    MFNETSOURCE_LINKBANDWIDTH_ID            = (MFNETSOURCE_CACHE_STATE_ID + 1),
    MFNETSOURCE_CONTENTBITRATE_ID           = (MFNETSOURCE_LINKBANDWIDTH_ID + 1),
    MFNETSOURCE_SPEEDFACTOR_ID              = (MFNETSOURCE_CONTENTBITRATE_ID + 1),
    MFNETSOURCE_BUFFERSIZE_ID               = (MFNETSOURCE_SPEEDFACTOR_ID + 1),
    MFNETSOURCE_BUFFERPROGRESS_ID           = (MFNETSOURCE_BUFFERSIZE_ID + 1),
    MFNETSOURCE_LASTBWSWITCHTS_ID           = (MFNETSOURCE_BUFFERPROGRESS_ID + 1),
    MFNETSOURCE_SEEKRANGESTART_ID           = (MFNETSOURCE_LASTBWSWITCHTS_ID + 1),
    MFNETSOURCE_SEEKRANGEEND_ID             = (MFNETSOURCE_SEEKRANGESTART_ID + 1),
    MFNETSOURCE_BUFFERINGCOUNT_ID           = (MFNETSOURCE_SEEKRANGEEND_ID + 1),
    MFNETSOURCE_INCORRECTLYSIGNEDPACKETS_ID = (MFNETSOURCE_BUFFERINGCOUNT_ID + 1),
    MFNETSOURCE_SIGNEDSESSION_ID            = (MFNETSOURCE_INCORRECTLYSIGNEDPACKETS_ID + 1),
    MFNETSOURCE_MAXBITRATE_ID               = (MFNETSOURCE_SIGNEDSESSION_ID + 1),
    MFNETSOURCE_RECEPTION_QUALITY_ID        = (MFNETSOURCE_MAXBITRATE_ID + 1),
    MFNETSOURCE_RECOVEREDPACKETS_ID         = (MFNETSOURCE_RECEPTION_QUALITY_ID + 1),
    MFNETSOURCE_VBR_ID                      = (MFNETSOURCE_RECOVEREDPACKETS_ID + 1),
    MFNETSOURCE_DOWNLOADPROGRESS_ID         = (MFNETSOURCE_VBR_ID + 1),
    MFNETSOURCE_UNPREDEFINEDPROTOCOLNAME_ID = (MFNETSOURCE_DOWNLOADPROGRESS_ID + 1)
  );
  {$EXTERNALSYM _MFNETSOURCE_STATISTICS_IDS}
  MFNETSOURCE_STATISTICS_IDS = _MFNETSOURCE_STATISTICS_IDS;
  {$EXTERNALSYM MFNETSOURCE_STATISTICS_IDS}

  PMFNET_PROXYSETTINGS = ^MFNET_PROXYSETTINGS;
  _MFNET_PROXYSETTINGS         = (
    MFNET_PROXYSETTING_NONE    = 0,
    MFNET_PROXYSETTING_MANUAL  = 1,
    MFNET_PROXYSETTING_AUTO    = 2,
    MFNET_PROXYSETTING_BROWSER = 3
  );
  {$EXTERNALSYM _MFNET_PROXYSETTINGS}
  MFNET_PROXYSETTINGS = _MFNET_PROXYSETTINGS;
  {$EXTERNALSYM MFNET_PROXYSETTINGS}


  // Interface IMFSchemeHandler
  // ==========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSchemeHandler);'}
  {$EXTERNALSYM IMFSchemeHandler}
  IMFSchemeHandler = interface(IUnknown)
  ['{6D4C7B74-52A0-4bb7-B0DB-55F29F47A668}']

    function BeginCreateObject(pwszURL: LPCWSTR;
                               dwFlags: DWord;
                               pProps: IPropertyStore;
                               out ppIUnknownCancelCookie: IUnknown;
                               pCallback: IMFAsyncCallback;
                               punkState: IUnknown): HResult; stdcall;

    function EndCreateObject(pResult: IMFAsyncResult;
                             out pObjectType: MF_OBJECT_TYPE;
                             out ppObject: IUnknown): HResult; stdcall;

    function CancelObjectCreation(pIUnknownCancelCookie: IUnknown): HResult; stdcall;

  end;
  IID_IMFSchemeHandler = IMFSchemeHandler;
  {$EXTERNALSYM IID_IMFSchemeHandler}


  // interface IMFByteStreamHandler
  // ==============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStreamHandler);'}
  {$EXTERNALSYM IMFByteStreamHandler}
  IMFByteStreamHandler = interface(IUnknown)
  ['{BB420AA4-765B-4a1f-91FE-D6A8A143924C}']

    function BeginCreateObject(pByteStream: IMFByteStream;
                               pwszURL: LPCWSTR;
                               dwFlags: DWord;
                               pProps: IPropertyStore;
                               out ppIUnknownCancelCookie: IUnknown;
                               pCallback: IMFAsyncCallback;
                               punkState: IUnknown): HResult; stdcall;

    function EndCreateObject(pResult: IMFAsyncResult;
                             out pObjectType: MF_OBJECT_TYPE;
                             out ppObject: IUnknown): HResult; stdcall;

    function CancelObjectCreation(pIUnknownCancelCookie: IUnknown): HResult; stdcall;

    function GetMaxNumberOfBytesRequiredForResolution(out pqwBytes: QWORD): HResult; stdcall;

  end;
  IID_IMFByteStreamHandler = IMFByteStreamHandler;
  {$EXTERNALSYM IID_IMFByteStreamHandler}


  // Interface IMFTrustedInput
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTrustedInput);'}
  {$EXTERNALSYM IMFTrustedInput}
  IMFTrustedInput = interface(IUnknown)
  ['{542612C4-A1B8-4632-B521-DE11EA64A0B0}']

    function GetInputTrustAuthority(const dwStreamID: DWord;
                                    const riid: REFIID;
                                    out ppunkObject: IUnknown): HResult; stdcall;
  end;
  IID_IMFTrustedInput = IMFTrustedInput;
  {$EXTERNALSYM IID_IMFTrustedInput}


  PMFPOLICYMANAGER_ACTION = ^MFPOLICYMANAGER_ACTION;
  _MFPOLICYMANAGER_ACTION = (
    PEACTION_NO        = 0,
    PEACTION_PLAY      = 1,
    PEACTION_COPY      = 2,
    PEACTION_EXPORT    = 3,
    PEACTION_EXTRACT   = 4,
    PEACTION_RESERVED1 = 5,
    PEACTION_RESERVED2 = 6,
    PEACTION_RESERVED3 = 7,
    PEACTION_LAST      = 7
  );
  {$EXTERNALSYM _MFPOLICYMANAGER_ACTION}
  MFPOLICYMANAGER_ACTION = _MFPOLICYMANAGER_ACTION;
  {$EXTERNALSYM MFPOLICYMANAGER_ACTION}

  PMFINPUTTRUSTAUTHORITY_ACCESS_ACTION = ^MFINPUTTRUSTAUTHORITY_ACCESS_ACTION;
  _MFINPUTTRUSTAUTHORITY_ACTION = record
    Action: MFPOLICYMANAGER_ACTION;
    pbTicket: PByte;
    cbTicket: DWORD;
  end;
  {$EXTERNALSYM _MFINPUTTRUSTAUTHORITY_ACTION}
  MFINPUTTRUSTAUTHORITY_ACCESS_ACTION = _MFINPUTTRUSTAUTHORITY_ACTION;
  {$EXTERNALSYM MFINPUTTRUSTAUTHORITY_ACCESS_ACTION}

  PMFINPUTTRUSTAUTHORITY_ACCESS_PARAMS = ^MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS;
  _MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS = record
    dwSize: DWORD;
    dwVer: DWORD;
    cbSignatureOffset: DWORD;
    cbSignatureSize: DWORD;
    cbExtensionOffset: DWORD;
    cbExtensionSize: DWORD;
    cActions: DWORD;
    rgOutputActions: array of MFINPUTTRUSTAUTHORITY_ACCESS_ACTION;
  end;
  {$EXTERNALSYM _MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS}
  MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS = _MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS;
  {$EXTERNALSYM MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS}


  // Interface IMFInputTrustAuthority
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFInputTrustAuthority);'}
  {$EXTERNALSYM IMFInputTrustAuthority}
  IMFInputTrustAuthority = interface(IUnknown)
  ['{D19F8E98-B126-4446-890C-5DCB7AD71453}']

    function GetDecrypter(const riid: REFIID;
                          out ppv: Pointer): HResult; stdcall;

    function RequestAccess(Action: MFPOLICYMANAGER_ACTION;
                           out ppContentEnablerActivate: IMFActivate): HResult; stdcall;

    function GetPolicy(Action: MFPOLICYMANAGER_ACTION;
                       out ppPolicy: IMFOutputPolicy): HResult; stdcall;

    function BindAccess(pParam: MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS): HResult; stdcall;

    function UpdateAccess(pParam: MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS): HResult; stdcall;

    function Reset(): HResult; stdcall;

  end;
  IID_IMFInputTrustAuthority = IMFInputTrustAuthority;
  {$EXTERNALSYM IID_IMFInputTrustAuthority}


  // Interface IMFTrustedOutput
  // ==========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTrustedOutput);'}
  {$EXTERNALSYM IMFTrustedOutput}
  IMFTrustedOutput = interface(IUnknown)
  ['{D19F8E95-B126-4446-890C-5DCB7AD71453}']

    function GetOutputTrustAuthorityCount(out pcOutputTrustAuthorities: DWord): HResult; stdcall;

    function GetOutputTrustAuthorityByIndex(dwIndex: Dword;
                                            out ppauthority: IMFOutputTrustAuthority): HResult; stdcall;

    function IsFinal(out pfIsFinal: BOOL): HResult; stdcall;

  end;
  IID_IMFTrustedOutput = IMFTrustedOutput;
  {$EXTERNALSYM IID_IMFTrustedOutput}


  // interface IMFOutputTrustAuthority
  // =================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFOutputTrustAuthority);'}
  {$EXTERNALSYM IMFOutputTrustAuthority}
  IMFOutputTrustAuthority = interface(IUnknown)
  ['{D19F8E94-B126-4446-890C-5DCB7AD71453}']

    function GetAction(out Action: MFPOLICYMANAGER_ACTION): HRESULT; stdcall;

    function SetPolicy(Policy: IMFOutputPolicy;
                       nPolicy: DWORD;
                       out bTicket: PBYTE;
                       out cbTicket: DWORD): HRESULT; stdcall;

  end;
  IID_IMFOutputTrustAuthority = IMFOutputTrustAuthority;
  {$EXTERNALSYM IID_IMFOutputTrustAuthority}


  // Interface IMFOutputPolicy
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFOutputPolicy);'}
  {$EXTERNALSYM IMFOutputPolicy}
  IMFOutputPolicy = interface(IUnknown)
  ['{7F00F10A-DAED-41AF-AB26-5FDFA4DFBA3C}']

    function GenerateRequiredSchemas(dwAttributes: DWord;
                                     const guidOutputSubType: TGuid;
                                     const rgGuidProtectionSchemasSupported: TGuid;
                                     cProtectionSchemasSupported: DWord;
                                     out ppRequiredProtectionSchemas: IMFCollection): HResult; stdcall;

    function GetOriginatorID(out pguidOriginatorID: TGuid): HResult; stdcall;

    function GetMinimumGRLVersion(out pdwMinimumGRLVersion: DWord): HResult; stdcall;

  end;
  IID_IMFOutputPolicy = IMFOutputPolicy;
  {$EXTERNALSYM IID_IMFOutputPolicy}


  // Interface IMFOutputSchema
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFOutputSchema);'}
  {$EXTERNALSYM IMFOutputSchema}
  IMFOutputSchema = interface(IUnknown)
  ['{7BE0FC5B-ABD9-44FB-A5C8-F50136E71599}']

    function GetSchemaType(out pguidSchemaType: TGuid): HResult; stdcall;

    function GetConfigurationData(out pdwVal: DWord): HResult; stdcall;

    function GetOriginatorID(out pguidOriginatorID: TGuid): HResult; stdcall;

  end;
  IID_IMFOutputSchema = IMFOutputSchema;
  {$EXTERNALSYM IID_IMFOutputSchema}


  PMF_OPM_CGMSA_PROTECTION_LEVEL = ^MF_OPM_CGMSA_PROTECTION_LEVEL;
  _MF_OPM_CGMSA_PROTECTION_LEVEL                 = (
    MF_OPM_CGMSA_OFF                             = 0,
    MF_OPM_CGMSA_COPY_FREELY                     = $1,
    MF_OPM_CGMSA_COPY_NO_MORE                    = $2,
    MF_OPM_CGMSA_COPY_ONE_GENERATION             = $3,
    MF_OPM_CGMSA_COPY_NEVER                      = $4,
    MF_OPM_CGMSA_REDISTRIBUTION_CONTROL_REQUIRED = $8
  );
  {$EXTERNALSYM _MF_OPM_CGMSA_PROTECTION_LEVEL}
  MF_OPM_CGMSA_PROTECTION_LEVEL = _MF_OPM_CGMSA_PROTECTION_LEVEL;
  {$EXTERNALSYM MF_OPM_CGMSA_PROTECTION_LEVEL}

  PMF_OPM_ACP_PROTECTION_LEVEL = ^MF_OPM_ACP_PROTECTION_LEVEL;
  _MF_OPM_ACP_PROTECTION_LEVEL = (
    MF_OPM_ACP_OFF         = 0,
    MF_OPM_ACP_LEVEL_ONE   = 1,
    MF_OPM_ACP_LEVEL_TWO   = 2,
    MF_OPM_ACP_LEVEL_THREE = 3,
    MF_OPM_ACP_FORCE_ULONG = $7FFFFFFF
  );
  {$EXTERNALSYM _MF_OPM_ACP_PROTECTION_LEVEL}
  MF_OPM_ACP_PROTECTION_LEVEL = _MF_OPM_ACP_PROTECTION_LEVEL;
  {$EXTERNALSYM MF_OPM_ACP_PROTECTION_LEVEL}

  PMFAudioConstriction = ^MFAudioConstriction;
  _MFAudioConstriction       = (
    MFaudioConstrictionOff   = 0,
    MFaudioConstriction48_16 = (MFaudioConstrictionOff  + 1),
    MFaudioConstriction44_16 = (MFaudioConstriction48_16  + 1),
    MFaudioConstriction14_14 = (MFaudioConstriction44_16  + 1),
    MFaudioConstrictionMute  = (MFaudioConstriction14_14  + 1)
  );
  {$EXTERNALSYM _MFAudioConstriction}
  MFAudioConstriction = _MFAudioConstriction;
  {$EXTERNALSYM MFAudioConstriction}


  // Interface IMFSecureChannel
  // ==========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSecureChannel);'}
  {$EXTERNALSYM IMFSecureChannel}
  IMFSecureChannel = interface(IUnknown)
  ['{d0ae555d-3b12-4d97-b060-0990bc5aeb67}']

    function GetCertificate(out ppCert: PByte;
                            out pcbCert: DWORD): HResult; stdcall;

    function SetupSession(pbEncryptedSessionKey: PByte;
                          cbSessionKey: DWORD): HResult; stdcall;

   end;
  IID_IMFSecureChannel = IMFSecureChannel;
  {$EXTERNALSYM IID_IMFSecureChannel}

  PSAMPLE_PROTECTION_VERSION = ^SAMPLE_PROTECTION_VERSION;
  SAMPLE_PROTECTION_VERSION              = (
    SAMPLE_PROTECTION_VERSION_NO         = 0,
    {$EXTERNALSYM SAMPLE_PROTECTION_VERSION_NO}
    SAMPLE_PROTECTION_VERSION_BASIC_LOKI = 1,
    {$EXTERNALSYM SAMPLE_PROTECTION_VERSION_BASIC_LOKI}
    SAMPLE_PROTECTION_VERSION_SCATTER    = 2,
    {$EXTERNALSYM SAMPLE_PROTECTION_VERSION_SCATTER}
    SAMPLE_PROTECTION_VERSION_RC4        = 3,
    {$EXTERNALSYM SAMPLE_PROTECTION_VERSION_RC4}
    SAMPLE_PROTECTION_VERSION_AES128CTR	 = 4
    {$EXTERNALSYM SAMPLE_PROTECTION_VERSION_AES128CTR}
  );
  {$EXTERNALSYM SAMPLE_PROTECTION_VERSION}


  // Interface IMFSampleProtection
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSampleProtection);'}
  {$EXTERNALSYM IMFSampleProtection}
  IMFSampleProtection = interface(IUnknown)
  ['{8e36395f-c7b9-43c4-a54d-512b4af63c95}']

    function GetInputProtectionVersion(out pdwVersion: DWord): HResult; stdcall;

    function GetOutputProtectionVersion(out pdwVersion: DWord): HResult; stdcall;

    function GetProtectionCertificate(dwVersion: DWord;
                                      out ppCert: PByte;
                                      out pcbCert: DWord): HResult; stdcall;

    function InitOutputProtection(dwVersion: DWord;
                                  dwOutputId: DWord;
                                  pbCert: PByte;
                                  cbCert: DWord;
                                  out ppbSeed: PByte;
                                  out pcbSeed: DWord): HResult; stdcall;

    function InitInputProtection(dwVersion: DWord;
                                 dwInputId: DWord;
                                 pbSeed: PByte;
                                 const cbSeed: DWord): HResult; stdcall;

  end;
  IID_IMFSampleProtection = IMFSampleProtection;
  {$EXTERNALSYM IID_IMFSampleProtection}


  // interface IMFMediaSinkPreroll
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSinkPreroll);'}
  {$EXTERNALSYM IMFMediaSinkPreroll}
  IMFMediaSinkPreroll = interface(IUnknown)
  ['{5dfd4b2a-7674-4110-a4e6-8a68fd5f3688}']

    function NotifyPreroll(hnsUpcomingStartTime: MFTIME): HRESULT; stdcall;

  end;
  IID_IMFMediaSinkPreroll = IMFMediaSinkPreroll;
  {$EXTERNALSYM IID_IMFMediaSinkPreroll}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  // Interface IMFFinalizableMediaSink
  // =================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFFinalizableMediaSink);'}
  {$EXTERNALSYM IMFFinalizableMediaSink}
  IMFFinalizableMediaSink = interface(IUnknown)
    ['{EAECB74A-9A50-42ce-9541-6A7F57AA4AD7}']

      function BeginFinalize(pCallback: IMFAsyncCallback;
                             punkState: IUnknown): HResult; stdcall;

      function EndFinalize(pResult: IMFAsyncResult): HResult; stdcall;

  end;
  IID_IMFFinalizableMediaSink = IMFFinalizableMediaSink;
  {$EXTERNALSYM IID_IMFFinalizableMediaSink}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  // interface IMFStreamingSinkConfig
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFStreamingSinkConfig);'}
  {$EXTERNALSYM IMFStreamingSinkConfig}
  IMFStreamingSinkConfig = interface(IUnknown)
    ['{9db7aa41-3cc5-40d4-8509-555804ad34cc}']

      function StartStreaming(fSeekOffsetIsByteOffset: BOOL;
                              qwSeekOffset: QWORD): HRESULT; stdcall;

  end;
  IID_IMFStreamingSinkConfig = IMFStreamingSinkConfig;
  {$EXTERNALSYM IID_IMFStreamingSinkConfig}


  // Interface IMFRemoteProxy
  // ========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRemoteProxy);'}
  {$EXTERNALSYM IMFRemoteProxy}
  IMFRemoteProxy = interface(IUnknown)
    ['{994e23ad-1cc2-493c-b9fa-46f1cb040fa4}']

      function GetRemoteObject(const riid: REFIID;
                               out ppv): HResult; stdcall;

      function GetRemoteHost(const riid: REFIID;
                             out ppv): HResult; stdcall;

  end;
  IID_IMFRemoteProxy = IMFRemoteProxy;
  {$EXTERNALSYM IID_IMFRemoteProxy}


  // interface IMFObjectReferenceStream
  // ==================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFObjectReferenceStream);'}
  {$EXTERNALSYM IMFObjectReferenceStream}
  IMFObjectReferenceStream = interface(IUnknown)
    ['{09EF5BE3-C8A7-469e-8B70-73BF25BB193F}']

      function SaveReference(const riid: REFIID;
                             pUnk: IUnknown): HRESULT; stdcall;

      function LoadReference(const riid: REFIID;
                             out ppv): HRESULT; stdcall;

  end;
  IID_IMFObjectReferenceStream = IMFObjectReferenceStream;
  {$EXTERNALSYM IID_IMFObjectReferenceStream}


  // Interface IMFPMPHost
  // ====================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMPHost);'}
  {$EXTERNALSYM IMFPMPHost}
  IMFPMPHost = interface(IUnknown)
    ['{F70CA1A9-FDC7-4782-B994-ADFFB1C98606}']

      function LockProcess(): HResult; stdcall;

      function UnlockProcess(): HResult; stdcall;

      function CreateObjectByCLSID(const clsid: REFCLSID;
                                   pStream: IStream;
                                   const riid: REFIID;
                                   out ppv): HResult; stdcall;

  end;
  IID_IMFPMPHost = IMFPMPHost;
  {$EXTERNALSYM IID_IMFPMPHost}


  // Interface IMFPMPClient
  // ======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMPClient);'}
  {$EXTERNALSYM IMFPMPClient}
  IMFPMPClient = interface(IUnknown)
  ['{6C4E655D-EAD8-4421-B6B9-54DCDBBDF820}']

    function SetPMPHost(pPMPHost: IMFPMPHost): HResult; stdcall;

  end;
  IID_IMFPMPClient = IMFPMPClient;
  {$EXTERNALSYM IID_IMFPMPClient}


  // Interface IMFPMPServer
  // ======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMPServer);'}
  {$EXTERNALSYM IMFPMPServer}
  IMFPMPServer = interface(IUnknown)
  ['{994e23af-1cc2-493c-b9fa-46f1cb040fa4}']

    function LockProcess(): HResult; stdcall;

    function UnlockProcess(): HResult; stdcall;

    function CreateObjectByCLSID(const clsid: REFCLSID;
                                 const riid: REFIID;
                                 out ppObject): HResult; stdcall;

  end;
  IID_IMFPMPServer = IMFPMPServer;
  {$EXTERNALSYM IID_IMFPMPServer}


  // Interface IMFRemoteDesktopPlugin
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRemoteDesktopPlugin);'}
  {$EXTERNALSYM IMFRemoteDesktopPlugin}
  IMFRemoteDesktopPlugin = interface(IUnknown)
  ['{1cde6309-cae0-4940-907e-c1ec9c3d1d4a}']

    function UpdateTopology(var Topology: IMFTopology): HRESULT; stdcall;

  end;
  IID_IMFRemoteDesktopPlugin = IMFRemoteDesktopPlugin;
  {$EXTERNALSYM IID_IMFRemoteDesktopPlugin}


  // Interface IMFSAMIStyle
  // ======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSAMIStyle);'}
  {$EXTERNALSYM IMFSAMIStyle}
  IMFSAMIStyle = interface(IUnknown)
  ['{A7E025DD-5303-4a62-89D6-E747E1EFAC73}']

    function GetStyleCount(out pdwCount: DWord): HResult; stdcall;
    // Gets the total number of styles

    function GetStyles(out pPropVarStyleArray: PROPVARIANTarray): HResult; stdcall;
    // Gets all of the styles as a string array in a PROPVARIANT (TMfPPROPVARIANT).
    // The caller must properly clear the PROPVARIANT to avoid memory leaks.

    function SetSelectedStyle(pwszStyle: LPCWSTR): HResult; stdcall;
    // Sets the current selected style to the provided style.

    function GetSelectedStyle(out ppwszStyle: LPWSTR): HResult; stdcall;
    // Gets the currently selected style.
    // The returned string must be deallocated using CoTaskMemFree().
  end;
  IID_IMFSAMIStyle = IMFSAMIStyle;
  {$EXTERNALSYM IID_IMFSAMIStyle}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  // Interface IMFTranscodeProfile
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTranscodeProfile);'}
  {$EXTERNALSYM IMFTranscodeProfile}
  IMFTranscodeProfile = interface(IUnknown)
  ['{4ADFDBA3-7AB0-4953-A62B-461E7FF3DA1E}']

    function SetAudioAttributes(pAttrs: IMFAttributes): HResult; stdcall;

    function GetAudioAttributes(out ppAttrs: IMFAttributes): HResult; stdcall;

    function SetVideoAttributes(pAttrs: IMFAttributes): HResult; stdcall;

    function GetVideoAttributes(out ppAttrs: IMFAttributes): HResult; stdcall;

    function SetContainerAttributes(pAttrs: IMFAttributes): HResult; stdcall;

    function GetContainerAttributes(out ppAttrs: IMFAttributes): HResult; stdcall;

  end;
  IID_IMFTranscodeProfile = IMFTranscodeProfile;
  {$EXTERNALSYM IID_IMFTranscodeProfile}

//#endif (WINVER >= _WIN32_WINNT_WIN7)


  PMF_TRANSCODE_TOPOLOGYMODE_FLAGS = ^MF_TRANSCODE_TOPOLOGYMODE_FLAGS;
  _MF_TRANSCODE_TOPOLOGYMODE_FLAGS             = (
    MF_TRANSCODE_TOPOLOGYMODE_SOFTWARE_ONLY    = 0,
    MF_TRANSCODE_TOPOLOGYMODE_HARDWARE_ALLOWED = 1
  );
  {$EXTERNALSYM _MF_TRANSCODE_TOPOLOGYMODE_FLAGS}
  MF_TRANSCODE_TOPOLOGYMODE_FLAGS = _MF_TRANSCODE_TOPOLOGYMODE_FLAGS;
  {$EXTERNALSYM MF_TRANSCODE_TOPOLOGYMODE_FLAGS}

  PMF_TRANSCODE_ADJUST_PROFILE_FLAGS = ^MF_TRANSCODE_ADJUST_PROFILE_FLAGS;
  _MF_TRANSCODE_ADJUST_PROFILE_FLAGS                  = (
    MF_TRANSCODE_ADJUST_PROFILE_DEFAULT               = 0,
    MF_TRANSCODE_ADJUST_PROFILE_USE_SOURCE_ATTRIBUTES = 1
  );
  {$EXTERNALSYM _MF_TRANSCODE_ADJUST_PROFILE_FLAGS}
  MF_TRANSCODE_ADJUST_PROFILE_FLAGS = _MF_TRANSCODE_ADJUST_PROFILE_FLAGS;
  {$EXTERNALSYM MF_TRANSCODE_ADJUST_PROFILE_FLAGS}

  PMF_VIDEO_PROCESSOR_ALGORITHM_TYPE = ^MF_VIDEO_PROCESSOR_ALGORITHM_TYPE;
  _MF_VIDEO_PROCESSOR_ALGORITHM_TYPE         = (
    MF_VIDEO_PROCESSOR_ALGORITHM_DEFAULT     = 0,
    MF_VIDEO_PROCESSOR_ALGORITHM_MRF_CRF_444 = 1
  );
  {$EXTERNALSYM _MF_VIDEO_PROCESSOR_ALGORITHM_TYPE}
  MF_VIDEO_PROCESSOR_ALGORITHM_TYPE = _MF_VIDEO_PROCESSOR_ALGORITHM_TYPE;
  {$EXTERNALSYM MF_VIDEO_PROCESSOR_ALGORITHM_TYPE}

  PMF_TRANSCODE_SINK_INFO = ^MF_TRANSCODE_SINK_INFO;
  _MF_TRANSCODE_SINK_INFO = record
    dwVideoStreamID: DWORD;
    pVideoMediaType: IMFMediaType;
    dwAudioStreamID: DWORD;
    pAudioMediaType: IMFMediaType;
  end;
  {$EXTERNALSYM _MF_TRANSCODE_SINK_INFO}
  MF_TRANSCODE_SINK_INFO = _MF_TRANSCODE_SINK_INFO;
  {$EXTERNALSYM MF_TRANSCODE_SINK_INFO}


  // Interface IMFTranscodeSinkInfoProvider
  // =======================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTranscodeSinkInfoProvider);'}
  {$EXTERNALSYM IMFTranscodeSinkInfoProvider}
  IMFTranscodeSinkInfoProvider = interface(IUnknown)
  ['{8CFFCD2E-5A03-4a3a-AFF7-EDCD107C620E}']

    function SetOutputFile(pwszFileName: LPCWSTR): HResult; stdcall;

    function SetOutputByteStream(pByteStreamActivate: IMFActivate): HResult; stdcall;

    function SetProfile(pProfile: IMFTranscodeProfile): HResult; stdcall;

    function GetSinkInfo(out pSinkInfo: MF_TRANSCODE_SINK_INFO): HResult; stdcall;

  end;
  IID_IMFTranscodeSinkInfoProvider = IMFTranscodeSinkInfoProvider;
  {$EXTERNALSYM IID_IMFTranscodeSinkInfoProvider}


  // Interface IMFFieldOfUseMFTUnlock
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFFieldOfUseMFTUnlock);'}
  {$EXTERNALSYM IMFFieldOfUseMFTUnlock}
  IMFFieldOfUseMFTUnlock = interface(IUnknown)
  ['{508E71D3-EC66-4fc3-8775-B4B9ED6BA847}']

    function Unlock(pUnkMFT: IUnknown): HResult; stdcall;

  end;
  IID_IMFFieldOfUseMFTUnlock = IMFFieldOfUseMFTUnlock;
  {$EXTERNALSYM IID_IMFFieldOfUseMFTUnlock}


  PMftRegistrationInfo = ^_MFT_REGISTRATION_INFO;
  PMFT_REGISTRATION_INFO = ^MFT_REGISTRATION_INFO;
  _MFT_REGISTRATION_INFO = record
    clsid: CLSID;
    guidCategory: TGUID;
    uiFlags: UINT32;
    pszName: PWideChar;
    cInTypes: DWORD;
    pInTypes: PMFT_REGISTER_TYPE_INFO;
    cOutTypes: DWORD;
    pOutTypes: PMFT_REGISTER_TYPE_INFO;
  end;
  {$EXTERNALSYM _MFT_REGISTRATION_INFO}
  MFT_REGISTRATION_INFO = _MFT_REGISTRATION_INFO;
  {$EXTERNALSYM MFT_REGISTRATION_INFO}


  // Interface IMFLocalMFTRegistration
  // =================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFLocalMFTRegistration);'}
  {$EXTERNALSYM IMFLocalMFTRegistration}
  IMFLocalMFTRegistration = interface(IUnknown)
  ['{149c4d73-b4be-4f8d-8b87-079e926b6add}']

    function RegisterMFTs(pMFTs: MFT_REGISTRATION_INFO;
                          cMFTs: DWord): HResult; stdcall;

  end;
  IID_IMFLocalMFTRegistration = IMFLocalMFTRegistration;
  {$EXTERNALSYM IID_IMFLocalMFTRegistration}

//#if (WINVER >= _WIN32_WINNT_WIN8)


  // Interface IMFCapturePhotoConfirmationInterface
  // ==============================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCapturePhotoConfirmation);'}
  {$EXTERNALSYM IMFCapturePhotoConfirmation}
  IMFCapturePhotoConfirmation = interface(IUnknown)
  ['{19f68549-ca8a-4706-a4ef-481dbc95e12c}']

    function SetPhotoConfirmationCallback(pNotificationCallback: IMFAsyncCallback): HResult; stdcall;

    // Sets the PhotoConfirmation pixel format
    function SetPixelFormat(subtype: TGUID): HResult; stdcall;

    // Returns the PhotoConfirmation pixel format
    function GetPixelFormat(out subtype: TGUID): HResult; stdcall;

  end;
  IID_IMFCapturePhotoConfirmation = IMFCapturePhotoConfirmation;
  {$EXTERNALSYM IID_IMFCapturePhotoConfirmation}


  // Interface IMFPMPHostApp
  // =======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMPHostApp);'}
  {$EXTERNALSYM IMFPMPHostApp}
  IMFPMPHostApp = interface(IUnknown)
    ['{84d2054a-3aa1-4728-a3b0-440a418cf49c}']

      function LockProcess(): HResult; stdcall;

      function UnlockProcess(): HResult; stdcall;

      function ActivateClassById(id: LPCWSTR;
                                 pStream: IStream;
                                 const riid: REFIID;
                                 out ppv): HResult; stdcall;

  end;
  IID_IMFPMPHostApp = IMFPMPHostApp;
  {$EXTERNALSYM IID_IMFPMPHostApp}



  // Interface IMFPMPClientApp
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMPClientApp);'}
  {$EXTERNALSYM IMFPMPClientApp}
  IMFPMPClientApp = interface(IUnknown)
    ['{c004f646-be2c-48f3-93a2-a0983eba1108}']

      function SetPMPHost(pPMPHost: IMFPMPHostApp): HResult; stdcall;

  end;
  IID_IMFPMPClientApp = IMFPMPClientApp;
  {$EXTERNALSYM IID_IMFPMPClientApp}


//#if (WINVER >= _WIN32_WINNT_WINBLUE)

  // Interface IMFMediaStreamSourceSampleRequest
  // ===========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaStreamSourceSampleRequest);'}
  {$EXTERNALSYM IMFMediaStreamSourceSampleRequest}
  IMFMediaStreamSourceSampleRequest = interface(IUnknown)
    ['{380b9af9-a85b-4e78-a2af-ea5ce645c6b4}']

      function SetSample(value: IMFSample): HResult; stdcall;

  end;
  IID_IMFMediaStreamSourceSampleRequest = IMFMediaStreamSourceSampleRequest;
  {$EXTERNALSYM IID_IMFMediaStreamSourceSampleRequest}


  // Interface IMFTrackedSample
  // ==========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTrackedSample);'}
  {$EXTERNALSYM IMFTrackedSample}
  IMFTrackedSample = interface(IUnknown)
    ['{245BF8E9-0755-40f7-88A5-AE0F18D55E17}']

      function SetAllocator(pSampleAllocator: IMFAsyncCallback;
                            pUnkState: IUnknown): HResult; stdcall;

  end;
  IID_IMFTrackedSample = IMFTrackedSample;
  {$EXTERNALSYM IID_IMFTrackedSample}


//#endif // (WINVER >= _WIN32_WINNT_WINBLUE)


  // Interface IMFProtectedEnvironmentAccess
  // =======================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFProtectedEnvironmentAccess);'}
  {$EXTERNALSYM IMFProtectedEnvironmentAccess}
  IMFProtectedEnvironmentAccess = interface(IUnknown)
    ['{ef5dc845-f0d9-4ec9-b00c-cb5183d38434}']

      function Call(inputLength: UINT32;
                    input: PByte;
                    outputLength: UINT32;
                    out output: PByte): HResult; stdcall;

      function ReadGRL(out outputLength: UINT32;
                       out output: PByte): HResult; stdcall;

  end;
  IID_IMFProtectedEnvironmentAccess = IMFProtectedEnvironmentAccess;
  {$EXTERNALSYM IID_IMFProtectedEnvironmentAccess}


  // Interface IMFSignedLibrary
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSignedLibrary);'}
  {$EXTERNALSYM IMFSignedLibrary}
  IMFSignedLibrary = interface(IUnknown)
    ['{4a724bca-ff6a-4c07-8e0d-7a358421cf06}']

      function GetProcedureAddress(name: LPCWSTR;
                                   out address): HResult; stdcall;

  end;
  IID_IMFSignedLibrary = IMFSignedLibrary;
  {$EXTERNALSYM IID_IMFSignedLibrary}


  // Interface IMFSystemId
  // ======================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSystemId);'}
  {$EXTERNALSYM IMFSystemId}
  IMFSystemId = interface(IUnknown)
    ['{fff4af3a-1fc1-4ef9-a29b-d26c49e2f31a}']

      function GetData(out size: UINT32;
                       out data: PByte): HResult; stdcall;

      function Setup(stage: UINT32;
                     cbIn: UINT32;
                     pbIn: PByte;
                     out pcbOut: UINT32;
                     out ppbOut: PByte): HResult; stdcall;

  end;
  IID_IMFSystemId = IMFSystemId;
  {$EXTERNALSYM IID_IMFSystemId}


  // Interface IMFContentProtectionDevice
  // ====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentProtectionDevice);'}
  {$EXTERNALSYM IMFContentProtectionDevice}
  IMFContentProtectionDevice = interface(IUnknown)
    ['{E6257174-A060-4C9A-A088-3B1B471CAD28}']

      function InvokeFunction(FunctionId: DWORD;
                              InputBufferByteCount: DWORD;
                              InputBuffer: PByte;
                              var OutputBufferByteCount: DWORD;
                              out OutputBuffer: PByte): HResult; stdcall;

      function GetPrivateDataByteCount(out PrivateInputByteCount: DWORD;
                                       out PrivateOutputByteCount: DWORD): HResult; stdcall;

  end;
  IID_IMFContentProtectionDevice = IMFContentProtectionDevice;
  {$EXTERNALSYM IID_IMFContentProtectionDevice}


  // Interface IMFContentDecryptorContext
  // ====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentDecryptorContext);'}
  {$EXTERNALSYM IMFContentDecryptorContext}
  IMFContentDecryptorContext = interface(IUnknown)
    ['{7EC4B1BD-43FB-4763-85D2-64FCB5C5F4CB}']

      function InitializeHardwareKey(InputPrivateDataByteCount: UINT;
                                     InputPrivateData: Pointer;
                                     out OutputPrivateData: UINT64): HResult; stdcall;

  end;
  IID_IMFContentDecryptorContext = IMFContentDecryptorContext;
  {$EXTERNALSYM IID_IMFContentDecryptorContext}


  PMFTIMER_FLAGS = ^MFTIMER_FLAGS;
  cwMFTIMER_FLAGS    = (
    MFTIMER_RELATIVE = $1
    {$EXTERNALSYM MFTIMER_RELATIVE}
  );
  {$EXTERNALSYM cwMFTIMER_FLAGS}
  MFTIMER_FLAGS = cwMFTIMER_FLAGS;
  {$EXTERNALSYM MFTIMER_FLAGS}


  PASF_FLAT_SYNCHRONISED_LYRICS = ^ASF_FLAT_SYNCHRONISED_LYRICS;
  _ASFFlatSynchronisedLyrics = record
    // Direct mapped fields
    bTimeStampFormat: PByte;
    bContentType: PByte;
    dwLyricsLen: DWord;
  end;
  {$EXTERNALSYM _ASFFlatSynchronisedLyrics}
  ASF_FLAT_SYNCHRONISED_LYRICS = _ASFFlatSynchronisedLyrics;
  {$EXTERNALSYM ASF_FLAT_SYNCHRONISED_LYRICS}


  PMF_MEDIAKEYSESSION_TYPE = ^MF_MEDIAKEYSESSION_TYPE;
  MF_MEDIAKEYSESSION_TYPE = (
    MF_MEDIAKEYSESSION_TYPE_TEMPORARY	                  = 0,
    MF_MEDIAKEYSESSION_TYPE_PERSISTENT_LICENSE	        = (MF_MEDIAKEYSESSION_TYPE_TEMPORARY + 1),
    MF_MEDIAKEYSESSION_TYPE_PERSISTENT_RELEASE_MESSAGE	= (MF_MEDIAKEYSESSION_TYPE_PERSISTENT_LICENSE + 1),
    MF_MEDIAKEYSESSION_TYPE_PERSISTENT_USAGE_RECORD	    = (MF_MEDIAKEYSESSION_TYPE_PERSISTENT_RELEASE_MESSAGE + 1 )
  );
  {$EXTERNALSYM MF_MEDIAKEYSESSION_TYPE}


  PMF_MEDIAKEY_STATUS = ^MF_MEDIAKEY_STATUS;
  MF_MEDIAKEY_STATUS = (
    MF_MEDIAKEY_STATUS_USABLE	            = 0,
    MF_MEDIAKEY_STATUS_EXPIRED	          = (MF_MEDIAKEY_STATUS_USABLE + 1),
    MF_MEDIAKEY_STATUS_OUTPUT_DOWNSCALED	= (MF_MEDIAKEY_STATUS_EXPIRED + 1),
    MF_MEDIAKEY_STATUS_OUTPUT_NOT_ALLOWED	= (MF_MEDIAKEY_STATUS_OUTPUT_DOWNSCALED + 1) ,
    MF_MEDIAKEY_STATUS_STATUS_PENDING	    = (MF_MEDIAKEY_STATUS_OUTPUT_NOT_ALLOWED + 1),
    MF_MEDIAKEY_STATUS_INTERNAL_ERROR	    = (MF_MEDIAKEY_STATUS_STATUS_PENDING + 1),
    MF_MEDIAKEY_STATUS_RELEASED	          = (MF_MEDIAKEY_STATUS_INTERNAL_ERROR + 1),
    MF_MEDIAKEY_STATUS_OUTPUT_RESTRICTED	= (MF_MEDIAKEY_STATUS_RELEASED + 1)
  );
  {$EXTERNALSYM MF_MEDIAKEY_STATUS}


  PMFMediaKeyStatus = ^MFMediaKeyStatus;
  MFMediaKeyStatus = record
    pbKeyId: PByte;
    cbKeyId: UINT;
    eMediaKeyStatus: MF_MEDIAKEY_STATUS;
  end;
  {$EXTERNALSYM MFMediaKeyStatus}


  PMF_MEDIAKEYSESSION_MESSAGETYPE = ^MF_MEDIAKEYSESSION_MESSAGETYPE;
  PMfMediakeysessionMessagetype = ^MF_MEDIAKEYSESSION_MESSAGETYPE;
  MF_MEDIAKEYSESSION_MESSAGETYPE                             = (
    MF_MEDIAKEYSESSION_MESSAGETYPE_LICENSE_REQUEST           = 0,
    MF_MEDIAKEYSESSION_MESSAGETYPE_LICENSE_RENEWAL           = 1,
    MF_MEDIAKEYSESSION_MESSAGETYPE_LICENSE_RELEASE           = 2,
    MF_MEDIAKEYSESSION_MESSAGETYPE_INDIVIDUALIZATION_REQUEST = 3
  );
  {$EXTERNALSYM MF_MEDIAKEYSESSION_MESSAGETYPE}

  // This enum maps to the W3C cross origin settings (CORS) attribute used by the HTML5 media element:
  //  * Omitted Attribute -- No CORS state.
  //  * Anonymous -- Requests for the element will have their mode set to "cors" and their credentials mode set to "same-origin".
  //  * Use Credentials -- Requests for the element will have their mode set to "cors" and their credentials mode set to "include".


  PMF_CROSS_ORIGIN_POLICY = ^_MF_CROSS_ORIGIN_POLICY;
  PMfCrossOriginPolicy = ^_MF_CROSS_ORIGIN_POLICY;
  _MF_CROSS_ORIGIN_POLICY                  = (
    MF_CROSS_ORIGIN_POLICY_NONE            = 0,
    MF_CROSS_ORIGIN_POLICY_ANONYMOUS       = 1,
    MF_CROSS_ORIGIN_POLICY_USE_CREDENTIALS = 2
  );
  {$EXTERNALSYM _MF_CROSS_ORIGIN_POLICY}
  MF_CROSS_ORIGIN_POLICY = _MF_CROSS_ORIGIN_POLICY;
  {$EXTERNALSYM MF_CROSS_ORIGIN_POLICY}


// #if (WINVER >= _WIN32_WINNT_WIN10)


  PMF_VIDEO_SPHERICAL_VIEWDIRECTION = ^_MF_VIDEO_SPHERICAL_VIEWDIRECTION;
  PMfVideoSphericalViewdirection = ^_MF_VIDEO_SPHERICAL_VIEWDIRECTION;
  _MF_VIDEO_SPHERICAL_VIEWDIRECTION = record
    iHeading: Integer;
    iPitch: Integer;
    iRoll: Integer;
  end;
  {$EXTERNALSYM _MF_VIDEO_SPHERICAL_VIEWDIRECTION}
  MF_VIDEO_SPHERICAL_VIEWDIRECTION = _MF_VIDEO_SPHERICAL_VIEWDIRECTION;
  {$EXTERNALSYM MF_VIDEO_SPHERICAL_VIEWDIRECTION}



  // Interface IMFNetCrossOriginSupport
  // ==================================
  // This interface is implemented by clients that want to enforce a cross origin policy for HTML5 media downloads.
  // The Media Foundation network code uses these client callbacks to implement and enforce cross origin downloads.
  //  * GetCrossOriginPolicy() returns the client's current cross origin policy to apply to the download session.
  //  * GetSourceOrigin() returns the W3C origin of the HTML5 media element.  Use CoTaskMemFree to free the string.
  //  * IsSameOrigin() returns true when the specified URL has the same origin as the HTML5 media element.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFNetCrossOriginSupport);'}
  {$EXTERNALSYM IMFNetCrossOriginSupport}
  IMFNetCrossOriginSupport = interface(IUnknown)
  ['{bc2b7d44-a72d-49d5-8376-1480dee58b22}']

    function GetCrossOriginPolicy(out pPolicy: MF_CROSS_ORIGIN_POLICY): HResult; stdcall;

    function GetSourceOrigin(out wszSourceOrigin: LPWSTR): HResult; stdcall;

    function IsSameOrigin(wszURL: LPWSTR;
                          out pfIsSameOrigin: BOOL): HResult; stdcall;

  end;
  IID_IMFNetCrossOriginSupport = IMFNetCrossOriginSupport;
  {$EXTERNALSYM IID_IMFNetCrossOriginSupport}


  // Interface IMFHttpDownloadRequest
  // ===============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFHttpDownloadRequest);'}
  {$EXTERNALSYM IMFHttpDownloadRequest}
  IMFHttpDownloadRequest = interface(IUnknown)
  ['{F779FDDF-26E7-4270-8A8B-B983D1859DE0}']

    function AddHeader(szHeader: LPWSTR): HResult; stdcall;

    function BeginSendRequest(pbPayload: PByte;
                              cbPayload: ULONG;
                              var pCallback: IMFAsyncCallback;
                              var punkState: IUnknown): HResult; stdcall;

    function EndSendRequest(const pResult: IMFAsyncResult): HResult; stdcall;

    function BeginReceiveResponse(pCallback: IMFAsyncCallback;
                                  punkState: IUnknown): HResult; stdcall;

    function EndReceiveResponse(pResult: IMFAsyncResult): HResult; stdcall;

    function BeginReadPayload(out pb: PByte;
                              cb: ULONG;
                              pCallback: IMFAsyncCallback;
                              punkState: IUnknown): HResult; stdcall;

    function EndReadPayload(pResult: IMFAsyncResult;
                            out pqwOffset: QWORD;
                            out pcbRead: ULONG): HResult; stdcall;

    function QueryHeader(szHeaderName: LPWSTR;
                         dwIndex: DWORD;
                         out ppszHeaderValue: LPWSTR): HResult; stdcall;

    function GetURL(out ppszURL: PWideChar): HResult; stdcall;

    function HasNullSourceOrigin(out pfNullSourceOrigin: BOOL): HResult; stdcall;

    function GetTimeSeekResult(out pqwStartTime: QWORD;
                               out pqwStopTime: QWORD;
                               out pqwDuration: QWORD): HResult; stdcall;

    function GetHttpStatus(out pdwHttpStatus: DWORD): HResult; stdcall;

    function GetAtEndOfPayload(out pfAtEndOfPayload: BOOL): HResult; stdcall;

    function GetTotalLength(out pqwTotalLength: QWORD): HResult; stdcall;

    function GetRangeEndOffset(out pqwRangeEnd: QWORD): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_IMFHttpDownloadRequest = IMFHttpDownloadRequest;
  {$EXTERNALSYM IID_IMFHttpDownloadRequest}


  // Interface IMFHttpDownloadSession
  // ================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFHttpDownloadSession);'}
  {$EXTERNALSYM IMFHttpDownloadSession}
  IMFHttpDownloadSession = interface(IUnknown)
  ['{71FA9A2C-53CE-4662-A132-1A7E8CBF62DB}']

    function SetServer(szServerName: LPWSTR;
                       nPort: DWORD): HResult; stdcall;

    function CreateRequest(szObjectName: LPWSTR;
                           fBypassProxyCache: BOOL;
                           fSecure: BOOL;
                           szVerb: LPWSTR;
                           szReferrer: LPWSTR;
                           ppRequest: PIMFHttpDownloadRequest): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_IMFHttpDownloadSession = IMFHttpDownloadSession;
  {$EXTERNALSYM IID_IMFHttpDownloadSession}


  // Interface IMFHttpDownloadSessionProvider
  // ========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFHttpDownloadSessionProvider);'}
  {$EXTERNALSYM IMFHttpDownloadSessionProvider}
  IMFHttpDownloadSessionProvider = interface(IUnknown)
  ['{1B4CF4B9-3A16-4115-839D-03CC5C99DF01}']

    function CreateHttpDownloadSession(wszScheme: LPWSTR;
                                       out ppDownloadSession: IMFHttpDownloadSession): HResult; stdcall;

  end;
  IID_IMFHttpDownloadSessionProvider = IMFHttpDownloadSessionProvider;
  {$EXTERNALSYM IID_IMFHttpDownloadSessionProvider}


  // Interface IMFMediaSource2
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSource2);'}
  {$EXTERNALSYM IMFMediaSource2}
  IMFMediaSource2 = interface(IMFMediaSourceEx)
  ['{FBB03414-D13B-4786-8319-5AC51FC0A136}']

    function SetMediaType(const dwStreamID: DWORD;
                          pMediaType: IMFMediaType): HResult; stdcall;

  end;
  IID_IMFMediaSource2 = IMFMediaSource2;
  {$EXTERNALSYM IID_IMFMediaSource2}


  // Interface IMFMediaStream2
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaStream2);'}
  {$EXTERNALSYM IMFMediaStream2}
  IMFMediaStream2 = interface(IMFMediaStream)
  ['{C5BC37D6-75C7-46A1-A132-81B5F723C20F}']

    function SetStreamState(value: MF_STREAM_STATE): HResult; stdcall;

    function GetStreamState(out value: MF_STREAM_STATE): HResult; stdcall;

  end;
  IID_IMFMediaStream2 = IMFMediaStream2;
  {$EXTERNALSYM IID_IMFMediaStream2}

//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)

  PMFSensorDeviceType = ^_MFSensorDeviceType;
  _MFSensorDeviceType = (
    MFSensorDeviceType_Unknown         = 0,
    MFSensorDeviceType_Device          = (MFSensorDeviceType_Unknown  + 1),
    MFSensorDeviceType_MediaSource     = (MFSensorDeviceType_Device  + 1),
    MFSensorDeviceType_FrameProvider   = (MFSensorDeviceType_MediaSource  + 1),
    MFSensorDeviceType_SensorTransform = (MFSensorDeviceType_FrameProvider  + 1)
  );
  {$EXTERNALSYM _MFSensorDeviceType}
  MFSensorDeviceType = _MFSensorDeviceType;
  {$EXTERNALSYM MFSensorDeviceType}


  PMFSensorStreamType = ^_MFSensorStreamType;
  _MFSensorStreamType = (
    MFSensorStreamType_Unknown = 0,
    MFSensorStreamType_Input   = (MFSensorStreamType_Unknown  + 1),
    MFSensorStreamType_Output  = (MFSensorStreamType_Input  + 1)
  );
  {$EXTERNALSYM _MFSensorStreamType}
  MFSensorStreamType = _MFSensorStreamType;
  {$EXTERNALSYM MFSensorStreamType}


  PMFSensorDeviceMode = ^_MFSensorDeviceMode;
  _MFSensorDeviceMode = (
    MFSensorDeviceMode_Controller = 0,
    MFSensorDeviceMode_Shared     = (MFSensorDeviceMode_Controller  + 1)
  );
  {$EXTERNALSYM _MFSensorDeviceMode}
  MFSensorDeviceMode = _MFSensorDeviceMode;
  {$EXTERNALSYM MFSensorDeviceMode}


//endif // (WINVER >= _WIN32_WINNT_WINTHRESHOLD)

// WIN10  April 2018 update
  PSENSORPROFILEID = ^SENSORPROFILEID;
  SENSORPROFILEID = record
    _Type: TGUID;
    Index: UINT32;
    Unused: UINT32;
  end;
  {$EXTERNALSYM SENSORPROFILEID}

// WIN10 October 2018 update Redstone 5

  PMFCameraIntrinsicCameraModel = ^TMFCameraIntrinsicCameraModel;
  _MFCameraIntrinsic_CameraModel = record
    FocalLength_x: FLOAT;
    FocalLength_y: FLOAT;
    PrincipalPoint_x: FLOAT;
    PrincipalPoint_y: FLOAT;
  end;
  {$EXTERNALSYM _MFCameraIntrinsic_CameraModel}
  MFCameraIntrinsic_CameraModel = _MFCameraIntrinsic_CameraModel;
  {$EXTERNALSYM MFCameraIntrinsic_CameraModel}
  TMFCameraIntrinsicCameraModel = _MFCameraIntrinsic_CameraModel;
  {$EXTERNALSYM TMFCameraIntrinsicCameraModel}


  PMFCameraIntrinsicDistortionModel6KT = ^MFCameraIntrinsic_DistortionModel6KT;
  _MFCameraIntrinsic_DistortionModel6KT = record
    Radial_k1: FLOAT;
    Radial_k2: FLOAT;
    Radial_k3: FLOAT;
    Radial_k4: FLOAT;
    Radial_k5: FLOAT;
    Radial_k6: FLOAT;
    Tangential_p1: FLOAT;
    Tangential_p2: FLOAT;
  end;
  {$EXTERNALSYM _MFCameraIntrinsic_DistortionModel6KT}
  MFCameraIntrinsic_DistortionModel6KT = _MFCameraIntrinsic_DistortionModel6KT;
  {$EXTERNALSYM MFCameraIntrinsic_DistortionModel6KT}


  PMFCameraIntrinsicDistortionModelArcTan = ^MFCameraIntrinsic_DistortionModelArcTan;
  _MFCameraIntrinsic_DistortionModelArcTan = record
    Radial_k0: FLOAT;
    DistortionCenter_x: FLOAT;
    DistortionCenter_y: FLOAT;
    Tangential_x: FLOAT;
    Tangential_y: FLOAT;
  end;
  {$EXTERNALSYM _MFCameraIntrinsic_DistortionModelArcTan}
  MFCameraIntrinsic_DistortionModelArcTan = _MFCameraIntrinsic_DistortionModelArcTan;
  {$EXTERNALSYM MFCameraIntrinsic_DistortionModelArcTan}


  PMFCameraIntrinsicDistortionModelType = ^MFCameraIntrinsic_DistortionModelType;
  _MFCameraIntrinsic_DistortionModelType         = (
    MFCameraIntrinsic_DistortionModelType_6KT    = 0,
    MFCameraIntrinsic_DistortionModelType_ArcTan = (MFCameraIntrinsic_DistortionModelType_6KT  + 1)
  );
  {$EXTERNALSYM _MFCameraIntrinsic_DistortionModelType}
  MFCameraIntrinsic_DistortionModelType = _MFCameraIntrinsic_DistortionModelType;
  {$EXTERNALSYM MFCameraIntrinsic_DistortionModelType}


  PMFExtendedCameraIntrinsicIntrinsicModel = ^MFExtendedCameraIntrinsic_IntrinsicModel;
  _MFExtendedCameraIntrinsic_IntrinsicModel = record
    Width: UINT32;
    Height: UINT32;
    SplitFrameId: UINT32;
    CameraModel: MFCameraIntrinsic_CameraModel;
  end;
  {$EXTERNALSYM _MFExtendedCameraIntrinsic_IntrinsicModel}
  MFExtendedCameraIntrinsic_IntrinsicModel = _MFExtendedCameraIntrinsic_IntrinsicModel;
  {$EXTERNALSYM MFExtendedCameraIntrinsic_IntrinsicModel}

 //END  WIN10 October 2018 update Redstone 5


//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)


  // Interface IMFSensorDevice
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorDevice);'}
  {$EXTERNALSYM IMFSensorDevice}
  IMFSensorDevice = interface(IUnknown)
  ['{FB9F48F2-2A18-4E28-9730-786F30F04DC4}']

    function GetDeviceId(out pDeviceId: ULONGLONG): HResult; stdcall;

    function GetDeviceType(out pType: MFSensorDeviceType): HResult; stdcall;

    function GetFlags(out pFlags: ULONGLONG): HResult; stdcall;

    function GetSymbolicLink(out SymbolicLink: LPWSTR;
                             cchSymbolicLink: LONG;
                             out pcchWritten: LONG): HResult; stdcall;

    function GetDeviceAttributes(out ppAttributes: IMFAttributes): HResult; stdcall;

    function GetStreamAttributesCount(eType: MFSensorStreamType;
                                      out pdwCount: DWORD): HResult; stdcall;

    function GetStreamAttributes(eType: MFSensorStreamType;
                                 dwIndex: DWORD;
                                 out ppAttributes: IMFAttributes): HResult; stdcall;

    function SetSensorDeviceMode(eMode: MFSensorDeviceMode): HResult; stdcall;

    function GetSensorDeviceMode(out peMode: MFSensorDeviceMode): HResult; stdcall;

  end;
  IID_IMFSensorDevice = IMFSensorDevice;
  {$EXTERNALSYM IID_IMFSensorDevice}


  // Interface IMFSensorGroup
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorGroup);'}
  {$EXTERNALSYM IMFSensorGroup}
  IMFSensorGroup = interface(IUnknown)
  ['{4110243A-9757-461F-89F1-F22345BCAB4E}']

    function GetSymbolicLink(out SymbolicLink: LPWSTR;
                             cchSymbolicLink: LONG;
                             out pcchWritten: LONG): HResult; stdcall;

    function GetFlags(out pFlags: ULONGLONG): HResult; stdcall;

    function GetSensorGroupAttributes(out ppAttributes: IMFAttributes): HResult; stdcall;

    function GetSensorDeviceCount(out pdwCount: DWORD): HResult; stdcall;

    function GetSensorDevice(dwIndex: DWORD;
                             out ppDevice: IMFSensorDevice): HResult; stdcall;

    function SetDefaultSensorDeviceIndex(const dwIndex: DWORD): HResult; stdcall;

    function GetDefaultSensorDeviceIndex(out pdwIndex: DWORD): HResult; stdcall;

    function CreateMediaSource(out ppSource: IMFMediaSource): HResult; stdcall;

  end;
  IID_IMFSensorGroup = IMFSensorGroup;
  {$EXTERNALSYM IID_IMFSensorGroup}


  // Interface IMFSensorStream
  // =========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorStream);'}
  {$EXTERNALSYM IMFSensorStream}
  IMFSensorStream = interface(IMFAttributes)
  ['{E9A42171-C56E-498A-8B39-EDA5A070B7FC}']

    function GetMediaTypeCount(out pdwCount: DWORD): HResult; stdcall;

    function GetMediaType(dwIndex: DWORD;
                          out ppMediaType: IMFMediaType): HResult; stdcall;

    function CloneSensorStream(out ppStream: IMFSensorStream): HResult; stdcall;

  end;
  IID_IMFSensorStream = IMFSensorStream;
  {$EXTERNALSYM IID_IMFSensorStream}


  // interface IMFSensorTransformFactory
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorTransformFactory);'}
  {$EXTERNALSYM IMFSensorTransformFactory}
  IMFSensorTransformFactory = interface(IUnknown)
  ['{EED9C2EE-66B4-4F18-A697-AC7D3960215C}']

    function GetFactoryAttributes(out ppAttributes: IMFAttributes): HResult; stdcall;

    function InitializeFactory(dwMaxTransformCount: DWORD;
                               pSensorDevices: IMFCollection;
                               pAttributes: IMFAttributes): HResult; stdcall;

    function GetTransformCount(out pdwCount: DWORD): HResult; stdcall;

    function GetTransformInformation(TransformIndex: DWORD;
                                     out pguidTransformId: TGUID;
                                     out ppAttributes: IMFAttributes;
                                     out ppStreamInformation: IMFCollection): HResult; stdcall;

    function CreateTransform(const guidSensorTransformID: REFGUID;
                             pAttributes: IMFAttributes;
                             out ppDeviceMFT: IMFDeviceTransform {see: MfPack.MFTransform}): HResult; stdcall;

  end;
  IID_IMFSensorTransformFactory = IMFSensorTransformFactory;
  {$EXTERNALSYM IID_IMFSensorTransformFactory}


// WIN10 April 2018 update

  // Interface IMFSensorProfile
  // ===========================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorProfile);'}
  {$EXTERNALSYM IMFSensorProfile}
  IMFSensorProfile = interface(IUnknown)
  ['{22F765D1-8DAB-4107-846D-56BAF72215E7}']

    function GetProfileId(out pId: SENSORPROFILEID): HRESULT; stdcall;

    function AddProfileFilter(StreamId: UINT32;
                              wzFilterSetString: LPWSTR): HRESULT; stdcall;

    function IsMediaTypeSupported(StreamId: UINT32;
                                  pMediaType: IMFMediaType;
                                  out  pfSupported: BOOL): HRESULT; stdcall;

    function AddBlockedControl(wzBlockedControl: LPWSTR): HRESULT; stdcall;

  end;
  IID_IMFSensorProfile = IMFSensorProfile;
  {$EXTERNALSYM IID_IMFSensorProfile}


  // Interface IMFSensorProfileCollection
  // ====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorProfileCollection);'}
  {$EXTERNALSYM IMFSensorProfileCollection}
  IMFSensorProfileCollection = interface(IUnknown)
  ['{C95EA55B-0187-48BE-9353-8D2507662351}']

      function GetProfileCount(): DWORD; stdcall;

      function GetProfile(Index: DWORD;
                          out ppProfile: IMFSensorProfile): HRESULT; stdcall;

      function AddProfile(pProfile: IMFSensorProfile): HRESULT; stdcall;

      function FindProfile(ProfileId: PSENSORPROFILEID;
                           out ppProfile: IMFSensorProfile): HRESULT; stdcall;

      function RemoveProfileByIndex(Index: DWORD): HRESULT; stdcall;

      function RemoveProfile(ProfileId: SENSORPROFILEID): HRESULT; stdcall;
  end;
  IID_IMFSensorProfileCollection = IMFSensorProfileCollection;
  {$EXTERNALSYM IID_IMFSensorProfileCollection}

// END WIN10 April 2018 update


  // Interface IMFSensorProcessActivity
  // ==================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorProcessActivity);'}
  {$EXTERNALSYM IMFSensorProcessActivity}
  IMFSensorProcessActivity = interface(IUnknown)
  ['{39DC7F4A-B141-4719-813C-A7F46162A2B8}']

    function GetProcessId(out pPID: ULONG): HResult; stdcall;

    function GetStreamingState(out pfStreaming: BOOL): HResult; stdcall;

    function GetStreamingMode(out pMode: MFSensorDeviceMode): HResult; stdcall;

    function GetReportTime(out pft: FILETIME): HResult; stdcall;

  end;
  IID_IMFSensorProcessActivity = IMFSensorProcessActivity;
  {$EXTERNALSYM IID_IMFSensorProcessActivity}


  // Interface IMFSensorActivityReport
  // ==================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorActivityReport);'}
  {$EXTERNALSYM IMFSensorActivityReport}
  IMFSensorActivityReport = interface(IUnknown)
  ['{3E8C4BE1-A8C2-4528-90DE-2851BDE5FEAD}']

    function GetFriendlyName(out FriendlyName: LPWSTR;
                             cchFriendlyName: ULONG;
                             out pcchWritten: ULONG): HResult; stdcall;

    function GetSymbolicLink(out SymbolicLink: LPWSTR;
                             cchSymbolicLink: ULONG;
                             out pcchWritten: ULONG): HResult; stdcall;

    function GetProcessCount(out pcCount: ULONG): HResult; stdcall;

    function GetProcessActivity(Index: ULONG;
                                out ppProcessActivity: IMFSensorProcessActivity): HResult; stdcall;

  end;
  IID_IMFSensorActivityReport = IMFSensorActivityReport;
  {$EXTERNALSYM IID_IMFSensorActivityReport}


  // Interface IMFSensorActivitiesReport
  // ===================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorActivitiesReport);'}
  {$EXTERNALSYM IMFSensorActivitiesReport}
  IMFSensorActivitiesReport = interface(IUnknown)
  ['{683F7A5E-4A19-43CD-B1A9-DBF4AB3F7777}']

    function GetCount(out pcCount: ULONG): HResult; stdcall;

    function GetActivityReport(Index: ULONG;
                               out sensorActivityReport: IMFSensorActivityReport): HResult; stdcall;

    function GetActivityReportByDeviceName(SymbolicName: LPWSTR;
                                           out sensorActivityReport: IMFSensorActivityReport): HResult; stdcall;

  end;
  IID_IMFSensorActivitiesReport = IMFSensorActivitiesReport;
  {$EXTERNALSYM IID_IMFSensorActivitiesReport}


  // Interface IMFSensorActivitiesReportCallback
  // ===========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorActivitiesReportCallback);'}
  {$EXTERNALSYM IMFSensorActivitiesReportCallback}
  IMFSensorActivitiesReportCallback = interface(IUnknown)
  ['{DE5072EE-DBE3-46DC-8A87-B6F631194751}']

    function OnActivitiesReport(sensorActivitiesReport: IMFSensorActivitiesReport): HResult; stdcall;

  end;
  IID_IMFSensorActivitiesReportCallback = IMFSensorActivitiesReportCallback;
  {$EXTERNALSYM IID_IMFSensorActivitiesReportCallback}


  // Interface IMFSensorActivityMonitor
  // ==================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSensorActivityMonitor);'}
  {$EXTERNALSYM IMFSensorActivityMonitor}
  IMFSensorActivityMonitor = interface(IUnknown)
  ['{D0CEF145-B3F4-4340-A2E5-7A5080CA05CB}']

    function Start(): HResult; stdcall;

    function Stop(): HResult; stdcall;

  end;
  IID_IMFSensorActivityMonitor = IMFSensorActivityMonitor;
  {$EXTERNALSYM IID_IMFSensorActivityMonitor}

 //#endif // (WINVER >= _WIN32_WINNT_WINTHRESHOLD)


  // WIN10 October 2018 update Redstone 5

  // Interface IMFExtendedCameraIntrinsicModel
  // =========================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedCameraIntrinsicModel);'}
  {$EXTERNALSYM IMFExtendedCameraIntrinsicModel}
  IMFExtendedCameraIntrinsicModel = interface(IUnknown)
  ['{5C595E64-4630-4231-855A-12842F733245}']

    function GetModel(out pIntrinsicModel: MFExtendedCameraIntrinsic_IntrinsicModel): HResult; stdcall;

    function SetModel(pIntrinsicModel: MFExtendedCameraIntrinsic_IntrinsicModel): HResult; stdcall;

    function GetDistortionModelType(Out pDistortionModelType: MFCameraIntrinsic_DistortionModelType): HResult; stdcall;

  end;
  IID_IMFExtendedCameraIntrinsicModel = IMFExtendedCameraIntrinsicModel;
  {$EXTERNALSYM IID_IMFExtendedCameraIntrinsicModel}


  // Interface IMFExtendedCameraIntrinsicsDistortionModel6KT
  // =======================================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedCameraIntrinsicsDistortionModel6KT);'}
  {$EXTERNALSYM IMFExtendedCameraIntrinsicsDistortionModel6KT}
  IMFExtendedCameraIntrinsicsDistortionModel6KT = interface(IUnknown)
  ['{74C2653B-5F55-4EB1-9F0F-18B8F68B7D3D}']

    function GetDistortionModel(out pDistortionModel: MFCameraIntrinsic_DistortionModel6KT): HResult; stdcall;

    function SetDistortionModel(pDistortionModel: MFCameraIntrinsic_DistortionModel6KT): HResult; stdcall;

  end;
  IID_IMFExtendedCameraIntrinsicsDistortionModel6KT = IMFExtendedCameraIntrinsicsDistortionModel6KT;
  {$EXTERNALSYM IID_IMFExtendedCameraIntrinsicsDistortionModel6KT}


  // Interface IMFExtendedCameraIntrinsicsDistortionModelArcTan
  // ==========================================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedCameraIntrinsicsDistortionModelArcTan);'}
  {$EXTERNALSYM IMFExtendedCameraIntrinsicsDistortionModelArcTan}
  IMFExtendedCameraIntrinsicsDistortionModelArcTan = interface(IUnknown)
  ['{812D5F95-B572-45DC-BAFC-AE24199DDDA8}']

    function GetDistortionModel(out pDistortionModel: MFCameraIntrinsic_DistortionModelArcTan): HResult; stdcall;

    function SetDistortionModel(pDistortionModel: MFCameraIntrinsic_DistortionModelArcTan): HResult; stdcall;

  end;
  IID_IMFExtendedCameraIntrinsicsDistortionModelArcTan = IMFExtendedCameraIntrinsicsDistortionModelArcTan;
  {$EXTERNALSYM IID_IMFExtendedCameraIntrinsicsDistortionModelArcTan}


  // Interface IMFExtendedCameraIntrinsics
  // =====================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedCameraIntrinsics);'}
  {$EXTERNALSYM IMFExtendedCameraIntrinsics}
  IMFExtendedCameraIntrinsics = interface(IUnknown)
  ['{687F6DAC-6987-4750-A16A-734D1E7A10FE}']

    function InitializeFromBuffer(pbBuffer: PBYTE;
                                  dwBufferSize: DWORD): HResult; stdcall;

    function GetBufferSize(out pdwBufferSize: DWORD): HResult; stdcall;

    function SerializeToBuffer(out pbBuffer: PByte;
                               var pdwBufferSize: DWORD): HResult; stdcall;

    function GetIntrinsicModelCount(out pdwCount: DWORD): HResult; stdcall;

    function GetIntrinsicModelByIndex(dwIndex: DWORD;
                                      out ppIntrinsicModel: IMFExtendedCameraIntrinsicModel): HResult; stdcall;

    function AddIntrinsicModel(pIntrinsicModel: IMFExtendedCameraIntrinsicModel): HResult; stdcall;

  end;
  IID_IMFExtendedCameraIntrinsics = IMFExtendedCameraIntrinsics;
  {$EXTERNALSYM IID_IMFExtendedCameraIntrinsics}

// End WIN10 Redstone 5


// NTDDI_VERSION >= NTDDI_WIN10_VB   // Win10 may 2020 update


  // Interface IMFExtendedCameraControl
  // This interface is used to configure the camera's extended properties.
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedCameraControl);'}
  {$EXTERNALSYM IMFExtendedCameraControl}
  IMFExtendedCameraControl = interface(IUnknown)
  ['{38E33520-FCA1-4845-A27A-68B7C6AB3789}']
    // <summary>
    // Queries for property capabilities.
    // </summary>
    // <return>
    // ULONGLONG mapping to property capabilities (KSCAMERA_EXTENDEDPROP_*) defined in ksmedia.h
    // </return>
    function GetCapabilities(): ULONGLONG; stdcall;

    /// <summary>
    /// Sets the property flags.
    /// </summary>
    /// <param name="ulFlags">
    /// ULONGLONG mapping to property flags (KSCAMERA_EXTENDEDPROP_*) defined in ksmedia.h
    /// </param>
    function SetFlags(ulFlags: ULONGLONG): HRESULT; stdcall;

    /// <summary>
    /// Queries for property flags.
    /// </summary>
    /// <return>
    /// ULONGLONG mapping to property flags (KSCAMERA_EXTENDEDPROP_*) defined in ksmedia.h
    /// </return>
    function GetFlags(): ULONGLONG; stdcall;

    /// <summary>
    /// Locks the internal payload buffer for query/changes.
    /// </summary>
    /// <param name="ppPayload">
    /// BYTE pointer to the buffer containing the raw payload.
    /// Caller should not free the buffer but instead call UnlockPayload
    /// </param>
    /// <param name="pulPayload">
    /// Size of the payload.
    /// </param>
    function LockPayload(out ppPayload: PByte;
                         out pulPayload: ULONG): HRESULT; stdcall;

    /// <summary>
    /// Unlocks the raw payload contained in this control.
    /// </summary>
    function UnlockPayload(): HRESULT; stdcall;

    /// <summary>
    /// Commits the configured control settings to the camera driver.
    /// </summary>
    function CommitSettings(): HRESULT; stdcall;

  end;
  IID_IMFExtendedCameraControl = IMFExtendedCameraControl;
  {$EXTERNALSYM IID_IMFExtendedCameraControl}



  // Interface IMFExtendedCameraController
  // This interface is used to configure the camera's extended properties.
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedCameraController);'}
  {$EXTERNALSYM IMFExtendedCameraController}
  IMFExtendedCameraController = interface(IUnknown)
  ['{B91EBFEE-CA03-4AF4-8A82-A31752F4A0FC}']
    /// <summary>
    /// Allows an app to get the current camera's extended property controls
    /// </summary>
    /// <param name = "dwStreamIndex">
    /// Indicates stream index to use for this property.
    /// MF_CAPTURE_ENGINE_MEDIASOURCE indicates a filter level property.
    /// </param>
    /// <param name = "ulPropertyId">
    /// ID index for identifying the property within KSPROPERTYSETID_ExtendedCameraControl.
    /// </param>
    /// <param name = "ppControl">
    /// IMFExtendedCameraControl pointer, representing the control.
    /// </param>
    function GetExtendedCameraControl(dwStreamIndex: DWORD;
                                      ulPropertyId: ULONG;
                                      out ppControl: IMFExtendedCameraControl): HRESULT; stdcall;

  end;
  IID_IMFExtendedCameraController = IMFExtendedCameraController;
  {$EXTERNALSYM IID_IMFExtendedCameraController}


  // Interface IMFRelativePanelReport
  // This interface is used to retrieve the relative panel current value.
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRelativePanelReport);'}
  {$EXTERNALSYM IMFRelativePanelReport}
  IMFRelativePanelReport = interface(IUnknown)
  ['{F25362EA-2C0E-447F-81E2-755914CDC0C3}']
    // <summary>
    // Allows an app to get the current relative panel value
    // </summary>
    // <param name = "panel">
    // Indicates the panel associated with the camera's relative facing to the displayRegion, using a
    // value from the ACPI_PLD_PANEL enumeration. Only the enumeration values of AcpiPldPanelFront,
    // AcpiPldPanelBack, and AcpiPldPanelUnknown are expected.
    // </param>
    function GetRelativePanel(out panel: ULONG): HRESULT; stdcall;
  end;
  IID_IMFRelativePanelReport = IMFRelativePanelReport;
  {$EXTERNALSYM IID_IMFRelativePanelReport}


  // Interface IMFRelativePanelWatcher
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRelativePanelWatcher);'}
  {$EXTERNALSYM IMFRelativePanelWatcher}
  IMFRelativePanelWatcher = interface(IMFShutdown)
  ['{421AF7F6-573E-4AD0-8FDA-2E57CEDB18C6}']
    function BeginGetReport(pCallback: IMFAsyncCallback;
                            pState: IUnknown): HRESULT; stdcall;

    function EndGetReport(pResult: IMFAsyncResult;
                          out ppRelativePanelReport: IMFRelativePanelReport): HRESULT; stdcall;

    // <summary>
    // Allows an app to retrieve the report of the current relative panel value
    // </summary>
    // <param name = "ppRelativePanelReport">
    // IMFRelativePanelReport pointer, representing the panel value.
    // </param>
    function GetReport(out ppRelativePanelReport: IMFRelativePanelReport): HRESULT; stdcall;
  end;

// end NTDDI_VERSION >= NTDDI_WIN10_VB


  // Interface IMFVideoCaptureSampleAllocator
  // The IMFVideoCaptureSampleAllocator interface is a specialized
  // sample allocator for video capture devices.
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoCaptureSampleAllocator);'}
  {$EXTERNALSYM IMFVideoCaptureSampleAllocator}
  IMFVideoCaptureSampleAllocator = interface(IMFVideoSampleAllocator)
  ['{725B77C7-CA9F-4FE5-9D72-9946BF9B3C70}']
    // <param name="cbSampleSize">
    // Sample size in bytes.
    // The actual sample size used by the allocator is the maximum of
    // the size required by pMediaType and cbSampleSize.
    // </param>
    // <param name="cbCaptureMetadataSize">
    // Capture metadata size in bytes. Applies only to callers that want to
    // to include additional metadata with the captured frames. The metadata size
    // should include the size of a KSCAMERA_METADATA_ITEMHEADER in addition to
    // the size of the metadata payload itself.
    // </param>
    // <param name="cbAlignment">
    // Buffer alignment size in bytes.
    // The default and minimum alignment size is 4KB. Custom alignment sizes
    // less than 4KB will be treated as 4KB.
    // </param>
    // <param name="cMinimumSamples">
    // Minimum number of pre-allocated samples.
    // This method will fail if the allocator cannot pre-allocate the specified
    // minimum number of samples.
    // </param>
    // <param name="pAttributes">
    // Optional parameter. An IMFAttributes store with additional configuration
    // attributes for the sample allocator. The attributes that are accepted
    // are the same as for IMFVideoSampleAllocatorEx::InitializeSampleAllocatorEx.
    // (See online documentation for that API for more details.)
    // </param>
    // <param name="pMediaType">
    // The IMFMediaType for which samples will be allocator. The sample
    // allocator uses this parameter to calculate the minimum required size
    // for the media samples.
    // </param>
    function InitializeCaptureSampleAllocator(cbSampleSize: DWORD;
                                              cbCaptureMetadataSize: DWORD;
                                              cbAlignment: DWORD;
                                              cMinimumSamples: DWORD;
                                              pAttributes: IMFAttributes;
                                              pMediaType: IMFMediaType): HResult; stdcall;
  end;
  IID_IMFVideoCaptureSampleAllocator = IMFVideoCaptureSampleAllocator;
  {$EXTERNALSYM IID_IMFVideoCaptureSampleAllocator}



  // <summary>
  //     The enumeration type defines video capture sample allocator mode
  // </summary>
  PMFSampleAllocatorUsage = ^TMFSampleAllocatorUsage;
  MFSampleAllocatorUsage                         = (
    MFSampleAllocatorUsage_UsesProvidedAllocator = 0,
    MFSampleAllocatorUsage_UsesCustomAllocator   = 1,
    MFSampleAllocatorUsage_DoesNotAllocate       = 2
  );
  {$EXTERNALSYM MFSampleAllocatorUsage}
  TMFSampleAllocatorUsage = MFSampleAllocatorUsage;



  // Interface IMFSampleAllocatorControl
  // An enumeration that specifies how an object that implements
  // IMFSampleAllocatorControl uses a sample allocator.
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSampleAllocatorControl);'}
  {$EXTERNALSYM IMFSampleAllocatorControl}
  IMFSampleAllocatorControl = interface(IUnknown)
  ['{DA62B958-3A38-4A97-BD27-149C640C0771}']
    // <summary>
    // Sets the default sample allocator to use for the specified output stream.
    // When this allocator is in use, the sample allocator usage value is
    // MFSampleAllocatorUsage_UsesProvidedAllocator.
    // </summary>
    // <param name="dwOutputStreamID">
    // The ID of the output stream that the pAllocator parameter applies to.
    // </param>
    // <param name="pAllocator">
    // Provides a sample allocator to use for the specified output stream. The
    // allocator must support one of the MF allocator interfaces, such as,
    // IMFVideoCaptureSampleAllocator or IMFVideoSampleAllocatorEx.
    // </param>
    function SetDefaultAllocator(dwOutputStreamID: DWORD;
                                 pAllocator: IUnknown): HResult; stdcall;

    // <summary>
    // Retrieves the sample allocator usage for the specified output stream.
    // </summary>
    // <param name="dwOutputStreamID">
    // The ID of the output stream whose sample allocator usage is requested.
    // </param>
    // <param name="pdwInputStreamID">
    // If the allocator usage is MFSampleAllocatorUsage_DoesNotAllocate,
    // then this output parameter is set to the ID of the input stream that
    // the output samples are coming from.
    // For all other allocator usage values, this parameter shall be ignored.
    // </param>
    // <param name="peUsage">
    // The sample allocator usage of the specified output stream.
    // </param>
    function GetAllocatorUsage(dwOutputStreamID: DWORD;
                               out pdwInputStreamID: DWORD;
                               out peUsage: MFSampleAllocatorUsage): HResult; stdcall;
  end;
  IID_IMFSampleAllocatorControl = IMFSampleAllocatorControl;
  {$EXTERNALSYM IID_IMFSampleAllocatorControl}


const

  MF_CONTENT_DECRYPTOR_SERVICE          :	TGUID = '{68a72927-fc7b-44ee-85f4-7c51bd55a659}';
  {$EXTERNALSYM MF_CONTENT_DECRYPTOR_SERVICE}
  MF_CONTENT_PROTECTION_DEVICE_SERVICE  :	TGUID = '{ff58436f-76a0-41fe-b566-10cc53962edd}';
  {$EXTERNALSYM MF_CONTENT_PROTECTION_DEVICE_SERVICE}


//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)  // Win 8

  MF_SD_AUDIO_ENCODER_DELAY        :	TGUID = '{8e85422c-73de-403f-9a35-550ad6e8b951}';
  {$EXTERNALSYM MF_SD_AUDIO_ENCODER_DELAY}
  MF_SD_AUDIO_ENCODER_PADDING      :	TGUID = '{529c7f2c-ac4b-4e3f-bfc3-0902194982cb}';
  {$EXTERNALSYM MF_SD_AUDIO_ENCODER_PADDING}
  CLSID_MSH264DecoderMFT           :	TGUID = '{62CE7E72-4C71-4d20-B15D-452831A87D9D}';
  {$EXTERNALSYM CLSID_MSH264DecoderMFT}
  CLSID_MSH264EncoderMFT           :	TGUID = '{6ca50344-051a-4ded-9779-a43305165e35}';
  {$EXTERNALSYM CLSID_MSH264EncoderMFT}
  CLSID_MSDDPlusDecMFT             :	TGUID = '{177C0AFE-900B-48d4-9E4C-57ADD250B3D4}';
  {$EXTERNALSYM CLSID_MSDDPlusDecMFT}
  CLSID_MP3DecMediaObject          :	TGUID = '{bbeea841-0a63-4f52-a7ab-a9b3a84ed38a}';
  {$EXTERNALSYM CLSID_MP3DecMediaObject}
  CLSID_MSAACDecMFT                :	TGUID = '{32d186a7-218f-4c75-8876-dd77273a8999}';
  {$EXTERNALSYM CLSID_MSAACDecMFT}
  CLSID_MSH265DecoderMFT           :	TGUID = '{420A51A3-D605-430C-B4FC-45274FA6C562}';
  {$EXTERNALSYM CLSID_MSH265DecoderMFT}
  CLSID_WMVDecoderMFT              :	TGUID = '{82d353df-90bd-4382-8bc2-3f6192b76e34}';
  {$EXTERNALSYM CLSID_WMVDecoderMFT}
  CLSID_WMADecMediaObject          :	TGUID = '{2eeb4adf-4578-4d10-bca7-bb955f56320a}';
  {$EXTERNALSYM CLSID_WMADecMediaObject}
  CLSID_MSMPEGAudDecMFT            :	TGUID = '{70707B39-B2CA-4015-ABEA-F8447D22D88B}';
  {$EXTERNALSYM CLSID_MSMPEGAudDecMFT}
  CLSID_MSMPEGDecoderMFT           :	TGUID = '{2D709E52-123F-49b5-9CBC-9AF5CDE28FB9}';
  {$EXTERNALSYM CLSID_MSMPEGDecoderMFT}
  CLSID_AudioResamplerMediaObject  :	TGUID = '{f447b69e-1884-4a7e-8055-346f74d6edb3}';
  {$EXTERNALSYM CLSID_AudioResamplerMediaObject}
  CLSID_MSVPxDecoder               :	TGUID = '{E3AAF548-C9A4-4C6E-234D-5ADA374B0000}';
  {$EXTERNALSYM CLSID_MSVPxDecoder}
  // CLSID_MSOpusDecoder
  // Opus decoder MFT
  // {63e17c10-2d43-4c42-8fe3-8d8b63e46a6a}
  CLSID_MSOpusDecoder              :	TGUID = '{63e17c10-2d43-4c42-8fe3-8d8b63e46a6a}';
  {$EXTERNALSYM CLSID_MSOpusDecoder}

  // CLSID_VideoProcessorMFT
  // Standard MF video processor component (a.k.a: XVP)
  // {88753B26-5B24-49bd-B2E7-0C445C78C982}
//#if (WINVER < _WIN32_WINNT_WINTHRESHOLD)
  CLSID_VideoProcessorMFT          : TGUID = '{88753b26-5b24-49bd-b2e7-0c445c78c982}';
  {$EXTERNALSYM CLSID_VideoProcessorMFT}
//#endif // (WINVER < _WIN32_WINNT_WINTHRESHOLD)


  // The property ID for the IMFNetCrossOriginSupport callback object in a source resolver's IPropertyStore.
  // Can be a member of the MF_MEDIA_ENGINE_SOURCE_RESOLVER_CONFIG_STORE's IPropertyStore.
  // Can also belong to the IPropertyStore passed to IMFSchemeHandler.BeginCreateObject().
  MFNETSOURCE_CROSS_ORIGIN_SUPPORT :	TGUID = '{9842207c-b02c-4271-a2fc-72e49308e5c2}';
  {$EXTERNALSYM MFNETSOURCE_CROSS_ORIGIN_SUPPORT}

//endif // (WINVER >= _WIN32_WINNT_WINTHRESHOLD)

//  TODO_WIN32_WINNT_REDSTONE

//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)

const
  // MF_SD_MEDIASOURCE_STATUS
  // Type: MF_MEDIASOURCE_STATUS_INFO
  // {1913678B-FC0F-44DA-8F43-1BA3B526F4AE}
  MF_SD_MEDIASOURCE_STATUS          :	TGUID = '{1913678b-fc0f-44da-8f43-1ba3b526f4ae}';
  {$EXTERNALSYM MF_SD_MEDIASOURCE_STATUS}

  // MF_MEDIASOURCE_EXPOSE_ALL_STREAMS
  // Type: boolean
  // {E7F250B8-8FD9-4A09-B6C1-6A315C7C720E}
  MF_MEDIASOURCE_EXPOSE_ALL_STREAMS :	TGUID = '{e7f250b8-8fd9-4a09-b6c1-6a315c7c720e}';
  {$EXTERNALSYM MF_MEDIASOURCE_EXPOSE_ALL_STREAMS}


  // Additional Prototypes for ALL interfaces

type

  // function MFCreatePresentationDescriptor
  {$NODEFINE PIMFStreamDescriptorArray}
  PIMFStreamDescriptorArray = ^TIMFStreamDescriptorArray;
  {$NODEFINE TIMFStreamDescriptorArray}
  TIMFStreamDescriptorArray = array [0..65535] of IMFStreamDescriptor;


  // End of Additional Prototypes



//////////////// FUNCTIONS /////////////////////////////////////////////////////

  function MFCreateSensorGroup(SensorGroupSymbolicLink: LPCWSTR;
                               out ppSensorGroup: IMFSensorGroup): HResult; stdcall;
  {$EXTERNALSYM MFCreateSensorGroup}

  function MFCreateSensorStream(StreamId: DWORD;
                                pAttributes: IMFAttributes;
                                pMediaTypeCollection: IMFCollection;
                                out ppStream: IMFSensorStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateSensorStream}

  function MFCreateSensorActivityMonitor(pCallback: IMFSensorActivitiesReportCallback;
                                         out ppActivityMonitor: IMFSensorActivityMonitor): HResult; stdcall;
  {$EXTERNALSYM MFCreateSensorActivityMonitor}

  function MFCreateSensorProfile(const ProfileType: REFGUID;
                                 ProfileIndex: UINT32;
                                 Constraints: PWideChar;
                                 out ppProfile: IMFSensorProfile): HResult; stdcall;
  {$EXTERNALSYM MFCreateSensorProfile}

  function MFCreateSensorProfileCollection(out ppSensorProfile: IMFSensorProfileCollection): HResult; stdcall;
  {$EXTERNALSYM MFCreateSensorProfileCollection}


  // MFCreateMediaSession
  //
  // Creates the Media Session in the application's process.
  // If your application does not play protected content,
  // you can use this function to create the Media Session in the application's process.
  // To use the Media Session for protected content, you must call MFCreatePMPMediaSession.
  // pConfiguration > Pointer to the IMFAttributes interface.
  // This parameter can be Nil.
  // You can use the pConfiguration parameter to specify any of the following attributes:
  //  MF_SESSION_GLOBAL_TIME
  //  MF_SESSION_QUALITY_MANAGER
  //  MF_SESSION_TOPOLOADER
  //  MF_LOW_LATENCY
  //
  // ppMS > Receives a pointer (C++) to the Media Session's IMFMediaSession interface.
  // The caller must release the interface.
  // Before releasing the last reference to the IMFMediaSession pointer,
  // the application must call the IMFMediaSession.Shutdown method.
  function MFCreateMediaSession(pConfiguration: IMFAttributes;
                                out ppMediaSession: IMFMediaSession): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaSession}

  // MFCreatePMPMediaSession
  //
  // Creates an instance of the Media Session inside a Protected Media Path (PMP) process.
  // Parameters
  //  dwCreationFlags
  //    A member of the MFPMPSESSION_CREATION_FLAGS enumeration that specifies how to create the session object.
  //  pConfiguration
  //    A pointer to the IMFAttributes interface. This parameter can be NULL. See Remarks.
  //  ppMediaSession
  //    Receives a pointer to the PMP Media Session's IMFMediaSession interface.
  //    The caller must release the interface. Before releasing the last reference to the IMFMediaSession pointer,
  //    the application must call the IMFMediaSession.Shutdown method.
  //  ppEnablerActivate
  //    Receives a pointer to the IMFActivate interface or the value NULL.
  //    If non-NIL, the caller must release the interface. See Remarks.

  //  Remarks
  //    You can use the pConfiguration parameter to set any of the following attributes:

  //    MF_SESSION_CONTENT_PROTECTION_MANAGER
  //    MF_SESSION_GLOBAL_TIME
  //    MF_SESSION_QUALITY_MANAGER
  //    MF_SESSION_REMOTE_SOURCE_MODE
  //    MF_SESSION_SERVER_CONTEXT
  //    MF_SESSION_TOPOLOADER
  //  If this function cannot create the PMP Media Session because a trusted binary was revoked,
  //  the ppEnablerActivate parameter receives an IMFActivate interface pointer.
  //  The application can use this pointer to create a content enabler object,
  //  which can then be used to download an updated binary:
  //
  //  Call IMFActivate.ActivateObject with the interface identifier IID_IMFContentEnabler to get
  //  an IMFContentEnabler interface pointer.
  //  Use that interface to download the updated binary.
  //  Call MFCreatePMPMediaSession again.
  //  If the function successfully creates the PMP Media Session, the ppEnablerActivate parameter receives the value NULL.

  //  Do not make calls to the PMP Media Session from a thread that is processing a window message sent from another thread.
  //  To test whether the current thread falls into this category, call InSendMessage.
  function MFCreatePMPMediaSession(dwCreationFlags: DWORD;
                                   pConfiguration: IMFAttributes;
                                   out ppMediaSession: IMFMediaSession;
                                   out ppEnablerActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreatePMPMediaSession}

  function MFCreateSourceResolver(out ppISourceResolver: IMFSourceResolver): HResult; stdcall;
  {$EXTERNALSYM MFCreateSourceResolver}

  function CreatePropertyStore(out ppStore: IPropertyStore): HResult; stdcall;
  {$EXTERNALSYM CreatePropertyStore}

  function MFGetSupportedSchemes(out pPropVarSchemeArray: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM MFGetSupportedSchemes}

  function MFGetSupportedMimeTypes(out pPropVarMimeTypeArray: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM MFGetSupportedMimeTypes}

  function MFCreateTopology(out ppTopo: IMFTopology): HResult; stdcall;
  {$EXTERNALSYM MFCreateTopology}

  // MFCreateTopologyNode
  //
  // Creates a topology node.
  // Parameters
  //  NodeType [in]
  //    The type of node to create, specified as a member of the MF_TOPOLOGY_TYPE enumeration.
  //  ppNode [out]
  //    Receives a pointer to the node's IMFTopologyNode interface.
  //    The caller must release the interface.
  function MFCreateTopologyNode(NodeType: MF_TOPOLOGY_TYPE;
                                out ppNode: IMFTopologyNode): HResult; stdcall;
  {$EXTERNALSYM MFCreateTopologyNode}



//#if (WINVER >= _WIN32_WINNT_WIN7)

  function MFGetTopoNodeCurrentType(pNode: IMFTopologyNode;
                                    dwStreamIndex: DWORD;
                                    fOutput: BOOL;
                                    out ppType: IMFMediaType): HResult; stdcall;
  {$EXTERNALSYM MFGetTopoNodeCurrentType}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  function MFGetService(punkObject: IUnknown;
                        const guidService: REFGUID;
                        const riid: REFIID;
                        out ppvObject): HResult; stdcall;
  {$EXTERNALSYM MFGetService}

  function MFGetSystemTime(): MFTIME; stdcall;
  {$EXTERNALSYM MFGetSystemTime}

  function MFCreatePresentationClock(out ppPresentationClock: IMFPresentationClock): HResult; stdcall;
  {$EXTERNALSYM MFCreatePresentationClock}

  function MFCreateSystemTimeSource(out ppSystemTimeSource: IMFPresentationTimeSource): HResult; stdcall;
  {$EXTERNALSYM MFCreateSystemTimeSource}

  function MFCreatePresentationDescriptor(cStreamDescriptors: DWORD;
                                          apStreamDescriptors: PIMFStreamDescriptor;  // pointer to array of IMFStreamDescriptor
                                          out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;
  {$EXTERNALSYM MFCreatePresentationDescriptor}

  function MFRequireProtectedEnvironment(pPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;
  {$EXTERNALSYM MFRequireProtectedEnvironment}

  function MFSerializePresentationDescriptor(pPD: IMFPresentationDescriptor;
                                             out pcbData: DWORD;
                                             out ppbData: PByte): HResult; stdcall;
  {$EXTERNALSYM MFSerializePresentationDescriptor}

  function MFDeserializePresentationDescriptor(cbData: DWORD;
                                               pbData: PByte;
                                               out ppPD: IMFPresentationDescriptor): HResult; stdcall;
  {$EXTERNALSYM MFDeserializePresentationDescriptor}

  function MFCreateStreamDescriptor(dwStreamIdentifier: DWORD;  // Stream identifier.
                                    cMediaTypes: DWORD;         // Number of elements in the apMediaTypes array.
                                    apMediaTypes: PIMFMediaType; // Pointer to an array of IMFMediaType interface pointers. These pointers are used to initialize the media type handler for the stream descriptor.
                                    out ppDescriptor: IMFStreamDescriptor): HResult; stdcall;
  {$EXTERNALSYM MFCreateStreamDescriptor}

  function MFCreateStreamDescriptor2(dwStreamIdentifier: DWORD;  // Stream identifier.
                                     cMediaTypes: DWORD;         // Number of elements in the apMediaTypes array.
                                     apMediaTypes: PIMFMediaType; // Array of IMFMediaType interfaces. These are used to initialize the media type handler for the stream descriptor.
                                     out ppDescriptor: IMFStreamDescriptor): HResult; stdcall;  // Receives a pointer to the IMFStreamDescriptor interface of the new stream descriptor. The caller must release the interface.
  {$EXTERNALSYM MFCreateStreamDescriptor2}


  function MFCreateSimpleTypeHandler(out ppHandler: IMFMediaTypeHandler): HResult; stdcall;
  {$EXTERNALSYM MFCreateSimpleTypeHandler}

  function MFShutdownObject(pUnk: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFShutdownObject}

  function MFCreateAudioRenderer(pAudioAttributes: IMFAttributes;
                                 out ppSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateAudioRenderer}

  function MFCreateAudioRendererActivate(out ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateAudioRendererActivate}

  function MFCreateVideoRendererActivate(const hwndVideo: HWND;
                                         out ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateVideoRendererActivate}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  function MFCreateMPEG4MediaSink(pIByteStream: IMFByteStream;
                                  pVideoMediaType: IMFMediaType;
                                  pAudioMediaType: IMFMediaType;
                                  out ppIMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateMPEG4MediaSink}

  function MFCreate3GPMediaSink(pIByteStream: IMFByteStream;
                                pVideoMediaType: IMFMediaType;
                                pAudioMediaType: IMFMediaType;
                                out ppIMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreate3GPMediaSink}

  function MFCreateMP3MediaSink(pTargetByteStream: IMFByteStream;
                                out ppMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateMP3MediaSink}

//#endif // (WINVER >= _WIN32_WINNT_WIN7

//#if (WINVER >= _WIN32_WINNT_WIN8)

  function MFCreateAC3MediaSink(pTargetByteStream: IMFByteStream;
                                pAudioMediaType: IMFMediaType;
                                out ppMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateAC3MediaSink}

  function MFCreateADTSMediaSink(pTargetByteStream: IMFByteStream;
                                 pAudioMediaType: IMFMediaType;
                                 out ppMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateADTSMediaSink}

  function MFCreateMuxSink(const guidOutputSubType: TGUID;
                           pOutputAttributes: IMFAttributes;
                           pOutputByteStream: IMFByteStream;
                           out ppMuxSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateMuxSink}

  function MFCreateFMPEG4MediaSink(pIByteStream: IMFByteStream;
                                   pVideoMediaType: IMFMediaType;
                                   pAudioMediaType: IMFMediaType;
                                   out ppIMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateFMPEG4MediaSink}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)

//#if (WINVER >= _WIN32_WINNT_WINBLUE) //win 10

  function MFCreateAVIMediaSink(pIByteStream: IMFByteStream;
                                pVideoMediaType: IMFMediaType;
                                pAudioMediaType: IMFMediaType;
                                out ppIMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateAVIMediaSink}

  function MFCreateWAVEMediaSink(pTargetByteStream: IMFByteStream;
                                 pAudioMediaType: IMFMediaType;
                                 out ppMediaSink: IMFMediaSink): HResult; stdcall;
  {$EXTERNALSYM MFCreateWAVEMediaSink}

//#endif // (WINVER >= _WIN32_WINNT_WINBLUE)


  function MFCreateTopoLoader(out ppObj: IMFTopoLoader): HResult; stdcall;
  {$EXTERNALSYM MFCreateTopoLoader}

  function MFCreateSampleGrabberSinkActivate(pIMFMediaType: IMFMediaType;
                                             pIMFSampleGrabberSinkCallback: IMFSampleGrabberSinkCallback;
                                             out ppIActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateSampleGrabberSinkActivate}

  function MFCreateStandardQualityManager(out ppQualityManager: IMFQualityManager): HResult; stdcall;
  {$EXTERNALSYM MFCreateStandardQualityManager}

  function MFCreateSequencerSource(pReserved: IUnknown;
                                   out ppSequencerSource: IMFSequencerSource): HResult; stdcall;
  {$EXTERNALSYM MFCreateSequencerSource}


  function MFCreateSequencerSegmentOffset(dwId: MFSequencerElementId;
                                          hnsOffset: MFTIME;
                                          out pvarSegmentOffset: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM MFCreateSequencerSegmentOffset}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  function MFCreateAggregateSource(pSourceCollection: IMFCollection;
                                   out ppAggSource: IMFMediaSource): HResult; stdcall;
  {$EXTERNALSYM MFCreateAggregateSource}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)

  function MFCreateCredentialCache(out ppCache: IMFNetCredentialCache): HResult; stdcall;
  {$EXTERNALSYM MFCreateCredentialCache}

  function MFCreateProxyLocator(pszProtocol: PWideChar;
                                pProxyConfig: IPropertyStore;
                                out ppProxyLocator: IMFNetProxyLocator): HResult; stdcall;
  {$EXTERNALSYM MFCreateProxyLocator}

  function MFCreateNetSchemePlugin(const riid: REFIID;
                                   out ppvHandler): HResult; stdcall;
  {$EXTERNALSYM MFCreateNetSchemePlugin}


  // Converted macro's

  function MAKE_MFPROTECTIONDATA_DISABLE(const Disable: BOOL): DWORD; inline;

  function EXTRACT_MFPROTECTIONDATA_DISABLE_ON(Data: DWORD): BOOL; inline;

  function EXTRACT_MFPROTECTIONDATA_DISABLE_RESERVED(Data: DWORD): DWORD; inline;

  function MAKE_MFPROTECTIONDATA_CONSTRICTAUDIO(Level: DWORD): DWORD; inline;

  function EXTRACT_MFPROTECTIONDATA_CONSTRICTAUDIO_LEVEL(Data: DWORD): DWORD; inline;

  function EXTRACT_MFPROTECTIONDATA_CONSTRICTAUDIO_RESERVED(Data: DWORD): DWORD; inline;

  function MAKE_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS(TestCertificateEnable: BOOL;
                                                     DigitalOutputDisable: BOOL;
                                                     DrmLevel: DWORD): DWORD; inline;

  function MAKE_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS2(TestCertificateEnable: BOOL;
                                                      DigitalOutputDisable: BOOL;
                                                      CopyOK: BOOL;
                                                      DrmLevel: DWORD): DWORD; inline;

  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_DRMLEVEL(Data: DWORD): DWORD; inline;

  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_DIGITALOUTPUTDISABLE(Data: DWORD): BOOL; inline;

  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_TESTCERTIFICATEENABLE(Data: DWORD): BOOL; inline;

//#if (WINVER >= _WIN32_WINNT_WIN7)

  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_COPYOK(Data: DWORD): BOOL; inline;

  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_RESERVED(Data: DWORD): DWORD; inline;

//Else

  function _EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_RESERVED(Data: DWORD): DWORD; inline;

//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  function MFCreatePMPServer(dwCreationFlags: DWORD;
                             out ppPMPServer: IMFPMPServer): HResult; stdcall;
  {$EXTERNALSYM MFCreatePMPServer}

  function MFCreateRemoteDesktopPlugin(out ppPlugin: IMFRemoteDesktopPlugin): HResult; stdcall;
  {$EXTERNALSYM MFCreateRemoteDesktopPlugin}

  function CreateNamedPropertyStore(out ppStore: INamedPropertyStore): HResult; stdcall;
  {$EXTERNALSYM CreateNamedPropertyStore}


//#if (WINVER >= _WIN32_WINNT_WIN7)

  function MFCreateSampleCopierMFT(out ppCopierMFT: IMFTransform): HResult; stdcall;
  {$EXTERNALSYM MFCreateSampleCopierMFT}

//#endif (WINVER >= _WIN32_WINNT_WIN7)

  function MFCreateTranscodeTopology(pSrc: IMFMediaSource;
                                     pwszOutputFilePath: LPCWSTR;
                                     pProfile: IMFTranscodeProfile;
                                     out ppTranscodeTopo: IMFTopology): HResult; stdcall;
  {$EXTERNALSYM MFCreateTranscodeTopology}

  function MFCreateTranscodeProfile(out ppTranscodeProfile: IMFTranscodeProfile): HResult; stdcall;
  {$EXTERNALSYM MFCreateTranscodeProfile}

//#if (WINVER >= _WIN32_WINNT_WIN8)

  function MFCreateTranscodeTopologyFromByteStream(pSrc: IMFMediaSource;
                                                   pOutputStream: IMFByteStream;
                                                   var pProfile: IMFTranscodeProfile;
                                                   var ppTranscodeTopo: IMFTopology): HResult; stdcall;
   {$EXTERNALSYM MFCreateTranscodeTopologyFromByteStream}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)


  function MFTranscodeGetAudioOutputAvailableTypes(const guidSubType: REFGUID;
                                                   dwMFTFlags: DWord;
                                                   pCodecConfig: IMFAttributes;
                                                   out ppAvailableTypes: IMFCollection): HResult; stdcall;
  {$EXTERNALSYM MFTranscodeGetAudioOutputAvailableTypes}

  function MFCreateTranscodeSinkActivate(out ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateTranscodeSinkActivate}



//#if (WINVER >= _WIN32_WINNT_WINBLUE)

  function MFCreateTrackedSample(out ppMFSample: IMFTrackedSample): HResult; stdcall;
  {$EXTERNALSYM MFCreateTrackedSample}

  function MFCreateMFByteStreamOnStream(pStream: IStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateMFByteStreamOnStream}

  function MFCreateStreamOnMFByteStream(var pByteStream: IMFByteStream;
                                        var ppStream: IStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateStreamOnMFByteStream}


  function MFCreateMFByteStreamOnStreamEx(var punkStream: IUnknown;
                                          var ppByteStream: IMFByteStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateMFByteStreamOnStreamEx}

  function MFCreateStreamOnMFByteStreamEx(var pByteStream: IMFByteStream;
                                          riid: REFIID;
                                          ppv: Pointer): HResult; stdcall;
  {$EXTERNALSYM MFCreateStreamOnMFByteStreamEx}

  function MFCreateMediaTypeFromProperties(var punkStream: IUnknown;
                                           var ppMediaType: IMFMediaType): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaTypeFromProperties}

  function MFCreatePropertiesFromMediaType(var pMediaType: IMFMediaType;
                                           const riid: REFIID;
                                           ppv: Pointer): HResult; stdcall;
  {$EXTERNALSYM MFCreatePropertiesFromMediaType}

//#endif // (WINVER >= _WIN32_WINNT_WINBLUE)


  // Enumerates a list of audio or video capture devices.
  function MFEnumDeviceSources(pAttributes: IMFAttributes;
                               out pppSourceActivate: PIMFActivate; // Pointer to array of IMFActivate
                               out pcSourceActivate: UINT32): HResult; stdcall;
  {$EXTERNALSYM MFEnumDeviceSources}

  // pAttributes [in]
  //   Pointer to an attribute store that contains search criteria.
  //   To create the attribute store, call MFCreateAttributes.
  //   Set one or more of the following attributes on the attribute store:
  //   - MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE - Specifies whether to enumerate audio or video devices. (Required.)
  //   - MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ROLE - For audio capture devices, specifies the device role. (Optional.)
  //   - MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_CATEGORY - For video capture devices, specifies the device category. (Optional.)

  // pppSourceActivate [out]
  //   Receives a pointer to array of IMFActivate interfaces.
  //   Each pointer represents an activation object for a media source.
  //   The function allocates the memory for the array.
  //   The caller must release the pointers in the array and call CoTaskMemFree to free the memory for the array.

  // pcSourceActivate [out]
  //   Receives the number of elements in the pppSourceActivate array.
  //   If no capture devices match the search criteria, this parameter receives the value 0.

  function MFCreateDeviceSource(pAttributes: IMFAttributes;
                                out ppSource: IMFMediaSource): HResult; stdcall;
  {$EXTERNALSYM MFCreateDeviceSource}

  function MFCreateDeviceSourceActivate(pAttributes: IMFAttributes;
                                        out ppActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFCreateDeviceSourceActivate}

  function MFCreateProtectedEnvironmentAccess(out ppAccess: IMFProtectedEnvironmentAccess): HResult; stdcall;
  {$EXTERNALSYM MFCreateProtectedEnvironmentAccess}

  function MFLoadSignedLibrary(pszName: LPWSTR;
                               out ppLib: IMFSignedLibrary): HResult; stdcall;
  {$EXTERNALSYM MFLoadSignedLibrary}

  function MFGetSystemId(out ppId: IMFSystemId): HResult; stdcall;
  {$EXTERNALSYM MFGetSystemId}

  function MFGetLocalId(verifier: PByte;
                        size: UINT32;
                        out id: PWideChar): HResult; stdcall;
   {$EXTERNALSYM MFGetLocalId}

  function MFCreateContentProtectionDevice(const ProtectionSystemId: REFGUID;
                                           out ContentProtectionDevice: IMFContentProtectionDevice): HResult; stdcall;
  {$EXTERNALSYM MFCreateContentProtectionDevice}

  function MFIsContentProtectionDeviceSupported(const ProtectionSystemId: REFGUID;
                                                out isSupported: BOOL): HResult; stdcall;
  {$EXTERNALSYM MFIsContentProtectionDeviceSupported}

  function MFCreateContentDecryptorContext(const guidMediaProtectionSystemId: REFGUID;
                                           pD3DManager: IMFDXGIDeviceManager;
                                           pContentProtectionDevice: IMFContentProtectionDevice;
                                           out ppContentDecryptorContext: IMFContentDecryptorContext): HResult; stdcall;
  {$EXTERNALSYM MFCreateContentDecryptorContext}

const
   MFStreamExtension_ExtendedCameraIntrinsics : TGUID = '{aa74b3df-9a2c-48d6-8393-5bd1c1a81e6e}';
   {$EXTERNALSYM MFStreamExtension_ExtendedCameraIntrinsics}
   MFSampleExtension_ExtendedCameraIntrinsics : TGUID = '{560bc4a5-4de0-4113-9cdc-832db974f03d}';
   {$EXTERNALSYM MFSampleExtension_ExtendedCameraIntrinsics}

//#if (NTDDI_VERSION >= NTDDI_WIN10_RS5)

  function MFCreateExtendedCameraIntrinsics(out ppExtendedCameraIntrinsics: IMFExtendedCameraIntrinsics): HResult; stdcall;
  {$EXTERNALSYM MFCreateExtendedCameraIntrinsics}

  function MFCreateExtendedCameraIntrinsicModel(distortionModelType: MFCameraIntrinsic_DistortionModelType;
                                                out ppExtendedCameraIntrinsicModel: IMFExtendedCameraIntrinsicModel): HResult; stdcall;
  {$EXTERNALSYM MFCreateExtendedCameraIntrinsicModel}

// end NTDDI_VERSION >= NTDDI_WIN10_RS5

// NTDDI_VERSION >= NTDDI_WIN10_VB
  // <summary>
  // Allows an app to create a Relative Panel Watcher
  // </summary>
  // <param name = "videoDeviceId">
  // string symbolic link name of the video capture device
  // </param>
  // <param name = "displayMonitorDeviceId">
  // string symbolic link name of the display monitor device
  // </param>
  // <param name = "ppRelativePanelWatcher"
  // IMFRelativePanelWatcher pointer, representing the watcher
  // </param>
  function MFCreateRelativePanelWatcher(videoDeviceId: PCWSTR;
                                        displayMonitorDeviceId: PCWSTR;
                                        out ppRelativePanelWatcher: IMFRelativePanelWatcher): HResult; stdcall
  {$EXTERNALSYM MFCreateRelativePanelWatcher}

// end NTDDI_VERSION >= NTDDI_WIN10_VB


  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

const
  MfIdlLib1 = 'Mf.dll';
  MfIdlLib2 = 'MFPlat.dll';
  MfIdlLib3 = 'mfsensorgroup.dll';
  MfIdlLib4 = 'mfcore.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateSensorGroup;            external MfIdlLib3 name 'MFCreateSensorGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSensorStream;           external MfIdlLib3 name 'MFCreateSensorStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSensorActivityMonitor;  external MfIdlLib3 name 'MFCreateSensorActivityMonitor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSensorProfile;          external MfIdlLib3 name 'MFCreateSensorProfile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSensorProfileCollection; external MfIdlLib3 name 'MFCreateSensorProfileCollection' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateMediaSession;           external MfIdlLib1 name 'MFCreateMediaSession' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreatePMPMediaSession;        external MfIdlLib1 name 'MFCreatePMPMediaSession' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSourceResolver;         external MfIdlLib1 name 'MFCreateSourceResolver' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateTopologyNode;           external MfIdlLib1 name 'MFCreateTopologyNode' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetTopoNodeCurrentType;       external MfIdlLib1 name 'MFGetTopoNodeCurrentType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFGetSystemTime;                external MfIdlLib2 name 'MFGetSystemTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateTopology;               external MfIdlLib1 name 'MFCreateTopology' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateTopoLoader;             external MfIdlLib1 name 'MFCreateTopoLoader' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFShutdownObject;               external MfIdlLib1 name 'MFShutdownObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAudioRenderer;          external MfIdlLib1 name 'MFCreateAudioRenderer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoRendererActivate;  external MfIdlLib1 name 'MFCreateVideoRendererActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAudioRendererActivate;  external MfIdlLib1 name 'MFCreateAudioRendererActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetService;                   external MfIdlLib1 name 'MFGetService' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreatePresentationClock;      external MfIdlLib1 name 'MFCreatePresentationClock' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateSystemTimeSource;       external MfIdlLib2 name 'MFCreateSystemTimeSource' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreatePropertyStore;            external MfIdlLib1 name 'CreatePropertyStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetSupportedSchemes;          external MfIdlLib1 name 'MFGetSupportedSchemes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetSupportedMimeTypes;        external MfIdlLib1 name 'MFGetSupportedSchemes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSequencerSource;        external MfIdlLib1 name 'MFCreateSequencerSource' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSequencerSegmentOffset; external MfIdlLib1 name 'MFCreateSequencerSegmentOffset' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreatePresentationDescriptor;      external MfIdlLib2 name 'MFCreatePresentationDescriptor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFRequireProtectedEnvironment;       external MfIdlLib1 name 'MFRequireProtectedEnvironment' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFSerializePresentationDescriptor;   external MfIdlLib2 name 'MFSerializePresentationDescriptor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFDeserializePresentationDescriptor; external MfIdlLib2 name 'MFDeserializePresentationDescriptor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateStreamDescriptor;            external MfIdlLib2 name 'MFCreateStreamDescriptor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateStreamDescriptor2;           external MfIdlLib2 name 'MFCreateStreamDescriptor' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateSimpleTypeHandler;           external MfIdlLib1 name 'MFCreateSimpleTypeHandler' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateMPEG4MediaSink;        external MfIdlLib1 name 'MFCreateMPEG4MediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreate3GPMediaSink;          external MfIdlLib1 name 'MFCreate3GPMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMP3MediaSink;          external MfIdlLib1 name 'MFCreateMP3MediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAC3MediaSink;          external MfIdlLib1 name 'MFCreateAC3MediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateADTSMediaSink;         external MfIdlLib1 name 'MFCreateADTSMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMuxSink;               external MfIdlLib1 name 'MFCreateMuxSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateFMPEG4MediaSink;       external MfIdlLib1 name 'MFCreateFMPEG4MediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAVIMediaSink;          external MfIdlLib1 name 'MFCreateAVIMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateWAVEMediaSink;         external MfIdlLib1 name 'MFCreateWAVEMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateSampleGrabberSinkActivate; external MfIdlLib1 name 'MFCreateSampleGrabberSinkActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateStandardQualityManager;    external MfIdlLib1 name 'MFCreateSampleGrabberSinkActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAggregateSource;           external MfIdlLib1 name 'MFCreateAggregateSource' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateCredentialCache;           external MfIdlLib1 name 'MFCreateCredentialCache' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateProxyLocator;              external MfIdlLib1 name 'MFCreateProxyLocator' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateNetSchemePlugin;           external MfIdlLib1 name 'MFCreateNetSchemePlugin' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreatePMPServer;                 external MfIdlLib1 name 'MFCreatePMPServer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateRemoteDesktopPlugin;       external MfIdlLib1 name 'MFCreateRemoteDesktopPlugin' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CreateNamedPropertyStore;          external MfIdlLib1 name 'CreateNamedPropertyStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSampleCopierMFT;           external MfIdlLib1 name 'MFCreateSampleCopierMFT' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateTranscodeProfile;          external MfIdlLib1 name 'MFCreateTranscodeProfile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateTranscodeTopology;         external MfIdlLib1 name 'MFCreateTranscodeTopology' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateTranscodeTopologyFromByteStream; external MfIdlLib1 name 'MFCreateTranscodeTopologyFromByteStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTranscodeGetAudioOutputAvailableTypes; external MfIdlLib1 name 'MFTranscodeGetAudioOutputAvailableTypes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateTranscodeSinkActivate;           external MfIdlLib1 name 'MFCreateTranscodeSinkActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateTrackedSample;           external MfIdlLib2 name 'MFCreateTrackedSample' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMFByteStreamOnStream;    external MfIdlLib2 name 'MFCreateMFByteStreamOnStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateStreamOnMFByteStream;    external MfIdlLib2 name 'MFCreateStreamOnMFByteStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMFByteStreamOnStreamEx;  external MfIdlLib2 name 'MFCreateMFByteStreamOnStreamEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateStreamOnMFByteStreamEx;  external MfIdlLib2 name 'MFCreateStreamOnMFByteStreamEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaTypeFromProperties; external MfIdlLib2 name 'MFCreateMediaTypeFromProperties' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreatePropertiesFromMediaType; external MfIdlLib2 name 'MFCreatePropertiesFromMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFEnumDeviceSources;             external MfIdlLib1 name 'MFEnumDeviceSources' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateDeviceSource;            external MfIdlLib1 name 'MFCreateDeviceSource' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateDeviceSourceActivate;    external MfIdlLib1 name 'MFCreateDeviceSourceActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateProtectedEnvironmentAccess; external MfIdlLib1 name 'MFCreateProtectedEnvironmentAccess' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFLoadSignedLibrary;                external MfIdlLib1 name 'MFLoadSignedLibrary' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetSystemId;                      external MfIdlLib1 name 'MFGetSystemId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetLocalId;                       external MfIdlLib1 name 'MFGetLocalId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateContentProtectionDevice;      external MfIdlLib2 name 'MFCreateContentProtectionDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFIsContentProtectionDeviceSupported; external MfIdlLib2 name 'MFIsContentProtectionDeviceSupported' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateContentDecryptorContext;      external MfIdlLib2 name 'MFCreateContentDecryptorContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  // NTDDI_VERSION >= NTDDI_WIN10_RS5
  function MFCreateExtendedCameraIntrinsics;     external MfIdlLib4 name 'MFCreateExtendedCameraIntrinsics' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateExtendedCameraIntrinsicModel; external MfIdlLib4 name 'MFCreateExtendedCameraIntrinsicModel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  // end NTDDI_VERSION >= NTDDI_WIN10_RS5

  // NTDDI_VERSION >= NTDDI_WIN10_VB  TODO: which lib?
  function MFCreateRelativePanelWatcher;    external MfIdlLib4 {?} name 'MFCreateRelativePanelWatcher' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  // end NTDDI_VERSION >= NTDDI_WIN10_VB

{$WARN SYMBOL_PLATFORM ON}



///// INTERNAL FUNCTIONS ///////////////////////////////////////////////////////

  function MAKE_MFPROTECTIONDATA_DISABLE(const Disable: BOOL): DWORD; inline;
    begin
      if (Disable = True) then
        Result:= $00000001
      else
        Result:= 0;
    end;


  function EXTRACT_MFPROTECTIONDATA_DISABLE_ON(Data: DWORD): BOOL; inline;
    begin
      Result:= BOOL((Data and $00000001) <> 0);
    end;


  function EXTRACT_MFPROTECTIONDATA_DISABLE_RESERVED(Data: DWORD): DWORD; inline;
    begin
      Result:= ((Data and $FFFFFFFE) shr 1);
    end;


  function MAKE_MFPROTECTIONDATA_CONSTRICTAUDIO(Level: DWORD): DWORD; inline;
    begin
      Result:= Level;
    end;


  function EXTRACT_MFPROTECTIONDATA_CONSTRICTAUDIO_LEVEL(Data: DWORD): DWORD; inline;
    begin
      Result:= Data and $000000FF;
    end;


  function EXTRACT_MFPROTECTIONDATA_CONSTRICTAUDIO_RESERVED(Data: DWORD): DWORD; inline;
    begin
      Result:= (Data and $FFFFFF00) shr 8;
    end;


  function MAKE_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS(TestCertificateEnable: BOOL;
                                                     DigitalOutputDisable: BOOL;
                                                     DrmLevel: DWORD): DWORD; inline;
    begin
      Result:= 0;   //init

      if (TestCertificateEnable = True) then
        Result:= $20000;

      if (DigitalOutputDisable = True) then
        Result:= Result or $10000;
      //final
      Result:= Result or DrmLevel;

    end;


  function MAKE_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS2(TestCertificateEnable: BOOL;
                                                      DigitalOutputDisable: BOOL;
                                                      CopyOK: BOOL;
                                                      DrmLevel: DWORD): DWORD; inline;
    begin
      Result:= 0; //init

      if (TestCertificateEnable = True) then
        Result:= $20000;

      if (DigitalOutputDisable = True) then
        Result:= Result or $10000;

      if (CopyOK = True) then
        Result:= Result or $40000;

        //final
      Result:= Result or DrmLevel;
    end;


  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_DRMLEVEL(Data: DWORD): DWORD; inline;
    begin
      Result:= (Data and $0000FFFF);
    end;


  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_DIGITALOUTPUTDISABLE(Data: DWORD): BOOL; inline;
    begin
      Result:= BOOL(0 <> (Data and $10000));
    end;


  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_TESTCERTIFICATEENABLE(Data: DWORD): BOOL; inline;
    begin
      Result:= BOOL(0 <> (Data and $20000));
    end;


  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_COPYOK(Data: DWORD): BOOL; inline;
    begin
      Result:= BOOL(0 <> (Data and $40000));
    end;


  function EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_RESERVED(Data: DWORD): DWORD; inline;
    begin
      Result:= ((Data and $FFF80000) shr 19);
    end;

  // Else

  function _EXTRACT_MFPROTECTIONDATA_TRUSTEDAUDIODRIVERS_RESERVED(Data: DWORD): DWORD; inline;
    begin
      Result:= ((Data and $FFF80000) shr 18);
    end;


procedure MFCLOCK_PROPERTIES.Copy(out destProps: MFCLOCK_PROPERTIES);
  begin
    destProps.qwCorrelationRate := qwCorrelationRate;
    destProps.guidClockId := guidClockId;
    destProps.dwClockFlags := dwClockFlags;
    destProps.qwClockFrequency := qwClockFrequency;
    destProps.dwClockTolerance := dwClockTolerance;
    destProps.dwClockJitter := dwClockJitter;
  end;

end.

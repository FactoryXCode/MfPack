// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMediaEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 09-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
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
// Remarks: Requires Windows 8.1 or later.
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
// Source: mfmediaengine.h
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
unit WinApi.MediaFoundationApi.MfMediaEngine;

  {$HPPEMIT '#include "mfmediaengine.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  {System}
  System.Types,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfTransform;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  MF_INVALID_PRESENTATION_TIME        = $8000000000000000;
  {$EXTERNALSYM MF_INVALID_PRESENTATION_TIME}


  ////////////////////////////////////////////////////////////////////////////////
  // Media Source Extension see:
  //
  // http://dvcs.w3.org/hg/html-media/raw-file/tip/media-source/media-source.html
  ////////////////////////////////////////////////////////////////////////////////

  // Media Source Extension creation attributes
  //===========================================

  // MF_MSE_CALLBACK
  // Data type:  IUnknown (IMFMediaSourceExtensionNotify)
  // guid: 9063a7c0-42c5-4ffd-a8a8-6fcf9ea3d00c
  MF_MSE_CALLBACK                           : TGUID = '{9063a7c0-42c5-4ffd-a8a8-6fcf9ea3d00c}';
  {$EXTERNALSYM MF_MSE_CALLBACK}

  // MF_MSE_ACTIVELIST_CALLBACK
  // Data type:  IUnknown (IMFBufferListNotify)
  // guid: 949bda0f-4549-46d5-ad7f-b846e1xab1652
  MF_MSE_ACTIVELIST_CALLBACK                : TGUID = '{949bda0f-4549-46d5-ad7f-b846e1ab1652}';
  {$EXTERNALSYM MF_MSE_ACTIVELIST_CALLBACK}

  // MF_MSE_BUFFERLIST_CALLBACK
  // Data type:  IUnknown (IMFBufferListNotify)
  // guid: 42e669b0-d60e-4afb-a85b-d8e5xfe6bdab5
  MF_MSE_BUFFERLIST_CALLBACK                : TGUID = '{42e669b0-d60e-4afb-a85b-d8e5fe6bdab5}';
  {$EXTERNALSYM MF_MSE_BUFFERLIST_CALLBACK}

  // MF_MSE_VP9_SUPPORT
  // Data type:  UINT32 (MF_MSE_VP9_SUPPORT)
  // guid:92D78429-D88B-4FF0-8322-803EFA6E9626
  MF_MSE_VP9_SUPPORT                        :  TGUID = '{92D78429-D88B-4FF0-8322-803EFA6E9626}';
  {$EXTERNALSYM MF_MSE_VP9_SUPPORT}

  // MF_MSE_OPUS_SUPPORT
  // Data type:  UINT32 (MF_MSE_OPUS_SUPPORT)
  // guid:4d224cc1-8cc4-48a3-a7a7-e4c16ce6388a
  MF_MSE_OPUS_SUPPORT                       :	 TGUID = '{4d224cc1-8cc4-48a3-a7a7-e4c16ce6388a}';
  {$EXTERNALSYM MF_MSE_OPUS_SUPPORT}

  // needKey event handler
  // MF_MEDIA_ENGINE_NEEDKEY_CALLBACK
  // Data type: IUnknown (IMFMediaEngineNeedKeyNotify)
  // 7ea80843-b6e4-432c-8ea4-7848ffe4220e
  MF_MEDIA_ENGINE_NEEDKEY_CALLBACK : TGUID = '{7ea80843-b6e4-432c-8ea4-7848ffe4220e}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_NEEDKEY_CALLBACK}

  // MFMediaEngine creation attributes
  //==========================================================

  // MF_MEDIA_ENGINE_CALLBACK
  // Data type: IUnknown (IMFMediaEngineNotify)
  MF_MEDIA_ENGINE_CALLBACK        : TGUID = '{c60381b8-83a4-41f8-a3d0-de05076849a9}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_CALLBACK}

  // MF_MEDIA_ENGINE_DXGI_MANAGER
  // Data type: IUnknown
  MF_MEDIA_ENGINE_DXGI_MANAGER    : TGUID = '{065702da-1094-486d-8617-ee7cc4ee4648}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_DXGI_MANAGER}

  // MF_MEDIA_ENGINE_EXTENSION
  // Data type: IUnknown (IMFMediaEngineExtension)
  MF_MEDIA_ENGINE_EXTENSION       : TGUID = '{3109fd46-060d-4b62-8dcf-faff811318d2}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_EXTENSION}


  // MF_MEDIA_ENGINE_PLAYBACK_HWND
  // Data type: UINT64
  MF_MEDIA_ENGINE_PLAYBACK_HWND   : TGUID = '{d988879b-67c9-4d92-baa7-6eadd446039d}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_PLAYBACK_HWND}

  // MF_MEDIA_ENGINE_OPM_HWND - needed for protected content when there is no PLAYBACK_HWND
  // Data type: UINT64
  // a0be8ee7-0572-4f2c-a801-2a151bd3e726
  MF_MEDIA_ENGINE_OPM_HWND        : TGUID = '{a0be8ee7-0572-4f2c-a801-2a151bd3e726}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_OPM_HWND}

  // MF_MEDIA_ENGINE_PLAYBACK_VISUAL
  // Data type: IUnknown
  MF_MEDIA_ENGINE_PLAYBACK_VISUAL : TGUID =  '{6debd26f-6ab9-4d7e-b0ee-c61a73ffad15}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_PLAYBACK_VISUAL}

  // MF_MEDIA_ENGINE_COREWINDOW - needed for app state (and protected content when there is no PLAYBACK_HWND)
  // Data type: IUnknown (ICoreWindow)
  // fccae4dc-0b7f-41c2-9f96-4659948acddc
  MF_MEDIA_ENGINE_COREWINDOW                  : TGUID = '{fccae4dc-0b7f-41c2-9f96-4659948acddc}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_COREWINDOW}

  // MF_MEDIA_ENGINE_VIDEO_OUTPUT_FORMAT
  // Data type: UINT32 (actually a  DXGI_FORMAT type)
  MF_MEDIA_ENGINE_VIDEO_OUTPUT_FORMAT         : TGUID = '{5066893c-8cf9-42bc-8b8a-472212e52726}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_VIDEO_OUTPUT_FORMAT}

  // MF_MEDIA_ENGINE_CONTENT_PROTECTION_FLAGS
  // Data type:  UINT32 - MF_MEDIA_ENGINE_PROTECTION_FLAGS
  // guid:e0350223-5aaf-4d76-a7c3-06de70894db4
  MF_MEDIA_ENGINE_CONTENT_PROTECTION_FLAGS    : TGUID = '{e0350223-5aaf-4d76-a7c3-06de70894db4}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_CONTENT_PROTECTION_FLAGS}

  // MF_MEDIA_ENGINE_CONTENT_PROTECTION_MANAGER
  // Data type:  IUnknown (IMFContentProtectionManager)
  // guid:fdd6dfaa-bd85-4af3-9e0f-a01d539d876a
  MF_MEDIA_ENGINE_CONTENT_PROTECTION_MANAGER  : TGUID = '{fdd6dfaa-bd85-4af3-9e0f-a01d539d876a}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_CONTENT_PROTECTION_MANAGER}

  // MF_MEDIA_ENGINE_AUDIO_ENDPOINT_ROLE
  // Data type:  UINT32
  // Values are from the ERole enumeration
  // guid:d2cb93d1-116a-44f2-9385-f7d0fda2fb46
  // Note: Sets the default audio device endpoint role, can be changed later
  //       by calling IMFMediaEngineEx.SetAudioEndpointRole.
  MF_MEDIA_ENGINE_AUDIO_ENDPOINT_ROLE  : TGUID = '{d2cb93d1-116a-44f2-9385-f7d0fda2fb46}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_AUDIO_ENDPOINT_ROLE}

  // MF_MEDIA_ENGINE_AUDIO_CATEGORY
  // Data type:  UINT32
  // guid:c8d4c51d-350e-41f2-ba46-faebbb0857f6
  // Note: Sets the default audio category, can be changed later
  //       by calling IMFMediaEngineEx.SetAudioStreamCategory.
  MF_MEDIA_ENGINE_AUDIO_CATEGORY  : TGUID = '{c8d4c51d-350e-41f2-ba46-faebbb0857f6}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_AUDIO_CATEGORY}

  // MF_MEDIA_ENGINE_STREAM_CONTAINS_ALPHA_CHANNEL
  // Data type:  VT_BOOL
  // guid:5cbfaf44-d2b2-4cfb-80a7-d429c74c789d
  MF_MEDIA_ENGINE_STREAM_CONTAINS_ALPHA_CHANNEL  : TGUID = '{5cbfaf44-d2b2-4cfb-80a7-d429c74c789d}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_STREAM_CONTAINS_ALPHA_CHANNEL}

  // MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE
  // Data type:  GUID
  // guid:4e0212e2-e18f-41e1-95e5-c0e7e9235bc3
  // Note: set this to one of the browser compatibility modes defined below.
  //       when defining new modes, ensure that Data1 is greater than previous modes,
  //       while leaving space for future modes to be defined
  MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE     : TGUID = '{4e0212e2-e18f-41e1-95e5-c0e7e9235bc3}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE}

  // MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE9
  // guid:052c2d39-40c0-4188-ab86-f828273b7522
  // Note: set as the value for MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE
  MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE9     : TGUID = '{052c2d39-40c0-4188-ab86-f828273b7522}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE9}

  // MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE10
  // guid:11a47afd-6589-4124-b312-6158ec517fc3
  // Note: set as the value for MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE
  MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE10    : TGUID = '{11a47afd-6589-4124-b312-6158ec517fc3}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE10}

  // MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE11
  // guid:1cf1315f-ce3f-4035-9391-16142f775189
  // Note: set as the value for MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE
  MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE11    : TGUID = '{1cf1315f-ce3f-4035-9391-16142f775189}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE11}

// #if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)  (Win 8)

  // MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE_EDGE
  // guid:a6f3e465-3aca-442c-a3f0-ad6ddad839ae
  // Note: set as the value for MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE
  MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE_EDGE :  TGUID = '{a6f3e465-3aca-442c-a3f0-ad6ddad839ae}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_BROWSER_COMPATIBILITY_MODE_IE_EDGE}

  // MF_MEDIA_ENGINE_COMPATIBILITY_MODE
  // Data type:  GUID
  // guid:3ef26ad4-dc54-45de-b9af-76c8-c66bfa8e
  // Note: set this to one of the compatibility modes defined below.
  //       when defining new modes, ensure that Data1 is greater than previous modes,
  //       while leaving space for future modes to be defined
  MF_MEDIA_ENGINE_COMPATIBILITY_MODE                :  TGUID = '{3ef26ad4-dc54-45de-b9af-76c8c66bfa8e}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_COMPATIBILITY_MODE}

  // MF_MEDIA_ENGINE_COMPATIBILITY_MODE_WWA_EDGE
  // guid:15b29098-9f01-4e4d-b65a-c06c-6c89da2a
  // Note: set as the value for MF_MEDIA_ENGINE_COMPATIBILITY_MODE
  MF_MEDIA_ENGINE_COMPATIBILITY_MODE_WWA_EDGE       :  TGUID = '{15b29098-9f01-4e4d-b65a-c06c6c89da2a}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_COMPATIBILITY_MODE_WWA_EDGE}

  // MF_MEDIA_ENGINE_COMPATIBILITY_MODE_WIN10
  // guid:5b25e089-6ca7-4139-a2cb-fcaa-b39552a3
  // Note: set as the value for MF_MEDIA_ENGINE_COMPATIBILITY_MODE
  MF_MEDIA_ENGINE_COMPATIBILITY_MODE_WIN10          :  TGUID = '{5b25e089-6ca7-4139-a2cb-fcaab39552a3}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_COMPATIBILITY_MODE_WIN10}

// #endif // (WINVER >= _WIN32_WINNT_WINTHRESHOLD)

  // MF_MEDIA_ENGINE_SOURCE_RESOLVER_CONFIG_STORE
  // Data type: IUnknown
  MF_MEDIA_ENGINE_SOURCE_RESOLVER_CONFIG_STORE      : TGUID = '{0ac0c497-b3c4-48c9-9cde-bb8ca2442ca3}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_SOURCE_RESOLVER_CONFIG_STORE}

  // MF_MEDIA_ENGINE_TRACK_ID
  // Data type: UINT32
  MF_MEDIA_ENGINE_TRACK_ID                          : TGUID = '{65bea312-4043-4815-8eab-44dce2ef8f2a}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_TRACK_ID}

  // MF_MEDIA_ENGINE_TELEMETRY_APPLICATION_ID
  // Data type: GUID
  MF_MEDIA_ENGINE_TELEMETRY_APPLICATION_ID          :  TGUID = '{1e7b273b-a7e4-402a-8f51-c48e88a2cabc}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_TELEMETRY_APPLICATION_ID}


  /////////////////////////////////////////////////////////////////////////////
  // CLSID_MFMediaEngineClassFactory
  // Data type: GUID
  // CLSID for creating the Media Engine class factory.
  // {B44392DA-499B-446b-A4CB-005FEAD0E6D5}
  CLSID_MFMediaEngineClassFactory                    : TGUID = '{B44392DA-499B-446b-A4CB-005FEAD0E6D5}';
  {$EXTERNALSYM CLSID_MFMediaEngineClassFactory}


  // MF_MEDIA_ENGINE_TIMEDTEXT
  // guid: {805EA411-92E0-4E59-9B6E-5C7D7915E64F}
  // Data type: IMFTimedText
  // Application can interact with Timed Text component after retrieving it from
  // Media Engine using IMFGetService interface and passing MF_MEDIA_ENGINE_TIMEDTEXT
  // as a service id.
  MF_MEDIA_ENGINE_TIMEDTEXT                         : TGUID = '{805EA411-92E0-4E59-9B6E-5C7D7915E64F}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_TIMEDTEXT}

//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD) (Win 8)

  // Encrypted Media Extensions (EME 2) March 31, 2015
  //
  // MF_MEDIA_ENGINE_CONTINUE_ON_CODEC_ERROR
  // Data type: UINT32
  MF_MEDIA_ENGINE_CONTINUE_ON_CODEC_ERROR           :  TGUID = '{dbcdb7f9-48e4-4295-b70d-d518234eeb38}';
  {$EXTERNALSYM MF_MEDIA_ENGINE_CONTINUE_ON_CODEC_ERROR}

//#endif (WINVER >= _WIN32_WINNT_WINTHRESHOLD) (Win 8)



  // Attributes for MediaKeySystemConfigurations
  MF_EME_INITDATATYPES       :  PROPERTYKEY = (fmtid: (D1: $497d231b; D2: $4eb9; D3: $4df0;
                                                       D4: ($b4, $74, $b9, $af, $eb, $0a, $df, $38));
                                                       pid: PID_FIRST_USABLE + 1);
  {$EXTERNALSYM MF_EME_INITDATATYPES}

  MF_EME_DISTINCTIVEID       :  PROPERTYKEY = (fmtid: (D1: $7dc9c4a5; D2: $12be; D3: $497e;
                                                       D4: ($8b, $ff, $9b, $60, $b2, $dc, $58, $45));
                                                       pid: PID_FIRST_USABLE + 2);
  {$EXTERNALSYM MF_EME_DISTINCTIVEID}

  MF_EME_PERSISTEDSTATE      :  PROPERTYKEY = (fmtid: (D1: $5d4df6ae; D2: $9af1; D3: $4e3d;
                                                       D4: ($95, $5b, $0e, $4b, $d2, $2f, $ed, $f0));
                                                       pid: PID_FIRST_USABLE + 3);
  {$EXTERNALSYM MF_EME_PERSISTEDSTATE}

  MF_EME_AUDIOCAPABILITIES   :  PROPERTYKEY = (fmtid: (D1: $980fbb84; D2: $297d; D3: $4ea7;
                                                       D4: ($89, $5f, $bc, $f2, $8a, $46, $28, $81));
                                                       pid: PID_FIRST_USABLE + 4);
  {$EXTERNALSYM MF_EME_AUDIOCAPABILITIES}

  MF_EME_VIDEOCAPABILITIES   :  PROPERTYKEY = (fmtid: (D1: $b172f83d; D2: $30dd; D3: $4c10;
                                                       D4: ($80, $06, $ed, $53, $da, $4d, $3b, $db));
                                                       pid: PID_FIRST_USABLE + 5);
  {$EXTERNALSYM MF_EME_VIDEOCAPABILITIES}

  MF_EME_LABEL               :  PROPERTYKEY = (fmtid: (D1: $9eae270e; D2: $b2d7; D3: $4817;
                                                       D4: ($b8, $8f, $54, $00, $99, $f2, $ef, $4e));
                                                       pid: PID_FIRST_USABLE + 6);
  {$EXTERNALSYM MF_EME_LABEL}

  MF_EME_SESSIONTYPES        :  PROPERTYKEY = (fmtid: (D1: $7623384f; D2: $00f5; D3: $4376;
                                                       D4: ($86, $98, $34, $58, $db, $03, $0e, $d5));
                                                       pid: PID_FIRST_USABLE + 7);
  {$EXTERNALSYM MF_EME_SESSIONTYPES}

  // Attributes for Audio/Video Capabilities
  MF_EME_ROBUSTNESS          :  PROPERTYKEY = (fmtid: (D1: $9d3d2b9e; D2: $7023; D3: $4944;
                                                       D4: ($a8, $f5, $ec, $ca, $52, $a4, $69, $90));
                                                       pid: PID_FIRST_USABLE + 1);
   {$EXTERNALSYM MF_EME_ROBUSTNESS}

  MF_EME_CONTENTTYPE         :  PROPERTYKEY = (fmtid: (D1: $289fb1fc; D2: $d9c4; D3: $4cc7;
                                                       D4: ($b2, $be, $97, $2b, $0e, $9b, $28, $3a));
                                                       pid: PID_FIRST_USABLE + 2);
  {$EXTERNALSYM MF_EME_CONTENTTYPE}

  // Attributes to pass to CreateMediaKeys
  MF_EME_CDM_INPRIVATESTOREPATH  :  PROPERTYKEY = (fmtid: (D1: $ec305fd9; D2: $039f; D3: $4ac8;
                                                           D4: ($98, $da, $e7, $92, $1e, $00, $6a, $90));
                                                           pid: PID_FIRST_USABLE + 1);
   {$EXTERNALSYM MF_EME_CDM_INPRIVATESTOREPATH}

  MF_EME_CDM_STOREPATH           :  PROPERTYKEY = (fmtid: (D1: $f795841e; D2: $99f9; D3: $44d7;
                                                           D4: ($af, $c0, $d3, $09, $c0, $4c, $94, $ab));
                                                           pid: PID_FIRST_USABLE + 2);
  {$EXTERNALSYM MF_EME_CDM_STOREPATH}


type

  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MEDIA_ENGINE_ERROR
  //  Synopsis:   Defines the error status of <video>/<audio> elements
  //------------------------------------------------------------------------------
  PMF_MEDIA_ENGINE_ERR = ^MF_MEDIA_ENGINE_ERR;
  PMfMediaEngineErr = ^MF_MEDIA_ENGINE_ERR;
  MF_MEDIA_ENGINE_ERR                     = (
    MF_MEDIA_ENGINE_ERR_NOERROR           = 0,
    MF_MEDIA_ENGINE_ERR_ABORTED           = 1,
    MF_MEDIA_ENGINE_ERR_NETWORK           = 2,
    MF_MEDIA_ENGINE_ERR_DECODE            = 3,
    MF_MEDIA_ENGINE_ERR_SRC_NOT_SUPPORTED = 4,
    MF_MEDIA_ENGINE_ERR_ENCRYPTED         = 5
   );
  {$EXTERNALSYM MF_MEDIA_ENGINE_ERR}

type
  PMfMediaEngineFrameProtectionFlags = ^MfMediaEngineFrameProtectionFlags;
  MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS = DWord;
  {$EXTERNALSYM MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS}
  MfMediaEngineFrameProtectionFlags = MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS;
  {$EXTERNALSYM MfMediaEngineFrameProtectionFlags}
const
  MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAG_PROTECTED                              = MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS($01);
  {$EXTERNALSYM MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAG_PROTECTED}
  MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAG_REQUIRES_SURFACE_PROTECTION            = MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS($02);
  {$EXTERNALSYM MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAG_REQUIRES_SURFACE_PROTECTION}
  MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAG_REQUIRES_ANTI_SCREEN_SCRAPE_PROTECTION = MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS($04);
  {$EXTERNALSYM MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAG_REQUIRES_ANTI_SCREEN_SCRAPE_PROTECTION}

type
  PMfMediaEngineCreateflags = ^MfMediaEngineCreateflags;
  MF_MEDIA_ENGINE_CREATEFLAGS = DWord;
  {$EXTERNALSYM MF_MEDIA_ENGINE_CREATEFLAGS}
  MfMediaEngineCreateflags = MF_MEDIA_ENGINE_CREATEFLAGS;
  {$EXTERNALSYM MfMediaEngineCreateflags}
const
  MF_MEDIA_ENGINE_AUDIOONLY             = MF_MEDIA_ENGINE_CREATEFLAGS($1);
  {$EXTERNALSYM MF_MEDIA_ENGINE_AUDIOONLY}
  MF_MEDIA_ENGINE_WAITFORSTABLE_STATE   = MF_MEDIA_ENGINE_CREATEFLAGS($2);
  {$EXTERNALSYM MF_MEDIA_ENGINE_WAITFORSTABLE_STATE}
  MF_MEDIA_ENGINE_FORCEMUTE             = MF_MEDIA_ENGINE_CREATEFLAGS($4);
  {$EXTERNALSYM MF_MEDIA_ENGINE_FORCEMUTE}
  MF_MEDIA_ENGINE_REAL_TIME_MODE        = MF_MEDIA_ENGINE_CREATEFLAGS($8);  // sets the default real time mode, can be changed later by calling IMFMediaEngineEx::SetRealTimeMode
  {$EXTERNALSYM MF_MEDIA_ENGINE_REAL_TIME_MODE}
  MF_MEDIA_ENGINE_DISABLE_LOCAL_PLUGINS = MF_MEDIA_ENGINE_CREATEFLAGS($10);
  {$EXTERNALSYM MF_MEDIA_ENGINE_DISABLE_LOCAL_PLUGINS}
  MF_MEDIA_ENGINE_CREATEFLAGS_MASK      = MF_MEDIA_ENGINE_CREATEFLAGS($1F); // Reserved, do not use.
  {$EXTERNALSYM MF_MEDIA_ENGINE_CREATEFLAGS_MASK}

type
  PMfMediaEngineProtectionFlags = ^MfMediaEngineProtectionFlags;
  MF_MEDIA_ENGINE_PROTECTION_FLAGS = UINT32;
  {$EXTERNALSYM MF_MEDIA_ENGINE_PROTECTION_FLAGS}
  MfMediaEngineProtectionFlags = MF_MEDIA_ENGINE_PROTECTION_FLAGS;
  {$EXTERNALSYM MfMediaEngineProtectionFlags}
const
  MF_MEDIA_ENGINE_ENABLE_PROTECTED_CONTENT = MF_MEDIA_ENGINE_PROTECTION_FLAGS(1);
  {$EXTERNALSYM MF_MEDIA_ENGINE_ENABLE_PROTECTED_CONTENT}
  MF_MEDIA_ENGINE_USE_PMP_FOR_ALL_CONTENT  = MF_MEDIA_ENGINE_PROTECTION_FLAGS(2);
  {$EXTERNALSYM MF_MEDIA_ENGINE_USE_PMP_FOR_ALL_CONTENT}
  MF_MEDIA_ENGINE_USE_UNPROTECTED_PMP      = MF_MEDIA_ENGINE_PROTECTION_FLAGS(4);
  {$EXTERNALSYM MF_MEDIA_ENGINE_USE_UNPROTECTED_PMP}


type

  // Forward declarations of interfaces defined in the type library
  IMFMediaKeys = interface;
  IMFMediaKeySessionNotify = interface;
  IMFMediaKeySession = interface;
  IMFCdmSuspendNotify = interface;
  IMFTimedTextNotify = interface;
  IMFTimedTextTrackList = interface;
  IMFTimedTextTrack = interface;
  IMFTimedTextCue = interface;
  IMFTimedTextCueList = interface;
  IMFTimedTextBinary = interface;
  IMFTimedTextRegion = interface;
  IMFTimedTextStyle = interface;
  IMFTimedTextFormattedText = interface;
  IMFMediaKeys2 = interface;
  IMFMediaKeySession2 = interface;


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaError
  //  Synopsis:   Media engine error information
  //  See http://dev.w3.org/html5/spec/video.html#mediaerror for details
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaError);'}
  {$EXTERNALSYM IMFMediaError}
  IMFMediaError = interface(IUnknown)
  ['{fc0e10d2-ab2a-4501-a951-06bb1075184c}']

    function GetErrorCode(): USHORT; stdcall;

    function GetExtendedErrorCode(): HResult; stdcall;

    function SetErrorCode(const error: MF_MEDIA_ENGINE_ERR): HResult; stdcall;

    function SetExtendedErrorCode(const error: HResult): HResult; stdcall;

  end;
  // IMFMediaError
  IID_IMFMediaError = IMFMediaError;
  {$EXTERNALSYM IID_IMFMediaError}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaTimeRange
  //  Synopsis:   IMFMediaTimeRange represents a list of ranges (periods) of time
  //  See http://dev.w3.org/html5/spec/video.html#timeranges for details
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaTimeRange);'}
  {$EXTERNALSYM IMFMediaTimeRange}
  IMFMediaTimeRange = interface(IUnknown)
  ['{db71a2fc-078a-414e-9df9-8c2531b0aa6c}']

    function GetLength(): DWORD; stdcall;

    function GetStart(const index: DWORD;
                      out pStart: Double): HResult; stdcall;

    function GetEnd(const index: DWORD;
                    out pEnd: Double): HResult; stdcall;
    // Extensions
    function ContainsTime(time: Double): BOOL; stdcall;

    function AddRange(startTime: Double;
                      endTime: Double): HResult; stdcall;

    function Clear(): HResult; stdcall;

  end;
  IID_IMFMediaTimeRange = IMFMediaTimeRange;
  {$EXTERNALSYM IID_IMFMediaTimeRange}


  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MEDIA_ENGINE_EVENT
  //  Synopsis:   A list of events generated by the media engine
  //  See http://dev.w3.org/html5/spec/video.html#mediaevents for details
  //------------------------------------------------------------------------------
  PMF_MEDIA_ENGINE_EVENT = ^MF_MEDIA_ENGINE_EVENT;
  PMfMediaEngineEvent = ^MF_MEDIA_ENGINE_EVENT;
  MF_MEDIA_ENGINE_EVENT                  = (
    //
    // Standard events
    //
    MF_MEDIA_ENGINE_EVENT_LOADSTART          = 1,
    MF_MEDIA_ENGINE_EVENT_PROGRESS           = 2,
    MF_MEDIA_ENGINE_EVENT_SUSPEND            = 3,
    MF_MEDIA_ENGINE_EVENT_ABORT              = 4,
    MF_MEDIA_ENGINE_EVENT_ERROR              = 5,
    MF_MEDIA_ENGINE_EVENT_EMPTIED            = 6,
    MF_MEDIA_ENGINE_EVENT_STALLED            = 7,
    MF_MEDIA_ENGINE_EVENT_PLAY               = 8,
    MF_MEDIA_ENGINE_EVENT_PAUSE              = 9,
    MF_MEDIA_ENGINE_EVENT_LOADEDMETADATA     = 10,
    MF_MEDIA_ENGINE_EVENT_LOADEDDATA         = 11,
    MF_MEDIA_ENGINE_EVENT_WAITING            = 12,
    MF_MEDIA_ENGINE_EVENT_PLAYING            = 13,
    MF_MEDIA_ENGINE_EVENT_CANPLAY            = 14,
    MF_MEDIA_ENGINE_EVENT_CANPLAYTHROUGH     = 15,
    MF_MEDIA_ENGINE_EVENT_SEEKING            = 16,
    MF_MEDIA_ENGINE_EVENT_SEEKED             = 17,
    MF_MEDIA_ENGINE_EVENT_TIMEUPDATE         = 18,
    MF_MEDIA_ENGINE_EVENT_ENDED              = 19,
    MF_MEDIA_ENGINE_EVENT_RATECHANGE         = 20,
    MF_MEDIA_ENGINE_EVENT_DURATIONCHANGE     = 21,
    MF_MEDIA_ENGINE_EVENT_VOLUMECHANGE       = 22,
    //
    // Extensions
    //
    MF_MEDIA_ENGINE_EVENT_FORMATCHANGE       = 1000,
    MF_MEDIA_ENGINE_EVENT_PURGEQUEUEDEVENTS  = 1001,
    MF_MEDIA_ENGINE_EVENT_TIMELINE_MARKER    = 1002,
    MF_MEDIA_ENGINE_EVENT_BALANCECHANGE      = 1003,
    MF_MEDIA_ENGINE_EVENT_DOWNLOADCOMPLETE   = 1004,
    MF_MEDIA_ENGINE_EVENT_BUFFERINGSTARTED   = 1005,
    MF_MEDIA_ENGINE_EVENT_BUFFERINGENDED     = 1006,
    MF_MEDIA_ENGINE_EVENT_FRAMESTEPCOMPLETED = 1007,
    MF_MEDIA_ENGINE_EVENT_NOTIFYSTABLESTATE  = 1008,
    MF_MEDIA_ENGINE_EVENT_FIRSTFRAMEREADY    = 1009,
    MF_MEDIA_ENGINE_EVENT_TRACKSCHANGE       = 1010,
    MF_MEDIA_ENGINE_EVENT_OPMINFO            = 1011,
    MF_MEDIA_ENGINE_EVENT_RESOURCELOST       = 1012,
    MF_MEDIA_ENGINE_EVENT_DELAYLOADEVENT_CHANGED  = 1013,
    MF_MEDIA_ENGINE_EVENT_STREAMRENDERINGERROR	  = 1014,
    MF_MEDIA_ENGINE_EVENT_SUPPORTEDRATES_CHANGED	= 1015,
    MF_MEDIA_ENGINE_EVENT_AUDIOENDPOINTCHANGE     = 1016
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_EVENT}
  MfMediaEngineEvent = MF_MEDIA_ENGINE_EVENT;
  {$EXTERNALSYM MfMediaEngineEvent}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineNotify
  //  Synopsis:   IMFMediaEngineNotify fires events
  //  This interface is implemented by the component using IMFMediaEngine
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineNotify);'}
  {$EXTERNALSYM IMFMediaEngineNotify}
  IMFMediaEngineNotify = interface(IUnknown)
  ['{fee7c112-e776-42b5-9bbf-0048524e2bd5}']

    function EventNotify(event: DWORD; {MF_MEDIA_ENGINE_EVENT}
                         param1: DWORD_PTR;
                         param2: DWORD): HResult; stdcall;

  end;
  // IMFMediaEngineNotify
  IID_IMFMediaEngineNotify = IMFMediaEngineNotify;
  {$EXTERNALSYM IID_IMFMediaEngineNotify}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineSrcElements
  //  Synopsis:   IMFMediaEngineSrcElements represents a list of <source> elements
  //  See http://dev.w3.org/html5/spec/video.html#the-source-element for details
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineSrcElements);'}
  {$EXTERNALSYM IMFMediaEngineSrcElements}
  IMFMediaEngineSrcElements = interface(IUnknown)
  ['{7a5e5354-b114-4c72-b991-3131d75032ea}']

    function GetLength(): DWORD; stdcall;

    function GetURL(const index: DWORD;
                    out pURL: BSTR): HResult; stdcall;

    function GetType(const index: DWORD;
                     out pType: BSTR): HResult; stdcall;

    function GetMedia(const index: DWORD;
                      out pMedia: BSTR): HResult; stdcall;

    function AddElement(pURL: BSTR;
                        pType: BSTR;
                        pMedia: BSTR): HResult; stdcall;

    function RemoveAllElements(): HResult; stdcall;

  end;
  // IMFMediaEngineSrcElements
  IID_IMFMediaEngineSrcElements = IMFMediaEngineSrcElements;
  {$EXTERNALSYM IID_IMFMediaEngineSrcElements}


  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MEDIA_ENGINE_NETWORK
  //  Synopsis:   Defines different network states of the <audio>/<video> elements
  //  See http://dev.w3.org/html5/spec/video.html#dom-media-networkstate for details
  //------------------------------------------------------------------------------
  PMfMediaEngineNetwork = ^MfMediaEngineNetwork;
  MF_MEDIA_ENGINE_NETWORK             = (
    MF_MEDIA_ENGINE_NETWORK_EMPTY     = 0,
    MF_MEDIA_ENGINE_NETWORK_IDLE      = 1,
    MF_MEDIA_ENGINE_NETWORK_LOADING   = 2,
    MF_MEDIA_ENGINE_NETWORK_NO_SOURCE = 3
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_NETWORK}
  MfMediaEngineNetwork = MF_MEDIA_ENGINE_NETWORK;
  {$EXTERNALSYM MfMediaEngineNetwork}


  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MEDIA_ENGINE_READY
  //  Synopsis:   Defines different ready states of the <audio>/<video> elements
  //  See http://dev.w3.org/html5/spec/video.html#the-ready-states for details
  //------------------------------------------------------------------------------
  PMfMediaEngineReady = ^MfMediaEngineReady;
  MF_MEDIA_ENGINE_READY                     = (
    MF_MEDIA_ENGINE_READY_HAVE_NOTHING      = 0,
    MF_MEDIA_ENGINE_READY_HAVE_METADATA     = 1,
    MF_MEDIA_ENGINE_READY_HAVE_CURRENT_DATA = 2,
    MF_MEDIA_ENGINE_READY_HAVE_FUTURE_DATA  = 3,
    MF_MEDIA_ENGINE_READY_HAVE_ENOUGH_DATA  = 4
  );
  MfMediaEngineReady = MF_MEDIA_ENGINE_READY;
  {$EXTERNALSYM MfMediaEngineReady}

  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MEDIA_ENGINE_CANPLAY
  //  Synopsis:   Defines the likelihood that the <audio>/<video> elements will be able
  //              to play a source
  //  See http://dev.w3.org/html5/spec/video.html#dom-navigator-canplaytype for details
  //------------------------------------------------------------------------------
  PMfMediaEngineCanplay = ^MfMediaEngineCanplay;
  MF_MEDIA_ENGINE_CANPLAY                 = (
    MF_MEDIA_ENGINE_CANPLAY_NOT_SUPPORTED = 0,
    MF_MEDIA_ENGINE_CANPLAY_MAYBE         = 1,
    MF_MEDIA_ENGINE_CANPLAY_PROBABLY      = 2
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_CANPLAY}
  MfMediaEngineCanplay = MF_MEDIA_ENGINE_CANPLAY;
  {$EXTERNALSYM MfMediaEngineCanplay}


  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MEDIA_ENGINE_PRELOAD
  //  Synopsis:   Defines different types of preloads
  //  See http://dev.w3.org/html5/spec/video.html#attr-media-preload for details
  //------------------------------------------------------------------------------
  PMfMediaEnginePreload = ^MfMediaEnginePreload;
  {$EXTERNALSYM MF_MEDIA_ENGINE_PRELOAD}
  MF_MEDIA_ENGINE_PRELOAD             = (
    MF_MEDIA_ENGINE_PRELOAD_MISSING   = 0,
    MF_MEDIA_ENGINE_PRELOAD_EMPTY     = 1,
    MF_MEDIA_ENGINE_PRELOAD_NONE      = 2,
    MF_MEDIA_ENGINE_PRELOAD_METADATA  = 3,
    MF_MEDIA_ENGINE_PRELOAD_AUTOMATIC = 4
  );
  {$EXTERNALSYM MfMediaEnginePreload}
  MfMediaEnginePreload = MF_MEDIA_ENGINE_PRELOAD;

 // Also declared in EVR.
 // Note: in Delphi this type is defined as TRectF.
 //       This struct is also defined in MfPack.MfpTypes

  // PMFVideoNormalizedRect = ^MFVideoNormalizedRect;
  // MFVideoNormalizedRect = record
  //   left: FLOAT;
  //   top: FLOAT;
  //   right: FLOAT;
  //   bottom: FLOAT;
  // end;

{$IFNDEF MFVideoNormalizedRect}
  PMFVideoNormalizedRect = PRectF;
  MFVideoNormalizedRect = TRectF;
  {$EXTERNALSYM MFVideoNormalizedRect}
{$DEFINE MFVideoNormalizedRect}
{$ENDIF}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngine
  //  Synopsis:   IMFMediaEngine provides audio and video playback for the
  //              HTML5 <audio> and <video> elements.
  //  See http://dev.w3.org/html5/spec/video.html#video for details
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngine);'}
  {$EXTERNALSYM IMFMediaEngine}
  IMFMediaEngine = interface(IUnknown)
  ['{98a1b0bb-03eb-4935-ae7c-93c1fa0e1c93}']

    // Error state
    function GetError(out ppError: IMFMediaError): HResult; stdcall;

    function SetErrorCode(const error: MF_MEDIA_ENGINE_ERR): HResult; stdcall;

    // Network state
    function SetSourceElements(pSrcElements: IMFMediaEngineSrcElements): HResult; stdcall;

    function SetSource(const pUrl: PWideChar): HResult; stdcall;

    function GetCurrentSource(out ppUrl: PWideChar): HResult; stdcall;

    function GetNetworkState(): USHORT; stdcall;

    function GetPreload(): MF_MEDIA_ENGINE_PRELOAD; stdcall;

    function SetPreload(Preload: MF_MEDIA_ENGINE_PRELOAD): HResult; stdcall;

    function GetBuffered(ppBuffered: IMFMediaTimeRange): HResult; stdcall;

    function Load(): HResult; stdcall;

    function CanPlayType(bstype: PWideChar;
                         out pAnswer: MF_MEDIA_ENGINE_CANPLAY): HResult; stdcall;

    // Ready state
    function GetReadyState(): USHORT; stdcall;

    function IsSeeking(): BOOL; stdcall;

    // Playback state
    function GetCurrentTime(): Double; stdcall;

    function SetCurrentTime(const seekTime: Double): HResult; stdcall;

    function GetStartTime(): Double; stdcall;

    function GetDuration(): Double; stdcall; // Returns the duration, in seconds.

    function IsPaused(): BOOL; stdcall;

    function GetDefaultPlaybackRate(): Double; stdcall;

    function SetDefaultPlaybackRate(const Rate: Double): HResult; stdcall;

    function GetPlaybackRate(): Double; stdcall;

    function SetPlaybackRate(const Rate: Double): HResult; stdcall;

    function GetPlayed(out ppPlayed: IMFMediaTimeRange): HResult; stdcall;

    function GetSeekable(out ppSeekable: IMFMediaTimeRange): HResult; stdcall;

    function IsEnded(): BOOL; stdcall;

    function GetAutoPlay(): BOOL; stdcall;

    function SetAutoPlay(const AutoPlay: BOOL): HResult; stdcall;

    function GetLoop(): BOOL; stdcall;

    function SetLoop(Loop: BOOL): HResult; stdcall;

    function Play(): HResult; stdcall;

    function Pause(): HResult; stdcall;

    // Controls
    function GetMuted(): BOOL; stdcall;

    function SetMuted(const Muted: BOOL): HResult; stdcall;

    function GetVolume(): Double; stdcall;

    function SetVolume(const Volume: Double): HResult; stdcall;

    // Extensions
    function HasVideo(): BOOL; stdcall;

    function HasAudio(): BOOL; stdcall;

    function GetNativeVideoSize(out cx: DWORD;
                                out cy: DWORD): HResult; stdcall;

    function GetVideoAspectRatio(out cx: DWORD;
                                 out cy: DWORD): HResult; stdcall;

    function Shutdown(): HResult; stdcall;

    // Copies the current video frame to a DXGI surface or WIC bitmap.
    //
    // Remarks:
    // In frame-server mode, call this method to blit the video frame to a DXGI or WIC surface.
    // The application can call this method at any time after the Media Engine loads a
    // video resource.
    // Typically, however, the application calls IMFMediaEngine.OnVideoStreamTick first,
    // to determine whether a new frame is available.
    // If OnVideoStreamTick returns S_OK, the application then calls TransferVideoFrame.
    //
    // The Media Engine scales and letterboxes the video to fit the destination rectangle.
    // It fills the letterbox area with the border color.
    //
    // For protected content, call the IMFMediaEngineProtectedContent.TransferVideoFrame method
    // instead of this method.
    function TransferVideoFrame(pDstSurf: IUnknown; // Delphi note: Pass the parameter as IUnknown(DXGI or WICbitmap interface)
                         {opt}  pSrc: PMFVideoNormalizedRect;
                         {opt}  pDst: PRECT;
                                pBorderClr: MFARGB): HResult; stdcall;

    // If a new frame is ready, receives the presentation time of the frame.
    //
    // Remarks:
    // In frame-server mode, the application should call this method whenever a
    // vertical blank occurs in the display device.
    // If the method returns S_OK, call IMFMediaEngine.TransferVideoFrame to blit
    // the frame to the render target.
    // If the method returns S_FALSE, wait for the next vertical blank and call the method again.
    //
    // Do not call this method in rendering- or audio-only mode!
    function OnVideoStreamTick(out pPts: LONGLONG): HResult; stdcall;


  end;
  // IMFMediaEngine
  IID_IMFMediaEngine = IMFMediaEngine;
  {$EXTERNALSYM IID_IMFMediaEngine}


  PMfMediaEngineS3dPackingMode = ^MfMediaEngineS3dPackingMode;
  MF_MEDIA_ENGINE_S3D_PACKING_MODE                = (
    MF_MEDIA_ENGINE_S3D_PACKING_MODE_NONE         = 0,
    MF_MEDIA_ENGINE_S3D_PACKING_MODE_SIDE_BY_SIDE = 1,
    MF_MEDIA_ENGINE_S3D_PACKING_MODE_TOP_BOTTOM   = 2
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_S3D_PACKING_MODE}
  MfMediaEngineS3dPackingMode = MF_MEDIA_ENGINE_S3D_PACKING_MODE;
  {$EXTERNALSYM MfMediaEngineS3dPackingMode}


  PMfMediaEngineStatistic = ^MfMediaEngineStatistic;
  MF_MEDIA_ENGINE_STATISTIC                     = (
    MF_MEDIA_ENGINE_STATISTIC_FRAMES_RENDERED   = 0,
    MF_MEDIA_ENGINE_STATISTIC_FRAMES_DROPPED    = 1,
    MF_MEDIA_ENGINE_STATISTIC_BYTES_DOWNLOADED  = 2,
    MF_MEDIA_ENGINE_STATISTIC_BUFFER_PROGRESS   = 3,
    MF_MEDIA_ENGINE_STATISTIC_FRAMES_PER_SECOND = 4,
    MF_MEDIA_ENGINE_STATISTIC_PLAYBACK_JITTER   = 5,
    MF_MEDIA_ENGINE_STATISTIC_FRAMES_CORRUPTED  = 6,
    MF_MEDIA_ENGINE_STATISTIC_TOTAL_FRAME_DELAY = 7
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_STATISTIC}
  MfMediaEngineStatistic = MF_MEDIA_ENGINE_STATISTIC;
  {$EXTERNALSYM MfMediaEngineStatistic}


  PMfMediaEngineSeekMode = ^MfMediaEngineSeekMode;
  MF_MEDIA_ENGINE_SEEK_MODE               = (
    MF_MEDIA_ENGINE_SEEK_MODE_NORMAL      = 0,
    MF_MEDIA_ENGINE_SEEK_MODE_APPROXIMATE = 1
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_SEEK_MODE}
  MfMediaEngineSeekMode = MF_MEDIA_ENGINE_SEEK_MODE;
  {$EXTERNALSYM MfMediaEngineSeekMode}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineEx
  //  Synopsis:   IMFMediaEngineEx extends the media engine beyond the basic
  //              specification of the HTML5 <audio> and <video> elements.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineEx);'}
  {$EXTERNALSYM IMFMediaEngineEx}
  IMFMediaEngineEx = interface(IMFMediaEngine)
  ['{83015ead-b1e6-40d0-a98a-37145ffe1ad1}']
    function SetSourceFromByteStream(pByteStream: IMFByteStream;
                                     const pURL: BSTR): HResult; stdcall;

    function GetStatistics(StatisticID: MF_MEDIA_ENGINE_STATISTIC;
                           out pStatistic: PROPVARIANT): HResult; stdcall;

    // Updates the source rectangle, destination rectangle, and border color for the video.
    // param cSrc: A pointer to an MFVideoNormalizedRect structure that specifies the source rectangle.
    //             The source rectangle defines the area of the video frame that is displayed.
    //             If this parameter is Nil, the entire video frame is displayed.
    // param cDst: A pointer to a RECT structure that specifies the destination rectangle.
    //             The destination rectangle defines the area of the window or DirectComposition
    //             visual where the video is drawn.
    // param pBorderClr: A pointer to an MFARGB structure that specifies the border color.
    // remarks
    //   In rendering mode, call this method to reposition the video, update the border color,
    //   or repaint the video frame. If all of the parameters are Nil,
    //   the method repaints the most recent video frame.
    //   In frame-server mode, this method has no effect.
    //   See Video Processor MFT for info regarding source and destination rectangles in the Video Processor MFT.
    function UpdateVideoStream(pSrc: PMFVideoNormalizedRect = Nil;
                               pDst: PRect = Nil;
                               pBorderClr: PMFARGB = Nil): HResult; stdcall;

    function GetBalance(): Double; stdcall;

    // The audio balance.
    // The value can be any number in the following range (inclusive).
    // Value	  Meaning
    // ~~~~~    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // -1       The left channel is at full volume; the right channel is silent.
    //  1       The right channel is at full volume; the left channel is silent.
    //
    // If the value is zero, the left and right channels are at equal volumes.
    // The default value is zero.
    function SetBalance(const balance: Double): HResult; stdcall;

    function IsPlaybackRateSupported(rate: Double): BOOL; stdcall;

    function FrameStep(bForward: BOOL): HResult; stdcall;

    function GetResourceCharacteristics(out pCharacteristics: DWORD): HResult; stdcall;

    function GetPresentationAttribute(const guidMFAttribute: REFGUID;
                                      out pvValue: PROPVARIANT): HResult; stdcall;

    function GetNumberOfStreams(out pdwStreamCount: DWORD): HResult; stdcall;

    function GetStreamAttribute(const dwStreamIndex: DWORD;
                                guidMFAttribute: REFGUID;
                                out pvValue: PROPVARIANT): HResult; stdcall;

    function GetStreamSelection(const dwStreamIndex: DWORD;
                                out pEnabled: BOOL): HResult; stdcall;

    function SetStreamSelection(const dwStreamIndex: DWORD;
                                Enabled: BOOL): HResult; stdcall;

    function ApplyStreamSelections(): HResult; stdcall;

    function IsProtected(out pProtected: BOOL): HResult; stdcall;

    function InsertVideoEffect(pEffect: IUnknown;
                               fOptional: BOOL): HResult; stdcall;

    function InsertAudioEffect(pEffect: IUnknown;
                               fOptional: BOOL): HResult; stdcall;

    function RemoveAllEffects(): HResult; stdcall;

    function SetTimelineMarkerTimer(const timeToFire: Double): HResult; stdcall;

    function GetTimelineMarkerTimer(out pTimeToFire: PDouble): HResult; stdcall;

    function CancelTimelineMarkerTimer(): HResult; stdcall;

    // Stereoscopic 3D support
    function IsStereo3D(): BOOL; stdcall;

    function GetStereo3DFramePackingMode(out packMode: MF_MEDIA_ENGINE_S3D_PACKING_MODE): HResult; stdcall;

    function SetStereo3DFramePackingMode(packMode: MF_MEDIA_ENGINE_S3D_PACKING_MODE): HResult; stdcall;

    function GetStereo3DRenderMode(out outputType: MF3DVideoOutputType): HResult; stdcall;

    function SetStereo3DRenderMode(outputType: MF3DVideoOutputType): HResult; stdcall;

    // DComp support
    function EnableWindowlessSwapchainMode(fEnable: BOOL): HResult; stdcall;

    function GetVideoSwapchainHandle(out phSwapchain: THandle): HResult; stdcall;

    // video mirroring
    function EnableHorizontalMirrorMode(fEnable: BOOL): HResult; stdcall;

    // audio settings for next resource load
    function GetAudioStreamCategory(out pCategory: UINT32): HResult; stdcall;

    function SetAudioStreamCategory(category: UINT32): HResult; stdcall;

    function GetAudioEndpointRole(out pRole: UINT32): HResult; stdcall;

    function SetAudioEndpointRole(role: UINT32): HResult; stdcall;

    function GetRealTimeMode(out pfEnabled: BOOL): HResult; stdcall;

    // Sets the real time mode used for the next call to SetSource or Load.
    function SetRealTimeMode(fEnable: BOOL): HResult; stdcall;

    // advanced seeking support
    function SetCurrentTimeEx(seekTime: Double;
                              seekMode: MF_MEDIA_ENGINE_SEEK_MODE): HResult; stdcall;
    // timer control
    // Enables or disables the time update timer.
    function EnableTimeUpdateTimer(fEnableTimer: BOOL): HResult; stdcall;

  end;
  // IMFMediaEngineEx
  IID_IMFMediaEngineEx = IMFMediaEngineEx;
  {$EXTERNALSYM IID_IMFMediaEngineEx}



  //+-----------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaEngineAudioEndpointId
  //
  //  Synopsis:   IMFMediaEngineAudioEndpointId extends the media engine for audio endpoint Ids
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineAudioEndpointId);'}
  {$EXTERNALSYM IMFMediaEngineAudioEndpointId}
  IMFMediaEngineAudioEndpointId = interface(IUnknown)
  ['{7a3bac98-0e76-49fb-8c20-8a86fd98eaf2}']

    function SetAudioEndpointId(pszEndpointId: LPCWSTR): HResult; stdcall;

    function GetAudioEndpointId(ppszEndpointId:  LPWSTR): HResult; stdcall;

  end;
  IID_IMFMediaEngineAudioEndpointId = IMFMediaEngineAudioEndpointId;
  {$EXTERNALSYM IID_IMFMediaEngineAudioEndpointId}


  PMfMediaEngineExtensionType = ^MfMediaEngineExtensionType;
  MF_MEDIA_ENGINE_EXTENSION_TYPE               = (
    MF_MEDIA_ENGINE_EXTENSION_TYPE_MEDIASOURCE = 0,
    MF_MEDIA_ENGINE_EXTENSION_TYPE_BYTESTREAM  = 1
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_EXTENSION_TYPE}
  MfMediaEngineExtensionType = MF_MEDIA_ENGINE_EXTENSION_TYPE;
  {$EXTERNALSYM MfMediaEngineExtensionType}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMediaEngineExtension
  //
  //  Synopsis:   Allows Media Engine clients to extend the range of formats
  //              supported by the Media Engine by providing the Media Engine
  //              with a mechanism to load custom MF Media Sources.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineExtension);'}
  {$EXTERNALSYM IMFMediaEngineExtension}
  IMFMediaEngineExtension = interface(IUnknown)
  ['{2f69d622-20b5-41e9-afdf-89ced1dda04e}']
    function CanPlayType(const AudioOnly: BOOL;   // Are we being called by an audio or video tag
                         MimeType: BSTR;          // Mime type of interest
                         out pAnswer: MF_MEDIA_ENGINE_CANPLAY): HResult; // Answer returned here

    function BeginCreateObject(bstrURL: PWideChar;               // URL of object to be created
                               pByteStream: IMFByteStream;       // Optional bytestream to use
                               mftype: MF_OBJECT_TYPE;           // Type of object to be created
                               out ppIUnknownCancelCookie: PIUnknown;  // Cancel cookie to be used if we abort the operation early
                               pCallback: IMFAsyncCallback;      // Callback to be invoked when operation completes
                               punkState: IUnknown): HResult; stdcall;   // Optional async state

    function CancelObjectCreation (pIUnknownCancelCookie: IUnknown): HResult; stdcall; // cancel cookie from BeginCreateObject

    function EndCreateObject(pResult: IMFAsyncResult;  // Object creation result
                             out ppObject: IUnknown): HResult; stdcall;   // pointer to created object

  end;
  // IMFMediaEngineExtension
  IID_IMFMediaEngineExtension = IMFMediaEngineExtension;
  {$EXTERNALSYM IID_IMFMediaEngineExtension}

  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineProtectedContent
  //  Synopsis:   Support for playback of DRM'ed content by the
  //              supported by the Media Engine by providing the Media Engine
  //              with a mechanism to load custom MF Media Sources.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineProtectedContent);'}
  {$EXTERNALSYM IMFMediaEngineProtectedContent}
  IMFMediaEngineProtectedContent = interface(IUnknown)
  ['{9f8021e8-9c8c-487e-bb5c-79aa4779938c}']

    function ShareResources(pUnkDeviceContext: IUnknown): HResult; stdcall;
    // Call this to share surfaces in a device with the PMP process

    function GetRequiredProtections(out pFrameProtectionFlags: PDWORD): HResult; stdcall;
    // Gets the content protections that must be applied in frame-server mode.
    // Receives a bitwise OR of zero or more flags from the MF_MEDIA_ENGINE_FRAME_PROTECTION_FLAGS enumeration.

    // these may be required by some or all frames in the content.
    function SetOPMWindow(hwnd: HWND): HResult; stdcall;

    // Set the window to apply link protections to
    function TransferVideoFrame(pDstSurf: IUnknown;
                                pSrc: MFVideoNormalizedRect;
                                pDst: TRECT;
                                pBorderClr: MFARGB;
                                out  pFrameProtectionFlags: PDWORD): HResult; stdcall;
    // Transfer a frame from protected content.

    function SetContentProtectionManager(pCPM: IMFContentProtectionManager): HResult; stdcall;
    // Set the content protection manager.

    function SetApplicationCertificate(pbBlob: PByte;
                                       cbBlob: DWORD): HResult; stdcall;
    // Set certificate - needed for accessing raw protected frames.

  end;
  // IMFMediaEngineProtectedContent
  IID_IMFMediaEngineProtectedContent = IMFMediaEngineProtectedContent;
  {$EXTERNALSYM IID_IMFMediaEngineProtectedContent}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:  IAudioSourceProvider
  //
  //  Synopsis:   IAudioSourceProvider allows the caller (when Web Audio is connected)
  //              to obtain uncompressed audio data (i.e. audio "frame server")
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSourceProvider);'}
  {$EXTERNALSYM IAudioSourceProvider}
  IAudioSourceProvider = interface(IUnknown)
  ['{EBBAF249-AFC2-4582-91C6-B60DF2E84954}']

    function ProvideInput(dwSampleCount: DWORD;
                          var pdwChannelCount: DWORD;
                          out pInterleavedAudioData: Single): HResult; stdcall;

  end;
  // IAudioSourceProvider
  IID_IAudioSourceProvider = IAudioSourceProvider;
  {$EXTERNALSYM IID_IAudioSourceProvider}


  //+-----------------------------------------------------------------------------
  //
  //  Enumeration:  MF_MSE_WEBM_SUPPORT_TYPE
  //
  //  Synopsis:   Controls WebM support in MSE
  //
  //------------------------------------------------------------------------------
  PMF_MSE_VP9_SUPPORT_TYPE = ^MF_MSE_VP9_SUPPORT_TYPE;
  MF_MSE_VP9_SUPPORT_TYPE      = (
    MF_MSE_VP9_SUPPORT_DEFAULT = 0,
    MF_MSE_VP9_SUPPORT_ON      = 1,
    MF_MSE_VP9_SUPPORT_OFF     = 2
  );
  {$EXTERNALSYM MF_MSE_VP9_SUPPORT_TYPE}


  PMF_MSE_OPUS_SUPPORT_TYPE = ^MF_MSE_OPUS_SUPPORT_TYPE;
  MF_MSE_OPUS_SUPPORT_TYPE  = (
    MF_MSE_OPUS_SUPPORT_ON  = 0,
    MF_MSE_OPUS_SUPPORT_OFF = 1
  );
  {$EXTERNALSYM MF_MSE_OPUS_SUPPORT_TYPE}

  //+-----------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaEngineWebSupport
  //
  //  Synopsis:   IMFMediaEngineWebSupport extends the Media Engine to support the
  //              latest W3C specifications.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineWebSupport);'}
  {$EXTERNALSYM IMFMediaEngineWebSupport}
  IMFMediaEngineWebSupport = interface(IUnknown)
  ['{ba2743a1-07e0-48ef-84b6-9a2ed023ca6c}']

    function ShouldDelayTheLoadEvent(): BOOL; stdcall;

    function ConnectWebAudio(dwSampleRate: DWORD;
                             out ppSourceProvider: IAudioSourceProvider): HResult; stdcall;

    function DisconnectWebAudio(): HResult; stdcall;

  end;
  // IMFMediaEngineWebSupport
  IID_IMFMediaEngineWebSupport = IMFMediaEngineWebSupport;
  {$EXTERNALSYM IID_IMFMediaEngineWebSupport}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaSourceExtensionNotify
  //
  //  Synopsis:   callback interfaces used to notify MSE clients about important
  //              events within the MSE.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourceExtensionNotify);'}
  {$EXTERNALSYM IMFMediaSourceExtensionNotify}
  IMFMediaSourceExtensionNotify = interface(IUnknown)
  ['{a7901327-05dd-4469-a7b7-0e01979e361d}']

    procedure OnSourceOpen(); stdcall;

    procedure OnSourceEnded(); stdcall;

    procedure OnSourceClose(); stdcall;

  end;
  // IMFMediaSourceExtensionNotify
  IID_IMFMediaSourceExtensionNotify = IMFMediaSourceExtensionNotify;
  {$EXTERNALSYM IID_IMFMediaSourceExtensionNotify}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFBufferListNotify
  //  Synopsis:   callback interfaces used to notify MSE buffer list clients
  //              about important events associated with the buffer list.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFBufferListNotify);'}
  {$EXTERNALSYM IMFBufferListNotify}
  IMFBufferListNotify = interface(IUnknown)
  ['{24cd47f7-81d8-4785-adb2-af697a963cd2}']

    procedure OnAddSourceBuffer(); stdcall;

    procedure OnRemoveSourceBuffer(); stdcall;

  end;
  // IMFBufferListNotify
  IID_IMFBufferListNotify = IMFBufferListNotify;
  {$EXTERNALSYM IID_IMFBufferListNotify}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFSourceBufferNotify
  //  Synopsis:   callback interfaces used to notify MSE buffer list clients
  //              about important events associated with the buffer list.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceBufferNotify);'}
  {$EXTERNALSYM IMFSourceBufferNotify}
  IMFSourceBufferNotify = interface(IUnknown)
  ['{87e47623-2ceb-45d6-9b88-d8520c4dcbbc}']

    procedure OnAbort; stdcall;

    procedure OnError(const hr: HResult); stdcall;

    procedure OnUpdate; stdcall;

    procedure OnUpdateEnd; stdcall;

  end;
  // IMFSourceBufferNotify
  IID_IMFSourceBufferNotify = IMFSourceBufferNotify;
  {$EXTERNALSYM IID_IMFSourceBufferNotify}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFSourceBuffer
  //  Synopsis:   interface to a Source Buffer
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceBuffer);'}
  {$EXTERNALSYM IMFSourceBuffer}
  IMFSourceBuffer = interface(IUnknown)
  ['{e2cd3a4b-af25-4d3d-9110-da0e6f8ee877}']

    function GetUpdating(): BOOL; stdcall;

    function GetBuffered(out ppBuffered: IMFMediaTimeRange): HResult; stdcall;

    function GetTimeStampOffset(): Double; stdcall;

    function SetTimeStampOffset(offset: Double): HResult; stdcall;

    function GetAppendWindowStart(): Double; stdcall;

    function SetAppendWindowStart(time: Double): HResult; stdcall;

    function GetAppendWindowEnd(): Double; stdcall;

    function SetAppendWindowEnd(time: Double): HResult; stdcall;

    function Append(pData: PByte;
                    len: DWORD): HResult; stdcall;

    function AppendByteStream(pStream: IMFByteStream;
                              pMaxLen: DWORDLONG): HResult; stdcall;

    function Abort(): HResult; stdcall;

    function Remove(dstart: Double;
                    dend: Double): HResult; stdcall;

  end;
  // IMFSourceBuffer
  IID_IMFSourceBuffer = IMFSourceBuffer;
  {$EXTERNALSYM IID_IMFSourceBuffer}


// #if (WINVER >= _WIN32_WINNT_WINTHRESHOLD   (Win 8)

  PMF_MSE_APPEND_MODE = ^MF_MSE_APPEND_MODE;
  MF_MSE_APPEND_MODE            = (
    MF_MSE_APPEND_MODE_SEGMENTS = 0,
    MF_MSE_APPEND_MODE_SEQUENCE = 1
  );
  {$EXTERNALSYM MF_MSE_APPEND_MODE}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:  IMFSourceBufferAppendMode
  //
  //  Synopsis:   controls the Source Buffer's AppendMode attribute
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceBufferAppendMode);'}
  {$EXTERNALSYM IMFSourceBufferAppendMode}
  IMFSourceBufferAppendMode = interface(IUnknown)
  ['{249981f8-8325-41f3-b80c-3b9e3aad0cbe}']

    function GetAppendMode(): MF_MSE_APPEND_MODE; stdcall;

    function SetAppendMode(mode: MF_MSE_APPEND_MODE): HResult; stdcall;

  end;
  IID_IMFSourceBufferAppendMode = IMFSourceBufferAppendMode;
  {$EXTERNALSYM IID_IMFSourceBufferAppendMode}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFSourceBufferList
  //  Synopsis:   SourceBufferList interface.
  //              Events generated:
  //                  onAddSourceBuffer
  //                  onRemoveSourceBuffer
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceBufferList);'}
  {$EXTERNALSYM IMFSourceBufferList}
  IMFSourceBufferList = interface(IUnknown)
  ['{249981f8-8325-41f3-b80c-3b9e3aad0cbe}']

    function GetLength(): DWORD; stdcall;

    function GetSourceBuffer(index: DWORD): IMFSourceBuffer; stdcall;

  end;
  // IMFSourceBufferList
  IID_IMFSourceBufferList = IMFSourceBufferList;
  {$EXTERNALSYM IID_IMFSourceBufferList}



  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MSE_READY
  //  Synopsis:   Defines different ready states of the Media Source Extension
  //------------------------------------------------------------------------------
  PMfMseReady = ^MfMseReady;
  MF_MSE_READY          = (
    MF_MSE_READY_CLOSED = 1,
    MF_MSE_READY_OPEN   = 2,
    MF_MSE_READY_ENDED  = 3
  );
  {$EXTERNALSYM MF_MSE_READY}
  MfMseReady = MF_MSE_READY;
  {$EXTERNALSYM MfMseReady}


  //+-----------------------------------------------------------------------------
  //  Enumeration:  MF_MSE_ERROR
  //  Synopsis:   Defines different error states of the Media Source Extension
  //------------------------------------------------------------------------------
  PMfMseError = ^MfMseError;
  MF_MSE_ERROR                 = (
    MF_MSE_ERROR_NOERROR       = 0,
    MF_MSE_ERROR_NETWORK       = 1,
    MF_MSE_ERROR_DECODE        = 2,
    MF_MSE_ERROR_UNKNOWN_ERROR = 3
  );
  {$EXTERNALSYM MF_MSE_ERROR}
  MfMseError = MF_MSE_ERROR;
  {$EXTERNALSYM MfMseError}



  //+-----------------------------------------------------------------------------
  //  Class:      IMFMediaSourceExtension
  //  Synopsis:   Media Source extension interface
  //              events generated:
  //               onSourceOpen
  //               onSourceEnded
  //               onSourceClose
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourceExtension);'}
  {$EXTERNALSYM IMFMediaSourceExtension}
  IMFMediaSourceExtension = interface(IUnknown)
  ['{e467b94e-a713-4562-a802-816a42e9008a}']

    function GetSourceBuffers(): IMFSourceBufferList; stdcall;

    function GetActiveSourceBuffers(): IMFSourceBufferList; stdcall;

    function GetReadyState(): MF_MSE_READY; stdcall;
    // enum State { "closed", "open", "ended" };

    function GetDuration(): Double; stdcall;

    function SetDuration(duration: Double): HResult; stdcall;

    function AddSourceBuffer(bstype: BSTR;
                             pNotify: IMFSourceBufferNotify;
                             out ppSourceBuffer: IMFSourceBuffer): HResult; stdcall;

    function RemoveSourceBuffer(pSourceBuffer: IMFSourceBuffer): HResult; stdcall;

    function SetEndOfStream(error: MF_MSE_ERROR): HResult; stdcall;
    // enum EOSError { "network", "decode" };

    function IsTypeSupported(bstype: BSTR): BOOL; stdcall;

    function GetSourceBuffer(dwStreamIndex: DWORD): IMFSourceBuffer; stdcall;

  end;
  // IMFMediaSourceExtension
  IID_IMFMediaSourceExtension = IMFMediaSourceExtension;
  {$EXTERNALSYM IID_IMFMediaSourceExtension}


  // Interface IMFMediaSourceExtensionLiveSeekableRange
  // ==================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSourceExtensionLiveSeekableRange);'}
  {$EXTERNALSYM IMFMediaSourceExtensionLiveSeekableRange}
  IMFMediaSourceExtensionLiveSeekableRange = interface(IUnknown)
  ['{5D1ABFD6-450A-4D92-9EFC-D6B6CBC1F4DA}']

    function SetLiveSeekableRange(start: Double;
                                  _end: Double): HResult; stdcall;

    function ClearLiveSeekableRange(): HResult; stdcall;

  end;
  IID_IMFMediaSourceExtensionLiveSeekableRange = IMFMediaSourceExtensionLiveSeekableRange;
  {$EXTERNALSYM IID_IMFMediaSourceExtensionLiveSeekableRange}


  //////////////////////////////////////////////////////////////////////////////////////////
  //  Encrypted Media Extensions (EME)
  //  See http://dvcs.w3.org/hg/html-media/raw-file/tip/encrypted-media/encrypted-media.html
  //////////////////////////////////////////////////////////////////////////////////////////


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineEME
  //  Synopsis:   Extensions to the media engine for EME extensions
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineEME);'}
  {$EXTERNALSYM IMFMediaEngineEME}
  IMFMediaEngineEME = interface(IUnknown)
  ['{50dc93e4-ba4f-4275-ae66-83e836e57469}']

    function get_Keys(out keys: IMFMediaKeys): HResult; stdcall;
    // May be NIL

    function SetMediaKeys(keys: IMFMediaKeys): HResult; stdcall;

  end;
  // IMFMediaEngineEME
  IID_IMFMediaEngineEME = IMFMediaEngineEME;
  {$EXTERNALSYM IID_IMFMediaEngineEME}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineSrcElementsEx
  //  Synopsis:
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineSrcElementsEx);'}
  {$EXTERNALSYM IMFMediaEngineSrcElementsEx}
  IMFMediaEngineSrcElementsEx = interface(IUnknown)
  ['{654a6bb3-e1a3-424a-9908-53a43a0dfda0}']

    function AddElementEx(pURL: BSTR;
                          pType: BSTR;
                          pMedia: BSTR;
                          keySystem: BSTR): HResult; stdcall;

    function GetKeySystem(index: DWORD;
                          out pType: BSTR): HResult; stdcall;

  end;
  // IMFMediaEngineSrcElementsEx
  IID_IMFMediaEngineSrcElementsEx = IMFMediaEngineSrcElementsEx;
  {$EXTERNALSYM IID_IMFMediaEngineSrcElementsEx}




  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaKeysNotify
  //  Synopsis:   Implement NeedKey
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeysNotify);'}
  {$EXTERNALSYM IMFMediaKeysNotify}
  IMFMediaKeysNotify = interface(IUnknown)
  ['{46a30204-a696-4b18-8804-246b8f031bb1}']
    procedure NeedKey(initData: PByte;
                      cb: DWORD); stdcall;

  end;
  // IMFMediaKeysNotify
  IID_IMFMediaKeysNotify = IMFMediaKeysNotify;
  {$EXTERNALSYM IID_IMFMediaKeysNotify}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaKeys
  //  Synopsis:   Media Keys interface for EME
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeys);'}
  {$EXTERNALSYM IMFMediaKeys}
  IMFMediaKeys = interface(IUnknown)
  ['{5cb31c05-61ff-418f-afda-caaf41421a38}']
    function CreateSession(mimeType: BSTR;     // Optional
                           initData: PByte;    // Optional
                           cb: DWORD;          // Optional
                           customData: PByte;
                           cbCustomData: DWORD;
                           notify: IMFMediaKeySessionNotify;
                           out ppSession: IMFMediaKeySession): HResult; stdcall;

    function get_KeySystem(out keySystem: BSTR): HResult; stdcall;

    function Shutdown(): HResult; stdcall;

    function GetSuspendNotify(out notify: IMFCdmSuspendNotify): HResult; stdcall;

  end;
  // IMFMediaKeys
  IID_IMFMediaKeys = IMFMediaKeys;
  {$EXTERNALSYM IID_IMFMediaKeys}



  //  Errors - see MediaKeyError interface
  PMfMediaEngineKeyerr = ^_MF_MEDIA_ENGINE_KEYERR;
  _MF_MEDIA_ENGINE_KEYERR                = (
    MF_MEDIAENGINE_KEYERR_UNKNOWN        = 1,
    MF_MEDIAENGINE_KEYERR_CLIENT         = 2,
    MF_MEDIAENGINE_KEYERR_SERVICE        = 3,
    MF_MEDIAENGINE_KEYERR_OUTPUT         = 4,
    MF_MEDIAENGINE_KEYERR_HARDWARECHANGE = 5,
    MF_MEDIAENGINE_KEYERR_DOMAIN         = 6
  );
  {$EXTERNALSYM _MF_MEDIA_ENGINE_KEYERR}
  MF_MEDIA_ENGINE_KEYERR = _MF_MEDIA_ENGINE_KEYERR;
  {$EXTERNALSYM MF_MEDIA_ENGINE_KEYERR}
  MfMediaEngineKeyerr = _MF_MEDIA_ENGINE_KEYERR;
  {$EXTERNALSYM MfMediaEngineKeyerr}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaKeySession
  //  Synopsis:   Media Key Session interface for EME
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeySession);'}
  {$EXTERNALSYM IMFMediaKeySession}
  IMFMediaKeySession = interface(IUnknown)
  ['{24fa67d5-d1d0-4dc5-995c-c0efdc191fb5}']

    function GetError(out code: USHORT;
                      out systemCode: DWORD): HResult; stdcall;
    //  Caller must turn this into a MediaKeyError object.

    function get_KeySystem(out keySystem: BSTR): HResult; stdcall;

    function get_SessionId(out sessionId: BSTR): HResult; stdcall;

    function Update(key: PByte;
                    cb: DWORD): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  // IMFMediaKeySession
  IID_IMFMediaKeySession = IMFMediaKeySession;
  {$EXTERNALSYM IID_IMFMediaKeySession}


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaKeySessionNotify
  //  Synopsis:   Events for MediaKeySession
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeySessionNotify);'}
  {$EXTERNALSYM IMFMediaKeySessionNotify}
  IMFMediaKeySessionNotify = interface(IUnknown)
  ['{6a0083f9-8947-4c1d-9ce0-cdee22b23135}']

    procedure KeyMessage(destinationURL: BSTR;
                         _message: PByte;
                         cb: DWORD); stdcall;
    // Passes information to the application so it can initiate a key acquisition.

    procedure KeyAdded(); stdcall;
    // Notifies the application that the key has been added.

    procedure KeyError(code: USHORT;
                       systemCode: DWORD); stdcall;
    // Notifies the application that an error occurred while processing the key.
    // Caller should turn data into MediaKeyError object

  end;
  // IMFMediaKeySessionNotify
  IID_IMFMediaKeySessionNotify = IMFMediaKeySessionNotify;
  {$EXTERNALSYM IID_IMFMediaKeySessionNotify}



  //+-----------------------------------------------------------------------------
  //  Interface:  IMFCdmSuspendNotify
  //  Synopsis:   Events for MediaKeySession
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCdmSuspendNotify);'}
  {$EXTERNALSYM IMFCdmSuspendNotify}
  IMFCdmSuspendNotify = interface(IUnknown)
  ['{7a5645d2-43bd-47fd-87b7-dcd24cc7d692}']

    function hrBegin(): HResult; stdcall;

    function hrEnd(): HResult; stdcall;

  end;
  // IMFCdmSuspendNotify
  IID_IMFCdmSuspendNotify = IMFCdmSuspendNotify;
  {$EXTERNALSYM IID_IMFCdmSuspendNotify}


  ////// End of Encrypted Media Extensions ///////////////////////////////////////


  //+-----------------------------------------------------------------------------
  //
  //  Interface:  MF_HDCP_STATUS
  //
  //  Synopsis:   HDCP Status values
  //
  //------------------------------------------------------------------------------
  PMF_HDCP_STATUS = ^MF_HDCP_STATUS;
  _MF_HDCP_STATUS                           = (
    MF_HDCP_STATUS_ON                       = 0,
    MF_HDCP_STATUS_OFF                      = 1,
    MF_HDCP_STATUS_ON_WITH_TYPE_ENFORCEMENT = 2
  );
  {$EXTERNALSYM _MF_HDCP_STATUS}
  MF_HDCP_STATUS = _MF_HDCP_STATUS;
  {$EXTERNALSYM MF_HDCP_STATUS}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:  IMFHDCPStatus
  //
  //  Synopsis:   HDCP Status set/get interface
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFHDCPStatus);'}
  {$EXTERNALSYM IMFHDCPStatus}
  IMFHDCPStatus = interface(IUnknown)
  ['{DE400F54-5BF1-40CF-8964-0BEA136B1E3D}']
    function Query(var pStatus: MF_HDCP_STATUS;
                   var pfStatus: BOOL): HResult; stdcall;

    function _Set(status: MF_HDCP_STATUS): HResult; stdcall;

  end;
  IID_IMFHDCPStatus = IMFHDCPStatus;
  {$EXTERNALSYM IID_IMFHDCPStatus}


  //+-----------------------------------------------------------------------------
  //
  //  Enumeration:  MF_MEDIA_ENGINE_OPM_STATUS
  //
  //  Synopsis:   Defines the status of OPM:
  //              0. MF_MEDIA_ENGINE_OPM_NOT_REQUESTED: default status, used to
  //                     return the correct status when the content is unprotected
  //              1. MF_MEDIA_ENGINE_OPM_ESTABLISHED: OPM succussfully established
  //              2. MF_MEDIA_ENGINE_OPM_FAILED_VM: running in a VM
  //              3. MF_MEDIA_ENGINE_OPM_FAILED_BDA: there is no graphics driver,
  //                     system is using BDA
  //              4. MF_MEDIA_ENGINE_OPM_FAILED_UNSIGNED_DRIVER: the graphics
  //                     driver is not PE signed, falling back to WARP
  //              5. MF_MEDIA_ENGINE_OPM_FAILED: OPM failed for other reasons
  //
  //------------------------------------------------------------------------------


  PMfMediaEngineOpmStatus = ^MF_MEDIA_ENGINE_OPM_STATUS;
  MF_MEDIA_ENGINE_OPM_STATUS                   = (
    MF_MEDIA_ENGINE_OPM_NOT_REQUESTED          = 0,
    MF_MEDIA_ENGINE_OPM_ESTABLISHED            = 1,
    MF_MEDIA_ENGINE_OPM_FAILED_VM              = 2,
    MF_MEDIA_ENGINE_OPM_FAILED_BDA             = 3,
    MF_MEDIA_ENGINE_OPM_FAILED_UNSIGNED_DRIVER = 4,
    MF_MEDIA_ENGINE_OPM_FAILED                 = 5
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_OPM_STATUS}

  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineOPMInfo
  //  Synopsis:   Interface to get OPM information and constriction status
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineOPMInfo);'}
  {$EXTERNALSYM IMFMediaEngineOPMInfo}
  IMFMediaEngineOPMInfo = interface(IUnknown)
  ['{765763e6-6c01-4b01-bb0f-b829f60ed28c}']
    function GetOPMInfo(out pStatus: MF_MEDIA_ENGINE_OPM_STATUS;
                        out pConstricted: BOOL): HResult; stdcall;

  end;
  // IMFMediaEngineOPMInfo
  IID_IMFMediaEngineOPMInfo = IMFMediaEngineOPMInfo;
  {$EXTERNALSYM IID_IMFMediaEngineOPMInfo}


  // MFMediaEngine creation attributes  (constants are moved upstairs)
  // =================================================================
  //


  //+-----------------------------------------------------------------------------
  //  Interface:  IMFMediaEngineClassFactory
  //  Synopsis:   Allows client applications to create a new instance of the
  //              Media Engine.  CoInitialize and MFStartup must be called
  //              prior to using this interface.
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineClassFactory);'}
  {$EXTERNALSYM IMFMediaEngineClassFactory}
  IMFMediaEngineClassFactory = interface(IUnknown)
  ['{4D645ACE-26AA-4688-9BE1-DF3516990B93}']
    // Delphi note: Parameter ppPlayer is of type IMFMEdiaEngine according to the ms headers.
    //              Because it can be IMFMEdiaEngine or IMFMEdiaEngineEx we have to use
    //              IUNknown to prevent Delphi's Error E2033 Types of actual and formal var parameters must be identical.
    //              When calling this funtion, you should cast the parameter like this: IUnknown(IMFMEdiaEngine) or
    //              IUnknown(IMFMEdiaEngineEx).
    function CreateInstance(dwFlags: DWORD;
                            const pAttr: IMFAttributes;
                            out ppPlayer: IMFMediaEngine): HResult; stdcall;

    function CreateTimeRange(out ppTimeRange: IMFMediaTimeRange): HResult; stdcall;

    function CreateError(out ppError: IMFMediaError): HResult; stdcall;

  end;
  // IMFMediaEngineClassFactory
  IID_IMFMediaEngineClassFactory = IMFMediaEngineClassFactory;
  {$EXTERNALSYM IID_IMFMediaEngineClassFactory}


  // Interface IMFMediaEngineClassFactoryEx
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineClassFactoryEx);'}
  {$EXTERNALSYM IMFMediaEngineClassFactoryEx}
  IMFMediaEngineClassFactoryEx = interface(IMFMediaEngineClassFactory)
  ['{c56156c6-ea5b-48a5-9df8-fbe035d0929e}']

    function CreateMediaSourceExtension(dwFlags: DWORD;
                                        out pAttr: IMFAttributes;
                                        out ppMSE: IMFMediaSourceExtension): HResult; stdcall;

    function CreateMediaKeys(keySystem: BSTR;
                             cdmStorePath: BSTR;
                             out ppKeys: IMFMediaKeys): HResult; stdcall;

    function IsTypeSupported(bstype: BSTR;
                             keySystem: BSTR;
                             out isSupported: BOOL): HResult; stdcall;


  end;
  // IMFMediaEngineClassFactoryEx
  IID_IMFMediaEngineClassFactoryEx = IMFMediaEngineClassFactoryEx;
  {$EXTERNALSYM IID_IMFMediaEngineClassFactoryEx}



  // Interface IMFMediaEngineClassFactory2
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineClassFactory2);'}
  {$EXTERNALSYM IMFMediaEngineClassFactory2}
  IMFMediaEngineClassFactory2 = interface(IUnknown)
  ['{09083cef-867f-4bf6-8776-dee3a7b42fca}']

    function CreateMediaKeys2(keySystem: BSTR;
                              defaultCdmStorePath: BSTR;
                              inprivateCdmStorePath: BSTR;
                              out ppKeys: IMFMediaKeys): HResult; stdcall;

  end;
  // IMFMediaEngineClassFactory2
  IID_IMFMediaEngineClassFactory2 = IMFMediaEngineClassFactory2;
  {$EXTERNALSYM IID_IMFMediaEngineClassFactory2}



  // Interface IMFExtendedDRMTypeSupport
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFExtendedDRMTypeSupport);'}
  {$EXTERNALSYM IMFExtendedDRMTypeSupport}
  IMFExtendedDRMTypeSupport = interface(IUnknown)
  ['{332EC562-3758-468D-A784-E38F23552128}']

    function IsTypeSupportedEx(_type: BSTR;
                               keySystem: BSTR;
                               out pAnswer: MF_MEDIA_ENGINE_CANPLAY): HResult; stdcall;

  end;
  IID_IMFExtendedDRMTypeSupport = IMFExtendedDRMTypeSupport;
  {$EXTERNALSYM IID_IMFExtendedDRMTypeSupport}


  // Interface IMFMediaEngineSupportsSourceTransfer
  // ==============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineSupportsSourceTransfer);'}
  {$EXTERNALSYM IMFMediaEngineSupportsSourceTransfer}
  IMFMediaEngineSupportsSourceTransfer = interface(IUnknown)
  ['{a724b056-1b2e-4642-a6f3-db9420c52908}']

    function ShouldTransferSource(var pfShouldTransfer: BOOL): HResult; stdcall;

    function DetachMediaSource(out ppByteStream: IMFByteStream;
                               out ppMediaSource: IMFMediaSource;
                               out ppMSE: IMFMediaSourceExtension): HResult; stdcall;

    function AttachMediaSource(pByteStream: IMFByteStream;
                               pMediaSource: IMFMediaSource;
                               pMSE: IMFMediaSourceExtension): HResult; stdcall;

  end;
  IID_IMFMediaEngineSupportsSourceTransfer = IMFMediaEngineSupportsSourceTransfer;
  {$EXTERNALSYM IID_IMFMediaEngineSupportsSourceTransfer}


  // Interface IMFMediaEngineTransferSource
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineTransferSource);'}
  {$EXTERNALSYM IMFMediaEngineTransferSource}
  IMFMediaEngineTransferSource = interface(IUnknown)
  ['{24230452-fe54-40cc-94f3-fcc394c340d6}']

    function TransferSourceToMediaEngine(destination: IMFMediaEngine): HResult; stdcall;

  end;
  IID_IMFMediaEngineTransferSource = IMFMediaEngineTransferSource;
  {$EXTERNALSYM IID_IMFMediaEngineTransferSource}


  //////////////////////////////////////////////////////////////////////////////
  //
  //  Timed Text
  //
  //////////////////////////////////////////////////////////////////////////////


  // MFMediaEngine services
  // ======================

  PMF_TIMED_TEXT_TRACK_KIND = ^MF_TIMED_TEXT_TRACK_KIND;
  MF_TIMED_TEXT_TRACK_KIND             = (
    MF_TIMED_TEXT_TRACK_KIND_UNKNOWN   = 0,
    MF_TIMED_TEXT_TRACK_KIND_SUBTITLES = 1,
    MF_TIMED_TEXT_TRACK_KIND_CAPTIONS  = 2,
    MF_TIMED_TEXT_TRACK_KIND_METADATA  = 3
  );
  {$EXTERNALSYM MF_TIMED_TEXT_TRACK_KIND}


  PMF_TIMED_TEXT_UNIT_TYPE = ^MF_TIMED_TEXT_UNIT_TYPE;
  MF_TIMED_TEXT_UNIT_TYPE              = (
    MF_TIMED_TEXT_UNIT_TYPE_PIXELS     = 0,
    MF_TIMED_TEXT_UNIT_TYPE_PERCENTAGE = 1
  );
  {$EXTERNALSYM MF_TIMED_TEXT_UNIT_TYPE}


  PMF_TIMED_TEXT_FONT_STYLE = ^MF_TIMED_TEXT_FONT_STYLE;
  MF_TIMED_TEXT_FONT_STYLE           = (
    MF_TIMED_TEXT_FONT_STYLE_NORMAL  = 0,
    MF_TIMED_TEXT_FONT_STYLE_OBLIQUE = 1,
    MF_TIMED_TEXT_FONT_STYLE_ITALIC  = 2
  );
  {$EXTERNALSYM MF_TIMED_TEXT_FONT_STYLE}


  PMF_TIMED_TEXT_ALIGNMENT = ^MF_TIMED_TEXT_ALIGNMENT;
  MF_TIMED_TEXT_ALIGNMENT          = (
    MF_TIMED_TEXT_ALIGNMENT_START  = 0,
    MF_TIMED_TEXT_ALIGNMENT_END    = 1,
    MF_TIMED_TEXT_ALIGNMENT_CENTER = 2
  );
  {$EXTERNALSYM MF_TIMED_TEXT_ALIGNMENT}


  PMF_TIMED_TEXT_DISPLAY_ALIGNMENT = ^MF_TIMED_TEXT_DISPLAY_ALIGNMENT;
  MF_TIMED_TEXT_DISPLAY_ALIGNMENT          = (
    MF_TIMED_TEXT_DISPLAY_ALIGNMENT_BEFORE = 0,
    MF_TIMED_TEXT_DISPLAY_ALIGNMENT_AFTER  = 1,
    MF_TIMED_TEXT_DISPLAY_ALIGNMENT_CENTER = 2
  );
  {$EXTERNALSYM MF_TIMED_TEXT_DISPLAY_ALIGNMENT}


  PMF_TIMED_TEXT_DECORATION = ^MF_TIMED_TEXT_DECORATION;
  MF_TIMED_TEXT_DECORATION                = (
    MF_TIMED_TEXT_DECORATION_NONE         = 0,
    MF_TIMED_TEXT_DECORATION_UNDERLINE    = 1,
    MF_TIMED_TEXT_DECORATION_LINE_THROUGH = 2,
    MF_TIMED_TEXT_DECORATION_OVERLINE     = 4
  );
  {$EXTERNALSYM MF_TIMED_TEXT_DECORATION}


  PMF_TIMED_TEXT_WRITING_MODE = ^MF_TIMED_TEXT_WRITING_MODE;
  MF_TIMED_TEXT_WRITING_MODE        = (
    MF_TIMED_TEXT_WRITING_MODE_LRTB = 0,
    MF_TIMED_TEXT_WRITING_MODE_RLTB = 1,
    MF_TIMED_TEXT_WRITING_MODE_TBRL = 2,
    MF_TIMED_TEXT_WRITING_MODE_TBLR = 3,
    MF_TIMED_TEXT_WRITING_MODE_LR   = 4,
    MF_TIMED_TEXT_WRITING_MODE_RL   = 5,
    MF_TIMED_TEXT_WRITING_MODE_TB   = 6
  );
  {$EXTERNALSYM MF_TIMED_TEXT_WRITING_MODE}


  PMF_TIMED_TEXT_SCROLL_MODE = ^MF_TIMED_TEXT_SCROLL_MODE;
  MF_TIMED_TEXT_SCROLL_MODE           = (
    MF_TIMED_TEXT_SCROLL_MODE_POP_ON  = 0,
    MF_TIMED_TEXT_SCROLL_MODE_ROLL_UP = 1
  );
  {$EXTERNALSYM MF_TIMED_TEXT_SCROLL_MODE}


  PMF_TIMED_TEXT_ERROR_CODE = ^MF_TIMED_TEXT_ERROR_CODE;
  MF_TIMED_TEXT_ERROR_CODE               = (
    MF_TIMED_TEXT_ERROR_CODE_NOERROR     = 0,   // No error
    MF_TIMED_TEXT_ERROR_CODE_FATAL       = 1,   // Fatal, non-continuable error, the state of the component will
    MF_TIMED_TEXT_ERROR_CODE_DATA_FORMAT = 2,   // Data format error - continuable
    MF_TIMED_TEXT_ERROR_CODE_NETWORK     = 3,   // Network error - continuable
    MF_TIMED_TEXT_ERROR_CODE_INTERNAL    = 4    // Internal error, involved data sources will be disabled
  );
  {$EXTERNALSYM MF_TIMED_TEXT_ERROR_CODE}

  PMF_TIMED_TEXT_CUE_EVENT = ^MF_TIMED_TEXT_CUE_EVENT;
  MF_TIMED_TEXT_CUE_EVENT = (
    MF_TIMED_TEXT_CUE_EVENT_ACTIVE,   // Cue has become active
    MF_TIMED_TEXT_CUE_EVENT_INACTIVE, // Cue has become inactive
    MF_TIMED_TEXT_CUE_EVENT_CLEAR     // All cues has been deactivated
  );
  {$EXTERNALSYM MF_TIMED_TEXT_CUE_EVENT}


  PMF_TIMED_TEXT_TRACK_READY_STATE = ^MF_TIMED_TEXT_TRACK_READY_STATE;
  MF_TIMED_TEXT_TRACK_READY_STATE = (
    MF_TIMED_TEXT_TRACK_READY_STATE_NONE,    // Loading track cues has not been started yet.
    MF_TIMED_TEXT_TRACK_READY_STATE_LOADING, // Track cues are being loaded.
    MF_TIMED_TEXT_TRACK_READY_STATE_LOADED,  // Track cues are loaded and ready.
    MF_TIMED_TEXT_TRACK_READY_STATE_ERROR    // Track error occurred.
  );
  {$EXTERNALSYM MF_TIMED_TEXT_TRACK_READY_STATE}


  // Interface IMFTimedText
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedText);'}
  {$EXTERNALSYM IMFTimedText}
  IMFTimedText = interface(IUnknown)
  ['{1f2a94c9-a3df-430d-9d0f-acd85ddc29af}']

    function RegisterNotifications(const notify: IMFTimedTextNotify): HResult; stdcall;

    function SelectTrack(trackId: DWORD;
                         selected: BOOL): HResult; stdcall;

    function AddDataSource(byteStream: IMFByteStream;
                           _label: LPCWSTR;
                           language: LPCWSTR;
                           kind: MF_TIMED_TEXT_TRACK_KIND;
                           isDefault: BOOL;
                           out trackId: DWORD): HResult; stdcall;

    function AddDataSourceFromUrl(url: LPCWSTR;
                                  _label: LPCWSTR;
                                  language: LPCWSTR;
                                  kind: MF_TIMED_TEXT_TRACK_KIND;
                                  isDefault: BOOL;
                                  out trackId: DWORD): HResult; stdcall;

    function AddTrack(_label: LPWSTR;
                      language: LPWSTR;
                      kind: MF_TIMED_TEXT_TRACK_KIND;
                      out track: IMFTimedTextTrack): HResult; stdcall;

    function RemoveTrack(track: IMFTimedTextTrack): HResult; stdcall;

    function GetCueTimeOffset(out offset: Double): HResult; stdcall;

    function SetCueTimeOffset(offset: Double): HResult; stdcall;

    function GetTracks(out tracks: IMFTimedTextTrackList): HResult; stdcall;

    function GetActiveTracks(out activeTracks: IMFTimedTextTrackList): HResult; stdcall;

    function GetTextTracks(out textTracks: IMFTimedTextTrackList): HResult; stdcall;

    function GetMetadataTracks(out metadataTracks: IMFTimedTextTrackList): HResult; stdcall;

    function SetInBandEnabled(enabled: BOOL): HResult; stdcall;

    function IsInBandEnabled(): BOOL; stdcall;

  end;
  IID_IMFTimedText = IMFTimedText;
  {$EXTERNALSYM IID_IMFTimedText}


  // Interface IMFTimedTextNotify
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextNotify);'}
  {$EXTERNALSYM IMFTimedTextNotify}
  IMFTimedTextNotify = interface(IUnknown)
  ['{df6b87b6-ce12-45db-aba7-432fe054e57d}']

    procedure TrackAdded(trackId: DWORD); stdcall;

    procedure TrackRemoved(trackId: DWORD); stdcall;

    procedure TrackSelected(trackId: DWORD;
                            selected: BOOL); stdcall;

    procedure TrackReadyStateChanged(trackId: DWORD); stdcall;

    procedure Error(errorCode: MF_TIMED_TEXT_ERROR_CODE;
                    extendedErrorCode: HResult;
                    sourceTrackId: DWORD); stdcall;

    procedure Cue(cueEvent: MF_TIMED_TEXT_CUE_EVENT;
                  currentTime: Double;
                  cue: IMFTimedTextCue); stdcall;

    procedure Reset(); stdcall;

  end;
  IID_IMFTimedTextNotify = IMFTimedTextNotify;
  {$EXTERNALSYM IID_IMFTimedTextNotify}


  // Interface IMFTimedTextTrack
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextTrack);'}
  {$EXTERNALSYM IMFTimedTextTrack}
  IMFTimedTextTrack = interface(IUnknown)
  ['{8822c32d-654e-4233-bf21-d7f2e67d30d4}']

    function GetId(): DWORD; stdcall;

    function GetLabel(var _label: LPWSTR): HResult; stdcall;

    function SetLabel(_label: LPWSTR): HResult; stdcall;

    function GetLanguage(var language: LPWSTR): HResult; stdcall;

    function GetTrackKind(): MF_TIMED_TEXT_TRACK_KIND; stdcall;

    function IsInBand(): BOOL; stdcall;

    function GetInBandMetadataTrackDispatchType(var dispatchType: LPWSTR): HResult; stdcall;

    function IsActive(): BOOL; stdcall;

    function GetErrorCode(): MF_TIMED_TEXT_ERROR_CODE; stdcall;

    function GetExtendedErrorCode(): HResult; stdcall;

    function GetDataFormat(var format: TGUID): HResult; stdcall;

    function GetReadyState(): MF_TIMED_TEXT_TRACK_READY_STATE; stdcall;

    function GetCueList(var cues: IMFTimedTextCueList): HResult; stdcall;

  end;
  IID_IMFTimedTextTrack = IMFTimedTextTrack;
  {$EXTERNALSYM IID_IMFTimedTextTrack}


  // Interface IMFTimedTextTrackList
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextTrackList);'}
  {$EXTERNALSYM IMFTimedTextTrackList}
  IMFTimedTextTrackList = interface(IUnknown)
  ['{23ff334c-442c-445f-bccc-edc438aa11e2}']

    function GetLength(): DWORD; stdcall;

    function GetTrack(index: DWORD;
                      out track: IMFTimedTextTrack): HResult; stdcall;

    function GetTrackById(trackId: DWORD;
                          out track: IMFTimedTextTrack): HResult; stdcall;

  end;
  IID_IMFTimedTextTrackList = IMFTimedTextTrackList;
  {$EXTERNALSYM IID_IMFTimedTextTrackList}


  // Interface IMFTimedTextCue
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextCue);'}
  {$EXTERNALSYM IMFTimedTextCue}
  IMFTimedTextCue = interface(IUnknown)
  ['{1e560447-9a2b-43e1-a94c-b0aaabfbfbc9}']

    function GetId(): DWORD; stdcall;

    function GetOriginalId(out originalId: LPWSTR): HResult; stdcall;

    function GetCueKind(): MF_TIMED_TEXT_TRACK_KIND; stdcall;

    function GetStartTime(): Double; stdcall;

    function GetDuration(): Double; stdcall;

    function GetTrackId(): DWORD; stdcall;

    function GetData(out data: IMFTimedTextBinary): HResult; stdcall;

    function GetRegion(out region: IMFTimedTextRegion): HResult; stdcall;

    function GetStyle(out style: IMFTimedTextStyle): HResult; stdcall;

    function GetLineCount(): DWORD; stdcall;

    function GetLine(const index: DWORD;
                     out line: IMFTimedTextFormattedText): HResult; stdcall;

  end;
  IID_IMFTimedTextCue = IMFTimedTextCue;
  {$EXTERNALSYM IID_IMFTimedTextCue}


  // Interface IMFTimedTextFormattedText
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextFormattedText);'}
  {$EXTERNALSYM IMFTimedTextFormattedText}
  IMFTimedTextFormattedText = interface(IUnknown)
  ['{e13af3c1-4d47-4354-b1f5-e83ae0ecae60}']

    function GetText(out text: LPWSTR): HResult; stdcall;

    function GetSubformattingCount(): DWORD; stdcall;

    function GetSubformatting(const index: DWORD;
                              out firstChar: DWORD;
                              out charLength: DWORD;
                              out style: IMFTimedTextStyle): HResult; stdcall;

  end;
  IID_IMFTimedTextFormattedText = IMFTimedTextFormattedText;
  {$EXTERNALSYM IID_IMFTimedTextFormattedText}


  // Interface IMFTimedTextStyle
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextStyle);'}
  {$EXTERNALSYM IMFTimedTextStyle}
  IMFTimedTextStyle = interface(IUnknown)
  ['{09b2455d-b834-4f01-a347-9052e21c450e}']

    function GetName(out name: LPWSTR): HResult; stdcall;

    function IsExternal(): BOOL; stdcall;

    function GetFontFamily(out fontFamily: LPWSTR): HResult; stdcall;

    function GetFontSize(out fontSize: Double;
                         out unitType: MF_TIMED_TEXT_UNIT_TYPE): HResult; stdcall;

    function GetColor(out color: MFARGB): HResult; stdcall;

    function GetBackgroundColor(out bgColor: MFARGB): HResult; stdcall;

    function GetShowBackgroundAlways(out showBackgroundAlways: BOOL): HResult; stdcall;

    function GetFontStyle(out fontStyle: MF_TIMED_TEXT_FONT_STYLE): HResult; stdcall;

    function GetBold(out bold: BOOL): HResult; stdcall;

    function GetRightToLeft(out rightToLeft: BOOL): HResult; stdcall;

    function GetTextAlignment(out textAlign: MF_TIMED_TEXT_ALIGNMENT): HResult; stdcall;

    function GetTextDecoration(out textDecoration: DWORD): HResult; stdcall;

    function GetTextOutline(out color: MFARGB;
                            out thickness: Double;
                            out blurRadius: Double;
                            out unitType: MF_TIMED_TEXT_UNIT_TYPE): HResult; stdcall;

  end;
  IID_IMFTimedTextStyle = IMFTimedTextStyle;
  {$EXTERNALSYM IID_IMFTimedTextStyle}


  // Interface IMFTimedTextRegion
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextRegion);'}
  {$EXTERNALSYM IMFTimedTextRegion}
  IMFTimedTextRegion = interface(IUnknown)
  ['{c8d22afc-bc47-4bdf-9b04-787e49ce3f58}']

    function GetName(var name: LPWSTR): HResult; stdcall;

    function GetPosition(out pX: Double;
                         out pY: Double;
                         out unitType: MF_TIMED_TEXT_UNIT_TYPE): HResult; stdcall;

    function GetExtent(out pWidth: Double;
                       out pHeight: Double;
                       out unitType: MF_TIMED_TEXT_UNIT_TYPE): HResult; stdcall;

    function GetBackgroundColor(out bgColor: MFARGB): HResult; stdcall;

    function GetWritingMode(out writingMode: MF_TIMED_TEXT_WRITING_MODE): HResult; stdcall;

    function GetDisplayAlignment(out displayAlign: MF_TIMED_TEXT_DISPLAY_ALIGNMENT): HResult; stdcall;

    function GetLineHeight(out pLineHeight: Double;
                           out unitType: MF_TIMED_TEXT_UNIT_TYPE): HResult; stdcall;

    function GetClipOverflow(out clipOverflow: BOOL): HResult; stdcall;

    function GetPadding(out before: Double;
                        out start: Double;
                        out after: Double;
                        out _end: Double;
                        out unitType: MF_TIMED_TEXT_UNIT_TYPE): HResult; stdcall;

    function GetWrap(out wrap: BOOL): HResult; stdcall;

    function GetZIndex(out zIndex: INT32): HResult; stdcall;

    function GetScrollMode(out scrollMode: MF_TIMED_TEXT_SCROLL_MODE): HResult; stdcall;

  end;
  IID_IMFTimedTextRegion = IMFTimedTextRegion;
  {$EXTERNALSYM IID_IMFTimedTextRegion}


  // Interface IMFTimedTextBinary
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextBinary);'}
  {$EXTERNALSYM IMFTimedTextBinary}
  IMFTimedTextBinary = interface(IUnknown)
  ['{4ae3a412-0545-43c4-bf6f-6b97a5c6c432}']

    function GetData(out data: PByte;
                     out length: DWORD): HResult; stdcall;

  end;
  IID_IMFTimedTextBinary = IMFTimedTextBinary;
  {$EXTERNALSYM IID_IMFTimedTextBinary}


  // Interface IMFTimedTextCueList
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTimedTextCueList);'}
  {$EXTERNALSYM IMFTimedTextCueList}
  IMFTimedTextCueList = interface(IUnknown)
  ['{ad128745-211b-40a0-9981-fe65f166d0fd}']
    function GetLength(): DWORD; stdcall;

    function GetCueByIndex(const index: DWORD;
                           out cue: IMFTimedTextCue): HResult; stdcall;

    function GetCueById(id: DWORD;
                        out cue: IMFTimedTextCue): HResult; stdcall;

    function GetCueByOriginalId(originalId: LPCWSTR;
                                out cue: IMFTimedTextCue): HResult; stdcall;

    function AddTextCue(start: Double;
                        duration: Double;
                        text: PWideChar;
                        out cue: IMFTimedTextCue): HResult; stdcall;

    function AddDataCue(start: Double;
                        duration: Double;
                        data: PByte;
                        dataSize: DWORD;
                        out cue: IMFTimedTextCue): HResult; stdcall;

    function RemoveCue(cue: IMFTimedTextCue): HResult; stdcall;

  end;
  IID_IMFTimedTextCueList = IMFTimedTextCueList;
  {$EXTERNALSYM IID_IMFTimedTextCueList}

  // End  TIMED TEXT



//#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD) (Win 8)

  //+-----------------------------------------------------------------------------
  //
  //  Enumeration:  MF_MEDIA_ENGINE_STREAMTYPE_FAILED
  //
  //  Synopsis:   Defines stream type that failed to render
  //
  //------------------------------------------------------------------------------

  PMF_MEDIA_ENGINE_STREAMTYPE_FAILED = ^MF_MEDIA_ENGINE_STREAMTYPE_FAILED;
  MF_MEDIA_ENGINE_STREAMTYPE_FAILED           = (
    MF_MEDIA_ENGINE_STREAMTYPE_FAILED_UNKNOWN = 0,
    MF_MEDIA_ENGINE_STREAMTYPE_FAILED_AUDIO   = 1,
    MF_MEDIA_ENGINE_STREAMTYPE_FAILED_VIDEO   = 2
  );
  {$EXTERNALSYM MF_MEDIA_ENGINE_STREAMTYPE_FAILED}

//#endif // (WINVER >= _WIN32_WINNT_WINTHRESHOLD) (Win 8)


  //////////////////////////////////////////////////////////////////////////////
  //
  //  Encrypted Media Extensions (EME 2) March 31,2015
  //
  //  See http://www.w3.org/TR/2015/WD-encrypted-media-20150331/
  //
  //////////////////////////////////////////////////////////////////////////////


  //+---------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaEngineEMENotify
  //
  //  Synopsis:   Implement EME2 Events for MediaEngine
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineEMENotify);'}
  {$EXTERNALSYM IMFMediaEngineEMENotify}
  IMFMediaEngineEMENotify = interface(IUnknown)
  ['{9e184d15-cdb7-4f86-b49e-566689f4a601}']

    procedure Encrypted(pbInitData: PByte;
                        cb: DWORD;
                        bstrInitDataType: BSTR); stdcall;

    procedure WaitingForKey(); stdcall;

  end;
  IID_IMFMediaEngineEMENotify = IMFMediaEngineEMENotify;
  {$EXTERNALSYM IID_IMFMediaEngineEMENotify}


  //+-----------------------------------------------------------------------------
  //
  //  Enumeration:  MF_MEDIA_KEYS_REQUIREMENT
  //
  //  Synopsis:   Defines requirement for DistinctiveId and PersistedState
  //
  //------------------------------------------------------------------------------
  PMF_MEDIAKEYS_REQUIREMENT = ^MF_MEDIAKEYS_REQUIREMENT;
  MF_MEDIAKEYS_REQUIREMENT               = (
    MF_MEDIAKEYS_REQUIREMENT_REQUIRED    = 1,
    MF_MEDIAKEYS_REQUIREMENT_OPTIONAL    = 2,
    MF_MEDIAKEYS_REQUIREMENT_NOT_ALLOWED = 3
  );
  {$EXTERNALSYM MF_MEDIAKEYS_REQUIREMENT}


  //+---------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaKeySessionNotify2
  //
  //  Synopsis:   Implement EME2 Events for MediaKeySession
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeySessionNotify2);'}
  {$EXTERNALSYM IMFMediaKeySessionNotify2}
  IMFMediaKeySessionNotify2 = interface(IMFMediaKeySessionNotify)
  ['{c3a9e92a-da88-46b0-a110-6cf953026cb9}']

    procedure KeyMessage2(eMessageType: MF_MEDIAKEYSESSION_MESSAGETYPE;
                          const destinationURL: BSTR;
                          pbMessage: PByte;
                          cbMessage: DWORD); stdcall;

    procedure KeyStatusChange(); stdcall;

  end;
  IID_IMFMediaKeySessionNotify2 = IMFMediaKeySessionNotify2;
  {$EXTERNALSYM IID_IMFMediaKeySessionNotify2}


  //+---------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaKeySystemAccess
  //
  //  Synopsis:   Implement return value of requestMediaKeySystemAccess for EME
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeySystemAccess);'}
  {$EXTERNALSYM IMFMediaKeySystemAccess}
  IMFMediaKeySystemAccess = interface(IUnknown)
  ['{aec63fda-7a97-4944-b35c-6c6df8085cc3}']

    function CreateMediaKeys(pCdmCustomConfig: IPropertyStore;
                             out ppKeys: IMFMediaKeys2): HResult; stdcall;

    function get_SupportedConfiguration(out ppSupportedConfiguration: IPropertyStore): HResult; stdcall;

    function get_KeySystem(out pKeySystem: BSTR): HResult; stdcall;

  end;
  IID_IMFMediaKeySystemAccess = IMFMediaKeySystemAccess;
  {$EXTERNALSYM IID_IMFMediaKeySystemAccess}


  //+---------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaEngineClassFactory3
  //
  //  Synopsis:   Implement requestMediaKeySystemAccess for EME
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineClassFactory3);'}
  {$EXTERNALSYM IMFMediaEngineClassFactory3}
  IMFMediaEngineClassFactory3 = interface(IUnknown)
  ['{3787614f-65f7-4003-b673-ead8293a0e60}']

    function CreateMediaKeySystemAccess(keySystem: BSTR;
                                        ppSupportedConfigurationsArray: IPropertyStore;
                                        uSize: UINT;
                                        out ppKeyAccess: IMFMediaKeySystemAccess): HResult; cdecl;

  end;
  IID_IMFMediaEngineClassFactory3 = IMFMediaEngineClassFactory3;
  {$EXTERNALSYM IID_IMFMediaEngineClassFactory3}


  //+---------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaKeys2
  //
  //  Synopsis:   Implement IMFMediaKeys2 for EME
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeys2);'}
  {$EXTERNALSYM IMFMediaKeys2}
  IMFMediaKeys2 = interface(IMFMediaKeys)
  ['{45892507-ad66-4de2-83a2-acbb13cd8d43}']

    function CreateSession2(eSessionType: MF_MEDIAKEYSESSION_TYPE;
                            pMFMediaKeySessionNotify2: IMFMediaKeySessionNotify2;
                            out ppSession: IMFMediaKeySession2): HResult; stdcall;

    function SetServerCertificate(pbServerCertificate: PByte;
                                  cb: DWORD): HResult; stdcall;

    function GetDOMException(systemCode: HResult;
                             out code: HResult): HResult; stdcall;

  end;
  IID_IMFMediaKeys2 = IMFMediaKeys2;
  {$EXTERNALSYM IID_IMFMediaKeys2}


  //+---------------------------------------------------------------------------
  //
  //  Interface:  IMFMediaKeySession2
  //
  //  Synopsis:   Implement IMFMediaKeySession2 for EME
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaKeySession2);'}
  {$EXTERNALSYM IMFMediaKeySession2}
  IMFMediaKeySession2 = interface(IMFMediaKeySession)
  ['{e9707e05-6d55-4636-b185-3de21210bd75}']

    function get_KeyStatuses(out pKeyStatusesArray: MFMediaKeyStatus;
                             out puSize: UINT): HResult; stdcall;

    function Load(bstrSessionId: BSTR;
                  out pfLoaded: BOOL): HResult; stdcall;

    function GenerateRequest(initDataType: BSTR;
                             pbInitData: PByte;
                             cb: DWORD): HResult; stdcall;

    function get_Expiration(out dblExpiration: Double): HResult; stdcall;

    function Remove(): HResult; stdcall;

    function Shutdown(): HResult; stdcall;

  end;
  IID_IMFMediaKeySession2 = IMFMediaKeySession2;
  {$EXTERNALSYM IID_IMFMediaKeySession2}


  //////////////////////////////////////////////////////////////////////////////
  //
  //  Store CDMs May 10, 2019
  //
  //  Allows direct usage of CDM objects that can be defined in store apps.
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface IMFMediaEngineClassFactory4
  //  Synopsis:   Implement direct access of CDM object that could be defined in store apps.
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEngineClassFactory4);'}
  {$EXTERNALSYM IMFMediaEngineClassFactory4}
  IMFMediaEngineClassFactory4 = interface(IUnknown)
  ['{fbe256c1-43cf-4a9b-8cb8-ce8632a34186}']
    function CreateContentDecryptionModuleFactory(keySystem: LPCWSTR;
                                                  const riid: REFIID;
                                                  out ppvObject: Pointer): HResult; stdcall;

  end;
  IID_IMFMediaEngineClassFactory4 = IMFMediaEngineClassFactory4;
  {$EXTERNALSYM IID_IMFMediaEngineClassFactory4}

// endif WINVER >= _WIN32_WINNT_WINTHRESHOLD


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

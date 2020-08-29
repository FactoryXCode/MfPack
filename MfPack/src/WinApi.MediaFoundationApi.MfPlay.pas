// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfPlay.pas
// Kind: Pascal / Delphi unit
// Release date: 28-06-2012
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: MFPlay is a Microsoft Media Foundation API for creating media
//              playback applications.
//              This API will be deprecated: See: https://docs.microsoft.com/en-us/windows/win32/api/mfplay/
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
// Remarks: Requires Windows Vista or higher.
//          Deprecated. This API may be removed from future releases of Windows.
//          Applications should use the Media Session for playback.
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
// Source: mfplay.h
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
unit WinApi.MediaFoundationApi.MfPlay;

  {$HPPEMIT '#include "mfplay.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.Evr;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  //////////////////////////////////////////////////////////////////////////////
  //
  // Position types that MFPlay uses.
  // See IMFPMediaItem.GetStartStopPosition and IMFPMediaItem.SetStartStopPosition.
  //
  MFP_POSITIONTYPE_100NS          : TGUID = '{00000000-0000-0000-0000-000000000000}';  // = GUID_NULL
  {$EXTERNALSYM MFP_POSITIONTYPE_100NS}

  // implementations of DEFINE_PROPERTYKEY macro, see comment: PropKeyDef.pas

  MFP_PKEY_StreamIndex            : PROPERTYKEY = (fmtid: (D1: $a7cf9740;
                                                   D2: $e8d9;
		                                               D3: $4a87;
                                                   D4: ($bd, $8e, $29, $67, $0, $1f, $d3, $ad));
                                                   pid: $0);
  {$EXTERNALSYM MFP_PKEY_StreamIndex}

  MFP_PKEY_StreamRenderingResults : PROPERTYKEY = (fmtid: (D1: $a7cf9740;
                                                   D2: $e8d9;
		                                               D3: $4a87;
                                                   D4: ($bd, $8e, $29, $67, $0, $1f, $d3, $ad));
                                                   pid: $01);
  {$EXTERNALSYM MFP_PKEY_StreamRenderingResults}



{$IFNDEF MFP_X_PTRS}
  // NOTES: A DWORD_PTR is NOT a pointer to DWord, but a NativeUInt (4 bytes) on a 32bit system or UInt64 (8 bytes) on a 64bit system.
  //        By default this type is defined in MfPack.MfpTypes
type
  DWORD_PTR = NativeUInt;
  {$EXTERNALSYM DWORD_PTR}
{$ENDIF}

{$IFNDEF MFP_SIZE}
type
  PSIZE = ^tagSIZE;
  tagSIZE = record
    cx: Longint;
    cy: Longint;
  end;
  {$EXTERNALSYM tagSIZE}
  SIZE = tagSIZE;
  {$EXTERNALSYM SIZE}
{$ENDIF}

{$IFNDEF MFP_COLORREF}
type
  COLORREF = DWORD;
  {$EXTERNALSYM COLORREF}
  LPCOLORREF = ^DWORD;
{$ENDIF}


type
  MFP_CREATION_OPTIONS = UINT32;
  {$EXTERNALSYM MFP_CREATION_OPTIONS}
const
  MFP_OPTION_NONE                             : MFP_CREATION_OPTIONS = 0;
  {$EXTERNALSYM MFP_OPTION_NONE}
  MFP_OPTION_FREE_THREADED_CALLBACK           : MFP_CREATION_OPTIONS = $1;
  {$EXTERNALSYM MFP_OPTION_FREE_THREADED_CALLBACK}
  MFP_OPTION_NO_MMCSS                         : MFP_CREATION_OPTIONS = $2;
  {$EXTERNALSYM MFP_OPTION_NO_MMCSS}
  MFP_OPTION_NO_REMOTE_DESKTOP_OPTIMIZATION   : MFP_CREATION_OPTIONS = $4;
  {$EXTERNALSYM MFP_OPTION_NO_REMOTE_DESKTOP_OPTIMIZATION}

type
  PMFP_MEDIAPLAYER_STATE = ^MFP_MEDIAPLAYER_STATE;
  MFP_MEDIAPLAYER_STATE            = (
    MFP_MEDIAPLAYER_STATE_EMPTY    = 0,
    MFP_MEDIAPLAYER_STATE_STOPPED  = $1,
    MFP_MEDIAPLAYER_STATE_PLAYING  = $2,
    MFP_MEDIAPLAYER_STATE_PAUSED   = $3,
    MFP_MEDIAPLAYER_STATE_SHUTDOWN = $4
  );
  {$EXTERNALSYM MFP_MEDIAPLAYER_STATE}


type
  MFP_MEDIAITEM_CHARACTERISTICS = UINT32;
  {$EXTERNALSYM MFP_MEDIAITEM_CHARACTERISTICS}
const
  MFP_MEDIAITEM_IS_LIVE       : MFP_MEDIAITEM_CHARACTERISTICS = $1;
  {$EXTERNALSYM MFP_MEDIAITEM_IS_LIVE}
  MFP_MEDIAITEM_CAN_SEEK      : MFP_MEDIAITEM_CHARACTERISTICS = $2;
  {$EXTERNALSYM MFP_MEDIAITEM_CAN_SEEK}
  MFP_MEDIAITEM_CAN_PAUSE     : MFP_MEDIAITEM_CHARACTERISTICS = $4;
  {$EXTERNALSYM MFP_MEDIAITEM_CAN_PAUSE}
  MFP_MEDIAITEM_HAS_SLOW_SEEK : MFP_MEDIAITEM_CHARACTERISTICS = $8;
  {$EXTERNALSYM MFP_MEDIAITEM_HAS_SLOW_SEEK}

type
  MFP_CREDENTIAL_FLAGS = UINT32;
  {$EXTERNALSYM MFP_CREDENTIAL_FLAGS}
const
  MFP_CREDENTIAL_PROMPT         : MFP_CREDENTIAL_FLAGS = $1;
  {$EXTERNALSYM MFP_CREDENTIAL_PROMPT}
  MFP_CREDENTIAL_SAVE           : MFP_CREDENTIAL_FLAGS = $2;
  {$EXTERNALSYM MFP_CREDENTIAL_SAVE}
  MFP_CREDENTIAL_DO_NOT_CACHE   : MFP_CREDENTIAL_FLAGS = $4;
  {$EXTERNALSYM MFP_CREDENTIAL_DO_NOT_CACHE}
  MFP_CREDENTIAL_CLEAR_TEXT     : MFP_CREDENTIAL_FLAGS = $8;
  {$EXTERNALSYM MFP_CREDENTIAL_CLEAR_TEXT}
  MFP_CREDENTIAL_PROXY          : MFP_CREDENTIAL_FLAGS = $10;
  {$EXTERNALSYM MFP_CREDENTIAL_PROXY}
  MFP_CREDENTIAL_LOGGED_ON_USER : MFP_CREDENTIAL_FLAGS = $20;
  {$EXTERNALSYM MFP_CREDENTIAL_LOGGED_ON_USER}

type

  // Forward Interface Declarations

  PIMFPMediaPlayer = ^IMFPMediaPlayer;
  IMFPMediaPlayer = interface;

  PIMFPMediaPlayerCallback = ^IMFPMediaPlayerCallback;
  IMFPMediaPlayerCallback = interface;

  PIMFPMediaItem = ^IMFPMediaItem;
  IMFPMediaItem = interface;


  PMFP_EVENT_TYPE = ^MFP_EVENT_TYPE;
  MFP_EVENT_TYPE = (
    MFP_EVENT_TYPE_PLAY                    = 0,
    MFP_EVENT_TYPE_PAUSE                   = 1,
    MFP_EVENT_TYPE_STOP                    = 2,
    MFP_EVENT_TYPE_POSITION_SET            = 3,
    MFP_EVENT_TYPE_RATE_SET                = 4,
    MFP_EVENT_TYPE_MEDIAITEM_CREATED       = 5,
    MFP_EVENT_TYPE_MEDIAITEM_SET           = 6,
    MFP_EVENT_TYPE_FRAME_STEP              = 7,
    MFP_EVENT_TYPE_MEDIAITEM_CLEARED       = 8,
    MFP_EVENT_TYPE_MF                      = 9,
    MFP_EVENT_TYPE_ERROR                   = 10,
    MFP_EVENT_TYPE_PLAYBACK_ENDED          = 11,
    MFP_EVENT_TYPE_ACQUIRE_USER_CREDENTIAL = 12
  );
  {$EXTERNALSYM MFP_EVENT_TYPE}


  PMFP_EVENT_HEADER = ^MFP_EVENT_HEADER;
  MFP_EVENT_HEADER = record
    eEventType: MFP_EVENT_TYPE;
    hrEvent: HResult;
    pMediaPlayer: IMFPMediaPlayer;
    eState: MFP_MEDIAPLAYER_STATE;
    pPropertyStore: IPropertyStore;
  end;
  {$EXTERNALSYM MFP_EVENT_HEADER}


  PMFP_PLAY_EVENT = ^MFP_PLAY_EVENT;
  MFP_PLAY_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_PLAY_EVENT}


  PMFP_PAUSE_EVENT = ^MFP_PAUSE_EVENT;
  cwMFP_PAUSE_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM cwMFP_PAUSE_EVENT}
  MFP_PAUSE_EVENT = cwMFP_PAUSE_EVENT;
  {$EXTERNALSYM MFP_PAUSE_EVENT}


  PMFP_STOP_EVENT = ^MFP_STOP_EVENT;
  MFP_STOP_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_STOP_EVENT}


  PMFP_POSITION_SET_EVENT = ^MFP_POSITION_SET_EVENT;
  MFP_POSITION_SET_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_POSITION_SET_EVENT}


  PMFP_RATE_SET_EVENT = ^MFP_RATE_SET_EVENT;
  MFP_RATE_SET_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
    flRate: Single;
  end;
  {$EXTERNALSYM MFP_RATE_SET_EVENT}


  PMFP_MEDIAITEM_CREATED_EVENT = ^MFP_MEDIAITEM_CREATED_EVENT;
  MFP_MEDIAITEM_CREATED_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
    dwUserData: DWORD_PTR;
  end;
  {$EXTERNALSYM MFP_MEDIAITEM_CREATED_EVENT}


  PMFP_MEDIAITEM_SET_EVENT = ^MFP_MEDIAITEM_SET_EVENT;
  MFP_MEDIAITEM_SET_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_MEDIAITEM_SET_EVENT}


  PMFP_FRAME_STEP_EVENT = ^MFP_FRAME_STEP_EVENT;
  MFP_FRAME_STEP_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_FRAME_STEP_EVENT}


  PMFP_MEDIAITEM_CLEARED_EVENT = ^MFP_MEDIAITEM_CLEARED_EVENT;
  MFP_MEDIAITEM_CLEARED_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_MEDIAITEM_CLEARED_EVENT}


  PMFP_MF_EVENT = ^MFP_MF_EVENT;
  MFP_MF_EVENT = record
    header: MFP_EVENT_HEADER;
    MFEventType: MediaEventType;
    pMFMediaEvent: IMFMediaEvent;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_MF_EVENT}


  PMFP_ERROR_EVENT = ^MFP_ERROR_EVENT;
  MFP_ERROR_EVENT = record
    header: MFP_EVENT_HEADER;
  end;
  {$EXTERNALSYM MFP_ERROR_EVENT}


  PMFP_PLAYBACK_ENDED_EVENT = ^MFP_PLAYBACK_ENDED_EVENT;
  MFP_PLAYBACK_ENDED_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: IMFPMediaItem;
  end;
  {$EXTERNALSYM MFP_PLAYBACK_ENDED_EVENT}


  PMFP_ACQUIRE_USER_CREDENTIAL_EVENT = ^MFP_ACQUIRE_USER_CREDENTIAL_EVENT;
  MFP_ACQUIRE_USER_CREDENTIAL_EVENT = record
    header: MFP_EVENT_HEADER;
    dwUserData: DWORD_PTR;
    fProceedWithAuthentication: BOOL;
    hrAuthenticationStatus: HResult;
    pwszURL: PWideChar;
    pwszSite: PWideChar;
    pwszRealm: PWideChar;
    pwszPackage: PWideChar;
    nRetries: LONG;
    flags: MFP_CREDENTIAL_FLAGS;
    pCredential: IMFNetCredential;
  end;
  {$EXTERNALSYM MFP_ACQUIRE_USER_CREDENTIAL_EVENT}


  // Interface IMFPMediaPlayer
  // =========================
  {
   NOTE: Deprecated.
   This API may be removed from future releases of Windows (> Windows 7).
   Applications should use the Media Session for playback.
   Contains methods to play media files.
   The MFPlay player object exposes this interface.
   To get a pointer to this interface, call function MFPCreateMediaPlayer.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMediaPlayer);'}
  {$EXTERNALSYM IMFPMediaPlayer}
  IMFPMediaPlayer = interface(IUnknown)
	['{A714590A-58AF-430a-85BF-44F5EC838D85}']

    function Play(): HResult; stdcall;

    function Pause(): HResult; stdcall;

    function Stop(): HResult; stdcall;

    function FrameStep(): HResult; stdcall;

    //
    // Position controls
    //

    function SetPosition(guidPositionType: REFGUID;
                         pvPositionValue: PROPVARIANT): HResult; stdcall;

    function GetPosition(guidPositionType: REFGUID;
                         out pvPositionValue: PROPVARIANT): HResult; stdcall;

    function GetDuration(guidPositionType: REFGUID;
                         out pvDurationValue: PROPVARIANT): HResult; stdcall;

    //
    // Rate Control
    //

    function SetRate(flRate: Single): HResult; stdcall;

    function GetRate(out pflRate: Single): HResult; stdcall;

    function GetSupportedRates(fForwardDirection: Boolean;
                               out pflSlowestRate: Single;
                               out pflFastestRate: Single): HResult; stdcall;

    //
    // State
    //

    function GetState(out peState: MFP_MEDIAPLAYER_STATE): HResult; stdcall;

    //
    // Media Item Management
    //

    function CreateMediaItemFromURL(pwszURL: LPCWSTR;
                                    fSync: BOOL;
                                    dwUserData: DWORD_PTR;
                                    ppMediaItem: PIMFPMediaItem): HResult; stdcall;

    function CreateMediaItemFromObject(pIUnknownObj: IUnknown;
                                       fSync: BOOL;
                                       dwUserData: DWORD_PTR;
                                       ppMediaItem: PIMFPMediaItem): HResult; stdcall;

    function SetMediaItem(pIMFPMediaItem: IMFPMediaItem): HResult; stdcall;

    function ClearMediaItem(): HResult; stdcall; // This method is currently not implemented.

    function GetMediaItem(out ppIMFPMediaItem: IMFPMediaItem): HResult; stdcall;

    //
    // Audio controls
    //

    function GetVolume(out pflVolume: FLOAT): HResult; stdcall;

    function SetVolume(const flVolume: FLOAT): HResult; stdcall;

    function GetBalance(out pflBalance: FLOAT): HResult; stdcall;

    function SetBalance(const flBalance: FLOAT): HResult; stdcall;

    function GetMute(out pfMute: BOOL): HResult; stdcall; //  4 bytes, BOOL;

    function SetMute(fMute: BOOL): HResult; stdcall; // 4 bytes, BOOL;

    //
    // Video controls
    //

    function GetNativeVideoSize(out pszVideo: SIZE;
                                out pszARVideo: SIZE): HResult; stdcall;

    function GetIdealVideoSize(out pszMin: SIZE;
                               out pszMax: SIZE): HResult; stdcall;

    function SetVideoSourceRect(pnrcSource: MFVideoNormalizedRect): HResult; stdcall;

    function GetVideoSourceRect(out pnrcSource: MFVideoNormalizedRect): HResult; stdcall;

    function SetAspectRatioMode(dwAspectRatioMode: DWord): HResult; stdcall;

    function GetAspectRatioMode(out pdwAspectRatioMode: DWord): HResult; stdcall;

    function GetVideoWindow(out phwndVideo: HWND): HResult; stdcall;

    function UpdateVideo(): HResult; stdcall;

    function SetBorderColor(const Clr: COLORREF): HResult; stdcall;

    function GetBorderColor(out pClr: COLORREF): HResult; stdcall;

    //
    // Effect Management
    //

    function InsertEffect(pEffect: IUnknown;
                          fOptional: Boolean): HResult; stdcall;

    function RemoveEffect(pEffect: IUnknown): HResult; stdcall;

    function RemoveAllEffects(): HResult; stdcall;

    //
    // Shutdown
    //

    function Shutdown(): HResult; stdcall;

  end;
  IID_IMFPMediaPlayer = IMFPMediaPlayer;
  {$EXTERNALSYM IID_IMFPMediaPlayer}


  // Interface IMFPMediaItem
  // =======================
  {
   NOTE:  Deprecated.
   This API may be removed from future releases of Windows.
   Applications should use the Media Session for playback.
   Represents a media item. A media item is an abstraction for a source of media data,
   such as a video file.
   Use this interface to get information about the source, or to change certain playback settings,
   such as the start and stop times.
   To get a pointer to this interface, call one of the following methods:
      IMFPMediaPlayer.CreateMediaItemFromObject
      IMFPMediaPlayer.CreateMediaItemFromURL
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMediaItem);'}
  {$EXTERNALSYM IMFPMediaItem}
  IMFPMediaItem = interface(IUnknown)
	['{90EB3E6B-ECBF-45cc-B1DA-C6FE3EA70D57}']

    function GetMediaPlayer(out ppMediaPlayer: IMFPMediaPlayer): HResult; stdcall;

    function GetURL(out ppwszURL: LPWSTR): HResult; stdcall;

    function GetObject(out ppIUnknown: IUnknown): HResult; stdcall;

    function GetUserData(out pdwUserData: DWORD_PTR): HResult; stdcall;

    function SetUserData(const dwUserData: DWORD_PTR): HResult; stdcall;

    function GetStartStopPosition(out pguidStartPositionType: TGuid;
                                  out pvStartValue: PROPVARIANT;
                                  out pguidStopPositionType: TGuid;
                                  out pvStopValue: PROPVARIANT): HResult; stdcall;

    function SetStartStopPosition(pguidStartPositionType: TGuid;
                                  pvStartValue: PROPVARIANT;
                                  pguidStopPositionType: TGuid;
                                  pvStopValue: PROPVARIANT): HResult; stdcall;

    function HasVideo(out pfHasVideo: BOOL;
                      out pfSelected: BOOL): HResult; stdcall;

    function HasAudio(out pfHasAudio: BOOL;
                      out pfSelected: BOOL): HResult; stdcall;

    function IsProtected(out pfProtected: BOOL): HResult; stdcall;

    function GetDuration(guidPositionType: REFGUID;
                         out pvDurationValue: PROPVARIANT): HResult; stdcall;

    function GetNumberOfStreams(out pdwStreamCount: DWord): HResult; stdcall;

    function GetStreamSelection(dwStreamIndex: DWord;
                                out pfEnabled: BOOL): HResult; stdcall;

    function SetStreamSelection(dwStreamIndex: DWord;
                                fEnabled: BOOL): HResult; stdcall;

    function GetStreamAttribute(const dwStreamIndex: DWord;
                                guidMFAttribute: REFGUID;
                                out pvValue: PROPVARIANT): HResult; stdcall;

    function GetPresentationAttribute(const guidMFAttribute: REFGUID;
                                      out pvValue: PROPVARIANT): HResult; stdcall;

    function GetCharacteristics(out pCharacteristics: MFP_MEDIAITEM_CHARACTERISTICS): HResult; stdcall;

    function SetStreamSink(dwStreamIndex: DWord;
                           pMediaSink: IUnknown): HResult; stdcall;

    function GetMetadata(out ppMetadataStore: IPropertyStore): HResult; stdcall;

  end;
  IID_IMFPMediaItem = IMFPMediaItem;
  {$EXTERNALSYM IID_IMFPMediaItem}


  // Interface IMFPMediaPlayerCallback
  // =================================
  {
   NOTE: Deprecated.
   This API may be removed from future releases of Windows (Windows 7).
   Applications should use the Media Session for playback.
   Callback interface for the IMFPMediaPlayer interface.
   To set the callback, pass an IMFPMediaPlayerCallback pointer to the MFPCreateMediaPlayer function in the pCallback parameter.
   The application implements the IMFPMediaPlayerCallback interface.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPMediaPlayerCallback);'}
  {$EXTERNALSYM IMFPMediaPlayerCallback}
  IMFPMediaPlayerCallback = interface(IUnknown)
    ['{766C8FFB-5FDB-4fea-A28D-B912996F51BD}']

      procedure OnMediaPlayerEvent(var pEventHeader: MFP_EVENT_HEADER); stdcall;

  end;
  IID_IMFPMediaPlayerCallback = IMFPMediaPlayerCallback;
  {$EXTERNALSYM IID_IMFPMediaPlayerCallback}


  {
   NOTE: Deprecated.
   This API may be removed from future releases of Windows.
   Applications should use the Media Session for playback.
   Creates a new instance of the MFPlay player object.
  }
  function MFPCreateMediaPlayer(const pwszURL: LPCWSTR;
                                fStartPlayback: BOOL;
                                creationOptions: MFP_CREATION_OPTIONS;
                                pCallback: IMFPMediaPlayerCallback;
                                hWnd: HWND;
                                out ppMediaPlayer: IMFPMediaPlayer): HResult; stdcall; deprecated;
  {$EXTERNALSYM MFPCreateMediaPlayer}


  // CONVERTED MACRO'S
  // NOTE: These will be deprecated: See: https://docs.microsoft.com/en-us/windows/win32/api/mfplay/
  //////////////////////////////////////////////////////////////////////////////
  /// <summary>
  ///     Macros to cast a pointer to a MFP_EVENT_HEADER structure into a pointer to a MFP_*_EVENT.
  ///     If the event is not of the correct type, the macro returns a Nil pointer.
  /// </summary>
  function MFP_GET_PLAY_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_PLAY_EVENT;
  {$EXTERNALSYM MFP_GET_PLAY_EVENT}
  function MFP_GET_PAUSE_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_PAUSE_EVENT;
  {$EXTERNALSYM MFP_GET_PAUSE_EVENT}
  function MFP_GET_STOP_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_STOP_EVENT;
  {$EXTERNALSYM MFP_GET_STOP_EVENT}
  function MFP_GET_POSITION_SET_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_POSITION_SET_EVENT;
  {$EXTERNALSYM MFP_GET_POSITION_SET_EVENT}
  function MFP_GET_RATE_SET_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_RATE_SET_EVENT;
  {$EXTERNALSYM MFP_GET_RATE_SET_EVENT}
  function MFP_GET_MEDIAITEM_CREATED_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MEDIAITEM_CREATED_EVENT;
  {$EXTERNALSYM MFP_GET_MEDIAITEM_CREATED_EVENT}
  function MFP_GET_MEDIAITEM_SET_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MEDIAITEM_SET_EVENT;
  {$EXTERNALSYM MFP_GET_MEDIAITEM_SET_EVENT}
  function MFP_GET_FRAME_STEP_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_FRAME_STEP_EVENT;
  {$EXTERNALSYM MFP_GET_FRAME_STEP_EVENT}
  function MFP_GET_MEDIAITEM_CLEARED_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MEDIAITEM_CLEARED_EVENT;
  {$EXTERNALSYM MFP_GET_MEDIAITEM_CLEARED_EVENT}
  function MFP_GET_MF_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MF_EVENT;
  {$EXTERNALSYM MFP_GET_MF_EVENT}
  function MFP_GET_ERROR_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_ERROR_EVENT;
  {$EXTERNALSYM MFP_GET_ERROR_EVENT}
  function MFP_GET_PLAYBACK_ENDED_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_PLAYBACK_ENDED_EVENT;
  {$EXTERNALSYM MFP_GET_PLAYBACK_ENDED_EVENT}
  function MFP_GET_ACQUIRE_USER_CREDENTIAL_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_ACQUIRE_USER_CREDENTIAL_EVENT;
  {$EXTERNALSYM MFP_GET_ACQUIRE_USER_CREDENTIAL_EVENT}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation

const
  MfPlayLib = 'MFPlay.dll';


function MFP_GET_PLAY_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_PLAY_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_PLAY then
    Result := PMFP_PLAY_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_PAUSE_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_PAUSE_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_PAUSE then
    Result := PMFP_PAUSE_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_STOP_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_STOP_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_STOP then
    Result := PMFP_STOP_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_POSITION_SET_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_POSITION_SET_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_POSITION_SET then
    Result := PMFP_POSITION_SET_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_RATE_SET_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_RATE_SET_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_RATE_SET then
    Result := PMFP_RATE_SET_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_MEDIAITEM_CREATED_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MEDIAITEM_CREATED_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_MEDIAITEM_CREATED then
    Result := PMFP_MEDIAITEM_CREATED_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_MEDIAITEM_SET_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MEDIAITEM_SET_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_MEDIAITEM_SET then
    Result := PMFP_MEDIAITEM_SET_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_FRAME_STEP_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_FRAME_STEP_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_FRAME_STEP then
    Result := PMFP_FRAME_STEP_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_MEDIAITEM_CLEARED_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MEDIAITEM_CLEARED_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_MEDIAITEM_CLEARED then
    Result := PMFP_MEDIAITEM_CLEARED_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_MF_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_MF_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_MF then
    Result := PMFP_MF_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_ERROR_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_ERROR_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_ERROR then
    Result := PMFP_ERROR_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_PLAYBACK_ENDED_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_PLAYBACK_ENDED_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_PLAYBACK_ENDED then
    Result := PMFP_PLAYBACK_ENDED_EVENT(pHdr)
  else
    Result := Nil;
end;

function MFP_GET_ACQUIRE_USER_CREDENTIAL_EVENT(const pHdr: PMFP_EVENT_HEADER): PMFP_ACQUIRE_USER_CREDENTIAL_EVENT;
begin
  if pHdr.eEventType = MFP_EVENT_TYPE_ACQUIRE_USER_CREDENTIAL then
    Result := PMFP_ACQUIRE_USER_CREDENTIAL_EVENT(pHdr)
  else
    Result := Nil;
end;

{$WARN SYMBOL_PLATFORM OFF}
  function MFPCreateMediaPlayer; external MfPlayLib name 'MFPCreateMediaPlayer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.VfW.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Video for windows include file for WIN32.
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
// Remarks:
//
//      This include files defines interfaces to the following
//      video components
//
//          COMPMAN         - Installable Compression Manager.
//          DRAWDIB         - Routines for drawing to the display.
//          VIDEO           - Video Capture Driver Interface
//
//          AVIFMT          - AVI File Format structure definitions.
//          MMREG           - FOURCC and other things
//
//          AVIFile         - Interface for reading AVI Files and AVI Streams
//          MCIWND          - MCI/AVI window class
//          AVICAP          - AVI Capture Window class
//
//          MSACM           - Audio compression manager.
//
//      The following symbols control inclusion of various parts of this file:
//
//          NOCOMPMAN       - dont include COMPMAN
//          NODRAWDIB       - dont include DRAWDIB
//          NOVIDEO         - dont include video capture interface
//
//          NOAVIFMT        - dont include AVI file format structs
//          NOMMREG         - dont include MMREG
//
//          NOAVIFILE       - dont include AVIFile interface
//          NOMCIWND        - dont include AVIWnd class.
//          NOAVICAP        - dont include AVICap class.
//
//          NOMSACM         - dont include ACM stuff.
//
// -----------------------------------------------------------------------------
//  Attention!
//    The vfw.h header defines a number of functions and structs that are aliases to
//    ANSI or Unicode versions based on the definition of the UNICODE preprocessor constant.
//    Mixing usage of the encoding-neutral alias with code that not encoding-neutral
//    can lead to mismatches that result in compilation or runtime errors.
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
// Source: vfw.h
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
unit WinApi.WinMM.VfW;

interface
uses
  WinApi.Windows,
  WinApi.Messages,
  WinApi.CommDlg,
  Vcl.Dialogs,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMeApi;


  //****************************************************************************
  //
  //  types
  //
  //****************************************************************************

type
  TWOCC = type WORD;
  {$EXTERNALSYM TWOCC}

  HIC = type THandle;     // Handle to a Installable Compressor
  {$EXTERNALSYM HIC}

  //****************************************************************************
  //
  //  VideoForWindowsVersion() - returns version of VfW
  //
  //****************************************************************************

  function VideoForWindowsVersion(): DWord; pascal;
  {$EXTERNALSYM VideoForWindowsVersion}

  //****************************************************************************
  //
  //  call these to start stop using VfW from your app.
  //
  //****************************************************************************
  //  Old obsolete implementations (VFW 16-bit).
  //  For some odd reason these are still defined in the header file.
  //
  //  function InitVFW(): LONG; stdcall;
  //
  //  function TermVFW(): LONG; stdcall;
  //
  //////////////////////////////////////////////////////////////////////////////

  //***************************************************************************
  //
  //  do we need MMSYSTEM?
  //
  //***************************************************************************


  //****************************************************************************
  //
  //        Macros
  //
  //  should we define this??
  //  No we don't, but just include it here (see FCC functions in .MMReg.pas)
  //****************************************************************************

{$IFNDEF MAKEFOURCC}
  function MAKEFOURCC(const ch0: AnsiChar;
                      const ch1: AnsiChar;
                      const ch2: AnsiChar;
                      const ch3: AnsiChar): FOURCC; inline;
{$DEFINE MAKEFOURCC}
{$ENDIF}


  //****************************************************************************
  //
  //  COMPMAN - Installable Compression Manager.
  //
  //****************************************************************************

const

  ICVERSION                           = $0104;
  {$EXTERNALSYM ICVERSION}


  //
  // this code in biCompression means the DIB must be accesed via
  // 48 bit pointers! using *ONLY* the selector given.
  //
const
  BI_1632                             = $32333631;  // '1632'
  {$EXTERNALSYM BI_1632}


{$IFNDEF mmioFOURCC}
  function mmioFOURCC(ch1: AnsiChar;
                      ch2: AnsiChar;
                      ch3: AnsiChar;
                      ch4: AnsiChar): FOURCC;

{$DEFINE mmioFOURCC}
{$ENDIF}


  function aviTWOCC(ch0: AnsiChar;
                    ch1: AnsiChar): TWOCC;
  {$EXTERNALSYM aviTWOCC}

const

  ICTYPE_VIDEO = ord('v') or (ord('i') shl 8) or (ord('d') shl 16) or (ord('c') shl 24); {vidc}
  {$EXTERNALSYM ICTYPE_VIDEO}
  ICTYPE_AUDIO = ord('a') or (ord('u') shl 8) or (ord('d') shl 16) or (ord('c') shl 24); {audc}
  {$EXTERNALSYM ICTYPE_AUDIO}

  ICERR_OK                            = 0;
  {$EXTERNALSYM ICERR_OK}
  ICERR_DONTDRAW                      = 1;
  {$EXTERNALSYM ICERR_DONTDRAW}
  ICERR_NEWPALETTE                    = 2;
  {$EXTERNALSYM ICERR_NEWPALETTE}
  ICERR_GOTOKEYFRAME                  = 3;
  {$EXTERNALSYM ICERR_GOTOKEYFRAME}
  ICERR_STOPDRAWING                   = 4;
  {$EXTERNALSYM ICERR_STOPDRAWING}

  ICERR_UNSUPPORTED                   = - 1;
  {$EXTERNALSYM ICERR_UNSUPPORTED}
  ICERR_BADFORMAT                     = - 2;
  {$EXTERNALSYM ICERR_BADFORMAT}
  ICERR_MEMORY                        = - 3;
  {$EXTERNALSYM ICERR_MEMORY}
  ICERR_INTERNAL                      = - 4;
  {$EXTERNALSYM ICERR_INTERNAL}
  ICERR_BADFLAGS                      = - 5;
  {$EXTERNALSYM ICERR_BADFLAGS}
  ICERR_BADPARAM                      = - 6;
  {$EXTERNALSYM ICERR_BADPARAM}
  ICERR_BADSIZE                       = - 7;
  {$EXTERNALSYM ICERR_BADSIZE}
  ICERR_BADHANDLE                     = - 8;
  {$EXTERNALSYM ICERR_BADHANDLE}
  ICERR_CANTUPDATE                    = - 9;
  {$EXTERNALSYM ICERR_CANTUPDATE}
  ICERR_ABORT                         = - 10;
  {$EXTERNALSYM ICERR_ABORT}
  ICERR_ERROR                         = - 100;
  {$EXTERNALSYM ICERR_ERROR}
  ICERR_BADBITDEPTH                   = - 200;
  {$EXTERNALSYM ICERR_BADBITDEPTH}
  ICERR_BADIMAGESIZE                  = - 201;
  {$EXTERNALSYM ICERR_BADIMAGESIZE}

  ICERR_CUSTOM                        = - 400;  // errors less than ICERR_CUSTOM...
  {$EXTERNALSYM ICERR_CUSTOM}

  // Values for dwFlags of ICOpen()

  ICMODE_COMPRESS                     = 1;
  {$EXTERNALSYM ICMODE_COMPRESS}
  ICMODE_DECOMPRESS                   = 2;
  {$EXTERNALSYM ICMODE_DECOMPRESS}
  ICMODE_FASTDECOMPRESS               = 3;
  {$EXTERNALSYM ICMODE_FASTDECOMPRESS}
  ICMODE_QUERY                        = 4;
  {$EXTERNALSYM ICMODE_QUERY}
  ICMODE_FASTCOMPRESS                 = 5;
  {$EXTERNALSYM ICMODE_FASTCOMPRESS}
  ICMODE_DRAW                         = 8;
  {$EXTERNALSYM ICMODE_DRAW}

  ICMODE_INTERNALF_FUNCTION32         = $8000;  // ;Internal
  {$EXTERNALSYM ICMODE_INTERNALF_FUNCTION32}
  ICMODE_INTERNALF_MASK               = $8000;  // ;Internal
  {$EXTERNALSYM ICMODE_INTERNALF_MASK}

  // quality flags
  ICQUALITY_LOW                       = 0;
  {$EXTERNALSYM ICQUALITY_LOW}
  ICQUALITY_HIGH                      = 10000;
  {$EXTERNALSYM ICQUALITY_HIGH}
  ICQUALITY_DEFAULT                   = - 1;
  {$EXTERNALSYM ICQUALITY_DEFAULT}

  //************************************************************************
  //************************************************************************

  ICM_USER                            = (DRV_USER + $0000);
  {$EXTERNALSYM ICM_USER}

  ICM_RESERVED_LOW                    = (DRV_USER + $1000);
  {$EXTERNALSYM ICM_RESERVED_LOW}
  ICM_RESERVED_HIGH                   = (DRV_USER + $2000);
  {$EXTERNALSYM ICM_RESERVED_HIGH}

  ICM_RESERVED                        = ICM_RESERVED_LOW;
  {$EXTERNALSYM ICM_RESERVED}

  //************************************************************************
  //
  //  messages.
  //
  //************************************************************************

  ICM_GETSTATE                        = (ICM_RESERVED + 0);  // Get compressor state
  {$EXTERNALSYM ICM_GETSTATE}
  ICM_SETSTATE                        = (ICM_RESERVED + 1);  // Set compressor state
  {$EXTERNALSYM ICM_SETSTATE}
  ICM_GETINFO                         = (ICM_RESERVED + 2);  // Query info about the compressor
  {$EXTERNALSYM ICM_GETINFO}

  ICM_CONFIGURE                       = (ICM_RESERVED + 10);  // show the configure dialog
  {$EXTERNALSYM ICM_CONFIGURE}
  ICM_ABOUT                           = (ICM_RESERVED + 11);  // show the about box
  {$EXTERNALSYM ICM_ABOUT}

  ICM_GETERRORTEXT                    = (ICM_RESERVED + 12);  // get error text TBD ;Internal
  {$EXTERNALSYM ICM_GETERRORTEXT}
  ICM_GETFORMATNAME                   = (ICM_RESERVED + 20);  // get a name for a format ;Internal
  {$EXTERNALSYM ICM_GETFORMATNAME}
  ICM_ENUMFORMATS                     = (ICM_RESERVED + 21);  // cycle through formats ;Internal
  {$EXTERNALSYM ICM_ENUMFORMATS}

  ICM_GETDEFAULTQUALITY               = (ICM_RESERVED + 30);  // get the default value for quality
  {$EXTERNALSYM ICM_GETDEFAULTQUALITY}
  ICM_GETQUALITY                      = (ICM_RESERVED + 31);  // get the current value for quality
  {$EXTERNALSYM ICM_GETQUALITY}
  ICM_SETQUALITY                      = (ICM_RESERVED + 32);  // set the default value for quality
  {$EXTERNALSYM ICM_SETQUALITY}

  ICM_SET                             = (ICM_RESERVED + 40);  // Tell the driver something
  {$EXTERNALSYM ICM_SET}
  ICM_GET                             = (ICM_RESERVED + 41);  // Ask the driver something
  {$EXTERNALSYM ICM_GET}

  // Constants for ICM_SET:
  ICM_FRAMERATE    = ord('F') or (ord('r') shl 8) or (ord('m') shl 16) or (ord('R') shl 24); {FrmR}  // mmioFOURCC('F','r','m','R')
  {$EXTERNALSYM ICM_FRAMERATE}
  ICM_KEYFRAMERATE = ord('K') or (ord('e') shl 8) or (ord('y') shl 16) or (ord('R') shl 24); {KeyR}  // mmioFOURCC('K','e','y','R')
  {$EXTERNALSYM ICM_KEYFRAMERATE}


  //************************************************************************
  //
  //  ICM specific messages.
  //
  //************************************************************************

  ICM_COMPRESS_GET_FORMAT             = (ICM_USER + 4);  // get compress format or size
  {$EXTERNALSYM ICM_COMPRESS_GET_FORMAT}
  ICM_COMPRESS_GET_SIZE               = (ICM_USER + 5);  // get output size
  {$EXTERNALSYM ICM_COMPRESS_GET_SIZE}
  ICM_COMPRESS_QUERY                  = (ICM_USER + 6);  // query support for compress
  {$EXTERNALSYM ICM_COMPRESS_QUERY}
  ICM_COMPRESS_BEGIN                  = (ICM_USER + 7);  // begin a series of compress calls.
  {$EXTERNALSYM ICM_COMPRESS_BEGIN}
  ICM_COMPRESS                        = (ICM_USER + 8);  // compress a frame
  {$EXTERNALSYM ICM_COMPRESS}
  ICM_COMPRESS_END                    = (ICM_USER + 9);  // end of a series of compress calls.
  {$EXTERNALSYM ICM_COMPRESS_END}

  ICM_DECOMPRESS_GET_FORMAT           = (ICM_USER + 10);  // get decompress format or size
  {$EXTERNALSYM ICM_DECOMPRESS_GET_FORMAT}
  ICM_DECOMPRESS_QUERY                = (ICM_USER + 11);  // query support for dempress
  {$EXTERNALSYM ICM_DECOMPRESS_QUERY}
  ICM_DECOMPRESS_BEGIN                = (ICM_USER + 12);  // start a series of decompress calls
  {$EXTERNALSYM ICM_DECOMPRESS_BEGIN}
  ICM_DECOMPRESS                      = (ICM_USER + 13);  // decompress a frame
  {$EXTERNALSYM ICM_DECOMPRESS}
  ICM_DECOMPRESS_END                  = (ICM_USER + 14);  // end a series of decompress calls
  {$EXTERNALSYM ICM_DECOMPRESS_END}
  ICM_DECOMPRESS_SET_PALETTE          = (ICM_USER + 29);  // fill in the DIB color table
  {$EXTERNALSYM ICM_DECOMPRESS_SET_PALETTE}
  ICM_DECOMPRESS_GET_PALETTE          = (ICM_USER + 30);  // fill in the DIB color table
  {$EXTERNALSYM ICM_DECOMPRESS_GET_PALETTE}

  ICM_DRAW_QUERY                      = (ICM_USER + 31);  // query support for dempress
  {$EXTERNALSYM ICM_DRAW_QUERY}
  ICM_DRAW_BEGIN                      = (ICM_USER + 15);  // start a series of draw calls
  {$EXTERNALSYM ICM_DRAW_BEGIN}
  ICM_DRAW_GET_PALETTE                = (ICM_USER + 16);  // get the palette needed for drawing
  {$EXTERNALSYM ICM_DRAW_GET_PALETTE}
  ICM_DRAW_UPDATE                     = (ICM_USER + 17);  // update screen with current frame ;Internal
  {$EXTERNALSYM ICM_DRAW_UPDATE}
  ICM_DRAW_START                      = (ICM_USER + 18);  // start decompress clock
  {$EXTERNALSYM ICM_DRAW_START}
  ICM_DRAW_STOP                       = (ICM_USER + 19);  // stop decompress clock
  {$EXTERNALSYM ICM_DRAW_STOP}
  ICM_DRAW_BITS                       = (ICM_USER + 20);  // decompress a frame to screen ;Internal
  {$EXTERNALSYM ICM_DRAW_BITS}
  ICM_DRAW_END                        = (ICM_USER + 21);  // end a series of draw calls
  {$EXTERNALSYM ICM_DRAW_END}
  ICM_DRAW_GETTIME                    = (ICM_USER + 32);  // get value of decompress clock
  {$EXTERNALSYM ICM_DRAW_GETTIME}
  ICM_DRAW                            = (ICM_USER + 33);  // generalized "render" message
  {$EXTERNALSYM ICM_DRAW}
  ICM_DRAW_WINDOW                     = (ICM_USER + 34);  // drawing window has moved or hidden
  {$EXTERNALSYM ICM_DRAW_WINDOW}
  ICM_DRAW_SETTIME                    = (ICM_USER + 35);  // set correct value for decompress clock
  {$EXTERNALSYM ICM_DRAW_SETTIME}
  ICM_DRAW_REALIZE                    = (ICM_USER + 36);  // realize palette for drawing
  {$EXTERNALSYM ICM_DRAW_REALIZE}
  ICM_DRAW_FLUSH                      = (ICM_USER + 37);  // clear out buffered frames
  {$EXTERNALSYM ICM_DRAW_FLUSH}
  ICM_DRAW_RENDERBUFFER               = (ICM_USER + 38);  // draw undrawn things in queue
  {$EXTERNALSYM ICM_DRAW_RENDERBUFFER}

  ICM_DRAW_START_PLAY                 = (ICM_USER + 39);  // start of a play
  {$EXTERNALSYM ICM_DRAW_START_PLAY}
  ICM_DRAW_STOP_PLAY                  = (ICM_USER + 40);  // end of a play
  {$EXTERNALSYM ICM_DRAW_STOP_PLAY}

  ICM_DRAW_SUGGESTFORMAT              = (ICM_USER + 50);  // Like ICGetDisplayFormat
  {$EXTERNALSYM ICM_DRAW_SUGGESTFORMAT}
  ICM_DRAW_CHANGEPALETTE              = (ICM_USER + 51);  // for animating palette
  {$EXTERNALSYM ICM_DRAW_CHANGEPALETTE}

  ICM_DRAW_IDLE                       = (ICM_USER + 52);  // send each frame time ;Internal
  {$EXTERNALSYM ICM_DRAW_IDLE}

  ICM_GETBUFFERSWANTED                = (ICM_USER + 41);  // ask about prebuffering
  {$EXTERNALSYM ICM_GETBUFFERSWANTED}

  ICM_GETDEFAULTKEYFRAMERATE          = (ICM_USER + 42);  // get the default value for key frames
  {$EXTERNALSYM ICM_GETDEFAULTKEYFRAMERATE}


  ICM_DECOMPRESSEX_BEGIN              = (ICM_USER + 60);  // start a series of decompress calls
  {$EXTERNALSYM ICM_DECOMPRESSEX_BEGIN}
  ICM_DECOMPRESSEX_QUERY              = (ICM_USER + 61);  // start a series of decompress calls
  {$EXTERNALSYM ICM_DECOMPRESSEX_QUERY}
  ICM_DECOMPRESSEX                    = (ICM_USER + 62);  // decompress a frame
  {$EXTERNALSYM ICM_DECOMPRESSEX}
  ICM_DECOMPRESSEX_END                = (ICM_USER + 63);  // end a series of decompress calls
  {$EXTERNALSYM ICM_DECOMPRESSEX_END}

  ICM_COMPRESS_FRAMES_INFO            = (ICM_USER + 70);  // tell about compress to come
  {$EXTERNALSYM ICM_COMPRESS_FRAMES_INFO}
  ICM_COMPRESS_FRAMES                 = (ICM_USER + 71);  // compress a bunch of frames ;Internal
  {$EXTERNALSYM ICM_COMPRESS_FRAMES}
  ICM_SET_STATUS_PROC                 = (ICM_USER + 72);  // set status callback
  {$EXTERNALSYM ICM_SET_STATUS_PROC}

  //************************************************************************
  //************************************************************************

type

  PICOPEN = ^_ICOPEN;
  _ICOPEN = record  // prefixed with _
    dwSize: DWORD;                 // sizeof(_ICOPEN)
    fccType: DWORD;                // 'vidc'
    fccHandler: DWORD;             //
    dwVersion: DWORD;              // version of compman opening you
    dwFlags: DWORD;                // LOWORD is type specific
    dwError: LRESULT;              // error return.
    pV1Reserved: Pointer;          // Reserved
    pV2Reserved: Pointer;          // Reserved
    dnDevNode: DWORD;              // Devnode for PnP devices
  end;
  {$EXTERNALSYM ICOPEN}

  //************************************************************************
  //************************************************************************/

type
  // to prevent namemangling with function ICInfo we prefixed with _
  PICINFO = ^_ICINFO;
  _ICINFO = record
    dwSize: DWORD;                              // sizeof(ICINFO)
    fccType: DWORD;                             // compressor type     'vidc' 'audc'
    fccHandler: DWORD;                          // compressor sub-type 'rle ' 'jpeg' 'pcm '
    dwFlags: DWORD;                             // flags LOWORD is type specific
    dwVersion: DWORD;                           // version of the driver
    dwVersionICM: DWORD;                        // version of the ICM used
    //
    // under Win32, the driver always returns UNICODE strings.
    //
    szName: array[0..15] of WideChar;           // short name
    szDescription: array[0..127] of WideChar;   // long name
    szDriver: array[0..127] of WideChar;        // driver that contains compressor
  end;
  {$EXTERNALSYM ICINFO}


  // Flags for the <dwFlags> field of the <ICINFO> structure.

const

  VIDCF_QUALITY                       = $0001;  // supports quality
  {$EXTERNALSYM VIDCF_QUALITY}
  VIDCF_CRUNCH                        = $0002;  // supports crunching to a frame size
  {$EXTERNALSYM VIDCF_CRUNCH}
  VIDCF_TEMPORAL                      = $0004;  // supports inter-frame compress
  {$EXTERNALSYM VIDCF_TEMPORAL}
  VIDCF_COMPRESSFRAMES                = $0008;  // wants the compress all frames message
  {$EXTERNALSYM VIDCF_COMPRESSFRAMES}
  VIDCF_DRAW                          = $0010;  // supports drawing
  {$EXTERNALSYM VIDCF_DRAW}
  VIDCF_FASTTEMPORALC                 = $0020;  // does not need prev frame on compress
  {$EXTERNALSYM VIDCF_FASTTEMPORALC}
  VIDCF_FASTTEMPORALD                 = $0080;  // does not need prev frame on decompress
  {$EXTERNALSYM VIDCF_FASTTEMPORALD}

  //#define VIDCF_QUALITYTIME    0x0040  // supports temporal quality

  //#define VIDCF_FASTTEMPORAL   (VIDCF_FASTTEMPORALC|VIDCF_FASTTEMPORALD)

  //************************************************************************
  //************************************************************************

  ICCOMPRESS_KEYFRAME                 = $00000001;
  {$EXTERNALSYM ICCOMPRESS_KEYFRAME}

type

  PICCOMPRESS = ^_ICCOMPRESS;
  _ICCOMPRESS = record   // prefixed with _
    dwFlags: DWORD;                   // flags
    lpbiOutput: PBITMAPINFOHEADER;    // output format
    lpOutput: Pointer;                // output data
    lpbiInput: PBITMAPINFOHEADER;     // format of frame to compress
    lpInput: Pointer;                 // frame data to compress
    lpckid: PDWORD;                   // ckid for data in AVI file
    lpdwFlags: PDWORD;                // flags in the AVI index.
    lFrameNum: LONG;                  // frame number of seq.
    dwFrameSize: DWORD;               // reqested size in bytes. (if non zero)
    dwQuality: DWORD;                 // quality
    // these are new fields
    lpbiPrev: PBITMAPINFOHEADER;      // format of previous frame
    lpPrev: Pointer;                  // previous frame
  end;
  {$EXTERNALSYM ICCOMPRESS}

  //************************************************************************
  //************************************************************************

const

  ICCOMPRESSFRAMES_PADDING            = $00000001;
  {$EXTERNALSYM ICCOMPRESSFRAMES_PADDING}

type
  { Delphi we put the callbackfunctions here, outside the record.}
  GetDataCallback = function({_In_} lInput: LPARAM;
                             {_In_} lFrame: LONG;
                             {_Out_} lpBits: Pointer;
                             {_In_} len: LONG): LONG; stdcall;

  PutDataCallback = function({_In_} lInput: LPARAM;
                             {_In_} lFrame: LONG;
                             {_In_} lpBits: Pointer;
                             {_In_} len: LONG): LONG; stdcall;

  PICCOMPRESSFRAMES = ^ICCOMPRESSFRAMES;
  ICCOMPRESSFRAMES = record
    dwFlags: DWORD;                   // flags
    lpbiOutput: PBITMAPINFOHEADER;    // output format
    lOutput: LPARAM;                  // output identifier
    lpbiInput: PBITMAPINFOHEADER;     // format of frame to compress
    lInput: LPARAM;                   // input identifier; Reserved; do not use.
    lStartFrame: LONG;                // start frame
    lFrameCount: LONG;                // # of frames
    lQuality: LONG;                   // quality
    lDataRate: LONG;                  // data rate
    lKeyRate: LONG;                   // key frame rate
    dwRate: DWORD;                    // frame rate, as always
    dwScale: DWORD;
    dwOverheadPerFrame: DWORD;        // Reserved; do not use.
    dwReserved2: DWORD;               // Reserved; do not use.
    GetData: GetDataCallback;         // Reserved; do not use.
    PutData: PutDataCallback;         // Reserved; do not use.
  end;
  {$EXTERNALSYM ICCOMPRESSFRAMES}


  // messages for Status callback

const

  {$EXTERNALSYM ICSTATUS_START}
  ICSTATUS_START                      = 0;
  {$EXTERNALSYM ICSTATUS_STATUS}
  ICSTATUS_STATUS                     = 1;  // l == % done
  {$EXTERNALSYM ICSTATUS_END}
  ICSTATUS_END                        = 2;
  {$EXTERNALSYM ICSTATUS_ERROR}
  ICSTATUS_ERROR                      = 3;  // l == error string (LPSTR)
  {$EXTERNALSYM ICSTATUS_YIELD}
  ICSTATUS_YIELD                      = 4;

type

  FICStatusProc = function(lParam: LPARAM;
                           _message: UINT;
                           l: LONG): LONG; stdcall;

  PICSETSTATUSPROC = ^_ICSETSTATUSPROC;
  _ICSETSTATUSPROC = record
    dwFlags: DWORD; // Reserved; set to zero.
    lParam: LPARAM;
    // return nonzero means abort operation in progress
    status: FICStatusProc;
  end;
  {$EXTERNALSYM _ICSETSTATUSPROC}

  //************************************************************************
  //************************************************************************

const

  ICDECOMPRESS_HURRYUP                = $80000000;  // don't draw just buffer (hurry up!)
  {$EXTERNALSYM ICDECOMPRESS_HURRYUP}
  ICDECOMPRESS_UPDATE                 = $40000000;  // don't draw just update screen
  {$EXTERNALSYM ICDECOMPRESS_UPDATE}
  ICDECOMPRESS_PREROLL                = $20000000;  // this frame is before real start
  {$EXTERNALSYM ICDECOMPRESS_PREROLL}
  ICDECOMPRESS_NULLFRAME              = $10000000;  // repeat last frame
  {$EXTERNALSYM ICDECOMPRESS_NULLFRAME}
  ICDECOMPRESS_NOTKEYFRAME            = $08000000;  // this frame is not a key frame
  {$EXTERNALSYM ICDECOMPRESS_NOTKEYFRAME}

type

  PICDECOMPRESS = ^_ICDECOMPRESS;
  _ICDECOMPRESS = record   // prefixed _
    dwFlags: DWORD;                   // flags (from AVI index...)
    lpbiInput: PBITMAPINFOHEADER;     // BITMAPINFO of compressed data
                                      // biSizeImage has the chunk size
    lpInput: Pointer;                 // compressed data
    lpbiOutput: PBITMAPINFOHEADER;    // DIB to decompress to
    lpOutput: Pointer;
    ckid: DWORD;                      // ckid from AVI file
  end;
  {$EXTERNALSYM ICDECOMPRESS}


  PICDECOMPRESSEX = ^_ICDECOMPRESSEX;
  _ICDECOMPRESSEX = record   // prefixed with _
    //
    // same as ICM_DECOMPRESS
    //
    dwFlags: DWORD;
    lpbiSrc: PBITMAPINFOHEADER;      // BITMAPINFO of compressed data
    lpSrc: Pointer;                  // compressed data
    lpbiDst: PBITMAPINFOHEADER;      // DIB to decompress to
    lpDst: Pointer;                  // output data
    //
    // new for ICM_DECOMPRESSEX
    //
    xDst: Integer;                   // destination rectangle
    yDst: Integer;
    dxDst: Integer;
    dyDst: Integer;
    xSrc: Integer;                   // source rectangle
    ySrc: Integer;
    dxSrc: Integer;
    dySrc: Integer;
  end;
  {$EXTERNALSYM _ICDECOMPRESSEX}

  //************************************************************************
  //************************************************************************/

const

  ICDRAW_QUERY                        = $00000001;  // test for support
  {$EXTERNALSYM ICDRAW_QUERY}
  ICDRAW_FULLSCREEN                   = $00000002;  // draw to full screen
  {$EXTERNALSYM ICDRAW_FULLSCREEN}
  ICDRAW_HDC                          = $00000004;  // draw to a HDC/HWND
  {$EXTERNALSYM ICDRAW_HDC}
  ICDRAW_ANIMATE                      = $00000008;  // expect palette animation
  {$EXTERNALSYM ICDRAW_ANIMATE}
  ICDRAW_CONTINUE                     = $00000010;  // draw is a continuation of previous draw
  {$EXTERNALSYM ICDRAW_CONTINUE}
  ICDRAW_MEMORYDC                     = $00000020;  // DC is offscreen, by the way
  {$EXTERNALSYM ICDRAW_MEMORYDC}
  ICDRAW_UPDATING                     = $00000040;  // We're updating, as opposed to playing
  {$EXTERNALSYM ICDRAW_UPDATING}
  ICDRAW_RENDER                       = $00000080;  // used to render data not draw it
  {$EXTERNALSYM ICDRAW_RENDER}
  ICDRAW_BUFFER                       = $00000100;  // please buffer this data offscreen, we will need to update it
  {$EXTERNALSYM ICDRAW_BUFFER}

type

  PICDRAWBEGIN = ^_ICDRAWBEGIN;
  _ICDRAWBEGIN = record  // prefixed with _
    dwFlags: DWORD;                  // flags
    hpal: HPALETTE;                  // palette to draw with
    _hWnd: HWND;                     // window to draw to
    hdc: HDC;                        // HDC to draw to
    xDst: Integer;                   // destination rectangle
    yDst: Integer;
    dxDst: Integer;
    dyDst: Integer;
    lpbi: PBITMAPINFOHEADER;         // format of frame to draw
    xSrc: Integer;                   // source rectangle
    ySrc: Integer;
    dxSrc: Integer;
    dySrc: Integer;
    dwRate: DWORD;                   // frames/second = (dwRate/dwScale)
    dwScale: DWORD;
  end;
  {$EXTERNALSYM _ICDRAWBEGIN}

  //************************************************************************
  //************************************************************************

const

  ICDRAW_HURRYUP                      = $80000000;  // don't draw just buffer (hurry up!)
  {$EXTERNALSYM ICDRAW_HURRYUP}
  ICDRAW_UPDATE                       = $40000000;  // don't draw just update screen
  {$EXTERNALSYM ICDRAW_UPDATE}
  ICDRAW_PREROLL                      = $20000000;  // this frame is before real start
  {$EXTERNALSYM ICDRAW_PREROLL}
  ICDRAW_NULLFRAME                    = $10000000;  // repeat last frame
  {$EXTERNALSYM ICDRAW_NULLFRAME}
  ICDRAW_NOTKEYFRAME                  = $08000000;  // this frame is not a key frame
  {$EXTERNALSYM ICDRAW_NOTKEYFRAME}

type

  PICDRAW = ^_ICDRAW;
  _ICDRAW = record
    dwFlags: DWORD;                  // flags
    lpFormat: Pointer;               // format of frame to decompress
    lpData: Pointer;                 // frame data to decompress
    cbData: DWORD;
    lTime: LONG;                     // time in drawbegin units (see dwRate and dwScale)
  end;
  {$EXTERNALSYM _ICDRAW}

  PICDRAWSUGGEST = ^ICDRAWSUGGEST;
  ICDRAWSUGGEST = record
    lpbiIn: PBITMAPINFOHEADER;        // format to be drawn
    lpbiSuggest: PBITMAPINFOHEADER;   // location for suggested format (or NULL to get size)
    dxSrc: Integer;                   // source extent or 0
    dySrc: Integer;
    dxDst: Integer;                   // dest extent or 0
    dyDst: Integer;
    hicDecompressor: HIC;             // decompressor you can talk to
  end;
  {$EXTERNALSYM ICDRAWSUGGEST}

  //************************************************************************
  //************************************************************************

  PICPALETTE = ^ICPALETTE;
  ICPALETTE = record
    dwFlags: DWORD;                  // flags (from AVI index...)
    iStart: Integer;                 // first palette to change
    iLen: Integer;                   // count of entries to change.
    lppe: PPALETTEENTRY;             // palette
  end;
  {$EXTERNALSYM ICPALETTE}


  //************************************************************************
  //
  //  ICM function declarations
  //
  //************************************************************************/


  function ICInfo(fccType: DWORD;
                  fccHandler: DWORD;
                  {_Out_} lpicinfo: PICINFO): BOOL; stdcall;

  function ICInstall(fccType: DWORD;
                     fccHandler: DWORD;
                     lParam: LPARAM;
                     szDesc: PAnsiChar;
                     wFlags: UINT): BOOL; stdcall;

  function ICRemove(fccType: DWORD;
                    fccHandler: DWORD;
                    wFlags: UINT): BOOL; stdcall;

  function ICGetInfo(hic: HIC;
                     {_Out_} picinfo: PICINFO;
                     cb: DWORD): LRESULT; stdcall;  //_Success_(return >= 0)

  function ICOpen(fccType: DWORD;
                  fccHandler: DWORD;
                  wMode: UINT): HIC; stdcall;

  function ICOpenFunction(fccType: DWORD;
                          fccHandler: DWORD;
                          wMode: UINT;
                          lpfnHandler: FARPROC): HIC; stdcall;

  function ICClose(hic: HIC): LRESULT; stdcall;

  // note that ICM swaps round the length and pointer
  // length in lparam2, pointer in lparam1
  function ICSendMessage(hic: HIC;
                         msg: UINT;
                         dw1: DWORD_PTR;
                         dw2: DWORD_PTR): LRESULT; stdcall;

  //this function is unsupported on Win32 as it is non-portable.
  //LRESULT VFWAPIV ICMessage(HIC hic, UINT msg, UINT cb, ...);



  // Values for wFlags of ICInstall()

const

  ICINSTALL_UNICODE                   = $8000;
  {$EXTERNALSYM ICINSTALL_UNICODE}

  ICINSTALL_FUNCTION                  = $0001;  // lParam is a DriverProc (function ptr)
  {$EXTERNALSYM ICINSTALL_FUNCTION}
  ICINSTALL_DRIVER                    = $0002;  // lParam is a driver name (string)
  {$EXTERNALSYM ICINSTALL_DRIVER}
  ICINSTALL_HDRV                      = $0004;  // lParam is a HDRVR (driver handle)
  {$EXTERNALSYM ICINSTALL_HDRV}

  ICINSTALL_DRIVERW                   = $8002;  // lParam is a unicode driver name
  {$EXTERNALSYM ICINSTALL_DRIVERW}

  //************************************************************************
  //
  //  query macros
  //
  //************************************************************************

  {$EXTERNALSYM ICMF_CONFIGURE_QUERY}
  ICMF_CONFIGURE_QUERY                = $00000001;
  {$EXTERNALSYM ICMF_ABOUT_QUERY}
  ICMF_ABOUT_QUERY                    = $00000001;

  function ICQueryAbout(hic: HIC): BOOL;
  {$EXTERNALSYM ICQueryAbout}

  function ICAbout(hic: HIC;
                   _hWnd: HWND): LRESULT;
  {$EXTERNALSYM ICAbout}

  {$EXTERNALSYM ICQueryConfigure}
  function ICQueryConfigure(hic: HIC): BOOL;

  {$EXTERNALSYM ICConfigure}
  function ICConfigure(hic: HIC;
                       _hWnd: HWND): LRESULT;


  //************************************************************************
  //
  //  get/set state macros
  //
  //************************************************************************

  function ICGetState(hic: HIC;
                      pv: LPVOID;
                      cb: DWORD_PTR): LRESULT;
  {$EXTERNALSYM ICGetState}

  function ICSetState(hic: HIC;
                      pv: LPVOID;
                      cb: DWORD_PTR): LRESULT;
  {$EXTERNALSYM ICSetState}

  function ICGetStateSize(hic: HIC): DWORD;
  {$EXTERNALSYM ICGetStateSize}


  //************************************************************************
  //
  //  get value macros
  //
  //************************************************************************


  function ICGetDefaultQuality(hic: HIC): LRESULT;
  {$EXTERNALSYM ICGetDefaultQuality}

  function ICGetDefaultKeyFrameRate(hic: HIC): LRESULT;
  {$EXTERNALSYM ICGetDefaultKeyFrameRate}

  //************************************************************************
  //
  //  draw window macro
  //
  //************************************************************************

  function ICDrawWindow(hic: HIC;
                        prc: PRect): LRESULT;
  {$EXTERNALSYM ICDrawWindow}


  //************************************************************************
  //
  //  compression functions
  //
  //************************************************************************

  //
  //  ICCompress()
  //
  //  compress a single frame
  //
  //

  function ICCompress(hic: HIC;
                      dwFlags: DWORD;                          // flags
                      lpbiOutput: PBITMAPINFOHEADER;           // output format
                      {_Out_} lpData: Pointer;                 // output data
                      lpbiInput: PBITMAPINFOHEADER;            // format of frame to compress
                      lpBits: Pointer;                         // frame data to compress
                      {_Out_opt_} lpckid: PDWORD;              // ckid for data in AVI file
                      {_Out_opt_} lpdwFlags: PDWORD;           // flags in the AVI index.
                      dwFrameSize: DWORD;                      // reqested size in bytes. (if non zero)
                      dwQuality: DWORD;                        // quality within one frame
                      {_In_opt_} lpbiPrev: PBITMAPINFOHEADER;  // format of previous frame
                      lpPrev: Pointer): DWORD; stdcall;

  //
  //  ICCompressBegin()
  //
  //  start compression from a source format (lpbiInput) to a dest
  //  format (lpbiOuput) is supported.
  //

  function ICCompressBegin(hic: HIC;
                           lpbiInput: PBITMAPINFOHEADER;
                           lpbiOutput: PBITMAPINFOHEADER): LRESULT; stdcall;
  {$EXTERNALSYM ICCompressBegin}


  //
  //  ICCompressQuery()
  //
  //  determines if compression from a source format (lpbiInput) to a dest
  //  format (lpbiOuput) is supported.
  //

  function ICCompressQuery(hic: HIC;
                           lpbiInput: PBITMAPINFOHEADER;
                           lpbiOutput: PBITMAPINFOHEADER): LRESULT; stdcall;
  {$EXTERNALSYM ICCompressQuery}


  //
  //  ICCompressGetFormat()
  //
  //  get the output format, (format of compressed data)
  //  if lpbiOutput is NULL return the size in bytes needed for format.
  //

  function ICCompressGetFormat(hic: HIC;
                               lpbiInput: PBITMAPINFOHEADER;
                               lpbiOutput: PBITMAPINFOHEADER): LResult; stdcall;
  {$EXTERNALSYM ICCompressGetFormat}

  function ICCompressGetFormatSize(hic: HIC;
                                   lpbi: PBITMAPINFOHEADER): DWord; stdcall;
  {$EXTERNALSYM ICCompressGetFormatSize}

  //
  //  ICCompressSize()
  //
  //  return the maximal size of a compressed frame
  //

  function ICCompressGetSize(hic: HIC;
                             lpbiInput: PBITMAPINFOHEADER;
                             lpbiOutput: PBITMAPINFOHEADER): DWord; stdcall;
  {$EXTERNALSYM ICCompressGetSize}

  function ICCompressEnd(hic: HIC): LRESULT; stdcall;
  {$EXTERNALSYM ICCompressEnd}


  //************************************************************************
  //
  //  decompression functions
  //
  //************************************************************************

  //
  //  ICDecompress()
  //
  //  decompress a single frame
  //
  //


  function ICDecompress(hic: HIC;
                        dwFlags: DWORD;                         // flags (from AVI index...)
                        lpbiFormat: PBITMAPINFOHEADER;          // BITMAPINFO of compressed data
                                                                // biSizeImage has the chunk size
                        lpData: Pointer;                        // data
                        lpbi: PBITMAPINFOHEADER;                // DIB to decompress to
                        {_Out_} lpBits: Pointer): DWord; stdcall;
  {$EXTERNALSYM ICDecompress}

  //
  //  ICDecompressBegin()
  //
  //  start compression from a source format (lpbiInput) to a dest
  //  format (lpbiOutput) is supported.
  //

  function ICDecompressBegin(hic: HIC;
                             lpbiInput: PBITMAPINFOHEADER;
                             lpbiOutput: PBITMAPINFOHEADER): LResult;
    {$EXTERNALSYM ICDecompressBegin}

  //
  //  ICDecompressQuery()
  //
  //  determines if compression from a source format (lpbiInput) to a dest
  //  format (lpbiOutput) is supported.
  //

  function ICDecompressQuery(hic: HIC;
                             lpbiInput: PBITMAPINFOHEADER;
                             lpbiOutput: PBITMAPINFOHEADER): LResult;
  {$EXTERNALSYM ICDecompressQuery}

  //
  //  ICDecompressGetFormat()
  //
  //  get the output format, (format of un-compressed data)
  //  if lpbiOutput is NULL return the size in bytes needed for format.
  //

  function ICDecompressGetFormat(hic: HIC;
                                 lpbiInput: PBITMAPINFOHEADER;
                                 lpbiOutput: PBITMAPINFOHEADER): LONG;
  {$EXTERNALSYM ICDecompressGetFormat}

  function ICDecompressGetFormatSize(hic: HIC;
                                     lpbi: PBITMAPINFOHEADER): LResult;
  {$EXTERNALSYM ICDecompressGetFormatSize}


  //
  //  ICDecompressGetPalette()
  //
  //  get the output palette
  //
  //

  function ICDecompressGetPalette(hic: HIC;
                                  lpbiInput: PBITMAPINFOHEADER;
                                  lpbiOutput: PBITMAPINFOHEADER): LResult;
  {$EXTERNALSYM ICDecompressGetPalette}

  function ICDecompressSetPalette(hic: HIC;
                                  lpbiPalette: PBITMAPINFOHEADER): LResult;
  {$EXTERNALSYM ICDecompressSetPalette}

  function ICDecompressEnd(hic: HIC): LResult;
  {$EXTERNALSYM ICDecompressEnd}


  //************************************************************************
  //
  //  decompression (ex) functions
  //
  //************************************************************************/

  //
  // on Win16 these functions are macros that call ICMessage. ICMessage will
  // not work on NT. rather than add new entrypoints we have given
  // them as static inline functions
  //

  //
  //  ICDecompressEx()
  //
  //  decompress a single frame
  //

  function ICDecompressEx(hic: HIC;
                          dwFlags: DWORD;
                          lpbiSrc: PBITMAPINFOHEADER;
                          lpSrc: Pointer;
                          xSrc: Integer;
                          ySrc: Integer;
                          dxSrc: Integer;
                          dySrc: Integer;
                          lpbiDst: PBITMAPINFOHEADER;
                          lpDst: Pointer;
                          xDst: Integer;
                          yDst: Integer;
                          dxDst: Integer;
                          dyDst: Integer): LResult; inline;


  //
  //  ICDecompressExBegin()
  //
  // start compression from a source format (lpbiInput) to a dest
  // format (lpbiOutput) is supported.
  //

  function ICDecompressExBegin(hic: HIC;
                               dwFlags: DWORD;
                               lpbiSrc: PBITMAPINFOHEADER;
                               {_In_opt_} lpSrc: Pointer;
                               xSrc: Integer;
                               ySrc: Integer;
                               dxSrc: Integer;
                               dySrc: Integer;
                               lpbiDst: PBITMAPINFOHEADER;
                               {_Out_opt_} lpDst: Pointer;
                               xDst: Integer;
                               yDst: Integer;
                               dxDst: Integer;
                               dyDst: Integer): LResult; inline;


  //
  //  ICDecompressExQuery()
  //

  function ICDecompressExQuery(hic: HIC;
                               dwFlags: DWORD;
                               lpbiSrc: PBITMAPINFOHEADER;
                               {_Reserved_} lpSrc: Pointer;
                               xSrc: Integer;
                               ySrc: Integer;
                               dxSrc: Integer;
                               dySrc: Integer;
                               {_In_opt_} lpbiDst: PBITMAPINFOHEADER;
                               {_Out_opt_} lpDst: Pointer;
                               xDst: Integer;
                               yDst: Integer;
                               dxDst: Integer;
                               dyDst: Integer): LResult; inline;


  {$EXTERNALSYM ICDecompressExEnd}
  function ICDecompressExEnd(hic: HIC): LResult;


  //************************************************************************
  //
  //  drawing functions
  //
  //************************************************************************/

  //
  //  ICDrawBegin()
  //
  //  start decompressing data with format (lpbiInput) directly to the screen
  //
  //  return zero if the decompressor supports drawing.
  //

  function ICDrawBegin(hic: HIC;
                       dwFlags: DWORD;            // flags
                       {_In_opt_} hpal: HPALETTE; // palette to draw with
                       {_In_opt_} _hWnd: HWND;     // window to draw to
                       {_In_opt_} hdc: HDC;       // HDC to draw to
                       xDst: Integer;             // destination rectangle
                       yDst: Integer;
                       dxDst: Integer;
                       dyDst: Integer;
                       lpbi: PBITMAPINFOHEADER;   // format of frame to draw
                       xSrc: Integer;             // source rectangle
                       ySrc: Integer;
                       dxSrc: Integer;
                       dySrc: Integer;
                       dwRate: DWORD;             // frames/second = (dwRate/dwScale)
                       dwScale: DWORD): DWORD; stdcall;

  //
  //  ICDraw()
  //
  //  decompress data directly to the screen
  //

  function ICDraw(hic: HIC;
                  dwFlags: DWORD;                   // flags
                  lpFormat: Pointer;                // format of frame to decompress
                  lpData: Pointer;                  // frame data to decompress
                  cbData: DWORD;                    // size of data
                  lTime: LONG): DWord; stdcall;


  // ICMessage is not supported on Win32, so provide a static inline function
  // to do the same job

  function ICDrawSuggestFormat(hic: HIC;
                               lpbiIn: PBITMAPINFOHEADER;
                               {_Out_} lpbiOut: PBITMAPINFOHEADER;
                               dxSrc: Integer;
                               dySrc: Integer;
                               dxDst: Integer;
                               dyDst: Integer;
                               hicDecomp: HIC): LResult; inline;


  //
  //  ICDrawQuery()
  //
  //  determines if the compressor is willing to render the specified format.
  //

  function ICDrawQuery(hic: HIC;
                       lpbiInput: PBITMAPINFOHEADER): LResult;
  {$EXTERNALSYM ICDrawQuery}

  function ICDrawChangePalette(hic: HIC;
                               lpbiInput: PBITMAPINFOHEADER): LResult;
  {$EXTERNALSYM ICDrawChangePalette}

  function ICGetBuffersWanted(hic: HIC;
                              lpdwBuffers: PDWord): LResult;
  {$EXTERNALSYM ICGetBuffersWanted}

  function ICDrawEnd(hic: HIC): LResult;
  {$EXTERNALSYM ICDrawEnd}

  function ICDrawStart(hic: HIC): LResult;
  {$EXTERNALSYM ICDrawStart}

  function ICDrawStartPlay(hic: HIC;
                           lFrom: DWord;
                           lTo: DWord): LResult;
  {$EXTERNALSYM ICDrawStartPlay}

  function ICDrawStop(hic: HIC): LResult;
  {$EXTERNALSYM ICDrawStop}

  function ICDrawStopPlay(hic: HIC): LResult;
  {$EXTERNALSYM ICDrawStopPlay}

  function ICDrawGetTime(hic: HIC;
                         lplTime: PDWORD_PTR): LResult;
  {$EXTERNALSYM ICDrawGetTime}

  function ICDrawSetTime(hic: HIC;
                         lTime: DWORD_PTR): LResult;
  {$EXTERNALSYM ICDrawSetTime}

  function ICDrawRealize(hic: HIC;
                         dc: HDC;
                         fBackground: BOOL): LResult;
  {$EXTERNALSYM ICDrawRealize}

  function ICDrawFlush(hic: HIC): LResult;
  {$EXTERNALSYM ICDrawFlush}

  function ICDrawRenderBuffer(hic: HIC): LResult;
  {$EXTERNALSYM ICDrawRenderBuffer}


  //************************************************************************
  //
  //  Status callback functions
  //
  //************************************************************************

  //
  //  ICSetStatusProc()
  //
  //  Set the status callback function
  //

  // ICMessage is not supported on NT
  function ICSetStatusProc(hic: HIC;
                           dwFlags: DWORD;
                           lParam: DWORD;
                           fpfnStatus: FICStatusProc): LRESULT;
{
    ICSETSTATUSPROC ic;

    ic.dwFlags = dwFlags;
    ic.lParam = lParam;
    ic.Status = fpfnStatus;

    // note that ICM swaps round the length and pointer
    // length in lparam2, pointer in lparam1
    return ICSendMessage(hic, ICM_SET_STATUS_PROC, (DWORD_PTR)&ic, sizeof(ic));
}


  //************************************************************************
  //
  //  helper routines for DrawDib and MCIAVI...
  //
  //************************************************************************

  function ICDecompressOpen(fccType: FOURCC;
                            fccHandler: FOURCC;
                            lpbiIn: PBITMAPINFOHEADER;
                            lpbiOut: PBITMAPINFOHEADER): HIC;
  {$EXTERNALSYM ICDecompressOpen}

  function ICDrawOpen(fccType: FOURCC;
                      fccHandler: FOURCC;
                      lpbiIn: PBITMAPINFOHEADER): HIC;
  {$EXTERNALSYM ICDrawOpen}

  function ICLocate(fccType: DWORD;
                    fccHandler: DWORD;
                    lpbiIn: PBITMAPINFOHEADER;
                    {_In_opt_} lpbiOut: PBITMAPINFOHEADER;
                    wFlags: WORD): HIC; stdcall;
  {$EXTERNALSYM ICLocate}

  function ICGetDisplayFormat({_In_opt_} hic: HIC;
                              lpbiIn: PBITMAPINFOHEADER;
                              {_Out_} lpbiOut: PBITMAPINFOHEADER;
                              BitDepth: Integer;
                              dx: Integer;
                              dy: Integer): HIC; stdcall;
  {$EXTERNALSYM ICGetDisplayFormat}


  //************************************************************************
  //  Higher level functions
  //************************************************************************

  function ICImageCompress(hic: HIC;                         // compressor to use
                           uiFlags: UINT;                    // flags (none yet)
                           lpbiIn: PBITMAPINFO;              // format to compress from
                           lpBits: Pointer;                  // data to compress
                           {_In_opt_} lpbiOut: PBITMAPINFO;  // compress to this (NULL ==> default)
                           lQuality: LONG;                   // quality to use
                           {_Inout_opt_} plSize: LONG): THandle; stdcall;
  {$EXTERNALSYM ICImageCompress}

  function ICImageDecompress({_In_opt_} hic: HIC;  // compressor to use
                             uiFlags: UINT;        // flags (none yet)
                             lpbiIn: PBITMAPINFO;  // format to decompress from
                             lpBits: Pointer;      // data to decompress
                             {_In_opt_} lpbiOut: PBITMAPINFO): THandle; stdcall;
  {$EXTERNALSYM ICImageDecompress}

  //
  // Structure used by ICSeqCompressFrame and ICCompressorChoose routines
  // Make sure this matches the autodoc in icm.c!
  //

type

  PCOMPVARS = ^COMPVARS;
  {$EXTERNALSYM COMPVARS}
  COMPVARS = record
    cbSize: LONG;                    // set to sizeof(COMPVARS) before
                                     // calling ICCompressorChoose
    dwFlags: DWORD;                  // see below...
    hic: HIC;                        // HIC of chosen compressor
    fccType: DWORD;                  // basically ICTYPE_VIDEO
    fccHandler: DWORD;               // handler of chosen compressor or
                                     // "" or "DIB "
    lpbiIn: PBITMAPINFO;             // input format
    lpbiOut: PBITMAPINFO;            // output format - will compress to this
    lpBitsOut: Pointer;
    lpBitsPrev: Pointer;
    lFrame: LONG;
    lKey: LONG;                      // key frames how often?
    lDataRate: LONG;                 // desired data rate KB/Sec
    lQ: LONG;                        // desired quality
    lKeyCount: LONG;
    lpState: Pointer;                // state of compressor
    cbState: LONG;                   // size of the state
  end;
  {$EXTERNALSYM PCOMPVARS}

  // FLAGS for dwFlags element of COMPVARS structure:
  // set this flag if you initialize COMPVARS before calling ICCompressorChoose

const

  ICMF_COMPVARS_VALID = $00000001;  // COMPVARS contains valid data
  {$EXTERNALSYM ICMF_COMPVARS_VALID}

  //
  //  allows user to choose compressor, quality etc...
  //

  function ICCompressorChoose({_In_opt_} _hWnd: HWND;     // parent window for dialog
                              uiFlags: UINT;              // flags
                              {_In_opt_} pvIn: Pointer;   // input format (optional)
                              {_In_opt_} lpData: Pointer; // input data (optional)
                              {_Inout_} pc: PCOMPVARS;    // data about the compressor/dlg
                              {_In_opt_} lpszTitle: PAnsiChar): BOOL; stdcall;

  // defines for uiFlags

const

  ICMF_CHOOSE_KEYFRAME                = $0001;  // show KeyFrame Every box
  {$EXTERNALSYM ICMF_CHOOSE_KEYFRAME}
  ICMF_CHOOSE_DATARATE                = $0002;  // show DataRate box
  {$EXTERNALSYM ICMF_CHOOSE_DATARATE}
  ICMF_CHOOSE_PREVIEW                 = $0004;  // allow expanded preview dialog
  {$EXTERNALSYM ICMF_CHOOSE_PREVIEW}
  ICMF_CHOOSE_ALLCOMPRESSORS          = $0008;  // don't only show those that
						                                    // can handle the input format
						                                    // or input data
  {$EXTERNALSYM ICMF_CHOOSE_ALLCOMPRESSORS}


  function ICSeqCompressFrameStart(pc: PCOMPVARS;
                                   lpbiIn: PBITMAPINFO): BOOL; stdcall;
  {$EXTERNALSYM ICSeqCompressFrameStart}

  procedure ICSeqCompressFrameEnd(pc: PCOMPVARS); stdcall;
  {$EXTERNALSYM ICSeqCompressFrameEnd}

  function ICSeqCompressFrame(pc: PCOMPVARS;              // set by ICCompressorChoose
                              {_Reserved_} uiFlags: UINT; // flags
                              lpBits: Pointer;            // input DIB bits
                              {_Out_} pfKey: PBOOL;       // did it end up being a key frame?
                              {_Inout_opt_} plSize: PLONG // size to compress to/of returned image
                              ): Pointer; stdcall;
  {$EXTERNALSYM ICSeqCompressFrame}

  procedure ICCompressorFree(pc: PCOMPVARS); stdcall;
  {$EXTERNALSYM ICCompressorFree}


  //**************************************************************************
  //
  //  DRAWDIB - Routines for drawing to the display.
  //
  //**************************************************************************

type
  HDRAWDIB = THandle;  { hdd }
  {$EXTERNALSYM HDRAWDIB}

  //**********************************************************************
  //
  //  DrawDib Flags
  //
  //**********************************************************************

const

  DDF_0001                            = $0001;  { ;Internal }
  {$EXTERNALSYM DDF_0001}
  DDF_UPDATE                          = $0002;  { re-draw the last DIB }
  {$EXTERNALSYM DDF_UPDATE}
  DDF_SAME_HDC                        = $0004;  { HDC same as last call (all setup) }
  {$EXTERNALSYM DDF_SAME_HDC}
  DDF_SAME_DRAW                       = $0008;  { draw params are the same }
  {$EXTERNALSYM DDF_SAME_DRAW}
  DDF_DONTDRAW                        = $0010;  { dont draw frame, just decompress }
  {$EXTERNALSYM DDF_DONTDRAW}
  DDF_ANIMATE                         = $0020;  { allow palette animation }
  {$EXTERNALSYM DDF_ANIMATE}
  DDF_BUFFER                          = $0040;  { always buffer image }
  {$EXTERNALSYM DDF_BUFFER}
  DDF_JUSTDRAWIT                      = $0080;  { just draw it with GDI }
  {$EXTERNALSYM DDF_JUSTDRAWIT}
  DDF_FULLSCREEN                      = $0100;  { use DisplayDib }
  {$EXTERNALSYM DDF_FULLSCREEN}
  DDF_BACKGROUNDPAL                   = $0200;  { Realize palette in background }
  {$EXTERNALSYM DDF_BACKGROUNDPAL}
  DDF_NOTKEYFRAME                     = $0400;  { this is a partial frame update, hint }
  {$EXTERNALSYM DDF_NOTKEYFRAME}
  DDF_HURRYUP                         = $0800;  { hurry up please! }
  {$EXTERNALSYM DDF_HURRYUP}
  DDF_HALFTONE                        = $1000;  { always halftone }
  {$EXTERNALSYM DDF_HALFTONE}
  DDF_2000                            = $2000;  { ;Internal }
  {$EXTERNALSYM DDF_2000}

  DDF_PREROLL                         = DDF_DONTDRAW;  { Builing up a non-keyframe }
  {$EXTERNALSYM DDF_PREROLL}
  DDF_SAME_DIB                        = DDF_SAME_DRAW;
  {$EXTERNALSYM DDF_SAME_DIB}
  DDF_SAME_SIZE                       = DDF_SAME_DRAW;
  {$EXTERNALSYM DDF_SAME_SIZE}

  //*********************************************************************
  //
  //  DrawDib functions
  //
  //*********************************************************************

  // Internal
  // function DrawDibInit(): BOOL; stdcall;
  // Internal

  //
  //  DrawDibOpen()
  //

  function DrawDibOpen(): HDRAWDIB; stdcall;

  //
  //  DrawDibClose()
  //

  function DrawDibClose(hdd: HDRAWDIB): BOOL; stdcall;

  //
  // DrawDibGetBuffer()
  //

  function DrawDibGetBuffer(hdd: HDRAWDIB;
                            {_Out_} lpbi: PBITMAPINFOHEADER;
                            dwSize: DWORD;
                            dwFlags: DWORD): Pointer; stdcall;

  // Internal
  //  function DrawDibError(HDRAWDIB hdd): UINT; stdcall;
  // Internal

  //
  //  DrawDibGetPalette()
  //
  //  get the palette used for drawing DIBs
  //

  function DrawDibGetPalette(hdd: HDRAWDIB): HPALETTE; stdcall;


  //
  //  DrawDibSetPalette()
  //
  //  get the palette used for drawing DIBs
  //

  function DrawDibSetPalette(hdd: HDRAWDIB;
                             {_In_opt_} hpal: HPALETTE): BOOL; stdcall;

  //
  //  DrawDibChangePalette()
  //

  function DrawDibChangePalette(hdd: HDRAWDIB;
                                iStart: Integer;
                                iLen: Integer;
                                lppe: PPALETTEENTRY): BOOL; stdcall;

  //
  //  DrawDibRealize()
  //
  //  realize the palette in a HDD
  //

  function DrawDibRealize(hdd: HDRAWDIB;
                          dc: HDC;
                          fBackground: BOOL): UINT; stdcall;

  //
  //  DrawDibStart()
  //
  //  start of streaming playback
  //

  function DrawDibStart(hdd: HDRAWDIB;
                        rate: DWORD): BOOL; stdcall;

  //
  //  DrawDibStop()
  //
  //  start of streaming playback
  //

  function DrawDibStop(hdd: HDRAWDIB): BOOL; stdcall;

  //
  //  DrawDibBegin()
  //
  //  prepare to draw
  //

  function DrawDibBegin(hdd: HDRAWDIB;
                        {_In_opt_} dc: HDC;
                        dxDst: Integer;
                        dyDst: Integer;
                        lpbi: PBITMAPINFOHEADER;
                        dxSrc: Integer;
                        dySrc: Integer;
                        wFlags: UINT): BOOL; stdcall;

  //
  //  DrawDibDraw()
  //
  //  actualy draw a DIB to the screen.
  //

  function DrawDibDraw(hdd: HDRAWDIB;
                       hdc: HDC;
                       xDst: Integer;
                       yDst: Integer;
                       dxDst: Integer;
                       dyDst: Integer;
                       {_In_opt_} lpbi: PBITMAPINFOHEADER;
                       {_In_opt_} lpBits: Pointer;
                       xSrc: Integer;
                       ySrc: Integer;
                       dxSrc: Integer;
                       dySrc: Integer;
                       wFlags: UINT): BOOL; stdcall;

  //
  //  DrawDibUpdate()
  //
  //  redraw the last image (may only be valid with DDF_BUFFER)
  //

  function DrawDibUpdate(hdd: HDRAWDIB;
                         dc: HDC;
                         x: Integer;
                         y: Integer): BOOL; stdcall;
  {$EXTERNALSYM DrawDibUpdate}


  //
  //  DrawDibEnd()
  //

  function DrawDibEnd(hdd: HDRAWDIB): BOOL; stdcall;
  {$EXTERNALSYM DrawDibEnd}

  //
  //  DrawDibTime()  [for debugging purposes only]
  //

type
  PDRAWDIBTIME = ^_DRAWDIBTIME;
  {$EXTERNALSYM PDRAWDIBTIME}
  _DRAWDIBTIME = record   // prefixed with _
    timeCount: LONG;
    timeDraw: LONG;
    timeDecompress: LONG;
    timeDither: LONG;
    timeStretch: LONG;
    timeBlt: LONG;
    timeSetDIBits: LONG;
  end;
  {$EXTERNALSYM _DRAWDIBTIME}
  LPDRAWDIBTIME = ^_DRAWDIBTIME;
  {$EXTERNALSYM LPDRAWDIBTIME}

  function DrawDibTime(hdd: HDRAWDIB;
                       {_Out_} lpddtime: PDRAWDIBTIME): BOOL; stdcall;
  {$EXTERNALSYM DrawDibTime}

  // display profiling

const

  PD_CAN_DRAW_DIB                     = $0001;  { if you can draw at all }
  {$EXTERNALSYM PD_CAN_DRAW_DIB}
  PD_CAN_STRETCHDIB                   = $0002;  { basicly RC_STRETCHDIB }
  {$EXTERNALSYM PD_CAN_STRETCHDIB}
  PD_STRETCHDIB_1_1_OK                = $0004;  { is it fast? }
  {$EXTERNALSYM PD_STRETCHDIB_1_1_OK}
  PD_STRETCHDIB_1_2_OK                = $0008;  { ... }
  {$EXTERNALSYM PD_STRETCHDIB_1_2_OK}
  PD_STRETCHDIB_1_N_OK                = $0010;  { ... }
  {$EXTERNALSYM PD_STRETCHDIB_1_N_OK}

  function DrawDibProfileDisplay(lpbi: PBITMAPINFOHEADER): LResult; stdcall;
  {$EXTERNALSYM DrawDibProfileDisplay}


  procedure StretchDIB(biDst: PBITMAPINFOHEADER;
	                     {_Out_} lpDst: Pointer;
                       DstX: Integer;
                       DstY: Integer;
                       DstXE: Integer;
                       DstYE: Integer;
                       biSrc: PBITMAPINFOHEADER;
                       lpSrc: Pointer;
                       SrcX: Integer;
                       SrcY: Integer;
                       SrcXE: Integer;
                       SrcYE: Integer); stdcall;


  //****************************************************************************
  //
  //  AVIFMT - AVI file format definitions
  //
  //****************************************************************************


  // The following is a short description of the AVI file format.  Please
  // see the accompanying documentation for a full explanation.
  //
  // An AVI file is the following RIFF form:
  //
  //	RIFF('AVI'
  //	      LIST('hdrl'
  //		    avih(<MainAVIHeader>)
  //                  LIST ('strl'
  //                      strh(<Stream header>)
  //                      strf(<Stream format>)
  //                      ... additional header data
  //            LIST('movi'
  //      	  { LIST('rec'
  //      		      SubChunk...
  //      		   )
  //      	      | SubChunk } ....
  //            )
  //            [ <AVIIndex> ]
  //      )
  //
  //	The main file header specifies how many streams are present.  For
  //	each one, there must be a stream header chunk and a stream format
  //	chunk, enlosed in a 'strl' LIST chunk.  The 'strf' chunk contains
  //	type-specific format information; for a video stream, this should
  //	be a BITMAPINFO structure, including palette.  For an audio stream,
  //	this should be a WAVEFORMAT (or PCMWAVEFORMAT) structure.
  //
  //	The actual data is contained in subchunks within the 'movi' LIST
  //	chunk.  The first two characters of each data chunk are the
  //	stream number with which that data is associated.
  //
  //	Some defined chunk types:
  //           Video Streams:
  //                  ##db:	RGB DIB bits
  //                  ##dc:	RLE8 compressed DIB bits
  //                  ##pc:	Palette Change
  //
  //           Audio Streams:
  //                  ##wb:	waveform audio bytes
  //
  // The grouping into LIST 'rec' chunks implies only that the contents of
  //   the chunk should be read into memory at the same time.  This
  //   grouping is used for files specifically intended to be played from
  //   CD-ROM.
  //
  // The index chunk at the end of the file should contain one entry for
  //   each data chunk in the file.
  //
  // Limitations for the current software:
  //	Only one video stream and one audio stream are allowed.
  //	The streams must start at the beginning of the file.
  //
  //
  // To register codec types please obtain a copy of the Multimedia
  // Developer Registration Kit from:
  //
  //  Microsoft Corporation
  //  Multimedia Systems Group
  //  Product Marketing
  //  One Microsoft Way
  //  Redmond, WA 98052-6399
  //


  // form types, list types, and chunk types

const

  formtypeAVI             = ord('A') or (ord('V') shl 8) or (ord('I') shl 16) or (ord(' ') shl 24);
  {$EXTERNALSYM formtypeAVI}
  listtypeAVIHEADER       = ord('h') or (ord('d') shl 8) or (ord('r') shl 16) or (ord('l') shl 24);
  {$EXTERNALSYM listtypeAVIHEADER}
  ckidAVIMAINHDR          = ord('a') or (ord('v') shl 8) or (ord('i') shl 16) or (ord('h') shl 24);
  {$EXTERNALSYM ckidAVIMAINHDR}
  listtypeSTREAMHEADER    = ord('s') or (ord('t') shl 8) or (ord('r') shl 16) or (ord('l') shl 24);
  {$EXTERNALSYM listtypeSTREAMHEADER}
  ckidSTREAMHEADER        = ord('s') or (ord('t') shl 8) or (ord('r') shl 16) or (ord('h') shl 24);
  {$EXTERNALSYM ckidSTREAMHEADER}
  ckidSTREAMFORMAT        = ord('s') or (ord('t') shl 8) or (ord('r') shl 16) or (ord('h') shl 24);
  {$EXTERNALSYM ckidSTREAMFORMAT}
  ckidSTREAMHANDLERDATA   = ord('s') or (ord('t') shl 8) or (ord('r') shl 16) or (ord('h') shl 24);
  {$EXTERNALSYM ckidSTREAMHANDLERDATA}
  ckidSTREAMNAME		      = ord('s') or (ord('t') shl 8) or (ord('r') shl 16) or (ord('n') shl 24);
  {$EXTERNALSYM ckidSTREAMNAME}

  listtypeAVIMOVIE        = ord('m') or (ord('o') shl 8) or (ord('v') shl 16) or (ord('i') shl 24);
  {$EXTERNALSYM listtypeAVIMOVIE}
  listtypeAVIRECORD       = ord('r') or (ord('e') shl 8) or (ord('c') shl 16) or (ord(' ') shl 24);
  {$EXTERNALSYM listtypeAVIRECORD}

  ckidAVINEWINDEX         = ord('i') or (ord('d') shl 8) or (ord('x') shl 16) or (ord('1') shl 24);
  {$EXTERNALSYM ckidAVINEWINDEX}

  //
  // Stream types for the <fccType> field of the stream header.
  //

  streamtypeVIDEO         = ord('v') or (ord('i') shl 8) or (ord('d') shl 16) or (ord('s') shl 24);
  {$EXTERNALSYM streamtypeVIDEO}
  streamtypeAUDIO         = ord('a') or (ord('u') shl 8) or (ord('d') shl 16) or (ord('s') shl 24);
  {$EXTERNALSYM streamtypeAUDIO}
  streamtypeMIDI		      = ord('m') or (ord('i') shl 8) or (ord('d') shl 16) or (ord('s') shl 24);
  {$EXTERNALSYM streamtypeMIDI}
  streamtypeTEXT          = ord('t') or (ord('x') shl 8) or (ord('t') shl 16) or (ord('s') shl 24);
  {$EXTERNALSYM streamtypeTEXT}

  // Basic chunk types

  {$EXTERNALSYM cktypeDIBbits}
  cktypeDIBbits           = ord('d') or ((ord('b') shl 8));
  {$EXTERNALSYM cktypeDIBcompressed}
  cktypeDIBcompressed     = ord('d') or ((ord('c') shl 8));
  {$EXTERNALSYM cktypePALchange}
  cktypePALchange         = ord('p') or ((ord('c') shl 8));
  {$EXTERNALSYM cktypeWAVEbytes}
  cktypeWAVEbytes         = ord('w') or ((ord('b') shl 8));

  // Chunk id to use for extra chunks for padding.

  ckidAVIPADDING          = ord('J') or (ord('U') shl 8) or (ord('N') shl 16) or (ord('K') shl 24);
  {$EXTERNALSYM ckidAVIPADDING}

  //
  // Useful macros
  //
  // Warning: These are nasty macro, and MS C 6.0 compiles some of them
  // incorrectly if optimizations are on.  Ack.
  //

  // Macro to get stream number out of a FOURCC ckid

  function FromHex(n: Byte): Byte;
  {$EXTERNALSYM FromHex}

  function StreamFromFOURCC(fcc: FOURCC): Byte;
  {$EXTERNALSYM StreamFromFOURCC}

  // Macro to get TWOCC chunk type out of a FOURCC ckid

  function TWOCCFromFOURCC(fcc: FOURCC): Word;
  {$EXTERNALSYM TWOCCFromFOURCC}


  // Macro to make a ckid for a chunk out of a TWOCC and a stream number
  // from 0-255.
  //

  function ToHex(n: Byte): Byte;
  {$EXTERNALSYM ToHex}

  function MAKEAVICKID(tcc: TWOCC;
                       stream: Byte): DWord;
  {$EXTERNALSYM MAKEAVICKID}


  //
  // Main AVI File Header
  //

  // flags for use in <dwFlags> in AVIFileHdr

const

  {$EXTERNALSYM AVIF_HASINDEX}
  AVIF_HASINDEX                       = $00000010;  // Index at end of file?
  AVIF_MUSTUSEINDEX                   = $00000020;
  {$EXTERNALSYM AVIF_MUSTUSEINDEX}
  AVIF_ISINTERLEAVED                  = $00000100;
  {$EXTERNALSYM AVIF_ISINTERLEAVED}
  AVIF_WASCAPTUREFILE                 = $00010000;
  {$EXTERNALSYM AVIF_WASCAPTUREFILE}
  AVIF_COPYRIGHTED                    = $00020000;
  {$EXTERNALSYM AVIF_COPYRIGHTED}

  // The AVI File Header LIST chunk should be padded to this size
  AVI_HEADERSIZE                      = 2048;  // size of AVI header list
  {$EXTERNALSYM AVI_HEADERSIZE}

type

  PMainAVIHeader = ^MainAVIHeader;
  MainAVIHeader = record
    dwMicroSecPerFrame: DWORD;       // frame display rate (or 0L)
    dwMaxBytesPerSec: DWORD;         // max. transfer rate
    dwPaddingGranularity: DWORD;     // pad to multiples of this
                                     // size; normally 2K.
    dwFlags: DWORD;                  // the ever-present flags
    dwTotalFrames: DWORD;            // # frames in file
    dwInitialFrames: DWORD;
    dwStreams: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwReserved: array[0..3] of DWORD;
  end;
  {$EXTERNALSYM MainAVIHeader}


  //
  // Stream header
  //

const

  AVISF_DISABLED                      = $00000001;
  {$EXTERNALSYM AVISF_DISABLED}

  AVISF_VIDEO_PALCHANGES              = $00010000;
  {$EXTERNALSYM AVISF_VIDEO_PALCHANGES}

type

  PAVIStreamHeader = ^AVIStreamHeader;
  AVIStreamHeader = record
    fccType: FOURCC;
    fccHandler: FOURCC;
    dwFlags: DWORD;                  { Contains AVITF_* flags }
    wPriority: WORD;
    wLanguage: WORD;
    dwInitialFrames: DWORD;
    dwScale: DWORD;
    dwRate: DWORD;                   { dwRate / dwScale == samples/second }
    dwStart: DWORD;
    dwLength: DWORD;                 { In units above... }
    dwSuggestedBufferSize: DWORD;
    dwQuality: DWORD;
    dwSampleSize: DWORD;
    rcFrame: TRect;
  end;
  {$EXTERNALSYM AVIStreamHeader}

  // Flags for index

const

  AVIIF_LIST                          = $00000001;  // chunk is a 'LIST'
  {$EXTERNALSYM AVIIF_LIST}
  AVIIF_TWOCC                         = $00000002;
  {$EXTERNALSYM AVIIF_TWOCC}
  AVIIF_KEYFRAME                      = $00000010;  // this frame is a key frame.
  {$EXTERNALSYM AVIIF_KEYFRAME}
  AVIIF_FIRSTPART                     = $00000020;  // this frame is the start of a partial frame.
  {$EXTERNALSYM AVIIF_FIRSTPART}
  AVIIF_LASTPART                      = $00000040;  // this frame is the end of a partial frame.
  {$EXTERNALSYM AVIIF_LASTPART}
  AVIIF_MIDPART                       = (AVIIF_LASTPART or AVIIF_FIRSTPART);
  {$EXTERNALSYM AVIIF_MIDPART}

  AVIIF_NOTIME                        = $00000100;  // this frame doesn't take any time
  {$EXTERNALSYM AVIIF_NOTIME}
  AVIIF_COMPUSE                       = $0FFF0000;  // these bits are for compressor use
  {$EXTERNALSYM AVIIF_COMPUSE}

type

  PAVIINDEXENTRY = ^AVIINDEXENTRY;
  AVIINDEXENTRY = record
    ckid: DWORD;
    dwFlags: DWORD;
    dwChunkOffset: DWORD;            // Position of chunk
    dwChunkLength: DWORD;           // Length of chunk
  end;
  {$EXTERNALSYM AVIINDEXENTRY}


  //
  // Palette change chunk
  //
  // Used in video streams.
  //

type

  PAVIPALCHANGE = ^AVIPALCHANGE;
  AVIPALCHANGE = record
    bFirstEntry: Byte;                       { first entry to change }
    bNumEntries: Byte;                       { # entries to change (0 if 256) }
    wFlags: WORD;                            { Mostly to preserve alignment... }
    peNew: array[0..0] of PALETTEENTRY;      { New color specifications }
  end;
  {$EXTERNALSYM AVIPALCHANGE}



  //****************************************************************************
  //
  //  AVIFile - routines for reading/writing standard AVI files
  //
  //*****************************************************************************


  //
  // Ansi - Unicode thunking.
  //
  // Unicode or Ansi-only apps can call the avifile APIs.
  // any Win32 app who wants to use
  // any of the AVI COM interfaces must be UNICODE - the AVISTREAMINFO and
  // AVIFILEINFO structures used in the Info methods of these interfaces are
  // the unicode variants, and no thunking to or from ansi takes place
  // except in the AVIFILE api entrypoints.
  //
  // For Ansi/Unicode thunking: for each entrypoint or structure that
  // uses chars or strings, two versions are declared in the Win32 version,
  // ApiNameW and ApiNameA. The default name ApiName is #defined to one or
  // other of these depending on whether UNICODE is defined (during
  // compilation of the app that is including this header). The source will
  // contain ApiName and ApiNameA (with ApiName being the Win16 implementation,
  // and also #defined to ApiNameW, and ApiNameA being the thunk entrypoint).
  //

  // For GetFrame.SetFormat - use the best format for the display

const

  AVIGETFRAMEF_BESTDISPLAYFMT         = 1;
  {$EXTERNALSYM AVIGETFRAMEF_BESTDISPLAYFMT}

  //
  // Structures used by AVIStreamInfo & AVIFileInfo.
  //
  // These are related to, but not identical to, the header chunks
  // in an AVI file.
  //

  //
  //
  // --- AVISTREAMINFO ------------------------------------------------
  //
  // for Unicode/Ansi thunking we need to declare three versions of this!
  //

type

  PAVISTREAMINFOW = ^_AVISTREAMINFOW;
  _AVISTREAMINFOW = record
    fccType: DWORD;
    fccHandler: DWORD;
    dwFlags: DWORD;                  { Contains AVITF_* flags }
    dwCaps: DWORD;
    wPriority: WORD;
    wLanguage: WORD;
    dwScale: DWORD;
    dwRate: DWORD;                   { dwRate / dwScale == samples/second }
    dwStart: DWORD;
    dwLength: DWORD;                 { In units above... }
    dwInitialFrames: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwQuality: DWORD;
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount: DWORD;
    dwFormatChangeCount: DWORD;
    szName: array[0..63] of WideChar;
  end;
  {$EXTERNALSYM _AVISTREAMINFOW}
  LPAVISTREAMINFOW = ^_AVISTREAMINFOW;
  {$EXTERNALSYM LPAVISTREAMINFOW}

  PAVISTREAMINFOA = ^_AVISTREAMINFOA;
  _AVISTREAMINFOA = record
    fccType: DWORD;
    fccHandler: DWORD;
    dwFlags: DWORD;                  { Contains AVITF_* flags }
    dwCaps: DWORD;
    wPriority: WORD;
    wLanguage: WORD;
    dwScale: DWORD;
    dwRate: DWORD;                   { dwRate / dwScale == samples/second }
    dwStart: DWORD;
    dwLength: DWORD;                 { In units above... }
    dwInitialFrames: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwQuality: DWORD;
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount: DWORD;
    dwFormatChangeCount: DWORD;
    szName: array[0..63] of AnsiChar;
  end;
  {$EXTERNALSYM _AVISTREAMINFOA}
  LPAVISTREAMINFOA = ^_AVISTREAMINFOA;
  {$EXTERNALSYM LPAVISTREAMINFOA}

  { unicode }

{$IFDEF UNICODE}
  TAVISTREAMINFO = _AVISTREAMINFOW;
  LPAVISTREAMINFO = ^_AVISTREAMINFOW;
  {$EXTERNALSYM LPAVISTREAMINFO}
{$ELSE}
  TAVISTREAMINFO = _AVISTREAMINFOA;
  LPAVISTREAMINFO = ^_AVISTREAMINFOA;
  {$EXTERNALSYM LPAVISTREAMINFO}
{$ENDIF}

const

  AVISTREAMINFO_DISABLED              = $00000001;
  AVISTREAMINFO_FORMATCHANGES         = $00010000;


  //
  // --- AVIFILEINFO ----------------------------------------------------
  //
  //

type
  PAVIFILEINFOW = ^_AVIFILEINFOW;
  _AVIFILEINFOW = record
    dwMaxBytesPerSec: DWORD;                // max. transfer rate
    dwFlags: DWORD;                         // the ever-present flags
    dwCaps: DWORD;
    dwStreams: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwScale: DWORD;
    dwRate: DWORD;                          { dwRate / dwScale == samples/second }
    dwLength: DWORD;
    dwEditCount: DWORD;
    szFileType: array[0..63] of WideChar;  // descriptive string for file type?
  end;
  {$EXTERNALSYM _AVIFILEINFOW}
  LPAVIFILEINFOW = ^_AVIFILEINFOW;
  {$EXTERNALSYM LPAVIFILEINFOW}

  _AVIFILEINFOA = record
    dwMaxBytesPerSec: DWORD;                // max. transfer rate
    dwFlags: DWORD;                         // the ever-present flags
    dwCaps: DWORD;
    dwStreams: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwScale: DWORD;
    dwRate: DWORD;                          { dwRate / dwScale == samples/second }
    dwLength: DWORD;
    dwEditCount: DWORD;
    szFileType: array[0..63] of AnsiChar;  // descriptive string for file type?
  end;
  {$EXTERNALSYM _AVIFILEINFOA}
  LPAVIFILEINFOA = ^_AVIFILEINFOA;
  {$EXTERNALSYM LPAVIFILEINFOA}

  { unicode }
{$IFDEF UNICODE}
  TAVIFILEINFO = _AVIFILEINFOW;
  LPAVIFILEINFO = ^_AVIFILEINFOW;
{$ELSE}
  TAVIFILEINFO = _AVIFILEINFOA;
  LPAVIFILEINFO = ^_AVIFILEINFOA;
{$ENDIF}

  // Flags for dwFlags

const

  AVIFILEINFO_HASINDEX                = $00000010;
  {$EXTERNALSYM AVIFILEINFO_HASINDEX}
  AVIFILEINFO_MUSTUSEINDEX            = $00000020;
  {$EXTERNALSYM AVIFILEINFO_MUSTUSEINDEX}
  AVIFILEINFO_ISINTERLEAVED           = $00000100;
  {$EXTERNALSYM AVIFILEINFO_ISINTERLEAVED}
  AVIFILEINFO_WASCAPTUREFILE          = $00010000;
  {$EXTERNALSYM AVIFILEINFO_WASCAPTUREFILE}
  AVIFILEINFO_COPYRIGHTED             = $00020000;
  {$EXTERNALSYM AVIFILEINFO_COPYRIGHTED}

  // Flags for dwCaps

  AVIFILECAPS_CANREAD                 = $00000001;
  {$EXTERNALSYM AVIFILECAPS_CANREAD}
  AVIFILECAPS_CANWRITE                = $00000002;
  {$EXTERNALSYM AVIFILECAPS_CANWRITE}
  AVIFILECAPS_ALLKEYFRAMES            = $00000010;
  {$EXTERNALSYM AVIFILECAPS_ALLKEYFRAMES}
  AVIFILECAPS_NOCOMPRESSION           = $00000020;
  {$EXTERNALSYM AVIFILECAPS_NOCOMPRESSION}

type

  AVISAVECALLBACK = function(arg1: Integer): BOOL; pascal;

  //************************************************************************/
  // Declaration for the AVICOMPRESSOPTIONS structure.  Make sure it 	     */
  // matches the AutoDoc in avisave.c !!!                            	     */
  //************************************************************************/

  PAVICOMPRESSOPTIONS = ^AVICOMPRESSOPTIONS;
  AVICOMPRESSOPTIONS = record
    fccType: DWORD;                  { stream type, for consistency }
    fccHandler: DWORD;               { compressor }
    dwKeyFrameEvery: DWORD;          { keyframe rate }
    dwQuality: DWORD;                { compress quality 0-10,000 }
    dwBytesPerSecond: DWORD;         { bytes per second }
    dwFlags: DWORD;                  { flags... see below }
    lpFormat: Pointer;               { save format }
    cbFormat: DWORD;
    lpParms: Pointer;                { compressor options }
    cbParms: DWORD;
    dwInterleaveEvery: DWORD;       { for non-video streams only }
  end;
  {$EXTERNALSYM AVICOMPRESSOPTIONS}
  LPAVICOMPRESSOPTIONS = ^AVICOMPRESSOPTIONS;
  {$EXTERNALSYM LPAVICOMPRESSOPTIONS}

  //
  // Defines for the dwFlags field of the AVICOMPRESSOPTIONS struct
  // Each of these flags determines if the appropriate field in the structure
  // (dwInterleaveEvery, dwBytesPerSecond, and dwKeyFrameEvery) is payed
  // attention to.  See the autodoc in avisave.c for details.
  //

const

  AVICOMPRESSF_INTERLEAVE             = $00000001;  // interleave
  {$EXTERNALSYM AVICOMPRESSF_INTERLEAVE}
  AVICOMPRESSF_DATARATE               = $00000002;  // use a data rate
  {$EXTERNALSYM AVICOMPRESSF_DATARATE}
  AVICOMPRESSF_KEYFRAMES              = $00000004;  // use keyframes
  {$EXTERNALSYM AVICOMPRESSF_KEYFRAMES}
  AVICOMPRESSF_VALID                  = $00000008;  // has valid data?
  {$EXTERNALSYM AVICOMPRESSF_VALID}

type

  //****** AVI Stream Interface *******************************************

  PAVIStream = ^IAVIStream;
  {$EXTERNALSYM PAVISTREAM}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAVIStream);'}
  {$EXTERNALSYM IAVIStream}
  IAVIStream = interface(IUnknown)
  ['{00020021-0000-0000-0000-C00000000046}']
    // *** IAVIStream methods ***
    function Create(lParam1: LPARAM;
                    lParam2: LPARAM): HResult; stdcall;

    function Info(var psi: TAVISTREAMINFO;
                  lSize: LONG): HResult; stdcall;

    function FindSample(lPos: LONG;
                        lFlags: LONG): LONG; stdcall;

    function ReadFormat(lPos: LONG;
			                  {_Out_} lpFormat: Pointer;
                        var lpcbFormat: LONG): HResult; stdcall;

    function SetFormat(lPos: LONG;
			                 lpFormat: Pointer;
                       cbFormat: LONG): HResult; stdcall;

    function Read(lStart: LONG;
                  lSamples: LONG;
			            {_Out_} lpBuffer: Pointer;
                  cbBuffer: LONG;
			            var plBytes: LONG;
                  var plSamples: LONG): HResult; stdcall;

    function Write(lStart: LONG;
                   lSamples: LONG;
			             lpBuffer: Pointer;
                   cbBuffer: LONG;
			             dwFlags: DWORD;
			             var plSampWritten: LONG;
			             var plBytesWritten: LONG): HResult; stdcall;

    function Delete(lStart: LONG;
                    lSamples: LONG): HResult; stdcall;

    function ReadData(fcc: DWORD;
                      {_Out_} lp: Pointer;
                      var lpcb: LONG): HResult; stdcall;

    function WriteData(fcc: DWORD;
                       lp: Pointer;
                       cb: LONG): HResult; stdcall;

    function SetInfo(lpInfo: TAVISTREAMINFO;
			               cbInfo: LONG): HResult; stdcall;

  end;
  IID_IAVIStream = IAVIStream;

  PAVISTREAMING = ^IAVIStreaming;
  {$EXTERNALSYM PAVISTREAMING}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAVIStreaming);'}
  {$EXTERNALSYM IAVIStreaming}
  IAVIStreaming = interface(IUnknown)
  ['{00020022-0000-0000-0000-C00000000046}']
    // *** IAVIStreaming methods ***
    function _Begin(lStart: LONG; 	    // start of what we expect
						                            // to play
		                lEnd: LONG; 		    // expected end, or -1
		                lRate: LONG         // Should this be a float?
                    ): HResult; stdcall;

    function _End(): HResult; stdcall;

  end;
  IID_IAVIStreaming = IAVIStreaming;
  {$EXTERNALSYM IID_IAVIStreaming}


  PAVIEDITSTREAM = ^IAVIEditStream;
  {$EXTERNALSYM PAVIEDITSTREAM}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAVIEditStream);'}
  {$EXTERNALSYM IAVIEditStream}
  IAVIEditStream = interface(IUnknown)
  ['{00020024-0000-0000-0000-C00000000046}']
    // *** IAVIEditStream methods ***
    function Cut(var plStart: LONG;
			           var plLength: LONG;
			           {_Outptr_} ppResult: PAVIStream): HResult; stdcall;

    function Copy(var plStart: LONG;
			            var plLength: LONG;
			            out ppResult: PAVIStream): HResult; stdcall;

    function Paste(var plPos: LONG;
			             var plLength: LONG;
			             var pstream: IAVIStream;
			             lStart: LONG;
			             lEnd: LONG): HResult; stdcall;

    function Clone({_Outptr_} ppResult: PAVIStream): HResult; stdcall;

    function SetInfo(lpInfo: TAVISTREAMINFO;
			               cbInfo: LONG): HResult; stdcall;

  end;
  IID_IAVIEditStream = IAVIEditStream;
  {$EXTERNALSYM IID_IAVIEditStream}


  //****** AVI File Interface *******************************************

//DECLARE_INTERFACE_(IAVIPersistFile, IPersistFile)
//{
//    STDMETHOD(Reserved1)(THIS) PURE;
//};

//typedef IAVIPersistFile FAR* PAVIPERSISTFILE;


  PAVIFILE = ^IAVIFile;
  {$EXTERNALSYM PAVIFILE}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAVIFile);'}
  {$EXTERNALSYM IAVIFile}
  IAVIFile = interface(IUnknown)
  ['{00020020-0000-0000-0000-C00000000046}']
    // *** IAVIFile methods ***
    function Info(pfi: TAVIFILEINFO;
                  lSize: LONG): HResult; stdcall;

    function GetStream({_Outptr_} ppStream: PAVIStream;
				               fccType: DWORD;
                       lParam: LONG): HResult; stdcall;

    function CreateStream({_Outptr_} ppStream: PAVIStream;
                          psi: TAVISTREAMINFO): HResult; stdcall;

    function WriteData(ckid: DWORD;
                       lpData: Pointer;
                       cbData: LONG): HResult; stdcall;

    function ReadData(ckid: DWORD;
                      lpData: Pointer;
                      var lpcbData: LONG): HResult; stdcall;

    function EndRecord(): HResult; stdcall;

    function DeleteStream(fccType: DWORD;
                          lParam: LONG): HResult; stdcall;

  end;
  IID_IAVIFile = IAVIFile;
  {$EXTERNALSYM IID_IAVIFile}

  //****** GetFrame Interface *******************************************


  PGETFRAME = ^IGetFrame;
  {$EXTERNALSYM PGETFRAME}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGetFrame);'}
  {$EXTERNALSYM IGetFrame}
  IGetFrame = interface(IUnknown)
  ['{00020023-0000-0000-0000-C00000000046}']
    // *** IGetFrame methods ***

    function GetFrame(lPos: LONG): PBitmapInfoHeader; stdcall; // See: AVIStreamGetFrame method and
                                                               //      https://docs.microsoft.com/en-us/windows/win32/api/vfw/nf-vfw-igetframe-getframe

    //  STDMETHOD_(LPVOID,GetFrameData) (THIS_ _In_ LONG lPos) PURE;   deprecated

    function _Begin(lStart: LONG;
                    lEnd: LONG;
                    lRate: LONG): HResult; stdcall;

    function _End(): HResult; stdcall;

    function SetFormat(lpbi: PBITMAPINFOHEADER;
                      {_In_opt_} lpBits: Pointer;
                      x: Integer;
                      y: Integer;
                      dx: Integer;
                      dy: Integer): HResult; stdcall;

//  STDMETHOD(DrawFrameStart) (THIS) PURE;  deprecated
//  STDMETHOD(DrawFrame) (THIS_ _In_ LONG lPos, HDC hdc, _In_ int x, _In_ int y, _In_ int dx, _In_ int dy) PURE; deprecated
//  STDMETHOD(DrawFrameEnd) (THIS) PURE; deprecated
  end;
  IID_IGetFrame = IGetFrame;
  {$EXTERNALSYM IID_IGetFrame}



  //****** GUIDs *******************************************

const

  CLSID_AVIFile: TGUID = '{00020000-0000-0000-0000-C00000000046}';

{$IFNDEF UNICODE}
  CLSID_AVISimpleUnMarshal: TGUID = '{00020009-0000-0000-0000-C00000000046}';
  {$EXTERNALSYM CLSID_AVISimpleUnMarshal}
{$ENDIF}

  AVIFILEHANDLER_CANREAD              = $0001;
  {$EXTERNALSYM AVIFILEHANDLER_CANREAD}
  AVIFILEHANDLER_CANWRITE             = $0002;
  {$EXTERNALSYM AVIFILEHANDLER_CANWRITE}
  AVIFILEHANDLER_CANACCEPTNONRGB      = $0004;
  {$EXTERNALSYM AVIFILEHANDLER_CANACCEPTNONRGB}


  //
  // functions
  //

  procedure AVIFileInit(); stdcall;  // Call this first!
  {$EXTERNALSYM AVIFileInit}

  procedure AVIFileExit(); stdcall;
  {$EXTERNALSYM AVIFileExit}

  function AVIFileAddRef(pfile: IAVIFile): DWord; stdcall;
  {$EXTERNALSYM AVIFileAddRef}

  function AVIFileRelease(pfile: IAVIFile): DWord; stdcall;
  {$EXTERNALSYM AVIFileRelease}

  function AVIFileOpenW({_Outptr_} ppfile: PAVIFILE;
                        szFile: PWideChar;
			                  uMode: UINT;
                        {_In_opt_} lpHandler: LPCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIFileOpenW}

  function AVIFileOpenA({_Outptr_} ppfile: PAVIFILE;
                        szFile: PAnsiChar;
			                  uMode: UINT;
                        {_In_opt_} lpHandler: LPCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIFileOpenA}

  { unicode }
  function AVIFileOpen({_Outptr_} ppfile: PAVIFILE;
                       szFile: PWideChar;
			                 uMode: UINT;
                       {_In_opt_} lpHandler: LPCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIFileOpen}


  function AVIFileInfoW(pfile: IAVIFile; // The argument pfile is a pointer to an IAVIFile interface.
                        out pfi: _AVIFILEINFOW;
                        lSize: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIFileInfoW}

  function AVIFileInfoA(pfile: IAVIFile;
                        out pfi: _AVIFILEINFOA;
                        lSize: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIFileInfoA}


  { unicode }
  function AVIFileInfo(pfile: IAVIFile;
                       out pfi: TAVIFILEINFO;
                       lSize: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIFileInfo}


  function AVIFileGetStream(pfile: IAVIFile;
                            {_Outptr_} ppavi: PAVIStream;
                            fccType: DWORD;
                            _lParam: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIFileGetStream}

{$IFDEF UNICODE}
  function AVIFileCreateStreamW(pfile: IAVIFile;
                                {_Outptr_} ppavi: PAVIStream;
                                psi: _AVISTREAMINFOW): HResult; stdcall;
  {$EXTERNALSYM AVIFileCreateStreamW}
{$ELSE}
  function AVIFileCreateStreamA(pfile: IAVIFile;
                                {_Outptr_} ppavi: PAVIStream;
                                psi: AVISTREAMINFOA): HResult; stdcall;
  {$EXTERNALSYM AVIFileCreateStreamA}
{$ENDIF}

  { unicode }
  function AVIFileCreateStream(pfile: IAVIFile;
                               {_Outptr_} ppavi: PAVIStream;
                               psi: TAVISTREAMINFO): HResult; stdcall;
  {$EXTERNALSYM AVIFileCreateStream}


  function AVIFileWriteData(pfile: IAVIFile;
					                  ckid: DWORD;
					                  lpData: Pointer;
					                  cbData: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIFileWriteData}

  function AVIFileReadData(pfile: IAVIFile;
					                 ckid: DWORD;
					                 {_Out_} lpData: Pointer;
					                 {_Inout_} lpcbData: PLONG): HResult; stdcall;
  {$EXTERNALSYM AVIFileReadData}

  function AVIFileEndRecord(pfile: IAVIFile): HResult; stdcall;
  {$EXTERNALSYM AVIFileEndRecord}

  // Stream

  function AVIStreamAddRef(pavi: IAVIStream): DWord; stdcall;
  {$EXTERNALSYM AVIStreamAddRef}

  function AVIStreamRelease(pavi: IAVIStream): DWord; stdcall;
  {$EXTERNALSYM AVIStreamRelease}

{$IFDEF UNICODE}
  function AVIStreamInfoW(pavi: IAVIStream;
                          {_Out_} psi: _AVISTREAMINFOW;
                          lSize: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamInfoW}
{$ELSE}
  function AVIStreamInfoA(pavi: IAVIStream;
                          {_Out_} psi: _AVISTREAMINFOA;
                          lSize: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamInfoA}
{$ENDIF}

  { unicode }
  function AVIStreamInfo(pavi: IAVIStream;
                         {_Out_} psi: TAVISTREAMINFO;
                         lSize: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamInfo}


  function AVIStreamFindSample(pavi: IAVIStream;
                               lPos: LONG;
                               lFlags: LONG): LONG; stdcall;
  {$EXTERNALSYM AVIStreamFindSample}

  function AVIStreamReadFormat(pavi: IAVIStream;
                               lPos: LONG;
                               {_Out_} lpFormat: Pointer;
                               {_Inout_} lpcbFormat: PLONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamReadFormat}

  function AVIStreamSetFormat(pavi: IAVIStream;
                              lPos: LONG;
                              lpFormat: Pointer;
                              cbFormat: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamSetFormat}

  function AVIStreamReadData(pavi: IAVIStream;
                             fcc: DWORD;
                             {_Out_} lp: Pointer;
                             {_Inout_} lpcb: PLONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamReadData}

  function AVIStreamWriteData(pavi: IAVIStream;
                              fcc: DWORD;
                              lp: Pointer;
                              cb: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamWriteData}

  function AVIStreamRead(pavi: IAVIStream;
			                   lStart: LONG;
			                   lSamples: LONG;
			                   {_Out_} lpBuffer: Pointer;
			                   cbBuffer: LONG;
			                   {_Out_opt_} plBytes: PLONG;
			                   {_Out_opt_} plSamples: PLONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamRead}

const

  AVISTREAMREAD_CONVENIENT            = (- 1);
  {$EXTERNALSYM AVISTREAMREAD_CONVENIENT}

  function AVIStreamWrite(pavi: IAVIStream;
			                    lStart: LONG;
                          lSamples: LONG;
			                    lpBuffer: Pointer;
                          cbBuffer: LONG;
                          dwFlags: DWORD;
			                    var plSampWritten: LONG;
			                    var plBytesWritten: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamWrite}

  // Right now, these just use AVIStreamInfo() to get information, then
  // return some of it.  Can they be more efficient?
  function AVIStreamStart(pavi: IAVIStream): LONG; stdcall;
  {$EXTERNALSYM AVIStreamStart}

  function AVIStreamLength(pavi: IAVIStream): LONG; stdcall;
  {$EXTERNALSYM AVIStreamLength}

  function AVIStreamTimeToSample(pavi: IAVIStream;
                                 lTime: LONG): LONG; stdcall;
  {$EXTERNALSYM AVIStreamTimeToSample}

  function AVIStreamSampleToTime(pavi: IAVIStream;
                                 lSample: LONG): LONG; stdcall;
  {$EXTERNALSYM AVIStreamSampleToTime}


  function AVIStreamBeginStreaming(pavi: IAVIStream;
                                   lStart: LONG;
                                   lEnd: LONG;
                                   lRate: LONG): HResult; stdcall;
  {$EXTERNALSYM AVIStreamBeginStreaming}

  function AVIStreamEndStreaming(pavi: IAVIStream): HResult; stdcall;
  {$EXTERNALSYM AVIStreamEndStreaming}

  //
  // helper functions for using IGetFrame
  //

  function AVIStreamGetFrameOpen(pavi: IAVIStream;
					                       {_In_opt_} lpbiWanted: PBITMAPINFOHEADER): PGETFRAME; stdcall;
  {$EXTERNALSYM AVIStreamGetFrameOpen}

  function AVIStreamGetFrame(pg: IGetFrame;
                             lPos: LONG): Pointer; stdcall;
  {$EXTERNALSYM AVIStreamGetFrame}

  function AVIStreamGetFrameClose(pg: IGetFrame): HResult; stdcall;
  {$EXTERNALSYM AVIStreamGetFrameClose}


  // !!! We need some way to place an advise on a stream....
  // function AVIStreamHasChanged(pavi: AVISTREAM)..; ..



  // Shortcut function

{$IFDEF UNICODE}
  function AVIStreamOpenFromFileW({_Outptr_} ppavi: PAVIStream;
                                  szFile: PWideChar;
			                            fccType: DWORD;
                                  lParam: LONG;
			                            mode: UINT;
                                  {_In_opt_} pclsidHandler: PCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIStreamOpenFromFileW}
{$ELSE}
  function AVIStreamOpenFromFileA({_Outptr_} ppavi: PAVIStream;
                                  szFile: PAnsiChar;
			                            fccType: DWORD;
                                  lParam: LONG;
			                            mode: UINT;
                                  {_In_opt_} pclsidHandler: PCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIStreamOpenFromFileA}
{$ENDIF}

  { unicode }
  function AVIStreamOpenFromFile({_Outptr_} ppavi: PAVIStream;
                                 szFile: PWideChar;
			                           fccType: DWORD;
                                 lParam: LONG;
			                           mode: UINT;
                                 {_In_opt_} pclsidHandler: PCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIStreamOpenFromFile}

  // Use to create disembodied streams
  function AVIStreamCreate({_Outptr_} ppavi: PAVIStream;
                           lParam1: LONG;
                           lParam2: LONG;
		                       {_In_opt_} pclsidHandler: PCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIStreamCreate}



  // PHANDLER    AVIAPI AVIGetHandler         (PAVISTREAM pavi, PAVISTREAMHANDLER psh);
  // PAVISTREAM  AVIAPI AVIGetStream          (PHANDLER p);

  //
  // flags for AVIStreamFindSample
  //

const

  FIND_DIR                            = $0000000F;  // direction
  {$EXTERNALSYM FIND_DIR}
  FIND_NEXT                           = $00000001;  // go forward
  {$EXTERNALSYM FIND_NEXT}
  FIND_PREV                           = $00000004;  // go backward
  {$EXTERNALSYM FIND_PREV}
  FIND_FROM_START                     = $00000008;  // start at the logical beginning
  {$EXTERNALSYM FIND_FROM_START}

  FIND_TYPE                           = $000000F0;  // type mask
  {$EXTERNALSYM FIND_TYPE}
  FIND_KEY                            = $00000010;  // find key frame.
  {$EXTERNALSYM FIND_KEY}
  FIND_ANY                            = $00000020;  // find any (non-empty) sample
  {$EXTERNALSYM FIND_ANY}
  FIND_FORMAT                         = $00000040;  // find format change
  {$EXTERNALSYM FIND_FORMAT}

  FIND_RET                            = $0000F000;  // return mask
  {$EXTERNALSYM FIND_RET}
  FIND_POS                            = $00000000;  // return logical position
  {$EXTERNALSYM FIND_POS}
  FIND_LENGTH                         = $00001000;  // return logical size
  {$EXTERNALSYM FIND_LENGTH}
  FIND_OFFSET                         = $00002000;  // return physical position
  {$EXTERNALSYM FIND_OFFSET}
  FIND_SIZE                           = $00003000;  // return physical size
  {$EXTERNALSYM FIND_SIZE}
  FIND_INDEX                          = $00004000;  // return physical index position
  {$EXTERNALSYM FIND_INDEX}


  //
  //  stuff to support backward compat.
  //


  function AVIStreamFindKeyFrame(pavi: IAVIStream;
                                 lPos: LONG;
                                 lFlags: LONG): LONG; stdcall;  // = function AVIStreamFindSample
  {$EXTERNALSYM AVIStreamFindKeyFrame}

  function AVIStreamClose(pavi: IAVIStream): DWord; stdcall; // = function AVIStreamRelease
  {$EXTERNALSYM AVIStreamClose}

  function AVIFileClose(pavi: IAVIStream): DWord; stdcall; // = function AVIFileRelease
  {$EXTERNALSYM AVIFileClose}

  procedure AVIStreamInit(); stdcall;  // Call this first!
                                       // = procedure AVIFileInit
  {$EXTERNALSYM AVIStreamInit}

  procedure AVIStreamExit(); stdcall;  // = procedure AVIFileExit
  {$EXTERNALSYM AVIStreamExit}

 const

  SEARCH_NEAREST                      = FIND_PREV;
  {$EXTERNALSYM SEARCH_NEAREST}
  SEARCH_BACKWARD                     = FIND_PREV;
  {$EXTERNALSYM SEARCH_BACKWARD}
  SEARCH_FORWARD                      = FIND_NEXT;
  {$EXTERNALSYM SEARCH_FORWARD}
  SEARCH_KEY                          = FIND_KEY;
  {$EXTERNALSYM SEARCH_KEY}
  SEARCH_ANY                          = FIND_ANY;
  {$EXTERNALSYM SEARCH_ANY}

  //
  //  helper macros.
  //

  {$EXTERNALSYM AVIStreamSampleToSample}
  function AVIStreamSampleToSample(pavi1: IAVIStream;
                                   pavi2: IAVIStream;
                                   l: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNextSample}
  function AVIStreamNextSample(pavi: IAVIStream;
                               l: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamPrevSample}
  function AVIStreamPrevSample(pavi: IAVIStream;
                               l: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNearestSample}
  function AVIStreamNearestSample(pavi: IAVIStream;
                                  l: LONG):
                                  LONG; inline;

  {$EXTERNALSYM AVIStreamNextKeyFrame}
  function AVIStreamNextKeyFrame(pavi: IAVIStream;
                                 l: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamPrevKeyFrame}
  function AVIStreamPrevKeyFrame(pavi: IAVIStream;
                                 l: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNearestKeyFrame}
  function AVIStreamNearestKeyFrame(pavi: IAVIStream;
                                    l: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamIsKeyFrame}
  function AVIStreamIsKeyFrame(pavi: IAVIStream;
                               l: LONG): BOOL; inline;

  {$EXTERNALSYM AVIStreamPrevSampleTime}
  function AVIStreamPrevSampleTime(pavi: IAVIStream;
                                   t: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNextSampleTime}
  function AVIStreamNextSampleTime(pavi: IAVIStream;
                                   t: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNearestSampleTime}
  function AVIStreamNearestSampleTime(pavi: IAVIStream;
                                      t: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNextKeyFrameTime}
  function AVIStreamNextKeyFrameTime(pavi: IAVIStream;
                                     t: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamPrevKeyFrameTime}
  function AVIStreamPrevKeyFrameTime(pavi: IAVIStream;
                                     t: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamNearestKeyFrameTime}
  function AVIStreamNearestKeyFrameTime(pavi: IAVIStream;
                                        t: LONG): LONG; inline;

  {$EXTERNALSYM AVIStreamStartTime}
  function AVIStreamStartTime(pavi: IAVIStream): LONG; inline;

  {$EXTERNALSYM AVIStreamLengthTime}
  function AVIStreamLengthTime(pavi: IAVIStream): LONG; inline;

  {$EXTERNALSYM AVIStreamEnd}
  function AVIStreamEnd(pavi: IAVIStream): LONG; inline;

  {$EXTERNALSYM AVIStreamEndTime}
  function AVIStreamEndTime(pavi: IAVIStream): LONG; inline;

  {$EXTERNALSYM AVIStreamSampleSize}
  function AVIStreamSampleSize(pavi: IAVIStream;
                               lPos: LONG;
                               plSize: PLONG): LONG; inline;

  {$EXTERNALSYM AVIStreamFormatSize}
  function AVIStreamFormatSize(pavi: IAVIStream;
                               lPos: LONG;
                               plSize: PLONG): LONG; inline;

  {$EXTERNALSYM AVIStreamDataSize}
  function AVIStreamDataSize(pavi: IAVIStream;
                             fcc: LONG;
                             plSize: PLONG): LONG; inline;


  //****************************************************************************
  //
  //  AVISave routines and structures
  //
  //****************************************************************************

const

  comptypeDIB  = ord('D') or (ord('I') shl 8) or (ord('B') shl 16) or (ord(' ') shl 24);
  {$EXTERNALSYM comptypeDIB}


  function AVIMakeCompressedStream({_Outptr_} ppsCompressed: PAVIStream;
		                               ppsSource: PAVISTREAM;
		                               lpOptions: PAVICOMPRESSOPTIONS;
		                               {_In_opt_} pclsidHandler: PCLSID): HResult; stdcall;
  {$EXTERNALSYM AVIMakeCompressedStream}

{$IFDEF UNICODE}
  function AVISaveVW(szFile: PWideChar;
                     pclsidHandler: PCLSID;
                     lpfnCallback: AVISAVECALLBACK;
                     nStreams: Integer;
                     ppavi: PAVIStream;
                     plpOptions: LPAVICOMPRESSOPTIONS): HResult; stdcall;
  {$EXTERNALSYM AVISaveVW}
{$ELSE}
  function AVISaveVA(szFile: PAnsiChar;
                     pclsidHandler: PCLSID;
                     lpfnCallback: AVISAVECALLBACK;
                     nStreams: Integer;
                     ppavi: PAVIStream;
                     plpOptions: LPAVICOMPRESSOPTIONS): HResult; stdcall;
  {$EXTERNALSYM AVISaveVA}
{$ENDIF}

  { unicode }
  function AVISaveW(szFile: PWideChar;
                    pclsidHandler: PCLSID;
                    lpfnCallback: AVISAVECALLBACK;
                    nStreams: Integer;
                    ppavi: PAVIStream;
                    plpOptions: LPAVICOMPRESSOPTIONS): HResult; stdcall;
  {$EXTERNALSYM AVISaveW}


  function AVISaveOptions(_hWnd: HWND;
			                     uiFlags: UINT;
                           nStreams: Integer;
                           ppavi: PAVIStream;
                           {_Inout_} plpOptions: LPAVICOMPRESSOPTIONS): INT_PTR; // Returns TRUE if the user pressed OK, FALSE for CANCEL, or an error otherwise.
  {$EXTERNALSYM AVISaveOptions}

  function AVISaveOptionsFree(nStreams: Integer;
			                        plpOptions: LPAVICOMPRESSOPTIONS): HResult; stdcall;
  {$EXTERNALSYM AVISaveOptionsFree}

  // FLAGS FOR uiFlags:
  //
  // Same as the flags for ICCompressorChoose (see compman.h)
  // These determine what the compression options dialog for video streams
  // will look like.

{$IFDEF UNICODE}
  function AVIBuildFilterW({_Out_} lpszFilter: PWideChar;
                           cbFilter: LONG;
                           fSaving: BOOL): HResult; stdcall;
  {$EXTERNALSYM AVIBuildFilterW}
{$ELSE}
  function AVIBuildFilterA({_Out_} lpszFilter: PAnsiChar;
                           cbFilter: LONG;
                           fSaving: BOOL): HResult; stdcall;
  {$EXTERNALSYM AVIBuildFilterA}
{$ENDIF}

  { unicode }
  function AVIBuildFilter({_Out_} lpszFilter: PWideChar;
                          cbFilter: LONG;
                          fSaving: BOOL): HResult; stdcall;
  {$EXTERNALSYM AVIBuildFilter}


  function AVIMakeFileFromStreams({_Outptr_} ppfile: PAVIFILE;
			                            nStreams: Integer;
			                            papStreams: PAVIStream): HResult; stdcall;
  {$EXTERNALSYM AVIMakeFileFromStreams}

  function AVIMakeStreamFromClipboard(cfFormat: UINT;
                                      hGlobal: THandle;
                                      {_Outptr_} ppstream: PAVIStream): HResult; stdcall;
  {$EXTERNALSYM AVIMakeStreamFromClipboard}


  //****************************************************************************
  //
  //  Clipboard routines
  //
  //****************************************************************************

  function AVIPutFileOnClipboard(pf: PAVIFile): HResult; stdcall;
  {$EXTERNALSYM AVIPutFileOnClipboard}

  function AVIGetFromClipboard({_Outptr_} lppf: PAVIFile): HResult; stdcall;
  {$EXTERNALSYM AVIGetFromClipboard}

  function AVIClearClipboard(): HResult; stdcall;
  {$EXTERNALSYM AVIClearClipboard}

  //****************************************************************************
  //
  //  Editing routines
  //
  //****************************************************************************

  function CreateEditableStream({_Outptr_} ppsEditable: PAVIStream;
		                            psSource: IAVIStream): HResult; stdcall;
  {$EXTERNALSYM CreateEditableStream}

  function EditStreamCut(pavi: IAVIStream;
                         {_Inout_} plStart: PLONG;
                         {_Inout_} plLength: PLONG;
                         {_Outptr_} ppResult: PAVISTREAM): HResult; stdcall;
  {$EXTERNALSYM EditStreamCut}

  function EditStreamCopy(pavi: IAVIStream;
                         {_Inout_} plStart: PLONG;
                         {_Inout_} plLength: PLONG;
                         {_Outptr_} ppResult: PAVISTREAM): HResult; stdcall;
  {$EXTERNALSYM EditStreamCopy}

  function EditStreamPaste(pavi: IAVIStream;
                           {_Inout_} plPos: PLONG;
                           {_Inout_} plLength: PLONG;
                           pstream: IAVIStream;
                           lStart: LONG;
                           lEnd: LONG): HResult; stdcall;
  {$EXTERNALSYM EditStreamPaste}

  function EditStreamClone(pavi: IAVIStream;
                           {_Outptr_} ppResult: PAVISTREAM): HResult; stdcall;
  {$EXTERNALSYM EditStreamClone}

{$IFDEF UNICODE}
  function EditStreamSetNameW(pavi: IAVIStream;
                              lpszName: PWideChar): HResult; stdcall;
  {$EXTERNALSYM EditStreamSetNameW}
{$ELSE}
  function EditStreamSetNameA(pavi: IAVIStream;
                              lpszName: PAnsiChar): HResult; stdcall;
  {$EXTERNALSYM EditStreamSetNameA}
{$ENDIF}

  { unicode }
  function EditStreamSetName(pavi: IAVIStream;
                             lpszName: PWideChar): HResult; stdcall;
  {$EXTERNALSYM EditStreamSetName}

{$IFDEF UNICODE}
  function EditStreamSetInfoW(pavi: IAVIStream;
                              lpInfo: LPAVISTREAMINFOW;
                              cbInfo: LONG): HResult; stdcall;
  {$EXTERNALSYM EditStreamSetInfoW}
{$ELSE}
  function EditStreamSetInfoA(pavi: IAVIStream;
                              lpInfo: LPAVISTREAMINFOA;
                              cbInfo: LONG): HResult; stdcall;
  {$EXTERNALSYM EditStreamSetInfoA}
{$ENDIF}

  { unicode }
  function EditStreamSetInfo(pavi: IAVIStream;
                             lpInfo: LPAVISTREAMINFOW;
                             cbInfo: LONG): HResult; stdcall;
  {$EXTERNALSYM EditStreamSetInfo}



  // --------------------

  function MAKE_AVIERR(error: DWord): DWord; inline;
  {$EXTERNALSYM MAKE_AVIERR}

const

  AVIERR_OK                           = 0;
  {$EXTERNALSYM AVIERR_OK}

  // !!! Questions to be answered:
  // How can you get a string form of these errors?
  // Which of these errors should be replaced by errors in SCODE.H?
  AVIERR_UNSUPPORTED                  = 101;
  {$EXTERNALSYM AVIERR_UNSUPPORTED}
  AVIERR_BADFORMAT                    = 102;
  {$EXTERNALSYM AVIERR_BADFORMAT}
  AVIERR_MEMORY                       = 103;
  {$EXTERNALSYM AVIERR_MEMORY}
  AVIERR_INTERNAL                     = 104;
  {$EXTERNALSYM AVIERR_INTERNAL}
  AVIERR_BADFLAGS                     = 105;
  {$EXTERNALSYM AVIERR_BADFLAGS}
  AVIERR_BADPARAM                     = 106;
  {$EXTERNALSYM AVIERR_BADPARAM}
  AVIERR_BADSIZE                      = 107;
  {$EXTERNALSYM AVIERR_BADSIZE}
  AVIERR_BADHANDLE                    = 108;
  {$EXTERNALSYM AVIERR_BADHANDLE}
  AVIERR_FILEREAD                     = 109;
  {$EXTERNALSYM AVIERR_FILEREAD}
  AVIERR_FILEWRITE                    = 110;
  {$EXTERNALSYM AVIERR_FILEWRITE}
  AVIERR_FILEOPEN                     = 111;
  {$EXTERNALSYM AVIERR_FILEOPEN}
  AVIERR_COMPRESSOR                   = 112;
  {$EXTERNALSYM AVIERR_COMPRESSOR}
  AVIERR_NOCOMPRESSOR                 = 113;
  {$EXTERNALSYM AVIERR_NOCOMPRESSOR}
  AVIERR_READONLY                     = 114;
  {$EXTERNALSYM AVIERR_READONLY}
  AVIERR_NODATA                       = 115;
  {$EXTERNALSYM AVIERR_NODATA}
  AVIERR_BUFFERTOOSMALL               = 116;
  {$EXTERNALSYM AVIERR_BUFFERTOOSMALL}
  AVIERR_CANTCOMPRESS                 = 117;
  {$EXTERNALSYM AVIERR_CANTCOMPRESS}
  AVIERR_USERABORT                    = 198;
  {$EXTERNALSYM AVIERR_USERABORT}
  AVIERR_ERROR                        = 199;
  {$EXTERNALSYM AVIERR_ERROR}


  //****************************************************************************
  //
  //  MCIWnd - Window class for MCI objects
  //
  //****************************************************************************

  //----------------------------------------------------------------------------*\
  //
  //  MCIWnd
  //
  //    MCIWnd window class header file.
  //
  //    the MCIWnd window class is a window class for controling MCI devices
  //    MCI devices include, wave files, midi files, AVI Video, cd audio,
  //    vcr, video disc, and others..
  //
  //    to learn more about MCI and mci command sets see the
  //    "Microsoft Multimedia Programmers's guide" in the Win31 SDK
  //
  //    the easiest use of the MCIWnd class is like so:
  //
  //          hwnd = MCIWndCreate(hwndParent, hInstance, 0, "chimes.wav");
  //          ...
  //          MCIWndPlay(hwnd);
  //          MCIWndStop(hwnd);
  //          MCIWndPause(hwnd);
  //          ....
  //          MCIWndDestroy(hwnd);
  //
  //    this will create a window with a play/pause, stop and a playbar
  //    and start the wave file playing.
  //
  //    mciwnd.h defines macros for all the most common MCI commands, but
  //    any string command can be used if needed.
  //
  //    Note: unlike the mciSendString() API, no alias or file name needs
  //    to be specifed, since the device to use is implied by the window handle.
  //
  //          MCIWndSendString(_hWnd, "setaudio stream to 2");
  //
  //    (C) Copyright Microsoft Corp. 1991-1995.  All rights reserved.
  //
  //    You have a royalty-free right to use, modify, reproduce and
  //    distribute the Sample Files (and/or any modified version) in
  //    any way you find useful, provided that you agree that
  //    Microsoft has no warranty obligations or liability for any
  //    Sample Application Files.
  //
  //    If you did not get this from Microsoft Sources, then it may not be the
  //    most current version.  This sample code in particular will be updated
  //    and include more documentation.
  //
  //    Sources are:
  //       CompuServe: WINSDK forum, MDK section.
  //       Anonymous FTP from ftp.uu.net vendor\microsoft\multimedia
  //
  // WIN32:
  //
  //    MCIWnd supports both ansi and unicode interfaces. For any message that
  //    takes or returns a text string, two versions of the message are defined,
  //    appended with A or W for Ansi or Wide Char. The message or api itself
  //    is defined to be one or other of these depending on whether you have
  //    UNICODE defined in your application.
  //    Thus for the api MCIWndCreate, there are in fact two apis,
  //    MCIWndCreateA and MCIWndCreateW. If you call MCIWndCreate, this will be
  //    re-routed to MCIWndCreateA unless UNICODE is defined when building your
  //    application. In any one application, you can mix calls to the
  //    Ansi and Unicode entrypoints.
  //
  //    If you use SendMessage instead of the macros below such as MCIWndOpen(),
  //    you will see that the messages have changed for WIN32, to support Ansi
  //    and Unicode entrypoints. In particular, MCI_OPEN has been replaced by
  //    MCWNDM_OPENA, or MCIWNDM_OPENW (MCIWNDM_OPEN is defined to be one or
  //    other of these).
  //
  //    Also, note that the WIN32 implementation of MCIWnd uses UNICODE
  //    so all apis and messages supporting ANSI strings do so by mapping them
  //    UNICODE strings and then calling the corresponding UNICODE entrypoint.
  //
  //----------------------------------------------------------------------------

// #define MCIWndSM ::SendMessage  /* SendMessage in C++*/
// We use the delphi Sendmessage

  function MCIWndSM(_hWnd: HWND;
                    Msg: UINT;
                    wParm: WPARAM;
                    lParm: LPARAM): DWORD;

const
  MCIWND_WINDOW_CLASS = 'MCIWndClass';
  {$EXTERNALSYM MCIWND_WINDOW_CLASS}

{$IFDEF UNICODE}
  function MCIWndCreateW({_In_opt_} hwndParent: HWND;
                         {_In_opt_} hInstance: HINSTANCE;
                         dwStyle: DWORD;
                         {_In_opt_} szFile: PWideChar): HWND; stdcall;
  {$EXTERNALSYM MCIWndCreateW}
{$ELSE}
  function MCIWndCreateA({_In_opt_} hwndParent: HWND;
                         {_In_opt_} hInstance: HINST;
                         dwStyle: DWORD;
                         {_In_opt_} szFile: PAnsiChar): HWND; stdcall;
  {$EXTERNALSYM MCIWndCreateA}
{$ENDIF}

  { unicode }
  function MCIWndCreate({_In_opt_} hwndParent: HWND;
                        {_In_opt_} hInstance: HINSTANCE;
                        dwStyle: DWORD;
                        {_In_opt_} szFile: PWideChar): HWND; stdcall;
  {$EXTERNALSYM MCIWndCreate}

  function MCIWndRegisterClass(): BOOL;
  {$EXTERNALSYM MCIWndRegisterClass}

  // Flags for the MCIWndOpen command

const

  MCIWNDOPENF_NEW                     = $0001;  // open a new file
  {$EXTERNALSYM MCIWNDOPENF_NEW}

  // window styles
  MCIWNDF_NOAUTOSIZEWINDOW            = $0001;  // when movie size changes
  {$EXTERNALSYM MCIWNDF_NOAUTOSIZEWINDOW}
  MCIWNDF_NOPLAYBAR                   = $0002;  // no toolbar
  {$EXTERNALSYM MCIWNDF_NOPLAYBAR}
  MCIWNDF_NOAUTOSIZEMOVIE             = $0004;  // when window size changes
  {$EXTERNALSYM MCIWNDF_NOAUTOSIZEMOVIE}
  MCIWNDF_NOMENU                      = $0008;  // no popup menu from RBUTTONDOWN
  {$EXTERNALSYM MCIWNDF_NOMENU}
  MCIWNDF_SHOWNAME                    = $0010;  // show name in caption
  {$EXTERNALSYM MCIWNDF_SHOWNAME}
  MCIWNDF_SHOWPOS                     = $0020;  // show position in caption
  {$EXTERNALSYM MCIWNDF_SHOWPOS}
  MCIWNDF_SHOWMODE                    = $0040;  // show mode in caption
  {$EXTERNALSYM MCIWNDF_SHOWMODE}
  MCIWNDF_SHOWALL                     = $0070;  // show all
  {$EXTERNALSYM MCIWNDF_SHOWALL}

  MCIWNDF_NOTIFYMODE                  = $0100;  // tell parent of mode change
  {$EXTERNALSYM MCIWNDF_NOTIFYMODE}
  MCIWNDF_NOTIFYPOS                   = $0200;  // tell parent of pos change
  {$EXTERNALSYM MCIWNDF_NOTIFYPOS}
  MCIWNDF_NOTIFYSIZE                  = $0400;  // tell parent of size change
  {$EXTERNALSYM MCIWNDF_NOTIFYSIZE}
  MCIWNDF_NOTIFYERROR                 = $1000;  // tell parent of an error
  {$EXTERNALSYM MCIWNDF_NOTIFYERROR}
  MCIWNDF_NOTIFYALL                   = $1F00;  // tell all
  {$EXTERNALSYM MCIWNDF_NOTIFYALL}

  MCIWNDF_NOTIFYANSI                  = $0080;
  {$EXTERNALSYM MCIWNDF_NOTIFYANSI}


  // The MEDIA notification includes a text string.
  // To receive notifications in ANSI instead of unicode set the
  // MCIWNDF_NOTIFYANSI style bit. The macro below includes this bit
  // by default unless you define UNICODE in your application.

const

  MCIWNDF_NOTIFYMEDIAA                = $0880;  // tell parent of media change
  {$EXTERNALSYM MCIWNDF_NOTIFYMEDIAA}
  MCIWNDF_NOTIFYMEDIAW                = $0800;  // tell parent of media change
  {$EXTERNALSYM MCIWNDF_NOTIFYMEDIAW}

{$IFDEF UNICODE}
  MCIWNDF_NOTIFYMEDIA                 = MCIWNDF_NOTIFYMEDIAW;
  {$EXTERNALSYM MCIWNDF_NOTIFYMEDIA}
{$ELSE}
  MCIWNDF_NOTIFYMEDIA                 = MCIWNDF_NOTIFYMEDIAA;
  {$EXTERNALSYM MCIWNDF_NOTIFYMEDIA}
{$ENDIF}

  MCIWNDF_RECORD                      = $2000;  // Give a record button
  {$EXTERNALSYM MCIWNDF_RECORD}
  MCIWNDF_NOERRORDLG                  = $4000;  // Show Error Dlgs for MCI cmds?
  {$EXTERNALSYM MCIWNDF_NOERRORDLG}
  MCIWNDF_NOOPEN                      = $8000;  // Don't allow user to open things
  {$EXTERNALSYM MCIWNDF_NOOPEN}


  // can macros

  function MCIWndCanPlay(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndCanPlay}

  function MCIWndCanRecord(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndCanRecord}

  function MCIWndCanSave(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndCanSave}

  function MCIWndCanWindow(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndCanWindow}

  function MCIWndCanEject(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndCanEject}

  function MCIWndCanConfig(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndCanConfig}

  function MCIWndPaletteKick(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM MCIWndPaletteKick}

  function MCIWndSave(_hWnd: HWND;
                      szFile: LPCSTR): DWORD; inline;
  {$EXTERNALSYM MCIWndSave}

  function MCIWndSaveDialog(_hWnd: HWND): DWORD; inline;
  {$EXTERNALSYM MCIWndSaveDialog}


  // if you dont give a device it will use the current device....

  function MCIWndNew(_hWnd: HWND; lp: Pointer): DWORD; inline;
  {$EXTERNALSYM MCIWndNew}

  function MCIWndRecord(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndRecord}

  function MCIWndOpen(_hWnd: HWND; sz: LPCSTR; f: BOOL): LONG; inline;
  {$EXTERNALSYM MCIWndOpen}

  function MCIWndOpenDialog(_hWnd: HWND): DWORD; inline;
  {$EXTERNALSYM MCIWndOpenDialog}

  function MCIWndClose(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndClose}

  function MCIWndPlay(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndPlay}

  function MCIWndStop(_hWnd: DWORD): LONG; inline;
  {$EXTERNALSYM MCIWndStop}

  function MCIWndPause(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndPause}

  function MCIWndResume(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndResume}

  function MCIWndSeek(_hWnd: HWND; lPos: LONG): LONG; inline;
  {$EXTERNALSYM MCIWndSeek}

  function MCIWndEject(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndEject}

  function MCIWndHome(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndHome}

  function MCIWndEnd(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndEnd}

  function MCIWndGetSource(_hWnd: HWND; prc: PRect): LONG; inline;
  {$EXTERNALSYM MCIWndGetSource}

  function MCIWndPutSource(_hWnd: HWND; prc: PRect): LONG; inline;
  {$EXTERNALSYM MCIWndPutSource}

  function MCIWndGetDest(_hWnd: HWND; prc: PRect): LONG; inline;
  {$EXTERNALSYM MCIWndGetDest}

  function MCIWndPutDest(_hWnd: HWND; prc: PRect): LONG; inline;
  {$EXTERNALSYM MCIWndPutDest}

  function MCIWndPlayReverse(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndPlayReverse}

  function MCIWndPlayFrom(_hWnd: HWND; lPos: LONG): LONG; inline;
  {$EXTERNALSYM MCIWndPlayFrom}

  function MCIWndPlayTo(_hWnd: HWND; lPos: LONG): LONG; inline;
  {$EXTERNALSYM MCIWndPlayTo}

  function MCIWndPlayFromTo(_hWnd: HWND; lStart: DWORD; lEnd: DWORD): LONG; inline;
  {$EXTERNALSYM MCIWndPlayFromTo}

  function MCIWndGetDeviceID(_hWnd: HWND): UINT; inline;
  {$EXTERNALSYM MCIWndGetDeviceID}

  function MCIWndGetAlias(_hWnd: HWND): UINT; inline;
  {$EXTERNALSYM MCIWndGetAlias}

  function MCIWndGetMode(_hWnd: HWND; lp: LPCSTR; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndGetMode}

  function MCIWndGetPosition(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndGetPosition}

  function MCIWndGetPositionString(_hWnd: HWND; lp: LPCSTR; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndGetPositionString}

  function MCIWndGetStart(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndGetStart}

  function MCIWndGetLength(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndGetLength}

  function MCIWndGetEnd(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndGetEnd}

  function MCIWndStep(_hWnd: HWND; n: LONG): LONG; inline;
  {$EXTERNALSYM MCIWndStep}

  procedure MCIWndDestroy(_hWnd: HWND); inline;
  {$EXTERNALSYM MCIWndDestroy}

  procedure MCIWndSetZoom(_hWnd: HWND; iZoom: UINT); inline;
  {$EXTERNALSYM MCIWndSetZoom}

  function MCIWndGetZoom(_hWnd: HWND): UINT; inline;
  {$EXTERNALSYM MCIWndGetZoom}

  function MCIWndSetVolume(_hWnd: HWND; iVol: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndSetVolume}

  function MCIWndGetVolume(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndGetVolume}

  function MCIWndSetSpeed(_hWnd: HWND; iSpeed: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndSetSpeed}

  function MCIWndGetSpeed(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndGetSpeed}

  function MCIWndSetTimeFormat(_hWnd: HWND; lp: LPCSTR): LONG; inline;
  {$EXTERNALSYM MCIWndSetTimeFormat}

  function MCIWndGetTimeFormat(_hWnd: HWND; lp: LPCSTR; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndGetTimeFormat}

  procedure MCIWndValidateMedia(_hWnd: HWND); inline;
  {$EXTERNALSYM MCIWndValidateMedia}

  procedure MCIWndSetRepeat(_hWnd: HWND; f: BOOL); inline;
  {$EXTERNALSYM MCIWndSetRepeat}

  function MCIWndGetRepeat(_hWnd: HWND): BOOL;  inline;
  {$EXTERNALSYM MCIWndGetRepeat}

  function MCIWndUseFrames(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndUseFrames}

  function MCIWndUseTime(_hWnd: HWND): LONG; inline;
  {$EXTERNALSYM MCIWndUseTime}

  procedure MCIWndSetActiveTimer(_hWnd: HWND; active: UINT); inline;
  {$EXTERNALSYM MCIWndSetActiveTimer}

  procedure MCIWndSetInactiveTimer(_hWnd: HWND; inactive: UINT); inline;
  {$EXTERNALSYM MCIWndSetInactiveTimer}

  procedure MCIWndSetTimers(_hWnd: HWND; active: UINT; inactive: UINT); inline;
  {$EXTERNALSYM MCIWndSetTimers}

  function MCIWndGetActiveTimer(_hWnd: HWND): UINT; inline;
  {$EXTERNALSYM MCIWndGetActiveTimer}

  function MCIWndGetInactiveTimer(_hWnd: HWND): UINT; inline;
  {$EXTERNALSYM MCIWndGetInactiveTimer}

  function MCIWndRealize(_hWnd: HWND; fBkgnd: BOOL): LONG; inline;
  {$EXTERNALSYM MCIWndRealize}

  function MCIWndSendString(_hWnd: HWND; sz: LPCSTR): LONG; inline;
  {$EXTERNALSYM MCIWndSendString}

  function MCIWndReturnString(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndReturnString}

  function MCIWndGetError(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndGetError}

  //#define MCIWndActivate(_hWnd, f)     (void)MCIWndSM(_hWnd, WM_ACTIVATE, (WPARAM)(BOOL)(f), 0)

  function MCIWndGetPalette(_hWnd: HWND): HPALETTE; inline;
  {$EXTERNALSYM MCIWndGetPalette}

  function MCIWndSetPalette(_hWnd: HWND; hpal: HPALETTE): LONG; inline;
  {$EXTERNALSYM MCIWndSetPalette}

  function MCIWndGetFileName(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndGetFileName}

  function MCIWndGetDevice(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
  {$EXTERNALSYM MCIWndGetDevice}

  function MCIWndGetStyles(_hWnd: HWND): UINT; inline;
  {$EXTERNALSYM MCIWndGetStyles}

  function MCIWndChangeStyles(_hWnd: HWND; mask: UINT; value: LONG): LONG; inline;
  {$EXTERNALSYM MCIWndChangeStyles}

  function MCIWndOpenInterface(_hWnd: HWND; pUnk: PUnknown): LONG;  inline;
  {$EXTERNALSYM MCIWndOpenInterface}

  function MCIWndSetOwner(_hWnd: HWND; hwndP: DWORD): LONG; inline;
  {$EXTERNALSYM MCIWndSetOwner}


  // Messages an app will send to MCIWND

  // all the text-related messages are defined out of order above (they need
  // to be defined before the MCIWndOpen() macros

const

  MCIWNDM_GETDEVICEID                 = (WM_USER + 100);
  {$EXTERNALSYM MCIWNDM_GETDEVICEID}
  MCIWNDM_GETSTART                    = (WM_USER + 103);
  {$EXTERNALSYM MCIWNDM_GETSTART}
  MCIWNDM_GETLENGTH                   = (WM_USER + 104);
  {$EXTERNALSYM MCIWNDM_GETLENGTH}
  MCIWNDM_GETEND                      = (WM_USER + 105);
  {$EXTERNALSYM MCIWNDM_GETEND}
  MCIWNDM_EJECT                       = (WM_USER + 107);
  {$EXTERNALSYM MCIWNDM_EJECT}
  MCIWNDM_SETZOOM                     = (WM_USER + 108);
  {$EXTERNALSYM MCIWNDM_SETZOOM}
  MCIWNDM_GETZOOM                     = (WM_USER + 109);
  {$EXTERNALSYM MCIWNDM_GETZOOM}
  MCIWNDM_SETVOLUME                   = (WM_USER + 110);
  {$EXTERNALSYM MCIWNDM_SETVOLUME}
  MCIWNDM_GETVOLUME                   = (WM_USER + 111);
  {$EXTERNALSYM MCIWNDM_GETVOLUME}
  MCIWNDM_SETSPEED                    = (WM_USER + 112);
  {$EXTERNALSYM MCIWNDM_SETSPEED}
  MCIWNDM_GETSPEED                    = (WM_USER + 113);
  {$EXTERNALSYM MCIWNDM_GETSPEED}
  MCIWNDM_SETREPEAT                   = (WM_USER + 114);
  {$EXTERNALSYM MCIWNDM_SETREPEAT}
  MCIWNDM_GETREPEAT                   = (WM_USER + 115);
  {$EXTERNALSYM MCIWNDM_GETREPEAT}
  MCIWNDM_REALIZE                     = (WM_USER + 118);
  {$EXTERNALSYM MCIWNDM_REALIZE}
  MCIWNDM_VALIDATEMEDIA               = (WM_USER + 121);
  {$EXTERNALSYM MCIWNDM_VALIDATEMEDIA}
  MCIWNDM_PLAYFROM                    = (WM_USER + 122);
  {$EXTERNALSYM MCIWNDM_PLAYFROM}
  MCIWNDM_PLAYTO                      = (WM_USER + 123);
  {$EXTERNALSYM MCIWNDM_PLAYTO}
  MCIWNDM_GETPALETTE                  = (WM_USER + 126);
  {$EXTERNALSYM MCIWNDM_GETPALETTE}
  MCIWNDM_SETPALETTE                  = (WM_USER + 127);
  {$EXTERNALSYM MCIWNDM_SETPALETTE}
  MCIWNDM_SETTIMERS                   = (WM_USER + 129);
  {$EXTERNALSYM MCIWNDM_SETTIMERS}
  MCIWNDM_SETACTIVETIMER              = (WM_USER + 130);
  {$EXTERNALSYM MCIWNDM_SETACTIVETIMER}
  MCIWNDM_SETINACTIVETIMER            = (WM_USER + 131);
  {$EXTERNALSYM MCIWNDM_SETINACTIVETIMER}
  MCIWNDM_GETACTIVETIMER              = (WM_USER + 132);
  {$EXTERNALSYM MCIWNDM_GETACTIVETIMER}
  MCIWNDM_GETINACTIVETIMER            = (WM_USER + 133);
  {$EXTERNALSYM MCIWNDM_GETINACTIVETIMER}
  MCIWNDM_CHANGESTYLES                = (WM_USER + 135);
  {$EXTERNALSYM MCIWNDM_CHANGESTYLES}
  MCIWNDM_GETSTYLES                   = (WM_USER + 136);
  {$EXTERNALSYM MCIWNDM_GETSTYLES}
  MCIWNDM_GETALIAS                    = (WM_USER + 137);
  {$EXTERNALSYM MCIWNDM_GETALIAS}
  MCIWNDM_PLAYREVERSE                 = (WM_USER + 139);
  {$EXTERNALSYM MCIWNDM_PLAYREVERSE}
  MCIWNDM_GET_SOURCE                  = (WM_USER + 140);
  {$EXTERNALSYM MCIWNDM_GET_SOURCE}
  MCIWNDM_PUT_SOURCE                  = (WM_USER + 141);
  {$EXTERNALSYM MCIWNDM_PUT_SOURCE}
  MCIWNDM_GET_DEST                    = (WM_USER + 142);
  {$EXTERNALSYM MCIWNDM_GET_DEST}
  MCIWNDM_PUT_DEST                    = (WM_USER + 143);
  {$EXTERNALSYM MCIWNDM_PUT_DEST}
  MCIWNDM_CAN_PLAY                    = (WM_USER + 144);
  {$EXTERNALSYM MCIWNDM_CAN_PLAY}
  MCIWNDM_CAN_WINDOW                  = (WM_USER + 145);
  {$EXTERNALSYM MCIWNDM_CAN_WINDOW}
  MCIWNDM_CAN_RECORD                  = (WM_USER + 146);
  {$EXTERNALSYM MCIWNDM_CAN_RECORD}
  MCIWNDM_CAN_SAVE                    = (WM_USER + 147);
  {$EXTERNALSYM MCIWNDM_CAN_SAVE}
  MCIWNDM_CAN_EJECT                   = (WM_USER + 148);
  {$EXTERNALSYM MCIWNDM_CAN_EJECT}
  MCIWNDM_CAN_CONFIG                  = (WM_USER + 149);
  {$EXTERNALSYM MCIWNDM_CAN_CONFIG}
  MCIWNDM_PALETTEKICK                 = (WM_USER + 150);
  {$EXTERNALSYM MCIWNDM_PALETTEKICK}
  MCIWNDM_OPENINTERFACE               = (WM_USER + 151);
  {$EXTERNALSYM MCIWNDM_OPENINTERFACE}
  MCIWNDM_SETOWNER                    = (WM_USER + 152);
  {$EXTERNALSYM MCIWNDM_SETOWNER}


  //define both A and W messages

const

  MCIWNDM_SENDSTRINGA                 = (WM_USER + 101);
  {$EXTERNALSYM MCIWNDM_SENDSTRINGA}
  MCIWNDM_GETPOSITIONA                = (WM_USER + 102);
  {$EXTERNALSYM MCIWNDM_GETPOSITIONA}
  MCIWNDM_GETMODEA                    = (WM_USER + 106);
  {$EXTERNALSYM MCIWNDM_GETMODEA}
  MCIWNDM_SETTIMEFORMATA              = (WM_USER + 119);
  {$EXTERNALSYM MCIWNDM_SETTIMEFORMATA}
  MCIWNDM_GETTIMEFORMATA              = (WM_USER + 120);
  {$EXTERNALSYM MCIWNDM_GETTIMEFORMATA}
  MCIWNDM_GETFILENAMEA                = (WM_USER + 124);
  {$EXTERNALSYM MCIWNDM_GETFILENAMEA}
  MCIWNDM_GETDEVICEA                  = (WM_USER + 125);
  {$EXTERNALSYM MCIWNDM_GETDEVICEA}
  MCIWNDM_GETERRORA                   = (WM_USER + 128);
  {$EXTERNALSYM MCIWNDM_GETERRORA}
  MCIWNDM_NEWA                        = (WM_USER + 134);
  {$EXTERNALSYM MCIWNDM_NEWA}
  MCIWNDM_RETURNSTRINGA               = (WM_USER + 138);
  {$EXTERNALSYM MCIWNDM_RETURNSTRINGA}
  MCIWNDM_OPENA                       = (WM_USER + 153);
  {$EXTERNALSYM MCIWNDM_OPENA}

  MCIWNDM_SENDSTRINGW                 = (WM_USER + 201);
  {$EXTERNALSYM MCIWNDM_SENDSTRINGW}
  MCIWNDM_GETPOSITIONW                = (WM_USER + 202);
  {$EXTERNALSYM MCIWNDM_GETPOSITIONW}
  MCIWNDM_GETMODEW                    = (WM_USER + 206);
  {$EXTERNALSYM MCIWNDM_GETMODEW}
  MCIWNDM_SETTIMEFORMATW              = (WM_USER + 219);
  {$EXTERNALSYM MCIWNDM_SETTIMEFORMATW}
  MCIWNDM_GETTIMEFORMATW              = (WM_USER + 220);
  {$EXTERNALSYM MCIWNDM_GETTIMEFORMATW}
  MCIWNDM_GETFILENAMEW                = (WM_USER + 224);
  {$EXTERNALSYM MCIWNDM_GETFILENAMEW}
  MCIWNDM_GETDEVICEW                  = (WM_USER + 225);
  {$EXTERNALSYM MCIWNDM_GETDEVICEW}
  MCIWNDM_GETERRORW                   = (WM_USER + 228);
  {$EXTERNALSYM MCIWNDM_GETERRORW}
  MCIWNDM_NEWW                        = (WM_USER + 234);
  {$EXTERNALSYM MCIWNDM_NEWW}
  MCIWNDM_RETURNSTRINGW               = (WM_USER + 238);
  {$EXTERNALSYM MCIWNDM_RETURNSTRINGW}
  MCIWNDM_OPENW                       = (WM_USER + 252);
  {$EXTERNALSYM MCIWNDM_OPENW}

  // map defaults to A or W depending on app's UNICODE setting

const

{$IFDEF UNICODE}
  MCIWNDM_SENDSTRING                  = MCIWNDM_SENDSTRINGW;
  {$EXTERNALSYM MCIWNDM_SENDSTRING}
  MCIWNDM_GETPOSITION                 = MCIWNDM_GETPOSITIONW;
  {$EXTERNALSYM MCIWNDM_GETPOSITION}
  MCIWNDM_GETMODE                     = MCIWNDM_GETMODEW;
  {$EXTERNALSYM MCIWNDM_GETMODE}
  MCIWNDM_SETTIMEFORMAT               = MCIWNDM_SETTIMEFORMATW;
  {$EXTERNALSYM MCIWNDM_SETTIMEFORMAT}
  MCIWNDM_GETTIMEFORMAT               = MCIWNDM_GETTIMEFORMATW;
  {$EXTERNALSYM MCIWNDM_GETTIMEFORMAT}
  MCIWNDM_GETFILENAME                 = MCIWNDM_GETFILENAMEW;
  {$EXTERNALSYM MCIWNDM_GETFILENAME}
  MCIWNDM_GETDEVICE                   = MCIWNDM_GETDEVICEW;
  {$EXTERNALSYM MCIWNDM_GETDEVICE}
  MCIWNDM_GETERROR                    = MCIWNDM_GETERRORW;
  {$EXTERNALSYM MCIWNDM_GETERROR}
  MCIWNDM_NEW                         = MCIWNDM_NEWW;
  {$EXTERNALSYM MCIWNDM_NEW}
  MCIWNDM_RETURNSTRING                = MCIWNDM_RETURNSTRINGW;
  {$EXTERNALSYM MCIWNDM_RETURNSTRING}
  MCIWNDM_OPEN                        = MCIWNDM_OPENW;
  {$EXTERNALSYM MCIWNDM_OPEN}
{$ELSE}
  MCIWNDM_SENDSTRING                  = MCIWNDM_SENDSTRINGA;
  {$EXTERNALSYM MCIWNDM_SENDSTRING}
  MCIWNDM_GETPOSITION                 = MCIWNDM_GETPOSITIONA;
  {$EXTERNALSYM MCIWNDM_GETPOSITION}
  MCIWNDM_GETMODE                     = MCIWNDM_GETMODEA;
  {$EXTERNALSYM MCIWNDM_GETMODE}
  MCIWNDM_SETTIMEFORMAT               = MCIWNDM_SETTIMEFORMATA;
  {$EXTERNALSYM MCIWNDM_SETTIMEFORMAT}
  MCIWNDM_GETTIMEFORMAT               = MCIWNDM_GETTIMEFORMATA;
  {$EXTERNALSYM MCIWNDM_GETTIMEFORMAT}
  MCIWNDM_GETFILENAME                 = MCIWNDM_GETFILENAMEA;
  {$EXTERNALSYM MCIWNDM_GETFILENAME}
  MCIWNDM_GETDEVICE                   = MCIWNDM_GETDEVICEA;
  {$EXTERNALSYM MCIWNDM_GETDEVICE}
  MCIWNDM_GETERROR                    = MCIWNDM_GETERRORA;
  {$EXTERNALSYM MCIWNDM_GETERROR}
  MCIWNDM_NEW                         = MCIWNDM_NEWA;
  {$EXTERNALSYM MCIWNDM_NEW}
  MCIWNDM_RETURNSTRING                = MCIWNDM_RETURNSTRINGA;
  {$EXTERNALSYM MCIWNDM_RETURNSTRING}
  MCIWNDM_OPEN                        = MCIWNDM_OPENA;
  {$EXTERNALSYM MCIWNDM_OPEN}
{$ENDIF}

  // note that the source text for MCIWND will thus contain
  // support for eg MCIWNDM_SENDSTRING (both the 16-bit entrypoint and
  // in win32 mapped to MCIWNDM_SENDSTRINGW), and MCIWNDM_SENDSTRINGA (the
  // win32 ansi thunk).

  // Messages MCIWND will send to an app
  // !!! Use less messages and use a code instead to indicate the type of notify? /* ;Internal */

const

  MCIWNDM_NOTIFYMODE                  = (WM_USER + 200);  // wp = _hWnd, lp = mode
  {$EXTERNALSYM MCIWNDM_NOTIFYMODE}
  MCIWNDM_NOTIFYPOS                   = (WM_USER + 201);  // wp = _hWnd, lp = pos
  {$EXTERNALSYM MCIWNDM_NOTIFYPOS}
  MCIWNDM_NOTIFYSIZE                  = (WM_USER + 202);  // wp = hwnd
  {$EXTERNALSYM MCIWNDM_NOTIFYSIZE}
  MCIWNDM_NOTIFYMEDIA                 = (WM_USER + 203);  // wp = _hWnd, lp = fn
  {$EXTERNALSYM MCIWNDM_NOTIFYMEDIA}
  MCIWNDM_NOTIFYERROR                 = (WM_USER + 205);  // wp = _hWnd, lp = error
  {$EXTERNALSYM MCIWNDM_NOTIFYERROR}

  // special seek values for START and END
  MCIWND_START                        = - 1;
  {$EXTERNALSYM MCIWND_START}
  MCIWND_END                          = - 2;
  {$EXTERNALSYM MCIWND_END}


  // MCI command message identifiers

  // win32 apps send MCIWNDM_OPEN
  MCI_OPEN                            = $0803;
  {$EXTERNALSYM MCI_OPEN}

  MCI_CLOSE                           = $0804;
  {$EXTERNALSYM MCI_CLOSE}
  MCI_PLAY                            = $0806;
  {$EXTERNALSYM MCI_PLAY}
  MCI_SEEK                            = $0807;
  {$EXTERNALSYM MCI_SEEK}
  MCI_STOP                            = $0808;
  {$EXTERNALSYM MCI_STOP}
  MCI_PAUSE                           = $0809;
  {$EXTERNALSYM MCI_PAUSE}
  MCI_STEP                            = $080E;
  {$EXTERNALSYM MCI_STEP}
  MCI_RECORD                          = $080F;
  {$EXTERNALSYM MCI_RECORD}
  MCI_SAVE                            = $0813;
  {$EXTERNALSYM MCI_SAVE}
  MCI_CUT                             = $0851;
  {$EXTERNALSYM MCI_CUT}
  MCI_COPY                            = $0852;
  {$EXTERNALSYM MCI_COPY}
  MCI_PASTE                           = $0853;
  {$EXTERNALSYM MCI_PASTE}
  MCI_RESUME                          = $0855;
  {$EXTERNALSYM MCI_RESUME}
  MCI_DELETE                          = $0856;
  {$EXTERNALSYM MCI_DELETE}


  // return values for 'status mode' command

  MCI_MODE_NOT_READY                  = (524);
  {$EXTERNALSYM MCI_MODE_NOT_READY}
  MCI_MODE_STOP                       = (525);
  {$EXTERNALSYM MCI_MODE_STOP}
  MCI_MODE_PLAY                       = (526);
  {$EXTERNALSYM MCI_MODE_PLAY}
  MCI_MODE_RECORD                     = (527);
  {$EXTERNALSYM MCI_MODE_RECORD}
  MCI_MODE_SEEK                       = (528);
  {$EXTERNALSYM MCI_MODE_SEEK}
  MCI_MODE_PAUSE                      = (529);
  {$EXTERNALSYM MCI_MODE_PAUSE}
  MCI_MODE_OPEN                       = (530);
  {$EXTERNALSYM MCI_MODE_OPEN}


  //****************************************************************************
  //
  //  VIDEO - Video Capture Driver Interface
  //
  //****************************************************************************

  // video data types

type

  PHVIDEO = ^HVIDEO;
  HVIDEO = type THandle;                 // generic handle
  {$EXTERNALSYM HVIDEO}
  LPHVIDEO = ^HVIDEO;
  {$EXTERNALSYM LPHVIDEO}


  //****************************************************************************
  //
  //                            Error Return Values
  //
  //****************************************************************************

const

  DV_ERR_OK                           = (0);  { No error }
  {$EXTERNALSYM DV_ERR_OK}
  DV_ERR_BASE                         = (1);  { Error Base }
  {$EXTERNALSYM DV_ERR_BASE}
  DV_ERR_NONSPECIFIC                  = (DV_ERR_BASE);
  {$EXTERNALSYM DV_ERR_NONSPECIFIC}
  { unsupported video format }
  DV_ERR_BADFORMAT                    = (DV_ERR_BASE + 1);
  {$EXTERNALSYM DV_ERR_BADFORMAT}
  { still something playing }
  DV_ERR_STILLPLAYING                 = (DV_ERR_BASE + 2);
  {$EXTERNALSYM DV_ERR_STILLPLAYING}
  { header not prepared }
  DV_ERR_UNPREPARED                   = (DV_ERR_BASE + 3);
  {$EXTERNALSYM DV_ERR_UNPREPARED}
  { device is synchronous }
  DV_ERR_SYNC                         = (DV_ERR_BASE + 4);
  {$EXTERNALSYM DV_ERR_SYNC}
  { number of channels exceeded }
  DV_ERR_TOOMANYCHANNELS              = (DV_ERR_BASE + 5);
  {$EXTERNALSYM DV_ERR_TOOMANYCHANNELS}
  DV_ERR_NOTDETECTED                  = (DV_ERR_BASE + 6);  { HW not detected }
  {$EXTERNALSYM DV_ERR_NOTDETECTED}
  DV_ERR_BADINSTALL                   = (DV_ERR_BASE + 7);  { Can not get Profile }
  {$EXTERNALSYM DV_ERR_BADINSTALL}
  DV_ERR_CREATEPALETTE                = (DV_ERR_BASE + 8);
  {$EXTERNALSYM DV_ERR_CREATEPALETTE}
  DV_ERR_SIZEFIELD                    = (DV_ERR_BASE + 9);
  {$EXTERNALSYM DV_ERR_SIZEFIELD}
  DV_ERR_PARAM1                       = (DV_ERR_BASE + 10);
  {$EXTERNALSYM DV_ERR_PARAM1}
  DV_ERR_PARAM2                       = (DV_ERR_BASE + 11);
  {$EXTERNALSYM DV_ERR_PARAM2}
  DV_ERR_CONFIG1                      = (DV_ERR_BASE + 12);
  {$EXTERNALSYM DV_ERR_CONFIG1}
  DV_ERR_CONFIG2                      = (DV_ERR_BASE + 13);
  {$EXTERNALSYM DV_ERR_CONFIG2}
  DV_ERR_FLAGS                        = (DV_ERR_BASE + 14);
  {$EXTERNALSYM DV_ERR_FLAGS}
  DV_ERR_13                           = (DV_ERR_BASE + 15);
  {$EXTERNALSYM DV_ERR_13}

  DV_ERR_NOTSUPPORTED                 = (DV_ERR_BASE + 16);  { function not suported }
  {$EXTERNALSYM DV_ERR_NOTSUPPORTED}
  DV_ERR_NOMEM                        = (DV_ERR_BASE + 17);  { out of memory }
  {$EXTERNALSYM DV_ERR_NOMEM}
  DV_ERR_ALLOCATED                    = (DV_ERR_BASE + 18);  { device is allocated }
  {$EXTERNALSYM DV_ERR_ALLOCATED}
  DV_ERR_BADDEVICEID                  = (DV_ERR_BASE + 19);
  {$EXTERNALSYM DV_ERR_BADDEVICEID}
  DV_ERR_INVALHANDLE                  = (DV_ERR_BASE + 20);
  {$EXTERNALSYM DV_ERR_INVALHANDLE}
  DV_ERR_BADERRNUM                    = (DV_ERR_BASE + 21);
  {$EXTERNALSYM DV_ERR_BADERRNUM}
  DV_ERR_NO_BUFFERS                   = (DV_ERR_BASE + 22);  { out of buffers }
  {$EXTERNALSYM DV_ERR_NO_BUFFERS}

  DV_ERR_MEM_CONFLICT                 = (DV_ERR_BASE + 23);  { Mem conflict detected }
  {$EXTERNALSYM DV_ERR_MEM_CONFLICT}
  DV_ERR_IO_CONFLICT                  = (DV_ERR_BASE + 24);  { I/O conflict detected }
  {$EXTERNALSYM DV_ERR_IO_CONFLICT}
  DV_ERR_DMA_CONFLICT                 = (DV_ERR_BASE + 25);  { DMA conflict detected }
  {$EXTERNALSYM DV_ERR_DMA_CONFLICT}
  DV_ERR_INT_CONFLICT                 = (DV_ERR_BASE + 26);  { Interrupt conflict detected }
  {$EXTERNALSYM DV_ERR_INT_CONFLICT}
  DV_ERR_PROTECT_ONLY                 = (DV_ERR_BASE + 27);  { Can not run in standard mode }
  {$EXTERNALSYM DV_ERR_PROTECT_ONLY}
  DV_ERR_LASTERROR                    = (DV_ERR_BASE + 27);
  {$EXTERNALSYM DV_ERR_LASTERROR}

  //#define DV_IDS_PROFILING        (DV_ERR_BASE + 900)
  //#define DV_IDS_LISTBOX          (DV_ERR_BASE + 901)

  DV_ERR_USER_MSG                     = (DV_ERR_BASE + 1000);  { Hardware specific errors }
  {$EXTERNALSYM DV_ERR_USER_MSG}

  //****************************************************************************
  //
  //                         Callback Messages
  //
  //  Note that the values for all installable driver callback messages are
  //  identical, (ie. MM_DRVM_DATA has the same value for capture drivers,
  //  installable video codecs, and the audio compression manager).
  //****************************************************************************

  MM_DRVM_OPEN                        = $3D0;
  {$EXTERNALSYM MM_DRVM_OPEN}
  MM_DRVM_CLOSE                       = $3D1;
  {$EXTERNALSYM MM_DRVM_CLOSE}
  MM_DRVM_DATA                        = $3D2;
  {$EXTERNALSYM MM_DRVM_DATA}
  MM_DRVM_ERROR                       = $3D3;
  {$EXTERNALSYM MM_DRVM_ERROR}

  DV_VM_OPEN                          = MM_DRVM_OPEN;  // Obsolete messages
  {$EXTERNALSYM DV_VM_OPEN}
  DV_VM_CLOSE                         = MM_DRVM_CLOSE;
  {$EXTERNALSYM DV_VM_CLOSE}
  DV_VM_DATA                          = MM_DRVM_DATA;
  {$EXTERNALSYM DV_VM_DATA}
  DV_VM_ERROR                         = MM_DRVM_ERROR;
  {$EXTERNALSYM DV_VM_ERROR}

  //****************************************************************************
  //
  //                         Structures
  //
  //****************************************************************************

  // video data block header

type

  PVIDEOHDR = ^videohdr_tag;
  {$EXTERNALSYM PVIDEOHDR}
  videohdr_tag = record
    lpData: PByte;                          { pointer to locked data buffer }
    dwBufferLength: DWORD;                  { Length of data buffer }
    dwBytesUsed: DWORD;                     { Bytes actually used }
    dwTimeCaptured: DWORD;                  { Milliseconds from start of stream }
    dwUser: DWORD_PTR;                      { for client's use }
    dwFlags: DWORD;                         { assorted flags (see defines) }
    dwReserved: array[0..3] of DWORD_PTR;  { reserved for driver }
  end;
  {$EXTERNALSYM videohdr_tag}
  VIDEOHDR = videohdr_tag;
  {$EXTERNALSYM VIDEOHDR}
  LPVIDEOHDR = videohdr_tag;
  {$EXTERNALSYM LPVIDEOHDR}

  // dwFlags field of VIDEOHDR

const

  VHDR_DONE                           = $00000001;  { Done bit }
  {$EXTERNALSYM VHDR_DONE}
  VHDR_PREPARED                       = $00000002;  { Set if this header has been prepared }
  {$EXTERNALSYM VHDR_PREPARED}
  VHDR_INQUEUE                        = $00000004;  { Reserved for driver }
  {$EXTERNALSYM VHDR_INQUEUE}
  VHDR_KEYFRAME                       = $00000008;  { Key Frame }
  {$EXTERNALSYM VHDR_KEYFRAME}
  VHDR_VALID                          = $0000000F;  { valid flags } { ;Internal }
  {$EXTERNALSYM VHDR_VALID}

  // Channel capabilities structure

type

  PCHANNEL_CAPS = ^channel_caps_tag;
  {$EXTERNALSYM PCHANNEL_CAPS}
  channel_caps_tag = record
    dwFlags: DWORD;                  { Capability flags}
    dwSrcRectXMod: DWORD;            { Granularity of src rect in x }
    dwSrcRectYMod: DWORD;            { Granularity of src rect in y }
    dwSrcRectWidthMod: DWORD;        { Granularity of src rect width }
    dwSrcRectHeightMod: DWORD;       { Granularity of src rect height }
    dwDstRectXMod: DWORD;            { Granularity of dst rect in x }
    dwDstRectYMod: DWORD;            { Granularity of dst rect in y }
    dwDstRectWidthMod: DWORD;        { Granularity of dst rect width }
    dwDstRectHeightMod: DWORD;      { Granularity of dst rect height }
  end;
  {$EXTERNALSYM channel_caps_tag}
  CHANNEL_CAPS = channel_caps_tag;
  {$EXTERNALSYM CHANNEL_CAPS}
  LPCHANNEL_CAPS = channel_caps_tag;
  {$EXTERNALSYM LPCHANNEL_CAPS}

  // dwFlags of CHANNEL_CAPS

const

  VCAPS_OVERLAY                       = $00000001;  { overlay channel }
  {$EXTERNALSYM VCAPS_OVERLAY}
  VCAPS_SRC_CAN_CLIP                  = $00000002;  { src rect can clip }
  {$EXTERNALSYM VCAPS_SRC_CAN_CLIP}
  VCAPS_DST_CAN_CLIP                  = $00000004;  { dst rect can clip }
  {$EXTERNALSYM VCAPS_DST_CAN_CLIP}
  VCAPS_CAN_SCALE                     = $00000008;  { allows src != dst }
  {$EXTERNALSYM VCAPS_CAN_SCALE}


  //****************************************************************************
  //
	//		API Flags
  //
  //****************************************************************************

  // Types of channels to open with the videoOpen function

const

  VIDEO_EXTERNALIN                    = $0001;
  {$EXTERNALSYM VIDEO_EXTERNALIN}
  VIDEO_EXTERNALOUT                   = $0002;
  {$EXTERNALSYM VIDEO_EXTERNALOUT}
  VIDEO_IN                            = $0004;
  {$EXTERNALSYM VIDEO_IN}
  VIDEO_OUT                           = $0008;
  {$EXTERNALSYM VIDEO_OUT}

  // Is a driver dialog available for this channel?
  VIDEO_DLG_QUERY                     = $0010;
  {$EXTERNALSYM VIDEO_DLG_QUERY}

  // videoConfigure (both GET and SET)
  VIDEO_CONFIGURE_QUERY               = $8000;
  {$EXTERNALSYM VIDEO_CONFIGURE_QUERY}

  // videoConfigure (SET only)
  VIDEO_CONFIGURE_SET                 = $1000;
  {$EXTERNALSYM VIDEO_CONFIGURE_SET}

  // videoConfigure (GET only)
  VIDEO_CONFIGURE_GET                 = $2000;
  {$EXTERNALSYM VIDEO_CONFIGURE_GET}
  VIDEO_CONFIGURE_QUERYSIZE           = $0001;
  {$EXTERNALSYM VIDEO_CONFIGURE_QUERYSIZE}

  VIDEO_CONFIGURE_CURRENT             = $0010;
  {$EXTERNALSYM VIDEO_CONFIGURE_CURRENT}
  VIDEO_CONFIGURE_NOMINAL             = $0020;
  {$EXTERNALSYM VIDEO_CONFIGURE_NOMINAL}
  VIDEO_CONFIGURE_MIN                 = $0040;
  {$EXTERNALSYM VIDEO_CONFIGURE_MIN}
  VIDEO_CONFIGURE_MAX                 = $0080;
  {$EXTERNALSYM VIDEO_CONFIGURE_MAX}

  //****************************************************************************
  //
	//		CONFIGURE MESSAGES
  //
  //****************************************************************************

  DVM_USER                            = $4000;
  {$EXTERNALSYM DVM_USER}

  DVM_CONFIGURE_START                 = $1000;
  {$EXTERNALSYM DVM_CONFIGURE_START}
  DVM_CONFIGURE_END                   = $1FFF;
  {$EXTERNALSYM DVM_CONFIGURE_END}

  DVM_PALETTE                         = (DVM_CONFIGURE_START + 1);
  {$EXTERNALSYM DVM_PALETTE}
  DVM_FORMAT                          = (DVM_CONFIGURE_START + 2);
  {$EXTERNALSYM DVM_FORMAT}
  DVM_PALETTERGB555                   = (DVM_CONFIGURE_START + 3);
  {$EXTERNALSYM DVM_PALETTERGB555}
  DVM_SRC_RECT                        = (DVM_CONFIGURE_START + 4);
  {$EXTERNALSYM DVM_SRC_RECT}
  DVM_DST_RECT                        = (DVM_CONFIGURE_START + 5);
  {$EXTERNALSYM DVM_DST_RECT}


  //****************************************************************************
  //
  //  AVICAP - Window class for AVI capture
  //
  //****************************************************************************

  function AVICapSM(_hWnd: HWND;
                    m: UINT;
                    w: WPARAM;
                    l: LPARAM): DWORD;
  {$EXTERNALSYM AVICapSM}

  // ------------------------------------------------------------------
  //  Window Messages  WM_CAP... which can be sent to an AVICAP window
  // ------------------------------------------------------------------


  // UNICODE
  //
  // The Win32 version of AVICAP on NT supports UNICODE applications:
  // for each API or message that takes a char or string parameter, there are
  // two versions, ApiNameA and ApiNameW. The default name ApiName is #defined
  // to one or other depending on whether UNICODE is defined. Apps can call
  // the A and W apis directly, and mix them.
  //
  // The 32-bit AVICAP on NT uses unicode exclusively internally.
  // ApiNameA() will be implemented as a call to ApiNameW() together with
  // translation of strings.

const

  // Defines start of the message range
  WM_CAP_START                        = WM_USER;
  {$EXTERNALSYM WM_CAP_START}

  // start of unicode messages
  WM_CAP_UNICODE_START                = WM_USER + 100;
  {$EXTERNALSYM WM_CAP_UNICODE_START}

  WM_CAP_GET_CAPSTREAMPTR             = (WM_CAP_START + 1);
  {$EXTERNALSYM WM_CAP_GET_CAPSTREAMPTR}

  WM_CAP_SET_CALLBACK_ERRORW          = (WM_CAP_UNICODE_START + 2);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_ERRORW}
  WM_CAP_SET_CALLBACK_STATUSW         = (WM_CAP_UNICODE_START + 3);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_STATUSW}
  WM_CAP_SET_CALLBACK_ERRORA          = (WM_CAP_START + 2);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_ERRORA}
  WM_CAP_SET_CALLBACK_STATUSA         = (WM_CAP_START + 3);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_STATUSA}

{$IFDEF UNICODE}
  WM_CAP_SET_CALLBACK_ERROR           = WM_CAP_SET_CALLBACK_ERRORW;
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_ERROR}
  WM_CAP_SET_CALLBACK_STATUS          = WM_CAP_SET_CALLBACK_STATUSW;
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_STATUS}
{$ELSE}
  WM_CAP_SET_CALLBACK_ERROR           = WM_CAP_SET_CALLBACK_ERRORA;
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_ERROR}
  WM_CAP_SET_CALLBACK_STATUS          = WM_CAP_SET_CALLBACK_STATUSA;
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_STATUS}
{$ENDIF}


  WM_CAP_SET_CALLBACK_YIELD           = (WM_CAP_START + 4);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_YIELD}
  WM_CAP_SET_CALLBACK_FRAME           = (WM_CAP_START + 5);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_FRAME}
  WM_CAP_SET_CALLBACK_VIDEOSTREAM     = (WM_CAP_START + 6);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_VIDEOSTREAM}
  WM_CAP_SET_CALLBACK_WAVESTREAM      = (WM_CAP_START + 7);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_WAVESTREAM}
  WM_CAP_GET_USER_DATA                = (WM_CAP_START + 8);
  {$EXTERNALSYM WM_CAP_GET_USER_DATA}
  WM_CAP_SET_USER_DATA                = (WM_CAP_START + 9);
  {$EXTERNALSYM WM_CAP_SET_USER_DATA}

  WM_CAP_DRIVER_CONNECT               = (WM_CAP_START + 10);
    {$EXTERNALSYM WM_CAP_DRIVER_CONNECT}
  WM_CAP_DRIVER_DISCONNECT            = (WM_CAP_START + 11);
  {$EXTERNALSYM WM_CAP_DRIVER_DISCONNECT}

  WM_CAP_DRIVER_GET_NAMEA             = (WM_CAP_START + 12);
  {$EXTERNALSYM WM_CAP_DRIVER_GET_NAMEA}
  WM_CAP_DRIVER_GET_VERSIONA          = (WM_CAP_START + 13);
  {$EXTERNALSYM WM_CAP_DRIVER_GET_VERSIONA}
  WM_CAP_DRIVER_GET_NAMEW             = (WM_CAP_UNICODE_START + 12);
  {$EXTERNALSYM WM_CAP_DRIVER_GET_NAMEW}
  WM_CAP_DRIVER_GET_VERSIONW          = (WM_CAP_UNICODE_START + 13);
  {$EXTERNALSYM WM_CAP_DRIVER_GET_VERSIONW}

{$IFDEF UNICODE}
  WM_CAP_DRIVER_GET_NAME              = WM_CAP_DRIVER_GET_NAMEW;
  {$EXTERNALSYM WM_CAP_DRIVER_GET_NAME}
  WM_CAP_DRIVER_GET_VERSION           = WM_CAP_DRIVER_GET_VERSIONW;
  {$EXTERNALSYM WM_CAP_DRIVER_GET_VERSION}
{$ELSE}
  WM_CAP_DRIVER_GET_NAME              = WM_CAP_DRIVER_GET_NAMEA;
  {$EXTERNALSYM WM_CAP_DRIVER_GET_NAME}
  WM_CAP_DRIVER_GET_VERSION           = WM_CAP_DRIVER_GET_VERSIONA;
  {$EXTERNALSYM WM_CAP_DRIVER_GET_VERSION}
{$ENDIF}

  {$EXTERNALSYM WM_CAP_DRIVER_GET_CAPS}
  WM_CAP_DRIVER_GET_CAPS              = (WM_CAP_START + 14);

  {$EXTERNALSYM WM_CAP_FILE_SET_CAPTURE_FILEA}
  WM_CAP_FILE_SET_CAPTURE_FILEA       = (WM_CAP_START + 20);
  {$EXTERNALSYM WM_CAP_FILE_GET_CAPTURE_FILEA}
  WM_CAP_FILE_GET_CAPTURE_FILEA       = (WM_CAP_START + 21);
  {$EXTERNALSYM WM_CAP_FILE_SAVEASA}
  WM_CAP_FILE_SAVEASA                 = (WM_CAP_START + 23);
  {$EXTERNALSYM WM_CAP_FILE_SAVEDIBA}
  WM_CAP_FILE_SAVEDIBA                = (WM_CAP_START + 25);
  {$EXTERNALSYM WM_CAP_FILE_SET_CAPTURE_FILEW}
  WM_CAP_FILE_SET_CAPTURE_FILEW       = (WM_CAP_UNICODE_START + 20);
  {$EXTERNALSYM WM_CAP_FILE_GET_CAPTURE_FILEW}
  WM_CAP_FILE_GET_CAPTURE_FILEW       = (WM_CAP_UNICODE_START + 21);
  {$EXTERNALSYM WM_CAP_FILE_SAVEASW}
  WM_CAP_FILE_SAVEASW                 = (WM_CAP_UNICODE_START + 23);
  {$EXTERNALSYM WM_CAP_FILE_SAVEDIBW}
  WM_CAP_FILE_SAVEDIBW                = (WM_CAP_UNICODE_START + 25);

{$IFDEF UNICODE}
  {$EXTERNALSYM WM_CAP_FILE_SET_CAPTURE_FILE}
  WM_CAP_FILE_SET_CAPTURE_FILE        = WM_CAP_FILE_SET_CAPTURE_FILEW;
  {$EXTERNALSYM WM_CAP_FILE_GET_CAPTURE_FILE}
  WM_CAP_FILE_GET_CAPTURE_FILE        = WM_CAP_FILE_GET_CAPTURE_FILEW;
  {$EXTERNALSYM WM_CAP_FILE_SAVEAS}
  WM_CAP_FILE_SAVEAS                  = WM_CAP_FILE_SAVEASW;
  {$EXTERNALSYM WM_CAP_FILE_SAVEDIB}
  WM_CAP_FILE_SAVEDIB                 = WM_CAP_FILE_SAVEDIBW;
{$ELSE}
  {$EXTERNALSYM WM_CAP_FILE_SET_CAPTURE_FILE}
  WM_CAP_FILE_SET_CAPTURE_FILE        = WM_CAP_FILE_SET_CAPTURE_FILEA;
  {$EXTERNALSYM WM_CAP_FILE_GET_CAPTURE_FILE}
  WM_CAP_FILE_GET_CAPTURE_FILE        = WM_CAP_FILE_GET_CAPTURE_FILEA;
  {$EXTERNALSYM WM_CAP_FILE_SAVEAS}
  WM_CAP_FILE_SAVEAS                  = WM_CAP_FILE_SAVEASA;
  {$EXTERNALSYM WM_CAP_FILE_SAVEDIB}
  WM_CAP_FILE_SAVEDIB                 = WM_CAP_FILE_SAVEDIBA;
{$ENDIF}

  // out of order to save on ifdefs
  WM_CAP_FILE_ALLOCATE                = (WM_CAP_START + 22);
  {$EXTERNALSYM WM_CAP_FILE_ALLOCATE}
  WM_CAP_FILE_SET_INFOCHUNK           = (WM_CAP_START + 24);
  {$EXTERNALSYM WM_CAP_FILE_SET_INFOCHUNK}

  WM_CAP_EDIT_COPY                    = (WM_CAP_START + 30);
  {$EXTERNALSYM WM_CAP_EDIT_COPY}

  WM_CAP_SET_AUDIOFORMAT              = (WM_CAP_START + 35);
  {$EXTERNALSYM WM_CAP_SET_AUDIOFORMAT}
  WM_CAP_GET_AUDIOFORMAT              = (WM_CAP_START + 36);
  {$EXTERNALSYM WM_CAP_GET_AUDIOFORMAT}

  WM_CAP_DLG_VIDEOFORMAT              = (WM_CAP_START + 41);
  {$EXTERNALSYM WM_CAP_DLG_VIDEOFORMAT}
  WM_CAP_DLG_VIDEOSOURCE              = (WM_CAP_START + 42);
  {$EXTERNALSYM WM_CAP_DLG_VIDEOSOURCE}
  WM_CAP_DLG_VIDEODISPLAY             = (WM_CAP_START + 43);
  {$EXTERNALSYM WM_CAP_DLG_VIDEODISPLAY}
  WM_CAP_GET_VIDEOFORMAT              = (WM_CAP_START + 44);
  {$EXTERNALSYM WM_CAP_GET_VIDEOFORMAT}
  WM_CAP_SET_VIDEOFORMAT              = (WM_CAP_START + 45);
  {$EXTERNALSYM WM_CAP_SET_VIDEOFORMAT}
  WM_CAP_DLG_VIDEOCOMPRESSION         = (WM_CAP_START + 46);
  {$EXTERNALSYM WM_CAP_DLG_VIDEOCOMPRESSION}

  WM_CAP_SET_PREVIEW                  = (WM_CAP_START + 50);
  {$EXTERNALSYM WM_CAP_SET_PREVIEW}
  WM_CAP_SET_OVERLAY                  = (WM_CAP_START + 51);
  {$EXTERNALSYM WM_CAP_SET_OVERLAY}
  WM_CAP_SET_PREVIEWRATE              = (WM_CAP_START + 52);
  {$EXTERNALSYM WM_CAP_SET_PREVIEWRATE}
  WM_CAP_SET_SCALE                    = (WM_CAP_START + 53);
  {$EXTERNALSYM WM_CAP_SET_SCALE}
  WM_CAP_GET_STATUS                   = (WM_CAP_START + 54);
  {$EXTERNALSYM WM_CAP_GET_STATUS}
  WM_CAP_SET_SCROLL                   = (WM_CAP_START + 55);
  {$EXTERNALSYM WM_CAP_SET_SCROLL}

  WM_CAP_GRAB_FRAME                   = (WM_CAP_START + 60);
  {$EXTERNALSYM WM_CAP_GRAB_FRAME}
  WM_CAP_GRAB_FRAME_NOSTOP            = (WM_CAP_START + 61);
  {$EXTERNALSYM WM_CAP_GRAB_FRAME_NOSTOP}

  WM_CAP_SEQUENCE                     = (WM_CAP_START + 62);
  {$EXTERNALSYM WM_CAP_SEQUENCE}
  WM_CAP_SEQUENCE_NOFILE              = (WM_CAP_START + 63);
  {$EXTERNALSYM WM_CAP_SEQUENCE_NOFILE}
  WM_CAP_SET_SEQUENCE_SETUP           = (WM_CAP_START + 64);
  {$EXTERNALSYM WM_CAP_SET_SEQUENCE_SETUP}
  WM_CAP_GET_SEQUENCE_SETUP           = (WM_CAP_START + 65);
  {$EXTERNALSYM WM_CAP_GET_SEQUENCE_SETUP}

  WM_CAP_SET_MCI_DEVICEA              = (WM_CAP_START + 66);
  {$EXTERNALSYM WM_CAP_SET_MCI_DEVICEA}
  WM_CAP_GET_MCI_DEVICEA              = (WM_CAP_START + 67);
  {$EXTERNALSYM WM_CAP_GET_MCI_DEVICEA}
  WM_CAP_SET_MCI_DEVICEW              = (WM_CAP_UNICODE_START + 66);
  {$EXTERNALSYM WM_CAP_SET_MCI_DEVICEW}
  WM_CAP_GET_MCI_DEVICEW              = (WM_CAP_UNICODE_START + 67);
  {$EXTERNALSYM WM_CAP_GET_MCI_DEVICEW}

{$IFDEF UNICODE}
  WM_CAP_SET_MCI_DEVICE               = WM_CAP_SET_MCI_DEVICEW;
  {$EXTERNALSYM WM_CAP_SET_MCI_DEVICE}
  WM_CAP_GET_MCI_DEVICE               = WM_CAP_GET_MCI_DEVICEW;
  {$EXTERNALSYM WM_CAP_GET_MCI_DEVICE}
{$ELSE}
  WM_CAP_SET_MCI_DEVICE               = WM_CAP_SET_MCI_DEVICEA;
  {$EXTERNALSYM WM_CAP_SET_MCI_DEVICE}
  WM_CAP_GET_MCI_DEVICE               = WM_CAP_GET_MCI_DEVICEA;
  {$EXTERNALSYM WM_CAP_GET_MCI_DEVICE}
{$ENDIF}


  WM_CAP_STOP                         = (WM_CAP_START + 68);
  {$EXTERNALSYM WM_CAP_STOP}
  WM_CAP_ABORT                        = (WM_CAP_START + 69);
  {$EXTERNALSYM WM_CAP_ABORT}

  WM_CAP_SINGLE_FRAME_OPEN            = (WM_CAP_START + 70);
  {$EXTERNALSYM WM_CAP_SINGLE_FRAME_OPEN}
  WM_CAP_SINGLE_FRAME_CLOSE           = (WM_CAP_START + 71);
  {$EXTERNALSYM WM_CAP_SINGLE_FRAME_CLOSE}
  WM_CAP_SINGLE_FRAME                 = (WM_CAP_START + 72);
  {$EXTERNALSYM WM_CAP_SINGLE_FRAME}

  WM_CAP_PAL_OPENA                    = (WM_CAP_START + 80);
  {$EXTERNALSYM WM_CAP_PAL_OPENA}
  WM_CAP_PAL_SAVEA                    = (WM_CAP_START + 81);
  {$EXTERNALSYM WM_CAP_PAL_SAVEA}
  WM_CAP_PAL_OPENW                    = (WM_CAP_UNICODE_START + 80);
  {$EXTERNALSYM WM_CAP_PAL_OPENW}
  WM_CAP_PAL_SAVEW                    = (WM_CAP_UNICODE_START + 81);
  {$EXTERNALSYM WM_CAP_PAL_SAVEW}

{$IFDEF UNICODE}
  WM_CAP_PAL_OPEN                     = WM_CAP_PAL_OPENW;
  {$EXTERNALSYM WM_CAP_PAL_OPEN}
  WM_CAP_PAL_SAVE                     = WM_CAP_PAL_SAVEW;
  {$EXTERNALSYM WM_CAP_PAL_SAVE}
{$ELSE}
  WM_CAP_PAL_OPEN                     = WM_CAP_PAL_OPENA;
  {$EXTERNALSYM WM_CAP_PAL_OPEN}
  WM_CAP_PAL_SAVE                     = WM_CAP_PAL_SAVEA;
  {$EXTERNALSYM WM_CAP_PAL_SAVE}
{$ENDIF}

  WM_CAP_PAL_PASTE                    = (WM_CAP_START + 82);
  {$EXTERNALSYM WM_CAP_PAL_PASTE}
  WM_CAP_PAL_AUTOCREATE               = (WM_CAP_START + 83);
  {$EXTERNALSYM WM_CAP_PAL_AUTOCREATE}
  WM_CAP_PAL_MANUALCREATE             = (WM_CAP_START + 84);
  {$EXTERNALSYM WM_CAP_PAL_MANUALCREATE}

  // Following added post VFW 1.1
  WM_CAP_SET_CALLBACK_CAPCONTROL      = (WM_CAP_START + 85);
  {$EXTERNALSYM WM_CAP_SET_CALLBACK_CAPCONTROL}


  // Defines end of the message range
  WM_CAP_UNICODE_END                  = WM_CAP_PAL_SAVEW;
  {$EXTERNALSYM WM_CAP_UNICODE_END}
  WM_CAP_END                          = WM_CAP_UNICODE_END;
  {$EXTERNALSYM WM_CAP_END}


  // ------------------------------------------------------------------
  //  Structures   moved to here
  // ------------------------------------------------------------------

type

  PCAPDRIVERCAPS = ^tagCapDriverCaps;
  {$EXTERNALSYM PCAPDRIVERCAPS}
  tagCapDriverCaps = record
    wDeviceIndex: UINT;              // Driver index in system.ini
    fHasOverlay: BOOL;               // Can device overlay?
    fHasDlgVideoSource: BOOL;        // Has Video source dlg?
    fHasDlgVideoFormat: BOOL;        // Has Format dlg?
    fHasDlgVideoDisplay: BOOL;       // Has External out dlg?
    fCaptureInitialized: BOOL;       // Driver ready to capture?
    fDriverSuppliesPalettes: BOOL;   // Can driver make palettes?
                                     // following always NULL on Win32.
    hVideoIn: THandle;               // Driver In channel
    hVideoOut: THandle;              // Driver Out channel
    hVideoExtIn: THandle;            // Driver Ext In channel
    hVideoExtOut: THandle;          // Driver Ext Out channel
  end;
  {$EXTERNALSYM tagCapDriverCaps}
  CAPDRIVERCAPS = tagCapDriverCaps;
  {$EXTERNALSYM CAPDRIVERCAPS}
  LPCAPDRIVERCAPS = ^tagCapDriverCaps;
  {$EXTERNALSYM LPCAPDRIVERCAPS}

  PCapstatus = ^tagCapStatus;
  {$EXTERNALSYM tagCapStatus}
  tagCapStatus = record
    uiImageWidth: UINT;                   // Width of the image
    uiImageHeight: UINT;                  // Height of the image
    fLiveWindow: BOOL;                    // Now Previewing video?
    fOverlayWindow: BOOL;                 // Now Overlaying video?
    fScale: BOOL;                         // Scale image to client?
    ptScroll: POINT;                      // Scroll position
    fUsingDefaultPalette: BOOL;           // Using default driver palette?
    fAudioHardware: BOOL;                 // Audio hardware present?
    fCapFileExists: BOOL;                 // Does capture file exist?
    dwCurrentVideoFrame: DWORD;           // # of video frames cap'td
    dwCurrentVideoFramesDropped: DWORD;   // # of video frames dropped
    dwCurrentWaveSamples: DWORD;          // # of wave samples cap'td
    dwCurrentTimeElapsedMS: DWORD;        // Elapsed capture duration
    hPalCurrent: HPALETTE;                // Current palette in use
    fCapturingNow: BOOL;                  // Capture in progress?
    dwReturn: DWORD;                      // Error value after any operation
    wNumVideoAllocated: UINT;             // Actual number of video buffers
    wNumAudioAllocated: UINT;            // Actual number of audio buffers
  end;
  CAPSTATUS = tagCapStatus;
  {$EXTERNALSYM CAPSTATUS}
  LPCAPSTATUS = ^tagCapStatus;
  {$EXTERNALSYM LPCAPSTATUS}

                                       // Default values in parenthesis
  PCAPTUREPARMS = ^tagCaptureParms;
  {$EXTERNALSYM PCAPTUREPARMS}
  tagCaptureParms = record
    dwRequestMicroSecPerFrame: DWORD;  // Requested capture rate
    fMakeUserHitOKToCapture: BOOL;     // Show "Hit OK to cap" dlg?
    wPercentDropForError: UINT;        // Give error msg if > (10%)
    fYield: BOOL;                      // Capture via background task?
    dwIndexSize: DWORD;                // Max index size in frames (32K)
    wChunkGranularity: UINT;           // Junk chunk granularity (2K)
    fUsingDOSMemory: BOOL;             // Use DOS buffers?
    wNumVideoRequested: UINT;          // # video buffers, If 0, autocalc
    fCaptureAudio: BOOL;               // Capture audio?
    wNumAudioRequested: UINT;          // # audio buffers, If 0, autocalc
    vKeyAbort: UINT;                   // Virtual key causing abort
    fAbortLeftMouse: BOOL;             // Abort on left mouse?
    fAbortRightMouse: BOOL;            // Abort on right mouse?
    fLimitEnabled: BOOL;               // Use wTimeLimit?
    wTimeLimit: UINT;                  // Seconds to capture
    fMCIControl: BOOL;                 // Use MCI video source?
    fStepMCIDevice: BOOL;              // Step MCI device?
    dwMCIStartTime: DWORD;             // Time to start in MS
    dwMCIStopTime: DWORD;              // Time to stop in MS
    fStepCaptureAt2x: BOOL;            // Perform spatial averaging 2x
    wStepCaptureAverageFrames: UINT;   // Temporal average n Frames
    dwAudioBufferSize: DWORD;          // Size of audio bufs (0 = default)
    fDisableWriteCache: BOOL;          // Attempt to disable write cache
    AVStreamMaster: UINT;              // Which stream controls length?
  end;
  {$EXTERNALSYM tagCaptureParms}
  CAPTUREPARMS = tagCaptureParms;
  {$EXTERNALSYM CAPTUREPARMS}
  LPCAPTUREPARMS = ^tagCaptureParms;
  {$EXTERNALSYM LPCAPTUREPARMS}

// moved to here
  PCAPINFOCHUNK = ^tagCapInfoChunk;
  {$EXTERNALSYM PCAPINFOCHUNK}
  tagCapInfoChunk = record
    fccInfoID: FOURCC;               // Chunk ID, "ICOP" for copyright
    lpData: Pointer;                 // pointer to data
    cbData: LONG;                    // size of lpData
  end;
  {$EXTERNALSYM tagCapInfoChunk}
  CAPINFOCHUNK = tagCapInfoChunk;
  {$EXTERNALSYM CAPINFOCHUNK}
  LPCAPINFOCHUNK = ^tagCapInfoChunk;
  {$EXTERNALSYM LPCAPINFOCHUNK}


  // moved to here
  // ------------------------------------------------------------------
  //  Callback Definitions
  // ------------------------------------------------------------------

type

  CAPYIELDCALLBACK = function(_hWnd: HWND): LRESULT; stdcall;

  CAPSTATUSCALLBACKW = function(_hWnd: HWND;
                                nID: Integer;
                                lpsz: PWideChar): LRESULT; stdcall;

  CAPERRORCALLBACKW = function(_hWnd: HWND;
                               nID: Integer;
                               lpsz: PWideChar): LRESULT; stdcall;

  CAPSTATUSCALLBACKA = function(_hWnd: HWND;
                                nID: Integer;
                                lpsz: PAnsiChar): LRESULT; stdcall;

  CAPERRORCALLBACKA = function(_hWnd: HWND;
                               nID: Integer;
                               lpsz: PAnsiChar): LRESULT; stdcall;

{$IFDEF UNICODE}
  {$EXTERNALSYM CAPSTATUSCALLBACK}
  CAPSTATUSCALLBACK                   = CAPSTATUSCALLBACKW;
  {$EXTERNALSYM CAPERRORCALLBACK}
  CAPERRORCALLBACK                    = CAPERRORCALLBACKW;
{$ELSE}
  {$EXTERNALSYM CAPSTATUSCALLBACK}
  CAPSTATUSCALLBACK                   = CAPSTATUSCALLBACKA;
  {$EXTERNALSYM CAPERRORCALLBACK}
  CAPERRORCALLBACK                    = CAPERRORCALLBACKA;
{$ENDIF}

  CAPVIDEOCALLBACK = function(_hWnd: HWND;
                              lpVHdr: LPVIDEOHDR): LRESULT; stdcall;
  {$EXTERNALSYM CAPVIDEOCALLBACK}

  CAPWAVECALLBACK = function(_hWnd: HWND;
                             lpWHdr: LPWAVEHDR): LRESULT; stdcall;
  {$EXTERNALSYM CAPWAVECALLBACK}

  CAPCONTROLCALLBACK = function(_hWnd: HWND;
                                nState: Integer): LRESULT; stdcall;
  {$EXTERNALSYM CAPCONTROLCALLBACK}

  // ------------------------------------------------------------------
  //  Message crackers for above
  // ------------------------------------------------------------------

  // message wrapper macros are defined for the default messages only. Apps
  // that wish to mix Ansi and UNICODE message sending will have to
  // reference the _A and _W messages directly

  function capSetCallbackOnError(_hWnd: HWND;
                                 fpProc: CAPERRORCALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnError}

  function capSetCallbackOnStatus(_hWnd: HWND;
                                  fpProc: CAPSTATUSCALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnStatus}

  function capSetCallbackOnYield(_hWnd: HWND;
                                 fpProc: CAPYIELDCALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnYield}

  function capSetCallbackOnFrame(_hWnd: HWND;
                                 fpProc: CAPVIDEOCALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnFrame}

  function capSetCallbackOnVideoStream(_hWnd: HWND;
                                       fpProc: CAPVIDEOCALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnVideoStream}

  function capSetCallbackOnWaveStream(_hWnd: HWND;
                                      fpProc: CAPWAVECALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnWaveStream}

  function capSetCallbackOnCapControl(_hWnd: HWND;
                                      fpProc: CAPCONTROLCALLBACK): BOOL; inline;
  {$EXTERNALSYM capSetCallbackOnCapControl}

  function capSetUserData(_hWnd: DWORD;
                          lUser: HWND): BOOL; inline;
  {$EXTERNALSYM capSetUserData}

  function capGetUserData(_hwnd: HWND): DWORD; inline;
  {$EXTERNALSYM capGetUserData}

  function capDriverConnect(_hWnd: HWND;
                            i: Integer): BOOL; inline;
  {$EXTERNALSYM capDriverConnect}

  function capDriverDisconnect(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capDriverDisconnect}

  function capDriverGetName(_hWnd: HWND;
                            szName: LPCSTR;
                            wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capDriverGetName}

  function capDriverGetVersion(_hWnd: HWND;
                               szVer: LPSTR;
                               wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capDriverGetVersion}

  function capDriverGetCaps(_hWnd: HWND;
                            s: PCAPDRIVERCAPS;
                            wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capDriverGetCaps}

  function capFileSetCaptureFile(_hWnd: HWND;
                                 szName: LPCSTR): BOOL; inline;
  {$EXTERNALSYM capFileSetCaptureFile}

  function capFileGetCaptureFile(_hWnd: HWND;
                                 szName: LPCSTR; wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capFileGetCaptureFile}

  function capFileAlloc(_hWnd: HWND;
                        dwSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capFileAlloc}

  function capFileSaveAs(_hWnd: HWND;
                         szName: LPCSTR): BOOL; inline;
  {$EXTERNALSYM capFileSaveAs}

  function capFileSetInfoChunk(_hWnd: HWND;
                               lpInfoChunk: PCAPINFOCHUNK): BOOL; inline;
  {$EXTERNALSYM capFileSetInfoChunk}

  function capFileSaveDIB(_hWnd: HWND;
                          szName: LPCSTR): BOOL; inline;
  {$EXTERNALSYM capFileSaveDIB}

  function capEditCopy(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capEditCopy}

  function capSetAudioFormat(_hWnd: HWND;
                             s: LPWAVEFORMATEX;
                             wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capSetAudioFormat}

  function capGetAudioFormat(_hWnd: HWND;
                             s: LPWAVEFORMATEX;
                             wSize: DWORD): DWORD; inline;
  {$EXTERNALSYM capGetAudioFormat}

  function capGetAudioFormatSize(_hwnd: HWND): DWORD; inline;
  {$EXTERNALSYM capGetAudioFormatSize}

  function capDlgVideoFormat(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM capDlgVideoFormat}

  function capDlgVideoSource(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capDlgVideoSource}

  function capDlgVideoDisplay(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capDlgVideoDisplay}

  function capDlgVideoCompression(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capDlgVideoCompression}

  function capGetVideoFormat(_hWnd: DWORD;
                             s: Pointer;
                             wSize: DWORD): DWORD; inline;
  {$EXTERNALSYM capGetVideoFormat}

  function capGetVideoFormatSize(_hwnd: HWND): DWORD; inline;
  {$EXTERNALSYM capGetVideoFormatSize}

  function capSetVideoFormat(_hWnd: HWND;
                             s: Pointer;
                             wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capSetVideoFormat}

  function capPreview(_hWnd: HWND;
                      f: BOOL): BOOL; inline;
  {$EXTERNALSYM capPreview}

  function capPreviewRate(_hWnd: HWND;
                          wMS: DWORD): BOOL; inline;
  {$EXTERNALSYM capPreviewRate}

  function capOverlay(_hWnd: HWND;
                      f: BOOL): BOOL; inline;
  {$EXTERNALSYM capOverlay}

  function capPreviewScale(_hWnd: HWND;
                           f: BOOL): BOOL; inline;
  {$EXTERNALSYM capPreviewScale}

  function capGetStatus(_hWnd: HWND;
                        s: LPCAPSTATUS;
                        wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capGetStatus}

  function capSetScrollPos(_hWnd: HWND;
                           lpP: PPoint): BOOL; inline;
  {$EXTERNALSYM capSetScrollPos}

  function capGrabFrame(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capGrabFrame}

  function capGrabFrameNoStop(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capGrabFrameNoStop}

  function capCaptureSequence(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capCaptureSequence}

  function capCaptureSequenceNoFile(_hwnd: DWORD): BOOL; inline;
  {$EXTERNALSYM capCaptureSequenceNoFile}

  function capCaptureStop(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capCaptureStop}

  function capCaptureAbort(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capCaptureAbort}

  function capCaptureSingleFrameOpen(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capCaptureSingleFrameOpen}

  function capCaptureSingleFrameClose(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capCaptureSingleFrameClose}

  function capCaptureSingleFrame(_hwnd: HWND): BOOL; inline;
  {$EXTERNALSYM capCaptureSingleFrame}

  function capCaptureGetSetup(_hWnd: HWND;
                              s: LPCAPTUREPARMS;
                              wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capCaptureGetSetup}

  function capCaptureSetSetup(_hWnd: HWND;
                              s: LPCAPTUREPARMS;
                              wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capCaptureSetSetup}

  function capSetMCIDeviceName(_hWnd: HWND;
                               szName: LPCSTR): BOOL; inline;
  {$EXTERNALSYM capSetMCIDeviceName}

  function capGetMCIDeviceName(_hWnd: HWND;
                               szName: LPCSTR;
                               wSize: DWORD): BOOL; inline;
  {$EXTERNALSYM capGetMCIDeviceName}

  function capPaletteOpen(_hWnd: HWND;
                          szName: LPCSTR): BOOL; inline;
  {$EXTERNALSYM capPaletteOpen}

  function capPaletteSave(_hWnd: HWND;
                          szName: LPCSTR): BOOL; inline;
  {$EXTERNALSYM capPaletteSave}

  function capPalettePaste(_hWnd: HWND): BOOL; inline;
  {$EXTERNALSYM capPalettePaste}

  function capPaletteAuto(_hWnd: HWND;
                          iFrames: DWORD;
                          iColors: Integer): BOOL; inline;
  {$EXTERNALSYM capPaletteAuto}

  function capPaletteManual(_hWnd: HWND;
                            fGrab: DWORD;
                            iColors: Integer): BOOL; inline;
  {$EXTERNALSYM capPaletteManual}


 // moved ^^

  // ------------------------------------------------------------------
  //  AVStreamMaster
  //  Since Audio and Video streams generally use non-synchronized capture
  //  clocks, this flag determines whether the audio stream is to be considered
  //  the master or controlling clock when writing the AVI file:
  //
  //  AVSTREAMMASTER_AUDIO  - Audio is master, video frame duration is forced
  //                          to match audio duration (VFW 1.0, 1.1 default)
  //  AVSTREAMMASTER_NONE   - No master, audio and video streams may be of
  //                          different lengths
  // ------------------------------------------------------------------

const

  {$EXTERNALSYM AVSTREAMMASTER_AUDIO}
  AVSTREAMMASTER_AUDIO                = 0;  { Audio master (VFW 1.0, 1.1) }
  {$EXTERNALSYM AVSTREAMMASTER_NONE}
  AVSTREAMMASTER_NONE                 = 1;  { No master }

// moved ^^

  // ------------------------------------------------------------------
  //  CapControlCallback states
  // ------------------------------------------------------------------

const

  CONTROLCALLBACK_PREROLL             = 1;  { Waiting to start capture }
  {$EXTERNALSYM CONTROLCALLBACK_PREROLL}
  CONTROLCALLBACK_CAPTURING           = 2;  { Now capturing }
  {$EXTERNALSYM CONTROLCALLBACK_CAPTURING}

  // ------------------------------------------------------------------
  //  The only exported functions from AVICAP.DLL
  // ------------------------------------------------------------------

{$IFDEF UNICODE}
  function capCreateCaptureWindowW(lpszWindowName: PWideChar;
                                   dwStyle: DWORD;
                                   x: Integer;
                                   y: Integer;
                                   nWidth: Integer;
                                   nHeight: Integer;
                                   {_In_opt_} hwndParent: HWND;
                                   nID: Integer): HWND; stdcall;
  {$EXTERNALSYM capCreateCaptureWindowW}

  function capGetDriverDescriptionW(wDriverIndex: UINT;
                                   {_Out_} lpszName: PWideChar;
                                   cbName: Integer;
                                   {_Out_} lpszVer: PWideChar;
                                   cbVer: Integer): BOOL; stdcall;
  {$EXTERNALSYM capGetDriverDescriptionW}
{$ELSE}
  function capCreateCaptureWindowA(lpszWindowName: PAnsiChar;
                                   dwStyle: DWORD;
                                   x: Integer;
                                   y: Integer;
                                   nWidth: Integer;
                                   nHeight: Integer;
                                   {_In_opt_} hwndParent: HWND;
                                   nID: Integer): HWND; stdcall;
  {$EXTERNALSYM capCreateCaptureWindowA}

  function capGetDriverDescriptionA(wDriverIndex: UINT;
                                    {_Out_} lpszName: PAnsiChar;
                                    cbName: Integer;
                                    {_Out_} lpszVer: PAnsiChar;
                                    cbVer: Integer): BOOL; stdcall;
  {$EXTERNALSYM capGetDriverDescriptionA}
{$ENDIF}


  { unicode }
  function capCreateCaptureWindow(lpszWindowName: PWideChar;
                                   dwStyle: DWORD;
                                   x: Integer;
                                   y: Integer;
                                   nWidth: Integer;
                                   nHeight: Integer;
                                   {_In_opt_} hwndParent: HWND;
                                   nID: Integer): HWND; stdcall;

  {$EXTERNALSYM capCreateCaptureWindow}
  function capGetDriverDescription(wDriverIndex: UINT;
                                   {_Out_} lpszName: PWideChar;
                                   cbName: Integer;
                                   {_Out_} lpszVer: PWideChar;
                                   cbVer: Integer): BOOL; stdcall;
  {$EXTERNALSYM capGetDriverDescription}


  // ------------------------------------------------------------------
  // New Information chunk IDs
  // ------------------------------------------------------------------

const

  infotypeDIGITIZATION_TIME  = ord('I') or (ord('D') shl 8) or (ord('I') shl 16) or (ord('T') shl 24);
  {$EXTERNALSYM infotypeDIGITIZATION_TIME}
  infotypeSMPTE_TIME         = ord('I') or (ord('S') shl 8) or (ord('M') shl 16) or (ord('P') shl 24);
  {$EXTERNALSYM infotypeSMPTE_TIME}

  // ------------------------------------------------------------------
  // String IDs from status and error callbacks
  // ------------------------------------------------------------------

const

  IDS_CAP_BEGIN                       = 300;  { "Capture Start" }
  {$EXTERNALSYM IDS_CAP_BEGIN}
  IDS_CAP_END                         = 301;  { "Capture End" }
  {$EXTERNALSYM IDS_CAP_END}

  IDS_CAP_INFO                        = 401;  { "%s" }
  {$EXTERNALSYM IDS_CAP_INFO}
  IDS_CAP_OUTOFMEM                    = 402;  { "Out of memory" }
  {$EXTERNALSYM IDS_CAP_OUTOFMEM}
  IDS_CAP_FILEEXISTS                  = 403;  { "File '%s' exists -- overwrite it?" }
  {$EXTERNALSYM IDS_CAP_FILEEXISTS}
  IDS_CAP_ERRORPALOPEN                = 404;  { "Error opening palette '%s'" }
  {$EXTERNALSYM IDS_CAP_ERRORPALOPEN}
  IDS_CAP_ERRORPALSAVE                = 405;  { "Error saving palette '%s'" }
  {$EXTERNALSYM IDS_CAP_ERRORPALSAVE}
  IDS_CAP_ERRORDIBSAVE                = 406;  { "Error saving frame '%s'" }
  {$EXTERNALSYM IDS_CAP_ERRORDIBSAVE}
  IDS_CAP_DEFAVIEXT                   = 407;  { "avi" }
  {$EXTERNALSYM IDS_CAP_DEFAVIEXT}
  IDS_CAP_DEFPALEXT                   = 408;  { "pal" }
  {$EXTERNALSYM IDS_CAP_DEFPALEXT}
  IDS_CAP_CANTOPEN                    = 409;  { "Cannot open '%s'" }
  {$EXTERNALSYM IDS_CAP_CANTOPEN}
  IDS_CAP_SEQ_MSGSTART                = 410;  { "Select OK to start capture\nof video sequence\nto %s." }
  {$EXTERNALSYM IDS_CAP_SEQ_MSGSTART}
  IDS_CAP_SEQ_MSGSTOP                 = 411;  { "Hit ESCAPE or click to end capture" }
  {$EXTERNALSYM IDS_CAP_SEQ_MSGSTOP}

  IDS_CAP_VIDEDITERR                  = 412;  { "An error occurred while trying to run VidEdit." }
  {$EXTERNALSYM IDS_CAP_VIDEDITERR}
  IDS_CAP_READONLYFILE                = 413;  { "The file '%s' is a read-only file." }
  {$EXTERNALSYM IDS_CAP_READONLYFILE}
  IDS_CAP_WRITEERROR                  = 414;  { "Unable to write to file '%s'.\nDisk may be full." }
  {$EXTERNALSYM IDS_CAP_WRITEERROR}
  IDS_CAP_NODISKSPACE                 = 415;  { "There is no space to create a capture file on the specified device." }
  {$EXTERNALSYM IDS_CAP_NODISKSPACE}
  IDS_CAP_SETFILESIZE                 = 416;  { "Set File Size" }
  {$EXTERNALSYM IDS_CAP_SETFILESIZE}
  IDS_CAP_SAVEASPERCENT               = 417;  { "SaveAs: %2ld%%  Hit Escape to abort." }
  {$EXTERNALSYM IDS_CAP_SAVEASPERCENT}

  IDS_CAP_DRIVER_ERROR                = 418;  { Driver specific error message }
  {$EXTERNALSYM IDS_CAP_DRIVER_ERROR}

  IDS_CAP_WAVE_OPEN_ERROR             = 419;  { "Error: Cannot open the wave input device.\nCheck sample size, frequency, and channels." }
  {$EXTERNALSYM IDS_CAP_WAVE_OPEN_ERROR}
  IDS_CAP_WAVE_ALLOC_ERROR            = 420;  { "Error: Out of memory for wave buffers." }
  {$EXTERNALSYM IDS_CAP_WAVE_ALLOC_ERROR}
  IDS_CAP_WAVE_PREPARE_ERROR          = 421;  { "Error: Cannot prepare wave buffers." }
  {$EXTERNALSYM IDS_CAP_WAVE_PREPARE_ERROR}
  IDS_CAP_WAVE_ADD_ERROR              = 422;  { "Error: Cannot add wave buffers." }
  {$EXTERNALSYM IDS_CAP_WAVE_ADD_ERROR}
  IDS_CAP_WAVE_SIZE_ERROR             = 423;  { "Error: Bad wave size." }
  {$EXTERNALSYM IDS_CAP_WAVE_SIZE_ERROR}

  IDS_CAP_VIDEO_OPEN_ERROR            = 424;  { "Error: Cannot open the video input device." }
  {$EXTERNALSYM IDS_CAP_VIDEO_OPEN_ERROR}
  IDS_CAP_VIDEO_ALLOC_ERROR           = 425;  { "Error: Out of memory for video buffers." }
  {$EXTERNALSYM IDS_CAP_VIDEO_ALLOC_ERROR}
  IDS_CAP_VIDEO_PREPARE_ERROR         = 426;  { "Error: Cannot prepare video buffers." }
  {$EXTERNALSYM IDS_CAP_VIDEO_PREPARE_ERROR}
  IDS_CAP_VIDEO_ADD_ERROR             = 427;  { "Error: Cannot add video buffers." }
  {$EXTERNALSYM IDS_CAP_VIDEO_ADD_ERROR}
  IDS_CAP_VIDEO_SIZE_ERROR            = 428;  { "Error: Bad video size." }
  {$EXTERNALSYM IDS_CAP_VIDEO_SIZE_ERROR}

  IDS_CAP_FILE_OPEN_ERROR             = 429;  { "Error: Cannot open capture file." }
  {$EXTERNALSYM IDS_CAP_FILE_OPEN_ERROR}
  IDS_CAP_FILE_WRITE_ERROR            = 430;  { "Error: Cannot write to capture file.  Disk may be full." }
  {$EXTERNALSYM IDS_CAP_FILE_WRITE_ERROR}
  IDS_CAP_RECORDING_ERROR             = 431;  { "Error: Cannot write to capture file.  Data rate too high or disk full." }
  {$EXTERNALSYM IDS_CAP_RECORDING_ERROR}
  IDS_CAP_RECORDING_ERROR2            = 432;  { "Error while recording" }
  {$EXTERNALSYM IDS_CAP_RECORDING_ERROR2}
  IDS_CAP_AVI_INIT_ERROR              = 433;  { "Error: Unable to initialize for capture." }
  {$EXTERNALSYM IDS_CAP_AVI_INIT_ERROR}
  IDS_CAP_NO_FRAME_CAP_ERROR          = 434;  { "Warning: No frames captured.\nConfirm that vertical sync interrupts\nare configured and enabled." }
  {$EXTERNALSYM IDS_CAP_NO_FRAME_CAP_ERROR}
  IDS_CAP_NO_PALETTE_WARN             = 435;  { "Warning: Using default palette." }
  {$EXTERNALSYM IDS_CAP_NO_PALETTE_WARN}
  IDS_CAP_MCI_CONTROL_ERROR           = 436;  { "Error: Unable to access MCI device." }
  {$EXTERNALSYM IDS_CAP_MCI_CONTROL_ERROR}
  IDS_CAP_MCI_CANT_STEP_ERROR         = 437;  { "Error: Unable to step MCI device." }
  {$EXTERNALSYM IDS_CAP_MCI_CANT_STEP_ERROR}
  IDS_CAP_NO_AUDIO_CAP_ERROR          = 438;  { "Error: No audio data captured.\nCheck audio card settings." }
  {$EXTERNALSYM IDS_CAP_NO_AUDIO_CAP_ERROR}
  IDS_CAP_AVI_DRAWDIB_ERROR           = 439;  { "Error: Unable to draw this data format." }
  {$EXTERNALSYM IDS_CAP_AVI_DRAWDIB_ERROR}
  IDS_CAP_COMPRESSOR_ERROR            = 440;  { "Error: Unable to initialize compressor." }
  {$EXTERNALSYM IDS_CAP_COMPRESSOR_ERROR}
  IDS_CAP_AUDIO_DROP_ERROR            = 441;  { "Error: Audio data was lost during capture, reduce capture rate." }
  {$EXTERNALSYM IDS_CAP_AUDIO_DROP_ERROR}
  IDS_CAP_AUDIO_DROP_COMPERROR        = 442;  { "Error: Audio data was lost during capture.  Try capturing without compressing." }
  {$EXTERNALSYM IDS_CAP_AUDIO_DROP_COMPERROR}

  // status string IDs

const

  IDS_CAP_STAT_LIVE_MODE              = 500;  { "Live window" }
  {$EXTERNALSYM IDS_CAP_STAT_LIVE_MODE}
  IDS_CAP_STAT_OVERLAY_MODE           = 501;  { "Overlay window" }
  {$EXTERNALSYM IDS_CAP_STAT_OVERLAY_MODE}
  IDS_CAP_STAT_CAP_INIT               = 502;  { "Setting up for capture - Please wait" }
  {$EXTERNALSYM IDS_CAP_STAT_CAP_INIT}
  IDS_CAP_STAT_CAP_FINI               = 503;  { "Finished capture, now writing frame %ld" }
  {$EXTERNALSYM IDS_CAP_STAT_CAP_FINI}
  IDS_CAP_STAT_PALETTE_BUILD          = 504;  { "Building palette map" }
  {$EXTERNALSYM IDS_CAP_STAT_PALETTE_BUILD}
  IDS_CAP_STAT_OPTPAL_BUILD           = 505;  { "Computing optimal palette" }
  {$EXTERNALSYM IDS_CAP_STAT_OPTPAL_BUILD}
  IDS_CAP_STAT_I_FRAMES               = 506;  { "%d frames" }
  {$EXTERNALSYM IDS_CAP_STAT_I_FRAMES}
  IDS_CAP_STAT_L_FRAMES               = 507;  { "%ld frames" }
  {$EXTERNALSYM IDS_CAP_STAT_L_FRAMES}
  IDS_CAP_STAT_CAP_L_FRAMES           = 508;  { "Captured %ld frames" }
  {$EXTERNALSYM IDS_CAP_STAT_CAP_L_FRAMES}
  IDS_CAP_STAT_CAP_AUDIO              = 509;  { "Capturing audio" }
  {$EXTERNALSYM IDS_CAP_STAT_CAP_AUDIO}
  IDS_CAP_STAT_VIDEOCURRENT           = 510;  { "Captured %ld frames (%ld dropped) %d.%03d sec." }
  {$EXTERNALSYM IDS_CAP_STAT_VIDEOCURRENT}
  IDS_CAP_STAT_VIDEOAUDIO             = 511;  { "Captured %d.%03d sec.  %ld frames (%ld dropped) (%d.%03d fps).  %ld audio bytes (%d,%03d sps)" }
  {$EXTERNALSYM IDS_CAP_STAT_VIDEOAUDIO}
  IDS_CAP_STAT_VIDEOONLY              = 512;  { "Captured %d.%03d sec.  %ld frames (%ld dropped) (%d.%03d fps)" }
  {$EXTERNALSYM IDS_CAP_STAT_VIDEOONLY}
  IDS_CAP_STAT_FRAMESDROPPED          = 513;  { "Dropped %ld of %ld frames (%d.%02d%%) during capture." }
  {$EXTERNALSYM IDS_CAP_STAT_FRAMESDROPPED}

  //****************************************************************************
  //
  //  ACM (Audio compression manager)
  //
  //****************************************************************************

  // see: include & uses section

  //****************************************************************************
  //
  //  FilePreview dialog.
  //
  //****************************************************************************

{$IFDEF UNICODE}
  function GetOpenFileNamePreviewW({_Inout_} lpofn: POPENFILENAMEW): BOOL; stdcall;
  {$EXTERNALSYM GetOpenFileNamePreviewW}
  function GetSaveFileNamePreviewW({_Inout_} lpofn: POPENFILENAMEW): BOOL; stdcall;
  {$EXTERNALSYM GetSaveFileNamePreviewW}
{$ELSE}
  function GetOpenFileNamePreviewA({_Inout_} lpofn: POPENFILENAMEA): BOOL; stdcall;
  {$EXTERNALSYM GetOpenFileNamePreviewA}
  function GetSaveFileNamePreviewA({_Inout_} lpofn: POPENFILENAMEA): BOOL; stdcall;
  {$EXTERNALSYM GetSaveFileNamePreviewA}
{$ENDIF}

  { unicode }
  function GetOpenFileNamePreview({_Inout_} lpofn: POPENFILENAMEW): BOOL; stdcall;
  {$EXTERNALSYM GetOpenFileNamePreview}
  function GetSaveFileNamePreview({_Inout_} lpofn: POPENFILENAMEW): BOOL; stdcall;
  {$EXTERNALSYM GetSaveFileNamePreview}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  VfWLib      = 'msvfw32.dll';
  AvifilLib   = 'avifil32.dll';
  AviCapLib   = 'avicap32.dll';


// Query macros ================================================================

function ICQueryAbout(hic: HIC): BOOL;
begin
  Result := ICSendMessage(hic,
                          ICM_ABOUT,
                          DWORD_PTR(-1),
                          ICMF_ABOUT_QUERY) = ICERR_OK;
end;

function ICAbout(hic: HIC;
                 _hWnd: HWND): LRESULT;
begin
  Result := ICSendMessage(hic,
                         ICM_ABOUT,
                         _hWnd,
                         0);
end;

function ICQueryConfigure(hic: HIC): BOOL;
begin
  Result := ICSendMessage(hic,
                          ICM_CONFIGURE,
                          DWORD_PTR(-1),
                          ICMF_CONFIGURE_QUERY) = ICERR_OK;
end;

function ICConfigure(hic: HIC;
                     _hWnd: HWND): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_CONFIGURE,
                          DWORD_PTR(_hWnd),
                          0);
end;


// get/set state macros ========================================================

function ICGetState(hic: HIC;
                    pv: LPVOID;
                    cb: DWORD_PTR): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_GETSTATE,
                          DWORD_PTR(@pv),
                          cb);
end;

function ICSetState(hic: HIC;
                    pv: LPVOID;
                    cb: DWORD_PTR): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_SETSTATE,
                          DWORD_PTR(@pv),
                          DWORD_PTR(cb));
end;

function ICGetStateSize(hic: HIC): DWORD;
begin
  Result := DWORD(ICGetState(hic,
                             nil,
                             0));
end;


// get value macros ============================================================

function ICGetDefaultQuality(hic: HIC): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_GETDEFAULTQUALITY,
                          DWORD_PTR(@Result),
                          SizeOf(Result));
end;

function ICGetDefaultKeyFrameRate(hic: HIC): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_GETDEFAULTKEYFRAMERATE,
                          DWORD_PTR(@Result),
                          SizeOf(Result));
end;


// draw window macro ===========================================================

function ICDrawWindow(hic: HIC;
                      prc: PRect): LRESULT;
begin
  Result :=  ICSendMessage(hic,
                           ICM_DRAW_WINDOW,
                           DWORD_PTR(@prc),
                           SizeOf(TRect));
end;


// ICCompress macro ============================================================

function ICCompressBegin(hic: HIC;
                         lpbiInput: PBITMAPINFOHEADER;
                         lpbiOutput: PBITMAPINFOHEADER): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_COMPRESS_BEGIN,
                          DWORD_PTR(@lpbiInput),
                          DWORD_PTR(@lpbiOutput));
end;

// Compress query macro ========================================================

function ICCompressQuery(hic: HIC;
                           lpbiInput: PBITMAPINFOHEADER;
                           lpbiOutput: PBITMAPINFOHEADER): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_COMPRESS_QUERY,
                          DWORD_PTR(@lpbiInput),
                          DWORD_PTR(@lpbiOutput));
end;

// Get formats macro ===========================================================

function ICCompressGetFormat(hic: HIC;
                             lpbiInput: PBITMAPINFOHEADER;
                             lpbiOutput: PBITMAPINFOHEADER): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_COMPRESS_GET_FORMAT,
                          DWORD_PTR(@lpbiInput),
                          DWORD_PTR(@lpbiOutput));
end;

function ICCompressGetFormatSize(hic: HIC;
                                 lpbi: PBITMAPINFOHEADER): DWord;
begin
  Result := DWord(ICCompressGetFormat(hic,
                                      lpbi,
                                      nil));
end;


// compress size macro ===========================================================

function ICCompressGetSize(hic: HIC;
                             lpbiInput: PBITMAPINFOHEADER;
                             lpbiOutput: PBITMAPINFOHEADER): DWord;
begin
  Result := DWORD(ICSendMessage(hic,
                                ICM_COMPRESS_GET_SIZE,
                                DWORD_PTR(@lpbiInput),
                                DWORD_PTR(@lpbiOutput)));
end;

function ICCompressEnd(hic: HIC): LRESULT;
begin
  Result := ICSendMessage(hic,
                          ICM_COMPRESS_END,
                          0,
                          0);
end;


// Decompress macro's =========================================================

function ICDecompressBegin(hic: HIC;
                           lpbiInput: PBITMAPINFOHEADER;
                           lpbiOutput: PBITMAPINFOHEADER): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESS_BEGIN,
                          DWORD_PTR(@lpbiInput),
                          DWORD_PTR(@lpbiOutput));
end;

function ICDecompressQuery(hic: HIC;
                           lpbiInput: PBITMAPINFOHEADER;
                           lpbiOutput: PBITMAPINFOHEADER): LResult;
begin
  Result :=  ICSendMessage(hic,
                           ICM_DECOMPRESS_QUERY,
                           DWORD_PTR(@lpbiInput),
                           DWORD_PTR(@lpbiOutput));
end;

function ICDecompressGetFormat(hic: HIC;
                               lpbiInput: PBITMAPINFOHEADER;
                               lpbiOutput: PBITMAPINFOHEADER): LONG;
begin
  Result := LONG(ICSendMessage(hic,
                              ICM_DECOMPRESS_GET_FORMAT,
                              DWORD_PTR(@lpbiInput),
                              DWORD_PTR(@lpbiOutput)));
end;

function ICDecompressGetFormatSize(hic: HIC;
                                   lpbi: PBITMAPINFOHEADER): LResult;
begin
  Result := ICDecompressGetFormat(hic,
                                  @lpbi,
                                  nil);
end;


// output palette macro's ======================================================

function ICDecompressGetPalette(hic: HIC;
                                lpbiInput: PBITMAPINFOHEADER;
                                lpbiOutput: PBITMAPINFOHEADER): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESS_GET_PALETTE,
                          DWORD_PTR(@lpbiInput),
                          DWORD_PTR(@lpbiOutput));
end;

function ICDecompressSetPalette(hic: HIC;
                                lpbiPalette: PBITMAPINFOHEADER): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESS_SET_PALETTE,
                          DWORD_PTR(@lpbiPalette),
                          0);
end;

function ICDecompressEnd(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESS_END,
                          0,
                          0);
end;


// Decompression functions =====================================================

function ICDecompressEx(hic: HIC;
                        dwFlags: DWORD;
                        lpbiSrc: PBITMAPINFOHEADER;
                        lpSrc: Pointer;
                        xSrc: Integer;
                        ySrc: Integer;
                        dxSrc: Integer;
                        dySrc: Integer;
                        lpbiDst: PBITMAPINFOHEADER;
                        lpDst: Pointer;
                        xDst: Integer;
                        yDst: Integer;
                        dxDst: Integer;
                        dyDst: Integer): LResult; inline;
var
  ic: _ICDECOMPRESSEX;

begin



  ic.dwFlags := dwFlags;
  ic.lpbiSrc := lpbiSrc;
  ic.lpSrc := lpSrc;
  ic.xSrc := xSrc;
  ic.ySrc := ySrc;
  ic.dxSrc := dxSrc;
  ic.dySrc := dySrc;
  ic.lpbiDst := lpbiDst;
  ic.lpDst := lpDst;
  ic.xDst := xDst;
  ic.yDst := yDst;
  ic.dxDst := dxDst;

  // note that ICM swaps round the length and pointer
  // length in lparam2, pointer in lparam1
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESSEX,
                          DWORD_PTR(@ic),
                          SizeOf(ic));
end;

function ICDecompressExBegin(hic: HIC;
                             dwFlags: DWORD;
                             lpbiSrc: PBITMAPINFOHEADER;
                             {_In_opt_} lpSrc: Pointer;
                             xSrc: Integer;
                             ySrc: Integer;
                             dxSrc: Integer;
                             dySrc: Integer;
                             lpbiDst: PBITMAPINFOHEADER;
                             {_Out_opt_} lpDst: Pointer;
                             xDst: Integer;
                             yDst: Integer;
                             dxDst: Integer;
                             dyDst: Integer): LResult; inline;
var
  ic: _ICDECOMPRESSEX;

begin

 ic.dwFlags := dwFlags;
 ic.lpbiSrc := lpbiSrc;
 ic.lpSrc := lpSrc;
 ic.xSrc := xSrc;
 ic.ySrc := ySrc;
 ic.dxSrc := dxSrc;
 ic.dySrc := dySrc;
 ic.lpbiDst := lpbiDst;
 ic.lpDst := lpDst;
 ic.xDst := xDst;
 ic.yDst := yDst;
 ic.dxDst := dxDst;
 ic.dyDst := dyDst;

 // note that ICM swaps round the length and pointer
 // length in lparam2, pointer in lparam1
 Result := ICSendMessage(hic,
                         ICM_DECOMPRESSEX_BEGIN,
                         DWORD_PTR(@ic),
                         SizeOf(ic));
end;

function ICDecompressExQuery(hic: HIC;
                             dwFlags: DWORD;
                             lpbiSrc: PBITMAPINFOHEADER;
                             {_Reserved_} lpSrc: Pointer;
                             xSrc: Integer;
                             ySrc: Integer;
                             dxSrc: Integer;
                             dySrc: Integer;
                             {_In_opt_} lpbiDst: PBITMAPINFOHEADER;
                             {_Out_opt_} lpDst: Pointer;
                             xDst: Integer;
                             yDst: Integer;
                             dxDst: Integer;
                             dyDst: Integer): LResult; inline;
var
  ic: _ICDECOMPRESSEX;

begin
  ic.dwFlags := dwFlags;
  ic.lpbiSrc := lpbiSrc;
  ic.lpSrc := lpSrc;
  ic.xSrc := xSrc;
  ic.ySrc := ySrc;
  ic.dxSrc := dxSrc;
  ic.dySrc := dySrc;
  ic.lpbiDst := lpbiDst;
  ic.lpDst := lpDst;
  ic.xDst := xDst;
  ic.yDst := yDst;
  ic.dxDst := dxDst;
  ic.dyDst := dyDst;

  // note that ICM swaps round the length and pointer
  // length in lparam2, pointer in lparam1
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESSEX_QUERY,
                          DWORD_PTR(@ic),
                          SizeOf(ic));
end;


function ICDecompressExEnd(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DECOMPRESSEX_END,
                          0,
                          0);
end;

function ICDrawSuggestFormat(hic: HIC;
                            lpbiIn: PBITMAPINFOHEADER;
                            {_Out_} lpbiOut: PBITMAPINFOHEADER;
                            dxSrc: Integer;
                            dySrc: Integer;
                            dxDst: Integer;
                            dyDst: Integer;
                            hicDecomp: HIC): LResult; inline;
var
  ic: ICDRAWSUGGEST;

begin
  ic.lpbiIn := lpbiIn;
  ic.lpbiSuggest := lpbiOut;
  ic.dxSrc := dxSrc;
  ic.dySrc := dySrc;
  ic.dxDst := dxDst;
  ic.dyDst := dyDst;
  ic.hicDecompressor := hicDecomp;

  // note that ICM swaps round the length and pointer
  // length in lparam2, pointer in lparam1
  Result := ICSendMessage(hic,
                          ICM_DRAW_SUGGESTFORMAT,
                          DWORD_PTR(@ic),
                          SizeOf(ic));
end;

// Draw macro's ================================================================

function ICDrawQuery(hic: HIC; lpbiInput: PBITMAPINFOHEADER): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_QUERY,
                          DWORD_PTR(@lpbiInput),
                          0);
end;

function ICDrawChangePalette(hic: HIC; lpbiInput: PBITMAPINFOHEADER): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_CHANGEPALETTE,
                          DWORD_PTR(@lpbiInput),
                          0);
end;

function ICGetBuffersWanted(hic: HIC;
                            lpdwBuffers: PDWord): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_GETBUFFERSWANTED,
                          DWORD_PTR(@lpdwBuffers),
                          0);
end;

function ICDrawEnd(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_END,
                          0,
                          0);
end;

function ICDrawStart(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_START,
                          0,
                          0);
end;

function ICDrawStartPlay(hic: HIC;
                         lFrom: DWord;
                         lTo: DWord): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_START_PLAY,
                          DWORD_PTR(lFrom),
                          DWORD_PTR(lTo));
end;

function ICDrawStop(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_STOP,
                          0,
                          0);
end;

function ICDrawStopPlay(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_STOP_PLAY,
                          0,
                          0);
end;

function ICDrawGetTime(hic: HIC;
                       lplTime: PDWORD_PTR): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_GETTIME,
                          DWORD_PTR(@lplTime),
                          0);
end;

function ICDrawSetTime(hic: HIC;
                       lTime: DWORD_PTR): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_SETTIME,
                          lTime,
                          0);
end;

function ICDrawRealize(hic: HIC;
                       dc: HDC; fBackground: BOOL): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_REALIZE,
                          DWORD_PTR(dc),
                          DWORD_PTR(fBackground));
end;

function ICDrawFlush(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_FLUSH,
                          0,
                          0);
end;

function ICDrawRenderBuffer(hic: HIC): LResult;
begin
  Result := ICSendMessage(hic,
                          ICM_DRAW_RENDERBUFFER,
                          0,
                          0);
end;


function ICSetStatusProc(hic: HIC;
                         dwFlags: DWORD;
                         lParam: DWORD;
                         fpfnStatus: FICStatusProc): LRESULT;
var
  ic : _ICSETSTATUSPROC;

begin
  ic.dwFlags := dwFlags;
  ic.lParam := lParam;
  ic.Status := fpfnStatus;
  Result := ICSendMessage(hic,
                          ICM_SET_STATUS_PROC,
                          DWORD(@ic),
                          SizeOf(ic));
end;


// helper routines for DrawDib and MCIAVI macro's  =============================

function ICDecompressOpen(fccType: FOURCC;
                          fccHandler: FOURCC;
                          lpbiIn: PBITMAPINFOHEADER;
                          lpbiOut: PBITMAPINFOHEADER): HIC;
begin
  Result := ICLocate(fccType,
                     fccHandler,
                     lpbiIn,
                     lpbiOut,
                     ICMODE_DECOMPRESS);
end;

function ICDrawOpen(fccType: FOURCC;
                    fccHandler: FOURCC;
                    lpbiIn: PBITMAPINFOHEADER): HIC;
begin
  Result := ICLocate(fccType,
                     fccHandler,
                     lpbiIn,
                     nil,
                     ICMODE_DRAW);
end;

//  Drawdib  ===================================================================

function DrawDibUpdate(hdd: HDRAWDIB;
                       dc: HDC;
                       x: Integer;
                       y: Integer): BOOL;
begin
  Result := DrawDibDraw(hdd,
                        dc,
                        x,
                        y,
                        0,
                        0,
                        nil,
                        nil,
                        0,
                        0,
                        0,
                        0,
                        DDF_UPDATE);
end;


//  TWO and FOURCC  ============================================================

function MAKEFOURCC(const ch0: AnsiChar;
                    const ch1: AnsiChar;
                    const ch2: AnsiChar;
                    const ch3: AnsiChar): FOURCC;
begin
  Result := DWORD(Ord(ch0)) or
           (DWORD(Ord(ch1)) shl 8) or
           (DWORD(Ord(ch2)) shl 16) or
           (DWORD(Ord(ch3)) shl 24);
end;

function MKFOURCC(ch0: AnsiChar;
                  ch1: AnsiChar;
                  ch2: AnsiChar;
                  ch3: AnsiChar): FOURCC;
begin
  Result := MAKEFOURCC(ch0,
                       ch1,
                       ch2,
                       ch3);
end;


function mmioFOURCC(ch1: AnsiChar;
                    ch2: AnsiChar;
                    ch3: AnsiChar;
                    ch4: AnsiChar): FOURCC;
begin
  Result :=  MAKEFOURCC(ch1,
                        ch2,
                        ch3,
                        ch4);
end;


function aviTWOCC(ch0: AnsiChar;
                  ch1: AnsiChar): TWOCC;
begin
  Result := (Word(ord(ch0))) or (Word(ord(ch1)) shl 8);
end;


function FromHex(n: Byte): Byte;
begin
  if n >= ord('A') then
    Result := ord(n) + 10 - ord('A')
  else
    Result := ord(n) - ord('0');
end;

function StreamFromFOURCC(fcc: FOURCC): Byte;
begin
  Result := (FromHex(Lo(LoWord(fcc))) shl 4) + FromHex(Hi(LoWord(fcc)));
end;

function TWOCCFromFOURCC(fcc: FOURCC): Word;
begin
  Result := HIWORD(fcc);
end;

function ToHex(n: Byte): Byte;
begin
  if n > 9 then
    Result := n - 10 + ord('A')
  else
    Result := n + ord('0');
end;

function MAKEAVICKID(tcc: TWOCC;
                     stream: Byte): DWord;
begin
  Result := MakeLONG((ToHex(stream and $0F) shl 8) or ToHex((stream and $F0) shr 4), tcc);
end;


//  helper macros ==============================================================

function AVIStreamSampleToSample(pavi1: IAVIStream;
                                 pavi2: IAVIStream;
                                 l: LONG): LONG; inline;
begin
  Result := AVIStreamTimeToSample(pavi1,
                                  AVIStreamSampleToTime(pavi2, l));
end;

function AVIStreamNextSample(pavi: IAVIStream;
                             l: LONG): LONG; inline;
begin
  Result := AVIStreamFindSample(pavi,
                                l +1,
                                FIND_NEXT or FIND_ANY);
end;

function AVIStreamPrevSample(pavi: IAVIStream;
                             l: LONG): LONG; inline;
begin
  Result := AVIStreamFindSample(pavi,
                                l -1,
                                FIND_PREV or FIND_ANY);
end;

function AVIStreamNearestSample(pavi: IAVIStream;
                                l: LONG):
                                LONG; inline;
begin
  Result := AVIStreamFindSample(pavi,
                                l,
                                FIND_PREV or FIND_ANY);
end;

function AVIStreamNextKeyFrame(pavi: IAVIStream;
                               l: LONG): LONG; inline;
begin
  Result := AVIStreamFindSample(pavi,
                                l +1,
                                FIND_NEXT or FIND_KEY);
end;

function AVIStreamPrevKeyFrame(pavi: IAVIStream;
                               l: LONG): LONG; inline;
begin
  Result := AVIStreamFindSample(pavi,
                                l -1,
                                FIND_PREV or FIND_KEY);
end;

function AVIStreamNearestKeyFrame(pavi: IAVIStream;
                                  l: LONG): LONG; inline;
begin
  Result := AVIStreamFindSample(pavi,
                                l,
                                FIND_PREV or FIND_KEY);
end;

function AVIStreamIsKeyFrame(pavi: IAVIStream;
                             l: LONG): BOOL; inline;
begin
  Result := (AVIStreamNearestKeyFrame(pavi,
                                      l) = l);
end;

function AVIStreamPrevSampleTime(pavi: IAVIStream;
                                 t: LONG): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamPrevSample(pavi,
                                                      AVIStreamTimeToSample(pavi,
                                                                            t)));
end;

function AVIStreamNextSampleTime(pavi: IAVIStream;
                                 t: LONG): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamNextSample(pavi,
                                                      AVIStreamTimeToSample(pavi,
                                                                            t)));
end;

function AVIStreamNearestSampleTime(pavi: IAVIStream;
                                    t: LONG): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamNearestSample(pavi,
                                                         AVIStreamTimeToSample(pavi,
                                                                               t)));
end;

function AVIStreamNextKeyFrameTime(pavi: IAVIStream;
                                   t: LONG): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamNextKeyFrame(pavi,
                                                        AVIStreamTimeToSample(pavi,
                                                                              t)));
end;

function AVIStreamPrevKeyFrameTime(pavi: IAVIStream;
                                   t: LONG): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamPrevKeyFrame(pavi,
                                                        AVIStreamTimeToSample(pavi,
                                                        t)));
end;

function AVIStreamNearestKeyFrameTime(pavi: IAVIStream;
                                      t: LONG): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamNearestKeyFrame(pavi,
                                                           AVIStreamTimeToSample(pavi,
                                                                                 t)));
end;

function AVIStreamStartTime(pavi: IAVIStream): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamStart(pavi));
end;

function AVIStreamLengthTime(pavi: IAVIStream): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamLength(pavi));
end;

function AVIStreamEnd(pavi: IAVIStream): LONG; inline;
begin
  Result := (AVIStreamStart(pavi) + AVIStreamLength(pavi));
end;

function AVIStreamEndTime(pavi: IAVIStream): LONG; inline;
begin
  Result := AVIStreamSampleToTime(pavi,
                                  AVIStreamEnd(pavi));
end;

function AVIStreamSampleSize(pavi: IAVIStream;
                             lPos: LONG;
                             plSize: PLONG): LONG; inline;
begin
  Result := AVIStreamRead(pavi,
                          lPos,
                          1,
                          nil,
                          0,
                          plSize,
                          nil);
end;

function AVIStreamFormatSize(pavi: IAVIStream;
                             lPos: LONG;
                             plSize: PLONG): LONG; inline;
begin
  Result := AVIStreamReadFormat(pavi,
                                lPos,
                                nil,
                                plSize);
end;

function AVIStreamDataSize(pavi: IAVIStream;
                           fcc: LONG;
                           plSize: PLong): LONG; inline;
begin
  Result := AVIStreamReadData(pavi,
                              fcc,
                              nil,
                              plSize);
end;



//  Message crackers ===========================================================

function capSetCallbackOnError(_hWnd: HWND;
                               fpProc: CAPERRORCALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_ERROR,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetCallbackOnStatus(_hWnd: HWND;
                                fpProc: CAPSTATUSCALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_STATUS,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetCallbackOnYield(_hWnd: HWND;
                               fpProc: CAPYIELDCALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_YIELD,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetCallbackOnFrame(_hWnd: HWND;
                               fpProc: CAPVIDEOCALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_FRAME,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetCallbackOnVideoStream(_hWnd: HWND;
                                     fpProc: CAPVIDEOCALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_VIDEOSTREAM,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetCallbackOnWaveStream(_hWnd: HWND;
                                    fpProc: CAPWAVECALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_WAVESTREAM,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetCallbackOnCapControl(_hWnd: HWND;
                                    fpProc: CAPCONTROLCALLBACK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_CALLBACK_CAPCONTROL,
                     0,
                     LPARAM(@fpProc)) <> 0;
end;

function capSetUserData(_hWnd: DWORD;
                        lUser: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_USER_DATA,
                     0,
                     LPARAM(lUser)) <> 0;
end;

function capGetUserData(_hwnd: HWND): DWORD; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_USER_DATA,
                     0,
                     0);
end;

function capDriverConnect(_hWnd: HWND;
                          i: Integer): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DRIVER_CONNECT,
                     WPARAM(i),
                     0) <> 0;
end;

function capDriverDisconnect(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DRIVER_DISCONNECT,
                     WPARAM(0),
                     0) <> 0;
end;

function capDriverGetName(_hWnd: HWND;
                          szName: LPCSTR;
                          wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DRIVER_GET_NAME,
                     WPARAM(wSize),
                     LPARAM(szName)) <> 0;
end;

function capDriverGetVersion(_hWnd: HWND;
                             szVer: LPSTR;
                             wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DRIVER_GET_VERSION,
                     WPARAM(wSize),
                     LPARAM(szVer)) <> 0;
end;

function capDriverGetCaps(_hWnd: HWND;
                          s: PCAPDRIVERCAPS;
                          wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DRIVER_GET_CAPS,
                     WPARAM(wSize),
                     LPARAM(s)) <> 0;
end;


function capFileSetCaptureFile(_hWnd: HWND;
                               szName: LPCSTR): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_FILE_SET_CAPTURE_FILE,
                     0,
                     LPARAM(szName)) <> 0;
end;

function capFileGetCaptureFile(_hWnd: HWND;
                               szName: LPCSTR;
                               wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_FILE_GET_CAPTURE_FILE,
                     WPARAM(wSize),
                     LPARAM(szName)) <> 0;
end;

function capFileAlloc(_hWnd: HWND;
                      dwSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_FILE_ALLOCATE,
                     0,
                     LPARAM(dwSize)) <> 0;
end;

function capFileSaveAs(_hWnd: HWND;
                       szName: LPCSTR): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_FILE_SAVEAS,
                     0,
                     LPARAM(szName)) <> 0;
end;

function capFileSetInfoChunk(_hWnd: HWND;
                             lpInfoChunk: PCAPINFOCHUNK): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_FILE_SET_INFOCHUNK,
                     WPARAM(0),
                     LPARAM(lpInfoChunk)) <> 0;
end;

function capFileSaveDIB(_hWnd: HWND;
                        szName: LPCSTR): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_FILE_SAVEDIB,
                     0,
                     LPARAM(szName)) <> 0;
end;

function capEditCopy(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_EDIT_COPY,
                     0,
                     0) <> 0;
end;

function capSetAudioFormat(_hWnd: HWND;
                           s: LPWAVEFORMATEX;
                           wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_AUDIOFORMAT,
                     WPARAM(wSize),
                     LPARAM(s)) <> 0;
end;

function capGetAudioFormat(_hWnd: HWND;
                           s: LPWAVEFORMATEX;
                           wSize: DWORD): DWORD; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_AUDIOFORMAT,
                     WPARAM(wSize),
                     LPARAM(s));
end;

function capGetAudioFormatSize(_hwnd: HWND): DWORD; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_AUDIOFORMAT,
                     WPARAM(0),
                     LPARAM(0));
end;

function capDlgVideoFormat(_hWnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DLG_VIDEOFORMAT,
                     0,
                     0) <> 0;
end;

function capDlgVideoSource(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DLG_VIDEOSOURCE,
                     0,
                     0) <> 0;
end;

function capDlgVideoDisplay(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DLG_VIDEODISPLAY,
                     0,
                     0) <> 0;
end;

function capDlgVideoCompression(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_DLG_VIDEOCOMPRESSION,
                     0,
                     0) <> 0;
end;

function capGetVideoFormat(_hWnd: DWORD;
                           s: Pointer;
                           wSize: DWORD): DWORD; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_VIDEOFORMAT,
                     WPARAM(wSize),
                     LPARAM(s));
end;

function capGetVideoFormatSize(_hwnd: HWND): DWORD; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_VIDEOFORMAT,
                     0,
                     0);
end;

function capSetVideoFormat(_hWnd: HWND;
                           s: Pointer;
                           wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_VIDEOFORMAT,
                     WPARAM(wSize),
                     LPARAM(s)) <> 0;
end;

function capPreview(_hWnd: HWND;
                    f: BOOL): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_PREVIEW,
                     WPARAM(f),
                     0) <> 0;
end;

function capPreviewRate(_hWnd: HWND;
                        wMS: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_PREVIEWRATE,
                     WPARAM(wMS),
                     0) <> 0;
end;

function capOverlay(_hWnd: HWND;
                    f: BOOL): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_OVERLAY,
                     WPARAM(f),
                     0) <> 0;
end;

function capPreviewScale(_hWnd: HWND;
                         f: BOOL): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_SCALE,
                     WPARAM(f),
                     0) <> 0;
end;

function capGetStatus(_hWnd: HWND;
                      s: LPCAPSTATUS;
                      wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_STATUS,
                     WPARAM(wSize),
                     LPARAM(s)) <> 0;
end;

function capSetScrollPos(_hWnd: HWND;
                         lpP: PPoint): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_SCROLL,
                     WPARAM(0),
                     LPARAM(lpP)) <> 0;
end;

function capGrabFrame(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GRAB_FRAME,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capGrabFrameNoStop(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GRAB_FRAME_NOSTOP,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureSequence(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SEQUENCE,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureSequenceNoFile(_hwnd: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SEQUENCE_NOFILE,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureStop(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_STOP,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureAbort(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_ABORT,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureSingleFrameOpen(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SINGLE_FRAME_OPEN,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureSingleFrameClose(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SINGLE_FRAME_CLOSE,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureSingleFrame(_hwnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SINGLE_FRAME,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capCaptureGetSetup(_hWnd: HWND;
                            s: LPCAPTUREPARMS;
                            wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_SEQUENCE_SETUP,
                     WPARAM(wSize),
                     LPARAM(s)) <> 0;
end;

function capCaptureSetSetup(_hWnd: HWND;
                            s: LPCAPTUREPARMS;
                            wSize: DWORD): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_SEQUENCE_SETUP,
                     WPARAM(wSize),
                     LPARAM(s)) <> 0;
end;

function capSetMCIDeviceName(_hWnd: HWND;
                             szName: LPCSTR): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_SET_MCI_DEVICE,
                     0,
                     LPARAM(szName)) <> 0;
end;

function capGetMCIDeviceName(_hWnd: HWND;
                             szName: LPCSTR;
                             wSize: DWORD): BOOL;  inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_GET_MCI_DEVICE,
                     WPARAM(wSize),
                     LPARAM(szName)) <> 0;
end;

function capPaletteOpen(_hWnd: HWND;
                        szName: LPCSTR): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_PAL_OPEN,
                     0,
                     LPARAM(szName)) <> 0;
end;

function capPaletteSave(_hWnd: HWND;
                        szName: LPCSTR): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_PAL_SAVE,
                     0,
                     LPARAM(szName)) <> 0;
end;

function capPalettePaste(_hWnd: HWND): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_PAL_PASTE,
                     WPARAM(0),
                     LPARAM(0)) <> 0;
end;

function capPaletteAuto(_hWnd: HWND;
                        iFrames: DWORD;
                        iColors: Integer): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_PAL_AUTOCREATE,
                     WPARAM(iFrames),
                     LPARAM(iColors)) <> 0;
end;

function capPaletteManual(_hWnd: HWND;
                          fGrab: DWORD;
                          iColors: Integer): BOOL; inline;
begin
  Result := AVICapSM(_hWnd,
                     WM_CAP_PAL_MANUALCREATE,
                     WPARAM(fGrab),
                     LPARAM(iColors)) <> 0;
end;



// MCI =========================================================================

function MCIWndNew(_hWnd: HWND; lp: Pointer): DWORD; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_NEW,
                     0,
                     LPARAM(lp));
end;

function MCIWndRecord(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_RECORD,
                     0,
                     0);
end;

function MCIWndOpen(_hWnd: HWND; sz: LPCSTR; f: BOOL): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_OPEN,
                     WPARAM(f),
                     LPARAM(sz));
end;

function MCIWndOpenDialog(_hWnd: HWND): DWORD; inline;
begin
  Result := MCIWndOpen(_hWnd,
                       LPCSTR(-1),
                       BOOL(0));
end;

function MCIWndClose(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_CLOSE,
                     0,
                     0);
end;

function MCIWndPlay(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_PLAY,
                     0,
                     0);
end;

function MCIWndStop(_hWnd: DWORD): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_STOP,
                     0,
                     0);
end;

function MCIWndPause(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_PAUSE,
                     0,
                     0);
end;

function MCIWndResume(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_RESUME,
                     0,
                     0);
end;

function MCIWndSeek(_hWnd: HWND; lPos: LONG): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_SEEK,
                     0,
                     LPARAM(lPos));
end;


function MCIWndEject(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_EJECT,
                     0,
                     0);
end;

function MCIWndHome(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSeek(_hWnd,
                       MCIWND_START);
end;

function MCIWndEnd(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSeek(_hWnd,
                       MCIWND_END);
end;

function MCIWndGetSource(_hWnd: HWND; prc: PRect): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GET_SOURCE,
                     0,
                     LPARAM(prc));
end;

function MCIWndPutSource(_hWnd: HWND; prc: PRect): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_PUT_SOURCE,
                     0,
                     LPARAM(prc));
end;

function MCIWndGetDest(_hWnd: HWND; prc: PRect): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GET_DEST,
                     0,
                     LPARAM(prc));
end;

function MCIWndPutDest(_hWnd: HWND; prc: PRect): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_PUT_DEST,
                     0,
                     LPARAM(prc));
end;

function MCIWndPlayReverse(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_PLAYREVERSE,
                     0,
                     0);
end;

function MCIWndPlayFrom(_hWnd: HWND; lPos: LONG): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_PLAYFROM,
                     0,
                     LPARAM(lPos));
end;

function MCIWndPlayTo(_hWnd: HWND;
                      lPos: LONG): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_PLAYTO,
                     0,
                     LPARAM(lPos));
end;

function MCIWndPlayFromTo(_hWnd: HWND;
                          lStart: DWORD;
                          lEnd: DWORD): LONG; inline;
begin
  MCIWndSeek(_hWnd,
             lStart);

  Result := MCIWndPlayTo(_hWnd,
                         lEnd);
end;

function MCIWndGetDeviceID(_hWnd: HWND): UINT; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETDEVICEID,
                     0,
                     0);
end;

function MCIWndGetAlias(_hWnd: HWND): UINT; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETALIAS,
                     0,
                     0);
end;

function MCIWndGetMode(_hWnd: HWND; lp: LPCSTR; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETMODE,
                     WPARAM(len),
                     LPARAM(lp));
end;

function MCIWndGetPosition(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETPOSITION,
                     0,
                     0);
end;

function MCIWndGetPositionString(_hWnd: HWND; lp: LPCSTR; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETPOSITION,
                     WPARAM(len),
                     LPARAM(lp));
end;

function MCIWndGetStart(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETSTART,
                     0,
                     0);
end;

function MCIWndGetLength(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETLENGTH,
                     0,
                     0);
end;

function MCIWndGetEnd(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETEND,
                     0,
                     0);
end;

function MCIWndStep(_hWnd: HWND; n: LONG): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_STEP,
                     0,
                     LPARAM(n));
end;


procedure MCIWndDestroy(_hWnd: HWND); inline;
begin
  MCIWndSM(_hWnd,
           WM_CLOSE,
           0,
           0);
end;

procedure MCIWndSetZoom(_hWnd: HWND; iZoom: UINT); inline;
begin
  MCIWndSM(_hWnd,
           MCIWNDM_SETZOOM,
           0,
           LPARAM(iZoom));
end;

function MCIWndGetZoom(_hWnd: HWND): UINT; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETZOOM,
                     0,
                     0);
end;

function MCIWndSetVolume(_hWnd: HWND; iVol: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_SETVOLUME,
                     0,
                     LPARAM(iVol));
end;

function MCIWndGetVolume(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETVOLUME,
                     0,
                     0);
end;

function MCIWndSetSpeed(_hWnd: HWND; iSpeed: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_SETSPEED,
                     0,
                     LPARAM(iSpeed));
end;

function MCIWndGetSpeed(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETSPEED,
                     0,
                     0);
end;

function MCIWndSetTimeFormat(_hWnd: HWND; lp: LPCSTR): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_SETTIMEFORMAT,
                     0,
                     LPARAM(lp));
end;

function MCIWndGetTimeFormat(_hWnd: HWND; lp: LPCSTR; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETTIMEFORMAT,
                     WPARAM(len),
                     LPARAM(lp));
end;

procedure MCIWndValidateMedia(_hWnd: HWND); inline;
begin
  MCIWndSM(_hWnd,
           MCIWNDM_VALIDATEMEDIA,
           0,
           0);
end;


procedure MCIWndSetRepeat(_hWnd: HWND; f: BOOL); inline;
begin
  MCIWndSM(_hWnd,
           MCIWNDM_SETREPEAT,
           0,
           LPARAM(f));
end;

function MCIWndGetRepeat(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETREPEAT,
                     0,
                     0) <> 0;
end;

function MCIWndUseFrames(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSetTimeFormat(_hWnd,
                                'frames');
end;

function MCIWndUseTime(_hWnd: HWND): LONG; inline;
begin
  Result := MCIWndSetTimeFormat(_hWnd,
                                'ms');
end;


procedure MCIWndSetActiveTimer(_hWnd: HWND; active: UINT); inline;
begin
  MCIWndSM(_hWnd,
          MCIWNDM_SETACTIVETIMER,
	        WPARAM(active),
          0);
end;


procedure MCIWndSetInactiveTimer(_hWnd: HWND; inactive: UINT); inline;
begin
  MCIWndSM(_hWnd,
           MCIWNDM_SETINACTIVETIMER,
         	 WPARAM(inactive),
           0);
end;


procedure MCIWndSetTimers(_hWnd: HWND; active: UINT; inactive: UINT); inline;
begin
  MCIWndSM(_hWnd,
           MCIWNDM_SETTIMERS,
           WPARAM(active),
	         LPARAM(inactive));
end;


function MCIWndGetActiveTimer(_hWnd: HWND): UINT; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETACTIVETIMER,
                     0,
                     0);
end;

function MCIWndGetInactiveTimer(_hWnd: HWND): UINT; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETINACTIVETIMER,
                     0,
                     0);
end;


function MCIWndRealize(_hWnd: HWND; fBkgnd: BOOL): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_REALIZE,
                     WPARAM(fBkgnd),
                     0);
end;


function MCIWndSendString(_hWnd: HWND; sz: LPCSTR): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_SENDSTRING,
                     0,
                     LPARAM(sz));
end;

function MCIWndReturnString(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_RETURNSTRING,
                     WPARAM(len),
                     LPARAM(lp));
end;

function MCIWndGetError(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETERROR,
                     WPARAM(len),
                     LPARAM(lp));
end;

//#define MCIWndActivate(_hWnd, f)     (void)MCIWndSM(_hWnd, WM_ACTIVATE, (WPARAM)(BOOL)(f), 0)

function MCIWndGetPalette(_hWnd: HWND): HPALETTE; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETPALETTE,
                     0,
                     0);
end;

function MCIWndSetPalette(_hWnd: HWND; hpal: HPALETTE): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_SETPALETTE,
                     WPARAM(hpal),
                     0);
end;


function MCIWndGetFileName(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETFILENAME,
                     WPARAM(len),
                     LPARAM(lp));
end;

function MCIWndGetDevice(_hWnd: HWND; lp: Pointer; len: UINT): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETDEVICE,
                     WPARAM(len),
                     LPARAM(lp));
end;

function MCIWndGetStyles(_hWnd: HWND): UINT; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_GETSTYLES,
                     0,
                     0);
end;

function MCIWndChangeStyles(_hWnd: HWND; mask: UINT; value: LONG): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_CHANGESTYLES,
                     WPARAM(mask),
                     LPARAM(value));
end;

function MCIWndOpenInterface(_hWnd: HWND; pUnk: PUnknown): LONG;  inline;
begin
  Result := MCIWndSM(_hWnd,
            MCIWNDM_OPENINTERFACE,
            0,
            LPARAM(pUnk));
end;

function MCIWndSetOwner(_hWnd: HWND; hwndP: DWORD): LONG; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_SETOWNER,
                     WPARAM(hwndP),
                     0);
end;

// AVICAP ======================================================================

function AVICapSM(_hWnd: HWND;
                  m: UINT;
                  w: WPARAM;
                  l: LPARAM): DWORD;
begin
  if IsWindow(_hWnd) then
    begin
      Result := SendMessage(_hWnd,
                            m,
                            w,
                            l);
    end
  else
    Result := 0;
end;


// MCIWndSM ====================================================================

function MCIWndSM(_hWnd: HWND;
                  Msg: UINT;
                  wParm: WPARAM;
                  lParm: LPARAM): DWORD;
begin
  Result := SendMessage(_hWnd,
                        Msg,
                        wParm,
                        lParm);
end;

// can macros ==================================================================

function MCIWndCanPlay(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_CAN_PLAY,
                     0,
                     0) <> 0;
end;

function MCIWndCanRecord(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_CAN_RECORD,
                     0,
                     0) <> 0;
end;

function MCIWndCanSave(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                          MCIWNDM_CAN_SAVE,
                          0,
                          0) <> 0;
end;

function MCIWndCanWindow(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_CAN_WINDOW,
                     0,
                     0) <> 0;
end;

function MCIWndCanEject(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_CAN_EJECT,
                     0,
                     0) <> 0;
end;

function MCIWndCanConfig(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_CAN_CONFIG,
                     0,
                     0) <> 0;
end;

function MCIWndPaletteKick(_hWnd: HWND): BOOL; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCIWNDM_PALETTEKICK,
                     0,
                     0) <> 0;
end;

function MCIWndSave(_hWnd: HWND;
                    szFile: LPCSTR): DWORD; inline;
begin
  Result := MCIWndSM(_hWnd,
                     MCI_SAVE,
                     0,
                     LPARAM(szFile));
end;

function MCIWndSaveDialog(_hWnd: HWND): DWORD; inline;
begin
  Result := MCIWndSave(_hWnd,
                       LPCSTR(-1));
end;


// AVI error ===================================================================

function MAKE_AVIERR(error: DWord): DWord; inline;
begin
  Result := MAKE_SCODE(SEVERITY_ERROR,
                       FACILITY_ITF, $4000 + error);
end;

// Externals ===================================================================

function VideoForWindowsVersion: DWord; pascal; external VfWLib;

//function InitVFW
//function TermVFW

//  ICM function declarations
function ICInfo; external VfWLib name 'ICInfo';
function ICInstall; external VfWLib name 'ICInstall';
function ICRemove; external VfWLib name 'ICRemove';
function ICGetInfo; external VfWLib name 'ICGetInfo';
function ICOpen; external VfWLib name 'ICOpen';
function ICOpenFunction; external VfWLib name 'ICOpenFunction';
function ICClose; external VfWLib name 'ICClose';
function ICSendMessage; external VfWLib name 'ICSendMessage';

function ICCompress; external VfWLib name 'ICCompress';

function ICDecompress; external VfWLib name 'ICDecompress';
function ICDrawBegin; external VfWLib name 'ICDrawBegin';
function ICDraw; external VfWLib name 'ICDraw';
function ICLocate; external VfWLib name 'ICLocate';
function ICGetDisplayFormat; external VfWLib name 'ICGetDisplayFormat';

function ICImageCompress; external VfWLib name 'ICImageCompress';
function ICImageDecompress; external VfWLib name 'ICImageDecompress';

function ICCompressorChoose; external VfWLib name 'ICCompressorChoose';

function ICSeqCompressFrameStart; external VfWLib name 'ICSeqCompressFrameStart';
procedure ICSeqCompressFrameEnd; external VfWLib name 'ICSeqCompressFrameEnd';
function ICSeqCompressFrame; external VfWLib name 'ICSeqCompressFrame';
procedure ICCompressorFree; external VfWLib name 'ICCompressorFree';

function DrawDibOpen; external VfWLib name 'DrawDibOpen';
function DrawDibClose; external VfWLib name 'DrawDibClose';
function DrawDibGetBuffer; external VfWLib name 'DrawDibGetBuffer';
function DrawDibGetPalette; external VfWLib name 'DrawDibGetPalette';
function DrawDibSetPalette; external VfWLib name 'DrawDibSetPalette';
function DrawDibChangePalette; external VfWLib name 'DrawDibChangePalette';
function DrawDibRealize; external VfWLib name 'DrawDibRealize';
function DrawDibStart; external VfWLib name 'DrawDibStart';
function DrawDibStop; external VfWLib name 'DrawDibStop';
function DrawDibBegin; external VfWLib name 'DrawDibBegin';
function DrawDibDraw; external VfWLib name 'DrawDibDraw';
function DrawDibEnd; external VfWLib name 'DrawDibEnd';
function DrawDibTime; external VfWLib name 'DrawDibTime';
function DrawDibProfileDisplay; external VfWLib name 'DrawDibProfileDisplay';
procedure StretchDIB; external VfWLib name 'StretchDIB';


// avi

procedure AVIFileInit; external AvifilLib name 'AVIFileInit';
procedure AVIStreamInit; external AvifilLib name 'AVIFileInit';
procedure AVIFileExit; external AvifilLib name 'AVIFileExit';
procedure AVIStreamExit; external AvifilLib name 'AVIFileExit';
function AVIFileAddRef; external AvifilLib name 'AVIFileAddRef';
function AVIFileRelease; external AvifilLib name 'AVIFileRelease';
function AVIFileClose; external AvifilLib name 'AVIFileRelease';
function AVIFileOpenW; external AvifilLib name 'AVIFileOpenW';
function AVIFileOpenA; external AvifilLib name 'AVIFileOpenA';
function AVIFileOpen; external AvifilLib name 'AVIFileOpenW';
function AVIFileInfoW; external AvifilLib name 'AVIFileInfoW';
function AVIFileInfoA; external AvifilLib name 'AVIFileInfoA';
function AVIFileInfo; external AvifilLib name 'AVIFileInfoW';
function AVIFileGetStream; external AvifilLib name 'AVIFileGetStream';
{$IFDEF UNICODE}
function AVIFileCreateStreamW; external AvifilLib name 'AVIFileCreateStreamW';
{$ELSE}
function AVIFileCreateStreamA; external AvifilLib name 'AVIFileCreateStreamA';
{$ENDIF}
function AVIFileCreateStream; external AvifilLib name 'AVIFileCreateStreamW';
function AVIFileWriteData; external AvifilLib name 'AVIFileWriteData';
function AVIFileReadData; external AvifilLib name 'AVIFileReadData';
function AVIFileEndRecord; external AvifilLib name 'AVIFileEndRecord';

function AVIStreamAddRef; external AvifilLib name 'AVIStreamAddRef';
function AVIStreamRelease; external AvifilLib name 'AVIStreamRelease';
function AVIStreamClose; external AvifilLib name 'AVIStreamRelease';
{$IFDEF UNICODE}
function AVIStreamInfoW; external AvifilLib name 'AVIStreamInfoW';
{$ELSE}
function AVIStreamInfoA; external AvifilLib name 'AVIStreamInfoA';
{$ENDIF}
function AVIStreamInfo; external AvifilLib name 'AVIStreamInfoW';
function AVIStreamFindSample; external AvifilLib name 'AVIStreamFindSample';
function AVIStreamFindKeyFrame; external AvifilLib name 'AVIStreamFindSample';
function AVIStreamReadFormat; external AvifilLib name 'AVIStreamReadFormat';
function AVIStreamSetFormat; external AvifilLib name 'AVIStreamSetFormat';
function AVIStreamReadData; external AvifilLib name 'AVIStreamReadData';
function AVIStreamWriteData; external AvifilLib name 'AVIStreamWriteData';
function AVIStreamRead; external AvifilLib name 'AVIStreamRead';
function AVIStreamWrite; external AvifilLib name 'AVIStreamWrite';
function AVIStreamStart; external AvifilLib name 'AVIStreamStart';
function AVIStreamLength; external AvifilLib name 'AVIStreamLength';
function AVIStreamTimeToSample; external AvifilLib name 'AVIStreamTimeToSample';
function AVIStreamSampleToTime; external AvifilLib name 'AVIStreamSampleToTime';
function AVIStreamBeginStreaming; external AvifilLib name 'AVIStreamBeginStreaming';
function AVIStreamEndStreaming; external AvifilLib name 'AVIStreamEndStreaming';
function AVIStreamGetFrameOpen; external AvifilLib name 'AVIStreamGetFrameOpen';
function AVIStreamGetFrame; external AvifilLib name 'AVIStreamGetFrame';
function AVIStreamGetFrameClose; external AvifilLib name 'AVIStreamGetFrameClose';
{$IFDEF UNICODE}
function AVIStreamOpenFromFileW; external AvifilLib name 'AVIStreamOpenFromFileW';
{$ELSE}
function AVIStreamOpenFromFileA; external AvifilLib name 'AVIStreamOpenFromFileA';
{$ENDIF}
function AVIStreamOpenFromFile; external AvifilLib name 'AVIStreamOpenFromFileW';
function AVIStreamCreate; external AvifilLib name 'AVIStreamCreate';
function AVIMakeCompressedStream; external AvifilLib name 'AVIMakeCompressedStream';
{$IFDEF UNICODE}
function AVISaveVW; external AvifilLib name 'AVISaveVW';
{$ELSE}
function AVISaveVA; external AvifilLib name 'AVISaveVA';
{$ENDIF}
function AVISaveW; external AvifilLib name 'AVISaveVW';
function AVISaveOptions; external AvifilLib name 'AVISaveOptions';
function AVISaveOptionsFree; external AvifilLib name 'AVISaveOptionsFree';
{$IFDEF UNICODE}
function AVIBuildFilterW; external AvifilLib name 'AVIBuildFilterW';
{$ELSE}
function AVIBuildFilterA; external AvifilLib name 'AVIBuildFilterA';
{$ENDIF}
function AVIBuildFilter; external AvifilLib name 'AVIBuildFilterW';
function AVIMakeFileFromStreams; external AvifilLib name 'AVIMakeFileFromStreams';
function AVIMakeStreamFromClipboard; external AvifilLib name 'AVIMakeStreamFromClipboard';

function AVIPutFileOnClipboard; external AvifilLib name 'AVIPutFileOnClipboard';
function AVIGetFromClipboard; external AvifilLib name 'AVIGetFromClipboard';
function AVIClearClipboard; external AvifilLib name 'AVIClearClipboard';

function CreateEditableStream; external AvifilLib name 'CreateEditableStream';
function EditStreamCut; external AvifilLib name 'EditStreamCut';
function EditStreamCopy; external AvifilLib name 'EditStreamCopy';
function EditStreamPaste; external AvifilLib name 'EditStreamPaste';
function EditStreamClone; external AvifilLib name 'EditStreamClone';
{$IFDEF UNICODE}
function EditStreamSetNameW; external AvifilLib name 'EditStreamSetNameW';
{$ELSE}
function EditStreamSetNameA; external AvifilLib name 'EditStreamSetNameA';
{$ENDIF}
function EditStreamSetName; external AvifilLib name 'EditStreamSetNameW';
{$IFDEF UNICODE}
function EditStreamSetInfoW; external AvifilLib name 'EditStreamSetInfoW';
{$ELSE}
function EditStreamSetInfoA; external AvifilLib name 'EditStreamSetInfoA';
{$ENDIF}
function EditStreamSetInfo; external AvifilLib name 'EditStreamSetInfoW';
{$IFDEF UNICODE}
function MCIWndCreateW; external VfWLib name 'MCIWndCreateW';
{$ELSE}
function MCIWndCreateA; external VfWLib name 'MCIWndCreateA';
{$ENDIF}
function MCIWndCreate; external VfWLib name 'MCIWndCreateW';
function MCIWndRegisterClass; external VfWLib name 'MCIWndRegisterClass';
{$IFDEF UNICODE}
function capCreateCaptureWindowW; external AviCapLib name 'capCreateCaptureWindowW';
function capGetDriverDescriptionW; external AviCapLib name 'capGetDriverDescriptionW';
{$ELSE}
function capCreateCaptureWindowA; external AviCapLib name 'capCreateCaptureWindowA';
function capGetDriverDescriptionA; external AviCapLib name 'capGetDriverDescriptionA';
{$ENDIF}
function capCreateCaptureWindow; external AviCapLib name 'capCreateCaptureWindowW';
function capGetDriverDescription; external AviCapLib name 'capGetDriverDescriptionW';
{$IFDEF UNICODE}
function GetOpenFileNamePreviewW; external VfWLib name 'GetOpenFileNamePreviewW';
function GetSaveFileNamePreviewW; external VfWLib name 'GetSaveFileNamePreviewW';
{$ELSE}
function GetOpenFileNamePreviewA; external VfWLib name 'GetOpenFileNamePreviewA';
function GetSaveFileNamePreviewA; external VfWLib name 'GetSaveFileNamePreviewA';
{$ENDIF}
function GetOpenFileNamePreview; external VfWLib name 'GetOpenFileNamePreviewW';
function GetSaveFileNamePreview; external VfWLib name 'GetSaveFileNamePreviewW';

  // Implement Additional Prototypes here.

end.

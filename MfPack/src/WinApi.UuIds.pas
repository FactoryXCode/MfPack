// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.UuIds.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2012
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
//          Contains the GUIDs for the MediaType type, subtype fields and format
//          types for standard media types, and also class ids for well-known
//          components.
//
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
// Source: uuids.h
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
unit WinApi.UuIds;

  {$HPPEMIT '#include "uuids.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

  //
  //  We want to use this list for generating strings for debugging too
  //  so we redefine OUR_GUID_ENTRY depending on what we want to do
  //
  //  It is imperative that all entries in this file are declared using
  //  OUR_GUID_ENTRY as that macro might have been defined in advance of
  //  including this file.  See wxdebug.cpp in sdk\classes\base.
  //
  //#ifndef OUR_GUID_ENTRY
  //#define   name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
  //  DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8);
  //#endif


  // -- to allow consistent labeling of Media types and subtypes --
const

  MEDIATYPE_NULL     :  TGUID =  (D1:$00000000;
                                  D2:$0000;
                                  D3:$0000;
                                  D4:($00, $00, $00, $00, $00, $00, $00, $00)); // = GUID_NULL
  {$EXTERNALSYM MEDIATYPE_NULL}

  MEDIASUBTYPE_NULL  :  TGUID =  (D1:$00000000;
                                  D2:$0000;
                                  D3:$0000;
                                  D4:($00, $00, $00, $00, $00, $00, $00, $00)); // = GUID_NULL
  {$EXTERNALSYM MEDIASUBTYPE_NULL}


  // -- Use this subtype if you don't have a use for a subtype for your type
  // e436eb8e-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_None
  MEDIASUBTYPE_None    :  TGUID = (D1: $e436eb8e;
                                   D2: $524f;
                                   D3: $11ce;
                                   D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_None}


// -- major types ---


  // 73646976-0000-0010-8000-00AA00389B71  'vids' == MEDIATYPE_Video
  MEDIATYPE_Video  :  TGUID = (D1: $73646976; D2: $0000; D3: $0010;
                               D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_Video}

  // 73647561-0000-0010-8000-00AA00389B71  'auds' == MEDIATYPE_Audio
  MEDIATYPE_Audio  :  TGUID = (D1: $73647561; D2: $0000; D3: $0010;
                               D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_Audio}

  // 73747874-0000-0010-8000-00AA00389B71  'txts' == MEDIATYPE_Text
  MEDIATYPE_Text  :  TGUID = (D1: $73747874; D2: $0000; D3: $0010;
                              D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_Text}

  // 7364696D-0000-0010-8000-00AA00389B71  'mids' == MEDIATYPE_Midi
  MEDIATYPE_Midi  :  TGUID = (D1: $7364696D; D2: $0000; D3: $0010;
                              D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_Midi}

  // e436eb83-524f-11ce-9f53-0020af0ba770            MEDIATYPE_Stream
  MEDIATYPE_Stream  :  TGUID = (D1: $e436eb83; D2: $524f; D3: $11ce;
                                D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIATYPE_Stream}

  // 73(s)76(v)61(a)69(i)-0000-0010-8000-00AA00389B71  'iavs' == MEDIATYPE_Interleaved
  MEDIATYPE_Interleaved  :  TGUID = (D1: $73766169; D2: $0000; D3: $0010;
                                     D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_Interleaved}

  // 656c6966-0000-0010-8000-00AA00389B71  'file' == MEDIATYPE_File
  MEDIATYPE_File         :  TGUID = (D1: $656c6966; D2: $0000; D3: $0010;
                                     D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_File}

  // 73636d64-0000-0010-8000-00AA00389B71  'scmd' == MEDIATYPE_ScriptCommand
  MEDIATYPE_ScriptCommand  :  TGUID = (D1: $73636d64; D2: $0000; D3: $0010;
                                       D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_ScriptCommand}

  // 670AEA80-3A82-11d0-B79B-00AA003767A7            MEDIATYPE_AUXLine21Data
  MEDIATYPE_AUXLine21Data  :  TGUID = (D1: $670aea80; D2: $3a82; D3: $11d0;
                                       D4: ($b7, $9b, $0, $aa, $0, $37, $67, $a7));
  {$EXTERNALSYM MEDIATYPE_AUXLine21Data}

  // {11264ACB-37DE-4eba-8C35-7F04A1A68332}
  MEDIATYPE_AUXTeletextPage  :  TGUID = (D1: $11264acb; D2: $37de; D3: $4eba;
                                         D4: ($8c, $35, $7f, $4, $a1, $a6, $83, $32));
  {$EXTERNALSYM MEDIATYPE_AUXTeletextPage}

  // AEB312E9-3357-43ca-B701-97EC198E2B62            MEDIATYPE_CC_CONTAINER
  MEDIATYPE_CC_CONTAINER  :  TGUID = (D1: $aeb312e9; D2: $3357; D3: $43ca;
                                     D4: ($b7, $1, $97, $ec, $19, $8e, $2b, $62));
  {$EXTERNALSYM MEDIATYPE_CC_CONTAINER}

  // FB77E152-53B2-499c-B46B-509FC33EDFD7             MEDIATYPE_DTVCCData
  MEDIATYPE_DTVCCData  :  TGUID = (D1: $fb77e152; D2: $53b2; D3: $499c;
                                   D4: ($b4, $6b, $50, $9f, $c3, $3e, $df, $d7));
  {$EXTERNALSYM MEDIATYPE_DTVCCData}

  // B88B8A89-B049-4C80-ADCF-5898985E22C1             MEDIATYPE_MSTVCaption
  MEDIATYPE_MSTVCaption  :  TGUID = (D1: $B88B8A89; D2: $B049; D3: $4C80;
                                     D4: ($AD, $CF, $58, $98, $98, $5E, $22, $C1));
  {$EXTERNALSYM MEDIATYPE_MSTVCaption}

  // F72A76E1-EB0A-11D0-ACE4-0000C0CC16BA            MEDIATYPE_VBI
  MEDIATYPE_VBI  :  TGUID = (D1: $f72a76e1; D2: $eb0a; D3: $11d0;
                             D4: ($ac, $e4, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM MEDIATYPE_VBI}

  // 34FFCBC3-D5B3-4171-9002-D4C60301697F             DVB_SUBTITLES
  MEDIASUBTYPE_DVB_SUBTITLES  :  TGUID = (D1: $34FFCBC3; D2: $D5B3; D3: $4171;
                                          D4: ($90, $02, $D4, $C6, $03, $01, $69, $7F));
  {$EXTERNALSYM MEDIASUBTYPE_DVB_SUBTITLES}

  // 059DD67D-2E55-4d41-8D1B-01F5E4F50607            ISDB_CAPTIONS
  MEDIASUBTYPE_ISDB_CAPTIONS  :  TGUID = (D1: $059dd67d; D2: $2e55; D3: $4d41;
                                         D4: ($8d, $1b, $01, $f5, $e4, $f5, $06, $07));
  {$EXTERNALSYM MEDIASUBTYPE_ISDB_CAPTIONS}

  // 36dc6d28-f1a6-4216-9048-9cfcefeb5eba            ISDB_SUPERIMPOSE
  MEDIASUBTYPE_ISDB_SUPERIMPOSE  :  TGUID = (D1: $36dc6d28; D2: $f1a6; D3: $4216;
                                             D4: ($90, $48, $9c, $fc, $ef, $eb, $5e, $ba));
  {$EXTERNALSYM MEDIASUBTYPE_ISDB_SUPERIMPOSE}

  // 0482DEE3-7817-11cf-8a03-00aa006ecb65            MEDIATYPE_Timecode
  MEDIATYPE_Timecode  :  TGUID = (D1: $482dee3; D2: $7817; D3: $11cf;
                                  D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIATYPE_Timecode}

  // 74726c6d-0000-0010-8000-00AA00389B71  'lmrt' == MEDIATYPE_LMRT
  MEDIATYPE_LMRT  :  TGUID = (D1: $74726c6d; D2: $0000; D3: $0010;
                              D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_LMRT}

  // 74726c6d-0000-0010-8000-00AA00389B71  'urls' == MEDIATYPE_URL_STREAM
  MEDIATYPE_URL_STREAM  :  TGUID = (D1: $736c7275; D2: $0000; D3: $0010;
                                    D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIATYPE_URL_STREAM}


  // -- sub types ---


  // 4C504C43-0000-0010-8000-00AA00389B71  'CLPL' == MEDIASUBTYPE_CLPL
  MEDIASUBTYPE_CLPL  :  TGUID = (D1: $4C504C43; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_CLPL}

  // 56595559-0000-0010-8000-00AA00389B71  'YUYV' == MEDIASUBTYPE_YUYV
  MEDIASUBTYPE_YUYV  :  TGUID = (D1: $56595559; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_YUYV}

  // 56555949-0000-0010-8000-00AA00389B71  'IYUV' == MEDIASUBTYPE_IYUV
  MEDIASUBTYPE_IYUV  :  TGUID = (D1: $56555949; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IYUV}

  // 39555659-0000-0010-8000-00AA00389B71  'YVU9' == MEDIASUBTYPE_YVU9
  MEDIASUBTYPE_YVU9  :  TGUID = (D1: $39555659; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_YVU9}

  // 31313459-0000-0010-8000-00AA00389B71  'Y411' == MEDIASUBTYPE_Y411
  MEDIASUBTYPE_Y411  :  TGUID = (D1: $31313459; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_Y411}

  // 50313459-0000-0010-8000-00AA00389B71  'Y41P' == MEDIASUBTYPE_Y41P
  MEDIASUBTYPE_Y41P  :  TGUID = (D1: $50313459; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_Y41P}

  // 32595559-0000-0010-8000-00AA00389B71  'YUY2' == MEDIASUBTYPE_YUY2
  MEDIASUBTYPE_YUY2 :  TGUID = (D1: $32595559; D2: $0000; D3: $0010;
                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_YUY2}

  // 55595659-0000-0010-8000-00AA00389B71  'YVYU' == MEDIASUBTYPE_YVYU
  MEDIASUBTYPE_YVYU  :  TGUID = (D1: $55595659; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_YVYU}

  // 59565955-0000-0010-8000-00AA00389B71  'UYVY' ==  MEDIASUBTYPE_UYVY
  MEDIASUBTYPE_UYVY  :  TGUID = (D1: $59565955; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_UYVY}

  // 31313259-0000-0010-8000-00AA00389B71  'Y211' ==  MEDIASUBTYPE_Y211
  MEDIASUBTYPE_Y211  :  TGUID = (D1: $31313259; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_Y211}

  // 524a4c43-0000-0010-8000-00AA00389B71  'CLJR' ==  MEDIASUBTYPE_CLJR
  MEDIASUBTYPE_CLJR  :  TGUID = (D1: $524a4c43; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_CLJR}

  // 39304649-0000-0010-8000-00AA00389B71  'IF09' ==  MEDIASUBTYPE_IF09
  MEDIASUBTYPE_IF09  :  TGUID = (D1: $39304649; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IF09}

  // 414c5043-0000-0010-8000-00AA00389B71  'CPLA' ==  MEDIASUBTYPE_CPLA
  MEDIASUBTYPE_CPLA  :  TGUID = (D1: $414c5043; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_CPLA}

  // 47504A4D-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_MJPG
  MEDIASUBTYPE_MJPG  :  TGUID = (D1: $47504A4D; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_MJPG}

  // 4A4D5654-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_TVMJ
  MEDIASUBTYPE_TVMJ  :  TGUID = (D1: $4A4D5654; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_TVMJ}

  // 454B4157-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_WAKE
  MEDIASUBTYPE_WAKE  :  TGUID = (D1: $454B4157; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_WAKE}

  // 43434643-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_CFCC
  MEDIASUBTYPE_CFCC  :  TGUID = (D1: $43434643; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_CFCC}

  // 47504A49-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_IJPG
  MEDIASUBTYPE_IJPG  :  TGUID = (D1: $47504A49; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IJPG}

  // 6D756C50-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_Plum
  MEDIASUBTYPE_Plum  :  TGUID = (D1: $6D756C50; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_Plum}

  // FAST DV-Master
  // 53435644-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_DVCS
  MEDIASUBTYPE_DVCS  :  TGUID = (D1: $53435644; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_DVCS}

  // H.264 compressed video stream
  // 34363248-0000-0010-8000-00AA00389B71  'H264' == MEDIASUBTYPE_H264
  MEDIASUBTYPE_H264  :  TGUID = (D1: $34363248; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_H264}

  // FAST DV-Master
  // 44535644-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_DVSD
  MEDIASUBTYPE_DVSD  :  TGUID = (D1: $44535644; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_DVSD}

  // MIROVideo DV
  // 4656444D-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_MDVF
  MEDIASUBTYPE_MDVF  :  TGUID = (D1: $4656444D; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_MDVF}

  // e436eb78-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB1
  MEDIASUBTYPE_RGB1  :  TGUID = (D1: $e436eb78; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB1}

  // e436eb79-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB4
  MEDIASUBTYPE_RGB4  :  TGUID = (D1: $e436eb79; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB4}

  // e436eb7a-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB8
  MEDIASUBTYPE_RGB8  :  TGUID = (D1: $e436eb7a; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB8}

  // e436eb7b-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB565
  MEDIASUBTYPE_RGB565  :  TGUID = (D1: $e436eb7b; D2: $524f; D3: $11ce;
                                   D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB565}

  // e436eb7c-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB555
  MEDIASUBTYPE_RGB555  :  TGUID = (D1: $e436eb7c; D2: $524f; D3: $11ce;
                                   D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB555}

  // e436eb7d-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB24
  MEDIASUBTYPE_RGB24  :  TGUID = (D1: $e436eb7d; D2: $524f; D3: $11ce;
                                  D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB24}

  // e436eb7e-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_RGB32
  MEDIASUBTYPE_RGB32  :  TGUID = (D1: $e436eb7e; D2: $524f; D3: $11ce;
                                  D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_RGB32}

  //
  // RGB surfaces that contain per pixel alpha values.
  //

  // 297C55AF-E209-4cb3-B757-C76D6B9C88A8            MEDIASUBTYPE_ARGB1555
  MEDIASUBTYPE_ARGB1555  :  TGUID = (D1: $297c55af; D2: $e209; D3: $4cb3;
                                     D4: ($b7, $57, $c7, $6d, $6b, $9c, $88, $a8));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB1555}

  // 6E6415E6-5C24-425f-93CD-80102B3D1CCA            MEDIASUBTYPE_ARGB4444
  MEDIASUBTYPE_ARGB4444  :  TGUID = (D1: $6e6415e6; D2: $5c24; D3: $425f;
                                     D4: ($93, $cd, $80, $10, $2b, $3d, $1c, $ca));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB4444}

  // 773c9ac0-3274-11d0-B724-00aa006c1A01            MEDIASUBTYPE_ARGB32
  MEDIASUBTYPE_ARGB32  :  TGUID = (D1: $773c9ac0; D2: $3274; D3: $11d0;
                                   D4: ($b7, $24, $0, $aa, $0, $6c, $1a, $1));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB32}

  // 2f8bb76d-b644-4550-acf3-d30caa65d5c5            MEDIASUBTYPE_A2R10G10B10
  MEDIASUBTYPE_A2R10G10B10  :  TGUID = (D1: $2f8bb76d; D2: $b644; D3: $4550;
                                        D4: ($ac, $f3, $d3, $0c, $aa, $65, $d5, $c5));
  {$EXTERNALSYM MEDIASUBTYPE_A2R10G10B10}

  // 576f7893-bdf6-48c4-875f-ae7b81834567            MEDIASUBTYPE_A2B10G10R10
  MEDIASUBTYPE_A2B10G10R10  :  TGUID = (D1: $576f7893; D2: $bdf6; D3: $48c4;
                                        D4: ($87, $5f, $ae, $7b, $81, $83, $45, $67));
  {$EXTERNALSYM MEDIASUBTYPE_A2B10G10R10}

  // 56555941-0000-0010-8000-00AA00389B71  'AYUV' == MEDIASUBTYPE_AYUV
  //
  // See the DX-VA header and documentation for a description of this format.
  //
  MEDIASUBTYPE_AYUV  :  TGUID = (D1: $56555941; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_AYUV}

  // 34344941-0000-0010-8000-00AA00389B71  'AI44' == MEDIASUBTYPE_AI44
  //
  // See the DX-VA header and documentation for a description of this format.
  //
  MEDIASUBTYPE_AI44  :  TGUID = (D1: $34344941; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_AI44}

  // 34344149-0000-0010-8000-00AA00389B71  'IA44' == MEDIASUBTYPE_IA44
  //
  // See the DX-VA header and documentation for a description of this format.
  //
  MEDIASUBTYPE_IA44  :  TGUID = (D1: $34344149; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IA44}

  //
  // DirectX7 D3D Render Target media subtypes.
  //

  // 32335237-0000-0010-8000-00AA00389B71  '7R32' == MEDIASUBTYPE_RGB32_D3D_DX7_RT
  MEDIASUBTYPE_RGB32_D3D_DX7_RT  :  TGUID = (D1: $32335237; D2: $0000; D3: $0010;
                                             D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_RGB32_D3D_DX7_RT}

  // 36315237-0000-0010-8000-00AA00389B71  '7R16' == MEDIASUBTYPE_RGB16_D3D_DX7_RT
  MEDIASUBTYPE_RGB16_D3D_DX7_RT  :  TGUID = (D1: $36315237; D2: $0000; D3: $0010;
                                             D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_RGB16_D3D_DX7_RT}

  // 38384137-0000-0010-8000-00AA00389B71  '7A88' == MEDIASUBTYPE_ARGB32_D3D_DX7_RT
  MEDIASUBTYPE_ARGB32_D3D_DX7_RT  :  TGUID = (D1: $38384137; D2: $0000; D3: $0010;
                                              D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB32_D3D_DX7_RT}

  // 34344137-0000-0010-8000-00AA00389B71  '7A44' == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT
  MEDIASUBTYPE_ARGB4444_D3D_DX7_RT  :  TGUID = (D1: $34344137; D2: $0000; D3: $0010;
                                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB4444_D3D_DX7_RT}

  // 35314137-0000-0010-8000-00AA00389B71  '7A15' == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT
  MEDIASUBTYPE_ARGB1555_D3D_DX7_RT  :  TGUID = (D1: $35314137; D2: $0000; D3: $0010;
                                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB1555_D3D_DX7_RT}


  //
  // DirectX9 D3D Render Target media subtypes.
  //

  // 32335239-0000-0010-8000-00AA00389B71  '9R32' == MEDIASUBTYPE_RGB32_D3D_DX9_RT
  MEDIASUBTYPE_RGB32_D3D_DX9_RT  :  TGUID = (D1: $32335239; D2: $0000; D3: $0010;
                                             D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_RGB32_D3D_DX9_RT}

  // 36315239-0000-0010-8000-00AA00389B71  '9R16' == MEDIASUBTYPE_RGB16_D3D_DX9_RT
  MEDIASUBTYPE_RGB16_D3D_DX9_RT  :  TGUID = (D1: $36315239; D2: $0000; D3: $0010;
                                             D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_RGB16_D3D_DX9_RT}

  // 38384139-0000-0010-8000-00AA00389B71  '9A88' == MEDIASUBTYPE_ARGB32_D3D_DX9_RT
  MEDIASUBTYPE_ARGB32_D3D_DX9_RT  :  TGUID = (D1: $38384139; D2: $0000; D3: $0010;
                                              D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB32_D3D_DX9_RT}

  // 34344139-0000-0010-8000-00AA00389B71  '9A44' == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT
  MEDIASUBTYPE_ARGB4444_D3D_DX9_RT  :  TGUID = (D1: $34344139; D2: $0000; D3: $0010;
                                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB4444_D3D_DX9_RT}

  // 35314139-0000-0010-8000-00AA00389B71  '9A15' == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT
  MEDIASUBTYPE_ARGB1555_D3D_DX9_RT  :  TGUID = (D1: $35314139; D2: $0000; D3: $0010;
                                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_ARGB1555_D3D_DX9_RT}


   // MACRO stuff
   //#define MEDIASUBTYPE_HASALPHA(mt) ( ((mt).subtype == MEDIASUBTYPE_ARGB4444)            || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB32)              || \
   //                                   ((mt).subtype == MEDIASUBTYPE_AYUV)                || \
   //                                   ((mt).subtype == MEDIASUBTYPE_AI44)                || \
   //                                   ((mt).subtype == MEDIASUBTYPE_IA44)                || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB1555)            || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX7_RT)   || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT) || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT) || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX9_RT)   || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT) || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT) )
   //
   //#define MEDIASUBTYPE_HASALPHA7(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX7_RT)   || \
   //                                   ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT) || \
   //                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT) )
   //
   //#define MEDIASUBTYPE_D3D_DX7_RT(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX7_RT)   || \
   //                                     ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT) || \
   //                                     ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT) || \
   //                                     ((mt).subtype == MEDIASUBTYPE_RGB32_D3D_DX7_RT)    || \
   //                                     ((mt).subtype == MEDIASUBTYPE_RGB16_D3D_DX7_RT))
   //
   //#define MEDIASUBTYPE_HASALPHA9(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX9_RT)   || \
   //                                    ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT) || \
   //                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT) )
   //
   //
   //#define MEDIASUBTYPE_D3D_DX9_RT(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX9_RT)   || \
   //                                     ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT) || \
   //                                     ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT) || \
   //                                     ((mt).subtype == MEDIASUBTYPE_RGB32_D3D_DX9_RT)    || \
   //                                     ((mt).subtype == MEDIASUBTYPE_RGB16_D3D_DX9_RT))
   //
   //----------------------------------------------------------------------------------------------

  //
  // DX-VA uncompressed surface formats
  //

  // 32315659-0000-0010-8000-00AA00389B71  'YV12' ==  MEDIASUBTYPE_YV12
  MEDIASUBTYPE_YV12  :  TGUID = (D1: $32315659; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_YV12}

  // 3231564E-0000-0010-8000-00AA00389B71  'NV12' ==  MEDIASUBTYPE_NV12
  MEDIASUBTYPE_NV12  :  TGUID = (D1: $3231564E; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_NV12}

  // 3131564E-0000-0010-8000-00AA00389B71  'NV11' ==  MEDIASUBTYPE_NV11
  //#ifndef MEDIASUBTYPE_NV11_DEFINED
  //#define MEDIASUBTYPE_NV11_DEFINED
  MEDIASUBTYPE_NV11  :  TGUID = (D1: $3131564E; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_NV11}

  //#endif

  // 38303250-0000-0010-8000-00AA00389B71  'P208' ==  MEDIASUBTYPE_P208
  MEDIASUBTYPE_P208  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_P208}

  // 38303250-0000-0010-8000-00AA00389B71  'P210' ==  MEDIASUBTYPE_P210
  MEDIASUBTYPE_P210  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_P210}

  // 38303250-0000-0010-8000-00AA00389B71  'P216' ==  MEDIASUBTYPE_P216
  MEDIASUBTYPE_P216  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_P216}

  // 38303250-0000-0010-8000-00AA00389B71  'P010' ==  MEDIASUBTYPE_P010
  MEDIASUBTYPE_P010  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_P010}

  // 38303250-0000-0010-8000-00AA00389B71  'P016' ==  MEDIASUBTYPE_P016
  MEDIASUBTYPE_P016  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_P016}

  // 38303250-0000-0010-8000-00AA00389B71  'Y210' ==  MEDIASUBTYPE_Y210
  MEDIASUBTYPE_Y210  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_Y210}

  // 38303250-0000-0010-8000-00AA00389B71  'Y216' ==  MEDIASUBTYPE_Y216
  MEDIASUBTYPE_Y216  :  TGUID = (D1: 38303250; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_Y216}

  // 38303450-0000-0010-8000-00AA00389B71  'P408' ==  MEDIASUBTYPE_P408
  MEDIASUBTYPE_P408  :  TGUID = (D1: 38303450; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_P408}

  // 3432564E-0000-0010-8000-00AA00389B71  'NV24' ==  MEDIASUBTYPE_NV24
  MEDIASUBTYPE_NV24  :  TGUID = (D1: $3432564E; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_NV24}

  // 4F303234-0000-0010-8000-00AA00389B71  '420O' ==  MEDIASUBTYPE_420O
  MEDIASUBTYPE_420O  :  TGUID = (D1: $4F303234; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_420O}

  // 31434D49-0000-0010-8000-00AA00389B71  'IMC1' ==  MEDIASUBTYPE_IMC1
  MEDIASUBTYPE_IMC1  :  TGUID = (D1: $31434D49; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IMC1}

  // 32434d49-0000-0010-8000-00AA00389B71  'IMC2' ==  MEDIASUBTYPE_IMC2
  MEDIASUBTYPE_IMC2  :  TGUID = (D1: $32434D49; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IMC2}

  // 33434d49-0000-0010-8000-00AA00389B71  'IMC3' ==  MEDIASUBTYPE_IMC3
  MEDIASUBTYPE_IMC3  :  TGUID = (D1: $33434D49; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IMC3}

  // 34434d49-0000-0010-8000-00AA00389B71  'IMC4' ==  MEDIASUBTYPE_IMC4
  MEDIASUBTYPE_IMC4 :  TGUID = (D1: $34434D49; D2: $0000; D3: $0010;
                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IMC4}

  // 30343353-0000-0010-8000-00AA00389B71  'S340' ==  MEDIASUBTYPE_S340
  MEDIASUBTYPE_S340  :  TGUID = (D1: $30343353; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_S340}

  // 32343353-0000-0010-8000-00AA00389B71  'S342' ==  MEDIASUBTYPE_S342
  MEDIASUBTYPE_S342  :  TGUID = (D1: $32343353; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_S342}

  // e436eb7f-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_Overlay
  MEDIASUBTYPE_Overlay  :  TGUID = (D1: $e436eb7f; D2: $524f; D3: $11ce;
                                    D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_Overlay}

  // e436eb80-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEGPacket
  MEDIASUBTYPE_MPEG1Packet  :  TGUID = (D1: $e436eb80; D2: $524f; D3: $11ce;
                                        D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1Packet}

  // e436eb81-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEG1Payload
  MEDIASUBTYPE_MPEG1Payload  :  TGUID = (D1: $e436eb81; D2: $524f; D3: $11ce;
                                         D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1Payload}

  // 00000050-0000-0010-8000-00AA00389B71         MEDIASUBTYPE_MPEG1AudioPayload
  MEDIASUBTYPE_MPEG1AudioPayload  :  TGUID = (D1: $00000050; D2: $0000; D3: $0010;
                                              D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1AudioPayload}

  // e436eb82-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEG1SystemStream
  MEDIATYPE_MPEG1SystemStream  :  TGUID = (D1: $e436eb82; D2: $524f; D3: $11ce;
                                           D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIATYPE_MPEG1SystemStream}

  // the next consecutive number is assigned to MEDIATYPE_Stream and appears higher up
  // e436eb84-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEG1System
  MEDIASUBTYPE_MPEG1System  :  TGUID = (D1: $e436eb84; D2: $524f; D3: $11ce;
                                        D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1System}

  // e436eb85-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEG1VideoCD
  MEDIASUBTYPE_MPEG1VideoCD  :  TGUID = (D1: $e436eb85; D2: $524f; D3: $11ce;
                                         D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1VideoCD}

  // e436eb86-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEG1Video
  MEDIASUBTYPE_MPEG1Video  :  TGUID = (D1: $e436eb86; D2: $524f; D3: $11ce;
                                       D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1Video}

  // e436eb87-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_MPEG1Audio
  MEDIASUBTYPE_MPEG1Audio  :  TGUID = (D1: $e436eb87; D2: $524f; D3: $11ce;
                                       D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_MPEG1Audio}

  // e436eb88-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_Avi
  MEDIASUBTYPE_Avi  :  TGUID = (D1: $e436eb88; D2: $524f; D3: $11ce;
                                D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_Avi}

  // {3DB80F90-9412-11d1-ADED-0000F8754B99}          MEDIASUBTYPE_Asf
  MEDIASUBTYPE_Asf  :  TGUID = (D1: $3db80f90; D2: $9412; D3: $11d1;
                               D4: ($ad, $ed, $0, $0, $f8, $75, $4b, $99));
  {$EXTERNALSYM MEDIASUBTYPE_Asf}

  // e436eb89-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_QTMovie
  MEDIASUBTYPE_QTMovie  :  TGUID = (D1: $e436eb89; D2: $524f; D3: $11ce;
                                    D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_QTMovie}

  // 617a7072-0000-0010-8000-00AA00389B71         MEDIASUBTYPE_Rpza
  MEDIASUBTYPE_QTRpza  :  TGUID = (D1: $617a7072; D2: $0000; D3: $0010;
                                   D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_QTRpza}

  // 20636d73-0000-0010-8000-00AA00389B71         MEDIASUBTYPE_Smc
  MEDIASUBTYPE_QTSmc  :  TGUID = (D1: $20636d73; D2: $0000; D3: $0010;
                                  D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_QTSmc}

  // 20656c72-0000-0010-8000-00AA00389B71        MEDIASUBTYPE_Rle
  MEDIASUBTYPE_QTRle  :  TGUID = (D1: $20656c72; D2: $0000; D3: $0010;
                                  D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_QTRle}

  // 6765706a-0000-0010-8000-00AA00389B71        MEDIASUBTYPE_Jpeg
  MEDIASUBTYPE_QTJpeg  :  TGUID = (D1: $6765706a; D2: $0000; D3: $0010;
                                   D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_QTJpeg}

  // e436eb8a-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_PCMAudio_Obsolete
  MEDIASUBTYPE_PCMAudio_Obsolete  :  TGUID = (D1: $e436eb8a; D2: $524f; D3: $11ce;
                                              D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_PCMAudio_Obsolete}

  // 00000001-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_PCM
  MEDIASUBTYPE_PCM  :  TGUID = (D1: $00000001; D2: $0000; D3: $0010;
                                D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MEDIASUBTYPE_PCM}

  // e436eb8b-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_WAVE
  MEDIASUBTYPE_WAVE  :  TGUID = (D1: $e436eb8b; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_WAVE}

  // e436eb8c-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_AU
  MEDIASUBTYPE_AU  :  TGUID = (D1: $e436eb8c; D2: $524f; D3: $11ce;
                               D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_AU}

  // e436eb8d-524f-11ce-9f53-0020af0ba770            MEDIASUBTYPE_AIFF
  MEDIASUBTYPE_AIFF  :  TGUID = (D1: $e436eb8d; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM MEDIASUBTYPE_AIFF}

  // 64(d)73(s)76(v)64(d)-0000-0010-8000-00AA00389B71  'dvsd' == MEDIASUBTYPE_dvsd
  MEDIASUBTYPE__dvsd  :  TGUID = (D1: $64737664; D2: $0000; D3: $0010;
                                  D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE__dvsd}

  // 64(d)68(h)76(v)64(d)-0000-0010-8000-00AA00389B71  'dvhd' == MEDIASUBTYPE_dvhd
  MEDIASUBTYPE_dvhd :  TGUID = (D1: $64687664; D2: $0000; D3: $0010;
                                D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_dvhd}

  // 6c(l)73(s)76(v)64(d)-0000-0010-8000-00AA00389B71  'dvsl' == MEDIASUBTYPE_dvsl
  MEDIASUBTYPE_dvsl  :  TGUID = (D1: $6c737664; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_dvsl}

  // 35(5)32(2)76(v)64(d)-0000-0010-8000-00AA00389B71  'dv25' ==  MEDIASUBTYPE_dv25
  MEDIASUBTYPE_dv25  :  TGUID = (D1: $35327664; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_dv25}

  // 30(0)35(5)76(v)64(d)-0000-0010-8000-00AA00389B71  'dv50' ==  MEDIASUBTYPE_dv50
  MEDIASUBTYPE_dv50  :  TGUID = (D1: $30357664; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_dv50}

  // 31(1)68(h)76(v)64(d)-0000-0010-8000-00AA00389B71  'dvh1' ==  MEDIASUBTYPE_dvh1
  MEDIASUBTYPE_dvh1  :  TGUID = (D1: $31687664; D2: $0000; D3: $0010;
                                 D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_dvh1}

  // 6E8D4A22-310C-11d0-B79A-00AA003767A7         MEDIASUBTYPE_Line21_BytePair
  MEDIASUBTYPE_Line21_BytePair  :  TGUID = (D1: $6e8d4a22; D2: $310c; D3: $11d0;
                                            D4: ($b7, $9a, $0, $aa, $0, $37, $67, $a7));
  {$EXTERNALSYM MEDIASUBTYPE_Line21_BytePair}

  // 6E8D4A23-310C-11d0-B79A-00AA003767A7         MEDIASUBTYPE_Line21_GOPPacket
  MEDIASUBTYPE_Line21_GOPPacket  :  TGUID = (D1: $6e8d4a23; D2: $310c; D3: $11d0;
                                             D4: ($b7, $9a, $0, $aa, $0, $37, $67, $a7));
  {$EXTERNALSYM MEDIASUBTYPE_Line21_GOPPacket}

  // 6E8D4A24-310C-11d0-B79A-00AA003767A7         MEDIASUBTYPE_Line21_VBIRawData
  MEDIASUBTYPE_Line21_VBIRawData  :  TGUID = (D1: $6e8d4a24; D2: $310c; D3: $11d0;
                                              D4: ($b7, $9a, $0, $aa, $0, $37, $67, $a7));
  {$EXTERNALSYM MEDIASUBTYPE_Line21_VBIRawData}

  //0AF414BC-4ED2-445e-9839-8F095568AB3C          MEDIASUBTYPE_708_608Data
  MEDIASUBTYPE_708_608Data  :  TGUID = (D1: $af414bc; D2: $4ed2; D3: $445e;
                                        D4: ($98, $39, $8f, $9, $55, $68, $ab, $3c));
   {$EXTERNALSYM MEDIASUBTYPE_708_608Data}

  // F52ADDAA-36F0-43F5-95EA-6D866484262A         MEDIASUBTYPE_DtvCcData
  MEDIASUBTYPE_DtvCcData  :  TGUID = (D1: $F52ADDAA; D2: $36F0; D3: $43F5;
                                      D4: ($95, $EA, $6D, $86, $64, $84, $26, $2A));
  {$EXTERNALSYM MEDIASUBTYPE_DtvCcData}

  // 7EA626DB-54DA-437b-BE9F-F73073ADFA3C         MEDIASUBTYPE_CC_CONTAINER
  MEDIASUBTYPE_CC_CONTAINER  :  TGUID = (D1: $7ea626db; D2: $54da; D3: $437b;
                                         D4: ($be, $9f, $f7, $30, $73, $ad, $fa, $3c));
  {$EXTERNALSYM MEDIASUBTYPE_CC_CONTAINER}

  // F72A76E3-EB0A-11D0-ACE4-0000C0CC16BA         MEDIASUBTYPE_TELETEXT
  MEDIASUBTYPE_TELETEXT  :  TGUID = (D1: $f72a76e3; D2: $eb0a; D3: $11d0;
                                     D4: ($ac, $e4, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM MEDIASUBTYPE_TELETEXT}

  // 663DA43C-03E8-4e9a-9CD5-BF11ED0DEF76         MEDIASUBTYPE_VBI
  MEDIASUBTYPE_VBI  :  TGUID = (D1: $663da43c; D2: $3e8; D3: $4e9a;
                                D4: ($9c, $d5, $bf, $11, $ed, $d, $ef, $76));
  {$EXTERNALSYM MEDIASUBTYPE_VBI}

  // 2791D576-8E7A-466F-9E90-5D3F3083738B         MEDIASUBTYPE_WSS
  MEDIASUBTYPE_WSS  :  TGUID = (D1: $2791D576; D2: $8E7A; D3: $466F;
                                D4: ($9E, $90, $5D, $3F, $30, $83, $73, $8B));
  {$EXTERNALSYM MEDIASUBTYPE_WSS}

  // 01CA73E3-DCE6-4575-AFE1-2BF1C902CAF3         MEDIASUBTYPE_XDS
  MEDIASUBTYPE_XDS  :  TGUID = (D1: $01ca73e3; D2: $dce6; D3: $4575;
                                D4: ($af, $e1, $2b, $f1, $c9, $2, $ca, $f3));
  {$EXTERNALSYM MEDIASUBTYPE_XDS}

  // A1B3F620-9792-4d8d-81A4-86AF25772090         MEDIASUBTYPE_VPS
  MEDIASUBTYPE_VPS  :  TGUID = (D1: $a1b3f620; D2: $9792; D3: $4d8d;
                                D4: ($81, $a4, $86, $af, $25, $77, $20, $90));
  {$EXTERNALSYM MEDIASUBTYPE_VPS}

  // derived from WAVE_FORMAT_DRM
  // 00000009-0000-0010-8000-00aa00389b71
  MEDIASUBTYPE_DRM_Audio  :  TGUID = (D1: $00000009; D2: $0000; D3: $0010;
                                      D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_DRM_Audio}

  // derived from WAVE_FORMAT_IEEE_FLOAT
  // 00000003-0000-0010-8000-00aa00389b71
  MEDIASUBTYPE_IEEE_FLOAT  :  TGUID = (D1: $00000003; D2: $0000; D3: $0010;
                                       D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_IEEE_FLOAT}

  // derived from WAVE_FORMAT_DOLBY_AC3_SPDIF
  // 00000092-0000-0010-8000-00aa00389b71
  MEDIASUBTYPE_DOLBY_AC3_SPDIF  :  TGUID = (D1: $00000092; D2: $0000; D3: $0010;
                                            D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_DOLBY_AC3_SPDIF}

  // derived from WAVE_FORMAT_RAW_SPORT
  // 00000240-0000-0010-8000-00aa00389b71
  MEDIASUBTYPE_RAW_SPORT  :  TGUID = (D1: $00000240; D2: $0000; D3: $0010;
                                      D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_RAW_SPORT}

  // derived from wave format tag 0x241, call it SPDIF_TAG_241h for now
  // 00000241-0000-0010-8000-00aa00389b71
  MEDIASUBTYPE_SPDIF_TAG_241h  :  TGUID = (D1: $00000241; D2: $0000; D3: $0010;
                                           D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));
  {$EXTERNALSYM MEDIASUBTYPE_SPDIF_TAG_241h}


  // DirectShow DSS definitions


  // A0AF4F81-E163-11d0-BAD9-00609744111A
  MEDIASUBTYPE_DssVideo  :  TGUID = (D1: $a0af4f81; D2: $e163; D3: $11d0;
                                     D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM MEDIASUBTYPE_DssVideo}

  // A0AF4F82-E163-11d0-BAD9-00609744111A
  MEDIASUBTYPE_DssAudio  :  TGUID = (D1: $a0af4f82; D2: $e163; D3: $11d0;
                                     D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM MEDIASUBTYPE_DssAudio}

  // 5A9B6A40-1A22-11D1-BAD9-00609744111A
  MEDIASUBTYPE_VPVideo  :  TGUID = (D1: $5a9b6a40; D2: $1a22; D3: $11d1;
                                    D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM MEDIASUBTYPE_VPVideo}

  // 5A9B6A41-1A22-11D1-BAD9-00609744111A
  MEDIASUBTYPE_VPVBI  :  TGUID = (D1: $5a9b6a41; D2: $1a22; D3: $11d1;
                                  D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM MEDIASUBTYPE_VPVBI}

  // BF87B6E0-8C27-11d0-B3F0-00AA003761C5     Capture graph building
  CLSID_CaptureGraphBuilder  :  TGUID = (D1: $BF87B6E0; D2: $8C27; D3: $11d0;
                                         D4: ($B3, $F0, $0, $AA, $00, $37, $61, $C5));
  {$EXTERNALSYM CLSID_CaptureGraphBuilder}

  // BF87B6E1-8C27-11d0-B3F0-00AA003761C5     New Capture graph building
  CLSID_CaptureGraphBuilder2  :  TGUID = (D1: $BF87B6E1; D2: $8C27; D3: $11d0;
                                          D4: ($B3, $F0, $0, $AA, $00, $37, $61, $C5));
  {$EXTERNALSYM CLSID_CaptureGraphBuilder2}

  // e436ebb0-524f-11ce-9f53-0020af0ba770            Prototype filtergraph
  CLSID_ProtoFilterGraph  :  TGUID = (D1: $e436ebb0; D2: $524f; D3: $11ce;
                                      D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_ProtoFilterGraph}

  // e436ebb1-524f-11ce-9f53-0020af0ba770            Reference clock
  CLSID_SystemClock  :  TGUID = (D1: $e436ebb1; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_SystemClock}

  // e436ebb2-524f-11ce-9f53-0020af0ba770           Filter Mapper
  CLSID_FilterMapper  :  TGUID = (D1: $e436ebb2; D2: $524f; D3: $11ce;
                                  D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_FilterMapper}

  // e436ebb3-524f-11ce-9f53-0020af0ba770           Filter Graph
  CLSID_FilterGraph  :  TGUID = (D1: $e436ebb3; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_FilterGraph}

  // e436ebb8-524f-11ce-9f53-0020af0ba770           Filter Graph no thread
  CLSID_FilterGraphNoThread  :  TGUID = (D1: $e436ebb8; D2: $524f; D3: $11ce;
                                         D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_FilterGraphNoThread}

  // a3ecbc41-581a-4476-b693-a63340462d8b
  CLSID_FilterGraphPrivateThread  :  TGUID = (D1: $a3ecbc41; D2: $581a; D3: $4476;
                                              D4: ($b6, $93, $a6, $33, $40, $46, $2d, $8b));
  {$EXTERNALSYM CLSID_FilterGraphPrivateThread}

  // e4bbd160-4269-11ce-838d-00aa0055595a           MPEG System stream
  CLSID_MPEG1Doc  :  TGUID = (D1: $e4bbd160; D2: $4269; D3: $11ce;
                              D4: ($83, $8d, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_MPEG1Doc}

  // 701722e0-8ae3-11ce-a85c-00aa002feab5           MPEG file reader
  CLSID_FileSource :  TGUID = (D1: $701722e0; D2: $8ae3; D3: $11ce;
                               D4: ($a8, $5c, $00, $aa, $00, $2f, $ea, $b5));
  {$EXTERNALSYM CLSID_FileSource}

  // 26C25940-4CA9-11ce-A828-00AA002FEAB5           Takes MPEG1 packets as input
  CLSID_MPEG1PacketPlayer  :  TGUID = (D1: $26c25940; D2: $4ca9; D3: $11ce;
                                       D4: ($a8, $28, $00, $aa, $00, $2f, $ea, $b5));
  {$EXTERNALSYM CLSID_MPEG1PacketPlayer}

  // 336475d0-942a-11ce-a870-00aa002feab5           MPEG splitter
  CLSID_MPEG1Splitter  :  TGUID = (D1: $336475d0; D2: $942a; D3: $11ce;
                                   D4: ($a8, $70, $00, $aa, $00, $2f, $ea, $b5));
  {$EXTERNALSYM CLSID_MPEG1Splitter}

  // feb50740-7bef-11ce-9bd9-0000e202599c           MPEG video decoder
  CLSID_CMpegVideoCodec  :  TGUID = (D1: $feb50740; D2: $7bef; D3: $11ce;
                                     D4: ($9b, $d9, $00, $00, $e2, $02, $59, $9c));
  {$EXTERNALSYM CLSID_CMpegVideoCodec}

  // 4a2286e0-7bef-11ce-9bd9-0000e202599c           MPEG audio decoder
  CLSID_CMpegAudioCodec  :  TGUID = (D1: $4a2286e0; D2: $7bef; D3: $11ce;
                                     D4: ($9b, $d9, $00, $00, $e2, $02, $59, $9c));
  {$EXTERNALSYM CLSID_CMpegAudioCodec}

  // e30629d3-27e5-11ce-875d-00608cb78066           Text renderer
  CLSID_TextRender  :  TGUID = (D1: $e30629d3; D2: $27e5; D3: $11ce;
                                D4: ($87, $5d, $00, $60, $8c, $b7, $80, $66));
  {$EXTERNALSYM CLSID_TextRender}


  // {F8388A40-D5BB-11d0-BE5A-0080C706568E}
  CLSID_InfTee  :  TGUID = (D1: $f8388a40; D2: $d5bb; D3: $11d0;
                            D4: ($be, $5a, $00, $80, $c7, $06, $56, $8e));
  {$EXTERNALSYM CLSID_InfTee}

  // 1b544c20-fd0b-11ce-8c63-00aa0044b51e           Avi Stream Splitter
  CLSID_AviSplitter  :  TGUID = (D1: $1b544c20; D2: $fd0b; D3: $11ce;
                                 D4: ($8c, $63, $00, $aa, $00, $44, $b5, $1e));
  {$EXTERNALSYM CLSID_AviSplitter}

  // 1b544c21-fd0b-11ce-8c63-00aa0044b51e           Avi File Reader
  CLSID_AviReader  :  TGUID = (D1: $1b544c21; D2: $fd0b; D3: $11ce;
                               D4: ($8c, $63, $00, $aa, $00, $44, $b5, $1e));
  {$EXTERNALSYM CLSID_AviReader}

  // 1b544c22-fd0b-11ce-8c63-00aa0044b51e           Vfw 2.0 Capture Driver
  CLSID_VfwCapture  :  TGUID = (D1: $1b544c22; D2: $fd0b; D3: $11ce;
                                D4: ($8c, $63, $00, $aa, $00, $44, $b5, $1e));
  {$EXTERNALSYM CLSID_VfwCapture}

  CLSID_CaptureProperties  :  TGUID = (D1: $1B544c22; D2: $FD0B; D3: $11ce;
                                       D4: ($8C, $63, $00, $AA, $00, $44, $B5, $1F));
  {$EXTERNALSYM CLSID_CaptureProperties}

  //e436ebb4-524f-11ce-9f53-0020af0ba770            Control Distributor
  CLSID_FGControl  :  TGUID = (D1: $e436ebb4; D2: $524f; D3: $11ce;
                               D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_FGControl}

  // 44584800-F8EE-11ce-B2D4-00DD01101B85           .MOV reader (old)
  CLSID_MOVReader  :  TGUID = (D1: $44584800; D2: $f8ee; D3: $11ce;
                               D4: ($b2, $d4, $00, $dd, $1, $10, $1b, $85));
  {$EXTERNALSYM CLSID_MOVReader}

  // D51BD5A0-7548-11cf-A520-0080C77EF58A           QT Splitter
  CLSID_QuickTimeParser  :  TGUID = (D1: $d51bd5a0; D2: $7548; D3: $11cf;
                                     D4: ($a5, $20, $0, $80, $c7, $7e, $f5, $8a));
  {$EXTERNALSYM CLSID_QuickTimeParser}

  // FDFE9681-74A3-11d0-AFA7-00AA00B67A42           QT Decoder
  CLSID_QTDec  :  TGUID = (D1: $fdfe9681; D2: $74a3; D3: $11d0;
                           D4: ($af, $a7, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_QTDec}

  // D3588AB0-0781-11ce-B03A-0020AF0BA770           AVIFile-based reader
  CLSID_AVIDoc  :  TGUID = (D1: $d3588ab0; D2: $0781; D3: $11ce;
                            D4: ($b0, $3a, $00, $20, $af, $b, $a7, $70));
  {$EXTERNALSYM CLSID_AVIDoc}

  // 70e102b0-5556-11ce-97c0-00aa0055595a           Video renderer
  CLSID_VideoRenderer  :  TGUID = (D1: $70e102b0; D2: $5556; D3: $11ce;
                                   D4: ($97, $c0, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_VideoRenderer}

  // 1643e180-90f5-11ce-97d5-00aa0055595a           Colour space convertor
  CLSID_Colour  :  TGUID = (D1: $1643e180; D2: $90f5; D3: $11ce;
                            D4: ($97, $d5, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_Colour}

  // 1da08500-9edc-11cf-bc10-00aa00ac74f6           VGA 16 color ditherer
  CLSID_Dither :  TGUID = (D1: $1da08500; D2: $9edc; D3: $11cf;
                           D4: ($bc, $10, $00, $aa, $00, $ac, $74, $f6));
  {$EXTERNALSYM CLSID_Dither}

  // 07167665-5011-11cf-BF33-00AA0055595A           Modex video renderer
  CLSID_ModexRenderer  :  TGUID = (D1: $7167665; D2: $5011; D3: $11cf;
                                   D4: ($bf, $33, $0, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_ModexRenderer}

  // e30629d1-27e5-11ce-875d-00608cb78066           Waveout audio renderer
  CLSID_AudioRender  :  TGUID = (D1: $e30629d1; D2: $27e5; D3: $11ce;
                                 D4: ($87, $5d, $0, $60, $8c, $b7, $80, $66));
  {$EXTERNALSYM CLSID_AudioRender}

  // 05589faf-c356-11ce-bf01-00aa0055595a           Audio Renderer Property Page
  CLSID_AudioProperties  :  TGUID = (D1: $05589faf; D2: $c356; D3: $11ce;
                                     D4: ($bf, $01, $0, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_AudioProperties}

  // 79376820-07D0-11cf-A24D-0020AFD79767           DSound audio renderer
  CLSID_DSoundRender  :  TGUID = (D1: $79376820; D2: $07D0; D3: $11CF;
                                  D4: ($A2, $4D, $0, $20, $AF, $D7, $97, $67));
  {$EXTERNALSYM CLSID_DSoundRender}

  // e30629d2-27e5-11ce-875d-00608cb78066           Wavein audio recorder
  CLSID_AudioRecord  :  TGUID = (D1: $e30629d2; D2: $27e5; D3: $11ce;
                                 D4: ($87, $5d, $00, $60, $8c, $b7, $80, $66));
  {$EXTERNALSYM CLSID_AudioRecord}

  // {2CA8CA52-3C3F-11d2-B73D-00C04FB6BD3D}         IAMAudioInputMixer property page
  CLSID_AudioInputMixerProperties  :  TGUID = (D1: $2ca8ca52; D2: $3c3f; D3: $11d2;
                                               D4: ($b7, $3d, $00, $c0, $4f, $b6, $bd, $3d));
  {$EXTERNALSYM CLSID_AudioInputMixerProperties}

  // {CF49D4E0-1115-11ce-B03A-0020AF0BA770}         AVI Decoder
  CLSID_AVIDec  :  TGUID = (D1: $cf49d4e0; D2: $1115; D3: $11ce;
                            D4: ($b0, $3a, $00, $20, $af, $b, $a7, $70));
  {$EXTERNALSYM CLSID_AVIDec}

  // {A888DF60-1E90-11cf-AC98-00AA004C0FA9}         AVI ICDraw* wrapper
  CLSID_AVIDraw  :  TGUID = (D1: $a888df60; D2: $1e90; D3: $11cf;
                             D4: ($ac, $98, $00, $aa, $0, $4c, $f, $a9));
  {$EXTERNALSYM CLSID_AVIDraw}

  // 6a08cf80-0e18-11cf-a24d-0020afd79767       ACM Wrapper
  CLSID_ACMWrapper  :  TGUID = (D1: $6a08cf80; D2: $0e18; D3: $11cf;
                                D4: ($a2, $4d, $00, $20, $af, $d7, $97, $67));
  {$EXTERNALSYM CLSID_ACMWrapper}

  // {e436ebb5-524f-11ce-9f53-0020af0ba770}    Async File Reader
  CLSID_AsyncReader  :  TGUID = (D1: $e436ebb5; D2: $524f; D3: $11ce;
                                 D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_AsyncReader}

  // {e436ebb6-524f-11ce-9f53-0020af0ba770}    Async URL Reader
  CLSID_URLReader  :  TGUID = (D1: $e436ebb6; D2: $524f; D3: $11ce;
                               D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_URLReader}

  // {e436ebb7-524f-11ce-9f53-0020af0ba770}    IPersistMoniker PID
  CLSID_PersistMonikerPID :  TGUID = (D1: $e436ebb7; D2: $524f; D3: $11ce;
                                     D4: ($9f, $53, $00, $20, $af, $0b, $a7, $70));
  {$EXTERNALSYM CLSID_PersistMonikerPID}

  // {D76E2820-1563-11cf-AC98-00AA004C0FA9}
  CLSID_AVICo  :  TGUID = (D1: $d76e2820; D2: $1563; D3: $11cf;
                           D4: ($ac, $98, $0, $aa, $0, $4c, $f, $a9));
  {$EXTERNALSYM CLSID_AVICo}

  // {8596E5F0-0DA5-11d0-BD21-00A0C911CE86}
  CLSID_FileWriter  :  TGUID = (D1: $8596e5f0; D2: $da5; D3: $11d0;
                                D4: ($bd, $21, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_FileWriter}

  // {E2510970-F137-11CE-8B67-00AA00A3F1A6}     AVI mux filter
  CLSID_AviDest  :  TGUID = (D1: $e2510970; D2: $f137; D3: $11ce;
                             D4: ($8b, $67, $0, $aa, $0, $a3, $f1, $a6));
  {$EXTERNALSYM CLSID_AviDest}

  // {C647B5C0-157C-11d0-BD23-00A0C911CE86}
  CLSID_AviMuxProptyPage  :  TGUID = (D1: $c647b5c0; D2: $157c; D3: $11d0;
                                      D4: ($bd, $23, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_AviMuxProptyPage}

  // {0A9AE910-85C0-11d0-BD42-00A0C911CE86}
  CLSID_AviMuxProptyPage1  :  TGUID = (D1: $a9ae910; D2: $85c0; D3: $11d0;
                                       D4: ($bd, $42, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_AviMuxProptyPage1}

  // {07b65360-c445-11ce-afde-00aa006c14f4}
  CLSID_AVIMIDIRender  :  TGUID = (D1: $07b65360; D2: $c445; D3: $11ce;
                                   D4: ($af, $de, $00, $aa, $00, $6c, $14, $f4));
  {$EXTERNALSYM CLSID_AVIMIDIRender}

  // {187463A0-5BB7-11d3-ACBE-0080C75E246E}    WMSDK-based ASF reader
  CLSID_WMAsfReader  :  TGUID = (D1: $187463a0; D2: $5bb7; D3: $11d3;
                                 D4: ($ac, $be, $0, $80, $c7, $5e, $24, $6e));
  {$EXTERNALSYM CLSID_WMAsfReader}

  // {7c23220e-55bb-11d3-8b16-00c04fb6bd3d}    WMSDK-based ASF writer
  CLSID_WMAsfWriter  :  TGUID = (D1: $7c23220e; D2: $55bb; D3: $11d3;
                                 D4: ($8b, $16, $0, $c0, $4f, $b6, $bd, $3d));
  {$EXTERNALSYM CLSID_WMAsfWriter}

  //  {afb6c280-2c41-11d3-8a60-0000f81e0e4a}
  CLSID_MPEG2Demultiplexer  :  TGUID = (D1: $afb6c280; D2: $2c41; D3: $11d3;
                                        D4: ($8a, $60, $00, $00, $f8, $1e, $0e, $4a));
  {$EXTERNALSYM CLSID_MPEG2Demultiplexer}

  // {687D3367-3644-467a-ADFE-6CD7A85C4A2C}
  CLSID_MPEG2Demultiplexer_NoClock  :  TGUID = (D1: $687d3367; D2: $3644; D3: $467a;
                                                D4: ($ad, $fe, $6c, $d7, $a8, $5c, $4a, $2c));
  {$EXTERNALSYM CLSID_MPEG2Demultiplexer_NoClock}

  // {3ae86b20-7be8-11d1-abe6-00a0c905f375}
  CLSID_MMSPLITTER  :  TGUID = (D1: $3ae86b20; D2: $7be8; D3: $11d1;
                               D4: ($ab, $e6, $00, $a0, $c9, $05, $f3, $75));
  {$EXTERNALSYM CLSID_MMSPLITTER}

  // {2DB47AE5-CF39-43c2-B4D6-0CD8D90946F4}
  CLSID_StreamBufferSink  :  TGUID = (D1: $2db47ae5; D2: $cf39; D3: $43c2;
                                      D4: ($b4, $d6, $c, $d8, $d9, $9, $46, $f4));
  {$EXTERNALSYM CLSID_StreamBufferSink}

  // {E2448508-95DA-4205-9A27-7EC81E723B1A}
  CLSID_SBE2Sink  :  TGUID = (D1: $e2448508; D2: $95da; D3: $4205;
                              D4: ($9a, $27, $7e, $c8, $1e, $72, $3b, $1a));
  {$EXTERNALSYM CLSID_SBE2Sink}

  // {C9F5FE02-F851-4eb5-99EE-AD602AF1E619}
  CLSID_StreamBufferSource  :  TGUID = (D1: $c9f5fe02; D2: $f851; D3: $4eb5;
                                        D4: ($99, $ee, $ad, $60, $2a, $f1, $e6, $19));
  {$EXTERNALSYM CLSID_StreamBufferSource}

  // {FA8A68B2-C864-4ba2-AD53-D3876A87494B}
  CLSID_StreamBufferConfig  :  TGUID = (D1: $fa8a68b2; D2: $c864; D3: $4ba2;
                                        D4: ($ad, $53, $d3, $87, $6a, $87, $49, $4b));
  {$EXTERNALSYM CLSID_StreamBufferConfig}

  // {E37A73F8-FB01-43dc-914E-AAEE76095AB9}
  CLSID_StreamBufferPropertyHandler  :  TGUID = (D1: $e37a73f8; D2: $fb01; D3: $43dc;
                                                 D4: ($91, $4e, $aa, $ee, $76, $9, $5a, $b9));
  {$EXTERNALSYM CLSID_StreamBufferPropertyHandler}

  // {713790EE-5EE1-45ba-8070-A1337D2762FA}
  CLSID_StreamBufferThumbnailHandler  :  TGUID = (D1: $713790ee; D2: $5ee1; D3: $45ba;
                                                  D4: ($80, $70, $a1, $33, $7d, $27, $62, $fa));
  {$EXTERNALSYM CLSID_StreamBufferThumbnailHandler}

  // {6CFAD761-735D-4aa5-8AFC-AF91A7D61EBA}
  CLSID_Mpeg2VideoStreamAnalyzer  :  TGUID = (D1: $6cfad761; D2: $735d; D3: $4aa5;
                                              D4: ($8a, $fc, $af, $91, $a7, $d6, $1e, $ba));
  {$EXTERNALSYM CLSID_Mpeg2VideoStreamAnalyzer}

  // {CCAA63AC-1057-4778-AE92-1206AB9ACEE6}
  CLSID_StreamBufferRecordingAttributes  :  TGUID = (D1: $ccaa63ac; D2: $1057; D3: $4778;
                                                     D4: ($ae, $92, $12, $6, $ab, $9a, $ce, $e6));
  {$EXTERNALSYM CLSID_StreamBufferRecordingAttributes}

  // {D682C4BA-A90A-42fe-B9E1-03109849C423}
  CLSID_StreamBufferComposeRecording  :  TGUID = (D1: $d682c4ba; D2: $a90a; D3: $42fe;
                                                  D4: ($b9, $e1, $3, $10, $98, $49, $c4, $23));
  {$EXTERNALSYM CLSID_StreamBufferComposeRecording}

  // {93A094D7-51E8-485b-904A-8D6B97DC6B39}
  CLSID_SBE2File  :  TGUID = (D1: $93a094d7; D2: $51e8; D3: $485b;
                              D4: ($90, $4a, $8d, $6b, $97, $dc, $6b, $39));
  {$EXTERNALSYM CLSID_SBE2File}

  // {B1B77C00-C3E4-11cf-AF79-00AA00B67A42}               DV video decoder
  CLSID_DVVideoCodec  :  TGUID = (D1: $b1b77c00; D2: $c3e4; D3: $11cf;
                                  D4: ($af, $79, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVVideoCodec}

  // {13AA3650-BB6F-11d0-AFB9-00AA00B67A42}               DV video encoder
  CLSID_DVVideoEnc  :  TGUID = (D1: $13aa3650; D2: $bb6f; D3: $11d0;
                                D4: ($af, $b9, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVVideoEnc}

  // {4EB31670-9FC6-11cf-AF6E-00AA00B67A42}               DV splitter
  CLSID_DVSplitter  :  TGUID = (D1: $4eb31670; D2: $9fc6; D3: $11cf;
                                D4: ($af, $6e, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVSplitter}

  // {129D7E40-C10D-11d0-AFB9-00AA00B67A42}               DV muxer
  CLSID_DVMux  :  TGUID = (D1: $129d7e40; D2: $c10d; D3: $11d0;
                           D4: ($af, $b9, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVMux}

  // {060AF76C-68DD-11d0-8FC1-00C04FD9189D}
  CLSID_SeekingPassThru  :  TGUID = (D1: $60af76c; D2: $68dd; D3: $11d0;
                                     D4: ($8f, $c1, $0, $c0, $4f, $d9, $18, $9d));
  {$EXTERNALSYM CLSID_SeekingPassThru}

  // 6E8D4A20-310C-11d0-B79A-00AA003767A7                 Line21 (CC) Decoder
  CLSID_Line21Decoder  :  TGUID = (D1: $6e8d4a20; D2: $310c; D3: $11d0;
                                   D4: ($b7, $9a, $0, $aa, $0, $37, $67, $a7));
  {$EXTERNALSYM CLSID_Line21Decoder}

  // E4206432-01A1-4BEE-B3E1-3702C8EDC574                 Line21 (CC) Decoder v2
  CLSID_Line21Decoder2  :  TGUID = (D1: $e4206432; D2: $01a1; D3: $4bee;
                                    D4: ($b3, $e1, $37, $02, $c8, $ed, $c5, $74));
  {$EXTERNALSYM CLSID_Line21Decoder2}

  CLSID_CCAFilter  :  TGUID = (D1: $3d07a539; D2: $35ca; D3: $447c;
                               D4: ($9b, $5, $8d, $85, $ce, $92, $4f, $9e));
  {$EXTERNALSYM CLSID_CCAFilter}

  // {CD8743A1-3736-11d0-9E69-00C04FD7C15B}
  CLSID_OverlayMixer  :  TGUID = (D1: $cd8743a1; D2: $3736; D3: $11d0;
                                  D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM CLSID_OverlayMixer}

  // {814B9800-1C88-11d1-BAD9-00609744111A}
  CLSID_VBISurfaces  :  TGUID = (D1: $814b9800; D2: $1c88; D3: $11d1;
                                 D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM CLSID_VBISurfaces}

  // {70BC06E0-5666-11d3-A184-00105AEF9F33}               WST Teletext Decoder
  CLSID_WSTDecoder  :  TGUID = (D1: $70bc06e0; D2: $5666; D3: $11d3;
                                D4: ($a1, $84, $0, $10, $5a, $ef, $9f, $33));
  {$EXTERNALSYM CLSID_WSTDecoder}

  // {301056D0-6DFF-11d2-9EEB-006008039E37}
  CLSID_MjpegDec  :  TGUID = (D1: $301056d0; D2: $6dff; D3: $11d2;
                              D4: ($9e, $eb, $0, $60, $8, $3, $9e, $37));
  {$EXTERNALSYM CLSID_MjpegDec}

  // {B80AB0A0-7416-11d2-9EEB-006008039E37}
  CLSID_MJPGEnc  :  TGUID = (D1: $b80ab0a0; D2: $7416; D3: $11d2;
                             D4: ($9e, $eb, $0, $60, $8, $3, $9e, $37));
  {$EXTERNALSYM CLSID_MJPGEnc}


  // pnp objects and categories


  // 62BE5D10-60EB-11d0-BD3B-00A0C911CE86                 ICreateDevEnum
  CLSID_SystemDeviceEnum  :  TGUID = (D1: $62BE5D10; D2: $60EB; D3: $11d0;
                                      D4: ($BD, $3B, $00, $A0, $C9, $11, $CE, $86));
  {$EXTERNALSYM CLSID_SystemDeviceEnum}

  // 4315D437-5B8C-11d0-BD3B-00A0C911CE86
  CLSID_CDeviceMoniker  :  TGUID = (D1: $4315D437; D2: $5B8C; D3: $11d0;
                                    D4: ($BD, $3B, $00, $A0, $C9, $11, $CE, $86));
  {$EXTERNALSYM CLSID_CDeviceMoniker}

  // 860BB310-5D01-11d0-BD3B-00A0C911CE86                 Video capture category
  CLSID_VideoInputDeviceCategory  :  TGUID = (D1: $860BB310; D2: $5D01; D3: $11d0;
                                              D4: ($BD, $3B, $00, $A0, $C9, $11, $CE, $86));
  {$EXTERNALSYM CLSID_VideoInputDeviceCategory}

   CLSID_CVidCapClassManager  :  TGUID = (D1: $860BB310; D2: $5D01; D3: $11d0;
                                          D4: ($BD, $3B, $00, $A0, $C9, $11, $CE, $86));
   {$EXTERNALSYM CLSID_CVidCapClassManager}

  // 083863F1-70DE-11d0-BD40-00A0C911CE86                 Filter category
  CLSID_LegacyAmFilterCategory  :  TGUID = (D1: $083863F1; D2: $70DE; D3: $11d0;
                                            D4: ($BD, $40, $00, $A0, $C9, $11, $CE, $86));
  {$EXTERNALSYM CLSID_LegacyAmFilterCategory}

  CLSID_CQzFilterClassManager  :  TGUID = (D1: $083863F1; D2: $70DE; D3: $11d0;
                                           D4: ($BD, $40, $00, $A0, $C9, $11, $CE, $86));
  {$EXTERNALSYM CLSID_CQzFilterClassManager}

  // 33D9A760-90C8-11d0-BD43-00A0C911CE86
  CLSID_VideoCompressorCategory  :  TGUID = (D1: $33d9a760; D2: $90c8; D3: $11d0;
                                             D4: ($bd, $43, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_VideoCompressorCategory}

  CLSID_CIcmCoClassManager  :  TGUID = (D1: $33d9a760; D2: $90c8; D3: $11d0;
                                        D4: ($bd, $43, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_CIcmCoClassManager}

  // 33D9A761-90C8-11d0-BD43-00A0C911CE86
  CLSID_AudioCompressorCategory  :  TGUID = (D1: $33d9a761; D2: $90c8; D3: $11d0;
                                             D4: ($bd, $43, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_AudioCompressorCategory}

  CLSID_CAcmCoClassManager  :  TGUID = (D1: $33d9a761; D2: $90c8; D3: $11d0;
                                        D4: ($bd, $43, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_CAcmCoClassManager}

  // 33D9A762-90C8-11d0-BD43-00A0C911CE86                 Audio source cateogry
  CLSID_AudioInputDeviceCategory  :  TGUID = (D1: $33d9a762; D2: $90c8; D3: $11d0;
                                              D4: ($bd, $43, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_AudioInputDeviceCategory}

  CLSID_CWaveinClassManager  :  TGUID = (D1: $33d9a762; D2: $90c8; D3: $11d0;
                                         D4: ($bd, $43, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_CWaveinClassManager}

  // E0F158E1-CB04-11d0-BD4E-00A0C911CE86                 Audio renderer category
  CLSID_AudioRendererCategory  :  TGUID = (D1: $e0f158e1; D2: $cb04; D3: $11d0;
                                           D4: ($bd, $4e, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_AudioRendererCategory}

  CLSID_CWaveOutClassManager  :  TGUID = (D1: $e0f158e1; D2: $cb04; D3: $11d0;
                                          D4: ($bd, $4e, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_CWaveOutClassManager}

  // 4EFE2452-168A-11d1-BC76-00C04FB9453B                 Midi renderer category
  CLSID_MidiRendererCategory  :  TGUID = (D1: $4EfE2452; D2: $168A; D3: $11d1;
                                          D4: ($BC, $76, $0, $c0, $4F, $B9, $45, $3B));
  {$EXTERNALSYM CLSID_MidiRendererCategory}

  CLSID_CMidiOutClassManager  :  TGUID = (D1: $4EfE2452; D2: $168A; D3: $11d1;
                                          D4: ($BC, $76, $0, $c0, $4F, $B9, $45, $3B));
  {$EXTERNALSYM CLSID_CMidiOutClassManager}

  // CC7BFB41-F175-11d1-A392-00E0291F3959     External Renderers Category
  CLSID_TransmitCategory  :  TGUID = (D1: $cc7bfb41; D2: $f175; D3: $11d1;
                                      D4: ($a3, $92, $0, $e0, $29, $1f, $39, $59));
  {$EXTERNALSYM CLSID_TransmitCategory}

  // CC7BFB46-F175-11d1-A392-00E0291F3959     Device Control Filters
  CLSID_DeviceControlCategory  :  TGUID = (D1: $cc7bfb46; D2: $f175; D3: $11d1;
                                           D4: ($a3, $92, $0, $e0, $29, $1f, $39, $59));
  {$EXTERNALSYM CLSID_DeviceControlCategory}

  // DA4E3DA0-D07D-11d0-BD50-00A0C911CE86
  CLSID_ActiveMovieCategories  :  TGUID = (D1: $da4e3da0; D2: $d07d; D3: $11d0;
                                           D4: ($bd, $50, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_ActiveMovieCategories}

  // 2721AE20-7E70-11D0-A5D6-28DB04C10000
  CLSID_DVDHWDecodersCategory  :  TGUID = (D1: $2721AE20; D2: $7E70; D3: $11D0;
                                           D4: ($A5, $D6, $28, $DB, $04, $C1, $00, $00));
  {$EXTERNALSYM CLSID_DVDHWDecodersCategory}

  // 7D22E920-5CA9-4787-8C2B-A6779BD11781     Encoder API encoder category
  CLSID_MediaEncoderCategory  :  TGUID = (D1: $7D22E920; D2: $5CA9; D3: $4787;
                                          D4: ($8C, $2B, $A6, $77, $9B, $D1, $17, $81));
  {$EXTERNALSYM CLSID_MediaEncoderCategory}

  // 236C9559-ADCE-4736-BF72-BAB34E392196     Encoder API multiplexer category
  CLSID_MediaMultiplexerCategory  :  TGUID = (D1: $236C9559; D2: $ADCE; D3: $4736;
                                              D4: ($BF, $72, $BA, $B3, $4E, $39, $21, $96));
   {$EXTERNALSYM CLSID_MediaMultiplexerCategory}

  // CDA42200-BD88-11d0-BD4E-00A0C911CE86
  CLSID_FilterMapper2  :  TGUID = (D1: $cda42200; D2: $bd88; D3: $11d0;
                                   D4: ($bd, $4e, $0, $a0, $c9, $11, $ce, $86));
  {$EXTERNALSYM CLSID_FilterMapper2}

  // 1e651cc0-b199-11d0-8212-00c04fc32c45
  CLSID_MemoryAllocator  :  TGUID = (D1: $1e651cc0; D2: $b199; D3: $11d0;
                                     D4: ($82, $12, $00, $c0, $4f, $c3, $2c, $45));
  {$EXTERNALSYM CLSID_MemoryAllocator}

  // CDBD8D00-C193-11d0-BD4E-00A0C911CE86
  CLSID_MediaPropertyBag  :  TGUID = (D1: $cdbd8d00; D2: $c193; D3: $11d0;
                                      D4: ($bd, $4e, $0, $a0, $c9, $11, $ce, $86));
   {$EXTERNALSYM CLSID_MediaPropertyBag}

  // FCC152B7-F372-11d0-8E00-00C04FD7C08B
  CLSID_DvdGraphBuilder  :  TGUID = (D1: $FCC152B7; D2: $F372; D3: $11d0;
                                     D4: ($8E, $00, $00, $C0, $4F, $D7, $C0, $8B));
  {$EXTERNALSYM CLSID_DvdGraphBuilder}

  // 9B8C4620-2C1A-11d0-8493-00A02438AD48
  CLSID_DVDNavigator  :  TGUID = (D1: $9b8c4620; D2: $2c1a; D3: $11d0;
                                  D4: ($84, $93, $0, $a0, $24, $38, $ad, $48));
  {$EXTERNALSYM CLSID_DVDNavigator}

  // f963c5cf-a659-4a93-9638-caf3cd277d13
  CLSID_DVDState  :  TGUID = (D1: $f963c5cf; D2: $a659; D3: $4a93;
                              D4: ($96, $38, $ca, $f3, $cd, $27, $7d, $13));
  {$EXTERNALSYM CLSID_DVDState}

  // CC58E280-8AA1-11d1-B3F1-00AA003761C5
  CLSID_SmartTee  :  TGUID = (D1: $cc58e280; D2: $8aa1; D3: $11d1;
                              D4: ($b3, $f1, $0, $aa, $0, $37, $61, $c5));
  {$EXTERNALSYM CLSID_SmartTee}

  // FB056BA0-2502-45B9-8E86-2B40DE84AD29
  CLSID_DtvCcFilter  :  TGUID = (D1: $fb056ba0; D2: $2502; D3: $45b9;
                                 D4: ($8e, $86, $2b, $40, $de, $84, $ad, $29));
  {$EXTERNALSYM CLSID_DtvCcFilter}

  // 2F7EE4B6-6FF5-4EB4-B24A-2BFC41117171
  CLSID_CaptionsFilter  :  TGUID = (D1: $2F7EE4B6; D2: $6FF5; D3: $4EB4;
                                   D4: ($B2, $4A, $2B, $FC, $41, $11, $71, $71));
  {$EXTERNALSYM CLSID_CaptionsFilter}

  // {9F22CFEA-CE07-41ab-8BA0-C7364AF90AF9}
  CLSID_SubtitlesFilter  :  TGUID = (D1: $9f22cfea; D2: $ce07; D3: $41ab;
                                     D4: ($8b, $a0, $c7, $36, $4a, $f9, $0a, $f9));
   {$EXTERNALSYM CLSID_SubtitlesFilter}

  // {8670C736-F614-427b-8ADA-BBADC587194B}
  CLSID_DirectShowPluginControl  :  TGUID = (D1: $8670c736; D2: $f614; D3: $427b;
                                             D4: ($8a, $da, $bb, $ad, $c5, $87, $19, $4b));
  {$EXTERNALSYM CLSID_DirectShowPluginControl}

  // -- format types ---

  // 0F6417D6-C318-11D0-A43F-00A0C9223196        FORMAT_None
  FORMAT_None  :  TGUID = (D1: $0F6417D6; D2: $c318; D3: $11d0;
                           D4: ($a4, $3f, $00, $a0, $c9, $22, $31, $96));
  {$EXTERNALSYM FORMAT_None}

  // 05589f80-c356-11ce-bf01-00aa0055595a        FORMAT_VideoInfo
  FORMAT_VideoInfo  :  TGUID = (D1: $05589f80; D2: $c356; D3: $11ce;
                                D4: ($bf, $01, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM FORMAT_VideoInfo}

  // F72A76A0-EB0A-11d0-ACE4-0000C0CC16BA        FORMAT_VideoInfo2
  FORMAT_VideoInfo2  :  TGUID = (D1: $f72a76A0; D2: $eb0a; D3: $11d0;
                                 D4: ($ac, $e4, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM FORMAT_VideoInfo2}

  // 05589f81-c356-11ce-bf01-00aa0055595a        FORMAT_WaveFormatEx
  FORMAT_WaveFormatEx  :  TGUID = (D1: $05589f81; D2: $c356; D3: $11ce;
                                   D4: ($bf, $01, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM FORMAT_WaveFormatEx}

  // 05589f82-c356-11ce-bf01-00aa0055595a        FORMAT_MPEGVideo
  FORMAT_MPEGVideo  :  TGUID = (D1: $05589f82; D2: $c356; D3: $11ce;
                                D4: ($bf, $01, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM FORMAT_MPEGVideo}

  // 05589f83-c356-11ce-bf01-00aa0055595a        FORMAT_MPEGStreams
  FORMAT_MPEGStreams  :  TGUID = (D1: $05589f83; D2: $c356; D3: $11ce;
                                  D4: ($bf, $01, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM FORMAT_MPEGStreams}

  // 05589f84-c356-11ce-bf01-00aa0055595a        FORMAT_DvInfo, DVINFO
  FORMAT_DvInfo  :  TGUID = (D1: $05589f84; D2: $c356; D3: $11ce;
                             D4: ($bf, $01, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM FORMAT_DvInfo}

  // C7ECF04D-4582-4869-9ABB-BFB523B62EDF       FORMAT_525WSS
  FORMAT_525WSS  :  TGUID = (D1: $c7ecf04d; D2: $4582; D3: $4869;
                             D4: ($9a, $bb, $bf, $b5, $23, $b6, $2e, $df));
  {$EXTERNALSYM FORMAT_525WSS}


  // -- Video related GUIDs ---

  // 944d4c00-dd52-11ce-bf0e-00aa0055595a
  CLSID_DirectDrawProperties  :  TGUID = (D1: $944d4c00; D2: $dd52; D3: $11ce;
                                          D4: ($bf, $0e, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_DirectDrawProperties}

  // 59ce6880-acf8-11cf-b56e-0080c7c4b68a
  CLSID_PerformanceProperties  :  TGUID = (D1: $59ce6880; D2: $acf8; D3: $11cf;
                                           D4: ($b5, $6e, $00, $80, $c7, $c4, $b6, $8a));
  {$EXTERNALSYM CLSID_PerformanceProperties}

  // 418afb70-f8b8-11ce-aac6-0020af0b99a3
  CLSID_QualityProperties  :  TGUID = (D1: $418afb70; D2: $f8b8; D3: $11ce;
                                       D4: ($aa, $c6, $00, $20, $af, $0b, $99, $a3));
  {$EXTERNALSYM CLSID_QualityProperties}

  // 61ded640-e912-11ce-a099-00aa00479a58
  IID_IBaseVideoMixer  :  TGUID = (D1: $61ded640; D2: $e912; D3: $11ce;
                                   D4: ($a0, $99, $00, $aa, $00, $47, $9a, $58));
   {$EXTERNALSYM IID_IBaseVideoMixer}

  // 36d39eb0-dd75-11ce-bf0e-00aa0055595a
  IID_IDirectDrawVideo  :  TGUID = (D1: $36d39eb0; D2: $dd75; D3: $11ce;
                                    D4: ($bf, $0e, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM IID_IDirectDrawVideo}

  // bd0ecb0-f8e2-11ce-aac6-0020af0b99a3
  IID_IQualProp  :  TGUID = (D1: $1bd0ecb0; D2: $f8e2; D3: $11ce;
                             D4: ($aa, $c6, $00, $20, $af, $0b, $99, $a3));
  {$EXTERNALSYM IID_IQualProp}

  // {CE292861-FC88-11d0-9E69-00C04FD7C15B}
  CLSID_VPObject  :  TGUID = (D1: $ce292861; D2: $fc88; D3: $11d0;
                              D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM CLSID_VPObject}

  // {CE292862-FC88-11d0-9E69-00C04FD7C15B}
  IID_IVPObject  :  TGUID = (D1: $ce292862; D2: $fc88; D3: $11d0;
                             D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IVPObject}

  // {25DF12C1-3DE0-11d1-9E69-00C04FD7C15B}
  IID_IVPControl  :  TGUID = (D1: $25df12c1; D2: $3de0; D3: $11d1;
                              D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IVPControl}

  // {814B9801-1C88-11d1-BAD9-00609744111A}
  CLSID_VPVBIObject  :  TGUID = (D1: $814b9801; D2: $1c88; D3: $11d1;
                                 D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM CLSID_VPVBIObject}

  // {814B9802-1C88-11d1-BAD9-00609744111A}
  IID_IVPVBIObject  :  TGUID = (D1: $814b9802; D2: $1c88; D3: $11d1;
                                D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM IID_IVPVBIObject}

  // {BC29A660-30E3-11d0-9E69-00C04FD7C15B}
  IID_IVPConfig  :  TGUID = (D1: $bc29a660; D2: $30e3; D3: $11d0;
                             D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IVPConfig}

  // {C76794A1-D6C5-11d0-9E69-00C04FD7C15B}
  IID_IVPNotify  :  TGUID = (D1: $c76794a1; D2: $d6c5; D3: $11d0;
                             D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IVPNotify}

  // {EBF47183-8764-11d1-9E69-00C04FD7C15B}
  IID_IVPNotify2  :  TGUID = (D1: $ebf47183; D2: $8764; D3: $11d1;
                              D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IVPNotify2}

  // {EC529B00-1A1F-11D1-BAD9-00609744111A}
  IID_IVPVBIConfig  :  TGUID = (D1: $ec529b00; D2: $1a1f; D3: $11d1;
                                D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM IID_IVPVBIConfig}

  // {EC529B01-1A1F-11D1-BAD9-00609744111A}
  IID_IVPVBINotify  :  TGUID = (D1: $ec529b01; D2: $1a1f; D3: $11d1;
                                D4: ($ba, $d9, $0, $60, $97, $44, $11, $1a));
  {$EXTERNALSYM IID_IVPVBINotify}

  // {593CDDE1-0759-11d1-9E69-00C04FD7C15B}
  IID_IMixerPinConfig  :  TGUID = (D1: $593cdde1; D2: $759; D3: $11d1;
                                   D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IMixerPinConfig}

  // {EBF47182-8764-11d1-9E69-00C04FD7C15B}
  IID_IMixerPinConfig2  :  TGUID = (D1: $ebf47182; D2: $8764; D3: $11d1;
                                    D4: ($9e, $69, $0, $c0, $4f, $d7, $c1, $5b));
  {$EXTERNALSYM IID_IMixerPinConfig2}

  // This is a real pain in the neck. The OLE GUIDs are separated out into a
  // different file from the main header files. The header files can then be
  // included multiple times and are protected with the following statements,
  //
  //      #ifndef __SOMETHING_DEFINED__
  //      #define __SOMETHING_DEFINED__
  //          all the header contents
  //      #endif   // __SOMETHING_DEFINED__
  //
  // When the actual GUIDs are to be defined (using initguid) the GUID header
  // file can then be included to really define them just once. Unfortunately
  // DirectDraw has the GUIDs defined in the main header file. So if the base
  // classes bring in ddraw.h to get at the DirectDraw structures and so on
  // nobody would then be able to really include ddraw.h to allocate the GUID
  // memory structures because of the aforementioned header file protection
  // Therefore the DirectDraw GUIDs are defined and allocated for real here

  //#ifndef __DDRAW_INCLUDED__
  CLSID_DirectDraw              :  TGUID = (D1: $D7B70EE0; D2: $4340; D3: $11CF;
                                            D4: ($B0, $63, $00, $20, $AF, $C2, $CD, $35));
  {$EXTERNALSYM CLSID_DirectDraw}

  CLSID_DirectDrawClipper       :  TGUID = (D1: $593817A0; D2: $7DB3; D3: $11CF;
                                            D4: ($A2, $DE, $00, $AA, $00, $b9, $33, $56));
  {$EXTERNALSYM CLSID_DirectDrawClipper}

  IID_IDirectDraw               :  TGUID = (D1: $6C14DB80; D2: $A733; D3: $11CE;
                                            D4: ($A5, $21, $00, $20, $AF, $0B, $E5, $60));
  {$EXTERNALSYM IID_IDirectDraw}

  IID_IDirectDraw2              :  TGUID = (D1: $B3A6F3E0; D2: $2B43; D3: $11CF;
                                            D4: ($A2, $DE, $00, $AA, $00, $B9, $33, $56));
  {$EXTERNALSYM IID_IDirectDraw2}

  IID_IDirectDrawSurface        :  TGUID = (D1: $6C14DB81; D2: $A733; D3: $11CE;
                                            D4: ($A5, $21, $00, $20, $AF, $0B, $E5, $60));
  {$EXTERNALSYM IID_IDirectDrawSurface}

  IID_IDirectDrawSurface2       :  TGUID = (D1: $57805885; D2: $6eec; D3: $11cf;
                                            D4: ($94, $41, $a8, $23, $03, $c1, $0e, $27));
  {$EXTERNALSYM IID_IDirectDrawSurface2}

  IID_IDirectDrawSurface3       :  TGUID = (D1: $DA044E00; D2: $69B2; D3: $11D0;
                                            D4: ($A1, $D5, $00, $AA, $00, $B8, $DF, $BB));
  {$EXTERNALSYM IID_IDirectDrawSurface3}

  IID_IDirectDrawSurface4       :  TGUID = (D1: $0B2B8630; D2: $AD35; D3: $11D0;
                                            D4: ($8E, $A6, $00, $60, $97, $97, $EA, $5B));
  {$EXTERNALSYM IID_IDirectDrawSurface4}

  IID_IDirectDrawSurface7       :  TGUID = (D1: $06675a80; D2: $3b9b; D3: $11d2;
                                            D4: ($b9, $2f, $00, $60, $97, $97, $ea, $5b));
  {$EXTERNALSYM IID_IDirectDrawSurface7}

  IID_IDirectDrawPalette        :  TGUID = (D1: $6C14DB84; D2: $A733; D3: $11CE;
                                           D4: ($A5, $21, $00, $20, $AF, $0B, $E5, $60));
  {$EXTERNALSYM IID_IDirectDrawPalette}

  IID_IDirectDrawClipper        :  TGUID = (D1: $6C14DB85; D2: $A733; D3: $11CE;
                                            D4: ($A5, $21, $00, $20, $AF, $0B, $E5, $60));
  {$EXTERNALSYM IID_IDirectDrawClipper}

  IID_IDirectDrawColorControl   :  TGUID = (D1: $4B9F0EE0; D2: $0D7E; D3: $11D0;
                                            D4: ($9B, $06, $00, $A0, $C9, $03, $A3, $B8));
  {$EXTERNALSYM IID_IDirectDrawColorControl}

  //#endif

  //#ifndef __DVP_INCLUDED__
  IID_IDDVideoPortContainer      :  TGUID = (D1: $6C142760; D2: $A733; D3: $11CE;
                                             D4: ($A5, $21, $00, $20, $AF, $0B, $E5, $60));
  {$EXTERNALSYM IID_IDDVideoPortContainer}

  //#endif

  //#ifndef __DDKM_INCLUDED__
  IID_IDirectDrawKernel          :  TGUID = (D1: $8D56C120; D2: $6A08; D3: $11D0;
                                             D4: ($9B, $06, $00, $A0, $C9, $03, $A3, $B8));
  {$EXTERNALSYM IID_IDirectDrawKernel}

  IID_IDirectDrawSurfaceKernel   :  TGUID = (D1: $60755DA0; D2: $6A40; D3: $11D0;
                                             D4: ($9B, $06, $00, $A0, $C9, $03, $A3, $B8));
  {$EXTERNALSYM IID_IDirectDrawSurfaceKernel}

  //#endif

  // 0618aa30-6bc4-11cf-bf36-00aa0055595a
  CLSID_ModexProperties  :  TGUID = (D1: $0618aa30; D2: $6bc4; D3: $11cf;
                                     D4: ($bf, $36, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM CLSID_ModexProperties}

  // dd1d7110-7836-11cf-bf47-00aa0055595a
  IID_IFullScreenVideo :  TGUID = (D1: $dd1d7110; D2: $7836; D3: $11cf;
                                   D4: ($bf, $47, $00, $aa, $00, $55, $59, $5a));
  {$EXTERNALSYM IID_IFullScreenVideo}

  // 53479470-f1dd-11cf-bc42-00aa00ac74f6
  IID_IFullScreenVideoEx  :  TGUID = (D1: $53479470; D2: $f1dd; D3: $11cf;
                                      D4: ($bc, $42, $00, $aa, $00, $ac, $74, $f6));
  {$EXTERNALSYM IID_IFullScreenVideoEx}

  // {101193C0-0BFE-11d0-AF91-00AA00B67A42}           DV decoder property
  CLSID_DVDecPropertiesPage  :  TGUID = (D1: $101193c0; D2: $bfe; D3: $11d0;
                                         D4: ($af, $91, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVDecPropertiesPage}

  // {4150F050-BB6F-11d0-AFB9-00AA00B67A42}           DV encoder property
  CLSID_DVEncPropertiesPage  :  TGUID = (D1: $4150f050; D2: $bb6f; D3: $11d0;
                                         D4: ($af, $b9, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVEncPropertiesPage}

  // {4DB880E0-C10D-11d0-AFB9-00AA00B67A42}           DV Muxer property
  CLSID_DVMuxPropertyPage  :  TGUID = (D1: $4db880e0; D2: $c10d; D3: $11d0;
                                       D4: ($af, $b9, $0, $aa, $0, $b6, $7a, $42));
  {$EXTERNALSYM CLSID_DVMuxPropertyPage}

  // -- Direct Sound Audio related GUID ---

  // 546F4260-D53E-11cf-B3F0-00AA003761C5
  IID_IAMDirectSound  :  TGUID = (D1: $546f4260; D2: $d53e; D3: $11cf;
                                  D4: ($b3, $f0, $0, $aa, $0, $37, $61, $c5));
  {$EXTERNALSYM IID_IAMDirectSound}


 // -- MPEG audio decoder properties

  // {b45dd570-3c77-11d1-abe1-00a0c905f375}
  IID_IMpegAudioDecoder  :  TGUID = (D1: $b45dd570; D2: $3c77; D3: $11d1;
                                     D4: ($ab, $e1, $00, $a0, $c9, $05, $f3, $75));
  {$EXTERNALSYM IID_IMpegAudioDecoder}

  // --- Line21 Decoder interface GUID ---

  // 6E8D4A21-310C-11d0-B79A-00AA003767A7            IID_IAMLine21Decoder
  IID_IAMLine21Decoder  :  TGUID = (D1: $6e8d4a21; D2: $310c; D3: $11d0;
                                    D4: ($b7, $9a, $0, $aa, $0, $37, $67, $a7));
  {$EXTERNALSYM IID_IAMLine21Decoder}

  // --- WST Decoder interface GUID ---

  // C056DE21-75C2-11d3-A184-00105AEF9F33            IID_IAMWstDecoder
  IID_IAMWstDecoder  :  TGUID = (D1: $c056de21; D2: $75c2; D3: $11d3;
                                 D4: ($a1, $84, $0, $10, $5a, $ef, $9f, $33));
  {$EXTERNALSYM IID_IAMWstDecoder}

  // --- WST Decoder Property Page ---

  // 04E27F80-91E4-11d3-A184-00105AEF9F33            WST Decoder Property Page
  CLSID_WstDecoderPropertyPage  :  TGUID = (D1: $4e27f80; D2: $91e4; D3: $11d3;
                                            D4: ($a1, $84, $0, $10, $5a, $ef, $9f, $33));
  {$EXTERNALSYM CLSID_WstDecoderPropertyPage}

  // -- Analog video related GUIDs ---


  // -- format types ---
  // 0482DDE0-7817-11cf-8A03-00AA006ECB65
  FORMAT_AnalogVideo  :  TGUID = (D1: $482dde0; D2: $7817; D3: $11cf;
                                  D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM FORMAT_AnalogVideo}

  // -- major type, Analog Video

  // 0482DDE1-7817-11cf-8A03-00AA006ECB65
  MEDIATYPE_AnalogVideo  :  TGUID = (D1: $482dde1; D2: $7817; D3: $11cf;
                                     D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIATYPE_AnalogVideo}

  // -- Analog Video subtypes, NTSC

  // 0482DDE2-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_NTSC_M  :  TGUID = (D1: $482dde2; D2: $7817; D3: $11cf;
                                               D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_NTSC_M}


  // -- Analog Video subtypes, PAL

  // 0482DDE5-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_B  :  TGUID = (D1: $482dde5; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_B}

  // 0482DDE6-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_D  :  TGUID = (D1: $482dde6; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_D}

  // 0482DDE7-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_G  :  TGUID = (D1: $482dde7; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_G}

  // 0482DDE8-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_H  :  TGUID = (D1: $482dde8; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_H}

  // 0482DDE9-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_I  :  TGUID = (D1: $482dde9; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_I}

  // 0482DDEA-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_M  :  TGUID = (D1: $482ddea; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_M}

  // 0482DDEB-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_N  :  TGUID = (D1: $482ddeb; D2: $7817; D3: $11cf;
                                              D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_N}

  // 0482DDEC-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_PAL_N_COMBO  :  TGUID = (D1: $482ddec; D2: $7817; D3: $11cf;
                                                    D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_PAL_N_COMBO}


  // -- Analog Video subtypes, SECAM

  // 0482DDF0-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_B  :  TGUID = (D1: $482ddf0; D2: $7817; D3: $11cf;
                                                D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_B}

  // 0482DDF1-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_D  :  TGUID = (D1: $482ddf1; D2: $7817; D3: $11cf;
                                                D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_D}

  // 0482DDF2-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_G  :  TGUID = (D1: $482ddf2; D2: $7817; D3: $11cf;
                                                D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_G}

  // 0482DDF3-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_H  :  TGUID = (D1: $482ddf3; D2: $7817; D3: $11cf;
                                                D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_H}

  // 0482DDF4-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_K  :  TGUID = (D1: $482ddf4; D2: $7817; D3: $11cf;
                                                D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_K}

  // 0482DDF5-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_K1  :  TGUID = (D1: $482ddf5; D2: $7817; D3: $11cf;
                                                 D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_K1}

  // 0482DDF6-7817-11cf-8A03-00AA006ECB65
  MEDIASUBTYPE_AnalogVideo_SECAM_L  :  TGUID = (D1: $482ddf6; D2: $7817; D3: $11cf;
                                                D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIASUBTYPE_AnalogVideo_SECAM_L}

  // --  External audio related GUIDs ---

  // -- major types, Analog Audio

  // 0482DEE1-7817-11cf-8a03-00aa006ecb65
  MEDIATYPE_AnalogAudio  :  TGUID = (D1: $482dee1; D2: $7817; D3: $11cf;
                                     D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM MEDIATYPE_AnalogAudio}

  // --  Video analysis related GUIDs ---

  // -- format types used by VA -- H.264, captioning

  // {A4EFC024-873E-4da3-898B-474DDBD79FD0}
  FORMAT_CAPTIONED_H264VIDEO  :  TGUID = (D1: $a4efc024; D2: $873e; D3: $4da3;
                                          D4: ($89, $8b, $47, $4d, $db, $d7, $9f, $d0));
  {$EXTERNALSYM FORMAT_CAPTIONED_H264VIDEO}


  // -- media, media subtype, and format types, CC container

  // {50997A4A-E508-4054-A2B2-10FF0AC1A69A}
  FORMAT_CC_CONTAINER  :  TGUID = (D1: $50997a4a; D2: $e508; D3: $4054;
                                   D4: ($a2, $b2, $10, $ff, $a, $c1, $a6, $9a));
  {$EXTERNALSYM FORMAT_CC_CONTAINER}

  // {3ED9CB31-FD10-4ade-BCCC-FB9105D2F3EF}
  CAPTION_FORMAT_ATSC  :  TGUID = (D1: $3ed9cb31; D2: $fd10; D3: $4ade;
                                   D4: ($bc, $cc, $fb, $91, $5, $d2, $f3, $ef));
  {$EXTERNALSYM CAPTION_FORMAT_ATSC}

  // {12230DB4-FF2A-447e-BB88-6841C416D068}
  CAPTION_FORMAT_DVB  :  TGUID = (D1: $12230db4; D2: $ff2a; D3: $447e;
                                  D4: ($bb, $88, $68, $41, $c4, $16, $d0, $68));
  {$EXTERNALSYM CAPTION_FORMAT_DVB}

  // {E9CA1CE7-915E-47be-9BB9-BF1D8A13A5EC}
  CAPTION_FORMAT_DIRECTV  :  TGUID = (D1: $e9ca1ce7; D2: $915e; D3: $47be;
                                      D4: ($9b, $b9, $bf, $1d, $8a, $13, $a5, $ec));
  {$EXTERNALSYM CAPTION_FORMAT_DIRECTV}

  // {EBB1A262-1158-4b99-AE80-92AC776952C4}
  CAPTION_FORMAT_ECHOSTAR  :  TGUID = (D1: $ebb1a262; D2: $1158; D3: $4b99;
                                       D4: ($ae, $80, $92, $ac, $77, $69, $52, $c4));
  {$EXTERNALSYM CAPTION_FORMAT_ECHOSTAR}

  // -- format types, MPEG-2

  // {7AB2ADA2-81B6-4f14-B3C8-D0C486393B67}
  FORMAT_CAPTIONED_MPEG2VIDEO  :  TGUID = (D1: $7ab2ada2; D2: $81b6; D3: $4f14;
                                           D4: ($b3, $c8, $d0, $c4, $86, $39, $3b, $67));
  {$EXTERNALSYM FORMAT_CAPTIONED_MPEG2VIDEO}


  //
  // DirectShow's include file based on ksmedia.h from WDM DDK
  //
  //#include "ksuuids.h"


  // -- Well known time format GUIDs ---


  // 00000000-0000-0000-0000-000000000000
  TIME_FORMAT_NONE  :  TGUID = (D1: $00000000; D2: $00; D3: $00;
                                D4: ($00, $00, $00, $00, $00, $00, $00, $00));
  {$EXTERNALSYM TIME_FORMAT_NONE}

  // 7b785570-8c82-11cf-bc0c-00aa00ac74f6
  TIME_FORMAT_FRAME  :  TGUID = (D1: $7b785570; D2: $8c82; D3: $11cf;
                                 D4: ($bc, $c, $0, $aa, $0, $ac, $74, $f6));
  {$EXTERNALSYM TIME_FORMAT_FRAME}

  // 7b785571-8c82-11cf-bc0c-00aa00ac74f6
  TIME_FORMAT_BYTE  :  TGUID = (D1: $7b785571; D2: $8c82; D3: $11cf;
                                D4: ($bc, $c, $0, $aa, $0, $ac, $74, $f6));
  {$EXTERNALSYM TIME_FORMAT_BYTE}

  // 7b785572-8c82-11cf-bc0c-00aa00ac74f6
  TIME_FORMAT_SAMPLE  :  TGUID = (D1: $7b785572; D2: $8c82; D3: $11cf;
                                  D4: ($bc, $c, $0, $aa, $0, $ac, $74, $f6));
  {$EXTERNALSYM TIME_FORMAT_SAMPLE}

  // 7b785573-8c82-11cf-bc0c-00aa00ac74f6
  TIME_FORMAT_FIELD :  TGUID = (D1: $7b785573; D2: $8c82; D3: $11cf;
                                D4: ($bc, $c, $0, $aa, $0, $ac, $74, $f6));
  {$EXTERNALSYM TIME_FORMAT_FIELD}

  // 7b785574-8c82-11cf-bc0c-00aa00ac74f6
  TIME_FORMAT_MEDIA_TIME  :  TGUID = (D1: $7b785574; D2: $8c82; D3: $11cf;
                                      D4: ($bc, $c, $0, $aa, $0, $ac, $74, $f6));
   {$EXTERNALSYM TIME_FORMAT_MEDIA_TIME}

  // for IKsPropertySet

  // 9B00F101-1567-11d1-B3F1-00AA003761C5
  AMPROPSETID_Pin  :  TGUID = (D1: $9b00f101; D2: $1567; D3: $11d1;
                               D4: ($b3, $f1, $0, $aa, $0, $37, $61, $c5));
  {$EXTERNALSYM AMPROPSETID_Pin}

  // fb6c4281-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_CAPTURE  :  TGUID = (D1: $fb6c4281; D2: $0353; D3: $11d1;
                                    D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_CAPTURE}

  // fb6c4282-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_PREVIEW  :  TGUID = (D1: $fb6c4282; D2: $0353; D3: $11d1;
                                    D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_PREVIEW}

  // fb6c4283-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_ANALOGVIDEOIN  :  TGUID = (D1: $fb6c4283; D2: $0353; D3: $11d1;
                                          D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_ANALOGVIDEOIN}

  // fb6c4284-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_VBI  :  TGUID = (D1: $fb6c4284; D2: $0353; D3: $11d1;
                                D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_VBI}

  // fb6c4285-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_VIDEOPORT  :  TGUID = (D1: $fb6c4285; D2: $0353; D3: $11d1;
                                      D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_VIDEOPORT}

  // fb6c4286-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_NABTS  :  TGUID = (D1: $fb6c4286; D2: $0353; D3: $11d1;
                                  D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_NABTS}

  // fb6c4287-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_EDS  :  TGUID = (D1: $fb6c4287; D2: $0353; D3: $11d1;
                                D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_EDS}

  // fb6c4288-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_TELETEXT  :  TGUID = (D1: $fb6c4288; D2: $0353; D3: $11d1;
                                     D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_TELETEXT}

  // fb6c4289-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_CC  :  TGUID = (D1: $fb6c4289; D2: $0353; D3: $11d1;
                               D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_CC}

  // fb6c428a-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_STILL  :  TGUID = (D1: $fb6c428a; D2: $0353; D3: $11d1;
                                  D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_STILL}

  // fb6c428b-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_TIMECODE  :  TGUID = (D1: $fb6c428b; D2: $0353; D3: $11d1;
                                     D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_TIMECODE}

  // fb6c428c-0353-11d1-905f-0000c0cc16ba
  PIN_CATEGORY_VIDEOPORT_VBI  :  TGUID = (D1: $fb6c428c; D2: $0353; D3: $11d1;
                                          D4: ($90, $5f, $00, $00, $c0, $cc, $16, $ba));
  {$EXTERNALSYM PIN_CATEGORY_VIDEOPORT_VBI}

  // the following special GUIDS are used by ICaptureGraphBuilder::FindInterface

  // {AC798BE0-98E3-11d1-B3F1-00AA003761C5}
  LOOK_UPSTREAM_ONLY  :  TGUID = (D1: $ac798be0; D2: $98e3; D3: $11d1;
                                  D4: ($b3, $f1, $00, $aa, $00, $37, $61, $c5));
  {$EXTERNALSYM LOOK_UPSTREAM_ONLY}

  // {AC798BE1-98E3-11d1-B3F1-00AA003761C5}
  LOOK_DOWNSTREAM_ONLY  :  TGUID = (D1: $ac798be1; D2: $98e3; D3: $11d1;
                                    D4: ($b3, $f1, $0, $aa, $0, $37, $61, $c5));
  {$EXTERNALSYM LOOK_DOWNSTREAM_ONLY}

  // -------------------------------------------------------------------------
  // KSProxy GUIDS
  // -------------------------------------------------------------------------

  // {266EEE41-6C63-11cf-8A03-00AA006ECB65}
  CLSID_TVTunerFilterPropertyPage  :  TGUID = (D1: $266eee41; D2: $6c63; D3: $11cf;
                                               D4: ($8a, $3, $0, $aa, $0, $6e, $cb, $65));
  {$EXTERNALSYM CLSID_TVTunerFilterPropertyPage}

  // {71F96461-78F3-11d0-A18C-00A0C9118956}
  CLSID_CrossbarFilterPropertyPage  :  TGUID = (D1: $71f96461; D2: $78f3; D3: $11d0;
                                                D4: ($a1, $8c, $0, $a0, $c9, $11, $89, $56));
  {$EXTERNALSYM CLSID_CrossbarFilterPropertyPage}

  // {71F96463-78F3-11d0-A18C-00A0C9118956}
  CLSID_TVAudioFilterPropertyPage  :  TGUID = (D1: $71f96463; D2: $78f3; D3: $11d0;
                                               D4: ($a1, $8c, $0, $a0, $c9, $11, $89, $56));
  {$EXTERNALSYM CLSID_TVAudioFilterPropertyPage}

  // {71F96464-78F3-11d0-A18C-00A0C9118956}
  CLSID_VideoProcAmpPropertyPage  :  TGUID = (D1: $71f96464; D2: $78f3; D3: $11d0;
                                              D4: ($a1, $8c, $0, $a0, $c9, $11, $89, $56));
  {$EXTERNALSYM CLSID_VideoProcAmpPropertyPage}

  // {71F96465-78F3-11d0-A18C-00A0C9118956}
  CLSID_CameraControlPropertyPage  :  TGUID = (D1: $71f96465; D2: $78f3; D3: $11d0;
                                               D4: ($a1, $8c, $0, $a0, $c9, $11, $89, $56));
  {$EXTERNALSYM CLSID_CameraControlPropertyPage}

  // {71F96466-78F3-11d0-A18C-00A0C9118956}
  CLSID_AnalogVideoDecoderPropertyPage  :  TGUID = (D1: $71f96466; D2: $78f3; D3: $11d0;
                                                   D4: ($a1, $8c, $0, $a0, $c9, $11, $89, $56));
  {$EXTERNALSYM CLSID_AnalogVideoDecoderPropertyPage}

  // {71F96467-78F3-11d0-A18C-00A0C9118956}
  CLSID_VideoStreamConfigPropertyPage  :  TGUID = (D1: $71f96467; D2: $78f3; D3: $11d0;
                                                   D4: ($a1, $8c, $0, $a0, $c9, $11, $89, $56));
  {$EXTERNALSYM CLSID_VideoStreamConfigPropertyPage}

  // {37E92A92-D9AA-11d2-BF84-8EF2B1555AED} Audio Renderer Advanced Property Page
  CLSID_AudioRendererAdvancedProperties  :  TGUID = (D1: $37e92a92; D2: $d9aa; D3: $11d2;
                                                     D4: ($bf, $84, $8e, $f2, $b1, $55, $5a, $ed));
   {$EXTERNALSYM CLSID_AudioRendererAdvancedProperties}

  // -------------------------------------------------------------------------
  // VMR GUIDS
  // -------------------------------------------------------------------------

  // {B87BEB7B-8D29-423f-AE4D-6582C10175AC}
  CLSID_VideoMixingRenderer  :  TGUID = (D1: $B87BEB7B; D2: $8D29; D3: $423f;
                                         D4: ($AE, $4D, $65, $82, $C1, $01, $75, $AC));
  {$EXTERNALSYM CLSID_VideoMixingRenderer}

  // {6BC1CFFA-8FC1-4261-AC22-CFB4CC38DB50}
  CLSID_VideoRendererDefault  :  TGUID = (D1: $6BC1CFFA; D2: $8FC1; D3: $4261;
                                          D4: ($AC, $22, $CF, $B4, $CC, $38, $DB, $50));
   {$EXTERNALSYM CLSID_VideoRendererDefault}

  // {99d54f63-1a69-41ae-aa4d-c976eb3f0713}
  CLSID_AllocPresenter  :  TGUID = (D1: $99d54f63; D2: $1a69; D3: $41ae;
                                    D4: ($aa, $4d, $c9, $76, $eb, $3f, $07, $13));
  {$EXTERNALSYM CLSID_AllocPresenter}

  // {4444ac9e-242e-471b-a3c7-45dcd46352bc}
  CLSID_AllocPresenterDDXclMode  :  TGUID = (D1: $4444ac9e; D2: $242e; D3: $471b;
                                             D4: ($a3, $c7, $45, $dc, $d4, $63, $52, $bc));
  {$EXTERNALSYM CLSID_AllocPresenterDDXclMode}

  // {6f26a6cd-967b-47fd-874a-7aed2c9d25a2}
  CLSID_VideoPortManager  :  TGUID = (D1: $6f26a6cd; D2: $967b; D3: $47fd;
                                      D4: ($87, $4a, $7a, $ed, $2c, $9d, $25, $a2));
  {$EXTERNALSYM CLSID_VideoPortManager}

  // -------------------------------------------------------------------------
  // VMR GUIDS for DX9
  // -------------------------------------------------------------------------

  // {51b4abf3-748f-4e3b-a276-c828330e926a}
  CLSID_VideoMixingRenderer9  :  TGUID = (D1: $51b4abf3; D2: $748f; D3: $4e3b;
                                          D4: ($a2, $76, $c8, $28, $33, $0e, $92, $6a));
  {$EXTERNALSYM CLSID_VideoMixingRenderer9}

  // -------------------------------------------------------------------------
  // EVR GUIDS
  // -------------------------------------------------------------------------

  // {FA10746C-9B63-4b6c-BC49-FC300EA5F256}
  CLSID_EnhancedVideoRenderer  :  TGUID = (D1: $fa10746c; D2: $9b63; D3: $4b6c;
                                           D4: ($bc, $49, $fc, $30, $0e, $a5, $f2, $56));
  {$EXTERNALSYM CLSID_EnhancedVideoRenderer}

  // {E474E05A-AB65-4f6a-827C-218B1BAAF31F}
  CLSID_MFVideoMixer9  :  TGUID = (D1: $E474E05A; D2: $AB65; D3: $4f6a;
                                   D4: ($82, $7C, $21, $8B, $1B, $AA, $F3, $1F));
  {$EXTERNALSYM CLSID_MFVideoMixer9}

  // {98455561-5136-4d28-AB08-4CEE40EA2781}
  CLSID_MFVideoPresenter9  :  TGUID = (D1: $98455561; D2: $5136; D3: $4d28;
                                       D4: ($ab, $08, $4c, $ee, $40, $ea, $27, $81));
  {$EXTERNALSYM CLSID_MFVideoPresenter9}

  // {a0a7a57b-59b2-4919-a694-add0a526c373}
  CLSID_EVRTearlessWindowPresenter9  :  TGUID = (D1: $a0a7a57b; D2: $59b2; D3: $4919;
                                                 D4: ($a6, $94, $ad, $d0, $a5, $26, $c3, $73));
  {$EXTERNALSYM CLSID_EVRTearlessWindowPresenter9}

  // {62079164-233b-41f8-a80f-f01705f514a8}
  CLSID_EVRPlaybackPipelineOptimizer  :  TGUID = (D1: $62079164; D2: $233b; D3: $41f8;
                                                  D4: ($a8, $0f, $f0, $17, $05, $f5, $14, $a8));
   {$EXTERNALSYM CLSID_EVRPlaybackPipelineOptimizer}

   // {e447df01-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df02-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df03-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df04-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df05-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df06-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df07-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df08-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df09-10ca-4d17-b17e-6a840f8a3a4c}
   // {e447df0a-10ca-4d17-b17e-6a840f8a3a4c}
   EVRConfig_ForceBob                  :  TGUID = (D1: $e447df01; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_ForceBob}

   EVRConfig_AllowDropToBob            :  TGUID = (D1: $e447df02; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_AllowDropToBob}

   EVRConfig_ForceThrottle             :  TGUID = (D1: $e447df03; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_ForceThrottle}

   EVRConfig_AllowDropToThrottle       :  TGUID = (D1: $e447df04; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_AllowDropToThrottle}

   EVRConfig_ForceHalfInterlace        :  TGUID = (D1: $e447df05; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_ForceHalfInterlace}

   EVRConfig_AllowDropToHalfInterlace  :  TGUID = (D1: $e447df06; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_AllowDropToHalfInterlace}

   EVRConfig_ForceScaling              :  TGUID = (D1: $e447df07; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_ForceScaling}

   EVRConfig_AllowScaling              :  TGUID = (D1: $e447df08; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
    {$EXTERNALSYM EVRConfig_AllowScaling}

   EVRConfig_ForceBatching             :  TGUID = (D1: $e447df09; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_ForceBatching}

   EVRConfig_AllowBatching             :  TGUID = (D1: $e447df0a; D2: $10ca; D3: $4d17;
                                                   D4: ($b1, $7e, $6a, $84, $0f, $8a, $3a, $4c));
   {$EXTERNALSYM EVRConfig_AllowBatching}


  // -------------------------------------------------------------------------
  // BDA Network Provider GUIDS
  // -------------------------------------------------------------------------

  // This is the GUID for the generic NP which would replace ATSC, DVBT, DVBS
  // and DVBC NP. All the other GUIDs are still kept for backward compatibility
  // {B2F3A67C-29DA-4c78-8831-091ED509A475}
  CLSID_NetworkProvider  :  TGUID = (D1: $b2f3a67c; D2: $29da; D3: $4c78;
                                     D4: ($88, $31, $9, $1e, $d5, $9, $a4, $75));
  {$EXTERNALSYM CLSID_NetworkProvider}

  // {0DAD2FDD-5FD7-11D3-8F50-00C04F7971E2}
  CLSID_ATSCNetworkProvider  :  TGUID = (D1: $0dad2fdd; D2: $5fd7; D3: $11d3;
                                         D4: ($8f, $50, $00, $c0, $4f, $79, $71, $e2));
  {$EXTERNALSYM CLSID_ATSCNetworkProvider}

  // {E3444D16-5AC4-4386-88DF-13FD230E1DDA}
  CLSID_ATSCNetworkPropertyPage  :  TGUID = (D1: $e3444d16; D2: $5ac4; D3: $4386;
                                             D4: ($88, $df, $13, $fd, $23, $0e, $1d, $da));
  {$EXTERNALSYM CLSID_ATSCNetworkPropertyPage}

  // {FA4B375A-45B4-4d45-8440-263957B11623}
  CLSID_DVBSNetworkProvider  :  TGUID = (D1: $fa4b375a; D2: $45b4; D3: $4d45;
                                         D4: ($84, $40, $26, $39, $57, $b1, $16, $23));
  {$EXTERNALSYM CLSID_DVBSNetworkProvider}

  // {216C62DF-6D7F-4e9a-8571-05F14EDB766A}
  CLSID_DVBTNetworkProvider  :  TGUID = (D1: $216c62df; D2: $6d7f; D3: $4e9a;
                                         D4: ($85, $71, $5, $f1, $4e, $db, $76, $6a));
  {$EXTERNALSYM CLSID_DVBTNetworkProvider}

  // {DC0C0FE7-0485-4266-B93F-68FBF80ED834}
  CLSID_DVBCNetworkProvider  :  TGUID = (D1: $dc0c0fe7; D2: $485; D3: $4266;
                                         D4: ($b9, $3f, $68, $fb, $f8, $e, $d8, $34));
  {$EXTERNALSYM CLSID_DVBCNetworkProvider}


  // -------------------------------------------------------------------------
  // attribute GUIDs
  // -------------------------------------------------------------------------

  // {EB7836CA-14FF-4919-BCE7-3AF12319E50C}
  DSATTRIB_UDCRTag  :  TGUID = (D1: $EB7836CA; D2: $14FF; D3: $4919;
                               D4: ($bc, $e7, $3a, $f1, $23, $19, $e5, $0c));
  {$EXTERNALSYM DSATTRIB_UDCRTag}

  // {2F5BAE02-7B8F-4f60-82D6-E4EA2F1F4C99}
  DSATTRIB_PicSampleSeq  :  TGUID = (D1: $2f5bae02; D2: $7b8f; D3: $4f60;
                                     D4: ($82, $d6, $e4, $ea, $2f, $1f, $4c, $99));
  {$EXTERNALSYM DSATTRIB_PicSampleSeq}

  // {5A5F08CA-55C2-4033-92AB-55DB8F781226}
  DSATTRIB_OptionalVideoAttributes  :  TGUID = (D1: $5A5F08CA; D2: $55C2; D3: $4033;
                                               D4: ($92, $AB, $55, $DB, $8F, $78, $12, $26));
  {$EXTERNALSYM DSATTRIB_OptionalVideoAttributes}

  // {e7e050fb-dd5d-40dd-9915-35dcb81bdc8a}
  DSATTRIB_CC_CONTAINER_INFO  :  TGUID = (D1: $e7e050fb; D2: $dd5d; D3: $40dd;
                                         D4: ($99, $15, $35, $DC, $B8, $1B, $DC, $8a));
  {$EXTERNALSYM DSATTRIB_CC_CONTAINER_INFO}

  // {B622F612-47AD-4671-AD6C-05A98E65DE3A}
  DSATTRIB_TRANSPORT_PROPERTIES  :  TGUID = (D1: $b622f612; D2: $47ad; D3: $4671;
                                             D4: ($ad, $6c, $05, $a9, $8e, $65, $de, $3a));
  {$EXTERNALSYM DSATTRIB_TRANSPORT_PROPERTIES}

  // {e0b56679-12b9-43cc-b7df-578caa5a7b63}
  DSATTRIB_PBDATAG_ATTRIBUTE  :  TGUID = (D1: $e0b56679; D2: $12b9; D3: $43cc;
                                         D4: ($b7, $df, $57, $8c, $aa, $5a, $7b, $63));
  {$EXTERNALSYM DSATTRIB_PBDATAG_ATTRIBUTE}

  // {0c1a5614-30cd-4f40-bcbf-d03e52306207}
  DSATTRIB_CAPTURE_STREAMTIME  :  TGUID = (D1: $0c1a5614; D2: $30cd; D3: $4f40;
                                           D4: ($bc, $bf, $d0, $3e, $52, $30, $62, $07));
   {$EXTERNALSYM DSATTRIB_CAPTURE_STREAMTIME}

  // {5FB5673B-0A2A-4565-827B-6853FD75E611}               DSATTRIB_DSHOW_STREAM_DESC
  DSATTRIB_DSHOW_STREAM_DESC  :  TGUID = (D1: $5fb5673b; D2: $a2a; D3: $4565;
                                         D4: ($82, $7b, $68, $53, $fd, $75, $e6, $11));
  {$EXTERNALSYM DSATTRIB_DSHOW_STREAM_DESC}

  // {892CD111-72F3-411d-8B91-A9E9123AC29A}
  DSATTRIB_SAMPLE_LIVE_STREAM_TIME  :  TGUID = (D1: $892cd111; D2: $72f3; D3: $411d;
                                               D4: ($8b, $91, $a9, $e9, $12, $3a, $c2, $9a));
  {$EXTERNALSYM DSATTRIB_SAMPLE_LIVE_STREAM_TIME}

  // UUID for supported UDRI TAG tables
  UUID_UdriTagTables  :  TGUID = (D1: $e1b98d74; D2: $9778; D3: $4878;
                                 D4: ($b6, $64, $eb, $20, $20, $36, $4d, $88));
  {$EXTERNALSYM UUID_UdriTagTables}

  // UUID for supported WMDRM TAG tables
  UUID_WMDRMTagTables  :  TGUID = (D1: $5DCD1101; D2: $9263; D3: $45bb;
                                   D4: ($a4, $d5, $c4, $15, $ab, $8c, $58, $9c));
  {$EXTERNALSYM UUID_WMDRMTagTables}


  // -------------------------------------------------------------------------
  // TVE Receiver filter guids
  // -------------------------------------------------------------------------

  // The CLSID used by the TVE Receiver filter
  // {05500280-FAA5-4DF9-8246-BFC23AC5CEA8}
  CLSID_DShowTVEFilter  :  TGUID = (D1: $05500280; D2: $FAA5; D3: $4DF9;
                                   D4: ($82, $46, $BF, $C2, $3A, $C5, $CE, $A8));
  {$EXTERNALSYM CLSID_DShowTVEFilter}

  // {05500281-FAA5-4DF9-8246-BFC23AC5CEA8}
  CLSID_TVEFilterTuneProperties  :  TGUID = (D1: $05500281; D2: $FAA5; D3: $4DF9;
                                             D4: ($82, $46, $BF, $C2, $3A, $C5, $CE, $A8));
  {$EXTERNALSYM CLSID_TVEFilterTuneProperties}

  // {05500282-FAA5-4DF9-8246-BFC23AC5CEA8}
  CLSID_TVEFilterCCProperties  :  TGUID = (D1: $05500282; D2: $FAA5; D3: $4DF9;
                                           D4: ($82, $46, $BF, $C2, $3A, $C5, $CE, $A8));
  {$EXTERNALSYM CLSID_TVEFilterCCProperties}

  // {05500283-FAA5-4DF9-8246-BFC23AC5CEA8}
  CLSID_TVEFilterStatsProperties  :  TGUID = (D1: $05500283; D2: $FAA5; D3: $4DF9;
                                              D4: ($82, $46, $BF, $C2, $3A, $C5, $CE, $A8));
  {$EXTERNALSYM CLSID_TVEFilterStatsProperties}


  // -------------------------------------------------------------------------
  // Defined ENCAPI parameter GUIDs
  // -------------------------------------------------------------------------

  // The CLSID for the original IVideoEncoder proxy plug-in
  // {B43C4EEC-8C32-4791-9102-508ADA5EE8E7}
  CLSID_IVideoEncoderProxy  :  TGUID = (D1: $b43c4eec; D2: $8c32; D3: $4791;
                                        D4: ($91, $2, $50, $8a, $da, $5e, $e8, $e7));
  {$EXTERNALSYM CLSID_IVideoEncoderProxy}

  // The CLSID for the ICodecAPI proxy plug-in
  // {7ff0997a-1999-4286-a73c-622b8814e7eb}
  CLSID_ICodecAPIProxy  :  TGUID = (D1: $7ff0997a; D2: $1999; D3: $4286;
                                    D4: ($a7, $3c, $62, $2b, $88, $14, $e7, $eb));
  {$EXTERNALSYM CLSID_ICodecAPIProxy}

  // The CLSID for the combination ICodecAPI/IVideoEncoder proxy plug-in
  // {b05dabd9-56e5-4fdc-afa4-8a47e91f1c9c}
  CLSID_IVideoEncoderCodecAPIProxy  :  TGUID = (D1: $b05dabd9; D2: $56e5; D3: $4fdc;
                                                D4: ($af, $a4, $8a, $47, $e9, $1f, $1c, $9c));
  {$EXTERNALSYM CLSID_IVideoEncoderCodecAPIProxy}


//#ifndef __ENCODER_API_GUIDS__
//#define __ENCODER_API_GUIDS__

  // {49CC4C43-CA83-4ad4-A9AF-F3696AF666DF}
  ENCAPIPARAM_BITRATE  :  TGUID = (D1: $49cc4c43; D2: $ca83; D3: $4ad4;
                                   D4: ($a9, $af, $f3, $69, $6a, $f6, $66, $df));
  {$EXTERNALSYM ENCAPIPARAM_BITRATE}

  // {703F16A9-3D48-44a1-B077-018DFF915D19}
  ENCAPIPARAM_PEAK_BITRATE  :  TGUID = (D1: $703f16a9; D2: $3d48; D3: $44a1;
                                        D4: ($b0, $77, $1, $8d, $ff, $91, $5d, $19));
  {$EXTERNALSYM ENCAPIPARAM_PEAK_BITRATE}

  // {EE5FB25C-C713-40d1-9D58-C0D7241E250F}
  ENCAPIPARAM_BITRATE_MODE  :  TGUID = (D1: $ee5fb25c; D2: $c713; D3: $40d1;
                                        D4: ($9d, $58, $c0, $d7, $24, $1e, $25, $f));
  {$EXTERNALSYM ENCAPIPARAM_BITRATE_MODE}

  // {0C0171DB-FEFC-4af7-9991-A5657C191CD1}
  ENCAPIPARAM_SAP_MODE  :  TGUID = (D1: $c0171db; D2: $fefc; D3: $4af7;
                                    D4: ($99, $91, $a5, $65, $7c, $19, $1c, $d1));
   {$EXTERNALSYM ENCAPIPARAM_SAP_MODE}


  // for kernel control

  // {62b12acf-f6b0-47d9-9456-96f22c4e0b9d}
  CODECAPI_CHANGELISTS  :  TGUID = (D1: $62b12acf; D2: $f6b0; D3: $47d9;
                                    D4: ($94, $56, $96, $f2, $2c, $4e, $0b, $9d));
  {$EXTERNALSYM CODECAPI_CHANGELISTS}

  // {7112e8e1-3d03-47ef-8e60-03f1cf537301 }
  CODECAPI_VIDEO_ENCODER  :  TGUID = (D1: $7112e8e1; D2: $3d03; D3: $47ef;
                                      D4: ($8e, $60, $03, $f1, $cf, $53, $73, $01));
  {$EXTERNALSYM CODECAPI_VIDEO_ENCODER}

  // {b9d19a3e-f897-429c-bc46-8138b7272b2d }
  CODECAPI_AUDIO_ENCODER  :  TGUID = (D1: $b9d19a3e; D2: $f897; D3: $429c;
                                      D4: ($bc, $46, $81, $38, $b7, $27, $2b, $2d));
  {$EXTERNALSYM CODECAPI_AUDIO_ENCODER}

  // {6c5e6a7c-acf8-4f55-a999-1a628109051b }
  CODECAPI_SETALLDEFAULTS  :  TGUID = (D1: $6c5e6a7c; D2: $acf8; D3: $4f55;
                                       D4: ($a9, $99, $1a, $62, $81, $09, $05, $1b));
  {$EXTERNALSYM CODECAPI_SETALLDEFAULTS}

  // {6a577e92-83e1-4113-adc2-4fcec32f83a1 }
  CODECAPI_ALLSETTINGS  :  TGUID = (D1: $6a577e92; D2: $83e1; D3: $4113;
                                    D4: ($ad, $c2, $4f, $ce, $c3, $2f, $83, $a1));
  {$EXTERNALSYM CODECAPI_ALLSETTINGS}

  // {0581af97-7693-4dbd-9dca-3f9ebd6585a1 }
  CODECAPI_SUPPORTSEVENTS  :  TGUID = (D1: $0581af97; D2: $7693; D3: $4dbd;
                                       D4: ($9d, $ca, $3f, $9e, $bd, $65, $85, $a1));
  {$EXTERNALSYM CODECAPI_SUPPORTSEVENTS}


  // {1cb14e83-7d72-4657-83fd-47a2c5b9d13d }
  CODECAPI_CURRENTCHANGELIST :  TGUID = (D1: $1cb14e83; D2: $7d72; D3: $4657;
                                         D4: ($83, $fd, $47, $a2, $c5, $b9, $d1, $3d));
  {$EXTERNALSYM CODECAPI_CURRENTCHANGELIST}

  // {1f26a602-2b5c-4b63-b8e8-9ea5c1a7dc2e}
  CLSID_SBE2MediaTypeProfile :  TGUID = (D1: $1f26a602; D2: $2b5c; D3: $4b63;
                                         D4: ($b8, $e8, $9e, $a5, $c1, $a7, $dc, $2e));
  {$EXTERNALSYM CLSID_SBE2MediaTypeProfile}

  // {3E458037-0CA6-41aa-A594-2AA6C02D709B}
  CLSID_SBE2FileScan :  TGUID = (D1: $3e458037; D2: $ca6; D3: $41aa;
                                 D4: ($a5, $94, $2a, $a6, $c0, $2d, $70, $9b));
   {$EXTERNALSYM CLSID_SBE2FileScan}



  // When generating strmiids.lib, include codecapi definitions
//#ifdef INITGUID
//#define UUID_GEN
//#include <codecapi.h>
//#endif

//#endif   // __ENCODER_API_GUIDS__

  // -----------------------------------------------
  // Used for decoders that exposing ICodecAPI
  // -----------------------------------------------
  CODECAPI_AVDecMmcssClass :  TGUID = (D1: $e0ad4828; D2: $df66; D3: $4893;
                                       D4: ($9f, $33, $78, $8a, $a4, $ec, $40, $82));
  {$EXTERNALSYM CODECAPI_AVDecMmcssClass}


//#undef OUR_GUID_ENTRY

  // Additional prototypes for ALL interfaces

  // End of Aadditional prototypes

implementation

  // Implement additional prototypes here.

end.

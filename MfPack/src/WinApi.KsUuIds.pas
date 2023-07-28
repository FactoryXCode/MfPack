// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.KsUuIds.pas
// Kind: Pascal / Delphi unit
// Release date: 27-07-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
//
//          Contains the GUIDs for the MediaType type, subtype fields and format
//          types for DVD/MPEG2 media types.
//
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: ksuuids.h
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.KsUuIds;

interface

uses
  WinApi.Windows;

const

  //
  // --- MPEG 2 definitions ---
  //

  // 36523B13-8EE5-11d1-8CA3-0060B057664A
  MEDIATYPE_MPEG2_PACK  : TGUID = (D1: $36523B13;
                                   D2: $8EE5;
                                   D3: $11d1;
                                   D4: ($8C, $A3, $00, $60, $B0, $57, $66, $4A));

  // e06d8020-db46-11cf-b4d1-00805f6cbbea
  MEDIATYPE_MPEG2_PES : TGUID = (D1: $e06d8020;
                                 D2: $db46;
                                 D3: $11cf;
                                 D4: ($b4, $d1, $00, $80, $5f, $6c, $bb, $ea));


  MEDIATYPE_CONTROL : TGUID = (D1: $e06d8021;
                               D2: $db46;
                               D3: $11cf;
                               D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // #if ( (NTDDI_VERSION >= NTDDI_WINXPSP2) && (NTDDI_VERSION < NTDDI_WS03) ) || (NTDDI_VERSION >= NTDDI_WS03SP1)

  MEDIATYPE_MPEG2_SECTIONS : TGUID = (D1: $455f176c;
                                      D2: $4b06;
                                      D3: $47ce;
                                      D4: ($9a, $ef, $8c, $ae, $f7, $3d, $f7, $b5));

  // {1ED988B0-3FFC-4523-8725-347BEEC1A8A0}
  MEDIASUBTYPE_MPEG2_VERSIONED_TABLES : TGUID = (D1: $1ed988b0;
                                                 D2: $3ffc;
                                                 D3: $4523;
                                                 D4: ($87, $25, $34, $7b, $ee, $c1, $a8, $a0));

  MEDIASUBTYPE_ATSC_SI : TGUID = (D1: $b3c7397c;
                                  D2: $d303;
                                  D3: $414d;
                                  D4: ($b3, $3c, $4e, $d2, $c9, $d2, $97, $33));

  MEDIASUBTYPE_DVB_SI : TGUID = (D1: $e9dd31a3;
                                 D2: $221d;
                                 D3: $4adb;
                                 D4: ($85, $32, $9a, $f3, $09, $c1, $a4, $08));

  MEDIASUBTYPE_ISDB_SI : TGUID = (D1: $e89ad298;
                                  D2: $3601;
                                  D3: $4b06;
                                  D4: ($aa, $ec, $9d, $de, $ed, $cc, $5b, $d0));

  // {EC232EB2-CB96-4191-B226-0EA129F38250}
  MEDIASUBTYPE_TIF_SI : TGUID = (D1: $ec232eb2;
                                 D2: $cb96;
                                 D3: $4191;
                                 D4: ($b2, $26, $0e, $a1, $29, $f3, $82, $50));

  // {C892E55B-252D-42b5-A316-D997E7A5D995}
  MEDIASUBTYPE_MPEG2DATA : TGUID = (D1: $c892e55b;
                                    D2: $252d;
                                    D3: $42b5;
                                    D4: ($a3, $16, $d9, $97, $e7, $a5, $d9, $95));
  // #endif


  MEDIASUBTYPE_MPEG2_WMDRM_TRANSPORT : TGUID = (D1: $18BEC4EA;
                                                D2: $4676;
                                                D3: $450e;
                                                D4: ($B4, $78, $0C, $D8, $4C, $54, $B3, $27));

  // e06d8026-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_MPEG2_VIDEO : TGUID = (D1: $e06d8026;
                                      D2: $db46;
                                      D3: $11cf;
                                      D4: ($b4, $d1, $00, $80, $5f, $6c, $bb, $ea));

  // use MPEG2VIDEOINFO (defined below) with FORMAT_MPEG2_VIDEO
  // e06d80e3-db46-11cf-b4d1-00805f6cbbea
  FORMAT_MPEG2_VIDEO : TGUID = (D1: $e06d80e3;
                                D2: $db46;
                                D3: $11cf;
                                D4: ($b4, $d1, $00, $80, $5f, $6c, $bb, $ea));

  // F72A76A0-EB0A-11d0-ACE4-0000C0CC16BA       (FORMAT_VideoInfo2)
  FORMAT_VIDEOINFO2 : TGUID = (D1: $f72a76A0;
                               D2: $eb0a;
                               D3: $11d0;
                               D4: ($ac, $e4, $00, $0, $c0, $cc, $16, $ba));

  // MPEG2 Other subtypes
  // e06d8022-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_MPEG2_PROGRAM : TGUID = (D1: $e06d8022;
                                        D2: $db46;
                                        D3: $11cf;
                                        D4: ($b4, $d1, $00, $80, $05, $6c, $bb, $ea));

  // e06d8023-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_MPEG2_TRANSPORT : TGUID = (D1: $e06d8023;
                                          D2: $db46;
                                          D3: $11cf;
                                          D4: ($b4, $d1, $00, $80, $05, $6c, $bb, $ea));

  // #if (NTDDI_VERSION >= NTDDI_WINXP)

  // 138AA9A4-1EE2-4c5b-988E-19ABFDBC8A11
  MEDIASUBTYPE_MPEG2_TRANSPORT_STRIDE : TGUID = (D1: $138aa9a4;
                                                 D2: $1ee2;
                                                 D3: $4c5b;
                                                 D4: ($98, $8e, $19, $ab, $fd, $bc, $8a, $11));

  // {18BEC4EA-4676-450e-B478-0CD84C54B327}
  MEDIASUBTYPE_MPEG2_UDCR_TRANSPORT : TGUID = (D1: $18BEC4EA;
                                               D2: $4676;
                                               D3: $450e;
                                               D4: ($B4, $78, $0C, $D8, $4C, $54, $B3, $27));

  // {0d7aed42-cb9a-11db-9705-005056c00008}
   MEDIASUBTYPE_MPEG2_PBDA_TRANSPORT_RAW : TGUID = (D1: $0d7aed42;
                                                    D2: $cb9a;
                                                    D3: $11db;
                                                    D4: ($97, $05, $00, $50, $56, $c0, $00, $08));

  // {af748dd4-d800-11db-9705-005056c00008}
  MEDIASUBTYPE_MPEG2_PBDA_TRANSPORT_PROCESSED : TGUID = (D1: $af748dd4;
                                                         D2: $d80;
                                                         D3: $11db;
                                                         D4: ($97, $05, $00, $50, $56, $c0, $00, $08));

  // #endif


  // e06d802b-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_MPEG2_AUDIO : TGUID = (D1: $e06d802b;
                                      D2: $db46;
                                      D3: $11c;
                                      D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d802c-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_DOLBY_AC3 : TGUID = (D1: $e06d802c;
                                    D2: $db46;
                                    D3: $11c;
                                    D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d802d-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_DVD_SUBPICTURE : TGUID = (D1: $e06d802d;
                                         D2: $db46;
                                         D3: $11c;
                                         D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d8032-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_DVD_LPCM_AUDIO : TGUID = (D1: $e06d8032;
                                         D2: $db46;
                                         D3: $11c;
                                         D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // #if (NTDDI_VERSION >= NTDDI_WINXP)

  // e06d8033-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_DTS : TGUID = (D1: $e06d8033;
                              D2: $db46;
                              D3: $11c;
                              D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d8034-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_SDDS : TGUID = (D1: $e06d8034;
                               D2: $db46;
                               D3: $11c;
                               D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // #endif


  // DVD-related mediatypes
  // ======================

  // ED0B916A-044D-11d1-AA78-00C04FC31D60
  MEDIATYPE_DVD_ENCRYPTED_PACK : TGUID = (D1: $ed0b916a;
                                          D2: $044d;
                                          D3: $11d1;
                                          D4: ($aa, $78, $00, $c0, $04, $c3, $1d, $60));

  // e06d802e-db46-11cf-b4d1-00805f6cbbea
  MEDIATYPE_DVD_NAVIGATION : TGUID = (D1: $e06d802e;
                                      D2: $db46;
                                      D3: $11cf;
                                      D4: ($b4, $d1, $00, $80, $05, $6c, $bb, $ea));

  // e06d802f-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_DVD_NAVIGATION_PCI : TGUID = (D1: $e06d802f;
                                             D2: $db46;
                                             D3: $11cf;
                                             D4: ($b4, $d1, $00, $80, $05, $6c, $bb, $ea));

  MEDIASUBTYPE_DVD_NAVIGATION_DSI : TGUID = (D1: $e06d8030;
                                             D2: $db46;
                                             D3: $11cf;
                                             D4: ($b4, $d1, $00, $80, $05, $6c, $bb, $ea));

  // e06d8031-db46-11cf-b4d1-00805f6cbbea
  MEDIASUBTYPE_DVD_NAVIGATION_PROVIDER : TGUID = (D1: $e06d8031;
                                                  D2: $db46;
                                                  D3: $11c;
                                                  D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // DVD - MPEG2/AC3-related Formats
  // ===============================

  // e06d80e3-db46-11cf-b4d1-00805f6cbbea
  FORMAT_MPEG2Vide : TGUID = (D1: $e06d80e3;
                              D2: $db46;
                              D3: $11c;
                              D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d80e4-db46-11cf-b4d1-00805f6cbbea
  FORMAT_DolbyAC3 : TGUID = (D1: $e06d80e4;
                             D2: $db46;
                             D3: $11c;
                             D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d80e5-db46-11cf-b4d1-00805f6cbbea
  FORMAT_MPEG2Audio : TGUID = (D1: $e06d80e5;
                               D2: $db46;
                               D3: $11c;
                               D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // e06d80e6-db46-11cf-b4d1-00805f6cbbea
  FORMAT_DVD_LPCMAudio : TGUID = (D1: $e06d80e6;
                                  D2: $db46;
                                  D3: $11c;
                                  D4: ($b4, $d1, $00, $80, $05f, $6c, $bb, $ea));

  // UVC 1.2 H.264 Video Format
  // ==========================

  // 2017be05-6629-4248-aaed-7e1a47bc9b9c
  FORMAT_UVCH264Video : TGUID = (D1: $2017be05;
                                 D2: $6629;
                                 D3: $4248;
                                 D4: ($aa, $ed, $7e, $1a, $47, $bc, $9b, $9c));

  // JPEG Image Format
  // =================

  // 692fa379-d3e8-4651-b5b4-0b94b013eeaf
  FORMAT_JPEGImage : TGUID = (D1: $692fa379;
                              D2: $d3e8;
                              D3: $4651;
                              D4: ($b5, $b4, $0b, $94, $b0, $13, $ee, $af));

  // Image Format
  // ============

  // 692fa379-d3e8-4651-b5b4-0b94b013eeaf
  FORMAT_Image : TGUID = (D1: $692fa379;
                          D2: $d3e8;
                          D3: $4651;
                          D4: ($b5, $b4, $0b, $94, $b0, $13, $ee, $af));


  // KS Property Set Id (to communicate with the WDM Proxy filter) -- from
  // ksmedia.h of WDM DDK.
  // =====================================================================

  // BFABE720-6E1F-11D0-BCF2-444553540000
  AM_KSPROPSETID_AC3 : TGUID = (D1: $BFABE720;
                                D2: $6E1F;
                                D3: $11D0;
                                D4: ($BC, $F2, $44, $45, $53, $54, $00, $00));

  // ac390460-43af-11d0-bd6a-003505c103a9
  AM_KSPROPSETID_DvdSubPic : TGUID = (D1: $ac390460;
                                      D2: $43af;
                                      D3: $11d0;
                                      D4: ($bd, $6a, $00, $35, $05, $c1, $03, $a9));

  // 0E8A0A40L-6AEF-11D0-9ED0-00A024CA19B3
  AM_KSPROPSETID_CopyProt : TGUID = (D1: $0E8A0A40;
                                     D2: $6AEF;
                                     D3: $11D0;
                                     D4: ($9E, $D0, $00, $A0, $24, $CA, $19, $B3));

  // A503C5C0-1D1D-11d1-AD80-444553540000
  AM_KSPROPSETID_TSRateChange : TGUID = (D1: $a503c5c0;
                                         D2: $1d1d;
                                         D3: $11d1;
                                         D4: ($ad, $80, $44, $45, $53, $54, $00, $00));

  // #if (NTDDI_VERSION >= NTDDI_WINXP)

  // 3577EB09-9582-477f-B29C-B0C452A4FF9A
  AM_KSPROPSETID_DVD_RateChange : TGUID = (D1: $3577eb09;
                                           D2: $9582;
                                           D3: $477f;
                                           D4: ($b2, $9c, $b0, $c4, $52, $a4, $ff, $9a));

  // ae4720ae-aa71-42d8-b82a-fffdf58b76fd
  AM_KSPROPSETID_DvdKaraoke : TGUID = (D1: $ae4720ae;
                                       D2: $aa71;
                                       D3: $42d8;
                                       D4: ($b8, $2a, $ff, $fd, $f5, $8b, $76, $fd));

  // c830acbd-ab07-492f-8852-45b6987c2979
  AM_KSPROPSETID_FrameStep : TGUID = (D1: $c830acbd;
                                      D2: $ab07;
                                      D3: $492f;
                                      D4: ($88, $52, $45, $b6, $98, $7c, $29, $79));

  // #endif

  // -----------------------------------------------
  // MPEG4 related KSPROPSETIDs from ksmedia.h of WDK
  // -----------------------------------------------

  // FF6C4BFA-07A9-4c7b-A237-672F9D68065F
  AM_KSPROPSETID_MPEG4_MediaType_Attributes : TGUID = (D1: $ff6c4bfa;
                                                       D2: $07a9;
                                                       D3: $4c7b;
                                                       D4: ($a2, $37, $67, $2f, $9d, $68, $06, $5f));


  // KS categories from ks.h and ksmedia.h
  // =====================================

  // 65E8773D-8F56-11D0-A3B9-00A0C9223196
  AM_KSCATEGORY_CAPTURE : TGUID = (D1: $65E8773D;
                                   D2: $8F56;
                                   D3: $11D0;
                                   D4: ($A3, $B9, $00, $A0, $C9, $22, $31, $96));

  // 65E8773E-8F56-11D0-A3B9-00A0C9223196
  AM_KSCATEGORY_RENDER : TGUID = (D1: $65E8773E;
                                  D2: $8F56;
                                  D3: $11D0;
                                  D4: ($A3, $B9, $00, $A0, $C9, $22, $31, $96));

  // 1E84C900-7E70-11D0-A5D6-28DB04C10000
  AM_KSCATEGORY_DATACOMPRESSOR : TGUID = (D1: $1E84C900;
                                          D2: $7E70;
                                          D3: $11D0;
                                          D4: ($A5, $D6, $28, $DB, $04, $C1, $00, $00));

  // 6994AD04-93EF-11D0-A3CC-00A0C9223196
  AM_KSCATEGORY_AUDIO : TGUID = (D1: $6994AD04;
                                 D2: $93EF;
                                 D3: $11D0;
                                 D4: ($A3, $CC, $00, $A0, $C9, $22, $31, $96));

  // 6994AD05-93EF-11D0-A3CC-00A0C9223196
  AM_KSCATEGORY_VIDEO : TGUID = (D1: $6994AD05;
                                 D2: $93EF;
                                 D3: $11D0;
                                 D4: ($A3, $CC, $00, $A0, $C9, $22, $31, $96));

  // a799a800-a46d-11d0-a18c-00a02401dcd4
  AM_KSCATEGORY_TVTUNER : TGUID = (D1: $a799a800;
                                   D2: $a46d;
                                   D3: $11d0;
                                   D4: ($a1, $8c, $00, $a0, $24, $01, $dc, $d4));

  // a799a801-a46d-11d0-a18c-00a02401dcd4
  AM_KSCATEGORY_CROSSBAR : TGUID = (D1: $a799a801;
                                    D2: $a46d;
                                    D3: $11d0;
                                    D4: ($a1, $8c, $00, $a0, $24, $01, $dc, $d4));

  // a799a802-a46d-11d0-a18c-00a02401dcd4
  AM_KSCATEGORY_TVAUDIO : TGUID = (D1: $a799a802;
                                   D2: $a46d;
                                   D3: $11d0;
                                   D4: ($a1, $8c, $00, $a0, $24, $01, $dc, $d4));


  // 07dad660L-22f1-11d1-a9f4-00c04fbbde8f
  AM_KSCATEGORY_VBICODEC : TGUID = (D1: $07dad660;
                                    D2: $22f1;
                                    D3: $11d1;
                                    D4: ($a9, $f4, $00, $c0, $4f, $bb, $de, $8f));

  // #if (NTDDI_VERSION >= NTDDI_WS03SP1)

  // multi-instance safe codec categories(kernel or user mode)
  // =========================================================

  // {9C24A977-0951-451a-8006-0E49BD28CD5F}
  AM_KSCATEGORY_VBICODEC_MI : TGUID = (D1: $9c24a977;
                                       D2: $0951;
                                       D3: $451a;
                                       D4: ($80, $06, $0e, $49, $bd, $28, $cd, $5f));
  // #endif

  // 0A4252A0L-7E70-11D0-A5D6-28DB04C10000
  AM_KSCATEGORY_SPLITTER : TGUID = (D1: $0A4252A0;
                                    D2: $7E70;
                                    D3: $11D0;
                                    D4: ($A5, $D6, $28, $DB, $04, $C1, $00, $00));


  // GUIDs needed to support IKsPin interface
  // ========================================

  // d3abc7e0l-9a61-11d0-a40d00a0c9223196
  IID_IKsInterfaceHandler : TGUID = (D1: $D3ABC7E0;
                                     D2: $9A61;
                                     D3: $11D0;
                                     D4: ($A4, $0D, $00, $A0, $C9, $22, $31, $96));

  // 5ffbaa02l-49a3-11d0-9f3600aa00a216a1
  IID_IKsDataTypeHandler : TGUID = (D1: $5FFBAA02;
                                    D2: $49A3;
                                    D3: $11D0;
                                    D4: ($9F, $36, $00, $AA, $00, $A2, $16, $A1));

  // b61178d1-a2d9-11cf-9e53-00aa00a216a1
  IID_IKsPin : TGUID = (D1: $b61178d1;
                        D2: $a2d9;
                        D3: $11cf;
                        D4: ($9e, $53, $00, $aa, $00, $a2, $16, $a1));

  // 28F54685-06FD-11D2-B27A-00A0C9223196
  IID_IKsControl : TGUID = (D1: $28F54685;
                            D2: $06FD;
                            D3: $11D2;
                            D4: ($B2, $7A, $00, $A0, $C9, $22, $31, $96));

  // CD5EBE6B-8B6E-11D1-8AE0-00A0C9223196
  IID_IKsPinFactory : TGUID = (D1: $CD5EBE6B;
                               D2: $8B6E;
                               D3: $11D1;
                               D4: ($8A, $E0, $00, $A0, $C9, $22, $31, $96));

  // 1A8766A0-62CE-11CF-A5D6-28DB04C10000
  AM_INTERFACESETID_Standard : TGUID = (D1: $1A8766A0;
                                        D2: $62CE;
                                        D3: $11CF;
                                        D4: ($A5, $D6, $28, $DB, $04, $C1, $00, $00));


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.

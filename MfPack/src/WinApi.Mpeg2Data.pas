// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Mpeg2Data.pas
// Kind: Pascal / Delphi unit
// Release date: 15-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Main Mpeg2Data Library Definition, and interface definitions for
//              the MPEG-2 Section and Table acquisition functionality
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (TopPlay)
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
// Source: mfapi.h
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
unit WinApi.Mpeg2Data;

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  WinApi.WinApiTypes,
  WinApi.MPeg2Structs;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const
  CLSID_Mpeg2TableFilter : TGUID = '{752845F1-758F-4c83-A043-4270C593308E}';
  {$EXTERNALSYM CLSID_Mpeg2TableFilter}
  LIBID_Mpeg2DataLib     : TGUID = '{DBAF6C1B-B6A4-4898-AE65-204F0D9509A1}';
  {$EXTERNALSYM LIBID_Mpeg2DataLib}
  CLSID_SectionList      : TGUID = '{73DA5D04-4347-45d3-A9DC-FAE9DDBE558D}';
  {$EXTERNALSYM CLSID_SectionList}
  CLSID_Mpeg2Stream      : TGUID = '{F91D96C7-8509-4d0b-AB26-A0DD10904BB7}';
  {$EXTERNALSYM CLSID_Mpeg2Stream}
  CLSID_Mpeg2Data        : TGUID = '{C666E115-BB62-4027-A113-82D643FE2D99}';
  {$EXTERNALSYM CLSID_Mpeg2Data}


// pragma(1)  We don't, because Win64 requires an 8-bit alignment.
//            For Win32 however it should be 1. See above.

const

  MPEG_PAT_PID                        = $0000;
  {$EXTERNALSYM MPEG_PAT_PID}
  MPEG_PAT_TID                        = $0;
  {$EXTERNALSYM MPEG_PAT_TID}
  MPEG_CAT_PID                        = $0001;
  {$EXTERNALSYM MPEG_CAT_PID}
  MPEG_CAT_TID                        = $01;
  {$EXTERNALSYM MPEG_CAT_TID}
  MPEG_PMT_TID                        = $02;
  {$EXTERNALSYM MPEG_PMT_TID}
  MPEG_TSDT_PID                       = $0002;
  {$EXTERNALSYM MPEG_TSDT_PID}
  MPEG_TSDT_TID                       = $03;
  {$EXTERNALSYM MPEG_TSDT_TID}
  ATSC_MGT_PID                        = $1FFB;
  {$EXTERNALSYM ATSC_MGT_PID}
  ATSC_MGT_TID                        = $C7;
  {$EXTERNALSYM ATSC_MGT_TID}
  ATSC_VCT_PID                        = $1FFB;
  {$EXTERNALSYM ATSC_VCT_PID}
  ATSC_VCT_TERR_TID                   = $C8;
  {$EXTERNALSYM ATSC_VCT_TERR_TID}
  ATSC_VCT_CABL_TID                   = $C9;
  {$EXTERNALSYM ATSC_VCT_CABL_TID}
  ATSC_EIT_TID                        = $CB;
  {$EXTERNALSYM ATSC_EIT_TID}
  ATSC_ETT_TID                        = $CC;
  {$EXTERNALSYM ATSC_ETT_TID}
  ATSC_RRT_TID                        = $CA;
  {$EXTERNALSYM ATSC_RRT_TID}
  ATSC_RRT_PID                        = $1FFB;
  {$EXTERNALSYM ATSC_RRT_PID}
  ATSC_STT_PID                        = $1FFB;
  {$EXTERNALSYM ATSC_STT_PID}
  ATSC_STT_TID                        = $CD;
  {$EXTERNALSYM ATSC_STT_TID}
  ATSC_PIT_TID                        = $D0;
  {$EXTERNALSYM ATSC_PIT_TID}
  DVB_NIT_PID                         = $0010;
  {$EXTERNALSYM DVB_NIT_PID}
  DVB_NIT_ACTUAL_TID                  = $40;
  {$EXTERNALSYM DVB_NIT_ACTUAL_TID}
  DVB_NIT_OTHER_TID                   = $41;
  {$EXTERNALSYM DVB_NIT_OTHER_TID}
  DVB_SDT_PID                         = $0011;
  {$EXTERNALSYM DVB_SDT_PID}
  DVB_SDT_ACTUAL_TID                  = $42;
  {$EXTERNALSYM DVB_SDT_ACTUAL_TID}
  DVB_SDT_OTHER_TID                   = $46;
  {$EXTERNALSYM DVB_SDT_OTHER_TID}
  DVB_BAT_PID                         = $0011;
  {$EXTERNALSYM DVB_BAT_PID}
  DVB_BAT_TID                         = $4A;
  {$EXTERNALSYM DVB_BAT_TID}
  DVB_EIT_PID                         = $0012;
  {$EXTERNALSYM DVB_EIT_PID}
  DVB_EIT_ACTUAL_TID                  = $4E;
  {$EXTERNALSYM DVB_EIT_ACTUAL_TID}
  DVB_EIT_OTHER_TID                   = $4F;
  {$EXTERNALSYM DVB_EIT_OTHER_TID}
  DVB_RST_PID                         = $0013;
  {$EXTERNALSYM DVB_RST_PID}
  DVB_RST_TID                         = $71;
  {$EXTERNALSYM DVB_RST_TID}
  DVB_TDT_PID                         = $0014;
  {$EXTERNALSYM DVB_TDT_PID}
  DVB_TDT_TID                         = $70;
  {$EXTERNALSYM DVB_TDT_TID}
  DVB_ST_PID_16                       = $0010;
  {$EXTERNALSYM DVB_ST_PID_16}
  DVB_ST_PID_17                       = $0011;
  {$EXTERNALSYM DVB_ST_PID_17}
  DVB_ST_PID_18                       = $0012;
  {$EXTERNALSYM DVB_ST_PID_18}
  DVB_ST_PID_19                       = $0013;
  {$EXTERNALSYM DVB_ST_PID_19}
  DVB_ST_PID_20                       = $0014;
  {$EXTERNALSYM DVB_ST_PID_20}
  DVB_ST_TID                          = $72;
  {$EXTERNALSYM DVB_ST_TID}
  ISDB_ST_TID                         = $72;
  {$EXTERNALSYM ISDB_ST_TID}
  DVB_TOT_PID                         = $0014;
  {$EXTERNALSYM DVB_TOT_PID}
  DVB_TOT_TID                         = $73;
  {$EXTERNALSYM DVB_TOT_TID}
  DVB_DIT_PID                         = $001E;
  {$EXTERNALSYM DVB_DIT_PID}
  DVB_DIT_TID                         = $7E;
  {$EXTERNALSYM DVB_DIT_TID}
  DVB_SIT_PID                         = $001F;
  {$EXTERNALSYM DVB_SIT_PID}
  DVB_SIT_TID                         = $7F;
  {$EXTERNALSYM DVB_SIT_TID}
  ISDB_EMM_TID                        = $85;
  {$EXTERNALSYM ISDB_EMM_TID}
  ISDB_BIT_PID                        = $0024;
  {$EXTERNALSYM ISDB_BIT_PID}
  ISDB_BIT_TID                        = $C4;
  {$EXTERNALSYM ISDB_BIT_TID}
  ISDB_NBIT_PID                       = $0025;
  {$EXTERNALSYM ISDB_NBIT_PID}
  ISDB_NBIT_MSG_TID                   = $C5;
  {$EXTERNALSYM ISDB_NBIT_MSG_TID}
  ISDB_NBIT_REF_TID                   = $C6;
  {$EXTERNALSYM ISDB_NBIT_REF_TID}
  ISDB_LDT_PID                        = $0025;
  {$EXTERNALSYM ISDB_LDT_PID}
  ISDB_LDT_TID                        = $C7;
  {$EXTERNALSYM ISDB_LDT_TID}
  ISDB_SDTT_PID                       = $0023;
  {$EXTERNALSYM ISDB_SDTT_PID}
  ISDB_SDTT_ALT_PID                   = $0028;
  {$EXTERNALSYM ISDB_SDTT_ALT_PID}
  ISDB_SDTT_TID                       = $C3;
  {$EXTERNALSYM ISDB_SDTT_TID}
  ISDB_CDT_PID                        = $0029;
  {$EXTERNALSYM ISDB_CDT_PID}
  ISDB_CDT_TID                        = $C8;
  {$EXTERNALSYM ISDB_CDT_TID}
  SCTE_EAS_TID                        = $D8;
  {$EXTERNALSYM SCTE_EAS_TID}
  SCTE_EAS_IB_PID                     = $1FFB;
  {$EXTERNALSYM SCTE_EAS_IB_PID}
  SCTE_EAS_OOB_PID                    = $1FFC;
  {$EXTERNALSYM SCTE_EAS_OOB_PID}


  // Forwarded interface declarations

type

  ISectionList      = interface;
  {$EXTERNALSYM ISectionList}
  IMpeg2Stream      = interface;
  {$EXTERNALSYM IMpeg2Stream}


  // Interface IMpeg2TableFilter
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMpeg2TableFilter);'}
  {$EXTERNALSYM IMpeg2TableFilter}
  IMpeg2TableFilter = interface(IUnknown)
  ['{BDCDD913-9ECD-4fb2-81AE-ADF747EA75A5}']
    function AddPID(p: PID): HResult; stdcall;

    function AddTable(p: PID;
                      t: TID): HResult; stdcall;

    function AddExtension(p: PID;
                          t: TID;
                          e: TEID): HResult; stdcall;

    function RemovePID(p: PID): HResult; stdcall;

    function RemoveTable(p: PID;
                         t: TID): HResult; stdcall;

    function RemoveExtension(p: PID;
                             t: TID;
                             e: TEID): HResult; stdcall;

  end;
  IID_IMpeg2TableFilter = IMpeg2TableFilter;
  {$EXTERNALSYM IID_IMpeg2TableFilter}


  PMpeg2TableSampleHdr = ^Mpeg2TableSampleHdr;
  {$EXTERNALSYM Mpeg2TableSampleHdr}
  Mpeg2TableSampleHdr = record
    SectionCount: Byte;
    Reserved: array[0..2] of Byte;
    SectionOffsets: array[0..0] of LONG;
  end;
  {$EXTERNALSYM Mpeg2TableSampleHdr}

  // Interface IMpeg2TableFilter
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMpeg2Data);'}
  {$EXTERNALSYM IMpeg2Data}
  IMpeg2Data = interface(IUnknown)
  ['{9B396D40-F380-4e3c-A514-1A82BF6EBFE6}']
    function GetSection(pid: PID;
                        tid: TID;
                        pFilter: PMPEG2_FILTER;
                        dwTimeout: DWORD;
                        out ppSectionList: ISectionList): HResult; stdcall;

    function GetTable(pid: PID;
                      tid: TID;
                      pFilter: PMPEG2_FILTER;
                      dwTimeout: DWORD;
                      out ppSectionList: ISectionList): HResult; stdcall;

    function GetStreamOfSections(pid: PID;
                                 tid: TID;
                                 pFilter: PMPEG2_FILTER;
                                 hDataReadyEvent: THANDLE;
                                 out ppMpegStream: IMpeg2Stream): HResult; stdcall;

  end;
  IID_IMpeg2Data = IMpeg2Data;
  {$EXTERNALSYM IID_IMpeg2Data}


  // Interface ISectionList
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISectionList);'}
  {$EXTERNALSYM ISectionList}
  ISectionList = interface(IUnknown)
  ['{AFEC1EB5-2A64-46c6-BF4B-AE3CCB6AFDB0}']
    function Initialize(requestType: MPEG_REQUEST_TYPE;
                        pMpeg2Data: IMpeg2Data;
                        pContext: PMPEG_CONTEXT;
                        pid: PID;
                        tid: TID; pFilter: PMPEG2_FILTER;
                        timeout: DWORD;
                        hDoneEvent: THANDLE): HResult; stdcall;

    function InitializeWithRawSections(pmplSections: PMPEG_PACKET_LIST): HResult; stdcall;

    function CancelPendingRequest(): HResult; stdcall;

    function GetNumberOfSections(out pCount: word): HResult; stdcall;

    function GetSectionData(sectionNumber: word;
                            out pdwRawPacketLength: DWORD;
                            out ppSection: PSECTION): HResult; stdcall;

    function GetProgramIdentifier(pPid: PPID): HResult; stdcall;

    function GetTableIdentifier(pTableId: PTID): HResult; stdcall;

  end;
  IID_ISectionList = ISectionList;
  {$EXTERNALSYM IID_ISectionList}


  // Interface IMpeg2Stream
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMpeg2Stream);'}
  {$EXTERNALSYM IMpeg2Stream}
  IMpeg2Stream = interface(IUnknown)
  ['{400CC286-32A0-4ce4-9041-39571125A635}']
    function Initialize(requestType: MPEG_REQUEST_TYPE;
                        pMpeg2Data: IMpeg2Data;
                        pContext: PMPEG_CONTEXT;
                        pid: PID;
                        tid: TID;
                        pFilter: PMPEG2_FILTER;
                        hDataReadyEvent: THandle): HResult; stdcall;

    function SupplyDataBuffer(pStreamBuffer: PMPEG_STREAM_BUFFER): HResult; stdcall;

  end;
  IID_IMpeg2Stream = IMpeg2Stream;
  {$EXTERNALSYM IID_IMpeg2Stream}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

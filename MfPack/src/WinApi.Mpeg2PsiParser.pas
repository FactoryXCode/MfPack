// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Mpeg2PsiParser.pas
// Kind: Pascal / Delphi unit
// Release date: 16-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Interface definitions for the MPEG-2 PSI parsing functionality.
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
// Source: Mpeg2PsiParser.h
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
unit WinApi.Mpeg2PsiParser;

interface

uses
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.mpeg2structs,
  WinApi.mpeg2data;


  // Interfaces.

type

  // Interface IGenericDescriptor
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGenericDescriptor);'}
  {$EXTERNALSYM IGenericDescriptor}
  PIGenericDescriptor = ^IGenericDescriptor;
  {$EXTERNALSYM PIGenericDescriptor}
  IGenericDescriptor = interface(IUnknown)
  ['{6A5918F8-A77A-4f61-AED0-5702BDCDA3E6}']
    function Initialize(pbDesc: PByte;
                        bCount: INT): HRESULT; stdcall;

    function GetTag(out pbVal: PByte): HRESULT; stdcall;

    function GetLength(out pbVal: PByte): HRESULT; stdcall;

    function GetBody(out ppbVal: PByte): HRESULT; stdcall;

  end;
  IID_IGenericDescriptor = IGenericDescriptor;
  {$EXTERNALSYM IID_IGenericDescriptor}


  // Interface IGenericDescriptor
  // =============================
  // mainly for PBDA with 16-bit descriptor length
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGenericDescriptor2);'}
  {$EXTERNALSYM IGenericDescriptor2}
  IGenericDescriptor2 = interface(IGenericDescriptor)
  ['{BF02FB7E-9792-4e10-A68D-033A2CC246A5}']

    function Initialize(pbDesc: PByte;
                        wCount: WORD): HRESULT; stdcall;

    function GetLength(out pwVal: WORD): HRESULT; stdcall;

  end;
  IID_IGenericDescriptor2 = IGenericDescriptor2;
  {$EXTERNALSYM IID_IGenericDescriptor2}



  PProgramElement = ^ProgramElement;
  ProgramElement = record
    wProgramNumber: WORD;
    wProgramMapPID: WORD;
  end;
  {$EXTERNALSYM ProgramElement}

  // Interface IPAT
  // ==============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPAT);'}
  {$EXTERNALSYM IPAT}
  PIPAT = ^IPAT;
  {$EXTERNALSYM PIPAT}
  IPAT = interface(IUnknown)
  ['{6623B511-4B5F-43c3-9A01-E8FF84188060}']

    function Initialize(pSectionList: ISectionList;
                        pMPEGData: IMpeg2Data): HRESULT; stdcall;

    function GetTransportStreamId(out pwVal: WORD): HRESULT; stdcall;

    function GetVersionNumber(out pbVal: PByte): HRESULT; stdcall;

    function GetCountOfRecords(out pdwVal: DWORD): HRESULT; stdcall;

    function GetRecordProgramNumber(dwIndex: DWORD;
                                    out pwVal: WORD): HRESULT; stdcall;

    function GetRecordProgramMapPid(dwIndex: DWORD;
                                    out pwVal: WORD): HRESULT; stdcall;

    function FindRecordProgramMapPid(wProgramNumber: WORD;
                                     out pwVal: WORD): HRESULT; stdcall;

    function RegisterForNextTable(hNextTableAvailable: THandle): HRESULT; stdcall;

    function GetNextTable(out ppPAT: PIPAT): HRESULT; stdcall;

    function RegisterForWhenCurrent(hNextTableIsCurrent: THandle): HRESULT; stdcall;

    function ConvertNextToCurrent(): HRESULT; stdcall;

  end;
  IID_IPAT = IPAT;
  {$EXTERNALSYM IID_IPAT}


  // Interface ICAT
  // ==============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICAT);'}
  {$EXTERNALSYM ICAT}
  PICAT = ^ICAT;
  {$EXTERNALSYM PICAT}
  ICAT = interface(IUnknown)
  ['{7C6995FB-2A31-4bd7-953E-B1AD7FB7D31C}']

    function Initialize(pSectionList: ISectionList;
                        pMPEGData: IMpeg2Data): HRESULT; stdcall;

    function GetVersionNumber(out pbVal: PByte): HRESULT; stdcall;

    function GetCountOfTableDescriptors(out pdwVal: DWORD): HRESULT; stdcall;

    function GetTableDescriptorByIndex(dwIndex: DWORD;
                                       out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function GetTableDescriptorByTag(bTag: Byte;
                                     {in, out} pdwCookie: PDWORD;  // OPTIONAL
                                     out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function RegisterForNextTable(hNextTableAvailable: THandle): HRESULT; stdcall;

    function GetNextTable(dwTimeout: DWORD;
                          out ppCAT: PICAT): HRESULT; stdcall;

    function RegisterForWhenCurrent(hNextTableIsCurrent: THandle): HRESULT; stdcall;

    function ConvertNextToCurrent(): HRESULT; stdcall;

  end;
  IID_ICAT = ICAT;
  {$EXTERNALSYM IID_ICAT}


  // Interface IPMT
  // ==============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPMT);'}
  {$EXTERNALSYM IPMT}
  PIPMT = ^IPMT;
  {$EXTERNALSYM PIPMT}
  IPMT = interface(IUnknown)
  ['{01F3B398-9527-4736-94DB-5195878E97A8}']

    function Initialize(pSectionList: ISectionList;
                        pMPEGData: IMpeg2Data): HRESULT; stdcall;

    function GetProgramNumber(out pwVal: PWORD): HRESULT; stdcall;

    function GetVersionNumber(out pbVal: PByte): HRESULT; stdcall;

    function GetPcrPid(out pPidVal: PID): HRESULT; stdcall;

    function GetCountOfTableDescriptors(out pdwVal: DWORD): HRESULT; stdcall;

    function GetTableDescriptorByIndex(dwIndex: DWORD;
                                       out ppDescriptor: IGenericDescriptor): HRESULT; stdcall;

    function GetTableDescriptorByTag(bTag: Byte;
                                    {in, out} pdwCookie: PDWORD;  // OPTIONAL
                                    out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function GetCountOfRecords(out pwVal: WORD): HRESULT; stdcall;

    function GetRecordStreamType(dwRecordIndex: DWORD;
                                out pbVal: PByte): HRESULT; stdcall;

    function GetRecordElementaryPid(dwRecordIndex: DWORD;
                                   out pPidVal: PID): HRESULT; stdcall;

    function GetRecordCountOfDescriptors(dwRecordIndex: DWORD;
                                        out pdwVal: PDWORD): HRESULT; stdcall;

    function GetRecordDescriptorByIndex(dwRecordIndex: DWORD;
                                        dwDescIndex: DWORD;
                                        out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function GetRecordDescriptorByTag(dwRecordIndex: DWORD;
                                      bTag: Byte;
                                      {in, out} pdwCookie: PDWORD;  // OPTIONAL
                                      out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function QueryServiceGatewayInfo(out ppDSMCCList: PDSMCC_ELEMENT;
                                     out puiCount: UINT): HRESULT; stdcall;

    function QueryMPEInfo(out ppMPEList: PMPE_ELEMENT;
                          out puiCount: UINT): HRESULT; stdcall;

    function RegisterForNextTable(hNextTableAvailable: THandle): HRESULT; stdcall;

    function GetNextTable(out ppPMT: PIPMT): HRESULT; stdcall;

    function RegisterForWhenCurrent(hNextTableIsCurrent: THandle): HRESULT; stdcall;

    function ConvertNextToCurrent(): HRESULT; stdcall;

  end;
  IID_IPMT = IPMT;
  {$EXTERNALSYM IID_IPMT}


  // Interface ITSDT
  // ===============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITSDT);'}
  {$EXTERNALSYM ITSDT}
  PITSDT = ^ITSDT;
  {$EXTERNALSYM PITSDT}
  ITSDT = interface(IUnknown)
  ['{D19BDB43-405B-4a7c-A791-C89110C33165}']

    function Initialize(pSectionList: ISectionList;
                        pMPEGData: IMpeg2Data): HRESULT; stdcall;

    function GetVersionNumber(out pbVal: PByte): HRESULT; stdcall;

    function GetCountOfTableDescriptors(out pdwVal: DWORD): HRESULT; stdcall;

    function GetTableDescriptorByIndex(dwIndex: DWORD;
                                       out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function GetTableDescriptorByTag(bTag: Byte;
                                     {in, out} pdwCookie: PDWORD;  // OPTIONAL
                                     out ppDescriptor: PIGenericDescriptor): HRESULT; stdcall;

    function RegisterForNextTable(hNextTableAvailable: THandle): HRESULT; stdcall;

    function GetNextTable(out ppTSDT: PITSDT): HRESULT; stdcall;

    function RegisterForWhenCurrent(hNextTableIsCurrent: THandle): HRESULT; stdcall;

    function ConvertNextToCurrent(): HRESULT; stdcall;

  end;
  IID_ITSDT = ITSDT;
  {$EXTERNALSYM IID_ITSDT}


  // Interface IPSITables
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPSITables);'}
  {$EXTERNALSYM IPSITables}
  IPSITables = interface(IUnknown)
  ['{919F24C5-7B14-42ac-A4B0-2AE08DAF00AC}']

    // The parameters are basically the ones from the event EVENTID_PSITable
    // defined in bdamedia.h
    // dwPara1 - TSID, ONID|TSID for DVB EIT
    // dwPara2 - TID|PID
    // dwPara3 - dwHashedVersion
    // dwPara4 - program number for PMT, Segment#|SID for DVB EIT, but not used for others
    function GetTable(dwTSID: DWORD;
                      dwTID_PID: DWORD;
                      dwHashedVer: DWORD;
                      dwPara4: DWORD;
                      out ppIUnknown: PIUnknown): HRESULT; stdcall;

  end;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

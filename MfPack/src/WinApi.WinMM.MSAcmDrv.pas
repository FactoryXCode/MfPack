// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MsAcm.pas
// Kind: Pascal / Delphi unit
// Release date: 25-06-2021
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Audio Compression Manager Public Header File for Drivers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
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
// Source: msacmdrv.h
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
unit WinApi.WinMM.MSAcmDrv;

interface

  (*$HPPEMIT '#include <msacmdrv.h>' *)

uses
  {WinApi}
  WinApi.Windows,
  {WinMM}
  WinApi.WinMM.MsAcm,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  ACM Driver Version:
//
//  the version is a 32 bit number that is broken into three parts as
//  follows:
//
//      bits 24 - 31:   8 bit _major_ version number
//      bits 16 - 23:   8 bit _minor_ version number
//      bits  0 - 15:   16 bit build number
//
//  this is then displayed as follows:
//
//      bMajor = (BYTE)(dwVersion >> 24)
//      bMinor = (BYTE)(dwVersion >> 16) &
//      wBuild = LOWORD(dwVersion)
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function MAKE_ACM_VERSION(mjr: LONG;
                            mnr: LONG;
                            bld: LONG): LONG; inline;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
//
//  under WIN32 all drivers use unicode structures.  these have already
//  been #defined in MSACM.H.  however, regardless of whether UNICODE is
//  defined, we will define these structures as unicode structures for use
//  in 32-bit drivers.
//
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

type

  {$EXTERNALSYM ACMDRIVERDETAILS}
  ACMDRIVERDETAILS                    = tACMDRIVERDETAILSW;
  {$EXTERNALSYM PACMDRIVERDETAILS}
  PACMDRIVERDETAILS                   = PACMDRIVERDETAILSW;
  {$EXTERNALSYM LPACMDRIVERDETAILS}
  LPACMDRIVERDETAILS                  = LPACMDRIVERDETAILSW;

  {$EXTERNALSYM ACMFORMATTAGDETAILS}
  ACMFORMATTAGDETAILS                 = tACMFORMATTAGDETAILSW;
  {$EXTERNALSYM PACMFORMATTAGDETAILS}
  PACMFORMATTAGDETAILS                = PACMFORMATTAGDETAILSW;
  {$EXTERNALSYM LPACMFORMATTAGDETAILS}
  LPACMFORMATTAGDETAILS               = LPACMFORMATTAGDETAILSW;

  {$EXTERNALSYM ACMFORMATDETAILS}
  ACMFORMATDETAILS                    = tACMFORMATDETAILSW;
  {$EXTERNALSYM PACMFORMATDETAILS}
  PACMFORMATDETAILS                   = PACMFORMATDETAILSW;
  {$EXTERNALSYM LPACMFORMATDETAILS}
  LPACMFORMATDETAILS                  = LPACMFORMATDETAILSW;

  {$EXTERNALSYM ACMFORMATCHOOSE}
  ACMFORMATCHOOSE                     = tACMFORMATCHOOSEW;
  {$EXTERNALSYM PACMFORMATCHOOSE}
  PACMFORMATCHOOSE                    = PACMFORMATCHOOSEW;
  {$EXTERNALSYM LPACMFORMATCHOOSE}
  LPACMFORMATCHOOSE                   = LPACMFORMATCHOOSEW;

  {$EXTERNALSYM ACMFILTERTAGDETAILS}
  ACMFILTERTAGDETAILS                 = tACMFILTERTAGDETAILSW;
  {$EXTERNALSYM PACMFILTERTAGDETAILS}
  PACMFILTERTAGDETAILS                = PACMFILTERTAGDETAILSW;
  {$EXTERNALSYM LPACMFILTERTAGDETAILS}
  LPACMFILTERTAGDETAILS               = LPACMFILTERTAGDETAILSW;

  {$EXTERNALSYM ACMFILTERDETAILS}
  ACMFILTERDETAILS                    = tACMFILTERDETAILSW;
  {$EXTERNALSYM PACMFILTERDETAILS}
  PACMFILTERDETAILS                   = PACMFILTERDETAILSW;
  {$EXTERNALSYM LPACMFILTERDETAILS}
  LPACMFILTERDETAILS                  = LPACMFILTERDETAILSW;

  {$EXTERNALSYM ACMFILTERCHOOSE}
  ACMFILTERCHOOSE                     = tACMFILTERCHOOSEW;
  {$EXTERNALSYM PACMFILTERCHOOSE}
  PACMFILTERCHOOSE                    = PACMFILTERCHOOSEW;
  {$EXTERNALSYM LPACMFILTERCHOOSE}
  LPACMFILTERCHOOSE                   = LPACMFILTERCHOOSEW;


  //
  //
  //
  //
  //
  {$DEFINE ACMDRVOPENDESC_SECTIONNAME_CHARS}

type

  PACMDRVOPENDESCA = ^tACMDRVOPENDESCA;
  {$EXTERNALSYM PACMDRVOPENDESCA}
  tACMDRVOPENDESCA = record
    cbStruct: DWORD;                 // sizeof(ACMDRVOPENDESC)
    fccType: FOURCC;                 // 'audc'
    fccComp: FOURCC;                 // sub-type (not used--must be 0)
    dwVersion: DWORD;                // current version of ACM opening you
    dwFlags: DWORD;                  //
    dwError: DWORD;                  // result from DRV_OPEN request
    pszSectionName: PAnsiChar;       // see DRVCONFIGINFO.lpszDCISectionName
    pszAliasName: PAnsiChar;         // see DRVCONFIGINFO.lpszDCIAliasName
    dnDevNode: DWORD;                // devnode id for pnp drivers.
  end;
  {$EXTERNALSYM tACMDRVOPENDESCA}
  ACMDRVOPENDESCA = tACMDRVOPENDESCA;
  {$EXTERNALSYM ACMDRVOPENDESCA}
  LPACMDRVOPENDESCA = ^tACMDRVOPENDESCA;
  {$EXTERNALSYM LPACMDRVOPENDESCA}

  PACMDRVOPENDESCW = ^tACMDRVOPENDESCW;
  {$EXTERNALSYM PACMDRVOPENDESCW}
  tACMDRVOPENDESCW = record
    cbStruct: DWORD;                 // sizeof(ACMDRVOPENDESC)
    fccType: FOURCC;                 // 'audc'
    fccComp: FOURCC;                 // sub-type (not used--must be 0)
    dwVersion: DWORD;                // current version of ACM opening you
    dwFlags: DWORD;                  //
    dwError: DWORD;                  // result from DRV_OPEN request
    pszSectionName: PWideChar;       // see DRVCONFIGINFO.lpszDCISectionName
    pszAliasName: PWideChar;         // see DRVCONFIGINFO.lpszDCIAliasName
    dnDevNode: DWORD;               // devnode id for pnp drivers.
  end;
  {$EXTERNALSYM tACMDRVOPENDESCW}
  ACMDRVOPENDESCW = tACMDRVOPENDESCW;
  {$EXTERNALSYM ACMDRVOPENDESCW}
  LPACMDRVOPENDESCW = ^tACMDRVOPENDESCW;
  {$EXTERNALSYM LPACMDRVOPENDESCW}

  ACMDRVOPENDESC                      = ACMDRVOPENDESCW;
  {$EXTERNALSYM ACMDRVOPENDESC}
  PACMDRVOPENDESC                     = PACMDRVOPENDESCW;
  {$EXTERNALSYM PACMDRVOPENDESC}
  LPACMDRVOPENDESC                    = LPACMDRVOPENDESCW;
  {$EXTERNALSYM LPACMDRVOPENDESC}


  //
  //
  //
  //
  //
  PACMDRVSTREAMINSTANCE = ^tACMDRVSTREAMINSTANCE;
  {$EXTERNALSYM PACMDRVSTREAMINSTANCE}
  tACMDRVSTREAMINSTANCE = record
    cbStruct: DWORD;
    pwfxSrc: LPWAVEFORMATEX;
    pwfxDst: LPWAVEFORMATEX;
    pwfltr: LPWAVEFILTER;
    dwCallback: DWORD_PTR;
    dwInstance: DWORD_PTR;
    fdwOpen: DWORD;
    fdwDriver: DWORD;
    dwDriver: DWORD_PTR;
    has: HACMSTREAM;
  end;
  {$EXTERNALSYM tACMDRVSTREAMINSTANCE}
  ACMDRVSTREAMINSTANCE = tACMDRVSTREAMINSTANCE;
  {$EXTERNALSYM ACMDRVSTREAMINSTANCE}
  LPACMDRVSTREAMINSTANCE = ^tACMDRVSTREAMINSTANCE;
  {$EXTERNALSYM LPACMDRVSTREAMINSTANCE}


  //
  //  NOTE! this structure must match the ACMSTREAMHEADER in msacm.h but
  //  defines more information for the driver writing convenience
  //
  LPACMDRVSTREAMHEADER = ^tACMDRVSTREAMHEADER;
  {$EXTERNALSYM LPACMDRVSTREAMHEADER}

  PACMDRVSTREAMHEADER = ^tACMDRVSTREAMHEADER;
  {$EXTERNALSYM PACMDRVSTREAMHEADER}

  tACMDRVSTREAMHEADER = record
    cbStruct: DWORD;
    fdwStatus: DWORD;
    dwUser: DWORD_PTR;
    pbSrc: PByte;
    cbSrcLength: DWORD;
    cbSrcLengthUsed: DWORD;
    dwSrcUser: DWORD_PTR;
    pbDst: PByte;
    cbDstLength: DWORD;
    cbDstLengthUsed: DWORD;
    dwDstUser: DWORD_PTR;
    fdwConvert: DWORD;                 // flags passed from convert func
    padshNext: LPACMDRVSTREAMHEADER;   // for async driver queueing
    fdwDriver: DWORD;                  // driver instance flags
    dwDriver: DWORD_PTR;               // driver instance data
                                       //
                                       //  all remaining fields are used by the ACM for bookkeeping purposes.
                                       //  an ACM driver should never use these fields (though than can be
                                       //  helpful for debugging)--note that the meaning of these fields
                                       //  may change, so do NOT rely on them in shipping code.
                                       //
    fdwPrepared: DWORD;
    dwPrepared: DWORD_PTR;
    pbPreparedSrc: PByte;
    cbPreparedSrcLength: DWORD;
    pbPreparedDst: PByte;
    cbPreparedDstLength: DWORD;
  end;
  {$EXTERNALSYM tACMDRVSTREAMHEADER}
  ACMDRVSTREAMHEADER = tACMDRVSTREAMHEADER;
  {$EXTERNALSYM ACMDRVSTREAMHEADER}


  //
  //  structure for ACMDM_STREAM_SIZE message
  //
  //
  PACMDRVSTREAMSIZE = ^tACMDRVSTREAMSIZE;
  {$EXTERNALSYM PACMDRVSTREAMSIZE}
  tACMDRVSTREAMSIZE = record
    cbStruct: DWORD;
    fdwSize: DWORD;
    cbSrcLength: DWORD;
    cbDstLength: DWORD;
  end;
  {$EXTERNALSYM tACMDRVSTREAMSIZE}
  ACMDRVSTREAMSIZE = tACMDRVSTREAMSIZE;
  {$EXTERNALSYM ACMDRVSTREAMSIZE}
  LPACMDRVSTREAMSIZE = ^tACMDRVSTREAMSIZE;
  {$EXTERNALSYM LPACMDRVSTREAMSIZE}



  //
  //  structure containing the information for the ACMDM_FORMAT_SUGGEST message
  //
  //
  PACMDRVFORMATSUGGEST = ^tACMDRVFORMATSUGGEST;
  {$EXTERNALSYM PACMDRVFORMATSUGGEST}
  tACMDRVFORMATSUGGEST = record
    cbStruct: DWORD;                 // sizeof(ACMDRVFORMATSUGGEST)
    fdwSuggest: DWORD;               // Suggest flags
    pwfxSrc: LPWAVEFORMATEX;         // Source Format
    cbwfxSrc: DWORD;                 // Source Size
    pwfxDst: LPWAVEFORMATEX;         // Dest format
    cbwfxDst: DWORD;                // Dest Size
  end;
  {$EXTERNALSYM tACMDRVFORMATSUGGEST}
  ACMDRVFORMATSUGGEST = tACMDRVFORMATSUGGEST;
  {$EXTERNALSYM ACMDRVFORMATSUGGEST}
  LPACMDRVFORMATSUGGEST = ^tACMDRVFORMATSUGGEST;
  {$EXTERNALSYM LPACMDRVFORMATSUGGEST}

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  ACM Driver Messages
  //
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

  ACMDM_DRIVER_NOTIFY                 = (ACMDM_BASE + 1);
  {$EXTERNALSYM ACMDM_DRIVER_NOTIFY}
  ACMDM_DRIVER_DETAILS                = (ACMDM_BASE + 10);
  {$EXTERNALSYM ACMDM_DRIVER_DETAILS}

  ACMDM_HARDWARE_WAVE_CAPS_INPUT      = (ACMDM_BASE + 20);
  {$EXTERNALSYM ACMDM_HARDWARE_WAVE_CAPS_INPUT}
  ACMDM_HARDWARE_WAVE_CAPS_OUTPUT     = (ACMDM_BASE + 21);
  {$EXTERNALSYM ACMDM_HARDWARE_WAVE_CAPS_OUTPUT}

  ACMDM_FORMATTAG_DETAILS             = (ACMDM_BASE + 25);
  {$EXTERNALSYM ACMDM_FORMATTAG_DETAILS}
  ACMDM_FORMAT_DETAILS                = (ACMDM_BASE + 26);
  {$EXTERNALSYM ACMDM_FORMAT_DETAILS}
  ACMDM_FORMAT_SUGGEST                = (ACMDM_BASE + 27);
  {$EXTERNALSYM ACMDM_FORMAT_SUGGEST}

  ACMDM_FILTERTAG_DETAILS             = (ACMDM_BASE + 50);
  {$EXTERNALSYM ACMDM_FILTERTAG_DETAILS}
  ACMDM_FILTER_DETAILS                = (ACMDM_BASE + 51);
  {$EXTERNALSYM ACMDM_FILTER_DETAILS}

  ACMDM_STREAM_OPEN                   = (ACMDM_BASE + 76);
  {$EXTERNALSYM ACMDM_STREAM_OPEN}
  ACMDM_STREAM_CLOSE                  = (ACMDM_BASE + 77);
  {$EXTERNALSYM ACMDM_STREAM_CLOSE}
  ACMDM_STREAM_SIZE                   = (ACMDM_BASE + 78);
  {$EXTERNALSYM ACMDM_STREAM_SIZE}
  ACMDM_STREAM_CONVERT                = (ACMDM_BASE + 79);
  {$EXTERNALSYM ACMDM_STREAM_CONVERT}
  ACMDM_STREAM_RESET                  = (ACMDM_BASE + 80);
  {$EXTERNALSYM ACMDM_STREAM_RESET}
  ACMDM_STREAM_PREPARE                = (ACMDM_BASE + 81);
  {$EXTERNALSYM ACMDM_STREAM_PREPARE}
  ACMDM_STREAM_UNPREPARE              = (ACMDM_BASE + 82);
  {$EXTERNALSYM ACMDM_STREAM_UNPREPARE}
  ACMDM_STREAM_UPDATE                 = (ACMDM_BASE + 83);
  {$EXTERNALSYM ACMDM_STREAM_UPDATE}


implementation


function MAKE_ACM_VERSION(mjr: LONG;
                          mnr: LONG;
                          bld: LONG): LONG; inline;
begin
  Result := (LONG(mjr) shr 24) or
            (LONG(mnr) shr 16) or
            (LONG(bld));
end;

end.

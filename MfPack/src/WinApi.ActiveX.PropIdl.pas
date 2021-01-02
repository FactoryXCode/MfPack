// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.PropIdl.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Structured Storage.
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
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//          This module can be replaced by WinApi.ActiveX, if your version of Delphi ActiveX is >= XE2.
//          In that case you have to include WinApi.ActiveX for every MFPack unit and change the mfPROPVARIANT with the ActiveX one.
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
// Source: propidl.h
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
unit WinApi.ActiveX.PropIdl;

  {$HPPEMIT '#include "propidl.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  {ActiveX}
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.OaIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  CCH_MAX_PROPSTG_NAME                = 31;
  {$EXTERNALSYM CCH_MAX_PROPSTG_NAME}

  PROPSETHDR_OSVERSION_UNKNOWN        = MAXDWORD;
  {$EXTERNALSYM PROPSETHDR_OSVERSION_UNKNOWN}

  // Flags for IPropertySetStorage.Create
  PROPSETFLAG_DEFAULT                 = 0;
  {$EXTERNALSYM PROPSETFLAG_DEFAULT}
  PROPSETFLAG_NONSIMPLE               = 1;
  {$EXTERNALSYM PROPSETFLAG_NONSIMPLE}
  PROPSETFLAG_ANSI                    = 2;
  {$EXTERNALSYM PROPSETFLAG_ANSI}

  // This flag is only supported on StgCreatePropStg & StgOpenPropStg
  PROPSETFLAG_UNBUFFERED              = 4;
  {$EXTERNALSYM PROPSETFLAG_UNBUFFERED}

  // This flag causes a version-1 property set to be created
  PROPSETFLAG_CASE_SENSITIVE          = 8;
  {$EXTERNALSYM PROPSETFLAG_CASE_SENSITIVE}

  // Flags for the reservied PID_BEHAVIOR property
  PROPSET_BEHAVIOR_CASE_SENSITIVE     = 1;
  {$EXTERNALSYM PROPSET_BEHAVIOR_CASE_SENSITIVE}

  // Reserved global Property IDs
  PID_DICTIONARY                      = 0;
  {$EXTERNALSYM PID_DICTIONARY}
  PID_CODEPAGE                        = $1;
  {$EXTERNALSYM PID_CODEPAGE}
  PID_FIRST_USABLE                    = $2;
  {$EXTERNALSYM PID_FIRST_USABLE}
  PID_FIRST_NAME_DEFAULT              = $FFF;
  {$EXTERNALSYM PID_FIRST_NAME_DEFAULT}
  PID_LOCALE                          = $80000000;
  {$EXTERNALSYM PID_LOCALE}
  PID_MODIFY_TIME                     = $80000001;
  {$EXTERNALSYM PID_MODIFY_TIME}
  PID_SECURITY                        = $80000002;
  {$EXTERNALSYM PID_SECURITY}
  PID_BEHAVIOR                        = $80000003;
  {$EXTERNALSYM PID_BEHAVIOR}
  PID_ILLEGAL                         = MAXDWORD;
  {$EXTERNALSYM PID_ILLEGAL}

  // Range which is read-only to downlevel implementations
  PID_MIN_READONLY                    = $80000000;
  {$EXTERNALSYM PID_MIN_READONLY}
  PID_MAX_READONLY                    = $BFFFFFFF;
  {$EXTERNALSYM PID_MAX_READONLY}

  // Property IDs for the DiscardableInformation Property Set
  PIDDI_THUMBNAIL                     = $00000002;  // VT_BLOB
  {$EXTERNALSYM PIDDI_THUMBNAIL}

  // Property IDs for the SummaryInformation Property Set
  PIDSI_TITLE                         = $00000002;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_TITLE}
  PIDSI_SUBJECT                       = $00000003;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_SUBJECT}
  PIDSI_AUTHOR                        = $00000004;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_AUTHOR}
  PIDSI_KEYWORDS                      = $00000005;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_KEYWORDS}
  PIDSI_COMMENTS                      = $00000006;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_COMMENTS}
  PIDSI_TEMPLATE                      = $00000007;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_TEMPLATE}
  PIDSI_LASTAUTHOR                    = $00000008;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_LASTAUTHOR}
  PIDSI_REVNUMBER                     = $00000009;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_REVNUMBER}
  PIDSI_EDITTIME                      = $0000000A;  // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_EDITTIME}
  PIDSI_LASTPRINTED                   = $0000000B;  // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_LASTPRINTED}
  PIDSI_CREATE_DTM                    = $0000000C;  // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_CREATE_DTM}
  PIDSI_LASTSAVE_DTM                  = $0000000D;  // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDSI_LASTSAVE_DTM}
  PIDSI_PAGECOUNT                     = $0000000E;  // VT_I4
  {$EXTERNALSYM PIDSI_PAGECOUNT}
  PIDSI_WORDCOUNT                     = $0000000F;  // VT_I4
  {$EXTERNALSYM PIDSI_WORDCOUNT}
  PIDSI_CHARCOUNT                     = $00000010;  // VT_I4
  {$EXTERNALSYM PIDSI_CHARCOUNT}
  PIDSI_THUMBNAIL                     = $00000011;  // VT_CF
  {$EXTERNALSYM PIDSI_THUMBNAIL}
  PIDSI_APPNAME                       = $00000012;  // VT_LPSTR
  {$EXTERNALSYM PIDSI_APPNAME}
  PIDSI_DOC_SECURITY                  = $00000013;  // VT_I4
  {$EXTERNALSYM PIDSI_DOC_SECURITY}

  // Property IDs for the DocSummaryInformation Property Set
  PIDDSI_CATEGORY                     = $00000002;  // VT_LPSTR
  {$EXTERNALSYM PIDDSI_CATEGORY}
  PIDDSI_PRESFORMAT                   = $00000003;  // VT_LPSTR
  {$EXTERNALSYM PIDDSI_PRESFORMAT}
  PIDDSI_BYTECOUNT                    = $00000004;  // VT_I4
  {$EXTERNALSYM PIDDSI_BYTECOUNT}
  PIDDSI_LINECOUNT                    = $00000005;  // VT_I4
  {$EXTERNALSYM PIDDSI_LINECOUNT}
  PIDDSI_PARCOUNT                     = $00000006;  // VT_I4
  {$EXTERNALSYM PIDDSI_PARCOUNT}
  PIDDSI_SLIDECOUNT                   = $00000007;  // VT_I4
  {$EXTERNALSYM PIDDSI_SLIDECOUNT}
  PIDDSI_NOTECOUNT                    = $00000008;  // VT_I4
  {$EXTERNALSYM PIDDSI_NOTECOUNT}
  PIDDSI_HIDDENCOUNT                  = $00000009;  // VT_I4
  {$EXTERNALSYM PIDDSI_HIDDENCOUNT}
  PIDDSI_MMCLIPCOUNT                  = $0000000A;  // VT_I4
  {$EXTERNALSYM PIDDSI_MMCLIPCOUNT}
  PIDDSI_SCALE                        = $0000000B;  // VT_BOOL
  {$EXTERNALSYM PIDDSI_SCALE}
  PIDDSI_HEADINGPAIR                  = $0000000C;  // VT_VARIANT | VT_VECTOR
  {$EXTERNALSYM PIDDSI_HEADINGPAIR}
  PIDDSI_DOCPARTS                     = $0000000D;  // VT_LPSTR | VT_VECTOR
  {$EXTERNALSYM PIDDSI_DOCPARTS}
  PIDDSI_MANAGER                      = $0000000E;  // VT_LPSTR
  {$EXTERNALSYM PIDDSI_MANAGER}
  PIDDSI_COMPANY                      = $0000000F;  // VT_LPSTR
  {$EXTERNALSYM PIDDSI_COMPANY}
  PIDDSI_LINKSDIRTY                   = $00000010;  // VT_BOOL
  {$EXTERNALSYM PIDDSI_LINKSDIRTY}


  // FMTID_MediaFileSummaryInfo - Property IDs
  PIDMSI_EDITOR                       = $00000002;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_EDITOR}
  PIDMSI_SUPPLIER                     = $00000003;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_SUPPLIER}
  PIDMSI_SOURCE                       = $00000004;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_SOURCE}
  PIDMSI_SEQUENCE_NO                  = $00000005;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_SEQUENCE_NO}
  PIDMSI_PROJECT                      = $00000006;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_PROJECT}
  PIDMSI_STATUS                       = $00000007;  // VT_UI4
  {$EXTERNALSYM PIDMSI_STATUS}
  PIDMSI_OWNER                        = $00000008;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_OWNER}
  PIDMSI_RATING                       = $00000009;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_RATING}
  PIDMSI_PRODUCTION                   = $0000000A;  // VT_FILETIME (UTC)
  {$EXTERNALSYM PIDMSI_PRODUCTION}
  PIDMSI_COPYRIGHT                    = $0000000B;  // VT_LPWSTR
  {$EXTERNALSYM PIDMSI_COPYRIGHT}


  PRSPEC_INVALID                      = MAXDWORD;
  {$EXTERNALSYM PRSPEC_INVALID}
  PRSPEC_LPWSTR                       = 0;
  {$EXTERNALSYM PRSPEC_LPWSTR}
  PRSPEC_PROPID                       = 1;
  {$EXTERNALSYM PRSPEC_PROPID}

type

  // This is the PROPVARIANT padding layout for marshaling.
  PROPVAR_PAD1 = Byte;
  {$EXTERNALSYM PROPVAR_PAD1}
  PROPVAR_PAD2 = Byte;
  {$EXTERNALSYM PROPVAR_PAD2}
  PROPVAR_PAD3 = ULONG;
  {$EXTERNALSYM PROPVAR_PAD3}


  LPVERSIONEDSTREAM = ^VERSIONEDSTREAM;
  tagVersionedStream = record
    guidVersion: TGUID;
    pStream: PIStream;
  end;
  {$EXTERNALSYM tagVersionedStream}
  VERSIONEDSTREAM = tagVersionedStream;
  {$EXTERNALSYM VERSIONEDSTREAM}

  // This is the standard C layout of the PROPVARIANT.

  PPROPVARIANT = ^PROPVARIANT;
  {$EXTERNALSYM PPROPVARIANT}

  PCAC = ^CAC;
  tagCAC = record
    cElems: ULONG;
    pElems: PAnsiChar;
  end;
  {$EXTERNALSYM tagCAC}
  CAC = tagCAC;
  {$EXTERNALSYM CAC}

  PCAUB = ^CAUB;
  tagCAUB = record
    cElems: ULONG;
    pElems: PUCHAR;
  end;
  {$EXTERNALSYM tagCAUB}
  CAUB = tagCAUB;
  {$EXTERNALSYM CAUB}

  PCAI = ^CAI;
  tagCAI = record
    cElems: ULONG;
    pElems: PSHORT;
  end;
  {$EXTERNALSYM tagCAI}
  CAI = tagCAI;
  {$EXTERNALSYM CAI}

  PCAUI = ^CAUI;
  tagCAUI = record
    cElems: ULONG;
    pElems: PUSHORT;
  end;
  {$EXTERNALSYM tagCAUI}
  CAUI = tagCAUI;
  {$EXTERNALSYM CAUI}

  PCAL = ^CAL;
  tagCAL = record
    cElems: ULONG;
    pElems: PLONG;
  end;
  {$EXTERNALSYM tagCAL}
  CAL = tagCAL;
  {$EXTERNALSYM CAL}

  PCAUL = ^CAUL;
  tagCAUL = record
    cElems: ULONG;
    pElems: PULONG;
  end;
  {$EXTERNALSYM tagCAUL}
  CAUL = tagCAUL;
  {$EXTERNALSYM CAUL}

  PCAFLT = ^CAFLT;
  tagCAFLT = record
    cElems: ULONG;
    pElems: PFLOAT;
  end;
  {$EXTERNALSYM tagCAFLT}
  CAFLT = tagCAFLT;
  {$EXTERNALSYM CAFLT}

  PCADBL = ^CADBL;
  tagCADBL = record
    cElems: ULONG;
    pElems: PDOUBLE;
  end;
  {$EXTERNALSYM tagCADBL}
  CADBL = tagCADBL;
  {$EXTERNALSYM CADBL}

  PCACY = ^CACY;
  tagCACY = record
    cElems: ULONG;
    pElems: PCY;
  end;
  {$EXTERNALSYM tagCACY}
  CACY = tagCACY;
  {$EXTERNALSYM CACY}

  PCADATE = ^CADATE;
  tagCADATE = record
    cElems: ULONG;
    pElems: PDATE;
  end;
  {$EXTERNALSYM tagCADATE}
  CADATE = tagCADATE;
  {$EXTERNALSYM CADATE}

  PCABSTR = ^CABSTR;
  tagCABSTR = record
    cElems: ULONG;
    pElems: PBSTR;
  end;
  {$EXTERNALSYM tagCABSTR}
  CABSTR = tagCABSTR;
  {$EXTERNALSYM CABSTR}

  PCABSTRBLOB = ^CABSTRBLOB;
  tagCABSTRBLOB = record
    cElems: ULONG;
    pElems: PBSTR;
  end;
  {$EXTERNALSYM tagCABSTRBLOB}
  CABSTRBLOB = tagCABSTRBLOB;
  {$EXTERNALSYM CABSTRBLOB}

  PCABOOL = ^CABOOL;
  tagCABOOL = record
    cElems: ULONG;
    pElems: PVARIANT_BOOL;
  end;
  {$EXTERNALSYM tagCABOOL}
  CABOOL = tagCABOOL;
  {$EXTERNALSYM CABOOL}

  PCASCODE = ^CASCODE;
  tagCASCODE = record
    cElems: ULONG;
    pElems: PSCODE;
  end;
  {$EXTERNALSYM tagCASCODE}
  CASCODE = tagCASCODE;
  {$EXTERNALSYM CASCODE}

  PCAPROPVARIANT = ^CAPROPVARIANT;
  tagCAPROPVARIANT = record
    cElems: ULONG;
    pElems: PPROPVARIANT;
  end;
  {$EXTERNALSYM tagCAPROPVARIANT}
  CAPROPVARIANT = tagCAPROPVARIANT;
  {$EXTERNALSYM CAPROPVARIANT}

  PCAH = ^CAH;
  tagCAH = record
    cElems: ULONG;
    pElems: PLARGE_INTEGER;
  end;
  {$EXTERNALSYM tagCAH}
  CAH = tagCAH;
  {$EXTERNALSYM CAH}

  PCAUH = ^CAUH;
  tagCAUH = record
    cElems: ULONG;
    pElems: PULARGE_INTEGER;
  end;
  {$EXTERNALSYM tagCAUH}
  CAUH = tagCAUH;
  {$EXTERNALSYM CAUH}

  PCALPSTR = ^CALPSTR;
  tagCALPSTR = record
    cElems: ULONG;
    pElems: PLPSTR;
  end;
  {$EXTERNALSYM tagCALPSTR}
  CALPSTR = tagCALPSTR;
  {$EXTERNALSYM CALPSTR}

  PCALPWSTR = ^CALPWSTR;
  tagCALPWSTR = record
    cElems: ULONG;
    pElems: LPWSTR;
  end;
  {$EXTERNALSYM tagCALPWSTR}
  CALPWSTR = tagCALPWSTR;
  {$EXTERNALSYM CALPWSTR}

  PCAFILETIME = ^CAFILETIME;
  tagCAFILETIME = record
    cElems: ULONG;
    pElems: PFILETIME;
  end;
  {$EXTERNALSYM tagCAFILETIME}
  CAFILETIME = tagCAFILETIME;
  {$EXTERNALSYM CAFILETIME}

  PCACLIPDATA = ^CACLIPDATA;
  tagCACLIPDATA = record
    cElems: ULONG;
    pElems: PCLIPDATA;
  end;
  {$EXTERNALSYM tagCACLIPDATA}
  CACLIPDATA = tagCACLIPDATA;
  {$EXTERNALSYM CACLIPDATA}

  PCACLSID = ^CACLSID;
  tagCACLSID = record
    cElems: ULONG;
    pElems: PCLSID;
  end;
  {$EXTERNALSYM tagCACLSID}
  CACLSID = tagCACLSID;
  {$EXTERNALSYM CACLSID}

  // typedef struct tagPROPVARIANT PROPVARIANT;
  // See WTypes.pas for details of the case select vars
  // this structure is also defined in ActiveX, but depending on the compilerversion
  // this could differ. So we undefine the -older - ActiveX version.
  //
  // Members

  // vt            Value type tag.
  // wReserved1    Reserved for future use.
  // wReserved2    Reserved for future use.
  // wReserved3    Reserved for future use.
  // cVal          VT_I1, Version 1
  // bVal          VT_UI1
  // iVal          VT_I2
  // uiVal         VT_UI2
  // lVal          VT_I4
  // ulVal         VT_UI4
  // intVal        VT_INT, Version 1
  // uintVal       VT_UINT, Version 1
  // hVal          VT_I8
  // uhVal         VT_UI8
  // fltVal        VT_R4
  // dblVal        VT_R8
  // boolVal       VT_BOOL
  // scode         VT_ERROR
  // cyVal         VT_CY
  // date          VT_DATE
  // filetime      VT_FILETIME
  // puuid         VT_CLSID
  // pclipdata     VT_CF
  // bstrVal       VT_BSTR
  // bstrblobVal   VT_BSTR_BLOB
  // blob          VT_BLOB, VT_BLOBOBJECT
  // pszVal        VT_LPSTR
  // pwszVal       VT_LPWSTR
  // punkVal       VT_UNKNOWN
  // pdispVal      VT_DISPATCH
  // pStream       VT_STREAM, VT_STREAMED_OBJECT
  // pStorage      VT_STORAGE, VT_STORED_OBJECT
  // pVersionedStream  VT_VERSIONED_STREAM
  // parray        VT_ARRAY Or VT_*, Version 1
  // cac           VT_VECTOR OR VT_I1, Version 1
  // caub          VT_VECTOR OR VT_UI1
  // cai           VT_VECTOR OR VT_I2
  // caui          VT_VECTOR OR VT_UI2
  // cal           VT_VECTOR OR VT_I4
  // caul          VT_VECTOR OR VT_UI4
  // cah           VT_VECTOR OR VT_I8
  // cauh          VT_VECTOR OR VT_UI8
  // caflt         VT_VECTOR OR VT_R4
  // cadbl         VT_VECTOR OR VT_R8
  // cabool        VT_VECTOR OR VT_BOOL
  // cascode       VT_VECTOR OR VT_ERROR
  // cacy          VT_VECTOR OR VT_CY
  // cadate        VT_VECTOR OR VT_DATE
  // cafiletime    VT_VECTOR OR VT_FILETIME
  // cauuid        VT_VECTOR OR VT_CLSID
  // caclipdata    VT_VECTOR OR VT_CF
  // cabstr        VT_VECTOR OR VT_BSTR
  // cabstrblob    VT_VECTOR OR VT_BSTR_BLOB
  // calpstr       VT_VECTOR OR VT_LPSTR
  // calpwstr      VT_VECTOR OR VT_LPWSTR
  // capropvar     VT_VECTOR OR VT_VARIANT
  // pcVal         VT_BYREF  OR VT_I1, Version 1
  // pbVal         VT_BYREF OR VT_UI1, Version 1
  // piVal         VT_BYREF OR VT_I2, Version 1
  // puiVal        VT_BYREF OR VT_UI2, Version 1
  // plVal         VT_BYREF OR VT_I4, Version 1
  // pulVal        VT_BYREF OR VT_UI4, Version 1
  // pintVal       VT_BYREF OR VT_INT, Version 1
  // puintVal      VT_BYREF OR VT_UINT, Version 1
  // pfltVal       VT_BYREF OR VT_R4, Version 1
  // pdblVal       VT_BYREF OR VT_R8, Version 1
  // pboolVal      VT_BYREF OR VT_BOOL, Version 1
  // pdecVal       VT_BYREF OR VT_DECIMAL, Version 1
  // pscode        VT_BYREF OR VT_ERROR, Version 1
  // pcyVal        VT_BYREF OR VT_CY, Version 1
  // pdate         VT_BYREF OR VT_DATE, Version 1
  // pbstrVal      VT_BYREF OR VT_BSTR, Version 1
  // ppunkVal      VT_BYREF OR VT_UNKNOWN, Version 1
  // ppdispVal     VT_BYREF OR VT_DISPATCH, Version 1
  // pparray       VT_BYREF OR VT_ARRAY, Version 1
  // pvarVal       VT_BYREF OR VT_VARIANT, Version 1
  //
  // Remarks
  //   The PROPVARIANT structure can also hold a value of VT_DECIMAL:
  //   DECIMAL       decVal;        // VT_DECIMAL
  //
  //   However, the value of the DECIMAL structure requires special handling.
  //   The DECIMAL structure is the same size as an entire PROPVARIANT structure and
  //   does not fit into the union that holds all other types of values.
  //   Instead, the value of the DECIMAL structure occupies the entire PROPVARIANT structure,
  //   including the reserved fields and the vt member.
  //   However, the first member of the DECIMAL structure is not used and is equal
  //   in size to the vt member of the PROPVARIANT structure.
  //   Therefore, the PROPVARIANT structure declaration in the Propidl.h header file of
  //   Win32 defines the decVal member intagBLOB such a way that it corresponds to the beginning of
  //   the PROPVARIANT structure.
  //   Therefore, to put the value of the DECIMAL structure into a PROPVARIANT structure,
  //   the value must be loaded into the decVal member and the vt member is set to VT_DECIMAL,
  //   just as for any other value.
  //
  //   PROPVARIANT is the fundamental data type by which property values are read and written
  //   through the IPropertyStorage interface.

  // This is the LPPROPVARIANT definition for marshaling.
  LPPROPVARIANT = ^tagPROPVARIANT;
  {$EXTERNALSYM LPPROPVARIANT}
  REFPROPVARIANT = ^tagPROPVARIANT;
  {$EXTERNALSYM REFPROPVARIANT}

  tagPROPVARIANT = record
    vt: VarType;
    wReserved1: PROPVAR_PAD1;
    wReserved2: PROPVAR_PAD2;
    wReserved3: PROPVAR_PAD3;

    case Integer of
      VT_I1:                      (cVal: AnsiChar);
      VT_UI1:                     (bVal: UCHAR);
      VT_I2:                      (iVal: SHORT);
      VT_UI2:                     (uiVal: USHORT);
      VT_I4:                      (lVal: LONG);
      VT_UI4:                     (ulVal: ULONG);
      VT_INT:                     (intVal: INT);
      VT_UINT:                    (uintVal: UINT);
      VT_I8:                      (hVal: LARGE_INTEGER);
      VT_UI8:                     (uhVal: ULARGE_INTEGER);
      VT_R4:                      (fltVal: FLOAT);
      VT_R8:                      (dblVal: DOUBLE);
      VT_BOOL:                    (boolVal: VARIANT_BOOL);
      VT_ERROR:                   (scode: SCODE);
      VT_CY:                      (cyVal: CY);
      VT_DATE:                    (date: DATE);
      VT_FILETIME:                (filetime: FILETIME);
      VT_CLSID:                   (puuid: PCLSID);
      VT_CF:                      (pclipdata: PCLIPDATA);
      VT_BSTR:                    (bstrVal: BSTR);
      VT_BSTR_BLOB:               (bstrblobVal: BSTRBLOB);
      VT_BLOB,
      VT_BLOB_OBJECT:             (blob: BLOB);
      VT_LPSTR:                   (pszVal: LPSTR);
      VT_LPWSTR:                  (pwszVal: LPWSTR);
      VT_UNKNOWN:                 (punkVal: PIUnknown);
      VT_DISPATCH:                (pdispVal: PIDispatch);
      VT_STREAM,
      VT_STREAMED_OBJECT:         (pStream: PIStream);
      VT_STORAGE,
      VT_STORED_OBJECT:           (pStorage: PIStorage);
      VT_VERSIONED_STREAM:        (pVersionedStream: LPVERSIONEDSTREAM);
      VT_ARRAY:                   (parray: PSAFEARRAY);
      VT_VECTOR OR VT_I1:         (cac: CAC);
      VT_VECTOR OR VT_UI1:        (caub: CAUB);
      VT_VECTOR OR VT_I2:         (cai: CAI);
      VT_VECTOR OR VT_UI2:        (caui: CAUI);
      VT_VECTOR OR VT_I4:         (cal: CAL);
      VT_VECTOR OR VT_UI4:        (caul: CAUL);
      VT_VECTOR OR VT_I8:         (cah: CAH);
      VT_VECTOR OR VT_UI8:        (cauh: CAUH);
      VT_VECTOR OR VT_R4:         (caflt: CAFLT);
      VT_VECTOR OR VT_R8:         (cadbl: CADBL);
      VT_VECTOR OR VT_BOOL:       (cabool: CABOOL);
      VT_VECTOR OR VT_ERROR:      (cascode: CASCODE);
      VT_VECTOR OR VT_CY:         (cacy: CACY);
      VT_VECTOR OR VT_DATE:       (cadate: CADATE);
      VT_VECTOR OR VT_FILETIME:   (cafiletime: CAFILETIME);
      VT_VECTOR OR VT_CLSID:      (cauuid: CACLSID);
      VT_VECTOR OR VT_CF:         (caclipdata: CACLIPDATA);
      VT_VECTOR OR VT_BSTR:       (cabstr: CABSTR);
      VT_VECTOR OR VT_BSTR_BLOB:  (cabstrblob: CABSTRBLOB);
      VT_VECTOR OR VT_LPSTR:      (calpstr: CALPSTR);
      VT_VECTOR OR VT_LPWSTR:     (calpwstr: PCALPWSTR);
      VT_VECTOR OR VT_VARIANT:    (capropvar: CAPROPVARIANT);
      VT_BYREF OR VT_I1:          (pcVal: PAnsiChar);
      VT_BYREF OR VT_UI1:         (pbVal: PUCHAR);
      VT_BYREF OR VT_I2:          (piVal: PSHORT);
      VT_BYREF OR VT_UI2:         (puiVal: PUSHORT);
      VT_BYREF OR VT_I4:          (plVal: PLONG);
      VT_BYREF OR VT_UI4:         (pulVal: PULONG);
      VT_BYREF OR VT_INT:         (pintVal: PINT);
      VT_BYREF OR VT_UINT:        (puintVal: PUINT);
      VT_BYREF OR VT_R4:          (pfltVal: PFLOAT);
      VT_BYREF OR VT_R8:          (pdblVal: PDOUBLE);
      VT_BYREF OR VT_BOOL:        (pboolVal: PVARIANT_BOOL);
      VT_BYREF OR VT_DECIMAL:     (pdecVal: PDECIMAL);
      VT_BYREF OR VT_ERROR:       (pscode: PSCODE);
      VT_BYREF OR VT_CY:          (pcyVal: PCY);
      VT_BYREF OR VT_DATE:        (pdate: POLE_DATE);
      VT_BYREF OR VT_BSTR:        (pbstrVal: PBSTR);
      VT_BYREF OR VT_UNKNOWN:     (ppunkVal: PPointer); // you have to cast this like: IUnknown(yourpropvar.ppunkVal).QueryInterface(IID_IUnknown, pObject);  {object = pointer}
      VT_BYREF OR VT_DISPATCH:    (ppdispVal: PPointer); // = IDispatch
      VT_BYREF OR VT_ARRAY:       (pparray: PSAFEARRAY);
      VT_BYREF OR VT_VARIANT:     (pvarVal: PPROPVARIANT); // = PPROPVARIANT
  end;
  {$EXTERNALSYM tagPROPVARIANT}
  PROPVARIANT = tagPROPVARIANT;
  {$EXTERNALSYM PROPVARIANT}
  TPropVariant = tagPROPVARIANT;
  PROPVARIANTArray = array [0..65535] of PROPVARIANT;

  // PIDMSI_STATUS value definitions
  PPIDMSI_STATUS_VALUE = ^PIDMSI_STATUS_VALUE;
  PIDMSI_STATUS_VALUE                  = (
   PIDMSI_STATUS_NORMAL                = 0,
   PIDMSI_STATUS_NEW                   = PIDMSI_STATUS_NORMAL + 1,
   PIDMSI_STATUS_PRELIM                = PIDMSI_STATUS_NEW + 1,
   PIDMSI_STATUS_DRAFT                 = PIDMSI_STATUS_PRELIM + 1,
   PIDMSI_STATUS_INPROGRESS            = PIDMSI_STATUS_DRAFT + 1,
   PIDMSI_STATUS_EDIT                  = PIDMSI_STATUS_INPROGRESS + 1,
   PIDMSI_STATUS_REVIEW                = PIDMSI_STATUS_EDIT + 1,
   PIDMSI_STATUS_PROOF                 = PIDMSI_STATUS_REVIEW + 1,
   PIDMSI_STATUS_FINAL                 = PIDMSI_STATUS_PROOF + 1,
   PIDMSI_STATUS_OTHER                 = $7fff
  );
  {$EXTERNALSYM PIDMSI_STATUS_VALUE}


{$IFNDEF _PROPSPEC_DEFINED}
// Integers are always initialized to 0 in Delphi,
// So, the default value here is 0.

  PPROPSPEC = ^tagPROPSPEC;
  tagPROPSPEC = record
    ulKind: ULONG;
    case Integer of
      0: ();  //* Empty union arm */
      1: (propid: PropID);
      2: (lpwstr: PWideChar);
  end;
  {$EXTERNALSYM tagPROPSPEC}
  PROPSPEC = tagPROPSPEC;
  {$EXTERNALSYM PROPSPEC}
{$DEFINE _PROPSPEC_DEFINED}
{$ENDIF}



  PSTATPROPSTG = ^tagSTATPROPSTG;
  tagSTATPROPSTG = record
    lpwstrName: PWideChar;
    propid: PROPID;
    vt: VARTYPE;
  end;
  {$EXTERNALSYM tagSTATPROPSTG}
  STATPROPSTG = tagSTATPROPSTG;
  {$EXTERNALSYM STATPROPSTG}

  PSTATPROPSETSTG = ^tagSTATPROPSETSTG;
  tagSTATPROPSETSTG = record
    fmtid: FMTID;
    clsid: CLSID;
    grfFlags: DWORD;
    mtime: FILETIME;
    ctime: FILETIME;
    atime: FILETIME;
    dwOSVersion: DWORD;
  end;
  {$EXTERNALSYM tagSTATPROPSETSTG}
  STATPROPSETSTG = tagSTATPROPSETSTG;
  {$EXTERNALSYM STATPROPSETSTG}

  PSERIALIZEDPROPERTYVALUE = ^tagSERIALIZEDPROPERTYVALUE;
  tagSERIALIZEDPROPERTYVALUE = record
    dwType: DWORD;
    rgb: array[0..0] of Byte;
  end;
  {$EXTERNALSYM tagSERIALIZEDPROPERTYVALUE}
  SERIALIZEDPROPERTYVALUE = tagSERIALIZEDPROPERTYVALUE;
  {$EXTERNALSYM SERIALIZEDPROPERTYVALUE}

type

  // Forward Interfaces Declarations

  PIEnumSTATPROPSTG = ^IEnumSTATPROPSTG;
  IEnumSTATPROPSTG = interface;

  PIEnumSTATPROPSETSTG = ^IEnumSTATPROPSETSTG;
  IEnumSTATPROPSETSTG = interface;


  // Interfaces ////////////////////////////////////////////////////////////////


  // Interface IPropertyStorage
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyStorage);'}
  {$EXTERNALSYM IPropertyStorage}
  IPropertyStorage = interface(IUnknown)
  ['{00000138-0000-0000-C000-000000000046}']
    function ReadMultiple(cpspec: ULONG;
                          rgpspec: PROPSPEC;
                          out rgpropvar: PROPVARIANT): HResult; stdcall;

    function WriteMultiple(cpspec: ULONG;
                           rgpspec: PROPSPEC;
                           rgpropvar: PROPVARIANT;
                           propidNameFirst: PROPID): HResult; stdcall;

    function DeleteMultiple(cpspec: ULONG;
                            rgpspec: PROPSPEC): HResult; stdcall;

    function ReadPropertyNames(cpropid: ULONG;
                               rgpropid: PROPID;
                               out rglpwstrName: POLESTR): HResult; stdcall;

    function WritePropertyNames(cpropid: ULONG;
                                rgpropid: PROPID;
                                rglpwstrName: POLESTR): HResult; stdcall;

    function DeletePropertyNames(cpropid: ULONG;
                                 rgpropid: PROPID): HResult; stdcall;

    function Commit(grfCommitFlags: DWORD): HResult; stdcall;

    function Revert(): HResult; stdcall;

    function Enum(out ppenum: PIEnumSTATPROPSTG): HResult; stdcall;

    function SetTimes(pctime: FILETIME;
                      patime: FILETIME;
                      pmtime: FILETIME): HResult; stdcall;

    function SetClass(const clsid: REFCLSID): HResult; stdcall;

    function Stat(out pstatpsstg: STATPROPSETSTG): HResult; stdcall;

  end;
  IID_IPropertyStorage = IPropertyStorage;
  {$EXTERNALSYM IID_IPropertyStorage}


  // Interface IPropertySetStorage
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertySetStorage);'}
  {$EXTERNALSYM IPropertySetStorage}
  IPropertySetStorage = interface(IUnknown)
  ['{0000013A-0000-0000-C000-000000000046}']
    function Create(const rfmtid: REFFMTID;
                    const pclsid: CLSID;
                    grfFlags: DWORD;
                    grfMode: DWORD;
                    out ppprstg: IPropertyStorage): HResult; stdcall;

    function Open(const rfmtid: REFFMTID;
                  grfMode: DWORD;
                  var ppprstg: IPropertyStorage): HResult; stdcall;

    function Delete(const rfmtid: REFFMTID): HResult; stdcall;

    function Enum(out ppenum: IEnumSTATPROPSETSTG): HResult; stdcall;

  end;
  IID_IPropertySetStorage = IPropertySetStorage;
  {$EXTERNALSYM IID_IPropertySetStorage}



  // Interface IEnumSTATPROPSTG
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumSTATPROPSTG);'}
  {$EXTERNALSYM IEnumSTATPROPSTG}
  IEnumSTATPROPSTG = interface(IUnknown)
  ['{00000139-0000-0000-C000-000000000046}']
    function Next(celt: ULONG;
                  rgelt: STATPROPSTG;
                  out pceltFetched: ULONG): HResult; stdcall;

    function Skip(celt: ULONG): HResult; stdcall;

    function Reset(): HResult; stdcall;

    function Clone(out ppenum: IEnumSTATPROPSTG): HResult; stdcall;

  end;
  IID_IEnumSTATPROPSTG = IEnumSTATPROPSTG;
  {$EXTERNALSYM IID_IEnumSTATPROPSTG}



  // Interface IEnumSTATPROPSETSTG
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumSTATPROPSETSTG);'}
  {$EXTERNALSYM IEnumSTATPROPSETSTG}
  IEnumSTATPROPSETSTG = interface(IUnknown)
  ['{00000139-0000-0000-C000-000000000046}']
    function Next(celt: ULONG;
                  out rgelt: STATPROPSETSTG;
                  out pceltFetched: ULONG): HResult; stdcall;

    function Skip(celt: ULONG): HResult; stdcall;

    function Reset(): HResult; stdcall;

    function Clone(out ppenum: IEnumSTATPROPSETSTG): HResult; stdcall;

  end;
  IID_IEnumSTATPROPSETSTG = IEnumSTATPROPSETSTG;
  {$EXTERNALSYM IID_IEnumSTATPROPSETSTG}



  // functions /////////////////////////////////////////////////////////////////

  // Macros for parsing the OS Version of the Property Set Header
  function PROPSETHDR_OSVER_KIND(dwOSVer: DWORD): DWORD;
  {$EXTERNALSYM PROPSETHDR_OSVER_KIND}
  function PROPSETHDR_OSVER_MAJOR(dwOSVer: DWORD): DWORD;
  {$EXTERNALSYM PROPSETHDR_OSVER_MAJOR}
  function PROPSETHDR_OSVER_MINOR(dwOSVer: DWORD): DWORD;
  {$EXTERNALSYM PROPSETHDR_OSVER_MINOR}




  // The PropVariantCopy function copies the contents of one PROPVARIANT structure to another.
  //
  // Remarks
  //   Copies a PROPVARIANT structure by value so the original pvarSrc and new
  //   pvarDest parameters may be freed independently with calls to PropVariantClear.
  //   PropVariantCopy does not free the destination as the VariantCopy function does.
  //   For nonsimple PROPVARIANT types such as VT_STREAM, VT_STORAGE, and so forth,
  //   which require a subobject, the copy is made by reference.
  //   The pointer is copied and IUnknown.AddRef is called on it.
  //   It is illegal to pass Nil for either pvarDest or pvarSrc.
  function PropVariantCopy(var pvarDest: PROPVARIANT;
                           var pvarSrc: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PropVariantCopy}

  // The PropVariantClear function frees all elements that can be freed in a
  // given PROPVARIANT structure.
  // For complex elements with known element pointers,
  // the underlying elements are freed prior to freeing the containing element.
  //
  // Remarks
  //   At any level of indirection, NULL pointers are ignored.
  //   For example, the pvar parameter points to a PROPVARIANT structure of type VT_CF.
  //   The pclipdata member of the PROPVARIANT structure points to a CLIPDATA structure.
  //   The pClipData pointer in the CLIPDATA structure is NULL.
  //   In this example, the pClipData pointer is ignored.
  //   However, the CLIPDATA structure pointed to by the pclipdata member of the
  //   PROPVARIANT structure is freed.
  //
  //   On return, this function writes zeroes to the specified PROPVARIANT structure,
  //   so the VT-type is VT_EMPTY.
  //   Passing NULL as the pvar parameter produces a return code of S_OK.
  //
  // Note
  //   Do not use this function to initialize PROPVARIANT structures.
  //   Instead, initialize these structures using the PropVariantInit procedure (defined in this unit).
  function PropVariantClear(var pvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PropVariantClear}

  //
  function FreePropVariantArray(cVariants: ULONG;
                                rgvars: PPROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM FreePropVariantArray}


  // Not included in Ole32.dll or defined in this unit,
  // This function (and PropVariantClear) are defined in PropSys.pas
  //procedure PropVariantInit(out pv: TMfPPROPVARIANT);


  function StgCreatePropStg(pUnk: IUnknown;
                            const fmtid: REFFMTID;
                            const pclsid: CLSID;
                            grfFlags: DWORD;
                            dwReserved: DWORD;
                            out ppPropStg: IPropertyStorage): HResult; stdcall;
  {$EXTERNALSYM StgCreatePropStg}

  function StgOpenPropStg(pUnk: IUnknown;
                          const fmtid: REFFMTID;
                          grfFlags: DWORD;
                          dwReserved: DWORD;
                          out ppPropStg: IPropertyStorage): HResult; stdcall;
  {$EXTERNALSYM StgOpenPropStg}

  function StgCreatePropSetStg(pStorage: IStorage;
                               dwReserved: DWORD;
                               out ppPropSetStg: IPropertySetStorage): HResult; stdcall;
  {$EXTERNALSYM StgCreatePropSetStg}

  function FmtIdToPropStgName(const pfmtid: REFFMTID;
                              out oszName: POLESTR): HResult; stdcall;
  {$EXTERNALSYM FmtIdToPropStgName}

  function PropStgNameToFmtId(oszName: POLESTR;
                              out pfmtid: REFFMTID): HResult; stdcall;
  {$EXTERNALSYM PropStgNameToFmtId}

  function StgConvertVariantToProperty(pvar: PROPVARIANT;
                                       CodePage: USHORT;
                                       out pprop: SERIALIZEDPROPERTYVALUE;
                                       pcb: PULONG;
                                       const pid: PROPID;
                                       __reserved: BOOLEAN;
                                       pcIndirect: PULONG): SERIALIZEDPROPERTYVALUE; stdcall;
  {$EXTERNALSYM StgConvertVariantToProperty}

  function StgConvertPropertyToVariant(prop: SERIALIZEDPROPERTYVALUE;
                                       CodePage: USHORT;
                                       out pvar: PROPVARIANT;
                                       pma: PMemoryAllocator): BOOLEAN; stdcall;
  {$EXTERNALSYM StgConvertPropertyToVariant}



  // Additional Prototypes for ALL interfaces

  // Forward functions
  procedure PropVariantInit(var pv: PROPVARIANT); inline;
  procedure PropVariantClearSafe(var pv: PROPVARIANT); inline;
  //procedure PropVariantClear(var pv: TMfPROPVARIANT);

  // end of Additional Prototypes

implementation

const
  PropIdlLib = 'Ole32.dll';

  // Macros for parsing the OS Version of the Property Set Header
  function PROPSETHDR_OSVER_KIND(dwOSVer: DWORD): DWORD;
  begin
    Result:= HIWORD(dwOSVer);
  end;

  function PROPSETHDR_OSVER_MAJOR(dwOSVer: DWORD): DWORD;
  begin
    Result:= LOBYTE(LOWORD(dwOSVer));
  end;

  function PROPSETHDR_OSVER_MINOR(dwOSVer: DWORD): DWORD;
  begin
    Result:= HIBYTE(LOWORD(dwOSVer));
  end;



  //--------------------- External definitions ---------------------------------
  
  procedure PropVariantInit(var pv: PROPVARIANT);
    begin
      FillChar(pv, sizeof(PROPVARIANT), 0);
    end;

  procedure PropVariantClearSafe(var pv: PROPVARIANT);
    begin
      ZeroMemory(@pv, SizeOf(pv));
    end;
    
{$WARN SYMBOL_PLATFORM OFF}
  function PropVariantCopy;             external PropIdlLib name 'PropVariantCopy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PropVariantClear;            external PropIdlLib name 'PropVariantClear' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function FreePropVariantArray;        external PropIdlLib name 'FreePropVariantArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function StgCreatePropStg;            external PropIdlLib name 'StgCreatePropStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StgOpenPropStg;              external PropIdlLib name 'StgOpenPropStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StgCreatePropSetStg;         external PropIdlLib name 'StgCreatePropSetStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function FmtIdToPropStgName;          external PropIdlLib name 'FmtIdToPropStgName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PropStgNameToFmtId;          external PropIdlLib name 'PropStgNameToFmtId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StgConvertVariantToProperty; external PropIdlLib name 'StgConvertVariantToProperty' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StgConvertPropertyToVariant; external PropIdlLib name 'StgConvertPropertyToVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.

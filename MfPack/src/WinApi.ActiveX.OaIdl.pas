// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.OaIdl.pas
// Kind: Pascal / Delphi unit
// Release date: 17-02-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This header is used by Automation.
//              For more information, see: https://docs.microsoft.com/en-us/windows/desktop/api/_automat
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
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
// =============================================================================
// Source: OAIdl.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// =============================================================================
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
// =============================================================================
unit WinApi.ActiveX.OaIdl;

  {$HPPEMIT '#include "OAIdl.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  {ActiveX}
  WinApi.ActiveX.ObjIdlbase;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  FADF_AUTO                         = ($1);
  {$EXTERNALSYM FADF_AUTO}
  FADF_STATIC                       = ($2);
  {$EXTERNALSYM FADF_STATIC}
  FADF_EMBEDDED                     = ($4);
  {$EXTERNALSYM FADF_EMBEDDED}
  FADF_FIXEDSIZE                    = ($10);
  {$EXTERNALSYM FADF_FIXEDSIZE}
  FADF_RECORD                       = ($20);
  {$EXTERNALSYM FADF_RECORD}
  FADF_HAVEIID                      = ($40);
  {$EXTERNALSYM FADF_HAVEIID}
  FADF_HAVEVARTYPE                  = ($80);
  {$EXTERNALSYM FADF_HAVEVARTYPE}
  FADF_BSTR                         = ($100);
  {$EXTERNALSYM FADF_BSTR}
  FADF_UNKNOWN                      = ($200);
  {$EXTERNALSYM FADF_UNKNOWN}
  FADF_DISPATCH                     = ($400);
  {$EXTERNALSYM FADF_DISPATCH}
  FADF_VARIANT                      = ($800);
  {$EXTERNALSYM FADF_VARIANT}
  FADF_RESERVED                     = ($F008);
  {$EXTERNALSYM FADF_RESERVED}

  PARAMFLAG_NONE                    = (0);
  {$EXTERNALSYM PARAMFLAG_NONE}
  PARAMFLAG_FIN                     = ($1);
  {$EXTERNALSYM PARAMFLAG_FIN}
  PARAMFLAG_FOUT                    = ($2);
  {$EXTERNALSYM PARAMFLAG_FOUT}
  PARAMFLAG_FLCID                   = ($4);
  {$EXTERNALSYM PARAMFLAG_FLCID}
  PARAMFLAG_FRETVAL                 = ($8);
  {$EXTERNALSYM PARAMFLAG_FRETVAL}
  PARAMFLAG_FOPT                    = ($10);
  {$EXTERNALSYM PARAMFLAG_FOPT}
  PARAMFLAG_FHASDEFAULT             = ($20);
  {$EXTERNALSYM PARAMFLAG_FHASDEFAULT}
  PARAMFLAG_FHASCUSTDATA            = ($40);
  {$EXTERNALSYM PARAMFLAG_FHASCUSTDATA}

  IDLFLAG_NONE                      = (PARAMFLAG_NONE);
  {$EXTERNALSYM IDLFLAG_NONE}
  IDLFLAG_FIN                       = (PARAMFLAG_FIN);
  {$EXTERNALSYM IDLFLAG_FIN}
  IDLFLAG_FOUT                      = (PARAMFLAG_FOUT);
  {$EXTERNALSYM IDLFLAG_FOUT}
  IDLFLAG_FLCID                     = (PARAMFLAG_FLCID);
  {$EXTERNALSYM IDLFLAG_FLCID}
  IDLFLAG_FRETVAL                   = (PARAMFLAG_FRETVAL);
  {$EXTERNALSYM IDLFLAG_FRETVAL}

  IMPLTYPEFLAG_FDEFAULT               = ($1);
  {$EXTERNALSYM IMPLTYPEFLAG_FDEFAULT}
  IMPLTYPEFLAG_FSOURCE                = ($2);
  {$EXTERNALSYM IMPLTYPEFLAG_FSOURCE}
  IMPLTYPEFLAG_FRESTRICTED            = ($4);
  {$EXTERNALSYM IMPLTYPEFLAG_FRESTRICTED}
  IMPLTYPEFLAG_FDEFAULTVTABLE         = ($8);
  {$EXTERNALSYM IMPLTYPEFLAG_FDEFAULTVTABLE}

  // DISPID reserved to indicate an "unknown" name
  // only reserved for data members (properties); reused as a method dispid below
  DISPID_UNKNOWN                      = (-1);
  {$EXTERNALSYM DISPID_UNKNOWN}
  // DISPID reserved for the "value" property
  DISPID_VALUE                        = (0);
  {$EXTERNALSYM DISPID_VALUE}
  //The following DISPID is reserved to indicate the param
  // that is the right-hand-side (or "put" value) of a PropertyPut
  DISPID_PROPERTYPUT                  = (-3);
  {$EXTERNALSYM DISPID_PROPERTYPUT}
  // DISPID reserved for the standard "NewEnum" method
  DISPID_NEWENUM                      = (-4);
  {$EXTERNALSYM DISPID_NEWENUM}
  // DISPID reserved for the standard "Evaluate" method
  DISPID_EVALUATE                     = (-5);
  {$EXTERNALSYM DISPID_EVALUATE}
  DISPID_CONSTRUCTOR                  = (-6);
  {$EXTERNALSYM DISPID_CONSTRUCTOR}
  DISPID_DESTRUCTOR                   = (-7);
  {$EXTERNALSYM DISPID_DESTRUCTOR}
  DISPID_COLLECT                      = (-8);
  {$EXTERNALSYM DISPID_COLLECT}

type
  PTYPEFLAGS = ^tagTYPEFLAGS;
  tagTYPEFLAGS = DWord;
  {$EXTERNALSYM tagTYPEFLAGS}
  TYPEFLAGS = tagTYPEFLAGS;
  {$EXTERNALSYM TYPEFLAGS}
const
  TYPEFLAG_FAPPOBJECT     = TYPEFLAGS($1);
  {$EXTERNALSYM TYPEFLAG_FAPPOBJECT}
  TYPEFLAG_FCANCREATE     = TYPEFLAGS($2);
  {$EXTERNALSYM TYPEFLAG_FCANCREATE}
  TYPEFLAG_FLICENSED      = TYPEFLAGS($4);
  {$EXTERNALSYM TYPEFLAG_FLICENSED}
  TYPEFLAG_FPREDECLID     = TYPEFLAGS($8);
  {$EXTERNALSYM TYPEFLAG_FPREDECLID}
  TYPEFLAG_FHIDDEN        = TYPEFLAGS($10);
  {$EXTERNALSYM TYPEFLAG_FHIDDEN}
  TYPEFLAG_FCONTROL       = TYPEFLAGS($20);
  {$EXTERNALSYM TYPEFLAG_FCONTROL}
  TYPEFLAG_FDUAL          = TYPEFLAGS($40);
  {$EXTERNALSYM TYPEFLAG_FDUAL}
  TYPEFLAG_FNONEXTENSIBLE = TYPEFLAGS($80);
  {$EXTERNALSYM TYPEFLAG_FNONEXTENSIBLE}
  TYPEFLAG_FOLEAUTOMATION = TYPEFLAGS($100);
  {$EXTERNALSYM TYPEFLAG_FOLEAUTOMATION}
  TYPEFLAG_FRESTRICTED    = TYPEFLAGS($200);
  {$EXTERNALSYM TYPEFLAG_FRESTRICTED}
  TYPEFLAG_FAGGREGATABLE  = TYPEFLAGS($400);
  {$EXTERNALSYM TYPEFLAG_FAGGREGATABLE}
  TYPEFLAG_FREPLACEABLE   = TYPEFLAGS($800);
  {$EXTERNALSYM TYPEFLAG_FREPLACEABLE}
  TYPEFLAG_FDISPATCHABLE  = TYPEFLAGS($1000);
  {$EXTERNALSYM TYPEFLAG_FDISPATCHABLE}
  TYPEFLAG_FREVERSEBIND   = TYPEFLAGS($2000);
  {$EXTERNALSYM TYPEFLAG_FREVERSEBIND}
  TYPEFLAG_FPROXY         = TYPEFLAGS($4000);
  {$EXTERNALSYM TYPEFLAG_FPROXY}

type
  PFUNCFLAGS = ^tagFUNCFLAGS;
  tagFUNCFLAGS = DWord;
  {$EXTERNALSYM tagFUNCFLAGS}
  FUNCFLAGS = tagFUNCFLAGS;
  {$EXTERNALSYM FUNCFLAGS}
const
  FUNCFLAG_FRESTRICTED       = FUNCFLAGS($1);
  {$EXTERNALSYM FUNCFLAG_FRESTRICTED}
  FUNCFLAG_FSOURCE           = FUNCFLAGS($2);
  {$EXTERNALSYM FUNCFLAG_FSOURCE}
  FUNCFLAG_FBINDABLE         = FUNCFLAGS($4);
  {$EXTERNALSYM FUNCFLAG_FBINDABLE}
  FUNCFLAG_FREQUESTEDIT      = FUNCFLAGS($8);
  {$EXTERNALSYM FUNCFLAG_FREQUESTEDIT}
  FUNCFLAG_FDISPLAYBIND      = FUNCFLAGS($10);
  {$EXTERNALSYM FUNCFLAG_FDISPLAYBIND}
  FUNCFLAG_FDEFAULTBIND      = FUNCFLAGS($20);
  {$EXTERNALSYM FUNCFLAG_FDEFAULTBIND}
  FUNCFLAG_FHIDDEN           = FUNCFLAGS($40);
  {$EXTERNALSYM FUNCFLAG_FHIDDEN}
  FUNCFLAG_FUSESGETLASTERROR = FUNCFLAGS($80);
  {$EXTERNALSYM FUNCFLAG_FUSESGETLASTERROR}
  FUNCFLAG_FDEFAULTCOLLELEM  = FUNCFLAGS($100);
  {$EXTERNALSYM FUNCFLAG_FDEFAULTCOLLELEM}
  FUNCFLAG_FUIDEFAULT        = FUNCFLAGS($200);
  {$EXTERNALSYM FUNCFLAG_FUIDEFAULT}
  FUNCFLAG_FNONBROWSABLE     = FUNCFLAGS($400);
  {$EXTERNALSYM FUNCFLAG_FNONBROWSABLE}
  FUNCFLAG_FREPLACEABLE      = FUNCFLAGS($800);
  {$EXTERNALSYM FUNCFLAG_FREPLACEABLE}
  FUNCFLAG_FIMMEDIATEBIND    = FUNCFLAGS($1000);
  {$EXTERNALSYM FUNCFLAG_FIMMEDIATEBIND}

type
  PVARFLAGS = ^tagVARFLAGS;
  tagVARFLAGS                = DWord;
  {$EXTERNALSYM tagVARFLAGS}
  VARFLAGS = tagVARFLAGS;
  {$EXTERNALSYM VARFLAGS}
const
  VARFLAG_FREADONLY        = VARFLAGS($1);
  {$EXTERNALSYM VARFLAG_FREADONLY}
  VARFLAG_FSOURCE          = VARFLAGS($2);
  {$EXTERNALSYM VARFLAG_FSOURCE}
  VARFLAG_FBINDABLE        = VARFLAGS($4);
  {$EXTERNALSYM VARFLAG_FBINDABLE}
  VARFLAG_FREQUESTEDIT     = VARFLAGS($8);
  {$EXTERNALSYM VARFLAG_FREQUESTEDIT}
  VARFLAG_FDISPLAYBIND     = VARFLAGS($10);
  {$EXTERNALSYM VARFLAG_FDISPLAYBIND}
  VARFLAG_FDEFAULTBIND     = VARFLAGS($20);
  {$EXTERNALSYM VARFLAG_FDEFAULTBIND}
  VARFLAG_FHIDDEN          = VARFLAGS($40);
  {$EXTERNALSYM VARFLAG_FHIDDEN}
  VARFLAG_FRESTRICTED      = VARFLAGS($80);
  {$EXTERNALSYM VARFLAG_FRESTRICTED}
  VARFLAG_FDEFAULTCOLLELEM = VARFLAGS($100);
  {$EXTERNALSYM VARFLAG_FDEFAULTCOLLELEM}
  VARFLAG_FUIDEFAULT       = VARFLAGS($200);
  {$EXTERNALSYM VARFLAG_FUIDEFAULT}
  VARFLAG_FNONBROWSABLE    = VARFLAGS($400);
  {$EXTERNALSYM VARFLAG_FNONBROWSABLE}
  VARFLAG_FREPLACEABLE     = VARFLAGS($800);
  {$EXTERNALSYM VARFLAG_FREPLACEABLE}
  VARFLAG_FIMMEDIATEBIND   = VARFLAGS($1000);
  {$EXTERNALSYM VARFLAG_FIMMEDIATEBIND}

type
  PLIBFLAGS = ^tagLIBFLAGS;
  tagLIBFLAGS = Dword;
  {$EXTERNALSYM tagLIBFLAGS}
  LIBFLAGS = tagLIBFLAGS;
  {$EXTERNALSYM LIBFLAGS}
const
  LIBFLAG_FRESTRICTED   = LIBFLAGS($1);
  {$EXTERNALSYM LIBFLAG_FRESTRICTED}
  LIBFLAG_FCONTROL      = LIBFLAGS($2);
  {$EXTERNALSYM LIBFLAG_FCONTROL}
  LIBFLAG_FHIDDEN       = LIBFLAGS($4);
  {$EXTERNALSYM LIBFLAG_FHIDDEN}
  LIBFLAG_FHASDISKIMAGE = LIBFLAGS($8);
  {$EXTERNALSYM LIBFLAG_FHASDISKIMAGE}


type

  // Forward Interface declarations

  PITypeInfo = ^ITypeInfo;
  ITypeInfo = Interface;

  PITypeLib = ^ITypeLib;
  LPTYPELIB = PITypeLib;
  ITypeLib = Interface;
  {$EXTERNALSYM ITypeLib}

  PITypeChangeEvents = ^ITypeChangeEvents;
  LPTYPECHANGEEVENTS = PITypeChangeEvents;
  {$EXTERNALSYM LPTYPECHANGEEVENTS}
  ITypeChangeEvents = Interface;
  {$EXTERNALSYM ITypeChangeEvents}

  PIRecordInfo = ^IRecordInfo;
  IRecordInfo = Interface;
  {$EXTERNALSYM IRecordInfo}

  LPTYPECOMP = ^ITypeComp;
  {$EXTERNALSYM LPTYPECOMP}


{$IFNDEF _CURRENCY_DEFINED}
{$DEFINE _CURRENCY_DEFINED}
  P_CURRENCY = ^CY;
  _CURRENCY = CY;
  {$EXTERNALSYM _CURRENCY}
{$ENDIF}

  PSAFEARRAYBOUND = ^tagSAFEARRAYBOUND;
  tagSAFEARRAYBOUND = record
    cElements: LongWord;
    lLbound: Longint;
  end;
  {$EXTERNALSYM tagSAFEARRAYBOUND}
  SAFEARRAYBOUND = tagSAFEARRAYBOUND;
  {$EXTERNALSYM SAFEARRAYBOUND}

  // See: https://docs.microsoft.com/en-us/archive/msdn-magazine/2017/march/introducing-the-safearray-data-structure
  PSAFEARRAY = ^tagSAFEARRAY;
  tagSAFEARRAY = record
    cDims: Word;
    fFeatures: Word;
    cbElements: LongWord;
    cLocks: LongWord;
    pvData: Pointer;
    rgsabound: array[0..0] of SAFEARRAYBOUND;
  end;
  {$EXTERNALSYM tagSAFEARRAY}
  SAFEARRAY = tagSAFEARRAY;
  {$EXTERNALSYM SAFEARRAY}


  PSAFEARR_BSTR = ^_wireSAFEARR_BSTR;
  _wireSAFEARR_BSTR = record
    Size: ULONG;
    aBstr: PwireBSTR;
  end;
  {$EXTERNALSYM _wireSAFEARR_BSTR}
  SAFEARR_BSTR = _wireSAFEARR_BSTR;
  {$EXTERNALSYM SAFEARR_BSTR}

  PSAFEARR_UNKNOWN = ^_wireSAFEARR_UNKNOWN;
  _wireSAFEARR_UNKNOWN = record
    Size: ULONG;
    apUnknown: PIUnknown;
  end;
  {$EXTERNALSYM _wireSAFEARR_UNKNOWN}
  SAFEARR_UNKNOWN = _wireSAFEARR_UNKNOWN;
  {$EXTERNALSYM SAFEARR_UNKNOWN}

  PSAFEARR_DISPATCH = ^_wireSAFEARR_DISPATCH;
  _wireSAFEARR_DISPATCH = record
    Size: ULONG;
    apDispatch: PIDispatch;
  end;
  {$EXTERNALSYM _wireSAFEARR_DISPATCH}
  SAFEARR_DISPATCH = _wireSAFEARR_DISPATCH;
  {$EXTERNALSYM SAFEARR_DISPATCH}

  PSAFEARR_HAVEIID = ^_wireSAFEARR_HAVEIID;
  _wireSAFEARR_HAVEIID = record
    Size: ULONG;
    apUnknown: PIUnknown;
    iid: IID;
  end;
  {$EXTERNALSYM _wireSAFEARR_HAVEIID}
  SAFEARR_HAVEIID = _wireSAFEARR_HAVEIID;
  {$EXTERNALSYM SAFEARR_HAVEIID}


  PSF_TYPE = ^tagSF_TYPE;
  {$EXTERNALSYM PSF_TYPE}
  tagSF_TYPE    = (
    SF_ERROR    = VT_ERROR,
    SF_I1       = VT_I1,
    SF_I2       = VT_I2,
    SF_I4       = VT_I4,
    SF_I8       = VT_I8,
    SF_BSTR     = VT_BSTR,
    SF_UNKNOWN  = VT_UNKNOWN,
    SF_DISPATCH = VT_DISPATCH,
    SF_VARIANT  = VT_VARIANT,
    SF_RECORD   = VT_RECORD,
    SF_HAVEIID  = (VT_UNKNOWN or VT_RESERVED)
  );
  {$EXTERNALSYM tagSF_TYPE}
  SF_TYPE = tagSF_TYPE;
  {$EXTERNALSYM SF_TYPE}


  // VARIANT STRUCTURE
  // =================

  PVARIANT = ^VARIANT;
  PVARIANTARG = ^VARIANTARG;
  tagVARIANT = record
    vt: VARTYPE;
    wReserved1: Word;
    wReserved2: Word;
    wReserved3: Word;
      case VARTYPE of
        VT_UI1:                  (bVal: Byte);
        VT_I2:                   (iVal: Smallint);
        VT_I4:                   (lVal: Longint);
        VT_R4:                   (fltVal: Single);
        VT_R8:                   (dblVal: Double);
        VT_BOOL:                 (vbool: WordBool);
        VT_ERROR:                (scode: HResult);
        VT_CY:                   (cyVal: Currency);
        VT_DATE:                 (date: OLE_DATE);
        VT_BSTR:                 (bstrVal: PWideChar); //WideString
        VT_UNKNOWN:              (unkVal: Pointer); //IUnknown
        VT_DISPATCH:             (dispVal: Pointer); //IDispatch
        VT_ARRAY:                (parray: PSafeArray);
        VT_BYREF or VT_UI1:      (pbVal: PByte);
        VT_BYREF or VT_I2:       (piVal: PSmallint);
        VT_BYREF or VT_I4:       (plVal: PLongint);
        VT_BYREF or VT_R4:       (pfltVal: PSingle);
        VT_BYREF or VT_R8:       (pdblVal: PDouble);
        VT_BYREF or VT_BOOL:     (pbool: POLE_BOOL);
        VT_BYREF or VT_ERROR:    (pscode: ^HResult);
        VT_BYREF or VT_CY:       (pcyVal: PCurrency);
        VT_BYREF or VT_DATE:     (pdate: POLE_DATE);
        VT_BYREF or VT_BSTR:     (pbstrVal: PWideString);
        VT_BYREF or VT_UNKNOWN:  (punkVal: Pointer {IUnknown});
        VT_BYREF or VT_DISPATCH: (pdispVal: Pointer {IDispatch});
        VT_BYREF or VT_ARRAY:    (pparray: PSafeArray);
        VT_BYREF or VT_VARIANT:  (pvarVal: PVariant);
        VT_BYREF:                (byRef: Pointer);
        VT_I1:                   (cVal: AnsiChar);
        VT_UI2:                  (uiVal: Word);
        VT_UI4:                  (ulVal: LongWord);
        VT_I8:                   (llVal : Int64);
        VT_UI8:                  (ullVal : UInt64);
        VT_INT:                  (intVal: Integer);
        VT_UINT:                 (uintVal: LongWord);
        VT_BYREF or VT_DECIMAL:  (pdecVal: PDecimal);
        VT_BYREF or VT_I1:       (pcVal: PAnsiChar);
        VT_BYREF or VT_UI2:      (puiVal: PWord);
        VT_BYREF or VT_UI4:      (pulVal: PInteger);
        VT_BYREF or VT_INT:      (pintVal: PInteger);
        VT_BYREF or VT_UINT:     (puintVal: PLongWord);
        VT_BYREF or VT_I8:       (pllVal : PInt64);
        VT_BYREF or VT_UI8:      (pullVal : PUInt64);
        VT_RECORD:               (pvRecord : Pointer;
                                  pRecInfo : Pointer);

  end;
  {$EXTERNALSYM tagVARIANT}

  VARIANT = tagVARIANT;
  {$EXTERNALSYM VARIANT}
  VARIANTARG = tagVARIANT;
  {$EXTERNALSYM VARIANTARG}


  PwireBRECORD = ^_wireBRECORD;
  _wireBRECORD = record
    fFlags: ULONG;
    clSize: ULONG;
    pRecInfo: IRecordInfo;
   pRecord: Pbyte;
  end;
  {$EXTERNALSYM _wireBRECORD}
  wireBRECORD = _wireBRECORD;
  {$EXTERNALSYM wireBRECORD}


  PwireVARIANT = ^_wireVARIANT;

  PSAFEARR_VARIANT = ^_wireSAFEARR_VARIANT;
   _wireSAFEARR_VARIANT = record
    Size: ULONG;
    aVariant: PwireVARIANT;
  end;
   {$EXTERNALSYM _wireSAFEARR_VARIANT}
  SAFEARR_VARIANT = _wireSAFEARR_VARIANT;
  {$EXTERNALSYM SAFEARR_VARIANT}

  PSAFEARR_BRECORD = ^_wireSAFEARR_BRECORD;
  _wireSAFEARR_BRECORD = record
    Size: ULONG;
    aRecord: PwireBRECORD;
  end;
  {$EXTERNALSYM _wireSAFEARR_BRECORD}
  SAFEARR_BRECORD = _wireSAFEARR_BRECORD;
  {$EXTERNALSYM SAFEARR_BRECORD}

  PSAFEARRAYUNION = ^_wireSAFEARRAY_UNION;
  _wireSAFEARRAY_UNION = record
    sfType: ULONG;
    u : record
    case integer of
      0 : (BstrStr     : SAFEARR_BSTR);
      1 : (UnknownStr  : SAFEARR_UNKNOWN);
      2 : (DispatchStr : SAFEARR_DISPATCH);
      3 : (VariantStr  : SAFEARR_VARIANT);
      4 : (RecordStr   : SAFEARR_BRECORD);
      5 : (HaveIidStr  : SAFEARR_HAVEIID);
      6 : (ByteStr     : BYTE_SIZEDARR);
      7 : (WordStr     : WORD_SIZEDARR);
      8 : (LongStr     : DWORD_SIZEDARR);
      9 : (HyperStr    : HYPER_SIZEDARR);
    end;
  {$EXTERNALSYM _wireSAFEARRAY_UNION}
  end;
  SAFEARRAYUNION = _wireSAFEARRAY_UNION;
  {$EXTERNALSYM SAFEARRAYUNION}


  PwireSAFEARRAY = ^_wireSAFEARRAY;
  _wireSAFEARRAY = record
    cDims: USHORT;
    fFeatures: USHORT;
    cbElements: ULONG;
    cLocks: ULONG;
    uArrayStructs: SAFEARRAYUNION;
    rgsabound: array [0..0] of SAFEARRAYBOUND;
  end;
  {$EXTERNALSYM _wireSAFEARRAY}
  wireSAFEARRAY = _wireSAFEARRAY;
  {$EXTERNALSYM wireSAFEARRAY}


  _wireVARIANT = record
    clSize: DWORD;           // wire buffer length in units of hyper (int64)
    rpcReserved: DWORD;      // for future use
    vt: USHORT;
    wReserved1: USHORT;
    wReserved2: USHORT;
    wReserved3: USHORT;
    case ULONG of
      VT_I8                   : (llVal: LONGLONG);          // VT_I8
      VT_I4                   : (lVal: LONG);               // VT_I4
      VT_UI1                  : (bVal: Byte);               // VT_UI1
      VT_I2                   : (iVal: SHORT);              // VT_I2
      VT_R4                   : (fltVal: FLOAT);            // VT_R4
      VT_R8                   : (dblVal: DOUBLE; );         // VT_R8
      VT_BOOL                 : (boolVal: VARIANT_BOOL);    // VT_BOOL
      VT_ERROR                : (scode: SCODE);             // VT_ERROR
      VT_CY                   : (cyVal: CY);                // VT_CY
      VT_DATE                 : (date: DATE);               // VT_DATE
      VT_BSTR                 : (bstrVal: wireBSTR);        // VT_BSTR
      VT_UNKNOWN              : (punkVal: PIUnknown);       // VT_UNKNOWN
      VT_DISPATCH             : (pdispVal: PIDispatch);     // VT_DISPATCH
      VT_ARRAY                : (parray: PwireSAFEARRAY);   // VT_ARRAY
      VT_RECORD,
      VT_RECORD or VT_BYREF   : (brecVal: PwireBRECORD);     // VT_RECORD
      VT_UI1 or VT_BYREF      : (pbVal: PByte);             // VT_BYREF or VT_UI1
      VT_I2 or VT_BYREF       : (piVal: PSHORT);            // VT_BYREF or VT_I2
      VT_I4 or VT_BYREF       : (plVal: PLONG);             // VT_BYREF or VT_I4
      VT_I8 or VT_BYREF       : (pllVal: PLONGLONG);        // VT_BYREF or VT_I8
      VT_R4 or VT_BYREF       : (pfltVal: PFLOAT);          // VT_BYREF or VT_R4
      VT_R8 or VT_BYREF       : (pdblVal: PDOUBLE);         // VT_BYREF or VT_R8
      VT_BOOL or VT_BYREF     : (pboolVal: PVARIANT_BOOL);  // VT_BYREF or VT_BOOL
      VT_ERROR or VT_BYREF    : (pscode: PSCODE);           // VT_BYREF or VT_ERROR
      VT_CY or VT_BYREF       : (pcyVal: PCY);              // VT_BYREF or VT_CY
      VT_DATE or VT_BYREF     : (pdate: PDATE);             // VT_BYREF or VT_DATE
      VT_BSTR or VT_BYREF     : (pbstrVal: PwireBSTR);      // VT_BYREF or VT_BSTR
      VT_UNKNOWN or VT_BYREF  : (ppunkVal: PIUnknown);      // VT_BYREF or VT_UNKNOWN
      VT_DISPATCH or VT_BYREF : (ppdispVal: PIDispatch);    // VT_BYREF or VT_DISPATCH
      VT_ARRAY or VT_BYREF    : (pparray: PwireSAFEARRAY);  // VT_BYREF or VT_ARRAY
      VT_VARIANT or VT_BYREF  : (pvarVal: PwireVARIANT);    // VT_BYREF or VT_VARIANT
      VT_I1                   : (cVal: AnsiChar);           // VT_I1
      VT_UI2                  : (uiVal: USHORT);            // VT_UI2
      VT_UI4                  : (ulVal: ULONG);             // VT_UI4
      VT_UI8                  : (ullVal: ULONGLONG);        // VT_UI8
      VT_INT                  : (intVal: INT);              // VT_INT
      VT_UINT                 : (uintVal: UINT);            // VT_UINT
      VT_DECIMAL              : (decVal: DECIMAL);          // VT_DECIMAL
      VT_BYREF or VT_DECIMAL  : (pdecVal: PDECIMAL);        // VT_BYREF or VT_DECIMAL
      VT_BYREF or VT_I1       : (pcVal: PCHAR);             // VT_BYREF or VT_I1
      VT_BYREF or VT_UI2      : (puiVal: PUSHORT);          // VT_BYREF or VT_UI2
      VT_BYREF or VT_UI4      : (pulVal: PULONG);           // VT_BYREF or VT_UI4
      VT_BYREF or VT_UI8      : (pullVal: PULONGLONG);      // VT_BYREF or VT_UI8
      VT_BYREF or VT_INT      : (pintVal: PINT);            // VT_BYREF or VT_INT
      VT_BYREF or VT_UINT     : (puintVal: PUINT);          // VT_BYREF or VT_UINT
      VT_EMPTY                : ({nothing});                // nothing
      VT_NULL                 : ({nothing});                // nothing
    end;
  {$EXTERNALSYM _wireVARIANT}
  wireVARIANT = _wireVARIANT;
  {$EXTERNALSYM wireVARIANT}



//########################################################################
//     End of VARIANT & SAFEARRAY
//########################################################################


  // TypeInfo stuff.

  PDISPID  = ^DISPID;
  DISPID  = LONG; //TDISPID = LongInt
  {$EXTERNALSYM DISPID}
  MEMBERID = DISPID;
  {$EXTERNALSYM MEMBERID}
  HREFTYPE = DWORD;
  {$EXTERNALSYM HREFTYPE}


  // Describes the type of a variable, the return type of a function, or the type of a function parameter.
  PArrayDesc = ^ARRAYDESC;
  PTypeDesc = ^tagTYPEDESC;
  tagTYPEDESC = record
    case integer of
      VT_PTR:         (ptdesc: PTypeDesc; vt: TVarType);
      VT_CARRAY:      (padesc: PArrayDesc);
      VT_USERDEFINED: (hreftype: HRefType);
  end;
  {$EXTERNALSYM tagTYPEDESC}
  TYPEDESC = tagTYPEDESC;
  {$EXTERNALSYM TYPEDESC}


  PTYPEKIND = ^tagTYPEKIND;
  tagTYPEKIND       = (
    TKIND_ENUM      = 0,
    TKIND_RECORD    = (TKIND_ENUM + 1),
    TKIND_MODULE    = (TKIND_RECORD + 1),
    TKIND_Interface = (TKIND_MODULE + 1),
    TKIND_DISPATCH  = (TKIND_Interface + 1),
    TKIND_COCLASS   = (TKIND_DISPATCH + 1),
    TKIND_ALIAS     = (TKIND_COCLASS + 1),
    TKIND_UNION     = (TKIND_ALIAS + 1),
    TKIND_MAX       = (TKIND_UNION + 1));
  {$EXTERNALSYM tagTYPEKIND}
  TYPEKIND = tagTYPEKIND;
  {$EXTERNALSYM TYPEKIND}

  PPARAMDESCEX = ^PARAMDESCEX;
  LPPARAMDESCEX = ^PARAMDESCEX;
  {$EXTERNALSYM LPPARAMDESCEX}
  tagPARAMDESCEX = record
    cBytes: ULONG;
    varDefaultValue: VARIANTARG;
  end;
  {$EXTERNALSYM tagPARAMDESCEX}
  PARAMDESCEX = tagPARAMDESCEX;
  {$EXTERNALSYM PARAMDESCEX}


  PPARAMDESC = ^PARAMDESC;
  LPPARAMDESC = ^PARAMDESC;
  {$EXTERNALSYM LPPARAMDESC}
  tagPARAMDESC = record
    pparamdescex: LPPARAMDESCEX;
    wParamFlags: USHORT;
  end;
  {$EXTERNALSYM tagPARAMDESC}
  PARAMDESC = tagPARAMDESC;
  {$EXTERNALSYM PARAMDESC}


  PIDLDESC = ^tagIDLDESC;
  LPIDLDESC = ^tagIDLDESC;
  {$EXTERNALSYM LPIDLDESC}
  tagIDLDESC = record
    dwReserved: ULONG_PTR;
    wIDLFlags: USHORT;
  end;
  {$EXTERNALSYM tagIDLDESC}
  IDLDESC = tagIDLDESC;
  {$EXTERNALSYM IDLDESC}

  PELEMDESC = ^tagELEMDESC;
  tagELEMDESC = record
    tdesc: TYPEDESC;
    paramdesc: PARAMDESC;
  end;
  {$EXTERNALSYM tagELEMDESC}
  ELEMDESC = tagELEMDESC;
  {$EXTERNALSYM ELEMDESC}

  tagARRAYDESC = record
    tdescElem: TYPEDESC;
    cDims: USHORT;
    rgbounds: array[0..0] of SAFEARRAYBOUND;
  end;
  {$EXTERNALSYM tagARRAYDESC}
  ARRAYDESC = tagARRAYDESC;
  {$EXTERNALSYM ARRAYDESC}


  PTYPEATTR = ^tagTYPEATTR;
  LPTYPEATTR = ^tagTYPEATTR;
  {$EXTERNALSYM LPTYPEATTR}
  tagTYPEATTR = record
    guid: TGUID;
    lcid: LCID;
    dwReserved: DWORD;
    memidConstructor: MEMBERID;
    memidDestructor: MEMBERID;
    lpstrSchema: LPOLESTR;
    cbSizeInstance: ULONG;
    typekind: TYPEKIND;
    cFuncs: WORD;
    cVars: WORD;
    cImplTypes: WORD;
    cbSizeVft: WORD;
    cbAlignment: WORD;
    wTypeFlags: WORD;
    wMajorVerNum: WORD;
    wMinorVerNum: WORD;
    tdescAlias: TYPEDESC;
    idldescType: IDLDESC;
  end;
  {$EXTERNALSYM tagTYPEATTR}
  TYPEATTR = tagTYPEATTR;
  {$EXTERNALSYM TYPEATTR}


  PDISPPARAMS = ^tagDISPPARAMS;
  tagDISPPARAMS = record
    rgvarg: PVARIANTARG;
    rgdispidNamedArgs: PDISPID;
    cArgs: UINT;
    cNamedArgs: UINT;
  end;
  {$EXTERNALSYM tagDISPPARAMS}
  DISPPARAMS = tagDISPPARAMS;
  {$EXTERNALSYM DISPPARAMS}


  PEXCEPINFO = ^tagEXCEPINFO;
  tagEXCEPINFO = record
    wCode: WORD;
    wReserved: WORD;
    bstrSource: BSTR;
    bstrDescription: BSTR;
    bstrHelpFile: BSTR;
    dwHelpContext: DWORD;
    pvReserved: ULONG_PTR;
    pfnDeferredFillIn: ULONG_PTR;
    scode: SCODE;
  end;
  {$EXTERNALSYM tagEXCEPINFO}
  EXCEPINFO = tagEXCEPINFO;
  {$EXTERNALSYM EXCEPINFO}


  PCALLCONV = ^tagCALLCONV;
  tagCALLCONV     = (
    CC_FASTCALL   = 0,
    CC_CDECL      = 1,
    CC_MSCPASCAL  = (CC_CDECL + 1),
    CC_PASCAL     = CC_MSCPASCAL,
    CC_MACPASCAL  = (CC_PASCAL + 1),
    CC_STDCALL    = (CC_MACPASCAL + 1),
    CC_FPFASTCALL = (CC_STDCALL + 1),
    CC_SYSCALL    = (CC_FPFASTCALL + 1),
    CC_MPWCDECL   = (CC_SYSCALL + 1),
    CC_MPWPASCAL  = (CC_MPWCDECL + 1),
    CC_MAX        = (CC_MPWPASCAL + 1)
  );
  {$EXTERNALSYM tagCALLCONV}
  CALLCONV = tagCALLCONV;
  {$EXTERNALSYM CALLCONV}


  PFUNCKIND = ^tagFUNCKIND;
  tagFUNCKIND        = (
    FUNC_VIRTUAL     = 0,
    FUNC_PUREVIRTUAL = (FUNC_VIRTUAL + 1),
    FUNC_NONVIRTUAL  = (FUNC_PUREVIRTUAL + 1),
    FUNC_STATIC      = (FUNC_NONVIRTUAL + 1),
    FUNC_DISPATCH    = (FUNC_STATIC + 1)
  );
  {$EXTERNALSYM tagFUNCKIND}
  FUNCKIND = tagFUNCKIND;
  {$EXTERNALSYM FUNCKIND}


  PINVOKEKIND = ^tagINVOKEKIND;
  tagINVOKEKIND           = (
    INVOKE_FUNC           = 1,
    INVOKE_PROPERTYGET    = 2,
    INVOKE_PROPERTYPUT    = 4,
    INVOKE_PROPERTYPUTREF = 8
  );
  {$EXTERNALSYM tagINVOKEKIND}
  INVOKEKIND = tagINVOKEKIND;
  {$EXTERNALSYM INVOKEKIND}


  PFUNCDESC = ^tagFUNCDESC;
  LPFUNCDESC = ^tagFUNCDESC;
  tagFUNCDESC = record
    memid: MEMBERID;
    lprgscode: PSCODE;
    lprgelemdescParam: PELEMDESC;
    funckind: FUNCKIND;
    invkind: INVOKEKIND;
    callconv: CALLCONV;
    cParams: SHORT;
    cParamsOpt: SHORT;
    oVft: SHORT;
    cScodes: SHORT;
    elemdescFunc: ELEMDESC;
    wFuncFlags: WORD;
  end;
  {$EXTERNALSYM tagFUNCDESC}
  FUNCDESC = tagFUNCDESC;
  {$EXTERNALSYM FUNCDESC}


  PVARKIND = ^tagVARKIND;
  tagVARKIND        = (
    VAR_PERINSTANCE = 0,
    VAR_STATIC      = (VAR_PERINSTANCE  + 1),
    VAR_CONST       = (VAR_STATIC  + 1),
    VAR_DISPATCH    = (VAR_CONST  + 1)
  );
  {$EXTERNALSYM tagVARKIND}
  VARKIND = tagVARKIND;
  {$EXTERNALSYM VARKIND}


  PVARDESC = ^tagVARDESC;
  LPVARDESC = ^tagVARDESC;
  tagVARDESC = record
    memid: MEMBERID;
    lpstrSchema: LPOLESTR;
    DUMMYUNIONNAME : record
      case integer of
        0 : (oInst: ULONG;);
        1 : (lpvarValue: PVARIANT;);
      end;
    elemdescVar: ELEMDESC;
    wVarFlags: WORD;
    varkind: VARKIND;
  end;
  {$EXTERNALSYM tagVARDESC}
  VARDESC = tagVARDESC;
  {$EXTERNALSYM VARDESC}


  PCLEANLOCALSTORAGE = ^tagCLEANLOCALSTORAGE;
  tagCLEANLOCALSTORAGE = record
    pInterface: PIUnknown;
    pStorage: Pointer;
    flags: DWORD;
  end;
  {$EXTERNALSYM tagCLEANLOCALSTORAGE}
  CLEANLOCALSTORAGE = tagCLEANLOCALSTORAGE;
  {$EXTERNALSYM CLEANLOCALSTORAGE}


  PCUSTDATAITEM = ^tagCUSTDATAITEM;
  LPCUSTDATAITEM = ^tagCUSTDATAITEM;
  {$EXTERNALSYM LPCUSTDATAITEM}
  tagCUSTDATAITEM = record
    guid: TGUID;
    varValue: VARIANTARG;
  end;
  {$EXTERNALSYM tagCUSTDATAITEM}
  CUSTDATAITEM = tagCUSTDATAITEM;
  {$EXTERNALSYM CUSTDATAITEM}


  PCUSTDATA = ^tagCUSTDATA;
  LPCUSTDATA = ^tagCUSTDATA;
  {$EXTERNALSYM LPCUSTDATA}
  tagCUSTDATA = record
    cCustData: DWORD;
    prgCustData: LPCUSTDATAITEM;
  end;
  {$EXTERNALSYM tagCUSTDATA}
  CUSTDATA = tagCUSTDATA;
  {$EXTERNALSYM CUSTDATA}


  // The range -500 through -999 is reserved for Controls
  // The range 0x80010000 through 0x8001FFFF is reserved for Controls
  // The range -5000 through -5499 is reserved for ActiveX Accessability
  // The range -2000 through -2499 is reserved for VB5
  // The range -3900 through -3999 is reserved for Forms
  // The range -5500 through -5550 is reserved for Forms
  // The remainder of the negative DISPIDs are reserved for future use


  PDESCKIND = ^tagDESCKIND;
  tagDESCKIND               = (
    DESCKIND_NONE           = 0,
    DESCKIND_FUNCDESC       = (DESCKIND_NONE + 1),
    DESCKIND_VARDESC        = (DESCKIND_FUNCDESC + 1),
    DESCKIND_TYPECOMP       = (DESCKIND_VARDESC + 1),
    DESCKIND_IMPLICITAPPOBJ = (DESCKIND_TYPECOMP + 1),
    DESCKIND_MAX            = (DESCKIND_IMPLICITAPPOBJ + 1)
  );
  {$EXTERNALSYM tagDESCKIND}
  DESCKIND = tagDESCKIND;
  {$EXTERNALSYM DESCKIND}


  PBINDPTR = ^tagBINDPTR;
  LPBINDPTR = ^tagBINDPTR;
  {$EXTERNALSYM LPBINDPTR}
  tagBINDPTR = record
    case Integer of
      0: (lpfuncdesc: PFuncDesc);
      1: (lpvardesc: PVarDesc);
      2: (lptcomp: Pointer {ITypeComp});
  end;
  {$EXTERNALSYM tagBINDPTR}
  BINDPTR = tagBINDPTR;
  {$EXTERNALSYM BINDPTR}

  // Interface ITypeLib
  PSYSKIND = ^tagSYSKIND;
  tagSYSKIND  = (
    SYS_WIN16 = 0,
    SYS_WIN32 = (SYS_WIN16 + 1),
    SYS_MAC   = (SYS_WIN32 + 1),
    SYS_WIN64 = (SYS_MAC + 1)
  );
  {$EXTERNALSYM tagSYSKIND}
  SYSKIND = tagSYSKIND;
  {$EXTERNALSYM SYSKIND}


  PTLIBATTR = ^tagTLIBATTR;
  LPTLIBATTR = ^tagTLIBATTR;
  {$EXTERNALSYM LPTLIBATTR}
  tagTLIBATTR = record
    guid: TGUID;
    lcid: LCID;
    syskind: SYSKIND;
    wMajorVerNum: WORD;
    wMinorVerNum: WORD;
    wLibFlags: WORD;
  end;
  {$EXTERNALSYM tagTLIBATTR}
  TLIBATTR = tagTLIBATTR;
  {$EXTERNALSYM TLIBATTR}

  PCHANGEKIND = ^tagCHANGEKIND;
  tagCHANGEKIND                 = (
    CHANGEKIND_ADDMEMBER        = 0,
    CHANGEKIND_DELETEMEMBER     = (CHANGEKIND_ADDMEMBER + 1),
    CHANGEKIND_SETNAMES         = (CHANGEKIND_DELETEMEMBER + 1),
    CHANGEKIND_SETDOCUMENTATION = (CHANGEKIND_SETNAMES + 1),
    CHANGEKIND_GENERAL          = (CHANGEKIND_SETDOCUMENTATION + 1),
    CHANGEKIND_INVALIDATE       = (CHANGEKIND_GENERAL + 1),
    CHANGEKIND_CHANGEFAILED     = (CHANGEKIND_INVALIDATE + 1),
    CHANGEKIND_MAX              = (CHANGEKIND_CHANGEFAILED + 1)
  );
  {$EXTERNALSYM tagCHANGEKIND}
  CHANGEKIND = tagCHANGEKIND;
  {$EXTERNALSYM CHANGEKIND}



  // Interfaces
  // ==========

  // Interface ICreateTypeInfo
  // =========================
  //
  {$HPPEMIT 'DECLARE_DInterface_TYPE(ICreateTypeInfo);'}
  {$EXTERNALSYM ICreateTypeInfo}
  ICreateTypeInfo = Interface(IUnknown)
  ['{00020405-0000-0000-C000-000000000046}']

    function SetGuid(const guid: REFGUID): HResult; stdcall;

    function SetTypeFlags(uTypeFlags: UINT): HResult; stdcall;

    function SetDocString(pStrDoc: LPOLESTR): HResult; stdcall;

    function SetHelpContext(dwHelpContext: DWORD): HResult; stdcall;

    function SetVersion(wMajorVerNum: WORD;
                        wMinorVerNum: WORD): HResult; stdcall;

    function AddRefTypeInfo(pTInfo: ITypeInfo;
                            phRefType: HREFTYPE): HResult; stdcall;

    function AddFuncDesc(index: UINT;
                         pFuncDesc: FUNCDESC): HResult; stdcall;

    function AddImplType(index: UINT;
                         hRefType: HREFTYPE): HResult; stdcall;

    function SetImplTypeFlags(index: UINT;
                              implTypeFlags: INT): HResult; stdcall;

    function SetAlignment(cbAlignment: WORD): HResult; stdcall;

    function SetSchema(pStrSchema: LPOLESTR): HResult; stdcall;

    function AddVarDesc(index: UINT;
                        pVarDesc: VARDESC): HResult; stdcall;

    function SetFuncAndParamNames(index: UINT;
                                  rgszNames: LPOLESTR;
                                  cNames: UINT): HResult; stdcall;

    function SetVarName(index: UINT;
                        szName: LPOLESTR): HResult; stdcall;

    function SetTypeDescAlias(pTDescAlias: TYPEDESC): HResult; stdcall;

    function DefineFuncAsDllEntry(index: UINT;
                                  szDllName: LPOLESTR;
                                  szProcName: LPOLESTR): HResult; stdcall;

    function SetFuncDocString(index: UINT;
                              szDocString: LPOLESTR): HResult; stdcall;

    function SetVarDocString(index: UINT;
                             szDocString: LPOLESTR): HResult; stdcall;

    function SetFuncHelpContext(index: UINT;
                                dwHelpContext: DWORD): HResult; stdcall;

    function SetVarHelpContext(index: UINT;
                               dwHelpContext: DWORD): HResult; stdcall;

    function SetMops(index: UINT;
                     bstrMops: BSTR): HResult; stdcall;

    function SetTypeIdldesc(pIdlDesc: IDLDESC): HResult; stdcall;

    function LayOut(): HResult; stdcall;

  end;
  IID_ICreateTypeInfo = ICreateTypeInfo;
  {$EXTERNALSYM IID_ICreateTypeInfo}


  // Interface ICreateTypeInfo2
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DInterface_TYPE(ICreateTypeInfo2);'}
  {$EXTERNALSYM ICreateTypeInfo2}
  ICreateTypeInfo2 = Interface(ICreateTypeInfo)
  ['{0002040E-0000-0000-C000-000000000046}']

    function DeleteFuncDesc(index: UINT): HResult; stdcall;

    function DeleteFuncDescByMemId(memid: MEMBERID;
                                   invKind: INVOKEKIND): HResult; stdcall;

    function DeleteVarDesc(index: UINT): HResult; stdcall;

    function DeleteVarDescByMemId(memid: MEMBERID): HResult; stdcall;

    function DeleteImplType(index: UINT): HResult; stdcall;

    function SetCustData(const guid: REFGUID;
                         pVarVal: VARIANT): HResult; stdcall;

    function SetFuncCustData(index: UINT;
                             const guid: REFGUID;
                             pVarVal: VARIANT): HResult; stdcall;

    function SetParamCustData(indexFunc: UINT;
                              indexParam: UINT;
                              const guid: REFGUID;
                              pVarVal: VARIANT): HResult; stdcall;

    function SetVarCustData(index: UINT;
                            const guid: REFGUID;
                            pVarVal: VARIANT): HResult; stdcall;

    function SetImplTypeCustData(index: UINT;
                                 const guid: REFGUID;
                                 pVarVal: VARIANT): HResult; stdcall;

    function SetHelpStringContext(dwHelpStringContext: ULONG): HResult; stdcall;

    function SetFuncHelpStringContext(index: UINT;
                                      dwHelpStringContext: ULONG): HResult; stdcall;

    function SetVarHelpStringContext(index: UINT;
                                     dwHelpStringContext: ULONG): HResult; stdcall;

    function Invalidate(): HResult; stdcall;

    function SetName(szName: LPOLESTR): HResult; stdcall;

  end;
  IID_ICreateTypeInfo2 = ICreateTypeInfo2;
  {$EXTERNALSYM IID_ICreateTypeInfo2}


  // Interface ICreateTypeLib
  // ========================
  //
  {$HPPEMIT 'DECLARE_DInterface_TYPE(ICreateTypeLib);'}
  {$EXTERNALSYM ICreateTypeLib}
  ICreateTypeLib = Interface(IUnknown)
  ['{00020406-0000-0000-C000-000000000046}']

    function CreateTypeInfo(szName: LPOLESTR;
                            tkind: TYPEKIND;
                            out ppCTInfo: ICreateTypeInfo): HResult; stdcall;

    function SetName(szName: LPOLESTR): HResult; stdcall;

    function SetVersion(wMajorVerNum: WORD;
                        wMinorVerNum: WORD): HResult; stdcall;

    function SetGuid(const guid: REFGUID): HResult; stdcall;

    function SetDocString(szDoc: LPOLESTR): HResult; stdcall;

    function SetHelpFileName(szHelpFileName: LPOLESTR): HResult; stdcall;

    function SetHelpContext(dwHelpContext: DWORD): HResult; stdcall;

    function SetLcid(const lcid: LCID): HResult; stdcall;

    function SetLibFlags(uLibFlags: UINT): HResult; stdcall;

    function SaveAllChanges(): HResult; stdcall;

  end;
  IID_ICreateTypeLib = ICreateTypeLib;
  {$EXTERNALSYM IID_ICreateTypeLib}


  // Interface ICreateTypeLib2
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICreateTypeLib2);'}
  {$EXTERNALSYM ICreateTypeLib2}
  ICreateTypeLib2 = Interface(ICreateTypeLib)
  ['{0002040F-0000-0000-C000-000000000046}']

    function DeleteTypeInfo(szName: LPOLESTR): HResult; stdcall;

    function SetCustData(const guid: REFGUID;
                         var pVarVal: VARIANT): HResult; stdcall;

    function SetHelpStringContext(dwHelpStringContext: ULONG): HResult; stdcall;

    function SetHelpStringDll(szFileName: LPOLESTR): HResult; stdcall;

  end;
  IID_ICreateTypeLib2 = ICreateTypeLib2;
  {$EXTERNALSYM IID_ICreateTypeLib2}


  // Interface IDispatch
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDispatch);'}
  {$EXTERNALSYM IDispatch}
  IDispatch = Interface(IUnknown)
  ['{00020400-0000-0000-C000-000000000046}']

    function GetTypeInfoCount(out pctinfo: UINT): HResult; stdcall;

    function GetTypeInfo(iTInfo: UINT;
                         const lcid: LCID;
                         out ppTInfo: ITypeInfo): HResult; stdcall;

    function GetIDsOfNames(const riid: REFIID;
                           rgszNames: LPOLESTR;
                           cNames: UINT;
                           const lcid: LCID;
                           var rgDispId: DISPID): HResult; stdcall;

    function Invoke(dispIdMember: DISPID;
                    const riid: REFIID;
                    lcid: LCID;
                    wFlags: WORD;
                    pDispParams: DISPPARAMS;
                    var pVarResult: VARIANT;
                    var pExcepInfo: EXCEPINFO;
                    var puArgErr: UINT): HResult; stdcall;

  end;
  IID_IDispatch = IDispatch;
  {$EXTERNALSYM IID_IDispatch}


  // Interface IEnumVARIANT
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumVARIANT);'}
  {$EXTERNALSYM IEnumVARIANT}
  IEnumVARIANT = Interface(IUnknown)
  ['{00020404-0000-0000-C000-000000000046}']

    function Next(celt: ULONG;
                  out rgVar: VARIANT;
                  out pCeltFetched: ULONG): HResult; stdcall;

    function Skip(celt: ULONG): HResult; stdcall;

    function Reset(): HResult; stdcall;

    function Clone(out ppEnum: IEnumVARIANT): HResult; stdcall;

  end;
  IID_IEnumVARIANT = IEnumVARIANT;
  {$EXTERNALSYM IID_IEnumVARIANT}


  // Interface ITypeComp
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeComp);'}
  {$EXTERNALSYM ITypeComp}
  ITypeComp = Interface(IUnknown)
  ['{00020403-0000-0000-C000-000000000046}']

    function Bind(szName: LPOLESTR;
                  lHashVal: ULONG;
                  wFlags: WORD;
                  out ppTInfo: ITypeInfo;
                  out pDescKind: DESCKIND;
                  out pBindPtr: BINDPTR): HResult; stdcall;

    function BindType(szName: LPOLESTR;
                      lHashVal: ULONG;
                      out ppTInfo: ITypeInfo;
                      out ppTComp: ITypeComp): HResult; stdcall;

  end;
  IID_ITypeComp = ITypeComp;
  {$EXTERNALSYM IID_ITypeComp}


  // Interface ITypeInfo
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeInfo);'}
  {$EXTERNALSYM ITypeInfo}
  ITypeInfo = Interface(IUnknown)
  ['{00020401-0000-0000-C000-000000000046}']

    function GetTypeAttr(out ppTypeAttr: PTYPEATTR): HResult; stdcall;

    function GetTypeComp(out ppTComp: ITypeComp): HResult; stdcall;

    function GetFuncDesc(index: UINT;
                         out ppFuncDesc: FUNCDESC): HResult; stdcall;

    function GetVarDesc(index: UINT;
                        out ppVarDesc: VARDESC): HRESULT; stdcall;

    function GetNames(memid: MEMBERID;
                      out rgBstrNames: BSTR;
                      cMaxNames: UINT;
                      out pcNames: UINT): HResult; stdcall;

    function GetRefTypeOfImplType(index: UINT;
                                  out pRefType: HREFTYPE): HResult; stdcall;

    function GetImplTypeFlags(index: UINT;
                              out pImplTypeFlags: INT): HResult; stdcall;

    function GetIDsOfNames(rgszNames: LPOLESTR;
                           cNames: UINT;
                           out pMemId: MEMBERID): HResult; stdcall;

    function Invoke(pvInstance: Pointer;
                    memid: MEMBERID;
                    wFlags: WORD;
                    var pDispParams: DISPPARAMS;
                    out pVarResult: VARIANT;
                    out pExcepInfo: EXCEPINFO;
                    out puArgErr: UINT): HResult; stdcall;

    function GetDocumentation(memid: MEMBERID;
                              out pBstrName: BSTR;
                              out pBstrDocString: BSTR;
                              out pdwHelpContext: DWORD;
                              out pBstrHelpFile: BSTR): HResult; stdcall;

    function GetDllEntry(memid: MEMBERID;
                         invKind: INVOKEKIND;
                         out pBstrDllName: BSTR;
                         out pBstrName: BSTR;
                         out pwOrdinal: WORD): HResult; stdcall;

    function GetRefTypeInfo(hRefType: HREFTYPE;
                            out ppTInfo: ITypeInfo): HResult; stdcall;

    function AddressOfMember(memid: MEMBERID;
                             invKind: INVOKEKIND;
                             out ppv {IUnknown}): HRESULT; stdcall;

    function CreateInstance(pUnkOuter: IUnknown;
                            const riid: REFIID;
                            outppvObj: Pointer): HResult; stdcall;

    function GetMops(memid: MEMBERID;
                     out pBstrMops: BSTR): HResult; stdcall;

    function GetContainingTypeLib(out ppTLib: ITypeLib;
                                  out pIndex: UINT): HResult; stdcall;

    procedure ReleaseTypeAttr(pTypeAttr: TYPEATTR); stdcall;

    procedure ReleaseFuncDesc(pFuncDesc: FUNCDESC); stdcall;

    procedure ReleaseVarDesc(pVarDesc: VARDESC); stdcall;

  end;
  IID_ITypeInfo = ITypeInfo;
  {$EXTERNALSYM IID_ITypeInfo}


  // Interface ITypeInfo2
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeInfo2);'}
  {$EXTERNALSYM ITypeInfo2}
  ITypeInfo2 = Interface(ITypeInfo)
  ['{00020412-0000-0000-C000-000000000046}']

    function GetTypeKind(out pTypeKind: TYPEKIND): HResult; stdcall;

    function GetTypeFlags(out pTypeFlags: ULONG): HResult; stdcall;

    function GetFuncIndexOfMemId(memid: MEMBERID;
                                 invKind: INVOKEKIND;
                                 out pFuncIndex: UINT): HResult; stdcall;

    function GetVarIndexOfMemId(memid: MEMBERID;
                                out pVarIndex: UINT): HResult; stdcall;

    function GetCustData(const guid: REFGUID;
                         out pVarVal: VARIANT): HResult; stdcall;

    function GetFuncCustData(index: UINT;
                             const guid: REFGUID;
                             out pVarVal: VARIANT): HResult; stdcall;

    function GetParamCustData(indexFunc: UINT;
                              indexParam: UINT;
                              const guid: REFGUID;
                              out pVarVal: VARIANT): HResult; stdcall;

    function GetVarCustData(index: UINT;
                            const guid: REFGUID;
                            out pVarVal: VARIANT): HResult; stdcall;

    function GetImplTypeCustData(index: UINT;
                                 const guid: REFGUID;
                                 out pVarVal: VARIANT): HResult; stdcall;

    function GetDocumentation2(memid: MEMBERID;
                               lcid: LCID;
                               out pbstrHelpString: BSTR;
                               var pdwHelpStringContext: DWORD;
                               var pbstrHelpStringDll: BSTR): HResult; stdcall;

    function GetAllCustData(out pCustData: CUSTDATA): HResult; stdcall;

    function GetAllFuncCustData(index: UINT;
                                out pCustData: CUSTDATA): HResult; stdcall;

    function GetAllParamCustData(indexFunc: UINT;
                                 indexParam: UINT;
                                 out pCustData: CUSTDATA): HResult; stdcall;

    function GetAllVarCustData(index: UINT;
                               out pCustData: CUSTDATA): HResult; stdcall;

    function GetAllImplTypeCustData(index: UINT;
                                    out pCustData: CUSTDATA): HResult; stdcall;

  end;
  IID_ITypeInfo2 = ITypeInfo2;
  {$EXTERNALSYM IID_ITypeInfo2}


  // Interface ITypeLib
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeLib);'}
  {$EXTERNALSYM ITypeLib}
  ITypeLib = Interface(IUnknown)
  ['{00020402-0000-0000-C000-000000000046}']

    function GetTypeInfoCount(): UINT; stdcall;

    function GetTypeInfo(index: UINT;
                         out ppTInfo: ITypeInfo): HResult; stdcall;

    function GetTypeInfoType(index: UINT;
                             out pTKind: TYPEKIND): HResult; stdcall;

    function GetTypeInfoOfGuid(const guid: REFGUID;
                               out ppTinfo: ITypeInfo): HResult; stdcall;

    function GetLibAttr(out ppTLibAttr: PTLIBATTR): HResult; stdcall;

    function GetTypeComp(out ppTComp: ITypeComp): HResult; stdcall;

    function GetDocumentation(index: INT;
                              out pBstrName: BSTR;
                              out pBstrDocString: BSTR;
                              out pdwHelpContext: DWORD;
                              out pBstrHelpFile: BSTR): HResult; stdcall;

    function IsName(szNameBuf: LPOLESTR;
                    lHashVal: ULONG;
                    out pfName: BOOL): HResult; stdcall;

    function FindName(var szNameBuf: LPOLESTR;
                      lHashVal: ULONG;
                      out ppTInfo: ITypeInfo;
                      out rgMemId: MEMBERID;
                      var pcFound: USHORT): HResult; stdcall;

    procedure ReleaseTLibAttr(pTLibAttr: TLIBATTR); stdcall;

  end;
  IID_ITypeLib = ITypeLib;
  {$EXTERNALSYM IID_ITypeLib}


  // Interface ITypeLib2
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeLib2);'}
  {$EXTERNALSYM ITypeLib2}
  ITypeLib2 = Interface(ITypeLib)
  ['{00020411-0000-0000-C000-000000000046}']

    function GetCustData(const guid: REFGUID;
                         out pVarVal: VARIANT): HResult; stdcall;

    function GetLibStatistics(out pcUniqueNames: ULONG;
                              out pcchUniqueNames: ULONG): HResult; stdcall;

    function GetDocumentation2(index: INT;
                               lcid: LCID;
                               out pbstrHelpString: BSTR;
                               out pdwHelpStringContext: DWORD;
                               out pbstrHelpStringDll: BSTR): HResult; stdcall;

    function GetAllCustData(out pCustData: CUSTDATA): HResult; stdcall;

  end;
  IID_ITypeLib2 = ITypeLib2;
  {$EXTERNALSYM IID_ITypeLib2}


  // Interface ITypeChangeEvents
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeChangeEvents);'}
  {$EXTERNALSYM ITypeChangeEvents}
  ITypeChangeEvents = Interface(IUnknown)
  ['{00020410-0000-0000-C000-000000000046}']

    function RequestTypeChange(changeKind: CHANGEKIND;
                               pTInfoBefore: ITypeInfo;
                               pStrName: LPOLESTR;
                               out pfCancel: INT): HResult; stdcall;

    function AfterTypeChange(changeKind: CHANGEKIND;
                             pTInfoAfter: ITypeInfo;
                             pStrName: LPOLESTR): HResult; stdcall;

  end;
  IID_ITypeChangeEvents = ITypeChangeEvents;
  {$EXTERNALSYM IID_ITypeChangeEvents}


  // Interface IErrorInfo
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IErrorInfo);'}
  {$EXTERNALSYM IErrorInfo}
  IErrorInfo = Interface(IUnknown)
  ['{1CF2B120-547D-101B-8E65-08002B2BD119}']

    function GetGUID(out pGUID: TGUID): HResult; stdcall;

    function GetSource(out pBstrSource: BSTR): HResult; stdcall;

    function GetDescription(out pBstrDescription: BSTR): HResult; stdcall;

    function GetHelpFile(out pBstrHelpFile: BSTR): HResult; stdcall;

    function GetHelpContext(out pdwHelpContext: DWORD): HResult; stdcall;

  end;
  IID_IErrorInfo = IErrorInfo;
  {$EXTERNALSYM IID_IErrorInfo}


  // Interface ICreateErrorInfo
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICreateErrorInfo);'}
  {$EXTERNALSYM ICreateErrorInfo}
  ICreateErrorInfo = Interface(IUnknown)
  ['{22F03340-547D-101B-8E65-08002B2BD119}']

    function SetGUID(rguid: REFGUID): HResult; stdcall;

    function SetSource(szSource: LPOLESTR): HResult; stdcall;

    function SetDescription(szDescription: LPOLESTR): HResult; stdcall;

    function SetHelpFile(szHelpFile: LPOLESTR): HResult; stdcall;

    function SetHelpContext(const dwHelpContext: DWORD): HResult; stdcall;

  end;
  IID_ICreateErrorInfo = ICreateErrorInfo;
  {$EXTERNALSYM IID_ICreateErrorInfo}


  // Interface ISupportErrorInfo
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISupportErrorInfo);'}
  {$EXTERNALSYM ISupportErrorInfo}
  ISupportErrorInfo = Interface(IUnknown)
  ['{DF0B3D60-548F-101B-8E65-08002B2BD119}']

    function InterfaceSupportsErrorInfo(const riid: REFIID): HResult; stdcall;

  end;
  IID_ISupportErrorInfo = ISupportErrorInfo;
  {$EXTERNALSYM IID_ISupportErrorInfo}


  // Interface ITypeFactory
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeFactory);'}
  {$EXTERNALSYM ITypeFactory}
  ITypeFactory = Interface(IUnknown)
  ['{0000002E-0000-0000-C000-000000000046}']

    function CreateFromTypeInfo(pTypeInfo: ITypeInfo;
                                const riid: REFIID;
                                out ppv {IUnknown}): HResult; stdcall;

  end;
  IID_ITypeFactory = ITypeFactory;
  {$EXTERNALSYM IID_ITypeFactory}


  // Interface ITypeMarshal
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeMarshal);'}
  {$EXTERNALSYM ITypeMarshal}
  ITypeMarshal = Interface(IUnknown)
  ['{0000002D-0000-0000-C000-000000000046}']

    function Size(pvType: Pointer;
                  dwDestContext: DWORD;
                  pvDestContext: Pointer;
                  out pSize: PULONG): HResult; stdcall;

    function Marshal(pvType: Pointer;
                     dwDestContext: DWORD;
                     pvDestContext: Pointer;
                     cbBufferLength: ULONG;
                     out pBuffer: PByte;
                     out pcbWritten: PULONG): HResult; stdcall;

    function Unmarshal(out pvType: Pointer;
                       dwFlags: DWORD;
                       cbBufferLength: ULONG;
                       pBuffer: PByte;
                       out pcbRead: ULONG): HResult; stdcall;

    function Free(pvType: Pointer): HResult; stdcall;

  end;
  IID_ITypeMarshal = ITypeMarshal;
  {$EXTERNALSYM IID_ITypeMarshal}


  // Interface IRecordInfo
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRecordInfo);'}
  {$EXTERNALSYM IRecordInfo}
  IRecordInfo = Interface(IUnknown)
  ['{0000002F-0000-0000-C000-000000000046}']

    function RecordInit(out pvNew: Pointer): HResult; stdcall;

    function RecordClear(pvExisting: Pointer): HResult; stdcall;

    function RecordCopy(pvExisting: Pointer;
                        out pvNew: Pointer): HResult; stdcall;

    function GetGuid(out _pguid: TGUID): HResult; stdcall;

    function GetName(out pbstrName: BSTR): HResult; stdcall;

    function GetSize(out pcbSize: ULONG): HResult; stdcall;

    function GetTypeInfo(out ppTypeInfo: ITypeInfo): HResult; stdcall;

    function GetField(pvData: Pointer;
                      szFieldName: LPCOLESTR;
                      out pvarField: VARIANT): HResult; stdcall;

    function GetFieldNoCopy(pvData: Pointer;
                            szFieldName: LPCOLESTR;
                            out pvarField: VARIANT;
                            out ppvDataCArray: Pointer): HResult; stdcall;

    function PutField(wFlags: ULONG;
                      var pvData: Pointer;
                      szFieldName: LPCOLESTR;
                      pvarField: VARIANT): HResult; stdcall;

    function PutFieldNoCopy(wFlags: ULONG;
                            var pvData: Pointer;
                            szFieldName: LPCOLESTR;
                            pvarField: VARIANT): HResult; stdcall;

    function GetFieldNames(var pcNames: ULONG;
                           out rgBstrNames: PBSTR): HResult; stdcall;  // pointer to array of type BSTR.

    function IsMatchingType(pRecordInfo: IRecordInfo): BOOL; stdcall;

    // This method returns a pointer to the created record.
    function RecordCreate(): PVOID; stdcall;

    function RecordCreateCopy(pvSource: Pointer;
                              out ppvDest: Pointer): HResult; stdcall;

    function RecordDestroy(pvRecord: Pointer): HResult; stdcall;

  end;
  IID_IRecordInfo = IRecordInfo;
  {$EXTERNALSYM IID_IRecordInfo}


  // Interface IErrorLog
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IErrorLog);'}
  {$EXTERNALSYM IErrorLog}
  IErrorLog = Interface(IUnknown)
  ['{3127CA40-446E-11CE-8135-00AA004BB851}']

    function AddError(pszPropName: LPCOLESTR;
                      pExcepInfo: EXCEPINFO): HResult; stdcall;

  end;
  IID_IErrorLog = IErrorLog;
  {$EXTERNALSYM IID_IErrorLog}


  // Interface IPropertyBag
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyBag);'}
  {$EXTERNALSYM IPropertyBag}
  IPropertyBag = Interface(IUnknown)
  ['{55272A00-42CB-11CE-8135-00AA004BB851}']

    function Read(pszPropName: LPCOLESTR;
                  var pVar: VARIANT;
                  const pErrorLog: IErrorLog): HResult; stdcall;

    function Write(pszPropName: LPCOLESTR;
                   pVar: VARIANT): HResult; stdcall;

  end;
  IID_IPropertyBag = IPropertyBag;
  {$EXTERNALSYM IID_IPropertyBag}


  // Interface ITypeLibRegistrationReader
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeLibRegistrationReader);'}
  {$EXTERNALSYM ITypeLibRegistrationReader}
  ITypeLibRegistrationReader = Interface(IUnknown)
  ['{ED6A8A2A-B160-4E77-8F73-AA7435CD5C27}']

    function EnumTypeLibRegistrations(out ppEnumUnknown: PIEnumUnknown): HResult; stdcall;

  end;
  IID_ITypeLibRegistrationReader = ITypeLibRegistrationReader;
  {$EXTERNALSYM IID_ITypeLibRegistrationReader}


  // Interface ITypeLibRegistration
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITypeLibRegistration);'}
  {$EXTERNALSYM ITypeLibRegistration}
  ITypeLibRegistration = Interface(IUnknown)
  ['{76A3E735-02DF-4A12-98EB-043AD3600AF3}']

    function GetGuid(out pGuid: TGUID): HResult; stdcall;

    function GetVersion(out pVersion: BSTR): HResult; stdcall;

    function GetLcid(out pLcid: LCID): HResult; stdcall;

    function GetWin32Path(out pWin32Path: BSTR): HResult; stdcall;

    function GetWin64Path(out pWin64Path: BSTR): HResult; stdcall;

    function GetDisplayName(out pDisplayName: BSTR): HResult; stdcall;

    function GetFlags(out pFlags: DWORD): HResult; stdcall;

    function GetHelpDir(out pHelpDir: BSTR): HResult; stdcall;

  end;
  IID_ITypeLibRegistration = ITypeLibRegistration;
  {$EXTERNALSYM IID_ITypeLibRegistration}


  // Additional Prototypes for ALL Interfaces

  {$NODEFINE PVariantArgList}
  PVariantArgList = ^TVariantArgList;
  {$NODEFINE TVariantArgList}
  TVariantArgList = array[0..65534] of VariantArg;

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

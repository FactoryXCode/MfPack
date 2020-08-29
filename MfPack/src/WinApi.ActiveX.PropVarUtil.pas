// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.PropVarUtil.pas
// Kind: Pascal / Delphi unit
// Release date: 13-02-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Variant and PropVariant helpers.
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
// Source: propvarutil.h
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
unit WinApi.ActiveX.PropVarUtil;

  {$HPPEMIT '#include "propvarutil.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ShTypes,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.OaIdl;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

// typedef _Return_type_success_(return >= 0) LONG NTSTATUS;

const
  // PSTIME_FLAGS
  PSTF_UTC   = $00000000;
  {$EXTERNALSYM PSTF_UTC}
  PSTF_LOCAL = $00000001;
  {$EXTERNALSYM PSTF_LOCAL}

type
  tagPSTIME_FLAGS = type integer;
  {$EXTERNALSYM tagPSTIME_FLAGS}
  PSTIME_FLAGS = tagPSTIME_FLAGS;
  {$EXTERNALSYM PSTIME_FLAGS}


//====================
//
// PropVariant Helpers
//
//====================

// Initialize a propvariant
function InitPropVariantFromResource(hinst: HINSTANCE;
                                     id: UINT;
                                     out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromResource}

function InitPropVariantFromBuffer(pv: Pointer;
                                   cb: UINT;
                                   out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromBuffer}

function InitPropVariantFromCLSID(const clsid: REFCLSID;
                                  out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromCLSID}

function InitPropVariantFromGUIDAsString(const guid: REFGUID;
                                        out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromGUIDAsString}

function InitPropVariantFromFileTime(pftIn: FILETIME;
                                     out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromFileTime}

function InitPropVariantFromPropVariantVectorElem(propvarIn: PROPVARIANT;
                                                  iElem: ULONG;
                                                  out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromPropVariantVectorElem}

function InitPropVariantVectorFromPropVariant(propvarSingle: PROPVARIANT;
                                              out ppropvarVector: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantVectorFromPropVariant}

function InitPropVariantFromStrRet(var pstrret: STRRET;
                                   pidl: PCUITEMID_CHILD;
                                   out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromStrRet}

function InitPropVariantFromBooleanVector(prgf: BOOL;
                                          cElems: ULONG;
                                          out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromBooleanVector}

function InitPropVariantFromInt16Vector(prgn: SHORT;
                                        cElems: ULONG;
                                        out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromInt16Vector}

function InitPropVariantFromUInt16Vector(prgn: USHORT;
                                         cElems: ULONG;
                                         out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromUInt16Vector}

function InitPropVariantFromInt32Vector(prgn: LONG;
                                        cElems: ULONG;
                                        out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromInt32Vector}

function InitPropVariantFromUInt32Vector(prgn: ULONG;
                                         cElems: ULONG;
                                         out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromUInt32Vector}

function InitPropVariantFromInt64Vector(prgn: LONGLONG;
                                        cElems: ULONG;
                                        out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromInt64Vector}

function InitPropVariantFromUInt64Vector(prgn: ULONGLONG;
                                         cElems: ULONG;
                                         out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromUInt64Vector}

function InitPropVariantFromDoubleVector(prgn: DOUBLE;
                                         cElems: ULONG;
                                         out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromDoubleVector}

function InitPropVariantFromFileTimeVector(prgft: FILETIME;
                                           cElems: ULONG;
                                           out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromFileTimeVector}

function InitPropVariantFromStringVector(prgsz: PCWSTR;
                                         cElems: ULONG;
                                         out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromStringVector}

function InitPropVariantFromStringAsVector(psz: PCWSTR;
                                           out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM InitPropVariantFromStringAsVector}

// Inline Helpers
//===============
// Note:
// Compilers < D 2005 do not support the inline directive.
// Inline functions are best reserved for small, frequently used functions.

function InitPropVariantFromBoolean(fVal: BOOL;
                                    out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromInt16(nVal: SHORT;
                                  out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromUInt16(uiVal: USHORT;
                                   out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromInt32(lVal: LONG;
                                  out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromUInt32(ulVal: ULONG;
                                   out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromInt64(llVal: LONGLONG;
                                  out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromUInt64(ullVal: ULONGLONG;
                                   out ppropvar: PROPVARIANT): HResult; inline;

function InitPropVariantFromDouble(dblVal: DOUBLE;
                                   out ppropvar: PROPVARIANT): HResult; inline;

// Creates a VT_LPWSTR propvariant.
function InitPropVariantFromString(psz: LPCWSTR;
                                   out ppropvar: PROPVARIANT): HResult; inline;

//Creates a VT_VECTOR | VT_UI1 propvariant.
function InitPropVariantFromGUIDAsBuffer(const guid: REFGUID;
                                         out ppropvar: PROPVARIANT): HResult; inline;

function IsPropVariantVector(const propvar: PROPVARIANT): BOOL; inline;

// If this function returns TRUE,
// the PROPVARIANT structure referenced in propvar contains a Unicode string.
// Use PropVariantToStringWithDefault(propvar, Nil) to retrieve it.
function IsPropVariantString(const propvar: PROPVARIANT): BOOL; inline;



// Extract data from a propvariant
// ===============================
function PropVariantToBooleanWithDefault(const propvarIn: PROPVARIANT;
                                         fDefault: BOOL): Boolean; stdcall;
{$EXTERNALSYM PropVariantToBooleanWithDefault}

function PropVariantToInt16WithDefault(const propvarIn: PROPVARIANT;
                                       iDefault: SHORT): SHORT; stdcall;
{$EXTERNALSYM PropVariantToInt16WithDefault}

function PropVariantToUInt16WithDefault(const propvarIn: PROPVARIANT;
                                        uiDefault: USHORT): USHORT; stdcall;
{$EXTERNALSYM PropVariantToUInt16WithDefault}

function PropVariantToInt32WithDefault(const propvarIn: PROPVARIANT;
                                       lDefault: LONG): LONG; stdcall;
{$EXTERNALSYM PropVariantToInt32WithDefault}

function PropVariantToUInt32WithDefault(const propvarIn: PROPVARIANT;
                                        ulDefault: ULONG): ULONG; stdcall;
{$EXTERNALSYM PropVariantToUInt32WithDefault}

function PropVariantToInt64WithDefault(const propvarIn: PROPVARIANT;
                                       llDefault: LONGLONG): LONGLONG; stdcall;
{$EXTERNALSYM PropVariantToInt64WithDefault}

function PropVariantToUInt64WithDefault(const propvarIn: PROPVARIANT;
                                        ullDefault: ULONGLONG): ULONGLONG; stdcall;
{$EXTERNALSYM PropVariantToUInt64WithDefault}

function PropVariantToDoubleWithDefault(const propvarIn: PROPVARIANT;
                                        dblDefault: DOUBLE): DOUBLE; stdcall;
{$EXTERNALSYM PropVariantToDoubleWithDefault}

function PropVariantToStringWithDefault(const propvarIn: PROPVARIANT;
                                        pszDefault: LPCWSTR): LPCWSTR; stdcall;
{$EXTERNALSYM PropVariantToStringWithDefault}

function PropVariantToBoolean(const propvarIn: PROPVARIANT;
                              out pfRet: BOOL): HResult; stdcall;
{$EXTERNALSYM PropVariantToBoolean}

function PropVariantToInt16(const propvarIn: PROPVARIANT;
                            out piRet: SHORT): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt16}

function PropVariantToUInt16(const propvarIn: PROPVARIANT;
                             out puiRet: USHORT): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt16}

function PropVariantToInt32(const propvarIn: PROPVARIANT;
                            out plRet: LONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt32}

function PropVariantToUInt32(const propvarIn: PROPVARIANT;
                             out pulRet: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt32}

function PropVariantToInt64(const propvarIn: PROPVARIANT;
                            out pllRet: LONGLONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt64}

function PropVariantToUInt64(const propvarIn: PROPVARIANT;
                             out pullRet: ULONGLONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt64}

function PropVariantToDouble(const propvarIn: PROPVARIANT;
                             out pdblRet: DOUBLE): HResult; stdcall;
{$EXTERNALSYM PropVariantToDouble}

function PropVariantToBuffer(const propvar: PROPVARIANT;
                             var pv;
                             cb: UINT): HResult; stdcall;
{$EXTERNALSYM PropVariantToBuffer}

function PropVariantToString(const propvar: PROPVARIANT;
                             out psz: PWideChar;
                             cch: UINT): HResult; stdcall;
{$EXTERNALSYM PropVariantToString}

function PropVariantToGUID(const propvar: PROPVARIANT;
                           out pguid: TGUID): HResult; stdcall;
{$EXTERNALSYM PropVariantToGUID}

function PropVariantToStringAlloc(const propvar: PROPVARIANT;
                                  out ppszOut: LPCWSTR): HResult; stdcall;
{$EXTERNALSYM PropVariantToStringAlloc}

function PropVariantToBSTR(const propvar: PROPVARIANT;
                           out pbstrOut: BSTR): HResult; stdcall;
{$EXTERNALSYM PropVariantToBSTR}

function PropVariantToStrRet(const propvar: PROPVARIANT;
                             var pstrret: STRRET): HResult; stdcall;
{$EXTERNALSYM PropVariantToStrRet}

function PropVariantToFileTime(const propvar: PROPVARIANT;
                               pstfOut: PSTIME_FLAGS;
                               out pftOut: FILETIME): HResult; stdcall;
{$EXTERNALSYM PropVariantToFileTime}


function PropVariantToCLSID(const propvarIn: PROPVARIANT;
                            out pclsid: CLSID): HRESULT; inline;
{$EXTERNALSYM PropVariantToCLSID}


// Returns element count of a VT_VECTOR or VT_ARRAY value; or 1 otherwise
function PropVariantGetElementCount(const vpropvar: PROPVARIANT): ULONG; stdcall;
{$EXTERNALSYM PropVariantGetElementCount}

// Extract data from a propvariant into a vector
function PropVariantToBooleanVector(const propvar: PROPVARIANT;
                                    out prgf: BOOL;
                                    crgf: ULONG;
                                    out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToBooleanVector}

function PropVariantToInt16Vector(const propvar: PROPVARIANT;
                                  out prgn: SHORT;
                                  crgn: ULONG;
                                  out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt16Vector}

function PropVariantToUInt16Vector(const propvar: PROPVARIANT;
                                   out prgn: USHORT;
                                   crgn: ULONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt16Vector}

function PropVariantToInt32Vector(const propvar: PROPVARIANT;
                                  out prgn: LONG;
                                  crgn: ULONG;
                                  out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt32Vector}

function PropVariantToUInt32Vector(const propvar: PROPVARIANT;
                                   out prgn: ULONG;
                                   crgn: ULONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt32Vector}

function PropVariantToInt64Vector(const propvar: PROPVARIANT;
                                  out prgn: LONGLONG;
                                  crgn: ULONG;
                                  out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt64Vector}

function PropVariantToUInt64Vector(const propvar: PROPVARIANT;
                                   out prgn: ULONGLONG;
                                   crgn: ULONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt64Vector}

function PropVariantToDoubleVector(const propvar: PROPVARIANT;
                                   out prgn: DOUBLE;
                                   crgn: ULONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToDoubleVector}

function PropVariantToFileTimeVector(const propvar: PROPVARIANT;
                                     out prgft: FILETIME;
                                     crgft: ULONG;
                                     out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToFileTimeVector}

function PropVariantToStringVector(const propvar: PROPVARIANT;
                                   out prgsz: LPCWSTR;
                                   crgsz: ULONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToStringVector}


// Extract data from a propvariant and return an newly
// allocated vector (free with CoTaskMemFree)

function PropVariantToBooleanVectorAlloc(const propvar: PROPVARIANT;
                                         out pprgf: PBOOL;
                                         out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToBooleanVectorAlloc}

function PropVariantToInt16VectorAlloc(const propvar: PROPVARIANT;
                                       out pprgn: PSHORT;
                                       out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt16VectorAlloc}

function PropVariantToUInt16VectorAlloc(const propvar: PROPVARIANT;
                                        out pprgn: PUSHORT;
                                        out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt16VectorAlloc}

function PropVariantToInt32VectorAlloc(const propvar: PROPVARIANT;
                                       out pprgn: PLONG;
                                       out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt32VectorAlloc}

function PropVariantToUInt32VectorAlloc(const propvar: PROPVARIANT;
                                        out pprgn: PULONG;
                                        out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt32VectorAlloc}

function PropVariantToInt64VectorAlloc(const propvar: PROPVARIANT;
                                       out pprgn: PLONGLONG;
                                       out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToInt64VectorAlloc}

function PropVariantToUInt64VectorAlloc(const propvar: PROPVARIANT;
                                        out pprgn: PULONGLONG;
                                        out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToUInt64VectorAlloc}

function PropVariantToDoubleVectorAlloc(const propvar: PROPVARIANT;
                                        out pprgn: PDOUBLE;
                                        out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToDoubleVectorAlloc}

function PropVariantToFileTimeVectorAlloc(const propvar: PROPVARIANT;
                                          out pprgft: PFILETIME;
                                          out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToFileTimeVectorAlloc}

function PropVariantToStringVectorAlloc(const propvar: PROPVARIANT;
                                        out pprgsz: PPWideChar;
                                        out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantToStringVectorAlloc}


// Extract a single element from a propvariant.
// If it is a VT_VECTOR or VT_ARRAY, returns the element you request.
// Otherwise iElem must equal 0 and the function will returns the value.

function PropVariantGetBooleanElem(const propvar: PROPVARIANT;
                                   iElem: ULONG;
                                   out pfVal: BOOL): HResult; stdcall;
{$EXTERNALSYM PropVariantGetBooleanElem}

function PropVariantGetInt16Elem(const propvar: PROPVARIANT;
                                 iElem: ULONG;
                                 out pnVal: SHORT): HResult; stdcall;
{$EXTERNALSYM PropVariantGetInt16Elem}

function PropVariantGetUInt16Elem(const propvar: PROPVARIANT;
                                  iElem: ULONG;
                                  out pnVal: USHORT): HResult; stdcall;
{$EXTERNALSYM PropVariantGetUInt16Elem}

function PropVariantGetInt32Elem(const propvar: PROPVARIANT;
                                 iElem: ULONG;
                                 out pnVal: LONG): HResult; stdcall;
{$EXTERNALSYM PropVariantGetInt32Elem}

function PropVariantGetUInt32Elem(const propvar: PROPVARIANT;
                                  iElem: ULONG;
                                  out pnVal: ULONG): HResult; stdcall;
{$EXTERNALSYM PropVariantGetUInt32Elem}

function PropVariantGetInt64Elem(const propvar: PROPVARIANT;
                                 iElem: ULONG;
                                 out pnVal: LONGLONG): HResult; stdcall;
{$EXTERNALSYM PropVariantGetInt64Elem}

function PropVariantGetUInt64Elem(propvar: PROPVARIANT;
                                  iElem: ULONG;
                                  out pnVal: ULONGLONG): HResult; stdcall;
{$EXTERNALSYM PropVariantGetUInt64Elem}

function PropVariantGetDoubleElem(const propvar: PROPVARIANT;
                                  iElem: ULONG;
                                  out pnVal: DOUBLE): HResult; stdcall;
{$EXTERNALSYM PropVariantGetDoubleElem}

function PropVariantGetFileTimeElem(const propvar: PROPVARIANT;
                                    iElem: ULONG;
                                    out pftVal: FILETIME): HResult; stdcall;
{$EXTERNALSYM PropVariantGetFileTimeElem}

function PropVariantGetStringElem(const propvar: PROPVARIANT;
                                  iElem: ULONG;
                                  out ppszVal: LPCWSTR): HResult; stdcall;
{$EXTERNALSYM PropVariantGetStringElem}



function PropVariantGetElem(const propvarIn: PROPVARIANT;
                            iElem: ULONG;
                            out ppropvar: PROPVARIANT): HResult; inline;
{$EXTERNALSYM PropVariantGetElem}

// Helpers
procedure ClearPropVariantArray(var rgPropVar: PROPVARIANT;
                                cVars: UINT); stdcall;
{$EXTERNALSYM ClearPropVariantArray}



type
  PPropvarCompareUnit = ^PROPVAR_COMPARE_UNIT;
  PROPVAR_COMPARE_UNIT = (PVCU_DEFAULT = 0,
  {$EXTERNALSYM PROPVAR_COMPARE_UNIT}
                          PVCU_SECOND  = 1,
                          {$EXTERNALSYM PVCU_SECOND}
                          PVCU_MINUTE  = 2,
                          {$EXTERNALSYM PVCU_MINUTE}
                          PVCU_HOUR    = 3,
                          {$EXTERNALSYM PVCU_HOUR}
                          PVCU_DAY     = 4,
                          {$EXTERNALSYM PVCU_DAY}
                          PVCU_MONTH   = 5,
                          {$EXTERNALSYM PVCU_MONTH}
                          PVCU_YEAR    = 6
                          {$EXTERNALSYM PVCU_YEAR}
  );


  tagPROPVAR_COMPARE_FLAGS = type integer;
  {$EXTERNALSYM tagPROPVAR_COMPARE_FLAGS}

const
  // PROPVAR_COMPARE_FLAGS
  PVCF_DEFAULT                       = $00000000; // When comparing strings, use StrCmpLogical
  {$EXTERNALSYM PVCF_DEFAULT}
  PVCF_TREATEMPTYASGREATERTHAN       = $00000001; // Empty/null values are greater-than non-empty values
  {$EXTERNALSYM PVCF_TREATEMPTYASGREATERTHAN}
  PVCF_USESTRCMP                     = $00000002; // When comparing strings, use StrCmp
  {$EXTERNALSYM PVCF_USESTRCMP}
  PVCF_USESTRCMPC                    = $00000004; // When comparing strings, use StrCmpC
  {$EXTERNALSYM PVCF_USESTRCMPC}
  PVCF_USESTRCMPI                    = $00000008; // When comparing strings, use StrCmpI
  {$EXTERNALSYM PVCF_USESTRCMPI}
  PVCF_USESTRCMPIC                   = $00000010; // When comparing strings, use StrCmpIC
  {$EXTERNALSYM PVCF_USESTRCMPIC}
  PVCF_DIGITSASNUMBERS_CASESENSITIVE = $00000020; // When comparing strings, use CompareStringEx with LOCALE_NAME_USER_DEFAULT and SORT_DIGITSASNUMBERS.  This corresponds to the linguistically correct order for UI lists.
  {$EXTERNALSYM PVCF_DIGITSASNUMBERS_CASESENSITIVE}

type
  PROPVAR_COMPARE_FLAGS = tagPROPVAR_COMPARE_FLAGS;
  {$EXTERNALSYM PROPVAR_COMPARE_FLAGS}

// Comparisons
function PropVariantCompareEx(const propvar1: PROPVARIANT;
                              const propvar2: PROPVARIANT;
                              _unit: PROPVAR_COMPARE_UNIT;
                              flags: PROPVAR_COMPARE_FLAGS): integer; stdcall;
{$EXTERNALSYM PropVariantCompareEx}

function PropVariantCompare(const propvar1: PROPVARIANT;
                            const propvar2: PROPVARIANT): integer; inline;
{$EXTERNALSYM PropVariantCompare}


type
  tagPROPVAR_CHANGE_FLAGS = type integer;
  {$EXTERNALSYM tagPROPVAR_CHANGE_FLAGS}

const
    // PROPVAR_CHANGE_FLAGS
    PVCHF_DEFAULT           = $00000000;
    {$EXTERNALSYM PVCHF_DEFAULT}
    PVCHF_NOVALUEPROP       = $00000001; // Maps to VARIANT_NOVALUEPROP for VariantChangeType
    {$EXTERNALSYM PVCHF_NOVALUEPROP}
    PVCHF_ALPHABOOL         = $00000002; // Maps to VARIANT_ALPHABOOL for VariantChangeType
    {$EXTERNALSYM PVCHF_ALPHABOOL}
    PVCHF_NOUSEROVERRIDE    = $00000004; // Maps to VARIANT_NOUSEROVERRIDE for VariantChangeType
    {$EXTERNALSYM PVCHF_NOUSEROVERRIDE}
    PVCHF_LOCALBOOL         = $00000008; // Maps to VARIANT_LOCALBOOL for VariantChangeType
    {$EXTERNALSYM PVCHF_LOCALBOOL}
    PVCHF_NOHEXSTRING       = $00000010; // Don't convert a string that looks like hexadecimal (0xABCD) to the numerical equivalent
    {$EXTERNALSYM PVCHF_NOHEXSTRING}
type
  PROPVAR_CHANGE_FLAGS = tagPROPVAR_CHANGE_FLAGS;

// Conversions

function PropVariantChangeType(var ppropvarDest: PROPVARIANT;
                               const propvarSrc: PROPVARIANT;
                               flags: PROPVAR_CHANGE_FLAGS;
                               vt: VARTYPE): HResult; stdcall;
{$EXTERNALSYM PropVariantChangeType}

function PropVariantToVariant(const pPropVar: PROPVARIANT;
                              out pVar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM PropVariantToVariant}

function VariantToPropVariant(const pVar: OLEVARIANT;
                              out pPropVar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM VariantToPropVariant}

 // Stg functions
function StgSerializePropVariant(const ppropvar: PROPVARIANT;
                                 out ppProp: PSERIALIZEDPROPERTYVALUE;
                                 out pcb: ULONG): HResult; stdcall;
{$EXTERNALSYM StgSerializePropVariant}

function StgDeserializePropVariant(pprop: SERIALIZEDPROPERTYVALUE;
                                   cbMax: ULONG;
                                   out ppropvar: PROPVARIANT): HResult; stdcall;
{$EXTERNALSYM StgDeserializePropVariant}


//================
//
// Variant Helpers
//
//================

function IsVarTypeFloat(vt: VARTYPE): BOOL; stdcall;
{$EXTERNALSYM IsVarTypeFloat}
function IsVariantArray(_var: REFVARIANT): BOOL; stdcall;
{$EXTERNALSYM IsVariantArray}
function IsVariantString(_var: REFVARIANT): BOOL; stdcall;
{$EXTERNALSYM IsVariantString}
function IsVarTypeNumber(vt: VARTYPE): BOOL; stdcall;
{$EXTERNALSYM IsVarTypeNumber}
function IsVarTypeInteger(vt: VARTYPE): BOOL; stdcall;
{$EXTERNALSYM IsVarTypeInteger}


// Initialize a OLEVARIANT

function InitVariantFromResource(hinst: HINSTANCE;
                                 id: UINT;
                                 out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromResource}

function InitVariantFromBuffer(pv: Pointer;
                               cb: UINT;
                               out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromBuffer}

function InitVariantFromGUIDAsString(guid: REFGUID;
                                     out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromGUIDAsString}

function InitVariantFromFileTime(pft: FILETIME;
                                 out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromFileTime}

function InitVariantFromFileTimeArray(prgft: FILETIME;
                                      cElems: ULONG;
                                      out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromFileTimeArray}

function InitVariantFromStrRet(pstrret: STRRET;
                               pidl: PCUITEMID_CHILD;
                               out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromStrRet}

function InitVariantFromVariantArrayElem(varIn: REFVARIANT;
                                         iElem: ULONG;
                                         out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromVariantArrayElem}

function InitVariantFromBooleanArray(prgf: BOOL;
                                     cElems: ULONG;
                                     out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromBooleanArray}

function InitVariantFromInt16Array(prgn: SHORT;
                                   cElems: ULONG;
                                   out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromInt16Array}

function InitVariantFromUInt16Array(prgn: USHORT;
                                    cElems: ULONG;
                                    out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromUInt16Array}

function InitVariantFromInt32Array(prgn: LONG;
                                   cElems: ULONG;
                                   out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromInt32Array}

function InitVariantFromUInt32Array(prgn: ULONG;
                                    cElems: ULONG;
                                    out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromUInt32Array}

function InitVariantFromInt64Array(prgn: LONGLONG;
                                   cElems: ULONG;
                                   out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromInt64Array}

function InitVariantFromUInt64Array(prgn: ULONGLONG;
                                    cElems: ULONG;
                                    out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromUInt64Array}

function InitVariantFromDoubleArray(prgn: DOUBLE;
                                    cElems: ULONG;
                                    out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromDoubleArray}

function InitVariantFromStringArray(prgsz: PCWSTR;
                                    cElems: ULONG;
                                    out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM InitVariantFromStringArray}


function InitVariantFromBoolean(fVal: BOOL;
                                out pvar: OleVariant): HResult; inline;
{$EXTERNALSYM InitVariantFromBoolean}

function InitVariantFromInt16(iVal: SHORT;
                              out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromInt16}

function InitVariantFromUInt16(uiVal: USHORT;
                               out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromUInt16}

function InitVariantFromInt32(lVal: LONG;
                              out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromInt32}

function InitVariantFromUInt32(ulVal: ULONG;
                               out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromUInt32}

function InitVariantFromInt64(llVal: LONGLONG;
                              out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromInt64}

function InitVariantFromUInt64(ullVal: ULONGLONG;
                               out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromUInt64}

function InitVariantFromDouble(dblVal: DOUBLE;
                               out pvar: OLEVARIANT): HResult; inline;
{$EXTERNALSYM InitVariantFromDouble}

function InitVariantFromString(psz: LPCWSTR;
                               out pvar: OleVariant): HResult; inline;
{$EXTERNALSYM InitVariantFromString}

function InitVariantFromDispatch(pdisp: IDispatch;
                                 out pvar: OleVariant): HResult; inline;
{$EXTERNALSYM InitVariantFromDispatch}

function InitVariantFromDosDateTime(wDate: WORD;
                                    wTime: WORD;
                                    var pvar: OleVariant): HResult; inline;
{$EXTERNALSYM InitVariantFromDosDateTime}

function InitVariantFromGUIDAsBuffer(const _guid: TGUID;
                                     var pvar: OleVariant): HResult; inline;
{$EXTERNALSYM InitVariantFromGUIDAsBuffer}

function InitVariantFromUnknown(unknown: IUnknown;
                                var _variant: OleVariant): HResult; inline;
{$EXTERNALSYM InitVariantFromUnknown}

// Extract data from a OLEVARIANT
function VariantToBooleanWithDefault(const varIn: REFVARIANT;
                                     fDefault: BOOL): Boolean; stdcall;
{$EXTERNALSYM VariantToBooleanWithDefault}

function VariantToInt16WithDefault(varIn: REFVARIANT;
                                   iDefault: SHORT): SHORT; stdcall;
{$EXTERNALSYM VariantToInt16WithDefault}

function VariantToUInt16WithDefault(varIn: REFVARIANT;
                                    uiDefault: USHORT): USHORT; stdcall;
{$EXTERNALSYM VariantToUInt16WithDefault}

function VariantToInt32WithDefault(varIn: REFVARIANT;
                                   lDefault: LONG): LONG; stdcall;
{$EXTERNALSYM VariantToInt32WithDefault}

function VariantToUInt32WithDefault(varIn: REFVARIANT;
                                    ulDefault: ULONG): ULONG; stdcall;
{$EXTERNALSYM VariantToUInt32WithDefault}

function VariantToInt64WithDefault(varIn: REFVARIANT;
                                   llDefault: LONGLONG): LONGLONG; stdcall;
{$EXTERNALSYM VariantToInt64WithDefault}

function VariantToUInt64WithDefault(varIn: REFVARIANT;
                                    ullDefault: ULONGLONG): ULONGLONG; stdcall;
{$EXTERNALSYM VariantToUInt64WithDefault}

function VariantToDoubleWithDefault(varIn: REFVARIANT;
                                    dblDefault: DOUBLE): DOUBLE; stdcall;
{$EXTERNALSYM VariantToDoubleWithDefault}

function VariantToStringWithDefault(varIn: REFVARIANT;
                                    pszDefault: LPCWSTR): PCWSTR; stdcall;
{$EXTERNALSYM VariantToStringWithDefault}



function VariantToBoolean(varIn: REFVARIANT;
                          out pfRet: Boolean): HResult; stdcall;
{$EXTERNALSYM VariantToBoolean}

function VariantToInt16(varIn: REFVARIANT;
                        out piRet: SHORT): HResult; stdcall;
{$EXTERNALSYM VariantToInt16}

function VariantToUInt16(varIn: REFVARIANT;
                         out puiRet: USHORT): HResult; stdcall;
{$EXTERNALSYM VariantToUInt16}

function VariantToInt32(varIn: REFVARIANT;
                        out plRet: LONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt32}

function VariantToUInt32(varIn: REFVARIANT;
                         out pulRet: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt32}

function VariantToInt64(const varIn: REFVARIANT;
                        out pllRet: LONGLONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt64}

function VariantToUInt64(varIn: REFVARIANT;
                         out pullRet: ULONGLONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt64}

function VariantToDouble(varIn: REFVARIANT;
                         out pdblRet: DOUBLE): HResult; stdcall;
{$EXTERNALSYM VariantToDouble}

function VariantToBuffer(varIn: REFVARIANT;
                         out pv: Pointer;
                         cb: UINT): HResult; stdcall;
{$EXTERNALSYM VariantToBuffer}

function VariantToGUID(varIn: REFVARIANT;
                       out pguid: TGUID): HResult; stdcall;
{$EXTERNALSYM VariantToGUID}

function VariantToString(varIn: REFVARIANT;
                         out pszBuf: PWideChar;
                         cchBuf: UINT): HResult; stdcall;
{$EXTERNALSYM VariantToString}

function VariantToStringAlloc(varIn: REFVARIANT;
                              out ppszBuf: LPCWSTR): HResult; stdcall;
{$EXTERNALSYM VariantToStringAlloc}

function VariantToDosDateTime(varIn: REFVARIANT;
                              out pwDate: WORD;
                              out pwTime: WORD): HResult; stdcall;
{$EXTERNALSYM VariantToDosDateTime}

function VariantToStrRet(varIn: REFVARIANT;
                         out pstrret: STRRET): HResult; stdcall;
{$EXTERNALSYM VariantToStrRet}

function VariantToFileTime(varIn: REFVARIANT;
                           stfOut: PSTIME_FLAGS;
                           out pftOut: FILETIME): HResult; stdcall;
{$EXTERNALSYM VariantToFileTime}


// Get the element count.
// Returns number of elements for values of type VT_ARRAY; returns 1 otherwise.
function VariantGetElementCount(const varIn: REFVARIANT): ULONG; stdcall;
{$EXTERNALSYM VariantGetElementCount}


// Extract data from a OLEVARIANT into a vector
function VariantToBooleanArray(_var: REFVARIANT;
                               out prgf: BOOL;
                               crgn: ULONG;
                               out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToBooleanArray}

function VariantToInt16Array(_var: REFVARIANT;
                             out prgn: SHORT;
                             crgn: ULONG;
                             out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt16Array}

function VariantToUInt16Array(_var: REFVARIANT;
                              out prgn: USHORT;
                              crgn: ULONG;
                              out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt16Array}

function VariantToInt32Array(_var: REFVARIANT;
                             out prgn: LONG;
                             crgn: ULONG;
                             out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt32Array}

function VariantToUInt32Array(_var: REFVARIANT;
                              out prgn: ULONG;
                              crgn: ULONG;
                              out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt32Array}

function VariantToInt64Array(_var: REFVARIANT;
                             out prgn: LONGLONG;
                             crgn: ULONG;
                             out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt64Array}

function VariantToUInt64Array(_var: REFVARIANT;
                              out prgn: ULONGLONG;
                              crgn: ULONG;
                              out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt64Array}

function VariantToDoubleArray(_var: REFVARIANT;
                              out prgn: DOUBLE;
                              crgn: ULONG;
                              out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToDoubleArray}

function VariantToStringArray(_var: REFVARIANT;
                              out prgsz: LPCWSTR;
                              crgsz: ULONG;
                              out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToStringArray}


// Extract data from a OLEVARIANT into a newly allocated vector (free with CoTaskMemFree)
function VariantToBooleanArrayAlloc(_var: REFVARIANT;
                                    out pprgf: PBOOL;
                                    out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToBooleanArrayAlloc}

function VariantToInt16ArrayAlloc(_var: REFVARIANT;
                                  out pprgn: PSHORT;
                                  out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt16ArrayAlloc}

function VariantToUInt16ArrayAlloc(_var: REFVARIANT;
                                   out pprgn: PUSHORT;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt16ArrayAlloc}

function VariantToInt32ArrayAlloc(_var: REFVARIANT;
                                  out pprgn: PLONG;
                                  out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt32ArrayAlloc}

function VariantToUInt32ArrayAlloc(_var: REFVARIANT;
                                   out pprgn: PULONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt32ArrayAlloc}

function VariantToInt64ArrayAlloc(_var: REFVARIANT;
                                  out pprgn: PLONGLONG;
                                  out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToInt64ArrayAlloc}

function VariantToUInt64ArrayAlloc(_var: REFVARIANT;
                                   out pprgn: PULONGLONG;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToUInt64ArrayAlloc}

function VariantToDoubleArrayAlloc(_var: REFVARIANT;
                                   out pprgn: PDOUBLE;
                                   out pcElem: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantToDoubleArrayAlloc}


// Get a single element of a OLEVARIANT.
// If it is type VT_ARRAY, returns a the requested element.
// Otherwise iElem must equal 0 and the function returns the value.
function VariantGetBooleanElem(_var: REFVARIANT;
                               iElem: ULONG;
                               out pfVal: Boolean): HResult; stdcall;
{$EXTERNALSYM VariantGetBooleanElem}

function VariantGetInt16Elem(_var: REFVARIANT;
                             iElem: ULONG;
                             out pnVal: SHORT): HResult; stdcall;
{$EXTERNALSYM VariantGetInt16Elem}

function VariantGetUInt16Elem(_var: REFVARIANT;
                              iElem: ULONG;
                              out pnVal: USHORT): HResult; stdcall;
{$EXTERNALSYM VariantGetUInt16Elem}

function VariantGetInt32Elem(_var: REFVARIANT;
                             iElem: ULONG;
                             out pnVal: LONG): HResult; stdcall;
{$EXTERNALSYM VariantGetInt32Elem}

function VariantGetUInt32Elem(_var: REFVARIANT;
                              iElem: ULONG;
                              out pnVal: ULONG): HResult; stdcall;
{$EXTERNALSYM VariantGetUInt32Elem}

function VariantGetInt64Elem(_var: REFVARIANT;
                             iElem: ULONG;
                             out pnVal: LONGLONG): HResult; stdcall;
{$EXTERNALSYM VariantGetInt64Elem}

function VariantGetUInt64Elem(_var: REFVARIANT;
                              iElem: ULONG;
                              out pnVal: ULONGLONG): HResult; stdcall;
{$EXTERNALSYM VariantGetUInt64Elem}

function VariantGetDoubleElem(_var: REFVARIANT;
                              iElem: ULONG;
                              out pnVal: DOUBLE): HResult; stdcall;
{$EXTERNALSYM VariantGetDoubleElem}

function VariantGetStringElem(_var: REFVARIANT;
                              iElem: ULONG;
                              out ppszVal: PWideChar): HResult; stdcall;
{$EXTERNALSYM VariantGetStringElem}

function VariantGetElem(varIn: REFVARIANT;
                        iElem: ULONG;
                        out pvar: OLEVARIANT): HResult; stdcall;
{$EXTERNALSYM VariantGetElem}


// Helpers
procedure ClearVariantArray(var pvars: OLEVARIANT;
                            cvars: UINT); stdcall;
{$EXTERNALSYM ClearVariantArray}

function VariantCompare(var1: REFVARIANT;
                        var2: REFVARIANT): integer; stdcall;
{$EXTERNALSYM VariantCompare}


type
//===========================
//
// Property-specific notions
//
//===========================


// The progress bar property control uses a specially formatted PROPVARIANT to
// convey the look of the progress bar
// propvar.vt = VT_UI4
// propvar.caul.pElems[0] = current progress
// propvar.caul.pElems[1] = total progress
// propvar.caul.pElems[2] = DRAWPROGRESSFLAGS (see below);

  PDRAWPROGRESSFLAGS = ^DRAWPROGRESSFLAGS;
  DRAWPROGRESSFLAGS      = (
    DPF_NONE             = $0,    // No progress flags.
    {$EXTERNALSYM DPF_NONE}
    DPF_MARQUEE          = $1,    // The progress bar should draw in marquee mode.
    {$EXTERNALSYM DPF_MARQUEE}
    DPF_MARQUEE_COMPLETE = $2,    // The marquee format progress bar has completed.
    {$EXTERNALSYM DPF_MARQUEE_COMPLETE}
    DPF_ERROR            = $4,    // The progress bar should be drawn in the error state.
    {$EXTERNALSYM DPF_ERROR}
    DPF_WARNING          = $8,    // The progress bar should be drawn in the warning state.
    {$EXTERNALSYM DPF_WARNING}
    DPF_STOPPED          = $10    // The progress bar is stopped.
    {$EXTERNALSYM DPF_STOPPED}
  );
  {$EXTERNALSYM DRAWPROGRESSFLAGS}



  // Additional Prototypes for ALL methodes


// Helpers:  (not a part of propvarutil.h, but Shlwapi.h)

// (See: https://docs.microsoft.com/nl-nl/windows/win32/api/shlwapi/nf-shlwapi-iunknown_set
procedure IUnknown_Set({_Inout_} var ppunk: Pointer {IUnknown};
                       {_In_opt_} punk: IUnknown); stdcall;
{$EXTERNALSYM IUnknown_Set}

// Makes a copy of a string in newly allocated memory.
function SHStrDupW(const psz: LPCWSTR;
                   ppwsz: LPCWSTR): HRESULT; stdcall;
{$EXTERNALSYM SHStrDupW}

// Additional helpers from OleAut32

function SysAllocString(psz: LPCWSTR): PWideChar; stdcall;
{$EXTERNALSYM SysAllocString}

procedure VariantInit(var varg: OleVariant); stdcall;
{$EXTERNALSYM VariantInit}

function VariantClear(var varg: OleVariant): HResult; stdcall;
{$EXTERNALSYM VariantClear}

function VariantCopy(var vargDest: OleVariant;
                     const vargSrc: OleVariant): HResult; stdcall;
{$EXTERNALSYM VariantCopy}

function VariantCopyInd(var varDest: OleVariant;
                        const vargSrc: OleVariant): HResult; stdcall;
{$EXTERNALSYM VariantCopyInd}

function VariantChangeType(var vargDest: OleVariant;
                           const vargSrc: OleVariant;
                           wFlags: Word;
                           vt: VARTYPE): HResult; stdcall;
{$EXTERNALSYM VariantChangeType}

function VariantChangeTypeEx(var vargDest: OleVariant;
                             const vargSrc: OleVariant;
                             const lcid: LCID;
                             wFlags: Word;
                             vvt: VARTYPE): HResult; stdcall;
{$EXTERNALSYM VariantChangeTypeEx}

function DosDateTimeToVariantTime(cwDosDate: Word;
                                  wDosTime: Word;
                                  out vtime: OLE_DATE): integer; stdcall;
{$EXTERNALSYM DosDateTimeToVariantTime}

  // end of Additional Prototypes


implementation

const
  PropVarUtilLibH1 = 'Shlwapi.dll';
  PropVarUtilLibH2 = 'OleAut32.dll';

{$WARN SYMBOL_PLATFORM OFF}

//See earlier comment
procedure IUnknown_Set; external PropVarUtilLibH1 name 'IUnknown_Set' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHStrDupW; external PropVarUtilLibH1 name 'SHStrDupW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function SysAllocString; external PropVarUtilLibH2 name 'SysAllocString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure VariantInit; external PropVarUtilLibH2 name 'VariantInit' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantClear; external PropVarUtilLibH2 name 'VariantClear' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantCopy; external PropVarUtilLibH2 name 'VariantCopy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantCopyInd; external PropVarUtilLibH2 name 'VariantCopyInd' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantChangeType; external PropVarUtilLibH2 name 'VariantChangeType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantChangeTypeEx; external PropVarUtilLibH2 name 'VariantChangeTypeEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function DosDateTimeToVariantTime; external PropVarUtilLibH2 name 'DosDateTimeToVariantTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  // end implementations of additional prototypes


const
  PropVarUtilLib = 'propsys.dll';

function InitPropVariantFromResource; external PropVarUtilLib name 'ClearPropVariantArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromBuffer; external PropVarUtilLib name 'InitPropVariantFromBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromCLSID; external PropVarUtilLib name 'InitPropVariantFromCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromGUIDAsString; external PropVarUtilLib name 'InitPropVariantFromGUIDAsString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromFileTime; external PropVarUtilLib name 'InitPropVariantFromFileTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromPropVariantVectorElem; external PropVarUtilLib name 'InitPropVariantFromPropVariantVectorElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantVectorFromPropVariant; external PropVarUtilLib name 'InitPropVariantVectorFromPropVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromStrRet; external PropVarUtilLib name 'InitPropVariantFromStrRet' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromBooleanVector; external PropVarUtilLib name 'InitPropVariantFromBooleanVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromInt16Vector; external PropVarUtilLib name 'InitPropVariantFromInt16Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromUInt16Vector; external PropVarUtilLib name 'InitPropVariantFromUInt16Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromInt32Vector; external PropVarUtilLib name 'InitPropVariantFromInt32Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromUInt32Vector; external PropVarUtilLib name 'InitPropVariantFromUInt32Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromInt64Vector; external PropVarUtilLib name 'InitPropVariantFromInt64Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromUInt64Vector; external PropVarUtilLib name 'InitPropVariantFromUInt64Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromDoubleVector; external PropVarUtilLib name 'InitPropVariantFromDoubleVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromFileTimeVector; external PropVarUtilLib name 'InitPropVariantFromFileTimeVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromStringVector; external PropVarUtilLib name 'InitPropVariantFromStringVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitPropVariantFromStringAsVector; external PropVarUtilLib name 'InitPropVariantFromStringAsVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function PropVariantToBooleanWithDefault; external PropVarUtilLib name 'PropVariantToBooleanWithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt16WithDefault; external PropVarUtilLib name 'PropVariantToInt16WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt16WithDefault; external PropVarUtilLib name 'PropVariantToUInt16WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt32WithDefault; external PropVarUtilLib name 'PropVariantToInt32WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt32WithDefault; external PropVarUtilLib name 'PropVariantToUInt32WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt64WithDefault; external PropVarUtilLib name 'PropVariantToInt64WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt64WithDefault; external PropVarUtilLib name 'PropVariantToUInt64WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToDoubleWithDefault; external PropVarUtilLib name 'PropVariantToDoubleWithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToStringWithDefault; external PropVarUtilLib name 'PropVariantToStringWithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function PropVariantToBoolean; external PropVarUtilLib name 'PropVariantToBoolean' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt16; external PropVarUtilLib name 'PropVariantToInt16' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt16; external PropVarUtilLib name 'PropVariantToUInt16' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt32; external PropVarUtilLib name 'PropVariantToInt32' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt32; external PropVarUtilLib name 'PropVariantToUInt32' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt64; external PropVarUtilLib name 'PropVariantToInt64' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt64; external PropVarUtilLib name 'PropVariantToUInt64' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToDouble; external PropVarUtilLib name 'PropVariantToDouble' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToBuffer; external PropVarUtilLib name 'PropVariantToBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToString; external PropVarUtilLib name 'PropVariantToString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToGUID; external PropVarUtilLib name 'PropVariantToGUID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToStringAlloc; external PropVarUtilLib name 'PropVariantToStringAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToBSTR; external PropVarUtilLib name 'PropVariantToBSTR' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToStrRet; external PropVarUtilLib name 'PropVariantToStrRet' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToFileTime; external PropVarUtilLib name 'PropVariantToFileTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function PropVariantGetElementCount; external PropVarUtilLib name 'PropVariantGetElementCount' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToBooleanVector; external PropVarUtilLib name 'PropVariantToBooleanVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt16Vector; external PropVarUtilLib name 'PropVariantToInt16Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt16Vector; external PropVarUtilLib name 'PropVariantToUInt16Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt32Vector; external PropVarUtilLib name 'PropVariantToInt32Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt32Vector; external PropVarUtilLib name 'PropVariantToUInt32Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt64Vector; external PropVarUtilLib name 'PropVariantToInt64Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt64Vector; external PropVarUtilLib name 'PropVariantToUInt64Vector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToDoubleVector; external PropVarUtilLib name 'PropVariantToDoubleVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToFileTimeVector; external PropVarUtilLib name 'PropVariantToFileTimeVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToStringVector; external PropVarUtilLib name 'PropVariantToStringVector' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

procedure ClearPropVariantArray; external PropVarUtilLib name 'ClearPropVariantArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function PropVariantCompareEx; external PropVarUtilLib name 'PropVariantCompareEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function InitVariantFromResource; external PropVarUtilLib name 'InitVariantFromResource' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromBuffer; external PropVarUtilLib name 'InitVariantFromBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromGUIDAsString; external PropVarUtilLib name 'InitVariantFromGUIDAsString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromFileTime; external PropVarUtilLib name 'InitVariantFromFileTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromFileTimeArray; external PropVarUtilLib name 'InitVariantFromFileTimeArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromStrRet; external PropVarUtilLib name 'InitVariantFromStrRet' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromVariantArrayElem; external PropVarUtilLib name 'InitVariantFromVariantArrayElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromBooleanArray; external PropVarUtilLib name 'InitVariantFromBooleanArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromInt16Array; external PropVarUtilLib name 'InitVariantFromInt16Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromUInt16Array; external PropVarUtilLib name 'InitVariantFromUInt16Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromInt32Array; external PropVarUtilLib name 'InitVariantFromInt32Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromUInt32Array; external PropVarUtilLib name 'InitVariantFromUInt32Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromInt64Array; external PropVarUtilLib name 'InitVariantFromInt64Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromUInt64Array; external PropVarUtilLib name 'InitVariantFromUInt64Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromDoubleArray; external PropVarUtilLib name 'InitVariantFromDoubleArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitVariantFromStringArray; external PropVarUtilLib name 'InitVariantFromStringArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function VariantGetElementCount; external PropVarUtilLib name 'VariantGetElementCount' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function PropVariantToBooleanVectorAlloc; external PropVarUtilLib name 'PropVariantToBooleanVectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt16VectorAlloc; external PropVarUtilLib name 'PropVariantToInt16VectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt16VectorAlloc; external PropVarUtilLib name 'PropVariantToUInt16VectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt32VectorAlloc; external PropVarUtilLib name 'PropVariantToInt32VectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt32VectorAlloc; external PropVarUtilLib name 'PropVariantToUInt32VectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToInt64VectorAlloc; external PropVarUtilLib name 'PropVariantToInt64VectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToUInt64VectorAlloc; external PropVarUtilLib name 'PropVariantToUInt64VectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToDoubleVectorAlloc; external PropVarUtilLib name 'PropVariantToDoubleVectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToFileTimeVectorAlloc; external PropVarUtilLib name 'PropVariantToFileTimeVectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToStringVectorAlloc; external PropVarUtilLib name 'PropVariantToStringVectorAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetBooleanElem; external PropVarUtilLib name 'PropVariantGetBooleanElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetInt16Elem; external PropVarUtilLib name 'PropVariantGetInt16Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetUInt16Elem; external PropVarUtilLib name 'PropVariantGetUInt16Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetInt32Elem; external PropVarUtilLib name 'PropVariantGetInt32Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetUInt32Elem; external PropVarUtilLib name 'PropVariantGetUInt32Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetInt64Elem; external PropVarUtilLib name 'PropVariantGetInt64Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetUInt64Elem; external PropVarUtilLib name 'PropVariantGetUInt64Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetDoubleElem; external PropVarUtilLib name 'PropVariantGetDoubleElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetFileTimeElem; external PropVarUtilLib name 'PropVariantGetFileTimeElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantGetStringElem; external PropVarUtilLib name 'PropVariantGetStringElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function PropVariantChangeType; external PropVarUtilLib name 'PropVariantChangeType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantToVariant; external PropVarUtilLib name 'PropVariantToVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToPropVariant; external PropVarUtilLib name 'VariantToPropVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgSerializePropVariant; external PropVarUtilLib name 'StgSerializePropVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgDeserializePropVariant; external PropVarUtilLib name 'StgDeserializePropVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function IsVarTypeFloat; external PropVarUtilLib name 'IsVarTypeFloat' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IsVariantArray; external PropVarUtilLib name 'IsVariantArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IsVariantString; external PropVarUtilLib name 'IsVariantString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IsVarTypeNumber; external PropVarUtilLib name 'IsVarTypeNumber' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IsVarTypeInteger; external PropVarUtilLib name 'IsVarTypeInteger' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

// Extract data from a OLEVARIANT
function VariantToBooleanWithDefault; external PropVarUtilLib name 'VariantToBooleanWithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt16WithDefault; external PropVarUtilLib name 'VariantToInt16WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt16WithDefault; external PropVarUtilLib name 'VariantToUInt16WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt32WithDefault; external PropVarUtilLib name 'VariantToInt32WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt32WithDefault; external PropVarUtilLib name 'VariantToUInt32WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt64WithDefault; external PropVarUtilLib name 'VariantToInt64WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt64WithDefault; external PropVarUtilLib name 'VariantToUInt64WithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToDoubleWithDefault; external PropVarUtilLib name 'VariantToDoubleWithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToStringWithDefault; external PropVarUtilLib name 'VariantToStringWithDefault' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function VariantToBoolean; external PropVarUtilLib name 'VariantToBoolean' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt16; external PropVarUtilLib name 'VariantToInt16' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt16; external PropVarUtilLib name 'VariantToUInt16' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt32; external PropVarUtilLib name 'VariantToInt32' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt32; external PropVarUtilLib name 'VariantToUInt32' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt64; external PropVarUtilLib name 'VariantToInt64' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt64; external PropVarUtilLib name 'VariantToUInt64' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToDouble; external PropVarUtilLib name 'VariantToDouble' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToBuffer; external PropVarUtilLib name 'VariantToBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToGUID; external PropVarUtilLib name 'VariantToGUID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToString; external PropVarUtilLib name 'VariantToString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToStringAlloc; external PropVarUtilLib name 'VariantToStringAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToDosDateTime; external PropVarUtilLib name 'VariantToDosDateTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToStrRet; external PropVarUtilLib name 'VariantToStrRet' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToFileTime; external PropVarUtilLib name 'VariantToFileTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};


function VariantToBooleanArray; external PropVarUtilLib name 'VariantToBooleanArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt16Array; external PropVarUtilLib name 'VariantToInt16Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt16Array; external PropVarUtilLib name 'VariantToUInt16Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt32Array; external PropVarUtilLib name 'VariantToInt32Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt32Array; external PropVarUtilLib name 'VariantToUInt32Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt64Array; external PropVarUtilLib name 'VariantToInt64Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt64Array; external PropVarUtilLib name 'VariantToUInt64Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToDoubleArray; external PropVarUtilLib name 'VariantToDoubleArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToStringArray; external PropVarUtilLib name 'VariantToStringArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function VariantToBooleanArrayAlloc; external PropVarUtilLib name 'VariantToBooleanArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt16ArrayAlloc; external PropVarUtilLib name 'VariantToInt16ArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt16ArrayAlloc; external PropVarUtilLib name 'VariantToUInt16ArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt32ArrayAlloc; external PropVarUtilLib name 'VariantToInt32ArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt32ArrayAlloc; external PropVarUtilLib name 'VariantToUInt32ArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToInt64ArrayAlloc; external PropVarUtilLib name 'VariantToInt64ArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToUInt64ArrayAlloc; external PropVarUtilLib name 'VariantToUInt64ArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantToDoubleArrayAlloc; external PropVarUtilLib name 'VariantToDoubleArrayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function VariantGetBooleanElem; external PropVarUtilLib name 'VariantGetBooleanElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetInt16Elem; external PropVarUtilLib name 'VariantGetInt16Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetUInt16Elem; external PropVarUtilLib name 'VariantGetUInt16Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetInt32Elem; external PropVarUtilLib name 'VariantGetInt32Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetUInt32Elem; external PropVarUtilLib name 'VariantGetUInt32Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetInt64Elem; external PropVarUtilLib name 'VariantGetInt64Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetUInt64Elem; external PropVarUtilLib name 'VariantGetUInt64Elem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetDoubleElem; external PropVarUtilLib name 'VariantGetDoubleElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetStringElem; external PropVarUtilLib name 'VariantGetStringElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function VariantGetElem; external PropVarUtilLib name 'VariantGetElem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

procedure ClearVariantArray; external PropVarUtilLib name 'ClearVariantArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function VariantCompare; external PropVarUtilLib name 'VariantCompare' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}


// Inline Helpers
//===============

function InitPropVariantFromBoolean(fVal: BOOL;
                                    out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_BOOL;
  ppropvar.boolVal:= fVal;
  Result:= S_OK;
end;

function InitPropVariantFromInt16(nVal: SHORT;
                                  out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_I2;
  ppropvar.iVal:= nVal;
  Result:= S_OK;
end;

function InitPropVariantFromUInt16(uiVal: USHORT;
                                   out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_UI2;
  ppropvar.uiVal:= uiVal;
  Result:= S_OK;
end;

function InitPropVariantFromInt32(lVal: Integer;
                                  out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_I4;
  ppropvar.lVal:= lVal;
  Result:= S_OK;
end;

function InitPropVariantFromUInt32(ulVal: ULONG;
                                   out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_UI4;
  ppropvar.ulVal:= ulVal;
  Result:= S_OK;
end;

function InitPropVariantFromInt64(llVal: LONGLONG;
                                  out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_I8;
  ppropvar.hVal.QuadPart:= llVal;
  Result:= S_OK;
end;

function InitPropVariantFromUInt64(ullVal: ULONGLONG;
                                   out ppropvar: PROPVARIANT): HRESULT; inline;
begin
  ppropvar.vt:= VT_UI8;
  ppropvar.uhVal.QuadPart:= ullVal;
  Result:= S_OK;
end;

function InitPropVariantFromDouble(dblVal: Double;
                                   out ppropvar: PROPVARIANT): HResult; inline;
begin
  ppropvar.vt:= VT_R8;
  ppropvar.dblVal:= dblVal;
  Result:= S_OK;
end;

function PropVariantToCLSID(const propvarIn: PROPVARIANT;
                            out pclsid: CLSID): HRESULT; inline;
begin
  Result:= PropVariantToGUID(propvarIn, pclsid);
end;

function PropVariantGetElem(const propvarIn: PROPVARIANT;
                            iElem: ULONG;
                            out ppropvar: PROPVARIANT): HResult; inline;
begin
  Result:= InitPropVariantFromPropVariantVectorElem(propvarIn,
                                                    iElem,
                                                    ppropvar);
end;

function PropVariantCompare(const propvar1: PROPVARIANT;
                            const propvar2: PROPVARIANT): integer; inline;
begin
  Result:= PropVariantCompareEx(propvar1,
                                propvar2,
                                PVCU_DEFAULT,
                                PVCF_DEFAULT);
end;



function InitVariantFromBoolean(fVal: BOOL;
                                out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_BOOL;
    if fVal = true then
      VARIANTARG(pvar).vbool:= VARIANT_TRUE
    else
      VARIANTARG(pvar).vbool:= VARIANT_FALSE;
    Result:= S_OK;
end;

function InitVariantFromInt16(iVal: SHORT;
                              out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_I2;
    VARIANTARG(pvar).iVal:= iVal;
    Result:= S_OK;
end;

function InitVariantFromUInt16(uiVal: USHORT;
                               out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_UI2;
    VARIANTARG(pvar).uiVal:= uiVal;
    Result:= S_OK;
end;

function InitVariantFromInt32(lVal: LONG;
                              out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_I4;
    VARIANTARG(pvar).lVal:= lVal;
    Result:= S_OK;
end;

function InitVariantFromUInt32(ulVal: ULONG;
                               out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_UI4;
    VARIANTARG(pvar).ulVal:= ulVal;
    Result:= S_OK;
end;

function InitVariantFromInt64(llVal: LONGLONG;
                              out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_I8;
    VARIANTARG(pvar).llVal:= llVal;
    Result:= S_OK;
end;

function InitVariantFromUInt64(ullVal: ULONGLONG;
                               out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_UI8;
    VARIANTARG(pvar).ullVal:= ullVal;
    Result:= S_OK;
end;

function InitVariantFromDouble(dblVal: DOUBLE;
                               out pvar: OLEVARIANT): HResult; inline;
begin
    VARIANTARG(pvar).vt:= VT_R8;
    VARIANTARG(pvar).dblVal:= dblVal;
    Result:= S_OK;
end;

function InitVariantFromString(psz: LPCWSTR;
                               out pvar: OLEVARIANT): HResult; inline;
var
  hr: HResult;

begin
  VARIANTARG(pvar).vt:= VT_BSTR;
  VARIANTARG(pvar).bstrVal:= SysAllocString(psz);

  if VARIANTARG(pvar).bstrVal <> nil then
    hr:= S_OK
  else
  if psz <> nil then
    hr:= E_OUTOFMEMORY
  else
    hr:= E_INVALIDARG;

  if (FAILED(hr)) then
    VariantInit(pvar);

  Result:= hr;
end;


function InitVariantFromDispatch(pdisp: IDispatch;
                                 out pvar: OLEVARIANT): HResult; inline;
begin
  VARIANTARG(pvar).vt:= VT_DISPATCH;
  IDispatch(VARIANTARG(pvar).pdispVal):= pdisp;
  if (pvar.pdispVal = True) then
    Result:= S_OK
  else
    Result:= E_FAIL;
end;

// Creates a VT_DATE variant
function InitVariantFromDosDateTime(wDate: WORD;
                                    wTime: WORD;
                                    var pvar: OLEVARIANT): HResult; inline;
begin
  VARIANTARG(pvar).vt:= VT_DATE;
  if DosDateTimeToVariantTime(wDate, wTime, VARIANTARG(pvar).date) <> 0 then
    Result:= S_OK
  else
    Result:= S_FALSE;
end;

// Changes the value of a Component Object Model (COM) interface pointer and
// releases the previous interface.
function InitVariantFromUnknown(unknown: IUnknown;
                                var _variant: OLEVARIANT): HResult; inline;
begin
  VariantInit(_variant);
	VARIANTARG(_variant).vt:= VT_UNKNOWN;

  if (unknown <> Nil) then
    IUnknown_Set(VARIANTARG(_variant).unkVal,
                 unknown);

  Result:= S_OK;
end;


function InitPropVariantFromString(psz: LPCWSTR;
                                   out ppropvar: PROPVARIANT): HResult; inline;
begin
  ppropvar.vt:= VT_LPWSTR;
  Result:= SHStrDupW(psz, ppropvar.pwszVal);
  if FAILED(Result) then
      PropVariantInit(ppropvar);
end;

function InitPropVariantFromGUIDAsBuffer(const guid: REFGUID;
                                         out ppropvar: PROPVARIANT): HResult; inline;
begin
  Result:= InitPropVariantFromBuffer(@guid, sizeof(GUID), ppropvar);
end;

function IsPropVariantVector(const propvar: PROPVARIANT): BOOL; inline;
begin
  Result:= (propvar.vt and (VT_ARRAY or VT_VECTOR) <> 0);
end;

function IsPropVariantString(const propvar: PROPVARIANT): BOOL; inline;
begin
  Result:= (PropVariantToStringWithDefault(propvar, Nil) <> Nil);
end;

function InitVariantFromGUIDAsBuffer(const _guid: TGUID;
                                     var pvar: OleVariant): HResult; inline;
begin
  Result:= InitVariantFromBuffer(@_guid, sizeof(_guid), pvar);
end;

  //Implement additional prototypes here.

end.

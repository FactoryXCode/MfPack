// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.StructuredQueryCondition_2020.pas
// Kind: Pascal / Delphi unit
// Release date: 06-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This header is used by Windows Search.
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
// Remarks: This is the newest WinApi.StructuredQueryCondition.pas
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
// Source: structuredquerycondition.h
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
unit WinApi.StructuredQueryCondition_2020;

  {$HPPEMIT '#include "structuredquerycondition.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


// These types should be inside a a WINAPI_PARTITION_DESKTOP region as well but MIDL generates references
// to them that we cannot remove so they must exist outside of WINAPI_PARTITION_DESKTOP.

type
  tagCONDITION_TYPE = type DWord;
  {$EXTERNALSYM tagCONDITION_TYPE}
  CONDITION_TYPE = tagCONDITION_TYPE;
  {$EXTERNALSYM CONDITION_TYPE}
  PConditionType = ^tagCONDITION_TYPE;
const
  CT_AND_CONDITION  = CONDITION_TYPE(0);               // AND of subconditions
  {$EXTERNALSYM CT_AND_CONDITION}
  CT_OR_CONDITION   = CONDITION_TYPE(1);               // OR of subconditions
  {$EXTERNALSYM CT_OR_CONDITION}
  CT_NOT_CONDITION  = CONDITION_TYPE(2);               // NOT of a single subcondition
  {$EXTERNALSYM CT_NOT_CONDITION}
  CT_LEAF_CONDITION = CONDITION_TYPE(3);               // No subcondition: property, operation, value.
  {$EXTERNALSYM CT_LEAF_CONDITION}


type
  // Prefix CT
  tagCONDITION_OPERATION = type DWord;
  {$EXTERNALSYM tagCONDITION_OPERATION}
  CONDITION_OPERATION = tagCONDITION_OPERATION;
  {$EXTERNALSYM CONDITION_OPERATION}
  PConditionOperation = ^tagCONDITION_OPERATION;
const
  COP_IMPLICIT             = CONDITION_OPERATION(0);
  {$EXTERNALSYM COP_IMPLICIT}
  COP_EQUAL                = CONDITION_OPERATION(1);
  {$EXTERNALSYM COP_EQUAL}
  COP_NOTEQUAL             = CONDITION_OPERATION(2);
  {$EXTERNALSYM COP_NOTEQUAL}
  COP_LESSTHAN             = CONDITION_OPERATION(3);
  {$EXTERNALSYM COP_LESSTHAN}
  COP_GREATERTHAN          = CONDITION_OPERATION(4);
  {$EXTERNALSYM COP_GREATERTHAN}
  COP_LESSTHANOREQUAL      = CONDITION_OPERATION(5);
  {$EXTERNALSYM COP_LESSTHANOREQUAL}
  COP_GREATERTHANOREQUAL   = CONDITION_OPERATION(6);
  {$EXTERNALSYM COP_GREATERTHANOREQUAL}
  COP_VALUE_STARTSWITH     = CONDITION_OPERATION(7);     // LIKE FOO%
  {$EXTERNALSYM COP_VALUE_STARTSWITH}
  COP_VALUE_ENDSWITH       = CONDITION_OPERATION(8);     // LIKE %FOO
  {$EXTERNALSYM COP_VALUE_ENDSWITH}
  COP_VALUE_CONTAINS       = CONDITION_OPERATION(9);     // LIKE %FOO%
  {$EXTERNALSYM COP_VALUE_CONTAINS}
  COP_VALUE_NOTCONTAINS    = CONDITION_OPERATION(10);    // NOT LIKE %FOO%
  {$EXTERNALSYM COP_VALUE_NOTCONTAINS}
  COP_DOSWILDCARDS         = CONDITION_OPERATION(11);    // "DOS wildcards" and the like
  {$EXTERNALSYM COP_DOSWILDCARDS}
  COP_WORD_EQUAL           = CONDITION_OPERATION(12);    // Contains a word/phrase somewhere.
  {$EXTERNALSYM COP_WORD_EQUAL}
  COP_WORD_STARTSWITH      = CONDITION_OPERATION(13);    // Contains a word/phrase beginning with this
  {$EXTERNALSYM COP_WORD_STARTSWITH}
  COP_APPLICATION_SPECIFIC = CONDITION_OPERATION(14);    // Application specific, presumably uses the Value.
  {$EXTERNALSYM COP_APPLICATION_SPECIFIC}

  // Prefix COP

type

  // Interface IRichChunk
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRichChunk);'}
  {$EXTERNALSYM IRichChunk}
  IRichChunk = interface(IUnknown)
  ['{4FDEF69C-DBC9-454e-9910-B34F3C64B510}']

    function GetData(var pFirstPos: ULONG;
                     var pLength: ULONG;
                     var ppsz: PWideChar;
                     var pValue: PROPVARIANT): HResult; stdcall;
  end;
  IID_IRichChunk = IRichChunk;
  {$EXTERNALSYM IID_IRichChunk}
  SID_IRichChunk = IRichChunk;
  {$EXTERNALSYM SID_IRichChunk}


  // Interface ICondition
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICondition);'}
  {$EXTERNALSYM ICondition}
  ICondition = interface(IPersistStream)
  ['{0FC988D4-C935-4b97-A973-46282EA175C8}']

    function GetConditionType(var pNodeType: CONDITION_TYPE): HResult; stdcall;
    // For any node, return what kind of node it is.

    function GetSubConditions(const riid: REFIID;
                              var ppv: Pointer): HResult; stdcall;
    // riid must be IID_IEnumUnknown, IID_IEnumVARIANT or IID_IObjectArray, or in the case of a negation node IID_ICondition.
    // If this is a leaf node, E_FAIL will be returned.
    // If this is a negation node, then if riid is IID_ICondition, *ppv will be set to a single ICondition, otherwise an enumeration of one.
    // If this is a conjunction or a disjunction, *ppv will be set to an enumeration of the subconditions.

    function GetComparisonInfo(var ppszPropertyName: PWideChar;
                               var pcop: CONDITION_OPERATION;
                               var ppropvar: PROPVARIANT): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // Retrieve the property name, operation and value from the leaf node.
    // Any one of ppszPropertyName, pcop and ppropvar may be NULL.

    function GetValueType(var ppszValueTypeName: PWideChar): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // ppszValueTypeName will be set to the semantic type of the value, or to NULL if this is not meaningful.

    function GetValueNormalization(var ppszNormalization: PWideChar): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // If the value of the leaf node is VT_EMPTY, *ppszNormalization will be set to an empty string.
    // If the value is a string (VT_LPWSTR, VT_BSTR or VT_LPSTR), then *ppszNormalization will be set to a
    // character-normalized form of the value.
    // Otherwise, ppszNormalization will be set to some (character-normalized) string representation of the value.

    function GetInputTerms(out ppPropertyTerm: IRichChunk;
                           out ppOperationTerm: IRichChunk;
                           out ppValueTerm: IRichChunk): HResult; stdcall;
    // Return information about what parts of the input produced the property, the operation and the value.
    // Any one of ppPropertyTerm, ppOperationTerm and ppValueTerm may be NIL.
    // For a leaf node returned by the parser, the position information of each IRichChunk identifies the tokens that
    // contributed the property/operation/value, the string value is the corresponding part of the input string, and
    // the TMfpPropVariant is VT_EMPTY.

    function Clone(out ppc: ICondition): HResult; stdcall;
  end;
  IID_ICondition = ICondition;
  {$EXTERNALSYM IID_ICondition}
  SID_ICondition = ICondition;
  {$EXTERNALSYM SID_ICondition}


  // Interface ICondition2
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICondition2);'}
  {$EXTERNALSYM ICondition2}
  ICondition2 = interface(ICondition)
  ['{0DB8851D-2E5B-47eb-9208-D28C325A01D7}']

    function GetLocale(var ppszLocaleName: PWideChar): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // ppszLocaleName will be set to the locale name of the value,
    // which may be NIL.

    function GetLeafConditionInfo(var ppropkey: PROPVARIANT;
                                  var pcop: CONDITION_OPERATION;
                                  var ppropvar: PROPVARIANT): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // Retrieve the property key, operation and value from the leaf node.
    // Any one of ppropkey, pcop and ppropvar may be NIL.
  end;
  IID_ICondition2 = ICondition2;
  {$EXTERNALSYM IID_ICondition2}
  SID_ICondition2 = ICondition2;
  {$EXTERNALSYM SID_ICondition2}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.


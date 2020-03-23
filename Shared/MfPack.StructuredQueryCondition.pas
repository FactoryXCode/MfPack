// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.StructuredQueryCondition.pas
// Kind: Pascal / Delphi unit
// Release date: 06-10-2015
// Language: ENU
//
// Revision Version: 2.6.4
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
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: -
// 
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
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
unit MfPack.StructuredQueryCondition;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "structuredquerycondition.h"'}
  {$HPPEMIT ''}

interface

uses

  {MfPack}
  MfPack.MfpTypes,
  MfPack.PropIdl,
  MfPack.ObjIdl;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}


// These types should be inside a a WINAPI_PARTITION_DESKTOP region as well but MIDL generates references
// to them that we cannot remove so they must exist outside of WINAPI_PARTITION_DESKTOP.

type

  PConditionType = ^tagCONDITION_TYPE;
  tagCONDITION_TYPE = (
    CT_AND_CONDITION,               // AND of subconditions
    CT_OR_CONDITION,                // OR of subconditions
    CT_NOT_CONDITION,               // NOT of a single subcondition
    CT_LEAF_CONDITION               // No subcondition: property, operation, value.
  );
  {$EXTERNALSYM tagCONDITION_TYPE}
  CONDITION_TYPE = tagCONDITION_TYPE;
  {$EXTERNALSYM CONDITION_TYPE}

  // Prefix CT

  PConditionOperation = ^tagCONDITION_OPERATION;
  tagCONDITION_OPERATION = (
    COP_IMPLICIT,
    COP_EQUAL,
    COP_NOTEQUAL,
    COP_LESSTHAN,
    COP_GREATERTHAN,
    COP_LESSTHANOREQUAL,
    COP_GREATERTHANOREQUAL,
    COP_VALUE_STARTSWITH,       // LIKE FOO%
    COP_VALUE_ENDSWITH,         // LIKE %FOO
    COP_VALUE_CONTAINS,         // LIKE %FOO%
    COP_VALUE_NOTCONTAINS,      // NOT LIKE %FOO%
    COP_DOSWILDCARDS,           // "DOS wildcards" and the like
    COP_WORD_EQUAL,             // Contains a word/phrase somewhere.
    COP_WORD_STARTSWITH,        // Contains a word/phrase beginning with this
    COP_APPLICATION_SPECIFIC    // Application specific, presumably uses the Value.
  );
  {$EXTERNALSYM tagCONDITION_OPERATION}
  CONDITION_OPERATION = tagCONDITION_OPERATION;
  {$EXTERNALSYM CONDITION_OPERATION}
  // Prefix COP

type

  // Interface IRichChunk
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRichChunk);'}
  {$EXTERNALSYM IRichChunk}
  IRichChunk = interface(IUnknown)
  ['{4FDEF69C-DBC9-454e-9910-B34F3C64B510}']

    function GetData(out pFirstPos: ULONG;
                     out pLength: ULONG;
                     out ppsz: PWideChar;
                     out pValue: MfPROPVARIANT): HResult; stdcall;

    function RemoteGetData(out pFirstPos: ULONG;
                           out pLength: ULONG;
                           out ppsz: PWideChar;
                           out pValue: MfPROPVARIANT): HResult; stdcall;
  end;
  IID_IRichChunk = IRichChunk;
  {$EXTERNALSYM IID_IRichChunk}


  // Interface ICondition
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICondition);'}
  {$EXTERNALSYM ICondition}
  ICondition = interface(IPersistStream)
  ['{0FC988D4-C935-4b97-A973-46282EA175C8}']

    function GetConditionType(out pNodeType: CONDITION_TYPE): HResult; stdcall;
    // For any node, return what kind of node it is.

    function GetSubConditions(const riid: REFIID;
                              out ppv): HResult; stdcall;
    // riid must be IID_IEnumUnknown, IID_IEnumVARIANT or IID_IObjectArray, or in the case of a negation node IID_ICondition.
    // If this is a leaf node, E_FAIL will be returned.
    // If this is a negation node, then if riid is IID_ICondition, *ppv will be set to a single ICondition, otherwise an enumeration of one.
    // If this is a conjunction or a disjunction, *ppv will be set to an enumeration of the subconditions.

    function GetComparisonInfo(out ppszPropertyName: PWideChar;
                               out pcop: CONDITION_OPERATION;
                               out ppropvar: MfPROPVARIANT): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // Retrieve the property name, operation and value from the leaf node.
    // Any one of ppszPropertyName, pcop and ppropvar may be NULL.

    function RemoteGetComparisonInfo(out ppszPropertyName: PWideChar;
                                     out pcop: CONDITION_OPERATION;
                                     out ppropvar: MfPROPVARIANT): HResult; stdcall;

    function GetValueType(out ppszValueTypeName: PWideChar): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // ppszValueTypeName will be set to the semantic type of the value, or to NULL if this is not meaningful.

    function GetValueNormalization(out ppszNormalization: PWideChar): HResult; stdcall;
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

    function RemoteGetInputTerms(out ppPropertyTerm: IRichChunk;
                                 out ppOperationTerm: IRichChunk;
                                 out ppValueTerm: IRichChunk): HResult; stdcall;

    function Clone(out ppc: ICondition): HResult; stdcall;

  end;
  IID_ICondition = ICondition;
  {$EXTERNALSYM IID_ICondition}


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

    function GetLeafConditionInfo(out ppropkey: MfPROPVARIANT;
                                  out pcop: CONDITION_OPERATION;
                                  out ppropvar: MfPROPVARIANT): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // Retrieve the property key, operation and value from the leaf node.
    // Any one of ppropkey, pcop and ppropvar may be NIL.

    function RemoteGetLeafConditionInfo(out ppropkey: PROPERTYKEY;
                                        out pcop: CONDITION_OPERATION;
                                        out ppropvar: MfPROPVARIANT): HResult; stdcall;
    // If this is not a leaf node, E_FAIL will be returned.
    // Retrieve the property key, operation and value from the leaf node.
    // Any one of ppropkey, pcop and ppropvar may be NIL.

  end;
  IID_ICondition2 = ICondition2;
  {$EXTERNALSYM IID_ICondition2}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.

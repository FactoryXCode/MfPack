// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.PropSys.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description:
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
// Source: propsys.h
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
unit WinApi.ActiveX.PropSys;

  {$HPPEMIT '#include "ObjIdlbase.h"'}

interface

uses

  {WinApi}
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.StructuredQueryCondition,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.OaIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  PKEY_PIDSTR_MAX                     = 10;  // will take care of any long integer value
  {$EXTERNALSYM PKEY_PIDSTR_MAX}
  GUIDSTRING_MAX                      = (1 + 8 + 1 + 4 + 1 + 4 + 1 + 4 + 1 + 12 + 1 + 1);  // "{12345678-1234-1234-1234-123456789012}"
  {$EXTERNALSYM GUIDSTRING_MAX}
  PKEYSTR_MAX                         = (GUIDSTRING_MAX + 1 + PKEY_PIDSTR_MAX);
  {$EXTERNALSYM PKEYSTR_MAX}

  // IPersistSerializedPropStorage
  SERIALIZEDPROPSTORAGE_VERSION_VISTA = $53505331;  // SPS1 in ASCII")
  {$EXTERNALSYM SERIALIZEDPROPSTORAGE_VERSION_VISTA}

  PDFF_PREFERFRIENDLY                 = $10000000;  // Use even friendlier date/time descriptions ;internal
  {$EXTERNALSYM PDFF_PREFERFRIENDLY}


type
  REFPROPERTYKEY = PROPERTYKEY;
  {$EXTERNALSYM REFPROPERTYKEY}


 // ActiveX IStream interface pointer
{$IFNDEF _PIStream_DEFINED}
{$DEFINE _PIStream_DEFINED}
  PIStream = ^IStream;
  {$EXTERNALSYM PIStream}
{$ENDIF}

type
  // The following are new for Vista, but are use in downlevel components
  PGETPROPERTYSTOREFLAGS = ^GETPROPERTYSTOREFLAGS;
  GETPROPERTYSTOREFLAGS = DWord;
  {$EXTERNALSYM GETPROPERTYSTOREFLAGS}
  // If no flags are specified (GPS_DEFAULT), a read-only property store is returned that includes properties for the file or item.
  // In the case that the shell item is a file, the property store contains:
  //     1. properties about the file from the file system
  //     2. properties from the file itself provided by the file's property handler, unless that file is offline,
  //         see GPS_OPENSLOWITEM
  //     3. if requested by the file's property handler and supported by the file system, properties stored in the
  //     alternate property store.
  //
  // Non-file shell items should return a similar read-only store
  //
  // Specifying other GPS_ flags modifies the store that is returned
const
  GPS_DEFAULT                 = GETPROPERTYSTOREFLAGS($0);
  {$EXTERNALSYM GPS_DEFAULT}
  GPS_HANDLERPROPERTIESONLY   = GETPROPERTYSTOREFLAGS($1);    // only include properties directly from the file's property handler
  {$EXTERNALSYM GPS_HANDLERPROPERTIESONLY}
  GPS_READWRITE               = GETPROPERTYSTOREFLAGS($2);    // Writable stores will only include handler properties
  {$EXTERNALSYM GPS_READWRITE}
  GPS_TEMPORARY               = GETPROPERTYSTOREFLAGS($4);    //  A read/write store that only holds properties for the lifetime of the IShellItem object
  {$EXTERNALSYM GPS_TEMPORARY}
  GPS_FASTPROPERTIESONLY      = GETPROPERTYSTOREFLAGS($8);    // do not include any properties from the file's property handler (because the file's property handler will hit the disk)
  {$EXTERNALSYM GPS_FASTPROPERTIESONLY}
  GPS_OPENSLOWITEM            = GETPROPERTYSTOREFLAGS($10);   // include properties from a file's property handler, even if it means retrieving the file from offline storage.
  {$EXTERNALSYM GPS_OPENSLOWITEM}
  GPS_DELAYCREATION           = GETPROPERTYSTOREFLAGS($20);   // delay the creation of the file's property handler until those properties are read, written, or enumerated
  {$EXTERNALSYM GPS_DELAYCREATION}
  GPS_BESTEFFORT              = GETPROPERTYSTOREFLAGS($40);   // For readonly stores, succeed and return all available properties, even if one or more sources of properties fails. Not valid with GPS_READWRITE.
  {$EXTERNALSYM GPS_BESTEFFORT}
  GPS_NO_OPLOCK               = GETPROPERTYSTOREFLAGS($80);   // some data sources protect the read property store with an oplock, this disables that
  {$EXTERNALSYM GPS_NO_OPLOCK}
  GPS_PREFERQUERYPROPERTIES   = GETPROPERTYSTOREFLAGS($100);  // For file system WDS results, only retrieve properties from the indexer
  {$EXTERNALSYM GPS_PREFERQUERYPROPERTIES}
  GPS_EXTRINSICPROPERTIES     = GETPROPERTYSTOREFLAGS($200);  // include properties from the file's secondary stream
  {$EXTERNALSYM GPS_EXTRINSICPROPERTIES}
  GPS_EXTRINSICPROPERTIESONLY = GETPROPERTYSTOREFLAGS($400);  // only include properties from the file's secondary stream
  {$EXTERNALSYM GPS_EXTRINSICPROPERTIESONLY}
  GPS_VOLATILEPROPERTIES	    = GETPROPERTYSTOREFLAGS($800);  // include properties from the file's volatile store in the indexer
  {$EXTERNALSYM GPS_VOLATILEPROPERTIES}
  GPS_VOLATILEPROPERTIESONLY	= GETPROPERTYSTOREFLAGS($1000); // only include properties from the file's volatile store in the indexer
  {$EXTERNALSYM GPS_VOLATILEPROPERTIESONLY}
  GPS_MASK_VALID	            = GETPROPERTYSTOREFLAGS($1fff);
  {$EXTERNALSYM GPS_MASK_VALID}


type
  PPKA_FLAGS = ^PKA_FLAGS;
  PKA_FLAGS = DWord;
  {$EXTERNALSYM PKA_FLAGS}
   // note, this enum type not named property, it should have been named PKA_OPERATION
const
  PKA_SET    = PKA_FLAGS(0);    // replace current value
  {$EXTERNALSYM PKA_SET}
  PKA_APPEND = PKA_FLAGS(1);    // append to current value - multi-value properties only
  {$EXTERNALSYM PKA_APPEND}
  PKA_DELETE = PKA_FLAGS(2);    // delete from current value - multi-value properties only
  {$EXTERNALSYM PKA_DELETE}


type
  // IPropertyDescription types
  // ==========================
  //
  PPROPDESC_TYPE_FLAGS = ^PROPDESC_TYPE_FLAGS;
  PROPDESC_TYPE_FLAGS = DWord;
  {$EXTERNALSYM PROPDESC_TYPE_FLAGS}
const
  PDTF_DEFAULT                    = PROPDESC_TYPE_FLAGS($0);
  {$EXTERNALSYM PDTF_DEFAULT}
  PDTF_MULTIPLEVALUES             = PROPDESC_TYPE_FLAGS($1);     // This property can have multiple values (as VT_VECTOR)
  {$EXTERNALSYM PDTF_MULTIPLEVALUES}
  PDTF_ISINNATE                   = PROPDESC_TYPE_FLAGS($2);     // This property cannot be written to
  {$EXTERNALSYM PDTF_ISINNATE}
  PDTF_ISGROUP                    = PROPDESC_TYPE_FLAGS($4);     // This property is a group heading
  {$EXTERNALSYM PDTF_ISGROUP}
  PDTF_CANGROUPBY                 = PROPDESC_TYPE_FLAGS($8);     // The user can group by this property
  {$EXTERNALSYM PDTF_CANGROUPBY}
  PDTF_CANSTACKBY                 = PROPDESC_TYPE_FLAGS($10);    // The user can stack by this property
  {$EXTERNALSYM PDTF_CANSTACKBY}
  PDTF_ISTREEPROPERTY             = PROPDESC_TYPE_FLAGS($20);    // This property contains a hierarchy
  {$EXTERNALSYM PDTF_ISTREEPROPERTY}
  PDTF_INCLUDEINFULLTEXTQUERY     = PROPDESC_TYPE_FLAGS($40);    // Deprecated
  {$EXTERNALSYM PDTF_INCLUDEINFULLTEXTQUERY}
  PDTF_ISVIEWABLE                 = PROPDESC_TYPE_FLAGS($80);    // This property is meant to be viewed by the user
  {$EXTERNALSYM PDTF_ISVIEWABLE}
  PDTF_ISQUERYABLE                = PROPDESC_TYPE_FLAGS($100);   // Deprecated
  {$EXTERNALSYM PDTF_ISQUERYABLE}
  PDTF_CANBEPURGED                = PROPDESC_TYPE_FLAGS($200);   // This property can be purged, even if it is innate (property handler should respect this)
  {$EXTERNALSYM PDTF_CANBEPURGED}
  PDTF_SEARCHRAWVALUE             = PROPDESC_TYPE_FLAGS($400);   // The raw (rather than formatted) value of this property should be used for searching
  {$EXTERNALSYM PDTF_SEARCHRAWVALUE}
  PDTF_DONTCOERCEEMPTYSTRINGS     = PROPDESC_TYPE_FLAGS($800);   // Don't coerce empty strings into null/empty
  {$EXTERNALSYM PDTF_DONTCOERCEEMPTYSTRINGS}
  PDTF_ALWAYSINSUPPLEMENTALSTORE  = PROPDESC_TYPE_FLAGS($1000);  // Property is persisted into supplemental store, not file format property handler's store
  {$EXTERNALSYM PDTF_ALWAYSINSUPPLEMENTALSTORE}
  PDTF_ISSYSTEMPROPERTY           = PROPDESC_TYPE_FLAGS($80000000);  // This property is owned by the system
  {$EXTERNALSYM PDTF_ISSYSTEMPROPERTY}
  PDTF_MASK_ALL                   = PROPDESC_TYPE_FLAGS($80001FFF);
  {$EXTERNALSYM PDTF_MASK_ALL}

type
  PPROPDESC_VIEW_FLAGS = ^PROPDESC_VIEW_FLAGS;
  PROPDESC_VIEW_FLAGS = DWord;
  {$EXTERNALSYM PROPDESC_VIEW_FLAGS}
const
  PDVF_DEFAULT             = PROPDESC_VIEW_FLAGS(0);
  {$EXTERNALSYM PDVF_DEFAULT}
  PDVF_CENTERALIGN         = PROPDESC_VIEW_FLAGS($1);
  {$EXTERNALSYM PDVF_CENTERALIGN}
  PDVF_RIGHTALIGN          = PROPDESC_VIEW_FLAGS($2);
  {$EXTERNALSYM PDVF_RIGHTALIGN}
  PDVF_BEGINNEWGROUP       = PROPDESC_VIEW_FLAGS($4);
  {$EXTERNALSYM PDVF_BEGINNEWGROUP}
  PDVF_FILLAREA            = PROPDESC_VIEW_FLAGS($8);
  {$EXTERNALSYM PDVF_FILLAREA}
  PDVF_SORTDESCENDING      = PROPDESC_VIEW_FLAGS($10);
  {$EXTERNALSYM PDVF_SORTDESCENDING}
  PDVF_SHOWONLYIFPRESENT   = PROPDESC_VIEW_FLAGS($20);
  {$EXTERNALSYM PDVF_SHOWONLYIFPRESENT}
  PDVF_SHOWBYDEFAULT       = PROPDESC_VIEW_FLAGS($40);
  {$EXTERNALSYM PDVF_SHOWBYDEFAULT}
  PDVF_SHOWINPRIMARYLIST   = PROPDESC_VIEW_FLAGS($80);
  {$EXTERNALSYM PDVF_SHOWINPRIMARYLIST}
  PDVF_SHOWINSECONDARYLIST = PROPDESC_VIEW_FLAGS($100);
  {$EXTERNALSYM PDVF_SHOWINSECONDARYLIST}
  PDVF_HIDELABEL           = PROPDESC_VIEW_FLAGS($200);
  {$EXTERNALSYM PDVF_HIDELABEL}
  PDVF_HIDDEN              = PROPDESC_VIEW_FLAGS($800);
  {$EXTERNALSYM PDVF_HIDDEN}
  PDVF_CANWRAP             = PROPDESC_VIEW_FLAGS($1000);
  {$EXTERNALSYM PDVF_CANWRAP}
  PDVF_MASK_ALL            = PROPDESC_VIEW_FLAGS($1BFF);
  {$EXTERNALSYM PDVF_MASK_ALL}



type
  PPROPDESC_FORMAT_FLAGS = ^PROPDESC_FORMAT_FLAGS;
  PROPDESC_FORMAT_FLAGS = DWord;
  {$EXTERNALSYM PROPDESC_FORMAT_FLAGS}
const
  PDFF_DEFAULT              = PROPDESC_FORMAT_FLAGS($00000000);
  {$EXTERNALSYM PDFF_DEFAULT}
  PDFF_PREFIXNAME           = PROPDESC_FORMAT_FLAGS($00000001);  // Prefix the value with the property name
  {$EXTERNALSYM PDFF_PREFIXNAME}
  PDFF_FILENAME             = PROPDESC_FORMAT_FLAGS($00000002);  // Treat as a file name
  {$EXTERNALSYM PDFF_FILENAME}
  PDFF_ALWAYSKB             = PROPDESC_FORMAT_FLAGS($00000004);  // Always format byte sizes as KB
  {$EXTERNALSYM PDFF_ALWAYSKB}
  PDFF_RESERVED_RIGHTTOLEFT = PROPDESC_FORMAT_FLAGS($00000008);  // Reserved for legacy use.
  {$EXTERNALSYM PDFF_RESERVED_RIGHTTOLEFT}
  PDFF_SHORTTIME            = PROPDESC_FORMAT_FLAGS($00000010);  // Show time as "5:17 pm"
  {$EXTERNALSYM PDFF_SHORTTIME}
  PDFF_LONGTIME             = PROPDESC_FORMAT_FLAGS($00000020);  // Show time as "5:17:14 pm"
  {$EXTERNALSYM PDFF_LONGTIME}
  PDFF_HIDETIME             = PROPDESC_FORMAT_FLAGS($00000040);  // Hide the time-portion of the datetime
  {$EXTERNALSYM PDFF_HIDETIME}
  PDFF_SHORTDATE            = PROPDESC_FORMAT_FLAGS($00000080);  // Show date as "3/21/04"
  {$EXTERNALSYM PDFF_SHORTDATE}
  PDFF_LONGDATE             = PROPDESC_FORMAT_FLAGS($00000100);  // Show date as "Monday, March 21, 2004"
  {$EXTERNALSYM PDFF_LONGDATE}
  PDFF_HIDEDATE             = PROPDESC_FORMAT_FLAGS($00000200);  // Hide the date-portion of the datetime
  {$EXTERNALSYM PDFF_HIDEDATE}
  PDFF_RELATIVEDATE         = PROPDESC_FORMAT_FLAGS($00000400);  // Use friendly date descriptions like "Yesterday"
  {$EXTERNALSYM PDFF_RELATIVEDATE}
  PDFF_USEEDITINVITATION    = PROPDESC_FORMAT_FLAGS($00000800);  // Use edit invitation text if failed or empty
  {$EXTERNALSYM PDFF_USEEDITINVITATION}
  PDFF_READONLY             = PROPDESC_FORMAT_FLAGS($00001000);  // Use readonly format, fill with default text if empty and !PDFF_FAILIFEMPTYPROP
  {$EXTERNALSYM PDFF_READONLY}
  PDFF_NOAUTOREADINGORDER   = PROPDESC_FORMAT_FLAGS($00002000);   // Don't detect reading order automatically. Useful if you will be converting to Ansi and don't want Unicode reading order characters
  {$EXTERNALSYM PDFF_NOAUTOREADINGORDER}


type
  // IPropertyDescriptionSearchInfo types
  // ====================================
  //
  // IPropertyDescriptionSearchInfo
  PPROPDESC_SEARCHINFO_FLAGS = ^PROPDESC_SEARCHINFO_FLAGS;
  PROPDESC_SEARCHINFO_FLAGS = DWord;
  {$EXTERNALSYM PROPDESC_SEARCHINFO_FLAGS}
const
  PDSIF_DEFAULT         = PROPDESC_SEARCHINFO_FLAGS($00000000);
  {$EXTERNALSYM PDSIF_DEFAULT}
  PDSIF_ININVERTEDINDEX = PROPDESC_SEARCHINFO_FLAGS($00000001);
  {$EXTERNALSYM PDSIF_ININVERTEDINDEX}
  PDSIF_ISCOLUMN        = PROPDESC_SEARCHINFO_FLAGS($00000002);
  {$EXTERNALSYM PDSIF_ISCOLUMN}
  PDSIF_ISCOLUMNSPARSE  = PROPDESC_SEARCHINFO_FLAGS($00000004);
  {$EXTERNALSYM PDSIF_ISCOLUMNSPARSE}
  PDSIF_ALWAYSINCLUDE   = PROPDESC_SEARCHINFO_FLAGS($00000008);
  {$EXTERNALSYM PDSIF_ALWAYSINCLUDE}
  PDSIF_USEFORTYPEAHEAD = PROPDESC_SEARCHINFO_FLAGS($00000010);
  {$EXTERNALSYM PDSIF_USEFORTYPEAHEAD}


type
  // PERSIST_SPROPSTORE_FLAGS should be converted to use DEFINE_ENUM_FLAG_OPERATORS() but some callers pass "0"
  // as the value of this to existing APIs.
  // Those callers need to change to use FPSPS_DEFAULT.
  PPERSIST_SPROPSTORE_FLAGS = ^PERSIST_SPROPSTORE_FLAGS;
  _PERSIST_SPROPSTORE_FLAGS = DWord;
  {$EXTERNALSYM _PERSIST_SPROPSTORE_FLAGS}
  PERSIST_SPROPSTORE_FLAGS = _PERSIST_SPROPSTORE_FLAGS;
  {$EXTERNALSYM PERSIST_SPROPSTORE_FLAGS}
const
  FPSPS_DEFAULT                   = PERSIST_SPROPSTORE_FLAGS($00000000);  // Windows 7 and later
  {$EXTERNALSYM FPSPS_DEFAULT}
  FPSPS_READONLY                  = PERSIST_SPROPSTORE_FLAGS($00000001);
  {$EXTERNALSYM FPSPS_READONLY}
  FPSPS_TREAT_NEW_VALUES_AS_DIRTY = PERSIST_SPROPSTORE_FLAGS($00000002);   // >= Win 8
  {$EXTERNALSYM FPSPS_TREAT_NEW_VALUES_AS_DIRTY}



type

  //============================= INTERFACES ===================================



  // Interface IInitializeWithFile
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeWithFile);'}
  {$EXTERNALSYM IInitializeWithFile}
  IInitializeWithFile = interface(IUnknown)
  ['{b7d14566-0509-4cce-a71f-0a554233bd9b}']
    function Initialize(pszFilePath: PAnsiChar;
                        grfMode: DWord): HRESULT; stdcall;

  end;
  IID_IInitializeWithFile = IInitializeWithFile;
  {$EXTERNALSYM IID_IInitializeWithFile}



  // Interface IInitializeWithStream
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeWithStream);'}
  {$EXTERNALSYM IInitializeWithStream}
   IInitializeWithStream = interface(IUnknown)
   ['{b824b49d-22ac-4161-ac8a-9916e8fa3f7f}']
     function Initialize(var pIStream: IStream;
                         grfMode: DWord): HRESULT; stdcall;

   end;
  IID_IInitializeWithStream = IInitializeWithStream;
  {$EXTERNALSYM IID_IInitializeWithStream}



  // Interface IPropertyStore
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyStore);'}
  {$EXTERNALSYM IPropertyStore}
   IPropertyStore = interface(IUnknown)
   ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
     function GetCount(out cProps: DWORD): HResult; stdcall;

     function GetAt(iProp: DWORD;
                    out pkey: PPROPERTYKEY): HResult; stdcall;

     function GetValue(const key: PROPERTYKEY;
                       out pv: PROPVARIANT): HResult; stdcall;

     function SetValue(const key: PROPERTYKEY;
                       const propvar: PROPVARIANT): HResult; stdcall;

     function Commit: HResult; stdcall;

   end;
  IID_IPropertyStore = IPropertyStore;
  {$EXTERNALSYM IID_IPropertyStore}



  // Interface INamedPropertyStore
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INamedPropertyStore);'}
  {$EXTERNALSYM INamedPropertyStore}
  INamedPropertyStore = interface(IUnknown)
  ['{71604b0f-97b0-4764-8577-2f13e98a1422}']

    function GetNamedValue(pszName: LPOLESTR;
                           var ppropvar: PROPVARIANT): HResult; stdcall;

    function SetNamedValue(pszName: LPOLESTR;
                           propvar: REFPROPVARIANT): HResult; stdcall;

    function GetNameCount(var pdwCount: DWORD): HResult; stdcall;

    function GetNameAt(iProp: DWORD;
                       var pbstrName: BSTR // The calling application's responsibility to free this resource when it is no longer needed.
                       ): HResult; stdcall;

  end;
  IID_INamedPropertyStore = INamedPropertyStore;
  {$EXTERNALSYM IID_INamedPropertyStore}



  // Interface IObjectWithPropertyKey
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithPropertyKey);'}
  {$EXTERNALSYM IObjectWithPropertyKey}
  IObjectWithPropertyKey = interface(IUnknown)
  ['{fc0ca0a7-c316-4fd2-9031-3e628e6d4f23}']

    function SetPropertyKey(key: REFPROPERTYKEY): HResult; stdcall;

    function GetPropertyKey(out pkey: PROPERTYKEY): HResult; stdcall;

  end;
  IID_IObjectWithPropertyKey = IObjectWithPropertyKey;
  {$EXTERNALSYM IID_IObjectWithPropertyKey}


  // Interface IPropertyChange
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyChange);'}
  {$EXTERNALSYM IPropertyChange}
  IPropertyChange = interface(IUnknown)
  ['{f917bc8a-1bba-4478-a245-1bde03eb9431}']

    function ApplyToPropVariant(propvarIn: REFPROPVARIANT;
                                out ppropvarOut: PROPVARIANT): HResult; stdcall;

  end;
  IID_IPropertyChange = IPropertyChange;
  {$EXTERNALSYM IID_IPropertyChange}



  // Interface IPropertyChangeArray
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyChangeArray);'}
  {$EXTERNALSYM IPropertyChangeArray}
  IPropertyChangeArray = interface(IUnknown)
  ['{380f5cad-1b5e-42f2-805d-637fd392d31e}']

    function GetCount(out pcOperations: UINT): HResult; stdcall;

    function GetAt(iIndex: UINT;
                   const riid: REFIID;
                   out ppv): HResult; stdcall;

    function InsertAt(iIndex: UINT;
                      ppropChange: IPropertyChange): HResult; stdcall;

    function Append(ppropChange: IPropertyChange): HResult; stdcall;
    // If the PROPERTYKEY for that change was not in the Array, then add it
    // else if the PROPERTYKEY was already part of the Array then
    // replace the first occurence of that PROPERTYKEY with the new ppropChange
    function AppendOrReplace(ppropChange: IPropertyChange): HResult; stdcall;

    function RemoveAt(iIndex: UINT): HResult; stdcall;

    function IsKeyInArray(key: REFPROPERTYKEY): HResult; stdcall;

  end;
  IID_IPropertyChangeArray = IPropertyChangeArray;
  {$EXTERNALSYM IID_IPropertyChangeArray}



  // Interface IPropertyStoreCapabilities
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyStoreCapabilities);'}
  {$EXTERNALSYM IPropertyStoreCapabilities}
  IPropertyStoreCapabilities = interface(IUnknown)
  ['{c8e2d566-186e-4d49-bf41-6909ead56acc}']

    function IsPropertyWritable(key: REFPROPERTYKEY): HResult; stdcall;

  end;
  IID_IPropertyStoreCapabilities = IPropertyStoreCapabilities;
  {$EXTERNALSYM IID_IPropertyStoreCapabilities}



  // Interface IPropertyStoreCache
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyStoreCache);'}
  {$EXTERNALSYM IPropertyStoreCache}
  IPropertyStoreCache = interface(IPropertyStore)
  ['{3017056d-9a91-4e90-937d-746c72abbf4f}']

  end;
  IID_IPropertyStoreCache = IPropertyStoreCache;
  {$EXTERNALSYM IID_IPropertyStoreCache}


  PPSC_STATE = ^PSC_STATE;
  PSC_STATE         = (
    PSC_NORMAL      = 0,
    PSC_NOTINSOURCE = 1,
    PSC_DIRTY       = 2,
    PSC_READONLY    = 3
  );
  {$EXTERNALSYM PSC_STATE}


  PPROPENUMTYPE = ^PROPENUMTYPE;
  PROPENUMTYPE        = (
    PET_DISCRETEVALUE = 0,           // Use GetValue  GetDisplayText
    PET_RANGEDVALUE   = 1,           // Use GetRangeValues  GetDisplayText
    PET_DEFAULTVALUE  = 2,           // Use GetDisplayText
    PET_ENDRANGE      = 3            // Use GetValue
  );
  {$EXTERNALSYM PROPENUMTYPE}


  // Interface IPropertyEnumType
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyEnumType);'}
  {$EXTERNALSYM IPropertyEnumType}
  IPropertyEnumType = interface(IUnknown)
  ['{11e1fbf9-2d56-4a6b-8db3-7cd193a471f2}']

    function GetEnumType(out penumtype: PROPENUMTYPE): HResult; stdcall;

    function GetValue(out ppropvar: PROPVARIANT): HResult; stdcall;

    function GetRangeMinValue(out ppropvarMin: PROPVARIANT): HResult; stdcall;

    function GetRangeSetValue(out ppropvarSet: PROPVARIANT): HResult; stdcall;

    function GetDisplayText(out ppszDisplay: LPOLESTR): HResult; stdcall;

  end;
  IID_IPropertyEnumType = IPropertyEnumType;
  {$EXTERNALSYM IID_IPropertyEnumType}



  // Interface IPropertyEnumType2
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyEnumType2);'}
  {$EXTERNALSYM IPropertyEnumType2}
  IPropertyEnumType2 = interface(IPropertyEnumType)
  ['{9b6e051c-5ddd-4321-9070-fe2acb55e794}']

    function GetImageReference(var ppszImageRes: LPOLESTR): HResult; stdcall;
    // returns a string of the form "<dll name>,-<resid>" that is suitable to be passed to PathParseIconLocation()

  end;
  IID_IPropertyEnumType2 = IPropertyEnumType2;
  {$EXTERNALSYM IID_IPropertyEnumType2}



  // Interface IPropertyEnumTypeList
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyEnumTypeList);'}
  {$EXTERNALSYM IPropertyEnumTypeList}
  IPropertyEnumTypeList = interface(IUnknown)
  ['{a99400f4-3d84-4557-94ba-1242fb2cc9a6}']
    function GetCount(out pctypes: UINT): HResult; stdcall;

    function GetAt(itype: UINT;
                   const riid: REFIID;  // riid may be IID_IPropertyEnumType
                   out ppv): HResult; stdcall;

    function GetConditionAt(nIndex: UINT;
                            const riid: REFIID;
                            out ppv): HResult; stdcall;

    function FindMatchingIndex(propvarCmp: REFPROPVARIANT;
                               out pnIndex: UINT): HResult; stdcall;

  end;
  IID_IPropertyEnumTypeList = IPropertyEnumTypeList;
  {$EXTERNALSYM IID_IPropertyEnumTypeList}


  PPROPDESC_DISPLAYTYPE = ^PROPDESC_DISPLAYTYPE;
  PROPDESC_DISPLAYTYPE = (
    PDDT_STRING     = 0,
    PDDT_NUMBER     = 1,
    PDDT_BOOLEAN    = 2,
    PDDT_DATETIME   = 3,
    PDDT_ENUMERATED = 4         // Use GetEnumTypeList
  );
  {$EXTERNALSYM PROPDESC_DISPLAYTYPE}

  PPROPDESC_GROUPING_RANGE = ^PROPDESC_GROUPING_RANGE;
  PROPDESC_GROUPING_RANGE = (
    PDGR_DISCRETE     = 0,              // Display individual values
    PDGR_ALPHANUMERIC = 1,              // Display static alphanumeric ranges for values
    PDGR_SIZE         = 2,              // Display static size ranges for values
    PDGR_DYNAMIC      = 3,              // Display dynamically created ranges for the values
    PDGR_DATE         = 4,              // Display month/year groups
    PDGR_PERCENT      = 5,              // Display percent buckets
    PDGR_ENUMERATED   = 6               // Display buckets from GetEnumTypeList
  );
  {$EXTERNALSYM PROPDESC_GROUPING_RANGE}

  PPROPDESC_SORTDESCRIPTION = ^PROPDESC_SORTDESCRIPTION;
  PROPDESC_SORTDESCRIPTION = (
    PDSD_GENERAL          = 0,
    PDSD_A_Z              = 1,
    PDSD_LOWEST_HIGHEST   = 2,
    PDSD_SMALLEST_BIGGEST = 3,
    PDSD_OLDEST_NEWEST    = 4
  );
  {$EXTERNALSYM PROPDESC_SORTDESCRIPTION}


  PPropdescRelativedescriptionType = ^PROPDESC_RELATIVEDESCRIPTION_TYPE;
  PROPDESC_RELATIVEDESCRIPTION_TYPE = (
    PDRDT_GENERAL  = 0,
    PDRDT_DATE     = 1,
    PDRDT_SIZE     = 2,
    PDRDT_COUNT    = 3,
    PDRDT_REVISION = 4,
    PDRDT_LENGTH   = 5,
    PDRDT_DURATION = 6,
    PDRDT_SPEED    = 7,
    PDRDT_RATE     = 8,
    PDRDT_RATING   = 9,
    PDRDT_PRIORITY = 10
  );
  {$EXTERNALSYM PROPDESC_RELATIVEDESCRIPTION_TYPE}


  PPROPDESC_AGGREGATION_TYPE = ^PROPDESC_AGGREGATION_TYPE;
  PROPDESC_AGGREGATION_TYPE = (
    PDAT_DEFAULT   = 0,               // Display "multiple-values"
    PDAT_FIRST     = 1,               // Display first property value in the selection.
    PDAT_SUM       = 2,               // Display the numerical sum of the values. This is never returned for VT_LPWSTR, VT_BOOL, and VT_FILETIME types.
    PDAT_AVERAGE   = 3,               // Display the numerical average of the values. This is never returned for VT_LPWSTR, VT_BOOL, and VT_FILETIME types.
    PDAT_DATERANGE = 4,               // Display the date range of the values. This is only returned for VT_FILETIME types.
    PDAT_UNION     = 5,               // Display values as union of all values. The order is undefined.
    PDAT_MAX       = 6,               // Displays the maximum of all the values.
    PDAT_MIN       = 7                // Displays the minimum of all the values.
  );
  {$EXTERNALSYM PROPDESC_AGGREGATION_TYPE}


  PPROPDESC_CONDITION_TYPE = ^PROPDESC_CONDITION_TYPE;
  PROPDESC_CONDITION_TYPE = (
    PDCOT_NONE     = 0,
    PDCOT_STRING   = 1,
    PDCOT_SIZE     = 2,
    PDCOT_DATETIME = 3,
    PDCOT_BOOLEAN  = 4,
    PDCOT_NUMBER   = 5
  );
  {$EXTERNALSYM PROPDESC_CONDITION_TYPE}


  // Interface IPropertyDescription
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyDescription);'}
  {$EXTERNALSYM IPropertyDescription}
  IPropertyDescription = interface(IUnknown)
  ['{6f79d558-3e96-4549-a1d1-7d75d2288814}']
    function GetPropertyKey(out pkey: PROPERTYKEY): HResult; stdcall;

    function GetCanonicalName(out ppszName: LPOLESTR): HResult; stdcall;

    function GetPropertyType(out pvartype: VARTYPE): HResult; stdcall;

    function GetDisplayName(out ppszName: LPOLESTR): HResult; stdcall;

    function GetEditInvitation(out ppszInvite: LPOLESTR): HResult; stdcall;

    function GetTypeFlags(mask: PROPDESC_TYPE_FLAGS;
                          out ppdtFlags: PROPDESC_TYPE_FLAGS): HResult; stdcall;

    function GetViewFlags(out ppdvFlags: PROPDESC_VIEW_FLAGS): HResult; stdcall;

    function GetDefaultColumnWidth(out pcxChars: UINT): HResult; stdcall;

    function GetDisplayType(out pdisplaytype: PROPDESC_DISPLAYTYPE): HResult; stdcall;

    function GetColumnState(out pcsFlags: SHCOLSTATEF): HResult; stdcall;

    // Needs to correspond to bits in SHCOLSTATE_TYPEMASK
    function GetGroupingRange(out pgr: PROPDESC_GROUPING_RANGE): HResult; stdcall;

    function GetRelativeDescriptionType(out prdt: PROPDESC_RELATIVEDESCRIPTION_TYPE): HResult; stdcall;

    function GetRelativeDescription(propvar1: REFPROPVARIANT;
                                    propvar2: REFPROPVARIANT;
                                    out ppszDesc1: LPOLESTR;
                                    out ppszDesc2: LPOLESTR): HResult; stdcall;

    function GetSortDescription(out psd: PROPDESC_SORTDESCRIPTION): HResult; stdcall;

    function GetSortDescriptionLabel(fDescending: BOOL;
                                     out ppszDescription: LPOLESTR): HResult; stdcall;

    function GetAggregationType(out paggtype: PROPDESC_AGGREGATION_TYPE): HResult; stdcall;

    function GetConditionType(out pcontype: PROPDESC_CONDITION_TYPE;
                              out popDefault: CONDITION_OPERATION): HResult; stdcall;

    // Returns an IPropertyEnumTypeList interface
    function GetEnumTypeList(const riid: REFIID;
                             out ppv): HResult; stdcall;

    function CoerceToCanonicalValue(var ppropvar: PROPVARIANT): HResult; stdcall;

    function RemoteCoerceToCanonicalValue(const propvar: REFPROPVARIANT;
                                          out ppropvar: PROPVARIANT): HResult; stdcall;

    function FormatForDisplay(propvar: REFPROPVARIANT;
                              pdfFlags: PROPDESC_FORMAT_FLAGS;
                              out ppszDisplay: LPOLESTR): HResult; stdcall;

    function IsValueCanonical(const propvar: REFPROPVARIANT): HResult; stdcall;

  end;
  // IPropertyDescription
  IID_IPropertyDescription = IPropertyDescription;
  {$EXTERNALSYM IID_IPropertyDescription}


  // Interface IPropertyDescription2
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyDescription2);'}
  {$EXTERNALSYM IPropertyDescription2}
  IPropertyDescription2 = interface(IPropertyDescription)
  ['{57d2eded-5062-400e-b107-5dae79fe57a6}']
    function GetImageReferenceForValue(propvar: REFPROPVARIANT;
                                       out ppszImageRes: LPOLESTR): HResult; stdcall;
    // returns a string of the form "<dll name>,-<resid>" that is suitable to be passed to PathParseIconLocation()

  end;
  // IPropertyDescription2
  IID_IPropertyDescription2 = IPropertyDescription2;
  {$EXTERNALSYM IID_IPropertyDescription2}


  // Interface IPropertyDescriptionAliasInfo
  // =======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyDescriptionAliasInfo);'}
  {$EXTERNALSYM IPropertyDescriptionAliasInfo}
  IPropertyDescriptionAliasInfo = interface(IPropertyDescription)
  ['{f67104fc-2af9-46fd-b32d-243c1404f3d1}']
    function GetSortByAlias(const riid: REFIID;
                            out ppv): HResult; stdcall;

    // Returns an IPropertyDescription interface
    function GetAdditionalSortByAliases(const riid: REFIID;
                                        out ppv): HResult; stdcall;
    // Returns an IPropertyDescriptionList interface
  end;
  // IPropertyDescriptionAliasInfo
  IID_IPropertyDescriptionAliasInfo = IPropertyDescriptionAliasInfo;
  {$EXTERNALSYM IID_IPropertyDescriptionAliasInfo}


  PPROPDESC_COLUMNINDEX_TYPE = ^PROPDESC_COLUMNINDEX_TYPE;
  PROPDESC_COLUMNINDEX_TYPE = (
    PDCIT_NONE         = 0,
    PDCIT_ONDISK       = 1,
    PDCIT_INMEMORY     = 2,
    PDCIT_ONDEMAND     = 3,
    PDCIT_ONDISKALL    = 4,
    PDCIT_ONDISKVECTOR = 5
  );
  {$EXTERNALSYM PROPDESC_COLUMNINDEX_TYPE}


  // Interface IPropertyDescriptionSearchInfo
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyDescriptionSearchInfo);'}
  {$EXTERNALSYM IPropertyDescriptionSearchInfo}
  IPropertyDescriptionSearchInfo = interface(IPropertyDescription)
  ['{078f91bd-29a2-440f-924e-46a291524520}']

    function GetSearchInfoFlags(out ppdsiFlags: PROPDESC_SEARCHINFO_FLAGS): HResult; stdcall;

    function GetColumnIndexType(out ppdciType: PROPDESC_COLUMNINDEX_TYPE): HResult; stdcall;

    function GetProjectionString(out ppszProjection: LPOLESTR): HResult; stdcall;

    function GetMaxSize(out pcbMaxSize: UINT): HResult; stdcall;

  end;
  // IPropertyDescriptionSearchInfo
  IID_IPropertyDescriptionSearchInfo = IPropertyDescriptionSearchInfo;
  {$EXTERNALSYM IID_IPropertyDescriptionSearchInfo}


  // Interface IPropertyDescriptionRelatedPropertyInfo
  // =================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyDescriptionRelatedPropertyInfo);'}
  {$EXTERNALSYM IPropertyDescriptionRelatedPropertyInfo}
  IPropertyDescriptionRelatedPropertyInfo = interface(IPropertyDescription)
  ['{507393f4-2a3d-4a60-b59e-d9c75716c2dd}']

    function GetRelatedProperty(pszRelationshipName: LPOLESTR;
                                const riid: REFIID;
                                out ppv): HResult; stdcall;
  // Returns an IPropertyDescription interface

  end;
  // IPropertyDescriptionRelatedPropertyInfo
  IID_IPropertyDescriptionRelatedPropertyInfo = IPropertyDescriptionRelatedPropertyInfo;
  {$EXTERNALSYM IID_IPropertyDescriptionRelatedPropertyInfo}


  //
  PPROPDESC_ENUMFILTER = ^PROPDESC_ENUMFILTER;
  PROPDESC_ENUMFILTER    = (
    PDEF_ALL             = 0,                   // All properties in system
    PDEF_SYSTEM          = 1,                   // Only system properties
    PDEF_NONSYSTEM       = 2,                   // Only non-system properties
    PDEF_VIEWABLE        = 3,                   // Only viewable properties
    PDEF_QUERYABLE       = 4,                   // Deprecated
    PDEF_INFULLTEXTQUERY = 5,                   // Deprecated
    PDEF_COLUMN          = 6                    // Only properties that are columns
  );
  {$EXTERNALSYM PROPDESC_ENUMFILTER}



  // Interface IPropertySystem
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertySystem);'}
  {$EXTERNALSYM IPropertySystem}
  IPropertySystem = interface(IUnknown)
  ['{ca724e8a-c3e6-442b-88a4-6fb0db8035a3}']

    function GetPropertyDescription(propkey: REFPROPERTYKEY;
                                    const riid: REFIID;
                                    out ppv): HResult; stdcall;

    // Returns an IPropertyDescription interface
    function GetPropertyDescriptionByName(pszCanonicalName: LPOLESTR;
                                          const riid: REFIID;
                                          out ppv): HResult; stdcall;

    function GetPropertyDescriptionListFromString(pszPropList: LPOLESTR;
                                                  const riid: REFIID;
                                                  out ppv): HResult; stdcall;

    // Returns an IPropertyDescriptionList interface
    function EnumeratePropertyDescriptions(filterOn: PROPDESC_ENUMFILTER;
                                           const riid: REFIID;
                                           out ppv): HResult; stdcall;

    function FormatForDisplay(key: REFPROPERTYKEY;
                              propvar: REFPROPVARIANT;
                              pdff: PROPDESC_FORMAT_FLAGS;
                              out pszText: LPOLESTR;
                              cchText: DWORD): HResult; stdcall; // 32K should be enough for anybody

    function FormatForDisplayAlloc(key: REFPROPERTYKEY;
                                   propvar: REFPROPVARIANT;
                                   pdff: PROPDESC_FORMAT_FLAGS;
                                   out ppszDisplay: LPOLESTR): HResult; stdcall;

    function RegisterPropertySchema(const pszPath: LPOLESTR): HResult; stdcall;

    function UnregisterPropertySchema(const pszPath: LPOLESTR): HResult; stdcall;

    function RefreshPropertySchema(): HResult; stdcall;

  end;
  // IPropertySystem
  IID_IPropertySystem = IPropertySystem;
  {$EXTERNALSYM IID_IPropertySystem}



  // Interface IPropertyDescriptionList
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyDescriptionList);'}
  {$EXTERNALSYM IPropertyDescriptionList}
  IPropertyDescriptionList = interface(IUnknown)
  ['{1f9fc1d0-c39b-4b26-817f-011967d3440e}']
    function GetCount(var pcElem: UINT): HResult; stdcall;

    function GetAt(iElem: UINT;
                   const riid: REFIID;
                   out ppv): HResult; stdcall;
    // Returns an IPropertyDescription interface

  end;
  // IPropertyDescriptionList
  IID_IPropertyDescriptionList = IPropertyDescriptionList;
  {$EXTERNALSYM IID_IPropertyDescriptionList}



  // Interface IPropertyStoreFactory
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyStoreFactory);'}
  {$EXTERNALSYM IPropertyStoreFactory}
  IPropertyStoreFactory = interface(IUnknown)
  ['{bc110b6d-57e8-4148-a9c6-91015ab2f3a5}']

    function GetPropertyStore(flags: GETPROPERTYSTOREFLAGS;
                              pUnkFactory: IUnknown;  // ICreateObject
                              const riid: REFIID;
                              out ppv): HResult; stdcall;

    // Returns an IPropertyStore interface
    function GetPropertyStoreForKeys(rgKeys: PROPERTYKEY;
                                     cKeys: UINT;
                                     flags: GETPROPERTYSTOREFLAGS;
                                     const riid: REFIID;
                                     out ppv): HResult; stdcall;

  end;
  // IPropertyStoreFactory
  IID_IPropertyStoreFactory = IPropertyStoreFactory;
  {$EXTERNALSYM IID_IPropertyStoreFactory}



  // Interface IDelayedPropertyStoreFactory
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDelayedPropertyStoreFactory);'}
  {$EXTERNALSYM IDelayedPropertyStoreFactory}
  IDelayedPropertyStoreFactory = interface(IPropertyStoreFactory)
  ['{40d4577f-e237-4bdb-bd69-58f089431b6a}']

    function GetDelayedPropertyStore(const flags: GETPROPERTYSTOREFLAGS;
                                     const dwStoreId: DWORD;
                                     const riid: REFIID;
                                     out ppv): HResult; stdcall;
     // Returns an IPropertyStore interface

  end;
  IID_IDelayedPropertyStoreFactory = IDelayedPropertyStoreFactory;
  {$EXTERNALSYM IID_IDelayedPropertyStoreFactory}


  // new

  PUSERIALIZEDPROPSTORAGE = ^tagSERIALIZEDPROPSTORAGE;
  {$EXTERNALSYM PUSERIALIZEDPROPSTORAGE}
  PCUSERIALIZEDPROPSTORAGE = ^tagSERIALIZEDPROPSTORAGE;
  {$EXTERNALSYM PCUSERIALIZEDPROPSTORAGE}
  PSERIALIZEDPROPSTORAGE = ^tagSERIALIZEDPROPSTORAGE;
  tagSERIALIZEDPROPSTORAGE = record
    cbNext: UINT;
    dwVersion: DWORD;
    fmtid: TGUID;
  end;
  {$EXTERNALSYM tagSERIALIZEDPROPSTORAGE}
  SERIALIZEDPROPSTORAGE = tagSERIALIZEDPROPSTORAGE;
  {$EXTERNALSYM SERIALIZEDPROPSTORAGE}



  // Interface IPersistSerializedPropStorage
  // =======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistSerializedPropStorage);'}
  {$EXTERNALSYM IPersistSerializedPropStorage}
  IPersistSerializedPropStorage = interface(IUnknown)
  ['{e318ad57-0aa0-450f-aca5-6fab7103d917}']

    function SetFlags(const flags: PERSIST_SPROPSTORE_FLAGS): HResult; stdcall;

    function SetPropertyStorage(psps: PCUSERIALIZEDPROPSTORAGE;
                                cb: DWORD): HResult; stdcall;

    function GetPropertyStorage(out ppsps: PSERIALIZEDPROPSTORAGE;
                                out pcb: DWORD): HResult; stdcall;

  end;
  IID_IPersistSerializedPropStorage = IPersistSerializedPropStorage;
  {$EXTERNALSYM IID_IPersistSerializedPropStorage}




  // Interface IPersistSerializedPropStorage2
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistSerializedPropStorage2);'}
  {$EXTERNALSYM IPersistSerializedPropStorage2}
  IPersistSerializedPropStorage2 = interface(IPersistSerializedPropStorage)
  ['{77effa68-4f98-4366-ba72-573b3d880571}']

    function GetPropertyStorageSize(out pcb: DWORD): HResult; stdcall;

    // Fails if cb is smaller than the total size of the serialized data.
    function GetPropertyStorageBuffer(out psps: PSERIALIZEDPROPSTORAGE;
                                      cb: DWORD; // The initial size, in bytes, of the buffer pointed to by psps
                                      out pcbWritten: DWORD): HResult; stdcall;

  end;
  IID_IPersistSerializedPropStorage2 = IPersistSerializedPropStorage2;
  {$EXTERNALSYM IID_IPersistSerializedPropStorage2}




  // Interface IPropertySystemChangeNotify
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertySystemChangeNotify);'}
  {$EXTERNALSYM IPropertySystemChangeNotify}
  IPropertySystemChangeNotify = interface(IUnknown)
  ['{fa955fd9-38be-4879-a6ce-824cf52d609f}']
    function SchemaRefreshed(): HResult; stdcall;

  end;
  // IPropertySystemChangeNotify
  IID_IPropertySystemChangeNotify = IPropertySystemChangeNotify;
  {$EXTERNALSYM IID_IPropertySystemChangeNotify}


  // Interface ICreateObject
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICreateObject);'}
  {$EXTERNALSYM ICreateObject}
  ICreateObject = interface(IUnknown)
  ['{75121952-e0d0-43e5-9380-1d80483acf72}']
    function CreateObject(const clsid: REFCLSID;
                          pUnkOuter: IUnknown;
                          const riid: REFIID;
                          out ppv): HResult; stdcall;

  end;
  IID_ICreateObject = ICreateObject;
  {$EXTERNALSYM IID_ICreateObject}


  // --- HELPERS ---------------------------------------------------------------

  function PSFormatForDisplay(propkey: REFPROPERTYKEY;
                              propvar: REFPROPVARIANT;
                              pdfFlags: PROPDESC_FORMAT_FLAGS;
                              out pwszText: LPOLESTR;
                              cchText: DWORD): HResult; stdcall;
  {$EXTERNALSYM PSFormatForDisplay}
  // Format a property value for display purposes

  function PSFormatForDisplayAlloc(key: REFPROPERTYKEY;
                                   propvar: REFPROPVARIANT;
                                   pdff: PROPDESC_FORMAT_FLAGS;
                                   out ppszDisplay: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSFormatForDisplayAlloc}

  function PSFormatPropertyValue(pps: IPropertyStore;
                                 ppd: IPropertyDescription;
                                 pdff: PROPDESC_FORMAT_FLAGS;
                                 out ppszDisplay: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSFormatPropertyValue}

  function PSGetImageReferenceForValue(propkey: REFPROPERTYKEY;
                                       propvar: REFPROPVARIANT;
                                       out ppszImageRes: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSGetImageReferenceForValue}
  // Retrieve the image reference associated with a property value (if specified)

  function PSStringFromPropertyKey(pkey: REFPROPERTYKEY;
                                   out psz: LPOLESTR;
                                   cch: UINT): HResult; stdcall;
  {$EXTERNALSYM PSStringFromPropertyKey}
  // Convert a PROPERTYKEY to and from a PWSTR

  function PSPropertyKeyFromString(pszString: LPOLESTR;
                                   out pkey: PROPERTYKEY): HResult; stdcall;
  {$EXTERNALSYM PSPropertyKeyFromString}

  function PSCreateMemoryPropertyStore(const riid: REFIID;
                                       out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreateMemoryPropertyStore}
  // Creates an in-memory property store
  // Returns an IPropertyStore, IPersistSerializedPropStorage, and related interfaces interface

  function PSCreateDelayedMultiplexPropertyStore(flags: GETPROPERTYSTOREFLAGS;
                                                 pdpsf: IDelayedPropertyStoreFactory;
                                                 rgStoreIds: DWORD;
                                                 cStores: DWORD;
                                                 const riid: REFIID;
                                                 out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreateDelayedMultiplexPropertyStore}
  // Create a read-only, delay-bind multiplexing property store
  // Returns an IPropertyStore interface or related interfaces.

  function PSCreateMultiplexPropertyStore(prgpunkStores: PIUnknown;
                                          cStores: DWORD;
                                          const riid: REFIID;
                                          out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreateMultiplexPropertyStore}
  // Create a read-only property store from one or more sources (which each must support either IPropertyStore or IPropertySetStorage)
  // Returns an IPropertyStore interface or related interfaces


  function PSCreatePropertyChangeArray(rgpropkey: PROPERTYKEY;
                                       rgflags: PKA_FLAGS;
                                       rgpropvar: PROPVARIANT;
                                       cChanges: UINT;
                                       const riid: REFIID;
                                       out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreatePropertyChangeArray}
  // Create a container for IPropertyChanges.
  // Returns an IPropertyChangeArray interface.


  function PSCreateSimplePropertyChange(flags: PKA_FLAGS;
                                        key: REFPROPERTYKEY;
                                        propvar: REFPROPVARIANT;
                                        const riid: REFIID;
                                        out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreateSimplePropertyChange}
  // Create a simple property change.
  // Returns an IPropertyChange interface.


  function PSGetPropertyDescription(propkey: REFPROPERTYKEY;
                                    const riid: REFIID;
                                    out ppv): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertyDescription}
  // Get a property description.
  // Returns an IPropertyDescription interface.


  function PSGetPropertyDescriptionByName(pszCanonicalName: LPOLESTR;
                                          const riid: REFIID;
                                          out ppv): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertyDescriptionByName}


  function PSLookupPropertyHandlerCLSID(pszFilePath: LPCWSTR;
                                        out pclsid: CLSID): HResult; stdcall;
  {$EXTERNALSYM PSLookupPropertyHandlerCLSID}
  // Lookup a per-machine registered file property handler



  function PSGetItemPropertyHandler(punkItem: IUnknown;
                                    fReadWrite: BOOL;
                                    const riid: REFIID;
                                    out ppv): HResult; stdcall;
  {$EXTERNALSYM PSGetItemPropertyHandler}
  // Get a property handler, on Vista or downlevel to XP.
  // punkItem is a shell item created with an SHCreateItemXXX API.
  // Returns an IPropertyStore.


  function PSGetItemPropertyHandlerWithCreateObject(punkItem: IUnknown;
                                                    fReadWrite: BOOL;
                                                    punkCreateObject: IUnknown;
                                                    const riid: REFIID;
                                                    out ppv): HResult; stdcall;
  {$EXTERNALSYM PSGetItemPropertyHandlerWithCreateObject}
  // Get a property handler, on Vista or downlevel to XP.
  // punkItem is a shell item created with an SHCreateItemXXX API .
  // punkCreateObject supports ICreateObject.
  // Returns an IPropertyStore.


  function PSGetPropertyValue(pps: IPropertyStore;
                              ppd: IPropertyDescription;
                              out ppropvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertyValue}
  // Get a property value from a store.


  function PSSetPropertyValue(pps: IPropertyStore;
                              ppd: IPropertyDescription;
                              propvar: REFPROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PSSetPropertyValue}
  // Set a property value from a store.


  // Interact with the set of property descriptions ............................

  function PSRegisterPropertySchema(pszPath: LPCWSTR): HResult; stdcall;
  {$EXTERNALSYM PSRegisterPropertySchema}
  function PSUnregisterPropertySchema(pszPath: LPCWSTR): HResult; stdcall;
  {$EXTERNALSYM PSUnregisterPropertySchema}
  function PSRefreshPropertySchema(): HResult; stdcall;
  {$EXTERNALSYM PSRefreshPropertySchema}
  // ...........................................................................



  function PSEnumeratePropertyDescriptions(filterOn: PROPDESC_ENUMFILTER;
                                           const riid: REFIID;
                                           out ppv): HResult; stdcall;
  {$EXTERNALSYM PSEnumeratePropertyDescriptions}
  // Returns either: IPropertyDescriptionList or IEnumUnknown interfaces.

  function PSGetPropertyKeyFromName(pszName: LPCWSTR;
                                    out ppropkey: PROPERTYKEY): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertyKeyFromName}
  // Convert between a PROPERTYKEY and its canonical name.

  function PSGetNameFromPropertyKey(propkey: REFPROPERTYKEY;
                                    out ppszCanonicalName: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSGetNameFromPropertyKey}

  function PSCoerceToCanonicalValue(key: REFPROPERTYKEY;
                                    out ppropvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PSCoerceToCanonicalValue}
  // Coerce and canonicalize a property value.

  function PSGetPropertyDescriptionListFromString(pszPropList: LPOLESTR;
                                                  const riid: REFIID;
                                                  out ppv): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertyDescriptionListFromString}
  // Convert a 'prop:' string into a list of property descriptions.
  // Returns an IPropertyDescriptionList interface.

  function PSCreatePropertyStoreFromPropertySetStorage(ppss: IPropertySetStorage;
                                                       grfMode: DWORD;
                                                       const riid: REFIID;
                                                       out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreatePropertyStoreFromPropertySetStorage}
  // Wrap an IPropertySetStorage interface in an IPropertyStore interface
  // Returns an IPropertyStore or related interface

  function PSCreatePropertyStoreFromObject(punk: IUnknown;
                                           grfMode: DWORD;
                                           const riid: REFIID;
                                           out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreatePropertyStoreFromObject}
  // punkSource must support IPropertyStore or IPropertySetStorage
  // On success, the returned ppv is guaranteed to support IPropertyStore.
  // If punkSource already supports IPropertyStore, no wrapper is created.

  function PSCreateAdapterFromPropertyStore(pps: IPropertyStore;
                                            const riid: REFIID;
                                            out ppv): HResult; stdcall;
  {$EXTERNALSYM PSCreateAdapterFromPropertyStore}
  // punkSource must support IPropertyStore.
  // riid may be IPropertyStore, IPropertySetStorage, IPropertyStoreCapabilities, or IObjectProvider.

  function PSGetPropertySystem(const riid: REFIID;
                               out ppv: PPointer): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertySystem}
  // Talk to the property system using an interface.
  // Returns an IPropertySystem interface.

  function PSGetPropertyFromPropertyStorage(psps: PCUSERIALIZEDPROPSTORAGE;
                                            cb: DWORD;
                                            rpkey: REFPROPERTYKEY;
                                            out ppropvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PSGetPropertyFromPropertyStorage}
  // Obtain a value from serialized property storage.

  function PSGetNamedPropertyFromPropertyStorage(psps: PCUSERIALIZEDPROPSTORAGE;
                                                 cb: DWORD;
                                                 pszName: LPOLESTR;
                                                 out ppropvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PSGetNamedPropertyFromPropertyStorage}
  // Obtain a named value from serialized property storage.




 //----- Helper functions for reading and writing values from IPropertyBag's ---

  function PSPropertyBag_ReadType(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  _var: VARIANT;
                                  out _type: VARTYPE): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadType}

  function PSPropertyBag_ReadStr(propBag: IPropertyBag;
                                 propName: LPOLESTR;
                                 out value: LPOLESTR;
                                 characterCount: Integer): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadStr}

  function PSPropertyBag_ReadStrAlloc(propBag: IPropertyBag;
                                      const propName: LPOLESTR;
                                      out value: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadStrAlloc}

  function PSPropertyBag_ReadBSTR(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  out value: BSTR): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadBSTR}

  function PSPropertyBag_WriteStr(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  value: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteStr}

  function PSPropertyBag_WriteBSTR(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   value: BSTR): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteBSTR}

  function PSPropertyBag_ReadInt(propBag: IPropertyBag;
                                 propName: LPOLESTR;
                                 out value: INT): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadInt}

  function PSPropertyBag_WriteInt(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  value: INT): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteInt}

  function PSPropertyBag_ReadSHORT(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   out value: SHORT): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadSHORT}

  function PSPropertyBag_WriteSHORT(propBag: IPropertyBag;
                                    propName: LPOLESTR;
                                    value: SHORT): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteSHORT}

  function PSPropertyBag_ReadLONG(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  value: LONG): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadLONG}

  function PSPropertyBag_WriteLONG(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   value: LONG): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteLONG}

  function PSPropertyBag_ReadDWORD(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   out value: DWORD): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadDWORD}

  function PSPropertyBag_WriteDWORD(propBag: IPropertyBag;
                                    const propName: LPOLESTR;
                                    const value: DWORD): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteDWORD}

  function PSPropertyBag_ReadBOOL(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  out value: BOOL): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadBOOL}

  function PSPropertyBag_WriteBOOL(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   value: BOOL): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteBOOL}

  function PSPropertyBag_ReadPOINTL(propBag: IPropertyBag;
                                    propName: LPOLESTR;
                                    out value: POINTL): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadPOINTL}

  function PSPropertyBag_WritePOINTL(propBag: IPropertyBag;
                                     propName: LPOLESTR;
                                     value: POINTL): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WritePOINTL}

  function PSPropertyBag_ReadPOINTS(propBag: IPropertyBag;
                                    propName: LPOLESTR;
                                    out value: POINTS): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadPOINTS}

  function PSPropertyBag_WritePOINTS(propBag: IPropertyBag;
                                     const propName: LPOLESTR;
                                     const value: POINTS): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WritePOINTS}

  function PSPropertyBag_ReadRECTL(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   out value: RECTL): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadRECTL}

  function PSPropertyBag_WriteRECTL(propBag: IPropertyBag;
                                    propName: LPOLESTR;
                                    value: RECTL): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteRECTL}

  function PSPropertyBag_ReadStream(propBag: IPropertyBag;
                                    propName: LPOLESTR;
                                    out value: IStream): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadStream}

  function PSPropertyBag_WriteStream(propBag: IPropertyBag;
                                     propName: LPOLESTR;
                                     value: IStream): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteStream}

  function PSPropertyBag_Delete(propBag: IPropertyBag;
                                propName: LPOLESTR): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_Delete}

  function PSPropertyBag_ReadULONGLONG(propBag: IPropertyBag;
                                       propName: LPOLESTR;
                                       out value: ULONGLONG): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadULONGLONG}

  function PSPropertyBag_WriteULONGLONG(propBag: IPropertyBag;
                                        propName: LPOLESTR;
                                        value: ULONGLONG): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteULONGLONG}

  function PSPropertyBag_ReadUnknown(propBag: IPropertyBag;
                                     propName: LPOLESTR;
                                     const riid: REFIID;
                                     out ppv): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadUnknown}

  function PSPropertyBag_WriteUnknown(propBag: IPropertyBag;
                                      propName: LPOLESTR;
                                      punk: IUnknown): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteUnknown}

  function PSPropertyBag_ReadGUID(propBag: IPropertyBag;
                                  propName: LPOLESTR;
                                  out value: TGUID): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadGUID}

  function PSPropertyBag_WriteGUID(propBag: IPropertyBag;
                                   propName: LPOLESTR;
                                   value: TGUID): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WriteGUID}

  function PSPropertyBag_ReadPropertyKey(propBag: IPropertyBag;
                                         propName: LPOLESTR;
                                         out value: PROPERTYKEY): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_ReadPropertyKey}

  function PSPropertyBag_WritePropertyKey(propBag: IPropertyBag;
                                          propName: LPOLESTR;
                                          value: REFPROPERTYKEY): HResult; stdcall;
  {$EXTERNALSYM PSPropertyBag_WritePropertyKey}

//[
//    uuid(2cda3294-6c4f-4020-b161-27c530c81fa6), // LIBID_PropSysObjects (not registered)
//    lcid(0x0000),
//    version(1.0)
//]

// library PropSysObjects
const
  CLSID_IInMemoryPropertyStore: TGUID = (D1:$9A02E012;
                                         D2:$6303;
                                         D3:$4E1E;
                                         D4:($B9,$A1,$63,$0F,$80,$25,$92,$C5));
  {$EXTERNALSYM CLSID_IInMemoryPropertyStore}

  CLSID_IPropertySystem       : TGUID = (D1:$B8967F85;
                                         D2:$58AE;
                                         D3:$4F46;
                                         D4:($9F,$B2,$5D,$79,$04,$79,$8F,$4B));
  {$EXTERNALSYM CLSID_IPropertySystem}


  function PropVariantToWinRTPropertyValue(propvar: REFPROPVARIANT;
                                           const riid: REFIID;
                                           out ppv): HResult; stdcall;
  {$EXTERNALSYM PropVariantToWinRTPropertyValue}
  //ppv may return NIL

  function WinRTPropertyValueToPropVariant(punkPropertyValue: IUnknown;
                                           out ppropvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM WinRTPropertyValueToPropVariant}
  // returns E_FAIL if not found

  function GetState(key: REFPROPERTYKEY;
                    out pstate: PSC_STATE): HResult; stdcall;
  {$EXTERNALSYM GetState}
  // retunrs E_FAIL if not found

  function GetValueAndState(key: REFPROPERTYKEY;
                            out ppropvar: PROPVARIANT;
                            out pstate: PSC_STATE): HResult; stdcall;
  {$EXTERNALSYM GetValueAndState}

  function SetState(key: REFPROPERTYKEY;
                    state: PSC_STATE): HResult; stdcall;
  {$EXTERNALSYM SetState}

  function SetValueAndState(key: REFPROPERTYKEY;
                            ppropvar: PROPVARIANT;
                            state: PSC_STATE): HResult; stdcall;
  {$EXTERNALSYM SetValueAndState}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

const
  PropSysLib = 'propsys.dll';

{$WARN SYMBOL_PLATFORM OFF}
  // Helpers
  function PSFormatForDisplay;                      external propSysLib name 'PSFormatForDisplay' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSFormatForDisplayAlloc;                 external propSysLib name 'PSFormatForDisplayAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSFormatPropertyValue;                   external propSysLib name 'PSFormatPropertyValue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetImageReferenceForValue;             external propSysLib name 'PSGetImageReferenceForValue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSStringFromPropertyKey;                 external propSysLib name 'PSStringFromPropertyKey' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyKeyFromString;                 external propSysLib name 'PSPropertyKeyFromString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreateMemoryPropertyStore;             external propSysLib name 'PSCreateMemoryPropertyStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreateDelayedMultiplexPropertyStore;   external propSysLib name 'PSCreateDelayedMultiplexPropertyStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreateMultiplexPropertyStore;          external propSysLib name 'PSCreateMultiplexPropertyStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreatePropertyChangeArray;             external propSysLib name 'PSCreatePropertyChangeArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreateSimplePropertyChange;            external propSysLib name 'PSGetPropertyDescription' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertyDescription;                external propSysLib name 'PSGetPropertyDescription' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertyDescriptionByName;          external propSysLib name 'PSGetPropertyDescriptionByName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSLookupPropertyHandlerCLSID;            external propSysLib name 'PSLookupPropertyHandlerCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetItemPropertyHandler;                external propSysLib name 'PSGetItemPropertyHandler' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetItemPropertyHandlerWithCreateObject; external propSysLib name 'PSGetItemPropertyHandlerWithCreateObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertyValue;                      external propSysLib name 'PSGetPropertyValue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSSetPropertyValue;                      external propSysLib name 'PSSetPropertyValue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSRegisterPropertySchema;                external propSysLib name 'PSRegisterPropertySchema' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSUnregisterPropertySchema;              external propSysLib name 'PSUnregisterPropertySchema' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSRefreshPropertySchema;                 external propSysLib name 'PSRefreshPropertySchema' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function PSEnumeratePropertyDescriptions;         external propSysLib name 'PSEnumeratePropertyDescriptions' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertyKeyFromName;                external propSysLib name 'PSGetPropertyKeyFromName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetNameFromPropertyKey;                external propSysLib name 'PSGetNameFromPropertyKey' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCoerceToCanonicalValue;                external propSysLib name 'PSCoerceToCanonicalValue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertyDescriptionListFromString;  external propSysLib name 'PSGetPropertyDescriptionListFromString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreatePropertyStoreFromPropertySetStorage; external propSysLib name 'PSCreatePropertyStoreFromPropertySetStorage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreatePropertyStoreFromObject;         external propSysLib name 'PSCreatePropertyStoreFromObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSCreateAdapterFromPropertyStore;        external propSysLib name 'PSCreateAdapterFromPropertyStore' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertySystem;                     external propSysLib name 'PSGetPropertySystem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetPropertyFromPropertyStorage;        external propSysLib name 'PSGetPropertyFromPropertyStorage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSGetNamedPropertyFromPropertyStorage;   external propSysLib name 'PSGetNamedPropertyFromPropertyStorage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function PSPropertyBag_ReadType;                  external propSysLib name 'PSPropertyBag_ReadType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadStrAlloc;              external propSysLib name 'PSPropertyBag_ReadStrAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadStr;                   external propSysLib name 'PSPropertyBag_ReadStr' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteStr;                  external propSysLib name 'PSPropertyBag_WriteStr' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadBSTR;                  external propSysLib name 'PSPropertyBag_ReadBSTR' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteBSTR;                 external propSysLib name 'PSPropertyBag_WriteBSTR' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadInt;                   external propSysLib name 'PSPropertyBag_ReadInt' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteInt;                  external propSysLib name 'PSPropertyBag_WriteInt' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadSHORT;                 external propSysLib name 'PSPropertyBag_ReadSHORT' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteSHORT;                external propSysLib name 'PSPropertyBag_WriteSHORT' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadLONG;                  external propSysLib name 'PSPropertyBag_ReadLONG' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteLONG;                 external propSysLib name 'PSPropertyBag_WriteLONG' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadDWORD;                 external propSysLib name 'PSPropertyBag_ReadDWORD' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteDWORD;                external propSysLib name 'PSPropertyBag_WriteDWORD' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadBOOL;                  external propSysLib name 'PSPropertyBag_ReadBOOL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteBOOL;                 external propSysLib name 'PSPropertyBag_WriteBOOL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadPOINTL;                external propSysLib name 'PSPropertyBag_ReadPOINTL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WritePOINTL;               external propSysLib name 'PSPropertyBag_WritePOINTL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadPOINTS;                external propSysLib name 'PSPropertyBag_ReadPOINTS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WritePOINTS;               external propSysLib name 'PSPropertyBag_WritePOINTS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadRECTL;                 external propSysLib name 'PSPropertyBag_ReadRECTL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteRECTL;                external propSysLib name 'PSPropertyBag_WriteRECTL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadStream;                external propSysLib name 'PSPropertyBag_ReadStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteStream;               external propSysLib name 'PSPropertyBag_WriteStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_Delete;                    external propSysLib name 'PSPropertyBag_Delete' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadULONGLONG;             external propSysLib name 'PSPropertyBag_ReadULONGLONG' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteULONGLONG;            external propSysLib name 'PSPropertyBag_WriteULONGLONG' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadUnknown;               external propSysLib name 'PSPropertyBag_ReadUnknown' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteUnknown;              external propSysLib name 'PSPropertyBag_WriteUnknown' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadGUID;                  external propSysLib name 'PSPropertyBag_ReadGUID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WriteGUID;                 external propSysLib name 'PSPropertyBag_WriteGUID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_ReadPropertyKey;           external propSysLib name 'PSPropertyBag_ReadPropertyKey' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PSPropertyBag_WritePropertyKey;          external propSysLib name 'PSPropertyBag_WritePropertyKey' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PropVariantToWinRTPropertyValue;         external propSysLib name 'PropVariantToWinRTPropertyValue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WinRTPropertyValueToPropVariant;         external propSysLib name 'WinRTPropertyValueToPropVariant' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function GetState;                                external propSysLib name 'GetState' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function GetValueAndState;                        external propSysLib name 'GetValueAndState' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function SetState;                                external propSysLib name 'SetState' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function SetValueAndState;                        external propSysLib name 'SetValueAndState' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.

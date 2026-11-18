// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.OCIdl.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 4.1.0
//
// Description: This header is used by Component Object Model (COM).
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows Vista or later.
//
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: OCIdl.h
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
unit WinApi.ActiveX.OCIdl;

  {$HPPEMIT '#include "OCIdl.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ServProv,
  {ActiveX}
  WinApi.ActiveX.OaIdl,
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.OleIdl,
  {System}
  System.SysUtils;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'WinApiTypes.inc'}

type
  PLCID = ^LCID;
  PHRESULT = ^HRESULT;

const

  MULTICLASSINFO_GETTYPEINFO           = $00000001;
  {$EXTERNALSYM MULTICLASSINFO_GETTYPEINFO}
  MULTICLASSINFO_GETNUMRESERVEDDISPIDS = $00000002;
  {$EXTERNALSYM MULTICLASSINFO_GETNUMRESERVEDDISPIDS}
  MULTICLASSINFO_GETIIDPRIMARY         = $00000004;
  {$EXTERNALSYM MULTICLASSINFO_GETIIDPRIMARY}
  MULTICLASSINFO_GETIIDSOURCE          = $00000008;
  {$EXTERNALSYM MULTICLASSINFO_GETIIDSOURCE}
  TIFLAGS_EXTENDDISPATCHONLY           = $00000001;
  {$EXTERNALSYM TIFLAGS_EXTENDDISPATCHONLY}


  // enum VIEWSTATUS
  VIEWSTATUS_OPAQUE              = 1;
  {$EXTERNALSYM VIEWSTATUS_OPAQUE}
  VIEWSTATUS_SOLIDBKGND          = 2;
  {$EXTERNALSYM VIEWSTATUS_SOLIDBKGND}
  VIEWSTATUS_DVASPECTOPAQUE      = 4;
  {$EXTERNALSYM VIEWSTATUS_DVASPECTOPAQUE}
  VIEWSTATUS_DVASPECTTRANSPARENT = 8;
  {$EXTERNALSYM VIEWSTATUS_DVASPECTTRANSPARENT}
  VIEWSTATUS_SURFACE             = 16;
  {$EXTERNALSYM VIEWSTATUS_SURFACE}
  VIEWSTATUS_3DSURFACE           = 32;
  {$EXTERNALSYM VIEWSTATUS_3DSURFACE}


  // enum DVASPECT2
  DVASPECT_OPAQUE      = 16;
  {$EXTERNALSYM DVASPECT_OPAQUE}
  DVASPECT_TRANSPARENT = 32;
  {$EXTERNALSYM DVASPECT_TRANSPARENT}

type
  PUASFLAGS = ^UASFLAGS;
  tagUASFLAGS = DWord;
  {$EXTERNALSYM tagUASFLAGS}
  UASFLAGS = tagUASFLAGS;
  TUasflags = tagUASFLAGS;
const
  UAS_NORMAL         = UASFLAGS($00);
  {$EXTERNALSYM UAS_NORMAL}
  UAS_BLOCKED        = UASFLAGS($01);
  {$EXTERNALSYM UAS_BLOCKED}
  UAS_NOPARENTENABLE = UASFLAGS($02);
  {$EXTERNALSYM UAS_NOPARENTENABLE}
  UAS_MASK           = UASFLAGS($03);
  {$EXTERNALSYM UAS_MASK}

type
  POLEDCFLAGS = ^OLEDCFLAGS;
  tagOLEDCFLAGS = DWord;
  {$EXTERNALSYM tagOLEDCFLAGS}
  OLEDCFLAGS = tagOLEDCFLAGS;
const
  {$EXTERNALSYM OLEDC_NODRAW}
  OLEDC_NODRAW     = OLEDCFLAGS($01);
  {$EXTERNALSYM OLEDC_PAINTBKGND}
  OLEDC_PAINTBKGND = OLEDCFLAGS($02);
  {$EXTERNALSYM OLEDC_OFFSCREEN}
  OLEDC_OFFSCREEN  = OLEDCFLAGS($04);

type
  PQACONTAINERFLAGS = ^QACONTAINERFLAGS;
  tagQACONTAINERFLAGS = DWord;
  {$EXTERNALSYM tagQACONTAINERFLAGS}
  QACONTAINERFLAGS = tagQACONTAINERFLAGS;
  {$EXTERNALSYM QACONTAINERFLAGS}
const
  QACONTAINER_SHOWHATCHING      = QACONTAINERFLAGS($0001);
  QACONTAINER_SHOWGRABHANDLES   = QACONTAINERFLAGS($0002);
  QACONTAINER_USERMODE          = QACONTAINERFLAGS($0004);
  QACONTAINER_DISPLAYASDEFAULT  = QACONTAINERFLAGS($0008);
  QACONTAINER_UIDEAD            = QACONTAINERFLAGS($0010);
  QACONTAINER_AUTOCLIP          = QACONTAINERFLAGS($0020);
  QACONTAINER_MESSAGEREFLECT    = QACONTAINERFLAGS($0040);
  QACONTAINER_SUPPORTSMNEMONICS = QACONTAINERFLAGS($0080);


type

  // Forward Interface Declarations

  // From Urlmon.h !!!
  IBindHost = interface;

  PENUMCONNECTIONS = ^IEnumConnections;
  LPENUMCONNECTIONS = ^IEnumConnections;
  {$EXTERNALSYM LPENUMCONNECTIONS}
  IEnumConnections = interface;

  LPCONNECTIONPOINT = ^IConnectionPoint;
  {$EXTERNALSYM LPCONNECTIONPOINT}
  PIConnectionPoint = ^IConnectionPoint;
  IConnectionPoint = interface;

  LPENUMCONNECTIONPOINTS = ^IEnumConnectionPoints;
  {$EXTERNALSYM LPENUMCONNECTIONPOINTS}
  PENUMCONNECTIONPOINTS = ^IEnumConnectionPoints;
  IEnumConnectionPoints = interface;

  LPIPropertyBag2 = ^IPropertyBag2;
  {$EXTERNALSYM LPIPropertyBag2}
  PIPropertyBag2 = ^IPropertyBag2;
  IPropertyBag2 = interface;

  LPPROVIDEMULTIPLECLASSINFO = ^IProvideMultipleClassInfo;
  {$EXTERNALSYM LPPROVIDEMULTIPLECLASSINFO}
  PIProvideClassInfo = ^IProvideClassInfo;
  IProvideClassInfo = interface;

  LPIProvideClassInfo2 = ^IProvideClassInfo2;
  {$EXTERNALSYM LPIProvideClassInfo2}
  PIProvideClassInfo2 = ^IProvideClassInfo2;
  IProvideClassInfo2 = interface;

  PIOleControlSite = ^IOleControlSite;
  IOleControlSite = interface;

  PIPropertyPageSite = ^IPropertyPageSite;
  IPropertyPageSite = interface;

  PIPropertyNotifySink = ^IPropertyNotifySink;
  IPropertyNotifySink = interface;

  LPPERSISTMEMORY = ^IPersistMemory;
  {$EXTERNALSYM LPPERSISTMEMORY}
  PIPersistMemory = ^IPersistMemory;
  IPersistMemory = interface;

  PIFont = ^IFont;
  IFont = interface;

  LPPICTURE2 = ^IPicture2;
  {$EXTERNALSYM LPPICTURE2}

  PIAdviseSinkEx = ^IAdviseSinkEx;
  IAdviseSinkEx = interface;

  PIOleUndoManager = ^IOleUndoManager;
  IOleUndoManager = interface;


  //* State values for the DISPID_READYSTATE property *//
  PREADYSTATE = ^READYSTATE;
  tagREADYSTATE              = (
    READYSTATE_UNINITIALIZED = 0,      // Never used except as default init state
    READYSTATE_LOADING       = 1,      // Control is loading its properties
    READYSTATE_LOADED        = 2,      // Has been init'ed via IPersist*::Load
    READYSTATE_INTERACTIVE   = 3,      // Interactive but not all data available
    READYSTATE_COMPLETE      = 4       // Control has all its data
  );
  {$EXTERNALSYM tagREADYSTATE}
  READYSTATE = tagREADYSTATE;
  {$EXTERNALSYM READYSTATE}

  LPCONNECTDATA = ^CONNECTDATA;
  PCONNECTDATA = ^CONNECTDATA;
  tagCONNECTDATA = record
    pUnk: IUnknown;
    dwCookie: DWORD;
  end;
  {$EXTERNALSYM tagCONNECTDATA}
  CONNECTDATA = tagCONNECTDATA;
  {$EXTERNALSYM CONNECTDATA}

  LPLICINFO = ^LICINFO;
  PLICINFO = ^LICINFO;
  tagLICINFO = record
    cbLicInfo: LONG;
    fRuntimeKeyAvail: BOOL;
    fLicVerified: BOOL;
  end;
  {$EXTERNALSYM tagLICINFO}
  LICINFO = tagLICINFO;
  {$EXTERNALSYM LICINFO}

  PGUIDKIND = ^GUIDKIND;
  tagGUIDKIND                        = (
    GUIDKIND_DEFAULT_SOURCE_DISP_IID = 1
  );
  {$EXTERNALSYM tagGUIDKIND}
  GUIDKIND = tagGUIDKIND;
  {$EXTERNALSYM GUIDKIND}

  LPOLECONTROL = ^CONTROLINFO;
  PCONTROLINFO = ^CONTROLINFO;
  tagCONTROLINFO = record
    cb: ULONG;
    hAccel: HACCEL;
    cAccel: USHORT;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM tagCONTROLINFO}
  CONTROLINFO = tagCONTROLINFO;
  {$EXTERNALSYM CONTROLINFO}

  PCTRLINFO = ^CTRLINFO;
  tagCTRLINFO            = (
    CTRLINFO_EATS_RETURN = 1,
    CTRLINFO_EATS_ESCAPE = 2
  );
  {$EXTERNALSYM tagCTRLINFO}
  CTRLINFO = tagCTRLINFO;
  {$EXTERNALSYM CTRLINFO}

  PPOINTF = ^POINTF;
  tagPOINTF = record
    x: FLOAT;
    y: FLOAT;
  end;
  {$EXTERNALSYM tagPOINTF}
  POINTF = tagPOINTF;
  {$EXTERNALSYM POINTF}

  PXFORMCOORDS = ^XFORMCOORDS;
  tagXFORMCOORDS                    = (
    XFORMCOORDS_POSITION            = $1,
    XFORMCOORDS_SIZE                = $2,
    XFORMCOORDS_HIMETRICTOCONTAINER = $4,
    XFORMCOORDS_CONTAINERTOHIMETRIC = $8,
    XFORMCOORDS_EVENTCOMPAT         = $10
  );
  {$EXTERNALSYM tagXFORMCOORDS}
  XFORMCOORDS = tagXFORMCOORDS;
  {$EXTERNALSYM XFORMCOORDS}

  PPROPPAGEINFO = ^PROPPAGEINFO;
  tagPROPPAGEINFO = record
    cb: ULONG;
    pszTitle: LPOLESTR;
    size: SIZE;
    pszDocString: LPOLESTR;
    pszHelpFile: LPOLESTR;
    dwHelpContext: DWORD;
  end;
  {$EXTERNALSYM tagPROPPAGEINFO}
  PROPPAGEINFO = tagPROPPAGEINFO;
  {$EXTERNALSYM PROPPAGEINFO}

  PPROPPAGESTATUS = ^PROPPAGESTATUS;
  tagPROPPAGESTATUS         = (
    PROPPAGESTATUS_DIRTY    = $01,
    PROPPAGESTATUS_VALIDATE = $02,
    PROPPAGESTATUS_CLEAN    = $04
  );
  {$EXTERNALSYM tagPROPPAGESTATUS}
  PROPPAGESTATUS = tagPROPPAGESTATUS;
  {$EXTERNALSYM PROPPAGESTATUS}

  LPCAUUID = ^tagCAUUID;
  PCAUUID = ^CAUUID;
  tagCAUUID = record
    cElems: ULONG;
    pElems: PGUID;
  end;
  {$EXTERNALSYM tagCAUUID}
  CAUUID = tagCAUUID;
  {$EXTERNALSYM CAUUID}

  PTextmetricole = ^TEXTMETRICOLE;
  TEXTMETRICOLE = TEXTMETRICW;
  {$EXTERNALSYM TEXTMETRICOLE}

  PPICTUREATTRIBUTES = ^PICTUREATTRIBUTES;
  tagPictureAttributes  = (
    PICTURE_SCALABLE    = $1,
    PICTURE_TRANSPARENT = $2
  );
  {$EXTERNALSYM tagPictureAttributes}
  PICTUREATTRIBUTES = tagPictureAttributes;
  {$EXTERNALSYM PICTUREATTRIBUTES}


  POleHandle = ^OLE_HANDLE;
  OLE_HANDLE = UINT;
  {$EXTERNALSYM OLE_HANDLE}

  POleXposHimetric = ^OLE_XPOS_HIMETRIC;
  OLE_XPOS_HIMETRIC = LONG;
  {$EXTERNALSYM OLE_XPOS_HIMETRIC}

  POleYposHimetric = ^OLE_YPOS_HIMETRIC;
  OLE_YPOS_HIMETRIC = LONG;
  {$EXTERNALSYM OLE_YPOS_HIMETRIC}

  POleXsizeHimetric = ^OLE_XSIZE_HIMETRIC;
  OLE_XSIZE_HIMETRIC = LONG;
  {$EXTERNALSYM OLE_XSIZE_HIMETRIC}

  POleYsizeHimetric = ^OLE_YSIZE_HIMETRIC;
  OLE_YSIZE_HIMETRIC = LONG;
  {$EXTERNALSYM OLE_YSIZE_HIMETRIC}

  PHhandle = ^HHANDLE;
  HHANDLE = UINT_PTR;
  {$EXTERNALSYM HHANDLE}

  tagVIEWSTATUS = DWORD;
  VIEWSTATUS = tagVIEWSTATUS;
  {$EXTERNALSYM VIEWSTATUS}
  // see above (const) for its values

  PHITRESULT = ^HITRESULT;
  tagHITRESULT            = (
    HITRESULT_OUTSIDE     = 0,
    HITRESULT_TRANSPARENT = 1,
    HITRESULT_CLOSE       = 2,
    HITRESULT_HIT         = 3
  );
  {$EXTERNALSYM tagHITRESULT}
  HITRESULT = tagHITRESULT;

  tagDVASPECT2 = DWORD;
  DVASPECT2 = tagDVASPECT2;
  {$EXTERNALSYM DVASPECT2}
  // see above (const) for its values

  PDVEXTENTINFO = ^DVEXTENTINFO;
  tagExtentInfo = record
    cb: ULONG;
    dwExtentMode: DWORD;
    sizelProposed: SIZEL;
  end;
  {$EXTERNALSYM tagExtentInfo}
  DVEXTENTINFO = tagExtentInfo;
  {$EXTERNALSYM DVEXTENTINFO}

  PDVEXTENTMODE = ^DVEXTENTMODE;
  tagExtentMode = (
    DVEXTENT_CONTENT,
    DVEXTENT_INTEGRAL);
  {$EXTERNALSYM tagExtentMode}
  DVEXTENTMODE = tagExtentMode;
  {$EXTERNALSYM DVEXTENTMODE}

  PDVASPECTINFOFLAG = ^DVASPECTINFOFLAG;
  tagAspectInfoFlag              = (
    DVASPECTINFOFLAG_CANOPTIMIZE = 1);
  {$EXTERNALSYM tagAspectInfoFlag}
  DVASPECTINFOFLAG = tagAspectInfoFlag;
  {$EXTERNALSYM DVASPECTINFOFLAG}

  PDVASPECTINFO = ^DVASPECTINFO;
  tagAspectInfo = record
    cb: ULONG;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM tagAspectInfo}
  DVASPECTINFO = tagAspectInfo;
  {$EXTERNALSYM DVASPECTINFO}

  PPOINTERINACTIVE = ^POINTERINACTIVE;
  tagPOINTERINACTIVE                  = (
    POINTERINACTIVE_ACTIVATEONENTRY   = 1,
    POINTERINACTIVE_DEACTIVATEONLEAVE = 2,
    POINTERINACTIVE_ACTIVATEONDRAG    = 4
  );
  {$EXTERNALSYM tagPOINTERINACTIVE}
  POINTERINACTIVE = tagPOINTERINACTIVE;
  {$EXTERNALSYM POINTERINACTIVE}


  POleStringArrayElement = ^LPOLESTR;
  PCALPOLESTR = ^CALPOLESTR;
  tagCALPOLESTR = record
    cElems: ULONG;
    pElems: POleStringArrayElement; // array of LPOLESTR
  end;
  {$EXTERNALSYM tagCALPOLESTR}
  CALPOLESTR = tagCALPOLESTR;
  {$EXTERNALSYM CALPOLESTR}

  LPCADWORD = ^tagCADWORD;
  PCadword = ^tagCADWORD;
  tagCADWORD = record
    cElems: ULONG;
    pElems: PDWORD;
  end;
  {$EXTERNALSYM tagCADWORD}
  CADWORD = tagCADWORD;
  {$EXTERNALSYM CADWORD}

  PPROPBAG2_TYPE = ^PROPBAG2_TYPE;
  _tagPROPBAG2_TYPE         = (
    PROPBAG2_TYPE_UNDEFINED = 0,
    PROPBAG2_TYPE_DATA      = 1,
    PROPBAG2_TYPE_URL       = 2,
    PROPBAG2_TYPE_OBJECT    = 3,
    PROPBAG2_TYPE_STREAM    = 4,
    PROPBAG2_TYPE_STORAGE   = 5,
    PROPBAG2_TYPE_MONIKER   = 6
  );
  {$EXTERNALSYM _tagPROPBAG2_TYPE}
  PROPBAG2_TYPE = _tagPROPBAG2_TYPE;
  {$EXTERNALSYM PROPBAG2_TYPE}

  PPROPBAG2 = ^PROPBAG2;
  _tagPROPBAG2 = record
    dwType: DWORD;
    vt: VARTYPE;
    cfType: CLIPFORMAT;
    dwHint: DWORD;
    pstrName: LPOLESTR;
    clsid: TGuid;
  end;
  {$EXTERNALSYM _tagPROPBAG2}
  PROPBAG2 = _tagPROPBAG2;
  {$EXTERNALSYM PROPBAG2}

  POLE_COLOR = ^OLE_COLOR;
  OLE_COLOR = DWORD;
  {$EXTERNALSYM OLE_COLOR}

  PQACONTAINER = ^QACONTAINER;
  tagQACONTAINER = record
    cbSize: ULONG;
    pClientSite: IOleClientSite;
    pAdviseSink: IAdviseSinkEx;
    pPropertyNotifySink: IPropertyNotifySink;
    pUnkEventSink: IUnknown;
    dwAmbientFlags: DWORD;
    colorFore: OLE_COLOR;
    colorBack: OLE_COLOR;
    pFont: IFont;
    pUndoMgr: IOleUndoManager;
    dwAppearance: DWORD;
    lcid: LONG;
    hpal: HPALETTE;
    pBindHost: IBindHost;
    pOleControlSite: IOleControlSite;
    pServiceProvider: IServiceProvider;
  end;
  {$EXTERNALSYM tagQACONTAINER}
  QACONTAINER = tagQACONTAINER;
  {$EXTERNALSYM QACONTAINER}

  PQACONTROL = ^QACONTROL;
  tagQACONTROL = record
    cbSize: ULONG;
    dwMiscStatus: DWORD;
    dwViewStatus: DWORD;
    dwEventCookie: DWORD;
    dwPropNotifyCookie: DWORD;
    dwPointerActivationPolicy: DWORD;
  end;
  {$EXTERNALSYM tagQACONTROL}
  QACONTROL = tagQACONTROL;
  {$EXTERNALSYM QACONTROL}



  //
  // Interface declarations
  //

  // From urlmon.h (keep as dummy)
  // Interface IBindHost
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBindHost);'}
  {$EXTERNALSYM IBindHost}
  IBindHost = interface(IUnknown)
  ['{fc4801a1-2ba9-11cf-a229-00aa003d7352}']

  end;
  IID_IBindHost = IBindHost;
  {$EXTERNALSYM IID_IBindHost}


  // Interface IEnumConnections
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumConnections);'}
  {$EXTERNALSYM IEnumConnections}
  IEnumConnections = interface(IUnknown)
  ['{B196B287-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Next(cConnections: ULONG; rgcd: PCONNECTDATA; pcFetched: PULONG): HRESULT; stdcall;

    function Skip(cConnections: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppEnum: IEnumConnections): HRESULT; stdcall;
  end;
  IID_IEnumConnections = IEnumConnections;
  {$EXTERNALSYM IID_IEnumConnections}



  // Interface IConnectionPoint
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IConnectionPoint);'}
  {$EXTERNALSYM IConnectionPoint}
  IConnectionPoint = interface(IUnknown)
  ['{B196B286-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetConnectionInterface(pIID: PGUID): HRESULT; stdcall;

    function GetConnectionPointContainer(out ppCPC: IConnectionPointContainer): HRESULT; stdcall;

    function Advise(pUnkSink: IUnknown; pdwCookie: PDWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function EnumConnections(out ppEnum: IEnumConnections): HRESULT; stdcall;
  end;
  IID_IConnectionPoint = IConnectionPoint;
  {$EXTERNALSYM IID_IConnectionPoint}


  // Interface IEnumConnectionPoints
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumConnectionPoints);'}
  {$EXTERNALSYM IEnumConnectionPoints}
  IEnumConnectionPoints = interface(IUnknown)
  ['{B196B285-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Next(cConnections: ULONG; out ppCP: IConnectionPoint; pcFetched: PULONG): HRESULT; stdcall;

    function Skip(cConnections: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppEnum: IEnumConnectionPoints): HRESULT; stdcall;
  end;
  IID_IEnumConnectionPoints = IEnumConnectionPoints;
  {$EXTERNALSYM IID_IEnumConnectionPoints}


  // Interface IConnectionPointContainer
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IConnectionPointContainer);'}
  {$EXTERNALSYM IConnectionPointContainer}
  IConnectionPointContainer = interface(IUnknown)
  ['{B196B284-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function EnumConnectionPoints(out ppEnum: IEnumConnectionPoints): HRESULT; stdcall;

    function FindConnectionPoint(const riid: TGUID; out ppCP: IConnectionPoint): HRESULT; stdcall;
  end;
  IID_IConnectionPointContainer = IConnectionPointContainer;
  {$EXTERNALSYM IID_IConnectionPointContainer}


  // Interface IClassFactory2
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IClassFactory2);'}
  {$EXTERNALSYM IClassFactory2}
  IClassFactory2 = interface(IClassFactory)
    ['{B196B28F-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetLicInfo(pLicInfo: PLICINFO): HRESULT; stdcall;

    function RequestLicKey(dwReserved: DWORD; out pBstrKey: BSTR): HRESULT; stdcall;

    function CreateInstanceLic(pUnkOuter: IUnknown; pUnkReserved: IUnknown; const riid: TGUID; bstrKey: BSTR; out ppvObj: Pointer): HRESULT; stdcall;
  end;
  IID_IClassFactory2 = IClassFactory2;
  {$EXTERNALSYM IID_IClassFactory2}


  // Interface IProvideClassInfo
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IProvideClassInfo);'}
  {$EXTERNALSYM IProvideClassInfo}
  IProvideClassInfo = interface(IUnknown)
  ['{B196B283-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetClassInfo(out ppTI: ITypeInfo): HRESULT; stdcall;
  end;
  IID_IProvideClassInfo = IProvideClassInfo;
  {$EXTERNALSYM IID_IProvideClassInfo}


  // Interface IProvideClassInfo2
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IProvideClassInfo2);'}
  {$EXTERNALSYM IProvideClassInfo2}
  IProvideClassInfo2 = interface(IProvideClassInfo)
  ['{A6BC3AC0-DBAA-11CE-9DE3-00AA004BB851}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetGUID(dwGuidKind: DWORD; pGUID: PGUID): HRESULT; stdcall;
  end;
  IID_IProvideClassInfo2 = IProvideClassInfo2;
  {$EXTERNALSYM IID_IProvideClassInfo2}


  // Interface IProvideMultipleClassInfo
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IProvideMultipleClassInfo);'}
  {$EXTERNALSYM IProvideMultipleClassInfo}
  IProvideMultipleClassInfo = interface(IProvideClassInfo2)
  ['{A7ABA9C1-8983-11cf-8F20-00805F2CD064}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetMultiTypeInfoCount(pcti: PULONG): HRESULT; stdcall;

    function GetInfoOfIndex(iti: ULONG; dwFlags: DWORD; out pptiCoClass: ITypeInfo; pdwTIFlags: PDWORD; pcdispidReserved: PULONG; piidPrimary: PGUID; piidSource: PGUID): HRESULT; stdcall;
  end;
  IID_IProvideMultipleClassInfo = IProvideMultipleClassInfo;
  {$EXTERNALSYM IID_IProvideMultipleClassInfo}


  // Interface IOleControl
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleControl);'}
  {$EXTERNALSYM IOleControl}
  IOleControl = interface(IUnknown)
  ['{B196B288-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetControlInfo(pCI: PCONTROLINFO): HRESULT; stdcall;

    function OnMnemonic(pMsg: PMSG): HRESULT; stdcall;

    function OnAmbientPropertyChange(dispID: DISPID): HRESULT; stdcall;

    function FreezeEvents(bFreeze: BOOL): HRESULT; stdcall;
  end;
  IID_IOleControl = IOleControl;
  {$EXTERNALSYM IID_IOleControl}


  // Interface IOleControlSite
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleControlSite);'}
  {$EXTERNALSYM IOleControlSite}
  IOleControlSite = interface(IUnknown)
    ['{B196B289-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function OnControlInfoChanged(): HRESULT; stdcall;

    function LockInPlaceActive(fLock: BOOL): HRESULT; stdcall;

    function GetExtendedControl(out ppDisp: IDispatch): HRESULT; stdcall;

    function TransformCoords(pPtlHimetric: PPOINTL; pPtfContainer: PPOINTF; dwFlags: DWORD): HRESULT; stdcall;

    function TranslateAccelerator(pMsg: PMSG; grfModifiers: DWORD): HRESULT; stdcall;

    function OnFocus(fGotFocus: BOOL): HRESULT; stdcall;

    function ShowPropertyFrame(): HRESULT; stdcall;
  end;
  IID_IOleControlSite = IOleControlSite;
  {$EXTERNALSYM IID_IOleControlSite}


  // Interface IPropertyPage
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyPage);'}
  {$EXTERNALSYM IPropertyPage}
  IPropertyPage = interface(IUnknown)
  ['{B196B28D-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function SetPageSite(pPageSite: IPropertyPageSite): HRESULT; stdcall;

    function Activate(hWndParent: HWND; pRect: PRect; bModal: BOOL): HRESULT; stdcall;

    function Deactivate(): HRESULT; stdcall;

    function GetPageInfo(pPageInfo: PPROPPAGEINFO): HRESULT; stdcall;

    function SetObjects(cObjects: ULONG; out ppUnk: IUnknown): HRESULT; stdcall;

    function Show(nCmdShow: UINT): HRESULT; stdcall;

    function Move(pRect: PRect): HRESULT; stdcall;

    function IsPageDirty(): HRESULT; stdcall;

    function Apply(): HRESULT; stdcall;

    function Help(pszHelpDir: PWideChar): HRESULT; stdcall;

    function TranslateAccelerator(pMsg: PMSG): HRESULT; stdcall;
  end;
  IID_IPropertyPage = IPropertyPage;
  {$EXTERNALSYM IID_IPropertyPage}


  // Interface IPropertyPage2
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyPage2);'}
  {$EXTERNALSYM IPropertyPage2}
  IPropertyPage2 = interface(IPropertyPage)
  ['{01E44665-24AC-101B-84ED-08002B2EC713}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function EditProperty(dispID: DISPID): HRESULT; stdcall;
  end;
  IID_IPropertyPage2 = IPropertyPage2;
  {$EXTERNALSYM IID_IPropertyPage2}


  // Interface IPropertyPageSite
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyPageSite);'}
  {$EXTERNALSYM IPropertyPageSite}
  IPropertyPageSite = interface(IUnknown)
  ['{B196B28C-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function OnStatusChange(dwFlags: DWORD): HRESULT; stdcall;

    function GetLocaleID(pLocaleID: PLCID): HRESULT; stdcall;

    function GetPageContainer(out ppUnk: IUnknown): HRESULT; stdcall;

    function TranslateAccelerator(pMsg: PMSG): HRESULT; stdcall;
  end;
  IID_IPropertyPageSite = IPropertyPageSite;
  {$EXTERNALSYM IID_IPropertyPageSite}


  // Interface IPropertyNotifySink
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyNotifySink);'}
  {$EXTERNALSYM IPropertyNotifySink}
  IPropertyNotifySink = interface(IUnknown)
  ['{9BFBBC02-EFF1-101A-84ED-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function OnChanged(dispID: DISPID): HRESULT; stdcall;

    function OnRequestEdit(dispID: DISPID): HRESULT; stdcall;
  end;
  IID_IPropertyNotifySink = IPropertyNotifySink;
  {$EXTERNALSYM IID_IPropertyNotifySink}


  // Interface ISpecifyPropertyPages
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpecifyPropertyPages);'}
  {$EXTERNALSYM ISpecifyPropertyPages}
  ISpecifyPropertyPages = interface(IUnknown)
  ['{B196B28B-BAB4-101A-B69C-00AA00341D07}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetPages(pPages: PCAUUID): HRESULT; stdcall;
  end;
  IID_ISpecifyPropertyPages = ISpecifyPropertyPages;
  {$EXTERNALSYM IID_ISpecifyPropertyPages}


  // Interface IPersistMemory
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistMemory);'}
  {$EXTERNALSYM IPersistMemory}
  IPersistMemory = interface(IPersist)
  ['{BD1AE5E0-A6AE-11CE-BD37-504200C10000}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function IsDirty(): HRESULT; stdcall;

    function Load(pMem: Pointer; cbSize: ULONG): HRESULT; stdcall;

    function Save(pMem: Pointer; fClearDirty: BOOL; cbSize: ULONG): HRESULT; stdcall;

    function GetSizeMax(pCbSize: PULONG): HRESULT; stdcall;

    function InitNew(): HRESULT; stdcall;
  end;
  IID_IPersistMemory = IPersistMemory;
  {$EXTERNALSYM IID_IPersistMemory}


  // Interface IPersistStreamInit
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistStreamInit);'}
  {$EXTERNALSYM IPersistStreamInit}
  IPersistStreamInit = interface(IPersist)
  ['{7FD52380-4E07-101B-AE2D-08002B2EC713}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function IsDirty(): HRESULT; stdcall;

    function Load(pStm: IStream): HRESULT; stdcall;

    function Save(pStm: IStream; fClearDirty: BOOL): HRESULT; stdcall;

    function GetSizeMax(pCbSize: PULARGE_INTEGER): HRESULT; stdcall;

    function InitNew(): HRESULT; stdcall;
  end;
  IID_IPersistStreamInit = IPersistStreamInit;
  {$EXTERNALSYM IID_IPersistStreamInit}



  // Interface IPersistPropertyBag
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistPropertyBag);'}
  {$EXTERNALSYM IPersistPropertyBag}
  IPersistPropertyBag = interface(IPersist)
  ['{37D84F60-42CB-11CE-8135-00AA004BB851}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function InitNew(): HRESULT; stdcall;

    function Load(pPropBag: IPropertyBag; pErrorLog: IErrorLog): HRESULT; stdcall;

    function Save(pPropBag: IPropertyBag; fClearDirty: BOOL; fSaveAllProperties: BOOL): HRESULT; stdcall;
  end;
  IID_IPersistPropertyBag = IPersistPropertyBag;
  {$EXTERNALSYM IID_IPersistPropertyBag}


  // Interface ISimpleFrameSite
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISimpleFrameSite);'}
  {$EXTERNALSYM ISimpleFrameSite}
  ISimpleFrameSite = interface(IUnknown)
  ['{742B0E01-14E6-101B-914E-00AA00300CAB}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function PreMessageFilter(hWnd: HWND; msg: UINT; wp: WPARAM; lp: LPARAM; plResult: PLRESULT; pdwCookie: PDWORD): HRESULT; stdcall;

    function PostMessageFilter(hWnd: HWND; msg: UINT; wp: WPARAM; lp: LPARAM; plResult: PLRESULT; dwCookie: DWORD): HRESULT; stdcall;
  end;
  IID_ISimpleFrameSite = ISimpleFrameSite;
  {$EXTERNALSYM IID_ISimpleFrameSite}


  // Interface IFont
  // ===============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFont);'}
  {$EXTERNALSYM IFont}
  IFont = interface(IUnknown)
  ['{BEF6E002-A874-101A-8BBA-00AA00300CAB}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function get_Name(out pName: BSTR): HRESULT; stdcall;

    function put_Name(name: BSTR): HRESULT; stdcall;

    function get_Size(out pSize: CY): HRESULT; stdcall;

    function put_Size(size: CY): HRESULT; stdcall;

    function get_Bold(out pBold: BOOL): HRESULT; stdcall;

    function put_Bold(bold: BOOL): HRESULT; stdcall;

    function get_Italic(out pItalic: BOOL): HRESULT; stdcall;

    function put_Italic(italic: BOOL): HRESULT; stdcall;

    function get_Underline(out pUnderline: BOOL): HRESULT; stdcall;

    function put_Underline(underline: BOOL): HRESULT; stdcall;

    function get_Strikethrough(out pStrikethrough: BOOL): HRESULT; stdcall;

    function put_Strikethrough(strikethrough: BOOL): HRESULT; stdcall;

    function get_Weight(out pWeight: SHORT): HRESULT; stdcall;

    function put_Weight(weight: SHORT): HRESULT; stdcall;

    function get_Charset(out pCharset: SHORT): HRESULT; stdcall;

    function put_Charset(charset: SHORT): HRESULT; stdcall;

    function get_hFont(out phFont: HFONT): HRESULT; stdcall;

    function Clone(out ppFont: IFont): HRESULT; stdcall;

    function IsEqual(pFontOther: IFont): HRESULT; stdcall;

    function SetRatio(cyLogical: LONG; cyHimetric: LONG): HRESULT; stdcall;

    function QueryTextMetrics(pTM: PTEXTMETRICOLE): HRESULT; stdcall;

    function AddRefHfont(hFont: HFONT): HRESULT; stdcall;

    function ReleaseHfont(hFont: HFONT): HRESULT; stdcall;

    function SetHdc(hDC: HDC): HRESULT; stdcall;
  end;
  IID_IFont = IFont;
  {$EXTERNALSYM IID_IFont}


  // Interface IPicture
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPicture);'}
  {$EXTERNALSYM IPicture}
  IPicture = interface(IUnknown)
  ['{7BF80980-BF32-101A-8BBB-00AA00300CAB}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function get_Handle(out pHandle: OLE_HANDLE): HRESULT; stdcall;

    function get_hPal(out phPal: OLE_HANDLE): HRESULT; stdcall;

    function get_Type(out pType: SHORT): HRESULT; stdcall;

    function get_Width(out pWidth: OLE_XSIZE_HIMETRIC): HRESULT; stdcall;

    function get_Height(out pHeight: OLE_YSIZE_HIMETRIC): HRESULT; stdcall;

    function Render(hDC: HDC; x: LONG; y: LONG; cx: LONG; cy: LONG; xSrc: OLE_XPOS_HIMETRIC; ySrc: OLE_YPOS_HIMETRIC; cxSrc: OLE_XSIZE_HIMETRIC; cySrc: OLE_YSIZE_HIMETRIC; pRcWBounds: PRect): HRESULT; stdcall;

    function set_hPal(hPal: OLE_HANDLE): HRESULT; stdcall;

    function get_CurDC(out phDC: HDC): HRESULT; stdcall;

    function SelectPicture(hDCIn: HDC; out phDCOut: HDC; out phBmpOut: OLE_HANDLE): HRESULT; stdcall;

    function get_KeepOriginalFormat(out pKeep: BOOL): HRESULT; stdcall;

    function put_KeepOriginalFormat(keep: BOOL): HRESULT; stdcall;

    function PictureChanged(): HRESULT; stdcall;

    function SaveAsFile(pStream: IStream; fSaveMemCopy: BOOL; pCbSize: PLongint): HRESULT; stdcall;

    function get_Attributes(pDwAttr: PDWORD): HRESULT; stdcall;
  end;
  IID_IPicture = IPicture;
  {$EXTERNALSYM IID_IPicture}



  // Interface IPicture2
  // ===================
  // IPicture2 defines a platform aware IDL representation of IPicture2
  // implemented by the standard StdPicture object.
  // It uses HHANDLE  defined as UINT_PTR and relies on HDC being already
  // defined as UINT_PTR to represent the platform specific HANDLEs.
  // This will result in signatures identical to IPicture on 32-bit
  // platforms but correctly sized pointer arguments for 64-bit machines.
  //
  // Notice the differences between the definition below and the definition
  // from stdole2.tlb (in particular the use of void* in stdole2 to represent
  // the "HANDLE" types.  These are needed because IDL does not accept void*
  // as arguments.
  //
  // A very important point that allows us to do this is the fact that the
  // StdPicture objects are not remoteable (their IMarshal implementation
  // returns E_NOTIMPL).  This means that stdole2.tlb will never be used to
  // marshal a call to IPicture2.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPicture2);'}
  {$EXTERNALSYM IPicture2}
  IPicture2 = interface(IUnknown)
  ['{F5185DD8-2012-4b0b-AAD9-F052C6BD482B}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function get_Handle(out pHandle: HHANDLE): HRESULT; stdcall;

    function get_hPal(out phPal: HHANDLE): HRESULT; stdcall;

    function get_Type(out pType: SHORT): HRESULT; stdcall;

    function get_Width(out pWidth: OLE_XSIZE_HIMETRIC): HRESULT; stdcall;

    function get_Height(out pHeight: OLE_YSIZE_HIMETRIC): HRESULT; stdcall;

    function Render(hDC: HDC; x: LONG; y: LONG; cx: LONG; cy: LONG; xSrc: OLE_XPOS_HIMETRIC; ySrc: OLE_YPOS_HIMETRIC; cxSrc: OLE_XSIZE_HIMETRIC; cySrc: OLE_YSIZE_HIMETRIC; pRcWBounds: PRect): HRESULT; stdcall;

    function set_hPal(hPal: HHANDLE): HRESULT; stdcall;

    function get_CurDC(out phDC: HDC): HRESULT; stdcall;

    function SelectPicture(hDCIn: HDC; out phDCOut: HDC; out phBmpOut: HHANDLE): HRESULT; stdcall;

    function get_KeepOriginalFormat(out pKeep: BOOL): HRESULT; stdcall;

    function put_KeepOriginalFormat(keep: BOOL): HRESULT; stdcall;

    function PictureChanged(): HRESULT; stdcall;

    function SaveAsFile(pStream: IStream; fSaveMemCopy: BOOL; pCbSize: PLongint): HRESULT; stdcall;

    function get_Attributes(pDwAttr: PDWORD): HRESULT; stdcall;
  end;
  IID_IPicture2 = IPicture2;
  {$EXTERNALSYM IID_IPicture2}


  // Interface IFontEventsDisp
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFontEventsDisp);'}
  {$EXTERNALSYM IFontEventsDisp}
  IFontEventsDisp = interface(IDispatch)
  ['{4EF6100A-AF88-11D0-9846-00C04FC29993}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.

  end;
  IID_IFontEventsDisp = IFontEventsDisp;
  {$EXTERNALSYM IID_IFontEventsDisp}


  // Interface IFontDisp
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFontDisp);'}
  {$EXTERNALSYM IFontDisp}
  IFontDisp = interface(IDispatch)
  ['{BEF6E003-A874-101A-8BBA-00AA00300CAB}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.

  end;
  IID_IFontDisp = IFontDisp;
  {$EXTERNALSYM IID_IFontDisp}


  // Interface IPictureDisp
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPictureDisp);'}
  {$EXTERNALSYM IPictureDisp}
  IPictureDisp = interface(IDispatch)
  ['{7BF80981-BF32-101A-8BBB-00AA00300CAB}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.

  end;
  IID_IPictureDisp = IPictureDisp;
  {$EXTERNALSYM IID_IPictureDisp}


  //+---------------------------------------------------------------------------
  //
  //  Extensions to OLE and OLE Controls.
  //
  //----------------------------------------------------------------------------

  // Interface IOleInPlaceObjectWindowless
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceObjectWindowless);'}
  {$EXTERNALSYM IOleInPlaceObjectWindowless}
  IOleInPlaceObjectWindowless = interface(IOleInPlaceObject)
  ['{1C2056CC-5EF4-101B-8BC8-00AA003E3B29}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function OnWindowMessage(msg: UINT; wParam: WPARAM; lParam: LPARAM; plResult: PLRESULT): HRESULT; stdcall;

    function GetDropTarget(out ppDropTarget: IDropTarget): HRESULT; stdcall;
  end;
  IID_IOleInPlaceObjectWindowless = IOleInPlaceObjectWindowless;
  {$EXTERNALSYM IID_IOleInPlaceObjectWindowless}


  // Interface IOleInPlaceSiteEx
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceSiteEx);'}
  {$EXTERNALSYM IOleInPlaceSiteEx}
  IOleInPlaceSiteEx = interface(IOleInPlaceSite)
  ['{9C2CAD80-3424-11CF-B670-00AA004CD6D8}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function OnInPlaceActivateEx(out pfNoRedraw: BOOL; dwFlags: DWORD): HRESULT; stdcall;

    function OnInPlaceDeactivateEx(fNoRedraw: BOOL): HRESULT; stdcall;

    function RequestUIActivate(): HRESULT; stdcall;
  end;
  IID_IOleInPlaceSiteEx = IOleInPlaceSiteEx;
  {$EXTERNALSYM IID_IOleInPlaceSiteEx}


  // Interface IOleInPlaceSiteWindowless
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceSiteWindowless);'}
  {$EXTERNALSYM IOleInPlaceSiteWindowless}
  IOleInPlaceSiteWindowless = interface(IOleInPlaceSiteEx)
  ['{922EADA0-3424-11CF-B670-00AA004CD6D8}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function CanWindowlessActivate(): HRESULT; stdcall;

    function GetCapture(): HRESULT; stdcall;

    function SetCapture(fCapture: BOOL): HRESULT; stdcall;

    function GetFocus(): HRESULT; stdcall;

    function SetFocus(fFocus: BOOL): HRESULT; stdcall;

    function GetDC(pRect: PRect; grfFlags: DWORD; out phDC: HDC): HRESULT; stdcall;

    function ReleaseDC(hDC: HDC): HRESULT; stdcall;

    function InvalidateRect(pRect: PRect; fErase: BOOL): HRESULT; stdcall;

    function InvalidateRgn(hRGN: HRGN; fErase: BOOL): HRESULT; stdcall;

    function ScrollRect(dx: INT; dy: INT; pRectScroll: PRect; pRectClip: PRect): HRESULT; stdcall;

    function AdjustRect(prc: PRect): HRESULT; stdcall;

    function OnDefWindowMessage(msg: UINT; wParam: WPARAM; lParam: LPARAM; plResult: PLRESULT): HRESULT; stdcall;
  end;
  IID_IOleInPlaceSiteWindowless = IOleInPlaceSiteWindowless;
  {$EXTERNALSYM IID_IOleInPlaceSiteWindowless}


  // Interface IViewObjectEx
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IViewObjectEx);'}
  {$EXTERNALSYM IViewObjectEx}
  IViewObjectEx = interface(IViewObject2)
  ['{3AF24292-0C96-11CE-A0CF-00AA00600AB8}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetRect(dwAspect: DWORD; pRect: PRect): HRESULT; stdcall;

    function GetViewStatus(pdwStatus: PDWORD): HRESULT; stdcall;

    function QueryHitPoint(dwAspect: DWORD; pRectBounds: PRect; ptlLoc: POINT; lCloseHint: LONG; pHitResult: PDWORD): HRESULT; stdcall;

    function QueryHitRect(dwAspect: DWORD; pRectBounds: PRect; pRectLoc: PRect; lCloseHint: LONG; pHitResult: PDWORD): HRESULT; stdcall;

    function GetNaturalExtent(dwAspect: DWORD; lindex: LONG; ptd: PDVTARGETDEVICE; hicTargetDev: HDC; pExtentInfo: PDVEXTENTINFO; pSizel: PSIZEL): HRESULT; stdcall;
  end;
  IID_IViewObjectEx = IViewObjectEx;
  {$EXTERNALSYM IID_IViewObjectEx}


  // Interface IOleUndoUnit
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleUndoUnit);'}
  {$EXTERNALSYM IOleUndoUnit}
  IOleUndoUnit = interface(IUnknown)
  ['{894AD3B0-EF97-11CE-9BC9-00AA00608E01}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function &Do(pUndoManager: IOleUndoManager): HRESULT; stdcall;

    function GetDescription(out pBstr: BSTR): HRESULT; stdcall;

    function GetUnitType(pClsid: PGUID; plID: PLongint): HRESULT; stdcall;

    function OnNextAdd(): HRESULT; stdcall;
  end;
  IID_IOleUndoUnit = IOleUndoUnit;
  {$EXTERNALSYM IID_IOleUndoUnit}



  // Interface IOleParentUndoUnit
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleParentUndoUnit);'}
  {$EXTERNALSYM IOleParentUndoUnit}
  IOleParentUndoUnit = interface(IOleUndoUnit)
  ['{A1FAF330-EF97-11CE-9BC9-00AA00608E01}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Open(pPUU: IOleParentUndoUnit): HRESULT; stdcall;

    function Close(pPUU: IOleParentUndoUnit; fCommit: BOOL): HRESULT; stdcall;

    function Add(pUU: IOleUndoUnit): HRESULT; stdcall;

    function FindUnit(pUU: IOleUndoUnit): HRESULT; stdcall;

    function GetParentState(pdwState: PDWORD): HRESULT; stdcall;
  end;
  IID_IOleParentUndoUnit = IOleParentUndoUnit;
  {$EXTERNALSYM IID_IOleParentUndoUnit}



  // Interface IEnumOleUndoUnits
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumOleUndoUnits);'}
  {$EXTERNALSYM IEnumOleUndoUnits}
  IEnumOleUndoUnits = interface(IUnknown)
  ['{B3E7C340-EF97-11CE-9BC9-00AA00608E01}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Next(cElt: ULONG; out rgElt: IOleUndoUnit; pcEltFetched: PULONG): HRESULT; stdcall;

    function Skip(cElt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppEnum: IEnumOleUndoUnits): HRESULT; stdcall;
  end;
  IID_IEnumOleUndoUnits = IEnumOleUndoUnits;
  {$EXTERNALSYM IID_IEnumOleUndoUnits}



  // Interface IOleUndoManager
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleUndoManager);'}
  {$EXTERNALSYM IOleUndoManager}
  IOleUndoManager = interface(IUnknown)
  ['{D001F200-EF97-11CE-9BC9-00AA00608E01}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Open(pPUU: IOleParentUndoUnit): HRESULT; stdcall;

    function Close(pPUU: IOleParentUndoUnit; fCommit: BOOL): HRESULT; stdcall;

    function Add(pUU: IOleUndoUnit): HRESULT; stdcall;

    function GetOpenParentState(pdwState: PDWORD): HRESULT; stdcall;

    function DiscardFrom(pUU: IOleUndoUnit): HRESULT; stdcall;

    function UndoTo(pUU: IOleUndoUnit): HRESULT; stdcall;

    function RedoTo(pUU: IOleUndoUnit): HRESULT; stdcall;

    function EnumUndoable(out ppEnum: IEnumOleUndoUnits): HRESULT; stdcall;

    function EnumRedoable(out ppEnum: IEnumOleUndoUnits): HRESULT; stdcall;

    function GetLastUndoDescription(out pBstr: BSTR): HRESULT; stdcall;

    function GetLastRedoDescription(out pBstr: BSTR): HRESULT; stdcall;

    function Enable(fEnable: BOOL): HRESULT; stdcall;
  end;
  IID_IOleUndoManager = IOleUndoManager;
  {$EXTERNALSYM IID_IOleUndoManager}



  // Interface IPointerInactive
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPointerInactive);'}
  {$EXTERNALSYM IPointerInactive}
  IPointerInactive = interface(IUnknown)
  ['{55980BA0-35AA-11CF-B671-00AA004CD6D8}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetActivationPolicy(pdwPolicy: PDWORD): HRESULT; stdcall;

    function OnInactiveMouseMove(pRectBounds: PRect; x: LONG; y: LONG; grfKeyState: DWORD): HRESULT; stdcall;

    function OnInactiveSetCursor(pRectBounds: PRect; x: LONG; y: LONG; dwMouseMsg: DWORD; fSetAlways: BOOL): HRESULT; stdcall;
  end;
  IID_IPointerInactive = IPointerInactive;
  {$EXTERNALSYM IID_IPointerInactive}



  // Interface IObjectWithSite
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithSite);'}
  {$EXTERNALSYM IObjectWithSite}
  IObjectWithSite = interface(IUnknown)
  ['{FC4801A3-2BA9-11CF-A229-00AA003D7352}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function SetSite(pUnkSite: IUnknown): HRESULT; stdcall;

    function GetSite(const riid: TGUID; out ppvSite: Pointer): HRESULT; stdcall;
  end;
  IID_IObjectWithSite = IObjectWithSite;
  {$EXTERNALSYM IID_IObjectWithSite}



  // Interface IPerPropertyBrowsing
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPerPropertyBrowsing);'}
  {$EXTERNALSYM IPerPropertyBrowsing}
  IPerPropertyBrowsing = interface(IUnknown)
  ['{376BD3AA-3845-101B-84ED-08002B2EC713}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetDisplayString(dispID: DISPID; out pBstr: BSTR): HRESULT; stdcall;

    function MapPropertyToPage(dispID: DISPID; pClsid: PGUID): HRESULT; stdcall;

    function GetPredefinedStrings(dispID: DISPID; pCaStringsOut: PCALPOLESTR; pCaCookiesOut: PCADWORD): HRESULT; stdcall;

    function GetPredefinedValue(dispID: DISPID; dwCookie: DWORD; pVarOut: PVARIANT): HRESULT; stdcall;
  end;
  IID_IPerPropertyBrowsing = IPerPropertyBrowsing;
  {$EXTERNALSYM IID_IPerPropertyBrowsing}



  // Interface IPropertyBag2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyBag2);'}
  {$EXTERNALSYM IPropertyBag2}
  IPropertyBag2 = interface(IUnknown)
  ['{22F55882-280B-11d0-A8A9-00A0C90C2004}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Read(cProperties: ULONG; pPropBag: PPROPBAG2; pErrLog: IErrorLog; pvarValue: PVARIANT; phrError: PHRESULT): HRESULT; stdcall;

    function Write(cProperties: ULONG; pPropBag: PPROPBAG2; pvarValue: PVARIANT): HRESULT; stdcall;

    function CountProperties(pcProperties: PULONG): HRESULT; stdcall;

    function GetPropertyInfo(iProperty: ULONG; cProperties: ULONG; pPropBag: PPROPBAG2; pcProperties: PULONG): HRESULT; stdcall;

    function LoadObject(pstrName: PWideChar; dwHint: DWORD; pUnkObject: IUnknown; pErrLog: IErrorLog): HRESULT; stdcall;
  end;
  IID_IPropertyBag2 = IPropertyBag2;
  {$EXTERNALSYM IID_IPropertyBag2}



  // Interface IPersistPropertyBag2
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistPropertyBag2);'}
  {$EXTERNALSYM IPersistPropertyBag2}
  IPersistPropertyBag2 = interface(IPersist)
  ['{22F55881-280B-11d0-A8A9-00A0C90C2004}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function InitNew(): HRESULT; stdcall;

    function Load(pPropBag: IPropertyBag2; pErrLog: IErrorLog): HRESULT; stdcall;

    function Save(pPropBag: IPropertyBag2; fClearDirty: BOOL; fSaveAllProperties: BOOL): HRESULT; stdcall;

    function IsDirty(): HRESULT; stdcall;
  end;
  IID_IPersistPropertyBag2 = IPersistPropertyBag2;
  {$EXTERNALSYM IID_IPersistPropertyBag2}



  // Interface IAdviseSinkEx
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAdviseSinkEx);'}
  {$EXTERNALSYM IAdviseSinkEx}
  IAdviseSinkEx = interface(IAdviseSink)
  ['{3AF24290-0C96-11CE-A0CF-00AA00600AB8}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    procedure OnViewStatusChange(dwViewStatus: DWORD); stdcall;
  end;
  IID_IAdviseSinkEx = IAdviseSinkEx;
  {$EXTERNALSYM IID_IAdviseSinkEx}


  // Interface IQuickActivate
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IQuickActivate);'}
  {$EXTERNALSYM IQuickActivate}
  IQuickActivate = interface(IUnknown)
  ['{cf51ed10-62fe-11cf-bf86-00a0c9034836}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function QuickActivate(pQaContainer: PQACONTAINER; pQaControl: PQACONTROL): HRESULT; stdcall;

    function SetContentExtent(pSizel: PSIZEL): HRESULT; stdcall;

    function GetContentExtent(pSizel: PSIZEL): HRESULT; stdcall;
  end;
  IID_IQuickActivate = IQuickActivate;
  {$EXTERNALSYM IID_IQuickActivate}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

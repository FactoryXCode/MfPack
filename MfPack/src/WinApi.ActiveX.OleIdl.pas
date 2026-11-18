// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.OleIdl.pas
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
// Source: OleIdl.h
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
unit WinApi.ActiveX.OleIdl;

  {$HPPEMIT '#include "OleIdl.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.ObjIdlBase,
  WinApi.ActiveX.ObjIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'WinApiTypes.inc'}

const

  // Cache update Flags
  UPDFCACHE_NODATACACHE   =       DWORD($00000001);
  {$EXTERNALSYM UPDFCACHE_NODATACACHE}
  UPDFCACHE_ONSAVECACHE   =       DWORD($00000002);
  {$EXTERNALSYM UPDFCACHE_ONSAVECACHE}
  UPDFCACHE_ONSTOPCACHE   =       DWORD($00000004);
  {$EXTERNALSYM UPDFCACHE_ONSTOPCACHE}
  UPDFCACHE_NORMALCACHE   =       DWORD($00000008);
  {$EXTERNALSYM UPDFCACHE_NORMALCACHE}
  UPDFCACHE_IFBLANK       =       DWORD($00000010);
  {$EXTERNALSYM UPDFCACHE_IFBLANK}
  UPDFCACHE_ONLYIFBLANK   =       DWORD($80000000);
  {$EXTERNALSYM UPDFCACHE_ONLYIFBLANK}

  UPDFCACHE_IFBLANKORONSAVECACHE  = (UPDFCACHE_IFBLANK or UPDFCACHE_ONSAVECACHE);
  {$EXTERNALSYM UPDFCACHE_IFBLANKORONSAVECACHE}
  UPDFCACHE_ALL                   = UPDFCACHE_ONLYIFBLANK;
  {$EXTERNALSYM UPDFCACHE_ALL}
  UPDFCACHE_ALLBUTNODATACACHE     = (UPDFCACHE_ALL and UPDFCACHE_NODATACACHE);
  {$EXTERNALSYM UPDFCACHE_ALLBUTNODATACACHE}

  OLEVERBATTRIB_NEVERDIRTIES    = 1;
  {$EXTERNALSYM OLEVERBATTRIB_NEVERDIRTIES}
  OLEVERBATTRIB_ONCONTAINERMENU = 2;
  {$EXTERNALSYM OLEVERBATTRIB_ONCONTAINERMENU}

  MK_ALT = DWORD($0020);
  {$EXTERNALSYM MK_ALT}

  DROPEFFECT_NONE = DWORD(0);
  {$EXTERNALSYM DROPEFFECT_NONE}
  DROPEFFECT_COPY = DWORD(1);
  {$EXTERNALSYM DROPEFFECT_COPY}
  DROPEFFECT_MOVE = DWORD(2);
  {$EXTERNALSYM DROPEFFECT_MOVE}
  DROPEFFECT_LINK = DWORD(4);
  {$EXTERNALSYM DROPEFFECT_LINK}
{ 10.0.28000.0  }
  DROPEFFECT_NEWTARGET = DWORD($40000000);
  {$EXTERNALSYM DROPEFFECT_NEWTARGET}
  DROPEFFECT_BACKGROUNDTARGET = DWORD($20000000);
  {$EXTERNALSYM DROPEFFECT_BACKGROUNDTARGET}
  DROPEFFECT_SCROLL = DWORD($80000000);
  {$EXTERNALSYM DROPEFFECT_SCROLL}

type

  // Forward Interface Declarations

  LPOLEINPLACEACTIVEOBJECT = ^IOleInPlaceActiveObject;
  {$EXTERNALSYM LPOLEINPLACEACTIVEOBJECT}
  IOleInPlaceActiveObject = interface;

  LPVIEWOBJECT = ^IViewObject;
  {$EXTERNALSYM LPVIEWOBJECT}

  LPDROPSOURCE = ^IDropSource;
  {$EXTERNALSYM LPDROPSOURCE}

  LPDROPTARGET = ^IDropTarget;
  {$EXTERNALSYM LPDROPTARGET}

  PIEnumOLEVERB = ^IEnumOLEVERB;
  IEnumOLEVERB = interface;


  // Interface IOleAdviseHolder
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleAdviseHolder);'}
  {$EXTERNALSYM IOleAdviseHolder}
  IOleAdviseHolder = interface(IUnknown)
  ['{00000111-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Advise(pAdvise: IAdviseSink; pdwConnection: PDWORD): HRESULT; stdcall;

    function Unadvise(dwConnection: DWORD): HRESULT; stdcall;

    function EnumAdvise(out ppenumAdvise: IEnumSTATDATA): HRESULT; stdcall;

    function SendOnRename(pmk: IMoniker): HRESULT; stdcall;

    function SendOnSave(): HRESULT; stdcall;

    function SendOnClose(): HRESULT; stdcall;
  end;
  IID_IOleAdviseHolder = IOleAdviseHolder;
  {$EXTERNALSYM IID_IOleAdviseHolder}



  // Interface IOleCache
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleCache);'}
  {$EXTERNALSYM IOleCache}
  IOleCache = interface(IUnknown)
  ['{0000011E-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Cache(pformatetc: PFORMATETC; advf: DWORD; pdwConnection: PDWORD): HRESULT; stdcall;

    function Uncache(dwConnection: DWORD): HRESULT; stdcall;

    function EnumCache(out ppenumSTATDATA: IEnumSTATDATA): HRESULT; stdcall;

    function InitCache(pDataObject: IDataObject): HRESULT; stdcall;

    function SetData(pformatetc: PFORMATETC; pmedium: PSTGMEDIUM; fRelease: BOOL): HRESULT; stdcall;
  end;
  IID_IOleCache = IOleCache;
  {$EXTERNALSYM IID_IOleCache}


  // IOleCache2.DiscardCache options
  PDISCARDCACHE = ^tagDISCARDCACHE;
  tagDISCARDCACHE            = (
    DISCARDCACHE_SAVEIFDIRTY = 0,   // Save all dirty cache before discarding
    DISCARDCACHE_NOSAVE      = 1    // Don't save dirty caches before discarding
  );
  {$EXTERNALSYM tagDISCARDCACHE}
  DISCARDCACHE = tagDISCARDCACHE;
  {$EXTERNALSYM DISCARDCACHE}



  // Interface IOleCache2
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleCache2);'}
  {$EXTERNALSYM IOleCache2}
  IOleCache2 = interface(IOleCache)
  ['{00000128-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function UpdateCache(pDataObject: IDataObject; grfUpdf: DWORD; pReserved: Pointer): HRESULT; stdcall;

    function DiscardCache(dwDiscardOptions: DWORD): HRESULT; stdcall;
  end;
  IID_IOleCache2 = IOleCache2;
  {$EXTERNALSYM IID_IOleCache2}



  // Interface IOleCacheControl
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleCacheControl);'}
  {$EXTERNALSYM IOleCacheControl}
  IOleCacheControl = interface(IUnknown)
  ['{00000129-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function OnRun(pDataObject: IDataObject): HRESULT; stdcall;

    function OnStop(): HRESULT; stdcall;
  end;
  IID_IOleCacheControl = IOleCacheControl;
  {$EXTERNALSYM IID_IOleCacheControl}



  // Interface IParseDisplayName
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IParseDisplayName);'}
  {$EXTERNALSYM IParseDisplayName}
  IParseDisplayName = interface(IUnknown)
  ['{0000011A-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function ParseDisplayName(pbc: IBindCtx; pszDisplayName: PWideChar; pchEaten: PULONG; out ppmkOut: IMoniker): HRESULT; stdcall;
  end;
  IID_IParseDisplayName = IParseDisplayName;
  {$EXTERNALSYM IID_IParseDisplayName}



  // Interface IOleContainer
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleContainer);'}
  {$EXTERNALSYM IOleContainer}
  IOleContainer = interface(IParseDisplayName)
  ['{0000011B-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function EnumObjects(grfFlags: DWORD; out ppenum: IEnumUnknown): HRESULT; stdcall;

    function LockContainer(fLock: BOOL): HRESULT; stdcall;
  end;
  IID_IOleContainer = IOleContainer;
  {$EXTERNALSYM IID_IOleContainer}



  // Interface IOleClientSite
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleClientSite);'}
  {$EXTERNALSYM IOleClientSite}
  IOleClientSite = interface(IUnknown)
  ['{00000118-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function SaveObject(): HRESULT; stdcall;

    function GetMoniker(dwAssign: DWORD; dwWhichMoniker: DWORD; out ppmk: IMoniker): HRESULT; stdcall;

    function GetContainer(out ppContainer: IOleContainer): HRESULT; stdcall;

    function ShowObject(): HRESULT; stdcall;

    function OnShowWindow(fShow: BOOL): HRESULT; stdcall;

    function RequestNewObjectLayout(): HRESULT; stdcall;
  end;
  IID_IOleClientSite = IOleClientSite;
  {$EXTERNALSYM IID_IOleClientSite}


  POLEGETMONIKER = ^OLEGETMONIKER;
  tagOLEGETMONIKER            = (
    OLEGETMONIKER_ONLYIFTHERE = 1,
    OLEGETMONIKER_FORCEASSIGN = 2,
    OLEGETMONIKER_UNASSIGN    = 3,
    OLEGETMONIKER_TEMPFORUSER = 4
  );
  {$EXTERNALSYM tagOLEGETMONIKER}
  OLEGETMONIKER = tagOLEGETMONIKER;
  {$EXTERNALSYM OLEGETMONIKER}

  POLEWHICHMK = ^OLEWHICHMK;
  tagOLEWHICHMK          = (
    OLEWHICHMK_CONTAINER = 1,
    OLEWHICHMK_OBJREL    = 2,
    OLEWHICHMK_OBJFULL   = 3
  );
  {$EXTERNALSYM tagOLEWHICHMK}
  OLEWHICHMK = tagOLEWHICHMK;
  {$EXTERNALSYM OLEWHICHMK}

  PUSERCLASSTYPE = ^USERCLASSTYPE;
  tagUSERCLASSTYPE        = (
    USERCLASSTYPE_FULL    = 1,
    USERCLASSTYPE_SHORT   = 2,
    USERCLASSTYPE_APPNAME = 3
  );
  {$EXTERNALSYM tagUSERCLASSTYPE}
  USERCLASSTYPE = tagUSERCLASSTYPE;
  {$EXTERNALSYM USERCLASSTYPE}


  POLEMISC = ^OLEMISC;
  tagOLEMISC                             = (
    OLEMISC_RECOMPOSEONRESIZE            = $00000001,
    OLEMISC_ONLYICONIC                   = $00000002,
    OLEMISC_INSERTNOTREPLACE             = $00000004,
    OLEMISC_STATIC                       = $00000008,
    OLEMISC_CANTLINKINSIDE               = $00000010,
    OLEMISC_CANLINKBYOLE1                = $00000020,
    OLEMISC_ISLINKOBJECT                 = $00000040,
    OLEMISC_INSIDEOUT                    = $00000080,
    OLEMISC_ACTIVATEWHENVISIBLE          = $00000100,
    OLEMISC_RENDERINGISDEVICEINDEPENDENT = $00000200,
    OLEMISC_INVISIBLEATRUNTIME           = $00000400,
    OLEMISC_ALWAYSRUN                    = $00000800,
    OLEMISC_ACTSLIKEBUTTON               = $00001000,
    OLEMISC_ACTSLIKELABEL                = $00002000,
    OLEMISC_NOUIACTIVATE                 = $00004000,
    OLEMISC_ALIGNABLE                    = $00008000,
    OLEMISC_SIMPLEFRAME                  = $00010000,
    OLEMISC_SETCLIENTSITEFIRST           = $00020000,
    OLEMISC_IMEMODE                      = $00040000,
    OLEMISC_IGNOREACTIVATEWHENVISIBLE    = $00080000,
    OLEMISC_WANTSTOMENUMERGE             = $00100000,
    OLEMISC_SUPPORTSMULTILEVELUNDO       = $00200000
  );
  {$EXTERNALSYM tagOLEMISC}
  OLEMISC = tagOLEMISC;
  {$EXTERNALSYM OLEMISC}

  POLECLOSE = ^OLECLOSE;
  tagOLECLOSE            = (
    OLECLOSE_SAVEIFDIRTY = 0,
    OLECLOSE_NOSAVE      = 1,
    OLECLOSE_PROMPTSAVE  = 2
  );
  {$EXTERNALSYM tagOLECLOSE}
  OLECLOSE = tagOLECLOSE;
  {$EXTERNALSYM OLECLOSE}


  // Interface IOleObject
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleObject);'}
  {$EXTERNALSYM IOleObject}
  IOleObject = interface(IUnknown)
  ['{00000112-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function SetClientSite(pClientSite: IOleClientSite): HRESULT; stdcall;

    function GetClientSite(out ppClientSite: IOleClientSite): HRESULT; stdcall;

    function SetHostNames(szContainerApp: PWideChar; szContainerObj: PWideChar): HRESULT; stdcall;

    function Close(dwSaveOption: DWORD): HRESULT; stdcall;

    function SetMoniker(dwWhichMoniker: DWORD; pmk: IMoniker): HRESULT; stdcall;

    function GetMoniker(dwAssign: DWORD; dwWhichMoniker: DWORD; out ppmk: IMoniker): HRESULT; stdcall;

    function InitFromData(pDataObject: IDataObject; fCreation: BOOL; dwReserved: DWORD): HRESULT; stdcall;

    function GetClipboardData(dwReserved: DWORD; out ppDataObject: IDataObject): HRESULT; stdcall;

    function DoVerb(iVerb: LONG; lpmsg: PMSG; pActiveSite: IOleClientSite; lindex: LONG; hwndParent: HWND; lprcPosRect: PRect): HRESULT; stdcall;

    function EnumVerbs(out ppEnumOleVerb: IEnumOLEVERB): HRESULT; stdcall;

    function Update(): HRESULT; stdcall;

    function IsUpToDate(): HRESULT; stdcall;

    function GetUserClassID(pClsid: PGUID): HRESULT; stdcall;

    function GetUserType(dwFormOfType: DWORD; out pszUserType: PWideChar): HRESULT; stdcall;

    function SetExtent(dwDrawAspect: DWORD; psizel: PSIZEL): HRESULT; stdcall;

    function GetExtent(dwDrawAspect: DWORD; psizel: PSIZEL): HRESULT; stdcall;

    function Advise(pAdvSink: IAdviseSink; pdwConnection: PDWORD): HRESULT; stdcall;

    function Unadvise(dwConnection: DWORD): HRESULT; stdcall;

    function EnumAdvise(out ppenumAdvise: IEnumSTATDATA): HRESULT; stdcall;

    function GetMiscStatus(dwAspect: DWORD; pdwStatus: PDWORD): HRESULT; stdcall;

    function SetColorScheme(pLogpal: PLOGPALETTE): HRESULT; stdcall;
  end;
  IID_IOleObject = IOleObject;
  {$EXTERNALSYM IID_IOleObject}


  //****** OLE value types ***********************************************/

  //* rendering options *//

  LPOLERENDER = ^OLERENDER;
  {$EXTERNALSYM LPOLERENDER}
  POLERENDER = ^OLERENDER;
  tagOLERENDER       = (
      OLERENDER_NONE   = 0,
      OLERENDER_DRAW   = 1,
      OLERENDER_FORMAT = 2,
      OLERENDER_ASIS   = 3
  );
  {$EXTERNALSYM tagOLERENDER}
  OLERENDER = tagOLERENDER;
  {$EXTERNALSYM OLERENDER}



  //****** Clipboard Data structures *****************************************/

  POBJECTDESCRIPTOR = ^tagOBJECTDESCRIPTOR;
  LPOBJECTDESCRIPTOR = ^tagOBJECTDESCRIPTOR;
  {$EXTERNALSYM LPOBJECTDESCRIPTOR}
  PLINKSRCDESCRIPTOR = ^tagOBJECTDESCRIPTOR;
  LPLINKSRCDESCRIPTOR = ^tagOBJECTDESCRIPTOR;
  {$EXTERNALSYM LPLINKSRCDESCRIPTOR}
  tagOBJECTDESCRIPTOR = record
    cbSize: ULONG;                   // Size of structure in bytes
    clsid: CLSID;                    // CLSID of data being transferred
    dwDrawAspect: DWORD;             // Display aspect of the object
                                     //     normally DVASPECT_CONTENT or ICON.
                                     //     dwDrawAspect will be 0 (which is NOT
                                     //     DVASPECT_CONTENT) if the copier or
                                     //     dragsource didn't draw the object to
                                     //     begin with.
    sizel: SIZEL;                    // size of the object in HIMETRIC
                                     //    sizel is opt.: will be (0,0) for apps
                                     //    which don't draw the object being
                                     //    transferred
    pointl: POINTL;                  // Offset in HIMETRIC units from the
                                     //    upper-left corner of the obj where the
                                     //    mouse went down for the drag.
                                     //    NOTE: y coordinates increase downward.
                                     //          x coordinates increase to right
                                     //    pointl is opt.; it is only meaningful
                                     //    if object is transfered via drag/drop.
                                     //    (0, 0) if mouse position is unspecified
                                     //    (eg. when obj transfered via clipboard)
    dwStatus: DWORD;                 // Misc. status flags for object. Flags are
                                     //    defined by OLEMISC enum. these flags
                                     //    are as would be returned
                                     //    by IOleObject::GetMiscStatus.
    dwFullUserTypeName: DWORD;       // Offset from beginning of structure to
                                     //    null-terminated string that specifies
                                     //    Full User Type Name of the object.
                                     //    0 indicates string not present.
    dwSrcOfCopy: DWORD;              // Offset from beginning of structure to
                                     //    null-terminated string that specifies
                                     //    source of the transfer.
                                     //    dwSrcOfCOpy is normally implemented as
                                     //    the display name of the temp-for-user
                                     //    moniker which identifies the source of
                                     //    the data.
                                     //    0 indicates string not present.
                                     //    NOTE: moniker assignment is NOT forced.
                                     //    see IOleObject.GetMoniker(OLEGETMONIKER_TEMPFORUSER)

    { variable sized string data may appear here }

  end;
  {$EXTERNALSYM tagOBJECTDESCRIPTOR}
  OBJECTDESCRIPTOR = tagOBJECTDESCRIPTOR;
  {$EXTERNALSYM OBJECTDESCRIPTOR}
  LINKSRCDESCRIPTOR = tagOBJECTDESCRIPTOR;
  {$EXTERNALSYM LINKSRCDESCRIPTOR}



  // Interface IOleWindow
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleWindow);'}
  {$EXTERNALSYM IOleWindow}
  IOleWindow = interface(IUnknown)
  ['{00000114-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetWindow(out phwnd: HWND): HRESULT; stdcall;

    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
  end;
  IID_IOleWindow = IOleWindow;
  {$EXTERNALSYM IID_IOleWindow}


  //* Link update options *//
  LPOLEUPDATE = ^OLEUPDATE;
  {$EXTERNALSYM LPOLEUPDATE}
  POLEUPDATE = ^OLEUPDATE;
  tagOLEUPDATE       = (
    OLEUPDATE_ALWAYS = 1,
    OLEUPDATE_ONCALL = 3
  );
  {$EXTERNALSYM tagOLEUPDATE}
  OLEUPDATE = tagOLEUPDATE;
  {$EXTERNALSYM OLEUPDATE}

  // for IOleLink.BindToSource
  POLELINKBIND = ^OLELINKBIND;
  tagOLELINKBIND                = (
    OLELINKBIND_EVENIFCLASSDIFF = 1
  );
  {$EXTERNALSYM tagOLELINKBIND}
  OLELINKBIND = tagOLELINKBIND;
  {$EXTERNALSYM OLELINKBIND}


  // Interface IOleLink
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleLink);'}
  {$EXTERNALSYM IOleLink}
  IOleLink = interface(IUnknown)
  ['{0000011D-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function SetUpdateOptions(dwUpdateOpt: DWORD): HRESULT; stdcall;

    function GetUpdateOptions(pdwUpdateOpt: PDWORD): HRESULT; stdcall;

    function SetSourceMoniker(pmk: IMoniker; const rclsid: TGUID): HRESULT; stdcall;

    function GetSourceMoniker(out ppmk: IMoniker): HRESULT; stdcall;

    function SetSourceDisplayName(pszStatusText: PWideChar): HRESULT; stdcall;

    function GetSourceDisplayName(out ppszDisplayName: PWideChar): HRESULT; stdcall;

    function BindToSource(bindflags: DWORD; pbc: IBindCtx): HRESULT; stdcall;

    function BindIfRunning(): HRESULT; stdcall;

    function GetBoundSource(out ppunk: IUnknown): HRESULT; stdcall;

    function UnbindSource(): HRESULT; stdcall;

    function Update(pbc: IBindCtx): HRESULT; stdcall;
  end;
  IID_IOleLink = IOleLink;
  {$EXTERNALSYM IID_IOleLink}


  // Interface IOleItemContainer
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleItemContainer);'}
  {$EXTERNALSYM IOleItemContainer}
  IOleItemContainer = interface(IOleContainer)
  ['{0000011C-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetObject(pszItem: PWideChar; dwSpeedNeeded: DWORD; pbc: IBindCtx; const riid: TGUID; out ppvObject: Pointer): HRESULT; stdcall;

    function GetObjectStorage(pszItem: PWideChar; pbc: IBindCtx; const riid: TGUID; out ppvStorage: Pointer): HRESULT; stdcall;

    function IsRunning(pszItem: PWideChar): HRESULT; stdcall;
  end;
  IID_IOleItemContainer = IOleItemContainer;
  {$EXTERNALSYM IID_IOleItemContainer}


  PBORDERWIDTHS = ^BORDERWIDTHS;
  BORDERWIDTHS = TRect;
  {$EXTERNALSYM BORDERWIDTHS}

  PLPBORDERWIDTHS = ^LPBORDERWIDTHS;
  LPBORDERWIDTHS = LPRECT;
  {$EXTERNALSYM LPBORDERWIDTHS}

  PLPCBORDERWIDTHS = ^LPCBORDERWIDTHS;
  LPCBORDERWIDTHS = LPCRECT;
  {$EXTERNALSYM LPCBORDERWIDTHS}


  // Interface IOleInPlaceUIWindow
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceUIWindow);'}
  {$EXTERNALSYM IOleInPlaceUIWindow}
  IOleInPlaceUIWindow = interface(IOleWindow)
  ['{00000115-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetBorder(lprectBorder: PRect): HRESULT; stdcall;

    function RequestBorderSpace(pborderwidths: PBORDERWIDTHS): HRESULT; stdcall;

    function SetBorderSpace(pborderwidths: PBORDERWIDTHS): HRESULT; stdcall;

    function SetActiveObject(pActiveObject: IOleInPlaceActiveObject; pszObjName: PWideChar): HRESULT; stdcall;
  end;
  IID_IOleInPlaceUIWindow = IOleInPlaceUIWindow;
  {$EXTERNALSYM IID_IOleInPlaceUIWindow}



  // Interface IOleInPlaceActiveObject
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceActiveObject);'}
  {$EXTERNALSYM IOleInPlaceActiveObject}
  IOleInPlaceActiveObject = interface(IOleWindow)
  ['{00000117-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function TranslateAccelerator(lpmsg: PMSG): HRESULT; stdcall;

    function OnFrameWindowActivate(fActivate: BOOL): HRESULT; stdcall;

    function OnDocWindowActivate(fActivate: BOOL): HRESULT; stdcall;

    function ResizeBorder(prcBorder: PRect; pUIWindow: IOleInPlaceUIWindow; fFrameWindow: BOOL): HRESULT; stdcall;

    function EnableModeless(fEnable: BOOL): HRESULT; stdcall;
  end;
  IID_IOleInPlaceActiveObject = IOleInPlaceActiveObject;
  {$EXTERNALSYM IID_IOleInPlaceActiveObject}


  POLEINPLACEFRAMEINFO = ^OLEINPLACEFRAMEINFO;
  tagOIFI = record
    cb: UINT;
    fMDIApp: BOOL;
    hwndFrame: HWND;
    haccel: HAccel;
    cAccelEntries: UINT;
  end;
  {$EXTERNALSYM tagOIFI}
  OLEINPLACEFRAMEINFO = tagOIFI;
  {$EXTERNALSYM OLEINPLACEFRAMEINFO}

  POLEMENUGROUPWIDTHS = ^OLEMENUGROUPWIDTHS;
  tagOleMenuGroupWidths = record
    _width: array[0..5] of Longint;
  end;
  {$EXTERNALSYM tagOleMenuGroupWidths}
  OLEMENUGROUPWIDTHS = tagOleMenuGroupWidths;
  {$EXTERNALSYM OLEMENUGROUPWIDTHS}

  PHOLEMENU = ^HOLEMENU;
  {$EXTERNALSYM HOLEMENU}
  HOLEMENU = HGLOBAL;



  // Interface IOleInPlaceFrame
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceFrame);'}
  {$EXTERNALSYM IOleInPlaceFrame}
  IOleInPlaceFrame = interface(IOleInPlaceUIWindow)
  ['{00000116-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function InsertMenus(hmenuShared: HMENU; lpMenuWidths: POLEMENUGROUPWIDTHS): HRESULT; stdcall;

    function SetMenu(hmenuShared: HMENU; holemenu: HOLEMENU; hwndActiveObject: HWND): HRESULT; stdcall;

    function RemoveMenus(hmenuShared: HMENU): HRESULT; stdcall;

    function SetStatusText(pszStatusText: PWideChar): HRESULT; stdcall;

    function EnableModeless(fEnable: BOOL): HRESULT; stdcall;

    function TranslateAccelerator(lpmsg: PMSG; wID: WORD): HRESULT; stdcall;
  end;
  IID_IOleInPlaceFrame = IOleInPlaceFrame;
  {$EXTERNALSYM IID_IOleInPlaceFrame}



  // Interface IOleInPlaceObject
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceObject);'}
  {$EXTERNALSYM IOleInPlaceObject}
  IOleInPlaceObject = interface(IOleWindow)
  ['{00000113-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function InPlaceDeactivate(): HRESULT; stdcall;

    function UIDeactivate(): HRESULT; stdcall;

    function SetObjectRects(lprcPosRect: PRect; lprcClipRect: PRect): HRESULT; stdcall;

    function ReactivateAndUndo(): HRESULT; stdcall;
  end;
  IID_IOleInPlaceObject = IOleInPlaceObject;
  {$EXTERNALSYM IID_IOleInPlaceObject}



  // Interface IOleInPlaceSite
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleInPlaceSite);'}
  {$EXTERNALSYM IOleInPlaceSite}
  IOleInPlaceSite = interface(IOleWindow)
  ['{00000119-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function CanInPlaceActivate(): HRESULT; stdcall;

    function OnInPlaceActivate(): HRESULT; stdcall;

    function OnUIActivate(): HRESULT; stdcall;

    function GetWindowContext(out ppFrame: IOleInPlaceFrame; out ppDoc: IOleInPlaceUIWindow; lprcPosRect: PRect; lprcClipRect: PRect; lpFrameInfo: POLEINPLACEFRAMEINFO): HRESULT; stdcall;

    function Scroll(scrollExtant: SIZE): HRESULT; stdcall;

    function OnUIDeactivate(fUndoable: BOOL): HRESULT; stdcall;

    function OnInPlaceDeactivate(): HRESULT; stdcall;

    function DiscardUndoState(): HRESULT; stdcall;

    function DeactivateAndUndo(): HRESULT; stdcall;

    function OnPosRectChange(lprcPosRect: PRect): HRESULT; stdcall;
  end;
  IID_IOleInPlaceSite = IOleInPlaceSite;
  {$EXTERNALSYM IID_IOleInPlaceSite}



  // Interface IContinue
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContinue);'}
  {$EXTERNALSYM IContinue}
  IContinue = interface(IUnknown)
  ['{0000012a-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function FContinue(): HRESULT; stdcall;
  end;
  IID_IContinue = IContinue;
  {$EXTERNALSYM IID_IContinue}

 //or see ActiveX
  FuncContinue = function(dwContinue: ULONG_PTR): BOOL; stdcall;
  {$EXTERNALSYM FuncContinue}


  // Interface IViewObject
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IViewObject);'}
  {$EXTERNALSYM IViewObject}
  IViewObject = interface(IUnknown)
  ['{0000010D-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Draw(dwDrawAspect: DWORD; lindex: LONG; pvAspect: Pointer; ptd: PDVTARGETDEVICE; hdcTargetDev: HDC; hdcDraw: HDC; lprcBounds: PRect; lprcWBounds: PRect; pfnContinue: FuncContinue; dwContinue: ULONG_PTR): HRESULT; stdcall;

    function GetColorSet(dwDrawAspect: DWORD; lindex: LONG; pvAspect: Pointer; ptd: PDVTARGETDEVICE; hicTargetDev: HDC; out ppColorSet: PLOGPALETTE): HRESULT; stdcall;

    function Freeze(dwDrawAspect: DWORD; lindex: LONG; pvAspect: Pointer; pdwFreeze: PDWORD): HRESULT; stdcall;

    function Unfreeze(dwFreeze: DWORD): HRESULT; stdcall;

    function SetAdvise(aspects: DWORD; advf: DWORD; pAdvSink: IAdviseSink): HRESULT; stdcall;

    function GetAdvise(pAspects: PDWORD; pAdvf: PDWORD; out ppAdvSink: IAdviseSink): HRESULT; stdcall;
  end;
  IID_IViewObject = IViewObject;
  {$EXTERNALSYM IID_IViewObject}


  // Interface IViewObject2
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IViewObject2);'}
  {$EXTERNALSYM IViewObject2}
  IViewObject2 = interface(IViewObject)
  ['{00000127-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function GetExtent(dwDrawAspect: DWORD; lindex: LONG; ptd: PDVTARGETDEVICE; lpsizel: PSIZEL): HRESULT; stdcall;
  end;



  // Interface IDropSource
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropSource);'}
  {$EXTERNALSYM IDropSource}
  IDropSource = interface(IUnknown)
  ['{00000121-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HRESULT; stdcall;

    function GiveFeedback(dwEffect: DWORD): HRESULT; stdcall;
  end;
  IID_IDropSource = IDropSource;
  {$EXTERNALSYM IID_IDropSource}


  // Interface IDropTarget
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropTarget);'}
  {$EXTERNALSYM IDropTarget}
  IDropTarget = interface(IUnknown)
  ['{00000122-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function DragEnter(pDataObj: IDataObject; grfKeyState: DWORD; pt: POINTL; pdwEffect: PDWORD): HRESULT; stdcall;

    function DragOver(grfKeyState: DWORD; pt: POINTL; pdwEffect: PDWORD): HRESULT; stdcall;

    function DragLeave(): HRESULT; stdcall;

    function Drop(pDataObj: IDataObject; grfKeyState: DWORD; pt: POINTL; pdwEffect: PDWORD): HRESULT; stdcall;
  end;
  IID_IDropTarget = IDropTarget;
  {$EXTERNALSYM IID_IDropTarget}



  // Interface IDropSourceNotify
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropSourceNotify);'}
  {$EXTERNALSYM IDropSourceNotify}
  IDropSourceNotify = interface(IUnknown)
  ['{0000012B-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function DragEnterTarget(hwndTarget: HWND): HRESULT; stdcall;

    function DragLeaveTarget(): HRESULT; stdcall;
  end;
  IID_IDropSourceNotify = IDropSourceNotify;
  {$EXTERNALSYM IID_IDropSourceNotify}


  // Interface IEnterpriseDropTarget
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnterpriseDropTarget);'}
  {$EXTERNALSYM IEnterpriseDropTarget}
  IEnterpriseDropTarget = interface(IUnknown)
  ['{390E3878-FD55-4E18-819D-4682081C0CFD}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function SetDropSourceEnterpriseId(identity: PWideChar): HRESULT; stdcall;

    function IsEvaluatingEdpPolicy(out value: BOOL): HRESULT; stdcall;
  end;
  IID_IEnterpriseDropTarget = IEnterpriseDropTarget;
  {$EXTERNALSYM IID_IEnterpriseDropTarget}


  POLEVERB = ^OLEVERB;
  tagOLEVERB = record
    lVerb: Longint;
    lpszVerbName: LPOLESTR;
    fuFlags: DWORD;
    grfAttribs: DWORD;
  end;
  {$EXTERNALSYM tagOLEVERB}
  OLEVERB = tagOLEVERB;
  {$EXTERNALSYM OLEVERB}

  // Bitwise verb attributes used in OLEVERB.grfAttribs
  tagOLEVERBATTRIB = DWORD;           // bitwise
  {$EXTERNALSYM tagOLEVERBATTRIB}
  OLEVERBATTRIB = tagOLEVERBATTRIB;
  {$EXTERNALSYM OLEVERBATTRIB}


  // Interface IEnumOLEVERB
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumOLEVERB);'}
  {$EXTERNALSYM IEnumOLEVERB}
  IEnumOLEVERB = interface(IUnknown)
  ['{00000104-0000-0000-C000-000000000046}']

    // SDK native virtual methods. Pointer parameters preserve optional buffers and arrays.
    function Next(celt: ULONG; rgelt: POLEVERB; pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumOLEVERB): HRESULT; stdcall;
  end;
  IID_IEnumOLEVERB = IEnumOLEVERB;
  {$EXTERNALSYM IID_IEnumOLEVERB}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

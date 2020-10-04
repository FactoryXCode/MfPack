// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ComBaseApi.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Base Component Object Model defintions.
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
// Remarks:
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
// Source: combaseapi.h
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
unit WinApi.ComBaseApi;

  {$HPPEMIT '#include "combaseapi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.PropIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


//****** Interface Declaration ***********************************************/

//*
// *      These are macros for declaring interfaces.  They exist so that
// *      a single definition of the interface is simulataneously a proper
// *      declaration of the interface structures (C++ abstract classes)
// *      for both C and C++.
// *
// *      DECLARE_INTERFACE(iface) is used to declare an interface that does
// *      not derive from a base interface.
// *      DECLARE_INTERFACE_(iface, baseiface) is used to declare an interface
// *      that does derive from a base interface.
// *
// *      By default if the source file has a .c extension the C version of
// *      the interface declaratations will be expanded; if it has a .cpp
// *      extension the C++ version will be expanded. if you want to force
// *      the C version expansion even though the source file has a .cpp
// *      extension, then define the macro "CINTERFACE".
// *      eg.     cl -DCINTERFACE file.cpp
// *
// *      Example Interface declaration:
// *
// *          #undef  INTERFACE
// *          #define INTERFACE   IClassFactory
// *
// *          DECLARE_INTERFACE_(IClassFactory, IUnknown)
// *          {
// *              // *** IUnknown methods ***
// *              STDMETHOD(QueryInterface) (THIS_
// *                                        REFIID riid,
// *                                        LPVOID FAR* ppvObj) PURE;
// *              STDMETHOD_(ULONG,AddRef) (THIS) PURE;
// *              STDMETHOD_(ULONG,Release) (THIS) PURE;
// *
// *              // *** IClassFactory methods ***
// *              STDMETHOD(CreateInstance) (THIS_
// *                                        LPUNKNOWN pUnkOuter,
// *                                        REFIID riid,
// *                                        LPVOID FAR* ppvObject) PURE;
// *          };
// *
// *      Example C++ expansion:
// *
// *          struct FAR IClassFactory : public IUnknown
// *          {
// *              virtual HRESULT STDMETHODCALLTYPE QueryInterface(
// *                                                  IID FAR& riid,
// *                                                  LPVOID FAR* ppvObj) = 0;
// *              virtual HRESULT STDMETHODCALLTYPE AddRef(void) = 0;
// *              virtual HRESULT STDMETHODCALLTYPE Release(void) = 0;
// *              virtual HRESULT STDMETHODCALLTYPE CreateInstance(
// *                                              LPUNKNOWN pUnkOuter,
// *                                              IID FAR& riid,
// *                                              LPVOID FAR* ppvObject) = 0;
// *          };
// *
// *          NOTE: Our documentation says '#define interface class' but we use
// *          'struct' instead of 'class' to keep a lot of 'public:' lines
// *          out of the interfaces.  The 'FAR' forces the 'this' pointers to
// *          be far, which is what we need.
// *
// *      Example C expansion:
// *
// *          typedef struct IClassFactory
// *          {
// *              const struct IClassFactoryVtbl FAR* lpVtbl;
// *          } IClassFactory;
// *
// *          typedef struct IClassFactoryVtbl IClassFactoryVtbl;
// *
// *          struct IClassFactoryVtbl
// *          {
// *              HRESULT (STDMETHODCALLTYPE * QueryInterface) (
// *                                                  IClassFactory FAR* This,
// *                                                  IID FAR* riid,
// *                                                  LPVOID FAR* ppvObj) ;
// *              HRESULT (STDMETHODCALLTYPE * AddRef) (IClassFactory FAR* This) ;
// *              HRESULT (STDMETHODCALLTYPE * Release) (IClassFactory FAR* This) ;
// *              HRESULT (STDMETHODCALLTYPE * CreateInstance) (
// *                                                  IClassFactory FAR* This,
// *                                                  LPUNKNOWN pUnkOuter,
// *                                                  IID FAR* riid,
// *                                                  LPVOID FAR* ppvObject);
// *              HRESULT (STDMETHODCALLTYPE * LockServer) (
// *                                                  IClassFactory FAR* This,
// *                                                  BOOL fLock);
// *          };
// */


const

  CLSCTX_INPROC               = (CLSCTX_INPROC_SERVER or CLSCTX_INPROC_HANDLER);
  {$EXTERNALSYM CLSCTX_INPROC}

  // With DCOM, CLSCTX_REMOTE_SERVER should be included
  // DCOM
  //#if (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM)

  CLSCTX_ALL                  = (CLSCTX_INPROC_SERVER or
                                 CLSCTX_INPROC_HANDLER or
                                 CLSCTX_LOCAL_SERVER or
                                 CLSCTX_REMOTE_SERVER);
  {$EXTERNALSYM CLSCTX_ALL}


  CLSCTX_SERVER = CLSCTX_INPROC_SERVER or
                  CLSCTX_LOCAL_SERVER or
                  CLSCTX_REMOTE_SERVER;
  {$EXTERNALSYM CLSCTX_SERVER}


type
  // class registration flags; passed to CoRegisterClassObject
  PREGCLS = ^tagREGCLS;
  tagREGCLS = DWord;
  {$EXTERNALSYM tagREGCLS}
  REGCLS = tagREGCLS;
  {$EXTERNALSYM REGCLS}
const
  REGCLS_SINGLEUSE      = REGCLS(0);  // class object only generates one instance
  {$EXTERNALSYM REGCLS_SINGLEUSE}

  REGCLS_MULTIPLEUSE    = REGCLS(1); // same class object genereates multiple inst.
  {$EXTERNALSYM REGCLS_MULTIPLEUSE}  // and local automatically goes into inproc tbl.

  REGCLS_MULTI_SEPARATE = REGCLS(2);    // multiple use, but separate control over each
  {$EXTERNALSYM REGCLS_MULTI_SEPARATE}  // context.

  REGCLS_SUSPENDED      = REGCLS(4);  // register is as suspended, will be activated
  {$EXTERNALSYM REGCLS_SUSPENDED}     // when app calls CoResumeClassObjects

  REGCLS_SURROGATE      = REGCLS(8);  // must be used when a surrogate process
  {$EXTERNALSYM REGCLS_SURROGATE}     // is registering a class object that will be
                                      // loaded in the surrogate

    //#if (NTDDI_VERSION >= NTDDI_WINTHRESHOLD)

  REGCLS_AGILE         = REGCLS($10);  // Class object aggregates the free-threaded marshaler
  {$EXTERNALSYM REGCLS_AGILE}          // and will be made visible to all inproc apartments.
                                       // Can be used together with other flags - for example,
                                       // REGCLS_AGILE | REGCLS_MULTIPLEUSE to register a
                                       // class object that can be used multiple times from
                                       // different apartments. Without other flags, behavior
                                       // will retain REGCLS_SINGLEUSE semantics in that only
                                       // one instance can be generated.



  //* here is where we pull in the MIDL generated headers for the interfaces */

  // from ObjIdl.pas
  //PIRpcStubBuffer = ^IRpcStubBuffer;
  //IRpcStubBuffer = interface;
  //PIRpcChannelBuffer = ^IRpcChannelBuffer;
  //IRpcChannelBuffer = interface;


  // COM initialization flags; passed to CoInitialize.
type
  PCOINITBASE = ^tagCOINITBASE;
  tagCOINITBASE  = DWord;
  {$EXTERNALSYM tagCOINITBASE}
  COINITBASE = tagCOINITBASE;
  {$EXTERNALSYM COINITBASE}
const
  // These constants are only valid on Windows NT 4.0
  COINITBASE_MULTITHREADED      = COINITBASE($0); // OLE calls objects on any thread.
  {$EXTERNALSYM COINITBASE_MULTITHREADED}


  //****** STD Object API Prototypes *****************************************/


  function CoGetMalloc(dwMemContext: DWORD;
                       out malloc: IMalloc): HResult; stdcall;
  {$EXTERNALSYM CoGetMalloc}

  function CreateStreamOnHGlobal(_hglobal: HGLOBAL;
                                 fDeleteOnRelease: BOOL;
                                 out stm: IStream): HResult; stdcall;
  {$EXTERNALSYM CreateStreamOnHGlobal}

  function GetHGlobalFromStream(stm: IStream;
                                out hglob: HGlobal): HResult; stdcall;
  {$EXTERNALSYM GetHGlobalFromStream}

  //* init/uninit */

  procedure CoUninitialize(); stdcall;
  {$EXTERNALSYM CoUninitialize}

  function CoGetCurrentProcess(): DWORD; stdcall;
  {$EXTERNALSYM CoGetCurrentProcess}

  function CoInitializeEx(pvReserved: Pointer;
                          dwCoInit: LongInt): HResult; stdcall;
  {$EXTERNALSYM CoInitializeEx}

  function CoGetCallerTID(out lpdwTID: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoGetCallerTID}

  function CoGetCurrentLogicalThreadId(out pguid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoGetCurrentLogicalThreadId}

//#if (_WIN32_WINNT >= 0x0501)
  function CoGetContextToken(out pToken: ULONG_PTR): HResult; stdcall;
  {$EXTERNALSYM CoGetContextToken}

  function CoGetDefaultContext(aptType: APTTYPE;
                               const riid: TGUID;
                               out ppv: PPointer): HResult; stdcall;
  {$EXTERNALSYM CoGetDefaultContext}

  // definition for Win7 new APIs
  //#if (NTDDI_VERSION >= NTDDI_WIN7)

  function CoGetApartmentType(out pAptType: APTTYPE;
                              out pAptQualifier: APTTYPEQUALIFIER): HResult; stdcall;
  {$EXTERNALSYM CoGetApartmentType}

  // definition for Win8 new APIs
  //#if (NTDDI_VERSION >= NTDDI_WIN8)

type

  PServerInformation = ^tagServerInformation;
  tagServerInformation = record
    dwServerPid: DWORD;
    dwServerTid: DWORD;
    ui64ServerAddress: UINT64;
  end;
  {$EXTERNALSYM tagServerInformation}
  ServerInformation = tagServerInformation;
  {$EXTERNALSYM ServerInformation}


  function CoDecodeProxy(const dwClientPid: DWORD;
                         ui64ProxyAddress: UINT64;
                         out pServerInformation: ServerInformation): HResult; stdcall;
  {$EXTERNALSYM CoDecodeProxy}


type
  CO_MTA_USAGE_COOKIE = THandle;
  {$EXTERNALSYM CO_MTA_USAGE_COOKIE}

  function CoIncrementMTAUsage(out pCookie: CO_MTA_USAGE_COOKIE): HResult; stdcall;
  {$EXTERNALSYM CoIncrementMTAUsage}

  function CoAllowUnmarshalerCLSID(const clsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoAllowUnmarshalerCLSID}

  function CoGetObjectContext(const riid: TGUID;
                              out ppv): HResult; stdcall;
  {$EXTERNALSYM CoGetObjectContext}

  //* register/revoke/get class objects */

  function CoGetClassObject(const rclsid: TGUID;
                            dwClsContext: DWORD;
                            pvReserved: Pointer;
                            const riid: TGUID;
                            out ppv): HResult; stdcall;
  {$EXTERNALSYM CoGetClassObject}

  function CoRegisterClassObject(const rclsid: TGUID;
                                 pUnk: PUNKNOWN;
                                 dwClsContext: DWORD;
                                 flags: DWORD;
                                 out lpdwRegister: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoRegisterClassObject}

  function CoRevokeClassObject(dwRegister: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoRevokeClassObject}

  function CoResumeClassObjects(): HResult; stdcall;
  {$EXTERNALSYM CoResumeClassObjects}

  function CoSuspendClassObjects(): HResult; stdcall;
  {$EXTERNALSYM CoSuspendClassObjects}

  // Returns the current reference count.
  function CoAddRefServerProcess(): ULONG; stdcall;
  {$EXTERNALSYM CoAddRefServerProcess}

  function CoReleaseServerProcess(): HResult; stdcall;
  {$EXTERNALSYM CoReleaseServerProcess}

  function CoGetPSClsid(const riid: TGUID;
                        out pClsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoGetPSClsid}

  function CoRegisterPSClsid(const riid: TGUID;
                             const rclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoRegisterPSClsid}


  // Registering surrogate processes
  function CoRegisterSurrogate(pSurrogate: LPSURROGATE): HResult; stdcall;
  {$EXTERNALSYM CoRegisterSurrogate}


  //* marshaling interface pointers *//

  function CoGetMarshalSizeMax(out pulSize: ULONG;
                               const riid: TGUID;
                               const pUnk: IUnknown;
                               dwDestContext: DWORD;
                               pvDestContext: Pointer;
                               mshlflags: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoGetMarshalSizeMax}

  function CoMarshalInterface(stm: IStream;
                              const iid: TGUID;
                              punk: IUnknown;
                              dwDestContext: DWORD;
                              pvDestContext: Pointer;
                              mshlflags: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoMarshalInterface}

  function CoUnmarshalInterface(stm: IStream;
                                const iid: TGUID;
                                out ppv): HResult; stdcall;
  {$EXTERNALSYM CoUnmarshalInterface}

  function CoMarshalHresult(pstm: LPSTREAM;
                            _hresult: HResult): HResult; stdcall;
  {$EXTERNALSYM CoMarshalHresult}

  function CoUnmarshalHResult(pstm: IStream;
                              out phresult: HResult): HResult; stdcall;
  {$EXTERNALSYM CoUnmarshalHResult}

  function CoReleaseMarshalData(pstm: IStream): HResult; stdcall;
  {$EXTERNALSYM CoReleaseMarshalData}

  function CoDisconnectObject(punk: IUnknown; {LPUNKNOWN}
                              dwReserved: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoDisconnectObject}

  function CoLockObjectExternal(punk: IUnknown;
                                fLock: BOOL;
                                fLastUnlockReleases: BOOL): HResult; stdcall;
  {$EXTERNALSYM CoLockObjectExternal}

  function CoGetStandardMarshal(const iid: TGUID;
                                punk: IUnknown; {LPUNKNOWN}
                                dwDestContext: DWORD;
                                pvDestContext: Pointer;
                                mshlflags: DWORD;
                                out marshal: IMarshal): HResult; stdcall;
  {$EXTERNALSYM CoGetStandardMarshal}

  function CoGetStdMarshalEx(pUnkOuter: IUnknown; {LPUNKNOWN}
                             smexflags: DWORD;
                             out ppUnkInner: IUnknown {PLPUNKNOWN}): HResult; stdcall;
  {$EXTERNALSYM CoGetStdMarshalEx}

type
  //* flags for CoGetStdMarshalEx *//
  PSTDMSHLFLAGS = ^tagSTDMSHLFLAGS;
  tagSTDMSHLFLAGS = (
    SMEXF_SERVER  = $01,           // server side aggregated std marshaler
    SMEXF_HANDLER = $02            // client side (handler) agg std marshaler
  );
  {$EXTERNALSYM tagSTDMSHLFLAGS}
  STDMSHLFLAGS = tagSTDMSHLFLAGS;
  {$EXTERNALSYM STDMSHLFLAGS}

  function CoIsHandlerConnected(pUnk: IUnknown): BOOL; stdcall;
  {$EXTERNALSYM CoIsHandlerConnected}

  // Apartment model inter-thread interface passing helpers

  function CoMarshalInterThreadInterfaceInStream(const riid: TGUID;
                                                 pUnk: IUnknown;
                                                 out ppStm: IStream): HResult; stdcall;
  {$EXTERNALSYM CoMarshalInterThreadInterfaceInStream}

  function CoGetInterfaceAndReleaseStream(pStm: IStream;
                                          const iid: TGUID;
                                          out ppv): HResult; stdcall;
  {$EXTERNALSYM CoGetInterfaceAndReleaseStream}

  function CoCreateFreeThreadedMarshaler(punkOuter: IUnknown;
                                         out ppunkMarshal: IUnknown): HResult; stdcall;
  {$EXTERNALSYM CoCreateFreeThreadedMarshaler}


  procedure CoFreeUnusedLibraries(); stdcall;
  {$EXTERNALSYM CoFreeUnusedLibraries}

  procedure CoFreeUnusedLibrariesEx(dwUnloadDelay: DWORD;
                                    dwReserved: DWORD); stdcall;
  {$EXTERNALSYM CoFreeUnusedLibrariesEx}

  //#if (_WIN32_WINNT >= 0x0600)

  function CoDisconnectContext(dwTimeout: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoDisconnectContext}


  //* Call Security. *//
  function CoInitializeSecurity(pSecDesc: Pointer;
                                cAuthSvc: LONG;
                                asAuthSvc: PSOLE_AUTHENTICATION_SERVICE;
                                pReserved1: Pointer;
                                dwAuthnLevel: DWORD;
                                dImpLevel: DWORD;
                                pAuthList: Pointer;
                                dwCapabilities: DWORD;
                                pReserved3: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoInitializeSecurity}


  function CoGetCallContext(const riid: TGUID;
                            ppInterface: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoGetCallContext}

  function CoQueryProxyBlanket(Proxy: IUnknown;
                               pwAuthnSvc: PDWORD;
                               pAuthzSvc: PDWORD;
                               pServerPrincName: PPOLESTR;
                               pAuthnLevel: PDWORD;
                               pImpLevel: PDWORD;
                               pAuthInfo: Pointer;
                               pCapabilites: PDWORD): HResult; stdcall;
  {$EXTERNALSYM CoQueryProxyBlanket}

  function CoSetProxyBlanket(pProxy: IUnknown;
                             dwAuthnSvc: DWORD;
                             dwAuthzSvc: DWORD;
                             pServerPrincName: POLESTR;
                             dwAuthnLevel: DWORD;
                             dwImpLevel: DWORD;
                             pAuthInfo: Pointer;
                             dwCapabilites: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoSetProxyBlanket}

  function CoCopyProxy(pProxy: IUnknown;
                       out ppCopy: IUnknown): HResult; stdcall;
  {$EXTERNALSYM CoCopyProxy}

  function CoQueryClientBlanket(pwAuthnSvc: PDWORD;
                                pAuthzSvc: PDWORD;
                                pServerPrincName: PPOLESTR;
                                var dwAuthnLevel: DWORD;
                                dwImpLevel: DWORD;
                                pPrivs: Pointer;
                                var dwCapabilites: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoQueryClientBlanket}

  function CoImpersonateClient(): HResult; stdcall;
  {$EXTERNALSYM CoImpersonateClient}

  function CoRevertToSelf(): HResult; stdcall;
  {$EXTERNALSYM CoRevertToSelf}

  function CoQueryAuthenticationServices(pcAuthSvc: PDWORD;
                                         asAuthSvc: PSOLE_AUTHENTICATION_SERVICE): HResult; stdcall;
  {$EXTERNALSYM CoQueryAuthenticationServices}

  function CoSwitchCallContext(NewObject: IUnknown;
                               out pOldObject: IUnknown): HResult; stdcall;
  {$EXTERNALSYM CoSwitchCallContext}


const

  COM_RIGHTS_EXECUTE                  = 1;
  {$EXTERNALSYM COM_RIGHTS_EXECUTE}
  COM_RIGHTS_EXECUTE_LOCAL            = 2;
  {$EXTERNALSYM COM_RIGHTS_EXECUTE_LOCAL}
  COM_RIGHTS_EXECUTE_REMOTE           = 4;
  {$EXTERNALSYM COM_RIGHTS_EXECUTE_REMOTE}
  COM_RIGHTS_ACTIVATE_LOCAL           = 8;
  {$EXTERNALSYM COM_RIGHTS_ACTIVATE_LOCAL}
  COM_RIGHTS_ACTIVATE_REMOTE          = 16;
  {$EXTERNALSYM COM_RIGHTS_ACTIVATE_REMOTE}
  COM_RIGHTS_RESERVED1                = 32;
  {$EXTERNALSYM COM_RIGHTS_RESERVED1}
  COM_RIGHTS_RESERVED2                = 64;
  {$EXTERNALSYM COM_RIGHTS_RESERVED2}


  //* helper for creating instances *//

  function CoCreateInstance(const rclsid: TGUID;
                            unkOuter: IUnknown;
                            dwClsContext: Longint;
                            const riid: TGUID;
                            out ppv): HResult; stdcall;
  {$EXTERNALSYM CoCreateInstance}

  function CoCreateInstanceEx(const Clsid: TGUID;
                              punkOuter: IUnknown;
                              dwClsCtx: Longint;
                              pServerInfo: PCoServerInfo;
                              dwCount: Longint;
                              pResults: PMULTI_QI {An array of MULTI_QI structures}): HResult; stdcall;
  {$EXTERNALSYM CoCreateInstanceEx}

  // Registers a process-wide filter to process activation requests.
  function CoRegisterActivationFilter(pActivationFilter: IActivationFilter): HResult; stdcall;
  {$EXTERNALSYM CoRegisterActivationFilter}

  //#if (_WIN32_WINNT >= 0x0602)

  function CoCreateInstanceFromApp(const Clsid: TGUID;
                                   punkOuter: IUnknown;
                                   dwClsCtx: DWORD;
                                   reserved: Pointer;
                                   dwCount: DWORD;
                                   pResults: PMULTI_QI {An array of MULTI_QI structures}): HResult; stdcall;
  {$EXTERNALSYM CoCreateInstanceFromApp}




  //* Call related APIs *//
  // DCOM

  function CoGetCancelObject(dwThreadId: DWORD;
                             const iid: TGUID;
                             out ppUnk: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoGetCancelObject}

  function CoSetCancelObject(pUnk: IUnknown): HResult; stdcall;
  {$EXTERNALSYM CoSetCancelObject}

  function CoCancelCall(dwThreadId: DWORD;
                        ulTimeout: ULONG): HResult; stdcall;
  {$EXTERNALSYM CoCancelCall}

  function CoTestCancel(): HResult; stdcall;
  {$EXTERNALSYM CoTestCancel}

  function CoEnableCallCancellation(pReserved: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoEnableCallCancellation}

  function CoDisableCallCancellation(pReserved: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoDisableCallCancellation}


  //* other helpers *

  function StringFromCLSID(const clsid: TGUID;
                           out lpsz: POlESTR): HResult; stdcall;
  {$EXTERNALSYM StringFromCLSID}

  function CLSIDFromString(lpsz: LPWSTR; //POlESTR;
                           out pclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CLSIDFromString}

  function StringFromIID(const rclsid: TGUID;
                         out lplpsz: POlESTR): HResult; stdcall;
  {$EXTERNALSYM StringFromIID}

  function IIDFromString(lpsz: POlESTR;
                         out lpiid: TGUID): HResult; stdcall;
  {$EXTERNALSYM IIDFromString}


  function ProgIDFromCLSID(const clsid: TGUID;
                           out lplpszProgID: POlESTR): HResult; stdcall;
  {$EXTERNALSYM ProgIDFromCLSID}

  function CLSIDFromProgID(lpszProgID: POlESTR;
                           out lpclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CLSIDFromProgID}

  function StringFromGUID2(const rguid: TGUID;
                           lpsz: POleStr;
                           cchMax: Integer): Integer; stdcall;
  {$EXTERNALSYM StringFromGUID2}

  function CoCreateGuid(out pguid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoCreateGuid}


  //* Prop variant support *//

  // NOTE: These functions are also declared in PropIdl.pas !

  function PropVariantCopy(out pvarDest: PROPVARIANT;
                           const pvarSrc: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PropVariantCopy}

  function PropVariantClear(var pvar: PROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM PropVariantClear}

  function FreePropVariantArray(cVariants: ULONG;
                                rgvars: PPROPVARIANT): HResult; stdcall;
  {$EXTERNALSYM FreePropVariantArray}

  // NOTE: This function is also declared in PropVarUtil.pas!
  procedure PropVariantInit(var pvar: PROPVARIANT); inline;


  // Waits for specified handles to be signaled or for a specified timeout period to elapse.
  function CoWaitForMultipleHandles(dwFlags: DWORD;
                                    dwTimeout: DWORD;
                                    cHandles: ULONG;
                                    pHandles: PHANDLE;     // array of handles
                                    out lpdwindex: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoWaitForMultipleHandles}




  //* Flags for Synchronization API and Classes *//
type
  PCOWAIT_FLAGS = ^tagCOWAIT_FLAGS;
  tagCOWAIT_FLAGS = DWord;
  {$EXTERNALSYM tagCOWAIT_FLAGS}
  COWAIT_FLAGS = tagCOWAIT_FLAGS;
  {$EXTERNALSYM COWAIT_FLAGS}
const
  COWAIT_DEFAULT                  = COWAIT_FLAGS(0);
  {$EXTERNALSYM COWAIT_DEFAULT}
  COWAIT_WAITALL                  = COWAIT_FLAGS(1);
  {$EXTERNALSYM COWAIT_WAITALL}
  COWAIT_ALERTABLE                = COWAIT_FLAGS(2);
  {$EXTERNALSYM COWAIT_ALERTABLE}
  COWAIT_INPUTAVAILABLE           = COWAIT_FLAGS(4);
  {$EXTERNALSYM COWAIT_INPUTAVAILABLE}
  COWAIT_DISPATCH_CALLS           = COWAIT_FLAGS(8);
  {$EXTERNALSYM COWAIT_DISPATCH_CALLS}
  COWAIT_DISPATCH_WINDOW_MESSAGES = COWAIT_FLAGS($10);
  {$EXTERNALSYM COWAIT_DISPATCH_WINDOW_MESSAGES}

//#if (NTDDI_VERSION >= NTDDI_WIN8)
type
  PCWMO_FLAGS = ^CWMO_FLAGS;
  PCwmoFlags = ^TCwmoFlags;
  CWMO_FLAGS = DWord;
  {$EXTERNALSYM CWMO_FLAGS}
  TCwmoFlags = CWMO_FLAGS;
  {$EXTERNALSYM TCwmoFlags}
const
  CWMO_DEFAULT                  = CWMO_FLAGS(0);
  {$EXTERNALSYM CWMO_DEFAULT}
  CWMO_DISPATCH_CALLS           = CWMO_FLAGS(1);
  {$EXTERNALSYM CWMO_DISPATCH_CALLS}
  CWMO_DISPATCH_WINDOW_MESSAGES = CWMO_FLAGS(2);
  {$EXTERNALSYM CWMO_DISPATCH_WINDOW_MESSAGES}


  function CoWaitForMultipleObjects(dwFlags: DWORD;
                                    dwTimeout: DWORD;
                                    cHandles: ULONG;
                                    pHandles: PHandle; // pointer to array of handles
                                    out lpdwindex: DWORD): HResult; stdcall;
  {$EXTERNALSYM CoWaitForMultipleObjects}


//#endif // (NTDDI_VERSION >= NTDDI_WIN8)

const

  CWMO_MAX_HANDLES                    = 56;
  {$EXTERNALSYM CWMO_MAX_HANDLES}

  function CoGetTreatAsClass(const clsidOld: TGUID;
                             out clsidNew: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoGetTreatAsClass}

  function CoTreatAsClass(const clsidOld: TGUID;
                          const clsidNew: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoTreatAsClass}



  //* for flushing OLESCM remote binding handles *//
  //#if (_WIN32_WINNT >= 0x0501)

  function CoInvalidateRemoteMachineBindings(pszMachineName: POLESTR): HResult; stdcall;
  {$EXTERNALSYM CoInvalidateRemoteMachineBindings}

  //#if (NTDDI_VERSION >= NTDDI_WINBLUE)


type
  PAgileReferenceOptions = ^AgileReferenceOptions;
  AgileReferenceOptions = DWord;
  {$EXTERNALSYM AgileReferenceOptions}
const
  AGILEREFERENCE_DEFAULT        = AgileReferenceOptions(0);
  {$EXTERNALSYM AGILEREFERENCE_DEFAULT}
  AGILEREFERENCE_DELAYEDMARSHAL = AgileReferenceOptions(1);
  {$EXTERNALSYM AGILEREFERENCE_DELAYEDMARSHAL}


  function RoGetAgileReference(options: AgileReferenceOptions;
                               const riid: TGUID;
                               pUnk: IUnknown;
                               out ppAgileReference: IAgileReference): HResult; stdcall;
  {$EXTERNALSYM RoGetAgileReference}



type

  //* the server dlls must define their DllGetClassObject and DllCanUnloadNow
  //* to match these; the typedefs are located here to ensure all are changed at
  //* the same time.
  //*

  DLLGetClassObject = function(const rclsid: TGUID;
                               const riid: TGUID;
                               out ppv): HResult; stdcall;
  {$EXTERNALSYM DLLGetClassObject}

  DLLCanUnloadNow = function(): HResult; stdcall;
  {$EXTERNALSYM DLLCanUnloadNow}


  //****** Default Memory Allocation *****************************************//

  function CoTaskMemAlloc(cb: SIZE_T): Pointer; stdcall;
  {$EXTERNALSYM CoTaskMemAlloc}

  function CoTaskMemRealloc(ppv: Pointer;
                            cb: SIZE_T): Pointer; stdcall;
  {$EXTERNALSYM CoTaskMemRealloc}

  procedure CoTaskMemFree(ppv: Pointer); stdcall;
  {$EXTERNALSYM CoTaskMemFree}



  function CoFileTimeNow(out filetime: FILETIME): HResult; stdcall;
  {$EXTERNALSYM CoFileTimeNow}

  function CLSIDFromProgIDEx(lpszProgID: POLESTR;
                             out lpclsid: PGUID): HResult; stdcall;
  {$EXTERNALSYM CLSIDFromProgIDEx}

// NTDDI_VERSION >= NTDDI_WIN10_VB

{$IFNDEF _CO_DEVICE_CATALOG_}
{$DEFINE _CO_DEVICE_CATALOG_}
{$ENDIF}

type
  CO_DEVICE_CATALOG_COOKIE = THandle;
  {$EXTERNALSYM CO_DEVICE_CATALOG_COOKIE}

  function CoRegisterDeviceCatalog(deviceInstanceId: PWideChar;
                                   out cookie: CO_DEVICE_CATALOG_COOKIE): HResult; stdcall;
  {$EXTERNALSYM CoRegisterDeviceCatalog}

  function CoRevokeDeviceCatalog(cookie: CO_DEVICE_CATALOG_COOKIE): HResult; stdcall;
  {$EXTERNALSYM CoRevokeDeviceCatalog}

// end NTDDI_VERSION >= NTDDI_WIN10_VB


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  ole32Lib = 'ole32.dll';


procedure PropVariantInit(var pvar: PROPVARIANT); inline;
begin
  FillChar(pvar,
           sizeof(PROPVARIANT),
           0);
end;

{$WARN SYMBOL_PLATFORM OFF}

function CoGetMalloc;                   external Ole32Lib name 'CoGetMalloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateStreamOnHGlobal;         external Ole32Lib name 'CreateStreamOnHGlobal' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetHGlobalFromStream;          external Ole32Lib name 'GetHGlobalFromStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoUninitialize;               external Ole32Lib name 'CoUninitialize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetCurrentProcess;           external Ole32Lib name 'CoGetCurrentProcess' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoInitializeEx;                external Ole32Lib name 'CoInitializeEx' {delayed};
function CoGetCallerTID;                external Ole32Lib name 'CoGetCallerTID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetCurrentLogicalThreadId;   external Ole32Lib name 'CoGetCurrentLogicalThreadId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetContextToken;             external Ole32Lib name 'CoGetContextToken' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetDefaultContext;           external Ole32Lib name 'CoGetDefaultContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetApartmentType;            external Ole32Lib name 'CoGetApartmentType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoDecodeProxy;                 external Ole32Lib name 'CoDecodeProxy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoIncrementMTAUsage;           external Ole32Lib name 'CoIncrementMTAUsage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoAllowUnmarshalerCLSID;       external Ole32Lib name 'CoAllowUnmarshalerCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetObjectContext;            external Ole32Lib name 'CoGetObjectContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetClassObject;              external Ole32Lib name 'CoGetClassObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterClassObject;         external Ole32Lib name 'CoRegisterClassObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRevokeClassObject;           external Ole32Lib name 'CoRevokeClassObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoResumeClassObjects;          external Ole32Lib name 'CoResumeClassObjects' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoSuspendClassObjects;         external Ole32Lib name 'CoSuspendClassObjects' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoAddRefServerProcess;         external Ole32Lib name 'CoAddRefServerProcess' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoReleaseServerProcess;        external Ole32Lib name 'CoReleaseServerProcess' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetPSClsid;                  external Ole32Lib name 'CoGetPSClsid' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterPSClsid;             external Ole32Lib name 'CoRegisterPSClsid' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterSurrogate;           external Ole32Lib name 'CoRegisterSurrogate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetMarshalSizeMax;           external Ole32Lib name 'CoGetMarshalSizeMax' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoMarshalInterface;            external Ole32Lib name 'CoMarshalInterface' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoUnmarshalInterface;          external Ole32Lib name 'CoUnmarshalInterface' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoMarshalHResult;              external Ole32Lib name 'CoMarshalHResult' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoUnmarshalHResult;            external Ole32Lib name 'CoUnmarshalHResult' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoReleaseMarshalData;          external Ole32Lib name 'CoReleaseMarshalData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoDisconnectObject;            external Ole32Lib name 'CoDisconnectObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoLockObjectExternal;          external Ole32Lib name 'CoLockObjectExternal' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetStandardMarshal;          external Ole32Lib name 'CoGetStandardMarshal' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetStdMarshalEx;             external Ole32Lib name 'CoGetStdMarshalEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoIsHandlerConnected;          external Ole32Lib name 'CoIsHandlerConnected' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoMarshalInterThreadInterfaceInStream; external Ole32Lib name 'CoMarshalInterThreadInterfaceInStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetInterfaceAndReleaseStream; external Ole32Lib name 'CoGetInterfaceAndReleaseStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCreateFreeThreadedMarshaler; external Ole32Lib name 'CoCreateFreeThreadedMarshaler' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoFreeLibrary;                external Ole32Lib name 'CoFreeLibrary' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoFreeAllLibraries;           external Ole32Lib name 'CoFreeAllLibraries' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoFreeUnusedLibraries;        external Ole32Lib name 'CoFreeUnusedLibraries' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoFreeUnusedLibrariesEx;      external Ole32Lib name 'CoFreeUnusedLibrariesEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoDisconnectContext;           external Ole32Lib name 'CoDisconnectContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoInitializeSecurity;          external Ole32Lib name 'CoInitializeSecurity' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetCallContext;              external Ole32Lib name 'CoGetCallContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoQueryProxyBlanket;           external Ole32Lib name 'CoQueryProxyBlanket' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoSetProxyBlanket;             external Ole32Lib name 'CoSetProxyBlanket' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCopyProxy;                   external Ole32Lib name 'CoCopyProxy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoQueryClientBlanket;          external Ole32Lib name 'CoQueryClientBlanket' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoImpersonateClient;           external Ole32Lib name 'CoImpersonateClient' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRevertToSelf;                external Ole32Lib name 'CoRevertToSelf' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoQueryAuthenticationServices; external Ole32Lib name 'CoQueryAuthenticationServices' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoSwitchCallContext;           external Ole32Lib name 'CoSwitchCallContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCreateInstance;              external Ole32Lib name 'CoCreateInstance' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCreateInstanceEx;            external Ole32Lib name 'CoCreateInstanceEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCreateInstanceFromApp;       external Ole32Lib name 'CoCreateInstanceFromApp' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetCancelObject;             external Ole32Lib name 'CoGetCancelObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoSetCancelObject;             external Ole32Lib name 'CoSetCancelObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCancelCall;                  external Ole32Lib name 'CoCancelCall' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoTestCancel;                  external Ole32Lib name 'CoTestCancel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoEnableCallCancellation;      external Ole32Lib name 'CoEnableCallCancellation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoDisableCallCancellation;     external Ole32Lib name 'CoDisableCallCancellation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterActivationFilter;    external Ole32Lib name 'CoRegisterActivationFilter' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StringFromCLSID;               external Ole32Lib name 'StringFromCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CLSIDFromString;               external Ole32Lib name 'CLSIDFromString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StringFromIID;                 external Ole32Lib name 'StringFromIID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IIDFromString;                 external Ole32Lib name 'IIDFromString' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function ProgIDFromCLSID;               external Ole32Lib name 'ProgIDFromCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CLSIDFromProgID;               external Ole32Lib name 'CLSIDFromProgID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StringFromGUID2;               external Ole32Lib name 'StringFromGUID2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCreateGuid;                  external Ole32Lib name 'CoCreateGuid' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantCopy;               external Ole32Lib name 'PropVariantCopy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropVariantClear;              external Ole32Lib name 'PropVariantClear' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function FreePropVariantArray;          external Ole32Lib name 'FreePropVariantArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoWaitForMultipleHandles;      external Ole32Lib name 'CoWaitForMultipleHandles' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoWaitForMultipleObjects;      external Ole32Lib name 'CoWaitForMultipleObjects' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoInvalidateRemoteMachineBindings; external Ole32Lib name 'CoInvalidateRemoteMachineBindings' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function RoGetAgileReference;           external Ole32Lib name 'RoGetAgileReference' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoFileTimeNow;                 external Ole32Lib name 'CoFileTimeNow' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CLSIDFromProgIDEx;             external Ole32Lib name 'CLSIDFromProgIDEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetTreatAsClass;             external Ole32Lib name 'CoGetTreatAsClass' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoTreatAsClass;                external Ole32Lib name 'CoTreatAsClass' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoTaskMemAlloc;                external Ole32Lib name 'CoTaskMemAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoTaskMemRealloc;              external Ole32Lib name 'CoTaskMemRealloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoTaskMemFree;                external Ole32Lib name 'CoTaskMemFree' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function CoRegisterDeviceCatalog;       external Ole32Lib name 'CoRegisterDeviceCatalog' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRevokeDeviceCatalog;         external Ole32Lib name 'CoRevokeDeviceCatalog' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}


  // Implement Additional functions here.


end.

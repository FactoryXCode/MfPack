// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfContentDecryptionModule.pas
// Kind: Pascal / Delphi unit
// Release date: 28-07-2020
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Definitions for kernel mode code OPM communication.
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
// Remarks: Minimum supported OS: Windows 10, version 2004 (10.0; Build 19041)
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
// Source: mfcontentdecryptionmodule.h
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
unit WinApi.MediaFoundationApi.MfContentDecryptionModule;

  {$HPPEMIT '#include "MfContentDecryptionModule.h"'}
  {$HPPEMIT '#include "mfmediaengine.h"'}

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.PropSys,
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.PropSys,
  {$ENDIF}
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMediaEngine;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

  ////////////////////////////////////////////////////////////////////////////////////////////////////
  //  Definitions for access to the Content Decryption Module (CDM) for encrypted media extensions support.
  //

  // IMFContentDecryptionModuleSession is designed based on EME MediaKeySession:
  // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession

type

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentDecryptionModuleSession);'}
  {$EXTERNALSYM IMFContentDecryptionModuleSession}
  IMFContentDecryptionModuleSession = interface(IUnknown)
  ['{4e233efd-1dd2-49e8-b577-d63eee4c0d33}']

    // A unique string identifier generated by the CDM that can be used by the application
    // to identify session objects.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-sessionid
    function GetSessionId(out sessionId: LPWSTR): HResult; stdcall;

    // The expiration time for all key(s) in the session, or NaN if no such time exists or
    // if the license explicitly never expires, as determined by the CDM.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-expiration
    // expiration is based on https://tc39.es/ecma262/#sec-time-values-and-time-range
    function GetExpiration(out expiration: Double): HResult; stdcall;

    // A reference to an array of key IDs known to the session to the current status
    // of the associated key. Each entry MUST have a unique key ID.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-keystatuses
    function GetKeyStatuses(out keyStatuses: MFMediaKeyStatus;
                            out numKeyStatuses: UINT): HResult; stdcall;

    // Loads the data stored for the specified session into this object.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-load
    function Load(sessionId: LPWSTR;
                  out loaded: BOOL): HResult; stdcall;

    // Generates a license request based on the initData. A message of type "license-request" or
    // "individualization-request" will always be queued if the algorithm succeeds
    // and the promise is resolved.
    // Structure for init data is based on initDataType
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-generaterequest
    // https://www.w3.org/TR/eme-initdata-registry/
    function GenerateRequest(initDataType: LPWSTR;
                             initData: PByte;
                             initDataSize: DWORD): HResult; stdcall;

    // Provides messages, including licenses, to the CDM.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-update
    function Update(response: PByte;
                    responseSize: DWORD): HResult; stdcall;

    // Indicates that the application no longer needs the session and the CDM should release any resources
    // associated with the session and close it. Persisted data should not be released or cleared.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-close
    function Close(): HResult; stdcall;

    // Removes all license(s) and key(s) associated with the session. For persistent session types,
    // other session data will be cleared as defined for each session type once a
    // release message acknowledgment is processed by Update().
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysession-remove
    function Remove(): HResult; stdcall;

  end;
  IID_IMFContentDecryptionModuleSession = IMFContentDecryptionModuleSession;
  {$EXTERNALSYM IID_IMFContentDecryptionModuleSession}


  // IMFContentDecryptionModuleSessionCallbacks is designed based on EME keystatuseschange event:
  // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-evt-keystatuseschange
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentDecryptionModuleSessionCallbacks);'}
  {$EXTERNALSYM IMFContentDecryptionModuleSessionCallbacks}
  IMFContentDecryptionModuleSessionCallbacks = interface(IUnknown)
  ['{3f96ee40-ad81-4096-8470-59a4b770f89a}']

     // The CDM has generated a message for the session.
    function KeyMessage(messageType: MF_MEDIAKEYSESSION_MESSAGETYPE;
                        _message: PByte;
                        messageSize: DWORD;
                        {in, optional} destinationURL: LPWSTR): HResult; stdcall;

    // There has been a change in the keys in the session or their status.
    function KeyStatusChanged(): HResult; stdcall;

  end;
  IID_IMFContentDecryptionModuleSessionCallbacks = IMFContentDecryptionModuleSessionCallbacks;
  {$EXTERNALSYM IID_IMFContentDecryptionModuleSessionCallbacks}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentDecryptionModule);'}
  {$EXTERNALSYM IMFContentDecryptionModule}
  IMFContentDecryptionModule = interface(IUnknown)
  ['{87be986c-10be-4943-bf48-4b54ce1983a2}']

    // This method allows the caller to specify the IMFContentEnabler interface that shall be used
    // by the Content Decryption Module.
    // The IMFContentEnabler is normally obtained from IMFInputTrustAuthority::RequestAccess.
    function SetContentEnabler(contentEnabler: IMFContentEnabler;
                               result: IMFAsyncResult): HResult; stdcall;

    // Provides an object for IMFContentDecryptionModuleSession suspend events.
    function GetSuspendNotify(out notify: IMFCdmSuspendNotify): HResult; stdcall;

    // This method allows the caller to specify the IMFPMPHostApp interface, which represents
    // a protected process. The IMFPMPHostApp interface is used by the CDM to create the
    // IMFTrustedInput object.
    function SetPMPHostApp(pmpHostApp: IMFPMPHostApp): HResult; stdcall;

    // Creates an object based on the EME spec MediaKeySession object:
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeys-createsession
    function CreateSession(sessionType: MF_MEDIAKEYSESSION_TYPE;
                           callbacks: IMFContentDecryptionModuleSessionCallbacks;
                           out session: IMFContentDecryptionModuleSession): HResult; stdcall;

    // Provides a server certificate to be used to encrypt messages to the license server.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeys-setservercertificate
    function SetServerCertificate(certificate: PByte;
                                  certificateSize: DWORD): HResult; stdcall;

    // Creates an IMFTrustedInput object that implements the decryption of content.
    // ContentInitData will only be used if initData from
    // IMFContentDecryptionModuleSession.GenerateRequest is not provided or incomplete.
    // Initialization Data should be structured in PSSH Box Format. For more details, see
    // https://www.w3.org/TR/eme-initdata-cenc/#common-system
    function CreateTrustedInput(contentInitData: PByte;
                                contentInitDataSize: DWORD;
                                out trustedInput: IMFTrustedInput): HResult; stdcall;

    // Identifies the SystemIDs that this object supports.
    // SystemIDs are identifiers used in the "cenc" Initialization Data Format. For more details, see
    // https://w3c.github.io/encrypted-media/format-registry/initdata/cenc.html
    // systemIds should be allocated and freed using CoTaskMem.
    function GetProtectionSystemIds(out systemIds: PGUID;
                                    out count: PDWORD): HResult; stdcall;

  end;
  IID_IMFContentDecryptionModule = IMFContentDecryptionModule;
  {$EXTERNALSYM IID_IMFContentDecryptionModule}


  // IMFContentDecryptionModuleAccess is designed based on MediaKeySystemAccess:
  // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#mediakeysystemaccess-interface

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentDecryptionModuleAccess);'}
  {$EXTERNALSYM IMFContentDecryptionModuleAccess}
  IMFContentDecryptionModuleAccess = interface(IUnknown)
  ['{a853d1f4-e2a0-4303-9edc-f1a68ee43136}']

    // Creates an object based on the EME spec MediaKeys object:
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#mediakeys-interface
    // Supported contentDecryptionModuleProperties are detailed below.
    function CreateContentDecryptionModule(contentDecryptionModuleProperties: IPropertyStore;
                                           out contentDecryptionModule: IMFContentDecryptionModule): HResult; stdcall;

    // Returns the supported combination of configuration options.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysystemaccess-getconfiguration
    function GetConfiguration(out configuration: IPropertyStore): HResult; stdcall;

    // Identifies the Key System being used.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#dom-mediakeysystemaccess-keysystem
    // String should be allocated and freed using CoTaskMem.
    function GetKeySystem(out keySystem: LPWSTR): HResult; stdcall;

  end;
  IID_IMFContentDecryptionModuleAccess = IMFContentDecryptionModuleAccess;
  {$EXTERNALSYM IID_IMFContentDecryptionModuleAccess}


const

  // Property keys that can be passed to CreateContentDecryptionModule

  // This Property is a LPWSTR that represents a file path.
  // Use this property to pass a storage location the CDM can use for content specific data.
  // The app should delete the store location after the CDM object has been released.
  MF_CONTENTDECRYPTIONMODULE_INPRIVATESTOREPATH  :	PROPERTYKEY = (fmtid: (D1: $730cb3ac;
                                                                           D2: $51dc;
                                                                           D3: $49da;
                                                                           D4: ($a5, $78, $b9, $53, $86, $b6, $2a, $fe));
                                                                           Pid: $01);

  // This Property is a LPWSTR that represents a file path.
  // Use this property to pass a storage location the CDM can use for initialization.
  // This storage location will also be used for content specific data if INPRIVATESTOREPATH is not set.
  // The app should not delete the store location to optimize performance of the CDM.
  MF_CONTENTDECRYPTIONMODULE_STOREPATH            :	PROPERTYKEY = (fmtid: (D1: $77d993b9;
                                                                           D2: $ba61;
                                                                           D3: $4bb7;
                                                                           D4: ($92, $c6, $18, $c8, $6a, $18, $9c, $06));
                                                                           Pid: $02);

  // This Property is a LPWSTR used in the Store Content Decryption Module scenario.
  // The CDM implementer should look for this property and pass to the
  // MediaProtectionPMPServer constructor as "Windows.Media.Protection.PMPStoreContext" Property.
  // Users calling the CreateContentDecryptionModule should not create this property.
  MF_CONTENTDECRYPTIONMODULE_PMPSTORECONTEXT      :	PROPERTYKEY = (fmtid: (D1: $6d2a2835;
                                                                           D2: $c3a9;
                                                                           D3: $4681;
                                                                           D4: ($97, $f2, $0a, $f5, $6b, $e9, $34, $46));
                                                                           Pid: $03);

type

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFContentDecryptionModuleFactory);'}
  {$EXTERNALSYM IMFContentDecryptionModuleFactory}
  IMFContentDecryptionModuleFactory = interface(IUnknown)
  ['{7d5abf16-4cbb-4e08-b977-9ba59049943e}']

    // Identifies what Key Systems are be supported.
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#key-system
    function IsTypeSupported(keySystem: LPCWSTR;
                            {in, optional} contentType: LPCWSTR): BOOL; stdcall;

    // Creates an object based on the EME spec MediaKeySystemAccess object:
    // https://www.w3.org/TR/2017/REC-encrypted-media-20170918/#mediakeysystemaccess-interface
    // See IMFMediaKeySystemAccess.CreateMediaKeys for configuration information.
    function CreateContentDecryptionModuleAccess(keySystem: LPCWSTR;
                                                 configurations: IPropertyStore;
                                                 numConfigurations: DWORD;
                                                 out contentDecryptionModuleAccess: IMFContentDecryptionModuleAccess): HResult; stdcall;

  end;
  IID_IMFContentDecryptionModuleFactory = IMFContentDecryptionModuleFactory;
  {$EXTERNALSYM IID_IMFContentDecryptionModuleFactory}


const

  //
  // MF_CONTENTDECRYPTIONMODULE_SERVICE
  // Service GUID used to obtain interfaces from an IMFContentDecryptionModule implementation, e.g.,
  // the IMediaProtectionPMPServer interface. An implementation of IMFContentDecryptionModule
  // should implement IMFGetService and support this service GUID.
  //
  MF_CONTENTDECRYPTIONMODULE_SERVICE: TGUID = '{15320C45-FF80-484A-9DCB-0DF894E69A13}';


  /// <summary>
  ///     Creates an EncryptedMediaExtensionsStoreActivate for StoreContentDecryptionModule scenarios.
  ///     The activate can be created in the protected process and activated in the app process.
  /// </summary>
  /// <param name="pmpHost">
  ///     The IMFPMPHostApp with the necessary information to
  ///     create an EncryptedMediaExtensions Store Object for this app package.
  /// </param>
  /// <param name="objectStream">
  ///     The object stream that will be loaded via IMFActivate::Load.
  /// </param>
  /// <param name="classId">
  ///     String representing the target object's activatable class id.
  /// </param>
  /// <param name="activate">
  ///     Returns pointer to an EncryptedMediaExtensions Store Activate
  /// </param>
  function MFCreateEncryptedMediaExtensionsStoreActivate(pmpHost: IMFPMPHostApp;
                                                         objectStream: IStream;
                                                         classId: LPCWSTR;
                                                         {Outptr} out activate: IMFActivate): HResult; stdcall;

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  MfContentDecryptionModuleLib  = 'mf.dll';

  {$WARN SYMBOL_PLATFORM OFF}
  function MFCreateEncryptedMediaExtensionsStoreActivate; external MfContentDecryptionModuleLib name 'MFCreateEncryptedMediaExtensionsStoreActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  {$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.

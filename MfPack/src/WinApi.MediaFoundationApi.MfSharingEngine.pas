// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfSharingEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 30-05-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (topPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
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
// Source: mfsharingengine.h
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
unit WinApi.MediaFoundationApi.MfSharingEngine;

  {$HPPEMIT '#include "mfsharingengine.h"'}

interface

uses

  {WinApi}
  WinApi.WinApiTypes,
  WinApi.Inspectable,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.Mfobjects,
  WinApi.MediaFoundationApi.MFMediaEngine;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  //  Structure describing a media device
  PDEVICE_INFO = ^DEVICE_INFO;
  DEVICE_INFO = record
    pFriendlyDeviceName: BSTR;
    pUniqueDeviceName: BSTR;
    pManufacturerName: BSTR;
    pModelName: BSTR;
    pIconURL: BSTR;
  end;
  {$EXTERNALSYM DEVICE_INFO}


  //  Events fired by the <video>/<audio>/<img> elements
  //  Extends MF_MEDIA_ENGINE_EVENT
  PMF_SHARING_ENGINE_EVENT = ^MF_SHARING_ENGINE_EVENT;
  MF_SHARING_ENGINE_EVENT                         = (
    MF_SHARING_ENGINE_EVENT_DISCONNECT            = 2000,
    MF_SHARING_ENGINE_EVENT_LOCALRENDERINGSTARTED = 2001,
    MF_SHARING_ENGINE_EVENT_LOCALRENDERINGENDED   = 2002,
    MF_SHARING_ENGINE_EVENT_STOPPED               = 2003,
    MF_SHARING_ENGINE_EVENT_ERROR                 = 2501
  );
  {$EXTERNALSYM MF_SHARING_ENGINE_EVENT}


  //  Events fired by the <video>/<audio> elements
  //  Extends MF_MEDIA_ENGINE_EVENT
  PMF_MEDIA_SHARING_ENGINE_EVENT = ^MF_MEDIA_SHARING_ENGINE_EVENT;
  MF_MEDIA_SHARING_ENGINE_EVENT              = (
    MF_MEDIA_SHARING_ENGINE_EVENT_DISCONNECT = 2000
  );
  {$EXTERNALSYM MF_MEDIA_SHARING_ENGINE_EVENT}


const

  // MFMediaSharingEngine creation attributes
  // ========================================

  // MF_MEDIA_SHARING_ENGINE_DEVICE_NAME
  // Data type: STRING
  // {771E05D1-862F-4299-95AC-AE81FD14F3E7}
  MF_MEDIA_SHARING_ENGINE_DEVICE_NAME       : TGUID = '{771E05D1-862F-4299-95AC-AE81FD14F3E7}';
  {$EXTERNALSYM MF_MEDIA_SHARING_ENGINE_DEVICE_NAME}

  // MF_MEDIA_SHARING_ENGINE_DEVICE
  // Data type: IUnknown
  // {B461C58A-7A08-4B98-99A8-70FD5F3BADFD}
  MF_MEDIA_SHARING_ENGINE_DEVICE            : TGUID = '{B461C58A-7A08-4B98-99A8-70FD5F3BADFD}';
  {$EXTERNALSYM MF_MEDIA_SHARING_ENGINE_DEVICE}

  // MF_MEDIA_SHARING_ENGINE_INITIAL_SEEK_TIME
  // Data type: DOUBLE
  // {6F3497F5-D528-4A4F-8DD7-DB36657EC4C9}
  MF_MEDIA_SHARING_ENGINE_INITIAL_SEEK_TIME : TGUID = '{6F3497F5-D528-4A4F-8DD7-DB36657EC4C9}';
  {$EXTERNALSYM MF_MEDIA_SHARING_ENGINE_INITIAL_SEEK_TIME}

  // MF_SHUTDOWN_RENDERER_ON_ENGINE_SHUTDOWN
  // Data type: UINT32
  // {C112D94D-6B9C-48f8-B6F9-7950FF9AB71E}
  MF_SHUTDOWN_RENDERER_ON_ENGINE_SHUTDOWN   : TGUID = '{C112D94D-6B9C-48f8-B6F9-7950FF9AB71E}';
  {$EXTERNALSYM MF_SHUTDOWN_RENDERER_ON_ENGINE_SHUTDOWN}


  // MF_PREFERRED_SOURCE_URI
  // Data type: PCWSTR
  // {5FC85488-436A-4DB8-90AF-4DB402AE5C57}
  MF_PREFERRED_SOURCE_URI                   : TGUID = '{5FC85488-436A-4DB8-90AF-4DB402AE5C57}';
  {$EXTERNALSYM MF_PREFERRED_SOURCE_URI}


  // MF_SHARING_ENGINE_SHAREDRENDERER
  // Data type: IUnknown
  // {EFA446A0-73E7-404E-8AE2-FEF60AF5A32B}
  MF_SHARING_ENGINE_SHAREDRENDERER          : TGUID = '{EFA446A0-73E7-404E-8AE2-FEF60AF5A32B}';
  {$EXTERNALSYM MF_SHARING_ENGINE_SHAREDRENDERER}


  // MF_SHARING_ENGINE_CALLBACK
  // Data type: IUnknown
  // {57dc1e95-d252-43fa-9bbc-180070eefe6d}
  MF_SHARING_ENGINE_CALLBACK                : TGUID = '{57dc1e95-d252-43fa-9bbc-180070eefe6d}';
  {$EXTERNALSYM MF_SHARING_ENGINE_CALLBACK}

  // CLSID_MFMediaSharingEngineClassFactory
  // Data type: GUID
  // CLSID for creating the Media Sharing Engine
  //
  // {F8E307FB-6D45-4AD3-9993-66CD5A529659}
  CLSID_MFMediaSharingEngineClassFactory    : TGUID = '{F8E307FB-6D45-4AD3-9993-66CD5A529659}';
  {$EXTERNALSYM CLSID_MFMediaSharingEngineClassFactory}

  // CLSID_MFImageSharingEngineClassFactory
  // Data type: GUID
  // CLSID for creating the Image Sharing Engine
  //
  // {B22C3339-87F3-4059-A0C5-037AA9707EAF}
  CLSID_MFImageSharingEngineClassFactory    : TGUID = '{B22C3339-87F3-4059-A0C5-037AA9707EAF}';
  {$EXTERNALSYM CLSID_MFImageSharingEngineClassFactory}

  // CLSID_PlayToSourceClassFactory
  // Data type: GUID
  // CLSID for creating a PlayToSource object
  //
  // {DA17539A-3DC3-42C1-A749-A183B51F085E}
  CLSID_PlayToSourceClassFactory            : TGUID = '{DA17539A-3DC3-42C1-A749-A183B51F085E}';
  {$EXTERNALSYM CLSID_PlayToSourceClassFactory}

  // GUID_PlayToService
  // Data type: GUID
  // GUID identifying the PlayToService. This is used when trying to access PlayTo interfaces through the IServiceProvider interface
  //
  // {f6a8ff9d-9e14-41c9-bf0f-120a2b3ce120}
  GUID_PlayToService                        : TGUID = '{f6a8ff9d-9e14-41c9-bf0f-120a2b3ce120}';
  {$EXTERNALSYM GUID_PlayToService}

  // GUID_NativeDeviceService
  // Data type: GUID
  // GUID identifying the NativeDeviceService. If the user has a pointer to just an IBasicDevice, it can be QI'd for IServiceProvider.
  // IServiceProvider->QueryService() can be used with GUID_NativeDeviceService to get native interfaces for the device.
  // For example: You can retrieve a IUPnPDevice pointer as follows: pBasicDevice->QueryService( GUID_NativeDeviceService, IID_IUPnPDevice, (void **)&spUPnPDevice );
  //
  // {ef71e53c-52f4-43c5-b86a-ad6cb216a61e}
  GUID_NativeDeviceService                  : TGUID = '{ef71e53c-52f4-43c5-b86a-ad6cb216a61e}';
  {$EXTERNALSYM GUID_NativeDeviceService}



  // PLAYTO_SOURCE_CREATEFLAGS
  //
  ///////////////////////////////////////////////////////////////////////////////
type
  PPLAYTO_SOURCE_CREATEFLAGS = ^PLAYTO_SOURCE_CREATEFLAGS;
  PLAYTO_SOURCE_CREATEFLAGS = DWord;
  {$EXTERNALSYM PLAYTO_SOURCE_CREATEFLAGS}
const
  PLAYTO_SOURCE_NONE      = PLAYTO_SOURCE_CREATEFLAGS($0);
  {$EXTERNALSYM PLAYTO_SOURCE_NONE}
  PLAYTO_SOURCE_IMAGE     = PLAYTO_SOURCE_CREATEFLAGS($1);
  {$EXTERNALSYM PLAYTO_SOURCE_AUDIO}
  PLAYTO_SOURCE_AUDIO     = PLAYTO_SOURCE_CREATEFLAGS($2);
  {$EXTERNALSYM PLAYTO_SOURCE_IMAGE}
  PLAYTO_SOURCE_VIDEO     = PLAYTO_SOURCE_CREATEFLAGS($4);
  {$EXTERNALSYM PLAYTO_SOURCE_VIDEO}
// (WINVER >= _WIN32_WINNT_WINBLUE)
  PLAYTO_SOURCE_PROTECTED = PLAYTO_SOURCE_CREATEFLAGS($8);
  {$EXTERNALSYM PLAYTO_SOURCE_PROTECTED}
// end (WINVER >= _WIN32_WINNT_WINBLUE)


type

  // Interfaces
  // ==========


  // IMFSharingEngineClassFactory interface
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSharingEngineClassFactory);'}
  {$EXTERNALSYM IMFSharingEngineClassFactory}
  IMFSharingEngineClassFactory = interface(IUnknown)
  ['{2BA61F92-8305-413B-9733-FAF15F259384}']

    function CreateInstance(dwFlags: DWORD;    // see MF_MEDIA_ENGINE_CREATEFLAGS
                            pAttr: IMFAttributes;
                            out ppEngine: IUnknown): HRESULT; stdcall;  // Implements either IMFMediaEngineEx, IMFMediaSharingEngine,
                                                                        // or IMFImageSharingEngine based on flags passed to IPlayToConnectionClassFactory
  end;
  IID_IMFSharingEngineClassFactory = IMFSharingEngineClassFactory;
  {$EXTERNALSYM IID_IMFSharingEngineClassFactory}


  // IMFMediaSharingEngine interface
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSharingEngine);'}
  {$EXTERNALSYM IMFMediaSharingEngine}
  IMFMediaSharingEngine = interface(IMFMediaEngine)
  ['{8D3CE1BF-2367-40E0-9EEE-40D377CC1B46}']

    function GetDevice(out pDevice: DEVICE_INFO): HRESULT; stdcall;

  end;
  IID_IMFMediaSharingEngine = IMFMediaSharingEngine;
  {$EXTERNALSYM IID_IMFMediaSharingEngine}


  // IMFMediaSharingEngineClassFactory interface
  // ===========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaSharingEngineClassFactory);'}
  {$EXTERNALSYM IMFMediaSharingEngineClassFactory}
  IMFMediaSharingEngineClassFactory = interface(IUnknown)
  ['{524D2BC4-B2B1-4FE5-8FAC-FA4E4512B4E0}']

    function CreateInstance(dwFlags: DWORD;
                            pAttr: IMFAttributes;
                            out ppEngine: IMFMediaSharingEngine): HRESULT; stdcall;

  end;
  IID_IMFMediaSharingEngineClassFactory = IMFMediaSharingEngineClassFactory;
  {$EXTERNALSYM IID_IMFMediaSharingEngineClassFactory}


  // IMFImageSharingEngine interface
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFImageSharingEngine);'}
  {$EXTERNALSYM IMFImageSharingEngine}
  IMFImageSharingEngine = interface(IUnknown)
  ['{CFA0AE8E-7E1C-44D2-AE68-FC4C148A6354}']

    function SetSource(pStream: IUnknown): HRESULT; stdcall;

    function GetDevice(out pDevice: DEVICE_INFO): HRESULT; stdcall;

    function Shutdown(): HRESULT; stdcall;

  end;
  IID_IMFImageSharingEngine = IMFImageSharingEngine;
  {$EXTERNALSYM IID_IMFImageSharingEngine}


  // IMFImageSharingEngineClassFactory interface
  // ===========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFImageSharingEngineClassFactory);'}
  {$EXTERNALSYM IMFImageSharingEngineClassFactory}
  IMFImageSharingEngineClassFactory = interface(IUnknown)
  ['{1FC55727-A7FB-4FC8-83AE-8AF024990AF1}']

    function CreateInstanceFromUDN(const pUniqueDeviceName: BSTR;
                                   out ppEngine: IMFImageSharingEngine): HRESULT; stdcall;

  end;
  IID_IMFImageSharingEngineClassFactory = IMFImageSharingEngineClassFactory;
  {$EXTERNALSYM IID_IMFImageSharingEngineClassFactory}



  // IPlayToControl interface
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPlayToControl);'}
  {$EXTERNALSYM IPlayToControl}
  IPlayToControl = interface(IUnknown)
  ['{607574EB-F4B6-45C1-B08C-CB715122901D}']

    function Connect(pFactory: IMFSharingEngineClassFactory): HRESULT; stdcall;

  end;
  IID_IPlayToControl = IPlayToControl;
  {$EXTERNALSYM IID_IPlayToControl}


  // IPlayToControlWithCapabilities Interface
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPlayToControlWithCapabilities);'}
  {$EXTERNALSYM IPlayToControlWithCapabilities}
  IPlayToControlWithCapabilities = interface(IPlayToControl)
  ['{AA9DD80F-C50A-4220-91C1-332287F82A34}']

    function GetCapabilities(out pCapabilities: PPLAYTO_SOURCE_CREATEFLAGS): HRESULT; stdcall;

  end;
  IID_IPlayToControlWithCapabilities = IPlayToControlWithCapabilities;
  {$EXTERNALSYM IID_IPlayToControlWithCapabilities}


  // IPlayToSourceClassFactory Interface
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPlayToSourceClassFactory);'}
  {$EXTERNALSYM IPlayToSourceClassFactory}
  IPlayToSourceClassFactory = interface(IPlayToControl)
  ['{842B32A3-9B9B-4D1C-B3F3-49193248A554}']

     function CreateInstance(dwFlags: DWORD;                               // see PLAYTO_SOURCE_CREATEFLAGS
                             pControl: IPlayToControl;                     // element which controls the media
                             out ppSource: IInspectable): HRESULT; stdcall;// implements Windows.Media.PlayTo.PlayToSource

  end;
  IID_IPlayToSourceClassFactory = IPlayToSourceClassFactory;
  {$EXTERNALSYM IID_IPlayToSourceClassFactory}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

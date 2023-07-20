// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfVirtualCamera.pas
// Kind: Pascal / Delphi unit
// Release date: 12-10-2015
// Language: ENU
//
// Revision Version: 3.1.5
// Description:
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Build 22000 or later.
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: mfvirtualcamera.h
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
//
//==============================================================================
unit WinApi.MediaFoundationApi.MfVirtualCamera;


interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.DevPropDef,
  {ActiveX}
  WinApi.ActiveX,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  // DEVPROPKEY set on all virtual cameras created through
  // MFCreateVirtuaCamera API.  This property will be set
  // on the device interface.
  //
  // Type:  DEVPROP_BOOLEAN
  //
  // Remarks:  For all virtual cameras this property will
  // be set to DEVPROP_TRUE.  If this property is missing
  // applications must assume the camera is a non-virtual
  // camera.
  DEVPKEY_DeviceInterface_IsVirtualCamera                  :   PROPERTYKEY  = (fmtid: (D1: $6EDC630D;
                                                                                       D2: $C2E3;
                                                                                       D3: $43B7;
                                                                                       D4: ($B2, $D1, $20, $52, $5A, $1A, $F1, $20));
                                                                                       Pid: 3);
  {$EXTERNALSYM DEVPKEY_DeviceInterface_IsVirtualCamera}



{if NTDDI_VERSION >= NTDDI_WIN10_NI}

  // DEVPROPKEY set on a camera interface property store
  // indicating the camera can enable MS Camera Effects.
  //
  // Type:  DEVPROP_BOOLEAN
  // {6EDC630D-C2E3-43B7-B2D1-20525A1AF120}, 4")
  DEVPKEY_DeviceInterface_IsWindowsCameraEffectAvailable     :  PROPERTYKEY = (fmtid: (D1: $6EDC630D;
                                                                                       D2: $C2E3;
                                                                                       D3: $43B7;
                                                                                       D4: ($B2, $D1, $20, $52, $5A, $1A, $F1, $20));
                                                                                       Pid: 4);

  // 28A5531A-57DD-4FD5-AAA7-385ABF57D785
  //
  // Type:  UINT32
  //
  // Attribute set during camera creation.  If set to non-zero
  // value camera effects will be enabled.  If the camera being
  // created does not have
  // DEVPKEY_DeviceInterface_IsWindowsCameraEffectAvailable
  // property set to true, setting this attribute during camera
  // initialization will result in an MF_E_INVALIDREQUEST
  // failure.
  MF_DEVSOURCE_ATTRIBUTE_ENABLE_MS_CAMERA_EFFECTS            : TGUID = '{28A5531A-57DD-4FD5-AAA7-385ABF57D785}';

  // {1BB79E7C-5D83-438C-94D8-E5F0DF6D3279}
  //
  // Type:  IUnknown
  //
  // IMFCollection object containing the IMFMediaSourceEx of the
  // various associated physical cameras for the virtual camera.
  // This attribute will be set on the IMFActivate's attribute
  // prior to calling IMFActivate::ActivateObject() method.
  MF_VIRTUALCAMERA_ASSOCIATED_CAMERA_SOURCES                 : TGUID = '{1BB79E7C-5D83-438C-94D8-E5F0DF6D3279}';

  // {F0273718-4A4D-4AC5-A15D-305EB5E90667}
  //
  // Type:  UINT32
  //
  // If present and non-zero, the virtual camera custom media
  // source is requesting the pipeline to provide the associated
  // physical camera sources.  If this attribute is not present
  // or set to 0, associated physical cameras will not be
  // provided through the
  // MF_VIRTUALCAMERA_ASSOCIATED_CAMERA_SOURCES attribute.
  MF_VIRTUALCAMERA_PROVIDE_ASSOCIATED_CAMERA_SOURCES         : TGUID = '{F0273718-4A4D-4AC5-A15D-305EB5E90667}';

 {end NTDDI_VERSION >= NTDDI_WIN10_NI}


  // MF_VIRTUALCAMERA_CONFIGURATION_APP_PACKAGE_FAMILY_NAME
  // Configuration app's PFN that could be launched by the
  // Settings page to configure a virtual camera.
  //
  // DataType:  String
  //
  // The name must be a Package Family Name.
  MF_VIRTUALCAMERA_CONFIGURATION_APP_PACKAGE_FAMILY_NAME     : TGUID = '{658ABE51-8044-462E-97EA-E676FD72055F}';
  {$EXTERNALSYM MF_VIRTUALCAMERA_CONFIGURATION_APP_PACKAGE_FAMILY_NAME}

  // Virtual Camera operation extended media event type.
  // {E52C4DFF-E46D-4D0B-BC75-DDD4C8723F96}
  MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_INITIALIZE        : TGUID = '{E52C4DFF-E46D-4D0B-BC75-DDD4C8723F96}';
  {$EXTERNALSYM MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_INITIALIZE}
  // {B1EEB989-B456-4F4A-AE40-079C28E24AF8}
  MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_START             : TGUID = '{B1EEB989-B456-4F4A-AE40-079C28E24AF8}';
  {$EXTERNALSYM MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_START}
  // {B7FE7A61-FE91-415E-8608-D37DEDB1A58B}
  MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_STOP              : TGUID = '{B7FE7A61-FE91-415E-8608-D37DEDB1A58B}';
  {$EXTERNALSYM MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_STOP}
  // {A0EBABA7-A422-4E33-8401-B37D2800AA67}
  MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_UNINITIALIZE      : TGUID = '{A0EBABA7-A422-4E33-8401-B37D2800AA67}';
  {$EXTERNALSYM MF_FRAMESERVER_VCAMEVENT_EXTENDED_SOURCE_UNINITIALIZE}
  // {45A81B31-43F8-4E5D-8CE2-22DCE026996D}
  MF_FRAMESERVER_VCAMEVENT_EXTENDED_PIPELINE_SHUTDOWN        : TGUID = '{45A81B31-43F8-4E5D-8CE2-22DCE026996D}';
  {$EXTERNALSYM MF_FRAMESERVER_VCAMEVENT_EXTENDED_PIPELINE_SHUTDOWN}
  // {6E59489C-47D3-4467-83EF-12D34E871665}
  MF_FRAMESERVER_VCAMEVENT_EXTENDED_CUSTOM_EVENT             : TGUID = '{6E59489C-47D3-4467-83EF-12D34E871665}';
  {$EXTERNALSYM MF_FRAMESERVER_VCAMEVENT_EXTENDED_CUSTOM_EVENT}


type

  MFVirtualCameraType = (
    MFVirtualCameraType_SoftwareCameraSource
  );
  {$EXTERNALSYM MFVirtualCameraType}
  PMFVirtualCameraType = ^MFVirtualCameraType;
  {$EXTERNALSYM PMFVirtualCameraType}

  MFVirtualCameraLifetime = (
    MFVirtualCameraLifetime_Session,
    MFVirtualCameraLifetime_System
  );
  {$EXTERNALSYM MFVirtualCameraLifetime}
  PMFVirtualCameraLifetime = ^MFVirtualCameraLifetime;
  {$EXTERNALSYM PMFVirtualCameraLifetime}

  MFVirtualCameraAccess = (
    MFVirtualCameraAccess_CurrentUser,
    MFVirtualCameraAccess_AllUsers
  );
  {$EXTERNALSYM MFVirtualCameraAccess}
  PMFVirtualCameraAccess = ^MFVirtualCameraAccess;
  {$EXTERNALSYM PMFVirtualCameraAccess}




  // Interface IMFCameraSyncObject
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCameraSyncObject);'}
  {$EXTERNALSYM IMFCameraSyncObject}
  IMFCameraSyncObject = interface(IUnknown)
  ['{6338B23A-3042-49D2-A3EA-EC0FED815407}']
    function WaitOnSignal(timeOutInMs: DWORD): HResult; stdcall;

    procedure Shutdown(); stdcall;
  end;
  IID_IMFCameraSyncObject = IMFCameraSyncObject;
  {$EXTERNALSYM IID_IMFCameraSyncObject}


  // Interface IMFVirtualCamera
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVirtualCamera);'}
  {$EXTERNALSYM IMFVirtualCamera}
  IMFVirtualCamera = interface(IMFAttributes)
  ['{1C08A864-EF6C-4C75-AF59-5F2D68DA9563}']
    function AddDeviceSourceInfo(DeviceSourceInfo: PWideChar): HResult; stdcall;

    function AddProperty(const pKey: DEVPROPKEY;
                         _Type: DEVPROPTYPE;
                         pbData: PByte;
                         cbData: ULONG): HResult; stdcall;

    function AddRegistryEntry(EntryName: PWideChar;
                              SubkeyPath: PWideChar;
                              dwRegType: DWORD;
                              pbData: PByte;
                              cbData: ULONG): HResult; stdcall;

    // <summary>
    //     Start the virtual camera discoverability via the PnP enumeration.
    // </summary>
    // <param name="pCallback">
    //     Caller implemented IMFAsyncCallback that receives the state
    //     change for the virtual camera.
    // </param>
    function Start(pCallback: IMFAsyncCallback = Nil): HResult; stdcall;

    // <summary>
    //     Stop the virtual camera discoverability via the PnP enumeration.
    // </summary>
    // <remarks>
    //     Stop does not remove the PnP device from the system.  It simply
    //     marks it as disabled (i.e., can still enumerate if the enumeration
    //     is specified for both enabled and disabled devices).  It also keeps
    //     the virtual camera configuration information.  Remove method will
    //     delete the PnP device node and delete all configuration information
    //     as well.
    // </remarks>
    function Stop(): HResult; stdcall;

    // <summary>
    //     Remove the PnP device from the machine.
    // </summary>
    // <remarks>
    //     Remove will delete the virtual camera device node and all stored
    //     configuration information.
    // </remarks>
    function Remove(): HResult; stdcall;

    function GetMediaSource(out ppMediaSource: IMFMediaSource): HResult; stdcall;

    // <summary>
    //     Issue a GET/SET command to the virtual camera.  This wraps
    //     the IKsControl.KsProperty method.
    // </summary>
    // <param name="propertySet">
    //     Corresponds to KSPROPERTY.Set defined in ks.h.
    // </param>
    // <param name="propertyId">
    //     Corresponds to KSPROPERTY.Id defined in ks.h.
    // </param>
    // <param name="propertyFlags">
    //     Corresponds to KSPROPERTY.Flags defined in ks.h
    //     Only KSPROPERTY_TYPE_SET, KSPROPERTY_TYPE_GET,
    //     KSPROPERTY_TYPE_BASICSUPPORT and KSPROPERTY_TYPE_DEFAULTVALUES
    //     are supported.
    // </param>
    // <param name="propertyPayload">
    //     Additional payload that will be added to the
    //     end of the KSPROPERTY structure when sent to
    //     the virtual camera's IKsControl::KsProperty.
    // </param>
    // <param name="propertyPayloadLength">
    //     Size in bytes of propertyPayload.
    // </param>
    // <param name="data">
    //     The data payload issued to the virtual camera's
    //     IKsControl.KsProperty.
    // </param>
    // <param name="dataLength">
    //     Size in bytes of  data.
    // </param>
    // <param name="dataWritten">
    //     When KSPROPERTY_TYPE_GET, KSPROPERTY_TYPE_BASICSUPPORT or
    //     KSPROPERTY_TYPE_DEFAULTVALUES is specified, the size in bytes
    //     written into the data buffer (or if the data buffer is insufficient,
    //     the amount needed).
    //     For KSPROPERTY_TYPE_SET, this parameter is not used.
    // </param>
    function SendCameraProperty(const propertySet: REFGUID;
                                propertyId: ULONG;
                                propertyFlags: ULONG;
                                propertyPayload: Pointer;
                                propertyPayloadLength: ULONG;
                                data: Pointer;
                                dataLength: ULONG;
                                out dataWritten: PULONG): HResult; stdcall;

    // <summary>
    //     Create a sync object wrapping the KSEVENT event handle.
    // </summary>
    // <param name="kseventSet">
    //     Corresponds to KSEVENT.Set defined in ks.h.
    // </param>
    // <param name="kseventId">
    //     Corresponds to KSEVENT.Id defined in ks.h.
    // </param>
    // <param name="kseventFlags">
    //     Corresponds to KSEVENT.Flags defined in ks.h
    //     Only KSEVENT_TYPE_ENABLE & KSEVENT_TYPE_ONESHOT are supported.
    // </param>
    // <param name="eventHandle">
    //     NT event handle to signal when the KSEVENT is triggered by
    //     the camera driver.
    // </param>
    function CreateSyncEvent(const kseventSet: REFGUID;
                             kseventId: ULONG;
                             kseventFlags: ULONG;
                             const eventHandle: THandle;
                             out cameraSyncObject: IMFCameraSyncObject): HResult; stdcall;

    // <summary>
    //     Create an sync object wrapping the KSEVENT semaphore handle.
    // </summary>
    // <param name="kseventSet">
    //     Corresponds to KSEVENT.Set defined in ks.h.
    // </param>
    // <param name="kseventId">
    //     Corresponds to KSEVENT.Id defined in ks.h.
    // </param>
    // <param name="kseventFlags">
    //     Corresponds to KSEVENT.Flags defined in ks.h
    //     Only KSEVENT_TYPE_ENABLE & KSEVENT_TYPE_ONESHOT are supported.
    // </param>
    // <param name="semaphoreHandle">
    //     NT semaphore handle to signal when the KSEVENT is triggered by
    //     the camera driver.
    // </param>
    // <param name="semaphoreAdjustment">
    //     Corresponds to the KSEVENTDATA.SemaphoreHandle.Adjustment
    //     defined in ks.h.
    // </param>
    function CreateSyncSemaphore(const kseventSet: REFGUID;
                                 kseventId: ULONG;
                                 kseventFlags: ULONG;
                                 const semaphoreHandle: THandle;
                                 semaphoreAdjustment: LONG;
                                 out cameraSyncObject: IMFCameraSyncObject): HResult; stdcall;

    function Shutdown(): HResult; stdcall;
  end;
  IID_IMFVirtualCamera = IMFVirtualCamera;
  {$EXTERNALSYM IID_IMFVirtualCamera}

  // Creates a virtual camera object which can be used by the caller to register, unregister,
  // or remove the virtual camera from the system.
  function MFCreateVirtualCamera(_type: MFVirtualCameraType;
                                 lifetime: MFVirtualCameraLifetime;
                                 access: MFVirtualCameraAccess;
                                 friendlyName: PWideChar;
                                 sourceId: PWideChar;
                                 const categories: PGUID;
                                 categoryCount: ULONG;
                                 out virtualCamera: IMFVirtualCamera): HResult; stdcall;
   {$EXTERNALSYM MFCreateVirtualCamera}

  function IsVirtualCameraTypeSupported(_type: MFVirtualCameraType;
                                        out supported: BOOL): HResult; stdcall;
   {$EXTERNALSYM IsVirtualCameraTypeSupported}


{END WINVER >= NTDDI_WIN10_CO}



  // Additional Prototypes for ALL Interfaces

  // End of Additional Prototypes

implementation

const
  MfVirtualCameraLib = 'mfsensorgroup.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateVirtualCamera;            external MfVirtualCameraLib name 'MFCreateVirtualCamera' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function IsVirtualCameraTypeSupported;     external MfVirtualCameraLib name 'IsVirtualCameraTypeSupported' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey.pas
// Kind: Pascal / Delphi unit
// Release date: 04-06-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Defines property keys for the Plug and Play Device Property API.
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
//          Author: Jim Cavalaris (jamesca) 10-14-2003
//          Environment: User-mode only.
//
//          Revision History:
//          14-October-2003     jamesca
//          Creation and initial implementation.
//          20-June-2006        dougb
//          Copied Jim's version replaced "DEFINE_DEVPROPKEY(DEVPKEY_" with "PKEY_")
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
// Source: functiondiscoverykeys_devpkey.h
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
unit WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey;

  {$HPPEMIT '#include "functiondiscoverykeys_devpkey.h"'}

interface

uses
  {MfPack}
  WinApi.WinApiTypes,
  WinApi.DevPropDef,
  WinApi.PropSys;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  PKEY_NAME: PROPERTYKEY = (fmtid: (D1: $B725F130;
                                    D2: $47EF;
                                    D3: $101A;
                                    D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
                                    pid: 10);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_NAME}

  // Device properties
  // These PKEYs correspond to the old setupapi SPDRP_XXX properties

  PKEY_Device_DeviceDesc: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                 D2: $DF1C;
                                                 D3: $4EFD;
                                                 D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                 pid: 2);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DeviceDesc}

  PKEY_Device_HardwareIds: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                  D2: $DF1C;
                                                  D3: $4EFD;
                                                  D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                  pid: 3);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_HardwareIds}

  PKEY_Device_CompatibleIds: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                    D2: $DF1C;
                                                    D3: $4EFD;
                                                    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                    pid: 4);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_CompatibleIds}

  PKEY_Device_Service: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                              D2: $DF1C;
                                              D3: $4EFD;
                                              D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                              pid: 6);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_Service}

  PKEY_Device_Class: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                            D2: $DF1C;
                                            D3: $4EFD;
                                            D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                            pid: 9);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_Class}

  PKEY_Device_ClassGuid: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                D2: $DF1C;
                                                D3: $4EFD;
                                                D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                pid: 10);  // DEVPROP_TYPE_GUID
  {$EXTERNALSYM PKEY_Device_ClassGuid}

  PKEY_Device_Driver: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                             D2: $DF1C;
                                             D3: $4EFD;
                                             D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                             pid: 11);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_Driver}

  PKEY_Device_ConfigFlags: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                  D2: $DF1C;
                                                  D3: $4EFD;
                                                  D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                  pid: 12);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_ConfigFlags}

  PKEY_Device_Manufacturer: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 13);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_Manufacturer}

  PKEY_Device_FriendlyName: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 14);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_FriendlyName}

  PKEY_Device_LocationInfo: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 15);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_LocationInfo}

  PKEY_Device_PDOName: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                              D2: $DF1C;
                                              D3: $4EFD;
                                              D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                              pid: 16);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_PDOName}

  PKEY_Device_Capabilities: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 17);  // DEVPROP_TYPE_UNINT32
  {$EXTERNALSYM PKEY_Device_Capabilities}

  PKEY_Device_UINumber: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                               D2: $DF1C;
                                               D3: $4EFD;
                                               D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                               pid: 18);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_UINumber}

  PKEY_Device_UpperFilters: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 19);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_UpperFilters}

  PKEY_Device_LowerFilters: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 20);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_LowerFilters}

  PKEY_Device_BusTypeGuid: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                  D2: $DF1C;
                                                  D3: $4EFD;
                                                  D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                  pid: 21);  // DEVPROP_TYPE_GUID
  {$EXTERNALSYM PKEY_Device_BusTypeGuid}

  PKEY_Device_LegacyBusType: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                    D2: $DF1C;
                                                    D3: $4EFD;
                                                    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                    pid: 22);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_LegacyBusType}

  PKEY_Device_BusNumber: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                D2: $DF1C;
                                                D3: $4EFD;
                                                D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                pid: 23);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_BusNumber}

  PKEY_Device_EnumeratorName: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                     D2: $DF1C;
                                                     D3: $4EFD;
                                                     D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                     pid: 24);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_EnumeratorName}

  PKEY_Device_Security: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                               D2: $DF1C;
                                               D3: $4EFD;
                                               D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                               pid: 25);  // DEVPROP_TYPE_SECURITY_DESCRIPTOR
  {$EXTERNALSYM PKEY_Device_Security}

  PKEY_Device_SecuritySDS: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                  D2: $DF1C;
                                                  D3: $4EFD;
                                                  D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                  pid: 26);  // DEVPROP_TYPE_SECURITY_DESCRIPTOR_STRING
  {$EXTERNALSYM PKEY_Device_SecuritySDS}

  PKEY_Device_DevType: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                              D2: $DF1C;
                                              D3: $4EFD;
                                              D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                              pid: 27);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_DevType}

  PKEY_Device_Exclusive: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                D2: $DF1C;
                                                D3: $4EFD;
                                                D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                pid: 28);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_Exclusive}

  PKEY_Device_Characteristics: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                      D2: $DF1C;
                                                      D3: $4EFD;
                                                      D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                      pid: 29);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_Characteristics}

  PKEY_Device_Address: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                              D2: $DF1C;
                                              D3: $4EFD;
                                              D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                              pid: 30);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_Address}

  PKEY_Device_UINumberDescFormat: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                         D2: $DF1C;
                                                         D3: $4EFD;
                                                         D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                         pid: 31);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_UINumberDescFormat}

  PKEY_Device_PowerData: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                D2: $DF1C;
                                                D3: $4EFD;
                                                D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                pid: 32);  // DEVPROP_TYPE_BINARY
  {$EXTERNALSYM PKEY_Device_PowerData}

  PKEY_Device_RemovalPolicy: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                    D2: $DF1C;
                                                    D3: $4EFD;
                                                    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                    pid: 33);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_RemovalPolicy}

  PKEY_Device_RemovalPolicyDefault: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                           D2: $DF1C;
                                                           D3: $4EFD;
                                                           D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                           pid: 34);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_RemovalPolicyDefault}

  PKEY_Device_RemovalPolicyOverride: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                            D2: $DF1C;
                                                            D3: $4EFD;
                                                            D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                            pid: 35);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_RemovalPolicyOverride}

  PKEY_Device_InstallState: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                   D2: $DF1C;
                                                   D3: $4EFD;
                                                   D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                   pid: 36);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_InstallState}

  PKEY_Device_LocationPaths: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                    D2: $DF1C;
                                                    D3: $4EFD;
                                                    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                    pid: 37);   // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_LocationPaths}

  PKEY_Device_BaseContainerId: PROPERTYKEY = (fmtid: (D1: $A45C254E;
                                                      D2: $DF1C;
                                                      D3: $4EFD;
                                                      D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0));
                                                      pid: 38);  // DEVPROP_TYPE_GUID
  {$EXTERNALSYM PKEY_Device_BaseContainerId}


  // Device properties
  // These PKEYs correspond to a device's status and problem code

  PKEY_Device_DevNodeStatus: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                    D2: $93FA;
                                                    D3: $4706;
                                                    D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                    pid: 2);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_DevNodeStatus}

  PKEY_Device_ProblemCode: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                  D2: $93FA;
                                                  D3: $4706;
                                                  D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                  pid: 3);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_ProblemCode}


  // Device properties
  // These PKEYs correspond to device relations

  PKEY_Device_EjectionRelations: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                        D2: $93FA;
                                                        D3: $4706;
                                                        D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                        pid: 4);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_EjectionRelations}

  PKEY_Device_RemovalRelations: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                       D2: $93FA;
                                                       D3: $4706;
                                                       D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                       pid: 5);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_RemovalRelations}

  PKEY_Device_PowerRelations: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                     D2: $93FA;
                                                     D3: $4706;
                                                     D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                     pid: 6);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_PowerRelations}

  PKEY_Device_BusRelations: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                   D2: $93FA;
                                                   D3: $4706;
                                                   D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                   pid: 7);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_BusRelations}

  PKEY_Device_Parent: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                             D2: $93FA;
                                             D3: $4706;
                                             D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                             pid: 8);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_Parent}

  PKEY_Device_Children: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                               D2: $93FA;
                                               D3: $4706;
                                               D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                               pid: 9);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_Children}

  PKEY_Device_Siblings: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                               D2: $93FA;
                                               D3: $4706;
                                               D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                               pid: 10);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_Siblings}

  PKEY_Device_TransportRelations: PROPERTYKEY = (fmtid: (D1: $4340A6C5;
                                                         D2: $93FA;
                                                         D3: $4706;
                                                         D4: ($97, $2C, $7B, $64, $80, $08, $A5, $A7));
                                                         pid: 11);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_TransportRelations}

  //
  // Other Device properties
  //

  PKEY_Device_Reported: PROPERTYKEY = (fmtid: (D1: $80497100;
                                               D2: $8C73;
                                               D3: $48B9;
                                               D4: ($AA, $D9, $CE, $38, $7E, $19, $C5, $6E));
                                               pid: 2);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_Reported}

  PKEY_Device_Legacy: PROPERTYKEY = (fmtid: (D1: $80497100;
                                             D2: $8C73;
                                             D3: $48B9;
                                             D4: ($AA, $D9, $CE, $38, $7E, $19, $C5, $6E));
                                             pid: 3);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_Legacy}

  PKEY_Device_InstanceId: PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                 D2: $104A; D3: $4ACA;
                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                 pid: 256);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_InstanceId}

  PKEY_Device_ContainerId: PROPERTYKEY = (fmtid: (D1: $8C7ED206;
                                                  D2: $3F8A;
                                                  D3: $4827;
                                                  D4: ($B3, $AB, $AE, $9E, $1F, $AE, $FC, $6C));
                                                  pid: 2);  // DEVPROP_TYPE_GUID
  {$EXTERNALSYM PKEY_Device_ContainerId}

  PKEY_Device_ModelId: PROPERTYKEY = (fmtid: (D1: $80D81EA6;
                                              D2: $7473;
                                              D3: $4B0C;
                                              D4: ($82, $16, $EF, $C1, $1A, $2C, $4C, $8B));
                                              pid: 2);  // DEVPROP_TYPE_GUID
  {$EXTERNALSYM PKEY_Device_ModelId}

  PKEY_Device_FriendlyNameAttributes: PROPERTYKEY = (fmtid: (D1: $80D81EA6;
                                                             D2: $7473;
                                                             D3: $4B0C;
                                                             D4: ($82, $16, $EF, $C1, $1A, $2C, $4C, $8B));
                                                             pid: 3);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_FriendlyNameAttributes}

  PKEY_Device_ManufacturerAttributes: PROPERTYKEY = (fmtid: (D1: $80D81EA6;
                                                             D2: $7473;
                                                             D3: $4B0C;
                                                             D4: ($82, $16, $EF, $C1, $1A, $2C, $4C, $8B));
                                                             pid: 4);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_ManufacturerAttributes}

  PKEY_Device_PresenceNotForDevice: PROPERTYKEY = (fmtid: (D1: $80D81EA6;
                                                           D2: $7473;
                                                           D3: $4B0C;
                                                           D4: ($82, $16, $EF, $C1, $1A, $2C, $4C, $8B));
                                                           pid: 5);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_PresenceNotForDevice}

  PKEY_Numa_Proximity_Domain: PROPERTYKEY = (fmtid: (D1: $540B947E;
                                                     D2: $8B40;
                                                     D3: $45BC;
                                                     D4: ($A8, $A2, $6A, $0B, $89, $4C, $BD, $A2));
                                                     pid: 1);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Numa_Proximity_Domain}

  PKEY_Device_DHP_Rebalance_Policy: PROPERTYKEY = (fmtid: (D1: $540B947E;
                                                           D2: $8B40;
                                                           D3: $45BC;
                                                           D4: ($A8, $A2, $6A, $0B, $89, $4C, $BD, $A2));
                                                           pid: 2);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_DHP_Rebalance_Policy}

  PKEY_Device_Numa_Node: PROPERTYKEY = (fmtid: (D1: $540B947E;
                                                D2: $8B40;
                                                D3: $45BC;
                                                D4: ($A8, $A2, $6A, $0B, $89, $4C, $BD, $A2));
                                                pid: 3);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_Numa_Node}

  PKEY_Device_BusReportedDeviceDesc: PROPERTYKEY = (fmtid: (D1: $540B947E;
                                                            D2: $8B40;
                                                            D3: $45BC;
                                                            D4: ($A8, $A2, $6A, $0B, $89, $4C, $BD, $A2));
                                                            pid: 4);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_BusReportedDeviceDesc}

  PKEY_Device_InstallInProgress: PROPERTYKEY = (fmtid: (D1: $83DA6326;
                                                        D2: $97A6;
                                                        D3: $4088;
                                                        D4: ($94, $53, $A1, $92, $3F, $57, $3B, $29));
                                                        pid: 9);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_InstallInProgress}

  // Device driver properties

  PKEY_Device_DriverDate: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                 D2: $2E3D;
                                                 D3: $4094;
                                                 D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                 pid: 2);  // DEVPROP_TYPE_FILETIME
  {$EXTERNALSYM PKEY_Device_DriverDate}

  PKEY_Device_DriverVersion: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                    D2: $2E3D;
                                                    D3: $4094;
                                                    D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                    pid: 3);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverVersion}

  PKEY_Device_DriverDesc: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                 D2: $2E3D;
                                                 D3: $4094;
                                                 D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                 pid: 4);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverDesc}

  PKEY_Device_DriverInfPath: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                    D2: $2E3D;
                                                    D3: $4094;
                                                    D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                    pid: 5);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverInfPath}

  PKEY_Device_DriverInfSection: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                       D2: $2E3D;
                                                       D3: $4094;
                                                       D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                       pid: 6);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverInfSection}

  PKEY_Device_DriverInfSectionExt: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                          D2: $2E3D;
                                                          D3: $4094;
                                                          D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                          pid: 7);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverInfSectionExt}

  PKEY_Device_MatchingDeviceId: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                       D2: $2E3D;
                                                       D3: $4094;
                                                       D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                       pid: 8);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_MatchingDeviceId}

  PKEY_Device_DriverProvider: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                     D2: $2E3D;
                                                     D3: $4094;
                                                     D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                     pid: 9);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverProvider}

  PKEY_Device_DriverPropPageProvider: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                             D2: $2E3D;
                                                             D3: $4094;
                                                             D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                             pid: 10);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_DriverPropPageProvider}

  PKEY_Device_DriverCoInstallers: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                         D2: $2E3D;
                                                         D3: $4094;
                                                         D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                         pid: 11);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_Device_DriverCoInstallers}

  PKEY_Device_ResourcePickerTags: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                         D2: $2E3D;
                                                         D3: 4094;
                                                         D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                         pid: 12);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_ResourcePickerTags}

  PKEY_Device_ResourcePickerExceptions: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                               D2: $2E3D;
                                                               D3: $4094;
                                                               D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                               pid: 13);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_Device_ResourcePickerExceptions}

  PKEY_Device_DriverRank: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                 D2: $2E3D;
                                                 D3: $4094;
                                                 D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                 pid: 14);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_DriverRank}

  PKEY_Device_DriverLogoLevel: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                      D2: $2E3D;
                                                      D3: $4094;
                                                      D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                      pid: 15);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_Device_DriverLogoLevel}

  PKEY_Device_NoConnectSound: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                     D2: $2E3D;
                                                     D3: $4094;
                                                     D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                     pid: 17);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_NoConnectSound}

  PKEY_Device_GenericDriverInstalled: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                             D2: $2E3D;
                                                             D3: $4094;
                                                             D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                             pid: 18);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_GenericDriverInstalled}

  PKEY_Device_AdditionalSoftwareRequested: PROPERTYKEY = (fmtid: (D1: $A8B865DD;
                                                                  D2: $2E3D;
                                                                  D3: $4094;
                                                                  D4: ($AD, $97, $E5, $93, $A7, $C, $75, $D6));
                                                                  pid: 19); // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_AdditionalSoftwareRequested}

  // Device safe-removal properties

  PKEY_Device_SafeRemovalRequired: PROPERTYKEY = (fmtid: (D1: $AFD97640;
                                                          D2: $86A3;
                                                          D3: $4210;
                                                          D4: ($B6, $7C, $28, $9C, $41, $AA, $BE, $55));
                                                          pid: 2);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_SafeRemovalRequired}

  PKEY_Device_SafeRemovalRequiredOverride: PROPERTYKEY = (fmtid: (D1: $AFD97640;
                                                                  D2: $86A3;
                                                                  D3: $4210;
                                                                  D4: ($B6, $7C, $28, $9C, $41, $AA, $BE, $55));
                                                                  pid: 3); // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_Device_SafeRemovalRequiredOverride}


  // Device properties that were set by the driver package that was installed
  // on the device.

  PKEY_DrvPkg_Model: PROPERTYKEY = (fmtid: (D1: $CF73BB51;
                                            D2: $3ABF;
                                            D3: $44A2;
                                            D4: ($85, $E0, $9A, $3D, $C7, $A1, $21, $32));
                                            pid: 2);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DrvPkg_Model}

  PKEY_DrvPkg_VendorWebSite: PROPERTYKEY = (fmtid: (D1: $CF73BB51;
                                                    D2: $3ABF;
                                                    D3: $44A2;
                                                    D4: ($85, $E0, $9A, $3D, $C7, $A1, $21, $32));
                                                    pid: 3);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DrvPkg_VendorWebSite}

  PKEY_DrvPkg_DetailedDescription: PROPERTYKEY = (fmtid: (D1: $CF73BB51;
                                                          D2: $3ABF;
                                                          D3: $44A2;
                                                          D4: ($85, $E0, $9A, $3D, $C7, $A1, $21, $32));
                                                          pid: 4);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DrvPkg_DetailedDescription}

  PKEY_DrvPkg_DocumentationLink: PROPERTYKEY = (fmtid: (D1: $CF73BB51;
                                                        D2: $3ABF;
                                                        D3: $44A2;
                                                        D4: ($85, $E0, $9A, $3D, $C7, $A1, $21, $32));
                                                        pid: 5);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DrvPkg_DocumentationLink}

  PKEY_DrvPkg_Icon: PROPERTYKEY = (fmtid: (D1: $CF73BB51;
                                           D2: $3ABF;
                                           D3: $44A2;
                                           D4: ($85, $E0, $9A, $3D, $C7, $A1, $21, $32));
                                           pid: 6);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_DrvPkg_Icon}

  PKEY_DrvPkg_BrandingIcon: PROPERTYKEY = (fmtid: (D1: $CF73BB51;
                                                   D2: $3ABF;
                                                   D3: $44A2;
                                                   D4: ($85, $E0, $9A, $3D, $C7, $A1, $21, $32));
                                                   pid: 7);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_DrvPkg_BrandingIcon}


  // Device setup class properties
  // These PKEYs correspond to the old setupapi SPCRP_XXX properties

  PKEY_DeviceClass_UpperFilters: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                        D2: $F69E;
                                                        D3: $470D;
                                                        D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                        pid: 19);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_DeviceClass_UpperFilters}

  PKEY_DeviceClass_LowerFilters: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                        D2: $F69E;
                                                        D3: $470D;
                                                        D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                        pid: 20);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_DeviceClass_LowerFilters}

  PKEY_DeviceClass_Security: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                    D2: $F69E;
                                                    D3: $470D;
                                                    D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                    pid: 25);  // DEVPROP_TYPE_SECURITY_DESCRIPTOR
  {$EXTERNALSYM PKEY_DeviceClass_Security}

  PKEY_DeviceClass_SecuritySDS: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                       D2: $F69E;
                                                       D3: $470D;
                                                       D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                       pid: 26);  // DEVPROP_TYPE_SECURITY_DESCRIPTOR_STRING
  {$EXTERNALSYM PKEY_DeviceClass_SecuritySDS}

  PKEY_DeviceClass_DevType: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                   D2: $F69E;
                                                   D3: $470D;
                                                   D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                   pid: 27);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_DeviceClass_DevType}

  PKEY_DeviceClass_Exclusive: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                     D2: $F69E;
                                                     D3: $470D;
                                                     D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                     pid: 28);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_DeviceClass_Exclusive}

  PKEY_DeviceClass_Characteristics: PROPERTYKEY = (fmtid: (D1: $4321918B;
                                                           D2: $F69E;
                                                           D3: $470D;
                                                           D4: ($A5, $DE, $4D, $88, $C7, $5A, $D2, $4B));
                                                           pid: 29);  // DEVPROP_TYPE_UINT32
  {$EXTERNALSYM PKEY_DeviceClass_Characteristics}


  // Device setup class properties
  // These PKEYs correspond to registry values under the device class GUID key

  PKEY_DeviceClass_Name: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                D2: $50A7;
                                                D3: $47CE;
                                                D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                pid: 2);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceClass_Name}

  PKEY_DeviceClass_ClassName: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                     D2: $50A7;
                                                     D3: $47CE;
                                                     D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                     pid: 3);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceClass_ClassName}

  PKEY_DeviceClass_Icon: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                D2: $50A7;
                                                D3: $47CE;
                                                D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                pid: 4);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceClass_Icon}

  PKEY_DeviceClass_ClassInstaller: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                          D2: $50A7;
                                                          D3: $47CE;
                                                          D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                          pid: 5);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceClass_ClassInstaller}

  PKEY_DeviceClass_PropPageProvider: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                            D2: $50A7;
                                                            D3: $47CE;
                                                            D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                            pid: 6);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceClass_PropPageProvider}

  PKEY_DeviceClass_NoInstallClass: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                          D2: $50A7;
                                                          D3: $47CE;
                                                          D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                          pid: 7);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_DeviceClass_NoInstallClass}

  PKEY_DeviceClass_NoDisplayClass: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                          D2: $50A7;
                                                          D3: $47CE;
                                                          D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                          pid: 8);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_DeviceClass_NoDisplayClass}

  PKEY_DeviceClass_SilentInstall: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                         D2: $50A7;
                                                         D3: $47CE;
                                                         D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                         pid: 9);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_DeviceClass_SilentInstall}

  PKEY_DeviceClass_NoUseClass: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                      D2: $50A7;
                                                      D3: $47CE;
                                                      D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                      pid: 10);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_DeviceClass_NoUseClass}

  PKEY_DeviceClass_DefaultService: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                          D2: $50A7;
                                                          D3: $47CE;
                                                          D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                          pid: 11);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceClass_DefaultService}

  PKEY_DeviceClass_IconPath: PROPERTYKEY = (fmtid: (D1: $259ABFFC;
                                                    D2: $50A7;
                                                    D3: $47CE;
                                                    D4: ($AF, $8, $68, $C9, $A7, $D7, $33, $66));
                                                    pid: 12);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_DeviceClass_IconPath}


  // Other Device setup class properties

  PKEY_DeviceClass_ClassCoInstallers: PROPERTYKEY = (fmtid: (D1: $713D1703;
                                                             D2: $A2E2;
                                                             D3: $49F5;
                                                             D4: ($92, $14, $56, $47, $2E, $F3, $DA, $5C));
                                                             pid: 2);  // DEVPROP_TYPE_STRING_LIST
  {$EXTERNALSYM PKEY_DeviceClass_ClassCoInstallers}


  // Device interface properties

  PKEY_DeviceInterface_FriendlyName: PROPERTYKEY = (fmtid: (D1: $026E516E;
                                                            D2: $B814;
                                                            D3: $414B;
                                                            D4: ($83, $CD, $85, $6D, $6F, $EF, $48, $22));
                                                            pid: 2);  // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceInterface_FriendlyName}

  PKEY_DeviceInterface_Enabled: PROPERTYKEY = (fmtid: (D1: $026E516E;
                                                       D2: $B814;
                                                       D3: $414B;
                                                       D4: ($83, $CD, $85, $6D, $6F, $EF, $48, $22));
                                                       pid: 3);  // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_DeviceInterface_Enabled}

  PKEY_DeviceInterface_ClassGuid: PROPERTYKEY = (fmtid: (D1: $026E516E;
                                                         D2: $B814;
                                                         D3: $414B;
                                                         D4: ($83, $CD, $85, $6D, $6F, $EF, $48, $22));
                                                         pid: 4);  // DEVPROP_TYPE_GUID
  {$EXTERNALSYM PKEY_DeviceInterface_ClassGuid}


  // Device interface class properties

  PKEY_DeviceInterfaceClass_DefaultInterface: PROPERTYKEY = (fmtid: (D1: $14C83A99;
                                                                     D2: $0B3F;
                                                                     D3: $44B7;
                                                                     D4: ($BE, $4C, $A1, $78, $D3, $99, $05, $64));
                                                                     pid: 2); // DEVPROP_TYPE_STRING
  {$EXTERNALSYM PKEY_DeviceInterfaceClass_DefaultInterface}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.FunctionDiscoveryApi.FunctionDiscoveryKeys.pas
// Kind: Pascal / Delphi unit
// Release date: 17-09-2020
// Language: ENU
//
// Revision Version: 4.0.0
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
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: functiondiscoverykeys.h
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
unit WinApi.FunctionDiscoveryApi.FunctionDiscoveryKeys;

interface

  {$HPPEMIT '#include "functiondiscoverykeys.h"'}

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.FunctionDiscoveryApi.FunctionDiscoveryKeys_devpkey;

{$MINENUMSIZE 4}

const

  // /* 08c0c253-a154-4746-9005-82de5317148b */
  PKEY_FunctionInstance : PROPERTYKEY = (fmtid: (D1: $08C0C253;
                                                 D2: $A154;
                                                 D3: $4746;
                                                 D4: ($90, $05, $82, $DE, $53, $17, $14, $8B));
                                                 pid: $00000001); // VT_UNKNOWN
  {$EXTERNALSYM PKEY_FunctionInstance}

  // FMTID_FD = {904b03a2-471d-423c-a584-f3483238a146}
  FMTID_FD:  TGUID = (D1: $904B03A2;
                      D2: $471D;
                      D3: $423C;
                      D4: ($A5, $84, $F3, $48, $32, $38, $A1, $46));
  {$EXTERNALSYM FMTID_FD}

  PKEY_FD_Visibility : PROPERTYKEY = (fmtid: (D1: $904B03A2;
                                              D2: $471D;
                                              D3: $423C;
                                              D4: ($A5, $84, $F3, $48, $32, $38, $A1, $46));
                                              pid: $00000001); //    VT_UINT
  {$EXTERNALSYM PKEY_FD_Visibility}

  FD_Visibility_Default = 0;
  {$EXTERNALSYM FD_Visibility_Default}
  FD_Visibility_Hidden = 1;
  {$EXTERNALSYM FD_Visibility_Hidden}

  // FMTID_Device = {78C34FC8-104A-4aca-9EA4-524D52996E57}
  FMTID_Device:  TGUID = (D1: $78C34FC8;
                          D2: $104A;
                          D3: $4ACA;
                          D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
  {$EXTERNALSYM FMTID_Device}

  PKEY_Device_NotPresent : PROPERTYKEY = (fmtid: (D1: $904B03A2;
                                                  D2: $471D;
                                                  D3: $423C;
                                                  D4: ($A5, $84, $F3, $48, $32, $38, $A1, $46));
                                                  pid: $00000002); //    VT_UINT
  {$EXTERNALSYM PKEY_Device_NotPresent}

  PKEY_Device_QueueSize : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                 D2: $104A;
                                                 D3: $4ACA;
                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                 pid: $00000024); //    VT_UI4
  {$EXTERNALSYM PKEY_Device_QueueSize}

  PKEY_Device_Status : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                              D2: $104A;
                                              D3: $4ACA;
                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                              pid: $00000025); //    VT_LPWSTR
  {$EXTERNALSYM PKEY_Device_Status}

  PKEY_Device_Comment : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                               D2: $104A;
                                               D3: $4ACA;
                                               D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                               pid: $00000026); //    VT_LPWSTR
  {$EXTERNALSYM PKEY_Device_Comment}

  PKEY_Device_Model : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                             D2: $104A;
                                             D3: $4ACA;
                                             D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                             pid: $00000027); //    VT_LPWSTR
  {$EXTERNALSYM PKEY_Device_Model}

  // FMTID_Device = {53808008-07BB-4661-BC3C-B5953E708560}
  FMTID_DeviceInterface:  TGUID = (D1: $53808008;
                                   D2: $07BB;
                                   D3: $4661;
                                   D4: ($BC, $3C, $B5, $95, $3E, $70, $85, $60));
  {$EXTERNALSYM FMTID_DeviceInterface}

  PKEY_DeviceInterface_DevicePath : PROPERTYKEY = (fmtid: (D1: $53808008;
                                                           D2: $07BB;
                                                           D3: $4661;
                                                           D4: ($BC, $3C, $B5, $95, $3E, $70, $85, $60));
                                                           pid: $00000001); //    VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceInterface_DevicePath}

  PKEY_DeviceDisplay_Address : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                      D2: $104A;
                                                      D3: $4ACA;
                                                      D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                      pid: $00000033); // VT_LPWSTR or VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_Address}

  PKEY_DeviceDisplay_DiscoveryMethod : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000034); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_DiscoveryMethod}

  PKEY_DeviceDisplay_IsEncrypted : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                          D2: $104A;
                                                          D3: $4ACA;
                                                          D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                          pid: $00000035); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsEncrypted}

  PKEY_DeviceDisplay_IsAuthenticated : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000036); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsAuthenticated}

  PKEY_DeviceDisplay_IsConnected : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                          D2: $104A;
                                                          D3: $4ACA;
                                                          D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                          pid: $00000037); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsConnected}

  PKEY_DeviceDisplay_IsPaired : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                       D2: $104A;
                                                       D3: $4ACA;
                                                       D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                       pid: $00000038); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsPaired}

  PKEY_DeviceDisplay_Icon : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                   D2: $104A;
                                                   D3: $4ACA;
                                                   D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                   pid: $00000039); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_Icon}

  PKEY_DeviceDisplay_Version : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                      D2: $104A;
                                                      D3: $4ACA;
                                                      D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                      pid: $00000041); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_Version}

  PKEY_DeviceDisplay_Last_Seen : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                        D2: $104A;
                                                        D3: $4ACA;
                                                        D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                        pid: $00000042); // VT_FIELTIME
  {$EXTERNALSYM PKEY_DeviceDisplay_Last_Seen}

  PKEY_DeviceDisplay_Last_Connected : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                             D2: $104A;
                                                             D3: $4ACA;
                                                             D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                             pid: $00000043); // VT_FILETIME
  {$EXTERNALSYM PKEY_DeviceDisplay_Last_Connected}

  PKEY_DeviceDisplay_IsShowInDisconnectedState : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                        D2: $104A;
                                                                        D3: $4ACA;
                                                                        D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                        pid: $00000044); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsShowInDisconnectedState}

  PKEY_DeviceDisplay_IsLocalMachine : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                             D2: $104A;
                                                             D3: $4ACA;
                                                             D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                             pid: $00000046); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsLocalMachine}

  PKEY_DeviceDisplay_MetadataPath : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                           D2: $104A;
                                                           D3: $4ACA;
                                                           D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                           pid: $00000047); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_MetadataPath}

  PKEY_DeviceDisplay_IsMetadataSearchInProgress : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                         D2: $104A;
                                                                         D3: $4ACA;
                                                                         D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                         pid: $00000048); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsMetadataSearchInProgress}

  PKEY_DeviceDisplay_MetadataChecksum : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                               D2: $104A;
                                                               D3: $4ACA;
                                                               D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                               pid: $00000049); // VT_UI1 | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_MetadataChecksum}

  PKEY_DeviceDisplay_IsNotInterestingForDisplay : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                         D2: $104A;
                                                                         D3: $4ACA;
                                                                         D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                         pid: $0000004A); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsNotInterestingForDisplay}

  PKEY_DeviceDisplay_LaunchDeviceStageOnDeviceConnect : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                               D2: $104A;
                                                                               D3: $4ACA;
                                                                               D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                               pid: $0000004C); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_LaunchDeviceStageOnDeviceConnect}

  PKEY_DeviceDisplay_LaunchDeviceStageFromExplorer : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                            D2: $104A;
                                                                            D3: $4ACA;
                                                                            D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                            pid: $0000004D); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_LaunchDeviceStageFromExplorer}

  PKEY_DeviceDisplay_BaselineExperienceId : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                   D2: $104A;
                                                                   D3: $4ACA;
                                                                   D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                   pid: $0000004E); // VT_CLSID
  {$EXTERNALSYM PKEY_DeviceDisplay_BaselineExperienceId}

  PKEY_DeviceDisplay_IsDeviceUniquelyIdentifiable : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                           D2: $104A;
                                                                           D3: $4ACA;
                                                                           D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                           pid: $0000004F); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsDeviceUniquelyIdentifiable}

  PKEY_DeviceDisplay_AssociationArray : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                               D2: $104A;
                                                               D3: $4ACA;
                                                               D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                               pid: $00000050); // VT_LPWSTR  | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_AssociationArray}

  PKEY_DeviceDisplay_DeviceDescription1 : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                 D2: $104A;
                                                                 D3: $4ACA;
                                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                 pid: $00000051); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_DeviceDescription1}

  PKEY_DeviceDisplay_DeviceDescription2 : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                 D2: $104A;
                                                                 D3: $4ACA;
                                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                 pid: $00000052); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_DeviceDescription2}

  PKEY_DeviceDisplay_IsNotWorkingProperly : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                   D2: $104A;
                                                                   D3: $4ACA;
                                                                   D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                   pid: $00000053); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsNotWorkingProperly}

  PKEY_DeviceDisplay_IsSharedDevice : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                             D2: $104A;
                                                             D3: $4ACA;
                                                             D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                             pid: $00000054); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsSharedDevice}

  PKEY_DeviceDisplay_IsNetworkDevice : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000055); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsNetworkDevice}

  PKEY_DeviceDisplay_IsDefaultDevice : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000056); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_IsDefaultDevice}

  PKEY_DeviceDisplay_MetadataCabinet : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000057); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_MetadataCabinet}

  PKEY_DeviceDisplay_RequiresPairingElevation : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                       D2: $104A;
                                                                       D3: $4ACA;
                                                                       D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                       pid: $00000058); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_RequiresPairingElevation}

  PKEY_DeviceDisplay_ExperienceId : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                           D2: $104A;
                                                           D3: $4ACA;
                                                           D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                           pid: $00000059); // VT_CLSID
  {$EXTERNALSYM PKEY_DeviceDisplay_ExperienceId}

  PKEY_DeviceDisplay_Category : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                       D2: $104A;
                                                       D3: $4ACA;
                                                       D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                       pid: $0000005A); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_Category}

  PKEY_DeviceDisplay_Category_Desc_Singular : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                     D2: $104A;
                                                                     D3: $4ACA;
                                                                     D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                     pid: $0000005B); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_Category_Desc_Singular}

  PKEY_DeviceDisplay_Category_Desc_Plural : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                   D2: $104A;
                                                                   D3: $4ACA;
                                                                   D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                   pid: $0000005C); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_Category_Desc_Plural}

  PKEY_DeviceDisplay_Category_Icon : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                            D2: $104A;
                                                            D3: $4ACA;
                                                            D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                            pid: $0000005D); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_Category_Icon}

  PKEY_DeviceDisplay_CategoryGroup_Desc : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                 D2: $104A;
                                                                 D3: $4ACA;
                                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                 pid: $0000005E); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_DeviceDisplay_CategoryGroup_Desc}

  PKEY_DeviceDisplay_CategoryGroup_Icon : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                 D2: $104A;
                                                                 D3: $4ACA;
                                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                 pid: $0000005F); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_CategoryGroup_Icon}

  PKEY_DeviceDisplay_PrimaryCategory : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000061); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_PrimaryCategory}

  PKEY_DeviceDisplay_UnpairUninstall : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                              D2: $104A;
                                                              D3: $4ACA;
                                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                              pid: $00000062); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_UnpairUninstall}

  PKEY_DeviceDisplay_RequiresUninstallElevation : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                         D2: $104A;
                                                                         D3: $4ACA;
                                                                         D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                         pid: $00000063); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_RequiresUninstallElevation}

  PKEY_DeviceDisplay_DeviceFunctionSubRank : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                    D2: $104A;
                                                                    D3: $4ACA;
                                                                    D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                    pid: $00000064); // VT_UI4
  {$EXTERNALSYM PKEY_DeviceDisplay_DeviceFunctionSubRank}

  PKEY_DeviceDisplay_AlwaysShowDeviceAsConnected : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                                          D2: $104A;
                                                                          D3: $4ACA;
                                                                          D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                                          pid: $00000065); // VT_BOOL
  {$EXTERNALSYM PKEY_DeviceDisplay_AlwaysShowDeviceAsConnected}

  PKEY_DeviceDisplay_FriendlyName : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                           D2: $ECC0;
                                                           D3: $43FD;
                                                           D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                           pid: $00003000); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_FriendlyName}

  PKEY_DeviceDisplay_Manufacturer : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                           D2: $ECC0;
                                                           D3: $43FD;
                                                           D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                           pid: $00002000); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_Manufacturer}

  PKEY_DeviceDisplay_ModelName : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                        D2: $ECC0;
                                                        D3: $43FD;
                                                        D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                        pid: $00002002); // VT_LPWSTR (localizable)
  {$EXTERNALSYM PKEY_DeviceDisplay_ModelName}

  PKEY_DeviceDisplay_ModelNumber : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                          D2: $ECC0;
                                                          D3: $43FD;
                                                          D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                          pid: $00002003); // VT_LPWSTR
  {$EXTERNALSYM PKEY_DeviceDisplay_ModelNumber}

  PKEY_DeviceDisplay_InstallInProgress : PROPERTYKEY = (fmtid: (D1: $83DA6326;
                                                                D2: $97A6;
                                                                D3: $4088;
                                                                D4: ($94, $53, $A1, $92, $3F, $57, $3B, $29));
                                                                pid: 9); // DEVPROP_TYPE_BOOLEAN
  {$EXTERNALSYM PKEY_DeviceDisplay_InstallInProgress}

  // FMTID_Pairing = {8807CAE6-7DB6-4f10-8EE4-435EAA1392BC}
  FMTID_Pairing:  TGUID = (D1: $8807CAE6;
                           D2: $7DB6;
                           D3: $4F10;
                           D4: ($8E, $E4, $43, $5E, $AA, $13, $92, $BC));
  {$EXTERNALSYM FMTID_Pairing}

  PKEY_Pairing_ListItemText : PROPERTYKEY = (fmtid: (D1: $8807CAE6;
                                                     D2: $7DB6;
                                                     D3: $4F10;
                                                     D4: ($8E, $E4, $43, $5E, $AA, $13, $92, $BC));
                                                     pid: $0000001); // VT_LPWSTR
  {$EXTERNALSYM PKEY_Pairing_ListItemText}

  PKEY_Pairing_ListItemDescription : PROPERTYKEY = (fmtid: (D1: $8807CAE6;
                                                            D2: $7DB6;
                                                            D3: $4F10;
                                                            D4: ($8E, $E4, $43, $5E, $AA, $13, $92, $BC));
                                                            pid: $0000002); // VT_LPWSTR
  {$EXTERNALSYM PKEY_Pairing_ListItemDescription}

  PKEY_Pairing_ListItemIcon : PROPERTYKEY = (fmtid: (D1: $8807CAE6;
                                                     D2: $7DB6;
                                                     D3: $4F10;
                                                     D4: ($8E, $E4, $43, $5E, $AA, $13, $92, $BC));
                                                     pid: $0000003); // VT_LPWSTR
  {$EXTERNALSYM PKEY_Pairing_ListItemIcon}

  PKEY_Pairing_ListItemDefault : PROPERTYKEY = (fmtid: (D1: $8807CAE6;
                                                        D2: $7DB6;
                                                        D3: $4F10;
                                                        D4: ($8E, $E4, $43, $5E, $AA, $13, $92, $BC));
                                                        pid: $0000004); // VT_BOOL
  {$EXTERNALSYM PKEY_Pairing_ListItemDefault}

  PKEY_Pairing_IsWifiOnlyDevice : PROPERTYKEY = (fmtid: (D1: $8807CAE6;
                                                         D2: $7DB6;
                                                         D3: $4F10;
                                                         D4: ($8E, $E4, $43, $5E, $AA, $13, $92, $BC));
                                                         pid: $0000010); // VT_BOOL
  {$EXTERNALSYM PKEY_Pairing_IsWifiOnlyDevice}


  // DiscoveryMethod values
  DEVICEDISPLAY_DISCOVERYMETHOD_BLUETOOTH = 'Bluetooth';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_BLUETOOTH}
  DEVICEDISPLAY_DISCOVERYMETHOD_BLUETOOTH_LE = 'Bluetooth Low Energy';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_BLUETOOTH_LE}
  DEVICEDISPLAY_DISCOVERYMETHOD_NETBIOS = 'NetBIOS';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_NETBIOS}
  DEVICEDISPLAY_DISCOVERYMETHOD_AD_PRINTER = 'Published Printer';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_AD_PRINTER}
  DEVICEDISPLAY_DISCOVERYMETHOD_PNP = 'PnP';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_PNP}
  DEVICEDISPLAY_DISCOVERYMETHOD_UPNP = 'UPnP';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_UPNP}
  DEVICEDISPLAY_DISCOVERYMETHOD_WSD = 'WSD';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_WSD}
  DEVICEDISPLAY_DISCOVERYMETHOD_WUSB = 'WUSB';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_WUSB}
  DEVICEDISPLAY_DISCOVERYMETHOD_WFD = 'WiFiDirect';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_WFD}
  DEVICEDISPLAY_DISCOVERYMETHOD_ASP_INFRA = 'AspInfra';
  {$EXTERNALSYM DEVICEDISPLAY_DISCOVERYMETHOD_ASP_INFRA}

  //  Name:     System.Device.BIOSVersion -- PKEY_Device_BIOSVersion
  //  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)  Legacy code may treat this as VT_BSTR.
  //  FormatID: EAEE7F1D-6A33-44D1-9441-5F46DEF23198, 9
  PKEY_Device_BIOSVersion : PROPERTYKEY = (fmtid: (D1: $EAEE7F1D;
                                                   D2: $6A33;
                                                   D3: $44D1;
                                                   D4: ($94, $41, $5F, $46, $DE, $F2, $31, $98));
                                                   pid: 9);
  {$EXTERNALSYM PKEY_Device_BIOSVersion}

  PKEY_Write_Time : PROPERTYKEY = (fmtid: (D1: $F53B7E1C;
                                           D2: $77E0;
                                           D3: $4450;
                                           D4: ($8C, $5F, $A7, $6C, $C7, $FD, $E0, $58));
                                           pid: $00000100); //    VT_FILETIME
  {$EXTERNALSYM PKEY_Write_Time}

  PKEY_Create_Time : PROPERTYKEY = (fmtid: (D1: $F53B7E1C;
                                            D2: $77E0;
                                            D3: $4450;
                                            D4: ($8C, $5F, $A7, $6C, $C7, $FD, $E0, $58));
                                            pid: $00000101); //    VT_FILETIME
  {$EXTERNALSYM PKEY_Create_Time}

  PKEY_Device_InstanceId : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                  D2: $104A;
                                                  D3: $4ACA;
                                                  D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                  pid: $00000100); //    VT_LPWSTR
  {$EXTERNALSYM PKEY_Device_InstanceId}

  PKEY_Device_Interface : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                 D2: $104A;
                                                 D3: $4ACA;
                                                 D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                 pid: $00000101); //    VT_CLSID
  {$EXTERNALSYM PKEY_Device_Interface}

  PKEY_ExposedIIDs : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                            D2: $104A;
                                            D3: $4ACA;
                                            D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                            pid: $00003002); //  VT_VECTOR | VT_CLSID
  {$EXTERNALSYM PKEY_ExposedIIDs}

  PKEY_ExposedCLSIDs : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                              D2: $104A;
                                              D3: $4ACA;
                                              D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                              pid: $00003003); //  VT_VECTOR | VT_CLSID
  {$EXTERNALSYM PKEY_ExposedCLSIDs}

  PKEY_InstanceValidatorClsid : PROPERTYKEY = (fmtid: (D1: $78C34FC8;
                                                       D2: $104A;
                                                       D3: $4ACA;
                                                       D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57));
                                                       pid: $00003004); // VT_CLSID
  {$EXTERNALSYM PKEY_InstanceValidatorClsid}

  // FMTID_WSD = {92506491-FF95-4724-A05A-5B81885A7C92}
  FMTID_WSD:  TGUID = (D1: $92506491;
                       D2: $FF95;
                       D3: $4724;
                       D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
   {$EXTERNALSYM FMTID_WSD}

  PKEY_WSD_AddressURI : PROPERTYKEY = (fmtid: (D1: $92506491;
                                               D2: $FF95;
                                               D3: $4724;
                                               D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                               pid: $00001000); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WSD_AddressURI}

  PKEY_WSD_Types : PROPERTYKEY = (fmtid: (D1: $92506491;
                                          D2: $FF95;
                                          D3: $4724;
                                          D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                          pid: $00001001); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WSD_Types}

  PKEY_WSD_Scopes : PROPERTYKEY = (fmtid: (D1: $92506491;
                                           D2: $FF95;
                                           D3: $4724;
                                           D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                           pid: $00001002); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WSD_Scopes}

  PKEY_WSD_MetadataVersion : PROPERTYKEY = (fmtid: (D1: $92506491;
                                                    D2: $FF95;
                                                    D3: $4724;
                                                    D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                                    pid: $00001003); //VT_UI8
  {$EXTERNALSYM PKEY_WSD_MetadataVersion}

  PKEY_WSD_AppSeqInstanceID : PROPERTYKEY = (fmtid: (D1: $92506491;
                                                     D2: $FF95;
                                                     D3: $4724;
                                                     D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                                     pid: $00001004); // VT_UI8
  {$EXTERNALSYM PKEY_WSD_AppSeqInstanceID}

  PKEY_WSD_AppSeqSessionID : PROPERTYKEY = (fmtid: (D1: $92506491;
                                                    D2: $FF95;
                                                    D3: $4724;
                                                    D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                                    pid: $00001005); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WSD_AppSeqSessionID}

  PKEY_WSD_AppSeqMessageNumber : PROPERTYKEY = (fmtid: (D1: $92506491;
                                                        D2: $FF95;
                                                        D3: $4724;
                                                        D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                                        pid: $00001006); // VT_UI8
  {$EXTERNALSYM PKEY_WSD_AppSeqMessageNumber}

  PKEY_WSD_XAddrs : PROPERTYKEY = (fmtid: (D1: $92506491;
                                           D2: $FF95;
                                           D3: $4724;
                                           D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                           pid: $00002000); // VT_LPWSTR or VT_VECTOR | VT_LPWSTR
  {$EXTERNALSYM PKEY_WSD_XAddrs}

  PKEY_WSD_MetadataClean : PROPERTYKEY = (fmtid: (D1: $92506491;
                                                  D2: $FF95;
                                                  D3: $4724;
                                                  D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                                  pid: $00000001); // VT_BOOL
  {$EXTERNALSYM PKEY_WSD_MetadataClean}

  PKEY_WSD_ServiceInfo : PROPERTYKEY = (fmtid: (D1: $92506491;
                                                D2: $FF95;
                                                D3: $4724;
                                                D4: ($A0, $5A, $5B, $81, $88, $5A, $7C, $92));
                                                pid: $00000002); // VT_VECTOR|VT_VARIANT (variants are VT_UNKNOWN)
  {$EXTERNALSYM PKEY_WSD_ServiceInfo}

  PKEY_PUBSVCS_TYPE : PROPERTYKEY = (fmtid: (D1: $F1B88AD3;
                                             D2: $109C;
                                             D3: $4FD2;
                                             D4: ($BA, $3F, $53, $5A, $76, $5F, $82, $F4));
                                             pid: $00005001); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PUBSVCS_TYPE}

  PKEY_PUBSVCS_SCOPE : PROPERTYKEY = (fmtid: (D1: $2AE2B567;
                                              D2: $EECB;
                                              D3: $4A3E;
                                              D4: ($B7, $53, $54, $C7, $25, $49, $43, $66));
                                              pid: $00005002); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PUBSVCS_SCOPE}

  PKEY_PUBSVCS_METADATA : PROPERTYKEY = (fmtid: (D1: $63C6D5B8;
                                                 D2: $F73A;
                                                 D3: $4ACA;
                                                 D4: ($96, $7E, $0C, $C7, $87, $E0, $B5, $59));
                                                 pid: $00005003); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PUBSVCS_METADATA}

  PKEY_PUBSVCS_METADATA_VERSION : PROPERTYKEY = (fmtid: (D1: $C0C96C15;
                                                         D2: $1823;
                                                         D3: $4E5B;
                                                         D4: ($93, $48, $E8, $25, $19, $92, $3F, $04));
                                                         pid: $00005004); // VT_UI8
  {$EXTERNALSYM PKEY_PUBSVCS_METADATA_VERSION}

  PKEY_PUBSVCS_NETWORK_PROFILES_ALLOWED : PROPERTYKEY = (fmtid: (D1: $63C6D5B8;
                                                                 D2: $F73A;
                                                                 D3: $4ACA;
                                                                 D4: ($96, $7E, $0C, $C7, $87, $E0, $B5, $59));
                                                                 pid: $00005005); // VT_VECTOR | VT_LPWSTR
  {$EXTERNALSYM PKEY_PUBSVCS_NETWORK_PROFILES_ALLOWED}

  PKEY_PUBSVCS_NETWORK_PROFILES_DENIED : PROPERTYKEY = (fmtid: (D1: $63C6D5B8;
                                                                D2: $F73A;
                                                                D3: $4ACA;
                                                                D4: ($96, $7E, $0C, $C7, $87, $E0, $B5, $59));
                                                                pid: $00005006); // VT_VECTOR | VT_LPWSTR
  {$EXTERNALSYM PKEY_PUBSVCS_NETWORK_PROFILES_DENIED}

  PKEY_PUBSVCS_NETWORK_PROFILES_DEFAULT : PROPERTYKEY = (fmtid: (D1: $63C6D5B8;
                                                                 D2: $F73A;
                                                                 D3: $4ACA;
                                                                 D4: ($96, $7E, $0C, $C7, $87, $E0, $B5, $59));
                                                                 pid: $00005007); // VT_BOOL
  {$EXTERNALSYM PKEY_PUBSVCS_NETWORK_PROFILES_DEFAULT}
  // FMTID_PNPX = {656A3BB3-ECC0-43FD-8477-4AE0404A96CD}
  FMTID_PNPX:  TGUID = (D1: $656A3BB3;
                        D2: $ECC0;
                        D3: $43FD;
                        D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
  {$EXTERNALSYM FMTID_PNPX}

  // from Discovery messages
  PKEY_PNPX_GlobalIdentity : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                    D2: $ECC0;
                                                    D3: $43FD;
                                                    D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                    pid: $00001000); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_GlobalIdentity}

  PKEY_PNPX_Types : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                           D2: $ECC0;
                                           D3: $43FD;
                                           D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                           pid: $00001001); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_Types}

  PKEY_PNPX_Scopes : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                            D2: $ECC0;
                                            D3: $43FD;
                                            D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                            pid: $00001002); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_Scopes}

  PKEY_PNPX_XAddrs : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                            D2: $ECC0;
                                            D3: $43FD;
                                            D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                            pid: $00001003); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_XAddrs}

  PKEY_PNPX_MetadataVersion : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00001004); // VT_UI8
  {$EXTERNALSYM PKEY_PNPX_MetadataVersion}

  PKEY_PNPX_ID : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                        D2: $ECC0;
                                        D3: $43FD;
                                        D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                        pid: $00001005); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ID}

  // for Directed Discovery
  PKEY_PNPX_RemoteAddress : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                   D2: $ECC0;
                                                   D3: $43FD;
                                                   D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                   pid: $00001006); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_RemoteAddress}
  // for installable ssdp root devices
  PKEY_PNPX_RootProxy : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                               D2: $ECC0;
                                               D3: $43FD;
                                               D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                               pid: $00001007); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_RootProxy}
  // from ThisModel metadata
  PKEY_PNPX_Manufacturer : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                  D2: $ECC0;
                                                  D3: $43FD;
                                                  D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                  pid: $00002000); // Deprecated! Please use PKEY_DeviceDisplay_Manufacturer
  {$EXTERNALSYM PKEY_PNPX_Manufacturer}

  PKEY_PNPX_ManufacturerUrl : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00002001); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ManufacturerUrl}

  PKEY_PNPX_ModelName : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                               D2: $ECC0;
                                               D3: $43FD;
                                               D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                               pid: $00002002); // Deprecated! Please use PKEY_DeviceDisplay_ModelName
  {$EXTERNALSYM PKEY_PNPX_ModelName}

  PKEY_PNPX_ModelNumber : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                 D2: $ECC0;
                                                 D3: $43FD;
                                                 D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                 pid: $00002003); //Depricated! Please use PKEY_DeviceDisplayModelNumber
  {$EXTERNALSYM PKEY_PNPX_ModelNumber}

  PKEY_PNPX_ModelUrl : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                              D2: $ECC0;
                                              D3: $43FD;
                                              D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                              pid: $00002004); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ModelUrl}

  PKEY_PNPX_Upc : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                         D2: $ECC0;
                                         D3: $43FD;
                                         D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                         pid: $00002005); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_Upc}

  PKEY_PNPX_PresentationUrl : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00002006); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_PresentationUrl}
        // from ThisDevice metadata
  PKEY_PNPX_FriendlyName : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                  D2: $ECC0;
                                                  D3: $43FD;
                                                  D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                  pid: $00003000); // Deprecated! Please use PKEY_DeviceDisplay_Name
  {$EXTERNALSYM PKEY_PNPX_FriendlyName}

  PKEY_PNPX_FirmwareVersion : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00003001); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_FirmwareVersion}

  PKEY_PNPX_SerialNumber : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                  D2: $ECC0;
                                                  D3: $43FD;
                                                  D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                  pid: $00003002); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_SerialNumber}

  PKEY_PNPX_DeviceCategory : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                    D2: $ECC0;
                                                    D3: $43FD;
                                                    D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                    pid: $00003004); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_DeviceCategory}
  // for secure devices
  PKEY_PNPX_SecureChannel : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                   D2: $ECC0;
                                                   D3: $43FD;
                                                   D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                   pid: $00007001); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_SecureChannel}

  PKEY_PNPX_CompactSignature : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                      D2: $ECC0;
                                                      D3: $43FD;
                                                      D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                      pid: $00007002); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_CompactSignature}

  PKEY_PNPX_DeviceCertHash : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                    D2: $ECC0;
                                                    D3: $43FD;
                                                    D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                    pid: $00007003); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_DeviceCertHash}

  // DeviceCategory values
  PNPX_DEVICECATEGORY_COMPUTER = 'Computers';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_COMPUTER}
  PNPX_DEVICECATEGORY_INPUTDEVICE = 'Input';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_INPUTDEVICE}
  PNPX_DEVICECATEGORY_PRINTER = 'Printers';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_PRINTER}
  PNPX_DEVICECATEGORY_SCANNER = 'Scanners';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_SCANNER}
  PNPX_DEVICECATEGORY_FAX = 'FAX';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_FAX}
  PNPX_DEVICECATEGORY_MFP = 'MFP';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_MFP}
  PNPX_DEVICECATEGORY_CAMERA = 'Cameras';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_CAMERA}
  PNPX_DEVICECATEGORY_STORAGE = 'Storage';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_STORAGE}
  PNPX_DEVICECATEGORY_NETWORK_INFRASTRUCTURE = 'NetworkInfrastructure';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_NETWORK_INFRASTRUCTURE}
  PNPX_DEVICECATEGORY_DISPLAYS = 'Displays';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_DISPLAYS}
  PNPX_DEVICECATEGORY_MULTIMEDIA_DEVICE = 'MediaDevices';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_MULTIMEDIA_DEVICE}
  PNPX_DEVICECATEGORY_GAMING_DEVICE = 'Gaming';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_GAMING_DEVICE}
  PNPX_DEVICECATEGORY_TELEPHONE = 'Phones';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_TELEPHONE}
  PNPX_DEVICECATEGORY_HOME_AUTOMATION_SYSTEM = 'HomeAutomation';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_HOME_AUTOMATION_SYSTEM}
  PNPX_DEVICECATEGORY_HOME_SECURITY_SYSTEM = 'HomeSecurity';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_HOME_SECURITY_SYSTEM}
  PNPX_DEVICECATEGORY_OTHER = 'Other';
  {$EXTERNALSYM PNPX_DEVICECATEGORY_OTHER}


  PKEY_PNPX_DeviceCategory_Desc : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                         D2: $ECC0;
                                                         D3: $43FD;
                                                         D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                         pid: $00003005); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_DeviceCategory_Desc}

  PKEY_PNPX_Category_Desc_NonPlural : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                             D2: $ECC0;
                                                             D3: $43FD;
                                                             D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                             pid: $00003010); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_Category_Desc_NonPlural}

  PKEY_PNPX_PhysicalAddress : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00003006); // VT_UI1 | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_PhysicalAddress}

  PKEY_PNPX_NetworkInterfaceLuid : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                          D2: $ECC0;
                                                          D3: $43FD;
                                                          D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                          pid: $00003007); // VT_UI8
  {$EXTERNALSYM PKEY_PNPX_NetworkInterfaceLuid}

  PKEY_PNPX_NetworkInterfaceGuid : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                          D2: $ECC0;
                                                          D3: $43FD;
                                                          D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                          pid: $00003008); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_NetworkInterfaceGuid}

  PKEY_PNPX_IpAddress : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                               D2: $ECC0;
                                               D3: $43FD;
                                               D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                               pid: $00003009); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_IpAddress}

  // from Relationship metadata
  PKEY_PNPX_ServiceAddress : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                    D2: $ECC0;
                                                    D3: $43FD;
                                                    D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                    pid: $00004000); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_ServiceAddress}

  PKEY_PNPX_ServiceId : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                               D2: $ECC0;
                                               D3: $43FD;
                                               D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                               pid: $00004001); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ServiceId}

  PKEY_PNPX_ServiceTypes : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                  D2: $ECC0;
                                                  D3: $43FD;
                                                  D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                  pid: $00004002); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_ServiceTypes}

  PKEY_PNPX_ServiceControlUrl : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                       D2: $ECC0;
                                                       D3: $43FD;
                                                       D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                       pid: $4004); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ServiceControlUrl}

  PKEY_PNPX_ServiceDescUrl : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                    D2: $ECC0;
                                                    D3: $43FD;
                                                    D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                    pid: $4005); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ServiceDescUrl}

  PKEY_PNPX_ServiceEventSubUrl : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                        D2: $ECC0;
                                                        D3: $43FD;
                                                        D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                        pid: $4006); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ServiceEventSubUrl}

  // Association DB PKEYs
  PKEY_PNPX_Devnode : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                             D2: $ECC0;
                                             D3: $43FD;
                                             D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                             pid: $00000001); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_Devnode}

  PKEY_PNPX_AssociationState : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                      D2: $ECC0;
                                                      D3: $43FD;
                                                      D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                      pid: $00000002); // VT_UINT
  {$EXTERNALSYM PKEY_PNPX_AssociationState}

  PKEY_PNPX_AssociatedInstanceId : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                          D2: $ECC0;
                                                          D3: $43FD;
                                                          D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                          pid: $00000003); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_AssociatedInstanceId}

  PKEY_PNPX_LastNotificationTime : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                          D2: $ECC0;
                                                          D3: $43FD;
                                                          D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                          pid: $00000004); // VT_DATE
  {$EXTERNALSYM PKEY_PNPX_LastNotificationTime}

  // for Computer Discovery
  PKEY_PNPX_DomainName : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                D2: $ECC0;
                                                D3: $43FD;
                                                D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                pid: $00005000); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_DomainName}

  // Use PKEY_ComputerName (propkey.h) DEFINE_PROPERTYKEY(PKEY_PNPX_MachineName, 0x656A3BB3, 0xECC0, 0x43FD, 0x84, 0x77, 0x4A, 0xE0, 0x40, 0x4A, 0x96, 0xCD, 0x00005001);   // VT_LPWSTR
  PKEY_PNPX_ShareName : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                               D2: $ECC0;
                                               D3: $43FD;
                                               D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                               pid: $00005002); // VT_LPWSTR
  {$EXTERNALSYM PKEY_PNPX_ShareName}

  // SSDP Provider custom properties
  PKEY_SSDP_AltLocationInfo : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00006000); // VT_LPWSTR
  {$EXTERNALSYM PKEY_SSDP_AltLocationInfo}

  PKEY_SSDP_DevLifeTime : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                 D2: $ECC0;
                                                 D3: $43FD;
                                                 D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                 pid: $00006001); // VT_UI4
  {$EXTERNALSYM PKEY_SSDP_DevLifeTime}
  PKEY_SSDP_NetworkInterface : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                      D2: $ECC0;
                                                      D3: $43FD;
                                                      D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                      pid: $00006002); // VT_BOOL
  {$EXTERNALSYM PKEY_SSDP_NetworkInterface}

  // FMTID_PNPXDynamicProperty = {4FC5077E-B686-44BE-93E3-86CAFE368CCD}
  FMTID_PNPXDynamicProperty:  TGUID = (D1: $4FC5077E;
                                       D2: $B686;
                                       D3: $44BE;
                                       D4: ($93, $E3, $86, $CA, $FE, $36, $8C, $CD));
  {$EXTERNALSYM FMTID_PNPXDynamicProperty}

  PKEY_PNPX_Installable : PROPERTYKEY = (fmtid: (D1: $4FC5077E;
                                                 D2: $B686;
                                                 D3: $44BE;
                                                 D4: ($93, $E3, $86, $CA, $FE, $36, $8C, $CD));
                                                 pid: $00000001); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_Installable}

  PKEY_PNPX_Associated : PROPERTYKEY = (fmtid: (D1: $4FC5077E;
                                                D2: $B686;
                                                D3: $44BE;
                                                D4: ($93, $E3, $86, $CA, $FE, $36, $8C, $CD));
                                                pid: $00000002); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_Associated}

  // PKEY_PNPX_Installed is deprecated this PKEY really represents Associated state
  PKEY_PNPX_Installed : PROPERTYKEY = (fmtid: (D1: $4FC5077E;
                                               D2: $B686;
                                               D3: $44BE;
                                               D4: ($93, $E3, $86, $CA, $FE, $36, $8C, $CD));
                                               pid: $00000002); // Deprecated! Please use PKEY_PNPX_Associated
  {$EXTERNALSYM PKEY_PNPX_Installed}

  PKEY_PNPX_CompatibleTypes : PROPERTYKEY = (fmtid: (D1: $4FC5077E;
                                                     D2: $B686;
                                                     D3: $44BE;
                                                     D4: ($93, $E3, $86, $CA, $FE, $36, $8C, $CD));
                                                     pid: $00000003); // VT_LPWSTR | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_CompatibleTypes}

  PKEY_PNPX_InstallState : PROPERTYKEY = (fmtid: (D1: $4FC5077E;
                                                  D2: $B686;
                                                  D3: $44BE;
                                                  D4: ($93, $E3, $86, $CA, $FE, $36, $8C, $CD));
                                                  pid: $00000004); // VT_UI4 | VT_VECTOR
  {$EXTERNALSYM PKEY_PNPX_InstallState}


  PNPX_INSTALLSTATE_NOTINSTALLED = 0; // vector length = 1
  {$EXTERNALSYM PNPX_INSTALLSTATE_NOTINSTALLED}
  PNPX_INSTALLSTATE_INSTALLED = 1; // vector length = 3, CM_Get_DevNode_Status in 2nd and 3rd elements
  {$EXTERNALSYM PNPX_INSTALLSTATE_INSTALLED}
  PNPX_INSTALLSTATE_INSTALLING = 2; // vector length = 1 or 3, CM_Get_DevNode_Status in 2nd and 3rd elements if available
  {$EXTERNALSYM PNPX_INSTALLSTATE_INSTALLING}
  PNPX_INSTALLSTATE_FAILED = 3; // vector length = 3, CM_Get_DevNode_Status in 2nd and 3rd elements
  {$EXTERNALSYM PNPX_INSTALLSTATE_FAILED}

  // Other
  PKEY_PNPX_Removable : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                               D2: $ECC0;
                                               D3: $43FD;
                                               D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                               pid: $00007000); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_Removable}

  PKEY_PNPX_IPBusEnumerated : PROPERTYKEY = (fmtid: (D1: $656A3BB3;
                                                     D2: $ECC0;
                                                     D3: $43FD;
                                                     D4: ($84, $77, $4A, $E0, $40, $4A, $96, $CD));
                                                     pid: $00007010); // VT_BOOL
  {$EXTERNALSYM PKEY_PNPX_IPBusEnumerated}

  // WNET Provider properties
  PKEY_WNET_Scope : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                           D2: $37B3;
                                           D3: $4383;
                                           D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                           pid: $00000001); // VT_UINT
  {$EXTERNALSYM PKEY_WNET_Scope}

  PKEY_WNET_Type : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                          D2: $37B3;
                                          D3: $4383;
                                          D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                          pid: $00000002); // VT_UINT
  {$EXTERNALSYM PKEY_WNET_Type}

  PKEY_WNET_DisplayType : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                                 D2: $37B3;
                                                 D3: $4383;
                                                 D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                                 pid: $00000003); // VT_UINT
  {$EXTERNALSYM PKEY_WNET_DisplayType}

  PKEY_WNET_Usage : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                           D2: $37B3;
                                           D3: $4383;
                                           D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                           pid: $00000004); // VT_UINT
  {$EXTERNALSYM PKEY_WNET_Usage}

  PKEY_WNET_LocalName : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                               D2: $37B3;
                                               D3: $4383;
                                               D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                               pid: $00000005); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WNET_LocalName}

  PKEY_WNET_RemoteName : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                                D2: $37B3;
                                                D3: $4383;
                                                D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                                pid: $00000006); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WNET_RemoteName}

  PKEY_WNET_Comment : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                             D2: $37B3;
                                             D3: $4383;
                                             D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                             pid: $00000007); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WNET_Comment}

  PKEY_WNET_Provider : PROPERTYKEY = (fmtid: (D1: $DEBDA43A;
                                              D2: $37B3;
                                              D3: $4383;
                                              D4: ($91, $E7, $44, $98, $DA, $29, $95, $AB));
                                              pid: $00000008); // VT_LPWSTR
  {$EXTERNALSYM PKEY_WNET_Provider}

  // WCN Provider properties
  PKEY_WCN_Version : PROPERTYKEY = (fmtid: (D1: $88190B80;
                                            D2: $4684;
                                            D3: $11DA;
                                            D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                            pid: $00000001); // VT_UI1
  {$EXTERNALSYM PKEY_WCN_Version}

  PKEY_WCN_RequestType : PROPERTYKEY = (fmtid: (D1: $88190B81;
                                                D2: $4684;
                                                D3: $11DA;
                                                D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                pid: $00000002); // VT_INT
  {$EXTERNALSYM PKEY_WCN_RequestType}

  PKEY_WCN_AuthType : PROPERTYKEY = (fmtid: (D1: $88190B82;
                                             D2: $4684;
                                             D3: $11DA;
                                             D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                             pid: $00000003); // VT_INT
  {$EXTERNALSYM PKEY_WCN_AuthType}

  PKEY_WCN_EncryptType : PROPERTYKEY = (fmtid: (D1: $88190B83;
                                                D2: $4684;
                                                D3: $11DA;
                                                D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                pid: $00000004); // VT_INT
  {$EXTERNALSYM PKEY_WCN_EncryptType}

  PKEY_WCN_ConnType : PROPERTYKEY = (fmtid: (D1: $88190B84;
                                             D2: $4684;
                                             D3: $11DA;
                                             D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                             pid: $00000005); // VT_INT
  {$EXTERNALSYM PKEY_WCN_ConnType}

  PKEY_WCN_ConfigMethods : PROPERTYKEY = (fmtid: (D1: $88190B85;
                                                  D2: $4684;
                                                  D3: $11DA;
                                                  D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                  pid: $00000006); // VT_INT
  {$EXTERNALSYM PKEY_WCN_ConfigMethods}

  // map WCN DeviceType to PKEY_PNPX_DeviceCategory
  //DEFINE_PROPERTYKEY(PKEY_WCN_DeviceType, 0x88190b86, 0x4684, 0x11da, 0xa2, 0x6a, 0x00, 0x02, 0xb3, 0x98, 0x8e, 0x81, 0x00000007); // VT_INT
  PKEY_WCN_RfBand : PROPERTYKEY = (fmtid: (D1: $88190B87;
                                           D2: $4684;
                                           D3: $11DA;
                                           D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                           pid: $00000008); // VT_INT
  {$EXTERNALSYM PKEY_WCN_RfBand}

  PKEY_WCN_AssocState : PROPERTYKEY = (fmtid: (D1: $88190B88;
                                               D2: $4684;
                                               D3: $11DA;
                                               D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                               pid: $00000009); // VT_INT
  {$EXTERNALSYM PKEY_WCN_AssocState}

  PKEY_WCN_ConfigError : PROPERTYKEY = (fmtid: (D1: $88190B89;
                                                D2: $4684;
                                                D3: $11DA;
                                                D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                pid: $0000000A); // VT_INT
  {$EXTERNALSYM PKEY_WCN_ConfigError}

  PKEY_WCN_ConfigState : PROPERTYKEY = (fmtid: (D1: $88190B89;
                                                D2: $4684;
                                                D3: $11DA;
                                                D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                pid: $0000000B); // VT_UI1
  {$EXTERNALSYM PKEY_WCN_ConfigState}

  PKEY_WCN_DevicePasswordId : PROPERTYKEY = (fmtid: (D1: $88190B89;
                                                     D2: $4684;
                                                     D3: $11DA;
                                                     D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                     pid: $0000000C); // VT_INT
  {$EXTERNALSYM PKEY_WCN_DevicePasswordId}

  PKEY_WCN_OSVersion : PROPERTYKEY = (fmtid: (D1: $88190B89;
                                              D2: $4684;
                                              D3: $11DA;
                                              D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                              pid: $0000000D); // VT_UINT
  {$EXTERNALSYM PKEY_WCN_OSVersion}

  PKEY_WCN_VendorExtension : PROPERTYKEY = (fmtid: (D1: $88190B8A;
                                                    D2: $4684;
                                                    D3: $11DA;
                                                    D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                    pid: $0000000E); // VT_UI1 | VT_VECTOR
  {$EXTERNALSYM PKEY_WCN_VendorExtension}

  PKEY_WCN_RegistrarType : PROPERTYKEY = (fmtid: (D1: $88190B8B;
                                                  D2: $4684;
                                                  D3: $11DA;
                                                  D4: ($A2, $6A, $00, $02, $B3, $98, $8E, $81));
                                                  pid: $0000000F); // VT_INT
  {$EXTERNALSYM PKEY_WCN_RegistrarType}

  //-----------------------------------------------------------------------------
  // DriverPackage properties
  // #define PKEY_DriverPackage_Model PKEY_DrvPkg_Model
  // #define PKEY_DriverPackage_VendorWebSite PKEY_DrvPkg_VendorWebSite
  // #define PKEY_DriverPackage_DetailedDescription PKEY_DrvPkg_DetailedDescription
  // #define PKEY_DriverPackage_DocumentationLink PKEY_DrvPkg_DocumentationLink
  // #define PKEY_DriverPackage_Icon PKEY_DrvPkg_Icon
  // #define PKEY_DriverPackage_BrandingIcon PKEY_DrvPkg_BrandingIcon
  //-----------------------------------------------------------------------------

  // Hardware properties
  PKEY_Hardware_Devinst : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                 D2: $E0CA;
                                                 D3: $4598;
                                                 D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                 pid: 4097);
  {$EXTERNALSYM PKEY_Hardware_Devinst}

  //  Name:     System.Hardware.DisplayAttribute -- PKEY_Hardware_DisplayAttribute
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 5
  PKEY_Hardware_DisplayAttribute : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                          D2: $E0CA;
                                                          D3: $4598;
                                                          D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                          pid: 5);
  {$EXTERNALSYM PKEY_Hardware_DisplayAttribute}

  //  Name:     System.Hardware.DriverDate -- PKEY_Hardware_DriverDate
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 11
  PKEY_Hardware_DriverDate : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                    D2: $E0CA;
                                                    D3: $4598;
                                                    D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                    pid: 11);
  {$EXTERNALSYM PKEY_Hardware_DriverDate}


  //  Name:     System.Hardware.DriverProvider -- PKEY_Hardware_DriverProvider
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 10
  PKEY_Hardware_DriverProvider : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                        D2: $E0CA;
                                                        D3: $4598;
                                                        D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                        pid: 10);
  {$EXTERNALSYM PKEY_Hardware_DriverProvider}

  //  Name:     System.Hardware.DriverVersion -- PKEY_Hardware_DriverVersion
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 9
  PKEY_Hardware_DriverVersion : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                       D2: $E0CA;
                                                       D3: $4598;
                                                       D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                       pid: 9);
  {$EXTERNALSYM PKEY_Hardware_DriverVersion}

  //  Name:     System.Hardware.Function -- PKEY_Hardware_Function
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 4099
  PKEY_Hardware_Function : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                  D2: $E0CA;
                                                  D3: $4598;
                                                  D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                  pid: 4099);
  {$EXTERNALSYM PKEY_Hardware_Function}

  //  Name:     System.Hardware.Icon -- PKEY_Hardware_Icon
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 3
  PKEY_Hardware_Icon : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                              D2: $E0CA;
                                              D3: $4598;
                                              D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                              pid: 3);
  {$EXTERNALSYM PKEY_Hardware_Icon}

  //  Name:     System.Hardware.Image -- PKEY_Hardware_Image
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 4098
  PKEY_Hardware_Image : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                               D2: $E0CA;
                                               D3: $4598;
                                               D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                               pid: 4098);
  {$EXTERNALSYM PKEY_Hardware_Image}

  //  Name:     System.Hardware.Manufacturer -- PKEY_Hardware_Manufacturer
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 6
  PKEY_Hardware_Manufacturer : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                      D2: $E0CA;
                                                      D3: $4598;
                                                      D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                      pid: 6);
  {$EXTERNALSYM PKEY_Hardware_Manufacturer}

  //  Name:     System.Hardware.Model -- PKEY_Hardware_Model
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 7
  PKEY_Hardware_Model : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                               D2: $E0CA;
                                               D3: $4598;
                                               D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                               pid: 7);
  {$EXTERNALSYM PKEY_Hardware_Model}

  //  Name:     System.Hardware.Name -- PKEY_Hardware_Name
  //  Type:     String -- VT_LPWSTR  (For variants: VT_BSTR)
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 2
  PKEY_Hardware_Name : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                              D2: $E0CA;
                                              D3: $4598;
                                              D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                              pid: 2);
  {$EXTERNALSYM PKEY_Hardware_Name}

  //  Name:     System.Hardware.SerialNumber -- PKEY_Hardware_SerialNumber
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 8
  PKEY_Hardware_SerialNumber : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                      D2: $E0CA;
                                                      D3: $4598;
                                                      D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                      pid: 8);
  {$EXTERNALSYM PKEY_Hardware_SerialNumber}

  //  Name:     System.Hardware.ShellAttributes -- PKEY_Hardware_ShellAttributes
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 4100
  PKEY_Hardware_ShellAttributes : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                         D2: $E0CA;
                                                         D3: $4598;
                                                         D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                         pid: 4100);
  {$EXTERNALSYM PKEY_Hardware_ShellAttributes}

  //  Name:     System.Hardware.Status -- PKEY_Hardware_Status
  //  Type:     Unspecified -- VT_NULL
  //  FormatID: 5EAF3EF2-E0CA-4598-BF06-71ED1D9DD953, 4096
  PKEY_Hardware_Status : PROPERTYKEY = (fmtid: (D1: $5EAF3EF2;
                                                D2: $E0CA;
                                                D3: $4598;
                                                D4: ($BF, $06, $71, $ED, $1D, $9D, $D9, $53));
                                                pid: 4096);
  {$EXTERNALSYM PKEY_Hardware_Status}


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.

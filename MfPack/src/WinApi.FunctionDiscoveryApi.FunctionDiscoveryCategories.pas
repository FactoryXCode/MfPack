//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.FunctionDiscoveryApi.FunctionDiscoveryCategories.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: - This unit intentionally declares both ANSI and Unicode entry points.
//          - Delphi applications should normally use the explicit W versions.
//          - The unsuffixed helper aliases at the bottom map to the Unicode versions.
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
// Source: functiondiscoverycategories.h
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
unit WinApi.FunctionDiscoveryApi.FunctionDiscoveryCategories;

 {$HPPEMIT '#include "functiondiscoverycategories.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

const

  // *****************************************************************************
  // Important:  Anything added here should also be added to FunctionDiscoveryManagedKeys.h
  // *****************************************************************************

  FD_SUBKEY                               = 'SOFTWARE\Microsoft\Function Discovery\';
  {$EXTERNALSYM FD_SUBKEY}
  FD_SUBKEY_CATEGORIES                    = FD_SUBKEY + 'Categories\';
  {$EXTERNALSYM FD_SUBKEY_CATEGORIES}

  // *****************************************************************************
  // Function Discovery Categories
  // *****************************************************************************
  // Important:  Anything added here should also be added to FunctionDiscoveryManagedKeys.h
  // *****************************************************************************

  // Provider Categories
  // Windows Vista
  FCTN_CATEGORY_PNP                       = 'Provider\Microsoft.Base.PnP';
  {$EXTERNALSYM FCTN_CATEGORY_PNP}
  FCTN_CATEGORY_REGISTRY                  = 'Provider\Microsoft.Base.Registry';
  {$EXTERNALSYM FCTN_CATEGORY_REGISTRY}
  FCTN_CATEGORY_SSDP                      = 'Provider\Microsoft.Networking.SSDP';
  {$EXTERNALSYM FCTN_CATEGORY_SSDP}
  FCTN_CATEGORY_WSDISCOVERY               = 'Provider\Microsoft.Networking.WSD';
  {$EXTERNALSYM FCTN_CATEGORY_WSDISCOVERY}
  FCTN_CATEGORY_NETBIOS                   = 'Provider\Microsoft.Networking.Netbios';
  {$EXTERNALSYM FCTN_CATEGORY_NETBIOS}
  FCTN_CATEGORY_WCN                       = 'Provider\Microsoft.Networking.WCN';
  {$EXTERNALSYM FCTN_CATEGORY_WCN}
  FCTN_CATEGORY_PUBLICATION               = 'Provider\Microsoft.Base.Publication';
  {$EXTERNALSYM FCTN_CATEGORY_PUBLICATION}
  FCTN_CATEGORY_PNPXASSOCIATION           = 'Provider\Microsoft.PnPX.Association';
  {$EXTERNALSYM FCTN_CATEGORY_PNPXASSOCIATION}

  // Wireless Update Release
  FCTN_CATEGORY_BT                        = 'Provider\Microsoft.Devices.Bluetooth';
  {$EXTERNALSYM FCTN_CATEGORY_BT}
  FCTN_CATEGORY_WUSB                      = 'Provider\Microsoft.Devices.WirelessUSB';
  {$EXTERNALSYM FCTN_CATEGORY_WUSB}
  FCTN_CATEGORY_DEVICEDISPLAYOBJECTS      = 'Provider\Microsoft.Base.DeviceDisplayObjects';
  {$EXTERNALSYM FCTN_CATEGORY_DEVICEDISPLAYOBJECTS}
  FCTN_CATEGORY_DEVQUERYOBJECTS           = 'Provider\Microsoft.Base.DevQueryObjects';
  {$EXTERNALSYM FCTN_CATEGORY_DEVQUERYOBJECTS}

  // Layered Categories
  // Windows Vista
  FCTN_CATEGORY_NETWORKDEVICES            = 'Layered\Microsoft.Networking.Devices';
  {$EXTERNALSYM FCTN_CATEGORY_NETWORKDEVICES}
  FCTN_CATEGORY_DEVICES                   = 'Layered\Microsoft.Base.Devices';
  {$EXTERNALSYM FCTN_CATEGORY_DEVICES}
  FCTN_CATEGORY_DEVICEFUNCTIONENUMERATORS = 'Layered\Microsoft.Devices.FunctionEnumerators';
  {$EXTERNALSYM FCTN_CATEGORY_DEVICEFUNCTIONENUMERATORS}
  FCTN_CATEGORY_DEVICEPAIRING             = 'Layered\Microsoft.Base.DevicePairing';
  {$EXTERNALSYM FCTN_CATEGORY_DEVICEPAIRING}

  // *****************************************************************************
  // Function Discovery SubCategories
  // *****************************************************************************
  // Important:  Anything added here should also be added to FunctionDiscoveryManagedKeys.h
  // *****************************************************************************

  // Subcategories of Devices FCTN_CATEGORY_DEVICES
  FCTN_SUBCAT_DEVICES_WSDPRINTERS         = 'WSDPrinters';
  {$EXTERNALSYM FCTN_SUBCAT_DEVICES_WSDPRINTERS}

  // Subcategories of Devices FCTN_CATEGORY_NETWORKDEVICES
  FCTN_SUBCAT_NETWORKDEVICES_SSDP         = 'SSDP';
  {$EXTERNALSYM FCTN_SUBCAT_NETWORKDEVICES_SSDP}
  FCTN_SUBCAT_NETWORKDEVICES_WSD          = 'WSD';
  {$EXTERNALSYM FCTN_SUBCAT_NETWORKDEVICES_WSD}

  // Subcategories of Registry
  FCTN_SUBCAT_REG_PUBLICATION             = 'Publication';
  {$EXTERNALSYM FCTN_SUBCAT_REG_PUBLICATION}
  FCTN_SUBCAT_REG_DIRECTED                = 'Directed';
  {$EXTERNALSYM FCTN_SUBCAT_REG_DIRECTED}

  // *****************************************************************************
  // Important:  Anything added here should also be added to FunctionDiscoveryManagedKeys.h
  // *****************************************************************************


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.

//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.FunctionDiscoveryApi.FunctionDiscoveryConstraints.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 3.2.0
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
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: functiondiscoveryconstraints.h
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
unit WinApi.FunctionDiscoveryApi.FunctionDiscoveryConstraints;

 {$HPPEMIT '#include "functiondiscoveryconstraints.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

  {$MINENUMSIZE 4}

const

////////////////////////////////////////////////////////////////////////////////
// QUERY Constraint defines
////////////////////////////////////////////////////////////////////////////////

  MAX_FDCONSTRAINTNAME_LENGTH  = 100;
  {$EXTERNALSYM MAX_FDCONSTRAINTNAME_LENGTH}
  MAX_FDCONSTRAINTVALUE_LENGTH = 1000;
  {$EXTERNALSYM MAX_FDCONSTRAINTVALUE_LENGTH}

  // Common Provider specific Constraints
  FD_QUERYCONSTRAINT_PROVIDERINSTANCEID  = 'ProviderInstanceID';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_PROVIDERINSTANCEID}
  FD_QUERYCONSTRAINT_SUBCATEGORY         = 'Subcategory';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_SUBCATEGORY}
  FD_QUERYCONSTRAINT_RECURSESUBCATEGORY  = 'RecurseSubcategory';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_RECURSESUBCATEGORY}
  FD_QUERYCONSTRAINT_VISIBILITY          = 'Visibility';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_VISIBILITY}

  // FD_CONSTRAINTVALUE_VISIBILITY_DEFAULT you want just default instances
  // (visible as defined by the provider)
  //
  // FD_CONSTRAINTVALUE_VISIBILITY_ALL (default) you want both visible
  // and not visible/hidden instances (as defined by the provider)

  FD_QUERYCONSTRAINT_COMCLSCONTEXT       = 'COMClsContext';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_COMCLSCONTEXT}
  FD_QUERYCONSTRAINT_ROUTINGSCOPE        = 'RoutingScope';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_ROUTINGSCOPE}

  // Common Provider specific Constraints values
  FD_CONSTRAINTVALUE_TRUE                        = 'TRUE';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_TRUE}
  FD_CONSTRAINTVALUE_FALSE                       = 'FALSE';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_FALSE}
  FD_CONSTRAINTVALUE_RECURSESUBCATEGORY_TRUE     = FD_CONSTRAINTVALUE_TRUE;
  {$EXTERNALSYM FD_CONSTRAINTVALUE_RECURSESUBCATEGORY_TRUE}
  FD_CONSTRAINTVALUE_VISIBILITY_DEFAULT          = '0';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_VISIBILITY_DEFAULT}
  FD_CONSTRAINTVALUE_VISIBILITY_ALL              = '1';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_VISIBILITY_ALL}
  FD_CONSTRAINTVALUE_COMCLSCONTEXT_INPROC_SERVER = '1';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_COMCLSCONTEXT_INPROC_SERVER}
  FD_CONSTRAINTVALUE_COMCLSCONTEXT_LOCAL_SERVER  = '4';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_COMCLSCONTEXT_LOCAL_SERVER}

  FD_CONSTRAINTVALUE_PAIRED    = 'Paired';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_PAIRED}
  FD_CONSTRAINTVALUE_UNPAIRED  = 'UnPaired';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_UNPAIRED}
  FD_CONSTRAINTVALUE_ALL       = 'All';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_ALL}

  FD_CONSTRAINTVALUE_ROUTINGSCOPE_ALL    = 'All';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_ROUTINGSCOPE_ALL}
  FD_CONSTRAINTVALUE_ROUTINGSCOPE_DIRECT = 'Direct';
  {$EXTERNALSYM FD_CONSTRAINTVALUE_ROUTINGSCOPE_DIRECT}

////////////////////////////////////////////////////////////////////////////////
// Provider inquiry constraints
////////////////////////////////////////////////////////////////////////////////

  FD_QUERYCONSTRAINT_PAIRING_STATE   = 'PairingState';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_PAIRING_STATE}

  // if unset, provider default is FD_CONSTRAINTVALUE_PAIRED
  // FD_CONSTRAINTVALUE_PAIRED will return all paired devices
  // FD_CONSTRAINTVALUE_UNPAIRED will return all unpaired devices
  // within wireless or wired range
  // FD_CONSTRAINTVALUE_ALL will return all devices cached and
  // within wireless or wired range

  FD_QUERYCONSTRAINT_INQUIRY_TIMEOUT = 'InquiryModeTimeout';
  {$EXTERNALSYM FD_QUERYCONSTRAINT_INQUIRY_TIMEOUT}

  // #seconds 6-600 supported, default is 300

////////////////////////////////////////////////////////////////////////////////
// PNP Provider specific Constraints
////////////////////////////////////////////////////////////////////////////////

  PROVIDERPNP_QUERYCONSTRAINT_INTERFACECLASS    = 'InterfaceClass';
  {$EXTERNALSYM PROVIDERPNP_QUERYCONSTRAINT_INTERFACECLASS}
  PROVIDERPNP_QUERYCONSTRAINT_NOTPRESENT        = 'NotPresent';
  {$EXTERNALSYM PROVIDERPNP_QUERYCONSTRAINT_NOTPRESENT}
  PROVIDERPNP_QUERYCONSTRAINT_NOTIFICATIONSONLY = 'NotifyOnly';
  {$EXTERNALSYM PROVIDERPNP_QUERYCONSTRAINT_NOTIFICATIONSONLY}

  // PNP_CONSTRAINTVALUE_NOTPRESENT you want "not present"
  // instances as well
  //
  // "FALSE" (default) you want only DIGCF_PRESENT instances.

  // PNP Provider specific Constraints values
  PNP_CONSTRAINTVALUE_NOTPRESENT        = FD_CONSTRAINTVALUE_TRUE;
  {$EXTERNALSYM PNP_CONSTRAINTVALUE_NOTPRESENT}
  PNP_CONSTRAINTVALUE_NOTIFICATIONSONLY = FD_CONSTRAINTVALUE_TRUE;
  {$EXTERNALSYM PNP_CONSTRAINTVALUE_NOTIFICATIONSONLY}

////////////////////////////////////////////////////////////////////////////////
// SSDP Provider specific Constraints
////////////////////////////////////////////////////////////////////////////////

  PROVIDERSSDP_QUERYCONSTRAINT_TYPE              = 'Type';
  {$EXTERNALSYM PROVIDERSSDP_QUERYCONSTRAINT_TYPE}
  PROVIDERSSDP_QUERYCONSTRAINT_CUSTOMXMLPROPERTY = 'CustomXmlProperty';
  {$EXTERNALSYM PROVIDERSSDP_QUERYCONSTRAINT_CUSTOMXMLPROPERTY}

  // SSDP Provider specific Constraints values
  SSDP_CONSTRAINTVALUE_TYPE_ALL           = 'ssdp:all';
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_ALL}
  SSDP_CONSTRAINTVALUE_TYPE_ROOT          = 'upnp:rootdevice';
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_ROOT}
  SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX = 'urn:schemas-upnp-org:device:';
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}
  SSDP_CONSTRAINTVALUE_TYPE_SVC_PREFIX    = 'urn:schemas-upnp-org:service:';
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_SVC_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_LIGHTING =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_LIGHTING}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'Lighting:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_REMINDER =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_REMINDER}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'Reminder:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_POWERDEVICE =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_POWERDEVICE}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'PowerDevice:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_IGD =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_IGD}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'InternetGatewayDevice:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_WANDEVICE =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_WANDEVICE}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'WANDevice:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_LANDEVICE =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_LANDEVICE}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'LANDevice:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_WANCONNDEVICE =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_WANCONNDEVICE}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'WANConnectionDevice:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}

  SSDP_CONSTRAINTVALUE_TYPE_DEV_LUXMETER =
  {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEV_LUXMETER}
    SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX + 'Luxmeter:1';
    {$EXTERNALSYM SSDP_CONSTRAINTVALUE_TYPE_DEVICE_PREFIX}


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.

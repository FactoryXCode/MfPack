// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.OpmApi.pas
// Kind: Pascal Unit
// Release date: 23-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
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
// Source: opmapi.h
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
unit WinApi.MediaFoundationApi.OpmApi;

  {$HPPEMIT '#include "opmapi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {DirectX (Clootie)}
  WinApi.DirectX.D3D9, {Use WinApi, MfPack or Clootie Dx}
  WinApi.DirectX.DxVa2Api;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

//==============================================================================
// Description:
//
//  OPM Information GUIDs. These GUIDs are used by IOPMVideoOutput.GetInformation()
//==============================================================================

const

  OPM_GET_CURRENT_HDCP_SRM_VERSION               : TGUID = '{99c5ceff-5f1d-4879-81c1-c52443c9482b}';
  {$EXTERNALSYM OPM_GET_CURRENT_HDCP_SRM_VERSION}
  // Description:
  //
  //  OPM Information GUIDs.  These GUIDs are used by
  //  IOPMVideoOutput.COPPCompatibleGetInformation().
  //
  OPM_GET_CONNECTED_HDCP_DEVICE_INFORMATION      : TGUID = '{0db59d74-a992-492e-a0bd-c23fda564e00}';
  {$EXTERNALSYM OPM_GET_CONNECTED_HDCP_DEVICE_INFORMATION}
  OPM_GET_ACP_AND_CGMSA_SIGNALING                : TGUID = '{6629a591-3b79-4cf3-924a-11e8e7811671}';
  {$EXTERNALSYM OPM_GET_ACP_AND_CGMSA_SIGNALING}
  // Description:
  //
  //  OPM Information GUIDs.  These GUIDs are used by IOPMVideoOutput.GetInformation()
  //  and IOPMVideoOutput.COPPCompatibleGetInformation().
  //
  OPM_GET_CONNECTOR_TYPE                         : TGUID = '{81d0bfd5-6afe-48c2-99c0-95a08f97c5da}';
  {$EXTERNALSYM OPM_GET_CONNECTOR_TYPE}
  OPM_GET_SUPPORTED_PROTECTION_TYPES             : TGUID = '{38f2a801-9a6c-48bb-9107-b6696e6f1797}';
  {$EXTERNALSYM OPM_GET_SUPPORTED_PROTECTION_TYPES}
  OPM_GET_VIRTUAL_PROTECTION_LEVEL               : TGUID = '{b2075857-3eda-4d5d-88db-748f8c1a0549}';
  {$EXTERNALSYM OPM_GET_VIRTUAL_PROTECTION_LEVEL}
  OPM_GET_ACTUAL_PROTECTION_LEVEL                : TGUID = '{1957210a-7766-452a-b99a-d27aed54f03a}';
  {$EXTERNALSYM OPM_GET_ACTUAL_PROTECTION_LEVEL}
  OPM_GET_ACTUAL_OUTPUT_FORMAT                   : TGUID = '{d7bf1ba3-ad13-4f8e-af98-0dcb3ca204cc}';
  {$EXTERNALSYM OPM_GET_ACTUAL_OUTPUT_FORMAT}
  OPM_GET_ADAPTER_BUS_TYPE                       : TGUID = '{c6f4d673-6174-4184-8e35-f6db5200bcba}';
  {$EXTERNALSYM OPM_GET_ADAPTER_BUS_TYPE}
  OPM_GET_OUTPUT_ID                              : TGUID = '{72cb6df3-244f-40ce-b09e-20506af6302f}';
  {$EXTERNALSYM OPM_GET_OUTPUT_ID}
  OPM_GET_DVI_CHARACTERISTICS                    : TGUID = '{a470b3bb-5dd7-4172-839c-3d3776e0ebf5}';
  {$EXTERNALSYM OPM_GET_DVI_CHARACTERISTICS}
  OPM_GET_CODEC_INFO                             : TGUID = '{4f374491-8f5f-4445-9dba-95588f6b58b4}';
  {$EXTERNALSYM OPM_GET_CODEC_INFO}
  OPM_GET_OUTPUT_HARDWARE_PROTECTION_SUPPORT     : TGUID = '{3b129589-2af8-4ef0-96a2-704a845a218e}';
  {$EXTERNALSYM OPM_GET_OUTPUT_HARDWARE_PROTECTION_SUPPORT}
  // Description:
  //
  //  OPM Configuration GUIDs.  These GUIDs are used by IOPMVideoOutput.Configure().
  //
  OPM_SET_PROTECTION_LEVEL                       : TGUID = '{9bb9327c-4eb5-4727-9f00-b42b0919c0da}';
  {$EXTERNALSYM OPM_SET_PROTECTION_LEVEL}
  OPM_SET_ACP_AND_CGMSA_SIGNALING                : TGUID = '{09a631a5-d684-4c60-8e4d-d3bb0f0be3ee}';
  {$EXTERNALSYM OPM_SET_ACP_AND_CGMSA_SIGNALING}
  OPM_SET_HDCP_SRM                               : TGUID = '{8b5ef5d1-c30d-44ff-84a5-ea71dce78f13}';
  {$EXTERNALSYM OPM_SET_HDCP_SRM}
  OPM_SET_PROTECTION_LEVEL_ACCORDING_TO_CSS_DVD  : TGUID = '{39ce333e-4cc0-44ae-bfcc-da50b5f82e72}';
  {$EXTERNALSYM OPM_SET_PROTECTION_LEVEL_ACCORDING_TO_CSS_DVD}

  // Description:
  //
  //  Constants used by the OPM API
  //

  OPM_OMAC_SIZE                                = 16;
  {$EXTERNALSYM OPM_OMAC_SIZE}
  OPM_128_BIT_RANDOM_NUMBER_SIZE               = 16;
  {$EXTERNALSYM OPM_128_BIT_RANDOM_NUMBER_SIZE}
  OPM_ENCRYPTED_INITIALIZATION_PARAMETERS_SIZE = 256;
  {$EXTERNALSYM OPM_ENCRYPTED_INITIALIZATION_PARAMETERS_SIZE}
  OPM_CONFIGURE_SETTING_DATA_SIZE              = 4056;
  {$EXTERNALSYM OPM_CONFIGURE_SETTING_DATA_SIZE}
  OPM_GET_INFORMATION_PARAMETERS_SIZE          = 4056;
  {$EXTERNALSYM OPM_GET_INFORMATION_PARAMETERS_SIZE}
  OPM_REQUESTED_INFORMATION_SIZE               = 4076;
  {$EXTERNALSYM OPM_REQUESTED_INFORMATION_SIZE}
  OPM_HDCP_KEY_SELECTION_VECTOR_SIZE           = 5;
  {$EXTERNALSYM OPM_HDCP_KEY_SELECTION_VECTOR_SIZE}
  OPM_PROTECTION_TYPE_SIZE                     = 4;
  {$EXTERNALSYM OPM_PROTECTION_TYPE_SIZE}
  OPM_BUS_TYPE_MASK                            = $FFFF;
  {$EXTERNALSYM OPM_BUS_TYPE_MASK}
  OPM_BUS_IMPLEMENTATION_MODIFIER_MASK         = $7FFF;
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_MASK}


type

  // Description:
  //
  //  Enumerations used by OPMGetVideoOutputFromHMONITOR() and
  //  OPMGetVideoOutputFromIDirect3DDevice9Object().
  //
  POPM_VIDEO_OUTPUT_SEMANTICS = ^OPM_VIDEO_OUTPUT_SEMANTICS;
  _OPM_VIDEO_OUTPUT_SEMANTICS = (
    OPM_VOS_COPP_SEMANTICS        = 0,
    OPM_VOS_OPM_SEMANTICS	        = 1,
    OPM_VOS_OPM_INDIRECT_DISPLAY	= 2
  );
  {$EXTERNALSYM _OPM_VIDEO_OUTPUT_SEMANTICS}
  OPM_VIDEO_OUTPUT_SEMANTICS = _OPM_VIDEO_OUTPUT_SEMANTICS;
  {$EXTERNALSYM OPM_VIDEO_OUTPUT_SEMANTICS}


  // Description:
  //
  //  Enumerations used by IOPMVideoOutput.COPPCompatibleGetInformation()
  //

const

  OPM_HDCP_FLAG_NONE     = 0;
  {$EXTERNALSYM OPM_HDCP_FLAG_NONE}
  OPM_HDCP_FLAG_REPEATER = $1;
  {$EXTERNALSYM OPM_HDCP_FLAG_REPEATER}

  // Description:
  //
  //  Enumerations used by IOPMVideoOutput.GetInformation() and
  //  IOPMVideoOutput.COPPCompatibleGetInformation()
  //
  OPM_STATUS_NORMAL                       = 0;
  {$EXTERNALSYM OPM_STATUS_NORMAL}
  OPM_STATUS_LINK_LOST                    = $01;
  {$EXTERNALSYM OPM_STATUS_LINK_LOST}
  OPM_STATUS_RENEGOTIATION_REQUIRED       = $02;
  {$EXTERNALSYM OPM_STATUS_RENEGOTIATION_REQUIRED}
  OPM_STATUS_TAMPERING_DETECTED           = $04;
  {$EXTERNALSYM OPM_STATUS_TAMPERING_DETECTED}
  OPM_STATUS_REVOKED_HDCP_DEVICE_ATTACHED = $08;
  {$EXTERNALSYM OPM_STATUS_REVOKED_HDCP_DEVICE_ATTACHED}


  OPM_CONNECTOR_TYPE_OTHER                    = Integer(-1);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_OTHER}
  OPM_CONNECTOR_TYPE_VGA                      = Integer(0);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_VGA}
  OPM_CONNECTOR_TYPE_SVIDEO                   = Integer(1);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_SVIDEO}
  OPM_CONNECTOR_TYPE_COMPOSITE_VIDEO          = Integer(2);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_COMPOSITE_VIDEO}
  OPM_CONNECTOR_TYPE_COMPONENT_VIDEO          = Integer(3);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_COMPONENT_VIDEO}
  OPM_CONNECTOR_TYPE_DVI                      = Integer(4);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_DVI}
  OPM_CONNECTOR_TYPE_HDMI                     = Integer(5);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_HDMI}
  OPM_CONNECTOR_TYPE_LVDS                     = Integer(6);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_LVDS}
  OPM_CONNECTOR_TYPE_D_JPN                    = Integer(8);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_D_JPN}
  OPM_CONNECTOR_TYPE_SDI                      = Integer(9);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_SDI}
  OPM_CONNECTOR_TYPE_DISPLAYPORT_EXTERNAL     = Integer(10);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_DISPLAYPORT_EXTERNAL}
  OPM_CONNECTOR_TYPE_DISPLAYPORT_EMBEDDED     = Integer(11);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_DISPLAYPORT_EMBEDDED}
  OPM_CONNECTOR_TYPE_UDI_EXTERNAL             = Integer(12);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_UDI_EXTERNAL}
  OPM_CONNECTOR_TYPE_UDI_EMBEDDED             = Integer(13);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_UDI_EMBEDDED}
  OPM_CONNECTOR_TYPE_RESERVED	                = Integer(14);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_RESERVED}
  OPM_CONNECTOR_TYPE_MIRACAST	                = Integer(15);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_MIRACAST}
  OPM_CONNECTOR_TYPE_TRANSPORT_AGNOSTIC_DIGITAL_MODE_A	= Integer(16);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_TRANSPORT_AGNOSTIC_DIGITAL_MODE_A}
  OPM_CONNECTOR_TYPE_TRANSPORT_AGNOSTIC_DIGITAL_MODE_B	= Integer(17);
  {$EXTERNALSYM OPM_CONNECTOR_TYPE_TRANSPORT_AGNOSTIC_DIGITAL_MODE_B}
  OPM_COPP_COMPATIBLE_CONNECTOR_TYPE_INTERNAL = Integer($80000000); // -1 to keep within delphi limits
  {$EXTERNALSYM OPM_COPP_COMPATIBLE_CONNECTOR_TYPE_INTERNAL}

  OPM_DVI_CHARACTERISTIC_1_0          = 1;
  {$EXTERNALSYM OPM_DVI_CHARACTERISTIC_1_0}
  OPM_DVI_CHARACTERISTIC_1_1_OR_ABOVE = 2;
  {$EXTERNALSYM OPM_DVI_CHARACTERISTIC_1_1_OR_ABOVE}

  // NUAE stands for Non-User Accessible Enclosure

  OPM_BUS_TYPE_OTHER                                                      = Integer(0);
  {$EXTERNALSYM OPM_BUS_TYPE_OTHER}
  OPM_BUS_TYPE_PCI                                                        = Integer($1);
  {$EXTERNALSYM OPM_BUS_TYPE_PCI}
  OPM_BUS_TYPE_PCIX                                                       = Integer($2);
  {$EXTERNALSYM OPM_BUS_TYPE_PCIX}
  OPM_BUS_TYPE_PCIEXPRESS                                                 = Integer($3);
  {$EXTERNALSYM OPM_BUS_TYPE_PCIEXPRESS}
  OPM_BUS_TYPE_AGP                                                        = Integer($4);
  {$EXTERNALSYM OPM_BUS_TYPE_AGP}
  OPM_BUS_IMPLEMENTATION_MODIFIER_INSIDE_OF_CHIPSET                       = Integer($10000);
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_INSIDE_OF_CHIPSET}
  OPM_BUS_IMPLEMENTATION_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_CHIP          = Integer($20000);
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_CHIP}
  OPM_BUS_IMPLEMENTATION_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_SOCKET        = Integer($30000);
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_SOCKET}
  OPM_BUS_IMPLEMENTATION_MODIFIER_DAUGHTER_BOARD_CONNECTOR                = Integer($40000);
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_DAUGHTER_BOARD_CONNECTOR}
  OPM_BUS_IMPLEMENTATION_MODIFIER_DAUGHTER_BOARD_CONNECTOR_INSIDE_OF_NUAE = Integer($50000);
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_DAUGHTER_BOARD_CONNECTOR_INSIDE_OF_NUAE}
  OPM_BUS_IMPLEMENTATION_MODIFIER_NON_STANDARD                            = Integer($80000000);
  {$EXTERNALSYM OPM_BUS_IMPLEMENTATION_MODIFIER_NON_STANDARD}
  OPM_COPP_COMPATIBLE_BUS_TYPE_INTEGRATED                                 = Integer($80000000);
  {$EXTERNALSYM OPM_COPP_COMPATIBLE_BUS_TYPE_INTEGRATED}

type
  // Description:
  //
  //  Enumerations used by IOPMVideoOutput.COPPCompatibleGetInformation(),
  //  IOPMVideoOutput.GetInformation() and IOPMVideoOutput.Configure().
  //
  POpmDpcpProtectionLevel = ^TOpmDpcpProtectionLevel;
  POPM_DPCP_PROTECTION_LEVEL = ^OPM_DPCP_PROTECTION_LEVEL;
  _OPM_DPCP_PROTECTION_LEVEL = ULONG;
  {$EXTERNALSYM _OPM_DPCP_PROTECTION_LEVEL}
  OPM_DPCP_PROTECTION_LEVEL = _OPM_DPCP_PROTECTION_LEVEL;
  {$EXTERNALSYM OPM_DPCP_PROTECTION_LEVEL}
  TOpmDpcpProtectionLevel = _OPM_DPCP_PROTECTION_LEVEL;
  {$EXTERNALSYM TOpmDpcpProtectionLevel}
const
  OPM_DPCP_OFF         = ULONG(0);
  {$EXTERNALSYM OPM_DPCP_OFF}
  OPM_DPCP_ON          = ULONG(1);
  {$EXTERNALSYM OPM_DPCP_ON}
  //OPM_DPCP_FORCE_ULONG = $7FFFFFFF;

type
  POPM_HDCP_PROTECTION_LEVEL = ^OPM_HDCP_PROTECTION_LEVEL;
  _OPM_HDCP_PROTECTION_LEVEL = ULONG;
  {$EXTERNALSYM _OPM_HDCP_PROTECTION_LEVEL}
  OPM_HDCP_PROTECTION_LEVEL = _OPM_HDCP_PROTECTION_LEVEL;
  {$EXTERNALSYM OPM_HDCP_PROTECTION_LEVEL}
const
  OPM_HDCP_OFF         = ULONG(0);
  {$EXTERNALSYM OPM_HDCP_OFF}
  OPM_HDCP_ON          = ULONG(1);
  {$EXTERNALSYM OPM_HDCP_ON}
  //OPM_HDCP_FORCE_ULONG = $7FFFFFFF;


const

  OPM_CGMSA_OFF                             = 0;
  {$EXTERNALSYM OPM_CGMSA_OFF}
  OPM_CGMSA_COPY_FREELY                     = $1;
  {$EXTERNALSYM OPM_CGMSA_COPY_FREELY}
  OPM_CGMSA_COPY_NO_MORE                    = $2;
  {$EXTERNALSYM OPM_CGMSA_COPY_NO_MORE}
  OPM_CGMSA_COPY_ONE_GENERATION             = $3;
  {$EXTERNALSYM OPM_CGMSA_COPY_ONE_GENERATION}
  OPM_CGMSA_COPY_NEVER                      = $4;
  {$EXTERNALSYM OPM_CGMSA_COPY_NEVER}
  OPM_CGMSA_REDISTRIBUTION_CONTROL_REQUIRED = $8;
  {$EXTERNALSYM OPM_CGMSA_REDISTRIBUTION_CONTROL_REQUIRED}

type
  POPM_ACP_PROTECTION_LEVEL = ^OPM_ACP_PROTECTION_LEVEL;
  _OPM_ACP_PROTECTION_LEVEL = ULONG;
  {$EXTERNALSYM _OPM_ACP_PROTECTION_LEVEL}
  OPM_ACP_PROTECTION_LEVEL = _OPM_ACP_PROTECTION_LEVEL;
  {$EXTERNALSYM OPM_ACP_PROTECTION_LEVEL}
const
  OPM_ACP_OFF         = OPM_ACP_PROTECTION_LEVEL(0);
  {$EXTERNALSYM OPM_ACP_OFF}
  OPM_ACP_LEVEL_ONE   = OPM_ACP_PROTECTION_LEVEL(1);
  {$EXTERNALSYM OPM_ACP_LEVEL_ONE}
  OPM_ACP_LEVEL_TWO   = OPM_ACP_PROTECTION_LEVEL(2);
  {$EXTERNALSYM OPM_ACP_LEVEL_TWO}
  OPM_ACP_LEVEL_THREE = OPM_ACP_PROTECTION_LEVEL(3);
  {$EXTERNALSYM OPM_ACP_LEVEL_THREE}
  //OPM_ACP_FORCE_ULONG = $7FFFFFFF);


const

  OPM_PROTECTION_TYPE_OTHER                 = Integer($80000000);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_OTHER}
  OPM_PROTECTION_TYPE_NONE                  = Integer(0);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_NONE}
  OPM_PROTECTION_TYPE_COPP_COMPATIBLE_HDCP  = Integer($1);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_COPP_COMPATIBLE_HDCP}
  OPM_PROTECTION_TYPE_ACP                   = Integer($2);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_ACP}
  OPM_PROTECTION_TYPE_CGMSA                 = Integer($4);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_CGMSA}
  OPM_PROTECTION_TYPE_HDCP                  = Integer($8);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_HDCP}
  OPM_PROTECTION_TYPE_DPCP                  = Integer($10);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_DPCP}
  OPM_PROTECTION_TYPE_TYPE_ENFORCEMENT_HDCP	= Integer($20);
  {$EXTERNALSYM OPM_PROTECTION_TYPE_TYPE_ENFORCEMENT_HDCP}


  OPM_PROTECTION_STANDARD_OTHER               = Integer($80000000);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_OTHER}
  OPM_PROTECTION_STANDARD_NONE                = Integer(0);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_NONE}
  OPM_PROTECTION_STANDARD_IEC61880_525I       = Integer($1);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_IEC61880_525I}
  OPM_PROTECTION_STANDARD_IEC61880_2_525I     = Integer($2);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_IEC61880_2_525I}
  OPM_PROTECTION_STANDARD_IEC62375_625P       = Integer($4);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_IEC62375_625P}
  OPM_PROTECTION_STANDARD_EIA608B_525         = Integer($8);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_EIA608B_525}
  OPM_PROTECTION_STANDARD_EN300294_625I       = Integer($10);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_EN300294_625I}
  OPM_PROTECTION_STANDARD_CEA805A_TYPEA_525P  = Integer($20);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_CEA805A_TYPEA_525P}
  OPM_PROTECTION_STANDARD_CEA805A_TYPEA_750P  = Integer($40);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_CEA805A_TYPEA_750P}
  OPM_PROTECTION_STANDARD_CEA805A_TYPEA_1125I = Integer($80);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_CEA805A_TYPEA_1125I}
  OPM_PROTECTION_STANDARD_CEA805A_TYPEB_525P  = Integer($100);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_CEA805A_TYPEB_525P}
  OPM_PROTECTION_STANDARD_CEA805A_TYPEB_750P  = Integer($200);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_CEA805A_TYPEB_750P}
  OPM_PROTECTION_STANDARD_CEA805A_TYPEB_1125I = Integer($400);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_CEA805A_TYPEB_1125I}
  OPM_PROTECTION_STANDARD_ARIBTRB15_525I      = Integer($800);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_ARIBTRB15_525I}
  OPM_PROTECTION_STANDARD_ARIBTRB15_525P      = Integer($1000);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_ARIBTRB15_525P}
  OPM_PROTECTION_STANDARD_ARIBTRB15_750P      = Integer($2000);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_ARIBTRB15_750P}
  OPM_PROTECTION_STANDARD_ARIBTRB15_1125I     = Integer($4000);
  {$EXTERNALSYM OPM_PROTECTION_STANDARD_ARIBTRB15_1125I}

type
  POPM_IMAGE_ASPECT_RATIO_EN300294 = ^OPM_IMAGE_ASPECT_RATIO_EN300294;
  _OPM_IMAGE_ASPECT_RATIO_EN300294 = ULONG;
  {$EXTERNALSYM _OPM_IMAGE_ASPECT_RATIO_EN300294}
  OPM_IMAGE_ASPECT_RATIO_EN300294 = _OPM_IMAGE_ASPECT_RATIO_EN300294;
  {$EXTERNALSYM OPM_IMAGE_ASPECT_RATIO_EN300294}
const
  OPM_ASPECT_RATIO_EN300294_FULL_FORMAT_4_BY_3                  = OPM_IMAGE_ASPECT_RATIO_EN300294(0);
  OPM_ASPECT_RATIO_EN300294_BOX_14_BY_9_CENTER                  = OPM_IMAGE_ASPECT_RATIO_EN300294(1);
  OPM_ASPECT_RATIO_EN300294_BOX_14_BY_9_TOP                     = OPM_IMAGE_ASPECT_RATIO_EN300294(2);
  OPM_ASPECT_RATIO_EN300294_BOX_16_BY_9_CENTER                  = OPM_IMAGE_ASPECT_RATIO_EN300294(3);
  OPM_ASPECT_RATIO_EN300294_BOX_16_BY_9_TOP                     = OPM_IMAGE_ASPECT_RATIO_EN300294(4);
  OPM_ASPECT_RATIO_EN300294_BOX_GT_16_BY_9_CENTER               = OPM_IMAGE_ASPECT_RATIO_EN300294(5);
  OPM_ASPECT_RATIO_EN300294_FULL_FORMAT_4_BY_3_PROTECTED_CENTER = OPM_IMAGE_ASPECT_RATIO_EN300294(6);
  OPM_ASPECT_RATIO_EN300294_FULL_FORMAT_16_BY_9_ANAMORPHIC      = OPM_IMAGE_ASPECT_RATIO_EN300294(7);
  //OPM_ASPECT_RATIO_FORCE_ULONG                                  = $7FFFFFFF);

type
  // Description:
  //
  //  Structures used by several functions
  //

  POPM_RANDOM_NUMBER = ^OPM_RANDOM_NUMBER;
  _OPM_RANDOM_NUMBER = record
    abRandomNumber: array[0..15] of Byte;
  end;
  {$EXTERNALSYM _OPM_RANDOM_NUMBER}
  OPM_RANDOM_NUMBER = _OPM_RANDOM_NUMBER;
  {$EXTERNALSYM OPM_RANDOM_NUMBER}


  POPM_OMAC = ^OPM_OMAC;
  _OPM_OMAC = record
    abOMAC: array[0..15] of Byte;
  end;
  {$EXTERNALSYM _OPM_OMAC}
  OPM_OMAC = _OPM_OMAC;
  {$EXTERNALSYM OPM_OMAC}


  // Description:
  //
  //  Structures used by IOPMVideoOutput.FinishInitialization()
  //

  POPM_ENCRYPTED_INITIALIZATION_PARAMETERS = ^_OPM_ENCRYPTED_INITIALIZATION_PARAMETERS;
  _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS = record
    abEncryptedInitializationParameters: array[0..255] of Byte;
  end;
  {$EXTERNALSYM _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS}
  OPM_ENCRYPTED_INITIALIZATION_PARAMETERS = _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS;
  {$EXTERNALSYM OPM_ENCRYPTED_INITIALIZATION_PARAMETERS}

  // Description:
  //
  //  Structures used by IOPMVideoOutput.GetInformation()
  //
  POPM_GET_INFO_PARAMETERS = ^OPM_GET_INFO_PARAMETERS;
  _OPM_GET_INFO_PARAMETERS = record
    omac: OPM_OMAC;
    rnRandomNumber: OPM_RANDOM_NUMBER;
    guidInformation: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..4055] of Byte;
  end;
  {$EXTERNALSYM _OPM_GET_INFO_PARAMETERS}
  OPM_GET_INFO_PARAMETERS = _OPM_GET_INFO_PARAMETERS;
  {$EXTERNALSYM OPM_GET_INFO_PARAMETERS}


  // Description:
  //
  //  Structures used by IOPMVideoOutput.COPPCompatibleGetInformation()
  //
  POPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS = ^OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS;
  _OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    guidInformation: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..4055] of Byte;
  end;
  {$EXTERNALSYM _OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS}
  OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS = _OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS;
  {$EXTERNALSYM OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS}


  POPM_HDCP_KEY_SELECTION_VECTOR = ^OPM_HDCP_KEY_SELECTION_VECTOR;
  _OPM_HDCP_KEY_SELECTION_VECTOR = record
    abKeySelectionVector: array[0..4] of Byte;
  end;
  {$EXTERNALSYM _OPM_HDCP_KEY_SELECTION_VECTOR}
  OPM_HDCP_KEY_SELECTION_VECTOR = _OPM_HDCP_KEY_SELECTION_VECTOR;
  {$EXTERNALSYM OPM_HDCP_KEY_SELECTION_VECTOR}


  POPM_CONNECTED_HDCP_DEVICE_INFORMATION = ^OPM_CONNECTED_HDCP_DEVICE_INFORMATION;
  _OPM_CONNECTED_HDCP_DEVICE_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulHDCPFlags: ULONG;
    ksvB: OPM_HDCP_KEY_SELECTION_VECTOR;
    Reserved: array[0..10] of Byte;
    Reserved2: array[0..15] of Byte;
    Reserved3: array[0..15] of Byte;
  end;
  {$EXTERNALSYM _OPM_CONNECTED_HDCP_DEVICE_INFORMATION}
  OPM_CONNECTED_HDCP_DEVICE_INFORMATION = _OPM_CONNECTED_HDCP_DEVICE_INFORMATION;
  {$EXTERNALSYM OPM_CONNECTED_HDCP_DEVICE_INFORMATION}

  // Description:
  //
  //  Structures used by IOPMVideoOutput.GetInformation() and
  //  IOPMVideoOutput.COPPCompatibleGetInformation()
  //
  POPM_REQUESTED_INFORMATION = ^OPM_REQUESTED_INFORMATION;
  _OPM_REQUESTED_INFORMATION = record
    omac: OPM_OMAC;
    cbRequestedInformationSize: ULONG;
    abRequestedInformation: array[0..4075] of Byte;
  end;
  {$EXTERNALSYM _OPM_REQUESTED_INFORMATION}
  OPM_REQUESTED_INFORMATION = _OPM_REQUESTED_INFORMATION;
  {$EXTERNALSYM OPM_REQUESTED_INFORMATION}


  POPM_STANDARD_INFORMATION = ^OPM_STANDARD_INFORMATION;
  _OPM_STANDARD_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulInformation: ULONG;
    ulReserved: ULONG;
    ulReserved2: ULONG;
  end;
  {$EXTERNALSYM _OPM_STANDARD_INFORMATION}
  OPM_STANDARD_INFORMATION = _OPM_STANDARD_INFORMATION;
  {$EXTERNALSYM OPM_STANDARD_INFORMATION}

{$IFNDEF DO_NOT_USE_DIRECTX_OR_DXVA2}
  POPM_ACTUAL_OUTPUT_FORMAT = ^OPM_ACTUAL_OUTPUT_FORMAT;
  _OPM_ACTUAL_OUTPUT_FORMAT = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulDisplayWidth: ULONG;
    ulDisplayHeight: ULONG;
    dsfSampleInterleaveFormat: DXVA2_SampleFormat;
    d3dFormat: D3DFORMAT;
    ulFrequencyNumerator: ULONG;
    ulFrequencyDenominator: ULONG;
  end;
  {$EXTERNALSYM _OPM_ACTUAL_OUTPUT_FORMAT}
  OPM_ACTUAL_OUTPUT_FORMAT = _OPM_ACTUAL_OUTPUT_FORMAT;
  {$EXTERNALSYM OPM_ACTUAL_OUTPUT_FORMAT}
{$ENDIF} // DO_NOT_USE_DIRECTX_OR_DXVA2


  POPM_ACP_AND_CGMSA_SIGNALING = ^OPM_ACP_AND_CGMSA_SIGNALING;
  _OPM_ACP_AND_CGMSA_SIGNALING = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulAvailableTVProtectionStandards: ULONG;
    ulActiveTVProtectionStandard: ULONG;
    ulReserved: ULONG;
    ulAspectRatioValidMask1: ULONG;
    ulAspectRatioData1: ULONG;
    ulAspectRatioValidMask2: ULONG;
    ulAspectRatioData2: ULONG;
    ulAspectRatioValidMask3: ULONG;
    ulAspectRatioData3: ULONG;
    ulReserved2: array[0..3] of ULONG;
    ulReserved3: array[0..3] of ULONG;
  end;
  {$EXTERNALSYM _OPM_ACP_AND_CGMSA_SIGNALING}
  OPM_ACP_AND_CGMSA_SIGNALING = _OPM_ACP_AND_CGMSA_SIGNALING;
  {$EXTERNALSYM OPM_ACP_AND_CGMSA_SIGNALING}


  POPM_OUTPUT_ID_DATA = ^OPM_OUTPUT_ID_DATA;
  _OPM_OUTPUT_ID_DATA = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    OutputId: UINT64;
  end;
  {$EXTERNALSYM _OPM_OUTPUT_ID_DATA}
  OPM_OUTPUT_ID_DATA = _OPM_OUTPUT_ID_DATA;
  {$EXTERNALSYM OPM_OUTPUT_ID_DATA}


  // Description:
  //
  //  Structures used by IOPMVideoOutput.GetInformation() and
  //  IOPMVideoOutput.COPPCompatibleGetInformation()
  //
  POPM_CONFIGURE_PARAMETERS = ^OPM_CONFIGURE_PARAMETERS;
  _OPM_CONFIGURE_PARAMETERS = record
    omac: OPM_OMAC;
    guidSetting: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..4055] of Byte;
  end;
  {$EXTERNALSYM _OPM_CONFIGURE_PARAMETERS}
  OPM_CONFIGURE_PARAMETERS = _OPM_CONFIGURE_PARAMETERS;
  {$EXTERNALSYM OPM_CONFIGURE_PARAMETERS}


  POPM_SET_PROTECTION_LEVEL_PARAMETERS = ^OPM_SET_PROTECTION_LEVEL_PARAMETERS;
  _OPM_SET_PROTECTION_LEVEL_PARAMETERS = record
    ulProtectionType: ULONG;
    ulProtectionLevel: ULONG;
    Reserved: ULONG;
    Reserved2: ULONG;
  end;
  {$EXTERNALSYM _OPM_SET_PROTECTION_LEVEL_PARAMETERS}
  OPM_SET_PROTECTION_LEVEL_PARAMETERS = _OPM_SET_PROTECTION_LEVEL_PARAMETERS;
  {$EXTERNALSYM OPM_SET_PROTECTION_LEVEL_PARAMETERS}


  POPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS = ^OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS;
  _OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS = record
    ulNewTVProtectionStandard: ULONG;
    ulAspectRatioChangeMask1: ULONG;
    ulAspectRatioData1: ULONG;
    ulAspectRatioChangeMask2: ULONG;
    ulAspectRatioData2: ULONG;
    ulAspectRatioChangeMask3: ULONG;
    ulAspectRatioData3: ULONG;
    ulReserved: array[0..3] of ULONG;
    ulReserved2: array[0..3] of ULONG;
    ulReserved3: ULONG;
  end;
  {$EXTERNALSYM _OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS}
  OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS = _OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS;
  {$EXTERNALSYM OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS}


  POPM_SET_HDCP_SRM_PARAMETERS = ^OPM_SET_HDCP_SRM_PARAMETERS;
  _OPM_SET_HDCP_SRM_PARAMETERS = record
    ulSRMVersion: ULONG;
  end;
  {$EXTERNALSYM _OPM_SET_HDCP_SRM_PARAMETERS}
  OPM_SET_HDCP_SRM_PARAMETERS = _OPM_SET_HDCP_SRM_PARAMETERS;
  {$EXTERNALSYM OPM_SET_HDCP_SRM_PARAMETERS}

  //  Description:
  //
  //  Input parameter for OPM_GET_CODEC_INFO GetInformation request
  //  This data is stored in the abParameters array of OPM_GET_INFO_PARAMETERS
  //
  POPM_GET_CODEC_INFO_PARAMETERS = ^OPM_GET_CODEC_INFO_PARAMETERS;
  _OPM_GET_CODEC_INFO_PARAMETERS = record
    cbVerifier: DWORD;
    Verifier: array[0..4051] of Byte;
  end;
  {$EXTERNALSYM _OPM_GET_CODEC_INFO_PARAMETERS}
  OPM_GET_CODEC_INFO_PARAMETERS = _OPM_GET_CODEC_INFO_PARAMETERS;
  {$EXTERNALSYM OPM_GET_CODEC_INFO_PARAMETERS}

  //  Description:
  //
  //  Output information for OPM_GET_CODEC_INFO GetInformation request
  //  This data is returned in the abRequestedInformation array of
  //  OPM_REQUESTED_INFORMATION structure
  //
  POPM_GET_CODEC_INFO_INFORMATION = ^OPM_GET_CODEC_INFO_INFORMATION;
  _OPM_GET_CODEC_INFO_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    Merit: DWORD;
  end;
  {$EXTERNALSYM _OPM_GET_CODEC_INFO_INFORMATION}
  OPM_GET_CODEC_INFO_INFORMATION = _OPM_GET_CODEC_INFO_INFORMATION;
  {$EXTERNALSYM OPM_GET_CODEC_INFO_INFORMATION}


type

  // Interface IOPMVideoOutput
  // =========================
  // Description:
  //
  //  IOPMVideoOutput represents a virtualized video output. It is used to
  //  establish a protected communications channel with an output, get
  //  information from an output and configure an output.
  //
  PIOPMVideoOutput = ^IOPMVideoOutput;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOPMVideoOutput);'}
  {$EXTERNALSYM IOPMVideoOutput}
  IOPMVideoOutput = interface(IUnknown)
  ['{0A15159D-41C7-4456-93E1-284CD61D4E8D}']

    function StartInitialization(prnRandomNumber: OPM_RANDOM_NUMBER;
                                 out ppbCertificate: PByte;
                                 out pulCertificateLength: ULONG): HResult; stdcall;

    function FinishInitialization(pParameters: OPM_ENCRYPTED_INITIALIZATION_PARAMETERS): HResult; stdcall;

    function GetInformation(pParameters: OPM_GET_INFO_PARAMETERS;
                            out pRequestedInformation: OPM_REQUESTED_INFORMATION): HResult; stdcall;

    function COPPCompatibleGetInformation(pParameters: OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS;
                                          out pRequestedInformation: OPM_REQUESTED_INFORMATION): HResult; stdcall;

    function Configure(pParameters: OPM_CONFIGURE_PARAMETERS;
                       ulAdditionalParametersSize: ULONG;
                       pbAdditionalParameters: PByte): HResult; stdcall;

  end;
  // IOPMVideoOutput
  IID_IOPMVideoOutput = IOPMVideoOutput;
  {$EXTERNALSYM IID_IOPMVideoOutput}


  // Description:
  //
  //  Ks Property set to use with AVStram drivers
  // KSPROPSETID_OPMVideoOutput {06F414BB-F43A-4fe2-A566-774B4C81F0DB}
const
  STATIC_KSPROPSETID_OPMVideoOutput   : TGUID = '{06f414bb-f43a-4fe2-a566-774b4c81f0db}';
  {$EXTERNALSYM STATIC_KSPROPSETID_OPMVideoOutput}
  KSPROPSETID_OPMVideoOutput          : TGUID = '{06F414BB-F43A-4fe2-A566-774B4C81F0DB}';
  {$EXTERNALSYM KSPROPSETID_OPMVideoOutput}


type

  PKSMETHOD_OPMVIDEOOUTPUT = ^KSMETHOD_OPMVIDEOOUTPUT;
  cwKSMETHOD_OPMVIDEOOUTPUT                      = (
    // Output is OPM_RANDOM_NUMBER followed by certifiate
    KSMETHOD_OPMVIDEOOUTPUT_STARTINITIALIZATION  = 0,
    // Input OPM_ENCRYPTED_INITIALIZATION_PARAMETERS
    // Output OPM_STANDARD_INFORMATION
    KSMETHOD_OPMVIDEOOUTPUT_FINISHINITIALIZATION = 1,
    // Input is OPM_GET_INFO_PARAMETERS, output is OPM_REQUESTED_INFORMATION
    // Use KsMethod - both input and output in the buffer (not after the KSMETHOD structure)
    KSMETHOD_OPMVIDEOOUTPUT_GETINFORMATION       = 2
  );
  {$EXTERNALSYM cwKSMETHOD_OPMVIDEOOUTPUT}
  KSMETHOD_OPMVIDEOOUTPUT = cwKSMETHOD_OPMVIDEOOUTPUT;
  {$EXTERNALSYM KSMETHOD_OPMVIDEOOUTPUT}


  // Description:
  //
  //  These functions create IOPMVideoOutput objects.
  //

  // public
  function OPMGetVideoOutputsFromHMONITOR(hMonitor: HMONITOR;
                                          vos: OPM_VIDEO_OUTPUT_SEMANTICS;
                                          out pulNumVideoOutputs: ULONG;
                                          out pppOPMVideoOutputArray: PIOPMVideoOutput): HResult; stdcall;
  {$EXTERNALSYM OPMGetVideoOutputsFromHMONITOR}


  function OPMGetVideoOutputForTarget(var pAdapterLuid: LUID;
                                      VidPnTarget: ULONG;
                                      vos: OPM_VIDEO_OUTPUT_SEMANTICS;
                                      var ppOPMVideoOutput: IOPMVideoOutput): HResult; stdcall;
  {$EXTERNALSYM OPMGetVideoOutputForTarget}


{$IFNDEF DO_NOT_USE_DIRECTX_OR_DXVA2}
  // public
  function OPMGetVideoOutputsFromIDirect3DDevice9Object(pDirect3DDevice9: IDirect3DDevice9;
                                                        vos: OPM_VIDEO_OUTPUT_SEMANTICS;
                                                        out pulNumVideoOutputs: ULONG;
                                                        out pppOPMVideoOutputArray: IOPMVideoOutput): HResult; stdcall;
  {$EXTERNALSYM OPMGetVideoOutputsFromIDirect3DDevice9Object}
{$ENDIF} // DO_NOT_USE_DIRECTX_OR_DXVA2

  // Description:
  //
  // These macros parse a ULONG which contains an OPM bus type constant and
  // possibly an OPM implementation constant.
  //

  // private
  function GetBusType(ulBusTypeAndImplementation: ULONG): ULONG;
  {$EXTERNALSYM GetBusType}
  function GetBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
  {$EXTERNALSYM GetBusImplementation}
  function IsNonStandardBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
  {$EXTERNALSYM IsNonStandardBusImplementation}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  OpmApiLib = 'Dxva2.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function OPMGetVideoOutputsFromHMONITOR; external OpmApiLib name 'OPMGetVideoOutputsFromHMONITOR' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function OPMGetVideoOutputForTarget; external OpmApiLib name 'OPMGetVideoOutputsFromHMONITOR' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function OPMGetVideoOutputsFromIDirect3DDevice9Object; external OpmApiLib name 'OPMGetVideoOutputsFromIDirect3DDevice9Object' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}


// private
function GetBusType(ulBusTypeAndImplementation: ULONG): ULONG;
begin
  Result:= ulBusTypeAndImplementation and Ord(OPM_BUS_TYPE_MASK);
end;

function GetBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
begin
  Result:= ulBusTypeAndImplementation and Ord(OPM_BUS_IMPLEMENTATION_MODIFIER_MASK) shr 16;
end;

function IsNonStandardBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
begin
  Result:= ulBusTypeAndImplementation and Ord(OPM_BUS_IMPLEMENTATION_MODIFIER_NON_STANDARD);
end;

  // Implement Additional Prototypes here.

end.

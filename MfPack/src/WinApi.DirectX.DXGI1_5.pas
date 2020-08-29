// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGI1_5.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Microsoft DirectX Graphics Infrastructure API
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
// Remarks: -
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
// Source: dxgi1_5.h
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
//
//==============================================================================
unit WinApi.DirectX.DXGI1_5;

  {$HPPEMIT '#include "dxgi1_5.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGI1_2,
  WinApi.DirectX.DXGI1_3,
  WinApi.DirectX.DXGI1_4;

  {$WEAKPACKAGEUNIT ON}


// Enums =======================================================================

type
  PDXGI_OUTDUPL_FLAG = ^DXGI_OUTDUPL_FLAG;
  DXGI_OUTDUPL_FLAG = DWord;
  {$EXTERNALSYM DXGI_OUTDUPL_FLAG}
const
  DXGI_OUTDUPL_COMPOSITED_UI_CAPTURE_ONLY = DXGI_OUTDUPL_FLAG(1);
  {$EXTERNALSYM DXGI_OUTDUPL_COMPOSITED_UI_CAPTURE_ONLY}

type
  PDXGI_HDR_METADATA_TYPE = ^DXGI_HDR_METADATA_TYPE;
  DXGI_HDR_METADATA_TYPE = DWord;
  {$EXTERNALSYM DXGI_HDR_METADATA_TYPE}
const
  DXGI_HDR_METADATA_TYPE_NONE      = DXGI_HDR_METADATA_TYPE(0);
  {$EXTERNALSYM DXGI_HDR_METADATA_TYPE_NONE}
  DXGI_HDR_METADATA_TYPE_HDR10     = DXGI_HDR_METADATA_TYPE(1);
  {$EXTERNALSYM DXGI_HDR_METADATA_TYPE_HDR10}
  DXGI_HDR_METADATA_TYPE_HDR10PLUS = DXGI_HDR_METADATA_TYPE(2);
  {$EXTERNALSYM DXGI_HDR_METADATA_TYPE_HDR10PLUS}

type
  //--------------------------------------------------------------------------------------------------------
  // Enums for IDXGIDevice4 interface
  //--------------------------------------------------------------------------------------------------------
  PDXGI_OFFER_RESOURCE_FLAGS = ^_DXGI_OFFER_RESOURCE_FLAGS;
  _DXGI_OFFER_RESOURCE_FLAGS = DWord;
  {$EXTERNALSYM _DXGI_OFFER_RESOURCE_FLAGS}
  DXGI_OFFER_RESOURCE_FLAGS = _DXGI_OFFER_RESOURCE_FLAGS;
  {$EXTERNALSYM DXGI_OFFER_RESOURCE_FLAGS}
const
  DXGI_OFFER_RESOURCE_FLAG_ALLOW_DECOMMIT = DXGI_OFFER_RESOURCE_FLAGS($1);
  {$EXTERNALSYM DXGI_OFFER_RESOURCE_FLAG_ALLOW_DECOMMIT}

type
  PDXGI_RECLAIM_RESOURCE_RESULTS = ^_DXGI_RECLAIM_RESOURCE_RESULTS;
  _DXGI_RECLAIM_RESOURCE_RESULTS = DWord;
  {$EXTERNALSYM _DXGI_RECLAIM_RESOURCE_RESULTS}
  DXGI_RECLAIM_RESOURCE_RESULTS = _DXGI_RECLAIM_RESOURCE_RESULTS;
  {$EXTERNALSYM DXGI_RECLAIM_RESOURCE_RESULTS}
const
  DXGI_RECLAIM_RESOURCE_RESULT_OK            = DXGI_RECLAIM_RESOURCE_RESULTS(0);
  {$EXTERNALSYM DXGI_RECLAIM_RESOURCE_RESULT_OK}
  DXGI_RECLAIM_RESOURCE_RESULT_DISCARDED     = DXGI_RECLAIM_RESOURCE_RESULTS(1);
  {$EXTERNALSYM DXGI_RECLAIM_RESOURCE_RESULT_DISCARDED}
  DXGI_RECLAIM_RESOURCE_RESULT_NOT_COMMITTED = DXGI_RECLAIM_RESOURCE_RESULTS(2);
  {$EXTERNALSYM DXGI_RECLAIM_RESOURCE_RESULT_NOT_COMMITTED}

type
  //+-----------------------------------------------------------------------------
  //  Enum for IDXGIFactory5.CheckFeatureSupport
  //------------------------------------------------------------------------------
  PDXGI_FEATURE = ^DXGI_FEATURE;
  DXGI_FEATURE = DWord;
  {$EXTERNALSYM DXGI_FEATURE}
const
  DXGI_FEATURE_PRESENT_ALLOW_TEARING = DXGI_FEATURE(0);
  {$EXTERNALSYM DXGI_FEATURE_PRESENT_ALLOW_TEARING}

// =============================================================================

type

  // Interface IDXGIOutput5
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIOutput5);'}
  {$EXTERNALSYM IDXGIOutput5}
  IDXGIOutput5 = interface(IDXGIOutput4)
  ['{80A07424-AB52-42EB-833C-0C42FD282D98}']

    function DuplicateOutput1(pDevice: IUnknown;
                              Flags: UINT;
                              SupportedFormatsCount: UINT;
                              pSupportedFormats: DXGI_FORMAT;
                              out ppOutputDuplication: IDXGIOutputDuplication): HResult; stdcall;

  end;
  IID_IDXGIOutput5 = IDXGIOutput5;
  {$EXTERNALSYM IID_IDXGIOutput5}


  //+-----------------------------------------------------------------------------
  //
  //  HDR MetaData types
  //
  //------------------------------------------------------------------------------

  PDXGI_HDR_METADATA_HDR10 = ^DXGI_HDR_METADATA_HDR10;
  DXGI_HDR_METADATA_HDR10 = record
    // Color gamut
    RedPrimary: array[0..1] of UINT16;
    GreenPrimary: array[0..1] of UINT16;
    BluePrimary: array[0..1] of UINT16;
    WhitePoint: array[0..1] of UINT16;
    // Luminance
    MaxMasteringLuminance: UINT;
    MinMasteringLuminance: UINT;
    MaxContentLightLevel: UINT16;
    MaxFrameAverageLightLevel: UINT16;
  end;
  {$EXTERNALSYM DXGI_HDR_METADATA_HDR10}

  PDXGI_HDR_METADATA_HDR10PLUS = ^DXGI_HDR_METADATA_HDR10PLUS;
  DXGI_HDR_METADATA_HDR10PLUS = record
    Data: array[0..71] of Byte;
  end;
  {$EXTERNALSYM DXGI_HDR_METADATA_HDR10PLUS}


  // Interface IDXGISwapChain4
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISwapChain4);'}
  {$EXTERNALSYM IDXGISwapChain4}
  IDXGISwapChain4 = interface(IDXGISwapChain3)
  ['{3D585D5A-BD4A-489E-B1F4-3DBCB6452FFB}']

    function SetHDRMetaData(_Type: DXGI_HDR_METADATA_TYPE;
                            Size: UINT;
                            pMetaData: Pointer): HResult; stdcall;

  end;
  IID_IDXGISwapChain4 = IDXGISwapChain4;
  {$EXTERNALSYM IID_IDXGISwapChain4}


  // Interface IDXGIDevice4
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIDevice4);'}
  {$EXTERNALSYM IDXGIDevice4}
  IDXGIDevice4 = interface(IDXGIDevice3)
  ['{95B4F95F-D8DA-4CA4-9EE6-3B76D5968A10}']

    function OfferResources1(NumResources: UINT;
                             ppResources: IDXGIResource; // An array of pointers to IDXGIResource interfaces for the resources to offer.
                             Priority: DXGI_OFFER_RESOURCE_PRIORITY;
                             Flags: UINT): HResult; stdcall;

    function ReclaimResources1(NumResources: UINT;
                               ppResources: IDXGIResource; // An array of pointers to IDXGIResource interfaces for the resources to reclaim.
                               out pResults: PDXGI_RECLAIM_RESOURCE_RESULTS): HResult; stdcall; // A pointer to an array that receives DXGI_RECLAIM_RESOURCE_RESULTS values.

  end;
  IID_IDXGIDevice4 = IDXGIDevice4;
  {$EXTERNALSYM IID_IDXGIDevice4}


  // Interface IDXGIFactory5
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory5);'}
  {$EXTERNALSYM IDXGIFactory5}
  IDXGIFactory5 = interface(IDXGIFactory4)
  ['{7632e1f5-ee65-4dca-87fd-84cd75f8838d}']

    function CheckFeatureSupport(Feature: DXGI_FEATURE;
                                pFeatureSupportData: Pointer;
                                FeatureSupportDataSize: UINT): HResult; stdcall;

  end;
  IID_IDXGIFactory5 = IDXGIFactory5;
  {$EXTERNALSYM IID_IDXGIFactory5}

  //--------------------------------------------------------------------------------------------------------


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

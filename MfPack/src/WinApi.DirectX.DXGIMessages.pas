// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGIMessages.pas
// Kind: Pascal / Delphi unit
// Release date: 27-02-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: DXGI Debug Message Enumeration.
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
// Source: DXGIMessages.h
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
unit WinApi.DirectX.DXGIMessages;

  {$HPPEMIT '#include "DXGIMessages.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}

// #if NTDDI_VERSION >= NTDDI_WIN7

type
  PDXGIMessageId = ^PDXGI_Message_Id;
  PDXGI_Message_Id = ^DXGI_Message_Id;
  DXGI_Message_Id = DWord;
  {$EXTERNALSYM DXGI_Message_Id}
const
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_InvalidOutputWindow}
  DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_InvalidOutputWindow = DXGI_Message_Id(0);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_BufferWidthInferred}
  DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_BufferWidthInferred = DXGI_Message_Id(1);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_BufferHeightInferred}
  DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_BufferHeightInferred = DXGI_Message_Id(2);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_NoScanoutFlagChanged}
  DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_NoScanoutFlagChanged = DXGI_Message_Id(3);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Creation_MaxBufferCountExceeded}
  DXGI_MSG_IDXGISwapChain_Creation_MaxBufferCountExceeded = DXGI_Message_Id(4);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Creation_TooFewBuffers}
  DXGI_MSG_IDXGISwapChain_Creation_TooFewBuffers = DXGI_Message_Id(5);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Creation_NoOutputWindow}
  DXGI_MSG_IDXGISwapChain_Creation_NoOutputWindow = DXGI_Message_Id(6);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Destruction_OtherMethodsCalled}
  DXGI_MSG_IDXGISwapChain_Destruction_OtherMethodsCalled = DXGI_Message_Id(7);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetDesc_pDescIsNULL}
  DXGI_MSG_IDXGISwapChain_GetDesc_pDescIsNULL = DXGI_Message_Id(8);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetBuffer_ppSurfaceIsNULL}
  DXGI_MSG_IDXGISwapChain_GetBuffer_ppSurfaceIsNULL = DXGI_Message_Id(9);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetBuffer_NoAllocatedBuffers}
  DXGI_MSG_IDXGISwapChain_GetBuffer_NoAllocatedBuffers = DXGI_Message_Id(10);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetBuffer_iBufferMustBeZero}
  DXGI_MSG_IDXGISwapChain_GetBuffer_iBufferMustBeZero = DXGI_Message_Id(11);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetBuffer_iBufferOOB}
  DXGI_MSG_IDXGISwapChain_GetBuffer_iBufferOOB = DXGI_Message_Id(12);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetContainingOutput_ppOutputIsNULL}
  DXGI_MSG_IDXGISwapChain_GetContainingOutput_ppOutputIsNULL = DXGI_Message_Id(13);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_SyncIntervalOOB}
  DXGI_MSG_IDXGISwapChain_Present_SyncIntervalOOB = DXGI_Message_Id(14);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_InvalidNonPreRotatedFlag}
  DXGI_MSG_IDXGISwapChain_Present_InvalidNonPreRotatedFlag = DXGI_Message_Id(15);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_NoAllocatedBuffers}
  DXGI_MSG_IDXGISwapChain_Present_NoAllocatedBuffers = DXGI_Message_Id(16);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_GetDXGIAdapterFailed}
  DXGI_MSG_IDXGISwapChain_Present_GetDXGIAdapterFailed = DXGI_Message_Id(17);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_BufferCountOOB}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_BufferCountOOB = DXGI_Message_Id(18);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_UnreleasedReferences}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_UnreleasedReferences = DXGI_Message_Id(19);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidSwapChainFlag}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidSwapChainFlag = DXGI_Message_Id(20);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidNonPreRotatedFlag}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidNonPreRotatedFlag = DXGI_Message_Id(21);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeTarget_RefreshRateDivideByZero}
  DXGI_MSG_IDXGISwapChain_ResizeTarget_RefreshRateDivideByZero = DXGI_Message_Id(22);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_InvalidTarget}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_InvalidTarget = DXGI_Message_Id(23);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetFrameStatistics_pStatsIsNULL}
  DXGI_MSG_IDXGISwapChain_GetFrameStatistics_pStatsIsNULL = DXGI_Message_Id(24);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetLastPresentCount_pLastPresentCountIsNULL}
  DXGI_MSG_IDXGISwapChain_GetLastPresentCount_pLastPresentCountIsNULL = DXGI_Message_Id(25);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_RemoteNotSupported}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_RemoteNotSupported = DXGI_Message_Id(26);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_TakeOwnership_FailedToAcquireFullscreenMutex}
  DXGI_MSG_IDXGIOutput_TakeOwnership_FailedToAcquireFullscreenMutex = DXGI_Message_Id(27);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSoftwareAdapter_ppAdapterInterfaceIsNULL}
  DXGI_MSG_IDXGIFactory_CreateSoftwareAdapter_ppAdapterInterfaceIsNULL = DXGI_Message_Id(28);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_EnumAdapters_ppAdapterInterfaceIsNULL}
  DXGI_MSG_IDXGIFactory_EnumAdapters_ppAdapterInterfaceIsNULL = DXGI_Message_Id(29);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_ppSwapChainIsNULL}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_ppSwapChainIsNULL = DXGI_Message_Id(30);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_pDescIsNULL}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_pDescIsNULL = DXGI_Message_Id(31);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_UnknownSwapEffect}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_UnknownSwapEffect = DXGI_Message_Id(32);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidFlags}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidFlags = DXGI_Message_Id(33);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_NonPreRotatedFlagAndWindowed}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_NonPreRotatedFlagAndWindowed = DXGI_Message_Id(34);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_NullDeviceInterface}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_NullDeviceInterface = DXGI_Message_Id(35);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_GetWindowAssociation_phWndIsNULL}
  DXGI_MSG_IDXGIFactory_GetWindowAssociation_phWndIsNULL = DXGI_Message_Id(36);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_MakeWindowAssociation_InvalidFlags}
  DXGI_MSG_IDXGIFactory_MakeWindowAssociation_InvalidFlags = DXGI_Message_Id(37);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_InvalidSurface}
  DXGI_MSG_IDXGISurface_Map_InvalidSurface = DXGI_Message_Id(38);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_FlagsSetToZero}
  DXGI_MSG_IDXGISurface_Map_FlagsSetToZero = DXGI_Message_Id(39);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_DiscardAndReadFlagSet}
  DXGI_MSG_IDXGISurface_Map_DiscardAndReadFlagSet = DXGI_Message_Id(40);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_DiscardButNotWriteFlagSet}
  DXGI_MSG_IDXGISurface_Map_DiscardButNotWriteFlagSet = DXGI_Message_Id(41);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_NoCPUAccess}
  DXGI_MSG_IDXGISurface_Map_NoCPUAccess = DXGI_Message_Id(42);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_ReadFlagSetButCPUAccessIsDynamic}
  DXGI_MSG_IDXGISurface_Map_ReadFlagSetButCPUAccessIsDynamic = DXGI_Message_Id(43);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_DiscardFlagSetButCPUAccessIsNotDynamic}
  DXGI_MSG_IDXGISurface_Map_DiscardFlagSetButCPUAccessIsNotDynamic = DXGI_Message_Id(44);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplayModeList_pNumModesIsNULL}
  DXGI_MSG_IDXGIOutput_GetDisplayModeList_pNumModesIsNULL = DXGI_Message_Id(45);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_ModeHasInvalidWidthOrHeight}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_ModeHasInvalidWidthOrHeight = DXGI_Message_Id(46);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetCammaControlCapabilities_NoOwnerDevice}
  DXGI_MSG_IDXGIOutput_GetCammaControlCapabilities_NoOwnerDevice = DXGI_Message_Id(47);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_TakeOwnership_pDeviceIsNULL}
  DXGI_MSG_IDXGIOutput_TakeOwnership_pDeviceIsNULL = DXGI_Message_Id(48);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_NoOwnerDevice}
  DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_NoOwnerDevice = DXGI_Message_Id(49);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_pDestinationIsNULL}
  DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_pDestinationIsNULL = DXGI_Message_Id(50);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_MapOfDestinationFailed}
  DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_MapOfDestinationFailed = DXGI_Message_Id(51);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetFrameStatistics_NoOwnerDevice}
  DXGI_MSG_IDXGIOutput_GetFrameStatistics_NoOwnerDevice = DXGI_Message_Id(52);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetFrameStatistics_pStatsIsNULL}
  DXGI_MSG_IDXGIOutput_GetFrameStatistics_pStatsIsNULL = DXGI_Message_Id(53);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_SetGammaControl_NoOwnerDevice}
  DXGI_MSG_IDXGIOutput_SetGammaControl_NoOwnerDevice = DXGI_Message_Id(54);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetGammaControl_NoOwnerDevice}
  DXGI_MSG_IDXGIOutput_GetGammaControl_NoOwnerDevice = DXGI_Message_Id(55);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetGammaControl_NoGammaControls}
  DXGI_MSG_IDXGIOutput_GetGammaControl_NoGammaControls = DXGI_Message_Id(56);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_SetDisplaySurface_IDXGIResourceNotSupportedBypPrimary}
  DXGI_MSG_IDXGIOutput_SetDisplaySurface_IDXGIResourceNotSupportedBypPrimary = DXGI_Message_Id(57);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_SetDisplaySurface_pPrimaryIsInvalid}
  DXGI_MSG_IDXGIOutput_SetDisplaySurface_pPrimaryIsInvalid = DXGI_Message_Id(58);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_SetDisplaySurface_NoOwnerDevice}
  DXGI_MSG_IDXGIOutput_SetDisplaySurface_NoOwnerDevice = DXGI_Message_Id(59);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_TakeOwnership_RemoteDeviceNotSupported}
  DXGI_MSG_IDXGIOutput_TakeOwnership_RemoteDeviceNotSupported = DXGI_Message_Id(60);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplayModeList_RemoteDeviceNotSupported}
  DXGI_MSG_IDXGIOutput_GetDisplayModeList_RemoteDeviceNotSupported = DXGI_Message_Id(61);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_RemoteDeviceNotSupported}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_RemoteDeviceNotSupported = DXGI_Message_Id(62);
  {$EXTERNALSYM DXGI_MSG_IDXGIDevice_CreateSurface_InvalidParametersWithpSharedResource}
  DXGI_MSG_IDXGIDevice_CreateSurface_InvalidParametersWithpSharedResource = DXGI_Message_Id(63);
  {$EXTERNALSYM DXGI_MSG_IDXGIObject_GetPrivateData_puiDataSizeIsNULL}
  DXGI_MSG_IDXGIObject_GetPrivateData_puiDataSizeIsNULL = DXGI_Message_Id(64);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Creation_InvalidOutputWindow}
  DXGI_MSG_IDXGISwapChain_Creation_InvalidOutputWindow = DXGI_Message_Id(65);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Release_SwapChainIsFullscreen}
  DXGI_MSG_IDXGISwapChain_Release_SwapChainIsFullscreen = DXGI_Message_Id(66);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_InvalidTargetSurfaceFormat}
  DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_InvalidTargetSurfaceFormat = DXGI_Message_Id(67);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSoftwareAdapter_ModuleIsNULL}
  DXGI_MSG_IDXGIFactory_CreateSoftwareAdapter_ModuleIsNULL = DXGI_Message_Id(68);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_IDXGIDeviceNotSupportedBypConcernedDevice}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_IDXGIDeviceNotSupportedBypConcernedDevice = DXGI_Message_Id(69);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_pModeToMatchOrpClosestMatchIsNULL}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_pModeToMatchOrpClosestMatchIsNULL = DXGI_Message_Id(70);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_ModeHasRefreshRateDenominatorZero}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_ModeHasRefreshRateDenominatorZero = DXGI_Message_Id(71);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_UnknownFormatIsInvalidForConfiguration}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_UnknownFormatIsInvalidForConfiguration = DXGI_Message_Id(72);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeScanlineOrdering}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeScanlineOrdering = DXGI_Message_Id(73);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeScaling}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeScaling = DXGI_Message_Id(74);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeFormatAndDeviceCombination}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeFormatAndDeviceCombination = DXGI_Message_Id(75);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_Creation_CalledFromDllMain}
  DXGI_MSG_IDXGIFactory_Creation_CalledFromDllMain = DXGI_Message_Id(76);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_OutputNotOwnedBySwapChainDevice}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_OutputNotOwnedBySwapChainDevice = DXGI_Message_Id(77);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Creation_InvalidWindowStyle}
  DXGI_MSG_IDXGISwapChain_Creation_InvalidWindowStyle = DXGI_Message_Id(78);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetFrameStatistics_UnsupportedStatistics}
  DXGI_MSG_IDXGISwapChain_GetFrameStatistics_UnsupportedStatistics = DXGI_Message_Id(79);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetContainingOutput_SwapchainAdapterDoesNotControlOutput}
  DXGI_MSG_IDXGISwapChain_GetContainingOutput_SwapchainAdapterDoesNotControlOutput = DXGI_Message_Id(80);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_SetOrGetGammaControl_pArrayIsNULL}
  DXGI_MSG_IDXGIOutput_SetOrGetGammaControl_pArrayIsNULL = DXGI_Message_Id(81);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_FullscreenInvalidForChildWindows}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_FullscreenInvalidForChildWindows = DXGI_Message_Id(82);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_Release_CalledFromDllMain}
  DXGI_MSG_IDXGIFactory_Release_CalledFromDllMain = DXGI_Message_Id(83);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_UnreleasedHDC}
  DXGI_MSG_IDXGISwapChain_Present_UnreleasedHDC = DXGI_Message_Id(84);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_NonPreRotatedAndGDICompatibleFlags}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_NonPreRotatedAndGDICompatibleFlags = DXGI_Message_Id(85);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_NonPreRotatedAndGDICompatibleFlags}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_NonPreRotatedAndGDICompatibleFlags = DXGI_Message_Id(86);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_GetDC_pHdcIsNULL}
  DXGI_MSG_IDXGISurface1_GetDC_pHdcIsNULL = DXGI_Message_Id(87);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_GetDC_SurfaceNotTexture2D}
  DXGI_MSG_IDXGISurface1_GetDC_SurfaceNotTexture2D = DXGI_Message_Id(88);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_GetDC_GDICompatibleFlagNotSet}
  DXGI_MSG_IDXGISurface1_GetDC_GDICompatibleFlagNotSet = DXGI_Message_Id(89);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_GetDC_UnreleasedHDC}
  DXGI_MSG_IDXGISurface1_GetDC_UnreleasedHDC = DXGI_Message_Id(90);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface_Map_NoCPUAccess2}
  DXGI_MSG_IDXGISurface_Map_NoCPUAccess2 = DXGI_Message_Id(91);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_ReleaseDC_GetDCNotCalled}
  DXGI_MSG_IDXGISurface1_ReleaseDC_GetDCNotCalled = DXGI_Message_Id(92);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_ReleaseDC_InvalidRectangleDimensions}
  DXGI_MSG_IDXGISurface1_ReleaseDC_InvalidRectangleDimensions = DXGI_Message_Id(93);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_TakeOwnership_RemoteOutputNotSupported}
  DXGI_MSG_IDXGIOutput_TakeOwnership_RemoteOutputNotSupported = DXGI_Message_Id(94);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_RemoteOutputNotSupported}
  DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_RemoteOutputNotSupported = DXGI_Message_Id(95);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplayModeList_RemoteOutputNotSupported}
  DXGI_MSG_IDXGIOutput_GetDisplayModeList_RemoteOutputNotSupported = DXGI_Message_Id(96);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_pDeviceHasMismatchedDXGIFactory}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_pDeviceHasMismatchedDXGIFactory = DXGI_Message_Id(97);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_NonOptimalFSConfiguration}
  DXGI_MSG_IDXGISwapChain_Present_NonOptimalFSConfiguration = DXGI_Message_Id(98);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_FlipSequentialNotSupportedOnD3D10}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_FlipSequentialNotSupportedOnD3D10 = DXGI_Message_Id(99);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_BufferCountOOBForFlipSequential}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_BufferCountOOBForFlipSequential = DXGI_Message_Id(100);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidFormatForFlipSequential}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidFormatForFlipSequential = DXGI_Message_Id(101);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_MultiSamplingNotSupportedForFlipSequential}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_MultiSamplingNotSupportedForFlipSequential = DXGI_Message_Id(102);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_BufferCountOOBForFlipSequential}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_BufferCountOOBForFlipSequential = DXGI_Message_Id(103);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidFormatForFlipSequential}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidFormatForFlipSequential = DXGI_Message_Id(104);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_PartialPresentationBeforeStandardPresentation}
  DXGI_MSG_IDXGISwapChain_Present_PartialPresentationBeforeStandardPresentation = DXGI_Message_Id(105);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_FullscreenPartialPresentIsInvalid}
  DXGI_MSG_IDXGISwapChain_Present_FullscreenPartialPresentIsInvalid = DXGI_Message_Id(106);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_InvalidPresentTestOrDoNotSequenceFlag}
  DXGI_MSG_IDXGISwapChain_Present_InvalidPresentTestOrDoNotSequenceFlag = DXGI_Message_Id(107);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ScrollInfoWithNoDirtyRectsSpecified}
  DXGI_MSG_IDXGISwapChain_Present_ScrollInfoWithNoDirtyRectsSpecified = DXGI_Message_Id(108);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_EmptyScrollRect}
  DXGI_MSG_IDXGISwapChain_Present_EmptyScrollRect = DXGI_Message_Id(109);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ScrollRectOutOfBackbufferBounds}
  DXGI_MSG_IDXGISwapChain_Present_ScrollRectOutOfBackbufferBounds = DXGI_Message_Id(110);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ScrollRectOutOfBackbufferBoundsWithOffset}
  DXGI_MSG_IDXGISwapChain_Present_ScrollRectOutOfBackbufferBoundsWithOffset = DXGI_Message_Id(111);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_EmptyDirtyRect}
  DXGI_MSG_IDXGISwapChain_Present_EmptyDirtyRect = DXGI_Message_Id(112);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_DirtyRectOutOfBackbufferBounds}
  DXGI_MSG_IDXGISwapChain_Present_DirtyRectOutOfBackbufferBounds = DXGI_Message_Id(113);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_UnsupportedBufferUsageFlags}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_UnsupportedBufferUsageFlags = DXGI_Message_Id(114);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_DoNotSequenceFlagSetButPreviousBufferIsUndefined}
  DXGI_MSG_IDXGISwapChain_Present_DoNotSequenceFlagSetButPreviousBufferIsUndefined = DXGI_Message_Id(115);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_UnsupportedFlags}
  DXGI_MSG_IDXGISwapChain_Present_UnsupportedFlags = DXGI_Message_Id(116);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_FlipModelChainMustResizeOrCreateOnFSTransition}
  DXGI_MSG_IDXGISwapChain_Present_FlipModelChainMustResizeOrCreateOnFSTransition = DXGI_Message_Id(117);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_pRestrictToOutputFromOtherIDXGIFactory}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_pRestrictToOutputFromOtherIDXGIFactory = DXGI_Message_Id(118);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_RestrictOutputNotSupportedOnAdapter}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_RestrictOutputNotSupportedOnAdapter = DXGI_Message_Id(119);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_RestrictToOutputFlagSetButInvalidpRestrictToOutput}
  DXGI_MSG_IDXGISwapChain_Present_RestrictToOutputFlagSetButInvalidpRestrictToOutput = DXGI_Message_Id(120);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_RestrictToOutputFlagdWithFullscreen}
  DXGI_MSG_IDXGISwapChain_Present_RestrictToOutputFlagdWithFullscreen = DXGI_Message_Id(121);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_RestrictOutputFlagWithStaleSwapChain}
  DXGI_MSG_IDXGISwapChain_Present_RestrictOutputFlagWithStaleSwapChain = DXGI_Message_Id(122);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_OtherFlagsCausingInvalidPresentTestFlag}
  DXGI_MSG_IDXGISwapChain_Present_OtherFlagsCausingInvalidPresentTestFlag = DXGI_Message_Id(123);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_UnavailableInSession0}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_UnavailableInSession0 = DXGI_Message_Id(124);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_MakeWindowAssociation_UnavailableInSession0}
  DXGI_MSG_IDXGIFactory_MakeWindowAssociation_UnavailableInSession0 = DXGI_Message_Id(125);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_GetWindowAssociation_UnavailableInSession0}
  DXGI_MSG_IDXGIFactory_GetWindowAssociation_UnavailableInSession0 = DXGI_Message_Id(126);
  {$EXTERNALSYM DXGI_MSG_IDXGIAdapter_EnumOutputs_UnavailableInSession0}
  DXGI_MSG_IDXGIAdapter_EnumOutputs_UnavailableInSession0 = DXGI_Message_Id(127);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreationOrSetFullscreenState_StereoDisabled}
  DXGI_MSG_IDXGISwapChain_CreationOrSetFullscreenState_StereoDisabled = DXGI_Message_Id(128);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_UnregisterStatus_CookieNotFound}
  DXGI_MSG_IDXGIFactory2_UnregisterStatus_CookieNotFound = DXGI_Message_Id(129);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithoutFSOrOverlay}
  DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithoutFSOrOverlay = DXGI_Message_Id(130);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithoutFlipSequential}
  DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithoutFlipSequential = DXGI_Message_Id(131);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ProtectedContentWithRDPDriver}
  DXGI_MSG_IDXGISwapChain_Present_ProtectedContentWithRDPDriver = DXGI_Message_Id(132);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithDWMOffOrInvalidDisplayAffinity}
  DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithDWMOffOrInvalidDisplayAffinity = DXGI_Message_Id(133);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_WidthOrHeightIsZero}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_WidthOrHeightIsZero = DXGI_Message_Id(134);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_OnlyFlipSequentialSupported}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_OnlyFlipSequentialSupported = DXGI_Message_Id(135);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_UnsupportedOnAdapter}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_UnsupportedOnAdapter = DXGI_Message_Id(136);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_UnsupportedOnWindows7}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_UnsupportedOnWindows7 = DXGI_Message_Id(137);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_FSTransitionWithCompositionSwapChain}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_FSTransitionWithCompositionSwapChain = DXGI_Message_Id(138);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeTarget_InvalidWithCompositionSwapChain}
  DXGI_MSG_IDXGISwapChain_ResizeTarget_InvalidWithCompositionSwapChain = DXGI_Message_Id(139);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_WidthOrHeightIsZero}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_WidthOrHeightIsZero = DXGI_Message_Id(140);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingNoneIsFlipModelOnly}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingNoneIsFlipModelOnly = DXGI_Message_Id(141);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingUnrecognized}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingUnrecognized = DXGI_Message_Id(142);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyFullscreenUnsupported}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyFullscreenUnsupported = DXGI_Message_Id(143);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyUnsupported}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyUnsupported = DXGI_Message_Id(144);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_RestartIsFullscreenOnly}
  DXGI_MSG_IDXGISwapChain_Present_RestartIsFullscreenOnly = DXGI_Message_Id(145);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_ProtectedWindowlessPresentationRequiresDisplayOnly}
  DXGI_MSG_IDXGISwapChain_Present_ProtectedWindowlessPresentationRequiresDisplayOnly = DXGI_Message_Id(146);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_DisplayOnlyUnsupported}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_DisplayOnlyUnsupported = DXGI_Message_Id(147);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain1_SetBackgroundColor_OutOfRange}
  DXGI_MSG_IDXGISwapChain1_SetBackgroundColor_OutOfRange = DXGI_Message_Id(148);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyFullscreenUnsupported}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyFullscreenUnsupported = DXGI_Message_Id(149);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyUnsupported}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyUnsupported = DXGI_Message_Id(150);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapchain_Present_ScrollUnsupported}
  DXGI_MSG_IDXGISwapchain_Present_ScrollUnsupported = DXGI_Message_Id(151);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain1_SetRotation_UnsupportedOS}
  DXGI_MSG_IDXGISwapChain1_SetRotation_UnsupportedOS = DXGI_Message_Id(152);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain1_GetRotation_UnsupportedOS}
  DXGI_MSG_IDXGISwapChain1_GetRotation_UnsupportedOS = DXGI_Message_Id(153);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapchain_Present_FullscreenRotation}
  DXGI_MSG_IDXGISwapchain_Present_FullscreenRotation = DXGI_Message_Id(154);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_PartialPresentationWithMSAABuffers}
  DXGI_MSG_IDXGISwapChain_Present_PartialPresentationWithMSAABuffers = DXGI_Message_Id(155);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain1_SetRotation_FlipSequentialRequired}
  DXGI_MSG_IDXGISwapChain1_SetRotation_FlipSequentialRequired = DXGI_Message_Id(156);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain1_SetRotation_InvalidRotation}
  DXGI_MSG_IDXGISwapChain1_SetRotation_InvalidRotation = DXGI_Message_Id(157);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain1_GetRotation_FlipSequentialRequired}
  DXGI_MSG_IDXGISwapChain1_GetRotation_FlipSequentialRequired = DXGI_Message_Id(158);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetHwnd_WrongType}
  DXGI_MSG_IDXGISwapChain_GetHwnd_WrongType = DXGI_Message_Id(159);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetCompositionSurface_WrongType}
  DXGI_MSG_IDXGISwapChain_GetCompositionSurface_WrongType = DXGI_Message_Id(160);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetCoreWindow_WrongType}
  DXGI_MSG_IDXGISwapChain_GetCoreWindow_WrongType = DXGI_Message_Id(161);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetFullscreenDesc_NonHwnd}
  DXGI_MSG_IDXGISwapChain_GetFullscreenDesc_NonHwnd = DXGI_Message_Id(162);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_CoreWindow}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_CoreWindow = DXGI_Message_Id(163);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_UnsupportedOnWindows7}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_UnsupportedOnWindows7 = DXGI_Message_Id(164);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_pWindowIsNULL}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_pWindowIsNULL = DXGI_Message_Id(165);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_FSUnsupportedForModernApps}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_FSUnsupportedForModernApps = DXGI_Message_Id(166);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_MakeWindowAssociation_ModernApp}
  DXGI_MSG_IDXGIFactory_MakeWindowAssociation_ModernApp = DXGI_Message_Id(167);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeTarget_ModernApp}
  DXGI_MSG_IDXGISwapChain_ResizeTarget_ModernApp = DXGI_Message_Id(168);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeTarget_pNewTargetParametersIsNULL}
  DXGI_MSG_IDXGISwapChain_ResizeTarget_pNewTargetParametersIsNULL = DXGI_Message_Id(169);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_SetDisplaySurface_ModernApp}
  DXGI_MSG_IDXGIOutput_SetDisplaySurface_ModernApp = DXGI_Message_Id(170);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_TakeOwnership_ModernApp}
  DXGI_MSG_IDXGIOutput_TakeOwnership_ModernApp = DXGI_Message_Id(171);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_pWindowIsInvalid}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_pWindowIsInvalid = DXGI_Message_Id(172);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCompositionSurface_InvalidHandle}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCompositionSurface_InvalidHandle = DXGI_Message_Id(173);
  {$EXTERNALSYM DXGI_MSG_IDXGISurface1_GetDC_ModernApp}
  DXGI_MSG_IDXGISurface1_GetDC_ModernApp = DXGI_Message_Id(174);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingNoneRequiresWindows8OrNewer}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingNoneRequiresWindows8OrNewer = DXGI_Message_Id(175);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoAndPreferRight}
  DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoAndPreferRight = DXGI_Message_Id(176);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoOrPreferRightWithDoNotSequence}
  DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoOrPreferRightWithDoNotSequence = DXGI_Message_Id(177);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoOrPreferRightWithoutStereo}
  DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoOrPreferRightWithoutStereo = DXGI_Message_Id(178);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoUnsupported}
  DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoUnsupported = DXGI_Message_Id(179);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_ArraySizeMismatch}
  DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_ArraySizeMismatch = DXGI_Message_Id(180);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_PartialPresentationWithSwapEffectDiscard}
  DXGI_MSG_IDXGISwapChain_Present_PartialPresentationWithSwapEffectDiscard = DXGI_Message_Id(181);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaUnrecognized}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaUnrecognized = DXGI_Message_Id(182);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaIsWindowlessOnly}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaIsWindowlessOnly = DXGI_Message_Id(183);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaIsFlipModelOnly}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaIsFlipModelOnly = DXGI_Message_Id(184);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_RestrictToOutputAdapterMismatch}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_RestrictToOutputAdapterMismatch = DXGI_Message_Id(185);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyOnLegacy}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyOnLegacy = DXGI_Message_Id(186);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyOnLegacy}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyOnLegacy = DXGI_Message_Id(187);
  {$EXTERNALSYM DXGI_MSG_IDXGIResource1_CreateSubresourceSurface_InvalidIndex}
  DXGI_MSG_IDXGIResource1_CreateSubresourceSurface_InvalidIndex = DXGI_Message_Id(188);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_InvalidScaling}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_InvalidScaling = DXGI_Message_Id(189);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForCoreWindow_InvalidSwapEffect}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForCoreWindow_InvalidSwapEffect = DXGI_Message_Id(190);
  {$EXTERNALSYM DXGI_MSG_IDXGIResource1_CreateSharedHandle_UnsupportedOS}
  DXGI_MSG_IDXGIResource1_CreateSharedHandle_UnsupportedOS = DXGI_Message_Id(191);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_RegisterOcclusionStatusWindow_UnsupportedOS}
  DXGI_MSG_IDXGIFactory2_RegisterOcclusionStatusWindow_UnsupportedOS = DXGI_Message_Id(192);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_RegisterOcclusionStatusEvent_UnsupportedOS}
  DXGI_MSG_IDXGIFactory2_RegisterOcclusionStatusEvent_UnsupportedOS = DXGI_Message_Id(193);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_DuplicateOutput_UnsupportedOS}
  DXGI_MSG_IDXGIOutput1_DuplicateOutput_UnsupportedOS = DXGI_Message_Id(194);
  {$EXTERNALSYM DXGI_MSG_IDXGIDisplayControl_IsStereoEnabled_UnsupportedOS}
  DXGI_MSG_IDXGIDisplayControl_IsStereoEnabled_UnsupportedOS = DXGI_Message_Id(195);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_InvalidAlphaMode}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_InvalidAlphaMode = DXGI_Message_Id(196);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_InvalidResource}
  DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_InvalidResource = DXGI_Message_Id(197);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_InvalidLUID}
  DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_InvalidLUID = DXGI_Message_Id(198);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_UnsupportedOS}
  DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_UnsupportedOS = DXGI_Message_Id(199);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_2DOnly}
  DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_2DOnly = DXGI_Message_Id(200);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_StagingOnly}
  DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_StagingOnly = DXGI_Message_Id(201);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_NeedCPUAccessWrite}
  DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_NeedCPUAccessWrite = DXGI_Message_Id(202);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_NoShared}
  DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_NoShared = DXGI_Message_Id(203);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_OnlyMipLevels1}
  DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_OnlyMipLevels1 = DXGI_Message_Id(204);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_MappedOrOfferedResource}
  DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_MappedOrOfferedResource = DXGI_Message_Id(205);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_FSUnsupportedForModernApps}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_FSUnsupportedForModernApps = DXGI_Message_Id(206);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_FailedToGoFSButNonPreRotated}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_FailedToGoFSButNonPreRotated = DXGI_Message_Id(207);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainOrRegisterOcclusionStatus_BlitModelUsedWhileRegisteredForOcclusionStatusEvents}
  DXGI_MSG_IDXGIFactory_CreateSwapChainOrRegisterOcclusionStatus_BlitModelUsedWhileRegisteredForOcclusionStatusEvents = DXGI_Message_Id(208);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_BlitModelUsedWhileRegisteredForOcclusionStatusEvents}
  DXGI_MSG_IDXGISwapChain_Present_BlitModelUsedWhileRegisteredForOcclusionStatusEvents = DXGI_Message_Id(209);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_WaitableSwapChainsAreFlipModelOnly}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_WaitableSwapChainsAreFlipModelOnly = DXGI_Message_Id(210);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_WaitableSwapChainsAreNotFullscreen}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_WaitableSwapChainsAreNotFullscreen = DXGI_Message_Id(211);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_Waitable}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_Waitable = DXGI_Message_Id(212);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveWaitableFlag}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveWaitableFlag = DXGI_Message_Id(213);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetFrameLatencyWaitableObject_OnlyWaitable}
  DXGI_MSG_IDXGISwapChain_GetFrameLatencyWaitableObject_OnlyWaitable = DXGI_Message_Id(214);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetMaximumFrameLatency_OnlyWaitable}
  DXGI_MSG_IDXGISwapChain_GetMaximumFrameLatency_OnlyWaitable = DXGI_Message_Id(215);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetMaximumFrameLatency_pMaxLatencyIsNULL}
  DXGI_MSG_IDXGISwapChain_GetMaximumFrameLatency_pMaxLatencyIsNULL = DXGI_Message_Id(216);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMaximumFrameLatency_OnlyWaitable}
  DXGI_MSG_IDXGISwapChain_SetMaximumFrameLatency_OnlyWaitable = DXGI_Message_Id(217);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMaximumFrameLatency_MaxLatencyIsOutOfBounds}
  DXGI_MSG_IDXGISwapChain_SetMaximumFrameLatency_MaxLatencyIsOutOfBounds = DXGI_Message_Id(218);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_ForegroundIsCoreWindowOnly}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_ForegroundIsCoreWindowOnly = DXGI_Message_Id(219);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_ForegroundUnsupportedOnAdapter}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_ForegroundUnsupportedOnAdapter = DXGI_Message_Id(220);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_InvalidScaling}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_InvalidScaling = DXGI_Message_Id(221);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_InvalidAlphaMode}
  DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_InvalidAlphaMode = DXGI_Message_Id(222);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveForegroundFlag}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveForegroundFlag = DXGI_Message_Id(223);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixPointerCannotBeNull}
  DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixPointerCannotBeNull = DXGI_Message_Id(224);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMatrixTransform_RequiresCompositionSwapChain}
  DXGI_MSG_IDXGISwapChain_SetMatrixTransform_RequiresCompositionSwapChain = DXGI_Message_Id(225);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixMustBeFinite}
  DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixMustBeFinite = DXGI_Message_Id(226);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixMustBeTranslateAndOrScale}
  DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixMustBeTranslateAndOrScale = DXGI_Message_Id(227);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetMatrixTransform_MatrixPointerCannotBeNull}
  DXGI_MSG_IDXGISwapChain_GetMatrixTransform_MatrixPointerCannotBeNull = DXGI_Message_Id(228);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetMatrixTransform_RequiresCompositionSwapChain}
  DXGI_MSG_IDXGISwapChain_GetMatrixTransform_RequiresCompositionSwapChain = DXGI_Message_Id(229);
  {$EXTERNALSYM DXGI_MSG_DXGIGetDebugInterface1_NULL_ppDebug}
  DXGI_MSG_DXGIGetDebugInterface1_NULL_ppDebug = DXGI_Message_Id(230);
  {$EXTERNALSYM DXGI_MSG_DXGIGetDebugInterface1_InvalidFlags}
  DXGI_MSG_DXGIGetDebugInterface1_InvalidFlags = DXGI_Message_Id(231);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_Decode}
  DXGI_MSG_IDXGISwapChain_Present_Decode = DXGI_Message_Id(232);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_Decode}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_Decode = DXGI_Message_Id(233);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetSourceSize_FlipModel}
  DXGI_MSG_IDXGISwapChain_SetSourceSize_FlipModel = DXGI_Message_Id(234);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetSourceSize_Decode}
  DXGI_MSG_IDXGISwapChain_SetSourceSize_Decode = DXGI_Message_Id(235);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetSourceSize_WidthHeight}
  DXGI_MSG_IDXGISwapChain_SetSourceSize_WidthHeight = DXGI_Message_Id(236);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetSourceSize_NullPointers}
  DXGI_MSG_IDXGISwapChain_GetSourceSize_NullPointers = DXGI_Message_Id(237);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetSourceSize_Decode}
  DXGI_MSG_IDXGISwapChain_GetSourceSize_Decode = DXGI_Message_Id(238);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_SetColorSpace_InvalidFlags}
  DXGI_MSG_IDXGIDecodeSwapChain_SetColorSpace_InvalidFlags = DXGI_Message_Id(239);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_SetSourceRect_InvalidRect}
  DXGI_MSG_IDXGIDecodeSwapChain_SetSourceRect_InvalidRect = DXGI_Message_Id(240);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_SetTargetRect_InvalidRect}
  DXGI_MSG_IDXGIDecodeSwapChain_SetTargetRect_InvalidRect = DXGI_Message_Id(241);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_SetDestSize_InvalidSize}
  DXGI_MSG_IDXGIDecodeSwapChain_SetDestSize_InvalidSize = DXGI_Message_Id(242);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_GetSourceRect_InvalidPointer}
  DXGI_MSG_IDXGIDecodeSwapChain_GetSourceRect_InvalidPointer = DXGI_Message_Id(243);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_GetTargetRect_InvalidPointer}
  DXGI_MSG_IDXGIDecodeSwapChain_GetTargetRect_InvalidPointer = DXGI_Message_Id(244);
  {$EXTERNALSYM DXGI_MSG_IDXGIDecodeSwapChain_GetDestSize_InvalidPointer}
  DXGI_MSG_IDXGIDecodeSwapChain_GetDestSize_InvalidPointer = DXGI_Message_Id(245);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_PresentBuffer_YUV}
  DXGI_MSG_IDXGISwapChain_PresentBuffer_YUV = DXGI_Message_Id(246);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetSourceSize_YUV}
  DXGI_MSG_IDXGISwapChain_SetSourceSize_YUV = DXGI_Message_Id(247);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetSourceSize_YUV}
  DXGI_MSG_IDXGISwapChain_GetSourceSize_YUV = DXGI_Message_Id(248);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetMatrixTransform_YUV}
  DXGI_MSG_IDXGISwapChain_SetMatrixTransform_YUV = DXGI_Message_Id(249);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_GetMatrixTransform_YUV}
  DXGI_MSG_IDXGISwapChain_GetMatrixTransform_YUV = DXGI_Message_Id(250);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_PartialPresentation_YUV}
  DXGI_MSG_IDXGISwapChain_Present_PartialPresentation_YUV = DXGI_Message_Id(251);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveFlag_YUV}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveFlag_YUV = DXGI_Message_Id(252);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_Alignment_YUV}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_Alignment_YUV = DXGI_Message_Id(253);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_ShaderInputUnsupported_YUV}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_ShaderInputUnsupported_YUV = DXGI_Message_Id(254);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput3_CheckOverlaySupport_NullPointers}
  DXGI_MSG_IDXGIOutput3_CheckOverlaySupport_NullPointers = DXGI_Message_Id(255);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput3_CheckOverlaySupport_IDXGIDeviceNotSupportedBypConcernedDevice}
  DXGI_MSG_IDXGIOutput3_CheckOverlaySupport_IDXGIDeviceNotSupportedBypConcernedDevice = DXGI_Message_Id(256);
  {$EXTERNALSYM DXGI_MSG_IDXGIAdapter_EnumOutputs2_InvalidEnumOutputs2Flag}
  DXGI_MSG_IDXGIAdapter_EnumOutputs2_InvalidEnumOutputs2Flag = DXGI_Message_Id(257);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreationOrSetFullscreenState_FSUnsupportedForFlipDiscard}
  DXGI_MSG_IDXGISwapChain_CreationOrSetFullscreenState_FSUnsupportedForFlipDiscard = DXGI_Message_Id(258);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput4_CheckOverlayColorSpaceSupport_NullPointers}
  DXGI_MSG_IDXGIOutput4_CheckOverlayColorSpaceSupport_NullPointers = DXGI_Message_Id(259);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput4_CheckOverlayColorSpaceSupport_IDXGIDeviceNotSupportedBypConcernedDevice}
  DXGI_MSG_IDXGIOutput4_CheckOverlayColorSpaceSupport_IDXGIDeviceNotSupportedBypConcernedDevice = DXGI_Message_Id(260);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain3_CheckColorSpaceSupport_NullPointers}
  DXGI_MSG_IDXGISwapChain3_CheckColorSpaceSupport_NullPointers = DXGI_Message_Id(261);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain3_SetColorSpace1_InvalidColorSpace}
  DXGI_MSG_IDXGISwapChain3_SetColorSpace1_InvalidColorSpace = DXGI_Message_Id(262);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidHwProtect}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidHwProtect = DXGI_Message_Id(263);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_HwProtectUnsupported}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_HwProtectUnsupported = DXGI_Message_Id(264);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidHwProtect}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidHwProtect = DXGI_Message_Id(265);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_HwProtectUnsupported}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_HwProtectUnsupported = DXGI_Message_Id(266);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers1_D3D12Only}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers1_D3D12Only = DXGI_Message_Id(267);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers1_FlipModel}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers1_FlipModel = DXGI_Message_Id(268);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers1_NodeMaskAndQueueRequired}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers1_NodeMaskAndQueueRequired = DXGI_Message_Id(269);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_CreateSwapChain_InvalidHwProtectGdiFlag}
  DXGI_MSG_IDXGISwapChain_CreateSwapChain_InvalidHwProtectGdiFlag = DXGI_Message_Id(270);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidHwProtectGdiFlag}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidHwProtectGdiFlag = DXGI_Message_Id(271);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_10BitFormatNotSupported}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_10BitFormatNotSupported = DXGI_Message_Id(272);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_FlipSwapEffectRequired}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_FlipSwapEffectRequired = DXGI_Message_Id(273);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidDevice}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidDevice = DXGI_Message_Id(274);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_TakeOwnership_Unsupported}
  DXGI_MSG_IDXGIOutput_TakeOwnership_Unsupported = DXGI_Message_Id(275);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidQueue}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidQueue = DXGI_Message_Id(276);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain3_ResizeBuffers1_InvalidQueue}
  DXGI_MSG_IDXGISwapChain3_ResizeBuffers1_InvalidQueue = DXGI_Message_Id(277);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChainForHwnd_InvalidScaling}
  DXGI_MSG_IDXGIFactory_CreateSwapChainForHwnd_InvalidScaling = DXGI_Message_Id(278);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidSize}
  DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidSize = DXGI_Message_Id(279);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidPointer}
  DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidPointer = DXGI_Message_Id(280);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidType}
  DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidType = DXGI_Message_Id(281);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_FullscreenAllowTearingIsInvalid}
  DXGI_MSG_IDXGISwapChain_Present_FullscreenAllowTearingIsInvalid = DXGI_Message_Id(282);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_AllowTearingRequiresPresentIntervalZero}
  DXGI_MSG_IDXGISwapChain_Present_AllowTearingRequiresPresentIntervalZero = DXGI_Message_Id(283);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_AllowTearingRequiresCreationFlag}
  DXGI_MSG_IDXGISwapChain_Present_AllowTearingRequiresCreationFlag = DXGI_Message_Id(284);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveAllowTearingFlag}
  DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveAllowTearingFlag = DXGI_Message_Id(285);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_AllowTearingFlagIsFlipModelOnly}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_AllowTearingFlagIsFlipModelOnly = DXGI_Message_Id(286);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CheckFeatureSupport_InvalidFeature}
  DXGI_MSG_IDXGIFactory_CheckFeatureSupport_InvalidFeature = DXGI_Message_Id(287);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CheckFeatureSupport_InvalidSize}
  DXGI_MSG_IDXGIFactory_CheckFeatureSupport_InvalidSize = DXGI_Message_Id(288);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput6_CheckHardwareCompositionSupport_NullPointer}
  DXGI_MSG_IDXGIOutput6_CheckHardwareCompositionSupport_NullPointer = DXGI_Message_Id(289);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_SetFullscreenState_PerMonitorDpiShimApplied}
  DXGI_MSG_IDXGISwapChain_SetFullscreenState_PerMonitorDpiShimApplied = DXGI_Message_Id(290);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_DuplicateOutput_PerMonitorDpiShimApplied}
  DXGI_MSG_IDXGIOutput_DuplicateOutput_PerMonitorDpiShimApplied = DXGI_Message_Id(291);
  {$EXTERNALSYM DXGI_MSG_IDXGIOutput_DuplicateOutput1_PerMonitorDpiRequired}
  DXGI_MSG_IDXGIOutput_DuplicateOutput1_PerMonitorDpiRequired = DXGI_Message_Id(292);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory7_UnregisterAdaptersChangedEvent_CookieNotFound}
  DXGI_MSG_IDXGIFactory7_UnregisterAdaptersChangedEvent_CookieNotFound = DXGI_Message_Id(293);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_LegacyBltModelSwapEffect}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_LegacyBltModelSwapEffect = DXGI_Message_Id(294);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain4_SetHDRMetaData_MetadataUnchanged}
  DXGI_MSG_IDXGISwapChain4_SetHDRMetaData_MetadataUnchanged = DXGI_Message_Id(295);
  {$EXTERNALSYM DXGI_MSG_IDXGISwapChain_Present_11On12_Released_Resource}
  DXGI_MSG_IDXGISwapChain_Present_11On12_Released_Resource = DXGI_Message_Id(296);
  {$EXTERNALSYM DXGI_MSG_IDXGIFactory_CreateSwapChain_MultipleSwapchainRefToSurface_DeferredDtr}
  DXGI_MSG_IDXGIFactory_CreateSwapChain_MultipleSwapchainRefToSurface_DeferredDtr = DXGI_Message_Id(297);

  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_NotForegroundWindow}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_NotForegroundWindow = DXGI_Message_Id(1000);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_DISCARD_BufferCount}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_DISCARD_BufferCount = DXGI_Message_Id(1001);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_SetFullscreenState_NotAvailable}
  DXGI_MSG_Phone_IDXGISwapChain_SetFullscreenState_NotAvailable = DXGI_Message_Id(1002);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_ResizeBuffers_NotAvailable}
  DXGI_MSG_Phone_IDXGISwapChain_ResizeBuffers_NotAvailable = DXGI_Message_Id(1003);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_ResizeTarget_NotAvailable}
  DXGI_MSG_Phone_IDXGISwapChain_ResizeTarget_NotAvailable = DXGI_Message_Id(1004);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidLayerIndex}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidLayerIndex = DXGI_Message_Id(1005);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_MultipleLayerIndex}
  DXGI_MSG_Phone_IDXGISwapChain_Present_MultipleLayerIndex = DXGI_Message_Id(1006);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidLayerFlag}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidLayerFlag = DXGI_Message_Id(1007);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidRotation}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidRotation = DXGI_Message_Id(1008);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidBlend}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidBlend = DXGI_Message_Id(1009);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidResource}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidResource = DXGI_Message_Id(1010);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidMultiPlaneOverlayResource}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidMultiPlaneOverlayResource = DXGI_Message_Id(1011);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidIndexForPrimary}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidIndexForPrimary = DXGI_Message_Id(1012);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidIndexForOverlay}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidIndexForOverlay = DXGI_Message_Id(1013);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidSubResourceIndex}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidSubResourceIndex = DXGI_Message_Id(1014);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidSourceRect}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidSourceRect = DXGI_Message_Id(1015);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidDestinationRect}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidDestinationRect = DXGI_Message_Id(1016);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_MultipleResource}
  DXGI_MSG_Phone_IDXGISwapChain_Present_MultipleResource = DXGI_Message_Id(1017);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_NotSharedResource}
  DXGI_MSG_Phone_IDXGISwapChain_Present_NotSharedResource = DXGI_Message_Id(1018);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidFlag}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidFlag = DXGI_Message_Id(1019);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidInterval}
  DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidInterval = DXGI_Message_Id(1020);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_MSAA_NotSupported}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_MSAA_NotSupported = DXGI_Message_Id(1021);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_ScalingAspectRatioStretch_Supported_ModernApp}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_ScalingAspectRatioStretch_Supported_ModernApp = DXGI_Message_Id(1022);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_GetFrameStatistics_NotAvailable_ModernApp}
  DXGI_MSG_Phone_IDXGISwapChain_GetFrameStatistics_NotAvailable_ModernApp = DXGI_Message_Id(1023);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present_ReplaceInterval0With1}
  DXGI_MSG_Phone_IDXGISwapChain_Present_ReplaceInterval0With1 = DXGI_Message_Id(1024);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FailedRegisterWithCompositor}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FailedRegisterWithCompositor = DXGI_Message_Id(1025);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_NotForegroundWindow_AtRendering}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_NotForegroundWindow_AtRendering = DXGI_Message_Id(1026);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FLIP_SEQUENTIAL_BufferCount}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FLIP_SEQUENTIAL_BufferCount = DXGI_Message_Id(1027);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FLIP_Modern_CoreWindow_Only}
  DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FLIP_Modern_CoreWindow_Only = DXGI_Message_Id(1028);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_Present1_RequiresOverlays}
  DXGI_MSG_Phone_IDXGISwapChain_Present1_RequiresOverlays = DXGI_Message_Id(1029);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_SetBackgroundColor_FlipSequentialRequired}
  DXGI_MSG_Phone_IDXGISwapChain_SetBackgroundColor_FlipSequentialRequired = DXGI_Message_Id(1030);
  {$EXTERNALSYM DXGI_MSG_Phone_IDXGISwapChain_GetBackgroundColor_FlipSequentialRequired}
  DXGI_MSG_Phone_IDXGISwapChain_GetBackgroundColor_FlipSequentialRequired = DXGI_Message_Id(1031);

// #endif NTDDI_VERSION >= NTDDI_WIN7

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

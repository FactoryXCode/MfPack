// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - DXGI
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DXGIMessages.pas
// Kind: Pascal / Delphi unit
// Release date: 27-02-2020
// Language: ENU
//
// Revision Version: 2.6.4
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
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
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
unit MfPack.DXGIMessages;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "DXGIMessages.h"'}
  {$HPPEMIT ''}

interface

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'MfPack.inc'}


// #if NTDDI_VERSION >= NTDDI_WIN7

type

  PDXGIMessageId = ^PDXGI_Message_Id;
  PDXGI_Message_Id = ^DXGI_Message_Id;
  DXGI_Message_Id = (
    DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_InvalidOutputWindow = 0,
    DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_BufferWidthInferred,
    DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_BufferHeightInferred,
    DXGI_MSG_IDXGISwapChain_CreationOrResizeBuffers_NoScanoutFlagChanged,
    DXGI_MSG_IDXGISwapChain_Creation_MaxBufferCountExceeded,
    DXGI_MSG_IDXGISwapChain_Creation_TooFewBuffers,
    DXGI_MSG_IDXGISwapChain_Creation_NoOutputWindow,
    DXGI_MSG_IDXGISwapChain_Destruction_OtherMethodsCalled,
    DXGI_MSG_IDXGISwapChain_GetDesc_pDescIsNULL,
    DXGI_MSG_IDXGISwapChain_GetBuffer_ppSurfaceIsNULL,
    DXGI_MSG_IDXGISwapChain_GetBuffer_NoAllocatedBuffers,
    DXGI_MSG_IDXGISwapChain_GetBuffer_iBufferMustBeZero,
    DXGI_MSG_IDXGISwapChain_GetBuffer_iBufferOOB,
    DXGI_MSG_IDXGISwapChain_GetContainingOutput_ppOutputIsNULL,
    DXGI_MSG_IDXGISwapChain_Present_SyncIntervalOOB,
    DXGI_MSG_IDXGISwapChain_Present_InvalidNonPreRotatedFlag,
    DXGI_MSG_IDXGISwapChain_Present_NoAllocatedBuffers,
    DXGI_MSG_IDXGISwapChain_Present_GetDXGIAdapterFailed,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_BufferCountOOB,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_UnreleasedReferences,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidSwapChainFlag,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidNonPreRotatedFlag,
    DXGI_MSG_IDXGISwapChain_ResizeTarget_RefreshRateDivideByZero,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_InvalidTarget,
    DXGI_MSG_IDXGISwapChain_GetFrameStatistics_pStatsIsNULL,
    DXGI_MSG_IDXGISwapChain_GetLastPresentCount_pLastPresentCountIsNULL,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_RemoteNotSupported,
    DXGI_MSG_IDXGIOutput_TakeOwnership_FailedToAcquireFullscreenMutex,
    DXGI_MSG_IDXGIFactory_CreateSoftwareAdapter_ppAdapterInterfaceIsNULL,
    DXGI_MSG_IDXGIFactory_EnumAdapters_ppAdapterInterfaceIsNULL,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_ppSwapChainIsNULL,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_pDescIsNULL,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_UnknownSwapEffect,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidFlags,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_NonPreRotatedFlagAndWindowed,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_NullDeviceInterface,
    DXGI_MSG_IDXGIFactory_GetWindowAssociation_phWndIsNULL,
    DXGI_MSG_IDXGIFactory_MakeWindowAssociation_InvalidFlags,
    DXGI_MSG_IDXGISurface_Map_InvalidSurface,
    DXGI_MSG_IDXGISurface_Map_FlagsSetToZero,
    DXGI_MSG_IDXGISurface_Map_DiscardAndReadFlagSet,
    DXGI_MSG_IDXGISurface_Map_DiscardButNotWriteFlagSet,
    DXGI_MSG_IDXGISurface_Map_NoCPUAccess,
    DXGI_MSG_IDXGISurface_Map_ReadFlagSetButCPUAccessIsDynamic,
    DXGI_MSG_IDXGISurface_Map_DiscardFlagSetButCPUAccessIsNotDynamic,
    DXGI_MSG_IDXGIOutput_GetDisplayModeList_pNumModesIsNULL,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_ModeHasInvalidWidthOrHeight,
    DXGI_MSG_IDXGIOutput_GetCammaControlCapabilities_NoOwnerDevice,
    DXGI_MSG_IDXGIOutput_TakeOwnership_pDeviceIsNULL,
    DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_NoOwnerDevice,
    DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_pDestinationIsNULL,
    DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_MapOfDestinationFailed,
    DXGI_MSG_IDXGIOutput_GetFrameStatistics_NoOwnerDevice,
    DXGI_MSG_IDXGIOutput_GetFrameStatistics_pStatsIsNULL,
    DXGI_MSG_IDXGIOutput_SetGammaControl_NoOwnerDevice,
    DXGI_MSG_IDXGIOutput_GetGammaControl_NoOwnerDevice,
    DXGI_MSG_IDXGIOutput_GetGammaControl_NoGammaControls,
    DXGI_MSG_IDXGIOutput_SetDisplaySurface_IDXGIResourceNotSupportedBypPrimary,
    DXGI_MSG_IDXGIOutput_SetDisplaySurface_pPrimaryIsInvalid,
    DXGI_MSG_IDXGIOutput_SetDisplaySurface_NoOwnerDevice,
    DXGI_MSG_IDXGIOutput_TakeOwnership_RemoteDeviceNotSupported,
    DXGI_MSG_IDXGIOutput_GetDisplayModeList_RemoteDeviceNotSupported,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_RemoteDeviceNotSupported,
    DXGI_MSG_IDXGIDevice_CreateSurface_InvalidParametersWithpSharedResource,
    DXGI_MSG_IDXGIObject_GetPrivateData_puiDataSizeIsNULL,
    DXGI_MSG_IDXGISwapChain_Creation_InvalidOutputWindow,
    DXGI_MSG_IDXGISwapChain_Release_SwapChainIsFullscreen,
    DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_InvalidTargetSurfaceFormat,
    DXGI_MSG_IDXGIFactory_CreateSoftwareAdapter_ModuleIsNULL,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_IDXGIDeviceNotSupportedBypConcernedDevice,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_pModeToMatchOrpClosestMatchIsNULL,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_ModeHasRefreshRateDenominatorZero,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_UnknownFormatIsInvalidForConfiguration,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeScanlineOrdering,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeScaling,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_InvalidDisplayModeFormatAndDeviceCombination,
    DXGI_MSG_IDXGIFactory_Creation_CalledFromDllMain,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_OutputNotOwnedBySwapChainDevice,
    DXGI_MSG_IDXGISwapChain_Creation_InvalidWindowStyle,
    DXGI_MSG_IDXGISwapChain_GetFrameStatistics_UnsupportedStatistics,
    DXGI_MSG_IDXGISwapChain_GetContainingOutput_SwapchainAdapterDoesNotControlOutput,
    DXGI_MSG_IDXGIOutput_SetOrGetGammaControl_pArrayIsNULL,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_FullscreenInvalidForChildWindows,
    DXGI_MSG_IDXGIFactory_Release_CalledFromDllMain,
    DXGI_MSG_IDXGISwapChain_Present_UnreleasedHDC,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_NonPreRotatedAndGDICompatibleFlags,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_NonPreRotatedAndGDICompatibleFlags,
    DXGI_MSG_IDXGISurface1_GetDC_pHdcIsNULL,
    DXGI_MSG_IDXGISurface1_GetDC_SurfaceNotTexture2D,
    DXGI_MSG_IDXGISurface1_GetDC_GDICompatibleFlagNotSet,
    DXGI_MSG_IDXGISurface1_GetDC_UnreleasedHDC,
    DXGI_MSG_IDXGISurface_Map_NoCPUAccess2,
    DXGI_MSG_IDXGISurface1_ReleaseDC_GetDCNotCalled,
    DXGI_MSG_IDXGISurface1_ReleaseDC_InvalidRectangleDimensions,
    DXGI_MSG_IDXGIOutput_TakeOwnership_RemoteOutputNotSupported,
    DXGI_MSG_IDXGIOutput_FindClosestMatchingMode_RemoteOutputNotSupported,
    DXGI_MSG_IDXGIOutput_GetDisplayModeList_RemoteOutputNotSupported,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_pDeviceHasMismatchedDXGIFactory,
    DXGI_MSG_IDXGISwapChain_Present_NonOptimalFSConfiguration,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_FlipSequentialNotSupportedOnD3D10,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_BufferCountOOBForFlipSequential,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidFormatForFlipSequential,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_MultiSamplingNotSupportedForFlipSequential,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_BufferCountOOBForFlipSequential,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidFormatForFlipSequential,
    DXGI_MSG_IDXGISwapChain_Present_PartialPresentationBeforeStandardPresentation,
    DXGI_MSG_IDXGISwapChain_Present_FullscreenPartialPresentIsInvalid,
    DXGI_MSG_IDXGISwapChain_Present_InvalidPresentTestOrDoNotSequenceFlag,
    DXGI_MSG_IDXGISwapChain_Present_ScrollInfoWithNoDirtyRectsSpecified,
    DXGI_MSG_IDXGISwapChain_Present_EmptyScrollRect,
    DXGI_MSG_IDXGISwapChain_Present_ScrollRectOutOfBackbufferBounds,
    DXGI_MSG_IDXGISwapChain_Present_ScrollRectOutOfBackbufferBoundsWithOffset,
    DXGI_MSG_IDXGISwapChain_Present_EmptyDirtyRect,
    DXGI_MSG_IDXGISwapChain_Present_DirtyRectOutOfBackbufferBounds,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_UnsupportedBufferUsageFlags,
    DXGI_MSG_IDXGISwapChain_Present_DoNotSequenceFlagSetButPreviousBufferIsUndefined,
    DXGI_MSG_IDXGISwapChain_Present_UnsupportedFlags,
    DXGI_MSG_IDXGISwapChain_Present_FlipModelChainMustResizeOrCreateOnFSTransition,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_pRestrictToOutputFromOtherIDXGIFactory,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_RestrictOutputNotSupportedOnAdapter,
    DXGI_MSG_IDXGISwapChain_Present_RestrictToOutputFlagSetButInvalidpRestrictToOutput,
    DXGI_MSG_IDXGISwapChain_Present_RestrictToOutputFlagdWithFullscreen,
    DXGI_MSG_IDXGISwapChain_Present_RestrictOutputFlagWithStaleSwapChain,
    DXGI_MSG_IDXGISwapChain_Present_OtherFlagsCausingInvalidPresentTestFlag,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_UnavailableInSession0,
    DXGI_MSG_IDXGIFactory_MakeWindowAssociation_UnavailableInSession0,
    DXGI_MSG_IDXGIFactory_GetWindowAssociation_UnavailableInSession0,
    DXGI_MSG_IDXGIAdapter_EnumOutputs_UnavailableInSession0,
    DXGI_MSG_IDXGISwapChain_CreationOrSetFullscreenState_StereoDisabled,
    DXGI_MSG_IDXGIFactory2_UnregisterStatus_CookieNotFound,
    DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithoutFSOrOverlay,
    DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithoutFlipSequential,
    DXGI_MSG_IDXGISwapChain_Present_ProtectedContentWithRDPDriver,
    DXGI_MSG_IDXGISwapChain_Present_ProtectedContentInWindowedModeWithDWMOffOrInvalidDisplayAffinity,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_WidthOrHeightIsZero,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_OnlyFlipSequentialSupported,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_UnsupportedOnAdapter,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_UnsupportedOnWindows7,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_FSTransitionWithCompositionSwapChain,
    DXGI_MSG_IDXGISwapChain_ResizeTarget_InvalidWithCompositionSwapChain,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_WidthOrHeightIsZero,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingNoneIsFlipModelOnly,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingUnrecognized,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyFullscreenUnsupported,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyUnsupported,
    DXGI_MSG_IDXGISwapChain_Present_RestartIsFullscreenOnly,
    DXGI_MSG_IDXGISwapChain_Present_ProtectedWindowlessPresentationRequiresDisplayOnly,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_DisplayOnlyUnsupported,
    DXGI_MSG_IDXGISwapChain1_SetBackgroundColor_OutOfRange,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyFullscreenUnsupported,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyUnsupported,
    DXGI_MSG_IDXGISwapchain_Present_ScrollUnsupported,
    DXGI_MSG_IDXGISwapChain1_SetRotation_UnsupportedOS,
    DXGI_MSG_IDXGISwapChain1_GetRotation_UnsupportedOS,
    DXGI_MSG_IDXGISwapchain_Present_FullscreenRotation,
    DXGI_MSG_IDXGISwapChain_Present_PartialPresentationWithMSAABuffers,
    DXGI_MSG_IDXGISwapChain1_SetRotation_FlipSequentialRequired,
    DXGI_MSG_IDXGISwapChain1_SetRotation_InvalidRotation,
    DXGI_MSG_IDXGISwapChain1_GetRotation_FlipSequentialRequired,
    DXGI_MSG_IDXGISwapChain_GetHwnd_WrongType,
    DXGI_MSG_IDXGISwapChain_GetCompositionSurface_WrongType,
    DXGI_MSG_IDXGISwapChain_GetCoreWindow_WrongType,
    DXGI_MSG_IDXGISwapChain_GetFullscreenDesc_NonHwnd,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_CoreWindow,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_UnsupportedOnWindows7,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_pWindowIsNULL,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_FSUnsupportedForModernApps,
    DXGI_MSG_IDXGIFactory_MakeWindowAssociation_ModernApp,
    DXGI_MSG_IDXGISwapChain_ResizeTarget_ModernApp,
    DXGI_MSG_IDXGISwapChain_ResizeTarget_pNewTargetParametersIsNULL,
    DXGI_MSG_IDXGIOutput_SetDisplaySurface_ModernApp,
    DXGI_MSG_IDXGIOutput_TakeOwnership_ModernApp,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_pWindowIsInvalid,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCompositionSurface_InvalidHandle,
    DXGI_MSG_IDXGISurface1_GetDC_ModernApp,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_ScalingNoneRequiresWindows8OrNewer,
    DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoAndPreferRight,
    DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoOrPreferRightWithDoNotSequence,
    DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoOrPreferRightWithoutStereo,
    DXGI_MSG_IDXGISwapChain_Present_TemporaryMonoUnsupported,
    DXGI_MSG_IDXGIOutput_GetDisplaySurfaceData_ArraySizeMismatch,
    DXGI_MSG_IDXGISwapChain_Present_PartialPresentationWithSwapEffectDiscard,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaUnrecognized,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaIsWindowlessOnly,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_AlphaIsFlipModelOnly,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_RestrictToOutputAdapterMismatch,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_DisplayOnlyOnLegacy,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_DisplayOnlyOnLegacy,
    DXGI_MSG_IDXGIResource1_CreateSubresourceSurface_InvalidIndex,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_InvalidScaling,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForCoreWindow_InvalidSwapEffect,
    DXGI_MSG_IDXGIResource1_CreateSharedHandle_UnsupportedOS,
    DXGI_MSG_IDXGIFactory2_RegisterOcclusionStatusWindow_UnsupportedOS,
    DXGI_MSG_IDXGIFactory2_RegisterOcclusionStatusEvent_UnsupportedOS,
    DXGI_MSG_IDXGIOutput1_DuplicateOutput_UnsupportedOS,
    DXGI_MSG_IDXGIDisplayControl_IsStereoEnabled_UnsupportedOS,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForComposition_InvalidAlphaMode,
    DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_InvalidResource,
    DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_InvalidLUID,
    DXGI_MSG_IDXGIFactory_GetSharedResourceAdapterLuid_UnsupportedOS,
    DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_2DOnly,
    DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_StagingOnly,
    DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_NeedCPUAccessWrite,
    DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_NoShared,
    DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_OnlyMipLevels1,
    DXGI_MSG_IDXGIOutput1_GetDisplaySurfaceData1_MappedOrOfferedResource,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_FSUnsupportedForModernApps,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_FailedToGoFSButNonPreRotated,
    DXGI_MSG_IDXGIFactory_CreateSwapChainOrRegisterOcclusionStatus_BlitModelUsedWhileRegisteredForOcclusionStatusEvents,
    DXGI_MSG_IDXGISwapChain_Present_BlitModelUsedWhileRegisteredForOcclusionStatusEvents,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_WaitableSwapChainsAreFlipModelOnly,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_WaitableSwapChainsAreNotFullscreen,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_Waitable,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveWaitableFlag,
    DXGI_MSG_IDXGISwapChain_GetFrameLatencyWaitableObject_OnlyWaitable,
    DXGI_MSG_IDXGISwapChain_GetMaximumFrameLatency_OnlyWaitable,
    DXGI_MSG_IDXGISwapChain_GetMaximumFrameLatency_pMaxLatencyIsNULL,
    DXGI_MSG_IDXGISwapChain_SetMaximumFrameLatency_OnlyWaitable,
    DXGI_MSG_IDXGISwapChain_SetMaximumFrameLatency_MaxLatencyIsOutOfBounds,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_ForegroundIsCoreWindowOnly,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_ForegroundUnsupportedOnAdapter,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_InvalidScaling,
    DXGI_MSG_IDXGIFactory2_CreateSwapChainForCoreWindow_InvalidAlphaMode,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveForegroundFlag,
    DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixPointerCannotBeNull,
    DXGI_MSG_IDXGISwapChain_SetMatrixTransform_RequiresCompositionSwapChain,
    DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixMustBeFinite,
    DXGI_MSG_IDXGISwapChain_SetMatrixTransform_MatrixMustBeTranslateAndOrScale,
    DXGI_MSG_IDXGISwapChain_GetMatrixTransform_MatrixPointerCannotBeNull,
    DXGI_MSG_IDXGISwapChain_GetMatrixTransform_RequiresCompositionSwapChain,
    DXGI_MSG_DXGIGetDebugInterface1_NULL_ppDebug,
    DXGI_MSG_DXGIGetDebugInterface1_InvalidFlags,
    DXGI_MSG_IDXGISwapChain_Present_Decode,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_Decode,
    DXGI_MSG_IDXGISwapChain_SetSourceSize_FlipModel,
    DXGI_MSG_IDXGISwapChain_SetSourceSize_Decode,
    DXGI_MSG_IDXGISwapChain_SetSourceSize_WidthHeight,
    DXGI_MSG_IDXGISwapChain_GetSourceSize_NullPointers,
    DXGI_MSG_IDXGISwapChain_GetSourceSize_Decode,
    DXGI_MSG_IDXGIDecodeSwapChain_SetColorSpace_InvalidFlags,
    DXGI_MSG_IDXGIDecodeSwapChain_SetSourceRect_InvalidRect,
    DXGI_MSG_IDXGIDecodeSwapChain_SetTargetRect_InvalidRect,
    DXGI_MSG_IDXGIDecodeSwapChain_SetDestSize_InvalidSize,
    DXGI_MSG_IDXGIDecodeSwapChain_GetSourceRect_InvalidPointer,
    DXGI_MSG_IDXGIDecodeSwapChain_GetTargetRect_InvalidPointer,
    DXGI_MSG_IDXGIDecodeSwapChain_GetDestSize_InvalidPointer,
    DXGI_MSG_IDXGISwapChain_PresentBuffer_YUV,
    DXGI_MSG_IDXGISwapChain_SetSourceSize_YUV,
    DXGI_MSG_IDXGISwapChain_GetSourceSize_YUV,
    DXGI_MSG_IDXGISwapChain_SetMatrixTransform_YUV,
    DXGI_MSG_IDXGISwapChain_GetMatrixTransform_YUV,
    DXGI_MSG_IDXGISwapChain_Present_PartialPresentation_YUV,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveFlag_YUV,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_Alignment_YUV,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_ShaderInputUnsupported_YUV,
    DXGI_MSG_IDXGIOutput3_CheckOverlaySupport_NullPointers,
    DXGI_MSG_IDXGIOutput3_CheckOverlaySupport_IDXGIDeviceNotSupportedBypConcernedDevice,
    DXGI_MSG_IDXGIAdapter_EnumOutputs2_InvalidEnumOutputs2Flag,
    DXGI_MSG_IDXGISwapChain_CreationOrSetFullscreenState_FSUnsupportedForFlipDiscard,
    DXGI_MSG_IDXGIOutput4_CheckOverlayColorSpaceSupport_NullPointers,
    DXGI_MSG_IDXGIOutput4_CheckOverlayColorSpaceSupport_IDXGIDeviceNotSupportedBypConcernedDevice,
    DXGI_MSG_IDXGISwapChain3_CheckColorSpaceSupport_NullPointers,
    DXGI_MSG_IDXGISwapChain3_SetColorSpace1_InvalidColorSpace,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidHwProtect,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_HwProtectUnsupported,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidHwProtect,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_HwProtectUnsupported,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers1_D3D12Only,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers1_FlipModel,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers1_NodeMaskAndQueueRequired,
    DXGI_MSG_IDXGISwapChain_CreateSwapChain_InvalidHwProtectGdiFlag,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_InvalidHwProtectGdiFlag,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_10BitFormatNotSupported,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_FlipSwapEffectRequired,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidDevice,
    DXGI_MSG_IDXGIOutput_TakeOwnership_Unsupported,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_InvalidQueue,
    DXGI_MSG_IDXGISwapChain3_ResizeBuffers1_InvalidQueue,
    DXGI_MSG_IDXGIFactory_CreateSwapChainForHwnd_InvalidScaling,
    DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidSize,
    DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidPointer,
    DXGI_MSG_IDXGISwapChain3_SetHDRMetaData_InvalidType,
    DXGI_MSG_IDXGISwapChain_Present_FullscreenAllowTearingIsInvalid,
    DXGI_MSG_IDXGISwapChain_Present_AllowTearingRequiresPresentIntervalZero,
    DXGI_MSG_IDXGISwapChain_Present_AllowTearingRequiresCreationFlag,
    DXGI_MSG_IDXGISwapChain_ResizeBuffers_CannotAddOrRemoveAllowTearingFlag,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_AllowTearingFlagIsFlipModelOnly,
    DXGI_MSG_IDXGIFactory_CheckFeatureSupport_InvalidFeature,
    DXGI_MSG_IDXGIFactory_CheckFeatureSupport_InvalidSize,
    DXGI_MSG_IDXGIOutput6_CheckHardwareCompositionSupport_NullPointer,
    DXGI_MSG_IDXGISwapChain_SetFullscreenState_PerMonitorDpiShimApplied,
    DXGI_MSG_IDXGIOutput_DuplicateOutput_PerMonitorDpiShimApplied,
    DXGI_MSG_IDXGIOutput_DuplicateOutput1_PerMonitorDpiRequired,
    DXGI_MSG_IDXGIFactory7_UnregisterAdaptersChangedEvent_CookieNotFound,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_LegacyBltModelSwapEffect,
    DXGI_MSG_IDXGISwapChain4_SetHDRMetaData_MetadataUnchanged,
    DXGI_MSG_IDXGISwapChain_Present_11On12_Released_Resource,
    DXGI_MSG_IDXGIFactory_CreateSwapChain_MultipleSwapchainRefToSurface_DeferredDtr,

    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_NotForegroundWindow     = 1000,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_DISCARD_BufferCount,
    DXGI_MSG_Phone_IDXGISwapChain_SetFullscreenState_NotAvailable,
    DXGI_MSG_Phone_IDXGISwapChain_ResizeBuffers_NotAvailable,
    DXGI_MSG_Phone_IDXGISwapChain_ResizeTarget_NotAvailable,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidLayerIndex,
    DXGI_MSG_Phone_IDXGISwapChain_Present_MultipleLayerIndex,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidLayerFlag,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidRotation,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidBlend,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidResource,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidMultiPlaneOverlayResource,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidIndexForPrimary,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidIndexForOverlay,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidSubResourceIndex,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidSourceRect,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidDestinationRect,
    DXGI_MSG_Phone_IDXGISwapChain_Present_MultipleResource,
    DXGI_MSG_Phone_IDXGISwapChain_Present_NotSharedResource,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidFlag,
    DXGI_MSG_Phone_IDXGISwapChain_Present_InvalidInterval,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_MSAA_NotSupported,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_ScalingAspectRatioStretch_Supported_ModernApp,
    DXGI_MSG_Phone_IDXGISwapChain_GetFrameStatistics_NotAvailable_ModernApp,
    DXGI_MSG_Phone_IDXGISwapChain_Present_ReplaceInterval0With1,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FailedRegisterWithCompositor,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_NotForegroundWindow_AtRendering,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FLIP_SEQUENTIAL_BufferCount,
    DXGI_MSG_Phone_IDXGIFactory_CreateSwapChain_FLIP_Modern_CoreWindow_Only,
    DXGI_MSG_Phone_IDXGISwapChain_Present1_RequiresOverlays,
    DXGI_MSG_Phone_IDXGISwapChain_SetBackgroundColor_FlipSequentialRequired,
    DXGI_MSG_Phone_IDXGISwapChain_GetBackgroundColor_FlipSequentialRequired);
  {$EXTERNALSYM DXGI_Message_Id}

// #endif NTDDI_VERSION >= NTDDI_WIN7

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

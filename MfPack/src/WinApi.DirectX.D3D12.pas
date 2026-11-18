// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D12.pas
// Kind: Pascal Unit
// Release date: 27-08-2022
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Media Foundation basic control-layer interfaces.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter Larson (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D12 is way outdated!
//          Search "Update May 2024", to find updates until May 2024.
//
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: d3d12.h
//
// Copyright (c) Microsoft Corporation. Licensed under the MIT license.
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
unit WinApi.DirectX.D3D12;

{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
{$DEFINE UNICODE}

interface


uses
  
  {WinApi}
  Windows,
  WinApi.WinApiTypes,
  {DirectX}
  Winapi.DirectX.D3DCommon,
  WinApi.DirectX.DXGICommon,
  Winapi.DirectX.DXGIType,
  Winapi.DirectX.DXGIFormat;
  //{$IF NOT DEFINED ( D3D12_IGNORE_SDK_LAYERS )}
  // Winapi.DirectX.D3D12SdkLayers,
  //{$ENDIF}
  {MediaFoundationApi}


  {$ALIGN ON}
  {$MINENUMSIZE 4}
  
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


const
  D3D12_16BIT_INDEX_STRIP_CUT_VALUE                                        = $FFFF;
  D3D12_32BIT_INDEX_STRIP_CUT_VALUE                                        = $FFFFFFFF;
  D3D12_8BIT_INDEX_STRIP_CUT_VALUE                                         = $FF;
  D3D12_APPEND_ALIGNED_ELEMENT                                             = $FFFFFFFF;
  D3D12_ARRAY_AXIS_ADDRESS_RANGE_BIT_COUNT                                 = 9;
  D3D12_CLIP_OR_CULL_DISTANCE_COUNT                                        = 8;
  D3D12_CLIP_OR_CULL_DISTANCE_ELEMENT_COUNT                                = 2;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT                        = 14;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_COMPONENTS                            = 4;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_COMPONENT_BIT_COUNT                   = 32;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_HW_SLOT_COUNT                         = 15;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_PARTIAL_UPDATE_EXTENTS_BYTE_ALIGNMENT = 16;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_REGISTER_COMPONENTS                   = 4;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_REGISTER_COUNT                        = 15;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_REGISTER_READS_PER_INST               = 1;
  D3D12_COMMONSHADER_CONSTANT_BUFFER_REGISTER_READ_PORTS                   = 1;
  D3D12_COMMONSHADER_FLOWCONTROL_NESTING_LIMIT                             = 64;
  D3D12_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_COMPONENTS         = 4;
  D3D12_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_COUNT              = 1;
  D3D12_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_READS_PER_INST     = 1;
  D3D12_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_READ_PORTS         = 1;
  D3D12_COMMONSHADER_IMMEDIATE_VALUE_COMPONENT_BIT_COUNT                   = 32;
  D3D12_COMMONSHADER_INPUT_RESOURCE_REGISTER_COMPONENTS                    = 1;
  D3D12_COMMONSHADER_INPUT_RESOURCE_REGISTER_COUNT                         = 128;
  D3D12_COMMONSHADER_INPUT_RESOURCE_REGISTER_READS_PER_INST                = 1;
  D3D12_COMMONSHADER_INPUT_RESOURCE_REGISTER_READ_PORTS                    = 1;
  D3D12_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT                             = 128;
  D3D12_COMMONSHADER_SAMPLER_REGISTER_COMPONENTS                           = 1;
  D3D12_COMMONSHADER_SAMPLER_REGISTER_COUNT                                = 16;
  D3D12_COMMONSHADER_SAMPLER_REGISTER_READS_PER_INST                       = 1;
  D3D12_COMMONSHADER_SAMPLER_REGISTER_READ_PORTS                           = 1;
  D3D12_COMMONSHADER_SAMPLER_SLOT_COUNT                                    = 16;
  D3D12_COMMONSHADER_SUBROUTINE_NESTING_LIMIT                              = 32;
  D3D12_COMMONSHADER_TEMP_REGISTER_COMPONENTS                              = 4;
  D3D12_COMMONSHADER_TEMP_REGISTER_COMPONENT_BIT_COUNT                     = 32;
  D3D12_COMMONSHADER_TEMP_REGISTER_COUNT                                   = 4096;
  D3D12_COMMONSHADER_TEMP_REGISTER_READS_PER_INST                          = 3;
  D3D12_COMMONSHADER_TEMP_REGISTER_READ_PORTS                              = 3;
  D3D12_COMMONSHADER_TEXCOORD_RANGE_REDUCTION_MAX                          = 10;
  D3D12_COMMONSHADER_TEXCOORD_RANGE_REDUCTION_MIN                          = -10;
  D3D12_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE                             = -8;
  D3D12_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE                             = 7;
  D3D12_CONSTANT_BUFFER_DATA_PLACEMENT_ALIGNMENT                           = 256;
  D3D12_CS_4_X_BUCKET00_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 256;
  D3D12_CS_4_X_BUCKET00_MAX_NUM_THREADS_PER_GROUP                          = 64;
  D3D12_CS_4_X_BUCKET01_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 240;
  D3D12_CS_4_X_BUCKET01_MAX_NUM_THREADS_PER_GROUP                          = 68;
  D3D12_CS_4_X_BUCKET02_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 224;
  D3D12_CS_4_X_BUCKET02_MAX_NUM_THREADS_PER_GROUP                          = 72;
  D3D12_CS_4_X_BUCKET03_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 208;
  D3D12_CS_4_X_BUCKET03_MAX_NUM_THREADS_PER_GROUP                          = 76;
  D3D12_CS_4_X_BUCKET04_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 192;
  D3D12_CS_4_X_BUCKET04_MAX_NUM_THREADS_PER_GROUP                          = 84;
  D3D12_CS_4_X_BUCKET05_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 176;
  D3D12_CS_4_X_BUCKET05_MAX_NUM_THREADS_PER_GROUP                          = 92;
  D3D12_CS_4_X_BUCKET06_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 160;
  D3D12_CS_4_X_BUCKET06_MAX_NUM_THREADS_PER_GROUP                          = 100;
  D3D12_CS_4_X_BUCKET07_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 144;
  D3D12_CS_4_X_BUCKET07_MAX_NUM_THREADS_PER_GROUP                          = 112;
  D3D12_CS_4_X_BUCKET08_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 128;
  D3D12_CS_4_X_BUCKET08_MAX_NUM_THREADS_PER_GROUP                          = 128;
  D3D12_CS_4_X_BUCKET09_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 112;
  D3D12_CS_4_X_BUCKET09_MAX_NUM_THREADS_PER_GROUP                          = 144;
  D3D12_CS_4_X_BUCKET10_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 96;
  D3D12_CS_4_X_BUCKET10_MAX_NUM_THREADS_PER_GROUP                          = 168;
  D3D12_CS_4_X_BUCKET11_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 80;
  D3D12_CS_4_X_BUCKET11_MAX_NUM_THREADS_PER_GROUP                          = 204;
  D3D12_CS_4_X_BUCKET12_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 64;
  D3D12_CS_4_X_BUCKET12_MAX_NUM_THREADS_PER_GROUP                          = 256;
  D3D12_CS_4_X_BUCKET13_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 48;
  D3D12_CS_4_X_BUCKET13_MAX_NUM_THREADS_PER_GROUP                          = 340;
  D3D12_CS_4_X_BUCKET14_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 32;
  D3D12_CS_4_X_BUCKET14_MAX_NUM_THREADS_PER_GROUP                          = 512;
  D3D12_CS_4_X_BUCKET15_MAX_BYTES_TGSM_WRITABLE_PER_THREAD                 = 16;
  D3D12_CS_4_X_BUCKET15_MAX_NUM_THREADS_PER_GROUP                          = 768;
  D3D12_CS_4_X_DISPATCH_MAX_THREAD_GROUPS_IN_Z_DIMENSION                   = 1;
  D3D12_CS_4_X_RAW_UAV_BYTE_ALIGNMENT                                      = 256;
  D3D12_CS_4_X_THREAD_GROUP_MAX_THREADS_PER_GROUP                          = 768;
  D3D12_CS_4_X_THREAD_GROUP_MAX_X                                          = 768;
  D3D12_CS_4_X_THREAD_GROUP_MAX_Y                                          = 768;
  D3D12_CS_4_X_UAV_REGISTER_COUNT                                          = 1;
  D3D12_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION                        = 65535;
  D3D12_CS_TGSM_REGISTER_COUNT                                             = 8192;
  D3D12_CS_TGSM_REGISTER_READS_PER_INST                                    = 1;
  D3D12_CS_TGSM_RESOURCE_REGISTER_COMPONENTS                               = 1;
  D3D12_CS_TGSM_RESOURCE_REGISTER_READ_PORTS                               = 1;
  D3D12_CS_THREADGROUPID_REGISTER_COMPONENTS                               = 3;
  D3D12_CS_THREADGROUPID_REGISTER_COUNT                                    = 1;
  D3D12_CS_THREADIDINGROUPFLATTENED_REGISTER_COMPONENTS                    = 1;
  D3D12_CS_THREADIDINGROUPFLATTENED_REGISTER_COUNT                         = 1;
  D3D12_CS_THREADIDINGROUP_REGISTER_COMPONENTS                             = 3;
  D3D12_CS_THREADIDINGROUP_REGISTER_COUNT                                  = 1;
  D3D12_CS_THREADID_REGISTER_COMPONENTS                                    = 3;
  D3D12_CS_THREADID_REGISTER_COUNT                                         = 1;
  D3D12_CS_THREAD_GROUP_MAX_THREADS_PER_GROUP                              = 1024;
  D3D12_CS_THREAD_GROUP_MAX_X                                              = 1024;
  D3D12_CS_THREAD_GROUP_MAX_Y                                              = 1024;
  D3D12_CS_THREAD_GROUP_MAX_Z                                              = 64;
  D3D12_CS_THREAD_GROUP_MIN_X                                              = 1;
  D3D12_CS_THREAD_GROUP_MIN_Y                                              = 1;
  D3D12_CS_THREAD_GROUP_MIN_Z                                              = 1;
  D3D12_CS_THREAD_LOCAL_TEMP_REGISTER_POOL                                 = 16384;
  D3D12_DEFAULT_BLEND_FACTOR_ALPHA                                         = 1.0;
  D3D12_DEFAULT_BLEND_FACTOR_BLUE                                          = 1.0;
  D3D12_DEFAULT_BLEND_FACTOR_GREEN                                         = 1.0;
  D3D12_DEFAULT_BLEND_FACTOR_RED                                           = 1.0;
  D3D12_DEFAULT_BORDER_COLOR_COMPONENT                                     = 0.0;
  D3D12_DEFAULT_DEPTH_BIAS                                                 = 0;
  D3D12_DEFAULT_DEPTH_BIAS_CLAMP                                           = 0.0;
  D3D12_DEFAULT_MAX_ANISOTROPY                                             = 16;
  D3D12_DEFAULT_MIP_LOD_BIAS                                               = 0.0;
  D3D12_DEFAULT_MSAA_RESOURCE_PLACEMENT_ALIGNMENT                          = 4194304;
  D3D12_DEFAULT_RENDER_TARGET_ARRAY_INDEX                                  = 0;
  D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT                               = 65536;
  D3D12_DEFAULT_SAMPLE_MASK                                                = $FFFFFFFF;
  D3D12_DEFAULT_SCISSOR_ENDX                                               = 0;
  D3D12_DEFAULT_SCISSOR_ENDY                                               = 0;
  D3D12_DEFAULT_SCISSOR_STARTX                                             = 0;
  D3D12_DEFAULT_SCISSOR_STARTY                                             = 0;
  D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS                                    = 0.0;
  D3D12_DEFAULT_STENCIL_READ_MASK                                          = $FF;
  D3D12_DEFAULT_STENCIL_REFERENCE                                          = 0;
  D3D12_DEFAULT_STENCIL_WRITE_MASK                                         = $FF;
  D3D12_DEFAULT_VIEWPORT_AND_SCISSORRECT_INDEX                             = 0;
  D3D12_DEFAULT_VIEWPORT_HEIGHT                                            = 0;
  D3D12_DEFAULT_VIEWPORT_MAX_DEPTH                                         = 0.0;
  D3D12_DEFAULT_VIEWPORT_MIN_DEPTH                                         = 0.0;
  D3D12_DEFAULT_VIEWPORT_TOPLEFTX                                          = 0;
  D3D12_DEFAULT_VIEWPORT_TOPLEFTY                                          = 0;
  D3D12_DEFAULT_VIEWPORT_WIDTH                                             = 0;
  D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND                                     = $FFFFFFFF;
  D3D12_DRIVER_RESERVED_REGISTER_SPACE_VALUES_END                          = $FFFFFFF7;
  D3D12_DRIVER_RESERVED_REGISTER_SPACE_VALUES_START                        = $FFFFFFF0;
  D3D12_DS_INPUT_CONTROL_POINTS_MAX_TOTAL_SCALARS                          = 3968;
  D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COMPONENTS                         = 4;
  D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COMPONENT_BIT_COUNT                = 32;
  D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COUNT                              = 32;
  D3D12_DS_INPUT_CONTROL_POINT_REGISTER_READS_PER_INST                     = 2;
  D3D12_DS_INPUT_CONTROL_POINT_REGISTER_READ_PORTS                         = 1;
  D3D12_DS_INPUT_DOMAIN_POINT_REGISTER_COMPONENTS                          = 3;
  D3D12_DS_INPUT_DOMAIN_POINT_REGISTER_COMPONENT_BIT_COUNT                 = 32;
  D3D12_DS_INPUT_DOMAIN_POINT_REGISTER_COUNT                               = 1;
  D3D12_DS_INPUT_DOMAIN_POINT_REGISTER_READS_PER_INST                      = 2;
  D3D12_DS_INPUT_DOMAIN_POINT_REGISTER_READ_PORTS                          = 1;
  D3D12_DS_INPUT_PATCH_CONSTANT_REGISTER_COMPONENTS                        = 4;
  D3D12_DS_INPUT_PATCH_CONSTANT_REGISTER_COMPONENT_BIT_COUNT               = 32;
  D3D12_DS_INPUT_PATCH_CONSTANT_REGISTER_COUNT                             = 32;
  D3D12_DS_INPUT_PATCH_CONSTANT_REGISTER_READS_PER_INST                    = 2;
  D3D12_DS_INPUT_PATCH_CONSTANT_REGISTER_READ_PORTS                        = 1;
  D3D12_DS_INPUT_PRIMITIVE_ID_REGISTER_COMPONENTS                          = 1;
  D3D12_DS_INPUT_PRIMITIVE_ID_REGISTER_COMPONENT_BIT_COUNT                 = 32;
  D3D12_DS_INPUT_PRIMITIVE_ID_REGISTER_COUNT                               = 1;
  D3D12_DS_INPUT_PRIMITIVE_ID_REGISTER_READS_PER_INST                      = 2;
  D3D12_DS_INPUT_PRIMITIVE_ID_REGISTER_READ_PORTS                          = 1;
  D3D12_DS_OUTPUT_REGISTER_COMPONENTS                                      = 4;
  D3D12_DS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT                             = 32;
  D3D12_DS_OUTPUT_REGISTER_COUNT                                           = 32;
  D3D12_FLOAT16_FUSED_TOLERANCE_IN_ULP                                     = 0.6;
  D3D12_FLOAT32_MAX                                                        = 3.402823466e+38;
  D3D12_FLOAT32_TO_INTEGER_TOLERANCE_IN_ULP                                = 0.6;
  D3D12_FLOAT_TO_SRGB_EXPONENT_DENOMINATOR                                 = 2.4;
  D3D12_FLOAT_TO_SRGB_EXPONENT_NUMERATOR                                   = 1.0;
  D3D12_FLOAT_TO_SRGB_OFFSET                                               = 0.055;
  D3D12_FLOAT_TO_SRGB_SCALE_1                                              = 12.92;
  D3D12_FLOAT_TO_SRGB_SCALE_2                                              = 1.055;
  D3D12_FLOAT_TO_SRGB_THRESHOLD                                            = 0.0031308;
  D3D12_FTOI_INSTRUCTION_MAX_INPUT                                         = 2147483647.999;
  D3D12_FTOI_INSTRUCTION_MIN_INPUT                                         = -2147483648.999;
  D3D12_FTOU_INSTRUCTION_MAX_INPUT                                         = 4294967295.999;
  D3D12_FTOU_INSTRUCTION_MIN_INPUT                                         = 0.0;
  D3D12_GS_INPUT_INSTANCE_ID_READS_PER_INST                                = 2;
  D3D12_GS_INPUT_INSTANCE_ID_READ_PORTS                                    = 1;
  D3D12_GS_INPUT_INSTANCE_ID_REGISTER_COMPONENTS                           = 1;
  D3D12_GS_INPUT_INSTANCE_ID_REGISTER_COMPONENT_BIT_COUNT                  = 32;
  D3D12_GS_INPUT_INSTANCE_ID_REGISTER_COUNT                                = 1;
  D3D12_GS_INPUT_PRIM_CONST_REGISTER_COMPONENTS                            = 1;
  D3D12_GS_INPUT_PRIM_CONST_REGISTER_COMPONENT_BIT_COUNT                   = 32;
  D3D12_GS_INPUT_PRIM_CONST_REGISTER_COUNT                                 = 1;
  D3D12_GS_INPUT_PRIM_CONST_REGISTER_READS_PER_INST                        = 2;
  D3D12_GS_INPUT_PRIM_CONST_REGISTER_READ_PORTS                            = 1;
  D3D12_GS_INPUT_REGISTER_COMPONENTS                                       = 4;
  D3D12_GS_INPUT_REGISTER_COMPONENT_BIT_COUNT                              = 32;
  D3D12_GS_INPUT_REGISTER_COUNT                                            = 32;
  D3D12_GS_INPUT_REGISTER_READS_PER_INST                                   = 2;
  D3D12_GS_INPUT_REGISTER_READ_PORTS                                       = 1;
  D3D12_GS_INPUT_REGISTER_VERTICES                                         = 32;
  D3D12_GS_MAX_INSTANCE_COUNT                                              = 32;
  D3D12_GS_MAX_OUTPUT_VERTEX_COUNT_ACROSS_INSTANCES                        = 1024;
  D3D12_GS_OUTPUT_ELEMENTS                                                 = 32;
  D3D12_GS_OUTPUT_REGISTER_COMPONENTS                                      = 4;
  D3D12_GS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT                             = 32;
  D3D12_GS_OUTPUT_REGISTER_COUNT                                           = 32;
  D3D12_HS_CONTROL_POINT_PHASE_INPUT_REGISTER_COUNT                        = 32;
  D3D12_HS_CONTROL_POINT_PHASE_OUTPUT_REGISTER_COUNT                       = 32;
  D3D12_HS_CONTROL_POINT_REGISTER_COMPONENTS                               = 4;
  D3D12_HS_CONTROL_POINT_REGISTER_COMPONENT_BIT_COUNT                      = 32;
  D3D12_HS_CONTROL_POINT_REGISTER_READS_PER_INST                           = 2;
  D3D12_HS_CONTROL_POINT_REGISTER_READ_PORTS                               = 1;
  D3D12_HS_FORK_PHASE_INSTANCE_COUNT_UPPER_BOUND                           = $FFFFFFFF;
  D3D12_HS_INPUT_FORK_INSTANCE_ID_REGISTER_COMPONENTS                      = 1;
  D3D12_HS_INPUT_FORK_INSTANCE_ID_REGISTER_COMPONENT_BIT_COUNT             = 32;
  D3D12_HS_INPUT_FORK_INSTANCE_ID_REGISTER_COUNT                           = 1;
  D3D12_HS_INPUT_FORK_INSTANCE_ID_REGISTER_READS_PER_INST                  = 2;
  D3D12_HS_INPUT_FORK_INSTANCE_ID_REGISTER_READ_PORTS                      = 1;
  D3D12_HS_INPUT_JOIN_INSTANCE_ID_REGISTER_COMPONENTS                      = 1;
  D3D12_HS_INPUT_JOIN_INSTANCE_ID_REGISTER_COMPONENT_BIT_COUNT             = 32;
  D3D12_HS_INPUT_JOIN_INSTANCE_ID_REGISTER_COUNT                           = 1;
  D3D12_HS_INPUT_JOIN_INSTANCE_ID_REGISTER_READS_PER_INST                  = 2;
  D3D12_HS_INPUT_JOIN_INSTANCE_ID_REGISTER_READ_PORTS                      = 1;
  D3D12_HS_INPUT_PRIMITIVE_ID_REGISTER_COMPONENTS                          = 1;
  D3D12_HS_INPUT_PRIMITIVE_ID_REGISTER_COMPONENT_BIT_COUNT                 = 32;
  D3D12_HS_INPUT_PRIMITIVE_ID_REGISTER_COUNT                               = 1;
  D3D12_HS_INPUT_PRIMITIVE_ID_REGISTER_READS_PER_INST                      = 2;
  D3D12_HS_INPUT_PRIMITIVE_ID_REGISTER_READ_PORTS                          = 1;
  D3D12_HS_JOIN_PHASE_INSTANCE_COUNT_UPPER_BOUND                           = $FFFFFFFF;
  D3D12_HS_MAXTESSFACTOR_LOWER_BOUND                                       = 1.0;
  D3D12_HS_MAXTESSFACTOR_UPPER_BOUND                                       = 64.0;
  D3D12_HS_OUTPUT_CONTROL_POINTS_MAX_TOTAL_SCALARS                         = 3968;
  D3D12_HS_OUTPUT_CONTROL_POINT_ID_REGISTER_COMPONENTS                     = 1;
  D3D12_HS_OUTPUT_CONTROL_POINT_ID_REGISTER_COMPONENT_BIT_COUNT            = 32;
  D3D12_HS_OUTPUT_CONTROL_POINT_ID_REGISTER_COUNT                          = 1;
  D3D12_HS_OUTPUT_CONTROL_POINT_ID_REGISTER_READS_PER_INST                 = 2;
  D3D12_HS_OUTPUT_CONTROL_POINT_ID_REGISTER_READ_PORTS                     = 1;
  D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_COMPONENTS                       = 4;
  D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_COMPONENT_BIT_COUNT              = 32;
  D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_COUNT                            = 32;
  D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_READS_PER_INST                   = 2;
  D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_READ_PORTS                       = 1;
  D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_SCALAR_COMPONENTS                = 128;
  D3D12_IA_DEFAULT_INDEX_BUFFER_OFFSET_IN_BYTES                            = 0;
  D3D12_IA_DEFAULT_PRIMITIVE_TOPOLOGY                                      = 0;
  D3D12_IA_DEFAULT_VERTEX_BUFFER_OFFSET_IN_BYTES                           = 0;
  D3D12_IA_INDEX_INPUT_RESOURCE_SLOT_COUNT                                 = 1;
  D3D12_IA_INSTANCE_ID_BIT_COUNT                                           = 32;
  D3D12_IA_INTEGER_ARITHMETIC_BIT_COUNT                                    = 32;
  D3D12_IA_PATCH_MAX_CONTROL_POINT_COUNT                                   = 32;
  D3D12_IA_PRIMITIVE_ID_BIT_COUNT                                          = 32;
  D3D12_IA_VERTEX_ID_BIT_COUNT                                             = 32;
  D3D12_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT                                = 32;
  D3D12_IA_VERTEX_INPUT_STRUCTURE_ELEMENTS_COMPONENTS                      = 128;
  D3D12_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT                            = 32;
  D3D12_INTEGER_DIVIDE_BY_ZERO_QUOTIENT                                    = $FFFFFFFF;
  D3D12_INTEGER_DIVIDE_BY_ZERO_REMAINDER                                   = $FFFFFFFF;
  D3D12_KEEP_RENDER_TARGETS_AND_DEPTH_STENCIL                              = $FFFFFFFF;
  D3D12_KEEP_UNORDERED_ACCESS_VIEWS                                        = $FFFFFFFF;
  D3D12_LINEAR_GAMMA                                                       = 1.0;
  D3D12_MAJOR_VERSION                                                      = 12;
  D3D12_MAX_BORDER_COLOR_COMPONENT                                         = 1.0;
  D3D12_MAX_DEPTH                                                          = 1.0;
  D3D12_MAX_LIVE_STATIC_SAMPLERS                                           = 2032;
  D3D12_MAX_MAXANISOTROPY                                                  = 16;
  D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT                                       = 32;
  D3D12_MAX_POSITION_VALUE                                                 = 3.402823466e+34;
  D3D12_MAX_ROOT_COST                                                      = 64;
  D3D12_MAX_SHADER_VISIBLE_DESCRIPTOR_HEAP_SIZE_TIER_1                     = 1000000;
  D3D12_MAX_SHADER_VISIBLE_DESCRIPTOR_HEAP_SIZE_TIER_2                     = 1000000;
  D3D12_MAX_SHADER_VISIBLE_SAMPLER_HEAP_SIZE                               = 2048;
  D3D12_MAX_TEXTURE_DIMENSION_2_TO_EXP                                     = 17;
  D3D12_MAX_VIEW_INSTANCE_COUNT                                            = 4;
  D3D12_MINOR_VERSION                                                      = 0;
  D3D12_MIN_BORDER_COLOR_COMPONENT                                         = 0.0;
  D3D12_MIN_DEPTH                                                          = 0.0;
  D3D12_MIN_MAXANISOTROPY                                                  = 0;
  D3D12_MIP_LOD_BIAS_MAX                                                   = 15.99;
  D3D12_MIP_LOD_BIAS_MIN                                                   = -16.0;
  D3D12_MIP_LOD_FRACTIONAL_BIT_COUNT                                       = 8;
  D3D12_MIP_LOD_RANGE_BIT_COUNT                                            = 8;
  D3D12_MULTISAMPLE_ANTIALIAS_LINE_WIDTH                                   = 1.4;
  D3D12_NONSAMPLE_FETCH_OUT_OF_RANGE_ACCESS_RESULT                         = 0;
  D3D12_OS_RESERVED_REGISTER_SPACE_VALUES_END                              = $FFFFFFFF;
  D3D12_OS_RESERVED_REGISTER_SPACE_VALUES_START                            = $FFFFFFF8;
  D3D12_PACKED_TILE                                                        = $FFFFFFFF;
  D3D12_PIXEL_ADDRESS_RANGE_BIT_COUNT                                      = 15;
  { 10.0.28000.0  }
  D3D12_PREVIEW_SDK_VERSION                                                = 718;
  D3D12_PRE_SCISSOR_PIXEL_ADDRESS_RANGE_BIT_COUNT                          = 16;
  D3D12_PS_CS_UAV_REGISTER_COMPONENTS                                      = 1;
  D3D12_PS_CS_UAV_REGISTER_COUNT                                           = 8;
  D3D12_PS_CS_UAV_REGISTER_READS_PER_INST                                  = 1;
  D3D12_PS_CS_UAV_REGISTER_READ_PORTS                                      = 1;
  D3D12_PS_FRONTFACING_DEFAULT_VALUE                                       = $FFFFFFFF;
  D3D12_PS_FRONTFACING_FALSE_VALUE                                         = 0;
  D3D12_PS_FRONTFACING_TRUE_VALUE                                          = $FFFFFFFF;
  D3D12_PS_INPUT_REGISTER_COMPONENTS                                       = 4;
  D3D12_PS_INPUT_REGISTER_COMPONENT_BIT_COUNT                              = 32;
  D3D12_PS_INPUT_REGISTER_COUNT                                            = 32;
  D3D12_PS_INPUT_REGISTER_READS_PER_INST                                   = 2;
  D3D12_PS_INPUT_REGISTER_READ_PORTS                                       = 1;
  D3D12_PS_LEGACY_PIXEL_CENTER_FRACTIONAL_COMPONENT                        = 0.0;
  D3D12_PS_OUTPUT_DEPTH_REGISTER_COMPONENTS                                = 1;
  D3D12_PS_OUTPUT_DEPTH_REGISTER_COMPONENT_BIT_COUNT                       = 32;
  D3D12_PS_OUTPUT_DEPTH_REGISTER_COUNT                                     = 1;
  D3D12_PS_OUTPUT_MASK_REGISTER_COMPONENTS                                 = 1;
  D3D12_PS_OUTPUT_MASK_REGISTER_COMPONENT_BIT_COUNT                        = 32;
  D3D12_PS_OUTPUT_MASK_REGISTER_COUNT                                      = 1;
  D3D12_PS_OUTPUT_REGISTER_COMPONENTS                                      = 4;
  D3D12_PS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT                             = 32;
  D3D12_PS_OUTPUT_REGISTER_COUNT                                           = 8;
  D3D12_PS_PIXEL_CENTER_FRACTIONAL_COMPONENT                               = 0.5;
  D3D12_RAW_UAV_SRV_BYTE_ALIGNMENT                                         = 16;
  D3D12_RAYTRACING_AABB_BYTE_ALIGNMENT                                     = 8;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BYTE_ALIGNMENT                   = 256;
  { 10.0.28000.0  }
  D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_BYTE_ALIGNMENT                   = 128;
  D3D12_RAYTRACING_OPACITY_MICROMAP_OC1_MAX_SUBDIVISION_LEVEL              = 12;
  D3D12_RAYTRACING_INSTANCE_DESCS_BYTE_ALIGNMENT                           = 16;
  D3D12_RAYTRACING_MAX_ATTRIBUTE_SIZE_IN_BYTES                             = 32;
  D3D12_RAYTRACING_MAX_DECLARABLE_TRACE_RECURSION_DEPTH                    = 31;
  D3D12_RAYTRACING_MAX_GEOMETRIES_PER_BOTTOM_LEVEL_ACCELERATION_STRUCTURE  = 16777216;
  D3D12_RAYTRACING_MAX_INSTANCES_PER_TOP_LEVEL_ACCELERATION_STRUCTURE      = 16777216;
  D3D12_RAYTRACING_MAX_PRIMITIVES_PER_BOTTOM_LEVEL_ACCELERATION_STRUCTURE  = 536870912;
  D3D12_RAYTRACING_MAX_RAY_GENERATION_SHADER_THREADS                       = 1073741824;
  D3D12_RAYTRACING_MAX_SHADER_RECORD_STRIDE                                = 4096;
  D3D12_RAYTRACING_SHADER_RECORD_BYTE_ALIGNMENT                            = 32;
  D3D12_RAYTRACING_SHADER_TABLE_BYTE_ALIGNMENT                             = 64;
  D3D12_RAYTRACING_TRANSFORM3X4_BYTE_ALIGNMENT                             = 16;
  D3D12_REQ_BLEND_OBJECT_COUNT_PER_DEVICE                                  = 4096;
  D3D12_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP                           = 27;
  D3D12_REQ_CONSTANT_BUFFER_ELEMENT_COUNT                                  = 4096;
  D3D12_REQ_DEPTH_STENCIL_OBJECT_COUNT_PER_DEVICE                          = 4096;
  D3D12_REQ_DRAWINDEXED_INDEX_COUNT_2_TO_EXP                               = 32;
  D3D12_REQ_DRAW_VERTEX_COUNT_2_TO_EXP                                     = 32;
  D3D12_REQ_FILTERING_HW_ADDRESSABLE_RESOURCE_DIMENSION                    = 16384;
  D3D12_REQ_GS_INVOCATION_32BIT_OUTPUT_COMPONENT_LIMIT                     = 1024;
  D3D12_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT                        = 4096;
  D3D12_REQ_MAXANISOTROPY                                                  = 16;
  D3D12_REQ_MIP_LEVELS                                                     = 15;
  D3D12_REQ_MULTI_ELEMENT_STRUCTURE_SIZE_IN_BYTES                          = 2048;
  D3D12_REQ_RASTERIZER_OBJECT_COUNT_PER_DEVICE                             = 4096;
  D3D12_REQ_RENDER_TO_BUFFER_WINDOW_WIDTH                                  = 16384;
  D3D12_REQ_RESOURCE_SIZE_IN_MEGABYTES_EXPRESSION_A_TERM                   = 128;
  D3D12_REQ_RESOURCE_SIZE_IN_MEGABYTES_EXPRESSION_B_TERM                   = 0.25;
  D3D12_REQ_RESOURCE_SIZE_IN_MEGABYTES_EXPRESSION_C_TERM                   = 2048;
  D3D12_REQ_RESOURCE_VIEW_COUNT_PER_DEVICE_2_TO_EXP                        = 20;
  D3D12_REQ_SAMPLER_OBJECT_COUNT_PER_DEVICE                                = 4096;
  D3D12_REQ_SUBRESOURCES                                                   = 30720;
  D3D12_REQ_TEXTURE1D_ARRAY_AXIS_DIMENSION                                 = 2048;
  D3D12_REQ_TEXTURE1D_U_DIMENSION                                          = 16384;
  D3D12_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION                                 = 2048;
  D3D12_REQ_TEXTURE2D_U_OR_V_DIMENSION                                     = 16384;
  D3D12_REQ_TEXTURE3D_U_V_OR_W_DIMENSION                                   = 2048;
  D3D12_REQ_TEXTURECUBE_DIMENSION                                          = 16384;
  D3D12_RESINFO_INSTRUCTION_MISSING_COMPONENT_RETVAL                       = 0;
  D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES                                  = $FFFFFFFF;
  D3D12_RS_SET_SHADING_RATE_COMBINER_COUNT                                 = 2;
  { 10.0.28000.0  }
  D3D12_SDK_VERSION                                                        = 617;
  D3D12_SHADER_IDENTIFIER_SIZE_IN_BYTES                                    = 32;
  D3D12_SHADER_MAJOR_VERSION                                               = 5;
  D3D12_SHADER_MAX_INSTANCES                                               = 65535;
  D3D12_SHADER_MAX_INTERFACES                                              = 253;
  D3D12_SHADER_MAX_INTERFACE_CALL_SITES                                    = 4096;
  D3D12_SHADER_MAX_TYPES                                                   = 65535;
  D3D12_SHADER_MINOR_VERSION                                               = 1;
  D3D12_SHIFT_INSTRUCTION_PAD_VALUE                                        = 0;
  D3D12_SHIFT_INSTRUCTION_SHIFT_VALUE_BIT_COUNT                            = 5;
  D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT                                   = 8;
  D3D12_SMALL_MSAA_RESOURCE_PLACEMENT_ALIGNMENT                            = 65536;
  D3D12_SMALL_RESOURCE_PLACEMENT_ALIGNMENT                                 = 4096;
  { 10.0.28000.0  }
  D3D12_TIGHT_ALIGNMENT_MIN_COMMITTED_RESOURCE_ALIGNMENT                   = 4096;
  D3D12_TIGHT_ALIGNMENT_MIN_PLACED_RESOURCE_ALIGNMENT                      = 8;
  D3D12_SO_BUFFER_MAX_STRIDE_IN_BYTES                                      = 2048;
  D3D12_SO_BUFFER_MAX_WRITE_WINDOW_IN_BYTES                                = 512;
  D3D12_SO_BUFFER_SLOT_COUNT                                               = 4;
  D3D12_SO_DDI_REGISTER_INDEX_DENOTING_GAP                                 = $FFFFFFFF;
  D3D12_SO_NO_RASTERIZED_STREAM                                            = $FFFFFFFF;
  D3D12_SO_OUTPUT_COMPONENT_COUNT                                          = 128;
  D3D12_SO_STREAM_COUNT                                                    = 4;
  { 10.0.28000.0  }
  D3D12_WORK_GRAPHS_BACKING_MEMORY_ALIGNMENT_IN_BYTES                      = 8;
  D3D12_SPEC_DATE_DAY                                                      = 14;
  D3D12_SPEC_DATE_MONTH                                                    = 11;
  D3D12_SPEC_DATE_YEAR                                                     = 2014;
  D3D12_SPEC_VERSION                                                       = 1.16;
  D3D12_SRGB_GAMMA                                                         = 2.2;
  D3D12_SRGB_TO_FLOAT_DENOMINATOR_1                                        = 12.92;
  D3D12_SRGB_TO_FLOAT_DENOMINATOR_2                                        = 1.055;
  D3D12_SRGB_TO_FLOAT_EXPONENT                                             = 2.4;
  D3D12_SRGB_TO_FLOAT_OFFSET                                               = 0.055;
  D3D12_SRGB_TO_FLOAT_THRESHOLD                                            = 0.04045;
  D3D12_SRGB_TO_FLOAT_TOLERANCE_IN_ULP                                     = 0.5;
  D3D12_STANDARD_COMPONENT_BIT_COUNT                                       = 32;
  D3D12_STANDARD_COMPONENT_BIT_COUNT_DOUBLED                               = 64;
  D3D12_STANDARD_MAXIMUM_ELEMENT_ALIGNMENT_BYTE_MULTIPLE                   = 4;
  D3D12_STANDARD_PIXEL_COMPONENT_COUNT                                     = 128;
  D3D12_STANDARD_PIXEL_ELEMENT_COUNT                                       = 32;
  D3D12_STANDARD_VECTOR_SIZE                                               = 4;
  D3D12_STANDARD_VERTEX_ELEMENT_COUNT                                      = 32;
  D3D12_STANDARD_VERTEX_TOTAL_COMPONENT_COUNT                              = 64;
  D3D12_SUBPIXEL_FRACTIONAL_BIT_COUNT                                      = 8;
  D3D12_SUBTEXEL_FRACTIONAL_BIT_COUNT                                      = 8;
  D3D12_SYSTEM_RESERVED_REGISTER_SPACE_VALUES_END                          = $FFFFFFFF;
  D3D12_SYSTEM_RESERVED_REGISTER_SPACE_VALUES_START                        = $FFFFFFF0;
  D3D12_TESSELLATOR_MAX_EVEN_TESSELLATION_FACTOR                           = 64;
  D3D12_TESSELLATOR_MAX_ISOLINE_DENSITY_TESSELLATION_FACTOR                = 64;
  D3D12_TESSELLATOR_MAX_ODD_TESSELLATION_FACTOR                            = 63;
  D3D12_TESSELLATOR_MAX_TESSELLATION_FACTOR                                = 64;
  D3D12_TESSELLATOR_MIN_EVEN_TESSELLATION_FACTOR                           = 2;
  D3D12_TESSELLATOR_MIN_ISOLINE_DENSITY_TESSELLATION_FACTOR                = 1;
  D3D12_TESSELLATOR_MIN_ODD_TESSELLATION_FACTOR                            = 1;
  D3D12_TEXEL_ADDRESS_RANGE_BIT_COUNT                                      = 16;
  D3D12_TEXTURE_DATA_PITCH_ALIGNMENT                                       = 256;
  D3D12_TEXTURE_DATA_PLACEMENT_ALIGNMENT                                   = 512;
  D3D12_TILED_RESOURCE_TILE_SIZE_IN_BYTES                                  = 65536;
  D3D12_TRACKED_WORKLOAD_MAX_INSTANCES                                     = 32;
  D3D12_UAV_COUNTER_PLACEMENT_ALIGNMENT                                    = 4096;
  D3D12_UAV_SLOT_COUNT                                                     = 64;
  D3D12_UNBOUND_MEMORY_ACCESS_RESULT                                       = 0;
  D3D12_VIDEO_DECODE_MAX_ARGUMENTS                                         = 10;
  D3D12_VIDEO_DECODE_MAX_HISTOGRAM_COMPONENTS                              = 4;
  D3D12_VIDEO_DECODE_MIN_BITSTREAM_OFFSET_ALIGNMENT                        = 256;
  D3D12_VIDEO_DECODE_MIN_HISTOGRAM_OFFSET_ALIGNMENT                        = 256;
  D3D12_VIDEO_DECODE_STATUS_MACROBLOCKS_AFFECTED_UNKNOWN                   = $FFFFFFFF;

  // Update May 2024
  D3D12_VIDEO_ENCODER_AV1_INVALID_DPB_RESOURCE_INDEX                       = $FF;
  D3D12_VIDEO_ENCODER_AV1_MAX_TILE_COLS                                    = 64;
  D3D12_VIDEO_ENCODER_AV1_MAX_TILE_ROWS                                    = 64;
  D3D12_VIDEO_ENCODER_AV1_SUPERRES_DENOM_MIN                               = 9;
  D3D12_VIDEO_ENCODER_AV1_SUPERRES_NUM                                     = 8;
  // ==

  D3D12_VIDEO_PROCESS_MAX_FILTERS                                          = 32;
  D3D12_VIDEO_PROCESS_STEREO_VIEWS                                         = 2;
  D3D12_VIEWPORT_AND_SCISSORRECT_MAX_INDEX                                 = 15;
  D3D12_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE                 = 16;
  D3D12_VIEWPORT_BOUNDS_MAX                                                = 32767;
  D3D12_VIEWPORT_BOUNDS_MIN                                                = -32768;
  D3D12_VS_INPUT_REGISTER_COMPONENTS                                       = 4;
  D3D12_VS_INPUT_REGISTER_COMPONENT_BIT_COUNT                              = 32;
  D3D12_VS_INPUT_REGISTER_COUNT                                            = 32;
  D3D12_VS_INPUT_REGISTER_READS_PER_INST                                   = 2;
  D3D12_VS_INPUT_REGISTER_READ_PORTS                                       = 1;
  D3D12_VS_OUTPUT_REGISTER_COMPONENTS                                      = 4;
  D3D12_VS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT                             = 32;
  D3D12_VS_OUTPUT_REGISTER_COUNT                                           = 32;
  D3D12_WHQL_CONTEXT_COUNT_FOR_RESOURCE_LIMIT                              = 10;
  D3D12_WHQL_DRAWINDEXED_INDEX_COUNT_2_TO_EXP                              = 25;
  D3D12_WHQL_DRAW_VERTEX_COUNT_2_TO_EXP                                    = 25;
  D3D12_WORK_GRAPHS_MAX_NODE_DEPTH                                         = 32; // Update May 2024

  // Moved to here.
  D3D12_SHADER_COMPONENT_MAPPING_MASK                                      = $7;
  D3D12_SHADER_COMPONENT_MAPPING_SHIFT                                     = 3;
  D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES  = (1 shl (D3D12_SHADER_COMPONENT_MAPPING_SHIFT * 4));
  D3D12_FILTER_REDUCTION_TYPE_MASK                                         = $0000003;
  D3D12_FILTER_REDUCTION_TYPE_SHIFT                                        = 7;
  D3D12_FILTER_TYPE_MASK                                                   = $0000003;
  D3D12_MIN_FILTER_SHIFT                                                   = 4;
  D3D12_MAG_FILTER_SHIFT                                                   = 2;
  D3D12_MIP_FILTER_SHIFT                                                   = 0;
  D3D12_ANISOTROPIC_FILTERING_BIT                                          = $00000040;


  //////////////////////////////////////////////////////////////////////////////
  //
  // MACRO'S  >> See: "Converted MACRO'S" below
  //
  //////////////////////////////////////////////////////////////////////////////


  D3D12_PROTECTED_RESOURCES_SESSION_HARDWARE_PROTECTED : TGUID = '{62B0084E-C70E-4DAA-A109-30FF8D5A0482}';

  D3D12_SHADING_RATE_X_AXIS_SHIFT     = 2;
  D3D12_SHADING_RATE_VALID_MASK       = 3;


// Forwarded enumerations

type
  PD3D12_PIPELINE_STATE_FLAGS = ^D3D12_PIPELINE_STATE_FLAGS;
  D3D12_PIPELINE_STATE_FLAGS = UINT;
const
    D3D12_PIPELINE_STATE_FLAG_NONE       = D3D12_PIPELINE_STATE_FLAGS($0);
    D3D12_PIPELINE_STATE_FLAG_TOOL_DEBUG = D3D12_PIPELINE_STATE_FLAGS($1);
    // Update May 2024
    D3D12_PIPELINE_STATE_FLAG_DYNAMIC_DEPTH_BIAS = D3D12_PIPELINE_STATE_FLAGS($4);
    D3D12_PIPELINE_STATE_FLAG_DYNAMIC_INDEX_BUFFER_STRIP_CUT = D3D12_PIPELINE_STATE_FLAGS($8);
    // ==

type
  PD3D12_SHADER_CACHE_SUPPORT_FLAGS = ^D3D12_SHADER_CACHE_SUPPORT_FLAGS;
  D3D12_SHADER_CACHE_SUPPORT_FLAGS = UINT;
const
    D3D12_SHADER_CACHE_SUPPORT_NONE                   = D3D12_SHADER_CACHE_SUPPORT_FLAGS($0);
    D3D12_SHADER_CACHE_SUPPORT_SINGLE_PSO             = D3D12_SHADER_CACHE_SUPPORT_FLAGS($1);  // Always supported
    D3D12_SHADER_CACHE_SUPPORT_LIBRARY                = D3D12_SHADER_CACHE_SUPPORT_FLAGS($2);
    D3D12_SHADER_CACHE_SUPPORT_AUTOMATIC_INPROC_CACHE = D3D12_SHADER_CACHE_SUPPORT_FLAGS($4);
    D3D12_SHADER_CACHE_SUPPORT_AUTOMATIC_DISK_CACHE   = D3D12_SHADER_CACHE_SUPPORT_FLAGS($8);
    D3D12_SHADER_CACHE_SUPPORT_DRIVER_MANAGED_CACHE   = D3D12_SHADER_CACHE_SUPPORT_FLAGS($10);
    D3D12_SHADER_CACHE_SUPPORT_SHADER_CONTROL_CLEAR   = D3D12_SHADER_CACHE_SUPPORT_FLAGS($20);
    D3D12_SHADER_CACHE_SUPPORT_SHADER_SESSION_DELETE  = D3D12_SHADER_CACHE_SUPPORT_FLAGS($40);

type
  PD3D12_COMMAND_LIST_SUPPORT_FLAGS = ^D3D12_COMMAND_LIST_SUPPORT_FLAGS;
  D3D12_COMMAND_LIST_SUPPORT_FLAGS = UINT;
const
    D3D12_COMMAND_LIST_SUPPORT_FLAG_NONE          = D3D12_COMMAND_LIST_SUPPORT_FLAGS($00000000);
    D3D12_COMMAND_LIST_SUPPORT_FLAG_DIRECT        = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 0 {D3D12_COMMAND_LIST_TYPE_DIRECT});
    D3D12_COMMAND_LIST_SUPPORT_FLAG_BUNDLE        = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 1 {D3D12_COMMAND_LIST_TYPE_BUNDLE});
    D3D12_COMMAND_LIST_SUPPORT_FLAG_COMPUTE       = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 2 {D3D12_COMMAND_LIST_TYPE_COMPUTE});
    D3D12_COMMAND_LIST_SUPPORT_FLAG_COPY          = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 3 {D3D12_COMMAND_LIST_TYPE_COPY});
    D3D12_COMMAND_LIST_SUPPORT_FLAG_VIDEO_DECODE  = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 4 {D3D12_COMMAND_LIST_TYPE_VIDEO_DECODE});
    D3D12_COMMAND_LIST_SUPPORT_FLAG_VIDEO_PROCESS = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 5 {D3D12_COMMAND_LIST_TYPE_VIDEO_PROCESS});
    D3D12_COMMAND_LIST_SUPPORT_FLAG_VIDEO_ENCODE  = D3D12_COMMAND_LIST_SUPPORT_FLAGS(1 shl 6 {D3D12_COMMAND_LIST_TYPE_VIDEO_ENCODE});


type
  PD3D12_HEAP_FLAGS = ^D3D12_HEAP_FLAGS;
  D3D12_HEAP_FLAGS = UINT;
const
    D3D12_HEAP_FLAG_NONE                           = D3D12_HEAP_FLAGS($0);
    D3D12_HEAP_FLAG_SHARED                         = D3D12_HEAP_FLAGS($1);
    D3D12_HEAP_FLAG_DENY_BUFFERS                   = D3D12_HEAP_FLAGS($4);
    D3D12_HEAP_FLAG_ALLOW_DISPLAY                  = D3D12_HEAP_FLAGS($8);
    D3D12_HEAP_FLAG_SHARED_CROSS_ADAPTER           = D3D12_HEAP_FLAGS($20);
    D3D12_HEAP_FLAG_DENY_RT_DS_TEXTURES            = D3D12_HEAP_FLAGS($40);
    D3D12_HEAP_FLAG_DENY_NON_RT_DS_TEXTURES        = D3D12_HEAP_FLAGS($80);
    D3D12_HEAP_FLAG_HARDWARE_PROTECTED             = D3D12_HEAP_FLAGS($100);
    D3D12_HEAP_FLAG_ALLOW_WRITE_WATCH              = D3D12_HEAP_FLAGS($200);
    D3D12_HEAP_FLAG_ALLOW_SHADER_ATOMICS           = D3D12_HEAP_FLAGS($400);
    D3D12_HEAP_FLAG_CREATE_NOT_RESIDENT            = D3D12_HEAP_FLAGS($800);
    D3D12_HEAP_FLAG_CREATE_NOT_ZEROED              = D3D12_HEAP_FLAGS($1000);
    D3D12_HEAP_FLAG_TOOLS_USE_MANUAL_WRITE_TRACKING = D3D12_HEAP_FLAGS($2000);  // Update May 2024

    // These are convenience aliases to manage resource heap tier restrictions. They cannot be bitwise OR'ed together cleanly.
    D3D12_HEAP_FLAG_ALLOW_ALL_BUFFERS_AND_TEXTURES = D3D12_HEAP_FLAGS($0);
    D3D12_HEAP_FLAG_ALLOW_ONLY_BUFFERS             = D3D12_HEAP_FLAGS($C0);
    D3D12_HEAP_FLAG_ALLOW_ONLY_NON_RT_DS_TEXTURES  = D3D12_HEAP_FLAGS($44);
    D3D12_HEAP_FLAG_ALLOW_ONLY_RT_DS_TEXTURES      = D3D12_HEAP_FLAGS($84);


type
  PD3D12_RESOURCE_FLAGS = ^D3D12_RESOURCE_FLAGS;
  D3D12_RESOURCE_FLAGS = UINT;
const
    D3D12_RESOURCE_FLAG_NONE                              = D3D12_RESOURCE_FLAGS($0);
    D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET               = D3D12_RESOURCE_FLAGS($1);
    D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL               = D3D12_RESOURCE_FLAGS($2);
    D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS            = D3D12_RESOURCE_FLAGS($4);
    D3D12_RESOURCE_FLAG_DENY_SHADER_RESOURCE              = D3D12_RESOURCE_FLAGS($8);
    D3D12_RESOURCE_FLAG_ALLOW_CROSS_ADAPTER               = D3D12_RESOURCE_FLAGS($10);
    D3D12_RESOURCE_FLAG_ALLOW_SIMULTANEOUS_ACCESS         = D3D12_RESOURCE_FLAGS($20);
    D3D12_RESOURCE_FLAG_VIDEO_DECODE_REFERENCE_ONLY       = D3D12_RESOURCE_FLAGS($40);
    D3D12_RESOURCE_FLAG_VIDEO_ENCODE_REFERENCE_ONLY       = D3D12_RESOURCE_FLAGS($80);
    D3D12_RESOURCE_FLAG_RAYTRACING_ACCELERATION_STRUCTURE = D3D12_RESOURCE_FLAGS($100);
    { 10.0.28000.0  }
    D3D12_RESOURCE_FLAG_USE_TIGHT_ALIGNMENT                 = D3D12_RESOURCE_FLAGS($400);


type
  PD3D12_TILE_MAPPING_FLAGS = ^D3D12_TILE_MAPPING_FLAGS;
  D3D12_TILE_MAPPING_FLAGS = UINT;
const
    D3D12_TILE_MAPPING_FLAG_NONE      = D3D12_TILE_MAPPING_FLAGS($0);
    D3D12_TILE_MAPPING_FLAG_NO_HAZARD = D3D12_TILE_MAPPING_FLAGS($1);


type
  PD3D12_TILE_COPY_FLAGS = ^D3D12_TILE_COPY_FLAGS;
  D3D12_TILE_COPY_FLAGS = UINT;
const
    D3D12_TILE_COPY_FLAG_NONE                                      = D3D12_TILE_COPY_FLAGS($0);
    D3D12_TILE_COPY_FLAG_NO_HAZARD                                 = D3D12_TILE_COPY_FLAGS($1);
    D3D12_TILE_COPY_FLAG_LINEAR_BUFFER_TO_SWIZZLED_TILED_RESOURCE  = D3D12_TILE_COPY_FLAGS($2);
    D3D12_TILE_COPY_FLAG_SWIZZLED_TILED_RESOURCE_TO_LINEAR_BUFFER  = D3D12_TILE_COPY_FLAGS($4);


  // Resource Barrier
type
  PD3D12_RESOURCE_STATES = ^D3D12_RESOURCE_STATES;
  D3D12_RESOURCE_STATES = UINT;
const
    D3D12_RESOURCE_STATE_COMMON                            = D3D12_RESOURCE_STATES($0);
    D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER        = D3D12_RESOURCE_STATES($00001);
    D3D12_RESOURCE_STATE_INDEX_BUFFER                      = D3D12_RESOURCE_STATES($00002);
    D3D12_RESOURCE_STATE_RENDER_TARGET                     = D3D12_RESOURCE_STATES($00004);
    D3D12_RESOURCE_STATE_UNORDERED_ACCESS                  = D3D12_RESOURCE_STATES($00008);
    D3D12_RESOURCE_STATE_DEPTH_WRITE                       = D3D12_RESOURCE_STATES($00010);
    D3D12_RESOURCE_STATE_DEPTH_READ                        = D3D12_RESOURCE_STATES($00020);
    D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE         = D3D12_RESOURCE_STATES($00040);
    D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE             = D3D12_RESOURCE_STATES($00080);
    D3D12_RESOURCE_STATE_STREAM_OUT                        = D3D12_RESOURCE_STATES($00100);
    D3D12_RESOURCE_STATE_INDIRECT_ARGUMENT                 = D3D12_RESOURCE_STATES($00200);
    D3D12_RESOURCE_STATE_COPY_DEST                         = D3D12_RESOURCE_STATES($00400);
    D3D12_RESOURCE_STATE_COPY_SOURCE                       = D3D12_RESOURCE_STATES($00800);
    D3D12_RESOURCE_STATE_RESOLVE_DEST                      = D3D12_RESOURCE_STATES($01000);
    D3D12_RESOURCE_STATE_RESOLVE_SOURCE                    = D3D12_RESOURCE_STATES($02000);
    D3D12_RESOURCE_STATE_RAYTRACING_ACCELERATION_STRUCTURE = D3D12_RESOURCE_STATES($400000);
    D3D12_RESOURCE_STATE_SHADING_RATE_SOURCE               = D3D12_RESOURCE_STATES($1000000);

    // Update May 2024
    D3D12_RESOURCE_STATE_RESERVED_INTERNAL_8000            = D3D12_RESOURCE_STATES($8000);
    D3D12_RESOURCE_STATE_RESERVED_INTERNAL_4000            = D3D12_RESOURCE_STATES($4000);
    D3D12_RESOURCE_STATE_RESERVED_INTERNAL_100000          = D3D12_RESOURCE_STATES($100000);
    D3D12_RESOURCE_STATE_RESERVED_INTERNAL_40000000        = D3D12_RESOURCE_STATES($40000000);
    D3D12_RESOURCE_STATE_RESERVED_INTERNAL_80000000        = D3D12_RESOURCE_STATES($80000000);
    // ==

    D3D12_RESOURCE_STATE_GENERIC_READ                      = (D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER or
                                                              D3D12_RESOURCE_STATE_INDEX_BUFFER or
                                                              D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE or
                                                              D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE or
                                                              D3D12_RESOURCE_STATE_INDIRECT_ARGUMENT or
                                                              D3D12_RESOURCE_STATE_COPY_SOURCE);


    D3D12_RESOURCE_STATE_ALL_SHADER_RESOURCE               = (D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE or
                                                              D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);

    D3D12_RESOURCE_STATE_PRESENT                           = D3D12_RESOURCE_STATES($0);
    D3D12_RESOURCE_STATE_PREDICATION                       = D3D12_RESOURCE_STATE_INDIRECT_ARGUMENT;
    D3D12_RESOURCE_STATE_VIDEO_DECODE_READ                 = D3D12_RESOURCE_STATES($00010000);
    D3D12_RESOURCE_STATE_VIDEO_DECODE_WRITE                = D3D12_RESOURCE_STATES($00020000);
    D3D12_RESOURCE_STATE_VIDEO_PROCESS_READ                = D3D12_RESOURCE_STATES($00040000);
    D3D12_RESOURCE_STATE_VIDEO_PROCESS_WRITE               = D3D12_RESOURCE_STATES($00080000);
    D3D12_RESOURCE_STATE_VIDEO_ENCODE_READ                 = D3D12_RESOURCE_STATES($00200000);
    D3D12_RESOURCE_STATE_VIDEO_ENCODE_WRITE                = D3D12_RESOURCE_STATES($00800000);


type
  PD3D12_RESOURCE_BARRIER_FLAGS = ^D3D12_RESOURCE_BARRIER_FLAGS;
  D3D12_RESOURCE_BARRIER_FLAGS = UINT;
const
    D3D12_RESOURCE_BARRIER_FLAG_NONE       = D3D12_RESOURCE_BARRIER_FLAGS(0);
    D3D12_RESOURCE_BARRIER_FLAG_BEGIN_ONLY = D3D12_RESOURCE_BARRIER_FLAGS($1);
    D3D12_RESOURCE_BARRIER_FLAG_END_ONLY   = D3D12_RESOURCE_BARRIER_FLAGS($2);


type
  PD3D12_VIEW_INSTANCING_FLAGS = ^D3D12_VIEW_INSTANCING_FLAGS;
  D3D12_VIEW_INSTANCING_FLAGS = UINT;
const
    D3D12_VIEW_INSTANCING_FLAG_NONE                         = D3D12_VIEW_INSTANCING_FLAGS($0);
    D3D12_VIEW_INSTANCING_FLAG_ENABLE_VIEW_INSTANCE_MASKING = D3D12_VIEW_INSTANCING_FLAGS($1);


type
  PD3D12_BUFFER_UAV_FLAGS = ^D3D12_BUFFER_UAV_FLAGS;
  D3D12_BUFFER_UAV_FLAGS = UINT;
const
    D3D12_BUFFER_UAV_FLAG_NONE  = D3D12_BUFFER_UAV_FLAGS($0);
    D3D12_BUFFER_UAV_FLAG_RAW   = D3D12_BUFFER_UAV_FLAGS($1);


type
  PD3D12_DSV_FLAGS = ^D3D12_DSV_FLAGS;
  D3D12_DSV_FLAGS = UINT;
const
    D3D12_DSV_FLAG_NONE              = D3D12_DSV_FLAGS($0);
    D3D12_DSV_FLAG_READ_ONLY_DEPTH   = D3D12_DSV_FLAGS($1);
    D3D12_DSV_FLAG_READ_ONLY_STENCIL = D3D12_DSV_FLAGS($2);

type
  PD3D12_CLEAR_FLAGS = ^D3D12_CLEAR_FLAGS;
  D3D12_CLEAR_FLAGS = UINT;
const
    D3D12_CLEAR_FLAG_DEPTH    = D3D12_CLEAR_FLAGS($1);
    D3D12_CLEAR_FLAG_STENCIL  = D3D12_CLEAR_FLAGS($2);


type
  PD3D12_FENCE_FLAGS = ^D3D12_FENCE_FLAGS;
  D3D12_FENCE_FLAGS = UINT;
const
    D3D12_FENCE_FLAG_NONE                  = D3D12_FENCE_FLAGS($0);
    D3D12_FENCE_FLAG_SHARED                = D3D12_FENCE_FLAGS($1);
    D3D12_FENCE_FLAG_SHARED_CROSS_ADAPTER  = D3D12_FENCE_FLAGS($2);


type
  PD3D12_DESCRIPTOR_HEAP_FLAGS = ^D3D12_DESCRIPTOR_HEAP_FLAGS;
  D3D12_DESCRIPTOR_HEAP_FLAGS = UINT;
const
    D3D12_DESCRIPTOR_HEAP_FLAG_NONE           = D3D12_DESCRIPTOR_HEAP_FLAGS($0);
    D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE = D3D12_DESCRIPTOR_HEAP_FLAGS($1);


type
  PD3D12_ROOT_SIGNATURE_FLAGS = ^D3D12_ROOT_SIGNATURE_FLAGS;
  D3D12_ROOT_SIGNATURE_FLAGS = UINT;
const
    D3D12_ROOT_SIGNATURE_FLAG_NONE                                = D3D12_ROOT_SIGNATURE_FLAGS($0);
    D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT  = D3D12_ROOT_SIGNATURE_FLAGS($1);
    D3D12_ROOT_SIGNATURE_FLAG_DENY_VERTEX_SHADER_ROOT_ACCESS      = D3D12_ROOT_SIGNATURE_FLAGS($2);
    D3D12_ROOT_SIGNATURE_FLAG_DENY_HULL_SHADER_ROOT_ACCESS        = D3D12_ROOT_SIGNATURE_FLAGS($4);
    D3D12_ROOT_SIGNATURE_FLAG_DENY_DOMAIN_SHADER_ROOT_ACCESS      = D3D12_ROOT_SIGNATURE_FLAGS($8);
    D3D12_ROOT_SIGNATURE_FLAG_DENY_GEOMETRY_SHADER_ROOT_ACCESS    = D3D12_ROOT_SIGNATURE_FLAGS($10);
    D3D12_ROOT_SIGNATURE_FLAG_DENY_PIXEL_SHADER_ROOT_ACCESS       = D3D12_ROOT_SIGNATURE_FLAGS($20);
    D3D12_ROOT_SIGNATURE_FLAG_ALLOW_STREAM_OUTPUT                 = D3D12_ROOT_SIGNATURE_FLAGS($40);


type
  PD3D12_DESCRIPTOR_RANGE_FLAGS = ^D3D12_DESCRIPTOR_RANGE_FLAGS;
  D3D12_DESCRIPTOR_RANGE_FLAGS = UINT;
const
    D3D12_DESCRIPTOR_RANGE_FLAG_NONE                                            = D3D12_DESCRIPTOR_RANGE_FLAGS($0);
    D3D12_DESCRIPTOR_RANGE_FLAG_DESCRIPTORS_VOLATILE                            = D3D12_DESCRIPTOR_RANGE_FLAGS($1);
    D3D12_DESCRIPTOR_RANGE_FLAG_DATA_VOLATILE                                   = D3D12_DESCRIPTOR_RANGE_FLAGS($2);
    D3D12_DESCRIPTOR_RANGE_FLAG_DATA_STATIC_WHILE_SET_AT_EXECUTE                = D3D12_DESCRIPTOR_RANGE_FLAGS($4);
    D3D12_DESCRIPTOR_RANGE_FLAG_DATA_STATIC                                     = D3D12_DESCRIPTOR_RANGE_FLAGS($8);
    D3D12_DESCRIPTOR_RANGE_FLAG_DESCRIPTORS_STATIC_KEEPING_BUFFER_BOUNDS_CHECKS = D3D12_DESCRIPTOR_RANGE_FLAGS($10000);


type
  PD3D12_ROOT_DESCRIPTOR_FLAGS = ^D3D12_ROOT_DESCRIPTOR_FLAGS;
  D3D12_ROOT_DESCRIPTOR_FLAGS = UINT;
const
    D3D12_ROOT_DESCRIPTOR_FLAG_NONE                             = D3D12_ROOT_DESCRIPTOR_FLAGS($0);
    D3D12_ROOT_DESCRIPTOR_FLAG_DATA_VOLATILE                    = D3D12_ROOT_DESCRIPTOR_FLAGS($2);
    D3D12_ROOT_DESCRIPTOR_FLAG_DATA_STATIC_WHILE_SET_AT_EXECUTE = D3D12_ROOT_DESCRIPTOR_FLAGS($4);
    D3D12_ROOT_DESCRIPTOR_FLAG_DATA_STATIC                      = D3D12_ROOT_DESCRIPTOR_FLAGS($8);


type
  PD3D12_BUFFER_SRV_FLAGS = ^D3D12_BUFFER_SRV_FLAGS;
  D3D12_BUFFER_SRV_FLAGS = UINT;
const
    D3D12_BUFFER_SRV_FLAG_NONE = D3D12_BUFFER_SRV_FLAGS($0);
    D3D12_BUFFER_SRV_FLAG_RAW  = D3D12_BUFFER_SRV_FLAGS($00000001); // allow device multi-component reads with DWORD addressing


type
  PD3D12_MULTIPLE_FENCE_WAIT_FLAGS = ^D3D12_MULTIPLE_FENCE_WAIT_FLAGS;
  D3D12_MULTIPLE_FENCE_WAIT_FLAGS = UINT;
const
    D3D12_MULTIPLE_FENCE_WAIT_FLAG_NONE = D3D12_MULTIPLE_FENCE_WAIT_FLAGS($0);
    D3D12_MULTIPLE_FENCE_WAIT_FLAG_ANY  = D3D12_MULTIPLE_FENCE_WAIT_FLAGS($1);
    D3D12_MULTIPLE_FENCE_WAIT_FLAG_ALL  = D3D12_MULTIPLE_FENCE_WAIT_FLAGS($0);  // Alias, default behavior is ALL


  // Applications may use the whole value range of UINT for a priority.
  // These names are merely semantic suggestions.
type
  PD3D12_RESIDENCY_PRIORITY = ^D3D12_RESIDENCY_PRIORITY;
  D3D12_RESIDENCY_PRIORITY = LongInt;
const
    D3D12_RESIDENCY_PRIORITY_MINIMUM = D3D12_RESIDENCY_PRIORITY($28000000);
    D3D12_RESIDENCY_PRIORITY_LOW     = D3D12_RESIDENCY_PRIORITY($50000000);
    D3D12_RESIDENCY_PRIORITY_NORMAL  = D3D12_RESIDENCY_PRIORITY($78000000);
    D3D12_RESIDENCY_PRIORITY_HIGH    = D3D12_RESIDENCY_PRIORITY($A0010000);
    D3D12_RESIDENCY_PRIORITY_MAXIMUM = D3D12_RESIDENCY_PRIORITY($C8000000);


type
  PD3D12_RESIDENCY_FLAGS = ^D3D12_RESIDENCY_FLAGS;
  D3D12_RESIDENCY_FLAGS = UINT;
const
    D3D12_RESIDENCY_FLAG_NONE            = D3D12_RESIDENCY_FLAGS($0);
    D3D12_RESIDENCY_FLAG_DENY_OVERBUDGET = D3D12_RESIDENCY_FLAGS($1);


type
  PD3D12_COMMAND_LIST_FLAGS = ^D3D12_COMMAND_LIST_FLAGS;
  D3D12_COMMAND_LIST_FLAGS = UINT;
const
    D3D12_COMMAND_LIST_FLAG_NONE = D3D12_COMMAND_LIST_FLAGS($0);


type
  PD3D12_COMMAND_POOL_FLAGS = ^D3D12_COMMAND_POOL_FLAGS;
  D3D12_COMMAND_POOL_FLAGS = UINT;
const
    D3D12_COMMAND_POOL_FLAG_NONE = D3D12_COMMAND_POOL_FLAGS($0);


type
  PD3D12_COMMAND_RECORDER_FLAGS = ^D3D12_COMMAND_RECORDER_FLAGS;
  D3D12_COMMAND_RECORDER_FLAGS = UINT;
const
    D3D12_COMMAND_RECORDER_FLAG_NONE = D3D12_COMMAND_RECORDER_FLAGS($0);



type
  PD3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS = ^D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS;
  D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS = UINT;
const
    D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAG_NONE      = D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS($0);
    D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAG_SUPPORTED = D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS($1);


type
  PD3D12_PROTECTED_RESOURCE_SESSION_FLAGS = ^D3D12_PROTECTED_RESOURCE_SESSION_FLAGS;
  D3D12_PROTECTED_RESOURCE_SESSION_FLAGS = UINT;
const
    D3D12_PROTECTED_RESOURCE_SESSION_FLAG_NONE = D3D12_PROTECTED_RESOURCE_SESSION_FLAGS($0);


type
  PD3D12_META_COMMAND_PARAMETER_FLAGS = ^D3D12_META_COMMAND_PARAMETER_FLAGS;
  D3D12_META_COMMAND_PARAMETER_FLAGS = UINT;
const
    D3D12_META_COMMAND_PARAMETER_FLAG_INPUT  = D3D12_META_COMMAND_PARAMETER_FLAGS($00000001);
    D3D12_META_COMMAND_PARAMETER_FLAG_OUTPUT = D3D12_META_COMMAND_PARAMETER_FLAGS($00000002);


type
  PD3D12_GRAPHICS_STATES = ^D3D12_GRAPHICS_STATES;
  D3D12_GRAPHICS_STATES = UINT;
const
    D3D12_GRAPHICS_STATE_NONE                    = D3D12_GRAPHICS_STATES(0);
    D3D12_GRAPHICS_STATE_IA_VERTEX_BUFFERS       = D3D12_GRAPHICS_STATES(1 shl 0);
    D3D12_GRAPHICS_STATE_IA_INDEX_BUFFER         = D3D12_GRAPHICS_STATES(1 shl 1);
    D3D12_GRAPHICS_STATE_IA_PRIMITIVE_TOPOLOGY   = D3D12_GRAPHICS_STATES(1 shl 2);
    D3D12_GRAPHICS_STATE_DESCRIPTOR_HEAP         = D3D12_GRAPHICS_STATES(1 shl 3);
    D3D12_GRAPHICS_STATE_GRAPHICS_ROOT_SIGNATURE = D3D12_GRAPHICS_STATES(1 shl 4);
    D3D12_GRAPHICS_STATE_COMPUTE_ROOT_SIGNATURE  = D3D12_GRAPHICS_STATES(1 shl 5);
    D3D12_GRAPHICS_STATE_RS_VIEWPORTS            = D3D12_GRAPHICS_STATES(1 shl 6);
    D3D12_GRAPHICS_STATE_RS_SCISSOR_RECTS        = D3D12_GRAPHICS_STATES(1 shl 7);
    D3D12_GRAPHICS_STATE_PREDICATION             = D3D12_GRAPHICS_STATES(1 shl 8);
    D3D12_GRAPHICS_STATE_OM_RENDER_TARGETS       = D3D12_GRAPHICS_STATES(1 shl 9);
    D3D12_GRAPHICS_STATE_OM_STENCIL_REF          = D3D12_GRAPHICS_STATES(1 shl 10);
    D3D12_GRAPHICS_STATE_OM_BLEND_FACTOR         = D3D12_GRAPHICS_STATES(1 shl 11);
    D3D12_GRAPHICS_STATE_PIPELINE_STATE          = D3D12_GRAPHICS_STATES(1 shl 12);
    D3D12_GRAPHICS_STATE_SO_TARGETS              = D3D12_GRAPHICS_STATES(1 shl 13);
    D3D12_GRAPHICS_STATE_OM_DEPTH_BOUNDS         = D3D12_GRAPHICS_STATES(1 shl 14);
    D3D12_GRAPHICS_STATE_SAMPLE_POSITIONS        = D3D12_GRAPHICS_STATES(1 shl 15);
    D3D12_GRAPHICS_STATE_VIEW_INSTANCE_MASK      = D3D12_GRAPHICS_STATES(1 shl 16);


type
  PD3D12_STATE_OBJECT_FLAGS = ^D3D12_STATE_OBJECT_FLAGS;
  D3D12_STATE_OBJECT_FLAGS = UINT;
const
    D3D12_STATE_OBJECT_FLAG_NONE                                             = D3D12_STATE_OBJECT_FLAGS($0);
    D3D12_STATE_OBJECT_FLAG_ALLOW_LOCAL_DEPENDENCIES_ON_EXTERNAL_DEFINITIONS = D3D12_STATE_OBJECT_FLAGS($1);
    D3D12_STATE_OBJECT_FLAG_ALLOW_EXTERNAL_DEPENDENCIES_ON_LOCAL_DEFINITIONS = D3D12_STATE_OBJECT_FLAGS($2);
    D3D12_STATE_OBJECT_FLAG_ALLOW_STATE_OBJECT_ADDITIONS                     = D3D12_STATE_OBJECT_FLAGS($4);


type
  PD3D12_EXPORT_FLAGS = ^D3D12_EXPORT_FLAGS;
  D3D12_EXPORT_FLAGS = UINT;
const
    D3D12_EXPORT_FLAG_NONE = D3D12_EXPORT_FLAGS($0);


type
  PD3D12_RAYTRACING_PIPELINE_FLAGS = ^D3D12_RAYTRACING_PIPELINE_FLAGS;
  D3D12_RAYTRACING_PIPELINE_FLAGS = UINT;
const
    D3D12_RAYTRACING_PIPELINE_FLAG_NONE                       = D3D12_RAYTRACING_PIPELINE_FLAGS($0);
    D3D12_RAYTRACING_PIPELINE_FLAG_SKIP_TRIANGLES             = D3D12_RAYTRACING_PIPELINE_FLAGS($100);
    D3D12_RAYTRACING_PIPELINE_FLAG_SKIP_PROCEDURAL_PRIMITIVES = D3D12_RAYTRACING_PIPELINE_FLAGS($200);
    { 10.0.28000.0  }
    D3D12_RAYTRACING_PIPELINE_FLAG_ALLOW_OPACITY_MICROMAPS     = D3D12_RAYTRACING_PIPELINE_FLAGS($400);


type
  PD3D12_RAYTRACING_GEOMETRY_FLAGS = ^D3D12_RAYTRACING_GEOMETRY_FLAGS;
  D3D12_RAYTRACING_GEOMETRY_FLAGS = UINT;
const
    D3D12_RAYTRACING_GEOMETRY_FLAG_NONE                           = D3D12_RAYTRACING_GEOMETRY_FLAGS($0);
    D3D12_RAYTRACING_GEOMETRY_FLAG_OPAQUE                         = D3D12_RAYTRACING_GEOMETRY_FLAGS($1);
    D3D12_RAYTRACING_GEOMETRY_FLAG_NO_DUPLICATE_ANYHIT_INVOCATION = D3D12_RAYTRACING_GEOMETRY_FLAGS($2);


type
  PD3D12_RAYTRACING_INSTANCE_FLAGS = ^D3D12_RAYTRACING_INSTANCE_FLAGS;
  D3D12_RAYTRACING_INSTANCE_FLAGS = UINT;
const
    D3D12_RAYTRACING_INSTANCE_FLAG_NONE                            = D3D12_RAYTRACING_INSTANCE_FLAGS($0);
    D3D12_RAYTRACING_INSTANCE_FLAG_TRIANGLE_CULL_DISABLE           = D3D12_RAYTRACING_INSTANCE_FLAGS($1);
    D3D12_RAYTRACING_INSTANCE_FLAG_TRIANGLE_FRONT_COUNTERCLOCKWISE = D3D12_RAYTRACING_INSTANCE_FLAGS($2);
    D3D12_RAYTRACING_INSTANCE_FLAG_FORCE_OPAQUE                    = D3D12_RAYTRACING_INSTANCE_FLAGS($4);
    D3D12_RAYTRACING_INSTANCE_FLAG_FORCE_NON_OPAQUE                = D3D12_RAYTRACING_INSTANCE_FLAGS($8);
    { 10.0.28000.0  }
    D3D12_RAYTRACING_INSTANCE_FLAG_FORCE_OMM_2_STATE                = D3D12_RAYTRACING_INSTANCE_FLAGS($10);
    D3D12_RAYTRACING_INSTANCE_FLAG_DISABLE_OMMS                     = D3D12_RAYTRACING_INSTANCE_FLAGS($20);


type
  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS = UINT;
const
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_NONE              = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($00);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_ALLOW_UPDATE      = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($01);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_ALLOW_COMPACTION  = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($02);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_PREFER_FAST_TRACE = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($04);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_PREFER_FAST_BUILD = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($08);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_MINIMIZE_MEMORY   = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($10);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_PERFORM_UPDATE    = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($20);
    { 10.0.28000.0  }
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_ALLOW_OMM_UPDATE  = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($40);
    D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAG_ALLOW_DISABLE_OMMS = D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS($80);


type
  PD3D12_RAY_FLAGS = ^D3D12_RAY_FLAGS;
  D3D12_RAY_FLAGS = UINT;
const
    D3D12_RAY_FLAG_NONE                            = D3D12_RAY_FLAGS($00);
    D3D12_RAY_FLAG_FORCE_OPAQUE                    = D3D12_RAY_FLAGS($01);
    D3D12_RAY_FLAG_FORCE_NON_OPAQUE                = D3D12_RAY_FLAGS($02);
    D3D12_RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH = D3D12_RAY_FLAGS($04);
    D3D12_RAY_FLAG_SKIP_CLOSEST_HIT_SHADER         = D3D12_RAY_FLAGS($08);
    D3D12_RAY_FLAG_CULL_BACK_FACING_TRIANGLES      = D3D12_RAY_FLAGS($10);
    D3D12_RAY_FLAG_CULL_FRONT_FACING_TRIANGLES     = D3D12_RAY_FLAGS($20);
    D3D12_RAY_FLAG_CULL_OPAQUE                     = D3D12_RAY_FLAGS($40);
    D3D12_RAY_FLAG_CULL_NON_OPAQUE                 = D3D12_RAY_FLAGS($80);
    D3D12_RAY_FLAG_SKIP_TRIANGLES                  = D3D12_RAY_FLAGS($100);
    D3D12_RAY_FLAG_SKIP_PROCEDURAL_PRIMITIVES      = D3D12_RAY_FLAGS($200);
    { 10.0.28000.0  }
    D3D12_RAY_FLAG_FORCE_OMM_2_STATE               = D3D12_RAY_FLAGS($400);


type
  PD3D12_DRED_FLAGS = ^D3D12_DRED_FLAGS;
  D3D12_DRED_FLAGS = UINT;
const
    D3D12_DRED_FLAG_NONE                    = D3D12_DRED_FLAGS(0);
    D3D12_DRED_FLAG_FORCE_ENABLE            = D3D12_DRED_FLAGS(1);
    D3D12_DRED_FLAG_DISABLE_AUTOBREADCRUMBS = D3D12_DRED_FLAGS(2);


type
  PD3D12_DRED_ALLOCATION_TYPE = ^D3D12_DRED_ALLOCATION_TYPE;
  D3D12_DRED_ALLOCATION_TYPE = UINT;
const
    // Enum starts at 19 and skips 26 to maintain compatibility with older D3D12 drivers
    D3D12_DRED_ALLOCATION_TYPE_COMMAND_QUEUE            = D3D12_DRED_ALLOCATION_TYPE(19);
    D3D12_DRED_ALLOCATION_TYPE_COMMAND_ALLOCATOR        = D3D12_DRED_ALLOCATION_TYPE(20);
    D3D12_DRED_ALLOCATION_TYPE_PIPELINE_STATE           = D3D12_DRED_ALLOCATION_TYPE(21);
    D3D12_DRED_ALLOCATION_TYPE_COMMAND_LIST             = D3D12_DRED_ALLOCATION_TYPE(22);
    D3D12_DRED_ALLOCATION_TYPE_FENCE                    = D3D12_DRED_ALLOCATION_TYPE(23);
    D3D12_DRED_ALLOCATION_TYPE_DESCRIPTOR_HEAP          = D3D12_DRED_ALLOCATION_TYPE(24);
    D3D12_DRED_ALLOCATION_TYPE_HEAP                     = D3D12_DRED_ALLOCATION_TYPE(25);
    D3D12_DRED_ALLOCATION_TYPE_QUERY_HEAP               = D3D12_DRED_ALLOCATION_TYPE(27);
    D3D12_DRED_ALLOCATION_TYPE_COMMAND_SIGNATURE        = D3D12_DRED_ALLOCATION_TYPE(28);
    D3D12_DRED_ALLOCATION_TYPE_PIPELINE_LIBRARY         = D3D12_DRED_ALLOCATION_TYPE(29);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_DECODER            = D3D12_DRED_ALLOCATION_TYPE(30);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_PROCESSOR          = D3D12_DRED_ALLOCATION_TYPE(32);
    D3D12_DRED_ALLOCATION_TYPE_RESOURCE                 = D3D12_DRED_ALLOCATION_TYPE(34);
    D3D12_DRED_ALLOCATION_TYPE_PASS                     = D3D12_DRED_ALLOCATION_TYPE(35);
    D3D12_DRED_ALLOCATION_TYPE_CRYPTOSESSION            = D3D12_DRED_ALLOCATION_TYPE(36);
    D3D12_DRED_ALLOCATION_TYPE_CRYPTOSESSIONPOLICY      = D3D12_DRED_ALLOCATION_TYPE(37);
    D3D12_DRED_ALLOCATION_TYPE_PROTECTEDRESOURCESESSION = D3D12_DRED_ALLOCATION_TYPE(38);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_DECODER_HEAP       = D3D12_DRED_ALLOCATION_TYPE(39);
    D3D12_DRED_ALLOCATION_TYPE_COMMAND_POOL             = D3D12_DRED_ALLOCATION_TYPE(40);
    D3D12_DRED_ALLOCATION_TYPE_COMMAND_RECORDER         = D3D12_DRED_ALLOCATION_TYPE(41);
    D3D12_DRED_ALLOCATION_TYPE_STATE_OBJECT             = D3D12_DRED_ALLOCATION_TYPE(42);
    D3D12_DRED_ALLOCATION_TYPE_METACOMMAND              = D3D12_DRED_ALLOCATION_TYPE(43);
    D3D12_DRED_ALLOCATION_TYPE_SCHEDULINGGROUP          = D3D12_DRED_ALLOCATION_TYPE(44);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_MOTION_ESTIMATOR   = D3D12_DRED_ALLOCATION_TYPE(45);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_MOTION_VECTOR_HEAP = D3D12_DRED_ALLOCATION_TYPE(46);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_EXTENSION_COMMAND  = D3D12_DRED_ALLOCATION_TYPE(47);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_ENCODER            = D3D12_DRED_ALLOCATION_TYPE(48);
    D3D12_DRED_ALLOCATION_TYPE_VIDEO_ENCODER_HEAP       = D3D12_DRED_ALLOCATION_TYPE(49);

    D3D12_DRED_ALLOCATION_TYPE_INVALID                  = D3D12_DRED_ALLOCATION_TYPE($FFFFFFFF);


type
  PD3D12_DRED_PAGE_FAULT_FLAGS = ^D3D12_DRED_PAGE_FAULT_FLAGS;
  D3D12_DRED_PAGE_FAULT_FLAGS = UINT;
const
    D3D12_DRED_PAGE_FAULT_FLAGS_NONE = D3D12_DRED_PAGE_FAULT_FLAGS(0);


  // UAV Access Mode
type
  PD3D12_RENDER_PASS_FLAGS = ^D3D12_RENDER_PASS_FLAGS;
  D3D12_RENDER_PASS_FLAGS = UINT;
const
    D3D12_RENDER_PASS_FLAG_NONE             = D3D12_RENDER_PASS_FLAGS($0);
    D3D12_RENDER_PASS_FLAG_ALLOW_UAV_WRITES = D3D12_RENDER_PASS_FLAGS($1);
    D3D12_RENDER_PASS_FLAG_SUSPENDING_PASS  = D3D12_RENDER_PASS_FLAGS($2);
    D3D12_RENDER_PASS_FLAG_RESUMING_PASS    = D3D12_RENDER_PASS_FLAGS($4);
    D3D12_RENDER_PASS_FLAG_BIND_READ_ONLY_DEPTH   = D3D12_RENDER_PASS_FLAGS(8);
    D3D12_RENDER_PASS_FLAG_BIND_READ_ONLY_STENCIL = D3D12_RENDER_PASS_FLAGS($10);


type
  PD3D12_SHADER_CACHE_FLAGS = ^D3D12_SHADER_CACHE_FLAGS;
  D3D12_SHADER_CACHE_FLAGS = UINT;
const
    D3D12_SHADER_CACHE_FLAG_NONE             = D3D12_SHADER_CACHE_FLAGS($0);
    D3D12_SHADER_CACHE_FLAG_DRIVER_VERSIONED = D3D12_SHADER_CACHE_FLAGS($1);
    D3D12_SHADER_CACHE_FLAG_USE_WORKING_DIR  = D3D12_SHADER_CACHE_FLAGS($2);


type
  PD3D12_BARRIER_LAYOUT = ^D3D12_BARRIER_LAYOUT;
  D3D12_BARRIER_LAYOUT = UINT;
const
    D3D12_BARRIER_LAYOUT_UNDEFINED                      = D3D12_BARRIER_LAYOUT($FFFFFFFF);
    D3D12_BARRIER_LAYOUT_COMMON                         = D3D12_BARRIER_LAYOUT(0);
    D3D12_BARRIER_LAYOUT_PRESENT                        = D3D12_BARRIER_LAYOUT(0);
    D3D12_BARRIER_LAYOUT_GENERIC_READ                   = D3D12_BARRIER_LAYOUT(1);
    D3D12_BARRIER_LAYOUT_RENDER_TARGET                  = D3D12_BARRIER_LAYOUT(2);
    D3D12_BARRIER_LAYOUT_UNORDERED_ACCESS               = D3D12_BARRIER_LAYOUT(3);
    D3D12_BARRIER_LAYOUT_DEPTH_STENCIL_WRITE            = D3D12_BARRIER_LAYOUT(4);
    D3D12_BARRIER_LAYOUT_DEPTH_STENCIL_READ             = D3D12_BARRIER_LAYOUT(5);
    D3D12_BARRIER_LAYOUT_SHADER_RESOURCE                = D3D12_BARRIER_LAYOUT(6);
    D3D12_BARRIER_LAYOUT_COPY_SOURCE                    = D3D12_BARRIER_LAYOUT(7);
    D3D12_BARRIER_LAYOUT_COPY_DEST                      = D3D12_BARRIER_LAYOUT(8);
    D3D12_BARRIER_LAYOUT_RESOLVE_SOURCE                 = D3D12_BARRIER_LAYOUT(9);
    D3D12_BARRIER_LAYOUT_RESOLVE_DEST                   = D3D12_BARRIER_LAYOUT(10);
    D3D12_BARRIER_LAYOUT_SHADING_RATE_SOURCE            = D3D12_BARRIER_LAYOUT(11);
    D3D12_BARRIER_LAYOUT_VIDEO_DECODE_READ              = D3D12_BARRIER_LAYOUT(12);
    D3D12_BARRIER_LAYOUT_VIDEO_DECODE_WRITE             = D3D12_BARRIER_LAYOUT(13);
    D3D12_BARRIER_LAYOUT_VIDEO_PROCESS_READ             = D3D12_BARRIER_LAYOUT(14);
    D3D12_BARRIER_LAYOUT_VIDEO_PROCESS_WRITE            = D3D12_BARRIER_LAYOUT(15);
    D3D12_BARRIER_LAYOUT_VIDEO_ENCODE_READ              = D3D12_BARRIER_LAYOUT(16);
    D3D12_BARRIER_LAYOUT_VIDEO_ENCODE_WRITE             = D3D12_BARRIER_LAYOUT(17);
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_COMMON            = D3D12_BARRIER_LAYOUT(18);
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_GENERIC_READ      = D3D12_BARRIER_LAYOUT(19);
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_UNORDERED_ACCESS  = D3D12_BARRIER_LAYOUT(20);
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_SHADER_RESOURCE   = D3D12_BARRIER_LAYOUT(21);
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_COPY_SOURCE       = D3D12_BARRIER_LAYOUT(22);
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_COPY_DEST         = D3D12_BARRIER_LAYOUT(23);
    D3D12_BARRIER_LAYOUT_COMPUTE_QUEUE_COMMON           = D3D12_BARRIER_LAYOUT(24);
    D3D12_BARRIER_LAYOUT_COMPUTE_QUEUE_GENERIC_READ     = D3D12_BARRIER_LAYOUT(25);
    D3D12_BARRIER_LAYOUT_COMPUTE_QUEUE_UNORDERED_ACCESS = D3D12_BARRIER_LAYOUT(26);
    D3D12_BARRIER_LAYOUT_COMPUTE_QUEUE_SHADER_RESOURCE  = D3D12_BARRIER_LAYOUT(27);
    D3D12_BARRIER_LAYOUT_COMPUTE_QUEUE_COPY_SOURCE      = D3D12_BARRIER_LAYOUT(28);
    D3D12_BARRIER_LAYOUT_COMPUTE_QUEUE_COPY_DEST        = D3D12_BARRIER_LAYOUT(29);
    { 10.0.28000.0  }
    D3D12_BARRIER_LAYOUT_DIRECT_QUEUE_GENERIC_READ_COMPUTE_QUEUE_ACCESSIBLE = D3D12_BARRIER_LAYOUT(31);
    D3D12_BARRIER_LAYOUT_VIDEO_QUEUE_COMMON             = D3D12_BARRIER_LAYOUT(30);


type
  PD3D12_BARRIER_SYNC = ^D3D12_BARRIER_SYNC;
  D3D12_BARRIER_SYNC = UINT;
const
    D3D12_BARRIER_SYNC_NONE                                                  = D3D12_BARRIER_SYNC($0);
    D3D12_BARRIER_SYNC_ALL                                                   = D3D12_BARRIER_SYNC($1);
    D3D12_BARRIER_SYNC_DRAW                                                  = D3D12_BARRIER_SYNC($2);
    D3D12_BARRIER_SYNC_INDEX_INPUT                                           = D3D12_BARRIER_SYNC($4); // Update May 2014
    D3D12_BARRIER_SYNC_VERTEX_SHADING                                        = D3D12_BARRIER_SYNC($8);
    D3D12_BARRIER_SYNC_PIXEL_SHADING                                         = D3D12_BARRIER_SYNC($10);
    D3D12_BARRIER_SYNC_DEPTH_STENCIL                                         = D3D12_BARRIER_SYNC($20);
    D3D12_BARRIER_SYNC_RENDER_TARGET                                         = D3D12_BARRIER_SYNC($40);
    D3D12_BARRIER_SYNC_COMPUTE_SHADING                                       = D3D12_BARRIER_SYNC($80);
    D3D12_BARRIER_SYNC_RAYTRACING                                            = D3D12_BARRIER_SYNC($100);
    D3D12_BARRIER_SYNC_COPY                                                  = D3D12_BARRIER_SYNC($200);
    D3D12_BARRIER_SYNC_RESOLVE                                               = D3D12_BARRIER_SYNC($400);
    D3D12_BARRIER_SYNC_EXECUTE_INDIRECT                                      = D3D12_BARRIER_SYNC($800);
    D3D12_BARRIER_SYNC_PREDICATION                                           = D3D12_BARRIER_SYNC($800);  // Aliased with SYNC_EXECUTE_INDIRECT
    D3D12_BARRIER_SYNC_ALL_SHADING                                           = D3D12_BARRIER_SYNC($1000);
    D3D12_BARRIER_SYNC_NON_PIXEL_SHADING                                     = D3D12_BARRIER_SYNC($2000);
    D3D12_BARRIER_SYNC_EMIT_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO = D3D12_BARRIER_SYNC($4000);
    D3D12_BARRIER_SYNC_CLEAR_UNORDERED_ACCESS_VIEW                           = D3D12_BARRIER_SYNC($8000);  // Update May 2014
    D3D12_BARRIER_SYNC_VIDEO_DECODE                                          = D3D12_BARRIER_SYNC($100000);
    D3D12_BARRIER_SYNC_VIDEO_PROCESS                                         = D3D12_BARRIER_SYNC($200000);
    D3D12_BARRIER_SYNC_VIDEO_ENCODE                                          = D3D12_BARRIER_SYNC($400000);
    D3D12_BARRIER_SYNC_BUILD_RAYTRACING_ACCELERATION_STRUCTURE               = D3D12_BARRIER_SYNC($800000);
    D3D12_BARRIER_SYNC_COPY_RAYTRACING_ACCELERATION_STRUCTURE                = D3D12_BARRIER_SYNC($1000000);
    D3D12_BARRIER_SYNC_SPLIT                                                 = D3D12_BARRIER_SYNC($80000000);


type
  PD3D12_BARRIER_ACCESS = ^D3D12_BARRIER_ACCESS;
  D3D12_BARRIER_ACCESS = UINT;
const
    D3D12_BARRIER_ACCESS_COMMON                                  = D3D12_BARRIER_ACCESS(0);
    D3D12_BARRIER_ACCESS_VERTEX_BUFFER                           = D3D12_BARRIER_ACCESS($1);
    D3D12_BARRIER_ACCESS_CONSTANT_BUFFER                         = D3D12_BARRIER_ACCESS($2);
    D3D12_BARRIER_ACCESS_INDEX_BUFFER                            = D3D12_BARRIER_ACCESS($4);
    D3D12_BARRIER_ACCESS_RENDER_TARGET                           = D3D12_BARRIER_ACCESS($8);
    D3D12_BARRIER_ACCESS_UNORDERED_ACCESS                        = D3D12_BARRIER_ACCESS($10);
    D3D12_BARRIER_ACCESS_DEPTH_STENCIL_WRITE                     = D3D12_BARRIER_ACCESS($20);
    D3D12_BARRIER_ACCESS_DEPTH_STENCIL_READ                      = D3D12_BARRIER_ACCESS($40);
    D3D12_BARRIER_ACCESS_SHADER_RESOURCE                         = D3D12_BARRIER_ACCESS($80);
    D3D12_BARRIER_ACCESS_STREAM_OUTPUT                           = D3D12_BARRIER_ACCESS($100);
    D3D12_BARRIER_ACCESS_INDIRECT_ARGUMENT                       = D3D12_BARRIER_ACCESS($200);
    D3D12_BARRIER_ACCESS_PREDICATION                             = D3D12_BARRIER_ACCESS($200);  // Aliased with ACCESS_INDIRECT_ARGUMENT
    D3D12_BARRIER_ACCESS_COPY_DEST                               = D3D12_BARRIER_ACCESS($400);
    D3D12_BARRIER_ACCESS_COPY_SOURCE                             = D3D12_BARRIER_ACCESS($800);
    D3D12_BARRIER_ACCESS_RESOLVE_DEST                            = D3D12_BARRIER_ACCESS($1000);
    D3D12_BARRIER_ACCESS_RESOLVE_SOURCE                          = D3D12_BARRIER_ACCESS($2000);
    D3D12_BARRIER_ACCESS_RAYTRACING_ACCELERATION_STRUCTURE_READ  = D3D12_BARRIER_ACCESS($4000);
    D3D12_BARRIER_ACCESS_RAYTRACING_ACCELERATION_STRUCTURE_WRITE = D3D12_BARRIER_ACCESS($8000);
    D3D12_BARRIER_ACCESS_SHADING_RATE_SOURCE                     = D3D12_BARRIER_ACCESS($10000);
    D3D12_BARRIER_ACCESS_VIDEO_DECODE_READ                       = D3D12_BARRIER_ACCESS($20000);
    D3D12_BARRIER_ACCESS_VIDEO_DECODE_WRITE                      = D3D12_BARRIER_ACCESS($40000);
    D3D12_BARRIER_ACCESS_VIDEO_PROCESS_READ                      = D3D12_BARRIER_ACCESS($80000);
    D3D12_BARRIER_ACCESS_VIDEO_PROCESS_WRITE                     = D3D12_BARRIER_ACCESS($100000);
    D3D12_BARRIER_ACCESS_VIDEO_ENCODE_READ                       = D3D12_BARRIER_ACCESS($200000);
    D3D12_BARRIER_ACCESS_VIDEO_ENCODE_WRITE                      = D3D12_BARRIER_ACCESS($400000);
    D3D12_BARRIER_ACCESS_NO_ACCESS                               = D3D12_BARRIER_ACCESS($80000000);


type
  PD3D12_TEXTURE_BARRIER_FLAGS = ^D3D12_TEXTURE_BARRIER_FLAGS;
  D3D12_TEXTURE_BARRIER_FLAGS = UINT;
const
    D3D12_TEXTURE_BARRIER_FLAG_NONE    = D3D12_TEXTURE_BARRIER_FLAGS($0);
    D3D12_TEXTURE_BARRIER_FLAG_DISCARD = D3D12_TEXTURE_BARRIER_FLAGS($1);

type
  PD3D12_SHADER_CACHE_KIND_FLAGS = ^D3D12_SHADER_CACHE_KIND_FLAGS;
  D3D12_SHADER_CACHE_KIND_FLAGS = UINT;
const
    D3D12_SHADER_CACHE_KIND_FLAG_IMPLICIT_D3D_CACHE_FOR_DRIVER = D3D12_SHADER_CACHE_KIND_FLAGS($1);
    D3D12_SHADER_CACHE_KIND_FLAG_IMPLICIT_D3D_CONVERSIONS      = D3D12_SHADER_CACHE_KIND_FLAGS($2);
    D3D12_SHADER_CACHE_KIND_FLAG_IMPLICIT_DRIVER_MANAGED       = D3D12_SHADER_CACHE_KIND_FLAGS($4);
    D3D12_SHADER_CACHE_KIND_FLAG_APPLICATION_MANAGED           = D3D12_SHADER_CACHE_KIND_FLAGS($8);

type
  PD3D12_RECT = ^D3D12_RECT;
  D3D12_RECT = TRECT;

  D3D12_PRIMITIVE_TOPOLOGY = D3D_PRIMITIVE_TOPOLOGY;

  D3D12_PRIMITIVE = D3D_PRIMITIVE;


const
  D3D12ExperimentalShaderModels : TGuid = (D1: $76f5573e;
                                           D2: $f13a;
                                           D3: $40f5;
                                           D4: ($b2, $97, $81, $ce, $9e, $18, $93, $3f));


  // --------------------------------------------------------------------------------------------------------------------------------
  // D3D12GetInterface
  //
  // Retrieve Global D3D12 Interface.
  //

  CLSID_D3D12Debug                     : TGUID = (D1: $f2352aeb;
                                                  D2: $dd84;
                                                  D3: $49fe;
                                                  D4: ($b9, $7b, $a9, $dc, $fd, $cc, $1b, $4f));

  CLSID_D3D12Tools                     : TGUID = (D1: $e38216b1;
                                                  D2: $3c8c;
                                                  D3: $4833;
                                                  D4: ($aa, $09, $0a, $06, $b6, $5d, $96, $c8));

  CLSID_D3D12DeviceRemovedExtendedData : TGUID = (D1: $4a75bbc4;
                                                  D2: $9ff4;
                                                  D3: $4ad8;
                                                  D4: ($9f, $18, $ab, $ae, $84, $dc, $5f, $f2));

  CLSID_D3D12SDKConfiguration          : TGUID = (D1: $7cda6aca;
                                                  D2: $a03e;
                                                  D3: $49c8;
                                                  D4: ($94, $58, $03, $34, $d2, $0e, $07, $ce));

  { 10.0.28000.0  }
  CLSID_D3D12DSRDeviceFactory          : TGUID = (D1: $bb6dd27e;
                                                  D2: $94a9;
                                                  D3: $41a6;
                                                  D4: ($9f, $1b, $13, $37, $72, $17, $24, $28));

  D3D12_STANDARD_MULTISAMPLE_PATTERN: UINT = $FFFFFFFF;
  D3D12_CENTER_MULTISAMPLE_PATTERN: UINT = $FFFFFFFE;


//----------------------------------------------------------------------------------------------------------


type

  // Forwarded interface declarations
  ID3D12CommandQueue = interface;
  ID3D12RootSignature = interface;
  ID3D12Resource = interface;
  ID3D12Device = interface;
  ID3D12Object = interface;
  ID3D12DeviceChild = Interface;
  ID3D12LifetimeTracker = interface;


  PD3D12_GPU_VIRTUAL_ADDRESS = ^D3D12_GPU_VIRTUAL_ADDRESS;
  D3D12_GPU_VIRTUAL_ADDRESS = UINT64;


  PD3D12_COMMAND_LIST_TYPE = ^D3D12_COMMAND_LIST_TYPE;
  D3D12_COMMAND_LIST_TYPE = (D3D12_COMMAND_LIST_TYPE_DIRECT        = 0,
                             D3D12_COMMAND_LIST_TYPE_BUNDLE        = 1,
                             D3D12_COMMAND_LIST_TYPE_COMPUTE       = 2,
                             D3D12_COMMAND_LIST_TYPE_COPY          = 3,
                             D3D12_COMMAND_LIST_TYPE_VIDEO_DECODE  = 4,
                             D3D12_COMMAND_LIST_TYPE_VIDEO_PROCESS = 5,
                             D3D12_COMMAND_LIST_TYPE_VIDEO_ENCODE  = 6,
                             D3D12_COMMAND_LIST_TYPE_NONE          = -1   // Update May 2024
                            );


  D3D12_COMMAND_QUEUE_FLAGS = (D3D12_COMMAND_QUEUE_FLAG_NONE                = 0,
                               D3D12_COMMAND_QUEUE_FLAG_DISABLE_GPU_TIMEOUT = $1
                              );


  PD3D12_COMMAND_QUEUE_PRIORITY = ^D3D12_COMMAND_QUEUE_PRIORITY;
  D3D12_COMMAND_QUEUE_PRIORITY = (D3D12_COMMAND_QUEUE_PRIORITY_NORMAL          = 0,
                                  D3D12_COMMAND_QUEUE_PRIORITY_HIGH            = 100,
                                  D3D12_COMMAND_QUEUE_PRIORITY_GLOBAL_REALTIME = 10000
                                 );


  PD3D12_COMMAND_QUEUE_DESC = ^D3D12_COMMAND_QUEUE_DESC;
  D3D12_COMMAND_QUEUE_DESC = record
    Type_: D3D12_COMMAND_LIST_TYPE;
    Priority: Integer;
    Flags: D3D12_COMMAND_QUEUE_FLAGS;
    NodeMask: UINT;
  end;


  PD3D12_PRIMITIVE_TOPOLOGY_TYPE = ^D3D12_PRIMITIVE_TOPOLOGY_TYPE;
  D3D12_PRIMITIVE_TOPOLOGY_TYPE = (D3D12_PRIMITIVE_TOPOLOGY_TYPE_UNDEFINED = 0,
                                   D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT     = 1,
                                   D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE      = 2,
                                   D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE  = 3,
                                   D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH     = 4
                                  );


  PD3D12_INPUT_CLASSIFICATION = ^D3D12_INPUT_CLASSIFICATION;
  D3D12_INPUT_CLASSIFICATION = (D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA  = 0,
                                D3D12_INPUT_CLASSIFICATION_PER_INSTANCE_DATA  = 1
                               );


  PD3D12_INPUT_ELEMENT_DESC = ^D3D12_INPUT_ELEMENT_DESC;
  D3D12_INPUT_ELEMENT_DESC = record
    SemanticName: LPCSTR;
    SemanticIndex: UINT;
    Format: DXGI_FORMAT;
    InputSlot: UINT;
    AlignedByteOffset: UINT;
    InputSlotClass: D3D12_INPUT_CLASSIFICATION;
    InstanceDataStepRate: UINT;
  end;


  // Keep FILL_MODE values in sync with earlier DX versions (HW consumes values directly).
  PD3D12_FILL_MODE = ^D3D12_FILL_MODE;
  // 1 was POINT in D3D, unused in D3D12
  D3D12_FILL_MODE = (D3D12_FILL_MODE_WIREFRAME  = 2,
                     D3D12_FILL_MODE_SOLID      = 3
                    );


  // Keep CULL_MODE values in sync with earlier DX versions (HW consumes values directly).
  PD3D12_CULL_MODE = ^D3D12_CULL_MODE;
  D3D12_CULL_MODE = (D3D12_CULL_MODE_NONE   = 1,
                     D3D12_CULL_MODE_FRONT  = 2,
                     D3D12_CULL_MODE_BACK   = 3
                    );


  PD3D12_SO_DECLARATION_ENTRY = ^D3D12_SO_DECLARATION_ENTRY;
  D3D12_SO_DECLARATION_ENTRY = record
    Stream: UINT;
    SemanticName: LPCSTR;
    SemanticIndex: UINT;
    StartComponent: BYTE;
    ComponentCount: BYTE;
    OutputSlot: BYTE;
  end;


  PD3D12_VIEWPORT = ^D3D12_VIEWPORT;
  D3D12_VIEWPORT = record
    TopLeftX: FLOAT;
    TopLeftY: FLOAT;
    Width: FLOAT;
    Height: FLOAT;
    MinDepth: FLOAT;
    MaxDepth: FLOAT;
  end;


  PD3D12_BOX = ^D3D12_BOX;
  D3D12_BOX = record
    left: UINT;
    top: UINT;
    front: UINT;
    right: UINT;
    bottom: UINT;
    back: UINT;
  end;


  PLUID = ^__LUID;
  __LUID = record
    LowPart: DWORD;
    HighPart: LONG;
  end;
  LUID = __LUID;


 ///////////////////////////////////////////////////////////////////////////////
 //
 // Depth-Stencil State
 //
 ///////////////////////////////////////////////////////////////////////////////

 // Keep COMPARISON_FUNC values in sync with earlier DX versions (HW consumes values directly).
  PD3D12_COMPARISON_FUNC = ^D3D12_COMPARISON_FUNC;
  D3D12_COMPARISON_FUNC = (D3D12_COMPARISON_FUNC_NONE           = 0,  // Update May 2024
                           D3D12_COMPARISON_FUNC_NEVER          = 1,
                           D3D12_COMPARISON_FUNC_LESS           = 2,
                           D3D12_COMPARISON_FUNC_EQUAL          = 3,
                           D3D12_COMPARISON_FUNC_LESS_EQUAL     = 4,
                           D3D12_COMPARISON_FUNC_GREATER        = 5,
                           D3D12_COMPARISON_FUNC_NOT_EQUAL      = 6,
                           D3D12_COMPARISON_FUNC_GREATER_EQUAL  = 7,
                           D3D12_COMPARISON_FUNC_ALWAYS         = 8
                          );


  PD3D12_DEPTH_WRITE_MASK = ^D3D12_DEPTH_WRITE_MASK;
  D3D12_DEPTH_WRITE_MASK = (D3D12_DEPTH_WRITE_MASK_ZERO  = 0,
                            D3D12_DEPTH_WRITE_MASK_ALL  = 1
                           );


  // Keep STENCILOP values in sync with earlier DX versions (HW consumes values directly).
  PD3D12_STENCIL_OP = ^D3D12_STENCIL_OP;
  D3D12_STENCIL_OP = (D3D12_STENCIL_OP_KEEP      = 1,
                      D3D12_STENCIL_OP_ZERO      = 2,
                      D3D12_STENCIL_OP_REPLACE   = 3,
                      D3D12_STENCIL_OP_INCR_SAT  = 4,
                      D3D12_STENCIL_OP_DECR_SAT  = 5,
                      D3D12_STENCIL_OP_INVERT    = 6,
                      D3D12_STENCIL_OP_INCR      = 7,
                      D3D12_STENCIL_OP_DECR      = 8
                     );


  PD3D12_DEPTH_STENCILOP_DESC = ^D3D12_DEPTH_STENCILOP_DESC;
  D3D12_DEPTH_STENCILOP_DESC = record
    StencilFailOp: D3D12_STENCIL_OP;
    StencilDepthFailOp: D3D12_STENCIL_OP;
    StencilPassOp: D3D12_STENCIL_OP;
    StencilFunc: D3D12_COMPARISON_FUNC;
  end;


  PD3D12_DEPTH_STENCIL_DESC = ^D3D12_DEPTH_STENCIL_DESC;
  D3D12_DEPTH_STENCIL_DESC = record
    DepthEnable: BOOL;
    DepthWriteMask: D3D12_DEPTH_WRITE_MASK;
    DepthFunc: D3D12_COMPARISON_FUNC;
    StencilEnable: BOOL;
    StencilReadMask: UINT8;
    StencilWriteMask: UINT8;
    FrontFace: D3D12_DEPTH_STENCILOP_DESC;
    BackFace: D3D12_DEPTH_STENCILOP_DESC;
  end;


  PD3D12_DEPTH_STENCIL_DESC1 = ^D3D12_DEPTH_STENCIL_DESC1;
  D3D12_DEPTH_STENCIL_DESC1 = record
    DepthEnable: BOOL;
    DepthWriteMask: D3D12_DEPTH_WRITE_MASK;
    DepthFunc: D3D12_COMPARISON_FUNC;
    StencilEnable: BOOL;
    StencilReadMask: UINT8;
    StencilWriteMask: UINT8;
    FrontFace: D3D12_DEPTH_STENCILOP_DESC;
    BackFace: D3D12_DEPTH_STENCILOP_DESC;
    DepthBoundsTestEnable: BOOL;
  end;

  // Update May 2024

  // Front/back face independent stencil masks
  PD3D12_DEPTH_STENCILOP_DESC1 = ^D3D12_DEPTH_STENCILOP_DESC1;
  D3D12_DEPTH_STENCILOP_DESC1 = record
    StencilFailOp: D3D12_STENCIL_OP;
    StencilDepthFailOp: D3D12_STENCIL_OP;
    StencilPassOp: D3D12_STENCIL_OP;
    StencilFunc: D3D12_COMPARISON_FUNC;
    StencilReadMask: UINT8;
    StencilWriteMask: UINT8;
  end;


  PD3D12_DEPTH_STENCIL_DESC2 = ^D3D12_DEPTH_STENCIL_DESC2;
  D3D12_DEPTH_STENCIL_DESC2 = record
    DepthEnable: BOOL;
    DepthWriteMask: D3D12_DEPTH_WRITE_MASK;
    DepthFunc: D3D12_COMPARISON_FUNC;
    StencilEnable: BOOL;
    FrontFace: D3D12_DEPTH_STENCILOP_DESC1;
    BackFace: D3D12_DEPTH_STENCILOP_DESC1;
    DepthBoundsTestEnable: BOOL;
  end;

  // ==

  //////////////////////////////////////////////////////////////////////////////
  //
  // Blend State
  //
  //////////////////////////////////////////////////////////////////////////////

  // Keep BLEND values in sync with earlier DX versions (HW consumes values directly).
  PD3D12_BLEND = ^D3D12_BLEND;
  D3D12_BLEND = (D3D12_BLEND_ZERO             = 1,
                 D3D12_BLEND_ONE              = 2,
                 D3D12_BLEND_SRC_COLOR        = 3,   // PS output oN.rgb (N is current RT being blended)
                 D3D12_BLEND_INV_SRC_COLOR    = 4,   // 1.0 - PS output oN.rgb
                 D3D12_BLEND_SRC_ALPHA        = 5,   // PS output oN.a
                 D3D12_BLEND_INV_SRC_ALPHA    = 6,   // 1.0 - PS output oN.a
                 D3D12_BLEND_DEST_ALPHA       = 7,   // RT(N).a (N is current RT being blended)
                 D3D12_BLEND_INV_DEST_ALPHA   = 8,   // 1.0 - RT(N).a
                 D3D12_BLEND_DEST_COLOR       = 9,   // RT(N).rgb
                 D3D12_BLEND_INV_DEST_COLOR   = 10,  // 1.0 - RT(N).rgb
                 D3D12_BLEND_SRC_ALPHA_SAT    = 11,  // (f,f,f,1), f = min(1 - RT(N).a, oN.a)
                 // 12 reserved (was BOTHSRCALPHA)
                 // 13 reserved (was BOTHSRCALPHA)
                 D3D12_BLEND_BLEND_FACTOR     = 14,
                 D3D12_BLEND_INV_BLEND_FACTOR = 15,
                 D3D12_BLEND_SRC1_COLOR       = 16,  // PS output o1.rgb
                 D3D12_BLEND_INV_SRC1_COLOR   = 17,  // 1.0 - PS output o1.rgb
                 D3D12_BLEND_SRC1_ALPHA       = 18,  // PS output o1.a
                 D3D12_BLEND_INV_SRC1_ALPHA   = 19,  // 1.0 - PS output o1.a
                 D3D12_BLEND_ALPHA_FACTOR     = 20,
                 D3D12_BLEND_INV_ALPHA_FACTOR = 21
                );


  // Keep BLENDOP values in sync with earlier DX versions (HW consumes values directly).
  PD3D12_BLEND_OP = ^D3D12_BLEND_OP;
  D3D12_BLEND_OP = (D3D12_BLEND_OP_ADD           = 1,
                    D3D12_BLEND_OP_SUBTRACT      = 2,
                    D3D12_BLEND_OP_REV_SUBTRACT  = 3,
                    D3D12_BLEND_OP_MIN           = 4, // min semantics are like min shader instruction
                    D3D12_BLEND_OP_MAX           = 5  // max semantics are like max shader instruction
                   );


  PD3D12_COLOR_WRITE_ENABLE = ^D3D12_COLOR_WRITE_ENABLE;
  D3D12_COLOR_WRITE_ENABLE = (D3D12_COLOR_WRITE_ENABLE_RED    = 1,
                              D3D12_COLOR_WRITE_ENABLE_GREEN  = 2,
                              D3D12_COLOR_WRITE_ENABLE_BLUE   = 4,
                              D3D12_COLOR_WRITE_ENABLE_ALPHA  = 8,
                              D3D12_COLOR_WRITE_ENABLE_ALL    = ((DWORD(D3D12_COLOR_WRITE_ENABLE_RED) or
                              DWORD(D3D12_COLOR_WRITE_ENABLE_GREEN)) or
                              DWORD(D3D12_COLOR_WRITE_ENABLE_BLUE)) or
                              DWORD(D3D12_COLOR_WRITE_ENABLE_ALPHA)
                             );


  PD3D12_LOGIC_OP = ^D3D12_LOGIC_OP;
  // Operation:
  // (s == PS output, d = RTV contents)
  D3D12_LOGIC_OP = (D3D12_LOGIC_OP_CLEAR = 0,       // 0
                    D3D12_LOGIC_OP_SET,             // 1
                    D3D12_LOGIC_OP_COPY,            // s
                    D3D12_LOGIC_OP_COPY_INVERTED,   // ~s
                    D3D12_LOGIC_OP_NOOP,            // d
                    D3D12_LOGIC_OP_INVERT,          // ~d
                    D3D12_LOGIC_OP_AND,             // s  d
                    D3D12_LOGIC_OP_NAND,            // ~(s  d)
                    D3D12_LOGIC_OP_OR,              // s | d
                    D3D12_LOGIC_OP_NOR,             // ~(s | d)
                    D3D12_LOGIC_OP_XOR,             // s ^ d
                    D3D12_LOGIC_OP_EQUIV,           // ~(s ^ d)
                    D3D12_LOGIC_OP_AND_REVERSE,     // s  ~d
                    D3D12_LOGIC_OP_AND_INVERTED,    // ~s  d
                    D3D12_LOGIC_OP_OR_REVERSE,      // s | ~d
                    D3D12_LOGIC_OP_OR_INVERTED      // ~s | d
                   );


  PD3D12_RENDER_TARGET_BLEND_DESC = ^D3D12_RENDER_TARGET_BLEND_DESC;
  D3D12_RENDER_TARGET_BLEND_DESC = record
    BlendEnable: BOOL;
    LogicOpEnable: BOOL;  // LogicOpEnable and BlendEnable can't both be True
    SrcBlend: D3D12_BLEND;
    DestBlend: D3D12_BLEND;
    BlendOp: D3D12_BLEND_OP;
    SrcBlendAlpha: D3D12_BLEND;
    DestBlendAlpha: D3D12_BLEND;
    BlendOpAlpha: D3D12_BLEND_OP;
    LogicOp: D3D12_LOGIC_OP;   // applies to RGBA
    RenderTargetWriteMask: UINT8;  // D3D12_COLOR_WRITE_ENABLE
  end;


  PD3D12_BLEND_DESC = ^D3D12_BLEND_DESC;
  D3D12_BLEND_DESC = record
    AlphaToCoverageEnable: BOOL;     // relevant to multisample antialiasing only
    IndependentBlendEnable: BOOL;    // if FALSE, then replicate the first entry in RenderTarget array to other entries
    RenderTarget: array[0..D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT - 1] of D3D12_RENDER_TARGET_BLEND_DESC;
  end;

  // Note, the array size for RenderTarget[] above is D3D12_SIMULTANEOUS_RENDERTARGET_COUNT.
  // IDL processing/generation of this header replaces the define; this comment is merely explaining what happened.


  //////////////////////////////////////////////////////////////////////////////
  //
  // Rasterizer State
  //
  //////////////////////////////////////////////////////////////////////////////

  D3D12_CONSERVATIVE_RASTERIZATION_MODE = (D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF  = 0,  // Default
                                           D3D12_CONSERVATIVE_RASTERIZATION_MODE_ON   = 1
                                          );


  PD3D12_RASTERIZER_DESC = ^D3D12_RASTERIZER_DESC;
  D3D12_RASTERIZER_DESC = record
    FillMode: D3D12_FILL_MODE;
    CullMode: D3D12_CULL_MODE;
    FrontCounterClockwise: BOOL;
    DepthBias: Integer;
    DepthBiasClamp: Single;
    SlopeScaledDepthBias: Single;
    DepthClipEnable: BOOL;
    MultisampleEnable: BOOL;
    AntialiasedLineEnable: BOOL;
    ForcedSampleCount: UINT;
    ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE;
  end;


  // Update May 2024

  PD3D12_RASTERIZER_DESC1 = ^D3D12_RASTERIZER_DESC1;
  D3D12_RASTERIZER_DESC1 = record
    FillMode: D3D12_FILL_MODE;
    CullMode: D3D12_CULL_MODE;
    FrontCounterClockwise: BOOL;
    DepthBias: FLOAT;
    DepthBiasClamp: FLOAT;
    SlopeScaledDepthBias: FLOAT;
    DepthClipEnable: BOOL;
    MultisampleEnable: BOOL;
    AntialiasedLineEnable: BOOL;
    ForcedSampleCount: UINT;
    ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE;
  end;


  PD3D12_LINE_RASTERIZATION_MODE = ^D3D12_LINE_RASTERIZATION_MODE;
  D3D12_LINE_RASTERIZATION_MODE = (D3D12_LINE_RASTERIZATION_MODE_ALIASED,
                                   D3D12_LINE_RASTERIZATION_MODE_ALPHA_ANTIALIASED,
                                   D3D12_LINE_RASTERIZATION_MODE_QUADRILATERAL_WIDE,
                                   D3D12_LINE_RASTERIZATION_MODE_QUADRILATERAL_NARROW
                                  );


  PD3D12_RASTERIZER_DESC2 = ^D3D12_RASTERIZER_DESC2;
  D3D12_RASTERIZER_DESC2 = record
    FillMode: D3D12_FILL_MODE;
    CullMode: D3D12_CULL_MODE;
    FrontCounterClockwise: BOOL;
    DepthBias: FLOAT;
    DepthBiasClamp: FLOAT;
    SlopeScaledDepthBias: FLOAT;
    DepthClipEnable: BOOL;
    LineRasterizationMode: D3D12_LINE_RASTERIZATION_MODE;
    ForcedSampleCount: UINT;
    ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE;
  end;

  // ==


  // Interface ID3D12Object
  // ======================
  //
  ID3D12Object = interface(IUnknown)
    ['{C4FEC28F-7966-4E95-9F94-F431CB56C3B8}']

    function GetPrivateData(const guid: TGUID;
                            pDataSize: PUINT;
                            pData: Pointer): HRESULT; stdcall;

    function SetPrivateData(const guid: TGUID;
                            DataSize: UINT;
                            pData: Pointer): HRESULT; stdcall;

    function SetPrivateDataInterface(const guid: TGUID;
                                     pData: IUnknown): HRESULT; stdcall;

    function SetName(Name: LPCWSTR): HRESULT; stdcall;
  end;
  IID_ID3D12Object = ID3D12Object;


  // Interface ID3D12DeviceChild
  // ============================
  //
  ID3D12DeviceChild = interface(ID3D12Object)
    ['{905DB94B-A00C-4140-9DF5-2B64CA9EA357}']

    function GetDevice(const riid: TGUID;
                       ppvDevice: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceChild = ID3D12DeviceChild;


  //////////////////////////////////////////////////////////////////////////////
  //
  // Pipeline State v1
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D12RootSignature
  // =============================
  //
  ID3D12RootSignature = interface(ID3D12DeviceChild)
    ['{C54A6B66-72DF-4EE8-8BE5-A946A1429214}']
  end;
  IID_ID3D12RootSignature = ID3D12RootSignature;


  PD3D12_SHADER_BYTECODE = ^D3D12_SHADER_BYTECODE;
  D3D12_SHADER_BYTECODE = record
    pShaderBytecode: Pointer;
    BytecodeLength: SIZE_T;
  end;


  PD3D12_STREAM_OUTPUT_DESC = ^D3D12_STREAM_OUTPUT_DESC;
  D3D12_STREAM_OUTPUT_DESC = record
    //[annotation("_Field_size_full_(NumEntries)")]
    pSODeclaration: PD3D12_SO_DECLARATION_ENTRY;
    NumEntries: UINT;
    // [annotation("_Field_size_full_(NumStrides)")]
    pBufferStrides: PUINT;
    NumStrides: UINT;
    RasterizedStream: UINT;
  end;


  PD3D12_INPUT_LAYOUT_DESC = ^D3D12_INPUT_LAYOUT_DESC;
  D3D12_INPUT_LAYOUT_DESC = record
    // [annotation("_Field_size_full_(NumElements)")]
    pInputElementDescs: PD3D12_INPUT_ELEMENT_DESC;
    NumElements: UINT;
  end;


  PD3D12_INDEX_BUFFER_STRIP_CUT_VALUE = ^D3D12_INDEX_BUFFER_STRIP_CUT_VALUE;
  D3D12_INDEX_BUFFER_STRIP_CUT_VALUE = (D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED   = 0,
                                        D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_0xFFFF     = 1,
                                        D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_0xFFFFFFFF = 2
                                       );

  { 10.0.28000.0  }
  PD3D12_STANDARD_MULTISAMPLE_QUALITY_LEVELS = ^D3D12_STANDARD_MULTISAMPLE_QUALITY_LEVELS;
  D3D12_STANDARD_MULTISAMPLE_QUALITY_LEVELS = UINT;


  PD3D12_CACHED_PIPELINE_STATE = ^D3D12_CACHED_PIPELINE_STATE;
  D3D12_CACHED_PIPELINE_STATE = record
    //[annotation("_Field_size_bytes_full_(CachedBlobSizeInBytes)")]
    pCachedBlob: Pointer;
    CachedBlobSizeInBytes: SIZE_T;
  end;


  PD3D12_GRAPHICS_PIPELINE_STATE_DESC = ^D3D12_GRAPHICS_PIPELINE_STATE_DESC;
  D3D12_GRAPHICS_PIPELINE_STATE_DESC = record
    pRootSignature: ID3D12RootSignature;
    VS: D3D12_SHADER_BYTECODE;
    PS: D3D12_SHADER_BYTECODE;
    DS: D3D12_SHADER_BYTECODE;
    HS: D3D12_SHADER_BYTECODE;
    GS: D3D12_SHADER_BYTECODE;
    StreamOutput: D3D12_STREAM_OUTPUT_DESC;
    BlendState: D3D12_BLEND_DESC;
    SampleMask: UINT;
    RasterizerState: D3D12_RASTERIZER_DESC;
    DepthStencilState: D3D12_DEPTH_STENCIL_DESC;
    InputLayout: D3D12_INPUT_LAYOUT_DESC;
    IBStripCutValue: D3D12_INDEX_BUFFER_STRIP_CUT_VALUE;
    PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE;
    NumRenderTargets: UINT;
    RTVFormats: array[0..D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT - 1] of DXGI_FORMAT;
    DSVFormat: DXGI_FORMAT;
    SampleDesc: DXGI_SAMPLE_DESC;
    NodeMask: UINT;
    CachedPSO: D3D12_CACHED_PIPELINE_STATE;
    Flags: D3D12_PIPELINE_STATE_FLAGS;
  end;


  PD3D12_COMPUTE_PIPELINE_STATE_DESC = ^D3D12_COMPUTE_PIPELINE_STATE_DESC;
  D3D12_COMPUTE_PIPELINE_STATE_DESC = record
    pRootSignature: ID3D12RootSignature;
    CS: D3D12_SHADER_BYTECODE;
    NodeMask: UINT;
    CachedPSO: D3D12_CACHED_PIPELINE_STATE;
    Flags: D3D12_PIPELINE_STATE_FLAGS;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //
  // Pipeline State v2
  //
  //////////////////////////////////////////////////////////////////////////////


  PD3D12_RT_FORMAT_ARRAY = ^D3D12_RT_FORMAT_ARRAY;
  D3D12_RT_FORMAT_ARRAY = record
    RTFormats: array [0..D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT - 1] of DXGI_FORMAT;
    NumRenderTargets: UINT;
  end;

  PD3D12_PIPELINE_STATE_STREAM_DESC = ^D3D12_PIPELINE_STATE_STREAM_DESC;
  D3D12_PIPELINE_STATE_STREAM_DESC = record
    SizeInBytes: SIZE_T;
    //[annotation("_In_reads_(_Inexpressible_(\"Dependent on size of subobjects\"))")]
    pPipelineStateSubobjectStream: Pointer;
  end;


  PD3D12_PIPELINE_STATE_SUBOBJECT_TYPE = ^D3D12_PIPELINE_STATE_SUBOBJECT_TYPE;
  D3D12_PIPELINE_STATE_SUBOBJECT_TYPE = (D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_ROOT_SIGNATURE,         // ID3D12RootSignature*
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_VS,                     // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_PS,                     // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DS,                     // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_HS,                     // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_GS,                     // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_CS,                     // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_STREAM_OUTPUT,          // D3D12_STREAM_OUTPUT_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_BLEND,                  // D3D12_BLEND_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SAMPLE_MASK,            // UINT
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RASTERIZER,             // D3D12_RASTERIZER_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL,          // D3D12_DEPTH_STENCIL_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_INPUT_LAYOUT,           // D3D12_INPUT_LAYOUT_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_IB_STRIP_CUT_VALUE,     // D3D12_INDEX_BUFFER_STRIP_CUT_VALUE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_PRIMITIVE_TOPOLOGY,     // D3D12_PRIMITIVE_TOPOLOGY_TYPE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RENDER_TARGET_FORMATS,  // D3D12_RT_FORMAT_ARRAY
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL_FORMAT,   // DXGI_FORMAT
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SAMPLE_DESC,            // DXGI_SAMPLE_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_NODE_MASK,              // UINT
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_CACHED_PSO,             // D3D12_CACHED_PIPELINE_STATE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_FLAGS,                  // D3D12_PIPELINE_STATE_FLAGS
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL1,         // D3D12_DEPTH_STENCIL_DESC1
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_VIEW_INSTANCING,        // D3D12_VIEW_INSTANCING_DESC
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_AS = 24,                // D3D12_SHADER_BYTECODE
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_MS = 25,                // D3D12_SHADER_BYTECODE
                                         // Update May 2024
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL2 = 26,    // D3D12_DEPTH_STENCIL_DESC2
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RASTERIZER1    = 27,    // D3D12_RASTERIZER_DESC1
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RASTERIZER2    = 28,    // D3D12_RASTERIZER_DESC2
                                         // ==
                                         { 10.0.28000.0  }
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SERIALIZED_ROOT_SIGNATURE = 29,
                                         D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_MAX_VALID = 30
                                        );


  //////////////////////////////////////////////////////////////////////////////
  //
  // D3D12 Caps
  //
  //////////////////////////////////////////////////////////////////////////////


  PD3D12_FEATURE = ^D3D12_FEATURE;
  D3D12_FEATURE                                         = (
    D3D12_FEATURE_D3D12_OPTIONS                         = 0,
    D3D12_FEATURE_ARCHITECTURE                          = 1,  // Deprecated by D3D12_FEATURE_ARCHITECTURE1
    D3D12_FEATURE_FEATURE_LEVELS                        = 2,
    D3D12_FEATURE_FORMAT_SUPPORT                        = 3,
    D3D12_FEATURE_MULTISAMPLE_QUALITY_LEVELS            = 4,
    D3D12_FEATURE_FORMAT_INFO                           = 5,
    D3D12_FEATURE_GPU_VIRTUAL_ADDRESS_SUPPORT           = 6,
    D3D12_FEATURE_SHADER_MODEL                          = 7,
    D3D12_FEATURE_D3D12_OPTIONS1                        = 8,
    D3D12_FEATURE_PROTECTED_RESOURCE_SESSION_SUPPORT    = 10,
    D3D12_FEATURE_ROOT_SIGNATURE                        = 12,
    D3D12_FEATURE_ARCHITECTURE1                         = 16,
    D3D12_FEATURE_D3D12_OPTIONS2                        = 18,
    D3D12_FEATURE_SHADER_CACHE                          = 19,
    D3D12_FEATURE_COMMAND_QUEUE_PRIORITY                = 20,
    D3D12_FEATURE_D3D12_OPTIONS3                        = 21,
    D3D12_FEATURE_EXISTING_HEAPS                        = 22,
    D3D12_FEATURE_D3D12_OPTIONS4                        = 23,
    D3D12_FEATURE_SERIALIZATION                         = 24,
    D3D12_FEATURE_CROSS_NODE                            = 25,
    D3D12_FEATURE_D3D12_OPTIONS5                        = 27,
    D3D12_FEATURE_DISPLAYABLE                           = 28,
    D3D12_FEATURE_D3D12_OPTIONS6                        = 30,
    D3D12_FEATURE_QUERY_META_COMMAND                    = 31,
    D3D12_FEATURE_D3D12_OPTIONS7                        = 32,
    D3D12_FEATURE_PROTECTED_RESOURCE_SESSION_TYPE_COUNT = 33,
    D3D12_FEATURE_PROTECTED_RESOURCE_SESSION_TYPES      = 34,
    D3D12_FEATURE_D3D12_OPTIONS8                        = 36,
    D3D12_FEATURE_D3D12_OPTIONS9                        = 37,
    D3D12_FEATURE_D3D12_OPTIONS10                       = 39,
    D3D12_FEATURE_D3D12_OPTIONS11                       = 40,
    D3D12_FEATURE_D3D12_OPTIONS12                       = 41,
    D3D12_FEATURE_D3D12_OPTIONS13                       = 42,
    // Update May 2024
    D3D12_FEATURE_D3D12_OPTIONS14                       = 43,
    D3D12_FEATURE_D3D12_OPTIONS15                       = 44,
    D3D12_FEATURE_D3D12_OPTIONS16                       = 45,
    D3D12_FEATURE_D3D12_OPTIONS17                       = 46,
    D3D12_FEATURE_D3D12_OPTIONS18                       = 47,
    // ==
    D3D12_FEATURE_D3D12_OPTIONS19                       = 48,
    D3D12_FEATURE_D3D12_OPTIONS20                       = 49,
    // Update May 2024
    D3D12_FEATURE_PREDICATION                           = 50,
    D3D12_FEATURE_PLACED_RESOURCE_SUPPORT_INFO          = 51,
    D3D12_FEATURE_HARDWARE_COPY                         = 52,
    D3D12_FEATURE_D3D12_OPTIONS21                       = 53,
    // ==
    { 10.0.28000.0  }
    D3D12_FEATURE_D3D12_TIGHT_ALIGNMENT                 = 54,
    D3D12_FEATURE_APPLICATION_SPECIFIC_DRIVER_STATE     = 56,
    D3D12_FEATURE_BYTECODE_BYPASS_HASH_SUPPORTED        = 57,
    D3D12_FEATURE_SHADER_CACHE_ABI_SUPPORT              = 61
  );


  PD3D12_SHADER_MIN_PRECISION_SUPPORT = ^D3D12_SHADER_MIN_PRECISION_SUPPORT;
  D3D12_SHADER_MIN_PRECISION_SUPPORT = (D3D12_SHADER_MIN_PRECISION_SUPPORT_NONE    = $0,
                                        D3D12_SHADER_MIN_PRECISION_SUPPORT_10_BIT  = $1,
                                        D3D12_SHADER_MIN_PRECISION_SUPPORT_16_BIT  = $2
                                       );


  PD3D12_TILED_RESOURCES_TIER = ^D3D12_TILED_RESOURCES_TIER;
  D3D12_TILED_RESOURCES_TIER = (D3D12_TILED_RESOURCES_TIER_NOT_SUPPORTED  = 0,
                                D3D12_TILED_RESOURCES_TIER_1              = 1,
                                D3D12_TILED_RESOURCES_TIER_2              = 2,
                                D3D12_TILED_RESOURCES_TIER_3              = 3,
                                D3D12_TILED_RESOURCES_TIER_4              = 4
                               );


  PD3D12_RESOURCE_BINDING_TIER = ^D3D12_RESOURCE_BINDING_TIER;
  D3D12_RESOURCE_BINDING_TIER = (D3D12_RESOURCE_BINDING_TIER_1  = 1,
                                 D3D12_RESOURCE_BINDING_TIER_2  = 2,
                                 D3D12_RESOURCE_BINDING_TIER_3  = 3
                                );


  PD3D12_CONSERVATIVE_RASTERIZATION_TIER = ^D3D12_CONSERVATIVE_RASTERIZATION_TIER;
  D3D12_CONSERVATIVE_RASTERIZATION_TIER = (D3D12_CONSERVATIVE_RASTERIZATION_TIER_NOT_SUPPORTED  = 0,
                                           D3D12_CONSERVATIVE_RASTERIZATION_TIER_1              = 1,
                                           D3D12_CONSERVATIVE_RASTERIZATION_TIER_2              = 2,
                                           D3D12_CONSERVATIVE_RASTERIZATION_TIER_3              = 3
                                          );


  PD3D12_FORMAT_SUPPORT1 = ^D3D12_FORMAT_SUPPORT1;
  D3D12_FORMAT_SUPPORT1 = (D3D12_FORMAT_SUPPORT1_NONE                          = 0,
                           D3D12_FORMAT_SUPPORT1_BUFFER                        = $1,
                           D3D12_FORMAT_SUPPORT1_IA_VERTEX_BUFFER              = $2,
                           D3D12_FORMAT_SUPPORT1_IA_INDEX_BUFFER               = $4,
                           D3D12_FORMAT_SUPPORT1_SO_BUFFER                     = $8,
                           D3D12_FORMAT_SUPPORT1_TEXTURE1D                     = $10,
                           D3D12_FORMAT_SUPPORT1_TEXTURE2D                     = $20,
                           D3D12_FORMAT_SUPPORT1_TEXTURE3D                     = $40,
                           D3D12_FORMAT_SUPPORT1_TEXTURECUBE                   = $80,
                           D3D12_FORMAT_SUPPORT1_SHADER_LOAD                   = $100,
                           D3D12_FORMAT_SUPPORT1_SHADER_SAMPLE                 = $200,
                           D3D12_FORMAT_SUPPORT1_SHADER_SAMPLE_COMPARISON      = $400,
                           D3D12_FORMAT_SUPPORT1_SHADER_SAMPLE_MONO_TEXT       = $800,
                           D3D12_FORMAT_SUPPORT1_MIP                           = $1000,
                           D3D12_FORMAT_SUPPORT1_RENDER_TARGET                 = $4000,
                           D3D12_FORMAT_SUPPORT1_BLENDABLE                     = $8000,
                           D3D12_FORMAT_SUPPORT1_DEPTH_STENCIL                 = $10000,
                           D3D12_FORMAT_SUPPORT1_MULTISAMPLE_RESOLVE           = $40000,
                           D3D12_FORMAT_SUPPORT1_DISPLAY                       = $80000,
                           D3D12_FORMAT_SUPPORT1_CAST_WITHIN_BIT_LAYOUT        = $100000,
                           D3D12_FORMAT_SUPPORT1_MULTISAMPLE_RENDERTARGET      = $200000,
                           D3D12_FORMAT_SUPPORT1_MULTISAMPLE_LOAD              = $400000,
                           D3D12_FORMAT_SUPPORT1_SHADER_GATHER                 = $800000,
                           D3D12_FORMAT_SUPPORT1_BACK_BUFFER_CAST              = $1000000,
                           D3D12_FORMAT_SUPPORT1_TYPED_UNORDERED_ACCESS_VIEW   = $2000000,
                           D3D12_FORMAT_SUPPORT1_SHADER_GATHER_COMPARISON      = $4000000,
                           D3D12_FORMAT_SUPPORT1_DECODER_OUTPUT                = $8000000,
                           D3D12_FORMAT_SUPPORT1_VIDEO_PROCESSOR_OUTPUT        = $10000000,
                           D3D12_FORMAT_SUPPORT1_VIDEO_PROCESSOR_INPUT         = $20000000,
                           D3D12_FORMAT_SUPPORT1_VIDEO_ENCODER                 = $40000000
                          );


  PD3D12_FORMAT_SUPPORT2 = ^D3D12_FORMAT_SUPPORT2;
  D3D12_FORMAT_SUPPORT2 = (D3D12_FORMAT_SUPPORT2_NONE                                          = 0,
                           D3D12_FORMAT_SUPPORT2_UAV_ATOMIC_ADD                                = $1,
                           D3D12_FORMAT_SUPPORT2_UAV_ATOMIC_BITWISE_OPS                        = $2,
                           D3D12_FORMAT_SUPPORT2_UAV_ATOMIC_COMPARE_STORE_OR_COMPARE_EXCHANGE  = $4,
                           D3D12_FORMAT_SUPPORT2_UAV_ATOMIC_EXCHANGE                           = $8,
                           D3D12_FORMAT_SUPPORT2_UAV_ATOMIC_SIGNED_MIN_OR_MAX                  = $10,
                           D3D12_FORMAT_SUPPORT2_UAV_ATOMIC_UNSIGNED_MIN_OR_MAX                = $20,
                           D3D12_FORMAT_SUPPORT2_UAV_TYPED_LOAD                                = $40,
                           D3D12_FORMAT_SUPPORT2_UAV_TYPED_STORE                               = $80,
                           D3D12_FORMAT_SUPPORT2_OUTPUT_MERGER_LOGIC_OP                        = $100,
                           D3D12_FORMAT_SUPPORT2_TILED                                         = $200,
                           D3D12_FORMAT_SUPPORT2_MULTIPLANE_OVERLAY                            = $4000,
                           D3D12_FORMAT_SUPPORT2_SAMPLER_FEEDBACK                              = $8000,
                           D3D12_FORMAT_SUPPORT2_DISPLAYABLE                                   = $10000
                          );


  PD3D12_MULTISAMPLE_QUALITY_LEVEL_FLAGS = ^D3D12_MULTISAMPLE_QUALITY_LEVEL_FLAGS;
  D3D12_MULTISAMPLE_QUALITY_LEVEL_FLAGS = (D3D12_MULTISAMPLE_QUALITY_LEVELS_FLAG_NONE            = 0,
                                           D3D12_MULTISAMPLE_QUALITY_LEVELS_FLAG_TILED_RESOURCE  = $1
                                          );


  PD3D12_CROSS_NODE_SHARING_TIER = ^D3D12_CROSS_NODE_SHARING_TIER;
  D3D12_CROSS_NODE_SHARING_TIER = (D3D12_CROSS_NODE_SHARING_TIER_NOT_SUPPORTED  = 0,
                                   D3D12_CROSS_NODE_SHARING_TIER_1_EMULATED     = 1,
                                   D3D12_CROSS_NODE_SHARING_TIER_1              = 2,
                                   D3D12_CROSS_NODE_SHARING_TIER_2              = 3
                                  );


  PD3D12_RESOURCE_HEAP_TIER = ^D3D12_RESOURCE_HEAP_TIER;
  D3D12_RESOURCE_HEAP_TIER = (D3D12_RESOURCE_HEAP_TIER_1  = 1,
                              D3D12_RESOURCE_HEAP_TIER_2  = 2
                             );


  PD3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER = ^D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER;
  D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER = (D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_NOT_SUPPORTED = 0,
                                              D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_1             = 1,
                                              D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_2             = 2
                                             );


  PD3D12_VIEW_INSTANCING_TIER = ^D3D12_VIEW_INSTANCING_TIER;
  D3D12_VIEW_INSTANCING_TIER = (D3D12_VIEW_INSTANCING_TIER_NOT_SUPPORTED = 0,
                                D3D12_VIEW_INSTANCING_TIER_1             = 1,
                                D3D12_VIEW_INSTANCING_TIER_2             = 2,
                                D3D12_VIEW_INSTANCING_TIER_3             = 3
                               );

  // Update May 2024
  PD3D12_WORK_GRAPHS_TIER = ^D3D12_WORK_GRAPHS_TIER;
  D3D12_WORK_GRAPHS_TIER                 = (
    D3D12_WORK_GRAPHS_TIER_NOT_SUPPORTED = 0,
    D3D12_WORK_GRAPHS_TIER_1_0           = 10
  );
  // ==


  PD3D12_FEATURE_DATA_D3D12_OPTIONS = ^D3D12_FEATURE_DATA_D3D12_OPTIONS;
  D3D12_FEATURE_DATA_D3D12_OPTIONS = record
    {_Out_} DoublePrecisionFloatShaderOps: BOOL;
    {_Out_} OutputMergerLogicOp: BOOL;
    {_Out_} MinPrecisionSupport: D3D12_SHADER_MIN_PRECISION_SUPPORT;
    {_Out_} TiledResourcesTier: D3D12_TILED_RESOURCES_TIER;
    {_Out_} ResourceBindingTier: D3D12_RESOURCE_BINDING_TIER;
    {_Out_} PSSpecifiedStencilRefSupported: BOOL;
    {_Out_} TypedUAVLoadAdditionalFormats: BOOL;
    {_Out_} ROVsSupported: BOOL;
    {_Out_} ConservativeRasterizationTier: D3D12_CONSERVATIVE_RASTERIZATION_TIER;
    {_Out_} MaxGPUVirtualAddressBitsPerResource: UINT;
    {_Out_} StandardSwizzle64KBSupported: BOOL;
    {_Out_} CrossNodeSharingTier: D3D12_CROSS_NODE_SHARING_TIER;
    {_Out_} CrossAdapterRowMajorTextureSupported: BOOL;
    {_Out_} VPAndRTArrayIndexFromAnyShaderFeedingRasterizerSupportedWithoutGSEmulation: BOOL;
    {_Out_} ResourceHeapTier: D3D12_RESOURCE_HEAP_TIER;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS1
  PD3D12_FEATURE_DATA_D3D12_OPTIONS1 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS1;
  D3D12_FEATURE_DATA_D3D12_OPTIONS1 = record
    {out} WaveOps: BOOL;
    {out} WaveLaneCountMin: UINT;
    {out} WaveLaneCountMax: UINT;
    {out} TotalLaneCount: UINT;
    {out} ExpandedComputeResourceStates: BOOL;
    {out} Int64ShaderOps: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS2
  PD3D12_FEATURE_DATA_D3D12_OPTIONS2 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS2;
  D3D12_FEATURE_DATA_D3D12_OPTIONS2 = record
    {out} DepthBoundsTestSupported: BOOL;
    {out} ProgrammableSamplePositionsTier: D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER;
  end;


  PD3D_ROOT_SIGNATURE_VERSION = ^D3D_ROOT_SIGNATURE_VERSION;
  D3D_ROOT_SIGNATURE_VERSION = (D3D_ROOT_SIGNATURE_VERSION_1   = $1,
                                D3D_ROOT_SIGNATURE_VERSION_1_0 = $1,
                                // Update May 2024
                                D3D_ROOT_SIGNATURE_VERSION_1_1 = $2,
                                D3D_ROOT_SIGNATURE_VERSION_1_2 = $3
                                // ==
                               );

  { 10.0.28000.0  }
  PD3D12_SERIALIZED_ROOT_SIGNATURE_DESC = ^D3D12_SERIALIZED_ROOT_SIGNATURE_DESC;
  D3D12_SERIALIZED_ROOT_SIGNATURE_DESC = record
    pSerializedBlob: Pointer;
    SerializedBlobSizeInBytes: SIZE_T;
  end;

  PD3D12_GLOBAL_SERIALIZED_ROOT_SIGNATURE = ^D3D12_GLOBAL_SERIALIZED_ROOT_SIGNATURE;
  D3D12_GLOBAL_SERIALIZED_ROOT_SIGNATURE = record
    Desc: D3D12_SERIALIZED_ROOT_SIGNATURE_DESC;
  end;

  PD3D12_LOCAL_SERIALIZED_ROOT_SIGNATURE = ^D3D12_LOCAL_SERIALIZED_ROOT_SIGNATURE;
  D3D12_LOCAL_SERIALIZED_ROOT_SIGNATURE = record
    Desc: D3D12_SERIALIZED_ROOT_SIGNATURE_DESC;
  end;


  PD3D12_FEATURE_DATA_ROOT_SIGNATURE = ^D3D12_FEATURE_DATA_ROOT_SIGNATURE;
  D3D12_FEATURE_DATA_ROOT_SIGNATURE = record
    {_Inout_} HighestVersion: D3D_ROOT_SIGNATURE_VERSION;
  end;

  // D3D12_FEATURE_ARCHITECTURE
  PD3D12_FEATURE_DATA_ARCHITECTURE = ^D3D12_FEATURE_DATA_ARCHITECTURE;
  D3D12_FEATURE_DATA_ARCHITECTURE = record
    {_In_} NodeIndex: UINT;
    {_Out_} TileBasedRenderer: BOOL;
    {_Out_} UMA: BOOL;
    {_Out_} CacheCoherentUMA: BOOL;
  end;


  // D3D12_FEATURE_ARCHITECTURE1
  PD3D12_FEATURE_DATA_ARCHITECTURE1 = ^D3D12_FEATURE_DATA_ARCHITECTURE1;
  D3D12_FEATURE_DATA_ARCHITECTURE1 = record
    {_In_} NodeIndex: UINT;
    {_Out_} TileBasedRenderer: BOOL;
    {_Out_} UMA: BOOL;                // Needed for applications to understand WRITE_COMBINE
    {_Out_} CacheCoherentUMA: BOOL;   // Only TRUE when UMA is also TRUE
    {_Out_} IsolatedMMU: BOOL;        // GPU accesses don't honor the same MMU functionality as the CPU
  end;


  PD3D12_FEATURE_DATA_FEATURE_LEVELS = ^D3D12_FEATURE_DATA_FEATURE_LEVELS;
  D3D12_FEATURE_DATA_FEATURE_LEVELS = record
    {_In_} NumFeatureLevels: UINT;
    pFeatureLevelsRequested: PD3D_FEATURE_LEVEL;
    {_Out_} MaxSupportedFeatureLevel: D3D_FEATURE_LEVEL;
  end;


  // D3D_SHADER_MODEL
  PD3D_SHADER_MODEL = ^D3D_SHADER_MODEL;
  D3D_SHADER_MODEL = (D3D_SHADER_MODEL_NONE    = $0, // e.g MCDM Generic devices  // Update May 2024
                      D3D_SHADER_MODEL_5_1     = $51,
                      D3D_SHADER_MODEL_6_0     = $60,
                      D3D_SHADER_MODEL_6_1     = $61,
                      D3D_SHADER_MODEL_6_2     = $62,
                      D3D_SHADER_MODEL_6_3     = $63,
                      D3D_SHADER_MODEL_6_4     = $64,
                      D3D_SHADER_MODEL_6_5     = $65,
                      D3D_SHADER_MODEL_6_6     = $66,
                      D3D_SHADER_MODEL_6_7     = $67,
                      // Update May 2024
                      D3D_SHADER_MODEL_6_8     = $68,
                      D3D_SHADER_MODEL_6_9     = $69,
                      D3D_HIGHEST_SHADER_MODEL = D3D_SHADER_MODEL_6_9
                      // ==
                     );


  PD3D12_FEATURE_DATA_SHADER_MODEL = ^D3D12_FEATURE_DATA_SHADER_MODEL;
  D3D12_FEATURE_DATA_SHADER_MODEL = record
    {_Inout_} HighestShaderModel: D3D_SHADER_MODEL;
  end;


  // D3D12_FEATURE_FORMAT_SUPPORT
  PD3D12_FEATURE_DATA_FORMAT_SUPPORT = ^D3D12_FEATURE_DATA_FORMAT_SUPPORT;
  D3D12_FEATURE_DATA_FORMAT_SUPPORT = record
    {_In_} Format: DXGI_FORMAT;
    {_Out_} Support1: D3D12_FORMAT_SUPPORT1;
    {_Out_} Support2: D3D12_FORMAT_SUPPORT2;
  end;


  PD3D12_FEATURE_DATA_MULTISAMPLE_QUALITY_LEVELS = ^D3D12_FEATURE_DATA_MULTISAMPLE_QUALITY_LEVELS;
  D3D12_FEATURE_DATA_MULTISAMPLE_QUALITY_LEVELS = record
    {_In_} Format: DXGI_FORMAT;
    {_In_} SampleCount: UINT;
    {_In_} Flags: D3D12_MULTISAMPLE_QUALITY_LEVEL_FLAGS;
    {_Out_} NumQualityLevels: UINT;
  end;


  PD3D12_FEATURE_DATA_FORMAT_INFO = ^D3D12_FEATURE_DATA_FORMAT_INFO;
  D3D12_FEATURE_DATA_FORMAT_INFO = record
    Format: DXGI_FORMAT;
    PlaneCount: UINT8;
  end;


  PD3D12_FEATURE_DATA_GPU_VIRTUAL_ADDRESS_SUPPORT = ^D3D12_FEATURE_DATA_GPU_VIRTUAL_ADDRESS_SUPPORT;
  D3D12_FEATURE_DATA_GPU_VIRTUAL_ADDRESS_SUPPORT = record
    MaxGPUVirtualAddressBitsPerResource: UINT;
    MaxGPUVirtualAddressBitsPerProcess: UINT;
  end;


  // D3D12_FEATURE_SHADER_CACHE
  PD3D12_FEATURE_DATA_SHADER_CACHE = ^D3D12_FEATURE_DATA_SHADER_CACHE;
  D3D12_FEATURE_DATA_SHADER_CACHE = record
   {_Out_} SupportFlags: D3D12_SHADER_CACHE_SUPPORT_FLAGS;
  end;


  // D3D12_FEATURE_COMMAND_QUEUE_PRIORITY
  PD3D12_FEATURE_DATA_COMMAND_QUEUE_PRIORITY = ^D3D12_FEATURE_DATA_COMMAND_QUEUE_PRIORITY;
  D3D12_FEATURE_DATA_COMMAND_QUEUE_PRIORITY = record
    {_In_} CommandListType: D3D12_COMMAND_LIST_TYPE;
    {_In_} Priority: UINT;
    {_Out_} PriorityForTypeIsSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS3
  PD3D12_FEATURE_DATA_D3D12_OPTIONS3 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS3;
  D3D12_FEATURE_DATA_D3D12_OPTIONS3 = record
    CopyQueueTimestampQueriesSupported: BOOL;
    CastingFullyTypedFormatSupported: BOOL;
    WriteBufferImmediateSupportFlags: D3D12_COMMAND_LIST_SUPPORT_FLAGS;
    ViewInstancingTier: D3D12_VIEW_INSTANCING_TIER;
    BarycentricsSupported: BOOL;
  end;


  // D3D12_FEATURE_EXISTING_HEAPS
  PD3D12_FEATURE_DATA_EXISTING_HEAPS = ^D3D12_FEATURE_DATA_EXISTING_HEAPS;
  D3D12_FEATURE_DATA_EXISTING_HEAPS = record
    {_Out_} Supported: BOOL;
  end;


  PD3D12_SHARED_RESOURCE_COMPATIBILITY_TIER = ^D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER;
  D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER = (D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER_0,
                                              D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER_1,
                                              D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER_2
                                             );


  PD3D12_FEATURE_DATA_DISPLAYABLE = ^D3D12_FEATURE_DATA_DISPLAYABLE;
  D3D12_FEATURE_DATA_DISPLAYABLE = record
    {_Out_} DisplayableTexture: BOOL;
    {_Out_} SharedResourceCompatibilityTier: D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS4
  PD3D12_FEATURE_DATA_D3D12_OPTIONS4 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS4;
  D3D12_FEATURE_DATA_D3D12_OPTIONS4 = record
    {_Out_} MSAA64KBAlignedTextureSupported: BOOL;
    {_Out_} SharedResourceCompatibilityTier: D3D12_SHARED_RESOURCE_COMPATIBILITY_TIER;
    {_Out_} Native16BitShaderOpsSupported: BOOL;
  end;


  PD3D12_HEAP_SERIALIZATION_TIER = ^D3D12_HEAP_SERIALIZATION_TIER;
  D3D12_HEAP_SERIALIZATION_TIER = (D3D12_HEAP_SERIALIZATION_TIER_0  = 0,
                                   D3D12_HEAP_SERIALIZATION_TIER_10 = 10
                                  );


  // D3D12_FEATURE_SERIALIZATION
  PD3D12_FEATURE_DATA_SERIALIZATION = ^D3D12_FEATURE_DATA_SERIALIZATION;
  D3D12_FEATURE_DATA_SERIALIZATION = record
    {_In_} NodeIndex: UINT;
    {_Out_} HeapSerializationTier: D3D12_HEAP_SERIALIZATION_TIER;
  end;


  // D3D12_FEATURE_CROSS_NODE
  PD3D12_FEATURE_DATA_CROSS_NODE = ^D3D12_FEATURE_DATA_CROSS_NODE;
  D3D12_FEATURE_DATA_CROSS_NODE = record
    SharingTier: D3D12_CROSS_NODE_SHARING_TIER;
    AtomicShaderInstructions: BOOL;
  end;


  PD3D12_RENDER_PASS_TIER = ^D3D12_RENDER_PASS_TIER;
  D3D12_RENDER_PASS_TIER = (D3D12_RENDER_PASS_TIER_0 = 0,
                            D3D12_RENDER_PASS_TIER_1 = 1,
                            D3D12_RENDER_PASS_TIER_2 = 2
                           );


  PD3D12_RAYTRACING_TIER = ^D3D12_RAYTRACING_TIER;
  D3D12_RAYTRACING_TIER = (D3D12_RAYTRACING_TIER_NOT_SUPPORTED = 0,
                           D3D12_RAYTRACING_TIER_1_0           = 10,
                           D3D12_RAYTRACING_TIER_1_1           = 11,
                           { 10.0.28000.0  }
                           D3D12_RAYTRACING_TIER_1_2           = 12
                          );


  // D3D12_FEATURE_D3D12_OPTIONS5
  PD3D12_FEATURE_DATA_D3D12_OPTIONS5 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS5;
  D3D12_FEATURE_DATA_D3D12_OPTIONS5 = record
    {_Out_} SRVOnlyTiledResourceTier3: BOOL;
    {_Out_} RenderPassesTier: D3D12_RENDER_PASS_TIER;
    {_Out_} RaytracingTier: D3D12_RAYTRACING_TIER;
  end;


  PD3D12_VARIABLE_SHADING_RATE_TIER = ^D3D12_VARIABLE_SHADING_RATE_TIER;
  D3D12_VARIABLE_SHADING_RATE_TIER                 = (
    D3D12_VARIABLE_SHADING_RATE_TIER_NOT_SUPPORTED = 0,
    D3D12_VARIABLE_SHADING_RATE_TIER_1             = 1,
    D3D12_VARIABLE_SHADING_RATE_TIER_2             = 2
  );


  // D3D12_FEATURE_D3D12_OPTIONS6
  PD3D12_FEATURE_DATA_D3D12_OPTIONS6 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS6;
  D3D12_FEATURE_DATA_D3D12_OPTIONS6 = record
    {_Out_} AdditionalShadingRatesSupported: BOOL;
    {_Out_} PerPrimitiveShadingRateSupportedWithViewportIndexing: BOOL;
    {_Out_} VariableShadingRateTier: D3D12_VARIABLE_SHADING_RATE_TIER;
    {_Out_} ShadingRateImageTileSize: UINT;
    {_Out_} BackgroundProcessingSupported: BOOL;
  end;


  PD3D12_MESH_SHADER_TIER = ^D3D12_MESH_SHADER_TIER;
  D3D12_MESH_SHADER_TIER = (D3D12_MESH_SHADER_TIER_NOT_SUPPORTED = 0,
                            D3D12_MESH_SHADER_TIER_1             = 10
                           );


  PD3D12_SAMPLER_FEEDBACK_TIER = ^D3D12_SAMPLER_FEEDBACK_TIER;
  D3D12_SAMPLER_FEEDBACK_TIER = (D3D12_SAMPLER_FEEDBACK_TIER_NOT_SUPPORTED = 0,
                                 D3D12_SAMPLER_FEEDBACK_TIER_0_9           = 90,
                                 D3D12_SAMPLER_FEEDBACK_TIER_1_0           = 100
                                );


  // D3D12_FEATURE_D3D12_OPTIONS7
  PD3D12_FEATURE_DATA_D3D12_OPTIONS7 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS7;
  D3D12_FEATURE_DATA_D3D12_OPTIONS7 = record
    {_Out_} MeshShaderTier: D3D12_MESH_SHADER_TIER;
    {_Out_} SamplerFeedbackTier: D3D12_SAMPLER_FEEDBACK_TIER;
  end;


  PD3D12_FEATURE_DATA_QUERY_META_COMMAND = ^D3D12_FEATURE_DATA_QUERY_META_COMMAND;
  D3D12_FEATURE_DATA_QUERY_META_COMMAND = record
    CommandId: TGUID;
    NodeMask: UINT;
    pQueryInputData: Pointer;
    QueryInputDataSizeInBytes: SIZE_T;
    pQueryOutputData: Pointer;
    QueryOutputDataSizeInBytes: SIZE_T;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS8
  PD3D12_FEATURE_DATA_D3D12_OPTIONS8 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS8;
  D3D12_FEATURE_DATA_D3D12_OPTIONS8 = record
    {_Out_} UnalignedBlockTexturesSupported: BOOL;
  end;


  PD3D12_WAVE_MMA_TIER = ^D3D12_WAVE_MMA_TIER;
  D3D12_WAVE_MMA_TIER = (D3D12_WAVE_MMA_TIER_NOT_SUPPORTED = 0,
                         D3D12_WAVE_MMA_TIER_1_0           = 10
                        );


  // D3D12_FEATURE_D3D12_OPTIONS9
  PD3D12_FEATURE_DATA_D3D12_OPTIONS9 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS9;
  D3D12_FEATURE_DATA_D3D12_OPTIONS9 = record
    {_Out_} MeshShaderPipelineStatsSupported: BOOL;
    {_Out_} MeshShaderSupportsFullRangeRenderTargetArrayIndex: BOOL;
    {_Out_} AtomicInt64OnTypedResourceSupported: BOOL;
    {_Out_} AtomicInt64OnGroupSharedSupported: BOOL;
    {_Out_} DerivativesInMeshAndAmplificationShadersSupported: BOOL;
    {_Out_} WaveMMATier: D3D12_WAVE_MMA_TIER;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS10
  PD3D12_FEATURE_DATA_D3D12_OPTIONS10 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS10;
  D3D12_FEATURE_DATA_D3D12_OPTIONS10 = record
    {_Out_} VariableRateShadingSumCombinerSupported: BOOL;
    {_Out_} MeshShaderPerPrimitiveShadingRateSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS11
  PD3D12_FEATURE_DATA_D3D12_OPTIONS11 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS11;
  D3D12_FEATURE_DATA_D3D12_OPTIONS11 = record
    {_Out_} AtomicInt64OnDescriptorHeapResourceSupported: BOOL;
  end;


  PD3D12_TRI_STATE = ^D3D12_TRI_STATE;
  D3D12_TRI_STATE           = (
    D3D12_TRI_STATE_UNKNOWN =  - 1,
    D3D12_TRI_STATE_FALSE   = 0,
    D3D12_TRI_STATE_TRUE    = 1
  );


  // D3D12_FEATURE_D3D12_OPTIONS12
  PD3D12_FEATURE_DATA_D3D12_OPTIONS12 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS12;
  D3D12_FEATURE_DATA_D3D12_OPTIONS12 = record
    {_Out_} MSPrimitivesPipelineStatisticIncludesCulledPrimitives: D3D12_TRI_STATE;
    {_Out_} EnhancedBarriersSupported: BOOL;
    {_Out_} RelaxedFormatCastingSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS13
  PD3D12_FEATURE_DATA_D3D12_OPTIONS13 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS13;
  D3D12_FEATURE_DATA_D3D12_OPTIONS13 = record
    {_Out_} UnrestrictedBufferTextureCopyPitchSupported: BOOL;
    {_Out_} UnrestrictedVertexElementAlignmentSupported: BOOL;
    {_Out_} InvertedViewportHeightFlipsYSupported: BOOL;
    {_Out_} InvertedViewportDepthFlipsZSupported: BOOL;
    {_Out_} TextureCopyBetweenDimensionsSupported: BOOL;
    {_Out_} AlphaBlendFactorSupported: BOOL;
  end;

  // Update May 2024

  // D3D12_FEATURE_D3D12_OPTIONS14
  PD3D12_FEATURE_DATA_D3D12_OPTIONS14 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS14;
  D3D12_FEATURE_DATA_D3D12_OPTIONS14 = record
    {_Out_} AdvancedTextureOpsSupported: BOOL;
    {_Out_} WriteableMSAATexturesSupported: BOOL;
    {_Out_} IndependentFrontAndBackStencilRefMaskSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS15
  PD3D12_FEATURE_DATA_D3D12_OPTIONS15 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS15;
  D3D12_FEATURE_DATA_D3D12_OPTIONS15 = record
    {_Out_} TriangleFanSupported: BOOL;
    {_Out_} DynamicIndexBufferStripCutSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS16
  PD3D12_FEATURE_DATA_D3D12_OPTIONS16 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS16;
  D3D12_FEATURE_DATA_D3D12_OPTIONS16 = record
    {_Out_} DynamicDepthBiasSupported: BOOL;
    {_Out_} GPUUploadHeapSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS17
  PD3D12_FEATURE_DATA_D3D12_OPTIONS17 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS17;
  D3D12_FEATURE_DATA_D3D12_OPTIONS17 = record
    {_Out_} NonNormalizedCoordinateSamplersSupported: BOOL;
    {_Out_} ManualWriteTrackingResourceSupported: BOOL;
  end;


  // D3D12_FEATURE_D3D12_OPTIONS18
  PD3D12_FEATURE_DATA_D3D12_OPTIONS18 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS18;
  D3D12_FEATURE_DATA_D3D12_OPTIONS18 = record
    {_Out_} RenderPassesValid: BOOL;
  end;

  // ==

  // D3D12_FEATURE_D3D12_OPTIONS19
  PD3D12_FEATURE_DATA_D3D12_OPTIONS19 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS19;
  D3D12_FEATURE_DATA_D3D12_OPTIONS19 = record
    MismatchingOutputDimensionsSupported: BOOL;
    SupportedSampleCountsWithNoOutputs: UINT;
    PointSamplingAddressesNeverRoundUp: BOOL;
    RasterizerDesc2Supported: BOOL;
    NarrowQuadrilateralLinesSupported: BOOL;
    AnisoFilterWithPointMipSupported: BOOL;
    MaxSamplerDescriptorHeapSize: UINT;
    MaxSamplerDescriptorHeapSizeWithStaticSamplers: UINT;
    MaxViewDescriptorHeapSize: UINT;
    {_Out_} ComputeOnlyCustomHeapSupported: BOOL;
  end;


  PD3D12_RECREATE_AT_TIER = ^D3D12_RECREATE_AT_TIER;
  D3D12_RECREATE_AT_TIER                 = (
    D3D12_RECREATE_AT_TIER_NOT_SUPPORTED = 0,
    // * Supports retrieving resource and heap allocation information
    //   with ID3D12PageableTools::GetAllocationInfo
    // * Supports setting resource and heap virtual addresses with
    //   ID3D12DeviceTools::SetNextAllocation
    D3D12_RECREATE_AT_TIER_1             = 1
  );


  // D3D12_FEATURE_D3D12_OPTIONS20
  PD3d12FeatureDataD3d12Options20 = ^TD3d12FeatureDataD3d12Options20;
  D3D12_FEATURE_DATA_D3D12_OPTIONS20 = record
    {_Out_} ComputeOnlyWriteWatchSupported: BOOL;
    RecreateAtTier: D3D12_RECREATE_AT_TIER;
  end;
  TD3d12FeatureDataD3d12Options20 = D3D12_FEATURE_DATA_D3D12_OPTIONS20;


  PD3D12_EXECUTE_INDIRECT_TIER = ^D3D12_EXECUTE_INDIRECT_TIER;
  D3D12_EXECUTE_INDIRECT_TIER       = (
    D3D12_EXECUTE_INDIRECT_TIER_1_0 = 10,
    D3D12_EXECUTE_INDIRECT_TIER_1_1 = 11
  );

  // Update MAy 2024

  PD3D12_FEATURE_DATA_D3D12_OPTIONS21 = ^D3D12_FEATURE_DATA_D3D12_OPTIONS21;
  D3D12_FEATURE_DATA_D3D12_OPTIONS21 = record
    {_Out_} WorkGraphsTier: D3D12_WORK_GRAPHS_TIER;
    {_Out_} ExecuteIndirectTier: D3D12_EXECUTE_INDIRECT_TIER;
    {_Out_} SampleCmpGradientAndBiasSupported: BOOL;
    {_Out_} ExtendedCommandInfoSupported: BOOL;
  end;

  { 10.0.28000.0 }
  PD3D12_TIGHT_ALIGNMENT_TIER = ^D3D12_TIGHT_ALIGNMENT_TIER;
  D3D12_TIGHT_ALIGNMENT_TIER = (D3D12_TIGHT_ALIGNMENT_TIER_NOT_SUPPORTED = 0,
                                D3D12_TIGHT_ALIGNMENT_TIER_1 = 1
                               );

  PD3D12_FEATURE_DATA_TIGHT_ALIGNMENT = ^D3D12_FEATURE_DATA_TIGHT_ALIGNMENT;
  D3D12_FEATURE_DATA_TIGHT_ALIGNMENT = record
    SupportTier: D3D12_TIGHT_ALIGNMENT_TIER;
  end;

  PD3D12_FEATURE_DATA_APPLICATION_SPECIFIC_DRIVER_STATE = ^D3D12_FEATURE_DATA_APPLICATION_SPECIFIC_DRIVER_STATE;
  D3D12_FEATURE_DATA_APPLICATION_SPECIFIC_DRIVER_STATE = record
    Supported: BOOL;
  end;

  PD3D12_FEATURE_DATA_BYTECODE_BYPASS_HASH_SUPPORTED = ^D3D12_FEATURE_DATA_BYTECODE_BYPASS_HASH_SUPPORTED;
  D3D12_FEATURE_DATA_BYTECODE_BYPASS_HASH_SUPPORTED = record
    Supported: BOOL;
  end;


  PD3D12_FEATURE_DATA_PREDICATION = ^D3D12_FEATURE_DATA_PREDICATION;
  D3D12_FEATURE_DATA_PREDICATION = record
    Supported: BOOL;
  end;


  PD3D12_FEATURE_DATA_HARDWARE_COPY = ^D3D12_FEATURE_DATA_HARDWARE_COPY;
  D3D12_FEATURE_DATA_HARDWARE_COPY = record
    Supported: BOOL;
  end;

  // ==


  PD3D12_RESOURCE_ALLOCATION_INFO = ^D3D12_RESOURCE_ALLOCATION_INFO;
  D3D12_RESOURCE_ALLOCATION_INFO = record
    SizeInBytes: UINT64;
    Alignment: UINT64;
  end;


  PD3D12_RESOURCE_ALLOCATION_INFO1 = ^D3D12_RESOURCE_ALLOCATION_INFO1;
  D3D12_RESOURCE_ALLOCATION_INFO1 = record
    Offset: UINT64;
    Alignment: UINT64;
    SizeInBytes: UINT64;
  end;


  PD3D12_HEAP_TYPE = ^D3D12_HEAP_TYPE;
  D3D12_HEAP_TYPE = (D3D12_HEAP_TYPE_DEFAULT    = 1,
                     D3D12_HEAP_TYPE_UPLOAD     = 2,
                     D3D12_HEAP_TYPE_READBACK   = 3,
                     D3D12_HEAP_TYPE_CUSTOM     = 4,
                     D3D12_HEAP_TYPE_GPU_UPLOAD = 5 // Update May 2024
                    );


  D3D12_CPU_PAGE_PROPERTY = (D3D12_CPU_PAGE_PROPERTY_UNKNOWN        = 0,
                             D3D12_CPU_PAGE_PROPERTY_NOT_AVAILABLE  = 1,
                             D3D12_CPU_PAGE_PROPERTY_WRITE_COMBINE  = 2,
                             D3D12_CPU_PAGE_PROPERTY_WRITE_BACK     = 3
                            );


  PD3D12_MEMORY_POOL = ^D3D12_MEMORY_POOL;
  D3D12_MEMORY_POOL = (D3D12_MEMORY_POOL_UNKNOWN  = 0,
                       D3D12_MEMORY_POOL_L0       = 1, // Maximum bandwidth for CPU
                       D3D12_MEMORY_POOL_L1       = 2  // More bandwidth for GPU, less for CPU
                      );


  PD3D12_HEAP_PROPERTIES = ^D3D12_HEAP_PROPERTIES;
  D3D12_HEAP_PROPERTIES = record
    Type_: D3D12_HEAP_TYPE;
    CPUPageProperty: D3D12_CPU_PAGE_PROPERTY;
    MemoryPoolPreference: D3D12_MEMORY_POOL;
    CreationNodeMask: UINT;
    VisibleNodeMask: UINT;
  end;


  PD3D12_HEAP_DESC = ^D3D12_HEAP_DESC;
  D3D12_HEAP_DESC = record
    SizeInBytes: UINT64;
    Properties: D3D12_HEAP_PROPERTIES;
    Alignment: UINT64;
    Flags: D3D12_HEAP_FLAGS;
  end;


  PD3D12_RESOURCE_DIMENSION = ^D3D12_RESOURCE_DIMENSION;
  D3D12_RESOURCE_DIMENSION = (D3D12_RESOURCE_DIMENSION_UNKNOWN    = 0,
                              D3D12_RESOURCE_DIMENSION_BUFFER     = 1,
                              D3D12_RESOURCE_DIMENSION_TEXTURE1D  = 2,
                              D3D12_RESOURCE_DIMENSION_TEXTURE2D  = 3,
                              D3D12_RESOURCE_DIMENSION_TEXTURE3D  = 4
                             );

  // Update May 2024
  PD3D12_FEATURE_DATA_PLACED_RESOURCE_SUPPORT_INFO = ^D3D12_FEATURE_DATA_PLACED_RESOURCE_SUPPORT_INFO;
  D3D12_FEATURE_DATA_PLACED_RESOURCE_SUPPORT_INFO = record
    {_In_} Format: DXGI_FORMAT;
    {_In_} Dimension: D3D12_RESOURCE_DIMENSION;
    {_Out_} DestHeapProperties: D3D12_HEAP_PROPERTIES;
    {_Out_} Supported: BOOL;
  end;
  // ==


  PD3D12_TEXTURE_LAYOUT = ^D3D12_TEXTURE_LAYOUT;
  D3D12_TEXTURE_LAYOUT = (D3D12_TEXTURE_LAYOUT_UNKNOWN                 = 0,
                          D3D12_TEXTURE_LAYOUT_ROW_MAJOR               = 1,
                          D3D12_TEXTURE_LAYOUT_64KB_UNDEFINED_SWIZZLE  = 2,
                          D3D12_TEXTURE_LAYOUT_64KB_STANDARD_SWIZZLE   = 3
                         );


  PD3D12_MIP_REGION = ^D3D12_MIP_REGION;
  D3D12_MIP_REGION = record
    Width: UINT;
    Height: UINT;
    Depth: UINT;
  end;


  PD3D12_RESOURCE_DESC = ^D3D12_RESOURCE_DESC;
  D3D12_RESOURCE_DESC = record
    Dimension: D3D12_RESOURCE_DIMENSION;
    Alignment: UINT64;
    Width: UINT64;
    Height: UINT;
    DepthOrArraySize: UINT16;
    MipLevels: UINT16;
    Format: DXGI_FORMAT;
    SampleDesc: DXGI_SAMPLE_DESC;
    Layout: D3D12_TEXTURE_LAYOUT;
    Flags: D3D12_RESOURCE_FLAGS;
  end;


  PD3D12_RESOURCE_DESC1 = ^D3D12_RESOURCE_DESC1;
  D3D12_RESOURCE_DESC1 = record
    Dimension: D3D12_RESOURCE_DIMENSION;
    Alignment: UINT64;
    Width: UINT64;
    Height: UINT;
    DepthOrArraySize: UINT16;
    MipLevels: UINT16;
    Format: DXGI_FORMAT;
    SampleDesc: DXGI_SAMPLE_DESC;
    Layout: D3D12_TEXTURE_LAYOUT;
    Flags: D3D12_RESOURCE_FLAGS;
    SamplerFeedbackMipRegion: D3D12_MIP_REGION;
  end;


  PD3D12_DEPTH_STENCIL_VALUE = ^D3D12_DEPTH_STENCIL_VALUE;
  D3D12_DEPTH_STENCIL_VALUE = record
    Depth: Single;
    Stencil: UINT8;
  end;


  PD3D12_CLEAR_VALUE = ^D3D12_CLEAR_VALUE;
  {$ALIGN 4}
  D3D12_CLEAR_VALUE = record
    Format: DXGI_FORMAT;
    case Integer of
      0: (Color: Fixed3FLOATArray);
      1: (DepthStencil: D3D12_DEPTH_STENCIL_VALUE);
  end;
  {$ALIGN 8}


  PD3D12_RANGE = ^D3D12_RANGE;
  D3D12_RANGE = record
    _Begin: SIZE_T;
    _End: SIZE_T;
  end;


  PD3D12_RANGE_UINT64 = ^D3D12_RANGE_UINT64;
  D3D12_RANGE_UINT64 = record
    Begin_: UINT64;
    End_: UINT64;      // One past end, so (End - Begin) = Size
  end;


  PD3D12_SUBRESOURCE_RANGE_UINT64 = ^D3D12_SUBRESOURCE_RANGE_UINT64;
  D3D12_SUBRESOURCE_RANGE_UINT64 = record
    Subresource: UINT;
    Range: D3D12_RANGE_UINT64;
  end;


  PD3D12_SUBRESOURCE_INFO = ^D3D12_SUBRESOURCE_INFO;
  D3D12_SUBRESOURCE_INFO = record
    Offset: UINT64;
    RowPitch: UINT;
    DepthPitch: UINT;
  end;


  PD3D12_TILED_RESOURCE_COORDINATE = ^D3D12_TILED_RESOURCE_COORDINATE;
  D3D12_TILED_RESOURCE_COORDINATE = record
    X: UINT;
    Y: UINT;
    Z: UINT;
    Subresource: UINT;
  end;


  PD3D12_TILE_REGION_SIZE = ^D3D12_TILE_REGION_SIZE;
  D3D12_TILE_REGION_SIZE = record
    NumTiles: UINT;
    UseBox: BOOL;
    Width: UINT;
    Height: UINT16;
    Depth: UINT16;
  end;


  PD3D12_TILE_RANGE_FLAGS = ^D3D12_TILE_RANGE_FLAGS;
  D3D12_TILE_RANGE_FLAGS = (D3D12_TILE_RANGE_FLAG_NONE               = 0,
                            D3D12_TILE_RANGE_FLAG_NULL               = 1,
                            D3D12_TILE_RANGE_FLAG_SKIP               = 2,
                            D3D12_TILE_RANGE_FLAG_REUSE_SINGLE_TILE  = 4
                           );


  PD3D12_SUBRESOURCE_TILING = ^D3D12_SUBRESOURCE_TILING;
  D3D12_SUBRESOURCE_TILING = record
    WidthInTiles: UINT;
    HeightInTiles: UINT16;
    DepthInTiles: UINT16;
    StartTileIndexInOverallResource: UINT;
  end;


  PD3D12_TILE_SHAPE = ^D3D12_TILE_SHAPE;
  D3D12_TILE_SHAPE = record
    WidthInTexels: UINT;
    HeightInTexels: UINT;
    DepthInTexels: UINT;
  end;


  PD3D12_PACKED_MIP_INFO = ^D3D12_PACKED_MIP_INFO;
  D3D12_PACKED_MIP_INFO = record
    NumStandardMips: UINT8;
    NumPackedMips: UINT8;
    NumTilesForPackedMips: UINT;
    StartTileIndexInOverallResource: UINT;
  end;



  PD3D12_RESOURCE_BARRIER_TYPE = ^D3D12_RESOURCE_BARRIER_TYPE;
  D3D12_RESOURCE_BARRIER_TYPE = (D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
                                 D3D12_RESOURCE_BARRIER_TYPE_ALIASING,
                                 D3D12_RESOURCE_BARRIER_TYPE_UAV
                                );



  PD3D12_RESOURCE_TRANSITION_BARRIER = ^D3D12_RESOURCE_TRANSITION_BARRIER;
  D3D12_RESOURCE_TRANSITION_BARRIER = record
  private
    FResource: Pointer;
    FSubresource: UINT;
    FStateBefore: D3D12_RESOURCE_STATES;
    FStateAfter: D3D12_RESOURCE_STATES;

    function GetpResource: ID3D12Resource;
    procedure SetpResource(Value: ID3D12Resource);

  public
    property pResource: ID3D12Resource read GetpResource write SetpResource;
    property Subresource: UINT read FSubresource write FSubresource;
    property StateBefore: D3D12_RESOURCE_STATES read FStateBefore write FStateBefore;
    property StateAfter: D3D12_RESOURCE_STATES read FStateAfter write FStateAfter;
  end;


  PD3D12_RESOURCE_ALIASING_BARRIER = ^D3D12_RESOURCE_ALIASING_BARRIER;
  D3D12_RESOURCE_ALIASING_BARRIER = record
  private
    FResourceBefore: Pointer;
    FResourceAfter: Pointer;

    function GetpResourceAfter: ID3D12Resource;
    function GetpResourceBefore: ID3D12Resource;
    procedure SetpResourceAfter(Value: ID3D12Resource);
    procedure SetpResourceBefore(Value: ID3D12Resource);

  public
    property pResourceBefore: ID3D12Resource read GetpResourceBefore write SetpResourceBefore;
    property pResourceAfter: ID3D12Resource read GetpResourceAfter write SetpResourceAfter;
  end;


  PD3D12_RESOURCE_UAV_BARRIER = ^D3D12_RESOURCE_UAV_BARRIER;
  D3D12_RESOURCE_UAV_BARRIER = record
  private
    FResource: Pointer;

    function GetpResource: ID3D12Resource;
    procedure SetpResource(Value: ID3D12Resource);

  public
    property pResource: ID3D12Resource read GetpResource write SetpResource;
  end;


  PD3D12_RESOURCE_BARRIER = ^D3D12_RESOURCE_BARRIER;
  D3D12_RESOURCE_BARRIER = record
    Type_: D3D12_RESOURCE_BARRIER_TYPE;
    Flags: D3D12_RESOURCE_BARRIER_FLAGS;
    case Integer of
      0: (Transition: D3D12_RESOURCE_TRANSITION_BARRIER);
      1: (Aliasing: D3D12_RESOURCE_ALIASING_BARRIER);
      2: (UAV: D3D12_RESOURCE_UAV_BARRIER);
  end;


  PD3D12_SUBRESOURCE_FOOTPRINT = ^D3D12_SUBRESOURCE_FOOTPRINT;
  D3D12_SUBRESOURCE_FOOTPRINT = record
    Format: DXGI_FORMAT;
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    RowPitch: UINT;
  end;


  PD3D12_PLACED_SUBRESOURCE_FOOTPRINT = ^D3D12_PLACED_SUBRESOURCE_FOOTPRINT;
  D3D12_PLACED_SUBRESOURCE_FOOTPRINT = record
    Offset: UINT64;  // Must be a multiple of D3D12_TEXTURE_DATA_PLACEMENT_ALIGNMENT
    Footprint: D3D12_SUBRESOURCE_FOOTPRINT;
  end;


  PD3D12_TEXTURE_COPY_TYPE = ^D3D12_TEXTURE_COPY_TYPE;
  D3D12_TEXTURE_COPY_TYPE = (D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX = 0,
                             D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT  = 1
                            );


  PD3D12_TEXTURE_COPY_LOCATION = ^D3D12_TEXTURE_COPY_LOCATION;
  D3D12_TEXTURE_COPY_LOCATION = record
    pResource: ID3D12Resource;
    Type_: D3D12_TEXTURE_COPY_TYPE;
    case Integer of
      0: (PlacedFootprint: D3D12_PLACED_SUBRESOURCE_FOOTPRINT);
      1: (SubresourceIndex: UINT);
  end;


  PD3D12_RESOLVE_MODE = ^D3D12_RESOLVE_MODE;
  D3D12_RESOLVE_MODE = (D3D12_RESOLVE_MODE_DECOMPRESS              = 0,
                        D3D12_RESOLVE_MODE_MIN                     = 1,
                        D3D12_RESOLVE_MODE_MAX                     = 2,
                        D3D12_RESOLVE_MODE_AVERAGE                 = 3,
                        D3D12_RESOLVE_MODE_ENCODE_SAMPLER_FEEDBACK = 4,
                        D3D12_RESOLVE_MODE_DECODE_SAMPLER_FEEDBACK = 5
                       );


  PD3D12_SAMPLE_POSITION = ^D3D12_SAMPLE_POSITION;
  D3D12_SAMPLE_POSITION = record
    X: INT8;
    Y: INT8;
  end;


  PD3D12_VIEW_INSTANCE_LOCATION = ^D3D12_VIEW_INSTANCE_LOCATION;
  D3D12_VIEW_INSTANCE_LOCATION = record
    ViewportArrayIndex: UINT;
    RenderTargetArrayIndex: UINT;
  end;


  PD3D12_VIEW_INSTANCING_DESC = ^D3D12_VIEW_INSTANCING_DESC;
  D3D12_VIEW_INSTANCING_DESC = record
    ViewInstanceCount: UINT;
    //[annotation("_Field_size_full_(ViewInstanceCount)")]
    pViewInstanceLocations: PD3D12_VIEW_INSTANCE_LOCATION;
    Flags: D3D12_VIEW_INSTANCING_FLAGS;
  end;


  // D3D12 view descriptions
  PD3D12_SHADER_COMPONENT_MAPPING = ^D3D12_SHADER_COMPONENT_MAPPING;
  D3D12_SHADER_COMPONENT_MAPPING = (D3D12_SHADER_COMPONENT_MAPPING_FROM_MEMORY_COMPONENT_0 = 0,
                                    D3D12_SHADER_COMPONENT_MAPPING_FROM_MEMORY_COMPONENT_1 = 1,
                                    D3D12_SHADER_COMPONENT_MAPPING_FROM_MEMORY_COMPONENT_2 = 2,
                                    D3D12_SHADER_COMPONENT_MAPPING_FROM_MEMORY_COMPONENT_3 = 3,
                                    D3D12_SHADER_COMPONENT_MAPPING_FORCE_VALUE_0           = 4,
                                    D3D12_SHADER_COMPONENT_MAPPING_FORCE_VALUE_1           = 5
                                   );


  PD3D12_BUFFER_SRV = ^D3D12_BUFFER_SRV;
  D3D12_BUFFER_SRV = record
    FirstElement: UINT64;
    NumElements: UINT;
    StructureByteStride: UINT;
    Flags: D3D12_BUFFER_SRV_FLAGS;
  end;


  PD3D12_TEX1D_SRV = ^D3D12_TEX1D_SRV;
  D3D12_TEX1D_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    ResourceMinLODClamp: Single;
  end;


  PD3D12_TEX1D_ARRAY_SRV = ^D3D12_TEX1D_ARRAY_SRV;
  D3D12_TEX1D_ARRAY_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    ResourceMinLODClamp: FLOAT;
  end;


  PD3D12_TEX2D_SRV = ^D3D12_TEX2D_SRV;
  D3D12_TEX2D_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    PlaneSlice: UINT;
    ResourceMinLODClamp: FLOAT;
  end;


  PD3D12_TEX2D_ARRAY_SRV = ^D3D12_TEX2D_ARRAY_SRV;
  D3D12_TEX2D_ARRAY_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    PlaneSlice: UINT;
    ResourceMinLODClamp: FLOAT;
  end;


  PD3D12_TEX3D_SRV = ^D3D12_TEX3D_SRV;
  D3D12_TEX3D_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    ResourceMinLODClamp: FLOAT;
  end;


  PD3D12_TEXCUBE_SRV = ^D3D12_TEXCUBE_SRV;
  D3D12_TEXCUBE_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    ResourceMinLODClamp: FLOAT;
  end;


  PD3D12_TEXCUBE_ARRAY_SRV = ^D3D12_TEXCUBE_ARRAY_SRV;
  D3D12_TEXCUBE_ARRAY_SRV = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    First2DArrayFace: UINT;
    NumCubes: UINT;
    ResourceMinLODClamp: FLOAT;
  end;


  PD3D12_TEX2DMS_SRV = ^D3D12_TEX2DMS_SRV;
  D3D12_TEX2DMS_SRV = record
    // don't need to define anything specific for this view dimension
    UnusedField_NothingToDefine: UINT;
  end;


  PD3D12_TEX2DMS_ARRAY_SRV = ^D3D12_TEX2DMS_ARRAY_SRV;
  D3D12_TEX2DMS_ARRAY_SRV = record
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_SRV = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_SRV;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_SRV = record
    Location: D3D12_GPU_VIRTUAL_ADDRESS;
  end;



  PD3D12_SRV_DIMENSION = ^D3D12_SRV_DIMENSION;
  D3D12_SRV_DIMENSION = (D3D12_SRV_DIMENSION_UNKNOWN          = 0,
                         D3D12_SRV_DIMENSION_BUFFER           = 1,
                         D3D12_SRV_DIMENSION_TEXTURE1D        = 2,
                         D3D12_SRV_DIMENSION_TEXTURE1DARRAY   = 3,
                         D3D12_SRV_DIMENSION_TEXTURE2D        = 4,
                         D3D12_SRV_DIMENSION_TEXTURE2DARRAY   = 5,
                         D3D12_SRV_DIMENSION_TEXTURE2DMS      = 6,
                         D3D12_SRV_DIMENSION_TEXTURE2DMSARRAY = 7,
                         D3D12_SRV_DIMENSION_TEXTURE3D        = 8,
                         D3D12_SRV_DIMENSION_TEXTURECUBE      = 9,
                         D3D12_SRV_DIMENSION_TEXTURECUBEARRAY = 10
                        );


  PD3D12_SHADER_RESOURCE_VIEW_DESC = ^D3D12_SHADER_RESOURCE_VIEW_DESC;
  D3D12_SHADER_RESOURCE_VIEW_DESC = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D12_SRV_DIMENSION;
    Shader4ComponentMapping: UINT;
    case Integer of
      0: (Buffer: D3D12_BUFFER_SRV);
      1: (Texture1D: D3D12_TEX1D_SRV);
      2: (Texture1DArray: D3D12_TEX1D_ARRAY_SRV);
      3: (Texture2D: D3D12_TEX2D_SRV);
      4: (Texture2DArray: D3D12_TEX2D_ARRAY_SRV);
      5: (Texture2DMS: D3D12_TEX2DMS_SRV);
      6: (Texture2DMSArray: D3D12_TEX2DMS_ARRAY_SRV);
      7: (Texture3D: D3D12_TEX3D_SRV);
      8: (TextureCube: D3D12_TEXCUBE_SRV);
      9: (TextureCubeArray: D3D12_TEXCUBE_ARRAY_SRV);
  end;


  PD3D12_CONSTANT_BUFFER_VIEW_DESC = ^D3D12_CONSTANT_BUFFER_VIEW_DESC;
  D3D12_CONSTANT_BUFFER_VIEW_DESC = record
    BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS;
    SizeInBytes: UINT;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //
  // Sampler
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D12_FILTER = ^D3D12_FILTER;
  // Bits used in defining enumeration of valid filters:
  // bits [1:0] - mip: 0 = point, 1 = linear, 2,3 unused
  // bits [3:2] - mag: 0 = point, 1 = linear, 2,3 unused
  // bits [5:4] - min: 0 = point, 1 = linear, 2,3 unused
  // bit  [6]   - aniso
  // bits [8:7] - reduction type:
  //                0 = standard filtering
  //                1 = comparison
  //                2 = min
  //                3 = max
  // bit  [31]  - mono 1-bit (narrow-purpose filter) [no longer supported in D3D12]
  D3D12_FILTER = (D3D12_FILTER_MIN_MAG_MIP_POINT                          = 0,
                  D3D12_FILTER_MIN_MAG_POINT_MIP_LINEAR                   = $1,
                  D3D12_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT             = $4,
                  D3D12_FILTER_MIN_POINT_MAG_MIP_LINEAR                   = $5,
                  D3D12_FILTER_MIN_LINEAR_MAG_MIP_POINT                   = $10,
                  D3D12_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR            = $11,
                  D3D12_FILTER_MIN_MAG_LINEAR_MIP_POINT                   = $14,
                  D3D12_FILTER_MIN_MAG_MIP_LINEAR                         = $15,
                  D3D12_FILTER_MIN_MAG_ANISOTROPIC_MIP_POINT              = $00000054,  // Update May 2024
                  D3D12_FILTER_ANISOTROPIC                                = $55,
                  D3D12_FILTER_COMPARISON_MIN_MAG_MIP_POINT               = $80,
                  D3D12_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR        = $81,
                  D3D12_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT  = $84,
                  D3D12_FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR        = $85,
                  D3D12_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT        = $90,
                  D3D12_FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR = $91,
                  D3D12_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT        = $94,
                  D3D12_FILTER_COMPARISON_MIN_MAG_MIP_LINEAR              = $95,
                  D3D12_FILTER_COMPARISON_MIN_MAG_ANISOTROPIC_MIP_POINT   = $000000d4,  // Update May 2024
                  D3D12_FILTER_COMPARISON_ANISOTROPIC                     = $d5,
                  D3D12_FILTER_MINIMUM_MIN_MAG_MIP_POINT                  = $100,
                  D3D12_FILTER_MINIMUM_MIN_MAG_POINT_MIP_LINEAR           = $101,
                  D3D12_FILTER_MINIMUM_MIN_POINT_MAG_LINEAR_MIP_POINT     = $104,
                  D3D12_FILTER_MINIMUM_MIN_POINT_MAG_MIP_LINEAR           = $105,
                  D3D12_FILTER_MINIMUM_MIN_LINEAR_MAG_MIP_POINT           = $110,
                  D3D12_FILTER_MINIMUM_MIN_LINEAR_MAG_POINT_MIP_LINEAR    = $111,
                  D3D12_FILTER_MINIMUM_MIN_MAG_LINEAR_MIP_POINT           = $114,
                  D3D12_FILTER_MINIMUM_MIN_MAG_MIP_LINEAR                 = $115,
                  D3D12_FILTER_MINIMUM_MIN_MAG_ANISOTROPIC_MIP_POINT      = $00000154,  // Update May 2024
                  D3D12_FILTER_MINIMUM_ANISOTROPIC                        = $155,
                  D3D12_FILTER_MAXIMUM_MIN_MAG_MIP_POINT                  = $180,
                  D3D12_FILTER_MAXIMUM_MIN_MAG_POINT_MIP_LINEAR           = $181,
                  D3D12_FILTER_MAXIMUM_MIN_POINT_MAG_LINEAR_MIP_POINT     = $184,
                  D3D12_FILTER_MAXIMUM_MIN_POINT_MAG_MIP_LINEAR           = $185,
                  D3D12_FILTER_MAXIMUM_MIN_LINEAR_MAG_MIP_POINT           = $190,
                  D3D12_FILTER_MAXIMUM_MIN_LINEAR_MAG_POINT_MIP_LINEAR    = $191,
                  D3D12_FILTER_MAXIMUM_MIN_MAG_LINEAR_MIP_POINT           = $194,
                  D3D12_FILTER_MAXIMUM_MIN_MAG_MIP_LINEAR                 = $195,
                  D3D12_FILTER_MAXIMUM_MIN_MAG_ANISOTROPIC_MIP_POINT      = $000001d4,  // Update May 2024
                  D3D12_FILTER_MAXIMUM_ANISOTROPIC                        = $1d5
                 );


  PD3D12_FILTER_TYPE = ^D3D12_FILTER_TYPE;
  D3D12_FILTER_TYPE = (D3D12_FILTER_TYPE_POINT  = 0,
                       D3D12_FILTER_TYPE_LINEAR = 1
                      );


  PD3D12_FILTER_REDUCTION_TYPE = ^D3D12_FILTER_REDUCTION_TYPE;
  D3D12_FILTER_REDUCTION_TYPE = (D3D12_FILTER_REDUCTION_TYPE_STANDARD   = 0,
                                 D3D12_FILTER_REDUCTION_TYPE_COMPARISON = 1,
                                 D3D12_FILTER_REDUCTION_TYPE_MINIMUM    = 2,
                                 D3D12_FILTER_REDUCTION_TYPE_MAXIMUM    = 3
                                );


  // D3D12_FILTER_REDUCTION_TYPE_MASK See: "Moved to here."

  // encode enum entry for most filters except anisotropic filtering
  // D3D12_ENCODE_BASIC_FILTER
  // See: Converted Macro's
  // encode enum entry for anisotropic filtering (with or without comparison filtering)
  // D3D12_ENCODE_ANISOTROPIC_FILTER
  // See: Converted Macro's
  // D3D12_ENCODE_MIN_MAG_ANISOTROPIC_MIP_POINT_FILTER // Update May 2024
  // See: Converted Macro's
  // D3D12_DECODE_MIN_FILTER
  // See: Converted Macro's
  // D3D12_DECODE_MAG_FILTER
  // See: Converted Macro's
  // D3D12_DECODE_MIP_FILTER
  // See: Converted Macro's
  // D3D12_DECODE_FILTER_REDUCTION
  // See: Converted Macro's
  // D3D12_DECODE_IS_COMPARISON_FILTER
  // See: Converted Macro's
  // D3D12_DECODE_IS_ANISOTROPIC_FILTER


  PD3D12_TEXTURE_ADDRESS_MODE = ^D3D12_TEXTURE_ADDRESS_MODE;
  D3D12_TEXTURE_ADDRESS_MODE = (D3D12_TEXTURE_ADDRESS_MODE_WRAP        = 1,
                                D3D12_TEXTURE_ADDRESS_MODE_MIRROR      = 2,
                                D3D12_TEXTURE_ADDRESS_MODE_CLAMP       = 3,
                                D3D12_TEXTURE_ADDRESS_MODE_BORDER      = 4,
                                D3D12_TEXTURE_ADDRESS_MODE_MIRROR_ONCE = 5
                               );


  PD3D12_SAMPLER_DESC = ^D3D12_SAMPLER_DESC;
  D3D12_SAMPLER_DESC = record
    Filter: D3D12_FILTER;
    AddressU: D3D12_TEXTURE_ADDRESS_MODE;
    AddressV: D3D12_TEXTURE_ADDRESS_MODE;
    AddressW: D3D12_TEXTURE_ADDRESS_MODE;
    MipLODBias: Single;
    MaxAnisotropy: UINT;
    ComparisonFunc: D3D12_COMPARISON_FUNC;
    BorderColor: Fixed3FLOATArray; // RGBA
    MinLOD: Single;
    MaxLOD: Single;
  end;

  // Update May 2024

  PD3D12_SAMPLER_FLAGS = ^D3D12_SAMPLER_FLAGS;
  D3D12_SAMPLER_FLAGS                             = (
    D3D12_SAMPLER_FLAG_NONE                       = $0,
    D3D12_SAMPLER_FLAG_UINT_BORDER_COLOR          = $01,
    D3D12_SAMPLER_FLAG_NON_NORMALIZED_COORDINATES = $02
  );

  // DEFINE_ENUM_FLAG_OPERATORS( D3D12_SAMPLER_FLAGS )

  D3D12_SAMPLER_DESC2_COLOR_UNION = record
    case Integer of
      0: (FloatBorderColor: array[0..3] of FLOAT);
      1: (UintBorderColor: array[0..3] of UINT);
  end;
  PD3D12_SAMPLER_DESC2 = ^D3D12_SAMPLER_DESC2;
  D3D12_SAMPLER_DESC2 = record
    Filter: D3D12_FILTER;
    AddressU: D3D12_TEXTURE_ADDRESS_MODE;
    AddressV: D3D12_TEXTURE_ADDRESS_MODE;
    AddressW: D3D12_TEXTURE_ADDRESS_MODE;
    MipLODBias: FLOAT;
    MaxAnisotropy: UINT;
    ComparisonFunc: D3D12_COMPARISON_FUNC;
    BorderColor: D3D12_SAMPLER_DESC2_COLOR_UNION;
    MinLOD: FLOAT;
    MaxLOD: FLOAT;
    Flags: D3D12_SAMPLER_FLAGS;
  end;

  // ==

  PD3D12_BUFFER_UAV = ^D3D12_BUFFER_UAV;
  D3D12_BUFFER_UAV = record
    FirstElement: UINT64;
    NumElements: UINT;
    StructureByteStride: UINT; // if nonzero, format must be DXGI_FORMAT_UNKNOWN
    CounterOffsetInBytes: UINT64;
    Flags: D3D12_BUFFER_UAV_FLAGS;
  end;


  PD3D12_TEX1D_UAV = ^D3D12_TEX1D_UAV;
  D3D12_TEX1D_UAV = record
    MipSlice: UINT;
  end;


  PD3D12_TEX1D_ARRAY_UAV = ^D3D12_TEX1D_ARRAY_UAV;
  D3D12_TEX1D_ARRAY_UAV = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_TEX2D_UAV = ^D3D12_TEX2D_UAV;
  D3D12_TEX2D_UAV = record
    MipSlice: UINT;
    PlaneSlice: UINT;
  end;


  PD3D12_TEX2D_ARRAY_UAV = ^D3D12_TEX2D_ARRAY_UAV;
  D3D12_TEX2D_ARRAY_UAV = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    PlaneSlice: UINT;
  end;

  // Update May 2024

  PD3D12_TEX2DMS_UAV = ^D3D12_TEX2DMS_UAV;
  D3D12_TEX2DMS_UAV = record
    // don't need to define anything specific for this view dimension
    UnusedField_NothingToDefine: UINT;
  end;


  PD3D12_TEX2DMS_ARRAY_UAV = ^D3D12_TEX2DMS_ARRAY_UAV;
  D3D12_TEX2DMS_ARRAY_UAV = record
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;

  // ==

  PD3D12_TEX3D_UAV = ^D3D12_TEX3D_UAV;
  D3D12_TEX3D_UAV = record
    MipSlice: UINT;
    FirstWSlice: UINT;
    WSize: UINT;
  end;


  PD3D12_UAV_DIMENSION = ^D3D12_UAV_DIMENSION;
  D3D12_UAV_DIMENSION = (D3D12_UAV_DIMENSION_UNKNOWN          = 0,
                         D3D12_UAV_DIMENSION_BUFFER           = 1,
                         D3D12_UAV_DIMENSION_TEXTURE1D        = 2,
                         D3D12_UAV_DIMENSION_TEXTURE1DARRAY   = 3,
                         D3D12_UAV_DIMENSION_TEXTURE2D        = 4,
                         D3D12_UAV_DIMENSION_TEXTURE2DARRAY   = 5,
                         // Update May 2024
                         D3D12_UAV_DIMENSION_TEXTURE2DMS      = 6,
                         D3D12_UAV_DIMENSION_TEXTURE2DMSARRAY = 7,
                         // ==
                         D3D12_UAV_DIMENSION_TEXTURE3D        = 8
                        );


  PD3D12_UNORDERED_ACCESS_VIEW_DESC = ^D3D12_UNORDERED_ACCESS_VIEW_DESC;
  D3D12_UNORDERED_ACCESS_VIEW_DESC = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D12_UAV_DIMENSION;
    case Integer of
      0: (Buffer: D3D12_BUFFER_UAV);
      1: (Texture1D: D3D12_TEX1D_UAV);
      2: (Texture1DArray: D3D12_TEX1D_ARRAY_UAV);
      3: (Texture2D: D3D12_TEX2D_UAV);
      4: (Texture2DArray: D3D12_TEX2D_ARRAY_UAV);
      // Update May 2024
      5: (Texture2DMS: D3D12_TEX2DMS_UAV);
      6: (Texture2DMSArray: D3D12_TEX2DMS_ARRAY_UAV);
      // ==
      7: (Texture3D: D3D12_TEX3D_UAV);
  end;


  PD3D12_BUFFER_RTV = ^D3D12_BUFFER_RTV;
  D3D12_BUFFER_RTV = record
    FirstElement: UINT64;
    NumElements: UINT;
  end;


  PD3D12_TEX1D_RTV = ^D3D12_TEX1D_RTV;
  D3D12_TEX1D_RTV = record
    MipSlice: UINT;
  end;


  PD3D12_TEX1D_ARRAY_RTV = ^D3D12_TEX1D_ARRAY_RTV;
  D3D12_TEX1D_ARRAY_RTV = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_TEX2D_RTV = ^D3D12_TEX2D_RTV;
  D3D12_TEX2D_RTV = record
    MipSlice: UINT;
    PlaneSlice: UINT;
  end;


  PD3D12_TEX2DMS_RTV = ^D3D12_TEX2DMS_RTV;
  D3D12_TEX2DMS_RTV = record
    UnusedField_NothingToDefine: UINT;
  end;


  PD3D12_TEX2D_ARRAY_RTV = ^D3D12_TEX2D_ARRAY_RTV;
  D3D12_TEX2D_ARRAY_RTV = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    PlaneSlice: UINT;
  end;


  PD3D12_TEX2DMS_ARRAY_RTV = ^D3D12_TEX2DMS_ARRAY_RTV;
  D3D12_TEX2DMS_ARRAY_RTV = record
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_TEX3D_RTV = ^D3D12_TEX3D_RTV;
  D3D12_TEX3D_RTV = record
    MipSlice: UINT;
    FirstWSlice: UINT;
    WSize: UINT;
  end;


  PD3D12_RTV_DIMENSION = ^D3D12_RTV_DIMENSION;
  D3D12_RTV_DIMENSION = (D3D12_RTV_DIMENSION_UNKNOWN          = 0,
                         D3D12_RTV_DIMENSION_BUFFER           = 1,
                         D3D12_RTV_DIMENSION_TEXTURE1D        = 2,
                         D3D12_RTV_DIMENSION_TEXTURE1DARRAY   = 3,
                         D3D12_RTV_DIMENSION_TEXTURE2D        = 4,
                         D3D12_RTV_DIMENSION_TEXTURE2DARRAY   = 5,
                         D3D12_RTV_DIMENSION_TEXTURE2DMS      = 6,
                         D3D12_RTV_DIMENSION_TEXTURE2DMSARRAY = 7,
                         D3D12_RTV_DIMENSION_TEXTURE3D        = 8
                        );


  PD3D12_RENDER_TARGET_VIEW_DESC = ^D3D12_RENDER_TARGET_VIEW_DESC;
  D3D12_RENDER_TARGET_VIEW_DESC = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D12_RTV_DIMENSION;
    case Integer of
      0: (Buffer: D3D12_BUFFER_RTV);
      1: (Texture1D: D3D12_TEX1D_RTV);
      2: (Texture1DArray: D3D12_TEX1D_ARRAY_RTV);
      3: (Texture2D: D3D12_TEX2D_RTV);
      4: (Texture2DArray: D3D12_TEX2D_ARRAY_RTV);
      5: (Texture2DMS: D3D12_TEX2DMS_RTV);
      6: (Texture2DMSArray: D3D12_TEX2DMS_ARRAY_RTV);
      7: (Texture3D: D3D12_TEX3D_RTV);
  end;


  PD3D12_TEX1D_DSV = ^D3D12_TEX1D_DSV;
  D3D12_TEX1D_DSV = record
    MipSlice: UINT;
  end;


  PD3D12_TEX1D_ARRAY_DSV = ^D3D12_TEX1D_ARRAY_DSV;
  D3D12_TEX1D_ARRAY_DSV = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_TEX2D_DSV = ^D3D12_TEX2D_DSV;
  D3D12_TEX2D_DSV = record
    MipSlice: UINT;
  end;


  PD3D12_TEX2D_ARRAY_DSV = ^D3D12_TEX2D_ARRAY_DSV;
  D3D12_TEX2D_ARRAY_DSV = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_TEX2DMS_DSV = ^D3D12_TEX2DMS_DSV;
  D3D12_TEX2DMS_DSV = record
    UnusedField_NothingToDefine: UINT;
  end;


  PD3D12_TEX2DMS_ARRAY_DSV = ^D3D12_TEX2DMS_ARRAY_DSV;
  D3D12_TEX2DMS_ARRAY_DSV = record
    FirstArraySlice: UINT;
    ArraySize: UINT;
  end;


  PD3D12_DSV_DIMENSION = ^D3D12_DSV_DIMENSION;
  D3D12_DSV_DIMENSION = (D3D12_DSV_DIMENSION_UNKNOWN          = 0,
                         D3D12_DSV_DIMENSION_TEXTURE1D        = 1,
                         D3D12_DSV_DIMENSION_TEXTURE1DARRAY   = 2,
                         D3D12_DSV_DIMENSION_TEXTURE2D        = 3,
                         D3D12_DSV_DIMENSION_TEXTURE2DARRAY   = 4,
                         D3D12_DSV_DIMENSION_TEXTURE2DMS      = 5,
                         D3D12_DSV_DIMENSION_TEXTURE2DMSARRAY = 6
                        );


  PD3D12_DEPTH_STENCIL_VIEW_DESC = ^D3D12_DEPTH_STENCIL_VIEW_DESC;
  {$ALIGN 4}
  D3D12_DEPTH_STENCIL_VIEW_DESC = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D12_DSV_DIMENSION;
    Flags: D3D12_DSV_FLAGS;
    case Integer of
      0: (Texture1D: D3D12_TEX1D_DSV);
      1: (Texture1DArray: D3D12_TEX1D_ARRAY_DSV);
      2: (Texture2D: D3D12_TEX2D_DSV);
      3: (Texture2DArray: D3D12_TEX2D_ARRAY_DSV);
      4: (Texture2DMS: D3D12_TEX2DMS_DSV);
      5: (Texture2DMSArray: D3D12_TEX2DMS_ARRAY_DSV);
  end;
  {$ALIGN 8}


  PD3D12_DESCRIPTOR_HEAP_TYPE = ^D3D12_DESCRIPTOR_HEAP_TYPE;
  D3D12_DESCRIPTOR_HEAP_TYPE = (D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
                                D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER,
                                D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
                                D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
                                D3D12_DESCRIPTOR_HEAP_TYPE_NUM_TYPES
                               );


  PD3D12_DESCRIPTOR_HEAP_DESC = ^D3D12_DESCRIPTOR_HEAP_DESC;
  D3D12_DESCRIPTOR_HEAP_DESC = record
    Type_: D3D12_DESCRIPTOR_HEAP_TYPE;
    NumDescriptors: UINT;
    Flags: D3D12_DESCRIPTOR_HEAP_FLAGS;
    NodeMask: UINT;
  end;


  PD3D12_DESCRIPTOR_RANGE_TYPE = ^D3D12_DESCRIPTOR_RANGE_TYPE;
  D3D12_DESCRIPTOR_RANGE_TYPE = (D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
                                 D3D12_DESCRIPTOR_RANGE_TYPE_UAV,
                                 D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
                                 D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER
                                );


  PD3D12_DESCRIPTOR_RANGE = ^D3D12_DESCRIPTOR_RANGE;
  D3D12_DESCRIPTOR_RANGE = record
    RangeType: D3D12_DESCRIPTOR_RANGE_TYPE;
    NumDescriptors: UINT;
    BaseShaderRegister: UINT;
    RegisterSpace: UINT;
    OffsetInDescriptorsFromTableStart: UINT;
  end;


  PD3D12_ROOT_DESCRIPTOR_TABLE = ^D3D12_ROOT_DESCRIPTOR_TABLE;
  D3D12_ROOT_DESCRIPTOR_TABLE = record
    NumDescriptorRanges: UINT;
    pDescriptorRanges: PD3D12_DESCRIPTOR_RANGE;
  end;


  PD3D12_ROOT_CONSTANTS = ^D3D12_ROOT_CONSTANTS;
  D3D12_ROOT_CONSTANTS = record
    ShaderRegister: UINT;
    RegisterSpace: UINT;
    Num32BitValues: UINT;
  end;


 PD3D12_ROOT_DESCRIPTOR = ^D3D12_ROOT_DESCRIPTOR;
  D3D12_ROOT_DESCRIPTOR = record
    ShaderRegister: UINT;
    RegisterSpace: UINT;
  end;


  PD3D12_SHADER_VISIBILITY = ^D3D12_SHADER_VISIBILITY;
  D3D12_SHADER_VISIBILITY = (D3D12_SHADER_VISIBILITY_ALL      = 0,
                             D3D12_SHADER_VISIBILITY_VERTEX   = 1,
                             D3D12_SHADER_VISIBILITY_HULL     = 2,
                             D3D12_SHADER_VISIBILITY_DOMAIN   = 3,
                             D3D12_SHADER_VISIBILITY_GEOMETRY = 4,
                             D3D12_SHADER_VISIBILITY_PIXEL    = 5
                            );


  PD3D12_ROOT_PARAMETER_TYPE = ^D3D12_ROOT_PARAMETER_TYPE;
  D3D12_ROOT_PARAMETER_TYPE = (D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
                               D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS,
                               D3D12_ROOT_PARAMETER_TYPE_CBV,
                               D3D12_ROOT_PARAMETER_TYPE_SRV,
                               D3D12_ROOT_PARAMETER_TYPE_UAV
                              );


  // Union helper for record D3D12_ROOT_PARAMETER
  D3D12_ROOT_PARAMETER_UNION = record
    case Integer of
      0: (DescriptorTable: D3D12_ROOT_DESCRIPTOR_TABLE);
      1: (Constants: D3D12_ROOT_CONSTANTS);
      2: (Descriptor: D3D12_ROOT_DESCRIPTOR);
  end;

  PD3D12_ROOT_PARAMETER = ^D3D12_ROOT_PARAMETER;
  D3D12_ROOT_PARAMETER = record
    ParameterType: D3D12_ROOT_PARAMETER_TYPE;
    UnionHelper: D3D12_ROOT_PARAMETER_UNION; // See D3D12_ROOT_PARAMETER_UNION
    ShaderVisibility: D3D12_SHADER_VISIBILITY;
  end;


  PD3D12_STATIC_BORDER_COLOR = ^D3D12_STATIC_BORDER_COLOR;
  D3D12_STATIC_BORDER_COLOR = (D3D12_STATIC_BORDER_COLOR_TRANSPARENT_BLACK = 0, // 0.0, 0.0, 0.0, 0.0
                               D3D12_STATIC_BORDER_COLOR_OPAQUE_BLACK      = 1, // 0.0, 0.0, 0.0, 1.0
                               D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE      = 2, // 1.0, 1.0, 1.0, 1.0
                               // Update May 2024
                               D3D12_STATIC_BORDER_COLOR_OPAQUE_BLACK_UINT = 3, // 0, 0, 0, 1
                               D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE_UINT = 4  // 1, 1, 1, 1
                               // ==
                              );


  PD3D12_STATIC_SAMPLER_DESC = ^D3D12_STATIC_SAMPLER_DESC;
  D3D12_STATIC_SAMPLER_DESC = record
    Filter: D3D12_FILTER;
    AddressU: D3D12_TEXTURE_ADDRESS_MODE;
    AddressV: D3D12_TEXTURE_ADDRESS_MODE;
    AddressW: D3D12_TEXTURE_ADDRESS_MODE;
    MipLODBias: FLOAT;
    MaxAnisotropy: UINT;
    ComparisonFunc: D3D12_COMPARISON_FUNC;
    BorderColor: D3D12_STATIC_BORDER_COLOR;
    MinLOD: FLOAT;
    MaxLOD: FLOAT;
    ShaderRegister: UINT;
    RegisterSpace: UINT;
    ShaderVisibility: D3D12_SHADER_VISIBILITY;
  end;

  // Update May 2024

  PD3D12_STATIC_SAMPLER_DESC1 = ^D3D12_STATIC_SAMPLER_DESC1;
  D3D12_STATIC_SAMPLER_DESC1 = record
    Filter: D3D12_FILTER;
    AddressU: D3D12_TEXTURE_ADDRESS_MODE;
    AddressV: D3D12_TEXTURE_ADDRESS_MODE;
    AddressW: D3D12_TEXTURE_ADDRESS_MODE;
    MipLODBias: FLOAT;
    MaxAnisotropy: UINT;
    ComparisonFunc: D3D12_COMPARISON_FUNC;
    BorderColor: D3D12_STATIC_BORDER_COLOR;
    MinLOD: FLOAT;
    MaxLOD: FLOAT;
    ShaderRegister: UINT;
    RegisterSpace: UINT;
    ShaderVisibility: D3D12_SHADER_VISIBILITY;
    Flags: D3D12_SAMPLER_FLAGS;
  end;

  // ==

  PD3D12_ROOT_SIGNATURE_DESC = ^D3D12_ROOT_SIGNATURE_DESC;
  D3D12_ROOT_SIGNATURE_DESC = record
    NumParameters: UINT;
    pParameters: PD3D12_ROOT_PARAMETER;
    NumStaticSamplers: UINT;
    pStaticSamplers: PD3D12_STATIC_SAMPLER_DESC;
    Flags: D3D12_ROOT_SIGNATURE_FLAGS;
  end;


  PD3D12_DESCRIPTOR_RANGE1 = ^D3D12_DESCRIPTOR_RANGE1;
  D3D12_DESCRIPTOR_RANGE1 = record
    RangeType: D3D12_DESCRIPTOR_RANGE_TYPE;
    NumDescriptors: UINT;
    BaseShaderRegister: UINT;
    RegisterSpace: UINT;
    Flags: D3D12_DESCRIPTOR_RANGE_FLAGS;
    OffsetInDescriptorsFromTableStart: UINT;
  end;


  PD3D12_ROOT_DESCRIPTOR_TABLE1 = ^D3D12_ROOT_DESCRIPTOR_TABLE1;
  D3D12_ROOT_DESCRIPTOR_TABLE1 = record
    NumDescriptorRanges: UINT;
    pDescriptorRanges: PD3D12_DESCRIPTOR_RANGE1;
  end;


  PD3D12_ROOT_DESCRIPTOR1 = ^D3D12_ROOT_DESCRIPTOR1;
  D3D12_ROOT_DESCRIPTOR1 = record
    ShaderRegister: UINT;
    RegisterSpace: UINT;
    Flags: D3D12_ROOT_DESCRIPTOR_FLAGS;
  end;


  D3D12_ROOT_PARAMETER1_UNION = record
    case Integer of
      0: (DescriptorTable: D3D12_ROOT_DESCRIPTOR_TABLE1);
      1: (Constants: D3D12_ROOT_CONSTANTS);
      2: (Descriptor: D3D12_ROOT_DESCRIPTOR1);
  end;
  PD3D12_ROOT_PARAMETER1 = ^D3D12_ROOT_PARAMETER1;
  D3D12_ROOT_PARAMETER1 = record
    ParameterType: D3D12_ROOT_PARAMETER_TYPE;
    UnionHelper: D3D12_ROOT_PARAMETER1_UNION;
    ShaderVisibility: D3D12_SHADER_VISIBILITY;
  end;


  PD3D12_ROOT_SIGNATURE_DESC1 = ^D3D12_ROOT_SIGNATURE_DESC1;
  D3D12_ROOT_SIGNATURE_DESC1 = record
    NumParameters: UINT;
    pParameters: PD3D12_ROOT_PARAMETER1;
    NumStaticSamplers: UINT;
    pStaticSamplers: PD3D12_STATIC_SAMPLER_DESC;
    Flags: D3D12_ROOT_SIGNATURE_FLAGS;
  end;

  // Update May 2024
  PD3D12_ROOT_SIGNATURE_DESC2 = ^D3D12_ROOT_SIGNATURE_DESC2;
  D3D12_ROOT_SIGNATURE_DESC2 = record
    NumParameters: UINT;
    pParameters: PD3D12_ROOT_PARAMETER1;
    NumStaticSamplers: UINT;
    pStaticSamplers: PD3D12_STATIC_SAMPLER_DESC1;
    Flags: D3D12_ROOT_SIGNATURE_FLAGS;
  end;
  // ==


  PD3D12_VERSIONED_ROOT_SIGNATURE_DESC = ^D3D12_VERSIONED_ROOT_SIGNATURE_DESC;
  D3D12_VERSIONED_ROOT_SIGNATURE_DESC = record
    Version: D3D_ROOT_SIGNATURE_VERSION;
    case Integer of
      0: (Desc_1_0: D3D12_ROOT_SIGNATURE_DESC);
      1: (Desc_1_1: D3D12_ROOT_SIGNATURE_DESC1);
      2: (Desc_1_2: D3D12_ROOT_SIGNATURE_DESC2);
    end;


  // Interface ID3D12RootSignatureDeserializer
  // =========================================
  //
  ID3D12RootSignatureDeserializer = interface(IUnknown)
    ['{34AB647B-3CC8-46AC-841B-C0965645C046}']

     function GetRootSignatureDesc: PD3D12_ROOT_SIGNATURE_DESC; stdcall;
  end;
  IID_ID3D12RootSignatureDeserializer = ID3D12RootSignatureDeserializer;


  // Interface ID3D12VersionedRootSignatureDeserializer
  // ===================================================
  //
  ID3D12VersionedRootSignatureDeserializer = interface(IUnknown)
    ['{7F91CE67-090C-4BB7-B78E-ED8FF2E31DA0}']

    function GetRootSignatureDescAtVersion(convertToVersion: D3D_ROOT_SIGNATURE_VERSION;
                                           ppDesc: PPointer): HRESULT; stdcall;

    function GetUnconvertedRootSignatureDesc(): PD3D12_VERSIONED_ROOT_SIGNATURE_DESC; stdcall;
  end;
  IID_ID3D12VersionedRootSignatureDeserializer = ID3D12VersionedRootSignatureDeserializer;


  PD3D12_CPU_DESCRIPTOR_HANDLE = ^D3D12_CPU_DESCRIPTOR_HANDLE;
  D3D12_CPU_DESCRIPTOR_HANDLE = record
    ptr: SIZE_T;
  end;


  PD3D12_GPU_DESCRIPTOR_HANDLE = ^D3D12_GPU_DESCRIPTOR_HANDLE;
  D3D12_GPU_DESCRIPTOR_HANDLE = record
    ptr: UINT64;
  end;


  // If rects are supplied in D3D12_DISCARD_REGION, below, the resource
  // must have 2D subresources with all specified subresources the same dimension.
  PD3D12_DISCARD_REGION = ^D3D12_DISCARD_REGION;
  D3D12_DISCARD_REGION = record
    NumRects: UINT;
    pRects: PD3D12_RECT;  // array of D3D12Rect
    FirstSubresource: UINT;
    NumSubresources: UINT;
  end;


  PD3D12_QUERY_HEAP_TYPE = ^D3D12_QUERY_HEAP_TYPE;
  D3D12_QUERY_HEAP_TYPE = (D3D12_QUERY_HEAP_TYPE_OCCLUSION               = 0,
                           D3D12_QUERY_HEAP_TYPE_TIMESTAMP               = 1,
                           D3D12_QUERY_HEAP_TYPE_PIPELINE_STATISTICS     = 2,
                           D3D12_QUERY_HEAP_TYPE_SO_STATISTICS           = 3,
                           D3D12_QUERY_HEAP_TYPE_VIDEO_DECODE_STATISTICS = 4,
                           D3D12_QUERY_HEAP_TYPE_COPY_QUEUE_TIMESTAMP    = 5,
                           D3D12_QUERY_HEAP_TYPE_PIPELINE_STATISTICS1    = 7
                          );


  PD3D12_QUERY_HEAP_DESC = ^D3D12_QUERY_HEAP_DESC;
  D3D12_QUERY_HEAP_DESC = record
    Type_: D3D12_QUERY_HEAP_TYPE;
    Count: UINT;
    NodeMask: UINT;
  end;


  PD3D12_QUERY_TYPE = ^D3D12_QUERY_TYPE;
  D3D12_QUERY_TYPE = (D3D12_QUERY_TYPE_OCCLUSION               = 0,
                      D3D12_QUERY_TYPE_BINARY_OCCLUSION        = 1,
                      D3D12_QUERY_TYPE_TIMESTAMP               = 2,
                      D3D12_QUERY_TYPE_PIPELINE_STATISTICS     = 3,
                      D3D12_QUERY_TYPE_SO_STATISTICS_STREAM0   = 4,
                      D3D12_QUERY_TYPE_SO_STATISTICS_STREAM1   = 5,
                      D3D12_QUERY_TYPE_SO_STATISTICS_STREAM2   = 6,
                      D3D12_QUERY_TYPE_SO_STATISTICS_STREAM3   = 7,
                      D3D12_QUERY_TYPE_VIDEO_DECODE_STATISTICS = 8,
                      D3D12_QUERY_TYPE_PIPELINE_STATISTICS1    = 10
                     );


  PD3D12_PREDICATION_OP = ^D3D12_PREDICATION_OP;
  D3D12_PREDICATION_OP = (D3D12_PREDICATION_OP_EQUAL_ZERO     = 0,
                          D3D12_PREDICATION_OP_NOT_EQUAL_ZERO = 1
                         );


  PD3D12_QUERY_DATA_PIPELINE_STATISTICS = ^D3D12_QUERY_DATA_PIPELINE_STATISTICS;
  D3D12_QUERY_DATA_PIPELINE_STATISTICS = record
    IAVertices: UINT64;
    IAPrimitives: UINT64;
    VSInvocations: UINT64;
    GSInvocations: UINT64;
    GSPrimitives: UINT64;
    CInvocations: UINT64;
    CPrimitives: UINT64;
    PSInvocations: UINT64;
    HSInvocations: UINT64;
    DSInvocations: UINT64;
    CSInvocations: UINT64;
  end;


  PD3D12_QUERY_DATA_PIPELINE_STATISTICS1 = ^D3D12_QUERY_DATA_PIPELINE_STATISTICS1;
  D3D12_QUERY_DATA_PIPELINE_STATISTICS1 = record
    IAVertices: UINT64;
    IAPrimitives: UINT64;
    VSInvocations: UINT64;
    GSInvocations: UINT64;
    GSPrimitives: UINT64;
    CInvocations: UINT64;
    CPrimitives: UINT64;
    PSInvocations: UINT64;
    HSInvocations: UINT64;
    DSInvocations: UINT64;
    CSInvocations: UINT64;
    ASInvocations: UINT64;
    MSInvocations: UINT64;
    MSPrimitives: UINT64;
  end;


  PD3D12_QUERY_DATA_SO_STATISTICS = ^D3D12_QUERY_DATA_SO_STATISTICS;
  D3D12_QUERY_DATA_SO_STATISTICS = record
    NumPrimitivesWritten: UINT64;
    PrimitivesStorageNeeded: UINT64;
  end;


  PD3D12_STREAM_OUTPUT_BUFFER_VIEW = ^D3D12_STREAM_OUTPUT_BUFFER_VIEW;
  D3D12_STREAM_OUTPUT_BUFFER_VIEW = record
    BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS;
    SizeInBytes: UINT64;
    BufferFilledSizeLocation: D3D12_GPU_VIRTUAL_ADDRESS;
  end;


  PD3D12_DRAW_ARGUMENTS = ^D3D12_DRAW_ARGUMENTS;
  D3D12_DRAW_ARGUMENTS = record
    VertexCountPerInstance: UINT;
    InstanceCount: UINT;
    StartVertexLocation: UINT;
    StartInstanceLocation: UINT;
  end;


  PD3D12_DRAW_INDEXED_ARGUMENTS = ^D3D12_DRAW_INDEXED_ARGUMENTS;
  D3D12_DRAW_INDEXED_ARGUMENTS = record
    IndexCountPerInstance: UINT;
    InstanceCount: UINT;
    StartIndexLocation: UINT;
    BaseVertexLocation: INT32;
    StartInstanceLocation: UINT;
  end;


  PD3D12_DISPATCH_ARGUMENTS = ^D3D12_DISPATCH_ARGUMENTS;
  D3D12_DISPATCH_ARGUMENTS = record
    ThreadGroupCountX: UINT;
    ThreadGroupCountY: UINT;
    ThreadGroupCountZ: UINT;
  end;


  PD3D12_VERTEX_BUFFER_VIEW = ^D3D12_VERTEX_BUFFER_VIEW;
  D3D12_VERTEX_BUFFER_VIEW = record
    BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS;
    SizeInBytes: UINT;
    StrideInBytes: UINT;
  end;


  PD3D12_INDEX_BUFFER_VIEW = ^D3D12_INDEX_BUFFER_VIEW;
  D3D12_INDEX_BUFFER_VIEW = record
    BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS;
    SizeInBytes: UINT;
    Format: DXGI_FORMAT;
  end;


  PD3D12_INDIRECT_ARGUMENT_TYPE = ^D3D12_INDIRECT_ARGUMENT_TYPE;
  D3D12_INDIRECT_ARGUMENT_TYPE = (D3D12_INDIRECT_ARGUMENT_TYPE_DRAW,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_DRAW_INDEXED,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_DISPATCH,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_VERTEX_BUFFER_VIEW,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_INDEX_BUFFER_VIEW,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_CONSTANT,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_CONSTANT_BUFFER_VIEW,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_SHADER_RESOURCE_VIEW,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_UNORDERED_ACCESS_VIEW,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_DISPATCH_RAYS,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_DISPATCH_MESH,
                                  D3D12_INDIRECT_ARGUMENT_TYPE_INCREMENTING_CONSTANT  // Update May 2024
                                 );


// typedef struct D3D12_INDIRECT_ARGUMENT_DESC
// {
//     D3D12_INDIRECT_ARGUMENT_TYPE Type;
//
//     union
//     {
//         struct
//         {
//             UINT Slot;
//         } VertexBuffer;
//
//         struct
//         {
//             UINT RootParameterIndex;
//             UINT DestOffsetIn32BitValues;
//             UINT Num32BitValuesToSet;
//         } Constant;
//
//         struct
//         {
//             UINT RootParameterIndex;
//         } ConstantBufferView;
//
//         struct
//         {
//             UINT RootParameterIndex;
//         } ShaderResourceView;
//
//         struct
//         {
//             UINT RootParameterIndex;
//         } UnorderedAccessView;
//     };
// } D3D12_INDIRECT_ARGUMENT_DESC;


// Delphi solution to define this record (struct):

  // First we define the records (structs)
  VertexBuffer = record
    Slot: UINT ;
  end;

  Constant = record
    RootParameterIndex: UINT;
    DestOffsetIn32BitValues: UINT;
    Num32BitValuesToSet: UINT;
  end;

  ConstantBufferView = record
    RootParameterIndex: UINT;
  end;

  ShaderResourceView = record
    RootParameterIndex: UINT;
  end;

  UnorderedAccessView = record
    RootParameterIndex: UINT;
  end;

  // Update May 2024
  // Tier 1.1 support
  IncrementingConstant = record
    RootParameterIndex: UINT;
    DestOffsetIn32BitValues: UINT;
   end;
  // ==

  // Finally we create the whole variant record
  PD3D12_INDIRECT_ARGUMENT_DESC = ^D3D12_INDIRECT_ARGUMENT_DESC;
  {$ALIGN 4}
  D3D12_INDIRECT_ARGUMENT_DESC = record
    Type_: D3D12_INDIRECT_ARGUMENT_TYPE;
    case Integer of
      0: (VertexBuffer: VertexBuffer);
      1: (Constant: Constant);
      2: (ConstantBufferView: ConstantBufferView);
      3: (ShaderResourceView: ShaderResourceView);
      4: (UnorderedAccessView: UnorderedAccessView);
      5: (IncrementingConstant: IncrementingConstant); // Update May 2024
  end;
  {$ALIGN 8}


  PD3D12_COMMAND_SIGNATURE_DESC = ^D3D12_COMMAND_SIGNATURE_DESC;
  D3D12_COMMAND_SIGNATURE_DESC = record
    ByteStride: UINT;
    NumArgumentDescs: UINT;
    pArgumentDescs: PD3D12_INDIRECT_ARGUMENT_DESC;
    NodeMask: UINT;
  end;


{$REGION 'This region has been moved up.'}

  // Interface ID3D12Object
  // ======================
  //
//  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12Object);'}
//  {$EXTERNALSYM ID3D12Object}
//  ID3D12Object = interface(IUnknown)
//    ['{C4FEC28F-7966-4E95-9F94-F431CB56C3B8}']

//    function GetPrivateData(const guid: TGUID;
//                            var pDataSize: UINT;
//                            pData: pointer): HRESULT; stdcall;

//    function SetPrivateData(const guid: TGUID;
//                            DataSize: UINT;
//                            pData: Pointer): HRESULT; stdcall;

//    function SetPrivateDataInterface(const guid: TGUID;
//                                     pdata: IUnknown): HRESULT; stdcall;

//    function SetName(Name: LPCWSTR): HRESULT; stdcall;

//  end;
//  IID_ID3D12Object = ID3D12Object;
//  {$EXTERNALSYM IID_ID3D12Object}


  // Interface ID3D12DeviceChild
  // ============================
  //
//  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12DeviceChild);'}
//  {$EXTERNALSYM ID3D12DeviceChild}
//  ID3D12DeviceChild = Interface(ID3D12Object)
//    ['{905db94b-a00c-4140-9df5-2b64ca9ea357}']

//    function GetDevice(const riid: REFIID;
//                       out ppvDevice): HRESULT; stdcall;

//  end;
//  IID_ID3D12DeviceChild = ID3D12DeviceChild;
//  {$EXTERNALSYM IID_ID3D12DeviceChild}

{$ENDREGION}


  // Interface ID3D12Pageable
  // ========================
  //
  PID3D12Pageable = ^ID3D12Pageable;
  ID3D12Pageable = interface(ID3D12DeviceChild)
    ['{63EE58FB-1268-4835-86DA-F008CE62F0D6}']
  end;
  IID_ID3D12Pageable = ID3D12Pageable;


  // Interface ID3D12Heap
  // ====================
  //
  ID3D12Heap = interface(ID3D12Pageable)
    ['{6B3B2502-6E51-45B3-90EE-9884265E8DF3}']

    procedure GetDesc(out RetVal: D3D12_HEAP_DESC); stdcall;
  end;
  IID_ID3D12Heap = ID3D12Heap;


  // Interface ID3D12Resource
  // ========================
  //
  PID3D12Resource = ^ID3D12Resource;
  ID3D12Resource = interface(ID3D12Pageable)
    ['{696442BE-A72E-4059-BC79-5B5C98040FAD}']

    function Map(Subresource: UINT;
                 pReadRange: PD3D12_RANGE;
                 ppData: PPointer): HRESULT; stdcall;

    procedure Unmap(Subresource: UINT;
                    pWrittenRange: PD3D12_RANGE); stdcall;

    procedure GetDesc(out RetVal: D3D12_RESOURCE_DESC); stdcall;

    function GetGPUVirtualAddress: D3D12_GPU_VIRTUAL_ADDRESS; stdcall;

    function WriteToSubresource(DstSubresource: UINT;
                                pDstBox: PD3D12_BOX;
                                pSrcData: Pointer;
                                SrcRowPitch: UINT;
                                SrcDepthPitch: UINT): HRESULT; stdcall;

    function ReadFromSubresource(pDstData: Pointer;
                                 DstRowPitch: UINT;
                                 DstDepthPitch: UINT;
                                 SrcSubresource: UINT;
                                 pSrcBox: PD3D12_BOX): HRESULT; stdcall;

    function GetHeapProperties(pHeapProperties: PD3D12_HEAP_PROPERTIES;
                               pHeapFlags: PD3D12_HEAP_FLAGS): HRESULT; stdcall;
  end;
  IID_ID3D12Resource = ID3D12Resource;


  // Interface ID3D12CommandAllocator
  // ================================
  //
  ID3D12CommandAllocator = interface(ID3D12Pageable)
    ['{6102DEE4-AF59-4B09-B999-B44D73F09B24}']

    function Reset(): HRESULT; stdcall;
  end;
  IID_ID3D12CommandAllocator = ID3D12CommandAllocator;


  // Interface ID3D12Fence
  //
  PID3D12Fence = ^ID3D12Fence;
  ID3D12Fence = interface(ID3D12Pageable)
    ['{0A753DCF-C4D8-4B91-ADF6-BE5A60D95A76}']

    function GetCompletedValue(): UINT64; stdcall;

    function SetEventOnCompletion(Value: UINT64;
                                  hEvent: THandle): HRESULT; stdcall;

    function Signal(Value: UINT64): HRESULT; stdcall;
  end;
  IID_ID3D12Fence = ID3D12Fence;


  // Interface ID3D12Fence1
  // =======================
  //
  ID3D12Fence1 = interface(ID3D12Fence)
    ['{433685FE-E22B-4CA0-A8DB-B5B4F4DD0E4A}']

    function GetCreationFlags(): D3D12_FENCE_FLAGS; stdcall;
  end;
  IID_ID3D12Fence1 = ID3D12Fence1;


  // Interface ID3D12PipelineState
  // =============================
  //
  ID3D12PipelineState = interface(ID3D12Pageable)
    ['{765A30F3-F624-4C6F-A828-ACE948622445}']

    function GetCachedBlob(ppBlob: PID3DBlob): HRESULT; stdcall;
  end;
  IID_ID3D12PipelineState = ID3D12PipelineState;

  { 10.0.28000.0  }
  ID3D12PipelineState1 = interface(ID3D12PipelineState)
    ['{5646804C-9638-48F7-9182-B3EE5A6B60FB}']
    function GetRootSignature(const riid: TGUID;
                              ppvRootSignature: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12PipelineState1 = ID3D12PipelineState1;


  // Interface ID3D12DescriptorHeap
  // ==============================
  //
  PID3D12DescriptorHeap = ^ID3D12DescriptorHeap;
  ID3D12DescriptorHeap = interface(ID3D12Pageable)
    ['{8EFB471D-616C-4F49-90F7-127BB763FA51}']

    procedure GetDesc(out RetVal: D3D12_DESCRIPTOR_HEAP_DESC); stdcall;

    procedure GetCPUDescriptorHandleForHeapStart(out RetVal: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure GetGPUDescriptorHandleForHeapStart(out RetVal: D3D12_GPU_DESCRIPTOR_HANDLE); stdcall;
  end;
  IID_ID3D12DescriptorHeap = ID3D12DescriptorHeap;


  // Interface ID3D12QueryHeap
  // =========================
  //
  ID3D12QueryHeap = interface(ID3D12Pageable)
    ['{0D9658AE-ED45-469E-A61D-970EC583CAB4}']
  end;
  IID_ID3D12QueryHeap = ID3D12QueryHeap;


  // Interface ID3D12CommandSignature
  // ================================
  ID3D12CommandSignature = interface(ID3D12Pageable)
    ['{C36A797C-EC80-4F0A-8985-A7B2475082D1}']
  end;
  IID_ID3D12CommandSignature = ID3D12CommandSignature;


  // Interface ID3D12CommandList
  // ===========================
  //
  PID3D12CommandList = ^ID3D12CommandList;
  ID3D12CommandList = interface(ID3D12DeviceChild)
    ['{7116D91C-E7E4-47CE-B8C6-EC8168F437E5}']

    function GetType: D3D12_COMMAND_LIST_TYPE; stdcall;
  end;
  IID_ID3D12CommandList = ID3D12CommandList;


  // Interface ID3D12GraphicsCommandList
  // ===================================
  // Superseded by ID3D12GraphicsCommandList1
  //
  ID3D12GraphicsCommandList = interface(ID3D12CommandList)
    ['{5B160D0F-AC1B-4185-8BA8-B3AE42A5A455}']

    function Close(): HRESULT; stdcall;

    function Reset(pAllocator: ID3D12CommandAllocator;
                   pInitialState: ID3D12PipelineState): HRESULT; stdcall;

    procedure ClearState(pPipelineState: ID3D12PipelineState); stdcall;

    procedure DrawInstanced(VertexCountPerInstance: UINT;
                            InstanceCount: UINT;
                            StartVertexLocation: UINT;
                            StartInstanceLocation: UINT); stdcall;

    procedure DrawIndexedInstanced(IndexCountPerInstance: UINT;
                                   InstanceCount: UINT;
                                   StartIndexLocation: UINT;
                                   BaseVertexLocation: INT;
                                   StartInstanceLocation: UINT); stdcall;

    procedure Dispatch(ThreadGroupCountX: UINT;
                       ThreadGroupCountY: UINT;
                       ThreadGroupCountZ: UINT); stdcall;

    procedure CopyBufferRegion(pDstBuffer: ID3D12Resource;
                               DstOffset: UINT64;
                               pSrcBuffer: ID3D12Resource;
                               SrcOffset: UINT64;
                               NumBytes: UINT64); stdcall;

    procedure CopyTextureRegion(pDst: PD3D12_TEXTURE_COPY_LOCATION;
                                DstX: UINT;
                                DstY: UINT;
                                DstZ: UINT;
                                pSrc: PD3D12_TEXTURE_COPY_LOCATION;
                                pSrcBox: PD3D12_BOX); stdcall;

    procedure CopyResource(pDstResource: ID3D12Resource;
                           pSrcResource: ID3D12Resource); stdcall;

    procedure CopyTiles(pTiledResource: ID3D12Resource;
                        pTileRegionStartCoordinate: PD3D12_TILED_RESOURCE_COORDINATE;
                        pTileRegionSize: PD3D12_TILE_REGION_SIZE;
                        pBuffer: ID3D12Resource;
                        BufferStartOffsetInBytes: UINT64;
                        Flags: D3D12_TILE_COPY_FLAGS); stdcall;

    procedure ResolveSubresource(pDstResource: ID3D12Resource;
                                 DstSubresource: UINT;
                                 pSrcResource: ID3D12Resource;
                                 SrcSubresource: UINT;
                                 Format: DXGI_FORMAT); stdcall;

    procedure IASetPrimitiveTopology(PrimitiveTopology: D3D12_PRIMITIVE_TOPOLOGY); stdcall;

    procedure RSSetViewports(NumViewports: UINT;
                             pViewports: PD3D12_VIEWPORT); stdcall;

    procedure RSSetScissorRects(NumRects: UINT;
                                pRects: PD3D12_RECT); stdcall;

    procedure OMSetBlendFactor(BlendFactor: PSingle); stdcall;

    procedure OMSetStencilRef(StencilRef: UINT); stdcall;

    procedure SetPipelineState(pPipelineState: ID3D12PipelineState); stdcall;

    procedure ResourceBarrier(NumBarriers: UINT;
                              pBarriers: PD3D12_RESOURCE_BARRIER); stdcall;

    procedure ExecuteBundle(pCommandList: ID3D12GraphicsCommandList); stdcall;

    procedure SetDescriptorHeaps(NumDescriptorHeaps: UINT;
                                 ppDescriptorHeaps: PID3D12DescriptorHeap); stdcall;

    procedure SetComputeRootSignature(pRootSignature: ID3D12RootSignature); stdcall;

    procedure SetGraphicsRootSignature(pRootSignature: ID3D12RootSignature); stdcall;

    procedure SetComputeRootDescriptorTable(RootParameterIndex: UINT;
                                            BaseDescriptor: D3D12_GPU_DESCRIPTOR_HANDLE); stdcall;

    procedure SetGraphicsRootDescriptorTable(RootParameterIndex: UINT;
                                             BaseDescriptor: D3D12_GPU_DESCRIPTOR_HANDLE); stdcall;

    procedure SetComputeRoot32BitConstant(RootParameterIndex: UINT;
                                          SrcData: UINT;
                                          DestOffsetIn32BitValues: UINT); stdcall;

    procedure SetGraphicsRoot32BitConstant(RootParameterIndex: UINT;
                                           SrcData: UINT;
                                           DestOffsetIn32BitValues: UINT); stdcall;

    procedure SetComputeRoot32BitConstants(RootParameterIndex: UINT;
                                           Num32BitValuesToSet: UINT;
                                           pSrcData: Pointer;
                                           DestOffsetIn32BitValues: UINT); stdcall;

    procedure SetGraphicsRoot32BitConstants(RootParameterIndex: UINT;
                                            Num32BitValuesToSet: UINT;
                                            pSrcData: Pointer;
                                            DestOffsetIn32BitValues: UINT); stdcall;

    procedure SetComputeRootConstantBufferView(RootParameterIndex: UINT;
                                               BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure SetGraphicsRootConstantBufferView(RootParameterIndex: UINT;
                                                BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure SetComputeRootShaderResourceView(RootParameterIndex: UINT;
                                               BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure SetGraphicsRootShaderResourceView(RootParameterIndex: UINT;
                                                BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure SetComputeRootUnorderedAccessView(RootParameterIndex: UINT;
                                                BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure SetGraphicsRootUnorderedAccessView(RootParameterIndex: UINT;
                                                 BufferLocation: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure IASetIndexBuffer(pView: PD3D12_INDEX_BUFFER_VIEW); stdcall;

    procedure IASetVertexBuffers(StartSlot: UINT;
                                 NumViews: UINT;
                                 pViews: PD3D12_VERTEX_BUFFER_VIEW); stdcall;

    procedure SOSetTargets(StartSlot: UINT;
                           NumViews: UINT;
                           pViews: PD3D12_STREAM_OUTPUT_BUFFER_VIEW); stdcall;

    procedure OMSetRenderTargets(NumRenderTargetDescriptors: UINT;
                                 pRenderTargetDescriptors: PD3D12_CPU_DESCRIPTOR_HANDLE;
                                 RTsSingleHandleToDescriptorRange: BOOL;
                                 pDepthStencilDescriptor: PD3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure ClearDepthStencilView(DepthStencilView: D3D12_CPU_DESCRIPTOR_HANDLE;
                                    ClearFlags: D3D12_CLEAR_FLAGS;
                                    Depth: FLOAT;
                                    Stencil: UINT8;
                                    NumRects: UINT;
                                    pRects: PD3D12_RECT); stdcall;

    procedure ClearRenderTargetView(RenderTargetView: D3D12_CPU_DESCRIPTOR_HANDLE;
                                    ColorRGBA: PSingle;
                                    NumRects: UINT;
                                    pRects: PD3D12_RECT); stdcall;

    procedure ClearUnorderedAccessViewUint(ViewGPUHandleInCurrentHeap: D3D12_GPU_DESCRIPTOR_HANDLE;
                                           ViewCPUHandle: D3D12_CPU_DESCRIPTOR_HANDLE;
                                           pResource: ID3D12Resource;
                                           Values: PUINT;
                                           NumRects: UINT;
                                           pRects: PD3D12_RECT); stdcall;

    procedure ClearUnorderedAccessViewFloat(ViewGPUHandleInCurrentHeap: D3D12_GPU_DESCRIPTOR_HANDLE;
                                            ViewCPUHandle: D3D12_CPU_DESCRIPTOR_HANDLE;
                                            pResource: ID3D12Resource;
                                            Values: PSingle;
                                            NumRects: UINT;
                                            pRects: PD3D12_RECT); stdcall;

    procedure DiscardResource(pResource: ID3D12Resource;
                              pRegion: PD3D12_DISCARD_REGION); stdcall;

    procedure BeginQuery(pQueryHeap: ID3D12QueryHeap;
                         Type_: D3D12_QUERY_TYPE;
                         Index: UINT); stdcall;

    procedure EndQuery(pQueryHeap: ID3D12QueryHeap;
                       Type_: D3D12_QUERY_TYPE;
                       Index: UINT); stdcall;

    procedure ResolveQueryData(pQueryHeap: ID3D12QueryHeap;
                               Type_: D3D12_QUERY_TYPE;
                               StartIndex: UINT;
                               NumQueries: UINT;
                               pDestinationBuffer: ID3D12Resource;
                               AlignedDestinationBufferOffset: UINT64); stdcall;

    procedure SetPredication(pBuffer: ID3D12Resource;
                             AlignedBufferOffset: UINT64;
                             Operation: D3D12_PREDICATION_OP); stdcall;

    procedure SetMarker(Metadata: UINT;
                        pData: Pointer;
                        Size: UINT); stdcall;

    procedure BeginEvent(Metadata: UINT;
                         pData: Pointer;
                         Size: UINT); stdcall;

    procedure EndEvent(); stdcall;

    procedure ExecuteIndirect(pCommandSignature: ID3D12CommandSignature;
                              MaxCommandCount: UINT;
                              pArgumentBuffer: ID3D12Resource;
                              ArgumentBufferOffset: UINT64;
                              pCountBuffer: ID3D12Resource;
                              CountBufferOffset: UINT64); stdcall;
  end;
  IID_ID3D12GraphicsCommandList = ID3D12GraphicsCommandList;


  // Interface ID3D12GraphicsCommandList1
  // ====================================
  //
  ID3D12GraphicsCommandList1 = interface(ID3D12GraphicsCommandList)
    ['{553103FB-1FE7-4557-BB38-946D7D0E7CA7}']

    procedure AtomicCopyBufferUINT(pDstBuffer: ID3D12Resource;
                                   DstOffset: UINT64;
                                   pSrcBuffer: ID3D12Resource;
                                   SrcOffset: UINT64;
                                   Dependencies: UINT;
                                   ppDependentResources: PID3D12Resource;
                                   pDependentSubresourceRanges: PD3D12_SUBRESOURCE_RANGE_UINT64); stdcall;

    // UINT64 is only valid on UMA architectures
    procedure AtomicCopyBufferUINT64(pDstBuffer: ID3D12Resource;
                                     DstOffset: UINT64;
                                     pSrcBuffer: ID3D12Resource;
                                     SrcOffset: UINT64;
                                     Dependencies: UINT;
                                     ppDependentResources: PID3D12Resource;
                                     pDependentSubresourceRanges: PD3D12_SUBRESOURCE_RANGE_UINT64); stdcall;

    procedure OMSetDepthBounds(Min: FLOAT;
                               Max: FLOAT); stdcall;

    procedure SetSamplePositions(NumSamplesPerPixel: UINT;
                                 NumPixels: UINT;
                                 pSamplePositions: PD3D12_SAMPLE_POSITION); stdcall;

    procedure ResolveSubresourceRegion(pDstResource: ID3D12Resource;
                                       DstSubresource: UINT;
                                       DstX: UINT;
                                       DstY: UINT;
                                       pSrcResource: ID3D12Resource;
                                       SrcSubresource: UINT;
                                       pSrcRect: PD3D12_RECT;
                                       Format: DXGI_FORMAT;
                                       ResolveMode: D3D12_RESOLVE_MODE); stdcall;

    procedure SetViewInstanceMask(Mask: UINT); stdcall;
  end;
  IID_ID3D12GraphicsCommandList1 = ID3D12GraphicsCommandList1;


  PD3D12_WRITEBUFFERIMMEDIATE_PARAMETER = ^D3D12_WRITEBUFFERIMMEDIATE_PARAMETER;
  D3D12_WRITEBUFFERIMMEDIATE_PARAMETER = record
    Dest: D3D12_GPU_VIRTUAL_ADDRESS;
    Value: UINT32;
  end;


  PD3D12_WRITEBUFFERIMMEDIATE_MODE = ^D3D12_WRITEBUFFERIMMEDIATE_MODE;
  D3D12_WRITEBUFFERIMMEDIATE_MODE = (D3D12_WRITEBUFFERIMMEDIATE_MODE_DEFAULT    = $0,
                                     D3D12_WRITEBUFFERIMMEDIATE_MODE_MARKER_IN  = $1,
                                     D3D12_WRITEBUFFERIMMEDIATE_MODE_MARKER_OUT = $2
                                    );


  // Interface ID3D12GraphicsCommandList2
  // ====================================
  //
  ID3D12GraphicsCommandList2 = interface(ID3D12GraphicsCommandList1)
    ['{38C3E585-FF17-412C-9150-4FC6F9D72A28}']

    procedure WriteBufferImmediate(Count: UINT;
                                   pParams: PD3D12_WRITEBUFFERIMMEDIATE_PARAMETER;
                                   pModes: PD3D12_WRITEBUFFERIMMEDIATE_MODE); stdcall;
  end;
  IID_ID3D12GraphicsCommandList2 = ID3D12GraphicsCommandList2;


  // Interface ID3D12CommandQueue
  // ============================
  //
  ID3D12CommandQueue = interface(ID3D12Pageable)
    ['{0EC870A6-5D7E-4C22-8CFC-5BAAE07616ED}']

    procedure UpdateTileMappings(pResource: ID3D12Resource;
                                 NumResourceRegions: UINT;
                                 pResourceRegionStartCoordinates: PD3D12_TILED_RESOURCE_COORDINATE;
                                 pResourceRegionSizes: PD3D12_TILE_REGION_SIZE;
                                 pHeap: ID3D12Heap;
                                 NumRanges: UINT;
                                 pRangeFlags: PD3D12_TILE_RANGE_FLAGS;
                                 pHeapRangeStartOffsets: PUINT;
                                 pRangeTileCounts: PUINT;
                                 Flags: D3D12_TILE_MAPPING_FLAGS); stdcall;

    procedure CopyTileMappings(pDstResource: ID3D12Resource;
                               pDstRegionStartCoordinate: PD3D12_TILED_RESOURCE_COORDINATE;
                               pSrcResource: ID3D12Resource;
                               pSrcRegionStartCoordinate: PD3D12_TILED_RESOURCE_COORDINATE;
                               pRegionSize: PD3D12_TILE_REGION_SIZE;
                               Flags: D3D12_TILE_MAPPING_FLAGS); stdcall;

    procedure ExecuteCommandLists(NumCommandLists: UINT;
                                  ppCommandLists: PID3D12CommandList); stdcall;

    procedure SetMarker(Metadata: UINT;
                        pData: Pointer;
                        Size: UINT); stdcall;

    procedure BeginEvent(Metadata: UINT;
                         pData: Pointer;
                         Size: UINT); stdcall;

    procedure EndEvent; stdcall;

    function Signal(pFence: ID3D12Fence;
                    Value: UINT64): HRESULT; stdcall;

    function Wait(pFence: ID3D12Fence;
                  Value: UINT64): HRESULT; stdcall;

    function GetTimestampFrequency(pFrequency: PUINT64): HRESULT; stdcall;

    function GetClockCalibration(pGpuTimestamp: PUINT64;
                                 pCpuTimestamp: PUINT64): HRESULT; stdcall;

    procedure GetDesc(out RetVal: D3D12_COMMAND_QUEUE_DESC); stdcall;
  end;
  IID_ID3D12CommandQueue = ID3D12CommandQueue;


  // Interface ID3D12Device
  // ======================
  //
  ID3D12Device = interface(ID3D12Object)
    ['{189819F1-1DB6-4B57-BE54-1821339B85F7}']

    function GetNodeCount(): UINT; stdcall;

    function CreateCommandQueue(pDesc: PD3D12_COMMAND_QUEUE_DESC;
                                const riid: TGUID;
                                ppCommandQueue: PPointer): HRESULT; stdcall;

    function CreateCommandAllocator(type_: D3D12_COMMAND_LIST_TYPE;
                                    const riid: TGUID;
                                    ppCommandAllocator: PPointer): HRESULT; stdcall;

    function CreateGraphicsPipelineState(pDesc: PD3D12_GRAPHICS_PIPELINE_STATE_DESC;
                                         const riid: TGUID;
                                         ppPipelineState: PPointer): HRESULT; stdcall;

    function CreateComputePipelineState(pDesc: PD3D12_COMPUTE_PIPELINE_STATE_DESC;
                                        const riid: TGUID;
                                        ppPipelineState: PPointer): HRESULT; stdcall;

    function CreateCommandList(nodeMask: UINT;
                               type_: D3D12_COMMAND_LIST_TYPE;
                               pCommandAllocator: ID3D12CommandAllocator;
                               pInitialState: ID3D12PipelineState;
                               const riid: TGUID;
                               ppCommandList: PPointer): HRESULT; stdcall;

    function CheckFeatureSupport(Feature: D3D12_FEATURE;
                                 pFeatureSupportData: Pointer;
                                 FeatureSupportDataSize: UINT): HRESULT; stdcall;

    function CreateDescriptorHeap(pDescriptorHeapDesc: PD3D12_DESCRIPTOR_HEAP_DESC;
                                  const riid: TGUID;
                                  ppvHeap: PPointer): HRESULT; stdcall;

    function GetDescriptorHandleIncrementSize(DescriptorHeapType: D3D12_DESCRIPTOR_HEAP_TYPE): UINT; stdcall;

    function CreateRootSignature(nodeMask: UINT;
                                 pBlobWithRootSignature: Pointer;
                                 blobLengthInBytes: SIZE_T;
                                 const riid: TGUID;
                                 ppvRootSignature: PPointer): HRESULT; stdcall;

    procedure CreateConstantBufferView(pDesc: PD3D12_CONSTANT_BUFFER_VIEW_DESC;
                                       DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure CreateShaderResourceView(pResource: ID3D12Resource;
                                       pDesc: PD3D12_SHADER_RESOURCE_VIEW_DESC;
                                       DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure CreateUnorderedAccessView(pResource: ID3D12Resource;
                                        pCounterResource: ID3D12Resource;
                                        pDesc: PD3D12_UNORDERED_ACCESS_VIEW_DESC;
                                        DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure CreateRenderTargetView(pResource: ID3D12Resource;
                                     pDesc: PD3D12_RENDER_TARGET_VIEW_DESC;
                                     DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure CreateDepthStencilView(pResource: ID3D12Resource;
                                     pDesc: PD3D12_DEPTH_STENCIL_VIEW_DESC;
                                     DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure CreateSampler(pDesc: PD3D12_SAMPLER_DESC;
                            DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure CopyDescriptors(NumDestDescriptorRanges: UINT;
                              pDestDescriptorRangeStarts: PD3D12_CPU_DESCRIPTOR_HANDLE;
                              pDestDescriptorRangeSizes: PUINT;
                              NumSrcDescriptorRanges: UINT;
                              pSrcDescriptorRangeStarts: PD3D12_CPU_DESCRIPTOR_HANDLE;
                              pSrcDescriptorRangeSizes: PUINT;
                              DescriptorHeapsType: D3D12_DESCRIPTOR_HEAP_TYPE); stdcall;

    procedure CopyDescriptorsSimple(NumDescriptors: UINT;
                                    DestDescriptorRangeStart: D3D12_CPU_DESCRIPTOR_HANDLE;
                                    SrcDescriptorRangeStart: D3D12_CPU_DESCRIPTOR_HANDLE;
                                    DescriptorHeapsType: D3D12_DESCRIPTOR_HEAP_TYPE); stdcall;

    procedure GetResourceAllocationInfo(out RetVal: D3D12_RESOURCE_ALLOCATION_INFO;
                                        visibleMask: UINT;
                                        numResourceDescs: UINT;
                                        pResourceDescs: PD3D12_RESOURCE_DESC); stdcall;

    procedure GetCustomHeapProperties(out RetVal: D3D12_HEAP_PROPERTIES;
                                      nodeMask: UINT;
                                      heapType: D3D12_HEAP_TYPE); stdcall;

    function CreateCommittedResource(pHeapProperties: PD3D12_HEAP_PROPERTIES;
                                     HeapFlags: D3D12_HEAP_FLAGS;
                                     pDesc: PD3D12_RESOURCE_DESC;
                                     InitialResourceState: D3D12_RESOURCE_STATES;
                                     pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                     const riidResource: TGUID;
                                     ppvResource: PPointer): HRESULT; stdcall;

    function CreateHeap(pDesc: PD3D12_HEAP_DESC;
                        const riid: TGUID;
                        ppvHeap: PPointer): HRESULT; stdcall;

    function CreatePlacedResource(pHeap: ID3D12Heap;
                                  HeapOffset: UINT64;
                                  pDesc: PD3D12_RESOURCE_DESC;
                                  InitialState: D3D12_RESOURCE_STATES;
                                  pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                  const riid: TGUID;
                                  ppvResource: PPointer): HRESULT; stdcall;

    function CreateReservedResource(pDesc: PD3D12_RESOURCE_DESC;
                                    InitialState: D3D12_RESOURCE_STATES;
                                    pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                    const riid: TGUID;
                                    ppvResource: PPointer): HRESULT; stdcall;

    function CreateSharedHandle(pObject: ID3D12DeviceChild;
                                pAttributes: PSecurityAttributes;
                                Access: DWORD;
                                Name: LPCWSTR;
                                pHandle: PHandle): HRESULT; stdcall;

    function OpenSharedHandle(NTHandle: THandle;
                              const riid: TGUID;
                              ppvObj: PPointer): HRESULT; stdcall;

    function OpenSharedHandleByName(Name: LPCWSTR;
                                    Access: DWORD;
                                    pNTHandle: PHandle): HRESULT; stdcall;

    function MakeResident(NumObjects: UINT;
                          ppObjects: PID3D12Pageable): HRESULT; stdcall;

    function Evict(NumObjects: UINT;
                   ppObjects: PID3D12Pageable): HRESULT; stdcall;

    function CreateFence(InitialValue: UINT64;
                         Flags: D3D12_FENCE_FLAGS;
                         const riid: TGUID;
                         ppFence: PPointer): HRESULT; stdcall;

    function GetDeviceRemovedReason: HRESULT; stdcall;

    procedure GetCopyableFootprints(pResourceDesc: PD3D12_RESOURCE_DESC;
                                    FirstSubresource: UINT;
                                    NumSubresources: UINT;
                                    BaseOffset: UINT64;
                                    pLayouts: PD3D12_PLACED_SUBRESOURCE_FOOTPRINT;
                                    pNumRows: PUINT;
                                    pRowSizeInBytes: PUINT64;
                                    pTotalBytes: PUINT64); stdcall;

    function CreateQueryHeap(pDesc: PD3D12_QUERY_HEAP_DESC;
                             const riid: TGUID;
                             ppvHeap: PPointer): HRESULT; stdcall;

    function SetStablePowerState(Enable: BOOL): HRESULT; stdcall;

    function CreateCommandSignature(pDesc: PD3D12_COMMAND_SIGNATURE_DESC;
                                    pRootSignature: ID3D12RootSignature;
                                    const riid: TGUID;
                                    ppvCommandSignature: PPointer): HRESULT; stdcall;

    procedure GetResourceTiling(pTiledResource: ID3D12Resource;
                                pNumTilesForEntireResource: PUINT;
                                pPackedMipDesc: PD3D12_PACKED_MIP_INFO;
                                pStandardTileShapeForNonPackedMips: PD3D12_TILE_SHAPE;
                                pNumSubresourceTilings: PUINT;
                                FirstSubresourceTilingToGet: UINT;
                                pSubresourceTilingsForNonPackedMips: PD3D12_SUBRESOURCE_TILING); stdcall;

    procedure GetAdapterLuid(out RetVal: LUID); stdcall;
  end;
  IID_ID3D12Device = ID3D12Device;


  // D3D12 Revision 1

  // Interface ID3D12PipelineLibrary
  // ===============================
  //
  ID3D12PipelineLibrary = interface(ID3D12DeviceChild)
    ['{C64226A8-9201-46AF-B4CC-53FB9FF7414F}']

    function StorePipeline(pName: LPCWSTR;
                           pPipeline: ID3D12PipelineState): HRESULT; stdcall;

    function LoadGraphicsPipeline(pName: LPCWSTR;
                                  pDesc: PD3D12_GRAPHICS_PIPELINE_STATE_DESC;
                                  const riid: TGUID;
                                  ppPipelineState: PPointer): HRESULT; stdcall;

    function LoadComputePipeline(pName: LPCWSTR;
                                 pDesc: PD3D12_COMPUTE_PIPELINE_STATE_DESC;
                                 const riid: TGUID;
                                 ppPipelineState: PPointer): HRESULT; stdcall;

    function GetSerializedSize(): SIZE_T; stdcall;

    function Serialize(pData: Pointer;
                       DataSizeInBytes: SIZE_T): HRESULT; stdcall;
  end;
  IID_ID3D12PipelineLibrary = ID3D12PipelineLibrary;


  // Pipeline State v2

  // Interface ID3D12PipelineLibrary1
  // ================================
  //
  ID3D12PipelineLibrary1 = interface(ID3D12PipelineLibrary)
    ['{80EABF42-2568-4E5E-BD82-C37F86961DC3}']

    function LoadPipeline(pName: LPCWSTR;
                          pDesc: PD3D12_PIPELINE_STATE_STREAM_DESC;
                          const riid: TGUID;
                          ppPipelineState: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12PipelineLibrary1 = ID3D12PipelineLibrary1;


  // Interface ID3D12Device1
  // =======================
  //
  ID3D12Device1 = interface(ID3D12Device)
    ['{77ACCE80-638E-4E65-8895-C1F23386863E}']

    function CreatePipelineLibrary(pLibraryBlob: Pointer;
                                   BlobLength: SIZE_T;
                                   const riid: TGUID;
                                   ppPipelineLibrary: PPointer): HRESULT; stdcall;

    function SetEventOnMultipleFenceCompletion(ppFences: PID3D12Fence;
                                               pFenceValues: PUINT64;
                                               NumFences: UINT;
                                               Flags: D3D12_MULTIPLE_FENCE_WAIT_FLAGS;
                                               hEvent: THandle): HRESULT; stdcall;

    function SetResidencyPriority(NumObjects: UINT;
                                  ppObjects: PID3D12Pageable;
                                  pPriorities: PD3D12_RESIDENCY_PRIORITY): HRESULT; stdcall;
  end;
  IID_ID3D12Device1 = ID3D12Device1;


  // Interface ID3D12Device2
  // =======================
  //
  ID3D12Device2 = interface(ID3D12Device1)
    ['{30BAA41E-B15B-475C-A0BB-1AF5C5B64328}']

    function CreatePipelineState(pDesc: PD3D12_PIPELINE_STATE_STREAM_DESC;
                                 const riid: TGUID;
                                 ppPipelineState: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Device2 = ID3D12Device2;


  // Interface ID3D12Device3
  // =======================
  //
  ID3D12Device3 = interface(ID3D12Device2)
    ['{81DADC15-2BAD-4392-93C5-101345C4AA98}']

    function OpenExistingHeapFromAddress(pAddress: Pointer;
                                         const riid: TGUID;
                                         ppvHeap: PPointer): HRESULT; stdcall;

    function OpenExistingHeapFromFileMapping(hFileMapping: THandle;
                                             const riid: TGUID;
                                             ppvHeap: PPointer): HRESULT; stdcall;

    function EnqueueMakeResident(Flags: D3D12_RESIDENCY_FLAGS;
                                 NumObjects: UINT;
                                 ppObjects: PID3D12Pageable;
                                 pFenceToSignal: ID3D12Fence;
                                 FenceValueToSignal: UINT64): HRESULT; stdcall;
  end;
  IID_ID3D12Device3 = ID3D12Device3;


  PD3D12_PROTECTED_SESSION_STATUS = ^D3D12_PROTECTED_SESSION_STATUS;
  D3D12_PROTECTED_SESSION_STATUS = (D3D12_PROTECTED_SESSION_STATUS_OK      = 0,
                                    D3D12_PROTECTED_SESSION_STATUS_INVALID = 1
                                   );


  // Interface ID3D12ProtectedSession
  // ================================
  //
  ID3D12ProtectedSession = interface(ID3D12DeviceChild)
    ['{A1533D18-0AC1-4084-85B9-89A96116806B}']

    function GetStatusFence(const riid: TGUID;
                            ppFence: PPointer): HRESULT; stdcall;

    function GetSessionStatus(): D3D12_PROTECTED_SESSION_STATUS; stdcall;
  end;
  IID_ID3D12ProtectedSession = ID3D12ProtectedSession;


  PD3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_SUPPORT = ^D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_SUPPORT;
  D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_SUPPORT = record
    NodeIndex: UINT;                                          // input
    Support: D3D12_PROTECTED_RESOURCE_SESSION_SUPPORT_FLAGS;  // output
  end;


  PD3D12_PROTECTED_RESOURCE_SESSION_DESC = ^D3D12_PROTECTED_RESOURCE_SESSION_DESC;
  D3D12_PROTECTED_RESOURCE_SESSION_DESC = record
    NodeMask: UINT;
    Flags: D3D12_PROTECTED_RESOURCE_SESSION_FLAGS;
  end;


  // Interface ID3D12ProtectedResourceSession
  // ========================================
  //
  ID3D12ProtectedResourceSession = interface(ID3D12ProtectedSession)
    ['{6CD696F4-F289-40CC-8091-5A6C0A099C3D}']

    procedure GetDesc(out RetVal: D3D12_PROTECTED_RESOURCE_SESSION_DESC); stdcall;
  end;
  IID_ID3D12ProtectedResourceSession = ID3D12ProtectedResourceSession;


  // Interface ID3D12Device4
  // =======================
  //
  ID3D12Device4 = interface(ID3D12Device3)
    ['{E865DF17-A9EE-46F9-A463-3098315AA2E5}']

    function CreateCommandList1(nodeMask: UINT;
                                type_: D3D12_COMMAND_LIST_TYPE;
                                flags: D3D12_COMMAND_LIST_FLAGS;
                                const riid: TGUID;
                                ppCommandList: PPointer): HRESULT; stdcall;

    function CreateProtectedResourceSession(pDesc: PD3D12_PROTECTED_RESOURCE_SESSION_DESC;
                                            const riid: TGUID;
                                            ppSession: PPointer): HRESULT; stdcall;

    function CreateCommittedResource1(pHeapProperties: PD3D12_HEAP_PROPERTIES;
                                      HeapFlags: D3D12_HEAP_FLAGS;
                                      pDesc: PD3D12_RESOURCE_DESC;
                                      InitialResourceState: D3D12_RESOURCE_STATES;
                                      pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                      pProtectedSession: ID3D12ProtectedResourceSession;
                                      const riidResource: TGUID;
                                      ppvResource: PPointer): HRESULT; stdcall;

    function CreateHeap1(pDesc: PD3D12_HEAP_DESC;
                         pProtectedSession: ID3D12ProtectedResourceSession;
                         const riid: TGUID;
                         ppvHeap: PPointer): HRESULT; stdcall;

    function CreateReservedResource1(pDesc: PD3D12_RESOURCE_DESC;
                                     InitialState: D3D12_RESOURCE_STATES;
                                     pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                     pProtectedSession: ID3D12ProtectedResourceSession;
                                     const riid: TGUID;
                                     ppvResource: PPointer): HRESULT; stdcall;

    procedure GetResourceAllocationInfo1(out RetVal: D3D12_RESOURCE_ALLOCATION_INFO;
                                         visibleMask: UINT;
                                         numResourceDescs: UINT;
                                         pResourceDescs: PD3D12_RESOURCE_DESC;
                                         pResourceAllocationInfo1: PD3D12_RESOURCE_ALLOCATION_INFO1); stdcall;
  end;
  IID_ID3D12Device4 = ID3D12Device4;



  PD3D12_LIFETIME_STATE = ^D3D12_LIFETIME_STATE;
  D3D12_LIFETIME_STATE = (D3D12_LIFETIME_STATE_IN_USE,
                          D3D12_LIFETIME_STATE_NOT_IN_USE
                         );


  // Interface ID3D12LifetimeOwner
  // =============================
  //
  ID3D12LifetimeOwner = interface(IUnknown)
    ['{E667AF9F-CD56-4F46-83CE-032E595D70A8}']

    procedure LifetimeStateUpdated(NewState: D3D12_LIFETIME_STATE); stdcall;
  end;
  IID_ID3D12LifetimeOwner = ID3D12LifetimeOwner;


  // Interface ID3D12SwapChainAssistant
  // ==================================
  //
  ID3D12SwapChainAssistant = interface(IUnknown)
    ['{F1DF64B6-57FD-49CD-8807-C0EB88B45C8F}']

    procedure GetLUID(out RetVal: LUID); stdcall;

    function GetSwapChainObject(const riid: TGUID;
                                ppv: PPointer): HRESULT; stdcall;

    function GetCurrentResourceAndCommandQueue(const riidResource: TGUID;
                                               ppvResource: PPointer;
                                               const riidQueue: TGUID;
                                               ppvQueue: PPointer): HRESULT; stdcall;

    function InsertImplicitSync(): HRESULT; stdcall;
  end;
  IID_ID3D12SwapChainAssistant = ID3D12SwapChainAssistant;


  // Interface ID3D12LifetimeTracker
  // ===============================
  //
  ID3D12LifetimeTracker = interface(ID3D12DeviceChild)
    ['{3FD03D36-4EB1-424A-A582-494ECB8BA813}']

    function DestroyOwnedObject(pObject: ID3D12DeviceChild): HRESULT; stdcall;
  end;
  IID_ID3D12LifetimeTracker = ID3D12LifetimeTracker;


  PD3D12_META_COMMAND_PARAMETER_TYPE = ^D3D12_META_COMMAND_PARAMETER_TYPE;
  D3D12_META_COMMAND_PARAMETER_TYPE = (D3D12_META_COMMAND_PARAMETER_TYPE_FLOAT                                       = 0,
                                       D3D12_META_COMMAND_PARAMETER_TYPE_UINT64                                      = 1,
                                       D3D12_META_COMMAND_PARAMETER_TYPE_GPU_VIRTUAL_ADDRESS                         = 2,
                                       D3D12_META_COMMAND_PARAMETER_TYPE_CPU_DESCRIPTOR_HANDLE_HEAP_TYPE_CBV_SRV_UAV = 3,
                                       D3D12_META_COMMAND_PARAMETER_TYPE_GPU_DESCRIPTOR_HANDLE_HEAP_TYPE_CBV_SRV_UAV = 4
                                      );


  PD3D12_META_COMMAND_PARAMETER_STAGE = ^D3D12_META_COMMAND_PARAMETER_STAGE;
  D3D12_META_COMMAND_PARAMETER_STAGE = (D3D12_META_COMMAND_PARAMETER_STAGE_CREATION       = 0,
                                        D3D12_META_COMMAND_PARAMETER_STAGE_INITIALIZATION = 1,
                                        D3D12_META_COMMAND_PARAMETER_STAGE_EXECUTION      = 2
                                       );


  PD3D12_META_COMMAND_PARAMETER_DESC = ^D3D12_META_COMMAND_PARAMETER_DESC;
  D3D12_META_COMMAND_PARAMETER_DESC = record
    Name: PWideChar;
    Type_: D3D12_META_COMMAND_PARAMETER_TYPE;
    Flags: D3D12_META_COMMAND_PARAMETER_FLAGS;
    RequiredResourceState: D3D12_RESOURCE_STATES;
    StructureOffset: UINT;
  end;


  PD3D12_META_COMMAND_DESC = ^D3D12_META_COMMAND_DESC;
  D3D12_META_COMMAND_DESC = record
    Id: TGUID;
    Name: PWideChar;
    InitializationDirtyState: D3D12_GRAPHICS_STATES;
    ExecutionDirtyState: D3D12_GRAPHICS_STATES;
  end;


  // Interface ID3D12StateObject
  // ===========================
  //
  ID3D12StateObject = interface(ID3D12Pageable)
    ['{47016943-FCA8-4594-93EA-AF258B55346D}']
  end;
  IID_ID3D12StateObject = ID3D12StateObject;


  // Interface ID3D12StateObjectProperties
  // =====================================
  //
  ID3D12StateObjectProperties = interface(IUnknown)
    ['{DE5FA827-9BF9-4F26-89FF-D7F56FDE3860}']

    function GetShaderIdentifier(pExportName: LPCWSTR): Pointer; stdcall;

    function GetShaderStackSize(pExportName: LPCWSTR): UINT64; stdcall;

    function GetPipelineStackSize(): UINT64; stdcall;

    procedure SetPipelineStackSize(PipelineStackSizeInBytes: UINT64); stdcall;
  end;
  IID_ID3D12StateObjectProperties = ID3D12StateObjectProperties;


  // Update May 2024

  PD3D12_PROGRAM_IDENTIFIER = ^D3D12_PROGRAM_IDENTIFIER;
  D3D12_PROGRAM_IDENTIFIER = record
    OpaqueData: array[0..3] of UINT64;
  end;


  // Interface ID3D12StateObjectProperties1
  // ======================================
  //
  ID3D12StateObjectProperties1 = interface(ID3D12StateObjectProperties)
    ['{460CAAC7-1D24-446A-A184-CA67DB494138}']

    procedure GetProgramIdentifier(out RetVal: D3D12_PROGRAM_IDENTIFIER;
                                   pProgramName: LPCWSTR); stdcall;
  end;
  IID_ID3D12StateObjectProperties1 = ID3D12StateObjectProperties1;

  { 10.0.28000.0  }
  ID3D12StateObjectProperties2 = interface(ID3D12StateObjectProperties1)
    ['{D5E82917-F0F1-44CF-AE5E-CE222DD0B884}']
    function GetGlobalRootSignatureForProgram(pProgramName: LPCWSTR;
                                              const riid: TGUID;
                                              ppvRootSignature: PPointer): HRESULT; stdcall;
    function GetGlobalRootSignatureForShader(pExportName: LPCWSTR;
                                             const riid: TGUID;
                                             ppvRootSignature: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12StateObjectProperties2 = ID3D12StateObjectProperties2;


  PD3D12_NODE_ID = ^D3D12_NODE_ID;
  D3D12_NODE_ID = record
    Name: PWideChar;
    ArrayIndex: UINT;
  end;


  PD3D12_WORK_GRAPH_MEMORY_REQUIREMENTS = ^D3D12_WORK_GRAPH_MEMORY_REQUIREMENTS;
  D3D12_WORK_GRAPH_MEMORY_REQUIREMENTS = record
    MinSizeInBytes: UINT64;
    MaxSizeInBytes: UINT64;
    SizeGranularityInBytes: UINT;
  end;


  // Interface ID3D12StateObjectProperties1
  // ======================================
  //
  ID3D12WorkGraphProperties = interface(IUnknown)
    ['{065ACF71-F863-4B89-82F4-02E4D5886757}']

    function GetNumWorkGraphs(): UINT; stdcall;
    function GetProgramName(WorkGraphIndex: UINT): LPCWSTR; stdcall;
    function GetWorkGraphIndex(pProgramName: LPCWSTR): UINT; stdcall;

    function GetNumNodes(WorkGraphIndex: UINT): UINT; stdcall;

    procedure GetNodeID(out RetVal: D3D12_NODE_ID;
                        WorkGraphIndex: UINT;
                        NodeIndex: UINT); stdcall;

    function GetNodeIndex(WorkGraphIndex: UINT;
                          NodeID: D3D12_NODE_ID): UINT; stdcall;

    function GetNodeLocalRootArgumentsTableIndex(WorkGraphIndex: UINT;
                                                 NodeIndex: UINT): UINT; stdcall;

    function GetNumEntrypoints(WorkGraphIndex: UINT): UINT; stdcall;

    procedure GetEntrypointID(out RetVal: D3D12_NODE_ID;
                              WorkGraphIndex: UINT;
                              EntrypointIndex: UINT); stdcall;

    function GetEntrypointIndex(WorkGraphIndex: UINT;
                                NodeID: D3D12_NODE_ID): UINT; stdcall;

    function GetEntrypointRecordSizeInBytes(WorkGraphIndex: UINT;
                                            EntrypointIndex: UINT): UINT; stdcall;

    procedure GetWorkGraphMemoryRequirements(WorkGraphIndex: UINT;
                                             pWorkGraphMemoryRequirements: PD3D12_WORK_GRAPH_MEMORY_REQUIREMENTS); stdcall;

    { 10.0.28000.0 }
    function GetEntrypointRecordAlignmentInBytes(WorkGraphIndex: UINT;
                                                 EntrypointIndex: UINT): UINT; stdcall;
  end;
  IID_ID3D12WorkGraphProperties = ID3D12WorkGraphProperties;

  // ==

  PD3D12_STATE_SUBOBJECT_TYPE = ^D3D12_STATE_SUBOBJECT_TYPE;
  D3D12_STATE_SUBOBJECT_TYPE = (D3D12_STATE_SUBOBJECT_TYPE_STATE_OBJECT_CONFIG                   = 0,   // D3D12_STATE_OBJECT_CONFIG
                                D3D12_STATE_SUBOBJECT_TYPE_GLOBAL_ROOT_SIGNATURE                 = 1,   // D3D12_GLOBAL_ROOT_SIGNATURE
                                D3D12_STATE_SUBOBJECT_TYPE_LOCAL_ROOT_SIGNATURE                  = 2,   // D3D12_LOCAL_ROOT_SIGNATURE
                                D3D12_STATE_SUBOBJECT_TYPE_NODE_MASK                             = 3,   // D3D12_NODE_MASK
                                // 4 unused
                                D3D12_STATE_SUBOBJECT_TYPE_DXIL_LIBRARY                          = 5,   // D3D12_DXIL_LIBRARY_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_EXISTING_COLLECTION                   = 6,   // D3D12_EXISTING_COLLECTION_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_SUBOBJECT_TO_EXPORTS_ASSOCIATION      = 7,   // D3D12_SUBOBJECT_TO_EXPORTS_ASSOCIATION
                                D3D12_STATE_SUBOBJECT_TYPE_DXIL_SUBOBJECT_TO_EXPORTS_ASSOCIATION = 8,   // D3D12_DXIL_SUBOBJECT_TO_EXPORTS_ASSOCIATION
                                D3D12_STATE_SUBOBJECT_TYPE_RAYTRACING_SHADER_CONFIG              = 9,   // D3D12_RAYTRACING_SHADER_CONFIG
                                D3D12_STATE_SUBOBJECT_TYPE_RAYTRACING_PIPELINE_CONFIG            = 10,  // D3D12_RAYTRACING_PIPELINE_CONFIG
                                D3D12_STATE_SUBOBJECT_TYPE_HIT_GROUP                             = 11,  // D3D12_HIT_GROUP_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_RAYTRACING_PIPELINE_CONFIG1           = 12,  // D3D12_RAYTRACING_PIPELINE_CONFIG1
                                // Update May 2024
                                D3D12_STATE_SUBOBJECT_TYPE_WORK_GRAPH                            = 13, // D3D12_WORK_GRAPH_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_STREAM_OUTPUT                         = 14,  // D3D12_STREAM_OUTPUT_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_BLEND                                 = 15,  // D3D12_BLEND_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_SAMPLE_MASK                           = 16,  // UINT
                                D3D12_STATE_SUBOBJECT_TYPE_RASTERIZER                            = 17,  // D3D12_RASTERIZER_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL                         = 18,  // D3D12_DEPTH_STENCIL_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_INPUT_LAYOUT                          = 19,  // D3D12_INPUT_LAYOUT_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_IB_STRIP_CUT_VALUE                    = 20,  // D3D12_INDEX_BUFFER_STRIP_CUT_VALUE
                                D3D12_STATE_SUBOBJECT_TYPE_PRIMITIVE_TOPOLOGY                    = 21,  // D3D12_PRIMITIVE_TOPOLOGY_TYPE
                                D3D12_STATE_SUBOBJECT_TYPE_RENDER_TARGET_FORMATS                 = 22,  // D3D12_RT_FORMAT_ARRAY
                                D3D12_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL_FORMAT                  = 23,  // DXGI_FORMAT
                                D3D12_STATE_SUBOBJECT_TYPE_SAMPLE_DESC                           = 24,  // DXGI_SAMPLE_DESC
                                // 25 is unused
                                D3D12_STATE_SUBOBJECT_TYPE_FLAGS                                 = 26,  // D3D12_PIPELINE_STATE_FLAGS
                                D3D12_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL1                        = 27, // D3D12_DEPTH_STENCIL_DESC1
                                D3D12_STATE_SUBOBJECT_TYPE_VIEW_INSTANCING                       = 28, // D3D12_VIEW_INSTANCING_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_GENERIC_PROGRAM                       = 29, // D3D12_GENERIC_PROGRAM_DESC
                                D3D12_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL2                        = 30, // D3D12_DEPTH_STENCIL_DESC2
                                // ==
                                { 10.0.28000.0 }
                                D3D12_STATE_SUBOBJECT_TYPE_GLOBAL_SERIALIZED_ROOT_SIGNATURE      = 31,
                                D3D12_STATE_SUBOBJECT_TYPE_LOCAL_SERIALIZED_ROOT_SIGNATURE       = 32,
                                D3D12_STATE_SUBOBJECT_TYPE_COMPILER_EXISITING_COLLECTION         = 33,
                                D3D12_STATE_SUBOBJECT_TYPE_MAX_VALID                             = 34
                               );


  PD3D12_STATE_SUBOBJECT = ^D3D12_STATE_SUBOBJECT;
  D3D12_STATE_SUBOBJECT = record
    Type_: D3D12_STATE_SUBOBJECT_TYPE;
    pDesc: Pointer;
  end;


  PD3D12_STATE_OBJECT_CONFIG = ^D3D12_STATE_OBJECT_CONFIG;
  D3D12_STATE_OBJECT_CONFIG = record
    Flags: D3D12_STATE_OBJECT_FLAGS;
  end;


  PD3D12_GLOBAL_ROOT_SIGNATURE = ^D3D12_GLOBAL_ROOT_SIGNATURE;
  D3D12_GLOBAL_ROOT_SIGNATURE = record
    pGlobalRootSignature: ID3D12RootSignature;
  end;


  PD3D12_LOCAL_ROOT_SIGNATURE = ^D3D12_LOCAL_ROOT_SIGNATURE;
  D3D12_LOCAL_ROOT_SIGNATURE = record
    pLocalRootSignature: ID3D12RootSignature;
  end;


  PD3D12_NODE_MASK = ^D3D12_NODE_MASK;
  D3D12_NODE_MASK = record
    NodeMask: UINT;
  end;

  // Update MAy 2024

  PD3D12_SAMPLE_MASK = ^D3D12_SAMPLE_MASK;
  D3D12_SAMPLE_MASK = record
    SampleMask: UINT;
  end;


  PD3D12_IB_STRIP_CUT_VALUE = ^D3D12_IB_STRIP_CUT_VALUE;
  D3D12_IB_STRIP_CUT_VALUE = record
    IndexBufferStripCutValue: D3D12_INDEX_BUFFER_STRIP_CUT_VALUE;
  end;


  PD3D12_PRIMITIVE_TOPOLOGY_DESC = ^D3D12_PRIMITIVE_TOPOLOGY_DESC;
  D3D12_PRIMITIVE_TOPOLOGY_DESC = record
    PrimitiveTopology: D3D12_PRIMITIVE_TOPOLOGY_TYPE;
  end;


  PD3D12_DEPTH_STENCIL_FORMAT = ^D3D12_DEPTH_STENCIL_FORMAT;
  D3D12_DEPTH_STENCIL_FORMAT = record
    DepthStencilFormat: DXGI_FORMAT;
  end;

  // ==

  PD3D12_EXPORT_DESC = ^D3D12_EXPORT_DESC;
  D3D12_EXPORT_DESC = record
    Name: LPCWSTR;
    ExportToRename: LPCWSTR;
    Flags: D3D12_EXPORT_FLAGS;
  end;


  PD3D12_DXIL_LIBRARY_DESC = ^D3D12_DXIL_LIBRARY_DESC;
  D3D12_DXIL_LIBRARY_DESC = record
    DXILLibrary: D3D12_SHADER_BYTECODE;
    NumExports: UINT;               // Optional, if 0 all exports in the library/collection will be surfaced
    pExports: PD3D12_EXPORT_DESC;
  end;


  PD3D12_EXISTING_COLLECTION_DESC = ^D3D12_EXISTING_COLLECTION_DESC;
  D3D12_EXISTING_COLLECTION_DESC = record
    pExistingCollection: ID3D12StateObject;
    NumExports: UINT;               // Optional, if 0 all exports in the library/collection will be surfaced
    pExports: PD3D12_EXPORT_DESC;
  end;


  PD3D12_SUBOBJECT_TO_EXPORTS_ASSOCIATION = ^D3D12_SUBOBJECT_TO_EXPORTS_ASSOCIATION;
  D3D12_SUBOBJECT_TO_EXPORTS_ASSOCIATION = record
    pSubobjectToAssociate: PD3D12_STATE_SUBOBJECT;
    NumExports: UINT;
    pExports: LPCWSTR;
  end;


  PD3D12_DXIL_SUBOBJECT_TO_EXPORTS_ASSOCIATION = ^D3D12_DXIL_SUBOBJECT_TO_EXPORTS_ASSOCIATION;
  D3D12_DXIL_SUBOBJECT_TO_EXPORTS_ASSOCIATION = record
    SubobjectToAssociate: LPCWSTR;
    NumExports: UINT;
    pExports: PLPWSTR;
  end;


  PD3D12_HIT_GROUP_TYPE = ^D3D12_HIT_GROUP_TYPE;
  D3D12_HIT_GROUP_TYPE = (D3D12_HIT_GROUP_TYPE_TRIANGLES            = $0,
                          D3D12_HIT_GROUP_TYPE_PROCEDURAL_PRIMITIVE = $1
                         );


  PD3D12_HIT_GROUP_DESC = ^D3D12_HIT_GROUP_DESC;
  D3D12_HIT_GROUP_DESC = record
    HitGroupExport: LPCWSTR;
    Type_: D3D12_HIT_GROUP_TYPE;
    AnyHitShaderImport: LPCWSTR;
    ClosestHitShaderImport: LPCWSTR;
    IntersectionShaderImport: LPCWSTR;
  end;


  PD3D12_RAYTRACING_SHADER_CONFIG = ^D3D12_RAYTRACING_SHADER_CONFIG;
  D3D12_RAYTRACING_SHADER_CONFIG = record
    MaxPayloadSizeInBytes: UINT;
    MaxAttributeSizeInBytes: UINT;
  end;


  PD3D12_RAYTRACING_PIPELINE_CONFIG = ^D3D12_RAYTRACING_PIPELINE_CONFIG;
  D3D12_RAYTRACING_PIPELINE_CONFIG = record
    MaxTraceRecursionDepth: UINT;
  end;


  PD3D12_RAYTRACING_PIPELINE_CONFIG1 = ^D3D12_RAYTRACING_PIPELINE_CONFIG1;
  D3D12_RAYTRACING_PIPELINE_CONFIG1 = record
    MaxTraceRecursionDepth: UINT;
    Flags: D3D12_RAYTRACING_PIPELINE_FLAGS;
  end;

  // Update May 2024

  PD3D12_NODE_OUTPUT_OVERRIDES = ^D3D12_NODE_OUTPUT_OVERRIDES;
  D3D12_NODE_OUTPUT_OVERRIDES = record
    OutputIndex: UINT;
    pNewName: PD3D12_NODE_ID;
    pAllowSparseNodes: PBOOL;
    pMaxRecords: PUINT;
    pMaxRecordsSharedWithOutputIndex: PUINT;
  end;


  PD3D12_BROADCASTING_LAUNCH_OVERRIDES = ^D3D12_BROADCASTING_LAUNCH_OVERRIDES;
  D3D12_BROADCASTING_LAUNCH_OVERRIDES = record
    pLocalRootArgumentsTableIndex: PUINT;
    pProgramEntry: PBOOL;
    pNewName: PD3D12_NODE_ID;
    pShareInputOf: PD3D12_NODE_ID;
    pDispatchGrid: PUINT;
    pMaxDispatchGrid: PUINT;
    NumOutputOverrides: UINT;
    pOutputOverrides: PD3D12_NODE_OUTPUT_OVERRIDES;
  end;


  PD3D12_COALESCING_LAUNCH_OVERRIDES = ^D3D12_COALESCING_LAUNCH_OVERRIDES;
  D3D12_COALESCING_LAUNCH_OVERRIDES = record
    pLocalRootArgumentsTableIndex: PUINT;
    pProgramEntry: PBOOL;
    pNewName: PD3D12_NODE_ID;
    pShareInputOf: PD3D12_NODE_ID;
    NumOutputOverrides: UINT;
    pOutputOverrides: PD3D12_NODE_OUTPUT_OVERRIDES;
  end;


  PD3D12_THREAD_LAUNCH_OVERRIDES = ^D3D12_THREAD_LAUNCH_OVERRIDES;
  D3D12_THREAD_LAUNCH_OVERRIDES = record
    pLocalRootArgumentsTableIndex: PUINT;
    pProgramEntry: PBOOL;
    pNewName: PD3D12_NODE_ID;
    pShareInputOf: PD3D12_NODE_ID;
    NumOutputOverrides: UINT;
    pOutputOverrides: PD3D12_NODE_OUTPUT_OVERRIDES;
  end;


  PD3D12_COMMON_COMPUTE_NODE_OVERRIDES = ^D3D12_COMMON_COMPUTE_NODE_OVERRIDES;
  D3D12_COMMON_COMPUTE_NODE_OVERRIDES = record
    pLocalRootArgumentsTableIndex: PUINT;
    pProgramEntry: PBOOL;
    pNewName: PD3D12_NODE_ID;
    pShareInputOf: PD3D12_NODE_ID;
    NumOutputOverrides: UINT;
    pOutputOverrides: PD3D12_NODE_OUTPUT_OVERRIDES;
  end;


  PD3D12_NODE_OVERRIDES_TYPE = ^D3D12_NODE_OVERRIDES_TYPE;
  D3D12_NODE_OVERRIDES_TYPE                       = (
    D3D12_NODE_OVERRIDES_TYPE_NONE                = 0,
    D3D12_NODE_OVERRIDES_TYPE_BROADCASTING_LAUNCH = 1,
    D3D12_NODE_OVERRIDES_TYPE_COALESCING_LAUNCH   = 2,
    D3D12_NODE_OVERRIDES_TYPE_THREAD_LAUNCH       = 3,
    D3D12_NODE_OVERRIDES_TYPE_COMMON_COMPUTE      = 4
  );

  PD3D12_SHADER_NODE = ^D3D12_SHADER_NODE;
  D3D12_SHADER_NODE = record
    Shader: PWideChar;
    OverridesType: D3D12_NODE_OVERRIDES_TYPE;
    case Integer of
      0:  (pBroadcastingLaunchOverrides: PD3D12_BROADCASTING_LAUNCH_OVERRIDES);
      1:  (pCoalescingLaunchOverrides: PD3D12_COALESCING_LAUNCH_OVERRIDES);
      2:  (pThreadLaunchOverrides: PD3D12_THREAD_LAUNCH_OVERRIDES);
      3:  (pCommonComputeNodeOverrides: PD3D12_COMMON_COMPUTE_NODE_OVERRIDES);
   end;


  PD3D12_NODE_TYPE = ^D3D12_NODE_TYPE;
  D3D12_NODE_TYPE          = (
    D3D12_NODE_TYPE_SHADER = $0
  );


  PD3D12_NODE = ^D3D12_NODE;
  D3D12_NODE = record
    NodeType: D3D12_NODE_TYPE;
    case Integer of
      0: (Shader: D3D12_SHADER_NODE); // D3D12_NODE_TYPE_SHADER
  end;



  PD3D12_WORK_GRAPH_FLAGS = ^D3D12_WORK_GRAPH_FLAGS;
  D3D12_WORK_GRAPH_FLAGS                              = (
    D3D12_WORK_GRAPH_FLAG_NONE                        = $0,
    D3D12_WORK_GRAPH_FLAG_INCLUDE_ALL_AVAILABLE_NODES = $1
  );

  // DEFINE_ENUM_FLAG_OPERATORS( D3D12_WORK_GRAPH_FLAGS )

  PD3D12_WORK_GRAPH_DESC = ^D3D12_WORK_GRAPH_DESC;
  D3D12_WORK_GRAPH_DESC = record
    ProgramName: PWideChar;
    Flags: D3D12_WORK_GRAPH_FLAGS;
    NumEntrypoints: UINT;
    pEntrypoints: PD3D12_NODE_ID;
    NumExplicitlyDefinedNodes: UINT;
    pExplicitlyDefinedNodes: PD3D12_NODE;
  end;


  PD3D12_GENERIC_PROGRAM_DESC = ^D3D12_GENERIC_PROGRAM_DESC;
  D3D12_GENERIC_PROGRAM_DESC = record
    ProgramName: PWideChar;
    NumExports: UINT;
    pExports: PPWideChar;
    NumSubobjects: UINT;
    ppSubobjects: PD3D12_STATE_SUBOBJECT;
  end;

  // ==

  PD3D12_STATE_OBJECT_TYPE = ^D3D12_STATE_OBJECT_TYPE;
  D3D12_STATE_OBJECT_TYPE = (D3D12_STATE_OBJECT_TYPE_COLLECTION              = 0,
                             D3D12_STATE_OBJECT_TYPE_RAYTRACING_PIPELINE     = 3,
                             D3D12_STATE_OBJECT_TYPE_EXECUTABLE              = 4
                            );


  PD3D12_STATE_OBJECT_DESC = ^D3D12_STATE_OBJECT_DESC;
  D3D12_STATE_OBJECT_DESC = record
    Type_: D3D12_STATE_OBJECT_TYPE;
    NumSubobjects: UINT;
    pSubobjects: PD3D12_STATE_SUBOBJECT;
  end;


  PD3D12_RAYTRACING_GEOMETRY_TYPE = ^D3D12_RAYTRACING_GEOMETRY_TYPE;
  D3D12_RAYTRACING_GEOMETRY_TYPE = (D3D12_RAYTRACING_GEOMETRY_TYPE_TRIANGLES,
                                    D3D12_RAYTRACING_GEOMETRY_TYPE_PROCEDURAL_PRIMITIVE_AABBS,
                                    { 10.0.28000.0 }
                                    D3D12_RAYTRACING_GEOMETRY_TYPE_OMM_TRIANGLES
                                   );


  PD3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE = ^D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;
  D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE = record
    StartAddress: D3D12_GPU_VIRTUAL_ADDRESS;
    StrideInBytes: UINT64;
  end;


  PD3D12_GPU_VIRTUAL_ADDRESS_RANGE = ^D3D12_GPU_VIRTUAL_ADDRESS_RANGE;
  D3D12_GPU_VIRTUAL_ADDRESS_RANGE = record
    StartAddress: D3D12_GPU_VIRTUAL_ADDRESS;
    SizeInBytes: UINT64;
  end;


  PD3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE = ^D3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE;
  D3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE = record
    StartAddress: D3D12_GPU_VIRTUAL_ADDRESS;
    SizeInBytes: UINT64;
    StrideInBytes: UINT64;
  end;


  PD3D12_RAYTRACING_GEOMETRY_TRIANGLES_DESC = ^D3D12_RAYTRACING_GEOMETRY_TRIANGLES_DESC;
  D3D12_RAYTRACING_GEOMETRY_TRIANGLES_DESC = record
    Transform3x4: D3D12_GPU_VIRTUAL_ADDRESS;
    IndexFormat: DXGI_FORMAT;
    VertexFormat: DXGI_FORMAT;
    IndexCount: UINT;
    VertexCount: UINT;
    IndexBuffer: D3D12_GPU_VIRTUAL_ADDRESS;
    VertexBuffer: D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;
  end;


  PD3D12_RAYTRACING_AABB = ^D3D12_RAYTRACING_AABB;
  D3D12_RAYTRACING_AABB = record
    MinX: FLOAT;
    MinY: FLOAT;
    MinZ: FLOAT;
    MaxX: FLOAT;
    MaxY: FLOAT;
    MaxZ: FLOAT;
  end;


  PD3D12_RAYTRACING_GEOMETRY_AABBS_DESC = ^D3D12_RAYTRACING_GEOMETRY_AABBS_DESC;
  D3D12_RAYTRACING_GEOMETRY_AABBS_DESC = record
    AABBCount: UINT64;
    AABBs: D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;
  end;

  { 10.0.28000.0 }
  D3D12_RAYTRACING_OPACITY_MICROMAP_SPECIAL_INDEX = (D3D12_RAYTRACING_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE = -4,
                                                     D3D12_RAYTRACING_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT = -3,
                                                     D3D12_RAYTRACING_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE = -2,
                                                     D3D12_RAYTRACING_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT = -1
                                                    );

  D3D12_RAYTRACING_OPACITY_MICROMAP_STATE = (D3D12_RAYTRACING_OPACITY_MICROMAP_STATE_TRANSPARENT = 0,
                                             D3D12_RAYTRACING_OPACITY_MICROMAP_STATE_OPAQUE = 1,
                                             D3D12_RAYTRACING_OPACITY_MICROMAP_STATE_UNKNOWN_TRANSPARENT = 2,
                                             D3D12_RAYTRACING_OPACITY_MICROMAP_STATE_UNKNOWN_OPAQUE = 3
                                            );

  D3D12_RAYTRACING_OPACITY_MICROMAP_FORMAT = (D3D12_RAYTRACING_OPACITY_MICROMAP_FORMAT_OC1_2_STATE = 1,
                                              D3D12_RAYTRACING_OPACITY_MICROMAP_FORMAT_OC1_4_STATE = 2
                                             );

  PD3D12_RAYTRACING_OPACITY_MICROMAP_DESC = ^D3D12_RAYTRACING_OPACITY_MICROMAP_DESC;
  D3D12_RAYTRACING_OPACITY_MICROMAP_DESC = record
    ByteOffset: UINT;
    SubdivisionLevel: Word;
    Format: Word;
  end;

  PD3D12_RAYTRACING_GEOMETRY_OMM_LINKAGE_DESC = ^D3D12_RAYTRACING_GEOMETRY_OMM_LINKAGE_DESC;
  D3D12_RAYTRACING_GEOMETRY_OMM_LINKAGE_DESC = record
    OpacityMicromapIndexBuffer: D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;
    OpacityMicromapIndexFormat: DXGI_FORMAT;
    OpacityMicromapBaseLocation: UINT;
    OpacityMicromapArray: D3D12_GPU_VIRTUAL_ADDRESS;
  end;

  PD3D12_RAYTRACING_GEOMETRY_OMM_TRIANGLES_DESC = ^D3D12_RAYTRACING_GEOMETRY_OMM_TRIANGLES_DESC;
  D3D12_RAYTRACING_GEOMETRY_OMM_TRIANGLES_DESC = record
    pTriangles: PD3D12_RAYTRACING_GEOMETRY_TRIANGLES_DESC;
    pOmmLinkage: PD3D12_RAYTRACING_GEOMETRY_OMM_LINKAGE_DESC;
  end;


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE = (D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE_CLONE                          = $0,
                                                       D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE_COMPACT                        = $1,
                                                       D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE_VISUALIZATION_DECODE_FOR_TOOLS = $2,
                                                       D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE_SERIALIZE                      = $3,
                                                       D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE_DESERIALIZE                    = $4
                                                      );


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE = (D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL              = $0,
                                                  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL           = $1,
                                                  { 10.0.28000.0 }
                                                  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE_OPACITY_MICROMAP_ARRAY = $2
                                                 );


  PD3D12_ELEMENTS_LAYOUT = ^D3D12_ELEMENTS_LAYOUT;
  D3D12_ELEMENTS_LAYOUT                     = (
    D3D12_ELEMENTS_LAYOUT_ARRAY             = $0,
    D3D12_ELEMENTS_LAYOUT_ARRAY_OF_POINTERS = $1
  );


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TYPE = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TYPE;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TYPE = (D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_COMPACTED_SIZE      = $0,
                                                                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TOOLS_VISUALIZATION = $1,
                                                                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_SERIALIZATION       = $2,
                                                                 D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_CURRENT_SIZE        = $3
                                                                );


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_DESC = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_DESC;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_DESC = record
    DestBuffer: D3D12_GPU_VIRTUAL_ADDRESS;
    InfoType: D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TYPE;
  end;


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TOOLS_VISUALIZATION_DESC = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TOOLS_VISUALIZATION_DESC;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_TOOLS_VISUALIZATION_DESC = record
    DecodedSizeInBytes: UINT64;
  end;


  PD3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_TOOLS_VISUALIZATION_HEADER = ^D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_TOOLS_VISUALIZATION_HEADER;
  D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_TOOLS_VISUALIZATION_HEADER = record
    Type_: D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE;
    NumDescs: UINT;
  end;


  // Regarding D3D12_BUILD_RAY_TRACING_ACCELERATION_STRUCTURE_TOOLS_VISUALIZATION_HEADER above,
  // depending on Type field, NumDescs above is followed by either:
  //       D3D12_RAY_TRACING_INSTANCE_DESC InstanceDescs[NumDescs]
  //    or D3D12_RAY_TRACING_GEOMETRY_DESC GeometryDescs[NumDescs].
  // There is 4 bytes of padding between GeometryDesc structs in the array so alignment is natural when viewed by CPU.


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_SERIALIZATION_DESC = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_SERIALIZATION_DESC;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_SERIALIZATION_DESC = record
    SerializedSizeInBytes: UINT64;
    NumBottomLevelAccelerationStructurePointers: UINT64;
  end;


  PD3D12_SERIALIZED_DATA_DRIVER_MATCHING_IDENTIFIER = ^D3D12_SERIALIZED_DATA_DRIVER_MATCHING_IDENTIFIER;
  D3D12_SERIALIZED_DATA_DRIVER_MATCHING_IDENTIFIER = record
    DriverOpaqueGUID: TGUID;
    DriverOpaqueVersioningData: array[0..15] of Byte;
  end;


  PD3D12_SERIALIZED_DATA_TYPE = ^D3D12_SERIALIZED_DATA_TYPE;
  D3D12_SERIALIZED_DATA_TYPE = (D3D12_SERIALIZED_DATA_RAYTRACING_ACCELERATION_STRUCTURE = $0,
                                { 10.0.28000.0  }
                                D3D12_SERIALIZED_DATA_APPLICATION_SPECIFIC_DRIVER_STATE  = $1
                               );


  PD3D12_DRIVER_MATCHING_IDENTIFIER_STATUS = ^D3D12_DRIVER_MATCHING_IDENTIFIER_STATUS;
  D3D12_DRIVER_MATCHING_IDENTIFIER_STATUS = (D3D12_DRIVER_MATCHING_IDENTIFIER_COMPATIBLE_WITH_DEVICE = $0,
                                             D3D12_DRIVER_MATCHING_IDENTIFIER_UNSUPPORTED_TYPE       = $1,
                                             D3D12_DRIVER_MATCHING_IDENTIFIER_UNRECOGNIZED           = $2,
                                             D3D12_DRIVER_MATCHING_IDENTIFIER_INCOMPATIBLE_VERSION   = $3,
                                             D3D12_DRIVER_MATCHING_IDENTIFIER_INCOMPATIBLE_TYPE      = $4
                                            );


  PD3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER = ^D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER;
  D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER = record
    DriverMatchingIdentifier: D3D12_SERIALIZED_DATA_DRIVER_MATCHING_IDENTIFIER;
    SerializedSizeInBytesIncludingHeader: UINT64;
    DeserializedSizeInBytes: UINT64;
    NumBottomLevelAccelerationStructurePointersAfterHeader: UINT64;
  end;

  { 10.0.28000.0 }
  D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER_POSTAMBLE_TYPE = UINT;
  const
    D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER_POSTAMBLE_TYPE_NONE = 0;
    D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER_POSTAMBLE_TYPE_BOTTOM_LEVEL_POINTERS = 0;
    D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER_POSTAMBLE_TYPE_BLOCKS = $FFFFFFFF;

  type
  PD3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER1 = ^D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER1;
  D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER1 = record
    DriverMatchingIdentifier: D3D12_SERIALIZED_DATA_DRIVER_MATCHING_IDENTIFIER;
    SerializedSizeInBytesIncludingHeader: UINT64;
    DeserializedSizeInBytes: UINT64;
    NumBottomLevelAccelerationStructurePointersAfterHeaderOrNumBlocks: UINT;
    HeaderPostambleType: D3D12_SERIALIZED_RAYTRACING_ACCELERATION_STRUCTURE_HEADER_POSTAMBLE_TYPE;
  end;

  D3D12_RAYTRACING_SERIALIZED_BLOCK_TYPE = (D3D12_RAYTRACING_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAPS = 0
                                           );
  D3D12_RAYTRACING_SERIALIZED_BLOCK = record
    Type_: D3D12_RAYTRACING_SERIALIZED_BLOCK_TYPE;
    NumBlockPointersAfterHeader: UINT64;
  end;


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_CURRENT_SIZE_DESC = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_CURRENT_SIZE_DESC;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_CURRENT_SIZE_DESC = record
    CurrentSizeInBytes: UINT64;
  end;


   D3D12_RAYTRACING_INSTANCE_DESC = record
   public
     Transform: array[0..2,0..3] of FLOAT;
   private
     Flags_: UINT64;
     function GetBits(const aIndex: Integer): Integer;
     procedure SetBits(const aIndex: Integer; const aValue: Integer);
   public
     AccelerationStructure: D3D12_GPU_VIRTUAL_ADDRESS;
     property InstanceID: Integer index $0018 read GetBits write SetBits;
     property InstanceMask: Integer index $1808 read GetBits write SetBits;
     property InstanceContributionToHitGroupIndex: Integer index $2018 read GetBits write SetBits;
     property Flags: Integer index $3808 read GetBits write SetBits;
   end;                                                                                            //                   64


  PPD3D12_RAYTRACING_GEOMETRY_DESC = ^PD3D12_RAYTRACING_GEOMETRY_DESC;
  PD3D12_RAYTRACING_GEOMETRY_DESC = ^D3D12_RAYTRACING_GEOMETRY_DESC;
  D3D12_RAYTRACING_GEOMETRY_DESC = record
    Type_: D3D12_RAYTRACING_GEOMETRY_TYPE;
    Flags: D3D12_RAYTRACING_GEOMETRY_FLAGS;
    case Integer of
      0:  (Triangles: D3D12_RAYTRACING_GEOMETRY_TRIANGLES_DESC);
      1:  (AABBs: D3D12_RAYTRACING_GEOMETRY_AABBS_DESC);
      { 10.0.28000.0  }
      2:  (OmmTriangles: D3D12_RAYTRACING_GEOMETRY_OMM_TRIANGLES_DESC);
  end;

  { 10.0.28000.0  }
  PD3D12_RAYTRACING_OPACITY_MICROMAP_HISTOGRAM_ENTRY = ^D3D12_RAYTRACING_OPACITY_MICROMAP_HISTOGRAM_ENTRY;
  D3D12_RAYTRACING_OPACITY_MICROMAP_HISTOGRAM_ENTRY = record
    Count: UINT;
    SubdivisionLevel: UINT;
    Format: D3D12_RAYTRACING_OPACITY_MICROMAP_FORMAT;
  end;

  PD3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_DESC = ^D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_DESC;
  D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_DESC = record
    NumOmmHistogramEntries: UINT;
    pOmmHistogram: PD3D12_RAYTRACING_OPACITY_MICROMAP_HISTOGRAM_ENTRY;
    InputBuffer: D3D12_GPU_VIRTUAL_ADDRESS;
    PerOmmDescs: D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;
  end;


  PD3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_INPUTS =^D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_INPUTS;
  D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_INPUTS = record
    Type_: D3D12_RAYTRACING_ACCELERATION_STRUCTURE_TYPE;
    Flags: D3D12_RAYTRACING_ACCELERATION_STRUCTURE_BUILD_FLAGS;
    NumDescs: UINT;
    DescsLayout: D3D12_ELEMENTS_LAYOUT;
    case Integer of
      0:    (InstanceDescs: D3D12_GPU_VIRTUAL_ADDRESS);
      1:    (pGeometryDescs: PD3D12_RAYTRACING_GEOMETRY_DESC);
      2:    (ppGeometryDescs: PPD3D12_RAYTRACING_GEOMETRY_DESC);
      { 10.0.28000.0 }
      3:    (pOpacityMicromapArrayDesc: PD3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_DESC);
  end;


  PD3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_DESC = ^D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_DESC;
  D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_DESC = record
    DestAccelerationStructureData: D3D12_GPU_VIRTUAL_ADDRESS;
    Inputs: D3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_INPUTS;
    {opt} SourceAccelerationStructureData: PD3D12_GPU_VIRTUAL_ADDRESS;
    ScratchAccelerationStructureData: D3D12_GPU_VIRTUAL_ADDRESS;
  end;


  PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_PREBUILD_INFO = ^D3D12_RAYTRACING_ACCELERATION_STRUCTURE_PREBUILD_INFO;
  D3D12_RAYTRACING_ACCELERATION_STRUCTURE_PREBUILD_INFO = record
    ResultDataMaxSizeInBytes: UINT64;
    ScratchDataSizeInBytes: UINT64;
    UpdateScratchDataSizeInBytes: UINT64;
  end;

  { 10.0.28000.0  }
  D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_TYPE = (D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_CURRENT_SIZE = 0,
                                                                 D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_TOOLS_VISUALIZATION = 1
                                                                );
  D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_DESC = record
    DestBuffer: D3D12_GPU_VIRTUAL_ADDRESS;
    InfoType: D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_TYPE;
  end;
  D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_CURRENT_SIZE_DESC = record
    CurrentSizeInBytes: UINT64;
  end;
  D3D12_RAYTRACING_OPACITY_MICROMAP_ARRAY_POSTBUILD_INFO_TOOLS_VISUALIZATION_DESC = record
    DecodedSizeInBytes: UINT64;
  end;


  PD3D12_HIT_KIND = ^D3D12_HIT_KIND;
  D3D12_HIT_KIND = (D3D12_HIT_KIND_TRIANGLE_FRONT_FACE = $FE,
                    D3D12_HIT_KIND_TRIANGLE_BACK_FACE  = $FF
                   );


  // Interface ID3D12Device5
  // =======================
  //
  ID3D12Device5 = interface(ID3D12Device4)
    ['{8B4F173B-2FEA-4B80-8F58-4307191AB95D}']

    function CreateLifetimeTracker(pOwner: ID3D12LifetimeOwner;
                                   const riid: TGUID;
                                   ppvTracker: PPointer): HRESULT; stdcall;

    procedure RemoveDevice(); stdcall;

    function EnumerateMetaCommands(pNumMetaCommands: PUINT;
                                   pDescs: PD3D12_META_COMMAND_DESC): HRESULT; stdcall;

    function EnumerateMetaCommandParameters(const CommandId: TGUID;
                                            Stage: D3D12_META_COMMAND_PARAMETER_STAGE;
                                            pTotalStructureSizeInBytes: PUINT;
                                            pParameterCount: PUINT;
                                            pParameterDescs: PD3D12_META_COMMAND_PARAMETER_DESC): HRESULT; stdcall;

    function CreateMetaCommand(const CommandId: TGUID;
                               NodeMask: UINT;
                               pCreationParametersData: Pointer;
                               CreationParametersDataSizeInBytes: SIZE_T;
                               const riid: TGUID;
                               ppMetaCommand: PPointer): HRESULT; stdcall;

    function CreateStateObject(pDesc: PD3D12_STATE_OBJECT_DESC;
                               const riid: TGUID;
                               ppStateObject: PPointer): HRESULT; stdcall;

    procedure GetRaytracingAccelerationStructurePrebuildInfo(pDesc: PD3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_INPUTS;
                                                             pInfo: PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_PREBUILD_INFO); stdcall;

    function CheckDriverMatchingIdentifier(SerializedDataType: D3D12_SERIALIZED_DATA_TYPE;
                                           pIdentifierToCheck: PD3D12_SERIALIZED_DATA_DRIVER_MATCHING_IDENTIFIER): D3D12_DRIVER_MATCHING_IDENTIFIER_STATUS; stdcall;
  end;
  IID_ID3D12Device5 = ID3D12Device5;


  PD3D12_AUTO_BREADCRUMB_OP = ^D3D12_AUTO_BREADCRUMB_OP;
  D3D12_AUTO_BREADCRUMB_OP = (D3D12_AUTO_BREADCRUMB_OP_SETMARKER                                        = 0,
                              D3D12_AUTO_BREADCRUMB_OP_BEGINEVENT                                       = 1,
                              D3D12_AUTO_BREADCRUMB_OP_ENDEVENT                                         = 2,
                              D3D12_AUTO_BREADCRUMB_OP_DRAWINSTANCED                                    = 3,
                              D3D12_AUTO_BREADCRUMB_OP_DRAWINDEXEDINSTANCED                             = 4,
                              D3D12_AUTO_BREADCRUMB_OP_EXECUTEINDIRECT                                  = 5,
                              D3D12_AUTO_BREADCRUMB_OP_DISPATCH                                         = 6,
                              D3D12_AUTO_BREADCRUMB_OP_COPYBUFFERREGION                                 = 7,
                              D3D12_AUTO_BREADCRUMB_OP_COPYTEXTUREREGION                                = 8,
                              D3D12_AUTO_BREADCRUMB_OP_COPYRESOURCE                                     = 9,
                              D3D12_AUTO_BREADCRUMB_OP_COPYTILES                                        = 10,
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVESUBRESOURCE                               = 11,
                              D3D12_AUTO_BREADCRUMB_OP_CLEARRENDERTARGETVIEW                            = 12,
                              D3D12_AUTO_BREADCRUMB_OP_CLEARUNORDEREDACCESSVIEW                         = 13,
                              D3D12_AUTO_BREADCRUMB_OP_CLEARDEPTHSTENCILVIEW                            = 14,
                              D3D12_AUTO_BREADCRUMB_OP_RESOURCEBARRIER                                  = 15,
                              D3D12_AUTO_BREADCRUMB_OP_EXECUTEBUNDLE                                    = 16,
                              D3D12_AUTO_BREADCRUMB_OP_PRESENT                                          = 17,
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVEQUERYDATA                                 = 18,
                              D3D12_AUTO_BREADCRUMB_OP_BEGINSUBMISSION                                  = 19,
                              D3D12_AUTO_BREADCRUMB_OP_ENDSUBMISSION                                    = 20,
                              D3D12_AUTO_BREADCRUMB_OP_DECODEFRAME                                      = 21,
                              D3D12_AUTO_BREADCRUMB_OP_PROCESSFRAMES                                    = 22,
                              D3D12_AUTO_BREADCRUMB_OP_ATOMICCOPYBUFFERUINT                             = 23,
                              D3D12_AUTO_BREADCRUMB_OP_ATOMICCOPYBUFFERUINT64                           = 24,
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVESUBRESOURCEREGION                         = 25,
                              D3D12_AUTO_BREADCRUMB_OP_WRITEBUFFERIMMEDIATE                             = 26,
                              D3D12_AUTO_BREADCRUMB_OP_DECODEFRAME1                                     = 27,
                              D3D12_AUTO_BREADCRUMB_OP_SETPROTECTEDRESOURCESESSION                      = 28,
                              D3D12_AUTO_BREADCRUMB_OP_DECODEFRAME2                                     = 29,
                              D3D12_AUTO_BREADCRUMB_OP_PROCESSFRAMES1                                   = 30,
                              D3D12_AUTO_BREADCRUMB_OP_BUILDRAYTRACINGACCELERATIONSTRUCTURE             = 31,
                              D3D12_AUTO_BREADCRUMB_OP_EMITRAYTRACINGACCELERATIONSTRUCTUREPOSTBUILDINFO = 32,
                              D3D12_AUTO_BREADCRUMB_OP_COPYRAYTRACINGACCELERATIONSTRUCTURE              = 33,
                              D3D12_AUTO_BREADCRUMB_OP_DISPATCHRAYS                                     = 34,
                              D3D12_AUTO_BREADCRUMB_OP_INITIALIZEMETACOMMAND                            = 35,
                              D3D12_AUTO_BREADCRUMB_OP_EXECUTEMETACOMMAND                               = 36,
                              D3D12_AUTO_BREADCRUMB_OP_ESTIMATEMOTION                                   = 37,
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVEMOTIONVECTORHEAP                          = 38,
                              D3D12_AUTO_BREADCRUMB_OP_SETPIPELINESTATE1                                = 39,
                              D3D12_AUTO_BREADCRUMB_OP_INITIALIZEEXTENSIONCOMMAND                       = 40,
                              D3D12_AUTO_BREADCRUMB_OP_EXECUTEEXTENSIONCOMMAND                          = 41,
                              D3D12_AUTO_BREADCRUMB_OP_DISPATCHMESH                                     = 42,
                              D3D12_AUTO_BREADCRUMB_OP_ENCODEFRAME                                      = 43,
                              // Update May 2024
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVEENCODEROUTPUTMETADATA                     = 44,
                              D3D12_AUTO_BREADCRUMB_OP_BARRIER                                          = 45,
                              D3D12_AUTO_BREADCRUMB_OP_BEGIN_COMMAND_LIST                               = 46,
                              D3D12_AUTO_BREADCRUMB_OP_DISPATCHGRAPH                                    = 47,
                              D3D12_AUTO_BREADCRUMB_OP_SETPROGRAM                                       = 48,
                              { 10.0.28000.0 }
                              D3D12_AUTO_BREADCRUMB_OP_ENCODEFRAME1                                     = 49,
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVEENCODEROUTPUTMETADATA1                    = 50,
                              D3D12_AUTO_BREADCRUMB_OP_RESOLVEINPUTPARAMLAYOUT                          = 51,
                              D3D12_AUTO_BREADCRUMB_OP_PROCESSFRAMES2                                   = 52,
                              D3D12_AUTO_BREADCRUMB_OP_SET_WORK_GRAPH_MAXIMUM_GPU_INPUT_RECORDS         = 53
                              // ==
                             );

  { 10.0.28000.0 }
  D3D12_MARKER_API = D3D12_AUTO_BREADCRUMB_OP;
  const
    D3D12_MARKER_API_SETMARKER = D3D12_MARKER_API(0);
    D3D12_MARKER_API_BEGINEVENT = D3D12_MARKER_API(1);
    D3D12_MARKER_API_ENDEVENT = D3D12_MARKER_API(2);
    D3D12_MARKER_API_DRAWINSTANCED = D3D12_MARKER_API(3);
    D3D12_MARKER_API_DRAWINDEXEDINSTANCED = D3D12_MARKER_API(4);
    D3D12_MARKER_API_EXECUTEINDIRECT = D3D12_MARKER_API(5);
    D3D12_MARKER_API_DISPATCH = D3D12_MARKER_API(6);
    D3D12_MARKER_API_COPYBUFFERREGION = D3D12_MARKER_API(7);
    D3D12_MARKER_API_COPYTEXTUREREGION = D3D12_MARKER_API(8);
    D3D12_MARKER_API_COPYRESOURCE = D3D12_MARKER_API(9);
    D3D12_MARKER_API_COPYTILES = D3D12_MARKER_API(10);
    D3D12_MARKER_API_RESOLVESUBRESOURCE = D3D12_MARKER_API(11);
    D3D12_MARKER_API_CLEARRENDERTARGETVIEW = D3D12_MARKER_API(12);
    D3D12_MARKER_API_CLEARUNORDEREDACCESSVIEW = D3D12_MARKER_API(13);
    D3D12_MARKER_API_CLEARDEPTHSTENCILVIEW = D3D12_MARKER_API(14);
    D3D12_MARKER_API_RESOURCEBARRIER = D3D12_MARKER_API(15);
    D3D12_MARKER_API_EXECUTEBUNDLE = D3D12_MARKER_API(16);
    D3D12_MARKER_API_PRESENT = D3D12_MARKER_API(17);
    D3D12_MARKER_API_RESOLVEQUERYDATA = D3D12_MARKER_API(18);
    D3D12_MARKER_API_BEGINSUBMISSION = D3D12_MARKER_API(19);
    D3D12_MARKER_API_ENDSUBMISSION = D3D12_MARKER_API(20);
    D3D12_MARKER_API_DECODEFRAME = D3D12_MARKER_API(21);
    D3D12_MARKER_API_PROCESSFRAMES = D3D12_MARKER_API(22);
    D3D12_MARKER_API_ATOMICCOPYBUFFERUINT = D3D12_MARKER_API(23);
    D3D12_MARKER_API_ATOMICCOPYBUFFERUINT64 = D3D12_MARKER_API(24);
    D3D12_MARKER_API_RESOLVESUBRESOURCEREGION = D3D12_MARKER_API(25);
    D3D12_MARKER_API_WRITEBUFFERIMMEDIATE = D3D12_MARKER_API(26);
    D3D12_MARKER_API_DECODEFRAME1 = D3D12_MARKER_API(27);
    D3D12_MARKER_API_SETPROTECTEDRESOURCESESSION = D3D12_MARKER_API(28);
    D3D12_MARKER_API_DECODEFRAME2 = D3D12_MARKER_API(29);
    D3D12_MARKER_API_PROCESSFRAMES1 = D3D12_MARKER_API(30);
    D3D12_MARKER_API_BUILDRAYTRACINGACCELERATIONSTRUCTURE = D3D12_MARKER_API(31);
    D3D12_MARKER_API_EMITRAYTRACINGACCELERATIONSTRUCTUREPOSTBUILDINFO = D3D12_MARKER_API(32);
    D3D12_MARKER_API_COPYRAYTRACINGACCELERATIONSTRUCTURE = D3D12_MARKER_API(33);
    D3D12_MARKER_API_DISPATCHRAYS = D3D12_MARKER_API(34);
    D3D12_MARKER_API_INITIALIZEMETACOMMAND = D3D12_MARKER_API(35);
    D3D12_MARKER_API_EXECUTEMETACOMMAND = D3D12_MARKER_API(36);
    D3D12_MARKER_API_ESTIMATEMOTION = D3D12_MARKER_API(37);
    D3D12_MARKER_API_RESOLVEMOTIONVECTORHEAP = D3D12_MARKER_API(38);
    D3D12_MARKER_API_SETPIPELINESTATE1 = D3D12_MARKER_API(39);
    D3D12_MARKER_API_INITIALIZEEXTENSIONCOMMAND = D3D12_MARKER_API(40);
    D3D12_MARKER_API_EXECUTEEXTENSIONCOMMAND = D3D12_MARKER_API(41);
    D3D12_MARKER_API_DISPATCHMESH = D3D12_MARKER_API(42);
    D3D12_MARKER_API_ENCODEFRAME = D3D12_MARKER_API(43);
    D3D12_MARKER_API_RESOLVEENCODEROUTPUTMETADATA = D3D12_MARKER_API(44);
    D3D12_MARKER_API_BARRIER = D3D12_MARKER_API(45);
    D3D12_MARKER_API_BEGIN_COMMAND_LIST = D3D12_MARKER_API(46);
    D3D12_MARKER_API_DISPATCHGRAPH = D3D12_MARKER_API(47);
    D3D12_MARKER_API_SETPROGRAM = D3D12_MARKER_API(48);
    D3D12_MARKER_API_ENCODEFRAME1 = D3D12_MARKER_API(49);
    D3D12_MARKER_API_RESOLVEENCODEROUTPUTMETADATA1 = D3D12_MARKER_API(50);
    D3D12_MARKER_API_RESOLVEINPUTPARAMLAYOUT = D3D12_MARKER_API(51);
    D3D12_MARKER_API_PROCESSFRAMES2 = D3D12_MARKER_API(52);
    D3D12_MARKER_API_SET_WORK_GRAPH_MAXIMUM_GPU_INPUT_RECORDS = D3D12_MARKER_API(53);
  type


  PD3D12_AUTO_BREADCRUMB_NODE = ^D3D12_AUTO_BREADCRUMB_NODE;
  D3D12_AUTO_BREADCRUMB_NODE = record
    pCommandListDebugNameA: PChar;
    pCommandListDebugNameW: Pwchar_t;
    pCommandQueueDebugNameA: PChar;
    pCommandQueueDebugNameW: Pwchar_t;
    pCommandList: ID3D12GraphicsCommandList;
    pCommandQueue: ID3D12CommandQueue;
    BreadcrumbCount: UINT32;
    pLastBreadcrumbValue: PUINT32;
    pCommandHistory: PD3D12_AUTO_BREADCRUMB_OP;
    pNext: PD3D12_AUTO_BREADCRUMB_NODE;  // pointer to next node
  end;


  PD3D12_DRED_BREADCRUMB_CONTEXT = ^D3D12_DRED_BREADCRUMB_CONTEXT;
  D3D12_DRED_BREADCRUMB_CONTEXT = record
    BreadcrumbIndex: UINT;
    pContextString: Pwchar_t;
  end;


  // !!! Must be castable to D3D12_AUTO_BREADCRUMB_NODE
  PD3D12_AUTO_BREADCRUMB_NODE1 = ^D3D12_AUTO_BREADCRUMB_NODE1;
  D3D12_AUTO_BREADCRUMB_NODE1 = record
    pCommandListDebugNameA: PChar;
    pCommandListDebugNameW: Pwchar_t;
    pCommandQueueDebugNameA: PChar;
    pCommandQueueDebugNameW: Pwchar_t;
    pCommandList: ID3D12GraphicsCommandList;
    pCommandQueue: ID3D12CommandQueue;
    BreadcrumbCount: UINT;
    pLastBreadcrumbValue: PUINT;
    pCommandHistory: PD3D12_AUTO_BREADCRUMB_OP;
    pNext: PD3D12_AUTO_BREADCRUMB_NODE1;  // pointer to next node
    BreadcrumbContextsCount: UINT;
    pBreadcrumbContexts: PD3D12_DRED_BREADCRUMB_CONTEXT;
  end;


  // D3D12_DRED_VERSION
  PD3D12_DRED_VERSION = ^D3D12_DRED_VERSION;
  D3D12_DRED_VERSION = (D3D12_DRED_VERSION_1_0 = $1,
                        D3D12_DRED_VERSION_1_1 = $2,
                        D3D12_DRED_VERSION_1_2 = $3,
                        D3D12_DRED_VERSION_1_3 = $4
                       );


  PD3D12_DRED_ENABLEMENT = ^D3D12_DRED_ENABLEMENT;
  D3D12_DRED_ENABLEMENT = (D3D12_DRED_ENABLEMENT_SYSTEM_CONTROLLED = 0,
                           D3D12_DRED_ENABLEMENT_FORCED_OFF        = 1,
                           D3D12_DRED_ENABLEMENT_FORCED_ON         = 2
                          );


  PD3D12_DEVICE_REMOVED_EXTENDED_DATA = ^D3D12_DEVICE_REMOVED_EXTENDED_DATA;
  D3D12_DEVICE_REMOVED_EXTENDED_DATA = record
    {in} Flags: D3D12_DRED_FLAGS;                                // Dred flags
    {out} pHeadAutoBreadcrumbNode: PD3D12_AUTO_BREADCRUMB_NODE;  // Pointer to head of a linked list of auto-breadcrumb data
  end;


  PD3D12_DRED_ALLOCATION_NODE = ^D3D12_DRED_ALLOCATION_NODE;
  D3D12_DRED_ALLOCATION_NODE = record
    ObjectNameA: PAnsiChar;
    ObjectNameW: PWideChar;
    AllocationType: D3D12_DRED_ALLOCATION_TYPE;
    pNext: PD3D12_DRED_ALLOCATION_NODE;
  end;


  PD3D12_DRED_ALLOCATION_NODE1 = ^D3D12_DRED_ALLOCATION_NODE1;
  D3D12_DRED_ALLOCATION_NODE1 = record
    ObjectNameA: PChar;
    ObjectNameW: PWideChar;
    AllocationType: D3D12_DRED_ALLOCATION_TYPE;
    pNext: PD3D12_DRED_ALLOCATION_NODE1;
    pObject: PIUnknown;
  end;


  PD3D12_DRED_AUTO_BREADCRUMBS_OUTPUT = ^D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT;
  D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT = record
    //[annotation("_Out_")]
    pHeadAutoBreadcrumbNode: PD3D12_AUTO_BREADCRUMB_NODE;  // Pointer to head of a null-terminated linked list of auto-breadcrumb data
  end;


  PD3D12_DRED_AUTO_BREADCRUMBS_OUTPUT1 = ^D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT1;
  D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT1 = record
    // [annotation("_Out_")]
    pHeadAutoBreadcrumbNode: PD3D12_AUTO_BREADCRUMB_NODE1;  // Pointer to head of a null-terminated linked list of auto-breadcrumb data
  end;


  PD3D12_DRED_PAGE_FAULT_OUTPUT = ^D3D12_DRED_PAGE_FAULT_OUTPUT;
  D3D12_DRED_PAGE_FAULT_OUTPUT = record
    PageFaultVA: D3D12_GPU_VIRTUAL_ADDRESS;  //If the device removal was due to GPU Fault, this contains the VA of the faulting op
    // [annotation("_Out_")]
    pHeadExistingAllocationNode: PD3D12_DRED_ALLOCATION_NODE;
    // [annotation("_Out_")]
    pHeadRecentFreedAllocationNode: PD3D12_DRED_ALLOCATION_NODE;
  end;


  PD3D12_DRED_PAGE_FAULT_OUTPUT1 = ^D3D12_DRED_PAGE_FAULT_OUTPUT1;
  D3D12_DRED_PAGE_FAULT_OUTPUT1 = record
    PageFaultVA: D3D12_GPU_VIRTUAL_ADDRESS;  //If the device removal was due to GPU Fault, this contains the VA of the faulting op
    // [annotation("_Out_")]
    pHeadExistingAllocationNode: PD3D12_DRED_ALLOCATION_NODE1;
    // [annotation("_Out_")]
    pHeadRecentFreedAllocationNode: PD3D12_DRED_ALLOCATION_NODE1;
  end;


  PD3D12_DRED_DEVICE_STATE = ^D3D12_DRED_DEVICE_STATE;
  D3D12_DRED_DEVICE_STATE = (D3D12_DRED_DEVICE_STATE_UNKNOWN   = 0,  // Only known if DRED event occurred
                             D3D12_DRED_DEVICE_STATE_HUNG      = 3,
                             D3D12_DRED_DEVICE_STATE_FAULT     = 6,
                             D3D12_DRED_DEVICE_STATE_PAGEFAULT = 7
                            );


  PD3D12_DRED_PAGE_FAULT_OUTPUT2 = ^D3D12_DRED_PAGE_FAULT_OUTPUT2;
  D3D12_DRED_PAGE_FAULT_OUTPUT2 = record
    PageFaultVA: D3D12_GPU_VIRTUAL_ADDRESS;  //If the device removal was due to GPU Fault, this contains the VA of the faulting op
    // [annotation("_Out_")]
    pHeadExistingAllocationNode: PD3D12_DRED_ALLOCATION_NODE1;
    // [annotation("_Out_")]
    pHeadRecentFreedAllocationNode: PD3D12_DRED_ALLOCATION_NODE1;
    PageFaultFlags: D3D12_DRED_PAGE_FAULT_FLAGS;
  end;


  PD3D12_DEVICE_REMOVED_EXTENDED_DATA1 = ^D3D12_DEVICE_REMOVED_EXTENDED_DATA1;
  D3D12_DEVICE_REMOVED_EXTENDED_DATA1 = record
    DeviceRemovedReason: HResult;
    AutoBreadcrumbsOutput: D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT;
    PageFaultOutput: D3D12_DRED_PAGE_FAULT_OUTPUT;
  end;


  PD3D12_DEVICE_REMOVED_EXTENDED_DATA2 = ^D3D12_DEVICE_REMOVED_EXTENDED_DATA2;
  D3D12_DEVICE_REMOVED_EXTENDED_DATA2 = record
    DeviceRemovedReason: HResult;
    AutoBreadcrumbsOutput: D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT1;
    PageFaultOutput: D3D12_DRED_PAGE_FAULT_OUTPUT1;
  end;


  PD3D12_DEVICE_REMOVED_EXTENDED_DATA3 = ^D3D12_DEVICE_REMOVED_EXTENDED_DATA3;
  D3D12_DEVICE_REMOVED_EXTENDED_DATA3 = record
    DeviceRemovedReason: HResult;
    AutoBreadcrumbsOutput: D3D12_DRED_AUTO_BREADCRUMBS_OUTPUT1;
    PageFaultOutput: D3D12_DRED_PAGE_FAULT_OUTPUT2;
    DeviceState: D3D12_DRED_DEVICE_STATE;
  end;


  PD3D12_VERSIONED_DEVICE_REMOVED_EXTENDED_DATA = ^D3D12_VERSIONED_DEVICE_REMOVED_EXTENDED_DATA;
  D3D12_VERSIONED_DEVICE_REMOVED_EXTENDED_DATA = record
    Version: D3D12_DRED_VERSION;  // Version of the DRED data
    case Integer of
      0:  (Dred_1_0: D3D12_DEVICE_REMOVED_EXTENDED_DATA);
      1:  (Dred_1_1: D3D12_DEVICE_REMOVED_EXTENDED_DATA1);
      2:  (Dred_1_2: D3D12_DEVICE_REMOVED_EXTENDED_DATA2);
      3:  (Dred_1_3: D3D12_DEVICE_REMOVED_EXTENDED_DATA3);
  end;


  // Interface ID3D12DeviceRemovedExtendedDataSettings
  // =================================================
  //
  ID3D12DeviceRemovedExtendedDataSettings = interface(IUnknown)
    ['{82BC481C-6B9B-4030-AEDB-7EE3D1DF1E63}']

    procedure SetAutoBreadcrumbsEnablement(Enablement: D3D12_DRED_ENABLEMENT); stdcall;

    procedure SetPageFaultEnablement(Enablement: D3D12_DRED_ENABLEMENT); stdcall;

    procedure SetWatsonDumpEnablement(Enablement: D3D12_DRED_ENABLEMENT); stdcall;
  end;
  IID_ID3D12DeviceRemovedExtendedDataSettings = ID3D12DeviceRemovedExtendedDataSettings;


  // Interface ID3D12DeviceRemovedExtendedDataSettings1
  // ==================================================
  //
  ID3D12DeviceRemovedExtendedDataSettings1 = interface(ID3D12DeviceRemovedExtendedDataSettings)
    ['{DBD5AE51-3317-4F0A-ADF9-1D7CEDCAAE0B}']

    procedure SetBreadcrumbContextEnablement(Enablement: D3D12_DRED_ENABLEMENT); stdcall;
  end;
  IID_ID3D12DeviceRemovedExtendedDataSettings1 = ID3D12DeviceRemovedExtendedDataSettings1;


  // Update May 2024

  // Interface ID3D12DeviceRemovedExtendedDataSettings2
  // ==================================================
  //
  ID3D12DeviceRemovedExtendedDataSettings2 = interface(ID3D12DeviceRemovedExtendedDataSettings1)
    ['{61552388-01AB-4008-A436-83DB189566EA}']

    procedure UseMarkersOnlyAutoBreadcrumbs(MarkersOnly: BOOL); stdcall;
  end;
  IID_ID3D12DeviceRemovedExtendedDataSettings2 = ID3D12DeviceRemovedExtendedDataSettings2;

  // ==

  // Interface ID3D12DeviceRemovedExtendedData
  // =========================================
  //
  ID3D12DeviceRemovedExtendedData = interface(IUnknown)
    ['{98931D33-5AE8-4791-AA3C-1A73A2934E71}']

    function GetAutoBreadcrumbsOutput(pOutput: PD3D12_DRED_AUTO_BREADCRUMBS_OUTPUT): HRESULT; stdcall;

    function GetPageFaultAllocationOutput(pOutput: PD3D12_DRED_PAGE_FAULT_OUTPUT): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceRemovedExtendedData = ID3D12DeviceRemovedExtendedData;


  // Interface ID3D12DeviceRemovedExtendedData1
  // ==========================================
  //
  ID3D12DeviceRemovedExtendedData1 = interface(ID3D12DeviceRemovedExtendedData)
    ['{9727A022-CF1D-4DDA-9EBA-EFFA653FC506}']

    function GetAutoBreadcrumbsOutput1(pOutput: PD3D12_DRED_AUTO_BREADCRUMBS_OUTPUT1): HRESULT; stdcall;

    function GetPageFaultAllocationOutput1(pOutput: PD3D12_DRED_PAGE_FAULT_OUTPUT1): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceRemovedExtendedData1 = ID3D12DeviceRemovedExtendedData1;


  // Interface ID3D12DeviceRemovedExtendedData2
  // ==========================================
  //
  ID3D12DeviceRemovedExtendedData2 = interface(ID3D12DeviceRemovedExtendedData1)
    ['{67FC5816-E4CA-4915-BF18-42541272DA54}']

    function GetPageFaultAllocationOutput2(pOutput: PD3D12_DRED_PAGE_FAULT_OUTPUT2): HRESULT; stdcall;

    function GetDeviceState(): D3D12_DRED_DEVICE_STATE; stdcall;
  end;
  IID_ID3D12DeviceRemovedExtendedData2 = ID3D12DeviceRemovedExtendedData2;


  PD3D12_BACKGROUND_PROCESSING_MODE = ^D3D12_BACKGROUND_PROCESSING_MODE;
  D3D12_BACKGROUND_PROCESSING_MODE = (D3D12_BACKGROUND_PROCESSING_MODE_ALLOWED,
                                      D3D12_BACKGROUND_PROCESSING_MODE_ALLOW_INTRUSIVE_MEASUREMENTS,
                                      D3D12_BACKGROUND_PROCESSING_MODE_DISABLE_BACKGROUND_WORK,
                                      D3D12_BACKGROUND_PROCESSING_MODE_DISABLE_PROFILING_BY_SYSTEM
                                     );


  PD3D12_MEASUREMENTS_ACTION = ^D3D12_MEASUREMENTS_ACTION;
  D3D12_MEASUREMENTS_ACTION = (D3D12_MEASUREMENTS_ACTION_KEEP_ALL,
                               D3D12_MEASUREMENTS_ACTION_COMMIT_RESULTS,
                               D3D12_MEASUREMENTS_ACTION_COMMIT_RESULTS_HIGH_PRIORITY,
                               D3D12_MEASUREMENTS_ACTION_DISCARD_PREVIOUS
                              );


  // Interface ID3D12Device6
  // =======================
  //
  ID3D12Device6 = interface(ID3D12Device5)
    ['{C70B221B-40E4-4A17-89AF-025A0727A6DC}']

    function SetBackgroundProcessingMode(Mode: D3D12_BACKGROUND_PROCESSING_MODE;
                                         MeasurementsAction: D3D12_MEASUREMENTS_ACTION;
                                         hEventToSignalUponCompletion: THandle;
                                         pbFurtherMeasurementsDesired: PBOOL): HRESULT; stdcall;
  end;
  IID_ID3D12Device6 = ID3D12Device6;


  // D3D12_FEATURE_PROTECTED_RESOURCE_SESSION_TYPE_COUNT
  PD3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_TYPE_COUNT = ^D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_TYPE_COUNT;
  D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_TYPE_COUNT = record
    NodeIndex: UINT;  // input
    Count: UINT;      // output
  end;


  // D3D12_FEATURE_PROTECTED_RESOURCE_SESSION_TYPES
  PD3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_TYPES = ^D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_TYPES;
  D3D12_FEATURE_DATA_PROTECTED_RESOURCE_SESSION_TYPES = record
    NodeIndex: UINT;                 // input
    Count: UINT;                     // input
    pTypes: PGUID;                   // output
  end;


  PD3D12_PROTECTED_RESOURCE_SESSION_DESC1 = ^D3D12_PROTECTED_RESOURCE_SESSION_DESC1;
  D3D12_PROTECTED_RESOURCE_SESSION_DESC1 = record
    NodeMask: UINT;
    Flags: D3D12_PROTECTED_RESOURCE_SESSION_FLAGS;
    ProtectionType: TGUID;
  end;


  // Interface ID3D12ProtectedResourceSession1
  // =========================================
  //
  ID3D12ProtectedResourceSession1 = interface(ID3D12ProtectedResourceSession)
    ['{D6F12DD6-76FB-406E-8961-4296EEFC0409}']

    procedure GetDesc1(out RetVal: D3D12_PROTECTED_RESOURCE_SESSION_DESC1); stdcall;
  end;
  IID_ID3D12ProtectedResourceSession1 = ID3D12ProtectedResourceSession1;


  // Interface ID3D12Device7
  // =======================
  //
  ID3D12Device7 = interface(ID3D12Device6)
    ['{5C014B53-68A1-4B9B-8BD1-DD6046B9358B}']

    function AddToStateObject(pAddition: PD3D12_STATE_OBJECT_DESC;
                              pStateObjectToGrowFrom: ID3D12StateObject;
                              const riid: TGUID;
                              ppNewStateObject: PPointer): HRESULT; stdcall;

    function CreateProtectedResourceSession1(pDesc: PD3D12_PROTECTED_RESOURCE_SESSION_DESC1;
                                             const riid: TGUID;
                                             ppSession: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Device7 = ID3D12Device7;


  // Interface ID3D12Device8
  // =======================
  //
  ID3D12Device8 = interface(ID3D12Device7)
    ['{9218E6BB-F944-4F7E-A75C-B1B2C7B701F3}']

    procedure GetResourceAllocationInfo2(out RetVal: D3D12_RESOURCE_ALLOCATION_INFO;
                                         visibleMask: UINT;
                                         numResourceDescs: UINT;
                                         pResourceDescs: PD3D12_RESOURCE_DESC1;
                                         pResourceAllocationInfo1: PD3D12_RESOURCE_ALLOCATION_INFO1); stdcall;

    function CreateCommittedResource2(pHeapProperties: PD3D12_HEAP_PROPERTIES;
                                      HeapFlags: D3D12_HEAP_FLAGS;
                                      pDesc: PD3D12_RESOURCE_DESC1;
                                      InitialResourceState: D3D12_RESOURCE_STATES;
                                      pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                      pProtectedSession: ID3D12ProtectedResourceSession;
                                      const riidResource: TGUID;
                                      ppvResource: PPointer): HRESULT; stdcall;

    function CreatePlacedResource1(pHeap: ID3D12Heap;
                                   HeapOffset: UINT64;
                                   pDesc: PD3D12_RESOURCE_DESC1;
                                   InitialState: D3D12_RESOURCE_STATES;
                                   pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                   const riid: TGUID;
                                   ppvResource: PPointer): HRESULT; stdcall;

    procedure CreateSamplerFeedbackUnorderedAccessView(pTargetedResource: ID3D12Resource;
                                                       pFeedbackResource: ID3D12Resource;
                                                       DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;

    procedure GetCopyableFootprints1(pResourceDesc: PD3D12_RESOURCE_DESC1;
                                     FirstSubresource: UINT;
                                     NumSubresources: UINT;
                                     BaseOffset: UINT64;
                                     pLayouts: PD3D12_PLACED_SUBRESOURCE_FOOTPRINT;
                                     pNumRows: PUINT;
                                     pRowSizeInBytes: PUINT64;
                                     pTotalBytes: PUINT64); stdcall;
  end;
  IID_ID3D12Device8 = ID3D12Device8;


  // Interface ID3D12Resource1
  // =========================
  //
  ID3D12Resource1 = interface(ID3D12Resource)
    ['{9D5E227A-4430-4161-88B3-3ECA6BB16E19}']

    function GetProtectedResourceSession(const riid: TGUID;
                                         ppProtectedSession: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Resource1 = ID3D12Resource1;


  // Interface ID3D12Resource2
  // =========================
  //
  ID3D12Resource2 = interface(ID3D12Resource1)
    ['{BE36EC3B-EA85-4AEB-A45A-E9D76404A495}']

     procedure GetDesc1(out RetVal: D3D12_RESOURCE_DESC1); stdcall;
  end;
  IID_ID3D12Resource2 = ID3D12Resource2;


  // Interface ID3D12Heap1
  // =====================
  //
  ID3D12Heap1 = interface(ID3D12Heap)
    ['{572F7389-2168-49E3-9693-D6DF5871BF6D}']

    function GetProtectedResourceSession(const riid: TGUID;
                                         ppProtectedSession: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Heap1 = ID3D12Heap1;


  // Interface ID3D12GraphicsCommandList3
  // ====================================
  //
  ID3D12GraphicsCommandList3 = interface(ID3D12GraphicsCommandList2)
    ['{6FDA83A7-B84C-4E38-9AC8-C7BD22016B3D}']

    procedure SetProtectedResourceSession(pProtectedResourceSession: ID3D12ProtectedResourceSession); stdcall;
  end;
  IID_ID3D12GraphicsCommandList3 = ID3D12GraphicsCommandList3;


  // Beginning Access
  PD3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE = ^D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE;
  D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE = (D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_DISCARD,
                                             D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_PRESERVE,
                                             D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR,
                                             D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_NO_ACCESS,
                                             // Update May 2024
                                             D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_PRESERVE_LOCAL_RENDER,
                                             D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_PRESERVE_LOCAL_SRV,
                                             D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_PRESERVE_LOCAL_UAV
                                             // ==
                                            );


  PD3D12_RENDER_PASS_BEGINNING_ACCESS_CLEAR_PARAMETERS = ^D3D12_RENDER_PASS_BEGINNING_ACCESS_CLEAR_PARAMETERS;
  D3D12_RENDER_PASS_BEGINNING_ACCESS_CLEAR_PARAMETERS = record
    ClearValue: D3D12_CLEAR_VALUE;
  end;

  // Update May 2024

  PD3D12_RENDER_PASS_BEGINNING_ACCESS_PRESERVE_LOCAL_PARAMETERS = ^D3D12_RENDER_PASS_BEGINNING_ACCESS_PRESERVE_LOCAL_PARAMETERS;
  D3D12_RENDER_PASS_BEGINNING_ACCESS_PRESERVE_LOCAL_PARAMETERS = record
    AdditionalWidth: UINT;
    AdditionalHeight: UINT;
  end;

  // ==

  {$ALIGN 4}
  D3D12_RENDER_PASS_BEGINNING_ACCESS = record
    Type_: D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE;
    case Integer of
      0:  (Clear: D3D12_RENDER_PASS_BEGINNING_ACCESS_CLEAR_PARAMETERS);
      1:  (PreserveLocal: D3D12_RENDER_PASS_BEGINNING_ACCESS_PRESERVE_LOCAL_PARAMETERS); // Update May 2024
  end;
  {$ALIGN 8}

  // Ending Access
  PD3D12_RENDER_PASS_ENDING_ACCESS_TYPE = ^D3D12_RENDER_PASS_ENDING_ACCESS_TYPE;
  D3D12_RENDER_PASS_ENDING_ACCESS_TYPE = (D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_DISCARD,
                                          D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_PRESERVE,
                                          D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_RESOLVE,
                                          D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_NO_ACCESS,
                                          // Update May 2024
                                          D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_PRESERVE_LOCAL_RENDER,
                                          D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_PRESERVE_LOCAL_SRV,
                                          D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_PRESERVE_LOCAL_UAV
                                          // ==
                                         );


  PD3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_SUBRESOURCE_PARAMETERS = ^D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_SUBRESOURCE_PARAMETERS;
  D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_SUBRESOURCE_PARAMETERS = record
    SrcSubresource: UINT;
    DstSubresource: UINT;
    DstX: UINT;
    DstY: UINT;
    SrcRect: D3D12_RECT; // (0, 0, 0, 0) = resolve entire resource.
  end;


  PD3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_PARAMETERS = ^D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_PARAMETERS;
  D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_PARAMETERS = record
    pSrcResource: PID3D12Resource;
    pDstResource: PID3D12Resource;
    // Can be a subset of RT's array slices, but can't target subresources that were't part of RTV/DSV.
    SubresourceCount: UINT;
    //[annotation("_Field_size_full_(SubresourceCount)")]
    pSubresourceParameters: PD3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_SUBRESOURCE_PARAMETERS;
    Format: DXGI_FORMAT;
    ResolveMode: D3D12_RESOLVE_MODE;
    PreserveResolveSource: BOOL;
  end;

  // Update May 2024

  PD3D12_RENDER_PASS_ENDING_ACCESS_PRESERVE_LOCAL_PARAMETERS = ^D3D12_RENDER_PASS_ENDING_ACCESS_PRESERVE_LOCAL_PARAMETERS;
  D3D12_RENDER_PASS_ENDING_ACCESS_PRESERVE_LOCAL_PARAMETERS = record
    AdditionalWidth: UINT;
    AdditionalHeight: UINT;
  end;

 // ==

  D3D12_RENDER_PASS_ENDING_ACCESS = record
    Type_: D3D12_RENDER_PASS_ENDING_ACCESS_TYPE;
    case Integer of
      0: (Resolve: D3D12_RENDER_PASS_ENDING_ACCESS_RESOLVE_PARAMETERS);
      1: (PreserveLocal: D3D12_RENDER_PASS_ENDING_ACCESS_PRESERVE_LOCAL_PARAMETERS);
  end;


  // Render Target Desc
  PD3D12_RENDER_PASS_RENDER_TARGET_DESC = ^D3D12_RENDER_PASS_RENDER_TARGET_DESC;
  D3D12_RENDER_PASS_RENDER_TARGET_DESC = record
    cpuDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE;
    BeginningAccess: D3D12_RENDER_PASS_BEGINNING_ACCESS;
    EndingAccess: D3D12_RENDER_PASS_ENDING_ACCESS;
  end;


  // Depth-Stencil Desc
  PD3D12_RENDER_PASS_DEPTH_STENCIL_DESC = ^D3D12_RENDER_PASS_DEPTH_STENCIL_DESC;
  D3D12_RENDER_PASS_DEPTH_STENCIL_DESC = record
    cpuDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE;
    DepthBeginningAccess: D3D12_RENDER_PASS_BEGINNING_ACCESS;
    StencilBeginningAccess: D3D12_RENDER_PASS_BEGINNING_ACCESS;
    DepthEndingAccess: D3D12_RENDER_PASS_ENDING_ACCESS;
    StencilEndingAccess: D3D12_RENDER_PASS_ENDING_ACCESS;
  end;

 // Moved UP
 {PD3D12_RENDER_PASS_FLAGS = ^D3D12_RENDER_PASS_FLAGS;
  D3D12_RENDER_PASS_FLAGS                         = (
    D3D12_RENDER_PASS_FLAG_NONE                   = $0,
    D3D12_RENDER_PASS_FLAG_ALLOW_UAV_WRITES       = $1,
    D3D12_RENDER_PASS_FLAG_SUSPENDING_PASS        = $2,
    D3D12_RENDER_PASS_FLAG_RESUMING_PASS          = $4,
    D3D12_RENDER_PASS_FLAG_BIND_READ_ONLY_DEPTH   = $8,
    D3D12_RENDER_PASS_FLAG_BIND_READ_ONLY_STENCIL = $10
  );}

  // DEFINE_ENUM_FLAG_OPERATORS( D3D12_RENDER_PASS_FLAGS )

  // Interface ID3D12MetaCommand
  // ============================
  //
  ID3D12MetaCommand = interface(ID3D12Pageable)
    ['{DBB84C27-36CE-4FC9-B801-F048C46AC570}']

    function GetRequiredParameterResourceSize(Stage: D3D12_META_COMMAND_PARAMETER_STAGE;
                                              ParameterIndex: UINT): UINT64; stdcall;
  end;
  IID_ID3D12MetaCommand = ID3D12MetaCommand;


  PD3D12_DISPATCH_RAYS_DESC = ^D3D12_DISPATCH_RAYS_DESC;
  D3D12_DISPATCH_RAYS_DESC = record
    RayGenerationShaderRecord: D3D12_GPU_VIRTUAL_ADDRESS_RANGE;
    MissShaderTable: D3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE;
    HitGroupTable: D3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE;
    CallableShaderTable: D3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE;
    Width: UINT;
    Height: UINT;
    Depth: UINT;
  end;

  // Update May 2024

  PD3D12_SET_WORK_GRAPH_FLAGS = ^D3D12_SET_WORK_GRAPH_FLAGS;
  D3D12_SET_WORK_GRAPH_FLAGS             = (
    D3D12_SET_WORK_GRAPH_FLAG_NONE       = $0,
    D3D12_SET_WORK_GRAPH_FLAG_INITIALIZE = $1
  );


  // DEFINE_ENUM_FLAG_OPERATORS( D3D12_SET_WORK_GRAPH_FLAGS )

  PD3D12_SET_WORK_GRAPH_DESC = ^D3D12_SET_WORK_GRAPH_DESC;
  D3D12_SET_WORK_GRAPH_DESC = record
    ProgramIdentifier: D3D12_PROGRAM_IDENTIFIER;
    Flags: D3D12_SET_WORK_GRAPH_FLAGS;
    BackingMemory: D3D12_GPU_VIRTUAL_ADDRESS_RANGE;
    NodeLocalRootArgumentsTable: D3D12_GPU_VIRTUAL_ADDRESS_RANGE_AND_STRIDE;
  end;


  PD3D12_SET_RAYTRACING_PIPELINE_DESC = ^D3D12_SET_RAYTRACING_PIPELINE_DESC;
  D3D12_SET_RAYTRACING_PIPELINE_DESC = record
    ProgramIdentifier: D3D12_PROGRAM_IDENTIFIER;
  end;


  PD3D12_SET_GENERIC_PIPELINE_DESC = ^D3D12_SET_GENERIC_PIPELINE_DESC;
  D3D12_SET_GENERIC_PIPELINE_DESC = record
    ProgramIdentifier: D3D12_PROGRAM_IDENTIFIER;
  end;


  PD3D12_PROGRAM_TYPE = ^D3D12_PROGRAM_TYPE;
  D3D12_PROGRAM_TYPE                       = (
    D3D12_PROGRAM_TYPE_GENERIC_PIPELINE    = 1,
    D3D12_PROGRAM_TYPE_RAYTRACING_PIPELINE = 4,
    D3D12_PROGRAM_TYPE_WORK_GRAPH          = 5
  );


  PD3D12_SET_PROGRAM_DESC = ^D3D12_SET_PROGRAM_DESC;
  D3D12_SET_PROGRAM_DESC = record
    _Type: D3D12_PROGRAM_TYPE;
    case Integer of
      0: (GenericPipeline: D3D12_SET_GENERIC_PIPELINE_DESC);
      1: (RaytracingPipeline: D3D12_SET_RAYTRACING_PIPELINE_DESC);
      2: (WorkGraph: D3D12_SET_WORK_GRAPH_DESC);
    end;


  PD3D12_DISPATCH_MODE = ^D3D12_DISPATCH_MODE;
  D3D12_DISPATCH_MODE                        = (
    D3D12_DISPATCH_MODE_NODE_CPU_INPUT       = 0,  // D3D12_NODE_CPU_INPUT
    D3D12_DISPATCH_MODE_NODE_GPU_INPUT       = 1,  // D3D12_NODE_GPU_INPUT in GPU memory
    D3D12_DISPATCH_MODE_MULTI_NODE_CPU_INPUT = 2,  // D3D12_MULTI_NODE_CPU_INPUT
    D3D12_DISPATCH_MODE_MULTI_NODE_GPU_INPUT = 3   // D3D12_MULTI_NODE_GPU_INPUT in GPU memory
  );


  PD3D12_NODE_CPU_INPUT = ^D3D12_NODE_CPU_INPUT;
  D3D12_NODE_CPU_INPUT = record
    EntrypointIndex: UINT;
    NumRecords: UINT;
    pRecords: Pointer;
    RecordStrideInBytes: UINT64;
  end;


  PD3D12_NODE_GPU_INPUT = ^D3D12_NODE_GPU_INPUT;
  D3D12_NODE_GPU_INPUT = record
    EntrypointIndex: UINT;
    NumRecords: UINT;
    Records: D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;
  end;


  PD3D12_MULTI_NODE_CPU_INPUT = ^D3D12_MULTI_NODE_CPU_INPUT;
  D3D12_MULTI_NODE_CPU_INPUT = record
    NumNodeInputs: UINT;
    pNodeInputs: PD3D12_NODE_CPU_INPUT;
    NodeInputStrideInBytes: UINT64;
  end;


  PD3D12_MULTI_NODE_GPU_INPUT = ^D3D12_MULTI_NODE_GPU_INPUT;
  D3D12_MULTI_NODE_GPU_INPUT = record
    NumNodeInputs: UINT;
    NodeInputs: D3D12_GPU_VIRTUAL_ADDRESS_AND_STRIDE;  // D3D12_GPU_NODE_INPUT array
  end;


  PD3D12_DISPATCH_GRAPH_DESC = ^D3D12_DISPATCH_GRAPH_DESC;
  D3D12_DISPATCH_GRAPH_DESC = record
    Mode: D3D12_DISPATCH_MODE;
    case Integer of
      0: (NodeCPUInput: D3D12_NODE_CPU_INPUT);
      1: (NodeGPUInput: D3D12_GPU_VIRTUAL_ADDRESS);
      2: (MultiNodeCPUInput: D3D12_MULTI_NODE_CPU_INPUT);
      3: (MultiNodeGPUInput: D3D12_GPU_VIRTUAL_ADDRESS);
  end;

  // ==

  // Interface ID3D12GraphicsCommandList4
  // =====================================
  //
  ID3D12GraphicsCommandList4 = interface(ID3D12GraphicsCommandList3)
    ['{8754318E-D3A9-4541-98CF-645B50DC4874}']

    procedure BeginRenderPass(NumRenderTargets: UINT;
                              pRenderTargets: PD3D12_RENDER_PASS_RENDER_TARGET_DESC;
                              pDepthStencil: PD3D12_RENDER_PASS_DEPTH_STENCIL_DESC;
                              Flags: D3D12_RENDER_PASS_FLAGS); stdcall;

    procedure EndRenderPass(); stdcall;

    procedure InitializeMetaCommand(pMetaCommand: ID3D12MetaCommand;
                                    pInitializationParametersData: Pointer;
                                    InitializationParametersDataSizeInBytes: SIZE_T); stdcall;

    procedure ExecuteMetaCommand(pMetaCommand: ID3D12MetaCommand;
                                 pExecutionParametersData: Pointer;
                                 ExecutionParametersDataSizeInBytes: SIZE_T); stdcall;

    procedure BuildRaytracingAccelerationStructure(pDesc: PD3D12_BUILD_RAYTRACING_ACCELERATION_STRUCTURE_DESC;
                                                   NumPostbuildInfoDescs: UINT;
                                                   pPostbuildInfoDescs: PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_DESC); stdcall;

    procedure EmitRaytracingAccelerationStructurePostbuildInfo(pDesc: PD3D12_RAYTRACING_ACCELERATION_STRUCTURE_POSTBUILD_INFO_DESC;
                                                               NumSourceAccelerationStructures: UINT;
                                                               pSourceAccelerationStructureData: PD3D12_GPU_VIRTUAL_ADDRESS); stdcall;

    procedure CopyRaytracingAccelerationStructure(DestAccelerationStructureData: D3D12_GPU_VIRTUAL_ADDRESS;
                                                  SourceAccelerationStructureData: D3D12_GPU_VIRTUAL_ADDRESS;
                                                  Mode: D3D12_RAYTRACING_ACCELERATION_STRUCTURE_COPY_MODE); stdcall;

    procedure SetPipelineState1(pStateObject: ID3D12StateObject); stdcall;

    procedure DispatchRays(pDesc: PD3D12_DISPATCH_RAYS_DESC); stdcall;
  end;
  IID_ID3D12GraphicsCommandList4 = ID3D12GraphicsCommandList4;


  PD3D12_SHADER_CACHE_MODE = ^D3D12_SHADER_CACHE_MODE;
  D3D12_SHADER_CACHE_MODE = (D3D12_SHADER_CACHE_MODE_MEMORY,
                             D3D12_SHADER_CACHE_MODE_DISK
                            );


  PD3D12_SHADER_CACHE_SESSION_DESC = ^D3D12_SHADER_CACHE_SESSION_DESC;
  D3D12_SHADER_CACHE_SESSION_DESC = record
    Identifier: TGUID;
    Mode: D3D12_SHADER_CACHE_MODE;
    Flags: D3D12_SHADER_CACHE_FLAGS;
    MaximumInMemoryCacheSizeBytes: UINT;
    MaximumInMemoryCacheEntries: UINT;
    MaximumValueFileSizeBytes: UINT;
    Version: UINT64;
  end;


  PD3D12_BARRIER_TYPE = ^D3D12_BARRIER_TYPE;
  D3D12_BARRIER_TYPE = (D3D12_BARRIER_TYPE_GLOBAL,
                        D3D12_BARRIER_TYPE_TEXTURE,
                        D3D12_BARRIER_TYPE_BUFFER
                       );


  PD3D12_BARRIER_SUBRESOURCE_RANGE = ^D3D12_BARRIER_SUBRESOURCE_RANGE;
  D3D12_BARRIER_SUBRESOURCE_RANGE = record
    IndexOrFirstMipLevel: UINT;
    NumMipLevels: UINT;
    FirstArraySlice: UINT;
    NumArraySlices: UINT;
    FirstPlane: UINT;
    NumPlanes: UINT;
  end;


  PD3D12_GLOBAL_BARRIER = ^D3D12_GLOBAL_BARRIER;
  D3D12_GLOBAL_BARRIER = record
    SyncBefore: D3D12_BARRIER_SYNC;
    SyncAfter: D3D12_BARRIER_SYNC;
    AccessBefore: D3D12_BARRIER_ACCESS;
    AccessAfter: D3D12_BARRIER_ACCESS;
  end;


  PD3D12_TEXTURE_BARRIER = ^D3D12_TEXTURE_BARRIER;
  D3D12_TEXTURE_BARRIER = record
    SyncBefore: D3D12_BARRIER_SYNC;
    SyncAfter: D3D12_BARRIER_SYNC;
    AccessBefore: D3D12_BARRIER_ACCESS;
    AccessAfter: D3D12_BARRIER_ACCESS;
    LayoutBefore: D3D12_BARRIER_LAYOUT;
    LayoutAfter: D3D12_BARRIER_LAYOUT;
    pResource: PID3D12Resource;
    Subresources: D3D12_BARRIER_SUBRESOURCE_RANGE;
    Flags: D3D12_TEXTURE_BARRIER_FLAGS;
  end;


  PD3D12_BUFFER_BARRIER = ^D3D12_BUFFER_BARRIER;
  D3D12_BUFFER_BARRIER = record
    SyncBefore: D3D12_BARRIER_SYNC;
    SyncAfter: D3D12_BARRIER_SYNC;
    AccessBefore: D3D12_BARRIER_ACCESS;
    AccessAfter: D3D12_BARRIER_ACCESS;
    pResource: PID3D12Resource;
    Offset: UINT64;
    Size: UINT64;
  end;

  PD3D12_BARRIER_GROUP = ^D3D12_BARRIER_GROUP;
  D3D12_BARRIER_GROUP = record
    Type_: D3D12_BARRIER_TYPE;
    NumBarriers: UINT32;
    case Integer of
      0: (pGlobalBarriers: PD3D12_GLOBAL_BARRIER);
      1: (pTextureBarriers: PD3D12_TEXTURE_BARRIER);
      2: (pBufferBarriers: PD3D12_BUFFER_BARRIER);
  end;


  // Interface ID3D12ShaderCacheSession
  // ===================================
  //
  ID3D12ShaderCacheSession = interface(ID3D12DeviceChild)
    ['{28E2495D-0F64-4AE4-A6EC-129255DC49A8}']

    function FindValue(pKey: Pointer;
                       KeySize: UINT;
                       pValue: Pointer;
                       pValueSize: PUINT): HRESULT; stdcall;

    function StoreValue(pKey: Pointer;
                        KeySize: UINT;
                        pValue: Pointer;
                        ValueSize: UINT): HRESULT; stdcall;

    procedure SetDeleteOnDestroy(); stdcall;

    procedure GetDesc(out RetVal: D3D12_SHADER_CACHE_SESSION_DESC); stdcall;
  end;
  IID_ID3D12ShaderCacheSession = ID3D12ShaderCacheSession;


  PD3D12_SHADER_CACHE_CONTROL_FLAGS = ^D3D12_SHADER_CACHE_CONTROL_FLAGS;
  D3D12_SHADER_CACHE_CONTROL_FLAGS = (D3D12_SHADER_CACHE_CONTROL_FLAG_DISABLE = $1,
                                      D3D12_SHADER_CACHE_CONTROL_FLAG_ENABLE  = $2,
                                      D3D12_SHADER_CACHE_CONTROL_FLAG_CLEAR   = $4
                                     );


  // Interface ID3D12Device9
  // ========================
  //
  ID3D12Device9 = interface(ID3D12Device8)
    ['{4C80E962-F032-4F60-BC9E-EBC2CFA1D83C}']

    function CreateShaderCacheSession(pDesc: PD3D12_SHADER_CACHE_SESSION_DESC;
                                      const riid: TGUID;
                                      ppvSession: PPointer): HRESULT; stdcall;

    function ShaderCacheControl(Kinds: D3D12_SHADER_CACHE_KIND_FLAGS;
                                Control: D3D12_SHADER_CACHE_CONTROL_FLAGS): HRESULT; stdcall;

    function CreateCommandQueue1(pDesc: PD3D12_COMMAND_QUEUE_DESC;
                                 const CreatorID: TGUID;
                                 const riid: TGUID;
                                 ppCommandQueue: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Device9 = ID3D12Device9;


  // Interface ID3D12Device10
  // =========================
  //
  ID3D12Device10 = interface(ID3D12Device9)
    ['{517F8718-AA66-49F9-B02B-A7AB89C06031}']

    function CreateCommittedResource3(pHeapProperties: PD3D12_HEAP_PROPERTIES;
                                      HeapFlags: D3D12_HEAP_FLAGS;
                                      pDesc: PD3D12_RESOURCE_DESC1;
                                      InitialLayout: D3D12_BARRIER_LAYOUT;
                                      pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                      pProtectedSession: ID3D12ProtectedResourceSession;
                                      NumCastableFormats: UINT32;
                                      pCastableFormats: PDXGI_FORMAT;
                                      const riidResource: TGUID;
                                      ppvResource: PPointer): HRESULT; stdcall;

    function CreatePlacedResource2(pHeap: ID3D12Heap;
                                   HeapOffset: UINT64;
                                   pDesc: PD3D12_RESOURCE_DESC1;
                                   InitialLayout: D3D12_BARRIER_LAYOUT;
                                   pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                   NumCastableFormats: UINT32;
                                   pCastableFormats: PDXGI_FORMAT;
                                   const riid: TGUID;
                                   ppvResource: PPointer): HRESULT; stdcall;

    function CreateReservedResource2(pDesc: PD3D12_RESOURCE_DESC;
                                     InitialLayout: D3D12_BARRIER_LAYOUT;
                                     pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                     pProtectedSession: ID3D12ProtectedResourceSession;
                                     NumCastableFormats: UINT32;
                                     pCastableFormats: PDXGI_FORMAT;
                                     const riid: TGUID;
                                     ppvResource: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Device10 = ID3D12Device10;


  // Update May 2024

  // Interface ID3D12Device11
  // ========================
  //
  ID3D12Device11 = interface(ID3D12Device10)
    ['{5405C344-D457-444E-B4DD-2366E45AEE39}']

    procedure CreateSampler2(pDesc: PD3D12_SAMPLER_DESC2;
                             DestDescriptor: D3D12_CPU_DESCRIPTOR_HANDLE); stdcall;
  end;
  IID_ID3D12Device11 = ID3D12Device11;


  // Interface ID3D12Device12
  // ========================
  //
  ID3D12Device12 = interface(ID3D12Device11)
    ['{5AF5C532-4C91-4CD0-B541-15A405395FC5}']

    procedure GetResourceAllocationInfo3(out RetVal: D3D12_RESOURCE_ALLOCATION_INFO;
                                         visibleMask: UINT;
                                         numResourceDescs: UINT;
                                         pResourceDescs: PD3D12_RESOURCE_DESC1;
                                         pNumCastableFormats: PUINT32;
                                         ppCastableFormats: PPointer;
                                         pResourceAllocationInfo1: PD3D12_RESOURCE_ALLOCATION_INFO1); stdcall;
  end;
  IID_ID3D12Device12 = ID3D12Device12;


  // Interface ID3D12Device13
  // ========================
  //
  ID3D12Device13 = interface(ID3D12Device12)
    ['{14EECFFC-4DF8-40F7-A118-5C816F45695E}']

    function OpenExistingHeapFromAddress1(pAddress: Pointer;
                                          size: SIZE_T;
                                          const riid: TGUID;
                                          ppvHeap: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Device13 = ID3D12Device13;


  // Interface ID3D12Device14
  // ========================
  //
  ID3D12Device14 = interface(ID3D12Device13)
    ['{5F6E592D-D895-44C2-8E4A-88AD4926D323}']

    function CreateRootSignatureFromSubobjectInLibrary(nodeMask: UINT;
                                                       pLibraryBlob: Pointer;
                                                       blobLengthInBytes: SIZE_T;
                                                       subobjectName: LPCWSTR;
                                                       const riid: TGUID;
                                                       ppvRootSignature: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12Device14 = ID3D12Device14;





  // Interface ID3D12VirtualizationGuestDevice
  // ==========================================
  //
  ID3D12VirtualizationGuestDevice = interface(IUnknown)
    ['{BC66D368-7373-4943-8757-FC87DC79E476}']

    function ShareWithHost(pObject: ID3D12DeviceChild;
                           pHandle: PHandle): HRESULT; stdcall;

    function CreateFenceFd(pFence: ID3D12Fence;
                           FenceValue: UINT64;
                           pFenceFd: PINT): HRESULT; stdcall;
  end;
  IID_ID3D12VirtualizationGuestDevice = ID3D12VirtualizationGuestDevice;


  { 10.0.28000.0 }
  PD3D12_VERSION_NUMBER = ^D3D12_VERSION_NUMBER;
  D3D12_VERSION_NUMBER = record
    case Integer of
      0: (Version: UINT64);
      1: (VersionParts: array[0..3] of UINT16);
  end;

  PD3D12_FEATURE_DATA_SHADERCACHE_ABI_SUPPORT = ^D3D12_FEATURE_DATA_SHADERCACHE_ABI_SUPPORT;
  D3D12_FEATURE_DATA_SHADERCACHE_ABI_SUPPORT = record
    szAdapterFamily: array[0..127] of WCHAR;
    MinimumABISupportVersion: UINT64;
    MaximumABISupportVersion: UINT64;
    CompilerVersion: D3D12_VERSION_NUMBER;
    ApplicationProfileVersion: D3D12_VERSION_NUMBER;
  end;

  // Interface ID3D12Tools
  // ======================
  //
  ID3D12Tools = interface(IUnknown)
    ['{7071E1F0-E84B-4B33-974F-12FA49DE65C5}']

    procedure EnableShaderInstrumentation(bEnable: BOOL); stdcall;

    function ShaderInstrumentationEnabled(): BOOL; stdcall;
  end;
  IID_ID3D12Tools = ID3D12Tools;

  { 10.0.28000.0 }
  ID3D12Tools1 = interface(ID3D12Tools)
    ['{E4FBC019-DD3C-43E1-8F32-7F649575F0A0}']
    function ReserveGPUVARangesAtCreate(pRanges: PD3D12_GPU_VIRTUAL_ADDRESS_RANGE;
                                        uiNumRanges: UINT): HRESULT; stdcall;
    procedure ClearReservedGPUVARangesList; stdcall;
  end;
  IID_ID3D12Tools1 = ID3D12Tools1;

  ID3D12Tools2 = interface(ID3D12Tools1)
    ['{01D393C5-C9B0-42A1-958C-C26B02D4D097}']
    function SetApplicationSpecificDriverState(pAdapter: IUnknown;
                                               pBlob: ID3DBlob): HRESULT; stdcall;
  end;
  IID_ID3D12Tools2 = ID3D12Tools2;

  ID3D12PageableTools = interface(IUnknown)
    ['{8F1359DB-D8D1-42F9-B5CF-79F4CBAD0D3D}']
    function GetAllocation(pAllocation: PD3D12_GPU_VIRTUAL_ADDRESS_RANGE): HRESULT; stdcall;
  end;
  IID_ID3D12PageableTools = ID3D12PageableTools;

  ID3D12DeviceTools = interface(IUnknown)
    ['{2EA68E9C-19C3-4E47-A109-6CDADFF0ACA9}']
    procedure SetNextAllocationAddress(nextAllocationVirtualAddress: D3D12_GPU_VIRTUAL_ADDRESS); stdcall;
  end;
  IID_ID3D12DeviceTools = ID3D12DeviceTools;

  D3D12_APPLICATION_SPECIFIC_DRIVER_BLOB_STATUS = (D3D12_APPLICATION_SPECIFIC_DRIVER_BLOB_UNKNOWN = 1,
                                                   D3D12_APPLICATION_SPECIFIC_DRIVER_BLOB_USED = 2,
                                                   D3D12_APPLICATION_SPECIFIC_DRIVER_BLOB_IGNORED = 3,
                                                   D3D12_APPLICATION_SPECIFIC_DRIVER_BLOB_NOT_SPECIFIED = 4
                                                  );

  ID3D12DeviceTools1 = interface(ID3D12DeviceTools)
    ['{E30E9FC7-E641-4D6E-8A81-9DD9206EC47A}']
    function GetApplicationSpecificDriverState(ppBlob: PID3DBlob): HRESULT; stdcall;
    function GetApplicationSpecificDriverBlobStatus: D3D12_APPLICATION_SPECIFIC_DRIVER_BLOB_STATUS; stdcall;
  end;
  IID_ID3D12DeviceTools1 = ID3D12DeviceTools1;


  PD3D12_SUBRESOURCE_DATA = ^D3D12_SUBRESOURCE_DATA;
  D3D12_SUBRESOURCE_DATA = record
    pData: Pointer;
    RowPitch: LONG_PTR;
    SlicePitch: LONG_PTR;
  end;


  PD3D12_MEMCPY_DEST = ^D3D12_MEMCPY_DEST;
  D3D12_MEMCPY_DEST = record
    pData: Pointer;
    RowPitch: SIZE_T;
    SlicePitch: SIZE_T;
  end;


  ///////////////////////////////////////////////////////////////////////////
  // D3D12CreateDevice
  // ------------------
  //
  // pAdapter
  //      If nil, D3D12CreateDevice will choose the primary adapter.
  //      If non-nil, D3D12CreateDevice will use the provided adapter.
  // MinimumFeatureLevel
  //      The minimum feature level required for successful device creation.
  // riid
  //      The interface IID of the device to be returned. Expected: ID3D12Device.
  // ppDevice
  //      Pointer to returned interface. May be nil.
  //
  // Return Values
  //  Any of those documented for
  //          CreateDXGIFactory1
  //          IDXGIFactory.EnumAdapters
  //          D3D12CreateDevice
  //
  ///////////////////////////////////////////////////////////////////////////

  function D3D12CreateDevice(pAdapter: IUnknown;
                               MinimumFeatureLevel: D3D_FEATURE_LEVEL;
                               const riid: TGUID;
                               out ppDevice): HRESULT; stdcall;

  function D3D12GetDebugInterface(const riid: TGUID;
                                    out ppvDebug): HRESULT; stdcall;


  // ---------------------------------------------------------------------------
  // D3D12EnableExperimentalFeatures
  //
  // Pass in a list of feature GUIDs to be enabled together.
  //
  // If a particular feature requires some configuration information on enablement, it will have
  // a configuration struct that can be passed alongside the GUID.
  //
  // Some features might use an interface IID as the GUID.  For these, once the feature is enabled via
  // D3D12EnableExperimentalFeatures, D3D12GetDebugInterface can then be called with the IID to retrieve the interface
  // for manipulating the feature.  This allows for control that might not cleanly be expressed by just
  // the configuration struct that D3D12EnableExperimentalFeatures provides.
  //
  // If this method is called and a change to existing feature enablement is made,
  // all current D3D12 devices are set to DEVICE_REMOVED state, since under the covers there is really only one
  // singleton device for a process.  Removing the devices when configuration changes prevents
  // mismatched expectations of how a device is supposed to work after it has been created from the app's point of view.
  //
  // The call returns E_NOINTERFACE if an unrecognized feature is passed in or Windows Developer mode is not on.
  // The call returns E_INVALIDARG if the configuration of a feature is incorrect, the set of features passed
  // in are known to be incompatible with each other, or other errors.
  // Returns S_OK otherwise.
  //
  // ---------------------------------------------------------------------------
  function D3D12EnableExperimentalFeatures(NumFeatures: UINT;
                                             pIIDs: PGUID;
                                             pConfigurationStructs: Pointer;
                                             pConfigurationStructSizes: PUINT): HRESULT; stdcall;

  // --------------------------------------------------------------------------------------------------------------------------------
  // Experimental Feature: D3D12ExperimentalShaderModels
  //
  // Use with D3D12EnableExperimentalFeatures to enable experimental shader model support,
  // meaning shader models that haven't been finalized for use in retail.
  //
  // Enabling D3D12ExperimentalShaderModels needs no configuration struct, pass NULL in the pConfigurationStructs array.
  //
  // --------------------------------------------------------------------------------------------------------------------------------


 function D3D12GetInterface(const rclsid: TGUID;
                               const riid: TGUID;
                               out ppvDebug): HRESULT; stdcall;


  //============================================================================
  //
  // SDK Configuration
  //
  //============================================================================

type

  // Interface ID3D12SDKConfiguration
  // ================================
  //
  ID3D12SDKConfiguration = interface(IUnknown)
    ['{E9EB5314-33AA-42B2-A718-D77F58B1F1C7}']

    function SetSDKVersion(SDKVersion: UINT;
                           SDKPath: LPCSTR): HRESULT; stdcall;
  end;
  IID_ID3D12SDKConfiguration = ID3D12SDKConfiguration;


  // Interface ID3D12SDKConfiguration1
  // =================================
  //
  ID3D12SDKConfiguration1 = interface(ID3D12SDKConfiguration)
    ['{8AAF9303-AD25-48B9-9A57-D9C37E009D9F}']

    function CreateDeviceFactory(SDKVersion: UINT;
                                 SDKPath: LPCSTR;
                                 const riid: TGUID;
                                 ppvFactory: PPointer): HRESULT; stdcall;

    procedure FreeUnusedSDKs(); stdcall;
  end;
  IID_ID3D12SDKConfiguration1 = ID3D12SDKConfiguration1;


  PD3D12_DEVICE_FACTORY_FLAGS = ^D3D12_DEVICE_FACTORY_FLAGS;
  D3D12_DEVICE_FACTORY_FLAGS                                               = (
    D3D12_DEVICE_FACTORY_FLAG_NONE                                         = 0,
    D3D12_DEVICE_FACTORY_FLAG_ALLOW_RETURNING_EXISTING_DEVICE              = $1,
    D3D12_DEVICE_FACTORY_FLAG_ALLOW_RETURNING_INCOMPATIBLE_EXISTING_DEVICE = $2,
    D3D12_DEVICE_FACTORY_FLAG_DISALLOW_STORING_NEW_DEVICE_AS_SINGLETON     = $4
  );

  // DEFINE_ENUM_FLAG_OPERATORS( D3D12_DEVICE_FACTORY_FLAGS )


  // Interface ID3D12SDKConfiguration1
  // =================================
  //
  ID3D12DeviceFactory = interface(IUnknown)
    ['{61F307D3-D34E-4E7C-8374-3BA4DE23CCCB}']

    function InitializeFromGlobalState(): HRESULT; stdcall;
    function ApplyToGlobalState(): HRESULT; stdcall;

    function SetFlags(flags: D3D12_DEVICE_FACTORY_FLAGS): HRESULT; stdcall;

    function GetFlags(): D3D12_DEVICE_FACTORY_FLAGS; stdcall;

    function GetConfigurationInterface(const clsid: TGUID;
                                       const iid: TGUID;
                                       ppv: PPointer): HRESULT; stdcall;

    function EnableExperimentalFeatures(NumFeatures: UINT;
                                        pIIDs: PIID;
                                        pConfigurationStructs: Pointer;
                                        pConfigurationStructSizes: PUINT): HRESULT; stdcall;

    function CreateDevice(adapter: IUnknown;
                          FeatureLevel: D3D_FEATURE_LEVEL;
                          const riid: TGUID;
                          ppvDevice: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceFactory = ID3D12DeviceFactory;


  PD3D12_DEVICE_FLAGS = ^D3D12_DEVICE_FLAGS;
  D3D12_DEVICE_FLAGS                                                 = (
    D3D12_DEVICE_FLAG_NONE                                           = 0,
    D3D12_DEVICE_FLAG_DEBUG_LAYER_ENABLED                            = $1,
    D3D12_DEVICE_FLAG_GPU_BASED_VALIDATION_ENABLED                   = $2,
    D3D12_DEVICE_FLAG_SYNCHRONIZED_COMMAND_QUEUE_VALIDATION_DISABLED = $4,
    D3D12_DEVICE_FLAG_DRED_AUTO_BREADCRUMBS_ENABLED                  = $8,
    D3D12_DEVICE_FLAG_DRED_PAGE_FAULT_REPORTING_ENABLED              = $10,
    D3D12_DEVICE_FLAG_DRED_WATSON_REPORTING_ENABLED                  = $20,
    D3D12_DEVICE_FLAG_DRED_BREADCRUMB_CONTEXT_ENABLED                = $40,
    D3D12_DEVICE_FLAG_DRED_USE_MARKERS_ONLY_BREADCRUMBS              = $80,
    D3D12_DEVICE_FLAG_SHADER_INSTRUMENTATION_ENABLED                 = $100,
    D3D12_DEVICE_FLAG_AUTO_DEBUG_NAME_ENABLED                        = $200,
    D3D12_DEVICE_FLAG_FORCE_LEGACY_STATE_VALIDATION                  = $400
  );

  // DEFINE_ENUM_FLAG_OPERATORS( D3D12_DEVICE_FLAGS )

  PD3D12_DEVICE_CONFIGURATION_DESC = ^D3D12_DEVICE_CONFIGURATION_DESC;
  D3D12_DEVICE_CONFIGURATION_DESC = record
    Flags: D3D12_DEVICE_FLAGS;
    GpuBasedValidationFlags: UINT;  // D3D12_GPU_BASED_VALIDATION_FLAGS from d3d12sdklayers.h
    SDKVersion: UINT;
    NumEnabledExperimentalFeatures: UINT;
  end;


  // Interface ID3D12DeviceConfiguration
  // ===================================
  //
  ID3D12DeviceConfiguration = interface(IUnknown)
    ['{78DBF87B-F766-422B-A61C-C8C446BDB9AD}']

    procedure GetDesc(out RetVal: D3D12_DEVICE_CONFIGURATION_DESC); stdcall;

    function GetEnabledExperimentalFeatures(pGuids: PGUID;
                                            NumGuids: UINT): HRESULT; stdcall;

    function SerializeVersionedRootSignature(pDesc: PD3D12_VERSIONED_ROOT_SIGNATURE_DESC;
                                             ppResult: PID3DBlob;
                                             ppError: PID3DBlob): HRESULT; stdcall;

    function CreateVersionedRootSignatureDeserializer(pBlob: Pointer;
                                                      Size: SIZE_T;
                                                      const riid: TGUID;
                                                      ppvDeserializer: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceConfiguration = ID3D12DeviceConfiguration;

  // Update May 2024

  // Interface ID3D12DeviceConfiguration1
  // ====================================
  //
  ID3D12DeviceConfiguration1 = interface(ID3D12DeviceConfiguration)
    ['{ED342442-6343-4E16-BB82-A3A577874E56}']

    function CreateVersionedRootSignatureDeserializerFromSubobjectInLibrary(pLibraryBlob: Pointer;
                                                                            Size: SIZE_T;
                                                                            RootSignatureSubobjectName: LPCWSTR;
                                                                            const riid: TGUID;
                                                                            ppvDeserializer: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceConfiguration1 = ID3D12DeviceConfiguration1;

  // ==

  PD3D12_AXIS_SHADING_RATE = ^D3D12_AXIS_SHADING_RATE;
  D3D12_AXIS_SHADING_RATE = (D3D12_AXIS_SHADING_RATE_1X = $0,
                             D3D12_AXIS_SHADING_RATE_2X = $1,
                             D3D12_AXIS_SHADING_RATE_4X = $2
                            );


  // https://learn.microsoft.com/en-us/windows/win32/direct3d12/vrs

  function D3D12_MAKE_COARSE_SHADING_RATE(x: Integer;
                                          y: Integer): Integer;

  function D3D12_GET_COARSE_SHADING_RATE_X_AXIS(x: Integer): Integer;


  function D3D12_GET_COARSE_SHADING_RATE_Y_AXIS(y: Integer): Integer;


type
  PD3D12_SHADING_RATE = ^D3D12_SHADING_RATE;
  D3D12_SHADING_RATE = (D3D12_SHADING_RATE_1X1 = $0,
                        D3D12_SHADING_RATE_1X2 = $1,
                        D3D12_SHADING_RATE_2X1 = $4,
                        D3D12_SHADING_RATE_2X2 = $5,
                        D3D12_SHADING_RATE_2X4 = $6,
                        D3D12_SHADING_RATE_4X2 = $9,
                        D3D12_SHADING_RATE_4X4 = $A
                       );


  PD3D12_SHADING_RATE_COMBINER = ^D3D12_SHADING_RATE_COMBINER;
  D3D12_SHADING_RATE_COMBINER = (D3D12_SHADING_RATE_COMBINER_PASSTHROUGH = 0,
                                 D3D12_SHADING_RATE_COMBINER_OVERRIDE    = 1,
                                 D3D12_SHADING_RATE_COMBINER_MIN         = 2,
                                 D3D12_SHADING_RATE_COMBINER_MAX         = 3,
                                 D3D12_SHADING_RATE_COMBINER_SUM         = 4
                                );


  // Interface ID3D12GraphicsCommandList5
  // =====================================
  //
  ID3D12GraphicsCommandList5 = interface(ID3D12GraphicsCommandList4)
    ['{55050859-4024-474C-87F5-6472EAEE44EA}']

    procedure RSSetShadingRate(baseShadingRate: D3D12_SHADING_RATE;
                               combiners: PD3D12_SHADING_RATE_COMBINER); stdcall;

    procedure RSSetShadingRateImage(shadingRateImage: ID3D12Resource); stdcall;
  end;
  IID_ID3D12GraphicsCommandList5 = ID3D12GraphicsCommandList5;


  PD3D12_DISPATCH_MESH_ARGUMENTS = ^D3D12_DISPATCH_MESH_ARGUMENTS;
  D3D12_DISPATCH_MESH_ARGUMENTS = record
    ThreadGroupCountX: UINT;
    ThreadGroupCountY: UINT;
    ThreadGroupCountZ: UINT;
  end;


  // Interface ID3D12GraphicsCommandList6
  // =====================================
  //
  ID3D12GraphicsCommandList6 = interface(ID3D12GraphicsCommandList5)
    ['{C3827890-E548-4CFA-96CF-5689A9370F80}']

    procedure DispatchMesh(ThreadGroupCountX: UINT;
                           ThreadGroupCountY: UINT;
                           ThreadGroupCountZ: UINT); stdcall;
  end;
  IID_ID3D12GraphicsCommandList6 = ID3D12GraphicsCommandList6;


  // Interface ID3D12GraphicsCommandList7
  // =====================================
  //
  ID3D12GraphicsCommandList7 = interface(ID3D12GraphicsCommandList6)
    ['{DD171223-8B61-4769-90E3-160CCDE4E2C1}']

    procedure Barrier(NumBarrierGroups: UINT32;
                      pBarrierGroups: PD3D12_BARRIER_GROUP); stdcall;
  end;
  IID_ID3D12GraphicsCommandList7 = ID3D12GraphicsCommandList7;

  // Update May 2024

  // Interface ID3D12GraphicsCommandList8
  // ====================================
  //
  ID3D12GraphicsCommandList8 = interface(ID3D12GraphicsCommandList7)
    ['{EE936EF9-599D-4D28-938E-23C4AD05CE51}']

    procedure OMSetFrontAndBackStencilRef(FrontStencilRef: UINT;
                                          BackStencilRef: UINT); stdcall;
  end;
  IID_ID3D12GraphicsCommandList8 = ID3D12GraphicsCommandList8;


  // Interface ID3D12GraphicsCommandList9
  // ====================================
  //
  ID3D12GraphicsCommandList9 = interface(ID3D12GraphicsCommandList8)
    ['{34ED2808-FFE6-4C2B-B11A-CABD2B0C59E1}']

    procedure RSSetDepthBias(DepthBias: FLOAT;
                             DepthBiasClamp: FLOAT;
                             SlopeScaledDepthBias: FLOAT); stdcall;

    procedure IASetIndexBufferStripCutValue(IBStripCutValue: D3D12_INDEX_BUFFER_STRIP_CUT_VALUE); stdcall;
  end;
  IID_ID3D12GraphicsCommandList9 = ID3D12GraphicsCommandList9;


  // Interface ID3D12GraphicsCommandList10
  // =====================================
  //
  ID3D12GraphicsCommandList10 = interface(ID3D12GraphicsCommandList9)
    ['{7013C015-D161-4B63-A08C-238552DD8ACC}']

    procedure SetProgram(pDesc: PD3D12_SET_PROGRAM_DESC); stdcall;

    procedure DispatchGraph(pDesc: PD3D12_DISPATCH_GRAPH_DESC); stdcall;
  end;
  IID_ID3D12GraphicsCommandList10 = ID3D12GraphicsCommandList10;

  // ==

  { 10.0.28000.0 }
  ID3D12DSRDeviceFactory = interface(IUnknown)
    ['{F343D1A0-AFE3-439F-B13D-CD87A43B70CA}']
    function CreateDSRDevice(pD3D12Device: ID3D12Device;
                             NodeMask: UINT;
                             const riid: TGUID;
                             ppvDSRDevice: PPointer): HRESULT; stdcall;
  end;
  IID_ID3D12DSRDeviceFactory = ID3D12DSRDeviceFactory;

  ID3D12GBVDiagnostics = interface(IUnknown)
    ['{597985AB-9B75-4DBB-BE23-0761195BEBEE}']
    function GetGBVEntireSubresourceStatesData(pResource: ID3D12Resource;
                                               pData: PINT;
                                               DataSize: UINT): HRESULT; stdcall;
    function GetGBVSubresourceState(pResource: ID3D12Resource;
                                    Subresource: UINT;
                                    pData: PINT): HRESULT; stdcall;
    function GetGBVResourceUniformState(pResource: ID3D12Resource;
                                        pData: PINT): HRESULT; stdcall;
    function GetGBVResourceInfo(pResource: ID3D12Resource;
                                pResourceDesc: PD3D12_RESOURCE_DESC;
                                pResourceHash: PUINT32;
                                pSubresourceStatesByteOffset: PUINT32): HRESULT; stdcall;
    procedure GBVReserved0; stdcall;
    procedure GBVReserved1; stdcall;
  end;
  IID_ID3D12GBVDiagnostics = ID3D12GBVDiagnostics;

  PD3D12_CREATE_STATE_OBJECT_STATISTICS = ^D3D12_CREATE_STATE_OBJECT_STATISTICS;
  D3D12_CREATE_STATE_OBJECT_STATISTICS = record
    NumCreated: UINT;
    NumPSDBCacheMissed: UINT;
    NumTotalCacheMissed: UINT;
    NumCacheUnknown: UINT;
  end;

  PD3D12_STATE_OBJECT_STATISTICS = ^D3D12_STATE_OBJECT_STATISTICS;
  D3D12_STATE_OBJECT_STATISTICS = record
    DefaultPSDBRegistered: BOOL;
    PipelineStateObjectStatistics: D3D12_CREATE_STATE_OBJECT_STATISTICS;
    StateObjectStatistics: D3D12_CREATE_STATE_OBJECT_STATISTICS;
  end;

  ID3D12DeviceStatistics = interface(IUnknown)
    ['{3D5CA1A8-A39E-4619-95E0-F9B0A40340F5}']
    function GetStateObjectStatistics(pStatistics: PD3D12_STATE_OBJECT_STATISTICS): HRESULT; stdcall;
  end;
  IID_ID3D12DeviceStatistics = ID3D12DeviceStatistics;


 // Converted MACRO'S /////////////////////////////////////////////////////////

  function D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING: UINT; overload;

  function D3D12_ENCODE_SHADER_4_COMPONENT_MAPPING(Src0: UINT;
                                                   Src1: UINT;
                                                   Src2: UINT;
                                                   Src3: UINT): UINT; overload;

  function D3D12_DECODE_SHADER_4_COMPONENT_MAPPING(ComponentToExtract: UINT;
                                                   Mapping: UINT): D3D12_SHADER_COMPONENT_MAPPING; overload;

  function D3D12_ENCODE_BASIC_FILTER(min: UINT;
                                     mag: UINT;
                                     mip: UINT;
                                     reduction: UINT): D3D12_FILTER;

  function D3D12_ENCODE_ANISOTROPIC_FILTER(reduction: UINT): D3D12_FILTER; inline;

  // Update May 2024
  function D3D12_ENCODE_MIN_MAG_ANISOTROPIC_MIP_POINT_FILTER(reduction: UINT): D3D12_FILTER; inline;
  // ==

  function D3D12_DECODE_MIN_FILTER(D3D12Filter: UINT): D3D12_FILTER_TYPE; inline;

  function D3D12_DECODE_MAG_FILTER(D3D12Filter: UINT): D3D12_FILTER_TYPE; inline;

  function D3D12_DECODE_MIP_FILTER(D3D12Filter: UINT): D3D12_FILTER_TYPE; inline;

  function D3D12_DECODE_FILTER_REDUCTION(D3D12Filter: UINT): D3D12_FILTER_REDUCTION_TYPE; inline;

  function D3D12_DECODE_IS_COMPARISON_FILTER(D3D12Filter: UINT): Boolean; inline;

  function D3D12_DECODE_IS_ANISOTROPIC_FILTER(D3D12Filter: UINT): Boolean; inline;

// =============================================================================

  function D3D12SerializeRootSignature(pRootSignature: PD3D12_ROOT_SIGNATURE_DESC;
                                         Version: D3D_ROOT_SIGNATURE_VERSION;
                                         ppBlob: PID3DBlob;
                                         ppErrorBlob: PID3DBlob): HRESULT; stdcall;


  function D3D12CreateRootSignatureDeserializer(pSrcData: LPCVOID;
                                                  SrcDataSizeInBytes: SIZE_T;
                                                  const pRootSignatureDeserializerInterface: TGUID;
                                                  out ppRootSignatureDeserializer): HRESULT; stdcall;


  function D3D12SerializeVersionedRootSignature(pRootSignature: PD3D12_VERSIONED_ROOT_SIGNATURE_DESC;
                                                  ppBlob: PID3DBlob;
                                                  ppErrorBlob: PID3DBlob): HRESULT; stdcall;

  { 10.0.28000.0 }
  function D3D12CreateVersionedRootSignatureDeserializerFromSubobjectInLibrary(pSrcData: LPCVOID;
                                                                                 SrcDataSizeInBytes: SIZE_T;
                                                                                 RootSignatureSubobjectName: LPCWSTR;
                                                                                 const pRootSignatureDeserializerInterface: TGUID;
                                                                                 out ppRootSignatureDeserializer): HRESULT; stdcall;



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

function D3D12CreateVersionedRootSignatureDeserializer(pSrcData: LPCVOID;
                                                           SrcDataSizeInBytes: SIZE_T;
                                                           const pRootSignatureDeserializerInterface: TGUID;
                                                           out ppRootSignatureDeserializer): HRESULT; stdcall;

implementation

  // Implement Additional Prototypes here.

const
  D3D12lib = 'd3d12.dll';
function D3D12CreateVersionedRootSignatureDeserializer; external D3D12Lib name 'D3D12CreateVersionedRootSignatureDeserializer';



function D3D12CreateDevice;   external D3D12Lib name 'D3D12CreateDevice' ;
function D3D12GetDebugInterface;   external D3D12Lib name 'D3D12GetDebugInterface' ;
function D3D12SerializeRootSignature;   external D3D12Lib name 'D3D12SerializeRootSignature' ;
function D3D12CreateRootSignatureDeserializer;   external D3D12Lib name 'D3D12CreateRootSignatureDeserializer' ;
function D3D12EnableExperimentalFeatures;  external D3D12Lib name 'D3D12EnableExperimentalFeatures' ;
function D3D12GetInterface;   external D3D12Lib name 'D3D12GetInterface' ;
function D3D12SerializeVersionedRootSignature;  external D3D12Lib name 'D3D12SerializeVersionedRootSignature' ;
{ 10.0.28000.0 }
function D3D12CreateVersionedRootSignatureDeserializerFromSubobjectInLibrary; external D3D12Lib name 'D3D12CreateVersionedRootSignatureDeserializerFromSubobjectInLibrary' ;



// Helpers for D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING

function D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING: UINT;
begin
  Result := D3D12_ENCODE_SHADER_4_COMPONENT_MAPPING(0,
                                                    1,
                                                    2,
                                                    3);
end;


function D3D12_ENCODE_SHADER_4_COMPONENT_MAPPING(Src0: UINT;
                                                 Src1: UINT;
                                                 Src2: UINT;
                                                 Src3: UINT): UINT; overload;
begin
  Result := (((Src0 and D3D12_SHADER_COMPONENT_MAPPING_MASK) or
             ((Src1 and D3D12_SHADER_COMPONENT_MAPPING_MASK) shl D3D12_SHADER_COMPONENT_MAPPING_SHIFT) or
             ((Src2 and D3D12_SHADER_COMPONENT_MAPPING_MASK) shl (D3D12_SHADER_COMPONENT_MAPPING_SHIFT * 2)) or
             ((Src3 and D3D12_SHADER_COMPONENT_MAPPING_MASK) shl (D3D12_SHADER_COMPONENT_MAPPING_SHIFT * 3)) or
              D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES))
end;


function D3D12_DECODE_SHADER_4_COMPONENT_MAPPING(ComponentToExtract: UINT;
                                                 Mapping: UINT): D3D12_SHADER_COMPONENT_MAPPING; overload;
begin
  Result := D3D12_SHADER_COMPONENT_MAPPING(Mapping shr (D3D12_SHADER_COMPONENT_MAPPING_SHIFT * ComponentToExtract) and
            D3D12_SHADER_COMPONENT_MAPPING_MASK);
end;


// Other helpers

function D3D12_ENCODE_BASIC_FILTER(min: UINT;
                                   mag: UINT;
                                   mip: UINT;
                                   reduction: UINT): D3D12_FILTER;
begin
  Result := D3D12_FILTER(((min and D3D12_FILTER_TYPE_MASK) shl D3D12_MIN_FILTER_SHIFT) or
                         ((mag and D3D12_FILTER_TYPE_MASK) shl D3D12_MAG_FILTER_SHIFT) or
                         ((mip and D3D12_FILTER_TYPE_MASK) shl D3D12_MIP_FILTER_SHIFT) or
                        ((reduction and D3D12_FILTER_REDUCTION_TYPE_MASK) shl D3D12_FILTER_REDUCTION_TYPE_SHIFT));
end;


function D3D12_ENCODE_ANISOTROPIC_FILTER(reduction: UINT): D3D12_FILTER;
var
  Ltmp: D3D12_FILTER;

begin
  Ltmp := D3D12_ENCODE_BASIC_FILTER(UINT(D3D12_FILTER_TYPE_LINEAR),
                                    UINT(D3D12_FILTER_TYPE_LINEAR),
                                    UINT(D3D12_FILTER_TYPE_LINEAR),
                                    reduction);

  Result := D3D12_FILTER(D3D12_ANISOTROPIC_FILTERING_BIT or UINT(Ltmp));
end;


function D3D12_ENCODE_MIN_MAG_ANISOTROPIC_MIP_POINT_FILTER(reduction: UINT): D3D12_FILTER;
var
  LTmp: D3D12_FILTER;

begin
  Ltmp :=  D3D12_ENCODE_BASIC_FILTER(UINT(D3D12_FILTER_TYPE_LINEAR),
                                     UINT(D3D12_FILTER_TYPE_LINEAR),
                                     UINT(D3D12_FILTER_TYPE_POINT),
                                     reduction);

  Result := D3D12_FILTER(D3D12_ANISOTROPIC_FILTERING_BIT or UINT(Ltmp));
end;


function D3D12_DECODE_MIN_FILTER(D3D12Filter: UINT): D3D12_FILTER_TYPE;
begin
  Result := D3D12_FILTER_TYPE((D3D12Filter shr D3D12_MIN_FILTER_SHIFT) and
                               D3D12_FILTER_TYPE_MASK);
end;


function D3D12_DECODE_MAG_FILTER(D3D12Filter: UINT): D3D12_FILTER_TYPE;
begin
  Result := D3D12_FILTER_TYPE((D3D12Filter shr D3D12_MAG_FILTER_SHIFT) and
                              D3D12_FILTER_TYPE_MASK);
end;


function D3D12_DECODE_MIP_FILTER(D3D12Filter: UINT): D3D12_FILTER_TYPE;
begin
  Result := D3D12_FILTER_TYPE((D3D12Filter shr D3D12_MIP_FILTER_SHIFT) and
                              D3D12_FILTER_TYPE_MASK);
end;


function D3D12_DECODE_FILTER_REDUCTION(D3D12Filter: UINT): D3D12_FILTER_REDUCTION_TYPE;
begin
  Result := D3D12_FILTER_REDUCTION_TYPE((D3D12Filter shr D3D12_FILTER_REDUCTION_TYPE_SHIFT) and
                                        D3D12_FILTER_REDUCTION_TYPE_MASK);
end;


function D3D12_DECODE_IS_COMPARISON_FILTER(D3D12Filter: UINT): Boolean;
begin
  Result := D3D12_DECODE_FILTER_REDUCTION(D3D12Filter) = D3D12_FILTER_REDUCTION_TYPE_COMPARISON;
end;


function D3D12_DECODE_IS_ANISOTROPIC_FILTER(D3D12Filter: UINT): Boolean; inline;
begin
  // Update May 2024
  Result := (Boolean(D3D12Filter and D3D12_ANISOTROPIC_FILTERING_BIT) and
            (D3D12_FILTER_TYPE_LINEAR = D3D12_DECODE_MIN_FILTER(D3D12Filter)) and
            (D3D12_FILTER_TYPE_LINEAR = D3D12_DECODE_MAG_FILTER(D3D12Filter)));
end;


// Helpers for D3D12_RESOURCE_UAV_BARRIER

function D3D12_RESOURCE_UAV_BARRIER.GetpResource: ID3D12Resource;
begin
  Result := ID3D12Resource(FResource);
end;


procedure D3D12_RESOURCE_UAV_BARRIER.SetpResource(Value: ID3D12Resource);
begin
  FResource := Pointer(Value);
end;


// Helpers for D3D12_RESOURCE_ALIASING_BARRIER

function D3D12_RESOURCE_ALIASING_BARRIER.GetpResourceAfter: ID3D12Resource;
begin
  Result := ID3D12Resource(FResourceAfter);
end;


function D3D12_RESOURCE_ALIASING_BARRIER.GetpResourceBefore: ID3D12Resource;
begin
  Result := ID3D12Resource(FResourceBefore);
end;


procedure D3D12_RESOURCE_ALIASING_BARRIER.SetpResourceAfter(Value: ID3D12Resource);
begin
  FResourceAfter := Pointer(Value);
end;


procedure D3D12_RESOURCE_ALIASING_BARRIER.SetpResourceBefore(Value: ID3D12Resource);
begin
  FResourceBefore := Pointer(Value);
end;


// Helpers for D3D12_RESOURCE_TRANSITION_BARRIER

function D3D12_RESOURCE_TRANSITION_BARRIER.GetpResource: ID3D12Resource;
begin
  Result := ID3D12Resource(FResource);
end;


procedure D3D12_RESOURCE_TRANSITION_BARRIER.SetpResource(Value: ID3D12Resource);
begin
  FResource := Pointer(Value);
end;


// D3D12_RAYTRACING_INSTANCE_DESC
function D3D12_RAYTRACING_INSTANCE_DESC.GetBits(const aIndex: Integer): Integer;
begin
  Result := Integer((Flags_ shr (aIndex shr 8)) and ((UInt64(1) shl Byte(aIndex))-1));
end;


procedure D3D12_RAYTRACING_INSTANCE_DESC.SetBits(const aIndex: Integer;
                                                 const aValue: Integer);
begin
  Flags_ := (Flags_ and not(((UInt64(1) shl Byte(aIndex))-1) shl (aIndex shr 8))) or ((UInt64(aValue) and ((UInt64(1) shl Byte(aIndex))-1)) shl (aIndex shr 8));
end;
// -----------------------------------------------------------------------------


// Debug
function D3D12_MAKE_COARSE_SHADING_RATE(x: Integer;
                                        y: Integer): Integer;
begin
  Result := ( (x) shl D3D12_SHADING_RATE_X_AXIS_SHIFT or (y) );
end;


function D3D12_GET_COARSE_SHADING_RATE_X_AXIS(x: Integer): Integer;
begin
  Result := (( (x) shr D3D12_SHADING_RATE_X_AXIS_SHIFT) and D3D12_SHADING_RATE_VALID_MASK);
end;


function D3D12_GET_COARSE_SHADING_RATE_Y_AXIS(y: Integer): Integer;
begin
  Result := ( (y) and D3D12_SHADING_RATE_VALID_MASK);
end;

end.

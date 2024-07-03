// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D12Shader.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description: D3D12 Shader Types and APIs.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D12 is outdated!
//          Search "Update May 2024", to find updates until May 2024.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: D3D12Shader.h
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
unit WinApi.DirectX.D3D12Shader;

interface

{$HPPEMIT '#include "d3dcommon.h"'}
{$HPPEMIT '#include "D3D12Shader.h"'}

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {DirectX}
  WinApi.DirectX.D3DCommon,
  WinApi.DirectX.D3D12;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type
  PD3D12_SHADER_VERSION_TYPE = ^D3D12_SHADER_VERSION_TYPE;
  D3D12_SHADER_VERSION_TYPE = (
    D3D12_SHVER_PIXEL_SHADER          = 0,
    D3D12_SHVER_VERTEX_SHADER         = 1,
    D3D12_SHVER_GEOMETRY_SHADER       = 2,
    // D3D11 Shaders
    D3D12_SHVER_HULL_SHADER           = 3,
    D3D12_SHVER_DOMAIN_SHADER         = 4,
    D3D12_SHVER_COMPUTE_SHADER        = 5,
    // D3D12 Shaders
    D3D12_SHVER_LIBRARY               = 6,
    D3D12_SHVER_RAY_GENERATION_SHADER = 7,
    D3D12_SHVER_INTERSECTION_SHADER   = 8,
    D3D12_SHVER_ANY_HIT_SHADER        = 9,
    D3D12_SHVER_CLOSEST_HIT_SHADER    = 10,
    D3D12_SHVER_MISS_SHADER           = 11,
    D3D12_SHVER_CALLABLE_SHADER       = 12,
    D3D12_SHVER_MESH_SHADER           = 13,
    D3D12_SHVER_AMPLIFICATION_SHADER  = 14,
    D3D12_SHVER_RESERVED0             = $FFF0
  );
  {$EXTERNALSYM D3D12_SHADER_VERSION_TYPE}


  // Converted macro's
  // =================
  function D3D12_SHVER_GET_TYPE(_Version: UINT): UINT; inline;
  {$EXTERNALSYM D3D12_SHVER_GET_TYPE}

  function D3D12_SHVER_GET_MAJOR(_Version: UINT): UINT; inline;
  {$EXTERNALSYM D3D12_SHVER_GET_MAJOR}

  function D3D12_SHVER_GET_MINOR(_Version: UINT): UINT; inline;
  {$EXTERNALSYM D3D12_SHVER_GET_MINOR}


const
  // Slot ID for library function return
  D3D_RETURN_PARAMETER_INDEX = -1;
  {$EXTERNALSYM D3D_RETURN_PARAMETER_INDEX}


type

  PD3D12_RESOURCE_RETURN_TYPE = ^D3D12_RESOURCE_RETURN_TYPE;
  D3D12_RESOURCE_RETURN_TYPE = D3D_RESOURCE_RETURN_TYPE;
  {$EXTERNALSYM D3D12_RESOURCE_RETURN_TYPE}

  PD3D12_CBUFFER_TYPE = ^D3D12_CBUFFER_TYPE;
  D3D12_CBUFFER_TYPE = D3D_CBUFFER_TYPE;
  {$EXTERNALSYM D3D12_CBUFFER_TYPE}


  PD3D12_SIGNATURE_PARAMETER_DESC = ^D3D12_SIGNATURE_PARAMETER_DESC;
  _D3D12_SIGNATURE_PARAMETER_DESC = record
    SemanticName: PAnsiChar;                      // Name of the semantic
    SemanticIndex: UINT;                          // Index of the semantic
    Register: UINT;                               // Number of member variables
    SystemValueType: D3D_NAME;                    // A predefined system value, or D3D_NAME_UNDEFINED if not applicable
    ComponentType: D3D_REGISTER_COMPONENT_TYPE;   // Scalar type (e.g. uint, float, etc.)
    Mask: Byte;                                   // Mask to indicate which components of the register
                                                  // are used (combination of D3D10_COMPONENT_MASK values)
    ReadWriteMask: Byte;                          // Mask to indicate whether a given component is
                                                  // never written (if this is an output signature) or
                                                  // always read (if this is an input signature).
                                                  // (combination of D3D_MASK_* values)
    Stream: UINT;                                 // Stream index
    MinPrecision: D3D_MIN_PRECISION;              // Minimum desired interpolation precision
  end;
  {$EXTERNALSYM _D3D12_SIGNATURE_PARAMETER_DESC}
  D3D12_SIGNATURE_PARAMETER_DESC = _D3D12_SIGNATURE_PARAMETER_DESC;
  {$EXTERNALSYM D3D12_SIGNATURE_PARAMETER_DESC}


  PD3D12_SHADER_BUFFER_DESC = ^D3D12_SHADER_BUFFER_DESC;
  _D3D12_SHADER_BUFFER_DESC = record
    Name: PAnsiChar;                 // Name of the constant buffer
    Type_: D3D_CBUFFER_TYPE;          // Indicates type of buffer content
    Variables: UINT;                 // Number of member variables
    Size: UINT;                      // Size of CB (in bytes)
    uFlags: UINT;                   // Buffer description flags
  end;
  {$EXTERNALSYM _D3D12_SHADER_BUFFER_DESC}
  D3D12_SHADER_BUFFER_DESC = _D3D12_SHADER_BUFFER_DESC;
  {$EXTERNALSYM D3D12_SHADER_BUFFER_DESC}


  PD3D12_SHADER_VARIABLE_DESC = ^D3D12_SHADER_VARIABLE_DESC;
  _D3D12_SHADER_VARIABLE_DESC = record
    Name: PAnsiChar;                 // Name of the variable
    StartOffset: UINT;               // Offset in constant buffer's backing store
    Size: UINT;                      // Size of variable (in bytes)
    uFlags: UINT;                    // Variable flags
    DefaultValue: Pointer;           // Raw pointer to default value
    StartTexture: UINT;              // First texture index (or -1 if no textures used)
    TextureSize: UINT;               // Number of texture slots possibly used.
    StartSampler: UINT;              // First sampler index (or -1 if no textures used)
    SamplerSize: UINT;               // Number of sampler slots possibly used.
  end;
  {$EXTERNALSYM _D3D12_SHADER_VARIABLE_DESC}
  D3D12_SHADER_VARIABLE_DESC = _D3D12_SHADER_VARIABLE_DESC;
  {$EXTERNALSYM D3D12_SHADER_VARIABLE_DESC}


  PD3D12_SHADER_TYPE_DESC = ^D3D12_SHADER_TYPE_DESC;
  _D3D12_SHADER_TYPE_DESC = record
    Class_: D3D_SHADER_VARIABLE_CLASS;  // Variable class (e.g. object, matrix, etc.)
    Type_: D3D_SHADER_VARIABLE_TYPE;    // Variable type (e.g. float, sampler, etc.)
    Rows: UINT;                         // Number of rows (for matrices, 1 for other numeric, 0 if not applicable)
    Columns: UINT;                      // Number of columns (for vectors  matrices, 1 for other numeric, 0 if not applicable)
    Elements: UINT;                     // Number of elements (0 if not an array)
    Members: UINT;                      // Number of members (0 if not a structure)
    Offset: UINT;                       // Offset from the start of structure (0 if not a structure member)
    Name: PAnsiChar;                    // Name of type, can be NULL
  end;
  {$EXTERNALSYM _D3D12_SHADER_TYPE_DESC}
  D3D12_SHADER_TYPE_DESC = _D3D12_SHADER_TYPE_DESC;
  {$EXTERNALSYM D3D12_SHADER_TYPE_DESC}


  PD3D12_TESSELLATOR_DOMAIN = ^D3D12_TESSELLATOR_DOMAIN;
  D3D12_TESSELLATOR_DOMAIN = D3D_TESSELLATOR_DOMAIN;
  {$EXTERNALSYM D3D12_TESSELLATOR_DOMAIN}


  PD3D12_TESSELLATOR_PARTITIONING = ^D3D12_TESSELLATOR_PARTITIONING;
  D3D12_TESSELLATOR_PARTITIONING = D3D_TESSELLATOR_PARTITIONING;
  {$EXTERNALSYM D3D12_TESSELLATOR_PARTITIONING}


  PD3D12_TESSELLATOR_OUTPUT_PRIMITIVE = ^D3D12_TESSELLATOR_OUTPUT_PRIMITIVE;
  D3D12_TESSELLATOR_OUTPUT_PRIMITIVE = D3D_TESSELLATOR_OUTPUT_PRIMITIVE;
  {$EXTERNALSYM D3D12_TESSELLATOR_OUTPUT_PRIMITIVE}


  PD3D12_SHADER_DESC = ^D3D12_SHADER_DESC;
  _D3D12_SHADER_DESC = record
    Version: UINT;                                         // Shader version
    Creator: LPCSTR;                                       // Creator string
    Flags: UINT;                                           // Shader compilation/parse flags
    ConstantBuffers: UINT;                                 // Number of constant buffers
    BoundResources: UINT;                                  // Number of bound resources
    InputParameters: UINT;                                 // Number of parameters in the input signature
    OutputParameters: UINT;                                // Number of parameters in the output signature
    InstructionCount: UINT;                                // Number of emitted instructions
    TempRegisterCount: UINT;                               // Number of temporary registers used
    TempArrayCount: UINT;                                  // Number of temporary arrays used
    DefCount: UINT;                                        // Number of constant defines
    DclCount: UINT;                                        // Number of declarations (input + output)
    TextureNormalInstructions: UINT;                       // Number of non-categorized texture instructions
    TextureLoadInstructions: UINT;                         // Number of texture load instructions
    TextureCompInstructions: UINT;                         // Number of texture comparison instructions
    TextureBiasInstructions: UINT;                         // Number of texture bias instructions
    TextureGradientInstructions: UINT;                     // Number of texture gradient instructions
    FloatInstructionCount: UINT;                           // Number of floating point arithmetic instructions used
    IntInstructionCount: UINT;                             // Number of signed integer arithmetic instructions used
    UintInstructionCount: UINT;                            // Number of unsigned integer arithmetic instructions used
    StaticFlowControlCount: UINT;                          // Number of static flow control instructions used
    DynamicFlowControlCount: UINT;                         // Number of dynamic flow control instructions used
    MacroInstructionCount: UINT;                           // Number of macro instructions used
    ArrayInstructionCount: UINT;                           // Number of array instructions used
    CutInstructionCount: UINT;                             // Number of cut instructions used
    EmitInstructionCount: UINT;                            // Number of emit instructions used
    GSOutputTopology: D3D_PRIMITIVE_TOPOLOGY;              // Geometry shader output topology
    GSMaxOutputVertexCount: UINT;                          // Geometry shader maximum output vertex count
    InputPrimitive: D3D_PRIMITIVE;                         // GS/HS input primitive
    PatchConstantParameters: UINT;                         // Number of parameters in the patch constant signature
    cGSInstanceCount: UINT;                                // Number of Geometry shader instances
    cControlPoints: UINT;                                  // Number of control points in the HS->DS stage
    HSOutputPrimitive: D3D_TESSELLATOR_OUTPUT_PRIMITIVE;   // Primitive output by the tessellator
    HSPartitioning: D3D_TESSELLATOR_PARTITIONING;          // Partitioning mode of the tessellator
    TessellatorDomain: D3D_TESSELLATOR_DOMAIN;             // Domain of the tessellator (quad, tri, isoline)
                                                           // instruction counts
    cBarrierInstructions: UINT;                            // Number of barrier instructions in a compute shader
    cInterlockedInstructions: UINT;                        // Number of interlocked instructions
    cTextureStoreInstructions: UINT;                       // Number of texture writes
  end;
  {$EXTERNALSYM _D3D12_SHADER_DESC}
  D3D12_SHADER_DESC = _D3D12_SHADER_DESC;
  {$EXTERNALSYM D3D12_SHADER_DESC}


  PD3D12_SHADER_INPUT_BIND_DESC = ^D3D12_SHADER_INPUT_BIND_DESC;
  _D3D12_SHADER_INPUT_BIND_DESC = record
    Name: LPCSTR;                           // Name of the resource
    Type_: D3D_SHADER_INPUT_TYPE;           // Type of resource (e.g. texture, cbuffer, etc.)
    BindPoint: UINT;                        // Starting bind point
    BindCount: UINT;                        // Number of contiguous bind points (for arrays)
    uFlags: UINT;                           // Input binding flags
    ReturnType: D3D_RESOURCE_RETURN_TYPE;   // Return type (if texture)
    Dimension: D3D_SRV_DIMENSION;           // Dimension (if texture)
    NumSamples: UINT;                       // Number of samples (0 if not MS texture)
    Space: UINT;                            // Register space
    uID: UINT;                              // Range ID in the bytecode
  end;
  {$EXTERNALSYM _D3D12_SHADER_INPUT_BIND_DESC}
  D3D12_SHADER_INPUT_BIND_DESC = _D3D12_SHADER_INPUT_BIND_DESC;
  {$EXTERNALSYM D3D12_SHADER_INPUT_BIND_DESC}

const

  D3D_SHADER_REQUIRES_DOUBLES                                       = $00000001;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_DOUBLES}
  D3D_SHADER_REQUIRES_EARLY_DEPTH_STENCIL                           = $00000002;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_EARLY_DEPTH_STENCIL}
  D3D_SHADER_REQUIRES_UAVS_AT_EVERY_STAGE                           = $00000004;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_UAVS_AT_EVERY_STAGE}
  D3D_SHADER_REQUIRES_64_UAVS                                       = $00000008;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_64_UAVS}
  D3D_SHADER_REQUIRES_MINIMUM_PRECISION                             = $00000010;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_MINIMUM_PRECISION}
  D3D_SHADER_REQUIRES_11_1_DOUBLE_EXTENSIONS                        = $00000020;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_11_1_DOUBLE_EXTENSIONS}
  D3D_SHADER_REQUIRES_11_1_SHADER_EXTENSIONS                        = $00000040;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_11_1_SHADER_EXTENSIONS}
  D3D_SHADER_REQUIRES_LEVEL_9_COMPARISON_FILTERING                  = $00000080;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_LEVEL_9_COMPARISON_FILTERING}
  D3D_SHADER_REQUIRES_TILED_RESOURCES                               = $00000100;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_TILED_RESOURCES}
  D3D_SHADER_REQUIRES_STENCIL_REF                                   = $00000200;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_STENCIL_REF}
  D3D_SHADER_REQUIRES_INNER_COVERAGE                                = $00000400;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_INNER_COVERAGE}
  D3D_SHADER_REQUIRES_TYPED_UAV_LOAD_ADDITIONAL_FORMATS             = $00000800;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_TYPED_UAV_LOAD_ADDITIONAL_FORMATS}
  D3D_SHADER_REQUIRES_ROVS                                          = $00001000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_ROVS}
  D3D_SHADER_REQUIRES_VIEWPORT_AND_RT_ARRAY_INDEX_FROM_ANY_SHADER_FEEDING_RASTERIZER = $00002000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_VIEWPORT_AND_RT_ARRAY_INDEX_FROM_ANY_SHADER_FEEDING_RASTERIZER}
  D3D_SHADER_REQUIRES_WAVE_OPS                                      = $00004000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_WAVE_OPS}
  D3D_SHADER_REQUIRES_INT64_OPS                                     = $00008000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_INT64_OPS}
  D3D_SHADER_REQUIRES_VIEW_ID                                       = $00010000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_VIEW_ID}
  D3D_SHADER_REQUIRES_BARYCENTRICS                                  = $00020000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_BARYCENTRICS}
  D3D_SHADER_REQUIRES_NATIVE_16BIT_OPS                              = $00040000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_NATIVE_16BIT_OPS}
  D3D_SHADER_REQUIRES_SHADING_RATE                                  = $00080000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_SHADING_RATE}
  D3D_SHADER_REQUIRES_RAYTRACING_TIER_1_1                           = $00100000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_RAYTRACING_TIER_1_1}
  D3D_SHADER_REQUIRES_SAMPLER_FEEDBACK                              = $00200000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_SAMPLER_FEEDBACK}
  D3D_SHADER_REQUIRES_ATOMIC_INT64_ON_TYPED_RESOURC                 = $00400000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_ATOMIC_INT64_ON_TYPED_RESOURC}
  D3D_SHADER_REQUIRES_ATOMIC_INT64_ON_GROUP_SHARED                  = $00800000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_ATOMIC_INT64_ON_GROUP_SHARED}
  D3D_SHADER_REQUIRES_DERIVATIVES_IN_MESH_AND_AMPLIFICATION_SHADERS = $01000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_DERIVATIVES_IN_MESH_AND_AMPLIFICATION_SHADERS}
  D3D_SHADER_REQUIRES_RESOURCE_DESCRIPTOR_HEAP_INDEXING             = $02000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_RESOURCE_DESCRIPTOR_HEAP_INDEXING}
  D3D_SHADER_REQUIRES_SAMPLER_DESCRIPTOR_HEAP_INDEXING              = $04000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_SAMPLER_DESCRIPTOR_HEAP_INDEXING}
  D3D_SHADER_REQUIRES_WAVE_MMA                                      = $08000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_WAVE_MMA}
  D3D_SHADER_REQUIRES_ATOMIC_INT64_ON_DESCRIPTOR_HEAP_RESOURCE      = $10000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_ATOMIC_INT64_ON_DESCRIPTOR_HEAP_RESOURCE}

  // Update May 2024
  D3D_SHADER_REQUIRES_ADVANCED_TEXTURE_OPS                          = $20000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_ADVANCED_TEXTURE_OPS}
  D3D_SHADER_REQUIRES_WRITEABLE_MSAA_TEXTURES                       = $40000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_WRITEABLE_MSAA_TEXTURES}
  D3D_SHADER_REQUIRES_SAMPLE_CMP_GRADIENT_OR_BIAS                   = $80000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_SAMPLE_CMP_GRADIENT_OR_BIAS}
  D3D_SHADER_REQUIRES_EXTENDED_COMMAND_INFO                         = $100000000;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_EXTENDED_COMMAND_INFO}

  // ==


type

  PD3D12_LIBRARY_DESC = ^D3D12_LIBRARY_DESC;
  _D3D12_LIBRARY_DESC = record
    Creator: LPCSTR;                 // The name of the originator of the library.
    Flags: UINT;                     // Compilation flags.
    FunctionCount: UINT;             // Number of functions exported from the library.
  end;
  {$EXTERNALSYM _D3D12_LIBRARY_DESC}
  D3D12_LIBRARY_DESC = _D3D12_LIBRARY_DESC;
  {$EXTERNALSYM D3D12_LIBRARY_DESC}


  PD3D12_FUNCTION_DESC = ^D3D12_FUNCTION_DESC;
  _D3D12_FUNCTION_DESC = record
    Version: UINT;                        // Shader version
    Creator: LPCSTR;                      // Creator string
    Flags: UINT;                          // Shader compilation/parse flags
    ConstantBuffers: UINT;                // Number of constant buffers
    BoundResources: UINT;                 // Number of bound resources
    InstructionCount: UINT;               // Number of emitted instructions
    TempRegisterCount: UINT;              // Number of temporary registers used
    TempArrayCount: UINT;                 // Number of temporary arrays used
    DefCount: UINT;                       // Number of constant defines
    DclCount: UINT;                       // Number of declarations (input + output)
    TextureNormalInstructions: UINT;      // Number of non-categorized texture instructions
    TextureLoadInstructions: UINT;        // Number of texture load instructions
    TextureCompInstructions: UINT;        // Number of texture comparison instructions
    TextureBiasInstructions: UINT;        // Number of texture bias instructions
    TextureGradientInstructions: UINT;    // Number of texture gradient instructions
    FloatInstructionCount: UINT;          // Number of floating point arithmetic instructions used
    IntInstructionCount: UINT;            // Number of signed integer arithmetic instructions used
    UintInstructionCount: UINT;           // Number of unsigned integer arithmetic instructions used
    StaticFlowControlCount: UINT;         // Number of static flow control instructions used
    DynamicFlowControlCount: UINT;        // Number of dynamic flow control instructions used
    MacroInstructionCount: UINT;          // Number of macro instructions used
    ArrayInstructionCount: UINT;          // Number of array instructions used
    MovInstructionCount: UINT;            // Number of mov instructions used
    MovcInstructionCount: UINT;           // Number of movc instructions used
    ConversionInstructionCount: UINT;     // Number of type conversion instructions used
    BitwiseInstructionCount: UINT;        // Number of bitwise arithmetic instructions used
    MinFeatureLevel: D3D_FEATURE_LEVEL;   // Min target of the function byte code
    RequiredFeatureFlags: UINT64;         // Required feature flags
    Name: PAnsiChar;                      // Function name
    FunctionParameterCount: INT;          // Number of logical parameters in the function signature (not including return)
    HasReturn: BOOL;                      // TRUE, if function returns a value, False - it is a subroutine
    Has10Level9VertexShader: BOOL;        // TRUE, if there is a 10L9 VS blob
    Has10Level9PixelShader: BOOL;         // TRUE, if there is a 10L9 PS blob
  end;
  {$EXTERNALSYM _D3D12_FUNCTION_DESC}
  D3D12_FUNCTION_DESC = _D3D12_FUNCTION_DESC;
  {$EXTERNALSYM D3D12_FUNCTION_DESC}


  PD3D12_PARAMETER_DESC = ^D3D12_PARAMETER_DESC;
  _D3D12_PARAMETER_DESC = record
    Name: LPCSTR;                                // Parameter name.
    SemanticName: LPCSTR;                        // Parameter semantic name (+index).
    Type_: D3D_SHADER_VARIABLE_TYPE;             // Element type.
    Class_: D3D_SHADER_VARIABLE_CLASS;           // Scalar/Vector/Matrix.
    Rows: UINT;                                  // Rows are for matrix parameters.
    Columns: UINT;                               // Components or Columns in matrix.
    InterpolationMode: D3D_INTERPOLATION_MODE;   // Interpolation mode.
    Flags: D3D_PARAMETER_FLAGS;                  // Parameter modifiers.
    FirstInRegister: UINT;                       // The first input register for this parameter.
    FirstInComponent: UINT;                      // The first input register component for this parameter.
    FirstOutRegister: UINT;                      // The first output register for this parameter.
    FirstOutComponent: UINT;                     // The first output register component for this parameter.
  end;
  {$EXTERNALSYM _D3D12_PARAMETER_DESC}
  D3D12_PARAMETER_DESC = _D3D12_PARAMETER_DESC;
  {$EXTERNALSYM D3D12_PARAMETER_DESC}



  //////////////////////////////////////////////////////////////////////////////
  // Interfaces ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // forward declarations
  ID3D12ShaderReflectionType = interface;
  PID3D12ShaderReflectionType = ^ID3D12ShaderReflectionType;
  LPD3D12SHADERREFLECTIONTYPE = PID3D12ShaderReflectionType;
  {$EXTERNALSYM LPD3D12SHADERREFLECTIONTYPE}

  ID3D12ShaderReflectionVariable = interface;
  PID3D12ShaderReflectionVariable = ^ID3D12ShaderReflectionVariable;
  LPD3D12SHADERREFLECTIONVARIABLE = PID3D12ShaderReflectionVariable;
  {$EXTERNALSYM LPD3D12SHADERREFLECTIONVARIABLE}

  ID3D12ShaderReflectionConstantBuffer = interface;
  PID3D12ShaderReflectionConstantBuffer = ^ID3D12ShaderReflectionConstantBuffer;
  LPD3D12SHADERREFLECTIONCONSTANTBUFFER = PID3D12ShaderReflectionConstantBuffer;
  {$EXTERNALSYM LPD3D12SHADERREFLECTIONCONSTANTBUFFER}

  ID3D12ShaderReflection = interface;
  PID3D12ShaderReflection = ^ID3D12ShaderReflection;
  LPD3D12SHADERREFLECTION = PID3D12ShaderReflection;
  {$EXTERNALSYM LPD3D12SHADERREFLECTION}

  ID3D12LibraryReflection = interface;
  PID3D12LibraryReflection = ^ID3D12LibraryReflection;
  LPD3D12LIBRARYREFLECTION = PID3D12LibraryReflection;
  {$EXTERNALSYM LPD3D12LIBRARYREFLECTION}

  ID3D12FunctionReflection = interface;
  PID3D12FunctionReflection = ^ID3D12FunctionReflection;
  LPD3D12FUNCTIONREFLECTION = PID3D12FunctionReflection;
  {$EXTERNALSYM LPD3D12FUNCTIONREFLECTION}

  ID3D12FunctionParameterReflection = interface;
  PID3D12FunctionParameterReflection = ^ID3D12FunctionParameterReflection;
  LPD3D12FUNCTIONPARAMETERREFLECTION = PID3D12FunctionParameterReflection;
  {$EXTERNALSYM LPD3D12FUNCTIONPARAMETERREFLECTION}


  // Interface ID3D12ShaderReflectionType
  // =====================================
  // This shader-reflection interface provides access to variable type.
  // See: https://learn.microsoft.com/en-us/windows/win32/api/d3d12shader/nn-d3d12shader-id3d12shaderreflectiontype
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12ShaderReflectionType);'}
  {$EXTERNALSYM ID3D12ShaderReflectionType}
  ID3D12ShaderReflectionType = Interface
    ['{E913C351-783D-48CA-A1D1-4F306284AD56}']

    function GetDesc(out pDesc: D3D12_SHADER_TYPE_DESC): HRESULT; stdcall;

    function GetMemberTypeByIndex(Index: UINT): ID3D12ShaderReflectionType; stdcall;

    function GetMemberTypeByName(Name: LPCSTR): ID3D12ShaderReflectionType; stdcall;

    function GetMemberTypeName(Index: UINT): LPCSTR; stdcall;

    function IsEqual(pType: ID3D12ShaderReflectionType): HRESULT; stdcall;

    function GetSubType(): ID3D12ShaderReflectionType; stdcall;

    function GetBaseClass(): ID3D12ShaderReflectionType; stdcall;

    function GetNumInterfaces(): UINT; stdcall;

    function GetInterfaceByIndex(uIndex: UINT): ID3D12ShaderReflectionType; stdcall;

    function IsOfType(pType: ID3D12ShaderReflectionType): HRESULT; stdcall;

    function ImplementsInterface(pBase: ID3D12ShaderReflectionType): HRESULT; stdcall;

  end;
  IID_ID3D12ShaderReflectionType = ID3D12ShaderReflectionType;
  {$EXTERNALSYM IID_ID3D12ShaderReflectionType}


  // Interface ID3D12ShaderReflectionVariable
  // =========================================
  // This shader-reflection interface provides access to a variable.
  // See: https://learn.microsoft.com/en-us/windows/win32/api/d3d12shader/nn-d3d12shader-id3d12shaderreflectionvariable
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12ShaderReflectionVariable);'}
  {$EXTERNALSYM ID3D12ShaderReflectionVariable}
  ID3D12ShaderReflectionVariable = Interface
    ['{8337A8A6-A216-444A-B2F4-314733A73AEA}']

    function GetDesc(out pDesc: D3D12_SHADER_VARIABLE_DESC): HRESULT; stdcall;

    function GetType: ID3D12ShaderReflectionType; stdcall;

    function GetBuffer: ID3D12ShaderReflectionConstantBuffer; stdcall;

    function GetInterfaceSlot(uArrayIndex: UINT): UINT; stdcall;

  end;
  IID_ID3D12ShaderReflectionVariable = ID3D12ShaderReflectionVariable;
  {$EXTERNALSYM IID_ID3D12ShaderReflectionVariable}


  // Interface ID3D12ShaderReflectionConstantBuffer
  // ===============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12ShaderReflectionConstantBuffer);'}
  {$EXTERNALSYM ID3D12ShaderReflectionConstantBuffer}
  ID3D12ShaderReflectionConstantBuffer = Interface
    ['{C59598B4-48B3-4869-B9B1-B1618B14A8B7}']

    function GetDesc(pDesc: PD3D12_SHADER_BUFFER_DESC): HRESULT; stdcall;

    function GetVariableByIndex(Index: UINT): ID3D12ShaderReflectionVariable; stdcall;

    function GetVariableByName(Name: LPCSTR): ID3D12ShaderReflectionVariable; stdcall;

  end;
  IID_ID3D12ShaderReflectionConstantBuffer = ID3D12ShaderReflectionConstantBuffer;
  {$EXTERNALSYM IID_ID3D12ShaderReflectionConstantBuffer}


  // Interface ID3D12ShaderReflection
  // =================================
  // The ID3D12ShaderReflection IID may change from SDK version to SDK version
  // if the reflection API changes.  This prevents new code with the new API
  // from working with an old binary.  Recompiling with the new header
  // will pick up the new IID.
  //
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12ShaderReflection);'}
  {$EXTERNALSYM ID3D12ShaderReflection}
  ID3D12ShaderReflection = Interface(IUnknown)
    ['{5A58797D-A72C-478D-8BA2-EFC6B0EFE88E}']

    function GetDesc(out pDesc: D3D12_SHADER_DESC ): HRESULT; stdcall;

    function GetConstantBufferByIndex(Index: UINT): ID3D12ShaderReflectionConstantBuffer; stdcall;

    function GetConstantBufferByName(Name: LPCSTR): ID3D12ShaderReflectionConstantBuffer; stdcall;

    function GetResourceBindingDesc(ResourceIndex: UINT;
                                    out pDesc: D3D12_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    function GetInputParameterDesc(ParameterIndex: UINT;
                                   out pDesc: D3D12_SIGNATURE_PARAMETER_DESC): HRESULT; stdcall;

    function GetOutputParameterDesc(ParameterIndex: UINT;
                                    out pDesc: D3D12_SIGNATURE_PARAMETER_DESC): HRESULT; stdcall;

    function GetPatchConstantParameterDesc(ParameterIndex: UINT;
                                           out pDesc: D3D12_SIGNATURE_PARAMETER_DESC): HRESULT; stdcall;

    function GetVariableByName(Name: LPCSTR): ID3D12ShaderReflectionVariable; stdcall;

    function GetResourceBindingDescByName(Name: LPCSTR;
                                          out pDesc: D3D12_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    function GetMovInstructionCount(): UINT; stdcall;

    function GetMovcInstructionCount(): UINT; stdcall;

    function GetConversionInstructionCount(): UINT; stdcall;

    function GetBitwiseInstructionCount(): UINT; stdcall;

    function GetGSInputPrimitive(): D3D_PRIMITIVE; stdcall;

    function IsSampleFrequencyShader(): BOOL; stdcall;

    function GetNumInterfaceSlots(): UINT; stdcall;

    function GetMinFeatureLevel(out pLevel: D3D_FEATURE_LEVEL): HRESULT; stdcall;

    function GetThreadGroupSize(pSizeX: UINT;  {Can be 0}
                                pSizeY: UINT;  {Can be 0}
                                pSizeZ: UINT {Can be 0}): UINT; stdcall;

    function GetRequiresFlags(): UINT64; stdcall;


  end;
  IID_ID3D12ShaderReflection = ID3D12ShaderReflection;
  {$EXTERNALSYM IID_ID3D12ShaderReflection}


  // Interface ID3D12LibraryReflection
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12LibraryReflection);'}
  {$EXTERNALSYM ID3D12LibraryReflection}
  ID3D12LibraryReflection = Interface(IUnknown)
    ['{8E349D19-54DB-4A56-9DC9-119D87BDB804}']

    //STDMETHOD(QueryInterface)(THIS_ _In_ REFIID iid, _Out_ LPVOID * ppv) PURE;
    //STDMETHOD_(ULONG, AddRef)(THIS) PURE;
    //STDMETHOD_(ULONG, Release)(THIS) PURE;

    function GetDesc(out pDesc: D3D12_LIBRARY_DESC): HRESULT; stdcall;

    function GetFunctionByIndex(FunctionIndex: INT32): ID3D12FunctionReflection; stdcall;

  end;
  IID_ID3D12LibraryReflection = ID3D12LibraryReflection;
  {$EXTERNALSYM IID_ID3D12LibraryReflection}


  // Interface ID3D12FunctionReflection
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12FunctionReflection);'}
  {$EXTERNALSYM ID3D12FunctionReflection}
  ID3D12FunctionReflection = Interface
    ['{1108795C-2772-4BA9-B2A8-D464DC7E2799}']

    function GetDesc(out pDesc: D3D12_FUNCTION_DESC): HRESULT; stdcall;

    function GetConstantBufferByIndex(BufferIndex: UINT): ID3D12ShaderReflectionConstantBuffer; stdcall;

    function GetConstantBufferByName(Name: LPCSTR): ID3D12ShaderReflectionConstantBuffer; stdcall;

    function GetResourceBindingDesc(ResourceIndex: UINT;
                                    out pDesc: D3D12_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    function GetVariableByName(Name: LPCSTR): ID3D12ShaderReflectionVariable; stdcall;

    function GetResourceBindingDescByName(Name: LPCSTR;
                                          out pDesc: D3D12_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    // Use D3D_RETURN_PARAMETER_INDEX to get description of the return value.
    function GetFunctionParameter(ParameterIndex: INT32): ID3D12FunctionParameterReflection; stdcall;


  end;
  IID_ID3D12FunctionReflection = ID3D12FunctionReflection;
  {$EXTERNALSYM IID_ID3D12FunctionReflection}


  // Interface ID3D12FunctionParameterReflection
  // ============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12FunctionParameterReflection);'}
  {$EXTERNALSYM ID3D12FunctionParameterReflection}
  ID3D12FunctionParameterReflection = Interface
    ['{EC25F42D-7006-4F2B-B33E-02CC3375733F}']

    function GetDesc(out pDesc: D3D12_PARAMETER_DESC): HRESULT; stdcall;

  end;
  IID_ID3D12FunctionParameterReflection = ID3D12FunctionParameterReflection;
  {$EXTERNALSYM IID_ID3D12FunctionParameterReflection}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.


// Converted macro's ///////////////////////////////////////////////////////////

function D3D12_SHVER_GET_TYPE(_Version: UINT): UINT;
begin
  Result := (_Version shr 16) and $FFFF;
end;

function D3D12_SHVER_GET_MAJOR(_Version: UINT): UINT;
begin
  Result := (_Version shr 4) and $F;
end;

function D3D12_SHVER_GET_MINOR(_Version: UINT): UINT;
begin
  Result := (_Version shr 0) and $F;
end;

////////////////////////////////////////////////////////////////////////////////

end.

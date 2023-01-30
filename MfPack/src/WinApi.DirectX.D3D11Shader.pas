// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D11Shader.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2022
// Language: ENU
//
// Revision Version: 3.1.4
// Description: D3D11 Shader Types and APIs.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
// 31/08/2022                     Updated to latest D3D11 SDK 10.0.22621.0 version.
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D11 is outdated!
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: D3D11Shader.h
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
unit WinApi.DirectX.D3D11Shader;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Types,
  {DirectX}
  WinApi.DirectX.D3DCommon;

  {$WEAKPACKAGEUNIT ON}
  {$ALIGN ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type

  PD3D11_SHADER_VERSION_TYPE = ^D3D11_SHADER_VERSION_TYPE;
  D3D11_SHADER_VERSION_TYPE     = (
    D3D11_SHVER_PIXEL_SHADER    = 0,
    D3D11_SHVER_VERTEX_SHADER   = 1,
    D3D11_SHVER_GEOMETRY_SHADER = 2,
    // D3D11 Shaders
    D3D11_SHVER_HULL_SHADER     = 3,
    D3D11_SHVER_DOMAIN_SHADER   = 4,
    D3D11_SHVER_COMPUTE_SHADER  = 5,
    D3D11_SHVER_RESERVED0       = $FFF0
  );
  {$EXTERNALSYM D3D11_SHADER_VERSION_TYPE}

  {VERSION MACRO'S}
  function D3D11_SHVER_GET_TYPE(_Version: DWORD): DWORD; inline;
  function D3D11_SHVER_GET_MAJOR(_Version: DWORD): DWORD; inline;
  function D3D11_SHVER_GET_MINOR(_Version: DWORD): DWORD; inline;

const
  // Slot ID for library function return
  D3D_RETURN_PARAMETER_INDEX          = (- 1);
  {$EXTERNALSYM D3D_RETURN_PARAMETER_INDEX}

type
  PD3D11_RESOURCE_RETURN_TYPE = ^D3D11_RESOURCE_RETURN_TYPE;
  D3D11_RESOURCE_RETURN_TYPE = D3D_RESOURCE_RETURN_TYPE;
  {$EXTERNALSYM D3D11_RESOURCE_RETURN_TYPE}

  PD3D11_CBUFFER_TYPE = ^D3D11_CBUFFER_TYPE;
  D3D11_CBUFFER_TYPE = D3D_CBUFFER_TYPE;
  {$EXTERNALSYM D3D11_CBUFFER_TYPE}


  PD3D11_SIGNATURE_PARAMETER_DESC = ^D3D11_SIGNATURE_PARAMETER_DESC;
  _D3D11_SIGNATURE_PARAMETER_DESC = record
    SemanticName: LPCSTR;                         // Name of the semantic
    SemanticIndex: UINT;                          // Index of the semantic
    Register_: UINT;                              // Number of member variables
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
  {$EXTERNALSYM _D3D11_SIGNATURE_PARAMETER_DESC}
  D3D11_SIGNATURE_PARAMETER_DESC = _D3D11_SIGNATURE_PARAMETER_DESC;
  {$EXTERNALSYM D3D11_SIGNATURE_PARAMETER_DESC}


  PD3D11_SHADER_BUFFER_DESC = ^D3D11_SHADER_BUFFER_DESC;
  _D3D11_SHADER_BUFFER_DESC = record
    Name: LPCSTR;                    // Name of the constant buffer
    Type_: D3D_CBUFFER_TYPE;         // Indicates type of buffer content
    Variables: UINT;                 // Number of member variables
    Size: UINT;                      // Size of CB (in bytes)
    uFlags: UINT;                    // Buffer description flags
  end;
  {$EXTERNALSYM _D3D11_SHADER_BUFFER_DESC}
  D3D11_SHADER_BUFFER_DESC = _D3D11_SHADER_BUFFER_DESC;
  {$EXTERNALSYM D3D11_SHADER_BUFFER_DESC}


  PD3D11_SHADER_VARIABLE_DESC = ^D3D11_SHADER_VARIABLE_DESC;
  _D3D11_SHADER_VARIABLE_DESC = record
    Name: LPCSTR;                    // Name of the variable
    StartOffset: UINT;               // Offset in constant buffer's backing store
    Size: UINT;                      // Size of variable (in bytes)
    uFlags: UINT;                    // Variable flags
    DefaultValue: Pointer;           // Raw pointer to default value
    StartTexture: UINT;              // First texture index (or -1 if no textures used)
    TextureSize: UINT;               // Number of texture slots possibly used.
    StartSampler: UINT;              // First sampler index (or -1 if no textures used)
    SamplerSize: UINT;               // Number of sampler slots possibly used.
  end;
  {$EXTERNALSYM _D3D11_SHADER_VARIABLE_DESC}
  D3D11_SHADER_VARIABLE_DESC = _D3D11_SHADER_VARIABLE_DESC;
  {$EXTERNALSYM D3D11_SHADER_VARIABLE_DESC}


  PD3D11_SHADER_TYPE_DESC = ^D3D11_SHADER_TYPE_DESC;
  _D3D11_SHADER_TYPE_DESC = record
    Class_: D3D_SHADER_VARIABLE_CLASS;  // Variable class (e.g. object, matrix, etc.)
    Type_: D3D_SHADER_VARIABLE_TYPE;    // Variable type (e.g. float, sampler, etc.)
    Rows: UINT;                         // Number of rows (for matrices, 1 for other numeric, 0 if not applicable)
    Columns: UINT;                      // Number of columns (for vectors  matrices, 1 for other numeric, 0 if not applicable)
    Elements: UINT;                     // Number of elements (0 if not an array)
    Members: UINT;                      // Number of members (0 if not a structure)
    Offset: UINT;                       // Offset from the start of structure (0 if not a structure member)
    Name: LPCSTR;                       // Name of type, can be nil.
  end;
  {$EXTERNALSYM _D3D11_SHADER_TYPE_DESC}
  D3D11_SHADER_TYPE_DESC = _D3D11_SHADER_TYPE_DESC;
  {$EXTERNALSYM D3D11_SHADER_TYPE_DESC}


  PD3D11_TESSELLATOR_DOMAIN = ^D3D11_TESSELLATOR_DOMAIN;
  D3D11_TESSELLATOR_DOMAIN = D3D_TESSELLATOR_DOMAIN;
  {$EXTERNALSYM D3D11_TESSELLATOR_DOMAIN}

  PD3D11_TESSELLATOR_PARTITIONING = ^D3D11_TESSELLATOR_PARTITIONING;
  D3D11_TESSELLATOR_PARTITIONING = D3D_TESSELLATOR_PARTITIONING;
  {$EXTERNALSYM D3D11_TESSELLATOR_PARTITIONING}

  PD3D11_TESSELLATOR_OUTPUT_PRIMITIVE = ^D3D11_TESSELLATOR_OUTPUT_PRIMITIVE;
  D3D11_TESSELLATOR_OUTPUT_PRIMITIVE = D3D_TESSELLATOR_OUTPUT_PRIMITIVE;
  {$EXTERNALSYM D3D11_TESSELLATOR_OUTPUT_PRIMITIVE}

  PD3D11_SHADER_DESC = ^D3D11_SHADER_DESC;
  _D3D11_SHADER_DESC = record
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
  {$EXTERNALSYM _D3D11_SHADER_DESC}
  D3D11_SHADER_DESC = _D3D11_SHADER_DESC;
  {$EXTERNALSYM D3D11_SHADER_DESC}


  PD3D11_SHADER_INPUT_BIND_DESC = ^D3D11_SHADER_INPUT_BIND_DESC;
  _D3D11_SHADER_INPUT_BIND_DESC = record
    Name: LPCSTR;                           // Name of the resource
    Type_: D3D_SHADER_INPUT_TYPE;           // Type of resource (e.g. texture, cbuffer, etc.)
    BindPoint: UINT;                        // Starting bind point
    BindCount: UINT;                        // Number of contiguous bind points (for arrays)
    uFlags: UINT;                           // Input binding flags
    ReturnType: D3D_RESOURCE_RETURN_TYPE;   // Return type (if texture)
    Dimension: D3D_SRV_DIMENSION;           // Dimension (if texture)
    NumSamples: UINT;                       // Number of samples (0 if not MS texture)
  end;
  {$EXTERNALSYM _D3D11_SHADER_INPUT_BIND_DESC}
  D3D11_SHADER_INPUT_BIND_DESC = _D3D11_SHADER_INPUT_BIND_DESC;
  {$EXTERNALSYM D3D11_SHADER_INPUT_BIND_DESC}

const
  D3D_SHADER_REQUIRES_DOUBLES                                    = $00000001;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_DOUBLES}
  D3D_SHADER_REQUIRES_EARLY_DEPTH_STENCIL                        = $00000002;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_EARLY_DEPTH_STENCIL}
  D3D_SHADER_REQUIRES_UAVS_AT_EVERY_STAGE                        = $00000004;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_UAVS_AT_EVERY_STAGE}
  D3D_SHADER_REQUIRES_64_UAVS                                    = $00000008;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_64_UAVS}
  D3D_SHADER_REQUIRES_MINIMUM_PRECISION                          = $00000010;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_MINIMUM_PRECISION}
  D3D_SHADER_REQUIRES_11_1_DOUBLE_EXTENSIONS                     = $00000020;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_11_1_DOUBLE_EXTENSIONS}
  D3D_SHADER_REQUIRES_11_1_SHADER_EXTENSIONS                     = $00000040;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_11_1_SHADER_EXTENSIONS}
  D3D_SHADER_REQUIRES_LEVEL_9_COMPARISON_FILTERING               = $00000080;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_LEVEL_9_COMPARISON_FILTERING}
  D3D_SHADER_REQUIRES_TILED_RESOURCES                            = $00000100;
  {$EXTERNALSYM D3D_SHADER_REQUIRES_TILED_RESOURCES}

type
  PD3D11_LIBRARY_DESC = ^D3D11_LIBRARY_DESC;
  _D3D11_LIBRARY_DESC = record
    Creator: LPCSTR;                 // The name of the originator of the library.
    Flags: UINT;                     // Compilation flags.
    FunctionCount: UINT;             // Number of functions exported from the library.
  end;
  {$EXTERNALSYM _D3D11_LIBRARY_DESC}
  D3D11_LIBRARY_DESC = _D3D11_LIBRARY_DESC;
  {$EXTERNALSYM D3D11_LIBRARY_DESC}


  PD3D11_FUNCTION_DESC = ^D3D11_FUNCTION_DESC;
  _D3D11_FUNCTION_DESC = record
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
    HasReturn: BOOL;                      // TRUE, if function returns a value, false - it is a subroutine
    Has10Level9VertexShader: BOOL;        // TRUE, if there is a 10L9 VS blob
    Has10Level9PixelShader: BOOL;         // TRUE, if there is a 10L9 PS blob
  end;
  {$EXTERNALSYM _D3D11_FUNCTION_DESC}
  D3D11_FUNCTION_DESC = _D3D11_FUNCTION_DESC;
  {$EXTERNALSYM D3D11_FUNCTION_DESC}


  PD3D11_PARAMETER_DESC = ^D3D11_PARAMETER_DESC;
  _D3D11_PARAMETER_DESC = record
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
  {$EXTERNALSYM _D3D11_PARAMETER_DESC}
  D3D11_PARAMETER_DESC = _D3D11_PARAMETER_DESC;
  {$EXTERNALSYM D3D11_PARAMETER_DESC}


  //////////////////////////////////////////////////////////////////////////////
  // Interfaces ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
type

  LPD3D11SHADERREFLECTIONTYPE = ^ID3D11ShaderReflectionType;
  {$EXTERNALSYM LPD3D11SHADERREFLECTIONTYPE}
  PID3D11ShaderReflectionType = ^ID3D11ShaderReflectionType;
  ID3D11ShaderReflectionType = interface;
  {$EXTERNALSYM ID3D11ShaderReflectionType}

  LPD3D11SHADERREFLECTIONVARIABLE = ^ID3D11ShaderReflectionVariable;
  {$EXTERNALSYM LPD3D11SHADERREFLECTIONVARIABLE}
  PID3D11ShaderReflectionVariable = ^ID3D11ShaderReflectionVariable;
  ID3D11ShaderReflectionVariable = interface;
  {$EXTERNALSYM ID3D11ShaderReflectionVariable}

  LPD3D11SHADERREFLECTIONCONSTANTBUFFER = ^ID3D11ShaderReflectionConstantBuffer;
  {$EXTERNALSYM LPD3D11SHADERREFLECTIONCONSTANTBUFFER}
  PID3D11ShaderReflectionConstantBuffer = ^ID3D11ShaderReflectionConstantBuffer;
  ID3D11ShaderReflectionConstantBuffer = interface;
  {$EXTERNALSYM ID3D11ShaderReflectionConstantBuffer}

  LPD3D11SHADERREFLECTION = ^ID3D11ShaderReflection;
  {$EXTERNALSYM LPD3D11SHADERREFLECTION}
  PID3D11ShaderReflection = ^ID3D11ShaderReflection;
  ID3D11ShaderReflection = interface;
  {$EXTERNALSYM ID3D11ShaderReflection}

  LPD3D11LIBRARYREFLECTION = ^ID3D11LibraryReflection;
  {$EXTERNALSYM LPD3D11LIBRARYREFLECTION}
  PID3D11LibraryReflection = ^ID3D11LibraryReflection;
  ID3D11LibraryReflection = interface;
  {$EXTERNALSYM ID3D11LibraryReflection}

  LPD3D11FUNCTIONREFLECTION = ^ID3D11FunctionReflection;
  {$EXTERNALSYM LPD3D11FUNCTIONREFLECTION}
  PID3D11FunctionReflection = ^ID3D11FunctionReflection;
  ID3D11FunctionReflection = interface;
  {$EXTERNALSYM ID3D11FunctionReflection}

  LPD3D11FUNCTIONPARAMETERREFLECTION = ^ID3D11FunctionParameterReflection;
  {$EXTERNALSYM LPD3D11FUNCTIONPARAMETERREFLECTION}
  PID3D11FunctionParameterReflection = ^ID3D11FunctionParameterReflection;
  ID3D11FunctionParameterReflection = interface;
  {$EXTERNALSYM ID3D11FunctionParameterReflection}



  // Interface ID3D11ShaderReflectionType
  // ====================================
  // NOTE: This isn't a COM interface, so you don't need to worry about reference counts or
  //       releasing the interface when you're done with it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11ShaderReflectionType);'}
  {$EXTERNALSYM ID3D11ShaderReflectionType}
  ID3D11ShaderReflectionType = interface
    ['{6E6FFA6A-9BAE-4613-A51E-91652D508C21}']

    function GetDesc(out pDesc: D3D11_SHADER_TYPE_DESC): HRESULT; stdcall;

    function GetMemberTypeByIndex(Index: UINT): ID3D11ShaderReflectionType; stdcall;

    function GetMemberTypeByName(Name: LPCSTR): ID3D11ShaderReflectionType; stdcall;

    function GetMemberTypeName(Index: UINT): LPCSTR; stdcall;

    function IsEqual(pType: ID3D11ShaderReflectionType): HRESULT; stdcall;

    function GetSubType(): ID3D11ShaderReflectionType; stdcall;

    function GetBaseClass(): ID3D11ShaderReflectionType; stdcall;

    function GetNumInterfaces(): UINT; stdcall;

    function GetInterfaceByIndex(uIndex: UINT): ID3D11ShaderReflectionType; stdcall;

    function IsOfType(pType: ID3D11ShaderReflectionType): HRESULT; stdcall;

    function ImplementsInterface(pBase: ID3D11ShaderReflectionType): HRESULT; stdcall;

  end;
  IID_ID3D11ShaderReflectionType = ID3D11ShaderReflectionType;
  {$EXTERNALSYM IID_ID3D11ShaderReflectionType}


  // Interface ID3D11ShaderReflectionVariable
  // ========================================
  // NOTE: This isn't a COM interface, so you don't need to worry about reference counts or
  //       releasing the interface when you're done with it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11ShaderReflectionVariable);'}
  {$EXTERNALSYM ID3D11ShaderReflectionVariable}
  ID3D11ShaderReflectionVariable = interface
    ['{51F23923-F3E5-4BD1-91CB-606177D8DB4C}']

    function GetDesc(out pDesc: D3D11_SHADER_VARIABLE_DESC): HRESULT; stdcall;

    function GetType(): ID3D11ShaderReflectionType; stdcall;

    function GetBuffer(): ID3D11ShaderReflectionConstantBuffer; stdcall;

    function GetInterfaceSlot(uArrayIndex: UINT): UINT; stdcall;

  end;
  IID_ID3D11ShaderReflectionVariable = ID3D11ShaderReflectionVariable;
  {$EXTERNALSYM IID_ID3D11ShaderReflectionVariable}


  // Interface ID3D11ShaderReflectionConstantBuffer
  // ==============================================
  // NOTE: This isn't a COM interface, so you don't need to worry about reference counts or
  //       releasing the interface when you're done with it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11ShaderReflectionConstantBuffer);'}
  {$EXTERNALSYM ID3D11ShaderReflectionConstantBuffer}
  ID3D11ShaderReflectionConstantBuffer = interface
    ['{EB62D63D-93DD-4318-8AE8-C6F83AD371B8}']

    function GetDesc(pDesc: D3D11_SHADER_BUFFER_DESC): HRESULT; stdcall;

    function GetVariableByIndex(Index: UINT): ID3D11ShaderReflectionVariable; stdcall;

    function GetVariableByName(Name: LPCSTR): ID3D11ShaderReflectionVariable; stdcall;

  end;
  IID_ID3D11ShaderReflectionConstantBuffer = ID3D11ShaderReflectionConstantBuffer;
  {$EXTERNALSYM IID_ID3D11ShaderReflectionConstantBuffer}


  // Interface ID3D11ShaderReflection
  // ================================
  // The ID3D11ShaderReflection IID may change from SDK version to SDK version
  // if the reflection API changes. This prevents new code with the new API
  // from working with an old binary. Recompiling with the new header
  // will pick up the new IID.
  //
  // NOTE: This is a COM interface!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11ShaderReflection);'}
  {$EXTERNALSYM ID3D11ShaderReflection}
  ID3D11ShaderReflection = interface(IUnknown)
    ['{8d536ca1-0cca-4956-a837-786963755584}']

    function GetDesc(out pDesc: D3D11_SHADER_DESC): HRESULT; stdcall;

    function GetConstantBufferByIndex(Index: UINT): ID3D11ShaderReflectionConstantBuffer; stdcall;

    function GetConstantBufferByName(Name: LPCSTR): ID3D11ShaderReflectionConstantBuffer; stdcall;

    function GetResourceBindingDesc(ResourceIndex: UINT;
                                    out pDesc: D3D11_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    function GetInputParameterDesc(ParameterIndex: UINT;
                                   out pDesc: D3D11_SIGNATURE_PARAMETER_DESC): HRESULT; stdcall;

    function GetOutputParameterDesc(ParameterIndex: UINT;
                                    out pDesc: D3D11_SIGNATURE_PARAMETER_DESC): HRESULT; stdcall;

    function GetPatchConstantParameterDesc(ParameterIndex: UINT;
                                           out pDesc: D3D11_SIGNATURE_PARAMETER_DESC): HRESULT; stdcall;

    function GetVariableByName(Name: LPCSTR): ID3D11ShaderReflectionVariable; stdcall;

    function GetResourceBindingDescByName(Name: LPCSTR;
                                          out pDesc: D3D11_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    function GetMovInstructionCount(): UINT; stdcall;

    function GetMovcInstructionCount(): UINT; stdcall;

    function GetConversionInstructionCount(): UINT; stdcall;

    function GetBitwiseInstructionCount(): UINT; stdcall;

    function GetGSInputPrimitive(): D3D_PRIMITIVE; stdcall;

    function IsSampleFrequencyShader(): BOOL; stdcall;

    function GetNumInterfaceSlots(): UINT; stdcall;

    function GetMinFeatureLevel(out pLevel: D3D_FEATURE_LEVEL): HRESULT; stdcall;

    function GetThreadGroupSize(out pSizeX: UINT;
                                out pSizeY: UINT;
                                out pSizeZ: UINT): UINT; stdcall;

    function GetRequiresFlags(): UINT64; stdcall;


  end;
  IID_ID3D11ShaderReflection = ID3D11ShaderReflection;
  {$EXTERNALSYM IID_ID3D11ShaderReflection}


  // Interface ID3D11LibraryReflection
  // =================================
  // NOTE: This isn't a COM interface, so you don't need to worry about reference counts or
  //       releasing the interface when you're done with it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11LibraryReflection);'}
  {$EXTERNALSYM ID3D11LibraryReflection}
  ID3D11LibraryReflection = interface(IUnknown)
    ['{54384F1B-5B3E-4BB7-AE01-60BA3097CBB6}']

    function GetDesc(out pDesc: D3D11_LIBRARY_DESC): HRESULT; stdcall;

   // function GetFunctionByIndex(FunctionIndex: INT): ID3D11FunctionReflection; stdcall;

  end;
  IID_ID3D11LibraryReflection = ID3D11LibraryReflection;
  {$EXTERNALSYM IID_ID3D11LibraryReflection}


  // Interface ID3D11FunctionReflection
  // ==================================
  // NOTE: This isn't a COM interface.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11FunctionReflection);'}
  {$EXTERNALSYM ID3D11FunctionReflection}
  ID3D11FunctionReflection = interface
    ['{207BCECB-D683-4A06-A8A3-9B149B9F73A4}']

    function GetDesc(out pDesc: D3D11_FUNCTION_DESC): HRESULT; stdcall;

    function GetConstantBufferByIndex(BufferIndex: UINT): ID3D11ShaderReflectionConstantBuffer; stdcall;

    function GetConstantBufferByName(Name: LPCSTR): ID3D11ShaderReflectionConstantBuffer; stdcall;

    function GetResourceBindingDesc(ResourceIndex: UINT;
                                    out pDesc: PD3D11_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    function GetVariableByName(Name: LPCSTR): ID3D11ShaderReflectionVariable;

    function GetResourceBindingDescByName(Name: LPCSTR;
                                          out pDesc: D3D11_SHADER_INPUT_BIND_DESC): HRESULT; stdcall;

    // Use D3D_RETURN_PARAMETER_INDEX to get description of the return value.
    function GetFunctionParameter(ParameterIndex: INT): ID3D11FunctionParameterReflection; stdcall;

  end;
  IID_ID3D11FunctionReflection = ID3D11FunctionReflection;
  {$EXTERNALSYM IID_ID3D11FunctionReflection}


  // Interface ID3D11FunctionParameterReflection
  // ===========================================
  // NOTE: This isn't a COM interface, so you don't need to worry about reference counts or
  //       releasing the interface when you're done with it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11FunctionParameterReflection);'}
  {$EXTERNALSYM ID3D11FunctionParameterReflection}
  ID3D11FunctionParameterReflection = interface
    ['{42757488-334F-47FE-982E-1A65D08CC462}']

    function GetDesc(out pDesc: D3D11_PARAMETER_DESC): HRESULT; stdcall;

  end;
  IID_ID3D11FunctionParameterReflection = ID3D11FunctionParameterReflection;
  {$EXTERNALSYM IID_ID3D11FunctionParameterReflection}


  // Interface ID3D11ModuleInstance
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11ModuleInstance);'}
  {$EXTERNALSYM ID3D11ModuleInstance}
  ID3D11ModuleInstance = interface(IUnknown)
    ['{469E07F7-045A-48D5-AA12-68A478CDF75D}']

    //
    // Resource binding API.
    //
    function BindConstantBuffer(uSrcSlot: UINT;
                                uDstSlot: UINT;
                                cbDstOffset: UINT): HRESULT; stdcall;

    function BindConstantBufferByName(pName: LPCSTR;
                                      uDstSlot: UINT;
                                      cbDstOffset: UINT): HRESULT; stdcall;

    function BindResource(uSrcSlot: UINT;
                          uDstSlot: UINT;
                          uCount: UINT): HRESULT; stdcall;

    function BindResourceByName(pName: LPCSTR;
                                uDstSlot: UINT;
                                uCount: UINT): HRESULT; stdcall;

    function BindSampler(uSrcSlot: UINT;
                         uDstSlot: UINT;
                         uCount: UINT): HRESULT; stdcall;

    function BindSamplerByName(pName: LPCSTR;
                               uDstSlot: UINT;
                               uCount: UINT): HRESULT; stdcall;

    function BindUnorderedAccessView(uSrcSlot: UINT;
                                     uDstSlot: UINT;
                                     uCount: UINT): HRESULT; stdcall;

    function BindUnorderedAccessViewByName(pName: LPCSTR;
                                           uDstSlot: UINT;
                                           uCount: UINT): HRESULT; stdcall;

    function BindResourceAsUnorderedAccessView(uSrcSrvSlot: UINT;
                                               uDstUavSlot: UINT;
                                               uCount: UINT): HRESULT; stdcall;

    function BindResourceAsUnorderedAccessViewByName(pSrvName: LPCSTR;
                                                     uDstUavSlot: UINT;
                                                     uCount: UINT): HRESULT; stdcall;

  end;
  IID_ID3D11ModuleInstance = ID3D11ModuleInstance;
  {$EXTERNALSYM IID_ID3D11ModuleInstance}


  // Interface ID3D11Module
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Module);'}
  {$EXTERNALSYM ID3D11Module}
  ID3D11Module = interface(IUnknown)
    ['{CAC701EE-80FC-4122-8242-10B39C8CEC34}']

    // Create an instance of a module for resource re-binding.
    function CreateInstance(pNamespace: LPCSTR;
                            [ref] const ppModuleInstance: ID3D11ModuleInstance): HRESULT; stdcall;

  end;
  IID_ID3D11Module = ID3D11Module;
  {$EXTERNALSYM IID_ID3D11Module}


  // Interface ID3D11Linker
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Linker);'}
  {$EXTERNALSYM ID3D11Linker}
  ID3D11Linker = interface(IUnknown)
    ['{59A6CD0E-E10D-4C1F-88C0-63ABA1DAF30E}']

    // Link the shader and produce a shader blob suitable to D3D runtime.
    function Link(pEntry: ID3D11ModuleInstance;
                  pEntryName: LPCSTR;
                  pTargetName: LPCSTR;
                  uFlags: UINT;
                  [ref] const ppShaderBlob: ID3DBlob;
                  out ppErrorBuffer: ID3DBlob): HRESULT; stdcall;

    // Add an instance of a library module to be used for linking.
    function UseLibrary(pLibraryMI: ID3D11ModuleInstance): HRESULT; stdcall;

    // Add a clip plane with the plane coefficients taken from a cbuffer entry for 10L9 shaders.
    function AddClipPlaneFromCBuffer(uCBufferSlot: UINT;
                                     uCBufferEntry: UINT): HRESULT; stdcall;

  end;
  IID_ID3D11Linker = ID3D11Linker;
  {$EXTERNALSYM IID_ID3D11Linker}


  // Interface ID3D11LinkingNode
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11LinkingNode);'}
  {$EXTERNALSYM ID3D11LinkingNode}
  ID3D11LinkingNode = interface(IUnknown)
    ['{D80DD70C-8D2F-4751-94A1-03C79B3556DB}']


  end;
  IID_ID3D11LinkingNode = ID3D11LinkingNode;
  {$EXTERNALSYM IID_ID3D11LinkingNode}


  // Interface ID3D11FunctionLinkingGraph
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11FunctionLinkingGraph);'}
  {$EXTERNALSYM ID3D11FunctionLinkingGraph}
  ID3D11FunctionLinkingGraph = interface(IUnknown)
    ['{54133220-1CE8-43D3-8236-9855C5CEECFF}']

    function CreateModuleInstance([ref] const ppModuleInstance: ID3D11ModuleInstance;
                                  out ppErrorBuffer: PID3DBlob): HRESULT; stdcall;

    function SetInputSignature(pInputParameters: D3D11_PARAMETER_DESC;
                               cInputParameters: UINT;
                               [ref] const ppInputNode: ID3D11LinkingNode): HRESULT; stdcall;

    function SetOutputSignature(pOutputParameters: D3D11_PARAMETER_DESC;
                                cOutputParameters: UINT;
                                [ref] const ppOutputNode: ID3D11LinkingNode): HRESULT; stdcall;

    function CallFunction(pModuleInstanceNamespace: LPCSTR;
                          pModuleWithFunctionPrototype: ID3D11Module;
                          pFunctionName: LPCSTR;
                          [ref] const ppCallNode: ID3D11LinkingNode): HRESULT; stdcall;

    function PassValue(pSrcNode: ID3D11LinkingNode;
                       SrcParameterIndex: INT;
                       pDstNode: ID3D11LinkingNode;
                       DstParameterIndex: INT): HRESULT; stdcall;

    function PassValueWithSwizzle(pSrcNode: ID3D11LinkingNode;
                                  SrcParameterIndex: INT;
                                  pSrcSwizzle: LPCSTR;
                                  pDstNode: ID3D11LinkingNode;
                                  DstParameterIndex: INT;
                                  pDstSwizzle: LPCSTR): HRESULT; stdcall;

    function GetLastError(out ppErrorBuffer: PID3DBlob): HRESULT; stdcall;

    function GenerateHlsl(uFlags: UINT; // uFlags is reserved for future use.
                         [ref] const ppBuffer: ID3DBlob): HRESULT; stdcall;


  end;
  IID_ID3D11FunctionLinkingGraph = ID3D11FunctionLinkingGraph;
  {$EXTERNALSYM IID_ID3D11FunctionLinkingGraph}



//////////////////////////////////////////////////////////////////////////////
// APIs //////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////



implementation

{VERSION MACRO'S}

function D3D11_SHVER_GET_TYPE(_Version: DWORD): DWORD; inline;
begin
  Result := (_Version shr 16) and $ffff;
end;

function D3D11_SHVER_GET_MAJOR(_Version: DWORD): DWORD; inline;
begin
  Result := (_Version shr 4) and $f;
end;

function D3D11_SHVER_GET_MINOR(_Version: DWORD): DWORD; inline;
begin
  Result := (_Version shr 0) and $f;
end;


end.

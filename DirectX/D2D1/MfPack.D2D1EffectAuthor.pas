// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - D2D1
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.D2D1EffectAuthor.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Direct2D: Hardware-accelerated, immediate-mode, 2-D graphics API that
//              provides high performance and high-quality rendering for 2-D geometry,
//              bitmaps, and text.
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
// Source: d2d1effectauthor.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit MfPack.D2D1EffectAuthor;

  {$HPPEMIT '#include "d2d1effectauthor.h"'}

interface

uses
  {WinApi}
  WinApi.WinCodec, {or MfPack.WinCodec,)
  {MfPack}
  MfPack.MfpTypes,
  MfPack.DCommon,
  MfPack.D3DCommon,
  MfPack.D2D1,
  MfPack.D2D1_1,
  MfPack.DXGIFormat;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

const

  //
  // Set to alignedByteOffset within D2D1_INPUT_ELEMENT_DESC for elements that
  // immediately follow preceding elements in memory
  //
  D2D1_APPEND_ALIGNED_ELEMENT         = -1;
  {$EXTERNALSYM D2D1_APPEND_ALIGNED_ELEMENT}


type
  // Function pointer that sets a property on an effect.
  // NOTE: Moved to D2D1_1.pas to prevent circular references
  // PD2D1_PROPERTY_SET_FUNCTION = function(effect: IUnknown;
  //                                        data: PByte;
  //                                        dataSize: UINT32): HRESULT; stdcall;


  // Function pointer that gets a property from an effect.
  // NOTE: Moved to D2D1_1.pas to prevent circular references
  // PD2D1_PROPERTY_GET_FUNCTION = function(effect: IUnknown;
  //                                        out data: PByte;
  //                                        dataSize: UINT32;
  //                                        actualSize: UINT32): HRESULT; stdcall;


  //
  // Forward interface declarations
  //

  PID2D1EffectContext = ^ID2D1EffectContext;
  ID2D1EffectContext = interface;

  // Indicates what has changed since the last time the effect was asked to prepare
  // to render.
  PD2D1_CHANGE_TYPE = ^D2D1_CHANGE_TYPE;
  D2D1_CHANGE_TYPE               = (
    // Nothing has changed.
    D2D1_CHANGE_TYPE_NONE        = 0,
    // The effect's properties have changed.
    D2D1_CHANGE_TYPE_PROPERTIES  = 1,
    // The internal context has changed and should be inspected.
    D2D1_CHANGE_TYPE_CONTEXT     = 2,
    // A new graph has been set due to a change in the input count.
    D2D1_CHANGE_TYPE_GRAPH       = 3,
    D2D1_CHANGE_TYPE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM D2D1_CHANGE_TYPE}


  // Indicates options for drawing using a pixel shader.
  PD2D1_PIXEL_OPTIONS = ^D2D1_PIXEL_OPTIONS;
  D2D1_PIXEL_OPTIONS                    = (
    // Default pixel processing.
    D2D1_PIXEL_OPTIONS_NONE             = 0,
    // Indicates that the shader samples its inputs only at exactly the same scene
    // coordinate as the output pixel, and that it returns transparent black whenever
    // the input pixels are also transparent black.
    D2D1_PIXEL_OPTIONS_TRIVIAL_SAMPLING = 1,
    D2D1_PIXEL_OPTIONS_FORCE_DWORD      = FORCEDWORD);
  {$EXTERNALSYM D2D1_PIXEL_OPTIONS}


  // Indicates options for drawing custom vertices set by transforms.
  PD2D1_VERTEX_OPTIONS = ^D2D1_VERTEX_OPTIONS;
  D2D1_VERTEX_OPTIONS                     = (
    // Default vertex processing.
    D2D1_VERTEX_OPTIONS_NONE              = 0,
    // Indicates that the output rectangle does not need to be cleared before drawing
    // custom vertices. This must only be used by transforms whose custom vertices
    // completely cover their output rectangle.
    D2D1_VERTEX_OPTIONS_DO_NOT_CLEAR      = 1,
    // Causes a depth buffer to be used while drawing custom vertices. This impacts
    // drawing behavior when primitives overlap one another.
    D2D1_VERTEX_OPTIONS_USE_DEPTH_BUFFER  = 2,
    // Indicates that custom vertices do not form primitives which overlap one another.
    D2D1_VERTEX_OPTIONS_ASSUME_NO_OVERLAP = 4,
    D2D1_VERTEX_OPTIONS_FORCE_DWORD       = FORCEDWORD);
  {$EXTERNALSYM D2D1_VERTEX_OPTIONS}


  // Describes how a vertex buffer is to be managed.
  PD2D1_VERTEX_USAGE = ^D2D1_VERTEX_USAGE;
  D2D1_VERTEX_USAGE               = (
    // The vertex buffer content do not change frequently from frame to frame.
    D2D1_VERTEX_USAGE_STATIC      = 0,
    // The vertex buffer is intended to be updated frequently.
    D2D1_VERTEX_USAGE_DYNAMIC     = 1,
    D2D1_VERTEX_USAGE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM D2D1_VERTEX_USAGE}


  // Describes a particular blend in the D2D1_BLEND_DESCRIPTION structure.
  PD2D1_BLEND_OPERATION = ^D2D1_BLEND_OPERATION;
  D2D1_BLEND_OPERATION                = (
    D2D1_BLEND_OPERATION_ADD          = 1,
    D2D1_BLEND_OPERATION_SUBTRACT     = 2,
    D2D1_BLEND_OPERATION_REV_SUBTRACT = 3,
    D2D1_BLEND_OPERATION_MIN          = 4,
    D2D1_BLEND_OPERATION_MAX          = 5,
    D2D1_BLEND_OPERATION_FORCE_DWORD  = FORCEDWORD);
  {$EXTERNALSYM D2D1_BLEND_OPERATION}


  // Describes a particular blend in the D2D1_BLEND_DESCRIPTION structure.
  PD2D1_BLEND = ^D2D1_BLEND;
  D2D1_BLEND                    = (
    D2D1_BLEND_ZERO             = 1,
    D2D1_BLEND_ONE              = 2,
    D2D1_BLEND_SRC_COLOR        = 3,
    D2D1_BLEND_INV_SRC_COLOR    = 4,
    D2D1_BLEND_SRC_ALPHA        = 5,
    D2D1_BLEND_INV_SRC_ALPHA    = 6,
    D2D1_BLEND_DEST_ALPHA       = 7,
    D2D1_BLEND_INV_DEST_ALPHA   = 8,
    D2D1_BLEND_DEST_COLOR       = 9,
    D2D1_BLEND_INV_DEST_COLOR   = 10,
    D2D1_BLEND_SRC_ALPHA_SAT    = 11,
    D2D1_BLEND_BLEND_FACTOR     = 14,
    D2D1_BLEND_INV_BLEND_FACTOR = 15,
    D2D1_BLEND_FORCE_DWORD      = FORCEDWORD);
  {$EXTERNALSYM D2D1_BLEND}


  // Allows a caller to control the channel depth of a stage in the rendering
  // pipeline.
  PD2D1_CHANNEL_DEPTH = ^D2D1_CHANNEL_DEPTH;
  D2D1_CHANNEL_DEPTH               = (
    D2D1_CHANNEL_DEPTH_DEFAULT     = 0,
    D2D1_CHANNEL_DEPTH_1           = 1,
    D2D1_CHANNEL_DEPTH_4           = 4,
    D2D1_CHANNEL_DEPTH_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM D2D1_CHANNEL_DEPTH}


  // Represents filtering modes transforms may select to use on their input textures.
  PD2D1_FILTER = ^D2D1_FILTER;
  D2D1_FILTER                                   = (
    D2D1_FILTER_MIN_MAG_MIP_POINT               = $00,
    D2D1_FILTER_MIN_MAG_POINT_MIP_LINEAR        = $01,
    D2D1_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT  = $04,
    D2D1_FILTER_MIN_POINT_MAG_MIP_LINEAR        = $05,
    D2D1_FILTER_MIN_LINEAR_MAG_MIP_POINT        = $10,
    D2D1_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR = $11,
    D2D1_FILTER_MIN_MAG_LINEAR_MIP_POINT        = $14,
    D2D1_FILTER_MIN_MAG_MIP_LINEAR              = $15,
    D2D1_FILTER_ANISOTROPIC                     = $55,
    D2D1_FILTER_FORCE_DWORD                     = FORCEDWORD);
  {$EXTERNALSYM D2D1_FILTER}


  // Defines capabilities of the underlying D3D device which may be queried using
  // CheckFeatureSupport.
  PD2D1_FEATURE = ^D2D1_FEATURE;
  D2D1_FEATURE                            = (
    D2D1_FEATURE_DOUBLES                  = 0,
    D2D1_FEATURE_D3D10_X_HARDWARE_OPTIONS = 1,
    D2D1_FEATURE_FORCE_DWORD              = FORCEDWORD);
  {$EXTERNALSYM D2D1_FEATURE}


  // Defines a property binding to a function. The name must match the property
  // defined in the registration schema.
  // Delphi Note: Moved to D2D1_1 (to prevent circular reference)
  //D2D1_PROPERTY_BINDING = record
  //  // The name of the property.
  //  propertyName: PCWSTR;
    // The function that will receive the data to set.
  //  setFunction: PD2D1_PROPERTY_SET_FUNCTION;
    // The function that will be asked to write the output data.
  //  getFunction: PD2D1_PROPERTY_GET_FUNCTION;
  //end;
  //PD2D1_PROPERTY_BINDING = ^D2D1_PROPERTY_BINDING;


  // This is used to define a resource texture when that resource texture is created.
  PD2D1_RESOURCE_TEXTURE_PROPERTIES = ^D2D1_RESOURCE_TEXTURE_PROPERTIES;
  D2D1_RESOURCE_TEXTURE_PROPERTIES = record
    extents: UINT32;
    dimensions: UINT32;
    bufferPrecision: D2D1_BUFFER_PRECISION;
    channelDepth: D2D1_CHANNEL_DEPTH;
    filter: D2D1_FILTER;
    extendModes: D2D1_EXTEND_MODE;
  end;
  {$EXTERNALSYM D2D1_RESOURCE_TEXTURE_PROPERTIES}


  // This defines a single element of the vertex layout.
  PD2D1_INPUT_ELEMENT_DESC = ^D2D1_INPUT_ELEMENT_DESC;
  D2D1_INPUT_ELEMENT_DESC = record
    semanticName: LPCSTR;
    semanticIndex: UINT32;
    format: DXGI_FORMAT;
    inputSlot: UINT32;
    alignedByteOffset: UINT32;
  end;
  {$EXTERNALSYM D2D1_INPUT_ELEMENT_DESC}


  // This defines the properties of a vertex buffer which uses the default vertex
  // layout.
  PD2D1_VERTEX_BUFFER_PROPERTIES = ^D2D1_VERTEX_BUFFER_PROPERTIES;
  D2D1_VERTEX_BUFFER_PROPERTIES = record
    inputCount: UINT32;
    usage: D2D1_VERTEX_USAGE;
    data: PByte;
    byteWidth: UINT32;
  end;
  {$EXTERNALSYM D2D1_VERTEX_BUFFER_PROPERTIES}


  // This defines the input layout of vertices and the vertex shader which processes
  // them.
  PD2D1_CUSTOM_VERTEX_BUFFER_PROPERTIES = ^D2D1_CUSTOM_VERTEX_BUFFER_PROPERTIES;
  D2D1_CUSTOM_VERTEX_BUFFER_PROPERTIES = record
    shaderBufferWithInputSignature: PByte;
    shaderBufferSize: UINT32;
    inputElements: PD2D1_INPUT_ELEMENT_DESC;
    elementCount: UINT32;
    stride: UINT32;
  end;
  {$EXTERNALSYM D2D1_CUSTOM_VERTEX_BUFFER_PROPERTIES}


  // This defines the range of vertices from a vertex buffer to draw.
  PD2D1_VERTEX_RANGE = ^D2D1_VERTEX_RANGE;
  D2D1_VERTEX_RANGE = record
    startVertex: UINT32;
    vertexCount: UINT32;
  end;
  {$EXTERNALSYM D2D1_VERTEX_RANGE}


  // Blend description which configures a blend transform object.
  PD2D1_BLEND_DESCRIPTION = ^D2D1_BLEND_DESCRIPTION;
  D2D1_BLEND_DESCRIPTION = record
    sourceBlend: D2D1_BLEND;
  {$EXTERNALSYM D2D1_BLEND_DESCRIPTION}
    destinationBlend: D2D1_BLEND;
    blendOperation: D2D1_BLEND_OPERATION;
    sourceBlendAlpha: D2D1_BLEND;
    destinationBlendAlpha: D2D1_BLEND;
    blendOperationAlpha: D2D1_BLEND_OPERATION;
    blendFactor: array[0..3] of FLOAT;
  end;


  // Describes options transforms may select to use on their input textures.
  PD2D1_INPUT_DESCRIPTION = ^D2D1_INPUT_DESCRIPTION;
  D2D1_INPUT_DESCRIPTION = record
    filter: D2D1_FILTER;
    levelOfDetailCount: UINT32;
  end;
  {$EXTERNALSYM D2D1_INPUT_DESCRIPTION}


  // Indicates whether shader support for doubles is present on the underlying
  // hardware. This may be populated using CheckFeatureSupport.
  PD2D1_FEATURE_DATA_DOUBLES = ^D2D1_FEATURE_DATA_DOUBLES;
  D2D1_FEATURE_DATA_DOUBLES = record
    doublePrecisionFloatShaderOps: BOOL;
  end;
  {$EXTERNALSYM D2D1_FEATURE_DATA_DOUBLES}


  // Indicates support for features which are optional on D3D10 feature levels. This
  // may be populated using CheckFeatureSupport.
  PD2D1_FEATURE_DATA_D3D10_X_HARDWARE_OPTIONS = ^D2D1_FEATURE_DATA_D3D10_X_HARDWARE_OPTIONS;
  D2D1_FEATURE_DATA_D3D10_X_HARDWARE_OPTIONS = record
    computeShaders_Plus_RawAndStructuredBuffers_Via_Shader_4_x: BOOL;
  end;
  {$EXTERNALSYM D2D1_FEATURE_DATA_D3D10_X_HARDWARE_OPTIONS}


  // Interfaces


  // Interface ID2D1VertexBuffer
  // ===========================
  // A transform uses this interface to write new vertices to a vertex buffer.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1VertexBuffer);'}
  {$EXTERNALSYM ID2D1VertexBuffer}
 ID2D1VertexBuffer = interface(IUnknown)
  ['{9b8b1336-00a5-4668-92b7-ced5d8bf9b7b}']

    function Map({out} data: PByte;
                 bufferSize: UINT32): HResult; stdcall;

    function Unmap(): HResult; stdcall;

  end;
  IID_ID2D1VertexBuffer = ID2D1VertexBuffer;
  {$EXTERNALSYM IID_ID2D1VertexBuffer}


  // Interface ID2D1ResourceTexture
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ResourceTexture);'}
  {$EXTERNALSYM ID2D1ResourceTexture}
  ID2D1ResourceTexture = interface(IUnknown)
  ['{688d15c3-02b0-438d-b13a-d1b44c32c39a}']

    // Update the vertex text.
    function Update(minimumExtents: UINT32;
                    maximimumExtents: UINT32;
                    strides: UINT32;
                    dimensions: UINT32;
                    data: PByte;
                    dataCount: UINT32): HResult; stdcall;

  end;
  IID_ID2D1ResourceTexture = ID2D1ResourceTexture;
  {$EXTERNALSYM IID_ID2D1ResourceTexture}


  // Interface ID2D1RenderInfo
  // =========================
  // A transform uses this interface to specify how to render a particular pass in
  // D2D.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1RenderInfo);'}
  {$EXTERNALSYM ID2D1RenderInfo}
  ID2D1RenderInfo = interface(IUnknown)
  ['{519ae1bd-d19a-420d-b849-364f594776b7}']

    // Sets options for sampling the specified image input
    function SetInputDescription(inputIndex: UINT32;
                                 inputDescription: D2D1_INPUT_DESCRIPTION): HResult; stdcall;

    // Controls the output precision and channel-depth for the associated transform.
    function SetOutputBuffer(bufferPrecision: D2D1_BUFFER_PRECISION;
                             channelDepth: D2D1_CHANNEL_DEPTH): HResult; stdcall;

    // Controls whether the output of the associated transform is cached.
    procedure SetCached(isCached: BOOL); stdcall;

    // Provides a hint of the approximate shader instruction count per pixel.  If
    // provided, it may improve performance when processing large images.  Instructions
    // should be counted multiple times if occurring within loops.
    procedure SetInstructionCountHint(instructionCount: UINT32); stdcall;

  end;
  IID_ID2D1RenderInfo = ID2D1RenderInfo;
  {$EXTERNALSYM IID_ID2D1RenderInfo}


  // Interface ID2D1DrawInfo
  // =======================
  // A transform uses this interface to specify how to render a particular pass using
  // pixel and vertex shaders.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DrawInfo);'}
  {$EXTERNALSYM ID2D1DrawInfo}
  ID2D1DrawInfo = interface(ID2D1RenderInfo)
  ['{693ce632-7f2f-45de-93fe-18d88b37aa21}']

    // Set the constant buffer for this transform's pixel shader.
    function SetPixelShaderConstantBuffer(buffer: PByte;
                                          bufferCount: UINT32): HResult; stdcall;

    // Sets the resource texture corresponding to the given shader texture index.
    function SetResourceTexture(textureIndex: UINT32;
                                resourceTexture: ID2D1ResourceTexture): HResult; stdcall;


    // Set the constant buffer for this transform's vertex shader.
    function SetVertexShaderConstantBuffer(buffer: PByte;
                                           bufferCount: UINT32): HResult; stdcall;


    // Set the shader instructions for this transform.
    function SetPixelShader(shaderId: REFGUID;
                            pixelOptions: D2D1_PIXEL_OPTIONS = D2D1_PIXEL_OPTIONS_NONE): HResult; stdcall;


    // Set custom vertices for the associated transform.  A blend mode if
    // foreground-over will be used if blendDescription is Nil.
    function SetVertexProcessing(vertexBuffer: ID2D1VertexBuffer;
                                 vertexOptions: D2D1_VERTEX_OPTIONS;
                                 blendDescription: PD2D1_BLEND_DESCRIPTION = Nil;
                                 vertexRange: PD2D1_VERTEX_RANGE = Nil;
                                 vertexShader: PGuid = Nil): HResult; stdcall;

  end;
  IID_ID2D1DrawInfo = ID2D1DrawInfo;
  {$EXTERNALSYM IID_ID2D1DrawInfo}



  // Interface ID2D1ComputeInfo
  // ==========================
  // A transform uses this interface to specify how to render a particular pass using
  // compute shader.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ComputeInfo);'}
  {$EXTERNALSYM ID2D1ComputeInfo}
  ID2D1ComputeInfo = interface(ID2D1RenderInfo)
  ['{5598b14b-9fd7-48b7-9bdb-8f0964eb38bc}']

    // Set the constant buffer for this transform.
    function SetComputeShaderConstantBuffer(buffer: PByte;
                                            bufferCount: UINT32): HResult; stdcall;

    // Set the shader instructions for this transform.
    function SetComputeShader(shaderId: REFGUID): HResult; stdcall;

    // Sets the resource texture corresponding to the given shader texture index.
    function SetResourceTexture(textureIndex: UINT32;
                                resourceTexture: ID2D1ResourceTexture): HResult; stdcall;

  end;
  IID_ID2D1ComputeInfo = ID2D1ComputeInfo;
  {$EXTERNALSYM IID_ID2D1ComputeInfo}


  // Interface ID2D1TransformNode
  // ============================
  // A base object which can be inserted into a transform graph.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1TransformNode);'}
  {$EXTERNALSYM ID2D1TransformNode}
 ID2D1TransformNode = interface(IUnknown)
  ['{b2efe1e7-729f-4102-949f-505fa21bf666}']

    // Return the number of input this node has.
    function GetInputCount(): UINT32; stdcall;

  end;
  IID_ID2D1TransformNode = ID2D1TransformNode;
  {$EXTERNALSYM IID_ID2D1TransformNode}


  // Interface ID2D1TransformGraph
  // =============================
  // The implementation of the actual graph.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1TransformGraph);'}
  {$EXTERNALSYM ID2D1TransformGraph}
  ID2D1TransformGraph = interface(IUnknown)
  ['{13d29038-c3e6-4034-9081-13b53a417992}']

    // Return the number of input this graph has.
    function GetInputCount(): UINT32; stdcall;

    // Sets the graph to contain a single transform whose inputs map 1:1 with effect
    // inputs.
    function SetSingleTransformNode(node: ID2D1TransformNode): HResult; stdcall;

    // Adds the given transform node to the graph.
    function AddNode(node: ID2D1TransformNode): HResult; stdcall;

    // Removes the given transform node from the graph.
    function RemoveNode(node: ID2D1TransformNode): HResult; stdcall;

    // Indicates that the given transform node should be considered to be the output
    // node of the graph.
    function SetOutputNode(node: ID2D1TransformNode): HResult; stdcall;

    // Connects one node to another node inside the graph.
    function ConnectNode(fromNode: ID2D1TransformNode;
                         toNode: ID2D1TransformNode;
                         toNodeInputIndex: UINT32): HResult; stdcall;

    // Connects a transform node inside the graph to the corresponding input of the
    // encapsulating effect.
    function ConnectToEffectInput(toEffectInputIndex: UINT32;
                                  node: ID2D1TransformNode;
                                  toNodeInputIndex: UINT32): HResult; stdcall;

    // Clears all nodes and connections from the transform graph.
    procedure Clear(); stdcall;

    // Uses the specified input as the effect output.
    function SetPassthroughGraph(effectInputIndex: UINT32): HResult; stdcall;

  end;
  IID_ID2D1TransformGraph = ID2D1TransformGraph;
  {$EXTERNALSYM IID_ID2D1TransformGraph}


  // Interface ID2D1Transform
  // ========================
  // The interface implemented by a transform author.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Transform);'}
  {$EXTERNALSYM ID2D1Transform}
  ID2D1Transform = interface(ID2D1TransformNode)
  ['{ef1a287d-342a-4f76-8fdb-da0d6ea9f92b}']

    function MapOutputRectToInputRects(outputRect: D2D1_RECT_L;
                                       out inputRects: PD2D1_RECT_L;
                                       inputRectsCount: UINT32): HResult; stdcall;

    function MapInputRectsToOutputRect(inputRects: PD2D1_RECT_L;
                                       inputOpaqueSubRects: PD2D1_RECT_L;
                                       inputRectCount: UINT32;
                                       out outputRect: D2D1_RECT_L;
                                       out outputOpaqueSubRect: D2D1_RECT_L): HResult; stdcall;

    function MapInvalidRect(inputIndex: UINT32;
                            invalidInputRect: D2D1_RECT_L;
                            out invalidOutputRect: D2D1_RECT_L): HResult; stdcall;

  end;
  IID_ID2D1Transform = ID2D1Transform;
  {$EXTERNALSYM IID_ID2D1Transform}


  // Interface ID2D1DrawTransform
  // ============================
  // The interface implemented by a transform author to provide a GPU-based effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DrawTransform);'}
  {$EXTERNALSYM ID2D1DrawTransform}
  ID2D1DrawTransform = interface(ID2D1Transform)
  ['{36bfdcb6-9739-435d-a30d-a653beff6a6f}']

    function SetDrawInfo(drawInfo: ID2D1DrawInfo): HResult; stdcall;

  end;
  IID_ID2D1DrawTransform = ID2D1DrawTransform;
  {$EXTERNALSYM IID_ID2D1DrawTransform}


  // Interface ID2D1ComputeTransform
  // ===============================
  // The interface implemented by a transform author to provide a Compute Shader
  // based effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ComputeTransform);'}
  {$EXTERNALSYM ID2D1ComputeTransform}
  ID2D1ComputeTransform = interface(ID2D1Transform)
  ['{0d85573c-01e3-4f7d-bfd9-0d60608bf3c3}']

    function SetComputeInfo(computeInfo: ID2D1ComputeInfo): HResult; stdcall;

    function CalculateThreadgroups(outputRect: D2D1_RECT_L;
                                   out dimensionX: UINT32;
                                   out dimensionY: UINT32;
                                   out dimensionZ: UINT32): HResult; stdcall;

  end;
  IID_ID2D1ComputeTransform = ID2D1ComputeTransform;
  {$EXTERNALSYM IID_ID2D1ComputeTransform}


  // Interface ID2D1AnalysisTransform
  // ================================
  // The interface implemented by a transform author to indicate that it should
  // receive an analysis result callback.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1AnalysisTransform);'}
  {$EXTERNALSYM ID2D1AnalysisTransform}
  ID2D1AnalysisTransform = interface(IUnknown)
  ['{0359dc30-95e6-4568-9055-27720d130e93}']

    function ProcessAnalysisResults(analysisData: PByte;
                                    analysisDataCount: UINT32): HResult; stdcall;

  end;
  IID_ID2D1AnalysisTransform = ID2D1AnalysisTransform;
  {$EXTERNALSYM IID_ID2D1AnalysisTransform}


  // Interface ID2D1SourceTransform
  // ==============================
  // The interface implemented by a transform author to provide a CPU based source
  // effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SourceTransform);'}
  {$EXTERNALSYM ID2D1SourceTransform}
  ID2D1SourceTransform = interface(ID2D1Transform)
  ['{db1800dd-0c34-4cf9-be90-31cc0a5653e1}']

    function SetRenderInfo(renderInfo: ID2D1RenderInfo): HResult; stdcall;

    function Draw(target: ID2D1Bitmap1;
                  drawRect: D2D1_RECT_L;
                  targetOrigin: D2D1_POINT_2U): HResult; stdcall;

  end;
  IID_ID2D1SourceTransform = ID2D1SourceTransform;
  {$EXTERNALSYM IID_ID2D1SourceTransform}



  // Interface ID2D1ConcreteTransform
  // ================================
  // Base interface for built-in transforms on which precision and caching may be
  // controlled.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ConcreteTransform);'}
  {$EXTERNALSYM ID2D1ConcreteTransform}
  ID2D1ConcreteTransform = interface(ID2D1TransformNode)
  ['{1a799d8a-69f7-4e4c-9fed-437ccc6684cc}']

    // Controls the output precision and channel-depth for this transform.
    function SetOutputBuffer(bufferPrecision: D2D1_BUFFER_PRECISION;
                             channelDepth: D2D1_CHANNEL_DEPTH): HResult; stdcall;

    // Controls whether the output of this transform is cached.
    procedure SetCached(isCached: BOOL); stdcall;

  end;
  IID_ID2D1ConcreteTransform = ID2D1ConcreteTransform;
  {$EXTERNALSYM IID_ID2D1ConcreteTransform}


  // Interface ID2D1BlendTransform
  // =============================
  // An effect uses this interface to configure a blending operation.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BlendTransform);'}
  {$EXTERNALSYM ID2D1BlendTransform}
  ID2D1BlendTransform = interface(ID2D1ConcreteTransform)
  ['{63ac0b32-ba44-450f-8806-7f4ca1ff2f1b}']

    procedure SetDescription(description: D2D1_BLEND_DESCRIPTION); stdcall;

    procedure GetDescription(out description: D2D1_BLEND_DESCRIPTION); stdcall;

  end;
  IID_ID2D1BlendTransform = ID2D1BlendTransform;
  {$EXTERNALSYM IID_ID2D1BlendTransform}


  // Interface ID2D1BorderTransform
  // ==============================
  // An effect uses this interface to configure border generation.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BorderTransform);'}
  {$EXTERNALSYM ID2D1BorderTransform}
  ID2D1BorderTransform = interface(ID2D1ConcreteTransform)
  ['{4998735c-3a19-473c-9781-656847e3a347}']

    procedure SetExtendModeX(extendMode: D2D1_EXTEND_MODE); stdcall;

    procedure SetExtendModeY(extendMode: D2D1_EXTEND_MODE); stdcall;

    function GetExtendModeX(): D2D1_EXTEND_MODE; stdcall;

    function GetExtendModeY(): D2D1_EXTEND_MODE; stdcall;

  end;
  IID_ID2D1BorderTransform = ID2D1BorderTransform;
  {$EXTERNALSYM IID_ID2D1BorderTransform}


  // Interface ID2D1OffsetTransform
  // ==============================
  // An effect uses this interface to offset an image without inserting a rendering
  // pass.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1OffsetTransform);'}
  {$EXTERNALSYM ID2D1OffsetTransform}
  ID2D1OffsetTransform = interface(ID2D1TransformNode)
  ['{3fe6adea-7643-4f53-bd14-a0ce63f24042}']

    procedure SetOffset(offset: D2D1_POINT_2L); stdcall;

    function GetOffset(): D2D1_POINT_2L; stdcall;

  end;
  IID_ID2D1OffsetTransform = ID2D1OffsetTransform;
  {$EXTERNALSYM IID_ID2D1OffsetTransform}


  // Interface ID2D1BoundsAdjustmentTransform
  // ========================================
  // An effect uses this interface to alter the image rectangle of its input.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BoundsAdjustmentTransform);'}
  {$EXTERNALSYM ID2D1BoundsAdjustmentTransform}
  ID2D1BoundsAdjustmentTransform = interface(ID2D1TransformNode)
  ['{90f732e2-5092-4606-a819-8651970baccd}']

    procedure SetOutputBounds(outputBounds: D2D1_RECT_L); stdcall;

    procedure GetOutputBounds(out outputBounds: D2D1_RECT_L); stdcall;

  end;
  IID_ID2D1BoundsAdjustmentTransform = ID2D1BoundsAdjustmentTransform;
  {$EXTERNALSYM IID_ID2D1BoundsAdjustmentTransform}


  // Interface ID2D1EffectImpl
  // =========================
  // This is the interface implemented by an effect author, along with the
  // constructor and registration information.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1EffectImpl);'}
  {$EXTERNALSYM ID2D1EffectImpl}
  ID2D1EffectImpl = interface(IUnknown)
  ['{a248fd3f-3e6c-4e63-9f03-7f68ecc91db9}']

    // Initialize the effect with a context and a transform graph. The effect must
    // populate the transform graph with a topology and can update it later.
    function Initialize(effectContext: ID2D1EffectContext;
                        transformGraph: ID2D1TransformGraph): HResult; stdcall;

    // Initialize the effect with a context and a transform graph. The effect must
    // populate the transform graph with a topology and can update it later.
    function PrepareForRender(changeType: D2D1_CHANGE_TYPE): HResult; stdcall;

    // Sets a new transform graph to the effect.  This happens when the number of
    // inputs to the effect changes, if the effect support a variable number of inputs.
    function SetGraph(transformGraph: ID2D1TransformGraph): HResult; stdcall;

  end;
  IID_ID2D1EffectImpl = ID2D1EffectImpl;
  {$EXTERNALSYM IID_ID2D1EffectImpl}


  // Interface ID2D1EffectContext
  // ============================
  // The internal context handed to effect authors to create transforms from effects
  // and any other operation tied to context which is not useful to the application
  // facing API.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1EffectContext);'}
  {$EXTERNALSYM ID2D1EffectContext}
  ID2D1EffectContext = interface(IUnknown)
  ['{3d9f916b-27dc-4ad7-b4f1-64945340f563}']

    procedure GetDpi(out dpiX: FLOAT;
                     out dpiY: FLOAT); stdcall;

    // Create a new effect, the effect must either be built in or previously registered
    // through ID2D1Factory1.RegisterEffect.
    function CreateEffect(const effectId: REFCLSID;
                          out effect: ID2D1Effect): HResult; stdcall;

    function GetMaximumSupportedFeatureLevel(featureLevels: D3D_FEATURE_LEVEL;
                                             featureLevelsCount: UINT32;
                                             out maximumSupportedFeatureLevel: D3D_FEATURE_LEVEL): HResult; stdcall;

    // Create a transform node from the passed in effect.
    function CreateTransformNodeFromEffect(effect: ID2D1Effect;
                                           out transformNode: ID2D1TransformNode): HResult; stdcall;

    function CreateBlendTransform(numInputs: UINT32;
                                  blendDescription: D2D1_BLEND_DESCRIPTION;
                                  out transform: ID2D1BlendTransform): HResult; stdcall;

    function CreateBorderTransform(extendModeX: D2D1_EXTEND_MODE;
                                   extendModeY: D2D1_EXTEND_MODE;
                                   out transform: ID2D1BorderTransform): HResult; stdcall;

    function CreateOffsetTransform(offset: D2D1_POINT_2L;
                                   out transform: ID2D1OffsetTransform): HResult; stdcall;

    function CreateBoundsAdjustmentTransform(outputRectangle: D2D1_RECT_L;
                                             out transform: ID2D1BoundsAdjustmentTransform): HResult; stdcall;

    function LoadPixelShader(const shaderId: REFGUID;
                             shaderBuffer: PByte;
                             shaderBufferCount: UINT32): HResult; stdcall;

    function LoadVertexShader(const resourceId: REFGUID;
                              shaderBuffer: PByte;
                              shaderBufferCount: UINT32): HResult; stdcall;

    function LoadComputeShader(const resourceId: REFGUID;
                               shaderBuffer: PByte;
                               shaderBufferCount: UINT32): HResult; stdcall;

    function IsShaderLoaded(const shaderId: REFGUID): BOOL; stdcall;

    function CreateResourceTexture(const resourceId: TGUID;
                                   resourceTextureProperties: D2D1_RESOURCE_TEXTURE_PROPERTIES;
                                   data: PByte;
                                   strides: UINT32;
                                   dataSize: UINT32;
                                   out resourceTexture: ID2D1ResourceTexture): HResult; stdcall;

    function FindResourceTexture(const resourceId: TGUID;
                                 out resourceTexture: ID2D1ResourceTexture): HResult; stdcall;

    function CreateVertexBuffer(vertexBufferProperties: D2D1_VERTEX_BUFFER_PROPERTIES;
                                const resourceId: TGUID;
                                customVertexBufferProperties: D2D1_CUSTOM_VERTEX_BUFFER_PROPERTIES;
                                out buffer: ID2D1VertexBuffer): HResult; stdcall;

    function FindVertexBuffer(const resourceId: TGUID;
                              out buffer: ID2D1VertexBuffer): HResult; stdcall;

    // Creates a color context from a color space.  If the space is Custom, the context
    // is initialized from the profile/profileSize arguments.  Otherwise the context is
    // initialized with the profile bytes associated with the space and
    // profile/profileSize are ignored.
    function CreateColorContext(space: D2D1_COLOR_SPACE;
                                profile: PByte;
                                profileSize: UINT32;
                                out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CreateColorContextFromFilename(const filename: PCWSTR;
                                            out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CreateColorContextFromWicColorContext(wicColorContext: IWICColorContext;
                                                   out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CheckFeatureSupport(feature: D2D1_FEATURE;
                                 out featureSupportData: Pointer;
                                 featureSupportDataSize: UINT32): HResult; stdcall;

    // Indicates whether the buffer precision is supported by D2D.
    function IsBufferPrecisionSupported(bufferPrecision: D2D1_BUFFER_PRECISION): BOOL; stdcall;

  end;
  IID_ID2D1EffectContext = ID2D1EffectContext;
  {$EXTERNALSYM IID_ID2D1EffectContext}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

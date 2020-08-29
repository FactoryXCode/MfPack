// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DComp.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: Enables high-performance bitmap composition with transforms,
//              effects, and animations.
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
// Remarks: Requires Windows 8 or later.
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
// Source: dcomp.h
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
unit WinApi.DirectX.DComp;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {DirectX}
  WinApi.DirectX.D2DBaseTypes,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1Effects,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGI1_2,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D3D9Types,
  WinApi.DirectX.DCompTypes,
  WinApi.DirectX.DCompAnimation;

  {$WEAKPACKAGEUNIT ON}
  {$I 'WinApiTypes.inc'}


type
// #if (NTDDI_VERSION >= NTDDI_WIN8)

  // Forward interface declarations

  PIDCompositionAffineTransform2DEffect = ^IDCompositionAffineTransform2DEffect;
  IDCompositionAffineTransform2DEffect = interface;
  {$EXTERNALSYM IDCompositionAffineTransform2DEffect}

  // IDCompositionAnimation = interface;  >> MfPack.DCompAnimation.pas

  PIDCompositionArithmeticCompositeEffect = ^IDCompositionArithmeticCompositeEffect;
  IDCompositionArithmeticCompositeEffect = interface;
  {$EXTERNALSYM IDCompositionArithmeticCompositeEffect}

  PIDCompositionBlendEffect = ^IDCompositionBlendEffect;
  IDCompositionBlendEffect = interface;
  {$EXTERNALSYM IDCompositionBlendEffect}

  PIDCompositionBrightnessEffect = ^IDCompositionBrightnessEffect;
  IDCompositionBrightnessEffect = interface;
  {$EXTERNALSYM IDCompositionBrightnessEffect}

  PIDCompositionClip = ^IDCompositionClip;
  IDCompositionClip = interface;
  {$EXTERNALSYM IDCompositionClip}

  PIDCompositionColorMatrixEffect = ^IDCompositionColorMatrixEffect;
  IDCompositionColorMatrixEffect = interface;
  {$EXTERNALSYM IDCompositionColorMatrixEffect}

  PIDCompositionCompositeEffect = ^IDCompositionCompositeEffect;
  IDCompositionCompositeEffect = interface;
  {$EXTERNALSYM IDCompositionCompositeEffect}

  PIDCompositionDevice = ^IDCompositionDevice;
  IDCompositionDevice = interface;
  {$EXTERNALSYM IDCompositionDevice}

  PIDCompositionTarget = ^IDCompositionTarget;
  IDCompositionTarget = interface;
  {$EXTERNALSYM IDCompositionTarget}

  PIDCompositionVisual = ^IDCompositionVisual;
  IDCompositionVisual = interface;
  {$EXTERNALSYM IDCompositionVisual}

  PIDCompositionTransform = ^IDCompositionTransform;
  IDCompositionTransform = interface;
  {$EXTERNALSYM IDCompositionTransform}

  PIDCompositionTransform3D = ^IDCompositionTransform3D;
  IDCompositionTransform3D = interface;
  {$EXTERNALSYM IDCompositionTransform3D}

  PIDCompositionTranslateTransform = ^IDCompositionTranslateTransform;
  IDCompositionTranslateTransform = interface;
  {$EXTERNALSYM IDCompositionTranslateTransform}

  PIDCompositionTranslateTransform3D = ^IDCompositionTranslateTransform3D;
  IDCompositionTranslateTransform3D = interface;
  {$EXTERNALSYM IDCompositionTranslateTransform3D}

  PIDCompositionScaleTransform = ^IDCompositionScaleTransform;
  IDCompositionScaleTransform = interface;
  {$EXTERNALSYM IDCompositionScaleTransform}

  PIDCompositionScaleTransform3D = ^IDCompositionScaleTransform3D;
  IDCompositionScaleTransform3D = interface;
  {$EXTERNALSYM IDCompositionScaleTransform3D}

  PIDCompositionRotateTransform = ^IDCompositionRotateTransform;
  IDCompositionRotateTransform = interface;
  {$EXTERNALSYM IDCompositionRotateTransform}

  PIDCompositionRotateTransform3D = ^IDCompositionRotateTransform3D;
  IDCompositionRotateTransform3D = interface;
  {$EXTERNALSYM IDCompositionRotateTransform3D}

  PIDCompositionSkewTransform = ^IDCompositionSkewTransform;
  IDCompositionSkewTransform = interface;
  {$EXTERNALSYM IDCompositionSkewTransform}

  PIDCompositionMatrixTransform = ^IDCompositionMatrixTransform;
  IDCompositionMatrixTransform = interface;
  {$EXTERNALSYM IDCompositionMatrixTransform}

  PIDCompositionMatrixTransform3D = ^IDCompositionMatrixTransform3D;
  IDCompositionMatrixTransform3D = interface;
  {$EXTERNALSYM IDCompositionMatrixTransform3D}

  PIDCompositionEffect = ^IDCompositionEffect;
  IDCompositionEffect = interface;
  {$EXTERNALSYM IDCompositionEffect}

  PIDCompositionEffectGroup = ^IDCompositionEffectGroup;
  IDCompositionEffectGroup = interface;
  {$EXTERNALSYM IDCompositionEffectGroup}

  PIDCompositionRectangleClip = ^IDCompositionRectangleClip;
  IDCompositionRectangleClip = interface;
  {$EXTERNALSYM IDCompositionRectangleClip}

  PIDCompositionSurface = ^IDCompositionSurface;
  IDCompositionSurface = interface;
  {$EXTERNALSYM IDCompositionSurface}

  PIDCompositionVirtualSurface = ^IDCompositionVirtualSurface;
  IDCompositionVirtualSurface = interface;
  {$EXTERNALSYM IDCompositionVirtualSurface}

  PIDCompositionFilterEffect = ^IDCompositionFilterEffect;
  IDCompositionFilterEffect = interface;
  {$EXTERNALSYM IDCompositionFilterEffect}

  PIDCompositionGaussianBlurEffect = ^IDCompositionGaussianBlurEffect;
  IDCompositionGaussianBlurEffect = interface;
  {$EXTERNALSYM IDCompositionGaussianBlurEffect}

  PIDCompositionShadowEffect = ^IDCompositionShadowEffect;
  IDCompositionShadowEffect = interface;
  {$EXTERNALSYM IDCompositionShadowEffect}

  PIDCompositionHueRotationEffect = ^IDCompositionHueRotationEffect;
  IDCompositionHueRotationEffect = interface;
  {$EXTERNALSYM IDCompositionHueRotationEffect}

  PIDCompositionSaturationEffect = ^IDCompositionSaturationEffect;
  IDCompositionSaturationEffect = interface;
  {$EXTERNALSYM IDCompositionSaturationEffect}

  PIDCompositionTurbulenceEffect = ^IDCompositionTurbulenceEffect;
  IDCompositionTurbulenceEffect = interface;
  {$EXTERNALSYM IDCompositionTurbulenceEffect}

  PIDCompositionLinearTransferEffect = ^IDCompositionLinearTransferEffect;
  IDCompositionLinearTransferEffect = interface;
  {$EXTERNALSYM IDCompositionLinearTransferEffect}

  PIDCompositionTableTransferEffect = ^IDCompositionTableTransferEffect;
  IDCompositionTableTransferEffect = interface;
  {$EXTERNALSYM IDCompositionTableTransferEffect}



  // INTERFACES ////////////////////////////////////////////////////////////////

  //+---------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionDevice
  //
  //  Synopsis:
  //   Serves as the root factory for all other DirectComposition objects and
  //   controls transactional composition.
  //
  //----------------------------------------------------------------------------


  // Interface IDCompositionDevice
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionDevice);'}
  {$EXTERNALSYM IDCompositionDevice}
  IDCompositionDevice = interface(IUnknown)
  ['{C37EA93A-E7AA-450D-B16F-9746CB0407F3}']
    // Commits all DirectComposition commands pending on this device.
    function Commit(): HResult; stdcall;

    // Waits for the last Commit to be processed by the composition engine
    function WaitForCommitCompletion(): HResult; stdcall;

    // Gets timing information about the composition engine.
    function GetFrameStatistics(out statistics: DCOMPOSITION_FRAME_STATISTICS): HResult; stdcall;

    // Creates a composition target bound to a window represented by an HWND.
    function CreateTargetForHwnd(_hwnd: HWND;
                                 topmost: BOOL;
                                 out target: IDCompositionTarget): HResult; stdcall;

    // Creates a new visual object.
    function CreateVisual(out visual: IDCompositionVisual): HResult; stdcall;

    // Creates a DirectComposition surface object
    function CreateSurface(width: UINT;
                           height: UINT;
                           pixelFormat: DXGI_FORMAT;
                           alphaMode: DXGI_ALPHA_MODE;
                           out surface: IDCompositionSurface): HResult; stdcall;

    // Creates a DirectComposition virtual surface object
    function CreateVirtualSurface(initialWidth: UINT;
                                  initialHeight: UINT;
                                  pixelFormat: DXGI_FORMAT;
                                  alphaMode: DXGI_ALPHA_MODE;
                                  out virtualSurface: IDCompositionVirtualSurface): HResult; stdcall;

    // Creates a surface wrapper around a pre-existing surface that can be associated with one or more visuals for composition.
    function CreateSurfaceFromHandle(_handle: THandle;
                                     out surface: IUnknown  // Pass this parameter as IUnknown(interface)
                                    ): HResult; stdcall;

    // Creates a wrapper object that represents the rasterization of a layered window and which can be associated with a visual for composition.
    function CreateSurfaceFromHwnd(_hwnd: HWND;
                                   out surface: IUnknown // Pass this parameter as IUnknown(interface)
                                  ): HResult; stdcall;

    // Creates a 2D translation transform object.
    function CreateTranslateTransform(out translateTransform: IDCompositionTranslateTransform): HResult; stdcall;

    // Creates a 2D scale transform object.
    function CreateScaleTransform(out scaleTransform: IDCompositionScaleTransform): HResult; stdcall;

    // Creates a 2D rotation transform object.
    function CreateRotateTransform(out rotateTransform: IDCompositionRotateTransform): HResult; stdcall;

    // Creates a 2D skew transform object.
    function CreateSkewTransform(out skewTransform: IDCompositionSkewTransform): HResult; stdcall;

    // Creates a 2D 3x2 matrix transform object.
    function CreateMatrixTransform(out matrixTransform: IDCompositionMatrixTransform): HResult; stdcall;

    // Creates a 2D transform object that holds an array of 2D transform objects.
    function CreateTransformGroup(transforms: PIDCompositionTransform;
                                  elements: UINT;
                                  out transformGroup: IDCompositionTransform): HResult; stdcall;

    // Creates a 3D translation transform object.
    function CreateTranslateTransform3D(out translateTransform3D: IDCompositionTranslateTransform3D): HResult; stdcall;

    // Creates a 3D scale transform object.
    function CreateScaleTransform3D(out scaleTransform3D: IDCompositionScaleTransform3D): HResult; stdcall;

    // Creates a 3D rotation transform object.
    function CreateRotateTransform3D(out rotateTransform3D: IDCompositionRotateTransform3D): HResult; stdcall;

    // Creates a 3D 4x4 matrix transform object.
    function CreateMatrixTransform3D(out matrixTransform3D: IDCompositionMatrixTransform3D): HResult; stdcall;

    // Creates a 3D transform object that holds an array of 3D transform objects.
    function CreateTransform3DGroup(transforms3D: IDCompositionTransform3D;
                                    elements: UINT;
                                    out transform3DGroup: IDCompositionTransform3D): HResult; stdcall;

    // Creates an effect group
    function CreateEffectGroup(out effectGroup: IDCompositionEffectGroup): HResult; stdcall;

    // Creates a clip object that can be used to clip the contents of a visual subtree.
    function CreateRectangleClip(out clip: IDCompositionRectangleClip): HResult; stdcall;

    // Creates an animation object
    function CreateAnimation(out animation: IDCompositionAnimation): HResult; stdcall;

    // Returns the states of the app's DX device and DWM's dx devices
    function CheckDeviceState(out pfValid: BOOL): HResult; stdcall;

  end;
  IID_IDCompositionDevice = IDCompositionDevice;
  {$EXTERNALSYM IID_IDCompositionDevice}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionTarget
  //
  //  Synopsis:
  //   An IDCompositionTarget interface represents a binding between a
  //   DirectComposition visual tree and a destination on top of which the
  //   visual tree should be composed.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTarget);'}
  {$EXTERNALSYM IDCompositionTarget}
  IDCompositionTarget = interface(IUnknown)
  ['{eacdd04c-117e-4e17-88f4-d1b12b0e3d89}']
    // Sets the root visual
    function SetRoot({In_opt} visual: IDCompositionVisual = Nil): HResult; stdcall;

  end;
  IID_IDCompositionTarget = IDCompositionTarget;
  {$EXTERNALSYM IID_IDCompositionTarget}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionVisual
  //
  //  Synopsis:
  //   An IDCompositionVisual interface represents a visual that participates in
  //   a visual tree.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionVisual);'}
  {$EXTERNALSYM IDCompositionVisual}
  IDCompositionVisual = interface(IUnknown)
  ['{4d93059d-097b-4651-9a60-f0f25116e2f3}']
    // Changes the value of OffsetX property
    function SetOffsetX(offsetX: Single): HResult; stdcall;

    // Animates the value of the OffsetX property.
    function _SetOffsetX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of OffsetY property
    function SetOffsetY(offsetY: Single): HResult; stdcall;

    // Animates the value of the OffsetY property.
    function _SetOffsetY(animation: IDCompositionAnimation): HResult; stdcall;

    // Sets the matrix that modifies the coordinate system of this visual.
    function SetTransform(matrix: D2D_MATRIX_3X2_F): HResult; stdcall;

    // Sets the transformation object that modifies the coordinate system of this visual.
    function _SetTransform({In_opt} transform: IDCompositionTransform = Nil): HResult; stdcall;

    // Sets the visual that should act as this visual's parent for the
    // purpose of establishing a base coordinate system.
    function SetTransformParent({In_opt} visual: IDCompositionVisual = Nil): HResult; stdcall;

    // Sets the effect object that is applied during the rendering of this visual
    function SetEffect({In_opt} effect: IDCompositionEffect = Nil): HResult; stdcall;

    // Sets the mode to use when interpolating pixels from bitmaps drawn not
    // exactly at scale and axis-aligned.
    function SetBitmapInterpolationMode(interpolationMode: DCOMPOSITION_BITMAP_INTERPOLATION_MODE): HResult; stdcall;

    // Sets the mode to use when drawing the edge of bitmaps that are not
    // exactly axis-aligned and at precise pixel boundaries.
    function SetBorderMode(borderMode: DCOMPOSITION_BORDER_MODE): HResult; stdcall;

    // Sets the clip object that restricts the rendering of this visual to a D2D rectangle.
    function SetClip(_rect: D2D_RECT_F): HResult; stdcall;

    // Sets the clip object that restricts the rendering of this visual to a rectangle.
    function _SetClip({In_opt} clip: IDCompositionClip = Nil): HResult; stdcall;

    // Associates a bitmap with a visual
    function SetContent({In_opt} content: IUnknown = Nil// Pass this parameter as IUnknown(interface)
                       ): HResult; stdcall;

    // Adds a visual to the children list of another visual.
    function AddVisual(visual: IDCompositionVisual;
                       insertAbove: BOOL;
                       {In_opt} referenceVisual: IDCompositionVisual = Nil): HResult; stdcall;

    // Removes a visual from the children list of another visual.
    function RemoveVisual(visual: IDCompositionVisual): HResult; stdcall;

    // Removes all visuals from the children list of another visual.
    function RemoveAllVisuals(): HResult; stdcall;

    // Sets the mode to use when composing the bitmap against the render target.
    function SetCompositeMode(compositeMode: DCOMPOSITION_COMPOSITE_MODE): HResult; stdcall;

  end;
  IID_IDCompositionVisual = IDCompositionVisual;
  {$EXTERNALSYM IID_IDCompositionVisual}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionEffect
  //
  //  Synopsis:
  //   An IDCompositionEffect interface represents an effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionEffect);'}
  {$EXTERNALSYM IDCompositionEffect}
  IDCompositionEffect = interface(IUnknown)
  ['{EC81B08F-BFCB-4e8d-B193-A915587999E8}']

  end;
  IID_IDCompositionEffect = IDCompositionEffect;
  {$EXTERNALSYM IID_IDCompositionEffect}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionTransform3D
  //
  //  Synopsis:
  //   An IDCompositionTransform3D interface represents a 3D transformation.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTransform3D);'}
  {$EXTERNALSYM IDCompositionTransform3D}
  IDCompositionTransform3D = interface(IDCompositionEffect)
  ['{71185722-246B-41f2-AAD1-0443F7F4BFC2}']

  end;
  IID_IDCompositionTransform3D = IDCompositionTransform3D;
  {$EXTERNALSYM IID_IDCompositionTransform3D}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionTransform
  //
  //  Synopsis:
  //   An IDCompositionTransform interface represents a 2D transformation that
  //   can be used to modify the coordinate space of a visual subtree.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTransform);'}
  {$EXTERNALSYM IDCompositionTransform}
  IDCompositionTransform = interface(IDCompositionTransform3D)
  ['{FD55FAA7-37E0-4c20-95D2-9BE45BC33F55}']

  end;
  IID_IDCompositionTransform = IDCompositionTransform;
  {$EXTERNALSYM IID_IDCompositionTransform}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionTranslateTransform
  //
  //  Synopsis:
  //   An IDCompositionTranslateTransform interface represents a 2D transformation
  //   that affects only the offset of a visual along the x and y axes.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTranslateTransform);'}
  {$EXTERNALSYM IDCompositionTranslateTransform}
  IDCompositionTranslateTransform = interface(IDCompositionTransform)
  ['{06791122-C6F0-417d-8323-269E987F5954}']

    // Changes the value of the OffsetX property.
    function SetOffsetX(offsetX: Single): HResult; stdcall;

    // Animates the value of the OffsetX property.
    function _SetOffsetX(animation: IDCompositionAnimation): HResult; stdcall;

   // Changes the value of the OffsetY property.
    function SetOffsetY(offsetY: Single): HResult; stdcall;

    // Animates the value of the OffsetY property.
    function _SetOffsetY(animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionTranslateTransform = IDCompositionTranslateTransform;
  {$EXTERNALSYM IID_IDCompositionTranslateTransform}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionScaleTransform
  //
  //  Synopsis:
  //   An IDCompositionScaleTransform interface represents a 2D transformation that
  //   affects the scale of a visual along the x and y axes. The coordinate system
  //   is scaled from the specified center point.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionScaleTransform);'}
  {$EXTERNALSYM IDCompositionScaleTransform}
  IDCompositionScaleTransform = interface(IDCompositionTransform)
  ['{71FDE914-40EF-45ef-BD51-68B037C339F9}']
    // Changes the value of the ScaleX property.
    function SetScaleX(scaleX: Single): HResult; stdcall;

    // Animates the value of the ScaleX property.
    function _SetScaleX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the ScaleY property.
    function SetScaleY(scaleY: Single): HResult; stdcall;

    // Animates the value of the ScaleY property.
    function _SetScaleY(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the CenterX property.
    function SetCenterX(centerX: Single): HResult; stdcall;

    // Animates the value of the CenterX property.
    function _SetCenterX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the CenterY property.
    function SetCenterY(centerY: Single): HResult; stdcall;

    // Animates the value of the CenterY property.
    function _SetCenterY(animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionScaleTransform = IDCompositionScaleTransform;
  {$EXTERNALSYM IID_IDCompositionScaleTransform}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionRotateTransform
  //
  //  Synopsis:
  //   An IDCompositionRotateTransform interface represents a 2D transformation
  //   that affects the rotation of a visual along the z axis. The coordinate system
  //   is rotated around the specified center point.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionRotateTransform);'}
  {$EXTERNALSYM IDCompositionRotateTransform}
  IDCompositionRotateTransform = interface(IDCompositionTransform)
  ['{641ED83C-AE96-46c5-90DC-32774CC5C6D5}']
    // Changes the value of the Angle property.
    function SetAngle(angle: Single): HResult; stdcall;

    // Animates the value of the Angle property.
    function _SetAngle(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the CenterX property.
    function SetCenterX(centerX: Single): HResult; stdcall;

    // Animates the value of the CenterX property.
    function _SetCenterX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the CenterY property.
    function SetCenterY(centerY: Single): HResult; stdcall;

    // Animates the value of the CenterY property.
    function _SetCenterY(animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionRotateTransform = IDCompositionRotateTransform;
  {$EXTERNALSYM IID_IDCompositionRotateTransform}


  //+-----------------------------------------------------------------------------
  //
  //  Interface:
  //   IDCompositionSkewTransform
  //
  //  Synopsis:
  //   An IDCompositionSkewTransform interface represents a 2D transformation that
  //   affects the skew of a visual along the x and y axes. The coordinate system
  //   is skewed around the specified center point.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionSkewTransform);'}
  {$EXTERNALSYM IDCompositionSkewTransform}
  IDCompositionSkewTransform = interface(IDCompositionTransform)
  ['{E57AA735-DCDB-4c72-9C61-0591F58889EE}']
    // Changes the value of the AngleX property.
    function SetAngleX(angleX: Single): HResult; stdcall;

    // Animates the value of the AngleX property.
    function _SetAngleX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the AngleY property.
    function SetAngleY(angleY: Single): HResult; stdcall;

    // Animates the value of the AngleY property.
    function _SetAngleY(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the CenterX property.
    function SetCenterX(centerX: Single): HResult; stdcall;

    // Animates the value of the CenterX property.
    function _SetCenterX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the CenterY property.
    function SetCenterY(centerY: Single): HResult; stdcall;

    // Animates the value of the CenterY property.
    function _SetCenterY(animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionSkewTransform = IDCompositionSkewTransform;
  {$EXTERNALSYM IID_IDCompositionSkewTransform}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionMatrixTransform
  //
  // Synopsis:
  //     An IDCompositionMatrixTransform interface represents an arbitrary affine
  //     2D transformation defined by a 3x2 matrix.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionMatrixTransform);'}
  {$EXTERNALSYM IDCompositionMatrixTransform}
  IDCompositionMatrixTransform = interface(IDCompositionTransform)
  ['{16CDFF07-C503-419c-83F2-0965C7AF1FA6}']

    // Changes all values of the matrix of this transform.
    function SetMatrix(matrix: D2D_MATRIX_3X2_F): HResult; stdcall;

    // Changes a single element of the matrix of this transform.
    function SetMatrixElement(row: Integer;
                              column: Integer;
                              value: Single): HResult; stdcall;

    // Animates a single element of the matrix of this transform.
    function _SetMatrixElement(row: Integer;
                               column: Integer;
                               animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionMatrixTransform = IDCompositionMatrixTransform;
  {$EXTERNALSYM IID_IDCompositionMatrixTransform}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionEffectGroup
  //
  // Synopsis:
  //     An IDCompositionEffectGroup holds effects, inluding 3D transforms that can
  //     be applied to a visual.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionEffectGroup);'}
  {$EXTERNALSYM IDCompositionEffectGroup}
  IDCompositionEffectGroup = interface(IDCompositionEffect)
  ['{A7929A74-E6B2-4bd6-8B95-4040119CA34D}']

    // Changes the opacity property.
    function SetOpacity(opacity: Single): HResult; stdcall;

    // Animates the opacity property
    function _SetOpacity(animation: IDCompositionAnimation): HResult; stdcall;

    // Sets the 3D transform
    function SetTransform3D({opt} transform3D: IDCompositionTransform3D = Nil): HResult; stdcall;

  end;
  IID_IDCompositionEffectGroup = IDCompositionEffectGroup;
  {$EXTERNALSYM IID_IDCompositionEffectGroup}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionTranslateTransform3D
  //
  // Synopsis:
  //     An IDCompositionTranslateTransform3D interface represents a 3D transformation
  //     that affects the offset of a visual along the x,y and z axes.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTranslateTransform3D);'}
  {$EXTERNALSYM IDCompositionTranslateTransform3D}
  IDCompositionTranslateTransform3D = interface(IDCompositionTransform3D)
  ['{91636D4B-9BA1-4532-AAF7-E3344994D788}']

    // Changes the value of the OffsetX property.
    function SetOffsetX(offsetX: Single): HResult; overload; stdcall;

    // Animates the value of the OffsetX property.
    function SetOffsetX(animation: IDCompositionAnimation): HResult; overload; stdcall;

   // Changes the value of the OffsetY property.
    function SetOffsetY(offsetY: Single): HResult; overload; stdcall;

    // Animates the value of the OffsetY property.
    function SetOffsetY(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the OffsetZ property.
    function SetOffsetZ(offsetZ: Single): HResult; overload; stdcall;

    // Animates the value of the OffsetZ property.
    function SetOffsetZ(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionTranslateTransform3D = IDCompositionTranslateTransform3D;
  {$EXTERNALSYM IID_IDCompositionTranslateTransform3D}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionScaleTransform3D
  //
  // Synopsis:
  //     An IDCompositionScaleTransform3D interface represents a 3D transformation that
  //     affects the scale of a visual along the x, y and z axes. The coordinate system
  //     is scaled from the specified center point.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionScaleTransform3D);'}
  {$EXTERNALSYM IDCompositionScaleTransform3D}
  IDCompositionScaleTransform3D = interface(IDCompositionTransform3D)
  ['{2A9E9EAD-364B-4b15-A7C4-A1997F78B389}']

    // Changes the value of the ScaleX property.
    function SetScaleX(scaleX: Single): HResult; overload; stdcall;

    // Animates the value of the ScaleX property.
    function SetScaleX(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the ScaleY property.
    function SetScaleY(scaleY: Single): HResult; overload; stdcall;

    // Animates the value of the ScaleY property.
    function SetScaleY(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the ScaleZ property.
    function SetScaleZ(scaleZ: Single): HResult; overload; stdcall;

    // Animates the value of the ScaleZ property.
    function SetScaleZ(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the CenterX property.
    function SetCenterX(centerX: Single): HResult; overload; stdcall;

    // Animates the value of the CenterX property.
    function SetCenterX(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the CenterY property.
    function SetCenterY(centerY: Single): HResult; overload; stdcall;

    // Animates the value of the CenterY property.
    function SetCenterY(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the CenterZ property.
    function SetCenterZ(centerZ: Single): HResult; overload; stdcall;

    // Animates the value of the CenterZ property.
    function SetCenterZ(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionScaleTransform3D = IDCompositionScaleTransform3D;
  {$EXTERNALSYM IID_IDCompositionScaleTransform3D}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionRotateTransform3D
  //
  // Synopsis:
  //     An IDCompositionRotateTransform3D interface represents a 3D transformation
  //     that affects the rotation of a visual along the specified axis at the
  //     specified center point.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionRotateTransform3D);'}
  {$EXTERNALSYM IDCompositionRotateTransform3D}
  IDCompositionRotateTransform3D = interface(IDCompositionTransform3D)
  ['{D8F5B23F-D429-4a91-B55A-D2F45FD75B18}']

    // Changes the value of the Angle property.
    function SetAngle(angle: Single): HResult; overload; stdcall;

    // Animates the value of the Angle property.
    function SetAngle(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the AxisX property.
    function SetAxisX(axisX: Single): HResult; overload; stdcall;

    // Animates the value of the AxisX property.
    function SetAxisX(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the AxisY property.
    function SetAxisY(axisY: Single): HResult; overload; stdcall;

    // Animates the value of the AxisY property.
    function SetAxisY(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the AxisZ property.
    function SetAxisZ(axisZ: Single): HResult; overload; stdcall;

    // Animates the value of the AxisZ property.
    function SetAxisZ(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the CenterX property.
    function SetCenterX(centerX: Single): HResult; overload; stdcall;

    // Animates the value of the CenterX property.
    function SetCenterX(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the CenterY property.
    function SetCenterY(centerY: Single): HResult; overload; stdcall;

    // Animates the value of the CenterY property.
    function SetCenterY(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the CenterZ property.
    function SetCenterZ(centerZ: Single): HResult; overload; stdcall;

    // Animates the value of the CenterZ property.
    function SetCenterZ(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionRotateTransform3D = IDCompositionRotateTransform3D;
  {$EXTERNALSYM IID_IDCompositionRotateTransform3D}


  //+---------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionMatrixTransform3D
  //
  // Synopsis:
  //     An IDCompositionMatrixTransform3D interface represents an arbitrary
  //     3D transformation defined by a 4x4 matrix.
  //
  //----------------------------------------------------------------------------

// #pragma warning(push)
// #pragma warning(disable : 4995)    //D3DMATRIX': name was marked as #pragma deprecated

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionMatrixTransform3D);'}
  {$EXTERNALSYM IDCompositionMatrixTransform3D}
  IDCompositionMatrixTransform3D = interface(IDCompositionTransform3D)
  ['{4B3363F0-643B-41b7-B6E0-CCF22D34467C}']

    // Changes all values of the matrix of this transform.
    function SetMatrix(matrix: D3DMATRIX): HResult; stdcall;

    // Changes a Single element of the matrix of this transform.
    function SetMatrixElement(row: Integer;
                              column: Integer;
                              value: Single): HResult; overload; stdcall;

    // Animates a Single element of the matrix of this transform.
    function SetMatrixElement(row: Integer;
                              column: Integer;
                              animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionMatrixTransform3D = IDCompositionMatrixTransform3D;
  {$EXTERNALSYM IID_IDCompositionMatrixTransform3D}

  // #pragma warning(pop)

  //+---------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionClip
  //
  // Synopsis:
  //     An IDCompositionClip interface represents a rectangle that restricts the
  //     rasterization of a visual subtree.
  //
  //----------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionClip);'}
  {$EXTERNALSYM IDCompositionClip}
  IDCompositionClip = interface(IUnknown)
  ['{64AC3703-9D3F-45ec-A109-7CAC0E7A13A7}']

  end;
  IID_IDCompositionClip = IDCompositionClip;
  {$EXTERNALSYM IID_IDCompositionClip}


  //+---------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionRectangleClip
  //
  // Synopsis:
  //     An IDCompositionRectangleClip interface represents a rectangle that restricts
  //     the rasterization of a visual subtree.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionRectangleClip);'}
  {$EXTERNALSYM IDCompositionRectangleClip}
  IDCompositionRectangleClip = interface(IDCompositionClip)
  ['{9842AD7D-D9CF-4908-AED7-48B51DA5E7C2}']

    // Changes the value of the Left property.
    function SetLeft(left: Single): HResult; stdcall;

    // Animates the value of the Left property.
    function _SetLeft(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the Top property.
    function SetTop(top: Single): HResult; stdcall;

    // Animates the value of the Top property.
    function _SetTop(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the Right property.
    function SetRight(right: Single): HResult; stdcall;

    // Animates the value of the Right property.
    function _SetRight(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the Bottom property.
    function SetBottom(bottom: Single): HResult; stdcall;

    // Animates the value of the Bottom property.
    function _SetBottom(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the x radius of the ellipse that rounds the
    // top-left corner of the clip.
    function SetTopLeftRadiusX(radius: Single): HResult; stdcall;

    // Animates the value of the x radius of the ellipse that rounds the
    // top-left corner of the clip.
    function _SetTopLeftRadiusX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the y radius of the ellipse that rounds the
    // top-left corner of the clip.
    function SetTopLeftRadiusY(radius: Single): HResult; stdcall;

    // Animates the value of the y radius of the ellipse that rounds the
    // top-left corner of the clip.
    function _SetTopLeftRadiusY(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the x radius of the ellipse that rounds the
    // top-right corner of the clip.
    function SetTopRightRadiusX(radius: Single): HResult; stdcall;

    // Animates the value of the x radius of the ellipse that rounds the
    // top-right corner of the clip.
    function _SetTopRightRadiusX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the y radius of the ellipse that rounds the
    // top-right corner of the clip.
    function SetTopRightRadiusY(radius: Single): HResult; stdcall;

    // Animates the value of the y radius of the ellipse that rounds the
    // top-right corner of the clip.
    function _SetTopRightRadiusY(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the x radius of the ellipse that rounds the
    // bottom-left corner of the clip.
    function SetBottomLeftRadiusX(radius: Single): HResult; stdcall;

    // Animates the value of the x radius of the ellipse that rounds the
    // bottom-left corner of the clip.
    function _SetBottomLeftRadiusX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the y radius of the ellipse that rounds the
    // bottom-left corner of the clip.
    function SetBottomLeftRadiusY(radius: Single): HResult; stdcall;

    // Animates the value of the y radius of the ellipse that rounds the
    // bottom-left corner of the clip.
    function _SetBottomLeftRadiusY(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the x radius of the ellipse that rounds the
    // bottom-right corner of the clip.
    function SetBottomRightRadiusX(radius: Single): HResult; stdcall;

    // Animates the value of the x radius of the ellipse that rounds the
    // bottom-right corner of the clip.
    function _SetBottomRightRadiusX(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes the value of the y radius of the ellipse that rounds the
    // bottom-right corner of the clip.
    function SetBottomRightRadiusY(radius: Single): HResult; stdcall;

    // Animates the value of the y radius of the ellipse that rounds the
    // bottom-right corner of the clip.
    function _SetBottomRightRadiusY(animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionRectangleClip = IDCompositionRectangleClip;
  {$EXTERNALSYM IID_IDCompositionRectangleClip}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionSurface
  //
  // Synopsis:
  //     An IDCompositionSurface interface represents a wrapper around a DirectX
  //     object, or a sub-rectangle of one of those objects.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionSurface);'}
  {$EXTERNALSYM IDCompositionSurface}
  IDCompositionSurface = interface(IUnknown)
  ['{BB8A4953-2C99-4F5A-96F5-4819027FA3AC}']

    function BeginDraw({opt} updateRect: PRect;
                       const iid: REFIID;
                       updateObject: Pointer;
                       out updateOffset: TPoint): HResult; stdcall;

    function EndDraw(): HResult; stdcall;

    function SuspendDraw(): HResult; stdcall;

    function ResumeDraw(): HResult; stdcall;

    function Scroll({opt} scrollRect: PRect;
                    {opt} clipRect: PRect;
                    offsetX: Integer;
                    offsetY: Integer): HResult; stdcall;

  end;
  IID_IDCompositionSurface = IDCompositionSurface;
  {$EXTERNALSYM IID_IDCompositionSurface}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionVirtualSurface
  //
  // Synopsis:
  //     An IDCompositionVirtualSurface interface represents a sparsely
  //     allocated surface.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionVirtualSurface);'}
  {$EXTERNALSYM IDCompositionVirtualSurface}
  IDCompositionVirtualSurface = interface(IDCompositionSurface)
  ['{AE471C51-5F53-4A24-8D3E-D0C39C30B3F0}']

    function Resize(_width: UINT;
                    _height: UINT): HResult; stdcall;

    function Trim(rectangles: PRECT;
                  count: UINT): HResult; stdcall;

  end;
  IID_IDCompositionVirtualSurface = IDCompositionVirtualSurface;
  {$EXTERNALSYM IID_IDCompositionVirtualSurface}

// #if (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)

  IDCompositionDevice2 = interface;
  PIDCompositionDevice2 = ^IDCompositionDevice2;

  IDCompositionDeviceDebug = interface;
  PIDCompositionDeviceDebug = ^IDCompositionDeviceDebug;

  IDCompositionDesktopDevice = interface;
  PIDCompositionDesktopDevice = ^IDCompositionDesktopDevice;

  IDCompositionVisual2 = interface;
  PIDCompositionVisual2 = ^IDCompositionVisual2;

  IDCompositionVisualDebug = interface;
  PIDCompositionVisualDebug = ^IDCompositionVisualDebug;

  IDCompositionSurfaceFactory = interface;
  PIDCompositionSurfaceFactory = ^IDCompositionSurfaceFactory;


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionDevice2
  //
  // Synopsis:
  //     Serves as the root factory for all other DirectComposition2 objects and
  //     controls transactional composition.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionDevice2);'}
  {$EXTERNALSYM IDCompositionDevice2}
  IDCompositionDevice2 = interface(IUnknown)
  ['{75F6468D-1B8E-447C-9BC6-75FEA80B5B25}']

    // Commits all DirectComposition commands pending on this device.
    function Commit(): HResult; stdcall;

    // Waits for the last Commit to be processed by the composition engine
    function WaitForCommitCompletion(): HResult; stdcall;

    // Gets timing information about the composition engine.
    function GetFrameStatistics(out statistics: DCOMPOSITION_FRAME_STATISTICS): HResult; stdcall;

    // Creates a new visual object.
    function CreateVisual(out visual: IDCompositionVisual2): HResult; stdcall;

    // Creates a factory for surface objects
    function CreateSurfaceFactory(renderingDevice: IUnknown;
                                  out surfaceFactory: IDCompositionSurfaceFactory): HResult; stdcall;

    // Creates a DirectComposition surface object
    function CreateSurface(_width: UINT;
                           _height: UINT;
                           pixelFormat: DXGI_FORMAT;
                           alphaMode: DXGI_ALPHA_MODE;
                           out surface: IDCompositionSurface): HResult; stdcall;

    // Creates a DirectComposition virtual surface object
    function CreateVirtualSurface(initialWidth: UINT;
                                  initialHeight: UINT;
                                  pixelFormat: DXGI_FORMAT;
                                  alphaMode: DXGI_ALPHA_MODE;
                                  out virtualSurface: IDCompositionVirtualSurface): HResult; stdcall;

    // Creates a 2D translation transform object.
    function CreateTranslateTransform(out translateTransform: IDCompositionTranslateTransform): HResult; stdcall;

    // Creates a 2D scale transform object.
    function CreateScaleTransform(out scaleTransform: IDCompositionScaleTransform): HResult; stdcall;

    // Creates a 2D rotation transform object.
    function CreateRotateTransform(out rotateTransform: IDCompositionRotateTransform): HResult; stdcall;

    // Creates a 2D skew transform object.
    function CreateSkewTransform(out skewTransform: IDCompositionSkewTransform): HResult; stdcall;

    // Creates a 2D 3x2 matrix transform object.
    function CreateMatrixTransform(out matrixTransform: IDCompositionMatrixTransform): HResult; stdcall;

    // Creates a 2D transform object that holds an array of 2D transform objects.
    function CreateTransformGroup(transforms: IDCompositionTransform;
                                  elements: UINT;
                                  out transformGroup: IDCompositionTransform): HResult; stdcall;

    // Creates a 3D translation transform object.
    function CreateTranslateTransform3D(out translateTransform3D: IDCompositionTranslateTransform3D): HResult; stdcall;

    // Creates a 3D scale transform object.
    function CreateScaleTransform3D(out scaleTransform3D: IDCompositionScaleTransform3D): HResult; stdcall;

    // Creates a 3D rotation transform object.
    function CreateRotateTransform3D(out rotateTransform3D: IDCompositionRotateTransform3D): HResult; stdcall;

    // Creates a 3D 4x4 matrix transform object.
    function CreateMatrixTransform3D(out matrixTransform3D: IDCompositionMatrixTransform3D): HResult; stdcall;

    // Creates a 3D transform object that holds an array of 3D transform objects.
    function CreateTransform3DGroup(transforms3D: IDCompositionTransform3D;
                                    elements: UINT;
                                    out transform3DGroup: IDCompositionTransform3D): HResult; stdcall;

    // Creates an effect group
    function CreateEffectGroup(out effectGroup: IDCompositionEffectGroup): HResult; stdcall;

    // Creates a clip object that can be used to clip the contents of a visual subtree.
    function CreateRectangleClip(out clip: IDCompositionRectangleClip): HResult; stdcall;

    // Creates an animation object
    function CreateAnimation(out animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionDevice2 = IDCompositionDevice2;
  {$EXTERNALSYM IID_IDCompositionDevice2}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionDesktopDevice
  //
  // Synopsis:
  //     Serves as the root factory for all other desktop DirectComposition
  //     objects.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionDesktopDevice);'}
  {$EXTERNALSYM IDCompositionDesktopDevice}
  IDCompositionDesktopDevice = interface(IDCompositionDevice2)
  ['{5F4633FE-1E08-4CB8-8C75-CE24333F5602}']

    function CreateTargetForHwnd(_hwnd: HWND;
                                 topmost: BOOL;
                                 out target: IDCompositionTarget): HResult; stdcall;

    // Creates a surface wrapper around a pre-existing surface that can be associated with one or more visuals for composition.
    function CreateSurfaceFromHandle(_handle: THandle;
                                     out surface: IUnknown): HResult; stdcall;

    // Creates a wrapper object that represents the rasterization of a layered window and which can be associated with a visual for composition.
    function CreateSurfaceFromHwnd(_hwnd: HWND;
                                   out surface: IUnknown): HResult; stdcall;

  end;
  IID_IDCompositionDesktopDevice = IDCompositionDesktopDevice;
  {$EXTERNALSYM IID_IDCompositionDesktopDevice}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionDeviceDebug
  //
  // Synopsis:
  //     IDCompositionDeviceDebug serves as a debug interface
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionDeviceDebug);'}
  {$EXTERNALSYM IDCompositionDeviceDebug}
  IDCompositionDeviceDebug = interface(IUnknown)
  ['{A1A3C64A-224F-4A81-9773-4F03A89D3C6C}']

    // Enables debug counters
    function EnableDebugCounters(): HResult; stdcall;

    // Enables debug counters
    function DisableDebugCounters(): HResult; stdcall;

  end;
  IID_IDCompositionDeviceDebug = IDCompositionDeviceDebug;
  {$EXTERNALSYM IID_IDCompositionDeviceDebug}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionSurfaceFactory
  //
  // Synopsis:
  //     An IDCompositionSurfaceFactory interface represents an object that can
  //     create surfaces suitable for composition.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionSurfaceFactory);'}
  {$EXTERNALSYM IDCompositionSurfaceFactory}
  IDCompositionSurfaceFactory = interface(IUnknown)
  ['{E334BC12-3937-4E02-85EB-FCF4EB30D2C8}']

    // Creates a DirectComposition surface object
    function CreateSurface(_width: UINT;
                           _height: UINT;
                           pixelFormat: DXGI_FORMAT;
                           alphaMode: DXGI_ALPHA_MODE;
                           out surface: IDCompositionSurface): HResult; stdcall;

    // Creates a DirectComposition virtual surface object
    function CreateVirtualSurface(initialWidth: UINT;
                                  initialHeight: UINT;
                                  pixelFormat: DXGI_FORMAT;
                                  alphaMode: DXGI_ALPHA_MODE;
                                  out virtualSurface: IDCompositionVirtualSurface): HResult; stdcall;

  end;
  IID_IDCompositionSurfaceFactory = IDCompositionSurfaceFactory;
  {$EXTERNALSYM IID_IDCompositionSurfaceFactory}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionVisual2
  //
  // Synopsis:
  //     An IDCompositionVisual2 interface represents a visual that participates in
  //     a visual tree.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionVisual2);'}
  {$EXTERNALSYM IDCompositionVisual2}
  IDCompositionVisual2 = interface(IDCompositionVisual)
  ['{E8DE1639-4331-4B26-BC5F-6A321D347A85}']
    // Changes the interpretation of the opacity property of an effect group
    // associated with this visual
    function SetOpacityMode(mode: DCOMPOSITION_OPACITY_MODE): HResult; stdcall;

    // Sets back face visibility
    function SetBackFaceVisibility(visibility: DCOMPOSITION_BACKFACE_VISIBILITY): HResult; stdcall;
  end;
  IID_IDCompositionVisual2 = IDCompositionVisual2;
  {$EXTERNALSYM IID_IDCompositionVisual2}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionVisualDebug
  //
  // Synopsis:
  //     An IDCompositionVisualDebug interface represents a debug visual
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionVisualDebug);'}
  {$EXTERNALSYM IDCompositionVisualDebug}
  IDCompositionVisualDebug = interface(IDCompositionVisual2)
  ['{FED2B808-5EB4-43A0-AEA3-35F65280F91B}']

    // Enable heat map
    function EnableHeatMap(color: D2D1_COLOR_F): HResult; stdcall;

    // Disable heat map
    function DisableHeatMap(): HResult; stdcall;

    // Enable redraw regions
    function EnableRedrawRegions(): HResult; stdcall;

    // Disable redraw regions
    function DisableRedrawRegions(): HResult; stdcall;

  end;
  IID_IDCompositionVisualDebug = IDCompositionVisualDebug;
  {$EXTERNALSYM IID_IDCompositionVisualDebug}

// #endif  //(_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)

// #if (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)

  IDCompositionDevice3 = interface;
  PIDCompositionDevice3 = ^IDCompositionDevice3;

  IDCompositionVisual3 = interface;
  PIDCompositionVisual3 = ^IDCompositionVisual3;


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionVisual3
  //
  // Synopsis:
  //     An IDCompositionVisual3 interface represents a visual that participates in
  //     a visual tree.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionVisual3);'}
  {$EXTERNALSYM IDCompositionVisual3}
  IDCompositionVisual3 = interface(IDCompositionVisualDebug)
  ['{2775F462-B6C1-4015-B0BE-B3E7D6A4976D}']

    // Sets depth mode property associated with this visual
    function SetDepthMode(mode: DCOMPOSITION_DEPTH_MODE): HResult; stdcall;

    // Changes the value of OffsetZ property.
    function SetOffsetZ(offsetZ: Single): HResult; overload; stdcall;

    // Animates the value of the OffsetZ property.
    function SetOffsetZ(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the value of the Opacity property.
    function SetOpacity(opacity: Single): HResult; overload; stdcall;

    // Animates the value of the Opacity property.
    function SetOpacity(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Sets the matrix that modifies the coordinate system of this visual.
    function SetTransform(matrix: D2D_MATRIX_4X4_F): HResult; overload; stdcall;

    // Sets the transformation object that modifies the coordinate system of this visual.
    function SetTransform({opt} transform: IDCompositionTransform3D = Nil): HResult; overload; stdcall;

    // Changes the value of the Visible property
    function SetVisible(visible: BOOL): HResult; stdcall;

  end;
  IID_IDCompositionVisual3 = IDCompositionVisual3;
  {$EXTERNALSYM IID_IDCompositionVisual3}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionDevice3
  //
  // Synopsis:
  //     Serves as the root factory for all other DirectComposition3 objects and
  //     controls transactional composition.
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionDevice3);'}
  {$EXTERNALSYM IDCompositionDevice3}
  IDCompositionDevice3 = interface(IDCompositionDevice2)
  ['{0987CB06-F916-48BF-8D35-CE7641781BD9}']

    // Effect creation calls, each creates an interface around a D2D1Effect
    function CreateGaussianBlurEffect(out gaussianBlurEffect: IDCompositionGaussianBlurEffect): HResult; stdcall;

    function CreateBrightnessEffect(out brightnessEffect: IDCompositionBrightnessEffect): HResult; stdcall;

    function CreateColorMatrixEffect(out colorMatrixEffect: IDCompositionColorMatrixEffect): HResult; stdcall;

    function CreateShadowEffect(out shadowEffect: IDCompositionShadowEffect): HResult; stdcall;

    function CreateHueRotationEffect(out hueRotationEffect: IDCompositionHueRotationEffect): HResult; stdcall;

    function CreateSaturationEffect(out saturationEffect: IDCompositionSaturationEffect): HResult; stdcall;

    function CreateTurbulenceEffect(out turbulenceEffect: IDCompositionTurbulenceEffect): HResult; stdcall;

    function CreateLinearTransferEffect(out linearTransferEffect: IDCompositionLinearTransferEffect): HResult; stdcall;

    function CreateTableTransferEffect(out tableTransferEffect: IDCompositionTableTransferEffect): HResult; stdcall;

    function CreateCompositeEffect(out compositeEffect: IDCompositionCompositeEffect): HResult; stdcall;

    function CreateBlendEffect(out blendEffect: IDCompositionBlendEffect): HResult; stdcall;

    function CreateArithmeticCompositeEffect(out arithmeticCompositeEffect: IDCompositionArithmeticCompositeEffect): HResult; stdcall;

    function CreateAffineTransform2DEffect(out affineTransform2dEffect: IDCompositionAffineTransform2DEffect): HResult; stdcall;

  end;
  IID_IDCompositionDevice3 = IDCompositionDevice3;
  {$EXTERNALSYM IID_IDCompositionDevice3}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionFilterEffect
  //
  // Synopsis:
  //     An IDCompositionFilterEffect interface represents a filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionFilterEffect);'}
  {$EXTERNALSYM IDCompositionFilterEffect}
  IDCompositionFilterEffect = interface(IDCompositionEffect)
  ['{30C421D5-8CB2-4E9F-B133-37BE270D4AC2}']

    // Sets the input at the given index to the filterEffect (Nil will use source visual, unless flagged otherwise)
    function SetInput(index: UINT;
                      {opt} input: IUnknown;
                      flags: UINT): HResult; stdcall;

  end;
  IID_IDCompositionFilterEffect = IDCompositionFilterEffect;
  {$EXTERNALSYM IID_IDCompositionFilterEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionGaussianBlurEffect
  //
  // Synopsis:
  //     An IDCompositionGaussianBlurEffect interface represents a gaussian blur filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionGaussianBlurEffect);'}
  {$EXTERNALSYM IDCompositionGaussianBlurEffect}
  IDCompositionGaussianBlurEffect = interface(IDCompositionFilterEffect)
  ['{45D4D0B7-1BD4-454E-8894-2BFA68443033}']

    //Changes the amount of blur to be applied.
    function SetStandardDeviation(amount: Single): HResult; overload; stdcall;

    function SetStandardDeviation(animation: IDCompositionAnimation): HResult; overload; stdcall;

    //Changes border mode (see D2D1_GAUSSIANBLUR)
    function SetBorderMode(mode: D2D1_BORDER_MODE): HResult; stdcall;

  end;
  IID_IDCompositionGaussianBlurEffect = IDCompositionGaussianBlurEffect;
  {$EXTERNALSYM IID_IDCompositionGaussianBlurEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionBrightnessEffect
  //
  // Synopsis:
  //     An IDCompositionBrightnessEffect interface represents a brightness filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionBrightnessEffect);'}
  {$EXTERNALSYM IDCompositionBrightnessEffect}
  IDCompositionBrightnessEffect = interface(IDCompositionFilterEffect)
  ['{6027496E-CB3A-49AB-934F-D798DA4F7DA6}']

    // Changes the value of white point property.
    function SetWhitePoint(whitePoint: D2D1_VECTOR_2F): HResult; stdcall;

    // Changes the value of black point property
    function SetBlackPoint(blackPoint: D2D1_VECTOR_2F): HResult; stdcall;

    // Changes the X value of the white point property.
    function SetWhitePointX(whitePointX: Single): HResult; overload; stdcall;

    function SetWhitePointX(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the Y value of the white point property.
    function SetWhitePointY(whitePointY: Single): HResult; overload; stdcall;

    function SetWhitePointY(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the X value of the black point property.
    function SetBlackPointX(blackPointX: Single): HResult; overload; stdcall;

    function SetBlackPointX(animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the Y value of the black point property.
    function SetBlackPointY(blackPointY: Single): HResult; overload; stdcall;

    function SetBlackPointY(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionBrightnessEffect = IDCompositionBrightnessEffect;
  {$EXTERNALSYM IID_IDCompositionBrightnessEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionColorMatrixEffect
  //
  // Synopsis:
  //     An IDCompositionColorMatrixEffect interface represents a color matrix filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionColorMatrixEffect);'}
  {$EXTERNALSYM IDCompositionColorMatrixEffect}
  IDCompositionColorMatrixEffect = interface(IDCompositionFilterEffect)
  ['{C1170A22-3CE2-4966-90D4-55408BFC84C4}']

    // Changes all values of the matrix for a color transform
    function SetMatrix(matrix: D2D1_MATRIX_5X4_F): HResult; stdcall;

    // Changes a Single element of the matrix of this color transform.
    function SetMatrixElement(row: Integer;
                              column: Integer;
                              value: Single): HResult; overload; stdcall;

    // Animates a single element of the matrix of this color transform.
    function SetMatrixElement(row: Integer;
                               column: Integer;
                               animation: IDCompositionAnimation): HResult; overload; stdcall;

    // Changes the alpha mode
    function SetAlphaMode(mode: D2D1_COLORMATRIX_ALPHA_MODE): HResult; stdcall;

    // Sets the clamp output property
    function SetClampOutput(clamp: BOOL): HResult; stdcall;

  end;
  IID_IDCompositionColorMatrixEffect = IDCompositionColorMatrixEffect;
  {$EXTERNALSYM IID_IDCompositionColorMatrixEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionShadowEffect
  //
  // Synopsis:
  //     An IDCompositionShadowEffect interface represents a shadow filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionShadowEffect);'}
  {$EXTERNALSYM IDCompositionShadowEffect}
  IDCompositionShadowEffect = interface(IDCompositionFilterEffect)
  ['{4AD18AC0-CFD2-4C2F-BB62-96E54FDB6879}']

    // Changes the amount of blur to be applied.
    function SetStandardDeviation(amount: Single): HResult; stdcall;

    function _SetStandardDeviation(animation: IDCompositionAnimation): HResult; stdcall;

    // Changes shadow color
    function SetColor(color: D2D1_VECTOR_4F): HResult; stdcall;

    function SetRed(amount: Single): HResult; overload; stdcall;

    function SetRed(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetGreen(amount: Single): HResult; overload; stdcall;

    function SetGreen(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetBlue(amount: Single): HResult; overload; stdcall;

    function SetBlue(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetAlpha(amount: Single): HResult; stdcall;


    function _SetAlpha(animation: IDCompositionAnimation): HResult; stdcall;

  end;
  IID_IDCompositionShadowEffect = IDCompositionShadowEffect;
  {$EXTERNALSYM IID_IDCompositionShadowEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionHueRotationEffect
  //
  // Synopsis:
  //     An IDCompositionHueRotationEffect interface represents a hue rotation filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionHueRotationEffect);'}
  {$EXTERNALSYM IDCompositionHueRotationEffect}
  IDCompositionHueRotationEffect = interface(IDCompositionFilterEffect)
  ['{6DB9F920-0770-4781-B0C6-381912F9D167}']

    // Changes the angle of rotation
    function SetAngle(amountDegrees: Single): HResult; overload; stdcall;

    function SetAngle(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionHueRotationEffect = IDCompositionHueRotationEffect;
  {$EXTERNALSYM IID_IDCompositionHueRotationEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionSaturationEffect
  //
  // Synopsis:
  //     An IDCompositionSaturationEffect interface represents a saturation filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionSaturationEffect);'}
  {$EXTERNALSYM IDCompositionSaturationEffect}
  IDCompositionSaturationEffect = interface(IDCompositionFilterEffect)
  ['{A08DEBDA-3258-4FA4-9F16-9174D3FE93B1}']

    // Changes the amount of saturation to be applied.
    function SetSaturation(ratio: Single): HResult; overload; stdcall;

    function SetSaturation(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionSaturationEffect = IDCompositionSaturationEffect;
  {$EXTERNALSYM IID_IDCompositionSaturationEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionTurbulenceEffect
  //
  // Synopsis:
  //     An IDCompositionTurbulenceEffect interface represents a turbulence filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTurbulenceEffect);'}
  {$EXTERNALSYM IDCompositionTurbulenceEffect}
  IDCompositionTurbulenceEffect = interface(IDCompositionFilterEffect)
  ['{A6A55BDA-C09C-49F3-9193-A41922C89715}']

    // Changes the starting offset of the turbulence
    function SetOffset(offset: D2D1_VECTOR_2F): HResult; stdcall;

    // Changes the base frequency of the turbulence
    function SetBaseFrequency(frequency: D2D1_VECTOR_2F): HResult; stdcall;

    // Changes the output size of the turbulence
    function SetSize(size: D2D1_VECTOR_2F): HResult; stdcall;

    // Sets the number of octaves
    function SetNumOctaves(numOctaves: UINT): HResult; stdcall;

    // Set the random number seed
    function SetSeed(seed: UINT): HResult; stdcall;

    // Set the noise mode
    function SetNoise(noise: D2D1_TURBULENCE_NOISE): HResult; stdcall;

    // Set stitchable
    function SetStitchable(stitchable: BOOL): HResult; stdcall;

  end;
  IID_IDCompositionTurbulenceEffect = IDCompositionTurbulenceEffect;
  {$EXTERNALSYM IID_IDCompositionTurbulenceEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionLinearTransferEffect
  //
  // Synopsis:
  //     An IDCompositionLinearTransferEffect interface represents a linear transfer filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionLinearTransferEffect);'}
  {$EXTERNALSYM IDCompositionLinearTransferEffect}
  IDCompositionLinearTransferEffect = interface(IDCompositionFilterEffect)
  ['{4305EE5B-C4A0-4C88-9385-67124E017683}']

    function SetRedYIntercept(redYIntercept: Single): HResult; overload; stdcall;

    function SetRedYIntercept(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetRedSlope(redSlope: Single): HResult; overload; stdcall;

    function SetRedSlope(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetRedDisable(redDisable: BOOL): HResult; stdcall;

    function SetGreenYIntercept(greenYIntercept: Single): HResult; overload; stdcall;

    function SetGreenYIntercept(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetGreenSlope(greenSlope: Single): HResult; overload; stdcall;

    function SetGreenSlope(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetGreenDisable(greenDisable: BOOL): HResult; stdcall;

    function SetBlueYIntercept(blueYIntercept: Single): HResult; overload; stdcall;

    function SetBlueYIntercept(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetBlueSlope(blueSlope: Single): HResult; overload; stdcall;

    function SetBlueSlope(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetBlueDisable(blueDisable: BOOL): HResult; stdcall;

    function SetAlphaYIntercept(alphaYIntercept: Single): HResult; overload; stdcall;

    function SetAlphaYIntercept(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetAlphaSlope(alphaSlope: Single): HResult; overload; stdcall;

    function SetAlphaSlope(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetAlphaDisable(alphaDisable: BOOL): HResult; stdcall;

    function SetClampOutput(clampOutput: BOOL): HResult; stdcall;

  end;
  IID_IDCompositionLinearTransferEffect = IDCompositionLinearTransferEffect;
  {$EXTERNALSYM IID_IDCompositionLinearTransferEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionTableTransferEffect
  //
  // Synopsis:
  //     An IDCompositionTableTransferEffect interface represents a Table transfer filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionTableTransferEffect);'}
  {$EXTERNALSYM IDCompositionTableTransferEffect}
  IDCompositionTableTransferEffect = interface(IDCompositionFilterEffect)
  ['{9B7E82E2-69C5-4EB4-A5F5-A7033F5132CD}']

    function SetRedTable(tableValues: Single;
                         count: UINT): HResult; stdcall;

    function SetGreenTable(tableValues: Single;
                           count: UINT): HResult; stdcall;

    function SetBlueTable(tableValues: Single;
                          count: UINT): HResult; stdcall;

    function SetAlphaTable(tableValues: Single;
                           count: UINT): HResult; stdcall;

    function SetRedDisable(redDisable: BOOL): HResult; stdcall;

    function SetGreenDisable(greenDisable: BOOL): HResult; stdcall;

    function SetBlueDisable(blueDisable: BOOL): HResult; stdcall;

    function SetAlphaDisable(alphaDisable: BOOL): HResult; stdcall;

    function SetClampOutput(clampOutput: BOOL): HResult; stdcall;

    //Note:  To set individual values, the table must have already been initialized
    //       with a buffer of values of the appropriate size, or these calls will fail
    function SetRedTableValue(index: UINT;
                              value: Single): HResult; overload; stdcall;

    function SetRedTableValue(index: UINT;
                              animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetGreenTableValue(index: UINT;
                                value: Single): HResult; overload; stdcall;

    function SetGreenTableValue(index: UINT;
                                animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetBlueTableValue(index: UINT;
                               value: Single): HResult; overload; stdcall;

    function SetBlueTableValue(index: UINT;
                               animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetAlphaTableValue(index: UINT;
                                value: Single): HResult; overload; stdcall;

    function SetAlphaTableValue(index: UINT;
                                animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionTableTransferEffect = IDCompositionTableTransferEffect;
  {$EXTERNALSYM IID_IDCompositionTableTransferEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionCompositeEffect
  //
  // Synopsis:
  //     An IDCompositionCompositeEffect interface represents a composite filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionCompositeEffect);'}
  {$EXTERNALSYM IDCompositionCompositeEffect}
  IDCompositionCompositeEffect = interface(IDCompositionFilterEffect)
  ['{576616C0-A231-494D-A38D-00FD5EC4DB46}']

    //Changes the composite mode.
    function SetMode(mode: D2D1_COMPOSITE_MODE): HResult; stdcall;

  end;
  IID_IDCompositionCompositeEffect = IDCompositionCompositeEffect;
  {$EXTERNALSYM IID_IDCompositionCompositeEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionBlendEffect
  //
  // Synopsis:
  //     An IDCompositionBlendEffect interface represents a blend filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionBlendEffect);'}
  {$EXTERNALSYM IDCompositionBlendEffect}
  IDCompositionBlendEffect = interface(IDCompositionFilterEffect)
  ['{33ECDC0A-578A-4A11-9C14-0CB90517F9C5}']

    function SetMode(mode: D2D1_BLEND_MODE): HResult; stdcall;

  end;
  IID_IDCompositionBlendEffect = IDCompositionBlendEffect;
  {$EXTERNALSYM IID_IDCompositionBlendEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionArithmeticCompositeEffect
  //
  // Synopsis:
  //     An IDCompositionArithmeticCompositeEffect interface represents an arithmetic composite filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionArithmeticCompositeEffect);'}
  {$EXTERNALSYM IDCompositionArithmeticCompositeEffect}
  IDCompositionArithmeticCompositeEffect = interface(IDCompositionFilterEffect)
  ['{3B67DFA8-E3DD-4E61-B640-46C2F3D739DC}']

    function SetCoefficients(coefficients: D2D1_VECTOR_4F): HResult; stdcall;

    function SetClampOutput(clampoutput: BOOL): HResult; stdcall;

    function SetCoefficient1(Coeffcient1: Single): HResult; overload; stdcall;

    function SetCoefficient1(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetCoefficient2(Coefficient2: Single): HResult; overload; stdcall;

    function SetCoefficient2(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetCoefficient3(Coefficient3: Single): HResult; overload; stdcall;

    function SetCoefficient3(animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetCoefficient4(Coefficient4: Single): HResult; overload; stdcall;

    function SetCoefficient4(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionArithmeticCompositeEffect = IDCompositionArithmeticCompositeEffect;
  {$EXTERNALSYM IID_IDCompositionArithmeticCompositeEffect}


  //+-----------------------------------------------------------------------------
  //
  // Interface:
  //     IDCompositionAffineTransform2DEffect
  //
  // Synopsis:
  //     An IDCompositionAffineTransform2DEffect interface represents a affine transform 2D filter effect
  //
  //------------------------------------------------------------------------------
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionAffineTransform2DEffect);'}
  {$EXTERNALSYM IDCompositionAffineTransform2DEffect}
  IDCompositionAffineTransform2DEffect = interface(IDCompositionFilterEffect)
  ['{0B74B9E8-CDD6-492F-BBBC-5ED32157026D}']

    function SetInterpolationMode(interpolationMode: D2D1_2DAFFINETRANSFORM_INTERPOLATION_MODE): HResult; stdcall;

    function SetBorderMode(borderMode: D2D1_BORDER_MODE): HResult; stdcall;

    function SetTransformMatrix(transformMatrix: D2D1_MATRIX_3X2_F): HResult; stdcall;

    function SetTransformMatrixElement(row: Integer;
                                       column: Integer;
                                       value: Single): HResult; overload; stdcall;

    function SetTransformMatrixElement(row: Integer;
                                       column: Integer;
                                       animation: IDCompositionAnimation): HResult; overload; stdcall;

    function SetSharpness(sharpness: Single): HResult; overload; stdcall;

    function SetSharpness(animation: IDCompositionAnimation): HResult; overload; stdcall;

  end;
  IID_IDCompositionAffineTransform2DEffect = IDCompositionAffineTransform2DEffect;
  {$EXTERNALSYM IID_IDCompositionAffineTransform2DEffect}

// #endif  //(_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)



  // FUNCTIONS /////////////////////////////////////////////////////////////////

  //+---------------------------------------------------------------------------
  //
  // Function:
  //     DCompositionCreateDevice
  //
  // Synopsis:
  //     Creates a new DirectComposition device object, which can be used to create
  //     other DirectComposition objects.
  //
  //----------------------------------------------------------------------------
  function DCompositionCreateDevice({In_opt} dxgiDevice: IDXGIDevice;
                                    const iid: REFIID;
                                    {out} dcompositionDevice: pointer ): HResult; stdcall;
  {$EXTERNALSYM DCompositionCreateDevice}

//#if (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)

  //+---------------------------------------------------------------------------
  //
  // Function:
  //     DCompositionCreateDevice2
  //
  // Synopsis:
  //     Creates a new DirectComposition device object, which can be used to create
  //     other DirectComposition objects.
  //
  //----------------------------------------------------------------------------
  function DCompositionCreateDevice2({In_opt} renderingDevice: IUnknown;
                                     const iid: REFIID;
                                     {out} dcompositionDevice: Pointer ): HResult; stdcall;
  {$EXTERNALSYM DCompositionCreateDevice2}

//#endif  //(_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)

//#if (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)

  //+---------------------------------------------------------------------------
  //
  // Function:
  //     DCompositionCreateDevice3
  //
  // Synopsis:
  //     Creates a new DirectComposition device object, which can be used to create
  //     other DirectComposition objects.
  //
  //----------------------------------------------------------------------------
  function DCompositionCreateDevice3({In_opt} renderingDevice: IUnknown;
                                     const iid: REFIID;
                                     {out} dcompositionDevice: Pointer): HResult; stdcall;
  {$EXTERNALSYM DCompositionCreateDevice3}

//  #endif  //(_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)


  //+---------------------------------------------------------------------------
  //
  // Function:
  //     DCompositionCreateSurfaceHandle
  //
  // Synopsis:
  //     Creates a new composition surface object, which can be bound to a
  //     DirectX swap chain or swap buffer or to a GDI bitmap and associated
  //     with a visual.
  //
  //----------------------------------------------------------------------------
  function DCompositionCreateSurfaceHandle(desiredAccess: DWORD;
                                           {In_opt} securityAttributes: SECURITY_ATTRIBUTES;
                                           out surfaceHandle: THandle): HResult; stdcall;
  {$EXTERNALSYM DCompositionCreateSurfaceHandle}

  //+---------------------------------------------------------------------------
  //
  // Function:
  //     DCompositionAttachMouseWheelToHwnd
  //
  // Synopsis:
  //     Creates an Interaction/InputSink to route mouse wheel messages to the
  //     given HWND. After calling this API, the device owning the visual must
  //     be committed.
  //
  //----------------------------------------------------------------------------
  function DCompositionAttachMouseWheelToHwnd(visual: IDCompositionVisual;
                                              _hwnd: HWND;
                                              enable: BOOL): HResult; stdcall;
  {$EXTERNALSYM DCompositionAttachMouseWheelToHwnd}

  //+---------------------------------------------------------------------------
  //
  // Function:
  //     DCompositionAttachMouseDragToHwnd
  //
  // Synopsis:
  //     Creates an Interaction/InputSink to route mouse button down and any
  //     subsequent move and up events to the given HWND. There is no move
  //     thresholding; when enabled, all events including and following the down
  //     are unconditionally redirected to the specified window. After calling this
  //     API, the device owning the visual must be committed.
  //
  //----------------------------------------------------------------------------
  function DCompositionAttachMouseDragToHwnd(visual: IDCompositionVisual;
                                             _hwnd: HWND;
                                             enable: LongBool): HResult; stdcall;
  {$EXTERNALSYM DCompositionAttachMouseDragToHwnd}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  DCompLib                = 'dcomp.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function DCompositionCreateDevice;           external DCompLib name 'DCompositionCreateDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function DCompositionCreateDevice2;          external DCompLib name 'DCompositionCreateDevice2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function DCompositionCreateDevice3;          external DCompLib name 'DCompositionCreateDevice3' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function DCompositionCreateSurfaceHandle;    external DCompLib name 'DCompositionCreateSurfaceHandle' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function DCompositionAttachMouseWheelToHwnd; external DCompLib name 'DCompositionAttachMouseWheelToHwnd' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function DCompositionAttachMouseDragToHwnd;  external DCompLib name 'DCompositionAttachMouseDragToHwnd' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.

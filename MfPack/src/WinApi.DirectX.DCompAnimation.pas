// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DCompAnimation.pas
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
// Source: dcompanimation.h
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
unit WinApi.DirectX.DCompAnimation;

interface

uses
  {WinApi}
   WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}


//+-----------------------------------------------------------------------------
//
//  Interface:
//      IDCompositionAnimation
//
//  Synopsis:
//      An IDCompositionAnimation interface represents a one-dimensional function that
//      can be used to animate the value of a property of another DirectComposition object.
//
//------------------------------------------------------------------------------

type

  // Interface IDCompositionAnimation
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDCompositionAnimation);'}
  {$EXTERNALSYM IDCompositionAnimation}
  IDCompositionAnimation = interface(IUnknown)
  ['{CBFD91D9-51B2-45e4-B3DE-D19CCFB863C5}']

    // Resets the animation function so that it contains no segments as when it was first created.
    function Reset(): HResult; stdcall;

    // Sets the absolute time at which the animation curve starts.
    function SetAbsoluteBeginTime(beginTime: LARGE_INTEGER): HResult; stdcall;

    // Adds a cubic polynomial segment to the animation function.
    function AddCubic(beginOffset: Double;
                      constantCoefficient: Single;
                      linearCoefficient: Single;
                      quadraticCoefficient: Single;
                      cubicCoefficient: Single): HResult; stdcall;

    // Adds a sinusoidal segment to the animation function.
    function AddSinusoidal(beginOffset: Double;
                           bias: Single;
                           amplitude: Single;
                           frequency: Single;
                           phase: Single): HResult; stdcall;

    // Repeats the animation.
    function AddRepeat(beginOffset: Double;
                       durationToRepeat: Double): HResult; stdcall;

    // Terminates an animation curve.
    function _End(endOffset: Double;
                  endValue: Single): HResult; stdcall;

  end;
  IID_IDCompositionAnimation = IDCompositionAnimation;
  {$EXTERNALSYM IID_IDCompositionAnimation}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

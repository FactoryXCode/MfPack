// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3DX9Math.pas
// Kind: Pascal / Delphi unit
// Release date: 14-01-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: D3DX math types and functions
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//

//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 2004)
//                                #1 Autobahn
// 10/08/2020 All                 #2 => #2b The Model
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
// Source: d3dx9math.h
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
unit WinApi.DirectX.D3DX9Math;

interface

uses
  WinApi.DirectX.D3D9Types;

//===========================================================================
//
// General purpose utilities
//
//===========================================================================

const

  D3DX_PI: Single       = 3.141592654;
  {$EXTERNALSYM D3DX_PI}

  D3DX_1BYPI: Single    = 0.318309886;
  {$EXTERNALSYM D3DX_1BYPI}

function D3DXToRadian(Degree: Single): Single; inline;
{$EXTERNALSYM D3DXToRadian}

function D3DXToDegree(Radian: Single): Single; inline;
{$EXTERNALSYM D3DXToDegree}



//===========================================================================
//
// 16 bit floating point numbers
//
//===========================================================================

const
  D3DX_16F_DIG          = 3;               // # of decimal digits of precision
  {$EXTERNALSYM D3DX_16F_DIG}
  D3DX_16F_EPSILON      = 4.8875809e-4;    // smallest such that 1.0 + epsilon <> 1.0
  {$EXTERNALSYM D3DX_16F_EPSILON}
  D3DX_16F_MANT_DIG     = 11;              // # of bits in mantissa
  {$EXTERNALSYM D3DX_16F_MANT_DIG}
  D3DX_16F_MAX          = 6.550400e+004;   // max value
  {$EXTERNALSYM D3DX_16F_MAX}
  D3DX_16F_MAX_10_EXP   = 4;               // max decimal exponent
  {$EXTERNALSYM D3DX_16F_MAX_10_EXP}
  D3DX_16F_MAX_EXP      = 15;              // max binary exponent
  {$EXTERNALSYM D3DX_16F_MAX_EXP}
  D3DX_16F_MIN          = 6.1035156e-5;    // min positive value
  {$EXTERNALSYM D3DX_16F_MIN}
  D3DX_16F_MIN_10_EXP   = -4;              // min decimal exponent
  {$EXTERNALSYM D3DX_16F_MIN_10_EXP}
  D3DX_16F_MIN_EXP      = -14;             // min binary exponent
  {$EXTERNALSYM D3DX_16F_MIN_EXP}
  D3DX_16F_RADIX        = 2;               // exponent radix
  {$EXTERNALSYM D3DX_16F_RADIX}
  D3DX_16F_ROUNDS       = 1;               // addition rounding: near
  {$EXTERNALSYM D3DX_16F_ROUNDS}


type
  PD3DXFloat16 = ^TD3DXFloat16;
  D3DXFLOAT16 = record
    value: Word;
    class operator Implicit(Value: Single): D3DXFLOAT16; inline;
    class operator Implicit(const Value: D3DXFLOAT16): Single; inline;
    class operator Equal(const Left, Right: D3DXFLOAT16): Boolean; inline;
    class function Zero: D3DXFLOAT16; static; inline;
  end;
  {$EXTERNALSYM D3DXFLOAT16}
  TD3DXFloat16 = D3DXFLOAT16;

// Some pascal equalents of C++ class functions & operators
const D3DXFloat16Zero: D3DXFLOAT16 = (value:0); // 0
//function D3DXFloat16(value: Single): TD3DXFloat16; inline;
function D3DXFloat16Equal(const v1, v2: D3DXFLOAT16): Boolean; inline;
function D3DXFloat16ToFloat(value: D3DXFLOAT16): Single; inline;

//===========================================================================
//
// Vectors
//
//===========================================================================


//--------------------------
// 2D Vector
//--------------------------
type
  PD3DXVector2 = ^TD3DXVector2;
  D3DXVECTOR2 = record
    x, y: Single;
    class function Create(X, Y: Single): D3DXVECTOR2; static; inline;
    class operator Equal(const Left, Right: D3DXVECTOR2): Boolean; inline;
    class function Zero: D3DXVECTOR2; static; inline;
  end;
  {$EXTERNALSYM D3DXVECTOR2}
  TD3DXVector2 = D3DXVECTOR2;

// Some pascal equalents of C++ class functions & operators
const D3DXVector2Zero: D3DXVECTOR2 = (x:0; y:0);  // (0,0)
//function D3DXVector2(_x, _y: Single): TD3DXVector2; inline;
function D3DXVector2Equal(const v1, v2: D3DXVECTOR2): Boolean; inline;

//--------------------------
// 2D Vector (16 bit)
//--------------------------
type
  PD3DXVector2_16F = ^TD3DXVector2_16F;
  {$EXTERNALSYM PD3DXVector2_16F}
  D3DXVECTOR2_16F = record
    x, y: D3DXFLOAT16;
    class function Create(X, Y: D3DXFLOAT16): D3DXVECTOR2_16F; overload; static; inline;
    class function Create(X, Y: Single): D3DXVECTOR2_16F; overload; static; inline;
    class operator Implicit(const Value: D3DXVECTOR2): D3DXVECTOR2_16F; inline;
    class operator Implicit(const Value: D3DXVECTOR2_16F): D3DXVECTOR2; inline;
    class operator Equal(const Left, Right: D3DXVECTOR2_16F): Boolean; inline;
    class function Zero: D3DXVECTOR2_16F; static; inline;
  end;
  {$EXTERNALSYM D3DXVECTOR2_16F}
  TD3DXVector2_16F = D3DXVECTOR2_16F;

// Some pascal equalents of C++ class functions & operators
const D3DXVector2_16fZero: D3DXVECTOR2_16F = (x:(value:0); y:(value:0));  // (0,0)
//function D3DXVector2_16F(_x, _y: TD3DXFloat16): TD3DXVector2_16F; inline;
function D3DXVector2_16fEqual(const v1, v2: D3DXVECTOR2_16F): Boolean; inline;
function D3DXVector2_16fFromVector2(const v: D3DXVECTOR2): D3DXVECTOR2_16F; inline;
function D3DXVector2FromVector2_16f(const v: D3DXVECTOR2_16F): D3DXVECTOR2; inline;

//--------------------------
// 3D Vector
//--------------------------
type
  PD3DXVector3 = ^TD3DXVector3;
  D3DXVECTOR3 = D3DVECTOR;
  {$EXTERNALSYM D3DXVECTOR3}
  TD3DXVector3 = D3DXVECTOR3;

// Some pascal equalents of C++ class functions & operators
const D3DXVector3Zero: D3DXVECTOR3 = (x:0; y:0; z:0);  // (0,0,0)
//function D3DXVector3(_x, _y, _z: Single): TD3DXVector3; inline;
function D3DXVector3Equal(const v1, v2: D3DXVECTOR3): Boolean; inline;

//--------------------------
// 3D Vector (16 bit)
//--------------------------
type
  PD3DXVector3_16F = ^TD3DXVector3_16F;
  {$EXTERNALSYM PD3DXVector3}
  D3DXVECTOR3_16F = record
    x, y, z: D3DXFLOAT16;
    class function Create(const X, Y, Z: D3DXFLOAT16): D3DXVECTOR3_16F; overload; static; inline;
    class function Create(const X, Y, Z: Single): D3DXVECTOR3_16F; overload; static; inline;
    class operator Implicit(const Value: D3DXVECTOR3): D3DXVECTOR3_16F; inline;
    class operator Implicit(const Value: D3DXVECTOR3_16F): D3DXVECTOR3; inline;
    class operator Equal(const Left, Right: D3DXVECTOR3_16F): Boolean; inline;
    class function Zero: D3DXVECTOR3_16F; static; inline;
  end;
  {$EXTERNALSYM D3DXVECTOR3_16F}
  TD3DXVector3_16F = D3DXVECTOR3_16F;

// Some pascal equalents of C++ class functions & operators
const D3DXVector3_16fZero: D3DXVECTOR3_16F = (x:(value:0); y:(value:0); z:(value:0));  // (0,0,0)
//function D3DXVector3_16F(_x, _y, _z: TD3DXFloat16): TD3DXVector3_16F; inline;
function D3DXVector3_16fEqual(const v1, v2: D3DXVECTOR3_16F): Boolean; inline;
function D3DXVector3_16fFromVector3(const v: D3DXVECTOR3): D3DXVECTOR3_16F; inline;
function D3DXVector3FromVector3_16f(const v: D3DXVECTOR3_16F): D3DXVECTOR3; inline;

//--------------------------
// 4D Vector
//--------------------------
type
  PD3DXVector4 = ^TD3DXVector4;
  D3DXVECTOR4 = record
    x, y, z, w: Single;
    class function Create(const XYZ: D3DXVECTOR3; W: Single): D3DXVECTOR4; overload; static; inline;
    class function Create(const X, Y, Z, W: Single): D3DXVECTOR4; overload; static; inline;
    class operator Equal(const Left, Right: D3DXVECTOR4): Boolean; inline;
    class function Zero: D3DXVECTOR4; static; inline;
  end;
  {$EXTERNALSYM D3DXVECTOR4}
  TD3DXVector4 = D3DXVECTOR4;

// Some pascal equalents of C++ class functions & operators
const D3DXVector4Zero: D3DXVECTOR4 = (x:0; y:0; z:0; w:0);  // (0,0,0,0)
//function D3DXVector4(_x, _y, _z, _w: Single): TD3DXVector4; overload; inline;
//function D3DXVector4(xyz: D3DXVector3; _w: Single): TD3DXVector4; overload; inline;
function D3DXVector4Equal(const v1, v2: D3DXVECTOR4): Boolean; inline;

//--------------------------
// 4D Vector (16 bit)
//--------------------------
type
  PD3DXVector4_16F = ^TD3DXVector4_16F;
  D3DXVECTOR4_16F = record
    x, y, z, w: D3DXFLOAT16;
    class function Create(const X, Y, Z, W: D3DXFLOAT16): D3DXVECTOR4_16F; overload; static; inline;
    class function Create(const XYZ: D3DXVECTOR3; W: Single): D3DXVECTOR4_16F; overload; static; inline;
    class function Create(const XYZ: D3DXVECTOR3_16F; W: D3DXFLOAT16): D3DXVECTOR4_16F; overload; static; inline;
    class function Create(const X, Y, Z, W: Single): D3DXVECTOR4_16F; overload; static; inline;
    class operator Implicit(const Value: D3DXVECTOR4): D3DXVECTOR4_16F; inline;
    class operator Implicit(const Value: D3DXVECTOR4_16F): D3DXVECTOR4; inline;
    class operator Equal(const Left, Right: D3DXVECTOR4_16F): Boolean; inline;
    class function Zero: D3DXVECTOR4_16F; static; inline;
  end;
  {$EXTERNALSYM D3DXVECTOR4_16F}
  TD3DXVector4_16F = D3DXVECTOR4_16F;

// Some pascal equalents of C++ class functions & operators
const D3DXVector4_16fZero: D3DXVECTOR4_16F = (x:(value:0); y:(value:0); z:(value:0); w:(value:0));  // (0,0,0,0)
//function D3DXVector4_16F(_x, _y, _z, _w: TD3DXFloat16): TD3DXVector4_16F; overload; inline;
//function D3DXVector4_16F(xyz: TD3DXVector3_16f; _w: TD3DXFloat16): TD3DXVector4_16F; overload; inline;
function D3DXVector4_16fEqual(const v1, v2: D3DXVECTOR4_16F): Boolean; inline;
function D3DXVector4_16fFromVector4(const v: D3DXVECTOR4): D3DXVECTOR4_16F; inline;
function D3DXVector4FromVector4_16f(const v: D3DXVECTOR4_16F): D3DXVECTOR4; inline;

//===========================================================================
//
// Matrices
//
//===========================================================================
type
  PPD3DXMatrix = ^PD3DXMatrix;
  PD3DXMatrix = ^D3DMATRIX;
  D3DXMATRIX = D3DMATRIX;
  {$EXTERNALSYM D3DXMATRIX}
  TD3DXMatrix = D3DMATRIX;

// Some pascal equalents of C++ class functions & operators
//function D3DXMatrix(
//  _m00, _m01, _m02, _m03,
//  _m10, _m11, _m12, _m13,
//  _m20, _m21, _m22, _m23,
//  _m30, _m31, _m32, _m33: Single): TD3DXMatrix; inline;
function D3DXMatrixAdd(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix; inline;
function D3DXMatrixSubtract(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix; inline;
function D3DXMatrixMul(out mOut: TD3DXMatrix; const m: TD3DXMatrix; MulBy: Single): PD3DXMatrix; inline;
function D3DXMatrixEqual(const m1, m2: TD3DXMatrix): Boolean; inline;


//---------------------------------------------------------------------------
// Aligned Matrices
//
// This class helps keep matrices 16-byte aligned as preferred by P4 cpus.
// It aligns matrices on the stack and on the heap or in global scope.
// It does this using __declspec(align(16)) which works on VC7 and on VC 6
// with the processor pack. Unfortunately there is no way to detect the
// latter so this is turned on only on VC7. On other compilers this is the
// the same as D3DXMATRIX.
//
// Using this class on a compiler that does not actually do the alignment
// can be dangerous since it will not expose bugs that ignore alignment.
// E.g if an object of this class in inside a struct or class, and some code
// memcopys data in it assuming tight packing. This could break on a compiler
// that eventually start aligning the matrix.
//---------------------------------------------------------------------------

// Translator comments: None of current pascal compilers can even align data
// inside records to 16 byte boundary, so we just leave aligned matrix
// declaration equal to standart matrix
type
  PD3DXMatrixA16 = ^TD3DXMatrixA16;
  TD3DXMatrixA16 = TD3DXMatrix;

//===========================================================================
//
//    Quaternions
//
//===========================================================================
type
  PD3DXQuaternion = ^TD3DXQuaternion;
  D3DXQUATERNION = record
    x, y, z, w: Single;
    class function Create(const X, Y, Z, W: Single): D3DXQUATERNION; static; inline;
    class operator Add(const Left, Right: D3DXQUATERNION): D3DXQUATERNION; inline;
    class operator Subtract(const Left, Right: D3DXQUATERNION): D3DXQUATERNION; inline;
    class operator Equal(const Left, Right: D3DXQUATERNION): Boolean; inline;
    class function Scale(const Q: D3DXQUATERNION; S: Single): D3DXQUATERNION; static; inline;
  end;
  {$EXTERNALSYM D3DXQUATERNION}
  TD3DXQuaternion = D3DXQUATERNION;

// Some pascal equalents of C++ class functions & operators
//function D3DXQuaternion(_x, _y, _z, _w: Single): TD3DXQuaternion; inline;
function D3DXQuaternionAdd(const q1, q2: D3DXQUATERNION): D3DXQUATERNION; inline;
function D3DXQuaternionSubtract(const q1, q2: D3DXQUATERNION): D3DXQUATERNION; inline;
function D3DXQuaternionEqual(const q1, q2: D3DXQUATERNION): Boolean; inline;
function D3DXQuaternionScale(out qOut: D3DXQUATERNION; const q: D3DXQUATERNION;
  s: Single): PD3DXQuaternion; inline;


//===========================================================================
//
// Planes
//
//===========================================================================
type
  PD3DXPlane = ^TD3DXPlane;
  D3DXPLANE = record
    a, b, c, d: Single;
    class function Create(const A, B, C, D: Single): D3DXPLANE; static; inline;
    class operator Equal(const Left, Right: D3DXPLANE): Boolean; inline;
  end;
  {$EXTERNALSYM D3DXPLANE}
  TD3DXPlane = D3DXPLANE;

// Some pascal equalents of C++ class functions & operators
const D3DXPlaneZero: TD3DXPlane = (a:0; b:0; c:0; d:0);  // (0,0,0,0)
//function D3DXPlane(_a, _b, _c, _d: Single): TD3DXPlane; inline;
function D3DXPlaneEqual(const p1, p2: TD3DXPlane): Boolean; inline;

//===========================================================================
//
// Colors
//
//===========================================================================
type
  PD3DXColor = PD3DColorValue;
  {$EXTERNALSYM PD3DXColor}
  D3DXCOLOR = TD3DColorValue;
  {$EXTERNALSYM D3DXCOLOR}
  TD3DXColor = D3DXCOLOR;

//function D3DXColor(_r, _g, _b, _a: Single): TD3DXColor; inline;
function D3DXColorToDWord(c: D3DXCOLOR): DWord; inline;
function D3DXColorFromDWord(c: DWord): D3DXCOLOR; inline;
function D3DXColorEqual(const c1, c2: D3DXCOLOR): Boolean; inline;

//===========================================================================
//
// D3DX math functions:
//
// NOTE:
//  * All these functions can take the same object as in and out parameters.
//
//  * Out parameters are typically also returned as return values, so that
//    the output of one function may be used as a parameter to another.
//
//===========================================================================

//--------------------------
// Float16
//--------------------------

// non-inline

// Converts an array 32-bit floats to 16-bit floats
function D3DXFloat32To16Array(pOut: PD3DXFloat16; pIn: PSingle; n: LongWord): PD3DXFloat16; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXFloat32To16Array}

// Converts an array 16-bit floats to 32-bit floats
function D3DXFloat16To32Array(pOut: PSingle; pIn: PD3DXFloat16; n: LongWord): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXFloat16To32Array}


//--------------------------
// 2D Vector
//--------------------------

// inline

function D3DXVec2Length(const v: TD3DXVector2): Single; inline;
{$EXTERNALSYM D3DXVec2Length}

function D3DXVec2LengthSq(const v: TD3DXVector2): Single; inline;
{$EXTERNALSYM D3DXVec2LengthSq}

function D3DXVec2Dot(const v1, v2: TD3DXVector2): Single; inline;
{$EXTERNALSYM D3DXVec2Dot}

// Z component of ((x1,y1,0) cross (x2,y2,0))
function D3DXVec2CCW(const v1, v2: TD3DXVector2): Single; inline;
{$EXTERNALSYM D3DXVec2CCW}

function D3DXVec2Add(const v1, v2: TD3DXVector2): TD3DXVector2; inline;
{$EXTERNALSYM D3DXVec2Add}

function D3DXVec2Subtract(const v1, v2: TD3DXVector2): TD3DXVector2; inline;
{$EXTERNALSYM D3DXVec2Subtract}

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec2Minimize(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2; inline;
{$EXTERNALSYM D3DXVec2Minimize}

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec2Maximize(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2; inline;
{$EXTERNALSYM D3DXVec2Maximize}

function D3DXVec2Scale(out vOut: TD3DXVector2; const v: TD3DXVector2; s: Single): PD3DXVector2; inline;
{$EXTERNALSYM D3DXVec2Scale}

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec2Lerp(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2; s: Single): PD3DXVector2; inline;
{$EXTERNALSYM D3DXVec2Lerp}

// non-inline
function D3DXVec2Normalize(out vOut: TD3DXVector2; const v: TD3DXVector2): PD3DXVector2; inline; overload;

function D3DXVec2Normalize(out vOut: TD3DXVector2; v: PD3DXVector2): PD3DXVector2; overload; stdcall;
{$EXTERNALSYM D3DXVec2Normalize}

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec2Hermite(out vOut: TD3DXVector2;
   const v1, t1, v2, t2: TD3DXVector2; s: Single): PD3DXVector2; inline; overload;

function D3DXVec2Hermite(out vOut: TD3DXVector2;
   v1, t1, v2, t2: PD3DXVector2; s: Single): PD3DXVector2; overload; stdcall;
{$EXTERNALSYM D3DXVec2Hermite}

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
function D3DXVec2CatmullRom(out vOut: TD3DXVector2;
   const v0, v1, v2, v3: TD3DXVector2; s: Single): PD3DXVector2; inline; overload;

function D3DXVec2CatmullRom(out vOut: TD3DXVector2;
   v0, v1, v2, v3: PD3DXVector2; s: Single): PD3DXVector2; overload; stdcall;
{$EXTERNALSYM D3DXVec2CatmullRom}

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec2BaryCentric(out vOut: TD3DXVector2;
   const v1, v2, v3: TD3DXVector2; f, g: Single): PD3DXVector2; inline; overload;

function D3DXVec2BaryCentric(out vOut: TD3DXVector2;
   v1, v2, v3: PD3DXVector2; f, g: Single): PD3DXVector2; overload; stdcall;
{$EXTERNALSYM D3DXVec2BaryCentric}

// Transform (x, y, 0, 1) by matrix.
function D3DXVec2Transform(out vOut: TD3DXVector4;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector4; inline; overload;

function D3DXVec2Transform(out vOut: TD3DXVector4;
  v: PD3DXVector2; const m: TD3DXMatrix): PD3DXVector4; overload; stdcall;
{$EXTERNALSYM D3DXVec2Transform}

// Transform (x, y, 0, 1) by matrix, project result back into w=1.
function D3DXVec2TransformCoord(out vOut: TD3DXVector2;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; inline; overload;

function D3DXVec2TransformCoord(out vOut: TD3DXVector2;
  v: PD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; overload; stdcall;
{$EXTERNALSYM D3DXVec2TransformCoord}

// Transform (x, y, 0, 0) by matrix.
function D3DXVec2TransformNormal(out vOut: TD3DXVector2;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; inline; overload;

function D3DXVec2TransformNormal(out vOut: TD3DXVector2;
  v: PD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; overload; stdcall;
{$EXTERNALSYM D3DXVec2TransformNormal}


// Transform Array (x, y, 0, 1) by matrix.
function D3DXVec2TransformArray(pOut: PD3DXVector4; OutStride: LongWord;
  pV: PD3DXVector2; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec2TransformArray}

// Transform Array (x, y, 0, 1) by matrix, project result back into w=1.
function D3DXVec2TransformCoordArray(pOut: PD3DXVector2; OutStride: LongWord;
  pV: PD3DXVector2; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector2; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec2TransformCoordArray}

// Transform Array (x, y, 0, 0) by matrix.
function D3DXVec2TransformNormalArray(pOut: PD3DXVector2; OutStride: LongWord;
  pV: PD3DXVector2; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector2; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec2TransformNormalArray}


//--------------------------
// 3D Vector
//--------------------------

// inline

function D3DXVec3Length(const v: TD3DXVector3): Single; inline;
{$EXTERNALSYM D3DXVec3Length}

function D3DXVec3LengthSq(const v: TD3DXVector3): Single; inline;
{$EXTERNALSYM D3DXVec3LengthSq}

function D3DXVec3Dot(const v1, v2: TD3DXVector3): Single; inline;
{$EXTERNALSYM D3DXVec3Dot}

function D3DXVec3Cross(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Cross}

function D3DXVec3Add(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Add}

function D3DXVec3Subtract(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Subtract}

// Minimize each component.  x = min(x1, x2), y = min(y1, y2), ...
function D3DXVec3Minimize(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Minimize}

// Maximize each component.  x = max(x1, x2), y = max(y1, y2), ...
function D3DXVec3Maximize(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Maximize}

function D3DXVec3Scale(out vOut: TD3DXVector3; const v: TD3DXVector3; s: Single): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Scale}

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec3Lerp(out vOut: TD3DXVector3;
  const v1, v2: TD3DXVector3; s: Single): PD3DXVector3; inline;
{$EXTERNALSYM D3DXVec3Lerp}

// non-inline

function D3DXVec3Normalize(out vOut: TD3DXVector3;
   const v: TD3DXVector3): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3Normalize}

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec3Hermite(out vOut: TD3DXVector3;
   const v1, t1, v2, t2: TD3DXVector3; s: Single): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3Hermite}

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
function D3DXVec3CatmullRom(out vOut: TD3DXVector3;
   const v0, v1, v2, v3: TD3DXVector3; s: Single): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3CatmullRom}

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec3BaryCentric(out vOut: TD3DXVector3;
   const v1, v2, v3: TD3DXVector3; f, g: Single): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3BaryCentric}

// Transform (x, y, z, 1) by matrix.
function D3DXVec3Transform(out vOut: TD3DXVector4;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3Transform}

// Transform (x, y, z, 1) by matrix, project result back into w=1.
function D3DXVec3TransformCoord(out vOut: TD3DXVector3;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3TransformCoord}

// Transform (x, y, z, 0) by matrix.  If you transforming a normal by a
// non-affine matrix, the matrix you pass to this function should be the
// transpose of the inverse of the matrix you would use to transform a coord.
function D3DXVec3TransformNormal(out vOut: TD3DXVector3;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3TransformNormal}


// Transform Array (x, y, z, 1) by matrix.
function D3DXVec3TransformArray(pOut: PD3DXVector4; OutStride: LongWord;
  pV: PD3DXVector3; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3TransformArray}

// Transform Array (x, y, z, 1) by matrix, project result back into w=1.
function D3DXVec3TransformCoordArray(pOut: PD3DXVector3; OutStride: LongWord;
  pV: PD3DXVector3; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3TransformCoordArray}

// Transform (x, y, z, 0) by matrix.  If you transforming a normal by a
// non-affine matrix, the matrix you pass to this function should be the
// transpose of the inverse of the matrix you would use to transform a coord.
function D3DXVec3TransformNormalArray(pOut: PD3DXVector3; OutStride: LongWord;
  pV: PD3DXVector3; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3TransformNormalArray}

// Project vector from object space into screen space
function D3DXVec3Project(out vOut: TD3DXVector3;
  const v: TD3DXVector3; const pViewport: TD3DViewport9;
  const pProjection, pView, pWorld: TD3DXMatrix): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3Project}

// Project vector from screen space into object space
function D3DXVec3Unproject(out vOut: TD3DXVector3;
  const v: TD3DXVector3; const pViewport: TD3DViewport9;
  const pProjection, pView, pWorld: TD3DXMatrix): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3Unproject}

// Project vector Array from object space into screen space
function D3DXVec3ProjectArray(pOut: PD3DXVector3; OutStride: LongWord;
  pV: PD3DXVector3; VStride: LongWord; const pViewport: TD3DViewport9;
  const pProjection, pView, pWorld: TD3DXMatrix; n: LongWord): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3ProjectArray}

// Project vector Array from screen space into object space
function D3DXVec3UnprojectArray(pOut: PD3DXVector3; OutStride: LongWord;
  pV: PD3DXVector3; VStride: LongWord; const pViewport: TD3DViewport9;
  const pProjection, pView, pWorld: TD3DXMatrix; n: LongWord): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec3UnprojectArray}


//--------------------------
// 4D Vector
//--------------------------

// inline

function D3DXVec4Length(const v: TD3DXVector4): Single; inline;
{$EXTERNALSYM D3DXVec4Length}

function D3DXVec4LengthSq(const v: TD3DXVector4): Single; inline;
{$EXTERNALSYM D3DXVec4LengthSq}

function D3DXVec4Dot(const v1, v2: TD3DXVector4): Single; inline;
{$EXTERNALSYM D3DXVec4Dot}

function D3DXVec4Add(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4; inline;
{$EXTERNALSYM D3DXVec4Add}

function D3DXVec4Subtract(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4; inline;
{$EXTERNALSYM D3DXVec4Subtract}

// Minimize each component.  x = min(x1, x2), y = min(y1, y2), ...
function D3DXVec4Minimize(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4; inline;
{$EXTERNALSYM D3DXVec4Minimize}

// Maximize each component.  x = max(x1, x2), y = max(y1, y2), ...
function D3DXVec4Maximize(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4; inline;
{$EXTERNALSYM D3DXVec4Maximize}

function D3DXVec4Scale(out vOut: TD3DXVector4; const v: TD3DXVector4; s: Single): PD3DXVector4; inline;
{$EXTERNALSYM D3DXVec4Scale}

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec4Lerp(out vOut: TD3DXVector4;
  const v1, v2: TD3DXVector4; s: Single): PD3DXVector4; inline;
{$EXTERNALSYM D3DXVec4Lerp}

// non-inline

// Cross-product in 4 dimensions.
function D3DXVec4Cross(out vOut: TD3DXVector4;
  const v1, v2, v3: TD3DXVector4): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4Cross}

function D3DXVec4Normalize(out vOut: TD3DXVector4;
  const v: TD3DXVector4): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4Normalize}

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec4Hermite(out vOut: TD3DXVector4;
   const v1, t1, v2, t2: TD3DXVector4; s: Single): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4Hermite}

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
function D3DXVec4CatmullRom(out vOut: TD3DXVector4;
   const v0, v1, v2, v3: TD3DXVector4; s: Single): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4CatmullRom}

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec4BaryCentric(out vOut: TD3DXVector4;
   const v1, v2, v3: TD3DXVector4; f, g: Single): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4BaryCentric}

// Transform vector by matrix.
function D3DXVec4Transform(out vOut: TD3DXVector4;
  const v: TD3DXVector4; const m: TD3DXMatrix): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4Transform}

// Transform vector array by matrix.
function D3DXVec4TransformArray(pOut: PD3DXVector4; OutStride: LongWord;
  pV: PD3DXVector4; VStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXVector4; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXVec4TransformArray}


//--------------------------
// 4D Matrix
//--------------------------

// inline

function D3DXMatrixIdentity(out mOut: TD3DXMatrix): PD3DXMatrix; inline;
{$EXTERNALSYM D3DXMatrixIdentity}

function D3DXMatrixIsIdentity(const m: TD3DXMatrix): BOOL; inline;
{$EXTERNALSYM D3DXMatrixIsIdentity}

// non-inline

function D3DXMatrixDeterminant(const m: TD3DXMatrix): Single; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixDeterminant}

function D3DXMatrixDecompose(pOutScale: PD3DXVector3; pOutRotation: PD3DXQuaternion;
   pOutTranslation: PD3DXVector3; const M: TD3DXMatrix): HRESULT; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixDecompose}

function D3DXMatrixTranspose(out pOut: TD3DXMatrix; const pM: TD3DXMatrix): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixTranspose}

// Matrix multiplication.  The result represents the transformation M2
// followed by the transformation M1.  (Out = M1 * M2)
function D3DXMatrixMultiply(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixMultiply}

// Matrix multiplication, followed by a transpose. (Out = T(M1 * M2))
function D3DXMatrixMultiplyTranspose(out pOut: TD3DXMatrix; const pM1, pM2: TD3DXMatrix): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixMultiplyTranspose}

// Calculate inverse of matrix.  Inversion my fail, in which case NULL will
// be returned.  The determinant of pM is also returned it pfDeterminant
// is non-NULL.
function D3DXMatrixInverse(out mOut: TD3DXMatrix; pfDeterminant: PSingle;
    const m: TD3DXMatrix): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixInverse}

// Build a matrix which scales by (sx, sy, sz)
function D3DXMatrixScaling(out mOut: TD3DXMatrix; sx, sy, sz: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixScaling}

// Build a matrix which translates by (x, y, z)
function D3DXMatrixTranslation(out mOut: TD3DXMatrix; x, y, z: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixTranslation}

// Build a matrix which rotates around the X axis
function D3DXMatrixRotationX(out mOut: TD3DXMatrix; angle: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixRotationX}

// Build a matrix which rotates around the Y axis
function D3DXMatrixRotationY(out mOut: TD3DXMatrix; angle: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixRotationY}

// Build a matrix which rotates around the Z axis
function D3DXMatrixRotationZ(out mOut: TD3DXMatrix; angle: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixRotationZ}

// Build a matrix which rotates around an arbitrary axis
function D3DXMatrixRotationAxis(out mOut: TD3DXMatrix; const v: TD3DXVector3;
  angle: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixRotationAxis}

// Build a matrix from a quaternion
function D3DXMatrixRotationQuaternion(out mOut: TD3DXMatrix; const Q: TD3DXQuaternion): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixRotationQuaternion}

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
function D3DXMatrixRotationYawPitchRoll(out mOut: TD3DXMatrix; yaw, pitch, roll: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixRotationYawPitchRoll}

// Build transformation matrix.  NULL arguments are treated as identity.
// Mout = Msc-1 * Msr-1 * Ms * Msr * Msc * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixTransformation(out mOut: TD3DXMatrix;
   pScalingCenter: PD3DXVector3;
   pScalingRotation: PD3DXQuaternion; pScaling, pRotationCenter: PD3DXVector3;
   pRotation: PD3DXQuaternion; pTranslation: PD3DXVector3): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixTransformation}

// Build 2D transformation matrix in XY plane.  NULL arguments are treated as identity.
// Mout = Msc-1 * Msr-1 * Ms * Msr * Msc * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixTransformation2D(out pOut: TD3DXMatrix;
   pScalingCenter: PD3DXVector2;
   ScalingRotation: Single; pScaling: PD3DXVector2; pRotationCenter: PD3DXVector2;
   Rotation: Single; pTranslation: PD3DXVector2): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixTransformation2D}

// Build affine transformation matrix.  NULL arguments are treated as identity.
// Mout = Ms * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixAffineTransformation(out mOut: TD3DXMatrix;
   Scaling: Single; pRotationCenter: PD3DXVector3;
   pRotation: PD3DXQuaternion; pTranslation: PD3DXVector3): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixAffineTransformation}

// Build 2D affine transformation matrix in XY plane.  NULL arguments are treated as identity.
// Mout = Ms * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixAffineTransformation2D(out mOut: TD3DXMatrix;
   Scaling: Single; pRotationCenter: PD3DXVector2;
   Rotation: Single; pTranslation: PD3DXVector2): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixAffineTransformation2D}

// Build a lookat matrix. (right-handed)
function D3DXMatrixLookAtRH(out mOut: TD3DXMatrix; const Eye, At, Up: TD3DXVector3): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixLookAtRH}

// Build a lookat matrix. (left-handed)
function D3DXMatrixLookAtLH(out mOut: TD3DXMatrix; const Eye, At, Up: TD3DXVector3): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixLookAtLH}

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveRH(out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixPerspectiveRH}

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveLH(out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixPerspectiveLH}

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveFovRH(out mOut: TD3DXMatrix; flovy, aspect, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixPerspectiveFovRH}

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveFovLH(out mOut: TD3DXMatrix; flovy, aspect, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixPerspectiveFovLH}

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveOffCenterRH(out mOut: TD3DXMatrix;
   l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixPerspectiveOffCenterRH}

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveOffCenterLH(out mOut: TD3DXMatrix;
   l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixPerspectiveOffCenterLH}

// Build an ortho projection matrix. (right-handed)
function D3DXMatrixOrthoRH(out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixOrthoRH}

// Build an ortho projection matrix. (left-handed)
function D3DXMatrixOrthoLH(out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixOrthoLH}

// Build an ortho projection matrix. (right-handed)
function D3DXMatrixOrthoOffCenterRH(out mOut: TD3DXMatrix;
  l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixOrthoOffCenterRH}

// Build an ortho projection matrix. (left-handed)
function D3DXMatrixOrthoOffCenterLH(out mOut: TD3DXMatrix;
  l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixOrthoOffCenterLH}

// Build a matrix which flattens geometry into a plane, as if casting
// a shadow from a light.
function D3DXMatrixShadow(out mOut: TD3DXMatrix;
  const Light: TD3DXVector4; const Plane: TD3DXPlane): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixShadow}

// Build a matrix which reflects the coordinate system about a plane
function D3DXMatrixReflect(out mOut: TD3DXMatrix;
   const Plane: TD3DXPlane): PD3DXMatrix; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXMatrixReflect}


//--------------------------
// Quaternion
//--------------------------

// inline

function D3DXQuaternionLength(const q: TD3DXQuaternion): Single; inline;
{$EXTERNALSYM D3DXQuaternionLength}

// Length squared, or "norm"
function D3DXQuaternionLengthSq(const q: TD3DXQuaternion): Single; inline;
{$EXTERNALSYM D3DXQuaternionLengthSq}

function D3DXQuaternionDot(const q1, q2: TD3DXQuaternion): Single; inline;
{$EXTERNALSYM D3DXQuaternionDot}

// (0, 0, 0, 1)
function D3DXQuaternionIdentity(out qOut: TD3DXQuaternion): PD3DXQuaternion; inline;
{$EXTERNALSYM D3DXQuaternionIdentity}

function D3DXQuaternionIsIdentity (const q: TD3DXQuaternion): BOOL; inline;
{$EXTERNALSYM D3DXQuaternionIsIdentity}

// (-x, -y, -z, w)
function D3DXQuaternionConjugate(out qOut: TD3DXQuaternion;
  const q: TD3DXQuaternion): PD3DXQuaternion; inline;
{$EXTERNALSYM D3DXQuaternionConjugate}


// non-inline

// Compute a quaternin's axis and angle of rotation. Expects unit quaternions.
procedure D3DXQuaternionToAxisAngle(const q: TD3DXQuaternion;
  out Axis: TD3DXVector3; out Angle: Single); stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionToAxisAngle}

// Build a quaternion from a rotation matrix.
function D3DXQuaternionRotationMatrix(out qOut: TD3DXQuaternion;
  const m: TD3DXMatrix): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionRotationMatrix}

// Rotation about arbitrary axis.
function D3DXQuaternionRotationAxis(out qOut: TD3DXQuaternion;
  const v: TD3DXVector3; Angle: Single): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionRotationAxis}

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
function D3DXQuaternionRotationYawPitchRoll(out qOut: TD3DXQuaternion;
  yaw, pitch, roll: Single): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionRotationYawPitchRoll}

// Quaternion multiplication.  The result represents the rotation Q2
// followed by the rotation Q1.  (Out = Q2 * Q1)
function D3DXQuaternionMultiply(out qOut: TD3DXQuaternion;
   const q1, q2: TD3DXQuaternion): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionMultiply}

function D3DXQuaternionNormalize(out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionNormalize}

// Conjugate and re-norm
function D3DXQuaternionInverse(out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionInverse}

// Expects unit quaternions.
// if q = (cos(theta), sin(theta) * v); ln(q) = (0, theta * v)
function D3DXQuaternionLn(out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionLn}

// Expects pure quaternions. (w == 0)  w is ignored in calculation.
// if q = (0, theta * v); exp(q) = (cos(theta), sin(theta) * v)
function D3DXQuaternionExp(out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionExp}

// Spherical linear interpolation between Q1 (s == 0) and Q2 (s == 1).
// Expects unit quaternions.
function D3DXQuaternionSlerp(out qOut: TD3DXQuaternion;
   const q1, q2: TD3DXQuaternion; t: Single): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionSlerp}

// Spherical quadrangle interpolation.
// Slerp(Slerp(Q1, C, t), Slerp(A, B, t), 2t(1-t))
function D3DXQuaternionSquad(out qOut: TD3DXQuaternion;
   const pQ1, pA, pB, pC: TD3DXQuaternion; t: Single): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionSquad}

// Setup control points for spherical quadrangle interpolation
// from Q1 to Q2.  The control points are chosen in such a way
// to ensure the continuity of tangents with adjacent segments.
procedure D3DXQuaternionSquadSetup(out pAOut, pBOut, pCOut: TD3DXQuaternion;
   const pQ0, pQ1, pQ2, pQ3: TD3DXQuaternion); stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionSquadSetup}

// Barycentric interpolation.
// Slerp(Slerp(Q1, Q2, f+g), Slerp(Q1, Q3, f+g), g/(f+g))
function D3DXQuaternionBaryCentric(out qOut: TD3DXQuaternion;
   const q1, q2, q3: TD3DXQuaternion; f, g: Single): PD3DXQuaternion; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXQuaternionBaryCentric}


//--------------------------
// Plane
//--------------------------

// inline

// ax + by + cz + dw
function D3DXPlaneDot(const p: TD3DXPlane; const v: TD3DXVector4): Single; inline;
{$EXTERNALSYM D3DXPlaneDot}

// ax + by + cz + d
function D3DXPlaneDotCoord(const p: TD3DXPlane; const v: TD3DXVector3): Single; inline;
{$EXTERNALSYM D3DXPlaneDotCoord}

// ax + by + cz
function D3DXPlaneDotNormal(const p: TD3DXPlane; const v: TD3DXVector3): Single; inline;
{$EXTERNALSYM D3DXPlaneDotNormal}

function D3DXPlaneScale(out pOut: TD3DXPlane; const pP: TD3DXPlane; s: Single): PD3DXPlane; inline;
{$EXTERNALSYM D3DXPlaneScale}


// non-inline

// Normalize plane (so that |a,b,c| == 1)
function D3DXPlaneNormalize(out pOut: TD3DXPlane; const p: TD3DXPlane): PD3DXPlane; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXPlaneNormalize}

// Find the intersection between a plane and a line.  If the line is
// parallel to the plane, NULL is returned.
function D3DXPlaneIntersectLine(out pOut: TD3DXVector3;
   const p: TD3DXPlane; const v1, v2: TD3DXVector3): PD3DXVector3; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXPlaneIntersectLine}

// Construct a plane from a point and a normal
function D3DXPlaneFromPointNormal(out pOut: TD3DXPlane;
   const vPoint, vNormal: TD3DXVector3): PD3DXPlane; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXPlaneFromPointNormal}

// Construct a plane from 3 points
function D3DXPlaneFromPoints(out pOut: TD3DXPlane;
   const v1, v2, v3: TD3DXVector3): PD3DXPlane; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXPlaneFromPoints}

// Transform a plane by a matrix.  The vector (a,b,c) must be normal.
// M should be the inverse transpose of the transformation desired.
function D3DXPlaneTransform(out pOut: TD3DXPlane; const p: TD3DXPlane; const m: TD3DXMatrix): PD3DXPlane; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXPlaneTransform}

// Transform an array of planes by a matrix.  The vectors (a,b,c) must be normal.
// M should be the inverse transpose of the transformation desired.
function D3DXPlaneTransformArray(pOut: PD3DXPlane; OutStride: LongWord;
  pP: PD3DXPlane; PStride: LongWord; const m: TD3DXMatrix; n: LongWord): PD3DXPlane; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXPlaneTransformArray}


//--------------------------
// Color
//--------------------------

// inline

// (1-r, 1-g, 1-b, a)
function D3DXColorNegative(out cOut: TD3DXColor; const c: TD3DXColor): PD3DXColor; inline;
{$EXTERNALSYM D3DXColorNegative}

function D3DXColorAdd(out cOut: TD3DXColor; const c1, c2: TD3DXColor): PD3DXColor; inline;
{$EXTERNALSYM D3DXColorAdd}

function D3DXColorSubtract(out cOut: TD3DXColor; const c1, c2: TD3DXColor): PD3DXColor; inline;
{$EXTERNALSYM D3DXColorSubtract}

function D3DXColorScale(out cOut: TD3DXColor; const c: TD3DXColor; s: Single): PD3DXColor; inline;
{$EXTERNALSYM D3DXColorScale}

// (r1*r2, g1*g2, b1*b2, a1*a2)
function D3DXColorModulate(out cOut: TD3DXColor; const c1, c2: TD3DXColor): PD3DXColor; inline;
{$EXTERNALSYM D3DXColorModulate}

// Linear interpolation of r,g,b, and a. C1 + s(C2-C1)
function D3DXColorLerp(out cOut: TD3DXColor; const c1, c2: TD3DXColor; s: Single): PD3DXColor; inline;
{$EXTERNALSYM D3DXColorLerp}

// non-inline

// Interpolate r,g,b between desaturated color and color.
// DesaturatedColor + s(Color - DesaturatedColor)
function D3DXColorAdjustSaturation(out cOut: TD3DXColor;
   const pC: TD3DXColor; s: Single): PD3DXColor; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXColorAdjustSaturation}

// Interpolate r,g,b between 50% grey and color.  Grey + s(Color - Grey)
function D3DXColorAdjustContrast(out cOut: TD3DXColor;
   const pC: TD3DXColor; c: Single): PD3DXColor; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXColorAdjustContrast}


//--------------------------
// Misc
//--------------------------

// Calculate Fresnel term given the cosine of theta (likely obtained by
// taking the dot of two normals), and the refraction index of the material.
function D3DXFresnelTerm(CosTheta, RefractionIndex: Single): Single; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXFresnelTerm}



//===========================================================================
//
//    Matrix Stack
//
//===========================================================================

type
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXMatrixStack);'}
  {$EXTERNALSYM ID3DXMatrixStack}
  ID3DXMatrixStack = interface(IUnknown)
    ['{C7885BA7-F990-4fe7-922D-8515E477DD85}']
    //
    // ID3DXMatrixStack methods
    //

    // Pops the top of the stack, returns the current top
    // *after* popping the top.
    function Pop: HResult; stdcall;

    // Pushes the stack by one, duplicating the current matrix.
    function Push: HResult; stdcall;

    // Loads identity in the current matrix.
    function LoadIdentity: HResult; stdcall;

    // Loads the given matrix into the current matrix
    function LoadMatrix(const M: TD3DXMatrix): HResult; stdcall;

    // Right-Multiplies the given matrix to the current matrix.
    // (transformation is about the current world origin)
    function MultMatrix(const M: TD3DXMatrix): HResult; stdcall;

    // Left-Multiplies the given matrix to the current matrix
    // (transformation is about the local origin of the object)
    function MultMatrixLocal(const M: TD3DXMatrix): HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the current world origin)
    function RotateAxis(const V: TD3DXVector3; Angle: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the local origin of the object)
    function RotateAxisLocal(const V: TD3DXVector3; Angle: Single): HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // current world origin)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRoll(yaw, pitch, roll: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // local origin of the object)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRollLocal(yaw, pitch, roll: Single): HResult; stdcall;

    // Right multiply the current matrix with the computed scale
    // matrix. (transformation is about the current world origin)
    function Scale(x, y, z: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed scale
    // matrix. (transformation is about the local origin of the object)
    function ScaleLocal(x, y, z: Single): HResult; stdcall;

    // Right multiply the current matrix with the computed translation
    // matrix. (transformation is about the current world origin)
    function Translate(x, y, z: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed translation
    // matrix. (transformation is about the local origin of the object)
    function TranslateLocal(x, y, z: Single): HResult; stdcall;

    // Obtain the current matrix at the top of the stack
    function GetTop: PD3DXMatrix; stdcall;
  end;

type
  IID_ID3DXMatrixStack = ID3DXMatrixStack;
  {$EXTERNALSYM IID_ID3DXMatrixStack}

function D3DXCreateMatrixStack(Flags: DWord; out Stack: ID3DXMatrixStack): HResult; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXCreateMatrixStack}

//===========================================================================
//
//  Spherical Harmonic Runtime Routines
//
// NOTE:
//  * Most of these functions can take the same object as in and out parameters.
//    The exceptions are the rotation functions.
//
//  * Out parameters are typically also returned as return values, so that
//    the output of one function may be used as a parameter to another.
//
//============================================================================


//============================================================================
//
//  Basic Spherical Harmonic math routines
//
//============================================================================

const
  D3DXSH_MINORDER = 2;
  {$EXTERNALSYM D3DXSH_MINORDER}
  D3DXSH_MAXORDER = 6;
  {$EXTERNALSYM D3DXSH_MAXORDER}

//============================================================================
//
//  D3DXSHEvalDirection:
//  --------------------
//  Evaluates the Spherical Harmonic basis functions
//
//  Parameters:
//   pOut
//      Output SH coefficients - basis function Ylm is stored at l*l + m+l
//      This is the pointer that is returned.
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pDir
//      Direction to evaluate in - assumed to be normalized
//
//============================================================================

function D3DXSHEvalDirection(pOut: PSingle; Order: LongWord;
    const pDir: TD3DXVector3): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHEvalDirection}

//============================================================================
//
//  D3DXSHRotate:
//  --------------------
//  Rotates SH vector by a rotation matrix
//
//  Parameters:
//   pOut
//      Output SH coefficients - basis function Ylm is stored at l*l + m+l
//      This is the pointer that is returned (should not alias with pIn.)
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pMatrix
//      Matrix used for rotation - rotation sub matrix should be orthogonal
//      and have a unit determinant.
//   pIn
//      Input SH coeffs (rotated), incorect results if this is also output.
//
//============================================================================

function D3DXSHRotate(pOut: PSingle; Order: LongWord;
    const pMatrix: TD3DXMatrix; pIn: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHRotate}

//============================================================================
//
//  D3DXSHRotateZ:
//  --------------------
//  Rotates the SH vector in the Z axis by an angle
//
//  Parameters:
//   pOut
//      Output SH coefficients - basis function Ylm is stored at l*l + m+l
//      This is the pointer that is returned (should not alias with pIn.)
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   Angle
//      Angle in radians to rotate around the Z axis.
//   pIn
//      Input SH coeffs (rotated), incorect results if this is also output.
//
//============================================================================

function D3DXSHRotateZ(pOut: PSingle; Order: LongWord;
    Angle: Single; pIn: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHRotateZ}

//============================================================================
//
//  D3DXSHAdd:
//  --------------------
//  Adds two SH vectors, pOut[i] = pA[i] + pB[i];
//
//  Parameters:
//   pOut
//      Output SH coefficients - basis function Ylm is stored at l*l + m+l
//      This is the pointer that is returned.
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pA
//      Input SH coeffs.
//   pB
//      Input SH coeffs (second vector.)
//
//============================================================================

function D3DXSHAdd(pOut: PSingle; Order: LongWord;
    pA, pB: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHAdd}

//============================================================================
//
//  D3DXSHScale:
//  --------------------
//  Adds two SH vectors, pOut[i] = pA[i]*Scale;
//
//  Parameters:
//   pOut
//      Output SH coefficients - basis function Ylm is stored at l*l + m+l
//      This is the pointer that is returned.
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pIn
//      Input SH coeffs.
//   Scale
//      Scale factor.
//
//============================================================================

function D3DXSHScale(pOut: PSingle; Order: LongWord;
    pIn: PSingle; Scale: Single): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHScale}

//============================================================================
//
//  D3DXSHDot:
//  --------------------
//  Computes the dot product of two SH vectors
//
//  Parameters:
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pA
//      Input SH coeffs.
//   pB
//      Second set of input SH coeffs.
//
//============================================================================

function D3DXSHDot(Order: LongWord; pA, pB: PSingle): Single; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHDot}

//============================================================================
//
//  D3DXSHMultiply[O]:
//  --------------------
//  Computes the product of two functions represented using SH (f and g), where:
//  pOut[i] = int(y_i(s) * f(s) * g(s)), where y_i(s) is the ith SH basis
//  function, f(s) and g(s) are SH functions (sum_i(y_i(s)*c_i)).  The order O
//  determines the lengths of the arrays, where there should always be O^2
//  coefficients.  In general the product of two SH functions of order O generates
//  and SH function of order 2*O - 1, but we truncate the result.  This means
//  that the product commutes (f*g == g*f) but doesn't associate
//  (f*(g*h) != (f*g)*h.
//
//  Parameters:
//   pOut
//      Output SH coefficients - basis function Ylm is stored at l*l + m+l
//      This is the pointer that is returned.
//   pF
//      Input SH coeffs for first function.
//   pG
//      Second set of input SH coeffs.
//
//============================================================================

function D3DXSHMultiply2(pOut: PSingle; const pF, pG: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHMultiply2}
function D3DXSHMultiply3(pOut: PSingle; const pF, pG: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHMultiply3}
function D3DXSHMultiply4(pOut: PSingle; const pF, pG: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHMultiply4}
function D3DXSHMultiply5(pOut: PSingle; const pF, pG: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHMultiply5}
function D3DXSHMultiply6(pOut: PSingle; const pF, pG: PSingle): PSingle; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHMultiply6}

//============================================================================
//
//  Basic Spherical Harmonic lighting routines
//
//============================================================================

//============================================================================
//
//  D3DXSHEvalDirectionalLight:
//  --------------------
//  Evaluates a directional light and returns spectral SH data.  The output
//  vector is computed so that if the intensity of R/G/B is unit the resulting
//  exit radiance of a point directly under the light on a diffuse object with
//  an albedo of 1 would be 1.0.  This will compute 3 spectral samples, pROut
//  has to be specified, while pGout and pBout are optional.
//
//  Parameters:
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pDir
//      Direction light is coming from (assumed to be normalized.)
//   RIntensity
//      Red intensity of light.
//   GIntensity
//      Green intensity of light.
//   BIntensity
//      Blue intensity of light.
//   pROut
//      Output SH vector for Red.
//   pGOut
//      Output SH vector for Green (optional.)
//   pBOut
//      Output SH vector for Blue (optional.)
//
//============================================================================

function D3DXSHEvalDirectionalLight(Order: LongWord; const pDir: TD3DXVector3;
    RIntensity: Single; GIntensity: Single; BIntensity: Single;
    pROut, pGOut, pBOut: PSingle): HResult; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHEvalDirectionalLight}

//============================================================================
//
//  D3DXSHEvalSphericalLight:
//  --------------------
//  Evaluates a spherical light and returns spectral SH data.  There is no
//  normalization of the intensity of the light like there is for directional
//  lights, care has to be taken when specifiying the intensities.  This will
//  compute 3 spectral samples, pROut has to be specified, while pGout and
//  pBout are optional.
//
//  Parameters:
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pPos
//      Position of light - reciever is assumed to be at the origin.
//   Radius
//      Radius of the spherical light source.
//   RIntensity
//      Red intensity of light.
//   GIntensity
//      Green intensity of light.
//   BIntensity
//      Blue intensity of light.
//   pROut
//      Output SH vector for Red.
//   pGOut
//      Output SH vector for Green (optional.)
//   pBOut
//      Output SH vector for Blue (optional.)
//
//============================================================================

function D3DXSHEvalSphericalLight(Order: LongWord; const pPos: TD3DXVector3; Radius: Single;
    RIntensity: Single; GIntensity: Single; BIntensity: Single;
    pROut, pGOut, pBOut: PSingle): HResult; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHEvalSphericalLight}

//============================================================================
//
//  D3DXSHEvalConeLight:
//  --------------------
//  Evaluates a light that is a cone of constant intensity and returns spectral
//  SH data.  The output vector is computed so that if the intensity of R/G/B is
//  unit the resulting exit radiance of a point directly under the light oriented
//  in the cone direction on a diffuse object with an albedo of 1 would be 1.0.
//  This will compute 3 spectral samples, pROut has to be specified, while pGout
//  and pBout are optional.
//
//  Parameters:
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pDir
//      Direction light is coming from (assumed to be normalized.)
//   Radius
//      Radius of cone in radians.
//   RIntensity
//      Red intensity of light.
//   GIntensity
//      Green intensity of light.
//   BIntensity
//      Blue intensity of light.
//   pROut
//      Output SH vector for Red.
//   pGOut
//      Output SH vector for Green (optional.)
//   pBOut
//      Output SH vector for Blue (optional.)
//
//============================================================================

function D3DXSHEvalConeLight(Order: LongWord; const pDir: TD3DXVector3; Radius: Single;
    RIntensity: Single; GIntensity: Single; BIntensity: Single;
    pROut, pGOut, pBOut: PSingle): HResult; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHEvalConeLight}

//============================================================================
//
//  D3DXSHEvalHemisphereLight:
//  --------------------
//  Evaluates a light that is a linear interpolant between two colors over the
//  sphere.  The interpolant is linear along the axis of the two points, not
//  over the surface of the sphere (ie: if the axis was (0,0,1) it is linear in
//  Z, not in the azimuthal angle.)  The resulting spherical lighting function
//  is normalized so that a point on a perfectly diffuse surface with no
//  shadowing and a normal pointed in the direction pDir would result in exit
//  radiance with a value of 1 if the top color was white and the bottom color
//  was black.  This is a very simple model where Top represents the intensity
//  of the "sky" and Bottom represents the intensity of the "ground".
//
//  Parameters:
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pDir
//      Axis of the hemisphere.
//   Top
//      Color of the upper hemisphere.
//   Bottom
//      Color of the lower hemisphere.
//   pROut
//      Output SH vector for Red.
//   pGOut
//      Output SH vector for Green
//   pBOut
//      Output SH vector for Blue
//
//============================================================================

function D3DXSHEvalHemisphereLight(Order: LongWord; const pDir: TD3DXVector3;
    Top, Bottom: TD3DXColor;
    pROut, pGOut, pBOut: PSingle): HResult; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHEvalHemisphereLight}

//============================================================================
//
//  Basic Spherical Harmonic projection routines
//
//============================================================================

//============================================================================
//
//  D3DXSHProjectCubeMap:
//  --------------------
//  Projects a function represented on a cube map into spherical harmonics.
//
//  Parameters:
//   Order
//      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
//   pCubeMap
//      CubeMap that is going to be projected into spherical harmonics
//   pROut
//      Output SH vector for Red.
//   pGOut
//      Output SH vector for Green
//   pBOut
//      Output SH vector for Blue
//
//============================================================================

function D3DXSHProjectCubeMap(Order: LongWord; pCubeMap: IDirect3DCubeTexture9;
    pROut, pGOut, pBOut: PSingle): HResult; stdcall; external d3dx9mathDLL;
{$EXTERNALSYM D3DXSHProjectCubeMap}

implementation

end.

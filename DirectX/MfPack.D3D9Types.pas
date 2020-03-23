// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectX
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.D3d9Types.pas
// Kind: Pascal / Delphi unit
// Release date: 14-01-2018
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Direct3D include file.
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
// Remarks: Requires Windows Vista or later.
// 
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
// Source: d3d9types.h
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
unit MfPack.D3d9Types;

  {$HPPEMIT '#include "d3d9types.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  {MfPack}
  MfPack.MfpTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}


{$IFNDEF DIRECT3D_VERSION}
const
  DIRECT3D_VERSION                    = $0900;
  {$EXTERNALSYM DIRECT3D_VERSION}
{$ENDIF}  //DIRECT3D_VERSION

type
// D3DCOLOR is equivalent to D3DFMT_A8R8G8B8
{$IFNDEF D3DCOLOR_DEFINED}
  D3DCOLOR = DWORD;
  {$EXTERNALSYM D3DCOLOR}
{$DEFINE D3DCOLOR_DEFINED}
{$ENDIF}

  // maps unsigned 8 bits/channel to D3DCOLOR
  // values must be in the range 0 through 255!
  function D3DCOLOR_ARGB(a: Int32; r: Int32; g: Int32; b: Int32): D3DCOLOR;

  function D3DCOLOR_RGBA(r: Int32; g: Int32; b: Int32; a: Int32): D3DCOLOR;

  function D3DCOLOR_XRGB(r: Int32; g: Int32; b: Int32): D3DCOLOR;

  function D3DCOLOR_XYUV(y: Int32; u: Int32; v: Int32): D3DCOLOR;

  function D3DCOLOR_AYUV(a: Int32; y: Int32; u: Int32; v: Int32): D3DCOLOR;


  // maps floating point channels (0.0 to 1.0 range) to D3DCOLOR
  function D3DCOLOR_COLORVALUE(r: FLOAT; g: FLOAT; b: FLOAT; a: FLOAT): D3DCOLOR;
  {$EXTERNALSYM D3DCOLOR_COLORVALUE}

type

{$IFNDEF D3DVECTOR_DEFINED}
  PD3DVECTOR = ^D3DVECTOR;
  _D3DVECTOR = record
    x: Single;
    y: Single;
    z: Single;
  end;
  {$EXTERNALSYM _D3DVECTOR}
  D3DVECTOR = _D3DVECTOR;
  {$EXTERNALSYM D3DVECTOR}
{$DEFINE D3DVECTOR_DEFINED}
{$ENDIF}


{$IFNDEF D3DCOLORVALUE_DEFINED}
  PD3DCOLORVALUE = ^D3DCOLORVALUE;
  _D3DCOLORVALUE = record
    r: Single;
    g: Single;
    b: Single;
    a: Single;
  end;
  {$EXTERNALSYM _D3DCOLORVALUE}
  D3DCOLORVALUE = _D3DCOLORVALUE;
  {$EXTERNALSYM D3DCOLORVALUE}
{$DEFINE D3DCOLORVALUE_DEFINED}
{$ENDIF}

{$IFNDEF D3DRECT_DEFINED}
  PD3DRECT = ^D3DRECT;
  _D3DRECT = record
    x1: LONG;
    y1: LONG;
    x2: LONG;
    y2: LONG;
  end;
  {$EXTERNALSYM _D3DRECT}
  D3DRECT = _D3DRECT;
  {$EXTERNALSYM D3DRECT}
{$DEFINE D3DRECT_DEFINED}
{$ENDIF}



{$IFNDEF D3DMATRIX_DEFINED}
 PD3DMATRIX = ^D3DMATRIX;
  _D3DMATRIX = record
    _11, _12, _13, _14: FLOAT;
    _21, _22, _23, _24: FLOAT;
    _31, _32, _33, _34: FLOAT;
    _41, _42, _43, _44: FLOAT;
  end;
  {$EXTERNALSYM _D3DMATRIX}
 D3DMATRIX = _D3DMATRIX;
 {$EXTERNALSYM D3DMATRIX}
{$DEFINE D3DMATRIX_DEFINED}
{$ENDIF}


  PD3DVIEWPORT9 = ^D3DVIEWPORT9;
  _D3DVIEWPORT9 = record
    X: DWORD;
    Y: DWORD;                        { Viewport Top left }
    Width: DWORD;
    Height: DWORD;                   { Viewport Dimensions }
    MinZ: FLOAT;                     { Min/max of clip Volume }
    MaxZ: FLOAT;
  end;
  {$EXTERNALSYM _D3DVIEWPORT9}
  D3DVIEWPORT9 = _D3DVIEWPORT9;
  {$EXTERNALSYM D3DVIEWPORT9}

const

  // Values for clip fields.
  // =======================

  // Max number of user clipping planes, supported in D3D.
  D3DMAXUSERCLIPPLANES                = 32;
  {$EXTERNALSYM D3DMAXUSERCLIPPLANES}

  // These bits could be ORed together to use with D3DRS_CLIPPLANEENABLE
  //
  D3DCLIPPLANE0                       = (1 shl 0);
  {$EXTERNALSYM D3DCLIPPLANE0}
  D3DCLIPPLANE1                       = (1 shl 1);
  {$EXTERNALSYM D3DCLIPPLANE1}
  D3DCLIPPLANE2                       = (1 shl 2);
  {$EXTERNALSYM D3DCLIPPLANE2}
  D3DCLIPPLANE3                       = (1 shl 3);
  {$EXTERNALSYM D3DCLIPPLANE3}
  D3DCLIPPLANE4                       = (1 shl 4);
  {$EXTERNALSYM D3DCLIPPLANE4}
  D3DCLIPPLANE5                       = (1 shl 5);
  {$EXTERNALSYM D3DCLIPPLANE5}

  // The following bits are used in the ClipUnion and ClipIntersection
  // members of the D3DCLIPSTATUS9
  //
  D3DCS_LEFT                          = $00000001;
  {$EXTERNALSYM D3DCS_LEFT}
  D3DCS_RIGHT                         = $00000002;
  {$EXTERNALSYM D3DCS_RIGHT}
  D3DCS_TOP                           = $00000004;
  {$EXTERNALSYM D3DCS_TOP}
  D3DCS_BOTTOM                        = $00000008;
  {$EXTERNALSYM D3DCS_BOTTOM}
  D3DCS_FRONT                         = $00000010;
  {$EXTERNALSYM D3DCS_FRONT}
  D3DCS_BACK                          = $00000020;
  {$EXTERNALSYM D3DCS_BACK}
  D3DCS_PLANE0                        = $00000040;
  {$EXTERNALSYM D3DCS_PLANE0}
  D3DCS_PLANE1                        = $00000080;
  {$EXTERNALSYM D3DCS_PLANE1}
  D3DCS_PLANE2                        = $00000100;
  {$EXTERNALSYM D3DCS_PLANE2}
  D3DCS_PLANE3                        = $00000200;
  {$EXTERNALSYM D3DCS_PLANE3}
  D3DCS_PLANE4                        = $00000400;
  {$EXTERNALSYM D3DCS_PLANE4}
  D3DCS_PLANE5                        = $00000800;
  {$EXTERNALSYM D3DCS_PLANE5}


  D3DCS_ALL                           = (D3DCS_LEFT OR
                                         D3DCS_RIGHT OR
                                         D3DCS_TOP OR
                                         D3DCS_BOTTOM OR
                                         D3DCS_FRONT OR
                                         D3DCS_BACK OR
                                         D3DCS_PLANE0 OR
                                         D3DCS_PLANE1 OR
                                         D3DCS_PLANE2 OR
                                         D3DCS_PLANE3 OR
                                         D3DCS_PLANE4 OR
                                         D3DCS_PLANE5);
  {$EXTERNALSYM D3DCS_ALL}


type

  PD3DCLIPSTATUS9 = ^D3DCLIPSTATUS9;
  _D3DCLIPSTATUS9 = record
    ClipUnion: DWORD;
    ClipIntersection: DWORD;
  end;
  {$EXTERNALSYM _D3DCLIPSTATUS9}
  D3DCLIPSTATUS9 = _D3DCLIPSTATUS9;
  {$EXTERNALSYM D3DCLIPSTATUS9}


  PD3DMATERIAL9 = ^D3DMATERIAL9;
  _D3DMATERIAL9 = record
    Diffuse: D3DCOLORVALUE;          { Diffuse color RGBA }
    Ambient: D3DCOLORVALUE;          { Ambient color RGB }
    Specular: D3DCOLORVALUE;         { Specular 'shininess' }
    Emissive: D3DCOLORVALUE;         { Emissive color RGB }
    Power: Single;                   { Sharpness if specular highlight }
  end;
  {$EXTERNALSYM _D3DMATERIAL9}
  D3DMATERIAL9 = _D3DMATERIAL9;
  {$EXTERNALSYM D3DMATERIAL9}


  PD3DLIGHTTYPE = ^D3DLIGHTTYPE;
  _D3DLIGHTTYPE          = (
    D3DLIGHT_POINT       = 1,
    D3DLIGHT_SPOT        = 2,
    D3DLIGHT_DIRECTIONAL = 3,
    D3DLIGHT_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DLIGHTTYPE}
  D3DLIGHTTYPE = _D3DLIGHTTYPE;
  {$EXTERNALSYM D3DLIGHTTYPE}


  PD3DLIGHT9 = ^D3DLIGHT9;
  _D3DLIGHT9 = record
    _Type: D3DLIGHTTYPE;             { Type of light source }
    Diffuse: D3DCOLORVALUE;          { Diffuse color of light }
    Specular: D3DCOLORVALUE;         { Specular color of light }
    Ambient: D3DCOLORVALUE;          { Ambient color of light }
    Position: D3DVECTOR;             { Position in world space }
    Direction: D3DVECTOR;            { Direction in world space }
    Range: FLOAT;                    { Cutoff range }
    Falloff: FLOAT;                  { Falloff }
    Attenuation0: FLOAT;             { Constant attenuation }
    Attenuation1: FLOAT;             { Linear attenuation }
    Attenuation2: FLOAT;             { Quadratic attenuation }
    Theta: FLOAT;                    { Inner angle of spotlight cone }
    Phi: FLOAT;                      { Outer angle of spotlight cone }
  end;
  {$EXTERNALSYM _D3DLIGHT9}
  D3DLIGHT9 = _D3DLIGHT9;
  {$EXTERNALSYM D3DLIGHT9}

const

  // Options for clearing
  D3DCLEAR_TARGET                     = $00000001;  { Clear target surface }
  {$EXTERNALSYM D3DCLEAR_TARGET}
  D3DCLEAR_ZBUFFER                    = $00000002;  { Clear target z buffer }
  {$EXTERNALSYM D3DCLEAR_ZBUFFER}
  D3DCLEAR_STENCIL                    = $00000004;  { Clear stencil planes }
  {$EXTERNALSYM D3DCLEAR_STENCIL}


  // The following defines the rendering states
  //===========================================

type

  PD3DSHADEMODE = ^D3DSHADEMODE;
  _D3DSHADEMODE          = (
    D3DSHADE_FLAT        = 1,
    D3DSHADE_GOURAUD     = 2,
    D3DSHADE_PHONG       = 3,
    D3DSHADE_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DSHADEMODE}
  D3DSHADEMODE = _D3DSHADEMODE;
  {$EXTERNALSYM D3DSHADEMODE}


  PD3DFILLMODE = ^D3DFILLMODE;
  _D3DFILLMODE          = (
    D3DFILL_POINT       = 1,
    D3DFILL_WIREFRAME   = 2,
    D3DFILL_SOLID       = 3,
    D3DFILL_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DFILLMODE}
  D3DFILLMODE = _D3DFILLMODE;
  {$EXTERNALSYM D3DFILLMODE}


  PD3DBLEND = ^D3DBLEND;
  _D3DBLEND                  = (
    D3DBLEND_ZERO            = 1,
    D3DBLEND_ONE             = 2,
    D3DBLEND_SRCCOLOR        = 3,
    D3DBLEND_INVSRCCOLOR     = 4,
    D3DBLEND_SRCALPHA        = 5,
    D3DBLEND_INVSRCALPHA     = 6,
    D3DBLEND_DESTALPHA       = 7,
    D3DBLEND_INVDESTALPHA    = 8,
    D3DBLEND_DESTCOLOR       = 9,
    D3DBLEND_INVDESTCOLOR    = 10,
    D3DBLEND_SRCALPHASAT     = 11,
    D3DBLEND_BOTHSRCALPHA    = 12,
    D3DBLEND_BOTHINVSRCALPHA = 13,
    D3DBLEND_BLENDFACTOR     = 14,         { Only supported if D3DPBLENDCAPS_BLENDFACTOR is on }
    D3DBLEND_INVBLENDFACTOR  = 15,         { Only supported if D3DPBLENDCAPS_BLENDFACTOR is on }
    //* D3D9Ex only -- */
    //{$IFDEF D3D_DISABLE_9EX}
    D3DBLEND_SRCCOLOR2       = 16,
    D3DBLEND_INVSRCCOLOR2    = 17,
    //{$ENDIF} // !D3D_DISABLE_9EX
    //* -- D3D9Ex only
    D3DBLEND_FORCE_DWORD     = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DBLEND}
  D3DBLEND = _D3DBLEND;
  {$EXTERNALSYM D3DBLEND}


  PD3DBLENDOP = ^D3DBLENDOP;
  _D3DBLENDOP              = (
    D3DBLENDOP_ADD         = 1,
    D3DBLENDOP_SUBTRACT    = 2,
    D3DBLENDOP_REVSUBTRACT = 3,
    D3DBLENDOP_MIN         = 4,
    D3DBLENDOP_MAX         = 5,
    D3DBLENDOP_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DBLENDOP}
  D3DBLENDOP = _D3DBLENDOP;
  {$EXTERNALSYM D3DBLENDOP}


  PD3DTEXTUREADDRESS = ^D3DTEXTUREADDRESS;
  _D3DTEXTUREADDRESS        = (
    D3DTADDRESS_WRAP        = 1,
    D3DTADDRESS_MIRROR      = 2,
    D3DTADDRESS_CLAMP       = 3,
    D3DTADDRESS_BORDER      = 4,
    D3DTADDRESS_MIRRORONCE  = 5,
    D3DTADDRESS_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DTEXTUREADDRESS}
  D3DTEXTUREADDRESS = _D3DTEXTUREADDRESS;
  {$EXTERNALSYM D3DTEXTUREADDRESS}


  PD3DCULL = ^D3DCULL;
  _D3DCULL              = (
    D3DCULL_NONE        = 1,
    D3DCULL_CW          = 2,
    D3DCULL_CCW         = 3,
    D3DCULL_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DCULL}
  D3DCULL = _D3DCULL;
  {$EXTERNALSYM D3DCULL}


  PD3DCMPFUNC = ^D3DCMPFUNC;
  {$EXTERNALSYM _D3DCMPFUNC}
  {$EXTERNALSYM _D3DCMPFUNC}
  _D3DCMPFUNC           = (
    D3DCMP_NEVER        = 1,
    D3DCMP_LESS         = 2,
    D3DCMP_EQUAL        = 3,
    D3DCMP_LESSEQUAL    = 4,
    D3DCMP_GREATER      = 5,
    D3DCMP_NOTEQUAL     = 6,
    D3DCMP_GREATEREQUAL = 7,
    D3DCMP_ALWAYS       = 8,
    D3DCMP_FORCE_DWORD  = FORCEDWORD  { force 32-bit size enum }
  );
  D3DCMPFUNC = _D3DCMPFUNC;
  {$EXTERNALSYM D3DCMPFUNC}


  PD3DSTENCILOP = ^D3DSTENCILOP;
  _D3DSTENCILOP              = (
    D3DSTENCILOP_KEEP        = 1,
    D3DSTENCILOP_ZERO        = 2,
    D3DSTENCILOP_REPLACE     = 3,
    D3DSTENCILOP_INCRSAT     = 4,
    D3DSTENCILOP_DECRSAT     = 5,
    D3DSTENCILOP_INVERT      = 6,
    D3DSTENCILOP_INCR        = 7,
    D3DSTENCILOP_DECR        = 8,
    D3DSTENCILOP_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DSTENCILOP}
  D3DSTENCILOP = _D3DSTENCILOP;
  {$EXTERNALSYM D3DSTENCILOP}


  PD3DFOGMODE = ^D3DFOGMODE;
  _D3DFOGMODE          = (
    D3DFOG_NONE        = 0,
    D3DFOG_EXP         = 1,
    D3DFOG_EXP2        = 2,
    D3DFOG_LINEAR      = 3,
    D3DFOG_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DFOGMODE}
  D3DFOGMODE = _D3DFOGMODE;
  {$EXTERNALSYM D3DFOGMODE}


  PD3DZBUFFERTYPE = ^D3DZBUFFERTYPE;
  _D3DZBUFFERTYPE     = (
    D3DZB_FALSE       = 0,
    D3DZB_TRUE        = 1,             // Z buffering
    D3DZB_USEW        = 2,             // W buffering
    D3DZB_FORCE_DWORD = FORCEDWORD    { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DZBUFFERTYPE}
  D3DZBUFFERTYPE = _D3DZBUFFERTYPE;
  {$EXTERNALSYM D3DZBUFFERTYPE}

  // Primitives supported by draw-primitive API

  PD3DPRIMITIVETYPE = ^D3DPRIMITIVETYPE;
  _D3DPRIMITIVETYPE     = (
    D3DPT_POINTLIST     = 1,
    D3DPT_LINELIST      = 2,
    D3DPT_LINESTRIP     = 3,
    D3DPT_TRIANGLELIST  = 4,
    D3DPT_TRIANGLESTRIP = 5,
    D3DPT_TRIANGLEFAN   = 6,
    D3DPT_FORCE_DWORD   = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DPRIMITIVETYPE}
  D3DPRIMITIVETYPE = _D3DPRIMITIVETYPE;
  {$EXTERNALSYM D3DPRIMITIVETYPE}


type

  PD3DTRANSFORMSTATETYPE = ^D3DTRANSFORMSTATETYPE;

  {$IFDEF TYPE_IDENTITY}
  _D3DTRANSFORMSTATETYPE = DWORD;
  {$EXTERNALSYM _D3DTRANSFORMSTATETYPE}
  {$ELSE}
  _D3DTRANSFORMSTATETYPE = (
    D3DTS_VIEW        = 2,
    D3DTS_PROJECTION  = 3,
    D3DTS_TEXTURE0    = 16,
    D3DTS_TEXTURE1    = 17,
    D3DTS_TEXTURE2    = 18,
    D3DTS_TEXTURE3    = 19,
    D3DTS_TEXTURE4    = 20,
    D3DTS_TEXTURE5    = 21,
    D3DTS_TEXTURE6    = 22,
    D3DTS_TEXTURE7    = 23,
    D3DTS_FORCE_DWORD = FORCEDWORD  { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DTRANSFORMSTATETYPE}
  {$ENDIF}
  D3DTRANSFORMSTATETYPE = _D3DTRANSFORMSTATETYPE;
  {$EXTERNALSYM D3DTRANSFORMSTATETYPE}


  function D3DTS_WORLDMATRIX(index: Int32): D3DTRANSFORMSTATETYPE;   // index value in the range 0 through 255.
  {$EXTERNALSYM D3DTS_WORLDMATRIX}

const

  D3DTS_WORLD  = D3DTRANSFORMSTATETYPE(255);
  {$EXTERNALSYM D3DTS_WORLD}
  D3DTS_WORLD1 = D3DTRANSFORMSTATETYPE(256);
  {$EXTERNALSYM D3DTS_WORLD1}
  D3DTS_WORLD2 = D3DTRANSFORMSTATETYPE(257);
  {$EXTERNALSYM D3DTS_WORLD2}
  D3DTS_WORLD3 = D3DTRANSFORMSTATETYPE(258);
  {$EXTERNALSYM D3DTS_WORLD3}


type

  PD3DRENDERSTATETYPE = ^D3DRENDERSTATETYPE;
  _D3DRENDERSTATETYPE                = (
    D3DRS_ZENABLE                    = 7,              { D3DZBUFFERTYPE (or TRUE/FALSE for legacy) }
    D3DRS_FILLMODE                   = 8,              { D3DFILLMODE }
    D3DRS_SHADEMODE                  = 9,              { D3DSHADEMODE }
    D3DRS_ZWRITEENABLE               = 14,             { TRUE to enable z writes }
    D3DRS_ALPHATESTENABLE            = 15,             { TRUE to enable alpha tests }
    D3DRS_LASTPIXEL                  = 16,             { TRUE for last-pixel on lines }
    D3DRS_SRCBLEND                   = 19,             { D3DBLEND }
    D3DRS_DESTBLEND                  = 20,             { D3DBLEND }
    D3DRS_CULLMODE                   = 22,             { D3DCULL }
    D3DRS_ZFUNC                      = 23,             { D3DCMPFUNC }
    D3DRS_ALPHAREF                   = 24,             { D3DFIXED }
    D3DRS_ALPHAFUNC                  = 25,             { D3DCMPFUNC }
    D3DRS_DITHERENABLE               = 26,             { TRUE to enable dithering }
    D3DRS_ALPHABLENDENABLE           = 27,             { TRUE to enable alpha blending }
    D3DRS_FOGENABLE                  = 28,             { TRUE to enable fog blending }
    D3DRS_SPECULARENABLE             = 29,             { TRUE to enable specular }
    D3DRS_FOGCOLOR                   = 34,             { D3DCOLOR }
    D3DRS_FOGTABLEMODE               = 35,             { D3DFOGMODE }
    D3DRS_FOGSTART                   = 36,             { Fog start (for both vertex and pixel fog) }
    D3DRS_FOGEND                     = 37,             { Fog end      }
    D3DRS_FOGDENSITY                 = 38,             { Fog density  }
    D3DRS_RANGEFOGENABLE             = 48,             { Enables range-based fog }
    D3DRS_STENCILENABLE              = 52,             { BOOL enable/disable stenciling }
    D3DRS_STENCILFAIL                = 53,             { D3DSTENCILOP to do if stencil test fails }
    D3DRS_STENCILZFAIL               = 54,             { D3DSTENCILOP to do if stencil test passes and Z test fails }
    D3DRS_STENCILPASS                = 55,             { D3DSTENCILOP to do if both stencil and Z tests pass }
    D3DRS_STENCILFUNC                = 56,             { D3DCMPFUNC fn.  Stencil Test passes if ((ref  mask) stencilfn (stencil  mask)) is true }
    D3DRS_STENCILREF                 = 57,             { Reference value used in stencil test }
    D3DRS_STENCILMASK                = 58,             { Mask value used in stencil test }
    D3DRS_STENCILWRITEMASK           = 59,             { Write mask applied to values written to stencil buffer }
    D3DRS_TEXTUREFACTOR              = 60,             { D3DCOLOR used for multi-texture blend }
    D3DRS_WRAP0                      = 128,            { wrap for 1st texture coord. set }
    D3DRS_WRAP1                      = 129,            { wrap for 2nd texture coord. set }
    D3DRS_WRAP2                      = 130,            { wrap for 3rd texture coord. set }
    D3DRS_WRAP3                      = 131,            { wrap for 4th texture coord. set }
    D3DRS_WRAP4                      = 132,            { wrap for 5th texture coord. set }
    D3DRS_WRAP5                      = 133,            { wrap for 6th texture coord. set }
    D3DRS_WRAP6                      = 134,            { wrap for 7th texture coord. set }
    D3DRS_WRAP7                      = 135,            { wrap for 8th texture coord. set }
    D3DRS_CLIPPING                   = 136,
    D3DRS_LIGHTING                   = 137,
    D3DRS_AMBIENT                    = 139,
    D3DRS_FOGVERTEXMODE              = 140,
    D3DRS_COLORVERTEX                = 141,
    D3DRS_LOCALVIEWER                = 142,
    D3DRS_NORMALIZENORMALS           = 143,
    D3DRS_DIFFUSEMATERIALSOURCE      = 145,
    D3DRS_SPECULARMATERIALSOURCE     = 146,
    D3DRS_AMBIENTMATERIALSOURCE      = 147,
    D3DRS_EMISSIVEMATERIALSOURCE     = 148,
    D3DRS_VERTEXBLEND                = 151,
    D3DRS_CLIPPLANEENABLE            = 152,
    D3DRS_POINTSIZE                  = 154,            { float point size }
    D3DRS_POINTSIZE_MIN              = 155,            { float point size min threshold }
    D3DRS_POINTSPRITEENABLE          = 156,            { BOOL point texture coord control }
    D3DRS_POINTSCALEENABLE           = 157,            { BOOL point size scale enable }
    D3DRS_POINTSCALE_A               = 158,            { float point attenuation A value }
    D3DRS_POINTSCALE_B               = 159,            { float point attenuation B value }
    D3DRS_POINTSCALE_C               = 160,            { float point attenuation C value }
    D3DRS_MULTISAMPLEANTIALIAS       = 161,            // BOOL - set to do FSAA with multisample buffer
    D3DRS_MULTISAMPLEMASK            = 162,            // DWORD - per-sample enable/disable
    D3DRS_PATCHEDGESTYLE             = 163,            // Sets whether patch edges will use float style tessellation
    D3DRS_DEBUGMONITORTOKEN          = 165,            // DEBUG ONLY - token to debug monitor
    D3DRS_POINTSIZE_MAX              = 166,            { float point size max threshold }
    D3DRS_INDEXEDVERTEXBLENDENABLE   = 167,
    D3DRS_COLORWRITEENABLE           = 168,            // per-channel write enable
    D3DRS_TWEENFACTOR                = 170,            // float tween factor
    D3DRS_BLENDOP                    = 171,            // D3DBLENDOP setting
    D3DRS_POSITIONDEGREE             = 172,            // NPatch position interpolation degree. D3DDEGREE_LINEAR or D3DDEGREE_CUBIC (default)
    D3DRS_NORMALDEGREE               = 173,            // NPatch normal interpolation degree. D3DDEGREE_LINEAR (default) or D3DDEGREE_QUADRATIC
    D3DRS_SCISSORTESTENABLE          = 174,
    D3DRS_SLOPESCALEDEPTHBIAS        = 175,
    D3DRS_ANTIALIASEDLINEENABLE      = 176,
    D3DRS_MINTESSELLATIONLEVEL       = 178,
    D3DRS_MAXTESSELLATIONLEVEL       = 179,
    D3DRS_ADAPTIVETESS_X             = 180,
    D3DRS_ADAPTIVETESS_Y             = 181,
    D3DRS_ADAPTIVETESS_Z             = 182,
    D3DRS_ADAPTIVETESS_W             = 183,
    D3DRS_ENABLEADAPTIVETESSELLATION = 184,
    D3DRS_TWOSIDEDSTENCILMODE        = 185,            { BOOL enable/disable 2 sided stenciling }
    D3DRS_CCW_STENCILFAIL            = 186,            { D3DSTENCILOP to do if ccw stencil test fails }
    D3DRS_CCW_STENCILZFAIL           = 187,            { D3DSTENCILOP to do if ccw stencil test passes and Z test fails }
    D3DRS_CCW_STENCILPASS            = 188,            { D3DSTENCILOP to do if both ccw stencil and Z tests pass }
    D3DRS_CCW_STENCILFUNC            = 189,            { D3DCMPFUNC fn.  ccw Stencil Test passes if ((ref  mask) stencilfn (stencil  mask)) is true }
    D3DRS_COLORWRITEENABLE1          = 190,            { Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS }
    D3DRS_COLORWRITEENABLE2          = 191,            { Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS }
    D3DRS_COLORWRITEENABLE3          = 192,            { Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS }
    D3DRS_BLENDFACTOR                = 193,            { D3DCOLOR used for a constant blend factor during alpha blending for devices that support D3DPBLENDCAPS_BLENDFACTOR }
    D3DRS_SRGBWRITEENABLE            = 194,            { Enable rendertarget writes to be DE-linearized to SRGB (for formats that expose D3DUSAGE_QUERY_SRGBWRITE) }
    D3DRS_DEPTHBIAS                  = 195,
    D3DRS_WRAP8                      = 198,            { Additional wrap states for vs_3_0+ attributes with D3DDECLUSAGE_TEXCOORD }
    D3DRS_WRAP9                      = 199,
    D3DRS_WRAP10                     = 200,
    D3DRS_WRAP11                     = 201,
    D3DRS_WRAP12                     = 202,
    D3DRS_WRAP13                     = 203,
    D3DRS_WRAP14                     = 204,
    D3DRS_WRAP15                     = 205,
    D3DRS_SEPARATEALPHABLENDENABLE   = 206,            { TRUE to enable a separate blending function for the alpha channel }
    D3DRS_SRCBLENDALPHA              = 207,            { SRC blend factor for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE }
    D3DRS_DESTBLENDALPHA             = 208,            { DST blend factor for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE }
    D3DRS_BLENDOPALPHA               = 209,            { Blending operation for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE }
    D3DRS_FORCE_DWORD                = FORCEDWORD      { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DRENDERSTATETYPE}
  D3DRENDERSTATETYPE = _D3DRENDERSTATETYPE;
  {$EXTERNALSYM D3DRENDERSTATETYPE}


const

  // Maximum number of simultaneous render targets D3D supports
  D3D_MAX_SIMULTANEOUS_RENDERTARGETS  = 4;
  {$EXTERNALSYM D3D_MAX_SIMULTANEOUS_RENDERTARGETS}


type

  // Values for material source
  PD3DMATERIALCOLORSOURCE = ^D3DMATERIALCOLORSOURCE;
  _D3DMATERIALCOLORSOURCE = (
    D3DMCS_MATERIAL    = 0,            // Color from material is used
    D3DMCS_COLOR1      = 1,            // Diffuse vertex color is used
    D3DMCS_COLOR2      = 2,            // Specular vertex color is used
    D3DMCS_FORCE_DWORD = FORCEDWORD    // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DMATERIALCOLORSOURCE}
  D3DMATERIALCOLORSOURCE = _D3DMATERIALCOLORSOURCE;
  {$EXTERNALSYM D3DMATERIALCOLORSOURCE}


const

  // Bias to apply to the texture coordinate set to apply a wrap to.
  D3DRENDERSTATE_WRAPBIAS             = LONG(128);
  {$EXTERNALSYM D3DRENDERSTATE_WRAPBIAS}

  //* Flags to construct the WRAP render states */
  D3DWRAP_U                           = $00000001;
  {$EXTERNALSYM D3DWRAP_U}
  D3DWRAP_V                           = $00000002;
  {$EXTERNALSYM D3DWRAP_V}
  D3DWRAP_W                           = $00000004;
  {$EXTERNALSYM D3DWRAP_W}

  //* Flags to construct the WRAP render states for 1D thru 4D texture coordinates */
  D3DWRAPCOORD_0                      = $00000001;  // same as D3DWRAP_U
  {$EXTERNALSYM D3DWRAPCOORD_0}
  D3DWRAPCOORD_1                      = $00000002;  // same as D3DWRAP_V
  {$EXTERNALSYM D3DWRAPCOORD_1}
  D3DWRAPCOORD_2                      = $00000004;  // same as D3DWRAP_W
  {$EXTERNALSYM D3DWRAPCOORD_2}
  D3DWRAPCOORD_3                      = $00000008;
  {$EXTERNALSYM D3DWRAPCOORD_3}

  //* Flags to construct D3DRS_COLORWRITEENABLE */
  D3DCOLORWRITEENABLE_RED             = (1 shl 0);
  {$EXTERNALSYM D3DCOLORWRITEENABLE_RED}
  D3DCOLORWRITEENABLE_GREEN           = (1 shl 1);
  {$EXTERNALSYM D3DCOLORWRITEENABLE_GREEN}
  D3DCOLORWRITEENABLE_BLUE            = (1 shl 2);
  {$EXTERNALSYM D3DCOLORWRITEENABLE_BLUE}
  D3DCOLORWRITEENABLE_ALPHA           = (1 shl 3);
  {$EXTERNALSYM D3DCOLORWRITEENABLE_ALPHA}


type

  // State enumerants for per-stage processing of fixed function pixel processing
  // Two of these affect fixed function vertex processing as well: TEXTURETRANSFORMFLAGS and TEXCOORDINDEX.

  PD3DTEXTURESTAGESTATETYPE = ^_D3DTEXTURESTAGESTATETYPE;
  _D3DTEXTURESTAGESTATETYPE      = (
    D3DTSS_COLOROP               = 1,             { D3DTEXTUREOP - per-stage blending controls for color channels }
    D3DTSS_COLORARG1             = 2,             { D3DTA_* (texture arg) }
    D3DTSS_COLORARG2             = 3,             { D3DTA_* (texture arg) }
    D3DTSS_ALPHAOP               = 4,             { D3DTEXTUREOP - per-stage blending controls for alpha channel }
    D3DTSS_ALPHAARG1             = 5,             { D3DTA_* (texture arg) }
    D3DTSS_ALPHAARG2             = 6,             { D3DTA_* (texture arg) }
    D3DTSS_BUMPENVMAT00          = 7,             { float (bump mapping matrix) }
    D3DTSS_BUMPENVMAT01          = 8,             { float (bump mapping matrix) }
    D3DTSS_BUMPENVMAT10          = 9,             { float (bump mapping matrix) }
    D3DTSS_BUMPENVMAT11          = 10,            { float (bump mapping matrix) }
    D3DTSS_TEXCOORDINDEX         = 11,            { identifies which set of texture coordinates index this texture }
    D3DTSS_BUMPENVLSCALE         = 22,            { float scale for bump map luminance }
    D3DTSS_BUMPENVLOFFSET        = 23,            { float offset for bump map luminance }
    D3DTSS_TEXTURETRANSFORMFLAGS = 24,            { D3DTEXTURETRANSFORMFLAGS controls texture transform }
    D3DTSS_COLORARG0             = 26,            { D3DTA_* third arg for triadic ops }
    D3DTSS_ALPHAARG0             = 27,            { D3DTA_* third arg for triadic ops }
    D3DTSS_RESULTARG             = 28,            { D3DTA_* arg for result (CURRENT or TEMP) }
    D3DTSS_CONSTANT              = 32,            { Per-stage constant D3DTA_CONSTANT }
    D3DTSS_FORCE_DWORD           = FORCEDWORD     { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DTEXTURESTAGESTATETYPE}
  D3DTEXTURESTAGESTATETYPE = _D3DTEXTURESTAGESTATETYPE;
  {$EXTERNALSYM D3DTEXTURESTAGESTATETYPE}


  // State enumerants for per-sampler texture processing.

  PD3DSAMPLERSTATETYPE = ^_D3DSAMPLERSTATETYPE;
  _D3DSAMPLERSTATETYPE                             = (
    D3DSAMP_ADDRESSU                               = 1,           { D3DTEXTUREADDRESS for U coordinate }
    D3DSAMP_ADDRESSV                               = 2,           { D3DTEXTUREADDRESS for V coordinate }
    D3DSAMP_ADDRESSW                               = 3,           { D3DTEXTUREADDRESS for W coordinate }
    D3DSAMP_BORDERCOLOR                            = 4,           { D3DCOLOR }
    D3DSAMP_MAGFILTER                              = 5,           { D3DTEXTUREFILTER filter to use for magnification }
    D3DSAMP_MINFILTER                              = 6,           { D3DTEXTUREFILTER filter to use for minification }
    D3DSAMP_MIPFILTER                              = 7,           { D3DTEXTUREFILTER filter to use between mipmaps during minification }
    D3DSAMP_MIPMAPLODBIAS                          = 8,           { float Mipmap LOD bias }
    D3DSAMP_MAXMIPLEVEL                            = 9,           { DWORD 0..(n-1) LOD index of largest map to use (0 == largest) }
    D3DSAMP_MAXANISOTROPY                          = 10,          { DWORD maximum anisotropy }
    D3DSAMP_SRGBTEXTURE                            = 11,          { Default = 0 (which means Gamma 1.0, no correction required.) else correct for
                                                                    Gamma = 2.2 }

    D3DSAMP_ELEMENTINDEX                           = 12,          { When multi-element texture is assigned to sampler, this
                                                                    indicates which element index to use.  Default = 0.0 }
    D3DSAMP_DMAPOFFSET                             = 13,          { Offset in vertices in the pre-sampled displacement map.
                                                                    Only valid for D3DDMAPSAMPLER sampler  }
    D3DSAMP_FORCE_DWORD                            = FORCEDWORD   { force 32-bit size enum }
  );
  {$EXTERNALSYM _D3DSAMPLERSTATETYPE}
  D3DSAMPLERSTATETYPE = _D3DSAMPLERSTATETYPE;
  {$EXTERNALSYM D3DSAMPLERSTATETYPE}


const

  // Special sampler which is used in the tesselator */
  D3DDMAPSAMPLER                      = 256;
  {$EXTERNALSYM D3DDMAPSAMPLER}

  // Samplers used in vertex shaders
  D3DVERTEXTEXTURESAMPLER0            = (D3DDMAPSAMPLER + 1);
  {$EXTERNALSYM D3DVERTEXTEXTURESAMPLER0}
  D3DVERTEXTEXTURESAMPLER1            = (D3DDMAPSAMPLER + 2);
  {$EXTERNALSYM D3DVERTEXTEXTURESAMPLER1}
  D3DVERTEXTEXTURESAMPLER2            = (D3DDMAPSAMPLER + 3);
  {$EXTERNALSYM D3DVERTEXTEXTURESAMPLER2}
  D3DVERTEXTEXTURESAMPLER3            = (D3DDMAPSAMPLER + 4);
  {$EXTERNALSYM D3DVERTEXTEXTURESAMPLER3}

  // Values, used with D3DTSS_TEXCOORDINDEX, to specify that the vertex data(position
  // and normal in the camera space) should be taken as texture coordinates
  // Low 16 bits are used to specify texture coordinate index, to take the WRAP mode from
  //
  D3DTSS_TCI_PASSTHRU                    = $00000000;
  {$EXTERNALSYM D3DTSS_TCI_PASSTHRU}
  D3DTSS_TCI_CAMERASPACENORMAL           = $00010000;
  {$EXTERNALSYM D3DTSS_TCI_CAMERASPACENORMAL}
  D3DTSS_TCI_CAMERASPACEPOSITION         = $00020000;
  {$EXTERNALSYM D3DTSS_TCI_CAMERASPACEPOSITION}
  D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR = $00030000;
  {$EXTERNALSYM D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR}
  D3DTSS_TCI_SPHEREMAP                   = $00040000;
  {$EXTERNALSYM D3DTSS_TCI_SPHEREMAP}


type

  // Enumerations for COLOROP and ALPHAOP texture blending operations set in
  // texture processing stage controls in D3DTSS.

  PD3DTEXTUREOP = ^_D3DTEXTUREOP;
  _D3DTEXTUREOP                      = (
    // Control
    D3DTOP_DISABLE                   = 1,              // disables stage
    D3DTOP_SELECTARG1                = 2,              // the default
    D3DTOP_SELECTARG2                = 3,
    // Modulate
    D3DTOP_MODULATE                  = 4,              // multiply args together
    D3DTOP_MODULATE2X                = 5,              // multiply and  1 bit
    D3DTOP_MODULATE4X                = 6,              // multiply and  2 bits
    // Add
    D3DTOP_ADD                       = 7,              // add arguments together
    D3DTOP_ADDSIGNED                 = 8,              // add with -0.5 bias
    D3DTOP_ADDSIGNED2X               = 9,              // as above but left  1 bit
    D3DTOP_SUBTRACT                  = 10,             // Arg1 - Arg2, with no saturation
    D3DTOP_ADDSMOOTH                 = 11,             // add 2 args, subtract product
                                                       // Arg1 + Arg2 - Arg1*Arg2
                                                       //  = Arg1 + (1-Arg1)*Arg2
    // Linear alpha blend: Arg1*(Alpha) + Arg2*(1-Alpha)
    D3DTOP_BLENDDIFFUSEALPHA         = 12,             // iterated alpha
    D3DTOP_BLENDTEXTUREALPHA         = 13,             // texture alpha
    D3DTOP_BLENDFACTORALPHA          = 14,             // alpha from D3DRS_TEXTUREFACTOR
    // Linear alpha blend with pre-multiplied arg1 input: Arg1 + Arg2*(1-Alpha)
    D3DTOP_BLENDTEXTUREALPHAPM       = 15,             // texture alpha
    D3DTOP_BLENDCURRENTALPHA         = 16,             // by alpha of current color
    // Specular mapping
    D3DTOP_PREMODULATE               = 17,             // modulate with next texture before use
    D3DTOP_MODULATEALPHA_ADDCOLOR    = 18,             // Arg1.RGB + Arg1.A*Arg2.RGB
                                                       // COLOROP only
    D3DTOP_MODULATECOLOR_ADDALPHA    = 19,             // Arg1.RGB*Arg2.RGB + Arg1.A
                                                       // COLOROP only
    D3DTOP_MODULATEINVALPHA_ADDCOLOR = 20,             // (1-Arg1.A)*Arg2.RGB + Arg1.RGB
                                                       // COLOROP only
    D3DTOP_MODULATEINVCOLOR_ADDALPHA = 21,             // (1-Arg1.RGB)*Arg2.RGB + Arg1.A
                                                       // COLOROP only

    // Bump mapping
    D3DTOP_BUMPENVMAP                = 22,             // per pixel env map perturbation
    D3DTOP_BUMPENVMAPLUMINANCE       = 23,             // with luminance channel
    // This can do either diffuse or specular bump mapping with correct input.
    // Performs the function (Arg1.R*Arg2.R + Arg1.G*Arg2.G + Arg1.B*Arg2.B)
    // where each component has been scaled and offset to make it signed.
    // The result is replicated into all four (including alpha) channels.
    // This is a valid COLOROP only.
    D3DTOP_DOTPRODUCT3               = 24,             // Triadic ops
    D3DTOP_MULTIPLYADD               = 25,             // Arg0 + Arg1*Arg2
    D3DTOP_LERP                      = 26,             // (Arg0)*Arg1 + (1-Arg0)*Arg2
    D3DTOP_FORCE_DWORD               = FORCEDWORD);
  {$EXTERNALSYM _D3DTEXTUREOP}
  D3DTEXTUREOP = _D3DTEXTUREOP;
  {$EXTERNALSYM D3DTEXTUREOP}


const
  // Values for COLORARG0,1,2, ALPHAARG0,1,2, and RESULTARG texture blending
  // operations set in texture processing stage controls in D3DRENDERSTATE.

  D3DTA_SELECTMASK                    = $0000000F;  // mask for arg selector
  {$EXTERNALSYM D3DTA_SELECTMASK}
  D3DTA_DIFFUSE                       = $00000000;  // select diffuse color (read only)
  {$EXTERNALSYM D3DTA_DIFFUSE}
  D3DTA_CURRENT                       = $00000001;  // select stage destination register (read/write)
  {$EXTERNALSYM D3DTA_CURRENT}
  D3DTA_TEXTURE                       = $00000002;  // select texture color (read only)
  {$EXTERNALSYM D3DTA_TEXTURE}
  D3DTA_TFACTOR                       = $00000003;  // select D3DRS_TEXTUREFACTOR (read only)
  {$EXTERNALSYM D3DTA_TFACTOR}
  D3DTA_SPECULAR                      = $00000004;  // select specular color (read only)
  {$EXTERNALSYM D3DTA_SPECULAR}
  D3DTA_TEMP                          = $00000005;  // select temporary register color (read/write)
  {$EXTERNALSYM D3DTA_TEMP}
  D3DTA_CONSTANT                      = $00000006;  // select texture stage constant
  {$EXTERNALSYM D3DTA_CONSTANT}
  D3DTA_COMPLEMENT                    = $00000010;  // take 1.0 - x (read modifier)
  {$EXTERNALSYM D3DTA_COMPLEMENT}
  D3DTA_ALPHAREPLICATE                = $00000020;  // replicate alpha to color components (read modifier)
  {$EXTERNALSYM D3DTA_ALPHAREPLICATE}


type
  //
  // Values for D3DSAMP_***FILTER texture stage states
  //
  PD3DTEXTUREFILTERTYPE = ^_D3DTEXTUREFILTERTYPE;
  _D3DTEXTUREFILTERTYPE     = (
    D3DTEXF_NONE            = 0,               // filtering disabled (valid for mip filter only)
    D3DTEXF_POINT           = 1,               // nearest
    D3DTEXF_LINEAR          = 2,               // linear interpolation
    D3DTEXF_ANISOTROPIC     = 3,               // anisotropic
    D3DTEXF_PYRAMIDALQUAD   = 6,               // 4-sample tent
    D3DTEXF_GAUSSIANQUAD    = 7,               // 4-sample gaussian
    // D3D9Ex only -- */
    //{$IFDEF D3D_DISABLE_9EX}
    D3DTEXF_CONVOLUTIONMONO = 8,               // Convolution filter for monochrome textures
    //{$ENDIF} // !D3D_DISABLE_9EX
    //* -- D3D9Ex only */
    D3DTEXF_FORCE_DWORD     = FORCEDWORD       // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DTEXTUREFILTERTYPE}
  D3DTEXTUREFILTERTYPE = _D3DTEXTUREFILTERTYPE;
  {$EXTERNALSYM D3DTEXTUREFILTERTYPE}


const

  //* Bits for Flags in ProcessVertices call */

  D3DPV_DONOTCOPYDATA                 = (1 shl 0);
  {$EXTERNALSYM D3DPV_DONOTCOPYDATA}

  //-------------------------------------------------------------------

  // Flexible vertex format bits
  //
  D3DFVF_RESERVED0                    = $001;
  {$EXTERNALSYM D3DFVF_RESERVED0}
  D3DFVF_POSITION_MASK                = $400E;
  {$EXTERNALSYM D3DFVF_POSITION_MASK}
  D3DFVF_XYZ                          = $002;
  {$EXTERNALSYM D3DFVF_XYZ}
  D3DFVF_XYZRHW                       = $004;
  {$EXTERNALSYM D3DFVF_XYZRHW}
  D3DFVF_XYZB1                        = $006;
  {$EXTERNALSYM D3DFVF_XYZB1}
  D3DFVF_XYZB2                        = $008;
  {$EXTERNALSYM D3DFVF_XYZB2}
  D3DFVF_XYZB3                        = $00A;
  {$EXTERNALSYM D3DFVF_XYZB3}
  D3DFVF_XYZB4                        = $00C;
  {$EXTERNALSYM D3DFVF_XYZB4}
  D3DFVF_XYZB5                        = $00E;
  {$EXTERNALSYM D3DFVF_XYZB5}
  D3DFVF_XYZW                         = $4002;
  {$EXTERNALSYM D3DFVF_XYZW}

  D3DFVF_NORMAL                       = $010;
  {$EXTERNALSYM D3DFVF_NORMAL}
  D3DFVF_PSIZE                        = $020;
  {$EXTERNALSYM D3DFVF_PSIZE}
  D3DFVF_DIFFUSE                      = $040;
  {$EXTERNALSYM D3DFVF_DIFFUSE}
  D3DFVF_SPECULAR                     = $080;
  {$EXTERNALSYM D3DFVF_SPECULAR}

  D3DFVF_TEXCOUNT_MASK                = $F00;
  {$EXTERNALSYM D3DFVF_TEXCOUNT_MASK}
  D3DFVF_TEXCOUNT_SHIFT               = 8;
  {$EXTERNALSYM D3DFVF_TEXCOUNT_SHIFT}
  D3DFVF_TEX0                         = $000;
  {$EXTERNALSYM D3DFVF_TEX0}
  D3DFVF_TEX1                         = $100;
  {$EXTERNALSYM D3DFVF_TEX1}
  D3DFVF_TEX2                         = $200;
  {$EXTERNALSYM D3DFVF_TEX2}
  D3DFVF_TEX3                         = $300;
  {$EXTERNALSYM D3DFVF_TEX3}
  D3DFVF_TEX4                         = $400;
  {$EXTERNALSYM D3DFVF_TEX4}
  D3DFVF_TEX5                         = $500;
  {$EXTERNALSYM D3DFVF_TEX5}
  D3DFVF_TEX6                         = $600;
  {$EXTERNALSYM D3DFVF_TEX6}
  D3DFVF_TEX7                         = $700;
  {$EXTERNALSYM D3DFVF_TEX7}
  D3DFVF_TEX8                         = $800;
  {$EXTERNALSYM D3DFVF_TEX8}

  D3DFVF_LASTBETA_UBYTE4              = $1000;
  {$EXTERNALSYM D3DFVF_LASTBETA_UBYTE4}
  D3DFVF_LASTBETA_D3DCOLOR            = $8000;
  {$EXTERNALSYM D3DFVF_LASTBETA_D3DCOLOR}

  D3DFVF_RESERVED2                    = $6000;  // 2 reserved bits
  {$EXTERNALSYM D3DFVF_RESERVED2}



type
  //---------------------------------------------------------------------
  // Vertex Shaders
  //

  // Vertex shader declaration

  // Vertex element semantics
  //

  PD3DDECLUSAGE = ^D3DDECLUSAGE;
  _D3DDECLUSAGE           = (
    D3DDECLUSAGE_POSITION = 0,
    D3DDECLUSAGE_BLENDWEIGHT,   // 1
    D3DDECLUSAGE_BLENDINDICES,  // 2
    D3DDECLUSAGE_NORMAL,        // 3
    D3DDECLUSAGE_PSIZE,         // 4
    D3DDECLUSAGE_TEXCOORD,      // 5
    D3DDECLUSAGE_TANGENT,       // 6
    D3DDECLUSAGE_BINORMAL,      // 7
    D3DDECLUSAGE_TESSFACTOR,    // 8
    D3DDECLUSAGE_POSITIONT,     // 9
    D3DDECLUSAGE_COLOR,         // 10
    D3DDECLUSAGE_FOG,           // 11
    D3DDECLUSAGE_DEPTH,         // 12
    D3DDECLUSAGE_SAMPLE         // 13
  );
  {$EXTERNALSYM _D3DDECLUSAGE}
  D3DDECLUSAGE = _D3DDECLUSAGE;
  {$EXTERNALSYM D3DDECLUSAGE}


const

  {$EXTERNALSYM MAXD3DDECLUSAGE}
  MAXD3DDECLUSAGE                     = D3DDECLUSAGE_SAMPLE;
  {$EXTERNALSYM MAXD3DDECLUSAGEINDEX}
  MAXD3DDECLUSAGEINDEX                = 15;
  {$EXTERNALSYM MAXD3DDECLLENGTH}
  MAXD3DDECLLENGTH                    = 64;  // does not include "end" marker vertex element



type

  PD3DDECLMETHOD = ^D3DDECLMETHOD;
  {$EXTERNALSYM _D3DDECLMETHOD}
  _D3DDECLMETHOD          = (
    D3DDECLMETHOD_DEFAULT = 0,
    D3DDECLMETHOD_PARTIALU,
    D3DDECLMETHOD_PARTIALV,
    D3DDECLMETHOD_CROSSUV,           // Normal
    D3DDECLMETHOD_UV,
    D3DDECLMETHOD_LOOKUP,            // Lookup a displacement map
    D3DDECLMETHOD_LOOKUPPRESAMPLED   // Lookup a pre-sampled displacement map
  );
  {$EXTERNALSYM D3DDECLMETHOD}
  D3DDECLMETHOD = _D3DDECLMETHOD;


const

  MAXD3DDECLMETHOD                    = D3DDECLMETHOD_LOOKUPPRESAMPLED;
  {$EXTERNALSYM MAXD3DDECLMETHOD}

  // Declarations for _Type fields
  //

type

  PD3DDECLTYPE = ^D3DDECLTYPE;
  _D3DDECLTYPE                                                 = (
    D3DDECLTYPE_FLOAT1                                         = 0,         // 1D float expanded to (value, 0., 0., 1.)
    D3DDECLTYPE_FLOAT2                                         = 1,         // 2D float expanded to (value, value, 0., 1.)
    D3DDECLTYPE_FLOAT3                                         = 2,         // 3D float expanded to (value, value, value, 1.)
    D3DDECLTYPE_FLOAT4                                         = 3,         // 4D float
    D3DDECLTYPE_D3DCOLOR                                       = 4,         // 4D packed unsigned bytes mapped to 0. to 1. range
    // Input is in D3DCOLOR format (ARGB) expanded to (R, G, B, A)
    D3DDECLTYPE_UBYTE4                                         = 5,         // 4D unsigned byte
    D3DDECLTYPE_SHORT2                                         = 6,         // 2D signed short expanded to (value, value, 0., 1.)
    D3DDECLTYPE_SHORT4                                         = 7,         // 4D signed short
    // The following types are valid only with vertex shaders >= 2.0
    D3DDECLTYPE_UBYTE4N                                        = 8,         // Each of 4 bytes is normalized by dividing to 255.0
    D3DDECLTYPE_SHORT2N                                        = 9,         // 2D signed short normalized (v[0]/32767.0,v[1]/32767.0,0,1)
    D3DDECLTYPE_SHORT4N                                        = 10,        // 4D signed short normalized (v[0]/32767.0,v[1]/32767.0,v[2]/32767.0,v[3]/32767.0)
    D3DDECLTYPE_USHORT2N                                       = 11,        // 2D unsigned short normalized (v[0]/65535.0,v[1]/65535.0,0,1)
    D3DDECLTYPE_USHORT4N                                       = 12,        // 4D unsigned short normalized (v[0]/65535.0,v[1]/65535.0,v[2]/65535.0,v[3]/65535.0)
    D3DDECLTYPE_UDEC3                                          = 13,        // 3D unsigned 10 10 10 format expanded to (value, value, value, 1)
    D3DDECLTYPE_DEC3N                                          = 14,        // 3D signed 10 10 10 format normalized and expanded to (v[0]/511.0, v[1]/511.0, v[2]/511.0, 1)
    D3DDECLTYPE_FLOAT16_2                                      = 15,        // Two 16-bit floating point values, expanded to (value, value, 0, 1)
    D3DDECLTYPE_FLOAT16_4                                      = 16,        // Four 16-bit floating point values
    D3DDECLTYPE_UNUSED                                         = 17         // When the type field in a decl is unused.
  );
  {$EXTERNALSYM _D3DDECLTYPE}
  D3DDECLTYPE = _D3DDECLTYPE;
  {$EXTERNALSYM D3DDECLTYPE}


const

  {$EXTERNALSYM MAXD3DDECLTYPE}
  MAXD3DDECLTYPE                      = D3DDECLTYPE_UNUSED;

type

  PD3DVERTEXELEMENT9 = ^D3DVERTEXELEMENT9;
  _D3DVERTEXELEMENT9 = record
    Stream: WORD;                    // Stream index
    Offset: WORD;                    // Offset in the stream in bytes
    _Type: Byte;                     // Data type
    Method: Byte;                    // Processing method
    Usage: Byte;                     // Semantics
    UsageIndex: Byte;                // Semantic index
  end;
  {$EXTERNALSYM _D3DVERTEXELEMENT9}
  D3DVERTEXELEMENT9 = _D3DVERTEXELEMENT9;
  {$EXTERNALSYM D3DVERTEXELEMENT9}


const
  // This is used to initialize the last vertex element in a vertex declaration
  // array
  //
  //#define D3DDECL_END() {0xFF,0,D3DDECLTYPE_UNUSED,0,0,0}

  // Maximum supported number of texture coordinate sets

  D3DDP_MAXTEXCOORD                   = 8;
  {$EXTERNALSYM D3DDP_MAXTEXCOORD}

  //---------------------------------------------------------------------
  // Values for IDirect3DDevice9::SetStreamSourceFreq's Setting parameter
  //---------------------------------------------------------------------
  D3DSTREAMSOURCE_INDEXEDDATA         = (1 shl 30);
  {$EXTERNALSYM D3DSTREAMSOURCE_INDEXEDDATA}
  D3DSTREAMSOURCE_INSTANCEDATA        = (2 shl 30);
  {$EXTERNALSYM D3DSTREAMSOURCE_INSTANCEDATA}



  //---------------------------------------------------------------------
  //
  // The internal format of Pixel Shader (PS) & Vertex Shader (VS)
  // Instruction Tokens is defined in the Direct3D Device Driver Kit
  //
  //---------------------------------------------------------------------

  //
  // Instruction Token Bit Definitions
  //

  D3DSI_OPCODE_MASK                   = $0000FFFF;
  {$EXTERNALSYM D3DSI_OPCODE_MASK}
  D3DSI_INSTLENGTH_MASK               = $0F000000;
  {$EXTERNALSYM D3DSI_INSTLENGTH_MASK}
  D3DSI_INSTLENGTH_SHIFT              = 24;
  {$EXTERNALSYM D3DSI_INSTLENGTH_SHIFT}

type

  PD3DSHADER_INSTRUCTION_OPCODE_TYPE = ^D3DSHADER_INSTRUCTION_OPCODE_TYPE;
  _D3DSHADER_INSTRUCTION_OPCODE_TYPE = (
    D3DSIO_NOP          =  0,
    D3DSIO_MOV          =  1,
    D3DSIO_ADD          =  2,
    D3DSIO_SUB          =  3,
    D3DSIO_MAD          =  4,
    D3DSIO_MUL          =  5,
    D3DSIO_RCP          =  6,
    D3DSIO_RSQ          =  7,
    D3DSIO_DP3          =  8,
    D3DSIO_DP4          =  9,
    D3DSIO_MIN          =  10,
    D3DSIO_MAX          =  11,
    D3DSIO_SLT          =  12,
    D3DSIO_SGE          =  13,
    D3DSIO_EXP          =  14,
    D3DSIO_LOG          =  15,
    D3DSIO_LIT          =  16,
    D3DSIO_DST          =  17,
    D3DSIO_LRP          =  18,
    D3DSIO_FRC          =  19,
    D3DSIO_M4x4         =  20,
    D3DSIO_M4x3         =  21,
    D3DSIO_M3x4         =  22,
    D3DSIO_M3x3         =  23,
    D3DSIO_M3x2         =  24,
    D3DSIO_CALL         =  25,
    D3DSIO_CALLNZ       =  26,
    D3DSIO_LOOP         =  27,
    D3DSIO_RET          =  28,
    D3DSIO_ENDLOOP      =  29,
    D3DSIO_LABEL        =  30,
    D3DSIO_DCL          =  31,
    D3DSIO_POW          =  32,
    D3DSIO_CRS          =  33,
    D3DSIO_SGN          =  34,
    D3DSIO_ABS          =  35,
    D3DSIO_NRM          =  36,
    D3DSIO_SINCOS       =  37,
    D3DSIO_REP          =  38,
    D3DSIO_ENDREP       =  39,
    D3DSIO_IF           =  40,
    D3DSIO_IFC          =  41,
    D3DSIO_ELSE         =  42,
    D3DSIO_ENDIF        =  43,
    D3DSIO_BREAK        =  44,
    D3DSIO_BREAKC       =  45,
    D3DSIO_MOVA         =  46,
    D3DSIO_DEFB         =  47,
    D3DSIO_DEFI         =  48,
    D3DSIO_TEXCOORD     =  64,
    D3DSIO_TEXKILL      =  65,
    D3DSIO_TEX          =  66,
    D3DSIO_TEXBEM       =  67,
    D3DSIO_TEXBEML      =  68,
    D3DSIO_TEXREG2AR    =  69,
    D3DSIO_TEXREG2GB    =  70,
    D3DSIO_TEXM3x2PAD   =  71,
    D3DSIO_TEXM3x2TEX   =  72,
    D3DSIO_TEXM3x3PAD   =  73,
    D3DSIO_TEXM3x3TEX   =  74,
    D3DSIO_RESERVED0    =  75,
    D3DSIO_TEXM3x3SPEC  =  76,
    D3DSIO_TEXM3x3VSPEC =  77,
    D3DSIO_EXPP         =  78,
    D3DSIO_LOGP         =  79,
    D3DSIO_CND          =  80,
    D3DSIO_DEF          =  81,
    D3DSIO_TEXREG2RGB   =  82,
    D3DSIO_TEXDP3TEX    =  83,
    D3DSIO_TEXM3x2DEPTH =  84,
    D3DSIO_TEXDP3       =  85,
    D3DSIO_TEXM3x3      =  86,
    D3DSIO_TEXDEPTH     =  87,
    D3DSIO_CMP          =  88,
    D3DSIO_BEM          =  89,
    D3DSIO_DP2ADD       =  90,
    D3DSIO_DSX          =  91,
    D3DSIO_DSY          =  92,
    D3DSIO_TEXLDD       =  93,
    D3DSIO_SETP         =  94,
    D3DSIO_TEXLDL       =  95,
    D3DSIO_BREAKP       =  96,
    D3DSIO_PHASE        = $FFFD,
    D3DSIO_COMMENT      = $FFFE,
    D3DSIO_END          = $FFFF,
    D3DSIO_FORCE_DWORD  = FORCEDWORD    // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DSHADER_INSTRUCTION_OPCODE_TYPE}
  D3DSHADER_INSTRUCTION_OPCODE_TYPE =  _D3DSHADER_INSTRUCTION_OPCODE_TYPE;
  {$EXTERNALSYM D3DSHADER_INSTRUCTION_OPCODE_TYPE}

const

  //---------------------------------------------------------------------
  // Use these constants with D3DSIO_SINCOS macro as SRC2, SRC3
  //
  //#define D3DSINCOSCONST1 -1.5500992e-006f, -2.1701389e-005f,  0.0026041667f, 0.00026041668f
  //#define D3DSINCOSCONST2 -0.020833334f, -0.12500000f, 1.0f, 0.50000000f

  //---------------------------------------------------------------------
  // Co-Issue Instruction Modifier - if set then this instruction is to be
  // issued in parallel with the previous instruction(s) for which this bit
  // is not set.
  //
  D3DSI_COISSUE         = $40000000;
  {$EXTERNALSYM D3DSI_COISSUE}

  //---------------------------------------------------------------------
  // Opcode specific controls
  D3DSP_OPCODESPECIFICCONTROL_MASK    = $00FF0000;
  {$EXTERNALSYM D3DSP_OPCODESPECIFICCONTROL_MASK}
  D3DSP_OPCODESPECIFICCONTROL_SHIFT   = 16;
  {$EXTERNALSYM D3DSP_OPCODESPECIFICCONTROL_SHIFT}

  // ps_2_0 texld controls
  D3DSI_TEXLD_PROJECT                 = ($01 shl D3DSP_OPCODESPECIFICCONTROL_SHIFT);
  {$EXTERNALSYM D3DSI_TEXLD_PROJECT}
  D3DSI_TEXLD_BIAS                    = ($02 shl D3DSP_OPCODESPECIFICCONTROL_SHIFT);
  {$EXTERNALSYM D3DSI_TEXLD_BIAS}

type

  // Comparison for dynamic conditional instruction opcodes (i.e. if, breakc)
  PD3DSHADER_COMPARISON = ^D3DSHADER_COMPARISON;
  _D3DSHADER_COMPARISON = (            // <             = >
    D3DSPC_RESERVED0 = 0,              // 0 0 0
    D3DSPC_GT        = 1,              // 0 0 1
    D3DSPC_EQ        = 2,              // 0 1 0
    D3DSPC_GE        = 3,              // 0 1 1
    D3DSPC_LT        = 4,              // 1 0 0
    D3DSPC_NE        = 5,              // 1 0 1
    D3DSPC_LE        = 6,              // 1 1 0
    D3DSPC_RESERVED1 = 7               // 1 1 1
  );
  {$EXTERNALSYM _D3DSHADER_COMPARISON}
  D3DSHADER_COMPARISON = _D3DSHADER_COMPARISON;
  {$EXTERNALSYM D3DSHADER_COMPARISON}

const

  // Comparison is part of instruction opcode token:
  D3DSHADER_COMPARISON_SHIFT          = D3DSP_OPCODESPECIFICCONTROL_SHIFT;
  {$EXTERNALSYM D3DSHADER_COMPARISON_SHIFT}
  D3DSHADER_COMPARISON_MASK           = ($7 shl D3DSHADER_COMPARISON_SHIFT);
  {$EXTERNALSYM D3DSHADER_COMPARISON_MASK}

  //---------------------------------------------------------------------
  // Predication flags on instruction token
  D3DSHADER_INSTRUCTION_PREDICATED    = ($1 shl 28);
  {$EXTERNALSYM D3DSHADER_INSTRUCTION_PREDICATED}

  //---------------------------------------------------------------------
  // DCL Info Token Controls

  // For dcl info tokens requiring a semantic (usage + index)
  D3DSP_DCL_USAGE_SHIFT               = 0;
  {$EXTERNALSYM D3DSP_DCL_USAGE_SHIFT}
  D3DSP_DCL_USAGE_MASK                = $0000000F;
  {$EXTERNALSYM D3DSP_DCL_USAGE_MASK}

  D3DSP_DCL_USAGEINDEX_SHIFT          = 16;
  {$EXTERNALSYM D3DSP_DCL_USAGEINDEX_SHIFT}
  D3DSP_DCL_USAGEINDEX_MASK           = $000F0000;
  {$EXTERNALSYM D3DSP_DCL_USAGEINDEX_MASK}

  // DCL pixel shader sampler info token.
  D3DSP_TEXTURETYPE_SHIFT             = 27;
  {$EXTERNALSYM D3DSP_TEXTURETYPE_SHIFT}
  D3DSP_TEXTURETYPE_MASK              = $78000000;
  {$EXTERNALSYM D3DSP_TEXTURETYPE_MASK}


type

  PD3DSAMPLER_TEXTURE_TYPE = ^D3DSAMPLER_TEXTURE_TYPE;
  _D3DSAMPLER_TEXTURE_TYPE = (
    D3DSTT_UNKNOWN     = 0  shl D3DSP_TEXTURETYPE_SHIFT,  // uninitialized value
    D3DSTT_2D          = 2  shl D3DSP_TEXTURETYPE_SHIFT,  // dcl_2d s# (for declaring a 2-D texture)
    D3DSTT_CUBE        = 3  shl D3DSP_TEXTURETYPE_SHIFT,  // dcl_cube s# (for declaring a cube texture)
    D3DSTT_VOLUME      = 4  shl D3DSP_TEXTURETYPE_SHIFT,  // dcl_volume s# (for declaring a volume texture)
    D3DSTT_FORCE_DWORD = FORCEDWORD                       // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DSAMPLER_TEXTURE_TYPE}
  D3DSAMPLER_TEXTURE_TYPE = _D3DSAMPLER_TEXTURE_TYPE;
  {$EXTERNALSYM D3DSAMPLER_TEXTURE_TYPE}


const

  //---------------------------------------------------------------------
  // Parameter Token Bit Definitions
  //
  D3DSP_REGNUM_MASK                   = $000007FF;
  {$EXTERNALSYM D3DSP_REGNUM_MASK}

  // destination parameter write mask
  D3DSP_WRITEMASK_0                   = $00010000;  // Component 0 (X;Red)
  {$EXTERNALSYM D3DSP_WRITEMASK_0}
  D3DSP_WRITEMASK_1                   = $00020000;  // Component 1 (Y;Green)
  {$EXTERNALSYM D3DSP_WRITEMASK_1}
  D3DSP_WRITEMASK_2                   = $00040000;  // Component 2 (Z;Blue)
  {$EXTERNALSYM D3DSP_WRITEMASK_2}
  D3DSP_WRITEMASK_3                   = $00080000;  // Component 3 (W;Alpha)
  {$EXTERNALSYM D3DSP_WRITEMASK_3}
  D3DSP_WRITEMASK_ALL                 = $000F0000;  // All Components
  {$EXTERNALSYM D3DSP_WRITEMASK_ALL}

  // destination parameter modifiers
  D3DSP_DSTMOD_SHIFT                  = 20;
  {$EXTERNALSYM D3DSP_DSTMOD_SHIFT}
  D3DSP_DSTMOD_MASK                   = $00F00000;
  {$EXTERNALSYM D3DSP_DSTMOD_MASK}

  // Bit masks for destination parameter modifiers
  D3DSPDM_NONE                        = (0 shl D3DSP_DSTMOD_SHIFT);  // nop
  {$EXTERNALSYM D3DSPDM_NONE}
  D3DSPDM_SATURATE                    = (1 shl D3DSP_DSTMOD_SHIFT);  // clamp to 0. to 1. range
  {$EXTERNALSYM D3DSPDM_SATURATE}
  D3DSPDM_PARTIALPRECISION            = (2 shl D3DSP_DSTMOD_SHIFT);  // Partial precision hint
  {$EXTERNALSYM D3DSPDM_PARTIALPRECISION}
  D3DSPDM_MSAMPCENTROID               = (4 shl D3DSP_DSTMOD_SHIFT);  // Relevant to multisampling only:
  {$EXTERNALSYM D3DSPDM_MSAMPCENTROID}                               // When the pixel center is not covered, sample
                                                                     //      attribute or compute gradients/LOD
                                                                     //      using multisample "centroid" location.
                                                                     //      "Centroid" is some location within the covered
                                                                     //      region of the pixel.

  // destination parameter
  D3DSP_DSTSHIFT_SHIFT                = 24;
  {$EXTERNALSYM D3DSP_DSTSHIFT_SHIFT}
  D3DSP_DSTSHIFT_MASK                 = $0F000000;
  {$EXTERNALSYM D3DSP_DSTSHIFT_MASK}

  // destination/source parameter register type
  D3DSP_REGTYPE_SHIFT                 = 28;
  {$EXTERNALSYM D3DSP_REGTYPE_SHIFT}
  D3DSP_REGTYPE_SHIFT2                = 8;
  {$EXTERNALSYM D3DSP_REGTYPE_SHIFT2}
  D3DSP_REGTYPE_MASK                  = $70000000;
  {$EXTERNALSYM D3DSP_REGTYPE_MASK}
  D3DSP_REGTYPE_MASK2                 = $00001800;
  {$EXTERNALSYM D3DSP_REGTYPE_MASK2}


type

  PD3DSHADER_PARAM_REGISTER_TYPE = ^D3DSHADER_PARAM_REGISTER_TYPE;
  _D3DSHADER_PARAM_REGISTER_TYPE = (
    D3DSPR_TEMP        = 0,                // Temporary Register File
    D3DSPR_INPUT       = 1,                // Input Register File
    D3DSPR_CONST       = 2,                // Constant Register File
    D3DSPR_ADDR        = 3,                // Address Register (VS)
    D3DSPR_TEXTURE     = 3,                // Texture Register File (PS)
    D3DSPR_RASTOUT     = 4,                // Rasterizer Register File
    D3DSPR_ATTROUT     = 5,                // Attribute Output Register File
    D3DSPR_TEXCRDOUT   = 6,                // Texture Coordinate Output Register File
    D3DSPR_OUTPUT      = 6,                // Output register file for VS3.0+
    D3DSPR_CONSTINT    = 7,                // Constant Integer Vector Register File
    D3DSPR_COLOROUT    = 8,                // Color Output Register File
    D3DSPR_DEPTHOUT    = 9,                // Depth Output Register File
    D3DSPR_SAMPLER     = 10,               // Sampler State Register File
    D3DSPR_CONST2      = 11,               // Constant Register File  2048 - 4095
    D3DSPR_CONST3      = 12,               // Constant Register File  4096 - 6143
    D3DSPR_CONST4      = 13,               // Constant Register File  6144 - 8191
    D3DSPR_CONSTBOOL   = 14,               // Constant Boolean register file
    D3DSPR_LOOP        = 15,               // Loop counter register file
    D3DSPR_TEMPFLOAT16 = 16,               // 16-bit float temp register file
    D3DSPR_MISCTYPE    = 17,               // Miscellaneous (single) registers.
    D3DSPR_LABEL       = 18,               // Label
    D3DSPR_PREDICATE   = 19,               // Predicate register
    D3DSPR_FORCE_DWORD = FORCEDWORD        // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DSHADER_PARAM_REGISTER_TYPE}
  D3DSHADER_PARAM_REGISTER_TYPE = _D3DSHADER_PARAM_REGISTER_TYPE;
  {$EXTERNALSYM D3DSHADER_PARAM_REGISTER_TYPE}

  // The miscellaneous register file (D3DSPR_MISCTYPES)
  // contains register types for which there is only ever one
  // register (i.e. the register # is not needed).
  // Rather than use up additional register types for such
  // registers, they are defined
  // as particular offsets into the misc. register file:
  PD3DSHADER_MISCTYPE_OFFSETS = ^D3DSHADER_MISCTYPE_OFFSETS;
  _D3DSHADER_MISCTYPE_OFFSETS = (
    D3DSMO_POSITION = 0,            // Input position x,y,z,rhw (PS)
    D3DSMO_FACE     = 1             // Floating point primitive area (PS)
  );
  {$EXTERNALSYM _D3DSHADER_MISCTYPE_OFFSETS}
  D3DSHADER_MISCTYPE_OFFSETS = _D3DSHADER_MISCTYPE_OFFSETS;
  {$EXTERNALSYM D3DSHADER_MISCTYPE_OFFSETS}

  // Register offsets in the Rasterizer Register File
  //
  PD3DVS_RASTOUT_OFFSETS = ^D3DVS_RASTOUT_OFFSETS;
  _D3DVS_RASTOUT_OFFSETS = (
    D3DSRO_POSITION    = 0,
    D3DSRO_FOG,
    D3DSRO_POINT_SIZE,
    D3DSRO_FORCE_DWORD = FORCEDWORD // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DVS_RASTOUT_OFFSETS}
  D3DVS_RASTOUT_OFFSETS = _D3DVS_RASTOUT_OFFSETS;
  {$EXTERNALSYM D3DVS_RASTOUT_OFFSETS}


const

  // Source operand addressing modes

  D3DVS_ADDRESSMODE_SHIFT             = 13;
  {$EXTERNALSYM D3DVS_ADDRESSMODE_SHIFT}
  D3DVS_ADDRESSMODE_MASK              = (1 shl D3DVS_ADDRESSMODE_SHIFT);
  {$EXTERNALSYM D3DVS_ADDRESSMODE_MASK}


type

  PD3DVS_ADDRESSMODE_TYPE = ^D3DVS_ADDRESSMODE_TYPE;
  _D3DVS_ADDRESSMODE_TYPE      = (
    D3DVS_ADDRMODE_ABSOLUTE    = (0 shl D3DVS_ADDRESSMODE_SHIFT),
    D3DVS_ADDRMODE_RELATIVE    = (1 shl D3DVS_ADDRESSMODE_SHIFT),
    D3DVS_ADDRMODE_FORCE_DWORD = FORCEDWORD    // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DVS_ADDRESSMODE_TYPE}
  D3DVS_ADDRESSMODE_TYPE = _D3DVS_ADDRESSMODE_TYPE;
  {$EXTERNALSYM D3DVS_ADDRESSMODE_TYPE}


const

  D3DSHADER_ADDRESSMODE_SHIFT         = 13;
  {$EXTERNALSYM D3DSHADER_ADDRESSMODE_SHIFT}
  D3DSHADER_ADDRESSMODE_MASK          = (1 shl D3DSHADER_ADDRESSMODE_SHIFT);
  {$EXTERNALSYM D3DSHADER_ADDRESSMODE_MASK}


type

  PD3DSHADER_ADDRESSMODE_TYPE = ^D3DSHADER_ADDRESSMODE_TYPE;
  _D3DSHADER_ADDRESSMODE_TYPE      = (
    D3DSHADER_ADDRMODE_ABSOLUTE    = (0 shl D3DSHADER_ADDRESSMODE_SHIFT),
    D3DSHADER_ADDRMODE_RELATIVE    = (1 shl D3DSHADER_ADDRESSMODE_SHIFT),
    D3DSHADER_ADDRMODE_FORCE_DWORD = FORCEDWORD   // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DSHADER_ADDRESSMODE_TYPE}
  D3DSHADER_ADDRESSMODE_TYPE = _D3DSHADER_ADDRESSMODE_TYPE;
  {$EXTERNALSYM D3DSHADER_ADDRESSMODE_TYPE}


const

  // Source operand swizzle definitions
  //
  D3DVS_SWIZZLE_SHIFT                 = 16;
  {$EXTERNALSYM D3DVS_SWIZZLE_SHIFT}
  D3DVS_SWIZZLE_MASK                  = $00FF0000;
  {$EXTERNALSYM D3DVS_SWIZZLE_MASK}

  // The following bits define where to take component X from:

  D3DVS_X_X                           = (0 shl D3DVS_SWIZZLE_SHIFT);
  {$EXTERNALSYM D3DVS_X_X}
  D3DVS_X_Y                           = (1 shl D3DVS_SWIZZLE_SHIFT);
  {$EXTERNALSYM D3DVS_X_Y}
  D3DVS_X_Z                           = (2 shl D3DVS_SWIZZLE_SHIFT);
  {$EXTERNALSYM D3DVS_X_Z}
  D3DVS_X_W                           = (3 shl D3DVS_SWIZZLE_SHIFT);
  {$EXTERNALSYM D3DVS_X_W}

  // The following bits define where to take component Y from:

  D3DVS_Y_X                           = (0 shl (D3DVS_SWIZZLE_SHIFT + 2));
  {$EXTERNALSYM D3DVS_Y_X}
  D3DVS_Y_Y                           = (1 shl (D3DVS_SWIZZLE_SHIFT + 2));
  {$EXTERNALSYM D3DVS_Y_Y}
  D3DVS_Y_Z                           = (2 shl (D3DVS_SWIZZLE_SHIFT + 2));
  {$EXTERNALSYM D3DVS_Y_Z}
  D3DVS_Y_W                           = (3 shl (D3DVS_SWIZZLE_SHIFT + 2));
  {$EXTERNALSYM D3DVS_Y_W}

  // The following bits define where to take component Z from:

  D3DVS_Z_X                           = (0 shl (D3DVS_SWIZZLE_SHIFT + 4));
  {$EXTERNALSYM D3DVS_Z_X}
  D3DVS_Z_Y                           = (1 shl (D3DVS_SWIZZLE_SHIFT + 4));
  {$EXTERNALSYM D3DVS_Z_Y}
  D3DVS_Z_Z                           = (2 shl (D3DVS_SWIZZLE_SHIFT + 4));
  {$EXTERNALSYM D3DVS_Z_Z}
  D3DVS_Z_W                           = (3 shl (D3DVS_SWIZZLE_SHIFT + 4));
  {$EXTERNALSYM D3DVS_Z_W}

  // The following bits define where to take component W from:

  D3DVS_W_X                           = (0 shl (D3DVS_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DVS_W_X}
  D3DVS_W_Y                           = (1 shl (D3DVS_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DVS_W_Y}
  D3DVS_W_Z                           = (2 shl (D3DVS_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DVS_W_Z}
  D3DVS_W_W                           = (3 shl (D3DVS_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DVS_W_W}

  // Value when there is no swizzle (X is taken from X, Y is taken from Y,
  // Z is taken from Z, W is taken from W
  //
  D3DVS_NOSWIZZLE                     = (D3DVS_X_X or D3DVS_Y_Y or D3DVS_Z_Z or D3DVS_W_W);
  {$EXTERNALSYM D3DVS_NOSWIZZLE}

  // source parameter swizzle
  D3DSP_SWIZZLE_SHIFT                 = 16;
  {$EXTERNALSYM D3DSP_SWIZZLE_SHIFT}
  D3DSP_SWIZZLE_MASK                  = $00FF0000;
  {$EXTERNALSYM D3DSP_SWIZZLE_MASK}

  D3DSP_NOSWIZZLE =
    (0 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
    (1 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
    (2 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
    (3 shl (D3DSP_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DSP_NOSWIZZLE}

  // pixel-shader swizzle ops
  D3DSP_REPLICATERED =
    (0 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
    (0 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
    (0 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
    (0 shl (D3DSP_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DSP_REPLICATERED}

  D3DSP_REPLICATEGREEN =
    (1 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
    (1 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
    (1 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
    (1 shl (D3DSP_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DSP_REPLICATEGREEN}

  D3DSP_REPLICATEBLUE =
    (2 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
    (2 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
    (2 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
    (2 shl (D3DSP_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DSP_REPLICATEBLUE}

  D3DSP_REPLICATEALPHA =
    (3 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
    (3 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
    (3 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
    (3 shl (D3DSP_SWIZZLE_SHIFT + 6));
  {$EXTERNALSYM D3DSP_REPLICATEALPHA}

  // source parameter modifiers
  D3DSP_SRCMOD_SHIFT                  = 24;
  {$EXTERNALSYM D3DSP_SRCMOD_SHIFT}
  D3DSP_SRCMOD_MASK                   = $0F000000;
  {$EXTERNALSYM D3DSP_SRCMOD_MASK}


type

  PD3DSHADER_PARAM_SRCMOD_TYPE = ^_D3DSHADER_PARAM_SRCMOD_TYPE;
  _D3DSHADER_PARAM_SRCMOD_TYPE = (
    D3DSPSM_NONE        = 0  shl D3DSP_SRCMOD_SHIFT,   // nop
    D3DSPSM_NEG         = 1  shl D3DSP_SRCMOD_SHIFT,   // negate
    D3DSPSM_BIAS        = 2  shl D3DSP_SRCMOD_SHIFT,   // bias
    D3DSPSM_BIASNEG     = 3  shl D3DSP_SRCMOD_SHIFT,   // bias and negate
    D3DSPSM_SIGN        = 4  shl D3DSP_SRCMOD_SHIFT,   // sign
    D3DSPSM_SIGNNEG     = 5  shl D3DSP_SRCMOD_SHIFT,   // sign and negate
    D3DSPSM_COMP        = 6  shl D3DSP_SRCMOD_SHIFT,   // complement
    D3DSPSM_X2          = 7  shl D3DSP_SRCMOD_SHIFT,   // *2
    D3DSPSM_X2NEG       = 8  shl D3DSP_SRCMOD_SHIFT,   // *2 and negate
    D3DSPSM_DZ          = 9  shl D3DSP_SRCMOD_SHIFT,   // divide through by z component
    D3DSPSM_DW          = 10  shl D3DSP_SRCMOD_SHIFT,  // divide through by w component
    D3DSPSM_ABS         = 11  shl D3DSP_SRCMOD_SHIFT,  // abs()
    D3DSPSM_ABSNEG      = 12  shl D3DSP_SRCMOD_SHIFT,  // -abs()
    D3DSPSM_NOT         = 13  shl D3DSP_SRCMOD_SHIFT,  // for predicate register: "!p0"
    D3DSPSM_FORCE_DWORD = FORCEDWORD                   // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DSHADER_PARAM_SRCMOD_TYPE}
  D3DSHADER_PARAM_SRCMOD_TYPE = _D3DSHADER_PARAM_SRCMOD_TYPE;
  {$EXTERNALSYM D3DSHADER_PARAM_SRCMOD_TYPE}


const

  // Source or dest token bits [15:14]:
  // destination parameter modifiers
  D3DSP_MIN_PRECISION_SHIFT           = 14;
  {$EXTERNALSYM D3DSP_MIN_PRECISION_SHIFT}
  D3DSP_MIN_PRECISION_MASK            = $0000C000;
  {$EXTERNALSYM D3DSP_MIN_PRECISION_MASK}


type

  PD3DSHADER_MIN_PRECISION = ^_D3DSHADER_MIN_PRECISION;
  _D3DSHADER_MIN_PRECISION = (
    D3DMP_DEFAULT = 0,               // Default precision for the shader model
    D3DMP_16      = 1,               // 16 bit per component
    D3DMP_2_8     = 2                // 10 bits (2.8) per component
  );
  {$EXTERNALSYM _D3DSHADER_MIN_PRECISION}
  D3DSHADER_MIN_PRECISION = _D3DSHADER_MIN_PRECISION;
  {$EXTERNALSYM D3DSHADER_MIN_PRECISION}

  // If the older D3DSPDM_PARTIALPRECISION is set,
  // that is equivalent to the whole operation specifying
  // D3DMP_16 (on all operands).  The two encodings are not
  // used together however.


  // pixel shader version token
  function D3DPS_VERSION(_Major: DWORD; _Minor: DWORD): DWORD;
  {$EXTERNALSYM D3DPS_VERSION}
  // vertex shader version token
  function D3DVS_VERSION(_Major: DWORD; _Minor: DWORD): DWORD;
  {$EXTERNALSYM D3DVS_VERSION}
  // extract major/minor from version cap
  function D3DSHADER_VERSION_MAJOR(_Version: DWORD): DWORD;
  {$EXTERNALSYM D3DSHADER_VERSION_MAJOR}
  function D3DSHADER_VERSION_MINOR(_Version: DWORD): DWORD;
  {$EXTERNALSYM D3DSHADER_VERSION_MINOR}

const

  // destination/source parameter register type
  D3DSI_COMMENTSIZE_SHIFT             = 16;
  {$EXTERNALSYM D3DSI_COMMENTSIZE_SHIFT}
  D3DSI_COMMENTSIZE_MASK              = $7FFF0000;
  {$EXTERNALSYM D3DSI_COMMENTSIZE_MASK}


  function D3DSHADER_COMMENT(_DWordSize: DWORD): DWORD;
  {$EXTERNALSYM D3DSHADER_COMMENT}

const

  // pixel/vertex shader end token
  D3DPS_END = $0000FFFF;
  {$EXTERNALSYM D3DPS_END}
  D3DVS_END = $0000FFFF;
  {$EXTERNALSYM D3DVS_END}


  //---------------------------------------------------------------------

type
  // High order surfaces
  //
  PD3DBASISTYPE = ^_D3DBASISTYPE;
   _D3DBASISTYPE          = (
    D3DBASIS_BEZIER      = 0,
    D3DBASIS_BSPLINE     = 1,
    D3DBASIS_CATMULL_ROM = 2,   { In D3D8 this used to be D3DBASIS_INTERPOLATE }
    D3DBASIS_FORCE_DWORD = FORCEDWORD);
   {$EXTERNALSYM _D3DBASISTYPE}
  D3DBASISTYPE = _D3DBASISTYPE;
  {$EXTERNALSYM D3DBASISTYPE}


  PD3DDEGREETYPE = ^_D3DDEGREETYPE;
  _D3DDEGREETYPE          = (
    D3DDEGREE_LINEAR      = 1,
    D3DDEGREE_QUADRATIC   = 2,
    D3DDEGREE_CUBIC       = 3,
    D3DDEGREE_QUINTIC     = 5,
    D3DDEGREE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DDEGREETYPE}
  D3DDEGREETYPE = _D3DDEGREETYPE;
  {$EXTERNALSYM D3DDEGREETYPE}


  PD3DPATCHEDGESTYLE = ^_D3DPATCHEDGESTYLE;
  _D3DPATCHEDGESTYLE         = (
    D3DPATCHEDGE_DISCRETE    = 0,
    D3DPATCHEDGE_CONTINUOUS  = 1,
    D3DPATCHEDGE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DPATCHEDGESTYLE}
  D3DPATCHEDGESTYLE = _D3DPATCHEDGESTYLE;
  {$EXTERNALSYM D3DPATCHEDGESTYLE}


  PD3DSTATEBLOCKTYPE = ^_D3DSTATEBLOCKTYPE;
  _D3DSTATEBLOCKTYPE   = (
    D3DSBT_ALL         = 1,                 // capture all state
    D3DSBT_PIXELSTATE  = 2,                 // capture pixel state
    D3DSBT_VERTEXSTATE = 3,                 // capture vertex state
    D3DSBT_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DSTATEBLOCKTYPE}
  D3DSTATEBLOCKTYPE = _D3DSTATEBLOCKTYPE;
  {$EXTERNALSYM D3DSTATEBLOCKTYPE}


  // The D3DVERTEXBLENDFLAGS type is used with D3DRS_VERTEXBLEND state.
  //
  PD3DVERTEXBLENDFLAGS = ^_D3DVERTEXBLENDFLAGS;
  _D3DVERTEXBLENDFLAGS = (
    D3DVBF_DISABLE     = 0,             // Disable vertex blending
    D3DVBF_1WEIGHTS    = 1,             // 2 matrix blending
    D3DVBF_2WEIGHTS    = 2,             // 3 matrix blending
    D3DVBF_3WEIGHTS    = 3,             // 4 matrix blending
    D3DVBF_TWEENING    = 255,           // blending using D3DRS_TWEENFACTOR
    D3DVBF_0WEIGHTS    = 256,           // one matrix is used with weight 1.0
    D3DVBF_FORCE_DWORD = FORCEDWORD     // force 32-bit size enum
  );
  {$EXTERNALSYM _D3DVERTEXBLENDFLAGS}
  D3DVERTEXBLENDFLAGS = _D3DVERTEXBLENDFLAGS;
  {$EXTERNALSYM D3DVERTEXBLENDFLAGS}


  PD3DTEXTURETRANSFORMFLAGS = ^_D3DTEXTURETRANSFORMFLAGS;
  _D3DTEXTURETRANSFORMFLAGS = (
    D3DTTFF_DISABLE     = 0,            // texture coordinates are passed directly
    D3DTTFF_COUNT1      = 1,            // rasterizer should expect 1-D texture coords
    D3DTTFF_COUNT2      = 2,            // rasterizer should expect 2-D texture coords
    D3DTTFF_COUNT3      = 3,            // rasterizer should expect 3-D texture coords
    D3DTTFF_COUNT4      = 4,            // rasterizer should expect 4-D texture coords
    D3DTTFF_PROJECTED   = 256,          // texcoords to be divided by COUNTth element
    D3DTTFF_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DTEXTURETRANSFORMFLAGS}
  D3DTEXTURETRANSFORMFLAGS = _D3DTEXTURETRANSFORMFLAGS;
  {$EXTERNALSYM D3DTEXTURETRANSFORMFLAGS}


const

  // Macros to set texture coordinate format bits in the FVF id

  D3DFVF_TEXTUREFORMAT2               = 0;  // Two floating point values
  {$EXTERNALSYM D3DFVF_TEXTUREFORMAT2}
  D3DFVF_TEXTUREFORMAT1               = 3;  // One floating point value
  {$EXTERNALSYM D3DFVF_TEXTUREFORMAT1}
  D3DFVF_TEXTUREFORMAT3               = 1;  // Three floating point values
  {$EXTERNALSYM D3DFVF_TEXTUREFORMAT3}
  D3DFVF_TEXTUREFORMAT4               = 2;  // Four floating point values
  {$EXTERNALSYM D3DFVF_TEXTUREFORMAT4}


  function D3DFVF_TEXCOORDSIZE3(CoordIndex: DWORD): DWORD; inline;
  {$EXTERNALSYM D3DFVF_TEXCOORDSIZE3}
  function D3DFVF_TEXCOORDSIZE2(CoordIndex: DWORD): DWORD; inline;
  {$EXTERNALSYM D3DFVF_TEXCOORDSIZE2}
  function D3DFVF_TEXCOORDSIZE4(CoordIndex: DWORD): DWORD; inline;
  {$EXTERNALSYM D3DFVF_TEXCOORDSIZE4}
  function D3DFVF_TEXCOORDSIZE1(CoordIndex: DWORD): DWORD; inline;
  {$EXTERNALSYM D3DFVF_TEXCOORDSIZE1}


// MAKEFOURCC is also defined in media foundation MFApi.pas and MmReg.pas
{$IFNDEF MAKEFOURCC}
  function MAKEFOURCC(ch0: AnsiChar;
                      ch1: AnsiChar;
                      ch2: AnsiChar;
                      ch3: AnsiChar): DWORD; inline;
  {$EXTERNALSYM MAKEFOURCC}
{$DEFINE MAKEFOURCC}
{$ENDIF}
  //---------------------------------------------------------------------
  //Tony: To simulate the MAKEFOURCC macro, we directly translate the code.
  //      For example: D3DFMT_UYVY = MAKEFOURCC(FOURCC ('U', 'Y', 'V', 'Y')) is not possible - generates
  //      E2026 Constant expression expected error -
  //      The solution is like this:
  //      D3DFMT_UYVY = Byte('U') or (Byte('Y') shl 8) or (Byte('V') shl 16) or (Byte('Y') shl 24)

type
  //* Direct3D9 Device types */

  PD3DDEVTYPE = ^D3DDEVTYPE;
  _D3DDEVTYPE              = (
    D3DDEVTYPE_HAL         = 1,
    D3DDEVTYPE_REF         = 2,
    D3DDEVTYPE_SW          = 3,
    D3DDEVTYPE_NULLREF     = 4,
    D3DDEVTYPE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DDEVTYPE}
  D3DDEVTYPE = _D3DDEVTYPE;
  {$EXTERNALSYM D3DDEVTYPE}


  //* Multi-Sample buffer types */
  PD3DMULTISAMPLE_TYPE = ^D3DMULTISAMPLE_TYPE;
  _D3DMULTISAMPLE_TYPE         = (
    D3DMULTISAMPLE_NONE        = 0,
    D3DMULTISAMPLE_NONMASKABLE = 1,
    D3DMULTISAMPLE_2_SAMPLES   = 2,
    D3DMULTISAMPLE_3_SAMPLES   = 3,
    D3DMULTISAMPLE_4_SAMPLES   = 4,
    D3DMULTISAMPLE_5_SAMPLES   = 5,
    D3DMULTISAMPLE_6_SAMPLES   = 6,
    D3DMULTISAMPLE_7_SAMPLES   = 7,
    D3DMULTISAMPLE_8_SAMPLES   = 8,
    D3DMULTISAMPLE_9_SAMPLES   = 9,
    D3DMULTISAMPLE_10_SAMPLES  = 10,
    D3DMULTISAMPLE_11_SAMPLES  = 11,
    D3DMULTISAMPLE_12_SAMPLES  = 12,
    D3DMULTISAMPLE_13_SAMPLES  = 13,
    D3DMULTISAMPLE_14_SAMPLES  = 14,
    D3DMULTISAMPLE_15_SAMPLES  = 15,
    D3DMULTISAMPLE_16_SAMPLES  = 16,
    D3DMULTISAMPLE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DMULTISAMPLE_TYPE}
  D3DMULTISAMPLE_TYPE = _D3DMULTISAMPLE_TYPE;
  {$EXTERNALSYM D3DMULTISAMPLE_TYPE}


// * Formats
// * Most of these names have the following convention:
// *      A = Alpha
// *      R = Red
// *      G = Green
// *      B = Blue
// *      X = Unused Bits
// *      P = Palette
// *      L = Luminance
// *      U = dU coordinate for BumpMap
// *      V = dV coordinate for BumpMap
// *      S = Stencil
// *      D = Depth (e.g. Z or W buffer)
// *      C = Computed from other channels (typically on certain read operations)
//*
// *      Further, the order of the pieces are from MSB first; hence
// *      D3DFMT_A8L8 indicates that the high byte of this two byte
// *      format is alpha.
// *
// *      D3DFMT_D16_LOCKABLE indicates:
// *           - An integer 16-bit value.
// *           - An app-lockable surface.
// *
// *      D3DFMT_D32F_LOCKABLE indicates:
// *           - An IEEE 754 floating-point value.
// *           - An app-lockable surface.
// *
// *      All Depth/Stencil formats except D3DFMT_D16_LOCKABLE and D3DFMT_D32F_LOCKABLE indicate:
// *          - no particular bit ordering per pixel, and
// *          - are not app lockable, and
// *          - the driver is allowed to consume more than the indicated
// *            number of bits per Depth channel (but not Stencil channel).
// */


{$IFDEF TYPE_IDENTITY}

  _D3DFORMAT = type DWord;
  {$EXTERNALSYM _D3DFORMAT}

{$ELSE}

type
  _D3DFORMAT                    = (
    D3DFMT_UNKNOWN              = 0,
    D3DFMT_R8G8B8               = 20,
    D3DFMT_A8R8G8B8             = 21,
    D3DFMT_X8R8G8B8             = 22,
    D3DFMT_R5G6B5               = 23,
    D3DFMT_X1R5G5B5             = 24,
    D3DFMT_A1R5G5B5             = 25,
    D3DFMT_A4R4G4B4             = 26,
    D3DFMT_R3G3B2               = 27,
    D3DFMT_A8                   = 28,
    D3DFMT_A8R3G3B2             = 29,
    D3DFMT_X4R4G4B4             = 30,
    D3DFMT_A2B10G10R10          = 31,
    D3DFMT_A8B8G8R8             = 32,
    D3DFMT_X8B8G8R8             = 33,
    D3DFMT_G16R16               = 34,
    D3DFMT_A2R10G10B10          = 35,
    D3DFMT_A16B16G16R16         = 36,

    D3DFMT_A8P8                 = 40,
    D3DFMT_P8                   = 41,

    D3DFMT_L8                   = 50,
    D3DFMT_A8L8                 = 51,
    D3DFMT_A4L4                 = 52,

    D3DFMT_V8U8                 = 60,
    D3DFMT_L6V5U5               = 61,
    D3DFMT_X8L8V8U8             = 62,
    D3DFMT_Q8W8V8U8             = 63,
    D3DFMT_V16U16               = 64,
    D3DFMT_A2W10V10U10          = 67,

    D3DFMT_UYVY                 = Byte('U') OR (Byte('Y') shl 8) OR (Byte('V') shl 16) OR (Byte('Y') shl 24),
    D3DFMT_RGBG                 = Byte('R') OR (Byte('G') shl 8) OR (Byte('B') shl 16) OR (Byte('G') shl 24),
    D3DFMT_YUY2                 = Byte('Y') OR (Byte('U') shl 8) OR (Byte('Y') shl 16) OR (Byte('2') shl 24),
    D3DFMT_GRGB                 = Byte('G') OR (Byte('R') shl 8) OR (Byte('G') shl 16) OR (Byte('B') shl 24),
    D3DFMT_DXT1                 = Byte('D') OR (Byte('X') shl 8) OR (Byte('T') shl 16) OR (Byte('1') shl 24),
    D3DFMT_DXT2                 = Byte('D') OR (Byte('X') shl 8) OR (Byte('T') shl 16) OR (Byte('2') shl 24),
    D3DFMT_DXT3                 = Byte('D') OR (Byte('X') shl 8) OR (Byte('T') shl 16) OR (Byte('3') shl 24),
    D3DFMT_DXT4                 = Byte('D') OR (Byte('X') shl 8) OR (Byte('T') shl 16) OR (Byte('4') shl 24),
    D3DFMT_DXT5                 = Byte('D') OR (Byte('X') shl 8) OR (Byte('T') shl 16) OR (Byte('5') shl 24),

    D3DFMT_D16_LOCKABLE         = 70,
    D3DFMT_D32                  = 71,
    D3DFMT_D15S1                = 73,
    D3DFMT_D24S8                = 75,
    D3DFMT_D24X8                = 77,
    D3DFMT_D24X4S4              = 79,
    D3DFMT_D16                  = 80,
    D3DFMT_D32F_LOCKABLE        = 82,
    D3DFMT_D24FS8               = 83,

   //* D3D9Ex only -- */
   //{$IFDEF D3D_DISABLE_9EX}
   //* Z-Stencil formats valid for CPU access */
    D3DFMT_D32_LOCKABLE         = 84,
    D3DFMT_S8_LOCKABLE          = 85,
   //{$ENDIF} //!D3D_DISABLE_9EX
   //* -- D3D9Ex only */
    D3DFMT_L16                  = 81,

    D3DFMT_VERTEXDATA           = 100,
    D3DFMT_INDEX16              = 101,
    D3DFMT_INDEX32              = 102,

    D3DFMT_Q16W16V16U16         = 110,

    D3DFMT_MULTI2_ARGB8         = Byte('M') OR (Byte('E') shl 8) OR (Byte('T') shl 16) OR (Byte('1') shl 24),

    // Floating point surface formats

    // s10e5 formats (16-bits per channel)
    D3DFMT_R16F                 = 111,
    D3DFMT_G16R16F              = 112,
    D3DFMT_A16B16G16R16F        = 113,

    // IEEE s23e8 formats (32-bits per channel)
    D3DFMT_R32F                 = 114,
    D3DFMT_G32R32F              = 115,
    D3DFMT_A32B32G32R32F        = 116,

    D3DFMT_CxV8U8               = 117,

    //* D3D9Ex only -- */
    //{$IFDEF D3D_DISABLE_9EX}
    // Monochrome 1 bit per pixel format
    D3DFMT_A1                   = 118,
    /// 2.8 biased fixed point
    D3DFMT_A2B10G10R10_XR_BIAS  = 119,
    // Binary format indicating that the data has no inherent type
    D3DFMT_BINARYBUFFER         = 199,
    //{$ENDIF} // !D3D_DISABLE_9EX
    //* -- D3D9Ex only */
    D3DFMT_FORCE_DWORD          = FORCEDWORD);
  {$EXTERNALSYM _D3DFORMAT}
{$ENDIF}

  PD3DFORMAT = ^_D3DFORMAT;
  D3DFORMAT = _D3DFORMAT;
  {$EXTERNALSYM D3DFORMAT}


  //* Display Modes */
  PD3DDISPLAYMODE = ^_D3DDISPLAYMODE;
  _D3DDISPLAYMODE = record
    Width: UINT;
    Height: UINT;
    RefreshRate: UINT;
    Format: D3DFORMAT;
  end;
  {$EXTERNALSYM _D3DDISPLAYMODE}
  D3DDISPLAYMODE = _D3DDISPLAYMODE;
  {$EXTERNALSYM D3DDISPLAYMODE}


  //* Creation Parameters */
  PD3DDEVICE_CREATION_PARAMETERS = ^_D3DDEVICE_CREATION_PARAMETERS;
  _D3DDEVICE_CREATION_PARAMETERS = record
    AdapterOrdinal: UINT;
    DeviceType: D3DDEVTYPE;
    hFocusWindow: HWND;
    BehaviorFlags: DWORD;
  end;
  {$EXTERNALSYM _D3DDEVICE_CREATION_PARAMETERS}
  D3DDEVICE_CREATION_PARAMETERS = _D3DDEVICE_CREATION_PARAMETERS;
  {$EXTERNALSYM D3DDEVICE_CREATION_PARAMETERS}


  //* SwapEffects */
  PD3DSWAPEFFECT = ^_D3DSWAPEFFECT;
  _D3DSWAPEFFECT              = (
    D3DSWAPEFFECT_DISCARD     = 1,
    D3DSWAPEFFECT_FLIP        = 2,
    D3DSWAPEFFECT_COPY        = 3,
    //* D3D9Ex only -- */
    //{$IFDEF D3D_DISABLE_9EX}
    D3DSWAPEFFECT_OVERLAY     = 4,
    D3DSWAPEFFECT_FLIPEX      = 5,
    //{$ENDIF} // !D3D_DISABLE_9EX
    //* -- D3D9Ex only */
    D3DSWAPEFFECT_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DSWAPEFFECT}
  D3DSWAPEFFECT = _D3DSWAPEFFECT;
  {$EXTERNALSYM D3DSWAPEFFECT}


  //* Pool types */
  PD3DPOOL = ^_D3DPOOL;
  _D3DPOOL              = (
    D3DPOOL_DEFAULT     = 0,
    D3DPOOL_MANAGED     = 1,
    D3DPOOL_SYSTEMMEM   = 2,
    D3DPOOL_SCRATCH     = 3,
    D3DPOOL_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DPOOL}
  D3DPOOL = _D3DPOOL;
  {$EXTERNALSYM D3DPOOL}


const

  //* RefreshRate pre-defines */
  D3DPRESENT_RATE_DEFAULT             = $00000000;
  {$EXTERNALSYM D3DPRESENT_RATE_DEFAULT}


type
  //* Resize Optional Parameters */
  PD3DPRESENT_PARAMETERS = ^_D3DPRESENT_PARAMETERS_;
  _D3DPRESENT_PARAMETERS_ = record
    BackBufferWidth: UINT;
    BackBufferHeight: UINT;
    BackBufferFormat: D3DFORMAT;
    BackBufferCount: UINT;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    MultiSampleQuality: DWORD;
    SwapEffect: D3DSWAPEFFECT;
    hDeviceWindow: HWND;
    Windowed: BOOL;
    EnableAutoDepthStencil: BOOL;
    AutoDepthStencilFormat: D3DFORMAT;
    Flags: DWORD;
    //* FullScreen_RefreshRateInHz must be zero for Windowed mode */
    FullScreen_RefreshRateInHz: UINT;
    PresentationInterval: UINT;
  end;
  {$EXTERNALSYM _D3DPRESENT_PARAMETERS_}
  D3DPRESENT_PARAMETERS = _D3DPRESENT_PARAMETERS_;
  {$EXTERNALSYM D3DPRESENT_PARAMETERS}


const
  // Values for D3DPRESENT_PARAMETERS.Flags

  D3DPRESENTFLAG_LOCKABLE_BACKBUFFER  = $00000001;
  {$EXTERNALSYM D3DPRESENTFLAG_LOCKABLE_BACKBUFFER}
  D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL = $00000002;
  {$EXTERNALSYM D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL}
  D3DPRESENTFLAG_DEVICECLIP           = $00000004;
  {$EXTERNALSYM D3DPRESENTFLAG_DEVICECLIP}
  D3DPRESENTFLAG_VIDEO                = $00000010;
  {$EXTERNALSYM D3DPRESENTFLAG_VIDEO}

  //* D3D9Ex only -- */
  //{$IFDEF D3D_DISABLE_9EX}
  D3DPRESENTFLAG_NOAUTOROTATE         = $00000020;
  {$EXTERNALSYM D3DPRESENTFLAG_NOAUTOROTATE}
  D3DPRESENTFLAG_UNPRUNEDMODE         = $00000040;
  {$EXTERNALSYM D3DPRESENTFLAG_UNPRUNEDMODE}
  D3DPRESENTFLAG_OVERLAY_LIMITEDRGB   = $00000080;
  {$EXTERNALSYM D3DPRESENTFLAG_OVERLAY_LIMITEDRGB}
  D3DPRESENTFLAG_OVERLAY_YCbCr_BT709  = $00000100;
  {$EXTERNALSYM D3DPRESENTFLAG_OVERLAY_YCbCr_BT709}
  D3DPRESENTFLAG_OVERLAY_YCbCr_xvYCC  = $00000200;
  {$EXTERNALSYM D3DPRESENTFLAG_OVERLAY_YCbCr_xvYCC}
  D3DPRESENTFLAG_RESTRICTED_CONTENT   = $00000400;
  {$EXTERNALSYM D3DPRESENTFLAG_RESTRICTED_CONTENT}
  D3DPRESENTFLAG_RESTRICT_SHARED_RESOURCE_DRIVER = $00000800;
  {$EXTERNALSYM D3DPRESENTFLAG_RESTRICT_SHARED_RESOURCE_DRIVER}

  //{$ENDIF} // !D3D_DISABLE_9EX
  //* -- D3D9Ex only */

type
  //* Gamma Ramp: Same as DX7 */

  P3DGAMMARAMP = ^_D3DGAMMARAMP;
  _D3DGAMMARAMP = record
    red: array[0..255] of WORD;
    green: array[0..255] of WORD;
    blue: array[0..255] of WORD;
  end;
  {$EXTERNALSYM _D3DGAMMARAMP}
  D3DGAMMARAMP = _D3DGAMMARAMP;
  {$EXTERNALSYM D3DGAMMARAMP}


  // back buffer types */
  PD3DBACKBUFFER_TYPE = ^_D3DBACKBUFFER_TYPE;
  {$EXTERNALSYM _D3DBACKBUFFER_TYPE}
  {$EXTERNALSYM _D3DBACKBUFFER_TYPE}
  _D3DBACKBUFFER_TYPE              = (
    D3DBACKBUFFER_TYPE_MONO        = 0,
    D3DBACKBUFFER_TYPE_LEFT        = 1,
    D3DBACKBUFFER_TYPE_RIGHT       = 2,
    D3DBACKBUFFER_TYPE_FORCE_DWORD = FORCEDWORD);
  D3DBACKBUFFER_TYPE = _D3DBACKBUFFER_TYPE;
  {$EXTERNALSYM D3DBACKBUFFER_TYPE}


  //* Types */

  PD3DRESOURCETYPE = ^_D3DRESOURCETYPE;
  _D3DRESOURCETYPE         = (
    D3DRTYPE_SURFACE       = 1,
    D3DRTYPE_VOLUME        = 2,
    D3DRTYPE_TEXTURE       = 3,
    D3DRTYPE_VOLUMETEXTURE = 4,
    D3DRTYPE_CUBETEXTURE   = 5,
    D3DRTYPE_VERTEXBUFFER  = 6,
    D3DRTYPE_INDEXBUFFER   = 7,   // if this changes, change _D3DDEVINFO_RESOURCEMANAGER definition
    D3DRTYPE_FORCE_DWORD   = FORCEDWORD);
  {$EXTERNALSYM _D3DRESOURCETYPE}
  D3DRESOURCETYPE = _D3DRESOURCETYPE;
  {$EXTERNALSYM D3DRESOURCETYPE}


const
  //* Usages */
  D3DUSAGE_RENDERTARGET               = ($00000001);
  {$EXTERNALSYM D3DUSAGE_RENDERTARGET}
  D3DUSAGE_DEPTHSTENCIL               = ($00000002);
  {$EXTERNALSYM D3DUSAGE_DEPTHSTENCIL}
  D3DUSAGE_DYNAMIC                    = ($00000200);
  {$EXTERNALSYM D3DUSAGE_DYNAMIC}

  //* D3D9Ex only -- */
  //{$IFDEF D3D_DISABLE_9EX}
  D3DUSAGE_NONSECURE                  = ($00800000);
  {$EXTERNALSYM D3DUSAGE_NONSECURE}
  //{$ENDIF} // !D3D_DISABLE_9EX

  //* -- D3D9Ex only */

  // When passed to CheckDeviceFormat, D3DUSAGE_AUTOGENMIPMAP may return
  // D3DOK_NOAUTOGEN if the device doesn't support autogeneration for that format.
  // D3DOK_NOAUTOGEN is a success code, not a failure code... the SUCCEEDED and FAILED macros
  // will return true and false respectively for this code.
  D3DUSAGE_AUTOGENMIPMAP              = ($00000400);
  {$EXTERNALSYM D3DUSAGE_AUTOGENMIPMAP}
  D3DUSAGE_DMAP                       = ($00004000);
  {$EXTERNALSYM D3DUSAGE_DMAP}

  // The following usages are valid only for querying CheckDeviceFormat
  D3DUSAGE_QUERY_LEGACYBUMPMAP        = ($00008000);
  {$EXTERNALSYM D3DUSAGE_QUERY_LEGACYBUMPMAP}
  D3DUSAGE_QUERY_SRGBREAD             = ($00010000);
  {$EXTERNALSYM D3DUSAGE_QUERY_SRGBREAD}
  D3DUSAGE_QUERY_FILTER               = ($00020000);
  {$EXTERNALSYM D3DUSAGE_QUERY_FILTER}
  D3DUSAGE_QUERY_SRGBWRITE            = ($00040000);
  {$EXTERNALSYM D3DUSAGE_QUERY_SRGBWRITE}
  D3DUSAGE_QUERY_POSTPIXELSHADER_BLENDING = ($00080000);
  {$EXTERNALSYM D3DUSAGE_QUERY_POSTPIXELSHADER_BLENDING}
  D3DUSAGE_QUERY_VERTEXTEXTURE        = ($00100000);
  {$EXTERNALSYM D3DUSAGE_QUERY_VERTEXTEXTURE}
  D3DUSAGE_QUERY_WRAPANDMIP           = ($00200000);
  {$EXTERNALSYM D3DUSAGE_QUERY_WRAPANDMIP}

  //* Usages for Vertex/Index buffers */
  D3DUSAGE_WRITEONLY                  = ($00000008);
  {$EXTERNALSYM D3DUSAGE_WRITEONLY}
  D3DUSAGE_SOFTWAREPROCESSING         = ($00000010);
  {$EXTERNALSYM D3DUSAGE_SOFTWAREPROCESSING}
  D3DUSAGE_DONOTCLIP                  = ($00000020);
  {$EXTERNALSYM D3DUSAGE_DONOTCLIP}
  D3DUSAGE_POINTS                     = ($00000040);
  {$EXTERNALSYM D3DUSAGE_POINTS}
  D3DUSAGE_RTPATCHES                  = ($00000080);
  {$EXTERNALSYM D3DUSAGE_RTPATCHES}
  D3DUSAGE_NPATCHES                   = ($00000100);
  {$EXTERNALSYM D3DUSAGE_NPATCHES}

  //* D3D9Ex only -- */
  //{$IFDEF D3D_DISABLE_9EX}
  D3DUSAGE_TEXTAPI                         = ($10000000);
  {$EXTERNALSYM D3DUSAGE_TEXTAPI}
  D3DUSAGE_RESTRICTED_CONTENT              = ($00000800);
  {$EXTERNALSYM D3DUSAGE_RESTRICTED_CONTENT}
  D3DUSAGE_RESTRICT_SHARED_RESOURCE        = ($00002000);
  {$EXTERNALSYM D3DUSAGE_RESTRICT_SHARED_RESOURCE}
  D3DUSAGE_RESTRICT_SHARED_RESOURCE_DRIVER = ($00001000);
  {$EXTERNALSYM D3DUSAGE_RESTRICT_SHARED_RESOURCE_DRIVER}
  //{$ENDIF} // !D3D_DISABLE_9EX
  //* -- D3D9Ex only */


type

  //* CubeMap Face identifiers */
  PD3DCUBEMAP_FACES = ^D3DCUBEMAP_FACES;
  _D3DCUBEMAP_FACES             = (
    D3DCUBEMAP_FACE_POSITIVE_X  = 0,
    D3DCUBEMAP_FACE_NEGATIVE_X  = 1,
    D3DCUBEMAP_FACE_POSITIVE_Y  = 2,
    D3DCUBEMAP_FACE_NEGATIVE_Y  = 3,
    D3DCUBEMAP_FACE_POSITIVE_Z  = 4,
    D3DCUBEMAP_FACE_NEGATIVE_Z  = 5,
    D3DCUBEMAP_FACE_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DCUBEMAP_FACES}
  D3DCUBEMAP_FACES = _D3DCUBEMAP_FACES;
  {$EXTERNALSYM D3DCUBEMAP_FACES}


const

  //* Lock flags */

  D3DLOCK_READONLY                    = $00000010;
  {$EXTERNALSYM D3DLOCK_READONLY}
  D3DLOCK_DISCARD                     = $00002000;
  {$EXTERNALSYM D3DLOCK_DISCARD}
  D3DLOCK_NOOVERWRITE                 = $00001000;
  {$EXTERNALSYM D3DLOCK_NOOVERWRITE}
  D3DLOCK_NOSYSLOCK                   = $00000800;
  {$EXTERNALSYM D3DLOCK_NOSYSLOCK}
  D3DLOCK_DONOTWAIT                   = $00004000;
  {$EXTERNALSYM D3DLOCK_DONOTWAIT}
  D3DLOCK_NO_DIRTY_UPDATE             = $00008000;
  {$EXTERNALSYM D3DLOCK_NO_DIRTY_UPDATE}


type

  //* Vertex Buffer Description */
  PD3DVERTEXBUFFER_DESC = ^_D3DVERTEXBUFFER_DESC;
  _D3DVERTEXBUFFER_DESC = record
    Format: D3DFORMAT;
    _Type: D3DRESOURCETYPE;
    Usage: DWORD;
    Pool: D3DPOOL;
    Size: UINT;
    FVF: DWORD;
  end;
  {$EXTERNALSYM _D3DVERTEXBUFFER_DESC}
  D3DVERTEXBUFFER_DESC = _D3DVERTEXBUFFER_DESC;
  {$EXTERNALSYM D3DVERTEXBUFFER_DESC}


  //* Index Buffer Description */
  PD3DINDEXBUFFER_DESC = ^_D3DINDEXBUFFER_DESC;
  _D3DINDEXBUFFER_DESC = record
    Format: D3DFORMAT;
    _Type: D3DRESOURCETYPE;
    Usage: DWORD;
    Pool: D3DPOOL;
    Size: UINT;
  end;
  {$EXTERNALSYM _D3DINDEXBUFFER_DESC}
  D3DINDEXBUFFER_DESC = _D3DINDEXBUFFER_DESC;
  {$EXTERNALSYM D3DINDEXBUFFER_DESC}



  //* Surface Description */
  PD3DSURFACE_DESC = ^_D3DSURFACE_DESC;
  _D3DSURFACE_DESC = record
    Format: D3DFORMAT;
    _Type: D3DRESOURCETYPE;
    Usage: DWORD;
    Pool: D3DPOOL;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    MultiSampleQuality: DWORD;
    Width: UINT;
    Height: UINT;
  end;
  {$EXTERNALSYM _D3DSURFACE_DESC}
  D3DSURFACE_DESC = _D3DSURFACE_DESC;
  {$EXTERNALSYM D3DSURFACE_DESC}


  PD3DVOLUME_DESC = ^_D3DVOLUME_DESC;
  _D3DVOLUME_DESC = record
    Format: D3DFORMAT;
    _Type: D3DRESOURCETYPE;
    Usage: DWORD;
    Pool: D3DPOOL;
    Width: UINT;
    Height: UINT;
    Depth: UINT;
  end;
  {$EXTERNALSYM _D3DVOLUME_DESC}
  D3DVOLUME_DESC = _D3DVOLUME_DESC;
  {$EXTERNALSYM D3DVOLUME_DESC}


  //* Structure for LockRect */
  PD3DLOCKED_RECT = ^_D3DLOCKED_RECT;
  _D3DLOCKED_RECT = record
    Pitch: INT;
    pBits: Pointer;
  end;
  {$EXTERNALSYM _D3DLOCKED_RECT}
  D3DLOCKED_RECT = _D3DLOCKED_RECT;
  {$EXTERNALSYM D3DLOCKED_RECT}


  //* Structures for LockBox */
  PD3DBOX = ^_D3DBOX;
  _D3DBOX = record
    Left: UINT;
    Top: UINT;
    Right: UINT;
    Bottom: UINT;
    Front: UINT;
    Back: UINT;
  end;
  {$EXTERNALSYM _D3DBOX}
  D3DBOX = _D3DBOX;
  {$EXTERNALSYM D3DBOX}


  PD3DLOCKED_BOX = ^_D3DLOCKED_BOX;
  _D3DLOCKED_BOX = record
    RowPitch: INT;
    SlicePitch: INT;
    pBits: Pointer;
  end;
  {$EXTERNALSYM _D3DLOCKED_BOX}
  D3DLOCKED_BOX = _D3DLOCKED_BOX;
  {$EXTERNALSYM D3DLOCKED_BOX}


  //* Structures for LockRange */
  PD3DRANGE = ^_D3DRANGE;
  _D3DRANGE = record
    Offset: UINT;
    Size: UINT;
  end;
  {$EXTERNALSYM _D3DRANGE}
  D3DRANGE = _D3DRANGE;
  {$EXTERNALSYM D3DRANGE}


  //* Structures for high order primitives */
  PD3DRECTPATCH_INFO = ^_D3DRECTPATCH_INFO;
  _D3DRECTPATCH_INFO = record
    StartVertexOffsetWidth: UINT;
    StartVertexOffsetHeight: UINT;
    Width: UINT;
    Height: UINT;
    Stride: UINT;
    Basis: D3DBASISTYPE;
    Degree: D3DDEGREETYPE;
  end;
  {$EXTERNALSYM _D3DRECTPATCH_INFO}
  D3DRECTPATCH_INFO = _D3DRECTPATCH_INFO;
  {$EXTERNALSYM D3DRECTPATCH_INFO}


  PD3DTRIPATCH_INFO = ^_D3DTRIPATCH_INFO;
  _D3DTRIPATCH_INFO = record
    StartVertexOffset: UINT;
    NumVertices: UINT;
    Basis: D3DBASISTYPE;
    Degree: D3DDEGREETYPE;
  end;
  {$EXTERNALSYM _D3DTRIPATCH_INFO}
  D3DTRIPATCH_INFO = _D3DTRIPATCH_INFO;
  {$EXTERNALSYM D3DTRIPATCH_INFO}


const

  //* Adapter Identifier */

  MAX_DEVICE_IDENTIFIER_STRING        = 512;
  {$EXTERNALSYM MAX_DEVICE_IDENTIFIER_STRING}


type

  PD3DADAPTER_IDENTIFIER9 = ^_D3DADAPTER_IDENTIFIER9;
  _D3DADAPTER_IDENTIFIER9 = record
    Driver: array[0..MAX_DEVICE_IDENTIFIER_STRING - 1] of AnsiChar;
    Description: array[0..MAX_DEVICE_IDENTIFIER_STRING - 1] of AnsiChar;
    DeviceName: array[0..31] of AnsiChar;  { Device name for GDI (ex. \\.\DISPLAY1) }
    //#ifdef _WIN32
    DriverVersion: LARGE_INTEGER;          { Defined for 32 bit components }
    //#else
    DriverVersionLowPart: DWORD;           { Defined for 16 bit driver components }
    DriverVersionHighPart: DWORD;
    //#endif
    VendorId: DWORD;
    DeviceId: DWORD;
    SubSysId: DWORD;
    Revision: DWORD;
    DeviceIdentifier: TGUID;
    WHQLLevel: DWORD;
  end;
  {$EXTERNALSYM _D3DADAPTER_IDENTIFIER9}
  D3DADAPTER_IDENTIFIER9 = _D3DADAPTER_IDENTIFIER9;
  {$EXTERNALSYM D3DADAPTER_IDENTIFIER9}



  //* Raster Status structure returned by GetRasterStatus */
  PD3DRASTER_STATUS = ^_D3DRASTER_STATUS;
  _D3DRASTER_STATUS = record
    InVBlank: BOOL;
    ScanLine: UINT;
  end;
  {$EXTERNALSYM _D3DRASTER_STATUS}
  D3DRASTER_STATUS = _D3DRASTER_STATUS;
  {$EXTERNALSYM D3DRASTER_STATUS}




  //* Debug monitor tokens (DEBUG only)

  // Note that if D3DRS_DEBUGMONITORTOKEN is set, the call is treated as
  // passing a token to the debug monitor.  For example, if, after passing
  // D3DDMT_ENABLE/DISABLE to D3DRS_DEBUGMONITORTOKEN other token values
  // are passed in, the enabled/disabled state of the debug
  // monitor will still persist.
  //
  // The debug monitor defaults to enabled.
  //
  // Calling GetRenderState on D3DRS_DEBUGMONITORTOKEN is not of any use.

  PD3DDEBUGMONITORTOKENS = ^_D3DDEBUGMONITORTOKENS;
  _D3DDEBUGMONITORTOKENS = (
    D3DDMT_ENABLE      = 0,              // enable debug monitor
    D3DDMT_DISABLE     = 1,              // disable debug monitor
    D3DDMT_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM _D3DDEBUGMONITORTOKENS}
  D3DDEBUGMONITORTOKENS = _D3DDEBUGMONITORTOKENS;
  {$EXTERNALSYM D3DDEBUGMONITORTOKENS}


  // Async feedback

  PD3DQUERYTYPE = ^_D3DQUERYTYPE;
  _D3DQUERYTYPE                    = (
    D3DQUERYTYPE_VCACHE            = 4,        { D3DISSUE_END }
    D3DQUERYTYPE_RESOURCEMANAGER   = 5,        { D3DISSUE_END }
    D3DQUERYTYPE_VERTEXSTATS       = 6,        { D3DISSUE_END }
    D3DQUERYTYPE_EVENT             = 8,        { D3DISSUE_END }
    D3DQUERYTYPE_OCCLUSION         = 9,        { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_TIMESTAMP         = 10,       { D3DISSUE_END }
    D3DQUERYTYPE_TIMESTAMPDISJOINT = 11,       { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_TIMESTAMPFREQ     = 12,       { D3DISSUE_END }
    D3DQUERYTYPE_PIPELINETIMINGS   = 13,       { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_INTERFACETIMINGS  = 14,       { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_VERTEXTIMINGS     = 15,       { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_PIXELTIMINGS      = 16,       { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_BANDWIDTHTIMINGS  = 17,       { D3DISSUE_BEGIN, D3DISSUE_END }
    D3DQUERYTYPE_CACHEUTILIZATION  = 18        { D3DISSUE_BEGIN, D3DISSUE_END }
    //* D3D9Ex only -- */
    //{$IFDEF D3D_DISABLE_9EX}
    , D3DQUERYTYPE_MEMORYPRESSURE  = 19        { D3DISSUE_BEGIN, D3DISSUE_END }
    //{$ENDIF} // !D3D_DISABLE_9EX
  );
  {$EXTERNALSYM _D3DQUERYTYPE}
  D3DQUERYTYPE = _D3DQUERYTYPE;
  {$EXTERNALSYM D3DQUERYTYPE}


const
  // Flags field for Issue
  D3DISSUE_END                        = (1 shl 0);  // Tells the runtime to issue the end of a query, changing it's state to "non-signaled".
  {$EXTERNALSYM D3DISSUE_END}
  D3DISSUE_BEGIN                      = (1 shl 1);  // Tells the runtime to issue the beginng of a query.
  {$EXTERNALSYM D3DISSUE_BEGIN}


  // Flags field for GetData
  D3DGETDATA_FLUSH                    = (1 shl 0);  // Tells the runtime to flush if the query is outstanding.
  {$EXTERNALSYM D3DGETDATA_FLUSH}


type

  PD3DRESOURCESTATS = ^_D3DRESOURCESTATS;
  _D3DRESOURCESTATS = record
                                     // Data collected since last Present()
    bThrashing: BOOL;                { indicates if thrashing }
    ApproxBytesDownloaded: DWORD;    { Approximate number of bytes downloaded by resource manager }
    NumEvicts: DWORD;                { number of objects evicted }
    NumVidCreates: DWORD;            { number of objects created in video memory }
    LastPri: DWORD;                  { priority of last object evicted }
    NumUsed: DWORD;                  { number of objects set to the device }
    NumUsedInVidMem: DWORD;          { number of objects set to the device, which are already in video memory }
    // Persistent data
    WorkingSet: DWORD;               { number of objects in video memory }
    WorkingSetBytes: DWORD;          { number of bytes in video memory }
    TotalManaged: DWORD;             { total number of managed objects }
    TotalBytes: DWORD;               { total number of bytes of managed objects }
  end;
  {$EXTERNALSYM _D3DRESOURCESTATS}
  D3DRESOURCESTATS = _D3DRESOURCESTATS;
  {$EXTERNALSYM D3DRESOURCESTATS}


const

  D3DRTYPECOUNT                       = DWORD(D3DRTYPE_INDEXBUFFER) + 1;
  {$EXTERNALSYM D3DRTYPECOUNT}


type

  PD3DDEVINFO_RESOURCEMANAGER = ^_D3DDEVINFO_RESOURCEMANAGER;
  LPD3DDEVINFO_RESOURCEMANAGER = ^_D3DDEVINFO_RESOURCEMANAGER;
  _D3DDEVINFO_RESOURCEMANAGER = record
    {$IFDEF WOW64_ENUM_WORKAROUND}
    stats: array[0..D3DRTYPECOUNT - 1] of D3DRESOURCESTATS;
    {$ELSE}
    stats: array[0..7] of D3DRESOURCESTATS;
    {$ENDIF}
  end;
  {$EXTERNALSYM _D3DDEVINFO_RESOURCEMANAGER}
  D3DDEVINFO_RESOURCEMANAGER = _D3DDEVINFO_RESOURCEMANAGER;
  {$EXTERNALSYM D3DDEVINFO_RESOURCEMANAGER}


  PD3DDEVINFO_D3DVERTEXSTATS = ^_D3DDEVINFO_D3DVERTEXSTATS;
  LPD3DDEVINFO_D3DVERTEXSTATS = ^_D3DDEVINFO_D3DVERTEXSTATS;
  _D3DDEVINFO_D3DVERTEXSTATS = record
    NumRenderedTriangles: DWORD;        { total number of triangles that are not clipped in this frame }
    NumExtraClippingTriangles: DWORD;   { Number of new triangles generated by clipping }
  end;
  {$EXTERNALSYM _D3DDEVINFO_D3DVERTEXSTATS}
  D3DDEVINFO_D3DVERTEXSTATS = _D3DDEVINFO_D3DVERTEXSTATS;
  {$EXTERNALSYM D3DDEVINFO_D3DVERTEXSTATS}


  PD3DDEVINFO_VCACHE = ^_D3DDEVINFO_VCACHE;
  LPD3DDEVINFO_VCACHE = ^_D3DDEVINFO_VCACHE;
  _D3DDEVINFO_VCACHE = record
    Pattern: DWORD;                  { bit pattern, return value must be FOUR_CC('C', 'A', 'C', 'H') }
    OptMethod: DWORD;                { optimization method 0 means longest strips, 1 means vertex cache based }
    CacheSize: DWORD;                { cache size to optimize for  (only required if type is 1) }
    MagicNumber: DWORD;              { used to determine when to restart strips (only required if type is 1)}
  end;
  {$EXTERNALSYM _D3DDEVINFO_VCACHE}
  D3DDEVINFO_VCACHE = _D3DDEVINFO_VCACHE;
  {$EXTERNALSYM D3DDEVINFO_VCACHE}


  PD3DDEVINFO_D3D9PIPELINETIMINGS = ^_D3DDEVINFO_D3D9PIPELINETIMINGS;
  _D3DDEVINFO_D3D9PIPELINETIMINGS = record
    VertexProcessingTimePercent: FLOAT;
    PixelProcessingTimePercent: FLOAT;
    OtherGPUProcessingTimePercent: FLOAT;
    GPUIdleTimePercent: FLOAT;
  end;
  {$EXTERNALSYM _D3DDEVINFO_D3D9PIPELINETIMINGS}
  D3DDEVINFO_D3D9PIPELINETIMINGS = _D3DDEVINFO_D3D9PIPELINETIMINGS;
  {$EXTERNALSYM D3DDEVINFO_D3D9PIPELINETIMINGS}


  PD3DDEVINFO_D3D9INTERFACETIMINGS = ^_D3DDEVINFO_D3D9INTERFACETIMINGS;
  _D3DDEVINFO_D3D9INTERFACETIMINGS = record
    WaitingForGPUToUseApplicationResourceTimePercent: FLOAT;
    WaitingForGPUToAcceptMoreCommandsTimePercent: FLOAT;
    WaitingForGPUToStayWithinLatencyTimePercent: FLOAT;
    WaitingForGPUExclusiveResourceTimePercent: FLOAT;
    WaitingForGPUOtherTimePercent: FLOAT;
  end;
  {$EXTERNALSYM _D3DDEVINFO_D3D9INTERFACETIMINGS}
  D3DDEVINFO_D3D9INTERFACETIMINGS = _D3DDEVINFO_D3D9INTERFACETIMINGS;
  {$EXTERNALSYM D3DDEVINFO_D3D9INTERFACETIMINGS}


  PD3DDEVINFO_D3D9STAGETIMINGS = ^_D3DDEVINFO_D3D9STAGETIMINGS;
  _D3DDEVINFO_D3D9STAGETIMINGS = record
    MemoryProcessingPercent: FLOAT;
    ComputationProcessingPercent: FLOAT;
  end;
  {$EXTERNALSYM _D3DDEVINFO_D3D9STAGETIMINGS}
  D3DDEVINFO_D3D9STAGETIMINGS = _D3DDEVINFO_D3D9STAGETIMINGS;
  {$EXTERNALSYM D3DDEVINFO_D3D9STAGETIMINGS}


  PD3DDEVINFO_D3D9BANDWIDTHTIMINGS = ^_D3DDEVINFO_D3D9BANDWIDTHTIMINGS;
  _D3DDEVINFO_D3D9BANDWIDTHTIMINGS = record
    MaxBandwidthUtilized: FLOAT;
    FrontEndUploadMemoryUtilizedPercent: FLOAT;
    VertexRateUtilizedPercent: FLOAT;
    TriangleSetupRateUtilizedPercent: FLOAT;
    FillRateUtilizedPercent: FLOAT;
  end;
  {$EXTERNALSYM _D3DDEVINFO_D3D9BANDWIDTHTIMINGS}
  D3DDEVINFO_D3D9BANDWIDTHTIMINGS = _D3DDEVINFO_D3D9BANDWIDTHTIMINGS;
  {$EXTERNALSYM D3DDEVINFO_D3D9BANDWIDTHTIMINGS}


  PD3DDEVINFO_D3D9CACHEUTILIZATION = ^_D3DDEVINFO_D3D9CACHEUTILIZATION;
  _D3DDEVINFO_D3D9CACHEUTILIZATION = record
    TextureCacheHitRate: FLOAT;     // Percentage of cache hits
    PostTransformVertexCacheHitRate: FLOAT;
  end;
  {$EXTERNALSYM _D3DDEVINFO_D3D9CACHEUTILIZATION}
  D3DDEVINFO_D3D9CACHEUTILIZATION = _D3DDEVINFO_D3D9CACHEUTILIZATION;
  {$EXTERNALSYM D3DDEVINFO_D3D9CACHEUTILIZATION}


  //* D3D9Ex only -- */
  //{$IFDEF D3D_DISABLE_9EX}

  PD3DMEMORYPRESSURE = ^_D3DMEMORYPRESSURE;
  _D3DMEMORYPRESSURE = record
    BytesEvictedFromProcess: UINT64;
    SizeOfInefficientAllocation: UINT64;
    LevelOfEfficiency: DWORD;
  end;
  {$EXTERNALSYM _D3DMEMORYPRESSURE}
  D3DMEMORYPRESSURE = _D3DMEMORYPRESSURE;
  {$EXTERNALSYM D3DMEMORYPRESSURE}


  PD3DCOMPOSERECTSOP = ^_D3DCOMPOSERECTSOP;
  _D3DCOMPOSERECTSOP            = (
    D3DCOMPOSERECTS_COPY        = 1,
    D3DCOMPOSERECTS_OR          = 2,
    D3DCOMPOSERECTS_AND         = 3,
    D3DCOMPOSERECTS_NEG         = 4,
    D3DCOMPOSERECTS_FORCE_DWORD = FORCEDWORD);  { force 32-bit size enum }
  {$EXTERNALSYM _D3DCOMPOSERECTSOP}
  D3DCOMPOSERECTSOP = _D3DCOMPOSERECTSOP;
  {$EXTERNALSYM D3DCOMPOSERECTSOP}


  PD3DCOMPOSERECTDESC = ^_D3DCOMPOSERECTDESC;
  _D3DCOMPOSERECTDESC = record
    X: USHORT;
    Y: USHORT;                       // Top-left coordinates of a rect in the source surface
    Width: USHORT;
    Height: USHORT;                  // Dimensions of the rect
  end;
  {$EXTERNALSYM _D3DCOMPOSERECTDESC}
  D3DCOMPOSERECTDESC = _D3DCOMPOSERECTDESC;
  {$EXTERNALSYM D3DCOMPOSERECTDESC}


  PD3DCOMPOSERECTDESTINATION = ^_D3DCOMPOSERECTDESTINATION;
  _D3DCOMPOSERECTDESTINATION = record
    SrcRectIndex: USHORT;            // Index of D3DCOMPOSERECTDESC
    Reserved: USHORT;                // For alignment
    X: SHORT;
    Y: SHORT;                        // Top-left coordinates of the rect in the destination surface
  end;
  {$EXTERNALSYM _D3DCOMPOSERECTDESTINATION}
  D3DCOMPOSERECTDESTINATION = _D3DCOMPOSERECTDESTINATION;
  {$EXTERNALSYM D3DCOMPOSERECTDESTINATION}


const

  D3DCOMPOSERECTS_MAXNUMRECTS         = $FFFF;
  {$EXTERNALSYM D3DCOMPOSERECTS_MAXNUMRECTS}
  D3DCONVOLUTIONMONO_MAXWIDTH         = 7;
  {$EXTERNALSYM D3DCONVOLUTIONMONO_MAXWIDTH}
  D3DCONVOLUTIONMONO_MAXHEIGHT        = D3DCONVOLUTIONMONO_MAXWIDTH;
  {$EXTERNALSYM D3DCONVOLUTIONMONO_MAXHEIGHT}
  D3DFMT_A1_SURFACE_MAXWIDTH          = 8192;
  {$EXTERNALSYM D3DFMT_A1_SURFACE_MAXWIDTH}
  D3DFMT_A1_SURFACE_MAXHEIGHT         = 2048;
  {$EXTERNALSYM D3DFMT_A1_SURFACE_MAXHEIGHT}


type

  PD3DPRESENTSTATS = ^_D3DPRESENTSTATS;
  _D3DPRESENTSTATS = record
    PresentCount: UINT;
    PresentRefreshCount: UINT;
    SyncRefreshCount: UINT;
    SyncQPCTime: LARGE_INTEGER;
    SyncGPUTime: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _D3DPRESENTSTATS}
  D3DPRESENTSTATS = _D3DPRESENTSTATS;
  {$EXTERNALSYM D3DPRESENTSTATS}


  PD3DSCANLINEORDERING = ^D3DSCANLINEORDERING;
  D3DSCANLINEORDERING               = (
    D3DSCANLINEORDERING_UNKNOWN     = 0,
    D3DSCANLINEORDERING_PROGRESSIVE = 1,
    D3DSCANLINEORDERING_INTERLACED  = 2);
  {$EXTERNALSYM D3DSCANLINEORDERING}


  PD3DDISPLAYMODEEX = ^D3DDISPLAYMODEEX;
  D3DDISPLAYMODEEX = record
    Size: UINT;
    Width: UINT;
    Height: UINT;
    RefreshRate: UINT;
    Format: D3DFORMAT;
    ScanLineOrdering: D3DSCANLINEORDERING;
  end;
  {$EXTERNALSYM D3DDISPLAYMODEEX}


  PD3DDISPLAYMODEFILTER = ^D3DDISPLAYMODEFILTER;
  D3DDISPLAYMODEFILTER = record
    Size: UINT;
    Format: D3DFORMAT;
    ScanLineOrdering: D3DSCANLINEORDERING;
  end;
  {$EXTERNALSYM D3DDISPLAYMODEFILTER}


  PD3DDISPLAYROTATION = ^D3DDISPLAYROTATION;
  D3DDISPLAYROTATION            = (
    D3DDISPLAYROTATION_IDENTITY = 1,  // No rotation.
    {$EXTERNALSYM D3DDISPLAYROTATION_IDENTITY}
    D3DDISPLAYROTATION_90       = 2,  // Rotated 90 degrees.
    {$EXTERNALSYM D3DDISPLAYROTATION_90}
    D3DDISPLAYROTATION_180      = 3,  // Rotated 180 degrees.
    {$EXTERNALSYM D3DDISPLAYROTATION_180}
    D3DDISPLAYROTATION_270      = 4
    {$EXTERNALSYM D3DDISPLAYROTATION_270}
  ); // Rotated 270 degrees.
  {$EXTERNALSYM D3DDISPLAYROTATION}


const

  //* For use in ID3DResource9::SetPriority calls */
  {$EXTERNALSYM D3D9_RESOURCE_PRIORITY_MINIMUM}
  {$EXTERNALSYM D3D9_RESOURCE_PRIORITY_MINIMUM}
  D3D9_RESOURCE_PRIORITY_MINIMUM      = $28000000;
  D3D9_RESOURCE_PRIORITY_LOW          = $50000000;
  {$EXTERNALSYM D3D9_RESOURCE_PRIORITY_LOW}
  D3D9_RESOURCE_PRIORITY_NORMAL       = $78000000;
  {$EXTERNALSYM D3D9_RESOURCE_PRIORITY_NORMAL}
  D3D9_RESOURCE_PRIORITY_HIGH         = $A0000000;
  {$EXTERNALSYM D3D9_RESOURCE_PRIORITY_HIGH}
  D3D9_RESOURCE_PRIORITY_MAXIMUM      = $C8000000;
  {$EXTERNALSYM D3D9_RESOURCE_PRIORITY_MAXIMUM}

  D3D_OMAC_SIZE                       = 16;
  {$EXTERNALSYM D3D_OMAC_SIZE}


type

  PD3D_OMAC = ^_D3D_OMAC;
  _D3D_OMAC = record
    Omac: array[0..D3D_OMAC_SIZE - 1] of Byte;
  end;
  {$EXTERNALSYM _D3D_OMAC}
  D3D_OMAC = _D3D_OMAC;


  PD3DAUTHENTICATEDCHANNELTYPE = ^_D3DAUTHENTICATEDCHANNELTYPE;
  _D3DAUTHENTICATEDCHANNELTYPE              = (
    D3DAUTHENTICATEDCHANNEL_D3D9            = 1,
    D3DAUTHENTICATEDCHANNEL_DRIVER_SOFTWARE = 2,
    D3DAUTHENTICATEDCHANNEL_DRIVER_HARDWARE = 3);
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNELTYPE}
  D3DAUTHENTICATEDCHANNELTYPE = _D3DAUTHENTICATEDCHANNELTYPE;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNELTYPE}


  PD3DAUTHENTICATEDCHANNEL_QUERY_INPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERY_INPUT = record
    QueryType: TGUID;
    hChannel: THandle;
    SequenceNumber: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERY_INPUT}
  D3DAUTHENTICATEDCHANNEL_QUERY_INPUT = _D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERY_INPUT}


  PD3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT = record
    omac: D3D_OMAC;
    QueryType: TGUID;
    hChannel: THandle;
    SequenceNumber: UINT;
    ReturnCode: HResult;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_PROTECTION : TGUID = (D1: $a84eb584;
                                              D2: $c495;
                                              D3: $48aa;
                                              D4: ($b9, $4d, $8b, $d2, $d6, $fb, $ce, $05));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_PROTECTION}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYPROTECTION_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYPROTECTION_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS = record
  public
      Value: Integer;
    private
      function ReadBits(const iIndex: Integer): Integer;
      procedure WriteBits(const iIndex: Integer; const iValue: Integer);
    public
      property ProtectionEnabled: Integer index $0001 read ReadBits write WriteBits;           // 1 bit at offset 0
      property OverlayOrFullscreenRequired: Integer index $0002 read ReadBits write WriteBits; // 1 bit at offset 1
      property Reserved: Integer index $0003 read ReadBits write WriteBits;                    // 30 bits at offset 3
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS}
  D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS  = _D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS;

  _D3DAUTHENTICATEDCHANNEL_QUERYPROTECTION_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    ProtectionFlags: D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS;
  end;
  D3DAUTHENTICATEDCHANNEL_QUERYPROTECTION_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYPROTECTION_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYPROTECTION_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_CHANNELTYPE : TGUID = (D1: $bc1b18a5;
                                               D2: $b1fb;
                                               D3: $42ab;
                                               D4: ($bd, $94, $b5, $82, $8b, $4b, $f7, $be));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_CHANNELTYPE}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    ChannelType: D3DAUTHENTICATEDCHANNELTYPE;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYCHANNELTYPE_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_DEVICEHANDLE : TGUID = (D1: $ec1c539d;
                                                D2: $8cff;
                                                D3: $4e2a;
                                                D4: ($bc, $c4, $f5, $69, $2f, $99, $f4, $80));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_DEVICEHANDLE}


type

  PD3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT = ^D3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    DeviceHandle: THandle;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYDEVICEHANDLE_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_CRYPTOSESSION : TGUID = (D1: $2634499e;
                                                 D2: $d018;
                                                 D3: $4d74;
                                                 D4: ($ac, $17, $7f, $72, $40, $59, $52, $8d));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_CRYPTOSESSION}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT = record
    Input: D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
    DXVA2DecodeHandle: THandle;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT = _D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_INPUT}


  PD3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    DXVA2DecodeHandle: THandle;
    CryptoSessionHandle: THandle;
    DeviceHandle: THandle;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYCRYPTOSESSION_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_RESTRICTEDSHAREDRESOURCEPROCESSCOUNT : TGUID = (D1: $db207b3;
                                                                        D2: $9450;
                                                                        D3: $46a6;
                                                                        D4: ($82, $de, $1b, $96, $d4, $4f, $9c, $f2));
   {$EXTERNALSYM D3DAUTHENTICATEDQUERY_RESTRICTEDSHAREDRESOURCEPROCESSCOUNT}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    NumRestrictedSharedResourceProcesses: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESSCOUNT_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_RESTRICTEDSHAREDRESOURCEPROCESS : TGUID = (D1: $649bbadb;
                                                                   D2: $f0f4;
                                                                   D3: $4639;
                                                                   D4: ($a1, $5b, $24, $39, $3f, $c3, $ab, $ac));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_RESTRICTEDSHAREDRESOURCEPROCESS}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT = record
    Input: D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
    ProcessIndex: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT = _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_INPUT}


  PD3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE = ^_D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE;
  _D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE = (
    PROCESSIDTYPE_UNKNOWN = 0,
    PROCESSIDTYPE_DWM     = 1,
    PROCESSIDTYPE_HANDLE  = 2);
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE}
  D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE = _D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE}


  PD3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    ProcessIndex: UINT;
    ProcessIdentifer: D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE;
    ProcessHandle: THandle;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYRESTRICTEDSHAREDRESOURCEPROCESS_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_UNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT : TGUID = (D1: $12f0bd6;
                                                                            D2: $e662;
                                                                            D3: $4474;
                                                                            D4: ($be, $fd, $aa, $53, $e5, $14, $3c, $6d));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_UNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    NumUnrestrictedProtectedSharedResources: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYUNRESTRICTEDPROTECTEDSHAREDRESOURCECOUNT_OUTPUT}


const

 D3DAUTHENTICATEDQUERY_OUTPUTIDCOUNT : TGUID = (D1: $2c042b5e;
                                                D2: $8c07;
                                                D3: $46d5;
                                                D4: ($aa, $be, $8f, $75, $cb, $ad, $4c, $31));
 {$EXTERNALSYM D3DAUTHENTICATEDQUERY_OUTPUTIDCOUNT}


type

  PD3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT = record
    Input: D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
    DeviceHandle: THandle;
    CryptoSessionHandle: THandle;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT = _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_INPUT}


  PD3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    DeviceHandle: THandle;
    CryptoSessionHandle: THandle;
    NumOutputIDs: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTIDCOUNT_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_OUTPUTID : TGUID = (D1: $839ddca3;
                                            D2: $9b4e;
                                            D3: $41e4;
                                            D4: ($b0, $53, $89, $2b, $d2, $a1, $1e, $e7));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_OUTPUTID}


type

  PD3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT = record
    Input: D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
    DeviceHandle: THandle;
    CryptoSessionHandle: THandle;
    OutputIDIndex: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT = _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_INPUT}


  PD3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    DeviceHandle: THandle;
    CryptoSessionHandle: THandle;
    OutputIDIndex: UINT;
    OutputID: UINT64;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYOUTPUTID_OUTPUT}

const

  D3DAUTHENTICATEDQUERY_ACCESSIBILITYATTRIBUTES : TGUID = (D1: $6214d9d2;
                                                           D2: $432c;
                                                           D3: $4abb;
                                                           D4: ($9f, $ce, $21, $6e, $ea, $26, $9e, $3b));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_ACCESSIBILITYATTRIBUTES}


type

  PD3DBUSTYPE = ^D3DBUSTYPE;
  _D3DBUSTYPE                                                   = (
    D3DBUSTYPE_OTHER                                            = $00000000,
    D3DBUSTYPE_PCI                                              = $00000001,
    D3DBUSTYPE_PCIX                                             = $00000002,
    D3DBUSTYPE_PCIEXPRESS                                       = $00000003,
    D3DBUSTYPE_AGP                                              = $00000004,
    D3DBUSIMPL_MODIFIER_INSIDE_OF_CHIPSET                       = $00010000,
    D3DBUSIMPL_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_CHIP          = $00020000,
    D3DBUSIMPL_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_SOCKET        = $00030000,
    D3DBUSIMPL_MODIFIER_DAUGHTER_BOARD_CONNECTOR                = $00040000,
    D3DBUSIMPL_MODIFIER_DAUGHTER_BOARD_CONNECTOR_INSIDE_OF_NUAE = $00050000,
    D3DBUSIMPL_MODIFIER_NON_STANDARD                            = Integer($80000000));
  {$EXTERNALSYM _D3DBUSTYPE}
  D3DBUSTYPE = _D3DBUSTYPE;
  {$EXTERNALSYM D3DBUSTYPE}


  PD3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    BusType: D3DBUSTYPE;
    bAccessibleInContiguousBlocks: BOOL;
    bAccessibleInNonContiguousBlocks: BOOL;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYINFOBUSTYPE_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_ENCRYPTIONWHENACCESSIBLEGUIDCOUNT : TGUID = (D1: $b30f7066;
                                                                     D2: $203c;
                                                                     D3: $4b07;
                                                                     D4: ($93, $fc, $ce, $aa, $fd, $61, $24, $1e));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_ENCRYPTIONWHENACCESSIBLEGUIDCOUNT}


type

  PD3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    NumEncryptionGuids: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUIDCOUNT_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_ENCRYPTIONWHENACCESSIBLEGUID : TGUID = (D1: $f83a5958;
                                                                D2: $e986;
                                                                D3: $4bda;
                                                                D4: ($be, $b0, $41, $1f, $6a, $7a, $01, $b7));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_ENCRYPTIONWHENACCESSIBLEGUID}


type

  PD3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT = record
    Input: D3DAUTHENTICATEDCHANNEL_QUERY_INPUT;
    EncryptionGuidIndex: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT = _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_INPUT}


  PD3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    EncryptionGuidIndex: UINT;
    EncryptionGuid: TGUID;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYEVICTIONENCRYPTIONGUID_OUTPUT}


const

  D3DAUTHENTICATEDQUERY_CURRENTENCRYPTIONWHENACCESSIBLE : TGUID = (D1: $ec1791c7;
                                                                   D2: $dad3;
                                                                   D3: $4f15;
                                                                   D4: ($9e, $c3, $fa, $a9, $3d, $60, $d4, $f0));
  {$EXTERNALSYM D3DAUTHENTICATEDQUERY_CURRENTENCRYPTIONWHENACCESSIBLE}

type

  PD3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT = record
    Output: D3DAUTHENTICATEDCHANNEL_QUERY_OUTPUT;
    EncryptionGuid: TGUID;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT = _D3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_QUERYUNCOMPRESSEDENCRYPTIONLEVEL_OUTPUT}


  PD3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT = ^_D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
  _D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT = record
    omac: D3D_OMAC;
    ConfigureType: TGUID;
    hChannel: THandle;
    SequenceNumber: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT}
  D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT = _D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT}


  PD3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT = ^_D3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT;
  _D3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT = record
    omac: D3D_OMAC;
    ConfigureType: TGUID;
    hChannel: THandle;
    SequenceNumber: UINT;
    ReturnCode: HResult;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT}
  D3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT = _D3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGURE_OUTPUT}

const

  D3DAUTHENTICATEDCONFIGURE_INITIALIZE : TGUID = (D1: $6114bdb;
                                                  D2: $3523;
                                                  D3: $470a;
                                                  D4: ($8d, $ca, $fb, $c2, $84, $51, $54, $f0));
  {$EXTERNALSYM D3DAUTHENTICATEDCONFIGURE_INITIALIZE}


type

  PD3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE = ^_D3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE;
  _D3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE = record
    Parameters: D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
    StartSequenceQuery: UINT;
    StartSequenceConfigure: UINT;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE}
  D3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE = _D3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGUREINITIALIZE}

const

  D3DAUTHENTICATEDCONFIGURE_PROTECTION : TGUID = (D1: $50455658;
                                                  D2: $3f47;
                                                  D3: $4362;
                                                  D4: ($bf, $99, $bf, $df, $cd, $e9, $ed, $29));
  {$EXTERNALSYM D3DAUTHENTICATEDCONFIGURE_PROTECTION}


type

  PD3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION = ^_D3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION;
  _D3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION = record
    Parameters: D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
    Protections: D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION}
  D3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION = _D3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGUREPROTECTION}

const

  D3DAUTHENTICATEDCONFIGURE_CRYPTOSESSION : TGUID = (D1: $6346cc54;
                                                     D2: $2cfc;
                                                     D3: $4ad4;
                                                     D4: ($82, $24, $d1, $58, $37, $de, $77, $00));
   {$EXTERNALSYM D3DAUTHENTICATEDCONFIGURE_CRYPTOSESSION}

type

  PD3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION = ^_D3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION;
  _D3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION = record
    Parameters: D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
    DXVA2DecodeHandle: THandle;
    CryptoSessionHandle: THandle;
    DeviceHandle: THandle;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION}
  D3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION = _D3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGURECRYPTOSESSION}

const

  D3DAUTHENTICATEDCONFIGURE_SHAREDRESOURCE : TGUID = (D1: $772d047;
                                                      D2: $1b40;
                                                      D3: $48e8;
                                                      D4: ($9c, $a6, $b5, $f5, $10, $de, $9f, $01));
  {$EXTERNALSYM D3DAUTHENTICATEDCONFIGURE_SHAREDRESOURCE}


type

  PD3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE = ^_D3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE;
  _D3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE = record
    Parameters: D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
    ProcessIdentiferType: D3DAUTHENTICATEDCHANNEL_PROCESSIDENTIFIERTYPE;
    ProcessHandle: THandle;
    AllowAccess: BOOL;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE}
  D3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE = _D3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGURESHAREDRESOURCE}

const

  D3DAUTHENTICATEDCONFIGURE_ENCRYPTIONWHENACCESSIBLE : TGUID = (D1: $41fff286;
                                                                D2: $6ae0;
                                                                D3: $4d43;
                                                                D4: ($9d, $55, $a4, $6e, $9e, $fd, $15, $8a));
  {$EXTERNALSYM D3DAUTHENTICATEDCONFIGURE_ENCRYPTIONWHENACCESSIBLE}


type

  PD3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION = ^_D3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION;
  _D3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION = record
    Parameters: D3DAUTHENTICATEDCHANNEL_CONFIGURE_INPUT;
    EncryptionGuid: TGUID;
  end;
  {$EXTERNALSYM _D3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION}
  D3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION = _D3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION;
  {$EXTERNALSYM D3DAUTHENTICATEDCHANNEL_CONFIGUREUNCOMPRESSEDENCRYPTION}


  PD3DENCRYPTED_BLOCK_INFO = ^_D3DENCRYPTED_BLOCK_INFO;
  _D3DENCRYPTED_BLOCK_INFO = record
    NumEncryptedBytesAtBeginning: UINT;
    NumBytesInSkipPattern: UINT;
    NumBytesInEncryptPattern: UINT;
  end;
  {$EXTERNALSYM _D3DENCRYPTED_BLOCK_INFO}
  D3DENCRYPTED_BLOCK_INFO = _D3DENCRYPTED_BLOCK_INFO;
  {$EXTERNALSYM D3DENCRYPTED_BLOCK_INFO}


  PD3DAES_CTR_IV = ^_D3DAES_CTR_IV;
  _D3DAES_CTR_IV = record
    IV: UINT64;                     // Big-Endian IV
    Count: UINT64;                  // Big-Endian Block Count
  end;
  {$EXTERNALSYM _D3DAES_CTR_IV}
  D3DAES_CTR_IV = _D3DAES_CTR_IV;
  {$EXTERNALSYM D3DAES_CTR_IV}

  //{$ENDIF}   // D3D_DISABLE_9EX

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  function _D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS.ReadBits(const iIndex: Integer): Integer;
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;

  begin
    NrBits:= iIndex and $FF;
    Offset:= iIndex shr 8;
    Mask:= ((1 shl NrBits) - 1);
    Result:= (Value shr Offset) and Mask;
  end;

  procedure _D3DAUTHENTICATEDCHANNEL_PROTECTION_FLAGS.WriteBits(const iIndex: Integer; const iValue: Integer);
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;

  begin
    NrBits:= iIndex and $FF;
    Offset:= iIndex shr 8;
    Mask:= ((1 shl NrBits) - 1);
    Assert(iValue <= Mask);
    Value:= (Value and (not (UINT(Mask) shl UINT(Offset)))) or (UINT(iValue) shl UINT(Offset));
  end;



  function D3DCOLOR_ARGB(a: Int32; r: Int32;
                         g: Int32; b: Int32): D3DCOLOR;
    begin
      Result:= ((a AND $FF) shl 24) OR ((r AND $FF) shl 16) OR ((g AND $FF) shl 8) OR (b AND $FF);
    end;


  function D3DCOLOR_RGBA(r: Int32; g: Int32;
                         b: Int32; a: Int32): D3DCOLOR;
  begin
    Result:= D3DCOLOR_ARGB (a, r, g, b);
  end;


  function D3DCOLOR_XRGB(r: Int32; g: Int32;
                         b: Int32): D3DCOLOR;
  begin
    Result:= D3DCOLOR_ARGB ($FF, r, g, b);
  end;


  function D3DCOLOR_XYUV(y: Int32; u: Int32;
                         v: Int32): D3DCOLOR;
  begin
    Result:= D3DCOLOR_ARGB ($FF, y, u, v);
  end;


  function D3DCOLOR_AYUV(a: Int32; y: Int32;
                         u: Int32; v: Int32): D3DCOLOR;
  begin
    Result:= D3DCOLOR_ARGB(a, y, u, v);
  end;


  function D3DCOLOR_COLORVALUE(r: FLOAT; g: FLOAT;
                               b: FLOAT; a: FLOAT): D3DCOLOR;
  begin
    Result:= D3DCOLOR_RGBA(Trunc(r * 255), Trunc(g * 255),
                           Trunc(b * 255), Trunc(a * 255));
  end;


  function D3DTS_WORLDMATRIX(index: Int32): D3DTRANSFORMSTATETYPE;
  begin
    Result:= D3DTRANSFORMSTATETYPE(index + 256);
  end;

  function D3DPS_VERSION(_Major: DWORD;
                         _Minor: DWORD): DWORD;
  begin
    Result:= $FFFF0000 OR (_Major shl 8) OR _Minor;
  end;


  function D3DVS_VERSION(_Major: DWORD;
                         _Minor: DWORD): Cardinal;
  begin
    Result:= $FFFE0000 OR (_Major shl 8) OR _Minor;
  end;


  function D3DSHADER_VERSION_MAJOR(_Version: DWORD): DWORD;
  begin
    Result:= (_Version shr 8) AND $FF;
  end;


  function D3DSHADER_VERSION_MINOR(_Version: DWORD): DWORD;
  begin
    Result:= (_Version shr 0) AND $FF;
  end;


  function D3DSHADER_COMMENT(_DWordSize: DWORD): DWORD;
  begin
    Result:= ((_DWordSize shl D3DSI_COMMENTSIZE_SHIFT)
               AND D3DSI_COMMENTSIZE_MASK)
               OR DWORD(D3DSIO_COMMENT);
  end;


  function D3DFVF_TEXCOORDSIZE3(CoordIndex: DWord): DWord; inline;
  begin
    Result:= D3DFVF_TEXTUREFORMAT3 shl (CoordIndex * 2 + 16);
  end;


  function D3DFVF_TEXCOORDSIZE2(CoordIndex: DWord): DWord; inline;
  begin
    Result:= D3DFVF_TEXTUREFORMAT2;
  end;


  function D3DFVF_TEXCOORDSIZE4(CoordIndex: DWord): DWord; inline;
  begin
    Result:= D3DFVF_TEXTUREFORMAT4 shl (CoordIndex * 2 + 16);
  end;


  function D3DFVF_TEXCOORDSIZE1(CoordIndex: DWord): DWord; inline;
  begin
    Result:= D3DFVF_TEXTUREFORMAT1 shl (CoordIndex * 2 + 16);
  end;


  function MAKEFOURCC(ch0: AnsiChar;
                      ch1: AnsiChar;
                      ch2: AnsiChar;
                      ch3: AnsiChar): DWORD; inline;
  begin
    Result:= DWORD(Ord(ch0)) OR
             (DWORD(Ord(ch1)) shl 8) OR
             (DWORD(Ord(ch2)) shl 16) OR
             (DWORD(Ord(ch3)) shl 24 );
  end;

  // Implement Additional functions here.

end.

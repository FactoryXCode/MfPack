// FactoryX
//
// Copyright © by FactoryX. All rights reserved.
//
// Project: Media Foundation - MfPack
// Project location: http://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApiTypes.inc
// Kind: Delphi include file
// Release date: 04-01-2016
// Language: ENU
//
// Revision Version: 3.0.1
// Description: Holds compiler definitions used by Media Foundation,
//              WASAPI and Core Audio.
//
//
//              Example: {$INCLUDE 'WinApiTypes.inc'}
//
// Initiator(s): Tony (maXcomX), Peter (OzShips)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
// 15/09/2020 Tony                Added support for HPSTR type
//------------------------------------------------------------------------------
//
// Remarks: All future updates (releases) will be named to an artist except for
//          the major Windows updates. So, we started with ISÁK.
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
// Todo: -
// =============================================================================
// Source: -
//
//
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================


const
//
// Preferred macros used in the Windows header files.
// If you define (MFPACK_)NTDDI_VERSION, you must also define (MFPACK)_WIN32_WINNT.
//
// The macros and compiler directives for all windows versions are described in
// sdkddkver.h (Master include file for versioning windows SDK/DDK.)
//
// Value for NTDDI_VERSION 	          | Minimum system required
//------------------------------------+-----------------------------------------

  MFPACK_NTDDI_WIN10_VB     = $0A000008; // Windows 10 2004
  MFPACK_NTDDI_WIN10_19H1   = $0A000007; // Windows 10 19H1
  MFPACK_NTDDI_WIN10_RS5    = $0A000006; // Windows 10 Redstone 5
  MFPACK_NTDDI_WIN10_RS4    = $0A000005; // Windows 10 Redstone 4
  MFPACK_NTDDI_WIN10_RS3    = $0A000004; // Windows 10 Redstone 3
  MFPACK_NTDDI_WIN10_RS2    = $0A000003; // Windows 10 Redstone 2
  MFPACK_NTDDI_WIN10_RS1    = $0A000002; // Windows 10 Redstone 1
  MFPACK_NTDDI_WIN10        = $0A000001; // Windows 10
  MFPACK_NTDDI_WINTHRESHOLD = $0A000000; // Windows 10
  MFPACK_NTDDI_WINBLUE      = $06030000; // Windows 8.1
  MFPACK_NTDDI_WIN8         = $06020000; // Windows 8
  MFPACK_NTDDI_WIN7         = $06010000; // Windows 7
  MFPACK_NTDDI_WS08         = $06000100; // Windows Server 2008
  MFPACK_NTDDI_VISTASP1     = $06000100; // Windows Vista with Service Pack 1 (SP1)
  MFPACK_NTDDI_VISTA        = $06000000; // Windows Vista
  MFPACK_NTDDI_WS03SP2      = $05020200; // Windows Server 2003 with Service Pack 2 (SP2)
  MFPACK_NTDDI_WS03SP1      = $05020100; // Windows Server 2003 with Service Pack 1 (SP1)
  MFPACK_NTDDI_WS03         = $05020000; // Windows Server 2003
  MFPACK_NTDDI_WINXPSP3     = $05010300; // Windows XP with Service Pack 3 (SP3)
  MFPACK_NTDDI_WINXPSP2     = $05010200; // Windows XP with Service Pack 2 (SP2)
  MFPACK_NTDDI_WINXPSP1     = $05010100; // Windows XP with Service Pack 1 (SP1)
  MFPACK_NTDDI_WINXP        = $05010000; // Windows XP

// _WIN32_WINNT version constants
//
  MFPACK_WIN32_WINNT_NT4            = $0400; // Windows NT 4.0
  MFPACK_WIN32_WINNT_WIN2K          = $0500; // Windows 2000
  MFPACK_WIN32_WINNT_WINXP          = $0501; // Windows XP
  MFPACK_WIN32_WINNT_WS03           = $0502; // Windows Server 2003
  MFPACK_WIN32_WINNT_WIN6           = $0600; // Windows Vista
  MFPACK_WIN32_WINNT_VISTA          = $0600; // Windows Vista
  MFPACK_WIN32_WINNT_WS08           = $0600; // Windows Server 2008
  MFPACK_WIN32_WINNT_LONGHORN       = $0600; // Windows Vista

  // Supported versions
  MFPACK_WIN32_WINNT_WIN7           = $06010000; // Windows 7
  MFPACK_WIN32_WINNT_WIN8           = $06020000; // Windows 8
  MFPACK_WIN32_WINNT_WINBLUE        = $06030000; // Windows 8.1
  MFPACK_WIN32_WINNT_WINTHRESHOLD   = $0A000000; // Windows 10 1507 "Threshold"
  MFPACK_WIN32_WINNT_WIN10          = $0A000001; // Windows 10 1511 "Threshold 2"
  MFPACK_WIN32_WINNT_WIN10_RS1      = $0A000002; // Windows 10 1607 "Redstone 1"
  MFPACK_WIN32_WINNT_WIN10_RS2      = $0A000003; // Windows 10 1703 "Redstone 2"
  MFPACK_WIN32_WINNT_WIN10_RS3      = $0A000004; // Windows 10 1709 "Redstone 3"
  MFPACK_WIN32_WINNT_WIN10_RS4      = $0A000005; // Windows 10 1803 "Redstone 4"
  MFPACK_WIN32_WINNT_WIN10_RS5      = $0A000006; // Windows 10 1809 "Redstone 5"
  MFPACK_WIN32_WINNT_WIN10_19H1     = $0A000007; // Windows 10 1903 "19H1"
  MFPACK_WIN32_WINNT_WIN10_VB       = $0A000008; // Windows 10 2004


//
// _WIN32_WINNT version constants from sdkddkver.h
//
  _WIN32_WINNT_NT4                    = $0400;
  {$EXTERNALSYM _WIN32_WINNT_NT4}
  _WIN32_WINNT_WIN2K                  = $0500;
  {$EXTERNALSYM _WIN32_WINNT_WIN2K}
  _WIN32_WINNT_WINXP                  = $0501;
  {$EXTERNALSYM _WIN32_WINNT_WINXP}
  _WIN32_WINNT_WS03                   = $0502;
  {$EXTERNALSYM _WIN32_WINNT_WS03}
  _WIN32_WINNT_WIN6                   = $0600;
  {$EXTERNALSYM _WIN32_WINNT_WIN6}
  _WIN32_WINNT_VISTA                  = $0600;
  {$EXTERNALSYM _WIN32_WINNT_VISTA}
  _WIN32_WINNT_WS08                   = $0600;
  {$EXTERNALSYM _WIN32_WINNT_WS08}
  _WIN32_WINNT_LONGHORN               = $0600;
  {$EXTERNALSYM _WIN32_WINNT_LONGHORN}
  _WIN32_WINNT_WIN7                   = $0601;
  {$EXTERNALSYM _WIN32_WINNT_WIN7}
  _WIN32_WINNT_WIN8                   = $0602;
  {$EXTERNALSYM _WIN32_WINNT_WIN8}
  _WIN32_WINNT_WINBLUE                = $0603;  // Win 8.1
  {$EXTERNALSYM _WIN32_WINNT_WINBLUE}
  _WIN32_WINNT_WINTHRESHOLD           = $0A00;  // ABRACADABRA_THRESHOLD
  {$EXTERNALSYM _WIN32_WINNT_WINTHRESHOLD}
  _WIN32_WINNT_WIN10                  = $0A00;  // ABRACADABRA_THRESHOLD
  {$EXTERNALSYM _WIN32_WINNT_WIN10}

//
// NTDDI version constants from sdkddkver.h
//
  NTDDI_WIN2K                         = $05000000;
  {$EXTERNALSYM NTDDI_WIN2K}
  NTDDI_WIN2KSP1                      = $05000100;
  {$EXTERNALSYM NTDDI_WIN2KSP1}
  NTDDI_WIN2KSP2                      = $05000200;
  {$EXTERNALSYM NTDDI_WIN2KSP2}
  NTDDI_WIN2KSP3                      = $05000300;
  {$EXTERNALSYM NTDDI_WIN2KSP3}
  NTDDI_WIN2KSP4                      = $05000400;
  {$EXTERNALSYM NTDDI_WIN2KSP4}

  NTDDI_WINXP                         = $05010000;
  {$EXTERNALSYM NTDDI_WINXP}
  NTDDI_WINXPSP1                      = $05010100;
  {$EXTERNALSYM NTDDI_WINXPSP1}
  NTDDI_WINXPSP2                      = $05010200;
  {$EXTERNALSYM NTDDI_WINXPSP2}
  NTDDI_WINXPSP3                      = $05010300;
  {$EXTERNALSYM NTDDI_WINXPSP3}
  NTDDI_WINXPSP4                      = $05010400;
  {$EXTERNALSYM NTDDI_WINXPSP4}

  NTDDI_WS03                          = $05020000;
  {$EXTERNALSYM NTDDI_WS03}
  NTDDI_WS03SP1                       = $05020100;
  {$EXTERNALSYM NTDDI_WS03SP1}
  NTDDI_WS03SP2                       = $05020200;
  {$EXTERNALSYM NTDDI_WS03SP2}
  NTDDI_WS03SP3                       = $05020300;
  {$EXTERNALSYM NTDDI_WS03SP3}
  NTDDI_WS03SP4                       = $05020400;
  {$EXTERNALSYM NTDDI_WS03SP4}

  NTDDI_WIN6                          = $06000000;
  {$EXTERNALSYM NTDDI_WIN6}
  NTDDI_WIN6SP1                       = $06000100;
  {$EXTERNALSYM NTDDI_WIN6SP1}
  NTDDI_WIN6SP2                       = $06000200;
  {$EXTERNALSYM NTDDI_WIN6SP2}
  NTDDI_WIN6SP3                       = $06000300;
  {$EXTERNALSYM NTDDI_WIN6SP3}
  NTDDI_WIN6SP4                       = $06000400;
  {$EXTERNALSYM NTDDI_WIN6SP4}

  NTDDI_VISTA                         = NTDDI_WIN6;
  {$EXTERNALSYM NTDDI_VISTA}
  NTDDI_VISTASP1                      = NTDDI_WIN6SP1;
  {$EXTERNALSYM NTDDI_VISTASP1}
  NTDDI_VISTASP2                      = NTDDI_WIN6SP2;
  {$EXTERNALSYM NTDDI_VISTASP2}
  NTDDI_VISTASP3                      = NTDDI_WIN6SP3;
  {$EXTERNALSYM NTDDI_VISTASP3}
  NTDDI_VISTASP4                      = NTDDI_WIN6SP4;
  {$EXTERNALSYM NTDDI_VISTASP4}

  NTDDI_LONGHORN                      = NTDDI_VISTA;
  {$EXTERNALSYM NTDDI_LONGHORN}

  NTDDI_WS08                          = NTDDI_WIN6SP1;
  {$EXTERNALSYM NTDDI_WS08}
  NTDDI_WS08SP2                       = NTDDI_WIN6SP2;
  {$EXTERNALSYM NTDDI_WS08SP2}
  NTDDI_WS08SP3                       = NTDDI_WIN6SP3;
  {$EXTERNALSYM NTDDI_WS08SP3}
  NTDDI_WS08SP4                       = NTDDI_WIN6SP4;
  {$EXTERNALSYM NTDDI_WS08SP4}

  NTDDI_WIN7                          = $06010000;
  {$EXTERNALSYM NTDDI_WIN7}
  NTDDI_WIN8                          = $06020000;
  {$EXTERNALSYM NTDDI_WIN8}
  NTDDI_WINBLUE                       = $06030000;
  {$EXTERNALSYM NTDDI_WINBLUE}
  NTDDI_WINTHRESHOLD                  = $0A000000;  { ABRACADABRA_THRESHOLD }
  {$EXTERNALSYM NTDDI_WINTHRESHOLD}
  NTDDI_WIN10                         = $0A000000;  { ABRACADABRA_THRESHOLD }
  {$EXTERNALSYM NTDDI_WIN10}
  NTDDI_WIN10_TH2                     = $0A000001;  { ABRACADABRA_WIN10_TH2 }
  {$EXTERNALSYM NTDDI_WIN10_TH2}
  NTDDI_WIN10_RS1                     = $0A000002;  { ABRACADABRA_WIN10_RS1 }
  {$EXTERNALSYM NTDDI_WIN10_RS1}
  NTDDI_WIN10_RS2                     = $0A000003;  { ABRACADABRA_WIN10_RS2 }
  {$EXTERNALSYM NTDDI_WIN10_RS2}
  NTDDI_WIN10_RS3                     = $0A000004;  { ABRACADABRA_WIN10_RS3 }
  {$EXTERNALSYM NTDDI_WIN10_RS3}
  NTDDI_WIN10_RS4                     = $0A000005;  { ABRACADABRA_WIN10_RS4 }
  {$EXTERNALSYM NTDDI_WIN10_RS4}
  NTDDI_WIN10_RS5                     = $0A000006;  { ABRACADABRA_WIN10_RS5 }
  {$EXTERNALSYM NTDDI_WIN10_RS5}
  NTDDI_WIN10_19H1                    = $0A000007;  { ABRACADABRA_WIN10_19H1 }
  {$EXTERNALSYM NTDDI_WIN10_19H1}
  NTDDI_WIN10_VB                      = $0A000008;  { ABRACADABRA_WIN10_VB }
  {$EXTERNALSYM NTDDI_WIN10_VB}
  // Latest WDK version
  WDK_NTDDI_VERSION                   = NTDDI_WIN10_VB;  { ABRACADABRA_WIN10_VB }
  {$EXTERNALSYM WDK_NTDDI_VERSION}


// WINVER
//
  MFPACK_WINVER        = _WIN32_WINNT_WIN10;
  MFPACK_WIN32_WINNT   = _WIN32_WINNT_WIN10;
  MFPACK_NTDDI_VERSION = NTDDI_WIN10_VB;



{$DEFINE MFPACK_INC}

// Since Delphi version 2010 DirectShow9 is included
{$IF CompilerVersion >= 21.0}
  {$DEFINE DELPHI_DSHOW9_INCLUDED}
{$ENDIF}

// Since Delphi 2009 POINTERMATH has been introduced.
// Pointer math is simply treating any given typed pointer in some narrow instances as a
// scaled ordinal where you can perform simple arithmetic operations directly on the pointer variable.
// It also allows you to treat such a pointer variable as an unbounded array using the array [] operator.
//
// The POINTERMATH directive has a local scope.
// That is, if you turn this directive ON and do not turn it off in the module,
// it will remain on only until the end of the module.
// Also, if you declare a typed pointer with this directive ON,
// any variable of that type allows scaled pointer arithmetic and array indexing,
// even after the directive has been turned OFF Similarly,
// any block of code surrounded by this directive allows arithmetic operations for ANY
// typed pointers within the block regardless of whether the typed pointer was
// originally declared with POINTERMATH ON.
//
// This directive affects only typed pointers.
// Variables of type Pointer do not allow the pointer math features,
// since type Pointer is effectively pointing to a void element,
// which is 0 bytes in size.
// Untyped var or const parameters are not affected because they are not really pointers.
//
//{$DEFINE POINTERMATH_ON}


// How to use:
// {$POINTERMATH ON} {$DEFINE POINTERMATH_ON}
// Your code
// {$POINTERMATH OFF} {$UNDEF POINTERMATH_ON}
//
// For instance: your function needs an other type of parameter
// {$IFDEF POINTERMATH_ON}
//   Your code is needing pointer arithmetics
// {$ELSE}
//   Your code does not need pointer arithmetics
// {$ENDIF}
//

// Default
{$POINTERMATH OFF}
{$UNDEF POINTERMATH_ON}
//{$DEFINE POINTERMATH_ON}

// Use compatible mode with mixed WinApi/MfPack code and definitions.
{$DEFINE WINAPI_COMPATIBLE}
//{$UNDEF WINAPI_COMPATIBLE}


// CPU
{$IFNDEF CPU64}
  {$IFDEF WIN32}
    {$DEFINE CPU32}
  {$ELSE}
    {$DEFINE CPU64}
  {$ENDIF}
{$ENDIF}


// MfPack versions
// ===============
// {$DEFINE MFPACK_VERSION_10_UP}
// {$DEFINE MFPACK_VERSION_110_UP}
// {$DEFINE MFPACK_VERSION_111_UP}
// {$DEFINE MFPACK_VERSION_112_UP}
// {$DEFINE MFPACK_VERSION_113_UP}
// {$DEFINE MFPACK_VERSION_201_UP} // Milestone update 1
// {$DEFINE MFPACK_VERSION_202_UP}
// {$DEFINE MFPACK_VERSION_213_UP}
// {$DEFINE MFPACK_VERSION_227}    // Milestone update 2. Some major things changed...
// {$DEFINE MFPACK_VERSION_228_UP} // debug version, never released.
// {$DEFINE MFPACK_VERSION_229_UP} // debug/test version, never released.
// {$DEFINE MFPACK_VERSION_230_UP} // Milestone update 3
// {$DEFINE MFPACK_VERSION_231_UP} // debug version, never released.
// {$DEFINE MFPACK_VERSION_232_UP} // update RedStone 1
// {$DEFINE MFPACK_VERSION_233_UP} // Milestone update 4 (RedStone 3)
// {$DEFINE MFPACK_VERSION_236_UP} // General update
// {$DEFINE MFPACK_VERSION_237_UP} // General update
// {$DEFINE MFPACK_VERSION_240_UP} // Milestone update 5 (RedStone 4)
// {$DEFINE MFPACK_VERSION_250_UP} // Milestone update 6 (RedStone 5)
// {$DEFINE MFPACK_VERSION_251_UP} // Milestone update 7 MfPack update 251.55
// {$DEFINE MFPACK_VERSION_252_UP} // Milestone update 8 MfPack update 252.62
// {$DEFINE MFPACK_VERSION_253_UP} // Milestone update 9 MfPack update 253.66
// {$DEFINE MFPACK_VERSION_254_UP} // Milestone update 10 MfPack update 254.68
// {$DEFINE MFPACK_VERSION_259_UP} // Milestone update 11 WIN10 May 2019 update (19H1))
// {$DEFINE MFPACK_VERSION_260_UP} // ISÁK release.
// {$DEFINE MFPACK_VERSION_261_UP} // Prodigy release.
// {$DEFINE MFPACK_VERSION_262_UP} // Leftfield release.
// {$DEFINE MFPACK_VERSION_263_UP} // Underworld release, Milestone update 12, WIN10 November 2019 Update (19H2))
// {$DEFINE MFPACK_VERSION_264_UP} // Kraftwerk release, Milestone update 13, WIN10 May 2020 pre-update
// {$DEFINE MFPACK_VERSION_264#1_UP} // Kraftwerk #1 Autobahn release, update 13#1, WIN10 May 2020 final update (2004))
// {$DEFINE MFPACK_VERSION_264#2_UP} // Kraftwerk #1 The Model release, update 13#2, WIN10 May 2020 final update (2004))
   {$DEFINE MFPACK_VERSION_300_UP} // Enigma release, Milestone update 14.


// WARNINGS
// enable/disable compiler warnings.
// Set in your IDE evironment or here in case MfPack only should be affected.

// {$WARN BOUNDS_ERROR ON}   // Enable W1012 Constant expression violates subrange bounds
// {$WARN BOUNDS_ERROR OFF}  // Disable W1012 Constant expression violates subrange bounds


// Important IDE settings.
//========================
// If you don't want to force the default IDE settings, you have to alter them here.
// For indepth information please read here: https://docs.microsoft.com/en-us/windows/win32/winprog/using-the-windows-headers
// and this file.


// Range checking
// Default = False
{$R-}

// Overflow checking
// Default = False
{$Q-}

// Assignable typed constants
// Default = False
{$J-}

// Extended syntax
// Default = True
{$X+}

// Strict type checking
// Default = True
{$V+}

// Code inlining control
// Turn this off for Delphi compiler versions <
// Default = ON
{$INLINE ON}

const
// a global version ID (for internal use only).
  MfpVersion   = '3.0.0'; // Release version.



//Definitions WinApiTypes
//=======================
// When encountering declaration issues with rtl definitions,
// you should enable or disable the definitions for
// WinApi.WinApiTypes.pas, etc. here.
// To do so, just (un)comment the definition you (don't) need.



// WinApiTypes.pas
//================


{$DEFINE MFP_DWORD}
// {$UNDEF MFP_DWORD}


// GuidDef.h
//----------
 {$DEFINE MFP_GUID_NULL}
// {$UNDEF MFP_GUID_NULL}

 {$DEFINE MFP_LPGUID}
// {$UNDEF MFP_LPGUID}

 {$DEFINE MFP_LPCGUID}
// {$UNDEF MFP_LPCGUID}

 {$DEFINE MFP_IID}
// {$UNDEF MFP_IID}

 {$DEFINE MFP_CLSID}
// {$UNDEF MFP_CLSID}

 {$DEFINE MFP_FMTID}
// {$UNDEF MFP_FMTID}

 {$DEFINE MFP_REFGUID}
// {$UNDEF MFP_REFGUID}

 {$DEFINE MFP_REFIID}
// {$UNDEF MFP_REFIID}

 {$DEFINE MFP_REFCLSID}
// {$UNDEF MFP_REFCLSID}

 {$DEFINE MFP_REFFMTID}
// {$UNDEF MFP_REFFMTID}

 {$DEFINE MFP_UUID}
// {$UNDEF MFP_UUID}

 {$DEFINE MFP_PID}
// {$UNDEF MFP_PID}

 {$DEFINE MFP_TID}
// {$UNDEF MFP_TID}

 {$DEFINE MFP_PVOID}
// {$UNDEF MFP_PVOID}

 {$DEFINE MFP_LPVOID}
// {$UNDEF MFP_LPVOID}

{$DEFINE MFP_LUID}
// {$UNDEF MFP_LUID}


// WTypesbase.h
//-------------
 {$DEFINE MFP_Hyper}
// {$UNDEF MFP_Hyper}

 {$DEFINE MFP_OLECHAR}
// {$UNDEF MFP_OLECHAR}

 {$DEFINE MFP_POLESTR}
// {$UNDEF MFP_POLESTR}

 {$DEFINE MFP_LPOLESTR}
// {$UNDEF MFP_LPOLESTR}

 {$DEFINE MFP_LPCOLESTR}
// {$UNDEF MFP_LPCOLESTR}

 {$DEFINE MFP_OLE_ENUM}
// {$UNDEF MFP_OLE_ENUM}

 {$DEFINE MFP_UCHAR}
// {$UNDEF MFP_UCHAR}

 {$DEFINE MFP_USHORT}
// {$UNDEF MFP_USHORT}

 {$DEFINE MFP_FOURCC}
// {$UNDEF MFP_FOURCC}

{$DEFINE MFP_ULONG} // Declared in WinApi.Windows
// {$UNDEF MFP_ULONG}

 {$DEFINE MFP_DWORDLONG}
// {$UNDEF MFP_DWORDLONG}

 {$DEFINE MFP_LONGLONG}
// {$UNDEF MFP_LONGLONG}

 {$DEFINE MFP_ULONGLONG}
// {$UNDEF MFP_ULONGLONG}

 {$DEFINE MFP_LARGE_INTEGER}
// {$UNDEF MFP_LARGE_INTEGER}

 {$DEFINE MFP_ULARGE_INTEGER}
// {$UNDEF MFP_ULARGE_INTEGER}

 {$DEFINE MFP_FILETIME}
// {$UNDEF MFP_FILETIME}

 {$DEFINE MFP_SYSTEMTIME}
// {$UNDEF MFP_SYSTEMTIME}

 {$DEFINE MFP_SECURITY_ATTRIBUTES_}
// {$UNDEF MFP_SECURITY_ATTRIBUTES_}

 {$DEFINE MFP_SECURITY_DESCRIPTOR_REVISION}
// {$UNDEF MFP_SECURITY_DESCRIPTOR_REVISION}

 {$DEFINE MFP_SECURITY_DESCRIPTOR_CONTROL}
// {$UNDEF MFP_SECURITY_DESCRIPTOR_CONTROL}

 {$DEFINE MFP_SID}
// {$UNDEF MFP_SID}

 {$DEFINE MFP_SID_AND_ATTRIBUTES}
// {$UNDEF MFP_SID_AND_ATTRIBUTES}

 {$DEFINE MFP_ACL}
// {$UNDEF MFP_ACL}

 {$DEFINE MFP_SECURITY_DESCRIPTOR}
// {$UNDEF MFP_SECURITY_DESCRIPTOR}

 {$DEFINE MFP_COAUTHIDENTITY}
// {$UNDEF MFP_COAUTHIDENTITY}

 {$DEFINE MFP_COAUTHINFO}
// {$UNDEF MFP_COAUTHINFO}

 {$DEFINE MFP_SCODE}
// {$UNDEF MFP_SCODE}

// {$DEFINE MFP_HRESULT}
// {$UNDEF MFP_HRESULT}

 {$DEFINE MFP__OBJECTID}
// {$UNDEF MFP__OBJECTID}

 {$DEFINE MFP_MEMCTX}
// {$UNDEF MFP_MEMCTX}

 {$DEFINE MFP_ROTREGFLAGS}
// {$UNDEF MFP_ROTREGFLAGS}

 {$DEFINE MFP_APPIDREGFLAGS}
// {$UNDEF MFP_APPIDREGFLAGS}

 {$DEFINE MFP_DCOMSCM}
// {$UNDEF MFP_DCOMSCM}

 {$DEFINE MFP_CLSCTX}
// {$UNDEF MFP_CLSCTX}

 {$DEFINE CLSCTX_VALID_MASK}
// {$UNDEF CLSCTX_VALID_MASK}

 {$DEFINE MFP_MSHLFLAGS}
// {$UNDEF MFP_MSHLFLAGS}

 {$DEFINE MFP_MSHCTX}
// {$UNDEF MFP_MSHCTX}

 {$DEFINE MFP_BYTE_BLOB}
// {$UNDEF MFP_BYTE_BLOB}

 {$DEFINE MFP_WORD_BLOB}
// {$UNDEF MFP_WORD_BLOB}

 {$DEFINE MFP_DWORD_BLOB}
// {$UNDEF MFP_DWORD_BLOB}

 {$DEFINE MFP_FLAGGED_BYTE_BLOB}
// {$UNDEF MFP_FLAGGED_BYTE_BLOB}

 {$DEFINE MFP_FLAGGED_WORD_BLOB}
// {$UNDEF MFP_FLAGGED_WORD_BLOB}

 {$DEFINE MFP_BYTE_SIZEDARR}
// {$UNDEF MFP_BYTE_SIZEDARR}

 {$DEFINE MFP_WORD_SIZEDARR}
// {$UNDEF MFP_WORD_SIZEDARR}

 {$DEFINE MFP_DWORD_SIZEDARR}
// {$UNDEF MFP_DWORD_SIZEDARR}

 {$DEFINE MFP_HYPER_SIZEDARR}
// {$UNDEF MFP_HYPER_SIZEDARR}

 {$DEFINE MFP_BLOB}
// {$UNDEF MFP_BLOB}


// wtypes.h
//---------

 {$DEFINE MFP_RemHGLOBAL}
// {$UNDEF MFP_RemHGLOBAL}

 {$DEFINE MFP_RemHMETAFILEPICT}
// {$UNDEF MFP_RemHMETAFILEPICT}

 {$DEFINE MFP_RemHBITMAP}
// {$UNDEF MFP_RemHBITMAP}

 {$DEFINE MFP_RemHPALETTE}
// {$UNDEF MFP_RemHPALETTE}

 {$DEFINE MFP_RemHBRUSH}
// {$UNDEF MFP_RemHBRUSH}

 {$DEFINE MFP_PBYTE}
// {$UNDEF MFP_PBYTE}

 {$DEFINE MFP_PWORD}
// {$UNDEF MFP_PWORD}

 {$DEFINE MFP_UINT}
// {$UNDEF MFP_UINT}

 {$DEFINE MFP_INT}
// {$UNDEF MFP_INT}

 {$DEFINE MFP_BOOL}
// {$UNDEF MFP_BOOL}

 {$DEFINE MFP_LONG}
// {$UNDEF MFP_LONG}

 {$DEFINE MFP_WPARAM}
// {$UNDEF MFP_WPARAM}

 {$DEFINE MFP_LPARAM}
// {$UNDEF MFP_LPARAM}

 {$DEFINE MFP_LRESULT}
// {$UNDEF MFP_LRESULT}

 {$DEFINE MFP_HANDLE}
// {$UNDEF MFP_HANDLE}

 {$DEFINE MFP_HSTRING}
// {$UNDEF MFP_HSTRING}

 {$DEFINE MFP_HMODULE}
// {$UNDEF MFP_HMODULE}

 {$DEFINE MFP_HINSTANCE}
// {$UNDEF MFP_HINSTANCE}

 {$DEFINE MFP_HTASK}
// {$UNDEF MFP_HTASK}

 {$DEFINE MFP_HKEY}
// {$UNDEF MFP_HKEY}

 {$DEFINE MFP_HDESK}
// {$UNDEF MFP_HDESK}

 {$DEFINE MFP_HMF}
// {$UNDEF MFP_HMF}

 {$DEFINE MFP_HEMF}
// {$UNDEF MFP_HEMF}

 {$DEFINE MFP_HPEN}
// {$UNDEF MFP_HPEN}

 {$DEFINE MFP_HRSRC}
// {$UNDEF MFP_HRSRC}

 {$DEFINE MFP_HSTR}
// {$UNDEF MFP_HSTR}

 {$DEFINE MFP_HWINSTA}
// {$UNDEF MFP_HWINSTA}

 {$DEFINE MFP_HKL}
// {$UNDEF MFP_HKL}

 {$DEFINE MFP_HGDIOBJ}
// {$UNDEF MFP_HGDIOBJ}

// {$DEFINE MFP_HMODULE}
// {$UNDEF MFP_HMODULE}

 {$DEFINE MFP_HDWP}
// {$UNDEF MFP_HDWP}

 {$DEFINE MFP_HFILE}
// {$UNDEF MFP_HFILE}

 {$DEFINE MFP_LPWORD}
// {$UNDEF MFP_LPWORD}

 {$DEFINE MFP_LPDWORD}
// {$UNDEF MFP_LPDWORD}

 {$DEFINE MFP_PCHAR}
// {$UNDEF MFP_PCHAR}

 {$DEFINE MFP_LPSTR}
// {$UNDEF MFP_LPSTR}

 {$DEFINE MFP_HPSTR}
// {$UNDEF MFP_HPSTR}

 {$DEFINE MFP_LPCSTR}
// {$UNDEF MFP_LPCSTR}

 {$DEFINE MFP_TCHAR}
// {$UNDEF MFP_TCHAR}
 
 {$DEFINE MFP_WCHAR}
// {$UNDEF MFP_WCHAR}

 {$DEFINE MFP_wchar_t}
// {$UNDEF MFP_wchar_t}

 {$DEFINE MFP_LPWSTR}
// {$UNDEF MFP_LPWSTR}

 {$DEFINE MFP_LPCWSTR}
// {$UNDEF MFP_LPCWSTR}

 {$DEFINE MFP_LPTSTR}
// {$UNDEF MFP_LPTSTR}

 {$DEFINE MFP_PCWSTR}
// {$UNDEF MFP_PCWSTR}

 {$DEFINE MFP_LPCTSTR}
// {$UNDEF MFP_LPCTSTR}

 {$DEFINE MFP_COLORREF}
// {$UNDEF MFP_COLORREF}

 {$DEFINE MFP_LPHANDLE}
// {$UNDEF MFP_LPHANDLE}

 {$DEFINE MFP_POINT}
// {$UNDEF MFP_POINT}

 {$DEFINE MFP_POINTL}
// {$UNDEF MFP_POINTL}

 {$DEFINE MFP_SIZE}
// {$UNDEF MFP_SIZE}

 {$DEFINE MFP_SIZEL}
// {$UNDEF MFP_SIZEL}

 {$DEFINE MFP_PALETTEENTRY}
// {$UNDEF MFP_PALETTEENTRY}

 {$DEFINE MFP_LOGPALETTE}
// {$UNDEF MFP_LOGPALETTE}

 {$DEFINE MFP_RECTL}
// {$UNDEF MFP_RECTL}

 {$DEFINE MFP_LPCRECT}
// {$UNDEF MFP_LPCRECT}

// {$DEFINE MFP_UCHAR}
// {$UNDEF MFP_UCHAR}

 {$DEFINE MFP_SHORT}
// {$UNDEF MFP_SHORT}

 {$DEFINE MFP_CSHORT}
// {$UNDEF MFP_CSHORT}

 {$DEFINE MFP_SMALL_RECT}
// {$UNDEF MFP_SMALL_RECT}

 {$DEFINE MFP_ROTFLAGS}
// {$UNDEF MFP_ROTFLAGS}

 {$DEFINE MFP_DVASPECT}
// {$UNDEF MFP_DVASPECT}

 {$DEFINE MFP_STGC}
// {$UNDEF MFP_STGC}

 {$DEFINE MFP_STATFLAG}
// {$UNDEF MFP_STATFLAG}

 {$DEFINE MFP_STGMOVE}
// {$UNDEF MFP_STGMOVE}

// {$DEFINE MFP_ROT_COMPARE_MAX}
// {$UNDEF MFP_ROT_COMPARE_MAX}

 {$DEFINE MFP_HCONTEXT}
// {$UNDEF MFP_HCONTEXT}

 {$DEFINE MFP_LCID}
// {$UNDEF MFP_LCID}

 {$DEFINE MFP_LANGID}
// {$UNDEF MFP_LANGID}

 {$DEFINE MFP_WDT}
// {$UNDEF MFP_WDT}

 {$DEFINE MFP_userCLIPFORMAT}
// {$UNDEF MFP_userCLIPFORMAT}

 {$DEFINE MFP_CLIPFORMAT}
// {$UNDEF MFP_CLIPFORMAT}

 {$DEFINE MFP_GDI_NONREMOTE}
// {$UNDEF MFP_GDI_NONREMOTE}

 {$DEFINE MFP_userHGLOBAL}
// {$UNDEF MFP_userHGLOBAL}

 {$DEFINE MFP_userHMETAFILE}
// {$UNDEF MFP_userHMETAFILE}

 {$DEFINE MFP_remoteMETAFILEPICT}
// {$UNDEF MFP_remoteMETAFILEPICT}

 {$DEFINE MFP_userHMETAFILEPICT}
// {$UNDEF MFP_userHMETAFILEPICT}

 {$DEFINE MFP_userHENHMETAFILE}
// {$UNDEF MFP_userHENHMETAFILE}

 {$DEFINE MFP_userBITMAP}
// {$UNDEF MFP_userBITMAP}

 {$DEFINE MFP_userHBITMAP}
// {$UNDEF MFP_userHBITMAP}

 {$DEFINE MFP_userHPALETTE}
// {$UNDEF MFP_userHPALETTE}

 {$DEFINE MFP_RemotableHandle}
// {$UNDEF MFP_RemotableHandle}

// {$DEFINE MFP_HWND}
// {$UNDEF MFP_HWND}

 {$DEFINE MFP_PHWND}
// {$UNDEF MFP_PHWND}

 {$DEFINE MFP_wireHBITMAP}
// {$UNDEF MFP_wireHBITMAP}

 {$DEFINE MFP_wireHPALETTE}
// {$UNDEF MFP_wireHPALETTE}

 {$DEFINE MFP_wireHENHMETAFILE}
// {$UNDEF MFP_wireHENHMETAFILE}

 {$DEFINE MFP_wireHMETAFILE}
// {$UNDEF MFP_wireHMETAFILE}

 {$DEFINE MFP_wireHMETAFILEPICT}
// {$UNDEF MFP_wireHMETAFILEPICT}

 {$DEFINE MFP_HMETAFILEPICT}
// {$UNDEF MFP_HMETAFILEPICT}

 {$DEFINE MFP_CY}
// {$UNDEF MFP_CY}

 {$DEFINE MFP_DECIMAL}
// {$UNDEF MFP_DECIMAL}

 {$DEFINE MFP_DECIMAL_NEG}
// {$UNDEF MFP_DECIMAL_NEG}

 {$DEFINE MFP_LPDECIMAL}
// {$UNDEF MFP_LPDECIMAL}

 {$DEFINE MFP_wireBSTR}
// {$UNDEF MFP_wireBSTR}

 {$DEFINE MFP_BSTR}
// {$UNDEF MFP_BSTR}

 {$DEFINE MFP_VARIANT_BOOL}
// {$UNDEF MFP_VARIANT_BOOL}

 {$DEFINE MFP_BSTRBLOB}
// {$UNDEF MFP_BSTRBLOB}

 {$DEFINE MFP_VARIANT_TRUE_FALSE}
// {$UNDEF MFP_VARIANT_TRUE_FALSE}

 {$DEFINE MFP_CLIPDATA}
// {$UNDEF MFP_CLIPDATA}

 {$DEFINE MFP_VARTYPE}
// {$UNDEF MFP_VARTYPE}

 {$DEFINE MFP_PROPID}
// {$UNDEF MFP_PROPID}

 {$DEFINE MFP_PROPERTYKEY}
// {$UNDEF MFP_PROPERTYKEY}

 {$DEFINE MFP_CSPLATFORM}
// {$UNDEF MFP_CSPLATFORM}

 {$DEFINE MFP_QUERYCONTEXT}
// {$UNDEF MFP_QUERYCONTEXT}

 {$DEFINE MFP_TYSPEC}
// {$UNDEF MFP_TYSPEC}

 {$DEFINE MFP_uCLSSPEC}
// {$UNDEF MFP_uCLSSPEC}

 {$DEFINE MFP_X_PTRS}
// {$UNDEF MFP_X_PTRS}

 {$DEFINE MFP_PtrInt}
// {$UNDEF MFP_PtrInt}

 {$DEFINE MFP_LPCOLESTR}
// {$UNDEF MFP_LPCOLESTR}

 {$DEFINE MFP_OLEDATE}
// {$UNDEF MFP_OLEDATE}

 {$DEFINE MFP_OLEBOOL}
// {$UNDEF MFP_OLEBOOL}

 {$DEFINE MFP_SIZE_T}
// {$UNDEF MFP_SIZE_T}


//MFPACK - shared
//---------------

 {$DEFINE MFP_UINT8}
// {$UNDEF MFP_UINT8}

 {$DEFINE MFP_UINT32}
// {$UNDEF MFP_UINT32}

 {$DEFINE MFP_INT32}
// {$UNDEF MFP_INT32}

 {$DEFINE MFP_ULONG64}
// {$UNDEF MFP_ULONG64}

 {$DEFINE MFP_FLOAT}
// {$UNDEF MFP_FLOAT}

 {$DEFINE MFP_FLOAT32}
// {$UNDEF MFP_FLOAT32}

 {$DEFINE MFP_FLOAT64}
// {$UNDEF MFP_FLOAT64}

 {$DEFINE MFP_LPRECT}
// {$UNDEF MFP_LPRECT}

 {$DEFINE MFP_PPByte}
// {$UNDEF MFP_PPByte}

 {$DEFINE MFP_SYSINT}
// {$UNDEF MFP_SYSINT}

 {$DEFINE MFP_TOPOID}
// {$UNDEF MFP_TOPOID}

 {$DEFINE MFP_TBool}
// {$UNDEF MFP_TBool}

 {$DEFINE MFP_bstrVal}
// {$UNDEF MFP_bstrVal}

 {$DEFINE MFP_DATE}
// {$UNDEF MFP_DATE}

 {$DEFINE MFP_MFTIME}
// {$UNDEF MFP_MFTIME}

 {$DEFINE MFP_HNSTIME}
// {$UNDEF MFP_HNSTIME}

 {$DEFINE MFP_SHCOLSTATEF}
// {$UNDEF MFP_SHCOLSTATEF}

 {$DEFINE MFP_POINTS}
// {$UNDEF MFP_POINTS}

// {$DEFINE MFP_GUID}
// {$UNDEF MFP_GUID}

 {$DEFINE MFP_PPTGUID}
// {$UNDEF MFP_PPTGUID}


// PROPERTYKEY related
//--------------------

 {$DEFINE MFP_PIDispatch}
// {$UNDEF MFP_PIDispatch}

 {$DEFINE MFP_MEDIATYPE_NULL}
// {$UNDEF MFP_MEDIATYPE_NULL}

 {$DEFINE MFP_MEDIASUBTYPE_NULL}
// {$UNDEF MFP_MEDIASUBTYPE_NULL}

 {$DEFINE MFP_REFVARIANT}
// {$UNDEF MFP_REFVARIANT}


  //=========== SPECIFIC PLATFORM TYPES ========================


  //Core Audio & DirectShow
  //-----------------------

 {$DEFINE MFP_REFERENCE_TIME}
// {$UNDEF MFP_REFERENCE_TIME}

 {$DEFINE MFP_REFTIME}
// {$UNDEF MFP_REFTIME}

 {$DEFINE MFP_HSEMAPHORE}
// {$UNDEF MFP_HSEMAPHORE}

 {$DEFINE MFP_HMONITOR}
// {$UNDEF MFP_HMONITOR}

 {$DEFINE MFP_HEVENT}
// {$UNDEF MFP_HEVENT}

 {$DEFINE MFP_ANYSIZE_ARRAY}
// {$UNDEF MFP_ANYSIZE_ARRAY}

 {$DEFINE MFP_MAX_INPUT_OUTPUT}
// {$UNDEF MFP_MAX_INPUT_OUTPUT}


  //Media Foundation
  //----------------



  //Special types
  //-------------


 {$DEFINE MFP_MemoryAllocator}
// {$UNDEF MFP_MemoryAllocator}



//Other files
//===========

// WinApi.MediaFoundationApi.MfApi
//================================

 {$DEFINE MFP_WORDARRAY}
// {$UNDEF MFP_WORDARRAY}



// WinApi.CoreAudioApi.DeviceTopology
//===================================
// {$DEFINE MFP_KSIDENTIFIER}
// {$UNDEF MFP_KSIDENTIFIER}



// WinApi.AudioAPOTypes
//=====================
// {$DEFINE MFP_AUDIO_CURVE_TYPE}
// {$UNDEF MFP_AUDIO_CURVE_TYPE}



//WinApi.MediaFoundationApi.OpmApi
//================================
// {$DEFINE DO_NOT_USE_DIRECTX_OR_DXVA2}
// {$UNDEF DO_NOT_USE_DIRECTX_OR_DXVA2}



// WinApi.MediaFoundationApi.WmCodecDsp
//=====================================
// {$DEFINE MEDIASUBTYPE_NV11}
// {$UNDEF MEDIASUBTYPE_NV11}

// {$DEFINE MFP_QWORD}
// {$UNDEF MFP_QWORD}

// {$DEFINE E_TOCPARSER_INVALIDASFFILE}
// {$UNDEF E_TOCPARSER_INVALIDASFFILE}

// {$DEFINE E_TOCPARSER_INVALIDRIFFFILE}
// {$UNDEF E_TOCPARSER_INVALIDRIFFFILE}

// {$DEFINE TOC_MAX_DESCRIPTION_SIZE}
// {$UNDEF TOC_MAX_DESCRIPTION_SIZE}

// {$DEFINE TOC_ENTRY_MAX_TITLE_SIZE}
// {$UNDEF TOC_ENTRY_MAX_TITLE_SIZE}



// WinApi.DevPropDef
//==================
// {$DEFINE MFP_DEVPROPTYPE}
// {$UNDEF MFP_DEVPROPTYPE}

// {$DEFINE MFP_DEVPROPKEY}
// {$UNDEF MFP_DEVPROPKEY}


// WinApi.DirectX.DXVA2Trace
//==========================
// {$DEFINE DXVA2Trace_PostProcessing}
// {$UNDEF DXVA2Trace_PostProcessing}



// MfPack.EvnTrace
//================
// {$DEFINE MFP_TRACEHANDLE}
// {$UNDEF MFP_TRACEHANDLE}


// WinApi.MediaFoundationApi.EVR9.pas
//===================================
// {$DEFINE MFP_RECT}
// {$UNDEF MFP_RECT}

// {$DEFINE MFP_FLOAT}  // Defined in MfpTypes.pas
// {$UNDEF MFP_FLOAT}

// {$DEFINE MFP_PPTGUID}
// {$UNDEF MFP_PPTGUID}

// {$DEFINE MFP_LPGUID}
// {$UNDEF MFP_LPGUID}


// WinApi.MediaFoundationApi.MfIdl.pas
//====================================
// {$DEFINE MFP_MFTIME} // Defined in MfpTypes.pas
// {$UNDEF MFP_MFTIME}

// {$DEFINE MFP_PIStream}
// {$UNDEF MFP_PIStream}



// WinApi.ActiveX.OaIdl
//=====================
// {$DEFINE MFP_CURRENCY}
// {$UNDEF MFP_CURRENCY}


// WinApi.ActiveX.PropKeyDef
//==========================
// {$DEFINE PID_FIRST_USABLE}
// {$UNDEF PID_FIRST_USABLE}


//WinApi.ActiveX.PropSys
//======================
// {$DEFINE MFP_PIStream}
// {$UNDEF MFP_PIStream}


// WinApi.ActiveX.PropIdl
//=======================
// {$DEFINE MFP_PROPSPEC}
// {$UNDEF MFP_PROPSPEC}


// WinApi.MmReg
//=============
// {$DEFINE MFP_MM_MIDI_MAPPER}
// {$UNDEF MFP_MM_MIDI_MAPPER}

// {$DEFINE MFP_WAVE_FORMAT_PCM}
// {$UNDEF MFP_WAVE_FORMAT_PCM}

// {$DEFINE WAVE_FORMAT_PCM = 1}
// {$UNDEF WAVE_FORMAT_PCM = 1}

// {$DEFINE MFP_WAVEFORMATEX}
// {$UNDEF MFP_WAVEFORMATEX}

// {$DEFINE MFP__WAVEFORMATEXTENSIBLE_}
// {$UNDEF MFP__WAVEFORMATEXTENSIBLE_}

// {$DEFINE MFP__SPEAKER_POSITIONS_}
// {$UNDEF MFP__SPEAKER_POSITIONS_}


// WinApi.Dbt
//===========
// {$DEFINE MFP_WCHAR_T}
// {$UNDEF MFP_WCHAR_T}

// {$DEFINE MFP_NATIVE_WCHAR_T}
// {$UNDEF MFP_NATIVE_WCHAR_T}



//WinApi.AudioAPOTypes
//====================
// {$DEFINE MFP_AUDIO_CURVE_TYPE}
// {$UNDEF MFP_AUDIO_CURVE_TYPE}



// SPECIFIC PLATFORM TYPES
//========================

//Core Audio & DirectShow
// {$DEFINE MFP_REFERENCE_TIME}
// {$UNDEF MFP_REFERENCE_TIME}

// {$DEFINE MFP_REFTIME}
// {$UNDEF MFP_REFTIME}

// {$DEFINE MFP_HSEMAPHORE}
// {$UNDEF MFP_HSEMAPHORE}

// {$DEFINE MFP_HMONITOR}
// {$UNDEF MFP_HMONITOR}


// DirectX

{$IFNDEF DIRECT3D_VERSION}
  {$DEFINE DIRECT3D_VERSION}
{$ENDIF

// {$UNDEF DIRECT3D_VERSION}


// Special types
//----------------

{$DEFINE UNICODE}
// {$UNDEF UNICODE}


// limits.h

{$DEFINE MFP_MAXIMUM_VOLUME_LABEL_LENGTH}
// {$UNDEF MFP_MAXIMUM_VOLUME_LABEL_LENGTH}

{$DEFINE MFP_ARG_MAX}
// {$UNDEF MFP_ARG_MAX}

{$DEFINE MFP_CHILD_MAX}
// {$UNDEF MFP_CHILD_MAX}

{$DEFINE MFP_LINK_MAX}
// {$UNDEF MFP_LINK_MAX}

{$DEFINE MFP_MAX_CANON}
// {$UNDEF MFP_MAX_CANON}

{$DEFINE MFP_MAX_INPUT}
// {$UNDEF MFP_MAX_INPUT}

{$DEFINE MFP_NAME_MAX}
// {$UNDEF MFP_NAME_MAX}

{$DEFINE MFP_NGROUPS_MAX}
// {$UNDEF MFP_NGROUPS_MAX}

{$DEFINE MFP_GID_MAX}
// {$UNDEF MFP_GID_MAX}

{$DEFINE MFP_UID_MAX}
// {$UNDEF MFP_UID_MAX}

{$DEFINE MFP_OPEN_MAX}
// {$UNDEF MFP_OPEN_MAX}

{$DEFINE MFP_PATH_MAX}
// {$UNDEF MFP_PATH_MAX}

{$DEFINE MFP_PIPE_BUF}
// {$UNDEF MFP_PIPE_BUF}

{$DEFINE MFP_BC_BASE_MAX}
// {$UNDEF MFP_BC_BASE_MAX}

{$DEFINE MFP_BC_DIM_MAX}
// {$UNDEF MFP_BC_DIM_MAX}

{$DEFINE MFP_BC_SCALE_MAX}
// {$UNDEF MFP_BC_SCALE_MAX}

{$DEFINE MFP_BC_STRING_MAX}
// {$UNDEF MFP_BC_STRING_MAX}

{$DEFINE MFP_CHARCLASS_NAME_MAX}
// {$UNDEF MFP_CHARCLASS_NAME_MAX}

{$DEFINE MFP_COLL_WEIGHTS_MAX}
// {$UNDEF MFP_COLL_WEIGHTS_MAX}

{$DEFINE MFP_EQUIV_CLASS_MAX}
// {$UNDEF MFP_EQUIV_CLASS_MAX}

{$DEFINE MFP_EXPR_NEST_MAX}
// {$UNDEF MFP_EXPR_NEST_MAX}

{$DEFINE MFP_LINE_MAX}
// {$UNDEF MFP_LINE_MAX}

{$DEFINE MFP_RE_DUP_MAX}
// {$UNDEF MFP_RE_DUP_MAX}

{$DEFINE MFP_NZERO}
// {$UNDEF MFP_NZERO}

{$DEFINE MFP_SCHAR_MAX}
// {$UNDEF MFP_SCHAR_MAX}

{$DEFINE MFP_SCHAR_MIN}
// {$UNDEF MFP_SCHAR_MIN}

{$DEFINE MFP_UCHAR_MAX}
// {$UNDEF MFP_UCHAR_MAX}

{$DEFINE MFP_CHAR_MAX}
// {$UNDEF MFP_CHAR_MAX}

{$DEFINE MFP_CHAR_MIN}
// {$UNDEF MFP_CHAR_MIN}

{$DEFINE MFP_USHRT_MAX}
// {$UNDEF MFP_USHRT_MAX}

{$DEFINE MFP_SHRT_MAX}
// {$UNDEF MFP_SHRT_MAX}

{$DEFINE MFP_SHRT_MIN}
// {$UNDEF MFP_SHRT_MIN}

{$DEFINE MFP_UINT_MAX}
// {$UNDEF MFP_UINT_MAX}

{$DEFINE MFP_FORCEDWORD}
// {$UNDEF MFP_FORCEDWORD}

{$DEFINE MFP_INT_MAX}
// {$UNDEF MFP_INT_MAX}

{$DEFINE MFP_INT_MIN}
// {$UNDEF MFP_INT_MIN}

{$DEFINE MFP_ULONG_MAX}
// {$UNDEF MFP_ULONG_MAX}

{$DEFINE MFP_LONG_MAX}
// {$UNDEF MFP_LONG_MAX}

{$DEFINE MFP_LONG_MIN}
// {$UNDEF MFP_LONG_MIN}

{$DEFINE MFP_ULLONG_MAX}
// {$UNDEF MFP_ULLONG_MAX}

{$DEFINE MFP_ULONGLONG_MAX}
// {$UNDEF MFP_ULONGLONG_MAX}

{$DEFINE MFP_LLONG_MAX}
// {$UNDEF MFP_LLONG_MAX}

{$DEFINE MFP_LLONG_MIN}
// {$UNDEF MFP_LLONG_MIN}

{$DEFINE MFP_LONG_BIT}
// {$UNDEF MFP_LONG_BIT}

{$DEFINE MFP_SSIZE_MAX}
// {$UNDEF MFP_SSIZE_MAX}

{$DEFINE MFP_WORD_BIT}
// {$UNDEF MFP_WORD_BIT}

{$DEFINE MFP_SIZE_T_MAX}
// {$UNDEF MFP_SIZE_T_MAX}

{$DEFINE MFP_UQUAD_MAX}
// {$UNDEF MFP_UQUAD_MAX}

{$DEFINE MFP_QUAD_MAX}
// {$UNDEF MFP_QUAD_MAX}

{$DEFINE MFP_QUAD_MIN}
// {$UNDEF MFP_QUAD_MIN}

// The RGBQuad structure is also defined in Winapi.Windows, no need to define.
//{$DEFINE MFP_RGBQUAD}
 {$UNDEF MFP_RGBQUAD}  // use the structure from WinApi.Windows

{$DEFINE MFP_RGB}
 //{$UNDEF MFP_RGB}

// When using unit MfpWinError
{$DEFINE MFP_WinError}
 //{$UNDEF MFP_WinError}


 // wingdi.h
{$DEFINE MFP_FONTSIGNATURE}
 //{$UNDEF MFP_FONTSIGNATURE}
 // wingdi.h
{$DEFINE MFP_LOGFONT}
 //{$UNDEF MFP_LOGFONT}

{$DEFINE MFP_DISPID}
// {$UNDEF MFP_DISPID}

//Templates
// {$DEFINE MFP_X}
// {$UNDEF MFP_X}

// <EOF>

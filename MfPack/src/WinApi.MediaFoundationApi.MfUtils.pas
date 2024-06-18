// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfUtils.pas
// Kind: Pascal / Delphi unit
// Release date: 29-07-2012
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Common methods used by Media Foundation,
//              Core Audio etc..
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: See comments.
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
unit WinApi.MediaFoundationApi.MfUtils;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX.PropIdl,
  {$ENDIF}
  {System}
  System.SysUtils,
  System.WideStrUtils,
  System.UITypes,
  System.Types,
  System.SyncObjs,
  {VCL}
  VCL.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.MfError;

  {$I 'WinApiTypes.inc'}
  {$DEFINE IGNORE_COMPILERVERSION}

  // WinApi.Windows
  // Used by function CreateEventEx dwFlags.
  {$IF COMPILERVERSION < 34.0}
  const
  CREATE_EVENT_MANUAL_RESET = DWord($0001);  // The initial state of the event object is signaled; otherwise, it is nonsignaled.

  CREATE_EVENT_INITIAL_SET  = DWord($0002);  // The event must be manually reset using the ResetEvent function.
                                             // Any number of waiting threads, or threads that subsequently begin
                                             // wait operations for the specified event object,
                                             // can be released while the object's state is signaled.
                                             // If this flag is not specified, the system automatically resets
                                             // the event after releasing a single waiting thread.
  {$ENDIF}



type

  TPComObj = array of Pointer;

  procedure SafeRelease(var IUnk);
  // Use for releasing objects.
  procedure SAFE_RELEASE(const [ref] IUnk);

  procedure Cpp_FreeAndNil(var Obj: TObject);

  procedure PropVariantClearSafe(var pv: PROPVARIANT); inline;

  // Compare GUIDS
  function InlineIsEqualGUID(rguid1: TGUID;
                             rguid2: TGUID): BOOL; inline;

  function IsEqualGUID(rguid1: TGUID;
                       rguid2: TGUID): BOOL; inline;

  // Same type, different names
  function IsEqualIID(riid1: TGUID;
                      riid2: TGUID): BOOL; inline;

  function IsEqualCLSID(rclsid1: TGUID;
                        rclsid2: TGUID): BOOL; inline;

  function IsEqualFMTID(rfmtid1: TGUID;
                        rfmtid2: TGUID): BOOL; inline;

  // Compare GUID pointers
  function IsEqualPGUID(rguid1: REFGUID;
                        rguid2: REFGUID): BOOL; stdcall;

  function IsEqualPIID(riid1: TGUID;
                       riid2: TGUID): BOOL; stdcall;

  function IsEqualPCLSID(rclsid1: TGUID;
                         rclsid2: TGUID): BOOL; stdcall;

  // Other guid methods
  function GuidFromString(Value: string): TGuid; inline;

  function IsEmptyGuid(pGuid: TGuid): BOOL; inline;


  // From wtypes.h
  procedure DECIMAL_SETZERO(var dec: DECIMAL); inline;

  // Macro translation to calculate the size of the pbClipData declared in tagCLIPDATA in WTypes.pas
  function CBPCLIPDATA(_clipdata: CLIPDATA): PByte; inline;

  // Functions to keep byte values in range of 0 - 255
  function ClipByteValue(byteval: UINT32): Byte; inline;
  function Clip(clr: Integer): Byte; inline;

  function OsIsWinXPOrHigher(): Boolean;
  function OsIsWinXPSP1OrHigher(): Boolean;
  function OsIsWinXPSP2OrHigher(): Boolean;
  function OsIsWinXPSP3OrHigher(): Boolean;
  function OsIsWinVistaOrHigher(): Boolean;
  function OsIsWinVistaSP1OrHigher(): Boolean;
  function OsIsWinVistaSP2OrHigher(): Boolean;
  function OsIsWin7OrHigher(): Boolean;
  function OsIsWin7SP1OrHigher(): Boolean;
  function OsIsWin8OrHigher(): Boolean;
  function OsIsWin81OrHigher(): Boolean;
  function OsIsWin10OrHigher(): Boolean;
  function OsIsWinServer(): Boolean;
  function IsWinVerOrHigher(wMajorVersion: Word;
                            wMinorVersion: Word;
                            wServicePackMajor: Word): Boolean;


  // C++ translations
  //=================
  function UIntAdd(var uAugend: UINT32;
                   const uAddend: UINT32;
                   out puResult: PUINT32): HRESULT; inline;

  function SizeTMult(const cbMultiplicand: SIZE_T;
                     const cbMultiplier: SIZE_T;
                     out pcbResult: PSIZE_T): HRESULT; inline;

  // Time formatting
  // Converts 100-Nano second units (Hns unit) to time string format.
  function HnsTimeToStr(hns: MFTIME;
                        DelimiterFormat: string;
                        ShowMilliSeconds: Boolean = True): string; overload; inline;

  function HnsTimeToStr(hns: MFTIME;
                        ShowMilliSeconds: Boolean = True): string; overload; inline;

  // Converts Milliseconds to a time string format
  function MSecToStr(wMsec: Int64;
                     ShowMilliSeconds: Boolean = True): string; inline;

  // Translate timeformat to milliseconds (default) or 100-Nano second units.
  function TimeToHnsTime(TimeStamp: string;
                         HnsTimeUnits: Boolean = True): MFTIME; inline;

  // Converts Seconds to time string format.
  // This funtion is primarily used by the MediaEngine interface
  function MfSecToStr(dSec: Double;
                      ShowMilliSec: Boolean): string; inline;

  // Convert Milliseconds to MFTIME (100-nano second units)
  function MSecToHnsTime(Millisec: ULONGLONG): MFTIME; inline;

  // Convert 100-nanosecond units to milliseconds.
  function HnsTimeToMsec(hnsTime: MFTIME): MFTIME; inline;

  // Convert frames to milliseconds
  function FramesToMsec(frames: Int64;
                        fps: Double): MFTIME; inline;

  // Convert frames to a timestring
  function FramesToTimeStr(frames: Int64;
                           fps: Double;
                           ShowMilliSeconds: boolean = True ): string; inline;

  // Converts the number of bytes, samplerate, channels and bits per second to hnstime and returns the timeformat.
  function BytesToTimeStr(const pBytesWritten: Int64;
                          const pSampleRate: Integer;
                          const pChannels: Integer;
                          const pBitsPerSample: Integer;
                          pShowMilliSeconds: Boolean = True): string; inline;

  // Operating system
  function GetOSArchitecture(): string;


  // Colors and pixelformats
  //========================

  // Translates a HTML color name name to TColor
  // Example: 'Red' >> TColor($0000FF) = TColors.Red = clRed;
  function GetTColorFromHtmlColorName(HtmlClrName: string): TColor;

  // HTMLcolor to TColor
  function HtmlColorToTColor(aValue: string;
                             aDefault: Tcolor = clNone): TColor; inline;

  // TColor to HTMLcolor
  function TColorToHtmlColor(aValue: TColor): string; inline;

  // TColor to MFARGB
  procedure CopyTColorToMFARGB(const cColor: TColor;
                               out argb: MFARGB); inline;
  // MFARGB to TColor
  procedure CopyMFARGBToTColor(const argb: MFARGB;
                               out cColor: TColor); inline;

  // TColor to COLORREF
  function ColorToColorRef(cColor: TColor): COLORREF; inline;

  // COLORREF to TColor
  function ColorRefToTColor(cColorRef: COLORREF): TColor; inline;

  // Copies RGBTRIPLE src to RGBTRIPLE srd
  procedure CopyRgbTriple(src: RGBTRIPLE;
                          out srd: RGBTRIPLE); inline;

  // Copies RGBTRIPLE src to RGBQUAD srd
  procedure CopyRgbTripleToRgbQuad(src: RGBTRIPLE;
                                   out srd: RGBQUAD); inline;

  // Copies RGBQUAD src to RGBTRIPLE srd
  procedure CopyRgbQuadToRgbTriple(src: RGBQUAD;
                                   out srd: RGBTRIPLE); inline;


  // copies a DWord (COLORREF) to RGBTRIPLE
  procedure CopyClrRefToRgbTriple(src: COLORREF;
                                  out srd: RGBTRIPLE); inline;

  // copies a RGBTRIPLE to DWord (COLORREF)
  procedure CopyRgbTripleToClrRef(src: RGBTRIPLE;
                                  out srd: COLORREF); inline;

  // copies a RGBQUAD to DWord (COLORREF)
  procedure CopyRGBQuadToClrRef(src: RGBQUAD;
                                out srd: COLORREF); inline;

  // copies a DWord (COLORREF) to RGBQUAD
  procedure CopyClrRefToRGBQuad(src: COLORREF;
                                out srd: RGBQUAD); inline;


  function ConvertYCrCbToRGB(aY: Integer;
                             aCr: Integer;
                             aCb: Integer): RGBQUAD; inline;

  function PixelFormatToStr(PixelFmt: TPixelFormat): string; inline;


  // External
  function StringFromCLSID(const rclsid: REFCLSID;
                           var lplpsz: POleStr): HResult; stdcall;

  // Simplified helper of StringToWideChar function
  function StrToPWideChar(source: string): PWideChar; //inline;


  // MFVideoNormalizedRect methods
  ////////////////////////////////
  // Support for older Delphi versions (<= XE2)

  // Set MFVideoNormalizedRect values
  //  example rcSrc:= SetTRect(0, 0, 10, 20);
  function SetVNRect(const nrLeft: FLOAT = 0.0;
                     const nrTop: FLOAT = 0.0;
                     const nrRight: FLOAT = 0.0;
                     const nrBottom: FLOAT = 0.0): MFVideoNormalizedRect; inline;

  // Copy MFVideoNormalizedRect values to another MFVideoNormalizedRect
  procedure CopyVNRectToVNRect(const rs: MFVideoNormalizedRect;
                               out rd: MFVideoNormalizedRect); inline;

  // Copy a MFVideoNormalizedRect to a PMFVideoNormalizedRect
  procedure CopyVNRectToPVNRect(rs: MFVideoNormalizedRect;
                                out rd: PMFVideoNormalizedRect); inline;

  // Copy a PMFVideoNormalizedRect to a MFVideoNormalizedRect
  procedure CopyPVNRectToVNRect(rs: PMFVideoNormalizedRect;
                                out rd: MFVideoNormalizedRect); inline;

  // Add a MFVideoNormalizedRect to another
  function AddVNRect(const r1: MFVideoNormalizedRect;
                     const r2: MFVideoNormalizedRect): MFVideoNormalizedRect; inline;

  // Substract a MFVideoNormalizedRect from another
  function SubstractVNRect(const r1: MFVideoNormalizedRect;
                           const r2: MFVideoNormalizedRect): MFVideoNormalizedRect; inline;

  // multiply a MFVideoNormalizedRect with another
  function multiplyVNRect(const r1: MFVideoNormalizedRect;
                          const scale: LONG): MFVideoNormalizedRect; inline;

  // Get the width of a MFVideoNormalizedRect
  function GetNrRectWidth(const rc: MFVideoNormalizedRect): LONG; inline;

  // Get the height of a MFVideoNormalizedRect
  function GetNrRectHeight(const rc: MFVideoNormalizedRect): LONG; inline;

  // Checks whether a MFVideoNormalizedRect is empty
  function IsEmptyNrRect(const nnRect: MFVideoNormalizedRect): Boolean; inline;

  //Clear PMFVideoNormalizedRect
  procedure ClearVNRect(var ppRect: PMFVideoNormalizedRect); inline;

  // Copy a TRect to a MFVideoNormalizedRect
  procedure CopyTRectToVNRect(const rs: TRect;
                              out rd: MFVideoNormalizedRect); inline;

  // Copy a MFVideoNormalizedRect to a TRect
  procedure CopyVNRectToTRect(const rs: MFVideoNormalizedRect;
                              out rd: TRect); inline;

  // Copy a PMFVideoNormalizedRect to a TRect
  procedure CopyPVNRectToTRect(rs: PMFVideoNormalizedRect;
                               out rd: TRect); inline;

  // Copy a PMFVideoNormalizedRect to a PRect
  procedure CopyPVNRectToPRect(rs: PMFVideoNormalizedRect;
                               out rd: PRect); inline;
  // TRect methods //

  // Copy TRect values to another TRect
  procedure CopyTRectToTRect(const rs: TRect;
                             out rd: TRect); inline;

  // Get the width of a TRect
  function GetTRectWidth(const rc: TRect): LONG; inline;

  // Get the height of a TRect
  function GetTRectHeight(const rc: TRect): LONG; inline;

  // Copy a TRect to a LPRECT
  procedure CopyTRectToLPRect(rs: TRect;
                              var rd: LPRECT); inline;

  // Copy a LPRECT to a TRect
  procedure CopyLPRectToTRect(rs: LPRECT;
                              var rd: TRect); inline;

  // Copy a TRect to a PRect
  procedure CopyTRectToPRect(rs: TRect;
                             var rd: PRect ); inline;

  // Copy a PRect to a TRect
  procedure CopyPRectToTRect(rs: PRect;
                             var rd: TRect); inline;


  // MFAREA & MFOFFSET
  //==================

  // Get Float (Single) value from MFOffSet.
  function MFOffsetToFloat(const offset: MFOffset): Float; inline;

  // MakeOffset
  function MakeOffset(v: Single): MFOffset; inline;

  // Get the display area from a (video) media type.
  function GetVideoDisplayArea(pType: IMFMediaType;
                               pArea: MFVideoArea): HRESULT;

  // initializes a MFVideoArea structure.
  function MakeArea(X: FLOAT;
                    Y: FLOAT;
                    dwWidth: DWORD;
                    dwHeight: DWORD): MFVideoArea; inline;


  // FOURCC functions
  //=================
  function MAKEFOURCC(const ch0: AnsiChar;
                      const ch1: AnsiChar;
                      const ch2: AnsiChar;
                      const ch3: AnsiChar): FOURCC; inline;

  // The reverse of MAKEFOURCC
  function GETFOURCC(frcc: FOURCC): WideString; inline;


  // Bitfield manipulation functions
  // Source: Embarcadero
  //================================

  // UINT64
  // Getter
  function Get64bitBits(const data: UINT64;
                        const index: Integer): Integer; inline;

  // Setter
  procedure Set64bitBits(var data: UINT64;
                         const index: Integer;
                         const value: Integer); inline;


  // DWORD
  // Getter
  function GetDWordBits(const data: DWORD;
                        const index: Integer): Integer; inline;

  // Setter
  procedure SetDWordBits(var data: DWORD;
                         const index: Integer;
                         const value: Integer); inline;


  // WORD
  // Getter
  function GetWordBits(const data: WORD;
                       const index: Integer): WORD; inline;


  // Setter
  procedure SetWordBits(var data: WORD;
                        const index: Integer;
                        const value: Integer); inline;



  // BYTE
  // Getter
  function GetByteBits(const data: Byte;
                       const index: Integer): Byte; inline;


  // Setter
  procedure SetByteBits(var data: Byte;
                        const index: Integer;
                        const value: Integer); inline;


  // System
  //=======
  function BoolToStrYesNo(const aBoolean: Boolean;
                          sYes: string = 'Yes';
                          sNo: string = 'No'): string; inline;
  // = BoolToStr, for backward compatibility.
  function MfpBoolToStr(const aBoolean: Boolean): string; inline;
  function BoolToStr(const aBoolean: Boolean): string; inline;

  // Constrain function, checks the high- and lowband limits of a value.
  function ConstrainValue(const Value: Integer;
                          Min: Integer;
                          Max: Integer): Integer; inline; overload;

  function ConstrainValue(const Value: Int64;
                          Min: Int64;
                          Max: Int64): Int64; inline; overload;

  function ConstrainValue(const Value: Float;
                          Min: Float;
                          Max: Float): Float; inline; overload;

  function ConstrainValue(const Value: byte;
                          Min: byte;
                          Max: byte): byte; inline; overload;


  // Since Win 10, use this function to create a file.
  function CreateFile2(lpFileName: PWideChar;
      {[in]}           dwDesiredAccess: DWORD;
      {[in]}           dwShareMode: DWORD;
      {[in]}           dwCreationDisposition: DWORD;
      {[in, optional]} pCreateExParams: LPCREATEFILE2_EXTENDED_PARAMETERS): THandle; stdcall;


  // Strings
  function StringCbCat(pszDest: PChar;
                       cbDest: SIZE_T;
                       const pszSrc: string): HRESULT;

  function StringCbCatA(pszDest: PAnsiChar;
                        cbDest: SIZE_T;
                        pszSrc: PAnsiChar): HRESULT; stdcall;

  function StringCbCatW(pszDest: PWideChar;
                        cbDest: SIZE_T;
                        pszSrc: PWideChar): HRESULT; stdcall;

  {$IF COMPILERVERSION < 29.0}
  // Introduced: Delphi XE8
  // Compares two Unicode strings.
  // Digits in the strings are considered as numerical content rather than text.
  // This test is not case-sensitive.
  function StrCmpLogicalW(psz1: LPCWSTR;
                          psz2: LPCWSTR): Integer; stdcall;
  {$ENDIF}

  // WinApi.Windows
  {$IF COMPILERVERSION < 34.0}
  function CreateEventEx(lpEventAttributes: PSecurityAttributes;
                         lpName: LPCWSTR;
                         dwFlags: DWord;
                         dwDesiredAccess: DWord): THandle; stdcall;

  function CreateEventExA(lpEventAttributes: PSecurityAttributes;
                          lpName: LPCSTR;
                          dwFlags: DWord;
                          dwDesiredAccess: DWord): THandle; stdcall;

  function CreateEventExW(lpEventAttributes: PSecurityAttributes;
                          lpName: LPCWSTR;
                          dwFlags: DWord;
                          dwDesiredAccess: DWord): THandle; stdcall;
  {$ENDIF}


implementation

uses
  {System}
  System.TypInfo;

const
  Kernel32Lib = 'kernel32.dll';
  Ole32Lib = 'Ole32.dll';
  Shlwapi32Lib = 'Shlwapi.dll';


  VER_EQUAL             = 1;
  VER_GREATER           = 2;
  VER_GREATER_EQUAL     = 3;
  VER_LESS              = 4;
  VER_LESS_EQUAL        = 5;
  VER_AND               = 6;
  VER_OR                = 7;

  //Defined in MfPack.inc
  //_WIN32_WINNT_WINXP    = $0501;
  //_WIN32_WINNT_VISTA    = $0600;
  //_WIN32_WINNT_WIN7     = $0601;
  //_WIN32_WINNT_WIN8     = $0602;
  //_WIN32_WINNT_WINBLUE  = $0603;  // Windows 8.1 (codename Windows Blue)
  //_WIN32_WINNT_WIN10    = $0A00;

  VER_SERVICEPACKMAJOR  = $0000010;
  VER_MAJORVERSION      = $0000002;
  VER_MINORVERSION      = $0000001;
  VER_NT_WORKSTATION    = 1;
  VER_PRODUCT_TYPE      = $80;


// SafeRelease
// Many of the code (examples) in the documentation use the SafeRelease method to
// release COM interfaced objects.
// Note: The object does initiate a reference call.
procedure SafeRelease(var IUnk);
begin
  if (IUnknown(IUnk) <> nil) then
    IUnknown(IUnk) := nil;
end;


// Note: Here the object does NOT initiate a reference call.
procedure Safe_Release(const [ref] IUnk);
begin
  if Assigned(IUnknown(IUnk)) then
    IUnknown(Pointer(@IUnk)^):= nil;
end;


// Equivalent method of CPPFreeAndNil
procedure Cpp_FreeAndNil(var Obj: TObject);
begin
  Obj.Free();
  Obj := nil;
end;


procedure PropVariantClearSafe(var pv: PROPVARIANT);
begin
  ZeroMemory(@pv,
             SizeOf(pv));
end;


////////////////////////////////////////////////////////////////////////////////


{$WARN SYMBOL_PLATFORM OFF}
// ole2: compare GUID type pointers
function IsEqualPGUID;   external Ole32Lib name 'IsEqualGUID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IsEqualPIID;    external Ole32Lib name 'IsEqualIID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function IsEqualPCLSID;  external Ole32Lib name 'IsEqualCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

function InlineIsEqualGUID(rguid1: TGUID;
                           rguid2: TGUID): BOOL; inline;
var
  rg1,
  rg2: PIntegerArray;

begin
  rg1 := PIntegerArray(@rguid1);
  rg2 := PIntegerArray(@rguid2);
  Result :=  (rg1^[0] = rg2^[0]) and
             (rg1^[1] = rg2^[1]) and
             (rg1^[2] = rg2^[2]) and
             (rg1^[3] = rg2^[3]);
end;


function IsEqualGUID(rguid1: TGUID;
                     rguid2: TGUID): BOOL; inline;
begin
  // Actually the same as InlineIsEqualGUID
  Result := InlineIsEqualGUID(rguid1,
                              rguid2);

  // MS solution: Has the same result, except this only works with pointers
  // So, to compare those pointer types of GUID, IID or CLSID, use:
  //  PIsEqualGUID, PIsEqualIID, PIsEqualCLSID
  // for your convenience here the Delphi equivalent to compare pointers:
  //  Result (Boolean) := CompareMem(refg1, refg2, SizeOf(TGUID));
end;


// Determines whether two interface identifiers are equal.
function IsEqualIID(riid1: TGUID;
                    riid2: TGUID): BOOL; inline;
begin
  Result := InLineIsEqualGUID(riid1,
                              riid2);
end;


function IsEqualCLSID(rclsid1: TGUID;
                      rclsid2: TGUID): BOOL; inline;
begin
  Result := InLineIsEqualGUID(rclsid1,
                              rclsid2);
end;


function IsEqualFMTID(rfmtid1: TGUID;
                      rfmtid2: TGUID): BOOL; inline;
begin
  Result := InLineIsEqualGUID(rfmtid1,
                              rfmtid2);
end;

// Other guid tools

function GuidFromString(Value: string): TGuid; inline;
begin
  result := StringToGuid(Value);
end;


function IsEmptyGuid(pGuid: TGUID): BOOL; inline;
begin
   result := InLineIsEqualGuid(pGuid,
                               GUID_NULL);
end;



// WTypes.h

// MACRO translations
//==================
procedure DECIMAL_SETZERO(var dec: DECIMAL); inline;
begin
  dec.Lo32 := 0;
  dec.Hi32 := 0;
  dec.signscale := 0;
end;


function CBPCLIPDATA(_clipdata: CLIPDATA): PByte; inline;
begin
  Result:= Pbyte(_clipdata.cbSize - sizeof(_clipdata.ulClipFmt));
end;


// clipping functions ////////////////////////////////////////////////////////////

function ClipByteValue(byteval: UINT32): Byte; inline;
begin
  if (byteval > 255) then
    Result := 255
  else
    Result := Byte(byteval);
end;

function Clip(clr: Integer): Byte; inline;
begin
  if (clr > 255) then
    Result := Byte(255)
  else if (clr < 0) then
    Result := Byte(0)
  else
    Result := Byte(clr);
end;



////////////// WINVERSIONS /////////////////////////////////////////////////////
// thanks to Glenn9999 on tek-tips.com on 06/06/2015 and http://thebesthacker.com

function VerifyVersionInfo(var LPOSVERSIONINFOEX : OSVERSIONINFOEX;
                           dwTypeMask: DWORD;
                           dwlConditionMask: Int64): BOOL; stdcall; external Kernel32Lib name 'VerifyVersionInfoA';


function VerSetConditionMask(dwlConditionMask: Int64;
                             dwTypeBitMask: DWORD;
                             dwConditionMask: Byte): Int64; stdcall; external Kernel32Lib;


function IsWinVerOrHigher(wMajorVersion, wMinorVersion, wServicePackMajor: Word): Boolean;
var
  osvi: OSVersionInfoEX;
  condmask: Int64;

begin
  FillChar(osvi,
           SizeOf(osvi),
           0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  FillChar(condmask,
           8,
           0);
  condmask := VerSetConditionMask(condmask,
                                  VER_MAJORVERSION,
                                  VER_GREATER_EQUAL);
  condmask := VerSetConditionMask(condmask,
                                  VER_MINORVERSION,
                                  VER_GREATER_EQUAL);
  condmask := VerSetConditionMask(condmask,
                                  VER_SERVICEPACKMAJOR,
                                  VER_GREATER_EQUAL);
  osvi.dwMajorVersion := wMajorVersion;
  osvi.dwMinorVersion := wMinorVersion;
  osvi.wServicePackMajor := wServicePackMajor;
  Result := VerifyVersionInfo(osvi,
                              VER_MAJORVERSION or
                              VER_MINORVERSION or
                              VER_SERVICEPACKMAJOR,
                              condmask);
end;

//
function OsIsWinXPOrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WINXP),
                             LoByte(_WIN32_WINNT_WINXP),
                             0);
end;

//
function OsIsWinXPSP1OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WINXP),
                             LoByte(_WIN32_WINNT_WINXP),
                             1);
end;

//
function OsIsWinXPSP2OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WINXP),
                             LoByte(_WIN32_WINNT_WINXP),
                             2);
end;

//
function OsIsWinXPSP3OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WINXP),
                             LoByte(_WIN32_WINNT_WINXP),
                             3);
end;

//
function OsIsWinVistaOrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_VISTA),
                             LoByte(_WIN32_WINNT_VISTA),
                             0);
end;

//
function OsIsWinVistaSP1OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_VISTA),
                             LoByte(_WIN32_WINNT_VISTA),
                             1);
end;

//
function OsIsWinVistaSP2OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_VISTA),
                             LoByte(_WIN32_WINNT_VISTA),
                             2);
end;

//
function OsIsWin7OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WIN7),
                             LoByte(_WIN32_WINNT_WIN7),
                             0);
end;

//
function OsIsWin7SP1OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WIN7),
                             LoByte(_WIN32_WINNT_WIN7),
                             0);
end;

//
function OsIsWin8OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WIN8),
                             LoByte(_WIN32_WINNT_WIN8),
                             0);
end;

//
function OsIsWin81OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WINBLUE),
                             LoByte(_WIN32_WINNT_WINBLUE),
                             0);
end;

//
function OsIsWin10OrHigher(): Boolean;
begin
  Result := IsWinVerOrHigher(HiByte(_WIN32_WINNT_WIN10),
                             LoByte(_WIN32_WINNT_WIN10),
                             0);
end;

//
function OsIsWinServer(): Boolean;
var
  osvi: OSVersionInfoEX;
  condmask: Int64;

begin
  FillChar(osvi,
           sizeof(osvi),
           0);
  osvi.dwOSVersionInfoSize:= SizeOf(osvi);
  FillChar(condmask,
           8,
           0);
  condmask:= VerSetConditionMask(condmask,
                                 VER_PRODUCT_TYPE,
                                 VER_EQUAL );
  osvi.wProductType := VER_NT_WORKSTATION;
  Result := not VerifyVersionInfo(osvi,
                                  VER_PRODUCT_TYPE,
                                  condmask);
end;

  // C++ function equivalents
  //=========================

  // Multiplies one value of type SIZE_T by another.
  // Example: hhresult = SizeTMult(length, sizeof(WCHAR), cb);
  // Parameters
  // cbMultiplicand [in]
  //    Type: SIZE_T
  //    The value to be multiplied by cbMultiplier.
  // cbMultiplier [in]
  //    Type: SIZE_T
  //    The value by which to multiply cbMultiplicand.
  // pcbResult [out]
  //    Type: SIZE_T*
  //    A pointer to the result.
  //    If the operation results in a value that overflows or underflows the capacity of the type,
  //    the function returns INTSAFE_E_ARITHMETIC_OVERFLOW and this parameter is not valid.
function SizeTMult(const cbMultiplicand: SIZE_T;
                   const cbMultiplier: SIZE_T;
                   out pcbResult: PSIZE_T): HRESULT;
begin
  try
    pcbResult := pointer(cbMultiplier * cbMultiplicand);
    Result := S_OK;
  except //Silent exception
    Result := INTSAFE_E_ARITHMETIC_OVERFLOW;
  end;
end; // SizeTMult


  // Adds two values of type UINT.
  // Parameters
  // uAugend [in]
  //    Type: UINT
  //    The first value in the equation.
  // uAddend [in]
  //    Type: UINT
  //    The value to add to uAugend.
  // puResult [out]
  //    Type: PUINT
  //    A pointer to the sum.
  //    If the operation results in a value that overflows or underflows the capacity of the type,
  //    the function returns INTSAFE_E_ARITHMETIC_OVERFLOW and this parameter is not valid.
  // Return value
  // Type: HRESULT
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  // Remarks
  // This is one of a set of inline functions designed to provide arithmetic operations and
  // perform validity checks with minimal impact on performance.
function UIntAdd(var uAugend: UINT32;
                 const uAddend: UINT32;
                 out puResult: PUINT32): HRESULT;
begin
  try
    uAugend := uAugend + uAddend;
    puResult := Pointer(uAugend);

    Result := S_OK;
  except //Silent exception
    Result := INTSAFE_E_ARITHMETIC_OVERFLOW;
  end;
end;


// Time formatting & conversion functions
//=======================================

function NanoSecondsToMilliSeconds(aNanoSec: Int64): Int64; inline;
begin
  Result :=  aNanoSec div 1000;
end;


function MilliSecondsToSeconds(aSec: Int64): Single; inline;
begin
  Result :=  aSec / 1000;
end;


// Converts Hns to a time string format
function HnsTimeToStr(hns: MFTIME;
                      DelimiterFormat: string;
                      ShowMilliSeconds: Boolean = True): string; inline;
var
  hours,
  mins,
  secs,
  millisec: Word;

begin

try
  hours := hns div MFTIME(36000000000);
  hns := hns mod MFTIME(36000000000);

  mins := hns div 600000000;
  hns := hns mod 600000000;

  secs := hns div 10000000;
  hns := hns mod 10000000;

  millisec := hns div 10000;

  if DelimiterFormat = '' then
    DelimiterFormat := ':';

  if ShowMilliSeconds then
    Result := Format('%2.2d%s%2.2d%s%2.2d,%3.3d',
                     [hours,
                      DelimiterFormat,
                      mins,
                      DelimiterFormat,
                      secs,
                      DelimiterFormat,
                      millisec])
  else
    Result := Format('%2.2d%s%2.2d%s%2.2d',
                     [hours,
                      DelimiterFormat,
                      mins,
                      DelimiterFormat,
                      secs]);

except
  on exception do Result:= '00:00:00,000';
end;
end;



function HnsTimeToStr(hns: MFTIME;
                      ShowMilliSeconds: Boolean = True): string; inline;
var
  hours,
  mins,
  secs,
  millisec: Word;

begin


try
  hours := hns div MFTIME(36000000000);
  hns := hns mod MFTIME(36000000000);

  mins := hns div 600000000;
  hns := hns mod 600000000;

  secs := hns div 10000000;
  hns := hns mod 10000000;

  millisec := hns div 10000;


  if ShowMilliSeconds then
    Result := Format('%2.2d:%2.2d:%2.2d,%3.3d',
                     [hours,
                      mins,
                      secs,
                      millisec])
  else
    Result := Format('%2.2d:%2.2d:%2.2d',
                     [hours,
                      mins,
                      secs]);

except
  on exception do Result:= '00:00:00,000';
end;
end;


// Converts milliseconds to a time string format
function MSecToStr(wMsec: Int64;
                   ShowMilliSeconds: boolean = True): string; inline;
var
  hours,
  mins,
  secs,
  millisec: Word;

begin
try
  hours := wMsec div 3600000;
  wMsec := wMsec mod 3600000;

  mins := wMsec div 60000;
  wMsec := wMsec mod 60000;

  secs := wMsec div 1000;
  wMsec := wMsec mod 1000;

  millisec := wMsec;

  if ShowMilliSeconds then
    Result := Format('%2.2d:%2.2d:%2.2d,%3.3d',
                     [hours,
                      mins,
                      secs,
                      millisec])
  else
    Result := Format('%2.2d:%2.2d:%2.2d', [hours,
                                           mins,
                                           secs]);

except
  on exception do Result:= '00:00:00,000';
end;
end;


// TimeStamp formats = hh:mm:ss,mmm or hh:mm:ss.mmm or hh:mm:ss:mmm etc.
// HnsTimeUnits: True = 100-Nanosecond units, False = Milliseconds
function TimeToHnsTime(TimeStamp: string;
                       HnsTimeUnits: Boolean = True): MFTIME; inline;
var
  hour,
  min,
  sec,
  ms: LONG;
  mft: MFTIME;

begin
  TryStrToInt(Copy(TimeStamp,
                   1,
                   2),
              hour);

  TryStrToInt(Copy(TimeStamp,
                   4,
                   2),
              min);

  TryStrToInt(Copy(TimeStamp,
                   7,
                   2),
              sec);

  TryStrToInt(Copy(TimeStamp,
                   10,
                   3),
              ms);

  mft := (hour * 3600000) +
         (min * 60000) +
         (sec * 1000) + ms;

  if HnsTimeUnits then
    Result := MSecToHnsTime(mft)
  else
    Result := mft;
end;

//
function MfSecToStr(dSec: Double;
                    ShowMilliSec: Boolean): string; inline;
var
  hours, mins, secs: Word;
  splitsec: Double;

begin
  try
    splitsec := Frac(dSec);

    hours := Trunc(dSec) div 3600;
    dSec := Trunc(dSec) mod 3600;

    mins := Trunc(dSec) div 60;
    dSec := Trunc(dSec) mod 60;

    secs := Trunc(dSec);

    if ShowMilliSec then
      begin
        splitsec := splitsec + secs;
        result := Format('%2.2d:%2.2d:%2.2f',
                         [hours,
                          mins,
                          splitsec]);
      end
    else
      begin
        secs := secs + Round(splitsec);
        result := Format('%2.2d:%2.2d:%2.2d',
                         [hours,
                          mins,
                          secs]);
      end;

  except
    on exception do Result:= '00:00:00';
  end;
end;

//
function MSecToHnsTime(Millisec: ULONGLONG): MFTIME; inline;
begin
  result := Millisec * ONE_HNS_MSEC;
end;

//
function HnsTimeToMsec(hnsTime: MFTIME): MFTIME; inline;
begin
  Result := (hnsTime div ONE_HNS_MSEC);
end;

//
function FramesToMsec(frames: Int64;
                      fps: Double): MFTIME; inline;
begin
  Result := Round((frames / fps) * 1000);
end;

//
function FramesToTimeStr(frames: Int64;
                         fps: Double;
                         ShowMilliSeconds: boolean = True ): string; inline;
var
  wMsec: UInt64;

begin
  wMsec := FramesToMsec(frames,
                        fps);
  Result := MSecToStr(wMsec,
                      ShowMilliSeconds);
end;

// Converts the number of bytes samplerate, channels and bitspersecond to hnstime and retunrs the timeformat.
function BytesToTimeStr(const pBytesWritten: Int64;
                        const pSampleRate: Integer;
                        const pChannels: Integer;
                        const pBitsPerSample: Integer;
                        pShowMilliSeconds: Boolean = True): string; inline;
const
  Bit32 = 4;
  Bit24 = 3;
  Bit16 = 2;

var
  hns: Int64;

begin

  // Calculate time in 100-nanosecond units.
  if (pBitsPerSample = 16) then
    hns := Trunc((pBytesWritten / (pSampleRate * Bit16 * pChannels)) * 10000000) // 16-bit audio.
  else if (pBitsPerSample = 24) then
    hns := Trunc((pBytesWritten / (pSampleRate * Bit24 * pChannels)) * 10000000) // 24-bit audio.
  else if (pBitsPerSample = 32) then
    hns := Trunc((pBytesWritten / (pSampleRate * Bit32 * pChannels)) * 10000000) // 32-bit audio.
  else // Exotic
    Exit;
  // Call HnsTimeToStr function to format the time string
  Result := HnsTimeToStr(hns,
                         pShowMilliSeconds);
end;

// OS

// Get the OS architecture (32 or 64 bit)
function GetOSArchitecture(): string;
var
  // 32 or 64 bit OS?
  OSArchitecture: TOSVersion.TArchitecture;

begin
  OSArchitecture:= TOSVersion.Architecture;
  case OSArchitecture of
    arIntelX86: Result := 'Intel 32Bit';
    arIntelX64: Result := 'Intel 64Bit';
{$IF CompilerVersion >= 27.0}
    arARM32:    Result := 'ARM 32Bit';
{$ENDIF}

    else
      Result := 'Unknown OS architecture';
  end;
end;


// Colors

function GetTColorFromHtmlColorName(HtmlClrName: string): TColor;
var
  I: Integer;

begin
  Result := clNone;
  for I := 0 to High(HTMLColorNames) do
    if HTMLColorNames[I].ClrName = UpperCase(HtmlClrName) then
      begin
        Result := HTMLColorNames[I].DelphiClr;
        Break;
      end;
end;


function HtmlColorToTColor(aValue: string;
                           aDefault: Tcolor = clNone): TColor; inline;
begin
  if (Length(aValue) <> 7) or (aValue[1] <> '#') then
    Result := aDefault
  else
    Result := RGB(StrToInt('$' + Copy(aValue,
                                      2,
                                      2)),
                  StrToInt('$' + Copy(aValue,
                                      4,
                                      2)),
                  StrToInt('$' + Copy(aValue,
                                      6,
                                      2)));
end;


function TColorToHtmlColor(aValue: TColor): string; inline;
var
  crRGB: TColorRef;
begin
  crRGB := ColorToRGB(aValue) ;
  Result := Format('#%.2x%.2x%.2x',
                   [GetRValue(crRGB),
                    GetGValue(crRGB),
                    GetBValue(crRGB)]);
end;


procedure CopyTColorToMFARGB(const cColor: TColor;
                             out argb: MFARGB); inline;
begin
  argb.rgbBlue  := (cColor shr 16) and $FF;
  argb.rgbGreen := (cColor shr 8) and $FF;
  argb.rgbRed   := (cColor and $FF);
  argb.rgbAlpha := (cColor shr 24) and $FF;
end;


procedure CopyMFARGBToTColor(const argb: MFARGB;
                             out cColor: TColor); inline;
begin
  cColor := ((argb.rgbRed shl 0) or
             (argb.rgbGreen shl 8) or
             (argb.rgbBlue shl 16) or
             (argb.rgbAlpha shl 24));
end;


function ColorToColorRef(cColor: TColor): COLORREF; inline;
begin
  Result := COLORREF(cColor);
end;


function ColorRefToTColor(cColorRef: COLORREF): TColor; inline;
begin
  Result := TColor(cColorRef);
end;


procedure CopyRgbTriple(src: RGBTRIPLE;
                        out srd: RGBTRIPLE); inline;
begin
  srd.rgbtBlue := src.rgbtBlue;
  srd.rgbtGreen := src.rgbtGreen;
  srd.rgbtRed := src.rgbtRed;
end;


procedure CopyRgbTripleToRgbQuad(src: RGBTRIPLE;
                                 out srd: RGBQUAD); inline;
begin
  srd.rgbBlue := src.rgbtBlue;
  srd.rgbGreen := src.rgbtGreen;
  srd.rgbRed := src.rgbtRed;
  srd.rgbReserved := Byte(0);
end;


// Copies RGBQUAD src to RGBTRIPLE srd
procedure CopyRgbQuadToRgbTriple(src: RGBQUAD;
                                 out srd: RGBTRIPLE); inline;
begin
  srd.rgbtBlue := src.rgbBlue;
  srd.rgbtGreen := src.rgbGreen;
  srd.rgbtRed := src.rgbRed;
end;


// copies a DWord (COLORREF) to RGBTRIPLE
procedure CopyClrRefToRgbTriple(src: COLORREF;
                                out srd: RGBTRIPLE); inline;
begin
  srd.rgbtBlue := byte(src shl 0);
  srd.rgbtGreen := byte(src shl 8);
  srd.rgbtRed := byte(src shl 16);
end;


// copies a RGBTRIPLE to DWord (COLORREF)
procedure CopyRgbTripleToClrRef(src: RGBTRIPLE;
                                out srd: COLORREF); inline;
begin
  srd := ((DWord(src.rgbtBlue) shl 0) or
          (DWord(src.rgbtGreen) shl 8) or
          (DWord(src.rgbtRed) shl 16) or
          ($00000000 shl 24));
end;


// copies a RGBQUAD to DWord (COLORREF)
procedure CopyRGBQuadToClrRef(src: RGBQUAD;
                              out srd: COLORREF); inline;
begin
  srd := ((DWord(src.rgbBlue) shl 0) or
          (DWord(src.rgbGreen) shl 8) or
          (DWord(src.rgbRed) shl 16) or
          (DWord(src.rgbReserved) shl 24)); // this should always be 0!
end;


// copies a DWord (COLORREF) to RGBQUAD
procedure CopyClrRefToRGBQuad(src: COLORREF;
                              out srd: RGBQUAD); inline;
begin
  srd.rgbBlue := byte(src shl 0);
  srd.rgbGreen := byte(src shl 8);
  srd.rgbRed := byte(src shl 16);
  srd.rgbReserved := byte(src shl 24); // this should always be 0!
end;


function ConvertYCrCbToRGB(aY: Integer;
                           aCr: Integer;
                           aCb: Integer): RGBQUAD; inline;
var
  rgbq: RGBQUAD;
  c, d, e: Integer;

begin
  rgbq := Default(RGBQUAD);
  c := aY - 16;
  d := aCb - 128;
  e := aCr - 128;

  rgbq.rgbBlue :=  Clip(( 298 * c + 516 * d + 128) shr 8);
  rgbq.rgbGreen := Clip(( 298 * c - 100 * d - 208 * e + 128) shr 8);
  rgbq.rgbRed :=   Clip(( 298 * c + 409 * e + 128) shr 8);

  Result:=  rgbq;
end;


function PixelFormatToStr(PixelFmt: TPixelFormat): string; inline;
begin
  case PixelFmt of
    pfDevice : Result := 'pfDevice';
    pf1bit   : Result := 'pf1bit';
    pf4bit   : Result := 'pf4bit';
    pf8bit   : Result := 'pf8bit';
    pf15bit  : Result := 'pf15bit';
    pf16bit  : Result := 'pf16bit';
    pf24bit  : Result := 'pf24bit';
    pf32bit  : Result := 'pf32bit';
    pfCustom : Result := 'pfCustom';
  else
    Result := 'Unknown';
  end;
end;


// MFVideoNormalizedRect structure
// Defines a normalized rectangle, which is used to specify sub-rectangles in a
// video rectangle.
// When a rectangle N is normalized relative to some other rectangle R,
// it means the following:
//
//  The coordinate (0.0, 0.0) on N is mapped to the upper-left corner of R.
//  The coordinate (1.0, 1.0) on N is mapped to the lower-right corner of R.
//  Any coordinates of N that fall outside the range [0..1] are mapped to positions
//  outside the rectangle R.
//
// A normalized rectangle can be used to specify a region within a video rectangle,
// without knowing the resolution or even the aspect ratio of the video.
// For example, the upper-left quadrant is defined as {0.0, 0.0, 0.5, 0.5}.

// Note: TRect/TRectF record methods are defined in Delphi



// MFVideoNormalizedRect methods ///////////////////////////////////////////////

//  example rcSrc:= SetTRect(0, 0, 10, 20);
// Set MFVideoNormalizedRect values
function SetVNRect(const nrLeft: FLOAT = 0.0;
                   const nrTop: FLOAT = 0.0;
                   const nrRight: FLOAT = 0.0;
                   const nrBottom: FLOAT = 0.0): MFVideoNormalizedRect; inline;
begin
  Result.Top := nrTop;
  Result.Bottom := nrRight;
  Result.Left := nrLeft;
  Result.Right := nrRight;
end;


// Copy MFVideoNormalizedRect values to another MFVideoNormalizedRect
procedure CopyVNRectToVNRect(const rs: MFVideoNormalizedRect;
                             out rd: MFVideoNormalizedRect); inline;
begin
  rd.Top := rs.Top;
  rd.Bottom := rs.Bottom;
  rd.Left := rs.Left;
  rd.Right := rs.Right;
end;


// Copy a MFVideoNormalizedRect to a PMFVideoNormalizedRect
procedure CopyVNRectToPVNRect(rs: MFVideoNormalizedRect;
                              out rd: PMFVideoNormalizedRect); inline;
begin
  rd := @rs
end;


// Copy a PMFVideoNormalizedRect to a MFVideoNormalizedRect
procedure CopyPVNRectToVNRect(rs: PMFVideoNormalizedRect;
                              out rd: MFVideoNormalizedRect); inline;
begin
  rd := rs^;
end;


// Add a MFVideoNormalizedRect to another
function AddVNRect(const r1: MFVideoNormalizedRect;
                   const r2: MFVideoNormalizedRect): MFVideoNormalizedRect; inline;
begin
  Result.Top := r1.Top + r2.Top;
  Result.Bottom := r1.Bottom + r2.Bottom;
  Result.Left := r1.Left + r2.Left;
  Result.Right := r1.Right + r2.Right;
end;


// Substract a MFVideoNormalizedRect from another
function SubstractVNRect(const r1: MFVideoNormalizedRect;
                         const r2: MFVideoNormalizedRect): MFVideoNormalizedRect; inline;
begin
  Result.Top := r1.Top - r2.Top;
  Result.Bottom := r1.Bottom - r2.Bottom;
  Result.Left := r1.Left - r2.Left;
  Result.Right := r1.Right - r2.Right;
end;


// multiply a MFVideoNormalizedRect with another
function multiplyVNRect(const r1: MFVideoNormalizedRect;
                        const scale: LONG): MFVideoNormalizedRect; inline;
begin
  Result.Top := r1.Top * scale;
  Result.Bottom := r1.Bottom * scale;
  Result.Left := r1.Left * scale;
  Result.Right := r1.Right * scale;
 end;


// Get the width of a MFVideoNormalizedRect
function GetNrRectWidth(const rc: MFVideoNormalizedRect): LONG; inline;
begin
  Result := Trunc(rc.Left + rc.Right);
end;


// Get the height of a MFVideoNormalizedRect
function GetNrRectHeight(const rc: MFVideoNormalizedRect): LONG; inline;
begin
  Result := Trunc(rc.Top + rc.Bottom);
end;


// Checks whether a MFVideoNormalizedRect is empty
function IsEmptyNrRect(const nnRect: MFVideoNormalizedRect): Boolean; inline;
begin
  Result := (nnRect.Left <= nnRect.Right) and (nnRect.Top <= nnRect.Bottom);
end;


//Clear PMFVideoNormalizedRect
procedure ClearVNRect(var ppRect: PMFVideoNormalizedRect); inline;
begin
  ppRect := nil;
end;


// Copy a TRect to a MFVideoNormalizedRect
procedure CopyTRectToVNRect(const rs: TRect;
                            out rd: MFVideoNormalizedRect); inline;
begin
  rd.Top := rs.Top;
  rd.Bottom := rs.Bottom;
  rd.Left := rs.Left;
  rd.Right := rs.Right;
end;


// Copy a MFVideoNormalizedRect to a TRect
procedure CopyVNRectToTRect(const rs: MFVideoNormalizedRect;
                            out rd: TRect); inline;
begin
  rd.Top := Trunc(rs.Top);
  rd.Bottom := Trunc(rs.Bottom);
  rd.Left := Trunc(rs.Left);
  rd.Right := Trunc(rs.Right);
end;

// Copy a PMFVideoNormalizedRect to a TRect
procedure CopyPVNRectToTRect(rs: PMFVideoNormalizedRect;
                             out rd: TRect); inline;
begin
  rd.Top := Trunc(rs^.Top);
  rd.Bottom := Trunc(rs^.Bottom);
  rd.Left := Trunc(rs^.Left);
  rd.Right := Trunc(rs^.Right);
end;

// Copy a PMFVideoNormalizedRect to a PRect
procedure CopyPVNRectToPRect(rs: PMFVideoNormalizedRect;
                             out rd: PRect); inline;
begin
  rd.Top := Trunc(rs.Top);
  rd.Bottom := Trunc(rs.Bottom);
  rd.Left := Trunc(rs.Left);
  rd.Right := Trunc(rs.Right);
end;


// TRect methods ///////////////////////////////////////////////////////////////


// Copy TRect values to another TRect
procedure CopyTRectToTRect(const rs: TRect;
                           out rd: TRect); inline;
begin
  rd.Top := rs.Top;
  rd.Bottom := rs.Bottom;
  rd.Left := rs.Left;
  rd.Right := rs.Right;
end;


// Get the width of a MFVideoNormalizedRect
function GetTRectWidth(const rc: TRect): LONG; inline;
begin
  Result := rc.Left + rc.Right;
end;


// Get the height of a MFVideoNormalizedRect
function GetTRectHeight(const rc: TRect): LONG; inline;
begin
  Result := rc.Top + rc.Bottom;
end;


// Note: LPRect is an alias for PRect.

// Copy a TRect to a LPRECT
procedure CopyTRectToLPRect(rs: TRect;
                            var rd: LPRECT); inline;
var
  rdRect: LPRect;
begin
  New(rdRect); // Create new structure in memory
try
  rdRect.Top := rs.Top;
  rdRect.Left := rs.Left;
  rdRect.Height := rs.Height;
  rdRect.Width := rs.Width;
finally
  rd := rdRect;
  Dispose(rdRect);
end;
end;


// Copy a LPRECT to a TRect
procedure CopyLPRectToTRect(rs: LPRECT;
                            var rd: TRect ); inline;
begin
  rd := rs^;
end;

// Copy a TRect to a PRect
procedure CopyTRectToPRect(rs: TRect;
                           var rd: PRect ); inline;
var
  rdRect: PRect;

begin
  New(rdRect); // Create new structure in memory
try
  rdRect.Top := rs.Top;
  rdRect.Left := rs.Left;
  rdRect.Height := rs.Height;
  rdRect.Width := rs.Width;
finally
  rd := rdRect;
  Dispose(rdRect);
end;
end;


// Copy a PRect to a TRect
procedure CopyPRectToTRect(rs: PRect;
                           var rd: TRect); inline;
var
  rsRect: PRect;
begin
  New(rsRect); // Create new structure in memory
try
  rsRect := rs;
  rd.Top := rsRect.Top;
  rd.Left := rsRect.Left;
  rd.Height := rsRect.Height;
  rd.Width := rsRect.Width;
finally
  Dispose(rsRect);
end;
end;


// MFAREA & MFOFFSET
//==================

// Get Float (Single) value from MFOffSet.
function MFOffsetToFloat(const offset: MFOffset): Float; inline;
begin
  Result := offset.value + (offset.fract / 65536.0);
end;


// The value of the number is value + (fract / float(65536.0)).
function MakeOffset(v: Single): MFOffset; inline;
var
  offset: MFOffset;
  i: Integer;

begin
  i := Trunc(v);
  offset.value := SHORT(i);
  offset.fract := WORD(65536 * (i - offset.value));

  Result := offset;
end;


// Get the display area from a video media type.
function GetVideoDisplayArea(pType: IMFMediaType;
                             pArea: MFVideoArea): HRESULT;
var
  hr: HResult;
  bPanScan: BOOL;
  unWidth: UINT32;
  unHeight: UINT32;

begin
  hr := S_OK;
  bPanScan := BOOL(MFGetAttributeUINT32(pType,
                                        MF_MT_PAN_SCAN_ENABLED,
                                        UINT32(False)));
{$POINTERMATH ON}
  // In pan/scan mode, try to get the pan/scan region.
  if (bPanScan) then
    hr := pType.GetBlob(MF_MT_PAN_SCAN_APERTURE,
                        PUINT8(@pArea),
                        SizeOf(MFVideoArea),
                        nil);


  // If not in pan/scan mode, or the pan/scan region is not set, get the minimimum display aperture.
  if (bPanScan or (hr = MF_E_ATTRIBUTENOTFOUND)) then
    begin
      hr := pType.GetBlob(MF_MT_MINIMUM_DISPLAY_APERTURE,
                          PUINT8(@pArea),
                          SizeOf(MFVideoArea),
                          nil);


      if (hr = MF_E_ATTRIBUTENOTFOUND) then
        begin
          // Minimum display aperture is not set.
          // For backward compatibility with some components, check for a geometric aperture.
          hr := pType.GetBlob(MF_MT_GEOMETRIC_APERTURE,
                              PUINT8(@pArea),
                              SizeOf(MFVideoArea),
                              nil);
        end;

      // Default: Use the entire video area.
      if (hr = MF_E_ATTRIBUTENOTFOUND) then
        begin
          hr := MFGetAttributeSize(pType,
                                   MF_MT_FRAME_SIZE,
                                   unWidth,
                                   unHeight);
          if SUCCEEDED(hr) then
            pArea := MakeArea(0.0,
                              0.0,
                              unWidth,
                              unHeight);
        end;
    end;
{$POINTERMATH OFF}
  Result := hr;
end;


// MakeArea
function MakeArea(X: FLOAT;
                  Y: FLOAT;
                  dwWidth: DWORD;
                  dwHeight: DWORD): MFVideoArea; inline;
var
  area: MFVideoArea;

begin
  area.OffsetX := MakeOffset(x);
  area.OffsetY := MakeOffset(y);
  area.Area.cx := dwWidth;
  area.Area.cy := dwHeight;
  Result := area;
end;


{$WARN SYMBOL_PLATFORM OFF}
// External methods
  function StringFromCLSID; external Ole32Lib name 'StringFromCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}


// Keep for previous versions;
function StrToPWideChar(source: string): PWideChar;// inline;
begin
  Result := PWideChar(source);
end;


// FOURCC functions

// create a FOURCC code
function MAKEFOURCC(const ch0: AnsiChar;
                    const ch1: AnsiChar;
                    const ch2: AnsiChar;
                    const ch3: AnsiChar): FOURCC; inline;
begin
  Result := DWORD(Ord(ch0)) or
            (DWORD(Ord(ch1)) shl 8) or
            (DWORD(Ord(ch2)) shl 16) or
            (DWORD(Ord(ch3)) shl 24);
end;


// Get FOURCC as string (little-endian)
function GETFOURCC(frcc: DWord): WideString; inline;
var
  afc : array[0..3] of AnsiChar;
begin
  afc[0] := AnsiChar(ord(frcc));
  afc[1] := AnsiChar(ord(frcc shr 8));
  afc[2] := AnsiChar(ord(frcc shr 16));
  afc[3] := AnsiChar(ord(frcc shr 24));
  Result := UpperCase(WideString(afc));
end;


// Bitshifting helpers
// Source: Patrick van Logchem, Embarcadero
//=========================================

// UINT64
// getter
function Get64bitBits(const data: UINT64;
                      const index: Integer): Integer; inline;
begin
  Result := (data shr (index shr 8)) and // offset
            ((1 shl Byte(index)) - 1);   // mask
end;

// Setter
procedure Set64bitBits(var data: UINT64;
                       const index: Integer;
                       const value: Integer); inline;
var
  offset: Byte;
  mask: Integer;
  val: Integer;

begin
  val := value;
  Mask := ((1 shl Byte(index)) - 1);

  //Assert(value <= Mask);
  if (val > mask) then
    val := mask;

  offset := index shr 8;
  data := (data and (not (mask shl offset)))
           OR UINT64(val shl offset);
end;



// DWORD
// Getter
function GetDWordBits(const data: DWORD;
                      const index: Integer): Integer; inline;
begin
  Result := (data shr (index shr 8)) and // offset
            ((1 shl Byte(index)) - 1);   // mask
end;

// Setter
procedure SetDWordBits(var data: DWORD;
                       const index: Integer;
                       const value: Integer); inline;
var
  offset: Byte;
  mask: Integer;
  val: Integer;

begin
  val := value;
  Mask := ((1 shl Byte(index)) - 1);

  // Assert(value <= Mask);
  if val > mask then
    val := mask;

  offset := index shr 8;
  data := (data and (not (mask shl offset)))
          OR DWORD(val shl offset);
end;

// Note: UINT can be casted as DWORD, use Set/GetDWordBits

// WORD
// Getter
function GetWordBits(const data: WORD;
                     const index: Integer): WORD; inline;
begin
  Result := (data shr (index shr 8)) and // offset
            ((1 shl Byte(index)) - 1);   // mask
end;

// Setter
procedure SetWordBits(var data: WORD;
                      const index: Integer;
                      const value: Integer); inline;
var
  offset: Byte;
  mask: Integer;
  val: Integer;

begin
  val := value;
  Mask := ((1 shl Byte(index)) - 1);

  //Assert(value <= Mask);
  if val > mask then
    val := mask;

  offset := index shr 8;
  data := (data and (not (mask shl offset)))
          OR WORD(val shl offset);
end;



// BYTE
// Getter
function GetByteBits(const data: Byte;
                     const index: Integer): Byte; inline;
begin
  Result := (data shr (index shr 8)) and
            ((1 shl Byte(index)) - 1);
end;

// Setter
procedure SetByteBits(var data: Byte;
                      const index: Integer;
                      const value: Integer); inline;
begin
  data := (data and (not ((index and $FF) shl (index shr 8)))) or
          ((value and index and $FF) shl (index shr 8));
end;


// System
function BoolToStrYesNo(const aBoolean: Boolean;
                        sYes: string = 'Yes';
                        sNo: string = 'No'): string; inline;
begin
  if aBoolean then
    Result := sYes
  else
    Result := sNo;
end;


// For backward compatibility
function MfpBoolToStr(const aBoolean: Boolean): string; inline;
begin
  BoolToStr(aBoolean);
end;


function BoolToStr(const aBoolean: Boolean): string; inline;
begin
  if aBoolean then
    Result := 'True'
  else
    Result := 'False';
end;


function ConstrainValue(const Value: Integer;
                        Min: Integer;
                        Max: Integer): Integer; inline; overload;
begin
  Result := Value;
  if (Result < Min) then
    Result := Min
  else if (Result > Max) then
    Result := Max
end;


function ConstrainValue(const Value: Int64;
                        Min: Int64;
                        Max: Int64): Int64; inline; overload;
begin
  Result := Value;
  if (Result < Min) then
    Result := Min
  else if (Result > Max) then
    Result := Max
end;


function ConstrainValue(const Value: Float;
                        Min: Float;
                        Max: Float): Float; inline; overload;
begin
  Result := Value;
  if (Result < Min) then
    Result := Min
  else if (Result > Max) then
    Result := Max;
end;


function ConstrainValue(const Value: byte;
                        Min: byte;
                        Max: byte): byte; inline; overload;
begin
  Result := Value;
  if (Result < Min) then
    Result := Min
  else if (Result > Max) then
    Result := Max;
end;


// Strings
function StringCbCat(pszDest: PChar;
                     cbDest: SIZE_T;
                     const pszSrc: string): HRESULT;
begin
  // choose the appropriate function based on the string type
  {$IFDEF UNICODE}
  Result := StringCbCatW(pszDest,
                         cbDest,
                         PWideChar(WideString(pszSrc)));
  {$ELSE}
  Result := StringCbCatA(pszDest,
                         cbDest,
                         PAnsiChar(AnsiString(pszSrc)));
  {$ENDIF}

end;

{$WARN SYMBOL_PLATFORM OFF}
function CreateFile2; external Kernel32Lib name 'CreateFile2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function StringCbCatA; external Kernel32Lib name 'StringCbCatA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StringCbCatW; external Kernel32Lib name 'StringCbCatW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$IF COMPILERVERSION < 29.0}
function StrCmpLogicalW; external Shlwapi32Lib name 'StrCmpLogicalW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}

// WinApi.Windows
{$IF COMPILERVERSION < 34.0}
function CreateEventEx; external Kernel32Lib name 'CreateEventExW';
function CreateEventExA; external Kernel32Lib name 'CreateEventExA';
function CreateEventExW; external Kernel32Lib name 'CreateEventExW';
{$ENDIF}

end.

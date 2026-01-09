// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  Helpers.pas
// Kind: Pascal Unit
// Release date: 21-12-2019
// Language: ENU
//
// Revision Version: 3.1.9
//
// Description:
//   Common helper methods.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 28 up to 36
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
// 
// Copyright (c) FactoryX. All rights reserved.
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
unit Helpers;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Win.ComObj,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi;

type

  // CriticalSection
  TMFCritSec = class
    private
    { private fields }
      FCriticalSection: TRTLCriticalSection;
    public
    { public methods }
      constructor Create();
      destructor Destroy(); override;
      procedure Lock();
      procedure Unlock();
   end;


  // Intitialize COM and MF
  function InitMF(): HResult;
  // Close COM and MF
  function CloseMF(): HResult;


implementation


// TMFCritSec //////////////////////////////////////////////////////////////////

constructor TMFCritSec.Create();
begin
  InitializeCriticalSection(FcriticalSection);
end;

destructor TMFCritSec.Destroy();
begin
  DeleteCriticalSection(FcriticalSection);
end;

procedure TMFCritSec.Lock();
begin
  EnterCriticalSection(FcriticalSection);
end;

procedure TMFCritSec.Unlock();
begin
  LeaveCriticalSection(FcriticalSection);
end;

// COM and Mf //////////////////////////////////////////////////////////////////

function InitMF(): HResult;
var
  hr: HResult;

begin

  // Intialize the Media Foundation platform and
  // check if the current MF version match user's version
  hr := MFStartup(MF_VERSION);

  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('Your computer does not support this Media Foundation API version' +
                         IntToStr(MF_VERSION) + '.'),
                 LPCWSTR('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;
  Result := hr;
end;


function CloseMF(): HResult;
begin
  // Shutdown MF
  Result := MFShutdown();
end;

end.

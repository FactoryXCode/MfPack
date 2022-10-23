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
// Revision Version: 3.1.3
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
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
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

  // If you don't want to use VCL.Forms Application.processMessages(), this is the alternative.
  // Use function GetCurrentThread() from unit Windows.pas to assign it's value to hThread.
  // Example: HandleMessages(GetCurrentThread(), value in milliseconds); or HandleMessages(GetCurrentThread());
  // or Assign your own thread handle to it.
  procedure HandleMessages(hThread: THandle; cWait: Cardinal = INFINITE);


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
  // Initialize the COM library.
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);


  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('COM library initialisation failure.'),
                 LPCWSTR('COM Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;


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
  // Shutdown COM
  CoUninitialize();
end;


procedure HandleMessages(hThread: THandle; cWait: Cardinal = INFINITE);
var
  Msg: TMsg;
begin

  while (MsgWaitForMultipleObjects(1,
                                   hThread,
                                   False,
                                   cWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(Msg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if Msg.Message = WM_QUIT then
        Exit;

      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end;

end.

// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RegisteredAudioDelayMFT.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: COM class factory and registration wrapper for
//              the PCM audio delay MFT.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//
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
unit RegisteredAudioDelayMFT;

// COM activation wrapper for the Sample 9 audio delay transform.
// The transform itself remains a TInterfacedObject.

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  {System}
  System.SysUtils,
  {Application}
  AudioDelayRegistration,
  MfAudioDelayMFT;

  function CreateAudioDelayClassFactory(): IClassFactory;
  function AudioDelayCanUnloadNow(): HResult;


implementation

uses
  {WinApi}
  WinApi.WinError,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform;

var
  // COM may ask to unload the DLL after releasing its last reference.
  // Count factories as well as transforms, and track LockServer separately.
  GObjectCount: LongInt = 0;
  GServerLocks: LongInt = 0;


type
  // The Sample 9 class still implements IUnknown through TInterfacedObject.
  // This subclass only adds DLL lifetime accounting.
  TRegisteredAudioDelayMFT = class(TMfAudioDelayMFT)
  public

    constructor Create();
    destructor Destroy(); override;
  end;

  TAudioDelayClassFactory = class(TInterfacedObject, IClassFactory)
  public

    constructor Create();
    destructor Destroy(); override;

    function CreateInstance(const pUnkOuter: IUnknown;
                            const riid: REFIID;
                            out ppvObject: Pointer): HResult; stdcall;

    function LockServer(fLock: BOOL): HResult; stdcall;
  end;


constructor TRegisteredAudioDelayMFT.Create();
begin

  inherited Create();

  InterlockedIncrement(GObjectCount);
end;


destructor TRegisteredAudioDelayMFT.Destroy();
begin

  InterlockedDecrement(GObjectCount);

  inherited;
end;


constructor TAudioDelayClassFactory.Create();
begin

  inherited Create();

  InterlockedIncrement(GObjectCount);
end;


destructor TAudioDelayClassFactory.Destroy();
begin

  InterlockedDecrement(GObjectCount);

  inherited;
end;


function TAudioDelayClassFactory.CreateInstance(const pUnkOuter: IUnknown;
                                                const riid: REFIID;
                                                out ppvObject: Pointer): HResult;
var
  Transform: IMFTransform;

begin

  ppvObject := nil;

  if Assigned(pUnkOuter) then
    Exit(CLASS_E_NOAGGREGATION);

  try
    // QueryInterface gives the caller its own reference. The local Transform
    // reference is released when this method returns.
    Transform := TRegisteredAudioDelayMFT.Create() as IMFTransform;
    Result := Transform.QueryInterface(riid,
                                       ppvObject);

  except
    on EOutOfMemory do Result := E_OUTOFMEMORY;
    on Exception do Result := E_UNEXPECTED;
  end;
end;


function TAudioDelayClassFactory.LockServer(fLock: BOOL): HResult;
begin

  if fLock then
    InterlockedIncrement(GServerLocks)
  else
    InterlockedDecrement(GServerLocks);

  Result := S_OK;
end;


function CreateAudioDelayClassFactory(): IClassFactory;
begin

  Result := TAudioDelayClassFactory.Create();
end;


function AudioDelayCanUnloadNow(): HResult;
begin

  // An active class factory, transform, or server lock keeps the code loaded.
  if (InterlockedCompareExchange(GObjectCount,
                                 0,
                                 0) = 0) and
     (InterlockedCompareExchange(GServerLocks,
                                 0,
                                 0) = 0) then
    Result := S_OK
  else
    Result := S_FALSE;

end;

end.

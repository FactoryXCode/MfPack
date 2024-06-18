// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: XAudio2_FXMasterLimiter.pas
// Kind: Pascal Unit
// Release date: 28-03-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: XAudio2 Master limiter.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// =============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/xaudio2/xaudio2-audio-effects
//
// Copyright © FacctoryX
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
unit XAudio2_FXMasterLimiter;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  {XAudio2}
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAPO,
  WinApi.DirectX.XAudio2.XAPOFx;


type

{
  // Define a custom XAPO effect class
  TMyCustomEffect = class(TInterfacedObject, IXAPOParameters)
  public
    // Implement methods of the IXAPOParameters interface
    procedure SetParameters(pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;

    procedure GetParameters(out pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;

    procedure OnSetParameters(pParameters: Pointer;
                              ParameterByteSize: UINT32); stdcall;

  end;
}


  TFxMasterLimiter = class(TInterfacedObject, IXAPOParameters)
  private
    pvMasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS;

  public

    constructor Create();
    destructor Destroy(); override;

    // Implement methods of the IXAPOParameters interface
    procedure SetParameters(pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;

    procedure GetParameters(out pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;

    procedure OnSetParameters(pParameters: Pointer;
                              ParameterByteSize: UINT32); stdcall;


    function CreateMasterLimiter(const pMasterLimiterParams: FXMASTERINGLIMITER_PARAMETERS): HResult;

  end;

var
  XAPOMasterLimiter: IInterface;


implementation


constructor TFxMasterLimiter.Create();
begin
  inherited;

  pvMasterLimiterparams.Release := FXMASTERINGLIMITER_DEFAULT_RELEASE;
  pvMasterLimiterparams.Loudness := FXMASTERINGLIMITER_DEFAULT_LOUDNESS;
end;


destructor TFxMasterLimiter.Destroy();
begin
  if Assigned(XAPOMasterLimiter) then
    XAPOMasterLimiter := nil;
  inherited;
end;


function TFxMasterLimiter.CreateMasterLimiter(const pMasterLimiterParams: FXMASTERINGLIMITER_PARAMETERS): HResult;
var
  hr: HResult;

begin
  // Remove previous XAPO.
  if Assigned(XAPOMasterLimiter) then
    XAPOMasterLimiter := nil;

  hr := CreateFX(IID_FXMasteringLimiter,
                 XAPOMasterLimiter,
                 @pMasterLimiterParams,
                 SizeOf(FXMASTERINGLIMITER_PARAMETERS));
  Result := hr;
end;


procedure TFxMasterLimiter.SetParameters(pParameters: Pointer;
                                         ParameterByteSize: UINT32);
begin
//
end;


procedure TFxMasterLimiter.GetParameters(out pParameters: Pointer;
                                         ParameterByteSize: UINT32);
begin
//
end;


procedure TFxMasterLimiter.OnSetParameters(pParameters: Pointer;
                                           ParameterByteSize: UINT32);
begin
//
end;

end.

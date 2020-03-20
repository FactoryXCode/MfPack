// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: VideoBufferLock.pas
// Kind: Pascal Unit
// Release date: 08-03-2018
// Language: ENU
//
// Version: 2.6.4
//
// Description: Requires Windows 7 or later.
//              Manages video preview.
//
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
// ----------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
// =============================================================================
// Source: MFCaptureD3D Sample
//         bufferlock.h : Direct3D bufferlock class.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit VideoBufferLock;

interface

uses
  {Vcl}
  Vcl.Dialogs,
  {MFPACK}
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  MfPack.MfObjects,
  MfPack.Mferror,
  MfPack.WinError;

  {$MINENUMSIZE 4}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}



//-------------------------------------------------------------------
//  VideoBufferLock class
//
//  Locks a video buffer that might or might not support IMF2DBuffer.
//
//-------------------------------------------------------------------
type
  TVideoBufferLock = class(TObject)
  private
    //
    m_pBuffer: IMFMediaBuffer;
    m_p2DBuffer: IMF2DBuffer;
    m_bLocked: BOOL;

  public
    // Constructor & destructor
    constructor Create(const pBuffer: IMFMediaBuffer);
    destructor Destroy(); override;

    procedure UnlockBuffer();
    function LockBuffer(lDefaultStride: LONG;     // Minimum stride (with no padding).
                        dwHeightInPixels: DWORD;  // Height of the image, in pixels.
                        var ppbScanLine0: PByte;  // Receives a pointer to the start of scan line 0.
                        var plStride: LONG        // Receives the actual stride.
                        ): HRESULT;
  end;


implementation


constructor TVideoBufferLock.Create(const pBuffer: IMFMediaBuffer);
var
  hr : HRESULT;

begin
  inherited Create();
  m_bLocked := False;
  m_p2DBuffer := Nil;
  m_pBuffer := pBuffer;
  // Query for the 2-D buffer interface. OK if this fails.
  // The IMFMediaBuffer is optimized to receive the IMF2DBuffer.
  hr := m_pBuffer.QueryInterface(IID_IMF2DBuffer,
                                 m_p2DBuffer);

  if FAILED(hr) then
    Exit; // no exception handling needed if hr fails.
end;


destructor TVideoBufferLock.Destroy();
begin
  UnlockBuffer(); // Unlock the buffer
  m_pBuffer := Nil;
  m_p2DBuffer := Nil;
  inherited Destroy();
end;


//-------------------------------------------------------------------
// LockBuffer
//
// Locks the buffer. Returns a pointer to scan line 0 and returns the stride.
//
// The caller must provide the default stride as an input parameter, in case
// the buffer does not expose IMF2DBuffer. You can calculate the default stride
// from the media type.
//-------------------------------------------------------------------

function TVideoBufferLock.LockBuffer(lDefaultStride: LONG;     // Minimum stride (with no padding).
                                     dwHeightInPixels: DWORD;  // Height of the image, in pixels.
                                     var ppbScanLine0: PByte;  // Receives a pointer to the start of scan line 0.
                                     var plStride: LONG): HRESULT;
var
  hr: HRESULT;
  pData: PByte;  // Any use of PByte require {$POINTERMATH ON)

begin
  New(pData);

  // Use the 2-D version if available.
  if Assigned(m_p2DBuffer) then
    hr := m_p2DBuffer.Lock2D(ppbScanLine0,
                             plStride)
  else
    begin
      // Use non-2D version.
      pData := Nil;

      hr := m_pBuffer.Lock(pData,
                           Nil,
                           Nil);

      if SUCCEEDED(hr) then
        begin
          plStride := lDefaultStride;
            if (lDefaultStride < 0) then
              begin
                // Bottom-up orientation. Return a pointer to the start of the
                // last row *in memory* which is the top row of the image.
                {$WARNINGS OFF}
                ppbScanLine0^ := pData^ + abs(lDefaultStride) * (dwHeightInPixels - 1);
                {$WARNINGS ON}
               end
            else
              begin
                // Top-down orientation.
                // Return a pointer to the start of the buffer.
                ppbScanLine0 := pData;
              end;
        end;
    end;

  Dispose(pData);

  m_bLocked := SUCCEEDED(hr);
  Result := hr;

end;


//-------------------------------------------------------------------
// UnlockBuffer
//
// Unlocks the buffer. Called automatically by the destructor.
//-------------------------------------------------------------------
procedure TVideoBufferLock.UnlockBuffer();
var
  hr: HResult;  //Debug

begin
  hr := S_OK;

  if m_bLocked then
    begin
      if Assigned(m_p2DBuffer) then
        hr := m_p2DBuffer.Unlock2D()
      else
        hr := m_pBuffer.Unlock();

      m_bLocked := False;
    end;

  if FAILED(hr) then
    begin
      ShowMessage('function TVideoBufferLock.UnlockBuffer() failed!');
    end;

end;


end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SampleConverter.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 4.1.0
//
// Description:
//   This unit uses the D2D1 API to perform the translation from sample to bitmap.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX410/Samples/MFFrameCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit SampleConverter;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat,
  // WinApi.DXGI included with Delphi <= 10.3.3 is not up to date!
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {VCL}
  VCL.Graphics,
  {System}
  System.Classes,
  System.Types,
  {Application}
  Support;

type
  TSampleConverter = class(TPersistent)
  protected
    FRenderTarget: ID2D1DCRenderTarget;

  private
    FDPI: Integer;
    F2DBitmapProperties: D2D1_BITMAP_PROPERTIES;
    procedure CreateDirect2DBitmapProperties;
    function CreateRenderTarget: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function BitmapFromSample(const ASample: IMFSample;
                              const AVideoInfo: TVideoFormatInfo;
                              var AError: string;
                              var AImage: TBitmap): Boolean;
  end;

implementation
uses System.SysUtils;
constructor TSampleConverter.Create;
begin inherited;end;
destructor TSampleConverter.Destroy;
begin inherited;end;
procedure TSampleConverter.CreateDirect2DBitmapProperties;
begin end;
function TSampleConverter.CreateRenderTarget: Boolean;
begin Result:=False;end;
function TSampleConverter.BitmapFromSample(const ASample: IMFSample;
 const AVideoInfo: TVideoFormatInfo; var AError: string; var AImage: TBitmap): Boolean;
var Buffer: IMFMediaBuffer; Data: PByte; Count: DWORD;
    RowBytes, Pitch, Y, Offset: Integer; Required: UInt64;
    Header: TBitmapFileHeader; Info: TBitmapInfoHeader; Stream: TMemoryStream;
begin
 Result:=False; AError:='Invalid RGB32 sample';
 if (ASample=nil) or (AImage=nil) or (AVideoInfo.iVideoWidth<=0) or
    (AVideoInfo.iVideoHeight<=0) or (AVideoInfo.iVideoWidth>MaxInt div 4) then Exit;
 RowBytes:=AVideoInfo.iVideoWidth*4; Pitch:=AVideoInfo.iStride;
 if Pitch=0 then Pitch:=RowBytes;
 if (Pitch=Low(Integer)) or (Abs(Pitch)<RowBytes) then Exit;
 Required:=UInt64(Abs(Pitch))*UInt64(AVideoInfo.iVideoHeight-1)+UInt64(RowBytes);
 if (Required>MaxInt) or
    (UInt64(RowBytes)*UInt64(AVideoInfo.iVideoHeight)>High(DWORD)-54) then Exit;
 if FAILED(ASample.ConvertToContiguousBuffer(@Buffer)) then Exit;
 if FAILED(Buffer.Lock(Data,nil,@Count)) then Exit;
 try
  if (Data=nil) or (Required>Count) then Exit;
  Stream:=TMemoryStream.Create;
  try
   FillChar(Header,SizeOf(Header),0); FillChar(Info,SizeOf(Info),0);
   Header.bfType:=$4D42; Header.bfOffBits:=SizeOf(Header)+SizeOf(Info);
   Info.biSize:=SizeOf(Info); Info.biWidth:=AVideoInfo.iVideoWidth;
   Info.biHeight:=-AVideoInfo.iVideoHeight; Info.biPlanes:=1; Info.biBitCount:=32;
   Info.biSizeImage:=RowBytes*AVideoInfo.iVideoHeight;
   Header.bfSize:=Header.bfOffBits+Info.biSizeImage;
   Stream.WriteBuffer(Header,SizeOf(Header)); Stream.WriteBuffer(Info,SizeOf(Info));
   for Y:=0 to AVideoInfo.iVideoHeight-1 do begin
    if Pitch<0 then Offset:=(AVideoInfo.iVideoHeight-1-Y)*(-Pitch) else Offset:=Y*Pitch;
    Stream.WriteBuffer(PByte(NativeUInt(Data)+NativeUInt(Offset))^,RowBytes);
   end;
   Stream.Position:=0; AImage.LoadFromStream(Stream); AError:=''; Result:=True;
  finally Stream.Free;end;
 finally Buffer.Unlock;end;
end;
end.

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SampleConverter.pas
// Kind: Pascal Unit
// Release date: 18-03-2022
// Language: ENU
//
// Revision Version: 3.1.1
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
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
// 22/03/2022 Ciaran              Added support for sample conversion using color converter
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX311/Samples/MFFrameSample
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
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
  {$IF CompilerVersion > 33}
  // Delphi 10.4 or above
  // WinApi.DXGI included with Delphi <= 10.3.3 is not up to date!
  WinApi.D2D1,
  WinApi.DxgiFormat,
  WinAPI.ActiveX,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.ComBaseApi,
  {$ENDIF}
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.UuIds,
  {VCL}
  VCL.Graphics,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  {Application}
  Support;


type
  TSampleReturnType = (srImage, srImageAndData, srData);

  TSampleConverter = class(TPersistent)
  protected
    FRenderTarget: ID2D1DCRenderTarget;
    FOutputType : IMFMediaType;
    FTransform : IMFTransform;
    FOnLog: TLogEvent;
    FSupportedInputs : TArray<TGUID>;
  private
    FDPI: Integer;
    F2DBitmapProperties: D2D1_BITMAP_PROPERTIES;
    FSampleReturnType : TSampleReturnType;

    procedure CreateDirect2DBitmapProperties;
    function CreateRenderTarget: Boolean;
    function ConvertSampleToRGB(const AInputSample : IMFSample; out AConvertedSample : IMFSample) : Boolean;
    function CheckSucceeded(AStatus : HRESULT; const AMethod : string; ALogFailure : Boolean = True): Boolean;
    function IndexOf(const AInput: TGUID; const AValues: array of TGUID): Integer;

    procedure FreeConverter;
    procedure NotifyBeginStreaming;
    procedure SetSupportedInputs;
    procedure RenderToBMP(ASourceRect : TRect; const ASurface : ID2D1Bitmap);
  public
    constructor Create;
    destructor Destroy; override;

    function UpdateConverter(const AInputType: IMFMediaType): Boolean;

    function BitmapFromSample(const ASample: IMFSample;
                              const AVideoInfo: TVideoFormatInfo;
                              var AError: string;
                              var AImage: TBitmap): Boolean;

    function IsInputSupported(const AInputFormat : TGUID) : Boolean;

    property SampleReturnType : TSampleReturnType read FSampleReturnType write FSampleReturnType;

    // Event hooks
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;


implementation

constructor TSampleConverter.Create;
begin
  inherited;
  FDPI := DevicePixelPerInch;
  CreateRenderTarget;
  CreateDirect2DBitmapProperties;
  SetSupportedInputs;
end;


destructor TSampleConverter.Destroy;
begin
  FreeConverter;
  FRenderTarget.Flush();
  SafeRelease(FRenderTarget);
  inherited;
end;

procedure TSampleConverter.SetSupportedInputs;
begin
  SetLength(FSupportedInputs, 20);
  FSupportedInputs[0] := MFVideoFormat_RGB24;
  FSupportedInputs[1] := MFVideoFormat_RGB32;
  FSupportedInputs[2] := MFVideoFormat_RGB555;
  FSupportedInputs[3] := MFVideoFormat_RGB565;
  FSupportedInputs[4] := MFVideoFormat_RGB8;
  FSupportedInputs[5] := MFVideoFormat_AYUV;
  FSupportedInputs[6] := MFVideoFormat_I420;
  FSupportedInputs[7] := MFVideoFormat_IYUV;
  FSupportedInputs[8] := MFVideoFormat_NV11;
  FSupportedInputs[9] := MFVideoFormat_NV12;
  FSupportedInputs[10] := MFVideoFormat_UYVY;
  FSupportedInputs[11] := MFVideoFormat_V216;
  FSupportedInputs[12] := MFVideoFormat_V410;
  FSupportedInputs[13] := MFVideoFormat_Y41P;
  FSupportedInputs[14] := MFVideoFormat_Y41T;
  FSupportedInputs[15] := MFVideoFormat_Y42T;
  FSupportedInputs[16] := MFVideoFormat_YUY2;
  FSupportedInputs[17] := MFVideoFormat_YV12;
  FSupportedInputs[18] := MFVideoFormat_YVU9;
  FSupportedInputs[19] := MFVideoFormat_YVYU;
end;


function TSampleConverter.IsInputSupported(const AInputFormat: TGUID): Boolean;
begin
  Result := IndexOf(AInputFormat, FSupportedInputs) > -1;
end;

function TSampleConverter.IndexOf(const AInput : TGUID; const AValues : array of TGUID) : Integer;
begin
  Result := high(AValues);
  while (Result >= low(AValues)) and (AInput <> AValues[Result]) do
    Dec(Result);
end;

function TSampleConverter.CreateRenderTarget: Boolean;
var
  pFactory: ID2D1Factory;
  oProperties: D2D1_RENDER_TARGET_PROPERTIES;

begin
  Result := SUCCEEDED(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
                                        IID_ID2D1Factory,
                                        Nil,
                                        pFactory));
  if Result then
    begin
      {$IF CompilerVersion > 33}
      // Delphi 10.4 or above
      // For some reason software rendering appears to perform slightly better.
      // Change to 'D2D1_RENDER_TARGET_TYPE_DEFAULT' to test otherwise.
      oProperties.&type := D2D1_RENDER_TARGET_TYPE_SOFTWARE;
      oProperties.PixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      oProperties.PixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      {$ELSE}
      oProperties._type := D2D1_RENDER_TARGET_TYPE_SOFTWARE;
      oProperties._PixelFormat.Format := WinApi.DirectX.DXGIFormat.DXGI_FORMAT_B8G8R8A8_UNORM;
      oProperties._PixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      {$ENDIF}

      oProperties.dpiX := 0;
      oProperties.dpiY := 0;

      oProperties.usage := D2D1_RENDER_TARGET_USAGE_NONE;
      oProperties.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;
      Result := SUCCEEDED(pFactory.CreateDCRenderTarget(oProperties,
                                                        FRenderTarget));
    end;
end;


procedure TSampleConverter.CreateDirect2DBitmapProperties;
var
  oPixelFormat: D2D1_PIXEL_FORMAT;
begin
  oPixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;

  {$IF CompilerVersion > 33}
  // Delphi 10.4 or above
  oPixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  F2DBitmapProperties.PixelFormat := oPixelFormat;
  {$ELSE}
  oPixelFormat.Format := WinApi.DirectX.DXGIFormat.DXGI_FORMAT_B8G8R8A8_UNORM;
  F2DBitmapProperties._PixelFormat := oPixelFormat;
  {$ENDIF}

  F2DBitmapProperties.dpiX := FDPI;
  F2DBitmapProperties.dpiY := FDPI;
end;


function TSampleConverter.BitmapFromSample(const ASample: IMFSample;
                                           const AVideoInfo: TVideoFormatInfo;
                                           var AError: string;
                                           var AImage: TBitmap): Boolean;
var
  pBuffer: IMFMediaBuffer;
  pBitmapData: PByte;
  cbBitmapData: DWord;
  o2DBitmap: ID2D1Bitmap;
  oSize: D2D1_SIZE_U;
  iPitch: Integer;
  iActualBMPDataSize: Integer;
  iExpectedBMPDataSize: Integer;
  {$IFDEF CompilerVersion <= 33}
  pClipRect: PRect;
  {$ENDIF}
  pConvertedSample : IMFSample;
begin
  AError := '';
  pConvertedSample := nil;

  if AVideoInfo.oSubType <> MFVideoFormat_RGB32 then
  begin
    Result := ConvertSampleToRGB(ASample, pConvertedSample);

    if Result then
     // Converts a sample with multiple buffers into a sample with a single buffer.
    Result := SUCCEEDED(pConvertedSample.ConvertToContiguousBuffer(pBuffer));
  end
  else
    // Converts a sample with multiple buffers into a sample with a single buffer.
    Result := SUCCEEDED(ASample.ConvertToContiguousBuffer(pBuffer));

  try

  if Result then
    begin
      Result := SUCCEEDED(pBuffer.Lock(pBitmapData,
                                       Nil,
                                       @cbBitmapData));
      try
        iPitch := 4 * AVideoInfo.iVideoWidth;

        // For full frame capture, use the buffer dimensions for the data size check
        iExpectedBMPDataSize := (AVideoInfo.iBufferWidth * 4) * AVideoInfo.iBufferHeight;
        iActualBMPDataSize := Integer(cbBitmapData);

        if Result then
          Result := iActualBMPDataSize = iExpectedBMPDataSize;
        if not Result then
        begin
          AError := Format('Sample size does not match expected size. Current: %d. Expected: %d',
                           [iActualBMPDataSize, iExpectedBMPDataSize]);
        end;

        if Result then
          begin
            // Bind the render target to the bitmap
            {$IF CompilerVersion > 33}
             // Delphi 10.4 or above
            Result := SUCCEEDED(FRenderTarget.BindDC(AImage.Canvas.Handle, AImage.Canvas.ClipRect));
            {$ELSE}

            CopyTRectToPRect(AImage.Canvas.ClipRect,
                             pClipRect);
            Result := SUCCEEDED(FRenderTarget.BindDC(AImage.Canvas.Handle,
                                                     pClipRect));
            {$ENDIF}

            if Result then
              begin
                // Create the 2D bitmap interface
                oSize.Width := AVideoInfo.iVideoWidth;
                oSize.Height := AVideoInfo.iVideoHeight;
                Result := SUCCEEDED(FRenderTarget.CreateBitmap(oSize,
                                                               pBitmapData,
                                                               iPitch,
                                                               F2DBitmapProperties,
                                                               o2DBitmap));
              end;

             // Draw the 2D bitmap to the render target
            if Result then
              RenderToBMP(AImage.Canvas.ClipRect, o2DBitmap);
          end;

      finally
        pBuffer.Unlock;
        SafeRelease(pBuffer);
      end;
    end;

  finally
    pBitmapData := Nil;
    {$IFDEF CompilerVersion <= 33}
    pClipRect := Nil;
    {$ENDIF}
  end;

  if Assigned(pConvertedSample) then
    SafeRelease(pConvertedSample);
end;

procedure TSampleConverter.RenderToBMP(ASourceRect : TRect; const ASurface : ID2D1Bitmap);
var
  iFrequency: int64;
  iStart: int64;
  iEnd : int64;
begin
  QueryPerformanceFrequency(iFrequency);
  QueryPerformanceCounter(iStart);

  FRenderTarget.BeginDraw;
  try
    FRenderTarget.DrawBitmap(ASurface);
  finally
    FRenderTarget.EndDraw();
  end;

  QueryPerformanceCounter(iEnd);
  if Assigned(OnLog) then
      OnLog(Format('RenderBMP took %f milliseconds',[(iEnd - iStart) / iFrequency * 1000]), ltDebug);
end;

procedure TSampleConverter.FreeConverter;
begin
  SafeRelease(FTransform);
  FOutputType := nil;
end;

function TSampleConverter.UpdateConverter(const AInputType : IMFMediaType) : Boolean;
begin
  FreeConverter;

  // Create the color converter
  // See: https://docs.microsoft.com/en-us/windows/win32/medfound/colorconverter
  Result := SUCCEEDED(CoCreateInstance(CLSID_CColorConvertDMO, nil, CLSCTX_INPROC_SERVER, IID_IMFTransform, FTransform));

  if Result then
  begin
    // Set the input type for the transform
    if Result then
      Result := SUCCEEDED(FTransform.SetInputType(0, AInputType, 0));

    // Create the output type
    if Result then
      Result := SUCCEEDED(MFCreateMediaType(FOutputType));

    // Copy all the properties from the input type to the output
    if Result then
      Result := SUCCEEDED(AInputType.CopyAllItems(FOutputType));

    // Set sub type RGB32
    if Result then
      Result := SUCCEEDED(FOutputType.SetGUID(MF_MT_SUBTYPE, MFVideoFormat_RGB32));

    // Assign the output type to the transform
    if Result then
      Result := SUCCEEDED(FTransform.SetOutputType(0, FOutputType, 0));

    NotifyBeginStreaming;
  end;
end;

procedure TSampleConverter.NotifyBeginStreaming;
begin
  // This should speed up the first frame request.
  // See: https://docs.microsoft.com/en-us/windows/win32/medfound/mft-message-notify-begin-streaming
  FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING, 0);
end;

function TSampleConverter.ConvertSampleToRGB(const AInputSample : IMFSample; out AConvertedSample : IMFSample) : Boolean;
var
  oStatus : DWord;
  oResult : HResult;
  pBufferOut : IMFMediaBuffer;
  oOutputDataBuffer : MFT_OUTPUT_DATA_BUFFER;
  oOutputStreamInfo : MFT_OUTPUT_STREAM_INFO;
  iFrequency: int64;
  iConvertStart: int64;
  iConvertEnd : int64;
begin
  QueryPerformanceFrequency(iFrequency);
  QueryPerformanceCounter(iConvertStart);

  Result := CheckSUCCEEDED(FTransform.ProcessInput(0, AInputSample, 0), 'ConvertSampleToRGB');

  if Result then
  begin
    Result := SUCCEEDED(FTransform.GetOutputStreamInfo(0, oOutputStreamInfo));
    try
      if Result then
        Result := SUCCEEDED(MFCreateMemoryBuffer(oOutputStreamInfo.cbSize, pBufferOut));

      if Result then
        Result := SUCCEEDED(MFCreateSample(AConvertedSample));

      if Result then
        Result := SUCCEEDED(AConvertedSample.AddBuffer(pBufferOut));

      if Result then
      begin
        oOutputDataBuffer.dwStreamID := 0;
        oOutputDataBuffer.dwStatus := 0;
        oOutputDataBuffer.pSample := AConvertedSample;
        oOutputDataBuffer.pEvents := nil;

        oResult := FTransform.ProcessOutput(DWord(MFT_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER), 1, @oOutputDataBuffer, oStatus);
        Result := SUCCEEDED(oResult);
      end;
    finally
      SafeRelease(pBufferOut);
    end;
  end;

  QueryPerformanceCounter(iConvertEnd);
  if Assigned(OnLog)  then
   OnLog(Format('ConvertSampleToRGB took %f milliseconds.',
               [(iConvertEnd - iConvertStart) / iFrequency * 1000]),
                                 ltDebug);

end;


function TSampleConverter.CheckSucceeded(AStatus : HRESULT; const AMethod : string; ALogFailure : Boolean = True) : Boolean;
begin
 Result := SUCCEEDED(AStatus);
 if not Result and Assigned(OnLog) then
    OnLog(Format('Method "%s" failed. Error code: %d',  [AMethod, AStatus]), ltError);
end;

end.

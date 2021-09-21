unit SampleConverter;

interface

uses
  {Winapi}
  WinApi.Windows, //
  WinApi.MediaFoundationApi.MfObjects, //
  WinApi.MediaFoundationApi.MfUtils, //
  WinApi.DirectX.D2D1, //
  WinApi.D2D1, //
  {VCL}
  VCL.Graphics, //
  {System}
  System.Classes, //
  {Application}
  Support;

type
  TSampleConverter = class(TPersistent)
  private
    FDPI : Integer;
    FRenderTarget : ID2D1DCRenderTarget;
    F2DBitmapProperties : TD2D1BitmapProperties;
    procedure CreateDirect2DBitmapProperties;
    function CreateRenderTarget : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function BitmapFromSample(const ASample : IMFSample; const AVideoInfo : TVideoFormatInfo; var AError : string; var AImage : TBitmap)
      : Boolean;
  end;

implementation

uses
  {Winapi}
  WinApi.DxgiFormat, //
  {System}
  System.SysUtils;

constructor TSampleConverter.Create;
begin
  inherited;
  FDPI := DevicePixelPerInch;
  CreateRenderTarget;
  CreateDirect2DBitmapProperties;
end;

destructor TSampleConverter.Destroy;
begin
  inherited;
  FRenderTarget.Flush();
  SafeRelease(FRenderTarget);
end;

function TSampleConverter.CreateRenderTarget : Boolean;
var
  pFactory : ID2D1Factory;
  oProperties : TD2D1RenderTargetProperties;
begin
  Result := SUCCEEDED(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, IID_ID2D1Factory, nil, pFactory));
  try
    if Result then
    begin
      oProperties.&type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
      oProperties.PixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      oProperties.PixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      oProperties.dpiX := 0;
      oProperties.dpiY := 0;
      oProperties.usage := D2D1_RENDER_TARGET_USAGE_NONE;
      oProperties.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;
      Result := SUCCEEDED(pFactory.CreateDCRenderTarget(oProperties, FRenderTarget));
    end;
  finally
    pFactory := nil;
  end;
end;

procedure TSampleConverter.CreateDirect2DBitmapProperties;
var
  oPixelFormat : D2D1_PIXEL_FORMAT;
begin
  oPixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  oPixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
  F2DBitmapProperties.PixelFormat := oPixelFormat;
  F2DBitmapProperties.dpiX := FDPI;
  F2DBitmapProperties.dpiY := FDPI;
end;

function TSampleConverter.BitmapFromSample(const ASample : IMFSample; const AVideoInfo : TVideoFormatInfo; var AError : string;
  var AImage : TBitmap) : Boolean;
var
  pBuffer : IMFMediaBuffer;
  pBitmapData : PByte;
  cbBitmapData : DWord;
  o2DBitmap : ID2D1Bitmap;
  oSize : D2D1_SIZE_U;
  iPitch : Integer;
  iActualBMPDataSize : Integer;
  iExpectedBMPDataSize : Integer;
begin
  AError := '';
  Result := SUCCEEDED(ASample.ConvertToContiguousBuffer(pBuffer));
  try
    if Result then
    begin
      Result := SUCCEEDED(pBuffer.Lock(pBitmapData, nil, @cbBitmapData));
      try
        iPitch := 4 * AVideoInfo.iVideoWidth;
        // For full frame capture, use the buffer dimensions for the data size check
        iExpectedBMPDataSize := (AVideoInfo.iBufferWidth * 4) * AVideoInfo.iBufferHeight;
        iActualBMPDataSize := Integer(cbBitmapData);
        if iActualBMPDataSize <> iExpectedBMPDataSize then
          AError := Format('Sample size does not match expected size. Current: %d. Expected: %d',
            [iActualBMPDataSize, iExpectedBMPDataSize]);
        if Result then
        begin
          // Bind the render target to the bitmap
          Result := SUCCEEDED(FRenderTarget.BindDC(AImage.Canvas.Handle, AImage.Canvas.ClipRect));
          if Result then
          begin
            // Create the 2D bitmap interface
            oSize.Width := AVideoInfo.iVideoWidth;
            oSize.Height := AVideoInfo.iVideoHeight;
            Result := SUCCEEDED(FRenderTarget.CreateBitmap(oSize, pBitmapData, iPitch, F2DBitmapProperties, o2DBitmap));
          end;
          // Draw the 2D bitmap to the render target
          if Result then
          begin
            try
              FRenderTarget.BeginDraw;
              try
                FRenderTarget.DrawBitmap(o2DBitmap);
              finally
                FRenderTarget.EndDraw();
              end;
            finally
              SafeRelease(o2DBitmap);
            end;
          end;
        end;
      finally
        pBuffer.Unlock;
        SafeRelease(pBuffer);
      end;
    end;
  finally
    pBitmapData := nil;
    SafeRelease(pBuffer);
  end;
end;

end.

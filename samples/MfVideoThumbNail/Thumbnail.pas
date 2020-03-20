// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
// Module: Thumbnail.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Videothumbnail generator.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
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
//==============================================================================
// Source: Microsoft samples.
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
//==============================================================================
unit Thumbnail;

interface

uses
  {WinApi}
  WinApi.Windows,
  Winapi.Messages,
  //Winapi.ActiveX,
  {System}
  System.Classes,
  System.Math,
  System.SysUtils,
  {MfPack dx}
  MfPack.DCommon,
  MfPack.DXGIFormat,
  MfPack.D2D1,
  MfPack.D2D1Helper,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfReadWrite,
  MfPack.MfError,
  MfPack.MfpUtils,
  MfPack.MfIdl,
  MfPack.MfObjects,
  MfPack.MfApi,
  MfPack.PropIdl,
  MfPack.PropVarUtil,
  {Application}
  Sprite,
  VideoTumbNailHelpers;

const
  SEEK_TOLERANCE = LONGLONG(10000000);
  MAX_FRAMES_TO_SKIP = LONGLONG(10);

type

  TThumbnailGenerator = class
  private

    llSampleTime: LONGLONG;

    m_pReader: IMFSourceReader;
    m_format: TFormatInfo;

    function CreateBitmap(pRT: ID2D1RenderTarget;
                          var hnsPos: LONGLONG;
                          var pSprite: TSprite): HRESULT;

    function SelectVideoStream(): HRESULT;

    function GetVideoFormat(out pFormat: TFormatInfo): HRESULT;

  public

    // Constructor & Destructor
    constructor Create();
    destructor Destroy(); override;


    function OpenFile(const wszFileName: LPCWSTR): HRESULT;
    function GetDuration(out phnsDuration: LONGLONG): HRESULT;
    function CanSeek(out pbCanSeek: BOOL): HRESULT;

    function CreateBitmaps(pRT: ID2D1RenderTarget;
                           count: DWORD;
                           pSprites: TSpritesArray): HRESULT;
  end;

var
  g_ThumbnailGen: TThumbnailGenerator;

// Helper
/////////////////////////////////////////////////////////////////////
function CorrectAspectRatio(const src: TRect;
                            const srcPAR: MFRatio): TRect;

implementation

//-------------------------------------------------------------------
// ThumbnailGenerator constructor
//-------------------------------------------------------------------
constructor TThumbnailGenerator.Create();
begin
  if (m_pReader = Nil) then
    begin
      ZeroMemory(@m_format,
                 sizeof(m_format));
    end;
end;

//-------------------------------------------------------------------
// ThumbnailGenerator destructor
//-------------------------------------------------------------------
destructor TThumbnailGenerator.Destroy();
begin
  SafeRelease(m_pReader);
  ZeroMemory(@m_format,
             sizeof(m_format));
end;


// private methods
/////////////////////////////////////////////////////////////////////

function TThumbnailGenerator.CreateBitmap(pRT: ID2D1RenderTarget;
                                          var hnsPos: LONGLONG;
                                          var pSprite: TSprite): HRESULT;
label
  done;

var
  hr: HResult;
  dwFlags: DWORD;
  pBitmapData: PByte;
  cbBitmapData: DWORD;
  hnsTimeStamp: LONGLONG;
  bCanSeek: BOOL;
  bBreak: BOOL;
  cSkipped: DWORD;
  pBuffer: IMFMediaBuffer;
  pSample: IMFSample;
  pSampleTmp: IMFSample;
  pBitmap: ID2D1Bitmap;
  _var: MfPROPVARIANT;
  pitch: UINT32;

  // debug
  //iSpriteIndex: Integer;

begin

  // debug: check sprite index
  //iSpriteIndex:= pSprite.m_SpriteIndex;

  dwFlags := 0;
  pBitmapData := Nil;      // Bitmap data
  cbBitmapData := 0;       // Size of data, in bytes
  hnsTimeStamp := 0;
  bCanSeek := FALSE;       // Can the source seek?
  cSkipped := 0;           // Number of skipped frames

  pBuffer := Nil;
  pSample := Nil;
  pBitmap := Nil;

  hr := CanSeek(bCanSeek);
  if (FAILED(hr)) then
    begin
      Result := hr;
      Exit;
    end;

  if (bCanSeek AND (hnsPos > 0)) then
    begin
      PropVariantInit(_var);

      _var.vt := VT_I8;
      _var.hVal.QuadPart := hnsPos;

      hr := m_pReader.SetCurrentPosition(GUID_NULL,
                                         _var);

      if (FAILED(hr)) then
        begin
          goto done;
        end;
    end;

  // Pulls video frames from the source reader.

  // NOTE: Seeking might be inaccurate, depending on the container
  //       format and how the file was indexed. Therefore, the first
  //       frame that we get might be earlier than the desired time.
  //       If so, we skip up to MAX_FRAMES_TO_SKIP frames.
  bBreak := FALSE;
  repeat
    begin

      pSampleTmp := Nil;

      // Read Sample
      hr := m_pReader.ReadSample(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                 0,
                                 Nil,
                                 @dwFlags,
                                 Nil,
                                 @pSampleTmp);

      if (FAILED(hr)) then
        begin
          case hr of
            E_POINTER: begin { Pointer error } end;
            else
              begin { Other error } end;
          end;


          if hr = E_POINTER then

         goto done;
        end;

      if (dwFlags = DWORD(MF_SOURCE_READERF_ENDOFSTREAM)) then
        begin
          break;
        end;

      if (dwFlags = DWORD(MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED)) then
        begin
          // Type change. Get the new format.
          hr := GetVideoFormat(m_format);

          if (FAILED(hr)) then
            begin
              goto done;
            end;
        end;

      if (pSampleTmp = Nil) then
        begin
          continue;
        end;

      // We got a sample. Hold onto it.

      pSample := Nil;
      pSample := pSampleTmp;

      if (SUCCEEDED(pSample.GetSampleTime(hnsTimeStamp))) then
        begin
          // Keep going until we get a frame that is within tolerance of the
          // desired seek position, or until we skip MAX_FRAMES_TO_SKIP frames.

          // During this process, we might reach the end of the file, so we
          // always cache the last sample that we got (pSample).

          if ((cSkipped < MAX_FRAMES_TO_SKIP) AND
                 (hnsTimeStamp + SEEK_TOLERANCE < hnsPos)) then
            begin
                pSampleTmp := Nil;
                inc(cSkipped);
                Continue;
            end;
        end;

        pSampleTmp := Nil;

        hnsPos := hnsTimeStamp;
        bBreak := TRUE;
    end;
  until bBreak = TRUE;

  if Assigned(pSample) then
    begin
      pitch := (4 * m_format.imageWidthPels);

      // Get the bitmap data from the sample, and use it to create a
      // Direct2D bitmap object. Then use the Direct2D bitmap to
      // initialize the sprite.

      hr := pSample.ConvertToContiguousBuffer(pBuffer);

      if FAILED(hr) then
        begin
          goto done;
        end;

      // get the frame time of the sample
      hr := pSample.GetSampleTime(llSampleTime);

      if (FAILED(hr)) then
        begin
          goto done;
        end;

      hr := pBuffer.Lock(pBitmapData,
                         Nil,
                         @cbBitmapData);

      if (FAILED(hr)) then
        begin
          goto done;
        end;

      assert(cbBitmapData = (pitch * m_format.imageHeightPels));  // debug!

      hr := pRT.CreateBitmap(D2D1SizeU(m_format.imageWidthPels,
                                       m_format.imageHeightPels),
                             pBitmapData,
                             pitch,
                             D2D1BitmapProperties(D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, // Format = RGB32
                                                                  D2D1_ALPHA_MODE_IGNORE)),
                             pBitmap);


      if (FAILED(hr)) then
        begin
          goto done;
        end;

      pSprite.SetBitmap(pBitmap,
                        m_format);
    end
  else
    begin
      hr := MF_E_END_OF_STREAM;
    end;

done:

  if Assigned(pBitmapData) then
    begin
      pBuffer.Unlock();
    end;

  pBuffer := Nil;
  pSample := Nil;
  pSampleTmp := Nil;

  Result := hr;
end;

//-------------------------------------------------------------------
// SelectVideoStream
//
// Finds the first video stream and sets the format to RGB32.
//-------------------------------------------------------------------
function TThumbnailGenerator.SelectVideoStream(): HRESULT;
var
  hr: HResult;
  pType: IMFMediaType;

begin
  pType := Nil;

  // Configure the source reader to give us progressive RGB32 frames.
  // The source reader will load the decoder if needed.

  hr := MFCreateMediaType(pType);

  if (SUCCEEDED(hr)) then
    begin
      hr := pType.SetGUID(MF_MT_MAJOR_TYPE,
                          MFMediaType_Video);
    end;

  if (SUCCEEDED(hr)) then
    begin
      hr := pType.SetGUID(MF_MT_SUBTYPE,
                          MFVideoFormat_RGB32);
    end;


  if (SUCCEEDED(hr)) then
    begin
      hr := m_pReader.SetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                          0,
                                          pType);
    end;

  // Ensure the stream is selected.
  if (SUCCEEDED(hr)) then
    begin
      hr := m_pReader.SetStreamSelection(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                         TRUE);

    end;

  if (SUCCEEDED(hr)) then
    begin
      hr := GetVideoFormat(m_format);
    end;

  SafeRelease(pType);
  Result := hr;
end;

//-------------------------------------------------------------------
// GetVideoFormat
//
// Gets format information for the video stream.
//
// iStream: Stream index.
// pFormat: Receives the format information.
//-------------------------------------------------------------------
function TThumbnailGenerator.GetVideoFormat(out pFormat: TFormatInfo): HRESULT;
label
  done;
var
  hr: HResult;
  _width, _height: UINT32;
  lStride: LONG;
  par: MFRatio;
  format: TFormatInfo;
  subtype: TGUID;
  pType: IMFMediaType;
  rcSrc: TRECT;

begin
  _width := 0;
  _height := 0;
  par.Numerator := 0;
  par.Denominator := 0;

  format := pFormat;
  subtype := GUID_NULL;
  pType := Nil;

  // Get the media type from the stream.
  hr := m_pReader.GetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_VIDEO_STREAM),
                                      pType);

  if (FAILED(hr)) then
    begin
      goto done;
    end;

  // Make sure it is a video format.
  hr := pType.GetGUID(MF_MT_SUBTYPE,
                      subtype);

  if FAILED(hr) or (subtype <> MFVideoFormat_RGB32) then
    begin
      hr := E_UNEXPECTED;
      goto done;
    end;

  // Get the width and height
  hr := MFGetAttributeSize(pType,
                           MF_MT_FRAME_SIZE,
                           _width,
                           _height);

  if (FAILED(hr)) then
    begin
      goto done;
    end;

  // Get the stride to find out if the bitmap is top-down or bottom-up.
  lStride := LONG(MFGetAttributeUINT32(pType,
                                       MF_MT_DEFAULT_STRIDE,
                                       1));

  format.bTopDown := (lStride > 0);

  // Get the pixel aspect ratio. (This value might not be set.)
  hr := MFGetAttributeRatio(pType,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            UINT32(par.Numerator),
                            UINT32(par.Denominator));

  if (SUCCEEDED(hr) AND (par.Denominator <> par.Numerator)) then
    begin
      //rcSrc. //:= { 0, 0, width, height };

      format.rcPicture := CorrectAspectRatio(rcSrc,
                                             par);
    end
  else
    begin
      // Either the PAR is not set (assume 1:1), or the PAR is set to 1:1.
      SetRect(format.rcPicture,
              0,
              0,
              _width,
              _height);
    end;

  format.imageWidthPels := _width;
  format.imageHeightPels := _height;
  pFormat := format;

done:
  SafeRelease(pType);
  Result := hr;
end;


// public methods
function TThumbnailGenerator.OpenFile(const wszFileName: LPCWSTR): HRESULT;
var
  hr: HResult;
  pAttributes: IMFAttributes;

begin
  pAttributes := Nil;

  SafeRelease(m_pReader);

  // Configure the source reader to perform video processing.
  //
  // This includes:
  //   - YUV to RGB-32
  //   - Software deinterlace

  hr := MFCreateAttributes(pAttributes,
                           1);

  if (SUCCEEDED(hr)) then
    begin
      hr := pAttributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                                  {TRUE}1);
    end;

  // Create the source reader from the URL.

  if (SUCCEEDED(hr)) then
    begin
      hr := MFCreateSourceReaderFromURL(wszFileName,
                                        pAttributes,
                                        m_pReader);
    end;

  if (SUCCEEDED(hr)) then
    begin
      // Attempt to find a video stream.
      hr := SelectVideoStream();
    end;

  Result := hr;
end;


function TThumbnailGenerator.GetDuration(out phnsDuration: LONGLONG): HRESULT;
var
  _var: MfPROPVARIANT;
  hr: HResult;

begin
  PropVariantInit(_var);

  if (m_pReader = Nil) then
    begin
      Result := MF_E_NOT_INITIALIZED;
      Exit;
    end;

  hr := m_pReader.GetPresentationAttribute(DWORD(MF_SOURCE_READER_MEDIASOURCE),
                                           MF_PD_DURATION,
                                           _var);

  if (SUCCEEDED(hr)) then
    begin
      assert(_var.vt = VT_UI8);  // debug
      phnsDuration := _var.hVal.QuadPart;
    end;

  {WinApi.ActiveX.}PropVariantClear(_var);
  Result := hr;
end;

//-------------------------------------------------------------------
// CanSeek: Queries whether the current video file is seekable.
//-------------------------------------------------------------------
function TThumbnailGenerator.CanSeek(out pbCanSeek: BOOL): HRESULT;
var
  hr: HResult;
  flags: ULONG;
  _var: MfPROPVARIANT;

begin

  flags := 0;
  {WinApi.ActiveX.}PropVariantInit(_var);

  if (m_pReader = Nil) then
    begin
      Result:= MF_E_NOT_INITIALIZED;
      Exit;
    end;

  pbCanSeek := FALSE;

  hr := m_pReader.GetPresentationAttribute(DWORD(MF_SOURCE_READER_MEDIASOURCE),
                                           MF_SOURCE_READER_MEDIASOURCE_CHARACTERISTICS,
                                           _var);

  if (SUCCEEDED(hr)) then
    begin
      hr := PropVariantToUInt32(_var,
                                flags);
    end;

  if (SUCCEEDED(hr)) then
    begin
      // If the source has slow seeking, we will treat it as
      // not supporting seeking.
      if ((flags AND ULONG(MFMEDIASOURCE_CAN_SEEK)) and (flags AND ULONG(MFMEDIASOURCE_HAS_SLOW_SEEK))) = 0 then
        begin
          pbCanSeek := TRUE;
        end;
    end;

  PropVariantClear(_var);
  Result := hr;
end;

//-------------------------------------------------------------------
// CreateBitmaps
//
// Creates an array of thumbnails from the video file.
//
// pRT:      Direct2D render target. Used to create the bitmaps.
// count:    Number of thumbnails to create.
// pSprites: An array of Sprite objects to hold the bitmaps.
//
// Note: The caller allocates the sprite objects.
//-------------------------------------------------------------------
function TThumbnailGenerator.CreateBitmaps(pRT: ID2D1RenderTarget;
                                           count: DWORD;
                                           pSprites: TSpritesArray): HRESULT;
var
  i: Integer;
  hr: HResult;
  bCanSeek: BOOL;
  hnsDuration: LONGLONG;
  hnsRangeStart: LONGLONG;
  hnsRangeEnd: LONGLONG;
  hnsIncrement: LONGLONG;
  hPos: LONGLONG;

begin
  bCanSeek := FALSE;

  hnsDuration := 0;
  hnsRangeStart := 0;
  hnsIncrement := 0;

  hr := CanSeek(bCanSeek);

  if (FAILED(hr)) then
   begin
     Result:= hr;
     Exit;
   end;

  if (bCanSeek) then
    begin
      hr := GetDuration(hnsDuration);

      if (FAILED(hr)) then
        begin
          Result:= hr;
          Exit;
        end;

      hnsRangeEnd := hnsDuration;

      // We have the file duration , so we'll take bitmaps from
      // several positions in the file. Occasionally, the first frame
      // in a video is black, so we don't start at time 0.

      hnsIncrement := (hnsRangeEnd - hnsRangeStart) div (count + 1);

    end;

  // Generate the bitmaps and invalidate the button controls so
  // they will be redrawn.
  for i := 0 to count-1 do
    begin
      hPos := hnsIncrement * (i + 1);

      hr := CreateBitmap(pRT,
                         hPos,
                         pSprites[i]);
    end;

  Result:= hr;
end;

// Helper

//-----------------------------------------------------------------------------
// CorrectAspectRatio
//
// Converts a rectangle from the source's pixel aspect ratio (PAR) to 1:1 PAR.
// Returns the corrected rectangle.
//
// For example, a 720 x 486 rect with a PAR of 9:10, when converted to 1x1 PAR,
// is stretched to 720 x 540.
//-----------------------------------------------------------------------------
function CorrectAspectRatio(const src: TRect;
                            const srcPAR: MFRatio): TRect;
var
  rc: TRECT;

begin
  // Start with a rectangle the same size as src, but offset to the origin (0,0).
 // rc := {0, 0, src.right - src.left, src.bottom - src.top};

  if ((srcPAR.Numerator <> 1) OR (srcPAR.Denominator <> 1)) then
    begin
      // Correct for the source's PAR.

      if (srcPAR.Numerator > srcPAR.Denominator) then
        begin
          // The source has "wide" pixels, so stretch the width.
          rc.right := MulDiv(rc.right,
                             srcPAR.Numerator,
                             srcPAR.Denominator);
        end
      else if (srcPAR.Numerator < srcPAR.Denominator) then
        begin
          // The source has "tall" pixels, so stretch the height.
          rc.bottom := MulDiv(rc.bottom,
                              srcPAR.Denominator,
                              srcPAR.Numerator);
        end;
        // else: PAR is 1:1, which is a no-op.
    end;

  Result := rc;
end;

end.

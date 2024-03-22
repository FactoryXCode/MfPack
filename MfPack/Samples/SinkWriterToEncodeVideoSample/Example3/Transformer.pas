// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Transformer.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.6
// Description:
//   Transforms video samples to uncompressed RGB32-samples with pixel-aspect
//   1x1, optionally changing the frame height (width by aspect), and the frame rate.
//   Designed to de-interlace interlaced videos, but not sure whether it really works.
//
// Planned addition:
//   TAudioTransformer doing the analogous for audio samples.
//
// Company: FactoryX
// Intiator(s): Renate Schaaf.
// Contributor(s): Renate Schaaf, Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code Sinkwriter and Transcode Examples.
//         Bitmaps2Video for Media Foundation.
//         https://github.com/rmesch/Bitmaps2Video-for-Media-Foundation
//
// Copyright © 2003-2024 Renate Schaaf
// Requires MFPack at https://github.com/FactoryXCode/MfPack
// Download the repository and add the folder "src" to your library path.
//
// The sinkwriter sample in this repository got me started on this project.
// Thanks for the great work!
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
// =============================================================================
unit Transformer;

interface

{$IFOPT O+ }
{$DEFINE O_PLUS }
{$O- }
{$ENDIF }

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {System}
  System.SysUtils,
  System.Types,
  System.Math,
  System.Classes,
  {VCL}
  VCL.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.Mfobjects,
  WinApi.MediaFoundationApi.CodecApi,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropVarUtil;

type

  TVideoInfo = record
    Codec: TGUID;
    CodecName: string;
    Duration: Int64;
    VideoWidth: DWord;
    VideoHeight: DWord;
    FrameRate: Single;
    PixelAspect: Single;
    InterlaceMode: DWord;
    InterlaceModeName: string;
    AudioStreamCount: DWord;
  end;

  eVideoFormatException = class(Exception);

  TVideoTransformer = class
  private
    pReader: IMFSourceReader;
    hrCoInit: HResult;
    fVideoInfo: TVideoInfo;
    pMediaTypeOut: IMFMediaType;
    fNewWidth, fNewHeight: DWord;
    fNewFrameRate: Single;
    fInputFile: string;
    fEndOfFile: Boolean;

  public
    constructor Create(const InputFile: string;
                       NewHeight: DWord;
                       NewFrameRate: Single);
    destructor Destroy(); override;

    procedure NextValidSampleToBitmap(const bm: TBitmap;
                                      out Timestamp: Int64;
                                      out Duration: Int64);

    procedure GetNextValidSample(out pSample: IMFSample;
                                 out Timestamp: Int64;
                                 out Duration: Int64);

    property NewVideoWidth: DWord read fNewWidth;
    property NewVideoHeight: DWord read fNewHeight;
    property NewFrameRate: Single read fNewFrameRate;
    property EndOfFile: Boolean read fEndOfFile;
    property VideoInfo: TVideoInfo read fVideoInfo;
  end;

  function GetVideoInfo(const VideoFileName: string): TVideoInfo;


implementation


function GetVideoInfo(const VideoFileName: string): TVideoInfo;
var
  GUID: TGUID;
  _var: TPropVariant;
  Numerator: DWord;
  Denumerator: DWord;
  pReader: IMFSourceReader;
  pMediaTypeIn: IMFMediaType;
  pMediaTypeOut: IMFMediaType;
  pPartialType: IMFMediaType;
  mfArea: MFVideoArea;
  attribs: IMFAttributes;
  hrCoInit: HResult;
  hrStartup: HResult;
  pb: PByte;
  FourCC: DWord;
  FourCCString: string[4];
  I: Integer;
  hr: HResult;
  AudioStreamNo: DWord;

const
  procname = 'function GetVideoInfo';

label
  done;

begin

  pReader := nil;
  hrStartup := E_FAIL;

  hrCoInit := CoInitializeEx(nil,
                             COINIT_APARTMENTTHREADED);
  hr := hrCoInit;
  if FAILED(hr) then
    goto done;

  hrStartup := MFStartup(MF_VERSION);
  hr := hrStartup;
  if FAILED(hr) then
    goto done;

  hr := MFCreateAttributes(attribs,
                           1);
  if FAILED(hr) then
    goto done;

  hr := attribs.SetUInt32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                          UInt32(True));
  if FAILED(hr) then
    goto done;

  // Create a sourcereader for the video file
  hr := MFCreateSourceReaderFromURL(PWideChar(VideoFileName),
                                    attribs,
                                    pReader);
  if FAILED(hr) then
    goto done;

  // Configure the sourcereader to decode to RGB32
  hr := pReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   0,
                                   pMediaTypeIn);
  if FAILED(hr) then
    goto done;

  hr := MFCreateMediaType(pPartialType);
  if FAILED(hr) then
    goto done;

  hr := pPartialType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video);
  if FAILED(hr) then
    goto done;

  hr := pPartialType.SetGUID(MF_MT_SUBTYPE,
                             MFVideoFormat_RGB32);
  if FAILED(hr) then
    goto done;

  hr := pReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    0,
                                    pPartialType);
  if FAILED(hr) then
    goto done;

  hr := pReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    pMediaTypeOut);
  if FAILED(hr) then
    goto done;

  hr := pMediaTypeIn.GetMajorType(GUID);
  if FAILED(hr) then
    goto done;

  hr := pMediaTypeIn.GetGUID(MF_MT_SUBTYPE,
                             GUID);
  if FAILED(hr) then
    goto done;

  Result.Codec := GUID;

  if (GUID = MFVideoFormat_MPEG2) then
      Result.CodecName := 'mpeg2'
  else
      begin
        FourCC := GUID.D1;
        pb := PByte(@FourCC);

        SetLength(FourCCString,
                  4);

        for I := 1 to 4 do
          begin
            FourCCString[I] := AnsiChar(pb^);
            Inc(pb);
          end;

        Result.CodecName := string(FourCCString);
      end;

  PropVariantInit(_var);

  hr := pReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                         MF_PD_DURATION,
                                         _var);
  if FAILED(hr) then
    goto done;

  hr := PropVariantToInt64(_var,
                           Result.Duration);
  if FAILED(hr) then
    goto done;

  // Result.Duration := _var.hVal.QuadPart; Makes no difference.
  PropVariantClear(_var);

  ZeroMemory(@mfArea,
             SizeOf(mfArea));

  // for some codecs, like HEVC, MF_MT_FRAME_SIZE does not
  // return the correct video size for display.
  // So we check first whether the correct size is
  // available via an MFVideoArea.
  hr := pMediaTypeIn.GetBlob(MF_MT_PAN_SCAN_APERTURE,
                             @mfArea,
                             SizeOf(MFVideoArea),
                             nil);

  if FAILED(hr) then
    hr := pMediaTypeIn.GetBlob(MF_MT_MINIMUM_DISPLAY_APERTURE,
                               @mfArea,
                               SizeOf(MFVideoArea),
                               nil);

  if SUCCEEDED(hr) then
    begin
      Result.VideoWidth := mfArea.Area.cx;
      Result.VideoHeight := mfArea.Area.cy;
    end
  else
    hr := MFGetAttributeSize(pMediaTypeIn,
                             MF_MT_FRAME_SIZE,
                             Result.VideoWidth,
                             Result.VideoHeight);
  if FAILED(hr) then
    goto done;

  hr := MFGetAttributeRatio(pMediaTypeIn,
                            MF_MT_FRAME_RATE,
                            Numerator,
                            Denumerator);
  if FAILED(hr) then
    goto done;

  Result.FrameRate := Numerator / Denumerator;

  // For some codecs it only reads the correct pixel aspect off the decoding media type.
  hr := MFGetAttributeRatio(pMediaTypeOut,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            Numerator,
                            Denumerator);

  if FAILED(hr) then // MF_E_PROPERTY_TYPE_NOT_ALLOWED
    hr := MFGetAttributeRatio(pMediaTypeIn,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              Numerator,
                              Denumerator);
  if FAILED(hr) then
    goto done;

  Result.PixelAspect := Numerator / Denumerator;

  hr := pMediaTypeIn.GetUInt32(MF_MT_INTERLACE_MODE,
                               Result.InterlaceMode);
  if FAILED(hr) then
    Result.InterlaceMode := 0;

  case Result.InterlaceMode of
    0: Result.InterlaceModeName := 'Unknown';
    2: Result.InterlaceModeName := 'Progressive';
    3: Result.InterlaceModeName := 'UpperFirst';
    4: Result.InterlaceModeName := 'LowerFirst';
    5: Result.InterlaceModeName := 'SingleUpper';
    6: Result.InterlaceModeName := 'SingleLower';
    7: Result.InterlaceModeName := 'InterlaceOrProgressive';
  else
    Result.InterlaceModeName := 'Unknown';
  end;

  // Get the nr. of audio-streams
  // Fails for .vob
  Result.AudioStreamCount := 0;
  AudioStreamNo := 0;

  repeat
    hr := pReader.GetNativeMediaType(AudioStreamNo,
                                     0,
                                     pMediaTypeIn);
    if FAILED(hr) then
      goto done;

    hr := pMediaTypeIn.GetMajorType(GUID);
    if FAILED(hr) then
      goto done;

    if (GUID = MFMediaType_Audio) then
      Inc(Result.AudioStreamCount);

    Inc(AudioStreamNo);
  until False;

done:

  if SUCCEEDED(hrStartup) then
    MFShutdown();

  if SUCCEEDED(hrCoInit) then
    CoUninitialize();

  if FAILED(hr) then
    begin
      raise Exception.CreateFmt('%s %s Result: %s. Change your choosen audio settings.',
                                [SysErrorMessage(hr) + #13,
                                 ProcName + #13,
                                 IntToHex(hr,
                                          8)]);
    end;
end;


{ TVideoTransformer }

constructor TVideoTransformer.Create(const InputFile: string;
                                     NewHeight: DWord;
                                     NewFrameRate: Single);
var
  hr: HResult;
  attribs: IMFAttributes;
  pPartialType: IMFMediaType;

const
  ProcName = 'TVideoTransformer.Create';

label
  done;

begin
  hr := S_OK;
  fInputFile := InputFile;
  fNewHeight := NewHeight;

  fVideoInfo := GetVideoInfo(fInputFile);
  if NewFrameRate = 0 then
    fNewFrameRate := fVideoInfo.FrameRate
  else
    fNewFrameRate := NewFrameRate;

  hrCoInit := CoInitializeEx(nil,
                             COINIT_APARTMENTTHREADED);

  if FAILED(hrCoInit) then
    goto done;

  MFStartup(MF_VERSION);

  hr := MFCreateAttributes(attribs,
                           1);
  if FAILED(hr) then
    goto done;

  // Enable the source-reader to make color-conversion, change video size, frame-rate and interlace-mode
  hr := attribs.SetUInt32(MF_SOURCE_READER_ENABLE_ADVANCED_VIDEO_PROCESSING,
                          UInt32(True));
  if FAILED(hr) then
    goto done;

    // The next causes problems for some video formats
    // CheckFail(attribs.SetUInt32
    // (MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, UInt32(True)));
    // Create a sourcereader for the video file
  hr := MFCreateSourceReaderFromURL(PWideChar(fInputFile),
                                    attribs,
                                    pReader);
  if FAILED(hr) then
    goto done;

    // Configure the sourcereader to decode to RGB32
  hr := MFCreateMediaType(pPartialType);
  if FAILED(hr) then
    goto done;

  hr := pPartialType.SetGUID(MF_MT_MAJOR_TYPE,
                             MFMediaType_Video);
  if FAILED(hr) then
    goto done;

  hr := pPartialType.SetGUID(MF_MT_SUBTYPE,
                             MFVideoFormat_RGB32);
  if FAILED(hr) then
    goto done;

  hr := pPartialType.SetUInt32(MF_MT_INTERLACE_MODE,
                               2);
  if FAILED(hr) then
    goto done;

  // 2 = progressive.
  hr := MFSetAttributeRatio(pPartialType,
                            MF_MT_FRAME_RATE,
                            Round(fNewFrameRate * 100),
                            100);
  if FAILED(hr) then
    goto done;

  fNewWidth := Round(fNewHeight * fVideoInfo.VideoWidth / fVideoInfo.VideoHeight * fVideoInfo.PixelAspect);

  hr := MFSetAttributeRatio(pPartialType,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            1,
                            1);
  if FAILED(hr) then
    goto done;

  hr := MFSetAttributeSize(pPartialType,
                           MF_MT_FRAME_SIZE,
                           fNewWidth,
                           fNewHeight);
  if FAILED(hr) then
    goto done;

  hr := pReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    0,
                                    pPartialType);
  if FAILED(hr) then
    goto done;

  hr := pReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    pMediaTypeOut);
  if FAILED(hr) then
    goto done;

  // Prevent memory leak.
  hr := pReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                   False);
  if FAILED(hr) then
    goto done;


  // Ensure the stream is selected.
  hr := pReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   True);
  fEndOfFile := False;

done:

  if FAILED(hr) then
    begin
      raise eVideoFormatException.Create('Video format of the input file is not supported.');
    end;
end;


destructor TVideoTransformer.Destroy();
begin
  MFShutdown();
  if SUCCEEDED(hrCoInit) then
    CoUninitialize();
  inherited;
end;


procedure TVideoTransformer.GetNextValidSample(out pSample: IMFSample;
                                               out Timestamp: Int64;
                                               out Duration: Int64);
var
  hr: HResult;
  pSampleLoc: IMFSample;
  Flags: DWord;

const
  ProcName = 'TVideoTransformer.GetNextValidSample';


begin

  pSample := nil;

  if fEndOfFile then
    Exit;

  repeat
    hr := pReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                             0,
                             nil,
                             @Flags,
                             nil,
                             @pSampleLoc);
    if FAILED(hr) then
      Break;

    if ((Flags and MF_SOURCE_READERF_STREAMTICK) <> 0) then
      Continue;

    // To be on the safe side we check all flags for which
    // further reading would not make any sense
    // and set EndOfFile to True
    if ((Flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) or
       ((Flags and MF_SOURCE_READERF_ERROR) <> 0) or
       ((Flags and MF_SOURCE_READERF_NEWSTREAM) <> 0) or
       ((Flags and MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED) <> 0) or
       ((Flags and MF_SOURCE_READERF_ALLEFFECTSREMOVED) <> 0) then
    begin
      fEndOfFile := True;
      Break;
    end;

    if (pSampleLoc <> nil) then
      begin
        SafeRelease(pSample);
        pSample := pSampleLoc;
        hr := pSample.GetSampleTime(Timestamp);

        if SUCCEEDED(hr) then
          hr := pSample.GetSampleDuration(Duration);

        // fVideoInfo.Duration can return the wrong value!
        // if Timestamp + Duration >= fVideoInfo.Duration then
        // fEndOfFile := True;
        if FAILED(hr) then
          begin
            fEndOfFile := True;
            pSample := nil;
          end;
        Break;
        Sleep(0);
      end;
    // Can it happen that we get an infinite loop here?
  until False;

  if FAILED(hr) then
    begin
      raise Exception.CreateFmt('%s Procedure: %s Result: %s.',
                                [SysErrorMessage(hr) + #13,
                                 ProcName + #13,
                                 IntToHex(hr,
                                          8)]);
    end;
end;


procedure TVideoTransformer.NextValidSampleToBitmap(const bm: TBitmap;
                                                    out Timestamp: Int64;
                                                    out Duration: Int64);
var
  hr: HResult;
  pSample: IMFSample;
  pBuffer: IMFMediaBuffer;
  Stride: Integer;
  pRow: PByte;
  pData: PByte;
  ImageSize: DWord;

const
  ProcName = 'TVideoTransformer.NextValidSampleToBitmap';

label
  done;

begin
  hr := S_OK;

  if fEndOfFile then
    Exit;

  GetNextValidSample(pSample,
                     Timestamp,
                     Duration);

  // an invalid sample is nil
  if Assigned(pSample) then
    begin
      hr := pSample.ConvertToContiguousBuffer(pBuffer);
      if FAILED(hr) then
        goto done;

      if Assigned(pBuffer) then
        begin
          bm.PixelFormat := pf32bit;
          bm.SetSize(fNewWidth,
                     fNewHeight);
          Stride := 4 * fNewWidth;
          pRow := bm.ScanLine[0];
          hr := pBuffer.Lock(pData,
                             nil,
                             @ImageSize);
          if FAILED(hr) then
            goto done;

          // Assert(ImageSize = 4 * fNewWidth * fNewHeight);
          hr := MFCopyImage(pRow { Destination buffer. },
                                -Stride { Destination stride. },
                                pData,
                                { First row in source. }
                                Stride { Source stride. },
                                Stride { Image width in bytes. },
                                fNewHeight { Image height in pixels. } );
          if FAILED(hr) then
            goto done;

          hr := pBuffer.Unlock();
          if FAILED(hr) then
            goto done;

          hr := pBuffer.SetCurrentLength(0);
          if FAILED(hr) then
            goto done;
          SafeRelease(pBuffer);
        end;
    sleep(0);
  end;

done:
  if FAILED(hr) then
    begin
      raise Exception.CreateFmt('%s Procedure: %s Result: %s.',
                                [SysErrorMessage(hr) + #13,
                                 ProcName + #13,
                                 IntToHex(hr,
                                          8)]);
    end;
end;

{$IFDEF O_PLUS}
{$O+}
{$UNDEF O_PLUS}
{$ENDIF}

end.


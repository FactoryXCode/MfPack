// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgAudioFormats.pas
// Kind: Pascal Unit
// Release date: 24-02-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Audio formats dialog.
//
// Company: FactoryX
// Intiator(s): Renate Schaaf.
// Contributor(s): Renate Schaaf, Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
unit ImageRenderer;

interface

// Some of the interface stuff fails with optimization turned on
// optimization will be turned on where it matters.
{$IFOPT O+ }
{$DEFINE O_PLUS }
{$O- }
{$ENDIF }

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {VCL}
  VCL.Graphics,
  {System}
  System.SysUtils,
  System.Types,
  System.Math,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.Mfobjects,
  WinApi.MediaFoundationApi.CodecApi,
  WinApi.ActiveX.PropIdl,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfMetLib,

  {$IFDEF DEBUG}
  WinApi.MediaFoundationApi.MfMediaTypeDebug,
  {$ENDIF}

  {MfPack}
  WinApi.MfPack.VideoStandardsCheat,
  {Application}
  {Parallel bitmap resampler}
  Common,
  Scale,
  Tools,
  {Transforms video-samples to the input-format of the sinkwriter}
  Transformer;

  {$DEFINE SAVE_DEBUG_REPORT}

type
TCodecID = (H264,
            H265
           );

  TCodecIDArray = array of TCodecID;

  TVideoFileExtensions = (vfeMp4);

const
  CodecNames: array [TCodecID] of string = ('H264 codec (Mpeg-4, AVC)',
                                            'H265 codec (HEVC)'
                                            );

  CodecShortNames: array [TCodecID] of string = ('H264',
                                                 'H265'
                                                 );

  CodecInfos: array [TCodecID] of string = ('Uses hardware-encoding, if supported.' + #13 +
                                            'If not, falls back to software-encoding.',

                                            'Uses hardware-encoding, if supported.' + #13 +
                                            'If not, falls back to software-encoding.' + #13 +
                                            'Better quality per bitrate than H264.' + #13 +
                                            'Requires Windows 10 or higher' + #13 +
                                            'Maximum frame size is 4k.');

  VideoContaineFormatExtensions: array [TVideoFileExtensions] of string = ('.mp4');
type
  // TZoom is a record (xcenter, ycenter, radius) defining a virtual zoom-rectangle
  // (xcenter-radius, ycenter-radius, xcenter+radius, ycenter+radius).
  // This rectangle should be a sub-rectangle of [0,1]x[0,1].
  // If multipied by the width/height of a target rectangle, it defines
  // an aspect-preserving sub-rectangle of the target.
  TZoom = record
    xCenter: Single;
    yCenter: Single;
    Radius: Single;

    function ToRectF(Width: Integer;
                     Height: Integer): TRectF; inline;
  end;

const
  _FullZoom: TZoom = (xCenter: 0.5;
                      yCenter: 0.5;
                      Radius: 0.5);

type
  // Can be a method of a class or free standing or anonymous
  TImageRendererProgressEvent = Reference to procedure(Sender: TObject;
                                                       FrameCount: Cardinal;
                                                       VideoTime: Int64;
                                                       var DoAbort: Boolean);

type

  TImageRenderer = class
  private
    // Fields needed to set up the MF-Sinkwriter and Sourcereader.
    fVideoWidth: DWord;
    fVideoHeight: DWord;
    fVideoFrameRate: Double;
    fFrameRateNumerator: UINT32;
    fFrameRateDenominator: UINT32;
    fQuality: DWord;

    fSampleDuration: DWord;
    fInputFormat: TGUID;
    pSinkWriter: IMFSinkWriter;
    fAudioSinkWriterStreamIndex: DWord; // Used by sinkwriter.
    pAudioSourceReader: IMFSourceReader;

    pAudioTypeIn: IMFMediaType;
    pAudioTypeOut: IMFMediaType;

    pSampleBuffer: IMFMediaBuffer;
    pSampleBufferAudio: IMFMediaBuffer;
    fBufferSizeVideo: DWord;
    fBufferSizeAudio: DWord;
    fstreamIndex: DWord;

    fAudioDuration, fAudioTime: Int64;
    fPicturePresentationTime: Int64;  // Default 4 seconds.
    fAudioDone: Boolean;
    fTargetFilename: string;

    // Audio
    fSrcAudioFileName: string;
    fDestAudioFormat: TMFAudioFormat;
    fSrcAudioFormat: TMFAudioFormat;
    //

    fCodec: TCodecID;

    { /fields needed to set up the MF-Sinkwriter }

    fWriteStart: Int64;
    fReadAhead: Int64;
    fInitialized: Boolean;
    fWriteAudio: Boolean;
    fAudioStart: int64;
    fBottomUp: Boolean;
    fVideoTime: Int64;
    fFrameCount: Int64;
    fThreadPool: TResamplingThreadPool;
    fFilter: TFilter;
    fTimingDebug: Boolean;
    fBrake: Integer;

    fAudioBytesPerSecond: DWord;
    fAudioBlockAlign: DWord;
    fAudioStreamIndex: DWord;

    fBmRGBA: TBitmap;
    fOnProgress: TImageRendererProgressEvent;
    // Resize/crop bm to input format for the encoder.
    procedure BitmapToRGBA(const bmSource: TBitmap;
                           const bmRGBA: TBitmap;
                           crop: Boolean);

    // Move the RGBA-pixels into an MF sample buffer
    procedure bmRGBAToSampleBuffer(const bm: TBitmap);

    // Encode one frame to video stream and the corresponding audio samples to audio stream
    function WriteOneFrame(TimeStamp: Int64;
                            Duration: Int64): HResult;

    procedure WriteAudio(TimeStamp: Int64);

    function InitAudioSourceReader(): HResult;

    function InitAudio(): HResult;

    function GetSilenceBufferSize(Duration: Int64): DWord;

    function GetAudioDuration(PCMSamples: DWord): int64;

  public
    constructor Create();
    destructor Destroy(); override;

    function GetAudioSourceFileAttributes(): HResult;

    /// <summary> Set up the video encoder for writing one output file. </summary>
    /// <param name="OutputFilename">Name of the output file. Must have extension .mp4. .wmv presently not supported. </param>
    /// <param name="Quality">Quality of the video encoding on a scale of 1 to 100</param>
    /// <param name="FrameRate">Frame rate in frames per second. Value >= 30 recommended. </param>
    /// <param name="VideoCodec">Video codec enum for encoding. Presently ciH264 or ciH265 </param>
    /// <param name="Resampler">Enum defining the quality of resizing. cfBox, cfBilinear, cfBicubic or cfLanczos</param>
    /// param name="PicturePresentationTime">Defines the duration of the image in the video.</param>
    /// <param name="AudioFileName">Optional audio or video file (.wav, .mp3, .aac, .mp4 etc.), audio stream encoded as AAC. Default ''</param>
    /// <param name="AudioBitRate"> in kb/sec (96, 128, 160, 192 accepted). Default 128 </param>
    /// <param name="AudioSampleRate"> 44100 or 48000. Default 44100 </param>
    /// <param name="AudioStart"> Delay of audio start in ms. Default 0 </param>
    function Initialize(const OutputFilename: string;
                        Quality: UInt32;
                        VideoCodec: TCodecID;
                        DestAudioFormat: TMFAudioFormat;
                        Resampler: TFilter = cfBicubic;
                        PicturePresentationTime: Int64 = 4000;  // Default 4000 milliseconds = 4 seconds.
                        AudioDuration: Int64 = 0;
                        const AudioFileName: string = '';
                        AudioStart: Int64 = 0): HResult;

    /// <summary> Finishes input, frees resources and closes the output file. </summary>
    procedure Finalize;

    /// <summary> Encodes a bitmap as the next video frame.
    /// Will be resized to maximal size fitting the video size (black BG),
    /// or (crop=true) cropped for maximal borderless size. </summary>
    procedure AddFrame(const bm: TBitmap;
                       crop: Boolean);

    /// <summary> Repeatedly encode the last frame for EffectTime ms </summary>
    procedure Freeze(aEffectDuration: Integer);

    /// <summary> Show a bitmap for ShowTime ms </summary>
    procedure AddStillImage(const aBitmap: TBitmap;
                            aImageDuration: Integer;
                            aCrop: Boolean);

    /// <summary> Make a crossfade transition from Sourcebm to Targetbm lasting EffectTime ms </summary>
    procedure CrossFade(const Sourcebm,
                        Targetbm: TBitmap;
                        EffectTime: Integer;
                        cropSource: Boolean;
                        cropTarget: Boolean);

    /// <summary> Make a crossfade transition from the last encoded frame to Targetbm lasting EffectTime ms </summary>
    procedure CrossFadeTo(const Targetbm: TBitmap;
                          EffectTime: Integer;
                          cropTarget: Boolean);

    /// <summary> Another transition as an example of how you can make more. Transition from Sourcebm to Targetbm </summary>
    procedure ZoomInOutTransition(const Sourcebm,
                                  Targetbm: TBitmap;
                                  ZoomSource: TZoom;
                                  ZoomTarget: TZoom;
                                  EffectTime: Integer;
                                  cropSource: Boolean;
                                  cropTarget: Boolean);

    /// <summary> Zoom-in-out transition from the last encoded frame to Targetbm lasting EffectTime ms </summary>
    procedure ZoomInOutTransitionTo(const Targetbm: TBitmap;
                                    ZoomSource: TZoom;
                                    ZoomTarget: TZoom;
                                    EffectTime: Integer;
                                    cropTarget: boolean);

    /// <summary> Insert a video clip (video stream only) into the stream of encoded bitmaps. </summary>
    /// <param name="VideoFile">Name of the file containing the video clip. Anything that Windows can decode should be supported. </param>
    /// <param name="TransitionTime">Optionally does a crossfade transition from the last encoded frame to the first video frame lasting TransitionTime ms. Default 0 </param>
    procedure AddVideo(const VideoFile: string;
                       TransitionTime: Integer = 0;
                       crop: Boolean = False);

    /// <summary>Videotime so far in ms.</summary>
    property VideoTime: Int64 read fVideoTime;

    /// <summary>Count of frames added so far.</summary>
    property FrameCount: Int64 read fFrameCount;

    /// <summary>The filename of the output video as entered in Initialize.</summary>
    property TargetFilename: string read fTargetFilename;

    /// <summary>The last encoded frame returned as a TBitmap.</summary>
    property LastFrame: TBitmap read fbmRGBA;

    /// <summary>If true, timestamp in sec will be displayed on the frames. A rough check for a uniform timing of frames.
    /// Timing could be very irregular at the beginning of development with high frame rates and large video sizes.
    /// I had to artificially slow down the generation of some frames to (hopefully) fix it,
    /// and read ahead in the audio file.
    /// See Freeze and WriteAudio.</summary>
    property TimingDebug: Boolean read fTimingDebug
                                  write fTimingDebug;

    // Event which fires every 30 frames. Use to indicate progress or abort encoding.
    property OnProgress: TImageRendererProgressEvent read fOnProgress
                                                     write fOnProgress;
  end;


  // List of codecs supported for encoding a file with the given extension.
  function GetSupportedCodecs(const FileExt: string): TCodecIDArray;
  function IsCodecSupported(const FileExt: string;
                            Codec: TCodecID): Boolean;

  // Translation of our codec-enumeration to MF-constants.
  function GetEncodingFormat(Id: TCodecID): TGUID;

  // Inline methods.
  function StartSlowEndSlow(t: Double): Double; inline;
  function StartFastEndSlow(t: Double): Double; inline;
  function Interpolate(Z1: TZoom;
                       Z2: TZoom;
                       t: Double): TZoom; inline;

  function IsSupportedAvgBytesPerSec(const codec: TGuid;
                                     avgbytesPerSec: UInt32): Boolean;


implementation

const
  // .wmv requires bottom-up order of input to the sample buffer
  // ... or is it the other way round? Anyway, the code works.
  BottomUp: array [TCodecID] of Boolean = (False,
                                           False);

// Record to divide up the work of a loop into threads.
type
  TParallelizer = record
    // Array of loopbounds for each thread.
    imin: TIntArray;
    imax: TIntArray;
    // InputCount: length of the loop
    procedure Init(ThreadCount: Integer;
                   InputCount: Integer);
  end;



procedure TParallelizer.Init(ThreadCount: Integer;
                             InputCount: Integer);
var
  chunk: Integer;
  Index: Integer;

begin
  SetLength(imin,
            ThreadCount);

  SetLength(imax,
            ThreadCount);

  chunk := InputCount div ThreadCount;

  for Index := 0 to ThreadCount - 1 do
    begin
      imin[Index] := Index * chunk;
      if (Index < ThreadCount - 1) then
        imax[Index] := (Index + 1) * chunk - 1
      else
        imax[Index] := InputCount - 1;
    end;
end;


function TImageRenderer.GetSilenceBufferSize(Duration: Int64): DWord;
begin
  Result := Trunc(fAudioBytesPerSecond / 1000 * Duration / 10000);
  Result := fAudioBlockAlign * (Result div fAudioBlockAlign);
end;


function TImageRenderer.GetAudioDuration(PCMSamples: DWord): Int64;
begin
  Result := Trunc(PCMSamples / fDestAudioFormat.unSamplesPerSec * 1000 * 10000);
end;


function TImageRenderer.Initialize(const OutputFilename: string;
                                   Quality: UInt32;
                                   VideoCodec: TCodecID;
                                   DestAudioFormat: TMFAudioFormat;
                                   Resampler: TFilter = cfBicubic;
                                   PicturePresentationTime: Int64 = 4000;  // Default 4000 milliseconds = 4 seconds.
                                   AudioDuration: Int64 = 0;
                                   const AudioFileName: string = '';
                                   AudioStart: Int64 = 0): HResult;
var
  hr: HResult;
  pMediaTypeOut: IMFMediaType;
  pMediaTypeIn: IMFMediaType;
  dwVideoStreamIndex: DWORD;
  pContainerAttributes: IMFAttributes;
  pExt: string;
  pStride: DWord;

label
  done;

begin

  // Do some checks..
  if fInitialized then
    begin
      raise Exception.Create('The bitmap-encoder must be finalized before re-initializing.');
      goto done;
    end;

  pExt := ExtractFileExt(OutputFilename);

  if not IsCodecSupported(pExt,
                          VideoCodec) then
    begin
      raise Exception.CreateFmt('Codec %s is not supported for file type %s', [ CodecShortNames[VideoCodec], pExt]);
      goto done;
    end;

  // Set initial parameters
  fInitialized := False;
  fVideoWidth := FVideoStandardsCheat.SelectedResolution.iWidth;
  fVideoHeight := FVideoStandardsCheat.SelectedResolution.iHeight;
  fVideoFrameRate := FVideoStandardsCheat.SelectedFrameRate.FrameRate;
  fFrameRateNumerator := FVideoStandardsCheat.SelectedFrameRate.Numerator;
  fFrameRateDenominator := FVideoStandardsCheat.SelectedFrameRate.Denominator;

  fSrcAudioFileName := AudioFileName;
  fTargetFileName := OutputFilename;

  fBrake := Max(Trunc(4800 / fVideoHeight),
                1);
  fQuality := Quality;
  fFilter := Resampler;
  fCodec := VideoCodec;

  // Calculate the average time/frame
  // Time is measured in units of 100 nanoseconds. 1 sec = 1000 * 10000 time-units
  fSampleDuration := Trunc(1000 * 10000 / fVideoFrameRate);
  fAudioStart := AudioStart * 10000;
  fAudioDuration := AudioDuration;
  fPicturePresentationTime := PicturePresentationTime;

  fInputFormat := MFVideoFormat_RGB32;
  fBottomUp := BottomUp[VideoCodec];
  fWriteStart := 0;
  fFrameCount := 0;
  fAudioSinkWriterStreamIndex := 0;
  pStride := 4 * fVideoWidth;  // 4 is the Size of a DWord or rgbquad in bytes.

  // Setup the container.
  hr := MFCreateAttributes(pContainerAttributes,
                           3);
  if FAILED(hr) then
    goto done;

  hr := pContainerAttributes.SetUINT32(MF_TRANSCODE_ADJUST_PROFILE,
                                       DWORD(MF_TRANSCODE_ADJUST_PROFILE_USE_SOURCE_ATTRIBUTES {MF_TRANSCODE_ADJUST_PROFILE_DEFAULT}));
  if FAILED(hr) then
    goto done;

  // Setting MF_SINK_WRITER_DISABLE_THROTTLING to True will affect timing,
  // especially when using the H265 codec.
  // Remarks:
  // By default, the sink writer's IMFSinkWriter.WriteSample method limits the data rate by
  // blocking the calling thread.
  // This prevents the application from delivering samples too quickly.
  // To disable this behavior, set the MF_SINK_WRITER_DISABLE_THROTTLING attribute
  // to TRUE when you create the sink writer.
  hr := pContainerAttributes.SetUInt32(MF_SINK_WRITER_DISABLE_THROTTLING,
                                       UInt32(0));
  if FAILED(hr) then
    goto done;

  if (ExtractFileExt(TargetFilename) = VideoContaineFormatExtensions[vfeMp4]) then
    hr := pContainerAttributes.SetGUID(MF_TRANSCODE_CONTAINERTYPE,
                                       MFTranscodeContainerType_MPEG4)
  else
    hr := ERROR_INVALID_PARAMETER;

  if FAILED(hr) then
    goto done;

  hr := MFCreateSinkWriterFromURL(StrToPWideChar(TargetFilename),
                                  nil,
                                  pContainerAttributes,
                                  pSinkWriter);

  // Set the output media type.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaType(pMediaTypeOut);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetGUID(MF_MT_SUBTYPE,
                                GetEncodingFormat(VideoCodec));

  // Has no effect on the bitrate with quality based encoding, it could have an effect
  // on the size of the leaky bucket buffer.
  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_AVG_BITRATE,
                                  fQuality * 60 * fVideoHeight);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_INTERLACE_MODE,
                                  MFVideoInterlace_Progressive);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeOut,
                             MF_MT_FRAME_SIZE,
                             fVideoWidth,
                             fVideoHeight);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_FRAME_RATE,
                              fFrameRateNumerator,
                              fFrameRateDenominator);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);

  if SUCCEEDED(hr) then
    hr := pSinkWriter.AddStream(pMediaTypeOut,
                                dwVideoStreamIndex);


  // Set the input media type.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaType(pMediaTypeIn);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetGUID(MF_MT_SUBTYPE,
                               MFVideoFormat_RGB32);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeIn,
                             MF_MT_FRAME_SIZE,
                             fVideoWidth,
                             fVideoHeight);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_FRAME_RATE,
                              fFrameRateNumerator,
                              fFrameRateDenominator);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);

  if SUCCEEDED(hr) then
    hr := pSinkWriter.SetInputMediaType(dwVideoStreamIndex,
                                        pMediaTypeIn,
                                        nil);

  // Audio =====================================================================
  if (fSrcAudioFileName <> '') then
    if FileExists(fSrcAudioFileName) then
      begin
        // Copy given audio format struct.
        CopyMemory(@fDestAudioFormat,
                   @DestAudioFormat,
                   SizeOf(TMFAudioFormat));

        hr := InitAudio();
        if FAILED(hr) then
          goto done;

        if SUCCEEDED(hr) then
          // Prevent memory leak if the the audiofile contains more than 1 stream.
          hr := pAudioSourceReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                                      False);
          if SUCCEEDED(hr) then
            // Ensure the stream is selected.
            hr := pAudioSourceReader.SetStreamSelection(fAudioStreamIndex,
                                                        True);
      end;

  fBmRGBA.PixelFormat := pf32bit;
  fBmRGBA.SetSize(fVideoWidth,
                  fVideoHeight);

  // Tell the sink writer to start accepting data.
  if SUCCEEDED(hr) then
    hr := pSinkWriter.BeginWriting();

  if FAILED(hr) then
    goto done;

  fBufferSizeVideo := pStride * fVideoHeight;
  hr := MFCreateMemoryBuffer(fBufferSizeVideo,
                             pSampleBuffer);
  if FAILED(hr) then
    goto done;

  fInitialized := True;

done:
  Result := hr;
end;


function TImageRenderer.InitAudioSourceReader(): HResult;
var
  hr: HResult;
  pSrcTypes: TArray<IMFMediaType>;
  dwCount: DWORD;

begin
  // Create a source-reader to read the audio file.
  hr := MFCreateSourceReaderFromURL(StrToPWideChar(fSrcAudioFileName),
                                    nil,
                                    pAudioSourceReader);
  if SUCCEEDED(hr) then
    // Prevent memory leak if the the audiofile contains more than 1 stream.
    hr := pAudioSourceReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                                False);

  if SUCCEEDED(hr) then
    // Ensure the audio stream is selected.
    hr := pAudioSourceReader.SetStreamSelection(fAudioStreamIndex,
                                                True);
  if SUCCEEDED(hr) then
    // Enumerate and check vsupported mediatype(s)
    hr := EnumerateMediaTypes(pAudioSourceReader,
                              fAudioStreamIndex,
                              pSrcTypes,
                              dwCount);

  if SUCCEEDED(hr) then
    hr := pAudioSourceReader.SetCurrentMediaType(fAudioStreamIndex,
                                                 0,
                                                 pSrcTypes[0]);

  if SUCCEEDED(hr) then
    // Read the full uncompressed input type from the reader.
    // pAudioTypeIn is the returned media type.
    hr := pAudioSourceReader.GetCurrentMediaType(fAudioStreamIndex,
                                                 pAudioTypeIn);

  {$IFDEF DEBUG}
    {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug.LogMediaType(pAudioTypeIn, 'pAudioTypeIn');
      FMediaTypeDebug.SafeAllDebugResultsToOneFile('pAudioTypeIn.txt');
    {$ENDIF}
  {$ENDIF}

  Result := hr;
end;


function TImageRenderer.InitAudio(): HResult;
var
  hr: HResult;
  pData: PByte;

const
  ProcName = 'TImageRenderer.InitAudio';

label
  done;

begin

  // To initialize the sink writer, perform the following steps.

    // 1. Call MFCreateSinkWriterFromURL to create a new instance of the sink writer.
    // 2. Create a media type that describes the encoded video. (output)
    // 3. Pass this media type to the IMFSinkWriter.AddStream method.
    // 4. Create a second media type that describes the uncompressed input. (input)
    // 5. Pass the uncompressed media type to the IMFSinkWriter.SetInputMediaType method.
    // 6. Call the IMFSinkWriter.BeginWriting method.
    // 7. The sink writer is now ready to accept input samples.

  fAudioTime := 0;
  fWriteAudio := True;
  fAudioDone := False;
  fAudioStreamIndex := MF_SOURCE_READER_FIRST_AUDIO_STREAM;

  hr := GetAudioSourceFileAttributes();
  if FAILED(hr) then
    goto done;

  // Create the encoded media type (AAC stereo with the specified sample- and bit-rates)
  // We set it up independent of the input type. In a future version we want to
  // add more than one audio file, so the input type should be allowed to change,
  // but not the output type.
  // So far it seems to work OK with one audio file.
  hr := MFCreateMediaType(pAudioTypeOut);
  if FAILED(hr) then
    goto done;

  hr := pAudioTypeOut.SetGUID(MF_MT_MAJOR_TYPE,
                              MFMediaType_Audio);
  if FAILED(hr) then
    goto done;

  // AAC or Dolby AC-3
  hr := pAudioTypeOut.SetGUID(MF_MT_SUBTYPE,
                              fDestAudioFormat.tgSubFormat);
  if FAILED(hr) then
    goto done;

  // Set the number of audio bits per sample. This must be 16 according to docs.
  hr := pAudioTypeOut.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                fDestAudioFormat.unBitsPerSample);
  if FAILED(hr) then
    goto done;

  // Set the number of audio samples per second.
  hr := pAudioTypeOut.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                fDestAudioFormat.unSamplesPerSec);
  if FAILED(hr) then
    goto done;

  // Set the number of audio channels. Hardwired to stereo, can be different from input format.
  hr := pAudioTypeOut.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                fDestAudioFormat.unChannels);
  if FAILED(hr) then
    goto done;


  // Set the block alignment of the samples. Hardwired to 1.
  hr := pAudioTypeOut.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                1);
  if FAILED(hr) then
    goto done;


  // When AAC is the selected format, we need to set the profilelevel and optionally the payload.
  if IsEqualGuid(fDestAudioFormat.tgSubFormat,
                 MFAudioFormat_AAC) then
    begin
      if not IsSupportedAvgBytesPerSec(MFAudioFormat_AAC,
                                       fDestAudioFormat.unAvgBytesPerSec) then
        begin
          // In case the value is not supported: Set to default.
          if (fDestAudioFormat.unChannels = 2) then
            fDestAudioFormat.unAvgBytesPerSec := 24000
          else if (fDestAudioFormat.unChannels = 1) then
            fDestAudioFormat.unAvgBytesPerSec := 96000;
        end;

      hr := pAudioTypeOut.SetUInt32(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                                    fDestAudioFormat.unAACProfileLevel);  // AAC Level 2 profile UInt32($29)
      if SUCCEEDED(hr) then
        hr := pAudioTypeOut.SetUInt32(MF_MT_AAC_PAYLOAD_TYPE,
                                      fDestAudioFormat.unAACPayload);  // Starting in Windows 8, the value can be 0 (raw AAC) or 1 (ADTS AAC).
                                                                       // Other values (2 and 3) are not supported!
     if FAILED(hr) then
       goto done;
    end;


  hr := pAudioTypeOut.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                fDestAudioFormat.unAvgBytesPerSec);

  if FAILED(hr) then
    goto done;


  hr := pAudioTypeOut.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                                UInt32(True));
  if FAILED(hr) then
    goto done;

  {$IFDEF DEBUG}
    {$IFDEF SAVE_DEBUG_REPORT}
      FMediaTypeDebug.LogMediaType(pAudioTypeOut, 'pAudioTypeOut');
      FMediaTypeDebug.SafeAllDebugResultsToOneFile('pAudioTypeOut.txt');
    {$ENDIF}
  {$ENDIF}

  hr := pSinkWriter.AddStream(pAudioTypeOut,
                              fAudioSinkWriterStreamIndex);
  if FAILED(hr) then
    goto done;


  // Initialize the sourcereader and get the input type.
  hr := InitAudioSourceReader();
    if SUCCEEDED(hr) then
      // Set the input audio type on the sinkwriter.
      pSinkWriter.SetInputMediaType(fAudioSinkWriterStreamIndex,
                                    pAudioTypeIn,
                                    nil);

  // Set up an audio buffer holding silence which we can add to the audio stream as necessary
  hr := pAudioTypeIn.GetUInt32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                               fAudioBytesPerSecond);
  if FAILED(hr) then
    goto done;

  hr := pAudioTypeIn.GetUInt32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                               fAudioBlockAlign);
  if FAILED(hr) then
    goto done;


  // Create an audio-buffer that holds silence.
  // The buffer should hold audio for the video frame time.
  fBufferSizeAudio := GetSilenceBufferSize(fSampleDuration);

  hr := MFCreateMemoryBuffer(fBufferSizeAudio,  // Amount of memory to allocate, in bytes.
                             pSampleBufferAudio);
  if FAILED(hr) then
    goto done;

  hr := pSampleBufferAudio.Lock(pData,
                                nil,
                                nil);
  if FAILED(hr) then
    goto done;

  FillChar(pData^,
           fBufferSizeAudio,
           0);

  // Prevent crack at beginnning of silence.
  PByteArray(pData)[2] := $06;

  // Update the current length.
  hr := pSampleBufferAudio.SetCurrentLength(fBufferSizeAudio);
  if FAILED(hr) then
    goto done;

  hr := pSampleBufferAudio.Unlock();
  if FAILED(hr) then
    goto done;

  // Set the amount of time we read ahead of the video-timestamp in the audio-file.
  fReadAhead := fAudioBlockAlign * GetAudioDuration(1024);

done:
  Result := hr;
end;


procedure TImageRenderer.Finalize();
begin

  if Assigned(pSinkWriter) then
    begin
      pSinkWriter.Finalize();
      SafeRelease(pSinkWriter);
    end;
  if Assigned(pAudioSourceReader) then
    SafeRelease(pAudioSourceReader);

  fInitialized := false;
end;


procedure TImageRenderer.AddVideo(const VideoFile: string;
                                  TransitionTime: Integer = 0;
                                  crop: boolean = False);
var
  VT: TVideoTransformer;
  bm: TBitmap;
  TimeStamp: Int64;
  Duration: Int64;
  VideoStart: Int64;

begin
  VT := TVideoTransformer.Create(VideoFile,
                                 fVideoHeight,
                                 fVideoFrameRate);
  try
    bm := TBitmap.Create;

    try
      VT.NextValidSampleToBitmap(bm,
                                 TimeStamp,
                                 Duration);

      if TransitionTime > 0 then
        CrossFadeTo(bm, TransitionTime, crop);
      VideoStart := fWriteStart;

      // fill gap at beginning of video stream
      if TimeStamp > 0 then
        AddStillImage(bm,
                      Trunc(TimeStamp / 10000),
                      crop);

      while (not VT.EndOfFile) and fInitialized do
        begin
          BitmapToRGBA(bm,
                       fBmRGBA,
                       crop);

          bmRGBAToSampleBuffer(fBmRGBA);

          WriteOneFrame(VideoStart + TimeStamp,
                        Duration);

          VT.NextValidSampleToBitmap(bm,
                                     TimeStamp,
                                     Duration);
      end;
    finally
      bm.Free;
    end;
  finally
    VT.Free;
  end;
end;


procedure TImageRenderer.AddFrame(const bm: TBitmap; crop: boolean);
begin
  BitmapToRGBA(bm,
               fBmRGBA,
               crop);
  bmRGBAToSampleBuffer(fBmRGBA);

  WriteOneFrame(fWriteStart,
                fSampleDuration);
end;


procedure TImageRenderer.AddStillImage(const aBitmap: TBitmap;
                                       aImageDuration: Integer;
                                       aCrop: Boolean);
var
  bmBuf: TBitmap;
  iStartTime: Int64;

begin
  iStartTime := fWriteStart;

  BitmapToRGBA(aBitmap,
               fBmRGBA,
               aCrop);

  if fTimingDebug then
    begin
      bmBuf := TBitmap.Create();
      try
        while (fWriteStart < (iStartTime + aImageDuration * 10000)) do
          begin
            bmBuf.Assign(fBmRGBA);
            bmRGBAToSampleBuffer(bmBuf);
            WriteOneFrame(fWriteStart,
                          fSampleDuration);
          end;
      finally
        bmBuf.Free;
      end;
    end
  else
    begin
      bmRGBAToSampleBuffer(fBmRGBA);
      Freeze(aImageDuration);
    end;
end;


// Resizes/crops bmSource to video size.
// We use a bitmap for the RGBA-output rather than a buffer, because we want to do
// bitmap operations like zooming on it.
procedure TImageRenderer.BitmapToRGBA(const bmSource: TBitmap;
                                      const bmRGBA: TBitmap;
                                      crop: Boolean);
var
  bmBack: TBitmap;
  bm: TBitmap;
  w: DWord;
  h: DWord;
  wSource: DWord;
  hSource: DWord;
  SourceRect: TRectF;
  bmWidth: DWord;
  bmHeight: DWord;

begin
  if (bmSource.Width = 0) or (bmSource.Height = 0) then
    raise Exception.Create('Bitmap has size 0');

  bmWidth := bmSource.Width;
  bmHeight := bmSource.Height;
  bm := TBitmap.Create;

  try
    bm.Assign(bmSource);
    bm.PixelFormat := pf32bit;

    if (bmWidth <> fVideoWidth) or (bmHeight <> fVideoHeight) then
      begin
        if (bmWidth / bmHeight) > (fVideoWidth / fVideoHeight) then
          begin
            if crop then
              begin
                h := fVideoHeight;
                w := fVideoWidth;
                hSource := bmHeight;
                wSource := Trunc((hSource * fVideoWidth) / fVideoHeight);
                SourceRect := RectF((bmWidth - wSource) div 2,
                                    0,
                                    (bmWidth + wSource) div 2,
                                    bm.Height);
              end
            else
              begin
                w := fVideoWidth;
                h := Trunc((fVideoWidth * bmHeight) / bmWidth);
                SourceRect := RectF(0,
                                    0,
                                    bmWidth,
                                    bmHeight);
             end;
          end
        else
          begin
            if crop then
              begin
                w := fVideoWidth;
                h := fVideoHeight;
                wSource := bm.Width;
                hSource := Trunc((wSource * fVideoHeight) / fVideoWidth);
                SourceRect := RectF(0,
                                    (bmHeight - hSource) div 2,
                                    bmWidth,
                                    (bmHeight + hSource) div 2);
              end
            else
              begin
                h := fVideoHeight;
                w := Trunc(fVideoHeight * bmWidth / bmHeight);
                SourceRect := FloatRect(0,
                                        0,
                                        bmWidth,
                                        bmHeight);
              end;
          end;

      bmBack := TBitmap.Create;

      try
        Scale.ZoomResampleParallelThreads(w,
                                          h,
                                          bm,
                                          bmBack,
                                          SourceRect,
                                          fFilter,
                                          0,
                                          amIgnore,
                                          @fThreadPool);

        if (w <> fVideoWidth) or (h <> fVideoHeight) then
          begin
            bmRGBA.PixelFormat := pf32bit;
            bmRGBA.SetSize(fVideoWidth, fVideoHeight);
            bmRGBA.Canvas.Lock;
            BitBlt(bmRGBA.Canvas.Handle,
                   0,
                   0,
                   fVideoWidth,
                   fVideoHeight,
                   0,
                   0,
                   0,
                   BLACKNESS);

            BitBlt(bmRGBA.Canvas.Handle,
                   (fVideoWidth - w) div 2,
                   (fVideoHeight - h) div 2,
                   w,
                   h,
                   bmBack.Canvas.Handle,
                   0,
                   0,
                   SRCCopy);
            bmRGBA.Canvas.Unlock;
          end
        else
          bmRGBA.Assign(bmBack);

      finally
        bmBack.Free;
      end;
    end
  else
     bmRGBA.Assign(bm);
  finally
    bm.Free;
  end;
end;


procedure TImageRenderer.bmRGBAToSampleBuffer(const bm: TBitmap);
var
  hr: HResult;
  pRow: PByte;
  StrideSource: Integer;
  StrideTarget: Integer;
  pData: PByte;
  time: string;

begin
  if fTimingDebug then
    begin
      time := IntToStr(fWriteStart div 10000000);
      bm.Canvas.Lock;
      bm.Canvas.Brush.Style := bsClear;
      bm.Canvas.Font.Color := clFuchsia;
      bm.Canvas.Font.Size := 32;
      bm.Canvas.TextOut(10,
                        10,
                        time);
      bm.Canvas.Unlock;
    end;

  if not fBottomUp then
    begin
      StrideSource := 4 * fVideoWidth;
      pRow := bm.ScanLine[fVideoHeight - 1];
    end
  else
    begin
      StrideSource := -4 * integer(fVideoWidth);
      pRow := bm.ScanLine[0];
    end;

  StrideTarget := 4 * fVideoWidth;
  hr := pSampleBuffer.Lock(pData,
                           nil,
                           nil);

  if SUCCEEDED(hr) then
    begin
      hr := MFCopyImage(pData,        // Destination buffer.
                        StrideTarget, // Destination stride.
                        pRow,         // First row in source image.
                        StrideSource, // Source stride.
                        StrideTarget, // Image width in bytes.         f
                        fVideoHeight  // Image height in pixels.
                       );

    if Assigned(pSampleBuffer) then
      pSampleBuffer.Unlock();

    if SUCCEEDED(hr) then
      // Set the data length of the buffer.
      hr := pSampleBuffer.SetCurrentLength(fBufferSizeVideo);
  end;

  if not SUCCEEDED(hr) then
    raise Exception.CreateFmt('TImageRenderer.bmRGBAToSampleBuffer failed with result %s. ',
                              [IntToHex(hr, 8) + #13,
                              SysErrorMessage(hr)]);

end;


constructor TImageRenderer.Create();
begin
  inherited Create();
  // Leave enough processors for the encoding threads.
  fThreadPool.Initialize(Min(16,
                             TThread.ProcessorCount div 2),
                         tpNormal);

  fBmRGBA := TBitmap.Create;
  {$IFDEF DEBUG}
    FMediaTypeDebug := TMediaTypeDebug.Create();
  {$ENDIF}

end;


destructor TImageRenderer.Destroy();
begin
  if fInitialized then
    Finalize;
  fThreadPool.Finalize;

  if Assigned(fBmRGBA) then
    FreeAndNil(fBmRGBA);

  {$IFDEF DEBUG}
  if Assigned(FMediaTypeDebug) then
    FMediaTypeDebug.Free();
  {$ENDIF}
  inherited;
end;


{$IFOPT O- }
{$DEFINE O_MINUS }
{$O+ }
{$ENDIF }
{$IFOPT Q+}
{$DEFINE Q_PLUS}
{$Q-}
{$ENDIF}


function GetCrossFadeProc(CF: TParallelizer;
                          Index: Integer;
                          alpha: Byte;
                          pOldStart: PByte;
                          pNewStart: PByte;
                          pTweenStart: PByte): TProc;
begin
  result := procedure
    var
      pold: PByte;
      pnew: PByte;
      pf: PByte;
      i: Integer;
      i1: Integer;
      i2: Integer;

    begin
      i1 := CF.imin[Index];
      i2 := CF.imax[Index];

      pold := pOldStart;
      pnew := pNewStart;
      pf := pTweenStart;

      Inc(pold,
          i1);
      Inc(pnew,
          i1);
      Inc(pf,
          i1);

      for i := i1 to i2 do
      begin
        pf^ := (alpha * (pnew^ - pold^)) div 256 + pold^;
        Inc(pf);
        Inc(pnew);
        Inc(pold);
      end;
    end;
end;
{$IFDEF O_MINUS}
{$O-}
{$UNDEF O_MINUS}
{$ENDIF}
{$IFDEF Q_PLUS}
{$Q+}
{$UNDEF Q_PLUS}
{$ENDIF}


function TImageRenderer.GetAudioSourceFileAttributes(): HResult;
var
  hr: HResult;

begin

  fSrcAudioFormat.Reset();

  // Create a mediasource by given URL.
  hr := CreateObjectFromUrl(StrToPWideChar(fSrcAudioFileName),
                            fSrcAudioFormat.mfSource);

  if SUCCEEDED(hr) then
    hr := GetAudioFormat(fSrcAudioFormat);

  Result := hr;
end;


procedure TImageRenderer.CrossFade(const Sourcebm: TBitmap;
                                   const Targetbm: TBitmap;
                                   EffectTime: Integer;
                                   cropSource: Boolean;
                                   cropTarget: Boolean);
var
  DurMs: Integer;

begin
  AddFrame(Sourcebm,
           cropSource);
  DurMs := Trunc(1 / 10000 * fSampleDuration);
  CrossFadeTo(Targetbm,
              EffectTime - DurMs,
              cropTarget);
end;


procedure TImageRenderer.CrossFadeTo(const Targetbm: TBitmap;
                                     EffectTime: Integer;
                                     cropTarget: Boolean);
var
  StartTime: Int64;
  EndTime: Int64;
  alpha: Byte;
  fact: Double;
  CF: TParallelizer;
  Index: Integer;
  bmOld: TBitmap;
  bmNew: TBitmap;
  bmTween: TBitmap;
  pOldStart: PByte;
  pNewStart: PByte;
  pTweenStart: PByte;

begin
  bmOld := TBitmap.Create;
  bmNew := TBitmap.Create;
  bmTween := TBitmap.Create;

  try
    bmOld.Assign(fBmRGBA);

    BitmapToRGBA(Targetbm,
                 bmNew,
                 cropTarget);

    bmTween.PixelFormat := pf32bit;
    bmTween.SetSize(fVideoWidth, fVideoHeight);
    pOldStart := bmOld.ScanLine[fVideoHeight - 1];
    pNewStart := bmNew.ScanLine[fVideoHeight - 1];
    pTweenStart := bmTween.ScanLine[fVideoHeight - 1];

    CF.Init(fThreadPool.ThreadCount,
            4 * fVideoWidth * fVideoHeight);

    StartTime := fWriteStart;
    EndTime := StartTime + EffectTime * 10000;
    fact := 255 / 10000 / EffectTime;

    while ((EndTime - fWriteStart)) > 0 do
      begin
        alpha := Trunc((fact * (fWriteStart - StartTime)));
        for Index := 0 to fThreadPool.ThreadCount - 1 do
          fThreadPool.ResamplingThreads[Index].RunAnonProc(GetCrossFadeProc(CF,
                                                                            Index,
                                                                            alpha,
                                                                            pOldStart,
                                                                            pNewStart,
                                                                            pTweenStart));
        for Index := 0 to fThreadPool.ThreadCount - 1 do
          fThreadPool.ResamplingThreads[Index].Done.WaitFor(INFINITE);

        bmRGBAToSampleBuffer(bmTween);
        WriteOneFrame(fWriteStart,
                      fSampleDuration);
      end;

  finally
    bmTween.Free;
    bmNew.Free;
    bmOld.Free;
  end;
end;


// TimeStamp = Video-timestamp
procedure TImageRenderer.WriteAudio(TimeStamp: int64);
var
  hr: HResult;
  ActualStreamIndex: DWord;
  flags: DWord;

  AudioTimestamp: Int64;
  AudioSampleDuration: Int64;
  pAudioSample: IMFSample;

const
  ProcName = 'TImageRenderer.WriteAudio';

begin
  hr := S_OK;
  // If audio is present write audio samples up to the Video-timestamp + fReadAhead.
  while ((fAudioTime + fAudioStart) < (TimeStamp + fReadAhead)) and (not fAudioDone) do

    begin
      // pull a sample out of the audio source reader
      hr := pAudioSourceReader.ReadSample(fAudioStreamIndex,  // Get a sample from audio stream.
                                          0,                  // No source reader controller flags.
                                          @ActualStreamIndex, // Get actual index of the stream.
                                          @flags,             // Get flags for this sample.
                                          @AudioTimestamp,    // Get the timestamp for this sample.
                                          @pAudioSample);     // Get the actual sample.
      if FAILED(hr) then
        Break;

      if ((flags and MF_SOURCE_READERF_STREAMTICK) <> 0) then
        begin
          pSinkWriter.SendStreamTick(fAudioSinkWriterStreamIndex,
                                     AudioTimestamp + fAudioStart);
          Continue;
       end;

     // To be on the safe side we check all flags for which
     // further reading would not make any sense and set fAudioDone to True.
     if ((flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) or
        ((flags and MF_SOURCE_READERF_ERROR) <> 0) or
        ((flags and MF_SOURCE_READERF_NEWSTREAM) <> 0) or
        ((flags and MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED) <> 0) or
        ((flags and MF_SOURCE_READERF_ALLEFFECTSREMOVED) <> 0) then
      begin
        fAudioDone := True;
      end;

     if (pAudioSample <> nil) then
       begin

         hr := pAudioSample.GetSampleDuration(@AudioSampleDuration);
         if FAILED(hr) then
           Break;

         hr := pAudioSample.SetSampleTime(AudioTimeStamp + fAudioStart);
         if FAILED(hr) then
           Break;

         hr := pAudioSample.SetSampleDuration(AudioSampleDuration);
         if FAILED(hr) then
           Break;

         // Send sample to sink-writer.
         hr := pSinkWriter.WriteSample(fAudioSinkWriterStreamIndex,
                                           pAudioSample);
         if FAILED(hr) then
           Break;

         // New end of sample time.
         fAudioTime := AudioTimestamp + AudioSampleDuration;
       end;

    if fAudioDone then
      hr := pSinkWriter.NotifyEndOfSegment(fAudioSinkWriterStreamIndex);
      if FAILED(hr) then
        Break;

    //The following should not be necessary in Delphi,
    //since interfaces are automatically released,
    //but it fixes a memory leak when reading .mkv-files.
    SafeRelease(pAudioSample);
  end;

  if FAILED(hr) then
    raise Exception.CreateFmt('Failure in %s'  + 'With result: $%s',
                              [ProcName + #13,
                               IntToHex(hr, 8) + #13,
                               SysErrorMessage(hr)]);
end;


function TImageRenderer.WriteOneFrame(TimeStamp: Int64;
                                       Duration: Int64): HResult;
var
  hr: HResult;
  pSample: IMFSample;
  pSampleAudio: IMFSample;
  i: DWord;
  imax: DWord;
  DoAbort: Boolean;

const
  ProcName = 'TImageRenderer.WriteOneFrame';

label
  done;

begin
  hr := S_OK;
  if not fInitialized then
    goto done;

  // The encoder collects a number of video and audio samples in a "leaky bucket" before
  // writing a chunk of the file. There need to be enough audio-samples in the bucket, so
  // we read ahead in the audio-file, otherwise video-frames might be dropped in an attempt
  // to "match to audio" (?).
  if fWriteAudio then
    begin

      if (TimeStamp < fAudioStart) then
        // write silence to the audio stream
        begin
          if (TimeStamp = 0) then
            imax := 2
          else
            imax := 0;

          for i := 0 to imax do
            begin
              hr := MFCreateSample(pSampleAudio);
              if FAILED(hr) then
                goto done;

              hr := pSampleAudio.AddBuffer(pSampleBufferAudio);
              if FAILED(hr) then
                goto done;

              // Write silence to the sinkwriter for 2 video frame durations ahead.
              hr := pSampleAudio.SetSampleTime(TimeStamp + (2 - imax + i) * Duration);
              if FAILED(hr) then
                goto done;

              hr := pSampleAudio.SetSampleDuration(Duration);
              if FAILED(hr) then
                goto done;

              hr := pSinkWriter.WriteSample(fAudioSinkWriterStreamIndex,
                                            pSampleAudio);
              if FAILED(hr) then
                goto done;

              hr := pSampleBufferAudio.SetCurrentLength(fBufferSizeAudio);
              if FAILED(hr) then
                goto done;

              SafeRelease(pSampleAudio);
            end;
        end
      else if (TimeStamp >= fAudioTime + fAudioStart - fReadAhead) and (not fAudioDone) then
        WriteAudio(TimeStamp);
  end;

  // Create a media sample and add the buffer to the sample.
  hr := MFCreateSample(pSample);
  if FAILED(hr) then
    goto done;

  hr := pSample.AddBuffer(pSampleBuffer);
  if FAILED(hr) then
    goto done;

  hr := pSample.SetSampleTime(TimeStamp);
  if FAILED(hr) then
    goto done;

  hr := pSample.SetSampleDuration(Duration);
  if FAILED(hr) then
    goto done;

  // Send the sample to the Sink Writer.
  hr := pSinkWriter.WriteSample(fstreamIndex, pSample);
  if FAILED(hr) then
    goto done;

  Inc(fFrameCount);
  // Timestamp for the next frame
  fWriteStart := TimeStamp + Duration;
  fVideoTime := fWriteStart div 10000;

  // Give the encoder-threads a chance to do their work.
  HandleThreadMessages(GetCurrentThread());

  if Assigned(fOnProgress) then
    if (fFrameCount mod 30 = 1) then
      begin
        DoAbort := False;
        fOnProgress(Self,
                    fFrameCount,
                    fVideoTime,
                    DoAbort);
        if DoAbort then
          Finalize;
    end;
done:
  Result := hr;
end;


procedure TImageRenderer.ZoomInOutTransition(const Sourcebm,
                                             Targetbm: TBitmap;
                                             ZoomSource: TZoom;
                                             ZoomTarget: TZoom;
                                             EffectTime: Integer;
                                             cropSource: Boolean;
                                             cropTarget: Boolean);
var
  DurMs: integer;
begin
  AddFrame(Sourcebm, cropSource);
  DurMs := Trunc(1 / 10000 * fSampleDuration);
  ZoomInOutTransitionTo(Targetbm,
                        ZoomSource,
                        ZoomTarget,
                        EffectTime - DurMs,
                        cropTarget);
end;


procedure TImageRenderer.ZoomInOutTransitionTo(const Targetbm: TBitmap;
                                               ZoomSource: TZoom;
                                               ZoomTarget: TZoom;
                                               EffectTime: Integer;
                                               cropTarget: Boolean);
var
  RGBASource: TBitmap;
  RGBATarget: TBitmap;
  RGBATweenSource: TBitmap;
  RGBATweenTarget: TBitmap;
  RGBATween: TBitmap;
  pSourceStart: PByte;
  pTargetStart: PByte;
  pTweenStart: PByte;
  ZIO: TParallelizer;
  StartTime: Int64;
  EndTime: Int64;
  fact: Double;
  alpha: Byte;
  t: Double;
  ZoomTweenSource: TRectF;
  ZoomTweenTarget: TRectF;
  Index: Integer;

begin
  // Create the bitmaps.
  RGBASource := TBitmap.Create;
  RGBATarget := TBitmap.Create;
  RGBATweenSource := TBitmap.Create;
  RGBATweenTarget := TBitmap.Create;
  RGBATween := TBitmap.Create;

  try
    RGBASource.Assign(fBmRGBA);

    BitmapToRGBA(Targetbm,
                 RGBATarget,
                 cropTarget);

    RGBATween.PixelFormat := pf32bit;
    RGBATween.SetSize(fVideoWidth,
                      fVideoHeight);
    ZIO.Init(fThreadPool.ThreadCount,
             4 * fVideoWidth * fVideoHeight);

    StartTime := fWriteStart;
    EndTime := StartTime + EffectTime * 10000;

    fact := 1 / 10000 / EffectTime;

    while (EndTime - fWriteStart > 0) do
      begin
        t := fact * (fWriteStart - StartTime);
        ZoomTweenSource := Interpolate(_FullZoom,
                                       ZoomSource,
                                       t).ToRectF(fVideoWidth,
                                                  fVideoHeight);

        ZoomTweenTarget := Interpolate(ZoomTarget,
                                       _FullZoom,
                                       t).ToRectF(fVideoWidth,
                                                  fVideoHeight);

        Scale.ZoomResampleParallelThreads(fVideoWidth,
                                          fVideoHeight,
                                          RGBASource,
                                          RGBATweenSource,
                                          ZoomTweenSource,
                                          cfBilinear,
                                          0,
                                          amIgnore,
                                          @fThreadPool);

        Scale.ZoomResampleParallelThreads(fVideoWidth,
                                          fVideoHeight,
                                          RGBATarget,
                                          RGBATweenTarget,
                                          ZoomTweenTarget,
                                          cfBilinear,
                                          0,
                                          amIgnore,
                                          @fThreadPool);

        pSourceStart := RGBATweenSource.ScanLine[fVideoHeight - 1];
        pTargetStart := RGBATweenTarget.ScanLine[fVideoHeight - 1];
        pTweenStart := RGBATween.ScanLine[fVideoHeight - 1];
        alpha := Trunc(255 * t);

        for Index := 0 to fThreadPool.ThreadCount - 1 do
          fThreadPool.ResamplingThreads[Index].RunAnonProc (GetCrossFadeProc(ZIO,
                                                                             Index,
                                                                             alpha,
                                                                             pSourceStart,
                                                                             pTargetStart,
                                                                             pTweenStart));

        for Index := 0 to fThreadPool.ThreadCount - 1 do
         fThreadPool.ResamplingThreads[Index].Done.WaitFor(INFINITE);

        bmRGBAToSampleBuffer(RGBATween);
        WriteOneFrame(fWriteStart, fSampleDuration);
      end;

  finally
    RGBASource.Free;
    RGBATarget.Free;
    RGBATweenSource.Free;
    RGBATweenTarget.Free;
    RGBATween.Free;
  end;
end;


procedure TImageRenderer.Freeze(aEffectDuration: Integer);
var
  StartTime: Int64;
  EndTime: Int64;

begin
  StartTime := fWriteStart;
  EndTime := StartTime + aEffectDuration * 10000;
  while (fWriteStart < EndTime) do
    begin
      WriteOneFrame(fWriteStart,
                    fSampleDuration);
      if (fFrameCount mod fBrake) = (fBrake - 1) then
        HandleThreadMessages(GetCurrentThread(), 1);

  end;
end;


{ TZoom }

function TZoom.ToRectF(Width: Integer;
                       Height: Integer): TRectF;
begin
  result.Left := Max((xCenter - Radius) * Width,
                     0);

  result.Right := Min((xCenter + Radius) * Width,
                      Width);

  result.Top := Max((yCenter - Radius) * Height,
                    0);

  result.Bottom := Min((yCenter + Radius) * Height,
                       Height);
end;


{ Tools }

function GetSupportedCodecs(const FileExt: string): TCodecIDArray;
begin
  SetLength(Result,
            0);

  if (FileExt = '.mp4') then
    begin
      SetLength(Result,
                2);
      Result[0] := H264;
      Result[1] := H265;
    end;
end;


function IsCodecSupported(const FileExt: string;
                          Codec: TCodecID): Boolean;
var
  ca: TCodecIDArray;
  i: Integer;

begin
  Result := False;
  ca := GetSupportedCodecs(FileExt);

  for i := 0 to Length(ca) - 1 do
    if (ca[i] = Codec) then
      begin
        Result := True;
        Break;
      end;
end;


function GetEncodingFormat(Id: TCodecID): TGUID;
begin
  case Id of
    H264:
      Result := MFVideoFormat_H264;
    H265:
      Result := MFVideoFormat_HEVC;
  end;
end;


function StartSlowEndSlow(t: Double): Double; inline;
begin
  if (t < 0.5) then
    Result := 2 * Sqr(t)
  else
    Result := 1 - 2 * Sqr(1 - t);
end;


function StartFastEndSlow(t: Double): Double; inline;
begin
  Result := 1 - Sqr(1 - t);
end;


function Interpolate(Z1: TZoom;
                     Z2: TZoom;
                     t: Double): TZoom; inline;
begin
  t := StartSlowEndSlow(t);
  Result.xCenter := t * (Z2.xCenter - Z1.xCenter) + Z1.xCenter;
  Result.yCenter := t * (Z2.yCenter - Z1.yCenter) + Z1.yCenter;
  Result.Radius := t * (Z2.Radius - Z1.Radius) + Z1.Radius;
end;


function IsSupportedAvgBytesPerSec(const codec: TGuid;
                                   avgbytesPerSec: UInt32): Boolean;
var
  i: Integer;
  rArr: array of UInt32;

const
  // The following values for AvgBytesPerSec are supported by Windows 10 and higher.
  aac_SupportedAvgBytesPerSecond : array[0..3] of UInt32 =
    (12000, 16000, 20000, 24000) ;

begin
  Result := False;
  // AAC
  if IsEqualGuid(codec,
                 MFAudioFormat_AAC) then
    begin
      SetLength(rArr,
                Length(aac_SupportedAvgBytesPerSecond));
      for i := 0 to Length(aac_SupportedAvgBytesPerSecond) do
        rArr[i] := aac_SupportedAvgBytesPerSecond[i];
    end;
  

  for i := 0 to Length(rArr) - 1 do
    if (avgbytesPerSec = rArr[i]) then
      begin
        Result := True;
        Break;
      end;
end;


initialization

{$IFDEF O_PLUS}
{$O+}
{$UNDEF O_PLUS}
{$ENDIF}


end.
// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Helpers.pas
// Kind: Pascal Unit
// Release date: 24-01-2020
// Language: ENU
//
// Revision Version: 3.1.1
// Description: Helpers for the Transcoding sample,
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: -
//
// =============================================================================
// Source: MfPack.Additional, MfpMetLib.pas
// Copyright (c) FactoryX. All rights reserved
// Source: Transcoder Example
// Copyright (c) Microsoft Corporation. All rights reserved
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
unit Helpers;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Unknwn,
  {System}
  System.SysUtils,
  {WinMM}
  WinApi.WinMM.MMReg,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.MediaFoundationApi.MfError;

type

  // See https://msdn.microsoft.com/en-us/library/windows/desktop/aa367377 and MfApi.pas
  TMediaTypes = (mtDefault,                   // Default stream.
                 mtAudio,                     // Audio stream.
                 mtVideo,                     // Video stream.
                 mtProtectedMedia,            // Protected media.
                 mtSAMI,                      // Synchronized Accessible Media Interchange (SAMI) captions (subtitling).
                 mtScript,                    // Script stream.
                 mtStillImage,                // Still image stream.
                 mtHTML,                      // HTML stream.
                 mtBinary,                    // Binary stream.
                 mtFileTransfer,              // A stream that contains data files.
                 mtStream,                    // Multiplexed stream or elementary stream.
                 mtMultiplexedFrames,         // Multiplexed frames stream.
                 mtSubTitle,                  // Subtitle stream.
                 mtPerception,                // Streams from a camera sensor or processing unit that reasons and understands raw video data and provides understanding of the environment or humans in it.
                 mtUnknown                    // Unknown stream type.
                );

  // Stream contents
  _StreamContents = record
    public
      procedure Init();
    public
      dwStreamIndex: DWORD;                 // The stream index (zero based !)
      dwStreamID: DWORD;                    // The stream identifier (see: https://msdn.microsoft.com/en-us/library/windows/desktop/ms703852)
      bSelected: BOOL;                      // The currently selected stream.
      idStreamMediaType: TMediaTypes;       // The mediatype (associated with the Major Type Guid
      idStreamMajorTypeGuid: TGuid;         // The majortype of the stream
      idStreamSubTypeGuid: TGuid;           // The subtype of the stream
      bCompressed: BOOL;                    // Compressed format.

      // Video
      video_FrameRateNumerator: UINT32;     // The upper 32 bits of the MF_MT_FRAME_RATE attribute value
      video_FrameRateDenominator: UINT32;   // The lower 32 bits of the MF_MT_FRAME_RATE attribute value
      // NOTE:
      //  To calculate the framerate in FPS use this formula: Double(video_FrameRateNominator / video_FrameRateDenominator)
      video_PixelAspectRatioNumerator: UINT32;   // The upper 32 bits of the MF_MT_PIXEL_ASPECT_RATIO attribute value
      video_PixelAspectRatioDenominator: UINT32; // The lower 32 bits of the MF_MT_PIXEL_ASPECT_RATIO attribute value
      // NOTE:
      //  To calculate the pixel aspect ratio use this formula: Double(video_PixelAspectRatioNumerator / video_PixelAspectRatioDenominator)
      video_FrameSizeHeigth: UINT32;        // Output frame heigth
      video_FrameSizeWidth: UINT32;         // Output frame width

      // Audio
      audio_lpStreamName: LPWSTR;           // The name of the stream (if stored in the sourcestream)
      audio_lpLangShortName: LPWSTR;        // Short language name (like 'en' for English, 'de' for German, 'fr' for French etc. stored in the stream
      audio_lpLangFullName: LPWSTR;         // Friendly language name (optional, caller needs to set these)
      audio_wsAudioDescr: WideString;       // Audio codec description.
      audio_iAudioChannels: UINT32;         // Number of audio channels.
      audio_iSamplesPerSec: UINT32;         // Audio Samples per second.
      audio_iBitsPerSample: UINT32;         // Audio bits per sample.
      audio_dwFormatTag: DWORD;             // FormatTag is the replacement for FOURCC
  end;
  TStreamContents = _StreamContents;
  TStreamContentsArray = array of TStreamContents;



  // Helper methods ////////////////////////////////////////////////////////////

  function CreateMediaSource(sURL: PWideChar;   // The URL of the file to open.
                             out ppMediaSource: IMFMediaSource): HResult;

  function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                                 out ppPD: IMFPresentationDescriptor): HResult;
  // Shows how to get the media type handler, enumerate the preferred media types, and set the media type.
  function GetMediaType(pStreamDesc: IMFStreamDescriptor; out tgMajorGuid: TGuid; out bIsCompressedFormat: BOOL): HRESULT;

  // Gets the pixel aspect ratio. This should be in almost all cases 1:1
  function GetPixelAspectRatio(pAttributes: IMFAttributes; out uiNumerator: UINT32; out uiDenominator: UINT32): HResult;

  // Helper function to get the frame rate from a video media type.
  function GetFrameRate(pAttributes: IMFAttributes;
                        out uiNumerator: UINT32;
                        out uiDenominator: UINT32): HResult;

  // Helper function to get the frame size from video attributes.
  function GetFrameSize(pAttributes: IMFAttributes; out frWidth: UINT32; out frHeight: UINT32): HResult; inline;

  // Gets the mediadesription of a stream
  function GetMediaDescription(pMajorGuid: TGuid; out mtMediaType: TMediaTypes): HRESULT;

  // Retrieves information of the streams from a source
  function GetStreamContents(pspd: IMFPresentationDescriptor;
                             mSource: IMFMediaSource;
                             var alsCont: TStreamContentsArray): HRESULT;
  // Retrieves a number of properties from an audio-subtype.
  function GetAudioSubType(mSource: IMFMediaSource;
                           out gSubType: TGUID;
                           out FormatTag: DWord;
                           out wsDescr: Widestring;
                           out cChannels: UINT32;
                           out samplesPerSec: UINT32;
                           out bitsPerSample: UINT32): HRESULT;

  // Alternative for ProcessMessages
  // Usage: HandleMessages( current running thread. );
  procedure HandleMessages(hThread: THandle);


implementation

///////////////////////////////////////////////////////////////////////
//  CreateMediaSource
//
//  Creates a media source from a URL.
///////////////////////////////////////////////////////////////////////

function CreateMediaSource(sURL: PWideChar;   // The URL of the file to open.
                           out ppMediaSource: IMFMediaSource): HResult;
var
  hr: HResult;
  ObjectType: MF_OBJECT_TYPE;
  pSourceResolver: IMFSourceResolver;
  pUnkSource: IUnknown;

begin

  if Not Assigned(sURL) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  ObjectType := MF_OBJECT_INVALID;

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);

  // Use the source resolver to create the media source.

  if SUCCEEDED(hr) then
    begin
      hr := pSourceResolver.CreateObjectFromURL(sURL,                      // URL of the source.
                                                MF_RESOLUTION_MEDIASOURCE, // Create a source object.
                                                Nil,                       // Optional property store.
                                                ObjectType,                // Receives the created object type.
                                                pUnkSource                 // Receives a pointer to the media source.
                                               );
    end;

  // Get the IMFMediaSource from the IUnknown pointer.
  if SUCCEEDED(hr) then
    hr := pUnkSource.QueryInterface(IID_IMFMediaSourceEx,
                                    ppMediaSource);

  Result := hr;
end;


////

// Given a topology, returns a pointer to the presentation descriptor.
function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                               out ppPD: IMFPresentationDescriptor): HResult;
var
  hr: HResult;
  pCollection: IMFCollection;
  pUnk: IUnknown;
  pNode: IMFTopologyNode;
  pPD: IMFPresentationDescriptor;

label
  done;

begin

  // Get the collection of source nodes from the topology.
  hr := pTopology.GetSourceNodeCollection(pCollection);
  if FAILED(hr) then
    goto done;

  // Any of the source nodes should have the PresentationDescriptor, so take the first
  // object in the collection.

  hr := pCollection.GetElement(0,
                               pUnk);
  if FAILED(hr) then
    goto done;

  hr := pUnk.QueryInterface(IID_IMFTopologyNode,
                            pNode);
  if FAILED(hr) then
    goto done;

  // Get the PD, which is stored as an attribute.
  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         pPD);
  if FAILED(hr) then
    goto done;

  ppPD := pPD;

done:
  Result := hr;
end;


function GetMediaType(pStreamDesc: IMFStreamDescriptor; out tgMajorGuid: TGuid; out bIsCompressedFormat: BOOL): HRESULT;
var
  hr: HRESULT;
  cTypes: DWORD;
  pHandler: IMFMediaTypeHandler;
  pMediaType: IMFMediaType;
  iType: DWORD;

begin

  cTypes := 0;
  tgMajorGuid := GUID_NULL;

  hr := pStreamDesc.GetMediaTypeHandler(pHandler);

  if SUCCEEDED(hr) then
    hr := pHandler.GetMediaTypeCount(cTypes);

  if SUCCEEDED(hr) then
    begin

      for iType := 0 to cTypes -1 do
        begin
          hr := pHandler.GetMediaTypeByIndex(iType,
                                            pMediaType);

          if FAILED(hr) then
            break;

          // Examine the media type.
          // here you have to examine the GetMajorType method
          // for major types that will give you information about video, audio etc.
          hr := pMediaType.GetMajorType(tgMajorGuid);

          // Check if it's a compressed format.
          if SUCCEEDED(hr) then
            hr := pMediaType.IsCompressedFormat(bIsCompressedFormat);
          pMediaType := Nil;
        end;
   end;
 Result := hr;
end;


function GetPixelAspectRatio(pAttributes: IMFAttributes; out uiNumerator: UINT32; out uiDenominator: UINT32): HResult;
begin
  Result := MFGetAttributeRatio(pAttributes, MF_MT_PIXEL_ASPECT_RATIO, uiNumerator, uiDenominator);
end;


function GetFrameRate(pAttributes: IMFAttributes; out uiNumerator: UINT32; out uiDenominator: UINT32): HResult;
begin
  Result := MFGetAttributeRatio(pAttributes, MF_MT_FRAME_RATE, uiNumerator, uiDenominator);
end;


function GetFrameSize(pAttributes: IMFAttributes; out frWidth: UINT32; out frHeight: UINT32): HResult; inline;
begin
  Result := MFGetAttributeSize(pAttributes, MF_MT_FRAME_SIZE, frWidth, frHeight);
end;


function GetMediaDescription(pMajorGuid: TGuid; out mtMediaType: TMediaTypes): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

  if isEqualGuid(pMajorGuid, MFMediaType_Default) then
    mtMediaType := mtDefault
  else if isEqualGuid(pMajorGuid, MFMediaType_Audio) then
    mtMediaType := mtAudio
  else if isEqualGuid(pMajorGuid, MFMediaType_Video) then
    mtMediaType := mtVideo
  else if isEqualGuid(pMajorGuid, MFMediaType_Protected) then
    mtMediaType := mtProtectedMedia
  else if isEqualGuid(pMajorGuid, MFMediaType_SAMI) then
    mtMediaType := mtSAMI
  else if isEqualGuid(pMajorGuid, MFMediaType_Script) then
    mtMediaType := mtScript
  else if isEqualGuid(pMajorGuid, MFMediaType_Image) then
    mtMediaType := mtStillImage
  else if isEqualGuid(pMajorGuid, MFMediaType_HTML) then
    mtMediaType := mtHTML
  else if isEqualGuid(pMajorGuid, MFMediaType_Binary) then
    mtMediaType := mtBinary
  else if isEqualGuid(pMajorGuid, MFMediaType_FileTransfer) then
    mtMediaType := mtFileTransfer
  else if isEqualGuid(pMajorGuid, MFMediaType_Stream) then
    mtMediaType := mtStream
  else if isEqualGuid(pMajorGuid, MFMediaType_MultiplexedFrames) then
    mtMediaType := mtMultiplexedFrames
  else if isEqualGuid(pMajorGuid, MFMediaType_Subtitle) then
    mtMediaType := mtSubTitle
  else if isEqualGuid(pMajorGuid, MFMediaType_Perception) then
    mtMediaType := mtPerception
  else
    begin
      mtMediaType := mtUnknown;
      hr := MF_E_INVALIDMEDIATYPE;
    end;
  Result := hr;
end;


// Retrieves information of the streams from a source
function GetStreamContents(pspd: IMFPresentationDescriptor;
                           mSource: IMFMediaSource;
                           var alsCont: TStreamContentsArray): HRESULT;
var
  hr: HRESULT;
  i: Integer;
  pSourceSD: IMFStreamDescriptor;
  pMediaTypeHandler: IMFMediaTypeHandler;
  pMediaType: IMFMediaType;
  pwszValue: LPWSTR;
  pcchLength,
  uiNumerator,
  uiDenominator,
  uiHeigth,
  uiWidth: UINT32;
  sdCount: DWORD;

begin

  pSourceSD := Nil;
  SetLength(alsCont, 0);
  sdCount := 0;
  pcchLength := 0;
  hr := S_OK;

try
try
  // Check if IMFPresentationDescriptor is initialized
  if assigned(pspd) then
    begin
      // Count streams
      hr := pspd.GetStreamDescriptorCount(sdCount);

      SetLength(alsCont, sdCount);

      for i := 0 to sdCount - 1 do
        begin
           // Initialize the record
           alsCont[i].Init();

           // Store the stream index
           alsCont[i].dwStreamIndex := i;

          // Get stream descriptor interface
          hr := pspd.GetStreamDescriptorByIndex(i,                    // Zero-based index of the stream.
                                                alsCont[i].bSelected, // TRUE if the stream is currently selected, FALSE if the stream is currently deselected.
                                                pSourceSD);           // Receives a pointer to the stream descriptor's IMFStreamDescriptor interface. The caller must release the interface.

          // Store the streamID
          if SUCCEEDED(hr) then
            pSourceSD.GetStreamIdentifier(alsCont[i].dwStreamId);

          // Get the media major type
          if SUCCEEDED(hr) then
            hr := GetMediaType(pSourceSD,
                               alsCont[i].idStreamMajorTypeGuid,
                               alsCont[i].bCompressed);


          // Figure out what media type we are dealing with
          if SUCCEEDED(hr) then
            hr := GetMediaDescription(alsCont[i].idStreamMajorTypeGuid,
                                      alsCont[i].idStreamMediaType);


          // If audio stream then try to get the language of this stream
          if SUCCEEDED(hr) and (alsCont[i].idStreamMediaType = mtAudio) then
            begin

              // Get the audio format type and qualities
              hr := GetAudioSubType(mSource,
                                    alsCont[i].idStreamSubTypeGuid,
                                    alsCont[i].audio_dwFormatTag,
                                    alsCont[i].audio_wsAudioDescr,
                                    alsCont[i].audio_iAudioChannels,
                                    alsCont[i].audio_iSamplesPerSec,
                                    alsCont[i].audio_iBitsPerSample);


              // Retrieves a wide-character string associated with a key (MF_SD_LANGUAGE).
              // This method allocates the memory for the string.
              // A returnvalue of -1072875802 / $C00D36E6
              // (The requested attribute was not found.) is returned when no language information was found.
              hr := pSourceSD.GetAllocatedString(MF_SD_LANGUAGE,
                                                 pwszValue,
                                                 pcchLength);


              if SUCCEEDED(hr) then
                alsCont[i].audio_lpLangShortName := pwszValue
              else
                begin
                  alsCont[i].audio_lpLangShortName := 'Not available';
                  hr := S_OK;
                end;
            end;

          pwszValue := Nil;
          pcchLength := 0;
          // Retrieves a wide-character string associated with a key (MF_SD_STREAM_NAME)
          // If a stream is not provided with a name the Hresult will be MF_E_ATTRIBUTENOTFOUND.
          hr := pSourceSD.GetAllocatedString(MF_SD_STREAM_NAME,
                                            pwszValue,
                                            pcchLength);
          if SUCCEEDED(hr) then
            alsCont[i].audio_lpStreamName := pwszValue
          else
            begin
              alsCont[i].audio_lpStreamName := 'Not available';
              hr := S_OK;
            end;

          // Note:
          // Set your initial preffered language somewhere in the caller.
          // hr := pspd.DeselectStream(dwDescriptorIndex);
          //
          // hr := pspd.SelectStream(iMySelectedLanguage);
          // finally set the new topology.

          // If video stream then try to get the properties of this stream
          if SUCCEEDED(hr) and (alsCont[i].idStreamMediaType = mtVideo) then
            begin
              hr := pSourceSD.GetMediaTypeHandler(pMediaTypeHandler);
              hr := pMediaTypeHandler.GetCurrentMediaType(pMediaType);

              // Get the video frame rate
              // To calculate the framerate in FPS : Single(uiNumerator / uiDenominator)
              hr := GetFrameRate(pMediaType,
                                 uiNumerator,
                                 uiDenominator);
              alsCont[i].video_FrameRateNumerator := uiNumerator;
              alsCont[i].video_FrameRateDenominator := uiDenominator;

              // Get the pixel aspect ratio
              // To calculate the pixel aspect ratio: Single(uiNumerator / uiDenominator)
              hr := GetPixelAspectRatio(pMediaType,
                                        uiNumerator,
                                        uiDenominator);
              alsCont[i].video_PixelAspectRatioNumerator := uiNumerator;
              alsCont[i].video_PixelAspectRatioDenominator := uiDenominator;

              // Get the video frame size
              hr := GetFrameSize(pMediaType,
                                 uiWidth,
                                 uiHeigth);
              alsCont[i].video_FrameSizeWidth := uiWidth;
              alsCont[i].video_FrameSizeHeigth := uiHeigth;
            end;
          pSourceSD := Nil;
        end;
    end;

except
  hr := E_POINTER;
end;
finally
  CoTaskMemFree(pwszValue);
  Result := hr;
end;
end;


function GetAudioSubType(mSource: IMFMediaSource;
                         out gSubType: TGUID;
                         out FormatTag: DWord;
                         out wsDescr: Widestring;
                         out cChannels: UINT32;
                         out samplesPerSec: UINT32;
                         out bitsPerSample: UINT32): HRESULT;

var
  hr: HResult;
  majortype: TGUID;
  subtype: TGUID;
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  cTypes: DWORD;
  i, j: DWORD;
  bSelected: BOOL;
  sGuid: string;
  sDescr: Widestring;

label done;

begin
  majortype := GUID_NULL;
  subtype := GUID_NULL;
  gSubType := subtype;
  cChannels := 0;
  samplesPerSec := 0;
  bitsPerSample := 0;
  cTypes := 0;
  i := 0;

  //
  repeat

  sGuid := '';
  sDescr := '';

  hr := mSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(i,
                                      bSelected,
                                      pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  hr := pHandler.GetMediaTypeCount(cTypes);
  if FAILED(hr) then
    goto done;

  // find the proper subtype
  for j := 0 to cTypes-1 do
    begin

      hr := pHandler.GetMediaTypeByIndex(j,
                                        pType);
      if FAILED(hr) then
        goto done;

      hr := pType.GetMajorType(majortype);
        if (FAILED(hr)) then
          goto done;

      if IsEqualGuid(majortype,
                     MFMediaType_Audio) then
        begin

          // Get the audio subtype. If not, skip.
          hr := pType.GetGUID(MF_MT_SUBTYPE,
                              subtype);
          if (FAILED(hr)) then
            goto done;

          // readable audiosubtype guid
          sGuid := GuidToString(subtype);

          // Get description by guid   (this is just a short list of most common audioformats)
          // You may want to extend the list with audiosubtypes.
          if IsEqualGuid(subtype, MFAudioFormat_PCM) then  //D1: WAVE_FORMAT_PCM;
            begin
              sDescr := 'Uncompressed PCM audio.';
              FormatTag := WAVE_FORMAT_PCM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Float) then //(D1: WAVE_FORMAT_IEEE_FLOAT;
            begin
              sDescr := 'Uncompressed IEEE floating-point audio.';
              FormatTag := WAVE_FORMAT_IEEE_FLOAT;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_DTS) then  //(D1: WAVE_FORMAT_DTS;
            begin
              sDescr := 'Microsoft DTS.';
              FormatTag := WAVE_FORMAT_DTS;
            end
          else if IsEqualGuid(subtype, MEDIASUBTYPE_DOLBY_TRUEHD) then // Not derived from an existing wFormatTag
            begin
              sDescr := 'Dolby Digital (AC-3) True High Definition.';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_AC3) then // Not derived from an existing wFormatTag
            begin
              sDescr := 'Dolby Digital (AC-3).';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MEDIASUBTYPE_DVM) then // Not derived from an existing wFormatTag
            begin
              sDescr := 'DVM AC-3 codec. Used when playing AVI files with Dolby Digital Audio.';
              FormatTag := WAVE_FORMAT_DVM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_AC3_SPDIF) then //(D1: WAVE_FORMAT_DOLBY_AC3_SPDIF;
            begin
              sDescr := 'Dolby AC-3 audio over Sony/Philips Digital Interface (S/PDIF).';
              FormatTag := WAVE_FORMAT_DOLBY_AC3_SPDIF;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_AC3_HDCP) then
            begin
              sDescr := 'Dolby AC-3. (HDCP)';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_DDPlus) then
            begin
              sDescr := 'Dolby Digital Plus.';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_DRM) then //(D1: WAVE_FORMAT_DRM;
            begin
              sDescr := 'Audio Digital Rights Management codec';
              FormatTag := WAVE_FORMAT_DRM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMAudioV8) then //(D1: WAVE_FORMAT_WMAUDIO2;
            begin
              sDescr := 'Windows Media Audio 8 codec, Windows Media Audio 9 codec, or Windows Media Audio 9.1 codec.';
              FormatTag := WAVE_FORMAT_WMAUDIO2;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMAudioV9) then //(D1: WAVE_FORMAT_WMAUDIO3;
            begin
              sDescr := 'Windows Media Audio 9 Professional codec or Windows Media Audio 9.1 Professional codec.';
              FormatTag := WAVE_FORMAT_WMAUDIO3;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMAudio_Lossless) then  //(D1: WAVE_FORMAT_WMAUDIO_LOSSLESS;
            begin
              sDescr := 'Windows Media Audio 9 Lossless codec or Windows Media Audio 9.1 codec.';
              FormatTag := WAVE_FORMAT_WMAUDIO_LOSSLESS;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMASPDIF) then //(D1: WAVE_FORMAT_WMASPDIF;
            begin
              sDescr := 'Windows Media Audio S/PDIF.';
              FormatTag := WAVE_FORMAT_WMASPDIF;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_MSP1) then  // (D1: WAVE_FORMAT_WMAVOICE9;
            begin
              sDescr := 'Windows Media Audio 9 Voice codec.';
              FormatTag := WAVE_FORMAT_WMAVOICE9;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_MP3) then  // (D1: WAVE_FORMAT_MPEGLAYER3;
            begin
              sDescr := 'MPEG Audio Layer-3 (MP3).';
              FormatTag := WAVE_FORMAT_MPEGLAYER3;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_MPEG) then // (D1: WAVE_FORMAT_MPEG;
            begin
              sDescr := 'MPEG-1 audio payload.';
              FormatTag := WAVE_FORMAT_MPEG;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AAC) then  // (D1: WAVE_FORMAT_MPEG_HEAAC;
            begin
              sDescr := 'Raw AAC or ADTS AAC.';
              FormatTag := WAVE_FORMAT_MPEG_HEAAC;
            end
          else if IsEqualGuid(subtype, MEDIASUBTYPE_RAW_AAC1) then  // (D1: WAVE_FORMAT_RAW_AAC1;
            begin
              sDescr := 'Raw AAC.';
              FormatTag := WAVE_FORMAT_MPEG_HEAAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_ADTS) then  // (D1: WAVE_FORMAT_MPEG_ADTS_AAC;
            begin
              sDescr := 'Mpeg ADTS (AAC).';
              FormatTag := WAVE_FORMAT_MPEG_ADTS_AAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AMR_NB) then  // (D1: WAVE_FORMAT_AMR_NB;
            begin
              sDescr := 'Adaptative Multi-Rate Wideband audio. (NB)';
              FormatTag := WAVE_FORMAT_AMR_NB;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AMR_WB) then  // (D1: WAVE_FORMAT_AMR_WB;
            begin
              sDescr := 'Adaptative Multi-Rate Wideband audio. (WB)';
              FormatTag := WAVE_FORMAT_AMR_WB;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AMR_WP) then  // (D1: WAVE_FORMAT_AMR_WP;
            begin
              sDescr := 'Adaptative Multi-Rate Wideband audio. (WP)';
              FormatTag := WAVE_FORMAT_AMR_WP;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_FLAC) then  // (D1: WAVE_FORMAT_FLAC;
            begin
              sDescr := 'Free Lossless Audio Codec.';
              FormatTag := WAVE_FORMAT_FLAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_ALAC) then  // (D1: WAVE_FORMAT_ALAC;
            begin
              sDescr := 'Apple Lossless Audio Codec.';
              FormatTag := WAVE_FORMAT_ALAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Opus) then  // (D1: WAVE_FORMAT_OPUS;
            begin
              sDescr := 'Opus audio codec.';
              FormatTag := WAVE_FORMAT_OPUS;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Vorbis) then
            begin
              sDescr := 'Vorbis audio codec.';
              FormatTag := $00006700; // See vorbis formats and FormatTag's in MMreg.pas
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Float_SpatialObjects) then
            begin
              sDescr := 'Uncompressed IEEE floating-point audio.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_LPCM) then
            begin
              sDescr := 'LPCM audio with headers for encapsulation in an MPEG2 bitstream.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_PCM_HDCP) then
            begin
              sDescr := 'Uncompressed PCM audio. (HDCP)';
              FormatTag := WAVE_FORMAT_PCM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AAC_HDCP) then
            begin
              sDescr := 'Raw AAC, HDCP AAC.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_ADTS_HDCP) then
            begin
              sDescr := 'Advanced Audio Coding (AAC) in Audio Data Transport Stream (ADTS) format.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Base_HDCP) then
            begin
              sDescr := 'Base HDCP (High-bandwidth Digital Content Protection).';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else
            begin
              sDescr := 'Unknown audio type';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end;

          gSubType := subtype;

          sGuid := ' ( GUID: ' + sGuid + ' )';
          wsDescr := sDescr + sGuid;


          // Get the sample rate and other information from the audio format.

          cChannels := MFGetAttributeUINT32(pType,
                                            MF_MT_AUDIO_NUM_CHANNELS,
                                            0);

          samplesPerSec := MFGetAttributeUINT32(pType,
                                                MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                0);

          bitsPerSample := MFGetAttributeUINT32(pType,
                                                MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                16);

          // Note: Some encoded audio formats do not contain a value for bits/sample.
          // In that case, use a default value of 16. Most codecs will accept this value.

          if (cChannels = 0) or (samplesPerSec = 0) then
            begin
              hr := MF_E_INVALIDTYPE;
              goto done;
            end;

        end; //if mediatype = audio
     end; //end if

      pPD := Nil;
      pSD := Nil;
      pHandler := Nil;
      pType := Nil;
      inc(i);

   until (i > cTypes);  // end repeat

done:
  Result := hr;
end;


procedure HandleMessages(hThread: THandle);
var
  Msg: TMsg;
  th: THandle;

begin
  if hThread = 0 then
    th := GetCurrentThread()
  else
    th := hThread;

  while (MsgWaitForMultipleObjects(1,
                                   th,
                                   False,
                                   INFINITE,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
      if Msg.Message = WM_QUIT then
        Exit;
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end;

// _StreamContents record
procedure _StreamContents.Init();
begin
  dwStreamIndex := 0;
  dwStreamID := 0;
  idStreamMediaType := mtUnknown;
  idStreamMajorTypeGuid := Guid_Null;
  idStreamSubTypeGuid := Guid_Null;
  bSelected := False;
  bCompressed := False;

  video_FrameRateNumerator := 0;
  video_FrameRateDenominator := 0;
  video_PixelAspectRatioNumerator := 0;
  video_PixelAspectRatioDenominator := 0;
  video_FrameSizeHeigth := 0;
  video_FrameSizeWidth := 0;

  audio_lpLangShortName := Nil;
  audio_lpLangFullName := Nil;
  audio_wsAudioDescr := '';
  audio_iAudioChannels := 0;
  audio_iSamplesPerSec := 0;
  audio_iBitsPerSample := 0;
  audio_dwFormatTag := 0;
end;

end.

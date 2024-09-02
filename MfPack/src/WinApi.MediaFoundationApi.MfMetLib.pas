// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMetLib.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Revision Version: 3.1.7
// Description: MfPack Methods Library.
//              This unit contains basic Media Foundation methods needed to play,
//              record, encode, decode, etc.
//              See: https://github.com/FactoryXCode/MfPack/wiki/MfPack-Methods-Library-Index
//
//
// Organisation: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues,
//                 (TopPlay)
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 27/07/2024 Tony                Added overloaded method ConfigureVideoEncoding
// -----------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
// Source: Parts and examples from learn.microsoft.com.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit WinApi.MediaFoundationApi.MfMetLib;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.Unknwn,
  WinApi.KsMedia,
  WinApi.Ks,
  WinApi.StrmIf,
  WinApi.UuIds,
  WinApi.AmVideo,
  WinApi.Dvdmedia,
  WinApi.ComBaseApi,
  WinApi.DevpKey,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.PropSys,
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX.PropVarUtil,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropSys,
  {$ENDIF}
  {System}
  System.Win.ComObj,
  System.Classes,
  System.SysUtils,
  System.Services.Dbt,
  {DirectX}
  WinApi.DirectX.DxVa2Api,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9,
  WinApi.MediaFoundationApi.MFTransform,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.WMCodecDsp,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfCaptureEngine,
  WinApi.DirectX.D3D9Types,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.MMDeviceApi;

  {$I 'WinApiTypes.inc'}

type

  // This record holds all audioformat parameters.
  TMFAudioFormat = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF} // Compatible with all Delphi versions.
    mfSource: IMFMediaSource; // MediaSource must be created before use!
    tgMajorFormat: TGUID;
    wcMajorFormat: LPWSTR; // readable guid of majorformat
    tgSubFormat: TGUID;
    wcSubFormat: LPWSTR; // readable guid of subformat
    dwFormatTag: DWord;  // FormatTag or FOURCC if present.
    wcFormatTag: LPWSTR; // Readable formattag or FOURCC
    wsDescr: LPWSTR;     // Description about the format or codec see: function GetAudioDescr
    wsGuid: LPWSTR;      // See: function GetAudioDescr
    unChannels: UINT32;
    unSamplesPerSec: UINT32; // Sample rate, in samples per second (Hertz).
                             // Common values for unSamplesPerSec are 8.0 kHz, 11.025 kHz, 22.05 kHz, and 44.1 kHz.
                             // To calculate unSamplesPerSec (samplerate) to kHz, use formula unSamplesPerSec / 1000.
    dblFloatSamplePerSec: Double; // Sample rate floatingpoint. Number of audio samples per second in an audio media type.

    unSamplesPerBlock: UINT32;
    unValidBitsPerSample: UINT32;
    unBitsPerSample: UINT32;
    unBlockAlignment: UINT32; // Note: For PCM audio formats,
                              //       the block alignment is equal to the number of audio channels multiplied by
                              //       the number of bytes per audio sample.
    unAvgBytesPerSec: UINT32; // Bytes per second or Bitrate.
                              // To calculate the bitrate to (kbps) use formula: (Average Bytes Per Sample * 8) / 1000.
    unChannelMask: UINT32;
    // AAC
    unAACPayload: UINT32;
    wsAACPayloadDescription: string;
    unAACProfileLevel: UINT32;
    wsAACProfileLevelDescription: string;
    // FLAC extra data
    unFlacMaxBlockSize: UINT32;
    public
      procedure Reset();
  end;
  // Array that holds audio data
  TMFAudioFormatArray = array of TMFAudioFormat;


  // Used by TDeviceProperties, holding capabillities of a video capture device.
  TVideoFormatInfo = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF} // Compatible with all Delphi versions.
  public
    FormatsIndex: Integer;     // The index of the native format found on a device.
    mfMediaType: IMFMediaType; // MediaType interface.

    // Major & Subtypes
    fMajorType: TGuid;
    wcMajorFormat: string; // Readable guid of majorformat.
    fSubType: TGuid;
    wcSubFormat: string;   // Readable guid of subformat.

    // FOURCC and codec description.
    unFormatTag: UINT32; // FormatTag (FOURCC).
    wcFormatTag: string; // Readable formattag.
    wsDescr: string;     // Description about the format or codec see: function GetVideoDescr.
    wsGuid: string;      // See: function GetVideoDescr.

    // Dimensions
    iVideoWidth: UINT32;
    iVideoHeight: UINT32;
    iBufferWidth: UINT32;
    iBufferHeight: UINT32;
    iStride: UINT32;    // Stride is positive for top-down images, and negative for bottom-up images.

    // Supported framerates
    fFrameRate: Float;
    iFrameRateNumerator: UINT32;
    iFrameRateDenominator: UINT32;
    iMaxFrameRate: UINT32;
    iMaxFrameRateDenominator: UINT32;
    iMinFrameRate: UINT32;
    iMinFrameRateDenominator: UINT32;

    // AspectRatio
    AspectRatioSupported: Boolean;
    AspectRatioNumerator: UINT32;
    AspectRatioDenominator: UINT32;

    // Is supported by Media Foundation
    bMFSupported: Boolean;
    public
      procedure Reset();
  end;

  // Array that holds retrieved capabillities records.
  TVideoFormatInfoArray = array of TVideoFormatInfo;


  // Used in arrays to hold capture device enum data.
  TDeviceProperties = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF}  // Compatible with all Delphi versions.
    riId: TGuid;             // Source type: video or audio capture devices.
    iCount: Integer;         // Number of devices of the same type and brand.
    iDeviceIndex: Integer;   // Zero based device index.
    lpFriendlyName: LPWSTR;  // Readable string from the system.
    lpDisplayName: LPWSTR;   // Displayname of the FriendlyName when doubles are found.
    lpSymbolicLink: LPWSTR;  // Device symlink.
    aVideoFormats: TVideoFormatInfoArray; // Video capabilities of the device supported by Media Foundation.
    aAudioFormats: TMFAudioFormatArray; // Audio capabilities of the device supported by Media Foundation.
    dwSupportedFormats: DWord; // Number of mediatype formats of the capturedevice supported by Media Foundation.
    dwNativeFormats: DWord;    // Number of native mediatype formats of the capturedevice.
    pActivate: IMFActivate;    // The activation object of the device.
    public
      procedure Reset();       // Resets the record to default.
  end;

  // Array that holds retrieved devices by name and/or index
  TDevicePropertiesArray = array of TDeviceProperties;

  // See https://learn.microsoft.com/en-us/windows/win32/medfound/media-type-guids and MfApi.pas
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

  // Audio and video stream contents.
  PStreamContents = ^TStreamContents;
  _StreamContents = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF}  // Compatible with all Delphi versions.
    dwStreamIndex: DWORD;                 // The stream index (zero based !)
    dwStreamID: DWORD;                    // The stream identifier (see: https://msdn.microsoft.com/en-us/library/windows/desktop/ms703852)
    bSelected: BOOL;                      // The currently selected stream.
    idStreamMediaType: TMediaTypes;       // The mediatype (associated with the Major Type Guid
    idStreamMajorTypeGuid: TGuid;         // The majortype of the stream
    idStreamSubTypeGuid: TGuid;           // The subtype of the stream
    bCompressed: BOOL;                    // Compressed format.

    // Video

    // NOTE:
    //  To calculate the framerate in FPS use this formula: Double(video_FrameRateNominator / video_FrameRateDenominator)
    video_FrameRateNumerator: UINT32;     // The upper 32 bits of the MF_MT_FRAME_RATE attribute value
    video_FrameRateDenominator: UINT32;   // The lower 32 bits of the MF_MT_FRAME_RATE attribute value

    // NOTE:
    //  To calculate the pixel aspect ratio use this formula: video_PixelAspectRatioNumerator / video_PixelAspectRatioDenominator.
    video_PixelAspectRatioNumerator: UINT32;   // The upper 32 bits of the MF_MT_PIXEL_ASPECT_RATIO attribute value
    video_PixelAspectRatioDenominator: UINT32; // The lower 32 bits of the MF_MT_PIXEL_ASPECT_RATIO attribute value

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
    audio_iblockAlignment: UINT32;        // Block alignment, in bytes, for an audio media type.
                                          // Note: For PCM audio formats, the block alignment is equal to the number of audio channels
                                          // multiplied by the number of bytes per audio sample.
    audio_AverageSampleRate: UINT32;      // The average sample rate in bytes per second.
    audio_dwFormatTag: DWORD;             // FormatTag is the replacement of FOURCC

    // NOTE:
    // To calculate bit rate in kbps use this formula:
    // Bitrate (kbps) = (Average Bytes Per Sample * 8) / 1000.
    audio_BitRate_kbps: Double;
    // To calculate sample rate in khz.
    // Samplerate (khz) = Samples Per Second / 1000.
    audio_SampleRate_khz: Double;

    audio_ChannelMask: UINT32;
    // AAC specific.
    audio_ProfileAndLevel: UINT32;
    audio_PayloadType: UINT32;
    // FLAC specific.
    audio_FLAC_: UINT32;

    public
      procedure Reset();
  end;
  TStreamContents = _StreamContents;
  TStreamContentsArray = array of TStreamContents;


  // Record that holds the readable major and subtypes and guidvalues.
  MFT_REGISTER_TYPE_DESCR = record
    RegisterTypeInfo: MFT_REGISTER_TYPE_INFO; // declared in MfObjects
    GuidName: LPWSTR;
    FormatTag: LPWSTR;
    FOURCC: DWord;
    majorTypeDescr: LPWSTR;
    subTypeDescr: LPWSTR;
  end;

  // Record that holds information about a MFT.
  TMftEnumInfo = record
    pwcStr: PWideChar;
    aInputTypes: array of MFT_REGISTER_TYPE_DESCR;
    aOutputTypes: array of MFT_REGISTER_TYPE_DESCR;
    pAttributes: IMFAttributes;
    public
      procedure Reset();
  end;

  // MFT CATEGORIES
  TMftCategory = record
    CategoryGuid: TGuid;
    CategoryGuidName: string;
    CategoryDescr: string;
  end;

  TMftCategoriesArray = array of TMftCategory;

// Collections
// ===========

  // Gets an interface pointer from a Media Foundation collection.
  function GetCollectionObject(pCollection: IMFCollection;
                               const dwIndex: DWORD;
                               out ppObject): HResult;

// Events
// ======

  function GetEventObject(pEvent: IMFMediaEvent;
                          out ppObject): HResult;

  // Alternative for ProcessMessages
  // Example usage: HandleMessages(GetCurrentThread());
  procedure HandleMessages(AThread: THandle;
                           AWait: Cardinal = INFINITE);


// Media Samples
// =============

  // Create a sample and add a buffer to it.
  function CreateMediaSample(cbData: DWORD;
                             out pSample: IMFSample): HResult;


// Media Source
// ============

  // Create a media object from an URL or stream.
  // NOTE: This is the replacement for earlier function CreateMediaSourceFromUrl
  function CreateObjectFromUrl(const sURL: WideString;           // URL of the source.
                               out pSource: IMFMediaSource;      // The received object (mediasource or bytestream)
                               pStore: IPropertyStore = nil;     // Optional property store
                               const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE): HResult;  // Create a source object.

  // Deprecated, use CreateObjectFromUrl.
  function CreateMediaSourceFromUrl(const sURL: WideString;
                                    out pSource: IMFMediaSource): HResult; deprecated 'Use function CreateObjectFromUrl';

  // Begins an asynchronous request to create a media source or a byte stream from an URL.
  function CreateObjectFromUrlAsync(const sURL: WideString;
                                    pCallback: IMFAsyncCallback;
                                    pStore: IPropertyStore = nil;
                                    const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE;
                                    pIUnknownCancelCookie: IUnknown = nil;
                                    punkState: IUnknown = nil): HResult;

  // The aggregated media source is useful for combining streams from separate media sources.
  // For example, you can use it to combine a video capture source and an audio capture source.
  function CreateAggregatedSource(pSource1: IMFMediaSource;
                                  pSource2: IMFMediaSource;
                                  out ppAggSource: IMFMediaSource): HResult;

  // Create a media source for the given device ID.
  // Note: The application has to enumerate the devices first.
  function CreateVideoDeviceSource(DeviceIndex: DWord;
                                   out pSource: IMFMediaSource): HResult;


// Sink- and Sourcereaders
// =======================


  // Creates a sourcereader or sinkwriter depending on the given CLSID.
  function CreateReaderWriter(const clsidObject: TGUID;   // CLSID_MFSinkWriter or CLSID_MFSourceReader
                              initSource: IMFMediaSource; // Must be the initial MediaSource!
                              attributes: IMFAttributes;  // Attributes must be set before using this method!
                              out iunkObject: IUnknown): HResult;

  // This method returns a video activation object that is able to render to a window or
  // any other visual component that has a THandle (HWND).
  function CreateVideoMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                        hVideoWnd: HWND;
                                        out mfActivate: IMFActivate): HResult;

  // This method returns an audio activation object for a renderer.
  function CreateAudioMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                        out mfActivate: IMFActivate): HResult;


// Topologies
// ==========

  // Creates a playback topology from the media source.
  //
  // Pre-condition: The media source must be created already.
  // Call CreateMediaSourceFromUrl() or  CreateMediaSourceFromUrlAsync() before calling this method
  // Create a playback topology from a media source.
  function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                  pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                  hVideoWnd: HWND;                       // Video window.
                                  var ppTopology: IMFTopology;           // Receives a pointer to the topology.
                                  dwSourceStreams: DWORD = 0): HResult;  // Recieves the number of streams

  //  Adds a topology branch for one stream.
  //
  //  pTopology: Pointer to the topology object.
  //  pSourcePD: The source's presentation descriptor.
  //  dwStream: Index of the stream to render.
  //
  //  Pre-conditions: The topology must be created already.
  //
  //  Notes: For each stream, we must do the following:
  //    1. Create a source node associated with the stream.
  //    2. Create an output node for the renderer.
  //    3. Connect the two nodes.
  //  The media session will resolve the topology, so we do not have
  //  to worry about decoders or other transforms.
  function AddBranchToPartialTopology(pTopology: IMFTopology;
                                      pSource: IMFMediaSource;
                                      pPD: IMFPresentationDescriptor;
                                      dwStream: DWord;
                                      hVideoWnd: HWND): HResult;

  // Create the nodes and connect them.
  // This function is very similar to the function named AddBranchToPartialTopology (Creates a playback topology).
  // The only difference is that this function adds the extra node for the decoder.
  function AddBranchToPartialTopologyWithDecoder(pTopology: IMFTopology;          // Topology.
                                                 pSource: IMFMediaSource;         // Media source.
                                                 pPD: IMFPresentationDescriptor;  // Presentation descriptor.
                                                 iStream: DWORD;                  // Stream index.
                                                 hVideoWnd: HWND): HResult;       // Window for video playback.

  // Given a topology, returns a pointer to the presentation descriptor.
  function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                                 out ppPD: IMFPresentationDescriptor): HResult;

  // Returns the duration from a topology.
  function GetDurationFromTopology(pTopology: IMFTopology;
                                   out phnsDuration: LONGLONG): HResult;



// Source nodes
// ============

  // Creates and initializes a source node from a MediaSource.
  function AddSourceNode(pTopology: IMFTopology;                  // Topology.
                         pSource: IMFMediaSource;                 // Media source.
                         pPD: IMFPresentationDescriptor;          // Presentation descriptor.
                         pSD: IMFStreamDescriptor;                // Stream descriptor.
                         out ppNode: IMFTopologyNode): HResult;   // Receives the node pointer.

  // Creates and initializes a source node from a MediaSource.
  function AddSourceStreamNode(pSource: IMFMediaSource;               // Media source.
                               pSourcePD: IMFPresentationDescriptor;  // Presentation descriptor.
                               pSourceSD: IMFStreamDescriptor;        // Stream descriptor.
                               out ppNode: IMFTopologyNode): HResult; // Receives the node pointer.



// Output nodes
// ============

  // Creates and initializes an output node from an activation object.
  function AddOutputNodeA(pTopology: IMFTopology;                 // Topology.
                          pActivate: IMFActivate;                 // Media sink activation object.
                          dwId: DWORD;                            // Identifier of the stream sink.
                          out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.

  // Creates and initializes an output node from a stream sink.
  function AddOutputNodeS(pTopology: IMFTopology;                 // Topology.
                          pStreamSink: IMFStreamSink;             // Stream sink.
                          out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.

  // Creates an output node from a stream descriptor.
  function CreateOutputNode(pSourceSD: IMFStreamDescriptor;        // Stream descriptor
                            hwndVideo: HWND;                       // The handle of an (visual) object.
                            out ppNode: IMFTopologyNode): HResult; // Receives the node pointer.



// Transform nodes
// ===============

  // Creates and initializes a transform node from an MFT (IMFTransform).
  function AddTransformNodeM(pTopology: IMFTopology;                // Topology.
                             pMFT: IMFTransform;                    // MFT.
                             out ppNode: IMFTopologyNode): HResult; // Receives the node pointer.

  // Creates and initializes a transform node from a CLSID.
  function AddTransformNodeC(pTopology: IMFTopology;                 // Topology.
                             const fclsid: CLSID;                    // CLSID of the MFT.
                             out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.

  // Creates and initializes a transform node from an activation object.
  function AddTransformNodeA(pTopology: IMFTopology;                 // Topology.
                             pActivate: IMFActivate;                 // MFT activation object.
                             out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.



// Seeking & Rate
// ==============

  // Scrub given a period (MFTime).
  function DoScrub(const SeekTime: MFTIME;
                   pMediaSession: IMFMediaSession): HResult;

  // Set the playback rate within a mediasession.
  function SetPlaybackRate(pMediaSession: IMFMediaSession;
                           const rateRequested: MFTIME;
                           const bThin: Boolean): HResult;


// Sessions
// ========

  // How to set a stop time for playback when using the Media Session.
  // Sets the Stop Time Before Playback begins.
  //
  // Before you queue a topology for playback, you can specify the stop time by using
  // the MF_TOPONODE_MEDIASTOP attribute.
  // For each output node in the topology, set the value of the MF_TOPONODE_MEDIASTOP
  // to the stop time in 100-nanosecond (hns) units.
  // Note that setting this attribute after playback starts has no effect.
  // Therefore, set the attribute before calling IMFMediaSession.Start().
  // The following code shows how to set the stop time on an existing topology.
  //
  function SetMediaStop(pTopology: IMFTopology;
                        stop: LONGLONG): HResult;

  // Sets the stop time AFTER playback has started.
  //
  // There is a way to set the stop time after the Media Session starts playback,
  // by using the IMFTopologyNodeAttributeEditor interface.
  //
  // Important:
  // This interface has a serious limitation, because the stop time is
  // specified as a 32-bit value. That means the maximum stop time that you can set
  // using this interface is $FFFFFFFF, or just over 7 minutes.
  // This limitation is due to an incorrect structure definition.
  //
  function SetMediaStopDynamic(pSession: IMFMediaSession;
                               pTopology: IMFTopology;
                               stop: LONGLONG): HResult;


// De- & Encoders
// ==============

  // This function searches for a video or audio decoder.
  // Asynchronous, hardware, transcode, and field-of-use decoders are excluded.
  // If a match is found, the code creates the first MFT in the list.
  function FindDecoderEx(const subtype: TGUID;                  // Subtype
                         bAudio: Boolean;                       // TRUE for audio, FALSE for video
                         out ppDecoder: IMFTransform            // Receives a pointer to the decoder.
                         ): HResult; deprecated 'superseded by function GetCodec';


  // Searches for a video or audio encoder.
  // Asynchronous, hardware, transcode, and field-of-use encoders are excluded.
  function FindEncoderEx(const subtype: TGUID;                  // Subtype
                         const bAudio: Boolean;                 // TRUE for audio, FALSE for video
                         out ppEncoder: IMFTransform            // Receives a pointer to the encoder.
                         ): HResult; deprecated 'superseded by function GetCodec';

  // This function is deprecated. Only here for backward compatibility.
  function FindVideoDecoder(const subtype: TGUID;
                            bAllowAsync: Boolean;
                            bAllowHardware: Boolean;
                            bAllowTranscode: Boolean;
                            out ppDecoder: IMFTransform): HResult;  deprecated 'superseded by function GetCodec';

  // Enumerates mft's by category and returns an array of IMFActivate pointers.
  function EnumMft(const mftCategory: TGuid;
                   mftInput: PMFT_REGISTER_TYPE_INFO;   // This parameter can be nil. If nil, all input types are matched.
                   mftOutput: PMFT_REGISTER_TYPE_INFO;  // This parameter can be nil. If nil, all output types are matched.
                   out ppActivate: PIMFActivate;
                   out count: UINT32): HResult;

  // Find the in- and outputs of a MFTclsid (for example CLSID_CMSH264EncoderMFT)
  function GetMftEnumInfo(const MftClsid: CLSID;
                          out mftEnum: TMftEnumInfo): HResult;


  // Searches for a codec (encoder or decoder), with options to include asynchronous, hardware,
  // or transcode decoders, majortype and category. See: MFT_ENUM_FLAG
  // For info for parameter mftRegisterTypeInfo.guidMajorType see: https://learn.microsoft.com/en-us/windows/win32/medfound/media-type-guids
  // For info for parameter mftRegisterTypeInfo.guidSubtype see: https://learn.microsoft.com/en-us/windows/win32/medfound/video-subtype-guids
  //                                                             https://learn.microsoft.com/en-us/windows/win32/medfound/audio-subtype-guids
  // Valid value for parameter mftCategory: MFT_CATEGORY_VIDEO_ENCODER, MFT_CATEGORY_VIDEO_DECODER, MFT_CATEGORY_AUDIO_ENCODER, MFT_CATEGORY_AUDIO_DECODER.
  // The value for parameter selIndex is 0 by default, which means the function will select the first mft in the list.
  // If you want another fmt, you should call function EnumMft prior to this function to determine the mft from the list.
  //
  // Parameters mftRegisterTypeInput and mftRegisterTypeOutput values can be retrieved by calling function GetMftEnumInfo prior
  // to calling this function. One or both parameters can be nil. If nil, all input or/and output types are matched.
  function GetCodec(mftRegisterTypeInput: PMFT_REGISTER_TYPE_INFO;   // Input type.
                    mftRegisterTypeOutput: PMFT_REGISTER_TYPE_INFO;  // Output type.
                    const mftCategory: TGuid;
                    out mftCodec: IMFTransform;
                    flags: MFT_ENUM_FLAG = MFT_ENUM_FLAG_ALL;
                    selIndex: Integer = 0): HResult;

  // Returns the MFT decoder based on the major type GUID.
  function GetDecoderCategory(const majorType: TGUID;
                              out pCategory: TGUID): HResult;

  // Returns the MFT encoder based on the major type GUID.
  function GetEncoderCategory(const majorType: TGUID;
                              out pCategory: TGUID): HResult;

  // Returns an array of MFT guids, guid names and mft descriptions.
  function GetMftCategories(): TArray<TMftCategory>;


  // Finds a decoder for a stream.
  //
  // If the stream is not compressed, pCLSID receives the value GUID_NULL.
  function FindDecoderForStream(pSD: IMFStreamDescriptor;      // Stream descriptor for the stream.
                                out opCLSID: CLSID): HResult;  // Receives the CLSID of the decoder.

  // Creates a transcode profile for the given params mfAudioFormat and mfTranscodeContainerType.
  function CreateTranscodeProfile(const mfAudioFormat: TGUID;  // For example: MFAudioFormat_WMAudioV9
                                  const mfTranscodeContainerType: TGUID; // For example: MFTranscodeContainerType_ASF
                                  out ppProfile: IMFTranscodeProfile): HResult;


  // Configures the recordsink for encoding video using default media type.
  function ConfigureVideoEncoding(pSource: IMFCaptureSource;
                                  pRecord: IMFCaptureRecordSink;
                                  const guidEncodingType: REFGUID): HResult; overload;

  // Configures the recordsink for encoding video using a given media type.
  function ConfigureVideoEncoding(pSource: IMFCaptureSource;
                                  pRecord: IMFCaptureRecordSink;
                                  const guidEncodingType: REFGUID;
                                  pMediaType: IMFMediaType): HResult; overload;


  // Configures the recordsink for audio (if an audiostream is present).
  function ConfigureAudioEncoding(pSource: IMFCaptureSource;
                                  pRecord: IMFCaptureRecordSink;
                                  const guidEncodingType: REFGUID): HResult;


  // Sets the Video Capture Format for a capture device
  //
  // Preparation
  // Call method EnumerateCaptureFormats first to inventarize the supported capture formats of the device.
  //
  // To set a capture format:
  //   1 - Get a pointer to the IMFMediaTypeHandler interface, as shown under Enumerations / function EnumerateCaptureFormats.
  //   2 - Call IMFMediaTypeHandler.GetMediaTypeByIndex to get the desired format, specified by index.
  //   3 - Call IMFMediaTypeHandler.SetCurrentMediaType to set the format.
  //
  // If you do not set the capture format, the capturedevice will use its default format.
  // This function sets the capture format.
  function SetDeviceFormat(pSource: IMFMediaSource;
                           dwFormatIndex: DWORD): HResult; overload;
  function SetDeviceFormat(pSource: IMFMediaSource;
                           pMediaType: IMFMediaType;
                           dwFormatIndex: DWORD): HResult; overload;


  // List audio or video encoders on this system.
  function ListEncoders(const subtype: TGUID;
                        bAudio: Boolean;
                        var aGuidArray: TClsidArray): Hresult;

  // Create an encoder found with function ListEncoders.
  function CreateEncoderFromClsid(mftCategory: CLSID;
                                  out pEncoder: IMFTransform): HResult;


  // The following functions might be useful when creating a video media type.
  // ===========================================================================
  // Function                                      Description
  // --------------------------------------------  ------------------------------------------------------------------------------------------------
  // MFAverageTimePerFrameToFrameRate	             Calculates the frame rate, given the average frame duration.
  // MFCalculateImageSize	                         Calculates the image size for an uncompressed video format.
  // MFFrameRateToAverageTimePerFrame	             Calculates the average duration of a video frame, given the frame rate.
  // MFGetStrideForBitmapInfoHeader	               Returns the minimum surface stride for a video format. For more information, see Image Stride.
  // MFInitVideoFormat	                           Initializes an MFVIDEOFORMAT structure for some standard video formats, such as NTSC television.
  //                                               You can then use the structure to initialize a media type.
  // MFIsFormatYUV	                               Queries whether a video format is a YUV format.


  // This function fills in the most common information for an uncompressed video format.
  // The function returns an IMFMediaType interface pointer.
  // You can then add additional attributes to the media type as needed.
  function CreateUncompressedVideoType(const fccFormat: DWORD;   // FOURCC or D3DFORMAT value.
                                       aWidth: UINT32;
                                       aHeight: UINT32;
                                       interlaceMode: MFVideoInterlaceMode;
                                       frameRate: MFRatio;
                                       par: MFRatio;
                                       out ppType: IMFMediaType): HResult;

  // This funtion takes an encoded video format as input, and creates a matching uncompressed video type.
  // This type would be suitable to set on an encoder or decoder.
  function ConvertVideoTypeToUncompressedType(pType: IMFMediaType;       // Pointer to an encoded video type.
                                              const subtype: TGUID;      // Uncompressed subtype (eg, RGB-32, AYUV)
                                              out ppType: IMFMediaType   // Receives a matching uncompressed video type.
                                              ): HResult;


// Enumerations
// ============

  // To enumerate the video capture devices on the system, do the following:
  // Call function EnumCaptureDeviceSources with the following parameters:
  //  Parameters:  AttributeSourceType: MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID for audio or
  //                                    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID for video.
  //               DeviceProperties: array of TDevicePropsA.
  //
  //  1 Call MFCreateAttributes to create an attribute store.
  //    This function receives an empty IMFAttributes pointer.
  //  2 Call IMFAttributes.SetGUID to set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE attribute.
  //    Set the attribute value to MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID.
  //  3 Call MFEnumDeviceSources.
  //    This function receives an array of IMFActivate pointers and the array size.
  //    Each pointer represents a distinct video capture device.
  //
  // To create an instance of a capture device:
  //  Call IMFActivate.ActivateObject to get a pointer to the IMFMediaSource interface.
  //
  // This function provide the steps 1 to 3:
  function EnumCaptureDeviceSources(const pAttributeSourceType: TGuid;
                                    var pDeviceProperties: TDevicePropertiesArray): HResult;

  // Retrieves all native video formats of a device and stores them in TDevicePropertiesArray
  function GetCaptureDeviceCaps(pSourceReader: IMFSourceReader;
                                var pDeviceProperties: TDevicePropertiesArray;
                                pDeviceIndex: DWord = 0;
                                pStreamIndex: DWord = MF_SOURCE_READER_FIRST_VIDEO_STREAM): HResult;

  // Checks if the the formats stored in the TDevicePropertiesArray are
  // supported by Media Foundation.
  function GetSupportedVideoFormats(CurrentArray: TDevicePropertiesArray): TDevicePropertiesArray;

  // This function activates a selected device stored in TDeviceProperties.
  function CreateCaptureDeviceInstance(pDeviceProperties: TDeviceProperties;
                                       out ppSource: IMFMediaSource;
                                       out ppActivate: IMFActivate): HResult;

  // Enumerates the capture formats for a device.
  // Note: See also function SetDeviceFormat
  // This function returns an pointer array of IMFMediaType.
  // To get the current capture device's IMFMediaSource object, call IMFCaptureSource.GetCaptureDeviceSource.
  function EnumerateCaptureFormats(pSource: IMFMediaSource;
                                   out ppMediaType: PIMFMediaType): HResult;

  // Enumerates the media types for every stream from a device or mediafile.
  // The function examines the mediatype format of the data internally for validation.
  // If the source represents a media file, there is typically only one type per stream.
  // A webcam might be able to stream video in several different formats.
  // In that case, an application can select which format to use from the list of media types.
  function EnumerateMediaTypes(pReader: IMFSourceReader;
                               pStreamIndex: DWord;
                               var ppMediaTypes: TArray<IMFMediaType>;
                               out pCount: DWord): HResult;

  // Returns the number of streams from a IMFSourceReader.
  function CountSourceReaderStreams(pReader: IMFSourceReader): DWord;

  // Counts mediatypes from a device
  // When the list index goes out of bounds, GetNativeMediaType returns MF_E_NO_MORE_TYPES.
  // This is not an error, but indicates the end of the list.
  // Set parameter MfSupportedOnly to False if you want to get the total of all native types from the device and
  // to True for Media Foundation supported formats
  function CountTypesFromDevice(pReader: IMFSourceReader;
                                const pStreamIndex: DWORD;
                                out pCount: DWord;
                                const pMfSupportedOnly: Boolean = True): HResult;

  // Returns the name, name of the formattag and FOURCC value of a guid.
  function GetGUIDNameConst(const majorType: TGuid;
                            const subType: TGuid;
                            out aGuidName: LPWSTR;
                            out aFormatTag: LPWSTR;
                            out aFOURCC: DWord;
                            out aFmtDesc: LPWSTR): HResult;

  // Checks if a given input subtype is supported by Media Foundation MFT.
  function IsMfSupportedFormat(const pSubType: TGuid): Boolean; inline; deprecated 'Use function IsMftSupportedInputFormat';
  function IsMftSupportedInputFormat(const pSubType: TGuid): Boolean; inline;

  // Returns an array of supported formats.
  function GetSupportedMftOutputFormats(): TArray<TGuid>;

  // Checks if a given output subtype is supported by Media Foundation MFT and if
  // the format has top-down data (stride).
  function IsMftSupportedOutputFormat(const pSubType: TGuid;
                                      out IsTopDown: Boolean): Boolean; inline;


// Device Loss
// ===========

  // Register for Device Notification.
  // Before you start capturing from a device, call the RegisterDeviceNotification
  // function to register for device notifications.
  // Register for the KSCATEGORY_CAPTURE device class, as shown in this function.
  function RegisterForDeviceNotification(hw: HWND;
                                         out g_hdevnotify: HDEVNOTIFY): Bool;

  // Before an application or device is closing, unregister for device notifications.
  function UnRegisterForDeviceNotification(g_hdevnotify: HDEVNOTIFY): Bool;


// Device SymLink & FriendlyName
//==============================
  // Get the Symbolic Link of the device.
  // Enumerate the video devices on the system, as described in Enumerating Video Capture Devices.
  // Choose a device from the list, and then query the activation object for the
  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK (= default) or MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK attribute,
  // as shown in this function.
  function GetSymbolicLink(pActivate: IMFActivate;
                           out g_pwszSymbolicLink: PWideChar;
                           out g_cchSymbolicLink: UINT32;
                           devMediaType: TGUID): HResult;

  // Get the readable name of the device.
  function GetDeviceName(pActivate: IMFActivate;
                         out g_pwszDeviceName: PWideChar;
                         out g_cchDeviceName: UINT32): HResult;

  function GetDeviceNameFromCollection(DeviceCollection: IMMDeviceCollection;
                                       DeviceIndex: UINT): LPWSTR;


  // Enable Video Acceleration
  // =========================
  // Adds an audio or video decoder to a topology (applies only to video decoders).
  // To get the best performance for video playback, you should enable
  // DirectX Video Acceleration (DXVA) if the video decoder supports it.
  // Usually this step is performed by the topology loader, but if you add the decoder to
  // the topology manually, then you must perform this step yourself.
  // As a precondition for this step, all output nodes in the topology must be bound to media sinks.
  // For more information, see:
  //   https://learn.microsoft.com/en-us/windows/win32/medfound/binding-output-nodes-to-media-sinks
  //
  // First, find the object in the topology that hosts the Direct3D device manager.
  // To do so, get the object pointer from each node and query the object for the
  // IDirect3DDeviceManager9 service.
  // Typically the enhanced video renderer (EVR) serves this role.
  //
  // The following function shows a function that finds the device manager:

  // Finds the node in the topology that provides the Direct3D device manager.

  function FindDeviceManager(pTopology: IMFTopology;            // Topology to search.
                             out ppDeviceManager: IInterface;   // Receives a pointer to the device manager.
                             out ppNode: IMFTopologyNode): HResult;


// Audio and video capture
// =======================

  // Creates a media source for the choosen deviceindex of the video capture device in the enumeration list.
  function CreateVideoCaptureDevice(const iDeviceIndex: UINT32;
                                    out pSource: IMFMediaSource): HResult; overload;

  // Does the same if you know the symbolic link
  function CreateVideoCaptureDevice(const pszSymbolicLink: LPCWSTR;
                                    out pSource: IMFMediaSource): HResult; overload;

  // Takes an audio endpoint ID and creates a media source.
  function CreateAudioCaptureDevice(const pszEndPointID: LPCWSTR;
                                    out pSource: IMFMediaSource): HResult;
  // Lists the devicenames from an IMFActivate array.
  procedure ListDeviceNames(ppDevices: PIMFActivate; // Pointer to array of IMFActivate
                            out iList: TStringList); // output

  // Sets the maximum frame rate on the media source.
  function SetMaxFrameRate(pSource: IMFMediaSource;
                           dwTypeIndex: DWORD): HResult;


// SAR (Streaming Audio Renderer)
// ==============================

 // Enumerates the audio rendering devices and assigns the first device or nDevice in the list to the SAR.
 function EnumAudioRenderingDevices(nDevice: UInt;
                                    out wstrID: PWideChar; // Id
                                    pSink: IMFMediaSink = nil;  // Streaming audio renderer (SAR)
                                    pActivate: IMFActivate = nil // Activation object, which can be used to create the SAR.
                                    ): HResult;


// Helper methods
// ==============

  // This function fills in a BITMAPINFOHEADER structure from a video media type.
  // Note that this conversions loses some of the format information (interlacing, frame rate,
  // extended color data).
  // However, it might be useful when saving a bitmap from a video frame, for example.
  // Converts a video type to a BITMAPINFO structure.
  // The caller must free the structure by calling CoTaskMemFree.
  function GetBitmapInfoHeaderFromMFMediaType(pType: IMFMediaType;     // Pointer to the media type.
                                              out ppBmih: PBITMAPINFOHEADER; // Receives a pointer to the structure.
                                              out pcbSize: DWORD // Receives the size of the structure.
                                              ): HResult;

  // Copies an attribute value from one attribute store to another.
  function CopyAttribute(pSrc: IMFAttributes;
                         var pDest: IMFAttributes;
                         const key: TGUID): HResult; overload;

  function CopyAttribute(pSrc: IMFMediaType;
                         var pDest: IMFMediaType;
                         const key: TGUID): HResult; overload;


  // Creates a compatible video format with a different subtype if param guidSubType <> GUID_NULL else
  // the SubType will be the source subtype.
  function CloneVideoMediaType(pSrcMediaType: IMFMediaType;
                               const guidSubType: REFGUID;
                               out ppNewMediaType: IMFMediaType): HResult;


  // Creates a JPEG, RGB32 or WIC GUID_ContainerFormat imagetype that is compatible with a specified video media type.
  // NOTE: When parameter pSubTypeGuid is wrong or not supported the result value in OnCaptureEvent.MF_CAPTURE_ENGINE_PHOTO_TAKEN will be WINCODEC_ERR_COMPONENTNOTFOUND
  //       Valid subformats are:
  //         MFImageFormat_JPEG, GUID_ContainerFormatBmp, GUID_ContainerFormatJpeg etc.
  //
  // WARNING: DON'T USE MFImageFormat_RGB32! (This will end with a WINCODEC_ERR_COMPONENTNOTFOUND)
  function CreatePhotoMediaType(const psubTypeGuid: TGuid;
                                var pPhotoMediaType: IMFMediaType): HResult; overload;
  function CreatePhotoMediaType(const psubTypeGuid: TGuid;
                                pSrcMediaType: IMFMediaType;
                                out ppPhotoMediaType: IMFMediaType): HResult; overload;


// Video Media Type Helpers ////////////////////////////////////////////////////
//==============================================================================

  // Helper function to get the frame rate from a video media type.
  function GetFrameRate(pType: IMFMediaType;
                        out uiNumerator: UINT32;
                        out uiDenominator: UINT32): HResult; inline;

  // Helper function to set the frame rate on a video media type.
  function SetFrameRate(pType: IMFMediaType;
                        uiNumerator: UINT32;
                        uiDenominator: UINT32): HResult; inline;

  // Helper to calculate framerate from ratio (numerator / denominator)
  function GetFrameRateFromRatio(uiNumerator: UINT32;
                                 uiDenominator: UINT32): FLOAT; inline;

  // Helper function to get the frame size from IMFAttributes.
  function GetFrameSize(pAttributes: IMFAttributes;
                        out uiWidth: UINT32;
                        out uiHeigth: UINT32): HResult; inline;

  // Helper function to get the frame size from a video media type.
  function GetFrameSizeFromMediaType(pType: IMFMediaType;
                                     out uiWidth: UINT32;
                                     out uiHeight: UINT32): HResult; inline;


  // Helper function to set the frame size on a video attributes.
  function SetFrameSize(pAttributes: IMFAttributes;
                        uiWidth: UINT32;
                        uiHeigth: UINT32): HResult; inline;

  // Helper function to set the frame size on a video media type.
  function SetFrameSizeOnMediaType(pType: IMFMediaType;
                                   uiWidth: UINT32;
                                   uiHeigth: UINT32): HResult; inline;

  // Helper to get the encoding bitrate from a media type.
  function GetEncodingBitrate(pType: IMFMediaType;
                              out uiEncodingBitrate: UINT32): HResult; inline;

  // Helper function to get the pixel aspect ratio from a video media type.
  function GetPixelAspectRatio(pAttributes: IMFAttributes;
                               out uiNumerator: UINT32;
                               out uiDenominator: UINT32): HResult; inline;

  // Helper function to set the pixel aspect ratio on a video media type.
  function SetPixelAspectRatio(pAttributes: IMFAttributes;
                               uiNumerator: UINT32;
                               uiDenominator: UINT32): HResult; inline;

  // Helper function to specify whether to pad a video image so that it fits within a specified aspect ratio.
  function SetOutputRectangleAspectRatio(pAttributes: IMFAttributes;
                                         stVideoPadFlags: MFVideoPadFlags = MFVideoPadFlag_PAD_TO_None): HResult; inline;


  // Gets display area from a media type.
  function GetVideoDisplayArea(pType: IMFMediaType;
                               out pArea: MFVideoArea): HResult;

  // Convert a rectangle from one pixel aspect ratio (PAR) to another,
  // while preserving the picture aspect ratio.
  function CorrectAspectRatio(const src: TRect;
                              const srcPAR: MFRatio;
                              const destPAR: MFRatio): TRect;

  // Calculates the letterbox area, given a source and destination rectangle.
  // It is assumed that both rectangles have the same PAR.
  function LetterBoxRect(const rcSrc: TRect;
                         const rcDst: TRect): TRect;


////////////////////////////////////////////////////////////////////////////////


  // Dumps the media buffer contents of an IMFSample to a stream.
  // [in] pSample: pointer to the media sample to dump the contents from.
  // [in] pStream: pointer to the stream to write to.
  function WriteSampleToStream(pSample: IMFSample;
                               pStream: TMemoryStream): HResult;

  // Gets metadata from a media source or other object.
  // If a media source supports this interface, it must expose the interface as a service.
  // To get a pointer to this interface from a media source, call IMFGetService.GetService.
  // The service identifier is MF_METADATA_PROVIDER_SERVICE.
  // Other types of object can expose this interface through QueryInterface.
  // Use this interface to get a pointer to the IMFMetadata interface.
  function GetMetadata(pSource: IMFMediaSource;
                       out ppMetadata: IMFMetadata;
                       dwStream: DWORD): HResult;

  // Returns the stream identifier from an active stream, of a given streamtype.
  function GetActiveStreamIndex(stmediaType: TMediaTypes;
                                pspd: IMFPresentationDescriptor;
                                out dwStreamId: DWORD): HResult;

  // Gets the streams information from a source  (like language, format, compression etc.)
  // It returns an array of the stream content values.
  function GetStreamContents(pspd: IMFPresentationDescriptor;
                             mSource: IMFMediaSource;
                             var alsCont: TStreamContentsArray): HResult;

  // Returns the Major guid and compression.
  function GetMediaType(pStreamDesc: IMFStreamDescriptor;
                        out tgMajorGuid: TGuid;
                        out bIsCompressedFormat: BOOL): HResult;

  // Returns a matching MediaType interface.
  function FindMatchingVideoType(pMediaTypeHandler: IMFMediaTypeHandler;
                                 const gPixelFormat: TGUID;
                                 pWidth: UINT32;
                                 pHeight: UINT32;
                                 pFps: UINT32;
                                 out pOutMediaType: IMFMediaType): HResult;

  // Check if a given guid is a major type.
  function IsMajorType(const guid: TGuid): Boolean;

  // Returns the mediatype associated with the Major guid.
  // To get the major type call function GetMediaType
  function GetMediaDescription(const pMajorGuid: TGuid;
                               out mtMediaType: TMediaTypes): HResult;

  // Returns the name of the Majortype constant. (RTTI will not work on some Delphi versions)
  function GetMajorTypeDescr(const pMajorGuid: TGuid): LPWSTR;

  // Gets audio (EndPoint)device capabilities
  function GetAudioFormat(var pMfAudioFormat: TMFAudioFormat): HResult;


  // Gets audio stream info from a media source V1.
  function GetAudioSubType(mSource: IMFMediaSource;
                           out pSubType: TGUID;
                           out pFormatTag: DWord;
                           out pDescr: Widestring;
                           out pChannels: UINT32;
                           out pSamplesPerSec: UINT32;
                           out pBitsPerSample: UINT32;
                           out pBlockAlignment: UINT32;
                           out pAverageSampleRate: UINT32;
                           out pBitRate: Double;
                           out pSampleRate: Double): HResult; overload;

  // Gets audio stream info from a media source V2.
  function GetAudioSubType(var pAudioFormat: TMFAudioFormat): HResult; overload;



  // Gets the Windows supported audio encoder formats (MFT's).
  function GetWinAudioEncoderFormats(const mfAudioFormat: TGuid;
                                     MftEnumFlag: MFT_ENUM_FLAG; // Flag for registering and enumeration Media Foundation Transforms (MFTs).
                                     out aAudioFmts: TMFAudioFormatArray): HResult;

  // Helper function to get an attribute whose value is a string.
  function AttributeGetString(pAttributes: IMFAttributes;
                              const guid: TGuid;  // For example: To get the MFT friendly name use MFT_FRIENDLY_NAME_Attribute
                              out pwcStr: PWideChar): HResult; overload;

  function AttributeGetString(pActivate: IMFActivate;
                              const guid: TGuid;
                              out pwcStr: PWideChar): HResult; overload;

// Ducking
// =======

  // This function gets a reference to the IAudioSessionControl2
  // interface and call its methods to determine whether the stream associated with
  // the audio session is a system sound.
  function SetDuckingForSystemSounds(): HResult;


// Sami (.smi .sami)
// =================

  // Synchronized Accessible Media Interchange (SAMI) is a format for adding captions to digital media.
  // The captions are stored in a separate text file with the file name extension .smi or .sami.
  // In Media Foundation, SAMI caption files are supported through the SAMI media source.
  // Use the Source Resolver to create an instance of the SAMI media source from a URL or byte stream.
  // Media Foundation does not provide a component that displays SAMI captions.
  // The application must interpret the caption data that it receives from the SAMI media source.
  //
  // To change the current style, use the IMFSAMIStyle interface.
  // This interface is obtained by calling IMFGetService.GetService on the SAMI media source.
  // (If you are using the SAMI media source with the Media Session, call GetService on the Media Session.)
  // The service identifier is MF_SAMI_SERVICE.
  // See: https://learn.microsoft.com/en-us/windows/win32/medfound/sami-media-source
  // The following function sets the current SAMI style, specified by index.
  function SetSAMIStyleByIndex(pSource: IMFMediaSource;
                               index: DWORD): HResult;


// Media files duration and filesize
// =================================

  // Getting the File Duration.
  // To get the duration of a media file, call the IMFSourceReader.GetPresentationAttribute method and
  // request the MF_PD_DURATION attribute.
  function GetFileDuration(pSource: IMFSourceReader;
                           out phnsDuration: LONGLONG): HResult; overload;

  // Same as above, but returns MFTIME.
  function GetFileDuration(pSourceReader: IMFSourceReader;
                           out mftDuration: MFTIME): HResult; overload;

  // Alternatively you might get the duration of a media file by calling the IMFMediaSource.CreatePresentationDescriptor method and
  // request the MF_PD_DURATION attribute.
  function GetFileDuration(pSource: IMFMediaSource;
                           out pDuration: LONGLONG): HResult; overload;

  // Get fileduration from an URL.
  function GetFileDuration(const sURL: PCWSTR;
                           out pDuration: LONGLONG): HResult; overload;

  // Get fileduration in MFTIME units from an URL.
  function GetFileDuration(const sURL: PCWSTR;
                           out pDuration: MFTIME): HResult; overload;


  // Gets the file size.
  function GetFileSize(pReader: IMFSourceReader;
                       out phnsFileSize: ULONGLONG): HResult; overload;

  // Alternatively you might get the filesize of a media file by calling the IMFMediaSource.CreatePresentationDescriptor method and
  // request the MF_PD_TOTAL_FILE_SIZE attribute.
  function GetFileSize(pReader: IMFMediaSource;
                       out phnsFileSize: ULONGLONG): HResult; overload;


// External methods
// ================

  // Brings the thread that created the specified window into the foreground and
  // activates the window.
  // Keyboard input is directed to the window, and various visual cues are changed for the user.
  // The system assigns a slightly higher priority to the thread that created
  // the foreground window than it does to other threads.
  function SetForegroundWindow(hWnd: HWND): Boolean; stdcall;

  // The foreground process can call the LockSetForegroundWindow function to disable
  // calls to the SetForegroundWindow function.
  function LockSetForegroundWindow(uLockCode: UINT): Boolean; stdcall;


// Sequencer Source
// ================

  // The sequencer source enables an application to play a collection of media sources sequentially,
  // with seamless transitions between the sources.
  // You can use it to create playlists, or to play streams from multiple sources simultaneously.
  // See: https://learn.microsoft.com/en-us/windows/win32/medfound/about-the-sequencer-source
  //      https://learn.microsoft.com/en-us/windows/win32/medfound/using-the-sequencer-source


const

// Aliases section
// ===============

  // Renamed functions and procedures for backward compatibility
  CreateVideoCaptureDeviceBySymolicLink: function(const pszSymbolicLink: LPCWSTR;
                                                  out ppSource: IMFMediaSource): HResult = CreateVideoCaptureDevice;


// System
// ======
// mfBoolToStr moved to MfUtils.



// Misc
// ====
procedure CopyWaveFormatEx(const SourceFmt: WAVEFORMATEX;
                           out DestFmt: PWAVEFORMATEX); //inline;

// Returns 16 - bit PCM format.
function GetDefaultWaveFmtEx(): WAVEFORMATEX; inline;



// Get the assignment of audio channels to speaker positions and name, from a given MF_MT_AUDIO_CHANNEL_MASK attribute.
procedure GetSpeakersLayOut(const ChannelMatrix: UINT32;
                            out aLayout: string;
                            out aChannels: string);


implementation


uses
  {$IFDEF DEBUG}
  WinApi.MediaFoundationApi.MfMediaTypeDebug;
  {$ENDIF}

const
  User32Lib = 'User32.dll';



{ TMFAudioFormat }
procedure TMFAudioFormat.Reset();
begin
  SafeRelease(mfSource);

  tgMajorFormat := GUID_NULL;
  tgSubFormat := GUID_NULL;

  dwFormatTag := 0;
  wsDescr := '';
  unChannels := 0;
  unSamplesPerSec := 0;
  unSamplesPerBlock := 0;
  unValidBitsPerSample := 0;
  unBitsPerSample := 0;
  unBlockAlignment := 0;
  unAvgBytesPerSec := 0;
  unChannelMask := 0;
  // AAC extra data.
  unAACPayload := 0;
  unAACProfileLevel := 0;
  // FLAC extra data.
  unFlacMaxBlockSize := 0;
end;


{ TVideoFormatInfo }
procedure TVideoFormatInfo.Reset();
begin

  iVideoWidth := 0;
  iVideoHeight := 0;
  iBufferWidth := 0;
  iBufferHeight := 0;
  iStride := 0;

  // Major & Subtypes
  fSubType := GUID_NULL;
  fMajorType := GUID_NULL;

  // Supported framerates
  fFrameRate := 0.0;
  iFrameRateNumerator := 0;
  iFrameRateDenominator := 0;
  iMaxFrameRate := 0;
  iMaxFrameRateDenominator := 0;
  iMinFrameRate := 0;
  iMinFrameRateDenominator := 0;

  // Aspectratio
  AspectRatioSupported := False;
  AspectRatioNumerator := 0;
  AspectRatioDenominator := 0;
  SafeRelease(mfMediaType);
end;


{ TDeviceDetails }
procedure TDeviceProperties.Reset();
var
  i: Integer;

begin
  riId := GUID_NULL;
  iCount := 0;
  iDeviceIndex := 0;
  lpFriendlyName := nil;
  lpDisplayName := nil;
  lpSymbolicLink := nil;
  dwSupportedFormats := 0;
  dwNativeFormats := 0;

  for i := 0 to Length(aVideoFormats) - 1 do
    aVideoFormats[i].Reset;
  aVideoFormats := nil;

  for i := 0 to Length(aAudioFormats) - 1 do
    aAudioFormats[i].Reset;
  aAudioFormats := nil;

end;


function GetEventObject(pEvent: IMFMediaEvent;
                        out ppObject): HResult;
var
  vVar: PROPVARIANT;
  hr: HResult;

begin

  PropVariantInit(vVar);

  hr := pEvent.GetValue(vvar);
  if (SUCCEEDED(hr)) then
    begin
      if (vvar.vt = VARTYPE(VT_UNKNOWN)) then
        hr := IUnknown(vvar.ppunkVal).QueryInterface(IID_IUnknown,
                                                     ppObject)
      else
        hr := MF_E_INVALIDTYPE;

      PropVariantClear(vVar);
    end;

  Result := hr;
end;


procedure HandleMessages(AThread: THandle;
                         AWait: Cardinal = INFINITE);
var
  pMsg: TMsg;

begin

  while (MsgWaitForMultipleObjects(1,
                                   AThread,
                                   False,
                                   AWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(pMsg,
                  0,
                  0,
                  0,
                  PM_REMOVE);  // Messages are not removed from the queue after processing by PeekMessage.

      if pMsg.Message = WM_QUIT then
        Exit;

      TranslateMessage(pMsg);
      DispatchMessage(pMsg);
    end;
end;


// Create a sample and add a buffer to it.
function CreateMediaSample(cbData: DWORD;
                           out pSample: IMFSample): HResult;
var
  hr: HResult;
  mfSample: IMFSample;
  pBuffer: IMFMediaBuffer;

begin
  hr := MFCreateSample(mfSample);

  if SUCCEEDED(hr) then
    hr := MFCreateMemoryBuffer(cbData,
                               pBuffer);

  if SUCCEEDED(hr) then
    hr := mfSample.AddBuffer(pBuffer);

  if SUCCEEDED(hr) then
    pSample := mfSample;

  Result := hr;
end;


// Deprecated, use CreateObjectFromUrl.
function CreateMediaSourceFromUrl(const sURL: WideString;
                                  out pSource: IMFMediaSource): HResult;
begin
  Result := CreateObjectFromUrl(sURL,
                                pSource);
end;


// Create a media object from an URL or stream.
// NOTE: This is the replacement for earlier function CreateMediaSourceFromUrl
function CreateObjectFromUrl(const sURL: WideString;
                             out pSource: IMFMediaSource;
                             pStore: IPropertyStore = nil;
                             const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE): HResult;
var
  ObjectType: MF_OBJECT_TYPE;
  pSourceResolver: IMFSourceResolver;
  unkSource: IUnknown;
  hr: HResult;

label
  done;

begin

  ObjectType := MF_OBJECT_INVALID;

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);
  if (FAILED(hr)) then
    goto done;

  // Use the source resolver to create the media source.
  // Note: For simplicity this function uses the synchronous method on
  // IMFSourceResolver to create the media source. However, creating a media
  // source can take a noticeable amount of time, especially for a network source.
  // For a more responsive UI, use the asynchronous function CreateObjectFromUrlAsync.

  hr := pSourceResolver.CreateObjectFromURL(LPCWSTR(sURL), // URL of the source.
                                            dwFlags,       // Create a source object.
                                            pStore,        // Optional property store.
                                            ObjectType,    // Receives the created object type.
                                            unkSource);    // Receives a pointer to the media source (IUnknown).
  if (FAILED(hr)) then
    goto done;



  // Get the IMFMediaSource interface from the media source.
  hr := unkSource.QueryInterface(IUnknown,
                                 pSource);

  // This will work as well: pSource := IMFMediaSource(unkSource);

done:
  // unlike C/CPP Delphi cleans up all interfaces when going out of scope.
  Result := hr;
end;


// Begins an asynchronous request to create a media source or a byte stream from a URL.
function CreateObjectFromUrlAsync(const sURL: WideString;
                                  pCallback: IMFAsyncCallback;
                                  pStore: IPropertyStore = nil;
                                  const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE;
                                  pIUnknownCancelCookie: IUnknown = nil;
                                  punkState: IUnknown = nil): HResult;
var
  pSourceResolver: IMFSourceResolver;
  hr: HResult;

label
  done;

begin

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);
  if (FAILED(hr)) then
    goto done;

  // Use the source resolver to create the media source.

  hr := pSourceResolver.BeginCreateObjectFromURL(LPCWSTR(sURL),         // URL of the source.
                                                 dwFlags,               // Create a source object.
                                                 pStore,                // Optional property store.
                                                 pIUnknownCancelCookie, // Receives an IUnknown pointer or the value nil.
                                                 pCallback,             // Pointer to the IMFAsyncCallback interface of a callback object. The caller must implement this interface.
                                                 punkState);            // Pointer to the IUnknown interface of a state object, defined by the caller. This parameter can be nil.

done:
  // unlike C/CPP Delphi cleans up all interfaces when going out of scope.
  Result := hr;
end;


// Combine streams from separate media sources. For example one for audio and one for video
function CreateAggregatedSource(pSource1: IMFMediaSource;
                                pSource2: IMFMediaSource;
                                out ppAggSource: IMFMediaSource): HResult;
var
  hr: HResult;
  pCollection: IMFCollection;

begin
  ppAggSource := nil;
  pCollection := nil;

  hr := MFCreateCollection(pCollection);

  // Add sources to IMFCollection
  if SUCCEEDED(hr) then
    hr := pCollection.AddElement(pSource1);

  if SUCCEEDED(hr) then
    hr := pCollection.AddElement(pSource2);

  // Create the aggregate source
  if SUCCEEDED(hr) then
    hr := MFCreateAggregateSource(pCollection,
                                  ppAggSource);

  Result := hr;
end;


function CreateVideoDeviceSource(DeviceIndex: DWord;
                                 out pSource: IMFMediaSource): HResult;
var
  hr: HResult;
  icount: UINT32;
  i: Integer;
  MediaSource: IMFMediaSource;
  pAttributes: IMFAttributes;
  ppDevices: PIMFActivate;

label
  done;

begin

  // Create an attribute store to specify the enumeration parameters.
  hr := MFCreateAttributes(pAttributes,
                           1);
  if FAILED(hr) then
    goto done;

  // Source type: video capture devices
  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
  if FAILED(hr) then
    goto done;

  // Enumerate devices first.
  hr := MFEnumDeviceSources(pAttributes,
                            ppDevices,
                            iCount);
  if FAILED(hr) then
    goto done;

  if (DeviceIndex > DWORD(icount)) or
     (icount = 0) then
    begin
      hr := MF_E_NO_CAPTURE_DEVICES_AVAILABLE;
      goto done;
    end;

{$POINTERMATH ON}
  // Create the media source object.

  hr := ppDevices[DeviceIndex].ActivateObject(IID_IMFMediaSource,
                                              Pointer(MediaSource));

  if FAILED(hr) then
    goto done;

  pSource := MediaSource;

done:

  for i := 0 to iCount -1 do
   SafeRelease(ppDevices[i]);
  CoTaskMemFree(ppDevices);
  Result := hr;
end;



// SINKS AND SOURCEREADERS
// =======================


// Creates a sourcereader or sinkwriter depending on the given CLSID
function CreateReaderWriter(const clsidObject: TGUID;   // CLSID_MFSinkWriter or CLSID_MFSourceReader
                            initSource: IMFMediaSource; // Must be the the initial MediaSource!
                            attributes: IMFAttributes;  // Attributes must be set before using this method!
                            out iunkObject: IUnknown): HResult;
var
  hr: HResult;
  factory: IMFReadWriteClassFactory;

begin
  hr := E_INVALIDARG;
  factory := CreateCOMObject(CLSID_MFReadWriteClassFactory) as IMFReadWriteClassFactory;

  if (clsidObject = CLSID_MFSinkWriter) then
    begin
      hr := factory.CreateInstanceFromObject(clsidObject,
                                             initSource,
                                             attributes,
                                             IID_IMFMediaSink,
                                             Pointer(iunkObject));
    end
  else if (clsidObject = CLSID_MFSourceReader) then
    begin
      hr := factory.CreateInstanceFromObject(clsidObject,
                                             initSource,
                                             attributes,
                                             CLSID_MFSourceReader,
                                             Pointer(iunkObject));
    end;

  Result := hr;
end;


// This method returns a video activation object that is able to render to a window or
// any other visual component that has a THandle (HWND).
function CreateVideoMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                      hVideoWnd: HWND;
                                      out mfActivate: IMFActivate): HResult;
var
  phandler: IMFMediaTypeHandler;
  pActivate: IMFActivate;
  guidMajorType: TGUID;
  hr: HResult;

label
  done;

begin
  // Get the media type handler for the stream
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  // Get the major media type
  hr := pHandler.GetMajorType(guidMajorType);
  if FAILED(hr) then
    goto done;

  // Create an IMFActivate object for the renderer, based on the media type
  if IsEqualGuid(MFMediaType_Video,
                 guidMajorType) then
    begin
      hr := MFCreateVideoRendererActivate(hVideoWnd,
                                          pActivate);
      if FAILED(hr) then
       goto done;


    end
  else
    hr := MF_E_CAPTURE_SOURCE_NO_VIDEO_STREAM_PRESENT;

  if FAILED(hr) then
    goto done;

  // Return IMFactivate pointer to caller
  mfActivate := pActivate;
done:
  Result := hr;
end;


// This method returns an audio activation object for a renderer.
function CreateAudioMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                      out mfActivate: IMFActivate): HResult;
var
  phandler: IMFMediaTypeHandler;
  pActivate: IMFActivate;
  guidMajorType: TGUID;
  hr: HResult;

label
  done;

begin
  // Get the media type handler for the stream
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  // Get the major media type
  hr := pHandler.GetMajorType(guidMajorType);
  if FAILED(hr) then
    goto done;

  // Create an IMFActivate object for the renderer, based on the media type
  if IsEqualGuid(MFMediaType_Audio,
                guidMajorType) then
    hr := MFCreateAudioRendererActivate(pActivate)
  else
    hr := MF_E_CAPTURE_SOURCE_NO_AUDIO_STREAM_PRESENT;

  if FAILED(hr) then
    goto done;

  // Return IMFactivate pointer to caller
  mfActivate := pActivate;

done:
  Result := hr;
end;


//
function AddSourceStreamNode(pSource: IMFMediaSource;
                             pSourcePD: IMFPresentationDescriptor;
                             pSourceSD: IMFStreamDescriptor;
                             out ppNode: IMFTopologyNode): HResult;
var
   hr: HResult;

label
  done;

begin

  if (not Assigned(pSource) or
      not Assigned(pSourcePD) or
      not Assigned(pSourceSD)) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Create the source-stream node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                             ppNode);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the media source.
  hr := ppNode.SetUnknown(MF_TOPONODE_SOURCE,
                          pSource);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the presentation descriptor.
  hr := ppNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                          pSourcePD);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the stream descriptor.
  hr := ppNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                          pSourceSD);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


// Create a playback topology from a media source.
function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                hVideoWnd: HWND;                       // Video window.
                                var ppTopology: IMFTopology;           // Receives a pointer to the topology.
                                dwSourceStreams: DWORD = 0): HResult;
var
  tmpTopology: IMFTopology;
  hr: HResult;
  i: integer;

label
  done;

begin

  hr := MFCreateTopology(tmpTopology);
  if (FAILED(hr)) then
    goto done;

  // Get the number of streams in the media source.
  hr := pPD.GetStreamDescriptorCount(dwSourceStreams);
  if (FAILED(hr)) then
    goto done;

  // For each stream, create the topology nodes and add them to the topology.
  for i := 0 to dwSourceStreams - 1 do
    begin
      hr := AddBranchToPartialTopology(tmpTopology,
                                       pSource,
                                       pPD,
                                       i,
                                       hVideoWnd);
      if (FAILED(hr)) then
        goto done;

      hr := pPD.SelectStream(i);
    end;

  ppTopology := tmpTopology;
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


// Add a source node to a topology.
function AddSourceNode(pTopology: IMFTopology;                   // Topology.
                       pSource: IMFMediaSource;                  // Media source.
                       pPD: IMFPresentationDescriptor;           // Presentation descriptor.
                       pSD: IMFStreamDescriptor;                 // Stream descriptor.
                       out ppNode: IMFTopologyNode): HResult;    // Receives the node pointer.

var
  hr: HResult;

label
  done;

begin
  ppNode := nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                             ppNode);
  if (FAILED(hr)) then
    goto done;

  // Set the attributes.
  hr := ppNode.SetUnknown(MF_TOPONODE_SOURCE,
                          pSource);
  if (FAILED(hr)) then
    goto done;

  hr := ppNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                          pPD);
  if (FAILED(hr)) then
    goto done;

  hr := ppNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                          pSD);
  if (FAILED(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(ppNode);

  if (FAILED(hr)) then
    goto done;

done:
   Result := hr;
end;


// Add an output node to a topology.
function AddOutputNodeA(pTopology: IMFTopology;                 // Topology.
                        pActivate: IMFActivate;                 // Media sink activation object.
                        dwId: DWORD;                            // Identifier of the stream sink.
                        out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.
var
  hr: HResult;

label
  done;

begin

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             ppNode);
  if (FAILED(hr)) then
    goto done;

  // Set the object pointer.
  hr := ppNode.SetObject(pActivate);
  if (FAILED(hr)) then
    goto done;

  // Set the stream sink ID attribute.
  hr := ppNode.SetUINT32(MF_TOPONODE_STREAMID,
                         dwId);
  if (FAILED(hr)) then
    goto done;

  hr := ppNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                         0);
  if (FAILED(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(ppNode);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


function AddOutputNodeS(pTopology: IMFTopology;                   // Topology.
                        pStreamSink: IMFStreamSink;               // Stream sink.
                        out ppNode: IMFTopologyNode): HResult;    // Receives the node pointer.
var
  hr: HResult;

begin
  ppNode := nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             ppNode);

  // Set the object pointer.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetObject(pStreamSink);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                             0);
    end;

  Result := hr;
end;


//  Add a topology branch for one stream.
//
//  For each stream, this function does the following:
//
//    1. Creates a source node associated with the stream.
//    2. Creates an output node for the renderer.
//    3. Connects the two nodes.
//
//  The media session will add any decoders that are needed.
function AddBranchToPartialTopology(pTopology: IMFTopology;
                                    pSource: IMFMediaSource;
                                    pPD: IMFPresentationDescriptor;
                                    dwStream: DWord;
                                    hVideoWnd: HWND): HResult;
var
  pSD: IMFStreamDescriptor;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  fSelected: BOOL;
  hr: HResult;

label
  done;

begin
  // Use assertions only for debugging purposes
  {$IF DEBUG}
  assert(pTopology <> nil);
  {$ENDIF}

  // Get the stream descriptor for this stream.
  hr := pPD.GetStreamDescriptorByIndex(dwStream,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  // Create the topology branch only if the stream is selected.
  // Otherwise, do nothing.
  if fSelected then
    begin
      // create the media sink activation object
      hr := CreateVideoMediaSinkActivate(pSD,
                                         hVideoWnd,
                                         pSinkActivate);

      if (hr = MF_E_CAPTURE_SOURCE_NO_VIDEO_STREAM_PRESENT) then
        hr := CreateAudioMediaSinkActivate(pSD,
                                           pSinkActivate);

      if FAILED(hr) then
        goto done;

      // Create a source node for this stream.
      hr := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);
      if (FAILED(hr)) then
        goto done;

      // Create the output node for the renderer.
      hr := AddOutPutNodeA(pTopology,
                           pSinkActivate,
                           0,
                           pOutPutNode);
      if (FAILED(hr)) then
        goto done;

      // Connect the source node to the output node.
      hr := pSourceNode.ConnectOutput(0,
                                      pOutputNode,
                                      0);
    end;

done:
  Result := hr;
end;

//
function AddBranchToPartialTopologyWithDecoder(pTopology: IMFTopology;          // Topology.
                                               pSource: IMFMediaSource;         // Media source.
                                               pPD: IMFPresentationDescriptor;  // Presentation descriptor.
                                               iStream: DWORD;                  // Stream index.
                                               hVideoWnd: HWND): HResult;       // Window for video playback.
var
  hr: HResult;
  pSD: IMFStreamDescriptor;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  pDecoderNode: IMFTopologyNode;

  fSelected: BOOL;
  clsidDecoder: CLSID;

begin

  fSelected := False;
  clsidDecoder := GUID_NULL;

  // Get the stream descriptor.
  hr := pPD.GetStreamDescriptorByIndex(iStream,
                                       fSelected,
                                       pSD);
  if (FAILED(hr)) then
    begin
      Result := hr;
      Exit;
    end;

  if (fSelected = True) then
    begin
      // Add a source node for this stream.
      hr := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);

      // Create the media sink activation object.
      if (SUCCEEDED(hr)) then
        begin
          hr := CreateVideoMediaSinkActivate(pSD,
                                             hVideoWnd,
                                             pSinkActivate);

          if hr = MF_E_CAPTURE_SOURCE_NO_VIDEO_STREAM_PRESENT then
            hr := CreateAudioMediaSinkActivate(pSD,
                                               pSinkActivate);
        end;

      // Create the output node for the renderer.
      if (SUCCEEDED(hr)) then
        begin
          hr := AddOutputNodeA(pTopology,
                               pSinkActivate,
                               0,
                               pOutputNode);
        end;

      // Find a decoder.
      if (SUCCEEDED(hr)) then
        begin
          hr := FindDecoderForStream(pSD,
                                     clsidDecoder);
        end;

      if (SUCCEEDED(hr)) then
        begin
          if (clsidDecoder = GUID_NULL) then
            begin
              // No decoder is required.
              // Connect the source node to the output node.
              hr := pSourceNode.ConnectOutput(0,
                                              pOutputNode,
                                              0);
            end
          else
            begin
              // Add a decoder node.
              hr := AddTransformNodeC(pTopology,
                                      clsidDecoder,
                                      pDecoderNode);

              // Connect the source node to the decoder node.
              if (SUCCEEDED(hr)) then
                begin
                  hr := pSourceNode.ConnectOutput(0,
                                                  pDecoderNode,
                                                  0);
                end;

              // Connect the decoder node to the output node.
              if (SUCCEEDED(hr)) then
                begin
                  hr := pDecoderNode.ConnectOutput(0,
                                                   pOutputNode,
                                                   0);
                end;
            end;
        end;

      // Mark this branch as not requiring a decoder.
      if (SUCCEEDED(hr)) then
        begin
          hr := pOutputNode.SetUINT32(MF_TOPONODE_CONNECT_METHOD,
                                      UINT32(MF_CONNECT_ALLOW_CONVERTER));
        end;

      if (SUCCEEDED(hr)) then
        begin
          hr := pDecoderNode.SetUINT32(MF_TOPONODE_CONNECT_METHOD,
                                       UINT32(MF_CONNECT_ALLOW_CONVERTER));
        end;

    end;
    // else: If not selected, don't add the branch.

  Result := hr;
end;


//
function CreateOutputNode(pSourceSD: IMFStreamDescriptor;
                          hwndVideo: HWND;
                          out ppNode: IMFTopologyNode): HResult;
var
  pNode: IMFTopologyNode;
  pHandler: IMFMediaTypeHandler;
  pRendererActivate: IMFActivate;
  guidMajorType: TGUID;
  streamID: DWORD;
  hr: HResult;

label
  done;

begin

  // Get the stream ID.
  streamID := 0;

  // Just for debugging, ignore any failures.
  hr := pSourceSD.GetStreamIdentifier(streamID);
  if (FAILED(hr)) then
    goto done;

  // Get the media type handler for the stream.
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if (FAILED(hr)) then
    goto done;

  // Get the major media type.
  hr := pHandler.GetMajorType(guidMajorType);
  if (FAILED(hr)) then
    goto done;

  // Create a downstream node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             pNode);
  if (FAILED(hr)) then
    goto done;

  // Create an IMFActivate object for the renderer, based on the media type.
  if MFMediaType_Audio = guidMajorType then
    begin
      // Create the audio renderer.
      hr := MFCreateAudioRendererActivate(pRendererActivate);
    end
  else
    if (MFMediaType_Video = guidMajorType) then
      begin
        // Create the video renderer.
        hr := MFCreateVideoRendererActivate(hwndVideo, pRendererActivate);
      end
    else
      hr := E_FAIL;

  if (FAILED(hr)) then
    goto done;

  // Set the IActivate object on the output node.
  hr := pNode.SetObject(pRendererActivate);
  if (FAILED(hr)) then
    goto done;

  // Return the IMFTopologyNode pointer to the caller.
  ppNode := pNode;
  
done:
  Result := hr;
end;


function AddTransformNodeM(pTopology: IMFTopology;      // Topology.
                           pMFT: IMFTransform;          // MFT.
                           out ppNode: IMFTopologyNode): HResult;
var
  hr: HResult;

begin
  ppNode := nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                             ppNode);

  // Set the object pointer.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetObject(pMFT);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  Result := hr;
end;


function AddTransformNodeC(pTopology: IMFTopology;                 // Topology.
                           const fclsid: CLSID;                    // CLSID of the MFT.
                           out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.
var
  hr: HResult;

begin
  ppNode := nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                             ppNode);

  // Set the CLSID attribute.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetGUID(MF_TOPONODE_TRANSFORM_OBJECTID,
                           fclsid);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  Result := hr;
end;


function AddTransformNodeA(pTopology: IMFTopology;                 // Topology.
                           pActivate: IMFActivate;                 // MFT activation object.
                           out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.
var
  hr: HResult;

begin
  ppNode := nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                             ppNode);

  // Set the object pointer.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetObject(pActivate);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  Result := hr;
end;


// Given a topology (pTopology), returns a pointer to the presentation descriptor.
function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                               out ppPD: IMFPresentationDescriptor): HResult;
var
  hr: HResult;
  pCollection: IMFCollection;
  pUnk: IUnknown;
  pNode: IMFTopologyNode;
  dwElementcount: DWord;
  dwIndex: DWORD;

label
  done;

begin

  dwIndex := 0;

  // Get the collection of source nodes from the topology.
  hr := pTopology.GetSourceNodeCollection(pCollection);
  if FAILED(hr) then
    goto done;

  // Any of the source nodes should have the PD, so take the first
  // object in the collection.
  hr := pCollection.GetElementCount(dwElementcount);
  if FAILED(hr) then
    begin
      OleCheck(hr);
      goto done;
    end;

  if (dwElementcount > 0) then
    begin
      hr := pCollection.GetElement(dwIndex,
                                   pUnk);
      if FAILED(hr) then
        begin
          OleCheck(hr);
          goto done;
        end;
    end
  else
    begin
      hr := MF_E_NOT_FOUND;
      goto done;
    end;

  hr := pUnk.QueryInterface(IID_IMFTopologyNode,
                            pNode);
    if FAILED(hr) then
      goto done;

  // Get the PD, which is stored as an attribute.
  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         Pointer(ppPD));

done:
  Result := hr;
end;

//
function GetDurationFromTopology(pTopology: IMFTopology;
                                 out phnsDuration: LONGLONG): HResult;
var
  pSourceNodes: IMFCollection;
  pNode: IMFTopologyNode;
  pPD: IMFPresentationDescriptor;
  hr: HResult;

label
  done;

begin
  phnsDuration := 0;

  hr := pTopology.GetSourceNodeCollection(pSourceNodes);
  if FAILED(hr) then
    goto done;

  hr := GetCollectionObject(pSourceNodes,
                            0,
                            pNode);
  if FAILED(hr) then
    goto done;

  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         Pointer(pPD));
  if FAILED(hr) then
    goto done;

  phnsDuration := MFGetAttributeUINT64(pPD,
                                       MF_PD_DURATION,
                                       0);

done:
  Result := hr;
end;


// Gets an interface pointer from a Media Foundation collection.
function GetCollectionObject(pCollection: IMFCollection;
                             const dwIndex: DWORD;
                             out ppObject): HResult;
var
  pUnk: IUnknown;
  hr: HResult;

begin

  hr := pCollection.GetElement(dwIndex,
                               pUnk);
  if SUCCEEDED(hr) then
    begin
      hr := pUnk.QueryInterface(IID_IUnknown,
                                ppObject);
    end;

  Result := hr;
end;



// SCRUBBING
function DoScrub(const SeekTime: MFTIME;
                 pMediaSession: IMFMediaSession): HResult;
var
  hr: HResult;
  pvar: PROPVARIANT;
  pRateControl: IMFRateControl;

begin
  pRateControl := nil;
  PropVariantInit(pvar);

  // Get the rate control service.
  hr := MFGetService(pMediaSession,
                     MF_RATE_CONTROL_SERVICE,
                     IID_IMFRateControl,
                     Pointer(pRateControl));

  // Set the playback rate to zero without thinning.
  if (SUCCEEDED(hr)) then
    hr := pRateControl.SetRate(Boolean(0),
                               0.0);

  // Create the Media Session start position.
  if (SeekTime = PRESENTATION_CURRENT_POSITION)then
    pvar.vt := VARTYPE(VT_EMPTY)
  else
    begin
      pvar.vt := VARTYPE(VT_I8);
      pvar.hVal.QuadPart := SeekTime;
    end;

  // Start the Media Session.
  if (SUCCEEDED(hr)) then
    hr := pMediaSession.Start(GUID_NULL,
                              pvar);

  // Clean up.
  PropVariantClear(pvar);
  Result := hr;
end;


///////////////////////////////////////////////////////////////////////
//  Name: SetPlaybackRate
//  Description:
//      Gets the rate control service from Media Session.
//      Sets the playback rate to the specified rate.
//  Parameter:
//      pMediaSession: [in] Media session object to query.
//      rateRequested: [in] Playback rate to set.
//      bThin: [in] Indicates whether to use thinning.
///////////////////////////////////////////////////////////////////////

function SetPlaybackRate(pMediaSession: IMFMediaSession;
                         const rateRequested: MFTIME;
                         const bThin: Boolean): HResult;
var
  hr: HResult;
  pRateControl: IMFRateControl;

begin

  pRateControl := nil;

  // Get the rate control object from the Media Session.
  hr := MFGetService(pMediaSession,
                     MF_RATE_CONTROL_SERVICE,
                     IID_IMFRateControl,
                     Pointer(pRateControl));

  // Set the playback rate.
  if (SUCCEEDED(hr)) then
    hr := pRateControl.SetRate(bThin,
                               rateRequested);

  Result := hr;
end;

// Before you queue a topology for playback, you can specify the stop time by using
// the MF_TOPONODE_MEDIASTOP attribute.
// For each output node in the topology, set the value of the MF_TOPONODE_MEDIASTOP
// to the stop time in 100-nanosecond units.
// Note that setting this attribute after playback starts has no effect.
// Therefore, set the attribute before calling IMFMediaSession.Start.
// The following code shows how to set the stop time on an existing topology.
// Note:
//   Instead of creating a class (like the MS example) we can do
//   with a nested function.
//
function SetMediaStop(pTopology: IMFTopology;
                      stop: LONGLONG): HResult;

  function GetCollectionObject(pCollection: IMFCollection;
                               dwIndex: DWORD;
                               ppObject: Pointer): HResult;
  var
    pUnk: IUnknown;
    hr: HResult;

  begin
    ppObject := nil;   // zero output
    pUnk := nil;

    hr := pCollection.GetElement(dwIndex,
                                 pUnk);
    if SUCCEEDED(hr) then
      begin
        hr := pUnk.QueryInterface(IID_IMFTopologyNode,
                                  ppObject);
        SafeRelease(pUnk);
      end;
    Result := hr;
  end;

var
  pCol: IMFCollection;
  cNodes: DWORD;
  hr: HResult;
  i: Integer;
  pNode: IMFTopologyNode;

begin

  hr := pTopology.GetSourceNodeCollection(pCol);
  if SUCCEEDED(hr) then
    begin
      hr := pCol.GetElementCount(cNodes);
    end;

  if SUCCEEDED(hr) then
    begin
      for i := 0 to cNodes - 1 do
        begin
          hr := GetCollectionObject(pCol,
                                    i,
                                    @pNode);
          if SUCCEEDED(hr) then
            begin
              pNode.SetUINT64(MF_TOPONODE_MEDIASTOP,
                              stop);
            end;
          SafeRelease(pNode);
        end;
    end;
  Result := hr;
end;

// Important
//  This interface has a serious limitation, because the stop time is specified as a 32-bit value.
//  That means the maximum stop time that you can set using this interface is $FFFFFFFF,
//  or just over 7 minutes. This limitation is due to an incorrect structure definition.
//
// To set the stop time using the IMFTopologyNodeAttributeEditor interface, perform the following steps.
// 1 Call MFGetService to get the IMFTopologyNodeAttributeEditor interface from the Media Session.
// 2 Call IMFTopology.GetTopologyID to get the ID of the playback topology.
// 3 For each output node in the topology, call IMFTopologyNodeAttributeEditor.UpdateNodeAttributes.
//   This method takes the topology ID and a pointer to a MFTOPONODE_ATTRIBUTE_UPDATE structure.
//   Initialize the structure as follows.
//
//    Member             Value
//    ------------------ ------------------------------------------------------------------------------
//    NodeId             The node ID. To get the node ID, call call IMFTopologyNode.GetTopoNodeID.
//    guidAttributeKey   MF_TOPONODE_MEDIASTOP
//    attrType           MF_ATTRIBUTE_UINT64
//    u64                The stop time, in 100-nanosecond units.
//
function SetMediaStopDynamic(pSession: IMFMediaSession;
                             pTopology: IMFTopology;
                             stop: LONGLONG): HResult;
const
  {$IFDEF WIN32}
  MAXVALUE = 4294967294;  // UINT32 0..4294967295 on 32 bit platforms.
  {$ELSE}
  MAXVALUE = 9223372036854775806;  // Int64 on 64 bit platforms.
  {$ENDIF}

var
  pAttr: IMFTopologyNodeAttributeEditor;
  pCol: IMFCollection;
  pNode: IMFTopologyNode;
  hr: HResult;
  id: TOPOID;
  nodeID: TOPOID;
  cNodes: DWORD;
  i: Integer;
  update: PMFTOPONODE_ATTRIBUTE_UPDATE;

label
  done;

begin

  {$IFDEF WIN32}
  if (stop > MAXUINT32) then
    stop := MAXUINT32;
  {$ELSE}
  if (stop > MAXUINT64) then
    stop := MAXUINT64;
  {$ENDIF}

  update := nil;

  hr := MFGetService(pSession,
                     MF_TOPONODE_ATTRIBUTE_EDITOR_SERVICE,
                     IID_IMFTopologyNodeAttributeEditor,
                     Pointer(pAttr));
  if FAILED(hr) then
    goto done;

  hr := pTopology.GetTopologyID(id);
  if FAILED(hr) then
    goto done;

  hr := pTopology.GetSourceNodeCollection(pCol);
  if FAILED(hr) then
    goto done;

  hr := pCol.GetElementCount(cNodes);
  if FAILED(hr) then
    goto done;

  for i := 0 to cNodes - 1 do
    begin
      hr := GetCollectionObject(pCol,
                                i,
                                pNode);
      if FAILED(hr) then
        goto done;

      hr := pNode.GetTopoNodeID(nodeID);

      if SUCCEEDED(hr) then
        begin
          update^.NodeId := nodeID;
          update^.guidAttributeKey := MF_TOPONODE_MEDIASTOP;
          update^.attrType := MF_ATTRIBUTE_UINT64;
          // Be careful to set the value of attrType correctly.
          // Although u32 is a 32-bit type, the method requires that attrType be set to MF_ATTRIBUTE_UINT64.
          {$IFDEF WIN32}
          update^.u32 := UINT32(stop); // ! See Remarks !
          {$ELSE} // Win64
          update^.u64 := UINT64(stop);
          {$ENDIF}

          hr := pAttr.UpdateNodeAttributes(id,
                                           1,
                                           update);
          if FAILED(hr) then
            goto done;
        end;

      SafeRelease(pNode);

      if FAILED(hr) then
        goto done;
    end;

done:
  Result := hr;
end;


// This function is deprecated. Only here for backward compatibility.
function FindDecoderEx(const subtype: TGUID;         // Subtype
                       bAudio: Boolean;              // TRUE for audio, FALSE for video
                       out ppDecoder: IMFTransform): HResult;
var
  mftRegisterTypeInfo: MFT_REGISTER_TYPE_DESCR;

begin
  if bAudio then
    begin
      mftRegisterTypeInfo.RegisterTypeInfo.guidMajorType := MFMediaType_Audio;
      mftRegisterTypeInfo.RegisterTypeInfo.guidSubtype := subtype;

      Result := GetCodec(nil,
                         @mftRegisterTypeInfo,
                         MFT_CATEGORY_AUDIO_DECODER,
                         ppDecoder);
    end
  else
    begin
      mftRegisterTypeInfo.RegisterTypeInfo.guidMajorType := MFMediaType_Video;
      mftRegisterTypeInfo.RegisterTypeInfo.guidSubtype := subtype;
      // Get readable type info
      mftRegisterTypeInfo.majorTypeDescr := GetMajorTypeDescr(MFMediaType_Video);

      Result := GetCodec(nil,
                         @mftRegisterTypeInfo,
                         MFT_CATEGORY_VIDEO_DECODER,
                         ppDecoder);
    end;
end;


// This function is deprecated. Only here for backward compatibility.
function FindEncoderEx(const subtype: TGUID;
                       const bAudio: Boolean;
                       out ppEncoder: IMFTransform): HResult;
var
  mftRegisterTypeInfo: MFT_REGISTER_TYPE_INFO;

begin

  if bAudio then
    begin
      mftRegisterTypeInfo.guidMajorType := MFMediaType_Audio;
      mftRegisterTypeInfo.guidSubtype := subtype;

      Result := GetCodec(@mftRegisterTypeInfo,
                         nil,
                         MFT_CATEGORY_AUDIO_ENCODER,
                         ppEncoder);
    end
  else
    begin
      mftRegisterTypeInfo.guidMajorType := MFMediaType_Video;
      mftRegisterTypeInfo.guidSubtype := subtype;

      Result := GetCodec(@mftRegisterTypeInfo,
                         nil,
                         MFT_CATEGORY_VIDEO_ENCODER,
                         ppEncoder);
    end;
end;


//
function EnumMft(const mftCategory: TGuid;
                 mftInput: PMFT_REGISTER_TYPE_INFO;
                 mftOutput: PMFT_REGISTER_TYPE_INFO;
                 out ppActivate: PIMFActivate;
                 out count: UINT32): HResult;
const
  unFlags = MFT_ENUM_FLAG_SYNCMFT or
            MFT_ENUM_FLAG_LOCALMFT or
            MFT_ENUM_FLAG_SORTANDFILTER;
var
  hr: HResult;

begin
  hr := MFTEnumEx(mftCategory,
                  unFlags,
                  mftInput,     // Input type
                  mftOutput,    // Output type
                  ppActivate,
                  count);

  if (SUCCEEDED(hr) and (count = 0)) then
    begin
      hr := MF_E_TOPO_CODEC_NOT_FOUND;
    end;

  Result := hr;
end;


//
function GetMftEnumInfo(const MftClsid: CLSID;
                        out mftEnum: TMftEnumInfo): HResult;
var
  hr: HResult;
  i: Integer;
  ppInputTypes: PMFT_REGISTER_TYPE_INFO;
  pcInputTypes: UINT32;
  ppOutputTypes: PMFT_REGISTER_TYPE_INFO;
  pcOutputTypes: UINT32;

begin
  // check for valid enum ID

  hr := MFTGetInfo(MftClsid,
                   mftEnum.pwcStr,
                   ppInputTypes,
                   pcInputTypes,
                   ppOutputTypes,
                   pcOutputTypes,
                   mftEnum.pAttributes);

  if FAILED(hr) then
    begin
      Result := hr;
      Exit;
    end;

  SetLength(mftEnum.aInputTypes,
            pcInputTypes);
  SetLength(mftEnum.aOutputTypes,
            pcOutputTypes);

{$POINTERMATH ON}

  // Get the supported input types.
  for i := 0 to pcInputTypes -1 do
    begin
      mftEnum.aInputTypes[i].RegisterTypeInfo := ppInputTypes[i];
      // Get the descriptions of the input formats.
      // Get majortype info
      GetGUIDNameConst(mftEnum.aInputTypes[i].RegisterTypeInfo.guidMajorType,
                       mftEnum.aInputTypes[i].RegisterTypeInfo.guidSubtype,
                       mftEnum.aInputTypes[i].GuidName,
                       mftEnum.aInputTypes[i].FormatTag,
                       mftEnum.aInputTypes[i].FOURCC,
                       mftEnum.aInputTypes[i].majorTypeDescr);
      // Get subtype info
      GetGUIDNameConst(mftEnum.aInputTypes[i].RegisterTypeInfo.guidMajorType,
                       mftEnum.aInputTypes[i].RegisterTypeInfo.guidSubtype,
                       mftEnum.aInputTypes[i].GuidName,
                       mftEnum.aInputTypes[i].FormatTag,
                       mftEnum.aInputTypes[i].FOURCC,
                       mftEnum.aInputTypes[i].subTypeDescr);
    end;

  // Get the supported output types.
  for i := 0 to pcOutputTypes -1 do
    begin
      mftEnum.aOutputTypes[i].RegisterTypeInfo := ppOutputTypes[i];
      // Get the descriptions of the output formats.
      GetGUIDNameConst(mftEnum.aOutputTypes[i].RegisterTypeInfo.guidMajorType,
                       mftEnum.aOutputTypes[i].RegisterTypeInfo.guidSubtype,
                       mftEnum.aOutputTypes[i].GuidName,
                       mftEnum.aOutputTypes[i].FormatTag,
                       mftEnum.aOutputTypes[i].FOURCC,
                       mftEnum.aOutputTypes[i].subTypeDescr);
    end;

  CoTaskMemFree(ppInputTypes);
  CoTaskMemFree(ppOutputTypes);

  Result := hr;
end;


function GetCodec(mftRegisterTypeInput: PMFT_REGISTER_TYPE_INFO;
                  mftRegisterTypeOutput: PMFT_REGISTER_TYPE_INFO;
                  const mftCategory: TGuid;
                  out mftCodec: IMFTransform;
                  flags: MFT_ENUM_FLAG = MFT_ENUM_FLAG_ALL;
                  selIndex: Integer = 0): HResult;

var
  hr: HResult;
  count: UINT32;
  ppActivate: PIMFActivate;
  attr: IMFAttributes;

begin
  hr := S_OK;
try

  hr := MFTEnumEx(mftCategory,
                  flags,  // default = MFT_ENUM_FLAG_ALL
                  mftRegisterTypeInput,  // Input type
                  mftRegisterTypeOutput, // Output type
                  ppActivate,  // Array of IMFActivate pointers
                  count);      // number of pointers returned.

  if (SUCCEEDED(hr) and (count = 0)) then
    begin
      hr := MF_E_TOPO_CODEC_NOT_FOUND;
    end;

{$POINTERMATH ON}
  // Create the first decoder in the list.
  // Note: call function EnumMft to get the number of mft's, so you may select one from the index.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppActivate[selIndex].ActivateObject(IID_IMFTransform,
                                                Pointer(mftCodec));
      mftCodec.GetAttributes(attr);
      //attr.GetString()
    end;

finally
  CoTaskMemFree(ppActivate);
  Result := hr;
end;
end;


// This function is deprecated. Only here for backward compatibility.
function FindVideoDecoder(const subtype: TGUID;
                          bAllowAsync: Boolean;
                          bAllowHardware: Boolean;
                          bAllowTranscode: Boolean;
                          out ppDecoder: IMFTransform): HResult;
var
  mftRegisterTypeInfo: MFT_REGISTER_TYPE_INFO;

begin
  mftRegisterTypeInfo.guidMajorType := MFMediaType_Video;
  mftRegisterTypeInfo.guidSubtype := subtype;

  Result := GetCodec(@mftRegisterTypeInfo,
                     nil,
                     MFT_CATEGORY_VIDEO_DECODER,
                     ppDecoder);
end;


//
function GetDecoderCategory(const majorType: TGUID;
                            out pCategory: TGUID): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  if (majorType = MFMediaType_Video) then
    begin
      pCategory := MFT_CATEGORY_VIDEO_DECODER;
    end
  else if (majorType = MFMediaType_Audio) then
    begin
      pCategory := MFT_CATEGORY_AUDIO_DECODER;
    end
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE;
    end;

  Result := hr;
end;


function GetEncoderCategory(const majorType: TGUID;
                            out pCategory: TGUID): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  if (majorType = MFMediaType_Video) then
    begin
      pCategory := MFT_CATEGORY_VIDEO_ENCODER;
    end
  else if (majorType = MFMediaType_Audio) then
    begin
      pCategory := MFT_CATEGORY_AUDIO_ENCODER;
    end
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE;
    end;

  Result := hr;
end;


function GetMftCategories(): TArray<TMftCategory>;
begin
  SetLength(Result, 12);

  // video
  Result[0].CategoryGuid := MFT_CATEGORY_VIDEO_DECODER;
  Result[0].CategoryGuidName := 'MFT_CATEGORY_VIDEO_DECODER';
  Result[0].CategoryDescr := 'Video decoder MFTs.';
  Result[1].CategoryGuid := MFT_CATEGORY_VIDEO_ENCODER;
  Result[1].CategoryGuidName := 'MFT_CATEGORY_VIDEO_ENCODER';
  Result[1].CategoryDescr := 'Video encoder MFTs.';
  Result[2].CategoryGuid := MFT_CATEGORY_VIDEO_PROCESSOR;
  Result[2].CategoryGuidName := 'MFT_CATEGORY_VIDEO_PROCESSOR';
  Result[2].CategoryDescr := 'Video processor MFTs.';
  Result[3].CategoryGuid := MFT_CATEGORY_VIDEO_EFFECT;
  Result[3].CategoryGuidName := 'MFT_CATEGORY_VIDEO_EFFECT';
  Result[3].CategoryDescr := 'Video effect MFTs.';
  Result[4].CategoryGuid := MFT_CATEGORY_VIDEO_RENDERER_EFFECT;
  Result[4].CategoryGuidName := 'MFT_CATEGORY_VIDEO_RENDERER_EFFECT';
  Result[4].CategoryDescr := 'Video renderer effect MFTs.';

  // Audio
  Result[5].CategoryGuid := MFT_CATEGORY_AUDIO_DECODER;
  Result[5].CategoryGuidName := 'MFT_CATEGORY_AUDIO_DECODER';
  Result[5].CategoryDescr := 'Audio decoder MFTs.';
  Result[6].CategoryGuid := MFT_CATEGORY_AUDIO_ENCODER;
  Result[6].CategoryGuidName := 'MFT_CATEGORY_AUDIO_ENCODER';
  Result[6].CategoryDescr := 'Audio encoder MFTs.';
  Result[7].CategoryGuid := MFT_CATEGORY_AUDIO_EFFECT;
  Result[7].CategoryGuidName := 'MFT_CATEGORY_AUDIO_EFFECT';
  Result[7].CategoryDescr := 'Audio effect MFTs.';

  // Multiplexers and de-multiplexers
  Result[8].CategoryGuid := MFT_CATEGORY_DEMULTIPLEXER;
  Result[8].CategoryGuidName := 'MFT_CATEGORY_DEMULTIPLEXER';
  Result[8].CategoryDescr := 'Demultiplexer MFTs.';
  Result[9].CategoryGuid := MFT_CATEGORY_MULTIPLEXER;
  Result[9].CategoryGuidName := 'MFT_CATEGORY_MULTIPLEXER';
  Result[9].CategoryDescr := 'Multiplexer MFTs.';

  // Encryptors
  Result[10].CategoryGuid := MFT_CATEGORY_ENCRYPTOR;
  Result[10].CategoryGuidName := 'MFT_CATEGORY_ENCRYPTOR';
  Result[10].CategoryDescr := 'Encryptor MFTs.';

  // Others
  Result[11].CategoryGuid := MFT_CATEGORY_OTHER;
  Result[11].CategoryGuidName := 'MFT_CATEGORY_OTHER';
  Result[11].CategoryDescr := 'Miscellaneous MFTs.';

end;


//
function FindDecoderForStream(pSD: IMFStreamDescriptor; // Stream descriptor for the stream.
                              out opCLSID: CLSID): HResult;  // Receives the CLSID of the decoder.
var
  hr: HResult;
  bIsCompressed: BOOL;
  guidMajorType: TGUID;
  guidSubtype: TGUID;
  guidDecoderCategory: TGUID;
  ppDecoderCLSIDs: array of CLSID;
  cDecoderCLSIDs: UINT32;    // Size of the array.
  pHandler: IMFMediaTypeHandler;
  pMediaType: IMFMediaType;
  mftinfo: MFT_REGISTER_TYPE_INFO;

begin
  bIsCompressed := False;
  guidMajorType := GUID_NULL;
  guidSubtype := GUID_NULL;
  guidDecoderCategory := GUID_NULL;
  cDecoderCLSIDs := 0;


  // Find the media type for the stream.
  hr := pSD.GetMediaTypeHandler(pHandler);

  if (SUCCEEDED(hr)) then
    begin
      hr := pHandler.GetCurrentMediaType(pMediaType);
    end;

  // Get the major type and subtype.
  if (SUCCEEDED(hr)) then
    begin
      hr := pMediaType.GetMajorType(guidMajorType);
    end;

  if (SUCCEEDED(hr)) then
    begin
      hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                               guidSubtype);
    end;

  // Check whether the stream is compressed.
  if (SUCCEEDED(hr)) then
    begin
      hr := pMediaType.IsCompressedFormat(bIsCompressed);
    end;

//{$if WINVER < _WIN32_WINNT_WIN7}

  // Starting in Windows 7, you can connect an uncompressed video source
  // directly to the EVR. In earlier versions of Media Foundation, this
  // is not supported.

  //if (SUCCEEDED(hr)) then
  //  begin
  //    if ((bIsCompressed = True) And (guidMajorType = MFMediaType_Video)) then
  //      begin
  //        hr := MF_E_INVALIDMEDIATYPE;
  //      end;
  //  end;

//{$endif}

  // If the stream is compressed, find a decoder.
  if (SUCCEEDED(hr)) then
    begin
      if (bIsCompressed) then
        begin
          // Select the decoder category from the major type (audio/video).
          hr := GetDecoderCategory(guidMajorType,
                                   guidDecoderCategory);

          // Look for a decoder.
          if (SUCCEEDED(hr)) then
            begin
              mftinfo.guidMajorType := guidMajorType;
              mftinfo.guidSubtype := guidSubtype;

              hr := MFTEnum(guidDecoderCategory,
                            0,                   // Reserved
                            @mftinfo,            // Input type to match. (Encoded type.)
                            nil,                 // Output type to match. (Don't care.)
                            nil,                 // Attributes to match. (None.)
                            @ppDecoderCLSIDs[0], // Receives an array of CLSIDs.
                            cDecoderCLSIDs);     // Receives the size of the array.
            end;

          // MFTEnum can return zero matches.
          if (SUCCEEDED(hr) and (cDecoderCLSIDs = 0)) then
            begin
              hr := MF_E_TOPO_CODEC_NOT_FOUND;
            end;

          // Return the first CLSID in the list to the caller.
          if (SUCCEEDED(hr) and (cDecoderCLSIDs > 0)) then
            begin
              opCLSID := ppDecoderCLSIDs[0];
            end;
        end
      else
        begin
          // Uncompressed. A decoder is not required.
          opCLSID := GUID_NULL;
        end;
    end;
  Result := hr;
end;


// Enumerates audio or video capture devices.
// Parameters:
// [in] AttributeSourceType:  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID or MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID
// [in/out] DeviceProperties:  TDevicePropsArray
function EnumCaptureDeviceSources(const pAttributeSourceType: TGuid;
                                  var pDeviceProperties: TDevicePropertiesArray): HResult;

  {$REGION Helper for function EnumCaptureDeviceSources}
  // Helper for function EnumCaptureDeviceSources
  function DeviceExists(pDeviceProperties: TDevicePropertiesArray;
                        const pName: PWideChar;
                        out pIndex: Integer): Boolean; inline;
  var
    i: Integer;

  begin
    pIndex := -1;
    i := Length(pDeviceProperties) - 1;
    // Find the last item in the array, with the same name
    while (pIndex = -1) and (i > -1) do
      begin
        // Note: SameText() is not case sensitive, like CompareStr() is.
        Result := SameText(WideCharToString(pName),
                           WideCharToString(pDeviceProperties[i].lpFriendlyName));
        if Result then
          pIndex := i;
        dec(i);
      end;
    Result := (pIndex >= 0);
  end;
  {$ENDREGION}

var
  hr: HResult;
  pAttributes: IMFAttributes;
  pMediaSource: IMFMediaSource;
  pSourceReader: IMFSourceReader;
  ppDevices: PIMFActivate; // Pointer to array of IMFActivate
  iCount: UINT32;
  uiNameLen: UINT32;
  iIndex: Integer;
  szName,
  szSymLink: LPWSTR;
  i,
  j: integer;

label
  done;

begin

  if (pAttributeSourceType <> MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID) and
     (pAttributeSourceType <> MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID) then
    begin
      hr := E_INVALIDARG;
      goto done;
    end;

  uiNameLen := 0;
  iIndex := 0;

  SetLength(pDeviceProperties,
            0);

  // Create an attribute store to specify the enumeration parameters.
  hr := MFCreateAttributes(pAttributes,
                           1);
  if (FAILED(hr)) then
    begin
      goto done;
    end;

  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            pAttributeSourceType); // Source type: video or audio capture device

  if (FAILED(hr)) then
    goto done;


  // Enumerate devices.
  hr := MFEnumDeviceSources(pAttributes,
                            ppDevices,   // pointer to array of IMFActivate interfaces.
                            iCount);    // Number of elements (ppDevices)

  if (FAILED(hr)) then
    begin
      GetLastError();
      goto done;
    end;

  if (iCount = 0) then
    begin
      // Nothing found
      hr := MF_E_NOT_FOUND;
      goto done;
    end;

  //Set the new length
  SetLength(pDeviceProperties,
            iCount);

{$POINTERMATH ON}

  for i := 0 to iCount -1 do
    begin

      // Try to get the readable name.
      hr := ppDevices[i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                            szName,
                                            uiNameLen);
      if FAILED(hr) then
        goto done;

      // Add the fiendlyname to the list.
      pDeviceProperties[i].iDeviceIndex := i;
      pDeviceProperties[i].lpFriendlyName := szName;

      // Add the Interface identifier (IID) of the requested interface.
      pDeviceProperties[i].riid := pAttributeSourceType;

      // Try to get the SymbolicLink name.
      if IsEqualGuid(pAttributeSourceType,
                     MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID) then
        begin
          // Video
          hr := ppDevices[i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                                szSymLink,
                                                uiNameLen);
        end
      else
        begin
          // Audio
          hr := ppDevices[i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK,
                                                szSymLink,
                                                uiNameLen);
        end;

      if SUCCEEDED(hr) then
        begin
          pDeviceProperties[i].lpSymbolicLink := szSymLink;

          //====================================================================

          // Update video formats
          if IsEqualGuid(pAttributeSourceType,
                 MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID) then
          begin
            // Get the MediaSource interface.
            hr := ppDevices[i].ActivateObject(IID_IMFMediaSource,
                                              Pointer(pMediaSource));
            if SUCCEEDED(hr) then
              // Create the SourceReader from the MediaSource
              hr := MFCreateSourceReaderFromMediaSource(pMediaSource,
                                                        ppDevices[i],
                                                        pSourceReader);
            if SUCCEEDED(hr) then
              // Get the device capabillities.
              hr := GetCaptureDeviceCaps(pSourceReader,
                                         pDeviceProperties,
                                         i,
                                         MF_SOURCE_READER_FIRST_VIDEO_STREAM);

            SafeRelease(pSourceReader);
            SafeRelease(pMediaSource);
          end
        else  // update audio format     TODO: Get all audio formats
          begin
           for j := 0 to Length(pDeviceProperties) -1 do
             begin
               // Get the MediaSource
               hr := ppDevices[i].ActivateObject(IID_IMFMediaSource,
                                                 Pointer(pMediaSource));

               if SUCCEEDED(hr) then
                 SetLength(pDeviceProperties[iIndex].aAudioFormats,
                           1);

               hr := GetAudioFormat(pDeviceProperties[iIndex].aAudioFormats[0]);
               if SUCCEEDED(hr) then
                 pDeviceProperties[iIndex].aAudioFormats[0].tgMajorFormat := MFMediaType_Audio;
             end;
          end;

          //====================================================================

        end;

      szName := nil;

      if (FAILED(hr)) then
        goto done;
    end;

{$POINTERMATH OFF}

  // Update display name for devices with the same name.
  for i := 0 to Length(pDeviceProperties) -1 do
    begin
      if DeviceExists(pDeviceProperties,
                      pDeviceProperties[i].lpFriendlyName,
                      iIndex) then
        begin
          iCount := pDeviceProperties[iIndex].iCount + 1;
          if (iCount >= 2) then
            begin
              // Update the first device to include '(1)'
              pDeviceProperties[iIndex].lpDisplayName := StrToPWideChar(Format('%s (%d)',
                                                                               [pDeviceProperties[iIndex].lpFriendlyName,
                                                                                pDeviceProperties[iIndex].iCount]));

              pDeviceProperties[iIndex].iCount := iCount;
            end
          else
            begin
              pDeviceProperties[iIndex].lpDisplayName := pDeviceProperties[iIndex].lpFriendlyName;
              pDeviceProperties[iIndex].iCount := 1;
            end;
        end
      else
        iCount := 1;
    end;

done:

  Result := hr;
end;


//
function GetCaptureDeviceCaps(pSourceReader: IMFSourceReader;
                              var pDeviceProperties: TDevicePropertiesArray;
                              pDeviceIndex: DWord = 0;
                              pStreamIndex: DWord = MF_SOURCE_READER_FIRST_VIDEO_STREAM): HResult;
var
  hr: HResult;
  pMediaType: IMFMediaType;
  dwIndex: DWord;
  dwCount: DWord;
  dwSupportedCount: DWord;
  dwNativeCount: DWord;
  i: Integer;

label
  done;

begin

  dwIndex := 0;
  dwNativeCount := 0;
  dwSupportedCount := 0;

  // Get the number of supported formats. To get all native formats from the device, set param MfSupportedOnly to False.
  hr := CountTypesFromDevice(pSourcereader,
                             pStreamIndex,
                             dwCount,
                             False); // We need all types, supported or not. The application has to manage which type is required.

  if FAILED(hr) or (dwCount = 0) then
    goto done;

  // Set length of the capabillities array
  SetLength(pDeviceProperties[pDeviceIndex].aVideoFormats,
            dwCount);


  while (dwIndex < dwCount) do
    begin
      if Assigned(pMediaType) then
        SafeRelease(pMediaType);

      // Always create IMFMediaType first!
      hr := MFCreateMediaType(pMediaType);
      if FAILED(hr) then
        Break;

      hr := pSourcereader.GetNativeMediaType(pStreamIndex,
                                             dwIndex,
                                             pMediaType);
      if FAILED(hr) then
        Break;

      if (hr = MF_E_NO_MORE_TYPES) then
        begin
          hr := S_OK;
          Break;
        end;

      // Get the capabillities from current (= index) stream

      hr := pMediaType.GetGUID(MF_MT_MAJOR_TYPE,
                               pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].fMajorType);
      if FAILED(hr) then
        Break;

      hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                               pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].fSubType);
      if FAILED(hr) then
        Break;

      hr := MFGetAttributeSize(pMediaType,
                               MF_MT_FRAME_SIZE,
                               pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iVideoWidth,
                               pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iVideoHeight);
      if FAILED(hr) then
        Break;

      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iBufferWidth := pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iVideoWidth;
      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iBufferHeight := pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iVideoHeight;

      hr := MFGetAttributeRatio(pMediaType,
                                MF_MT_FRAME_RATE,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iFrameRateNumerator,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iFrameRateDenominator);
      if FAILED(hr) then
        Break;

      // Calculate framerate ( = FrameRateNumerator / iFrameRateDenominator )
      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].fFrameRate := GetFrameRateFromRatio(pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iFrameRateNumerator,
                                                                                                 pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iFrameRateDenominator);

      hr := MFGetAttributeRatio(pMediaType,
                                MF_MT_FRAME_RATE_RANGE_MIN,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iMinFrameRate,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iMinFrameRateDenominator);
      if FAILED(hr) then
        Break;

      hr := MFGetAttributeRatio(pMediaType,
                                MF_MT_FRAME_RATE_RANGE_MAX,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iMaxFrameRate,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iMaxFrameRateDenominator);
      if FAILED(hr) then
        Break;

      // Get the stride to find out if the image is top-down or bottom-up.
      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].iStride := MFGetAttributeUINT32(pMediaType,
                                                                                             MF_MT_DEFAULT_STRIDE,
                                                                                             1);

      // Get the pixel aspect ratio. (This value might not be set.)
      hr := MFGetAttributeRatio(pMediaType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].AspectRatioNumerator,
                                pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].AspectRatioDenominator);
      if FAILED(hr) then
        Break;


      // On this point we check if the format is supported or not.
      // See: https://learn.microsoft.com/en-us/windows/win32/medfound/video-processor-mft#input-formats
      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].bMFSupported := IsMftSupportedInputFormat(pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].fSubType);

      if pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].bMFSupported then
        Inc(dwSupportedCount);

      // We get all native types. Unsupported formats are marked as unsupported.
      // The application should process the needs.
      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].bMFSupported := IsMftSupportedInputFormat(pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].fSubType);
      pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].FormatsIndex := dwIndex;

      hr := MfCreateMediaType(pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].mfMediaType);

      if SUCCEEDED(hr) then
        pDeviceProperties[pDeviceIndex].aVideoFormats[dwIndex].mfMediaType := pMediaType
      else
        Break;

      Inc(dwNativeCount);
      Inc(dwIndex);
    end;

done:

   if SUCCEEDED(hr) then
     begin
       // Store supported output formats only.
       pDeviceProperties[pDeviceIndex].dwSupportedFormats := dwSupportedCount;
       // Store unsupported and supported formats.
       pDeviceProperties[pDeviceIndex].dwNativeFormats := dwNativeCount;
       // the activation object

     end
   else // If a failure occurs, the entire array will be cleared.
     begin
       for i := 0 to Length(pDeviceProperties) do
         pDeviceProperties[i].Reset;
       pDeviceProperties := nil;
     end;
  Result := hr;
end;


function GetSupportedVideoFormats(CurrentArray: TDevicePropertiesArray): TDevicePropertiesArray;
var
  i, j: Integer;

begin


  for i := 0 to Length(CurrentArray) -1 do
    begin
      for j := 0 to Length(CurrentArray[i].aVideoFormats) -1 do
        if CurrentArray[i].aVideoFormats[j].bMFSupported then
          begin
            SetLength(Result[i].aVideoFormats, 1);
            Result[i].aVideoFormats[j] := CurrentArray[i].aVideoFormats[j]
          end;
    end;

end;

// CreateCaptureDeviceInstance
function CreateCaptureDeviceInstance(pDeviceProperties: TDeviceProperties;
                                     out ppSource: IMFMediaSource;
                                     out ppActivate: IMFActivate): HResult;
var
  count: UINT32;
  pConfig: IMFAttributes;
  ppDevices: PIMFActivate;  // Pointer to array of IMFActivate
  hr: HResult;

label
  done;

begin
  count := 0;

  // Create an attribute store to hold the search criteria.
  hr := MFCreateAttributes(pConfig,
                           1);
  if FAILED(hr) then
    goto done;

  // Request video capture devices.
  hr := pConfig.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                        pDeviceProperties.riid);
  if FAILED(hr) then
    goto done;

  // Enumerate the devices again, because in the mean while a device could be
  // disconnected.
  hr := MFEnumDeviceSources(pConfig,
                            ppDevices,
                            count);
  if FAILED(hr) then
    goto done;

{$POINTERMATH ON}

  // Create a media source from the selected device.
  if (count > 0) then
    begin
      hr := ppDevices[pDeviceProperties.iDeviceIndex].ActivateObject(IID_IMFMediaSource,
                                                                      Pointer(ppSource));
      if FAILED(hr) then
        goto done;

      ppActivate := ppDevices[pDeviceProperties.iDeviceIndex];
    end
  else
    hr := MF_E_NOT_FOUND;

done:
  CoTaskMemFree(ppDevices);
  Result := hr;
end;


//
function EnumerateCaptureFormats(pSource: IMFMediaSource;
                                 out ppMediaType: PIMFMediaType): HResult;
var
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  hr: HResult;
  fSelected: BOOL;
  cTypes: DWORD;
  i: Integer;

label
  done;

begin

  cTypes := 0;

  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(0,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  hr := pHandler.GetMediaTypeCount(cTypes);
  if FAILED(hr) then
    goto done;

  for i := 0 to cTypes - 1 do
    begin
      hr := pHandler.GetMediaTypeByIndex(i,
                                         pType);
      if FAILED(hr) then
        goto done;
{$POINTERMATH ON}
      // store the mediatypes in the array
      ppMediaType[i] := pType;
{$POINTERMATH OFF}
      SafeRelease(pType);
    end;

done:
  Result := hr;
end;

// Helper for EnumerateMediaTypes.
function EnumerateTypesForStream(pReader: IMFSourceReader;
                                 const pStreamIndex: DWORD;
                                 var ppMediaTypes: TArray<IMFMediaType>;
                                 out pCount: DWord): HResult;
var
  hr: HResult;
  dwMediaTypeIndex: DWORD;
  pMediaType: IMFMediaType;
  gSubFormat: TGUID;

begin
  dwMediaTypeIndex := 0;

  hr := MFCreateMediaType(pMediaType);

  if FAILED(hr) then
    begin
      Result := hr;
      Exit;
    end;

  while SUCCEEDED(hr) do
    begin
      hr := pReader.GetNativeMediaType(pStreamIndex,
                                       dwMediaTypeIndex,
                                       pMediaType);
      if (hr = MF_E_NO_MORE_TYPES) or (hr = MF_E_INVALIDSTREAMNUMBER) then
        begin
            hr := S_OK;
            break;
        end
      else if SUCCEEDED(hr) then
        begin
          // Examine the media type.
          // Get the subtype.
          hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                                   gSubFormat);
            if SUCCEEDED(hr) then
              begin
                // Check if format is supported.
                if IsMftSupportedInputFormat(gSubFormat) then
                  begin

                    ppMediaTypes[dwMediaTypeIndex] := pMediaType;

                    Inc(pCount);
                    Break;
                  end;
              end;
          SafeRelease(pMediaType);
        end;
      Inc(dwMediaTypeIndex);
    end;

  Result := hr;
end;


function EnumerateMediaTypes(pReader: IMFSourceReader;
                             pStreamIndex: DWord;
                             var ppMediaTypes: TArray<IMFMediaType>;
                             out pCount: DWord): HResult;
var
  hr: HResult;
  pMediaType: IMFMediaType;
  gSubFormat: TGUID;
  dwStreamCount: DWORD;
  dwMediaTypeIndex: DWORD;
  i: Integer;

begin
  dwMediaTypeIndex := 0;
  dwStreamCount := CountSourceReaderStreams(pReader);
  SetLength(ppMediaTypes,
            dwStreamCount);

  hr := MFCreateMediaType(pMediaType);

  for i := 0 to dwStreamCount - 1 do
    begin
      //hr := EnumerateTypesForStream(pReader,
      //                              i,
      //                              ppMediaTypes,
      //                              pCount);
      //Inc(pStreamIndex);

      while SUCCEEDED(hr) do
        begin
          hr := pReader.GetNativeMediaType(pStreamIndex,
                                           dwMediaTypeIndex,
                                           pMediaType);
          if (hr = MF_E_NO_MORE_TYPES) or (hr = MF_E_INVALIDSTREAMNUMBER) then
            begin
               hr := S_OK;
               Break;
           end
      else if SUCCEEDED(hr) then
        begin
          // Examine the media type.
          // Get the subtype.
          hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                                   gSubFormat);
            if SUCCEEDED(hr) then
              begin
                // Check if format is supported.
                if IsMftSupportedInputFormat(gSubFormat) then
                  begin
                    ppMediaTypes[dwMediaTypeIndex] := pMediaType;
                    Inc(pCount);
                    Break;
                  end;
              end;
          SafeRelease(pMediaType);
        end;
      Inc(dwMediaTypeIndex);
    end;
    end;
  Result := hr;
end;


function CountSourceReaderStreams(pReader: IMFSourceReader): DWord;
var
  hr: HResult;
  dwStreamCount: DWORD;
  bSelected: Boolean;

begin
  hr := S_OK;
  dwStreamCount := 0;
  while (SUCCEEDED(hr)) do
    begin
      hr := pReader.GetStreamSelection(dwStreamCount,
                                       bSelected);
      if (hr = MF_E_INVALIDSTREAMNUMBER) then
        Break;

      Inc(dwStreamCount);
    end;
 Result := dwStreamCount
end;


//
function CountTypesFromDevice(pReader: IMFSourceReader;
                              const pStreamIndex: DWord;
                              out pCount: DWord;
                              const pMfSupportedOnly: Boolean = True): HResult;
var
  hr: HResult;
  dwIndex: DWORD;
  dwNativeCount: DWord;
  dwMfSupportedCount: DWord;
  mfType: IMFMediaType;
  fSubType: TGuid;

begin
  dwMfSupportedCount := 0;
  dwNativeCount := 0;
  dwIndex := 0;

  repeat
    begin

      SafeRelease(mfType);

      hr := MFCreateMediaType(mfType);
      if FAILED(hr) then
        Break;

      hr := pReader.GetNativeMediaType(pStreamIndex,
                                       dwIndex,
                                       mfType);

      if (hr = MF_E_NO_MORE_TYPES) then
        begin
          hr := S_OK;
          // We did set both +1. But since we have a hit, decrease by one because we stop processing.
          Dec(dwMfSupportedCount);
          Dec(dwNativeCount);
          Break;
        end;

      hr := mfType.GetGUID(MF_MT_SUBTYPE,
                           fSubType);
      if FAILED(hr) then
        Break;

      // Examine the mediatype here.

      if pMfSupportedOnly then
        begin
          if IsMftSupportedInputFormat(fSubType) then
            begin
              Inc(dwMfSupportedCount);
              Inc(dwNativeCount);
            end
          else
            Inc(dwNativeCount);
        end
      else  // Get all native types from the capturedevice.
        Inc(dwNativeCount);

      Inc(dwIndex);
    end;
  until (hr = MF_E_NO_MORE_TYPES);

  if pMfSupportedOnly then
    pCount := dwMfSupportedCount
  else
    pCount := dwNativeCount;

  Result := hr;
end;


// Note: RTTI is not used, because it will not work on all Delphi versions.
//       So, we do it the alternative way.
function GetGUIDNameConst(const majorType: TGuid;
                          const subType: TGuid;
                          out aGuidName: LPWSTR;
                          out aFormatTag: LPWSTR;
                          out aFOURCC: DWord;
                          out aFmtDesc: LPWSTR): HResult;
var
  hr: HResult;
  sGuidName: LPWSTR;
  sFormatTag: LPWSTR;
  sFmtDesc: LPWSTR;
  dwFOURCC: DWord;

  function IfEqualReturnProps(const gtype: TGuid;
                              sgName: LPWSTR;
                              sFmtTag: LPWSTR;
                              dFCC: Dword;
                              sDesc: LPWSTR): Boolean;
    begin
      sGuidName := '';
      if IsEqualGuid(subType,
                     gtype) then
        begin
          sGuidName := sgName;
          sFormatTag := sFmtTag;
          dwFOURCC := dFCC;
          sFmtDesc := sDesc;
          hr := S_OK;
          Result := True;
        end
      else
        Result := False;
    end;

label
  done;

begin

  sGuidName := 'Unknown guid.';
  sFormatTag := 'Unknown format tag.';
  dwFOURCC := 0;
  sFmtDesc := 'Unknown format.';
  hr := ERROR_NOT_FOUND;

  if IsEqualGuid(majorType,
                 MFMediaType_Video) or
     IsEqualGuid(majorType,
                 MFMediaType_Audio) then
    begin
      if IfEqualReturnProps(MF_MT_MAJOR_TYPE,
                            'MF_MT_MAJOR_TYPE',
                            '',
                            0,
                           'Major type GUID for a media type.') then
       goto done;
    end;

  if IfEqualReturnProps(MF_MT_SUBTYPE,
                        'MF_MT_SUBTYPE',
                        '',
                        0,
                        'Sub type GUID for a media type.') then
    goto done;


  if IfEqualReturnProps(MF_MT_ALL_SAMPLES_INDEPENDENT,
                        'MF_MT_ALL_SAMPLES_INDEPENDENT',
                        '',
                        0,
                        'Specifies for a media type whether each sample is independent of the other samples in the stream.') then
    goto done;
  if IfEqualReturnProps(MF_MT_FIXED_SIZE_SAMPLES,
                        'MF_MT_FIXED_SIZE_SAMPLES',
                        '',
                        0,
                        'Specifies for a media type whether the samples have a fixed size.') then
    goto done;
  if IfEqualReturnProps(MF_MT_COMPRESSED,
                       'MF_MT_COMPRESSED',
                       '',
                       0,
                       'Specifies for a media type whether the media data is compressed.') then
    goto done;
  if IfEqualReturnProps(MF_MT_SAMPLE_SIZE,
                        'MF_MT_SAMPLE_SIZE',
                        '',
                        0,
                        'Specifies the size of each sample, in bytes, in a media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_WRAPPED_TYPE,
                        'MF_MT_WRAPPED_TYPE',
                        '',
                        0,
                        'Contains a media type that has been wrapped in another media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_NUM_CHANNELS,
                        'MF_MT_AUDIO_NUM_CHANNELS',
                        '',
                        0,
                        'Number of audio channels in an audio media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        'MF_MT_AUDIO_SAMPLES_PER_SECOND',
                        '',
                        0,
                        'Number of audio samples per second in an audio media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND,
                        'MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND',
                        '',
                        0,
                        'Number of audio samples per second in an audio media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                        'MF_MT_AUDIO_AVG_BYTES_PER_SECOND',
                        '',
                        0,
                        'Average number of bytes per second in an audio media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                        'MF_MT_AUDIO_BLOCK_ALIGNMENT',
                        '',
                        0,
                        'Block alignment, in bytes, for an audio media type. Is minimum atomic unit of data for the audio format.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_BITS_PER_SAMPLE,
                        'MF_MT_AUDIO_BITS_PER_SAMPLE',
                        '',
                        0,
                        'Number of bits per audio sample in an audio media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_VALID_BITS_PER_SAMPLE,
                        'MF_MT_AUDIO_VALID_BITS_PER_SAMPLE',
                        '',
                        0,
                        'Number of valid bits of audio data in each audio sample.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_SAMPLES_PER_BLOCK,
                        'MF_MT_AUDIO_SAMPLES_PER_BLOCK',
                        '',
                        0,
                        'Number of audio samples contained in one compressed block of audio data.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_CHANNEL_MASK,
                        'MF_MT_AUDIO_CHANNEL_MASK',
                        '',
                        0,
                        'In an audio media type, specifies the assignment of audio channels to speaker positions.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_FOLDDOWN_MATRIX,
                        'MF_MT_AUDIO_FOLDDOWN_MATRIX',
                        '',
                        0,
                        'Specifies how an audio decoder should transform multichannel audio to stereo output (fold-down).') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_WMADRC_PEAKREF,
                        'MF_MT_AUDIO_WMADRC_PEAKREF',
                        '',
                        0,
                        'Reference peak volume level of a Windows Media Audio file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_WMADRC_PEAKTARGET,
                       'MF_MT_AUDIO_WMADRC_PEAKTARGET',
                       '',
                       0,
                       'Target peak volume level of a Windows Media Audio file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_WMADRC_AVGREF,
                        'MF_MT_AUDIO_WMADRC_AVGREF',
                        '',
                        0,
                        'Reference average volume level of a Windows Media Audio file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_WMADRC_AVGTARGET,
                        'MF_MT_AUDIO_WMADRC_AVGTARGET',
                        '',
                        0,
                        'Target average volume level of a Windows Media Audio file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AUDIO_PREFER_WAVEFORMATEX,
                        'MF_MT_AUDIO_PREFER_WAVEFORMATEX',
                        '',
                        0,
                        'Specifies the preferred legacy format structure to use when converting an audio media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AAC_PAYLOAD_TYPE,
                       'MF_MT_AAC_PAYLOAD_TYPE',
                        '',
                        0,
                        'Specifies the payload type of an Advanced Audio Coding (AAC) stream.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                        'MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION',
                        '',
                        0,
                        'Specifies the audio profile and level of an Advanced Audio Coding (AAC) stream.') then
    goto done;

  // Properties
  if IfEqualReturnProps(MF_MT_FRAME_SIZE,
                        'MF_MT_FRAME_SIZE',
                        '',
                        0,
                        'Width and height of a video frame, in pixels.') then
    goto done;
  if IfEqualReturnProps(MF_MT_FRAME_RATE,
                        'MF_MT_FRAME_RATE',
                        '',
                        0,
                        'Frame rate of a video media type, in frames per second.') then
    goto done;
  if IfEqualReturnProps(MF_MT_FRAME_RATE_RANGE_MAX,
                        'MF_MT_FRAME_RATE_RANGE_MAX',
                         '',
                         0,
                         'The maximum frame rate that is supported by a video capture device, in frames per second.') then
    goto done;
  if IfEqualReturnProps(MF_MT_FRAME_RATE_RANGE_MIN,
                        'MF_MT_FRAME_RATE_RANGE_MIN',
                        '',
                        0,
                        'The minimum frame rate that is supported by a video capture device, in frames per second.') then
    goto done;
  if IfEqualReturnProps(MF_MT_PIXEL_ASPECT_RATIO,
                        'MF_MT_PIXEL_ASPECT_RATIO',
                        '',
                        0,
                        'Pixel aspect ratio for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DRM_FLAGS,
                        'MF_MT_DRM_FLAGS',
                        '',
                        0,
                        'Specifies whether a video media type requires the enforcement of copy protection.') then
    goto done;
  if IfEqualReturnProps(MF_MT_PAD_CONTROL_FLAGS,
                        'MF_MT_PAD_CONTROL_FLAGS',
                        '',
                        0,
                        'Specifies the aspect ratio of the output rectangle for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_SOURCE_CONTENT_HINT,
                        'MF_MT_SOURCE_CONTENT_HINT',
                        '',
                        0,
                        'Describes the intended aspect ratio for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_VIDEO_CHROMA_SITING,
                        'MF_MT_VIDEO_CHROMA_SITING',
                        '',
                        0,
                        'Describes how chroma was sampled for a Y''Cb''Cr'' video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_INTERLACE_MODE,
                        'MF_MT_INTERLACE_MODE',
                        '',
                        0,
                        'Describes how the frames in a video media type are interlaced.') then
    goto done;
  if IfEqualReturnProps(MF_MT_TRANSFER_FUNCTION,
                        'MF_MT_TRANSFER_FUNCTION',
                        '',
                        0,
                        'Specifies the conversion function from RGB to R''G''B'' for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_VIDEO_PRIMARIES,
                        'MF_MT_VIDEO_PRIMARIES',
                        '',
                        0,
                        'Specifies the color primaries for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_CUSTOM_VIDEO_PRIMARIES,
                        'MF_MT_CUSTOM_VIDEO_PRIMARIES',
                        '',
                        0,
                        'Specifies custom color primaries for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_YUV_MATRIX,
                        'MF_MT_YUV_MATRIX',
                        '',
                        0,
                        'For YUV media types, defines the conversion matrix from the Y''Cb''Cr'' color space to the R''G''B'' color space.') then
    goto done;
  if IfEqualReturnProps(MF_MT_VIDEO_LIGHTING,
                        'MF_MT_VIDEO_LIGHTING',
                        '',
                        0,
                        'Specifies the optimal lighting conditions for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_VIDEO_NOMINAL_RANGE,
                        'MF_MT_VIDEO_NOMINAL_RANGE',
                        '',
                        0,
                        'Specifies the nominal range of the color information in a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_GEOMETRIC_APERTURE,
                       'MF_MT_GEOMETRIC_APERTURE',
                       '',
                       0,
                       'Defines the geometric aperture for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MINIMUM_DISPLAY_APERTURE,
                        'MF_MT_MINIMUM_DISPLAY_APERTURE',
                        '',
                        0,
                        'Defines the display aperture, which is the region of a video frame that contains valid image data.') then
    goto done;
  if IfEqualReturnProps(MF_MT_PAN_SCAN_APERTURE,
                       'MF_MT_PAN_SCAN_APERTURE',
                       '',
                       0,
                       'Defines the pan/scan aperture, which is the 4x3 region of video that should be displayed in pan/scan mode.') then
    goto done;
  if IfEqualReturnProps(MF_MT_PAN_SCAN_ENABLED,
                        'MF_MT_PAN_SCAN_ENABLED',
                        '',
                        0,
                        'Specifies whether pan/scan mode is enabled.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AVG_BITRATE,
                        'MF_MT_AVG_BITRATE',
                        '',
                        0,
                        'Approximate data rate of the video stream, in bits per second, for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AVG_BIT_ERROR_RATE,
                        'MF_MT_AVG_BIT_ERROR_RATE',
                        '',
                        0,
                        'Data error rate, in bit errors per second, for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MAX_KEYFRAME_SPACING,
                        'MF_MT_MAX_KEYFRAME_SPACING',
                        '',
                        0,
                        'Maximum number of frames from one key frame to the next, in a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DEFAULT_STRIDE,
                        'MF_MT_DEFAULT_STRIDE',
                        '',
                        0,
                        'Default surface stride, for an uncompressed video media type. Stride is the number of bytes needed to go from one row of pixels to the next.') then
    goto done;
  if IfEqualReturnProps(MF_MT_PALETTE,
                        'MF_MT_PALETTE',
                        '',
                        0,
                        'Contains the palette entries for a video media type. Use this attribute for palettized video formats, such as RGB 8.') then
    goto done;
  if IfEqualReturnProps(MF_MT_USER_DATA,
                        'MF_MT_USER_DATA',
                        '',
                        0,
                        'Contains additional format data for a media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_AM_FORMAT_TYPE,
                        'MF_MT_AM_FORMAT_TYPE',
                        '',
                        0,
                        'Contains a DirectShow format GUID for a media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG_START_TIME_CODE,
                        'MF_MT_MPEG_START_TIME_CODE',
                        '',
                        0,
                        'Group-of-pictures (GOP) start time code, for an MPEG-1 or MPEG-2 video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG2_PROFILE,
                        'MF_MT_MPEG2_PROFILE',
                        '',
                        0,
                        'Specifies the MPEG-2 or H.264 profile in a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG2_LEVEL,
                        'MF_MT_MPEG2_LEVEL',
                        '',
                        0,
                        'Specifies the MPEG-2 or H.264 level in a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG2_FLAGS,
                        'MF_MT_MPEG2_FLAGS',
                        '',
                        0,
                        'Contains miscellaneous flags for an MPEG-2 video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG_SEQUENCE_HEADER,
                        'MF_MT_MPEG_SEQUENCE_HEADER',
                        '',
                        0,
                        'Contains the MPEG-1 or MPEG-2 sequence header for a video media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DV_AAUX_SRC_PACK_0,
                        'MF_MT_DV_AAUX_SRC_PACK_0',
                        '',
                        0,
                        'Audio auxiliary (AAUX) source pack for the first audio block in a digital video (DV) media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DV_AAUX_CTRL_PACK_0,
                        'MF_MT_DV_AAUX_CTRL_PACK_0',
                        '',
                        0,
                        'Audio auxiliary (AAUX) source control pack for the first audio block in a digital video (DV) media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DV_AAUX_SRC_PACK_1,
                        'MF_MT_DV_AAUX_SRC_PACK_1',
                        '',
                        0,
                        'Audio auxiliary (AAUX) source pack for the second audio block in a digital video (DV) media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DV_AAUX_CTRL_PACK_1,
                        'MF_MT_DV_AAUX_CTRL_PACK_1',
                        '',
                        0,
                        'Audio auxiliary (AAUX) source control pack for the second audio block in a digital video (DV) media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DV_VAUX_SRC_PACK,
                        'MF_MT_DV_VAUX_SRC_PACK',
                        '',
                        0,
                        'Video auxiliary (VAUX) source pack in a digital video (DV) media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_DV_VAUX_CTRL_PACK,
                        'MF_MT_DV_VAUX_CTRL_PACK',
                        '',
                        0,
                        'Video auxiliary (VAUX) source control pack in a digital video (DV) media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_ARBITRARY_HEADER,
                        'MF_MT_ARBITRARY_HEADER',
                        '',
                        0,
                        'Type-specific data for a binary stream in an Advanced Systems Format (ASF) file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_ARBITRARY_FORMAT,
                        'MF_MT_ARBITRARY_FORMAT',
                        '',
                        0,
                        'Additional format data for a binary stream in an Advanced Systems Format (ASF) file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_IMAGE_LOSS_TOLERANT,
                        'MF_MT_IMAGE_LOSS_TOLERANT',
                        '',
                        0,
                        'Specifies whether an ASF image stream is a degradable JPEG type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG4_SAMPLE_DESCRIPTION,
                        'MF_MT_MPEG4_SAMPLE_DESCRIPTION',
                        '',
                        0,
                        'Contains the sample description box for an MP4 or 3GP file.') then
    goto done;
  if IfEqualReturnProps(MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY,
                        'MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY',
                        '',
                        0,
                        'Specifies the current entry in the sample description box for an MPEG-4 media type.') then
    goto done;
  if IfEqualReturnProps(MF_MT_ORIGINAL_4CC,
                        'MF_MT_ORIGINAL_4CC',
                        '',
                        0,
                        'Contains the original codec FOURCC for a video stream.') then
    goto done;
  if IfEqualReturnProps(MF_MT_ORIGINAL_WAVE_FORMAT_TAG,
                        'MF_MT_ORIGINAL_WAVE_FORMAT_TAG',
                        '',
                        0,
                        'Contains the original WAVE format tag for an audio stream.') then
    goto done;

  // Major Media types
  if IfEqualReturnProps(MFMediaType_Audio,
                        'MFMediaType_Audio',
                        '',
                        0,
                        'Audio.') then
    goto done;
  if IfEqualReturnProps(MFMediaType_Video,
                        'MFMediaType_Video',
                        '',
                        0,
                        'Video') then
    goto done;
  if IfEqualReturnProps(MFMediaType_Protected,
                        'MFMediaType_Protected',
                        '',
                        0,
                        'Protected content. (DRM)') then
    goto done;
  if IfEqualReturnProps(MFMediaType_SAMI,
                        'MFMediaType_SAMI',
                        '',
                        0,
                        'Synchronized Accessible Media Interchange (SAMI) captions.') then
    goto done;
  if IfEqualReturnProps(MFMediaType_Script,
                        'MFMediaType_Script',
                        '',
                        0,
                        'Script stream.') then
    goto done;
  if IfEqualReturnProps(MFMediaType_Image,
                        'MFMediaType_Image',
                        '',
                        0,
                        'Still image stream.') then
    goto done;
  if IfEqualReturnProps(MFMediaType_HTML,
                        'MFMediaType_HTML',
                        '',
                        0,
                        '') then
    goto done;
  if IfEqualReturnProps(MFMediaType_Binary,
                        'MFMediaType_Binary',
                        '',
                        0,
                        'Binary stream.') then
    goto done;
  if IfEqualReturnProps(MFMediaType_FileTransfer,
                        'MFMediaType_FileTransfer',
                        '',
                        0,
                        'A stream that contains data files.') then
    goto done;

  // Video formats ////////////////////////////////////////////////////////////

  // Uncompressed RGB Formats
  // Note: These subtypes do not match the RGB subtype GUIDs used in previous SDKs, such as DirectShow.

  if IfEqualReturnProps(MFVideoFormat_ARGB32,
                        'MFVideoFormat_ARGB32',
                        'D3DFMT_A8R8G8B8',
                        D3DFMT_A8R8G8B8,
                        'Uncompressed RGB Format, 32 bpp with alpha channel.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_RGB24,
                        'MFVideoFormat_RGB24',
                        'D3DFMT_R8G8B8',
                        D3DFMT_R8G8B8,
                        'Uncompressed RGB Format, 24 bpp.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_RGB32,
                        'MFVideoFormat_RGB32',
                        'D3DFMT_X8R8G8B8',
                        D3DFMT_X8R8G8B8,
                        'Uncompressed RGB Format, 32 bpp.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_RGB555,
                        'MFVideoFormat_RGB555',
                        'D3DFMT_X1R5G5B5',
                        D3DFMT_X1R5G5B5,
                        'Uncompressed RGB Format, 555, 16 bpp. (Same memory layout as D3DFMT_X1R5G5B5.)') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_RGB565,
                        'MFVideoFormat_RGB565',
                        'D3DFMT_R5G6B5',
                        D3DFMT_R5G6B5,
                        'Uncompressed RGB Format, 565, 16 bpp. (Same memory layout as D3DFMT_R5G6B5.)') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_RGB8,
                        'MFVideoFormat_RGB8',
                        'RGB8',
                        FCC('RGB8'),
                        'Uncompressed RGB Format, 8 bits per pixel (bpp). (Same memory layout as D3DFMT_P8.)') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_A16B16G16R16F,
                        'MFVideoFormat_A16B16G16R16F',
                        'D3DFMT_A16B16G16R16F',
                        D3DFMT_A16B16G16R16F,
                        'Uncompressed RGB Format, 16 bpp with alpha channel. (Same memory layout as D3DFMT_A16B16G16R16F)') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_A2R10G10B10,
                        'MFVideoFormat_A2R10G10B10',
                        'D3DFMT_A2B10G10R10',
                        D3DFMT_A2B10G10R10,
                        'Uncompressed RGB Format, 10 bpp for each color and 2 bpp for alpha. (Same memory layout as D3DFMT_A2B10G10R10)') then
    goto done;

  // YUV Formats: 8-Bit and Palettized.
  if IfEqualReturnProps(MFVideoFormat_AI44,
                        'MFVideoFormat_AI44',
                        'AI44',
                        FCC('AI44'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:4:4, Packed, Bits per channel: Palettized') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_AYUV,
                        'MFVideoFormat_AYUV',
                        'AYUV',
                        FCC('AYUV'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:4:4, Packed, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_I420,
                        'MFVideoFormat_I420',
                        'I420',
                        FCC('I420'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:0, Planar, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_IYUV,
                        'MFVideoFormat_IYUV',
                        'IYUV',
                        FCC('IYUV'),
                         'YUV 8-Bit and Palettized Format. Sampling: 4:2:0, Planar, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_NV11,
                        'MFVideoFormat_NV11',
                        'NV11',
                        FCC('NV11'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:1:1, Planar, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_NV12,
                        'MFVideoFormat_NV12',
                        'NV12',
                        FCC('NV12'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:0, Planar, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_NV21,
                        'MFVideoFormat_NV21',
                        'NV21',
                        FCC('NV21'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:0, Planar, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_UYVY,
                        'MFVideoFormat_UYVY',
                        'UYVY',
                        FCC('UYVY'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:2, Packed, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y41P,
                        'MFVideoFormat_Y41P',
                        'Y41P',
                        FCC('Y41P'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:1:1, Packed, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y41T,
                        'MFVideoFormat_Y41T',
                        'Y41T',
                        FCC('Y41T'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:1:1, Packed, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y42T,
                        'MFVideoFormat_Y42T',
                        'Y42T',
                        FCC('Y42T'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:2, Packed, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_YUY2,
                        'MFVideoFormat_YUY2',
                        'YUY2',
                        FCC('YUY2'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:2, Packed, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_YVU9,
                        'MFVideoFormat_YVU9',
                        'YUY9',
                        FCC('YUY9'),
                        'YUV 8-Bit and Palettized Format. Sampling: 8:4:4, Planar, Bits per channel: 9') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_YV12,
                        'MFVideoFormat_YV12',
                        'YV12',
                        FCC('YV12'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:0, Planar, Bits per channel: 8') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_YVYU,
                        'MFVideoFormat_YVYU',
                        'YVYU',
                        FCC('YVYU'),
                        'YUV 8-Bit and Palettized Format. Sampling: 4:2:2, Packed, Bits per channel: 8') then
    goto done;

  // YUV Formats: 10-Bit and 16-Bit.
  if IfEqualReturnProps(MFVideoFormat_P010,
                        'MFVideoFormat_P010',
                        'P010',
                        FCC('P010'),
                        'YUV 10-Bit Format. Sampling: 4:2:0, Planar, Bits per channel: 10') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_P016,
                         'MFVideoFormat_P016',
                         'P016',
                         FCC('P016'),
                         'YUV 16-Bit Format. Sampling: 4:2:0, Planar, Bits per channel: 16') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_P210,
                        'MFVideoFormat_P210',
                        'P210',
                        FCC('P210'),
                        'YUV 10-Bit Format. Sampling: 4:2:2, Planar, Bits per channel: 10') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_P216,
                        'MFVideoFormat_P216',
                        'P216',
                        FCC('P216'),
                        'YUV 16-Bit Format. Sampling: 4:2:0, Planar, Bits per channel: 16') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y210,
                        'MFVideoFormat_Y210',
                        'Y210',
                        FCC('Y210'),
                        'YUV 10-Bit Format. Sampling: 4:2:2, Packed, Bits per channel: 10') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y216,
                        'MFVideoFormat_v216',
                        'Y216',
                        FCC('Y216'),
                        'YUV 16-Bit Format. Sampling: 4:2:2, Packed, Bits per channel: 16') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y410,
                        'MFVideoFormat_Y410',
                        'Y410',
                        FCC('Y40 '),
                        'YUV 10-Bit Format. Sampling: 4:4:4, Packed, Bits per channel: 10') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_Y416,
                        'MFVideoFormat_Y416',
                        'Y416',
                        FCC('Y416'),
                        'YUV 10-Bit Format. Sampling: 4:4:4, Packed, Bits per channel: 10') then
    goto done;

  // Luminance and Depth Formats
  if IfEqualReturnProps(MFVideoFormat_L8,
                        'MFVideoFormat_L8',
                        'D3DFMT_L8',
                        D3DFMT_L8,
                        '8-bit luminance only. (bpp). (Same memory layout as D3DFMT_L8.)') then
    goto done;


  if IfEqualReturnProps(MFVideoFormat_L16,
                         'MFVideoFormat_L16',
                         'D3DFMT_L16',
                         D3DFMT_L16,
                         '16-bit luminance only. (Same memory layout as D3DFMT_L16.)') then
   goto done;

  // Note: MFVideoFormat_L16 and MFAudioFormat_MPEG share the same guidvalue.
  if IsEqualGuid(majorType,
                 MFMediaType_Video) then
   begin
     if IfEqualReturnProps(MFVideoFormat_D16,
                           'MFVideoFormat_D16',
                           'D3DFMT_D16',
                           D3DFMT_D16,
                           '16-bit z-buffer depth. (Same memory layout as D3DFMT_D16.)') then
       goto done;
   end;

  // Encoded Video Types
  if IfEqualReturnProps(MFVideoFormat_DV25,
                        'MFVideoFormat_DV25',
                        'dv25',
                        FCC('dv25'),
                        'Encoded Video Type. DVCPRO 25 (525-60 or 625-50).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_DV50,
                        'MFVideoFormat_DV50',
                        'dv50',
                        FCC('dv50'),
                        'Encoded Video Type. DVCPRO 50 (525-60 or 625-50).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_DVC,
                        'MFVideoFormat_DVC',
                        'dvc ',
                        FCC('dvc '),
                        'Encoded Video Type. DVC/DV Video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_DVH1,
                        'MFVideoFormat_DVH1',
                        'dvh1',
                        FCC('dvh1'),
                        'Encoded Video Type. DVCPRO 100 (1080/60i, 1080/50i, or 720/60P).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_DVHD,
                        'MFVideoFormat_DVHD',
                        'dvhd',
                        FCC('dvhd'),
                        'Encoded Video Type. HD-DVCR (1125-60 or 1250-50).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_DVSD,
                        'MFVideoFormat_DVSD',
                        'dvsd',
                        FCC('dvsd'),
                        'Encoded Video Type. SDL-DVCR (525-60 or 625-50).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_DVSL,
                        'MFVideoFormat_DVSL',
                        'dvsl', FCC('dvsl'),
                        'Encoded Video Type. SD-DVCR (525-60 or 625-50).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_H263,
                        'MFVideoFormat_H263',
                        'H263',
                        FCC('H263'),
                        'Encoded Video Type. H.263 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_H264,
                        'MFVideoFormat_H264',
                        'H264',
                        FCC('H264'),
                        'Encoded Video Type. H.264 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_H264_ES,
                        'MFVideoFormat_H264_ES',
                        '',
                        0,
                        'Encoded Video Type. H.264 elementary stream. This media type is the same as MFVideoFormat_H264, except media samples contain a fragmented H.264 bitstream.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_HEVC,
                        'MFVideoFormat_HEVC',
                        'HEVC',
                        FCC('HEVC'),
                        'Encoded Video Type. The HEVC Main profile and Main Still Picture profile. Each sample contains one complete picture.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_HEVC_ES,
                        'MFVideoFormat_HEVC_ES',
                        'HEVS',
                        FCC('HEVS'),
                        'Encoded Video Type. This media type is the same as MFVideoFormat_HEVC, except media samples contain a fragmented HEVC bitstream. ') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_M4S2,
                        'MFVideoFormat_M4S2',
                        'M4S2',
                        FCC('M4S2'),
                        'Encoded Video Type. MPEG-4 part 2 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MJPG,
                        'MFVideoFormat_MJPG',
                        'MJPG',
                        FCC('MJPG'),
                        'Encoded Video Type. Motion JPEG.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MP43,
                        'MFVideoFormat_MP43',
                        'MP43',
                        FCC('MP43'),
                        'Encoded Video Type. Microsoft MPEG 4 codec version 3. This codec is no longer supported.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MP4S,
                        'MFVideoFormat_MP4S',
                        'MP4S',
                        FCC('MP4S'),
                        'Encoded Video Type. ISO MPEG 4 codec version 1.' )then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MP4V,
                        'MFVideoFormat_MP4V',
                        'MP4V',
                        FCC('MP4V'),
                        'Encoded Video Type. MPEG-4 part 2 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MPEG2,
                        'MFVideoFormat_MPEG2',
                        '',
                        0,
                        'Encoded Video Type. MPEG-2 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MPG1,
                        'MFVideoFormat_MPG1',
                        'MPG1',
                        FCC('MPG1'),
                        'Encoded Video Type. MPEG-1 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_VP80,
                        'MFVideoFormat_VP80',
                        'VP80',
                        FCC('VP80'),
                        'Encoded Video Type. VP8 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_VP90,
                        'MFVideoFormat_VP90',
                        'VP90',
                        FCC('VP90'),
                        'Encoded Video Type. VP9 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MSS1,
                        'MFVideoFormat_MSS1',
                        'MSS1',
                        FCC('MSS1'),
                        'Encoded Video Type. Windows Media Screen codec version 1.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_MSS2,
                        'MFVideoFormat_MSS2',
                        'MSS2',
                        FCC('MSS2'),
                        'Encoded Video Type. Windows Media Screen codec version 2.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_WMV1,
                        'MFVideoFormat_WMV1',
                        'WMV1',
                        FCC('WMV1'),
                        'Encoded Video Type. Windows Media Video codec version 7.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_WMV2,
                        'MFVideoFormat_WMV2',
                        'WMV2',
                        FCC('WMV2'),
                        'Encoded Video Type. Windows Media Video 8 codec.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_WMV3,
                        'MFVideoFormat_WMV3',
                        'WMV3',
                        FCC('WMV3'),
                        'Encoded Video Type. Windows Media Video 9 codec.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_WVC1,
                        'MFVideoFormat_WVC1',
                        'WVC1',
                        FCC('WVC1'),
                        'Encoded Video Type. SMPTE 421M ("VC-1").') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_420O,
                        'MFVideoFormat_420O',
                        '420O',
                        FCC('420O'),
                        'Encoded Video Type. 8-bit per channel planar YUV 4:2:0 video.') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_AV1,
                        'MFVideoFormat_AV1',
                        'AV01',
                        FCC('AV01'),
                        'Encoded Video Type. AV1 video.') then
    goto done;

  // Audio formats
  if IfEqualReturnProps(MFAudioFormat_PCM,
                        'MFAudioFormat_PCM',
                        'WAVE_FORMAT_PCM',
                        WAVE_FORMAT_PCM,
                        'Uncompressed PCM audio.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Float,
                        'MFAudioFormat_Float',
                        'WAVE_FORMAT_IEEE_FLOAT',
                        WAVE_FORMAT_IEEE_FLOAT,
                        'Uncompressed IEEE floating-point audio.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS,
                        'MFAudioFormat_DTS',
                        'WAVE_FORMAT_DTS',
                        WAVE_FORMAT_DTS,
                        'Microsoft DTS (Data Transformation Services Package File Format) audio.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC3_SPDIF,
                        'MFAudioFormat_Dolby_AC3_SPDIF',
                        'WAVE_FORMAT_DOLBY_AC3_SPDIF',
                        WAVE_FORMAT_DOLBY_AC3_SPDIF,
                        'Dolby AC-3 audio over Sony/Philips Digital Interface (S/PDIF).') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DRM,
                       'MFAudioFormat_DRM',
                        'WAVE_FORMAT_DRM',
                        WAVE_FORMAT_DRM,
                        'Audio Digital Rights Management codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_WMAudioV8,
                        'MFAudioFormat_WMAudioV8',
                        'WAVE_FORMAT_WMAUDIO2',
                        WAVE_FORMAT_WMAUDIO2,
                        'Windows Media Audio 8 codec, Windows Media Audio 9 codec, or Windows Media Audio 9.1 codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_WMAudioV9,
                        'MFAudioFormat_WMAudioV9',
                        'WAVE_FORMAT_WMAUDIO3',
                        WAVE_FORMAT_WMAUDIO3,
                        'Windows Media Audio 9 Professional audio codec or Windows Media Audio 9.1 Professional codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_WMAudio_Lossless,
                        'MFAudioFormat_WMAudio_Lossless',
                        'WAVE_FORMAT_WMAUDIO_LOSSLESS',
                        WAVE_FORMAT_WMAUDIO_LOSSLESS,
                        'Windows Media Audio 9 Lossless codec or Windows Media Audio 9.1 codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_WMASPDIF,
                        'MFAudioFormat_WMASPDIF',
                        'WAVE_FORMAT_WMASPDIF',
                        WAVE_FORMAT_WMASPDIF,
                        'Windows Media Audio S/PDIF.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_MSP1,
                        'MFAudioFormat_MSP1',
                        'WAVE_FORMAT_WMAVOICE9',
                        WAVE_FORMAT_WMAVOICE9,
                        'Windows Media Audio 9 Voice codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_MP3,
                        'MFAudioFormat_MP3',
                        'WAVE_FORMAT_MPEGLAYER3',
                        WAVE_FORMAT_MPEGLAYER3,
                        'MPEG Audio Layer-3 (MP3).') then
    goto done;

 // Note: MFVideoFormat_L16 and MFAudioFormat_MPEG share the same guidvalue.
 if IsEqualGuid(majorType,
                MFMediaType_Audio) then
   begin
     if IfEqualReturnProps(MFAudioFormat_MPEG,
                           'MFAudioFormat_MPEG',
                           'WAVE_FORMAT_MPEG',
                           WAVE_FORMAT_MPEG,
                           'MPEG-1 audio payload.') then
     goto done;
   end;

  if IfEqualReturnProps(MFAudioFormat_AAC,
                        'MFAudioFormat_AAC',
                        'WAVE_FORMAT_MPEG_HEAAC',
                        WAVE_FORMAT_MPEG_HEAAC,
                        'High-Efficiency Advanced Audio Coding (HE-AAC).') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_ADTS,
                        'MFAudioFormat_ADTS',
                        'WAVE_FORMAT_MPEG_ADTS_AAC',
                        WAVE_FORMAT_MPEG_ADTS_AAC,
                        'Advanced Audio Coding (AAC) in Audio Data Transport Stream (ADTS) format.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_AMR_NB,
                        'MFAudioFormat_AMR_NB',
                        'WAVE_FORMAT_AMR_NB',
                        WAVE_FORMAT_AMR_NB,
                        'Adaptive Multi-Rate Narrowband (NB) audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_AMR_WB,
                        'MFAudioFormat_AMR_WB',
                        'WAVE_FORMAT_AMR_WB',
                        WAVE_FORMAT_AMR_WB,
                        'ITU-T G.722.2, Adaptive Multi-Rate Wideband (WB) audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_AMR_WP,
                        'MFAudioFormat_AMR_WP',
                        'WAVE_FORMAT_AMR_WP',
                        WAVE_FORMAT_AMR_WP,
                        'ITU-T G.722.2, Adaptive Multi-Rate Wideband Plus (WP) audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_FLAC,
                        'MFAudioFormat_FLAC',
                        'WAVE_FORMAT_FLAC',
                        WAVE_FORMAT_FLAC,
                        'Free Lossless Audio Codec (FLAC).') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_ALAC,
                        'MFAudioFormat_ALAC',
                        'WAVE_FORMAT_ALAC',
                        WAVE_FORMAT_ALAC,
                        'Apple Lossless Audio Codec (ALAC).') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Opus,
                        'MFAudioFormat_Opus',
                        'WAVE_FORMAT_OPUS',
                        WAVE_FORMAT_OPUS,
                        'Opus Interactive Audio Codec. (https://xiph.org)') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC4,
                        'MFAudioFormat_Dolby_AC4',
                        'WAVE_FORMAT_DOLBY_AC4',
                        WAVE_FORMAT_DOLBY_AC4,
                        'Dolby lossy audio compression format (AC-4) that can contain audio channels and/or audio objects.') then
    goto done;
  if IfEqualReturnProps(MEDIASUBTYPE_RAW_AAC1,
                        'MEDIASUBTYPE_RAW_AAC1',
                        'WAVE_FORMAT_RAW_AAC1',
                        WAVE_FORMAT_RAW_AAC1,
                        'Advanced Audio Coding (AAC). This subtype is used for AAC contained in an AVI file.') then
    goto done;

  // The following audio types are not derived from an existing FormatTag ( = FOURCC)
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC3,
                        'MFAudioFormat_Dolby_AC3',
                        'AC-3',
                        FCC('AC-3'),
                        'Dolby Digital (also known as AC-3) lossy audio compression format.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_DDPlus,
                        'MFAudioFormat_Dolby_DDPlus',
                        'NONE',
                        0,
                        'Dolby Digital Plus (also known as E-AC-3) lossy audio codec based on Dolby Digital that is backward compatible.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC4_V1,
                        'MFAudioFormat_Dolby_AC4_V1',
                        'NONE',
                        0,
                        'Dolby AC-4 bitstream versions 0 and 1 audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC4_V2,
                        'MFAudioFormat_Dolby_AC4_V2',
                        'NONE',
                        0,
                        'Dolby AC-4 bitstream version 2 audio codec. (Supports Immersive Stereo.)') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC4_V1_ES,
                        'MFAudioFormat_Dolby_AC4_V1_ES',
                        'NONE',
                        0,
                        'Dolby version 1 lossy audio format used for AC-4 streams that use ac4_syncframe and the optional crc at the end of each frame.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC4_V2_ES,
                        'MFAudioFormat_Dolby_AC4_V2_ES',
                        'NONE',
                        0,
                        'Dolby version 2 lossy audio format used for AC-4 streams that use ac4_syncframe and the optional crc at the end of each frame.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Vorbis,
                        'MFAudioFormat_Vorbis',
                        'NONE',
                        0,
                        'Vorbis audio codec based on Modified Discrete Cosine Transform. (https://xiph.org)') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS_RAW,
                        'MFAudioFormat_DTS_RAW',
                        'NONE',
                        0,
                        'Digital Theater Systems (DTS) raw audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS_HD,
                        'MFAudioFormat_DTS_HD',
                        'NONE',
                        0,
                        'Digital Theater Systems (DTS) High Definition audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS_XLL,
                        'MFAudioFormat_DTS_XLL',
                        'NONE',
                        0,
                        'Digital Theater Systems (DTS) XLL audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS_LBR,
                        'MFAudioFormat_DTS_LBR',
                        'NONE',
                        0,
                        'Digital Theater Systems (DTS) Low Bitrate (LBR) audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS_UHD,
                        'MFAudioFormat_DTS_UHD',
                        'NONE',
                        0,
                        'Digital Theater Systems (DTS) Ultra High Definition (UHD) audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_DTS_UHDY,
                        'MFAudioFormat_DTS_UHDY',
                        'NONE',
                        0,
                        'Digital Theater Systems (DTS) Ultra High Definition (UHDY) audio codec.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Float_SpatialObjects,
                        'MFAudioFormat_Float_SpatialObjects',
                        'NONE',
                        0,
                        'Uncompressed IEEE floating-point audio.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_LPCM,
                        'MFAudioFormat_LPCM',
                        'NONE',
                        0,
                        'LPCM audio with headers for encapsulation in an MPEG2 bitstream.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_PCM_HDCP,
                        'MFAudioFormat_PCM_HDCP',
                        'NONE',
                        0,
                        'Uncompressed PCM audio. (High-bandwidth Digital Content Protection)') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Dolby_AC3_HDCP,
                        'MFAudioFormat_Dolby_AC3_HDCP',
                        'NONE',
                        0,
                        'Dolby Digital, also called Dolby AC-3 (High-bandwidth Digital Content Protection)') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_AAC_HDCP,
                        'MFAudioFormat_AAC_HDCP',
                        'NONE',
                        0,
                        'High-Efficiency Advanced Audio Coding (HE-AAC)(High-bandwidth Digital Content Protection).') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_ADTS_HDCP,
                        'MFAudioFormat_ADTS_HDCP',
                        'NONE',
                        0,
                        'Advanced Audio Coding (AAC) in Audio Data Transport Stream (ADTS) (High-bandwidth Digital Content Protection) format.') then
    goto done;
  if IfEqualReturnProps(MFAudioFormat_Base_HDCP,
                        'MFAudioFormat_Base_HDCP',
                        'NONE',
                        0,
                        'Base HDCP (High-bandwidth Digital Content Protection) audio.') then
    goto done;

  // Video Formats
  if IfEqualReturnProps(MFVideoFormat_H264_HDCP,
                        'MFVideoFormat_H264_HDCP',
                        'H264',
                        FCC('H264'),
                        'H.264 (High-bandwidth Digital Content Protection).') then
    goto done;
  if IfEqualReturnProps(MFVideoFormat_HEVC_HDCP,
                        'MFVideoFormat_HEVC_HDCP',
                        'HEVC',
                        FCC('HEVC'),
                        'H.265/HEVC content in Annex B format (High-bandwidth Digital Content Protection) and can be used in mp4 and m2ts files.') then
    goto done;

done:
  Result := hr;
  aGuidName := sGuidName;
  aFormatTag := sFormatTag;
  aFOURCC := dwFOURCC;
  aFmtDesc := sFmtDesc;
end;


/// <summary>Deprecated, renamed to IsMfSupportedInputFormat</summary>
function IsMfSupportedFormat(const pSubType: TGuid): Boolean; inline;
begin
  Result := IsMftSupportedInputFormat(pSubType);
end;

/// <summary>Get supported subtype formats for input.</summary>
/// <seealso href="https://learn.microsoft.com/en-us/windows/win32/medfound/video-processor-mft#input-formats">[Video Processor MFT]</seealso>
function IsMftSupportedInputFormat(const pSubType: TGuid): Boolean; inline;
var
  bRes: Boolean;
  arSubTypes: array [0..38] of TGuid;
  i: Integer;

label
  done;

begin
  bRes := False;

  // Supported video subtype formats for input.
  // See also: https://learn.microsoft.com/en-us/windows/win32/medfound/video-processor-mft#input-formats
  // Note:
  //   Not every combination of input and output formats is supported.
  //   To test whether a conversion is supported,
  //   set the input type and then call IMFTransform.GetOutputAvailableType.

  arSubTypes[0]  := MFVideoFormat_ARGB32;
  arSubTypes[1]  := MFVideoFormat_RGB24;
  arSubTypes[2]  := MFVideoFormat_RGB32;
  arSubTypes[3]  := MFVideoFormat_RGB555;
  arSubTypes[4]  := MFVideoFormat_RGB565;
  arSubTypes[5]  := MFVideoFormat_RGB8;
  arSubTypes[6]  := MFVideoFormat_AYUV;
  arSubTypes[7]  := MFVideoFormat_I420;
  arSubTypes[8]  := MFVideoFormat_IYUV;
  arSubTypes[9]  := MFVideoFormat_NV11;
  arSubTypes[10] := MFVideoFormat_NV12;
  arSubTypes[11] := MFVideoFormat_UYVY;
  arSubTypes[12] := MFVideoFormat_V216;
  arSubTypes[13] := MFVideoFormat_V410;
  arSubTypes[14] := MFVideoFormat_Y41P;
  arSubTypes[15] := MFVideoFormat_Y41T;
  arSubTypes[16] := MFVideoFormat_Y42T;
  arSubTypes[17] := MFVideoFormat_YUY2;
  arSubTypes[18] := MFVideoFormat_YV12;
  arSubTypes[19] := MFVideoFormat_YVYU;
  arSubTypes[20] := MFVideoFormat_WMV1;
  arSubTypes[21] := MFVideoFormat_WMV2;
  arSubTypes[22] := MFVideoFormat_WMV3;
  arSubTypes[23] := MFVideoFormat_H263;
  arSubTypes[24] := MFVideoFormat_H264;
  arSubTypes[25] := MFVideoFormat_H265;
  arSubTypes[26] := MFVideoFormat_HEVC;
  arSubTypes[27] := MFVideoFormat_HEVC_ES;

  // Supported audio subtype codecs for input.
  arSubTypes[28] := MFAudioFormat_AAC;
  arSubTypes[29] := MFAudioFormat_MP3;
  arSubTypes[30] := MFAudioFormat_FLAC;
  arSubTypes[31] := MFAudioFormat_PCM;
  arSubTypes[32] := MFAudioFormat_WMAudioV8;
  arSubTypes[33] := MFAudioFormat_WMAudioV9;
  arSubTypes[34] := MFAudioFormat_MPEG;
  arSubTypes[35] := MFAudioFormat_Opus;
  arSubTypes[36] := MFAudioFormat_Dolby_AC4;
  arSubTypes[37] := MFAudioFormat_Dolby_AC3;
  arSubTypes[38] := MFAudioFormat_Vorbis;

  for i := 0 to Length(arSubTypes) -1 do
    begin
      if IsEqualGuid(pSubType,
                     arSubTypes[i]) then
        begin
          bRes := True;
          goto done;
        end;
    end;

done:
  Result := bRes;
end;

/// <summary>Get supported subtype formats for ounput.</summary>
/// <seealso href="https://learn.microsoft.com/en-us/windows/win32/medfound/video-processor-mft#input-formats">[Video Processor MFT]</seealso>
function GetSupportedMftOutputFormats(): TArray<TGuid>;
begin

  SetLength(Result,
            13);
  // Supported subtype formats for output.
  // See: https://learn.microsoft.com/en-us/windows/win32/medfound/video-processor-mft#output-formats
  // Note:
  //   Not every combination of input and output formats is supported.
  //   To test whether a conversion is supported,
  //   set the input type and then call IMFTransform.GetOutputAvailableType.
  Result[0]  := MFVideoFormat_ARGB32;
  Result[1]  := MFVideoFormat_AYUV;
  Result[2]  := MFVideoFormat_I420;
  Result[3]  := MFVideoFormat_IYUV;
  Result[4]  := MFVideoFormat_NV12;
  Result[5]  := MFVideoFormat_RGB24;
  Result[6]  := MFVideoFormat_RGB32;
  Result[7]  := MFVideoFormat_RGB555;
  Result[8]  := MFVideoFormat_RGB565;
  Result[9]  := MFVideoFormat_UYVY;
  Result[10] := MFVideoFormat_V216;
  Result[11] := MFVideoFormat_YUY2;
  Result[12] := MFVideoFormat_YV12;
end;


function IsMftSupportedOutputFormat(const pSubType: TGuid;
                                    out IsTopDown: Boolean): Boolean; inline;
var
  arSubTypes: TArray<TGuid>;
  i, j: Integer;

begin
  Result := False;
  IsTopDown := False;
  arSubTypes := GetSupportedMftOutputFormats();

  for i := 0 to Length(arSubTypes) -1 do
    begin
      if IsEqualGuid(pSubType,
                     arSubTypes[i]) then
        begin
          for j := 0 to 2 do
            begin
              if (pSubType = MFVideoFormat_AYUV) or
                 (pSubType = MFVideoFormat_IYUV) or
                 (pSubType = MFVideoFormat_YUY2) then
                IsTopDown := True;
            end;
          Result := True;
          Break;
        end;
    end;
end;



// Device Loss
// ===========
// Allways call UnregisterDeviceNotification (Windows) when finnished
function RegisterForDeviceNotification(hw: HWND;
                                       out g_hdevnotify: HDEVNOTIFY): Bool;
var
  devbroadcastdevice: DEV_BROADCAST_DEVICEINTERFACE;
  iSize: Integer;

begin
  if (hw > 0) then
    begin
      iSize := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
      ZeroMemory(@devbroadcastdevice,
                 iSize);

      devbroadcastdevice.dbcc_size := iSize;
      devbroadcastdevice.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
      devbroadcastdevice.dbcc_reserved := 0;
      devbroadcastdevice.dbcc_classguid := KSCATEGORY_VIDEO_CAMERA; // KSCATEGORY_CAPTURE : Since windows 10 you should not use this guid to register for device loss! Otherwise it will return a wrong symoliclink when detecting a device lost.
      devbroadcastdevice.dbcc_name := #0;

      g_hdevnotify := RegisterDeviceNotification(hw,
                                                 @devbroadcastdevice,
                                                 DEVICE_NOTIFY_WINDOW_HANDLE);
    end;
  Result := Assigned(g_hdevnotify);
end;


//
function UnRegisterForDeviceNotification(g_hdevnotify: HDEVNOTIFY): Bool;
begin
  Result := True;
  if Assigned(g_hdevnotify) then
    Result := UnregisterDeviceNotification(g_hdevnotify);
end;


// Device SymLink & FriendlyName
//==============================
function GetSymbolicLink(pActivate: IMFActivate;
                         out g_pwszSymbolicLink: PWideChar;
                         out g_cchSymbolicLink: UINT32;
                         devMediaType: TGUID): HResult;
begin

  if IsEqualGuid(devMediaType, MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK) or
     IsEqualGuid(devMediaType, MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK) then
    begin
      Result := (pActivate as IMFAttributes).GetAllocatedString(devMediaType,
                                                                g_pwszSymbolicLink,
                                                                g_cchSymbolicLink);
    end
  else
    Result := E_FAIL;
end;


//
function GetDeviceName(pActivate: IMFActivate;
                       out g_pwszDeviceName: PWideChar;
                       out g_cchDeviceName: UINT32): HResult; overload;
begin
  Result := (pActivate as IMFAttributes).GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                                                g_pwszDeviceName,
                                                                g_cchDeviceName);
end;

//
function GetDeviceNameFromCollection(DeviceCollection: IMMDeviceCollection;
                                     DeviceIndex: UINT): LPWSTR;
var
  device: IMMDevice;
  deviceId: PWideChar;
  hr: HResult;
  propertyStore: IPropertyStore;
  friendlyName: PROPVARIANT;
  deviceName: PWideChar;
  returnValue: PWideChar;

label
  done;

begin

  returnValue := nil;

  hr := DeviceCollection.Item(DeviceIndex,
                                device);
  if FAILED(hr) then
    goto done;

  hr := device.GetId(deviceId);
  if FAILED(hr) then
    goto done;

  hr := device.OpenPropertyStore($00000000, // STGM_READ
                                 propertyStore);
  SafeRelease(device);

  if FAILED(hr) then
    goto done;

  PropVariantInit(friendlyName);

  hr := propertyStore.GetValue(DEVPKEY_Device_FriendlyName,
                               friendlyName);

  SafeRelease(propertyStore);

  if FAILED(hr) then
    goto done;

  if (friendlyName.vt <> VT_LPWSTR) then
    deviceName := 'Unknown'
  else
    deviceName := friendlyName.pwszVal;

  returnValue := StrNew(deviceName);

  PropVariantClear(friendlyName);
  CoTaskMemFree(deviceId);

done:
  Result := returnValue;

  if Assigned(returnValue) then
    StrDispose(returnValue);
end;



// Enable Video Acceleration
// =========================
function FindDeviceManager(pTopology: IMFTopology;          // Topology to search.
                           out ppDeviceManager: IInterface;     // Receives a pointer to the device manager.
                           out ppNode: IMFTopologyNode): HResult;
var
  hr: HResult;
  cNodes: WORD;
  bFound: Boolean;
  pNode: IMFTopologyNode;
  pNodeObject: IInterface;  // = IUnknown !
  pD3DManager: IDirect3DDeviceManager9;
  i: integer;

begin

  cNodes := 0;
  bFound := False;

  // Search all of the nodes in the topology.

  hr := pTopology.GetNodeCount(cNodes);

  if FAILED(hr) then
    begin
      Result := hr;
      Exit;
    end;

  for i := 0 to cNodes - 1 do
    begin
      // For each of the following calls, failure just means we
      // did not find the node we're looking for, so keep looking.

      hr := pTopology.GetNode(i,
                             pNode);

      // Get the node's object pointer.
      if SUCCEEDED(hr) then
        begin
          hr := pNode.GetObject(pNodeObject);
        end;

      // Query the node object for the device manager service.
      if SUCCEEDED(hr) then
        begin
          hr := MFGetService(pNodeObject,
                             MR_VIDEO_ACCELERATION_SERVICE,
                             IID_IDirect3DDeviceManager9,
                             Pointer(pD3DManager));
        end;

      if SUCCEEDED(hr) then
        begin
          // Found the right node. Return the pointers to the caller.
          ppDeviceManager := pD3DManager;
          ppNode := pNode;
          bFound := True;
          Break;
        end;

      SafeRelease(pNodeObject);
      SafeRelease(pD3DManager);
      SafeRelease(pNode);

    end; // End of for loop.

  if (bFound = True) then
    Result := S_OK
  else
    Result := E_FAIL;
end;


// Audio and video capture
//========================

//
// To enumerate the capture devices on the system, perform the following steps:
//  1 Call the MFCreateAttributes function to create an attribute store.
//  2 Set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE attribute to one of the following values:
//    Value	                                          Description
//    ----------------------------------------------  -------------------------------------
//    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID	Enumerate audio capture devices.
//    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID	Enumerate video capture devices.
//  3 Call the MFEnumDeviceSources function. This function allocates an array of IMFActivate pointers.
//    Each pointer represents an activation object for one device on the system.
//  4 Call the IMFActivate.ActivateObject method to create an instance of the media source from
//    one of the activation objects.
//

// The following fuction creates a media source for the given video capture device (iDeviceIndex) in
// the enumeration list:
//
function CreateVideoCaptureDevice(const iDeviceIndex: UINT32;
                                  out pSource: IMFMediaSource): HResult; overload;
var
  count: UINT32;
  i: Integer;
  pConfig: IMFAttributes;
  ppDevices: PIMFActivate; // Pointer to array of IMFActivate interfaces
  hr: HResult;

begin

  count := 0;

  // Create an attribute store to hold the search criteria.
  hr := MFCreateAttributes(pConfig,
                           1);

  // Request video capture devices.
  if SUCCEEDED(hr) then
    begin
      hr := pConfig.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
    end;

  // Enumerate the devices,
  if SUCCEEDED(hr) then
    begin
      hr := MFEnumDeviceSources(pConfig,
                                ppDevices,
                                count);
    end;

{$POINTERMATH ON}

  // Create a media source for the first device in the list.
  if SUCCEEDED(hr) then
    begin
      if (count > 0) and (iDeviceIndex <= count) then
        begin
          hr := ppDevices[iDeviceIndex].ActivateObject(IID_IMFMediaSource,
                                                       Pointer(pSource));
        end
      else
        begin
          hr := MF_E_NOT_FOUND;
        end;
    end;

  for i := 0 to count - 1 do
    SafeRelease(ppDevices[i]);
  CoTaskMemFree(ppDevices);

{$POINTERMATH OFF}
  Result := hr;
end;


//
function CreateVideoCaptureDevice(const pszSymbolicLink: LPCWSTR;
                                  out pSource: IMFMediaSource): HResult; overload;

var
  pAttributes: IMFAttributes;
  hr: HResult;

begin

  hr := MFCreateAttributes(pAttributes,
                           2);

  // Set the device type to video.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
    end;

  // Set the symbolic link.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                  LPCWSTR(pszSymbolicLink));
    end;

  if SUCCEEDED(hr) then
    begin
      hr := MFCreateDeviceSource(pAttributes,
                                 pSource);
    end;

  Result := hr;
end;

// Equivalent way to create an audio device from the audio endpoint ID:
// 1 Call MFCreateAttributes to create an attribute store.
// 2 Set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE attribute to
//   MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID.
// 3 Set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID attribute to the endpoint ID.
// 4 Call either the MFCreateDeviceSource or MFCreateDeviceSourceActivate function.
//
function CreateAudioCaptureDevice(const pszEndPointID: LPCWSTR;
                                  out pSource: IMFMediaSource): HResult;
var
  pAttributes: IMFAttributes;
  hr: HResult;

begin

  hr := MFCreateAttributes(pAttributes,
                           2);

  // Set the device type to audio.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID);
    end;

  // Set the endpoint ID.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID,
                                  pszEndPointID);
    end;

  if SUCCEEDED(hr) then
    begin
      hr := MFCreateDeviceSource(pAttributes,
                                 pSource);
    end;

  Result := hr;
end;

// You can query the activation objects for various attributes, including the following:
// * The MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME attribute contains the display name of the device.
//   The display name is suitable for showing to the user, but might not be unique.
// * For video devices, the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK
//   attribute contains the symbolic link to the device.
//   The symbolic link uniquely identifies the device on the system, but is not a readable string.
// * For audio devices, the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID
//   attribute contains the audio endpoint ID of the device.
//   The audio endpoint ID is similar to a symbolic link.
//   It uniquely identifies the device on the system, but is not a readable string.
//
// The following function takes an array of IMFActivate pointers and copies the
// the name of each device to the stringlist:
//
procedure ListDeviceNames(ppDevices: PIMFActivate;
                          out iList: TStringList);
var
  i: Integer;
  hr: HResult;
  szFriendlyName: LPWSTR;
  cchName: UINT32;
  iCount: UINT32;

begin
  cchName := 0;
  iList.Clear;

{$POINTERMATH ON}

  hr := ppDevices[0].GetCount(iCount);

  if SUCCEEDED(hr) then
    for i := 0 to iCount - 1 do
      begin
        szFriendlyName := '';

        // Try to get the display name.
        hr := (ppDevices[i] as IMFAttributes).GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                                                 szFriendlyName,
                                                                 cchName);
        // Add the fiendlyname to the stringlist.
        if SUCCEEDED(hr) then
          iList.Append(szFriendlyName);

        szFriendlyName := nil;

    end;
{$POINTERMATH OFF}
end;


//
function EnumAudioRenderingDevices(nDevice: UInt;
                                   out wstrID: PWideChar; // Device ID.
                                   pSink: IMFMediaSink = nil;  // Streaming audio renderer (SAR)
                                   pActivate: IMFActivate = nil // Activation object, which can be used to create the SAR.
                                   ): HResult;
var
  hr: HResult;
  pEnum: IMMDeviceEnumerator;       // Audio device enumerator.
  pDevices: IMMDeviceCollection;    // Audio device collection.
  pDevice: IMMDevice;               // An audio device.
  pAttributes: IMFAttributes;       // Attribute store.

begin

  // Create the device enumerator.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnum);

  // Enumerate the rendering devices.
  if SUCCEEDED(hr) then
    hr := pEnum.EnumAudioEndpoints(eRender,
                                   DEVICE_STATE_ACTIVE,
                                   pDevices);

  // Get ID of the first device in the list by default or the nDevice value.
  if SUCCEEDED(hr) then
    hr := pDevices.Item(nDevice,
                        pDevice);

  if SUCCEEDED(hr) then
    hr := pDevice.GetId(wstrID);

  // Create the activation object for the SAR if needed ie when streaming protected content.
  if Assigned(pActivate) then
    begin
      hr := MFCreateAudioRendererActivate(pActivate);
      if SUCCEEDED(hr) then
        hr := pActivate.SetString(MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID,
                                  wstrID);
    end
  else  // For unprotected content
    begin
      // Create an attribute store and set the device ID attribute.
      if SUCCEEDED(hr) then
        hr := MFCreateAttributes(pAttributes,
                                 1);

      if SUCCEEDED(hr) then
        hr := pAttributes.SetString(MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID,
                                    wstrID);

      // Create the audio renderer.
      if SUCCEEDED(hr) then
        hr := MFCreateAudioRenderer(pAttributes,
                                    pSink);
    end;

  CoTaskMemFree(wstrID);
  Result := hr;
end;


//
function SetMaxFrameRate(pSource: IMFMediaSource;
                         dwTypeIndex: DWORD): HResult;
var
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  hr: HResult;
  fSelected: BOOL;
  propvar: PROPVARIANT;

label
  done;

begin
  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(dwTypeIndex,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  hr := pHandler.GetCurrentMediaType(pType);
  if FAILED(hr) then
    goto done;

  // Get the maximum frame rate for the selected capture format.

  // Note: To get the minimum frame rate, use the
  // MF_MT_FRAME_RATE_RANGE_MIN attribute instead.

  if SUCCEEDED(pType.GetItem(MF_MT_FRAME_RATE_RANGE_MAX,
                             propvar)) then
    begin
      hr := pType.SetItem(MF_MT_FRAME_RATE,
                          propvar);

      PropVariantClear(propvar);

      if FAILED(hr) then
        goto done;

      hr := pHandler.SetCurrentMediaType(pType);
    end;

done:
  Result := hr;
end;


//
function GetBitmapInfoHeaderFromMFMediaType(pType: IMFMediaType;     // Pointer to the media type.
                                            out ppBmih: PBITMAPINFOHEADER; // Receives a pointer to the structure.
                                            out pcbSize: DWORD // Receives the size of the structure.
                                            ): HResult;
var
  majorType: TGUID;
  pmt: PAM_MEDIA_TYPE;
  cbSize: DWORD;
  cbOffset: DWORD;
  pBMIH: PBITMAPINFOHEADER;
  hr: HResult;
  ulcbOffset: ULONG;

label
  done;

begin
  pBMIH := nil;
  ppBmih := nil;
  pcbSize := 0;

  majorType := GUID_NULL;
  pmt := nil;

  // Verify that this is a video type.
  hr := pType.GetMajorType(majorType);
  if FAILED(hr) then
    goto done;

  if not isEqualGuid(majorType,
                     MFMediaType_Video) then
    begin
      hr := MF_E_INVALIDMEDIATYPE;
      goto done;
    end;

  hr := pType.GetRepresentation(AM_MEDIA_TYPE_REPRESENTATION,
                                Pointer(pmt));
  if FAILED(hr) then
    goto done;


  // To avoid use of RTTI (to make a kind of FIELD_OFFSET work) we have have luck, because
  // both  recordfields are at the end of the VIDEOINFOHEADER(2) records.
  // This makes calculating the recordfieldoffset a bit easier.
  if IsEqualGuid(pmt.formattype,
                 FORMAT_VideoInfo) then
    cbOffset := SizeOf(VIDEOINFOHEADER) - SizeOf(WinApi.AmVideo.VIDEOINFOHEADER)
  else if (pmt.formattype = FORMAT_VideoInfo2) then
    cbOffset := SizeOf(VIDEOINFOHEADER2) - SizeOf(WinApi.DvdMedia.VIDEOINFOHEADER2)
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE; // Unsupported format type.
      goto done;
    end;

    if ((pmt.cbFormat - cbOffset) < SizeOf(BITMAPINFOHEADER)) then
      begin
        hr := E_UNEXPECTED; // Bad format size.
        goto done;
      end;

    cbSize := (pmt.cbFormat - cbOffset);

    pBMIH := CoTaskMemAlloc(cbSize);
    if (pBMIH = nil) then
      begin
        hr := E_OUTOFMEMORY;
        goto done;
      end;

    ulcbOffset := (pmt.cbFormat + cbOffset);

    CopyMemory(pBMIH,
               @ulcbOffset,
               cbSize);

    ppBmih := pBMIH;
    pcbSize := cbSize;

done:
  if assigned(pmt) then
    pType.FreeRepresentation(AM_MEDIA_TYPE_REPRESENTATION,
                             pmt);
  if Assigned(pBMIH) then
    CoTaskMemFree(pBMIH);

  Result := hr;

end;


//
function CopyAttribute(pSrc: IMFAttributes;
                       var pDest: IMFAttributes;
                       const key: TGUID): HResult;
var
  pvar: PROPVARIANT;
  hr: HResult;

begin
  if Assigned(pSrc) then
    begin
      PropVariantInit(pvar);
      hr := pSrc.GetItem(key,
                         pvar);
      if (SUCCEEDED(hr)) then
        hr := pDest.SetItem(key,
                            pvar);
      PropVariantClear(pvar);
    end
  else
    hr := E_POINTER;
  Result := hr;
end;


function CopyAttribute(pSrc: IMFMediaType;
                       var pDest: IMFMediaType;
                       const key: TGUID): HResult;
var
  pvar: PROPVARIANT;
  hr: HResult;

begin
  if Assigned(pSrc) then
    begin
      PropVariantInit(pvar);
      hr := pSrc.GetItem(key,
                         pvar);
      if (SUCCEEDED(hr)) then
        hr := pDest.SetItem(key,
                            pvar);
      PropVariantClear(pvar);
    end
  else
    hr := E_POINTER;
  Result := hr;
end;


// Creates a compatible video format with a different subtype if param guidSubType <> GUID_NULL else
// the SubType will be the source subtype.
function CloneVideoMediaType(pSrcMediaType: IMFMediaType;
                             const guidSubType: REFGUID;
                             out ppNewMediaType: IMFMediaType): HResult;
var
  hr: HResult;
  rGuid: REFGUID;
  tmpMediaType: IMFMediaType;

 // For debug use.
 // {$IFDEF DEBUG}
 // FMediaTypeDebug: TMediaTypeDebug;
 // {$ENDIF}

label
  done;

begin

// For debug use.
//  {$IFDEF DEBUG}
//  FMediaTypeDebug := TMediaTypeDebug.Create();
//  FMediaTypeDebug.LogMediaType(pSrcMediaType);
//  FMediaTypeDebug.SafeDebugResultsToFile('MFMetLib CloneVideoMediaType');
//  FreeAndNil(FMediaTypeDebug);
//  {$ENDIF}

  hr := MFCreateMediaType(tmpMediaType);
  if (FAILED(hr)) then
    goto done;

  if (guidSubType = GUID_NULL) then
    hr := pSrcMediaType.GetGUID(MF_MT_SUBTYPE,
                                rGuid)
  else
    hr := tmpMediaType.SetGUID(MF_MT_SUBTYPE,
                               guidSubType);
  if (FAILED(hr)) then
    goto done;

  hr := tmpMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                             MFMediaType_Video);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      tmpMediaType,
                      MF_MT_FRAME_SIZE);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      tmpMediaType,
                      MF_MT_FRAME_RATE);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      tmpMediaType,
                      MF_MT_PIXEL_ASPECT_RATIO);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      tmpMediaType,
                      MF_MT_INTERLACE_MODE);
  if (FAILED(hr)) then
    goto done;

  ppNewMediaType := tmpMediaType;

done:
  Result := hr;
end;


//
function CreatePhotoMediaType(const psubTypeGuid: TGuid; {can be one of the following: MFImageFormat_RGB32, MFImageFormat_JPEG or WIC guidContainerFormats like GUID_ContainerFormatBmp etc.}
                              var pPhotoMediaType: IMFMediaType): HResult;
var
  hr: HResult;
  mfPhotoMediaType: IMFMediaType;

label
  done;

begin

  hr := MFCreateMediaType(mfPhotoMediaType);
  if (FAILED(hr)) then
    goto done;

  hr := mfPhotoMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Image);
  if (FAILED(hr)) then
    goto done;

  hr := mfPhotoMediaType.SetGUID(MF_MT_SUBTYPE,
                                 pSubTypeGuid);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(mfPhotoMediaType,
                      pPhotoMediaType,
                      MF_MT_FRAME_SIZE);
  if (FAILED(hr)) then
    goto done;

  pPhotoMediaType := mfPhotoMediaType;

done:
  Result := hr;
end;

// overloaded function
function CreatePhotoMediaType(const psubTypeGuid: TGuid;
                              pSrcMediaType: IMFMediaType;
                              out ppPhotoMediaType: IMFMediaType): HResult;
var
  hr: HResult;
  pPhotoMediaType: IMFMediaType;

label
  done;
begin

  ppPhotoMediaType := nil;

  hr := MFCreateMediaType(pPhotoMediaType);
  if (FAILED(hr)) then
    goto done;

  hr := pPhotoMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Image);
  if (FAILED(hr)) then
    goto done;

  hr := pPhotoMediaType.SetGUID(MF_MT_SUBTYPE,
                                psubTypeGuid);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      pPhotoMediaType,
                      MF_MT_FRAME_SIZE);
  if (FAILED(hr)) then
    goto done;

  ppPhotoMediaType := pPhotoMediaType;

done:
  Result := hr;
end;

//
function GetFrameRate(pType: IMFMediaType;
                      out uiNumerator: UINT32;
                      out uiDenominator: UINT32): HResult; inline;
begin
  Result := MFGetAttribute2UINT32asUINT64(pType,
                                          MF_MT_FRAME_RATE,
                                          uiNumerator,
                                          uiDenominator);
end;


//
function SetFrameRate(pType: IMFMediaType;
                      uiNumerator: UINT32;
                      uiDenominator: UINT32): HResult; inline;
begin
  Result := MFSetAttribute2UINT32asUINT64(pType,
                                          MF_MT_FRAME_RATE,
                                          uiNumerator,
                                          uiDenominator);
end;


//
function GetFrameRateFromRatio(uiNumerator: UINT32;
                               uiDenominator: UINT32): FLOAT; inline;
begin
  if (uiNumerator > 0) and (uiDenominator > 0) then
    Result := (uiNumerator / uiDenominator)
  else
    Result := 0.0;
end;


//
function GetFrameSize(pAttributes: IMFAttributes;
                      out uiWidth: UINT32;
                      out uiHeigth: UINT32): HResult; inline;
begin
  Result := MFGetAttributeSize(pAttributes,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeigth);
end;

//
function GetFrameSizeFromMediaType(pType: IMFMediaType;
                                   out uiWidth: UINT32;
                                   out uiHeight: UINT32): HResult; inline;
begin
  Result := MFGetAttributeSize(pType,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeight);
end;

//
function SetFrameSize(pAttributes: IMFAttributes;
                      uiWidth: UINT32;
                      uiHeigth: UINT32): HResult; inline;
begin
  Result := MFSetAttributeSize(pAttributes,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeigth);
end;

//
function SetFrameSizeOnMediaType(pType: IMFMediaType;
                                 uiWidth: UINT32;
                                 uiHeigth: UINT32): HResult; inline;
begin
  Result := MFSetAttributeSize(pType,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeigth);
end;


//
function GetEncodingBitrate(pType: IMFMediaType;
                            out uiEncodingBitrate: UINT32): HResult; inline;
var
  uiWidth: UINT32;
  uiHeight: UINT32;
  uiBitrate: Single;
  uiFrameRateNum: UINT32;
  uiFrameRateDenom: UINT32;
  hr: HResult;

label
  done;

begin

  hr := GetFrameSize(pType,
                     uiWidth,
                     uiHeight);
  if FAILED(hr) then
    goto done;

  hr := GetFrameRate(pType,
                     uiFrameRateNum,
                     uiFrameRateDenom);
  if FAILED(hr) then
    goto done;

  uiBitrate := (((uiWidth / 3.0) * uiHeight) * uiFrameRateNum) / uiFrameRateDenom;

  uiEncodingBitrate := Round(uiBitrate);

done:
  Result := hr;
end;


//
function GetPixelAspectRatio(pAttributes: IMFAttributes;
                             out uiNumerator: UINT32;
                             out uiDenominator: UINT32): HResult; inline;
begin
  Result := MFGetAttributeRatio(pAttributes,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                uiNumerator,
                                uiDenominator);
end;


//
function SetPixelAspectRatio(pAttributes: IMFAttributes;
                             uiNumerator: UINT32;
                             uiDenominator: UINT32): HResult; inline;
begin
  Result := MFGetAttributeRatio(pAttributes,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                uiNumerator,
                                uiDenominator);
end;

// MF_MT_PAD_CONTROL_FLAGS Specifies the aspect ratio of the output rectangle for a video media type.
function SetOutputRectangleAspectRatio(pAttributes: IMFAttributes;
                                       stVideoPadFlags: MFVideoPadFlags = MFVideoPadFlag_PAD_TO_None): HResult; inline;
begin
  Result := MFGetAttributeUINT32(pAttributes,
                                 MF_MT_PAD_CONTROL_FLAGS,
                                 UINT32(stVideoPadFlags));
end;


function GetVideoDisplayArea(pType: IMFMediaType;
                             out pArea: MFVideoArea): HResult;
var
  hr: HResult;
  bPanScan: UINT32;
  pWidth: UINT32;
  pHeight: UINT32;

begin

  hr := S_OK;
  pWidth := 0;
  pHeight := 0;

  bPanScan := MFGetAttributeUINT32(pType,
                                   MF_MT_PAN_SCAN_ENABLED,
                                   {False} UINT32(0));

  // In pan-and-scan mode, try to get the pan-and-scan region.
  if (bPanScan <> 0) then
    hr := pType.GetBlob(MF_MT_PAN_SCAN_APERTURE,
                       @pArea,
                       SizeOf(MFVideoArea),
                       nil);

  // If not in pan-and-scan mode, or the pan-and-scan region is not set,
  // get the minimimum display aperture.

  if (bPanScan = 0) or (hr = MF_E_ATTRIBUTENOTFOUND) then
    begin
      hr := pType.GetBlob(MF_MT_MINIMUM_DISPLAY_APERTURE,
                          @pArea,
                          SizeOf(MFVideoArea),
                          nil);

      if hr = MF_E_ATTRIBUTENOTFOUND then
        begin
          // Minimum display aperture is not set.

          // For backward compatibility with some components,
          // check for a geometric aperture.

          hr := pType.GetBlob(MF_MT_GEOMETRIC_APERTURE,
                              @pArea,
                              SizeOf(MFVideoArea),
                              nil);
        end;

      // Default: Use the entire video area.

      if (hr = MF_E_ATTRIBUTENOTFOUND) then
        begin
          hr := MFGetAttributeSize(pType,
                                   MF_MT_FRAME_SIZE,
                                   pWidth,
                                   pHeight);

          if SUCCEEDED(hr) then
            pArea := MakeArea(0.0,
                              0.0,
                              pWidth,
                              pHeight);
        end;
    end;
  Result := hr;
end;


// Converts a rectangle from one pixel aspect ratio (PAR) to another PAR.
// Returns the corrected rectangle.
//
// For example, a 720 x 486 rect with a PAR of 9:10, when converted to 1x1 PAR,
// must be stretched to 720 x 540.
function CorrectAspectRatio(const src: TRect;
                            const srcPAR: MFRatio;
                            const destPAR: MFRatio): TRect;
var
  rc: TRect;

begin
  // Start with a rectangle the same size as src, but offset to (0,0).
  //rc := {0, 0, src.right - src.left, src.bottom - src.top};
  rc.Left := 0;
  rc.Top := 0;
  rc.Right := src.Right - src.Left;
  rc.Bottom := src.Bottom - src.Top;

  // If the source and destination have the same PAR, there is nothing to do.
  // Otherwise, adjust the image size, in two steps:
  //  1. Transform from source PAR to 1:1
  //  2. Transform from 1:1 to destination PAR.

  if (srcPAR.Numerator <> destPAR.Numerator) or
     (srcPAR.Denominator <> destPAR.Denominator) then
    begin
      // Correct for the source's PAR.

      if (srcPAR.Numerator > srcPAR.Denominator) then
        begin
          // The source has "wide" pixels, so stretch the width.
          rc.right := MulDiv(rc.Right,
                             srcPAR.Numerator,
                             srcPAR.Denominator);
        end
      else if (srcPAR.Numerator < srcPAR.Denominator) then
        begin
          // The source has "tall" pixels, so stretch the height.
          rc.Bottom := MulDiv(rc.Bottom,
                              srcPAR.Denominator,
                              srcPAR.Numerator);
        end;
      // else: PAR is 1:1, which is a no-op.

      // Next, correct for the target's PAR. This is the inverse operation of
      // the previous.
      if (destPAR.Numerator > destPAR.Denominator) then
        begin
            // The destination has "wide" pixels, so stretch the height.
            rc.bottom := MulDiv(rc.bottom,
                                destPAR.Numerator,
                                destPAR.Denominator);
        end
      else if (destPAR.Numerator < destPAR.Denominator) then
        begin
          // The destination has "tall" pixels, so stretch the width.
          rc.right := MulDiv(rc.Right,
                             destPAR.Denominator,
                             destPAR.Numerator);
        end;
      // else: PAR is 1:1, which is also a no-op.
    end;
  Result := rc;
end;


function LetterBoxRect(const rcSrc: TRect;
                       const rcDst: TRect): TRect;
var
  iSrcWidth: Integer;
  iSrcHeight: Integer;
  iDstWidth: Integer;
  iDstHeight: Integer;
  iDstLBWidth: Integer;
  iDstLBHeight: Integer;
  lLeft: LONG;
  lTop: LONG;
  rc: TRect;

begin
  // Compute source/destination ratios.
  iSrcWidth  := rcSrc.Right - rcSrc.left;
  iSrcHeight := rcSrc.Bottom - rcSrc.top;
  iDstWidth  := rcDst.Right - rcDst.Left;
  iDstHeight := rcDst.Bottom - rcDst.Top;

  if (MulDiv(iSrcWidth,
             iDstHeight,
             iSrcHeight) <= iDstWidth) then
    begin
      // Column letterboxing ("pillar box")
      iDstLBWidth  := MulDiv(iDstHeight,
                             iSrcWidth,
                             iSrcHeight);
      iDstLBHeight := iDstHeight;
    end
  else
    begin
        // Row letterboxing.
        iDstLBWidth  := iDstWidth;
        iDstLBHeight := MulDiv(iDstWidth,
                               iSrcHeight,
                               iSrcWidth);
    end;

  // Create a centered rectangle within the current destination rect.
  lLeft := rcDst.Left + ((iDstWidth - iDstLBWidth) div 2);
  lTop := rcDst.Top + ((iDstHeight - iDstLBHeight) div 2);
  SetRect(rc,
          lLeft,
          lTop,
          lLeft + iDstLBWidth,
          lTop + iDstLBHeight);
  Result := rc;
end;


// Dumps the media buffer contents of an IMF sample to a stream.
// [in] pSample: pointer to the media sample to dump the contents from.
// [in] pStream: pointer to the stream to write to.
function WriteSampleToStream(pSample: IMFSample;
                             pStream: TMemoryStream): HResult;
var
  hr: HResult;
  strres: LongInt;
  MediaBuffer: IMFMediaBuffer;
  BufferLength: DWORD;
  ByteBuffer: PByte;
  BufferMaxLength: DWORD;
  BufferCurrLength: DWORD;

label
  done;

begin
  strres := 0;

  hr := pSample.ConvertToContiguousBuffer(@MediaBuffer);

  if SUCCEEDED(hr) then
    hr := MediaBuffer.GetCurrentLength(BufferLength);

  if SUCCEEDED(hr) then
    begin
      BufferMaxLength := 0;
      BufferCurrLength := 0;
      hr := MediaBuffer.Lock(ByteBuffer,
                             @BufferMaxLength,
                             @BufferCurrLength);
    end;
  if SUCCEEDED(hr) then
    strres := pStream.Write(ByteBuffer,
                            BufferLength);
  if (strres = 0) then
    hr := E_FAIL;

done:
  Result := hr;
end;


// Get an IMFMetadata pointer from a media source.
// Metadata contains descriptive information for the media content, such as title, artist, composer, and genre.
// Metadata can also describe encoding parameters.
// It can be faster to access this information through metadata than through media-type attributes.
function GetMetadata(pSource: IMFMediaSource;
                     out ppMetadata: IMFMetadata;
                     dwStream: DWORD): HResult;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;
  pProvider: IMFMetadataProvider;

label
  done;

begin

  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;


  hr := MFGetService(pSource,
                     MF_METADATA_PROVIDER_SERVICE,
                     IID_IMFMetadataProvider,
                     Pointer(pProvider));

  if FAILED(hr) then
    goto done;

  hr := pProvider.GetMFMetadata(pPD,
                                dwStream,
                                0,
                                ppMetadata);

done:
  Result := hr;
end;



// Returns the stream identifier from an active stream.
function GetActiveStreamIndex(stmediaType: TMediaTypes;          // [in] mediatype
                              pspd: IMFPresentationDescriptor;   // [in] presentation descriptor interface
                              out dwStreamId: DWORD): HResult;   // [out] stream identifier
var
  hr: HResult;
  sdCount: DWORD;
  i: Integer;
  pSourceSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  bSelected: BOOL;
  gStreamMajorType: TGuid;
  tmpMediaType: TMediaTypes;

begin

  hr := S_OK;

  // Check if IMFPresentationDescriptor is initialized
  if assigned(pspd) then
    begin
      // Count the streams
      hr := pspd.GetStreamDescriptorCount(sdCount);

      // Itterate through streams to get the given stream result
      for i := 0 to sdCount - 1 do
        begin
          //
          hr := pspd.GetStreamDescriptorByIndex(i,          // Zero-based index of the stream.
                                                bSelected,  // TRUE if the stream is currently selected, FALSE if the stream is currently deselected.
                                                pSourceSD); // Receives a pointer to the stream descriptor's IMFStreamDescriptor interface. The caller must release the interface.

          if SUCCEEDED(hr) and (bSelected = True) then
            begin
              // Get the media type handler interface
              hr := pSourceSD.GetMediaTypeHandler(pHandler);

              // Get Major guid from the stream
              if SUCCEEDED(hr) then
                pHandler.GetMajorType(gStreamMajorType);

              // Figure out what media type we are dealing with
              if SUCCEEDED(hr) then
                GetMediaDescription(gStreamMajorType,
                                    tmpMediaType);

              // Return streamID when both params match
              if (tmpMediaType = stmediaType) then
                begin
                  dwStreamId := i;
                  Break;
                end;
            end;
          SafeRelease(pSourceSD);
          SafeRelease(pHandler);
        end;
    end;
  Result := hr;
end;


// Retrieves information of the streams from a source
function GetStreamContents(pspd: IMFPresentationDescriptor;
                           mSource: IMFMediaSource;
                           var alsCont: TStreamContentsArray): HResult;
var
  hr: HResult;
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
      if SUCCEEDED(hr) then
        SetLength(alsCont,
                  sdCount);

      if SUCCEEDED(hr) then
        for i := 0 to sdCount - 1 do
          begin
             // Initialize the record
             alsCont[i].Reset();

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
                                      alsCont[i].audio_iBitsPerSample,
                                      alsCont[i].audio_iblockAlignment,
                                      alsCont[i].audio_AverageSampleRate,
                                      alsCont[i].audio_BitRate_kbps,
                                      alsCont[i].audio_SampleRate_khz);


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

          pwszValue := nil;
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
              // To calculate the framerate in FPS : uiNumerator / uiDenominator
              hr := GetFrameRate(pMediaType,
                                 uiNumerator,
                                 uiDenominator);
              alsCont[i].video_FrameRateNumerator := uiNumerator;
              alsCont[i].video_FrameRateDenominator := uiDenominator;

              // Get the pixel aspect ratio
              // To calculate the pixel aspect ratio: uiNumerator / uiDenominator
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


// Get the media type handler, enumerate the preferred media types, and set the media type.
function GetMediaType(pStreamDesc: IMFStreamDescriptor;
                      out tgMajorGuid: TGuid;
                      out bIsCompressedFormat: BOOL): HResult;
var
  hr: HResult;
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
          SafeRelease(pMediaType);
        end;
   end;
 Result := hr;
end;


//
function FindMatchingVideoType(pMediaTypeHandler: IMFMediaTypeHandler;
                               const gPixelFormat: TGUID;
                               pWidth: UINT32;
                               pHeight: UINT32;
                               pFps: UINT32;
                               out pOutMediaType: IMFMediaType): HResult;
var
  hr: HResult;
  i: Integer;
  pMediaType: IMFMediaType;
  mediaTypeCount: DWORD;
  subType: TGUID;
  uWidth: UINT32;
  uHeigth: UINT32;
  uFpsNumerator: UINT32;
  uFpsDenominator: UINT32;

label
  done;

begin
  hr := pMediaTypeHandler.GetMediaTypeCount(mediaTypeCount);

  if SUCCEEDED(hr) then
    for i := 0 to mediaTypeCount -1 do
      begin
        hr := pMediaTypeHandler.GetMediaTypeByIndex(i,
                                                    pMediaType);

        if SUCCEEDED(hr) then
          hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                                   subType);

        if SUCCEEDED(hr) then
          hr := MFGetAttributeSize(pMediaType,
                                   MF_MT_FRAME_SIZE,
                                   uWidth,
                                   uHeigth);

        if SUCCEEDED(hr) then
          hr := MFGetAttributeRatio(pMediaType,
                                    MF_MT_FRAME_RATE,
                                    uFpsNumerator,
                                    uFpsDenominator);

        if SUCCEEDED(hr) then
          if (IsEqualGUID(gPixelFormat,
                          subType)) and
             (uWidth = pWidth) and
             (uHeigth = pHeight) and
             (pFps = uFpsNumerator) and
             (uFpsDenominator = 1) then
            begin
              hr := pMediaType.CopyAllItems(pOutMediaType);

              if SUCCEEDED(hr) then
                begin
                  Safe_Release(pMediaType);
                  hr := S_OK;
                  Break;
                end;
            end
          else
            Safe_Release(pMediaType);
      end;
done:
  Result := hr;
end;


// Check if a given guid is a major type.
function IsMajorType(const guid: TGuid): Boolean;
begin
  if isEqualGuid(guid, MFMediaType_Default) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Audio) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Video) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Protected) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_SAMI) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Script) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Image) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_HTML) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Binary) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_FileTransfer) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Stream) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_MultiplexedFrames) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Subtitle) then
    Result := True
  else if isEqualGuid(guid, MFMediaType_Perception) then
    Result := True
  else
    Result := False;
end;

// Returns the mediatype associated with the Major guid.
// To get the major type call function GetMediaType
function GetMediaDescription(const pMajorGuid: TGuid;
                             out mtMediaType: TMediaTypes): HResult;
var
  hr: HResult;

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


//
function GetMajorTypeDescr(const pMajorGuid: TGuid): LPWSTR;
begin
  if isEqualGuid(pMajorGuid, MFMediaType_Default) then
    Result := 'MFMediaType_Default'
  else if isEqualGuid(pMajorGuid, MFMediaType_Audio) then
    Result := 'MFMediaType_Audio'
  else if isEqualGuid(pMajorGuid, MFMediaType_Video) then
    Result := 'MFMediaType_Video'
  else if isEqualGuid(pMajorGuid, MFMediaType_Protected) then
    Result := 'MFMediaType_Protected'
  else if isEqualGuid(pMajorGuid, MFMediaType_SAMI) then
    Result := 'MFMediaType_SAMI'
  else if isEqualGuid(pMajorGuid, MFMediaType_Script) then
    Result := 'MFMediaType_Script'
  else if isEqualGuid(pMajorGuid, MFMediaType_Image) then
    Result := 'MFMediaType_Image'
  else if isEqualGuid(pMajorGuid, MFMediaType_HTML) then
    Result := 'MFMediaType_HTML'
  else if isEqualGuid(pMajorGuid, MFMediaType_Binary) then
    Result := 'MFMediaType_Binary'
  else if isEqualGuid(pMajorGuid, MFMediaType_FileTransfer) then
    Result := 'MFMediaType_FileTransfer'
  else if isEqualGuid(pMajorGuid, MFMediaType_Stream) then
    Result := 'MFMediaType_Stream'
  else if isEqualGuid(pMajorGuid, MFMediaType_MultiplexedFrames) then
    Result := 'MFMediaType_MultiplexedFrames'
  else if isEqualGuid(pMajorGuid, MFMediaType_Subtitle) then
    Result := 'MFMediaType_Subtitle'
  else if isEqualGuid(pMajorGuid, MFMediaType_Perception) then
    Result := 'MFMediaType_Perception'
  else
    Result := 'MFMediaType_Unknown';
end;


//
function GetAudioFormat(var pMfAudioFormat: TMFAudioFormat): HResult;
var
  hr: HResult;
  gMajorType: TGUID;
  gSubType: TGUID;
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  dwTypes: DWORD;
  unFormatTag: UINT32;
  i, j: DWORD;
  bSelected: LongBool;


label done;

begin

  i := 0;

  repeat

  dwTypes := 0;

  if not Assigned(pMfAudioFormat.mfSource) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  hr := pMfAudioFormat.mfSource.CreatePresentationDescriptor(pPD);
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

  hr := pHandler.GetMediaTypeCount(dwTypes);
  if FAILED(hr) then
    goto done;

  // find the properties
  for j := 0 to dwTypes -1 do
    begin

      hr := pHandler.GetMediaTypeByIndex(j,
                                         pType);
      if FAILED(hr) then
        goto done;

      hr := pType.GetMajorType(gMajorType);
        if (FAILED(hr)) then
          goto done;

      if IsEqualGuid(gMajorType,
                     MFMediaType_Audio) then
        begin
          pMfAudioFormat.tgMajorFormat := gMajorType;
          pMfAudioFormat.wcMajorFormat := GetMajorTypeDescr(gMajorType);
          // Get the audio subtype. If not, skip.
          hr := pType.GetGUID(MF_MT_SUBTYPE,
                              pMfAudioFormat.tgSubFormat);
          if (FAILED(hr)) then
            goto done;

          // Readable subtype info.
          GetGUIDNameConst(pMfAudioFormat.tgMajorFormat,
                           pMfAudioFormat.tgSubFormat,
                           pMfAudioFormat.wcSubFormat,
                           pMfAudioFormat.wcFormatTag,
                           pMfAudioFormat.dwFormatTag,
                           pMfAudioFormat.wsDescr);


          // Get information from the audio format.
          // ===================================================================

          // Number of audio channels in an audio media type.
          // This attribute corresponds to the nChannels member of the WAVEFORMATEX structure.
          pMfAudioFormat.unChannels := MFGetAttributeUINT32(pType,
                                                            MF_MT_AUDIO_NUM_CHANNELS,
                                                            UINT32(2));

          // Number of audio samples per second in an audio media type.
          // This attribute corresponds to the nSamplesPerSec member of the WAVEFORMATEX structure.
          pMfAudioFormat.unSamplesPerSec := MFGetAttributeUINT32(pType,
                                                                 MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                                 UINT32(0));

          // Average number of bytes per second in an audio media type.
          // This attribute corresponds to the nAvgBytesPerSec member of the WAVEFORMATEX structure.
          pMfAudioFormat.unAvgBytesPerSec := MFGetAttributeUINT32(pType,
                                                                  MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                                                  UINT32(0));

          // Number of variable audio samples per second in an audio media type.
          pMfAudioFormat.dblFloatSamplePerSec := MFGetAttributeDouble(pType,
                                                                      MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND,
                                                                      0.0);

          // Number of audio samples contained in one compressed block of audio data.
          // This attribute can be used in compressed audio formats that have a fixed number of samples within each block.
          // This attribute corresponds to the wSamplesPerBlock member of the WAVEFORMATEXTENSIBLE structure.
          pMfAudioFormat.unSamplesPerBlock := MFGetAttributeUINT32(pType,
                                                                   MF_MT_AUDIO_SAMPLES_PER_BLOCK,
                                                                   UINT32(0));

          // Note: Some encoded audio formats do not contain a value for bits/sample.
          // In that case, use a default value of 16 or 32 incase of variable samples per second.
          // Most codecs will accept this value.
          if (pMfAudioFormat.dblFloatSamplePerSec = 0.0) then
            pMfAudioFormat.unBitsPerSample := MFGetAttributeUINT32(pType,
                                                                   MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                                   UINT32(16))
          else
            pMfAudioFormat.unBitsPerSample := MFGetAttributeUINT32(pType,
                                                                   MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                                   UINT32(32));

          // Number of valid bits of audio data in each audio sample.
          // Remarks:
          // The MF_MT_AUDIO_VALID_BITS_PER_SAMPLE attribute is used for audio formats that contain padding after each audio sample.
          // The total size of each audio sample, including padding bits, is given in the MF_MT_AUDIO_BITS_PER_SAMPLE attribute.
          //
          // If the MF_MT_AUDIO_VALID_BITS_PER_SAMPLE attribute is not set, use the MF_MT_AUDIO_BITS_PER_SAMPLE attribute to
          // find the number of valid bits per sample.
          //
          // If an audio format does not contain any padding bits, then this attribute should not be set,
          // or should be set to the same value as the MF_MT_AUDIO_BITS_PER_SAMPLE attribute.
          //
          // This attribute corresponds to the wValidBitsPerSample member of the WAVEFORMATEXTENSIBLE structure.
          pMfAudioFormat.unValidBitsPerSample := MFGetAttributeUINT32(pType,
                                                                      MF_MT_AUDIO_VALID_BITS_PER_SAMPLE,
                                                                      UINT32(0));

          // For PCM audio formats, the block alignment is equal to the number of
          // audio channels multiplied by the number of bytes per audio sample.
          // This attribute corresponds to the nBlockAlign member of the WAVEFORMATEX structure.
          pMfAudioFormat.unBlockAlignment := MFGetAttributeUINT32(pType,
                                                                  MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                                                  UINT32(0));

          // In an audio media type, specifies the assignment of audio channels to speaker positions.
          pMfAudioFormat.unChannelMask := MFGetAttributeUINT32(pType,
                                                               MF_MT_AUDIO_CHANNEL_MASK,
                                                               UINT32(0));
          // AAC specific
          if IsEqualGuid(pMfAudioFormat.tgSubFormat,
                         MFAudioFormat_AAC) then
            begin
              pMfAudioFormat.unAACPayload := MFGetAttributeUINT32(pType,
                                                                  MF_MT_AAC_PAYLOAD_TYPE,
                                                                  UINT32(0));

              pMfAudioFormat.unAACProfileLevel := MFGetAttributeUINT32(pType,
                                                                       MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                                                                       UINT32(0));
            end
          else
            begin
              pMfAudioFormat.unAACPayload := 0;
              pMfAudioFormat.unAACProfileLevel := 0;
            end;

          // FLAC extra data.
          if IsEqualGuid(pMfAudioFormat.tgSubFormat,
                         MFAudioFormat_FLAC) then
            pMfAudioFormat.unFlacMaxBlockSize := MFGetAttributeUINT32(pType,
                                                                      MF_MT_AUDIO_FLAC_MAX_BLOCK_SIZE,
                                                                      UINT32(0))
          else
            pMfAudioFormat.unFlacMaxBlockSize := 0;

          // Do a brief check.
          if (pMfAudioFormat.unChannels = 0) or (pMfAudioFormat.unSamplesPerSec = 0) then
            begin
              hr := MF_E_INVALIDTYPE;
              goto done;
            end;

        end; //if mediatype = audio
     end; //end if

      pPD := nil;
      pSD := nil;
      pHandler := nil;
      pType := nil;
      inc(i);

  until (i >= dwTypes);  // end repeat


done:
  Result := hr;
end;


//  V1
function GetAudioSubType(mSource: IMFMediaSource;
                         out pSubType: TGUID;
                         out pFormatTag: DWord;
                         out pDescr: WideString;
                         out pChannels: UINT32;
                         out psamplesPerSec: UINT32;
                         out pbitsPerSample: UINT32;
                         out pBlockAlignment: UINT32;
                         out pAverageSampleRate: UINT32;
                         out pBitRate: Double;
                         out pSampleRate: Double): HResult; overload;

var
  hr: HResult;
  majortype: TGUID;
  subtype: TGUID;
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  mfType: IMFMediaType;
  cTypes: DWORD;
  i, j: DWORD;
  bSelected: BOOL;
  sGuid: string;
  sDescr: Widestring;
  uAverageSampleRate: UINT32;

label done;

begin
  majortype := GUID_NULL;
  subtype := GUID_NULL;
  pSubType := subtype;
  pChannels := 0;
  pSamplesPerSec := 0;
  pBitsPerSample := 0;
  pBlockAlignment := 0;
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
                                         mfType);
      if FAILED(hr) then
        goto done;

      hr := mfType.GetMajorType(majortype);
        if (FAILED(hr)) then
          goto done;

      if IsEqualGuid(majortype,
                     MFMediaType_Audio) then
        begin

          // Get the audio subtype. If not, skip.
          hr := mfType.GetGUID(MF_MT_SUBTYPE,
                               subtype);
          if (FAILED(hr)) then
            goto done;

          // readable audiosubtype guid
          sGuid := GuidToString(subtype);

          // Get description by guid   (this is just a short list of most common audioformats)


          pSubType := subtype;

          sGuid := ' ( GUID: ' + sGuid + ' )';
          pDescr := sDescr + sGuid;


          // Get a brief information from the audio format.

          pChannels := MFGetAttributeUINT32(mfType,
                                            MF_MT_AUDIO_NUM_CHANNELS,
                                            2);

          pSamplesPerSec := MFGetAttributeUINT32(mfType,
                                                 MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                 44100);

          // Note: Some encoded audio formats do not contain a value for bits/sample.
          // In that case, use a default value of 16. Most codecs will accept this value.
          pBitsPerSample := MFGetAttributeUINT32(mfType,
                                                 MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                 16);

          // Bitrate (kbps)
          pBitRate := (pAverageSampleRate * 8) / 1000;


          uAverageSampleRate := Round((pChannels * pBitRate) / 8);

          pAverageSampleRate := MFGetAttributeUINT32(mfType,
                                                     MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                                     uAverageSampleRate);

          // Samplerate (khz)
          pSampleRate := pSamplesPerSec / 1000;

          if (pChannels = 0) or (pSamplesPerSec = 0) then
            begin
              hr := MF_E_INVALIDTYPE;
              goto done;
            end;

        end; //if mediatype = audio
     end; //end if

      pPD := nil;
      pSD := nil;
      pHandler := nil;
      mfType := nil;
      inc(i);

   until (i > cTypes);  // end repeat

done:
  Result := hr;
end;


//  V2
function GetAudioSubType(var pAudioFormat: TMFAudioFormat): HResult; overload;
begin
  Result := GetAudioFormat(pAudioFormat);
end;


//
function GetWinAudioEncoderFormats(const mfAudioFormat: TGuid;
                                   MftEnumFlag: MFT_ENUM_FLAG;
                                   out aAudioFmts: TMFAudioFormatArray): HResult;
var
  hr: HResult;
  i: Integer;
  dwMTCount: DWORD;
  UnkAudioType: IUnknown;
  mfAvailableTypes: IMFCollection;
  mfAudioType: IMFMediaType;

label
  done;

begin
  dwMTCount := 0;

  // Get the list of output formats supported by the Windows Media
  // audio encoder.
  hr := MFTranscodeGetAudioOutputAvailableTypes(mfAudioFormat,
                                                DWord(MftEnumFlag),
                                                nil,
                                                mfAvailableTypes);

  // Get the number of elements in the list.
  if SUCCEEDED(hr) then
    begin
      hr := mfAvailableTypes.GetElementCount(dwMTCount);

      if dwMTCount = 0 then
        begin
          hr := E_UNEXPECTED;
          goto done;
        end;
    end;

  // Add the media types in the collection.
  if SUCCEEDED(hr) then
    begin
      SetLength(aAudioFmts,
                dwMTCount);

      for i := 0 to dwMTCount -1 do
        begin
          hr := mfAvailableTypes.GetElement(i,
                                            UnkAudioType);
          if SUCCEEDED(hr) then
            hr := UnkAudioType.QueryInterface(IID_IMFMediaType,
                                              mfAudioType);

          // Get the formats
          if SUCCEEDED(hr) then
            hr := mfAudioType.GetMajorType(aAudioFmts[i].tgMajorFormat);

          // Get a description of the major format.
          aAudioFmts[i].wcMajorFormat := GetMajorTypeDescr(aAudioFmts[i].tgMajorFormat);

          if (SUCCEEDED(hr)) then
            hr := mfAudioType.GetGUID(MF_MT_SUBTYPE,
                                      aAudioFmts[i].tgSubFormat);

          // Readable subtype info
          GetGUIDNameConst(aAudioFmts[i].tgMajorFormat,
                           aAudioFmts[i].tgSubFormat,
                           aAudioFmts[i].wcSubFormat,
                           aAudioFmts[i].wcFormatTag,
                           aAudioFmts[i].dwFormatTag,
                           aAudioFmts[i].wsDescr);

          // Get information from the audio format.

          aAudioFmts[i].unChannels := MFGetAttributeUINT32(mfAudioType,
                                                           MF_MT_AUDIO_NUM_CHANNELS,
                                                           0);

          aAudioFmts[i].unSamplesPerSec := MFGetAttributeUINT32(mfAudioType,
                                                                MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                                0);

          // Number of audio samples per second in an audio media type.
          aAudioFmts[i].dblFloatSamplePerSec := MFGetAttributeDouble(mfAudioType,
                                                                     MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND,
                                                                     0.0);

          // Note: Some encoded audio formats do not contain a value for bits/sample.
          // In that case, use a default value of 16. Most codecs will accept this value.
          aAudioFmts[i].unBitsPerSample := MFGetAttributeUINT32(mfAudioType,
                                                                MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                                16);


          // Number of audio samples contained in one compressed block of audio data.
          aAudioFmts[i].unSamplesPerBlock := MFGetAttributeUINT32(mfAudioType,
                                                                  MF_MT_AUDIO_SAMPLES_PER_BLOCK,
                                                                  0);

          aAudioFmts[i].unValidBitsPerSample := MFGetAttributeUINT32(mfAudioType,
                                                                     MF_MT_AUDIO_VALID_BITS_PER_SAMPLE,
                                                                     0);
          // Block alignment, in bytes, for an audio media type.
          // The block alignment is the minimum atomic unit of data for the audio format.
          aAudioFmts[i].unBlockAlignment := MFGetAttributeUINT32(mfAudioType,
                                                                 MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                                                 0);

          // Average number of bytes per second in an audio media type.
          aAudioFmts[i].unAvgBytesPerSec := MFGetAttributeUINT32(mfAudioType,
                                                                 MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                                                 0);

          // ChannelMask is the same as speaker-layout.
          aAudioFmts[i].unChannelMask := MFGetAttributeUINT32(mfAudioType,
                                                              MF_MT_AUDIO_CHANNEL_MASK,
                                                              0);

          // AAC specific
          // Starting in Windows 8, the payload value can be 0 (raw AAC) or 1 (ADTS AAC).
          aAudioFmts[i].unAACPayload := MFGetAttributeUINT32(mfAudioType,
                                                             MF_MT_AAC_PAYLOAD_TYPE,
                                                             0);
          // unAACPayloadDescription
          case aAudioFmts[i].unAACPayload of
            0: aAudioFmts[i].wsAACPayloadDescription := 'Contains raw_data_block elements only (Raw AAC).';
            1: aAudioFmts[i].wsAACPayloadDescription := 'Audio Data Transport Stream (ADTS).';
            // The following payloads are not supported by Windows!
            2: aAudioFmts[i].wsAACPayloadDescription := 'Audio Data Interchange Format (ADIF). (Not supported)';
            3: aAudioFmts[i].wsAACPayloadDescription := 'MPEG-4 audio transport stream with a synchronization layer (LOAS) and a multiplex layer (LATM) (Not supported)';
          end;


          aAudioFmts[i].unAACProfileLevel := MFGetAttributeUINT32(mfAudioType,
                                                                  MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                                                                  0);

          // Add a readable profile description.
          case aAudioFmts[i].unAACProfileLevel of
            //$28
            40: aAudioFmts[i].wsAACProfileLevelDescription := 'AAC Profile L2 (AAC-Low Complexity. Most used.)';
            //$29
            41: aAudioFmts[i].wsAACProfileLevelDescription := 'AAC Profile L2 (AAC-Low Complexity. Most used.)';
            //$2A
            42: aAudioFmts[i].wsAACProfileLevelDescription := 'AAC Profile L4 (AAC-Low Complexity)';
            //$2B
            43: aAudioFmts[i].wsAACProfileLevelDescription := 'AAC Profile L5 (AAC-Low Complexity)';

            //$2C
            44: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v1 AAC Profile L2 (For low bitrates)';
            //$2E
            46: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v1 AAC Profile L4 (For low bitrates)';
            //$2F
            47: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v1 AAC Profile L5 (For low bitrates)';

            //$30
            48: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v2 AAC Profile L2 (For very low bitrates)';
            //$31
            49: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v2 AAC Profile L3 (For very low bitrates)';
            //$32
            50: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v2 AAC Profile L4 (For very low bitrates)';
            //$33
            51: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v2 AAC Profile L5 (For very low bitrates)';
            // Not documented by MS
            //$50
            80: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v2 AAC Profile. 8 channels (7.1)';
            //$52
            82: aAudioFmts[i].wsAACProfileLevelDescription := 'High Efficiency v2 AAC Profile. 8 channels (7.1)';
            else
               aAudioFmts[i].wsAACProfileLevelDescription := Format('Unknown AAC Profile Level %d.',
                                                                    [aAudioFmts[i].unAACProfileLevel]);
          end;

          // FLAC specific
          aAudioFmts[i].unFlacMaxBlockSize := MFGetAttributeUINT32(mfAudioType,
                                                                   MF_MT_AUDIO_FLAC_MAX_BLOCK_SIZE,
                                                                   (0));

          if FAILED(hr) then
            Break;
        end;
    end;

done:
  Result := hr;
end;


function AttributeGetString(pAttributes: IMFAttributes;
                            const guid: TGuid;
                            out pwcStr: PWideChar): HResult; overload;
var
  hr: HResult;
  cchLength: UINT32;
  pString: PWideChar;
  wcSize: UINT32;

begin

  hr := pAttributes.GetStringLength(guid,
                                    cchLength);

  if SUCCEEDED(hr) then
    begin
      wcSize := (cchLength + 1) * SizeOf(WideChar);
      GetMem(pString, wcSize);
      if (pString = nil) then
        hr := E_OUTOFMEMORY;
    end;

  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.GetString(guid,
                                  pString,
                                  wcSize,
                                  cchLength);
    end;

  // Free the allocated memory when it's no longer needed.
  if Assigned(pString) then
    FreeMem(pString);

  Result := hr;
end;


function AttributeGetString(pActivate: IMFActivate;
                            const guid: TGuid;
                            out pwcStr: PWideChar): HResult; overload;
var
  hr: HResult;
  cchLength: UINT32;
  pString: PWideChar;
  wcSize: UINT32;

begin

  hr := pActivate.GetStringLength(guid,
                                  cchLength);

  if SUCCEEDED(hr) then
    begin
      wcSize := (cchLength + 1) * SizeOf(WideChar);
      GetMem(pString, wcSize);
      if (pString = nil) then
        hr := E_OUTOFMEMORY;
    end;

  if SUCCEEDED(hr) then
    begin
      hr := pActivate.GetString(guid,
                                pString,
                                wcSize,
                                cchLength);
    end;

  // Free the allocated memory when it's no longer needed.
  if Assigned(pString) then
    FreeMem(pString);

  Result := hr;
end;


// Ducking
// =======
function SetDuckingForSystemSounds(): HResult;
var
  hr: HResult;
  bCoUnInit: Boolean;
  pDevice: IMMDevice;
  pEnumerator: IMMDeviceEnumerator;
  pSessionControl: IAudioSessionControl;
  pSessionControl2: IAudioSessionControl2;
  pSessionManager: IAudioSessionManager;

label
  done;

begin

  bCoUnInit := False;

  // Do a CoInitialize check
  // This is just a check, because normally CoInitialize should be initialized
  // before starting MF.

  hr := CoInitialize(nil);
  if (hr and $80000000 = 0) then
    CoUnInitialize()
  else
    bCoUnInit := True;


  // Create the device enumerator.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         IUnknown(pEnumerator));

  // Get the default audio device.
  if Succeeded(hr) then
    hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                              eConsole,
                                              pDevice);

  // Get the audio client.
  if Succeeded(hr) then
    hr := pDevice.Activate(IID_IAudioSessionManager,
                           CLSCTX_ALL,
                           nil,
                           Pointer(pSessionManager));

  // Get a reference to the session manager.
  if Succeeded(hr) then
    hr := pSessionManager.GetAudioSessionControl(nil,
                                                 0,
                                                 pSessionControl);

  // Get the extended session control interface pointer.
  if Succeeded(hr) then
    hr := pSessionControl.QueryInterface(IAudioSessionControl2,
                                         pSessionControl2);

  // Check whether this is a system sound.
  if Succeeded(hr) then
    hr := pSessionControl2.IsSystemSoundsSession();

  // If it is a system sound, opt out of the default
  // stream attenuation experience.
  if Succeeded(hr) then
    hr := pSessionControl2.SetDuckingPreference(True);

done:
  // Clean up. In Delphi all interfaces are reference counted, so
  // Delphi compiler will take care of releasing the interfaces.

  if bCoUnInit then
    CoUnInitialize();

  Result := hr;
end;


// Sami/smi
//=================

// This function Sets the current SAMI style, specified by index and
// calls the following methods on the SAMI media source:
//   IMFSAMIStyle.GetStyleCount gets the number of styles.
//   IMFSAMIStyle.GetStyles gets a list of the style names, stored in a PROPVARIANT (PROPVARIANT).
//   IMFSAMIStyle.SetSelectedStyle sets a style by name.
// The list of style names is also stored on the presentation descriptor, in the MF_PD_SAMI_STYLELIST attribute.
//
function SetSAMIStyleByIndex(pSource: IMFMediaSource;
                             index: DWORD): HResult;
var
  hr: HResult;
  pSami: IMFSAMIStyle;
  cStyles: DWORD;
  varStyles: PPROPVARIANT;
  i: Integer;

label
  done;

begin
  // Get the SAMIstyle interface
  hr := MFGetService(pSource,
                     MF_SAMI_SERVICE,
                     IID_IMFSAMIStyle,
                     Pointer(pSami));

  if FAILED(hr) then
    goto done;

  hr := pSami.GetStyleCount(cStyles);

  // When using a dynamic array
  //SetLength(varStyles, cStyles);

{$POINTERMATH ON}
  for i := 0 to cStyles -1 do
    PropVariantInit(varStyles[i]);

  if FAILED(hr) then
    goto done;

  if (index >= cStyles) then
    begin
      hr := E_INVALIDARG;
      goto done;
    end;

  hr := pSami.GetStyles(varStyles);

  if FAILED(hr) then
    goto done;

  hr := pSami.SetSelectedStyle(varStyles[index].calpwstr.pElems);

done:
  for i := 0 to cStyles-1 do
    PropVariantClear(varStyles[i]);
{$POINTERMATH OFF}
  Result := hr;
end;



// Getting the File Duration
// To get the duration of a media file, call the IMFSourceReader.GetPresentationAttribute method and
// request the MF_PD_DURATION attribute, as shown in the following code.
function GetFileDuration(pSource: IMFSourceReader;
                         out phnsDuration: LONGLONG): HResult; overload;
var
  hr: HResult;
  pvVar: PROPVARIANT;

begin
  PropVariantInit(pvVar);

  // Get file duration
  // Gets the duration in 100-nanosecond units.
  // Divide by 10,000,000 to get the duration in seconds.
  hr := pSource.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                         MF_PD_DURATION,
                                         pvVar);
  if SUCCEEDED(hr) then
    hr := PropVariantToInt64(pvVar,
                             phnsDuration);

  PropVariantClear(pvVar);
  Result := hr;
end;


function GetFileDuration(pSourceReader: IMFSourceReader;
                         out mftDuration: MFTIME): HResult; overload;
var
  hr: HResult;
  pvVar: PROPVARIANT;

begin
  PropVariantInit(pvVar);

  // Get file duration
  // Gets the duration in 100-nanosecond units.
  // Divide by 10,000,000 to get the duration in seconds.
  hr := pSourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                               MF_PD_DURATION,
                                               pvVar);
  if SUCCEEDED(hr) then
    hr := PropVariantToUInt64(pvVar,
                              mftDuration);

  PropVariantClear(pvVar);
  Result := hr;
end;



// Alternatively you might get the duration of a media file by calling the IMFMediaSource.CreatePresentationDescriptor method and
// request the MF_PD_DURATION attribute, as shown in the following code.
function GetFileDuration(pSource: IMFMediaSource;
                         out pDuration: LONGLONG): HResult; overload;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;

begin
  pDuration := 0;

  hr := pSource.CreatePresentationDescriptor(pPD);
  if SUCCEEDED(hr) then
    hr := pPD.GetUINT64(MF_PD_DURATION,
                        UINT64(pDuration));

  Result := hr;
end;


// Get fileduration from an URL.
function GetFileDuration(const sURL: PCWSTR;
                         out pDuration: LONGLONG): HResult; overload;
var
  hr: HResult;
  pSource: IMFMediaSource;
  pPD: IMFPresentationDescriptor;

begin
  pDuration := 0;
  // Create a mediasource by given URL.
  hr := CreateObjectFromUrl(sURL,
                            pSource);

  // Get the duration of a media file by calling the IMFMediaSource.CreatePresentationDescriptor method
  if SUCCEEDED(hr) then
    hr := pSource.CreatePresentationDescriptor(pPD);

  if SUCCEEDED(hr) then
    hr := pPD.GetUINT64(MF_PD_DURATION,
                        UINT64(pDuration));

  Result := hr;
end;


// Get fileduration in MFTIME units from an URL.
function GetFileDuration(const sURL: PCWSTR;
                         out pDuration: MFTIME): HResult; overload;
var
  hr: HResult;
  pSource: IMFMediaSource;
  pPD: IMFPresentationDescriptor;

begin
  pDuration := 0;
  // Create a mediasource by given URL.
  hr := CreateObjectFromUrl(sURL,
                            pSource);

  // Get the duration of a media file by calling the IMFMediaSource.CreatePresentationDescriptor method
  if SUCCEEDED(hr) then
    hr := pSource.CreatePresentationDescriptor(pPD);

  if SUCCEEDED(hr) then
    hr := pPD.GetUINT64(MF_PD_DURATION,
                        pDuration);

  Result := hr;
end;



// Gets de file size
function GetFileSize(pReader: IMFSourceReader;
                     out phnsFileSize: ULONGLONG): HResult; overload;
var
  hr: HResult;
  pvVar: PROPVARIANT;

begin
  PropVariantInit(pvVar);
  hr := pReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                         MF_PD_TOTAL_FILE_SIZE,
                                         pvVar);
  if SUCCEEDED(hr) then
    hr := PropVariantToUInt64(pvVar,
                              phnsFileSize);

  PropVariantClear(pvVar);
  Result := hr;
end;

// Alternatively you might get the filesize of a media file by calling the IMFMediaSource.CreatePresentationDescriptor method and
// request the MF_PD_TOTAL_FILE_SIZE attribute.
function GetFileSize(pReader: IMFMediaSource;
                     out phnsFileSize: ULONGLONG): HResult; overload;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;

begin

  hr := pReader.CreatePresentationDescriptor(pPD);
  if SUCCEEDED(hr) then
    hr := pPD.GetUINT64(MF_PD_TOTAL_FILE_SIZE,
                        phnsFileSize);

  Result := hr;
end;



// TStreamContents record
procedure TStreamContents.Reset();
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

  audio_lpLangShortName := nil;
  audio_lpLangFullName := nil;
  audio_wsAudioDescr := '';
  audio_iAudioChannels := 0;
  audio_iSamplesPerSec := 0;
  audio_iBitsPerSample := 0;
  audio_dwFormatTag := 0;
  audio_ChannelMask := 0;
  // AAC specific.
  audio_ProfileAndLevel := 0;
  audio_PayloadType := 0;
  // FLAC specific.
  audio_FLAC_ := 0;
end;


// Creates a transcode profile for the given params mfAudioFormat and mfTranscodeContainerType.
function CreateTranscodeProfile(const mfAudioFormat: TGUID;  // For example: MFAudioFormat_WMAudioV9
                                const mfTranscodeContainerType: TGUID; // For example: MFTranscodeContainerType_ASF
                                out ppProfile: IMFTranscodeProfile): HResult;
var
  hr: HResult;
  pProfile: IMFTranscodeProfile;   // Transcode profile.
  pAvailableTypes: IMFCollection;  // List of audio media types.
  pAudioType: IMFMediaType;        // Audio media type.
  pAudioAttrs: IMFAttributes;      // Copy of the audio media type.
  pContainer: IMFAttributes;       // Container attributes.
  dwMTCount: DWORD;
  dwFlags: DWORD;

label
  done;

begin

  // Create an empty transcode profile.
  hr := MFCreateTranscodeProfile(pProfile);
  if FAILED(hr) then
    goto done;

  // Get output media types for the Windows Media audio encoder.

  // Enumerate all codecs except for codecs with field-of-use restrictions.
  // Sort the results.
  dwFlags := (MFT_ENUM_FLAG_ALL and not MFT_ENUM_FLAG_FIELDOFUSE) or
             MFT_ENUM_FLAG_SORTANDFILTER;

  hr := MFTranscodeGetAudioOutputAvailableTypes(mfAudioFormat,
                                                dwFlags,
                                                nil,
                                                pAvailableTypes);
  if FAILED(hr) then
    goto done;

  hr := pAvailableTypes.GetElementCount(dwMTCount);
  if FAILED(hr) then
    goto done;

  if (dwMTCount = 0) then
    begin
      hr := E_FAIL;
      goto done;
    end;

  // Get the first audio type in the collection and make a copy.
  hr := GetCollectionObject(pAvailableTypes,
                            0,
                            pAudioType);
  if FAILED(hr) then
    goto done;

  hr := MFCreateAttributes(pAudioAttrs,
                           0);
  if FAILED(hr) then
    goto done;

  hr := pAudioType.CopyAllItems(pAudioAttrs);
  if FAILED(hr) then
    goto done;

  // Set the audio attributes on the profile.
  hr := pProfile.SetAudioAttributes(pAudioAttrs);
  if FAILED(hr) then
    goto done;

  // Set the container attributes.
  hr := MFCreateAttributes(pContainer,
                           1);
  if FAILED(hr) then
    goto done;

  hr := pContainer.SetGUID(MF_TRANSCODE_CONTAINERTYPE,
                           mfTranscodeContainerType);
  if FAILED(hr) then
    goto done;

  hr := pProfile.SetContainerAttributes(pContainer);
  if FAILED(hr) then
    goto done;

  ppProfile := pProfile;

done:
  Result := hr;
end;


// Configures the recordsink for encoding video using default media type.
//=======================================================================
function ConfigureVideoEncoding(pSource: IMFCaptureSource;
                                pRecord: IMFCaptureRecordSink;
                                const guidEncodingType: REFGUID): HResult;
var
  pMediaType: IMFMediaType;
  pMediaType2: IMFMediaType;
  guidSubType: TGUID;
  uiEncodingBitrate: UINT32;
  dwSinkStreamIndex: DWORD;
  hr: HResult;

begin
  guidSubType := GUID_NULL;

  // Configure the video format for the recording sink.
  hr := pSource.GetCurrentDeviceMediaType(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD,
                                          pMediaType);
  if FAILED(hr) then
    Exit(hr);

  hr := CloneVideoMediaType(pMediaType,
                            guidEncodingType,
                            pMediaType2);
  if FAILED(hr) then
    Exit(hr);


  hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                           guidSubType);
  if FAILED(hr) then
    Exit(hr);

  if IsEqualGUID(guidSubType,
                 MFVideoFormat_H264_ES) or
     IsEqualGUID(guidSubType,
                 MFVideoFormat_H264) then
    begin
      // When the webcam supports H264_ES or H264, we just bypass the stream.
      // The output from the capture engine will be the same as the native type supported by the webcam.
      hr := pMediaType2.SetGUID(MF_MT_SUBTYPE,
                                MFVideoFormat_H264);
    end
  else
    begin
      hr := GetEncodingBitrate(pMediaType2,
                               uiEncodingBitrate);
      if FAILED(hr) then
        Exit(hr);

      hr := pMediaType2.SetUINT32(MF_MT_AVG_BITRATE,
                                  uiEncodingBitrate);
    end;

  if FAILED(hr) then
    Exit(hr);

  // Connect the video stream to the recording sink.
  Result := pRecord.AddStream(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD,
                              pMediaType2,
                              nil,
                              dwSinkStreamIndex);

end;


// Configuration for Encoding video using a given media type.
//===========================================================
function ConfigureVideoEncoding(pSource: IMFCaptureSource;
                                pRecord: IMFCaptureRecordSink;
                                const guidEncodingType: REFGUID;
                                pMediaType: IMFMediaType): HResult;
var
  pMediaType2: IMFMediaType;
  guidSubType: TGUID;
  uiEncodingBitrate: UINT32;
  dwSinkStreamIndex: DWORD;
  hr: HResult;

begin
  guidSubType := GUID_NULL;

  hr := CloneVideoMediaType(pMediaType,
                            guidEncodingType,
                            pMediaType2);
  if FAILED(hr) then
    Exit(hr);


  hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                           guidSubType);
  if FAILED(hr) then
    Exit(hr);

  if IsEqualGUID(guidSubType,
                 MFVideoFormat_H264_ES) or
     IsEqualGUID(guidSubType,
                 MFVideoFormat_H264) then
    begin
      // When the webcam supports H264_ES or H264, we just bypass the stream.
      // The output from the capture engine will be the same as the native type supported by the webcam.
      hr := pMediaType2.SetGUID(MF_MT_SUBTYPE,
                                MFVideoFormat_H264);
    end
  else
    begin
      hr := GetEncodingBitrate(pMediaType2,
                               uiEncodingBitrate);
      if FAILED(hr) then
        Exit(hr);

      hr := pMediaType2.SetUINT32(MF_MT_AVG_BITRATE,
                                  uiEncodingBitrate);
    end;

  if FAILED(hr) then
    Exit(hr);

  // Connect the video stream to the recording sink.
  Result := pRecord.AddStream(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD,
                              pMediaType2,
                              nil,
                              dwSinkStreamIndex);
end;


//
function ConfigureAudioEncoding(pSource: IMFCaptureSource;
                                pRecord: IMFCaptureRecordSink;
                                const guidEncodingType: REFGUID): HResult;
var
  pAvailableTypes: IMFCollection;
  pMediaType: IMFMediaType;
  pAttributes: IMFAttributes;
  dwSinkStreamIndex: DWORD;
  hr: HResult;

label
  done;

begin

  if (guidEncodingType = GUID_NULL) then
    begin
      hr := ERROR_INVALID_PARAMETER;
      goto done;
    end;

  // Configure the audio format for the recording sink.

  hr := MFCreateAttributes(pAttributes,
                           1);
  if FAILED(hr) then
    goto done;

  // Enumerate low latency media types
  hr := pAttributes.SetUINT32(MF_LOW_LATENCY,
                              UINT32(True));
  if FAILED(hr) then
    goto done;

  // Get a list of encoded output formats that are supported by the encoder.
  hr := MFTranscodeGetAudioOutputAvailableTypes(guidEncodingType,
                                                MFT_ENUM_FLAG_ALL or MFT_ENUM_FLAG_SORTANDFILTER,
                                                pAttributes,
                                                pAvailableTypes);
  if FAILED(hr) then
    goto done;

  // Pick the first format from the list.
  hr := GetCollectionObject(pAvailableTypes,
                            0,
                            pMediaType);
  if FAILED(hr) then
    goto done;

  // Connect the audio stream to the recording sink.
  hr := pRecord.AddStream(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_AUDIO,
                          pMediaType,
                          nil,
                          dwSinkStreamIndex);

  if (hr = MF_E_INVALIDSTREAMNUMBER) then
    begin
      // If an audio device is not present, allow video only recording
      hr := S_OK;
    end;

done:
  Result := hr;
end;


//
function SetDeviceFormat(pSource: IMFMediaSource;
                         dwFormatIndex: DWORD): HResult;
var
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  hr: HResult;
  fSelected: Bool;
  dwCount: DWord;

label
  done;

begin

  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(0,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  pHandler.GetMediaTypeCount(dwCount);

  if (dwFormatIndex < dwCount) then
    hr := pHandler.GetMediaTypeByIndex(dwFormatIndex,
                                       pType)
  else  // get the default
    hr := pHandler.GetMediaTypeByIndex(0,
                                       pType);

  if FAILED(hr) then
    goto done;

  hr := pHandler.SetCurrentMediaType(pType);

done:
  Result := hr;
end;


//
function SetDeviceFormat(pSource: IMFMediaSource;
                         pMediaType: IMFMediaType;
                         dwFormatIndex: DWORD): HResult;
var
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  hr: HResult;
  fSelected: Bool;
  dwCount: DWord;

label
  done;

begin

  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(0,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  pHandler.GetMediaTypeCount(dwCount);

  if (dwFormatIndex < dwCount) then
     hr := pHandler.SetCurrentMediaType(pMediaType)
  else  // get the default
    begin
      hr := pHandler.GetMediaTypeByIndex(0,
                                         pType);
      if FAILED(hr) then
       goto done;

    end;

done:
  Result := hr;
end;


//
function ListEncoders(const subtype: TGUID;
                      bAudio: Boolean;
                      var aGuidArray: TClsidArray): Hresult;
var
  hr: HResult;
  i: Integer;
  iCount: UINT32;
  ppCLSIDs: PCLSID;
  mftInfo: MFT_REGISTER_TYPE_INFO;
  mftCategory: TGuid;

begin

  iCount := 0;
  ppCLSIDs := nil;

  if bAudio then
    begin
      mftInfo.guidMajorType := MFMediaType_Audio;
      mftCategory := MFT_CATEGORY_AUDIO_ENCODER;
    end
  else
    begin
      mftInfo.guidMajorType := MFMediaType_Video;
      mftCategory := MFT_CATEGORY_VIDEO_ENCODER;
    end;

  hr := MFTEnum(mftCategory,
                0,          // Reserved
                nil,        // Input type
                @mftInfo,   // Output type
                nil,        // Reserved
                ppCLSIDs,
                iCount);

  if SUCCEEDED(hr) and (iCount = 0) then
    hr := MF_E_TOPO_CODEC_NOT_FOUND;

  // Copy the the mft guids to array.
  SetLength(aGuidArray, iCount);
  {$POINTERMATH ON}
  if SUCCEEDED(hr) then
    begin
      for i := 0 to iCount -1 do
        aGuidArray[i] := ppCLSIDs[i];
    end;

  // Note:
  // You can instantiate an encoder by calling the COM function CoCreateInstance and
  // passing the CLSID of the encoder you want to use.
  //
  // Like this:
  // CoCreateInstance(aGuidArray[index],
  //                  nil,
  //                  CLSCTX_INPROC_SERVER,
  //                  IID_IMFTransform,
  //                  pEncoder);  {pEncoder: IMFTransform;}
  //
  // or just call CreateEncoder(guid, pEncoder);
  CoTaskMemFree(ppCLSIDs);
  Result := hr;
end;


//
function CreateEncoderFromClsid(mftCategory: CLSID;
                                out pEncoder: IMFTransform): HResult;
begin
  Result := CoCreateInstance(mftCategory,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IID_IMFTransform,
                             pEncoder);
end;


{procedure CopyWaveFormatEx(const SourceFmt: WAVEFORMATEX;
                           out DestFmt: PWAVEFORMATEX); //inline;
var
  dFmt: PWAVEFORMATEX;

begin
  // Allocate memory for DestFormat
  GetMem(dFmt,
         SizeOf(WAVEFORMATEX));

  // Copy SourceFormat to DestFormat
  Move(SourceFmt,
       dFmt^,
       SizeOf(WAVEFORMATEX));

  // User is responsible to free the memory occupied by parameter DestFmt.
  DestFmt := dFmt;
  FreeMem(dFmt);
end;}


procedure CopyWaveFormatEx(const SourceFmt: WAVEFORMATEX;
                           out DestFmt: PWAVEFORMATEX); //inline;
//var
  //dFmt: PWAVEFORMATEX;

begin
  // Allocate memory for DestFormat
  GetMem(DestFmt,
         SizeOf(WAVEFORMATEX));

  // Copy SourceFormat to DestFormat
  Move(SourceFmt,
       DestFmt^,
       SizeOf(WAVEFORMATEX));

  // User is responsible to free the memory occupied by parameter DestFmt.
  //DestFmt := dFmt;
  //FreeMem(dFmt);
end;


function GetDefaultWaveFmtEx(): WAVEFORMATEX;
var
  wavFmtEx: WAVEFORMATEX;

begin
  wavFmtEx.wFormatTag      := WAVE_FORMAT_PCM;
  wavFmtEx.nChannels       := 2;
  wavFmtEx.nSamplesPerSec  := 44100;
  wavFmtEx.wBitsPerSample  := 16;
  wavFmtEx.nBlockAlign     := (wavFmtEx.nChannels * wavFmtEx.wBitsPerSample) div BITS_PER_BYTE;
  wavFmtEx.nAvgBytesPerSec := wavFmtEx.nBlockAlign * wavFmtEx.nSamplesPerSec;
  wavFmtEx.cbSize          := 0;
  Result := wavFmtEx;
end;


procedure GetSpeakersLayOut(const ChannelMatrix: UINT32;
                            out aLayout: string;
                            out aChannels: string);
begin

  // Note: When ChannelMatrix is zero, assume stereo.

  case ChannelMatrix of

    SPEAKER_MONO:      begin
                         aLayout := 'Front Center.';
                         aChannels := 'Mono';
                       end;
    0, SPEAKER_STEREO: begin
                         aLayout := 'Front Left & Front Right.';
                         aChannels := 'Stereo';
                       end;
    SPEAKER_2POINT1:   begin
                         aLayout := 'Front Left & Front Right & Low Frequentie.';
                         aChannels := '2.1';
                       end;
    SPEAKER_SURROUND:  begin
                         aLayout := 'Front Left & Front Right & Front Center & Back Center.';
                         aChannels := 'Surround';
                       end;
    SPEAKER_QUAD:      begin
                         aLayout := 'Front Left & Front Right & Back Left & Back Right.';
                         aChannels := 'Quad';
                       end;
    SPEAKER_4POINT1:   begin
                         aLayout := 'Front Left & Front Right & Low Frequentie & Back Left & Back Right.';
                         aChannels := '4.1';
                       end;

    SPEAKER_5POINT1:   begin
                         aLayout := 'Front Left & Front Right & Front Center & Low Frequentie & Back Left & Back Right.';
                         aChannels := '5.1';
                       end;

    SPEAKER_7POINT1:   begin
                         aLayout := 'Front Left & Front Right & Front Center & Low Frequentie & Back Left & Back Right & Front Left of Center & Front Right of Center';
                         aChannels := '7.1';
                       end;

    SPEAKER_5POINT1_SURROUND:   begin
                                  aLayout := 'Front Left & Front Right & Front Center & Low Frequentie & Side Left & Side Right.';
                                  aChannels := '5.1 Surround';
                                end;

    SPEAKER_7POINT1_SURROUND:   begin
                                  aLayout := 'Front Left & Front Right & Front Center & Low Frequentie & Back Left & Back Right & Side Left & Side Right.';
                                  aChannels := '7.1 Surround';
                                end;
    else // Unknown
      begin
        aLayout := 'Unknown';
        aChannels := 'Unknown';
      end;
  end;
end;


function CreateUncompressedVideoType(const fccFormat: DWORD;
                                     aWidth: UINT32;
                                     aHeight: UINT32;
                                     interlaceMode: MFVideoInterlaceMode;
                                     frameRate: MFRatio;
                                     par: MFRatio;
                                     out ppType: IMFMediaType): HResult;
var
  hr: HResult;
  subtype: TGUID;
  lStride: LONG;
  cbImage: UINT;
  pType: IMFMediaType;

label
  done;

begin
  lStride := 0;
  subtype := MFVideoFormat_Base;

  // Set the subtype GUID from the FOURCC or D3DFORMAT value.
  subtype.D1 := fccFormat;

  hr := MFCreateMediaType(pType);
  if FAILED(hr) then
    goto done;

  hr := pType.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Video);
  if FAILED(hr) then
    goto done;

  hr := pType.SetGUID(MF_MT_SUBTYPE,
                      subtype);
  if FAILED(hr) then
    goto done;

  hr := pType.SetUINT32(MF_MT_INTERLACE_MODE,
                        interlaceMode);
  if FAILED(hr) then
    goto done;

  hr := MFSetAttributeSize(pType,
                           MF_MT_FRAME_SIZE,
                           aWidth,
                           aHeight);
  if FAILED(hr) then
    goto done;

  // Calculate the default stride value.
  hr := pType.SetUINT32(MF_MT_DEFAULT_STRIDE,
                        UINT32(lStride));
  if FAILED(hr) then
    goto done;

  // Calculate the image size in bytes.
  hr := MFCalculateImageSize(subtype,
                             aWidth,
                             aHeight,
                             cbImage);
  if FAILED(hr) then
    goto done;

  hr := pType.SetUINT32(MF_MT_SAMPLE_SIZE,
                        cbImage);
  if FAILED(hr) then
    goto done;

  hr := pType.SetUINT32(MF_MT_FIXED_SIZE_SAMPLES,
                        UINT32(1) {True});
  if FAILED(hr) then
    goto done;

  hr := pType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                        UINT32(1) {True});
  if FAILED(hr) then
    goto done;

  // Frame rate
  hr := MFSetAttributeRatio(pType,
                            MF_MT_FRAME_RATE,
                            frameRate.Numerator,
                            frameRate.Denominator);
  if FAILED(hr) then
    goto done;

  // Pixel aspect ratio
  hr := MFSetAttributeRatio(pType,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            par.Numerator,
                            par.Denominator);
  if FAILED(hr) then
    goto done;

  // Return the pointer to the caller.
  ppType := pType;

done:
  Result := hr;

end;


function ConvertVideoTypeToUncompressedType(pType: IMFMediaType;     // Pointer to an encoded video type.
                                            const subtype: TGUID;    // Uncompressed subtype (eg, RGB-32, AYUV)
                                            out ppType: IMFMediaType // Receives a matching uncompressed video type.
                                            ): HResult;
var
  hr: HResult;
  pTypeUncomp: IMFMediaType;
  majortype: TGUID;
  par: MFRatio;

label
  done;

begin

  hr := pType.GetMajorType(majortype);
  if FAILED(hr) then
    goto done;

  if not IsEqualGUID(majortype,
                     MFMediaType_Video) then
    begin
      hr := MF_E_INVALIDMEDIATYPE;
      goto done;
    end;

  // Create a new media type and copy over all of the items.
  // This ensures that extended color information is retained.
  hr := MFCreateMediaType(pTypeUncomp);
  if FAILED(hr) then
    goto done;

  hr := pType.CopyAllItems(pTypeUncomp);
  if FAILED(hr) then
    goto done;

  // Set the subtype.
  hr := pTypeUncomp.SetGUID(MF_MT_SUBTYPE,
                            subtype);
  if FAILED(hr) then
    goto done;

  // Uncompressed means all samples are independent.
  hr := pTypeUncomp.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                              UINT32(1) {True});
  if FAILED(hr) then
    goto done;

  // Fix up PAR if not set on the original type.
  hr := MFGetAttributeRatio(pTypeUncomp,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            UINT32(par.Numerator),
                            UINT32(par.Denominator));
  if FAILED(hr) then
    goto done;

  // Default to square pixels.
  hr := MFSetAttributeRatio(pTypeUncomp,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            1,
                            1);
  if FAILED(hr) then
    goto done;

  ppType := pTypeUncomp;

done:
  Result := hr;
end;


//
procedure TMftEnumInfo.Reset();
begin
  // Clean up
  CoTaskMemFree(pwcStr);
  pwcStr := nil;
  CoTaskMemFree(@aInputTypes);
  CoTaskMemFree(@aInputTypes);
  SafeRelease(pAttributes);
end;

// External methods
//=================

{$WARN SYMBOL_PLATFORM OFF}
  function SetForegroundWindow; external User32Lib name 'SetForegroundWindow' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  // If the window was brought to the foreground, the return value is nonzero.
  // If the window was not brought to the foreground, the return value is zero.

  function LockSetForegroundWindow; external User32Lib name 'LockSetForegroundWindow' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.

unit Utils;


interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfCaptureEngine,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMetLib;


  // Helper: Returns the frame size from a video media type.
  function GetFrameSize(pType: IMFMediaType;
                        out pWidth: UINT32;
                        out pHeight: UINT32): HResult; inline;

  // Helper: Returns the frame rate from a video media type.
  function GetFrameRate(pType: IMFMediaType;
                        out pNumerator: UINT32;
                        out pDenominator: UINT32): HResult; inline;

  // Returns the encoding bitrate.
  function GetEncodingBitrate(pMediaType: IMFMediaType;
                              out uiEncodingBitrate: UINT32): HResult;

  // Configures the recordsink for video.
  function ConfigureVideoEncoding(pSource: IMFCaptureSource;
                                  pRecord: IMFCaptureRecordSink;
                                  guidEncodingType: REFGUID): HResult;

  // Configures the recordsink for audio (if audiostream is present).
  function ConfigureAudioEncoding(pSource: IMFCaptureSource;
                                pRecord: IMFCaptureRecordSink;
                                guidEncodingType: REFGUID): HResult;


implementation



function GetFrameSize(pType: IMFMediaType;
                      out pWidth: UINT32;
                      out pHeight: UINT32): HResult; inline;
begin
  Result := MFGetAttributeSize(pType,
                               MF_MT_FRAME_SIZE,
                               pWidth,
                               pHeight);
end;


function GetFrameRate(pType: IMFMediaType;
                      out pNumerator: UINT32;
                      out pDenominator: UINT32): HResult; inline;
begin
  Result := MFGetAttributeRatio(pType,
                                MF_MT_FRAME_RATE,
                                pNumerator,
                                pDenominator);
end;


function GetEncodingBitrate(pMediaType: IMFMediaType;
                            out uiEncodingBitrate: UINT32): HResult;
var
  uiWidth: UINT32;
  uiHeight: UINT32;
  uiBitrate: Single;
  uiFrameRateNum: UINT32;
  uiFrameRateDenom: UINT32;
  hr: HResult;

label
  Done;

begin

  hr := GetFrameSize(pMediaType,
                     uiWidth,
                     uiHeight);
  if FAILED(hr) then
    goto Done;

  hr := GetFrameRate(pMediaType,
                     uiFrameRateNum,
                     uiFrameRateDenom);
  if FAILED(hr) then
    goto Done;

  uiBitrate := (((uiWidth / 3.0) * uiHeight) * uiFrameRateNum) / uiFrameRateDenom;

  uiEncodingBitrate := Round(uiBitrate);

done:
  Result := hr;
end;


function ConfigureVideoEncoding(pSource: IMFCaptureSource;
                                pRecord: IMFCaptureRecordSink;
                                guidEncodingType: REFGUID): HResult;
var
    pMediaType: IMFMediaType;
    pMediaType2: IMFMediaType;
    guidSubType: TGUID;
    uiEncodingBitrate: UINT32;
    dwSinkStreamIndex: DWORD;
    hr: HResult;

label
  Done;

begin
  guidSubType := GUID_NULL;

  // Configure the video format for the recording sink.
  hr := pSource.GetCurrentDeviceMediaType(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD,
                                          pMediaType);
  if FAILED(hr) then
    goto Done;

  hr := CloneVideoMediaType(pMediaType,
                            guidEncodingType,
                            pMediaType2);
  if FAILED(hr) then
    goto Done;


  hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                           guidSubType);
  if FAILED(hr) then
    goto Done;

  if (guidSubType = MFVideoFormat_H264_ES) and (guidSubType = MFVideoFormat_H264) then
    begin
      // When the webcam supports H264_ES or H264, we just bypass the stream.
      // The output from Capture engine shall be the same as the native type supported by the webcam
      hr := pMediaType2.SetGUID(MF_MT_SUBTYPE,
                                MFVideoFormat_H264);
    end
  else
    begin
      hr := GetEncodingBitrate(pMediaType2,
                               uiEncodingBitrate);
      if FAILED(hr) then
        goto Done;

        hr := pMediaType2.SetUINT32(MF_MT_AVG_BITRATE,
                                    uiEncodingBitrate);
    end;

  if FAILED(hr) then
    goto Done;

  // Connect the video stream to the recording sink.
  hr := pRecord.AddStream(MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD,
                          pMediaType2,
                          nil,
                          dwSinkStreamIndex);

done:
  Result := hr;
end;


function ConfigureAudioEncoding(pSource: IMFCaptureSource;
                                pRecord: IMFCaptureRecordSink;
                                guidEncodingType: REFGUID): HResult;
var
  pAvailableTypes: IMFCollection;
  pMediaType: IMFMediaType;
  pAttributes: IMFAttributes;
  dwSinkStreamIndex: DWORD;
  hr: HResult;

label
  Done;

begin
  // Configure the audio format for the recording sink.

  hr := MFCreateAttributes(pAttributes,
                           1);
  if FAILED(hr) then
    goto Done;

  // Enumerate low latency media types
  hr := pAttributes.SetUINT32(MF_LOW_LATENCY,
                              UINT32(True));
  if FAILED(hr) then
    goto Done;

  // Get a list of encoded output formats that are supported by the encoder.
  hr := MFTranscodeGetAudioOutputAvailableTypes(guidEncodingType,
                                                MFT_ENUM_FLAG_ALL or MFT_ENUM_FLAG_SORTANDFILTER,
                                                pAttributes,
                                                pAvailableTypes);
  if FAILED(hr) then
    goto Done;

  // Pick the first format from the list.
  hr := GetCollectionObject(pAvailableTypes,
                            0,
                            pMediaType);
  if FAILED(hr) then
    goto Done;

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

end.

unit WinApi.MediaFoundationApi.MfTrace;

interface

uses
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;


  function EventName(met: MediaEventType): string;

implementation

// IMPORTANT: you should only use these functions for debugging purposes.

// Media Foundation event names
function EventName(met: MediaEventType): string;
begin
  case met of
    MeError: Result := 'MEExtendedType';
    MEExtendedType: Result := 'MEExtendedType';
    MESessionTopologySet: Result := 'MESessionTopologySet';
    MESessionTopologiesCleared: Result := 'MESessionTopologiesCleared';
    MESessionStarted: Result := 'MESessionStarted';
    MESessionPaused: Result := 'MESessionPaused';
    MESessionStopped: Result := 'MESessionStopped';
    MESessionClosed: Result := 'MESessionClosed';
    MESessionEnded: Result := 'MESessionEnded';
    MESessionRateChanged: Result := 'MESessionRateChanged';
    MESessionScrubSampleComplete: Result := 'MESessionScrubSampleComplete';
    MESessionCapabilitiesChanged: Result := 'MESessionCapabilitiesChanged';
    MESessionTopologyStatus: Result := 'MESessionTopologyStatus';
    MESessionNotifyPresentationTime: Result := 'MESessionNotifyPresentationTime';
    MENewPresentation: Result := 'MENewPresentation';
    MELicenseAcquisitionStart: Result := 'MELicenseAcquisitionStart';
    MELicenseAcquisitionCompleted: Result := 'MELicenseAcquisitionCompleted';
    MEIndividualizationStart: Result := 'MEIndividualizationStart';
    MEIndividualizationCompleted: Result := 'MEIndividualizationCompleted';
    MEEnablerProgress: Result := 'MEEnablerProgress';
    MEEnablerCompleted: Result := 'MEEnablerCompleted';
    MEPolicyError: Result := 'MEPolicyError';
    MEPolicyReport: Result := 'MEPolicyReport';
    MEBufferingStarted: Result := 'MEBufferingStarted';
    MEBufferingStopped: Result := 'MEBufferingStopped';
    MEConnectStart: Result := 'MEConnectStart';
    MEConnectEnd: Result := 'MEConnectEnd';
    MEReconnectStart: Result := 'MEReconnectStart';
    MEReconnectEnd: Result := 'MEReconnectEnd';
    MERendererEvent: Result := 'MERendererEvent';
    MESessionStreamSinkFormatChanged: Result := 'MESessionStreamSinkFormatChanged';
    MESourceStarted: Result := 'MESourceStarted';
    MEStreamStarted: Result := 'MEStreamStarted';
    MESourceSeeked: Result := 'MESourceSeeked';
    MEStreamSeeked: Result := 'MEStreamSeeked';
    MENewStream: Result := 'MENewStream';
    MEUpdatedStream: Result := 'MEUpdatedStream';
    MESourceStopped: Result := 'MESourceStopped';
    MEStreamStopped: Result := 'MEStreamStopped';
    MESourcePaused: Result := 'MESourcePaused';
    MEStreamPaused: Result := 'MEStreamPaused';
    MEEndOfPresentation: Result := 'MEEndOfPresentation';
    MEEndOfStream: Result := 'MEEndOfStream';
    MEMediaSample: Result := 'MEMediaSample';
    MEStreamTick: Result := 'MEStreamTick';
    MEStreamThinMode: Result := 'MEStreamThinMode';
    MEStreamFormatChanged: Result := 'MEStreamFormatChanged';
    MESourceRateChanged: Result := 'MESourceRateChanged';
    MEEndOfPresentationSegment: Result := 'MEEndOfPresentationSegment';
    MESourceCharacteristicsChanged: Result := 'MESourceCharacteristicsChanged';
    MESourceRateChangeRequested: Result := 'MESourceRateChangeRequested';
    MESourceMetadataChanged: Result := 'MESourceMetadataChanged';
    MESequencerSourceTopologyUpdated: Result := 'MESequencerSourceTopologyUpdated';
    MEStreamSinkStarted: Result := 'MEStreamSinkStarted';
    MEStreamSinkStopped: Result := 'MEStreamSinkStopped';
    MEStreamSinkPaused: Result := 'MEStreamSinkPaused';
    MEStreamSinkRateChanged: Result := 'MEStreamSinkRateChanged';
    MEStreamSinkRequestSample: Result := 'MEStreamSinkRequestSample';
    MEStreamSinkMarker: Result := 'MEStreamSinkMarker';
    MEStreamSinkPrerolled: Result := 'MEStreamSinkPrerolled';
    MEStreamSinkScrubSampleComplete: Result := 'MEStreamSinkScrubSampleComplete';
    MEStreamSinkFormatChanged: Result := 'MEStreamSinkFormatChanged';
    MEStreamSinkDeviceChanged: Result := 'MEStreamSinkDeviceChanged';
    MEQualityNotify: Result := 'MEQualityNotify';
    MESinkInvalidated: Result := 'MESinkInvalidated';
    MEAudioSessionNameChanged: Result := 'MEAudioSessionNameChanged';
    MEAudioSessionVolumeChanged: Result := 'MEAudioSessionVolumeChanged';
    MEAudioSessionDeviceRemoved: Result := 'MEAudioSessionDeviceRemoved';
    MEAudioSessionServerShutdown: Result := 'MEAudioSessionServerShutdown';
    MEAudioSessionGroupingParamChanged: Result := 'MEAudioSessionGroupingParamChanged';
    MEAudioSessionIconChanged: Result := 'MEAudioSessionIconChanged';
    MEAudioSessionFormatChanged: Result := 'MEAudioSessionFormatChanged';
    MEAudioSessionDisconnected: Result := 'MEAudioSessionDisconnected';
    MEAudioSessionExclusiveModeOverride: Result := 'MEAudioSessionExclusiveModeOverride';
    MEPolicyChanged: Result := 'MEPolicyChanged';
    MEContentProtectionMessage: Result := 'MEContentProtectionMessage';
    MEPolicySet, METrustV1Anchor: Result := 'MEPolicySet';
    MEWMDRMLicenseBackupCompleted: Result := 'MEWMDRMLicenseBackupCompleted';
    MEWMDRMLicenseBackupProgress: Result := 'MEWMDRMLicenseBackupProgress';
    MEWMDRMLicenseRestoreCompleted: Result := 'MEWMDRMLicenseRestoreCompleted';
    MEWMDRMLicenseRestoreProgress: Result := 'MEWMDRMLicenseRestoreProgress';
    MEWMDRMLicenseAcquisitionCompleted: Result := 'MEWMDRMLicenseAcquisitionCompleted';
    MEWMDRMIndividualizationCompleted: Result := 'MEWMDRMIndividualizationCompleted';
    MEWMDRMIndividualizationProgress: Result := 'MEWMDRMIndividualizationProgress';
    MEWMDRMProximityCompleted: Result := 'MEWMDRMProximityCompleted';
    MEWMDRMLicenseStoreCleaned: Result := 'MEWMDRMLicenseStoreCleaned';
    MEWMDRMRevocationDownloadCompleted: Result := 'MEWMDRMRevocationDownloadCompleted';
    MEWMDRMV1Anchor: Result := 'MEWMDRMV1Anchor';
    METransformUnknown: Result := 'METransformUnknown';
    METransformNeedInput: Result := 'METransformNeedInput';
    METransformHaveOutput: Result := 'METransformHaveOutput';
    METransformDrainComplete: Result := 'METransformDrainComplete';
    METransformMarker: Result := 'METransformMarker';
    METransformInputStreamStateChanged: Result := 'METransformInputStreamStateChanged';
    MEByteStreamCharacteristicsChanged: Result := 'MEByteStreamCharacteristicsChanged';
    MEVideoCaptureDeviceRemoved: Result := 'MEVideoCaptureDeviceRemoved';
    MEVideoCaptureDevicePreempted: Result := 'MEVideoCaptureDevicePreempted';
    MEStreamSinkFormatInvalidated: Result := 'MEStreamSinkFormatInvalidated';
    MEEncodingParameters: Result := 'MEEncodingParameters';
    MEContentProtectionMetadata: Result := 'MEContentProtectionMetadata';
    MEDeviceThermalStateChanged: Result := 'MEDeviceThermalStateChanged';
    else
      Result := 'Unknown event';
end;


end.

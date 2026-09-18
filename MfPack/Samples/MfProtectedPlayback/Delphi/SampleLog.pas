// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SampleLog.pas
// Kind: Pascal Unit
// Release date: 19-09-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Provides thread-safe window-message logging and readable Media Foundation event names.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
// 19/09/2026 Tony                Delphi translation of ProtectedPlayback.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Parts of Microsoft ProtectedPlayback and CPlayer examples
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

unit SampleLog;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects;

const
  WM_APP_LOG = WM_APP + 10;

  procedure PostLogMessage(Window: HWND;
                           const Text: string);
  function FormatHR(HR: HRESULT): string;
  function MediaEventName(EventType: MediaEventType): string;


implementation

uses
  {System}
  System.SysUtils;


procedure PostLogMessage(Window: HWND;
                         const Text: string);
var
  MessageText: PString;

begin

  if (Window = 0) then
    Exit;

  New(MessageText);
  MessageText^ := Text;

  if not PostMessage(Window,
                     WM_APP_LOG,
                     0,
                     LPARAM(MessageText)) then
    Dispose(MessageText);
end;


function FormatHR(HR: HRESULT): string;
begin

  Result := Format('$%.8x',
                   [Cardinal(HR)]);
end;

function MediaEventName(EventType: MediaEventType): string;
begin

  case EventType of
    MEError: Result := 'MEError';
    MEExtendedType: Result := 'MEExtendedType';
    MESessionTopologySet: Result := 'MESessionTopologySet';
    MESessionTopologiesCleared: Result := 'MESessionTopologiesCleared';
    MESessionStarted: Result := 'MESessionStarted';
    MESessionPaused: Result := 'MESessionPaused';
    MESessionStopped: Result := 'MESessionStopped';
    MESessionClosed: Result := 'MESessionClosed';
    MESessionEnded: Result := 'MESessionEnded';
    MESessionRateChanged: Result := 'MESessionRateChanged';
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
    MEPolicyChanged: Result := 'MEPolicyChanged';
    MEContentProtectionMessage: Result := 'MEContentProtectionMessage';
    MEWMDRMLicenseAcquisitionCompleted: Result := 'MEWMDRMLicenseAcquisitionCompleted';
    MEWMDRMIndividualizationCompleted: Result := 'MEWMDRMIndividualizationCompleted';
    MEWMDRMIndividualizationProgress: Result := 'MEWMDRMIndividualizationProgress';
    MEWMDRMRevocationDownloadCompleted: Result := 'MEWMDRMRevocationDownloadCompleted';
  else
    Result := Format('MediaEventType(%d)',
                     [EventType]);
  end;
end;

end.

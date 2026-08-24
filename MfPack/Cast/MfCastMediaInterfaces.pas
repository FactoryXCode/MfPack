// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastMediaInterfaces.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Optional Media Foundation-specific publishing, preview, remux
//              and transcode interfaces for MfCast.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: -
//
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
unit MfCastMediaInterfaces;

interface

uses
  WinApi.Windows,
  WinApi.MediaFoundationApi.MfObjects,
  MfCastTypes,
  MfCastInterfaces;

type
  IMfCastSegmentPublisher = interface
    ['{696081C4-E77F-4F7D-A756-F84A16F45B56}']
    function BeginPresentation(const AContentType: string;
                               out AEntryPath: string): HRESULT;
    function GetByteStream(out AByteStream: IMFByteStream): HRESULT;
    function CompletePresentation(): HRESULT;
    function AbortPresentation(const AReason: HRESULT): HRESULT;
  end;

  IMfCastPreviewSink = interface
    ['{DE22302E-0B96-44C7-A7BF-644A97DAE34A}']
    function SetWindow(const AWindow: HWND): HRESULT;
    function IsEnabled(): Boolean;
    function ConfigureVideo(const AWidth: UINT32;
                            const AHeight: UINT32): HRESULT;
    function PresentSample(const ASample: IMFSample;
                           const ASampleTime100ns: Int64;
                           const ASampleDuration100ns: Int64): HRESULT;
    function ConfigureAudio(const AChannels: UINT32;
                            const ASamplesPerSecond: UINT32;
                            const ABitsPerSample: UINT32): HRESULT;
    function PresentAudioSample(const ASample: IMFSample;
                                const ASampleTime100ns: Int64;
                                const ASampleDuration100ns: Int64): HRESULT;
    function PauseAudio(): HRESULT;
    function ResumeAudio(): HRESULT;
    function Flush(): HRESULT;
  end;

  // Local Media Session preview for routes that do not pass decoded samples
  // through the Cast transcoder (direct HTTP/file playback and remuxing).
  IMfCastDirectPreviewPlayer = interface
    ['{7BE7043A-B984-40BE-86DE-0346174BAEF2}']
    function SetWindow(const AWindow: HWND): HRESULT;
    function IsEnabled(): Boolean;
    function IsActive(): Boolean;
    function Open(const ASourceName: string;
                  const AVolume: Single;
                  const AMuted: Boolean): HRESULT;
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function UpdateVideo(): HRESULT;
  end;

  IMfCastTranscodePipeline = interface
    ['{BB145D8E-BF87-4E9B-B325-E400E88BA5D5}']
    function Configure(const ASettings: TMfCastEncodingSettings): HRESULT;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(const ARequest: TMfCastTranscodeRequest;
                   const APublisher: IMfCastSegmentPublisher;
                   const APreviewSink: IMfCastPreviewSink): HRESULT;
    function Pause(): HRESULT;
    function Resume(): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function GetState(): TMfCastState;
  end;

  IMfCastRemuxPipeline = interface
    ['{BCB72B7C-F35F-4C39-B4ED-D45CC5F67A7B}']
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(const ARequest: TMfCastRemuxRequest;
                   const APublisher: IMfCastSegmentPublisher): HRESULT;
    function Pause(): HRESULT;
    function Resume(): HRESULT;
    function Stop(): HRESULT;
    function GetState(): TMfCastState;
  end;

implementation

end.

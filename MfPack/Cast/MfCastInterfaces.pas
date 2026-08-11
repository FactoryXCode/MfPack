// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastInterfaces.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Media Foundation-neutral interfaces for discovery, TLS
//              transport, Cast channel, HTTP serving, media inspection,
//              capability resolution, planning and the controller.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// Source: Parts of CPlayer Examples
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
unit MfCastInterfaces;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils,
  {Cast}
  MfCastTypes;

type

  IMfCastLogger = interface
    ['{BCC09A23-67E3-4B0A-A763-9BBE392868FC}']
    procedure Log(const ALevel: TMfCastLogLevel;
                  const ASource: string;
                  const AMessage: string);
  end;

  IMfCastDiscovery = interface
    ['{BA4108B2-1AB7-435E-A998-848A47B275A5}']
    function Configure(const AProtocol: TMfCastProtocolSettings;
                       const ASettings: TMfCastDiscoverySettings): HRESULT;
    procedure SetCallbacks(const ACallbacks: TMfCastDiscoveryCallbacks);
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(): HRESULT;
    function Stop(): HRESULT;
    function Refresh(): HRESULT;
    function GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
    function IsRunning(): Boolean;
  end;

  IMfCastTransport = interface
    ['{692D05FC-96A1-44DC-ABF4-4AD172AE84D7}']
    function Configure(const ASettings: TMfCastProtocolSettings): HRESULT;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Connect(const AHost: string;
                     const APort: Word): HRESULT;
    function Disconnect: HRESULT;
    function SendBuffer(const ABuffer: Pointer;
                        const ASize: Cardinal): HRESULT;
    function ReceiveBuffer(ABuffer: Pointer;
                           const ABufferSize: Cardinal;
                           out ABytesRead: Cardinal): HRESULT;
    function IsConnected: Boolean;
  end;

  IMfCastChannel = interface
    ['{42EDE32D-61A6-4D0E-B4BA-1BE5D9F9C992}']
    function Configure(const ASettings: TMfCastProtocolSettings): HRESULT;
    procedure SetCallbacks(const ACallbacks: TMfCastChannelCallbacks);
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Connect(const ADevice: TMfCastDevice): HRESULT;
    function Disconnect(): HRESULT;
    function LaunchReceiver(): HRESULT;
    function LoadMedia(const ARequest: TMfCastLoadRequest): HRESULT;
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function RequestReceiverStatus(): HRESULT;
    function RequestMediaStatus(): HRESULT;
    function SynchronizeMediaStatus(const ATimeoutMs: Cardinal): HRESULT;
    function GetState(): TMfCastState;
  end;

  IMfCastHttpContent = interface
    ['{26AF57D3-6935-47DA-A022-F0C3B7AC707E}']
    function GetContentType(): string;
    function GetLength(out ALength: UInt64): HRESULT;
    function CanSeek(): Boolean;
    function IsComplete: Boolean;
    function ReadAt(const AOffset: UInt64;
                    ABuffer: Pointer;
                    const ABufferSize: Cardinal;
                    out ABytesRead: Cardinal): HRESULT;
  end;

  // Optional capability implemented only by content that can grow while it is
  // being served. Static file and memory resources deliberately do not expose
  // this interface.
  IMfCastLiveHttpContent = interface
    ['{1EF8CFA8-1778-4F7D-B0D8-89BEB2A1E6B9}']
    function WaitForData(const AOffset: UInt64;
                         const ATimeoutMs: Cardinal): HRESULT;
  end;

  IMfCastHttpServer = interface
    ['{47DAF329-B5D9-423F-9764-409732E50C57}']
    function Configure(const ASettings: TMfCastHttpSettings): HRESULT;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start: HRESULT;
    function Stop: HRESULT;
    function Publish(const AResourceName: string;
                     const AContent: IMfCastHttpContent;
                     out APublishedPath: string): HRESULT;
    function Unpublish(const APublishedPath: string): HRESULT;
    function BuildUrl(const APublishedPath: string;
                      out AUrl: string): HRESULT;
    function IsRunning(): Boolean;
    function GetListenPort(): Word;
    function GetRequestCount(): Cardinal;
  end;

  IMfCastMediaInspector = interface
    ['{5D6397F8-9604-430A-B8D0-7D61DBDC46EA}']
    procedure SetLogger(const ALogger: IMfCastLogger);
    function Inspect(const ASourceName: string;
                     out AMediaInfo: TMfCastMediaInfo): HRESULT;
    function EnumerateTracks(const ASourceName: string;
                             out ATracks: TMfCastTrackInfoArray): HRESULT;
  end;

  IMfCastCapabilityResolver = interface
    ['{FB72D0F4-2406-4506-A505-74A090245B06}']
    procedure SetLogger(const ALogger: IMfCastLogger);
    function ResolveProfile(const ADevice: TMfCastDevice;
                            out AProfile: TMfCastDeviceProfile): HRESULT;
  end;

  IMfCastMediaPlanner = interface
    ['{159A5401-6097-4B49-932B-C805B987197E}']
    procedure SetLogger(const ALogger: IMfCastLogger);
    function ChooseMode(const ADevice: TMfCastDevice;
                        const AMediaInfo: TMfCastMediaInfo;
                        const ARequestedMediaMode: TMfCastMediaMode;
                        const ARequestedSubtitleMode: TMfCastSubtitleMode;
                        out ASelectedMediaMode: TMfCastMediaMode;
                        out ASelectedSubtitleMode: TMfCastSubtitleMode): HRESULT;
  end;

  IMfCastController = interface
    ['{595F9777-2220-4D88-A167-FEA2DFD6C46F}']
    function Configure(const ASettings: TMfCastSettings): HRESULT;
    procedure SetCallbacks(const ACallbacks: TMfCastControllerCallbacks);
    function GetCallbacks(): TMfCastControllerCallbacks;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function StartDiscovery: HRESULT;
    function StopDiscovery: HRESULT;
    function RefreshDiscovery: HRESULT;
    function GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
    function GetMediaTracks(const ASourceName: string;
                            out ATracks: TMfCastTrackInfoArray): HRESULT;
    function Connect(const ADevice: TMfCastDevice): HRESULT;
    function LoadFile(const ASourceName: string;
                      const ASubtitle: TMfCastSubtitleAsset;
                      const AMediaMode: TMfCastMediaMode;
                      const ASubtitleMode: TMfCastSubtitleMode;
                      const AStartTime100ns: Int64 = 0): HRESULT;
    function CastFile(const ADevice: TMfCastDevice;
                      const ASourceName: string;
                      const ASubtitle: TMfCastSubtitleAsset;
                      const AMediaMode: TMfCastMediaMode;
                      const ASubtitleMode: TMfCastSubtitleMode;
                      const AStartTime100ns: Int64 = 0): HRESULT;
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SelectAudioTrack(const ATrackId: Int64): HRESULT;
    function SelectSubtitleTrack(const ATrackId: Int64): HRESULT;
    function SelectSubtitle(const ASubtitle: TMfCastSubtitleAsset): HRESULT;
    function DisableSubtitles(): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function Disconnect(): HRESULT;
    function GetState(): TMfCastState;
    function GetHttpRequestCount(): Cardinal;
  end;

implementation

end.

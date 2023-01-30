// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: ChatTransport.pas
// Kind: Pascal / Delphi unit
// Release date: 04-10-2020
// Language: ENU
//
// Revision Version: 3.1.4
// Description: Baseclass for WasApiChat and WaveChat.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Note that this sample requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: DuckingCaptureSample: chattransport.h
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
unit ChatTransport;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.Messages;

type

  ChatTransportType = (ChatTransportWave,
                       ChatTransportWasapi);

  //
  //  Pure virtual class which defines a "Chat Transport"
  //

  CChatTransport = class
  protected
    _AppWindow: HWND;

  public
    constructor Create(_hWnd: HWND); virtual; abstract;
    function Initialize(const UseCaptureDevice: Boolean): Boolean; virtual; abstract;
    procedure Shutdown(); virtual; abstract;

    function StartChat(const HideFromVolumeMixer: Boolean): Boolean; virtual; abstract;
    procedure StopChat(); virtual; abstract;

    function TransportType(): ChatTransportType; virtual; abstract;

    // We left out the C++ handling of messages, to keep things simple and do it the Delphi way.
  end;


implementation

end.

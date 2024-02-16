//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Common.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.6
// Description: Contains common helpers.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips).
// Contributor(s): Tony Kalf (maXcomX)
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
// Source: Transcoding Example
//
// Copyright (c) Microsoft Corporation. All rights reserved .
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
unit Common;

interface

  // Undefine this when not needed!
  {$DEFINE SAVE_DEBUG_REPORT}
  {$DEFINE SHOW_IN_IDE_MESSAGES}

  uses
    WinApi.Windows,
    {MediaFoundationApi}

    {$IFDEF SAVE_DEBUG_REPORT}
    WinApi.MediaFoundationApi.MfMediaTypeDebug,
    {$ENDIF}
    System.SysUtils;

const
  // File filters                                                   filter index
  VIDEO_FILE_FILTER = 'Audio Video Interleave|*.avi|' +             // 1
                      'MPEG-4 Video with AAC Audio|*.mp4|' +        // 2
                      'MPEG-4 Video with Dolby AC-3 Audio|*.mp4|' + // 3
                      'Windows Media Video|*.wmv';                  // 4

  AUDIO_FILE_FILTER = 'Waveform Audio File Format|*.wav|' +         // 5
                      'MPEG Audio Layer III|*.mp3|' +               // 6
                      'Free Lossless Audio Codec|*.flac|' +         // 7
                      'MPEG-4 Audio|*.m4a|' +                       // 8
                      'Windows Media Audio|*.wma';                  // 9

  ALL_FILE_FILTER   = 'All Files|*.*';                              // 10  Source File Dialog only

// ITEM                                  URL
// MF Supported Media Formats            https://github.com/MicrosoftDocs/win32/blob/docs/desktop-src/medfound/supported-media-formats-in-media-foundation.md
//

  {$IFDEF SAVE_DEBUG_REPORT}
  var FMediaTypeDebug: TMediaTypeDebug;
  {$ENDIF}

  // Simple debugMsg message handler that reports by default in the IDE Messages screen or
  // shows a messagedialog at runtime.
  procedure DebugMsg(pErrMsg: string;
                     pHr: HResult);


implementation

uses
  Vcl.Dialogs,
  WinApi.MediaFoundationApi.MfUtils;

// DebugMsg
procedure DebugMsg(pErrMsg: string;
                   pHr: HResult);
begin
{$IFDEF SHOW_IN_IDE_MESSAGES}
  OutputDebugString(StrToPWideChar(Format('Error: %s (hr = %d)',
                                          [pErrMsg, pHr])))
{$ELSE}

  ShowMessage(format('Error: %s (hr = %d)',
                     [pErrMsg, pHr]));
{$ENDIF}
end;


initialization

  {$IFDEF SAVE_DEBUG_REPORT}
  FMediaTypeDebug := TMediaTypeDebug.Create();
  {$ENDIF}

finalization
  {$IFDEF SAVE_DEBUG_REPORT}
  FreeAndNil(FMediaTypeDebug);
  {$ENDIF}
end.

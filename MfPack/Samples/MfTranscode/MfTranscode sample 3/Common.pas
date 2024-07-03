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
// Revision Version: 3.1.7
// Description: Contains common helpers.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
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
    WinApi.Messages,
    {MediaFoundationApi}

    {$IFDEF SAVE_DEBUG_REPORT}
    WinApi.MediaFoundationApi.MfMediaTypeDebug,
    {$ENDIF}
    System.SysUtils,
    WinApi.MediaFoundationApi.MfObjects,
    WinApi.MediaFoundationApi.CodecApi;


const
  // File filters                                                          filter index
  VIDEO_FILE_FILTER  = 'Audio Video Interleave|*.avi|' +                    // 1
                       'MPEG-4 H.265/HEVC Video with AAC Audio|*.mp4|' +    // 2
                       'MPEG-4 H.264 Video with Dolby AC-3 Audio|*.mp4|' +  // 3
                       'Windows Media Video|*.wmv';                         // 4

  AUDIO_FILE_FILTER  = 'Waveform Audio File Format|*.wav|' +                // 5
                       'MPEG Audio Layer III|*.mp3|' +                      // 6
                       'Dolby AC-3 audio|*.ac3|' +                          // 7
                       'Free Lossless Audio Codec|*.flac|' +                // 8
                       'Advanced Audio Coding (AAC)|*.aac|' +               // 9
                       'MPEG-4 Audio|*.m4a|' +                              // 10
                       'Windows Media Audio|*.wma';                         // 11

  CUSTOM_FILE_FILTER = 'Custom format|';                                    // 12

  ALL_FILE_FILTER    = 'All Files|*.*';                                     // Source File Dialog only

  {$IFDEF SAVE_DEBUG_REPORT}
  var FMediaTypeDebug: TMediaTypeDebug;
  {$ENDIF}

  // Simple debugMsg message handler that reports by default in the IDE Messages screen or
  // shows a messagedialog at runtime.
  procedure DebugMsg(pErrMsg: string;
                     pHr: HResult);

  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);

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


procedure HandleThreadMessages(AThread: THandle;
                               AWait: Cardinal = INFINITE);
var
  oMsg: TMsg;

begin

  while (MsgWaitForMultipleObjects(1,
                                   AThread,
                                   False,
                                   AWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(oMsg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if oMsg.Message = WM_QUIT then
        Exit;

      TranslateMessage(oMsg);
      DispatchMessage(oMsg);
    end;
end;


Initialization

  {$IFDEF SAVE_DEBUG_REPORT}
  FMediaTypeDebug := TMediaTypeDebug.Create();
  {$ENDIF}

Finalization
  {$IFDEF SAVE_DEBUG_REPORT}
  FreeAndNil(FMediaTypeDebug);
  {$ENDIF}

// ITEM                                  URL
// MF Supported Media Formats            https://github.com/MicrosoftDocs/win32/blob/docs/desktop-src/medfound/supported-media-formats-in-media-foundation.md
//



end.

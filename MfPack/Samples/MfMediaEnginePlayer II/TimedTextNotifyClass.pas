// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: TimedTextNotifyClass.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 2.6.4
// Description: TimedText callback implementation.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues, (Ciaran).
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 2004)
//                                #1 Autobahn
//------------------------------------------------------------------------------
//
// Remarks: Requires MfAdditional (XE) 2.6.4 (or higher) and MfComponents to be installed!
//          All TimedText interfaces require Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
// =============================================================================
// Source: -
//
// Copyright © FactoryX, Netherlands/Australia
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit TimedTextNotifyClass;

interface

uses
  {WinApi}
  WinApi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  MfPack.WinError,
  MfPack.MfIdl,
  MfPack.MfMediaEngine,
  MfPack.MfObjects;

const
  WM_TIMEDTEXTNOTIFY = WM_USER + 1002;

  // SubtitleFileExtensions
   EXTSUBRIP    = '.srt';
   EXTSUBVIEWER = '.sub';   // > not implemented
   EXTYOUTUBE   = '.sbv';   // > not implemented
   EXTSAMI      = '.smi';

type

  TcTimedTextNotify = class(TInterfacedObject, IMFTimedTextNotify)
  private

    gv_url: String;
    gv_label: PWideChar;
    gv_lang: PWideChar;
    gv_Handle: THandle;  // handle to caller
    
    ttnmsg_text: string;   // Returned text from callback.
    ttnmsg_Font: TFont;    // Returned text font.
    ttnmsg_Lines: DWord;   // Returned textlines from callback.
    ttnmsg_TrackId: DWord;
    ttnmsg_startTime: Double;  // Returned start time
    ttnmsg_durTime: Double;    // Returned duration
    ttnmsg_endTime: Double;    // Returned end time

    ttLiveText: LPWSTR;      // live text / comment that can be injected.

    bTrackAdded: Boolean;

    // EVENT HANDLERS ////
    //////////////////////////////////////////////////////
    procedure TrackAdded(trackId: DWORD); stdcall;

    procedure TrackRemoved(trackId: DWORD); stdcall;

    procedure TrackSelected(trackId: DWORD;
                            selected: BOOL); stdcall;

    procedure TrackReadyStateChanged(trackId: DWORD); stdcall;

    procedure Error(errorCode: MF_TIMED_TEXT_ERROR_CODE;
                    extendedErrorCode: HResult;
                    sourceTrackId: DWORD); stdcall;

    procedure Cue(cueEvent: MF_TIMED_TEXT_CUE_EVENT;
                  currentTime: Double;
                  cue: IMFTimedTextCue); stdcall;

    // Resets the timed-text-notify object.
    procedure Reset(); stdcall;

    // ////////////////////////////////////////////////////////////////////

    // Send message to caller new text is presented
    procedure NotifyUpdate();

    // Set font properties, if defined in the subtitlefile
    procedure SetFontProperties(fstyle: IMFTimedTextStyle);

    //  ///////////////////////////////////////////////////////////////////

    // Getters and setters
    function GetLiveText(): string;
    procedure SetLiveText(value: string);
    function GetSubTitle(): string;
    procedure SetSubTitle(value: string);
    function GetLines(): DWord;
    function GetFont(): TFont;
    function GetStartTime(): Double;
    function GetEndTime(): Double;
    function GetTimeSpan(): Double;


  public

    // Constructor
    constructor Create(const url: String;
                       pclabel: PWideChar;
                       pclang: PWideChar;
                       CallerHandle: HWND); overload;

    // Destructor
    destructor Destroy(); override;

    // Properties
    property LiveText: String read GetLiveText write SetLiveText;
    property SubTitle: String read GetSubTitle write SetSubTitle;
    property Lines: DWord read GetLines;
    property Font: TFont read GetFont;

    property StartTime: Double read GetStartTime;
    property EndTime: Double read GetEndTime;
    property TimeSpan: Double read GetTimeSpan;

  end;


implementation

// CONSTRUCTOR
constructor TcTimedTextNotify.Create(const url: String;
                                     pclabel: PWideChar;
                                     pclang: PWideChar;
                                     CallerHandle: HWND);
begin
  inherited Create();

  gv_url := url;
  gv_label := pclabel;
  gv_lang := pclang;
  gv_Handle := CallerHandle;

  ttnmsg_TrackId := 0;
  // Create font
  ttnmsg_Font := TFont.Create();
  ttnmsg_Font.Name := 'Default';
  ttnmsg_Font.Color := clWhite;
  ttnmsg_Font.Size := 12;
  ttnmsg_Font.Style := [fsBold];

end;


// DESTRUCTOR
destructor TcTimedTextNotify.Destroy();
begin
  FreeAndNil(ttnmsg_Font);

  inherited Destroy();
end;




// callback methods

procedure TcTimedTextNotify.TrackAdded(trackId: DWORD);
begin
  bTrackAdded := (trackId > ttnmsg_TrackId);
end;


procedure TcTimedTextNotify.TrackRemoved(trackId: DWORD);
begin
  bTrackAdded := (trackId = ttnmsg_TrackId);
end;


procedure TcTimedTextNotify.TrackSelected(trackId: DWORD;
                                          selected: BOOL);
begin
  // Not implemented
end;


procedure TcTimedTextNotify.TrackReadyStateChanged(trackId: DWORD);
begin
  // MF_TIMED_TEXT_TRACK_READY_STATE
  case trackId of
    // MF_TIMED_TEXT_TRACK_READY_STATE_NONE  > Loading track cues has not been started yet.
    0: begin
         // Not implemented

       end;
    // MF_TIMED_TEXT_TRACK_READY_STATE_LOADING  > Track cues are being loaded.
    1: begin
         // Not implemented

       end;
    // MF_TIMED_TEXT_TRACK_READY_STATE_LOADED  > Track cues are loaded and ready.
    2: begin
         // Not implemented

       end;
    // MF_TIMED_TEXT_TRACK_READY_STATE_ERROR  > Track error occurred.
    3: begin
         // Not implemented

       end;
  end;
end;


procedure TcTimedTextNotify.Error(errorCode: MF_TIMED_TEXT_ERROR_CODE;
                                  extendedErrorCode: HResult;
                                  sourceTrackId: DWORD);
begin
  // Not implemented
end;


procedure TcTimedTextNotify.Cue(cueEvent: MF_TIMED_TEXT_CUE_EVENT;
                                currentTime: Double;
                                cue: IMFTimedTextCue);
var
  trackKind: MF_TIMED_TEXT_TRACK_KIND;
  fText: IMFTimedTextFormattedText;
  fstyle: IMFTimedTextStyle;
  hr: HResult;
  TxtBuffer: LPWSTR;
  dwFirstChar, dwLength, SubFmtCount: DWord;
  i: Integer;

begin

try
  // When playing stops, there is nothing to do here.
  if (cueEvent = MF_TIMED_TEXT_CUE_EVENT_CLEAR) then
    begin
      ttnmsg_text := '';
      Exit;
    end;

  // check what type of file we are dealing with
  trackKind := cue.GetCueKind();

  case Ord(trackKind) of
    // MF_TIMED_TEXT_TRACK_KIND_SUBTITLES
    1: begin
         ttnmsg_Lines := 0;
         ttnmsg_TrackId := cue.GetId();
         // Each track is called on the start and end time.
         // On the end of the timespan we erase the subtitle.
         if (cueEvent = MF_TIMED_TEXT_CUE_EVENT_INACTIVE) then
           ttnmsg_text := ''
         else
           begin
             // Get number of subtitlelines
             ttnmsg_Lines := cue.GetLineCount();  // Zero based index!

             // If there are multiple lines, we have to itterate through them
             for i := 0 to ttnmsg_Lines -1 do
               begin
                 cue.GetLine(i,
                             fText);

                 {hr :=} fText.GetText(TxtBuffer);

                 if (i = 0) then
                   ttnmsg_text := TxtBuffer
                 else if (i > 0) then
                   ttnmsg_text := ttnmsg_text + chr(13) + TxtBuffer;
              end;

             // Gets the number of subformats in the formatted timed-text object.
             SubFmtCount := fText.GetSubformattingCount();

             for i := 0 to SubFmtCount -1 do
               begin
                 // font style  Note; If no fontstyle can be found, result will be <> S_OK
                 hr := fText.GetSubformatting(i,
                                              dwFirstChar,
                                              dwLength,
                                              fstyle);

                // If fstyle <> Nil or hr <> 0, there are no fontstyle definitions
                if SUCCEEDED(hr) then
                  SetFontProperties(fstyle); // set font properties
               end;
         end; // MF_TIMED_TEXT_TRACK_KIND_SUBTITLES
    end;
  end; // case

  // Get new time info
  ttnmsg_startTime := cue.GetStartTime();
  ttnmsg_durTime := cue.GetDuration();
  ttnmsg_endTime := ttnmsg_startTime + ttnmsg_durTime;

finally
  // Send message to the window we got the HWND (gv_Handle) from.
  NotifyUpdate();
end;
end;


procedure TcTimedTextNotify.Reset();
begin
  // Not implemented
end;


// Send text and properties to caller
procedure TcTimedTextNotify.NotifyUpdate();
begin

  // Send a message to the caller a text event occured
  SendMessage(gv_Handle,
              WM_TIMEDTEXTNOTIFY,
              WPARAM(1),
              0);

end;



procedure TcTimedTextNotify.SetFontProperties(fstyle: IMFTimedTextStyle);
var
  hr: HResult;
  fgColor: TColor;
  argb: MFARGB;
  bbold: BOOL;
  fSize: Double;
  fSizeType: MF_TIMED_TEXT_UNIT_TYPE;
  fttStyle: MF_TIMED_TEXT_FONT_STYLE;
  fFontName: PWideChar;

begin

  // Set the font properties

  // Color
  hr := fstyle.GetColor(argb);
  if Succeeded(hr) then
    begin
      CopyMFARGBToTColor(argb, fgColor);
      ttnmsg_Font.Color := fgColor;
    end
  else
    ttnmsg_Font.Color := clWhite;

  // Bold
  hr := fstyle.GetBold(bbold);
  if Succeeded(hr) then
    begin
      if bbold then
        ttnmsg_Font.Style := ttnmsg_Font.Style + [fsBold]
      else
        ttnmsg_Font.Style := ttnmsg_Font.Style - [fsBold];
    end
  else
    ttnmsg_Font.Style := [];

  // Size
  hr := fstyle.GetFontSize(fSize, fSizeType);
  if Succeeded(hr) then
    begin
      if (fSizeType = MF_TIMED_TEXT_UNIT_TYPE_PIXELS) then
        ttnmsg_Font.Size := Trunc(fSize)
      else if (fSizeType = MF_TIMED_TEXT_UNIT_TYPE_PERCENTAGE) then
        ttnmsg_Font.Size := ttnmsg_Font.Size * Trunc(fSize/100);
    end
  else
    ttnmsg_Font.Size := 14;


  // Style
  hr := fstyle.GetFontStyle(fttStyle);
  if Succeeded(hr) then
    begin
      if (fttStyle = MF_TIMED_TEXT_FONT_STYLE_NORMAL) then
        ttnmsg_Font.Style := ttnmsg_Font.Style - []
      else if (fttStyle = MF_TIMED_TEXT_FONT_STYLE_ITALIC) then
        ttnmsg_Font.Style := ttnmsg_Font.Style + [fsItalic]
      else // MF_TIMED_TEXT_FONT_STYLE_OBLIQUE  Delphi has no slightly sloped styles,
           // So we keep it by Italic
           ttnmsg_Font.Style := ttnmsg_Font.Style + [fsItalic];
    end
  else
    ttnmsg_Font.Style := [];

  // Name
  hr := fstyle.GetName(fFontName);
  if Succeeded(hr) then
    ttnmsg_Font.Name := String(fFontName)
  else
    ttnmsg_Font.Name := 'Default';

end;



// Getters and setters /////////////////////////////////////////////////////////
function TcTimedTextNotify.GetLiveText(): string;
begin
  Result := String(ttLiveText);
end;

procedure TcTimedTextNotify.SetLiveText(value: string);
begin
  if (ttLiveText <> value) then
    ttLiveText := LPWSTR(value);
end;

function TcTimedTextNotify.GetSubTitle(): string;
begin
  Result := String(ttnmsg_text);
end;

procedure TcTimedTextNotify.SetSubTitle(value: string);
begin
  if (ttnmsg_text <> value) then
    ttnmsg_text := LPWSTR(value);
end;

function TcTimedTextNotify.GetLines(): DWord;
begin
  Result := ttnmsg_Lines;
end;

function TcTimedTextNotify.GetFont(): TFont;
begin
  Result := ttnmsg_Font;
end;


function TcTimedTextNotify.GetStartTime(): Double;
begin
  Result := ttnmsg_startTime;
end;

function TcTimedTextNotify.GetEndTime(): Double;
begin
  Result := ttnmsg_endTime;
end;

function TcTimedTextNotify.GetTimeSpan(): Double;
begin
  Result := ttnmsg_durTime;
end;


end.

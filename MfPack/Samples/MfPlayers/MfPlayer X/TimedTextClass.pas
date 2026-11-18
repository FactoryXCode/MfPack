// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Brazil. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: TimedTextClass.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Revision Version: 4.1.0
// Description: This unit contains methods to get and
//              present TimedText from currently SubRib and MicroDvd files.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
// ----------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Parts from FactoryX.SubtitleConv.
//
// Copyright (c) FactoryX. All rights reserved.
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
unit TimedTextClass;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.StrUtils,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.RegularExpressions,
  System.UITypes,
  {vcl}
  Vcl.Graphics,
  Vcl.Dialogs,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  LangTags,
  MfPCXConstants;

// function ContainsText returns true if the subtext is found, without case-sensitivity, in the given text.
// In StrUtils you'll also find handy functions like StartsText, EndsText and ReplaceText


// TIMEDTEXT MAIN //////////////////////////////////////////////////////////////

type
  TTimedTextId = (ttSubRip, ttMicroDvd, ttYouTube, ttSAMI, ttWebVTT, ttUnknown, ttError);

  TFormattedText = record
    private
      constructor Create(aFont: TFont);
    public
      TextLine : string;
      TextFont : TFont;
  end;
  TFormattedTextArray = array of TFormattedText;

  TSubTitleTrack = record
  public
    procedure Init();
    procedure Clear();
  public
    stIndex       : Integer;          // track index
    Start         : MFTIME;           // Start time
    Stop          : MFTIME;           // End time
    Duration      : MFTIME;           // Duration
    // Each string can have it's own font
    TrackText     : TFormattedTextArray;  // text lines and font
  end;
  TSubTitleTracksArray = array of TSubTitleTrack;


  TMfTimedText = class(TObject)
  protected
    ap_SubTitleTracks           : TSubTitleTracksArray;
    tp_SubTitleFile             : TextFile;

  private
    m_bTimedTextNotifyProcessed : Boolean;                   // Signal from floating form we need a new track
    ip_TickIndex                : Integer;                   //
    ip_Tracks                   : Integer;                   // Number of tracks in tracklist
    hp_HwSender                 : HWnd;                      // Handle from sender
    hp_TTextHandle              : HWnd;                      // Handle to this object
    sp_Url                      : WideString;                // Filename including path
    tp_FileType                 : TTXT_TYPE;                 // Timed text file type
    sp_PreferredLanguage        : string;                    // Preferred language for subtitling
    sp_FriendlyLangName         : string;                    // Friendly localised languagename
    bp_AutoFormatText           : Boolean;                   // Option for automated formatting
    cp_clDefaultTextColor       : TColor;                    // DEfault text color
    lp_MatchList                : TStringlist;               // Global matchlist, stores tracktimes
    tp_SubTitleTrack            : TSubTitleTrack;            // Presentation record
    fTrackFont                  : TFont;

    // Catches all messages to this object
    procedure WndProc(var Msg: TMessage);

    // Sends a msg to a window a new timedtext record has been processed.
    // The result can be any of the following values:
    // 0, pending state
    // > 0 Active index
    // -1, End of index
    // -10, error, No index found
    function SendSubTitleText(index: Integer): Integer;

    // Helper to get a valid colorvalue from a string
    function StrToColor(sValue: string): TColor;

    // Process srt font tags and store to list
    procedure ProcessSrtTags(fText: string;
                             trackIndex: integer;
                             txtLine: integer);
    // Remove srt font tags
    procedure DeleteSrtFontTags(var fText: string);

    // Process vtt font tags and store to list
    procedure ProcessVttTags(fText: string;
                             trackIndex: integer;
                             txtLine: integer);

    // Remove vtt font tags
    procedure DeleteVttFontTags(var fText: string);

    // Write .srt file in to memory
    function ReadSubRipFile(const sUrl: WideString): HResult;

    // Write .vtt file in to memory
    function ReadWEBVTTFile(const sUrl: WideString): HResult;

    // Write .sub file in to memory
    function ReadMicroDvdFile(const sUrl: WideString): HResult;

    // Clears the SubTitle array and matchlist
    procedure Clear();

    // RegEx implementation
    function GetMatchCount(const pattern: string; // RegEx pattern
                           const txt: string;     // Text to search in
                           const options: TRegExOptions = [roNotEmpty]): integer;
{$HINTS OFF}
    // Not used, keep for later implementations
    function GetMatch(const pattern: string; // RegEx pattern
                      const txt: string;     // Text to search in
                      const options: TRegExOptions = [roNotEmpty]): string;
{$HINTS ON}
    // compares given pattern with given string
    function IsMatch(const pattern: string;
                     const txt: string): Boolean;

    // Adds match to list
    procedure AddMatchToList(match: TMatch);

    // setters/getters
    procedure SetAutoFormatText(aValue: Boolean);
    procedure SetDefaultTextColor(aValue: TColor);
    procedure SetPreferredLanguage(aValue: string);
    procedure SetTrackIndex(aValue: Integer);
    procedure SetTimedTextFile(aValue: WideString);

  public
    // constructor & destructor
    constructor Create(hwSender: HWnd;
                       TimedTextFile: WideString;
                       SubtitleLanguage: string);
    procedure BeforeDestruction(); override;
    destructor Destroy(); override;

    function GetClosestStartTime(search: MFTIME): MFTIME;
    // Open, check and decide what filetype to process
    function OpenTimedTextFile(const sUrl: WideString): HResult;

    // Properties
    property AutoFormatText: Boolean read bp_AutoFormatText write SetAutoFormatText default True;
    property DefaultTextColor: TColor read cp_clDefaultTextColor write SetDefaultTextColor default clWhite;
    property Handle: HWnd read hp_TTextHandle;
    property PreferredLanguage: string read sp_PreferredLanguage write SetPreferredLanguage;
    property FriendlyLanguage: string read sp_FriendlyLangName;
    property Track: TSubTitleTrack read tp_SubTitleTrack; // multiple formatted lines
    property TrackIndex: Integer read ip_TickIndex write SetTrackIndex default 0;
    property Tracks: Integer read ip_Tracks;
    property TimedTextFile: WideString read sp_Url write SetTimedTextFile;
  end;

var
  // TimedText object
  fTimedText: TMfTimedText;


implementation


const
  Kernel32Lib = 'kernel32.dll';    // Also declared in WinApi.Windows


constructor TMfTimedText.Create(hwSender: HWnd;
                                TimedTextFile: WideString;
                                SubtitleLanguage: string);
begin

  hp_HwSender       := hwSender;
  sp_Url            := TimedTextFile;

  // Do not call property PreferredLanguage directly, because this will
  // initiate function OpenTimedTextFile. At this moment we don't want to.
  sp_PreferredLanguage := SubtitleLanguage;
  // create a handle for the timer
  hp_TTextHandle := AllocateHWnd(WndProc);
  // Default color
  cp_clDefaultTextColor := clWhite;
  // Global stringlist to store matches
  lp_MatchList := TStringList.Create();
  // Create trackfont
  fTrackFont := TFont.Create();
  // Set the default font properties
  with fTrackFont do
    begin
      Name        := 'Verdana';
      Charset     := DEFAULT_CHARSET;
      Orientation := 0;
      Size        := 12;
      Orientation := 0;
      Quality     := fqDefault;
      Pitch       := fpDefault;
      Height      := -19;
      Color       := clWhite;
      Style       := [fsBold];
    end;
  AutoFormatText := True;
  pc_LanguageTags := TLanguageTags.Create();
end;


procedure TMfTimedText.BeforeDestruction();
begin
  Clear();
  FreeAndNil(lp_MatchList);
  fTrackFont.Free;
  fTrackFont := Nil;
  pc_LanguageTags.Free;
  pc_LanguageTags := Nil;
  inherited BeforeDestruction();
end;


destructor TMfTimedText.Destroy();
begin
  DeAllocateHWnd(hp_TTextHandle);
  inherited Destroy();
end;


procedure TMfTimedText.WndProc(var Msg: TMessage);
begin
  // Initiate the first track
  if (Msg.Msg = WM_TIMEDTEXTNOTIFY_INIT) then
    begin
      m_bTimedTextNotifyProcessed := True;
      SendSubTitleText(0);  // Get first track
    end
  // Get the next track
  else if (Msg.Msg = WM_TIMEDTEXTNOTIFY_PROCESSED) then
    begin
      m_bTimedTextNotifyProcessed := True;
      SendSubTitleText(TrackIndex + 1);
    end
  else
    DefWindowProc(hp_TTextHandle,
                  Msg.Msg,
                  Msg.wParam,
                  Msg.lParam);
end;


// searches for srt or other implemented formats and opens them by corresponding language
function TMfTimedText.OpenTimedTextFile(const sUrl: WideString): HRESULT;
var
  Kind,Index,J: Integer;
  Extension: string;
  BaseName: WideString;
begin
  Clear();
  sp_Url := '';
  BaseName := ChangeFileExt(sUrl,'');
  for Kind := 0 to 2 do
    begin
      case Kind of
        0: Extension := EXTSUBRIP;
        1: Extension := EXTWEBVTT;
        2: Extension := EXTMICRODVD;
      end;
      pc_LanguageTags.TimedTxtPropsArray := pc_LanguageTags.ReadFileTags(BaseName,PreferredLanguage,0,Extension);
      if Length(pc_LanguageTags.TimedTxtPropsArray) = 0 then Continue;
      Index := 0;
      for J := 0 to High(pc_LanguageTags.TimedTxtPropsArray) do
        if pc_LanguageTags.TimedTxtPropsArray[J].bActiveFile then
          begin Index := J;Break;end;
      case Kind of
        0: Result := ReadSubRipFile(pc_LanguageTags.TimedTxtPropsArray[Index].sFile);
        1: Result := ReadWEBVTTFile(pc_LanguageTags.TimedTxtPropsArray[Index].sFile);
        2: Result := ReadMicroDvdFile(pc_LanguageTags.TimedTxtPropsArray[Index].sFile);
      end;
      if SUCCEEDED(Result) then
        begin
          sp_Url := pc_LanguageTags.TimedTxtPropsArray[Index].sFile;
          tp_FileType := pc_LanguageTags.TimedTxtPropsArray[Index].sTTxtType;
          sp_FriendlyLangName := pc_LanguageTags.TimedTxtPropsArray[Index].sFriendlyLanguageName;
          ip_Tracks := Length(ap_SubTitleTracks);
          while (ip_Tracks > 0) and (Length(ap_SubTitleTracks[ip_Tracks-1].TrackText)=0) do Dec(ip_Tracks);
          SetLength(ap_SubTitleTracks,ip_Tracks);
          if ip_Tracks > 0 then tp_SubTitleTrack := ap_SubTitleTracks[0];
        end;
      Exit;
    end;
  Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
end;


procedure TMfTimedText.AddMatchToList(match: TMatch);
var
  Group : TGroup;
  i : integer;

begin
  lp_MatchList.Append(match.Value);
  if (match.Groups.Count > 1) then
    begin
      //group 0 is the entire match, so the first real group is 1
      for i := 1 to match.Groups.Count -1 do
        begin
          Group := match.Groups.Item[i];
          lp_MatchList.Append(Group.Value);
        end;
    end;

  match := match.NextMatch;
  if match.Success then
    AddMatchToList(match);
end;


function TMfTimedText.GetMatchCount(const pattern: string; // RegEx pattern
                                    const txt: string;     // Text to search in
                                    const options: TRegExOptions = [roNotEmpty]): Integer;
var
  RegEx : TRegEx;
  Match : TMatch;

begin
  lp_MatchList.Clear();

try
  RegEx := TRegEx.Create(pattern, options);
  Match := RegEx.Match(txt);
  if match.Success then
    begin
      AddMatchToList(Match);
      Result := lp_MatchList.Count;
    end
  else
    Result := 0;

except
  on e: Exception do
    begin
      Result := GetLastError();
    end;
end;
end;


function TMfTimedText.GetMatch(const pattern: string; // RegEx pattern
                               const txt: string;     // Text to search in
                               const options: TRegExOptions = [roNotEmpty]): string;
var
  RegEx : TRegEx;
  Match : TMatch;

begin
  RegEx := TRegEx.Create(pattern, options);
  Match := RegEx.Match(txt);

  if match.Success then
    Result := Match.Value
  else
    Result := '';
end;


// compares given pattern with given string
function TMfTimedText.IsMatch(const pattern: string;
                              const txt: string): Boolean;
begin
  Result := TRegEx.IsMatch(txt, pattern); // For example: '[a-z][a-z]_[A-Z][A-Z]' or '_[A-Z][A-Z]' or '[a-z][a-z]_' etc.
end;


function TMfTimedText.ReadSubRipFile(const sUrl: WideString): HResult;
var
  hr: HResult;
  bPrev: Boolean;
  I,
  J,
  aSize,
  iMatches,
  intCheck: Integer;
  flTemp: TStrings;
  pwInt: PWideChar;

begin
  hr := S_OK;
  bPrev := False;
  flTemp := TStringList.Create();
  flTemp.LoadFromFile(sUrl);
  if (flTemp.Count > 0) and StartsText('WEBVTT',Trim(flTemp[0])) then flTemp.Delete(0);
  while (flTemp.Count > 0) and (Trim(flTemp[0]) = '') do flTemp.Delete(0);
  if (flTemp.Count > 0) and (Trim(flTemp[flTemp.Count-1]) <> '') then flTemp.Add('');

try
try

  // Remove empty lines if more than 1 and/or insert a track marker
  for I := flTemp.Count - 1 downto 0 do
    begin
      if (flTemp.Strings[I] = Trim('')) and (bPrev = False) then
        begin
          flTemp.Strings[I] := IMARKER;  // insert a track end tag
          inc(bPrev);
        end
      else if (flTemp.Strings[I] = Trim('')) and (bPrev = True) then
        flTemp.Delete(I)
      else if (flTemp.Strings[I] <> Trim('')) then
        bPrev := False;
    end;

  SetLength(ap_SubTitleTracks, 1);
  aSize := 0;
  I := 0;

  while (I < flTemp.Count -1) do
    begin
      // Get index
      if TryStrToInt(flTemp.Strings[I], intCheck) then  // we found a track index
        begin
          // Get the track index
          if StrToIntDef(flTemp.Strings[I], 0) > 0 then
            begin
              ap_SubTitleTracks[aSize].stIndex := StrToIntDef(flTemp.Strings[I], 0);
              ip_Tracks := ap_SubTitleTracks[aSize].stIndex;
            end;
        end
      else
        // Get time codes
        if (Pos(WTP, flTemp.Strings[I]) > 0) then
          begin
            // Get the start and stop time from this format 00:00:00,000 --> 00:00:00,000
            iMatches := GetMatchCount('[0-9][0-9]:[0-5][0-9]:[0-5][0-9][,.][0-9][0-9][0-9]',
                                      flTemp.Strings[I],
                                      [roSingleLine, roNotEmpty]);

            if (iMatches = 2) then
              begin
                // Calculate begin
                ap_SubTitleTracks[aSize].Start := TimeToHnsTime(lp_MatchList.Strings[0], False);
                // Calculate end
                ap_SubTitleTracks[aSize].Stop := TimeToHnsTime(lp_MatchList.Strings[1], False);
                // Calculate duration
                ap_SubTitleTracks[aSize].Duration := ap_SubTitleTracks[aSize].Stop - ap_SubTitleTracks[aSize].Start;
              end
            else
              // The file or format is corrupted
              begin
                hr := E_INVALIDARG;
                Break;
              end;
            lp_MatchList.Clear();
          end
        else
          // Get the text tracks
          if (Pos(WTP, flTemp.Strings[I]) = 0) or (TryStrToInt(flTemp.Strings[I], intCheck) = false) then
            begin
              J := 0;
              while (Pos(IMARKER, flTemp.Strings[I]) = 0) do
                begin
                  inc(J);
                  // Increase the SubTitleLines array
                  SetLength(ap_SubTitleTracks[aSize].TrackText, J);
                  // Clear paragraph tags and set the fontproperties for each record.
                  // fTextFormat can be a custom or auto format, where autoformat is the format
                  // defined by the tags.
                  ProcessSrtTags(flTemp.Strings[I], aSize, J -1);
                  Inc(I);
                end;

              // Add New record
              if (Trim(flTemp.Strings[I]) <> '') and (I < flTemp.Count - 1) then
                begin
                  Inc(aSize);
                  SetLength(ap_SubTitleTracks, aSize +1);
                end;
            end
          else
            // The file or format is corrupted
            begin
              hr := E_INVALIDARG;
              Break;
            end;
        inc(I);
   end; // while

except
  pwInt := PWideChar(InttoStr(hr));

  MessageBox(0,
             PWideChar('An exception has been returned. (hr: ' + pwInt + ')'),
             PWideChar('Error'),
             MB_ICONERROR);
end;

finally
  flTemp.Free;
  if FAILED(hr) then
    begin
      Clear();
      lp_MatchList.Clear();
    end;
  Result := hr;
end;
end;


function TMfTimedText.ReadWEBVTTFile(const sUrl: WideString): HRESULT;
begin
  Result := ReadSubRipFile(sUrl);
end;


function TMfTimedText.ReadMicroDvdFile(const sUrl: WideString): HRESULT;
var
  Lines: TStringList;
  Value,Payload,TextLine: string;
  I,P,N,J: Integer;
  FirstFrame,LastFrame: Int64;
  FrameRate: Double;
  Settings: TFormatSettings;
begin
  Lines := TStringList.Create();
  try
    Lines.LoadFromFile(sUrl);
    FrameRate := 25.0;
    Settings := TFormatSettings.Create();
    Settings.DecimalSeparator := '.';
    for I := 0 to Lines.Count-1 do
      begin
        Value := Trim(Lines[I]);
        if Value = '' then Continue;
        if Value[1] <> '{' then Exit(E_INVALIDARG);
        P := Pos('}',Value);
        if (P < 2) or not TryStrToInt64(Copy(Value,2,P-2),FirstFrame) then Exit(E_INVALIDARG);
        Delete(Value,1,P);
        if (Value = '') or (Value[1] <> '{') then Exit(E_INVALIDARG);
        P := Pos('}',Value);
        if (P < 2) or not TryStrToInt64(Copy(Value,2,P-2),LastFrame) then Exit(E_INVALIDARG);
        Payload := Copy(Value,P+1,MaxInt);
        if (Length(ap_SubTitleTracks)=0) and (FirstFrame=1) and (LastFrame=1) then
          begin
            if not TryStrToFloat(Payload,FrameRate,Settings) or (FrameRate<1) or (FrameRate>240) then Exit(E_INVALIDARG);
            Continue;
          end;
        if (FirstFrame<0) or (LastFrame<FirstFrame) then Exit(E_INVALIDARG);
        N := Length(ap_SubTitleTracks);
        SetLength(ap_SubTitleTracks,N+1);
        ap_SubTitleTracks[N].stIndex := N+1;
        ap_SubTitleTracks[N].Start := Round(FirstFrame*1000.0/FrameRate);
        ap_SubTitleTracks[N].Stop := Round(LastFrame*1000.0/FrameRate);
        ap_SubTitleTracks[N].Duration := ap_SubTitleTracks[N].Stop-ap_SubTitleTracks[N].Start;
        J := 0;
        repeat
          P := Pos('|',Payload);
          if P=0 then begin TextLine := Payload;Payload := '';end
          else begin TextLine := Copy(Payload,1,P-1);Delete(Payload,1,P);end;
          SetLength(ap_SubTitleTracks[N].TrackText,J+1);
          ProcessSrtTags(TextLine,N,J);
          Inc(J);
        until P=0;
      end;
    if Length(ap_SubTitleTracks)=0 then Result := E_INVALIDARG else Result := S_OK;
  finally
    Lines.Free;
  end;
end;


// Clears the whole SubTitle array
procedure TMfTimedText.Clear();
var I,J: Integer;
begin
  if Assigned(lp_MatchList) then lp_MatchList.Clear();
  for I := 0 to High(ap_SubTitleTracks) do
    for J := 0 to High(ap_SubTitleTracks[I].TrackText) do
      FreeAndNil(ap_SubTitleTracks[I].TrackText[J].TextFont);
  SetLength(ap_SubTitleTracks,0);
  tp_SubTitleTrack := Default(TSubTitleTrack);
  ip_Tracks := 0;
  ip_TickIndex := 0;
end;


procedure TMfTimedText.SetAutoFormatText(aValue: Boolean);
begin
  bp_AutoFormatText := aValue;
end;


procedure TMfTimedText.SetDefaultTextColor(aValue: TColor);
begin
  cp_clDefaultTextColor := aValue;
end;


procedure TMfTimedText.SetPreferredLanguage(aValue: string);
var
  hr: HResult;

begin
  if (sp_PreferredLanguage <> aValue) then
    begin
      sp_PreferredLanguage := aValue;
      // Language has been changed: Set the language and load the corresponding file.
      hr := OpenTimedTextFile(sp_Url);
      if FAILED(hr) then
        sp_PreferredLanguage := '!na';
    end;
end;


function TMfTimedText.GetClosestStartTime(search: MFTIME): MFTIME;
var
  I: Integer;
  distance, minDistance: MFTIME;

begin
  Result := 0;
  minDistance := High(MFTIME);

  for I := Low(ap_SubTitleTracks) to High(ap_SubTitleTracks) do
    begin
      distance := Abs(search - ap_SubTitleTracks[I].Start);
      if (distance < minDistance) then
        begin
          minDistance := distance;
          // read a track and add to property
          tp_SubTitleTrack := ap_SubTitleTracks[I];
          Result := ap_SubTitleTracks[I].Start;
          if (minDistance = 0) then
            Break;
        end;
    end;
end;


procedure TMfTimedText.SetTrackIndex(aValue: Integer);
begin
  if (aValue <> ip_TickIndex) then
    ip_TickIndex := aValue;
end;


procedure TMfTimedText.SetTimedTextFile(aValue: WideString);
begin
  sp_Url := aValue;
end;


// Sender did send a message, we need a new track
function TMfTimedText.SendSubTitleText(Index: Integer): Integer;
begin

try
  // store into the global index
  TrackIndex := Index;

  if (Index >= 0) and (Index < ip_Tracks) then
    begin
      // read a track and add to property
      tp_SubTitleTrack := ap_SubTitleTracks[Index];
      m_bTimedTextNotifyProcessed := False;

      // Send a message to the owner: A new track is ready.
      SendMessage(hp_HwSender,
                  WM_TIMEDTEXTNOTIFY_UPDATE,
                  WPARAM(1), // 1 = track ready to read
                  LPARAM(TrackIndex));
      Result := Index;
    end
  else
    Result := -1; // End tag processing

except
  Result := -10;
end;
end;


// Helper to get a valid colorvalue
function TMfTimedText.StrToColor(sValue: string): TColor;
var
  liColor: LongInt;

begin
  if not IdentToColor(sValue, liColor) then
    begin
      if TryStrToInt(sValue, liColor) then
        Result:= TColor(liColor)
      else
        Result:= cp_clDefaultTextColor;  // if no valid color found, use the default
    end
  else
    Result:= TColor(liColor);
end;


// SRT (SubRip) supports the following tags:
//  SRTTAG_ITALIC_START = '<i>' or {i};
//  SRTTAG_ITALIC_END = '</i>' or {/i};
//  SRTTAG_BOLD_START = '<b>' or {/i};
//  SRTTAG_BOLD_END = '</b>' or {/i};
//  SRTTAG_UNDERLINE_START = '<u>' or {/i};
//  SRTTAG_UNDERLINE_END = '</u>' or {/i};
//  SRTTAG_COLOR_START = '<font color=';
//  SRTTAG_COLOR_END = '</font>';
procedure TMfTimedText.ProcessSrtTags(fText: string;
                                      trackIndex: integer;
                                      txtLine: integer);
var
  sTmp: string;
  sColor: string;
  iTest: Integer;

begin
  // Create the font, this can be nil for default or a predefined font (like fTrackFont)
  ap_SubTitleTracks[trackIndex].TrackText[txtLine].Create(fTrackFont);

  if AutoFormatText then
    begin
     // Italic
     if pos(SRTTAG_ITALIC_START, AnsiLowerCase(fText)) > 0 then
       begin
         ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style + [fsItalic];
       end;

    // Bold
    if pos(SRTTAG_BOLD_START, AnsiLowerCase(fText)) > 0 then
      begin
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style + [fsBold];
      end;

    // Underline
    if pos(SRTTAG_UNDERLINE_START, AnsiLowerCase(fText)) > 0 then
      begin
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style + [fsUnderline];
      end;

    // Color
    if pos(SRTTAG_COLOR_START, AnsiLowerCase(fText)) > 0 then
      begin
        sColor := Trim(copy(fText,
                       pos(SRTTAG_COLOR_START, fText) + Length(SRTTAG_COLOR_START),
                       Length(fText))); //the length of the color information is 8 ex '$' or '#'
        sColor := Trim(copy(sColor, 0, pos('">', sColor)-1));

        // Check if we have to deal with text, decimal or hex.

        if (Pos(sColor, '#') > 0) then // Hex?
          sColor := '$' + copy(sColor, 1, Length(sColor))
        else  // integer?
          if TryStrToInt(sColor, iTest) then
            sColor := '$' + IntToStr(iTest)
          else // text?
            if (Pos(sColor, '#') = 0) then
              sColor := 'cl' + sColor
            else // not valid?
              sColor := ColorToString(cp_clDefaultTextColor);

        fTrackFont.Color := StrToColor(sColor);
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Color := fTrackFont.Color;
      end;  // color
  end; // AutoFormat

  // strip tags from textline
  sTmp := fText;
  DeleteSrtFontTags(sTmp);
  ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextLine := sTmp;
end;


procedure TMfTimedText.DeleteSrtFontTags(var fText: string);

  function CleanUp(sTag: string): string;
    begin
      Result := StringReplace(fText,
                              sTag,
                              '',
                              [rfReplaceAll, rfIgnoreCase]);
    end;

begin
   fText := CleanUp(SRTTAG_ITALIC_START);
   fText := CleanUp(SRTTAG_ITALIC_END);
   fText := CleanUp(SRTTAG_BOLD_START);
   fText := CleanUp(SRTTAG_BOLD_START);
   fText := CleanUp(SRTTAG_BOLD_END);
   fText := CleanUp(SRTTAG_UNDERLINE_START);
   fText := CleanUp(SRTTAG_UNDERLINE_END);
   fText := CleanUp(SRTTAG_COLOR_START);
   fText := CleanUp(SRTTAG_COLOR_END);
   fText := CleanUp(SRTTAG_COLOR_EQOUTE);
end;


//
procedure TMfTimedText.ProcessVttTags(fText: string;
                                      trackIndex: integer;
                                      txtLine: integer);
var
  sTmp: string;

begin
  // Create the font, this can be nil for default or a predefined font (like fTrackFont)
  ap_SubTitleTracks[trackIndex].TrackText[txtLine].Create(fTrackFont);

  if AutoFormatText then
    begin
     // Italic
     if pos(WVTT_ITALIC_START, AnsiLowerCase(fText)) > 0 then
       begin
         ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style + [fsItalic];
       end;

    // Bold
    if pos(WVTT_BOLD_START, AnsiLowerCase(fText)) > 0 then
      begin
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style + [fsBold];
      end;

    // Underline
    if pos(WVTT_UNDERLINE_START, AnsiLowerCase(fText)) > 0 then
      begin
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style + [fsUnderline];
      end;

    // Color
    // The text anf text background color is not yet implemented, so, we keep it White text and transparent background.
    fTrackFont.Color := clWhite;
    ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Color := fTrackFont.Color;

  end; // AutoFormat

  // strip tags from textline
  sTmp := fText;
  DeleteVttFontTags(sTmp);
  ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextLine := sTmp;
end;


//
procedure TMfTimedText.DeleteVttFontTags(var fText: string);

  function CleanUp(sTag: string): string;
    begin
      Result := StringReplace(fText,
                              sTag,
                              '',
                              [rfReplaceAll, rfIgnoreCase]);
    end;

begin
   fText := CleanUp(WVTT_ITALIC_START);
   fText := CleanUp(WVTT_ITALIC_END);
   fText := CleanUp(WVTT_BOLD_START);
   fText := CleanUp(WVTT_BOLD_START);
   fText := CleanUp(WVTT_BOLD_END);
   fText := CleanUp(WVTT_UNDERLINE_START);
   fText := CleanUp(WVTT_UNDERLINE_END);
   fText := CleanUp(WVTT_RUBY_START);
   fText := CleanUp(WVTT_RUBY_END);
   // Text and text background colors are not implemented (yet).
end;


constructor TFormattedText.Create(aFont: TFont);
begin
  if (aFont = Nil) then
    begin
      TextFont := TFont.Create();
      with TextFont do
        begin
          Name        := 'Verdana';
          Charset     := DEFAULT_CHARSET;
          Orientation := 0;
          Size        := 12;
          Orientation := 0;
          Quality     := fqDefault;
          Pitch       := fpDefault;
          Height      := -19;
          Color       := clWhite;
          Style       := [fsBold];
       end;
    end
  else
    begin
      TextFont := TFont.Create();
      TextFont.Assign(aFont);
    end;
end;

procedure TSubTitleTrack.Init();
begin
  SetLength(TrackText, 0);
end;

procedure TSubTitleTrack.Clear();
begin
  SetLength(TrackText, 0);
  Finalize(TrackText);
  TrackText := nil;
end;

end.

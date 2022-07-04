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
// Version: 3.1.0
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
// 28/06/2022 All                 Mercury release  SDK 10.0.22621.0 (Windows 11)
// ----------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX312
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit TimedTextClass;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
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
    lp_MatchList                : TStringlist;                  // Global matchlist, stores tracktimes
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

    // Process font tags and store to list
    procedure ProcessSrtTags(fText: string;
                             trackIndex: integer;
                             txtLine: integer);

    procedure DeleteFontTags(var fText: string);

    // Write .srt file in to memory
    function ReadSubRipFile(const sUrl: WideString): HResult;

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


// searches for srt or implemented formats and opens them by corresponding language
function TMfTimedText.OpenTimedTextFile(const sUrl: WideString): HResult;
var
  hr: HResult;
  Path, UrlFileName: WideString;
  I: Integer;
  bFound: Boolean;

label
  Done,
  srt,
  sub;

begin
  hr := S_OK;
  sp_Url := '';
  tp_FileType := NONE;
  bFound := False;
  I := 0;

  // check if Url has filename
  if (sUrl = '') then
    begin
      hr := ERROR_INVALID_PARAMETER;
      goto Done;
    end;

  // Check if the file exists
  if Not FileExists(sUrl) then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      goto Done;
    end;

  UrlFileName := ExtractFilename(sUrl);
  Path := IncludeTrailingPathDelimiter(ExtractFileDir(sUrl));



  // Reset array to 0
  SetLength(pc_LanguageTags.TimedTxtPropsArray,
            0);

  // Try to find SubRip (.srt) files  TODO:  set the SetPreferredLanguage ?
  pc_LanguageTags.TimedTxtPropsArray := pc_LanguageTags.ReadFileTags(Path + UrlFileName,
                                                                     PreferredLanguage,
                                                                     0,
                                                                     EXTSUBRIP);
  if (Length(pc_LanguageTags.TimedTxtPropsArray) > 0) then
    goto srt;



  // Try to find MicroDvd (.sub) files  TODO:  set the SetPreferredLanguage ?
  pc_LanguageTags.TimedTxtPropsArray := pc_LanguageTags.ReadFileTags(Path + UrlFileName,
                                                                     PreferredLanguage,
                                                                     0,
                                                                     EXTMICRODVD);
  if (Length(pc_LanguageTags.TimedTxtPropsArray) > 0) then
    goto sub;


  // Implement other supported formats here ////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////



  // If we end up here, nothing is found : exit

  if (Length(pc_LanguageTags.TimedTxtPropsArray) = 0) then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      goto Done;
    end;

// from here we try to get the language of the .srt (SubRip) file
srt:

  // NOTE: It's possible that tags will contain mymovie.srt and - for example - mymovie[en_US].srt are returning.
  // In that case mymovie[en_US].srt will be the preferred subtitle file.
  if (Length(pc_LanguageTags.TimedTxtPropsArray) = 1) then
    begin
      hr := ReadSubRipFile(LPCWSTR(pc_LanguageTags.TimedTxtPropsArray[0].sFile));
      // Found, store url and extension
      if SUCCEEDED(hr) then
        bFound := True;
    end
  else
    begin
      // compare, ignoring case
      for I := 0 to High(pc_LanguageTags.TimedTxtPropsArray) do
        begin
          if IsMatch(PreferredLanguage + '_[A-Z][A-Z]',
                     pc_LanguageTags.TimedTxtPropsArray[I].sFile) then
            begin
              hr := ReadSubRipFile(LPCWSTR(pc_LanguageTags.TimedTxtPropsArray[I].sFile));

              // Found, store url and extension
              if SUCCEEDED(hr) then
                begin
                  bFound := True;
                  Break;
               end;
            end;
        end;
    end;
  goto Done;

// from here we try to get the language of the .sub (MicroDvd) file
sub:

  // NOTE: It's possible that tags will contain mymovie.sub and - for example - mymovie[en_US].sub are returning.
  // In that case mymovie[en_US].sub will be the preferred subtitle file.
  if (Length(pc_LanguageTags.TimedTxtPropsArray) = 1) then
    begin
      hr := ReadMicroDvdFile(LPCWSTR(pc_LanguageTags.TimedTxtPropsArray[0].sFile));
      // Found, store url and extension
      if SUCCEEDED(hr) then
        bFound := True;
    end
  else
    begin
      // compare, ignoring case
      for I := 0 to High(pc_LanguageTags.TimedTxtPropsArray) do
        begin
          if IsMatch(PreferredLanguage + '_[A-Z][A-Z]',
                     pc_LanguageTags.TimedTxtPropsArray[I].sFile) then
            begin
              hr := ReadMicroDvdFile(LPCWSTR(pc_LanguageTags.TimedTxtPropsArray[I].sFile));

              // Found, store url and extension
              if SUCCEEDED(hr) then
                begin
                  bFound := True;
                  Break;
               end;
            end;
        end;
    end;
  goto Done;

Done:
  // store   goto Done;
  if bFound then
    begin
      sp_Url := pc_LanguageTags.TimedTxtPropsArray[I].sFile;
      tp_FileType := pc_LanguageTags.TimedTxtPropsArray[I].sTTxtType;
      sp_FriendlyLangName := pc_LanguageTags.TimedTxtPropsArray[I].sFriendlyLanguageName
    end;
  Result := hr;
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

try
try

  // Remove empty lines if more than 1 and/or insert a track marker
  for I := flTemp.Count - 1 downto 0 do
    begin
      if (flTemp.Strings[I] = Trim('')) and (bPrev = False) then
        begin
          flTemp.Strings[I] := '<E_T>';  // insert a track end tag
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
            iMatches := GetMatchCount('[0-9][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]',
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
              while (Pos('<E_T>', flTemp.Strings[I]) = 0) do
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


function TMfTimedText.ReadMicroDvdFile(const sUrl: WideString): HResult;
var
  hr: HResult;
  bPrev: Boolean;
  I,
  aSize,
  iMatches: Integer;
  flTemp: TStrings;
  pwInt: PWideChar;

begin
  hr := S_OK;
  bPrev := False;
  flTemp := TStringList.Create();
  flTemp.LoadFromFile(sUrl);

try
try

  // Remove empty lines if more than 1 and/or insert a track marker
  for I := flTemp.Count - 1 downto 0 do
    begin
      if (flTemp.Strings[I] = Trim('')) and (bPrev = False) then
        begin
          flTemp.Strings[I] := '<E_T>';  // insert a track end tag
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
      // Get the start and stop frame from this format {start-frame}{stop-frame}
      // This regex expression should always return 2 matches.
      iMatches := GetMatchCount('{\K[-+]?[0-9]*',
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
      else // The file or format is corrupted
        begin
          hr := E_INVALIDARG;
          Break;
        end;
      lp_MatchList.Clear();
    end;

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
      if lp_MatchList <> Nil then
        lp_MatchList.Clear();
    end;
  Result := hr;
end;
end;


// Clears the whole SubTitle array
procedure TMfTimedText.Clear();
begin
  if Assigned(lp_MatchList) then
    begin
      lp_MatchList.Clear;
      lp_MatchList.Free;
    end;
  if Assigned(fTimedText) then
     fTimedText.Clear();

  SetLength(ap_SubTitleTracks, 0);
  Finalize(ap_SubTitleTracks);
  ap_SubTitleTracks := nil;
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

  if (Index <= ip_Tracks) then
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
  // Create the font, this can be Nil for default or a predefined font (like fTrackFont)
  ap_SubTitleTracks[trackIndex].TrackText[txtLine].Create(fTrackFont);

  if AutoFormatText then
    begin
     // Italic
     if pos(SRTTAG_ITALIC_START, AnsiLowerCase(fText)) > 0 then
       begin
         fTrackFont.Style := fTrackFont.Style + [fsItalic];
         ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := fTrackFont.Style;
       end;

    // Bold
    if pos(SRTTAG_BOLD_START, AnsiLowerCase(fText)) > 0 then
      begin
        fTrackFont.Style := fTrackFont.Style + [fsBold];
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := fTrackFont.Style;
      end;

    // Underline
    if pos(SRTTAG_UNDERLINE_START, AnsiLowerCase(fText)) > 0 then
      begin
        fTrackFont.Style := fTrackFont.Style + [fsUnderline];
        ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextFont.Style := fTrackFont.Style;
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
  DeleteFontTags(sTmp);
  ap_SubTitleTracks[trackIndex].TrackText[txtLine].TextLine := sTmp;
end;


procedure TMfTimedText.DeleteFontTags(var fText: string);

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
    TextFont := aFont;
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

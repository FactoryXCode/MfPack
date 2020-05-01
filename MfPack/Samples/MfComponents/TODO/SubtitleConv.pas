// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: SubtitleConv.pas
// Kind: Pascal Unit
// Release date: 02-02-2002
// Language: ENU
//
// Version: 2.6.3
// Description: SRT and SMIL/SAMI parser/converter component.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships).
//
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 28/05/2019                     WIN10 May 2019 update (version 1903)
// 03/06/2019                     ISÁK release.
// 18/06/2019                     Prodigy release.
// 13/08/2019                     Leftfield release.
// 15/09/2019                     Renamed the unit MfMethods to MfpMetLib.pas and added more helpers.
// 24/12/2019                     Underworld release.
// ----------------------------------------------------------------------------
//
// Remarks: For use with Windows SDK 10.0.17763.0 and earlier.
//
// Related objects: MfPack Samples >= 2.6.3
// Related projects: >= MfPackX263
// Known Issues: -
// Compiler version: 23 up to 33
// Todo: -
// SDK version: 10.0.18362.0 (19H1)
// =============================================================================
// Source: -
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit SubtitleConv;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ShlObj,
  {Vcl}
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  {System}
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.DateUtils,
  System.StrUtils;



type
  TModes = (mpInit, mpRead, mpClear);
  TFileType = (mpSMI, mpSRT, mpError);

  TSubTitleRec = record
    stIndex: Int64;
    StartTime: Int64;
    StopTime: Int64;
    SubTitleLine: array of string;
    FontTagStart: TFont;
    FontTagEnd: TFont;
  end;


const
  WTP               = ' --> ';   // srt time separator

  TFLENGTH          = 8;
  LFEED             = #13;
  ULBR              = #13#10;
  LINEBR            = '<BR-';
  SMI_BREAK         = '<BR>';
  SMI_S_BODY        = '<BODY>';
  SMI_E_BODY        = '</BODY>';
  SMI_S_SYNC        = '<SYNC START='; //formal parameter
  SMI_ST_SYNC       = '<SYNCSTART='; //read parameter
  SMI_PAR           = '<P';
  SMI_BLANK         = '>' + SMI_BREAK; //'><br>';
  SMI_BLANK2        = '>&NBSP';  //alternative blank line code
  SMI_ClASS         = '<P Class=';
  SMI_CLASS_UNK     = 'UNKNOWNCC';
  SMI_CLASS_SUBT    = 'SUBTTL';
  SMI_BEGIN         = '<SAMI>';
  SMI_END           = '</SAMI>';
  SMI_S_PARAM       = '<SAMIParam>';
  SMI_E_PARAM       = '</SAMIParam>';
  SMI_STYLE         = SMI_S_SYNC + '0' + SMI_ClASS + 'SUBTTL' + '>';

  CLASS_STYLE_1     = '<PCLASS=UNKNOWNCC>'; //read parameter
  CLASS_STYLE_2     = '<PCLASS=SUBTTL>';    //read parameter

  SMI_HEADER =
    SMI_BEGIN + ULBR + '<HEAD>' + ULBR + '<TITLE>' + '</TITLE>' + ULBR +
    '<SAMIParam>' + ULBR + '<!--' + ULBR + 'Generator: "MfPack";' +
    ULBR + 'GeneratorURL: "http://sourceforge.net/projects/MfPack";' +  ULBR + 'GeneratorVersion: "3.3.9" ' +
    '-->' + ULBR + '</SAMIParam>' + ULBR + '<Style TYPE="text/css">' + ULBR +
    '<!-- P {margin-left: 29pt; margin-right: 29pt; font-size: 14pt;' +
    'text-align: center; font-family: tahoma, arial, sans-serif;' +
    'font-weight: bold; color: white; background-color: black;}' +
    '.SUBTTL {Name: ''Subtitles''; SAMIType: CC;}-->' +
    ULBR + '</Style>' + ULBR +
    '</HEAD>' + ULBR + SMI_S_BODY + ULBR + ULBR;


  SMI_FOOTER = ULBR + ULBR + SMI_E_BODY + ULBR + SMI_END;
  SUBT_EXT_SRT =    '.srt';     // Subtitle file extension SubRip
  SUBT_EXT_SAMI =   '.smi';     // Subtitle file extension SMIL

  // SRT tags
  // SRT (SubRip) supports the following tags
  SRTTAG_ITALIC_START = '<i>';
  SRTTAG_ITALIC_END = '</i>';
  SRTTAG_BOLD_START = '<b>';
  SRTTAG_BOLD_END = '</b>';
  SRTTAG_UNDERLINE_START = '<u>';
  SRTTAG_UNDERLINE_END = '</u>';
  SRTTAG_COLOR_START = '<font color="';
  SRTTAG_COLOR_END = '</font>';


resourcestring
  //Add language dependent constants
  {$INCLUDE SubtitleConv-lang-eng.inc}

const
  DEFAULT_TIMER_INTERVAL = 100; // 100 msec


type
  TSubtitleTimer = class;

  //
  TMfpTimerThread = class(TThread)
  private
    { Private declarations }
    FTimer: TSubtitleTimer;

  protected
    { Protected declarations }
    procedure DoTimer();

  public
    { Public declarations }
    constructor Create(ATimer: TSubtitleTimer);
    destructor Destroy(); override;

    procedure Execute(); override;
  end;


  // Subtitletimer
  TSubtitleTimer = class(TComponent)
  private
    { Private declarations }
    bActivated    : Boolean;
    iResolution   : Int64;
    FOnTimer      : TNotifyEvent;

    procedure SetResolution(const Value: Int64);


  protected
    { Protected declarations }
    FTimerThread: TMfpTimerThread;
    bNeedNewText: Boolean;
    procedure UpdateTimer();

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start();
    procedure Stop();

  published
    { Published declarations}

    property Activated: Boolean read bActivated;
    property Resolution: Int64 read iResolution write SetResolution default DEFAULT_TIMER_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;

  end;



type
  TSubTitle = class(TSubtitleTimer)
  private
    { Private declarations }
    SubTitleList          : array of TSubTitleRec;
    Modes                 : TModes;
    FileType              : TFileType;
    SubTitleFile          : textfile;

    iSubTitleIndex        : Int64;
    fTextFormat           : TFont;
    bAutoFormatText       : Boolean;
    clDefaultTextColor    : TColor;
    
  protected
    { Protected declarations }
    function TimeToMilliseconds(sTime: string): int64;

    function GetSMIText(_Text: string;
                        _dummy: string): string;

    function ExtractSMITimeCode(_Str: string): string;
    function DelSpaces(_Str: string): string;

    function ProcessTags(sValue: string;
                         iIndex: integer): string;

    procedure GetContents(_FileType: TFileType);
    function GetSubTitleIndex(): int64;
    procedure SetSubTitleIndex(value: int64);
    procedure SetNeedNewText(value: Boolean);
    function GetSubTitle(_find: int64): Boolean;
    procedure SetTextFormat(fFont: TFont);
    procedure SetAutoFormatText(bval: Boolean);
    procedure SetDefaultTextColor(clClr: TColor);
    // Helper to get a valid colorvalue from a string
    function StrToColor(sval: string): TColor;

  public
    { Public declarations }
    SubTitleFileName      : WideString;
    SubTitleRec           : TSubTitleRec;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Translators
    procedure TranslateSUBtoSMI(wSRTfile: WideString;
                                DestDir: WideString;
                                wFPS: integer);

    procedure TranslateSRTtoSMI(wSRTfile: WideString;
                                DestDir: WideString);

  published
    { Published declarations }

    procedure ResetIndex();
    function ReadSubTitleFile(Runstate: TModes): TFileType;
    function GetNewSubTitle(_AtTime: int64): int64;


    property Mode: TModes read Modes;
    property NeedNewText: boolean read bNeedNewText write SetNeedNewText default true;
    property SubTitleFileType: TFileType read FileType;
    property SubTitleIndex: int64 read GetSubTitleIndex write SetSubTitleIndex default 0;
    property AutoFormatText: Boolean read bAutoFormatText write SetAutoFormatText default False;
    property TextFormat: TFont read fTextFormat write SetTextFormat;
    property DefaultTextColor: TColor read clDefaultTextColor write SetDefaultTextColor default clWhite;
  end;

procedure Register;

implementation


// TSUBTITLE
procedure TSubTitle.TranslateSUBtoSMI(wSRTfile: WideString;
                                      DestDir: WideString;
                                      wFPS: integer);
var
   SMIfileName: WideString;
   strTmp,
   wTimeStartTmp,
   wTimeStopTmp,
   wSubTmp: string;
   _i, _c: Byte;
   SMIfile, SubTitleFile: Textfile;

begin
   try
   //Open SRT file for reading

   AssignFile(SubTitleFile, wSRTfile);
   Reset(SubTitleFile);
   //Open SMI file for creation and writing
   if DestDir = '' then
    SMIfileName := copy(wSRTfile, 1, length(wSRTfile) - 4) + SUBT_EXT_SAMI
   else
    begin
      strTmp := DestDir + '\' + ExtractFileName(wSRTfile);
      SMIfileName := copy(strTmp, 1, length(strTmp) - 4) + SUBT_EXT_SAMI;
    end;

   AssignFile(SMIfile, SMIfileName);
   Rewrite(SMIfile);
   //files are open now for read/write

   //write the initiating header to the SMIfile
   writeln(SMIfile, SMI_HEADER);

   //Read the MicroDvdfile and write the body contents to the SMIfile
   repeat
      _i := 0;
      _c := 0;
      readln(SubTitleFile, strTmp);
      if (Pos('}{', strTmp) > 0) then  // Find the ' }{ ' first
         begin
            wTimeStartTmp := copy(strTmp, 2, Pos('}{', strTmp) - 2);
            wTimeStopTmp := copy(strTmp, Pos('}{', strTmp) + 2, Length(wTimeStartTmp));

            wTimeStartTmp := IntToStr((StrToInt(wTimeStartTmp) * 1000) div wFPS);
            wTimeStopTmp := IntToStr((StrToInt(wTimeStopTmp) * 1000) div wFPS);

            repeat
               inc(_c);
               if copy(strTmp,_c,1) = '}' then inc(_i);
            until (_i = 2);
            wSubTmp := copy(strTmp, _c + 1, 255);
            _i := 0;
            repeat  //replace the '|'
              inc(_i);
              if Pos('|', wSubTmp)>0 then
               wSubTmp := StuffString(wSubTmp, Pos('|', wSubTmp), 1, SMI_BREAK);
            until (Pos('|', wSubTmp) = 0) or ( _i > 3); //Add a counter (i_) in case something goes wrong.

            writeln(SMIfile, SMI_S_SYNC + wTimeStartTmp + '>' + SMI_PAR + wSubTmp);
            writeln(SMIfile, SMI_S_SYNC + wTimeStopTmp + '>' + SMI_BREAK);

         end;
   until (Eof(SubTitleFile)= True);


   // Finally write the SMIfile footer
   writeln(SMIfile, SMI_FOOTER);

   // Close files when done
   CloseFile(SubTitleFile);
   CloseFile(SMIfile);
   except
     on exception do
      if Application.Messagebox(PWideChar(MSG16),
                                PWideChar(MSG19),
                                MB_OK + MB_ICONERROR) = IDOK then
                                Exit;
   end;
    if Application.MessageBox(PWideChar(MSG20),
                              PWideChar(MSG21 + #13 + MSG21A + #13 + SMIfileName),
                              MB_OK + MB_ICONINFORMATION) = IDOK then
                              Exit;
end;

//-------------------------------------------------------------------------

procedure TSubTitle.TranslateSRTtoSMI(wSRTfile: WideString;
                                      DestDir: WideString);
var
   strTmp, SMIfileName: WideString;
   wTimeStartTmp, wTimeStopTmp: Int64;
   wSubTmp: string;
   _i: Byte;
   SMIfile, SubTitleFile: Textfile;

begin
   try
   //Open SRT file for reading
   AssignFile(SubTitleFile, wSRTfile);
   Reset(SubTitleFile);

   //Open SMI file for creation and writing
   if DestDir = '' then
    SMIfileName := copy(wSRTfile, 1,length(wSRTfile) - 4) + SUBT_EXT_SAMI
   else
    begin
      strTmp := DestDir + '\' + ExtractFileName(wSRTfile);
      SMIfileName := copy(strTmp, 1, length(strTmp) - 4) + SUBT_EXT_SAMI;
    end;

   AssignFile(SMIfile, SMIfileName);
   Rewrite(SMIfile);
   //files are open now for read/write

   //write the initiating header to the SMIfile
   writeln(SMIfile, SMI_HEADER);

   //Read the SRTfile and write the body contents to the SMIfile
   repeat
      _i := 0;
      readln(SubTitleFile, strTmp);
      if (Pos(WTP, AnsiUpperCase(strTmp)) > 0) then  // Find the ' --> ' first
         begin
            wTimeStartTmp := TimeToMilliseconds(copy(strTmp, 2, 11));
            wTimeStopTmp := TimeToMilliseconds(copy(strTmp, 19, 11));

            repeat
               readln(SubTitleFile, strTmp);
               _i := _i + 1;
               If (_i = 1) and (strTmp <> '') then wSubTmp := strTmp;
               If (_i > 1) and (strTmp <> '') then wSubTmp := wSubTmp + SMI_BREAK + strTmp;
            until (strTmp= '');

            writeln(SMIfile, SMI_S_SYNC + IntToStr(wTimeStartTmp) + '>' + SMI_ClASS + SMI_CLASS_SUBT + '>' + wSubTmp + ULBR);
            writeln(SMIfile, SMI_S_SYNC + IntToStr(wTimeStopTmp) + '>' + SMI_BLANK + ULBR);

         end;
   until (Eof(SubTitleFile)= True);


   //Finally write the SMIfile footer
   writeln(SMIfile, SMI_FOOTER);

   // Close files when done
   CloseFile(SubTitleFile);
   CloseFile(SMIfile);
   except
      on exception do
        if Application.MessageBox(PWideChar(MSG19),
                                  PWideChar(MSG16),
                                  MB_OK + MB_ICONERROR) = IDOK then
                                  Exit;
   end;
      if Application.MessageBox(PWideChar(MSG21 + #13 + MSG21A + #13 + SMIfileName),
                                PWideChar(MSG20),
                                MB_OK + MB_ICONINFORMATION) = IDOK then
                                Exit;
end;

//-------------------------------------------------------------------------

function TSubTitle.TimeToMilliseconds(sTime: string): int64;
var
  wcTime: TTime;
  wMSecs: Int64;
  RemMs: Int64;
  HMSTime: string;

begin
  if pos(',', sTime) > 0 then
    begin
      HMSTime := copy(sTime, 1, pos(',', sTime) - 1);
      RemMs := StrToInt(copy(sTime, pos(',', sTime) + 1, 3));
    end
  else
    begin
      HMSTime := sTime;
      RemMs := 0;
    end;

   try

      wcTime := StrToTime(HMSTime);
      wMSecs := MilliSecondOfTheDay(wcTime) + RemMs;
      Result := wMSecs;
   except
      Result := 0;
   end;
end;

//-------------------------------------------------------------------------

function TSubTitle.GetSMIText(_Text: string ; _dummy: string): string;
var
  _dum: string;

begin
  _dum := UpperCase(_dummy);
  // Get text
  if Pos(SMI_PAR + '>', _dum) > 0 then
    result := copy(_Text, Pos(SMI_PAR + '>', _Text) + 3, Length(_Text));
  if Pos(CLASS_STYLE_1, _dum) > 0 then
    result := copy(_Text, Pos(RightStr(CLASS_STYLE_1, 4), _Text) + 4, Length(_Text));
  if Pos(CLASS_STYLE_2, _dum) > 0 then
    result := copy(_Text, Pos(RightStr(CLASS_STYLE_2, 4), _Text) + 4, Length(_Text));
  // add extra break
  result := result + SMI_BREAK;
end;


function TSubTitle.ReadSubTitleFile(Runstate: TModes): TFileType;
var
  _Tmp: String;

begin
  Result := mpError;

  case Runstate of

    mpClear:
      begin
        try
          SubTitleList := Nil;
        except
          Result := mpError;
          Exit;
        end;
      end;

    mpInit:
      begin
        AssignFile(SubTitleFile, SubTitleFileName);
        Reset(SubTitleFile);
        // Check for valid contents of the SAMI file
        if lowercase(ExtractFileExt(SubTitleFileName))= '.smi' then
          begin
            repeat
              readln(SubTitleFile, _Tmp);
              if (Pos(SMI_BEGIN, AnsiUpperCase(_Tmp)) > 0) then
                Result := mpSMI;
            until (Pos(SMI_S_BODY, AnsiUpperCase(_Tmp)) > 0) or (Eof(SubTitleFile));
            // Error message: wrong file or no contents
            if (Eof(SubTitleFile)= True) then
              if Application.MessageBox(PWideChar(WRNXX),
                                        PWideChar(ERR10),
                                        MB_OK + MB_ICONERROR) = IDOK then
                begin
                  CloseFile(SubTitleFile);
                  //SubTRunning := mpNo;
                  Result := mpError;
                  exit;
                end;
          end
        else
          Result := mpSRT;
        // now fill the strings
        GetContents(Result);
      end;
  end;
end;


// Helper to get a valid colorvalue
function TSubTitle.StrToColor(sval: string): TColor;
var
  liColor: LongInt;

begin
  if not IdentToColor(sval, liColor) then
    begin
      if TryStrToInt(sval, liColor) then
        Result := TColor(liColor)
      else
        Result := clDefaultTextColor;  // if no valid color found, use the default
    end
  else
    Result := TColor(liColor);
end;

// SRT (SubRip) supports the following tags:
//  SRTTAG_ITALIC_START = '<i>';
//  SRTTAG_ITALIC_END = '</i>';
//  SRTTAG_BOLD_START = '<b>';
//  SRTTAG_BOLD_END = '</b>';
//  SRTTAG_UNDERLINE_START = '<u>';
//  SRTTAG_UNDERLINE_END = '</u>';
//  SRTTAG_COLOR_START = '<font color=';
//  SRTTAG_COLOR_END = '</font>';
function TSubTitle.ProcessTags(sValue: string; iIndex: integer): string;
var
  _tmp: String;
  _Color: String;
  _iTest: Integer;
  
begin
  _tmp := sValue;

  // Italic
  if pos(SRTTAG_ITALIC_START, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(sValue, pos(SRTTAG_ITALIC_START, sValue) + 3, Length(sValue)));
      if AutoFormatText then
        begin
          fTextFormat.Style := fTextFormat.Style + [fsItalic];
          SubTitleList[iIndex].FontTagStart.Style := fTextFormat.Style;
        end;
    end;

  if pos(SRTTAG_ITALIC_END, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(_tmp, 0, pos(SRTTAG_ITALIC_END, _tmp) - 1));
      if AutoFormatText then
        begin
          fTextFormat.Style := fTextFormat.Style - [fsItalic];
          SubTitleList[iIndex].FontTagEnd.Style := fTextFormat.Style;
        end;
    end;

  // Bold
  if pos(SRTTAG_BOLD_START, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(sValue, pos(SRTTAG_BOLD_START, sValue) + 3, Length(sValue)));
      if AutoFormatText then
        begin
          fTextFormat.Style := fTextFormat.Style + [fsBold];
          SubTitleList[iIndex].FontTagStart.Style := fTextFormat.Style;
        end;
    end;

  if pos(SRTTAG_BOLD_END, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(_tmp, 0, pos(SRTTAG_BOLD_END, _tmp) - 1));
      if AutoFormatText then
        begin
          fTextFormat.Style := fTextFormat.Style - [fsBold];
          SubTitleList[iIndex].FontTagEnd.Style := fTextFormat.Style;
        end;
    end;

  // Underline
  if pos(SRTTAG_UNDERLINE_START, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(sValue, pos(SRTTAG_UNDERLINE_START, sValue) + 3, Length(sValue)));
      if AutoFormatText then
        begin
          fTextFormat.Style := fTextFormat.Style + [fsUnderline];
          SubTitleList[iIndex].FontTagStart.Style := fTextFormat.Style;
        end;
    end;

  if pos(SRTTAG_UNDERLINE_END, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(_tmp, 0, pos(SRTTAG_UNDERLINE_END, _tmp) - 1));
      if AutoFormatText then
        begin
          fTextFormat.Style := fTextFormat.Style - [fsUnderline];
          SubTitleList[iIndex].FontTagEnd.Style := fTextFormat.Style;
        end;
    end;

  // Color
  if pos(SRTTAG_COLOR_START, AnsiLowerCase(sValue)) > 0 then
    begin
      _Color := Trim(copy(sValue,
                         pos(SRTTAG_COLOR_START, sValue) + Length(SRTTAG_COLOR_START),
                         Length(sValue))); //the length of the color information is 8 ex '$' or '#'  
      _Color := Trim(copy(_Color, 0, pos('">', _Color)-1));

      // Check if we have to deal with text, decimal or hex.

      if (Pos(_Color, '#') > 0) then // Hex?
        _Color := '$' + copy(_Color, 1, Length(_Color))
      else  // integer?
        if TryStrToInt(_Color, _iTest) then
          _Color := '$' + IntToStr(_iTest)
        else // text?
          if (Pos(_Color, '#') = 0) then
            _Color := 'cl' + _Color
          else // not valid? 
            _Color := ColorToString(clDefaultTextColor);
        
      if AutoFormatText then
        begin
          fTextFormat.Color := StrToColor(_Color);
          SubTitleList[iIndex].FontTagStart.Color := fTextFormat.Color;
        end;
    end;

  if pos(SRTTAG_COLOR_END, AnsiLowerCase(sValue)) > 0 then
    begin
      _tmp := Trim(copy(_tmp, 0, pos(SRTTAG_COLOR_END, sValue) - 1));
      if AutoFormatText then
        begin
          fTextFormat.Color := clDefaultTextColor;
          SubTitleList[iIndex].FontTagEnd.Color := fTextFormat.Color;
        end;
    end;

  Result := _tmp;

end;


procedure TSubTitle.GetContents(_FileType: TFileType);
var
  _Tmp, _subtline, STStart, STStop, STText, STTestStr: string;
  EOSubt, _completed: boolean;
  _i, _j, _linenr: integer;
  _prevStop: int64;

begin
try
  _i := 1;
  SetLength(SubTitleList, _i);
  _prevStop := 0;

  case _FileType of

  mpSRT:
    begin
      repeat
        readln(SubTitleFile, _Tmp);
        if (Pos(WTP, AnsiUpperCase(_Tmp)) > 0) then
          begin
            // Get the start and stop time
            STStart := Trim(copy(_Tmp, 1, pos(WTP, AnsiUpperCase(_Tmp))));
            STStop := Trim(copy(_Tmp, pos('>', AnsiUpperCase(_Tmp)) + 1, Length(_Tmp)));
            SubTitleList[_i -1].StartTime := TimeToMilliseconds(STStart);
            SubTitleList[_i -1].StopTime := TimeToMilliseconds(STStop);

            // Init the fontproperties
            SubTitleList[_i -1].FontTagStart := TFont.Create;
            SubTitleList[_i -1].FontTagEnd := TFont.Create;
            SubTitleList[_i -1].FontTagStart.Assign(fTextFormat);
            SubTitleList[_i -1].FontTagEnd.Assign(fTextFormat);
            _linenr := 0;

            // Get the textlines
            repeat
              readln(SubTitleFile, _Tmp);
              if (_Tmp <> '') then
                begin
                  // Prepare the next one
                  inc(_linenr);
                  SetLength(SubTitleList[_i - 1].SubTitleLine, _linenr);
                  // Clear paragraph tags and set the fontproperties for each record.
                  // fTextFormat can be a custom or auto format
                  SubTitleList[_i - 1].SubTitleLine[_linenr - 1] := ProcessTags(_Tmp, _i - 1);
                end;
            until _Tmp = '';

            // Add an extra empty textline
            inc(_linenr);
            SetLength(SubTitleList[_i - 1].SubTitleLine, _linenr);
            // Add next record
            inc(_i);
            SetLength(SubTitleList, _i);
          end;
      until Eof(SubTitleFile)
    end;

//SMI FORMAT
  mpSMI:
    begin
      EOSubt := False;
      _completed := False;
      repeat
        STTestStr := '';
        _linenr := 1;
        SetLength(SubTitleList[_i - 1].SubTitleLine, _linenr);

        readln(SubTitleFile, _Tmp);
        if (_Tmp > '') then
          begin
            // Remove temp all spaces to prevent read errors while checking for parameters
            STTestStr := DelSpaces(_Tmp);

            if (Pos(SMI_E_BODY, AnsiUpperCase(STTestStr)) > 0) then
              EOSubt := True;

            // Check for '<SYNCSTART=' and '><P>' is not in same line : this contains only time
            if (Pos(SMI_ST_SYNC, AnsiUpperCase(STTestStr)) > 0) and
               (Pos(SMI_PAR, AnsiUpperCase(STTestStr)) > 0) then
              begin
                // Check for stop ( like <SYNC Start=195226><P><br> )
                if (Pos(SMI_BLANK, AnsiUpperCase(STTestStr)) > 0) Or
                   (Pos(SMI_BLANK2, AnsiUpperCase(STTestStr)) > 0) then
                  begin
                    STStop := ExtractSMITimeCode(STTestStr);
                    _completed := True;
                  end
                else // Trapped a text line
                  begin
                    STStart := ExtractSMITimeCode(STTestStr);
                    STText := GetSMIText(_Tmp, STTestStr); //Use the original readstring here !
                    _completed := False;
                  end;
              end
            else   // Alternative
              begin
                if (Pos(SMI_ST_SYNC, AnsiUpperCase(STTestStr)) > 0) then
                  if STStart = '' then
                    STStart := ExtractSMITimeCode(STTestStr)
                  else
                    STStop := ExtractSMITimeCode(STTestStr);

                if (Pos(SMI_BLANK, AnsiUpperCase(STTestStr)) > 0) Or
                   (Pos(SMI_BLANK2, AnsiUpperCase(STTestStr)) > 0) then
                  begin
                    STStop := STStop;
                    _completed := true;
                  end
                else
                  if (Pos(SMI_PAR, AnsiUpperCase(STTestStr)) > 0) then
                    STText := GetSMIText(_Tmp, STTestStr); // Use the original readstring here !
              end;

            if _completed then
              begin
                if _prevStop >= StrToInt(STStart) then
                  SubTitleList[_i -1].StartTime := StrToInt(STStart) + 100
                else
                  SubTitleList[_i-1].StartTime := StrToInt(STStart);

                _prevStop := StrToInt(STStop);
                SubTitleList[_i -1].StopTime := _prevStop;

                repeat
                  STText := StuffString(STText, Pos(SMI_BREAK, STText), length(SMI_BREAK), LFEED);
                until Pos(SMI_BREAK, STText) = 0;

                //
                for _j := 1 to length(STText) do
                  begin
                    _tmp := copy(STText, _j, 1);
                    if (_tmp = LFEED) or (_j = length(STText)) then
                      begin
                        SubTitleList[_i -1].SubTitleLine[_linenr - 1] := _subtline;
                        inc(_linenr);
                        SetLength(SubTitleList[_i - 1].SubTitleLine, _linenr);
                        _subtline := '';
                      end
                    else
                      _subtline := _subtline + _tmp;
                  end;

                inc(_i);
                SetLength(SubTitleList, _i);

                _completed := false;
                STStop :='';
                STStart :='';
                STText :='';
              end;
          end;
      until (EOSubt) or (Eof(SubTitleFile));
    end;
  end;
finally
  CloseFile(SubTitleFile);
  //SubTRunning := mpPending;
end;
end;


function TSubTitle.GetSubTitleIndex(): int64;
begin
  Result := iSubTitleIndex;
end;


procedure TSubTitle.SetSubTitleIndex(value: int64);
begin
  iSubTitleIndex := value;
end;


procedure TSubTitle.SetNeedNewText(value: boolean);
begin
  bNeedNewText := value;
end;


procedure TSubTitle.ResetIndex();
begin
  iSubTitleIndex := 0;
end;


function TSubTitle.DelSpaces(_Str: string): string;
var
  _i: integer;
begin
  if (length(_Str) > 1) then
    begin
      for _i := 1 to Length(_str) do
        begin
          if copy(_Str, _i, 1) <> ' ' then
            result := result + copy(_Str, _i, 1);
        end;
     end
  else
    result := '';
end;


function TSubTitle.ExtractSMITimeCode(_Str: string): string;
var
  _i: integer;
  _Tmp, _itime: string;

begin
  if (Length(_Str) > 1) then
    for _i := 1 to Length(_Str) do
      begin
        _Tmp := copy(_Str, _i, 1);
        if (_Tmp >= '0') and (_Tmp <= '9') then
          _itime := _itime + copy(_Str, _i, 1)
        else
          if _Tmp = '>' then break;
      end
  else
    Result := ERR09;

  if (_itime = '') then
    Result := ERR09
  else
    Result := _itime;
end;


procedure TSubTitle.SetAutoFormatText(bval: Boolean);
begin
  bAutoFormatText := bval;
end;


procedure TSubTitle.SetDefaultTextColor(clClr: TColor);
begin
  clDefaultTextColor := clClr;
end;


procedure TSubTitle.SetTextFormat(fFont: TFont);
begin
  fTextFormat := fFont;
end;


// Find the desired subtitle in the list
function TSubTitle.GetSubTitle(_find: int64): boolean;
var
  _j: integer;

begin

try
  SubTitleRec.SubTitleLine := Nil;
  if (SubTitleFileName <> 'NO_SUBTITLE_FILE') then
    begin
      SetLength(SubTitleRec.SubTitleLine, 0);
      SubTitleRec.StartTime := SubTitleList[_find].StartTime;
      SubTitleRec.StopTime := SubTitleList[_find].StopTime;

      if (SubTitleList[_find].FontTagStart <> Nil) Or
        (SubTitleList[_find].FontTagEnd <> Nil) then
        begin
          SubTitleRec.FontTagStart.Style := SubTitleList[_find].FontTagStart.Style;
          SubTitleRec.FontTagEnd.Style := SubTitleList[_find].FontTagEnd.Style;
          SubTitleRec.FontTagStart.Color := SubTitleList[_find].FontTagStart.Color;
          SubTitleRec.FontTagEnd.Color := SubTitleList[_find].FontTagEnd.Color;
        end;
    
      SubTitleRec.stIndex := SubTitleList[_find].stIndex;
  
      for _j := 0 to Length(SubTitleList[_find].SubTitleLine) - 1 do
        begin
          SetLength(SubTitleRec.SubTitleLine, _j + 1);
          SubTitleRec.SubTitleLine[_j] := SubTitleList[_find].SubTitleLine[_j];
        end;
      Result := True;
    end
  else
    Result := False;

except
  on exception do
    Result := False;
end;
end;


function TSubTitle.GetNewSubTitle(_AtTime: int64): int64; //returns the index
var
  iindex: Int64;

begin
  Result := 0;
try
  Stop();  // Stop timer
  iindex := 1;
  while (1 = 1) do
    begin
    //store the index
    if GetSubTitle(iindex) then
      Inc(iindex, 1)
    else
      begin
        Break;
        iSubTitleIndex := iindex;
      end;
    if (SubTitleRec.StartTime >= _AtTime) then
      begin
        Break;
      end;
    end;

  Tag := 0;
  Result := iSubTitleIndex;
  Start();  // Start timer
except
  on Exception do Exit;   //Silent exception
end;
end;

///-----------------------------------------------------------------------------

// TIMER methods
//==============

// TMfpTimerThread

constructor TMfpTimerThread.Create(ATimer: TSubTitleTimer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTimer := ATimer;
end;


destructor TMfpTimerThread.Destroy();
begin
  inherited Destroy();
end;


procedure TMfpTimerThread.DoTimer();
begin
  if Assigned(FTimer.OnTimer) then
    FTimer.OnTimer(FTimer);
end;


procedure TMfpTimerThread.Execute();
begin
  while (not Self.Terminated) and (FTimer.Activated) do
    begin
      WaitForSingleObject(Self.Handle,
                          FTimer.Resolution);
      Synchronize(DoTimer);
    end;
end;


// SubtitleTimer
//==============
constructor TSubtitleTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bActivated := False;
  iResolution := DEFAULT_TIMER_INTERVAL;   // one tenth of a second
end;


destructor TSubtitleTimer.Destroy();
begin
  inherited Destroy();
end;


procedure TSubtitleTimer.UpdateTimer();
begin
  if Assigned(FTimerThread) then
    begin
      FTimerThread.Terminate();
      FTimerThread := Nil;
    end;

  if bActivated then
    begin
      FTimerThread := TMfpTimerThread.Create(Self);
      FTimerThread.Start();
    end;
end;


procedure TSubtitleTimer.Start();
begin
  bActivated := True;
  bNeedNewText := True;
  UpdateTimer();
end;


procedure TSubtitleTimer.Stop();
begin
  bActivated := False;
  UpdateTimer();
end;


procedure TSubtitleTimer.SetResolution(const Value: Int64);
begin
  if (Value <> iResolution) then
  begin
    iResolution := Value;
    UpdateTimer();
  end;
end;



// Component
//==========
constructor TSubTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bNeedNewText := False;
  iSubTitleIndex := 0;
  // Create the textformat
  fTextFormat := TFont.Create;
  // Set the default properties
  with fTextFormat do
    begin
      Name := 'Verdana';
      Charset := DEFAULT_CHARSET;
      Orientation := 0;
      Size := 14;
      Orientation := 0;
      Quality := fqDefault;
      Pitch := fpDefault;
      Height := -19;
      Color := clWhite;
      Style := [];
    end;
  // Initialise the SubTitle record font properties
  SubTitleRec.FontTagStart := TFont.Create;
  SubTitleRec.FontTagEnd := TFont.Create;
  SubTitleRec.FontTagStart.Assign(fTextFormat);
  SubTitleRec.FontTagEnd.Assign(fTextFormat);

  clDefaultTextColor := clWhite;
  
end;


destructor TSubTitle.Destroy;
var
  _i: integer;

begin
  fTextFormat.Free;
  for _i := 0 to Length(SubTitleList) -1 do
    begin
      SubTitleList[_i].FontTagStart.Free;
      SubTitleList[_i].FontTagEnd.Free;
    end;
  SubTitleList := Nil;
  inherited Destroy;
end;


procedure Register;
begin
  RegisterComponents('MfPack Samples', [TSubTitle]);
end;

end.

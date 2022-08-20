// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: LangTags.pas
// Kind: Delphi file
// Release date: 13-08-2019
// Language: ENU
//
// Revision Version: 3.1.2
// Description: Language tag definitions file.
//
// Initiator(s): Tony (maXcomX), Peter (OzShips)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/06/2022 All                 Mercury release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
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
// Source: -
//
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit LangTags;


interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Globalization,
  {System}
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.RegularExpressions,
  {Project}
  MfPCXConstants;


  // Facebook
  // The namingconvention for SubRip-files (.srt) is:
  // filename.[language code]_[country code].srt

  // Flex
  // filename [Year or date].[language code].srt

type

  TTXT_TYPE = (NONE      = $0,    // None
               SUBRIB    = $1,    // SubRip
               MICRODVD  = $2,    // MicroDvd
               YOUTUBE   = $3,    // YouTube
               SAMI      = $4,    // SAMI/SMI
               WEBVTT    = $5,    // Web vtt HTML5
               UNKNOWN   = $7FFFFFFF);  // Unknown format


  GEO_ENUMPROC = function(GeoId: GEOID): BOOL; stdcall;

  GEO_ENUMNAMEPROC = function(Arg1: PWideChar;
                              Arg2: LPARAM): BOOL; stdcall;

  TEnumGeoData = record
    GeoCode: string;
    GeoName: string;
    LPar: LParam;
    Success: Boolean;
  end;

  TLangIDType = (idUser,
                 idSystem);

  TTxtProp = record
    bActiveFile: Boolean;          // True when user selected this or automated when loading a mediafile.
    sLanguageTag: string;          // Like: en
    sFriendlyLanguageName: string; // Like: English
    sFile: string;  // Url (Filename including path)
    sTTxtType: TTXT_TYPE;          // Timed text type
  end;
  TTxtPropArray = array of TTxtProp;


  TLanguageTags = class(TObject)
  private
    {Private declarations}
    lp_MatchList: TStrings;

    // REGEX implementation
    procedure AddMatchToList(match: TMatch);

{$HINTS OFF}
    // Gets all matches and puts it in a matchlist (Not used, keep here for later implementations)
    function GetMatches(const pattern: string; // RegEx pattern
                        const txt: string;     // Text to search in
                        const options: TRegExOptions = [roNotEmpty]): Integer;
{$HINTS ON}
    // Returns the single match
    function GetMatch(const pattern: string; // RegEx pattern
                      const txt: string;     // Text to search in
                      const options: TRegExOptions = [roNotEmpty]): string;

    // Compares pattern in string
    function IsMatch(const pattern: string;
                     const txt: string): Boolean;

  public
    {Public declarations}
    TimedTxtPropsArray             : TTxtPropArray;

    {Public methods}

    constructor  Create();
    destructor Destroy(); override;

    procedure Clear();

    // Gets the readable language from a LCID
    // siLocale can be one of the following:
    //   LOCALE_SNATIVELANGUAGENAME, LOCALE_SNATIVECOUNTRYNAME, LOCALE_SLANGUAGE
    //   LOCALE_SCOUNTRY, etc.
    function GetLocaleFromLCID(lcid: LCID;
                               siLocale: ShortInt): string;
    //
    function GetLocaleNameByLcId(dwLcid: LCID): string;

    function GetLcIdByLocaleName(lpName: LPWSTR): LCID;

    // Gets the readable language from a 2 char tag xx  = language or XX = Country
    function GetLangOrCountryFromTag(const tag: string;
                                     bReturnLang: Boolean = True): string;

    // List the timedtextfiles that match the language tag from the given url
    // If lang = '*' then ignore the language tags, just list them all
    function ReadFileTags(sUrl: WideString;
                          sPrefLang: string;
                          fps: DWord;
                          TimedTextExt: string): TTxtPropArray;

    //Example: ext = '.srt' or '.*', sUrl = 'c:\myvideos\myvideo.avi' or 'c:\myvideos\'
    function ListTimedTextFiles(ext: string;
                                sUrl: WideString): TArray<string>;
    // Returns the type of a timed text file
    function GetTimedTextType(sUrl: string): TTXT_TYPE;

    // Simplyfied function of GetUserDefaultGeoName
    // NOTE: Windows 10 >= version 1709 [desktop apps only]
    // Gets the country (not the language!)
    function GetUserDefaultGeoName(): string;
    //
    function GetUserDefaultLanguage(): string;

    //
    function GetUserGeoTag(): GEOID;
    // Returns format [language (Nation)] in local language
    function GetUserVerLanguage(DefaultLangId: TLangIDType): string;
    // Returns different formats depending on the LOCALE_S**********
    function GetUserLocaleInfo(dwLocales: DWORD): string;

    // < Win 10
    function TryGetCountryNameByISO2(const Code: string;  // Like 'EN'
                                     out Name: string): Boolean;  // The full language name
    // Win 10
    function TryGetCountryNameByISO2Ex(const Code: string;  // Like 'EN'
                                       out Name: string): Boolean;  // The full language name
  end;

var
  pc_LanguageTags: TLanguageTags;



  // Simplyfied function of GetUserDefaultGeoName
  function GetUserDefaultGeoName(): string;

  // Retrieves the number of installed languages.
  // Use this function before calling GetUserDefaultLanguageTag to determine the max
  // number of languages (TagIndex).
  function GetNumberOfUserDefaultLanguageTags(): Integer;

  // Get the language part bFullTag = False (xx) or full tag bFullTag = True (xx-XX)
  // By default the first tag will be retrieved (TagIndex = 1)
  function GetUserDefaultLanguageTag(TagIndex: ULONG = 1;
                                     bFullTag: Boolean = False): string;

  // Gets the country tag of the system  Win version <= 8.1
  function TryGetGeoInfo(GeoId: GEOID;
                         GeoType: GEOTYPE;
                         out Value: string): Boolean;

  // Gets the country tag of the system  Win version > 8.1
  function TryGetGeoInfoEx(Arg1: PWideChar;  // tag, like 'EN'
                           GeoType: GEOTYPE;
                           out Value: string): Boolean;

  // Retrieves the two-letter International Organization for
  // Standardization (ISO) 3166-1 code or numeric United Nations (UN) Series M,
  // Number 49 (M.49) code for the default geographical location of the user.
  // NOTE: This is an aliased function for GetUserDefaultGeoName!
  function GetUserDefaultGeoTag(geoName: LPWSTR;
                                geoNameCount: NativeInt): Integer; stdcall;

  function GetUserDefaultUILanguageTag(): LANGID; stdcall;

  function GetUserPreferredUILanguageTags(dwFlags: DWORD;
                                          pulNumLanguages: PULONG;
                                          pwszLanguagesBuffer: PWideChar;
                                          pcchLanguagesBuffer: PULONG): BOOL; stdcall;

  function GetSystemPreferredUILanguageTags(dwFlags: DWORD;
                                            pulNumLanguages: PULONG;
                                            pwszLanguagesBuffer: PWideChar;
                                            pcchLanguagesBuffer: PULONG): BOOL; stdcall;


  // Enumerates the geographical location identifiers (type GEOID) that are
  // available on the operating system.
  // NOTE:
  function EnumSystemGeoID(GeoClass: GEOCLASS;  // Geographical location class for which to enumerate the identifiers. At present, only GEOCLASS_NATION is supported. This type causes the function to enumerate all geographical identifiers for nations on the operating system.
                           ParentGeoId: GEOID;  // Reserved. This parameter must be 0.
                           lpGeoEnumProc: GEO_ENUMPROC  // Pointer to the application-defined callback function EnumGeoInfoProc. The EnumSystemGeoID function makes repeated calls to this callback function until it returns FALSE.
                          ): BOOL; stdcall;

  // Enumerates the two-letter International Organization for Standardization
  // (ISO) 3166-1 codes or numeric United Nations (UN) Series M,
  // Number 49 (M.49) codes for geographical locations that are available on the operating system.
  function EnumSystemGeoNames(geoClass: GEOCLASS;
                              geoEnumProc: GEO_ENUMNAMEPROC;
                              data: LPARAM
                             ): BOOL; stdcall;

  // Retrieves information about a specified geographical location.
  // For Windows 10 >= version 1709, use GetGeoInfoEx
  function GetGeoInfo(Location: GEOID;
                      GeoType: GEOTYPE;
                      lpGeoData: LPTSTR;
                      cchData: Integer;
                      LangId: LANGID): Integer; stdcall;

  // Retrieves information about a geographic location that you specify by using
  // a two-letter International Organization for Standardization (ISO) 3166-1 code or
  // numeric United Nations (UN) Series M, Number 49 (M.49) code.
  // NOTE: Minimum supported client is Windows 10, version 1709 [desktop apps only]
  function GetGeoInfoEx(location: PWideChar;
                        geoType: GEOTYPE;
                        geoData: PWideChar;
                        geoDataCount: Integer
                       ): Integer; stdcall;

  // Get the locale by LcId
  function LCIDToLocaleName(Locale: LCID;
                            lpName: LPWSTR;
                            cchName: Integer;
                            dwFlags: DWord): Integer; stdcall;

  // Get the LCID for the locale
  function LocaleNameToLCID(lpName: LPCWSTR;
                            dwFlags: DWord): LCID; stdcall;

implementation

const
  kernel32Lib ='kernel32.dll';

var
  EnumGeoData: TEnumGeoData;


constructor TLanguageTags.Create();
begin
  inherited Create();
  // Global stringlist to store matches
  lp_MatchList := TStringList.Create();
end;


destructor TLanguageTags.Destroy();
begin
  Clear();
  inherited Destroy();
end;


procedure TLanguageTags.Clear();

begin
  lp_MatchList.Clear;
  lp_MatchList.Free;
  SetLength(TimedTxtPropsArray, 0);
  Finalize(TimedTxtPropsArray);
  TimedTxtPropsArray := nil;
end;


function TLanguageTags.GetLocaleFromLCID(lcid: LCID;
                                         siLocale: ShortInt): string;
const
  MAXLINE = 254;
var
  pcResult : PWideChar;

begin
  Result:= '';
  pcResult:= StrAlloc(MAXLINE);
try
  fillchar(pcResult^,
           MAXLINE,
           0);
  GetLocaleInfo(lcid,
                siLocale,
                pcResult,
                MAXLINE);
  Result := String(pcResult);
finally
  StrDispose(pcResult);
end;
end;


function TLanguageTags.GetLocaleNameByLcId(dwLcid: LCID): string;
var
  wsName : array [0..LOCALE_NAME_MAX_LENGTH - 1] of WideChar;
begin
  if (LCIDToLocaleName(dwLcid,
                       wsName,
                       LOCALE_NAME_MAX_LENGTH,
                       0) = 0) then
    Result := 'Error(0)'
  else
    Result := wsName;
end;


function TLanguageTags.GetLcIdByLocaleName(lpName: LPWSTR): LCID;
begin
  // 0 = error
  Result := LocaleNameToLCID(lpName,
                             0);
end;


// Returns the language only or language and country with input tag = 'ZZ', 'xx' or 'xx_ZZ'
function TLanguageTags.GetLangOrCountryFromTag(const tag: string;
                                               bReturnLang: Boolean = True): string;
var
  dwLcId: LCID;
  sTag: String;

begin
  if Not Length(tag) in [2, 5] then
    begin
      Result:= 'Invalid Input';
      Exit;
    end;

  if bReturnLang then
    begin
      if Length(tag) = 2 then
        sTag := AnsiLowerCase(tag)
      else
        if Length(Tag) = 5 then
          sTag := AnsiLowerCase(LeftStr(tag, 2));
    end
  else
    begin
      if Length(tag) = 2 then
        sTag := AnsiUpperCase(tag)
      else
        if Length(Tag) = 5 then
          sTag := AnsiUpperCase(RightStr(tag, 2));
    end;

  dwLcId := GetLcIdByLocaleName(LPWSTR(stag));

  if bReturnLang then
    Result := GetLocaleFromLCID(dwLcId,
                                LOCALE_SNATIVELANGUAGENAME)
  else
    Result := GetLocaleFromLCID(dwLcId,
                                LOCALE_SNATIVECOUNTRYNAME);

end;


function GetCountryTag(sTag: string): string;
begin
  Result := MidStr(sTag, Pos('_', sTag) +1, 2);
end;

// This function reads all the language tags from the timedtext filenames and stores them
// in an array.
// NOTE: A tag has this format [xx_XX] where the first field specifies the language and
//       the second (in capitals) the country (where the language is spoken.)
//       The result will be the files that match
function TLanguageTags.ReadFileTags(sUrl: WideString;
                                    sPrefLang: string;
                                    fps: DWord;
                                    TimedTextExt: string): TTxtPropArray;
var
  sMediafile, sPath, sMatch: string;
  ar: TArray<string>;
  I: Integer;
  bFoundDefLangFile: Boolean;

begin

try

  bFoundDefLangFile := False;
  sMediafile := ExtractFileName(sUrl);
  sPath := IncludeTrailingPathDelimiter(ExtractFileDir(sUrl));

  ar := ListTimedTextFiles(TimedTextExt,
                           sPath);

  if (Length(ar) = 0) then
    Exit;  // result will be nil

  for I := 0 to Length(ar) - 1 do
    begin
      // Increase the array
      SetLength(Result, I + 1);

      Result[I].sFile := sPath + ar[I];
      // See if the file has a valid languagetag
      if IsMatch('[a-z][a-z]_[A-Z][A-Z]', ar[I]) then
        begin
          // Check and store if the systems default language or preferred language matches
          // the file tag we are going to use.
          sMatch := GetMatch('\[\K[a-z][a-z]',
                             ar[I],
                             [roSingleLine, roNotEmpty]);

          if (sPrefLang = sMatch) and (bFoundDefLangFile = False) then
            begin
              bFoundDefLangFile := True;
              Result[I].bActiveFile := bFoundDefLangFile;
            end
          else
             Result[I].bActiveFile := False;

          // Store the language tag part
          Result[I].sLanguageTag := sMatch;
        end
      else // No tag was found, we may assume that this file is the default users langauge,
           // based on the users system language. However, if a valid file was detected, we keep that one as the active file.
        begin
          bFoundDefLangFile := True;
          Result[I].bActiveFile := bFoundDefLangFile;
          Result[I].sLanguageTag := GetUserDefaultLanguageTag(1, False);
          Result[I].sTTxtType := GetTimedTextType(ar[I]);
        end;
      // Get the native language (localised friendly style)
      Result[I].sFriendlyLanguageName := pc_LanguageTags.GetLangOrCountryFromTag(Result[I].sLanguageTag);
    end;

finally
  Finalize(ar);
end;
end;


function TLanguageTags.ListTimedTextFiles(ext: string;
                                          sUrl: WideString): TArray<string>;
Var
  Path: string;
  searchrec : TSearchRec;
  Dir : TStrings;

begin

  Path := IncludeTrailingPathDelimiter(ExtractFileDir(sUrl));
  Dir := TStringList.Create;

try
{$WARN SYMBOL_PLATFORM OFF}
  if FindFirst(Path + '*' + ext, faArchive, searchrec) = 0 then
{$WARN SYMBOL_PLATFORM ON}
    begin
      repeat
        // Now we do a little check, because FindFirst will return all
        // extensions that match ext in it. Like .srtw, .srt~, srt_bak etc.
        // This is known as the 8.3 filename issue in Windows.
        if AnsiCompareText(ExtractFileExt(searchrec.Name), ext) = 0 then
          Dir.Add(searchrec.Name); // Add to list
      until FindNext(searchrec) <> 0;
      FindClose(searchrec);
    end;
  Result := Dir.ToStringArray;
finally
  Dir.Free;
end;
end;


function TLanguageTags.GetTimedTextType(sUrl: string): TTXT_TYPE;
var
  sExt: string;

begin
  sExt := ExtractFileExt(sUrl);

  if AnsiCompareText(sExt, EXTSUBRIP) = 0 then
    Result := SUBRIB
  else if AnsiCompareText(sExt, EXTMICRODVD) = 0 then
    Result := MICRODVD
  else if AnsiCompareText(sExt, EXTYOUTUBE) = 0 then
    Result := YOUTUBE
  else if AnsiCompareText(sExt, EXTSAMI) = 0 then
    Result := SAMI
  else if AnsiCompareText(sExt, EXTSMI) = 0 then
    Result := SAMI
  else if AnsiCompareText(sExt, EXTWEBVTT) = 0 then
    Result := WEBVTT
  else
    Result := UNKNOWN;
end;



// API

function TLanguageTags.GetUserDefaultGeoName(): string;
begin
  Result := GetUserDefaultGeoName();
end;


function GetNumberOfUserDefaultLanguageTags(): Integer;
var
  ulNumLang: ULONG;
  LanguagesBufSize: ULONG;
begin
  LanguagesBufSize := 0;
  Result := 0;
  if GetUserPreferredUILanguageTags(MUI_LANGUAGE_NAME,
                                    @ulNumLang,
                                    Nil,
                                    @LanguagesBufSize) then
  Result := ulNumLang;
end;


function GetUserDefaultLanguageTag(TagIndex: ULONG = 1;
                                   bFullTag: Boolean = False): string;
var
  ulNumLang: ULONG;
  LanguagesBufSize: ULONG;
  LanguagesBuf: TArray<WideChar>;
  wcp: PWideChar;
  ws: string;
  iTags: ULONG;

begin
  LanguagesBufSize := 0;
  //GetUserPreferredUILanguages
  // Get the buffersize first
  // SEE: https://docs.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-getuserpreferreduilanguages
  if GetUserPreferredUILanguageTags(MUI_LANGUAGE_NAME,
                                    @ulNumLang,
                                    Nil,
                                    @LanguagesBufSize) then
  // Set the size of the buffer
  SetLength(LanguagesBuf, LanguagesBufSize);

  if GetUserPreferredUILanguageTags(MUI_LANGUAGE_NAME,
                                    @ulNumLang,
                                    @LanguagesBuf[0],
                                    @LanguagesBufSize) then
    begin
      iTags := 0;
      wcp := @LanguagesBuf[0];

      // Check if TagIndex is inbetween boundaries
      if (TagIndex < 1) then
        TagIndex := 1
      else if (TagIndex > ulNumLang) then
        TagIndex := ulNumLang;

      // Extract the null terminated tags
      while (wcp^ <> #0) and (iTags < TagIndex) do
        begin
          ws := wcp;
          inc(iTags);
          inc(wcp, Length(ws) + 1);
       end;

      // Return the language part or the full part
      if bFullTag then
        Result := ws   // Full tag  xx-XX
      else
        Result := copy(ws, 1, Pos('-', ws) - 1); // language part
    end;
end;


function TLanguageTags.GetUserDefaultLanguage(): string;
var
  lid: LANGID;
  userlang: array [0..64] of char;

begin
  // Get the default language tag from the system
  lid := GetUserDefaultUILanguageTag();
  VerLanguageName(lid,
                  userlang,
                  SizeOf(userlang));
  Result := string(userlang);
end;


// Windows 10, version 1709 [desktop apps only]
function GetUserDefaultGeoName(): string;
var
  Buffer: PWideChar;
  Size, rSize: Integer;

begin
  // Do a initiate first to get the size of the buffer.
  rSize := GetUserDefaultGeoTag(Nil,
                                0);
  // Reserve memory for the buffer
  GetMem(Buffer, rSize);

try
  // Call the function again to retrieve the languagetag(s)
  Size := GetUserDefaultGeoTag(Buffer,
                               rSize);
  if (Size > 0) then
    Result := string(Buffer)
  else
    Result := 'Unknown Geo Tag';

finally
  FreeMem(Buffer);
end;
end;


function TLanguageTags.GetUserGeoTag(): GEOID;
begin
  Result := GetUserGeoID(DWORD(GEOCLASS_NATION));
end;


function TLanguageTags.GetUserVerLanguage(DefaultLangId: TLangIDType): string;
var
  lId : LangID;
  aLc: array [0..254] of WideChar;

begin

  if (DefaultLangId = idUser) then
    lId := GetUserDefaultUILanguage()
  else   // idSystem
    lId := GetSystemDefaultLangID();

  VerLanguageName(lId,
                  aLc,
                  SizeOf(aLc));

  Result := string(aLc);

end;


function TLanguageTags.GetUserLocaleInfo(dwLocales: DWORD): string;
var
  Buffer: PWideChar;
  Size: Integer;

begin
  // Do a initiate first to get the size of the buffer
  Size := GetLocaleInfo(LOCALE_USER_DEFAULT,
                        dwLocales,
                        Nil,
                        0);
  GetMem(Buffer, Size);

try
  // Call the function again to retrieve the languagetag(s)
  GetLocaleInfo(LOCALE_USER_DEFAULT,
                dwLocales,
                Buffer,
                Size);

  Result := string(Buffer);

finally
  FreeMem(Buffer);
end;
end;


//
function TryGetGeoInfo(GeoId: GEOID;
                       GeoType: GEOTYPE;
                       out Value: string): Boolean;
var
  Buffer: string;
  BufferLen: Integer;

begin
  Result := False;
  // Get the needed buffersize
  BufferLen := GetGeoInfo(GeoId,
               GeoType,
               LPTSTR(Buffer),
               0,
               0);

  if (BufferLen <> 0) then
    begin
      SetLength(Buffer, BufferLen);
      Result := GetGeoInfo(GeoId,
                           GeoType,
                           LPTSTR(Buffer),
                           BufferLen,
                           0) <> 0;
      if Result then
        Value := Trim(Buffer);
  end;
end;


function TryGetGeoInfoEx(Arg1: PWideChar;  // tag, like 'EN'
                         GeoType: GEOTYPE;
                         out Value: string): Boolean;
var
  Buffer: string;
  BufferLen: Integer;

begin
  Result := False;
  // Get the needed buffersize
  BufferLen := GetGeoInfoEx(Arg1,
                            GeoType,
                            LPTSTR(Buffer),
                            0);

  if (BufferLen <> 0) then
    begin
      SetLength(Buffer, BufferLen);
      Result := GetGeoInfoEx(Arg1,
                             GeoType,
                             LPTSTR(Buffer),
                             BufferLen) <> 0;
      if Result then
        Value := Trim(Buffer);
  end;
end;

//
function EnumGeoInfoProc(GeoId: GEOID): BOOL; stdcall;
var
  S: string;

begin
  Result := TryGetGeoInfo(GeoId,
                          GEOTYPE(GEO_ISO2),
                          S);

  if Result and (S = EnumGeoData.GeoCode) then
  begin
    // stop the enumeration since we've found the country by its ISO code
    Result := False;
    // return the success flag and try to return the friendly name of the country to the
    // EnumData.GeoName record field; you may optionally query the GEO_OFFICIALNAME
    EnumGeoData.Success := TryGetGeoInfo(GeoId,
                                         GEOTYPE(GEO_FRIENDLYNAME),
                                         EnumGeoData.GeoName);
  end;
end;


//
function EnumGeoInfoProcEx(Arg1: PWideChar;  // tag, like 'EN'
                           Arg2: LPARAM): BOOL; stdcall;
var
  S: string;

begin
  Result := TryGetGeoInfoEx(Arg1,
                            GEOTYPE(GEO_FRIENDLYNAME),
                            S);
  // test
  if Arg2 > 0 then
    Result := False;

  if Result and (S = EnumGeoData.GeoCode) then
  begin
    // stop the enumeration since we've found the country by its ISO code
    Result := False;
    // return the success flag and try to return the friendly name of the country to the
    // EnumData.GeoName record field; you can optionally query the GEO_OFFICIALNAME
    EnumGeoData.Success := TryGetGeoInfoEx(Arg1,
                                           GEOTYPE(GEO_FRIENDLYNAME),
                                           EnumGeoData.GeoName);
  end;
end;


function TLanguageTags.TryGetCountryNameByISO2(const Code: string;  // Like 'EN'
                                               out Name: string): Boolean;  // The full language name
begin
  EnumGeoData.GeoCode := UpperCase(RightStr(Code, 2), loUserLocale);
  EnumGeoData.Success := False;

  if not EnumSystemGeoID(GEOCLASS(GEOCLASS_NATION),
                         0,
                         EnumGeoInfoProc) then
    RaiseLastOSError;

  Result := EnumGeoData.Success;
  if Result then
    Name := EnumGeoData.GeoName
  else
    Name := 'Unknown';
end;


function TLanguageTags.TryGetCountryNameByISO2Ex(const Code: string;  // Like 'EN'
                                                 out Name: string): Boolean;  // The full language name
begin
  // if the Code contains the full tag, like 'en_UK', we need to strip off the language part.
  EnumGeoData.GeoCode := UpperCase(RightStr(Code, 2), loUserLocale);
  EnumGeoData.Success := False;
  EnumGeoData.LPar := 0;

  if not EnumSystemGeoNames(GEOCLASS(GEOCLASS_NATION),
                            EnumGeoInfoProcEx,
                            EnumGeoData.LPar) then
    RaiseLastOSError;

  Result := EnumGeoData.Success;
  if Result then
    Name := EnumGeoData.GeoName
  else
    Name := 'Unknown';
end;


////////////////////////////////////////////////////////////////////////////////

procedure TLanguageTags.AddMatchToList(match: TMatch);
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


function TLanguageTags.GetMatches(const pattern: string; // RegEx pattern
                                  const txt: string;     // Text to search in
                                  const options: TRegExOptions = [roNotEmpty]): Integer;
var
  RegEx : TRegEx;
  Match : TMatch;

begin
  if Not Assigned(lp_MatchList) then
    // Global stringlist to store matches
  lp_MatchList := TStringList.Create();

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


function TLanguageTags.GetMatch(const pattern: string; // RegEx pattern
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
function TLanguageTags.IsMatch(const pattern: string;
                               const txt: string): Boolean;
begin
  Result := TRegEx.IsMatch(txt, pattern); // For example: '[a-z][a-z]_[A-Z][A-Z]' or '_[A-Z][A-Z]' or '[a-z][a-z]_' etc.
end;


{$WARN SYMBOL_PLATFORM OFF}

function GetUserDefaultGeoTag; external kernel32Lib name 'GetUserDefaultGeoName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetUserDefaultUILanguageTag; external kernel32Lib name 'GetUserDefaultUILanguage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetUserPreferredUILanguageTags; external kernel32Lib name 'GetUserPreferredUILanguages' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetSystemPreferredUILanguageTags; external kernel32Lib name 'GetSystemPreferredUILanguages' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function EnumSystemGeoID; external kernel32Lib name 'EnumSystemGeoID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function EnumSystemGeoNames; external kernel32Lib name 'EnumSystemGeoNames' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetGeoInfo; external kernel32Lib name {$IFDEF UNICODE}'GetGeoInfoW'{$ELSE}'GetGeoInfoA'{$ENDIF} {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetGeoInfoEx; external kernel32Lib name 'GetGeoInfoEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function LCIDToLocaleName; external kernel32Lib name 'LCIDToLocaleName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function LocaleNameToLCID; external kernel32Lib name 'LocaleNameToLCID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

end.

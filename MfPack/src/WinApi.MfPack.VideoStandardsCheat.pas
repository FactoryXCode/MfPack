// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MfPack.VideoStandardsCheat.pas
// Kind: Pascal / Delphi unit
// Release date: 14-11-2023
// Language: ENU
//
// Revision Version: 3.1.6
// Description: Video standards cheat.
//              This class contains the latest standardised video resolutions,
//              aspect ratios and framerates.
// Usage:
//      Call Create to create the class.
//      The constructor calls GetResolutions() to get the resolutions and aspectratio's and
//      GetFrameRates() to get the framerates.
//      When not needed anymore call Free.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
//==============================================================================
// Source: https://www.unravel.com.au/aspect-ratio-cheat-sheet
//         https://quickref.me/aspect-ratio.html
//         https://inwhitefilms.nl/en/film-tools/aspect-ratio-cheat-sheet
//         https://www.scribd.com/document/411401867/Aspect-Ratio-Resoultions-Cheat-Sheet-pdf
//         https://www.ibm.com/docs/en/video-analytics/1.0.6?topic=requirements-camera-frame-rate-resolution-video-format
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
unit WinApi.MfPack.VideoStandardsCheat;


interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils;

type

  TResolution = record
    Resolution: string;
    iWidth: UInt32;
    iHeight: UInt32;
    AspectRatio: Double;
    AspectRatioNumerator: Double;
    AspectRatioDenominator: UInt32;
    StrAspectRatio: string;
    Remarks: string;
  end;

  TResolutionsArray = array of TResolution;

  TFrameRate = record
    FrameRate: Double;
    Numerator: UINT32;
    Denominator: UINT32;
    sFrameRate: string;
    sHint: string;
  end;

  TFrameRatesArray = array of TFrameRate;

  /// <summary> This class stores most common resolutions, aspect ratios and framerates used today.</summary>
  TVideoStandardsCheat = class
  private
    iCounter: Integer;

    aResolutionArray: TResolutionsArray;
    aFrameRatesArray: TFrameRatesArray;
    rResolution: TResolution;
    rFrameRate: TFrameRate;

    /// <summary> Stores all resolutions and aspectratios in a public array (property Resolutions). </summary>
    procedure GetResolutions();
    /// <summary> Stores all framerates in a public array (property FrameRates). </summary>
    procedure GetFrameRates();


    procedure SetRes(hResolution: string;
                     hWidth: Integer;
                     hHeight: Integer;
                     hAspectRatio: Double;
                     hAspectRatioNumerator: UINT32;
                     hAspectRatioDenominator: UINT32;
                     hStrAspectRatio: string;
                     hRemarks: string); inline;

    procedure SetFrameRates(iIndex: Integer;
                            fRate: Double;
                            Numerator: UINT32;
                            Denominator: UINT32;
                            sHint: string);

  public
    /// <summary> run myVideoStandardsCheat := TVideoStandardsCheat.Create. </summary>
    constructor Create();
    destructor Destroy(); override;

    /// <summary> Set a resolution by selected array index. </summary>
    procedure SetResolutionByIndex(index: Integer);
    /// <summary> Get the resolution type by given height and width. </summary>
    function GetResolutionType(aWidth: UInt32;
                               aHeight: UInt32): string;
    /// <summary> Set a frame rate by selected array index. </summary>
    procedure SetFrameRateByIndex(index: Integer);

    /// <summary> Holds the array with all common resolutions. </summary>
    property Resolutions: TResolutionsArray read aResolutionArray write aResolutionArray;
    /// <summary> Holds the array with all common framerates. </summary>
    property FrameRates: TFrameRatesArray read aFrameRatesArray write aFrameRatesArray;
    /// <summary> Holds the selected resolution by index record. </summary>
    property SelectedResolution: TResolution read rResolution;
    /// <summary> Holds the selected framerate by index record. </summary>
    property SelectedFrameRate: TFrameRate read rFrameRate;

  end;

  // For global access.
  // Usage: FVideoStandardsCheat := TVideoStandardsCheat.Create;
var
  FVideoStandardsCheat: TVideoStandardsCheat;


implementation


constructor TVideoStandardsCheat.Create();
begin
  inherited Create();

  GetResolutions();
  GetFrameRates();
end;


destructor TVideoStandardsCheat.Destroy();
begin
  SetLength(aResolutionArray,
            0);
  aResolutionArray := nil;
  SetLength(aFrameRatesArray,
            0);
  aFrameRatesArray := nil;
  inherited Destroy();
end;


procedure TVideoStandardsCheat.SetRes(hResolution: string;
                                      hWidth: Integer;
                                      hHeight: Integer;
                                      hAspectRatio: Double;
                                      hAspectRatioNumerator: UINT32;
                                      hAspectRatioDenominator: UINT32;
                                      hStrAspectRatio: string;
                                      hRemarks: string);
begin
  aResolutionArray[iCounter].Resolution := hResolution;
  aResolutionArray[iCounter].iWidth := hWidth;
  aResolutionArray[iCounter].iHeight := hHeight;
  aResolutionArray[iCounter].AspectRatio:= hAspectRatio;
  aResolutionArray[iCounter].AspectRatioNumerator := hAspectRatioNumerator;
  aResolutionArray[iCounter].AspectRatioDenominator := hAspectRatioDenominator;
  aResolutionArray[iCounter].StrAspectRatio := hStrAspectRatio;
  aResolutionArray[iCounter].Remarks := hRemarks;
  Inc(iCounter);
end;


procedure TVideoStandardsCheat.GetResolutions();
var
  rRes: string;
  rWidth: Integer;

const
  // String
  s4_3   = '4:3';
  s5_3   = '5:3';
  s16_10 = '16:10';
  s16_9  = '16:9';
  s21_9  = '21:9';
  s64_27 = '64:27';
  s1_85  = '1.85:1';
  s1_90  = '1.90:1';
  s1_94  = '1.94:1';
  s2_00  = '2.0:1';
  s2_35  = '2.35:1';
  s2_37  = '2.37:1';
  s2_39  = '2.39:1';
  s2_40  = '2.40:1';
  s2_44  = '2.44:1';
  // Double
  r4_3   = 1.33; // 4:3
  r4_3_D = 133;
  r4_3_N = 100;

  r5_3   = 1.66; // 5:3
  r5_3_D = 166;
  r5_3_N = 100;

  r16_10 = 1.60; // 16:10
  r16_10_D = 160;
  r16_10_N = 100;

  r16_9  = 1.78; // 16:9
  r16_9_D = 178;
  r16_9_N = 100;

  r1_85  = 1.85;
  r1_85_D = 185;
  r1_85_N = 100;

  r1_90  = 1.90;
  r1_90_D = 190;
  r1_90_N = 100;

  r1_94  = 1.94;
  r1_94_D = 194;
  r1_94_N = 100;

  r2_00  = 2.0;
  r2_00_D = 200;
  r2_00_N = 100;

  r2_35  = 2.35;
  r2_35_D = 235;
  r2_35_N = 100;

  r2_37  = 2.37; // 64:27
  r2_37_D = 237;
  r2_37_N = 100;

  r2_39  = 2.39;
  r2_39_D = 239;
  r2_39_N = 100;

  r2_40  = 2.40; // 21:9
  r2_40_D = 240;
  r2_40_N = 100;

  r2_44  = 2.44;
  r2_44_D = 244;
  r2_44_N = 100;

  // DCP = Digital Cinema Package
  Cinema_Scope = 'Cinema Scope';
  Cinema_Flat  = 'Cinema Flat';
  RED_Wide     = 'RED Wide';

begin
  SetLength(aResolutionArray,
            141);
  iCounter := 0;

  // 8K
  rRes := '8K';
  rWidth := 8192;



  SetRes(rRes, rWidth, 6144, r4_3, r4_3_N, r4_3_D, s4_3, '');                {1}
  SetRes(rRes, rWidth, 4914, r5_3, r5_3_N, r5_3_D, s5_3, '');                {2}
  SetRes(rRes, rWidth, 4608, r16_9, r16_9_N, r16_9_D, s16_9, '');            {3}
  SetRes(rRes, rWidth, 4428, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);   {4}
  SetRes(rRes, rWidth, 4320, r1_90, r1_90_N, r1_90_D, s1_90, 'RED 8K FF');   {5}
  SetRes(rRes, rWidth, 4096, r2_00, r2_00_N, r2_00_D, s2_00, '');            {6}
  SetRes(rRes, rWidth, 3484, r2_35, r2_35_N, r2_35_D, s2_35, '');            {7}
  SetRes(rRes, rWidth, 3456, r2_37, r2_37_N, r2_37_D, s2_37, 'RED 8K Wide'); {8}
  SetRes(rRes, rWidth, 3432, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);  {9}
  SetRes(rRes, rWidth, 3412, r2_40, r2_40_N, r2_40_D, s21_9, '');           {10}
  SetRes(rRes, rWidth, 3356, r2_44, r2_44_N, r2_44_D, s2_44, '');           {11}


  // 8K UHD
  rRes := '8K UHD';
  rWidth := 7680;

  SetRes(rRes, rWidth, 5760, r4_3, r4_3_N, r4_3_D, s4_3, '');               {12}
  SetRes(rRes, rWidth, 4608, r5_3, r5_3_N, r5_3_D, s5_3, '');               {13}
  SetRes(rRes, rWidth, 4320, r16_9, r16_9_N, r16_9_D, s16_9, '');           {14}
  SetRes(rRes, rWidth, 4150, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);  {15}
  SetRes(rRes, rWidth, 4042, r1_90, r1_90_N, r1_90_D, s1_90, '');           {16}
  SetRes(rRes, rWidth, 3840, r2_00, r2_00_N, r2_00_D, s2_00, '');           {17}
  SetRes(rRes, rWidth, 3268, r2_35, r2_35_N, r2_35_D, s2_35, '');           {18}
  SetRes(rRes, rWidth, 3240, r2_37, r2_37_N, r2_37_D, s2_37, '');           {19}
  SetRes(rRes, rWidth, 3216, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope); {20}
  SetRes(rRes, rWidth, 3200, r2_40, r2_40_N, r2_40_D, s21_9, '');           {21}
  SetRes(rRes, rWidth, 3146, r2_44, r2_44_N, r2_44_D, s2_44, '');           {22}


  // 6K
  rRes := '6K';
  rWidth := 6144;

  SetRes(rRes, rWidth, 4608, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {23}
  SetRes(rRes, rWidth, 3686, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {24}
  SetRes(rRes, rWidth, 3456, r16_9, r16_9_N, r16_9_D, s16_9, '');             {25}
  SetRes(rRes, rWidth, 3320, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {26}
  SetRes(rRes, rWidth, 3232, r1_90, r1_90_N, r1_90_D, s1_90, '');             {27}
  SetRes(rRes, rWidth, 3160, r1_94, r1_94_N, r1_94_D, s1_94, '6K Dragon FF'); {28}
  SetRes(rRes, rWidth, 3072, r2_00, r2_00_N, r2_00_D, s2_00, '');             {29}
  SetRes(rRes, rWidth, 2614, r2_35, r2_35_N, r2_35_D, s2_35, '');             {30}
  SetRes(rRes, rWidth, 2592, r2_37, r2_37_N, r2_37_D, s2_37, 'RED 6K Wide');  {31}
  SetRes(rRes, rWidth, 2574, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);   {32}
  SetRes(rRes, rWidth, 2560, r2_40, r2_40_N, r2_40_D, s21_9, '');             {33}
  SetRes(rRes, rWidth, 2518, r2_44, r2_44_N, r2_44_D, s2_44, '');             {34}


  // 5K
  rRes := '5K';
  rWidth := 5120;

  SetRes(rRes, rWidth, 3840, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {35}
  SetRes(rRes, rWidth, 3072, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {36}
  SetRes(rRes, rWidth, 2880, r16_9, r16_9_N, r16_9_D, s16_9, '');             {37}
  SetRes(rRes, rWidth, 2766, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {38}
  SetRes(rRes, rWidth, 2700, r1_90, r1_90_N, r1_90_D, s1_90, 'Epic-X FF');    {39}
  SetRes(rRes, rWidth, 2560, r2_00, r2_00_N, r2_00_D, s2_00, '');             {40}
  SetRes(rRes, rWidth, 2178, r2_35, r2_35_N, r2_35_D, s2_35, '');             {41}
  SetRes(rRes, rWidth, 2160, r2_37, r2_37_N, r2_37_D, s2_37, RED_Wide);       {42}
  SetRes(rRes, rWidth, 2144, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);   {43}
  SetRes(rRes, rWidth, 2132, r2_40, r2_40_N, r2_40_D, s21_9, '');             {44}
  SetRes(rRes, rWidth, 2098, r2_44, r2_44_N, r2_44_D, s2_44, '');             {45}


  // Cinema DCP 4K
  rRes := 'Cinema DCP 4K';

  SetRes(rRes, 3996, 2160, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);      {46}
  SetRes(rRes, 4096, 1716, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);     {47}
  SetRes(rRes, 4096, 2160, r1_90, r1_90_N, r1_90_D, s1_90, '');               {48}


  // 4K
  rRes := '4K';
  rWidth := 4096;

  SetRes(rRes, rWidth, 3072, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {49}
  SetRes(rRes, rWidth, 2456, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {50}
  SetRes(rRes, rWidth, 2304, r16_9, r16_9_N, r16_9_D, s16_9, '');             {51}
  SetRes(rRes, rWidth, 2214, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {52}
  SetRes(rRes, rWidth, 2160, r1_90, r1_90_N, r1_90_D, s1_90, '4K RED');       {53}
  SetRes(rRes, rWidth, 2048, r2_00, r2_00_N, r2_00_D, s2_00, '');             {54}
  SetRes(rRes, rWidth, 1742, r2_35, r2_35_N, r2_35_D, s2_35, '');             {55}
  SetRes(rRes, rWidth, 1728, r2_37, r2_37_N, r2_37_D, s64_27, RED_Wide);      {56}
  SetRes(rRes, rWidth, 1716, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);   {57}
  SetRes(rRes, rWidth, 1706, r2_40, r2_40_N, r2_40_D, s21_9, '');             {58}
  SetRes(rRes, rWidth, 1678, r2_44, r2_44_N, r2_44_D, s2_44, '');             {59}


  // 4K UHD
  rRes := '4K UHD';
  rWidth := 3840;

  SetRes(rRes, rWidth, 2880, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {60}
  SetRes(rRes, rWidth, 2304, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {61}
  SetRes(rRes, rWidth, 2160, r16_9, r16_9_N, r16_9_D, s16_9, '');             {62}
  SetRes(rRes, rWidth, 2074, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {63}
  SetRes(rRes, rWidth, 2020, r1_90, r1_90_N, r1_90_D, s1_90, '');             {64}
  SetRes(rRes, rWidth, 1920, r2_00, r2_00_N, r2_00_D, s2_00, '');             {65}
  SetRes(rRes, rWidth, 1634, r2_35, r2_35_N, r2_35_D, s2_35, '');             {66}
  SetRes(rRes, rWidth, 1620, r2_37, r2_37_N, r2_37_D, s64_27, '');            {67}
  SetRes(rRes, rWidth, 1608, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);   {68}
  SetRes(rRes, rWidth, 1600, r2_40, r2_40_N, r2_40_D, s21_9, '');             {69}
  SetRes(rRes, rWidth, 1572, r2_44, r2_44_N, r2_44_D, s2_44, '');             {70}


  // 3K
  rRes := '3K';
  rWidth := 3072;

  SetRes(rRes, rWidth, 2304, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {71}
  SetRes(rRes, rWidth, 1842, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {72}
  SetRes(rRes, rWidth, 1728, r16_9, r16_9_N, r16_9_D, s16_9, '');             {73}
  SetRes(rRes, rWidth, 1660, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {74}
  SetRes(rRes, rWidth, 1620, r1_90, r1_90_N, r1_90_D, s1_90, '3K, RED');      {75}
  SetRes(rRes, rWidth, 1536, r2_00, r2_00_N, r2_00_D, s2_00, '');             {76}
  SetRes(rRes, rWidth, 1306, r2_35, r2_35_N, r2_35_D, s2_35, '');             {77}
  SetRes(rRes, rWidth, 1296, r2_37, r2_37_N, r2_37_D, s64_27, RED_Wide);      {78}
  SetRes(rRes, rWidth, 1286, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);   {79}
  SetRes(rRes, rWidth, 1280, r2_40, r2_40_N, r2_40_D, s21_9, '');             {80}
  SetRes(rRes, rWidth, 1258, r2_44, r2_44_N, r2_44_D, s2_44, '');             {81}


  // 3K HD
  rRes := '3K HD';
  rWidth := 2880;

  SetRes(rRes, rWidth, 2160, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {82}
  SetRes(rRes, rWidth, 1728, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {83}
  SetRes(rRes, rWidth, 1620, r16_9, r16_9_N, r16_9_D, s16_9, '');             {84}
  SetRes(rRes, rWidth, 1556, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {85}
  SetRes(rRes, rWidth, 1514, r1_90, r1_90_N, r1_90_D, s1_90, '');             {86}
  SetRes(rRes, rWidth, 1440, r2_00, r2_00_N, r2_00_D, s2_00, '');             {87}
  SetRes(rRes, rWidth, 1224, r2_35, r2_35_N, r2_35_D, s2_35, '');             {88}
  SetRes(rRes, rWidth, 1214, r2_37, r2_37_N, r2_37_D, s64_27, '');            {89}
  SetRes(rRes, rWidth, 1206, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);   {90}
  SetRes(rRes, rWidth, 1200, r2_40, r2_40_N, r2_40_D, s21_9, '');             {91}
  SetRes(rRes, rWidth, 1180, r2_44, r2_44_N, r2_44_D, s2_44, '');             {92}


  // Cinema DCP 2K
  rRes := 'Cinema DCP 2K';

  SetRes(rRes, 1998, 1080, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);      {93}
  SetRes(rRes, 2048, 858, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);      {94}
  SetRes(rRes, 2048, 1080, r1_90, r1_90_N, r1_90_D, s1_90, '');               {95}


  // 2K
  rRes := '2K';
  rWidth := 2048;

  SetRes(rRes, rWidth, 1536, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {96}
  SetRes(rRes, rWidth, 1228, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {97}
  SetRes(rRes, rWidth, 1152, r16_9, r16_9_N, r16_9_D, s16_9, '');             {98}
  SetRes(rRes, rWidth, 1106, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {99}
  SetRes(rRes, rWidth, 1078, r1_90, r1_90_N, r1_90_D, s1_90, '');             {100}
  SetRes(rRes, rWidth, 1024, r2_00, r2_00_N, r2_00_D, s2_00, '');             {101}
  SetRes(rRes, rWidth, 870, r2_35, r2_35_N, r2_35_D, s2_35, '');              {102}
  SetRes(rRes, rWidth, 864, r2_37, r2_37_N, r2_37_D, s64_27, '');             {103}
  SetRes(rRes, rWidth, 858, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);    {104}
  SetRes(rRes, rWidth, 852, r2_40, r2_40_N, r2_40_D, s21_9, '');              {105}
  SetRes(rRes, rWidth, 838, r2_44, r2_44_N, r2_44_D, s2_44, '');              {106}


  // 1080p
  rRes := '1080p';
  rWidth := 1920;

  SetRes(rRes, rWidth, 1440, r4_3, r4_3_N, r4_3_D, s4_3, '');                 {107}
  SetRes(rRes, rWidth, 1152, r5_3, r5_3_N, r5_3_D, s5_3, '');                 {108}
  SetRes(rRes, rWidth, 1200, r16_10, r16_10_N, r16_10_D, s16_10, '');         {109}
  SetRes(rRes, rWidth, 1080, r16_9, r16_9_N, r16_9_D, s16_9, '');             {110}
  SetRes(rRes, rWidth, 1036, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);    {111}
  SetRes(rRes, rWidth, 1010, r1_90, r1_90_N, r1_90_D, s1_90, '');             {112}
  SetRes(rRes, rWidth, 960, r2_00, r2_00_N, r2_00_D, s2_00, '');              {113}
  SetRes(rRes, rWidth, 816, r2_35, r2_35_N, r2_35_D, s2_35, '');              {114}
  SetRes(rRes, rWidth, 810, r2_37, r2_37_N, r2_37_D, s64_27, '');             {115}
  SetRes(rRes, rWidth, 804, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);    {116}
  SetRes(rRes, rWidth, 800, r2_40, r2_40_N, r2_40_D, s21_9, 'Blu-ray Scope'); {117}
  SetRes(rRes, rWidth, 786, r2_44, r2_44_N, r2_44_D, s2_44, '');              {118}


  // 720p
  rRes := '720p';
  rWidth := 1280;

  SetRes(rRes, rWidth, 962, r4_3, r4_3_N, r4_3_D, s4_3, '');                  {119}
  SetRes(rRes, rWidth, 768, r5_3, r5_3_N, r5_3_D, s5_3, '');                  {120}
  SetRes(rRes, rWidth, 720, r16_9, r16_9_N, r16_9_D, s16_9, '');              {121}
  SetRes(rRes, rWidth, 690, r1_85, r1_85_N, r1_85_D, s1_85, Cinema_Flat);     {122}
  SetRes(rRes, rWidth, 672, r1_90, r1_90_N, r1_90_D, s1_90, '');              {123}
  SetRes(rRes, rWidth, 640, r2_00, r2_00_N, r2_00_D, s2_00, '');              {124}
  SetRes(rRes, rWidth, 544, r2_35, r2_35_N, r2_35_D, s2_35, '');              {125}
  SetRes(rRes, rWidth, 540, r2_37, r2_37_N, r2_37_D, s64_27, '');             {126}
  SetRes(rRes, rWidth, 536, r2_39, r2_39_N, r2_39_D, s2_39, Cinema_Scope);    {127}
  SetRes(rRes, rWidth, 532, r2_40, r2_40_N, r2_40_D, s21_9, '');              {128}
  SetRes(rRes, rWidth, 524, r2_44, r2_44_N, r2_44_D, s2_44, '');              {129}


  // 576p PAL SD
  rRes := '576p PAL SD';

  SetRes(rRes, 768, 576, r4_3, r4_3_N, r4_3_D, s4_3, 'Square pixels 1.0');                           {130}
  SetRes(rRes, 720, 576, r4_3, r4_3_N, r4_3_D, s4_3, 'PAL DV 1.09 pixel aspect ratio');              {131}
  SetRes(rRes, 102, 576, r16_9, r16_9_N, r16_9_D, s16_9, 'Square pixels 1.0');                       {132}
  SetRes(rRes, 720, 576, r16_9, r16_9_N, r16_9_D, s16_9, 'PAL Widescreen 1.46 Pixel Aspect Ratio');  {133}


  // 480p NTSC DV SD
  rRes := '480p NTSC DV SD/HD';

  SetRes(rRes, 640, 480, r4_3, r4_3_N, r4_3_D, s4_3, 'Square Pixels 1.0 SD');                              {134}
  SetRes(rRes, 720, 480, r4_3, r4_3_N, r4_3_D, s4_3, 'NTSC DV 0.91 Pixel Aspect Ratio SD');                {135}
  SetRes(rRes, 853, 480, r16_9, r16_9_N, r16_9_D, s16_9, 'Square Pixels 1.0 HD');                          {136}
  SetRes(rRes, 720, 480, r16_9, r16_9_N, r16_9_D, s16_9, 'NTSC DV Widescreen 1.21 Pixel Aspect Ratio HD'); {137}


  // 486p NTSC D1 SD
  rRes := '486p NTSC D1 SD/HD';

  SetRes(rRes, 648, 486, r4_3, r4_3_N, r4_3_D, s4_3, 'Square Pixels 1.0 SD');                              {138}
  SetRes(rRes, 720, 486, r4_3, r4_3_N, r4_3_D, s4_3, 'NTSC DV 0.91 Pixel Aspect Ratio SD');                {139}
  SetRes(rRes, 864, 486, r16_9, r16_9_N, r16_9_D, s16_9, 'Square Pixels 1.0 HD');                          {140}
  SetRes(rRes, 720, 486, r16_9, r16_9_N, r16_9_D, s16_9, 'NTSC DV Widescreen 1.21 Pixel Aspect Ratio HD'); {141}

end;


procedure TVideoStandardsCheat.SetFrameRates(iIndex: Integer;
                                             fRate: Double;
                                             Numerator: UINT32;
                                             Denominator: UINT32;
                                             sHint: string);
begin
  aFrameRatesArray[iIndex].FrameRate := fRate;
  aFrameRatesArray[iIndex].Numerator := Numerator;
  aFrameRatesArray[iIndex].Denominator := Denominator;
  aFrameRatesArray[iIndex].sFrameRate := Format('%n',
                                                [fRate]);
  aFrameRatesArray[iIndex].sHint := sHint;
end;


procedure TVideoStandardsCheat.GetFrameRates();
var
  i: Integer;
begin
  // Standard definitions for format versus framerate:
  // 240p: 18 fps.
  // 360p: 23.976 or 24 fps.
  // 480p: 23.976 or 24 fps.
  // 720p: 23.976 or 24 fps  HD Ready.
  // 1080p: 29.97 or 30 fps  Full HD.
  // 4k: 59.94 or 60 fps Ultra HD.
  // 5k: 59.94 or 60 fps (supported by action camera's like GoPro).
  // 8k: 59.94, 60 or 120 fps.

  SetLength(aFrameRatesArray,
            19);

  // FrameRate   Description
  // ===========================================================================
  //   8         Primitive animations.
  //  10         Primitive animations.
  //  12         Primitive animations.
  //  15         Primitive animations.
  //  16         Animations & silent movies
  //  18         Animations & silent movies
  //  23.976     Cinematic movies and television NTSC (intended color).
  //  24         Cinematic movies and television (intended b&w).
  //  25         European television standard.
  //  29.97      NTSC standard to support b&w TV.
  //  30         NTSC, live TV, sports and recording on smartphones.
  //  45         Cinematic movies, rarely used.
  //  48         Cinematic movies, rarely used.
  //  59.94      High definition cameras, limited slow motion purpose, compatible with NTSC (National Television System Committee).
  //  60         High definition cameras, games, sports, action footage and fast movements (limited slow motion purpose).
  //  90         Rarely used. Games, sports, action footage and fast movements (slow motion purpose).
  // 120         Games, sports, action footage and fast movements (slow motion purpose).
  // 240         Extremely slow motion footage and creating slow motion videos.
  // 300         Extremely slow motion footage and creating slow motion videos.

  i := 0;
  SetFrameRates(i,
                8,
                800,
                100,
                'Primitive animations.');
  Inc(i);
  SetFrameRates(i,
                10,
                1000,
                100,
                'Primitive animations.');

  Inc(i);
  SetFrameRates(i,
                12,
                1200,
                100,
                'Primitive animations.');

  Inc(i);
  SetFrameRates(i,
                15,
                1500,
                100,
                'Primitive animations.');

  Inc(i);
  SetFrameRates(i,
                16,
                1600,
                100,
                'Animations & silent movies.');

  Inc(i);
  SetFrameRates(i,
                18,
                1800,
                100,
                'Animations & silent movies.');

  Inc(i);
  SetFrameRates(i,
                23.976,
                23976,
                1000,
                'Cinematic movies and television NTSC (intended color).');

  Inc(i);
  SetFrameRates(i,
                24,
                2400,
                100,
                'Cinematic movies and television (intended b&w).');

  Inc(i);
  SetFrameRates(i,
                25,
                2500,
                100,
                'European television standard (PAL)');

  Inc(i);
  SetFrameRates(i,
                29.97,
                2997,
                100,
                'NTSC standard to support b&w TV.');

  Inc(i);
  SetFrameRates(i,
                30,
                3000,
                100,
                'NTSC, live TV, sports and recording on smartphones.');

  Inc(i);
  SetFrameRates(i,
                45,
                4500,
                100,
                'Cinematic movies, rarely used.');

  Inc(i);
  SetFrameRates(i,
                48,
                4800,
                100,
                'Cinematic movies, rarely used.');

  Inc(i);
  SetFrameRates(i,
                59.94,
                5994,
                100,
                'High definition cameras, limited slow motion purpose, compatible with NTSC (National Television System Committee).');

  Inc(i);
  SetFrameRates(i,
                60,
                6000,
                100,
                'High definition cameras, games, sports, action footage and fast movements (limited slow motion purpose).');

  Inc(i);
  SetFrameRates(i,
                90,
                9000,
                100,
                'Rarely used. Games, sports, action footage and fast movements (slow motion purpose).');

  Inc(i);
  SetFrameRates(i,
                120,
                12000,
                100,
                'Games, sports, action footage and fast movements (slow motion purpose).');

  Inc(i);
  SetFrameRates(i,
                240,
                24000,
                100,
                'Extremely slow motion footage and creating slow motion videos.');

  Inc(i);
  SetFrameRates(i,
                300,
                30000,
                100,
                'Extremely slow motion footage and creating slow motion videos.');

end;


function TVideoStandardsCheat.GetResolutionType(aWidth: UInt32;
                                                aHeight: UInt32): string;
var
  i: Integer;

begin
  if (Length(aResolutionArray) = 0) then
    begin
      Result := '';
      Exit;
    end;

  for i := 0 to Length(aResolutionArray) - 1 do
    begin
      if (aHeight = aResolutionArray[i].iHeight) then
        begin
          Result := aResolutionArray[i].Resolution;
          Break;
        end
      else if (aWidth = aResolutionArray[i].iWidth) then
        begin
          Result := aResolutionArray[i].Resolution;
          Break;
        end
      else if (aWidth = aResolutionArray[i].iWidth) and (aHeight = aResolutionArray[i].iHeight) then
        begin
          Result := aResolutionArray[i].Resolution;
          Break;
        end
      else
        Result := Format('Width: %d x Height: %d', [aWidth, aHeight]);
    end;
end;


procedure TVideoStandardsCheat.SetResolutionByIndex(index: Integer);
begin
  CopyMemory(@rResolution,
             @aResolutionArray[index],
             SizeOf(TResolution));
end;


procedure TVideoStandardsCheat.SetFrameRateByIndex(index: Integer);
begin
  CopyMemory(@rFrameRate,
             @aFrameRatesArray[index],
             SizeOf(TFrameRate));
end;

end.

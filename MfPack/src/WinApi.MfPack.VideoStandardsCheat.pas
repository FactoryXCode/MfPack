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
// Revision Version: 3.1.5
// Description: Video standards cheat.
//              This class contains the latest standardised video resolutions,
//              aspect ratios and framerates.
// Usage:
//      Call Create to create the class.
//      Call procedure GetResolutions() to get the resolutions and aspectratio's.
//      Call procedure GetFrameRates() to get the framerates.
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
// 15/11/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX315
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
    Resolution: ShortString;
    iWidth: Integer;
    iHeight: Integer;
    AspectRatio: Double;
    AspectRatioNumerator: Double;
    AspectRatioDenominator: Integer;
    StrAspectRatio: ShortString;
    Remarks: ShortString;
  end;

  TResolutionArray = array of TResolution;

  TFrameRate = record
    FrameRate: Double;
    sFrameRate: ShortString;
    sHint: ShortString;
  end;

  TFrameRatesArray = array of TFrameRate;

  /// <summary> This class stores most common resolutions, aspect ratios and framerates used today.</summary>
  TVideoStandardsCheat = class
  private
    iCounter: Integer;
    iCurrentResolution: Integer;
    iCurrentFrameRate: Integer;
    aResolutionArray: TResolutionArray;
    aFrameRatesArray: TFrameRatesArray;

    procedure SetRes(hResolution: ShortString;
                     hWidth: Integer;
                     hHeight: Integer;
                     hAspectRatio: Double;
                     hAspectRatioNumerator: Double;
                     hAspectRatioDenominator: Integer;
                     hStrAspectRatio: ShortString;
                     hRemarks: ShortString); inline;

    procedure SetFrameRates(iIndex: Integer;
                            fRate: Double;
                            sHint: ShortString); inline;

  public
    /// <summary> run myVideoStandardsCheat := TVideoStandardsCheat.Create. </summary>
    constructor Create();
    destructor Destroy(); override;
    /// <summary> Stores all resolutions and aspectratios in a public array (property Resolutions). </summary>
    procedure GetResolutions();
    /// <summary> Stores all framerates in a public array (property FrameRates). </summary>
    procedure GetFrameRates();

    property Resolutions: TResolutionArray read aResolutionArray write aResolutionArray;
    property CurrentResolution: Integer read iCurrentResolution write iCurrentResolution;
    property FrameRates: TFrameRatesArray read aFrameRatesArray write aFrameRatesArray;
    property CurrentFrameRate: Integer read iCurrentFrameRate write iCurrentFrameRate;

  end;


implementation


constructor TVideoStandardsCheat.Create();
begin
  inherited Create();
  CurrentResolution := 0;
end;


destructor TVideoStandardsCheat.Destroy();
begin
  SetLength(aResolutionArray,
            0);
  SetLength(aFrameRatesArray,
            0);
  inherited Destroy();
end;


procedure TVideoStandardsCheat.SetRes(hResolution: ShortString;
                                      hWidth: Integer;
                                      hHeight: Integer;
                                      hAspectRatio: Double;
                                      hAspectRatioNumerator: Double;
                                      hAspectRatioDenominator: Integer;
                                      hStrAspectRatio: ShortString;
                                      hRemarks: ShortString);
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
  rRes: ShortString;
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
  r5_3   = 1.66; // 5:3
  r16_10 = 1.60; // 16:10
  r16_9  = 1.78; // 16:9
  r1_85  = 1.85;
  r1_90  = 1.90;
  r1_94  = 1.94;
  r2_00  = 2.0;
  r2_35  = 2.35;
  r2_37  = 2.37; // 64:27
  r2_39  = 2.39;
  r2_40  = 2.40; // 21:9
  r2_44  = 2.44;

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

  SetRes(rRes, rWidth, 6144, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 4914, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 4608, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 4428, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 4320, r1_90, r1_90, 1, s1_90, 'RED 8K FF');
  SetRes(rRes, rWidth, 4096, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 3484, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 3456, r2_37, r2_37, 1, s2_37, 'RED 8K Wide');
  SetRes(rRes, rWidth, 3432, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 3412, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 3356, r2_44, r2_44, 1, s2_44, '');


  // 8K UHD
  rRes := '8K UHD';
  rWidth := 7680;

  SetRes(rRes, rWidth, 5760, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 4608, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 4320, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 4150, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 4042, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 3840, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 3268, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 3240, r2_37, r2_37, 1, s2_37, '');
  SetRes(rRes, rWidth, 3216, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 3200, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 3146, r2_44, r2_44, 1, s2_44, '');


  // 6K
  rRes := '6K';
  rWidth := 6144;

  SetRes(rRes, rWidth, 4608, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 3686, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 3456, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 3320, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 3232, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 3160, r1_94, r1_94, 1, s1_94, '6K Dragon FF');
  SetRes(rRes, rWidth, 3072, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 2614, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 2592, r2_37, r2_37, 1, s2_37, 'RED 6K Wide');
  SetRes(rRes, rWidth, 2574, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 2560, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 2518, r2_44, r2_44, 1, s2_44, '');


  // 5K
  rRes := '5K';
  rWidth := 5120;

  SetRes(rRes, rWidth, 3840, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 3072, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 2880, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 2766, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 2700, r1_90, r1_90, 1, s1_90, 'Epic-X FF');
  SetRes(rRes, rWidth, 2560, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 2178, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 2160, r2_37, r2_37, 1, s2_37, RED_Wide);
  SetRes(rRes, rWidth, 2144, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 2132, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 2098, r2_44, r2_44, 1, s2_44, '');



  // Cinema DCP 4K
  rRes := 'Cinema DCP 4K';

  SetRes(rRes, 3996, 2160, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, 4096, 1716, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, 4096, 2160, r1_90, r1_90, 1, s1_90, '');


  // 4K
  rRes := '4K';
  rWidth := 4096;

  SetRes(rRes, rWidth, 3072, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 2456, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 2304, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 2214, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 2160, r1_90, r1_90, 1, s1_90, '4K RED');
  SetRes(rRes, rWidth, 2048, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 1742, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 1728, r2_37, r2_37, 1, s64_27, RED_Wide);
  SetRes(rRes, rWidth, 1716, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 1706, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 1678, r2_44, r2_44, 1, s2_44, '');


  // 4K UHD
  rRes := '4K UHD';
  rWidth := 3840;

  SetRes(rRes, rWidth, 2880, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 2304, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 2160, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 2074, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 2020, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 1920, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 1634, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 1620, r2_37, 64, 27, s64_27, '');
  SetRes(rRes, rWidth, 1608, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 1600, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 1572, 2.44, r2_44, 1, s2_44, '');


  // 3K
  rRes := '3K';
  rWidth := 3072;

  SetRes(rRes, rWidth, 2304, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 1842, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 1728, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 1660, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 1620, r1_90, r1_90, 1, s1_90, '3K, RED');
  SetRes(rRes, rWidth, 1536, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 1306, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 1296, r2_37, 64, 27, s64_27, RED_Wide);
  SetRes(rRes, rWidth, 1286, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 1280, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 1258, r2_44, r2_44, 1, s2_44, '');


  // 3K HD
  rRes := '3K HD';
  rWidth := 2880;

  SetRes(rRes, rWidth, 2160, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 1728, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 1620, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 1556, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 1514, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 1440, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 1224, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 1214, r2_37, 64, 27, s64_27, '');
  SetRes(rRes, rWidth, 1206, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 1200, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 1180, r2_44, r2_44, 1, s2_44, '');



  // Cinema DCP 2K
  rRes := 'Cinema DCP 2K';

  SetRes(rRes, 1998, 1080, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, 2048, 858, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, 2048, 1080, r1_90, r1_90, 1, s1_90, '');


  // 2K
  rRes := '2K';
  rWidth := 2048;

  SetRes(rRes, rWidth, 1536, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 1228, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 1152, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 1106, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 1078, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 1024, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 870, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 864, r2_37, 64, 27, s64_27, '');
  SetRes(rRes, rWidth, 858, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 852, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 838, r2_44, r2_44, 1, s2_44, '');


  // 1080p
  rRes := '1080p';
  rWidth := 1920;

  SetRes(rRes, rWidth, 1440, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 1152, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 1080, r16_10, 16, 10, s16_10, '');
  SetRes(rRes, rWidth, 1080, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 1036, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 1010, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 960, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 816, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 810, r2_37, 64, 27, s64_27, '');
  SetRes(rRes, rWidth, 804, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 800, r2_40, 21, 9, s21_9, 'Blu-ray Scope');
  SetRes(rRes, rWidth, 786, r2_44, r2_44, 1, s2_44, '');


  // 720p
  rRes := '720p';
  rWidth := 1280;

  SetRes(rRes, rWidth, 962, r4_3, 4, 3, s4_3, '');
  SetRes(rRes, rWidth, 768, r5_3, 5, 3, s5_3, '');
  SetRes(rRes, rWidth, 720, r16_9, 16, 9, s16_9, '');
  SetRes(rRes, rWidth, 690, r1_85, r1_85, 1, s1_85, Cinema_Flat);
  SetRes(rRes, rWidth, 672, r1_90, r1_90, 1, s1_90, '');
  SetRes(rRes, rWidth, 640, r2_00, r2_00, 1, s2_00, '');
  SetRes(rRes, rWidth, 544, r2_35, r2_35, 1, s2_35, '');
  SetRes(rRes, rWidth, 540, r2_37, 64, 27, s64_27, '');
  SetRes(rRes, rWidth, 536, r2_39, r2_39, 1, s2_39, Cinema_Scope);
  SetRes(rRes, rWidth, 532, r2_40, 21, 9, s21_9, '');
  SetRes(rRes, rWidth, 524, r2_44, r2_44, 1, s2_44, '');


  // 576p PAL SD
  rRes := '576p PAL SD';

  SetRes(rRes, 768, 576, r4_3, 4, 3, s4_3, 'Square pixels 1.0');
  SetRes(rRes, 720, 576, r4_3, 4, 3, s4_3, 'PAL DV 1.09 pixel aspect ratio');
  SetRes(rRes, 102, 576, r16_9, 16, 9, s16_9, 'Square pixels 1.0');
  SetRes(rRes, 720, 576, r16_9, 16, 9, s16_9, 'PAL Widescreen 1.46 Pixel Aspect Ratio');


  // 480p NTSC DV SD
  rRes := '480p NTSC DV SD/HD';

  SetRes(rRes, 640, 480, r4_3, 4, 3, s4_3, 'Square Pixels 1.0 SD');
  SetRes(rRes, 720, 480, r4_3, 4, 3, s4_3, 'NTSC DV 0.91 Pixel Aspect Ratio SD');
  SetRes(rRes, 853, 480, r16_9, 16, 9, s16_9, 'Square Pixels 1.0 HD');
  SetRes(rRes, 720, 480, r16_9, 16, 9, s16_9, 'NTSC DV Widescreen 1.21 Pixel Aspect Ratio HD');


  // 486p NTSC D1 SD
  rRes := '486p NTSC D1 SD/HD';

  SetRes(rRes, 648, 486, r4_3, 4, 3, s4_3, 'Square Pixels 1.0 SD');
  SetRes(rRes, 720, 486, r4_3, 4, 3, s4_3, 'NTSC DV 0.91 Pixel Aspect Ratio SD');
  SetRes(rRes, 864, 486, r16_9, 16, 9, s16_9, 'Square Pixels 1.0 HD');
  SetRes(rRes, 720, 486, r16_9, 16, 9, s16_9, 'NTSC DV Widescreen 1.21 Pixel Aspect Ratio HD');

end;


procedure TVideoStandardsCheat.SetFrameRates(iIndex: Integer;
                                             fRate: Double;
                                             sHint: ShortString);
begin
  aFrameRatesArray[iIndex].FrameRate := fRate;
  aFrameRatesArray[iIndex].sFrameRate := ShortString(Format('%n',
                                                            [fRate]));
  aFrameRatesArray[iIndex].sHint := sHint;
end;


procedure TVideoStandardsCheat.GetFrameRates();
var
  i: Integer;
begin
  // Standard definitions for format versus framerate:
  // 240p: 18 fps.
  // 360p: 24 fps.
  // 480p: 24 fps.
  // 720p: 24 fps  HD Ready.
  // 1080p: 30 fps  Full HD.
  // 4k: 60 fps Ultra HD.
  // 5k: 60 fps (supported by action camera's like GoPro).
  // 8k: 60 or 120 fps.

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
                'Primitive animations.');
  Inc(i);
  SetFrameRates(i,
                10,
                'Primitive animations.');

  Inc(i);
  SetFrameRates(i,
                12,
                'Primitive animations.');

  Inc(i);
  SetFrameRates(i,
                15,
                'Primitive animations.');

  Inc(i);
  SetFrameRates(i,
                16,
                'Animations & silent movies.');

  Inc(i);
  SetFrameRates(i,
                18,
                'Animations & silent movies.');

  Inc(i);
  SetFrameRates(i,
                23.976,
                'Cinematic movies and television NTSC (intended color).');

  Inc(i);
  SetFrameRates(i,
                24,
                'Cinematic movies and television (intended b&w).');

  Inc(i);
  SetFrameRates(i,
                25,
                'European television standard (PAL)');

  Inc(i);
  SetFrameRates(i,
                29.97,
                'NTSC standard to support b&w TV.');

  Inc(i);
  SetFrameRates(i,
                30,
                'NTSC, live TV, sports and recording on smartphones.');

  Inc(i);
  SetFrameRates(i,
                45,
                'Cinematic movies, rarely used.');

  Inc(i);
  SetFrameRates(i,
                48,
                'Cinematic movies, rarely used.');

  Inc(i);
  SetFrameRates(i,
                59.94,
                'High definition cameras, limited slow motion purpose, compatible with NTSC (National Television System Committee).');

  Inc(i);
  SetFrameRates(i,
                60,
                'High definition cameras, games, sports, action footage and fast movements (limited slow motion purpose).');

  Inc(i);
  SetFrameRates(i,
                90,
                'Rarely used. Games, sports, action footage and fast movements (slow motion purpose).');

  Inc(i);
  SetFrameRates(i,
                120,
                'Games, sports, action footage and fast movements (slow motion purpose).');

  Inc(i);
  SetFrameRates(i,
                240,
                'Extremely slow motion footage and creating slow motion videos.');

  Inc(i);
  SetFrameRates(i,
                300,
                'Extremely slow motion footage and creating slow motion videos.');

end;

end.

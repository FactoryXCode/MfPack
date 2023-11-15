// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.AspectRatioCheat.pas
// Kind: Pascal / Delphi unit
// Release date: 14-11-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Aspect Ratio Cheat v2.2
//              This class contains the latest standardised video resolutions and their aspect ratios.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
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
// Source: See comments.
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
unit AspectRatioCheat;


interface

uses
  WinApi.Windows,
  System.Classes;

type

  TResolution = record
    Resolution: ShortString;
    iWidth: Integer;
    iHeight: Integer;
    AspectRatio: Single;
    AspectRatioNominator: Integer;
    AspectRatioDenominator: Integer;
    Scope: ShortString;
    Remarks: ShortString;
  end;

  TResolutionArray = array of TResolution;

  /// <summary> This class holds all common resolutions and their aspect ratios used today (v2.2). </summary>
  TAspectRatioCheat = class
    iCounter: Integer;
    //aResolution: TResolution;
    aResolutionArray: TResolutionArray;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure SetRes(hResolution: ShortString;
                     hWidth: Integer;
                     hHeight: Integer;
                     hAspectRatio: Single;
                     hAspectRatioNominator: Integer;
                     hAspectRatioDenominator: Integer;
                     hScope: ShortString;
                     hRemarks: ShortString); inline;

    procedure GetResolutions();

    property Resolutions: TResolutionArray read aResolutionArray write aResolutionArray;

  end;


implementation


constructor TAspectRatioCheat.Create();
begin
  inherited Create();
  iCounter := 0;
  SetLength(aResolutionArray,
            0);
end;


destructor TAspectRatioCheat.Destroy();
begin
  SetLength(aResolutionArray,
            0);
  inherited Destroy();
end;



procedure TAspectRatioCheat.SetRes(hResolution: ShortString;
                                   hWidth: Integer;
                                   hHeight: Integer;
                                   hAspectRatio: Single;
                                   hAspectRatioNominator: Integer;
                                   hAspectRatioDenominator: Integer;
                                   hScope: ShortString;
                                   hRemarks: ShortString);
begin
  aResolutionArray[iCounter].Resolution := hResolution;
  aResolutionArray[iCounter].iWidth := hWidth;
  aResolutionArray[iCounter].iHeight := hHeight;
  aResolutionArray[iCounter].AspectRatio:= hAspectRatio;
  aResolutionArray[iCounter].AspectRatioNominator := hAspectRatioNominator;
  aResolutionArray[iCounter].AspectRatioDenominator := hAspectRatioDenominator;
  aResolutionArray[iCounter].Scope := hScope;
  aResolutionArray[iCounter].Remarks := hRemarks;
  Inc(iCounter);
end;


procedure TAspectRatioCheat.GetResolutions();

var
  rRes: ShortString;
  rWidth: Integer;

const
  r4_3 = '4:3';
  r5_3 = '5:3';
  r16_9 = '16:9';
  r64_27 = '64:27';
  DCI_Scope = 'DCI Scope';
  RED_Wide = 'RED Wide';

begin
  SetLength(aResolutionArray,
            139);
  iCounter := 0;


  // 8K
  rRes := '8K';
  rWidth := 8192;

  SetRes(rRes, rWidth, 6144, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 4914, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 4608, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 4428, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 4320, 1.90, 0, 0, 'RED 8K FF', '');
  SetRes(rRes, rWidth, 4096, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 3484, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 3484, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 3456, 2.37, 0, 0, 'RED 8K Wide', '');
  SetRes(rRes, rWidth, 3432, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 3412, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 3356, 2.44, 0, 0, '', '');


  // 8K UHD
  rRes := '8K UHD';
  rWidth := 7680;

  SetRes(rRes, rWidth, 5760, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 4608, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 4320, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 4150, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 4042, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 3840, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 3268, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 3240, 2.37, 0, 0, '', '');
  SetRes(rRes, rWidth, 3216, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 3200, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 3146, 2.44, 0, 0, '', '');


  // 6K
  rRes := '6K';
  rWidth := 6144;

  SetRes(rRes, rWidth, 4608, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 3686, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 3456, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 3320, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 3232, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 3160, 1.94, 0, 0, '6K Dragon FF', '');
  SetRes(rRes, rWidth, 3072, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 2614, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 2592, 2.37, 0, 0, 'RED 6K Wide', '');
  SetRes(rRes, rWidth, 2574, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 2560, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 2518, 2.44, 0, 0, '', '');


  // 5K
  rRes := '5K';
  rWidth := 5120;

  SetRes(rRes, rWidth, 3840, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 3072, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 2880, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 2766, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 2700, 1.90, 0, 0, 'Epic-X FF', '');
  SetRes(rRes, rWidth, 2560, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 2178, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 2160, 2.37, 0, 0, RED_Wide, '');
  SetRes(rRes, rWidth, 2144, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 2132, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 2098, 2.44, 0, 0, '', '');


  // Cinema DCP 4K
  rRes := 'Cinema DCP 4K';

  SetRes(rRes, 3996, 2160, 1.85, 0, 0, '', '');
  SetRes(rRes, 4096, 1716, 2.39, 0, 0, '', '');
  SetRes(rRes, 4096, 2160, 1.90, 0, 0, '', '');


  // 4K
  rRes := '4K';
  rWidth := 4096;

  SetRes(rRes, rWidth, 3072, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 2456, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 2304, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 2214, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 2160, 1.90, 0, 0, '4K RED', '');
  SetRes(rRes, rWidth, 2048, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 1742, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 1728, 2.37, 0, 0, RED_Wide, '');
  SetRes(rRes, rWidth, 1716, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 1706, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 1678, 2.44, 0, 0, '', '');


  // 4K/UHD
  rRes := '4K UHD';
  rWidth := 3840;

  SetRes(rRes, rWidth, 2880, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 2304, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 2160, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 2074, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 2020, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 1920, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 1634, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 1620, 2.37, 64, 27, r64_27, '');
  SetRes(rRes, rWidth, 1608, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 1600, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 1572, 2.44, 0, 0, '', '');


  // 3K
  rRes := '3K';
  rWidth := 3072;


  SetRes(rRes, rWidth, 2304, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 1842, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 1728, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 1660, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 1620, 1.90, 0, 0, '3K, RED', '');
  SetRes(rRes, rWidth, 1536, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 1306, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 1296, 2.37, 0, 0, RED_Wide, '');
  SetRes(rRes, rWidth, 1286, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 1280, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 1258, 2.44, 0, 0, '', '');


  // 3K HD
  rRes := '3K HD';
  rWidth := 2880;

  SetRes(rRes, rWidth, 2160, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 1728, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 1620, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 1556, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 1514, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 1440, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 1556, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 1514, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 1440, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 1224, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 1214, 2.37, 64, 27, r64_27, '');
  SetRes(rRes, rWidth, 1206, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 1200, 2.40, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 1180, 2.44, 0, 0, DCI_Scope, '');


  // Cinema DCP 2K
  rRes := 'Cinema DCP 2K';

  SetRes(rRes, 1998, 1080, 1.85, 0, 0, '', '');
  SetRes(rRes, 2048, 858, 2.39, 0, 0, '', '');
  SetRes(rRes, 2048, 1080, 1.90, 0, 0, '', '');


  // 2K
  rRes := '2K';
  rWidth := 2048;

  SetRes(rRes, rWidth, 1536, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 1228, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 1152, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 1106, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 1078, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 1024, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 870, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 864, 2.37, 64, 27, r64_27, '');
  SetRes(rRes, rWidth, 858, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 852, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 838, 2.44, 0, 0, '', '');


  // 1080p
  rRes := '1080p';
  rWidth := 1920;

  SetRes(rRes, rWidth, 1440, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 1152, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 1080, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 1036, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 1010, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 960, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 816, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 810, 2.37, 64, 27, r64_27, '');
  SetRes(rRes, rWidth, 804, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 800, 2.40, 0, 0, 'Blue Ray Scope', '');
  SetRes(rRes, rWidth, 786, 2.44, 0, 0, '', '');


  // 720p
  rRes := '720p';
  rWidth := 1280;

  SetRes(rRes, rWidth, 962, 1.33, 4, 3, r4_3, '');
  SetRes(rRes, rWidth, 768, 1.66, 5, 3, r5_3, '');
  SetRes(rRes, rWidth, 720, 1.78, 16, 9, r16_9, '');
  SetRes(rRes, rWidth, 690, 1.85, 0, 0, '', '');
  SetRes(rRes, rWidth, 672, 1.90, 0, 0, '', '');
  SetRes(rRes, rWidth, 640, 2.00, 0, 0, '', '');
  SetRes(rRes, rWidth, 544, 2.35, 0, 0, '', '');
  SetRes(rRes, rWidth, 540, 2.37, 64, 27, r64_27, '');
  SetRes(rRes, rWidth, 536, 2.39, 0, 0, DCI_Scope, '');
  SetRes(rRes, rWidth, 532, 2.40, 0, 0, '', '');
  SetRes(rRes, rWidth, 524, 2.44, 0, 0, '', '');


  // 576p PAL SD
  rRes := '576p PAL SD';

  SetRes(rRes, 768, 576, 1.33, 4, 3, r4_3, 'Square pixels 1.0');
  SetRes(rRes, 720, 576, 1.33, 4, 3, r4_3, 'PAL DV 1.09 pixel aspect ratio');
  SetRes(rRes, 102, 576, 1.78, 16, 9, r16_9, 'Square pixels 1.0');
  SetRes(rRes, 720, 576, 1.78, 16, 9, r16_9, 'PAL Widescreen 1.46 Pixel Aspect Ratio');


  // 480p NTSC DV SD
  rRes := '480p NTSC DV SD';

  SetRes(rRes, 640, 480, 1.33, 4, 3, r4_3, 'Square Pixels 1.0');
  SetRes(rRes, 720, 480, 1.33, 4, 3, r4_3, 'NTSC DV 0.91 Pixel Aspect Ratio');
  SetRes(rRes, 853, 480, 1.78, 16, 9,	r16_9, 'Square Pixels 1.0');
  SetRes(rRes, 720, 480, 1.78, 16, 9,	r16_9, 'NTSC DV Widescreen 1.21 Pixel Aspect Ratio');


  // 486p NTSC D1 SD
  rRes := '486p NTSC D1 SD';

  SetRes(rRes, 648, 486, 1.33, 4, 3, r4_3, 'Square Pixels 1.0');
  SetRes(rRes, 720, 486, 1.33, 4, 3, r4_3, 'NTSC DV 0.91 Pixel Aspect Ratio');
  SetRes(rRes, 864, 486, 1.78, 16, 9, r16_9, 'Square Pixels 1.0');
  SetRes(rRes, 720, 486, 1.78, 16, 9, r16_9, 'NTSC DV Widescreen 1.21 Pixel Aspect Ratio');

end;

end.

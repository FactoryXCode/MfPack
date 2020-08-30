// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1Effects_2.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description:  Image effects parts of the Direct2D API for Windows 8 and later.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: d2d1effects_2.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit WinApi.DirectX.D2D1Effects_2;

  {$HPPEMIT '#include "d2d1effects_2.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {WinApi.DirectX}
  WinApi.DirectX.D2D1Effects_1;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}


const

  // Built in effect CLSIDs

  CLSID_D2D1Contrast                 : TGUID = '{b648a78a-0ed5-4f80-a94a-8e825aca6b77}';
  {$EXTERNALSYM CLSID_D2D1Contrast}
  CLSID_D2D1RgbToHue                 : TGUID = '{23f3e5ec-91e8-4d3d-ad0a-afadc1004aa1}';
  {$EXTERNALSYM CLSID_D2D1RgbToHue}
  CLSID_D2D1HueToRgb                 : TGUID = '{7b78a6bd-0141-4def-8a52-6356ee0cbdd5}';
  {$EXTERNALSYM CLSID_D2D1HueToRgb}
  CLSID_D2D1ChromaKey                : TGUID = '{74C01F5B-2A0D-408C-88E2-C7A3C7197742}';
  {$EXTERNALSYM CLSID_D2D1ChromaKey}
  CLSID_D2D1Emboss                   : TGUID = '{b1c5eb2b-0348-43f0-8107-4957cacba2ae}';
  {$EXTERNALSYM CLSID_D2D1Emboss}
  CLSID_D2D1Exposure                 : TGUID = '{b56c8cfa-f634-41ee-bee0-ffa617106004}';
  {$EXTERNALSYM CLSID_D2D1Exposure}
  CLSID_D2D1Grayscale                : TGUID = '{36DDE0EB-3725-42E0-836D-52FB20AEE644}';
  {$EXTERNALSYM CLSID_D2D1Grayscale}
  CLSID_D2D1Invert                   : TGUID = '{e0c3784d-cb39-4e84-b6fd-6b72f0810263}';
  {$EXTERNALSYM CLSID_D2D1Invert}
  CLSID_D2D1Posterize                : TGUID = '{2188945e-33a3-4366-b7bc-086bd02d0884}';
  {$EXTERNALSYM CLSID_D2D1Posterize}
  CLSID_D2D1Sepia                    : TGUID = '{3a1af410-5f1d-4dbe-84df-915da79b7153}';
  {$EXTERNALSYM CLSID_D2D1Sepia}
  CLSID_D2D1Sharpen                  : TGUID = '{C9B887CB-C5FF-4DC5-9779-273DCF417C7D}';
  {$EXTERNALSYM CLSID_D2D1Sharpen}
  CLSID_D2D1Straighten               : TGUID = '{4da47b12-79a3-4fb0-8237-bbc3b2a4de08}';
  {$EXTERNALSYM CLSID_D2D1Straighten}
  CLSID_D2D1TemperatureTint          : TGUID = '{89176087-8AF9-4A08-AEB1-895F38DB1766}';
  {$EXTERNALSYM CLSID_D2D1TemperatureTint}
  CLSID_D2D1Vignette                 : TGUID = '{c00c40be-5e67-4ca3-95b4-f4b02c115135}';
  {$EXTERNALSYM CLSID_D2D1Vignette}
  CLSID_D2D1EdgeDetection            : TGUID = '{EFF583CA-CB07-4AA9-AC5D-2CC44C76460F}';
  {$EXTERNALSYM CLSID_D2D1EdgeDetection}
  CLSID_D2D1HighlightsShadows        : TGUID = '{CADC8384-323F-4C7E-A361-2E2B24DF6EE4}';
  {$EXTERNALSYM CLSID_D2D1HighlightsShadows}
  CLSID_D2D1LookupTable3D            : TGUID = '{349E0EDA-0088-4A79-9CA3-C7E300202020}';
  {$EXTERNALSYM CLSID_D2D1LookupTable3D}

//#if NTDDI_VERSION >= NTDDI_WIN10_RS1

  CLSID_D2D1Opacity                  : TGUID = '{811d79a4-de28-4454-8094-c64685f8bd4c}';
  {$EXTERNALSYM CLSID_D2D1Opacity}
  CLSID_D2D1AlphaMask                : TGUID = '{c80ecff0-3fd5-4f05-8328-c5d1724b4f0a}';
  {$EXTERNALSYM CLSID_D2D1AlphaMask}
  CLSID_D2D1CrossFade                : TGUID = '{12f575e8-4db1-485f-9a84-03a07dd3829f}';
  {$EXTERNALSYM CLSID_D2D1CrossFade}
  CLSID_D2D1Tint                     : TGUID = '{36312b17-f7dd-4014-915d-ffca768cf211}';
  {$EXTERNALSYM CLSID_D2D1Tint}

//#endif // #if NTDDI_VERSION >= NTDDI_WIN10_RS1

  // The number of nits that sRGB or scRGB color space uses for SDR white, or
  // floating point values of 1.0f. Note that this value is only constant when the
  // color space uses scene-referred luminance, which is true for HDR content. If
  // the color space uses display-referred luminance instead, then the SDR white
  // level should be queried from the display.
  D2D1_SCENE_REFERRED_SDR_WHITE_LEVEL = 80.0;
  {$EXTERNALSYM D2D1_SCENE_REFERRED_SDR_WHITE_LEVEL}

//#if NTDDI_VERSION >= NTDDI_WIN10_RS5

  CLSID_D2D1WhiteLevelAdjustment      : TGUID = '{44a1cadb-6cdd-4818-8ff4-26c1cfe95bdb}';
  {$EXTERNALSYM CLSID_D2D1WhiteLevelAdjustment}
  CLSID_D2D1HdrToneMap                : TGUID = '{7b0b748d-4610-4486-a90c-999d9a2e2b11}';
  {$EXTERNALSYM CLSID_D2D1HdrToneMap}

//#endif // #if NTDDI_VERSION >= NTDDI_WIN10_RS5


type
  // The enumeration of the Contrast effect's top level properties.
  // Effect description: Adjusts the contrast of an image.
  PD2D1_CONTRAST_PROP = ^D2D1_CONTRAST_PROP;
  D2D1_CONTRAST_PROP = DWord;
  {$EXTERNALSYM D2D1_CONTRAST_PROP}
const
  // Property Name: "Contrast"
  // Property Type: FLOAT
  D2D1_CONTRAST_PROP_CONTRAST    = D2D1_CONTRAST_PROP(0);
  {$EXTERNALSYM D2D1_CONTRAST_PROP_CONTRAST}
  // Property Name: "ClampInput"
  // Property Type: BOOL
  D2D1_CONTRAST_PROP_CLAMP_INPUT = D2D1_CONTRAST_PROP(1);
  {$EXTERNALSYM D2D1_CONTRAST_PROP_CLAMP_INPUT}
  //D2D1_CONTRAST_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the RgbToHue effect's top level properties.
  // Effect description: Converts an RGB bitmap to HSV or HSL.
  PD2D1_RGBTOHUE_PROP = ^D2D1_RGBTOHUE_PROP;
  D2D1_RGBTOHUE_PROP = DWord;
  {$EXTERNALSYM D2D1_RGBTOHUE_PROP}
const
  // Property Name: "OutputColorSpace"
  // Property Type: D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE
  D2D1_RGBTOHUE_PROP_OUTPUT_COLOR_SPACE = D2D1_RGBTOHUE_PROP(0);
  {$EXTERNALSYM D2D1_RGBTOHUE_PROP_OUTPUT_COLOR_SPACE}
  //D2D1_RGBTOHUE_PROP_FORCE_DWORD    = FORCEDWORD;

type
  PD2D1_RGBTOHUE_OUTPUT_COLOR_SPACE = ^D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE;
  D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE = DWord;
  {$EXTERNALSYM D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE}
const
  D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE_HUE_SATURATION_VALUE     = D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE(0);
  {$EXTERNALSYM D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE_HUE_SATURATION_LIGHTNESS}
  D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE_HUE_SATURATION_LIGHTNESS = D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE(1);
  {$EXTERNALSYM D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE_HUE_SATURATION_VALUE}
  //D2D1_RGBTOHUE_OUTPUT_COLOR_SPACE_FORCE_DWORD        = FORCEDWORD;

type
  // The enumeration of the HueToRgb effect's top level properties.
  // Effect description: Converts an HSV or HSL bitmap into an RGB bitmap.
  PD2D1_HUETORGB_PROP = ^D2D1_HUETORGB_PROP;
  D2D1_HUETORGB_PROP = DWord;
  {$EXTERNALSYM D2D1_HUETORGB_PROP}
const
  // Property Name: "InputColorSpace"
  // Property Type: D2D1_HUETORGB_INPUT_COLOR_SPACE
  D2D1_HUETORGB_PROP_INPUT_COLOR_SPACE = D2D1_HUETORGB_PROP(0);
  {$EXTERNALSYM D2D1_HUETORGB_PROP_INPUT_COLOR_SPACE}
  //D2D1_HUETORGB_PROP_FORCE_DWORD     = FORCEDWORD;

type
  PD2D1_HUETORGB_INPUT_COLOR_SPACE = ^D2D1_HUETORGB_INPUT_COLOR_SPACE;
  D2D1_HUETORGB_INPUT_COLOR_SPACE = DWord;
  {$EXTERNALSYM D2D1_HUETORGB_INPUT_COLOR_SPACE}
const
  D2D1_HUETORGB_INPUT_COLOR_SPACE_HUE_SATURATION_VALUE   = D2D1_HUETORGB_INPUT_COLOR_SPACE(0);
  {$EXTERNALSYM D2D1_HUETORGB_INPUT_COLOR_SPACE_HUE_SATURATION_VALUE}
  D2D1_HUETORGB_INPUT_COLOR_SPACE_HUE_SATURATION_LIGHTNESS = D2D1_HUETORGB_INPUT_COLOR_SPACE(1);
  {$EXTERNALSYM D2D1_HUETORGB_INPUT_COLOR_SPACE_HUE_SATURATION_LIGHTNESS}
  //D2D1_HUETORGB_INPUT_COLOR_SPACE_FORCE_DWORD        = FORCEDWORD;

type
  // The enumeration of the Chroma Key effect's top level properties.
  // Effect description: Converts a specified color to transparent.
  PD2D1_CHROMAKEY_PROP = ^D2D1_CHROMAKEY_PROP;
  D2D1_CHROMAKEY_PROP = DWord;
  {$EXTERNALSYM D2D1_CHROMAKEY_PROP}
const
  // Property Name: "Color"
  // Property Type: D2D1_VECTOR_3F
  D2D1_CHROMAKEY_PROP_COLOR    = D2D1_CHROMAKEY_PROP(0);
  {$EXTERNALSYM D2D1_CHROMAKEY_PROP_COLOR}
  // Property Name: "Tolerance"
  // Property Type: FLOAT
  D2D1_CHROMAKEY_PROP_TOLERANCE  = D2D1_CHROMAKEY_PROP(1);
  {$EXTERNALSYM D2D1_CHROMAKEY_PROP_TOLERANCE}
  // Property Name: "InvertAlpha"
  // Property Type: BOOL
  D2D1_CHROMAKEY_PROP_INVERT_ALPHA = D2D1_CHROMAKEY_PROP(2);
  {$EXTERNALSYM D2D1_CHROMAKEY_PROP_INVERT_ALPHA}
  // Property Name: "Feather"
  // Property Type: BOOL
  D2D1_CHROMAKEY_PROP_FEATHER    = D2D1_CHROMAKEY_PROP(3);
  {$EXTERNALSYM D2D1_CHROMAKEY_PROP_FEATHER}
  //D2D1_CHROMAKEY_PROP_FORCE_DWORD  = FORCEDWORD;

type
  // The enumeration of the Emboss effect's top level properties.
  // Effect description: Applies an embossing effect to an image.
  PD2D1_EMBOSS_PROP = ^D2D1_EMBOSS_PROP;
  D2D1_EMBOSS_PROP         = DWord;
  {$EXTERNALSYM D2D1_EMBOSS_PROP}
const
  // Property Name: "Height"
  // Property Type: FLOAT
  D2D1_EMBOSS_PROP_HEIGHT    = D2D1_EMBOSS_PROP(0);
  // Property Name: "Direction"
  // Property Type: FLOAT
  D2D1_EMBOSS_PROP_DIRECTION   = D2D1_EMBOSS_PROP(1);
  //D2D1_EMBOSS_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Exposure effect's top level properties.
  // Effect description: Simulates camera exposure adjustment.
  PD2D1_EXPOSURE_PROP = ^D2D1_EXPOSURE_PROP;
  D2D1_EXPOSURE_PROP = DWord;
  {$EXTERNALSYM D2D1_EXPOSURE_PROP}
const
  // Property Name: "ExposureValue"
  // Property Type: FLOAT
  D2D1_EXPOSURE_PROP_EXPOSURE_VALUE = D2D1_EXPOSURE_PROP(0);
  {$EXTERNALSYM D2D1_EXPOSURE_PROP_EXPOSURE_VALUE}
  //D2D1_EXPOSURE_PROP_FORCE_DWORD  = FORCEDWORD;

type
  // The enumeration of the Posterize effect's top level properties.
  // Effect description: Reduces the number of colors in an image.
  PD2D1_POSTERIZE_PROP = ^D2D1_POSTERIZE_PROP;
  D2D1_POSTERIZE_PROP = DWord;
  {$EXTERNALSYM D2D1_POSTERIZE_PROP}
const
  // Property Name: "RedValueCount"
  // Property Type: UINT32
  D2D1_POSTERIZE_PROP_RED_VALUE_COUNT   = D2D1_POSTERIZE_PROP(0);
  {$EXTERNALSYM D2D1_POSTERIZE_PROP_RED_VALUE_COUNT}
  // Property Name: "GreenValueCount"
  // Property Type: UINT32
  D2D1_POSTERIZE_PROP_GREEN_VALUE_COUNT = D2D1_POSTERIZE_PROP(1);
  {$EXTERNALSYM D2D1_POSTERIZE_PROP_GREEN_VALUE_COUNT}
  // Property Name: "BlueValueCount"
  // Property Type: UINT32
  D2D1_POSTERIZE_PROP_BLUE_VALUE_COUNT  = D2D1_POSTERIZE_PROP(2);
  {$EXTERNALSYM D2D1_POSTERIZE_PROP_BLUE_VALUE_COUNT}
  //D2D1_POSTERIZE_PROP_FORCE_DWORD     = FORCEDWORD;

type
  // The enumeration of the Sepia effect's top level properties.
  // Effect description: Applies a Sepia tone to an image.
  PD2D1_SEPIA_PROP = ^D2D1_SEPIA_PROP;
  D2D1_SEPIA_PROP = DWord;
  {$EXTERNALSYM D2D1_SEPIA_PROP}
const
  // Property Name: "Intensity"
  // Property Type: FLOAT
  D2D1_SEPIA_PROP_INTENSITY   = D2D1_SEPIA_PROP(0);
  {$EXTERNALSYM D2D1_SEPIA_PROP_INTENSITY}
  // Property Name: "AlphaMode"
  // Property Type: D2D1_ALPHA_MODE
  D2D1_SEPIA_PROP_ALPHA_MODE  = D2D1_SEPIA_PROP(1);
  {$EXTERNALSYM D2D1_SEPIA_PROP_ALPHA_MODE}
  //D2D1_SEPIA_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Sharpen effect's top level properties.
  // Effect description: Performs sharpening adjustment
  PD2D1_SHARPEN_PROP = ^D2D1_SHARPEN_PROP;
  D2D1_SHARPEN_PROP = DWord;
  {$EXTERNALSYM D2D1_SHARPEN_PROP}
const
  // Property Name: "Sharpness"
  // Property Type: FLOAT
  D2D1_SHARPEN_PROP_SHARPNESS   = D2D1_SHARPEN_PROP(0);
  {$EXTERNALSYM D2D1_SHARPEN_PROP_SHARPNESS}
  // Property Name: "Threshold"
  // Property Type: FLOAT
  D2D1_SHARPEN_PROP_THRESHOLD   = D2D1_SHARPEN_PROP(1);
  {$EXTERNALSYM D2D1_SHARPEN_PROP_THRESHOLD}
  //D2D1_SHARPEN_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Straighten effect's top level properties.
  // Effect description: Straightens an image.
  PD2D1_STRAIGHTEN_PROP = ^D2D1_STRAIGHTEN_PROP;
  D2D1_STRAIGHTEN_PROP = DWord;
  {$EXTERNALSYM D2D1_STRAIGHTEN_PROP}
const
  // Property Name: "Angle"
  // Property Type: FLOAT
  D2D1_STRAIGHTEN_PROP_ANGLE     = D2D1_STRAIGHTEN_PROP(0);
  {$EXTERNALSYM D2D1_STRAIGHTEN_PROP_ANGLE}
  // Property Name: "MaintainSize"
  // Property Type: BOOL
  D2D1_STRAIGHTEN_PROP_MAINTAIN_SIZE = D2D1_STRAIGHTEN_PROP(1);
  {$EXTERNALSYM D2D1_STRAIGHTEN_PROP_MAINTAIN_SIZE}
  // Property Name: "ScaleMode"
  // Property Type: D2D1_STRAIGHTEN_SCALE_MODE
  D2D1_STRAIGHTEN_PROP_SCALE_MODE  = D2D1_STRAIGHTEN_PROP(2);
  {$EXTERNALSYM D2D1_STRAIGHTEN_PROP_SCALE_MODE}
  //D2D1_STRAIGHTEN_PROP_FORCE_DWORD   = FORCEDWORD;

type
  PD2D1_STRAIGHTEN_SCALE_MODE = ^D2D1_STRAIGHTEN_SCALE_MODE;
  D2D1_STRAIGHTEN_SCALE_MODE = DWord;
  {$EXTERNALSYM D2D1_STRAIGHTEN_SCALE_MODE}
const
  D2D1_STRAIGHTEN_SCALE_MODE_NEAREST_NEIGHBOR  = D2D1_STRAIGHTEN_SCALE_MODE(0);
  {$EXTERNALSYM D2D1_STRAIGHTEN_SCALE_MODE_NEAREST_NEIGHBOR}
  D2D1_STRAIGHTEN_SCALE_MODE_LINEAR        = D2D1_STRAIGHTEN_SCALE_MODE(1);
  {$EXTERNALSYM D2D1_STRAIGHTEN_SCALE_MODE_LINEAR}
  D2D1_STRAIGHTEN_SCALE_MODE_CUBIC         = D2D1_STRAIGHTEN_SCALE_MODE(2);
  {$EXTERNALSYM D2D1_STRAIGHTEN_SCALE_MODE_CUBIC}
  D2D1_STRAIGHTEN_SCALE_MODE_MULTI_SAMPLE_LINEAR = D2D1_STRAIGHTEN_SCALE_MODE(3);
  {$EXTERNALSYM D2D1_STRAIGHTEN_SCALE_MODE_MULTI_SAMPLE_LINEAR}
  D2D1_STRAIGHTEN_SCALE_MODE_ANISOTROPIC     = D2D1_STRAIGHTEN_SCALE_MODE(4);
  {$EXTERNALSYM D2D1_STRAIGHTEN_SCALE_MODE_ANISOTROPIC}
  //D2D1_STRAIGHTEN_SCALE_MODE_FORCE_DWORD     = FORCEDWORD;

type
  // The enumeration of the Temperature And Tint effect's top level properties.
  // Effect description: Adjusts the temperature and tint of an image.
  PD2D1_TEMPERATUREANDTINT_PROP = ^D2D1_TEMPERATUREANDTINT_PROP;
  D2D1_TEMPERATUREANDTINT_PROP = DWord;
  {$EXTERNALSYM D2D1_TEMPERATUREANDTINT_PROP}
const
  // Property Name: "Temperature"
  // Property Type: FLOAT
  D2D1_TEMPERATUREANDTINT_PROP_TEMPERATURE = D2D1_TEMPERATUREANDTINT_PROP(0);
  {$EXTERNALSYM D2D1_TEMPERATUREANDTINT_PROP_TEMPERATURE}
  // Property Name: "Tint"
  // Property Type: FLOAT
  D2D1_TEMPERATUREANDTINT_PROP_TINT    = D2D1_TEMPERATUREANDTINT_PROP(1);
  {$EXTERNALSYM D2D1_TEMPERATUREANDTINT_PROP_TINT}
  //D2D1_TEMPERATUREANDTINT_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Vignette effect's top level properties.
  // Effect description: Fades the edges of an image to the specified color.
  PD2D1_VIGNETTE_PROP = ^D2D1_VIGNETTE_PROP;
  D2D1_VIGNETTE_PROP = DWord;
  {$EXTERNALSYM D2D1_VIGNETTE_PROP}
const
  // Property Name: "Color"
  // Property Type: D2D1_VECTOR_4F
  D2D1_VIGNETTE_PROP_COLOR       = D2D1_VIGNETTE_PROP(0);
  {$EXTERNALSYM D2D1_VIGNETTE_PROP_COLOR}
  // Property Name: "TransitionSize"
  // Property Type: FLOAT
  D2D1_VIGNETTE_PROP_TRANSITION_SIZE = D2D1_VIGNETTE_PROP(1);
  {$EXTERNALSYM D2D1_VIGNETTE_PROP_TRANSITION_SIZE}
  // Property Name: "Strength"
  // Property Type: FLOAT
  D2D1_VIGNETTE_PROP_STRENGTH    = D2D1_VIGNETTE_PROP(2);
  {$EXTERNALSYM D2D1_VIGNETTE_PROP_STRENGTH}
  //D2D1_VIGNETTE_PROP_FORCE_DWORD   = FORCEDWORD;

type
  // The enumeration of the Edge Detection effect's top level properties.
  // Effect description: Detects edges of an image.
  PD2D1_EDGEDETECTION_PROP = ^D2D1_EDGEDETECTION_PROP;
  D2D1_EDGEDETECTION_PROP = DWord;
  {$EXTERNALSYM D2D1_EDGEDETECTION_PROP}
const
  // Property Name: "Strength"
  // Property Type: FLOAT
  D2D1_EDGEDETECTION_PROP_STRENGTH    = D2D1_EDGEDETECTION_PROP(0);
  {$EXTERNALSYM D2D1_EDGEDETECTION_PROP_STRENGTH}
  // Property Name: "BlurRadius"
  // Property Type: FLOAT
  D2D1_EDGEDETECTION_PROP_BLUR_RADIUS   = D2D1_EDGEDETECTION_PROP(1);
  {$EXTERNALSYM D2D1_EDGEDETECTION_PROP_BLUR_RADIUS}
  // Property Name: "Mode"
  // Property Type: D2D1_EDGEDETECTION_MODE
  D2D1_EDGEDETECTION_PROP_MODE      = D2D1_EDGEDETECTION_PROP(2);
  {$EXTERNALSYM D2D1_EDGEDETECTION_PROP_MODE}
  // Property Name: "OverlayEdges"
  // Property Type: BOOL
  D2D1_EDGEDETECTION_PROP_OVERLAY_EDGES = D2D1_EDGEDETECTION_PROP(3);
  {$EXTERNALSYM D2D1_EDGEDETECTION_PROP_OVERLAY_EDGES}
  // Property Name: "AlphaMode"
  // Property Type: D2D1_ALPHA_MODE
  D2D1_EDGEDETECTION_PROP_ALPHA_MODE  = D2D1_EDGEDETECTION_PROP(4);
  {$EXTERNALSYM D2D1_EDGEDETECTION_PROP_ALPHA_MODE}
  //D2D1_EDGEDETECTION_PROP_FORCE_DWORD   = FORCEDWORD;

type
  PD2D1_EDGEDETECTION_MODE = ^D2D1_EDGEDETECTION_MODE;
  D2D1_EDGEDETECTION_MODE = DWord;
  {$EXTERNALSYM D2D1_EDGEDETECTION_MODE}
const
  D2D1_EDGEDETECTION_MODE_SOBEL     = D2D1_EDGEDETECTION_MODE(0);
  {$EXTERNALSYM D2D1_EDGEDETECTION_MODE_SOBEL}
  D2D1_EDGEDETECTION_MODE_PREWITT   = D2D1_EDGEDETECTION_MODE(1);
  {$EXTERNALSYM D2D1_EDGEDETECTION_MODE_PREWITT}
  //D2D1_EDGEDETECTION_MODE_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Highlights and Shadows effect's top level properties.
  // Effect description: Adjusts the highlight and shadow strength of an image.
  PD2D1_HIGHLIGHTSANDSHADOWS_PROP = ^D2D1_HIGHLIGHTSANDSHADOWS_PROP;
  D2D1_HIGHLIGHTSANDSHADOWS_PROP = DWord;
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_PROP}
const
  // Property Name: "Highlights"
  // Property Type: FLOAT
  D2D1_HIGHLIGHTSANDSHADOWS_PROP_HIGHLIGHTS     = D2D1_HIGHLIGHTSANDSHADOWS_PROP(0);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_PROP_HIGHLIGHTS}
  // Property Name: "Shadows"
  // Property Type: FLOAT
  D2D1_HIGHLIGHTSANDSHADOWS_PROP_SHADOWS      = D2D1_HIGHLIGHTSANDSHADOWS_PROP(1);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_PROP_SHADOWS}
  // Property Name: "Clarity"
  // Property Type: FLOAT
  D2D1_HIGHLIGHTSANDSHADOWS_PROP_CLARITY      = D2D1_HIGHLIGHTSANDSHADOWS_PROP(2);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_PROP_CLARITY}
  // Property Name: "InputGamma"
  // Property Type: D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA
  D2D1_HIGHLIGHTSANDSHADOWS_PROP_INPUT_GAMMA    = D2D1_HIGHLIGHTSANDSHADOWS_PROP(3);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_PROP_INPUT_GAMMA}
  // Property Name: "MaskBlurRadius"
  // Property Type: FLOAT
  D2D1_HIGHLIGHTSANDSHADOWS_PROP_MASK_BLUR_RADIUS = D2D1_HIGHLIGHTSANDSHADOWS_PROP(4);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_PROP_MASK_BLUR_RADIUS}
  //D2D1_HIGHLIGHTSANDSHADOWS_PROP_FORCE_DWORD    = FORCEDWORD;

type
  PD2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA = ^D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA;
  D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA = DWord;
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA}
const
  D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_LINEAR    = D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA(0);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_LINEAR}
  D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_SRGB    = D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA(1);
  {$EXTERNALSYM D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_SRGB}
  //D2D1_HIGHLIGHTSANDSHADOWS_INPUT_GAMMA_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Lookup Table 3D effect's top level properties.
  // Effect description: Remaps colors in an image via a 3D lookup table.
  PD2D1_LOOKUPTABLE3D_PROP = ^D2D1_LOOKUPTABLE3D_PROP;
  D2D1_LOOKUPTABLE3D_PROP = DWord;
  {$EXTERNALSYM D2D1_LOOKUPTABLE3D_PROP}
const
  // Property Name: "Lut"
  // Property Type: IUnknown *
  D2D1_LOOKUPTABLE3D_PROP_LUT     = D2D1_LOOKUPTABLE3D_PROP(0);
  {$EXTERNALSYM D2D1_LOOKUPTABLE3D_PROP_LUT}
  // Property Name: "AlphaMode"
  // Property Type: D2D1_ALPHA_MODE
  D2D1_LOOKUPTABLE3D_PROP_ALPHA_MODE  = D2D1_LOOKUPTABLE3D_PROP(1);
  {$EXTERNALSYM D2D1_LOOKUPTABLE3D_PROP_ALPHA_MODE}
  //D2D1_LOOKUPTABLE3D_PROP_FORCE_DWORD = FORCEDWORD;

//#if NTDDI_VERSION >= NTDDI_WIN10_RS1

type
  // The enumeration of the Opacity effect's top level properties.
  // Effect description: Adjusts the opacity of an image by multiplying the alpha
  // channel by the specified opacity.
  PD2D1_OPACITY_PROP = ^D2D1_OPACITY_PROP;
  D2D1_OPACITY_PROP = DWord;
  {$EXTERNALSYM D2D1_OPACITY_PROP}
const
  // Property Name: "Opacity"
  // Property Type: FLOAT
  D2D1_OPACITY_PROP_OPACITY   = D2D1_OPACITY_PROP(0);
  //D2D1_OPACITY_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Cross Fade effect's top level properties.
  // Effect description: This effect combines two images by adding weighted pixels
  // from input images. The formula can be expressed as output = weight * Destination
  // + (1 - weight) * Source
  PD2D1_CROSSFADE_PROP = ^D2D1_CROSSFADE_PROP;
  D2D1_CROSSFADE_PROP = DWord;
  {$EXTERNALSYM D2D1_CROSSFADE_PROP}
const
  // Property Name: "Weight"
  // Property Type: FLOAT
  D2D1_CROSSFADE_PROP_WEIGHT    = D2D1_CROSSFADE_PROP(0);
  //D2D1_CROSSFADE_PROP_FORCE_DWORD = FORCEDWORD;

type
  // The enumeration of the Tint effect's top level properties.
  // Effect description: This effect tints the source image by multiplying the
  // specified color by the source image.
  PD2D1_TINT_PROP = ^D2D1_TINT_PROP;
  D2D1_TINT_PROP = DWord;
  {$EXTERNALSYM D2D1_TINT_PROP}
const
  // Property Name: "Color"
  // Property Type: D2D1_VECTOR_4F
  D2D1_TINT_PROP_COLOR    = D2D1_TINT_PROP(0);
  {$EXTERNALSYM D2D1_TINT_PROP_COLOR}
  // Property Name: "ClampOutput"
  // Property Type: BOOL
  D2D1_TINT_PROP_CLAMP_OUTPUT = D2D1_TINT_PROP(1);
  {$EXTERNALSYM D2D1_TINT_PROP_CLAMP_OUTPUT}
  //D2D1_TINT_PROP_FORCE_DWORD  = FORCEDWORD;

//#endif // #if NTDDI_VERSION >= NTDDI_WIN10_RS1

//#if NTDDI_VERSION >= NTDDI_WIN10_RS5

type
  // The enumeration of the White Level Adjustment effect's top level properties.
  // Effect description: This effect adjusts the white level of the source image by
  // multiplying the source image color by the ratio of the input and output white
  // levels. Input and output white levels are specified in nits.
  PD2D1_WHITELEVELADJUSTMENT_PROP = ^D2D1_WHITELEVELADJUSTMENT_PROP;
  D2D1_WHITELEVELADJUSTMENT_PROP = DWord;
  {$EXTERNALSYM D2D1_WHITELEVELADJUSTMENT_PROP}
const
  // Property Name: "InputWhiteLevel"
  // Property Type: FLOAT
  D2D1_WHITELEVELADJUSTMENT_PROP_INPUT_WHITE_LEVEL  = D2D1_WHITELEVELADJUSTMENT_PROP(0);
  {$EXTERNALSYM D2D1_WHITELEVELADJUSTMENT_PROP_INPUT_WHITE_LEVEL}
  // Property Name: "OutputWhiteLevel"
  // Property Type: FLOAT
  D2D1_WHITELEVELADJUSTMENT_PROP_OUTPUT_WHITE_LEVEL = D2D1_WHITELEVELADJUSTMENT_PROP(1);
  {$EXTERNALSYM D2D1_WHITELEVELADJUSTMENT_PROP_OUTPUT_WHITE_LEVEL}
  //D2D1_WHITELEVELADJUSTMENT_PROP_FORCE_DWORD    = FORCEDWORD;

type
  // The enumeration of the HDR Tone Map effect's top level properties.
  // Effect description: Adjusts the maximum luminance of the source image to fit
  // within the maximum output luminance supported. Input and output luminance values
  // are specified in nits. Note that the color space of the image is assumed to be
  // scRGB.
  PD2D1_HDRTONEMAP_PROP = ^D2D1_HDRTONEMAP_PROP;
  D2D1_HDRTONEMAP_PROP = DWord;
  {$EXTERNALSYM D2D1_HDRTONEMAP_PROP}
const
  // Property Name: "InputMaxLuminance"
  // Property Type: FLOAT
  D2D1_HDRTONEMAP_PROP_INPUT_MAX_LUMINANCE  = D2D1_HDRTONEMAP_PROP(0);
  {$EXTERNALSYM D2D1_HDRTONEMAP_PROP_INPUT_MAX_LUMINANCE}
  // Property Name: "OutputMaxLuminance"
  // Property Type: FLOAT
  D2D1_HDRTONEMAP_PROP_OUTPUT_MAX_LUMINANCE = D2D1_HDRTONEMAP_PROP(1);
  {$EXTERNALSYM D2D1_HDRTONEMAP_PROP_OUTPUT_MAX_LUMINANCE}
  // Property Name: "DisplayMode"
  // Property Type: D2D1_HDRTONEMAP_DISPLAY_MODE
  D2D1_HDRTONEMAP_PROP_DISPLAY_MODE     = D2D1_HDRTONEMAP_PROP(2);
  {$EXTERNALSYM D2D1_HDRTONEMAP_PROP_DISPLAY_MODE}
  //D2D1_HDRTONEMAP_PROP_FORCE_DWORD      = FORCEDWORD;

type
  PD2D1_HDRTONEMAP_DISPLAY_MODE = ^D2D1_HDRTONEMAP_DISPLAY_MODE;
  D2D1_HDRTONEMAP_DISPLAY_MODE = DWord;
  {$EXTERNALSYM D2D1_HDRTONEMAP_DISPLAY_MODE}
const
  D2D1_HDRTONEMAP_DISPLAY_MODE_SDR     = D2D1_HDRTONEMAP_DISPLAY_MODE(0);
  {$EXTERNALSYM D2D1_HDRTONEMAP_DISPLAY_MODE_SDR}
  D2D1_HDRTONEMAP_DISPLAY_MODE_HDR     = D2D1_HDRTONEMAP_DISPLAY_MODE(1);
  {$EXTERNALSYM D2D1_HDRTONEMAP_DISPLAY_MODE_HDR}
  //D2D1_HDRTONEMAP_DISPLAY_MODE_FORCE_DWORD = FORCEDWORD;

//#endif // #if NTDDI_VERSION >= NTDDI_WIN10_RS1


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.

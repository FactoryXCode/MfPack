// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectWrite
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DWrite_1.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
//
// Description: DirectX Typography Services public API definitions.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: dwrite_1.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit MfPack.DWrite_1;

  {$HPPEMIT '#include "dwrite_1.h"'}

interface

uses
  {MfPack}
  MfPack.MfpTypes,
  MfPack.DCommon,
  MfPack.DWrite;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}
  {$I 'MfPack.inc'}


type
  // The overall kind of family.
  PDWRITE_PANOSE_FAMILY = ^DWRITE_PANOSE_FAMILY;
  DWRITE_PANOSE_FAMILY = (
    DWRITE_PANOSE_FAMILY_ANY = 0,
    DWRITE_PANOSE_FAMILY_NO_FIT = 1,
    DWRITE_PANOSE_FAMILY_TEXT_DISPLAY = 2,
    DWRITE_PANOSE_FAMILY_SCRIPT = 3, // or hand written
    DWRITE_PANOSE_FAMILY_DECORATIVE = 4,
    DWRITE_PANOSE_FAMILY_SYMBOL = 5, // or symbol
    DWRITE_PANOSE_FAMILY_PICTORIAL = DWRITE_PANOSE_FAMILY_SYMBOL);
  {$EXTERNALSYM DWRITE_PANOSE_FAMILY}


  // Appearance of the serifs.
  // Present for families: 2-text
  PDWRITE_PANOSE_SERIF_STYLE = ^DWRITE_PANOSE_SERIF_STYLE;
  DWRITE_PANOSE_SERIF_STYLE = (
    DWRITE_PANOSE_SERIF_STYLE_ANY = 0,
    DWRITE_PANOSE_SERIF_STYLE_NO_FIT = 1,
    DWRITE_PANOSE_SERIF_STYLE_COVE = 2,
    DWRITE_PANOSE_SERIF_STYLE_OBTUSE_COVE = 3,
    DWRITE_PANOSE_SERIF_STYLE_SQUARE_COVE = 4,
    DWRITE_PANOSE_SERIF_STYLE_OBTUSE_SQUARE_COVE = 5,
    DWRITE_PANOSE_SERIF_STYLE_SQUARE = 6,
    DWRITE_PANOSE_SERIF_STYLE_THIN = 7,
    DWRITE_PANOSE_SERIF_STYLE_OVAL = 8,
    DWRITE_PANOSE_SERIF_STYLE_EXAGGERATED = 9,
    DWRITE_PANOSE_SERIF_STYLE_TRIANGLE = 10,
    DWRITE_PANOSE_SERIF_STYLE_NORMAL_SANS = 11,
    DWRITE_PANOSE_SERIF_STYLE_OBTUSE_SANS = 12,
    DWRITE_PANOSE_SERIF_STYLE_PERPENDICULAR_SANS = 13,
    DWRITE_PANOSE_SERIF_STYLE_FLARED = 14,
    DWRITE_PANOSE_SERIF_STYLE_ROUNDED = 15,
    DWRITE_PANOSE_SERIF_STYLE_SCRIPT = 16,
    DWRITE_PANOSE_SERIF_STYLE_PERP_SANS = DWRITE_PANOSE_SERIF_STYLE_PERPENDICULAR_SANS,
    DWRITE_PANOSE_SERIF_STYLE_BONE = DWRITE_PANOSE_SERIF_STYLE_OVAL);
  {$EXTERNALSYM DWRITE_PANOSE_SERIF_STYLE}


  // PANOSE font weights. These roughly correspond to the DWRITE_FONT_WEIGHT's
  // using (panose_weight - 2) * 100.
  // Present for families: 2-text, 3-script, 4-decorative, 5-symbol
  PDWRITE_PANOSE_WEIGHT = ^DWRITE_PANOSE_WEIGHT;
  DWRITE_PANOSE_WEIGHT = (
    DWRITE_PANOSE_WEIGHT_ANY = 0,
    DWRITE_PANOSE_WEIGHT_NO_FIT = 1,
    DWRITE_PANOSE_WEIGHT_VERY_LIGHT = 2,
    DWRITE_PANOSE_WEIGHT_LIGHT = 3,
    DWRITE_PANOSE_WEIGHT_THIN = 4,
    DWRITE_PANOSE_WEIGHT_BOOK = 5,
    DWRITE_PANOSE_WEIGHT_MEDIUM = 6,
    DWRITE_PANOSE_WEIGHT_DEMI = 7,
    DWRITE_PANOSE_WEIGHT_BOLD = 8,
    DWRITE_PANOSE_WEIGHT_HEAVY = 9,
    DWRITE_PANOSE_WEIGHT_BLACK = 10,
    DWRITE_PANOSE_WEIGHT_EXTRA_BLACK = 11,
    DWRITE_PANOSE_WEIGHT_NORD = DWRITE_PANOSE_WEIGHT_EXTRA_BLACK);
  {$EXTERNALSYM DWRITE_PANOSE_WEIGHT}


  // Proportion of the glyph shape considering additional detail to standard
  // characters.
  // Present for families: 2-text
  PDWRITE_PANOSE_PROPORTION = ^DWRITE_PANOSE_PROPORTION;
  DWRITE_PANOSE_PROPORTION = (
    DWRITE_PANOSE_PROPORTION_ANY = 0,
    DWRITE_PANOSE_PROPORTION_NO_FIT = 1,
    DWRITE_PANOSE_PROPORTION_OLD_STYLE = 2,
    DWRITE_PANOSE_PROPORTION_MODERN = 3,
    DWRITE_PANOSE_PROPORTION_EVEN_WIDTH = 4,
    DWRITE_PANOSE_PROPORTION_EXPANDED = 5,
    DWRITE_PANOSE_PROPORTION_CONDENSED = 6,
    DWRITE_PANOSE_PROPORTION_VERY_EXPANDED = 7,
    DWRITE_PANOSE_PROPORTION_VERY_CONDENSED = 8,
    DWRITE_PANOSE_PROPORTION_MONOSPACED = 9);
  {$EXTERNALSYM DWRITE_PANOSE_PROPORTION}


  // Ratio between thickest and thinnest point of the stroke for a letter such
  // as uppercase 'O'.
  // Present for families: 2-text, 3-script, 4-decorative
  PDWRITE_PANOSE_CONTRAST = ^DWRITE_PANOSE_CONTRAST;
  DWRITE_PANOSE_CONTRAST = (
    DWRITE_PANOSE_CONTRAST_ANY = 0,
    DWRITE_PANOSE_CONTRAST_NO_FIT = 1,
    DWRITE_PANOSE_CONTRAST_NONE = 2,
    DWRITE_PANOSE_CONTRAST_VERY_LOW = 3,
    DWRITE_PANOSE_CONTRAST_LOW = 4,
    DWRITE_PANOSE_CONTRAST_MEDIUM_LOW = 5,
    DWRITE_PANOSE_CONTRAST_MEDIUM = 6,
    DWRITE_PANOSE_CONTRAST_MEDIUM_HIGH = 7,
    DWRITE_PANOSE_CONTRAST_HIGH = 8,
    DWRITE_PANOSE_CONTRAST_VERY_HIGH = 9,
    DWRITE_PANOSE_CONTRAST_HORIZONTAL_LOW = 10,
    DWRITE_PANOSE_CONTRAST_HORIZONTAL_MEDIUM = 11,
    DWRITE_PANOSE_CONTRAST_HORIZONTAL_HIGH = 12,
    DWRITE_PANOSE_CONTRAST_BROKEN = 13);
  {$EXTERNALSYM DWRITE_PANOSE_CONTRAST}


  // Relationship between thin and thick stems.
  // Present for families: 2-text
  PDWRITE_PANOSE_STROKE_VARIATION = ^DWRITE_PANOSE_STROKE_VARIATION;
  DWRITE_PANOSE_STROKE_VARIATION = (
    DWRITE_PANOSE_STROKE_VARIATION_ANY = 0,
    DWRITE_PANOSE_STROKE_VARIATION_NO_FIT = 1,
    DWRITE_PANOSE_STROKE_VARIATION_NO_VARIATION = 2,
    DWRITE_PANOSE_STROKE_VARIATION_GRADUAL_DIAGONAL = 3,
    DWRITE_PANOSE_STROKE_VARIATION_GRADUAL_TRANSITIONAL = 4,
    DWRITE_PANOSE_STROKE_VARIATION_GRADUAL_VERTICAL = 5,
    DWRITE_PANOSE_STROKE_VARIATION_GRADUAL_HORIZONTAL = 6,
    DWRITE_PANOSE_STROKE_VARIATION_RAPID_VERTICAL = 7,
    DWRITE_PANOSE_STROKE_VARIATION_RAPID_HORIZONTAL = 8,
    DWRITE_PANOSE_STROKE_VARIATION_INSTANT_VERTICAL = 9,
    DWRITE_PANOSE_STROKE_VARIATION_INSTANT_HORIZONTAL = 10);
  {$EXTERNALSYM DWRITE_PANOSE_STROKE_VARIATION}


  // Style of termination of stems and rounded letterforms.
  // Present for families: 2-text
  PDWRITE_PANOSE_ARM_STYLE = ^DWRITE_PANOSE_ARM_STYLE;
  DWRITE_PANOSE_ARM_STYLE = (
    DWRITE_PANOSE_ARM_STYLE_ANY = 0,
    DWRITE_PANOSE_ARM_STYLE_NO_FIT = 1,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_HORIZONTAL = 2,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_WEDGE = 3,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_VERTICAL = 4,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_SINGLE_SERIF = 5,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_DOUBLE_SERIF = 6,
    DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_HORIZONTAL = 7,
    DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_WEDGE = 8,
    DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_VERTICAL = 9,
    DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_SINGLE_SERIF = 10,
    DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_DOUBLE_SERIF = 11,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_HORZ = DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_HORIZONTAL,
    DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_VERT = DWRITE_PANOSE_ARM_STYLE_STRAIGHT_ARMS_VERTICAL,
    DWRITE_PANOSE_ARM_STYLE_BENT_ARMS_HORZ = DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_HORIZONTAL,
    DWRITE_PANOSE_ARM_STYLE_BENT_ARMS_WEDGE = DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_WEDGE,
    DWRITE_PANOSE_ARM_STYLE_BENT_ARMS_VERT = DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_VERTICAL,
    DWRITE_PANOSE_ARM_STYLE_BENT_ARMS_SINGLE_SERIF = DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_SINGLE_SERIF,
    DWRITE_PANOSE_ARM_STYLE_BENT_ARMS_DOUBLE_SERIF = DWRITE_PANOSE_ARM_STYLE_NONSTRAIGHT_ARMS_DOUBLE_SERIF);
  {$EXTERNALSYM DWRITE_PANOSE_ARM_STYLE}


  // Roundness of letterform.
  // Present for families: 2-text
  PDWRITE_PANOSE_LETTERFORM = ^DWRITE_PANOSE_LETTERFORM;
  DWRITE_PANOSE_LETTERFORM = (
    DWRITE_PANOSE_LETTERFORM_ANY = 0,
    DWRITE_PANOSE_LETTERFORM_NO_FIT = 1,
    DWRITE_PANOSE_LETTERFORM_NORMAL_CONTACT = 2,
    DWRITE_PANOSE_LETTERFORM_NORMAL_WEIGHTED = 3,
    DWRITE_PANOSE_LETTERFORM_NORMAL_BOXED = 4,
    DWRITE_PANOSE_LETTERFORM_NORMAL_FLATTENED = 5,
    DWRITE_PANOSE_LETTERFORM_NORMAL_ROUNDED = 6,
    DWRITE_PANOSE_LETTERFORM_NORMAL_OFF_CENTER = 7,
    DWRITE_PANOSE_LETTERFORM_NORMAL_SQUARE = 8,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_CONTACT = 9,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_WEIGHTED = 10,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_BOXED = 11,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_FLATTENED = 12,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_ROUNDED = 13,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_OFF_CENTER = 14,
    DWRITE_PANOSE_LETTERFORM_OBLIQUE_SQUARE = 15);
  {$EXTERNALSYM DWRITE_PANOSE_LETTERFORM}


  // Placement of midline across uppercase characters and treatment of diagonal
  // stem apexes.
  // Present for families: 2-text
  PDWRITE_PANOSE_MIDLINE = ^DWRITE_PANOSE_MIDLINE;
  DWRITE_PANOSE_MIDLINE = (
    DWRITE_PANOSE_MIDLINE_ANY = 0,
    DWRITE_PANOSE_MIDLINE_NO_FIT = 1,
    DWRITE_PANOSE_MIDLINE_STANDARD_TRIMMED = 2,
    DWRITE_PANOSE_MIDLINE_STANDARD_POINTED = 3,
    DWRITE_PANOSE_MIDLINE_STANDARD_SERIFED = 4,
    DWRITE_PANOSE_MIDLINE_HIGH_TRIMMED = 5,
    DWRITE_PANOSE_MIDLINE_HIGH_POINTED = 6,
    DWRITE_PANOSE_MIDLINE_HIGH_SERIFED = 7,
    DWRITE_PANOSE_MIDLINE_CONSTANT_TRIMMED = 8,
    DWRITE_PANOSE_MIDLINE_CONSTANT_POINTED = 9,
    DWRITE_PANOSE_MIDLINE_CONSTANT_SERIFED = 10,
    DWRITE_PANOSE_MIDLINE_LOW_TRIMMED = 11,
    DWRITE_PANOSE_MIDLINE_LOW_POINTED = 12,
    DWRITE_PANOSE_MIDLINE_LOW_SERIFED = 13);
  {$EXTERNALSYM DWRITE_PANOSE_MIDLINE}


  // Relative size of lowercase letters and treament of diacritic marks
  // and uppercase glyphs.
  // Present for families: 2-text
  PDWRITE_PANOSE_XHEIGHT = ^DWRITE_PANOSE_XHEIGHT;
  DWRITE_PANOSE_XHEIGHT = (
    DWRITE_PANOSE_XHEIGHT_ANY = 0,
    DWRITE_PANOSE_XHEIGHT_NO_FIT = 1,
    DWRITE_PANOSE_XHEIGHT_CONSTANT_SMALL = 2,
    DWRITE_PANOSE_XHEIGHT_CONSTANT_STANDARD = 3,
    DWRITE_PANOSE_XHEIGHT_CONSTANT_LARGE = 4,
    DWRITE_PANOSE_XHEIGHT_DUCKING_SMALL = 5,
    DWRITE_PANOSE_XHEIGHT_DUCKING_STANDARD = 6,
    DWRITE_PANOSE_XHEIGHT_DUCKING_LARGE = 7,
    DWRITE_PANOSE_XHEIGHT_CONSTANT_STD = DWRITE_PANOSE_XHEIGHT_CONSTANT_STANDARD,
    DWRITE_PANOSE_XHEIGHT_DUCKING_STD = DWRITE_PANOSE_XHEIGHT_DUCKING_STANDARD);
  {$EXTERNALSYM DWRITE_PANOSE_XHEIGHT}


  // Kind of tool used to create character forms.
  // Present for families: 3-script
  PDWRITE_PANOSE_TOOL_KIND = ^DWRITE_PANOSE_TOOL_KIND;
  DWRITE_PANOSE_TOOL_KIND = (
    DWRITE_PANOSE_TOOL_KIND_ANY = 0,
    DWRITE_PANOSE_TOOL_KIND_NO_FIT = 1,
    DWRITE_PANOSE_TOOL_KIND_FLAT_NIB = 2,
    DWRITE_PANOSE_TOOL_KIND_PRESSURE_POINT = 3,
    DWRITE_PANOSE_TOOL_KIND_ENGRAVED = 4,
    DWRITE_PANOSE_TOOL_KIND_BALL = 5,
    DWRITE_PANOSE_TOOL_KIND_BRUSH = 6,
    DWRITE_PANOSE_TOOL_KIND_ROUGH = 7,
    DWRITE_PANOSE_TOOL_KIND_FELT_PEN_BRUSH_TIP = 8,
    DWRITE_PANOSE_TOOL_KIND_WILD_BRUSH = 9);
  {$EXTERNALSYM DWRITE_PANOSE_TOOL_KIND}


  // Monospace vs proportional.
  // Present for families: 3-script, 5-symbol
  PDWRITE_PANOSE_SPACING = ^DWRITE_PANOSE_SPACING;
  DWRITE_PANOSE_SPACING = (
    DWRITE_PANOSE_SPACING_ANY = 0,
    DWRITE_PANOSE_SPACING_NO_FIT = 1,
    DWRITE_PANOSE_SPACING_PROPORTIONAL_SPACED = 2,
    DWRITE_PANOSE_SPACING_MONOSPACED = 3);
  {$EXTERNALSYM DWRITE_PANOSE_SPACING}


  // Ratio between width and height of the face.
  // Present for families: 3-script
  PDWRITE_PANOSE_ASPECT_RATIO = ^DWRITE_PANOSE_ASPECT_RATIO;
  DWRITE_PANOSE_ASPECT_RATIO = (
    DWRITE_PANOSE_ASPECT_RATIO_ANY = 0,
    DWRITE_PANOSE_ASPECT_RATIO_NO_FIT = 1,
    DWRITE_PANOSE_ASPECT_RATIO_VERY_CONDENSED = 2,
    DWRITE_PANOSE_ASPECT_RATIO_CONDENSED = 3,
    DWRITE_PANOSE_ASPECT_RATIO_NORMAL = 4,
    DWRITE_PANOSE_ASPECT_RATIO_EXPANDED = 5,
    DWRITE_PANOSE_ASPECT_RATIO_VERY_EXPANDED = 6);
  {$EXTERNALSYM DWRITE_PANOSE_ASPECT_RATIO}


  // Topology of letterforms.
  // Present for families: 3-script
  PDWRITE_PANOSE_SCRIPT_TOPOLOGY = ^DWRITE_PANOSE_SCRIPT_TOPOLOGY;
  DWRITE_PANOSE_SCRIPT_TOPOLOGY = (
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_ANY = 0,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_NO_FIT = 1,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_ROMAN_DISCONNECTED = 2,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_ROMAN_TRAILING = 3,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_ROMAN_CONNECTED = 4,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_CURSIVE_DISCONNECTED = 5,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_CURSIVE_TRAILING = 6,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_CURSIVE_CONNECTED = 7,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_BLACKLETTER_DISCONNECTED = 8,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_BLACKLETTER_TRAILING = 9,
    DWRITE_PANOSE_SCRIPT_TOPOLOGY_BLACKLETTER_CONNECTED = 10);
  {$EXTERNALSYM DWRITE_PANOSE_SCRIPT_TOPOLOGY}


  // General look of the face, considering slope and tails.
  // Present for families: 3-script
  PDWRITE_PANOSE_SCRIPT_FORM = ^DWRITE_PANOSE_SCRIPT_FORM;
  DWRITE_PANOSE_SCRIPT_FORM = (
    DWRITE_PANOSE_SCRIPT_FORM_ANY = 0,
    DWRITE_PANOSE_SCRIPT_FORM_NO_FIT = 1,
    DWRITE_PANOSE_SCRIPT_FORM_UPRIGHT_NO_WRAPPING = 2,
    DWRITE_PANOSE_SCRIPT_FORM_UPRIGHT_SOME_WRAPPING = 3,
    DWRITE_PANOSE_SCRIPT_FORM_UPRIGHT_MORE_WRAPPING = 4,
    DWRITE_PANOSE_SCRIPT_FORM_UPRIGHT_EXTREME_WRAPPING = 5,
    DWRITE_PANOSE_SCRIPT_FORM_OBLIQUE_NO_WRAPPING = 6,
    DWRITE_PANOSE_SCRIPT_FORM_OBLIQUE_SOME_WRAPPING = 7,
    DWRITE_PANOSE_SCRIPT_FORM_OBLIQUE_MORE_WRAPPING = 8,
    DWRITE_PANOSE_SCRIPT_FORM_OBLIQUE_EXTREME_WRAPPING = 9,
    DWRITE_PANOSE_SCRIPT_FORM_EXAGGERATED_NO_WRAPPING = 10,
    DWRITE_PANOSE_SCRIPT_FORM_EXAGGERATED_SOME_WRAPPING = 11,
    DWRITE_PANOSE_SCRIPT_FORM_EXAGGERATED_MORE_WRAPPING = 12,
    DWRITE_PANOSE_SCRIPT_FORM_EXAGGERATED_EXTREME_WRAPPING = 13);
  {$EXTERNALSYM DWRITE_PANOSE_SCRIPT_FORM}


  // How character ends and miniscule ascenders are treated.
  // Present for families: 3-script
  PDWRITE_PANOSE_FINIALS = ^DWRITE_PANOSE_FINIALS;
  DWRITE_PANOSE_FINIALS = (
    DWRITE_PANOSE_FINIALS_ANY = 0,
    DWRITE_PANOSE_FINIALS_NO_FIT = 1,
    DWRITE_PANOSE_FINIALS_NONE_NO_LOOPS = 2,
    DWRITE_PANOSE_FINIALS_NONE_CLOSED_LOOPS = 3,
    DWRITE_PANOSE_FINIALS_NONE_OPEN_LOOPS = 4,
    DWRITE_PANOSE_FINIALS_SHARP_NO_LOOPS = 5,
    DWRITE_PANOSE_FINIALS_SHARP_CLOSED_LOOPS = 6,
    DWRITE_PANOSE_FINIALS_SHARP_OPEN_LOOPS = 7,
    DWRITE_PANOSE_FINIALS_TAPERED_NO_LOOPS = 8,
    DWRITE_PANOSE_FINIALS_TAPERED_CLOSED_LOOPS = 9,
    DWRITE_PANOSE_FINIALS_TAPERED_OPEN_LOOPS = 10,
    DWRITE_PANOSE_FINIALS_ROUND_NO_LOOPS = 11,
    DWRITE_PANOSE_FINIALS_ROUND_CLOSED_LOOPS = 12,
    DWRITE_PANOSE_FINIALS_ROUND_OPEN_LOOPS = 13);
  {$EXTERNALSYM DWRITE_PANOSE_FINIALS}

  // Relative size of the lowercase letters.
  // Present for families: 3-script
  PDWRITE_PANOSE_XASCENT = ^DWRITE_PANOSE_XASCENT;
  DWRITE_PANOSE_XASCENT = (
    DWRITE_PANOSE_XASCENT_ANY = 0,
    DWRITE_PANOSE_XASCENT_NO_FIT = 1,
    DWRITE_PANOSE_XASCENT_VERY_LOW = 2,
    DWRITE_PANOSE_XASCENT_LOW = 3,
    DWRITE_PANOSE_XASCENT_MEDIUM = 4,
    DWRITE_PANOSE_XASCENT_HIGH = 5,
    DWRITE_PANOSE_XASCENT_VERY_HIGH = 6);
  {$EXTERNALSYM DWRITE_PANOSE_XASCENT}

  // General look of the face.
  // Present for families: 4-decorative
  PDWRITE_PANOSE_DECORATIVE_CLASS = ^DWRITE_PANOSE_DECORATIVE_CLASS;
  DWRITE_PANOSE_DECORATIVE_CLASS = (
    DWRITE_PANOSE_DECORATIVE_CLASS_ANY = 0,
    DWRITE_PANOSE_DECORATIVE_CLASS_NO_FIT = 1,
    DWRITE_PANOSE_DECORATIVE_CLASS_DERIVATIVE = 2,
    DWRITE_PANOSE_DECORATIVE_CLASS_NONSTANDARD_TOPOLOGY = 3,
    DWRITE_PANOSE_DECORATIVE_CLASS_NONSTANDARD_ELEMENTS = 4,
    DWRITE_PANOSE_DECORATIVE_CLASS_NONSTANDARD_ASPECT = 5,
    DWRITE_PANOSE_DECORATIVE_CLASS_INITIALS = 6,
    DWRITE_PANOSE_DECORATIVE_CLASS_CARTOON = 7,
    DWRITE_PANOSE_DECORATIVE_CLASS_PICTURE_STEMS = 8,
    DWRITE_PANOSE_DECORATIVE_CLASS_ORNAMENTED = 9,
    DWRITE_PANOSE_DECORATIVE_CLASS_TEXT_AND_BACKGROUND = 10,
    DWRITE_PANOSE_DECORATIVE_CLASS_COLLAGE = 11,
    DWRITE_PANOSE_DECORATIVE_CLASS_MONTAGE = 12);
  {$EXTERNALSYM DWRITE_PANOSE_DECORATIVE_CLASS}

  // Ratio between the width and height of the face.
  // Present for families: 4-decorative
  PDWRITE_PANOSE_ASPECT = ^DWRITE_PANOSE_ASPECT;
  DWRITE_PANOSE_ASPECT = (
    DWRITE_PANOSE_ASPECT_ANY = 0,
    DWRITE_PANOSE_ASPECT_NO_FIT = 1,
    DWRITE_PANOSE_ASPECT_SUPER_CONDENSED = 2,
    DWRITE_PANOSE_ASPECT_VERY_CONDENSED = 3,
    DWRITE_PANOSE_ASPECT_CONDENSED = 4,
    DWRITE_PANOSE_ASPECT_NORMAL = 5,
    DWRITE_PANOSE_ASPECT_EXTENDED = 6,
    DWRITE_PANOSE_ASPECT_VERY_EXTENDED = 7,
    DWRITE_PANOSE_ASPECT_SUPER_EXTENDED = 8,
    DWRITE_PANOSE_ASPECT_MONOSPACED = 9);
  {$EXTERNALSYM DWRITE_PANOSE_ASPECT}

  // Type of fill/line (treatment).
  // Present for families: 4-decorative
  PDWRITE_PANOSE_FILL = ^DWRITE_PANOSE_FILL;
  DWRITE_PANOSE_FILL = (
    DWRITE_PANOSE_FILL_ANY = 0,
    DWRITE_PANOSE_FILL_NO_FIT = 1,
    DWRITE_PANOSE_FILL_STANDARD_SOLID_FILL = 2,
    DWRITE_PANOSE_FILL_NO_FILL = 3,
    DWRITE_PANOSE_FILL_PATTERNED_FILL = 4,
    DWRITE_PANOSE_FILL_COMPLEX_FILL = 5,
    DWRITE_PANOSE_FILL_SHAPED_FILL = 6,
    DWRITE_PANOSE_FILL_DRAWN_DISTRESSED = 7);
  {$EXTERNALSYM DWRITE_PANOSE_FILL}

  // Outline handling.
  // Present for families: 4-decorative
  PDWRITE_PANOSE_LINING = ^DWRITE_PANOSE_LINING;
  DWRITE_PANOSE_LINING = (
    DWRITE_PANOSE_LINING_ANY = 0,
    DWRITE_PANOSE_LINING_NO_FIT = 1,
    DWRITE_PANOSE_LINING_NONE = 2,
    DWRITE_PANOSE_LINING_INLINE = 3,
    DWRITE_PANOSE_LINING_OUTLINE = 4,
    DWRITE_PANOSE_LINING_ENGRAVED = 5,
    DWRITE_PANOSE_LINING_SHADOW = 6,
    DWRITE_PANOSE_LINING_RELIEF = 7,
    DWRITE_PANOSE_LINING_BACKDROP = 8);
  {$EXTERNALSYM DWRITE_PANOSE_LINING}

  // Overall shape characteristics of the font.
  // Present for families: 4-decorative
  PDWRITE_PANOSE_DECORATIVE_TOPOLOGY = ^DWRITE_PANOSE_DECORATIVE_TOPOLOGY;
  DWRITE_PANOSE_DECORATIVE_TOPOLOGY = (
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_ANY = 0,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_NO_FIT = 1,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_STANDARD = 2,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_SQUARE = 3,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_MULTIPLE_SEGMENT = 4,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_ART_DECO = 5,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_UNEVEN_WEIGHTING = 6,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_DIVERSE_ARMS = 7,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_DIVERSE_FORMS = 8,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_LOMBARDIC_FORMS = 9,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_UPPER_CASE_IN_LOWER_CASE = 10,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_IMPLIED_TOPOLOGY = 11,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_HORSESHOE_E_AND_A = 12,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_CURSIVE = 13,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_BLACKLETTER = 14,
    DWRITE_PANOSE_DECORATIVE_TOPOLOGY_SWASH_VARIANCE = 15);
  {$EXTERNALSYM DWRITE_PANOSE_DECORATIVE_TOPOLOGY}

  // Type of characters available in the font.
  // Present for families: 4-decorative
  PDWRITE_PANOSE_CHARACTER_RANGES = ^DWRITE_PANOSE_CHARACTER_RANGES;
  DWRITE_PANOSE_CHARACTER_RANGES = (
    DWRITE_PANOSE_CHARACTER_RANGES_ANY = 0,
    DWRITE_PANOSE_CHARACTER_RANGES_NO_FIT = 1,
    DWRITE_PANOSE_CHARACTER_RANGES_EXTENDED_COLLECTION = 2,
    DWRITE_PANOSE_CHARACTER_RANGES_LITERALS = 3,
    DWRITE_PANOSE_CHARACTER_RANGES_NO_LOWER_CASE = 4,
    DWRITE_PANOSE_CHARACTER_RANGES_SMALL_CAPS = 5);
  {$EXTERNALSYM DWRITE_PANOSE_CHARACTER_RANGES}

  // Kind of symbol set.
  // Present for families: 5-symbol
  PDWRITE_PANOSE_SYMBOL_KIND = ^DWRITE_PANOSE_SYMBOL_KIND;
  DWRITE_PANOSE_SYMBOL_KIND = (
    DWRITE_PANOSE_SYMBOL_KIND_ANY = 0,
    DWRITE_PANOSE_SYMBOL_KIND_NO_FIT = 1,
    DWRITE_PANOSE_SYMBOL_KIND_MONTAGES = 2,
    DWRITE_PANOSE_SYMBOL_KIND_PICTURES = 3,
    DWRITE_PANOSE_SYMBOL_KIND_SHAPES = 4,
    DWRITE_PANOSE_SYMBOL_KIND_SCIENTIFIC = 5,
    DWRITE_PANOSE_SYMBOL_KIND_MUSIC = 6,
    DWRITE_PANOSE_SYMBOL_KIND_EXPERT = 7,
    DWRITE_PANOSE_SYMBOL_KIND_PATTERNS = 8,
    DWRITE_PANOSE_SYMBOL_KIND_BOARDERS = 9,
    DWRITE_PANOSE_SYMBOL_KIND_ICONS = 10,
    DWRITE_PANOSE_SYMBOL_KIND_LOGOS = 11,
    DWRITE_PANOSE_SYMBOL_KIND_INDUSTRY_SPECIFIC = 12);
  {$EXTERNALSYM DWRITE_PANOSE_SYMBOL_KIND}

  // Aspect ratio of symbolic characters.
  // Present for families: 5-symbol
  PDWRITE_PANOSE_SYMBOL_ASPECT_RATIO = ^DWRITE_PANOSE_SYMBOL_ASPECT_RATIO;
  DWRITE_PANOSE_SYMBOL_ASPECT_RATIO = (
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_ANY = 0,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_NO_FIT = 1,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_NO_WIDTH = 2,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_EXCEPTIONALLY_WIDE = 3,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_SUPER_WIDE = 4,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_VERY_WIDE = 5,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_WIDE = 6,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_NORMAL = 7,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_NARROW = 8,
    DWRITE_PANOSE_SYMBOL_ASPECT_RATIO_VERY_NARROW = 9);
  {$EXTERNALSYM DWRITE_PANOSE_SYMBOL_ASPECT_RATIO}

  // Specifies the policy used by GetRecommendedRenderingMode to determine whether to
  // render glyphs in outline mode. Glyphs are rendered in outline mode by default at
  // large sizes for performance reasons, but how large (i.e., the outline threshold)
  // depends on the quality of outline rendering. If the graphics system renders anti-
  // aliased outlines then a relatively low threshold is used, but if the graphics
  // system renders aliased outlines then a much higher threshold is used.
  PDWRITE_OUTLINE_THRESHOLD = ^DWRITE_OUTLINE_THRESHOLD;
  DWRITE_OUTLINE_THRESHOLD = (
    DWRITE_OUTLINE_THRESHOLD_ANTIALIASED,
    DWRITE_OUTLINE_THRESHOLD_ALIASED);
  {$EXTERNALSYM DWRITE_OUTLINE_THRESHOLD}

  // Baseline for text alignment.
  PDWRITE_BASELINE = ^DWRITE_BASELINE;
  DWRITE_BASELINE = (
    // The Roman baseline for horizontal, Central baseline for vertical.
    DWRITE_BASELINE_DEFAULT,
    // The baseline used by alphabetic scripts such as Latin, Greek, Cyrillic.
    DWRITE_BASELINE_ROMAN,
    // Central baseline, generally used for vertical text.
    DWRITE_BASELINE_CENTRAL,
    // Mathematical baseline which math characters are centered on.
    DWRITE_BASELINE_MATH,
    // Hanging baseline, used in scripts like Devanagari.
    DWRITE_BASELINE_HANGING,
    // Ideographic bottom baseline for CJK, left in vertical.
    DWRITE_BASELINE_IDEOGRAPHIC_BOTTOM,
    // Ideographic top baseline for CJK, right in vertical.
    DWRITE_BASELINE_IDEOGRAPHIC_TOP,
    // The bottom-most extent in horizontal, left-most in vertical.
    DWRITE_BASELINE_MINIMUM,
    // The top-most extent in horizontal, right-most in vertical.
    DWRITE_BASELINE_MAXIMUM);
  {$EXTERNALSYM DWRITE_BASELINE}

  // The desired kind of glyph orientation for the text. The client specifies
  // this to the analyzer as the desired orientation, but note this is the
  // client preference, and the constraints of the script will determine the
  // final presentation.
  PDWRITE_VERTICAL_GLYPH_ORIENTATION = ^DWRITE_VERTICAL_GLYPH_ORIENTATION;
  DWRITE_VERTICAL_GLYPH_ORIENTATION = (
    // In vertical layout, naturally horizontal scripts (Latin, Thai, Arabic,
    // Devanagari) rotate 90 degrees clockwise, while ideographic scripts
    // (Chinese, Japanese, Korean) remain upright, 0 degrees.
    DWRITE_VERTICAL_GLYPH_ORIENTATION_DEFAULT,
    // Ideographic scripts and scripts that permit stacking
    // (Latin, Hebrew) are stacked in vertical reading layout.
    // Connected scripts (Arabic, Syriac, 'Phags-pa, Ogham),
    // which would otherwise look broken if glyphs were kept
    // at 0 degrees, remain connected and rotate.
    DWRITE_VERTICAL_GLYPH_ORIENTATION_STACKED);
  {$EXTERNALSYM DWRITE_VERTICAL_GLYPH_ORIENTATION}

  // How the glyph is oriented to the x-axis. This is an output from the text
  // analyzer, dependent on the desired orientation, bidi level, and character
  // properties.
  PDWRITE_GLYPH_ORIENTATION_ANGLE = ^DWRITE_GLYPH_ORIENTATION_ANGLE;
  DWRITE_GLYPH_ORIENTATION_ANGLE = (
    // Glyph orientation is upright.
    DWRITE_GLYPH_ORIENTATION_ANGLE_0_DEGREES,
    // Glyph orientation is rotated 90 clockwise.
    DWRITE_GLYPH_ORIENTATION_ANGLE_90_DEGREES,
    // Glyph orientation is upside-down.
    DWRITE_GLYPH_ORIENTATION_ANGLE_180_DEGREES,
    // Glyph orientation is rotated 270 clockwise.
    DWRITE_GLYPH_ORIENTATION_ANGLE_270_DEGREES);
  {$EXTERNALSYM DWRITE_GLYPH_ORIENTATION_ANGLE}


  // struct DWRITE_FONT_METRICS1 : public DWRITE_FONT_METRICS
  PDWRITE_FONT_METRICS1 = ^DWRITE_FONT_METRICS1;
  DWRITE_FONT_METRICS1 = record

    // Within Delphi we can't inherit from records
    // So, we hardcopy the DWRITE_FONT_METRICS fields here.
    DWriteFontMetrics: DWRITE_FONT_METRICS;

    // Left edge of accumulated bounding blackbox of all glyphs in the font.
    glyphBoxLeft: INT16;
    // Top edge of accumulated bounding blackbox of all glyphs in the font.
    glyphBoxTop: INT16;
    // Right edge of accumulated bounding blackbox of all glyphs in the font.
    glyphBoxRight: INT16;
    // Bottom edge of accumulated bounding blackbox of all glyphs in the font.
    glyphBoxBottom: INT16;
    // Horizontal position of the subscript relative to the baseline origin.
    // This is typically negative (to the left) in italic/oblique fonts, and
    // zero in regular fonts.
    subscriptPositionX: INT16;
    // Vertical position of the subscript relative to the baseline.
    // This is typically negative.
    subscriptPositionY: INT16;
    // Horizontal size of the subscript em box in design units, used to
    // scale the simulated subscript relative to the full em box size.
    // This the numerator of the scaling ratio where denominator is the
    // design units per em. If this member is zero, the font does not specify
    // a scale factor, and the client should use its own policy.
    subscriptSizeX: INT16;
    // Vertical size of the subscript em box in design units, used to
    // scale the simulated subscript relative to the full em box size.
    // This the numerator of the scaling ratio where denominator is the
    // design units per em. If this member is zero, the font does not specify
    // a scale factor, and the client should use its own policy.
    subscriptSizeY: INT16;
    // Horizontal position of the superscript relative to the baseline origin.
    // This is typically positive (to the right) in italic/oblique fonts, and
    // zero in regular fonts.
    superscriptPositionX: INT16;
    // Vertical position of the superscript relative to the baseline.
    // This is typically positive.
    superscriptPositionY: INT16;
    // Horizontal size of the superscript em box in design units, used to
    // scale the simulated superscript relative to the full em box size.
    // This the numerator of the scaling ratio where denominator is the
    // design units per em. If this member is zero, the font does not specify
    // a scale factor, and the client should use its own policy.
    superscriptSizeX: INT16;
    // Vertical size of the superscript em box in design units, used to
    // scale the simulated superscript relative to the full em box size.
    // This the numerator of the scaling ratio where denominator is the
    // design units per em. If this member is zero, the font does not specify
    // a scale factor, and the client should use its own policy.
    superscriptSizeY: INT16;
    // Indicates that the ascent, descent, and lineGap are based on newer
    // 'typographic' values in the font, rather than legacy values.
    hasTypographicMetrics: BOOL;
  end;
  {$EXTERNALSYM DWRITE_FONT_METRICS1}


  // Metrics for caret placement in a font.
  PDWRITE_CARET_METRICS = ^DWRITE_CARET_METRICS;
  DWRITE_CARET_METRICS = record
    // Vertical rise of the caret. Rise / Run yields the caret angle.
    // Rise = 1 for perfectly upright fonts (non-italic).
    slopeRise: INT16;

    // Horizontal run of th caret. Rise / Run yields the caret angle.
    // Run = 0 for perfectly upright fonts (non-italic).
    slopeRun: INT16;

    // Horizontal offset of the caret along the baseline for good appearance.
    // Offset = 0 for perfectly upright fonts (non-italic).
    offset: INT16
  end;
  {$EXTERNALSYM DWRITE_CARET_METRICS}


  // Typeface classification values, used for font selection and matching.

  // Remarks
  // Note the family type (index 0) is the only stable entry in the 10-byte
  // array, as all the following entries can change dynamically depending on
  // context of the first field.
  PDWRITE_PANOSE = ^DWRITE_PANOSE;
  DWRITE_PANOSE = record
    values: array [0..9] of UINT8;
    //familyKind: UINT8;  // this is the only field that never changes meaning
    case familyKind: UINT8 of
      2:(_familyKind: UINT8;
         serifStyle: UINT8;
         weight: UINT8;
         proportion: UINT8;
         contrast: UINT8;
         strokeVariation: UINT8;
         armStyle: UINT8;
         letterform: UINT8;
         midline: UINT8;
         xHeight: UINT8;); // text
      3:(__familyKind: UINT8;
         toolKind: UINT8;
         _weight: UINT8;
         spacing: UINT8;
         aspectRatio: UINT8;
         _contrast: UINT8;
         scriptTopology: UINT8;
         scriptForm: UINT8;
         finials: UINT8;
         xAscent: UINT8;); // script
      4:(familyKind_: UINT8;  // = 4 for decorative
         decorativeClass: UINT8;
         __weight: UINT8;
         aspect: UINT8;
         __contrast: UINT8;
         serifVariant: UINT8;
         fill: UINT8;  // treatment
         lining: UINT8;
         decorativeTopology: UINT8;
         characterRange: UINT8;); // decorative
      5:(familyKind__: UINT8;  // = 5 for symbol
         symbolKind: UINT8;
         weight_: UINT8;
         spacing_: UINT8;
         aspectRatioAndContrast: UINT8;  // hard coded to no-fit (1)
         aspectRatio94: UINT8;
         aspectRatio119: UINT8;
         aspectRatio157: UINT8;
         aspectRatio163: UINT8;
         aspectRatio211: UINT8;); // symbol
    end;
  {$EXTERNALSYM DWRITE_PANOSE}


  // Range of Unicode codepoints.
  PDWRITE_UNICODE_RANGE = ^DWRITE_UNICODE_RANGE;
  DWRITE_UNICODE_RANGE = record
    // The first codepoint in the Unicode range.
    first: UINT32;
    // The last codepoint in the Unicode range.
    last: UINT32;
  end;
  {$EXTERNALSYM DWRITE_UNICODE_RANGE}

  // Script-specific properties for caret navigation and justification.
  PDWRITE_SCRIPT_PROPERTIES = ^DWRITE_SCRIPT_PROPERTIES;
  DWRITE_SCRIPT_PROPERTIES = record
    // The standardized four character code for the given script.
    // Note these only include the general Unicode scripts, not any
    // additional ISO 15924 scripts for bibliographic distinction
    // (for example, Fraktur Latin vs Gaelic Latin).
    // http://unicode.org/iso15924/iso15924-codes.html
    isoScriptCode: UINT32;

    // The standardized numeric code, ranging 0-999.
    // http://unicode.org/iso15924/iso15924-codes.html
    isoScriptNumber: UINT32;

    // Number of characters to estimate look-ahead for complex scripts.
    // Latin and all Kana are generally 1. Indic scripts are up to 15,
    // and most others are 8. Note that combining marks and variation
    // selectors can produce clusters longer than these look-aheads,
    // so this estimate is considered typical language use. Diacritics
    // must be tested explicitly separately.
    clusterLookahead: UINT32;

    // Appropriate character to elongate the given script for justification.
    //
    // Examples:
    //   Arabic    - U+0640 Tatweel
    //   Ogham     - U+1680 Ogham Space Mark
    justificationCharacter: UINT32;

    private

      Flags: DWord;
      function GetBits(const aIndex: Integer): Integer;
      procedure SetBits(const aIndex: Integer; const aValue: Integer);

    public
      // Restrict the caret to whole clusters, like Thai and Devanagari. Scripts
      // such as Arabic by default allow navigation between clusters. Others
      // like Thai always navigate across whole clusters.
      property restrictCaretToClusters: Integer index $0001 read GetBits write SetBits; // 1 bit at offset 0

      // The language uses dividers between words, such as spaces between Latin
      // or the Ethiopic wordspace.
      //
      // Examples: Latin, Greek, Devanagari, Ethiopic
      // Excludes: Chinese, Korean, Thai.
      property usesWordDividers: Integer index $0101 read GetBits write SetBits;         // 1 bit at offset 1

    // The characters are discrete units from each other. This includes both
    // block scripts and clustered scripts.
    //
    // Examples: Latin, Greek, Cyrillic, Hebrew, Chinese, Thai
    property isDiscreteWriting: Integer index $0201 read GetBits write SetBits;          // 1 bit at offset 2

    // The language is a block script, expanding between characters.
    //
    // Examples: Chinese, Japanese, Korean, Bopomofo.
    property isBlockWriting: Integer index $0301 read GetBits write SetBits;             // 1 bit at offset 3

    // The language is justified within glyph clusters, not just between glyph
    // clusters. One such as the character sequence is Thai Lu and Sara Am
    // (U+E026, U+E033) which form a single cluster but still expand between
    // them.
    //
    // Examples: Thai, Lao, Khmer
    property isDistributedWithinCluster: Integer index $0401 read GetBits write SetBits; // 1 bit at offset 4

    // The script's clusters are connected to each other (such as the
    // baseline-linked Devanagari), and no separation should be added
    // between characters. Note that cursively linked scripts like Arabic
    // are also connected (but not all connected scripts are
    // cursive).
    //
    // Examples: Devanagari, Arabic, Syriac, Bengali, Gurmukhi, Ogham
    // Excludes: Latin, Chinese, Thaana
    property isConnectedWriting: Integer index $0501 read GetBits write SetBits;         // 1 bit at offset 5

    // The script is naturally cursive (Arabic/Syriac), meaning it uses other
    // justification methods like kashida extension rather than intercharacter
    // spacing. Note that although other scripts like Latin and Japanese may
    // actually support handwritten cursive forms, they are not considered
    // cursive scripts.
    //
    // Examples: Arabic, Syriac, Mongolian
    // Excludes: Thaana, Devanagari, Latin, Chinese
    property isCursiveWriting: Integer index $0601 read GetBits write SetBits;           // 1 bit at offset 6

    property reserved: Integer index $0719 read GetBits write SetBits;                   // 25 bits at offset 7

  end;
  {$EXTERNALSYM DWRITE_SCRIPT_PROPERTIES}


  // Justification information per glyph.
  PDWRITE_JUSTIFICATION_OPPORTUNITY = ^DWRITE_JUSTIFICATION_OPPORTUNITY;
  DWRITE_JUSTIFICATION_OPPORTUNITY = record
    // Minimum amount of expansion to apply to the side of the glyph.
    // This may vary from 0 to infinity, typically being zero except
    // for kashida.
    expansionMinimum: FLOAT;

    // Maximum amount of expansion to apply to the side of the glyph.
    // This may vary from 0 to infinity, being zero for fixed-size characters
    // and connected scripts, and non-zero for discrete scripts, and non-zero
    // for cursive scripts at expansion points.
    expansionMaximum: FLOAT;

    // Maximum amount of compression to apply to the side of the glyph.
    // This may vary from 0 up to the glyph cluster size.
    compressionMaximum: FLOAT;

    private

      Flags: DWord;
      function GetBits(const aIndex: Integer): Integer;
      procedure SetBits(const aIndex: Integer; const aValue: Integer);

    public

    // Priority of this expansion point. Larger priorities are applied later,
    // while priority zero does nothing.
    property expansionPriority: Integer index $0008 read GetBits write SetBits;        // 8 bits at offset //$00

    // Priority of this compression point. Larger priorities are applied later,
    // while priority zero does nothing.
    property compressionPriority: Integer index $0808 read GetBits write SetBits;      // 8 bits at offset 8 //$08

    // Allow this expansion point to use up any remaining slack space even
    // after all expansion priorities have been used up.
    property allowResidualExpansion: Integer index $1001 read GetBits write SetBits;   // 1 bits at offset 16 //$10

    // Allow this compression point to use up any remaining space even after
    // all compression priorities have been used up.
    property allowResidualCompression: Integer index $1101 read GetBits write SetBits; // 1 bits at offset 17 //$11

    // Apply expansion/compression to the leading edge of the glyph. This will
    // be false for connected scripts, fixed-size characters, and diacritics.
    // It is generally false within a multi-glyph cluster, unless the script
    // allows expansion of glyphs within a cluster, like Thai.
    property applyToLeadingEdge: Integer index $1201 read GetBits write SetBits;       // 1 bits at offset 18 //$12

    // Apply expansion/compression to the trailing edge of the glyph. This will
    // be false for connected scripts, fixed-size characters, and diacritics.
    // It is generally false within a multi-glyph cluster, unless the script
    // allows expansion of glyphs within a cluster, like Thai.
    property applyToTrailingEdge: Integer index $1301 read GetBits write SetBits;      // 1 bits at offset 19 //$13

    property reserved: Integer index $140C read GetBits write SetBits;                 // 12 bits at offset 20 //$14

  end;
  {$EXTERNALSYM DWRITE_JUSTIFICATION_OPPORTUNITY}


type
  //
  // Forward interface declarations
  //
  IDWriteTextAnalysisSource1 = interface;
  PIDWriteTextAnalysisSource1 = ^IDWriteTextAnalysisSource1;

  IDWriteTextAnalysisSink1 = interface;
  PIDWriteTextAnalysisSink1 = ^IDWriteTextAnalysisSink1;

  IDWriteRenderingParams1 = interface;
  PIDWriteRenderingParams1 = ^IDWriteRenderingParams1;


  // The root factory interface for all DWrite objects.
  // =====================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory1);'}
  {$EXTERNALSYM IDWriteFactory1}
  IDWriteFactory1 = interface(IDWriteFactory)
  ['{30572f99-dac6-41db-a16e-0486307e606a}']
    // Gets a font collection representing the set of end-user defined
    // custom fonts.

    // <param name="fontCollection">Receives a pointer to the EUDC font
    //     collection object, or NULL in case of failure.</param>
    // <param name="checkForUpdates">If this parameter is nonzero, the
    //     function performs an immediate check for changes to the set of
    //     EUDC fonts. If this parameter is FALSE, the function will still
    //     detect changes, but there may be some latency. For example, an
    //     application might specify TRUE if it has itself just modified a
    //     font and wants to be sure the font collection contains that font.
    //     </param>

    // Standard HRESULT error code. Note that if no EUDC is set on the system,
    // the returned collection will be empty, meaning it will return success
    // but GetFontFamilyCount will be zero.

    // Remarks
    // Querying via IDWriteFontCollection::FindFamilyName for a specific
    // family (like MS Gothic) will return the matching family-specific EUDC
    // font if one exists. Querying for "" will return the global EUDC font.
    // For example, if you were matching an EUDC character within a run of
    // the base font PMingLiu, you would retrieve the corresponding EUDC font
    // face using GetEudcFontCollection, then FindFamilyName with "PMingLiu",
    // followed by GetFontFamily and CreateFontFace.
    //
    // Be aware that eudcedit.exe can create placeholder empty glyphs that
    // have zero advance width and no glyph outline. Although they are present
    // in the font (HasCharacter returns true), you are best to ignore
    // these and continue on with font fallback in your layout if the metrics
    // for the glyph are zero.
    //
    function GetEudcFontCollection(fontCollection: IDWriteFontCollection;
                                   checkForUpdates: BOOL = FALSE): HResult; stdcall;


    // Creates a rendering parameters object with the specified properties.

    // <param name="gamma">The gamma value used for gamma correction, which must be greater than zero and cannot exceed 256.</param>
    // <param name="enhancedContrast">The amount of contrast enhancement, zero or greater.</param>
    // <param name="enhancedContrastGrayscale">The amount of contrast enhancement to use for grayscale antialiasing, zero or greater.</param>
    // <param name="clearTypeLevel">The degree of ClearType level, from 0.0f (no ClearType) to 1.0f (full ClearType).</param>
    // <param name="pixelGeometry">The geometry of a device pixel.</param>
    // <param name="renderingMode">Method of rendering glyphs. In most cases, this should be DWRITE_RENDERING_MODE_DEFAULT to automatically use an appropriate mode.</param>
    // <param name="renderingParams">Holds the newly created rendering parameters object, or NULL in case of failure.</param>

    // Standard HRESULT error code.

    function CreateCustomRenderingParams(gamma: FLOAT;
                                         enhancedContrast: FLOAT;
                                         enhancedContrastGrayscale: FLOAT;
                                         clearTypeLevel: FLOAT;
                                         pixelGeometry: DWRITE_PIXEL_GEOMETRY;
                                         renderingMode: DWRITE_RENDERING_MODE;
                                         out renderingParams: IDWriteRenderingParams1): HResult; stdcall;

  end;
  IID_IDWriteFactory1 = IDWriteFactory1;
  {$EXTERNALSYM IID_IDWriteFactory1}




  // Interface IDWriteFontFace1
  // ==========================
  // The interface that represents an absolute reference to a font face.
  // It contains font face type, appropriate file references and face identification data.
  // Various font data such as metrics, names and glyph outlines is obtained from IDWriteFontFace.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace1);'}
  {$EXTERNALSYM IDWriteFontFace1}
  IDWriteFontFace1 = interface(IDWriteFontFace)
  ['{a71efdb4-9fdb-4838-ad90-cfc3be8c3daf}']

    // Gets common metrics for the font in design units.
    // These metrics are applicable to all the glyphs within a font,
    // and are used by applications for layout calculations.
    // <param name="fontMetrics">Metrics structure to fill in.</param>
    procedure GetMetrics(out fontMetrics: DWRITE_FONT_METRICS1); stdcall;

    // Gets common metrics for the font in design units.
    // These metrics are applicable to all the glyphs within a font,
    // and are used by applications for layout calculations.
    // <param name="emSize">Logical size of the font in DIP units. A DIP
    //     ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For
    //     example, if the DPI of the rendering surface is 96 this value is
    //     1.0f. If the DPI is 120, this value is 120.0f/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and
    //     their positions. This transform is applied after the scaling
    //     specified by the font size and pixelsPerDip.</param>
    // <param name="fontMetrics">Font metrics structure to fill in.</param>
    // Standard HRESULT error code.
    function GetGdiCompatibleMetrics(emSize: FLOAT;
                                     pixelsPerDip: FLOAT;
                                     transform: PDWRITE_MATRIX;
                                     out fontMetrics: DWRITE_FONT_METRICS1): HResult; stdcall;


    // Gets caret metrics for the font in design units. These are used by
    // text editors for drawing the correct caret placement/slant.
    // <param name="caretMetrics">Metrics structure to fill in.</param>
    procedure GetCaretMetrics(out caretMetrics: DWRITE_CARET_METRICS); stdcall;


    // Returns the list ouof character ranges supported by the font, which is
    // useful for scenarios like character picking, glyph display, and
    // efficient font selection lookup. This is similar to GDI's
    // GetFontUnicodeRanges, except that it returns the full Unicode range,
    // not just 16-bit UCS-2.

    // <param name="maxRangeCount">Maximum number of character ranges passed
    //     in from the client.</param>
    // <param name="unicodeRanges">Array of character ranges.</param>
    // <param name="actualRangeCount">Actual number of character ranges,
    //     regardless of the maximum count.</param>
    // Remarks
    // These ranges are from the cmap, not the OS/2::ulCodePageRange1.
    //
    // Standard HRESULT error code.
    function GetUnicodeRanges(maxRangeCount: UINT32;
                              unicodeRanges: PDWRITE_UNICODE_RANGE;
                              actualRangeCount: UINT32): HResult; stdcall;

    // Returns true if the font is monospaced, meaning its characters are the
    // same fixed-pitch width (non-proportional).
    function IsMonospacedFont(): BOOL; stdcall;

    // Returns the advances in design units for a sequences of glyphs.
    // <param name="glyphCount">Number of glyphs to retrieve advances for.</param>
    // <param name="glyphIndices">Array of glyph id's to retrieve advances for.</param>
    // <param name="glyphAdvances">Returned advances in font design units for
    //     each glyph.</param>
    // <param name="isSideways">Retrieve the glyph's vertical advance height
    //     rather than horizontal advance widths.</param>
    // Remarks
    // This is equivalent to calling GetGlyphMetrics and using only the
    // advance width/height.
    //
    // Standard HRESULT error code.

    function GetDesignGlyphAdvances(glyphCount: UINT32;
                                    glyphIndices: UINT16;
                                    glyphAdvances: INT32;
                                    isSideways: BOOL = FALSE): HResult; stdcall;

    // Returns the pixel-aligned advances for a sequences of glyphs, the same
    // as GetGdiCompatibleGlyphMetrics would return.
    // <param name="emSize">Logical size of the font in DIP units. A DIP
    //     ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For
    //     example, if the DPI of the rendering surface is 96 this value is
    //     1.0f. If the DPI is 120, this value is 120.0f/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and
    //     their positions. This transform is applied after the scaling
    //     specified by the font size and pixelsPerDip.</param>
    // <param name="useGdiNatural">When FALSE, the metrics are the same as
    //     GDI aliased text (DWRITE_MEASURING_MODE_GDI_CLASSIC). When TRUE,
    //     the metrics are the same as those measured by GDI using a font
    //     using CLEARTYPE_NATURAL_QUALITY (DWRITE_MEASURING_MODE_GDI_NATURAL).</param>
    // <param name="isSideways">Retrieve the glyph's vertical advances rather
    //     than horizontal advances.</param>
    // <param name="glyphCount">Total glyphs to retrieve adjustments for.</param>
    // <param name="glyphIndices">Array of glyph id's to retrieve advances.</param>
    // <param name="glyphAdvances">Returned advances in font design units for
    //     each glyph.</param>
    // Remarks
    // This is equivalent to calling GetGdiCompatibleGlyphMetrics and using only
    // the advance width/height. Like GetGdiCompatibleGlyphMetrics, these are in
    // design units, meaning they must be scaled down by
    // DWRITE_FONT_METRICS::designUnitsPerEm.
    //
    // Standard HRESULT error code.
    function GetGdiCompatibleGlyphAdvances(emSize: FLOAT;
                                           pixelsPerDip: FLOAT;
                                           transform: PDWRITE_MATRIX;
                                           useGdiNatural: BOOL;
                                           isSideways: BOOL;
                                           glyphCount: UINT32;
                                           glyphIndices: UINT16;
                                           out glyphAdvances: INT32): HResult; stdcall;

    // Retrieves the kerning pair adjustments from the font's kern table.
    // <param name="glyphCount">Number of glyphs to retrieve adjustments for.</param>
    // <param name="glyphIndices">Array of glyph id's to retrieve adjustments
    //     for.</param>
    // <param name="glyphAdvanceAdjustments">Returned advances in font design units for
    //     each glyph. The last glyph adjustment is zero.</param>
    // Remarks
    // This is not a direct replacement for GDI's character based
    // GetKerningPairs, but it serves the same role, without the client
    // needing to cache them locally. It also uses glyph id's directly
    // rather than UCS-2 characters (how the kern table actually stores
    // them) which avoids glyph collapse and ambiguity, such as the dash
    // and hyphen, or space and non-breaking space.
    //
    // Remarks
    // Newer fonts may have only GPOS kerning instead of the legacy pair
    // table kerning. Such fonts, like Gabriola, will only return 0's for
    // adjustments. This function does not virtualize and flatten these
    // GPOS entries into kerning pairs.
    //
    // Standard HRESULT error code.
    function GetKerningPairAdjustments(glyphCount: UINT32;
                                       glyphIndices: UINT16;
                                       out glyphAdvanceAdjustments: INT32): HResult; stdcall;

    // Returns whether or not the font supports pair-kerning.
    // Remarks
    // If the font does not support pair table kerning, there is no need to
    // call GetKerningPairAdjustments (it would be all zeroes).
    //
    // Whether the font supports kerning pairs.
    function HasKerningPairs(): BOOL; stdcall;

    // Determines the recommended text rendering mode to be used based on the
    // font, size, world transform, and measuring mode.
    // <param name="fontEmSize">Logical font size in DIPs.</param>
    // <param name="dpiX">Number of pixels per logical inch in the horizontal direction.</param>
    // <param name="dpiY">Number of pixels per logical inch in the vertical direction.</param>
    // <param name="transform">Specifies the world transform.</param>
    // <param name="outlineThreshold">Specifies the quality of the graphics system's outline rendering,
    // affects the size threshold above which outline rendering is used.</param>
    // <param name="measuringMode">Specifies the method used to measure during text layout. For proper
    // glyph spacing, the function returns a rendering mode that is compatible with the specified
    // measuring mode.</param>
    // <param name="renderingMode">Receives the recommended rendering mode.</param>
    // Remarks
    // This method should be used to determine the actual rendering mode in cases where the rendering
    // mode of the rendering params object is DWRITE_RENDERING_MODE_DEFAULT.
    //
    // Standard HRESULT error code.
    function GetRecommendedRenderingMode(fontEmSize: FLOAT;
                                         dpiX: FLOAT;
                                         dpiY: FLOAT;
                                         transform: DWRITE_MATRIX;
                                         isSideways: BOOL;
                                         outlineThreshold: DWRITE_OUTLINE_THRESHOLD;
                                         measuringMode: DWRITE_MEASURING_MODE;
                                         out renderingMode: DWRITE_RENDERING_MODE): HResult; stdcall;

    // Retrieves the vertical forms of the nominal glyphs retrieved from
    // GetGlyphIndices, using the font's 'vert' table. This is used in
    // CJK vertical layout so the correct characters are shown.
    // <param name="glyphCount">Number of glyphs to retrieve.</param>
    // <param name="nominalGlyphIndices">Original glyph indices from cmap.</param>
    // <param name="verticalGlyphIndices">The vertical form of glyph indices.</param>
    // Remarks
    // Call GetGlyphIndices to get the nominal glyph indices, followed by
    // calling this to remap the to the substituted forms, when the run
    // is sideways, and the font has vertical glyph variants.
    //
    // Standard HRESULT error code.
    function GetVerticalGlyphVariants(glyphCount: UINT32;
                                      nominalGlyphIndices: UINT16;
                                      verticalGlyphIndices: UINT16): HResult; stdcall;

    // Returns whether or not the font has any vertical glyph variants.
    // Remarks
    // For OpenType fonts, this will return true if the font contains a 'vert'
    // feature.
    //
    // True if the font contains vertical glyph variants.
    function HasVerticalGlyphVariants(): BOOL; stdcall;

  end;
  IID_IDWriteFontFace1 = IDWriteFontFace1;
  {$EXTERNALSYM IID_IDWriteFontFace1}



  // Interface IDWriteFont1
  // ======================
  // The IDWriteFont interface represents a physical font in a font collection.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFont1);'}
  {$EXTERNALSYM IDWriteFont1}
  IDWriteFont1 = interface(IDWriteFont)
  ['{acd16696-8c14-4f5d-877e-fe3fc1d32738}']

    // Gets common metrics for the font in design units.
    // These metrics are applicable to all the glyphs within a font,
    // and are used by applications for layout calculations.
    // <param name="fontMetrics">Metrics structure to fill in.</param>
    procedure GetMetrics(out fontMetrics: DWRITE_FONT_METRICS1); stdcall;

    // Gets the PANOSE values from the font, used for font selection and
    // matching.
    // <param name="panose">PANOSE structure to fill in.</param>
    // Remarks
    // The function does not simulate these, such as substituting a weight or
    // proportion inferred on other values. If the font does not specify them,
    // they are all set to 'any' (0).
    //
    procedure GetPanose(out panose: DWRITE_PANOSE); stdcall;

    // Returns the list of character ranges supported by the font, which is
    // useful for scenarios like character picking, glyph display, and
    // efficient font selection lookup. This is similar to GDI's
    // GetFontUnicodeRanges, except that it returns the full Unicode range,
    // not just 16-bit UCS-2.
    // <param name="maxRangeCount">Maximum number of character ranges passed
    //     in from the client.</param>
    // <param name="unicodeRanges">Array of character ranges.</param>
    // <param name="actualRangeCount">Actual number of character ranges,
    //     regardless of the maximum count.</param>
    // Remarks
    // These ranges are from the cmap, not the OS/2::ulCodePageRange1.
    //
    // Standard HRESULT error code.
    function GetUnicodeRanges(maxRangeCount: UINT32;
                              unicodeRanges: PDWRITE_UNICODE_RANGE; // pointer to array
                              actualRangeCount: UINT32): HResult; stdcall;

    // Returns true if the font is monospaced, meaning its characters are the
    // same fixed-pitch width (non-proportional).
    function IsMonospacedFont(): BOOL; stdcall;

  end;
  IID_IDWriteFont1 = IDWriteFont1;
  {$EXTERNALSYM IID_IDWriteFont1}


  // Interface IDWriteRenderingParams1
  // =================================
  // The interface that represents text rendering settings for glyph rasterization and filtering.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteRenderingParams1);'}
  {$EXTERNALSYM IDWriteRenderingParams1}
  IDWriteRenderingParams1 = interface(IDWriteRenderingParams)
  ['{94413cf4-a6fc-4248-8b50-6674348fcad3}']
    // Gets the amount of contrast enhancement to use for grayscale antialiasing.
    // Valid values are greater than or equal to zero.
    function GetGrayscaleEnhancedContrast(): FLOAT; stdcall;
  end;
  IID_IDWriteRenderingParams1 = IDWriteRenderingParams1;
  {$EXTERNALSYM IID_IDWriteRenderingParams1}


  // Interface IDWriteTextAnalyzer1
  // ==============================
  // Analyzes various text properties for complex script processing.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalyzer1);'}
  {$EXTERNALSYM IDWriteTextAnalyzer1}
  IDWriteTextAnalyzer1 = interface(IDWriteTextAnalyzer)
  ['{80DAD800-E21F-4E83-96CE-BFCCE500DB7C}']
    // Applies spacing between characters, properly adjusting glyph clusters
    // and diacritics.
    // <param name="leadingSpacing">The spacing before each character, in reading order.</param>
    // <param name="trailingSpacing">The spacing after each character, in reading order.</param>
    // <param name="minimumAdvanceWidth">The minimum advance of each character,
    //     to prevent characters from becoming too thin or zero-width. This
    //     must be zero or greater.</param>
    // <param name="textLength">The length of the clustermap and original text.</param>
    // <param name="glyphCount">The number of glyphs.</param>
    // <param name="clusterMap">Mapping from character ranges to glyph ranges.</param>
    // <param name="glyphAdvances">The advance width of each glyph.</param>
    // <param name="glyphOffsets">The offset of the origin of each glyph.</param>
    // <param name="glyphProperties">Properties of each glyph, from GetGlyphs.</param>
    // <param name="modifiedGlyphAdvances">The new advance width of each glyph.</param>
    // <param name="modifiedGlyphOffsets">The new offset of the origin of each glyph.</param>
    // Remarks
    // The input and output advances/offsets are allowed to alias the same array.
    //
    // Standard HRESULT error code.
    function ApplyCharacterSpacing(leadingSpacing: FLOAT;
                                   trailingSpacing: FLOAT;
                                   minimumAdvanceWidth: FLOAT;
                                   textLength: UINT32;
                                   glyphCount: UINT32;
                                   clusterMap: UINT16;
                                   glyphAdvances: FLOAT;
                                   glyphOffsets: DWRITE_GLYPH_OFFSET;
                                   glyphProperties: DWRITE_SHAPING_GLYPH_PROPERTIES;
                                   modifiedGlyphAdvances: FLOAT;
                                   modifiedGlyphOffsets: PDWRITE_GLYPH_OFFSET): HResult; stdcall;

    // Retrieves the given baseline from the font.
    // <param name="fontFace">The font face to read.</param>
    // <param name="baseline">The baseline of interest.</param>
    // <param name="isVertical">Whether the baseline is vertical or horizontal.</param>
    // <param name="isSimulationAllowed">Simulate the baseline if it is missing in the font.</param>
    // <param name="scriptAnalysis">Script analysis result from AnalyzeScript.</param>
    // <param name="localeName">The language of the run.</param>
    // <param name="baselineCoordinate">The baseline coordinate value in design units.</param>
    // <param name="exists">Whether the returned baseline exists in the font.</param>
    // Remarks
    // If the baseline does not exist in the font, it is not considered an
    // error, but the function will return exists = false. You may then use
    // heuristics to calculate the missing base, or, if the flag
    // simulationAllowed is true, the function will compute a reasonable
    // approximation for you.
    //
    // Standard HRESULT error code.
    function GetBaseline(fontFace: IDWriteFontFace;
                         baseline: DWRITE_BASELINE;
                         isVertical: BOOL;
                         isSimulationAllowed: BOOL;
                         scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                         localeName: WideChar;
                         out baselineCoordinate: INT32;
                         out exists: BOOL): HResult; stdcall;

    // Analyzes a text range for script orientation, reading text and
    // attributes from the source and reporting results to the sink.
    // <param name="analysisSource">Source object to analyze.</param>
    // <param name="textPosition">Starting position within the source object.</param>
    // <param name="textLength">Length to analyze.</param>
    // <param name="analysisSink">Callback object.</param>
    // Standard HRESULT error code.
    // Remarks
    // All bidi analysis should be resolved before calling this.
    //
    function AnalyzeVerticalGlyphOrientation(analysisSource: IDWriteTextAnalysisSource1;
                                             textPosition: UINT32;
                                             textLength: UINT32;
                                             analysisSink: IDWriteTextAnalysisSink1): HResult; stdcall;

    // Returns 2x3 transform matrix for the respective angle to draw the
    // glyph run.
    // <param name="glyphOrientationAngle">The angle reported into
    //     SetGlyphOrientation.</param>
    // <param name="isSideways">Whether the run's glyphs are sideways or not.</param>
    // <param name="transform">Returned transform.</param>
    //
    // Standard HRESULT error code.
    // Remarks
    // The returned displacement is zero.
    //
    function GetGlyphOrientationTransform(glyphOrientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                                          isSideways: BOOL;
                                          out transform: DWRITE_MATRIX): HResult; stdcall;

    // Returns the properties for a given script.
    // <param name="scriptAnalysis">The script for a run of text returned
    //     from IDWriteTextAnalyzer::AnalyzeScript.</param>
    // <param name="scriptProperties">Information for the script.</param>
    // Returns properties for the given script. If the script is invalid,
    // it returns generic properties for the unknown script and E_INVALIDARG.
    function GetScriptProperties(scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                                 out scriptProperties: PDWRITE_SCRIPT_PROPERTIES): HResult; stdcall;

    // Determines the complexity of text, and whether or not full script
    // shaping needs to be called (GetGlyphs).
    // <param name="fontFace">The font face to read.</param>
    // <param name="textLength">Length of the text to check.</param>
    // <param name="textString">The text to check for complexity. This string
    //     may be UTF-16, but any supplementary characters will be considered
    //     complex.</param>
    // <param name="isTextSimple">If true, the text is simple, and the
    //     glyphIndices array will already have the nominal glyphs for you.
    //     Otherwise you need to call GetGlyphs to properly shape complex
    //     scripts and OpenType features.
    //     </param>
    // <param name="textLengthRead">The length read of the text run with the
    //     same complexity, simple or complex. You may call again from that
    //     point onward.</param>
    // <param name="glyphIndices">Optional glyph indices for the text. If the
    //     function returned that the text was simple, you already have the
    //     glyphs you need. Otherwise the glyph indices are not meaningful,
    //     and you should call shaping instead.</param>
    // Remarks
    // Text is not simple if the characters are part of a script that has
    // complex shaping requirements, require bidi analysis, combine with
    // other characters, reside in the supplementary planes, or have glyphs
    // which participate in standard OpenType features. The length returned
    // will not split combining marks from their base characters.
    //
    // Standard HRESULT error code.

    function GetTextComplexity(textString: WideChar;
                               textLength: UINT32;
                               fontFace: IDWriteFontFace;
                               isTextSimple: BOOL;
                               textLengthRead: UINT32;
                               glyphIndices: UINT16): HResult; stdcall;

    // Retrieves justification opportunity information for each of the glyphs
    // given the text and shaping glyph properties.
    // <param name="fontFace">Font face that was used for shaping. This is
    //     mainly important for returning correct results of the kashida
    //     width.</param>
    // <param name="fontEmSize">Font em size used for the glyph run.</param>
    // <param name="scriptAnalysis">Script of the text from the itemizer.</param>
    // <param name="textLength">Length of the text.</param>
    // <param name="glyphCount">Number of glyphs.</param>
    // <param name="textString">Characters used to produce the glyphs.</param>
    // <param name="clusterMap">Clustermap produced from shaping.</param>
    // <param name="glyphProperties">Glyph properties produced from shaping.</param>
    // <param name="justificationOpportunities">Receives information for the
    //     allowed justification expansion/compression for each glyph.</param>
    // Remarks
    // This function is called per-run, after shaping is done via GetGlyphs().
    // Note this function only supports natural metrics (DWRITE_MEASURING_MODE_NATURAL).
    //
    // Standard HRESULT error code.
    function GetJustificationOpportunities(fontFace: IDWriteFontFace;
                                           fontEmSize: FLOAT;
                                           scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                                           textLength: UINT32;
                                           glyphCount: UINT32;
                                           textString: WideChar;
                                           clusterMap: UINT16;
                                           glyphProperties: DWRITE_SHAPING_GLYPH_PROPERTIES;
                                           out justificationOpportunities: DWRITE_JUSTIFICATION_OPPORTUNITY): HResult; stdcall;

    // Justifies an array of glyph advances to fit the line width.
    // <param name="lineWidth">Width of the line.</param>
    // <param name="glyphCount">Number of glyphs.</param>
    // <param name="justificationOpportunities">Opportunities per glyph. Call
    //     GetJustificationOpportunities() to get suitable opportunities
    //     according to script.</param>
    // <param name="glyphAdvances">Original glyph advances from shaping.</param>
    // <param name="glyphOffsets">Original glyph offsets from shaping.</param>
    // <param name="justifiedGlyphAdvances">Justified glyph advances.</param>
    // <param name="justifiedGlyphOffsets">Justified glyph offsets.</param>
    // Remarks
    // This is called after all the opportunities have been collected, and it
    // spans across the entire line. The input and output arrays are allowed
    // to alias each other, permitting in-place update.
    //
    // Standard HRESULT error code.
    function JustifyGlyphAdvances(lineWidth: FLOAT;
                                  glyphCount: UINT32;
                                  justificationOpportunities: DWRITE_JUSTIFICATION_OPPORTUNITY;
                                  glyphAdvances: FLOAT;
                                  glyphOffsets: DWRITE_GLYPH_OFFSET;
                                  out justifiedGlyphAdvances: PFLOAT;
                                  out justifiedGlyphOffsets: PDWRITE_GLYPH_OFFSET): HResult; stdcall;

    // Fills in new glyphs for complex scripts where justification increased
    // the advances of glyphs, such as Arabic with kashida.
    // <param name="fontFace">Font face used for shaping.</param>
    // <param name="fontEmSize">Font em size used for the glyph run.</param>
    // <param name="scriptAnalysis">Script of the text from the itemizer.</param>
    // <param name="textLength">Length of the text.</param>
    // <param name="glyphCount">Number of glyphs.</param>
    // <param name="maxGlyphCount">Maximum number of output glyphs allocated
    //     by caller.</param>
    // <param name="clusterMap">Clustermap produced from shaping.</param>
    // <param name="glyphIndices">Original glyphs produced from shaping.</param>
    // <param name="glyphAdvances">Original glyph advances produced from shaping.</param>
    // <param name="justifiedGlyphAdvances">Justified glyph advances from
    //     JustifyGlyphAdvances().</param>
    // <param name="justifiedGlyphOffsets">Justified glyph offsets from
    //     JustifyGlyphAdvances().</param>
    // <param name="glyphProperties">Properties of each glyph, from GetGlyphs.</param>
    // <param name="actualGlyphCount">The new glyph count written to the
    //     modified arrays, or the needed glyph count if the size is not
    //     large enough.</param>
    // <param name="modifiedClusterMap">Updated clustermap.</param>
    // <param name="modifiedGlyphIndices">Updated glyphs with new glyphs
    //     inserted where needed.</param>
    // <param name="modifiedGlyphAdvances">Updated glyph advances.</param>
    // <param name="modifiedGlyphOffsets">Updated glyph offsets.</param>
    // Remarks
    // This is called after the line has been justified, and it is per-run.
    // It only needs to be called if the script has a specific justification
    // character via GetScriptProperties, and it is mainly for cursive scripts
    // like Arabic. If maxGlyphCount is not large enough, the error
    // E_NOT_SUFFICIENT_BUFFER will be returned, with actualGlyphCount holding
    // the final/needed glyph count.
    //
    // Standard HRESULT error code.

    function GetJustifiedGlyphs(fontFace: IDWriteFontFace;
                                fontEmSize: FLOAT;
                                scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                                textLength: UINT32;
                                glyphCount: UINT32;
                                maxGlyphCount: UINT32;
                                clusterMap: UINT16;
                                glyphIndices: UINT16;
                                glyphAdvances: FLOAT;
                                justifiedGlyphAdvances: FLOAT;
                                justifiedGlyphOffsets: DWRITE_GLYPH_OFFSET;
                                glyphProperties: DWRITE_SHAPING_GLYPH_PROPERTIES;
                                out actualGlyphCount: UINT32;
                                out modifiedClusterMap: UINT16;
                                out modifiedGlyphIndices: UINT16;
                                out modifiedGlyphAdvances: FLOAT;
                                out modifiedGlyphOffsets: DWRITE_GLYPH_OFFSET): HResult; stdcall;

  end;
  IID_IDWriteTextAnalyzer1 = IDWriteTextAnalyzer1;
  {$EXTERNALSYM IID_IDWriteTextAnalyzer1}


  // Interface IDWriteTextAnalysisSource1
  // ====================================
  // The interface implemented by the client to provide needed information to
  // the text analyzer, such as the text and associated text properties.
  // If any of these callbacks returns an error, the analysis functions will
  // stop prematurely and return a callback error.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalysisSource1);'}
  {$EXTERNALSYM IDWriteTextAnalysisSource1}
  IDWriteTextAnalysisSource1 = interface(IDWriteTextAnalysisSource)
  ['{639CFAD8-0FB4-4B21-A58A-067920120009}']
    // The text analyzer calls back to this to get the desired glyph
    // orientation and resolved bidi level, which it uses along with the
    // script properties of the text to determine the actual orientation of
    // each character, which it reports back to the client via the sink
    // SetGlyphOrientation method.
    // <param name="textPosition">First position of the piece to obtain. All
    //     positions are in UTF-16 code-units, not whole characters, which
    //     matters when supplementary characters are used.</param>
    // <param name="textLength">Number of UTF-16 units of the retrieved chunk.
    //     The returned length is not the length of the block, but the length
    //     remaining in the block, from the given position until its end.
    //     So querying for a position that is 75 positions into a 100
    //     postition block would return 25.</param>
    // <param name="glyphOrientation">The type of glyph orientation the
    //     client wants for this range, up to the returned text length.</param>
    // <param name="bidiLevel">The bidi level for this range up to
    //     the returned text length, which comes from an earlier
    //     bidirectional analysis.</param>
    // Standard HRESULT error code. Returning an error will abort the
    // analysis.
    function GetVerticalGlyphOrientation(textPosition: UINT32;
                                         out textLength: UINT32;
                                         out glyphOrientation: DWRITE_VERTICAL_GLYPH_ORIENTATION;
                                         out bidiLevel: UINT8): HResult; stdcall;
  end;
  IID_IDWriteTextAnalysisSource1 = IDWriteTextAnalysisSource1;
  {$EXTERNALSYM IID_IDWriteTextAnalysisSource1}


  // Interface IDWriteTextAnalysisSink1
  // ==================================
  // The interface implemented by the client to receive the
  // output of the text analyzers.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalysisSink1);'}
  {$EXTERNALSYM IDWriteTextAnalysisSink1}
  IDWriteTextAnalysisSink1 = interface(IDWriteTextAnalysisSink)
  ['{B0D941A0-85E7-4D8B-9FD3-5CED9934482A}']

    // The text analyzer calls back to this to report the actual orientation
    // of each character for shaping and drawing.
    // <param name="textPosition">Starting position to report from.</param>
    // <param name="textLength">Number of UTF-16 units of the reported range.</param>
    // <param name="glyphOrientationAngle">Angle of the glyphs within the text
    //     range (pass to GetGlyphOrientationTransform to get the world
    //     relative transform).</param>
    // <param name="adjustedBidiLevel">The adjusted bidi level to be used by
    //     the client layout for reordering runs. This will differ from the
    //     resolved bidi level retrieved from the source for cases such as
    //     Arabic stacked top-to-bottom, where the glyphs are still shaped
    //     as RTL, but the runs are TTB along with any CJK or Latin.</param>
    // <param name="isSideways">Whether the glyphs are rotated on their side,
    //     which is the default case for CJK and the case stacked Latin</param>
    // <param name="isRightToLeft">Whether the script should be shaped as
    //     right-to-left. For Arabic stacked top-to-bottom, even when the
    //     adjusted bidi level is coerced to an even level, this will still
    //     be true.</param>
    // A successful code or error code to abort analysis.
    function SetGlyphOrientation(textPosition: UINT32;
                                 textLength: UINT32;
                                 glyphOrientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                                 adjustedBidiLevel: UINT8;
                                 isSideways: BOOL;
                                 isRightToLeft: BOOL): HResult; stdcall;

  end;
  IID_IDWriteTextAnalysisSink1 = IDWriteTextAnalysisSink1;
  {$EXTERNALSYM IID_IDWriteTextAnalysisSink1}


  // Interface IDWriteTextLayout1
  // ============================
  // The IDWriteTextLayout1 interface represents a block of text after it has
  // been fully analyzed and formatted.
  //
  // All coordinates are in device independent pixels (DIPs).
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextLayout1);'}
  {$EXTERNALSYM IDWriteTextLayout1}
  IDWriteTextLayout1 = interface(IDWriteTextLayout)
  ['{9064D822-80A7-465C-A986-DF65F78B8FEB}']
    // Enables/disables pair-kerning on the given range.
    // <param name="isPairKerningEnabled">The Boolean flag indicates whether text is pair-kerned.</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // Standard HRESULT error code.
    function SetPairKerning(isPairKerningEnabled: BOOL;
                            textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Get whether or not pair-kerning is enabled at given position.
    // <param name="currentPosition">The current text position.</param>
    // <param name="isPairKerningEnabled">The Boolean flag indicates whether text is pair-kerned.</param>
    // <param name="textRange">The position range of the current format.</param>
    // Standard HRESULT error code.
    function GetPairKerning(currentPosition: UINT32;
                            out isPairKerningEnabled: PBOOL;
                            textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Sets the spacing between characters.
    // <param name="leadingSpacing">The spacing before each character, in reading order.</param>
    // <param name="trailingSpacing">The spacing after each character, in reading order.</param>
    // <param name="minimumAdvanceWidth">The minimum advance of each character,
    //     to prevent characters from becoming too thin or zero-width. This
    //     must be zero or greater.</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // Standard HRESULT error code.
    function SetCharacterSpacing(leadingSpacing: FLOAT;
                                 trailingSpacing: FLOAT;
                                 minimumAdvanceWidth: FLOAT;
                                 textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Gets the spacing between characters.
    // <param name="currentPosition">The current text position.</param>
    // <param name="leadingSpacing">The spacing before each character, in reading order.</param>
    // <param name="trailingSpacing">The spacing after each character, in reading order.</param>
    // <param name="minimumAdvanceWidth">The minimum advance of each character,
    //     to prevent characters from becoming too thin or zero-width. This
    //     must be zero or greater.</param>
    // <param name="textRange">The position range of the current format.</param>
    // Standard HRESULT error code.
    function GetCharacterSpacing(currentPosition: UINT32;
                                 out leadingSpacing: FLOAT;
                                 out trailingSpacing: FLOAT;
                                 out minimumAdvanceWidth: FLOAT;
                                 textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;
  end;
  IID_IDWriteTextLayout1 = IDWriteTextLayout1;
  {$EXTERNALSYM IID_IDWriteTextLayout1}


  // Represents the type of antialiasing to use for text when the rendering mode calls for
  // antialiasing.
  PDWRITE_TEXT_ANTIALIAS_MODE = ^DWRITE_TEXT_ANTIALIAS_MODE;
  DWRITE_TEXT_ANTIALIAS_MODE = (
    // ClearType antialiasing computes coverage independently for the red, green, and blue
    // color elements of each pixel. This allows for more detail than conventional antialiasing.
    // However, because there is no one alpha value for each pixel, ClearType is not suitable
    // rendering text onto a transparent intermediate bitmap.
    DWRITE_TEXT_ANTIALIAS_MODE_CLEARTYPE,
    // Grayscale antialiasing computes one coverage value for each pixel. Because the alpha
    // value of each pixel is well-defined, text can be rendered onto a transparent bitmap,
    // which can then be composited with other content. Note that grayscale rendering with
    // IDWriteBitmapRenderTarget1 uses premultiplied alpha.
    DWRITE_TEXT_ANTIALIAS_MODE_GRAYSCALE);
  {$EXTERNALSYM DWRITE_TEXT_ANTIALIAS_MODE}


  // Interface IDWriteBitmapRenderTarget1
  // ====================================
  // Encapsulates a 32-bit device independent bitmap and device context, which can be used for rendering glyphs.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteBitmapRenderTarget1);'}
  {$EXTERNALSYM IDWriteBitmapRenderTarget1}
  IDWriteBitmapRenderTarget1 = interface(IDWriteBitmapRenderTarget)
  ['{791e8298-3ef3-4230-9880-c9bdecc42064}']

    // Gets the current text antialiasing mode of the bitmap render target.
    // Returns the antialiasing mode.
    function GetTextAntialiasMode(): DWRITE_TEXT_ANTIALIAS_MODE; stdcall;

    // Sets the current text antialiasing mode of the bitmap render target.
    // Returns S_OK if successful, or E_INVALIDARG if the argument is not valid.
    // Remarks
    // The antialiasing mode of a newly-created bitmap render target defaults to
    // DWRITE_TEXT_ANTIALIAS_MODE_CLEARTYPE. An application can change the antialiasing
    // mode by calling SetTextAntialiasMode. For example, an application might specify
    // grayscale antialiasing when rendering text onto a transparent bitmap.
    //
    function SetTextAntialiasMode(antialiasMode: DWRITE_TEXT_ANTIALIAS_MODE): HResult; stdcall;


  end;
  IID_IDWriteBitmapRenderTarget1 = IDWriteBitmapRenderTarget1;
  {$EXTERNALSYM IID_IDWriteBitmapRenderTarget1}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


// DWRITE_SCRIPT_PROPERTIES
function DWRITE_SCRIPT_PROPERTIES.GetBits(const aIndex: Integer): Integer;
begin
  Result := GetDWordBits(Flags,
                         aIndex);
end;

procedure DWRITE_SCRIPT_PROPERTIES.SetBits(const aIndex: Integer; const aValue: Integer);
begin
  SetDWordBits(Flags,
               aIndex,
               aValue);
end;

// DWRITE_JUSTIFICATION_OPPORTUNITY
function DWRITE_JUSTIFICATION_OPPORTUNITY.GetBits(const aIndex: Integer): Integer;
begin
  Result := GetDWordBits(Flags,
                         aIndex);
end;

procedure DWRITE_JUSTIFICATION_OPPORTUNITY.SetBits(const aIndex: Integer;
                                                   const aValue: Integer);
begin
  SetDWordBits(Flags,
               aIndex,
               aValue);
end;

  // Implement Additional functions here.

end.

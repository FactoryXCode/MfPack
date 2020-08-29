// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectWrite
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DWrite.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows Vista or later.
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
// Source: dwrite.h
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
//
//==============================================================================
unit WinApi.DirectX.DWrite;

  {$HPPEMIT '#include "dwrite.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {DirectX}
  WinApi.DirectX.DCommon;

  {$WEAKPACKAGEUNIT ON}

const

  // Maximum alpha value in a texture returned by IDWriteGlyphRunAnalysis.CreateAlphaTexture.
  DWRITE_ALPHA_MAX                    = 255;
  {$EXTERNALSYM DWRITE_ALPHA_MAX}

type
  // The type of a font represented by a single font file.
  // Font formats that consist of multiple files, e.g. Type 1 .PFM and .PFB, have
  // separate enum values for each of the file type.
  PDWRITE_FONT_FILE_TYPE = ^DWRITE_FONT_FILE_TYPE;
  DWRITE_FONT_FILE_TYPE = DWord;
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE}
const
  // Font type is not recognized by the DirectWrite font system.
  DWRITE_FONT_FILE_TYPE_UNKNOWN             = DWRITE_FONT_FILE_TYPE(0);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_UNKNOWN}
  // OpenType font with CFF outlines.
  DWRITE_FONT_FILE_TYPE_CFF                 = DWRITE_FONT_FILE_TYPE(1);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_CFF}
  // OpenType font with TrueType outlines.
  DWRITE_FONT_FILE_TYPE_TRUETYPE            = DWRITE_FONT_FILE_TYPE(2);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_TRUETYPE}
  // OpenType font that contains a TrueType collection.
  DWRITE_FONT_FILE_TYPE_OPENTYPE_COLLECTION = DWRITE_FONT_FILE_TYPE(3);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_OPENTYPE_COLLECTION}
  // Type 1 PFM font.
  DWRITE_FONT_FILE_TYPE_TYPE1_PFM           = DWRITE_FONT_FILE_TYPE(4);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_TYPE1_PFM}
  // Type 1 PFB font.
  DWRITE_FONT_FILE_TYPE_TYPE1_PFB           = DWRITE_FONT_FILE_TYPE(5);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_TYPE1_PFB}
  // Vector .FON font.
  DWRITE_FONT_FILE_TYPE_VECTOR              = DWRITE_FONT_FILE_TYPE(6);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_VECTOR}
  // Bitmap .FON font.
  DWRITE_FONT_FILE_TYPE_BITMAP              = DWRITE_FONT_FILE_TYPE(7);
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_BITMAP}
  // The following name is obsolete, but kept as an alias to avoid breaking existing code.
  DWRITE_FONT_FILE_TYPE_TRUETYPE_COLLECTION = DWRITE_FONT_FILE_TYPE_OPENTYPE_COLLECTION;
  {$EXTERNALSYM DWRITE_FONT_FILE_TYPE_TRUETYPE_COLLECTION}

type
  // The file format of a complete font face.
  // Font formats that consist of multiple files, e.g. Type 1 .PFM and .PFB, have
  // a single enum entry.
  PDWRITE_FONT_FACE_TYPE = ^DWRITE_FONT_FACE_TYPE;
  DWRITE_FONT_FACE_TYPE = DWord;
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE}
const
  // OpenType font face with CFF outlines.
  DWRITE_FONT_FACE_TYPE_CFF                 = DWRITE_FONT_FACE_TYPE(0);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_CFF}
  // OpenType font face with TrueType outlines.
  DWRITE_FONT_FACE_TYPE_TRUETYPE            = DWRITE_FONT_FACE_TYPE(1);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_TRUETYPE}
  // OpenType font face that is a part of a TrueType or CFF collection.
  DWRITE_FONT_FACE_TYPE_OPENTYPE_COLLECTION = DWRITE_FONT_FACE_TYPE(2);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_OPENTYPE_COLLECTION}
  // A Type 1 font face.
  DWRITE_FONT_FACE_TYPE_TYPE1               = DWRITE_FONT_FACE_TYPE(3);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_TYPE1}
  // A vector .FON format font face.
  DWRITE_FONT_FACE_TYPE_VECTOR              = DWRITE_FONT_FACE_TYPE(4);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_VECTOR}
  // A bitmap .FON format font face.
  DWRITE_FONT_FACE_TYPE_BITMAP              = DWRITE_FONT_FACE_TYPE(5);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_BITMAP}
  // Font face type is not recognized by the DirectWrite font system.
  DWRITE_FONT_FACE_TYPE_UNKNOWN             = DWRITE_FONT_FACE_TYPE(6);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_UNKNOWN}
  // The font data includes only the CFF table from an OpenType CFF font.
  // This font face type can be used only for embedded fonts (i.e., custom
  // font file loaders) and the resulting font face object supports only the
  // minimum functionality necessary to render glyphs.
  DWRITE_FONT_FACE_TYPE_RAW_CFF             = DWRITE_FONT_FACE_TYPE(7);
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_RAW_CFF}
  // The following name is obsolete, but kept as an alias to avoid breaking existing code.
  DWRITE_FONT_FACE_TYPE_TRUETYPE_COLLECTION = DWRITE_FONT_FACE_TYPE_OPENTYPE_COLLECTION;
  {$EXTERNALSYM DWRITE_FONT_FACE_TYPE_TRUETYPE_COLLECTION}

type
  // Specifies algorithmic style simulations to be applied to the font face.
  // Bold and oblique simulations can be combined via bitwise OR operation.
  PDWRITE_FONT_SIMULATIONS = ^DWRITE_FONT_SIMULATIONS;
  DWRITE_FONT_SIMULATIONS = DWord;
  {$EXTERNALSYM DWRITE_FONT_SIMULATIONS}
const
  // No simulations are performed.
  DWRITE_FONT_SIMULATIONS_NONE    = DWRITE_FONT_SIMULATIONS($0000);
  {$EXTERNALSYM DWRITE_FONT_SIMULATIONS_NONE}
  // Algorithmic emboldening is performed.
  DWRITE_FONT_SIMULATIONS_BOLD    = DWRITE_FONT_SIMULATIONS($0001);
  {$EXTERNALSYM DWRITE_FONT_SIMULATIONS_BOLD}
  // Algorithmic italicization is performed.
  DWRITE_FONT_SIMULATIONS_OBLIQUE = DWRITE_FONT_SIMULATIONS($0002);
  {$EXTERNALSYM DWRITE_FONT_SIMULATIONS_OBLIQUE}

type
  // The font weight enumeration describes common values for degree of blackness or thickness of strokes of characters in a font.
  // Font weight values less than 1 or greater than 999 are considered to be invalid, and they are rejected by font API functions.
  PDWRITE_FONT_WEIGHT = ^DWRITE_FONT_WEIGHT;
  DWRITE_FONT_WEIGHT = DWord;
  {$EXTERNALSYM DWRITE_FONT_WEIGHT}
const
  // Predefined font weight : Thin (100).
  DWRITE_FONT_WEIGHT_THIN        = DWRITE_FONT_WEIGHT(100);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_THIN}
  // Predefined font weight : Extra-light (200).
  DWRITE_FONT_WEIGHT_EXTRA_LIGHT = DWRITE_FONT_WEIGHT(200);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_EXTRA_LIGHT}
  // Predefined font weight : Ultra-light (200).
  DWRITE_FONT_WEIGHT_ULTRA_LIGHT = DWRITE_FONT_WEIGHT(200);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_ULTRA_LIGHT}
  // Predefined font weight : Light (300).
  DWRITE_FONT_WEIGHT_LIGHT       = DWRITE_FONT_WEIGHT(300);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_LIGHT}
  // Predefined font weight : Semi-light (350).
  DWRITE_FONT_WEIGHT_SEMI_LIGHT  = DWRITE_FONT_WEIGHT(350);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_SEMI_LIGHT}
  // Predefined font weight : Normal (400).
  DWRITE_FONT_WEIGHT_NORMAL      = DWRITE_FONT_WEIGHT(400);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_NORMAL}
  // Predefined font weight : Regular (400).
  DWRITE_FONT_WEIGHT_REGULAR     = DWRITE_FONT_WEIGHT(400);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_REGULAR}
  // Predefined font weight : Medium (500).
  DWRITE_FONT_WEIGHT_MEDIUM      = DWRITE_FONT_WEIGHT(500);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_MEDIUM}
  // Predefined font weight : Demi-bold (600).
  DWRITE_FONT_WEIGHT_DEMI_BOLD   = DWRITE_FONT_WEIGHT(600);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_DEMI_BOLD}
  // Predefined font weight : Semi-bold (600).
  DWRITE_FONT_WEIGHT_SEMI_BOLD   = DWRITE_FONT_WEIGHT(600);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_SEMI_BOLD}
  // Predefined font weight : Bold (700).
  DWRITE_FONT_WEIGHT_BOLD        = DWRITE_FONT_WEIGHT(700);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_BOLD}
  // Predefined font weight : Extra-bold (800).
  DWRITE_FONT_WEIGHT_EXTRA_BOLD  = DWRITE_FONT_WEIGHT(800);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_EXTRA_BOLD}
  // Predefined font weight : Ultra-bold (800).
  DWRITE_FONT_WEIGHT_ULTRA_BOLD  = DWRITE_FONT_WEIGHT(800);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_ULTRA_BOLD}
  // Predefined font weight : Black (900).
  DWRITE_FONT_WEIGHT_BLACK       = DWRITE_FONT_WEIGHT(900);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_BLACK}
  // Predefined font weight : Heavy (900).
  DWRITE_FONT_WEIGHT_HEAVY       = DWRITE_FONT_WEIGHT(900);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_HEAVY}
  // Predefined font weight : Extra-black (950).
  DWRITE_FONT_WEIGHT_EXTRA_BLACK = DWRITE_FONT_WEIGHT(950);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_EXTRA_BLACK}
  // Predefined font weight : Ultra-black (950).
  DWRITE_FONT_WEIGHT_ULTRA_BLACK = DWRITE_FONT_WEIGHT(950);
  {$EXTERNALSYM DWRITE_FONT_WEIGHT_ULTRA_BLACK}

type
  // The font stretch enumeration describes relative change from the normal aspect ratio
  // as specified by a font designer for the glyphs in a font.
  // Values less than 1 or greater than 9 are considered to be invalid, and they are rejected by font API functions.
  PDWRITE_FONT_STRETCH = ^DWRITE_FONT_STRETCH;
  DWRITE_FONT_STRETCH = DWord;
  {$EXTERNALSYM DWRITE_FONT_STRETCH}
const
  // Predefined font stretch : Not known (0).
  DWRITE_FONT_STRETCH_UNDEFINED       = DWRITE_FONT_STRETCH(0);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_UNDEFINED}
  // Predefined font stretch : Ultra-condensed (1).
  DWRITE_FONT_STRETCH_ULTRA_CONDENSED = DWRITE_FONT_STRETCH(1);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_ULTRA_CONDENSED}
  // Predefined font stretch : Extra-condensed (2).
  DWRITE_FONT_STRETCH_EXTRA_CONDENSED = DWRITE_FONT_STRETCH(2);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_EXTRA_CONDENSED}
  // Predefined font stretch : Condensed (3).
  DWRITE_FONT_STRETCH_CONDENSED       = DWRITE_FONT_STRETCH(3);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_CONDENSED}
  // Predefined font stretch : Semi-condensed (4).
  DWRITE_FONT_STRETCH_SEMI_CONDENSED  = DWRITE_FONT_STRETCH(4);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_SEMI_CONDENSED}
  // Predefined font stretch : Normal (5).
  DWRITE_FONT_STRETCH_NORMAL          = DWRITE_FONT_STRETCH(5);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_NORMAL}
  // Predefined font stretch : Medium (5).
  DWRITE_FONT_STRETCH_MEDIUM          = DWRITE_FONT_STRETCH(5);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_MEDIUM}
  // Predefined font stretch : Semi-expanded (6).
  DWRITE_FONT_STRETCH_SEMI_EXPANDED   = DWRITE_FONT_STRETCH(6);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_SEMI_EXPANDED}
  // Predefined font stretch : Expanded (7).
  DWRITE_FONT_STRETCH_EXPANDED        = DWRITE_FONT_STRETCH(7);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_EXPANDED}
  // Predefined font stretch : Extra-expanded (8).
  DWRITE_FONT_STRETCH_EXTRA_EXPANDED  = DWRITE_FONT_STRETCH(8);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_EXTRA_EXPANDED}
  // Predefined font stretch : Ultra-expanded (9).
  DWRITE_FONT_STRETCH_ULTRA_EXPANDED  = DWRITE_FONT_STRETCH(9);
  {$EXTERNALSYM DWRITE_FONT_STRETCH_ULTRA_EXPANDED}

type
  // The font style enumeration describes the slope style of a font face, such as Normal, Italic or Oblique.
  // Values other than the ones defined in the enumeration are considered to be invalid, and they are rejected by font API functions.
  PDWRITE_FONT_STYLE = ^DWRITE_FONT_STYLE;
  DWRITE_FONT_STYLE = DWord;
  {$EXTERNALSYM DWRITE_FONT_STYLE}
const
  // Font slope style : Normal.
  DWRITE_FONT_STYLE_NORMAL  = DWRITE_FONT_STYLE(0);
  {$EXTERNALSYM DWRITE_FONT_STYLE_NORMAL}
  // Font slope style : Oblique.
  DWRITE_FONT_STYLE_OBLIQUE = DWRITE_FONT_STYLE(1);
  {$EXTERNALSYM DWRITE_FONT_STYLE_OBLIQUE}
  // Font slope style : Italic.
  DWRITE_FONT_STYLE_ITALIC  = DWRITE_FONT_STYLE(2);
  {$EXTERNALSYM DWRITE_FONT_STYLE_ITALIC}

type
  // The informational string enumeration identifies a string in a font.
  PDWRITE_INFORMATIONAL_STRING_ID = ^DWRITE_INFORMATIONAL_STRING_ID;
  DWRITE_INFORMATIONAL_STRING_ID = DWord;
const
  // Unspecified name ID.
  DWRITE_INFORMATIONAL_STRING_NONE                             = DWRITE_INFORMATIONAL_STRING_ID(0);
  // Copyright notice provided by the font.
  DWRITE_INFORMATIONAL_STRING_COPYRIGHT_NOTICE                 = DWRITE_INFORMATIONAL_STRING_ID(1);
  // String containing a version number.
  DWRITE_INFORMATIONAL_STRING_VERSION_STRINGS                  = DWRITE_INFORMATIONAL_STRING_ID(2);
  // Trademark information provided by the font.
  DWRITE_INFORMATIONAL_STRING_TRADEMARK                        = DWRITE_INFORMATIONAL_STRING_ID(3);
  // Name of the font manufacturer.
  DWRITE_INFORMATIONAL_STRING_MANUFACTURER                     = DWRITE_INFORMATIONAL_STRING_ID(4);
  // Name of the font designer.
  DWRITE_INFORMATIONAL_STRING_DESIGNER                         = DWRITE_INFORMATIONAL_STRING_ID(5);
  // URL of font designer (with protocol, e.g., http://, ftp://).
  DWRITE_INFORMATIONAL_STRING_DESIGNER_URL                     = DWRITE_INFORMATIONAL_STRING_ID(6);
  // Description of the font. Can contain revision information, usage recommendations, history, features, etc.
  DWRITE_INFORMATIONAL_STRING_DESCRIPTION                      = DWRITE_INFORMATIONAL_STRING_ID(7);
  // URL of font vendor (with protocol, e.g., http://, ftp://). If a unique serial number is embedded in the URL, it can be used to register the font.
  DWRITE_INFORMATIONAL_STRING_FONT_VENDOR_URL                  = DWRITE_INFORMATIONAL_STRING_ID(8);
  // Description of how the font may be legally used, or different example scenarios for licensed use. This field should be written in plain language, not legalese.
  DWRITE_INFORMATIONAL_STRING_LICENSE_DESCRIPTION              = DWRITE_INFORMATIONAL_STRING_ID(9);
  // URL where additional licensing information can be found.
  DWRITE_INFORMATIONAL_STRING_LICENSE_INFO_URL                 = DWRITE_INFORMATIONAL_STRING_ID(10);
  // GDI-compatible family name. Because GDI allows a maximum of four fonts per family, fonts in the same family may have different GDI-compatible family names
  // (e.g., "Arial", "Arial Narrow", "Arial Black").
  DWRITE_INFORMATIONAL_STRING_WIN32_FAMILY_NAMES               = DWRITE_INFORMATIONAL_STRING_ID(11);
  // GDI-compatible subfamily name.
  DWRITE_INFORMATIONAL_STRING_WIN32_SUBFAMILY_NAMES            = DWRITE_INFORMATIONAL_STRING_ID(12);
  // Typographic family name preferred by the designer. This enables font designers to group more than four fonts in a single family without losing compatibility with
  // GDI. This name is typically only present if it differs from the GDI-compatible family name.
  DWRITE_INFORMATIONAL_STRING_TYPOGRAPHIC_FAMILY_NAMES         = DWRITE_INFORMATIONAL_STRING_ID(13);
  // Typographic subfamily name preferred by the designer. This name is typically only present if it differs from the GDI-compatible subfamily name.
  DWRITE_INFORMATIONAL_STRING_TYPOGRAPHIC_SUBFAMILY_NAMES      = DWRITE_INFORMATIONAL_STRING_ID(14);
  // Sample text. This can be the font name or any other text that the designer thinks is the best example to display the font in.
  DWRITE_INFORMATIONAL_STRING_SAMPLE_TEXT                      = DWRITE_INFORMATIONAL_STRING_ID(15);
  // The full name of the font, e.g. "Arial Bold", from name id 4 in the name table.
  DWRITE_INFORMATIONAL_STRING_FULL_NAME                        = DWRITE_INFORMATIONAL_STRING_ID(16);
  // The postscript name of the font, e.g. "GillSans-Bold" from name id 6 in the name table.
  DWRITE_INFORMATIONAL_STRING_POSTSCRIPT_NAME                  = DWRITE_INFORMATIONAL_STRING_ID(17);
  // The postscript CID findfont name, from name id 20 in the name table.
  DWRITE_INFORMATIONAL_STRING_POSTSCRIPT_CID_NAME              = DWRITE_INFORMATIONAL_STRING_ID(18);
  // Family name for the weight-stretch-style model.
  DWRITE_INFORMATIONAL_STRING_WEIGHT_STRETCH_STYLE_FAMILY_NAME = DWRITE_INFORMATIONAL_STRING_ID(19);
  // Script/language tag to identify the scripts or languages that the font was
  // primarily designed to support. See DWRITE_FONT_PROPERTY_ID_DESIGN_SCRIPT_LANGUAGE_TAG
  // for a longer description.
  DWRITE_INFORMATIONAL_STRING_DESIGN_SCRIPT_LANGUAGE_TAG       = DWRITE_INFORMATIONAL_STRING_ID(20);
  // Script/language tag to identify the scripts or languages that the font declares
  // it is able to support.
  DWRITE_INFORMATIONAL_STRING_SUPPORTED_SCRIPT_LANGUAGE_TAG    = DWRITE_INFORMATIONAL_STRING_ID(21);
  // Obsolete aliases kept to avoid breaking existing code.
  DWRITE_INFORMATIONAL_STRING_PREFERRED_FAMILY_NAMES           = DWRITE_INFORMATIONAL_STRING_TYPOGRAPHIC_FAMILY_NAMES;
  DWRITE_INFORMATIONAL_STRING_PREFERRED_SUBFAMILY_NAMES        = DWRITE_INFORMATIONAL_STRING_TYPOGRAPHIC_SUBFAMILY_NAMES;
  DWRITE_INFORMATIONAL_STRING_WWS_FAMILY_NAME                  = DWRITE_INFORMATIONAL_STRING_WEIGHT_STRETCH_STYLE_FAMILY_NAME;
  {$EXTERNALSYM DWRITE_INFORMATIONAL_STRING_ID}

type

  // The DWRITE_FONT_METRICS structure specifies the metrics of a font face that
  // are applicable to all glyphs within the font face.
  PDWRITE_FONT_METRICS = ^DWRITE_FONT_METRICS;
  DWRITE_FONT_METRICS = record
    // The number of font design units per em unit.
    // Font files use their own coordinate system of font design units.
    // A font design unit is the smallest measurable unit in the em square,
    // an imaginary square that is used to size and align glyphs.
    // The concept of em square is used as a reference scale factor when defining font size and device transformation semantics.
    // The size of one em square is also commonly used to compute the paragraph indentation value.
    designUnitsPerEm: UINT16;
    // Ascent value of the font face in font design units.
    // Ascent is the distance from the top of font character alignment box to English baseline.
    ascent: UINT16;
    // Descent value of the font face in font design units.
    // Descent is the distance from the bottom of font character alignment box to English baseline.
    descent: UINT16;
    // Line gap in font design units.
    // Recommended additional white space to add between lines to improve legibility. The recommended line spacing
    // (baseline-to-baseline distance) is thus the sum of ascent, descent, and lineGap. The line gap is usually
    // positive or zero but can be negative, in which case the recommended line spacing is less than the height
    // of the character alignment box.
    lineGap: INT16;
    // Cap height value of the font face in font design units.
    // Cap height is the distance from English baseline to the top of a typical English capital.
    // Capital "H" is often used as a reference character for the purpose of calculating the cap height value.
    capHeight: UINT16;
    // x-height value of the font face in font design units.
    // x-height is the distance from English baseline to the top of lowercase letter "x", or a similar lowercase character.
    xHeight: UINT16;
    // The underline position value of the font face in font design units.
    // Underline position is the position of underline relative to the English baseline.
    // The value is usually made negative in order to place the underline below the baseline.
    underlinePosition: INT16;
    // The suggested underline thickness value of the font face in font design units.
    underlineThickness: UINT16;
    // The strikethrough position value of the font face in font design units.
    // Strikethrough position is the position of strikethrough relative to the English baseline.
    // The value is usually made positive in order to place the strikethrough above the baseline.
    strikethroughPosition: INT16;
    // The suggested strikethrough thickness value of the font face in font design units.
    strikethroughThickness: UINT16
  end;
  {$EXTERNALSYM DWRITE_FONT_METRICS}


  // The DWRITE_GLYPH_METRICS structure specifies the metrics of an individual glyph.
  // The units depend on how the metrics are obtained.
  PDWRITE_GLYPH_METRICS = ^DWRITE_GLYPH_METRICS;
  DWRITE_GLYPH_METRICS = record
    // Specifies the X offset from the glyph origin to the left edge of the black box.
    // The glyph origin is the current horizontal writing position.
    // A negative value means the black box extends to the left of the origin (often true for lowercase italic 'f').
    leftSideBearing: INT32;

    // Specifies the X offset from the origin of the current glyph to the origin of the next glyph when writing horizontally.
    advanceWidth: UINT32;

    // Specifies the X offset from the right edge of the black box to the origin of the next glyph when writing horizontally.
    // The value is negative when the right edge of the black box overhangs the layout box.
    rightSideBearing: INT32;

    // Specifies the vertical offset from the vertical origin to the top of the black box.
    // Thus, a positive value adds whitespace whereas a negative value means the glyph overhangs the top of the layout box.
    topSideBearing: INT32;

    // Specifies the Y offset from the vertical origin of the current glyph to the vertical origin of the next glyph when writing vertically.
    // (Note that the term "origin" by itself denotes the horizontal origin. The vertical origin is different.
    // Its Y coordinate is specified by verticalOriginY value,
    // and its X coordinate is half the advanceWidth to the right of the horizontal origin).
    advanceHeight: UINT32;

    // Specifies the vertical distance from the black box's bottom edge to the advance height.
    // Positive when the bottom edge of the black box is within the layout box.
    // Negative when the bottom edge of black box overhangs the layout box.
    bottomSideBearing: INT32;

    // Specifies the Y coordinate of a glyph's vertical origin, in the font's design coordinate system.
    // The y coordinate of a glyph's vertical origin is the sum of the glyph's top side bearing
    // and the top (i.e. yMax) of the glyph's bounding box.
    verticalOriginY: INT32;
  end;
  {$EXTERNALSYM DWRITE_GLYPH_METRICS}


  // Optional adjustment to a glyph's position. A glyph offset changes the position of a glyph without affecting
  // the pen position. Offsets are in logical, pre-transform units.
  PDWRITE_GLYPH_OFFSET = ^DWRITE_GLYPH_OFFSET;
  DWRITE_GLYPH_OFFSET = record
    // Offset in the advance direction of the run. A positive advance offset moves the glyph to the right
    // (in pre-transform coordinates) if the run is left-to-right or to the left if the run is right-to-left.
    advanceOffset: Single;

    // Offset in the ascent direction, i.e., the direction ascenders point. A positive ascender offset moves
    // the glyph up (in pre-transform coordinates).
    ascenderOffset: Single;
  end;
  {$EXTERNALSYM DWRITE_GLYPH_OFFSET}

type
  // Specifies the type of DirectWrite factory object.
  // DirectWrite factory contains internal state such as font loader registration and cached font data.
  // In most cases it is recommended to use the shared factory object, because it allows multiple components
  // that use DirectWrite to share internal DirectWrite state and reduce memory usage.
  // However, there are cases when it is desirable to reduce the impact of a component,
  // such as a plug-in from an untrusted source, on the rest of the process by sandboxing and isolating it
  // from the rest of the process components. In such cases, it is recommended to use an isolated factory for the sandboxed
  // component.
  PDWRITE_FACTORY_TYPE = ^DWRITE_FACTORY_TYPE;
  DWRITE_FACTORY_TYPE = DWord;
  {$EXTERNALSYM DWRITE_FACTORY_TYPE}
const
  // Shared factory allow for re-use of cached font data across multiple in process components.
  // Such factories also take advantage of cross process font caching components for better performance.
  DWRITE_FACTORY_TYPE_SHARED   = DWRITE_FACTORY_TYPE(0);
  // Objects created from the isolated factory do not interact with internal DirectWrite state from other components.
  DWRITE_FACTORY_TYPE_ISOLATED = DWRITE_FACTORY_TYPE(1);

// Forwarded enums =============================================================

type
  // Represents the internal structure of a device pixel (i.e., the physical arrangement of red,
  // green, and blue color components) that is assumed for purposes of rendering text.
  PDWRITE_PIXEL_GEOMETRY = ^DWRITE_PIXEL_GEOMETRY;
  DWRITE_PIXEL_GEOMETRY = DWord;
  {$EXTERNALSYM DWRITE_PIXEL_GEOMETRY}
const
  // The red, green, and blue color components of each pixel are assumed to occupy the same point.
  DWRITE_PIXEL_GEOMETRY_FLAT = DWRITE_PIXEL_GEOMETRY(0);
  // Each pixel comprises three vertical stripes, with red on the left, green in the center, and
  // blue on the right. This is the most common pixel geometry for LCD monitors.
  DWRITE_PIXEL_GEOMETRY_RGB  = DWRITE_PIXEL_GEOMETRY(1);
  // Each pixel comprises three vertical stripes, with blue on the left, green in the center, and
  // red on the right.
  DWRITE_PIXEL_GEOMETRY_BGR  = DWRITE_PIXEL_GEOMETRY(2);

type
  // Represents a method of rendering glyphs.
  PDWRITE_RENDERING_MODE = ^DWRITE_RENDERING_MODE;
  DWRITE_RENDERING_MODE = DWord;
  {$EXTERNALSYM DWRITE_RENDERING_MODE}
const
  // Specifies that the rendering mode is determined automatically based on the font and size.
  DWRITE_RENDERING_MODE_DEFAULT                     = DWRITE_RENDERING_MODE(0);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_DEFAULT}
  // Specifies that no antialiasing is performed. Each pixel is either set to the foreground
  // color of the text or retains the color of the background.
  DWRITE_RENDERING_MODE_ALIASED                     = DWRITE_RENDERING_MODE(1);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_ALIASED}
  // Specifies that antialiasing is performed in the horizontal direction and the appearance
  // of glyphs is layout-compatible with GDI using CLEARTYPE_QUALITY. Use DWRITE_MEASURING_MODE_GDI_CLASSIC
  // to get glyph advances. The antialiasing may be either ClearType or grayscale depending on
  // the text antialiasing mode.
  DWRITE_RENDERING_MODE_GDI_CLASSIC                 = DWRITE_RENDERING_MODE(2);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_GDI_CLASSIC}
  // Specifies that antialiasing is performed in the horizontal direction and the appearance
  // of glyphs is layout-compatible with GDI using CLEARTYPE_NATURAL_QUALITY. Glyph advances
  // are close to the font design advances = DWRITE_RENDERING_MODE(0); but are still rounded to whole pixels. Use
  // DWRITE_MEASURING_MODE_GDI_NATURAL to get glyph advances. The antialiasing may be either
  // ClearType or grayscale depending on the text antialiasing mode.
  DWRITE_RENDERING_MODE_GDI_NATURAL                 = DWRITE_RENDERING_MODE(3);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_GDI_NATURAL}
  // Specifies that antialiasing is performed in the horizontal direction. This rendering
  // mode allows glyphs to be positioned with subpixel precision and is therefore suitable
  // for natural (i.e. resolution-independent) layout. The antialiasing may be either
  // ClearType or grayscale depending on the text antialiasing mode.
  DWRITE_RENDERING_MODE_NATURAL                     = DWRITE_RENDERING_MODE(4);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_NATURAL}
  // Similar to natural mode except that antialiasing is performed in both the horizontal
  // and vertical directions. This is typically used at larger sizes to make curves and
  // diagonal lines look smoother. The antialiasing may be either ClearType or grayscale
  // depending on the text antialiasing mode.
  DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC           = DWRITE_RENDERING_MODE(5);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC}
  // Specifies that rendering should bypass the rasterizer and use the outlines directly.
  // This is typically used at very large sizes.
  DWRITE_RENDERING_MODE_OUTLINE                     = DWRITE_RENDERING_MODE(6);
  {$EXTERNALSYM DWRITE_RENDERING_MODE_OUTLINE}
  // The following names are obsolete but are kept as aliases to avoid breaking existing code.
  // Each of these rendering modes may result in either ClearType or grayscale antialiasing
  // depending on the DWRITE_TEXT_ANTIALIASING_MODE.
  DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC       = DWRITE_RENDERING_MODE_GDI_CLASSIC;
  {$EXTERNALSYM DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC}
  DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL       = DWRITE_RENDERING_MODE_GDI_NATURAL;
  {$EXTERNALSYM DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL}
  DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL           = DWRITE_RENDERING_MODE_NATURAL;
  {$EXTERNALSYM DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL}
  DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL_SYMMETRIC = DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC;
  {$EXTERNALSYM DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL_SYMMETRIC}

type
  // Direction for how reading progresses.
  PDWRITE_READING_DIRECTION = ^DWRITE_READING_DIRECTION;
  DWRITE_READING_DIRECTION = DWord;
  {$EXTERNALSYM DWRITE_READING_DIRECTION}
const
  // Reading progresses from left to right.
  DWRITE_READING_DIRECTION_LEFT_TO_RIGHT = DWRITE_READING_DIRECTION(0);
  {$EXTERNALSYM DWRITE_READING_DIRECTION_LEFT_TO_RIGHT}
  // Reading progresses from right to left.
  DWRITE_READING_DIRECTION_RIGHT_TO_LEFT = DWRITE_READING_DIRECTION(1);
  {$EXTERNALSYM DWRITE_READING_DIRECTION_RIGHT_TO_LEFT}
  // Reading progresses from top to bottom.
  DWRITE_READING_DIRECTION_TOP_TO_BOTTOM = DWRITE_READING_DIRECTION(2);
  {$EXTERNALSYM DWRITE_READING_DIRECTION_TOP_TO_BOTTOM}
  // Reading progresses from bottom to top.
  DWRITE_READING_DIRECTION_BOTTOM_TO_TOP = DWRITE_READING_DIRECTION(3);
  {$EXTERNALSYM DWRITE_READING_DIRECTION_BOTTOM_TO_TOP}

type
  // Direction for how lines of text are placed relative to one another.
  PDWRITE_FLOW_DIRECTION = ^DWRITE_FLOW_DIRECTION;
  DWRITE_FLOW_DIRECTION = DWord;
  {$EXTERNALSYM DWRITE_FLOW_DIRECTION}
const
  // Text lines are placed from top to bottom.
  DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM = DWRITE_FLOW_DIRECTION(0);
  {$EXTERNALSYM DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM}
  // Text lines are placed from bottom to top.
  DWRITE_FLOW_DIRECTION_BOTTOM_TO_TOP = DWRITE_FLOW_DIRECTION(1);
  {$EXTERNALSYM DWRITE_FLOW_DIRECTION_BOTTOM_TO_TOP}
  // Text lines are placed from left to right.
  DWRITE_FLOW_DIRECTION_LEFT_TO_RIGHT = DWRITE_FLOW_DIRECTION(2);
  {$EXTERNALSYM DWRITE_FLOW_DIRECTION_LEFT_TO_RIGHT}
  // Text lines are placed from right to left.
  DWRITE_FLOW_DIRECTION_RIGHT_TO_LEFT = DWRITE_FLOW_DIRECTION(3);
  {$EXTERNALSYM DWRITE_FLOW_DIRECTION_RIGHT_TO_LEFT}

type
  // Alignment of paragraph text along the reading direction axis relative to
  // the leading and trailing edge of the layout box.
  PDWRITE_TEXT_ALIGNMENT = ^DWRITE_TEXT_ALIGNMENT;
  DWRITE_TEXT_ALIGNMENT = DWord;
  {$EXTERNALSYM DWRITE_TEXT_ALIGNMENT}
const
  // The leading edge of the paragraph text is aligned to the layout box's leading edge.
  DWRITE_TEXT_ALIGNMENT_LEADING   = DWRITE_TEXT_ALIGNMENT(0);
  {$EXTERNALSYM DWRITE_TEXT_ALIGNMENT_LEADING}
  // The trailing edge of the paragraph text is aligned to the layout box's trailing edge.
  DWRITE_TEXT_ALIGNMENT_TRAILING  = DWRITE_TEXT_ALIGNMENT(1);
  {$EXTERNALSYM DWRITE_TEXT_ALIGNMENT_TRAILING}
  // The center of the paragraph text is aligned to the center of the layout box.
  DWRITE_TEXT_ALIGNMENT_CENTER    = DWRITE_TEXT_ALIGNMENT(2);
  {$EXTERNALSYM DWRITE_TEXT_ALIGNMENT_CENTER}
  // Align text to the leading side, and also justify text to fill the lines.
  DWRITE_TEXT_ALIGNMENT_JUSTIFIED = DWRITE_TEXT_ALIGNMENT(3);
  {$EXTERNALSYM DWRITE_TEXT_ALIGNMENT_JUSTIFIED}

type
  // Alignment of paragraph text along the flow direction axis relative to the
  // flow's beginning and ending edge of the layout box.
  PDWRITE_PARAGRAPH_ALIGNMENT = ^DWRITE_PARAGRAPH_ALIGNMENT;
  DWRITE_PARAGRAPH_ALIGNMENT = DWord;
  {$EXTERNALSYM DWRITE_PARAGRAPH_ALIGNMENT}
const
  // The first line of paragraph is aligned to the flow's beginning edge of the layout box.
  DWRITE_PARAGRAPH_ALIGNMENT_NEAR   = DWRITE_PARAGRAPH_ALIGNMENT(0);
  {$EXTERNALSYM DWRITE_PARAGRAPH_ALIGNMENT_NEAR}
  // The last line of paragraph is aligned to the flow's ending edge of the layout box.
  DWRITE_PARAGRAPH_ALIGNMENT_FAR    = DWRITE_PARAGRAPH_ALIGNMENT(1);
  {$EXTERNALSYM DWRITE_PARAGRAPH_ALIGNMENT_FAR}
  // The center of the paragraph is aligned to the center of the flow of the layout box.
  DWRITE_PARAGRAPH_ALIGNMENT_CENTER = DWRITE_PARAGRAPH_ALIGNMENT(2);
  {$EXTERNALSYM DWRITE_PARAGRAPH_ALIGNMENT_CENTER}

type
  // Word wrapping in multiline paragraph.
  PDWRITE_WORD_WRAPPING = ^DWRITE_WORD_WRAPPING;
  DWRITE_WORD_WRAPPING = DWord;
  {$EXTERNALSYM DWRITE_WORD_WRAPPING}
const
  // Words are broken across lines to avoid text overflowing the layout box.
  DWRITE_WORD_WRAPPING_WRAP            = DWRITE_WORD_WRAPPING(0);
  {$EXTERNALSYM DWRITE_WORD_WRAPPING_WRAP}
  // Words are kept within the same line even when it overflows the layout box.
  // This option is often used with scrolling to reveal overflow text.
  DWRITE_WORD_WRAPPING_NO_WRAP         = DWRITE_WORD_WRAPPING(1);
  {$EXTERNALSYM DWRITE_WORD_WRAPPING_NO_WRAP}
  // Words are broken across lines to avoid text overflowing the layout box.
  // Emergency wrapping occurs if the word is larger than the maximum width.
  DWRITE_WORD_WRAPPING_EMERGENCY_BREAK = DWRITE_WORD_WRAPPING(2);
  {$EXTERNALSYM DWRITE_WORD_WRAPPING_EMERGENCY_BREAK}
  // Only wrap whole words, never breaking words (emergency wrapping) when the
  // layout width is too small for even a single word.
  DWRITE_WORD_WRAPPING_WHOLE_WORD      = DWRITE_WORD_WRAPPING(3);
  {$EXTERNALSYM DWRITE_WORD_WRAPPING_WHOLE_WORD}
  // Wrap between any valid characters clusters.
  DWRITE_WORD_WRAPPING_CHARACTER       = DWRITE_WORD_WRAPPING(4);
  {$EXTERNALSYM DWRITE_WORD_WRAPPING_CHARACTER}

type
  // The method used for line spacing in layout.
  PDWRITE_LINE_SPACING_METHOD = ^DWRITE_LINE_SPACING_METHOD;
  DWRITE_LINE_SPACING_METHOD = DWord;
  {$EXTERNALSYM DWRITE_LINE_SPACING_METHOD}
const
  // Line spacing depends solely on the content, growing to accommodate the size of fonts and inline objects.
  DWRITE_LINE_SPACING_METHOD_DEFAULT      = DWRITE_LINE_SPACING_METHOD(0);
  {$EXTERNALSYM DWRITE_LINE_SPACING_METHOD_DEFAULT}
  // Lines are explicitly set to uniform spacing, regardless of contained font sizes.
  // This can be useful to avoid the uneven appearance that can occur from font fallback.
  DWRITE_LINE_SPACING_METHOD_UNIFORM      = DWRITE_LINE_SPACING_METHOD(1);
  {$EXTERNALSYM DWRITE_LINE_SPACING_METHOD_UNIFORM}
  // Line spacing and baseline distances are proportional to the computed values based on the content, the size of the fonts and inline objects.
  DWRITE_LINE_SPACING_METHOD_PROPORTIONAL = DWRITE_LINE_SPACING_METHOD(2);
  {$EXTERNALSYM DWRITE_LINE_SPACING_METHOD_PROPORTIONAL}

type
  // Text granularity used to trim text overflowing the layout box.
  PDWRITE_TRIMMING_GRANULARITY = ^DWRITE_TRIMMING_GRANULARITY;
  DWRITE_TRIMMING_GRANULARITY = DWord;
  {$EXTERNALSYM DWRITE_TRIMMING_GRANULARITY}
const
  // No trimming occurs. Text flows beyond the layout width.
  DWRITE_TRIMMING_GRANULARITY_NONE      = DWRITE_TRIMMING_GRANULARITY(0);
  {$EXTERNALSYM DWRITE_TRIMMING_GRANULARITY_NONE}
  // Trimming occurs at character cluster boundary.
  DWRITE_TRIMMING_GRANULARITY_CHARACTER = DWRITE_TRIMMING_GRANULARITY(1);
  {$EXTERNALSYM DWRITE_TRIMMING_GRANULARITY_CHARACTER}
  // Trimming occurs at word boundary.
  DWRITE_TRIMMING_GRANULARITY_WORD      = DWRITE_TRIMMING_GRANULARITY(2);
  {$EXTERNALSYM DWRITE_TRIMMING_GRANULARITY_WORD}

type
  // Typographic feature of text supplied by the font.
  // Remarks:
  //   Use DWRITE_MAKE_FONT_FEATURE_TAG() to create a custom one.
  // Delphi Note:
  //  We did hardcode the macro's here, but you might also use this type of declaration, for example:
  //  DWRITE_FONT_FEATURE_TAG_ALTERNATIVE_FRACTIONS = ord('a') or
  //                                                  (ord('f') shl 8) or
  //                                                  (ord('r') shl 16) or
  //                                                  (ord('c') shl 24); resulting in Int $63726661

  PDWRITE_FONT_FEATURE_TAG = ^DWRITE_FONT_FEATURE_TAG;
  DWRITE_FONT_FEATURE_TAG = Integer;
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG}
const
  DWRITE_FONT_FEATURE_TAG_ALTERNATIVE_FRACTIONS               = $63726661; //DWRITE_MAKE_OPENTYPE_TAG('a','f','r','c'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_ALTERNATIVE_FRACTIONS}
  DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS_FROM_CAPITALS       = $63703263; //DWRITE_MAKE_OPENTYPE_TAG('c','2','p','c'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS_FROM_CAPITALS}
  DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS_FROM_CAPITALS        = $63733263; //DWRITE_MAKE_OPENTYPE_TAG('c','2','s','c'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS_FROM_CAPITALS}
  DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES               = $746C6163; //DWRITE_MAKE_OPENTYPE_TAG('c','a','l','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES}
  DWRITE_FONT_FEATURE_TAG_CASE_SENSITIVE_FORMS                = $65736163; //DWRITE_MAKE_OPENTYPE_TAG('c','a','s','e'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_CASE_SENSITIVE_FORMS}
  DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION     = $706D6363; //DWRITE_MAKE_OPENTYPE_TAG('c','c','m','p'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION}
  DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES                = $67696C63; //DWRITE_MAKE_OPENTYPE_TAG('c','l','i','g'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES}
  DWRITE_FONT_FEATURE_TAG_CAPITAL_SPACING                     = $70737063; //DWRITE_MAKE_OPENTYPE_TAG('c','p','s','p'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_CAPITAL_SPACING}
  DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_SWASH                    = $68777363; //DWRITE_MAKE_OPENTYPE_TAG('c','s','w','h'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_SWASH}
  DWRITE_FONT_FEATURE_TAG_CURSIVE_POSITIONING                 = $73727563; //DWRITE_MAKE_OPENTYPE_TAG('c','u','r','s'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_CURSIVE_POSITIONING}
  DWRITE_FONT_FEATURE_TAG_DEFAULT                             = $746C6664; //DWRITE_MAKE_OPENTYPE_TAG('d','f','l','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_DEFAULT}
  DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES             = $67696C64; //DWRITE_MAKE_OPENTYPE_TAG('d','l','i','g'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES}
  DWRITE_FONT_FEATURE_TAG_EXPERT_FORMS                        = $74707865; //DWRITE_MAKE_OPENTYPE_TAG('e','x','p','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_EXPERT_FORMS}
  DWRITE_FONT_FEATURE_TAG_FRACTIONS                           = $63617266; //DWRITE_MAKE_OPENTYPE_TAG('f','r','a','c'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_FRACTIONS}
  DWRITE_FONT_FEATURE_TAG_FULL_WIDTH                          = $64697766; //DWRITE_MAKE_OPENTYPE_TAG('f','w','i','d'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_FULL_WIDTH}
  DWRITE_FONT_FEATURE_TAG_HALF_FORMS                          = $666C6168; //DWRITE_MAKE_OPENTYPE_TAG('h','a','l','f'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HALF_FORMS}
  DWRITE_FONT_FEATURE_TAG_HALANT_FORMS                        = $6E6C6168; //DWRITE_MAKE_OPENTYPE_TAG('h','a','l','n'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HALANT_FORMS}
  DWRITE_FONT_FEATURE_TAG_ALTERNATE_HALF_WIDTH                = $746C6168; //DWRITE_MAKE_OPENTYPE_TAG('h','a','l','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_ALTERNATE_HALF_WIDTH}
  DWRITE_FONT_FEATURE_TAG_HISTORICAL_FORMS                    = $74736968; //DWRITE_MAKE_OPENTYPE_TAG('h','i','s','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HISTORICAL_FORMS}
  DWRITE_FONT_FEATURE_TAG_HORIZONTAL_KANA_ALTERNATES          = $616E6B68; //DWRITE_MAKE_OPENTYPE_TAG('h','k','n','a'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HORIZONTAL_KANA_ALTERNATES}
  DWRITE_FONT_FEATURE_TAG_HISTORICAL_LIGATURES                = $67696C68; //DWRITE_MAKE_OPENTYPE_TAG('h','l','i','g'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HISTORICAL_LIGATURES}
  DWRITE_FONT_FEATURE_TAG_HALF_WIDTH                          = $64697768; //DWRITE_MAKE_OPENTYPE_TAG('h','w','i','d'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HALF_WIDTH}
  DWRITE_FONT_FEATURE_TAG_HOJO_KANJI_FORMS                    = $6F6A6F68; //DWRITE_MAKE_OPENTYPE_TAG('h','o','j','o'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_HOJO_KANJI_FORMS}
  DWRITE_FONT_FEATURE_TAG_JIS04_FORMS                         = $3430706A; //DWRITE_MAKE_OPENTYPE_TAG('j','p','0','4'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_JIS04_FORMS}
  DWRITE_FONT_FEATURE_TAG_JIS78_FORMS                         = $3837706A; //DWRITE_MAKE_OPENTYPE_TAG('j','p','7','8'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_JIS78_FORMS}
  DWRITE_FONT_FEATURE_TAG_JIS83_FORMS                         = $3338706A; //DWRITE_MAKE_OPENTYPE_TAG('j','p','8','3'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_JIS83_FORMS}
  DWRITE_FONT_FEATURE_TAG_JIS90_FORMS                         = $3039706A; //DWRITE_MAKE_OPENTYPE_TAG('j','p','9','0'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_JIS90_FORMS}
  DWRITE_FONT_FEATURE_TAG_KERNING                             = $6E72656B; //DWRITE_MAKE_OPENTYPE_TAG('k','e','r','n'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_KERNING}
  DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES                  = $6167696C; //DWRITE_MAKE_OPENTYPE_TAG('l','i','g','a'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES}
  DWRITE_FONT_FEATURE_TAG_LINING_FIGURES                      = $6D756E6C; //DWRITE_MAKE_OPENTYPE_TAG('l','n','u','m'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_LINING_FIGURES}
  DWRITE_FONT_FEATURE_TAG_LOCALIZED_FORMS                     = $6C636F6C; //DWRITE_MAKE_OPENTYPE_TAG('l','o','c','l'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_LOCALIZED_FORMS}
  DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING                    = $6B72616D; //DWRITE_MAKE_OPENTYPE_TAG('m','a','r','k'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING}
  DWRITE_FONT_FEATURE_TAG_MATHEMATICAL_GREEK                  = $6B72676D; //DWRITE_MAKE_OPENTYPE_TAG('m','g','r','k'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_MATHEMATICAL_GREEK}
  DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING            = $6B6D6B6D; //DWRITE_MAKE_OPENTYPE_TAG('m','k','m','k'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING}
  DWRITE_FONT_FEATURE_TAG_ALTERNATE_ANNOTATION_FORMS          = $746C616E; //DWRITE_MAKE_OPENTYPE_TAG('n','a','l','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_ALTERNATE_ANNOTATION_FORMS}
  DWRITE_FONT_FEATURE_TAG_NLC_KANJI_FORMS                     = $6B636C6E; //DWRITE_MAKE_OPENTYPE_TAG('n','l','c','k'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_NLC_KANJI_FORMS}
  DWRITE_FONT_FEATURE_TAG_OLD_STYLE_FIGURES                   = $6D756E6F; //DWRITE_MAKE_OPENTYPE_TAG('o','n','u','m'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_OLD_STYLE_FIGURES}
  DWRITE_FONT_FEATURE_TAG_ORDINALS                            = $6E64726F; //DWRITE_MAKE_OPENTYPE_TAG('o','r','d','n'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_ORDINALS}
  DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_ALTERNATE_WIDTH        = $746C6170; //DWRITE_MAKE_OPENTYPE_TAG('p','a','l','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_ALTERNATE_WIDTH}
  DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS                     = $70616370; //DWRITE_MAKE_OPENTYPE_TAG('p','c','a','p'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS}
  DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_FIGURES                = $6D756E70; //DWRITE_MAKE_OPENTYPE_TAG('p','n','u','m'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_FIGURES}
  DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_WIDTHS                 = $64697770; //DWRITE_MAKE_OPENTYPE_TAG('p','w','i','d'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_WIDTHS}
  DWRITE_FONT_FEATURE_TAG_QUARTER_WIDTHS                      = $64697771; //DWRITE_MAKE_OPENTYPE_TAG('q','w','i','d'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_QUARTER_WIDTHS}
  DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES                  = $67696C72; //DWRITE_MAKE_OPENTYPE_TAG('r','l','i','g'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES}
  DWRITE_FONT_FEATURE_TAG_RUBY_NOTATION_FORMS                 = $79627572; //DWRITE_MAKE_OPENTYPE_TAG('r','u','b','y'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_RUBY_NOTATION_FORMS}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_ALTERNATES                = $746C6173; //DWRITE_MAKE_OPENTYPE_TAG('s','a','l','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_ALTERNATES}
  DWRITE_FONT_FEATURE_TAG_SCIENTIFIC_INFERIORS                = $666E6973; //DWRITE_MAKE_OPENTYPE_TAG('s','i','n','f'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SCIENTIFIC_INFERIORS}
  DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS                      = $70636D73; //DWRITE_MAKE_OPENTYPE_TAG('s','m','c','p'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS}
  DWRITE_FONT_FEATURE_TAG_SIMPLIFIED_FORMS                    = $6C706D73; //DWRITE_MAKE_OPENTYPE_TAG('s','m','p','l'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SIMPLIFIED_FORMS}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_1                     = $31307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','1'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_1}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_2                     = $32307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','2'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_2}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_3                     = $33307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','3'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_3}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_4                     = $34307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','4'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_4}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_5                     = $35307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','5'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_5}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_6                     = $36307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','6'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_6}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_7                     = $37307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','7'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_7}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_8                     = $38307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','8'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_8}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_9                     = $39307373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','0','9'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_9}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_10                    = $30317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','0'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_10}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_11                    = $31317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','1'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_11}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_12                    = $32317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','2'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_12}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_13                    = $33317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','3'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_13}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_14                    = $34317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','4'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_14}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_15                    = $35317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','5'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_15}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_16                    = $36317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','6'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_16}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_17                    = $37317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','7'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_17}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_18                    = $38317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','8'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_18}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_19                    = $39317373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','1','9'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_19}
  DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_20                    = $30327373; //DWRITE_MAKE_OPENTYPE_TAG('s','s','2','0'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_20}
  DWRITE_FONT_FEATURE_TAG_SUBSCRIPT                           = $73627573; //DWRITE_MAKE_OPENTYPE_TAG('s','u','b','s'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SUBSCRIPT}
  DWRITE_FONT_FEATURE_TAG_SUPERSCRIPT                         = $73707573; //DWRITE_MAKE_OPENTYPE_TAG('s','u','p','s'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SUPERSCRIPT}
  DWRITE_FONT_FEATURE_TAG_SWASH                               = $68737773; //DWRITE_MAKE_OPENTYPE_TAG('s','w','s','h'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SWASH}
  DWRITE_FONT_FEATURE_TAG_TITLING                             = $6C746974; //DWRITE_MAKE_OPENTYPE_TAG('t','i','t','l'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_TITLING}
  DWRITE_FONT_FEATURE_TAG_TRADITIONAL_NAME_FORMS              = $6D616E74; //DWRITE_MAKE_OPENTYPE_TAG('t','n','a','m'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_TRADITIONAL_NAME_FORMS}
  DWRITE_FONT_FEATURE_TAG_TABULAR_FIGURES                     = $6D756E74; //DWRITE_MAKE_OPENTYPE_TAG('t','n','u','m'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_TABULAR_FIGURES}
  DWRITE_FONT_FEATURE_TAG_TRADITIONAL_FORMS                   = $64617274; //DWRITE_MAKE_OPENTYPE_TAG('t','r','a','d'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_TRADITIONAL_FORMS}
  DWRITE_FONT_FEATURE_TAG_THIRD_WIDTHS                        = $64697774; //DWRITE_MAKE_OPENTYPE_TAG('t','w','i','d'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_THIRD_WIDTHS}
  DWRITE_FONT_FEATURE_TAG_UNICASE                             = $63696E75; //DWRITE_MAKE_OPENTYPE_TAG('u','n','i','c'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_UNICASE}
  DWRITE_FONT_FEATURE_TAG_VERTICAL_WRITING                    = $74726576; //DWRITE_MAKE_OPENTYPE_TAG('v','e','r','t'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_VERTICAL_WRITING}
  DWRITE_FONT_FEATURE_TAG_VERTICAL_ALTERNATES_AND_ROTATION    = $32747276; //DWRITE_MAKE_OPENTYPE_TAG('v','r','t','2'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_VERTICAL_ALTERNATES_AND_ROTATION}
  DWRITE_FONT_FEATURE_TAG_SLASHED_ZERO                        = $6F72657A; //DWRITE_MAKE_OPENTYPE_TAG('z','e','r','o'),
  {$EXTERNALSYM DWRITE_FONT_FEATURE_TAG_SLASHED_ZERO}

type
  // How to apply number substitution on digits and related punctuation.
  PDWRITE_NUMBER_SUBSTITUTION_METHOD = ^DWRITE_NUMBER_SUBSTITUTION_METHOD;
  DWRITE_NUMBER_SUBSTITUTION_METHOD = DWord;
  {$EXTERNALSYM DWRITE_NUMBER_SUBSTITUTION_METHOD}
const
  // Specifies that the substitution method should be determined based
  // on LOCALE_IDIGITSUBSTITUTION value of the specified text culture.
  DWRITE_NUMBER_SUBSTITUTION_METHOD_FROM_CULTURE = DWRITE_NUMBER_SUBSTITUTION_METHOD(0);
  {$EXTERNALSYM DWRITE_NUMBER_SUBSTITUTION_METHOD_FROM_CULTURE}
  // If the culture is Arabic or Farsi, specifies that the number shape
  // depend on the context. Either traditional or nominal number shape
  // are used depending on the nearest preceding strong character or (if
  // there is none) the reading direction of the paragraph.
  DWRITE_NUMBER_SUBSTITUTION_METHOD_CONTEXTUAL   = DWRITE_NUMBER_SUBSTITUTION_METHOD(1);
  {$EXTERNALSYM DWRITE_NUMBER_SUBSTITUTION_METHOD_CONTEXTUAL}
  // Specifies that code points 0x30-0x39 are always rendered as nominal numeral
  // shapes (ones of the European number), i.e., no substitution is performed.
  DWRITE_NUMBER_SUBSTITUTION_METHOD_NONE         = DWRITE_NUMBER_SUBSTITUTION_METHOD(2);
  {$EXTERNALSYM DWRITE_NUMBER_SUBSTITUTION_METHOD_NONE}
  // Specifies that number are rendered using the national number shape
  // as specified by the LOCALE_SNATIVEDIGITS value of the specified text culture.
  DWRITE_NUMBER_SUBSTITUTION_METHOD_NATIONAL     = DWRITE_NUMBER_SUBSTITUTION_METHOD(3);
  {$EXTERNALSYM DWRITE_NUMBER_SUBSTITUTION_METHOD_NATIONAL}
  // Specifies that number are rendered using the traditional shape
  // for the specified culture. For most cultures, this is the same as
  // NativeNational. However, NativeNational results in Latin number
  // for some Arabic cultures, whereas this value results in Arabic
  // number for all Arabic cultures.
  DWRITE_NUMBER_SUBSTITUTION_METHOD_TRADITIONAL  = DWRITE_NUMBER_SUBSTITUTION_METHOD(4);
  {$EXTERNALSYM DWRITE_NUMBER_SUBSTITUTION_METHOD_TRADITIONAL}

type
  PDWRITE_SCRIPT_SHAPES = ^DWRITE_SCRIPT_SHAPES;
  DWRITE_SCRIPT_SHAPES = DWord;
  {$EXTERNALSYM DWRITE_SCRIPT_SHAPES}
const
  // No additional shaping requirement. Text is shaped with the writing system default behavior.
  DWRITE_SCRIPT_SHAPES_DEFAULT   = DWRITE_SCRIPT_SHAPES(0);
  {$EXTERNALSYM DWRITE_SCRIPT_SHAPES_DEFAULT}
  // Text should leave no visual on display i.e. control or format control characters.
  DWRITE_SCRIPT_SHAPES_NO_VISUAL = DWRITE_SCRIPT_SHAPES(1);
  {$EXTERNALSYM DWRITE_SCRIPT_SHAPES_NO_VISUAL}

type
  // Condition at the edges of inline object or text used to determine
  // line-breaking behavior.
  PDWRITE_BREAK_CONDITION = ^DWRITE_BREAK_CONDITION;
  DWRITE_BREAK_CONDITION = DWord;
  {$EXTERNALSYM DWRITE_BREAK_CONDITION}
const
  // Whether a break is allowed is determined by the condition of the
  // neighboring text span or inline object.
  DWRITE_BREAK_CONDITION_NEUTRAL       = DWRITE_BREAK_CONDITION(0);
  {$EXTERNALSYM DWRITE_BREAK_CONDITION_NEUTRAL}
  // A break is allowed, unless overruled by the condition of the
  // neighboring text span or inline object, either prohibited by a
  // May Not or forced by a Must.
  DWRITE_BREAK_CONDITION_CAN_BREAK     = DWRITE_BREAK_CONDITION(1);
  {$EXTERNALSYM DWRITE_BREAK_CONDITION_CAN_BREAK}
  // There should be no break, unless overruled by a Must condition from
  // the neighboring text span or inline object.
  DWRITE_BREAK_CONDITION_MAY_NOT_BREAK = DWRITE_BREAK_CONDITION(2);
  {$EXTERNALSYM DWRITE_BREAK_CONDITION_MAY_NOT_BREAK}
  // The break must happen, regardless of the condition of the adjacent
  // text span or inline object.
  DWRITE_BREAK_CONDITION_MUST_BREAK    = DWRITE_BREAK_CONDITION(3);
  {$EXTERNALSYM DWRITE_BREAK_CONDITION_MUST_BREAK}

type
  // The DWRITE_TEXTURE_TYPE enumeration identifies a type of alpha texture. An alpha texture is a bitmap of alpha values, each
  // representing the darkness (i.e., opacity) of a pixel or subpixel.
  PDWRITE_TEXTURE_TYPE = ^DWRITE_TEXTURE_TYPE;
  DWRITE_TEXTURE_TYPE = DWord;
  {$EXTERNALSYM DWRITE_TEXTURE_TYPE}
const
  // Specifies an alpha texture for aliased text rendering (i.e., bi-level, where each pixel is either fully opaque or fully transparent),
  // with one byte per pixel.
  DWRITE_TEXTURE_ALIASED_1x1   = DWRITE_TEXTURE_TYPE(0);
  {$EXTERNALSYM DWRITE_TEXTURE_ALIASED_1x1}
  // Specifies an alpha texture for ClearType text rendering, with three bytes per pixel in the horizontal dimension and
  // one byte per pixel in the vertical dimension.
  DWRITE_TEXTURE_CLEARTYPE_3x1 = DWRITE_TEXTURE_TYPE(1);
  {$EXTERNALSYM DWRITE_TEXTURE_CLEARTYPE_3x1}



// =============================================================================

type

  // Forward interface declarations

  PIDWriteFontFileStream = ^IDWriteFontFileStream;
  IDWriteFontFileStream = interface;

  PIDWriteFontFile = ^IDWriteFontFile;
  IDWriteFontFile = interface;

  PIDWriteFontFileEnumerator = ^IDWriteFontFileEnumerator;
  IDWriteFontFileEnumerator = interface;

  PIDWriteFontFamily = ^IDWriteFontFamily;
  IDWriteFontFamily = interface;

  PIDWriteFont = ^IDWriteFont;
  IDWriteFont = interface;

  PIDWriteInlineObject = ^IDWriteInlineObject;
  IDWriteInlineObject = interface;

  PIDWriteTextRenderer = ^IDWriteTextRenderer;
  IDWriteTextRenderer = interface;

  PIDWriteFactory = ^IDWriteFactory;
  IDWriteFactory = interface;

  PIDWriteFontCollection = ^IDWriteFontCollection;
  IDWriteFontCollection = interface;

// Delphi Note: Moved to D2D1.pas to prevent cricular reference error F2047
//  IDWriteGeometrySink = ID2D1SimplifiedGeometrySink;
//  PIDWriteGeometrySink = ^IDWriteGeometrySink;


// INTERFACES //////////////////////////////////////////////////////////////////


  // Interface IDWriteFontFileLoader
  // ===============================
  // Font file loader interface handles loading font file resources of a particular type from a key.
  // The font file loader interface is recommended to be implemented by a singleton object.
  // IMPORTANT: font file loader implementations must not register themselves with DirectWrite factory
  // inside their constructors and must not unregister themselves in their destructors, because
  // registration and unregistration operations increment and decrement the object reference count respectively.
  // Instead, registration and unregistration of font file loaders with DirectWrite factory should be performed
  // outside of the font file loader implementation as a separate step.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFileLoader);'}
  {$EXTERNALSYM IDWriteFontFileLoader}
  IDWriteFontFileLoader = interface(IUnknown)   // IID_IDWriteFontFileLoader: TGuid = '{727cad4e-d6af-4c9e-8a08-d695b11caa49}';
  ['{727cad4e-d6af-4c9e-8a08-d695b11caa49}']

    // Creates a font file stream object that encapsulates an open file resource.
    // The resource is closed when the last reference to fontFileStream is released.
    // <param name="fontFileReferenceKey">Font file reference key that uniquely identifies the font file resource
    // within the scope of the font loader being used.</param>
    // <param name="fontFileReferenceKeySize">Size of font file reference key in bytes.</param>
    // <param name="fontFileStream">Pointer to the newly created font file stream.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateStreamFromKey(fontFileReferenceKey: Pointer;
                                 fontFileReferenceKeySize: UINT32;
                                 out fontFileStream: IDWriteFontFileStream): HResult; stdcall;
  end;
  IID_IDWriteFontFileLoader = IDWriteFontFileLoader;
  {$EXTERNALSYM IID_IDWriteFontFileLoader}


  // Interface IDWriteLocalFontFileLoader
  // ====================================
  // A built-in implementation of IDWriteFontFileLoader interface that operates on local font files
  // and exposes local font file information from the font file reference key.
  // Font file references created using CreateFontFileReference use this font file loader.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteLocalFontFileLoader);'}
  {$EXTERNALSYM IDWriteLocalFontFileLoader}
  IDWriteLocalFontFileLoader = interface(IDWriteFontFileLoader)
  ['{b2d9f3ec-c9fe-4a11-a2ec-d86208f7c0a2}']

    // Obtains the length of the absolute file path from the font file reference key.
    // <param name="fontFileReferenceKey">Font file reference key that uniquely identifies the local font file
    // within the scope of the font loader being used.</param>
    // <param name="fontFileReferenceKeySize">Size of font file reference key in bytes.</param>
    // <param name="filePathLength">Length of the file path string not including the terminated NULL character.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFilePathLengthFromKey(fontFileReferenceKey: Pointer;
                                      fontFileReferenceKeySize: UINT32;
                                      out filePathLength: UINT32): HResult; stdcall;

    // Obtains the absolute font file path from the font file reference key.
    // <param name="fontFileReferenceKey">Font file reference key that uniquely identifies the local font file
    // within the scope of the font loader being used.</param>
    // <param name="fontFileReferenceKeySize">Size of font file reference key in bytes.</param>
    // <param name="filePath">Character array that receives the local file path.</param>
    // <param name="filePathSize">Size of the filePath array in character count including the terminated NULL character.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFilePathFromKey(fontFileReferenceKey: Pointer;
                                fontFileReferenceKeySize: UINT32;
                                out filePath: PWideChar;
                                filePathSize: UINT32): HResult; stdcall;

    // Obtains the last write time of the file from the font file reference key.
    // <param name="fontFileReferenceKey">Font file reference key that uniquely identifies the local font file
    // within the scope of the font loader being used.</param>
    // <param name="fontFileReferenceKeySize">Size of font file reference key in bytes.</param>
    // <param name="lastWriteTime">Last modified time of the font file.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLastWriteTimeFromKey(fontFileReferenceKey: Pointer;
                                     fontFileReferenceKeySize: UINT32;
                                     out lastWriteTime: FILETIME): HResult; stdcall;

  end;
  IID_IDWriteLocalFontFileLoader = IDWriteLocalFontFileLoader;
  {$EXTERNALSYM IID_IDWriteLocalFontFileLoader}


  // Interface IDWriteFontFileStream
  // ===============================
  // The interface for loading font file data.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFileStream);'}
  {$EXTERNALSYM IDWriteFontFileStream}
  IDWriteFontFileStream = interface(IUnknown)
  ['{6d4865fe-0ab8-4d91-8f62-5dd6be34a3e0}']

    // Reads a fragment from a file.
    // <param name="fragmentStart">Receives the pointer to the start of the font file fragment.</param>
    // <param name="fileOffset">Offset of the fragment from the beginning of the font file.</param>
    // <param name="fragmentSize">Size of the fragment in bytes.</param>
    // <param name="fragmentContext">The client defined context to be passed to the ReleaseFileFragment.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // IMPORTANT: ReadFileFragment() implementations must check whether the requested file fragment
    // is within the file bounds. Otherwise, an error should be returned from ReadFileFragment.
    // </remarks>
    function ReadFileFragment(fragmentStart: Pointer;
                              fileOffset: UINT64;
                              fragmentSize: UINT64;
                              out fragmentContext: Pointer): HResult; stdcall;

    // Releases a fragment from a file.
    // <param name="fragmentContext">The client defined context of a font fragment returned from ReadFileFragment.</param>
    procedure ReleaseFileFragment(fragmentContext: Pointer); stdcall;

    // Obtains the total size of a file.
    // <param name="fileSize">Receives the total size of the file.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // Implementing GetFileSize() for asynchronously loaded font files may require
    // downloading the complete file contents, therefore this method should only be used for operations that
    // either require complete font file to be loaded (e.g., copying a font file) or need to make
    // decisions based on the value of the file size (e.g., validation against a persisted file size).
    // </remarks>
    function GetFileSize(out fileSize: UINT64): HResult; stdcall;

    // Obtains the last modified time of the file. The last modified time is used by DirectWrite font selection algorithms
    // to determine whether one font resource is more up to date than another one.
    // <param name="lastWriteTime">Receives the last modified time of the file in the format that represents
    // the number of 100-nanosecond intervals since January 1, 1601 (UTC).</param>
    // <returns>
    // Standard HRESULT error code. For resources that don't have a concept of the last modified time, the implementation of
    // GetLastWriteTime should return E_NOTIMPL.
    // </returns>
    function GetLastWriteTime(out lastWriteTime: UINT64): HResult; stdcall;

  end;
  IID_IDWriteFontFileStream = IDWriteFontFileStream;
  {$EXTERNALSYM IID_IDWriteFontFileStream}


  // Interface IDWriteFontFile
  // =========================
  // The interface that represents a reference to a font file.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFile);'}
  {$EXTERNALSYM IDWriteFontFile}
  IDWriteFontFile = interface(IUnknown)
  ['{739d886a-cef5-47dc-8769-1a8b41bebbb0}']

    // This method obtains the pointer to the reference key of a font file.
    //The pointer is only valid until the object that refers to it is released.
    // <param name="fontFileReferenceKey">Pointer to the font file reference key.
    // IMPORTANT: The pointer value is valid until the font file reference object it is obtained from is released.</param>
    // <param name="fontFileReferenceKeySize">Size of font file reference key in bytes.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetReferenceKey(fontFileReferenceKey: Pointer;
                             out fontFileReferenceKeySize: UINT32): HResult; stdcall;

    // Obtains the file loader associated with a font file object.
    // <param name="fontFileLoader">The font file loader associated with the font file object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLoader(out fontFileLoader: IDWriteFontFileLoader): HResult; stdcall;

    // Analyzes a file and returns whether it represents a font, and whether the font type is supported by the font system.
    // <param name="isSupportedFontType">TRUE if the font type is supported by the font system, FALSE otherwise.</param>
    // <param name="fontFileType">The type of the font file. Note that even if isSupportedFontType is FALSE,
    // the fontFileType value may be different from DWRITE_FONT_FILE_TYPE_UNKNOWN.</param>
    // <param name="fontFaceType">The type of the font face that can be constructed from the font file.
    // Note that even if isSupportedFontType is FALSE, the fontFaceType value may be different from
    // DWRITE_FONT_FACE_TYPE_UNKNOWN.</param>
    // <param name="numberOfFaces">Number of font faces contained in the font file.</param>
    // <returns>
    // Standard HRESULT error code if there was a processing error during analysis.
    // </returns>
    // <remarks>
    // IMPORTANT: certain font file types are recognized, but not supported by the font system.
    // For example, the font system will recognize a file as a Type 1 font file,
    // but will not be able to construct a font face object from it. In such situations, Analyze will set
    // isSupportedFontType output parameter to FALSE.
    // </remarks>
    function Analyze(out isSupportedFontType: BOOL;
                     out fontFileType: DWRITE_FONT_FILE_TYPE;
                     out fontFaceType: DWRITE_FONT_FACE_TYPE;
                     out numberOfFaces: UINT32): HResult; stdcall;

  end;
  IID_IDWriteFontFile = IDWriteFontFile;
  {$EXTERNALSYM IID_IDWriteFontFile}


  // The DWRITE_MATRIX structure specifies the graphics transform to be applied
  // to rendered glyphs.
  PDWRITE_MATRIX = ^DWRITE_MATRIX;
  DWRITE_MATRIX = record
    // Horizontal scaling / cosine of rotation
    _11: Single; //m11: Single;
    // Vertical shear / sine of rotation
    _12: Single; //m12: Single;
    // Horizontal shear / negative sine of rotation
    _21: Single; //m21: Single;
    // Vertical scaling / cosine of rotation
    _22: Single; //m22: Single;
    // Horizontal shift (always orthogonal regardless of rotation)
    _31: Single; //dx: Single;
    // Vertical shift (always orthogonal regardless of rotation)
    _32: Single; //dy: Single;
  end;
  {$EXTERNALSYM DWRITE_MATRIX}


  // Interface IDWriteRenderingParams
  // ================================
  // The interface that represents text rendering settings for glyph rasterization and filtering.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteRenderingParams);'}
  {$EXTERNALSYM IDWriteRenderingParams}
  IDWriteRenderingParams = interface(IUnknown)
  ['{2f0da53a-2add-47cd-82ee-d9ec34688e75}']

    // Gets the gamma value used for gamma correction. Valid values must be
    // greater than zero and cannot exceed 256.
    function GetGamma(): Single;

    // Gets the amount of contrast enhancement. Valid values are greater than
    // or equal to zero.
    function GetEnhancedContrast(): Single;

    // Gets the ClearType level. Valid values range from 0.0 (no ClearType)
    // to 1.0 (full ClearType).
    function GetClearTypeLevel(): Single;

    // Gets the pixel geometry.
    function GetPixelGeometry(): DWRITE_PIXEL_GEOMETRY;

    // Gets the rendering mode.
    function GetRenderingMode(): DWRITE_RENDERING_MODE;

  end;
  // Interface IDWriteRenderingParams
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteRenderingParams);'}
  {$EXTERNALSYM IDWriteRenderingParams}


  // Interface IDWriteFontFace
  // =========================
  // This interface exposes various font data such as metrics, names, and glyph outlines.
  // It contains font face type, appropriate file references and face identification data.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace);'}
  {$EXTERNALSYM IDWriteFontFace}
  IDWriteFontFace = interface(IUnknown)
  ['{5f49804d-7024-4d43-bfa9-d25984f53849}']

    // Obtains the file format type of a font face.
    function GetType(): DWRITE_FONT_FACE_TYPE;

    // Obtains the font files representing a font face.
    // <param name="numberOfFiles">The number of files representing the font face.</param>
    // <param name="fontFiles">User provided array that stores pointers to font files representing the font face.
    // This parameter can be NULL if the user is only interested in the number of files representing the font face.
    // This API increments reference count of the font file pointers returned according to COM conventions, and the client
    // should release them when finished.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFiles(out numberOfFiles: UINT32;
                      out fontFiles: IDWriteFontFile): HResult; stdcall;

    // Obtains the zero-based index of the font face in its font file or files. If the font files contain a single face,
    // the return value is zero.
    function GetIndex(): UINT32; stdcall;

    // Obtains the algorithmic style simulation flags of a font face.
    function GetSimulations(): DWRITE_FONT_SIMULATIONS; stdcall;

    // Determines whether the font is a symbol font.
    function IsSymbolFont(): BOOL; stdcall;

    // Obtains design units and common metrics for the font face.
    // These metrics are applicable to all the glyphs within a fontface and are used by applications for layout calculations.
    // <param name="fontFaceMetrics">Points to a DWRITE_FONT_METRICS structure to fill in.
    // The metrics returned by this function are in font design units.</param>
    procedure GetMetrics(out fontFaceMetrics: DWRITE_FONT_METRICS); stdcall;

    // Obtains the number of glyphs in the font face.
    function GetGlyphCount(): UINT16;

    // Obtains ideal glyph metrics in font design units. Design glyphs metrics are used for glyph positioning.
    // <param name="glyphIndices">An array of glyph indices to compute the metrics for.</param>
    // <param name="glyphCount">The number of elements in the glyphIndices array.</param>
    // <param name="glyphMetrics">Array of DWRITE_GLYPH_METRICS structures filled by this function.
    // The metrics returned by this function are in font design units.</param>
    // <param name="isSideways">Indicates whether the font is being used in a sideways run.
    // This can affect the glyph metrics if the font has oblique simulation
    // because sideways oblique simulation differs from non-sideways oblique simulation.</param>
    // <returns>
    // Standard HRESULT error code. If any of the input glyph indices are outside of the valid glyph index range
    // for the current font face, E_INVALIDARG will be returned.
    // </returns>
    function GetDesignGlyphMetrics(glyphIndices: UINT16;
                                   glyphCount: UINT32;
                                   out glyphMetrics: DWRITE_GLYPH_METRICS;
                                   isSideways: BOOL = False): HResult; stdcall;

    // Returns the nominal mapping of UTF-32 Unicode code points to glyph indices as defined by the font 'cmap' table.
    // Note that this mapping is primarily provided for line layout engines built on top of the physical font API.
    // Because of OpenType glyph substitution and line layout character substitution, the nominal conversion does not always correspond
    // to how a Unicode string will map to glyph indices when rendering using a particular font face.
    // Also, note that Unicode Variation Selectors provide for alternate mappings for character to glyph.
    // This call will always return the default variant.
    // <param name="codePoints">An array of UTF-32 code points to obtain nominal glyph indices from.</param>
    // <param name="codePointCount">The number of elements in the codePoints array.</param>
    // <param name="glyphIndices">Array of nominal glyph indices filled by this function.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetGlyphIndices(codePoints: UINT32;
                             codePointCount: UINT32;
                             out glyphIndices: UINT16): HResult; stdcall;

    // Finds the specified OpenType font table if it exists and returns a pointer to it.
    // The function accesses the underlying font data via the IDWriteFontFileStream interface
    // implemented by the font file loader.
    // <param name="openTypeTableTag">Four character tag of table to find.
    //     Use the DWRITE_MAKE_OPENTYPE_TAG() macro to create it.
    //     Unlike GDI, it does not support the special TTCF and null tags to access the whole font.</param>
    // <param name="tableData">
    //     Pointer to base of table in memory.
    //     The pointer is only valid so long as the FontFace used to get the font table still exists
    //     (not any other FontFace, even if it actually refers to the same physical font).
    // </param>
    // <param name="tableSize">Byte size of table.</param>
    // <param name="tableContext">
    //     Opaque context which must be freed by calling ReleaseFontTable.
    //     The context actually comes from the lower level IDWriteFontFileStream,
    //     which may be implemented by the application or DWrite itself.
    //     It is possible for a NULL tableContext to be returned, especially if
    //     the implementation directly memory maps the whole file.
    //     Nevertheless, always release it later, and do not use it as a test for function success.
    //     The same table can be queried multiple times,
    //     but each returned context can be different, so release each separately.
    // </param>
    // <param name="exists">True if table exists.</param>
    // <returns>
    // Standard HRESULT error code.
    // If a table can not be found, the function will not return an error, but the size will be 0, table NULL, and exists = FALSE.
    // The context does not need to be freed if the table was not found.
    // </returns>
    // <remarks>
    // The context for the same tag may be different for each call,
    // so each one must be held and released separately.
    // </remarks>
    function TryGetFontTable(openTypeTableTag: UINT32;
                             out tableData: Pointer;
                             out tableSize: UINT32;
                             out tableContext: Pointer;
                             out exists: BOOL): HResult; stdcall;

    // Releases the table obtained earlier from TryGetFontTable.
    // <param name="tableContext">Opaque context from TryGetFontTable.</param>
    procedure ReleaseFontTable(tableContext: Pointer); stdcall;

    // Computes the outline of a run of glyphs by calling back to the outline sink interface.
    // <param name="emSize">Logical size of the font in DIP units. A DIP ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="glyphIndices">Array of glyph indices.</param>
    // <param name="glyphAdvances">Optional array of glyph advances in DIPs.</param>
    // <param name="glyphOffsets">Optional array of glyph offsets.</param>
    // <param name="glyphCount">Number of glyphs.</param>
    // <param name="isSideways">If true, specifies that glyphs are rotated 90 degrees to the left and vertical metrics are used.
    // A client can render a vertical run by specifying isSideways = true and rotating the resulting geometry 90 degrees to the
    // right using a transform.</param>
    // <param name="isRightToLeft">If true, specifies that the advance direction is right to left. By default, the advance direction
    // is left to right.</param>
    // <param name="geometrySink">Interface the function calls back to draw each element of the geometry.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    //function GetGlyphRunOutline(emSize: Single;
    //                            glyphIndices: UINT16;
    //                            glyphAdvances: Single;
    //                            glyphOffsets: PDWRITE_GLYPH_OFFSET;
    //                            glyphCount: UINT32;
    //                            isSideways: BOOL;
    //                            isRightToLeft: BOOL;
    //                            geometrySink: IDWriteGeometrySink): HResult; stdcall;

    // Determines the recommended rendering mode for the font given the specified size and rendering parameters.
    // <param name="emSize">Logical size of the font in DIP units. A DIP ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For example, if the DPI of the rendering surface is 96 this
    // value is 1.0f. If the DPI is 120, this value is 120.0f/96.</param>
    // <param name="measuringMode">Specifies measuring mode that will be used for glyphs in the font.
    // Renderer implementations may choose different rendering modes for given measuring modes, but
    // best results are seen when the corresponding modes match:
    // DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL for DWRITE_MEASURING_MODE_NATURAL
    // DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC for DWRITE_MEASURING_MODE_GDI_CLASSIC
    // DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL for DWRITE_MEASURING_MODE_GDI_NATURAL
    // </param>
    // <param name="renderingParams">Rendering parameters object. This parameter is necessary in case the rendering parameters
    // object overrides the rendering mode.</param>
    // <param name="renderingMode">Receives the recommended rendering mode to use.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetRecommendedRenderingMode(emSize: Single;
                                         pixelsPerDip: Single;
                                         measuringMode: DWRITE_MEASURING_MODE;
                                         renderingParams: IDWriteRenderingParams;
                                         out renderingMode: DWRITE_RENDERING_MODE): HResult; stdcall;

    // Obtains design units and common metrics for the font face.
    // These metrics are applicable to all the glyphs within a fontface and are used by applications for layout calculations.
    // <param name="emSize">Logical size of the font in DIP units. A DIP ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For example, if the DPI of the rendering surface is 96 this
    // value is 1.0f. If the DPI is 120, this value is 120.0f/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified by the font size and pixelsPerDip.</param>
    // <param name="fontFaceMetrics">Points to a DWRITE_FONT_METRICS structure to fill in.
    // The metrics returned by this function are in font design units.</param>
    function GetGdiCompatibleMetrics(emSize: Single;
                                     pixelsPerDip: Single;
                                     transform: PDWRITE_MATRIX;
                                     out fontFaceMetrics: DWRITE_FONT_METRICS): HResult; stdcall;

    // Obtains glyph metrics in font design units with the return values compatible with what GDI would produce.
    // Glyphs metrics are used for positioning of individual glyphs.
    // <param name="emSize">Logical size of the font in DIP units. A DIP ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For example, if the DPI of the rendering surface is 96 this
    // value is 1.0f. If the DPI is 120, this value is 120.0f/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified by the font size and pixelsPerDip.</param>
    // <param name="useGdiNatural">
    // When set to FALSE, the metrics are the same as the metrics of GDI aliased text.
    // When set to TRUE, the metrics are the same as the metrics of text measured by GDI using a font
    // created with CLEARTYPE_NATURAL_QUALITY.
    // </param>
    // <param name="glyphIndices">An array of glyph indices to compute the metrics for.</param>
    // <param name="glyphCount">The number of elements in the glyphIndices array.</param>
    // <param name="glyphMetrics">Array of DWRITE_GLYPH_METRICS structures filled by this function.
    // The metrics returned by this function are in font design units.</param>
    // <param name="isSideways">Indicates whether the font is being used in a sideways run.
    // This can affect the glyph metrics if the font has oblique simulation
    // because sideways oblique simulation differs from non-sideways oblique simulation.</param>
    // <returns>
    // Standard HRESULT error code. If any of the input glyph indices are outside of the valid glyph index range
    // for the current font face, E_INVALIDARG will be returned.
    // </returns>
    function GetGdiCompatibleGlyphMetrics(emSize: Single;
                                          pixelsPerDip: Single;
                                          transform: PDWRITE_MATRIX;
                                          useGdiNatural: BOOL;
                                          glyphIndices: UINT16;
                                          glyphCount: UINT32;
                                          out glyphMetrics: DWRITE_GLYPH_METRICS;
                                          isSideways: BOOL = False): HResult; stdcall;

   end;
  IID_IDWriteFontFace = IDWriteFontFace;
  {$EXTERNALSYM IID_IDWriteFontFace}


  // Interface IDWriteFontCollectionLoader
  // =====================================
  // The font collection loader interface is used to construct a collection of fonts given a particular type of key.
  // The font collection loader interface is recommended to be implemented by a singleton object.
  // IMPORTANT: font collection loader implementations must not register themselves with a DirectWrite factory
  // inside their constructors and must not unregister themselves in their destructors, because
  // registration and unregistration operations increment and decrement the object reference count respectively.
  // Instead, registration and unregistration of font file loaders with DirectWrite factory should be performed
  // outside of the font file loader implementation as a separate step.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontCollectionLoader);'}
  {$EXTERNALSYM IDWriteFontCollectionLoader}
  IDWriteFontCollectionLoader = interface(IUnknown)
  ['{cca920e4-52f0-492b-bfa8-29c72ee0a468}']

    // Creates a font file enumerator object that encapsulates a collection of font files.
    // The font system calls back to this interface to create a font collection.
    // <param name="factory">Factory associated with the loader.</param>
    // <param name="collectionKey">Font collection key that uniquely identifies the collection of font files within
    // the scope of the font collection loader being used.</param>
    // <param name="collectionKeySize">Size of the font collection key in bytes.</param>
    // <param name="fontFileEnumerator">Pointer to the newly created font file enumerator.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateEnumeratorFromKey(factory: IDWriteFactory;
                                     collectionKey: Pointer;
                                     collectionKeySize: UINT32;
                                     out fontFileEnumerator: IDWriteFontFileEnumerator): HResult; stdcall;

  end;
  IID_IDWriteFontCollectionLoader = IDWriteFontCollectionLoader;
  {$EXTERNALSYM IID_IDWriteFontCollectionLoader}


  // Interface IDWriteFontFileEnumerator
  // ===================================
  // The font file enumerator interface encapsulates a collection of font files. The font system uses this interface
  // to enumerate font files when building a font collection.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFileEnumerator);'}
  {$EXTERNALSYM IDWriteFontFileEnumerator}
  IDWriteFontFileEnumerator = interface(IUnknown)
  ['{72755049-5ff7-435d-8348-4be97cfa6c7c}']

    // Advances to the next font file in the collection. When it is first created, the enumerator is positioned
    // before the first element of the collection and the first call to MoveNext advances to the first file.
    // <param name="hasCurrentFile">Receives the value TRUE if the enumerator advances to a file, or FALSE if
    // the enumerator advanced past the last file in the collection.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function MoveNext(out hasCurrentFile: BOOL): HResult; stdcall;

    // Gets a reference to the current font file.
    // <param name="fontFile">Pointer to the newly created font file object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetCurrentFontFile(out fontFile: IDWriteFontFile): HResult; stdcall;

  end;
  IID_IDWriteFontFileEnumerator = IDWriteFontFileEnumerator;
  {$EXTERNALSYM IID_IDWriteFontFileEnumerator}


  // Interface IDWriteLocalizedStrings
  // =================================
  // Represents a collection of strings indexed by locale name.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteLocalizedStrings);'}
  {$EXTERNALSYM IDWriteLocalizedStrings}
  IDWriteLocalizedStrings = interface(IUnknown)
  ['{08256209-099a-4b34-b86d-c22b110e7771}']

    // Gets the number of language/string pairs.
    function GetCount(): UINT32; stdcall;

    // Gets the index of the item with the specified locale name.
    // <param name="localeName">Locale name to look for.</param>
    // <param name="index">Receives the zero-based index of the locale name/string pair.</param>
    // <param name="exists">Receives TRUE if the locale name exists or FALSE if not.</param>
    // <returns>
    // Standard HRESULT error code. If the specified locale name does not exist, the return value is S_OK,
    // but *index is UINT_MAX and *exists is FALSE.
    // </returns>
    function FindLocaleName(localeName: PWideChar;
                            out index: UINT32;
                            out exists: BOOL): HResult; stdcall;

    // Gets the length in characters (not including the null terminator) of the locale name with the specified index.
    // <param name="index">Zero-based index of the locale name.</param>
    // <param name="length">Receives the length in characters, not including the null terminator.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLocaleNameLength(index: UINT32;
                                 out length: UINT32): HResult; stdcall;

    // Copies the locale name with the specified index to the specified array.
    // <param name="index">Zero-based index of the locale name.</param>
    // <param name="localeName">Character array that receives the locale name.</param>
    // <param name="size">Size of the array in characters. The size must include space for the terminating
    // null character.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLocaleName(index: UINT32;
                           out localeName: PWideChar;
                           size: UINT32): HResult; stdcall;

    // Gets the length in characters (not including the null terminator) of the string with the specified index.
    // <param name="index">Zero-based index of the string.</param>
    // <param name="length">Receives the length in characters, not including the null terminator.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetStringLength(index: UINT32;
                             out length: UINT32): HResult; stdcall;

    // Copies the string with the specified index to the specified array.
    // <param name="index">Zero-based index of the string.</param>
    // <param name="stringBuffer">Character array that receives the string.</param>
    // <param name="size">Size of the array in characters. The size must include space for the terminating
    // null character.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetString(index: UINT32;
                       out stringBuffer: PWideChar;
                       size: UINT32): HResult; stdcall;

  end;
  IID_IDWriteLocalizedStrings = IDWriteLocalizedStrings;
  {$EXTERNALSYM IID_IDWriteLocalizedStrings}


  // Interface IDWriteFontCollection
  // ===============================
  // The IDWriteFontCollection encapsulates a collection of font families.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontCollection);'}
  {$EXTERNALSYM IDWriteFontCollection}
  IDWriteFontCollection = interface(IUnknown)
  ['{a84cee02-3eea-4eee-a827-87c1a02a0fcc}']

    // Gets the number of font families in the collection.
    function GetFontFamilyCount(): UINT32; stdcall;

    // Creates a font family object given a zero-based font family index.
    // <param name="index">Zero-based index of the font family.</param>
    // <param name="fontFamily">Receives a pointer the newly created font family object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontFamily(index: UINT32;
                           out fontFamily: IDWriteFontFamily): HResult; stdcall;

    // Finds the font family with the specified family name.
    // <param name="familyName">Name of the font family. The name is not case-sensitive but must otherwise exactly match a family name in the collection.</param>
    // <param name="index">Receives the zero-based index of the matching font family if the family name was found or UINT_MAX otherwise.</param>
    // <param name="exists">Receives TRUE if the family name exists or FALSE otherwise.</param>
    // <returns>
    // Standard HRESULT error code. If the specified family name does not exist, the return value is S_OK, but *index is UINT_MAX and *exists is FALSE.
    // </returns>
    function FindFamilyName(familyName: PWideChar;
                            out index: UINT32;
                            out exists: BOOL): HResult; stdcall;

    // Gets the font object that corresponds to the same physical font as the specified font face object. The specified physical font must belong
    // to the font collection.
    // <param name="fontFace">Font face object that specifies the physical font.</param>
    // <param name="font">Receives a pointer to the newly created font object if successful or NULL otherwise.</param>
    // <returns>
    // Standard HRESULT error code. If the specified physical font is not part of the font collection the return value is DWRITE_E_NOFONT.
    // </returns>
    function GetFontFromFontFace(fontFace: IDWriteFontFace;
                                 out font: IDWriteFont): HResult; stdcall;

  end;
  IID_IDWriteFontCollection = IDWriteFontCollection;
  {$EXTERNALSYM IID_IDWriteFontCollection}


  // Interface IDWriteFontList
  // =========================
  // The IDWriteFontList interface represents an ordered set of fonts that are part of an IDWriteFontCollection.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontList);'}
  {$EXTERNALSYM IDWriteFontList}
  IDWriteFontList = interface(IUnknown)
  ['{1a0d8438-1d97-4ec1-aef9-a2fb86ed6acb}']

    // Gets the font collection that contains the fonts.
    // <param name="fontCollection">Receives a pointer to the font collection object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontCollection(out fontCollection: IDWriteFontCollection): HResult; stdcall;

    // Gets the number of fonts in the font list.
    function GetFontCount(): UINT32; stdcall;

    // Gets a font given its zero-based index.
    // <param name="index">Zero-based index of the font in the font list.</param>
    // <param name="font">Receives a pointer to the newly created font object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFont(index: UINT32;
                     out font: IDWriteFont): HResult; stdcall;

  end;
  IID_IDWriteFontList = IDWriteFontList;
  {$EXTERNALSYM IID_IDWriteFontList}


  // Interface IDWriteFontFamily
  // ===========================
  // The IDWriteFontFamily interface represents a set of fonts that share the same design but are differentiated
  // by weight, stretch, and style.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFamily);'}
  {$EXTERNALSYM IDWriteFontFamily}
  IDWriteFontFamily = interface(IDWriteFontList)
  ['{da20d8ef-812a-4c43-9802-62ec4abd7add}']

    // Creates a localized strings object that contains the family names for the font family, indexed by locale name.
    // <param name="names">Receives a pointer to the newly created localized strings object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFamilyNames(out names: IDWriteLocalizedStrings): HResult; stdcall;

    // Gets the font that best matches the specified properties.
    // <param name="weight">Requested font weight.</param>
    // <param name="stretch">Requested font stretch.</param>
    // <param name="style">Requested font style.</param>
    // <param name="matchingFont">Receives a pointer to the newly created font object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFirstMatchingFont(weight: DWRITE_FONT_WEIGHT;
                                  stretch: DWRITE_FONT_STRETCH;
                                  style: DWRITE_FONT_STYLE;
                                  out matchingFont: IDWriteFont): HResult; stdcall;

    // Gets a list of fonts in the font family ranked in order of how well they match the specified properties.
    // <param name="weight">Requested font weight.</param>
    // <param name="stretch">Requested font stretch.</param>
    // <param name="style">Requested font style.</param>
    // <param name="matchingFonts">Receives a pointer to the newly created font list object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetMatchingFonts(weight: DWRITE_FONT_WEIGHT;
                              stretch: DWRITE_FONT_STRETCH;
                              style: DWRITE_FONT_STYLE;
                              out matchingFonts: IDWriteFontList): HResult; stdcall;

  end;
  IID_IDWriteFontFamily = IDWriteFontFamily;
  {$EXTERNALSYM IID_IDWriteFontFamily}


  // Interface IDWriteFont
  // =====================
  // The IDWriteFont interface represents a physical font in a font collection.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFont);'}
  {$EXTERNALSYM IDWriteFont}
  IDWriteFont = interface(IUnknown)
  ['{acd16696-8c14-4f5d-877e-fe3fc1d32737}']

    // Gets the font family to which the specified font belongs.
    // <param name="fontFamily">Receives a pointer to the font family object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontFamily(out fontFamily: IDWriteFontFamily): HResult; stdcall;

    // Gets the weight of the specified font.
    function GetWeight(): DWRITE_FONT_WEIGHT; stdcall;

    // Gets the stretch (aka. width) of the specified font.
    function GetStretch(): DWRITE_FONT_STRETCH; stdcall;

    // Gets the style (aka. slope) of the specified font.
    function GetStyle(): DWRITE_FONT_STYLE; stdcall;

    // Returns TRUE if the font is a symbol font or FALSE if not.
    function IsSymbolFont(): BOOL; stdcall;

    // Gets a localized strings collection containing the face names for the font (e.g., Regular or Bold), indexed by locale name.
    // <param name="names">Receives a pointer to the newly created localized strings object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFaceNames(out names: IDWriteLocalizedStrings): HResult; stdcall;

    // Gets a localized strings collection containing the specified informational strings, indexed by locale name.
    // <param name="informationalStringID">Identifies the string to get.</param>
    // <param name="informationalStrings">Receives a pointer to the newly created localized strings object.</param>
    // <param name="exists">Receives the value TRUE if the font contains the specified string ID or FALSE if not.</param>
    // <returns>
    // Standard HRESULT error code. If the font does not contain the specified string, the return value is S_OK but
    // informationalStrings receives a NULL pointer and exists receives the value FALSE.
    // </returns>
    function GetInformationalStrings(informationalStringID: DWRITE_INFORMATIONAL_STRING_ID;
                                     out informationalStrings: IDWriteLocalizedStrings;
                                     out exists: BOOL): HResult; stdcall;

    // Gets a value that indicates what simulation are applied to the specified font.
    function GetSimulations(): DWRITE_FONT_SIMULATIONS; stdcall;

    // Gets the metrics for the font.
    // <param name="fontMetrics">Receives the font metrics.</param>
    procedure GetMetrics(out fontMetrics: DWRITE_FONT_METRICS); stdcall;

    // Determines whether the font supports the specified character.
    // <param name="unicodeValue">Unicode (UCS-4) character value.</param>
    // <param name="exists">Receives the value TRUE if the font supports the specified character or FALSE if not.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function HasCharacter(unicodeValue: UINT32;
                          out exists: BOOL): HResult; stdcall;

    // Creates a font face object for the font.
    // <param name="fontFace">Receives a pointer to the newly created font face object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateFontFace(out fontFace: IDWriteFontFace): HResult; stdcall;

  end;
  IID_IDWriteFont = IDWriteFont;
  {$EXTERNALSYM IID_IDWriteFont}


  // The DWRITE_TEXT_RANGE structure specifies a range of text positions where format is applied.
  PDWRITE_TEXT_RANGE = ^DWRITE_TEXT_RANGE;
  DWRITE_TEXT_RANGE = record
    // The start text position of the range.
    startPosition: UINT32;
    // The number of text positions in the range.
    length: UINT32;
  end;
  {$EXTERNALSYM DWRITE_TEXT_RANGE}


  // The DWRITE_FONT_FEATURE structure specifies properties used to identify and execute typographic feature in the font.
  PDWRITE_FONT_FEATURE = ^DWRITE_FONT_FEATURE;
  DWRITE_FONT_FEATURE = record
    // The feature OpenType name identifier.
    nameTag: DWRITE_FONT_FEATURE_TAG;
    // Execution parameter of the feature.
    // Remarks:
    //   The parameter should be non-zero to enable the feature.
    //   Once enabled, a feature can't be disabled again within
    //   the same range.
    //   Features requiring a selector use this value to indicate the selector index.
    parameter: UINT32;
  end;
  {$EXTERNALSYM DWRITE_FONT_FEATURE}


  // Defines a set of typographic features to be applied during shaping.
  // Notice the character range which this feature list spans is specified
  // as a separate parameter to GetGlyphs.
  PDWRITE_TYPOGRAPHIC_FEATURES = ^DWRITE_TYPOGRAPHIC_FEATURES;
  DWRITE_TYPOGRAPHIC_FEATURES = record
    // Array of font features.
    features: DWRITE_FONT_FEATURE;
    // The number of features.
    featureCount: UINT32;
  end;
  {$EXTERNALSYM DWRITE_TYPOGRAPHIC_FEATURES}


  // The DWRITE_TRIMMING structure specifies the trimming option for text overflowing the layout box.
  PDWRITE_TRIMMING = ^DWRITE_TRIMMING;
  DWRITE_TRIMMING = record
    // Text granularity of which trimming applies.
    granularity: DWRITE_TRIMMING_GRANULARITY;
    // Character code used as the delimiter signaling the beginning of the portion of text to be preserved,
    // most useful for path ellipsis, where the delimiter would be a slash. Leave this zero if there is no
    // delimiter.
    delimiter: UINT32;
    // How many occurrences of the delimiter to step back. Leave this zero if there is no delimiter.
    delimiterCount: UINT32;
  end;
  {$EXTERNALSYM DWRITE_TRIMMING}


  // Interface IDWriteTextFormat
  // ===========================
  // The format of text used for text layout.
  // <remarks>
  // This object may not be thread-safe and it may carry the state of text format change.
  // </remarks>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextFormat);'}
  {$EXTERNALSYM IDWriteTextFormat}
  IDWriteTextFormat = interface(IUnknown)
  ['{9c906818-31d7-4fd3-a151-7c5e225db55a}']

    // Set alignment option of text relative to layout box's leading and trailing edge.
    // <param name="textAlignment">Text alignment option</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetTextAlignment(textAlignment: DWRITE_TEXT_ALIGNMENT): HResult; stdcall;

    // Set alignment option of paragraph relative to layout box's top and bottom edge.
    // <param name="paragraphAlignment">Paragraph alignment option</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetParagraphAlignment(paragraphAlignment: DWRITE_PARAGRAPH_ALIGNMENT): HResult; stdcall;

    // Set word wrapping option.
    // <param name="wordWrapping">Word wrapping option</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetWordWrapping(wordWrapping: DWRITE_WORD_WRAPPING): HResult; stdcall;

    // Set paragraph reading direction.
    // <param name="readingDirection">Text reading direction</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // The flow direction must be perpendicular to the reading direction.
    // Setting both to a vertical direction or both to horizontal yields
    // DWRITE_E_FLOWDIRECTIONCONFLICTS when calling GetMetrics or Draw.
    // </remark>
    function SetReadingDirection(readingDirection: DWRITE_READING_DIRECTION): HResult; stdcall;

    // Set paragraph flow direction.
    // <param name="flowDirection">Paragraph flow direction</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // The flow direction must be perpendicular to the reading direction.
    // Setting both to a vertical direction or both to horizontal yields
    // DWRITE_E_FLOWDIRECTIONCONFLICTS when calling GetMetrics or Draw.
    // </remark>
    function SetFlowDirection(flowDirection: DWRITE_FLOW_DIRECTION): HResult; stdcall;

    // Set incremental tab stop position.
    // <param name="incrementalTabStop">The incremental tab stop value</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetIncrementalTabStop(incrementalTabStop: Single): HResult; stdcall;

    // Set trimming options for any trailing text exceeding the layout width
    // or for any far text exceeding the layout height.
    // <param name="trimmingOptions">Text trimming options.</param>
    // <param name="trimmingSign">Application-defined omission sign. This parameter may be NULL if no trimming sign is desired.</param>
    // <remarks>
    // Any inline object can be used for the trimming sign, but CreateEllipsisTrimmingSign
    // provides a typical ellipsis symbol. Trimming is also useful vertically for hiding
    // partial lines.
    // </remarks>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetTrimming(trimmingOptions: DWRITE_TRIMMING;
                         trimmingSign: IDWriteInlineObject): HResult; stdcall;

    // Set line spacing.
    // <param name="lineSpacingMethod">How to determine line height.</param>
    // <param name="lineSpacing">The line height, or rather distance between one baseline to another.</param>
    // <param name="baseline">Distance from top of line to baseline. A reasonable ratio to lineSpacing is 80%.</param>
    // <remarks>
    // For the default method, spacing depends solely on the content.
    // For uniform spacing, the given line height will override the content.
    // </remarks>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetLineSpacing(lineSpacingMethod: DWRITE_LINE_SPACING_METHOD;
                            lineSpacing: Single;
                            baseline: Single): HResult; stdcall;

    // Get alignment option of text relative to layout box's leading and trailing edge.
    function GetTextAlignment(): DWRITE_TEXT_ALIGNMENT; stdcall;

    // Get alignment option of paragraph relative to layout box's top and bottom edge.
    function GetParagraphAlignment(): DWRITE_PARAGRAPH_ALIGNMENT; stdcall;

    // Get word wrapping option.
    function GetWordWrapping(): DWRITE_WORD_WRAPPING; stdcall;

    // Get paragraph reading direction.
    function GetReadingDirection(): DWRITE_READING_DIRECTION; stdcall;

    // Get paragraph flow direction.
    function GetFlowDirection(): DWRITE_FLOW_DIRECTION; stdcall;

    // Get incremental tab stop position.
    function GetIncrementalTabStop(): Single; stdcall;

    // Get trimming options for text overflowing the layout width.
    // <param name="trimmingOptions">Text trimming options.</param>
    // <param name="trimmingSign">Trimming omission sign. This parameter may be NULL.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetTrimming(out trimmingOptions: DWRITE_TRIMMING;
                         out trimmingSign: IDWriteInlineObject): HResult; stdcall;

    // Get line spacing.
    // <param name="lineSpacingMethod">How line height is determined.</param>
    // <param name="lineSpacing">The line height, or rather distance between one baseline to another.</param>
    // <param name="baseline">Distance from top of line to baseline.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLineSpacing(out lineSpacingMethod: DWRITE_LINE_SPACING_METHOD;
                            out lineSpacing: Single;
                            out baseline: Single): HResult; stdcall;

    // Get the font collection.
    // <param name="fontCollection">The current font collection.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontCollection(out fontCollection: IDWriteFontCollection): HResult; stdcall;

    // Get the length of the font family name, in characters, not including the terminating NULL character.
    function GetFontFamilyNameLength(): UINT32; stdcall;

    // Get a copy of the font family name.
    // <param name="fontFamilyName">Character array that receives the current font family name</param>
    // <param name="nameSize">Size of the character array in character count including the terminated NULL character.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontFamilyName(out fontFamilyName: PWideChar;
                               nameSize: UINT32): HResult; stdcall;

    // Get the font weight.
    function GetFontWeight(): DWRITE_FONT_WEIGHT; stdcall;

    // Get the font style.
    function GetFontStyle(): DWRITE_FONT_STYLE; stdcall;

    // Get the font stretch.
    function GetFontStretch(): DWRITE_FONT_STRETCH; stdcall;

    // Get the font em height.
    function GetFontSize(): Single; stdcall;

    // Get the length of the locale name, in characters, not including the terminating NULL character.
    function GetLocaleNameLength(): UINT32; stdcall;

    // Get a copy of the locale name.
    // <param name="localeName">Character array that receives the current locale name</param>
    // <param name="nameSize">Size of the character array in character count including the terminated NULL character.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLocaleName(out localeName: PWideChar;
                           nameSize: UINT32): HResult; stdcall;

  end;
  IID_IDWriteTextFormat = IDWriteTextFormat;
  {$EXTERNALSYM IID_IDWriteTextFormat}



  // Interface IDWriteTypography
  // ===========================
  // Font typography setting.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTypography);'}
  {$EXTERNALSYM IDWriteTypography}
  IDWriteTypography = interface(IUnknown)
  ['{55f1112b-1dc2-4b3c-9541-f46894ed85b6}']

    // Add font feature.
    // <param name="fontFeature">The font feature to add.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function AddFontFeature(fontFeature: DWRITE_FONT_FEATURE): HResult; stdcall;

    // Get the number of font features.
    function GetFontFeatureCount(): UINT32; stdcall;

    // Get the font feature at the specified index.
    // <param name="fontFeatureIndex">The zero-based index of the font feature to get.</param>
    // <param name="fontFeature">The font feature.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontFeature(fontFeatureIndex: UINT32;
                            out fontFeature: DWRITE_FONT_FEATURE): HResult; stdcall;

  end;
  IID_IDWriteTypography = IDWriteTypography;
  {$EXTERNALSYM IID_IDWriteTypography}


  // Association of text and its writing system script as well as some display attributes.
  PDWRITE_SCRIPT_ANALYSIS = ^DWRITE_SCRIPT_ANALYSIS;
  DWRITE_SCRIPT_ANALYSIS = record
    // Zero-based index representation of writing system script.
    script: UINT16;
    // Additional shaping requirement of text.
    shapes: DWRITE_SCRIPT_SHAPES
  end;
  {$EXTERNALSYM DWRITE_SCRIPT_ANALYSIS}



  // Line breakpoint characteristics of a character.
  PDWRITE_LINE_BREAKPOINT = ^DWRITE_LINE_BREAKPOINT;
  DWRITE_LINE_BREAKPOINT = record

    data: UINT8;  //or byte

  private

    function GetUINT8(const index: Integer): UINT8;
    procedure SetUINT8(const index: Integer;
                       const value: UINT8);

  public
    // Breaking condition before the character.
    property breakConditionBefore: UINT8 index $0002 read GetUINT8 write SetUINT8; // mask $0002, offset 0
    // Breaking condition after the character.
    property breakConditionAfter: UINT8 index $0202 read GetUINT8 write SetUINT8;  // mask $0002, offset 2
    // The character is some form of whitespace, which may be meaningful
    // for justification.
    property isWhitespace: UINT8 index $0401 read GetUINT8 write SetUINT8;         // mask $0001, offset 4
    // The character is a soft hyphen, often used to indicate hyphenation
    // points inside words.
    property isSoftHyphen: UINT8 index $0501 read GetUINT8 write SetUINT8;         // mask $0001, offset 5
    // padding bits
    property padding: UINT8 index $0602 read GetUINT8 write SetUINT8;              // mask $0002, offset 6
  end;
  {$EXTERNALSYM DWRITE_LINE_BREAKPOINT}


  // Interface IDWriteNumberSubstitution
  // ===================================
  // Holds the appropriate digits and numeric punctuation for a given locale.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteNumberSubstitution);'}
  {$EXTERNALSYM IDWriteNumberSubstitution}
  IDWriteNumberSubstitution = interface(IUnknown)
  ['{14885CC9-BAB0-4f90-B6ED-5C366A2CD03D}']

  end;
  IID_IDWriteNumberSubstitution = IDWriteNumberSubstitution;
  {$EXTERNALSYM IID_IDWriteNumberSubstitution}


  // Shaping output properties per input character.
  PDWRITE_SHAPING_TEXT_PROPERTIES = ^DWRITE_SHAPING_TEXT_PROPERTIES;
  DWRITE_SHAPING_TEXT_PROPERTIES = record

    data : Word;

    private
      // Getter and setter
      // function DWRITE_SHAPING_TEXT_PROPERTIES.GetUINT16
      function GetUINT16(const index: Integer): Word;
      procedure SetUINT16(const index: Integer;
                          const value: Word);
    public
      // This character can be shaped independently from the others
      // (usually set for the space character). [1]
      property isShapedAlone: UINT16 Index $00000001 read GetUINT16 write SetUINT16;        // mask $0001, offset 0
      // Reserved for use by shaping engine. [1]
      property reserved1: UINT16 Index $00010001 read GetUINT16 write SetUINT16;            // mask $0001, offset 1
      // Glyph shaping can be cut after this point without affecting shaping
      // before or after it. Otherwise, splitting a call to GetGlyphs would
      // cause a reflow of glyph advances and shapes. [1]
      property canBreakShapingAfter: UINT16 Index $00020001 read GetUINT16 write SetUINT16; // mask $0001, offset 2
      // Reserved for use by shaping engine. [13]
      property reserved: UINT16      Index $0003000D read GetUINT16 write SetUINT16;        // mask $000D, offset 3
  end;
  {$EXTERNALSYM DWRITE_SHAPING_TEXT_PROPERTIES}


  // Shaping output properties per output glyph.
  // Delphi Note:
  PDWRITE_SHAPING_GLYPH_PROPERTIES = ^DWRITE_SHAPING_GLYPH_PROPERTIES;
  DWRITE_SHAPING_GLYPH_PROPERTIES = record

    data : UINT16; // or WORD;

    private
      // Getter and setter
      function GetUINT16(const index: Integer): Word;
      procedure SetUINT16(const index: Integer;
                          const value: Word);

    public
      // Justification class, whether to use spacing, kashidas, or
      // another method. This exists for backwards compatibility
      // with Uniscribe's SCRIPT_JUSTIFY enum.      [4]
      property justification: UINT16    index $0000000F read GetUINT16 write SetUINT16; // mask $000F, offset 0
      // Indicates glyph is the first of a cluster. [1]
      property isClusterStart: UINT16   index $00040001 read GetUINT16 write SetUINT16; // mask $0001, offset 4
      // Glyph is a diacritic.                      [1]
      property isDiacritic: UINT16      index $00050001 read GetUINT16 write SetUINT16; // mask $0001, offset 5
      // Glyph has no width, mark, ZWJ, ZWNJ, ZWSP, LRM etc.
      // This flag is not limited to just U+200B.   [1]
      property isZeroWidthSpace: UINT16 index $00060001 read GetUINT16 write SetUINT16; // mask $0001, offset 6
      // Reserved for use by shaping engine.        [9]
      property reserved: UINT16         index $00070009 read GetUINT16 write SetUINT16; // mask $0009, offset 7
  end;
  {$EXTERNALSYM DWRITE_SHAPING_GLYPH_PROPERTIES}


  // Interface IDWriteTextAnalysisSource
  // ===================================
  // The interface implemented by the text analyzer's client to provide text to
  // the analyzer. It allows the separation between the logical view of text as
  // a continuous stream of characters identifiable by unique text positions,
  // and the actual memory layout of potentially discrete blocks of text in the
  // client's backing store.
  //
  // If any of these callbacks returns an error, the analysis functions will
  // stop prematurely and return a callback error. Rather than return E_NOTIMPL,
  // an application should stub the method and return a constant/null and S_OK.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalysisSource);'}
  {$EXTERNALSYM IDWriteTextAnalysisSource}
  IDWriteTextAnalysisSource = interface(IUnknown)
  ['{688e1a58-5094-47c8-adc8-fbcea60ae92b}']

    // Get a block of text starting at the specified text position.
    // Returning NULL indicates the end of text - the position is after
    // the last character. This function is called iteratively for
    // each consecutive block, tying together several fragmented blocks
    // in the backing store into a virtual contiguous string.
    // <param name="textPosition">First position of the piece to obtain. All
    //     positions are in UTF16 code-units, not whole characters, which
    //     matters when supplementary characters are used.</param>
    // <param name="textString">Address that receives a pointer to the text block
    //     at the specified position.</param>
    // <param name="textLength">Number of UTF16 units of the retrieved chunk.
    //     The returned length is not the length of the block, but the length
    //     remaining in the block, from the given position until its end.
    //     So querying for a position that is 75 positions into a 100
    //     position block would return 25.</param>
    // <returns>Pointer to the first character at the given text position.
    // NULL indicates no chunk available at the specified position, either
    // because textPosition >= the entire text content length or because the
    // queried position is not mapped into the app's backing store.</returns>
    // <remarks>
    // Although apps can implement sparse textual content that only maps part of
    // the backing store, the app must map any text that is in the range passed
    // to any analysis functions.
    // </remarks>
    function GetTextAtPosition(textPosition: UINT32;
                               out textString: PWideChar;
                               out textLength: UINT32): HResult; stdcall;

    // Get a block of text immediately preceding the specified position.
    // <param name="textPosition">Position immediately after the last position of the chunk to obtain.</param>
    // <param name="textString">Address that receives a pointer to the text block
    //     at the specified position.</param>
    // <param name="textLength">Number of UTF16 units of the retrieved block.
    //     The length returned is from the given position to the front of
    //     the block.</param>
    // <returns>Pointer to the first character at (textPosition - textLength).
    // NULL indicates no chunk available at the specified position, either
    // because textPosition == 0,the textPosition > the entire text content
    // length, or the queried position is not mapped into the app's backing
    // store.</returns>
    // <remarks>
    // Although apps can implement sparse textual content that only maps part of
    // the backing store, the app must map any text that is in the range passed
    // to any analysis functions.
    // </remarks>
    function GetTextBeforePosition(textPosition: UINT32;
                                   out textString: PWideChar;
                                   out textLength: UINT32): HResult; stdcall;

    // Get paragraph reading direction.
    function GetParagraphReadingDirection(): DWRITE_READING_DIRECTION; stdcall;

    // Get locale name on the range affected by it.
    // <param name="textPosition">Position to get the locale name of.</param>
    // <param name="textLength">Receives the length from the given position up to the
    //     next differing locale.</param>
    // <param name="localeName">Address that receives a pointer to the locale
    //     at the specified position.</param>
    // <remarks>
    // The localeName pointer must remain valid until the next call or until
    // the analysis returns.
    // </remarks>
    function GetLocaleName(textPosition: UINT32;
                           out textLength: UINT32;
                           out localeName: PWideChar): HResult; stdcall;

    // Get number substitution on the range affected by it.
    // <param name="textPosition">Position to get the number substitution of.</param>
    // <param name="textLength">Receives the length from the given position up to the
    //     next differing number substitution.</param>
    // <param name="numberSubstitution">Address that receives a pointer to the number substitution
    //     at the specified position.</param>
    // <remarks>
    // Any implementation should return the number substitution with an
    // incremented ref count, and the analysis will release when finished
    // with it (either before the next call or before it returns). However,
    // the sink callback may hold onto it after that.
    // </remarks>
    function GetNumberSubstitution(textPosition: UINT32;
                                   out textLength: PUINT32;
                                   out numberSubstitution: IDWriteNumberSubstitution): HResult; stdcall;

  end;
  IID_IDWriteTextAnalysisSource = IDWriteTextAnalysisSource;
  {$EXTERNALSYM IID_IDWriteTextAnalysisSource}


  // Interface IDWriteTextAnalysisSink
  // =================================
  // The interface implemented by the text analyzer's client to receive the
  // output of a given text analysis. The Text analyzer disregards any current
  // state of the analysis sink, therefore a Set method call on a range
  // overwrites the previously set analysis result of the same range.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalysisSink);'}
  {$EXTERNALSYM IDWriteTextAnalysisSink}
  IDWriteTextAnalysisSink = interface(IUnknown)
  ['{5810cd44-0ca0-4701-b3fa-bec5182ae4f6}']
    // Report script analysis for the text range.
    // <param name="textPosition">Starting position to report from.</param>
    // <param name="textLength">Number of UTF16 units of the reported range.</param>
    // <param name="scriptAnalysis">Script analysis of characters in range.</param>
    // <returns>
    // A successful code or error code to abort analysis.
    // </returns>
    function SetScriptAnalysis(textPosition: UINT32;
                               textLength: UINT32;
                               scriptAnalysis: PDWRITE_SCRIPT_ANALYSIS): HResult; stdcall;

    // Report line-break opportunities for each character, starting from
    // the specified position.
    // <param name="textPosition">Starting position to report from.</param>
    // <param name="textLength">Number of UTF16 units of the reported range.</param>
    // <param name="lineBreakpoints">Breaking conditions for each character.</param>
    // <returns>
    // A successful code or error code to abort analysis.
    // </returns>
    function SetLineBreakpoints(textPosition: UINT32;
                                textLength: UINT32;
                                lineBreakpoints: PDWRITE_LINE_BREAKPOINT): HResult; stdcall;

    // Set bidirectional level on the range, called once per each
    // level run change (either explicit or resolved implicit).
    // <param name="textPosition">Starting position to report from.</param>
    // <param name="textLength">Number of UTF16 units of the reported range.</param>
    // <param name="explicitLevel">Explicit level from embedded control codes
    //     RLE/RLO/LRE/LRO/PDF, determined before any additional rules.</param>
    // <param name="resolvedLevel">Final implicit level considering the
    //     explicit level and characters' natural directionality, after all
    //     Bidi rules have been applied.</param>
    // <returns>
    // A successful code or error code to abort analysis.
    // </returns>
    function SetBidiLevel(textPosition: UINT32;
                          textLength: UINT32;
                          explicitLevel: UINT8;
                          resolvedLevel: UINT8): HResult; stdcall;

    // Set number substitution on the range.
    // <param name="textPosition">Starting position to report from.</param>
    // <param name="textLength">Number of UTF16 units of the reported range.</param>
    // <param name="numberSubstitution">The number substitution applicable to
    //     the returned range of text. The sink callback may hold onto it by
    //     incrementing its ref count.</param>
    // <returns>
    // A successful code or error code to abort analysis.
    // </returns>
    // <remark>
    // Unlike script and bidi analysis, where every character passed to the
    // analyzer has a result, this will only be called for those ranges where
    // substitution is applicable. For any other range, you will simply not
    // be called.
    // </remark>
    function SetNumberSubstitution(textPosition: UINT32;
                                   textLength: UINT32;
                                   numberSubstitution: IDWriteNumberSubstitution): HResult; stdcall;

  end;
  IID_IDWriteTextAnalysisSink = IDWriteTextAnalysisSink;
  {$EXTERNALSYM IID_IDWriteTextAnalysisSink}


  // Interface IDWriteTextAnalyzer
  // =============================
  // Analyzes various text properties for complex script processing.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalyzer);'}
  {$EXTERNALSYM IDWriteTextAnalyzer}
 IDWriteTextAnalyzer = interface(IUnknown)
  ['{b7e6163e-7f46-43b4-84b3-e4e6249c365d}']

    // Analyzes a text range for script boundaries, reading text attributes
    // from the source and reporting the Unicode script ID to the sink
    // callback SetScript.
    // <param name="analysisSource">Source object to analyze.</param>
    // <param name="textPosition">Starting position within the source object.</param>
    // <param name="textLength">Length to analyze.</param>
    // <param name="analysisSink">Callback object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function AnalyzeScript(analysisSource: IDWriteTextAnalysisSource;
                           textPosition: UINT32;
                           textLength: UINT32;
                           analysisSink: IDWriteTextAnalysisSink): HResult; stdcall;

    // Analyzes a text range for script directionality, reading attributes
    // from the source and reporting levels to the sink callback SetBidiLevel.
    // <param name="analysisSource">Source object to analyze.</param>
    // <param name="textPosition">Starting position within the source object.</param>
    // <param name="textLength">Length to analyze.</param>
    // <param name="analysisSink">Callback object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // While the function can handle multiple paragraphs, the text range
    // should not arbitrarily split the middle of paragraphs. Otherwise the
    // returned levels may be wrong, since the Bidi algorithm is meant to
    // apply to the paragraph as a whole.
    // </remarks>
    // <remarks>
    // Embedded control codes (LRE/LRO/RLE/RLO/PDF) are taken into account.
    // </remarks>
    function AnalyzeBidi(analysisSource: IDWriteTextAnalysisSource;
                         textPosition: UINT32;
                         textLength: UINT32;
                         analysisSink: IDWriteTextAnalysisSink): HResult; stdcall;

    // Analyzes a text range for spans where number substitution is applicable,
    // reading attributes from the source and reporting substitutable ranges
    // to the sink callback SetNumberSubstitution.
    // <param name="analysisSource">Source object to analyze.</param>
    // <param name="textPosition">Starting position within the source object.</param>
    // <param name="textLength">Length to analyze.</param>
    // <param name="analysisSink">Callback object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // While the function can handle multiple ranges of differing number
    // substitutions, the text ranges should not arbitrarily split the
    // middle of numbers. Otherwise it will treat the numbers separately
    // and will not translate any intervening punctuation.
    // </remarks>
    // <remarks>
    // Embedded control codes (LRE/LRO/RLE/RLO/PDF) are taken into account.
    // </remarks>
    function AnalyzeNumberSubstitution(analysisSource: IDWriteTextAnalysisSource;
                                       textPosition: UINT32;
                                       textLength: UINT32;
                                       analysisSink: IDWriteTextAnalysisSink): HResult; stdcall;

    // Analyzes a text range for potential breakpoint opportunities, reading
    // attributes from the source and reporting breakpoint opportunities to
    // the sink callback SetLineBreakpoints.
    // <param name="analysisSource">Source object to analyze.</param>
    // <param name="textPosition">Starting position within the source object.</param>
    // <param name="textLength">Length to analyze.</param>
    // <param name="analysisSink">Callback object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // While the function can handle multiple paragraphs, the text range
    // should not arbitrarily split the middle of paragraphs, unless the
    // given text span is considered a whole unit. Otherwise the
    // returned properties for the first and last characters will
    // inappropriately allow breaks.
    // </remarks>
    // <remarks>
    // Special cases include the first, last, and surrogate characters. Any
    // text span is treated as if adjacent to inline objects on either side.
    // So the rules with contingent-break opportunities are used, where the
    // edge between text and inline objects is always treated as a potential
    // break opportunity, dependent on any overriding rules of the adjacent
    // objects to prohibit or force the break (see Unicode TR #14).
    // Surrogate pairs never break between.
    // </remarks>
    function AnalyzeLineBreakpoints(analysisSource: IDWriteTextAnalysisSource;
                                    textPosition: UINT32;
                                    textLength: UINT32;
                                    analysisSink: IDWriteTextAnalysisSink): HResult; stdcall;

    // Parses the input text string and maps it to the set of glyphs and associated glyph data
    // according to the font and the writing system's rendering rules.
    // <param name="textString">The string to convert to glyphs.</param>
    // <param name="textLength">The length of textString.</param>
    // <param name="fontFace">The font face to get glyphs from.</param>
    // <param name="isSideways">Set to true if the text is intended to be
    // drawn vertically.</param>
    // <param name="isRightToLeft">Set to TRUE for right-to-left text.</param>
    // <param name="scriptAnalysis">Script analysis result from AnalyzeScript.</param>
    // <param name="localeName">The locale to use when selecting glyphs.
    // e.g. the same character may map to different glyphs for ja-jp vs zh-chs.
    // If this is NULL then the default mapping based on the script is used.</param>
    // <param name="numberSubstitution">Optional number substitution which
    // selects the appropriate glyphs for digits and related numeric characters,
    // depending on the results obtained from AnalyzeNumberSubstitution. Passing
    // null indicates that no substitution is needed and that the digits should
    // receive nominal glyphs.</param>
    // <param name="features">An array of pointers to the sets of typographic
    // features to use in each feature range.</param>
    // <param name="featureRangeLengths">The length of each feature range, in characters.
    // The sum of all lengths should be equal to textLength.</param>
    // <param name="featureRanges">The number of feature ranges.</param>
    // <param name="maxGlyphCount">The maximum number of glyphs that can be
    // returned.</param>
    // <param name="clusterMap">The mapping from character ranges to glyph
    // ranges.</param>
    // <param name="textProps">Per-character output properties.</param>
    // <param name="glyphIndices">Output glyph indices.</param>
    // <param name="glyphProps">Per-glyph output properties.</param>
    // <param name="actualGlyphCount">The actual number of glyphs returned if
    // the call succeeds.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // Note that the mapping from characters to glyphs is, in general, many-
    // to-many.  The recommended estimate for the per-glyph output buffers is
    // (3 * textLength / 2 + 16).  This is not guaranteed to be sufficient.
    //
    // The value of the actualGlyphCount parameter is only valid if the call
    // succeeds.  In the event that maxGlyphCount is not big enough
    // E_NOT_SUFFICIENT_BUFFER, which is equivalent to HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER),
    // will be returned.  The application should allocate a larger buffer and try again.
    // </remarks>
    function GetGlyphs(textString: PWideChar;
                       textLength: UINT32;
                       fontFace: IDWriteFontFace;
                       isSideways: BOOL;
                       isRightToLeft: BOOL;
                       scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                       localeName: PWideChar;
                       numberSubstitution: IDWriteNumberSubstitution;
                       features: DWRITE_TYPOGRAPHIC_FEATURES;
                       featureRangeLengths: UINT32;
                       featureRanges: UINT32;
                       maxGlyphCount: UINT32;
                       out clusterMap: UINT16;
                       out textProps: DWRITE_SHAPING_TEXT_PROPERTIES;
                       out glyphIndices: UINT16;
                       out glyphProps: DWRITE_SHAPING_GLYPH_PROPERTIES;
                       out actualGlyphCount: UINT32): HResult; stdcall;

    // Place glyphs output from the GetGlyphs method according to the font
    // and the writing system's rendering rules.
    // <param name="textString">The original string the glyphs came from.</param>
    // <param name="clusterMap">The mapping from character ranges to glyph
    // ranges. Returned by GetGlyphs.</param>
    // <param name="textProps">Per-character properties. Returned by
    // GetGlyphs.</param>
    // <param name="textLength">The length of textString.</param>
    // <param name="glyphIndices">Glyph indices. See GetGlyphs</param>
    // <param name="glyphProps">Per-glyph properties. See GetGlyphs</param>
    // <param name="glyphCount">The number of glyphs.</param>
    // <param name="fontFace">The font face the glyphs came from.</param>
    // <param name="fontEmSize">Logical font size in DIP's.</param>
    // <param name="isSideways">Set to true if the text is intended to be
    // drawn vertically.</param>
    // <param name="isRightToLeft">Set to TRUE for right-to-left text.</param>
    // <param name="scriptAnalysis">Script analysis result from AnalyzeScript.</param>
    // <param name="localeName">The locale to use when selecting glyphs.
    // e.g. the same character may map to different glyphs for ja-jp vs zh-chs.
    // If this is NULL then the default mapping based on the script is used.</param>
    // <param name="features">An array of pointers to the sets of typographic
    // features to use in each feature range.</param>
    // <param name="featureRangeLengths">The length of each feature range, in characters.
    // The sum of all lengths should be equal to textLength.</param>
    // <param name="featureRanges">The number of feature ranges.</param>
    // <param name="glyphAdvances">The advance width of each glyph.</param>
    // <param name="glyphOffsets">The offset of the origin of each glyph.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetGlyphPlacements(textString: PWideChar;
                                clusterMap: UINT16;
                                out textProps: DWRITE_SHAPING_TEXT_PROPERTIES;
                                textLength: UINT32;
                                glyphIndices: UINT16;
                                glyphProps: DWRITE_SHAPING_GLYPH_PROPERTIES;
                                glyphCount: UINT32;
                                fontFace: IDWriteFontFace;
                                fontEmSize: Single;
                                isSideways: BOOL;
                                isRightToLeft: BOOL;
                                scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                                localeName: PWideChar;
                                features: DWRITE_TYPOGRAPHIC_FEATURES;
                                featureRangeLengths: UINT32;
                                featureRanges: UINT32;
                                out glyphAdvances: Single;
                                out glyphOffsets: DWRITE_GLYPH_OFFSET): HResult; stdcall;

    // Place glyphs output from the GetGlyphs method according to the font
    // and the writing system's rendering rules.
    // <param name="textString">The original string the glyphs came from.</param>
    // <param name="clusterMap">The mapping from character ranges to glyph
    // ranges. Returned by GetGlyphs.</param>
    // <param name="textProps">Per-character properties. Returned by
    // GetGlyphs.</param>
    // <param name="textLength">The length of textString.</param>
    // <param name="glyphIndices">Glyph indices. See GetGlyphs</param>
    // <param name="glyphProps">Per-glyph properties. See GetGlyphs</param>
    // <param name="glyphCount">The number of glyphs.</param>
    // <param name="fontFace">The font face the glyphs came from.</param>
    // <param name="fontEmSize">Logical font size in DIP's.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For example, if the DPI of the rendering surface is 96 this
    // value is 1.0f. If the DPI is 120, this value is 120.0f/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified by the font size and pixelsPerDip.</param>
    // <param name="useGdiNatural">
    // When set to FALSE, the metrics are the same as the metrics of GDI aliased text.
    // When set to TRUE, the metrics are the same as the metrics of text measured by GDI using a font
    // created with CLEARTYPE_NATURAL_QUALITY.
    // </param>
    // <param name="isSideways">Set to true if the text is intended to be
    // drawn vertically.</param>
    // <param name="isRightToLeft">Set to TRUE for right-to-left text.</param>
    // <param name="scriptAnalysis">Script analysis result from AnalyzeScript.</param>
    // <param name="localeName">The locale to use when selecting glyphs.
    // e.g. the same character may map to different glyphs for ja-jp vs zh-chs.
    // If this is NULL then the default mapping based on the script is used.</param>
    // <param name="features">An array of pointers to the sets of typographic
    // features to use in each feature range.</param>
    // <param name="featureRangeLengths">The length of each feature range, in characters.
    // The sum of all lengths should be equal to textLength.</param>
    // <param name="featureRanges">The number of feature ranges.</param>
    // <param name="glyphAdvances">The advance width of each glyph.</param>
    // <param name="glyphOffsets">The offset of the origin of each glyph.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetGdiCompatibleGlyphPlacements(textString: PWideChar;
                                             clusterMap: UINT16;
                                             textProps: DWRITE_SHAPING_TEXT_PROPERTIES;
                                             textLength: UINT32;
                                             glyphIndices: UINT16;
                                             glyphProps: DWRITE_SHAPING_GLYPH_PROPERTIES;
                                             glyphCount: UINT32;
                                             fontFace: IDWriteFontFace;
                                             fontEmSize: Single;
                                             pixelsPerDip: Single;
                                             transform: DWRITE_MATRIX;
                                             useGdiNatural: BOOL;
                                             isSideways: BOOL;
                                             isRightToLeft: BOOL;
                                             scriptAnalysis: PDWRITE_SCRIPT_ANALYSIS;
                                             localeName: PWideChar;
                                             features: DWRITE_TYPOGRAPHIC_FEATURES;
                                             featureRangeLengths: UINT32;
                                             featureRanges: UINT32;
                                             out glyphAdvances: Single;
                                             out glyphOffsets: DWRITE_GLYPH_OFFSET): HResult; stdcall;

  end;
  IID_IDWriteTextAnalyzer = IDWriteTextAnalyzer;
  {$EXTERNALSYM IID_IDWriteTextAnalyzer}


  // The DWRITE_GLYPH_RUN structure contains the information needed by renderers
  // to draw glyph runs. All coordinates are in device independent pixels (DIPs).
  PDWRITE_GLYPH_RUN = ^DWRITE_GLYPH_RUN;
  DWRITE_GLYPH_RUN = record
    // The physical font face to draw with.
    fontFace: IDWriteFontFace;
    // Logical size of the font in DIPs, not points (equals 1/96 inch).
    fontEmSize: Single;
    // The number of glyphs.
    glyphCount: UINT32;
    // The indices to render.
    glyphIndices: UINT16;
    // Glyph advance widths.
    glyphAdvances: Single;
    // Glyph offsets.
    glyphOffsets: DWRITE_GLYPH_OFFSET;
    // If true, specifies that glyphs are rotated 90 degrees to the left and
    // vertical metrics are used. Vertical writing is achieved by specifying
    // isSideways = true and rotating the entire run 90 degrees to the right
    // via a rotate transform.
    isSideways: BOOL;
    // The implicit resolved bidi level of the run. Odd levels indicate
    // right-to-left languages like Hebrew and Arabic, while even levels
    // indicate left-to-right languages like English and Japanese (when
    // written horizontally). For right-to-left languages, the text origin
    // is on the right, and text should be drawn to the left.
    bidiLevel: UINT32;
  end;
  {$EXTERNALSYM DWRITE_GLYPH_RUN}


  // The DWRITE_GLYPH_RUN_DESCRIPTION structure contains additional properties
  // related to those in DWRITE_GLYPH_RUN.
  PDWRITE_GLYPH_RUN_DESCRIPTION = ^DWRITE_GLYPH_RUN_DESCRIPTION;
  DWRITE_GLYPH_RUN_DESCRIPTION = record
    // The locale name associated with this run.
    localeName: PWideChar;
    // The text associated with the glyphs.
    _string: PWideChar;
    // The number of characters (UTF16 code-units).
    // Note that this may be different than the number of glyphs.
    stringLength: UINT32;
    // An array of indices to the glyph indices array, of the first glyphs of
    // all the glyph clusters of the glyphs to render.
    clusterMap: UINT16;
    // Corresponding text position in the original string
    // this glyph run came from.
    textPosition: UINT32;
  end;
  {$EXTERNALSYM DWRITE_GLYPH_RUN_DESCRIPTION}


  // The DWRITE_UNDERLINE structure contains information about the size and
  // placement of underlines. All coordinates are in device independent
  // pixels (DIPs).
  PDWRITE_UNDERLINE = ^DWRITE_UNDERLINE;
  DWRITE_UNDERLINE = record
    // Width of the underline, measured parallel to the baseline.
    width: Single;
    // Thickness of the underline, measured perpendicular to the
    // baseline.
    thickness: Single;
    // Offset of the underline from the baseline.
    // A positive offset represents a position below the baseline and
    // a negative offset is above.
    offset: Single;
    // Height of the tallest run where the underline applies.
    runHeight: Single;
    // Reading direction of the text associated with the underline.  This
    // value is used to interpret whether the width value runs horizontally
    // or vertically.
    readingDirection: DWRITE_READING_DIRECTION;
    // Flow direction of the text associated with the underline.  This value
    // is used to interpret whether the thickness value advances top to
    // bottom, left to right, or right to left.
    flowDirection: DWRITE_FLOW_DIRECTION;
    // Locale of the text the underline is being drawn under. Can be
    // pertinent where the locale affects how the underline is drawn.
    // For example, in vertical text, the underline belongs on the
    // left for Chinese but on the right for Japanese.
    // This choice is completely left up to higher levels.
    localeName: PWideChar;
    // The measuring mode can be useful to the renderer to determine how
    // underlines are rendered, e.g. rounding the thickness to a whole pixel
    // in GDI-compatible modes.
    measuringMode: DWRITE_MEASURING_MODE;
  end;
  {$EXTERNALSYM DWRITE_UNDERLINE}


  // The DWRITE_STRIKETHROUGH structure contains information about the size and
  // placement of strikethroughs. All coordinates are in device independent
  // pixels (DIPs).
  PDWRITE_STRIKETHROUGH = ^DWRITE_STRIKETHROUGH;
  DWRITE_STRIKETHROUGH = record
    // Width of the strikethrough, measured parallel to the baseline.
    width: Single;
    // Thickness of the strikethrough, measured perpendicular to the
    // baseline.
    thickness: Single;
    // Offset of the strikethrough from the baseline.
    // A positive offset represents a position below the baseline and
    // a negative offset is above.
    offset: Single;
    // Reading direction of the text associated with the strikethrough.  This
    // value is used to interpret whether the width value runs horizontally
    // or vertically.
    readingDirection: DWRITE_READING_DIRECTION;
    // Flow direction of the text associated with the strikethrough.  This
    // value is used to interpret whether the thickness value advances top to
    // bottom, left to right, or right to left.
    flowDirection: DWRITE_FLOW_DIRECTION;
    // Locale of the range. Can be pertinent where the locale affects the style.
    localeName: PWideChar;
    // The measuring mode can be useful to the renderer to determine how
    // underlines are rendered, e.g. rounding the thickness to a whole pixel
    // in GDI-compatible modes.
    measuringMode: DWRITE_MEASURING_MODE;
  end;
  {$EXTERNALSYM DWRITE_STRIKETHROUGH}


  // The DWRITE_LINE_METRICS structure contains information about a formatted
  // line of text.
  PDWRITE_LINE_METRICS = ^DWRITE_LINE_METRICS;
  DWRITE_LINE_METRICS = record
    // The number of total text positions in the line.
    // This includes any trailing whitespace and newline characters.
    length: UINT32;
    // The number of whitespace positions at the end of the line.  Newline
    // sequences are considered whitespace.
    trailingWhitespaceLength: UINT32;
    // The number of characters in the newline sequence at the end of the line.
    // If the count is zero, then the line was either wrapped or it is the
    // end of the text.
    newlineLength: UINT32;
    // Height of the line as measured from top to bottom.
    height: Single;
    // Distance from the top of the line to its baseline.
    baseline: Single;
    // The line is trimmed.
    isTrimmed: BOOL;
  end;
  {$EXTERNALSYM DWRITE_LINE_METRICS}


  // The DWRITE_CLUSTER_METRICS structure contains information about a glyph cluster.
  PDWRITE_CLUSTER_METRICS = ^DWRITE_CLUSTER_METRICS;
  DWRITE_CLUSTER_METRICS = record

    // The total advance width of all glyphs in the cluster.
    width: Single;
    // The number of text positions in the cluster.
    length: UINT16;
    //
    data: UINT16; // or WORD;

    private

      function GetUINT16(const index: Integer): Word;
      procedure SetUINT16(const index: Integer;
                          const value: Word);
    public
    // Indicate whether line can be broken right after the cluster. [1]
    property canWrapLineAfter: WORD index $0001 read GetUINT16 write SetUINT16; // mask $0001, offset 0
    // Indicate whether the cluster corresponds to whitespace character. [1]
    property isWhitespace: WORD index $1001 read GetUINT16 write SetUINT16;     // mask $0001, offset 1
    // Indicate whether the cluster corresponds to a newline character. [1]
    property isNewline: WORD index $2001 read GetUINT16 write SetUINT16;        // mask $0001, offset 2
    // Indicate whether the cluster corresponds to soft hyphen character. [1]
    property isSoftHyphen: WORD index $3001 read GetUINT16 write SetUINT16;     // mask $0001, offset 3
    // Indicate whether the cluster is read from right to left. [1]
    property isRightToLeft: WORD index $4001 read GetUINT16 write SetUINT16;    // mask $0001, offset 4
    // Padding bits [11]
    property padding: WORD index $500B read GetUINT16 write SetUINT16;          // mask $000B, offset 5
  end;
  {$EXTERNALSYM DWRITE_CLUSTER_METRICS}


  // Overall metrics associated with text after layout.
  // All coordinates are in device independent pixels (DIPs).
  PDWRITE_TEXT_METRICS = ^DWRITE_TEXT_METRICS;
  DWRITE_TEXT_METRICS = record
    // Left-most point of formatted text relative to layout box
    // (excluding any glyph overhang).
    left: Single;       // Top-most point of formatted text relative to layout box
    // (excluding any glyph overhang).
    top: Single;
    // The width of the formatted text ignoring trailing whitespace
    // at the end of each line.
    width: Single;
    // The width of the formatted text taking into account the
    // trailing whitespace at the end of each line.
    widthIncludingTrailingWhitespace: Single;
    // The height of the formatted text. The height of an empty string
    // is determined by the size of the default font's line height.
    height: Single;
    // Initial width given to the layout. Depending on whether the text
    // was wrapped or not, it can be either larger or smaller than the
    // text content width.
    layoutWidth: Single;
    // Initial height given to the layout. Depending on the length of the
    // text, it may be larger or smaller than the text content height.
    layoutHeight: Single;
    // The maximum reordering count of any line of text, used
    // to calculate the most number of hit-testing boxes needed.
    // If the layout has no bidirectional text or no text at all,
    // the minimum level is 1.
    maxBidiReorderingDepth: UINT32;
    // Total number of lines.
    lineCount: UINT32;
  end;
  {$EXTERNALSYM DWRITE_TEXT_METRICS}


  // Properties describing the geometric measurement of an
  // application-defined inline object.
  PDWRITE_INLINE_OBJECT_METRICS = ^DWRITE_INLINE_OBJECT_METRICS;
  DWRITE_INLINE_OBJECT_METRICS = record
    // Width of the inline object.
    width: Single;
    // Height of the inline object as measured from top to bottom.
    height: Single;
    // Distance from the top of the object to the baseline where it is lined up with the adjacent text.
    // If the baseline is at the bottom, baseline simply equals height.
    baseline: Single;
    // Flag indicating whether the object is to be placed upright or alongside the text baseline
    // for vertical text.
    supportsSideways: BOOL;
  end;
  {$EXTERNALSYM DWRITE_INLINE_OBJECT_METRICS}


  // The DWRITE_OVERHANG_METRICS structure holds how much any visible pixels
  // (in DIPs) overshoot each side of the layout or inline objects.
  // <remarks>
  // Positive overhangs indicate that the visible area extends outside the layout
  // box or inline object, while negative values mean there is whitespace inside.
  // The returned values are unaffected by rendering transforms or pixel snapping.
  // Additionally, they may not exactly match final target's pixel bounds after
  // applying grid fitting and hinting.
  // </remarks>
  PDWRITE_OVERHANG_METRICS = ^DWRITE_OVERHANG_METRICS;
  DWRITE_OVERHANG_METRICS = record
    // The distance from the left-most visible DIP to its left alignment edge.
    left: Single;
    // The distance from the top-most visible DIP to its top alignment edge.
    top: Single;
    // The distance from the right-most visible DIP to its right alignment edge.
    right: Single;
    // The distance from the bottom-most visible DIP to its bottom alignment edge.
    bottom: Single;
  end;
  {$EXTERNALSYM DWRITE_OVERHANG_METRICS}


  // Geometry enclosing of text positions.
  PDWRITE_HIT_TEST_METRICS = ^DWRITE_HIT_TEST_METRICS;
  DWRITE_HIT_TEST_METRICS = record
    // First text position within the geometry.
    textPosition: UINT32;
    // Number of text positions within the geometry.
    length: UINT32;
    // Left position of the top-left coordinate of the geometry.
    left: Single;
    // Top position of the top-left coordinate of the geometry.
    top: Single;
    // Geometry's width.
    width: Single;
    // Geometry's height.
    height: Single;
    // Bidi level of text positions enclosed within the geometry.
    bidiLevel: UINT32;
    // Geometry encloses text?
    isText: BOOL;
    // Range is trimmed.
    isTrimmed: BOOL;
  end;
  {$EXTERNALSYM DWRITE_HIT_TEST_METRICS}


  // Interface IDWriteInlineObject
  // =============================
  // The IDWriteInlineObject interface wraps an application defined inline graphic,
  // allowing DWrite to query metrics as if it was a glyph inline with the text.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteInlineObject);'}
  {$EXTERNALSYM IDWriteInlineObject}
  IDWriteInlineObject = interface(IUnknown)
  ['{8339FDE3-106F-47ab-8373-1C6295EB10B3}']

    // The application implemented rendering callback (IDWriteTextRenderer::DrawInlineObject)
    // can use this to draw the inline object without needing to cast or query the object
    // type. The text layout does not call this method directly.
    // <param name="clientDrawingContext">The context passed to IDWriteTextLayout::Draw.</param>
    // <param name="renderer">The renderer passed to IDWriteTextLayout::Draw as the object's containing parent.</param>
    // <param name="originX">X-coordinate at the top-left corner of the inline object.</param>
    // <param name="originY">Y-coordinate at the top-left corner of the inline object.</param>
    // <param name="isSideways">The object should be drawn on its side.</param>
    // <param name="isRightToLeft">The object is in an right-to-left context and should be drawn flipped.</param>
    // <param name="clientDrawingEffect">The drawing effect set in IDWriteTextLayout::SetDrawingEffect.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function Draw(clientDrawingContext: Pointer;
                  renderer: IDWriteTextRenderer;
                  originX: Single;
                  originY: Single;
                  isSideways: BOOL;
                  isRightToLeft: BOOL;
                  clientDrawingEffect: IUnknown): HResult; stdcall;

    // TextLayout calls this callback function to get the measurement of the inline object.
    // <param name="metrics">Returned metrics</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetMetrics(out metrics: DWRITE_INLINE_OBJECT_METRICS): HResult; stdcall;

    // TextLayout calls this callback function to get the visible extents (in DIPs) of the inline object.
    // In the case of a simple bitmap, with no padding and no overhang, all the overhangs will
    // simply be zeroes.
    // <param name="overhangs">Overshoot of visible extents (in DIPs) outside the object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // The overhangs should be returned relative to the reported size of the object
    // (DWRITE_INLINE_OBJECT_METRICS::width/height), and should not be baseline
    // adjusted. If you have an image that is actually 100x100 DIPs, but you want it
    // slightly inset (perhaps it has a glow) by 20 DIPs on each side, you would
    // return a width/height of 60x60 and four overhangs of 20 DIPs.
    // </remarks>
    function GetOverhangMetrics(out overhangs: DWRITE_OVERHANG_METRICS): HResult; stdcall;

    // Layout uses this to determine the line breaking behavior of the inline object
    // amidst the text.
    // <param name="breakConditionBefore">Line-breaking condition between the object and the content immediately preceding it.</param>
    // <param name="breakConditionAfter" >Line-breaking condition between the object and the content immediately following it.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetBreakConditions(out breakConditionBefore: DWRITE_BREAK_CONDITION;
                                out breakConditionAfter: DWRITE_BREAK_CONDITION): HResult; stdcall;

  end;
  IID_IDWriteInlineObject = IDWriteInlineObject;
  {$EXTERNALSYM IID_IDWriteInlineObject}


  // Interface IDWritePixelSnapping
  // ==============================
  // The IDWritePixelSnapping interface defines the pixel snapping properties of a text renderer.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWritePixelSnapping);'}
  {$EXTERNALSYM IDWritePixelSnapping}
  IDWritePixelSnapping = interface(IUnknown)
  ['{eaf3a2da-ecf4-4d24-b644-b34f6842024b}']

    // Determines whether pixel snapping is disabled. The recommended default is FALSE,
    // unless doing animation that requires subpixel vertical placement.
    // <param name="clientDrawingContext">The context passed to IDWriteTextLayout::Draw.</param>
    // <param name="isDisabled">Receives TRUE if pixel snapping is disabled or FALSE if it not.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function IsPixelSnappingDisabled(clientDrawingContext: Pointer;
                                     out isDisabled: BOOL): HResult; stdcall;

    // Gets the current transform that maps abstract coordinates to DIPs,
    // which may disable pixel snapping upon any rotation or shear.
    // <param name="clientDrawingContext">The context passed to IDWriteTextLayout::Draw.</param>
    // <param name="transform">Receives the transform.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetCurrentTransform(clientDrawingContext: Pointer;
                                 out transform: DWRITE_MATRIX): HResult; stdcall;

    // Gets the number of physical pixels per DIP. A DIP (device-independent pixel) is 1/96 inch,
    // so the pixelsPerDip value is the number of logical pixels per inch divided by 96 (yielding
    // a value of 1 for 96 DPI and 1.25 for 120).
    // <param name="clientDrawingContext">The context passed to IDWriteTextLayout::Draw.</param>
    // <param name="pixelsPerDip">Receives the number of physical pixels per DIP.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetPixelsPerDip(clientDrawingContext: Pointer;
                             out pixelsPerDip: Single): HResult; stdcall;

  end;
  IID_IDWritePixelSnapping = IDWritePixelSnapping;
  {$EXTERNALSYM IID_IDWritePixelSnapping}


  // Interface IDWriteTextRenderer
  // =============================
  // The IDWriteTextRenderer interface represents a set of application-defined
  // callbacks that perform rendering of text, inline objects, and decorations
  // such as underlines.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextRenderer);'}
  {$EXTERNALSYM IDWriteTextRenderer}
  IDWriteTextRenderer = interface(IDWritePixelSnapping)
  ['{ef8a8135-5cc6-45fe-8825-c5a0724eb819}']

    // IDWriteTextLayout.Draw calls this function to instruct the client to
    // render a run of glyphs.
    // <param name="clientDrawingContext">The context passed to
    // IDWriteTextLayout::Draw.</param>
    // <param name="baselineOriginX">X-coordinate of the baseline.</param>
    // <param name="baselineOriginY">Y-coordinate of the baseline.</param>
    // <param name="measuringMode">Specifies measuring mode for glyphs in the run.
    // Renderer implementations may choose different rendering modes for given measuring modes,
    // but best results are seen when the rendering mode matches the corresponding measuring mode:
    // DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL for DWRITE_MEASURING_MODE_NATURAL
    // DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC for DWRITE_MEASURING_MODE_GDI_CLASSIC
    // DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL for DWRITE_MEASURING_MODE_GDI_NATURAL
    // </param>
    // <param name="glyphRun">The glyph run to draw.</param>
    // <param name="glyphRunDescription">Properties of the characters
    // associated with this run.</param>
    // <param name="clientDrawingEffect">The drawing effect set in
    // IDWriteTextLayout.SetDrawingEffect.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function DrawGlyphRun(clientDrawingContext: Pointer;
                          baselineOriginX: Single;
                          baselineOriginY: Single;
                          measuringMode: DWRITE_MEASURING_MODE;
                          glyphRun: DWRITE_GLYPH_RUN;
                          glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                          clientDrawingEffect: IUnknown): HResult; stdcall;

    // IDWriteTextLayout::Draw calls this function to instruct the client to draw
    // an underline.
    // <param name="clientDrawingContext">The context passed to
    // IDWriteTextLayout::Draw.</param>
    // <param name="baselineOriginX">X-coordinate of the baseline.</param>
    // <param name="baselineOriginY">Y-coordinate of the baseline.</param>
    // <param name="underline">Underline logical information.</param>
    // <param name="clientDrawingEffect">The drawing effect set in
    // IDWriteTextLayout::SetDrawingEffect.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // A single underline can be broken into multiple calls, depending on
    // how the formatting changes attributes. If font sizes/styles change
    // within an underline, the thickness and offset will be averaged
    // weighted according to characters.
    // To get the correct top coordinate of the underline rect, add underline::offset
    // to the baseline's Y. Otherwise the underline will be immediately under the text.
    // The x coordinate will always be passed as the left side, regardless
    // of text directionality. This simplifies drawing and reduces the
    // problem of round-off that could potentially cause gaps or a double
    // stamped alpha blend. To avoid alpha overlap, round the end points
    // to the nearest device pixel.
    // </remarks>
    function DrawUnderline(clientDrawingContext: Pointer;
                           baselineOriginX: Single;
                           baselineOriginY: Single;
                           underline: DWRITE_UNDERLINE;
                           clientDrawingEffect: IUnknown): HResult; stdcall;

    // IDWriteTextLayout.Draw calls this function to instruct the client to draw
    // a strikethrough.
    // <param name="clientDrawingContext">The context passed to
    // IDWriteTextLayout::Draw.</param>
    // <param name="baselineOriginX">X-coordinate of the baseline.</param>
    // <param name="baselineOriginY">Y-coordinate of the baseline.</param>
    // <param name="strikethrough">Strikethrough logical information.</param>
    // <param name="clientDrawingEffect">The drawing effect set in
    // IDWriteTextLayout::SetDrawingEffect.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // A single strikethrough can be broken into multiple calls, depending on
    // how the formatting changes attributes. Strikethrough is not averaged
    // across font sizes/styles changes.
    // To get the correct top coordinate of the strikethrough rect,
    // add strikethrough.offset to the baseline's Y.
    // Like underlines, the x coordinate will always be passed as the left side,
    // regardless of text directionality.
    // </remarks>
    function DrawStrikethrough(clientDrawingContext: Pointer;
                               baselineOriginX: Single;
                               baselineOriginY: Single;
                               strikethrough: DWRITE_STRIKETHROUGH;
                               clientDrawingEffect: IUnknown): HResult; stdcall;

    // IDWriteTextLayout.Draw calls this application callback when it needs to
    // draw an inline object.
    // <param name="clientDrawingContext">The context passed to IDWriteTextLayout::Draw.</param>
    // <param name="originX">X-coordinate at the top-left corner of the inline object.</param>
    // <param name="originY">Y-coordinate at the top-left corner of the inline object.</param>
    // <param name="inlineObject">The object set using IDWriteTextLayout::SetInlineObject.</param>
    // <param name="isSideways">The object should be drawn on its side.</param>
    // <param name="isRightToLeft">The object is in an right-to-left context and should be drawn flipped.</param>
    // <param name="clientDrawingEffect">The drawing effect set in
    // IDWriteTextLayout.SetDrawingEffect.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // The right-to-left flag is a hint for those cases where it would look
    // strange for the image to be shown normally (like an arrow pointing to
    // right to indicate a submenu).
    // </remarks>
    function DrawInlineObject(clientDrawingContext: Pointer;
                              originX: Single;
                              originY: Single;
                              inlineObject: IDWriteInlineObject;
                              isSideways: BOOL;
                              isRightToLeft: BOOL;
                              clientDrawingEffect: IUnknown): HResult; stdcall;

  end;
  IID_IDWriteTextRenderer = IDWriteTextRenderer;
  {$EXTERNALSYM IID_IDWriteTextRenderer}


  // All coordinates are in device independent pixels (DIPs).
  // Interface IDWriteTextLayout
  // ===========================
  // The IDWriteTextLayout interface represents a block of text after it has
  // been fully analyzed and formatted.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextLayout);'}
  {$EXTERNALSYM IDWriteTextLayout}
  IDWriteTextLayout = interface(IDWriteTextFormat)
  ['{53737037-6d14-410b-9bfe-0b182bb70961}']

    // Set layout maximum width
    // <param name="maxWidth">Layout maximum width</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetMaxWidth(maxWidth: Single): HResult; stdcall;

    // Set layout maximum height
    // <param name="maxHeight">Layout maximum height</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetMaxHeight(maxHeight: Single): HResult; stdcall;

    // Set the font collection.
    // <param name="fontCollection">The font collection to set</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetFontCollection(fontCollection: IDWriteFontCollection;
                               textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set null-terminated font family name.
    // <param name="fontFamilyName">Font family name</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetFontFamilyName(fontFamilyName: PWideChar;
                               textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set font weight.
    // <param name="fontWeight">Font weight</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetFontWeight(fontWeight: DWRITE_FONT_WEIGHT;
                           textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set font style.
    // <param name="fontStyle">Font style</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetFontStyle(fontStyle: DWRITE_FONT_STYLE;
                          textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set font stretch.
    // <param name="fontStretch">font stretch</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetFontStretch(fontStretch: DWRITE_FONT_STRETCH;
                            textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set font em height.
    // <param name="fontSize">Font em height</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetFontSize(fontSize: Single;
                         textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set underline.
    // <param name="hasUnderline">The Boolean flag indicates whether underline takes place</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetUnderline(hasUnderline: BOOL;
                          textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set strikethrough.
    // <param name="hasStrikethrough">The Boolean flag indicates whether strikethrough takes place</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetStrikethrough(hasStrikethrough: BOOL;
                              textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set application-defined drawing effect.
    // <param name="drawingEffect">Pointer to an application-defined drawing effect.</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // This drawing effect is associated with the specified range and will be passed back
    // to the application via the callback when the range is drawn at drawing time.
    // </remarks>
    function SetDrawingEffect(drawingEffect: IUnknown;
                              textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set inline object.
    // <param name="inlineObject">Pointer to an application-implemented inline object.</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // This inline object applies to the specified range and will be passed back
    // to the application via the DrawInlineObject callback when the range is drawn.
    // Any text in that range will be suppressed.
    // </remarks>
    function SetInlineObject(inlineObject: IDWriteInlineObject;
                             textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set font typography features.
    // <param name="typography">Pointer to font typography setting.</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetTypography(typography: IDWriteTypography;
                           textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Set locale name.
    // <param name="localeName">Locale name</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetLocaleName(localeName: PWideChar;
                           textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Get layout maximum width
    function GetMaxWidth(): Single; stdcall;

    // Get layout maximum height
    function GetMaxHeight(): Single; stdcall;

    // Get the font collection where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="fontCollection">The current font collection</param>
    // <param name="textRange">Text range to which this change applies.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontCollection(currentPosition: UINT32;
                               out fontCollection: IDWriteFontCollection;
                               textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the length of the font family name where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="nameLength">Size of the character array in character count not including the terminated NULL character.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontFamilyNameLength(currentPosition: UINT32;
                                     out nameLength: PUINT32;
                                     textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Copy the font family name where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="fontFamilyName">Character array that receives the current font family name</param>
    // <param name="nameSize">Size of the character array in character count including the terminated NULL character.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontFamilyName(currentPosition: UINT32;
                               out fontFamilyName: PWideChar;
                               nameSize: UINT32;
                               textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the font weight where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="fontWeight">The current font weight</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontWeight(currentPosition: UINT32;
                           out fontWeight: DWRITE_FONT_WEIGHT;
                           textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the font style where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="fontStyle">The current font style</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontStyle(currentPosition: UINT32;
                          out fontStyle: DWRITE_FONT_STYLE;
                          textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the font stretch where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="fontStretch">The current font stretch</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontStretch(currentPosition: UINT32;
                            out fontStretch: DWRITE_FONT_STRETCH;
                            textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the font em height where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="fontSize">The current font em height</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetFontSize(currentPosition: UINT32;
                         out fontSize: Single;
                         textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the underline presence where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="hasUnderline">The Boolean flag indicates whether text is underlined.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetUnderline(currentPosition: UINT32;
                          out hasUnderline: PBOOL;
                          textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the strikethrough presence where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="hasStrikethrough">The Boolean flag indicates whether text has strikethrough.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetStrikethrough(currentPosition: UINT32;
                              out hasStrikethrough: PBOOL;
                              textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the application-defined drawing effect where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="drawingEffect">The current application-defined drawing effect.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetDrawingEffect(currentPosition: UINT32;
                              out drawingEffect: IUnknown;
                              textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the inline object at the given position.
    // <param name="currentPosition">The given text position.</param>
    // <param name="inlineObject">The inline object.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetInlineObject(currentPosition: UINT32;
                             out inlineObject: IDWriteInlineObject;
                             textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the typography setting where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="typography">The current typography setting.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetTypography(currentPosition: UINT32;
                           out typography: IDWriteTypography;
                           textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the length of the locale name where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="nameLength">Size of the character array in character count not including the terminated NULL character.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLocaleNameLength(currentPosition: UINT32;
                                 out nameLength: UINT32;
                                 textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the locale name where the current position is at.
    // <param name="currentPosition">The current text position.</param>
    // <param name="localeName">Character array that receives the current locale name</param>
    // <param name="nameSize">Size of the character array in character count including the terminated NULL character.</param>
    // <param name="textRange">The position range of the current format.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetLocaleName(currentPosition: UINT32;
                           out localeName: PWideChar;
                           nameSize: UINT32;
                           textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Initiate drawing of the text.
    // <param name="clientDrawingContext">An application defined value
    // included in rendering callbacks.</param>
    // <param name="renderer">The set of application-defined callbacks that do
    // the actual rendering.</param>
    // <param name="originX">X-coordinate of the layout's left side.</param>
    // <param name="originY">Y-coordinate of the layout's top side.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function Draw(clientDrawingContext: Pointer;
                  renderer: IDWriteTextRenderer;
                  originX: Single;
                  originY: Single): HResult; stdcall;

    // GetLineMetrics returns properties of each line.
    // <param name="lineMetrics">The array to fill with line information.</param>
    // <param name="maxLineCount">The maximum size of the lineMetrics array.</param>
    // <param name="actualLineCount">The actual size of the lineMetrics
    // array that is needed.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // If maxLineCount is not large enough E_NOT_SUFFICIENT_BUFFER,
    // which is equivalent to HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER),
    // is returned and *actualLineCount is set to the number of lines
    // needed.
    // </remarks>
    function GetLineMetrics(lineMetrics: PDWRITE_LINE_METRICS;
                            maxLineCount: UINT32;
                            out actualLineCount: UINT32): HResult; stdcall;

    // GetMetrics retrieves overall metrics for the formatted string.
    // <param name="textMetrics">The returned metrics.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // Drawing effects like underline and strikethrough do not contribute
    // to the text size, which is essentially the sum of advance widths and
    // line heights. Additionally, visible swashes and other graphic
    // adornments may extend outside the returned width and height.
    // </remarks>
    function GetMetrics(out textMetrics: DWRITE_TEXT_METRICS): HResult; stdcall;

    // GetOverhangMetrics returns the overhangs (in DIPs) of the layout and all
    // objects contained in it, including text glyphs and inline objects.
    // <param name="overhangs">Overshoots of visible extents (in DIPs) outside the layout.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // Any underline and strikethrough do not contribute to the black box
    // determination, since these are actually drawn by the renderer, which
    // is allowed to draw them in any variety of styles.
    // </remarks>
    function GetOverhangMetrics(out overhangs: DWRITE_OVERHANG_METRICS): HResult; stdcall;

    // Retrieve logical properties and measurement of each cluster.
    // <param name="clusterMetrics">The array to fill with cluster information.</param>
    // <param name="maxClusterCount">The maximum size of the clusterMetrics array.</param>
    // <param name="actualClusterCount">The actual size of the clusterMetrics array that is needed.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // If maxClusterCount is not large enough E_NOT_SUFFICIENT_BUFFER,
    // which is equivalent to HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER),
    // is returned and *actualClusterCount is set to the number of clusters
    // needed.
    // </remarks>
    function GetClusterMetrics(clusterMetrics: PDWRITE_CLUSTER_METRICS; // Pointer to array of DWRITE_CLUSTER_METRICS
                               maxClusterCount: UINT32;
                               out actualClusterCount: UINT32): HResult; stdcall;

    // Determines the minimum possible width the layout can be set to without
    // emergency breaking between the characters of whole words.
    // <param name="minWidth">Minimum width.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function DetermineMinWidth(out minWidth: Single): HResult; stdcall;

    // Given a coordinate (in DIPs) relative to the top-left of the layout box,
    // this returns the corresponding hit-test metrics of the text string where
    // the hit-test has occurred. This is useful for mapping mouse clicks to caret
    // positions. When the given coordinate is outside the text string, the function
    // sets the output value *isInside to false but returns the nearest character
    // position.
    // <param name="pointX">X coordinate to hit-test, relative to the top-left location of the layout box.</param>
    // <param name="pointY">Y coordinate to hit-test, relative to the top-left location of the layout box.</param>
    // <param name="isTrailingHit">Output flag indicating whether the hit-test location is at the leading or the trailing
    //     side of the character. When the output *isInside value is set to false, this value is set according to the output
    //     *position value to represent the edge closest to the hit-test location. </param>
    // <param name="isInside">Output flag indicating whether the hit-test location is inside the text string.
    //     When false, the position nearest the text's edge is returned.</param>
    // <param name="hitTestMetrics">Output geometry fully enclosing the hit-test location. When the output *isInside value
    //     is set to false, this structure represents the geometry enclosing the edge closest to the hit-test location.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function HitTestPoint(pointX: Single;
                          pointY: Single;
                          out isTrailingHit: BOOL;
                          out isInside: BOOL;
                          out hitTestMetrics: DWRITE_HIT_TEST_METRICS): HResult; stdcall;

    // Given a text position and whether the caret is on the leading or trailing
    // edge of that position, this returns the corresponding coordinate (in DIPs)
    // relative to the top-left of the layout box. This is most useful for drawing
    // the caret's current position, but it could also be used to anchor an IME to the
    // typed text or attach a floating menu near the point of interest. It may also be
    // used to programmatically obtain the geometry of a particular text position
    // for UI automation.
    // <param name="textPosition">Text position to get the coordinate of.</param>
    // <param name="isTrailingHit">Flag indicating whether the location is of the leading or the trailing side of the specified text position. </param>
    // <param name="pointX">Output caret X, relative to the top-left of the layout box.</param>
    // <param name="pointY">Output caret Y, relative to the top-left of the layout box.</param>
    // <param name="hitTestMetrics">Output geometry fully enclosing the specified text position.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // When drawing a caret at the returned X,Y, it should be centered on X
    // and drawn from the Y coordinate down. The height will be the size of the
    // hit-tested text (which can vary in size within a line).
    // Reading direction also affects which side of the character the caret is drawn.
    // However, the returned X coordinate will be correct for either case.
    // You can get a text length back that is larger than a single character.
    // This happens for complex scripts when multiple characters form a single cluster,
    // when diacritics join their base character, or when you test a surrogate pair.
    // </remarks>
    function HitTestTextPosition(textPosition: UINT32;
                                 isTrailingHit: BOOL;
                                 out pointX: Single;
                                 out pointY: Single;
                                 out hitTestMetrics: DWRITE_HIT_TEST_METRICS): HResult; stdcall;

    // The application calls this function to get a set of hit-test metrics
    // corresponding to a range of text positions. The main usage for this
    // is to draw highlighted selection of the text string.
    //
    // The function returns E_NOT_SUFFICIENT_BUFFER, which is equivalent to
    // HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER), when the buffer size of
    // hitTestMetrics is too small to hold all the regions calculated by the
    // function. In such situation, the function sets the output value
    // *actualHitTestMetricsCount to the number of geometries calculated.
    // The application is responsible to allocate a new buffer of greater
    // size and call the function again.
    //
    // A good value to use as an initial value for maxHitTestMetricsCount may
    // be calculated from the following equation:
    //     maxHitTestMetricsCount = lineCount * maxBidiReorderingDepth
    //
    // where lineCount is obtained from the value of the output argument
    // *actualLineCount from the function IDWriteTextLayout::GetLineMetrics,
    // and the maxBidiReorderingDepth value from the DWRITE_TEXT_METRICS
    // structure of the output argument *textMetrics from the function
    // IDWriteFactory::CreateTextLayout.
    // <param name="textPosition">First text position of the specified range.</param>
    // <param name="textLength">Number of positions of the specified range.</param>
    // <param name="originX">Offset of the X origin (left of the layout box) which is added to each of the hit-test metrics returned.</param>
    // <param name="originY">Offset of the Y origin (top of the layout box) which is added to each of the hit-test metrics returned.</param>
    // <param name="hitTestMetrics">Pointer to a buffer of the output geometry fully enclosing the specified position range.</param>
    // <param name="maxHitTestMetricsCount">Maximum number of distinct metrics it could hold in its buffer memory.</param>
    // <param name="actualHitTestMetricsCount">Actual number of metrics returned or needed.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // There are no gaps in the returned metrics. While there could be visual gaps,
    // depending on bidi ordering, each range is contiguous and reports all the text,
    // including any hidden characters and trimmed text.
    // The height of each returned range will be the same within each line, regardless
    // of how the font sizes vary.
    // </remarks>
    function HitTestTextRange(textPosition: UINT32;
                              textLength: UINT32;
                              originX: Single;
                              originY: Single;
                              out hitTestMetrics: DWRITE_HIT_TEST_METRICS;
                              maxHitTestMetricsCount: UINT32;
                              out actualHitTestMetricsCount: UINT32): HResult; stdcall;

  end;
  IID_IDWriteTextLayout = IDWriteTextLayout;
  {$EXTERNALSYM IID_IDWriteTextLayout}



  // Interface IDWriteBitmapRenderTarget
  // ===================================
  // Encapsulates a 32-bit device independent bitmap and device context, which can be used for rendering glyphs.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteBitmapRenderTarget);'}
  {$EXTERNALSYM IDWriteBitmapRenderTarget}
  IDWriteBitmapRenderTarget = interface(IUnknown)
  ['{5e5a32a3-8dff-4773-9ff6-0696eab77267}']

    // Draws a run of glyphs to the bitmap.
    // <param name="baselineOriginX">Horizontal position of the baseline origin, in DIPs, relative to the upper-left corner of the DIB.</param>
    // <param name="baselineOriginY">Vertical position of the baseline origin, in DIPs, relative to the upper-left corner of the DIB.</param>
    // <param name="measuringMode">Specifies measuring mode for glyphs in the run.
    // Renderer implementations may choose different rendering modes for different measuring modes, for example
    // DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL for DWRITE_MEASURING_MODE_NATURAL,
    // DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC for DWRITE_MEASURING_MODE_GDI_CLASSIC, and
    // DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL for DWRITE_MEASURING_MODE_GDI_NATURAL.
    // </param>
    // <param name="glyphRun">Structure containing the properties of the glyph run.</param>
    // <param name="renderingParams">Object that controls rendering behavior.</param>
    // <param name="textColor">Specifies the foreground color of the text.</param>
    // <param name="blackBoxRect">Optional rectangle that receives the bounding box (in pixels not DIPs) of all the pixels affected by
    // drawing the glyph run. The black box rectangle may extend beyond the dimensions of the bitmap.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function DrawGlyphRun(baselineOriginX: Single;
                          baselineOriginY: Single;
                          measuringMode: DWRITE_MEASURING_MODE;
                          glyphRun: DWRITE_GLYPH_RUN;
                          renderingParams: IDWriteRenderingParams;
                          textColor: COLORREF;
                          blackBoxRect: PRECT = Nil): HResult; stdcall;

    // Gets a handle to the memory device context.
    // <returns>
    // Returns the device context handle.
    // </returns>
    // <remarks>
    // An application can use the device context to draw using GDI functions. An application can obtain the bitmap handle
    // (HBITMAP) by calling GetCurrentObject. An application that wants information about the underlying bitmap, including
    // a pointer to the pixel data, can call GetObject to fill in a DIBSECTION structure. The bitmap is always a 32-bit
    // top-down DIB.
    // </remarks>
    function GetMemoryDC(): HDC; stdcall;

    // Gets the number of bitmap pixels per DIP. A DIP (device-independent pixel) is 1/96 inch so this value is the number
    // if pixels per inch divided by 96.
    // <returns>
    // Returns the number of bitmap pixels per DIP.
    // </returns>
    function GetPixelsPerDip(): Single; stdcall;

    // Sets the number of bitmap pixels per DIP. A DIP (device-independent pixel) is 1/96 inch so this value is the number
    // if pixels per inch divided by 96.
    // <param name="pixelsPerDip">Specifies the number of pixels per DIP.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetPixelsPerDip(pixelsPerDip: Single): HResult; stdcall;

    // Gets the transform that maps abstract coordinate to DIPs. By default this is the identity
    // transform. Note that this is unrelated to the world transform of the underlying device
    // context.
    // <param name="transform">Receives the transform.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetCurrentTransform(out transform: DWRITE_MATRIX): HResult; stdcall;

    // Sets the transform that maps abstract coordinate to DIPs. This does not affect the world
    // transform of the underlying device context.
    // <param name="transform">Specifies the new transform. This parameter can be NULL, in which
    // case the identity transform is implied.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function SetCurrentTransform(transform: PDWRITE_MATRIX): HResult; stdcall;

    // Gets the dimensions of the bitmap.
    // <param name="size">Receives the size of the bitmap in pixels.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetSize(out _size: SIZE): HResult; stdcall;

    // Resizes the bitmap.
    // <param name="width">New bitmap width, in pixels.</param>
    // <param name="height">New bitmap height, in pixels.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function Resize(width: UINT32;
                    height: UINT32): HResult; stdcall;

  end;
  IID_IDWriteBitmapRenderTarget = IDWriteBitmapRenderTarget;
  {$EXTERNALSYM IID_IDWriteBitmapRenderTarget}


  // Interface IDWriteGdiInterop
  // ===========================
  // The GDI interop interface provides interoperability with GDI.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteGdiInterop);'}
  {$EXTERNALSYM IDWriteGdiInterop}
  IDWriteGdiInterop = interface(IUnknown)
  ['{1edd9491-9853-4299-898f-6432983b6f3a}']
    // Creates a font object that matches the properties specified by the LOGFONT structure
    // in the system font collection (GetSystemFontCollection).
    // <param name="logFont">Structure containing a GDI-compatible font description.</param>
    // <param name="font">Receives a newly created font object if successful, or NULL in case of error.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateFontFromLOGFONT(logFont: LOGFONTW;
                                   out font: IDWriteFont): HResult; stdcall;

    // Initializes a LOGFONT structure based on the GDI-compatible properties of the specified font.
    // <param name="font">Specifies a font.</param>
    // <param name="logFont">Structure that receives a GDI-compatible font description.</param>
    // <param name="isSystemFont">Contains TRUE if the specified font object is part of the system font collection
    // or FALSE otherwise.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function ConvertFontToLOGFONT(font: IDWriteFont;
                                  out logFont: LOGFONTW;
                                  out isSystemFont: BOOL): HResult; stdcall;

    // Initializes a LOGFONT structure based on the GDI-compatible properties of the specified font.
    // <param name="font">Specifies a font face.</param>
    // <param name="logFont">Structure that receives a GDI-compatible font description.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function ConvertFontFaceToLOGFONT(font: IDWriteFontFace;
                                      out logFont: LOGFONTW): HResult; stdcall;

    // Creates a font face object that corresponds to the currently selected HFONT.
    // <param name="hdc">Handle to a device context into which a font has been selected. It is assumed that the client
    // has already performed font mapping and that the font selected into the DC is the actual font that would be used
    // for rendering glyphs.</param>
    // <param name="fontFace">Contains the newly created font face object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateFontFaceFromHdc(hdc: HDC;
                                   out fontFace: IDWriteFontFace): HResult; stdcall;

    // Creates an object that encapsulates a bitmap and memory DC which can be used for rendering glyphs.
    // <param name="hdc">Optional device context used to create a compatible memory DC.</param>
    // <param name="width">Width of the bitmap.</param>
    // <param name="height">Height of the bitmap.</param>
    // <param name="renderTarget">Receives a pointer to the newly created render target.</param>
    function CreateBitmapRenderTarget(hdc: HDC;
                                      width: UINT32;
                                      height: UINT32;
                                      out renderTarget: IDWriteBitmapRenderTarget): HResult; stdcall;

  end;
  IID_IDWriteGdiInterop = IDWriteGdiInterop;
  {$EXTERNALSYM IID_IDWriteGdiInterop}


  // Interface IDWriteGlyphRunAnalysis
  // =================================
  // Interface that encapsulates information used to render a glyph run.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteGlyphRunAnalysis);'}
  {$EXTERNALSYM IDWriteGlyphRunAnalysis}
  IDWriteGlyphRunAnalysis = interface(IUnknown)
  ['{7d97dbf7-e085-42d4-81e3-6a883bded118}']

    // Gets the bounding rectangle of the physical pixels affected by the glyph run.
    // <param name="textureType">Specifies the type of texture requested. If a bi-level texture is requested, the
    // bounding rectangle includes only bi-level glyphs. Otherwise, the bounding rectangle includes only anti-aliased
    // glyphs.</param>
    // <param name="textureBounds">Receives the bounding rectangle, or an empty rectangle if there are no glyphs
    // if the specified type.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetAlphaTextureBounds(textureType: DWRITE_TEXTURE_TYPE;
                                   out textureBounds: TRECT): HResult; stdcall;

    // Creates an alpha texture of the specified type.
    // <param name="textureType">Specifies the type of texture requested. If a bi-level texture is requested, the
    // texture contains only bi-level glyphs. Otherwise, the texture contains only anti-aliased glyphs.</param>
    // <param name="textureBounds">Specifies the bounding rectangle of the texture, which can be different than
    // the bounding rectangle returned by GetAlphaTextureBounds.</param>
    // <param name="alphaValues">Receives the array of alpha values.</param>
    // <param name="bufferSize">Size of the alphaValues array. The minimum size depends on the dimensions of the
    // rectangle and the type of texture requested.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateAlphaTexture(textureType: DWRITE_TEXTURE_TYPE;
                                textureBounds: TRECT;
                                out alphaValues: PByte;
                                bufferSize: UINT32): HResult; stdcall;

    // Gets properties required for ClearType blending.
    // <param name="renderingParams">Rendering parameters object. In most cases, the values returned in the output
    // parameters are based on the properties of this object. The exception is if a GDI-compatible rendering mode
    // is specified.</param>
    // <param name="blendGamma">Receives the gamma value to use for gamma correction.</param>
    // <param name="blendEnhancedContrast">Receives the enhanced contrast value.</param>
    // <param name="blendClearTypeLevel">Receives the ClearType level.</param>
    function GetAlphaBlendParams(renderingParams: IDWriteRenderingParams;
                                 out blendGamma: Single;
                                 out blendEnhancedContrast: Single;
                                 out blendClearTypeLevel: Single): HResult; stdcall;

  end;
  IID_IDWriteGlyphRunAnalysis = IDWriteGlyphRunAnalysis;
  {$EXTERNALSYM IID_IDWriteGlyphRunAnalysis}



  // Interface IDWriteFactory
  // ========================
  // The root factory interface for all DWrite objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory);'}
  {$EXTERNALSYM IDWriteFactory}
  IDWriteFactory = interface(IUnknown)
  ['{b859ee5a-d838-4b5b-a2e8-1adc7d93db48}']

    // Gets a font collection representing the set of installed fonts.
    // <param name="fontCollection">Receives a pointer to the system font collection object, or NULL in case of failure.</param>
    // <param name="checkForUpdates">If this parameter is nonzero, the function performs an immediate check for changes to the set of
    // installed fonts. If this parameter is FALSE, the function will still detect changes if the font cache service is running, but
    // there may be some latency. For example, an application might specify TRUE if it has itself just installed a font and wants to
    // be sure the font collection contains that font.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetSystemFontCollection(out fontCollection: IDWriteFontCollection;
                                     checkForUpdates: BOOL = False): HResult; stdcall;

    // Creates a font collection using a custom font collection loader.
    // <param name="collectionLoader">Application-defined font collection loader, which must have been previously
    // registered using RegisterFontCollectionLoader.</param>
    // <param name="collectionKey">Key used by the loader to identify a collection of font files.</param>
    // <param name="collectionKeySize">Size in bytes of the collection key.</param>
    // <param name="fontCollection">Receives a pointer to the system font collection object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateCustomFontCollection(collectionLoader: IDWriteFontCollectionLoader;
                                        collectionKey: Pointer;
                                        collectionKeySize: UINT32;
                                        out fontCollection: IDWriteFontCollection): HResult; stdcall;

    // Registers a custom font collection loader with the factory object.
    // <param name="fontCollectionLoader">Application-defined font collection loader.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function RegisterFontCollectionLoader(fontCollectionLoader: IDWriteFontCollectionLoader): HResult; stdcall;

    // Unregisters a custom font collection loader that was previously registered using RegisterFontCollectionLoader.
    // <param name="fontCollectionLoader">Application-defined font collection loader.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function UnregisterFontCollectionLoader(fontCollectionLoader: IDWriteFontCollectionLoader): HResult; stdcall;

    // CreateFontFileReference creates a font file reference object from a local font file.
    // <param name="filePath">Absolute file path. Subsequent operations on the constructed object may fail
    // if the user provided filePath doesn't correspond to a valid file on the disk.</param>
    // <param name="lastWriteTime">Last modified time of the input file path. If the parameter is omitted,
    // the function will access the font file to obtain its last write time, so the clients are encouraged to specify this value
    // to avoid extra disk access. Subsequent operations on the constructed object may fail
    // if the user provided lastWriteTime doesn't match the file on the disk.</param>
    // <param name="fontFile">Contains newly created font file reference object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateFontFileReference(filePath: PWideChar;
                                     lastWriteTime: PFILETIME;
                                     out fontFile: IDWriteFontFile): HResult; stdcall;

    // CreateCustomFontFileReference creates a reference to an application specific font file resource.
    // This function enables an application or a document to use a font without having to install it on the system.
    // The fontFileReferenceKey has to be unique only in the scope of the fontFileLoader used in this call.
    // <param name="fontFileReferenceKey">Font file reference key that uniquely identifies the font file resource
    // during the lifetime of fontFileLoader.</param>
    // <param name="fontFileReferenceKeySize">Size of font file reference key in bytes.</param>
    // <param name="fontFileLoader">Font file loader that will be used by the font system to load data from the file identified by
    // fontFileReferenceKey.</param>
    // <param name="fontFile">Contains the newly created font file object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // This function is provided for cases when an application or a document needs to use a font
    // without having to install it on the system. fontFileReferenceKey has to be unique only in the scope
    // of the fontFileLoader used in this call.
    // </remarks>
    function CreateCustomFontFileReference(fontFileReferenceKey: Pointer;
                                           fontFileReferenceKeySize: UINT32;
                                           fontFileLoader: IDWriteFontFileLoader;
                                           out fontFile: IDWriteFontFile): HResult; stdcall;

    // Creates a font face object.
    // <param name="fontFaceType">The file format of the font face.</param>
    // <param name="numberOfFiles">The number of font files required to represent the font face.</param>
    // <param name="fontFiles">Font files representing the font face. Since IDWriteFontFace maintains its own references
    // to the input font file objects, it's OK to release them after this call.</param>
    // <param name="faceIndex">The zero based index of a font face in cases when the font files contain a collection of font faces.
    // If the font files contain a single face, this value should be zero.</param>
    // <param name="fontFaceSimulationFlags">Font face simulation flags for algorithmic emboldening and italicization.</param>
    // <param name="fontFace">Contains the newly created font face object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateFontFace(fontFaceType: DWRITE_FONT_FACE_TYPE;
                            numberOfFiles: UINT32;
                            fontFiles: PIDWriteFontFile; // pointer to array of fontFiles
                            faceIndex: UINT32;
                            fontFaceSimulationFlags: DWRITE_FONT_SIMULATIONS;
                            out fontFace: IDWriteFontFace): HResult; stdcall;

    // Creates a rendering parameters object with default settings for the primary monitor.
    // <param name="renderingParams">Holds the newly created rendering parameters object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateRenderingParams(out renderingParams: IDWriteRenderingParams): HResult; stdcall;

    // Creates a rendering parameters object with default settings for the specified monitor.
    // <param name="monitor">The monitor to read the default values from.</param>
    // <param name="renderingParams">Holds the newly created rendering parameters object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateMonitorRenderingParams(monitor: HMONITOR;
                                          out renderingParams: IDWriteRenderingParams): HResult; stdcall;

    // Creates a rendering parameters object with the specified properties.
    // <param name="gamma">The gamma value used for gamma correction, which must be greater than zero and cannot exceed 256.</param>
    // <param name="enhancedContrast">The amount of contrast enhancement, zero or greater.</param>
    // <param name="clearTypeLevel">The degree of ClearType level, from 0.0f (no ClearType) to 1.0f (full ClearType).</param>
    // <param name="pixelGeometry">The geometry of a device pixel.</param>
    // <param name="renderingMode">Method of rendering glyphs. In most cases, this should be DWRITE_RENDERING_MODE_DEFAULT to automatically use an appropriate mode.</param>
    // <param name="renderingParams">Holds the newly created rendering parameters object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateCustomRenderingParams(gamma: Single;
                                         enhancedContrast: Single;
                                         clearTypeLevel: Single;
                                         pixelGeometry: DWRITE_PIXEL_GEOMETRY;
                                         renderingMode: DWRITE_RENDERING_MODE;
                                         out renderingParams: IDWriteRenderingParams): HResult; stdcall;

    // Registers a font file loader with DirectWrite.
    // <param name="fontFileLoader">Pointer to the implementation of the IDWriteFontFileLoader for a particular file resource type.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // This function registers a font file loader with DirectWrite.
    // Font file loader interface handles loading font file resources of a particular type from a key.
    // The font file loader interface is recommended to be implemented by a singleton object.
    // A given instance can only be registered once.
    // Succeeding attempts will return an error that it has already been registered.
    // IMPORTANT: font file loader implementations must not register themselves with DirectWrite
    // inside their constructors and must not unregister themselves in their destructors, because
    // registration and unregistration operations increment and decrement the object reference count respectively.
    // Instead, registration and unregistration of font file loaders with DirectWrite should be performed
    // outside of the font file loader implementation as a separate step.
    // </remarks>
    function RegisterFontFileLoader(fontFileLoader: IDWriteFontFileLoader): HResult; stdcall;

    // Unregisters a font file loader that was previously registered with the DirectWrite font system using RegisterFontFileLoader.
    // <param name="fontFileLoader">Pointer to the file loader that was previously registered with the DirectWrite font system using RegisterFontFileLoader.</param>
    // <returns>
    // This function will succeed if the user loader is requested to be removed.
    // It will fail if the pointer to the file loader identifies a standard DirectWrite loader,
    // or a loader that is never registered or has already been unregistered.
    // </returns>
    // <remarks>
    // This function unregisters font file loader callbacks with the DirectWrite font system.
    // The font file loader interface is recommended to be implemented by a singleton object.
    // IMPORTANT: font file loader implementations must not register themselves with DirectWrite
    // inside their constructors and must not unregister themselves in their destructors, because
    // registration and unregistration operations increment and decrement the object reference count respectively.
    // Instead, registration and unregistration of font file loaders with DirectWrite should be performed
    // outside of the font file loader implementation as a separate step.
    // </remarks>
    function UnregisterFontFileLoader(fontFileLoader: IDWriteFontFileLoader): HResult; stdcall;

    // Create a text format object used for text layout.
    // <param name="fontFamilyName">Name of the font family</param>
    // <param name="fontCollection">Font collection. NULL indicates the system font collection.</param>
    // <param name="fontWeight">Font weight</param>
    // <param name="fontStyle">Font style</param>
    // <param name="fontStretch">Font stretch</param>
    // <param name="fontSize">Logical size of the font in DIP units. A DIP ("device-independent pixel") equals 1/96 inch.</param>
    // <param name="localeName">Locale name</param>
    // <param name="textFormat">Contains newly created text format object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    // <remarks>
    // If fontCollection is nullptr, the system font collection is used, grouped by typographic family name
    // (DWRITE_FONT_FAMILY_MODEL_WEIGHT_STRETCH_STYLE) without downloadable fonts.
    // </remarks>
    function CreateTextFormat(fontFamilyName: PWideChar;
                              fontCollection: IDWriteFontCollection;
                              fontWeight: DWRITE_FONT_WEIGHT;
                              fontStyle: DWRITE_FONT_STYLE;
                              fontStretch: DWRITE_FONT_STRETCH;
                              fontSize: Single;
                              localeName: PWideChar;
                              out textFormat: IDWriteTextFormat): HResult; stdcall;

    // Create a typography object used in conjunction with text format for text layout.
    // <param name="typography">Contains newly created typography object, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateTypography(out typography: IDWriteTypography): HResult; stdcall;

    // Create an object used for interoperability with GDI.
    // <param name="gdiInterop">Receives the GDI interop object if successful, or NULL in case of failure.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function GetGdiInterop(out gdiInterop: IDWriteGdiInterop): HResult; stdcall;

    // CreateTextLayout takes a string, format, and associated constraints
    // and produces an object representing the fully analyzed
    // and formatted result.
    // <param name="string">The string to layout.</param>
    // <param name="stringLength">The length of the string.</param>
    // <param name="textFormat">The format to apply to the string.</param>
    // <param name="maxWidth">Width of the layout box.</param>
    // <param name="maxHeight">Height of the layout box.</param>
    // <param name="textLayout">The resultant object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateTextLayout(_string: PWideChar;
                              stringLength: UINT32;
                              textFormat: IDWriteTextFormat;
                              maxWidth: Single;
                              maxHeight: Single;
                              out textLayout: IDWriteTextLayout): HResult; stdcall;

    // CreateGdiCompatibleTextLayout takes a string, format, and associated constraints
    // and produces and object representing the result formatted for a particular display resolution
    // and measuring mode. The resulting text layout should only be used for the intended resolution,
    // and for cases where text scalability is desired, CreateTextLayout should be used instead.
    // <param name="string">The string to layout.</param>
    // <param name="stringLength">The length of the string.</param>
    // <param name="textFormat">The format to apply to the string.</param>
    // <param name="layoutWidth">Width of the layout box.</param>
    // <param name="layoutHeight">Height of the layout box.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For example, if rendering onto a 96 DPI device then pixelsPerDip
    // is 1. If rendering onto a 120 DPI device then pixelsPerDip is 120/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified the font size and pixelsPerDip.</param>
    // <param name="useGdiNatural">
    // When set to FALSE, instructs the text layout to use the same metrics as GDI aliased text.
    // When set to TRUE, instructs the text layout to use the same metrics as text measured by GDI using a font
    // created with CLEARTYPE_NATURAL_QUALITY.
    // </param>
    // <param name="textLayout">The resultant object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateGdiCompatibleTextLayout(_string: PWideChar;
                                           stringLength: UINT32;
                                           textFormat: IDWriteTextFormat;
                                           layoutWidth: Single;
                                           layoutHeight: Single;
                                           pixelsPerDip: Single;
                                           transform: PDWRITE_MATRIX;
                                           useGdiNatural: BOOL;
                                           out textLayout: IDWriteTextLayout): HResult; stdcall;

    // The application may call this function to create an inline object for trimming, using an ellipsis as the omission sign.
    // The ellipsis will be created using the current settings of the format, including base font, style, and any effects.
    // Alternate omission signs can be created by the application by implementing IDWriteInlineObject.
    // <param name="textFormat">Text format used as a template for the omission sign.</param>
    // <param name="trimmingSign">Created omission sign.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateEllipsisTrimmingSign(textFormat: IDWriteTextFormat;
                                        out trimmingSign: IDWriteInlineObject): HResult; stdcall;

    // Return an interface to perform text analysis with.
    // <param name="textAnalyzer">The resultant object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateTextAnalyzer(out textAnalyzer: IDWriteTextAnalyzer): HResult; stdcall;

    // Creates a number substitution object using a locale name,
    // substitution method, and whether to ignore user overrides (uses NLS
    // defaults for the given culture instead).
    // <param name="substitutionMethod">Method of number substitution to use.</param>
    // <param name="localeName">Which locale to obtain the digits from.</param>
    // <param name="ignoreUserOverride">Ignore the user's settings and use the locale defaults</param>
    // <param name="numberSubstitution">Receives a pointer to the newly created object.</param>
    function CreateNumberSubstitution(substitutionMethod: DWRITE_NUMBER_SUBSTITUTION_METHOD;
                                      localeName: PWideChar;
                                      ignoreUserOverride: BOOL;
                                      out numberSubstitution: IDWriteNumberSubstitution): HResult; stdcall;

    // Creates a glyph run analysis object, which encapsulates information
    // used to render a glyph run.
    // <param name="glyphRun">Structure specifying the properties of the glyph run.</param>
    // <param name="pixelsPerDip">Number of physical pixels per DIP. For example, if rendering onto a 96 DPI bitmap then pixelsPerDip
    // is 1. If rendering onto a 120 DPI bitmap then pixelsPerDip is 120/96.</param>
    // <param name="transform">Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified by the emSize and pixelsPerDip.</param>
    // <param name="renderingMode">Specifies the rendering mode, which must be one of the raster rendering modes (i.e., not default
    // and not outline).</param>
    // <param name="measuringMode">Specifies the method to measure glyphs.</param>
    // <param name="baselineOriginX">Horizontal position of the baseline origin, in DIPs.</param>
    // <param name="baselineOriginY">Vertical position of the baseline origin, in DIPs.</param>
    // <param name="glyphRunAnalysis">Receives a pointer to the newly created object.</param>
    // <returns>
    // Standard HRESULT error code.
    // </returns>
    function CreateGlyphRunAnalysis(glyphRun: DWRITE_GLYPH_RUN;
                                    pixelsPerDip: Single;
                                    transform: PDWRITE_MATRIX;
                                    renderingMode: DWRITE_RENDERING_MODE;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    baselineOriginX: Single;
                                    baselineOriginY: Single;
                                    out glyphRunAnalysis: IDWriteGlyphRunAnalysis): HResult; stdcall;
  end; // interface IDWriteFactory
  IID_IDWriteFactory = IDWriteFactory;
  {$EXTERNALSYM IID_IDWriteFactory}



  // Creates a DirectWrite factory object that is used for subsequent creation of individual DirectWrite objects.
  // <param name="factoryType">Identifies whether the factory object will be shared or isolated.</param>
  // <param name="iid">Identifies the DirectWrite factory interface, such as __uuidof(IDWriteFactory).</param>
  // <param name="factory">Receives the DirectWrite factory object.</param>
  // <returns>
  // Standard HRESULT error code.
  // </returns>
  // <remarks>
  // Obtains DirectWrite factory object that is used for subsequent creation of individual DirectWrite classes.
  // DirectWrite factory contains internal state such as font loader registration and cached font data.
  // In most cases it is recommended to use the shared factory object, because it allows multiple components
  // that use DirectWrite to share internal DirectWrite state and reduce memory usage.
  // However, there are cases when it is desirable to reduce the impact of a component,
  // such as a plug-in from an untrusted source, on the rest of the process by sandboxing and isolating it
  // from the rest of the process components. In such cases, it is recommended to use an isolated factory for the sandboxed
  // component.
  // </remarks>
  function DWriteCreateFactory(factoryType: DWRITE_FACTORY_TYPE;
                               const iid: TGuid;
                               out factory: IUnknown): HResult; stdcall;
  {$EXTERNALSYM DWriteCreateFactory}

const

  // Macros used to define DirectWrite error codes.
  FACILITY_DWRITE                     = $898;
  {$EXTERNALSYM FACILITY_DWRITE}
  DWRITE_ERR_BASE                     = $5000;
  {$EXTERNALSYM DWRITE_ERR_BASE}



  // Creates an OpenType tag as a 32bit integer such that
  // the first character in the tag is the lowest byte,
  // (least significant on little endian architectures)
  // which can be used to compare with tags in the font file.
  // This macro is compatible with DWRITE_FONT_FEATURE_TAG.
  //
  // Example: DWRITE_MAKE_OPENTYPE_TAG('c','c','m','p')
  // Dword:   $706D6363
  function DWRITE_MAKE_OPENTYPE_TAG(a: AnsiChar;
                                    b: AnsiChar;
                                    c: AnsiChar;
                                    d: AnsiChar): DWORD;
  {$EXTERNALSYM DWRITE_MAKE_OPENTYPE_TAG}

  // Creates an OpenType tag for glyph positioning and substitution font features.
  function DWRITE_MAKE_FONT_FEATURE_TAG(a: AnsiChar;
                                        b: AnsiChar;
                                        c: AnsiChar;
                                        d: AnsiChar): DWORD;
  {$EXTERNALSYM DWRITE_MAKE_FONT_FEATURE_TAG}


  function MAKE_DWRITE_HR(severity: DWord;
                          code: DWord): HResult;
  {$EXTERNALSYM MAKE_DWRITE_HR}

  function MAKE_DWRITE_HR_ERR(code: DWord): HResult;
  {$EXTERNALSYM MAKE_DWRITE_HR_ERR}


  // DWrite errors have moved to WinError.h/.pas


  // Additional Prototypes for ALL interfaces

  // Bitfield manipulation functions
  // Source: Embarcadero

  // DWORD
  // Getter
  function GetDWordBits(const data: DWORD;
                        const index: Integer): Integer; inline;


  // Setter
  procedure SetDWordBits(var data: DWORD;
                         const index: Integer;
                         const value: Integer); inline;

  // WORD
  // Getter
  function GetWordBits(const data: WORD;
                       const index: Integer): WORD; inline;


  // Setter
  procedure SetWordBits(var data: WORD;
                        const index: Integer;
                        const value: Integer); inline;


  // BYTE
  // Getter
  function GetByteBits(const data: Byte;
                       const index: Integer): Byte; inline;


  // Setter
  procedure SetByteBits(var data: Byte;
                        const index: Integer;
                        const value: Integer); inline;

  // End of Additional Prototypes


implementation

const
  DWriteLib = 'Dwrite.dll';
  {$EXTERNALSYM DWriteLib}


function DWRITE_MAKE_OPENTYPE_TAG(a: AnsiChar;
                                  b: AnsiChar;
                                  c: AnsiChar;
                                  d: AnsiChar): DWORD;
begin
  Result := DWORD((Ord(d) shl 24) or (Ord(c) shl 16) or (Ord(b) shl 8) or Ord(a));
end;


function DWRITE_MAKE_FONT_FEATURE_TAG(a: AnsiChar;
                                      b: AnsiChar;
                                      c: AnsiChar;
                                      d: AnsiChar): DWORD;
begin
  Result := DWRITE_MAKE_OPENTYPE_TAG(a, b, c, d);
end;



function MAKE_DWRITE_HR(severity: DWord; code: DWord): HResult;
begin
  Result := MAKE_HRESULT(severity,
                         FACILITY_DWRITE,
                         (DWRITE_ERR_BASE + code));
end;


function MAKE_DWRITE_HR_ERR(code: DWord): HResult;
begin
  Result := MAKE_DWRITE_HR(SEVERITY_ERROR,
                           code);
end;


{$WARN SYMBOL_PLATFORM OFF}
  function DWriteCreateFactory;      external DWriteLib name 'DWriteCreateFactory' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}



// Bit shifting

// function DWRITE_SHAPING_TEXT_PROPERTIES.GetUINT16
function DWRITE_SHAPING_TEXT_PROPERTIES.GetUINT16(const index: Integer): Word;
begin
  Result := GetWordBits(data,
                        index);
end;

procedure DWRITE_SHAPING_TEXT_PROPERTIES.SetUINT16(const index: Integer;
                                                   const value: Word);
begin
  SetWordBits(data,
              index,
              value);
end;


// DWRITE_SHAPING_GLYPH_PROPERTIES
function DWRITE_SHAPING_GLYPH_PROPERTIES.GetUINT16(const index: Integer): UINT16;
begin
  Result := GetWordBits(data,
                        index);
end;


procedure DWRITE_SHAPING_GLYPH_PROPERTIES.SetUINT16(const index: Integer;
                                                    const value: UINT16);
begin
  SetWordBits(data,
              index,
              value);
end;



// DWRITE_LINE_BREAKPOINT
function DWRITE_LINE_BREAKPOINT.GetUINT8(const index: Integer): UINT8;
begin
  Result := GetByteBits(data,
                        index);
end;

procedure DWRITE_LINE_BREAKPOINT.SetUINT8(const index: Integer;
                                          const value: UINT8);
begin
  SetByteBits(data,
              index,
              value);
end;


// DWRITE_CLUSTER_METRICS
function DWRITE_CLUSTER_METRICS.GetUINT16(const index: integer): Word;
begin
  Result := GetWordBits(data,
                        index);
end;

procedure DWRITE_CLUSTER_METRICS.SetUINT16(const index: Integer;
                                           const value: Word);
begin
  SetWordBits(data,
              index,
              value);
end;


  // Implement Additional Prototypes here.

// Bitshifting helpers
// Source: Patrick van Logchem, Embarcadero

// DWORD
// Getter
function GetDWordBits(const data: DWORD;
                      const index: Integer): Integer;
begin
  Result := (data shr (index shr 8)) AND // offset
            ((1 shl Byte(index)) - 1);   // mask
end;

// Setter
procedure SetDWordBits(var data: DWORD;
                       const index: Integer;
                       const value: Integer);
var
  offset: Byte;
  mask: Integer;
  val: Integer;

begin
  val := value;
  Mask := ((1 shl Byte(index)) - 1);

  //Assert(value <= Mask);
  if val > mask then
    val := mask;

  offset := index shr 8;
  data := (data AND (not (mask shl offset)))
          OR DWORD(val shl offset);
end;

// WORD
// Getter
function GetWordBits(const data: WORD;
                     const index: Integer): WORD;
begin
  Result := (data shr (index shr 8)) AND // offset
            ((1 shl Byte(index)) - 1);   // mask
end;

// Setter
procedure SetWordBits(var data: WORD;
                      const index: Integer;
                      const value: Integer);
var
  offset: Byte;
  mask: Integer;
  val: Integer;

begin
  val := value;
  Mask := ((1 shl Byte(index)) - 1);

  //Assert(value <= Mask);
  if val > mask then
    val := mask;

  offset := index shr 8;
  data := (data AND (not (mask shl offset)))
          OR WORD(val shl offset);
end;



// BYTE
// Getter
function GetByteBits(const data: Byte;
                     const index: Integer): Byte;
begin
  //Result := ((data shr (index shr 8)) AND // offset
  //           (index AND $FF));            // mask

  Result := (data shr (index shr 8)) AND
            ((1 shl Byte(index)) - 1);
end;

// Setter
procedure SetByteBits(var data: Byte;
                      const index: Integer;
                      const value: Integer);
begin
  data := (data AND (Not ((index AND $FF) shl (index shr 8)))) or
          ((value AND index AND $FF) shl (index shr 8));
end;

end.

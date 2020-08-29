// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectWrite
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DWrite_3.pas
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
// Remarks: - Requires Windows 7 or later.
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
// Source: dwrite_3.h
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
unit WinApi.DirectX.DWrite_3;

  {$HPPEMIT '#include "dwrite_3.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DWrite,
  WinApi.DirectX.DWrite_1,
  WinApi.DirectX.DWrite_2;

  {$WEAKPACKAGEUNIT ON}

const
  // A font resource could not be accessed because it was remote. This can happen
  // when calling CreateFontFace on a non-local font or trying to measure/draw
  // glyphs that are not downloaded yet.
  DWRITE_E_REMOTEFONT                 = $8898500D;
  {$EXTERNALSYM DWRITE_E_REMOTEFONT}

  // The download was canceled, which happens if the application calls
  // IDWriteFontDownloadQueue.CancelDownload before they finish.
  DWRITE_E_DOWNLOADCANCELLED          = $8898500E;
  {$EXTERNALSYM DWRITE_E_DOWNLOADCANCELLED}

  // The download failed to complete because the remote resource is missing
  // or the network is down.
  DWRITE_E_DOWNLOADFAILED             = $8898500F;
  {$EXTERNALSYM DWRITE_E_DOWNLOADFAILED}

  // A download request was not added or a download failed because there
  // are too many active downloads.
  DWRITE_E_TOOMANYDOWNLOADS           = $88985010;
  {$EXTERNALSYM DWRITE_E_TOOMANYDOWNLOADS}


  // Four character identifier for a font axis.
  // remarks
  //   Use DWRITE_MAKE_FONT_AXIS_TAG() to create a custom one.
  DWRITE_FONT_AXIS_TAG_WEIGHT        = Ord('w') or (Ord('g') shl 8) or (Ord('h') shl 16) or (Ord('t') shl 24);
  {$EXTERNALSYM DWRITE_FONT_AXIS_TAG_WEIGHT}
  DWRITE_FONT_AXIS_TAG_WIDTH         = Ord('w') or (Ord('d') shl 8) or (Ord('t') shl 16) or (Ord('h') shl 24);
  {$EXTERNALSYM DWRITE_FONT_AXIS_TAG_WIDTH}
  DWRITE_FONT_AXIS_TAG_SLANT         = Ord('s') or (Ord('l') shl 8) or (Ord('n') shl 16) or (Ord('t') shl 24);
  {$EXTERNALSYM DWRITE_FONT_AXIS_TAG_SLANT}
  DWRITE_FONT_AXIS_TAG_OPTICAL_SIZE  = Ord('o') or (Ord('p') shl 8) or (Ord('s') shl 16) or (Ord('z') shl 24);
  {$EXTERNALSYM DWRITE_FONT_AXIS_TAG_OPTICAL_SIZE}
  DWRITE_FONT_AXIS_TAG_ITALIC        = Ord('i') or (Ord('t') shl 8) or (Ord('a') shl 16) or (Ord('l') shl 24);
  {$EXTERNALSYM DWRITE_FONT_AXIS_TAG_ITALIC}

// Enums =======================================================================

type
  // The font property enumeration identifies a string in a font.
  PDWRITE_FONT_PROPERTY_ID = ^DWRITE_FONT_PROPERTY_ID;
  DWRITE_FONT_PROPERTY_ID = DWord;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID}
const
  // Unspecified font property identifier.
  DWRITE_FONT_PROPERTY_ID_NONE = DWRITE_FONT_PROPERTY_ID(0);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_NONE}
  // Family name for the weight-stretch-style model.
  DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FAMILY_NAME = DWRITE_FONT_PROPERTY_ID(1);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FAMILY_NAME}
  // Family name preferred by the designer. This enables font designers to group more than four fonts in a single family without losing compatibility with
  // GDI. This name is typically only present if it differs from the GDI-compatible family name.
  DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FAMILY_NAME = DWRITE_FONT_PROPERTY_ID(2);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FAMILY_NAME}
  // Face name of the for the weight-stretch-style (e.g., Regular or Bold).
  DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FACE_NAME = DWRITE_FONT_PROPERTY_ID(3);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FACE_NAME}
  // The full name of the font, e.g. "Arial Bold", from name id 4 in the name table.
  DWRITE_FONT_PROPERTY_ID_FULL_NAME = DWRITE_FONT_PROPERTY_ID(4);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_FULL_NAME}
  // GDI-compatible family name. Because GDI allows a maximum of four fonts per family, fonts in the same family may have different GDI-compatible family names
  // (e.g., "Arial", "Arial Narrow", "Arial Black").
  DWRITE_FONT_PROPERTY_ID_WIN32_FAMILY_NAME = DWRITE_FONT_PROPERTY_ID(5);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_WIN32_FAMILY_NAME}
  // The postscript name of the font, e.g. "GillSans-Bold" from name id 6 in the name table.
  DWRITE_FONT_PROPERTY_ID_POSTSCRIPT_NAME = DWRITE_FONT_PROPERTY_ID(6);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_POSTSCRIPT_NAME}
  // Script/language tag to identify the scripts or languages that the font was
  // primarily designed to support.
  // <remarks>
  // The design script/language tag is meant to be understood from the perspective of
  // users. For example, a font is considered designed for English if it is considered
  // useful for English users. Note that this is different from what a font might be
  // capable of supporting. For example, the Meiryo font was primarily designed for
  // Japanese users. While it is capable of displaying English well, it was not
  // meant to be offered for the benefit of non-Japanese-speaking English users.
  //
  // As another example, a font designed for Chinese may be capable of displaying
  // Japanese text, but would likely look incorrect to Japanese users.
  //
  // The valid values for this property are "ScriptLangTag" values. These are adapted
  // from the IETF BCP 47 specification, "Tags for Identifying Languages" (see
  // http://tools.ietf.org/html/bcp47). In a BCP 47 language tag, a language subtag
  // element is mandatory and other subtags are optional. In a ScriptLangTag, a
  // script subtag is mandatory and other subtags are option. The following
  // augmented BNF syntax, adapted from BCP 47, is used:
  //
  //     ScriptLangTag = [language "-"]
  //                     script
  //                     ["-" region]
  //                     *("-" variant)
  //                     *("-" extension)
  //                     ["-" privateuse]
  //
  // The expansion of the elements and the intended semantics associated with each
  // are as defined in BCP 47. Script subtags are taken from ISO 15924. At present,
  // no extensions are defined, and any extension should be ignored. Private use
  // subtags are defined by private agreement between the source and recipient and
  // may be ignored.
  //
  // Subtags must be valid for use in BCP 47 and contained in the Language Subtag
  // Registry maintained by IANA. (See
  // http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
  // and section 3 of BCP 47 for details.
  //
  // Any ScriptLangTag value not conforming to these specifications is ignored.
  //
  // Examples:
  //   "Latn" denotes Latin script (and any language or writing system using Latin)
  //   "Cyrl" denotes Cyrillic script
  //   "sr-Cyrl" denotes Cyrillic script as used for writing the Serbian language;
  //       a font that has this property value may not be suitable for displaying
  //       text in Russian or other languages written using Cyrillic script
  //   "Jpan" denotes Japanese writing (Han + Hiragana + Katakana)
  //
  // When passing this property to GetPropertyValues, use the overload which does
  // not take a language parameter, since this property has no specific language.
  // </remarks>
  DWRITE_FONT_PROPERTY_ID_DESIGN_SCRIPT_LANGUAGE_TAG = DWRITE_FONT_PROPERTY_ID(7);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_DESIGN_SCRIPT_LANGUAGE_TAG}
  // Script/language tag to identify the scripts or languages that the font declares
  // it is able to support.
  DWRITE_FONT_PROPERTY_ID_SUPPORTED_SCRIPT_LANGUAGE_TAG = DWRITE_FONT_PROPERTY_ID(8);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_SUPPORTED_SCRIPT_LANGUAGE_TAG}
  // Semantic tag to describe the font (e.g. Fancy, Decorative, Handmade, Sans-serif, Swiss, Pixel, Futuristic).
  DWRITE_FONT_PROPERTY_ID_SEMANTIC_TAG = DWRITE_FONT_PROPERTY_ID(9);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_SEMANTIC_TAG}
  // Weight of the font represented as a decimal string in the range 1-999.
  // <remark>
  // This enum is discouraged for use with IDWriteFontSetBuilder2 in favor of the more generic font axis
  // DWRITE_FONT_AXIS_TAG_WEIGHT which supports higher precision and range.
  // </remark>
  DWRITE_FONT_PROPERTY_ID_WEIGHT = DWRITE_FONT_PROPERTY_ID(10);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_WEIGHT}
  // Stretch of the font represented as a decimal string in the range 1-9.
  // <remark>
  // This enum is discouraged for use with IDWriteFontSetBuilder2 in favor of the more generic font axis
  // DWRITE_FONT_AXIS_TAG_WIDTH which supports higher precision and range.
  // </remark>
  DWRITE_FONT_PROPERTY_ID_STRETCH = DWRITE_FONT_PROPERTY_ID(11);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_STRETCH}
  // Style of the font represented as a decimal string in the range 0-2.
  // <remark>
  // This enum is discouraged for use with IDWriteFontSetBuilder2 in favor of the more generic font axes
  // DWRITE_FONT_AXIS_TAG_SLANT and DWRITE_FONT_AXIS_TAG_ITAL.
  // </remark>
  DWRITE_FONT_PROPERTY_ID_STYLE = DWRITE_FONT_PROPERTY_ID(12);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_STYLE}
  // Face name preferred by the designer. This enables font designers to group more than four fonts in a single
  // family without losing compatibility with GDI.
  DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FACE_NAME = DWRITE_FONT_PROPERTY_ID(13);
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FACE_NAME}
  // Total number of properties for NTDDI_WIN10 (IDWriteFontSet).
  // <remarks>
  // DWRITE_FONT_PROPERTY_ID_TOTAL cannot be used as a property ID.
  // </remarks>
  DWRITE_FONT_PROPERTY_ID_TOTAL = DWRITE_FONT_PROPERTY_ID_STYLE + 1;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_TOTAL}

  // Total number of properties for NTDDI_WIN10_RS3 (IDWriteFontSet1).
  DWRITE_FONT_PROPERTY_ID_TOTAL_RS3 = DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FACE_NAME + 1;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_TOTAL_RS3}
  // Obsolete aliases kept to avoid breaking existing code.
  DWRITE_FONT_PROPERTY_ID_PREFERRED_FAMILY_NAME = DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FAMILY_NAME;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_PREFERRED_FAMILY_NAME}
  DWRITE_FONT_PROPERTY_ID_FAMILY_NAME = DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FAMILY_NAME;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_FAMILY_NAME}
  DWRITE_FONT_PROPERTY_ID_FACE_NAME = DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FACE_NAME;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY_ID_FACE_NAME}

type
  // Specifies the locality of a resource.
  PDWRITE_LOCALITY = ^DWRITE_LOCALITY;
  DWRITE_LOCALITY = DWord;
  {$EXTERNALSYM DWRITE_LOCALITY}
const
  // The resource is remote, and information is unknown yet, including the file size and date.
  // Attempting to create a font or file stream will fail until locality becomes at least partial.
  DWRITE_LOCALITY_REMOTE = DWRITE_LOCALITY(0);
  {$EXTERNALSYM DWRITE_LOCALITY_REMOTE}
  // The resource is partially local, meaning you can query the size and date of the file
  // stream, and you may be able to create a font face and retrieve the particular glyphs
  // for metrics and drawing, but not all the glyphs will be present.
  DWRITE_LOCALITY_PARTIAL = DWRITE_LOCALITY(1);
  {$EXTERNALSYM DWRITE_LOCALITY_PARTIAL}
  // The resource is completely local, and all font functions can be called
  // without concern of missing data or errors related to network connectivity.
  DWRITE_LOCALITY_LOCAL = DWRITE_LOCALITY(2);
  {$EXTERNALSYM DWRITE_LOCALITY_LOCAL}

type
  // Represents a method of rendering glyphs.
  PDWRITE_RENDERING_MODE1 = ^DWRITE_RENDERING_MODE1;
  DWRITE_RENDERING_MODE1 = DWord;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1}
const
  // Specifies that the rendering mode is determined automatically based on the font and size.
  DWRITE_RENDERING_MODE1_DEFAULT = DWRITE_RENDERING_MODE_DEFAULT;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_DEFAULT}
  // Specifies that no antialiasing is performed. Each pixel is either set to the foreground
  // color of the text or retains the color of the background.
  DWRITE_RENDERING_MODE1_ALIASED = DWRITE_RENDERING_MODE_ALIASED;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_ALIASED}
  // Specifies that antialiasing is performed in the horizontal direction and the appearance
  // of glyphs is layout-compatible with GDI using CLEARTYPE_QUALITY. Use DWRITE_MEASURING_MODE_GDI_CLASSIC
  // to get glyph advances. The antialiasing may be either ClearType or grayscale depending on
  // the text antialiasing mode.
  DWRITE_RENDERING_MODE1_GDI_CLASSIC = DWRITE_RENDERING_MODE_GDI_CLASSIC;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_GDI_CLASSIC}
  // Specifies that antialiasing is performed in the horizontal direction and the appearance
  // of glyphs is layout-compatible with GDI using CLEARTYPE_NATURAL_QUALITY. Glyph advances
  // are close to the font design advances, but are still rounded to whole pixels. Use
  // DWRITE_MEASURING_MODE_GDI_NATURAL to get glyph advances. The antialiasing may be either
  // ClearType or grayscale depending on the text antialiasing mode.
  DWRITE_RENDERING_MODE1_GDI_NATURAL = DWRITE_RENDERING_MODE_GDI_NATURAL;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_GDI_NATURAL}
  // Specifies that antialiasing is performed in the horizontal direction. This rendering
  // mode allows glyphs to be positioned with subpixel precision and is therefore suitable
  // for natural (i.e., resolution-independent) layout. The antialiasing may be either
  // ClearType or grayscale depending on the text antialiasing mode.
  DWRITE_RENDERING_MODE1_NATURAL = DWRITE_RENDERING_MODE_NATURAL;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_NATURAL}
  // Similar to natural mode except that antialiasing is performed in both the horizontal
  // and vertical directions. This is typically used at larger sizes to make curves and
  // diagonal lines look smoother. The antialiasing may be either ClearType or grayscale
  // depending on the text antialiasing mode.
  DWRITE_RENDERING_MODE1_NATURAL_SYMMETRIC = DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_NATURAL_SYMMETRIC}
  // Specifies that rendering should bypass the rasterizer and use the outlines directly.
  // This is typically used at very large sizes.
  DWRITE_RENDERING_MODE1_OUTLINE = DWRITE_RENDERING_MODE_OUTLINE;
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_OUTLINE}
  // Similar to natural symmetric mode except that when possible, text should be rasterized
  // in a downsampled form.
  DWRITE_RENDERING_MODE1_NATURAL_SYMMETRIC_DOWNSAMPLED = DWRITE_RENDERING_MODE1(7);
  {$EXTERNALSYM DWRITE_RENDERING_MODE1_NATURAL_SYMMETRIC_DOWNSAMPLED}

type
  // Specify whether DWRITE_FONT_METRICS.lineGap value should be part of the line metrics.
  PDWRITE_FONT_LINE_GAP_USAGE = ^DWRITE_FONT_LINE_GAP_USAGE;
  DWRITE_FONT_LINE_GAP_USAGE = DWord;
  {$EXTERNALSYM DWRITE_FONT_LINE_GAP_USAGE}
const
  // The usage of the font line gap depends on the method used for text layout.
  DWRITE_FONT_LINE_GAP_USAGE_DEFAULT = DWRITE_FONT_LINE_GAP_USAGE(0);
  {$EXTERNALSYM DWRITE_FONT_LINE_GAP_USAGE_DEFAULT}
  // The font line gap is excluded from line spacing
  DWRITE_FONT_LINE_GAP_USAGE_DISABLED = DWRITE_FONT_LINE_GAP_USAGE(1);
  {$EXTERNALSYM DWRITE_FONT_LINE_GAP_USAGE_DISABLED}
  // The font line gap is included in line spacing
  DWRITE_FONT_LINE_GAP_USAGE_ENABLED = DWRITE_FONT_LINE_GAP_USAGE(2);
  {$EXTERNALSYM DWRITE_FONT_LINE_GAP_USAGE_ENABLED}

type
  // Specifies the container format of a font resource. A container format is distinct from
  // a font file format (DWRITE_FONT_FILE_TYPE) because the container describes the container
  // in which the underlying font file is packaged.
  PDWRITE_CONTAINER_TYPE = ^DWRITE_CONTAINER_TYPE;
  DWRITE_CONTAINER_TYPE = DWord;
  {$EXTERNALSYM DWRITE_CONTAINER_TYPE}
const
  DWRITE_CONTAINER_TYPE_UNKNOWN = DWRITE_CONTAINER_TYPE(0);
  {$EXTERNALSYM DWRITE_CONTAINER_TYPE_UNKNOWN}
  DWRITE_CONTAINER_TYPE_WOFF = DWRITE_CONTAINER_TYPE(1);
  {$EXTERNALSYM DWRITE_CONTAINER_TYPE_WOFF}
  DWRITE_CONTAINER_TYPE_WOFF2 = DWRITE_CONTAINER_TYPE(2);
  {$EXTERNALSYM DWRITE_CONTAINER_TYPE_WOFF2}

type
  // How font families are grouped together, used by IDWriteFontCollection.
  PDWRITE_FONT_FAMILY_MODEL = ^DWRITE_FONT_FAMILY_MODEL;
  DWRITE_FONT_FAMILY_MODEL = DWord;
  {$EXTERNALSYM DWRITE_FONT_FAMILY_MODEL}
const
  // Families are grouped by the typographic family name preferred by the font author. The family can contain as
  // many face as the font author wants.
  // This corresponds to the DWRITE_FONT_PROPERTY_ID_TYPOGRAPHIC_FAMILY_NAME.
  DWRITE_FONT_FAMILY_MODEL_TYPOGRAPHIC = DWRITE_FONT_FAMILY_MODEL(0);
  {$EXTERNALSYM DWRITE_FONT_FAMILY_MODEL_TYPOGRAPHIC}
  // Families are grouped by the weight-stretch-style family name, where all faces that differ only by those three
  // axes are grouped into the same family, but any other axes go into a distinct family. For example, the Sitka
  // family with six different optical sizes yields six separate families (Sitka Caption, Display, Text, Subheading,
  // Heading, Banner...). This corresponds to the DWRITE_FONT_PROPERTY_ID_WEIGHT_STRETCH_STYLE_FAMILY_NAME.
  DWRITE_FONT_FAMILY_MODEL_WEIGHT_STRETCH_STYLE = DWRITE_FONT_FAMILY_MODEL(1);
  {$EXTERNALSYM DWRITE_FONT_FAMILY_MODEL_WEIGHT_STRETCH_STYLE}

type
  // Apply certain axes automatically in layout during font selection.
  PDWRITE_AUTOMATIC_FONT_AXES = ^DWRITE_AUTOMATIC_FONT_AXES;
  DWRITE_AUTOMATIC_FONT_AXES = DWord;
  {$EXTERNALSYM DWRITE_AUTOMATIC_FONT_AXES}
const
  // No axes are automatically applied.
  DWRITE_AUTOMATIC_FONT_AXES_NONE         = DWRITE_AUTOMATIC_FONT_AXES($0000);
  {$EXTERNALSYM DWRITE_AUTOMATIC_FONT_AXES_NONE}
  // Automatically pick an appropriate optical value based on the font size (via SetFontSize) when no value is
  // specified via DWRITE_FONT_AXIS_TAG_OPTICAL_SIZE. Callers can still explicitly apply the 'opsz' value over
  // text ranges via SetFontAxisValues, which take priority.
  DWRITE_AUTOMATIC_FONT_AXES_OPTICAL_SIZE = DWRITE_AUTOMATIC_FONT_AXES($0001);
  {$EXTERNALSYM DWRITE_AUTOMATIC_FONT_AXES_OPTICAL_SIZE}

type
  // Attributes for a font axis.
  PDWRITE_FONT_AXIS_ATTRIBUTES = ^DWRITE_FONT_AXIS_ATTRIBUTES;
  DWRITE_FONT_AXIS_ATTRIBUTES = DWord;
  {$EXTERNALSYM DWRITE_FONT_AXIS_ATTRIBUTES}
const
  // No attributes.
  DWRITE_FONT_AXIS_ATTRIBUTES_NONE = DWRITE_FONT_AXIS_ATTRIBUTES($0000);
  {$EXTERNALSYM DWRITE_FONT_AXIS_ATTRIBUTES_NONE}
  // This axis is implemented as a variation axis in a variable font, with a continuous range of
  // values, such as a range of weights from 100..900. Otherwise it is either a static axis that
  // holds a single point, or it has a range but doesn't vary, such as optical size in the Skia
  // Heading font which covers a range of points but doesn't interpolate any new glyph outlines.
  DWRITE_FONT_AXIS_ATTRIBUTES_VARIABLE = DWRITE_FONT_AXIS_ATTRIBUTES($0001);
  {$EXTERNALSYM DWRITE_FONT_AXIS_ATTRIBUTES_VARIABLE}
  // This axis is recommended to be remain hidden in user interfaces. The font developer may
  // recommend this if an axis is intended to be accessed only programmatically, or is meant for
  // font-internal or font-developer use only. The axis may be exposed in lower-level font
  // inspection utilities, but should not be exposed in common or even advanced-mode user
  // interfaces in content-authoring apps.
  DWRITE_FONT_AXIS_ATTRIBUTES_HIDDEN = DWRITE_FONT_AXIS_ATTRIBUTES($0002);
  {$EXTERNALSYM DWRITE_FONT_AXIS_ATTRIBUTES_HIDDEN}

type
  // The font source type identifies the mechanism by which a font came to be included in a font set.
  PDWRITE_FONT_SOURCE_TYPE = ^DWRITE_FONT_SOURCE_TYPE;
  DWRITE_FONT_SOURCE_TYPE = DWord;
  {$EXTERNALSYM DWRITE_FONT_SOURCE_TYPE}
const
  // The font source is unknown or is not any of the other defined font source types.
  DWRITE_FONT_SOURCE_TYPE_UNKNOWN = DWRITE_FONT_SOURCE_TYPE(0);
  {$EXTERNALSYM DWRITE_FONT_SOURCE_TYPE_UNKNOWN}
  // The font source is a font file, which is installed for all users on the device.
  DWRITE_FONT_SOURCE_TYPE_PER_MACHINE = DWRITE_FONT_SOURCE_TYPE(1);
  {$EXTERNALSYM DWRITE_FONT_SOURCE_TYPE_PER_MACHINE}
  // The font source is a font file, which is installed for the current user.
  DWRITE_FONT_SOURCE_TYPE_PER_USER = DWRITE_FONT_SOURCE_TYPE(2);
  {$EXTERNALSYM DWRITE_FONT_SOURCE_TYPE_PER_USER}
  // The font source is an APPX package, which includes one or more font files.
  // The font source name is the full name of the package.
  DWRITE_FONT_SOURCE_TYPE_APPX_PACKAGE = DWRITE_FONT_SOURCE_TYPE(3);
  {$EXTERNALSYM DWRITE_FONT_SOURCE_TYPE_APPX_PACKAGE}
  // font: The is a font provider for downloadable fonts.
  DWRITE_FONT_SOURCE_TYPE_REMOTE_FONT_PROVIDER = DWRITE_FONT_SOURCE_TYPE(4);
  {$EXTERNALSYM DWRITE_FONT_SOURCE_TYPE_REMOTE_FONT_PROVIDER}


// =============================================================================

type


  // Forward interface declarations

  PIDWriteFontFaceReference = ^IDWriteFontFaceReference;
  IDWriteFontFaceReference = interface;
  PIDWriteFont3 = ^IDWriteFont3;
  IDWriteFont3 = interface;
  PIDWriteFontFace3 = ^IDWriteFontFace3;
  IDWriteFontFace3 = interface;
  PIDWriteFontSet = ^IDWriteFontSet;
  IDWriteFontSet = interface;
  PIDWriteFontSetBuilder = ^IDWriteFontSetBuilder;
  IDWriteFontSetBuilder = interface;
  PIDWriteFontCollection1 = ^IDWriteFontCollection1;
  IDWriteFontCollection1 = interface;
  PIDWriteFontFamily1 = ^IDWriteFontFamily1;
  IDWriteFontFamily1 = interface;
  PIDWriteStringList = ^IDWriteStringList;
  IDWriteStringList = interface;
  PIDWriteFontDownloadQueue = ^IDWriteFontDownloadQueue;
  IDWriteFontDownloadQueue = interface;


  // Font property used for filtering font sets and
  // building a font set with explicit properties.
  PDWRITE_FONT_PROPERTY = ^DWRITE_FONT_PROPERTY;
  DWRITE_FONT_PROPERTY = record
    // Specifies the requested font property, such as DWRITE_FONT_PROPERTY_ID_FAMILY_NAME.
    propertyId: DWRITE_FONT_PROPERTY_ID;

    // Specifies the property value, such as "Segoe UI".
    propertyValue: WideChar;

    // Specifies the language / locale to use, such as "en-US".
    // remarks
    // When passing property information to AddFontFaceReference, localeName indicates
    // the language of the property value. BCP 47 language tags should be used. If a
    // property value is inherently non-linguistic, this can be left empty.
    //
    // When used for font set filtering, leave this empty: a match will be found
    // regardless of language associated with property values.
    localeName: PWideChar;
  end;
  {$EXTERNALSYM DWRITE_FONT_PROPERTY}


  // The interface that represents text rendering settings for glyph rasterization and filtering.
  IDWriteRenderingParams3 = interface(IDWriteRenderingParams2)
  ['{B7924BAA-391B-412A-8C5C-E44CC2D867DC}']

    // Gets the rendering mode.
    function GetRenderingMode1(): DWRITE_RENDERING_MODE1; stdcall;

  end;


  // Interface IDWriteFactory3
  // =========================
  // The root factory interface for all DWrite objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory3);'}
  {$EXTERNALSYM IDWriteFactory3}
  IDWriteFactory3 = interface(IDWriteFactory2)
  ['{9A1B41C3-D3BB-466A-87FC-FE67556A3B65}']

    // Creates a glyph run analysis object, which encapsulates information
    // used to render a glyph run.
    // param name: glyphRun: Structure specifying the properties of the glyph run.
    // param name: transform: Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified by the emSize.
    // param name: renderingMode: Specifies the rendering mode, which must be one of the raster rendering modes (i.e., not default
    // and not outline).
    // param name: measuringMode: Specifies the method to measure glyphs.
    // param name: gridFitMode: How to grid-fit glyph outlines. This must be non-default.
    // param name: baselineOriginX: Horizontal position of the baseline origin, in DIPs.
    // param name: baselineOriginY: Vertical position of the baseline origin, in DIPs.
    // param name: glyphRunAnalysis: Receives a pointer to the newly created object.
    // returns
    // Standard HRESULT error code.
    function CreateGlyphRunAnalysis(glyphRun: DWRITE_GLYPH_RUN;
                                    transform: DWRITE_MATRIX;
                                    renderingMode: DWRITE_RENDERING_MODE1;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    gridFitMode: DWRITE_GRID_FIT_MODE;
                                    antialiasMode: DWRITE_TEXT_ANTIALIAS_MODE;
                                    baselineOriginX: Single;
                                    baselineOriginY: Single;
                                    out glyphRunAnalysis: IDWriteGlyphRunAnalysis): HResult; stdcall;

    // Creates a rendering parameters object with the specified properties.
    // param name: gamma: The gamma value used for gamma correction, which must be greater than zero and cannot exceed 256.
    // param name: enhancedContrast: The amount of contrast enhancement, zero or greater.
    // param name: grayscaleEnhancedContrast: The amount of contrast enhancement to use for grayscale antialiasing, zero or greater.
    // param name: clearTypeLevel: The degree of ClearType level, from 0.0f (no ClearType) to 1.0f (full ClearType).
    // param name: pixelGeometry: The geometry of a device pixel.
    // param name: renderingMode: Method of rendering glyphs. In most cases, this should be DWRITE_RENDERING_MODE_DEFAULT to automatically use an appropriate mode.
    // param name: gridFitMode: How to grid fit glyph outlines. In most cases, this should be DWRITE_GRID_FIT_DEFAULT to automatically choose an appropriate mode.
    // param name: renderingParams: Receives a pointer to the newly created rendering parameters object, or NULL in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateCustomRenderingParams(gamma: Single;
                                         enhancedContrast: Single;
                                         grayscaleEnhancedContrast: Single;
                                         clearTypeLevel: Single;
                                         pixelGeometry: DWRITE_PIXEL_GEOMETRY;
                                         renderingMode: DWRITE_RENDERING_MODE1;
                                         gridFitMode: DWRITE_GRID_FIT_MODE;
                                         out renderingParams: IDWriteRenderingParams3): HResult; stdcall;

    // Creates a reference to a font given a full path.
    // param name: filePath: Absolute file path. Subsequent operations on the constructed object may fail
    //     if the user provided filePath doesn't correspond to a valid file on the disk.
    // param name: lastWriteTime: Last modified time of the input file path. If the parameter is omitted,
    //     the function will access the font file to obtain its last write time, so the clients are encouraged to specify this value
    //     to avoid extra disk access. Subsequent operations on the constructed object may fail
    //     if the user provided lastWriteTime doesn't match the file on the disk.
    // param name: faceIndex: The zero based index of a font face in cases when the font files contain a collection of font faces.
    //     If the font files contain a single face, this value should be zero.
    // param name: fontSimulations: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontFaceReference: Receives a pointer to the newly created font face reference object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontFaceReference(filePath: PWideChar;
                                     lastWriteTime: FILETIME;
                                     faceIndex: UINT32;
                                     fontSimulations: DWRITE_FONT_SIMULATIONS;
                                     out fontFaceReference: IDWriteFontFaceReference): HResult; overload; stdcall;

    // Creates a reference to a font given a file.
    // param name: fontFile: User provided font file representing the font face.
    // param name: faceIndex: The zero based index of a font face in cases when the font files contain a collection of font faces.
    //     If the font files contain a single face, this value should be zero.
    // param name: fontSimulations: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontFaceReference: Receives a pointer to the newly created font face reference object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontFaceReference(fontFile: IDWriteFontFile;
                                     faceIndex: UINT32;
                                     fontSimulations: DWRITE_FONT_SIMULATIONS;
                                     out fontFaceReference: IDWriteFontFaceReference): HResult; overload; stdcall;

    // Retrieves the list of system fonts.
    // param name: fontSet: Receives a pointer to the font set object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontSet(out fontSet: IDWriteFontSet): HResult; stdcall;

    // Creates an empty font set builder to add font face references
    // and create a custom font set.
    // param name: fontSetBuilder: Receives a pointer to the newly created font set builder object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontSetBuilder(out fontSetBuilder: IDWriteFontSetBuilder): HResult; stdcall;

    // Create a weight-stretch-style based collection of families (DWRITE_FONT_FAMILY_MODEL_WEIGHT_STRETCH_STYLE)
    // from a set of fonts.
    // param name: fontSet: A set of fonts to use to build the collection.
    // param name: fontCollection: Receives a pointer to the newly created font collection object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontCollectionFromFontSet(fontSet: IDWriteFontSet;
                                             out fontCollection: IDWriteFontCollection1): HResult; stdcall;

    // Retrieves a weight-stretch-style based collection of font families.
    // param name: includeDownloadableFonts: Include downloadable fonts or only locally installed ones.
    // param name: fontCollection: Receives a pointer to the newly created font collection object, or nullptr in
    //     case of failure.
    // param name: checkForUpdates: If this parameter is nonzero, the function performs an immediate check for changes
    //     to the set of system fonts. If this parameter is FALSE, the function will still detect changes if the font
    //     cache service is running, but there may be some latency. For example, an application might specify TRUE if
    //     it has itself just installed a font and wants to be sure the font collection contains that font.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontCollection(includeDownloadableFonts: BOOL;
                                     out fontCollection: IDWriteFontCollection1;
                                     checkForUpdates: BOOL = FALSE): HResult; stdcall;

    // Gets the font download queue associated with this factory object.
    // param name: IDWriteFontDownloadQueue: Receives a pointer to the font download queue interface.
    // returns
    // Standard HRESULT error code.
    function GetFontDownloadQueue(out fontDownloadQueue: IDWriteFontDownloadQueue): HResult; stdcall;

  end;
  IID_IDWriteFactory3 = IDWriteFactory3;
  {$EXTERNALSYM IID_IDWriteFactory3}


  // Interface IDWriteFontSet
  // ========================
  // Set of fonts used for creating font faces, selecting nearest matching fonts, and filtering.
  // Unlike IDWriteFontFamily and IDWriteFontList, which are part of the IDWriteFontCollection heirarchy, font sets
  // are unordered flat lists.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSet);'}
  {$EXTERNALSYM IDWriteFontSet}
  IDWriteFontSet = interface(IUnknown)
  ['{53585141-D9F8-4095-8321-D73CF6BD116B}']

    // Get the number of total fonts in the set.
    // returns
    // Standard HRESULT error code.
    function GetFontCount(): UINT32; stdcall;

    // Get a reference to the font at this index, which may be local or remote.
    // param name: listIndex: Zero-based index of the font.
    // param name: fontFaceReference: Receives a pointer the font face reference object, or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    function GetFontFaceReference(listIndex: UINT32;
                                  out fontFaceReference: IDWriteFontFaceReference): HResult; stdcall;

    // Gets the index of the matching font face reference in the font set, with the same file, face index, and simulations.
    // param name: fontFaceReference: Font face reference object that specifies the physical font.
    // param name: listIndex: Receives the zero-based index of the matching font if the font was found, or UINT_MAX otherwise.
    // param name: exists: Receives TRUE if the font exists or FALSE otherwise.
    // returns
    // Standard HRESULT error code.
    function FindFontFaceReference(fontFaceReference: IDWriteFontFaceReference;
                                   out listIndex: UINT32;
                                   out exists: BOOL): HResult; stdcall;

    // Gets the index of the matching font face reference in the font set, with the same file, face index, and simulations.
    // param name: fontFaceReference: Font face object that specifies the physical font.
    // param name: listIndex: Receives the zero-based index of the matching font if the font was found, or UINT_MAX otherwise.
    // param name: exists: Receives TRUE if the font exists or FALSE otherwise.
    // returns
    // Standard HRESULT error code.
    function FindFontFace(fontFace: IDWriteFontFace;
                          out listIndex: UINT32;
                          out exists: BOOL): HResult; stdcall;

    // Returns the property values of a specific font item index.
    // param name: listIndex: Zero-based index of the font.
    // param name: propertyID: Font property of interest.
    // param name: exists: Receives the value TRUE if the font contains the specified property identifier or FALSE if not.
    // param name: strings: Receives a pointer to the newly created localized strings object, or nullptr on failure or non-existent property.
    // returns
    // Standard HRESULT error code.
    function GetPropertyValues(listIndex: UINT32;
                               propertyId: DWRITE_FONT_PROPERTY_ID;
                               out exists: BOOL;
                               values: IDWriteLocalizedStrings): HResult; overload; stdcall;

    // Returns all unique property values in the set, which can be used
    // for purposes such as displaying a family list or tag cloud. Values are
    // returned in priority order according to the language list, such that if
    // a font contains more than one localized name, the preferred one will be
    // returned.
    // param name: propertyID: Font property of interest.
    // param name: preferredLocaleNames: List of semicolon delimited language names in preferred
    //     order. When a particular string like font family has more than one localized name,
    //     the first match is returned.
    // param name: stringsList: Receives a pointer to the newly created strings list.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // For example, suppose the font set includes the Meiryo family, which has both Japanese and English family names.
    // The returned list of distinct family names would include either the Japanese name (if "ja-jp" was specified as
    // a preferred locale) or the English name (in all other cases).
    // </remarks>
    function GetPropertyValues(propertyID: DWRITE_FONT_PROPERTY_ID;
                               preferredLocaleNames: PWideChar;
                               out values: IDWriteStringList): HResult; overload; stdcall;

    // Returns all unique property values in the set, which can be used
    // for purposes such as displaying a family list or tag cloud. All values
    // are returned regardless of language, including all localized names.
    // param name: propertyID: Font property of interest.
    // param name: stringsList: Receives a pointer to the newly created strings list.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // For example, suppose the font set includes the Meiryo family, which has both Japanese and English family names.
    // The returned list of distinct family names would include both the Japanese and English names.
    // </remarks>
    function GetPropertyValues(propertyID: DWRITE_FONT_PROPERTY_ID;
                               out values: IDWriteStringList): HResult; overload; stdcall;

    // Returns how many times a given property value occurs in the set.
    // param name: property: Font property of interest.
    // param name: propertyOccurrenceCount: How many times that property occurs.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // For example, the family name "Segoe UI" may return a count of 12,
    // whereas Harrington only has 1.
    // </remarks>
    function GetPropertyOccurrenceCount(_property: DWRITE_FONT_PROPERTY;
                                        out propertyOccurrenceCount: UINT32): HResult; stdcall;

    // Returns a subset of fonts filtered by the given properties.
    // param name: properties: List of properties to filter using.
    // param name: propertyCount: How many properties to filter.
    // param name: filteredSet: Subset of fonts that match the properties,
    //     or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // If no fonts matched the filter, the subset will be empty (GetFontCount
    // returns 0), but the function does not return an error. The subset will
    // always be equal to or less than the original set.
    // </remarks>
    function GetMatchingFonts(properties: DWRITE_FONT_PROPERTY;
                              propertyCount: UINT32;
                              out filteredSet: IDWriteFontSet): HResult; overload; stdcall;

    // Returns a list of fonts within the given WWS family prioritized by
    // WWS distance.
    // param name: familyName: Neutral or localized family name of font.
    // param name: fontWeight: Weight of font.
    // param name: fontStretch: Stretch of font.
    // param name: fontStyle: Slope of font.
    // param name: filteredSet: Subset of fonts that match the properties,
    //     or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // The returned list can include simulated bold and oblique variants,
    // which would be useful for font fallback selection.
    // </remarks>
    function GetMatchingFonts(familyName: PWideChar;
                              fontWeight: DWRITE_FONT_WEIGHT;
                              fontStretch: DWRITE_FONT_STRETCH;
                              fontStyle: DWRITE_FONT_STYLE;
                              out filteredSet: IDWriteFontSet): HResult; overload; stdcall;

  end;
  IID_IDWriteFontSet = IDWriteFontSet;
  {$EXTERNALSYM IID_IDWriteFontSet}


  // Interface IDWriteFontSetBuilder
  // ===============================
  // Builder interface to add font face references and create a font set.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSetBuilder);'}
  {$EXTERNALSYM IDWriteFontSetBuilder}
  IDWriteFontSetBuilder = interface(IUnknown)
  ['{2F642AFE-9C68-4F40-B8BE-457401AFCB3D}']

    // Adds a reference to a font to the set being built. The necessary
    // metadata will automatically be extracted from the font upon calling
    // CreateFontSet.
    // param name: fontFaceReference: Font face reference object to add to the set.
    // returns
    // Standard HRESULT error code.
    function AddFontFaceReference(fontFaceReference: IDWriteFontFaceReference): HResult; overload; stdcall;

    // Adds a reference to a font to the set being built. The caller
    // supplies enough information to search on, avoiding the need to open
    // the potentially non-local font. Any properties not supplied by the
    // caller will be missing, and those properties will not be available as
    // filters in GetMatchingFonts. GetPropertyValues for missing properties
    // will return an empty string list. The properties passed should generally
    // be consistent with the actual font contents, but they need not be. You
    // could, for example, alias a font using a different name or unique
    // identifier, or you could set custom tags not present in the actual
    // font.
    // param name: fontFaceReference: Reference to the font.
    // param name: properties: List of properties to associate with the reference.
    // param name: propertyCount: How many properties are defined.
    // returns
    // Standard HRESULT error code.
    function AddFontFaceReference(fontFaceReference: IDWriteFontFaceReference;
                                  properties: DWRITE_FONT_PROPERTY;
                                  propertyCount: UINT32): HResult; overload; stdcall;

    // Appends an existing font set to the one being built, allowing
    // one to aggregate two sets or to essentially extend an existing one.
    // param name: fontSet: Font set to append font face references from.
    // returns
    // Standard HRESULT error code.
    function AddFontSet(fontSet: IDWriteFontSet): HResult; stdcall;

    // Creates a font set from all the font face references added so
    // far via AddFontFaceReference.
    // param name: fontSet: Receives a pointer to the newly created font set object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // Creating a font set takes less time if the references were added
    // with metadata rather than needing to extract the metadata from the
    // font file.
    // </remarks>
    function CreateFontSet(out fontSet: IDWriteFontSet): HResult; stdcall;

  end;
  IID_IDWriteFontSetBuilder = IDWriteFontSetBuilder;
  {$EXTERNALSYM IID_IDWriteFontSetBuilder}


  // Interface IDWriteFontCollection1
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontCollection1);'}
  {$EXTERNALSYM IDWriteFontCollection1}
  IDWriteFontCollection1 = interface(IDWriteFontCollection)
  ['{53585141-D9F8-4095-8321-D73CF6BD116C}']

    // Get the underlying font set used by this collection.
    // param name: fontSet: Contains font set used by the collection.
    // returns
    // Standard HRESULT error code.
    function GetFontSet(out fontSet: IDWriteFontSet): HResult; stdcall;

    // Creates a font family object given a zero-based font family index.
    // param name: index: Zero-based index of the font family.
    // param name: fontFamily: Receives a pointer the newly created font family object.
    // returns
    // Standard HRESULT error code.
    function GetFontFamily(index: UINT32;
                           out fontFamily: IDWriteFontFamily1): HResult; stdcall;

  end;
  IID_IDWriteFontCollection1 = IDWriteFontCollection1;
  {$EXTERNALSYM IID_IDWriteFontCollection1}


  // Interface IDWriteFontFamily1
  // ============================
  // The IDWriteFontFamily interface represents a set of fonts that share the same design but are differentiated
  // by weight, stretch, and style.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFamily1);'}
  {$EXTERNALSYM IDWriteFontFamily1}
  IDWriteFontFamily1 = interface(IDWriteFontFamily)
  ['{DA20D8EF-812A-4C43-9802-62EC4ABD7ADF}']

    // Gets the current locality of a font given its zero-based index.
    // param name: listIndex: Zero-based index of the font in the font list.
    // <remarks>
    // The locality enumeration. For fully local files, the result will always
    // be DWRITE_LOCALITY_LOCAL. For downloadable files, the result depends on how
    // much of the file has been downloaded, and GetFont() fails if the locality
    // is REMOTE and potentially fails if PARTIAL. The application can explicitly
    // ask for the font to be enqueued for download via EnqueueFontDownloadRequest
    // followed by BeginDownload().
    // remarks:
    // returns
    // The locality enumeration.
    function GetFontLocality(listIndex: UINT32): DWRITE_LOCALITY; stdcall;

    // Gets a font given its zero-based index.
    // param name: listIndex: Zero-based index of the font in the font list.
    // param name: font: Receives a pointer to the newly created font object.
    // returns
    // Standard HRESULT error code.
    function GetFont(listIndex: UINT32;
                     out font: IDWriteFont3): HResult; stdcall;

    // Gets a font face reference given its zero-based index.
    // param name: listIndex: Zero-based index of the font in the font list.
    // param name: fontFaceReference: Receives a pointer to the newly created font face reference object.
    // returns
    // Standard HRESULT error code.
    function GetFontFaceReference(listIndex: UINT32;
                                  out fontFaceReference: IDWriteFontFaceReference): HResult; stdcall;

  end;
  IID_IDWriteFontFamily1 = IDWriteFontFamily1;
  {$EXTERNALSYM IID_IDWriteFontFamily1}


  // Interface IDWriteFontList1
  // ==========================
  // The IDWriteFontList interface represents a list of fonts.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontList1);'}
  {$EXTERNALSYM IDWriteFontList1}
  IDWriteFontList1 = interface(IDWriteFontList)
  ['{DA20D8EF-812A-4C43-9802-62EC4ABD7ADE}']

    // Gets the current locality of a font given its zero-based index.
    // param name: listIndex: Zero-based index of the font in the font list.
    // <remarks>
    // The locality enumeration. For fully local files, the result will always
    // be DWRITE_LOCALITY_LOCAL. For downloadable files, the result depends on how
    // much of the file has been downloaded, and GetFont() fails if the locality
    // is REMOTE and potentially fails if PARTIAL. The application can explicitly
    // ask for the font to be enqueued for download via EnqueueFontDownloadRequest
    // followed by BeginDownload().
    // </remarks>
    // returns
    // The locality enumeration.
    function GetFontLocality(listIndex: UINT32): DWRITE_LOCALITY; stdcall;

    // Gets a font given its zero-based index.
    // param name: listIndex: Zero-based index of the font in the font list.
    // param name: font: Receives a pointer to the newly created font object.
    // returns
    // Standard HRESULT error code. The function returns DWRITE_E_REMOTEFONT if it could not construct a remote font.
    function GetFont(listIndex: UINT32;
                     out font: IDWriteFont3): HResult; stdcall;

    // Gets a font face reference given its zero-based index.
    // param name: listIndex: Zero-based index of the font in the font list.
    // param name: fontFaceReference: Receives a pointer to the newly created font face reference object.
    // returns
    // Standard HRESULT error code.
    function GetFontFaceReference(listIndex: UINT32;
                                  out fontFaceReference: IDWriteFontFaceReference): HResult; stdcall;

  end;
  IID_IDWriteFontList1 = IDWriteFontList1;
  {$EXTERNALSYM IID_IDWriteFontList1}


  // Interface IDWriteFontFaceReference
  // ==================================
  // A uniquely identifying reference to a font, from which you can create a font
  // face to query font metrics and use for rendering. A font face reference
  // consists of a font file, font face index, and font face simulation. The file
  // data may or may not be physically present on the local machine yet.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFaceReference);'}
  {$EXTERNALSYM IDWriteFontFaceReference}
   IDWriteFontFaceReference = interface(IUnknown)
   ['{5E7FA7CA-DDE3-424C-89F0-9FCD6FED58CD}']

    // Creates a font face from the reference for use with layout,
    // shaping, or rendering.
    // param name: fontFace: Newly created font face object, or nullptr in the case of failure.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // This function can fail with DWRITE_E_REMOTEFONT if the font is not local.
    // </remarks>
    function CreateFontFace(out fontFace: IDWriteFontFace3): HResult; stdcall;

    // Creates a font face with alternate font simulations, for example, to
    // explicitly simulate a bold font face out of a regular variant.
    // param name: fontFaceSimulationFlags: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontFace: Newly created font face object, or nullptr in the case of failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    // This function can fail with DWRITE_E_REMOTEFONT if the font is not local.
    function CreateFontFaceWithSimulations(fontFaceSimulationFlags: DWRITE_FONT_SIMULATIONS;
                                           out fontFace: IDWriteFontFace3): HResult; stdcall;

    // Compares two instances of a font face references for equality.
    function Equals(fontFaceReference: IDWriteFontFaceReference): BOOL; stdcall;

    // Obtains the zero-based index of the font face in its font file. If the font files contain a single face,
    // the return value is zero.
    function GetFontFaceIndex(): UINT32; stdcall;

    // Obtains the algorithmic style simulation flags of a font face.
    function GetSimulations(): DWRITE_FONT_SIMULATIONS; stdcall;

    // Obtains the font file representing a font face.
    function GetFontFile(out fontFile: IDWriteFontFile): HResult; stdcall;

    // Get the local size of the font face in bytes.
    // <remarks>
    // The value returned by GetLocalFileSize will always be less than or
    // equal to the value returned by GetFullSize. If the locality is remote,
    // the GetLocalFileSize value is zero. If the locality is local, this
    // value will equal the value returned by GetFileSize. If the locality is
    // partial, this value will equal the size of the portions of the font
    // data that have been downloaded, which will be greater than zero and
    // less than or equal to the GetFileSize value.
    // </remarks>
    function GetLocalFileSize(): UINT64; stdcall;

    // Get the total size of the font face in bytes.
    // <remarks>
    // If the locality is remote, this value is unknown and will be zero.
    // If the locality is partial or local, the value is the full size of
    // the font face.
    // </remarks>
    function GetFileSize(): UINT64; stdcall;

    // Get the last modified date.
    // <remarks>
    // The time may be zero if the font file loader does not expose file time.
    // </remarks>
    function GetFileTime(out lastWriteTime: FILETIME): HResult; stdcall;

    // Get the locality of this font face reference. You can always successfully
    // create a font face from a fully local font. Attempting to create a font
    // face on a remote or partially local font may fail with DWRITE_E_REMOTEFONT.
    // This function may change between calls depending on background downloads
    // and whether cached data expires.
    function GetLocality(): DWRITE_LOCALITY; stdcall;

    // Adds a request to the font download queue (IDWriteFontDownloadQueue).
    // returns
    // Standard HRESULT error code.
    function EnqueueFontDownloadRequest(): HResult; stdcall;

    // Adds a request to the font download queue (IDWriteFontDownloadQueue).
    // param name: characters: Array of characters to download.
    // param name: characterCount: The number of elements in the character array.
    // returns
    // Standard HRESULT error code.
    // remarks
    //   Downloading a character involves downloading every glyph it depends on
    //   directly or indirectly, via font tables (cmap, GSUB, COLR, glyf).
    function EnqueueCharacterDownloadRequest(characters: WideChar;
                                             characterCount: UINT32): HResult; stdcall;

    // Adds a request to the font download queue (IDWriteFontDownloadQueue).
    // param name: glyphIndices: Array of glyph indices to download.
    // param name: glyphCount: The number of elements in the glyph index array.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // Downloading a glyph involves downloading any other glyphs it depends on
    // from the font tables (GSUB, COLR, glyf).
    // </remarks>
    function EnqueueGlyphDownloadRequest(glyphIndices: UINT16;
                                         glyphCount: UINT32): HResult; stdcall;

    // Adds a request to the font download queue (IDWriteFontDownloadQueue).
    // param name: fileOffset: Offset of the fragment from the beginning of the font file.
    // param name: fragmentSize: Size of the fragment in bytes.
    // returns
    // Standard HRESULT error code.
    function EnqueueFileFragmentDownloadRequest(fileOffset: UINT64;
                                                fragmentSize: UINT64): HResult; stdcall;

  end;
  IID_IDWriteFontFaceReference = IDWriteFontFaceReference;
  {$EXTERNALSYM IID_IDWriteFontFaceReference}


  // Interface IDWriteFont3
  // ======================
  // The IDWriteFont interface represents a font in a font collection.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFont3);'}
  {$EXTERNALSYM IDWriteFont3}
  IDWriteFont3 = interface(IDWriteFont2)
  ['{29748ED6-8C9C-4A6A-BE0B-D912E8538944}']

    // Creates a font face object for the font.
    // param name: fontFace: Receives a pointer to the newly created font face object.
    // returns
    // Standard HRESULT error code. The function returns DWRITE_E_REMOTEFONT if it could not construct a remote font.
    function CreateFontFace(out fontFace: IDWriteFontFace3): HResult; stdcall;

    // Compares two instances of a font references for equality.
    function Equals(font: IDWriteFont): BOOL; stdcall;

    // Return a font face reference identifying this font.
    // param name: fontFaceReference: A uniquely identifying reference to a font face.
    // returns
    // Standard HRESULT error code.
    function GetFontFaceReference(out fontFaceReference: IDWriteFontFaceReference): HResult; stdcall;

    // Determines whether the font supports the specified character.
    // param name: unicodeValue: Unicode (UCS-4) character value.
    // returns
    // Returns TRUE if the font has the specified character, FALSE if not.
    function HasCharacter(unicodeValue: UINT32): BOOL; stdcall;

    // Gets the current locality of the font.
    // <remarks>
    // The locality enumeration. For fully local files, the result will always
    // be DWRITE_LOCALITY_LOCAL. A downloadable file may be any of the states,
    // and this function may change between calls.
    // </remarks>
    // returns
    // The locality enumeration.
    function GetLocality(): DWRITE_LOCALITY; stdcall;

  end;
  IID_IDWriteFont3 = IDWriteFont3;
  {$EXTERNALSYM IID_IDWriteFont3}


  // Interface IDWriteFontFace3
  // ==========================
  // The interface that represents an absolute reference to a font face.
  // It contains font face type, appropriate file references and face identification data.
  // Various font data such as metrics, names and glyph outlines is obtained from IDWriteFontFace.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace3);'}
  {$EXTERNALSYM IDWriteFontFace3}
  IDWriteFontFace3 = interface(IDWriteFontFace2)
  ['{D37D7598-09BE-4222-A236-2081341CC1F2}']

    // Return a font face reference identifying this font.
    // param name: fontFaceReference: A uniquely identifying reference to a font face.
    // returns
    // Standard HRESULT error code.
    function GetFontFaceReference(out fontFaceReference: IDWriteFontFaceReference): HResult; stdcall;

    // Gets the PANOSE values from the font, used for font selection and
    // matching.
    // param name: panose: PANOSE structure to fill in.
    // <remarks>
    // The function does not simulate these, such as substituting a weight or
    // proportion inferred on other values. If the font does not specify them,
    // they are all set to 'any' (0).
    // </remarks>
    procedure GetPanose(out panose: DWRITE_PANOSE); stdcall;

    // Gets the weight of the specified font.
    function GetWeight(): DWRITE_FONT_WEIGHT; stdcall;

    // Gets the stretch (aka. width) of the specified font.
    function GetStretch(): DWRITE_FONT_STRETCH; stdcall;

    // Gets the style (aka. slope) of the specified font.
    function GetStyle(): DWRITE_FONT_STYLE; stdcall;

    // Creates an localized strings object that contains the weight-stretch-style family names for the font family, indexed by locale name.
    // param name: names: Receives a pointer to the newly created localized strings object.
    // returns
    // Standard HRESULT error code.
    function GetFamilyNames(out names: IDWriteLocalizedStrings): HResult; stdcall;

    // Gets a localized strings collection containing the weight-stretch-style face names for the font (e.g., Regular or Bold), indexed by locale name.
    // param name: names: Receives a pointer to the newly created localized strings object.
    // returns
    // Standard HRESULT error code.
    function GetFaceNames(out names: IDWriteLocalizedStrings): HResult; stdcall;

    // Gets a localized strings collection containing the specified informational strings, indexed by locale name.
    // param name: informationalStringID: Identifies the string to get.
    // param name: informationalStrings: Receives a pointer to the newly created localized strings object.
    // param name: exists: Receives the value TRUE if the font contains the specified string ID or FALSE if not.
    // returns
    // Standard HRESULT error code. If the font does not contain the specified string, the return value is S_OK but
    // informationalStrings receives a NULL pointer and exists receives the value FALSE.
    function GetInformationalStrings(informationalStringID: DWRITE_INFORMATIONAL_STRING_ID;
                                     informationalStrings: IDWriteLocalizedStrings;
                                     out exists: BOOL): HResult; stdcall;

    // Determines whether the font supports the specified character.
    // param name: unicodeValue: Unicode (UCS-4) character value.
    // returns
    // Returns TRUE if the font has the specified character, FALSE if not.
    function HasCharacter(unicodeValue: UINT32): BOOL; stdcall;

    // Determines the recommended text rendering and grid-fit mode to be used based on the
    // font, size, world transform, and measuring mode.
    // param name: fontEmSize: Logical font size in DIPs.
    // param name: dpiX: Number of pixels per logical inch in the horizontal direction.
    // param name: dpiY: Number of pixels per logical inch in the vertical direction.
    // param name: transform: Specifies the world transform.
    // param name: outlineThreshold: Specifies the quality of the graphics system's outline rendering,
    // affects the size threshold above which outline rendering is used.
    // param name: measuringMode: Specifies the method used to measure during text layout. For proper
    // glyph spacing, the function returns a rendering mode that is compatible with the specified
    // measuring mode.
    // param name: renderingParams: Rendering parameters object. This parameter is necessary in case the rendering parameters
    // object overrides the rendering mode.
    // param name: renderingMode: Receives the recommended rendering mode.
    // param name: gridFitMode: Receives the recommended grid-fit mode.
    // <remarks>
    // This method should be used to determine the actual rendering mode in cases where the rendering
    // mode of the rendering params object is DWRITE_RENDERING_MODE_DEFAULT, and the actual grid-fit
    // mode when the rendering params object is DWRITE_GRID_FIT_MODE_DEFAULT.
    // </remarks>
    // returns
    // Standard HRESULT error code.
    function GetRecommendedRenderingMode(fontEmSize: Single;
                                         dpiX: Single;
                                         dpiY: Single;
                                         transform: DWRITE_MATRIX;
                                         isSideways: BOOL;
                                         outlineThreshold: DWRITE_OUTLINE_THRESHOLD;
                                         measuringMode: DWRITE_MEASURING_MODE;
                                         renderingParams: IDWriteRenderingParams;
                                         out renderingMode: DWRITE_RENDERING_MODE1;
                                         out gridFitMode: DWRITE_GRID_FIT_MODE): HResult; stdcall;


    // Determines whether the character is locally downloaded from the font.
    // param name: unicodeValue: Unicode (UCS-4) character value.
    // returns
    // Returns TRUE if the font has the specified character locally available,
    // FALSE if not or if the font does not support that character.
    function IsCharacterLocal(unicodeValue: UINT32): BOOL; stdcall;

    // Determines whether the glyph is locally downloaded from the font.
    // param name: glyphId: Glyph identifier.
    // returns
    // Returns TRUE if the font has the specified glyph locally available.
    function IsGlyphLocal(glyphId: UINT16): BOOL; stdcall;

    // Determines whether the specified characters are local.
    // param name: characters: Array of characters.
    // param name: characterCount: The number of elements in the character array.
    // param name: enqueueIfNotLocal: Specifies whether to enqueue a download request
    // if any of the specified characters are not local.
    // param name: isLocal: Receives TRUE if all of the specified characters are local,
    // FALSE if any of the specified characters are remote.
    // returns
    // Standard HRESULT error code.
    function AreCharactersLocal(characters: WideChar;
                                characterCount: UINT32;
                                enqueueIfNotLocal: BOOL;
                                out isLocal: BOOL): HResult; stdcall;

    // Determines whether the specified glyphs are local.
    // param name: glyphIndices: Array of glyph indices.
    // param name: glyphCount: The number of elements in the glyph index array.
    // param name: enqueueIfNotLocal: Specifies whether to enqueue a download request
    // if any of the specified glyphs are not local.
    // param name: isLocal: Receives TRUE if all of the specified glyphs are local,
    // FALSE if any of the specified glyphs are remote.
    // returns
    // Standard HRESULT error code.
    function AreGlyphsLocal(glyphIndices: UINT16;
                            glyphCount: UINT32;
                            enqueueIfNotLocal: BOOL;
                            out isLocal: BOOL): HResult; stdcall;

  end;
  IID_IDWriteFontFace3 = IDWriteFontFace3;
  {$EXTERNALSYM IID_IDWriteFontFace3}


  // Interface IDWriteStringList
  // ===========================
  // Represents a collection of strings indexed by number.
  // An IDWriteStringList is otherwise identical to IDWriteLocalizedStrings except
  // for the semantics, where localized strings are indexed on language (each
  // language has one string property) whereas a string list may contain multiple
  // strings of the same language, such as a string list of family names from a
  // font set. You can QueryInterface from an IDWriteLocalizedStrings to an
  // IDWriteStringList.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteStringList);'}
  {$EXTERNALSYM IDWriteStringList}
  IDWriteStringList = interface(IUnknown)
  ['{CFEE3140-1157-47CA-8B85-31BFCF3F2D0E}']

    // Gets the number of strings.
    function GetCount(): UINT32; stdcall;

    // Gets the length in characters (not including the null terminator) of the locale name with the specified index.
    // param name: listIndex: Zero-based index of the locale name.
    // param name: length: Receives the length in characters, not including the null terminator.
    // returns
    // Standard HRESULT error code.
    function GetLocaleNameLength(listIndex: UINT32;
                                 out length: UINT32): HResult; stdcall;

    // Copies the locale name with the specified index to the specified array.
    // param name: listIndex: Zero-based index of the locale name.
    // param name: localeName: Character array that receives the locale name.
    // param name: size: Size of the array in characters. The size must include space for the terminating
    // null character.
    // returns
    // Standard HRESULT error code.
    function GetLocaleName(listIndex: UINT32;
                           localeName: PWideChar;
                           size: UINT32): HResult; stdcall;

    // Gets the length in characters (not including the null terminator) of the string with the specified index.
    // param name: listIndex: Zero-based index of the string.
    // param name: length: Receives the length in characters, not including the null terminator.
    // returns
    // Standard HRESULT error code.
    function GetStringLength(listIndex: UINT32;
                             out length: UINT32): HResult; stdcall;

    // Copies the string with the specified index to the specified array.
    // param name: listIndex: Zero-based index of the string.
    // param name: stringBuffer: Character array that receives the string.
    // param name: size: Size of the array in characters. The size must include space for the terminating
    //     null character.
    // returns
    // Standard HRESULT error code.
    function GetString(listIndex: UINT32;
                       stringBuffer: PWideChar;
                       stringBufferSize: UINT32): HResult; stdcall;

  end;
  IID_IDWriteStringList = IDWriteStringList;
  {$EXTERNALSYM IID_IDWriteStringList}


  // Interface IDWriteFontDownloadListener
  // =====================================
  // Application-defined callback interface that receives notifications from the font
  // download queue (IDWriteFontDownloadQueue interface). Callbacks will occur on the
  // downloading thread, and objects must be prepared to handle calls on their methods
  // from other threads at any time.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontDownloadListener);'}
  {$EXTERNALSYM IDWriteFontDownloadListener}
  IDWriteFontDownloadListener = interface(IUnknown)
  ['{B06FE5B9-43EC-4393-881B-DBE4DC72FDA7}']

    // The DownloadCompleted method is called back on an arbitrary thread when a
    // download operation ends.
    // param name: downloadQueue: Pointer to the download queue interface on which
    // the BeginDownload method was called.
    // param name: context: Optional context object that was passed to BeginDownload.
    // AddRef is called on the context object by BeginDownload and Release is called
    // after the DownloadCompleted method returns.
    // param name: downloadResult: Result of the download operation.
    procedure DownloadCompleted(downloadQueue: IDWriteFontDownloadQueue;
                                context: IUnknown;
                                downloadResult: HResult); stdcall;
  end;
  IID_IDWriteFontDownloadListener = IDWriteFontDownloadListener;
  {$EXTERNALSYM IID_IDWriteFontDownloadListener}


  // Interface IDWriteFontDownloadQueue
  // ==================================
  // Interface that enqueues download requests for remote fonts, characters, glyphs, and font fragments.
  // Provides methods to asynchronously execute a download, cancel pending downloads, and be notified of
  // download completion. Callbacks to listeners will occur on the downloading thread, and objects must
  // be must be able to handle calls on their methods from other threads at any time.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontDownloadQueue);'}
  {$EXTERNALSYM IDWriteFontDownloadQueue}
  IDWriteFontDownloadQueue = interface(IUnknown)
  ['{B71E6052-5AEA-4FA3-832E-F60D431F7E91}']

    // Registers a client-defined listener object that receives download notifications.
    // All registered listener's DownloadCompleted will be called after BeginDownload
    // completes.
    // param name: listener: Listener object to add.
    // param name: token: Receives a token value, which the caller must subsequently
    // pass to RemoveListener.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // An IDWriteFontDownloadListener can also be passed to BeginDownload via the
    // context parameter, rather than globally registered to the queue.
    // </remarks>
    function AddListener(listener: IDWriteFontDownloadListener;
                         out token: UINT32): HResult; stdcall;

    // Unregisters a notification handler that was previously registered using
    // AddListener.
    // param name: token: Token value previously returned by AddListener.
    // returns
    // Returns S_OK if successful or E_INVALIDARG if the specified token does not
    // correspond to a registered listener.
    function RemoveListener(token: UINT32): HResult; stdcall;

    // Determines whether the download queue is empty. Note that the queue does not
    // include requests that are already being downloaded. In other words, BeginDownload
    // clears the queue.
    // returns
    // TRUE if the queue is empty, FALSE if there are requests pending for BeginDownload.
    function IsEmpty(): BOOL; stdcall;

    // Begins an asynchronous download operation. The download operation executes
    // in the background until it completes or is cancelled by a CancelDownload call.
    // param name: context: Optional context object that is passed back to the
    // download notification handler's DownloadCompleted method. If the context object
    // implements IDWriteFontDownloadListener, its DownloadCompleted will be called
    // when done.
    // returns
    // Returns S_OK if a download was successfully begun, S_FALSE if the queue was
    // empty, or a standard HRESULT error code.
    // <remarks>
    // BeginDownload removes all download requests from the queue, transferring them
    // to a background download operation. If any previous downloads are still ongoing
    // when BeginDownload is called again, the new download does not complete until
    // the previous downloads have finished. If the queue is empty and no active
    // downloads are pending, the DownloadCompleted callback is called immediately with
    // DWRITE_DOWNLOAD_RESULT_NONE.
    // </remarks>
    function BeginDownload(context: IUnknown = IUnknown(Nil)): HResult; stdcall;

    // Removes all download requests from the queue and cancels any active download
    // operations. This calls DownloadCompleted with DWRITE_E_DOWNLOADCANCELLED.
    // Applications should call this when shutting down if they started any
    // downloads that have not finished yet with a call to DownloadCompleted.
    // returns
    // Standard HRESULT error code.
    function CancelDownload(): HResult; stdcall;

    // Get the current generation number of the download queue, which is incremented
    // every time after a download completes, whether failed or successful. This cookie
    // comparison value may be used to compared against cached data to know when it is
    // stale.
    // returns
    // The number of download queue generations.
    function GetGenerationCount(): UINT64; stdcall;

  end;
  IID_IDWriteFontDownloadQueue = IDWriteFontDownloadQueue;
  {$EXTERNALSYM IID_IDWriteFontDownloadQueue}


  // Interface IDWriteGdiInterop1
  // ============================
  // The GDI interop interface provides interoperability with GDI.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteGdiInterop1);'}
  {$EXTERNALSYM IDWriteGdiInterop1}
  IDWriteGdiInterop1 = interface(IDWriteGdiInterop)
  ['{4556BE70-3ABD-4F70-90BE-421780A6F515}']

    // Creates a font object that matches the properties specified by the LOGFONT structure.
    // param name: logFont: Structure containing a GDI-compatible font description.
    // param name: fontCollection: The font collection to search. If NULL, the local system font collection is used.
    // param name: font: Receives a newly created font object if successful, or NULL in case of error.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // The only fields that matter include: lfFaceName, lfCharSet, lfWeight, lfItalic.
    // Font size and rendering mode are a rendering time property, not a font property,
    // and text decorations like underline are drawn separately from the text. If no
    // font matches the given weight, slope, and character set, the best match within
    // the given GDI family name will be returned. DWRITE_E_NOFONT is returned if there
    // is no matching font name using either the GDI family name (e.g. Arial) or the
    // full font name (e.g. Arial Bold Italic).
    // </remarks>
    function CreateFontFromLOGFONT(_logFont: LOGFONT;
                                   fontCollection: IDWriteFontCollection;
                                   out font: IDWriteFont): HResult; stdcall;

    // Reads the font signature from the given font.
    // param name: font: Font to read font signature from.
    // param name: fontSignature: Font signature from the OS/2 table, ulUnicodeRange and ulCodePageRange.
    // returns
    // Standard HRESULT error code.
    function GetFontSignature(font: IDWriteFont;
                              out _fontSignature: FONTSIGNATURE): HResult; overload; stdcall;

    // Reads the font signature from the given font.
    // param name: font: Font to read font signature from.
    // param name: fontSignature: Font signature from the OS/2 table, ulUnicodeRange and ulCodePageRange.
    // returns
    // Standard HRESULT error code.
    function GetFontSignature(fontFace: IDWriteFontFace;
                              out fontSignature: FONTSIGNATURE): HResult; overload; stdcall;

    // Get a list of matching fonts based on the LOGFONT values. Only fonts
    // of that family name will be returned.
    // returns
    // Standard HRESULT error code.
    function GetMatchingFontsByLOGFONT(_logFont: LOGFONT;
                                       fontSet: IDWriteFontSet;
                                       out filteredSet: IDWriteFontSet): HResult; stdcall;

  end;
  IID_IDWriteGdiInterop1 = IDWriteGdiInterop1;
  {$EXTERNALSYM IID_IDWriteGdiInterop1}


  // Information about a formatted line of text.
  PDWRITE_LINE_METRICS1 = ^DWRITE_LINE_METRICS1;
  DWRITE_LINE_METRICS1 = record
    DWriteLineMetrics: DWRITE_LINE_METRICS;

    // White space before the content of the line. This is included in the line height and baseline distances.
    // If the line is formatted horizontally either with a uniform line spacing or with proportional
    // line spacing, this value represents the extra space above the content.
    leadingBefore: Single;

    // White space after the content of the line. This is included in the height of the line.
    // If the line is formatted horizontally either with a uniform line spacing or with proportional
    // line spacing, this value represents the extra space below the content.
    leadingAfter: Single;

  end;
  {$EXTERNALSYM DWRITE_LINE_METRICS1}


  // The DWRITE_LINE_SPACING structure specifies the parameters used to specify how to manage space between lines.
  PDWRITE_LINE_SPACING = ^DWRITE_LINE_SPACING;
  DWRITE_LINE_SPACING = record

    // Method used to determine line spacing.
    method: DWRITE_LINE_SPACING_METHOD;

    // Spacing between lines.
    // The interpretation of this parameter depends upon the line spacing method, as follows:
    // - default line spacing: ignored
    // - uniform line spacing: explicit distance in DIPs between lines
    // - proportional line spacing: a scaling factor to be applied to the computed line height;
    //   for each line, the height of the line is computed as for default line spacing, and the scaling factor is applied to that value.
    height: Single;

    // Distance from top of line to baseline.
    // The interpretation of this parameter depends upon the line spacing method, as follows:
    // - default line spacing: ignored
    // - uniform line spacing: explicit distance in DIPs from the top of the line to the baseline
    // - proportional line spacing: a scaling factor applied to the computed baseline; for each line,
    //   the baseline distance is computed as for default line spacing, and the scaling factor is applied to that value.
    baseline: Single;

    // Proportion of the entire leading distributed before the line. The allowed value is between 0 and 1.0. The remaining
    // leading is distributed after the line. It is ignored for the default and uniform line spacing methods.
    // The leading that is available to distribute before or after the line depends on the values of the height and
    // baseline parameters.
    leadingBefore: Single;

    // Specify whether DWRITE_FONT_METRICS.lineGap value should be part of the line metrics.
    fontLineGapUsage: DWRITE_FONT_LINE_GAP_USAGE;
  end;
  {$EXTERNALSYM DWRITE_LINE_SPACING}

  // Interface IDWriteTextFormat2
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextFormat2);'}
  {$EXTERNALSYM IDWriteTextFormat2}
  IDWriteTextFormat2 = interface(IDWriteTextFormat1)
  ['{F67E0EDD-9E3D-4ECC-8C32-4183253DFE70}']

    // Set line spacing.
    // param name: lineSpacing: How to manage space between lines.
    // returns
    // Standard HRESULT error code.
    function SetLineSpacing(lineSpacingOptions: DWRITE_LINE_SPACING): HResult; stdcall;

    // Get line spacing.
    // param name: lineSpacing: How to manage space between lines.
    // returns
    // Standard HRESULT error code.
    function GetLineSpacing(out lineSpacingOptions: DWRITE_LINE_SPACING): HResult; stdcall;

  end;
  IID_IDWriteTextFormat2 = IDWriteTextFormat2;
  {$EXTERNALSYM IID_IDWriteTextFormat2}


  // Interface IDWriteTextLayout3
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextLayout3);'}
  {$EXTERNALSYM IDWriteTextLayout3}
  IDWriteTextLayout3 = interface(IDWriteTextLayout2)
  ['{07DDCD52-020E-4DE8-AC33-6C953D83F92D}']

    // Invalidates the layout, forcing layout to remeasure before calling the
    // metrics or drawing functions. This is useful if the locality of a font
    // changes, and layout should be redrawn, or if the size of a client
    // implemented IDWriteInlineObject changes.
    // returns
    // Standard HRESULT error code.
    function InvalidateLayout(): HResult; stdcall;

    // Set line spacing.
    // param name: lineSpacing: How to manage space between lines.
    // returns
    // Standard HRESULT error code.
    function SetLineSpacing(lineSpacingOptions: DWRITE_LINE_SPACING): HResult; stdcall;

    // Get line spacing.
    // param name: lineSpacing: How to manage space between lines.
    // returns
    // Standard HRESULT error code.
    function GetLineSpacing(out lineSpacingOptions: DWRITE_LINE_SPACING): HResult; stdcall;

    // GetLineMetrics returns properties of each line.
    // param name: lineMetrics: The array to fill with line information.
    // param name: maxLineCount: The maximum size of the lineMetrics array.
    // param name: actualLineCount: The actual size of the lineMetrics
    // array that is needed.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // If maxLineCount is not large enough E_NOT_SUFFICIENT_BUFFER,
    // which is equivalent to HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER),
    // is returned and *actualLineCount is set to the number of lines
    // needed.
    // </remarks>
    function GetLineMetrics(out lineMetrics: PDWRITE_LINE_METRICS1;
                            maxLineCount: UINT32;
                            out actualLineCount: UINT32): HResult; stdcall;

  end;
  IID_IDWriteTextLayout3 = IDWriteTextLayout3;
  {$EXTERNALSYM IID_IDWriteTextLayout3}


///////////////////////////////////////////////////////////////////


// #if NTDDI_VERSION >= NTDDI_WIN10_RS1


  // Represents a color glyph run. The IDWriteFactory4.TranslateColorGlyphRun
  // method returns an ordered collection of color glyph runs of varying types
  // depending on what the font supports.
  // For runs without any specific color, such as PNG data, the runColor field will be zero.
  DWRITE_COLOR_GLYPH_RUN1 = record
    DWriteColorGlyphRun: DWRITE_COLOR_GLYPH_RUN;
    // Type of glyph image format for this color run. Exactly one type will be set since
    // TranslateColorGlyphRun has already broken down the run into separate parts.
    glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS;

    // Measuring mode to use for this glyph run.
    measuringMode: DWRITE_MEASURING_MODE;
  end;
    {$EXTERNALSYM DWRITE_COLOR_GLYPH_RUN1}


  // Data for a single glyph from GetGlyphImageData.
  PDWRITE_GLYPH_IMAGE_DATA = ^DWRITE_GLYPH_IMAGE_DATA;
  DWRITE_GLYPH_IMAGE_DATA = record

    // Pointer to the glyph data, be it SVG, PNG, JPEG, TIFF.
    imageData: Pointer;

    // Size of glyph data in bytes.
    imageDataSize: UINT32;

    // Unique identifier for the glyph data. Clients may use this to cache a parsed/decompressed
    // version and tell whether a repeated call to the same font returns the same data.
    uniqueDataId: UINT32;

    // Pixels per em of the returned data. For non-scalable raster data (PNG/TIFF/JPG), this can be larger
    // or smaller than requested from GetGlyphImageData when there isn't an exact match.
    // For scaling intermediate sizes, use: desired pixels per em * font em size / actual pixels per em.
    pixelsPerEm: UINT32;

    // Size of image when the format is pixel data.
    pixelSize: D2D1_SIZE_U;

    // Left origin along the horizontal Roman baseline.
    horizontalLeftOrigin: D2D1_POINT_2L;

    // Right origin along the horizontal Roman baseline.
    horizontalRightOrigin: D2D1_POINT_2L;

    // Top origin along the vertical central baseline.
    verticalTopOrigin: D2D1_POINT_2L;

    // Bottom origin along vertical central baseline.
    verticalBottomOrigin: D2D1_POINT_2L;
  end;
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_DATA}


  // Interface IDWriteColorGlyphRunEnumerator1
  // =========================================
  // Enumerator for an ordered collection of color glyph runs.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteColorGlyphRunEnumerator1);'}
  {$EXTERNALSYM IDWriteColorGlyphRunEnumerator1}
  IDWriteColorGlyphRunEnumerator1 = interface(IDWriteColorGlyphRunEnumerator)
  ['{7C5F86DA-C7A1-4F05-B8E1-55A179FE5A35}']

    // Gets the current color glyph run.
    // param name: colorGlyphRun: Receives a pointer to the color
    // glyph run. The pointer remains valid until the next call to
    // MoveNext or until the interface is released.
    // returns
    // Standard HRESULT error code. An error is returned if there is
    // no current glyph run, i.e., if MoveNext has not yet been called
    // or if the end of the sequence has been reached.
    function GetCurrentRun(colorGlyphRun: DWRITE_COLOR_GLYPH_RUN1): HResult; stdcall;

  end;
  IID_IDWriteColorGlyphRunEnumerator1 = IDWriteColorGlyphRunEnumerator1;
  {$EXTERNALSYM IID_IDWriteColorGlyphRunEnumerator1}


  // Interface IDWriteFontFace4
  // ==========================
  // The interface that represents an absolute reference to a font face.
  // It contains font face type, appropriate file references and face identification data.
  // Various font data such as metrics, names and glyph outlines is obtained from IDWriteFontFace.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace4);'}
  {$EXTERNALSYM IDWriteFontFace4}
  IDWriteFontFace4 = interface(IDWriteFontFace3)
  ['{27F2A904-4EB8-441D-9678-0563F53E3E2F}']

    // Gets all the glyph image formats supported by the entire font (SVG, PNG, JPEG, ...).
    function GetGlyphImageFormats(): DWRITE_GLYPH_IMAGE_FORMATS; overload; stdcall;

    // Gets the available image formats of a specific glyph and ppem. Glyphs often have at least TrueType
    // or CFF outlines, but they may also have SVG outlines, or they may have only bitmaps
    // with no TrueType/CFF outlines. Some image formats, notably the PNG/JPEG ones, are size
    // specific and will return no match when there isn't an entry in that size range.
    // <remarks>
    // Glyph ids beyond the glyph count return DWRITE_GLYPH_IMAGE_FORMATS_NONE.
    // </remarks>
    function GetGlyphImageFormats(glyphId: UINT16;
                                  pixelsPerEmFirst: UINT32;
                                  pixelsPerEmLast: UINT32;
                                  out glyphImageFormats: DWRITE_GLYPH_IMAGE_FORMATS): HResult; overload; stdcall;

    // Gets a pointer to the glyph data based on the desired image format.
    // <remarks>
    // The glyphDataContext must be released via ReleaseGlyphImageData when done if the data is not empty,
    // similar to IDWriteFontFileStream.ReadFileFragment and IDWriteFontFileStream.ReleaseFileFragment.
    // The data pointer is valid so long as the IDWriteFontFace exists and ReleaseGlyphImageData has not
    // been called.
    // </remarks>
    // <remarks>
    // The DWRITE_GLYPH_IMAGE_DATA.uniqueDataId is valuable for caching purposes so that if the same
    // resource is returned more than once, an existing resource can be quickly retrieved rather than
    // needing to reparse or decompress the data.
    // </remarks>
    // <remarks>
    // The function only returns SVG or raster data - requesting TrueType/CFF/COLR data returns
    // DWRITE_E_INVALIDARG. Those must be drawn via DrawGlyphRun or queried using GetGlyphOutline instead.
    // Exactly one format may be requested or else the function returns DWRITE_E_INVALIDARG.
    // If the glyph does not have that format, the call is not an error, but the function returns empty data.
    // </remarks>
    function GetGlyphImageData(glyphId: UINT16;
                               pixelsPerEm: UINT32;
                               glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS;
                               out glyphData: DWRITE_GLYPH_IMAGE_DATA;
                               glyphDataContext: Pointer): HResult; stdcall;

    // Releases the table data obtained earlier from ReadGlyphData.
    // param name: glyphDataContext: Opaque context from ReadGlyphData.
    procedure ReleaseGlyphImageData(glyphDataContext: Pointer); stdcall;

  end;
  IID_IDWriteFontFace4 = IDWriteFontFace4;
  {$EXTERNALSYM IID_IDWriteFontFace4}


  // Interface IDWriteFactory4
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory4);'}
  {$EXTERNALSYM IDWriteFactory4}
  IDWriteFactory4 = interface(IDWriteFactory3)
  ['{4B0B5BD3-0797-4549-8AC5-FE915CC53856}']

    // Translates a glyph run to a sequence of color glyph runs, which can be
    // rendered to produce a color representation of the original "base" run.
    // param name: baselineOriginX: Horizontal and vertical origin of the base glyph run in
    // pre-transform coordinates.
    // param name: glyphRun: Pointer to the original "base" glyph run.
    // param name: glyphRunDescription: Optional glyph run description.
    // param name: glyphImageFormats: Which data formats TranslateColorGlyphRun
    // should split the runs into.
    // param name: measuringMode: Measuring mode, needed to compute the origins
    // of each glyph.
    // param name: worldToDeviceTransform: Matrix converting from the client's
    // coordinate space to device coordinates (pixels), i.e., the world transform
    // multiplied by any DPI scaling.
    // param name: colorPaletteIndex: Zero-based index of the color palette to use.
    // Valid indices are less than the number of palettes in the font, as returned
    // by IDWriteFontFace2.GetColorPaletteCount.
    // param name: colorLayers: If the function succeeds, receives a pointer
    // to an enumerator object that can be used to obtain the color glyph runs.
    // If the base run has no color glyphs, then the output pointer is NULL
    // and the method returns DWRITE_E_NOCOLOR.
    // returns
    // Returns DWRITE_E_NOCOLOR if the font has no color information, the glyph run
    // does not contain any color glyphs, or the specified color palette index
    // is out of range. In this case, the client should render the original glyph
    // run. Otherwise, returns a standard HRESULT error code.
    // <remarks>
    // The old IDWriteFactory2.TranslateColorGlyphRun is equivalent to passing
    // DWRITE_GLYPH_IMAGE_FORMATS_TRUETYPE|CFF|COLR.
    // </remarks>
    function TranslateColorGlyphRun(baselineOrigin: D2D1_POINT_2F;
                                    glyphRun: DWRITE_GLYPH_RUN;
                                    glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                                    desiredGlyphImageFormats: DWRITE_GLYPH_IMAGE_FORMATS;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    worldAndDpiTransform: DWRITE_MATRIX;
                                    colorPaletteIndex: UINT32;
                                    out colorLayers: IDWriteColorGlyphRunEnumerator1): HResult; stdcall;

    // Converts glyph run placements to glyph origins.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // The transform and DPI have no affect on the origin scaling.
    // They are solely used to compute glyph advances when not supplied
    // and align glyphs in pixel aligned measuring modes.
    // </remarks>
    function ComputeGlyphOrigins(glyphRun: DWRITE_GLYPH_RUN;
                                 measuringMode: DWRITE_MEASURING_MODE;
                                 baselineOrigin: D2D1_POINT_2F;
                                 worldAndDpiTransform: DWRITE_MATRIX;
                                 out glyphOrigins: D2D1_POINT_2F): HResult; overload; stdcall;

    // Converts glyph run placements to glyph origins. This overload is for natural metrics, which
    // includes SVG, TrueType natural modes, and bitmap placement.
    function ComputeGlyphOrigins(glyphRun: DWRITE_GLYPH_RUN;
                                 baselineOrigin: D2D1_POINT_2F;
                                 out glyphOrigins: D2D1_POINT_2F): HResult; overload; stdcall;

  end;
  IID_IDWriteFactory4 = IDWriteFactory4;
  {$EXTERNALSYM IID_IDWriteFactory4}

//#endif // NTDDI_VERSION >= NTDDI_WIN10_RS1


///////////////////////////////////////////////////////////////////


//#if NTDDI_VERSION >= NTDDI_WIN10_RS2

  // Interface IDWriteFontSetBuilder1
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSetBuilder1);'}
  {$EXTERNALSYM IDWriteFontSetBuilder1}
  IDWriteFontSetBuilder1 = interface(IDWriteFontSetBuilder)
  ['{3FF7715F-3CDC-4DC6-9B72-EC5621DCCAFD}']

    // Adds references to all the fonts in the specified font file. The method
    // parses the font file to determine the fonts and their properties.
    // param name: fontFile: Font file reference object to add to the set.
    // returns
    // Standard HRESULT error code.
    function AddFontFile(fontFile: IDWriteFontFile): HResult; stdcall;

  end;
  IID_IDWriteFontSetBuilder1 = IDWriteFontSetBuilder1;
  {$EXTERNALSYM IID_IDWriteFontSetBuilder1}


  // Interface IDWriteAsyncResult
  // ============================
  // The IDWriteAsyncResult interface represents the result of an asynchronous
  // operation. A client can use the interface to wait for the operation to
  // complete and to get the result.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteAsyncResult);'}
  {$EXTERNALSYM IDWriteAsyncResult}
  IDWriteAsyncResult = interface(IUnknown)
  ['{CE25F8FD-863B-4D13-9651-C1F88DC73FE2}']

    // The GetWaitHandleMethod method returns a handle that can be used to wait
    // for the asynchronous operation to complete. The handle remains valid
    // until the interface is released.
    function GetWaitHandle(): THandle; stdcall;

    // The GetResult method returns the result of the asynchronous operation.
    // The return value is E_PENDING if the operation has not yet completed.
    function GetResult(): HResult; stdcall;

  end;
  IID_IDWriteAsyncResult = IDWriteAsyncResult;
  {$EXTERNALSYM IID_IDWriteAsyncResult}


  // DWRITE_FILE_FRAGMENT represents a range of bytes in a font file.
  PDWRITE_FILE_FRAGMENT = ^DWRITE_FILE_FRAGMENT;
  DWRITE_FILE_FRAGMENT = record

    // Starting offset of the fragment from the beginning of the file.
    fileOffset: UINT64;

    // Size of the file fragment, in bytes.
    fragmentSize: UINT64;
  end;
  {$EXTERNALSYM DWRITE_FILE_FRAGMENT}



  // Interface IDWriteRemoteFontFileStream
  // =====================================
  // IDWriteRemoteFontFileStream represents a font file stream parts of which may be
  // non-local. Non-local data must be downloaded before it can be accessed using
  // ReadFragment. The interface exposes methods to download font data and query the
  // locality of font data.
  // <remarks>
  // For more information, see the description of IDWriteRemoteFontFileLoader.
  // </remarks>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteRemoteFontFileStream);'}
  {$EXTERNALSYM IDWriteRemoteFontFileStream}
  IDWriteRemoteFontFileStream = interface(IDWriteFontFileStream)
  ['{4DB3757A-2C72-4ED9-B2B6-1ABABE1AFF9C}']

    // GetLocalFileSize returns the number of bytes of the font file that are
    // currently local, which should always be less than or equal to the full
    // file size returned by GetFileSize. If the locality is remote, the return
    // value is zero. If the file is fully local, the return value must be the
    // same as GetFileSize.
    // param name: localFileSize: Receives the local size of the file.
    // returns
    // Standard HRESULT error code.
    function GetLocalFileSize(out localFileSize: UINT64): HResult; stdcall;

    // GetFileFragmentLocality returns information about the locality of a byte range (i.e.,
    // font fragment) within the font file stream.
    // param name: fileOffset: Offset of the fragment from the beginning of the font file.
    // param name: fragmentSize: Size of the fragment in bytes.
    // param name: isLocal: Receives TRUE if the first byte of the fragment is local, FALSE if not.
    // param name: partialSize: Receives the number of contiguous bytes from the start of the
    // fragment that have the same locality as the first byte.
    // returns
    // Standard HRESULT error code.
    function GetFileFragmentLocality(fileOffset: UINT64;
                                     fragmentSize: UINT64;
                                     out isLocal: BOOL;
                                     out partialSize: UINT64): HResult; stdcall;

    // Gets the current locality of the file.
    // returns
    // Returns the locality enumeration (i.e., remote, partial, or local).
    function GetLocality(): DWRITE_LOCALITY; stdcall;

    // BeginDownload begins downloading all or part of the font file.
    // param name: fileFragments: Array of structures, each specifying a byte
    // range to download.
    // param name: fragmentCount: Number of elements in the fileFragments array.
    // This can be zero to just download file information, such as the size.
    // param name: asyncResult: Receives an object that can be used to wait for
    // the asynchronous download to complete and to get the download result upon
    // completion. The result may be NULL if the download completes synchronously.
    // For example, this can happen if method determines that the requested data
    // is already local.
    // returns
    // Standard HRESULT error code.
    function BeginDownload(downloadOperationID: TGuid;
                           fileFragments: DWRITE_FILE_FRAGMENT;
                           fragmentCount: UINT32;
                           asyncResult: IDWriteAsyncResult): HResult; stdcall;

  end;
  IID_IDWriteRemoteFontFileStream = IDWriteRemoteFontFileStream;
  {$EXTERNALSYM IID_IDWriteRemoteFontFileStream}


  // Interface IDWriteRemoteFontFileLoader
  // =====================================
  // The IDWriteRemoteFontFileLoader interface represents a font file loader that can access
  // remote (i.e., downloadable) fonts. The IDWriteFactory5.CreateHttpFontFileLoader method
  // returns an instance of this interface, or a client can create its own implementation.
  // remarks
  //   Calls to a remote file loader or stream should never block waiting for network operations.
  //   Any call that cannot succeeded immediately using local (e.g., cached) must should return
  //   DWRITE_E_REMOTEFONT. This error signifies to DWrite that it should add requests to the
  //   font download queue.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteRemoteFontFileLoader);'}
  {$EXTERNALSYM IDWriteRemoteFontFileLoader}
  IDWriteRemoteFontFileLoader = interface(IDWriteFontFileLoader)
  ['{68648C83-6EDE-46C0-AB46-20083A887FDE}']

    // Creates a remote font file stream object that encapsulates an open file resource
    // and can be used to download remote file data.
    // param name: fontFileReferenceKey: Font file reference key that uniquely identifies the font file resource
    // within the scope of the font loader being used.
    // param name: fontFileReferenceKeySize: Size of font file reference key in bytes.
    // param name: fontFileStream: Pointer to the newly created font file stream.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // Unlike CreateStreamFromKey, this method can be used to create a stream for a remote file. If the file is
    // remote, the returned stream's BeginDownload method can be used to download all or part of the font file.
    // However, the stream cannot be used to get the file size or access font data unless the file is at least
    // partially local.
    // </remarks>
    function CreateRemoteStreamFromKey(fontFileReferenceKey: Pointer;
                                       fontFileReferenceKeySize: UINT32;
                                       out fontFileStream: IDWriteRemoteFontFileStream): HResult; stdcall;

    // Gets the locality of the file resource identified by the unique key.
    // param name: fontFileReferenceKey: Font file reference key that uniquely identifies the font file resource
    // within the scope of the font loader being used.
    // param name: fontFileReferenceKeySize: Size of font file reference key in bytes.
    // param name: locality: Locality of the file.
    // returns
    // Standard HRESULT error code.
    function GetLocalityFromKey(fontFileReferenceKey: Pointer;
                                fontFileReferenceKeySize: UINT32;
                                out locality: DWRITE_LOCALITY): HResult; stdcall;

    // Creates a font file reference from a URL if the loader supports this capability.
    // param name: factory: Factory used to create the font file reference.
    // param name: baseUrl: Optional base URL. The base URL is used to resolve the fontFileUrl
    // if it is relative. For example, the baseUrl might be the URL of the referring document
    // that contained the fontFileUrl.
    // param name: fontFileUrl: URL of the font resource.
    // param name: fontFile: Receives a pointer to the newly created font file reference.
    // returns
    // Standard HRESULT error code, or E_NOTIMPL if the loader does not implement this method.
    function CreateFontFileReferenceFromUrl(factory: IDWriteFactory;
                                            baseUrl: WideChar;
                                            fontFileUrl: WideChar;
                                            out fontFile: IDWriteFontFile): HResult; stdcall;

  end;
  IID_IDWriteRemoteFontFileLoader = IDWriteRemoteFontFileLoader;
  {$EXTERNALSYM IID_IDWriteRemoteFontFileLoader}



  // Interface IDWriteInMemoryFontFileLoader
  // =======================================
  // The IDWriteInMemoryFontFileLoader interface enables clients to reference
  // in-memory fonts without having to implement a custom loader. The
  // IDWriteFactory5.CreateInMemoryFontFileLoader method returns an instance
  // of this interface, which the client is responsible for registering and
  // unregistering using IDWriteFactory.RegisterFontFileLoader and
  // IDWriteFactory.UnregisterFontFileLoader.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteInMemoryFontFileLoader);'}
  {$EXTERNALSYM IDWriteInMemoryFontFileLoader}
  IDWriteInMemoryFontFileLoader = interface(IDWriteFontFileLoader)
  ['{DC102F47-A12D-4B1C-822D-9E117E33043F}']

    // The CreateInMemoryFontFileReference method creates a font file reference
    // (IDWriteFontFile object) from an array of bytes. The font file reference
    // is bound to the IDWriteInMemoryFontFileLoader instance with which it was
    // created and remains valid for as long as that loader is registered with
    // the factory.
    // param name: factory: Factory object used to create the font file reference.
    // param name: fontData: Pointer to a memory block containing the font data.
    // param name: fontDataSize: Size of the font data.
    // param name: ownerObject: Optional object that owns the memory specified by
    // the fontData parameter. If this parameter is not NULL, the method stores a
    // pointer to the font data and adds a reference to the owner object. The
    // fontData pointer must remain valid until the owner object is released. If
    // this parameter is NULL, the method makes a copy of the font data.
    // param name: fontFile: Receives a pointer to the newly-created font file
    // reference.
    // returns
    // Standard HRESULT error code.
    function CreateInMemoryFontFileReference(factory: IDWriteFactory;
                                             fontData: Pointer;
                                             fontDataSize: UINT32;
                                             ownerObject: IUnknown;
                                             out fontFile: IDWriteFontFile): HResult; stdcall;

    // The GetFileCount method returns the number of font file references that
    // have been created using this loader instance.
    function GetFileCount(): UINT32; stdcall;

  end;
  IID_IDWriteInMemoryFontFileLoader = IDWriteInMemoryFontFileLoader;
  {$EXTERNALSYM IID_IDWriteInMemoryFontFileLoader}


  // Interface IDWriteFactory5
  // =========================
  // The root factory interface for all DWrite objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory5);'}
  {$EXTERNALSYM IDWriteFactory5}
  IDWriteFactory5 = interface(IDWriteFactory4)
  ['{958DB99A-BE2A-4F09-AF7D-65189803D1D3}']

    // Creates an empty font set builder to add font face references
    // and create a custom font set.
    // param name: fontSetBuilder: Receives a pointer to the newly created font set builder object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontSetBuilder(out fontSetBuilder: IDWriteFontSetBuilder1): HResult; stdcall;

    // The CreateInMemoryFontFileLoader method creates a loader object that can
    // be used to create font file references to in-memory fonts. The caller is
    // responsible for registering and unregistering the loader.
    // param name: newLoader: Receives a pointer to the newly-created loader object.
    // returns
    // Standard HRESULT error code.
    function CreateInMemoryFontFileLoader(out newLoader: IDWriteInMemoryFontFileLoader): HResult; stdcall;

    // The CreateHttpFontFileLoader function creates a remote font file loader
    // that can create font file references from HTTP or HTTPS URLs. The caller
    // is responsible for registering and unregistering the loader.
    // param name: referrerUrl: Optional referrer URL for HTTP requests.
    // param name: extraHeaders: Optional additional header fields to include
    // in HTTP requests. Each header field consists of a name followed by a colon
    // (":") and the field value, as specified by RFC 2616. Multiple header fields
    // may be separated by newlines.
    // param name: newLoader: Receives a pointer to the newly-created loader object.
    // returns
    // Standard HRESULT error code.
    function CreateHttpFontFileLoader(referrerUrl: WideChar;
                                      extraHeaders: WideChar;
                                      out newLoader: IDWriteRemoteFontFileLoader): HResult; stdcall;

    // The AnalyzeContainerType method analyzes the specified file data to determine
    // whether it is a known font container format (e.g., WOFF or WOFF2).
    // returns
    // Returns the container type if recognized. DWRITE_CONTAINER_TYPE_UNKOWNN is
    // returned for all other files, including uncompressed font files.
    function AnalyzeContainerType(fileData: Pointer;
                                  fileDataSize: UINT32): DWRITE_CONTAINER_TYPE; stdcall;

    // The UnpackFontFile method unpacks font data from a container file (WOFF or
    // WOFF2) and returns the unpacked font data in the form of a font file stream.
    // param name: containerType: Container type returned by AnalyzeContainerType.
    // param name: fileData: Pointer to the compressed data.
    // param name: fileDataSize: Size of the compressed data, in bytes.
    // param name: unpackedFontStream: Receives a pointer to a newly created font
    // file stream containing the uncompressed data.
    // returns
    // Standard HRESULT error code. The return value is E_INVALIDARG if the container
    // type is DWRITE_CONTAINER_TYPE_UNKNOWN.
    function UnpackFontFile(containerType: DWRITE_CONTAINER_TYPE;
                            fileData: Pointer;
                            fileDataSize: UINT32;
                            out unpackedFontStream: IDWriteFontFileStream): HResult; stdcall;

  end;
  IID_IDWriteFactory5 = IDWriteFactory5;
  {$EXTERNALSYM IID_IDWriteFactory5}

// #endif // NTDDI_VERSION >= NTDDI_WIN10_RS2


///////////////////////////////////////////////////////////////////


// #if NTDDI_VERSION >= NTDDI_WIN10_RS3

  PIDWriteFontResource = ^IDWriteFontResource;
  IDWriteFontResource = interface;
  PIDWriteFontFace5 = ^IDWriteFontFace5;
  IDWriteFontFace5 = interface;
  PIDWriteFontFaceReference1 = ^IDWriteFontFaceReference1;
  IDWriteFontFaceReference1 = interface;
  PIDWriteFontSet1 = ^IDWriteFontSet1;
  IDWriteFontSet1 = interface;
  PIDWriteFontCollection2 = ^IDWriteFontCollection2;
  IDWriteFontCollection2 = interface;
  PIDWriteTextFormat3 = ^IDWriteTextFormat3;
  IDWriteTextFormat3 = interface;
  PIDWriteFontSetBuilder2 = ^IDWriteFontSetBuilder2;
  IDWriteFontSetBuilder2 = interface;


  // Four character identifier for a font axis.
  // remarks
  //   Use funtion DwriteMakeFontAxisTag() to create a custom one.
  DWRITE_FONT_AXIS_TAG = UINT32;
  {$EXTERNALSYM DWRITE_FONT_AXIS_TAG}


  // Value for a font axis, used when querying and creating font instances.
  PDWRITE_FONT_AXIS_VALUE = ^DWRITE_FONT_AXIS_VALUE;
  DWRITE_FONT_AXIS_VALUE = record
    // Four character identifier of the font axis (weight, width, slant, italic...).
    axisTag: DWRITE_FONT_AXIS_TAG;

    // Value for the given axis, with the meaning and range depending on the axis semantics.
    // Certain well known axes have standard ranges and defaults, such as weight (1..1000, default=400),
    // width (>0, default=100), slant (-90..90, default=-20), and italic (0 or 1).
    value: Single;
  end;
  {$EXTERNALSYM DWRITE_FONT_AXIS_VALUE}


  // Minimum and maximum range of a font axis.
  PDWRITE_FONT_AXIS_RANGE = ^DWRITE_FONT_AXIS_RANGE;
  DWRITE_FONT_AXIS_RANGE = record

    // Four character identifier of the font axis (weight, width, slant, italic...).
    axisTag: DWRITE_FONT_AXIS_TAG;

    // Minimum value supported by this axis.
    minValue: Single;

    // Maximum value supported by this axis. The maximum can equal the minimum.
    maxValue: Single;
  end;
  {$EXTERNALSYM DWRITE_FONT_AXIS_RANGE}


  // Interface IDWriteFactory6
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory6);'}
  {$EXTERNALSYM IDWriteFactory6}
  IDWriteFactory6 = interface(IDWriteFactory5)
  ['{F3744D80-21F7-42EB-B35D-995BC72FC223}']

    // Creates a reference to a specific font instance within a file.
    // param name: fontFile: User provided font file representing the font face.
    // param name: faceIndex: The zero based index of a font face in cases when the font files contain a collection of font faces.
    // If the font files contain a single face, this value should be zero.
    // param name: fontSimulations: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: fontFaceReference: Receives a pointer to the newly created font face reference object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontFaceReference(fontFile: IDWriteFontFile;
                                     faceIndex: UINT32;
                                     fontSimulations: DWRITE_FONT_SIMULATIONS;
                                     fontAxisValues: DWRITE_FONT_AXIS_VALUE;
                                     fontAxisValueCount: UINT32;
                                     out fontFaceReference: IDWriteFontFaceReference1): HResult; stdcall;

    // Creates a font resource given a font file and face index.
    // param name: fontFile: User provided font file representing the font face.
    // param name: faceIndex: The zero based index of a font face in cases when the font files contain a collection of font faces.
    // If the font files contain a single face, this value should be zero.
    // param name: fontResource: Receives a pointer to the newly created font resource object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontResource(fontFile: IDWriteFontFile;
                                faceIndex: UINT32;
                                out fontResource: IDWriteFontResource): HResult; stdcall;

    // Retrieves the set of system fonts.
    // param name: includeDownloadableFonts: Include downloadable fonts or only locally installed ones.
    // param name: fontSet: Receives a pointer to the font set object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontSet(includeDownloadableFonts: BOOL;
                              out fontSet: IDWriteFontSet1): HResult; stdcall;

    // Retrieves a collection of fonts grouped into families.
    // param name: includeDownloadableFonts: Include downloadable fonts or only locally installed ones.
    // param name: fontFamilyModel: How to group families in the collection.
    // param name: fontCollection: Receives a pointer to the font collection object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontCollection(includeDownloadableFonts: BOOL;
                                     fontFamilyModel: DWRITE_FONT_FAMILY_MODEL;
                                     out fontCollection: IDWriteFontCollection2): HResult; stdcall;

    // Create a collection of fonts grouped into families from a font set.
    // param name: fontSet: A set of fonts to use to build the collection.
    // param name: fontFamilyModel: How to group families in the collection.
    // param name: fontCollection: Receives a pointer to the newly created font collection object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontCollectionFromFontSet(fontSet: IDWriteFontSet;
                                             fontFamilyModel: DWRITE_FONT_FAMILY_MODEL;
                                             out fontCollection: IDWriteFontCollection2): HResult; stdcall;

    // Creates an empty font set builder to add font instances and create a custom font set.
    // param name: fontSetBuilder: Receives a pointer to the newly created font set builder object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateFontSetBuilder(out fontSetBuilder: IDWriteFontSetBuilder2): HResult; stdcall;

    // Create a text format object used for text layout.
    // param name: fontFamilyName: Name of the font family from the collection.
    // param name: fontCollection: Font collection, with nullptr indicating the system font collection.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: fontSize: Logical size of the font in DIP units.
    // param name: localeName: Locale name (e.g. "ja-JP", "en-US", "ar-EG").
    // param name: textFormat: Receives a pointer to the newly created text format object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    // If fontCollection is nullptr, the system font collection is used, grouped by typographic family name
    // (DWRITE_FONT_FAMILY_MODEL_TYPOGRAPHIC) without downloadable fonts.
    function CreateTextFormat(fontFamilyName: WideChar;
                              fontCollection: IDWriteFontCollection;
                              fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                              fontAxisValueCount: UINT32;
                              fontSize: Single;
                              localeName: WideChar;
                              out textFormat: IDWriteTextFormat3): HResult; stdcall;

  end;
  IID_IDWriteFactory6 = IDWriteFactory6;
  {$EXTERNALSYM IID_IDWriteFactory6}


  // Interface IDWriteFontFace5
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace5);'}
  {$EXTERNALSYM IDWriteFontFace5}
  IDWriteFontFace5 = interface(IDWriteFontFace4)
  ['{98EFF3A5-B667-479A-B145-E2FA5B9FDC29}']

    // Get the number of axes defined by the font. This includes both static and variable axes.
    function GetFontAxisValueCount(): UINT32; stdcall;

    // Get the list of axis values used by the font.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Maximum number of font axis values to write.
    // returns
    // Standard HRESULT error code, or E_INVALIDARG if fontAxisValueCount doesn't match GetFontAxisValueCount.
    // remarks
    // The values are returned in the canonical order defined by the font, clamped to the actual range supported,
    // not specifically the same axis value array passed to CreateFontFace.
    function GetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                               fontAxisValueCount: UINT32): HResult; stdcall;

    // Whether this font's resource supports any variable axes. When true, at least one DWRITE_FONT_AXIS_RANGE
    // in the font resource has a non-empty range (maximum > minimum).
    function HasVariations(): BOOL; stdcall;

    // Get the underlying font resource for this font face. A caller can use that to query information on the resource
    // or recreate a new font face instance with different axis values.
    // param name: fontResource: Newly created font resource object.
    // returns
    // Standard HRESULT error code.
    function GetFontResource(out fontResource: IDWriteFontResource): HResult; stdcall;

    // Compares two instances of a font face for equality.
    function Equals(fontFace: IDWriteFontFace): BOOL; stdcall;

  end;
  IID_IDWriteFontFace5 = IDWriteFontFace5;
  {$EXTERNALSYM IID_IDWriteFontFace5}


  // Interface IDWriteFontResource
  // =============================
  // Interface to return axis information for a font resource and create specific font face instances.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontResource);'}
  {$EXTERNALSYM IDWriteFontResource}
  IDWriteFontResource = interface(IUnknown)
  ['{1F803A76-6871-48E8-987F-B975551C50F2}']

    // Get the font file of the resource.
    // param name: fontFile: Receives a pointer to the font file.
    // returns
    // Standard HRESULT error code.
    function GetFontFile(out fontFile: IDWriteFontFile): HResult; stdcall;

    // Obtains the zero-based index of the font face in its font file. If the font files contain a single face,
    // the return value is zero.
    function GetFontFaceIndex(): UINT32; stdcall;

    // Get the number of axes supported by the font resource. This includes both static and variable axes.
    function GetFontAxisCount(): UINT32; stdcall;

    // Get the default values for all axes supported by the font resource.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Maximum number of font axis values to write.
    // remarks
    //   Different font resources may have different defaults.
    //   For OpenType 1.8 fonts, these values come from the STAT and fvar tables.
    //   For older fonts without a STAT table, weight-width-slant-italic are read from the OS/2 table.
    // returns
    // Standard HRESULT error code, or E_INVALIDARG if fontAxisValueCount doesn't match GetFontAxisCount.
    function GetDefaultFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                                      fontAxisValueCount: UINT32): HResult; stdcall;

    // Get ranges of each axis.
    // param name: fontAxisRanges:
    // param name: fontAxisRangeCount: Total number of axis ranges
    // returns
    // Standard HRESULT error code, or E_INVALIDARG if fontAxisRangeCount doesn't match GetFontAxisCount.
    // remarks
    // Non-varying axes will have empty ranges (minimum==maximum).
    function GetFontAxisRanges(fontAxisRanges: PDWRITE_FONT_AXIS_RANGE;
                               fontAxisRangeCount: UINT32): HResult; stdcall;

    // Gets attributes about the given axis, such as whether the font author recommends to hide the axis
    // in user interfaces.
    // param name: axisIndex: Font axis, from 0 to GetFontAxisValueCount - 1.
    // param name: axisAttributes: Receives the attributes for the given axis.
    // returns
    // Attributes for a font axis, or NONE if axisIndex is beyond the font count.
    function GetFontAxisAttributes(axisIndex: UINT32): DWRITE_FONT_AXIS_ATTRIBUTES; stdcall;

    // Gets the localized names of a font axis.
    // param name: axisIndex: Font axis, from 0 to GetFontAxisCount - 1.
    // param name: names: Receives a pointer to the newly created localized strings object.
    // remarks
    //   The font author may not have supplied names for some font axes. The localized strings
    //   will be empty in that case.
    // returns
    // Standard HRESULT error code.
    function GetAxisNames(axisIndex: UINT32;
                          out names: IDWriteLocalizedStrings): HResult; stdcall;

    // Get the number of named values for a specific axis.
    // param name: axisIndex: Font axis, from 0 to GetFontAxisCount - 1.
    // returns
    // Number of named values.
    function GetAxisValueNameCount(axisIndex: UINT32): UINT32; stdcall;

    // Gets the localized names of specific values for a font axis.
    // param name: axisIndex: Font axis, from 0 to GetFontAxisCount - 1.
    // param name: axisValueIndex: Value index, from 0 to GetAxisValueNameCount - 1.
    // param name: fontAxisRange: Range of the named value.
    // param name: names: Receives a pointer to the newly created localized strings object.
    // remarks
    //   The font author may not have supplied names for some font axis values. The localized strings
    //   will be empty in that case. The range may be a single point, where minimum == maximum.
    //   All ranges are in ascending order by axisValueIndex.
    // returns
    // Standard HRESULT error code.
    function GetAxisValueNames(axisIndex: UINT32;
                               axisValueIndex: UINT32;
                               out fontAxisRange: PDWRITE_FONT_AXIS_RANGE;
                               out names: IDWriteLocalizedStrings): HResult; stdcall;

    // Whether this font's resource supports any variable axes. When true, at least one DWRITE_FONT_AXIS_RANGE
    // in the font resource has a non-empty range (maximum > minimum).
    function HasVariations(): BOOL; stdcall;

    // Creates a font face instance with specific axis values.
    // param name: fontSimulations: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: fontFace: Receives a pointer to the newly created font face object, or nullptr on failure.
    // remarks
    //   The passed input axis values are permitted to be a subset or superset of all the ones actually supported by
    //   the font. Any unspecified axes use their default values, values beyond the ranges are clamped, and any
    //   non-varying axes have no effect.
    // returns
    // Standard HRESULT error code, or DWRITE_E_REMOTEFONT if the face is not local.
    function CreateFontFace(fontSimulations: DWRITE_FONT_SIMULATIONS;
                            fontAxisValues: DWRITE_FONT_AXIS_VALUE;
                            fontAxisValueCount: UINT32;
                            out fontFace: IDWriteFontFace5): HResult; stdcall;

    // Creates a font face reference with specific axis values.
    // param name: fontSimulations: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: fontFaceReference: Receives a pointer to the newly created font face reference object, or nullptr on failure.
    // remarks
    //   The passed input axis values are permitted to be a subset or superset of all the ones actually supported by
    //   the font. Any unspecified axes use their default values, values beyond the ranges are clamped, and any
    //   non-varying axes have no effect.
    // returns
    // Standard HRESULT error code.
    function CreateFontFaceReference(fontSimulations: DWRITE_FONT_SIMULATIONS;
                                     fontAxisValues: DWRITE_FONT_AXIS_VALUE;
                                     fontAxisValueCount: UINT32;
                                     out fontFaceReference: IDWriteFontFaceReference1): HResult; stdcall;

  end;
  IID_IDWriteFontResource = IDWriteFontResource;
  {$EXTERNALSYM IID_IDWriteFontResource}


  // Interface IDWriteFontFaceReference1
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFaceReference1);'}
  {$EXTERNALSYM IDWriteFontFaceReference1}
  IDWriteFontFaceReference1 = interface(IDWriteFontFaceReference)
  ['{C081FE77-2FD1-41AC-A5A3-34983C4BA61A}']

    // Creates a font face from the reference for use with layout, shaping, or rendering.
    // param name: fontFace: Newly created font face object, or nullptr in the case of failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    // This function can fail with DWRITE_E_REMOTEFONT if the font is not local.
    function CreateFontFace(out fontFace: IDWriteFontFace5): HResult; stdcall;

    // Get the number of axes specified by the reference.
    function GetFontAxisValueCount(): UINT32; stdcall;

    // Get the list of font axis values specified by the reference.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // returns
    // Standard HRESULT error code, or E_INVALIDARG if fontAxisValueCount doesn't match GetFontAxisValueCount.
    function GetFontAxisValues(out fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                               fontAxisValueCount: UINT32): HResult; stdcall;

  end;
  IID_IDWriteFontFaceReference1 = IDWriteFontFaceReference1;
  {$EXTERNALSYM IID_IDWriteFontFaceReference1}


  // Interface IDWriteFontSetBuilder2
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSetBuilder2);'}
  {$EXTERNALSYM IDWriteFontSetBuilder2}
  IDWriteFontSetBuilder2 = interface(IDWriteFontSetBuilder1)
  ['{EE5BA612-B131-463C-8F4F-3189B9401E45}']

    // Adds a font to the set being built, with the caller supplying enough information to search on
    // and determine axis ranges, avoiding the need to open the potentially non-local font.
    // param name: fontFile: Font file reference object to add to the set.
    // param name: faceIndex: The zero based index of a font face in a collection.
    // param name: fontSimulations: Font face simulation flags for algorithmic emboldening and italicization.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: fontAxisRanges: List of axis ranges.
    // param name: fontAxisRangeCount: Number of axis ranges.
    // param name: properties: List of properties to associate with the reference.
    // param name: propertyCount: How many properties are defined.
    // returns
    // Standard HRESULT error code.
    // remarks
    // The font properties should include at least a family (typographic or weight/style/stretch).
    // Otherwise the font would be accessible in the IDWriteFontSet only by index, not name.
    function AddFont(fontFile: IDWriteFontFile;
                     fontFaceIndex: UINT32;
                     fontSimulations: DWRITE_FONT_SIMULATIONS;
                     fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                     fontAxisValueCount: UINT32;
                     fontAxisRanges: PDWRITE_FONT_AXIS_RANGE;
                     fontAxisRangeCount: UINT32;
                     properties: PDWRITE_FONT_PROPERTY;
                     propertyCount: UINT32): HResult; stdcall;

    // Adds references to all the fonts in the specified font file. The method
    // parses the font file to determine the fonts and their properties.
    // param name: filePath: Absolute file path to add to the font set.
    // returns
    // Standard HRESULT error code.
    function AddFontFile(filePath: WideChar): HResult; stdcall;

  end;
  IID_IDWriteFontSetBuilder2 = IDWriteFontSetBuilder2;
  {$EXTERNALSYM IID_IDWriteFontSetBuilder2}


  // Interface IDWriteFontSet1
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSet1);'}
  {$EXTERNALSYM IDWriteFontSet1}
  IDWriteFontSet1 = interface(IDWriteFontSet)
  ['{7E9FDA85-6C92-4053-BC47-7AE3530DB4D3}']

    // Generates a matching font set based on the requested inputs, ordered so that nearer matches are earlier.
    // param name: property: Font property of interest, such as typographic family or weight/stretch/style family.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: matchingSet: Prioritized list of fonts that match the properties, or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    // This can yield distinct items that were not in the original font set, including items with simulation flags
    // (if they would be a closer match to the request) and instances that were not named by the font author.
    // Items from the same font resources are collapsed into one, the closest possible match.
    function GetMatchingFonts(fontProperty: PDWRITE_FONT_PROPERTY;
                              fontAxisValues: DWRITE_FONT_AXIS_VALUE;
                              fontAxisValueCount: UINT32;
                              out matchingFonts: IDWriteFontSet1): HResult; stdcall;

    // Returns a font set that contains only the first occurrence of each font resource in the given set.
    // param name: fontSet: New font set consisting of single default instances from font resources.
    // returns
    // Standard HRESULT error code.
    function GetFirstFontResources(out filteredFontSet: IDWriteFontSet1): HResult; stdcall;

    // Returns a subset of fonts filtered by the given properties.
    // param name: properties: List of properties to filter using.
    // param name: propertyCount: How many properties to filter.
    // param name: selectAnyProperty: Select any property rather rather than the intersection of them all.
    // param name: filteredSet: Subset of fonts that match the properties, or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    //   If no fonts matched the filter, the returned subset will be empty (GetFontCount returns 0).
    //   The subset will always be equal to or less than the original set.
    function GetFilteredFonts(properties: PDWRITE_FONT_PROPERTY;
                              propertyCount: UINT32;
                              selectAnyProperty: BOOL;
                              out filteredFontSet: IDWriteFontSet1): HResult; overload; stdcall;

    // Returns a subset of fonts filtered by the given ranges, endpoint-inclusive.
    // param name: fontAxisRanges: List of axis ranges.
    // param name: fontAxisRangeCount: Number of axis ranges.
    // param name: selectAnyRange: Select any range rather rather than the intersection of them all.
    // param name: filteredSet: Subset of fonts that fall within the ranges, or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    //   If no fonts matched the filter, the subset will be empty (GetFontCount returns 0), but the function does not
    //   return an error. The subset will always be equal to or less than the original set.
    function GetFilteredFonts(fontAxisRanges: DWRITE_FONT_AXIS_RANGE;
                              fontAxisRangeCount: UINT32;
                              selectAnyRange: BOOL;
                              out filteredFontSet: IDWriteFontSet1): HResult; overload; stdcall;

    // Returns a subset of fonts filtered by the given indices.
    // param name: indices: Array of indices, each index from [0..GetFontCount() - 1].
    // param name: indexCount: Number of indices.
    // param name: filteredSet: Subset of fonts that come from the given indices, or nullptr on failure.
    // returns
    // Standard HRESULT error code.
    // remarks
    //   The indices can come in any order, meaning this function can produce a new set with items removed, duplicated,
    //   or reordered from the original. If zero indices were passed, an empty font set is returned.
    function GetFilteredFonts(indices: UINT32;
                              indexCount: UINT32;
                              out filteredFontSet: IDWriteFontSet1): HResult; overload; stdcall;

    // Get all the item indices filtered by the given properties.
    // param name: properties: List of properties to filter using.
    // param name: propertyCount: How many properties to filter.
    // param name: selectAnyProperty: Select any property rather rather than the intersection of them all.
    // param name: indices: Ascending array of indices [0..GetFontCount() - 1].
    // param name: indexCount: Number of indices.
    // param name: actualIndexCount: Actual number of indices written or needed [0..GetFontCount()-1].
    // returns
    // E_NOT_SUFFICIENT_BUFFER if the buffer is too small, with actualIndexCount set to the needed size.
    // The actualIndexCount will always be <= IDwriteFontSet.GetFontCount.
    function GetFilteredFontIndices(properties: DWRITE_FONT_PROPERTY;
                                    propertyCount: UINT32;
                                    selectAnyProperty: BOOL;
                                    out indices: UINT32;
                                    maxIndexCount: UINT32;
                                    out actualIndexCount: UINT32): HResult; overload; stdcall;

    // Get all the item indices filtered by the given ranges.
    // param name: fontAxisRanges: List of axis ranges.
    // param name: fontAxisRangeCount: Number of axis ranges.
    // param name: selectAnyRange: Select any property rather rather than the intersection of them all.
    // param name: indices: Ascending array of indices [0..GetFontCount() - 1].
    // param name: indexCount: Number of indices.
    // param name: actualIndexCount: Actual number of indices written or needed [0..GetFontCount()-1].
    // returns
    // E_NOT_SUFFICIENT_BUFFER if the buffer is too small, with actualIndexCount set to the needed size.
    function GetFilteredFontIndices(fontAxisRanges: DWRITE_FONT_AXIS_RANGE;
                                    fontAxisRangeCount: UINT32;
                                    selectAnyRange: BOOL;
                                    out indices: UINT32;
                                    maxIndexCount: UINT32;
                                    out actualIndexCount: UINT32): HResult; overload; stdcall;

    // Gets all axis ranges in the font set, the union of all contained items.
    // param name: fontAxisRanges: List of axis ranges.
    // param name: fontAxisRangeCount: Number of axis ranges.
    // param name: actualFontAxisRangeCount: Actual number of axis ranges written or needed.
    // returns
    // E_NOT_SUFFICIENT_BUFFER if the buffer is too small, with actualFontAxisRangeCount set to the needed size.
    function GetFontAxisRanges(fontAxisRanges: PDWRITE_FONT_AXIS_RANGE;
                               maxFontAxisRangeCount: UINT32;
                               out actualFontAxisRangeCount: UINT32): HResult; overload; stdcall;

    // Get the axis ranges of a single item.
    // param name: listIndex: Zero-based index of the font in the set.
    // param name: fontAxisRanges: List of axis ranges.
    // param name: fontAxisRangeCount: Number of axis ranges.
    // param name: actualFontAxisRangeCount: Actual number of axis ranges written or needed.
    // returns
    // E_NOT_SUFFICIENT_BUFFER if the buffer is too small, with actualFontAxisRangeCount set to the needed size.
    function GetFontAxisRanges(listIndex: UINT32;
                               out fontAxisRanges: PDWRITE_FONT_AXIS_RANGE;
                               maxFontAxisRangeCount: UINT32;
                               out actualFontAxisRangeCount: PUINT32): HResult; overload; stdcall;

    // Get the font face reference of a single item.
    // param name: listIndex: Zero-based index of the font item in the set.
    // param name: fontFaceReference: Receives a pointer to the font face reference.
    // returns
    // Standard HRESULT error code.
    function GetFontFaceReference(listIndex: UINT32;
                                  out fontFaceReference: IDWriteFontFaceReference1): HResult; stdcall;

    // Create the font resource of a single item.
    // param name: listIndex: Zero-based index of the font item in the set.
    // param name: fontResource: Receives a pointer to the font resource.
    // returns
    // Standard HRESULT error code, or DWRITE_E_REMOTEFONT if the file is not local.
    function CreateFontResource(listIndex: UINT32;
                                out fontResource: IDWriteFontResource): HResult; stdcall;

    // Create a font face for a single item (rather than going through the font face reference).
    // param name: listIndex: Zero-based index of the font item in the set.
    // param name: fontFile: Receives a pointer to the font face.
    // returns
    // Standard HRESULT error code, or DWRITE_E_REMOTEFONT if the file is not local.
    function CreateFontFace(listIndex: UINT32;
                            out fontFace: IDWriteFontFace5): HResult; stdcall;

    // Return the locality of a single item.
    // param name: listIndex: Zero-based index of the font item in the set.
    // <remarks>
    // The locality enumeration. For fully local files, the result will always
    // be DWRITE_LOCALITY_LOCAL. For downloadable files, the result depends on how
    // much of the file has been downloaded.
    // </remarks>
    // returns
    // The locality enumeration.
    function GetFontLocality(listIndex: UINT32): DWRITE_LOCALITY; stdcall;

  end;
  IID_IDWriteFontSet1 = IDWriteFontSet1;
  {$EXTERNALSYM IID_IDWriteFontSet1}


  // Interface IDWriteFontList2
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontList2);'}
  {$EXTERNALSYM IDWriteFontList2}
  IDWriteFontList2 = interface(IDWriteFontList1)
  ['{C0763A34-77AF-445A-B735-08C37B0A5BF5}']

    // Get the underlying font set used by this list.
    // param name: fontSet: Contains font set used by the list.
    // returns
    // Standard HRESULT error code.
    function GetFontSet(out fontSet: IDWriteFontSet1): HResult; stdcall;

  end;
  IID_IDWriteFontList2 = IDWriteFontList2;
  {$EXTERNALSYM IID_IDWriteFontList2}


  // Interface IDWriteFontFamily2
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFamily2);'}
  {$EXTERNALSYM IDWriteFontFamily2}
  IDWriteFontFamily2 = interface(IDWriteFontFamily1)
  ['{3ED49E77-A398-4261-B9CF-C126C2131EF3}']

    // Gets a list of fonts in the font family ranked in order of how well they match the specified axis values.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: matchingFonts: Receives a pointer to the newly created font list object.
    // returns
    // Standard HRESULT error code.
    function GetMatchingFonts(fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                              fontAxisValueCount: UINT32;
                              out matchingFonts: IDWriteFontList2): HResult; stdcall;

    // Get the underlying font set used by this family.
    // param name: fontSet: Contains font set used by the family.
    // returns
    // Standard HRESULT error code.
    function GetFontSet(out fontSet: IDWriteFontSet1): HResult; stdcall;

  end;
  IID_IDWriteFontFamily2 = IDWriteFontFamily2;
  {$EXTERNALSYM IID_IDWriteFontFamily2}


  // Interface IDWriteFontCollection2
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontCollection2);'}
  {$EXTERNALSYM IDWriteFontCollection2}
  IDWriteFontCollection2 = interface(IDWriteFontCollection1)
  ['{514039C6-4617-4064-BF8B-92EA83E506E0}']

    // Creates a font family object given a zero-based font family index.
    // param name: index: Zero-based index of the font family.
    // param name: fontFamily: Receives a pointer the newly created font family object.
    // returns
    // Standard HRESULT error code.
    function GetFontFamily(index: UINT32;
                           out fontFamily: IDWriteFontFamily2): HResult; stdcall;

    // Gets a list of fonts in the specified font family ranked in order of how well they match the specified axis values.
    // param name: familyName: Name of the font family. The name is not case-sensitive but must otherwise exactly match a family name in the collection.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: matchingFonts: Receives a pointer to the newly created font list object.
    // <remarks>
    // If no fonts matched, the list will be empty (GetFontCount returns 0),
    // but the function does not return an error.
    // </remarks>
    // returns
    // Standard HRESULT error code.
    function GetMatchingFonts(familyName: WideChar;
                              fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                              fontAxisValueCount: UINT32;
                              out fontList: IDWriteFontList2): HResult; stdcall;

    // Get the font family model used by the font collection to group families.
    // returns
    // Family model enumeration.
    function GetFontFamilyModel(): DWRITE_FONT_FAMILY_MODEL; stdcall;

    // Get the underlying font set used by this collection.
    // param name: fontSet: Contains font set used by the collection.
    // returns
    // Standard HRESULT error code.
    function GetFontSet(out fontSet: IDWriteFontSet1): HResult; stdcall;

  end;
  IID_IDWriteFontCollection2 = IDWriteFontCollection2;
  {$EXTERNALSYM IID_IDWriteFontCollection2}


  // Interface IDWriteTextLayout4
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextLayout4);'}
  {$EXTERNALSYM IDWriteTextLayout4}
  IDWriteTextLayout4 = interface(IDWriteTextLayout3)
  ['{05A9BF42-223F-4441-B5FB-8263685F55E9}']

    // Set values for font axes over a range of text.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // returns
    // Standard HRESULT error code.
    function SetFontAxisValues(fontAxisValues: DWRITE_FONT_AXIS_VALUE;
                               fontAxisValueCount: UINT32;
                               textRange: DWRITE_TEXT_RANGE): HResult; stdcall;

    // Get the number of axes set on the text position.
    function GetFontAxisValueCount(currentPosition: UINT32): UINT32; stdcall;

    // Get the list of font axis values on the text position.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Maximum number of font axis values to write.
    // returns
    // Standard HRESULT error code, or E_INVALIDARG if fontAxisValueCount doesn't match GetFontAxisValueCount.
    function GetFontAxisValues(currentPosition: UINT32;
                               out fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                               fontAxisValueCount: UINT32;
                               textRange: PDWRITE_TEXT_RANGE = Nil): HResult; stdcall;

    // Get the automatic axis options.
    // returns
    // Automatic axis options.
    function GetAutomaticFontAxes(): DWRITE_AUTOMATIC_FONT_AXES; stdcall;

    // Sets the automatic font axis options.
    // param name: automaticFontAxes: Automatic font axis options.
    // returns
    // Standard HRESULT error code.
    function SetAutomaticFontAxes(automaticFontAxes: DWRITE_AUTOMATIC_FONT_AXES): HResult; stdcall;

  end;
  IID_IDWriteTextLayout4 = IDWriteTextLayout4;
  {$EXTERNALSYM IID_IDWriteTextLayout4}


  // Interface IDWriteTextFormat3
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextFormat3);'}
  {$EXTERNALSYM IDWriteTextFormat3}
  IDWriteTextFormat3 = interface(IDWriteTextFormat2)
  ['{6D3B5641-E550-430D-A85B-B7BF48A93427}']

    // Set values for font axes of the format.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // returns
    // Standard HRESULT error code.
    function SetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                               fontAxisValueCount: UINT32): HResult; stdcall;

    // Get the number of axes set on the format.
    function GetFontAxisValueCount(): UINT32; stdcall;

    // Get the list of font axis values on the format.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Maximum number of font axis values to write.
    // returns
    // Standard HRESULT error code.
    function GetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE;
                               fontAxisValueCount: UINT32): HResult; stdcall;

    // Get the automatic axis options.
    // returns
    // Automatic axis options.
    function GetAutomaticFontAxes(): DWRITE_AUTOMATIC_FONT_AXES; stdcall;

    // Sets the automatic font axis options.
    // param name: automaticFontAxes: Automatic font axis options.
    // returns
    // Standard HRESULT error code.
    function SetAutomaticFontAxes(automaticFontAxes: DWRITE_AUTOMATIC_FONT_AXES): HResult; stdcall;

  end;
  IID_IDWriteTextFormat3 = IDWriteTextFormat3;
  {$EXTERNALSYM IID_IDWriteTextFormat3}


  // Interface IDWriteFontFallback1
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFallback1);'}
  {$EXTERNALSYM IDWriteFontFallback1}
  IDWriteFontFallback1 = interface(IDWriteFontFallback)
  ['{2397599D-DD0D-4681-BD6A-F4F31EAADE77}']

    // Determines an appropriate font to use to render the range of text.
    // param name: source: The text source implementation holds the text and locale.
    // param name: textLength: Length of the text to analyze.
    // param name: baseFontCollection: Default font collection to use.
    // param name: baseFamilyName: Family name of the base font. If you pass nullptr, no matching will be done against
    // the base family.
    // param name: fontAxisValues: List of font axis values.
    // param name: fontAxisValueCount: Number of font axis values.
    // param name: mappedLength: Length of text mapped to the mapped font. This will always be less or equal to the
    // input text length and greater than zero (if the text length is non-zero) so that the caller advances at
    // least one character each call.
    // param name: mappedFontFace: The font face that should be used to render the first mappedLength characters of the text.
    // If it returns null, then no known font can render the text, and mappedLength is the number of unsupported
    // characters to skip.
    // param name: scale: Scale factor to multiply the em size of the returned font by.
    // returns
    // Standard HRESULT error code.
    function MapCharacters(analysisSource: IDWriteTextAnalysisSource;
                           textPosition: UINT32;
                           textLength: UINT32;
                           baseFontCollection: PIDWriteFontCollection;
                           baseFamilyName: PWideChar;
                           fontAxisValues: DWRITE_FONT_AXIS_VALUE;
                           fontAxisValueCount: UINT32;
                           mappedLength: UINT32;
                           out scale: Single;
                           out mappedFontFace: IDWriteFontFace5): HResult; stdcall;

  end;
  IID_IDWriteFontFallback1 = IDWriteFontFallback1;
  {$EXTERNALSYM IID_IDWriteFontFallback1}

// #endif // NTDDI_VERSION >= NTDDI_WIN10_RS3


// #if NTDDI_VERSION >= NTDDI_WIN10_RS4

  // Interface IDWriteFontSet2
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSet2);'}
  {$EXTERNALSYM IDWriteFontSet2}
  IDWriteFontSet2 = interface(IDWriteFontSet1)
  ['{DC7EAD19-E54C-43AF-B2DA-4E2B79BA3F7F}']

    // Gets the expiration event for the font set, if any. The expiration event is set on a system font set object if
    // it is out of date due to fonts being installed, uninstalled, or updated. The client should handle the event by
    // getting a new system font set.
    // returns
    // Returns an event handle if called on the system font set, or nullptr if called on a custom font set.
    // <remarks>
    // The client must not call CloseHandle on the returned event handle. The handle is owned by the font set
    // object, and remains valid as long as the client holds a reference to the font set. The client can wait
    // on the returned event or use RegisterWaitForSingleObject to request a callback when the event is set.
    // </remarks>
    function GetExpirationEvent(): THandle; stdcall;

  end;
  IID_IDWriteFontSet2 = IDWriteFontSet2;
  {$EXTERNALSYM IID_IDWriteFontSet2}


  // Interface IDWriteFontCollection3
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontCollection3);'}
  {$EXTERNALSYM IDWriteFontCollection3}
  IDWriteFontCollection3 = interface(IDWriteFontCollection2)
  ['{A4D055A6-F9E3-4E25-93B7-9E309F3AF8E9}']

    // Gets the expiration event for the font collection, if any. The expiration event is set on a system font
    // collection object if it is out of date due to fonts being installed, uninstalled, or updated. The client
    // should handle the event by getting a new system font collection.
    // returns
    // Returns an event handle if called on the system font collection, or nullptr if called on a custom font
    // collection.
    // <remarks>
    // The client must not call CloseHandle on the returned event handle. The handle is owned by the font collection
    // object, and remains valid as long as the client holds a reference to the font collection. The client can wait
    // on the returned event or use RegisterWaitForSingleObject to request a callback when the event is set.
    // </remarks>
    function GetExpirationEvent(): THandle; stdcall;

  end;
  IID_IDWriteFontCollection3 = IDWriteFontCollection3;
  {$EXTERNALSYM IID_IDWriteFontCollection3}


  // Interface IDWriteFactory7
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory7);'}
  {$EXTERNALSYM IDWriteFactory7}
  IDWriteFactory7 = interface(IDWriteFactory6)
  ['{35D0E0B3-9076-4D2E-A016-A91B568A06B4}']

    // Retrieves the set of system fonts.
    // param name: includeDownloadableFonts: Include downloadable fonts or only locally installed ones.
    // param name: fontSet: Receives a pointer to the font set object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontSet(includeDownloadableFonts: BOOL;
                              out fontSet: IDWriteFontSet2): HResult; stdcall;

    // Retrieves a collection of fonts grouped into families.
    // param name: includeDownloadableFonts: Include downloadable fonts or only locally installed ones.
    // param name: fontFamilyModel: How to group families in the collection.
    // param name: fontCollection: Receives a pointer to the font collection object, or nullptr in case of failure.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontCollection(includeDownloadableFonts: BOOL;
                                     fontFamilyModel: DWRITE_FONT_FAMILY_MODEL;
                                     out fontCollection: IDWriteFontCollection3): HResult; stdcall;

  end;
  IID_IDWriteFactory7 = IDWriteFactory7;
  {$EXTERNALSYM IID_IDWriteFactory7}

// #endif // NTDDI_VERSION >= NTDDI_WIN10_RS4

// #if NTDDI_VERSION >= NTDDI_WIN10_RS5

  // Interface IDWriteFontSet3
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSet3);'}
  {$EXTERNALSYM IDWriteFontSet3}
  IDWriteFontSet3 = interface(IDWriteFontSet2)
  ['{7C073EF2-A7F4-4045-8C32-8AB8AE640F90}']

    // Gets the font source type of the specified font.
    // param name: listIndex: Zero-based index of the font.
    function GetFontSourceType(fontIndex: UINT32): DWRITE_FONT_SOURCE_TYPE; stdcall;

    // Gets the length of the font source name for the specified font.
    // param name: listIndex: Zero-based index of the font.
    function GetFontSourceNameLength(listIndex: UINT32): UINT32; stdcall;

    // Copies the font source name for the specified font to an output array.
    // param name: listIndex: Zero-based index of the font.
    // param name: stringBuffer: Character array that receives the string.
    // param name: stringBufferSize: Size of the array in characters. The size must include space for the terminating
    // null character.
    // returns
    // Standard HRESULT error code.
    function GetFontSourceName(listIndex: UINT32;
                               out stringBuffer: PWideChar;
                               stringBufferSize: UINT32): HResult; stdcall;

  end;
  IID_IDWriteFontSet3 = IDWriteFontSet3;
  {$EXTERNALSYM IID_IDWriteFontSet3}


//#endif // NTDDI_VERSION >= NTDDI_WIN10_RS5

  // Creates an OpenType tag for a font axis.
  function DwriteMakeFontAxisTag(a: AnsiChar;
                                 b: AnsiChar;
                                 c: AnsiChar;
                                 d: AnsiChar): DWRITE_FONT_AXIS_TAG;
  {$EXTERNALSYM DwriteMakeFontAxisTag}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation


function DwriteMakeFontAxisTag(a: AnsiChar;
                               b: AnsiChar;
                               c: AnsiChar;
                               d: AnsiChar): DWRITE_FONT_AXIS_TAG;
begin
  Result:= UINT32(Ord(a)) or
           (UINT32(Ord(b)) shl 8) or
           (UINT32(Ord(c)) shl 16) or
           (UINT32(Ord(d)) shl 24);
end;

  // Implement Additional Prototypes here.

end.


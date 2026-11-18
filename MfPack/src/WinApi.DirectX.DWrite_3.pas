// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectWrite
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DWrite_3.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.DirectX.DWrite_3;

  {$HPPEMIT '#include "dwrite_3.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {DirectX}
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2DBaseTypes,
  WinApi.DirectX.DWrite,
  WinApi.DirectX.DWrite_1,
  WinApi.DirectX.DWrite_2;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


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

type
  PLOGFONTW = ^LOGFONTW;
  PFONTSIGNATURE = ^FONTSIGNATURE;
  PLOGFONT = ^LOGFONT;
  PDWRITE_GLYPH_IMAGE_FORMATS = ^DWRITE_GLYPH_IMAGE_FORMATS;
  PD2D1_POINT_2F = ^D2D1_POINT_2F;
  PUUID = ^UUID;
  PD2D_RECT_F = ^D2D_RECT_F;

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
  DWRITE_RENDERING_MODE1 = type DWord;
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
  PIDWriteFontSet4 = ^IDWriteFontSet4;
  IDWriteFontSet4 = interface;

  // Font property used for filtering font sets and
  // building a font set with explicit properties.
  PDWRITE_FONT_PROPERTY = ^DWRITE_FONT_PROPERTY;
  DWRITE_FONT_PROPERTY = record
    // Specifies the requested font property, such as DWRITE_FONT_PROPERTY_ID_FAMILY_NAME.
    propertyId: DWRITE_FONT_PROPERTY_ID;

    // Specifies the property value, such as "Segoe UI".
    propertyValue: PWideChar;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetRenderingMode1(): DWRITE_RENDERING_MODE1; stdcall; overload;

  end;


  // Interface IDWriteFactory3
  // =========================
  // The root factory interface for all DWrite objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory3);'}
  {$EXTERNALSYM IDWriteFactory3}
  IDWriteFactory3 = interface(IDWriteFactory2)
  ['{9A1B41C3-D3BB-466A-87FC-FE67556A3B65}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateGlyphRunAnalysis(glyphRun: PDWRITE_GLYPH_RUN; transform: PDWRITE_MATRIX; renderingMode: DWRITE_RENDERING_MODE1; measuringMode: DWRITE_MEASURING_MODE; gridFitMode: DWRITE_GRID_FIT_MODE; antialiasMode: DWRITE_TEXT_ANTIALIAS_MODE; baselineOriginX: Single; baselineOriginY: Single; out glyphRunAnalysis: IDWriteGlyphRunAnalysis): HRESULT; stdcall; overload;

    function CreateCustomRenderingParams(gamma: Single; enhancedContrast: Single; grayscaleEnhancedContrast: Single; clearTypeLevel: Single; pixelGeometry: DWRITE_PIXEL_GEOMETRY; renderingMode: DWRITE_RENDERING_MODE1; gridFitMode: DWRITE_GRID_FIT_MODE; out renderingParams: IDWriteRenderingParams3): HRESULT; stdcall; overload;

    function CreateFontFaceReference(fontFile: IDWriteFontFile; faceIndex: UINT32; fontSimulations: DWRITE_FONT_SIMULATIONS; out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

    function CreateFontFaceReference(filePath: PWideChar; lastWriteTime: PFILETIME; faceIndex: UINT32; fontSimulations: DWRITE_FONT_SIMULATIONS; out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

    function GetSystemFontSet(out fontSet: IDWriteFontSet): HRESULT; stdcall; overload;

    function CreateFontSetBuilder(out fontSetBuilder: IDWriteFontSetBuilder): HRESULT; stdcall; overload;

    function CreateFontCollectionFromFontSet(fontSet: IDWriteFontSet; out fontCollection: IDWriteFontCollection1): HRESULT; stdcall; overload;

    function GetSystemFontCollection(includeDownloadableFonts: BOOL; out fontCollection: IDWriteFontCollection1; checkForUpdates: BOOL): HRESULT; stdcall; overload;

    function GetFontDownloadQueue(out fontDownloadQueue: IDWriteFontDownloadQueue): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontCount(): UINT32; stdcall; overload;

    function GetFontFaceReference(listIndex: UINT32; out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

    function FindFontFaceReference(fontFaceReference: IDWriteFontFaceReference; listIndex: PUINT32; exists: PBOOL): HRESULT; stdcall; overload;

    function FindFontFace(fontFace: IDWriteFontFace; listIndex: PUINT32; exists: PBOOL): HRESULT; stdcall; overload;

    function GetPropertyValues(propertyID: DWRITE_FONT_PROPERTY_ID; out values: IDWriteStringList): HRESULT; stdcall; overload;

    function GetPropertyValues(propertyID: DWRITE_FONT_PROPERTY_ID; preferredLocaleNames: PWideChar; out values: IDWriteStringList): HRESULT; stdcall; overload;

    function GetPropertyValues(listIndex: UINT32; propertyId: DWRITE_FONT_PROPERTY_ID; exists: PBOOL; out values: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

    function GetPropertyOccurrenceCount(_property: PDWRITE_FONT_PROPERTY; propertyOccurrenceCount: PUINT32): HRESULT; stdcall; overload;

    function GetMatchingFonts(familyName: PWideChar; fontWeight: DWRITE_FONT_WEIGHT; fontStretch: DWRITE_FONT_STRETCH; fontStyle: DWRITE_FONT_STYLE; out filteredSet: IDWriteFontSet): HRESULT; stdcall; overload;

    function GetMatchingFonts(properties: PDWRITE_FONT_PROPERTY; propertyCount: UINT32; out filteredSet: IDWriteFontSet): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function AddFontFaceReference(fontFaceReference: IDWriteFontFaceReference; properties: PDWRITE_FONT_PROPERTY; propertyCount: UINT32): HRESULT; stdcall; overload;

    function AddFontFaceReference(fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

    function AddFontSet(fontSet: IDWriteFontSet): HRESULT; stdcall; overload;

    function CreateFontSet(out fontSet: IDWriteFontSet): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontSet(out fontSet: IDWriteFontSet): HRESULT; stdcall; overload;

    function GetFontFamily(index: UINT32; out fontFamily: IDWriteFontFamily1): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontLocality(listIndex: UINT32): DWRITE_LOCALITY; stdcall; overload;

    function GetFont(listIndex: UINT32; out font: IDWriteFont3): HRESULT; stdcall; overload;

    function GetFontFaceReference(listIndex: UINT32; out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontLocality(listIndex: UINT32): DWRITE_LOCALITY; stdcall; overload;

    function GetFont(listIndex: UINT32; out font: IDWriteFont3): HRESULT; stdcall; overload;

    function GetFontFaceReference(listIndex: UINT32; out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateFontFace(out fontFace: IDWriteFontFace3): HRESULT; stdcall; overload;

    function CreateFontFaceWithSimulations(fontFaceSimulationFlags: DWRITE_FONT_SIMULATIONS; out fontFace: IDWriteFontFace3): HRESULT; stdcall; overload;

    function Equals(fontFaceReference: IDWriteFontFaceReference): BOOL; stdcall; overload;

    function GetFontFaceIndex(): UINT32; stdcall; overload;

    function GetSimulations(): DWRITE_FONT_SIMULATIONS; stdcall; overload;

    function GetFontFile(out fontFile: IDWriteFontFile): HRESULT; stdcall; overload;

    function GetLocalFileSize(): UINT64; stdcall; overload;

    function GetFileSize(): UINT64; stdcall; overload;

    function GetFileTime(lastWriteTime: PFILETIME): HRESULT; stdcall; overload;

    function GetLocality(): DWRITE_LOCALITY; stdcall; overload;

    function EnqueueFontDownloadRequest(): HRESULT; stdcall; overload;

    function EnqueueCharacterDownloadRequest(characters: PWideChar; characterCount: UINT32): HRESULT; stdcall; overload;

    function EnqueueGlyphDownloadRequest(glyphIndices: PWORD; glyphCount: UINT32): HRESULT; stdcall; overload;

    function EnqueueFileFragmentDownloadRequest(fileOffset: UINT64; fragmentSize: UINT64): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateFontFace(out fontFace: IDWriteFontFace3): HRESULT; stdcall; overload;

    function Equals(font: IDWriteFont): BOOL; stdcall; overload;

    function GetFontFaceReference(out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

    function HasCharacter(unicodeValue: UINT32): BOOL; stdcall; overload;

    function GetLocality(): DWRITE_LOCALITY; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontFaceReference(out fontFaceReference: IDWriteFontFaceReference): HRESULT; stdcall; overload;

    procedure GetPanose(panose: PDWRITE_PANOSE); stdcall; overload;

    function GetWeight(): DWRITE_FONT_WEIGHT; stdcall; overload;

    function GetStretch(): DWRITE_FONT_STRETCH; stdcall; overload;

    function GetStyle(): DWRITE_FONT_STYLE; stdcall; overload;

    function GetFamilyNames(out names: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

    function GetFaceNames(out names: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

    function GetInformationalStrings(informationalStringID: DWRITE_INFORMATIONAL_STRING_ID; out informationalStrings: IDWriteLocalizedStrings; exists: PBOOL): HRESULT; stdcall; overload;

    function HasCharacter(unicodeValue: UINT32): BOOL; stdcall; overload;

    function GetRecommendedRenderingMode(fontEmSize: Single; dpiX: Single; dpiY: Single; transform: PDWRITE_MATRIX; isSideways: BOOL; outlineThreshold: DWRITE_OUTLINE_THRESHOLD; measuringMode: DWRITE_MEASURING_MODE; renderingParams: IDWriteRenderingParams; renderingMode: PDWRITE_RENDERING_MODE1; gridFitMode: PDWRITE_GRID_FIT_MODE): HRESULT; stdcall; overload;

    function IsCharacterLocal(unicodeValue: UINT32): BOOL; stdcall; overload;

    function IsGlyphLocal(glyphId: UINT16): BOOL; stdcall; overload;

    function AreCharactersLocal(characters: PWideChar; characterCount: UINT32; enqueueIfNotLocal: BOOL; isLocal: PBOOL): HRESULT; stdcall; overload;

    function AreGlyphsLocal(glyphIndices: PWORD; glyphCount: UINT32; enqueueIfNotLocal: BOOL; isLocal: PBOOL): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetCount(): UINT32; stdcall; overload;

    function GetLocaleNameLength(listIndex: UINT32; length: PUINT32): HRESULT; stdcall; overload;

    function GetLocaleName(listIndex: UINT32; localeName: PWideChar; size: UINT32): HRESULT; stdcall; overload;

    function GetStringLength(listIndex: UINT32; length: PUINT32): HRESULT; stdcall; overload;

    function GetString(listIndex: UINT32; stringBuffer: PWideChar; stringBufferSize: UINT32): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    procedure DownloadCompleted(downloadQueue: IDWriteFontDownloadQueue; context: IUnknown; downloadResult: HRESULT); stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function AddListener(listener: IDWriteFontDownloadListener; token: PUINT32): HRESULT; stdcall; overload;

    function RemoveListener(token: UINT32): HRESULT; stdcall; overload;

    function IsEmpty(): BOOL; stdcall; overload;

    function BeginDownload(context: IUnknown): HRESULT; stdcall; overload;

    function CancelDownload(): HRESULT; stdcall; overload;

    function GetGenerationCount(): UINT64; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateFontFromLOGFONT(logFont: PLOGFONTW; fontCollection: IDWriteFontCollection; out font: IDWriteFont): HRESULT; stdcall; overload;

    function GetFontSignature(fontFace: IDWriteFontFace; fontSignature: PFONTSIGNATURE): HRESULT; stdcall; overload;

    function GetFontSignature(font: IDWriteFont; fontSignature: PFONTSIGNATURE): HRESULT; stdcall; overload;

    function GetMatchingFontsByLOGFONT(logFont: PLOGFONT; fontSet: IDWriteFontSet; out filteredSet: IDWriteFontSet): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function SetLineSpacing(lineSpacingOptions: PDWRITE_LINE_SPACING): HRESULT; stdcall; overload;

    function GetLineSpacing(lineSpacingOptions: PDWRITE_LINE_SPACING): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function InvalidateLayout(): HRESULT; stdcall; overload;

    function SetLineSpacing(lineSpacingOptions: PDWRITE_LINE_SPACING): HRESULT; stdcall; overload;

    function GetLineSpacing(lineSpacingOptions: PDWRITE_LINE_SPACING): HRESULT; stdcall; overload;

    function GetLineMetrics(lineMetrics: PDWRITE_LINE_METRICS1; maxLineCount: UINT32; actualLineCount: PUINT32): HRESULT; stdcall; overload;

  end;
  IID_IDWriteTextLayout3 = IDWriteTextLayout3;
  {$EXTERNALSYM IID_IDWriteTextLayout3}


///////////////////////////////////////////////////////////////////


// #if NTDDI_VERSION >= NTDDI_WIN10_RS1


  // Represents a color glyph run. The IDWriteFactory4.TranslateColorGlyphRun
  // method returns an ordered collection of color glyph runs of varying types
  // depending on what the font supports.
  // For runs without any specific color, such as PNG data, the runColor field will be zero.
  PDWRITE_COLOR_GLYPH_RUN1 = ^DWRITE_COLOR_GLYPH_RUN1;
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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetCurrentRun(out colorGlyphRun: PDWRITE_COLOR_GLYPH_RUN1): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetGlyphImageFormats(glyphId: UINT16; pixelsPerEmFirst: UINT32; pixelsPerEmLast: UINT32; glyphImageFormats: PDWRITE_GLYPH_IMAGE_FORMATS): HRESULT; stdcall; overload;

    function GetGlyphImageFormats(): DWRITE_GLYPH_IMAGE_FORMATS; stdcall; overload;

    function GetGlyphImageData(glyphId: UINT16; pixelsPerEm: UINT32; glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS; glyphData: PDWRITE_GLYPH_IMAGE_DATA; out glyphDataContext: Pointer): HRESULT; stdcall; overload;

    procedure ReleaseGlyphImageData(glyphDataContext: Pointer); stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function TranslateColorGlyphRun(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION; desiredGlyphImageFormats: DWRITE_GLYPH_IMAGE_FORMATS; measuringMode: DWRITE_MEASURING_MODE; worldAndDpiTransform: PDWRITE_MATRIX; colorPaletteIndex: UINT32; out colorLayers: IDWriteColorGlyphRunEnumerator1): HRESULT; stdcall; overload;

    function ComputeGlyphOrigins(glyphRun: PDWRITE_GLYPH_RUN; baselineOrigin: D2D1_POINT_2F; glyphOrigins: PD2D1_POINT_2F): HRESULT; stdcall; overload;

    function ComputeGlyphOrigins(glyphRun: PDWRITE_GLYPH_RUN; measuringMode: DWRITE_MEASURING_MODE; baselineOrigin: D2D1_POINT_2F; worldAndDpiTransform: PDWRITE_MATRIX; glyphOrigins: PD2D1_POINT_2F): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function AddFontFile(fontFile: IDWriteFontFile): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetWaitHandle(): THandle; stdcall; overload;

    function GetResult(): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetLocalFileSize(localFileSize: PUINT64): HRESULT; stdcall; overload;

    function GetFileFragmentLocality(fileOffset: UINT64; fragmentSize: UINT64; isLocal: PBOOL; partialSize: PUINT64): HRESULT; stdcall; overload;

    function GetLocality(): DWRITE_LOCALITY; stdcall; overload;

    function BeginDownload(downloadOperationID: PUUID; fileFragments: PDWRITE_FILE_FRAGMENT; fragmentCount: UINT32; out asyncResult: IDWriteAsyncResult): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateRemoteStreamFromKey(fontFileReferenceKey: Pointer; fontFileReferenceKeySize: UINT32; out fontFileStream: IDWriteRemoteFontFileStream): HRESULT; stdcall; overload;

    function GetLocalityFromKey(fontFileReferenceKey: Pointer; fontFileReferenceKeySize: UINT32; locality: PDWRITE_LOCALITY): HRESULT; stdcall; overload;

    function CreateFontFileReferenceFromUrl(factory: IDWriteFactory; baseUrl: PWideChar; fontFileUrl: PWideChar; out fontFile: IDWriteFontFile): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateInMemoryFontFileReference(factory: IDWriteFactory; fontData: Pointer; fontDataSize: UINT32; ownerObject: IUnknown; out fontFile: IDWriteFontFile): HRESULT; stdcall; overload;

    function GetFileCount(): UINT32; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateFontSetBuilder(out fontSetBuilder: IDWriteFontSetBuilder1): HRESULT; stdcall; overload;

    function CreateInMemoryFontFileLoader(out newLoader: IDWriteInMemoryFontFileLoader): HRESULT; stdcall; overload;

    function CreateHttpFontFileLoader(referrerUrl: PWideChar; extraHeaders: PWideChar; out newLoader: IDWriteRemoteFontFileLoader): HRESULT; stdcall; overload;

    function AnalyzeContainerType(fileData: Pointer; fileDataSize: UINT32): DWRITE_CONTAINER_TYPE; stdcall; overload;

    function UnpackFontFile(containerType: DWRITE_CONTAINER_TYPE; fileData: Pointer; fileDataSize: UINT32; out unpackedFontStream: IDWriteFontFileStream): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateFontFaceReference(fontFile: IDWriteFontFile; faceIndex: UINT32; fontSimulations: DWRITE_FONT_SIMULATIONS; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; out fontFaceReference: IDWriteFontFaceReference1): HRESULT; stdcall; overload;

    function CreateFontResource(fontFile: IDWriteFontFile; faceIndex: UINT32; out fontResource: IDWriteFontResource): HRESULT; stdcall; overload;

    function GetSystemFontSet(includeDownloadableFonts: BOOL; out fontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

    function GetSystemFontCollection(includeDownloadableFonts: BOOL; fontFamilyModel: DWRITE_FONT_FAMILY_MODEL; out fontCollection: IDWriteFontCollection2): HRESULT; stdcall; overload;

    function CreateFontCollectionFromFontSet(fontSet: IDWriteFontSet; fontFamilyModel: DWRITE_FONT_FAMILY_MODEL; out fontCollection: IDWriteFontCollection2): HRESULT; stdcall; overload;

    function CreateFontSetBuilder(out fontSetBuilder: IDWriteFontSetBuilder2): HRESULT; stdcall; overload;

    function CreateTextFormat(fontFamilyName: PWideChar; fontCollection: IDWriteFontCollection; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; fontSize: Single; localeName: PWideChar; out textFormat: IDWriteTextFormat3): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontAxisValueCount(): UINT32; stdcall; overload;

    function GetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32): HRESULT; stdcall; overload;

    function HasVariations(): BOOL; stdcall; overload;

    function GetFontResource(out fontResource: IDWriteFontResource): HRESULT; stdcall; overload;

    function Equals(fontFace: IDWriteFontFace): BOOL; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontFile(out fontFile: IDWriteFontFile): HRESULT; stdcall; overload;

    function GetFontFaceIndex(): UINT32; stdcall; overload;

    function GetFontAxisCount(): UINT32; stdcall; overload;

    function GetDefaultFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32): HRESULT; stdcall; overload;

    function GetFontAxisRanges(fontAxisRanges: PDWRITE_FONT_AXIS_RANGE; fontAxisRangeCount: UINT32): HRESULT; stdcall; overload;

    function GetFontAxisAttributes(axisIndex: UINT32): DWRITE_FONT_AXIS_ATTRIBUTES; stdcall; overload;

    function GetAxisNames(axisIndex: UINT32; out names: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

    function GetAxisValueNameCount(axisIndex: UINT32): UINT32; stdcall; overload;

    function GetAxisValueNames(axisIndex: UINT32; axisValueIndex: UINT32; fontAxisRange: PDWRITE_FONT_AXIS_RANGE; out names: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

    function HasVariations(): BOOL; stdcall; overload;

    function CreateFontFace(fontSimulations: DWRITE_FONT_SIMULATIONS; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; out fontFace: IDWriteFontFace5): HRESULT; stdcall; overload;

    function CreateFontFaceReference(fontSimulations: DWRITE_FONT_SIMULATIONS; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; out fontFaceReference: IDWriteFontFaceReference1): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function CreateFontFace(out fontFace: IDWriteFontFace5): HRESULT; stdcall; overload;

    function GetFontAxisValueCount(): UINT32; stdcall; overload;

    function GetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function AddFont(fontFile: IDWriteFontFile; fontFaceIndex: UINT32; fontSimulations: DWRITE_FONT_SIMULATIONS; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; fontAxisRanges: PDWRITE_FONT_AXIS_RANGE; fontAxisRangeCount: UINT32; properties: PDWRITE_FONT_PROPERTY; propertyCount: UINT32): HRESULT; stdcall; overload;

    function AddFontFile(filePath: PWideChar): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetMatchingFonts(fontProperty: PDWRITE_FONT_PROPERTY; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; out matchingFonts: IDWriteFontSet1): HRESULT; stdcall; overload;

    function GetFirstFontResources(out filteredFontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

    function GetFilteredFonts(indices: PUINT32; indexCount: UINT32; out filteredFontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

    function GetFilteredFonts(fontAxisRanges: PDWRITE_FONT_AXIS_RANGE; fontAxisRangeCount: UINT32; selectAnyRange: BOOL; out filteredFontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

    function GetFilteredFonts(properties: PDWRITE_FONT_PROPERTY; propertyCount: UINT32; selectAnyProperty: BOOL; out filteredFontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

    function GetFilteredFontIndices(fontAxisRanges: PDWRITE_FONT_AXIS_RANGE; fontAxisRangeCount: UINT32; selectAnyRange: BOOL; indices: PUINT32; maxIndexCount: UINT32; actualIndexCount: PUINT32): HRESULT; stdcall; overload;

    function GetFilteredFontIndices(properties: PDWRITE_FONT_PROPERTY; propertyCount: UINT32; selectAnyProperty: BOOL; indices: PUINT32; maxIndexCount: UINT32; actualIndexCount: PUINT32): HRESULT; stdcall; overload;

    function GetFontAxisRanges(listIndex: UINT32; fontAxisRanges: PDWRITE_FONT_AXIS_RANGE; maxFontAxisRangeCount: UINT32; actualFontAxisRangeCount: PUINT32): HRESULT; stdcall; overload;

    function GetFontAxisRanges(fontAxisRanges: PDWRITE_FONT_AXIS_RANGE; maxFontAxisRangeCount: UINT32; actualFontAxisRangeCount: PUINT32): HRESULT; stdcall; overload;

    function GetFontFaceReference(listIndex: UINT32; out fontFaceReference: IDWriteFontFaceReference1): HRESULT; stdcall; overload;

    function CreateFontResource(listIndex: UINT32; out fontResource: IDWriteFontResource): HRESULT; stdcall; overload;

    function CreateFontFace(listIndex: UINT32; out fontFace: IDWriteFontFace5): HRESULT; stdcall; overload;

    function GetFontLocality(listIndex: UINT32): DWRITE_LOCALITY; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontSet(out fontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetMatchingFonts(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; out matchingFonts: IDWriteFontList2): HRESULT; stdcall; overload;

    function GetFontSet(out fontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontFamily(index: UINT32; out fontFamily: IDWriteFontFamily2): HRESULT; stdcall; overload;

    function GetMatchingFonts(familyName: PWideChar; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; out fontList: IDWriteFontList2): HRESULT; stdcall; overload;

    function GetFontFamilyModel(): DWRITE_FONT_FAMILY_MODEL; stdcall; overload;

    function GetFontSet(out fontSet: IDWriteFontSet1): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function SetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; textRange: DWRITE_TEXT_RANGE): HRESULT; stdcall; overload;

    function GetFontAxisValueCount(currentPosition: UINT32): UINT32; stdcall; overload;

    function GetFontAxisValues(currentPosition: UINT32; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; textRange: PDWRITE_TEXT_RANGE): HRESULT; stdcall; overload;

    function GetAutomaticFontAxes(): DWRITE_AUTOMATIC_FONT_AXES; stdcall; overload;

    function SetAutomaticFontAxes(automaticFontAxes: DWRITE_AUTOMATIC_FONT_AXES): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function SetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32): HRESULT; stdcall; overload;

    function GetFontAxisValueCount(): UINT32; stdcall; overload;

    function GetFontAxisValues(fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32): HRESULT; stdcall; overload;

    function GetAutomaticFontAxes(): DWRITE_AUTOMATIC_FONT_AXES; stdcall; overload;

    function SetAutomaticFontAxes(automaticFontAxes: DWRITE_AUTOMATIC_FONT_AXES): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function MapCharacters(analysisSource: IDWriteTextAnalysisSource; textPosition: UINT32; textLength: UINT32; baseFontCollection: IDWriteFontCollection; baseFamilyName: PWideChar; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; mappedLength: PUINT32; scale: PSingle; out mappedFontFace: IDWriteFontFace5): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetExpirationEvent(): THandle; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetExpirationEvent(): THandle; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetSystemFontSet(includeDownloadableFonts: BOOL; out fontSet: IDWriteFontSet2): HRESULT; stdcall; overload;

    function GetSystemFontCollection(includeDownloadableFonts: BOOL; fontFamilyModel: DWRITE_FONT_FAMILY_MODEL; out fontCollection: IDWriteFontCollection3): HRESULT; stdcall; overload;

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

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFontSourceType(fontIndex: UINT32): DWRITE_FONT_SOURCE_TYPE; stdcall; overload;

    function GetFontSourceNameLength(listIndex: UINT32): UINT32; stdcall; overload;

    function GetFontSourceName(listIndex: UINT32; stringBuffer: PWideChar; stringBufferSize: UINT32): HRESULT; stdcall; overload;

  end;
  IID_IDWriteFontSet3 = IDWriteFontSet3;
  {$EXTERNALSYM IID_IDWriteFontSet3}






//#endif // NTDDI_VERSION >= NTDDI_WIN10_RS5




{if NTDDI_VERSION >= NTDDI_WIN10_MN}


  // Interface IDWriteFontFace6
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace6);'}
  {$EXTERNALSYM IDWriteFontFace6}
  IDWriteFontFace6 = interface(IDWriteFontFace5)
  ['{C4B1FE1B-6E84-47D5-B54C-A597981B06AD}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetFamilyNames(fontFamilyModel: DWRITE_FONT_FAMILY_MODEL; out names: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

    function GetFaceNames(fontFamilyModel: DWRITE_FONT_FAMILY_MODEL; out names: IDWriteLocalizedStrings): HRESULT; stdcall; overload;

  end;
  IID_IDWriteFontFace6 = IDWriteFontFace6;
  {$EXTERNALSYM IID_IDWriteFontFace6}

{endif NTDDI_VERSION >= NTDDI_WIN10_MN}


{if NTDDI_VERSION >= NTDDI_WIN10_NI}


  // Interface IDWriteFontSet4
  //==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontSet4);'}
  {$EXTERNALSYM IDWriteFontSet4}
  IDWriteFontSet4 = interface(IDWriteFontSet3)
  ['{EEC175FC-BEA9-4C86-8B53-CCBDD7DF0C82}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function ConvertWeightStretchStyleToFontAxisValues(inputAxisValues: PDWRITE_FONT_AXIS_VALUE; inputAxisCount: UINT32; fontWeight: DWRITE_FONT_WEIGHT; fontStretch: DWRITE_FONT_STRETCH; fontStyle: DWRITE_FONT_STYLE; fontSize: Single; outputAxisValues: PDWRITE_FONT_AXIS_VALUE): UINT32; stdcall; overload;

    function GetMatchingFonts(familyName: PWideChar; fontAxisValues: PDWRITE_FONT_AXIS_VALUE; fontAxisValueCount: UINT32; allowedSimulations: DWRITE_FONT_SIMULATIONS; out matchingFonts: IDWriteFontSet4): HRESULT; stdcall; overload;

  end;
  IID_IDWriteFontSet4 = IDWriteFontSet4;
  {$EXTERNALSYM IID_IDWriteFontSet4}





  // Creates an OpenType tag for a font axis.
  function DwriteMakeFontAxisTag(a: AnsiChar;
                                 b: AnsiChar;
                                 c: AnsiChar;
                                 d: AnsiChar): DWRITE_FONT_AXIS_TAG;
  {$EXTERNALSYM DwriteMakeFontAxisTag}

{endif NTDDI_VERSION >= NTDDI_WIN10_NI}


//
// May 2024 update  NTDDI_VERSION >= NTDDI_WIN10_CU ////////////////////////////
//

type
  /// <summary>
  /// Contains information about a bitmap associated with an IDWriteBitmapRenderTarget.
  /// The bitmap is top-down with 32-bits per pixel and no padding between scan lines.
  /// </summary>
  PDWRITE_BITMAP_DATA_BGRA32 = ^DWRITE_BITMAP_DATA_BGRA32;
  DWRITE_BITMAP_DATA_BGRA32 = record
    width: UINT32;
    height: UINT32;

    pixels: PUINT32;
   end;
  {$EXTERNALSYM DWRITE_BITMAP_DATA_BGRA32}

  /// <summary>
  /// Encapsulates a bitmap which can be used for rendering glyphs.
  /// </summary>
  ///
  // Interface IDWriteFontSet4
  //==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteBitmapRenderTarget2);'}
  {$EXTERNALSYM IDWriteBitmapRenderTarget2}
  IDWriteBitmapRenderTarget2 = interface(IDWriteBitmapRenderTarget1)
  ['{C553A742-FC01-44DA-A66E-B8B9ED6C3995}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetBitmapData(bitmapData: PDWRITE_BITMAP_DATA_BGRA32): HRESULT; stdcall; overload;

  end;
  IID_IDWriteBitmapRenderTarget2 = IDWriteBitmapRenderTarget2;
  {$EXTERNALSYM IID_IDWriteBitmapRenderTarget2}


  /// <summary>
  /// Defines known feature level for use with the IDWritePaintReader interface and
  /// related APIs. A feature level represents a level of functionality. For example, it
  /// determines what DWRITE_PAINT_TYPE values might be returned.
  /// </summary>
  /// <remarks>
  /// See the DWRITE_PAINT_TYPE enumeration for which paint types are required for each
  /// feature level.
  /// </remarks>


type

  PDWRITE_PAINT_FEATURE_LEVEL = ^DWRITE_PAINT_FEATURE_LEVEL;
  DWRITE_PAINT_FEATURE_LEVEL = INT32;
  {$EXTERNALSYM DWRITE_PAINT_FEATURE_LEVEL}
const
  /// <summary>
  /// No paint API support.
  /// </summary>
  DWRITE_PAINT_FEATURE_LEVEL_NONE = DWRITE_PAINT_FEATURE_LEVEL(0);
  {$EXTERNALSYM DWRITE_PAINT_FEATURE_LEVEL_NONE}

  /// <summary>
  /// Specifies a level of functionality corresponding to OpenType COLR version 0.
  /// </summary>
  DWRITE_PAINT_FEATURE_LEVEL_COLR_V0 = DWRITE_PAINT_FEATURE_LEVEL(1);
  {$EXTERNALSYM DWRITE_PAINT_FEATURE_LEVEL_COLR_V0}

  /// <summary>
  /// Specifies a level of functionality corresponding to OpenType COLR version 1.
  /// </summary>
  DWRITE_PAINT_FEATURE_LEVEL_COLR_V1 = DWRITE_PAINT_FEATURE_LEVEL(2);
  {$EXTERNALSYM DWRITE_PAINT_FEATURE_LEVEL_COLR_V1}


  /// <summary>
  /// Combination of flags specifying attributes of a color glyph or of specific color values in
  /// a color glyph.
  /// </summary>
type
   PDWRITE_PAINT_ATTRIBUTES = ^DWRITE_PAINT_ATTRIBUTES;
   DWRITE_PAINT_ATTRIBUTES = (

    /// <summary>
    /// Specifies no attribute.
    /// </summary>
    DWRITE_PAINT_ATTRIBUTES_NONE = $0,

    /// <summary>
    /// Specifies that the color value (or any color value in the glyph) comes from the font's
    /// color palette. This means the appearance may depend on the current palette index, which
    /// may be important to clients that cache color glyphs.
    /// </summary>
    DWRITE_PAINT_ATTRIBUTES_USES_PALETTE = $01,

    /// <summary>
    /// Specifies that the color value (or any color value in the glyph) comes from the client-specified
    /// text color. This means the appearance may depend on the text color, which may be important to
    /// clients that cache color glyphs.
    /// </summary>
    DWRITE_PAINT_ATTRIBUTES_USES_TEXT_COLOR = $02
  );
   {$EXTERNALSYM DWRITE_PAINT_ATTRIBUTES}


//#ifdef DEFINE_ENUM_FLAG_OPERATORS
//DEFINE_ENUM_FLAG_OPERATORS(DWRITE_PAINT_ATTRIBUTES)
//#endif

type
  /// <summary>
  /// Represents a color in a color glyph.
  /// </summary>
  PDWRITE_PAINT_COLOR = ^DWRITE_PAINT_COLOR;
  DWRITE_PAINT_COLOR = record

    /// <summary>
    /// Color value (not premultiplied). See the colorAttributes member for information about how
    /// the color is determined.
    /// </summary>
    value: DWRITE_COLOR_F;

    /// <summary>
    /// If the colorAttributes member is DWRITE_PAINT_ATTRIBUTES_USES_PALETTE, this member is
    /// the index of a palette entry in the selected color palette. Otherwise, this member is
    /// DWRITE_NO_PALETTE_INDEX (0xFFFF).
    /// </summary>
    paletteEntryIndex: UINT16;

    /// <summary>
    /// Specifies an alpha value multiplier in the range 0 to 1 that was used to compute the color
    /// value. Color glyph descriptions may include alpha values to be multiplied with the alpha
    /// values of palette entries.
    /// </summary>
    alphaMultiplier: Single;

    /// <summary>
    /// Specifies how the color value is determined. If this member is
    /// DWRITE_PAINT_ATTRIBUTES_USES_PALETTE, the color value is determined by getting the color at
    /// paletteEntryIndex in the current color palette. The color's alpha value is then multiplied
    /// by alphaMultiplier. If a font has multiple color palettes, a client can set the current color
    /// palette using the IDWritePaintReader::SetColorPaletteIndex method. A client that uses a custom
    /// palette can use the paletteEntryIndex and alphaMultiplier methods to compute the color. If this
    /// member is DWRITE_PAINT_ATTRIBUTES_USES_TEXT_COLOR, the color value is equal to the text
    /// foreground color, which can be set using the IDWritePaintReader::SetTextColor method.
    /// </summary>
    colorAttributes: DWRITE_PAINT_ATTRIBUTES;
  end;
  {$EXTERNALSYM DWRITE_PAINT_COLOR}

  /// <summary>
  /// Specifies a composite mode for combining source and destination paint elements in a
  /// color glyph. These are taken from the W3C Compositing and Blending Level 1 specification.
  /// </summary>
  DWRITE_COLOR_COMPOSITE_MODE = (

    // Porter-Duff modes.
    DWRITE_COLOR_COMPOSITE_CLEAR,
    DWRITE_COLOR_COMPOSITE_SRC,
    DWRITE_COLOR_COMPOSITE_DEST,
    DWRITE_COLOR_COMPOSITE_SRC_OVER,
    DWRITE_COLOR_COMPOSITE_DEST_OVER,
    DWRITE_COLOR_COMPOSITE_SRC_IN,
    DWRITE_COLOR_COMPOSITE_DEST_IN,
    DWRITE_COLOR_COMPOSITE_SRC_OUT,
    DWRITE_COLOR_COMPOSITE_DEST_OUT,
    DWRITE_COLOR_COMPOSITE_SRC_ATOP,
    DWRITE_COLOR_COMPOSITE_DEST_ATOP,
    DWRITE_COLOR_COMPOSITE_XOR,
    DWRITE_COLOR_COMPOSITE_PLUS,

    // Separable color blend modes.
    DWRITE_COLOR_COMPOSITE_SCREEN,
    DWRITE_COLOR_COMPOSITE_OVERLAY,
    DWRITE_COLOR_COMPOSITE_DARKEN,
    DWRITE_COLOR_COMPOSITE_LIGHTEN,
    DWRITE_COLOR_COMPOSITE_COLOR_DODGE,
    DWRITE_COLOR_COMPOSITE_COLOR_BURN,
    DWRITE_COLOR_COMPOSITE_HARD_LIGHT,
    DWRITE_COLOR_COMPOSITE_SOFT_LIGHT,
    DWRITE_COLOR_COMPOSITE_DIFFERENCE,
    DWRITE_COLOR_COMPOSITE_EXCLUSION,
    DWRITE_COLOR_COMPOSITE_MULTIPLY,

    // Non-separable color blend modes.
    DWRITE_COLOR_COMPOSITE_HSL_HUE,
    DWRITE_COLOR_COMPOSITE_HSL_SATURATION,
    DWRITE_COLOR_COMPOSITE_HSL_COLOR,
    DWRITE_COLOR_COMPOSITE_HSL_LUMINOSITY
  );

  /// <summary>
  /// Identifies a type of paint element in a color glyph. A color glyph's visual representation
  /// is defined by a tree of paint elements. A paint element's properties are specified by a
  /// DWRITE_PAINT_ELEMENT structure, which combines a paint type an a union.
  /// </summary>
  /// <remarks>
  /// For more information about each paint type, see DWRITE_PAINT_ELEMENT.
  /// </remarks>
  DWRITE_PAINT_TYPE = (

    // The following paint types may be returned for color feature levels greater than
    // or equal to DWRITE_PAINT_FEATURE_LEVEL_COLR_V0.
    DWRITE_PAINT_TYPE_NONE,
    DWRITE_PAINT_TYPE_LAYERS,
    DWRITE_PAINT_TYPE_SOLID_GLYPH,

    // The following paint types may be returned for color feature levels greater than
    // or equal to DWRITE_PAINT_FEATURE_LEVEL_COLR_V1.
    DWRITE_PAINT_TYPE_SOLID,
    DWRITE_PAINT_TYPE_LINEAR_GRADIENT,
    DWRITE_PAINT_TYPE_RADIAL_GRADIENT,
    DWRITE_PAINT_TYPE_SWEEP_GRADIENT,
    DWRITE_PAINT_TYPE_GLYPH,
    DWRITE_PAINT_TYPE_COLOR_GLYPH,
    DWRITE_PAINT_TYPE_TRANSFORM,
    DWRITE_PAINT_TYPE_COMPOSITE
  );


type

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_LAYERS.
  /// Contains one or more child paint elements to be drawn in bottom-up order.
  /// </summary>
  /// <remarks>
  /// This corresponds to a PaintColrLayers record in the OpenType COLR table.
  /// Or it may correspond to a BaseGlyph record defined by COLR version 0.
  /// </remarks>
  PAINT_LAYERS = record
  {$EXTERNALSYM PAINT_LAYERS}

    /// <summary>
    /// Number of child paint elements in bottom-up order. Use the IDWritePaintReader
    /// interface's MoveFirstChild and MoveNextSibling methods to retrieve the child paint
    /// elements. Use the MoveParent method to return to the parent element.
    /// </summary>
    childCount: UINT32;
  end;

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_SOLID_GLYPH.
  /// Specifies a glyph with a solid color fill.
  /// This paint element has no child elements.
  /// </summary>
  /// <remarks>
  /// This corresponds to a combination of two paint records in the OpenType COLR table:
  /// a PaintGlyph record, which references either a PaintSolid or PaintVarSolid record.
  /// Or it may correspond to a Layer record defined by COLR version 0.
  /// </remarks>
  PAINT_SOLID_GLYPH = record

    /// <summary>
    /// Glyph index defining the shape to be filled.
    /// </summary>
    glyphIndex: UINT32;

    /// <summary>
    /// Glyph color used to fill the glyph shape.
    /// </summary>
    color: DWRITE_PAINT_COLOR;
  end;
  {$EXTERNALSYM PAINT_SOLID_GLYPH}

  // Define PAINT_LINEAR_GRADIENT record
  PAINT_LINEAR_GRADIENT = record
    /// <summary>
    /// D2D1_EXTEND_MODE value speciying how colors outside the interval are defined.
    /// </summary>
    extendMode: UINT32;

    /// <summary>
    /// Number of gradient stops. Use the IDWritePaintReader::GetGradientStops method to
    /// get the gradient stops.
    /// </summary>
    gradientStopCount: UINT32;

    /// <summary>
    /// X coordinate of the start point of the color line.
    /// </summary>
    x0: Single;

    /// <summary>
    /// Y coordinate of the start point of the color line.
    /// </summary>
    y0: Single;

    /// <summary>
    /// X coordinate of the end point of the color line.
    /// </summary>
    x1: Single;

    /// <summary>
    /// Y coordinate of the end point of the color line.
    /// </summary>
    y1: Single;

    /// <summary>
    /// X coordinate of the rotation point of the color line.
    /// </summary>
    x2: Single;

    /// <summary>
    /// Y coordinate of the rotation point of the color line.
    /// </summary>
    y2: Single;
  end;
  {$EXTERNALSYM PAINT_LINEAR_GRADIENT}

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_RADIAL_GRADIENT.
  /// Specifies a radial gradient used to fill the current shape or clip.
  /// This paint element has no child elements.
  /// </summary>
  /// <remarks>
  /// This corresponds to a PaintRadialGradient or PaintVarRadialGradient record in the OpenType
  /// COLR table.
  /// </remarks>
  PAINT_RADIAL_GRADIENT = record

    /// <summary>
    /// D2D1_EXTEND_MODE value speciying how colors outside the interval are defined.
    /// </summary>
    extendMode: UINT32;

    /// <summary>
    /// Number of gradient stops. Use the IDWritePaintReader::GetGradientStops method to
    /// get the gradient stops.
    /// </summary>
    gradientStopCount: UINT32;

    /// <summary>
    /// Center X coordinate of the start circle.
    /// </summary>
    x0: Single;

    /// <summary>
    /// Center Y coordinate of the start circle.
    /// </summary>
    y0: Single;

    /// <summary>
    /// Radius of the start circle.
    /// </summary>
    radius0: Single;

    /// <summary>
    /// Center X coordinate of the end circle.
    /// </summary>
    x1: Single;

    /// <summary>
    /// Center Y coordinate of the end circle.
    /// </summary>
    y1: Single;

    /// <summary>
    /// Radius of the end circle.
    /// </summary>
    radius1: Single;
  end;
  {$EXTERNALSYM PAINT_RADIAL_GRADIENT}

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_SWEEP_GRADIENT.
  /// Specifies a sweep gradient used to fill the current shape or clip.
  /// This paint element has no child elements.
  /// </summary>
  /// <remarks>
  /// This corresponds to a PaintSweepGradient or PaintVarSweepGradient record in the OpenType
  /// COLR table.
  /// </remarks>
  PAINT_SWEEP_GRADIENT = record

    /// <summary>
    /// D2D1_EXTEND_MODE value speciying how colors outside the interval are defined.
    /// </summary>
    extendMode: UINT32;

    /// <summary>
    /// Number of gradient stops. Use the IDWritePaintReader::GetGradientStops method to
    /// get the gradient stops.
    /// </summary>
    gradientStopCount: UINT32;

    /// <summary>
    /// Center X coordinate.
    /// </summary>
    centerX: Single;

    /// <summary>
    /// Center Y coordinate.
    /// </summary>
    centerY: Single;

    /// <summary>
    /// Start of the angular range of the gradient, measured in counter-clockwise degrees
    /// from the direction of the positive x axis.
    /// </summary>
    startAngle: Single;

    /// <summary>
    /// End of the angular range of the gradient, measured in counter-clockwise degrees
    /// from the direction of the positive x axis.
    /// </summary>
    endAngle: Single;
  end;
  {$EXTERNALSYM PAINT_SWEEP_GRADIENT}

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_GLYPH.
  /// Specifies a glyph shape to be filled or, equivalently, a clip region.
  /// This paint element has one child element.
  /// </summary>
  /// <remarks>
  /// The child paint element defines how the glyph shape is filled. The child element can be a single paint
  /// element, such as a linear gradient. Or the child element can be the root of a visual tree to be rendered
  /// with the glyph shape as a clip region.
  /// This corresponds to a PaintGlyph record in the OpenType COLR table.
  /// </remarks>
  PAINT_GLYPH = record

    /// <summary>
    /// Glyph index of the glyph that defines the shape to be filled.
    /// </summary>
    glyphIndex: UINT32;
  end;
  {$EXTERNALSYM PAINT_GLYPH}

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_COLOR_GLYPH.
  /// Specifies another color glyph, used as a reusable component.
  /// This paint element has one child element, which is the root paint element of the specified color glyph.
  /// </summary>
  /// <remarks>
  /// This corresponds to a PaintColorGlyph record in the OpenType COLR table.
  /// </remarks>ord
  PAINT_COLOR_GLYPH = record

    /// <summary>
    /// Glyph index defining the shape to be filled.
    /// </summary>
    glyphIndex: UINT32;

    /// <summary>
    /// Clip box of the referenced color glyph, in ems. This is an empty rectangle of the color glyph does
    /// not specify a clip box. If it is not an empty rect, the client is required to clip the child content
    /// to this box.
    /// </summary>
    clipBox: D2D_RECT_F;
  end;
  {$EXTERNALSYM PAINT_COLOR_GLYPH}

  /// <summary>
  /// Valid for paint elements of type DWRITE_PAINT_TYPE_COMPOSITE.
  /// Combines the two child paint elements using the specified compositing or blending mode.
  /// This paint element has two child elements. The first child is the paint source. The
  /// second child is the paint destination (or backdrop).
  /// </summary>
  /// <remarks>
  /// This corresponds to a PaintComposite record in the OpenType COLR table.
  /// </remarks>
  PAINT_COMPOSITE = record

    /// <summary>
    /// Specifies the compositing or blending mode.
    /// </summary>
    mode: DWRITE_COLOR_COMPOSITE_MODE;
  end;
  {$EXTERNALSYM PAINT_COMPOSITE}

  // Union type containing all possible paint element types
  PAINT_UNION = record
    case Integer of
      0: (layers: PAINT_LAYERS);
      1: (solidGlyph: PAINT_SOLID_GLYPH);
      2: (solid: DWRITE_PAINT_COLOR);
      3: (linearGradient: PAINT_LINEAR_GRADIENT);
      4: (radialGradient: PAINT_RADIAL_GRADIENT);
      5: (sweepGradient: PAINT_SWEEP_GRADIENT);
      6: (glyph: PAINT_GLYPH);
      7: (colorGlyph: PAINT_COLOR_GLYPH);
      8: (transform: DWRITE_MATRIX);
      9: (composite: PAINT_COMPOSITE);
  end;
  {$EXTERNALSYM PAINT_UNION}

  // Main DWRITE_PAINT_ELEMENT record

  /// <summary>
  /// Specifies properties of a paint element, which is one node in a visual tree associated
  /// with a color glyph. This is passed as an output parameter to various IDWritePaintReader
  /// methods.
  /// </summary>
  /// <remarks>
  /// For a detailed description of how paint elements should be rendered, see the OpenType COLOR
  /// table specification. Comments below reference the COLR paint record formats associated with
  /// each paint type.
  ///
  /// Note that this structure (and its size) may differ for different versions of the API, as
  /// newer versions may have additional union members for new paint types. For this reason,
  /// IDWritePaintReader methods that take a DWRITE_PAINT_ELEMENT output parameter also take a
  /// structSize parameter, for which the caller should specify actual size of the structure
  /// allocated by the caller, i.e., sizeof(DWRITE_PAINT_ELEMENT). Clients should use caution
  /// when passing DWRITE_PAINT_ELEMENT objects between components that may have been compiled
  /// against different versions of this header file.
  /// </remarks>
  PDWRITE_PAINT_ELEMENT = ^DWRITE_PAINT_ELEMENT;
  DWRITE_PAINT_ELEMENT = record
    /// <summary>
    /// Specifies the paint type, and thus which member of the union is valid.
    /// </summary>
    paintType: DWRITE_PAINT_TYPE;

    /// <summary>
    /// Specifies type-specific properties of the paint element.
    /// </summary>
    paint: PAINT_UNION;
  end;


  PD2D1_GRADIENT_STOP = ^D2D1_GRADIENT_STOP;
  D2D1_GRADIENT_STOP = record
    position: Single;
    color: D2D_COLOR_F;
  end;

  /// <summary>
  /// Interface used to read color glyph data for a specific font. A color glyph is
  /// represented as a visual tree of paint elements.
  /// </summary>
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWritePaintReader);'}
  {$EXTERNALSYM IDWritePaintReader}
  IDWritePaintReader = interface(IUnknown)
  ['{8128E912-3B97-42A5-AB6C-24AAD3A86E54}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function SetCurrentGlyph(glyphIndex: UINT32; paintElement: PDWRITE_PAINT_ELEMENT; structSize: UINT32; clipBox: PD2D_RECT_F; glyphAttributes: PDWRITE_PAINT_ATTRIBUTES): HRESULT; stdcall; overload;

    function SetTextColor(textColor: PDWRITE_COLOR_F): HRESULT; stdcall; overload;

    function SetColorPaletteIndex(colorPaletteIndex: UINT32): HRESULT; stdcall; overload;

    function SetCustomColorPalette(paletteEntries: PDWRITE_COLOR_F; paletteEntryCount: UINT32): HRESULT; stdcall; overload;

    function MoveToFirstChild(paintElement: PDWRITE_PAINT_ELEMENT; structSize: UINT32): HRESULT; stdcall; overload;

    function MoveToNextSibling(paintElement: PDWRITE_PAINT_ELEMENT; structSize: UINT32): HRESULT; stdcall; overload;

    function MoveToParent(): HRESULT; stdcall; overload;

    function GetGradientStops(firstGradientStopIndex: UINT32; gradientStopCount: UINT32; gradientStops: PD2D1_GRADIENT_STOP): HRESULT; stdcall; overload;

    function GetGradientStopColors(firstGradientStopIndex: UINT32; gradientStopCount: UINT32; gradientStopColors: PDWRITE_PAINT_COLOR): HRESULT; stdcall; overload;

  end;
  IID_IDWritePaintReader = IDWritePaintReader;
  {$EXTERNALSYM IID_IDWritePaintReader}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace7);'}
  {$EXTERNALSYM IDWriteFontFace7}
  IDWriteFontFace7 = interface(IDWriteFontFace6)
  ['{3945B85B-BC95-40F7-B72C-8B73BFC7E13B}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetPaintFeatureLevel(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS): DWRITE_PAINT_FEATURE_LEVEL; stdcall; overload;

    function CreatePaintReader(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS; paintFeatureLevel: DWRITE_PAINT_FEATURE_LEVEL; out paintReader: IDWritePaintReader): HRESULT; stdcall; overload;

  end;
  IID_IDWriteFontFace7 = IDWriteFontFace7;
  {$EXTERNALSYM IID_IDWriteFontFace7}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory8);'}
  {$EXTERNALSYM IDWriteFactory8}
  IDWriteFactory8 = interface(IDWriteFactory7)
  ['{EE0A7FB5-DEF4-4C23-A454-C9C7DC878398}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function TranslateColorGlyphRun(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION; desiredGlyphImageFormats: DWRITE_GLYPH_IMAGE_FORMATS; paintFeatureLevel: DWRITE_PAINT_FEATURE_LEVEL; measuringMode: DWRITE_MEASURING_MODE; worldAndDpiTransform: PDWRITE_MATRIX; colorPaletteIndex: UINT32; out colorEnumerator: IDWriteColorGlyphRunEnumerator1): HRESULT; stdcall; overload;

  end;
  IID_IDWriteFactory8 = IDWriteFactory8;
  {$EXTERNALSYM IID_IDWriteFactory8}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteBitmapRenderTarget3);'}
  {$EXTERNALSYM IDWriteBitmapRenderTarget3}
  IDWriteBitmapRenderTarget3 = interface(IDWriteBitmapRenderTarget2)
  ['{AEEC37DB-C337-40F1-8E2A-9A41B167B238}']

    // Native slot order verified against Microsoft Windows projection; inline C++ helpers are not slots.
    function GetPaintFeatureLevel(): DWRITE_PAINT_FEATURE_LEVEL; stdcall; overload;

    function DrawPaintGlyphRun(baselineOriginX: Single; baselineOriginY: Single; measuringMode: DWRITE_MEASURING_MODE; glyphRun: PDWRITE_GLYPH_RUN; glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS; textColor: COLORREF; colorPaletteIndex: UINT32; blackBoxRect: PRECT): HRESULT; stdcall; overload;

    function DrawGlyphRunWithColorSupport(baselineOriginX: Single; baselineOriginY: Single; measuringMode: DWRITE_MEASURING_MODE; glyphRun: PDWRITE_GLYPH_RUN; renderingParams: IDWriteRenderingParams; textColor: COLORREF; colorPaletteIndex: UINT32; blackBoxRect: PRECT): HRESULT; stdcall; overload;

  end;
  IID_IDWriteBitmapRenderTarget3 = IDWriteBitmapRenderTarget3;
  {$EXTERNALSYM IID_IDWriteBitmapRenderTarget3}


  // Additional Prototypes for ALL interfaces


  /// <summary>
  /// Example of how to use interface IDWritePaintReader in Delphi.
  /// </summary>
  ///
  ///type

  ///  TWritePaintReader = class(TInterfacedObject, IDWritePaintReader)
  ///  public
  ///    function SetCurrentGlyph(glyphIndex: UINT32;
  ///                             out paintElement: DWRITE_PAINT_ELEMENT;
  ///                             structSize: UINT32;
  ///                             out clipBox: D2D_RECT_F;
  ///                             out glyphAttributes: DWRITE_PAINT_ATTRIBUTES): HResult; stdcall; overload;
  ///
  ///    function SetCurrentGlyph(glyphIndex: UINT32;
  ///                             out paintElement: DWRITE_PAINT_ELEMENT;
  ///                             out clipBox: D2D_RECT_F;
  ///                             out glyphAttributes: DWRITE_PAINT_ATTRIBUTES): HRESULT; stdcall; overload;
  ///
  ///    function SetTextColor(textColor: DWRITE_COLOR_F): HResult stdcall;
  ///
  ///    function SetColorPaletteIndex(colorPaletteIndex: UINT32): HResult stdcall;
  ///
  ///    function SetCustomColorPalette(paletteEntries: PDWRITE_COLOR_F;
  ///                                   paletteEntryCount: UINT32): HResult stdcall;
  ///
  ///    function MoveToFirstChild(out paintElement: DWRITE_PAINT_ELEMENT;
  ///                              out structSize: UINT32): HResult stdcall;
  ///
  ///    function MoveToNextSibling(paintElement: DWRITE_PAINT_ELEMENT;
  ///                               structSize: UINT32): HResult stdcall;
  ///
  ///    function MoveToParent(): HResult stdcall;
  ///
  ///    function GetGradientStops(firstGradientStopIndex: UINT32;
  ///                              gradientStopCount: UINT32;
  ///                              out gradientStops: D2D1_GRADIENT_STOP): HResult stdcall;
  ///
  ///    function GetGradientStopColors(firstGradientStopIndex: UINT32;
  ///                                   gradientStopCount: UINT32;
  ///                                   out gradientStopColors: DWRITE_PAINT_COLOR): HResult stdcall;
  ///  end;
  ///

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


///
/// Example of how to use interface IDWritePaintReader implementation in Delphi.
///


end.


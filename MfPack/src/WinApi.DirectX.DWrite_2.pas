// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectWrite
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DWrite_2.pas
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
// Source: dwrite_2.h
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
unit WinApi.DirectX.DWrite_2;

  {$HPPEMIT '#include "dwrite_2.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.D3D9Types,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DWrite,
  WinApi.DirectX.DWrite_1;

  {$WEAKPACKAGEUNIT ON}

// Enums =======================================================================

type
  // How to align glyphs to the margin.
  PDWRITE_OPTICAL_ALIGNMENT = ^DWRITE_OPTICAL_ALIGNMENT;
  DWRITE_OPTICAL_ALIGNMENT = DWord;
  {$EXTERNALSYM DWRITE_OPTICAL_ALIGNMENT}
const
  // Align to the default metrics of the glyph.
  DWRITE_OPTICAL_ALIGNMENT_NONE = DWRITE_OPTICAL_ALIGNMENT(0);
  {$EXTERNALSYM DWRITE_OPTICAL_ALIGNMENT_NONE}
  // Align glyphs to the margins. Without this, some small whitespace
  // may be present between the text and the margin from the glyph's side
  // bearing values. Note that glyphs may still overhang outside the
  // margin, such as flourishes or italic slants.
  DWRITE_OPTICAL_ALIGNMENT_NO_SIDE_BEARINGS = DWRITE_OPTICAL_ALIGNMENT(1);
  {$EXTERNALSYM DWRITE_OPTICAL_ALIGNMENT_NO_SIDE_BEARINGS}

type
  // Whether to enable grid-fitting of glyph outlines (a.k.a. hinting).
  PDWRITE_GRID_FIT_MODE = ^DWRITE_GRID_FIT_MODE;
  DWRITE_GRID_FIT_MODE = DWord;
  {$EXTERNALSYM DWRITE_GRID_FIT_MODE}
const
  // Choose grid fitting base on the font's gasp table information.
  DWRITE_GRID_FIT_MODE_DEFAULT = DWRITE_GRID_FIT_MODE(0);
  {$EXTERNALSYM DWRITE_GRID_FIT_MODE_DEFAULT}
  // Always disable grid fitting, using the ideal glyph outlines.
  DWRITE_GRID_FIT_MODE_DISABLED = DWRITE_GRID_FIT_MODE(1);
  {$EXTERNALSYM DWRITE_GRID_FIT_MODE_DISABLED}
  // Enable grid fitting, adjusting glyph outlines for device pixel display.
  DWRITE_GRID_FIT_MODE_ENABLED = DWRITE_GRID_FIT_MODE(2);
  {$EXTERNALSYM DWRITE_GRID_FIT_MODE_ENABLED}


// =============================================================================

type

  // Forward interfaces

  PIDWriteFontFallback = ^IDWriteFontFallback;
  IDWriteFontFallback = interface;



  // Overall metrics associated with text after layout.
  // All coordinates are in device independent pixels (DIPs).
  PDWRITE_TEXT_METRICS1 = ^DWRITE_TEXT_METRICS1;
  DWRITE_TEXT_METRICS1 = record
    DWriteTextMetrics : DWRITE_TEXT_METRICS;

    // The height of the formatted text taking into account the
    // trailing whitespace at the end of each line, which will
    // matter for vertical reading directions.
    heightIncludingTrailingWhitespace: Single;
  end;
  {$EXTERNALSYM DWRITE_TEXT_METRICS1}


  // Interface IDWriteTextRenderer1
  // ==============================
  // The text renderer interface represents a set of application-defined
  // callbacks that perform rendering of text, inline objects, and decorations
  // such as underlines.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextRenderer1);'}
  {$EXTERNALSYM IDWriteTextRenderer1}
  IDWriteTextRenderer1 = interface(IDWriteTextRenderer)
  ['{D3E0E934-22A0-427E-AAE4-7D9574B59DB1}']

    // IDWriteTextLayout.Draw calls this function to instruct the client to
    // render a run of glyphs.
    // param name = clientDrawingContext: The context passed to
    //     IDWriteTextLayout.Draw.
    // param name = baselineOriginX: X-coordinate of the baseline.
    // param name = baselineOriginY: Y-coordinate of the baseline.
    // param name = orientationAngle: Orientation of the glyph run.
    // param name = measuringMode: Specifies measuring method for glyphs in
    //     the run. Renderer implementations may choose different rendering
    //     modes for given measuring methods, but best results are seen when
    //     the rendering mode matches the corresponding measuring mode:
    //     DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL for DWRITE_MEASURING_MODE_NATURAL
    //     DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC for DWRITE_MEASURING_MODE_GDI_CLASSIC
    //     DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL for DWRITE_MEASURING_MODE_GDI_NATURAL
    //
    // param name = glyphRun: The glyph run to draw.
    // param name = glyphRunDescription: Properties of the characters
    //     associated with this run.
    // param name = clientDrawingEffect: The drawing effect set in
    //     IDWriteTextLayout.SetDrawingEffect.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // If a non-identity orientation is passed, the glyph run should be
    // rotated around the given baseline x and y coordinates. The function
    // IDWriteAnalyzer2.GetGlyphOrientationTransform will return the
    // necessary transform for you, which can be combined with any existing
    // world transform on the drawing context.
    // </remarks>
    function DrawGlyphRun(clientDrawingContext: Pointer;
                          baselineOriginX: Single;
                          baselineOriginY: Single;
                          orientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                          measuringMode: DWRITE_MEASURING_MODE;
                          glyphRun: DWRITE_GLYPH_RUN;
                          glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                          clientDrawingEffect: IUnknown = IUnknown(Nil)): HResult; stdcall;

    // IDWriteTextLayout.Draw calls this function to instruct the client to draw
    // an underline.
    // param name = clientDrawingContext: The context passed to
    // IDWriteTextLayout.Draw.
    // param name = baselineOriginX: X-coordinate of the baseline.
    // param name = baselineOriginY: Y-coordinate of the baseline.
    // param name = orientationAngle: Orientation of the underline.
    // param name = underline: Underline logical information.
    // param name = clientDrawingEffect: The drawing effect set in
    //     IDWriteTextLayout.SetDrawingEffect.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // A single underline can be broken into multiple calls, depending on
    // how the formatting changes attributes. If font sizes/styles change
    // within an underline, the thickness and offset will be averaged
    // weighted according to characters.
    //
    // To get the correct top coordinate of the underline rect, add
    // underline.offset to the baseline's Y. Otherwise the underline will
    // be immediately under the text. The x coordinate will always be passed
    // as the left side, regardless of text directionality. This simplifies
    // drawing and reduces the problem of round-off that could potentially
    // cause gaps or a double stamped alpha blend. To avoid alpha overlap,
    // round the end points to the nearest device pixel.
    // </remarks>
    function DrawUnderline(clientDrawingContext: Pointer;
                           baselineOriginX: Single;
                           baselineOriginY: Single;
                           orientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                           underline: DWRITE_UNDERLINE;
                           clientDrawingEffect: IUnknown = IUnknown(Nil)): HResult; stdcall;

    // IDWriteTextLayout.Draw calls this function to instruct the client to draw
    // a strikethrough.
    // param name = clientDrawingContext: The context passed to
    // IDWriteTextLayout.Draw.
    // param name = baselineOriginX: X-coordinate of the baseline.
    // param name = baselineOriginY: Y-coordinate of the baseline.
    // param name = orientationAngle: Orientation of the strikethrough.
    // param name = strikethrough: Strikethrough logical information.
    // param name = clientDrawingEffect: The drawing effect set in
    //     IDWriteTextLayout.SetDrawingEffect.
    // returns
    // Standard HRESULT error code.
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
                               orientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                               strikethrough: DWRITE_STRIKETHROUGH;
                               clientDrawingEffect: IUnknown = IUnknown(Nil)): HResult; stdcall;

    // IDWriteTextLayout.Draw calls this application callback when it needs to
    // draw an inline object.
    // param name = clientDrawingContext: The context passed to
    //     IDWriteTextLayout.Draw.
    // param name = originX: X-coordinate at the top-left corner of the
    //     inline object.
    // param name = originY: Y-coordinate at the top-left corner of the
    //     inline object.
    // param name = orientationAngle: Orientation of the inline object.
    // param name = inlineObject: The object set using IDWriteTextLayout.SetInlineObject.
    // param name = isSideways: The object should be drawn on its side.
    // param name = isRightToLeft: The object is in an right-to-left context
    //     and should be drawn flipped.
    // param name = clientDrawingEffect: The drawing effect set in
    //     IDWriteTextLayout.SetDrawingEffect.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // The right-to-left flag is a hint to draw the appropriate visual for
    // that reading direction. For example, it would look strange to draw an
    // arrow pointing to the right to indicate a submenu. The sideways flag
    // similarly hints that the object is drawn in a different orientation.
    // If a non-identity orientation is passed, the top left of the inline
    // object should be rotated around the given x and y coordinates.
    // IDWriteAnalyzer2.GetGlyphOrientationTransform returns the necessary
    // transform for this.
    // </remarks>
    function DrawInlineObject(clientDrawingContext: Pointer;
                              originX: Single;
                              originY: Single;
                              orientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                              inlineObject: PIDWriteInlineObject;
                              isSideways: BOOL;
                              isRightToLeft: BOOL;
                              clientDrawingEffect: IUnknown = IUnknown(Nil)): HResult; stdcall;

  end;
  IID_IDWriteTextRenderer1 = IDWriteTextRenderer1;
  {$EXTERNALSYM IID_IDWriteTextRenderer1}


  // Interface IDWriteTextFormat1
  // ============================
  // The format of text used for text layout.
  // <remarks>
  // This object may not be thread-safe and it may carry the state of text format change.
  // </remarks>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextFormat1);'}
  {$EXTERNALSYM IDWriteTextFormat1}
  IDWriteTextFormat1 = interface(IDWriteTextFormat)
  ['{5F174B49-0D8B-4CFB-8BCA-F1CCE9D06C67}']

    // Set the preferred orientation of glyphs when using a vertical reading direction.
    // param name = glyphOrientation: Preferred glyph orientation.
    // returns
    // Standard HRESULT error code.
    function SetVerticalGlyphOrientation(glyphOrientation: DWRITE_VERTICAL_GLYPH_ORIENTATION): HResult; stdcall;

    // Get the preferred orientation of glyphs when using a vertical reading
    // direction.
    function GetVerticalGlyphOrientation(): DWRITE_VERTICAL_GLYPH_ORIENTATION; stdcall;

    // Set whether or not the last word on the last line is wrapped.
    // param name = isLastLineWrappingEnabled: Line wrapping option.
    // returns
    // Standard HRESULT error code.
    function SetLastLineWrapping(isLastLineWrappingEnabled: BOOL): HResult; stdcall;

    // Get whether or not the last word on the last line is wrapped.
    function GetLastLineWrapping(): BOOL; stdcall;

    // Set how the glyphs align to the edges the margin. Default behavior is
    // to align glyphs using their default glyphs metrics which include side
    // bearings.
    // param name = opticalAlignment: Optical alignment option.
    // returns
    // Standard HRESULT error code.
    function SetOpticalAlignment(opticalAlignment: DWRITE_OPTICAL_ALIGNMENT): HResult; stdcall;

    // Get how the glyphs align to the edges the margin.
    function GetOpticalAlignment(): DWRITE_OPTICAL_ALIGNMENT; stdcall;

    // Apply a custom font fallback onto layout. If none is specified,
    // layout uses the system fallback list.
    // param name = fontFallback: Custom font fallback created from
    //     IDWriteFontFallbackBuilder.CreateFontFallback or from
    //     IDWriteFactory2.GetSystemFontFallback.
    // returns
    // Standard HRESULT error code.
    function SetFontFallback(fontFallback: IDWriteFontFallback): HResult; stdcall;

    // Get the current font fallback object.
    function GetFontFallback(fontFallback: IDWriteFontFallback): HResult; stdcall;

  end;
  IID_IDWriteTextFormat1 = IDWriteTextFormat1;
  {$EXTERNALSYM IID_IDWriteTextFormat1}


  // All coordinates are in device independent pixels (DIPs).
  // Interface IDWriteTextLayout2
  // ============================
  // The text layout interface represents a block of text after it has
  // been fully analyzed and formatted.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextLayout2);'}
  {$EXTERNALSYM IDWriteTextLayout2}
  IDWriteTextLayout2 = interface(IDWriteTextLayout1)
  ['{1093C18F-8D5E-43F0-B064-0917311B525E}']
    // GetMetrics retrieves overall metrics for the formatted string.
    // param name = textMetrics: The returned metrics.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // Drawing effects like underline and strikethrough do not contribute
    // to the text size, which is essentially the sum of advance widths and
    // line heights. Additionally, visible swashes and other graphic
    // adornments may extend outside the returned width and height.
    // </remarks>
    function GetMetrics(out textMetrics: DWRITE_TEXT_METRICS1): HResult; stdcall;


    // Set the preferred orientation of glyphs when using a vertical reading direction.
    // param name = glyphOrientation: Preferred glyph orientation.
    // returns
    // Standard HRESULT error code.
    function SetVerticalGlyphOrientation(glyphOrientation: DWRITE_VERTICAL_GLYPH_ORIENTATION): HResult; stdcall;

    // Get the preferred orientation of glyphs when using a vertical reading
    // direction.
    function GetVerticalGlyphOrientation(): DWRITE_VERTICAL_GLYPH_ORIENTATION; stdcall;

    // Set whether or not the last word on the last line is wrapped.
    // param name = isLastLineWrappingEnabled: Line wrapping option.
    // returns
    // Standard HRESULT error code.
    function SetLastLineWrapping(isLastLineWrappingEnabled: BOOL): HResult; stdcall;

    // Get whether or not the last word on the last line is wrapped.
    function GetLastLineWrapping(): BOOL; stdcall;

    // Set how the glyphs align to the edges the margin. Default behavior is
    // to align glyphs using their default glyphs metrics which include side
    // bearings.
    // param name = opticalAlignment: Optical alignment option.
    // returns
    // Standard HRESULT error code.
    function SetOpticalAlignment(opticalAlignment: DWRITE_OPTICAL_ALIGNMENT): HResult; stdcall;

    // Get how the glyphs align to the edges the margin.
    function GetOpticalAlignment(): DWRITE_OPTICAL_ALIGNMENT; stdcall;

    // Apply a custom font fallback onto layout. If none is specified,
    // layout uses the system fallback list.
    // param name = fontFallback: Custom font fallback created from
    //     IDWriteFontFallbackBuilder.CreateFontFallback or
    //     IDWriteFactory2.GetSystemFontFallback.
    // returns
    // Standard HRESULT error code.
    function SetFontFallback(fontFallback: IDWriteFontFallback): HResult; stdcall;

    // Get the current font fallback object.
    function GetFontFallback(out fontFallback: IDWriteFontFallback): HResult; stdcall;

  end;
  IID_IDWriteTextLayout2 = IDWriteTextLayout2;
  {$EXTERNALSYM IID_IDWriteTextLayout2}


  // Interface IDWriteTextAnalyzer2
  // ==============================
  // The text analyzer interface represents a set of application-defined
  // callbacks that perform rendering of text, inline objects, and decorations
  // such as underlines.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteTextAnalyzer2);'}
  {$EXTERNALSYM IDWriteTextAnalyzer2}
  IDWriteTextAnalyzer2 = interface(IDWriteTextAnalyzer1)
  ['{553A9FF3-5693-4DF7-B52B-74806F7F2EB9}']
    // Returns 2x3 transform matrix for the respective angle to draw the
    // glyph run or other object.
    // param name = glyphOrientationAngle: The angle reported to one of the application callbacks,
    //     including IDWriteTextAnalysisSink1.SetGlyphOrientation and IDWriteTextRenderer1.Draw*.
    // param name = isSideways: Whether the run's glyphs are sideways or not.
    // param name = originX: X origin of the element, be it a glyph run or underline or other.
    // param name = originY: Y origin of the element, be it a glyph run or underline or other.
    // param name = transform: Returned transform.
    // returns
    // Standard HRESULT error code.
    // <remarks>
    // This rotates around the given origin x and y, returning a translation component
    // such that the glyph run, text decoration, or inline object is drawn with the
    // right orientation at the expected coordinate.
    // </remarks>
    function GetGlyphOrientationTransform(glyphOrientationAngle: DWRITE_GLYPH_ORIENTATION_ANGLE;
                                          isSideways: BOOL;
                                          originX: Single;
                                          originY: Single;
                                          out transform: DWRITE_MATRIX): HResult; stdcall;

    // Returns a list of typographic feature tags for the given script and language.
    // param name = fontFace: The font face to get features from.
    // param name = scriptAnalysis: Script analysis result from AnalyzeScript.
    // param name = localeName: The locale to use when selecting the feature,
    //     such en-us or ja-jp.
    // param name = maxTagCount: Maximum tag count.
    // param name = actualTagCount: Actual tag count. If greater than
    //     maxTagCount, E_NOT_SUFFICIENT_BUFFER is returned, and the call
    //     should be retried with a larger buffer.
    // param name = tags: Feature tag list.
    // returns
    // Standard HRESULT error code.
    function GetTypographicFeatures(fontFace: IDWriteFontFace;
                                    scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                                    localeName: PWideChar;
                                    maxTagCount: UINT32;
                                    out actualTagCount: UINT32;
                                    out tags: PDWRITE_FONT_FEATURE_TAG): HResult; stdcall;

    // Returns an array of which glyphs are affected by a given feature.
    // param name = fontFace: The font face to read glyph information from.
    // param name = scriptAnalysis: Script analysis result from AnalyzeScript.
    // param name = localeName: The locale to use when selecting the feature,
    //     such en-us or ja-jp.
    // param name = featureTag: OpenType feature name to use, which may be one
    //     of the DWRITE_FONT_FEATURE_TAG values or a custom feature using
    //     DWRITE_MAKE_OPENTYPE_TAG.
    // param name = glyphCount: Number of glyph indices to check.
    // param name = glyphIndices: Glyph indices to check for feature application.
    // param name = featureApplies: Output of which glyphs are affected by the
    //     feature, where for each glyph affected, the respective array index
    //     will be 1. The result is returned per-glyph without regard to
    //     neighboring context of adjacent glyphs.
    // </remarks>
    // returns
    // Standard HRESULT error code.
    function CheckTypographicFeature(fontFace: IDWriteFontFace;
                                     scriptAnalysis: DWRITE_SCRIPT_ANALYSIS;
                                     localeName: PWideChar;
                                     featureTag: DWRITE_FONT_FEATURE_TAG;
                                     glyphCount: UINT32;
                                     glyphIndices: UINT16;
                                     featureApplies: PByte): HResult; stdcall;

  end;
  IID_IDWriteTextAnalyzer2 = IDWriteTextAnalyzer2;
  {$EXTERNALSYM IID_IDWriteTextAnalyzer2}


  // Interface IDWriteFontFallback
  // =============================
  // A font fallback definition used for mapping characters to fonts capable of
  // supporting them.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFallback);'}
  {$EXTERNALSYM IDWriteFontFallback}
  IDWriteFontFallback = interface(IUnknown)
  ['{EFA008F9-F7A1-48BF-B05C-F224713CC0FF}']

    // Determines an appropriate font to use to render the range of text.
    // param name = source: The text source implementation holds the text and
    //     locale.
    // param name = textLength: Length of the text to analyze.
    // param name = baseFontCollection: Default font collection to use.
    // param name = baseFamilyName: Family name of the base font. If you pass
    //     null, no matching will be done against the family.
    // param name = baseWeight: Desired weight.
    // param name = baseStyle: Desired style.
    // param name = baseStretch: Desired stretch.
    // param name = mappedLength: Length of text mapped to the mapped font.
    //     This will always be less or equal to the input text length and
    //     greater than zero (if the text length is non-zero) so that the
    //     caller advances at least one character each call.
    // param name = mappedFont: The font that should be used to render the
    //     first mappedLength characters of the text. If it returns NULL,
    //     then no known font can render the text, and mappedLength is the
    //     number of unsupported characters to skip.
    // param name = scale: Scale factor to multiply the em size of the
    //     returned font by.
    // returns
    // Standard HRESULT error code.
    function MapCharacters(analysisSource: IDWriteTextAnalysisSource;
                           textPosition: UINT32;
                           textLength: UINT32;
                           baseFontCollection: IDWriteFontCollection;
                           baseFamilyName: PWideChar;
                           baseWeight: DWRITE_FONT_WEIGHT;
                           baseStyle: DWRITE_FONT_STYLE;
                           baseStretch: DWRITE_FONT_STRETCH;
                           mappedLength: UINT32;
                           mappedFont: PIDWriteFont;
                           out scale: Single): HResult; stdcall;

  end;
  IID_IDWriteFontFallback = IDWriteFontFallback;
  {$EXTERNALSYM IID_IDWriteFontFallback}


  // Interface IDWriteFontFallbackBuilder
  // ====================================
  // Builder used to create a font fallback definition by appending a series of
  // fallback mappings, followed by a creation call.
  // remarks
  // This object may not be thread-safe.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFallbackBuilder);'}
  {$EXTERNALSYM IDWriteFontFallbackBuilder}
  IDWriteFontFallbackBuilder = interface(IUnknown)
  ['{FD882D06-8ABA-4FB8-B849-8BE8B73E14DE}']
    // Appends a single mapping to the list. Call this once for each additional mapping.
    // param name = ranges: Unicode ranges that apply to this mapping.
    // param name = rangesCount: Number of Unicode ranges.
    // param name = localeName: Locale of the context (e.g. document locale).
    // param name = baseFamilyName: Base family name to match against, if applicable.
    // param name = fontCollection: Explicit font collection for this mapping (optional).
    // param name = targetFamilyNames: List of target family name strings.
    // param name = targetFamilyNamesCount: Number of target family names.
    // param name = scale: Scale factor to multiply the result target font by.
    // returns
    // Standard HRESULT error code.
    function AddMapping(ranges: PDWRITE_UNICODE_RANGE;
                        rangesCount: UINT32;
                        targetFamilyNames: PWideChar;
                        targetFamilyNamesCount: UINT32;
                        fontCollection: IDWriteFontCollection = Nil;
                        localeName: PWideChar = Nil;
                        baseFamilyName: PWideChar = Nil;
                        scale: Single = 1.0): HResult; stdcall;

    // Appends all the mappings from an existing font fallback object.
    // param name = fontFallback: Font fallback to read mappings from.
    // returns
    // Standard HRESULT error code.
    function AddMappings(rfontFallback: IDWriteFontFallback): HResult; stdcall;

    // Creates the finalized fallback object from the mappings added.
    // param name = fontFallback: Created fallback list.
    // returns
    // Standard HRESULT error code.
    function CreateFontFallback(fontFallback: IDWriteFontFallback): HResult; stdcall;

  end;
  IID_IDWriteFontFallbackBuilder = IDWriteFontFallbackBuilder;
  {$EXTERNALSYM IID_IDWriteFontFallbackBuilder}

// DWRITE_COLOR_F
//#ifndef D3DCOLORVALUE_DEFINED

//typedef struct _D3DCOLORVALUE {
//    union {
//    Single r;
//    Single dvR;
//    };
//    union {
//    Single g;
//    Single dvG;
//    };
//    union {
//    Single b;
//    Single dvB;
//    };
//    union {
//    Single a;
//    Single dvA;
//    };
//} D3DCOLORVALUE;

//#define D3DCOLORVALUE_DEFINED
//#endif // D3DCOLORVALUE_DEFINED


  PDWRITE_COLOR_F = ^D3DCOLORVALUE;
  DWRITE_COLOR_F = D3DCOLORVALUE;
  {$EXTERNALSYM DWRITE_COLOR_F}


  // Interface IDWriteFont2
  // ======================
  // The IDWriteFont interface represents a physical font in a font collection.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFont2);'}
  {$EXTERNALSYM IDWriteFont2}
  IDWriteFont2 = interface(IDWriteFont1)
  ['{29748ed6-8c9c-4a6a-be0b-d912e8538944}']

    // Returns TRUE if the font contains tables that can provide color information
    // (including COLR, CPAL, SVG, CBDT, sbix  tables), or FALSE if not. Note that
    // TRUE is returned even in the case when the font tables contain only grayscale
    // images.
    function IsColorFont(): BOOL; stdcall;

  end;
  IID_IDWriteFont2 = IDWriteFont2;
  {$EXTERNALSYM IID_IDWriteFont2}


  // Interface IDWriteFontFace2
  // ==========================
  // The interface that represents an absolute reference to a font face.
  // It contains font face type, appropriate file references and face identification data.
  // Various font data such as metrics, names and glyph outlines is obtained from IDWriteFontFace.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFontFace2);'}
  {$EXTERNALSYM IDWriteFontFace2}
  IDWriteFontFace2 = interface(IDWriteFontFace1)
  ['{d8b768ff-64bc-4e66-982b-ec8e87f693f7}']

    // Returns TRUE if the font contains tables that can provide color information
    // (including COLR, CPAL, SVG, CBDT, sbix  tables), or FALSE if not. Note that
    // TRUE is returned even in the case when the font tables contain only grayscale
    // images.
    function IsColorFont(): BOOL; stdcall;

    // Returns the number of color palettes defined by the font. The return
    // value is zero if the font has no color information. Color fonts must
    // have at least one palette, with palette index zero being the default.
    function GetColorPaletteCount(): UINT32; stdcall;

    // Returns the number of entries in each color palette. All color palettes
    // in a font have the same number of palette entries. The return value is
    // zero if the font has no color information.
    function GetPaletteEntryCount(): UINT32; stdcall;

    // Reads color values from the font's color palette.
    // param name = colorPaletteIndex: Zero-based index of the color palette. If the
    // font does not have a palette with the specified index, the method returns
    // DWRITE_E_NOCOLOR.<param>
    // param name = firstEntryIndex: Zero-based index of the first palette entry
    // to read.
    // param name = entryCount: Number of palette entries to read.
    // param name = paletteEntries: Array that receives the color values.<param>
    // returns
    // Standard HRESULT error code.
    // The return value is E_INVALIDARG if firstEntryIndex + entryCount is greater
    // than the actual number of palette entries as returned by GetPaletteEntryCount.
    // The return value is DWRITE_E_NOCOLOR if the font does not have a palette
    // with the specified palette index.
    function GetPaletteEntries(colorPaletteIndex: UINT32;
                               firstEntryIndex: UINT32;
                               entryCount: UINT32;
                               paletteEntries: PDWRITE_COLOR_F): HResult; stdcall;

    // Determines the recommended text rendering and grid-fit mode to be used based on the
    // font, size, world transform, and measuring mode.
    // param name = fontEmSize: Logical font size in DIPs.
    // param name = dpiX: Number of pixels per logical inch in the horizontal direction.
    // param name = dpiY: Number of pixels per logical inch in the vertical direction.
    // param name = transform: Specifies the world transform.
    // param name = outlineThreshold: Specifies the quality of the graphics system's outline rendering,
    // affects the size threshold above which outline rendering is used.
    // param name = measuringMode: Specifies the method used to measure during text layout. For proper
    // glyph spacing, the function returns a rendering mode that is compatible with the specified
    // measuring mode.
    // param name = renderingParams: Rendering parameters object. This parameter is necessary in case the rendering parameters
    // object overrides the rendering mode.
    // param name = renderingMode: Receives the recommended rendering mode.
    // param name = gridFitMode: Receives the recommended grid-fit mode.
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
                                         transform: PDWRITE_MATRIX;
                                         isSideways: BOOL;
                                         outlineThreshold: DWRITE_OUTLINE_THRESHOLD;
                                         measuringMode: DWRITE_MEASURING_MODE;
                                         renderingParams: IDWriteRenderingParams;
                                         out renderingMode: DWRITE_RENDERING_MODE;
                                         out gridFitMode: DWRITE_GRID_FIT_MODE): HResult; stdcall;

  end;
  IID_IDWriteFontFace2 = IDWriteFontFace2;
  {$EXTERNALSYM IID_IDWriteFontFace2}


  // Represents a color glyph run. The IDWriteFactory2.TranslateColorGlyphRun
  // method returns an ordered collection of color glyph runs, which can be
  // layered on top of each other to produce a color representation of the
  // given base glyph run.
  PDWRITE_COLOR_GLYPH_RUN = ^DWRITE_COLOR_GLYPH_RUN;
  DWRITE_COLOR_GLYPH_RUN = record
    // Glyph run to render.
    glyphRun: DWRITE_GLYPH_RUN;
    // Optional glyph run description.
    glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION;
    // Location at which to draw this glyph run.
    baselineOriginX: Single;
    baselineOriginY: Single;
    // Color to use for this layer, if any. This is the same color that
    // IDWriteFontFace2.GetPaletteEntries would return for the current
    // palette index if the paletteIndex member is less than 0xFFFF. If
    // the paletteIndex member is 0xFFFF then there is no associated
    // palette entry, this member is set to { 0, 0, 0, 0 }, and the client
    // should use the current foreground brush.
    runColor: DWRITE_COLOR_F;
    // Zero-based index of this layer's color entry in the current color
    // palette, or 0xFFFF if this layer is to be rendered using
    // the current foreground brush.
    paletteIndex: UINT16;
  end;
  {$EXTERNALSYM DWRITE_COLOR_GLYPH_RUN}


  // Interface IDWriteColorGlyphRunEnumerator
  // ========================================
  // Enumerator for an ordered collection of color glyph runs.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteColorGlyphRunEnumerator);'}
  {$EXTERNALSYM IDWriteColorGlyphRunEnumerator}
  IDWriteColorGlyphRunEnumerator = interface(IUnknown)
  ['{d31fbe17-f157-41a2-8d24-cb779e0560e8}']

    // Advances to the first or next color run. The runs are enumerated
    // in order from back to front.
    // param name = hasRun: Receives TRUE if there is a current run or
    // FALSE if the end of the sequence has been reached.
    // returns
    // Standard HRESULT error code.
    function MoveNext(out hasRun: PBOOL): HResult; stdcall;

    // Gets the current color glyph run.
    // param name = colorGlyphRun: Receives a pointer to the color
    // glyph run. The pointer remains valid until the next call to
    // MoveNext or until the interface is released.
    // returns
    // Standard HRESULT error code. An error is returned if there is
    // no current glyph run, i.e., if MoveNext has not yet been called
    // or if the end of the sequence has been reached.
    function GetCurrentRun(colorGlyphRun: DWRITE_COLOR_GLYPH_RUN): HResult; stdcall;

  end;
  IID_IDWriteColorGlyphRunEnumerator = IDWriteColorGlyphRunEnumerator;
  {$EXTERNALSYM IID_IDWriteColorGlyphRunEnumerator}


  // Interface IDWriteRenderingParams2
  // =================================
  // The interface that represents text rendering settings for glyph rasterization and filtering.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteRenderingParams2);'}
  {$EXTERNALSYM IDWriteRenderingParams2}
  IDWriteRenderingParams2 = interface(IDWriteRenderingParams1)
  ['{F9D711C3-9777-40AE-87E8-3E5AF9BF0948}']

    // Gets the grid fitting mode.
    function GetGridFitMode(): DWRITE_GRID_FIT_MODE; stdcall;

  end;
  IID_IDWriteRenderingParams2 = IDWriteRenderingParams2;
  {$EXTERNALSYM IID_IDWriteRenderingParams2}


  // Interface IDWriteFactory2
  // =========================
  // The root factory interface for all DWrite objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDWriteFactory2);'}
  {$EXTERNALSYM IDWriteFactory2}
  IDWriteFactory2 = interface(IDWriteFactory1)
  ['{0439fc60-ca44-4994-8dee-3a9af7b732ec}']

    // Get the system-appropriate font fallback mapping list.
    // param name = fontFallback: The system fallback list.
    // returns
    // Standard HRESULT error code.
    function GetSystemFontFallback(fontFallback: IDWriteFontFallback): HResult; stdcall;

    // Create a custom font fallback builder.
    // param name = fontFallbackBuilder: Empty font fallback builder.
    // returns
    // Standard HRESULT error code.
    function CreateFontFallbackBuilder(fontFallbackBuilder: IDWriteFontFallbackBuilder): HResult; stdcall;

    // Translates a glyph run to a sequence of color glyph runs, which can be
    // rendered to produce a color representation of the original "base" run.
    // param name = baselineOriginX: Horizontal origin of the base glyph run in
    // pre-transform coordinates.
    // param name = baselineOriginY: Vertical origin of the base glyph run in
    // pre-transform coordinates.
    // param name = glyphRun: Pointer to the original "base" glyph run.
    // param name = glyphRunDescription: Optional glyph run description.
    // param name = measuringMode: Measuring mode, needed to compute the origins
    // of each glyph.
    // param name = worldToDeviceTransform: Matrix converting from the client's
    // coordinate space to device coordinates (pixels), i.e., the world transform
    // multiplied by any DPI scaling.
    // param name = colorPaletteIndex: Zero-based index of the color palette to use.
    // Valid indices are less than the number of palettes in the font, as returned
    // by IDWriteFontFace2.GetColorPaletteCount.
    // param name = colorLayers: If the function succeeds, receives a pointer
    // to an enumerator object that can be used to obtain the color glyph runs.
    // If the base run has no color glyphs, then the output pointer is NULL
    // and the method returns DWRITE_E_NOCOLOR.
    // returns
    // Returns DWRITE_E_NOCOLOR if the font has no color information, the base
    // glyph run does not contain any color glyphs, or the specified color palette
    // index is out of range. In this case, the client should render the base glyph
    // run. Otherwise, returns a standard HRESULT error code.
    function TranslateColorGlyphRun(baselineOriginX: Single;
                                    baselineOriginY: Single;
                                    glyphRun: DWRITE_GLYPH_RUN;
                                    glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    worldToDeviceTransform: PDWRITE_MATRIX;
                                    colorPaletteIndex: UINT32;
                                    out colorLayers: IDWriteColorGlyphRunEnumerator): HResult; stdcall;

    // Creates a rendering parameters object with the specified properties.
    // param name = gamma: The gamma value used for gamma correction, which must be greater than zero and cannot exceed 256.
    // param name = enhancedContrast: The amount of contrast enhancement, zero or greater.
    // param name = clearTypeLevel: The degree of ClearType level, from 0.0f (no ClearType) to 1.0f (full ClearType).
    // param name = pixelGeometry: The geometry of a device pixel.
    // param name = renderingMode: Method of rendering glyphs. In most cases, this should be DWRITE_RENDERING_MODE_DEFAULT to automatically use an appropriate mode.
    // param name = gridFitMode: How to grid fit glyph outlines. In most cases, this should be DWRITE_GRID_FIT_DEFAULT to automatically choose an appropriate mode.
    // param name = renderingParams: Holds the newly created rendering parameters object, or NULL in case of failure.
    // returns
    // Standard HRESULT error code.
    function CreateCustomRenderingParams(gamma: Single;
                                         enhancedContrast: Single;
                                         grayscaleEnhancedContrast: Single;
                                         clearTypeLevel: Single;
                                         pixelGeometry: DWRITE_PIXEL_GEOMETRY;
                                         renderingMode: DWRITE_RENDERING_MODE;
                                         gridFitMode: DWRITE_GRID_FIT_MODE;
                                         out renderingParams: IDWriteRenderingParams2): HResult; stdcall;


    // Creates a glyph run analysis object, which encapsulates information
    // used to render a glyph run.
    // param name = glyphRun: Structure specifying the properties of the glyph run.
    // param name = transform: Optional transform applied to the glyphs and their positions. This transform is applied after the
    // scaling specified by the emSize and pixelsPerDip.
    // param name = renderingMode: Specifies the rendering mode, which must be one of the raster rendering modes (i.e., not default
    // and not outline).
    // param name = measuringMode: Specifies the method to measure glyphs.
    // param name = gridFitMode: How to grid-fit glyph outlines. This must be non-default.
    // param name = baselineOriginX: Horizontal position of the baseline origin, in DIPs.
    // param name = baselineOriginY: Vertical position of the baseline origin, in DIPs.
    // param name = glyphRunAnalysis: Receives a pointer to the newly created object.
    // returns
    // Standard HRESULT error code.
    function CreateGlyphRunAnalysis(glyphRun: DWRITE_GLYPH_RUN;
                                    transform: PDWRITE_MATRIX;
                                    renderingMode: DWRITE_RENDERING_MODE;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    gridFitMode: DWRITE_GRID_FIT_MODE;
                                    antialiasMode: DWRITE_TEXT_ANTIALIAS_MODE;
                                    baselineOriginX: Single;
                                    baselineOriginY: Single;
                                    out glyphRunAnalysis: IDWriteGlyphRunAnalysis): HResult; stdcall;

  end;
  IID_IDWriteFactory2 = IDWriteFactory2;
  {$EXTERNALSYM IID_IDWriteFactory2}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.


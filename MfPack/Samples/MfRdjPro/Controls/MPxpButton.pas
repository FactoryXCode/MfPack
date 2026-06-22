// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MPxpButton.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 3.2.0
// Description: Single visual component: Hybrid button/checkbox.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 11/04/2026 All                 BauHaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.code
//
// Copyright (c) FactoryX. All rights reserved.
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
unit MPxpButton;

interface

uses

  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.ExtCtrls,
  {Application}
  MPxpButtons;

type
  TMPButtonLayout = (blGlyphLeft,
                     blGlyphRight,
                     blGlyphTop,
                     blGlyphBottom);

  TMPButtonStyle  = (bsNormal,
                     bsEncarta,
                     bsModern,
                     bsOld,
                     bsShape,
                     bsQuicken);

  TMPxpButtonBehavior = (bbAuto,
                         bbPushButton,
                         bbCheckBox,
                         bbRadioButton);

  TMPxpButton = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FAllowAllUp: Boolean;
    FColorWhenDown: TColor;
    FColorWhenUp: TColor;
    FDown: Boolean;
    FFlat: Boolean;
    FWordWrap: Boolean;
    FGlyph: TBitmap;
    FGroupIndex: Integer;
    FHotTrackColor: TColor;
    FHTFont: TFont;
    FLayout: TMPButtonLayout;
    FLightColor: TColor;
    FNumGlyphs: Integer;
    FShadowColor: TColor;
    FStyle: TMPButtonStyle;
    FTransparent: Boolean;
    FUseHTFont: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FDummyStyle: TMPxpColorStyle;
    bCursorOnButton: Boolean;
    FHotTrackStep: Integer;
    FGlyphTPColor: TColor;
    FActiveColor: TColor;
    FGlTransp: Boolean;

    // Behavior
    FBehavior: TMPxpButtonBehavior;

    // Optional state glyphs
    FGlyphUnchecked: TBitmap;
    FGlyphChecked: TBitmap;

    // Optional imagelist support
    FImages: TCustomImageList;
    FImageIndexUnchecked: Integer;
    FImageIndexChecked: Integer;

    // Checked event
    FOnCheckedChanged: TNotifyEvent;

    // Focus rect + keyboard
    FShowFocusRect: Boolean;

    // Slow hot-track fade-out without threads
    FSlowDecease: Boolean;
    FDecTimer: TTimer;

    procedure DecTimerTick(Sender: TObject);

    procedure SetAlignment(Value: TAlignment);
    procedure SetFlat(Value: Boolean);
    procedure SetColorWhenDown(Value: TColor);
    procedure SetColorWhenUp(Value: TColor);
    procedure SetColorStyle(Value: TMPxpColorStyle);
    procedure SetDown(Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGroupIndex(Value: Integer);
    procedure SetHTFont(Value: TFont);
    procedure SetLayout(Value: TMPButtonLayout);
    procedure SetLightColor(Value: TColor);
    procedure SetNumGlyphs(Value: Integer);
    procedure SetShadowColor(Value: TColor);
    procedure SetStyle(Value: TMPButtonStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure SetSlowDecease(Value: Boolean);
    procedure SetGlyphTPColor(Value: TColor);
    procedure SetFGlTransp(Value: Boolean);

    function GetCaption: TCaption;

    // New checkbox API
    function GetChecked(): Boolean;
    procedure SetChecked(Value: Boolean);

    procedure SetBehavior(Value: TMPxpButtonBehavior);
    procedure SetGlyphUnchecked(Value: TBitmap);
    procedure SetGlyphChecked(Value: TBitmap);

    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndexUnchecked(Value: Integer);
    procedure SetImageIndexChecked(Value: Integer);

    procedure SetShowFocusRect(Value: Boolean);

    procedure DoCheckedChanged(); virtual;

    function HasStateGlyphs(): Boolean;
    function GetEffectiveBehavior(): TMPxpButtonBehavior;
    function GetPaintGlyph(): TBitmap;
    function GetPaintNumGlyphs(): Integer;

    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;

  protected

    procedure Paint(); override;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure KeyDown(var Key: Word;
                      Shift: TShiftState); override;

    procedure DoMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure DoMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure DoDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;

    procedure MouseDown(Button: TMouseButton;
                        Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
                      Shift: TShiftState;
                      X, Y: Integer); override;
    procedure ActionChange(Sender: TObject;
                           CheckDefaults: Boolean); override;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Click(); override;

  published

    property Action;

    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp default False;
    property Anchors;
    property Caption: TCaption read GetCaption write SetCaption;
    property Color;
    property ColorWhenDown: TColor read FColorWhenDown write SetColorWhenDown default clNone;
    property ColorWhenUp: TColor read FColorWhenUp write SetColorWhenUp default clNone;
    property ColorStyle: TMPxpColorStyle read FDummyStyle write SetColorStyle default lcsCustom;
    property Down: Boolean read FDown write SetDown default False;

    property Behavior: TMPxpButtonBehavior read FBehavior write SetBehavior default bbAuto;
    property Checked: Boolean read GetChecked write SetChecked;

    property GlyphUnchecked: TBitmap read FGlyphUnchecked write SetGlyphUnchecked;
    property GlyphChecked: TBitmap read FGlyphChecked write SetGlyphChecked;

    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndexUnchecked: Integer read FImageIndexUnchecked write SetImageIndexUnchecked default -1;
    property ImageIndexChecked: Integer read FImageIndexChecked write SetImageIndexChecked default -1;

    property TabStop default False;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;

    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphTransparentColor: TColor read FGlyphTPColor write SetGlyphTPColor;
    property GlyphTransparent: Boolean read FGlTransp write SetFGlTransp;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HotTrackColor: TColor read FHotTrackColor write FHotTrackColor default clNone;
    property HotTrackFont: TFont read FHTFont write SetHTFont;
    property Hint;
    property Layout: TMPButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property LightColor: TColor read FLightColor write SetLightColor default clWhite;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 0;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property ShowHint;
    property SlowDecease: Boolean read FSlowDecease write SetSlowDecease default False;
    property Style: TMPButtonStyle read FStyle write SetStyle default bsNormal;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property UseHotTrackFont: Boolean read FUseHTFont write FUseHTFont default False;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property OnClick;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;


implementation

const
  cHotTrackSteps = 10;

procedure Register;
begin

  RegisterComponents('RDJ Controls',
                     [TMPxpButton]);
end;


{ TMPxpButton }

constructor TMPxpButton.Create(AOwner: TComponent);
begin

  inherited;

  ControlStyle := ControlStyle + [csSetCaption,
                                  csClickEvents,
                                  csDoubleClicks,
                                  csCaptureMouse,
                                  csOpaque];

  DoubleBuffered := True;
  ParentDoubleBuffered := False;

  Height := 23;
  Width := 100;

  bCursorOnButton := False;
  FHotTrackStep := 0;

  FLightColor := clWhite;
  FShadowColor := clBlack;
  FColorWhenDown := clNone;
  FHotTrackColor := clNone;
  FActiveColor := clYellow;
  FWordWrap := False;
  FAlignment := taLeftJustify;
  FDummyStyle := lcsCustom;

  // Important: initialize colors to non-black defaults
  FColorWhenUp := clBtnFace;
  Color := clBtnFace;

  // transparent key for bitmap work (only used when glyph transparency enabled)
  FGlyphTPColor := clFuchsia;
  FGlTransp := True;

  FGlyph := TBitmap.Create;
  FGlyphUnchecked := TBitmap.Create;
  FGlyphChecked := TBitmap.Create;

  FHTFont := TFont.Create;
  if (AOwner is TForm) then
    FHTFont.Assign(TForm(AOwner).Font)
  else
    FHTFont.Assign(Font);

  FImages := nil;
  FImageIndexUnchecked := -1;
  FImageIndexChecked := -1;

  FBehavior := bbAuto;
  FShowFocusRect := True;

  FSlowDecease := False;
  FDecTimer := nil;

  TabStop := False;
end;


destructor TMPxpButton.Destroy;
begin

  FreeAndNil(FDecTimer);
  FreeAndNil(FHTFont);
  FreeAndNil(FGlyph);
  FreeAndNil(FGlyphUnchecked);
  FreeAndNil(FGlyphChecked);

  inherited;
end;


procedure TMPxpButton.CMColorChanged(var Msg: TMessage);
begin

  inherited;

  // Treat runtime changes to Color as the new "Up" color unless user explicitly set ColorWhenUp
  if (csDesigning in ComponentState) then
    Exit;

  if (FColorWhenUp = clNone) or
     (FColorWhenUp = clBtnFace) then
    FColorWhenUp := Color;

  Invalidate;
end;


procedure TMPxpButton.SetCaption(const Value: TCaption);
begin

  inherited Caption := Value;
  Invalidate;
end;


function TMPxpButton.GetCaption(): TCaption;
begin

  Result := inherited Caption;
end;


procedure TMPxpButton.SetAlignment(Value: TAlignment);
begin

  if (FAlignment = Value) then
    Exit;
  FAlignment := Value;
  Invalidate;
end;


procedure TMPxpButton.SetFlat(Value: Boolean);
begin

  if (FFlat = Value) then
    Exit;
  FFlat := Value;
  Invalidate;
end;


procedure TMPxpButton.SetColorWhenDown(Value: TColor);
begin

  FColorWhenDown := Value;
  Invalidate;
end;


procedure TMPxpButton.SetColorWhenUp(Value: TColor);
begin

  FColorWhenUp := Value;
  if (Value <> clNone) then
    Color := Value;
  Invalidate;
end;


procedure TMPxpButton.SetColorStyle(Value: TMPxpColorStyle);
var
  bModern,
  bQuicken: Boolean;
  BaseColor,
  FontColor: TColor;
  NewStyle: TMPButtonStyle;

begin

  FDummyStyle := Value;

  if (Value = lcsCustom) then
    begin

      Invalidate;
      Exit;
    end;

  GetPreDefinedColors(Value,
                      BaseColor,
                      FLightColor,
                      FShadowColor,
                      FColorWhenDown,
                      FHotTrackColor,
                      FontColor,
                      FFlat,
                      bModern,
                      bQuicken);

  // Store base color as Up color and actual Color
  FColorWhenUp := BaseColor;
  Color := BaseColor;

  Font.Color := FontColor;

  NewStyle := bsNormal;
  if bModern then
    NewStyle := bsModern;
  if bQuicken then
    NewStyle := bsQuicken;
  FStyle := NewStyle;

  Invalidate;
end;


procedure TMPxpButton.SetDown(Value: Boolean);
var
  OldDown: Boolean;
  i: Integer;
  btn: TMPxpButton;

begin

  if (csDestroying in ComponentState) then
    Exit;

  if (FDown = Value) then
    Exit;

  OldDown := FDown;

  // Radio-group behavior when GroupIndex <> 0
  if Value and (GroupIndex <> 0) and (Parent <> nil) then
    begin
      for i := 0 to Parent.ControlCount - 1 do
        if Parent.Controls[i] is TMPxpButton then
          begin

            btn := TMPxpButton(Parent.Controls[i]);
            if (btn <> Self) and (btn.GroupIndex = GroupIndex) then
              btn.FDown := False;
          end;
    end;

  FDown := Value;

  if (OldDown <> FDown) and (GetEffectiveBehavior <> bbPushButton) and
     (not (csLoading in ComponentState)) and (not (csDesigning in ComponentState)) then
    DoCheckedChanged;

  Invalidate;
end;


procedure TMPxpButton.SetGlyph(Value: TBitmap);
begin

  if (Value <> nil) then
    begin

      FGlyph.Assign(Value);
      if (Value.Height <> 0) then
        FNumGlyphs := Value.Width div Value.Height
      else
        FNumGlyphs := 0;
    end
  else
    begin

      FGlyph.SetSize(0,
                     0);
      FNumGlyphs := 0;
    end;
  Invalidate;
end;


procedure TMPxpButton.SetGroupIndex(Value: Integer);
begin

  if (FGroupIndex = Value) then
    Exit;
  FGroupIndex := Value;
  Invalidate;
end;


procedure TMPxpButton.SetHTFont(Value: TFont);
begin

  if (Value = nil) then
    Exit;
  FHTFont.Assign(Value);
  Invalidate;
end;


procedure TMPxpButton.SetLayout(Value: TMPButtonLayout);
begin

  if (FLayout = Value) then
    Exit;
  FLayout := Value;
  Invalidate;
end;


procedure TMPxpButton.SetLightColor(Value: TColor);
begin

  if (FLightColor = Value) then
    Exit;
  FLightColor := Value;
  Invalidate;
end;


procedure TMPxpButton.SetNumGlyphs(Value: Integer);
begin

  if (FNumGlyphs = Value) then
    Exit;
  FNumGlyphs := Value;
  Invalidate;
end;


procedure TMPxpButton.SetShadowColor(Value: TColor);
begin

  if (FShadowColor = Value) then
    Exit;
  FShadowColor := Value;
  Invalidate;
end;


procedure TMPxpButton.SetStyle(Value: TMPButtonStyle);
begin

  // Respect old 8bpp limitation but don't silently ignore property updates
  if (Value = bsModern) and
     (GetDeviceCaps(Canvas.Handle,
                    BITSPIXEL) <= 8) and
     (not (csDesigning in ComponentState)) then
    Value := bsNormal;

  if (FStyle = Value) then
    Exit;
  FStyle := Value;
  Invalidate;
end;


procedure TMPxpButton.SetTransparent(Value: Boolean);
begin

  if (FTransparent = Value) then
    Exit;

  FTransparent := Value;

  // We still paint fully; Transparent controls need WS_EX_TRANSPARENT style for true passthrough.
  // Here it means: blend with parent background in our offscreen buffer.

  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];

  Invalidate;
end;


procedure TMPxpButton.SetWordWrap(Value: Boolean);
begin

  if (FWordWrap = Value) then
    Exit;
  FWordWrap := Value;
  Invalidate;
end;


procedure TMPxpButton.SetGlyphTPColor(Value: TColor);
begin

  if (FGlyphTPColor = Value) then
    Exit;

  FGlyphTPColor := Value;
  Invalidate;
end;


procedure TMPxpButton.SetFGlTransp(Value: Boolean);
begin

  if (FGlTransp = Value) then
    Exit;
  FGlTransp := Value;
  Invalidate;
end;


procedure TMPxpButton.SetSlowDecease(Value: Boolean);
begin

  if (FSlowDecease = Value) then
    Exit;
  FSlowDecease := Value;

  if FSlowDecease then
    begin

      if (FDecTimer = nil) then
        begin

          FDecTimer := TTimer.Create(Self);
          FDecTimer.Enabled := False;
          FDecTimer.Interval := 20;
          FDecTimer.OnTimer := DecTimerTick;
        end;
    end
  else
    begin

      if (FDecTimer <> nil) then
        FDecTimer.Enabled := False;
      FHotTrackStep := 0;
    end;
  Invalidate;
end;


procedure TMPxpButton.DecTimerTick(Sender: TObject);
begin

  if bCursorOnButton then
    begin

      FDecTimer.Enabled := False;
      Exit;
    end;

  if (FHotTrackStep > 0) then
    begin

      Dec(FHotTrackStep);
      Invalidate;
    end
  else
    FDecTimer.Enabled := False;
end;


function TMPxpButton.GetChecked(): Boolean;
begin

  Result := Down;
end;


procedure TMPxpButton.SetChecked(Value: Boolean);
begin

  Down := Value;
end;


procedure TMPxpButton.SetBehavior(Value: TMPxpButtonBehavior);
begin

  if (FBehavior = Value) then
    Exit;
  FBehavior := Value;
  Invalidate;
end;


procedure TMPxpButton.SetGlyphUnchecked(Value: TBitmap);
begin

  if (Value <> nil) then
    FGlyphUnchecked.Assign(Value)
  else
    FGlyphUnchecked.SetSize(0,
                            0);
  Invalidate;
end;


procedure TMPxpButton.SetGlyphChecked(Value: TBitmap);
begin

  if (Value <> nil) then
    FGlyphChecked.Assign(Value)
  else
    FGlyphChecked.SetSize(0,
                          0);
  Invalidate;
end;


procedure TMPxpButton.SetImages(Value: TCustomImageList);
begin

  if (FImages = Value) then
  Exit;
  FImages := Value;
  Invalidate;
end;


procedure TMPxpButton.SetImageIndexUnchecked(Value: Integer);
begin

  if (FImageIndexUnchecked = Value) then
    Exit;
  FImageIndexUnchecked := Value;
  Invalidate;
end;


procedure TMPxpButton.SetImageIndexChecked(Value: Integer);
begin

  if (FImageIndexChecked = Value) then
    Exit;
  FImageIndexChecked := Value;
  Invalidate;
end;


procedure TMPxpButton.SetShowFocusRect(Value: Boolean);
begin

  if (FShowFocusRect = Value) then
    Exit;
  FShowFocusRect := Value;
  Invalidate;
end;


procedure TMPxpButton.DoCheckedChanged();
begin

  if Assigned(FOnCheckedChanged) then
    FOnCheckedChanged(Self);
end;


function TMPxpButton.HasStateGlyphs(): Boolean;
begin

  Result := (FGlyphUnchecked <> nil) and
            (FGlyphChecked <> nil) and
            (not FGlyphUnchecked.Empty) and
            (not FGlyphChecked.Empty);
end;


function TMPxpButton.GetEffectiveBehavior(): TMPxpButtonBehavior;
begin

  if (FBehavior <> bbAuto) then
    Exit(FBehavior);

  // legacy: GroupIndex 0 => push, else radio
  if (GroupIndex = 0) then
    Result := bbPushButton
  else
    Result := bbRadioButton;
end;


function TMPxpButton.GetPaintGlyph(): TBitmap;
var
  idx: Integer;
  bmp: TBitmap;

begin

  // State glyph bitmaps override
  if HasStateGlyphs then
  begin

    if Down then
      Exit(FGlyphChecked)
    else
      Exit(FGlyphUnchecked);
  end;

  // ImageList override
  if (FImages <> nil) then
  begin

    if Down then
      idx := FImageIndexChecked
    else
      idx := FImageIndexUnchecked;

    if (idx >= 0) then
      begin

        bmp := TBitmap.Create;
        bmp.SetSize(FImages.Width, FImages.Height);
        bmp.Canvas.Brush.Color := FGlyphTPColor;
        bmp.Canvas.FillRect(Rect(0,
                                 0,
                                 bmp.Width,
                                 bmp.Height));

        bmp.Transparent := True;
        bmp.TransparentColor := FGlyphTPColor;

        FImages.Draw(bmp.Canvas,
                     0,
                     0,
                     idx,
                     True);
        Exit(bmp); // owned by caller
      end;
  end;

  Result := FGlyph;
end;


function TMPxpButton.GetPaintNumGlyphs(): Integer;
begin

  if HasStateGlyphs then
    Exit(1);

  if (FImages <> nil) and
     ((FImageIndexUnchecked >= 0) or
     (FImageIndexChecked >= 0)) then
    Exit(1);

  Result := FNumGlyphs;
end;


procedure TMPxpButton.Notification(AComponent: TComponent;
                                   Operation: TOperation);
begin

  inherited;

  if (Operation = opRemove) and (AComponent = FImages) then
  begin

    FImages := nil;
    Invalidate;
  end;
end;


procedure TMPxpButton.KeyDown(var Key: Word;
                              Shift: TShiftState);
begin

  inherited;

  if not Enabled then
  Exit;

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin

    Click();
    Key := 0;
  end;
end;


procedure TMPxpButton.DoMouseEnter(var Msg: TMessage);
begin

  inherited;

  if Enabled and
     Visible and
     (Parent <> nil) and
     Parent.Showing then
  begin

    bCursorOnButton := True;
    FHotTrackStep := cHotTrackSteps;
    if (FDecTimer <> nil) then
      FDecTimer.Enabled := False;

    Invalidate;

    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
  end;
end;


procedure TMPxpButton.DoMouseLeave(var Msg: TMessage);
begin

  inherited;
  bCursorOnButton := False;

  if FSlowDecease and (FDecTimer <> nil) then
    FDecTimer.Enabled := True
  else
    begin

      FHotTrackStep := 0;
      Invalidate;
    end;

  if Enabled and
     Visible and
     (Parent <> nil) and
     Parent.Showing then
       if Assigned(FOnMouseExit) then
         FOnMouseExit(Self);
end;


procedure TMPxpButton.DoDialogChar(var Message: TCMDialogChar);
begin

  inherited;

  // basic '&' accelerator support
  if IsAccel(Message.CharCode,
     Caption) and
     Enabled and
     Visible then
  begin

    Click();
    Message.Result := 1;
  end;
end;


procedure TMPxpButton.MouseDown(Button: TMouseButton;
                                Shift: TShiftState;
                                X, Y: Integer);
var
  mode: TMPxpButtonBehavior;

begin

  inherited;

  if (Button <> mbLeft) then
  Exit;

  if (TabStop and CanFocus) then
    SetFocus;

  mode := GetEffectiveBehavior;

  case mode of
    bbPushButton: Down := True;
    bbCheckBox: Down := not Down;
    bbRadioButton:
    begin

      if Down then
        begin

          if FAllowAllUp then
            Down := False;
        end
      else
        Down := True;
    end;
  end;
end;


procedure TMPxpButton.MouseUp(Button: TMouseButton;
                              Shift: TShiftState;
                              X, Y: Integer);
var
  mode: TMPxpButtonBehavior;

begin

  inherited;

  if (Button <> mbLeft) then
  Exit;

  mode := GetEffectiveBehavior();
  if (mode = bbPushButton) then
    Down := False;
end;


procedure TMPxpButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin

  Perform(WM_LBUTTONDOWN,
          Message.Keys,
          Longint(Message.Pos));
end;


procedure TMPxpButton.WMSize(var Msg: TWMSize);
begin

  inherited;

  Invalidate;
end;


procedure TMPxpButton.WMWindowPosChanged(var Msg: TWMWindowPosChanged);
begin

  inherited;

  Invalidate;
end;


procedure TMPxpButton.CMParentColorChanged(var Msg: TMessage);
begin

  inherited;

  Invalidate;
end;


procedure TMPxpButton.ActionChange(Sender: TObject;
                                   CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList;
                      Index: Integer);
  begin

    if (ImageList = nil) or (Index < 0) then
    Exit;

    with Glyph do
      begin

        Width := ImageList.Width;
        Height := ImageList.Height;
        Canvas.Brush.Color := clFuchsia;

        Canvas.FillRect(Rect(0,
                             0,
                             Width,
                             Height));

        ImageList.Draw(Canvas,
                       0,
                       0,
                       Index);

        Transparent := True;
        TransparentColor := clFuchsia;
      end;
  end;

begin

  inherited;

  if Sender is TCustomAction then
    begin

      if not CheckDefaults or (Caption = '') then
        Caption := TCustomAction(Sender).Caption;

      if not CheckDefaults or (Hint = '') then
        Hint := TCustomAction(Sender).Hint;

      if not CheckDefaults or (not Assigned(Glyph)) then
        if (TCustomAction(Sender).ActionList <> nil) and
           (TCustomAction(Sender).ActionList.Images <> nil) and
           (TCustomAction(Sender).ImageIndex >= 0) then
          CopyImage(TCustomAction(Sender).ActionList.Images,
                    TCustomAction(Sender).ImageIndex);
    end;
end;


procedure TMPxpButton.Click();
begin

  inherited;
end;


procedure TMPxpButton.Paint();

  procedure PaintParentBackground(ABitmap: TBitmap);
  var
    SaveIndex: Integer;
    DC: HDC;

  begin

    if (Parent = nil) then
    Exit;

    DC := ABitmap.Canvas.Handle;
    SaveIndex := SaveDC(DC);

    try

      // map parent coords into our bitmap
      SetViewportOrgEx(DC,
                       -Left,
                       -Top,
                       nil);

      Parent.Perform(WM_ERASEBKGND,
                     WPARAM(DC),
                     0);

      Parent.Perform(WM_PRINTCLIENT,
                     WPARAM(DC),
                     PRF_CLIENT);
    finally

      RestoreDC(DC,
                SaveIndex);
    end;
  end;

var
  aStyle: MPxpButtons.TMPxpButtonStyle;
  aLayout: MPxpButtons.TMPxpButtonLayout;
  bmp: TBitmap;
  paintGlyph: TBitmap;
  numGlyphs: Integer;
  tmpGlyphOwned: Boolean;
  baseColor: TColor;
  fontToUse: TFont;

begin

  if (csLoading in ComponentState) then
  Exit;

  if not (Visible or (csDesigning in ComponentState)) then
    Exit;

  bmp := TBitmap.Create;
  paintGlyph := nil;
  tmpGlyphOwned := False;

  try

    bmp.SetSize(Width,
                Height);
    bmp.PixelFormat := pf32bit;

    // Background
    if FTransparent then
      PaintParentBackground(bmp)
    else
      begin

        bmp.Canvas.Brush.Color := Color;
        bmp.Canvas.FillRect(Rect(0,
                            0,
                            Width,
                            Height));
      end;

    // Map style/layout to MPxpButtons
    aStyle := MPxpButtons.bsNormal;

    case FStyle of
      bsEncarta: aStyle := MPxpButtons.bsEncarta;
      bsModern:  aStyle := MPxpButtons.bsModern;
      bsOld:     aStyle := MPxpButtons.bsOld;
      bsShape:   aStyle := MPxpButtons.bsShape;
      bsQuicken: aStyle := MPxpButtons.bsQuicken;
    end;

    aLayout := MPxpButtons.blGlyphLeft;
    case FLayout of
      blGlyphTop:    aLayout := MPxpButtons.blGlyphTop;
      blGlyphRight:  aLayout := MPxpButtons.blGlyphRight;
      blGlyphBottom: aLayout := MPxpButtons.blGlyphBottom;
    end;

    paintGlyph := GetPaintGlyph();
    numGlyphs := GetPaintNumGlyphs();
    tmpGlyphOwned := (paintGlyph <> nil) and
                     (paintGlyph <> FGlyph) and
                     (paintGlyph <> FGlyphUnchecked) and
                     (paintGlyph <> FGlyphChecked);

    // Choose base color: allow ColorWhenUp override
    baseColor := Color;
    if (FColorWhenUp <> clNone) then
      baseColor := FColorWhenUp;

    if bCursorOnButton and FUseHTFont then
      fontToUse := FHTFont
    else
      fontToUse := Font;

    // Draw the button (we DO NOT force transparency for "modern" styles anymore)
    MPxpPaintButton(bmp.Canvas,
                    Width,
                    Height,
                    FHotTrackStep,
                    numGlyphs,
                    paintGlyph,
                    FDown,
                    bCursorOnButton,
                    False {Transparent},
                    Enabled,
                    Flat,
                    FWordWrap,
                    Assigned(PopupMenu),
                    aStyle,
                    baseColor,
                    FColorWhenDown,
                    FHotTrackColor,
                    FLightColor,
                    FShadowColor,
                    fontToUse,
                    aLayout,
                    Caption,
                    FAlignment);

    Canvas.Draw(0,
                0, bmp);

    // Focus rectangle
    if FShowFocusRect and
       TabStop and
       Focused and
       Enabled then
    begin

      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psDot;
      Canvas.Rectangle(2,
                       2,
                       Width - 2,
                       Height - 2);
      Canvas.Pen.Style := psSolid;
    end;

  finally

    if tmpGlyphOwned and (paintGlyph <> nil) then
      paintGlyph.Free;
    bmp.Free;
  end;
end;

end.

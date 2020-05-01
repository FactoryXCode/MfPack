object FloatingForm: TFloatingForm
  Left = 538
  Top = 279
  Margins.Bottom = 6
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'TFloatingForm'
  ClientHeight = 473
  ClientWidth = 840
  Color = clBlack
  Ctl3D = False
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object lblSubTitle: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 834
    Height = 464
    Margins.Bottom = 6
    ParentCustomHint = False
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'There is a reason to believe, that this is a subtitle..  Or not?' +
      ' '
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowAccelChar = False
    ShowHint = False
    Transparent = True
    Layout = tlBottom
    WordWrap = True
    ExplicitTop = 5
  end
end

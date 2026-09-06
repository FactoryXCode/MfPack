object frmTagEditor: TfrmTagEditor
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Tag Editor'
  ClientHeight = 451
  ClientWidth = 568
  Color = 5850948
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object lblPath: TLabel
    Left = 8
    Top = 12
    Width = 79
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Path'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblArtist: TLabel
    Left = 8
    Top = 38
    Width = 79
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Artist'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTitle: TLabel
    Left = 8
    Top = 64
    Width = 79
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Title'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAlbum: TLabel
    Left = 8
    Top = 90
    Width = 79
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Album'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAlbumArtist: TLabel
    Left = 8
    Top = 116
    Width = 79
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Album/Artist'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGenre: TLabel
    Left = 8
    Top = 142
    Width = 79
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Genre'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblComposer: TLabel
    Left = 8
    Top = 168
    Width = 79
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Composer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblComment: TLabel
    Left = 8
    Top = 265
    Width = 79
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Comment'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblYear: TLabel
    Left = 8
    Top = 199
    Width = 79
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Year'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTrackNo: TLabel
    Left = 193
    Top = 199
    Width = 80
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Track No'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblDiscNo: TLabel
    Left = 383
    Top = 199
    Width = 70
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Disc No'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblBPM: TLabel
    Left = 8
    Top = 226
    Width = 79
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'BPM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblKey: TLabel
    Left = 193
    Top = 226
    Width = 80
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Key'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGainDb: TLabel
    Left = 381
    Top = 228
    Width = 72
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Gain Db'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 398
    Width = 568
    Height = 53
    Align = alBottom
    Color = 5850948
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 386
    object btnOk: TMPxpButton
      Left = 356
      Top = 10
      Width = 96
      Height = 36
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Ok'
      Color = 6303744
      ColorWhenDown = 11363625
      ColorWhenUp = 6303744
      ColorStyle = lcsQuicken
      Behavior = bbPushButton
      Checked = False
      ImageIndexUnchecked = 1
      ImageIndexChecked = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = 5850948
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnOkClick
    end
    object btnCancel: TMPxpButton
      Left = 458
      Top = 10
      Width = 96
      Height = 36
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Cancel'
      Color = 6303744
      ColorWhenDown = 11363625
      ColorWhenUp = 6303744
      ColorStyle = lcsQuicken
      Behavior = bbPushButton
      Checked = False
      ImageIndexUnchecked = 1
      ImageIndexChecked = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = 5850948
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnCancelClick
    end
  end
  object edtPath: TEdit
    Left = 97
    Top = 10
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object edtArtist: TEdit
    Left = 97
    Top = 36
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object edtTitle: TEdit
    Left = 97
    Top = 62
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object edtAlbum: TEdit
    Left = 97
    Top = 88
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object edtAlbumArtist: TEdit
    Left = 97
    Top = 114
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object edtGenre: TEdit
    Left = 97
    Top = 140
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object edtComposer: TEdit
    Left = 97
    Top = 166
    Width = 457
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object edtYear: TEdit
    Left = 97
    Top = 197
    Width = 90
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object edtTrackNo: TEdit
    Left = 284
    Top = 197
    Width = 90
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
  end
  object edtDiscNo: TEdit
    Left = 464
    Top = 197
    Width = 90
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
  end
  object edtBPM: TEdit
    Left = 97
    Top = 226
    Width = 90
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
  end
  object edtKey: TEdit
    Left = 284
    Top = 225
    Width = 90
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
  end
  object edtGainDb: TEdit
    Left = 464
    Top = 226
    Width = 90
    Height = 24
    BevelInner = bvNone
    BevelKind = bkSoft
    BevelOuter = bvRaised
    BorderStyle = bsNone
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
  end
  object memComment: TMemo
    Left = 97
    Top = 263
    Width = 457
    Height = 124
    Color = 9216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 14
  end
end

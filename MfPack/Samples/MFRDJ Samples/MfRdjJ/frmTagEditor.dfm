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
  ClientHeight = 732
  ClientWidth = 947
  Color = 5850948
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 25
  object lblPath: TLabel
    Left = 11
    Top = 18
    Width = 132
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Path'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblArtist: TLabel
    Left = 11
    Top = 69
    Width = 132
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Artist'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTitle: TLabel
    Left = 11
    Top = 110
    Width = 132
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Title'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAlbum: TLabel
    Left = 11
    Top = 149
    Width = 132
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Album'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAlbumArtist: TLabel
    Left = 11
    Top = 189
    Width = 132
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Album/Artist'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGenre: TLabel
    Left = 11
    Top = 230
    Width = 132
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Genre'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblComposer: TLabel
    Left = 11
    Top = 269
    Width = 132
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Composer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblComment: TLabel
    Left = 20
    Top = 401
    Width = 132
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Comment'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblYear: TLabel
    Left = 11
    Top = 309
    Width = 132
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Year'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTrackNo: TLabel
    Left = 321
    Top = 309
    Width = 134
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Track No'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblDiscNo: TLabel
    Left = 638
    Top = 309
    Width = 117
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Disc No'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblBPM: TLabel
    Left = 11
    Top = 350
    Width = 132
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'BPM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblKey: TLabel
    Left = 321
    Top = 350
    Width = 134
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Key'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGainDb: TLabel
    Left = 635
    Top = 350
    Width = 120
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Gain Db'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 644
    Width = 947
    Height = 88
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Color = 5850948
    ParentBackground = False
    TabOrder = 0
    object btnOk: TMPxpButton
      Left = 593
      Top = 17
      Width = 160
      Height = 60
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Font.Height = -20
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
      Left = 764
      Top = 17
      Width = 159
      Height = 60
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Font.Height = -20
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
    Left = 162
    Top = 18
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object edtArtist: TEdit
    Left = 162
    Top = 69
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object edtTitle: TEdit
    Left = 162
    Top = 110
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object edtAlbum: TEdit
    Left = 162
    Top = 149
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object edtAlbumArtist: TEdit
    Left = 162
    Top = 189
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object edtGenre: TEdit
    Left = 162
    Top = 230
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object edtComposer: TEdit
    Left = 162
    Top = 269
    Width = 761
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object edtYear: TEdit
    Left = 162
    Top = 309
    Width = 150
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object edtTrackNo: TEdit
    Left = 473
    Top = 309
    Width = 150
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
  end
  object edtDiscNo: TEdit
    Left = 773
    Top = 309
    Width = 150
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
  end
  object edtBPM: TEdit
    Left = 162
    Top = 350
    Width = 150
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
  end
  object edtKey: TEdit
    Left = 473
    Top = 350
    Width = 150
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
  end
  object edtGainDb: TEdit
    Left = 773
    Top = 350
    Width = 150
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
  end
  object memComment: TMemo
    Left = 162
    Top = 401
    Width = 761
    Height = 207
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Color = 6899524
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 14
  end
end

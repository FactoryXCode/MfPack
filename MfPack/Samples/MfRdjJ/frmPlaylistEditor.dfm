object frmPlaylistEditor: TfrmPlaylistEditor
  Left = 0
  Top = 0
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  BorderIcons = []
  BorderStyle = bsSizeToolWin
  ClientHeight = 815
  ClientWidth = 1265
  Color = 5850948
  DefaultMonitor = dmDesktop
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 16
  object pnlClient: TPanel
    Left = 0
    Top = 136
    Width = 1265
    Height = 650
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 2
    ExplicitTop = 117
    ExplicitHeight = 647
    object splLeft: TSplitter
      Left = 520
      Top = 0
      Width = 5
      Height = 650
      Color = clGray
      MinSize = 5
      ParentColor = False
      ExplicitHeight = 647
    end
    object pnlLibrary: TPanel
      Left = 0
      Top = 0
      Width = 520
      Height = 650
      Align = alLeft
      TabOrder = 0
      ExplicitHeight = 647
      object grdLibrary: TStringGrid
        Left = 1
        Top = 1
        Width = 519
        Height = 648
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alLeft
        Color = 5850948
        DefaultRowHeight = 20
        DoubleBuffered = False
        DrawingStyle = gdsGradient
        FixedColor = 5850948
        FixedCols = 0
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        GradientEndColor = clGray
        GradientStartColor = 5850948
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goRowSelect]
        ParentDoubleBuffered = False
        ParentFont = False
        TabOrder = 0
        OnDblClick = grdLibraryDblClick
        OnMouseDown = grdLibraryMouseDown
        OnSelectCell = grdLibrarySelectCell
        OnSetEditText = grdLibrarySetEditText
        ExplicitHeight = 645
      end
    end
    object pnlActions: TPanel
      Left = 525
      Top = 0
      Width = 119
      Height = 650
      Align = alLeft
      BevelOuter = bvNone
      Color = 5850948
      ParentBackground = False
      TabOrder = 1
      ExplicitHeight = 647
      object btnAddToPlaylist: TMPxpButton
        Left = 7
        Top = 214
        Width = 107
        Height = 40
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Add'
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
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        Glyph.Data = {
          42010000424D4201000000000000760000002800000011000000110000000100
          040000000000CC000000C40E0000C40E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000F000000
          00000000000000000FF0000000000000000000000FFF00000000000000000000
          0FFFF00000000000000000000FFFFF00000000000000000000FFFFF000000000
          00000000000FFFFF00000000000000000000FFFFF00000000000000000000FFF
          FF000000000000000000FFFFF000000000000000000FFFFF0000000000000000
          00FFFFF000000000000000000FFFFF0000000000000000000FFFF00000000000
          000000000FFF000000000000000000000FF0000000000000000000000F000000
          000000000000}
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
        NumGlyphs = 1
        ParentColor = False
        ParentFont = False
        ShadowColor = clSkyBlue
        Style = bsModern
        OnClick = btnAddToPlaylistClick
      end
      object btnRemoveFromPlaylist: TMPxpButton
        Left = 7
        Top = 261
        Width = 106
        Height = 40
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Remove'
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
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        Glyph.Data = {
          42010000424D4201000000000000760000002800000011000000110000000100
          040000000000CC000000C40E0000C40E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000F
          0000000000000000000000FF000000000000000000000FFF0000000000000000
          0000FFFF0000000000000000000FFFFF000000000000000000FFFFF000000000
          000000000FFFFF000000000000000000FFFFF000000000000000000FFFFF0000
          0000000000000000FFFFF00000000000000000000FFFFF000000000000000000
          00FFFFF00000000000000000000FFFFF00000000000000000000FFFF00000000
          0000000000000FFF0000000000000000000000FF00000000000000000000000F
          000000000000}
        GlyphTransparentColor = clFuchsia
        GlyphTransparent = True
        HotTrackColor = 5850948
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -18
        HotTrackFont.Name = 'Segoe UI'
        HotTrackFont.Style = []
        LightColor = 12348265
        NumGlyphs = 1
        ParentColor = False
        ParentFont = False
        ShadowColor = clSkyBlue
        Style = bsModern
        OnClick = btnRemoveFromPlaylistClick
      end
      object btnMoveUp: TMPxpButton
        Left = 7
        Top = 313
        Width = 106
        Height = 40
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Move Up'
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
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        Glyph.Data = {
          42010000424D4201000000000000760000002800000011000000110000000100
          040000000000CC000000C40E0000C40E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FFFFF0000000FFFFF000
          00000FFFFF00000FFFFF0000000000FFFFF000FFFFF000000000000FFFFF0FFF
          FF00000000000000FFFFFFFFF0000000000000000FFFFFFF0000000000000000
          00FFFFF00000000000000000000FFF0000000000000000000000F00000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000}
        GlyphTransparentColor = clFuchsia
        GlyphTransparent = True
        HotTrackColor = 5850948
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -18
        HotTrackFont.Name = 'Segoe UI'
        HotTrackFont.Style = []
        Layout = blGlyphTop
        LightColor = 12348265
        NumGlyphs = 1
        ParentColor = False
        ParentFont = False
        ShadowColor = clSkyBlue
        Style = bsModern
        OnClick = btnMoveUpClick
      end
      object btnMoveDown: TMPxpButton
        Left = 6
        Top = 360
        Width = 107
        Height = 40
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Move Down'
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
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        Glyph.Data = {
          42010000424D4201000000000000760000002800000011000000110000000100
          040000000000CC000000C40E0000C40E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000F0000000000000000000000FFF00000000000000000000FFFFF000000000
          000000000FFFFFFF0000000000000000FFFFFFFFF00000000000000FFFFF0FFF
          FF000000000000FFFFF000FFFFF0000000000FFFFF00000FFFFF00000000FFFF
          F0000000FFFFF000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000}
        GlyphTransparentColor = clFuchsia
        GlyphTransparent = True
        HotTrackColor = 5850948
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -18
        HotTrackFont.Name = 'Segoe UI'
        HotTrackFont.Style = []
        Layout = blGlyphBottom
        LightColor = 12348265
        NumGlyphs = 1
        ParentColor = False
        ParentFont = False
        ShadowColor = clSkyBlue
        Style = bsModern
        OnClick = btnMoveDownClick
      end
      object MPxpButton1: TMPxpButton
        Left = 7
        Top = 10
        Width = 106
        Height = 40
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Tag editor'
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
        OnClick = btnTagEditorClick
      end
      object btnScanFolder: TMPxpButton
        Left = 5
        Top = 468
        Width = 107
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Scan Folder...'
        Color = 5914932
        ColorWhenDown = 11363625
        ColorWhenUp = 5914932
        Checked = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
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
        OnClick = btnScanFolderClick
      end
      object btnCancelScan: TMPxpButton
        Left = 5
        Top = 514
        Width = 107
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Cancel Scan'
        Color = 5914932
        ColorWhenDown = 11363625
        ColorWhenUp = 5914932
        Checked = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
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
        OnClick = btnCancelScanClick
      end
      object btnClearLibrary: TMPxpButton
        Left = 5
        Top = 560
        Width = 107
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Clear Library'
        Color = 5914932
        ColorWhenDown = 11363625
        ColorWhenUp = 5914932
        Checked = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
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
        OnClick = btnClearLibraryClick
      end
      object btnClearMissingTracks: TMPxpButton
        Left = 5
        Top = 606
        Width = 107
        Height = 40
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Clear Missing Tracks'
        Color = 5914932
        ColorWhenDown = 11363625
        ColorWhenUp = 5914932
        Checked = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
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
        WordWrap = True
        OnClick = btnClearMissingTracksClick
      end
    end
    object grdPlaylist: TStringGrid
      Left = 644
      Top = 0
      Width = 621
      Height = 650
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Color = 5850948
      DefaultRowHeight = 20
      DrawingStyle = gdsGradient
      FixedColor = 5850948
      FixedCols = 0
      RowCount = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      GradientEndColor = clGray
      GradientStartColor = 5850948
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
      ParentFont = False
      TabOrder = 2
      OnMouseDown = grdPlaylistMouseDown
      ExplicitHeight = 647
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 37
    Width = 1265
    Height = 99
    Align = alTop
    BevelOuter = bvNone
    Color = 5850948
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 2
    ExplicitTop = 18
    object Bevel2: TBevel
      Left = 4
      Top = 1
      Width = 581
      Height = 94
    end
    object lblSearch: TLabel
      Left = 10
      Top = 18
      Width = 85
      Height = 21
      Hint = 'Search for a name, title etc.'
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Search for:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object edtSearch: TEdit
      Left = 103
      Top = 16
      Width = 302
      Height = 27
      Hint = 'Search for a name, title etc.'
      Alignment = taCenter
      AutoSize = False
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnSearch: TMPxpButton
      Left = 417
      Top = 7
      Width = 106
      Height = 40
      Hint = 'Search for an artist, title etc.'
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Search'
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
      Font.Height = -13
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
      ParentShowHint = False
      ShadowColor = clSkyBlue
      ShowHint = True
      Style = bsModern
      OnClick = btnSearchClick
    end
    object btnClearSearch: TMPxpButton
      Left = 530
      Top = 7
      Width = 45
      Height = 80
      Hint = 'Clear Search'
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Clear'
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
      Font.Height = -13
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
      ParentShowHint = False
      ShadowColor = clSkyBlue
      ShowHint = True
      Style = bsModern
      OnClick = btnClearSearchClick
    end
    object pnlPlaylist: TPanel
      Left = 644
      Top = 1
      Width = 611
      Height = 90
      BevelOuter = bvNone
      Color = 5850948
      ParentBackground = False
      TabOrder = 3
      object lblPlaylist: TLabel
        Left = 7
        Top = 6
        Width = 124
        Height = 23
        AutoSize = False
        Caption = 'Playlist'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object lblPlayListDuration: TLabel
        Left = 6
        Top = 64
        Width = 123
        Height = 23
        Caption = 'Playlist Duration: 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object cbPlaylists: TComboBox
        Left = 7
        Top = 33
        Width = 254
        Height = 24
        Hint = 'Select a playlist'
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = True
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = cbPlaylistsChange
        OnDblClick = cbPlaylistsDblClick
      end
      object btnDeletePlaylist: TMPxpButton
        Left = 273
        Top = 18
        Width = 106
        Height = 40
        Hint = 'Delete current playlist'
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Delete'
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
        Font.Height = -13
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
        ParentShowHint = False
        ShadowColor = clSkyBlue
        ShowHint = True
        Style = bsModern
        OnClick = btnDeletePlaylistClick
      end
      object btnSavePlaylist: TMPxpButton
        Left = 386
        Top = 18
        Width = 107
        Height = 40
        Hint = 'Save current playlist'
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'Save'
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
        Font.Height = -13
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
        ParentShowHint = False
        ShadowColor = clSkyBlue
        ShowHint = True
        Style = bsModern
        OnClick = btnSavePlaylistClick
      end
      object btnNewPlaylist: TMPxpButton
        Left = 499
        Top = 18
        Width = 107
        Height = 40
        Hint = 'Add a new playlist'
        Alignment = taCenter
        AllowAllUp = True
        Caption = 'New'
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
        Font.Height = -13
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
        ParentShowHint = False
        ShadowColor = clSkyBlue
        ShowHint = True
        Style = bsModern
        OnClick = btnNewPlaylistClick
      end
    end
    object btnOpenFile: TMPxpButton
      Left = 8
      Top = 52
      Width = 91
      Height = 40
      Hint = 'Search for an audiofile'
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'File'
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
      Font.Height = -13
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
      ParentShowHint = False
      ShadowColor = clSkyBlue
      ShowHint = True
      Style = bsModern
      OnClick = btnOpenFileClick
    end
    object edFileName: TEdit
      Left = 102
      Top = 60
      Width = 421
      Height = 27
      Hint = 'Search for an audiofile'
      Alignment = taCenter
      AutoSize = False
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 5
      OnMouseDown = edFileNameMouseDown
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 786
    Width = 1265
    Height = 29
    Align = alBottom
    BevelOuter = bvLowered
    Color = 5850948
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    ExplicitLeft = 2
    ExplicitTop = 764
    object lblStatus: TLabel
      Left = 8
      Top = 7
      Width = 35
      Height = 16
      Caption = 'Ready'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 1265
    Height = 37
    Align = alTop
    Color = 4865081
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    OnMouseDown = pnlCaptionMouseDown
    ExplicitLeft = 2
    ExplicitTop = 2
    ExplicitWidth = 283
    object lblCaption: TLabel
      Left = 8
      Top = 11
      Width = 123
      Height = 16
      Hint = 'Channel number'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Playlist Editor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Layout = tlCenter
      OnMouseDown = pnlCaptionMouseDown
    end
    object btnMinimize: TMPxpButton
      Left = 1131
      Top = 1
      Width = 67
      Height = 35
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Alignment = taCenter
      AllowAllUp = True
      Caption = ''
      Color = 5914932
      ColorWhenDown = 11363625
      ColorWhenUp = 5914932
      Behavior = bbPushButton
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC000000C40E0000C40E00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000777777777777777770000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
        FFFFF0000000FFFFFFFFFFFFFFFFF00000007777777777777777700000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000}
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clWhite
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      NumGlyphs = 1
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      Transparent = True
      OnClick = btnMinimizeClick
      ExplicitLeft = 636
      ExplicitTop = 0
      ExplicitHeight = 41
    end
    object btnMaxNormal: TMPxpButton
      Left = 1064
      Top = 1
      Width = 67
      Height = 35
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Alignment = taCenter
      AllowAllUp = True
      Caption = ''
      Color = 5914932
      ColorWhenDown = 11363625
      ColorWhenUp = 5914932
      Behavior = bbPushButton
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC000000C40E0000C40E00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000FFFFFFFFFFF
        FFFF00000000FFFFFFFFFFFFFFFFF0000000FF8888888888888FF0000000FF80
        00000000008FF0000000FF8000000000008FF0000000FF8000000000008FF000
        0000FF8000000000008FF0000000FF8000000000008FF0000000FF8000000000
        008FF0000000FF8000000000008FF0000000FF8000000000008FF0000000FF80
        00000000008FF0000000FF8000000000008FF0000000FF8000000000008FF000
        0000FF8888888888888FF0000000FFFFFFFFFFFFFFFFF00000000FFFFFFFFFFF
        FFFF00000000}
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clWhite
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      NumGlyphs = 1
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      Transparent = True
      OnClick = btnMaxNormalClick
      ExplicitLeft = 703
      ExplicitTop = 0
      ExplicitHeight = 41
    end
    object btnExit: TMPxpButton
      Left = 1198
      Top = 1
      Width = 66
      Height = 35
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Alignment = taCenter
      AllowAllUp = True
      Caption = ''
      Color = 5914932
      ColorWhenDown = 11363625
      ColorWhenUp = 5914932
      Behavior = bbPushButton
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      Glyph.Data = {
        42010000424D4201000000000000760000002800000011000000110000000100
        040000000000CC000000C40E0000C40E00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00099900000000
        0999000000009999900000009999900000009999990000099999900000009999
        9990009999999000000009999999099999990000000000999999999999900000
        0000000999999999990000000000000099999999900000000000000009999999
        0000000000000000999999999000000000000009999999999900000000000099
        9999999999900000000009999999099999990000000099999990009999999000
        0000999999000009999990000000999990000000999990000000099900000000
        099900000000}
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clWhite
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      NumGlyphs = 1
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      Transparent = True
      OnClick = btnExitClick
      ExplicitLeft = 770
      ExplicitTop = 0
      ExplicitHeight = 41
    end
  end
end

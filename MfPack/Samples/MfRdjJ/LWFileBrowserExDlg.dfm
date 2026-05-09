object LWFileBrowserExDlg: TLWFileBrowserExDlg
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Select an audio file'
  ClientHeight = 640
  ClientWidth = 940
  Color = 5850948
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object Splitter1: TSplitter
    Left = 270
    Top = 111
    Width = 6
    Height = 450
    Color = 4473924
    ParentColor = False
    ExplicitTop = 86
    ExplicitHeight = 475
  end
  object SplitterPreview: TSplitter
    Left = 704
    Top = 111
    Width = 6
    Height = 450
    Align = alRight
    Color = 4473924
    ParentColor = False
    ExplicitTop = 86
    ExplicitHeight = 475
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 940
    Height = 111
    Align = alTop
    BevelOuter = bvNone
    Color = 5850948
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
    object lblLocation: TLabel
      Left = 10
      Top = 7
      Width = 53
      Height = 17
      Caption = 'Location'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbxLocations: TComboBox
      Left = 10
      Top = 28
      Width = 260
      Height = 25
      Style = csOwnerDrawFixed
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 19
      ParentFont = False
      TabOrder = 0
      OnDrawItem = cbxLocationsDrawItem
      OnSelect = cbxLocationsSelect
    end
    object edtPath: TEdit
      Left = 390
      Top = 28
      Width = 237
      Height = 25
      Hint = 'Find a local map or network station. (like: \\MyServer)'
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnKeyDown = edtPathKeyDown
    end
    object btnGo: TMPxpButton
      Left = 636
      Top = 23
      Width = 68
      Height = 40
      Alignment = taCenter
      Caption = 'Find'
      Color = 6303744
      ColorWhenDown = 11363625
      ColorWhenUp = 6303744
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -13
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = [fsBold]
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnGoClick
    end
    object cbxFileFilter: TFilterComboBox
      Left = 4
      Top = 76
      Width = 703
      Height = 25
      Color = 5850948
      FileList = flbFiles
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object btnScanNetwork: TMPxpButton
      Left = 276
      Top = 23
      Width = 91
      Height = 40
      Hint = 'Scan network again.'
      Alignment = taCenter
      Caption = 'Scan Network'
      Color = 6303744
      ColorWhenDown = 11363625
      ColorWhenUp = 6303744
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -13
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = [fsBold]
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShadowColor = clSkyBlue
      ShowHint = True
      Style = bsModern
      WordWrap = True
      OnClick = btnScanNetworkClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 561
    Width = 940
    Height = 79
    Align = alBottom
    BevelOuter = bvNone
    Color = 5850948
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 1
    ExplicitTop = 544
    ExplicitWidth = 934
    object lblSelectedFile: TLabel
      Left = 14
      Top = 15
      Width = 79
      Height = 17
      Caption = 'Selected file:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblDuration: TLabel
      Left = 14
      Top = 44
      Width = 113
      Height = 17
      Caption = 'Duration: 00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnOk: TMPxpButton
      Left = 707
      Top = 31
      Width = 107
      Height = 40
      Alignment = taCenter
      Caption = 'Ok'
      Color = 6303744
      ColorWhenDown = 11363625
      ColorWhenUp = 6303744
      Checked = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -13
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = [fsBold]
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnOkClick
    end
    object btnCancel: TMPxpButton
      Left = 821
      Top = 31
      Width = 107
      Height = 40
      Alignment = taCenter
      Caption = 'Cancel'
      Color = 6303744
      ColorWhenDown = 11363625
      ColorWhenUp = 6303744
      Checked = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clFuchsia
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -13
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = [fsBold]
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnCancelClick
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 111
    Width = 270
    Height = 450
    Align = alLeft
    BevelOuter = bvNone
    Color = 5850948
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 86
    ExplicitHeight = 458
    object lbFolders: TListBox
      Left = 0
      Top = 0
      Width = 270
      Height = 450
      Style = lbOwnerDrawFixed
      Align = alClient
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 21
      ParentFont = False
      TabOrder = 0
      OnDblClick = lbFoldersDblClick
      OnDrawItem = lbFoldersDrawItem
      ExplicitTop = 17
      ExplicitHeight = 458
    end
  end
  object flbFiles: TFileListBox
    Left = 276
    Top = 111
    Width = 428
    Height = 450
    Align = alClient
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ItemHeight = 17
    ParentColor = True
    ParentFont = False
    ShowGlyphs = True
    TabOrder = 3
    OnChange = flbFilesChange
    OnDblClick = flbFilesDblClick
    ExplicitLeft = 288
    ExplicitTop = 103
    ExplicitHeight = 475
  end
  object pnlPreview: TPanel
    Left = 710
    Top = 111
    Width = 230
    Height = 450
    Align = alRight
    BevelOuter = bvNone
    Color = 5850948
    ParentBackground = False
    TabOrder = 4
    ExplicitLeft = 704
    ExplicitTop = 86
    ExplicitHeight = 458
    object lblPreview: TLabel
      Left = 0
      Top = 0
      Width = 230
      Height = 33
      Align = alTop
      Alignment = taCenter
      Caption = 'Preview'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object imgPreview: TImage
      Left = 0
      Top = 33
      Width = 230
      Height = 417
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
      ExplicitTop = 38
      ExplicitHeight = 412
    end
  end
end

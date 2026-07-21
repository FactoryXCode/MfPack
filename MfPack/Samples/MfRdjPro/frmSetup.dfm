object frmSetup: TfrmSetup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'RDJ Setup'
  ClientHeight = 1099
  ClientWidth = 689
  Color = 5850948
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label12: TLabel
    Left = 20
    Top = 296
    Width = 128
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Password:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label15: TLabel
    Left = 20
    Top = 377
    Width = 128
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Broadcast Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object pnlBroadCastSettings: TPanel
    Left = -1
    Top = 52
    Width = 682
    Height = 982
    BevelOuter = bvNone
    Color = 5850948
    Padding.Left = 12
    Padding.Top = 12
    Padding.Right = 12
    Padding.Bottom = 12
    ParentBackground = False
    ShowCaption = False
    TabOrder = 3
    DesignSize = (
      682
      982)
    object Bevel8: TBevel
      Left = 8
      Top = 327
      Width = 671
      Height = 78
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Bevel10: TBevel
      Left = 8
      Top = 427
      Width = 671
      Height = 70
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Bevel9: TBevel
      Left = 9
      Top = 24
      Width = 670
      Height = 281
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Label18: TLabel
      Left = 18
      Top = 15
      Width = 163
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Caddy / json settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object lblCaddyPath: TLabel
      Left = 20
      Top = 49
      Width = 127
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Root Path:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblCaddyConfigPath: TLabel
      Left = 20
      Top = 80
      Width = 127
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Config Path:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblCaddyJsonNowPlayingPath: TLabel
      Left = 20
      Top = 112
      Width = 127
      Height = 23
      Hint = 'Caddy'#39's NowPlaying.json path'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'json Path:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblCaddyCmdLine: TLabel
      Left = 20
      Top = 256
      Width = 127
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Caddy Command:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label21: TLabel
      Left = 20
      Top = 144
      Width = 127
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Artwork Path:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label35: TLabel
      Left = 20
      Top = 420
      Width = 75
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Camera'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label25: TLabel
      Left = 97
      Top = 449
      Width = 50
      Height = 16
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = 'Camera:'
      ParentBiDiMode = False
    end
    object Label22: TLabel
      Left = 22
      Top = 176
      Width = 127
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Video Path:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label24: TLabel
      Left = 21
      Top = 208
      Width = 127
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Content Type URL:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label27: TLabel
      Left = 17
      Top = 357
      Width = 130
      Height = 22
      Hint = 'MP4 segment file size in milliseconds. Default is 2000 ms'
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MP4 Segmentsize:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblMp4SegmentSize: TLabel
      Left = 540
      Top = 358
      Width = 107
      Height = 16
      AutoSize = False
      Caption = '2000 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 20
      Top = 320
      Width = 75
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Mse MP4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object edtCaddyPath: TEdit
      Left = 155
      Top = 46
      Width = 474
      Height = 26
      Hint = 'Leave empty if Icecast/Caddy is on a server.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
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
      Text = 'C:\Caddy'
    end
    object edtCaddyConfigPath: TEdit
      Left = 155
      Top = 77
      Width = 474
      Height = 26
      Hint = 'Leave empty if Icecast/Caddy is o'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'C:\Caddy\caddy.cff'
    end
    object edtCaddyJsonNowPlayingPath: TEdit
      Left = 155
      Top = 109
      Width = 474
      Height = 26
      Hint = 'Caddy nowplaying.json path (Path on Server or local)'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'C:\Caddy\nowplaying.json'
    end
    object edtCaddyCmdLine: TEdit
      Left = 155
      Top = 253
      Width = 516
      Height = 26
      Hint = 
        'Caddy commandline.'#13#10'Leave empty if Caddy is starting as a servic' +
        'e.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'caddy.exe run --config "C:\Caddy\Caddy.cff" --adapter caddyfile'
    end
    object btnGetCaddyPath: TMPxpButton
      Left = 631
      Top = 46
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnGetCaddyPathClick
    end
    object btnGetCaddyConfigPath: TMPxpButton
      Left = 631
      Top = 77
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnGetCaddyConfigPathClick
    end
    object btnGetCaddyJsonPath: TMPxpButton
      Left = 631
      Top = 110
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnGetCaddyJsonPathClick
    end
    object edtCaddyArtworkPath: TEdit
      Left = 155
      Top = 141
      Width = 474
      Height = 26
      Hint = 'Enter the Caddy artwork location '
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'C:\Caddy\Artwork'
    end
    object btnCaddyArtworkPath: TMPxpButton
      Left = 631
      Top = 142
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnCaddyArtworkPathClick
    end
    object cbSelectCamera: TComboBox
      Left = 154
      Top = 446
      Width = 361
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      BiDiMode = bdLeftToRight
      Color = 5850948
      ParentBiDiMode = False
      TabOrder = 9
      OnChange = cbSelectCameraChange
    end
    object btnRefreshCameras: TMPxpButton
      Left = 531
      Top = 439
      Width = 106
      Height = 40
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Refresh Cameras'
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
      OnClick = btnOkClick
    end
    object edtCaddyVideoPath: TEdit
      Left = 157
      Top = 173
      Width = 474
      Height = 26
      Hint = 'Enter the Caddy video location '
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      Text = 'C:\Caddy\Video'
    end
    object btnCaddyVideoPath: TMPxpButton
      Left = 633
      Top = 174
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnCaddyVideoPathClick
    end
    object edtCaddyContentTypeURL: TEdit
      Left = 156
      Top = 205
      Width = 474
      Height = 26
      Hint = 'Content default = video/mp4'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      Text = 'video/mp4'
    end
    object tbMp4SegmentSize: TMfTrackBar
      Left = 154
      Top = 349
      Width = 377
      Height = 34
      TabOrder = 14
      Color = 5850948
      ParentColor = False
      Minimum = 500
      Maximum = 9000
      Position = 2000
      ThumbWidth = 50
      ThumbHeight = 60
      TransparentColor = clBlack
      ThumbPictureHorz.Data = {
        07544269746D617012010000424D120100000000000076000000280000001300
        00000D00000001000400000000009C000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900C4C4C400FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00055555554445555555000000531425454045452413500000531425454045
        4524135000005314254540454524135000005314254540454524135000005314
        2545404545241350000053142545404545241350000053142545404545241350
        0000531425454045452413500000531425454045452413500000531425454045
        452413500000531425454045452413500000055555554445555555000000}
      ShowTicks = True
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickTextColor = clSilver
      TickPlacementH = tphBoth
      TickPlacementV = tpvBoth
      SmallChange = 100
      LargeChange = 100
      OnChange = tbMp4SegmentSizeChange
    end
  end
  object pnlGeneralSettings: TPanel
    Left = -1
    Top = 52
    Width = 682
    Height = 972
    BevelOuter = bvNone
    Color = 5850948
    Padding.Left = 12
    Padding.Top = 12
    Padding.Right = 12
    Padding.Bottom = 12
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      682
      972)
    object Bevel7: TBevel
      Left = 8
      Top = 417
      Width = 669
      Height = 66
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Bevel6: TBevel
      Left = 35
      Top = 225
      Width = 520
      Height = 46
    end
    object Bevel5: TBevel
      Left = 8
      Top = 801
      Width = 669
      Height = 152
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Bevel2: TBevel
      Left = 8
      Top = 209
      Width = 669
      Height = 175
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object lblAudioBufSize: TLabel
      Left = 14
      Top = 295
      Width = 154
      Height = 22
      Hint = ' in milliseconds. Default is 60 ms'
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Capture buffersize:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label4: TLabel
      Left = 24
      Top = 338
      Width = 582
      Height = 35
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Enlarge the audio buffersize, if you hear small disruptions or d' +
        'istortions in the audio stream. Default value for the audiobuffe' +
        'r is ~60 milliseconds.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 604
    end
    object lblChannels: TLabel
      Left = 47
      Top = 240
      Width = 121
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Channel decks:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblBuffSize: TLabel
      Left = 509
      Top = 295
      Width = 107
      Height = 16
      AutoSize = False
      Caption = '60 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 20
      Top = 201
      Width = 75
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'General'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Bevel3: TBevel
      Left = 8
      Top = 498
      Width = 669
      Height = 286
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Label2: TLabel
      Left = 20
      Top = 490
      Width = 123
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Audio recorder'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label3: TLabel
      Left = 15
      Top = 528
      Width = 154
      Height = 20
      Hint = 
        'Capture buffersize 10-120 milliseconds (0 - 10 = auto buffersize' +
        ', default = 60)'
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Capture buffersize:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblAudioRecBufSize: TLabel
      Left = 509
      Top = 529
      Width = 107
      Height = 16
      AutoSize = False
      Caption = '60 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 15
      Top = 609
      Width = 154
      Height = 20
      Hint = 'Latency in milliseconds. Default = 10 ms'
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Latency:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblSysLatency: TLabel
      Left = 509
      Top = 610
      Width = 107
      Height = 16
      AutoSize = False
      Caption = '10 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblAudioFormat: TLabel
      Left = 40
      Top = 655
      Width = 128
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Output format:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label5: TLabel
      Left = 20
      Top = 793
      Width = 169
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'Application directories'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label7: TLabel
      Left = 63
      Top = 819
      Width = 128
      Height = 23
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio Recordings:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblBufferDuration: TLabel
      Left = 179
      Top = 561
      Width = 469
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Capture buffer duration: 60 milliseconds.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 491
    end
    object lblLoopbackDecks: TLabel
      Left = 289
      Top = 240
      Width = 121
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Loopback decks:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel1: TBevel
      Left = 8
      Top = 14
      Width = 669
      Height = 179
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = False
    end
    object Bevel4: TBevel
      Left = 19
      Top = 96
      Width = 646
      Height = 63
    end
    object lblMainOut: TLabel
      Left = 35
      Top = 37
      Width = 135
      Height = 16
      Caption = 'Main output (MASTER):'
    end
    object lblCueOut: TLabel
      Left = 41
      Top = 129
      Width = 128
      Height = 16
      Caption = 'Cue output (PHONES):'
    end
    object lblHint: TLabel
      Left = 18
      Top = 166
      Width = 613
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Pick devices by name. Inactive devices are shown too (Disabled/U' +
        'nplugged/Not present).'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 635
    end
    object lblAudioRecorder: TLabel
      Left = 20
      Top = 5
      Width = 143
      Height = 18
      Alignment = taCenter
      AutoSize = False
      Caption = 'Enddpoint devices'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object lblMicIn: TLabel
      Left = 34
      Top = 450
      Width = 190
      Height = 16
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = 'Microphone input (MICROPHONE)'
      ParentBiDiMode = False
    end
    object Label19: TLabel
      Left = 63
      Top = 879
      Width = 128
      Height = 23
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Database:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label20: TLabel
      Left = 63
      Top = 909
      Width = 128
      Height = 23
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Artwork:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label23: TLabel
      Left = 63
      Top = 849
      Width = 128
      Height = 23
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Video Recordings:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object cbChannels: TComboBox
      Left = 179
      Top = 238
      Width = 80
      Height = 24
      Style = csDropDownList
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object tbAudioBufferDuration: TMfTrackBar
      Left = 179
      Top = 287
      Width = 312
      Height = 34
      TabOrder = 1
      Color = 5850948
      ParentColor = False
      Minimum = 30
      Maximum = 120
      Position = 60
      ThumbWidth = 50
      ThumbHeight = 60
      TransparentColor = clBlack
      ThumbPictureHorz.Data = {
        07544269746D617012010000424D120100000000000076000000280000001300
        00000D00000001000400000000009C000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900C4C4C400FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00055555554445555555000000531425454045452413500000531425454045
        4524135000005314254540454524135000005314254540454524135000005314
        2545404545241350000053142545404545241350000053142545404545241350
        0000531425454045452413500000531425454045452413500000531425454045
        452413500000531425454045452413500000055555554445555555000000}
      ShowTicks = True
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickTextColor = clSilver
      TickPlacementH = tphBoth
      TickPlacementV = tpvBoth
      OnChange = tbAudioBufferDurationChange
    end
    object tbRecCapBufferSize: TMfTrackBar
      Left = 180
      Top = 520
      Width = 311
      Height = 35
      Hint = 'Capture buffersize 10-120 (0 - 10 = auto buffersize)'
      TabOrder = 2
      Color = 5850948
      ParentColor = False
      Minimum = 30
      Maximum = 180
      Position = 60
      ThumbWidth = 50
      ThumbHeight = 60
      TransparentColor = clBlack
      ThumbPictureHorz.Data = {
        07544269746D617012010000424D120100000000000076000000280000001300
        00000D00000001000400000000009C000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900C4C4C400FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00055555554445555555000000531425454045452413500000531425454045
        4524135000005314254540454524135000005314254540454524135000005314
        2545404545241350000053142545404545241350000053142545404545241350
        0000531425454045452413500000531425454045452413500000531425454045
        452413500000531425454045452413500000055555554445555555000000}
      ShowTicks = True
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickTextColor = clSilver
      TickPlacementH = tphBoth
      TickPlacementV = tpvBoth
      OnChange = tbRecCapBufferSizeChange
    end
    object tbSysLatency: TMfTrackBar
      Left = 180
      Top = 601
      Width = 311
      Height = 34
      TabOrder = 3
      Color = 5850948
      ParentColor = False
      Maximum = 500
      Position = 10
      ThumbWidth = 50
      ThumbHeight = 60
      TransparentColor = clBlack
      ThumbPictureHorz.Data = {
        07544269746D617012010000424D120100000000000076000000280000001300
        00000D00000001000400000000009C000000330B0000330B0000100000001000
        000000000000959595009B9B9B00A2A2A200B9B9B900C4C4C400FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00055555554445555555000000531425454045452413500000531425454045
        4524135000005314254540454524135000005314254540454524135000005314
        2545404545241350000053142545404545241350000053142545404545241350
        0000531425454045452413500000531425454045452413500000531425454045
        452413500000531425454045452413500000055555554445555555000000}
      ShowTicks = True
      DbFloor = -50.000000000000000000
      DbCeil = 6.000000000000000000
      TickColor = clAqua
      TickTextColor = clSilver
      TickPlacementH = tphBoth
      TickPlacementV = tpvBoth
      OnChange = tbSysLatencyChange
    end
    object chkDontOverWrite: TMPxpButton
      Left = 38
      Top = 695
      Width = 200
      Height = 40
      Alignment = taCenter
      Caption = 'Don'#39't overwrite excisting files'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
    end
    object chkDisableMMCSS: TMPxpButton
      Left = 244
      Top = 695
      Width = 200
      Height = 40
      Alignment = taCenter
      Caption = 'Disable MMCSS'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Behavior = bbCheckBox
      Checked = False
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
    end
    object chkUsePCMFormat: TMPxpButton
      Left = 38
      Top = 739
      Width = 200
      Height = 40
      Alignment = taCenter
      Caption = 'Default PCM audio output format'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Behavior = bbCheckBox
      Checked = False
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
    end
    object chkEnableStreamSwitchDetection: TMPxpButton
      Left = 244
      Top = 739
      Width = 200
      Height = 40
      Alignment = taCenter
      Caption = 'Enable stream switch detection'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
    end
    object cbxOutputFormat: TComboBox
      Left = 180
      Top = 655
      Width = 201
      Height = 24
      Hint = 'Select a playlist'
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ItemIndex = 1
      ParentColor = True
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Text = 'FLAC (Lossless)'
      Items.Strings = (
        'WAV (Lossless)'
        'FLAC (Lossless)')
    end
    object edAudioRecordingsDirName: TEdit
      Left = 199
      Top = 817
      Width = 426
      Height = 26
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      Text = 'AudioRecordings'
    end
    object cbLoopbackDecks: TComboBox
      Left = 421
      Top = 238
      Width = 80
      Height = 24
      Style = csDropDownList
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
    end
    object cbMainOut: TComboBox
      Left = 180
      Top = 35
      Width = 451
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Color = 5850948
      TabOrder = 11
    end
    object cbCueOut: TComboBox
      Left = 181
      Top = 127
      Width = 450
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Color = 5850948
      TabOrder = 12
    end
    object chkPfl: TMPxpButton
      Left = 30
      Top = 77
      Width = 245
      Height = 40
      Alignment = taCenter
      Caption = 'Enable Headphones output (PFL / CUE)'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      OnClick = chkPflClick
    end
    object cbMicIn: TComboBox
      Left = 231
      Top = 448
      Width = 400
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      BiDiMode = bdLeftToRight
      Color = 5850948
      ParentBiDiMode = False
      TabOrder = 14
    end
    object chkMicDeckEnabled: TMPxpButton
      Left = 22
      Top = 397
      Width = 245
      Height = 40
      Alignment = taCenter
      Caption = 'Enable Microphone input'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      OnClick = chkPflClick
    end
    object edDataBaseDirName: TEdit
      Left = 199
      Top = 877
      Width = 426
      Height = 26
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      Text = 'Data'
    end
    object btnAudioRecordingsDirName: TMPxpButton
      Left = 629
      Top = 817
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnAudioRecordingsDirNameClick
    end
    object btnDataBaseDirName: TMPxpButton
      Left = 629
      Top = 877
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnDataBaseDirNameClick
    end
    object edArtworkDirName: TEdit
      Left = 199
      Top = 907
      Width = 426
      Height = 26
      Hint = 'The sub-directory where covers are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      Text = 'Artwork'
    end
    object btnArtworkDirName: TMPxpButton
      Left = 629
      Top = 907
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnArtworkDirNameClick
    end
    object edVideoRecordingsDirName: TEdit
      Left = 199
      Top = 847
      Width = 426
      Height = 26
      Hint = 'The sub-directory where recordings are stored.'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      BevelKind = bkTile
      BevelOuter = bvRaised
      Color = 5850948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      Text = 'VideoRecordings'
    end
    object btnVideoRecordingsDirName: TMPxpButton
      Left = 629
      Top = 847
      Width = 40
      Height = 26
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = '...'
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
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphRight
      LightColor = 12348265
      ParentColor = False
      ParentFont = False
      ShadowColor = clSkyBlue
      Style = bsModern
      OnClick = btnVideoRecordingsDirNameClick
    end
    object chkOverrideSleepMode: TMPxpButton
      Left = 450
      Top = 695
      Width = 200
      Height = 40
      Hint = 'Prevents the OS is going to sleep mode while RDJPro is running.'
      Alignment = taCenter
      Caption = 'Override System Sleep'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackColor = clAqua
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShadowColor = clAqua
      ShowHint = True
      SlowDecease = True
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 1042
    Width = 689
    Height = 57
    Align = alBottom
    BevelOuter = bvNone
    Color = 5850948
    Padding.Left = 12
    Padding.Top = 8
    Padding.Right = 12
    Padding.Bottom = 8
    ParentBackground = False
    TabOrder = 1
    object btnOk: TMPxpButton
      Left = 443
      Top = 1
      Width = 106
      Height = 40
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
      Caption = 'Ok'
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
      Left = 557
      Top = 1
      Width = 106
      Height = 40
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taCenter
      AllowAllUp = True
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
  object pnlAudioEndPoints: TPanel
    Left = 0
    Top = 0
    Width = 689
    Height = 41
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvSpace
    Color = 9598320
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    ExplicitTop = 2
    object chkGeneralSettings: TMPxpButton
      Left = 4
      Top = 4
      Width = 107
      Height = 38
      Alignment = taCenter
      Caption = 'General'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Down = True
      Behavior = bbCheckBox
      Checked = True
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      WordWrap = True
      OnClick = chkGeneralSettingsClick
    end
    object chkBroadcastSettings: TMPxpButton
      Left = 112
      Top = 4
      Width = 107
      Height = 38
      Alignment = taCenter
      Caption = 'Broadcast'
      Color = 6899524
      ColorWhenDown = 10045252
      ColorWhenUp = 6899524
      Behavior = bbCheckBox
      Checked = False
      GlyphUnchecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        1900001900000019190019000000190019001919000060606000607E6000956B
        430000005C002F0000001D00000002000000020002000000130000002F000200
        2F00000042000000420000005C0000005C0000025C0000026F00001D7800132F
        7800002F8B001D5393002F5393002F669C0042669C005C789C007893A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
        A500A5A5A500A5A5A500A5A5A500A5A5A50095A1A500413C3C00191919000000
        A50000A5000000A5A500A5000000A500A500A5A50000A5A5A500000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      GlyphChecked.Data = {
        C6040000424DC60400000000000036040000280000000C0000000C0000000100
        08000000000090000000230B0000230B00000001000000010000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A600004ABD0094081800841839006B184A006B296B0042297B00524A94006B4A
        94001839A5001842A500004ABD00315ABD005A6BBD00296BCE005A84D6007B94
        D6005A94E70084B5EF0094B5EF0094C6F700A5C6F700BDD6F700D6EFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        00000000000000000010151717151000000000001116181A1A18161100000010
        16191C1E1E1C191610000015181B1F20201F1B18150000171A1D202121201D1A
        170000171A1D202121201D1A17000015181B1F20201F1B181500001016191C1E
        1E1C1916100000001116181A1A18161100000000001015171715100000000000
        00000000000000000000}
      ImageIndexUnchecked = 0
      ImageIndexChecked = 0
      ShowFocusRect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      GlyphTransparentColor = clBlack
      GlyphTransparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -18
      HotTrackFont.Name = 'Segoe UI'
      HotTrackFont.Style = []
      Layout = blGlyphTop
      LightColor = clSkyBlue
      ParentColor = False
      ParentFont = False
      ShadowColor = clAqua
      SlowDecease = True
      WordWrap = True
      OnClick = chkBroadcastSettingsClick
    end
  end
end

object VideoFormatDlg: TVideoFormatDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Output Video and Audio Format'
  ClientHeight = 526
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 489
    Width = 719
    Height = 37
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 170
    ExplicitWidth = 506
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 8
    Width = 713
    Height = 55
    Margins.Top = 8
    Align = alTop
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 2
    object Label1: TLabel
      Left = 5
      Top = 4
      Width = 105
      Height = 13
      Caption = ' Container Format '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label6: TLabel
      Left = 8
      Top = 55
      Width = 616
      Height = 20
      AutoSize = False
      WordWrap = True
    end
    object Label5: TLabel
      Left = 220
      Top = 28
      Width = 483
      Height = 46
      AutoSize = False
      Caption = 'AVI file container. Supported in Windows 8.1 and and later.'
    end
    object cbxContainerFmt: TComboBox
      Left = 8
      Top = 24
      Width = 199
      Height = 21
      TabOrder = 0
      Text = 'Audio Video Interleave (AVI)'
      Items.Strings = (
        'Audio Video Interleave (AVI)'
        'MPEG-4 Video (mp4)'
        'Windows Media Video (WMV)')
    end
  end
  object Button1: TButton
    Left = 554
    Top = 548
    Width = 73
    Height = 23
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 633
    Top = 548
    Width = 73
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 74
    Width = 713
    Height = 169
    Margins.Top = 8
    Align = alTop
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 3
    object Label2: TLabel
      Left = 5
      Top = 8
      Width = 81
      Height = 13
      Caption = ' Video Format '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label4: TLabel
      Left = 7
      Top = 64
      Width = 69
      Height = 13
      Caption = ' Video Format '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label7: TLabel
      Left = 222
      Top = 30
      Width = 483
      Height = 46
      AutoSize = False
      Caption = 'H.262'
    end
    object ComboBox1: TComboBox
      Left = 7
      Top = 27
      Width = 209
      Height = 21
      TabOrder = 0
      Text = 'H.262 (MPEG 2)'
      Items.Strings = (
        'None'
        'H.262 (MPEG 2)'
        'H.264'
        'H.265/'
        'WMV3')
    end
    object ComboBox3: TComboBox
      Left = 7
      Top = 82
      Width = 209
      Height = 21
      TabOrder = 1
      Text = 'SD (Standard Definition, 360p)'
      Items.Strings = (
        'SD (Standard Definition, 360p)'
        'SD (Standard Definition, 480p)'
        'HD (High Definition, 720p)'
        'Full HD (FHD, 1080p)'
        'QHD (Quad HD, 1440p)'
        '2K Video (1080p)'
        '4k Video (4K, 2160p)'
        '8k Video (8K, 4320p)')
    end
    object StaticText3: TStaticText
      Left = 8
      Top = 109
      Width = 120
      Height = 20
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = sbsSunken
      Caption = 'Pixels (Width x Heigth)'
      Color = clMenuHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 2
      Transparent = False
    end
    object StaticText4: TStaticText
      Left = 128
      Top = 109
      Width = 120
      Height = 20
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = sbsSunken
      Caption = 'Aspect Ratio'
      Color = clMenuHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 3
      Transparent = False
    end
    object StaticText5: TStaticText
      Left = 8
      Top = 130
      Width = 120
      Height = 24
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = sbsSunken
      Caption = '640 x 360'
      Color = clSkyBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 4
      Transparent = False
    end
    object StaticText6: TStaticText
      Left = 128
      Top = 129
      Width = 120
      Height = 24
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = sbsSunken
      Caption = '4:3'
      Color = clSkyBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 5
      Transparent = False
    end
  end
  object Panel3: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 254
    Width = 713
    Height = 229
    Margins.Top = 8
    Align = alTop
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 4
    object Label3: TLabel
      Left = 5
      Top = 4
      Width = 82
      Height = 13
      Caption = ' Audio Format '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label8: TLabel
      Left = 421
      Top = 54
      Width = 290
      Height = 165
      AutoSize = False
      Caption = 'AVI file container. Supported in Windows 8.1 and and later.'
    end
    object ComboBox2: TComboBox
      Left = 8
      Top = 23
      Width = 212
      Height = 21
      TabOrder = 0
      Text = 'None'
      Items.Strings = (
        'None'
        'Advanced Audio Coding (AAC)'
        'Dolby AC-3 audio (ac3)'
        'Pulse Code Moduladed (PCM)'
        'Windows Media Audio 9 (wma)'
        'MPEG Audio Layer-3 (mp3)'
        'Free Lossless Audio Codec (flac)'
        '')
    end
    object stxtBitRate: TStaticText
      Left = 7
      Top = 53
      Width = 101
      Height = 20
      Alignment = taCenter
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = sbsSunken
      Caption = 'Bit Rate (kbps)'
      Color = clMenuHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      Transparent = False
    end
    object stxtSampleRate: TStaticText
      Left = 108
      Top = 53
      Width = 101
      Height = 20
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'Sampling Rate (Khz)'
      Color = clMenuHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 2
      Transparent = False
    end
    object stxtBitsPerSample: TStaticText
      Left = 209
      Top = 53
      Width = 101
      Height = 20
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'Bits per sample'
      Color = clMenuHighlight
      ParentColor = False
      TabOrder = 3
      Transparent = False
    end
    object stxtChannels: TStaticText
      Left = 310
      Top = 53
      Width = 101
      Height = 20
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'Channels'
      Color = clMenuHighlight
      ParentColor = False
      TabOrder = 4
      Transparent = False
    end
    object sgAudioFormats: TStringGrid
      Left = 6
      Top = 73
      Width = 405
      Height = 148
      DefaultColWidth = 100
      FixedCols = 0
      RowCount = 2
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      ScrollBars = ssVertical
      TabOrder = 5
    end
  end
end

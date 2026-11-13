object CastPlayerForm: TCastPlayerForm
  Left = 0
  Top = 0
  Caption = 'MfCastPlayer V2.2'
  ClientHeight = 799
  ClientWidth = 1036
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblState: TLabel
    Left = 0
    Top = 719
    Width = 1036
    Height = 18
    Align = alBottom
    AutoSize = False
    Caption = 'State: Idle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = False
    ExplicitTop = 637
    ExplicitWidth = 700
  end
  object memLog: TMemo
    Left = 0
    Top = 737
    Width = 1036
    Height = 62
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 345
    Width = 1036
    Height = 374
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
  end
  object pnlCtrl: TPanel
    Left = 0
    Top = 0
    Width = 1036
    Height = 345
    Align = alTop
    Constraints.MinHeight = 335
    Constraints.MinWidth = 1036
    TabOrder = 2
    object Bevel2: TBevel
      Left = 6
      Top = 15
      Width = 590
      Height = 288
    end
    object Bevel1: TBevel
      Left = 602
      Top = 15
      Width = 437
      Height = 288
    end
    object lblDevices: TLabel
      Left = 16
      Top = 32
      Width = 44
      Height = 13
      Caption = 'Devices'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSource: TLabel
      Left = 16
      Top = 88
      Width = 168
      Height = 13
      Caption = 'Media file or direct media URL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblCaptureCodec: TLabel
      Left = 614
      Top = 170
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Capture codec'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblArtwork: TLabel
      Left = 614
      Top = 56
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Artwork'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblSeek: TLabel
      Left = 16
      Top = 177
      Width = 80
      Height = 13
      Caption = 'Position (sec.)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblVolume: TLabel
      Left = 16
      Top = 233
      Width = 65
      Height = 13
      Caption = 'Volume 0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 611
      Top = 5
      Width = 62
      Height = 20
      Alignment = taCenter
      AutoSize = False
      Caption = 'Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Bevel3: TBevel
      Left = 1
      Top = 309
      Width = 1034
      Height = 35
      Align = alBottom
      ExplicitTop = 310
    end
    object Label2: TLabel
      Left = 18
      Top = 5
      Width = 89
      Height = 20
      Alignment = taCenter
      AutoSize = False
      Caption = 'Chromecast'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object lblAudioStream: TLabel
      Left = 614
      Top = 134
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio stream'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 615
      Top = 98
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Subtitles'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 615
      Top = 208
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Stream Format'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object trkVolume: TTrackBar
      Left = 16
      Top = 252
      Width = 505
      Height = 43
      Max = 100
      ShowSelRange = False
      TabOrder = 22
      TickStyle = tsNone
      OnChange = trkVolumeChange
    end
    object cbxDevices: TComboBox
      Left = 18
      Top = 51
      Width = 412
      Height = 24
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = SourceOrDeviceChanged
    end
    object edtSource: TEdit
      Left = 18
      Top = 107
      Width = 490
      Height = 24
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvRaised
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = SourceOrDeviceChanged
    end
    object edtArtwork: TEdit
      Left = 700
      Top = 53
      Width = 239
      Height = 21
      Hint = 'Artwork for audio files or Audio + artwork desktop casting'
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvRaised
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnChange = SourceOrDeviceChanged
    end
    object cbxSubtitleLanguage: TComboBox
      Left = 701
      Top = 93
      Width = 239
      Height = 24
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = SubtitleSelectionChanged
    end
    object cbxCaptureCodec: TComboBox
      Left = 700
      Top = 165
      Width = 123
      Height = 24
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 4
      Text = 'Automatic'
      Items.Strings = (
        'Automatic'
        'H.264'
        'HEVC')
    end
    object cbxAudioStream: TComboBox
      Left = 700
      Top = 129
      Width = 239
      Height = 24
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnChange = AudioSelectionChanged
    end
    object btnCastDiscover: TButton
      Left = 435
      Top = 49
      Width = 74
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Discover'
      TabOrder = 6
      WordWrap = True
      OnClick = btnDiscoverClick
    end
    object btnRefresh: TButton
      Left = 513
      Top = 50
      Width = 74
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Refresh'
      TabOrder = 7
      WordWrap = True
      OnClick = btnRefreshClick
    end
    object btnBrowse: TButton
      Left = 513
      Top = 106
      Width = 74
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Browse...'
      TabOrder = 8
      WordWrap = True
      OnClick = btnBrowseClick
    end
    object cbxKeepOnTop: TCheckBox
      Left = 927
      Top = 245
      Width = 93
      Height = 24
      Hint = 'Use a static image instead of the camera.'
      Caption = 'Keep on top'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      WordWrap = True
      OnClick = cbxKeepOnTopClick
    end
    object cbxCaptureMode: TComboBox
      Left = 701
      Top = 203
      Width = 181
      Height = 24
      Hint = 'Streams to capture when using Cast desktop'
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      Text = 'Audio + video'
      OnChange = CaptureModeChanged
      Items.Strings = (
        'Audio + video'
        'Audio only'
        'Video only'
        'Audio + artwork')
    end
    object chkEmbeddedSubtitles: TCheckBox
      Left = 946
      Top = 84
      Width = 56
      Height = 42
      Hint = 'Use a static image instead of the camera.'
      Caption = 'Enable subtitles'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      WordWrap = True
      OnClick = SourceOrDeviceChanged
    end
    object btnBrowseArtwork: TButton
      Left = 946
      Top = 50
      Width = 74
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Browse...'
      TabOrder = 11
      WordWrap = True
      OnClick = btnBrowseArtworkClick
    end
    object MPxpButton6: TButton
      Left = 98
      Top = 345
      Width = 74
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Cast'
      TabOrder = 13
      WordWrap = True
    end
    object btnCast: TButton
      Left = 7
      Top = 314
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Cast'
      TabOrder = 14
      WordWrap = True
      OnClick = btnCastClick
    end
    object btnPlay: TButton
      Left = 101
      Top = 314
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Play'
      TabOrder = 15
      WordWrap = True
      OnClick = btnPlayClick
    end
    object btnPause: TButton
      Left = 195
      Top = 314
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Pause'
      TabOrder = 16
      WordWrap = True
      OnClick = btnPauseClick
    end
    object btnStop: TButton
      Left = 289
      Top = 314
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Stop'
      TabOrder = 17
      WordWrap = True
      OnClick = btnStopClick
    end
    object btnDisconnect: TButton
      Left = 390
      Top = 314
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Disconnect'
      TabOrder = 18
      WordWrap = True
      OnClick = btnDisconnectClick
    end
    object btnCastDesktop: TButton
      Left = 488
      Top = 314
      Width = 95
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Cast Desktop'
      TabOrder = 19
      WordWrap = True
      OnClick = btnCastDesktopClick
    end
    object btnSeek: TButton
      Left = 514
      Top = 195
      Width = 74
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Seek'
      TabOrder = 20
      WordWrap = True
      OnClick = btnSeekClick
    end
    object chkMuted: TCheckBox
      Left = 537
      Top = 255
      Width = 50
      Height = 34
      Hint = 'Use a static image instead of the camera.'
      Caption = 'Mute'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      WordWrap = True
      OnClick = chkMutedClick
    end
    object trkSeek: TTrackBar
      Left = 16
      Top = 196
      Width = 493
      Height = 23
      Max = 7200
      ShowSelRange = False
      TabOrder = 23
      TickStyle = tsNone
      OnChange = trkSeekChange
    end
  end
end

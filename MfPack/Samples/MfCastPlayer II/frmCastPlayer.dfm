object CastPlayerForm: TCastPlayerForm
  Left = 0
  Top = 0
  Caption = 'MfCastPlayer V2.1'
  ClientHeight = 717
  ClientWidth = 700
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
    Top = 637
    Width = 700
    Height = 18
    Align = alBottom
    AutoSize = False
    Caption = 'State: Idle'
    Color = 1977626
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object memLog: TMemo
    Left = 0
    Top = 655
    Width = 700
    Height = 62
    Align = alBottom
    Color = 1977626
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitLeft = -1
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 299
    Width = 700
    Height = 338
    Align = alClient
    BevelOuter = bvNone
    Color = 1977626
    ParentBackground = False
    TabOrder = 1
  end
  object pnlCtrl: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 299
    Align = alTop
    TabOrder = 2
    object Bevel2: TBevel
      Left = 4
      Top = 17
      Width = 298
      Height = 142
    end
    object Bevel1: TBevel
      Left = 308
      Top = 17
      Width = 381
      Height = 142
    end
    object lblDevices: TLabel
      Left = 16
      Top = 30
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
      Top = 82
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
      Left = 320
      Top = 108
      Width = 70
      Height = 13
      Caption = 'Capture codec'
    end
    object lblArtwork: TLabel
      Left = 321
      Top = 30
      Width = 38
      Height = 13
      Caption = 'Artwork'
    end
    object lblSeek: TLabel
      Left = 16
      Top = 172
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
      Top = 231
      Width = 42
      Height = 13
      Caption = 'Volume'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 317
      Top = 9
      Width = 58
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Bevel3: TBevel
      Left = 1
      Top = 264
      Width = 698
      Height = 34
      Align = alBottom
      ExplicitLeft = 2
      ExplicitTop = 273
    end
    object Label2: TLabel
      Left = 11
      Top = 9
      Width = 82
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Chromecast'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object lblAudioStream: TLabel
      Left = 320
      Top = 84
      Width = 63
      Height = 13
      Caption = 'Audio stream'
    end
    object cbxDevices: TComboBox
      Left = 16
      Top = 49
      Width = 279
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = SourceOrDeviceChanged
    end
    object btnDiscover: TButton
      Left = 151
      Top = 23
      Width = 72
      Height = 25
      Caption = 'Discover'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 1
      OnClick = btnDiscoverClick
    end
    object btnRefresh: TButton
      Left = 223
      Top = 23
      Width = 72
      Height = 25
      Caption = 'Refresh'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object edtSource: TEdit
      Left = 16
      Top = 101
      Width = 279
      Height = 21
      TabOrder = 3
      OnChange = SourceOrDeviceChanged
    end
    object btnBrowse: TButton
      Left = 223
      Top = 75
      Width = 72
      Height = 25
      Caption = 'Browse...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 4
      OnClick = btnBrowseClick
    end
    object edtArtwork: TEdit
      Left = 370
      Top = 27
      Width = 239
      Height = 21
      Hint = 'Artwork only works in combination with audio only.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnChange = SourceOrDeviceChanged
    end
    object btnBrowseArtwork: TButton
      Left = 611
      Top = 25
      Width = 72
      Height = 25
      Caption = 'Browse...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 12
      OnClick = btnBrowseArtworkClick
    end
    object chkEmbeddedSubtitles: TCheckBox
      Left = 321
      Top = 56
      Width = 98
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Enable subtitles'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = SourceOrDeviceChanged
    end
    object trkSeek: TTrackBar
      Left = 14
      Top = 191
      Width = 677
      Height = 29
      Max = 7200
      Frequency = 300
      TabOrder = 6
      OnChange = trkSeekChange
    end
    object btnSeek: TButton
      Left = 120
      Top = 167
      Width = 52
      Height = 25
      Caption = 'Seek'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 7
      OnClick = btnSeekClick
    end
    object trkVolume: TTrackBar
      Left = 120
      Top = 231
      Width = 571
      Height = 26
      Hint = 'Global: Ctrl+Alt+Up / Ctrl+Alt+Down'
      Max = 100
      ParentShowHint = False
      Frequency = 10
      Position = 75
      ShowHint = True
      TabOrder = 8
      OnChange = trkVolumeChange
    end
    object chkMuted: TCheckBox
      Left = 67
      Top = 230
      Width = 55
      Height = 17
      Hint = 'Global: Ctrl+Alt+M'
      Caption = 'Mute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = chkMutedClick
    end
    object cbxSubtitleLanguage: TComboBox
      Left = 425
      Top = 53
      Width = 184
      Height = 21
      Style = csDropDownList
      TabOrder = 10
      OnChange = SubtitleSelectionChanged
    end
    object cbxCaptureCodec: TComboBox
      Left = 396
      Top = 105
      Width = 106
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 13
      Text = 'Automatic'
      Items.Strings = (
        'Automatic'
        'H.264'
        'HEVC')
    end
    object chkCaptureAudio: TCheckBox
      Left = 515
      Top = 107
      Width = 168
      Height = 17
      Caption = 'Capture system audio'
      Checked = True
      State = cbChecked
      TabOrder = 20
    end
    object btnCast: TButton
      Left = 11
      Top = 268
      Width = 72
      Height = 25
      Hint = 'Global: Ctrl+Alt+C'
      Caption = 'Cast'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = btnCastClick
    end
    object btnPlay: TButton
      Left = 85
      Top = 268
      Width = 72
      Height = 25
      Hint = 'Global: Ctrl+Alt+F5'
      Caption = 'Play'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = btnPlayClick
    end
    object btnPause: TButton
      Left = 157
      Top = 268
      Width = 72
      Height = 25
      Hint = 'Global: Ctrl+Alt+F6'
      Caption = 'Pause'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = btnPauseClick
    end
    object btnStop: TButton
      Left = 230
      Top = 268
      Width = 72
      Height = 25
      Hint = 'Global: Ctrl+Alt+F7'
      Caption = 'Stop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = btnStopClick
    end
    object btnDisconnect: TButton
      Left = 303
      Top = 268
      Width = 72
      Height = 25
      Caption = 'Disconnect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 18
      OnClick = btnDisconnectClick
    end
    object btnCastDesktop: TButton
      Left = 380
      Top = 268
      Width = 104
      Height = 25
      Hint = 'Global: Ctrl+Alt+D'
      Caption = 'Cast desktop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = btnCastDesktopClick
    end
    object cbxKeepOnTop: TCheckBox
      Left = 321
      Top = 134
      Width = 88
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Keep on top'
      Checked = True
      State = cbChecked
      TabOrder = 21
      OnClick = cbxKeepOnTopClick
    end
    object cbxAudioStream: TComboBox
      Left = 396
      Top = 80
      Width = 213
      Height = 21
      Style = csDropDownList
      TabOrder = 22
      OnChange = AudioSelectionChanged
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Cast media|*.mp4;*.m4v;*.m4a;*.webm;*.mp3;*.aac;*.flac;*.wav;*.o' +
      'gg;*.oga;*.opus;*.mkv;*.avi|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 232
    Top = 352
  end
  object ArtworkDialog: TOpenDialog
    Filter = 'Picture files|*.jpg;*.jpeg;*.png;*.bmp;*.gif|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 314
    Top = 352
  end
end

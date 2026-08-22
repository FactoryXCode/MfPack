object CastPlayerForm: TCastPlayerForm
  Left = 0
  Top = 0
  Caption = 'MfCastPlayer'
  ClientHeight = 692
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
    Top = 612
    Width = 700
    Height = 18
    Align = alBottom
    AutoSize = False
    Caption = 'State: Idle'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    ExplicitTop = 594
    ExplicitWidth = 695
  end
  object memLog: TMemo
    Left = 0
    Top = 630
    Width = 700
    Height = 62
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 695
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 285
    Width = 700
    Height = 327
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 271
    ExplicitWidth = 695
    ExplicitHeight = 329
  end
  object pnlCtrl: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 285
    Align = alTop
    TabOrder = 2
    ExplicitTop = 2
    object Bevel2: TBevel
      Left = 4
      Top = 16
      Width = 298
      Height = 119
    end
    object Bevel1: TBevel
      Left = 308
      Top = 17
      Width = 381
      Height = 118
    end
    object lblDevices: TLabel
      Left = 16
      Top = 30
      Width = 114
      Height = 13
      Caption = 'Chromecast devices'
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
      Width = 174
      Height = 13
      Caption = 'Media file, URL or YouTube URL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblYouTubeMode: TLabel
      Left = 320
      Top = 104
      Width = 71
      Height = 13
      Caption = 'YouTube mode'
    end
    object lblArtwork: TLabel
      Left = 321
      Top = 40
      Width = 38
      Height = 13
      Caption = 'Artwork'
    end
    object lblSeek: TLabel
      Left = 16
      Top = 147
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
      Top = 206
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
      Left = 318
      Top = 9
      Width = 61
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
      Top = 246
      Width = 698
      Height = 38
      Align = alBottom
      ExplicitLeft = 0
      ExplicitTop = 247
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
      Left = 136
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
      Left = 208
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
      Left = 196
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
      Top = 37
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
      Top = 35
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
      Top = 71
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
      Top = 166
      Width = 677
      Height = 29
      Max = 7200
      Frequency = 300
      TabOrder = 6
      OnChange = trkSeekChange
    end
    object btnSeek: TButton
      Left = 120
      Top = 142
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
      Top = 206
      Width = 571
      Height = 26
      Max = 100
      Frequency = 10
      Position = 75
      TabOrder = 8
      OnChange = trkVolumeChange
    end
    object chkMuted: TCheckBox
      Left = 67
      Top = 205
      Width = 55
      Height = 17
      Caption = 'Mute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 9
      OnClick = chkMutedClick
    end
    object cbxSubtitleLanguage: TComboBox
      Left = 425
      Top = 69
      Width = 258
      Height = 21
      Style = csDropDownList
      TabOrder = 10
      OnChange = SubtitleSelectionChanged
    end
    object cbxYouTubeMode: TComboBox
      Left = 397
      Top = 101
      Width = 106
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 13
      Text = 'Fast (360p)'
      OnChange = YouTubeModeChanged
      Items.Strings = (
        'Fast (360p)'
        'Best quality (slow load)')
    end
    object btnCast: TButton
      Left = 11
      Top = 252
      Width = 72
      Height = 25
      Caption = 'Cast'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 14
      OnClick = btnCastClick
    end
    object btnPlay: TButton
      Left = 85
      Top = 252
      Width = 72
      Height = 25
      Caption = 'Play'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 15
      OnClick = btnPlayClick
    end
    object btnPause: TButton
      Left = 157
      Top = 252
      Width = 72
      Height = 25
      Caption = 'Pause'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 16
      OnClick = btnPauseClick
    end
    object btnStop: TButton
      Left = 230
      Top = 252
      Width = 72
      Height = 25
      Caption = 'Stop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 17
      OnClick = btnStopClick
    end
    object btnDisconnect: TButton
      Left = 303
      Top = 252
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
    Top = 354
  end
end

object SimpleCastPlayerForm: TSimpleCastPlayerForm
  Left = 0
  Top = 0
  Caption = 'MfSimpleCastPlayer'
  ClientHeight = 692
  ClientWidth = 695
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
    Top = 600
    Width = 695
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
  end
  object memLog: TMemo
    Left = 0
    Top = 618
    Width = 695
    Height = 74
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitTop = 612
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 271
    Width = 695
    Height = 329
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    OnResize = PreviewResize
    ExplicitLeft = 247
    ExplicitTop = 342
    ExplicitWidth = 327
    ExplicitHeight = 182
  end
  object pnlCtrl: TPanel
    Left = 0
    Top = 0
    Width = 695
    Height = 271
    Align = alTop
    TabOrder = 2
    object Bevel2: TBevel
      Left = 312
      Top = 12
      Width = 377
      Height = 155
    end
    object lblDevices: TLabel
      Left = 16
      Top = 18
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
      Left = 321
      Top = 18
      Width = 250
      Height = 13
      Caption = 'Local media file or direct HTTP(S) media URL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSeek: TLabel
      Left = 8
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
      Left = 8
      Top = 236
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
    object Bevel3: TBevel
      Left = 313
      Top = 120
      Width = 375
      Height = 11
      Shape = bsTopLine
    end
    object Bevel1: TBevel
      Left = 4
      Top = 10
      Width = 302
      Height = 155
    end
    object lstDevices: TListBox
      Left = 16
      Top = 34
      Width = 279
      Height = 86
      ItemHeight = 13
      TabOrder = 0
      OnClick = SubtitleSelectionChanged
    end
    object btnDiscover: TButton
      Left = 16
      Top = 131
      Width = 72
      Height = 25
      Caption = 'Discover'
      TabOrder = 1
      OnClick = btnDiscoverClick
    end
    object btnRefresh: TButton
      Left = 90
      Top = 131
      Width = 72
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object edtSource: TEdit
      Left = 318
      Top = 41
      Width = 291
      Height = 21
      TabOrder = 3
      OnChange = SourceOrDeviceChanged
    end
    object btnBrowse: TButton
      Left = 611
      Top = 39
      Width = 72
      Height = 25
      Caption = 'Browse...'
      TabOrder = 4
      OnClick = btnBrowseClick
    end
    object btnCast: TButton
      Left = 318
      Top = 77
      Width = 72
      Height = 25
      Caption = 'Cast'
      TabOrder = 5
      OnClick = btnCastClick
    end
    object btnPlay: TButton
      Left = 391
      Top = 77
      Width = 72
      Height = 25
      Caption = 'Play'
      TabOrder = 6
      OnClick = btnPlayClick
    end
    object btnPause: TButton
      Left = 464
      Top = 77
      Width = 72
      Height = 25
      Caption = 'Pause'
      TabOrder = 7
      OnClick = btnPauseClick
    end
    object btnStop: TButton
      Left = 537
      Top = 77
      Width = 72
      Height = 25
      Caption = 'Stop'
      TabOrder = 8
      OnClick = btnStopClick
    end
    object btnDisconnect: TButton
      Left = 611
      Top = 77
      Width = 72
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 9
      OnClick = btnDisconnectClick
    end
    object chkEmbeddedSubtitles: TCheckBox
      Left = 321
      Top = 135
      Width = 98
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Enable subtitles'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = SourceOrDeviceChanged
    end
    object trkSeek: TTrackBar
      Left = 0
      Top = 196
      Width = 683
      Height = 29
      Max = 7200
      Frequency = 300
      TabOrder = 11
      OnChange = trkSeekChange
    end
    object btnSeek: TButton
      Left = 99
      Top = 171
      Width = 52
      Height = 25
      Caption = 'Seek'
      TabOrder = 12
      OnClick = btnSeekClick
    end
    object trkVolume: TTrackBar
      Left = 112
      Top = 235
      Width = 571
      Height = 26
      Max = 100
      Frequency = 10
      Position = 75
      TabOrder = 13
      OnChange = trkVolumeChange
    end
    object chkMuted: TCheckBox
      Left = 59
      Top = 235
      Width = 55
      Height = 17
      Caption = 'Mute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 14
      OnClick = chkMutedClick
    end
    object cbxSubtitleLanguage: TComboBox
      Left = 425
      Top = 133
      Width = 258
      Height = 21
      Style = csDropDownList
      TabOrder = 15
      OnChange = SubtitleSelectionChanged
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Cast media|*.mp4;*.m4v;*.m4a;*.webm;*.mp3;*.aac;*.mkv;*.avi|All ' +
      'files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 168
    Top = 14
  end
end

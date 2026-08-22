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
  end
  object pnlCtrl: TPanel
    Left = 0
    Top = 0
    Width = 695
    Height = 271
    Align = alTop
    TabOrder = 2
    ExplicitTop = 2
    object Bevel4: TBevel
      Left = 1
      Top = 233
      Width = 693
      Height = 37
      Align = alBottom
      ExplicitLeft = 4
      ExplicitTop = 232
      ExplicitWidth = 689
    end
    object Bevel1: TBevel
      Left = 4
      Top = 10
      Width = 302
      Height = 121
    end
    object Bevel2: TBevel
      Left = 312
      Top = 10
      Width = 377
      Height = 121
    end
    object lblDevices: TLabel
      Left = 12
      Top = 22
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
      Top = 22
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
      Top = 145
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
      Top = 204
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
      Top = 74
      Width = 375
      Height = 11
      Shape = bsTopLine
    end
    object lstDevices: TListBox
      Left = 12
      Top = 44
      Width = 279
      Height = 77
      ItemHeight = 13
      TabOrder = 0
      OnClick = SubtitleSelectionChanged
    end
    object btnDiscover: TButton
      Left = 157
      Top = 17
      Width = 60
      Height = 25
      Caption = 'Discover'
      TabOrder = 1
      OnClick = btnDiscoverClick
    end
    object btnRefresh: TButton
      Left = 231
      Top = 17
      Width = 60
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object edtSource: TEdit
      Left = 318
      Top = 44
      Width = 299
      Height = 21
      TabOrder = 3
      OnChange = SourceOrDeviceChanged
    end
    object btnBrowse: TButton
      Left = 623
      Top = 42
      Width = 60
      Height = 25
      Caption = 'Browse...'
      TabOrder = 4
      OnClick = btnBrowseClick
    end
    object btnCast: TButton
      Left = 6
      Top = 240
      Width = 72
      Height = 25
      Caption = 'Cast'
      TabOrder = 5
      OnClick = btnCastClick
    end
    object btnPlay: TButton
      Left = 79
      Top = 240
      Width = 72
      Height = 25
      Caption = 'Play'
      TabOrder = 6
      OnClick = btnPlayClick
    end
    object btnPause: TButton
      Left = 152
      Top = 240
      Width = 72
      Height = 25
      Caption = 'Pause'
      TabOrder = 7
      OnClick = btnPauseClick
    end
    object btnStop: TButton
      Left = 225
      Top = 240
      Width = 72
      Height = 25
      Caption = 'Stop'
      TabOrder = 8
      OnClick = btnStopClick
    end
    object btnDisconnect: TButton
      Left = 299
      Top = 240
      Width = 72
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 9
      OnClick = btnDisconnectClick
    end
    object chkEmbeddedSubtitles: TCheckBox
      Left = 321
      Top = 97
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
      Top = 164
      Width = 683
      Height = 29
      Max = 7200
      Frequency = 300
      TabOrder = 11
      OnChange = trkSeekChange
    end
    object btnSeek: TButton
      Left = 99
      Top = 139
      Width = 60
      Height = 25
      Caption = 'Seek'
      TabOrder = 12
      OnClick = btnSeekClick
    end
    object trkVolume: TTrackBar
      Left = 112
      Top = 203
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
      Top = 203
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
      Top = 95
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
    Left = 326
    Top = 304
  end
end

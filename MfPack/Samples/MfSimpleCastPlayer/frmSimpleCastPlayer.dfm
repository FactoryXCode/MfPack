object SimpleCastPlayerForm: TSimpleCastPlayerForm
  Left = 0
  Top = 0
  Caption = 'MfPack Simple Cast Player'
  ClientHeight = 523
  ClientWidth = 639
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
  object Bevel2: TBevel
    Left = 14
    Top = 196
    Width = 371
    Height = 87
  end
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 623
    Height = 280
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
    Left = 20
    Top = 202
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
    Top = 294
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
    Top = 345
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
  object lblState: TLabel
    Left = 0
    Top = 396
    Width = 639
    Height = 16
    Align = alBottom
    AutoSize = False
    Caption = 'State: Idle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel3: TBevel
    Left = 387
    Top = 196
    Width = 239
    Height = 87
  end
  object lstDevices: TListBox
    Left = 16
    Top = 35
    Width = 607
    Height = 121
    ItemHeight = 13
    TabOrder = 0
    OnClick = SubtitleSelectionChanged
  end
  object btnDiscover: TButton
    Left = 16
    Top = 162
    Width = 72
    Height = 25
    Caption = 'Discover'
    TabOrder = 1
    OnClick = btnDiscoverClick
  end
  object btnRefresh: TButton
    Left = 90
    Top = 162
    Width = 72
    Height = 25
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = btnRefreshClick
  end
  object edtSource: TEdit
    Left = 17
    Top = 223
    Width = 291
    Height = 21
    TabOrder = 3
    OnChange = SourceOrDeviceChanged
  end
  object btnBrowse: TButton
    Left = 310
    Top = 221
    Width = 72
    Height = 25
    Caption = 'Browse...'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object btnCast: TButton
    Left = 17
    Top = 250
    Width = 72
    Height = 25
    Caption = 'Cast'
    TabOrder = 5
    OnClick = btnCastClick
  end
  object btnPlay: TButton
    Left = 90
    Top = 250
    Width = 72
    Height = 25
    Caption = 'Play'
    TabOrder = 6
    OnClick = btnPlayClick
  end
  object btnPause: TButton
    Left = 163
    Top = 250
    Width = 72
    Height = 25
    Caption = 'Pause'
    TabOrder = 7
    OnClick = btnPauseClick
  end
  object btnStop: TButton
    Left = 236
    Top = 250
    Width = 72
    Height = 25
    Caption = 'Stop'
    TabOrder = 8
    OnClick = btnStopClick
  end
  object btnDisconnect: TButton
    Left = 310
    Top = 250
    Width = 72
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 9
    OnClick = btnDisconnectClick
  end
  object chkEmbeddedSubtitles: TCheckBox
    Left = 391
    Top = 201
    Width = 145
    Height = 17
    Caption = 'Enable subtitles'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = SourceOrDeviceChanged
  end
  object trkSeek: TTrackBar
    Left = 0
    Top = 310
    Width = 559
    Height = 29
    Max = 7200
    Frequency = 300
    TabOrder = 11
    OnChange = trkSeekChange
  end
  object btnSeek: TButton
    Left = 559
    Top = 308
    Width = 72
    Height = 25
    Caption = 'Seek'
    TabOrder = 12
    OnClick = btnSeekClick
  end
  object trkVolume: TTrackBar
    Left = 0
    Top = 364
    Width = 559
    Height = 26
    Max = 100
    Frequency = 10
    Position = 75
    TabOrder = 13
    OnChange = trkVolumeChange
  end
  object chkMuted: TCheckBox
    Left = 559
    Top = 366
    Width = 55
    Height = 17
    Caption = 'Mute'
    TabOrder = 14
    OnClick = chkMutedClick
  end
  object memLog: TMemo
    Left = 0
    Top = 412
    Width = 639
    Height = 111
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 15
  end
  object cbxSubtitleLanguage: TComboBox
    Left = 391
    Top = 224
    Width = 232
    Height = 21
    Style = csDropDownList
    TabOrder = 16
    OnChange = SubtitleSelectionChanged
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Cast media|*.mp4;*.m4v;*.m4a;*.webm;*.mp3;*.aac;*.mkv;*.avi|All ' +
      'files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 242
    Top = 18
  end
end

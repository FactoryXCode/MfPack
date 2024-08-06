object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player (Basic Sample)'
  ClientHeight = 293
  ClientWidth = 391
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 391
    Height = 234
    Align = alTop
  end
  object Bevel3: TBevel
    Left = 46
    Top = 19
    Width = 78
    Height = 208
  end
  object Bevel2: TBevel
    Left = 127
    Top = 19
    Width = 78
    Height = 208
  end
  object lblDuration: TLabel
    Left = 229
    Top = 23
    Width = 92
    Height = 13
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 229
    Top = 77
    Width = 52
    Height = 13
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 238
    Top = 50
    Width = 83
    Height = 13
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 136
    Top = 27
    Width = 17
    Height = 172
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcRight
    DeviceRole = eConsole
    Precision = 1
  end
  object pmLeft: TMfPeakMeter
    Left = 103
    Top = 27
    Width = 18
    Height = 172
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcLeft
    DeviceRole = eConsole
    Precision = 10
  end
  object Label1: TLabel
    Left = 78
    Top = 209
    Width = 22
    Height = 14
    Caption = 'Left'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 150
    Top = 209
    Width = 28
    Height = 14
    Caption = 'Right'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object butPlayPause: TButton
    Left = 11
    Top = 240
    Width = 74
    Height = 24
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 91
    Top = 240
    Width = 74
    Height = 24
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 269
    Width = 391
    Height = 24
    Panels = <>
    SimplePanel = True
    SimpleText = 'Open an audio file'
    ExplicitTop = 267
  end
  object trbVolumeR: TTrackBar
    Left = 159
    Top = 23
    Width = 39
    Height = 180
    DoubleBuffered = True
    Max = 0
    Min = -224
    Orientation = trVertical
    ParentDoubleBuffered = False
    Frequency = 10
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = trbVolumeRChange
  end
  object trbVolumeL: TTrackBar
    Left = 53
    Top = 23
    Width = 39
    Height = 180
    DoubleBuffered = True
    Max = 0
    Min = -224
    Orientation = trVertical
    ParentDoubleBuffered = False
    Frequency = 10
    TabOrder = 4
    TickMarks = tmBoth
    OnChange = trbVolumeLChange
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 78
    Top = 2
    Width = 153
    Height = 15
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object butReplay: TButton
    Left = 171
    Top = 240
    Width = 74
    Height = 24
    Caption = 'Replay'
    Enabled = False
    TabOrder = 6
    OnClick = butReplayClick
  end
  object mnuMain: TMainMenu
    Left = 231
    Top = 131
    object OpenAudioFile1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'WAV|*.wav|MPEG Audio Layer-3  (MP3)|*.mp3|Free Losless Audio Cod' +
      'er (FLAC)|*.flac|All files|*.*'
    Left = 282
    Top = 130
  end
end

object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WasApi Player Sample 1'
  ClientHeight = 338
  ClientWidth = 469
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  OldCreateOrder = True
  Position = poScreenCenter
  WindowMenu = Open1
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 469
    Height = 286
    Align = alTop
  end
  object Bevel3: TBevel
    Left = 7
    Top = 25
    Width = 70
    Height = 210
  end
  object Bevel2: TBevel
    Left = 83
    Top = 25
    Width = 70
    Height = 210
  end
  object lblDuration: TLabel
    Left = 8
    Top = 247
    Width = 101
    Height = 16
    AutoSize = False
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 115
    Top = 261
    Width = 139
    Height = 16
    AutoSize = False
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 19
    Top = 261
    Width = 98
    Height = 16
    AutoSize = False
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 88
    Top = 52
    Width = 15
    Height = 155
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcRight
    DeviceRole = eConsole
    Precision = 1
  end
  object pmLeft: TMfPeakMeter
    Left = 56
    Top = 52
    Width = 16
    Height = 155
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcLeft
    DeviceRole = eConsole
    Precision = 10
  end
  object Label1: TLabel
    Left = 34
    Top = 217
    Width = 19
    Height = 13
    Caption = 'Left'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 103
    Top = 217
    Width = 25
    Height = 13
    Caption = 'Right'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblLeftVolume: TLabel
    Left = 11
    Top = 32
    Width = 61
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRightVolume: TLabel
    Left = 88
    Top = 32
    Width = 60
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 0
    Top = 322
    Width = 469
    Height = 16
    Align = alBottom
    AutoSize = False
    Caption = 'Open an audio file'
  end
  object MfAudioVisualizer1: TMfAudioVisualizer
    Left = 162
    Top = 25
    Width = 298
    Height = 210
    PeakThreshold = 0.500000000000000000
    PeakCapFrac = 0.119999997317791000
    AutoStart = True
    BarCount = 64
    ReleaseMs = 60
  end
  object butPlay: TButton
    Left = 33
    Top = 292
    Width = 67
    Height = 24
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayClick
  end
  object butStop: TButton
    Left = 179
    Top = 292
    Width = 67
    Height = 24
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 19
    Top = 4
    Width = 134
    Height = 16
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object butPause: TButton
    Left = 106
    Top = 292
    Width = 67
    Height = 24
    Caption = 'Pause'
    Enabled = False
    TabOrder = 5
    OnClick = butPauseClick
  end
  object trbVolumeL: TTrackBar
    Left = 11
    Top = 50
    Width = 36
    Height = 162
    Hint = 'Press and release SHIFT + ESC to set zero position.'
    DoubleBuffered = True
    Max = 0
    Min = -100
    Orientation = trVertical
    ParentDoubleBuffered = False
    ParentShowHint = False
    Frequency = 10
    Position = -30
    ShowHint = True
    TabOrder = 3
    ThumbLength = 30
    TickMarks = tmBoth
    OnChange = trbVolumeLChange
  end
  object trbVolumeR: TTrackBar
    Left = 109
    Top = 50
    Width = 31
    Height = 162
    Hint = 'Press and release SHIFT + ESC to set zero position.'
    DoubleBuffered = True
    Max = 0
    Min = -100
    Orientation = trVertical
    ParentDoubleBuffered = False
    ParentShowHint = False
    Frequency = 10
    Position = -30
    ShowHint = True
    TabOrder = 2
    ThumbLength = 30
    TickMarks = tmBoth
    OnChange = trbVolumeRChange
  end
  object mnuMain: TMainMenu
    Left = 578
    Top = 450
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
    DefaultExt = '*.*'
    Filter = 
      'WAV|*.wav|MPEG Audio Layer-3  (MP3)|*.mp3|Free Losless Audio Cod' +
      'er (FLAC)|*.flac|All files|*.*'
    Title = 'Open an Audio File'
    Left = 474
    Top = 449
  end
end

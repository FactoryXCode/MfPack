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
  ClientHeight = 563
  ClientWidth = 781
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  Position = poScreenCenter
  WindowMenu = Open1
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 144
  TextHeight = 25
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 781
    Height = 477
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    ExplicitWidth = 474
  end
  object Bevel3: TBevel
    Left = 12
    Top = 42
    Width = 117
    Height = 350
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object Bevel2: TBevel
    Left = 138
    Top = 42
    Width = 117
    Height = 350
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object lblDuration: TLabel
    Left = 18
    Top = 411
    Width = 156
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 197
    Top = 435
    Width = 231
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 33
    Top = 435
    Width = 135
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 147
    Top = 87
    Width = 25
    Height = 258
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcRight
    DeviceRole = eConsole
    Precision = 1
  end
  object pmLeft: TMfPeakMeter
    Left = 93
    Top = 87
    Width = 27
    Height = 258
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcLeft
    DeviceRole = eConsole
    Precision = 10
  end
  object Label1: TLabel
    Left = 57
    Top = 362
    Width = 30
    Height = 22
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Left'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 172
    Top = 362
    Width = 41
    Height = 22
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Right'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblLeftVolume: TLabel
    Left = 19
    Top = 54
    Width = 101
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRightVolume: TLabel
    Left = 147
    Top = 54
    Width = 99
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 0
    Top = 536
    Width = 781
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    AutoSize = False
    Caption = 'Open an audio file'
    ExplicitWidth = 474
  end
  object MfAudioVisualizer1: TMfAudioVisualizer
    Left = 270
    Top = 42
    Width = 496
    Height = 350
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BarColor = clTurquoise
    PeakColor = clDarkorange
    PeakThreshold = 0.500000000000000000
    PeakCapFrac = 0.119999997317791000
    AutoStart = True
    BarCount = 64
    ReleaseMs = 60
  end
  object butPlay: TButton
    Left = 65
    Top = 486
    Width = 111
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayClick
  end
  object butStop: TButton
    Left = 299
    Top = 486
    Width = 111
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 31
    Top = 6
    Width = 224
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object butPause: TButton
    Left = 182
    Top = 486
    Width = 111
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Pause'
    Enabled = False
    TabOrder = 5
    OnClick = butPauseClick
  end
  object trbVolumeL: TTrackBar
    Left = 19
    Top = 83
    Width = 59
    Height = 270
    Hint = 'Press and release SHIFT + ESC to set zero position.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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
    Left = 181
    Top = 83
    Width = 53
    Height = 270
    Hint = 'Press and release SHIFT + ESC to set zero position.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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

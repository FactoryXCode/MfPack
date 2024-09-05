object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WasApi Player Sample 1'
  ClientHeight = 375
  ClientWidth = 316
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  Position = poScreenCenter
  WindowMenu = Open1
  OnCloseQuery = FormCloseQuery
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 316
    Height = 318
    Align = alTop
    ExplicitWidth = 317
  end
  object Bevel3: TBevel
    Left = 77
    Top = 28
    Width = 78
    Height = 233
  end
  object Bevel2: TBevel
    Left = 161
    Top = 28
    Width = 78
    Height = 233
  end
  object lblDuration: TLabel
    Left = 12
    Top = 274
    Width = 104
    Height = 18
    AutoSize = False
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 131
    Top = 290
    Width = 154
    Height = 18
    AutoSize = False
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 12
    Top = 290
    Width = 90
    Height = 18
    AutoSize = False
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 167
    Top = 58
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
    Left = 131
    Top = 58
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
    Left = 107
    Top = 241
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
    Left = 184
    Top = 241
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
  object lblLeftVolume: TLabel
    Left = 82
    Top = 36
    Width = 67
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRightVolume: TLabel
    Left = 167
    Top = 36
    Width = 66
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 0
    Top = 357
    Width = 316
    Height = 18
    Align = alBottom
    AutoSize = False
    Caption = 'Open an audio file'
    ExplicitLeft = 3
    ExplicitTop = 364
    ExplicitWidth = 311
  end
  object butPlay: TButton
    Left = 43
    Top = 324
    Width = 74
    Height = 24
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayClick
  end
  object butStop: TButton
    Left = 199
    Top = 324
    Width = 74
    Height = 24
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 90
    Top = 4
    Width = 149
    Height = 18
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object butPause: TButton
    Left = 121
    Top = 324
    Width = 74
    Height = 24
    Caption = 'Pause'
    Enabled = False
    TabOrder = 5
    OnClick = butPauseClick
  end
  object trbVolumeL: TTrackBar
    Left = 82
    Top = 55
    Width = 39
    Height = 180
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
    TickMarks = tmBoth
    OnChange = trbVolumeLChange
  end
  object trbVolumeR: TTrackBar
    Left = 190
    Top = 55
    Width = 35
    Height = 180
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
    TickMarks = tmBoth
    OnChange = trbVolumeRChange
  end
  object mnuMain: TMainMenu
    Left = 263
    Top = 146
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
    Left = 264
    Top = 203
  end
end

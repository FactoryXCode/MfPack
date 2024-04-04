object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player  Sample 3'
  ClientHeight = 388
  ClientWidth = 419
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel6: TBevel
    Left = 238
    Top = 30
    Width = 170
    Height = 233
  end
  object Bevel5: TBevel
    Left = 0
    Top = 344
    Width = 419
    Height = 20
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 354
    ExplicitWidth = 324
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 419
    Height = 309
    Align = alTop
  end
  object Bevel4: TBevel
    Left = 177
    Top = 30
    Width = 55
    Height = 233
  end
  object Bevel3: TBevel
    Left = 9
    Top = 30
    Width = 78
    Height = 233
  end
  object Bevel2: TBevel
    Left = 93
    Top = 30
    Width = 78
    Height = 233
  end
  object lblDuration: TLabel
    Left = 11
    Top = 270
    Width = 92
    Height = 13
    AutoSize = False
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 122
    Top = 289
    Width = 154
    Height = 13
    AutoSize = False
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 20
    Top = 289
    Width = 83
    Height = 13
    AutoSize = False
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 99
    Top = 60
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
    Left = 63
    Top = 60
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
    Left = 39
    Top = 243
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
    Left = 116
    Top = 243
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
  object Label3: TLabel
    Left = 190
    Top = 243
    Width = 27
    Height = 14
    Caption = 'Pitch'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblLeftVolume: TLabel
    Left = 14
    Top = 38
    Width = 67
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRightVolume: TLabel
    Left = 99
    Top = 38
    Width = 66
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPitch: TLabel
    Left = 180
    Top = 38
    Width = 49
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 6
    Top = 347
    Width = 404
    Height = 13
    AutoSize = False
    Caption = 'Status: Stopped'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 300
    Top = 243
    Width = 38
    Height = 14
    Caption = 'Effects'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Bevel7: TBevel
    Left = 9
    Top = 9
    Width = 162
    Height = 23
  end
  object butPlay: TButton
    Left = 8
    Top = 315
    Width = 74
    Height = 24
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayClick
  end
  object butStop: TButton
    Left = 166
    Top = 315
    Width = 74
    Height = 24
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 364
    Width = 419
    Height = 24
    Panels = <>
    ParentShowHint = False
    ShowHint = True
    SimplePanel = True
    SimpleText = 'Open an audio file'
    OnMouseMove = StatusBarMouseMove
    ExplicitTop = 366
  end
  object trbVolumeR: TTrackBar
    Left = 122
    Top = 57
    Width = 35
    Height = 180
    Hint = 'Press and release SHIFT + ESC to set zero position.'
    DoubleBuffered = True
    Max = 100
    Orientation = trVertical
    ParentDoubleBuffered = False
    ParentShowHint = False
    Frequency = 10
    Position = 70
    ShowHint = True
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = trbVolumeRChange
  end
  object trbVolumeL: TTrackBar
    Left = 14
    Top = 57
    Width = 39
    Height = 180
    Hint = 'Press and release SHIFT + ESC to set zero position.'
    DoubleBuffered = True
    Max = 100
    Orientation = trVertical
    ParentDoubleBuffered = False
    ParentShowHint = False
    Frequency = 10
    Position = 70
    ShowHint = True
    TabOrder = 4
    TickMarks = tmBoth
    OnChange = trbVolumeLChange
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 33
    Top = 12
    Width = 115
    Height = 15
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object butReplay: TButton
    Left = 241
    Top = 317
    Width = 74
    Height = 24
    Caption = 'Replay'
    Enabled = False
    TabOrder = 6
    OnClick = butReplayClick
  end
  object butPause: TButton
    Left = 86
    Top = 315
    Width = 74
    Height = 24
    Caption = 'Pause'
    Enabled = False
    TabOrder = 7
    OnClick = butPauseClick
  end
  object trbPitch: TTrackBar
    Left = 185
    Top = 57
    Width = 38
    Height = 180
    DoubleBuffered = True
    Max = 50
    Min = -50
    Orientation = trVertical
    ParentDoubleBuffered = False
    TabOrder = 8
    TickMarks = tmBoth
    OnChange = trbPitchChange
  end
  object cbxReverbMain: TCheckBox
    Left = 241
    Top = 39
    Width = 165
    Height = 16
    Caption = 'Set Reverb on Main Voice'
    TabOrder = 9
    OnClick = cbxReverbMainClick
  end
  object cbxReverbSource: TCheckBox
    Left = 241
    Top = 61
    Width = 165
    Height = 16
    Caption = 'Set Reverb on Source Voice'
    TabOrder = 10
    OnClick = cbxReverbSourceClick
  end
  object mnuMain: TMainMenu
    Left = 437
    Top = 75
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
    Left = 436
    Top = 27
  end
end

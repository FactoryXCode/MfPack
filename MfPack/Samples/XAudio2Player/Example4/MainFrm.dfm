object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player  Sample 4'
  ClientHeight = 438
  ClientWidth = 390
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object lblDuration: TLabel
    Left = 8
    Top = 275
    Width = 92
    Height = 13
    AutoSize = False
    Caption = 'Duration: 00:00:00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblProcessed: TLabel
    Left = 10
    Top = 311
    Width = 117
    Height = 12
    AutoSize = False
    Caption = 'Samples: 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPlayed: TLabel
    Left = 18
    Top = 293
    Width = 82
    Height = 14
    AutoSize = False
    Caption = 'Played: 00:00:00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblBarPositionInSamples: TLabel
    Left = 182
    Top = 292
    Width = 153
    Height = 13
    AutoSize = False
    Caption = 'Position: 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblBarPositionInSTime: TLabel
    Left = 182
    Top = 311
    Width = 96
    Height = 12
    AutoSize = False
    Caption = 'Position: 00:00:00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object butPlayPause: TButton
    Left = 7
    Top = 356
    Width = 73
    Height = 24
    Caption = 'Play'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 86
    Top = 356
    Width = 73
    Height = 24
    Caption = 'Stop'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 407
    Width = 390
    Height = 31
    Panels = <>
    ParentShowHint = False
    ShowHint = True
    SimplePanel = True
    SimpleText = 'Open an audio file'
    OnMouseMove = StatusBarMouseMove
  end
  object butReplay: TButton
    Left = 165
    Top = 356
    Width = 74
    Height = 24
    Caption = 'Replay'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = butReplayClick
  end
  object pbProgress: TProgressBar
    Left = 7
    Top = 330
    Width = 377
    Height = 14
    Enabled = False
    ParentShowHint = False
    Smooth = True
    BarColor = clHighlight
    ShowHint = True
    TabOrder = 4
    OnMouseMove = pbProgressMouseMove
    OnMouseUp = pbProgressMouseUp
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 390
    Height = 266
    Align = alTop
    Enabled = False
    TabOrder = 5
    object Bevel5: TBevel
      Left = 198
      Top = 30
      Width = 183
      Height = 102
    end
    object Bevel1: TBevel
      Left = 198
      Top = 136
      Width = 183
      Height = 95
    end
    object Bevel4: TBevel
      Left = 136
      Top = 30
      Width = 59
      Height = 230
    end
    object Bevel3: TBevel
      Left = 8
      Top = 30
      Width = 59
      Height = 230
    end
    object Bevel2: TBevel
      Left = 72
      Top = 30
      Width = 60
      Height = 230
    end
    object pmRight: TMfPeakMeter
      Left = 81
      Top = 57
      Width = 8
      Height = 170
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      DeviceRole = eConsole
      Precision = 1
    end
    object pmLeft: TMfPeakMeter
      Left = 51
      Top = 57
      Width = 8
      Height = 170
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      DeviceRole = eConsole
      Precision = 10
    end
    object Label1: TLabel
      Left = 29
      Top = 240
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
      Left = 89
      Top = 240
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
    object Label3: TLabel
      Left = 149
      Top = 240
      Width = 23
      Height = 13
      Caption = 'Pitch'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblLeftVolume: TLabel
      Left = 10
      Top = 38
      Width = 58
      Height = 12
      Alignment = taCenter
      AutoSize = False
      Caption = '50%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblRightVolume: TLabel
      Left = 73
      Top = 38
      Width = 60
      Height = 12
      Alignment = taCenter
      AutoSize = False
      Caption = '50%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblPitch: TLabel
      Left = 141
      Top = 38
      Width = 49
      Height = 12
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 273
      Top = 240
      Width = 34
      Height = 13
      Caption = 'Effects'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Bevel7: TBevel
      Left = 8
      Top = 9
      Width = 125
      Height = 23
    end
    object Label7: TLabel
      Left = 231
      Top = 173
      Width = 61
      Height = 13
      Hint = 'Tuning factor with no specific units (default = 6).'
      Alignment = taRightJustify
      Caption = 'Release time'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label8: TLabel
      Left = 245
      Top = 201
      Width = 47
      Height = 13
      Hint = 'Loudness target (default = 1000).'
      Alignment = taRightJustify
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object Label9: TLabel
      Left = 208
      Top = 39
      Width = 121
      Height = 13
      Caption = 'Reverb on Main Voice'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 208
      Top = 82
      Width = 133
      Height = 13
      Caption = 'Reverb on Source Voice'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object trbVolumeR: TTrackBar
      Left = 89
      Top = 56
      Width = 35
      Height = 178
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      DoubleBuffered = True
      Max = 0
      Min = -224
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 10
      Position = -112
      ShowHint = True
      TabOrder = 0
      TickMarks = tmBoth
      OnChange = trbVolumeRChange
    end
    object trbVolumeL: TTrackBar
      Left = 10
      Top = 56
      Width = 35
      Height = 178
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      DoubleBuffered = True
      Max = 0
      Min = -224
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 10
      Position = -112
      ShowHint = True
      TabOrder = 1
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 13
      Top = 13
      Width = 114
      Height = 15
      Caption = 'Lock Volume Sliders'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 2
    end
    object trbPitch: TTrackBar
      Left = 145
      Top = 56
      Width = 42
      Height = 178
      DoubleBuffered = True
      Max = 50
      Min = -50
      Orientation = trVertical
      ParentDoubleBuffered = False
      TabOrder = 3
      TickMarks = tmBoth
      OnChange = trbPitchChange
    end
    object ckbReverbMain: TComboBox
      Left = 208
      Top = 58
      Width = 127
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 4
      Text = 'None'
      OnCloseUp = ckbReverbMainCloseUp
      Items.Strings = (
        'None')
    end
    object ckbReverbSource: TComboBox
      Left = 208
      Top = 101
      Width = 127
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 5
      Text = 'None'
      OnCloseUp = ckbReverbSourceCloseUp
      Items.Strings = (
        'None')
    end
    object CheckBox1: TCheckBox
      Left = 208
      Top = 149
      Width = 180
      Height = 15
      Caption = 'Master limiter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = CheckBox1Click
    end
    object spedLimiterReleaseTime: TSpinEdit
      Left = 298
      Top = 170
      Width = 48
      Height = 22
      Hint = 'Tuning factor with no specific units (default = 6).'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 20
      MinValue = 1
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Value = 6
    end
    object spedLimiterThreshold: TSpinEdit
      Left = 298
      Top = 198
      Width = 48
      Height = 22
      Hint = 'Loudness target (default = 1000).'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 1800
      MinValue = 1
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Value = 1000
    end
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 390
    Width = 390
    Height = 17
    Align = alBottom
    AutoSize = False
    BorderStyle = sbsSunken
    Caption = 'Status: Stopped'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Transparent = False
  end
  object mnuMain: TMainMenu
    Left = 332
    Top = 348
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
    FilterIndex = 0
    Left = 281
    Top = 350
  end
end

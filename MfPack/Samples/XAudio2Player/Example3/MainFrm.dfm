object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player  Sample 3'
  ClientHeight = 434
  ClientWidth = 394
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
  object lblDuration: TLabel
    Left = 8
    Top = 278
    Width = 93
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
    Top = 314
    Width = 118
    Height = 13
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
    Top = 296
    Width = 83
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
    Left = 184
    Top = 295
    Width = 154
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
    Left = 184
    Top = 314
    Width = 97
    Height = 13
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
    Top = 360
    Width = 74
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
    Left = 87
    Top = 360
    Width = 74
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
    Top = 410
    Width = 394
    Height = 24
    Panels = <>
    ParentShowHint = False
    ShowHint = True
    SimplePanel = True
    SimpleText = 'Open an audio file'
    OnMouseMove = StatusBarMouseMove
  end
  object butReplay: TButton
    Left = 167
    Top = 360
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
    Top = 333
    Width = 381
    Height = 15
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
    Width = 394
    Height = 269
    Align = alTop
    Enabled = False
    TabOrder = 5
    object Bevel4: TBevel
      Left = 137
      Top = 30
      Width = 60
      Height = 233
    end
    object Bevel3: TBevel
      Left = 8
      Top = 30
      Width = 60
      Height = 233
    end
    object Bevel2: TBevel
      Left = 73
      Top = 30
      Width = 60
      Height = 233
    end
    object pmRight: TMfPeakMeter
      Left = 82
      Top = 57
      Width = 8
      Height = 172
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      DeviceRole = eConsole
      Precision = 1
    end
    object pmLeft: TMfPeakMeter
      Left = 52
      Top = 57
      Width = 8
      Height = 172
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      DeviceRole = eConsole
      Precision = 10
    end
    object Label1: TLabel
      Left = 29
      Top = 243
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
      Left = 90
      Top = 243
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
      Left = 150
      Top = 243
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
      Width = 59
      Height = 13
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
    object lblRightVolume: TLabel
      Left = 74
      Top = 38
      Width = 60
      Height = 13
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
    object lblPitch: TLabel
      Left = 140
      Top = 38
      Width = 49
      Height = 13
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
      Left = 276
      Top = 243
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
      Width = 126
      Height = 23
    end
    object Label5: TLabel
      Left = 217
      Top = 60
      Width = 71
      Height = 13
      Alignment = taRightJustify
      Caption = 'Effect Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 217
      Top = 109
      Width = 71
      Height = 13
      Alignment = taRightJustify
      Caption = 'Effect Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 227
      Top = 170
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
      Left = 241
      Top = 198
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
    object trbVolumeR: TTrackBar
      Left = 93
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
      TabOrder = 0
      TickMarks = tmBoth
      OnChange = trbVolumeRChange
    end
    object trbVolumeL: TTrackBar
      Left = 10
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
      TabOrder = 1
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 13
      Top = 13
      Width = 115
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
      Top = 57
      Width = 38
      Height = 180
      DoubleBuffered = True
      Max = 50
      Min = -50
      Orientation = trVertical
      ParentDoubleBuffered = False
      TabOrder = 3
      TickMarks = tmBoth
      OnChange = trbPitchChange
    end
    object cbxReverbMain: TCheckBox
      Left = 203
      Top = 38
      Width = 182
      Height = 16
      Caption = 'Set Reverb on Main Voice'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = cbxReverbMainClick
    end
    object cbxReverbSource: TCheckBox
      Left = 203
      Top = 87
      Width = 182
      Height = 16
      Caption = 'Set Reverb on Source Voice'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = cbxReverbSourceClick
    end
    object ckbReverbMain: TComboBox
      Left = 294
      Top = 57
      Width = 67
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 6
      Text = 'Default'
      Items.Strings = (
        'Default'
        'Minimum'
        'Maximum'
        'Manual')
    end
    object ckbReverbSource: TComboBox
      Left = 294
      Top = 106
      Width = 67
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 7
      Text = 'Default'
      Items.Strings = (
        'Default'
        'Minimum'
        'Maximum'
        'Manual')
    end
    object CheckBox1: TCheckBox
      Left = 203
      Top = 145
      Width = 182
      Height = 16
      Caption = 'Master limiter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnClick = CheckBox1Click
    end
    object spedLimiterReleaseTime: TSpinEdit
      Left = 294
      Top = 167
      Width = 49
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
      TabOrder = 9
      Value = 6
    end
    object spedLimiterThreshold: TSpinEdit
      Left = 294
      Top = 195
      Width = 49
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
      TabOrder = 10
      Value = 1000
    end
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 393
    Width = 394
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
    Left = 355
    Top = 365
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
    Left = 309
    Top = 364
  end
end

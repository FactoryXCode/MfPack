object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player  Sample 3'
  ClientHeight = 428
  ClientWidth = 386
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
    Left = 7
    Top = 271
    Width = 102
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
    Left = 9
    Top = 309
    Width = 124
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
    Left = 17
    Top = 290
    Width = 92
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
    Left = 180
    Top = 288
    Width = 151
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
    Left = 180
    Top = 307
    Width = 95
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
    Top = 352
    Width = 72
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
    Left = 85
    Top = 352
    Width = 72
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
    Top = 404
    Width = 386
    Height = 24
    Color = clWindow
    Panels = <>
    ParentFont = True
    ParentShowHint = False
    ShowHint = True
    SimplePanel = True
    SimpleText = 'Open an audio file'
    SizeGrip = False
    UseSystemFont = False
    OnMouseMove = StatusBarMouseMove
  end
  object butReplay: TButton
    Left = 163
    Top = 352
    Width = 73
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
    Left = 5
    Top = 327
    Width = 373
    Height = 13
    DoubleBuffered = False
    Enabled = False
    ParentDoubleBuffered = False
    ParentShowHint = False
    MarqueeInterval = 100
    BarColor = clHighlight
    ShowHint = True
    TabOrder = 4
    OnMouseMove = pbProgressMouseMove
    OnMouseUp = pbProgressMouseUp
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 386
    Height = 263
    Align = alTop
    Enabled = False
    TabOrder = 5
    object Bevel5: TBevel
      Left = 196
      Top = 30
      Width = 181
      Height = 101
    end
    object Bevel1: TBevel
      Left = 196
      Top = 135
      Width = 181
      Height = 93
    end
    object Bevel4: TBevel
      Left = 135
      Top = 30
      Width = 58
      Height = 227
    end
    object Bevel3: TBevel
      Left = 8
      Top = 30
      Width = 58
      Height = 227
    end
    object Bevel2: TBevel
      Left = 71
      Top = 30
      Width = 60
      Height = 227
    end
    object pmRight: TMfPeakMeter
      Left = 80
      Top = 57
      Width = 8
      Height = 167
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      DeviceDataFlow = EDataFlow_enum_count
      DeviceRole = eConsole
      Precision = 25
    end
    object pmLeft: TMfPeakMeter
      Left = 52
      Top = 56
      Width = 8
      Height = 167
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      DeviceDataFlow = EDataFlow_enum_count
      DeviceRole = eConsole
      Precision = 25
    end
    object Label1: TLabel
      Left = 29
      Top = 237
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
      Left = 88
      Top = 237
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
      Left = 148
      Top = 237
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
      Width = 57
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
    object lblRightVolume: TLabel
      Left = 72
      Top = 38
      Width = 60
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
    object lblPitch: TLabel
      Left = 140
      Top = 38
      Width = 48
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
      Left = 270
      Top = 237
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
      Width = 124
      Height = 23
    end
    object Label7: TLabel
      Left = 228
      Top = 171
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
      Left = 242
      Top = 199
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
      Left = 206
      Top = 38
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
      Left = 206
      Top = 81
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
      Left = 90
      Top = 55
      Width = 35
      Height = 177
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      DoubleBuffered = True
      Max = 0
      Min = -224
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 10
      Position = -30
      ShowHint = True
      TabOrder = 0
      TickMarks = tmBoth
      OnChange = trbVolumeRChange
    end
    object trbVolumeL: TTrackBar
      Left = 11
      Top = 55
      Width = 35
      Height = 177
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      DoubleBuffered = True
      Max = 0
      Min = -224
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 10
      Position = -30
      ShowHint = True
      TabOrder = 1
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 13
      Top = 13
      Width = 112
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
      Left = 144
      Top = 55
      Width = 41
      Height = 177
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
      Left = 206
      Top = 57
      Width = 125
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
      Left = 206
      Top = 100
      Width = 125
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
      Left = 206
      Top = 148
      Width = 137
      Height = 14
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
      Left = 295
      Top = 168
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
      Left = 295
      Top = 196
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
    Top = 387
    Width = 386
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
    Left = 328
    Top = 358
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
    Left = 268
    Top = 358
  end
end

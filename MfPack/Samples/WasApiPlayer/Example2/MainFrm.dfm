object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WasApi Player Sample 2'
  ClientHeight = 377
  ClientWidth = 302
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
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 15
  object butPlayPause: TButton
    Left = 8
    Top = 329
    Width = 66
    Height = 26
    Caption = 'Play'
    TabOrder = 0
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 77
    Top = 329
    Width = 67
    Height = 26
    Caption = 'Stop'
    TabOrder = 1
    OnClick = butStopClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 302
    Height = 286
    Align = alTop
    TabOrder = 2
    ExplicitLeft = -1
    object Bevel1: TBevel
      Left = 144
      Top = 25
      Width = 151
      Height = 210
    end
    object Bevel3: TBevel
      Left = 13
      Top = 25
      Width = 61
      Height = 210
    end
    object pmLeft: TMfPeakMeter
      Left = 52
      Top = 54
      Width = 15
      Height = 152
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      DeviceRole = eConsole
      Precision = 10
    end
    object Bevel2: TBevel
      Left = 77
      Top = 25
      Width = 62
      Height = 210
    end
    object lblDuration: TLabel
      Left = 9
      Top = 247
      Width = 104
      Height = 17
      AutoSize = False
      Caption = 'Duration: 00:00:00'
    end
    object lblProcessed: TLabel
      Left = 125
      Top = 265
      Width = 138
      Height = 16
      AutoSize = False
      Caption = 'Samples: 0'
    end
    object lblPlayed: TLabel
      Left = 20
      Top = 265
      Width = 93
      Height = 16
      AutoSize = False
      Caption = 'Played: 00:00:00'
    end
    object pmRight: TMfPeakMeter
      Left = 85
      Top = 54
      Width = 15
      Height = 152
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      DeviceRole = eConsole
      Precision = 1
    end
    object Label1: TLabel
      Left = 36
      Top = 217
      Width = 19
      Height = 16
      Alignment = taCenter
      Caption = 'Left'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 92
      Top = 217
      Width = 25
      Height = 16
      Alignment = taCenter
      Caption = 'Right'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblLeftVolume: TLabel
      Left = 15
      Top = 29
      Width = 56
      Height = 16
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
      Left = 79
      Top = 29
      Width = 57
      Height = 16
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
    object lblBass: TLabel
      Left = 151
      Top = 217
      Width = 22
      Height = 16
      Alignment = taCenter
      Caption = 'Bass'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 189
      Top = 217
      Width = 30
      Height = 16
      Alignment = taCenter
      Caption = 'Treble'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 151
      Top = 29
      Width = 70
      Height = 16
      Caption = 'Attack/release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 151
      Top = 71
      Width = 36
      Height = 13
      Alignment = taRightJustify
      Caption = 'Custom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 20
      Top = 4
      Width = 134
      Height = 16
      Caption = 'Lock Volume Sliders'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object trbVolumeL: TTrackBar
      Left = 15
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
      TabOrder = 1
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object trbVolumeR: TTrackBar
      Left = 103
      Top = 50
      Width = 30
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
    object tbBass: TTrackBar
      Left = 146
      Top = 126
      Width = 32
      Height = 86
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      DoubleBuffered = True
      Max = 24
      Min = -24
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 4
      ShowHint = True
      TabOrder = 3
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = tbBassChange
    end
    object tbTreble: TTrackBar
      Left = 189
      Top = 126
      Width = 32
      Height = 86
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      DoubleBuffered = True
      Max = 24
      Min = -24
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 4
      ShowHint = True
      TabOrder = 4
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = tbTrebleChange
    end
    object cbxSetRamp: TComboBox
      Left = 151
      Top = 45
      Width = 90
      Height = 23
      TabOrder = 5
      Text = 'Smooth'
      OnChange = cbxSetRampChange
      Items.Strings = (
        'Off'
        'Fast'
        'Smooth'
        'Manual')
    end
    object chkResetEQOnNewFile: TCheckBox
      Left = 151
      Top = 92
      Width = 134
      Height = 15
      Caption = 'Reset EQ on new file'
      TabOrder = 6
    end
    object edtRampMs: TEdit
      Left = 193
      Top = 68
      Width = 48
      Height = 23
      Enabled = False
      NumbersOnly = True
      TabOrder = 7
      Text = '30'
    end
    object cbEnableEq: TCheckBox
      Left = 151
      Top = 113
      Width = 106
      Height = 17
      Hint = 'Enables or by-passes the MFT'
      Caption = 'Enable MFT'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 8
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 286
    Width = 302
    Height = 37
    Align = alTop
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 3
    ExplicitWidth = 284
    object lblBarPositionInSTime: TLabel
      Left = 9
      Top = 18
      Width = 86
      Height = 16
      AutoSize = False
      Caption = 'Time: 00:00:00'
    end
    object lblBarPositionInSamples: TLabel
      Left = 111
      Top = 18
      Width = 104
      Height = 16
      AutoSize = False
      Caption = 'Position: 0'
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 300
      Height = 15
      Align = alTop
      TabOrder = 0
      OnMouseMove = pbProgressMouseMove
      OnMouseUp = pbProgressMouseUp
      ExplicitWidth = 282
    end
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 359
    Width = 302
    Height = 18
    Align = alBottom
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = 'Open an audio file'
    Color = clCream
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 4
    Transparent = False
    ExplicitWidth = 284
  end
  object mnuMain: TMainMenu
    Left = 320
    Top = 549
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
    Left = 414
    Top = 548
  end
end

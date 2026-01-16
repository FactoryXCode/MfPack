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
  ClientHeight = 628
  ClientWidth = 474
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
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 144
  TextHeight = 25
  object butPlayPause: TButton
    Left = 13
    Top = 551
    Width = 111
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Play'
    TabOrder = 0
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 129
    Top = 551
    Width = 111
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stop'
    TabOrder = 1
    OnClick = butStopClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 474
    Height = 477
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 2
    object Bevel1: TBevel
      Left = 240
      Top = 42
      Width = 227
      Height = 350
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object Bevel3: TBevel
      Left = 21
      Top = 42
      Width = 103
      Height = 350
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object pmLeft: TMfPeakMeter
      Left = 86
      Top = 90
      Width = 27
      Height = 253
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
    object Bevel2: TBevel
      Left = 129
      Top = 42
      Width = 103
      Height = 350
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object lblDuration: TLabel
      Left = 18
      Top = 409
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
      Left = 208
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
      Left = 34
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
      Left = 145
      Top = 90
      Width = 25
      Height = 253
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
    object Label1: TLabel
      Left = 60
      Top = 362
      Width = 30
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'Left'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 154
      Top = 362
      Width = 41
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'Right'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblLeftVolume: TLabel
      Left = 25
      Top = 54
      Width = 94
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
      Left = 132
      Top = 54
      Width = 94
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
    object lblBass: TLabel
      Left = 252
      Top = 363
      Width = 36
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'Bass'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 315
      Top = 363
      Width = 49
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'Treble'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 252
      Top = 53
      Width = 110
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Attack/release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 252
      Top = 115
      Width = 60
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Caption = 'Custom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 40
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
      TabOrder = 0
    end
    object trbVolumeL: TTrackBar
      Left = 28
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
      TabOrder = 1
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object trbVolumeR: TTrackBar
      Left = 173
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
    object tbBass: TTrackBar
      Left = 244
      Top = 210
      Width = 53
      Height = 143
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Left = 315
      Top = 210
      Width = 40
      Height = 143
      Hint = 'Press and release SHIFT + ESC to set zero position.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Left = 252
      Top = 75
      Width = 150
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Left = 252
      Top = 153
      Width = 199
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Reset EQ on new file'
      TabOrder = 6
    end
    object edtRampMs: TEdit
      Left = 322
      Top = 110
      Width = 80
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Enabled = False
      NumbersOnly = True
      TabOrder = 7
      Text = '30'
    end
    object cbEnableEq: TCheckBox
      Left = 252
      Top = 183
      Width = 176
      Height = 29
      Hint = 'Enables or by-passes the MFT'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
    Top = 477
    Width = 474
    Height = 61
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 3
    object lblBarPositionInSTime: TLabel
      Left = 15
      Top = 30
      Width = 144
      Height = 27
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Time: 00:00:00'
    end
    object lblBarPositionInSamples: TLabel
      Left = 182
      Top = 30
      Width = 173
      Height = 27
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Samples: 0'
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 472
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      TabOrder = 0
      OnMouseMove = pbProgressMouseMove
      OnMouseUp = pbProgressMouseUp
    end
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 599
    Width = 474
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = 'Open an audio file'
    Color = clCream
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 4
    Transparent = False
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

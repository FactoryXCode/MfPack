object frmMain: TfrmMain
  Left = 0
  Top = 0
  Hint = 
    'Press Press CTRL + ALT + F1 to hide the GUI.'#13#10'Press Press CTRL +' +
    ' ALT + ESC to show the GUI.'
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WASAPI Player Sample 4 - EQ MFT and DSP'
  ClientHeight = 630
  ClientWidth = 648
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  PixelsPerInch = 144
  TextHeight = 25
  object lblFile: TLabel
    Left = 60
    Top = 744
    Width = 119
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '(no file loaded)'
  end
  object btnLoad: TButton
    Left = 14
    Top = 545
    Width = 135
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Load...'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object butPlayPause: TButton
    Left = 159
    Top = 545
    Width = 135
    Height = 38
    Hint = 'CTRL + ALT + RIGHT'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Play'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 304
    Top = 545
    Width = 135
    Height = 38
    Hint = 'CTRL + ALT + SPACE'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stop'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = butStopClick
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 601
    Width = 648
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BorderStyle = sbsSingle
    Caption = 'Open an audio file'
    TabOrder = 3
  end
  object Panel2: TPanel
    Left = 0
    Top = 469
    Width = 648
    Height = 61
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 4
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
    object lblBarPosition: TLabel
      Left = 182
      Top = 30
      Width = 257
      Height = 27
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Position: 0'
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 646
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
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 648
    Height = 469
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    DoubleBuffered = False
    FullRepaint = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 5
    object Bevel1: TBevel
      Left = 242
      Top = 42
      Width = 383
      Height = 350
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object lblLow: TLabel
      Left = 273
      Top = 359
      Width = 32
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Low'
    end
    object lblMid: TLabel
      Left = 339
      Top = 359
      Width = 31
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Mid'
    end
    object lblHigh: TLabel
      Left = 401
      Top = 359
      Width = 38
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'High'
    end
    object lblRamp: TLabel
      Left = 477
      Top = 186
      Width = 99
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Ramp mode'
    end
    object lblRampMs: TLabel
      Left = 477
      Top = 263
      Width = 126
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Ramp time (ms)'
    end
    object Bevel3: TBevel
      Left = 14
      Top = 42
      Width = 103
      Height = 350
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object pmLeft: TMfPeakMeter
      Left = 79
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
      Left = 122
      Top = 42
      Width = 103
      Height = 350
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object pmRight: TMfPeakMeter
      Left = 138
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
      Left = 53
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
      Left = 147
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
      Left = 18
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
      Left = 125
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
    object lblDuration: TLabel
      Left = 12
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
    object lblPlayed: TLabel
      Left = 29
      Top = 433
      Width = 135
      Height = 27
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Played: 00:00:00'
    end
    object chkEQ: TCheckBox
      Left = 261
      Top = 53
      Width = 240
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Enable EQ'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkEQClick
    end
    object tbLow: TTrackBar
      Left = 266
      Top = 207
      Width = 48
      Height = 142
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      DoubleBuffered = True
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 6
      ShowHint = False
      TabOrder = 1
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = tbLowChange
    end
    object tbMid: TTrackBar
      Left = 330
      Top = 207
      Width = 48
      Height = 142
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      DoubleBuffered = True
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 6
      ShowHint = False
      TabOrder = 2
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = tbMidChange
    end
    object tbHigh: TTrackBar
      Left = 398
      Top = 207
      Width = 48
      Height = 142
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      DoubleBuffered = True
      Orientation = trVertical
      ParentDoubleBuffered = False
      ParentShowHint = False
      Frequency = 6
      ShowHint = False
      TabOrder = 3
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = tbHighChange
    end
    object cbxRamp: TComboBox
      Left = 477
      Top = 217
      Width = 126
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbxRampChange
    end
    object edtRampMs: TEdit
      Left = 477
      Top = 293
      Width = 82
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 5
      Text = '30'
      OnChange = edtRampMsChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 33
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
      TabOrder = 6
    end
    object trbVolumeL: TTrackBar
      Left = 21
      Top = 83
      Width = 59
      Height = 270
      Hint = 
        'Press and release SHIFT + ESC to set zero position.'#13#10'Press CTRL ' +
        '+ ALT + DOWN for volume down.'
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
      TabOrder = 7
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object trbVolumeR: TTrackBar
      Left = 166
      Top = 83
      Width = 53
      Height = 270
      Hint = 
        'Press and release SHIFT + ESC to set zero position.'#13#10'Press CTRL ' +
        '+ ALT + UP for volume up.'
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
      TabOrder = 8
      ThumbLength = 30
      TickMarks = tmBoth
      OnChange = trbVolumeRChange
    end
    object pnlEq: TPanel
      Left = 261
      Top = 84
      Width = 342
      Height = 98
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Color = 6656
      ParentBackground = False
      TabOrder = 9
      object imgSpectrumAnalizer: TImage
        Left = 12
        Top = 8
        Width = 316
        Height = 82
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        ParentCustomHint = False
        ParentShowHint = False
        ShowHint = False
      end
    end
    object stxtHighIndex: TStaticText
      Left = 402
      Top = 186
      Width = 42
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 10
    end
    object stxtMidIndex: TStaticText
      Left = 333
      Top = 186
      Width = 42
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 11
    end
    object stxtLowIndex: TStaticText
      Left = 273
      Top = 186
      Width = 42
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 12
    end
    object stxtProcessed: TStaticText
      Left = 188
      Top = 433
      Width = 263
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Samples: 0'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 13
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Audio files|*.wav;*.mp3;*.aac;*.wma;*.flac;*.m4a|All files|*.*'
    Left = 495
    Top = 460
  end
  object MainMenu1: TMainMenu
    Left = 497
    Top = 362
    object Application1: TMenuItem
      Caption = 'Application'
      object Settings1: TMenuItem
        Caption = '&Settings'
        OnClick = Settings1Click
      end
    end
  end
end

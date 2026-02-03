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
  Caption = 'WASAPI Player Sample 3 - High/Mid/Low EQ MFT'
  ClientHeight = 378
  ClientWidth = 389
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 15
  object lblFile: TLabel
    Left = 36
    Top = 446
    Width = 80
    Height = 15
    Caption = '(no file loaded)'
  end
  object btnLoad: TButton
    Left = 8
    Top = 327
    Width = 81
    Height = 23
    Caption = 'Load...'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object butPlayPause: TButton
    Left = 95
    Top = 327
    Width = 81
    Height = 23
    Hint = 'CTRL + ALT + RIGHT'
    Caption = 'Play'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 182
    Top = 327
    Width = 81
    Height = 23
    Hint = 'CTRL + ALT + SPACE'
    Caption = 'Stop'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = butStopClick
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 359
    Width = 389
    Height = 19
    Align = alBottom
    BorderStyle = sbsSingle
    Caption = 'Open an audio file'
    TabOrder = 3
  end
  object Panel2: TPanel
    Left = 0
    Top = 281
    Width = 389
    Height = 37
    Align = alTop
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 4
    object lblBarPositionInSTime: TLabel
      Left = 9
      Top = 18
      Width = 86
      Height = 16
      AutoSize = False
      Caption = 'Time: 00:00:00'
    end
    object lblBarPosition: TLabel
      Left = 109
      Top = 18
      Width = 149
      Height = 16
      AutoSize = False
      Caption = 'Position: 0'
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 387
      Height = 15
      Align = alTop
      TabOrder = 0
      OnMouseMove = pbProgressMouseMove
      OnMouseUp = pbProgressMouseUp
      ExplicitLeft = 2
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 389
    Height = 281
    Align = alTop
    DoubleBuffered = False
    FullRepaint = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 5
    object Bevel1: TBevel
      Left = 145
      Top = 26
      Width = 230
      Height = 210
    end
    object lblLow: TLabel
      Left = 164
      Top = 215
      Width = 22
      Height = 15
      Caption = 'Low'
    end
    object lblMid: TLabel
      Left = 203
      Top = 215
      Width = 21
      Height = 15
      Caption = 'Mid'
    end
    object lblHigh: TLabel
      Left = 241
      Top = 215
      Width = 26
      Height = 15
      Caption = 'High'
    end
    object lblRamp: TLabel
      Left = 286
      Top = 112
      Width = 65
      Height = 15
      Caption = 'Ramp mode'
    end
    object lblRampMs: TLabel
      Left = 286
      Top = 158
      Width = 85
      Height = 15
      Caption = 'Ramp time (ms)'
    end
    object Bevel3: TBevel
      Left = 8
      Top = 25
      Width = 62
      Height = 210
    end
    object pmLeft: TMfPeakMeter
      Left = 47
      Top = 54
      Width = 17
      Height = 152
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      DeviceRole = eConsole
      Precision = 10
    end
    object Bevel2: TBevel
      Left = 73
      Top = 25
      Width = 62
      Height = 210
    end
    object pmRight: TMfPeakMeter
      Left = 83
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
      Left = 32
      Top = 217
      Width = 19
      Height = 13
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
      Left = 88
      Top = 217
      Width = 25
      Height = 13
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
      Left = 11
      Top = 32
      Width = 56
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
      Left = 75
      Top = 32
      Width = 56
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
    object lblDuration: TLabel
      Left = 7
      Top = 245
      Width = 94
      Height = 17
      AutoSize = False
      Caption = 'Duration: 00:00:00'
    end
    object lblPlayed: TLabel
      Left = 17
      Top = 260
      Width = 81
      Height = 16
      AutoSize = False
      Caption = 'Played: 00:00:00'
    end
    object chkEQ: TCheckBox
      Left = 157
      Top = 32
      Width = 144
      Height = 15
      Caption = 'Enable EQ'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkEQClick
    end
    object tbLow: TTrackBar
      Left = 160
      Top = 124
      Width = 28
      Height = 85
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
      Left = 198
      Top = 124
      Width = 29
      Height = 85
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
      Left = 239
      Top = 124
      Width = 29
      Height = 85
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
      Left = 286
      Top = 130
      Width = 76
      Height = 23
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbxRampChange
    end
    object edtRampMs: TEdit
      Left = 286
      Top = 176
      Width = 49
      Height = 23
      TabOrder = 5
      Text = '30'
      OnChange = edtRampMsChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 20
      Top = 4
      Width = 134
      Height = 16
      Caption = 'Lock Volume Sliders'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object trbVolumeL: TTrackBar
      Left = 13
      Top = 50
      Width = 35
      Height = 162
      Hint = 
        'Press and release SHIFT + ESC to set zero position.'#13#10'Press CTRL ' +
        '+ ALT + DOWN for volume down.'
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
      Left = 100
      Top = 50
      Width = 31
      Height = 162
      Hint = 
        'Press and release SHIFT + ESC to set zero position.'#13#10'Press CTRL ' +
        '+ ALT + UP for volume up.'
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
      Left = 157
      Top = 50
      Width = 205
      Height = 59
      Color = 6656
      ParentBackground = False
      TabOrder = 9
      object imgSpectrumAnalizer: TImage
        Left = 7
        Top = 5
        Width = 190
        Height = 49
        ParentCustomHint = False
        ParentShowHint = False
        ShowHint = False
      end
    end
    object stxtHighIndex: TStaticText
      Left = 241
      Top = 112
      Width = 25
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 10
    end
    object stxtMidIndex: TStaticText
      Left = 200
      Top = 112
      Width = 25
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 11
    end
    object stxtLowIndex: TStaticText
      Left = 164
      Top = 112
      Width = 25
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 12
    end
    object stxtProcessed: TStaticText
      Left = 113
      Top = 260
      Width = 158
      Height = 15
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

object frmMain: TfrmMain
  Left = 0
  Top = 0
  Hint = 
    'Press Press CTRL + ALT + F1 to hide the GUI.'#13#10'Press Press CTRL +' +
    ' ALT + ESC to show the GUI.'
  Margins.Left = 12
  Margins.Top = 12
  Margins.Right = 12
  Margins.Bottom = 12
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WASAPI Player Sample 4 - EQ MFT and DSP'
  ClientHeight = 510
  ClientWidth = 1013
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
    Left = 48
    Top = 602
    Width = 119
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '(no file loaded)'
  end
  object btnLoad: TButton
    Left = 11
    Top = 443
    Width = 109
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Load...'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object butPlayPause: TButton
    Left = 129
    Top = 443
    Width = 108
    Height = 30
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
    Left = 246
    Top = 443
    Width = 110
    Height = 30
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
    Top = 481
    Width = 1013
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
    Top = 380
    Width = 1013
    Height = 49
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 4
    object lblBarPositionInSTime: TLabel
      Left = 26
      Top = 24
      Width = 115
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Caption = 'Time: 00:00:00'
    end
    object lblBarPosition: TLabel
      Left = 171
      Top = 24
      Width = 207
      Height = 21
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
      Width = 1011
      Height = 19
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
    Width = 1013
    Height = 380
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
      Left = 197
      Top = 35
      Width = 310
      Height = 283
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object lblLow: TLabel
      Left = 230
      Top = 291
      Width = 32
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Low'
    end
    object lblMid: TLabel
      Left = 284
      Top = 291
      Width = 31
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Mid'
    end
    object lblHigh: TLabel
      Left = 336
      Top = 291
      Width = 38
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'High'
    end
    object lblRamp: TLabel
      Left = 386
      Top = 152
      Width = 99
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Ramp mode'
    end
    object lblRampMs: TLabel
      Left = 386
      Top = 213
      Width = 126
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Ramp time (ms)'
    end
    object Bevel3: TBevel
      Left = 11
      Top = 35
      Width = 84
      Height = 283
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object pmLeft: TMfPeakMeter
      Left = 68
      Top = 74
      Width = 24
      Height = 204
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      Precision = 10
    end
    object Bevel2: TBevel
      Left = 99
      Top = 35
      Width = 84
      Height = 283
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object pmRight: TMfPeakMeter
      Left = 113
      Top = 74
      Width = 19
      Height = 204
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      Precision = 1
    end
    object Label1: TLabel
      Left = 44
      Top = 294
      Width = 28
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'Left'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 119
      Top = 294
      Width = 39
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Caption = 'Right'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblLeftVolume: TLabel
      Left = 15
      Top = 44
      Width = 77
      Height = 16
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblRightVolume: TLabel
      Left = 102
      Top = 44
      Width = 75
      Height = 16
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblDuration: TLabel
      Left = 11
      Top = 332
      Width = 146
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Duration: 00:00:00'
    end
    object lblPlayed: TLabel
      Left = 27
      Top = 351
      Width = 129
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Played: 00:00:00'
    end
    object chkEQ: TCheckBox
      Left = 222
      Top = 6
      Width = 113
      Height = 20
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Enable EQ'
      TabOrder = 0
      OnClick = chkEQClick
    end
    object tbLow: TTrackBar
      Left = 216
      Top = 168
      Width = 38
      Height = 114
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
      ThumbLength = 68
      TickMarks = tmBoth
      OnChange = tbLowChange
    end
    object tbMid: TTrackBar
      Left = 272
      Top = 168
      Width = 36
      Height = 114
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
      ThumbLength = 68
      TickMarks = tmBoth
      OnChange = tbMidChange
    end
    object tbHigh: TTrackBar
      Left = 323
      Top = 168
      Width = 39
      Height = 114
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
      ThumbLength = 68
      TickMarks = tmBoth
      OnChange = tbHighChange
    end
    object cbxRamp: TComboBox
      Left = 386
      Top = 176
      Width = 103
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
      Left = 386
      Top = 237
      Width = 67
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
      Left = 27
      Top = 6
      Width = 182
      Height = 21
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
      Left = 18
      Top = 68
      Width = 47
      Height = 219
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
      ThumbLength = 68
      TickMarks = tmBoth
      OnChange = trbVolumeLChange
    end
    object trbVolumeR: TTrackBar
      Left = 135
      Top = 68
      Width = 42
      Height = 219
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
      ThumbLength = 68
      TickMarks = tmBoth
      OnChange = trbVolumeRChange
    end
    object pnlEq: TPanel
      Left = 510
      Top = 35
      Width = 497
      Height = 283
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Color = 6656
      DoubleBuffered = True
      ParentBackground = False
      ParentDoubleBuffered = False
      TabOrder = 9
      object avGraph: TMfAudioVisualizer
        Left = 11
        Top = 11
        Width = 477
        Height = 267
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        PeakThreshold = 0.980000019073486300
        PeakCapFrac = 0.100000001490116100
        BarCount = 32
      end
    end
    object stxtHighIndex: TStaticText
      Left = 326
      Top = 152
      Width = 33
      Height = 21
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
      Left = 270
      Top = 152
      Width = 35
      Height = 21
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
      Left = 222
      Top = 152
      Width = 33
      Height = 21
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
      Left = 171
      Top = 351
      Width = 213
      Height = 21
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
    object Panel1: TPanel
      Left = 213
      Top = 44
      Width = 279
      Height = 105
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Color = 6656
      ParentBackground = False
      TabOrder = 14
      object imgSpectrumAnalizer: TImage
        Left = 12
        Top = 6
        Width = 257
        Height = 90
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        ParentCustomHint = False
        ParentShowHint = False
        ShowHint = False
      end
    end
    object chkGraph: TCheckBox
      Left = 516
      Top = 6
      Width = 147
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Enable Graph'
      TabOrder = 15
      OnClick = chkGraphClick
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Audio files|*.wav;*.mp3;*.aac;*.wma;*.flac;*.m4a|All files|*.*'
    Left = 495
    Top = 460
  end
  object MainMenu1: TMainMenu
    Left = 656
    Top = 347
    object Application1: TMenuItem
      Caption = 'Application'
      object Settings1: TMenuItem
        Caption = '&Settings'
        OnClick = Settings1Click
      end
    end
  end
end

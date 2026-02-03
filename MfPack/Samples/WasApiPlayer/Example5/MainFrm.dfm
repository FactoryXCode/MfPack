object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WasApi Player Sample 5'
  ClientHeight = 707
  ClientWidth = 1650
  Color = clSilver
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
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 25
  object butPlayPause: TButton
    Left = 116
    Top = 630
    Width = 100
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 221
    Top = 630
    Width = 100
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 1650
    Height = 562
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clSilver
    ParentBackground = False
    TabOrder = 2
    object lblDuration: TLabel
      Left = 17
      Top = 504
      Width = 157
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Duration: 00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblProcessed: TLabel
      Left = 188
      Top = 530
      Width = 291
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Samples: 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblPlayed: TLabel
      Left = 35
      Top = 530
      Width = 138
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Played: 00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object gbEQ: TGroupBox
      Left = 262
      Top = 11
      Width = 405
      Height = 482
      Caption = ' Parametric EQ '
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      object lblEqGain: TLabel
        Left = 17
        Top = 108
        Width = 98
        Height = 23
        Caption = 'Gain: 0.0 dB'
      end
      object lblEqFreq: TLabel
        Left = 17
        Top = 174
        Width = 112
        Height = 23
        Caption = 'Freq: 1000 Hz'
      end
      object lblEqQ: TLabel
        Left = 17
        Top = 242
        Width = 58
        Height = 23
        Caption = 'Q: 1.00'
      end
      object lblEqBW: TLabel
        Left = 17
        Top = 308
        Width = 103
        Height = 23
        Caption = 'BW: 1.00 oct'
      end
      object lblEqTP: TLabel
        Left = 17
        Top = 373
        Width = 171
        Height = 23
        Caption = 'Ceiling: -1.0 dBTP, 4x'
      end
      object cbEqEnabled: TCheckBox
        Left = 17
        Top = 33
        Width = 108
        Height = 24
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbEqEnabledClick
      end
      object cbEqUseBW: TCheckBox
        Left = 135
        Top = 33
        Width = 220
        Height = 24
        Caption = 'Use Bandwidth (oct)'
        TabOrder = 1
        OnClick = cbEqUseBWClick
      end
      object cbEqTruePeak: TCheckBox
        Left = 17
        Top = 65
        Width = 252
        Height = 24
        Caption = 'True-peak guard (-1.0 dBTP)'
        TabOrder = 2
        OnClick = cbEqTruePeakClick
      end
      object trbEqGain: TTrackBar
        Left = 17
        Top = 134
        Width = 369
        Height = 31
        Max = 240
        Min = -240
        Frequency = 40
        TabOrder = 3
        ThumbLength = 30
        OnChange = trbEqGainChange
      end
      object trbEqFreq: TTrackBar
        Left = 17
        Top = 200
        Width = 369
        Height = 31
        Max = 1000
        Frequency = 100
        TabOrder = 4
        ThumbLength = 30
        OnChange = trbEqFreqChange
      end
      object trbEqQ: TTrackBar
        Left = 17
        Top = 267
        Width = 369
        Height = 32
        Max = 120
        Min = 2
        Frequency = 10
        Position = 10
        TabOrder = 5
        ThumbLength = 30
        OnChange = trbEqQChange
      end
      object trbEqBW: TTrackBar
        Left = 17
        Top = 333
        Width = 369
        Height = 32
        Max = 40
        Min = 1
        Frequency = 5
        Position = 10
        TabOrder = 6
        ThumbLength = 30
        OnChange = trbEqBWChange
      end
    end
    object gbFlanger: TGroupBox
      Left = 678
      Top = 11
      Width = 378
      Height = 482
      Caption = ' Flanger / Echo '
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      object lblFlWet: TLabel
        Left = 17
        Top = 78
        Width = 78
        Height = 23
        Caption = 'Wet: 0.35'
      end
      object lblFlDelay: TLabel
        Left = 17
        Top = 144
        Width = 132
        Height = 23
        Caption = 'Base delay: 3 ms'
      end
      object lblFlDepth: TLabel
        Left = 17
        Top = 210
        Width = 98
        Height = 23
        Caption = 'Depth: 2 ms'
      end
      object lblFlRate: TLabel
        Left = 17
        Top = 278
        Width = 107
        Height = 23
        Caption = 'Rate: 0.25 Hz'
      end
      object lblFlFeedback: TLabel
        Left = 17
        Top = 344
        Width = 122
        Height = 23
        Caption = 'Feedback: 0.20'
      end
      object cbFlEnabled: TCheckBox
        Left = 17
        Top = 33
        Width = 108
        Height = 24
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbFlEnabledClick
      end
      object trbFlWet: TTrackBar
        Left = 17
        Top = 102
        Width = 342
        Height = 32
        Max = 100
        Frequency = 10
        Position = 35
        TabOrder = 1
        ThumbLength = 30
        OnChange = trbFlWetChange
      end
      object trbFlDelay: TTrackBar
        Left = 17
        Top = 170
        Width = 342
        Height = 31
        Max = 2000
        Frequency = 100
        Position = 3
        TabOrder = 2
        ThumbLength = 30
        OnChange = trbFlDelayChange
      end
      object trbFlDepth: TTrackBar
        Left = 17
        Top = 236
        Width = 342
        Height = 31
        Max = 50
        Frequency = 5
        Position = 2
        TabOrder = 3
        ThumbLength = 30
        OnChange = trbFlDepthChange
      end
      object trbFlRate: TTrackBar
        Left = 17
        Top = 303
        Width = 342
        Height = 32
        Max = 500
        Frequency = 50
        Position = 25
        TabOrder = 4
        ThumbLength = 30
        OnChange = trbFlRateChange
      end
      object trbFlFeedback: TTrackBar
        Left = 17
        Top = 369
        Width = 342
        Height = 32
        Max = 98
        Frequency = 10
        Position = 20
        TabOrder = 5
        ThumbLength = 30
        OnChange = trbFlFeedbackChange
      end
    end
    object gbDynamics: TGroupBox
      Left = 1068
      Top = 11
      Width = 563
      Height = 482
      Caption = ' Dynamics (Compressor/Limiter)  '
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 2
      object Bevel1: TBevel
        Left = 294
        Top = 192
        Width = 256
        Height = 278
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
      end
      object lblDynTP: TLabel
        Left = 17
        Top = 66
        Width = 281
        Height = 23
        Caption = 'True-peak guard ceiling: -1.0 dBTP'
      end
      object lblDynThresh: TLabel
        Left = 17
        Top = 101
        Width = 158
        Height = 23
        Caption = 'Threshold: -18.0 dB'
      end
      object lblDynRatio: TLabel
        Left = 17
        Top = 161
        Width = 92
        Height = 23
        Caption = 'Ratio: 3.0:1'
      end
      object lblDynAttack: TLabel
        Left = 17
        Top = 219
        Width = 127
        Height = 23
        Caption = 'Attack: 10.0 ms'
      end
      object lblDynRelease: TLabel
        Left = 17
        Top = 279
        Width = 127
        Height = 23
        Caption = 'Release: 120 ms'
      end
      object lblDynKnee: TLabel
        Left = 17
        Top = 339
        Width = 101
        Height = 23
        Caption = 'Knee: 6.0 dB'
      end
      object lblDynMakeup: TLabel
        Left = 17
        Top = 398
        Width = 127
        Height = 23
        Caption = 'Makeup: 0.0 dB'
      end
      object lblDynLimCeil: TLabel
        Left = 321
        Top = 219
        Width = 185
        Height = 23
        Caption = 'Limiter ceiling: -1.0 dB'
      end
      object lblDynLimLook: TLabel
        Left = 321
        Top = 279
        Width = 135
        Height = 23
        Caption = 'Lookahead: 5 ms'
      end
      object lblDynLimRel: TLabel
        Left = 321
        Top = 339
        Width = 176
        Height = 23
        Caption = 'Limiter release: 80 ms'
      end
      object lblDynCompGR: TLabel
        Left = 321
        Top = 414
        Width = 138
        Height = 23
        Caption = 'Comp GR: 0.0 dB'
      end
      object lblDynLimGR: TLabel
        Left = 321
        Top = 440
        Width = 120
        Height = 23
        Caption = 'Lim GR: 0.0 dB'
      end
      object cbDynEnabled: TCheckBox
        Left = 17
        Top = 36
        Width = 114
        Height = 23
        Caption = 'Enabled'
        TabOrder = 0
        OnClick = cbDynEnabledClick
      end
      object cbDynRms: TCheckBox
        Left = 389
        Top = 33
        Width = 150
        Height = 23
        Caption = 'RMS detector'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbDynRmsClick
      end
      object cbDynAutoMakeup: TCheckBox
        Left = 390
        Top = 66
        Width = 149
        Height = 23
        Caption = 'Auto makeup'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbDynAutoMakeupClick
      end
      object cbDynTruePeak: TCheckBox
        Left = 390
        Top = 102
        Width = 122
        Height = 23
        Caption = 'True-peak'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbDynTruePeakClick
      end
      object cmbDynTPOS: TComboBox
        Left = 390
        Top = 129
        Width = 99
        Height = 31
        Style = csDropDownList
        TabOrder = 4
        OnChange = cmbDynTPOSChange
      end
      object trbDynThresh: TTrackBar
        Left = 17
        Top = 126
        Width = 216
        Height = 32
        TabOrder = 5
        ThumbLength = 30
        OnChange = trbDynThreshChange
      end
      object trbDynRatio: TTrackBar
        Left = 17
        Top = 186
        Width = 216
        Height = 32
        TabOrder = 6
        ThumbLength = 30
        OnChange = trbDynRatioChange
      end
      object trbDynAttack: TTrackBar
        Left = 17
        Top = 245
        Width = 216
        Height = 31
        TabOrder = 7
        ThumbLength = 30
        OnChange = trbDynAttackChange
      end
      object trbDynRelease: TTrackBar
        Left = 17
        Top = 305
        Width = 216
        Height = 31
        TabOrder = 8
        ThumbLength = 30
        OnChange = trbDynReleaseChange
      end
      object trbDynKnee: TTrackBar
        Left = 17
        Top = 363
        Width = 216
        Height = 32
        TabOrder = 9
        ThumbLength = 30
        OnChange = trbDynKneeChange
      end
      object trbDynMakeup: TTrackBar
        Left = 17
        Top = 423
        Width = 216
        Height = 32
        TabOrder = 10
        ThumbLength = 30
        OnChange = trbDynMakeupChange
      end
      object trbDynLimCeil: TTrackBar
        Left = 321
        Top = 245
        Width = 216
        Height = 31
        TabOrder = 11
        ThumbLength = 30
        OnChange = trbDynLimCeilChange
      end
      object trbDynLimLook: TTrackBar
        Left = 321
        Top = 305
        Width = 216
        Height = 31
        TabOrder = 12
        ThumbLength = 30
        OnChange = trbDynLimLookChange
      end
      object trbDynLimRel: TTrackBar
        Left = 321
        Top = 363
        Width = 216
        Height = 32
        TabOrder = 13
        ThumbLength = 30
        OnChange = trbDynLimRelChange
      end
    end
    object GroupBox1: TGroupBox
      Left = 11
      Top = 11
      Width = 239
      Height = 482
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = ' Volume '
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 3
      object pmLeft: TMfPeakMeter
        Left = 90
        Top = 107
        Width = 24
        Height = 316
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
      object pmRight: TMfPeakMeter
        Left = 119
        Top = 107
        Width = 22
        Height = 316
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
        Left = 33
        Top = 443
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
        Left = 164
        Top = 443
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
        Left = 19
        Top = 74
        Width = 58
        Height = 18
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
        Left = 158
        Top = 74
        Width = 45
        Height = 18
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
      object cbLockVolumeSliders: TCheckBox
        Left = 5
        Top = 32
        Width = 193
        Height = 25
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
        Left = 16
        Top = 98
        Width = 55
        Height = 339
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
        ThumbLength = 45
        TickMarks = tmBoth
        OnChange = trbVolumeLChange
      end
      object trbVolumeR: TTrackBar
        Left = 152
        Top = 98
        Width = 55
        Height = 339
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
        ThumbLength = 45
        TickMarks = tmBoth
        OnChange = trbVolumeRChange
      end
    end
  end
  object pnlTrackbar: TPanel
    Left = 0
    Top = 562
    Width = 1650
    Height = 55
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 3
    object lblBarPositionInSTime: TLabel
      Left = 14
      Top = 27
      Width = 129
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Time: 00:00:00'
    end
    object lblBarPositionInSamples: TLabel
      Left = 164
      Top = 27
      Width = 204
      Height = 24
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
      Width = 1648
      Height = 22
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
    Top = 680
    Width = 1650
    Height = 27
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
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 4
    Transparent = False
    ExplicitTop = 683
  end
  object btnLoad: TButton
    Left = 12
    Top = 630
    Width = 99
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Load ...'
    TabOrder = 5
    OnClick = btnLoadClick
  end
  object mnuMain: TMainMenu
    Left = 386
    Top = 498
    object OpenAudioFile1: TMenuItem
      Caption = 'File'
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
    Left = 477
    Top = 498
  end
  object MfWasApiPlayerEngine: TMfWasApiPlayerEngine
    Left = 437
    Top = 332
  end
  object FXCompressorLimiter: TMfCompressorLimiterEffect
    CompThresholdDb = -18.000000000000000000
    CompRatio = 3.000000000000000000
    CompAttackMs = 10.000000000000000000
    CompReleaseMs = 120.000000000000000000
    CompKneeDb = 6.000000000000000000
    LimCeilingDb = -1.000000000000000000
    LimReleaseMs = 80.000000000000000000
    LimLookaheadMs = 5.000000000000000000
    TruePeakCeilingDbTP = -1.000000000000000000
    TruePeakOversample = 4
    Left = 1215
    Top = 496
  end
  object FXParametricEq: TMfParametricEqEffect
    GainDb = 3.000000000000000000
    CenterFreqHz = 1000.000000000000000000
    Q = 1.000000000000000000
    BandwidthOctaves = 1.000000000000000000
    RampMode = rmFast
    TruePeakCeilingDbTP = -1.000000000000000000
    TruePeakOversample = 4
    Left = 616
    Top = 331
  end
  object FXFlangerEcho: TMfFlangerEchoEffect
    BaseDelayMs = 3.000000000000000000
    DepthMs = 2.000000000000000000
    RateHz = 0.250000000000000000
    Feedback = 0.200000002980232200
    Wet = 0.349999994039535500
    Left = 705
    Top = 331
  end
  object waFxRack: TMfWasApiEffectsRack
    Slots = <
      item
        Effect = FXParametricEq
      end
      item
        Effect = FXCompressorLimiter
      end
      item
        Effect = FXFlangerEcho
      end>
    Left = 804
    Top = 498
  end
end

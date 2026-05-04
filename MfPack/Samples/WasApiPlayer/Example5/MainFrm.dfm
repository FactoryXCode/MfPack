object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 12
  Margins.Top = 12
  Margins.Right = 12
  Margins.Bottom = 12
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WasApi Player Sample 5'
  ClientHeight = 1068
  ClientWidth = 1461
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
  object Label4: TLabel
    Left = 150
    Top = 399
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
  object butPlayPause: TButton
    Left = 107
    Top = 1001
    Width = 90
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Play'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 201
    Top = 1001
    Width = 90
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stop'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 1461
    Height = 933
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clSilver
    ParentBackground = False
    TabOrder = 2
    object lblDuration: TLabel
      Left = 21
      Top = 903
      Width = 157
      Height = 23
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
      Left = 345
      Top = 903
      Width = 261
      Height = 23
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
      Left = 187
      Top = 903
      Width = 139
      Height = 24
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
    object visAudioMix: TMfAudioMixVisualizer
      Left = 432
      Top = 444
      Width = 719
      Height = 408
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      PeakThreshold = 0.980000019073486300
      PeakCapFrac = 0.119999997317791000
      DbMin = -60.000000000000000000
      DbTickStep = 2
      DbLabelStep = 4
      DbScaleWidth = 60
      AutoStart = True
      VolumeScaleMode = vsmDbPerceptual
      BarCount = 64
    end
    object gbEQ: TGroupBox
      Left = 12
      Top = 9
      Width = 318
      Height = 423
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
        Left = 15
        Top = 98
        Width = 91
        Height = 23
        Caption = 'Gain: 0.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblEqFreq: TLabel
        Left = 15
        Top = 156
        Width = 103
        Height = 23
        Caption = 'Freq: 1000 Hz'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblEqQ: TLabel
        Left = 15
        Top = 218
        Width = 53
        Height = 23
        Caption = 'Q: 1.00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblEqBW: TLabel
        Left = 15
        Top = 278
        Width = 95
        Height = 23
        Caption = 'BW: 1.00 oct'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblEqTP: TLabel
        Left = 15
        Top = 336
        Width = 159
        Height = 23
        Caption = 'Ceiling: -1.0 dBTP, 4x'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object cbEqEnabled: TCheckBox
        Left = 15
        Top = 30
        Width = 98
        Height = 21
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbEqEnabledClick
      end
      object cbEqUseBW: TCheckBox
        Left = 122
        Top = 30
        Width = 198
        Height = 21
        Caption = 'Use Bandwidth (oct)'
        TabOrder = 1
        OnClick = cbEqUseBWClick
      end
      object cbEqTruePeak: TCheckBox
        Left = 15
        Top = 59
        Width = 227
        Height = 21
        Caption = 'True-peak guard (-1.0 dBTP)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = cbEqTruePeakClick
      end
      object trbEqGain: TTrackBar
        Left = 8
        Top = 120
        Width = 306
        Height = 29
        Max = 240
        Min = -240
        Frequency = 40
        TabOrder = 3
        ThumbLength = 45
        OnChange = trbEqGainChange
      end
      object trbEqFreq: TTrackBar
        Left = 8
        Top = 180
        Width = 306
        Height = 29
        Max = 1000
        Frequency = 100
        TabOrder = 4
        ThumbLength = 45
        OnChange = trbEqFreqChange
      end
      object trbEqQ: TTrackBar
        Left = 8
        Top = 240
        Width = 306
        Height = 29
        Max = 120
        Min = 2
        Frequency = 10
        Position = 10
        TabOrder = 5
        ThumbLength = 45
        OnChange = trbEqQChange
      end
      object trbEqBW: TTrackBar
        Left = 8
        Top = 300
        Width = 306
        Height = 29
        Max = 40
        Min = 1
        Frequency = 5
        Position = 10
        TabOrder = 6
        ThumbLength = 45
        OnChange = trbEqBWChange
      end
    end
    object gbFlanger: TGroupBox
      Left = 336
      Top = 9
      Width = 318
      Height = 423
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
        Left = 15
        Top = 99
        Width = 70
        Height = 23
        Caption = 'Wet: 0.35'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblFlDelay: TLabel
        Left = 15
        Top = 159
        Width = 125
        Height = 23
        Caption = 'Base delay: 3 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblFlDepth: TLabel
        Left = 15
        Top = 218
        Width = 92
        Height = 23
        Caption = 'Depth: 2 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblFlRate: TLabel
        Left = 15
        Top = 279
        Width = 99
        Height = 23
        Caption = 'Rate: 0.25 Hz'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblFlFeedback: TLabel
        Left = 15
        Top = 339
        Width = 111
        Height = 23
        Caption = 'Feedback: 0.20'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object cbFlEnabled: TCheckBox
        Left = 15
        Top = 30
        Width = 98
        Height = 21
        Caption = 'Enabled'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = cbFlEnabledClick
      end
      object trbFlWet: TTrackBar
        Left = 8
        Top = 120
        Width = 306
        Height = 30
        Max = 100
        Frequency = 10
        Position = 35
        TabOrder = 1
        ThumbLength = 45
        OnChange = trbFlWetChange
      end
      object trbFlDelay: TTrackBar
        Left = 8
        Top = 182
        Width = 306
        Height = 28
        Max = 2000
        Frequency = 100
        Position = 3
        TabOrder = 2
        ThumbLength = 45
        OnChange = trbFlDelayChange
      end
      object trbFlDepth: TTrackBar
        Left = 8
        Top = 242
        Width = 306
        Height = 27
        Max = 50
        Frequency = 5
        Position = 2
        TabOrder = 3
        ThumbLength = 45
        OnChange = trbFlDepthChange
      end
      object trbFlRate: TTrackBar
        Left = 8
        Top = 302
        Width = 306
        Height = 28
        Max = 500
        Frequency = 50
        Position = 25
        TabOrder = 4
        ThumbLength = 45
        OnChange = trbFlRateChange
      end
      object trbFlFeedback: TTrackBar
        Left = 8
        Top = 359
        Width = 306
        Height = 28
        Max = 98
        Frequency = 10
        Position = 20
        TabOrder = 5
        ThumbLength = 45
        OnChange = trbFlFeedbackChange
      end
    end
    object gbDynamics: TGroupBox
      Left = 1160
      Top = 9
      Width = 291
      Height = 884
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
      object lblDynTP: TLabel
        Left = 15
        Top = 60
        Width = 261
        Height = 23
        Caption = 'True-peak guard ceiling: -1.0 dBTP'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynThresh: TLabel
        Left = 15
        Top = 92
        Width = 147
        Height = 23
        Caption = 'Threshold: -18.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynRatio: TLabel
        Left = 15
        Top = 146
        Width = 83
        Height = 23
        Caption = 'Ratio: 3.0:1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynAttack: TLabel
        Left = 15
        Top = 197
        Width = 115
        Height = 23
        Caption = 'Attack: 10.0 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynRelease: TLabel
        Left = 15
        Top = 251
        Width = 120
        Height = 23
        Caption = 'Release: 120 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynKnee: TLabel
        Left = 15
        Top = 305
        Width = 94
        Height = 23
        Caption = 'Knee: 6.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynMakeup: TLabel
        Left = 15
        Top = 359
        Width = 117
        Height = 23
        Caption = 'Makeup: 0.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynLimCeil: TLabel
        Left = 15
        Top = 456
        Width = 169
        Height = 23
        Caption = 'Limiter ceiling: -1.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynLimLook: TLabel
        Left = 15
        Top = 512
        Width = 128
        Height = 23
        Caption = 'Lookahead: 5 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynLimRel: TLabel
        Left = 15
        Top = 566
        Width = 164
        Height = 23
        Caption = 'Limiter release: 80 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynCompGR: TLabel
        Left = 15
        Top = 633
        Width = 129
        Height = 23
        Caption = 'Comp GR: 0.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblDynLimGR: TLabel
        Left = 15
        Top = 657
        Width = 110
        Height = 23
        Caption = 'Lim GR: 0.0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object cbDynEnabled: TCheckBox
        Left = 15
        Top = 33
        Width = 104
        Height = 20
        Caption = 'Enabled'
        TabOrder = 0
        OnClick = cbDynEnabledClick
      end
      object cbDynRms: TCheckBox
        Left = 15
        Top = 696
        Width = 135
        Height = 21
        Caption = 'RMS detector'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 1
        OnClick = cbDynRmsClick
      end
      object cbDynAutoMakeup: TCheckBox
        Left = 15
        Top = 726
        Width = 135
        Height = 21
        Caption = 'Auto makeup'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 2
        OnClick = cbDynAutoMakeupClick
      end
      object cbDynTruePeak: TCheckBox
        Left = 15
        Top = 759
        Width = 110
        Height = 21
        Caption = 'True-peak'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 3
        OnClick = cbDynTruePeakClick
      end
      object cmbDynTPOS: TComboBox
        Left = 15
        Top = 783
        Width = 90
        Height = 31
        Style = csDropDownList
        TabOrder = 4
        OnChange = cmbDynTPOSChange
      end
      object trbDynThresh: TTrackBar
        Left = 15
        Top = 114
        Width = 270
        Height = 29
        TabOrder = 5
        ThumbLength = 45
        OnChange = trbDynThreshChange
      end
      object trbDynRatio: TTrackBar
        Left = 15
        Top = 168
        Width = 270
        Height = 29
        TabOrder = 6
        ThumbLength = 45
        OnChange = trbDynRatioChange
      end
      object trbDynAttack: TTrackBar
        Left = 15
        Top = 221
        Width = 270
        Height = 28
        TabOrder = 7
        ThumbLength = 45
        OnChange = trbDynAttackChange
      end
      object trbDynRelease: TTrackBar
        Left = 15
        Top = 275
        Width = 270
        Height = 28
        TabOrder = 8
        ThumbLength = 45
        OnChange = trbDynReleaseChange
      end
      object trbDynKnee: TTrackBar
        Left = 15
        Top = 327
        Width = 270
        Height = 29
        TabOrder = 9
        ThumbLength = 45
        OnChange = trbDynKneeChange
      end
      object trbDynMakeup: TTrackBar
        Left = 15
        Top = 381
        Width = 270
        Height = 29
        TabOrder = 10
        ThumbLength = 45
        OnChange = trbDynMakeupChange
      end
      object trbDynLimCeil: TTrackBar
        Left = 15
        Top = 482
        Width = 270
        Height = 28
        TabOrder = 11
        ThumbLength = 45
        OnChange = trbDynLimCeilChange
      end
      object trbDynLimLook: TTrackBar
        Left = 15
        Top = 536
        Width = 270
        Height = 28
        TabOrder = 12
        ThumbLength = 45
        OnChange = trbDynLimLookChange
      end
      object trbDynLimRel: TTrackBar
        Left = 15
        Top = 588
        Width = 270
        Height = 29
        TabOrder = 13
        ThumbLength = 45
        OnChange = trbDynLimRelChange
      end
    end
    object gbChorus: TGroupBox
      Left = 660
      Top = 9
      Width = 492
      Height = 423
      Caption = ' Chorus '
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 4
      object lblChMix: TLabel
        Left = 17
        Top = 99
        Width = 67
        Height = 23
        Caption = 'Mix: 0.35'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChBaseDelay: TLabel
        Left = 17
        Top = 159
        Width = 147
        Height = 23
        Caption = 'Base delay: 22.0 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChDepth: TLabel
        Left = 17
        Top = 218
        Width = 105
        Height = 23
        Caption = 'Depth: 8.0 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChFeedback: TLabel
        Left = 17
        Top = 282
        Width = 111
        Height = 23
        Caption = 'Feedback: 0.10'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChRate: TLabel
        Left = 258
        Top = 99
        Width = 99
        Height = 23
        Caption = 'Rate: 0.35 Hz'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChTempo: TLabel
        Left = 260
        Top = 159
        Width = 169
        Height = 23
        Caption = 'Tempo: 120 BPM / 1/8'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChWidth: TLabel
        Left = 266
        Top = 218
        Width = 92
        Height = 23
        Caption = 'Width: 70 %'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblChSmooth: TLabel
        Left = 260
        Top = 279
        Width = 138
        Height = 23
        Caption = 'Smoothing: 20 ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object cbChEnabled: TCheckBox
        Left = 17
        Top = 32
        Width = 108
        Height = 22
        Caption = 'Enabled'
        TabOrder = 0
        OnClick = cbChEnabledClick
      end
      object trbChMix: TTrackBar
        Left = 11
        Top = 120
        Width = 229
        Height = 30
        Max = 100
        Frequency = 10
        Position = 35
        TabOrder = 1
        ThumbLength = 45
        OnChange = trbChMixChange
      end
      object trbChBaseDelay: TTrackBar
        Left = 11
        Top = 182
        Width = 229
        Height = 28
        Max = 60
        Min = 1
        Frequency = 5
        Position = 22
        TabOrder = 2
        ThumbLength = 45
        OnChange = trbChBaseDelayChange
      end
      object trbChDepth: TTrackBar
        Left = 11
        Top = 242
        Width = 229
        Height = 28
        Max = 250
        Frequency = 25
        Position = 80
        TabOrder = 3
        ThumbLength = 45
        OnChange = trbChDepthChange
      end
      object trbChFeedback: TTrackBar
        Left = 9
        Top = 305
        Width = 231
        Height = 30
        Max = 95
        Frequency = 10
        Position = 10
        TabOrder = 4
        ThumbLength = 45
        OnChange = trbChFeedbackChange
      end
      object rbChRateFree: TRadioButton
        Left = 258
        Top = 29
        Width = 102
        Height = 22
        Caption = 'Rate (Hz)'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        TabStop = True
        OnClick = rbChRateModeClick
      end
      object rbChRateSync: TRadioButton
        Left = 258
        Top = 59
        Width = 119
        Height = 22
        Caption = 'Tempo sync'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        OnClick = rbChRateModeClick
      end
      object trbChRate: TTrackBar
        Left = 251
        Top = 120
        Width = 225
        Height = 30
        Max = 2000
        Min = 1
        Frequency = 200
        Position = 35
        TabOrder = 7
        ThumbLength = 45
        OnChange = trbChRateChange
      end
      object edtChBpm: TEdit
        Left = 260
        Top = 183
        Width = 66
        Height = 31
        TabOrder = 8
        Text = '120'
        OnChange = edtChBpmChange
      end
      object cmbChNoteDiv: TComboBox
        Left = 338
        Top = 183
        Width = 81
        Height = 31
        Style = csDropDownList
        TabOrder = 9
        OnChange = cmbChNoteDivChange
      end
      object trbChWidth: TTrackBar
        Left = 251
        Top = 240
        Width = 225
        Height = 29
        Max = 100
        Frequency = 10
        Position = 70
        TabOrder = 10
        ThumbLength = 45
        OnChange = trbChWidthChange
      end
      object trbChSmooth: TTrackBar
        Left = 258
        Top = 300
        Width = 225
        Height = 29
        Max = 200
        Frequency = 20
        Position = 20
        TabOrder = 11
        ThumbLength = 45
        OnChange = trbChSmoothChange
      end
    end
    object GroupBox1: TGroupBox
      Left = 12
      Top = 440
      Width = 221
      Height = 453
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
        Left = 85
        Top = 96
        Width = 21
        Height = 285
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
        Left = 111
        Top = 96
        Width = 21
        Height = 285
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
        Left = 30
        Top = 398
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
        Left = 147
        Top = 398
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
        Left = 17
        Top = 66
        Width = 52
        Height = 17
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
        Left = 154
        Top = 66
        Width = 40
        Height = 17
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
        Top = 29
        Width = 132
        Height = 22
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Lock Sliders'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 0
      end
      object trbVolumeL: TTrackBar
        Left = 1
        Top = 84
        Width = 74
        Height = 304
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
        ThumbLength = 68
        TickMarks = tmBoth
        OnChange = trbVolumeLChange
      end
      object trbVolumeR: TTrackBar
        Left = 130
        Top = 89
        Width = 74
        Height = 304
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
        ThumbLength = 68
        TickMarks = tmBoth
        OnChange = trbVolumeRChange
      end
    end
    object GroupBox2: TGroupBox
      Left = 242
      Top = 440
      Width = 176
      Height = 453
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Master Volume'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 5
      object lblMainLeftVolume: TLabel
        Left = 19
        Top = 66
        Width = 52
        Height = 17
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
      object lblMainRightVolume: TLabel
        Left = 105
        Top = 66
        Width = 40
        Height = 17
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
      object Label3: TLabel
        Left = 17
        Top = 398
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
      object Label5: TLabel
        Left = 77
        Top = 398
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
      object trbMainVolumeL: TTrackBar
        Left = 2
        Top = 89
        Width = 86
        Height = 304
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
        TabOrder = 0
        ThumbLength = 68
        TickMarks = tmBoth
        OnChange = trbMainVolumeLChange
      end
      object trbMainVolumeR: TTrackBar
        Left = 82
        Top = 89
        Width = 79
        Height = 304
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
        ThumbLength = 68
        TickMarks = tmBoth
        OnChange = trbMainVolumeRChange
      end
      object CheckBox1: TCheckBox
        Left = 9
        Top = 29
        Width = 117
        Height = 22
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Lock Sliders'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 2
      end
      object cbxMute: TCheckBox
        Left = 9
        Top = 426
        Width = 110
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Mute'
        TabOrder = 3
        OnClick = cbxMuteClick
      end
    end
    object rbSpectrum: TRadioButton
      Left = 956
      Top = 861
      Width = 106
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Spectrum'
      Checked = True
      TabOrder = 6
      TabStop = True
      OnClick = rbSpectrumClick
    end
    object rbVu: TRadioButton
      Left = 1074
      Top = 861
      Width = 75
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'VU'
      TabOrder = 7
      OnClick = rbVuClick
    end
  end
  object pnlTrackbar: TPanel
    Left = 0
    Top = 933
    Width = 1461
    Height = 51
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 3
    object lblBarPositionInSTime: TLabel
      Left = 12
      Top = 24
      Width = 117
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Time: 00:00:00'
    end
    object lblBarPositionInSamples: TLabel
      Left = 147
      Top = 24
      Width = 185
      Height = 23
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
      Width = 1459
      Height = 21
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
    Top = 1044
    Width = 1461
    Height = 24
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
  end
  object btnLoad: TButton
    Left = 12
    Top = 1001
    Width = 90
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Load ...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = btnLoadClick
  end
  object mnuMain: TMainMenu
    Left = 527
    Top = 1065
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
    Left = 435
    Top = 1065
  end
  object fxChorus: TMfChorusEffect
    Enabled = False
    NoteDiv = cnd1_1
    Left = 410
    Top = 25
  end
  object fxCompressorLimiter: TMfCompressorLimiterEffect
    Enabled = False
    CompEnabled = False
    CompAutoMakeup = False
    LimEnabled = False
    RmsDetector = False
    TruePeakGuard = True
    TruePeakOversample = 4
    Left = 607
    Top = 28
  end
  object fxParametricEq: TMfParametricEqEffect
    Enabled = False
    RampMode = rmOff
    RampTimeMs = 0
    TruePeakOversample = 0
    Left = 327
    Top = 27
  end
  object fxFlangerEcho: TMfFlangerEchoEffect
    Enabled = False
    Left = 158
    Top = 23
  end
  object waFxRack: TMfWasApiEffectsRack
    Slots = <
      item
        Effect = fxParametricEq
      end
      item
        Effect = fxChorus
      end
      item
        Effect = fxCompressorLimiter
      end
      item
        Effect = fxFlangerEcho
      end>
    Left = 248
    Top = 25
  end
  object MfWasApiPlayerEngine: TMfWasApiPlayerEngine
    UseDefaultDevice = False
    EffectsRack = waFxRack
    OnStateChanged = MfWasApiPlayerEngineStateChanged
    OnError = MfWasApiPlayerEngineError
    OnReady = MfWasApiPlayerEngineReady
    OnProcessed = MfWasApiPlayerEngineProcessed
    OnEnded = MfWasApiPlayerEngineEnded
    PitchRangePct = 16.000000000000000000
    PitchDetentPct = 0.100000000000000000
    PitchAutoZeroPct = 0.300000000000000000
    PitchRampMs = 50
    VarispeedEnabled = True
    Left = 495
    Top = 28
  end
  object aepMaster: TMfAudioEndPoint
    DeviceState = 'Active'
    MasterScalarVolume = 0.624515175819397000
    MasterDbVolume = -7.139482975006104000
    Mute = True
    Left = 158
    Top = 90
  end
end

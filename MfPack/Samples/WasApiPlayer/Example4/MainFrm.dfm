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
  ClientHeight = 312
  ClientWidth = 638
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
    Left = 29
    Top = 361
    Width = 80
    Height = 15
    Caption = '(no file loaded)'
  end
  object btnLoad: TButton
    Left = 7
    Top = 266
    Width = 65
    Height = 25
    Caption = 'Load...'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object butPlayPause: TButton
    Left = 77
    Top = 266
    Width = 65
    Height = 25
    Hint = 'CTRL + ALT + RIGHT'
    Caption = 'Play'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 148
    Top = 266
    Width = 66
    Height = 25
    Hint = 'CTRL + ALT + SPACE'
    Caption = 'Stop'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = butStopClick
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 293
    Width = 638
    Height = 19
    Align = alBottom
    BorderStyle = sbsSingle
    Caption = 'Open an audio file'
    TabOrder = 3
    ExplicitTop = 287
    ExplicitWidth = 608
  end
  object Panel2: TPanel
    Left = 0
    Top = 228
    Width = 638
    Height = 35
    Align = alTop
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 4
    object lblBarPositionInsTime: TLabel
      Left = 11
      Top = 17
      Width = 75
      Height = 15
      Alignment = taRightJustify
      Caption = 'Time: 00:00:00'
    end
    object lblBarPosition: TLabel
      Left = 109
      Top = 17
      Width = 124
      Height = 16
      AutoSize = False
      Caption = 'Position: 0'
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 636
      Height = 16
      Align = alTop
      TabOrder = 0
      OnMouseMove = pbProgressMouseMove
      OnMouseUp = pbProgressMouseUp
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 638
    Height = 228
    Align = alTop
    DoubleBuffered = False
    FullRepaint = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 5
    ExplicitLeft = 1
    ExplicitTop = 2
    object Bevel1: TBevel
      Left = 132
      Top = 21
      Width = 200
      Height = 170
    end
    object lblLow: TLabel
      Left = 150
      Top = 172
      Width = 22
      Height = 15
      Caption = 'Low'
    end
    object lblMid: TLabel
      Left = 182
      Top = 172
      Width = 21
      Height = 15
      Caption = 'Mid'
    end
    object lblHigh: TLabel
      Left = 214
      Top = 172
      Width = 26
      Height = 15
      Caption = 'High'
    end
    object lblRamp: TLabel
      Left = 244
      Top = 91
      Width = 65
      Height = 15
      Caption = 'Ramp mode'
    end
    object lblRampMs: TLabel
      Left = 243
      Top = 131
      Width = 85
      Height = 15
      Caption = 'Ramp time (ms)'
    end
    object Bevel3: TBevel
      Left = 7
      Top = 21
      Width = 60
      Height = 170
    end
    object pmLeft: TMfPeakMeter
      Left = 47
      Top = 44
      Width = 11
      Height = 123
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      Precision = 10
    end
    object Bevel2: TBevel
      Left = 69
      Top = 21
      Width = 60
      Height = 170
    end
    object pmRight: TMfPeakMeter
      Left = 78
      Top = 44
      Width = 11
      Height = 123
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      Precision = 1
    end
    object Label1: TLabel
      Left = 26
      Top = 173
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
      Left = 81
      Top = 173
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
      Left = 9
      Top = 26
      Width = 46
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
      Left = 73
      Top = 26
      Width = 45
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
    object lblDuration: TLabel
      Left = 5
      Top = 194
      Width = 94
      Height = 16
      Caption = 'Duration: 00:00:00'
    end
    object lblPlayed: TLabel
      Left = 16
      Top = 210
      Width = 83
      Height = 16
      Caption = 'Played: 00:00:00'
    end
    object chkEQ: TCheckBox
      Left = 144
      Top = 3
      Width = 79
      Height = 12
      Caption = 'Enable EQ'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkEQClick
    end
    object tbLow: TTrackBar
      Left = 140
      Top = 101
      Width = 22
      Height = 68
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
      Left = 175
      Top = 101
      Width = 22
      Height = 68
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
      Left = 206
      Top = 101
      Width = 23
      Height = 68
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
      Left = 244
      Top = 106
      Width = 61
      Height = 23
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbxRampChange
    end
    object edtRampMs: TEdit
      Left = 244
      Top = 148
      Width = 40
      Height = 23
      TabOrder = 5
      Text = '30'
      OnChange = edtRampMsChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 7
      Top = 3
      Width = 125
      Height = 12
      Caption = 'Lock Volume Sliders'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object trbVolumeL: TTrackBar
      Left = 11
      Top = 44
      Width = 28
      Height = 131
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
      Left = 91
      Top = 44
      Width = 24
      Height = 131
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
      Left = 334
      Top = 21
      Width = 298
      Height = 170
      Color = 6656
      DoubleBuffered = True
      ParentBackground = False
      ParentDoubleBuffered = False
      TabOrder = 9
      object avGraph: TMfAudioVisualizer
        Left = 4
        Top = 5
        Width = 290
        Height = 160
        PeakThreshold = 0.980000019073486300
        PeakCapFrac = 0.100000001490116100
        BarCount = 32
      end
    end
    object stxtHighIndex: TStaticText
      Left = 208
      Top = 91
      Width = 19
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 10
    end
    object stxtMidIndex: TStaticText
      Left = 174
      Top = 91
      Width = 21
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 11
    end
    object stxtLowIndex: TStaticText
      Left = 145
      Top = 91
      Width = 20
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 12
    end
    object stxtProcessed: TStaticText
      Left = 112
      Top = 209
      Width = 127
      Height = 16
      AutoSize = False
      Caption = 'Samples: 0'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 13
    end
    object Panel1: TPanel
      Left = 139
      Top = 26
      Width = 186
      Height = 63
      Color = 6656
      ParentBackground = False
      TabOrder = 14
      object imgSpectrumAnalizer: TImage
        Left = 4
        Top = 5
        Width = 178
        Height = 54
        ParentCustomHint = False
        ParentShowHint = False
        ShowHint = False
      end
    end
    object chkGraph: TCheckBox
      Left = 338
      Top = 3
      Width = 88
      Height = 12
      Caption = 'Enable Graph'
      Checked = True
      State = cbChecked
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

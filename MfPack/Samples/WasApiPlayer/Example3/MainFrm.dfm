object frmMain: TfrmMain
  Left = 0
  Top = 0
  Hint = 
    'Press Press CTRL + ALT + F1 to hide the GUI.'#13#10'Press Press CTRL +' +
    ' ALT + ESC to show the GUI.'
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WASAPI Player Sample 3 - High/Mid/Low EQ MFT'
  ClientHeight = 340
  ClientWidth = 418
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
    Left = 32
    Top = 401
    Width = 80
    Height = 15
    Caption = '(no file loaded)'
  end
  object btnLoad: TButton
    Left = 7
    Top = 292
    Width = 73
    Height = 24
    Caption = 'Load...'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object butPlayPause: TButton
    Left = 86
    Top = 292
    Width = 72
    Height = 24
    Hint = 'CTRL + ALT + RIGHT'
    Caption = 'Play'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = butPlayPauseClick
  end
  object butStop: TButton
    Left = 164
    Top = 292
    Width = 73
    Height = 24
    Hint = 'CTRL + ALT + SPACE'
    Caption = 'Stop'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = butStopClick
  end
  object stxtStatus: TStaticText
    Left = 0
    Top = 321
    Width = 418
    Height = 19
    Align = alBottom
    BorderStyle = sbsSingle
    Caption = 'Open an audio file'
    TabOrder = 3
    ExplicitWidth = 356
  end
  object Panel2: TPanel
    Left = 0
    Top = 253
    Width = 418
    Height = 33
    Align = alTop
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 4
    ExplicitWidth = 356
    object lblBarPositionInSTime: TLabel
      Left = 8
      Top = 16
      Width = 84
      Height = 15
      AutoSize = False
      Caption = 'Time: 00:00:00'
    end
    object lblBarPosition: TLabel
      Left = 98
      Top = 16
      Width = 134
      Height = 15
      AutoSize = False
      Caption = 'Position: 0'
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 416
      Height = 13
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
    Width = 418
    Height = 253
    Align = alTop
    DoubleBuffered = False
    FullRepaint = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 5
    ExplicitTop = 2
    object Bevel1: TBevel
      Left = 171
      Top = 23
      Width = 230
      Height = 189
    end
    object lblLow: TLabel
      Left = 186
      Top = 193
      Width = 22
      Height = 15
      Caption = 'Low'
    end
    object lblMid: TLabel
      Left = 221
      Top = 193
      Width = 21
      Height = 15
      Caption = 'Mid'
    end
    object lblHigh: TLabel
      Left = 255
      Top = 193
      Width = 26
      Height = 15
      Caption = 'High'
    end
    object lblRamp: TLabel
      Left = 295
      Top = 100
      Width = 65
      Height = 15
      Caption = 'Ramp mode'
    end
    object lblRampMs: TLabel
      Left = 295
      Top = 141
      Width = 85
      Height = 15
      Caption = 'Ramp time (ms)'
    end
    object Bevel3: TBevel
      Left = 7
      Top = 23
      Width = 56
      Height = 189
    end
    object pmLeft: TMfPeakMeter
      Left = 45
      Top = 49
      Width = 14
      Height = 136
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcLeft
      DeviceRole = eConsole
      Precision = 10
    end
    object Bevel2: TBevel
      Left = 66
      Top = 23
      Width = 56
      Height = 189
    end
    object pmRight: TMfPeakMeter
      Left = 72
      Top = 49
      Width = 14
      Height = 136
      BackGroundColor = clAppWorkSpace
      BarColor = clAqua
      Direction = pdVertical
      SampleChannel = mcRight
      DeviceRole = eConsole
      Precision = 1
    end
    object Label1: TLabel
      Left = 29
      Top = 196
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
      Left = 79
      Top = 196
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
      Top = 27
      Width = 51
      Height = 11
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
      Left = 68
      Top = 27
      Width = 50
      Height = 11
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
      Top = 220
      Width = 100
      Height = 15
      AutoSize = False
      Caption = 'Duration: 00:00:00'
    end
    object lblPlayed: TLabel
      Left = 16
      Top = 234
      Width = 91
      Height = 14
      AutoSize = False
      Caption = 'Played: 00:00:00'
    end
    object chkEQ: TCheckBox
      Left = 182
      Top = 27
      Width = 129
      Height = 14
      Caption = 'Enable EQ'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkEQClick
    end
    object tbLow: TTrackBar
      Left = 182
      Top = 111
      Width = 25
      Height = 76
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
      Left = 216
      Top = 111
      Width = 27
      Height = 76
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
      Left = 253
      Top = 111
      Width = 26
      Height = 76
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
      Left = 295
      Top = 116
      Width = 69
      Height = 23
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbxRampChange
    end
    object edtRampMs: TEdit
      Left = 295
      Top = 157
      Width = 45
      Height = 23
      TabOrder = 5
      Text = '30'
      OnChange = edtRampMsChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 10
      Top = 5
      Width = 121
      Height = 14
      Caption = 'Lock Volume Sliders'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object trbVolumeL: TTrackBar
      Left = 12
      Top = 44
      Width = 27
      Height = 146
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
      Left = 89
      Top = 44
      Width = 28
      Height = 146
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
      Left = 180
      Top = 44
      Width = 211
      Height = 53
      Color = 6656
      ParentBackground = False
      TabOrder = 9
      object imgSpectrumAnalizer: TImage
        Left = 4
        Top = 4
        Width = 203
        Height = 44
        ParentCustomHint = False
        ParentShowHint = False
        ShowHint = False
      end
    end
    object stxtHighIndex: TStaticText
      Left = 255
      Top = 100
      Width = 22
      Height = 14
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 10
    end
    object stxtMidIndex: TStaticText
      Left = 218
      Top = 100
      Width = 23
      Height = 14
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 11
    end
    object stxtLowIndex: TStaticText
      Left = 186
      Top = 100
      Width = 22
      Height = 14
      Alignment = taCenter
      AutoSize = False
      Caption = '0 dB'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 12
    end
    object stxtProcessed: TStaticText
      Left = 113
      Top = 233
      Width = 142
      Height = 14
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

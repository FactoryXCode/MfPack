object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player  Sample 2'
  ClientHeight = 362
  ClientWidth = 324
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 324
    Height = 298
    Align = alTop
    ExplicitTop = 2
  end
  object Bevel4: TBevel
    Left = 177
    Top = 19
    Width = 55
    Height = 233
  end
  object Bevel3: TBevel
    Left = 9
    Top = 19
    Width = 78
    Height = 233
  end
  object Bevel2: TBevel
    Left = 93
    Top = 19
    Width = 78
    Height = 233
  end
  object lblDuration: TLabel
    Left = 12
    Top = 258
    Width = 92
    Height = 13
    AutoSize = False
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 122
    Top = 277
    Width = 154
    Height = 13
    AutoSize = False
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 21
    Top = 277
    Width = 83
    Height = 13
    AutoSize = False
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 99
    Top = 49
    Width = 17
    Height = 172
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcRight
    DeviceRole = eConsole
    Precision = 1
  end
  object pmLeft: TMfPeakMeter
    Left = 63
    Top = 49
    Width = 18
    Height = 172
    BackGroundColor = clAppWorkSpace
    BarColor = clAqua
    Direction = pdVertical
    SampleChannel = mcLeft
    DeviceRole = eConsole
    Precision = 10
  end
  object Label1: TLabel
    Left = 39
    Top = 232
    Width = 22
    Height = 14
    Caption = 'Left'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 116
    Top = 232
    Width = 28
    Height = 14
    Caption = 'Right'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 190
    Top = 232
    Width = 27
    Height = 14
    Caption = 'Pitch'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblLeftVolume: TLabel
    Left = 14
    Top = 27
    Width = 67
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRightVolume: TLabel
    Left = 99
    Top = 27
    Width = 66
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPitch: TLabel
    Left = 180
    Top = 27
    Width = 49
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object butPlay: TButton
    Left = 8
    Top = 308
    Width = 74
    Height = 24
    Caption = 'Play'
    Enabled = False
    TabOrder = 0
    OnClick = butPlayClick
  end
  object butStop: TButton
    Left = 164
    Top = 308
    Width = 74
    Height = 24
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 338
    Width = 324
    Height = 24
    Panels = <>
    SimplePanel = True
    SimpleText = 'Open an audio file'
  end
  object trbVolumeR: TTrackBar
    Left = 122
    Top = 46
    Width = 35
    Height = 180
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
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = trbVolumeRChange
  end
  object trbVolumeL: TTrackBar
    Left = 14
    Top = 46
    Width = 39
    Height = 180
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
    TabOrder = 4
    TickMarks = tmBoth
    OnChange = trbVolumeLChange
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 44
    Top = 2
    Width = 115
    Height = 15
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object butReplay: TButton
    Left = 242
    Top = 308
    Width = 74
    Height = 24
    Caption = 'Replay'
    Enabled = False
    TabOrder = 6
    OnClick = butReplayClick
  end
  object butPause: TButton
    Left = 86
    Top = 308
    Width = 74
    Height = 24
    Caption = 'Pause'
    Enabled = False
    TabOrder = 7
    OnClick = butPauseClick
  end
  object trbPitch: TTrackBar
    Left = 185
    Top = 46
    Width = 38
    Height = 180
    DoubleBuffered = True
    Max = 50
    Min = -50
    Orientation = trVertical
    ParentDoubleBuffered = False
    TabOrder = 8
    TickMarks = tmBoth
    OnChange = trbPitchChange
  end
  object mnuMain: TMainMenu
    Left = 351
    Top = 68
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
    Filter = 
      'WAV|*.wav|MPEG Audio Layer-3  (MP3)|*.mp3|Free Losless Audio Cod' +
      'er (FLAC)|*.flac|All files|*.*'
    Left = 350
    Top = 19
  end
end

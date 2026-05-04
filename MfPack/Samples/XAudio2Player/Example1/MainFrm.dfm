object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAudio2 Player (Basic Sample)'
  ClientHeight = 440
  ClientWidth = 587
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMain
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 144
  TextHeight = 21
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 587
    Height = 351
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
  end
  object Bevel3: TBevel
    Left = 69
    Top = 29
    Width = 117
    Height = 312
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object Bevel2: TBevel
    Left = 191
    Top = 29
    Width = 117
    Height = 312
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object lblDuration: TLabel
    Left = 344
    Top = 35
    Width = 141
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Duration: 00:00:00'
  end
  object lblProcessed: TLabel
    Left = 344
    Top = 116
    Width = 82
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Samples: 0'
  end
  object lblPlayed: TLabel
    Left = 357
    Top = 75
    Width = 125
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Played: 00:00:00'
  end
  object pmRight: TMfPeakMeter
    Left = 204
    Top = 41
    Width = 26
    Height = 258
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
  object pmLeft: TMfPeakMeter
    Left = 155
    Top = 41
    Width = 27
    Height = 258
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
  object Label1: TLabel
    Left = 117
    Top = 314
    Width = 30
    Height = 22
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Left'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 225
    Top = 314
    Width = 41
    Height = 22
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Right'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = cl3DDkShadow
    Font.Height = -18
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object butPlayPause: TButton
    Left = 17
    Top = 360
    Width = 111
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
    Left = 137
    Top = 360
    Width = 111
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 404
    Width = 587
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Panels = <>
    SimplePanel = True
    SimpleText = 'Open an audio file'
  end
  object trbVolumeR: TTrackBar
    Left = 239
    Top = 35
    Width = 58
    Height = 270
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    DoubleBuffered = True
    Max = 0
    Min = -224
    Orientation = trVertical
    ParentDoubleBuffered = False
    Frequency = 10
    TabOrder = 3
    ThumbLength = 30
    TickMarks = tmBoth
    OnChange = trbVolumeRChange
  end
  object trbVolumeL: TTrackBar
    Left = 80
    Top = 35
    Width = 58
    Height = 270
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    DoubleBuffered = True
    Max = 0
    Min = -224
    Orientation = trVertical
    ParentDoubleBuffered = False
    Frequency = 10
    TabOrder = 4
    ThumbLength = 30
    TickMarks = tmBoth
    OnChange = trbVolumeLChange
  end
  object cbLockVolumeSliders: TCheckBox
    Left = 117
    Top = 3
    Width = 230
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Lock Volume Sliders'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object butReplay: TButton
    Left = 257
    Top = 360
    Width = 111
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Replay'
    Enabled = False
    TabOrder = 6
    OnClick = butReplayClick
  end
  object mnuMain: TMainMenu
    Left = 231
    Top = 131
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
    Left = 282
    Top = 130
  end
end

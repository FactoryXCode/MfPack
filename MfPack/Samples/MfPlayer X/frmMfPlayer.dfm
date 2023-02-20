object frm_MfPlayer: Tfrm_MfPlayer
  Left = 48
  Top = 50
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  Caption = 'MfPlayer X'
  ClientHeight = 439
  ClientWidth = 725
  Color = clBtnFace
  TransparentColorValue = clNone
  DefaultMonitor = dmMainForm
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  SnapBuffer = 0
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = pnlVideoResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 725
    Height = 392
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ParentCustomHint = False
    Align = alClient
    Anchors = [akLeft, akTop, akRight]
    AutoSize = True
    BevelOuter = bvNone
    Color = clNone
    Ctl3D = False
    UseDockManager = False
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Font.Quality = fqAntialiased
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    TabStop = True
    OnResize = pnlVideoResize
  end
  object pnlControls: TPanel
    Left = 0
    Top = 392
    Width = 725
    Height = 47
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ParentCustomHint = False
    Align = alBottom
    Alignment = taRightJustify
    BevelKind = bkFlat
    BevelOuter = bvLowered
    Caption = 'MfPlayer III   '
    Ctl3D = True
    UseDockManager = False
    DoubleBuffered = True
    FullRepaint = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Font.Quality = fqClearType
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ParentFont = False
    ParentShowHint = False
    ShowCaption = False
    ShowHint = False
    TabOrder = 1
    TabStop = True
    VerticalAlignment = taAlignBottom
    DesignSize = (
      721
      43)
    object butStop: TButton
      Left = 15
      Top = 16
      Width = 55
      Height = 24
      Hint = 'Stop'
      Caption = 'Stop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = butStopClick
    end
    object butPause: TButton
      Left = 73
      Top = 16
      Width = 55
      Height = 24
      Hint = 'Pause'
      Caption = 'Pause'
      DoubleBuffered = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentDoubleBuffered = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = butPauseClick
    end
    object butPlay: TButton
      Left = 132
      Top = 16
      Width = 55
      Height = 24
      Hint = 'Play'
      Caption = 'Play'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = butPlayClick
    end
    object trbVolumeL: TTrackBar
      Left = 193
      Top = 8
      Width = 82
      Height = 19
      Hint = 'Volume Left'
      ParentCustomHint = False
      Max = 100
      ParentShowHint = False
      Frequency = 2
      PositionToolTip = ptTop
      ShowHint = True
      ShowSelRange = False
      TabOrder = 3
      ThumbLength = 10
      TickMarks = tmTopLeft
      OnChange = trbVolumeLChange
    end
    object prbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 719
      Height = 12
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      ParentCustomHint = False
      Align = alTop
      DoubleBuffered = False
      ParentDoubleBuffered = False
      ParentShowHint = False
      MarqueeInterval = 1
      BarColor = clMenuHighlight
      BackgroundColor = clGradientInactiveCaption
      Step = 1
      ShowHint = False
      TabOrder = 4
      OnMouseUp = prbProgressMouseUp
    end
    object butFullScreen: TButton
      Left = 444
      Top = 16
      Width = 25
      Height = 24
      Hint = 'Full Screen, hit Esc to switch.'
      Anchors = [akTop, akRight]
      Caption = '[]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = butFullScreenClick
    end
    object trbVolumeR: TTrackBar
      Left = 193
      Top = 27
      Width = 82
      Height = 19
      Hint = 'Volume Right'
      Max = 100
      ParentShowHint = False
      Frequency = 2
      PositionToolTip = ptBottom
      ShowHint = True
      ShowSelRange = False
      TabOrder = 6
      ThumbLength = 10
      OnChange = trbVolumeRChange
    end
    object cbLockVolumeSliders: TCheckBox
      Left = 278
      Top = 22
      Width = 13
      Height = 13
      Checked = True
      Ctl3D = True
      ParentCtl3D = False
      State = cbChecked
      TabOrder = 7
    end
  end
  object dlgOpenUrl: TOpenDialog
    Title = 'Open mediafile'
    Left = 106
    Top = 10
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 10
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuOpen: TMenuItem
        Caption = '&Open'
        OnClick = mnuOpenClick
      end
      object muSeparator1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mnuExitClick
      end
    end
    object mnuExtra: TMenuItem
      Caption = '&Extra'
      object mnuSetPosition: TMenuItem
        Caption = 'Set&Position'
        Enabled = False
        OnClick = mnuSetPositionClick
      end
      object mnuTakeScreenshot: TMenuItem
        Caption = 'Take Screenshot (F8)'
        Enabled = False
        OnClick = mnuTakeScreenshotClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuAspectRatio: TMenuItem
        Caption = 'Aspect &Ratio'
        object mnuCinema: TMenuItem
          Caption = '2.35 : 1'
          OnClick = mnuCinemaClick
        end
        object mnuSixteenByNine: TMenuItem
          Caption = '16 : 9'
          Checked = True
          Default = True
          OnClick = mnuSixteenByNineClick
        end
        object mnuFourByThree: TMenuItem
          Caption = '4 : 3'
          OnClick = mnuFourByThreeClick
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuSetRate: TMenuItem
        Caption = 'Set Rate'
        Enabled = False
        object Ratem1: TMenuItem
          AutoCheck = True
          Caption = 'Slowest'
          OnClick = Ratem1Click
        end
        object Rate1: TMenuItem
          AutoCheck = True
          Caption = 'Normal'
          OnClick = Rate1Click
        end
        object Ratep2: TMenuItem
          AutoCheck = True
          Caption = 'Maximum'
          OnClick = Ratep2Click
        end
      end
      object mnuSubTitling: TMenuItem
        Caption = 'SubTitling'
        Enabled = False
        object mnuEnableSubtitling: TMenuItem
          AutoCheck = True
          Caption = 'Enable Subtitling'
          OnClick = mnuEnableSubtitlingClick
        end
        object mnuLanguage: TMenuItem
          Caption = 'Language'
          OnClick = mnuLanguageClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuSelectStreams: TMenuItem
        Caption = 'Select Streams'
        Enabled = False
        OnClick = mnuSelectStreamsClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuMediaInfo: TMenuItem
        Caption = 'Media Info'
        Enabled = False
        OnClick = mnuMediaInfoClick
      end
    end
  end
  object QTimer1: TQTimer
    Period = 0
    Left = 172
    Top = 10
  end
end

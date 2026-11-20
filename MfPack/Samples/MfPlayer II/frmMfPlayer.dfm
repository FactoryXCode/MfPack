object frm_MfPlayer: Tfrm_MfPlayer
  Left = 0
  Top = 0
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 44
  Caption = 'MfPlayer II'
  ClientHeight = 404
  ClientWidth = 597
  Color = clBtnShadow
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
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 361
    Width = 597
    Height = 43
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ParentCustomHint = False
    Align = alBottom
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'MfPlayer II   '
    Ctl3D = True
    UseDockManager = False
    DoubleBuffered = False
    Enabled = False
    FullRepaint = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Font.Quality = fqClearType
    ParentBiDiMode = False
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ParentFont = False
    ParentShowHint = False
    ShowCaption = False
    ShowHint = False
    TabOrder = 0
    TabStop = True
    VerticalAlignment = taAlignBottom
    DesignSize = (
      597
      43)
    object butStop: TButton
      Left = 15
      Top = 15
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
      Top = 15
      Width = 55
      Height = 24
      Hint = 'Pause'
      Caption = 'Pause'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = butPauseClick
    end
    object butPlay: TButton
      Left = 131
      Top = 15
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
    object trbVolume: TTrackBar
      Left = 192
      Top = 15
      Width = 81
      Height = 23
      Hint = 'Volume'
      ParentShowHint = False
      PositionToolTip = ptTop
      ShowHint = True
      TabOrder = 3
      ThumbLength = 15
      OnChange = trbVolumeChange
    end
    object prbProgress: TProgressBar
      Left = 1
      Top = 1
      Width = 595
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
      ShowHint = False
      TabOrder = 5
    end
    object butFullScreen: TButton
      Left = 555
      Top = 15
      Width = 24
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
      TabOrder = 4
      OnClick = butFullScreenClick
    end
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 597
    Height = 361
    Align = alClient
    BevelOuter = bvNone
    Caption = 'MfPlayer II'
    Color = clNone
    UseDockManager = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Arial Unicode MS'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    StyleElements = []
    ExplicitLeft = 1
    ExplicitTop = 1
    object stxtSubs: TStaticText
      AlignWithMargins = True
      Left = 4
      Top = 313
      Width = 589
      Height = 44
      Margins.Left = 4
      Margins.Top = 0
      Margins.Right = 4
      Margins.Bottom = 4
      ParentCustomHint = False
      Align = alBottom
      Alignment = taCenter
      AutoSize = False
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = clNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Font.Quality = fqAntialiased
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowAccelChar = False
      ShowHint = False
      TabOrder = 0
      Visible = False
      StyleElements = []
    end
  end
  object dlgOpenUrl: TOpenDialog
    Title = 'Open mediafile'
    Left = 98
    Top = 10
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 10
    object muFile: TMenuItem
      Caption = '&File'
      object muOpen: TMenuItem
        Caption = '&Open'
        OnClick = muOpenClick
      end
      object muSeparator1: TMenuItem
        Caption = '-'
      end
      object muExit: TMenuItem
        Caption = 'E&xit'
        OnClick = muExitClick
      end
    end
    object Extra1: TMenuItem
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
    end
  end
end

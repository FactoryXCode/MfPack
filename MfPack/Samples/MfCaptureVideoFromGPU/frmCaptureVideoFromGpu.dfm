object frmCapture: TfrmCapture
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Capture Video From GPU'
  ClientHeight = 895
  ClientWidth = 997
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 25
  object pnlPreview: TPanel
    Left = 0
    Top = 0
    Width = 997
    Height = 602
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Color = clBlack
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object mmoLog: TMemo
    Left = 0
    Top = 697
    Width = 997
    Height = 198
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Color = clBlack
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 602
    Width = 997
    Height = 95
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    TabOrder = 2
    object Label1: TLabel
      Left = 333
      Top = 15
      Width = 113
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Resolution:'
    end
    object Label2: TLabel
      Left = 333
      Top = 55
      Width = 113
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Frame rate:'
    end
    object btnStart: TButton
      Left = 15
      Top = 10
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 147
      Top = 10
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Stop'
      TabOrder = 1
      OnClick = btnStopClick
    end
    object cbxResulotuions: TComboBox
      Left = 456
      Top = 12
      Width = 226
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 2
      TabOrder = 2
      Text = 'Full HD (1920 x 1080)'
      Items.Strings = (
        'Preview window size'
        '720p (1280 x 720)'
        'Full HD (1920 x 1080)'
        '2K (2560 x 1440)'
        '4K (3840 x 2160)')
    end
    object cbxFrameRate: TComboBox
      Left = 456
      Top = 52
      Width = 94
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 1
      TabOrder = 3
      Text = '60 Hz'
      Items.Strings = (
        '30 Hz'
        '60 Hz')
    end
  end
end

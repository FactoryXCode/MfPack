object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfCustomMFT Sample 2 - Source Reader to Grayscale MFT'
  ClientHeight = 650
  ClientWidth = 1040
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCommands: TPanel
    Left = 0
    Top = 0
    Width = 1040
    Height = 64
    Align = alTop
    TabOrder = 0
    object lblSummary: TLabel
      Left = 760
      Top = 10
      Width = 268
      Height = 13
      Caption = 'Open a video, then release the trackbar thumb to seek.'
    end
    object lblPosition: TLabel
      Left = 760
      Top = 34
      Width = 74
      Height = 13
      Caption = '0.000 / 0.000 s'
    end
    object btnOpen: TButton
      Left = 16
      Top = 16
      Width = 120
      Height = 32
      Caption = 'Open video...'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object tbPosition: TTrackBar
      Left = 148
      Top = 8
      Width = 596
      Height = 45
      Max = 1000
      PageSize = 10
      Frequency = 50
      TabOrder = 1
      TickStyle = tsNone
      OnChange = tbPositionChange
    end
  end
  object pnlSource: TPanel
    Left = 0
    Top = 64
    Width = 520
    Height = 426
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object lblSource: TLabel
      Left = 0
      Top = 0
      Width = 520
      Height = 28
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Source Reader output (RGB32)'
      Layout = tlCenter
    end
    object imgSource: TImage
      Left = 0
      Top = 28
      Width = 520
      Height = 398
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object pnlOutput: TPanel
    Left = 520
    Top = 64
    Width = 520
    Height = 426
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lblOutput: TLabel
      Left = 0
      Top = 0
      Width = 520
      Height = 28
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Same sample after the grayscale MFT'
      Layout = tlCenter
    end
    object imgOutput: TImage
      Left = 0
      Top = 28
      Width = 520
      Height = 398
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object memLog: TMemo
    Left = 0
    Top = 490
    Width = 1040
    Height = 160
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object dlgOpenVideo: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select a video file'
    Left = 868
    Top = 14
  end
end

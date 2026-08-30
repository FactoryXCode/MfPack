object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfCustomMFT Sample 3 - Asynchronous grayscale playback'
  ClientHeight = 680
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCommands: TPanel
    Left = 0
    Top = 0
    Width = 1040
    Height = 72
    Align = alTop
    TabOrder = 0
    object lblTime: TLabel
      Left = 824
      Top = 29
      Width = 74
      Height = 13
      Caption = '0.000 / 0.000 s'
    end
    object btnOpen: TButton
      Left = 16
      Top = 20
      Width = 110
      Height = 32
      Caption = 'Open video...'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnPlayPause: TButton
      Left = 138
      Top = 20
      Width = 90
      Height = 32
      Caption = 'Play'
      Default = True
      TabOrder = 1
      OnClick = btnPlayPauseClick
    end
    object btnStop: TButton
      Left = 240
      Top = 20
      Width = 90
      Height = 32
      Caption = 'Stop'
      TabOrder = 2
      OnClick = btnStopClick
    end
    object pbPosition: TProgressBar
      Left = 350
      Top = 25
      Width = 454
      Height = 22
      Max = 1000
      TabOrder = 3
    end
  end
  object pnlSource: TPanel
    Left = 0
    Top = 72
    Width = 520
    Height = 438
    Align = alLeft
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    object lblSource: TLabel
      Left = 0
      Top = 0
      Width = 520
      Height = 28
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Asynchronous Source Reader output'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
      Layout = tlCenter
    end
    object imgSource: TImage
      Left = 0
      Top = 28
      Width = 520
      Height = 410
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object pnlOutput: TPanel
    Left = 520
    Top = 72
    Width = 520
    Height = 438
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 2
    object lblOutput: TLabel
      Left = 0
      Top = 0
      Width = 520
      Height = 28
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Same frame after the grayscale MFT'
      Color = clBtnFace
      ParentColor = False
      Transparent = False
      Layout = tlCenter
    end
    object imgOutput: TImage
      Left = 0
      Top = 28
      Width = 520
      Height = 410
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object memLog: TMemo
    Left = 0
    Top = 510
    Width = 1040
    Height = 170
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
    Left = 936
    Top = 16
  end
  object tmrFrame: TTimer
    Enabled = False
    Interval = 15
    OnTimer = tmrFrameTimer
    Left = 976
    Top = 16
  end
end

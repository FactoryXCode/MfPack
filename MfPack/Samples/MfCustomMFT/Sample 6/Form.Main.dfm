object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfCustomMFT Sample 4 - Grayscale MFT in a Media Session topology'
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
  OnPaint = FormPaint
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
    object lblState: TLabel
      Left = 360
      Top = 29
      Width = 77
      Height = 13
      Caption = 'No video loaded'
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
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 72
    Width = 1040
    Height = 438
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
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
    TabOrder = 2
  end
  object dlgOpenVideo: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select a video file'
    Left = 968
    Top = 16
  end
end

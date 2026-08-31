object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfCustomMFT - RGB32 Grayscale Transform'
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
      Left = 205
      Top = 25
      Width = 301
      Height = 13
      Caption = 
        'The source frame is generated locally; no video file is required' +
        '.'
    end
    object btnProcess: TButton
      Left = 16
      Top = 16
      Width = 170
      Height = 32
      Caption = 'Process one RGB32 frame'
      Default = True
      TabOrder = 0
      OnClick = btnProcessClick
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
      Caption = 'Input sample (RGB32)'
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
      Caption = 'Output sample (same buffer, grayscale)'
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
end

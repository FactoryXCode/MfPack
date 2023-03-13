object frmLoopBackCapture: TfrmLoopBackCapture
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Loopback Capture Sample'
  ClientHeight = 147
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 34
    Top = 18
    Width = 79
    Height = 13
    Hint = 'Enter a file name without extension.'
    AutoSize = False
    Caption = 'FileName'
    ParentShowHint = False
    ShowHint = True
  end
  object lblFileExt: TLabel
    Left = 299
    Top = 39
    Width = 50
    Height = 21
    Hint = 'Enter a file name without extension.'
    AutoSize = False
    Caption = '.wav'
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = False
  end
  object butStart: TButton
    Left = 69
    Top = 86
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 0
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 160
    Top = 86
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object edFileName: TEdit
    Left = 34
    Top = 39
    Width = 265
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = 'loopback-capture'
  end
  object sbMsg: TStatusBar
    Left = 0
    Top = 123
    Width = 430
    Height = 24
    Panels = <>
    SimplePanel = True
    SimpleText = 'Start Capture'
  end
  object butPlayData: TButton
    Left = 251
    Top = 86
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = butPlayDataClick
  end
  object cbxDontOverWrite: TCheckBox
    Left = 88
    Top = 18
    Width = 115
    Height = 15
    Hint = 'Do not overwrite files with the same name.'
    Caption = 'Don'#39't Overwrite'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 5
  end
end

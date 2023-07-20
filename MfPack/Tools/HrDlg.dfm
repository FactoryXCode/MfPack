object dlgHrTools: TdlgHrTools
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 
    'HResult Lookup Tool Application version 3.1.5  (Windows SDK 10.0' +
    '.22621.0) '
  ClientHeight = 445
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Lucida Sans'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label2: TLabel
    AlignWithMargins = True
    Left = 6
    Top = 157
    Width = 625
    Height = 14
    Margins.Left = 6
    Margins.Top = 6
    Align = alTop
    AutoSize = False
    Caption = 'Description'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitTop = 168
    ExplicitWidth = 811
  end
  object Bevel1: TBevel
    Left = 0
    Top = 414
    Width = 634
    Height = 31
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 412
  end
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 634
    Height = 65
    Align = alTop
    ExplicitLeft = 18
    ExplicitTop = 8
    ExplicitWidth = 789
  end
  object Bevel3: TBevel
    Left = 0
    Top = 65
    Width = 634
    Height = 86
    Align = alTop
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 6
    Top = 341
    Width = 625
    Height = 10
    Margins.Left = 6
    Margins.Top = 6
    Align = alTop
    AutoSize = False
    Caption = 'Reference:   '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitTop = 407
    ExplicitWidth = 811
  end
  object edHrHex: TEdit
    Left = 10
    Top = 69
    Width = 100
    Height = 22
    Hint = 'Enter or paste the hexadecimal HResult value.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = '$00000000'
    TextHint = '$'
  end
  object butTransToHex: TButton
    Left = 116
    Top = 95
    Width = 136
    Height = 25
    Hint = 'Translate decimal to hexadecimal value.'
    ParentCustomHint = False
    Caption = 'Translate to &hex'
    DoubleBuffered = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 6
    OnClick = butTransToHexClick
  end
  object edHrDec: TEdit
    Left = 10
    Top = 96
    Width = 100
    Height = 22
    Hint = 'Enter or paste the decimal HResult value.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Text = '0'
  end
  object butTransToDec: TButton
    Left = 116
    Top = 69
    Width = 136
    Height = 25
    Hint = 'Translate hexadecimal to decimal value.'
    Caption = 'Translate to &decimal'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = butTransToDecClick
  end
  object mmoDescription: TMemo
    Left = 0
    Top = 174
    Width = 634
    Height = 161
    Align = alTop
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object cbxFormStyle: TCheckBox
    Left = 520
    Top = 4
    Width = 108
    Height = 17
    BiDiMode = bdRightToLeft
    Caption = 'Stay On Top'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    TabOrder = 10
    OnClick = cbxFormStyleClick
  end
  object butSearch: TButton
    Left = 8
    Top = 121
    Width = 242
    Height = 25
    Caption = '&Search..'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = butSearchClick
    OnMouseMove = butSearchMouseMove
  end
  object butClose: TButton
    Left = 8
    Top = 418
    Width = 85
    Height = 25
    Caption = '&Exit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ModalResult = 8
    ParentFont = False
    TabOrder = 11
    OnClick = butCloseClick
  end
  object cbxAutoPaste: TCheckBox
    Left = 520
    Top = 23
    Width = 108
    Height = 17
    Hint = 'Copies CLipboardcontent automaticly to the HResult value fields.'
    BiDiMode = bdRightToLeft
    Caption = 'Auto paste'
    Checked = True
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentBiDiMode = False
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 12
  end
  object rbHLD: TRadioButton
    Left = 10
    Top = 4
    Width = 442
    Height = 17
    Caption = 'Use the Internal Error Lookup Tool.'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
  end
  object rbMSELTDESC: TRadioButton
    Left = 10
    Top = 23
    Width = 456
    Height = 17
    Hint = 
      'This option wil run the Windows Lookup Tool and prints the resul' +
      'ts in the description area.'
    Caption = 'Use the Microsoft Error Lookup Tool. '
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object rbSEMLTDLG: TRadioButton
    Left = 10
    Top = 42
    Width = 456
    Height = 17
    Caption = 'Use the System Error Messager Lookup Tool in a message dialog.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object lbxReference: TListBox
    Left = 0
    Top = 354
    Width = 634
    Height = 57
    Cursor = crHandPoint
    Align = alTop
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Lucida Sans'
    Font.Style = [fsUnderline]
    ItemHeight = 14
    Items.Strings = (
      'https://learn.microsoft.com')
    ParentFont = False
    TabOrder = 9
    OnClick = lbxReferenceClick
  end
end

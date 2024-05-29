object MftDlg: TMftDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MFT properties'
  ClientHeight = 495
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblMftSupportedInput: TLabel
    Left = 8
    Top = 7
    Width = 93
    Height = 13
    Caption = 'Supported Input'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSupportedOutput: TLabel
    Left = 8
    Top = 234
    Width = 101
    Height = 13
    Caption = 'Supported Output'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 0
    Top = 461
    Width = 558
    Height = 34
    Align = alBottom
    Shape = bsTopLine
    ExplicitLeft = 223
    ExplicitTop = 463
    ExplicitWidth = 182
  end
  object butOk: TButton
    Left = 398
    Top = 468
    Width = 73
    Height = 24
    Caption = 'Ok'
    TabOrder = 0
    OnClick = butOkClick
  end
  object Button2: TButton
    Left = 477
    Top = 468
    Width = 73
    Height = 24
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object sgMftInput: TStringGrid
    Left = 8
    Top = 46
    Width = 541
    Height = 148
    ColCount = 3
    DefaultColWidth = 260
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 2
    OnClick = sgMftInputClick
  end
  object stxtBitRate: TStaticText
    Left = 8
    Top = 26
    Width = 263
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Major Type'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 3
    Transparent = False
  end
  object StaticText1: TStaticText
    Left = 272
    Top = 26
    Width = 261
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Sub Type'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 4
    Transparent = False
  end
  object edInputMajorType: TEdit
    Left = 8
    Top = 200
    Width = 263
    Height = 21
    ReadOnly = True
    TabOrder = 5
    Text = 'edInputMajorType'
  end
  object edInputSubType: TEdit
    Left = 272
    Top = 200
    Width = 277
    Height = 21
    ReadOnly = True
    TabOrder = 6
    Text = 'edSelInput'
  end
  object sgMftOutput: TStringGrid
    Left = 8
    Top = 273
    Width = 541
    Height = 148
    ColCount = 3
    DefaultColWidth = 260
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 7
    OnClick = sgMftOutputClick
  end
  object StaticText2: TStaticText
    Left = 8
    Top = 253
    Width = 263
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Major Type'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 8
    Transparent = False
  end
  object StaticText3: TStaticText
    Left = 272
    Top = 253
    Width = 261
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Sub Type'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 9
    Transparent = False
  end
  object edOutputMajorType: TEdit
    Left = 8
    Top = 427
    Width = 263
    Height = 21
    ReadOnly = True
    TabOrder = 10
    Text = 'edSelInput'
  end
  object edOutPutSubType: TEdit
    Left = 272
    Top = 427
    Width = 277
    Height = 21
    ReadOnly = True
    TabOrder = 11
    Text = 'edSelInput'
  end
end

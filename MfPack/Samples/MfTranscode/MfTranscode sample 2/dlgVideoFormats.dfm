object VideoFormatDlg: TVideoFormatDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Output Video Format'
  ClientHeight = 337
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 13
  object Bevel1: TBevel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 423
    Height = 300
    Align = alTop
    Shape = bsFrame
    ExplicitTop = 4
    ExplicitWidth = 418
  end
  object Label4: TLabel
    Left = 11
    Top = 8
    Width = 81
    Height = 13
    Caption = ' Video Format '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 8
    Top = 51
    Width = 97
    Height = 13
    Caption = ' Video Resolution'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 8
    Top = 258
    Width = 66
    Height = 13
    Caption = 'Frame Rate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Bevel2: TBevel
    AlignWithMargins = True
    Left = 8
    Top = 70
    Width = 408
    Height = 187
    Shape = bsFrame
  end
  object Button1: TButton
    Left = 554
    Top = 548
    Width = 73
    Height = 23
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 633
    Top = 548
    Width = 73
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object butOk: TButton
    Left = 259
    Top = 309
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 340
    Top = 310
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cbxFormats: TComboBox
    Left = 11
    Top = 24
    Width = 243
    Height = 21
    TabOrder = 4
    OnCloseUp = cbxFormatsCloseUp
  end
  object StaticText3: TStaticText
    Left = 19
    Top = 95
    Width = 120
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Pixels (Width x Heigth)'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 5
    Transparent = False
  end
  object StaticText4: TStaticText
    Left = 140
    Top = 95
    Width = 120
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Aspect Ratio'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 6
    Transparent = False
  end
  object sgVideoFormats: TStringGrid
    Left = 14
    Top = 119
    Width = 264
    Height = 133
    ColCount = 3
    DefaultColWidth = 120
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 7
    OnClick = sgVideoFormatsClick
  end
  object cbxFrameRates: TComboBox
    Left = 8
    Top = 274
    Width = 264
    Height = 21
    TabOrder = 8
    OnCloseUp = cbxFrameRatesCloseUp
  end
  object cbxKeepOriginal: TCheckBox
    Left = 14
    Top = 75
    Width = 111
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Keep Original'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = cbxKeepOriginalClick
  end
end

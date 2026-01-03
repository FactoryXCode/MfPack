object dlgProcessInfo: TdlgProcessInfo
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Choose a process'
  ClientHeight = 554
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefault
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 21
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 139
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Running processes'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 495
    Width = 645
    Height = 59
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Shape = bsTopLine
  end
  object butOk: TButton
    Left = 12
    Top = 506
    Width = 143
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Ok'
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 164
    Top = 506
    Width = 142
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = butCancelClick
  end
  object butRefresh: TButton
    Left = 12
    Top = 422
    Width = 143
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Refresh'
    TabOrder = 3
    OnClick = butRefreshClick
  end
  object sgProcesses: TStringGrid
    Left = 12
    Top = 71
    Width = 621
    Height = 342
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ColCount = 3
    DefaultColWidth = 150
    DefaultRowHeight = 27
    DrawingStyle = gdsClassic
    FixedColor = clSilver
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object cbxSort: TCheckBox
    Left = 324
    Top = 422
    Width = 147
    Height = 22
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Sort ascending'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object butSort: TButton
    Left = 159
    Top = 422
    Width = 143
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Sort'
    TabOrder = 5
    OnClick = butSortClick
  end
  object cbxSortOnColumn: TCheckBox
    Left = 324
    Top = 453
    Width = 147
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Sort on name'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object StaticText1: TStaticText
    Left = 12
    Top = 44
    Width = 305
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Process name'
    Color = clHotLight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 7
    Transparent = False
  end
  object StaticText2: TStaticText
    Left = 314
    Top = 44
    Width = 156
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Process ID'
    Color = clHotLight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 8
    Transparent = False
  end
  object StaticText3: TStaticText
    Left = 467
    Top = 44
    Width = 153
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Main Process ID'
    Color = clHotLight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 9
    Transparent = False
  end
end

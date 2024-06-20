object dlgProcessInfo: TdlgProcessInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Choose a process'
  ClientHeight = 369
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 90
    Height = 13
    Caption = 'Running processes'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 330
    Width = 430
    Height = 39
    Align = alBottom
    Shape = bsTopLine
    ExplicitWidth = 328
  end
  object butOk: TButton
    Left = 8
    Top = 337
    Width = 95
    Height = 27
    Caption = '&Ok'
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 109
    Top = 337
    Width = 95
    Height = 27
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = butCancelClick
  end
  object butRefresh: TButton
    Left = 8
    Top = 281
    Width = 95
    Height = 27
    Caption = '&Refresh'
    TabOrder = 3
    OnClick = butRefreshClick
  end
  object sgProcesses: TStringGrid
    Left = 8
    Top = 47
    Width = 414
    Height = 228
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 18
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
    Left = 216
    Top = 281
    Width = 98
    Height = 15
    Caption = 'Sort ascending'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object butSort: TButton
    Left = 106
    Top = 281
    Width = 95
    Height = 27
    Caption = '&Sort'
    TabOrder = 5
    OnClick = butSortClick
  end
  object cbxSortOnColumn: TCheckBox
    Left = 216
    Top = 302
    Width = 98
    Height = 15
    Caption = 'Sort on name'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 29
    Width = 203
    Height = 18
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Process name'
    Color = clHotLight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 7
    Transparent = False
  end
  object StaticText2: TStaticText
    Left = 209
    Top = 29
    Width = 104
    Height = 18
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Process ID'
    Color = clHotLight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 8
    Transparent = False
  end
  object StaticText3: TStaticText
    Left = 311
    Top = 29
    Width = 102
    Height = 18
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Main Process ID'
    Color = clHotLight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 9
    Transparent = False
  end
end

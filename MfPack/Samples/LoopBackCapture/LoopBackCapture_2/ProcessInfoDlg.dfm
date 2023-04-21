object dlgProcessInfo: TdlgProcessInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Get running processes'
  ClientHeight = 369
  ClientWidth = 328
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
    Left = 12
    Top = 11
    Width = 90
    Height = 13
    Caption = 'Running processes'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 330
    Width = 328
    Height = 39
    Align = alBottom
    Shape = bsTopLine
  end
  object butOk: TButton
    Left = 118
    Top = 337
    Width = 95
    Height = 27
    Caption = '&Ok'
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 219
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
    Top = 30
    Width = 308
    Height = 243
    ColCount = 2
    DefaultRowHeight = 18
    DrawingStyle = gdsClassic
    FixedColor = clSilver
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 2
    ColWidths = (
      83
      108)
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
end

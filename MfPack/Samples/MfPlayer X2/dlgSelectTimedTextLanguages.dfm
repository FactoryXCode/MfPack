object DlgTimedTextLanguages: TDlgTimedTextLanguages
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Select preferred subtitle language'
  ClientHeight = 192
  ClientWidth = 307
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object butOk: TButton
    Left = 3
    Top = 165
    Width = 65
    Height = 21
    Caption = 'Ok'
    Default = True
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 69
    Top = 165
    Width = 65
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
  end
  object lvTTxtLang: TListView
    Left = 0
    Top = 0
    Width = 307
    Height = 157
    Align = alTop
    Checkboxes = True
    Columns = <
      item
        Caption = 'Current'
        MinWidth = 50
      end
      item
        Caption = 'Language tag'
        MinWidth = 50
        Width = 120
      end
      item
        AutoSize = True
        Caption = 'Language'
        MinWidth = 50
      end>
    ColumnClick = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnMouseUp = lvTTxtLangMouseUp
  end
end

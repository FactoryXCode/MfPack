object DlgTimedTextLanguages: TDlgTimedTextLanguages
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Select preferred subtitle language'
  ClientHeight = 221
  ClientWidth = 287
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object butOk: TButton
    Left = 4
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 80
    Top = 190
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
  end
  object lvTTxtLang: TListView
    Left = 4
    Top = 2
    Width = 283
    Height = 171
    Checkboxes = True
    Columns = <
      item
        Caption = 'Current'
        MaxWidth = 50
        MinWidth = 50
      end
      item
        Caption = 'Language tag'
        MinWidth = 50
        Width = 80
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
